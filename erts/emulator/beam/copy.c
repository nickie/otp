/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2012. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_gc.h"
#include "big.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "dtrace-wrapper.h"

static void move_one_frag(Eterm** hpp, Eterm* src, Uint src_sz, ErlOffHeap*);

/*
 *  Copy object "obj" to process p.
 */
Eterm
copy_object(Eterm obj, Process* to)
{
    Uint size = size_object(obj);
    Eterm* hp = HAlloc(to, size);
    Eterm res;

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(copy_object)) {
        DTRACE_CHARBUF(proc_name, 64);

        erts_snprintf(proc_name, sizeof(proc_name), "%T", to->common.id);
        DTRACE2(copy_object, proc_name, size);
    }
#endif
    res = copy_struct(obj, size, &hp, &to->off_heap);
#ifdef DEBUG
    if (eq(obj, res) == 0) {
	erl_exit(ERTS_ABORT_EXIT, "copy not equal to source\n");
    }
#endif
    return res;
}

/*
 * Return the "flat" size of the object.
 */

#if HALFWORD_HEAP
Uint size_object_rel(Eterm obj, Eterm* base)
#else
Uint size_object(Eterm obj)
#endif
{
    Uint sum = 0;
    Eterm* ptr;
    int arity;
#ifdef DEBUG
    Eterm mypid = erts_get_current_pid();
#endif

    DECLARE_ESTACK(s);

    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] size_object %p\n", mypid, obj));

    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST:
	    sum += 2;
	    ptr = list_val_rel(obj,base);
	    obj = *ptr++;
	    if (!IS_CONST(obj)) {
		ESTACK_PUSH(s, obj);
	    }	    
	    obj = *ptr;
	    break;
	case TAG_PRIMARY_BOXED:
	    {
		Eterm hdr = *boxed_val_rel(obj,base);
		ASSERT(is_header(hdr));
		switch (hdr & _TAG_HEADER_MASK) {
		case ARITYVAL_SUBTAG:
		    ptr = tuple_val_rel(obj,base);
		    arity = header_arity(hdr);
		    sum += arity + 1;
		    if (arity == 0) { /* Empty tuple -- unusual. */
			goto pop_next;
		    }
		    while (arity-- > 1) {
			obj = *++ptr;
			if (!IS_CONST(obj)) {
			    ESTACK_PUSH(s, obj);
			}
		    }
		    obj = *++ptr;
		    break;
		case FUN_SUBTAG:
		    {
			Eterm* bptr = fun_val_rel(obj,base);
			ErlFunThing* funp = (ErlFunThing *) bptr;
			unsigned eterms = 1 /* creator */ + funp->num_free;
			unsigned sz = thing_arityval(hdr);
			sum += 1 /* header */ + sz + eterms;
			bptr += 1 /* header */ + sz;
			while (eterms-- > 1) {
			  obj = *bptr++;
			  if (!IS_CONST(obj)) {
			    ESTACK_PUSH(s, obj);
			  }
			}
			obj = *bptr;
			break;
		    }
		case SUB_BINARY_SUBTAG:
		    {
			Eterm real_bin;
			ERTS_DECLARE_DUMMY(Uint offset); /* Not used. */
			Uint bitsize;
			Uint bitoffs;
			Uint extra_bytes;
			Eterm hdr;
			ERTS_GET_REAL_BIN_REL(obj, real_bin, offset, bitoffs, bitsize, base);
			if ((bitsize + bitoffs) > 8) {
			    sum += ERL_SUB_BIN_SIZE;
			    extra_bytes = 2;
			} else if ((bitsize + bitoffs) > 0) {
			    sum += ERL_SUB_BIN_SIZE;
			    extra_bytes = 1;
			} else {
			    extra_bytes = 0;
			}
			hdr = *binary_val_rel(real_bin,base);
			if (thing_subtag(hdr) == REFC_BINARY_SUBTAG) {
			    sum += PROC_BIN_SIZE;
			} else {
			    sum += heap_bin_size(binary_size_rel(obj,base)+extra_bytes);
			}
			goto pop_next;
		    }
		    break;
		case BIN_MATCHSTATE_SUBTAG:
		    erl_exit(ERTS_ABORT_EXIT,
			     "size_object: matchstate term not allowed");
		default:
		    sum += thing_arityval(hdr) + 1;
		    goto pop_next;
		}
	    }
	    break;
	case TAG_PRIMARY_IMMED1:
	pop_next:
	    if (ESTACK_ISEMPTY(s)) {
		DESTROY_ESTACK(s);
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] size was: %u\n", mypid, sum));
		return sum;
	    }
	    obj = ESTACK_POP(s);
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT, "size_object: bad tag for %#x\n", obj);
	}
    }
}

/*
 *  Machinery for sharing preserving information
 */

#define DECLARE_BITSTORE(s)						    \
    DECLARE_WSTACK(s);							    \
    int WSTK_CONCAT(s,_bitoffs) = 0;					    \
    int WSTK_CONCAT(s,_offset) = 0;					    \
    UWord WSTK_CONCAT(s,_buffer) = 0
#define DESTROY_BITSTORE(s) DESTROY_WSTACK(s)
#define BITSTORE_PUT(s,i)						    \
do {									    \
    WSTK_CONCAT(s,_buffer) |= i << WSTK_CONCAT(s,_bitoffs);		    \
    WSTK_CONCAT(s,_bitoffs) += 2;					    \
    if (WSTK_CONCAT(s,_bitoffs) >= 8*sizeof(UWord)) {			    \
	WSTACK_PUSH(s, WSTK_CONCAT(s,_buffer));				    \
	WSTK_CONCAT(s,_bitoffs) = 0;					    \
	WSTK_CONCAT(s,_buffer) = 0;					    \
    }									    \
} while(0)
#define BITSTORE_RESET(s)						    \
do {									    \
    if (WSTK_CONCAT(s,_bitoffs) > 0) {					    \
	WSTACK_PUSH(s, WSTK_CONCAT(s,_buffer));				    \
	WSTK_CONCAT(s,_bitoffs) = 0;					    \
    }									    \
    WSTK_CONCAT(s,_offset) = 0;						    \
} while(0)
#define BITSTORE_GET(s,i)						    \
do {									    \
    if (WSTK_CONCAT(s,_bitoffs) <= 0) {					    \
	WSTK_CONCAT(s,_buffer) = WSTK_SUBSCRIPT(s, WSTK_CONCAT(s,_offset)); \
	WSTK_CONCAT(s,_offset) += sizeof(UWord);			    \
	WSTK_CONCAT(s,_bitoffs) = 8*sizeof(UWord);			    \
    }									    \
    WSTK_CONCAT(s,_bitoffs) -= 2;					    \
    (i) = WSTK_CONCAT(s,_buffer) & 3;					    \
    WSTK_CONCAT(s,_buffer) >>= 2;					    \
} while(0)

#define BOXED_VISITED_MASK	 ((Eterm) 3)
#define BOXED_VISITED		 ((Eterm) 1)
#define BOXED_SHARED_UNPROCESSED ((Eterm) 2)
#define BOXED_SHARED_PROCESSED	 ((Eterm) 3)


/*
 *  Is an object in the local heap of a process?
 */

#define INHEAP_SIMPLE(p, ptr) (					\
    (OLD_HEAP(p) && OLD_HEAP(p) <= ptr && ptr < OLD_HEND(p)) ||	\
    (HEAP_START(p) <= ptr && ptr < HEAP_END(p))			\
  )
#define INHEAP(p, ptr) (					\
    INHEAP_SIMPLE(p, ptr) ||					\
    (force_local ? (force_local = 0, 1) : 0)			\
  )
#define COUNT_OFF_HEAP 0


/*
 *  Return the real size of an object and find sharing information
 *  This currently returns the same as erts_debug:size/1.
 *  It is argued whether the size of subterms in constant pools
 *  should be counted or not.
 */
#if HALFWORD_HEAP
Uint size_shared_rel(Eterm obj, Eterm* base)
#else
Uint size_shared(Eterm obj)
#endif
{
    Eterm saved_obj = obj;
    Uint sum = 0;
    Eterm* ptr;
    Process* myself;

    DECLARE_EQUEUE(s);
    DECLARE_BITSTORE(b);

    myself = erts_get_current_process();
    if (myself == NULL)
	return size_object(obj);

    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    ptr = list_val_rel(obj, base);
	    /* we're not counting anything that's outside our heap !!! */
	    if (!COUNT_OFF_HEAP && !INHEAP_SIMPLE(myself, ptr)) {
		goto pop_next;
	    }
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if it's visited, don't count it */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER ||
		primary_tag(head) == TAG_PRIMARY_HEADER) {
		goto pop_next;
	    }
	    /* else make it visited now */
	    switch (primary_tag(tail)) {
	    case TAG_PRIMARY_LIST:
		ptr[1] = (tail - TAG_PRIMARY_LIST) | TAG_PRIMARY_HEADER;
		break;
	    case TAG_PRIMARY_IMMED1:
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_IMMED1) | primary_tag(head);
		break;
	    case TAG_PRIMARY_BOXED:
		BITSTORE_PUT(b, primary_tag(head));
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_BOXED) | TAG_PRIMARY_HEADER;
		break;
	    }
	    /* and count it */
	    sum += 2;
	    if (!IS_CONST(head)) {
		EQUEUE_PUT(s, head);
	    }
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    ptr = boxed_val_rel(obj, base);
	    /* we're not counting anything that's outside our heap !!! */
	    if (!COUNT_OFF_HEAP && !INHEAP_SIMPLE(myself, ptr)) {
		goto pop_next;
	    }
	    hdr = *ptr;
	    /* if it's visited, don't count it */
	    if (primary_tag(hdr) != TAG_PRIMARY_HEADER) {
		goto pop_next;
	    }
	    /* else make it visited now */
	    *ptr = (hdr - primary_tag(hdr)) + BOXED_VISITED;
	    /* and count it */
	    ASSERT(is_header(hdr));
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
		sum += arity + 1;
		if (arity == 0) { /* Empty tuple -- unusual. */
		    goto pop_next;
		}
		while (arity-- > 0) {
		    obj = *++ptr;
		    if (!IS_CONST(obj)) {
			EQUEUE_PUT(s, obj);
		    }
		}
		goto pop_next;
	    }
	    case FUN_SUBTAG: {
		ErlFunThing* funp = (ErlFunThing *) ptr;
		unsigned eterms = 1 /* creator */ + funp->num_free;
		unsigned sz = thing_arityval(hdr);
		sum += 1 /* header */ + sz + eterms;
		ptr += 1 /* header */ + sz;
		while (eterms-- > 0) {
		    obj = *ptr++;
		    if (!IS_CONST(obj)) {
			EQUEUE_PUT(s, obj);
		    }
		}
		goto pop_next;
	    }
	    case SUB_BINARY_SUBTAG: {
		ErlSubBin* sb = (ErlSubBin *) ptr;
		Uint extra_bytes;
		Eterm hdr;
		ASSERT((sb->thing_word & ~BOXED_VISITED_MASK) == HEADER_SUB_BIN);
		if (sb->bitsize + sb->bitoffs > 8) {
		    sum += ERL_SUB_BIN_SIZE;
		    extra_bytes = 2;
		} else if (sb->bitsize + sb->bitoffs > 0) {
		    sum += ERL_SUB_BIN_SIZE;
		    extra_bytes = 1;
		} else {
		    extra_bytes = 0;
		}
		ptr = binary_val_rel(sb->orig, base);
		hdr = (*ptr) & ~BOXED_VISITED_MASK;
		if (thing_subtag(hdr) == REFC_BINARY_SUBTAG) {
		    sum += PROC_BIN_SIZE;
		} else {
		    ASSERT(thing_subtag(hdr) == HEAP_BINARY_SUBTAG);
		    sum += heap_bin_size(binary_size_rel(obj, base) + extra_bytes);
		}
		goto pop_next;
	    }
	    case BIN_MATCHSTATE_SUBTAG:
		erl_exit(ERTS_ABORT_EXIT,
			 "size_shared: matchstate term not allowed");
	    default:
		sum += thing_arityval(hdr) + 1;
		goto pop_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	pop_next:
	    if (EQUEUE_ISEMPTY(s)) {
		goto cleanup;
	    }
	    EQUEUE_GET(s, obj);
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT, "size_shared: bad tag for %#x\n", obj);
	}
    }

cleanup:
    obj = saved_obj;
    BITSTORE_RESET(b);
    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    ptr = list_val_rel(obj, base);
	    if (!COUNT_OFF_HEAP && !INHEAP_SIMPLE(myself, ptr)) {
		goto cleanup_next;
	    }
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if not already clean, clean it up */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER) {
		if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		    Eterm saved;
		    BITSTORE_GET(b, saved);
		    CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) | saved;
		    CDR(ptr) = tail = (tail - TAG_PRIMARY_HEADER) | TAG_PRIMARY_BOXED;
		} else {
		    CDR(ptr) = tail = (tail - TAG_PRIMARY_HEADER) | TAG_PRIMARY_LIST;
		}
	    } else if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) | primary_tag(tail);
		CDR(ptr) = tail = (tail - primary_tag(tail)) | TAG_PRIMARY_IMMED1;
	    } else {
		goto cleanup_next;
	    }
	    /* and its children too */
	    if (!IS_CONST(head)) {
		EQUEUE_PUT_UNCHECKED(s, head);
	    }
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    ptr = boxed_val_rel(obj, base);
	    if (!COUNT_OFF_HEAP && !INHEAP_SIMPLE(myself, ptr)) {
		goto cleanup_next;
	    }
	    hdr = *ptr;
	    /* if not already clean, clean it up */
	    if (primary_tag(hdr) == TAG_PRIMARY_HEADER) {
		goto cleanup_next;
	    }
	    else {
		ASSERT(primary_tag(hdr) == BOXED_VISITED);
		*ptr = hdr = (hdr - BOXED_VISITED) + TAG_PRIMARY_HEADER;
	    }
	    /* and its children too */
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
		if (arity == 0) { /* Empty tuple -- unusual. */
		    goto cleanup_next;
		}
		while (arity-- > 0) {
		    obj = *++ptr;
		    if (!IS_CONST(obj)) {
			EQUEUE_PUT_UNCHECKED(s, obj);
		    }
		}
		goto cleanup_next;
	    }
	    case FUN_SUBTAG: {
		ErlFunThing* funp = (ErlFunThing *) ptr;
		unsigned eterms = 1 /* creator */ + funp->num_free;
		unsigned sz = thing_arityval(hdr);
		ptr += 1 /* header */ + sz;
		while (eterms-- > 0) {
		    obj = *ptr++;
		    if (!IS_CONST(obj)) {
			EQUEUE_PUT_UNCHECKED(s, obj);
		    }
		}
		goto cleanup_next;
	    }
	    default:
		goto cleanup_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	cleanup_next:
	    if (EQUEUE_ISEMPTY(s)) {
		goto all_clean;
	    }
	    EQUEUE_GET(s, obj);
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT, "size_shared: bad tag for %#x\n", obj);
	}
    }

 all_clean:
    /* Return the result */
    DESTROY_EQUEUE(s);
    DESTROY_BITSTORE(b);
    return sum;
}


/*
 *  Copy a structure to a heap.
 */
#if HALFWORD_HEAP
Eterm copy_struct_rel(Eterm obj, Uint sz, Eterm** hpp, ErlOffHeap* off_heap,
                      Eterm* src_base, Eterm* dst_base)
#else
Eterm copy_struct(Eterm obj, Uint sz, Eterm** hpp, ErlOffHeap* off_heap)
#endif
{
    char* hstart;
    Uint hsize;
    Eterm* htop;
    Eterm* hbot;
    Eterm* hp;
    Eterm* objp;
    Eterm* tp;
    Eterm  res;
    Eterm  elem;
    Eterm* tailp;
    Eterm* argp;
    Eterm* const_tuple;
    Eterm hdr;
    int i;
#ifdef DEBUG
    Eterm org_obj = obj;
    Uint org_sz = sz;
    Eterm mypid = erts_get_current_pid();
#endif

    if (IS_CONST(obj))
	return obj;

    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] copy_struct %p\n", mypid, obj));

    DTRACE1(copy_struct, (int32_t)sz);

    hp = htop = *hpp;
    hbot   = htop + sz;
    hstart = (char *)htop;
    hsize = (char*) hbot - hstart;
    const_tuple = 0;

    /* Copy the object onto the heap */
    switch (primary_tag(obj)) {
    case TAG_PRIMARY_LIST:
	argp = &res;
	objp = list_val_rel(obj,src_base);
	goto L_copy_list;
    case TAG_PRIMARY_BOXED: argp = &res; goto L_copy_boxed;
    default:
	erl_exit(ERTS_ABORT_EXIT,
		 "%s, line %d: Internal error in copy_struct: 0x%08x\n",
		 __FILE__, __LINE__,obj);
    }

 L_copy:
    while (hp != htop) {
	obj = *hp;

	switch (primary_tag(obj)) {
	case TAG_PRIMARY_IMMED1:
	    hp++;
	    break;
	case TAG_PRIMARY_LIST:
	    objp = list_val_rel(obj,src_base);
	#if !HALFWORD_HEAP || defined(DEBUG)
	    if (in_area(objp,hstart,hsize)) {
		ASSERT(!HALFWORD_HEAP);
		hp++;
		break;
	    }
	#endif
	    argp = hp++;
	    /* Fall through */

	L_copy_list:
	    tailp = argp;
	    for (;;) {
		tp = tailp;
		elem = CAR(objp);
		if (IS_CONST(elem)) {
		    hbot -= 2;
		    CAR(hbot) = elem;
		    tailp = &CDR(hbot);
		}
		else {
		    CAR(htop) = elem;
		#if HALFWORD_HEAP
		    CDR(htop) = CDR(objp);
		    *tailp = make_list_rel(htop,dst_base);
		    htop += 2;
		    goto L_copy;
		#else
		    tailp = &CDR(htop);
		    htop += 2;
		#endif
		}
		ASSERT(!HALFWORD_HEAP || tp < hp || tp >= hbot);
		*tp = make_list_rel(tailp - 1, dst_base);
		obj = CDR(objp);
		if (!is_list(obj)) {
		    break;
		}
		objp = list_val_rel(obj,src_base);
	    }
	    switch (primary_tag(obj)) {
	    case TAG_PRIMARY_IMMED1: *tailp = obj; goto L_copy;
	    case TAG_PRIMARY_BOXED: argp = tailp; goto L_copy_boxed;
	    default:
		erl_exit(ERTS_ABORT_EXIT,
			 "%s, line %d: Internal error in copy_struct: 0x%08x\n",
			 __FILE__, __LINE__,obj);
	    }
	    
	case TAG_PRIMARY_BOXED:
	#if !HALFWORD_HEAP || defined(DEBUG)
	    if (in_area(boxed_val_rel(obj,src_base),hstart,hsize)) {
		ASSERT(!HALFWORD_HEAP);
		hp++;
		break;
	    }
	#endif
	    argp = hp++;

	L_copy_boxed:
	    objp = boxed_val_rel(obj, src_base);
	    hdr = *objp;
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG:
		{
		    int const_flag = 1; /* assume constant tuple */
		    i = arityval(hdr);
		    *argp = make_tuple_rel(htop, dst_base);
		    tp = htop;	/* tp is pointer to new arity value */
		    *htop++ = *objp++; /* copy arity value */
		    while (i--) {
			elem = *objp++;
			if (!IS_CONST(elem)) {
			    const_flag = 0;
			}
			*htop++ = elem;
		    }
		    if (const_flag) {
			const_tuple = tp; /* this is the latest const_tuple */
		    }
		}
		break;
	    case REFC_BINARY_SUBTAG:
		{
		    ProcBin* pb;

		    pb = (ProcBin *) objp;
		    if (pb->flags) {
			erts_emasculate_writable_binary(pb);
		    }
		    i = thing_arityval(*objp) + 1;
		    hbot -= i;
		    tp = hbot;
		    while (i--)  {
			*tp++ = *objp++;
		    }
		    *argp = make_binary_rel(hbot, dst_base);
		    pb = (ProcBin*) hbot;
		    erts_refc_inc(&pb->val->refc, 2);
		    pb->next = off_heap->first;
		    pb->flags = 0;
		    off_heap->first = (struct erl_off_heap_header*) pb;
		    OH_OVERHEAD(off_heap, pb->size / sizeof(Eterm));
		}
		break;
	    case SUB_BINARY_SUBTAG:
		{
		    ErlSubBin* sb = (ErlSubBin *) objp;
		    Eterm real_bin = sb->orig;
		    Uint bit_offset = sb->bitoffs;
		    Uint bit_size = sb -> bitsize;
		    Uint offset = sb->offs;
		    size_t size = sb->size;
		    Uint extra_bytes;
		    Uint real_size;
		    if ((bit_size + bit_offset) > 8) {
			extra_bytes = 2;
		    } else if ((bit_size + bit_offset) > 0) {
			extra_bytes = 1;
		    } else {
			extra_bytes = 0;
		    } 
		    real_size = size+extra_bytes;
		    objp = binary_val_rel(real_bin,src_base);
		    if (thing_subtag(*objp) == HEAP_BINARY_SUBTAG) {
			ErlHeapBin* from = (ErlHeapBin *) objp;
			ErlHeapBin* to;
			i = heap_bin_size(real_size);
			hbot -= i;
			to = (ErlHeapBin *) hbot;
			to->thing_word = header_heap_bin(real_size);
			to->size = real_size;
			sys_memcpy(to->data, ((byte *)from->data)+offset, real_size);
		    } else {
			ProcBin* from = (ProcBin *) objp;
			ProcBin* to;
			
			ASSERT(thing_subtag(*objp) == REFC_BINARY_SUBTAG);
			if (from->flags) {
			    erts_emasculate_writable_binary(from);
			}
			hbot -= PROC_BIN_SIZE;
			to = (ProcBin *) hbot;
			to->thing_word = HEADER_PROC_BIN;
			to->size = real_size;
			to->val = from->val;
			erts_refc_inc(&to->val->refc, 2);
			to->bytes = from->bytes + offset;
			to->next = off_heap->first;
			to->flags = 0;
			off_heap->first = (struct erl_off_heap_header*) to;
			OH_OVERHEAD(off_heap, to->size / sizeof(Eterm));
		    }
		    *argp = make_binary_rel(hbot, dst_base);
		    if (extra_bytes != 0) {
			ErlSubBin* res;
			hbot -= ERL_SUB_BIN_SIZE;
			res = (ErlSubBin *) hbot;
			res->thing_word = HEADER_SUB_BIN;
			res->size = size;
			res->bitsize = bit_size;
			res->bitoffs = bit_offset;
			res->offs = 0;
			res->is_writable = 0;
			res->orig = *argp;
			*argp = make_binary_rel(hbot, dst_base);
		    }
		    break;
		}
		break;
	    case FUN_SUBTAG:
		{
		    ErlFunThing* funp = (ErlFunThing *) objp;

		    i =  thing_arityval(hdr) + 2 + funp->num_free;
		    tp = htop;
		    while (i--)  {
			*htop++ = *objp++;
		    }
		    funp = (ErlFunThing *) tp;
		    funp->next = off_heap->first;
		    off_heap->first = (struct erl_off_heap_header*) funp;
		    erts_refc_inc(&funp->fe->refc, 2);
		    *argp = make_fun_rel(tp, dst_base);
		}
		break;
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		{
		  ExternalThing *etp = (ExternalThing *) htop;

		  i =  thing_arityval(hdr) + 1;
		  tp = htop;

		  while (i--)  {
		    *htop++ = *objp++;
		  }

		  etp->next = off_heap->first;
		  off_heap->first = (struct erl_off_heap_header*)etp;
		  erts_refc_inc(&etp->node->refc, 2);

		  *argp = make_external_rel(tp, dst_base);
		}
		break;
	    case BIN_MATCHSTATE_SUBTAG:
		erl_exit(ERTS_ABORT_EXIT,
			 "copy_struct: matchstate term not allowed");
	    default:
		i = thing_arityval(hdr)+1;
		hbot -= i;
		tp = hbot;
		*argp = make_boxed_rel(hbot, dst_base);
		while (i--) {
		    *tp++ = *objp++;
		}
	    }
	    break;
	case TAG_PRIMARY_HEADER:
	    if (header_is_thing(obj) || hp == const_tuple) {
		hp += header_arity(obj) + 1;
	    } else {
		hp++;
	    }
	    break;
	}
    }

#ifdef DEBUG
    if (htop != hbot)
	erl_exit(ERTS_ABORT_EXIT,
		 "Internal error in copy_struct() when copying %T:"
		 " htop=%p != hbot=%p (sz=%beu)\n",
		 org_obj, htop, hbot, org_sz); 
#else
    if (htop > hbot) {
	erl_exit(ERTS_ABORT_EXIT,
		 "Internal error in copy_struct(): htop, hbot overrun\n");
    }
#endif
    *hpp = (Eterm *) (hstart+hsize);
    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] result is at %p\n", mypid, res));
    return res;
}


/*
 *  Machinery for the table used by the sharing preserving copier
 */

#define DECLARE_SHTABLE(s)						\
    DECLARE_ESTACK(s);							\
    Uint ESTK_CONCAT(s,_offset) = 0	       /* offset in bytes */
#define DESTROY_SHTABLE(s) DESTROY_ESTACK(s)
#define SHTABLE_NEXT(s)	ESTK_CONCAT(s,_offset)
#define SHTABLE_PUSH(s,x,y,b)						\
do {									\
    if (ESTK_CONCAT(s,_sp) > ESTK_CONCAT(s,_end) - 4) {			\
	erl_grow_stack(&ESTK_CONCAT(s,_start), &ESTK_CONCAT(s,_sp),	\
		&ESTK_CONCAT(s,_end));					\
    }									\
    *ESTK_CONCAT(s,_sp)++ = (x);					\
    *ESTK_CONCAT(s,_sp)++ = (y);					\
    *ESTK_CONCAT(s,_sp)++ = (Eterm) NULL;				\
    *ESTK_CONCAT(s,_sp)++ = (Eterm) (b);	/* bad in HALF_WORD */	\
    ESTK_CONCAT(s,_offset) += 4 * sizeof(Eterm);			\
} while(0)
#define SHTABLE_X(s,e) ESTK_SUBSCRIPT(s,e)
#define SHTABLE_Y(s,e) ESTK_SUBSCRIPT(s,(e)+sizeof(Eterm))
#define SHTABLE_FWD(s,e) ((Eterm *) ESTK_SUBSCRIPT(s,(e)+2*sizeof(Eterm)))
#define SHTABLE_FWD_UPD(s,e, p) (ESTK_SUBSCRIPT(s,(e)+2*sizeof(Eterm)) = (Eterm) p)
#define SHTABLE_REV(s,e) ((Eterm *) ESTK_SUBSCRIPT(s,(e)+3*sizeof(Eterm)))

#define LIST_SHARED_UNPROCESSED ((Eterm) 0)
#define LIST_SHARED_PROCESSED	((Eterm) 1)

#define HEAP_ELEM_TO_BE_FILLED	_unchecked_make_list(NULL)


/*
 *  Specialized macros for using/reusing the persistent state
 */

#define DECLARE_EQUEUE_INIT_INFO(s, info)					\
    Eterm* EQUE_CONCAT(s,_start) = info->queue_default;				\
    Eterm* EQUE_CONCAT(s,_front) = EQUE_CONCAT(s,_start);			\
    Eterm* EQUE_CONCAT(s,_back) = EQUE_CONCAT(s,_start);			\
    int	   EQUE_CONCAT(s,_possibly_empty) = 1;					\
    Eterm* EQUE_CONCAT(s,_end) = EQUE_CONCAT(s,_start) + DEF_EQUEUE_SIZE

#define DECLARE_BITSTORE_INIT_INFO(s, info)					\
    UWord* WSTK_CONCAT(s,_start) = info->bitstore_default;			\
    UWord* WSTK_CONCAT(s,_sp) = WSTK_CONCAT(s,_start);				\
    UWord* WSTK_CONCAT(s,_end) = WSTK_CONCAT(s,_start) + DEF_WSTACK_SIZE;	\
    int	   WSTK_CONCAT(s,_bitoffs) = 0;						\
    int	   WSTK_CONCAT(s,_offset) = 0;						\
    UWord  WSTK_CONCAT(s,_buffer) = 0

#define DECLARE_SHTABLE_INIT_INFO(s, info)					\
    Eterm* ESTK_CONCAT(s,_start) = info->shtable_default;			\
    Eterm* ESTK_CONCAT(s,_sp) = ESTK_CONCAT(s,_start);				\
    Eterm* ESTK_CONCAT(s,_end) = ESTK_CONCAT(s,_start) + DEF_ESTACK_SIZE;	\
    Uint   ESTK_CONCAT(s,_offset) = 0

#define DECLARE_EQUEUE_FROM_INFO(s, info)					\
    Eterm* EQUE_CONCAT(s,_start) = info->queue_start;				\
    Eterm* EQUE_CONCAT(s,_front) = EQUE_CONCAT(s,_start);			\
    Eterm* EQUE_CONCAT(s,_back) = EQUE_CONCAT(s,_start);			\
    int	   EQUE_CONCAT(s,_possibly_empty) = 1;					\
    Eterm* EQUE_CONCAT(s,_end) = info->queue_end

#define DECLARE_BITSTORE_FROM_INFO(s, info)					\
    UWord* WSTK_CONCAT(s,_start) = info->bitstore_start;			\
    int	   WSTK_CONCAT(s,_bitoffs) = 0;						\
    int	   WSTK_CONCAT(s,_offset) = 0;						\
    UWord  WSTK_CONCAT(s,_buffer) = 0

#define DECLARE_SHTABLE_FROM_INFO(s, info)					\
    Eterm* ESTK_CONCAT(s,_start) = info->shtable_start

/*
 *  Copy object "obj" preserving sharing.
 *  First half: count size and calculate sharing.
 *  NOTE: We do not support HALF_WORD (yet?).
 */
Uint copy_shared_calculate(Eterm obj, shcopy_info *info, unsigned flags)
{
    Uint sum;
    Uint e;
    unsigned sz;
    Eterm* ptr;
    Process* myself;
    int force_local = flags & ERTS_SHCOPY_FLG_TMP_BUF;

    DECLARE_EQUEUE_INIT_INFO(s, info);
    DECLARE_BITSTORE_INIT_INFO(b, info);
    DECLARE_SHTABLE_INIT_INFO(t, info);

    /* step #0:
       -------------------------------------------------------
       get rid of the easy cases first:
       - copying constants
       - if not a proper process, do flat copy
    */

    if (IS_CONST(obj))
	return 0;

    myself = erts_get_current_process();
    if (myself == NULL || (flags & ERTS_SHCOPY_FLG_NONE))
	return size_object(obj);

    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] copy_shared_calculate %p\n", myself->common.id, obj));
    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] message is %T\n", myself->common.id, obj));

    /* step #1:
       -------------------------------------------------------
       traverse the term and calculate the size;
       when traversing, transform as you do in size_shared
       but when you find shared objects:

       a. add entry in the table, indexed by i
       b. mark them:
	  b1. boxed terms, set header to (i | 11)
	      store (old header, NONV, NULL, backptr) in the entry
	  b2. cons cells, set CDR to NONV, set CAR to i
	      store (old CAR, old CDR, NULL, backptr) in the entry
    */

    sum = 0;

    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    ptr = list_val_rel(obj, base);
	    /* off heap list pointers are copied verbatim */
	    if (!INHEAP(myself, ptr)) {
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] bypassed copying %p is %T\n", myself->common.id, ptr, obj));
		goto pop_next;
	    }
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if it's visited, don't count it;
	       if not already shared, make it shared and store it in the table */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER ||
		primary_tag(head) == TAG_PRIMARY_HEADER) {
		if (tail != THE_NON_VALUE) {
		    e = SHTABLE_NEXT(t);
		    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] tabling L %p\n", myself->common.id, ptr));
		    SHTABLE_PUSH(t, head, tail, ptr);
		    CAR(ptr) = (e << _TAG_PRIMARY_SIZE) | LIST_SHARED_UNPROCESSED;
		    CDR(ptr) = THE_NON_VALUE;
		}
		goto pop_next;
	    }
	    /* else make it visited now */
	    switch (primary_tag(tail)) {
	    case TAG_PRIMARY_LIST:
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] mangling L/L %p\n", myself->common.id, ptr));
		CDR(ptr) = (tail - TAG_PRIMARY_LIST) | TAG_PRIMARY_HEADER;
		break;
	    case TAG_PRIMARY_IMMED1:
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] mangling L/I %p\n", myself->common.id, ptr));
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_IMMED1) | primary_tag(head);
		break;
	    case TAG_PRIMARY_BOXED:
		BITSTORE_PUT(b, primary_tag(head));
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] mangling L/B %p\n", myself->common.id, ptr));
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_BOXED) | TAG_PRIMARY_HEADER;
		break;
	    }
	    /* and count it */
	    sum += 2;
	    if (!IS_CONST(head)) {
		EQUEUE_PUT(s, head);
	    }
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    ptr = boxed_val_rel(obj, base);
	    /* off heap pointers to boxes are copied verbatim */
	    if (!INHEAP(myself, ptr)) {
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] bypassed copying %p is %T\n", myself->common.id, ptr, obj));
		goto pop_next;
	    }
	    hdr = *ptr;
	    /* if it's visited, don't count it;
	       if not already shared, make it shared and store it in the table */
	    if (primary_tag(hdr) != TAG_PRIMARY_HEADER) {
		if (primary_tag(hdr) == BOXED_VISITED) {
		    e = SHTABLE_NEXT(t);
		    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] tabling B %p\n", myself->common.id, ptr));
		    SHTABLE_PUSH(t, hdr, THE_NON_VALUE, ptr);
		    *ptr = (e << _TAG_PRIMARY_SIZE) | BOXED_SHARED_UNPROCESSED;
		}
		goto pop_next;
	    }
	    /* else make it visited now */
	    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] mangling B %p\n", myself->common.id, ptr));
	    *ptr = (hdr - primary_tag(hdr)) + BOXED_VISITED;
	    /* and count it */
	    ASSERT(is_header(hdr));
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
		sum += arity + 1;
		if (arity == 0) { /* Empty tuple -- unusual. */
		    goto pop_next;
		}
		while (arity-- > 0) {
		    obj = *++ptr;
		    if (!IS_CONST(obj)) {
			EQUEUE_PUT(s, obj);
		    }
		}
		goto pop_next;
	    }
	    case FUN_SUBTAG: {
		ErlFunThing* funp = (ErlFunThing *) ptr;
		unsigned eterms = 1 /* creator */ + funp->num_free;
		sz = thing_arityval(hdr);
		sum += 1 /* header */ + sz + eterms;
		ptr += 1 /* header */ + sz;
		while (eterms-- > 0) {
		    obj = *ptr++;
		    if (!IS_CONST(obj)) {
			EQUEUE_PUT(s, obj);
		    }
		}
		goto pop_next;
	    }
	    case SUB_BINARY_SUBTAG: {
		ErlSubBin* sb = (ErlSubBin *) ptr;
		Eterm real_bin = sb->orig;
		Uint bit_offset = sb->bitoffs;
		Uint bit_size = sb->bitsize;
		size_t size = sb->size;
		Uint extra_bytes;
		Eterm hdr;
		if (bit_size + bit_offset > 8) {
		    sum += ERL_SUB_BIN_SIZE;
		    extra_bytes = 2;
		} else if (bit_size + bit_offset > 0) {
		    sum += ERL_SUB_BIN_SIZE;
		    extra_bytes = 1;
		} else {
		    extra_bytes = 0;
		}
		ASSERT(is_boxed(rterm2wterm(real_bin, base)) &&
		       (((*boxed_val(rterm2wterm(real_bin, base))) &
			 (_TAG_HEADER_MASK - _BINARY_XXX_MASK - BOXED_VISITED_MASK))
			== _TAG_HEADER_REFC_BIN));
		hdr = *_unchecked_binary_val(rterm2wterm(real_bin, base)) & ~BOXED_VISITED_MASK;
		if (thing_subtag(hdr) == HEAP_BINARY_SUBTAG) {
		    sum += heap_bin_size(size+extra_bytes);
		} else {
		    ASSERT(thing_subtag(hdr) == REFC_BINARY_SUBTAG);
		    sum += PROC_BIN_SIZE;
		}
		goto pop_next;
	    }
	    case BIN_MATCHSTATE_SUBTAG:
		erl_exit(ERTS_ABORT_EXIT,
			 "size_shared: matchstate term not allowed");
	    default:
		sum += thing_arityval(hdr) + 1;
		goto pop_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	pop_next:
	    if (EQUEUE_ISEMPTY(s)) {
		// add sentinel to the table
		SHTABLE_PUSH(t, THE_NON_VALUE, THE_NON_VALUE, NULL);
		// store persistent info
		BITSTORE_RESET(b);
		info->queue_start = EQUE_CONCAT(s,_start);
		info->queue_end = EQUE_CONCAT(s,_end);
		info->bitstore_start = WSTK_CONCAT(b,_start);
		info->shtable_start = ESTK_CONCAT(t,_start);
		// single point of return: the size of the object
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] size was: %u\n", myself->common.id, sum));
		return sum;
	    }
	    EQUEUE_GET(s, obj);
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT, "[pid=%T] size_shared: bad tag for %#x\n", obj);
	}
    }
}


/*
 *  Copy object "obj" preserving sharing.
 *  Second half: copy and restore the object.
 *  NOTE: We do not support HALF_WORD (yet?).
 */
Uint copy_shared_perform(Eterm obj, Uint size, shcopy_info *info, Eterm** hpp, ErlOffHeap* off_heap, unsigned flags)
{
    Uint e;
    unsigned sz;
    Eterm* ptr;
    Eterm* hp;
    Eterm* hscan;
    Eterm result;
    Eterm* resp;
    unsigned remaining;
    Process* myself;
    int force_local = flags & ERTS_SHCOPY_FLG_TMP_BUF;
#ifdef DEBUG
    Eterm saved_obj = obj;
#endif

    DECLARE_EQUEUE_FROM_INFO(s, info);
    DECLARE_BITSTORE_FROM_INFO(b, info);
    DECLARE_SHTABLE_FROM_INFO(t, info);

    /* step #0:
       -------------------------------------------------------
       get rid of the easy cases first:
       - copying constants
       - if not a proper process, do flat copy
    */

    if (IS_CONST(obj))
	return obj;

    myself = erts_get_current_process();
    if (myself == NULL || (flags & ERTS_SHCOPY_FLG_NONE))
	return copy_struct(obj, size, hpp, off_heap);

    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] copy_shared_perform %p\n", myself->common.id, obj));

    /* step #2: was performed before this function was called
       -------------------------------------------------------
       allocate new space
    */

    hscan = hp = *hpp;

    /* step #3:
       -------------------------------------------------------
       traverse the term a second time and when traversing:
       a. if the object is marked as shared
	  a1. if the entry contains a forwarding ptr, use that
	  a2. otherwise, copy it to the new space and store the
	      forwarding ptr to the entry
      b. otherwise, reverse-transform as you do in size_shared
	 and copy to the new space
    */

    resp = &result;
    remaining = 0;
    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    ptr = list_val_rel(obj, base);
	    /* off heap list pointers are copied verbatim */
	    if (!INHEAP(myself, ptr)) {
		*resp = obj;
		goto cleanup_next;
	    }
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if it is shared */
	    if (tail == THE_NON_VALUE) {
		e = head >> _TAG_PRIMARY_SIZE;
		/* if it has been processed, just use the forwarding pointer */
		if (primary_tag(head) == LIST_SHARED_PROCESSED) {
		    *resp = make_list(SHTABLE_FWD(t, e));
		    goto cleanup_next;
		}
		/* else, let's process it now,
		   copy it and keep the forwarding pointer */
		else {
		    CAR(ptr) = (head - primary_tag(head)) + LIST_SHARED_PROCESSED;
		    head = SHTABLE_X(t, e);
		    tail = SHTABLE_Y(t, e);
		    ptr = &(SHTABLE_X(t, e));
		    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] tabled L %p is %p\n", myself->common.id, ptr, SHTABLE_REV(t, e)));
		    SHTABLE_FWD_UPD(t, e, hp);
		}
	    }
	    /* if not already clean, clean it up and copy it */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER) {
		if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		    Eterm saved;
		    BITSTORE_GET(b, saved);
		    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] unmangling L/B %p\n", myself->common.id, ptr));
		    CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) + saved;
		    CDR(ptr) = tail = (tail - TAG_PRIMARY_HEADER) + TAG_PRIMARY_BOXED;
		} else {
		    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] unmangling L/L %p\n", myself->common.id, ptr));
		    CDR(ptr) = tail = (tail - TAG_PRIMARY_HEADER) + TAG_PRIMARY_LIST;
		}
	    } else if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] unmangling L/I %p\n", myself->common.id, ptr));
		CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) | primary_tag(tail);
		CDR(ptr) = tail = (tail - primary_tag(tail)) | TAG_PRIMARY_IMMED1;
	    } else {
		ASSERT(0 && "cannot come here");
		goto cleanup_next;
	    }
	    /* and its children too */
	    if (IS_CONST(head)) {
		CAR(hp) = head;
	    } else {
		EQUEUE_PUT_UNCHECKED(s, head);
		CAR(hp) = HEAP_ELEM_TO_BE_FILLED;
	    }
	    *resp = make_list(hp);
	    resp = &(CDR(hp));
	    hp += 2;
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    ptr = boxed_val_rel(obj, base);
	    /* off heap pointers to boxes are copied verbatim */
	    if (!INHEAP(myself, ptr)) {
		*resp = obj;
		goto cleanup_next;
	    }
	    hdr = *ptr;
	    /* clean it up, unless it's already clean or shared and processed */
	    switch (primary_tag(hdr)) {
	    case TAG_PRIMARY_HEADER:
		ASSERT(0 && "cannot come here");
	    /* if it is shared and has been processed,
	       just use the forwarding pointer */
	    case BOXED_SHARED_PROCESSED:
		e = hdr >> _TAG_PRIMARY_SIZE;
		*resp = make_boxed(SHTABLE_FWD(t, e));
		goto cleanup_next;
	    /* if it is shared but has not been processed yet, let's process
	       it now: copy it and keep the forwarding pointer */
	    case BOXED_SHARED_UNPROCESSED:
		e = hdr >> _TAG_PRIMARY_SIZE;
		*ptr = (hdr - primary_tag(hdr)) + BOXED_SHARED_PROCESSED;
		hdr = SHTABLE_X(t, e);
		ASSERT(primary_tag(hdr) == BOXED_VISITED);
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] tabled B %p is %p\n", myself->common.id, ptr, SHTABLE_REV(t, e)));
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] unmangling B %p\n", myself->common.id, ptr));
		SHTABLE_X(t, e) = hdr = (hdr - BOXED_VISITED) + TAG_PRIMARY_HEADER;
		SHTABLE_FWD_UPD(t, e, hp);
		break;
	    case BOXED_VISITED:
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] unmangling B %p\n", myself->common.id, ptr));
		*ptr = hdr = (hdr - BOXED_VISITED) + TAG_PRIMARY_HEADER;
		break;
	    }
	    /* and its children too */
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
		*resp = make_boxed(hp);
		*hp++ = hdr;
		while (arity-- > 0) {
		    obj = *++ptr;
		    if (IS_CONST(obj)) {
			*hp++ = obj;
		    } else {
			EQUEUE_PUT_UNCHECKED(s, obj);
			*hp++ = HEAP_ELEM_TO_BE_FILLED;
		    }
		}
		goto cleanup_next;
	    }
	    case FUN_SUBTAG: {
		ErlFunThing* funp = (ErlFunThing *) ptr;
		unsigned eterms = 1 /* creator */ + funp->num_free;
		sz = thing_arityval(hdr);
		funp = (ErlFunThing *) hp;
		*resp = make_fun(hp);
		*hp++ = hdr;
		ptr++;
		while (sz-- > 0) {
		    *hp++ = *ptr++;
		}
		while (eterms-- > 0) {
		    obj = *ptr++;
		    if (IS_CONST(obj)) {
			*hp++ = obj;
		    } else {
			EQUEUE_PUT_UNCHECKED(s, obj);
			*hp++ = HEAP_ELEM_TO_BE_FILLED;
		    }
		}
		funp->next = off_heap->first;
		off_heap->first = (struct erl_off_heap_header*) funp;
		erts_refc_inc(&funp->fe->refc, 2);
		goto cleanup_next;
	    }
	    case REFC_BINARY_SUBTAG: {
		ProcBin* pb = (ProcBin *) ptr;
		sz = thing_arityval(hdr);
		if (pb->flags) {
		    erts_emasculate_writable_binary(pb);
		}
		pb = (ProcBin *) hp;
		*resp = make_binary(hp);
		*hp++ = hdr;
		ptr++;
		while (sz-- > 0) {
		    *hp++ = *ptr++;
		}
		erts_refc_inc(&pb->val->refc, 2);
		pb->next = off_heap->first;
		pb->flags = 0;
		off_heap->first = (struct erl_off_heap_header*) pb;
		OH_OVERHEAD(off_heap, pb->size / sizeof(Eterm));
		goto cleanup_next;
	    }
	    case SUB_BINARY_SUBTAG: {
		ErlSubBin* sb = (ErlSubBin *) ptr;
		Eterm real_bin = sb->orig;
		Uint bit_offset = sb->bitoffs;
		Uint bit_size = sb->bitsize;
		Uint offset = sb->offs;
		size_t size = sb->size;
		Uint extra_bytes;
		Uint real_size;
		if ((bit_size + bit_offset) > 8) {
		    extra_bytes = 2;
		} else if ((bit_size + bit_offset) > 0) {
		    extra_bytes = 1;
		} else {
		    extra_bytes = 0;
		}
		real_size = size+extra_bytes;
		ASSERT(is_boxed(rterm2wterm(real_bin, base)) &&
		       (((*boxed_val(rterm2wterm(real_bin, base))) &
			 (_TAG_HEADER_MASK - _BINARY_XXX_MASK - BOXED_VISITED_MASK))
			== _TAG_HEADER_REFC_BIN));
		ptr = _unchecked_binary_val(rterm2wterm(real_bin, base));
		*resp = make_binary(hp);
		if (extra_bytes != 0) {
		    ErlSubBin* res = (ErlSubBin *) hp;
		    hp += ERL_SUB_BIN_SIZE;
		    res->thing_word = HEADER_SUB_BIN;
		    res->size = size;
		    res->bitsize = bit_size;
		    res->bitoffs = bit_offset;
		    res->offs = 0;
		    res->is_writable = 0;
		    res->orig = make_binary(hp);
		}
		if (thing_subtag(*ptr & ~BOXED_VISITED_MASK) == HEAP_BINARY_SUBTAG) {
		    ErlHeapBin* from = (ErlHeapBin *) ptr;
		    ErlHeapBin* to = (ErlHeapBin *) hp;
		    hp += heap_bin_size(real_size);
		    to->thing_word = header_heap_bin(real_size);
		    to->size = real_size;
		    sys_memcpy(to->data, ((byte *)from->data)+offset, real_size);
		} else {
		    ProcBin* from = (ProcBin *) ptr;
		    ProcBin* to = (ProcBin *) hp;
		    ASSERT(thing_subtag(*ptr & ~BOXED_VISITED_MASK) == REFC_BINARY_SUBTAG);
		    if (from->flags) {
			erts_emasculate_writable_binary(from);
		    }
		    hp += PROC_BIN_SIZE;
		    to->thing_word = HEADER_PROC_BIN;
		    to->size = real_size;
		    to->val = from->val;
		    erts_refc_inc(&to->val->refc, 2);
		    to->bytes = from->bytes + offset;
		    to->next = off_heap->first;
		    to->flags = 0;
		    off_heap->first = (struct erl_off_heap_header*) to;
		    OH_OVERHEAD(off_heap, to->size / sizeof(Eterm));
		}
		goto cleanup_next;
	    }
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG: {
		ExternalThing *etp = (ExternalThing *) hp;
		sz = thing_arityval(hdr);
		*resp = make_external(hp);
		*hp++ = hdr;
		ptr++;
		while (sz-- > 0) {
		    *hp++ = *ptr++;
		}
		etp->next = off_heap->first;
		off_heap->first = (struct erl_off_heap_header*) etp;
		erts_refc_inc(&etp->node->refc, 2);
		goto cleanup_next;
	    }
	    default:
		sz = thing_arityval(hdr);
		*resp = make_boxed(hp);
		*hp++ = hdr;
		ptr++;
		while (sz-- > 0) {
		    *hp++ = *ptr++;
		}
		goto cleanup_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	    *resp = obj;
	cleanup_next:
	    if (EQUEUE_ISEMPTY(s)) {
		goto all_clean;
	    }
	    EQUEUE_GET(s, obj);
	    for (;;) {
		ASSERT(hscan < hp);
		if (remaining == 0) {
		    if (*hscan == HEAP_ELEM_TO_BE_FILLED) {
			resp = hscan;
			hscan += 2;
			break; /* scanning loop */
		    } else if (primary_tag(*hscan) == TAG_PRIMARY_HEADER) {
			switch (*hscan & _TAG_HEADER_MASK) {
			case ARITYVAL_SUBTAG:
			    remaining = header_arity(*hscan);
			    hscan++;
			    break;
			case FUN_SUBTAG: {
			    ErlFunThing* funp = (ErlFunThing *) hscan;
			    hscan += 1 + thing_arityval(*hscan);
			    remaining = 1 + funp->num_free;
			    break;
			}
			case SUB_BINARY_SUBTAG:
			    ASSERT(((ErlSubBin *) hscan)->bitoffs +
				   ((ErlSubBin *) hscan)->bitsize > 0);
			    hscan += ERL_SUB_BIN_SIZE;
			    break;
			default:
			    hscan += 1 + thing_arityval(*hscan);
			    break;
			}
		    } else {
			hscan++;
		    }
		} else if (*hscan == HEAP_ELEM_TO_BE_FILLED) {
		    resp = hscan++;
		    remaining--;
		    break; /* scanning loop */
		} else {
		    hscan++;
		    remaining--;
		}
	    }
	    ASSERT(resp < hp);
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT, "size_shared: bad tag for %#x\n", obj);
	}
    }

    /* step #4:
       -------------------------------------------------------
       traverse the table and reverse-transform all stored entries
    */

all_clean:
    for (e = 0; ; e += 4*sizeof(Eterm)) {
	ptr = SHTABLE_REV(t, e);
	if (ptr == NULL)
	    break;
	VERBOSE(DEBUG_SHCOPY, ("[copy] restoring shared: %x\n", ptr));
	/* entry was a list */
	if (SHTABLE_Y(t, e) != THE_NON_VALUE) {
	    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] untabling L %p\n", myself->common.id, ptr));
	    CAR(ptr) = SHTABLE_X(t, e);
	    CDR(ptr) = SHTABLE_Y(t, e);
	}
	/* entry was boxed */
	else {
	    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] untabling B %p\n", myself->common.id, ptr));
	    *ptr = SHTABLE_X(t, e);
	    ASSERT(primary_tag(*ptr) == TAG_PRIMARY_HEADER);
	}
    }

#ifdef DEBUG
    if (eq(saved_obj, result) == 0) {
	erts_fprintf(stderr, "original = %T\n", saved_obj);
	erts_fprintf(stderr, "copy = %T\n", result);
	erl_exit(ERTS_ABORT_EXIT, "copy (shared) not equal to source\n");
    }
#endif

    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] original was %T\n", myself->common.id, saved_obj));
    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] copy is %T\n", myself->common.id, result));
    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] result is at %p\n", myself->common.id, result));

    ASSERT(hp == *hpp + size);
    *hpp = hp;
    return result;
}


/*
 * Copy a term that is guaranteed to be contained in a single
 * heap block. The heap block is copied word by word, and any
 * pointers are offsetted to point correctly in the new location.
 *
 * Typically used to copy a term from an ets table.
 *
 * NOTE: Assumes that term is a tuple (ptr is an untagged tuple ptr).
 */
#if HALFWORD_HEAP
Eterm copy_shallow_rel(Eterm* ptr, Uint sz, Eterm** hpp, ErlOffHeap* off_heap,
		       Eterm* src_base)
#else
Eterm copy_shallow(Eterm* ptr, Uint sz, Eterm** hpp, ErlOffHeap* off_heap)
#endif
{
    Eterm* tp = ptr;
    Eterm* hp = *hpp;
    const Eterm res = make_tuple(hp);
#if HALFWORD_HEAP
    const Sint offs = COMPRESS_POINTER(hp - (tp - src_base));
#else
    const Sint offs = (hp - tp) * sizeof(Eterm);
#endif

    while (sz--) {
	Eterm val = *tp++;

	switch (primary_tag(val)) {
	case TAG_PRIMARY_IMMED1:
	    *hp++ = val;
	    break;
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    *hp++ = byte_offset_ptr(val, offs);
	    break;
	case TAG_PRIMARY_HEADER:
	    *hp++ = val;
	    switch (val & _HEADER_SUBTAG_MASK) {
	    case ARITYVAL_SUBTAG:
		break;
	    case REFC_BINARY_SUBTAG:
		{
		    ProcBin* pb = (ProcBin *) (tp-1);
		    erts_refc_inc(&pb->val->refc, 2);
		    OH_OVERHEAD(off_heap, pb->size / sizeof(Eterm));
		}
		goto off_heap_common;

	    case FUN_SUBTAG:
		{
		    ErlFunThing* funp = (ErlFunThing *) (tp-1);
		    erts_refc_inc(&funp->fe->refc, 2);
		}
		goto off_heap_common;

	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		{
		    ExternalThing* etp = (ExternalThing *) (tp-1);
		    erts_refc_inc(&etp->node->refc, 2);
		}
	    off_heap_common:
		{
		    struct erl_off_heap_header* ohh = (struct erl_off_heap_header*)(hp-1);
		    int tari = thing_arityval(val);
		    
		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
		    ohh->next = off_heap->first;
		    off_heap->first = ohh;
		}
		break;
	    default:
		{
		    int tari = header_arity(val);
    
		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
		}
		break;
	    }
	    break;
	}
    }
    *hpp = hp;

    return res;
}

/* Move all terms in heap fragments into heap. The terms must be guaranteed to 
 * be contained within the fragments. The source terms are destructed with
 * move markers.
 * Typically used to copy a multi-fragmented message (from NIF).
 */
void move_multi_frags(Eterm** hpp, ErlOffHeap* off_heap, ErlHeapFragment* first,
		      Eterm* refs, unsigned nrefs)
{
    ErlHeapFragment* bp;
    Eterm* hp_start = *hpp;
    Eterm* hp_end;
    Eterm* hp;
    unsigned i;

    for (bp=first; bp!=NULL; bp=bp->next) {
	move_one_frag(hpp, bp->mem, bp->used_size, off_heap);
	OH_OVERHEAD(off_heap, bp->off_heap.overhead);
    }
    hp_end = *hpp;
    for (hp=hp_start; hp<hp_end; ++hp) {
	Eterm* ptr;
	Eterm val;
	Eterm gval = *hp;
	switch (primary_tag(gval)) {
	case TAG_PRIMARY_BOXED:
	    ptr = boxed_val(gval);
	    val = *ptr;
	    if (IS_MOVED_BOXED(val)) {
		ASSERT(is_boxed(val));
		*hp = val;
	    }
	    break;
	case TAG_PRIMARY_LIST:
	    ptr = list_val(gval);
	    val = *ptr;
	    if (IS_MOVED_CONS(val)) {
		*hp = ptr[1];
	    }
	    break;
	case TAG_PRIMARY_HEADER:
	    if (header_is_thing(gval)) {
		hp += thing_arityval(gval);
	    }
	    break;
	}
    }
    for (i=0; i<nrefs; ++i) {
	refs[i] = follow_moved(refs[i]);
    }
}

static void
move_one_frag(Eterm** hpp, Eterm* src, Uint src_sz, ErlOffHeap* off_heap)
{
    Eterm* ptr = src;
    Eterm* end = ptr + src_sz;
    Eterm dummy_ref;
    Eterm* hp = *hpp;

    while (ptr != end) {
	Eterm val;
	ASSERT(ptr < end);
	val = *ptr;
	ASSERT(val != ERTS_HOLE_MARKER);
	if (is_header(val)) {
	    struct erl_off_heap_header* hdr = (struct erl_off_heap_header*)hp;
	    ASSERT(ptr + header_arity(val) < end);
	    MOVE_BOXED(ptr, val, hp, &dummy_ref);	    
	    switch (val & _HEADER_SUBTAG_MASK) {
	    case REFC_BINARY_SUBTAG:
	    case FUN_SUBTAG:
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		hdr->next = off_heap->first;
		off_heap->first = hdr;
		break;
	    }
	}
	else { /* must be a cons cell */
	    ASSERT(ptr+1 < end);
	    MOVE_CONS(ptr, val, hp, &dummy_ref);
	    ptr += 2;
	}
    }
    *hpp = hp;
}
