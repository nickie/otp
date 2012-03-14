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

#ifdef DEBUG
// !!! nickie: turn on assertions!
#define NDEBUG 1
#endif

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_gc.h"
#include "erl_nmgc.h"
#include "big.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "dtrace-wrapper.h"

#ifdef HYBRID
MA_STACK_DECLARE(src);
MA_STACK_DECLARE(dst);
MA_STACK_DECLARE(offset);
#endif

static void move_one_frag(Eterm** hpp, Eterm* src, Uint src_sz, ErlOffHeap*);

void
init_copy(void)
{
#ifdef HYBRID
    MA_STACK_ALLOC(src);
    MA_STACK_ALLOC(dst);
    MA_STACK_ALLOC(offset);
#endif
}

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

        erts_snprintf(proc_name, sizeof(proc_name), "%T", to->id);
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

    DECLARE_ESTACK(s);
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
    (i) = WSTK_CONCAT(s,_buffer) & 3;                                       \
    WSTK_CONCAT(s,_buffer) >>= 2;                                           \
} while(0)

#define BOXED_VISITED_MASK       ((Eterm) 3)
#define BOXED_VISITED            ((Eterm) 1)
#define BOXED_SHARED_UNPROCESSED ((Eterm) 2)
#define BOXED_SHARED_PROCESSED   ((Eterm) 3)


/*
 *  Return the real size of an object and find sharing information
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

    DECLARE_ESTACK(s);
    DECLARE_BITSTORE(b);
    for (;;) {
	VERBOSE(DEBUG_NICKIE, ("[size] visiting: %x ", obj));
        switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    VERBOSE(DEBUG_NICKIE, ("L"));
	    ptr = list_val_rel(obj, base);
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if it's visited, don't count it */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER ||
		primary_tag(head) == TAG_PRIMARY_HEADER) {
		VERBOSE(DEBUG_NICKIE, ("!"));
		goto pop_next;
	    }
	    /* else make it visited now */
	    switch (primary_tag(tail)) {
	    case TAG_PRIMARY_LIST:
		VERBOSE(DEBUG_NICKIE, ("/L"));
		ptr[1] = (tail - TAG_PRIMARY_LIST) | TAG_PRIMARY_HEADER;
		break;
	    case TAG_PRIMARY_IMMED1:
		VERBOSE(DEBUG_NICKIE, ("/I"));
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_IMMED1) | primary_tag(head);
		break;
	    case TAG_PRIMARY_BOXED:
		VERBOSE(DEBUG_NICKIE, ("/B saved %d", primary_tag(head)));
		BITSTORE_PUT(b, primary_tag(head));
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_BOXED) | TAG_PRIMARY_HEADER;
		break;
	    }
	    /* and count it */
	    sum += 2;
	    if (!IS_CONST(head)) {
		ESTACK_PUSH(s, head);
	    }
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    VERBOSE(DEBUG_NICKIE, ("B"));
	    ptr = boxed_val_rel(obj, base);
	    hdr = *ptr;
	    /* if it's visited, don't count it */
	    if (primary_tag(hdr) != TAG_PRIMARY_HEADER) {
		VERBOSE(DEBUG_NICKIE, ("!"));
		goto pop_next;
	    }
	    /* else make it visited now */
	    *ptr = (hdr - primary_tag(hdr)) + BOXED_VISITED;
	    /* and count it */
	    ASSERT(is_header(hdr));
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
		VERBOSE(DEBUG_NICKIE, ("/T"));
		sum += arity + 1;
		if (arity == 0) { /* Empty tuple -- unusual. */
		    VERBOSE(DEBUG_NICKIE, ("e"));
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
	    }
	    case FUN_SUBTAG: {
		ErlFunThing* funp = (ErlFunThing *) ptr;
		unsigned eterms = 1 /* creator */ + funp->num_free;
		unsigned sz = thing_arityval(hdr);
		VERBOSE(DEBUG_NICKIE, ("/F"));
		sum += 1 /* header */ + sz + eterms;
		ptr += 1 /* header */ + sz;
		while (eterms-- > 1) {
		    obj = *ptr++;
		    if (!IS_CONST(obj)) {
			ESTACK_PUSH(s, obj);
		    }
		}
		obj = *ptr;
		break;
	    }
	    case SUB_BINARY_SUBTAG: {
		Eterm real_bin;
		Uint bitsize;
		Uint bitoffs;
		Uint extra_bytes;
		Eterm hdr;
#if 0
		ERTS_DECLARE_DUMMY(Uint offset); /* Not used. */
		ERTS_GET_REAL_BIN_REL(obj, real_bin, offset, bitoffs, bitsize, base);
#else
		/* not using ERTS_GET_REAL_BIN_REL here
		   because we need to forget if it's visited */
		ErlSubBin* _sb = (ErlSubBin *) ptr;
		if ((_sb->thing_word & ~BOXED_VISITED_MASK) == HEADER_SUB_BIN) {
		    VERBOSE(DEBUG_NICKIE, ("/B1"));
		    real_bin = _sb->orig;
		    bitoffs = _sb->bitoffs;
		    bitsize = _sb->bitsize;
		} else {
		    VERBOSE(DEBUG_NICKIE, ("/B2"));
		    real_bin = obj;
		    bitoffs = bitsize = 0;
		}
		/* end of ERTS_GET_REAL_BIN_REL */
#endif
		if ((bitsize + bitoffs) > 8) {
		    sum += ERL_SUB_BIN_SIZE;
		    extra_bytes = 2;
		} else if ((bitsize + bitoffs) > 0) {
		    sum += ERL_SUB_BIN_SIZE;
		    extra_bytes = 1;
		} else {
		    extra_bytes = 0;
		}
#if 0
		hdr = *binary_val_rel(real_bin, base);
#else
		/* this cannot be checked so easily */
		ASSERT(is_boxed(rterm2wterm(real_bin, base)) &&
		       (((*boxed_val(rterm2wterm(real_bin, base))) &
			 (_TAG_HEADER_MASK - _BINARY_XXX_MASK - BOXED_VISITED_MASK))
			== _TAG_HEADER_REFC_BIN));
		hdr = *_unchecked_binary_val(rterm2wterm(real_bin, base));
		/* end of binary_val_rel */
#endif
		if (thing_subtag(hdr) == REFC_BINARY_SUBTAG) {
		    sum += PROC_BIN_SIZE;
		} else {
		    sum += heap_bin_size(binary_size_rel(obj,base)+extra_bytes);
		}
		goto pop_next;
	    }
	    case BIN_MATCHSTATE_SUBTAG:
		erl_exit(ERTS_ABORT_EXIT,
			 "size_shared: matchstate term not allowed");
	    default:
		VERBOSE(DEBUG_NICKIE, ("/D"));
		sum += thing_arityval(hdr) + 1;
		goto pop_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	    VERBOSE(DEBUG_NICKIE, ("I"));
	pop_next:
	    if (ESTACK_ISEMPTY(s)) {
		goto cleanup;
	    }
	    obj = ESTACK_POP(s);
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT, "size_shared: bad tag for %#x\n", obj);
	}
	VERBOSE(DEBUG_NICKIE, ("\n"));
    }

cleanup:
    VERBOSE(DEBUG_NICKIE, ("\n"));
    obj = saved_obj;
    BITSTORE_RESET(b);
    for (;;) {
	VERBOSE(DEBUG_NICKIE, ("[size] revisiting: %x ", obj));
        switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    VERBOSE(DEBUG_NICKIE, ("L"));
	    ptr = list_val_rel(obj, base);
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if not already clean, clean it up */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER) {
		if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		    Eterm saved;
		    BITSTORE_GET(b, saved);
		    VERBOSE(DEBUG_NICKIE, ("/B restoring %d", saved));
		    CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) | saved;
		    CDR(ptr) = tail = (tail - TAG_PRIMARY_HEADER) | TAG_PRIMARY_BOXED;
		} else {
		    VERBOSE(DEBUG_NICKIE, ("/L"));
		    ptr[1] = tail = (tail - TAG_PRIMARY_HEADER) | TAG_PRIMARY_LIST;
		}
	    } else if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		VERBOSE(DEBUG_NICKIE, ("/I"));
		CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) | primary_tag(tail);
		CDR(ptr) = tail = (tail - primary_tag(tail)) | TAG_PRIMARY_IMMED1;
	    } else {
		VERBOSE(DEBUG_NICKIE, ("!"));
		goto cleanup_next;
	    }
	    /* and its children too */
	    if (!IS_CONST(head)) {
		ESTACK_PUSH(s, head);
	    }
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    VERBOSE(DEBUG_NICKIE, ("B"));
	    ptr = boxed_val_rel(obj, base);
	    hdr = *ptr;
	    /* if not already clean, clean it up */
	    if (primary_tag(hdr) == TAG_PRIMARY_HEADER) {
		goto cleanup_next;
	    }
	    else {
		ASSERT(primary_tag(hdr) == BOXED_VISITED);
		*ptr = hdr = hdr - BOXED_VISITED;
	    }
	    /* and its children too */
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
		if (arity == 0) { /* Empty tuple -- unusual. */
		    goto cleanup_next;
		}
		while (arity-- > 1) {
		    obj = *++ptr;
		    if (!IS_CONST(obj)) {
			ESTACK_PUSH(s, obj);
		    }
		}
		obj = *++ptr;
		break;
	    }
	    case FUN_SUBTAG: {
		ErlFunThing* funp = (ErlFunThing *) ptr;
		unsigned eterms = 1 /* creator */ + funp->num_free;
		unsigned sz = thing_arityval(hdr);
		ptr += 1 /* header */ + sz;
		while (eterms-- > 1) {
		    obj = *ptr++;
		    if (!IS_CONST(obj)) {
			ESTACK_PUSH(s, obj);
		    }
		}
		obj = *ptr;
		break;
	    }
	    default:
		goto cleanup_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	cleanup_next:
	    if (ESTACK_ISEMPTY(s)) {
		goto all_clean;
	    }
	    obj = ESTACK_POP(s);
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT, "size_shared: bad tag for %#x\n", obj);
	}
	VERBOSE(DEBUG_NICKIE, ("\n"));
    }

 all_clean:
    VERBOSE(DEBUG_NICKIE, ("\n"));
    /* Return the result */
    DESTROY_ESTACK(s);
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
#endif

    if (IS_CONST(obj))
	return obj;

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
#ifndef HYBRID /* FIND ME! */
		    funp = (ErlFunThing *) tp;
		    funp->next = off_heap->first;
		    off_heap->first = (struct erl_off_heap_header*) funp;
		    erts_refc_inc(&funp->fe->refc, 2);
#endif
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
    return res;
}


/*
 *  Machinery for sharing preserving copy
 */

#define DECLARE_SHTABLE(s)						\
    DECLARE_ESTACK(s);							\
    Uint ESTK_CONCAT(s,_offset) = 0            /* offset in bytes */
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
#define SHTABLE_REV(s,e) ((Eterm *) ESTK_SUBSCRIPT(s,(e)+3*sizeof(Eterm)))

#define LIST_SHARED_UNPROCESSED ((Eterm) 0)
#define LIST_SHARED_PROCESSED   ((Eterm) 1)


/*
 *  Copy object "obj" to process "p" preserving sharing.
 *  We do not support HALF_WORD (yet?).
 */
Eterm
copy_shared(Eterm obj, Process* to)
{
    Eterm saved_obj = obj;
    Uint sum = 0;
    Uint e;
    Eterm* ptr;

    DECLARE_ESTACK(s);
    DECLARE_BITSTORE(b);
    DECLARE_SHTABLE(t);

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

    for (;;) {
	VERBOSE(DEBUG_NICKIE, ("[copy] visiting: %x ", obj));
        switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    VERBOSE(DEBUG_NICKIE, ("L"));
	    ptr = list_val_rel(obj, base);
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if it's visited, don't count it;
	       if not already shared, make it shared and store it in the table */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER ||
		primary_tag(head) == TAG_PRIMARY_HEADER) {
		VERBOSE(DEBUG_NICKIE, ("!"));
		if (tail != THE_NON_VALUE) {
		    e = SHTABLE_NEXT(t);
		    SHTABLE_PUSH(t, head, tail, ptr);
		    CAR(ptr) = (e << _TAG_PRIMARY_SIZE) | LIST_SHARED_UNPROCESSED;
		    CDR(ptr) = THE_NON_VALUE;
		}
		goto pop_next;
	    }
	    /* else make it visited now */
	    switch (primary_tag(tail)) {
	    case TAG_PRIMARY_LIST:
		VERBOSE(DEBUG_NICKIE, ("/L"));
		ptr[1] = (tail - TAG_PRIMARY_LIST) | TAG_PRIMARY_HEADER;
		break;
	    case TAG_PRIMARY_IMMED1:
		VERBOSE(DEBUG_NICKIE, ("/I"));
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_IMMED1) | primary_tag(head);
		break;
	    case TAG_PRIMARY_BOXED:
		VERBOSE(DEBUG_NICKIE, ("/B saved %d", primary_tag(head)));
		BITSTORE_PUT(b, primary_tag(head));
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_BOXED) | TAG_PRIMARY_HEADER;
		break;
	    }
	    /* and count it */
	    sum += 2;
	    if (!IS_CONST(head)) {
		ESTACK_PUSH(s, head);
	    }
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    VERBOSE(DEBUG_NICKIE, ("B"));
	    ptr = boxed_val_rel(obj, base);
	    hdr = *ptr;
	    /* if it's visited, don't count it;
	       if not already shared, make it shared and store it in the table */
	    if (primary_tag(hdr) != TAG_PRIMARY_HEADER) {
		VERBOSE(DEBUG_NICKIE, ("!"));
		if (primary_tag(hdr) == BOXED_VISITED) {
		    e = SHTABLE_NEXT(t);
		    SHTABLE_PUSH(t, hdr, THE_NON_VALUE, ptr);
		    *ptr = (e << _TAG_PRIMARY_SIZE) | BOXED_SHARED_UNPROCESSED;
		}
		goto pop_next;
	    }
	    /* else make it visited now */
	    *ptr = (hdr - primary_tag(hdr)) + BOXED_VISITED;
	    /* and count it */
	    ASSERT(is_header(hdr));
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
		VERBOSE(DEBUG_NICKIE, ("/T"));
		sum += arity + 1;
		if (arity == 0) { /* Empty tuple -- unusual. */
		    VERBOSE(DEBUG_NICKIE, ("e"));
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
	    }
	    case FUN_SUBTAG: {
		ErlFunThing* funp = (ErlFunThing *) ptr;
		unsigned eterms = 1 /* creator */ + funp->num_free;
		unsigned sz = thing_arityval(hdr);
		VERBOSE(DEBUG_NICKIE, ("/F"));
		sum += 1 /* header */ + sz + eterms;
		ptr += 1 /* header */ + sz;
		while (eterms-- > 1) {
		    obj = *ptr++;
		    if (!IS_CONST(obj)) {
			ESTACK_PUSH(s, obj);
		    }
		}
		obj = *ptr;
		break;
	    }
	    case SUB_BINARY_SUBTAG: {
		Eterm real_bin;
		Uint bitsize;
		Uint bitoffs;
		Uint extra_bytes;
		Eterm hdr;
#if 0
		ERTS_DECLARE_DUMMY(Uint offset); /* Not used. */
		ERTS_GET_REAL_BIN_REL(obj, real_bin, offset, bitoffs, bitsize, base);
#else
		/* not using ERTS_GET_REAL_BIN_REL here
		   because we need to forget if it's visited */
		ErlSubBin* _sb = (ErlSubBin *) ptr;
		if ((_sb->thing_word & ~BOXED_VISITED_MASK) == HEADER_SUB_BIN) {
		    VERBOSE(DEBUG_NICKIE, ("/B1"));
		    real_bin = _sb->orig;
		    bitoffs = _sb->bitoffs;
		    bitsize = _sb->bitsize;
		} else {
		    VERBOSE(DEBUG_NICKIE, ("/B2"));
		    real_bin = obj;
		    bitoffs = bitsize = 0;
		}
		/* end of ERTS_GET_REAL_BIN_REL */
#endif
		if ((bitsize + bitoffs) > 8) {
		    sum += ERL_SUB_BIN_SIZE;
		    extra_bytes = 2;
		} else if ((bitsize + bitoffs) > 0) {
		    sum += ERL_SUB_BIN_SIZE;
		    extra_bytes = 1;
		} else {
		    extra_bytes = 0;
		}
#if 0
		hdr = *binary_val_rel(real_bin, base);
#else
		/* this cannot be checked so easily */
		ASSERT(is_boxed(rterm2wterm(real_bin, base)) &&
		       (((*boxed_val(rterm2wterm(real_bin, base))) &
			 (_TAG_HEADER_MASK - _BINARY_XXX_MASK - BOXED_VISITED_MASK))
			== _TAG_HEADER_REFC_BIN));
		hdr = *_unchecked_binary_val(rterm2wterm(real_bin, base));
		/* end of binary_val_rel */
#endif
		if (thing_subtag(hdr) == REFC_BINARY_SUBTAG) {
		    sum += PROC_BIN_SIZE;
		} else {
		    sum += heap_bin_size(binary_size_rel(obj,base)+extra_bytes);
		}
		goto pop_next;
	    }
	    case BIN_MATCHSTATE_SUBTAG:
		erl_exit(ERTS_ABORT_EXIT,
			 "size_shared: matchstate term not allowed");
	    default:
		VERBOSE(DEBUG_NICKIE, ("/D"));
		sum += thing_arityval(hdr) + 1;
		goto pop_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	    VERBOSE(DEBUG_NICKIE, ("I"));
	pop_next:
	    if (ESTACK_ISEMPTY(s)) {
		goto alloc;
	    }
	    obj = ESTACK_POP(s);
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT, "size_shared: bad tag for %#x\n", obj);
	}
	VERBOSE(DEBUG_NICKIE, ("\n"));
    }

    /* step #2:
       -------------------------------------------------------
       allocate new space !!!
    */

alloc:
    ;

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

    VERBOSE(DEBUG_NICKIE, ("\n"));
    obj = saved_obj;
    BITSTORE_RESET(b);
    for (;;) {
	VERBOSE(DEBUG_NICKIE, ("[copy] revisiting: %x ", obj));
        switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    VERBOSE(DEBUG_NICKIE, ("L"));
	    ptr = list_val_rel(obj, base);
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if it is shared */
	    if (tail == THE_NON_VALUE) {
		/* if it has been processed, skip it */
		if (primary_tag(head) == LIST_SHARED_PROCESSED) {
		    VERBOSE(DEBUG_NICKIE, ("!"));
		    goto cleanup_next;
		}
		/* else, let's process it now */
		else {
		    e = head >> _TAG_PRIMARY_SIZE;
		    VERBOSE(DEBUG_NICKIE, ("$"));
		    CAR(ptr) = (head - primary_tag(head)) + LIST_SHARED_PROCESSED;
		    head = SHTABLE_X(t, e);
		    tail = SHTABLE_Y(t, e);
		    ptr = &(SHTABLE_X(t, e));
		}
	    }
	    /* if not already clean, clean it up */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER) {
		if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		    Eterm saved;
		    BITSTORE_GET(b, saved);
		    VERBOSE(DEBUG_NICKIE, ("/B restoring %d", saved));
		    CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) + saved;
		    CDR(ptr) = tail = (tail - TAG_PRIMARY_HEADER) + TAG_PRIMARY_BOXED;
		} else {
		    VERBOSE(DEBUG_NICKIE, ("/L"));
		    ptr[1] = tail = (tail - TAG_PRIMARY_HEADER) + TAG_PRIMARY_LIST;
		}
	    } else if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		VERBOSE(DEBUG_NICKIE, ("/I"));
		CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) | primary_tag(tail);
		CDR(ptr) = tail = (tail - primary_tag(tail)) | TAG_PRIMARY_IMMED1;
	    } else {
		VERBOSE(DEBUG_NICKIE, ("!"));
		goto cleanup_next;
	    }
	    /* and its children too */
	    if (!IS_CONST(head)) {
		ESTACK_PUSH(s, head);
	    }
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    VERBOSE(DEBUG_NICKIE, ("B"));
	    ptr = boxed_val_rel(obj, base);
	    hdr = *ptr;
	    /* clean it up, unless it's already clean or shared and processed */
	    switch (primary_tag(hdr)) {
	    case TAG_PRIMARY_HEADER:
	    case BOXED_SHARED_PROCESSED:
		VERBOSE(DEBUG_NICKIE, ("!"));
		goto cleanup_next;
	    case BOXED_SHARED_UNPROCESSED:
		e = hdr >> _TAG_PRIMARY_SIZE;
		VERBOSE(DEBUG_NICKIE, ("$"));
		*ptr = (hdr - primary_tag(hdr)) + BOXED_SHARED_PROCESSED;
		hdr = SHTABLE_X(t, e);
		ASSERT(primary_tag(hdr) == BOXED_VISITED);
		SHTABLE_X(t, e) = hdr = (hdr - BOXED_VISITED) + TAG_PRIMARY_HEADER;
		break;
	    case BOXED_VISITED:
		*ptr = hdr = (hdr - BOXED_VISITED) + TAG_PRIMARY_HEADER;
		break;
	    }	    
	    /* and its children too */
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
		if (arity == 0) { /* Empty tuple -- unusual. */
		    goto cleanup_next;
		}
		while (arity-- > 1) {
		    obj = *++ptr;
		    if (!IS_CONST(obj)) {
			ESTACK_PUSH(s, obj);
		    }
		}
		obj = *++ptr;
		break;
	    }
	    case FUN_SUBTAG: {
		ErlFunThing* funp = (ErlFunThing *) ptr;
		unsigned eterms = 1 /* creator */ + funp->num_free;
		unsigned sz = thing_arityval(hdr);
		ptr += 1 /* header */ + sz;
		while (eterms-- > 1) {
		    obj = *ptr++;
		    if (!IS_CONST(obj)) {
			ESTACK_PUSH(s, obj);
		    }
		}
		obj = *ptr;
		break;
	    }
	    default:
		goto cleanup_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	cleanup_next:
	    if (ESTACK_ISEMPTY(s)) {
		goto all_clean;
	    }
	    obj = ESTACK_POP(s);
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT, "size_shared: bad tag for %#x\n", obj);
	}
	VERBOSE(DEBUG_NICKIE, ("\n"));
    }

    /* step #4:
       -------------------------------------------------------
       traverse the table and reverse-transform all stored entries
    */

all_clean:
    VERBOSE(DEBUG_NICKIE, ("\n"));
    for (e = 0; e < SHTABLE_NEXT(t); e += 4*sizeof(Eterm)) {
	ptr = SHTABLE_REV(t, e);
	VERBOSE(DEBUG_NICKIE, ("[copy] restoring shared: %x\n", ptr));
	/* entry was a list */
	if (SHTABLE_Y(t, e) != THE_NON_VALUE) {
	    CAR(ptr) = SHTABLE_X(t, e);
	    CDR(ptr) = SHTABLE_Y(t, e);
	}
	/* entry was boxed */
	else {
	    *ptr = SHTABLE_X(t, e);
	    ASSERT(primary_tag(*ptr) == TAG_PRIMARY_HEADER);
	}
    }
    DESTROY_ESTACK(s);
    DESTROY_BITSTORE(b);
    DESTROY_SHTABLE(t);
    return saved_obj;     /* !!! */
}


#ifdef HYBRID

#ifdef BM_MESSAGE_SIZES
#  define BM_ADD(var,val) (var) += (val);
#else
#  define BM_ADD(var,val)
#endif

#ifdef DEBUG
#  define CLEARMEM(PTR,SIZE) memset(PTR,0,SIZE*sizeof(Eterm))
#else
#  define CLEARMEM(PTR,SIZE)
#endif

#ifdef INCREMENTAL
#define GlobalAlloc(p, need, hp)                                        \
do {                                                                    \
    Uint n = (need);                                                    \
    BM_ADD(words_copied,n);                                             \
    BM_SWAP_TIMER(copy,system);                                         \
    /* If a new collection cycle is started during copy, the message *  \
     * will end up in the old generation and all allocations         *  \
     * thereafter must go directly into the old generation.          */ \
    if (alloc_old) {                                                    \
        erts_incremental_gc((p),n,&dest,1);                             \
        (hp) = erts_inc_alloc(n);                                       \
    } else {                                                            \
        (hp) = IncAlloc((p),n,&dest,1);                                 \
        if (ma_gc_flags & GC_CYCLE_START) {                             \
            alloc_old = 1;                                              \
            global_htop = global_heap;                                  \
            (hp) = erts_inc_alloc(n);                                   \
        }                                                               \
    }                                                                   \
    CLEARMEM((hp),(n));                                                 \
    BM_SWAP_TIMER(system,copy);                                         \
} while(0)

#else /* no INCREMELNTAL */

#define GlobalAlloc(p, need, hp)                                        \
do {                                                                    \
    Uint n = (need);                                                    \
    total_need += n;                                                    \
    if (total_need >= global_heap_sz)                                   \
        erl_exit(ERTS_ABORT_EXIT, "Copying a message (%d words) larger than the nursery simply won't work...\n", total_need); \
    if (global_hend - n < global_htop) {                                \
        BM_SWAP_TIMER(copy,system);                                     \
        erts_global_garbage_collect((p),total_need,NULL,0);             \
        BM_SWAP_TIMER(system,copy);                                     \
        total_need = 0;                                                 \
        ma_src_top = 0;                                                 \
        ma_dst_top = 0;                                                 \
        ma_offset_top = 0;                                              \
        goto copy_start;                                                \
    }                                                                   \
    (hp) = global_htop;                                                 \
    global_htop += n;                                                   \
    BM_ADD(words_copied,n);                                             \
} while(0)
#endif /* INCREMENTAL */

/* Copy a message to the message area. */
Eterm copy_struct_lazy(Process *from, Eterm orig, Uint offs)
{
    Eterm obj;
    Eterm dest;
#ifdef INCREMENTAL
    int alloc_old = 0;
#else
    int total_need = 0;
#endif

    VERBOSE(DEBUG_MESSAGES,
            ("COPY START; %T is sending a message @ 0x%016x\n%T\n",
             from->id, orig, orig));

#ifndef INCREMENTAL
 copy_start:
#endif
    MA_STACK_PUSH(src,orig);
    MA_STACK_PUSH(dst,&dest);
    MA_STACK_PUSH(offset,offs);

    while (ma_src_top > 0) {
        obj = MA_STACK_POP(src);

        /* copy_struct_lazy should never be called with something that
         * do not need to be copied. Within the loop, nothing that do
         * not need copying should be placed in the src-stack.
         */
        ASSERT(!NO_COPY(obj));

        switch (primary_tag(obj)) {
        case TAG_PRIMARY_LIST: {
            Eterm *hp;
            Eterm *objp;

            GlobalAlloc(from,2,hp);
            objp = list_val(obj);

            MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_list(hp));
            MA_STACK_POP(dst);

            /* TODO: Byt ordningen nedan så att CDR pushas först. */

            if (NO_COPY(*objp)) {
                hp[0] = *objp;
#ifdef INCREMENTAL
                if (ptr_within(ptr_val(*objp),inc_fromspc,inc_fromend))
                    INC_STORE(gray,hp,2);
#endif
            } else {
                MA_STACK_PUSH(src,*objp);
                MA_STACK_PUSH(dst,hp);
                MA_STACK_PUSH(offset,0);
            }

            objp++;

            if (NO_COPY(*objp)) {
                hp[1] = *objp;
#ifdef INCREMENTAL
                if (ptr_within(ptr_val(*objp),inc_fromspc,inc_fromend))
                    INC_STORE(gray,hp,2);
#endif
            }
            else {
                MA_STACK_PUSH(src,*objp);
                MA_STACK_PUSH(dst,hp);
                MA_STACK_PUSH(offset,1);
            }
            continue;
        }

        case TAG_PRIMARY_BOXED: {
            Eterm *objp = boxed_val(obj);

            switch (*objp & _TAG_HEADER_MASK) {
            case ARITYVAL_SUBTAG: {
                Uint ari = arityval(*objp);
                Uint i;
                Eterm *hp;
                GlobalAlloc(from,ari + 1,hp);
                /* A GC above might invalidate the value of objp */
                objp = boxed_val(obj);
                MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_tuple(hp));
                MA_STACK_POP(dst);
                *hp = *objp++;
                for (i = 1; i <= ari; i++) {
                    switch (primary_tag(*objp)) {
                    case TAG_PRIMARY_LIST:
                    case TAG_PRIMARY_BOXED:
                        if (NO_COPY(*objp)) {
                            hp[i] = *objp;
#ifdef INCREMENTAL
                            if (ptr_within(ptr_val(*objp),
                                           inc_fromspc,inc_fromend))
                                INC_STORE(gray,hp,BOXED_NEED(hp,*hp));
#endif
                            objp++;
                        } else {
                            MA_STACK_PUSH(src,*objp++);
                            MA_STACK_PUSH(dst,hp);
                            MA_STACK_PUSH(offset,i);
                        }
                        break;
                    default:
                        hp[i] = *objp++;
                    }
                }
                continue;
            }

            case REFC_BINARY_SUBTAG: {
                ProcBin *pb;
                Uint i = thing_arityval(*objp) + 1;
                Eterm *hp;
                GlobalAlloc(from,i,hp);
                /* A GC above might invalidate the value of objp */
                objp = boxed_val(obj);
                MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_binary(hp));
                MA_STACK_POP(dst);
                pb = (ProcBin*) hp;
                while (i--) {
                    *hp++ = *objp++;
                }
                erts_refc_inc(&pb->val->refc, 2);
                pb->next = erts_global_offheap.first;
                erts_global_offheap.first = pb;
		OH_OVERHEAD(off_heap, pb->size / sizeof(Eterm));
                continue;
            }

            case FUN_SUBTAG: {
                ErlFunThing *funp = (ErlFunThing*) objp;
                Uint i = thing_arityval(*objp) + 1;
                Uint j = i + 1 + funp->num_free;
                Uint k = i;
                Eterm *hp, *hp_start;
                GlobalAlloc(from,j,hp);
                /* A GC above might invalidate the value of objp */
                objp = boxed_val(obj);
                hp_start = hp;
                MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_fun(hp));
                MA_STACK_POP(dst);
                funp = (ErlFunThing*) hp;
                while (i--) {
                    *hp++ = *objp++;
                }
#ifndef HYBRID /* FIND ME! */
                funp->next = erts_global_offheap.first;
                erts_global_offheap.first = funp;
                erts_refc_inc(&funp->fe->refc, 2);
#endif
                for (i = k; i < j; i++) {
                    switch (primary_tag(*objp)) {
                    case TAG_PRIMARY_LIST:
                    case TAG_PRIMARY_BOXED:
                        if (NO_COPY(*objp)) {
#ifdef INCREMENTAL
                            if (ptr_within(ptr_val(*objp),
                                           inc_fromspc,inc_fromend))
                                INC_STORE(gray,hp,BOXED_NEED(hp,*hp));
#endif
                            *hp++ = *objp++;
                        } else {
                            MA_STACK_PUSH(src,*objp++);
                            MA_STACK_PUSH(dst,hp_start);
                            MA_STACK_PUSH(offset,i);
                            hp++;
                        }
                        break;
                    default:
                        *hp++ = *objp++;
                    }
                }
                continue;
            }

            case EXTERNAL_PID_SUBTAG:
            case EXTERNAL_PORT_SUBTAG:
            case EXTERNAL_REF_SUBTAG: {
                ExternalThing *etp;
                Uint i =  thing_arityval(*objp) + 1;
                Eterm *hp;
                GlobalAlloc(from,i,hp);
                /* A GC above might invalidate the value of objp */
                objp = boxed_val(obj);
                MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_external(hp));
                MA_STACK_POP(dst);
                etp = (ExternalThing*) hp;
                while (i--)  {
                    *hp++ = *objp++;
                }

                etp->next = erts_global_offheap.first;
                erts_global_offheap.first = etp;
		erts_refc_inc(&etp->node->refc, 2);
                continue;
            }

            case SUB_BINARY_SUBTAG: {
                ErlSubBin *sb = (ErlSubBin *) objp;
		Eterm *hp;
		Eterm res_binary;
                Eterm real_bin = sb->orig;
                Uint bit_offset = sb->bitoffs;
		Uint bit_size = sb -> bitsize;
		Uint sub_offset = sb->offs;
                size_t size = sb->size;
		Uint extra_bytes;
		Uint real_size;
		Uint sub_binary_heapneed;
		if ((bit_size + bit_offset) > 8) {
		    extra_bytes = 2;
		    sub_binary_heapneed = ERL_SUB_BIN_SIZE;
		} else if ((bit_size + bit_offset) > 0) {
		    extra_bytes = 1;
		    sub_binary_heapneed = ERL_SUB_BIN_SIZE;
		} else {
		    extra_bytes = 0;
		    sub_binary_heapneed = 0;
		}

		real_size = size+extra_bytes;
                objp = binary_val(real_bin);
                if (thing_subtag(*objp) == HEAP_BINARY_SUBTAG) {
                    ErlHeapBin *from_bin;
                    ErlHeapBin *to_bin;
                    Uint i = heap_bin_size(real_size);
                    GlobalAlloc(from,i+sub_binary_heapneed,hp);
                    from_bin = (ErlHeapBin *) objp;
                    to_bin = (ErlHeapBin *) hp;
                    to_bin->thing_word = header_heap_bin(real_size);
                    to_bin->size = real_size;
                    sys_memcpy(to_bin->data, ((byte *)from_bin->data) +
                               sub_offset, real_size);
		    res_binary = make_binary(to_bin);
		    hp += i;
                } else {
                    ProcBin *from_bin;
                    ProcBin *to_bin;

                    ASSERT(thing_subtag(*objp) == REFC_BINARY_SUBTAG);
		    from_bin = (ProcBin *) objp;
		    erts_refc_inc(&from_bin->val->refc, 2);
                    GlobalAlloc(from,PROC_BIN_SIZE+sub_binary_heapneed,hp);
                    to_bin = (ProcBin *) hp;
                    to_bin->thing_word = HEADER_PROC_BIN;
                    to_bin->size = real_size;
                    to_bin->val = from_bin->val;
                    to_bin->bytes = from_bin->bytes + sub_offset;
                    to_bin->next = erts_global_offheap.first;
                    erts_global_offheap.first = to_bin;
		    OH_OVERHEAD(&erts_global_offheap, to_bin->size / sizeof(Eterm));
		    res_binary=make_binary(to_bin);
		    hp += PROC_BIN_SIZE;
                }
		if (extra_bytes != 0) {
		    ErlSubBin* res;
		    res = (ErlSubBin *) hp;
		    res->thing_word = HEADER_SUB_BIN;
		    res->size = size;
		    res->bitsize = bit_size;
		    res->bitoffs = bit_offset;
		    res->offs = 0;
		    res->is_writable = 0;
		    res->orig = res_binary;
		    res_binary = make_binary(hp);
		}
		MA_STACK_UPDATE(dst,MA_STACK_POP(offset),res_binary);
		MA_STACK_POP(dst);
                continue;
            }

	    case BIN_MATCHSTATE_SUBTAG:
		erl_exit(ERTS_ABORT_EXIT,
			 "copy_struct_lazy: matchstate term not allowed");

            default: {
                Uint size = thing_arityval(*objp) + 1;
                Eterm *hp;
                GlobalAlloc(from,size,hp);
                /* A GC above might invalidate the value of objp */
                objp = boxed_val(obj);
                MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_boxed(hp));
                MA_STACK_POP(dst);
                while (size--) {
                    *hp++ = *objp++;
                }
                continue;
            }
            }
            continue;
        }

        case TAG_PRIMARY_HEADER:
        ASSERT((obj & _TAG_HEADER_MASK) == ARITYVAL_SUBTAG);
        {
            Eterm *objp = &obj;
            Uint ari = arityval(obj);
            Uint i;
            Eterm *hp;
            GlobalAlloc(from,ari + 1,hp);
            MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_tuple(hp));
            MA_STACK_POP(dst);
            *hp = *objp++;
            for (i = 1; i <= ari; i++) {
                switch (primary_tag(*objp)) {
                case TAG_PRIMARY_LIST:
                case TAG_PRIMARY_BOXED:
                    if (NO_COPY(*objp)) {
#ifdef INCREMENTAL
                        if (ptr_within(ptr_val(*objp),inc_fromspc,inc_fromend))
                            INC_STORE(gray,hp,ari + 1);
#endif
                        hp[i] = *objp++;
                    } else {
                        MA_STACK_PUSH(src,*objp++);
                        MA_STACK_PUSH(dst,hp);
                        MA_STACK_PUSH(offset,i);
                    }
                    break;
                default:
                    hp[i] = *objp++;
                }
            }
            continue;
        }

        default:
            erl_exit(ERTS_ABORT_EXIT,
		     "%s, line %d: Internal error in copy_struct_lazy: 0x%08x\n",
                     __FILE__, __LINE__,obj);
        }
    }

    VERBOSE(DEBUG_MESSAGES,
            ("Copy allocated @ 0x%08lx:\n%T\n",
             (unsigned long)ptr_val(dest),dest));

    ma_gc_flags &= ~GC_CYCLE_START;

    ASSERT(eq(orig, dest));
    ASSERT(ma_src_top == 0);
    ASSERT(ma_dst_top == 0);
    ASSERT(ma_offset_top == 0);
    return dest;
}

#undef NO_COPY
#endif /* HYBRID */

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

/*
Local Variables:
  c-basic-offset: 4
  c-indent-level: 4
  indent-tabs-mode: t
End:
*/
