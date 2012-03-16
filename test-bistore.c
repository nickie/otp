#include <memory.h>
#include <stdlib.h>
#include <stdio.h>

typedef unsigned char byte;
typedef unsigned int UWord, Uint;

#define WSTK_CONCAT(a,b) a##b
#define WSTK_SUBSCRIPT(s,i) *((UWord *)((byte *)WSTK_CONCAT(s,_start) + (i)))
#define DEF_WSTACK_SIZE (16)

/*
 * Helper function for the ESTACK macros defined in global.h.
 */
void
erl_grow_wstack(UWord** start, UWord** sp, UWord** end)
{
  Uint old_size = (*end - *start);
  Uint new_size = old_size * 2;
  Uint sp_offs = *sp - *start;
  if (new_size > 2 * DEF_WSTACK_SIZE) {
    *start = realloc((void *) *start, new_size*sizeof(UWord));
  } else {
    UWord* new_ptr = malloc(new_size*sizeof(UWord));
    memcpy(new_ptr, *start, old_size*sizeof(UWord));
    *start = new_ptr;
  }
  *end = *start + new_size;
  *sp = *start + sp_offs;
}

#define DECLARE_WSTACK(s)						\
    UWord WSTK_CONCAT(s,_default_stack)[DEF_WSTACK_SIZE];		\
    UWord* WSTK_CONCAT(s,_start) = WSTK_CONCAT(s,_default_stack);	\
    UWord* WSTK_CONCAT(s,_sp) = WSTK_CONCAT(s,_start);			\
    UWord* WSTK_CONCAT(s,_end) = WSTK_CONCAT(s,_start) + DEF_WSTACK_SIZE

#define DESTROY_WSTACK(s)						\
do {									\
    if (WSTK_CONCAT(s,_start) != WSTK_CONCAT(s,_default_stack)) {	\
	erts_free(ERTS_ALC_T_ESTACK, WSTK_CONCAT(s,_start));		\
    }									\
} while(0)

#define WSTACK_PUSH(s, x)						\
do {									\
    if (WSTK_CONCAT(s,_sp) == WSTK_CONCAT(s,_end)) {			\
	erl_grow_wstack(&WSTK_CONCAT(s,_start), &WSTK_CONCAT(s,_sp),	\
	               &WSTK_CONCAT(s,_end));				\
    }									\
    *WSTK_CONCAT(s,_sp)++ = (x);					\
} while(0)

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

#define PRALL                                                           \
do {                                                                    \
  UWord *p = b_start;                                                   \
  fprintf(stderr, "stack = ");                                          \
  if (p == b_sp)                                                        \
    fprintf(stderr, "empty, ");                                         \
  else                                                                  \
    while (p < b_sp)                                                    \
      fprintf(stderr, "%x, ", *p++);                                    \
  fprintf(stderr, "buffer = %x (%d bits)\n", b_buffer, b_bitoffs);      \
} while(0)

#define MAX 100

int main ()
{
  unsigned int x, a[MAX], i;
  DECLARE_BITSTORE(b);
  PRALL;
  for (i=0; i<MAX; i++) {
    a[i] = random() % 4;
    fprintf(stderr, "put %d, ", a[i]);
    BITSTORE_PUT(b, a[i]);
    PRALL;
  }
  fprintf(stderr, "reset, ");
  BITSTORE_RESET(b);
  PRALL;
  for (i=0; i<MAX; i++) {
    BITSTORE_GET(b, x);
    if (x == a[i])
      fprintf(stderr, "get %d, ", a[i]);
    else {
      fprintf(stderr, "ERROR at i=%d, expecting %d, got %d\n", i, a[i], x);
      return 1;
    }
    PRALL;
  }
  // DESTROY_BISTORE(b);
  return 0;
}
