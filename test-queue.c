#include <memory.h>
#include <stdlib.h>
#include <stdio.h>

#define DEBUGPR 0
#define NDEBUG
#include <assert.h>
#define ASSERT(x) assert(x)

typedef unsigned char byte;
typedef unsigned int Eterm, Uint;

#define EQUE_CONCAT(a,b) a##b
#define DEF_EQUEUE_SIZE (16)

/*
 * Helper function for the EQUEUE macros defined in global.h.
 */
void
erl_grow_queue(Eterm** start, Eterm** front, Eterm** back, Eterm** end)
{
    Uint old_size = (*end - *start);
    Uint new_size = old_size * 2;
    Uint first_part = (*end - *front);
    Uint second_part = (*back - *start);
#if 0    
    Eterm* new_ptr = erts_alloc(ERTS_ALC_T_ESTACK, new_size*sizeof(Eterm));
#else
    Eterm* new_ptr = malloc(new_size*sizeof(Eterm));
#endif
    ASSERT(*back == *front);   // of course the queue is full now!
    if (first_part > 0)
#if 0    
      sys_memcpy(new_ptr, *front, first_part*sizeof(Eterm));
#else
      memcpy(new_ptr, *front, first_part*sizeof(Eterm));
#endif
    if (second_part > 0)
#if 0    
      sys_memcpy(new_ptr+first_part, *start, second_part*sizeof(Eterm));
#else
      memcpy(new_ptr+first_part, *start, second_part*sizeof(Eterm));
#endif
    if (old_size != DEF_EQUEUE_SIZE)
#if 0    
      erts_free(ERTS_ALC_T_ESTACK, *start);
#else
      free(*start);
#endif
    *start = new_ptr;
    *end = *start + new_size;
    *front = *start;
    *back = *start + old_size;
}

#define DECLARE_EQUEUE(s)						\
    Eterm  EQUE_CONCAT(s,_default_queue)[DEF_EQUEUE_SIZE];		\
    Eterm* EQUE_CONCAT(s,_start) = EQUE_CONCAT(s,_default_queue);	\
    Eterm* EQUE_CONCAT(s,_front) = EQUE_CONCAT(s,_start);		\
    Eterm* EQUE_CONCAT(s,_back) = EQUE_CONCAT(s,_start);		\
    int    EQUE_CONCAT(s,_possibly_empty) = 1;				\
    Eterm* EQUE_CONCAT(s,_end) = EQUE_CONCAT(s,_start) + DEF_EQUEUE_SIZE

#define DESTROY_EQUEUE(s)						\
do {									\
    if (EQUE_CONCAT(s,_start) != EQUE_CONCAT(s,_default_queue)) {	\
	erts_free(ERTS_ALC_T_STACK, EQUE_CONCAT(s,_start));		\
    }									\
} while(0)

#define EQUEUE_PUT(s, x)						\
do {									\
    if (EQUE_CONCAT(s,_back) == EQUE_CONCAT(s,_front) &&                \
        !EQUE_CONCAT(s,_possibly_empty)) {                              \
	erl_grow_queue(&EQUE_CONCAT(s,_start), &EQUE_CONCAT(s,_front),	\
	               &EQUE_CONCAT(s,_back), &EQUE_CONCAT(s,_end));    \
    }									\
    EQUE_CONCAT(s,_possibly_empty) = 0;					\
    *EQUE_CONCAT(s,_back) = (x);					\
    if (++EQUE_CONCAT(s,_back) == EQUE_CONCAT(s,_end)) {		\
        EQUE_CONCAT(s,_back) = EQUE_CONCAT(s,_start);			\
    }                                                                   \
} while(0)

#define EQUEUE_ISEMPTY(s)                               		\
  (EQUE_CONCAT(s,_back) == EQUE_CONCAT(s,_front) &&                     \
    EQUE_CONCAT(s,_possibly_empty))

#define EQUEUE_GET(s, x)                                           	\
do {                                                                  	\
    EQUE_CONCAT(s,_possibly_empty) = 1;                                 \
    (x) = *EQUE_CONCAT(s,_front);                                       \
    if (++EQUE_CONCAT(s,_front) == EQUE_CONCAT(s,_end)) {		\
        EQUE_CONCAT(s,_front) = EQUE_CONCAT(s,_start);			\
    }                                                                   \
} while(0)

#if DEBUGPR == 1
#define debug(...) fprintf(stderr, __VA_ARGS__)
#define PRALL                                                           \
do {                                                                    \
  Eterm *p = b_front;                                                   \
  fprintf(stderr, "queue = ");                                          \
  if (p == b_back && b_possibly_empty)                                  \
    fprintf(stderr, "empty");                                           \
  else                                                                  \
    for (;;) {                                                          \
      fprintf(stderr, "%x", *p);                                        \
      if (++p == b_end) p = b_start;                                    \
      if (p == b_back) break;                                           \
      fprintf(stderr, ", ");                                            \
    }                                                                   \
  fprintf(stderr, "\n");                                                \
} while(0)
#elif DEBUGPR == 2
#define debug(...) fprintf(stderr, __VA_ARGS__)
#define PRALL fprintf(stderr, "\n")
#else
#define PRALL do {} while(0)
#define debug(...) do ; while(0)
#endif

#define MAX 100000000
#define NEXT(seed) ((seed) = ((seed) * 137 + 220), seed)

int main ()
{
  Uint seed1 = 0x12345678;
  Uint seed2 = seed1;
  Uint num = 0, i, x;

  DECLARE_EQUEUE(b);
  PRALL;
  for (i=0; i<MAX; i++) {
    if (num >= (1 << 24) || (num > 0 && random() % 100 < 30)) {
      num--;
      EQUEUE_GET(b, x);
      if (x == NEXT(seed2))
        debug("get %x, ", x);
      else {
        fprintf(stderr, "ERROR at i=%d, expecting %x, got %x\n", i, seed2, x);
        return 1;
      }
      PRALL;
    }
    else {
      num++;
      EQUEUE_PUT(b, NEXT(seed1));
      debug("put %x, ", seed1);
      PRALL;
    }
  }
  // DESTROY_EQUEUE(b);
  return 0;
}
