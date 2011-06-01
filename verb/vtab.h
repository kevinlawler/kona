#ifndef VTAB_H
#define VTAB_H

/* verb tables */

/* Verb types. Where atoms and lists are handled distinctly, use TIA etc. */
typedef enum { TL, TI, TF, TC, TS, TD, TN, TFun, TAny,
               TIA = -1, TFA = -2, TCA = -3, TSA = -4,
} TV;

typedef enum verb_flags {
        VF_NONE,
        VF_SIZE,                /* return size known before running */
        VF_SAME_SIZE,           /* same return size as argument(s), implies VF_SIZE */
        VF_EXPANDER,            /* tends to expand input */
        VF_REDUCER,             /* tends to reduce input */
        VF_REARRANGE,           /* rearranges input (same size) */
        VF_MUTATES,             /* modifies input in place */
        /* other attributes? */
} verb_flags;

struct dispatch_nilad {
        K (*f)(K z),
        TV rt,
        verb_flags flags
};


/* Type signature for monadic verbs could be f(K a), f(K a, K z),
 * f(void *a, I n), f(void *a, I n, K z), etc.
 * Are there any monadic cases where the argument could be mutated
 * in-place (w/ refcount of 1)? Most of those are dyadic.
 * Passing in a void * and count would allow different ranges of
 * the argument to be handled in parallel.
 * While void *s would be used, each verb function is already
 * made type-specific at compile-time, i.e., floorF or firstI.
 */
struct dispatch_monad {
        int at,
        K (*f)(K a, K z),
        TV rt,
        verb_flags flags
};


/* Same issues as dispatch_monad, except most scalars would clearly
 * benefit from type f(void *a, I an, void *b, I bn, K z); if b has
 * the expected return type and length and b's refcount is 1, z's
 * vector should point at b (mutate in-place), otherwise alloc a
 * new K for the result.
 */
struct dispatch_dyad {
        int at,
        int bt,
        K (*f)(K a, K b, K z),
        TV rt,
        verb_flags flags
};

/* Are there any cases for triad or tetrad where the dyadic
 * optimizations apply?
 */
struct dispatch_triad {
        int at,
        int bt,
        int ct,
        K (*f)(K a, K b, K c, K z),
        TV rt,
        verb_flags flags
};

struct dispatch_tetrad {
        int at,
        int bt,
        int ct,
        int dt,
        K (*f)(K a, K b, K c, K d, K z),
        TV rt,
        verb_flags flags
};

#define VF_SCALAR (VF_SAME_SIZE)

#endif
