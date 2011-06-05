/* dyadic verbs */

#include "vtab.h"

/* These are just notes so far, for brainstorming about optimizations. Not
 * actually compiled, yet. Eventually, this may drive verb dispatch.
 *
 * Also, note that projection over lists is not explicitly noted here,
 * e.g. for 1 + (1 2 3; 4 5 6; 7 8 9) there aren't explict (TI, TL), (TF, TL),
 * (TL, TI), ... cases. That may be simpler to handle in the dispatcher.
 * Either way, anything marked SCALAR should project. (right?)
 */


/* Tags:
 * ATOM: atom / list distinction is significant
 * BOOL: special case for boolean vectors
 */

/* poss. type signatures for dyads:
 *
 * basic:
 * K *foo(K *a, K *b)
 *
 * pass in return pointer, when b can be modified in place:
 * K *foo(K *a, K *b, K *z)
 *
 * pass in value vectors & counts, for scalars that don't need to access
 * other parts of the K (so that one thread could handle the first (ct)
 * values, another would get an offset pointer and handle the next (ct),
 * etc.):
 * K *foo(V *a, I na, V *b, I nb, V *z)
 * 
 * Variant for 1:N and N:1 cases (e.g. 1+1 2 3):
 * K *foo(V *a, I na, V b, V *z)
 * K *foo(V a, V *b, I nb, V *z)
 *
 * Others? Not all dyadic funs should need the same type signature.
 */


/**********
 * Macros *
 **********/

#define d_table(name) static const struct dispatch_dyad name []

/* sentinel. If reached during dispatch, throw type error. */
#define EOV {0, 0, 0, 0, 0}

#define SCALAR(fname, flags) \
        { TI, TI, fname ## II, TI, flags },     \
        { TI, TF, fname ## IF, TF, flags },     \
        { TF, TI, fname ## FI, TF, flags },     \
        { TF, TF, fname ## FF, TF, flags }

/*********
 * Verbs *
 *********/

d_table(d_plus) = {
        SCALAR(plus, VF_SAME_SIZE),
        EOV
};

d_table(d_minus) = {
        SCALAR(minus, VF_SAME_SIZE),
        EOV
};


d_table(d_asterisk) = {
        SCALAR(times, VF_SAME_SIZE),
        EOV
};


d_table(d_percent) = {
        SCALAR(div, VF_SAME_SIZE),
        EOV
};


d_table(d_pipe) = {
        SCALAR(max_or, VF_SAME_SIZE),
        EOV
};


d_table(d_ampersand) = {
        SCALAR(min_and, VF_SAME_SIZE),
        EOV
};


d_table(d_caret) = {
        SCALAR(power, VF_SAME_SIZE),
        EOV
};

/* 2!1 2 3 4 5 is rotate, 1 2 3 4 5!2 is mod. One of few cases where the
 * ATOM / list distinction actually impacts results. */
d_table(d_excl) = {             /* ATOM */
        { TIA, TAny, rotate, TAny, VF_SAME_SIZE | VF_REARRANGE},
        { TI, TIA, modII, TI, VF_SAME_SIZE /*as left*/},
        { TF, TIA, modFI, TI, VF_SAME_SIZE /*as left*/},
        { TI, TF, mod, TI, VF_SAME_SIZE},
        EOV
};

d_table(d_lt) = { /* Could also return BOOL */
        { TI, TI, ltII, TI, VF_SAME_SIZE},
        { TI, TF, ltIF, TI, VF_SAME_SIZE},
        { TF, TI, ltFI, TI, VF_SAME_SIZE},
        { TF, TF, ltFF, TI, VF_SAME_SIZE},
        { TC, TC, ltCC, TI, VF_SAME_SIZE},
        { TS, TS, ltSS, TI, VF_SAME_SIZE},
        EOV
};

d_table(d_gt) = {
        { TI, TI, gtII, TI, VF_SAME_SIZE},
        { TI, TF, gtIF, TI, VF_SAME_SIZE},
        { TF, TI, gtFI, TI, VF_SAME_SIZE},
        { TF, TF, gtFF, TI, VF_SAME_SIZE},
        { TC, TC, gtCC, TI, VF_SAME_SIZE},
        { TS, TS, gtSS, TI, VF_SAME_SIZE},
        EOV
};

d_table(d_eq) = {
        { TI, TI, eqII, TI, VF_SAME_SIZE},
        { TI, TF, eqIF, TI, VF_SAME_SIZE},
        { TF, TI, eqFI, TI, VF_SAME_SIZE},
        { TF, TF, eqFF, TI, VF_SAME_SIZE},
        { TC, TC, eqCC, TI, VF_SAME_SIZE},
        { TS, TS, eqSS, TI, VF_SAME_SIZE},
        EOV
};

/* Unlike eq, returns 0 or 1 for whole structure, not atoms. */
d_table(d_tilde) = {
        { TL, TL, matchLL, TI, VF_REDUCER},
        { TI, TI, matchII, TI, VF_REDUCER},
        { TI, TF, matchIF, TI, VF_REDUCER},
        { TF, TI, matchFI, TI, VF_REDUCER},
        { TF, TF, matchFF, TI, VF_REDUCER},
        { TC, TC, matchCC, TI, VF_REDUCER},
        { TS, TS, matchSS, TI, VF_REDUCER},
        { TD, TD, matchDD, TI, VF_REDUCER}, /* DICT */
        /* all other comparisons fail, just return 0 */
        { TAny, TAny, return0, TI, VF_REDUCER},
        EOV
};

d_table(d_at) = {
        { TFun, TAny, dot, TAny, VF_NONE}, /* enlist right arg & apply function */
        { TS, TAny, nyi, TAny, NF_NYI},
        { TAny, TI, at_verbI, TAny, NF_NONE},      /* vector lookup */
        { TD, TS, at_verbDS, TAny, NF_NONE},       /* DICT lookup */
        { TAny, TN, identity, TAny, NF_SAME_SIZE}, /* a[] or a@_n -> a */
        EOV
};

d_table(d_question) = {
        { TFun, TAny, what_triadic, TAny, VF_NONE}, /* find function inverse, secant method */
        /* 10?30 = 10 random ints 0<=n<30; -10?30 is w/out replacement (all distinct) */
        { TI, TI, qrandI, TI, VF_EXPANDER},
        /* same as above, but random floats; 2?1.0 -> 0.2953862 0.2792765 */
        { TI, TF, qrandF, TF, VF_EXPANDER},
        EOV
};

d_table(d_underscore) = {
        { TIA, TAny, drop, TAny, VF_REDUCER}, /* 3_!5 -> 3 4; -3_!5 -> 0 1 */
        { TI, TAny, cut, TAny, VF_REARRANGE}, /* 3 6_!10 -> (3 4 5; 6 7 8 9) */
        EOV
};

d_table(d_comma) = {            /* missing any? */
        { TL, TL, joinLL, TL, VF_EXPANDER},
        { TI, TI, joinII, TI, VF_EXPANDER},
        { TF, TF, joinFF, TF, VF_EXPANDER},
        { TC, TC, joinCC, TC, VF_EXPANDER},
        { TS, TS, joinSS, TS, VF_EXPANDER},
        EOV
};

d_table(d_pound) = {
        /* reduces when a < b->n, "overtaking" loops b to fill */
        { TIA, TAny, take, TAny, VF_NONE},
        { TI, TAny, reshape, TAny, VF_NONE}, /* num dimensions = #a */
        EOV
};

d_table(d_dollar) = {                          /* "format", convert to char */
        { TI, TAny, formatI, TAny /*TC or TL*/, VF_NONE},
        { TF, TI, formatFI, TAny /*TC or TL*/, VF_NONE},
        { TF, TF, formatFF, TAny /*TC or TL*/, VF_NONE},
        { TC, TC, identity, TC, VF_SAME_SIZE},      /* a is ignored, why? */
        { TSA, TC, char_to_symbol, TS, VF_REDUCER}, /* `$"foo" -> `foo */
        EOV
};

d_table(d_dot) = {              /* TODO */
        EOV
};

d_table(d_colon) = {            /* TODO */
        EOV
};
