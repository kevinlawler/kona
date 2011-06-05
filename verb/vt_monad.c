/* monadic verbs */

#include "vtab.h"

/* These are just notes so far, for brainstorming about optimizations. Not
 * actually compiled, yet. Eventually, this may drive verb dispatch. */


/* Tags:
 * ATOM: atom / list distinction is significant
 * BOOL: special case for boolean vectors
 */

/**********
 * Macros *
 **********/

/* These are really repetitive, but may still be worth spelling them out
 * explicitly if we generate docs from this.
 */

/* macro for all 'a -> 'a monadic verbs */
#define eachL1(fname, flags)   \
{ TL, fname ## L, TL, flags }, \
{ TI, fname ## I, TI, flags }, \
{ TF, fname ## F, TF, flags }, \
{ TC, fname ## C, TC, flags }, \
{ TS, fname ## S, TS, flags }

/* macro for all 'a -> 'a monadic verbs, same fun */
#define eachL1Same(fname, flags)   \
{ TL, fname, TL, flags }, \
{ TI, fname, TI, flags }, \
{ TF, fname, TF, flags }, \
{ TC, fname, TC, flags }, \
{ TS, fname, TS, flags }

/* macro for all 'a -> TI monadic verbs */
#define eachL1I(fname, flags)  \
{ TL, fname ## L, TI, flags }, \
{ TI, fname ## I, TI, flags }, \
{ TF, fname ## F, TI, flags }, \
{ TC, fname ## C, TI, flags }, \
{ TS, fname ## S, TI, flags }


#define m_table(name) static const struct dispatch_monad name []

/* sentinel. If reached during dispatch, throw type error. */
#define EOV {0, 0, 0, 0}

/*********
 * Verbs *
 *********/

m_table(m_plus) = {
        { TL, flip, TL, VF_TYPE },      /* NB: shadows TL */
        eachL1(identify, VF_TYPE),
        EOV
};

m_table(m_minus) = {
        { TI, negI, TI, 0 },
        { TF, negF, TF, 0 },
        EOV
};


m_table(m_asterisk) = {
        eachL1(first, VF_SIZE | VF_REDUCER),
        EOV
};


m_table(m_percent) = {
        { TI, recipI, TI, VF_SAME_SIZE },
        { TF, recipF, TF, VF_SAME_SIZE },
        EOV
};


m_table(m_pipe) = {
        eachL1(reverse, VF_SIZE | VF_REARRANGE),
        EOV
};


/* & can either reduce or grow, though is often used to reduce
 * implicitly boolean vectors. A distinct BOOL type would make this clearer,
 * but converting to/from bools elsewhere could also be messy. */
m_table(m_ampersand) = {
        { TI, where, TI, VF_REDUCER }, /* doesn't always reduce... */
        EOV
};


m_table(m_caret) = {
        { TL, shape, TI, 0 },           /* all share same implementation */
        { TI, shape, TI, 0 },
        { TF, shape, TI, 0 },
        { TC, shape, TI, 0 },
        { TS, shape, TI, 0 },
        { TD, shape, TI, 0 }, /* DICT */
        { TN, shape, TI, 0 },
        { TF, shape, TI, 0 },
        EOV
};

/* ATOM, length ~= 1 for arg is currently unused */
m_table(m_excl) = {
        { TIA, enumerate, TI, 0 },
        { TFA, enumerate, TI, 0 },
        { TCA, ls_directory, TL, 0 },    /* currently called "enumerate_charvec" */
/*  NYI { TS, enumerate dictionary of sym on k-tree, TL, 0 }, */
        { TD, keys, TS, 0 }, /* DICT */
        { TN, enumerateN, TD, 0 },
// { TF, funcname, Tr, 0 },
        EOV
};

m_table(m_lt) = {
        eachL1I(gradeup, VF_SAME_SIZE),
        EOV
};

m_table(m_gt) = {
        eachL1I(gradedown, VF_SAME_SIZE),
        EOV
};

m_table(m_eq) = {
        eachL1I(group, 0),
        EOV
};

m_table(m_tilde) = {
        { TL, not_attr_project, TL, 0 },
        { TI, notI, TI, VF_SAME_SIZE }, /* 0 0 1 0 -> 1 1 0 1, BOOL */
        { TF, notF, TI, 0 },            /* 0.0 0.1 3.4 0.0 5.0 -> 1 0 0 1 0, BOOL */
        { TS, notsp, TS, 0 },           /* this adds . to a symbol. use case? */
        EOV
};

m_table(m_at) = {
        eachL1Same(atom, VF_REDUCER, VF_SIZE),
        { TD, atom, TI, VF_REDUCER, VF_SIZE }, /* DICT */
        { TN, atom, TI, VF_REDUCER, VF_SIZE },
        { TF, atom, TI, VF_REDUCER, VF_SIZE },
        EOV
};

m_table(m_question) = {
        eachL1(range, VF_REDUCER),
        EOV
};

m_table(m_underscore) = {
        { TL, floorL, TL, 0 },
        { TI, identity, TI, 0 },
        { TF, floorF, TF, 0 },
        EOV
};

m_table(m_comma) = { /* FIXME atom vs list */
        eachL1(enlist, 0),
        EOV
};

m_table(m_pound) = {
        eachL1Same(count, VF_REDUCER | VF_SIZE),
        EOV
};

m_table(m_dollar) = {                  /* ATOM/list affects return type, C or L-of-C */
        { TIA, formatI, TC, 0 },
        { TFA, formatF, TC, 0 },
        { TCA, identity, TC, 0 },
        { TSA, formatS, TC, 0 },
        { TL, formatL, TL, 0 },
        { TI, formatI, TL, 0 },
        { TF, formatF, TL, 0 },
        { TC, identity, TL, 0 },
        { TS, formatS, TL, 0 },
        { TD, funcname, Tr, 0 }, /* FIXME "Beats me -- this has a similar signature to a _hash" DICT */
        EOV
};

m_table(m_dot) = { /* are these correct? */
        { TL, make_dict, TD, 0 }, /* DICT */
        { TC, eval, TAny, 0 },
        { TS, dump_tree, TD, 0 }, /* DICT */
        { TD, unmake_dict, TL, 0 }, /* DICT */
        EOV
};

m_table(m_colon) = { /* FIXME. Just calls ci(a). Use? */
// { TL, funcname, Tr, 0 },
// { TI, funcname, Tr, 0 },
// { TF, funcname, Tr, 0 },
// { TC, funcname, Tr, 0 },
// { TS, funcname, Tr, 0 },
// { TD, funcname, Tr, 0 },
// { TN, funcname, Tr, 0 },
// { TF, funcname, Tr, 0 },
        EOV
};
