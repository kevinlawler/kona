#ifndef SCALAR_H
#define SCALAR_H

/* Init vars for scalar dyad */
#define SCALAR_INIT(maxt)               \
  I at=a->t, an=a->n, bt=b->t, bn=b->n; \
  I type = MAX(ABS(at),ABS(bt));        \
  P(at <= 0 && bt <= 0 && an != bn, LE) \
  P(type > maxt, TE ) /* > allowed types? */              \
  I zt=type;          /* Starting at worst known type */  \
  if(MIN(at,bt) < 1) zt=-zt; /* Plural? */                \
  if(!at || !bt) zt=0;       /* Generic list trumps */    \
  I zn=at>0?bn:an;

/* Macro case: N:N, 1:N, N:1 implicit looping for op */
#define SCALAR_OP_CASE(op, cres, ca, cb)                      \
   if (an==bn) { DO(zn,cres [i]= op (ca [i], cb [i])) }       \
   else if (an==1) { DO(zn,cres [i]= op (ca [0], cb [i])) }   \
   else /* bn==1 */ { DO(zn,cres [i]= op (ca [i], cb [0])) }

/* Scalar operator macro, with proper float/int/array treatment */
#define SCALAR_OP(op,verb)                                                      \
   if (2==ABS(at) && 2==ABS(bt)) { SCALAR_OP_CASE(op,kF(z),kF(a),kF(b)) }       \
   else if (2==ABS(at) && 1==ABS(bt)) { SCALAR_OP_CASE(op,kF(z),kF(a),kI(b)) }  \
   else if (1==ABS(at) && 2==ABS(bt)) { SCALAR_OP_CASE(op,kF(z),kI(a),kF(b)) }  \
   else if (1==ABS(at) && 1==ABS(bt)) { SCALAR_OP_CASE(op,kI(z),kI(a),kI(b)) }  \
   else if (0==at || 0==bt) { dp(&z,verb,a,b); }

/* Macro case: N:N, 1:N, N:1 implicit looping for expression */
#define SCALAR_EXPR_CASE(expr, cres, ca, cb, vx, vy)        \
   if (an==bn) { DO(zn, vx=ca [i];vy=cb [i]; expr); }       \
   else if (an==1) { vx=ca [0]; DO(zn, vy=cb [i]; expr); }  \
   else /* bn==1 */ { vy=cb [0]; DO(zn, vx=ca [i]; expr); }

/* Scalar expression macro, with proper float/int/array treatment */
#define SCALAR_EXPR(expr,verb,vx,vy)                                                      \
   if (2==ABS(at) && 2==ABS(bt)) { SCALAR_EXPR_CASE(expr,kF(z),kF(a),kF(b),vx,vy) }       \
   else if (2==ABS(at) && 1==ABS(bt)) { SCALAR_EXPR_CASE(expr,kF(z),kF(a),kI(b),vx,vy) }  \
   else if (1==ABS(at) && 2==ABS(bt)) { SCALAR_EXPR_CASE(expr,kF(z),kI(a),kF(b),vx,vy) }  \
   else if (1==ABS(at) && 1==ABS(bt)) { SCALAR_EXPR_CASE(expr,kI(z),kI(a),kI(b),vx,vy) }  \
   else if (0==at || 0==bt) { dp(&z,verb,a,b); }

/* Macro case: N:N, 1:N, N:1 implicit looping for fun call, w/ optional suffix */
#define SCALAR_EXPR_FUN(fun, cres, ca, cb, post)                       \
   if (an==bn) { DO(zn, cres [i]= fun (ca [i], cb [i]) post) }     \
   else if (an==1) { DO(zn, cres [i]= fun (ca [0], cb [i]) post) } \
   else /* bn==1 */ { DO(zn, cres [i]= fun (ca [i], cb [0]) post) }

#endif
