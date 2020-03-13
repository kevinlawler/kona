#include "incs.h"
#include "scalar.h"
#include "k.h"
#include "r.h"
#include "vc.h"

#include "va.h"

/* scalar arithmetic verbs */

#ifdef K3_ARITH

F kpow(F a,F b)
{
  if(isnan(a))a=0.;
  if(isnan(b)){
    if(FC(a,0.))R FN;
    b=0;
  }else if(isinf(b)&&!FC(a,1.))R FN;
  if(!FC(b,0.))R isinf(a)?FN:1.;
  else if(!FC(a,0.))R 0.;
  R pow(a,b);
}

#endif

K power(K a, K b)
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  I type = MAX(ABS(at),ABS(bt));

  P(at <= 0 && bt <= 0 && an != bn, LE)
  P(type > 2, TE);

  I zt=type;
  if(MIN(at,bt) < 1) zt=-zt;
  if(!at || !bt) zt=0;
  if(1==zt*zt)zt*=2;
  I zn=at>0?bn:an;
  K z=newK(zt,zn); U(z)

#ifndef K3_ARITH
  F x,y;
  //K3.2 silently yields 0n for -3^0.5 , even though some Kx documentation says domain error.
  #define FPOWER kF(z)[i]=(0==y)?1:(0==x)?0:pow(x,y); //x^0==1; 0^y==0 for y!=0; rest should be same as pow
  SCALAR_EXPR(FPOWER,power,x,y)
  #undef FPOWER
#else
  #define KPOW_FI(x,y) kpow(x,I2F(y))
  #define KPOW_IF(x,y) kpow(I2F(x),y)
  #define KPOW_II(x,y) kpow(I2F(x),I2F(y))
  if(2==ABS(at)&&2==ABS(bt)){ SCALAR_OP_CASE(kpow,kF(z),kF(a),kF(b)) }
  else if(2==ABS(at)&&1==ABS(bt)){ SCALAR_OP_CASE(KPOW_FI,kF(z),kF(a),kI(b)) }
  else if(1==ABS(at)&&2==ABS(bt)){ SCALAR_OP_CASE(KPOW_IF,kF(z),kI(a),kF(b)) }
  else if(1==ABS(at)&&1==ABS(bt)){ SCALAR_OP_CASE(KPOW_II,kF(z),kI(a),kI(b)) }
  else if(0==at||0==bt){ dp(&z,power,a,b); }
#endif
  R z;
}

K plus(K a, K b) //compare plus() to times() or minus()
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)          //Finally, we know what we're going to make

  #define PLUS(x, y) ((x) + (y))
#ifndef K3_ARITH
  SCALAR_OP(PLUS,plus)
#else
  #define PLUS_FI(x, y) ((x) + I2F(y))
  #define PLUS_IF(x, y) (I2F(x) + (y))
  if(2==ABS(at)&&2==ABS(bt)){ SCALAR_OP_CASE(PLUS,kF(z),kF(a),kF(b)) }
  else if(2==ABS(at)&&1==ABS(bt)){ SCALAR_OP_CASE(PLUS_FI,kF(z),kF(a),kI(b)) }
  else if(1==ABS(at)&&2==ABS(bt)){ SCALAR_OP_CASE(PLUS_IF,kF(z),kI(a),kF(b)) }
  else if(1==ABS(at)&&1==ABS(bt)){ SCALAR_OP_CASE(PLUS,kI(z),kI(a),kI(b)) }
  else if(0==at||0==bt){ dp(&z,plus,a,b); }
  #undef PLUS_FI
  #undef PLUS_IF
#endif
  #undef PLUS

  R z;
}

K times(K a, K b)//TODO: Float results will respect intermediate OI or Oi. Other functions too. (& casts.)
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)

  #define TIMES(x, y) ((x) * (y))
#ifndef K3_ARITH
  SCALAR_OP(TIMES,times)
#else
  #define TIMES_FI(x, y) ((x) * I2F(y))
  #define TIMES_IF(x, y) (I2F(x) * (y))
       if(2==ABS(at)&&2==ABS(bt)){ SCALAR_OP_CASE(TIMES,   kF(z),kF(a),kF(b)) }
  else if(2==ABS(at)&&1==ABS(bt)){ SCALAR_OP_CASE(TIMES_FI,kF(z),kF(a),kI(b)) }
  else if(1==ABS(at)&&2==ABS(bt)){ SCALAR_OP_CASE(TIMES_IF,kF(z),kI(a),kF(b)) }
  else if(1==ABS(at)&&1==ABS(bt)){ SCALAR_OP_CASE(TIMES,   kI(z),kI(a),kI(b)) }
  else if(0==at||0==bt){ dp(&z,times,a,b); }
  #undef TIMES_FI
  #undef TIMES_IF
#endif
  #undef TIMES

  R z;
}

K _dot(K a,K b)
{
  SCALAR_INIT(2);
  I A=ABS(at),B=ABS(bt);
  I accI=0;F accF=0.0;
  #define DOT_F   accF+=x*y
  #define DOT_FI  accF+=x*I2F(y)
  #define DOT_IF  accF+=I2F(x)*y
  #define DOT_I   accI+=x*y
       if(2==A&&2==B){ F x,y;   SCALAR_EXPR_CASE(DOT_F, F,kF(a),kF(b),x,y) }
  else if(2==A&&1==B){ F x;I y; SCALAR_EXPR_CASE(DOT_FI,F,kF(a),kI(b),x,y) }
  else if(1==A&&2==B){ I x;F y; SCALAR_EXPR_CASE(DOT_IF,F,kI(a),kF(b),x,y) }
  else if(1==A&&1==B){ I x,y;   SCALAR_EXPR_CASE(DOT_I, I,kI(a),kI(b),x,y) }
  else if(0==A||0==B){
    V p[]={0,(V)0x16};
    K x,y=overDyad(0,p+2,(x=times(a,b))); cd(x);
    R y;
  }
  R 1==ABS(zt)?Ki(accI):Kf(accF);
}

K mod(K a, K b) //In K4: {x-y*x div y}
{
  I at=a->t, an=a->n, bt=b->t;
  P(ABS(at) > 2,TE)
  //Know bt in 1,2  and  at in -2,-1,0,1,2
  I t=(0==at)?0:MAX(ABS(at),ABS(bt))*(at>0?1:-1);

  K z=newK(t,an); U(z)
  I c,d,e; F f,g,h;
#if __INT_MAX__ == 2147483647
  F ct=1e-13; // Comparison tolerance for 32 bit
#else
  F ct=0; // Not needed for 64 bit
#endif
  #define FMOD h=g?f-g*floor(ct+f/g):f; kF(z)[i]=(ABS(h)>ct)?h:0;
  if     (2==ABS(at) && 2==bt) { g=*kF(b); DO(an, f=kF(a)[i]; FMOD) }
  else if(2==ABS(at) && 1==bt) { g=*kI(b); DO(an, f=kF(a)[i]; FMOD) }
  else if(1==ABS(at) && 2==bt) { g=*kF(b); DO(an, f=kI(a)[i]; FMOD) }
  else if(1==ABS(at) && 1==bt)
  {
    g=d=*kI(b);
    // K 2.91, K 3.2: the sign of result = sign of b
    DO(an, c=kI(a)[i]; e=d?c-d*floor(c/g):c; kI(z)[i]=e)
    // if(d>0) DO(an, c=kI(a)[i]; e=d?c-d*(c%d):c; kI(z)[i]=e)
    // else    DO(an, c=kI(a)[i]; e=d?c-d*floor(c/(F)d):c; kI(z)[i]=e) //TODO: casting to F is slow/wrong for big#. NB: floor does not equal truncate for negatives

  }
  else if(0==at) DO(an, if(!(kK(z)[i]=mod(kK(a)[i],b))){cd(z);R 0;})
  R z;
}

K minus(K a, K b)
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)

  #define MINUS(x, y) ((x) - (y))
#ifndef K3_ARITH
  SCALAR_OP(MINUS,minus)
#else
  #define MINUS_FI(x, y)  ((x) - I2F(y))
  #define MINUS_IF(x, y)  (I2F(x) - (y))
  if(2==ABS(at)&&2==ABS(bt)){ SCALAR_OP_CASE(MINUS,kF(z),kF(a),kF(b)) }
  else if(2==ABS(at)&&1==ABS(bt)){ SCALAR_OP_CASE(MINUS_FI,kF(z),kF(a),kI(b)) }
  else if(1==ABS(at)&&2==ABS(bt)){ SCALAR_OP_CASE(MINUS_IF,kF(z),kI(a),kF(b)) }
  else if(1==ABS(at)&&1==ABS(bt)){ SCALAR_OP_CASE(MINUS,kI(z),kI(a),kI(b)) }
  else if(0==at||0==bt){ dp(&z,minus,a,b); }
  #undef MINUS_FI
  #undef MINUS_IF
#endif
  #undef MINUS

  R z;
}

K negate(K x){K y,z; U(y=Ki(0)) z=minus(y,x); cd(y); R z;} //TODO: probably implemented using negation vector operations

K divide(K a, K b)//NB: Integral values promoted to float
{
  SCALAR_INIT(2)
  //if(1==zt*zt)zt*=2;//don't do because I%I is now I
  //if(zt==1)zt=2;
  K z=newK(zt,zn);U(z)

  F u,d,y=FI;//nUmerator, Denominator, infinitY
  //TODO:nulls;is it necessary to check for inf? IEEE may handle it already everywhere
  //TODO: ensure that 1/inf==0 and 1/-inf ==0

  I s,t,w=II;
  if(1==ABS(at) && 1==ABS(bt))//save I from being cast to F for greater accuracy
  { if (an==bn)      { DO(zn,s= kI(a)[i];t=kI(b)[i];kI(z)[i]=!t?!s?IN:s>0?w:-w:s/t)}
    else if (an==1)  { DO(zn,s= kI(a)[0];t=kI(b)[i];kI(z)[i]=!t?!s?IN:s>0?w:-w:s/t)}
    else /* bn==1 */ { DO(zn,s= kI(a)[i];t=kI(b)[0];kI(z)[i]=!t?!s?IN:s>0?w:-w:s/t)}
    R z; }

  #define FDIVIDE kF(z)[i]=!d?!u?FN:u>0?y:-y:u/d //0/0=FN, 1/0=oo, -1/0=-oo, 1/2=0.5
  SCALAR_EXPR(FDIVIDE,divide,u,d)

  R z;
}

K reciprocal(K x){K y,z; U(y=Kf(1)) z=divide(y,x); cd(y); R z;}

K min_and(K a, K b)
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)
  SCALAR_OP(MIN,min_and)
  R z;
}

K max_or(K a, K b)
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)

  SCALAR_OP(MAX,max_or)

  R z;
}

K floor_ceil(K a, F(*g)(F))
{
  if(strcmp(errmsg,"(nil)"))R (K)0;
  I at=a->t, an=a->n;
  F(*h)(F)=g==ceil?floor:ceil;
  P(2<ABS(at) || (0==at && 7==kK(a)[0]->t),TE)
  if(1==ABS(at))R ci(a);

  //TODO: oom
  K z=newK(at?SIGN(at):0,an);//Compress F {-2,2} into I {-1,1}
  F e,f;I r;
  if(2==ABS(at))DO(an, e=kF(a)[i]; if(isnan(e))r=IN;else if(isinf(e)||e<=-II||e>=II)r=e<0?-II:II;else {f=FF(e); r=(f>0&&!FC(f,1))||(f<0&&!FC(f,0))?h(e):g(e);} kI(z)[i]=r)
  else if(!at) DO(an, kK(z)[i]=floor_ceil(kK(a)[i],g))
  R z;
}

K floor_verb(K a){R floor_ceil(a,floor);}//K3.2  "_ -5 + 1.0 * 1 + -OI" yields -0I not Domain Error
