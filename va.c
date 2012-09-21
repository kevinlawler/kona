#include "incs.h"
#include "scalar.h"
#include "k.h"
#include "r.h"
#include "vc.h"

/* scalar arithmetic verbs */

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

  F x,y;
  //K3.2 silently yields 0n for -3^0.5 , even though some Kx documentation says domain error.
  #define FPOWER kF(z)[i]=(0==y)?1:(0==x)?0:pow(x,y); //x^0==1; 0^y==0 for y!=0; rest should be same as pow
  SCALAR_EXPR(FPOWER,power,x,y)

  R z;
}

K plus(K a, K b) //compare plus() to times() or minus()
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)          //Finally, we know what we're going to make

  #define PLUS(x, y) ((x) + (y))
  SCALAR_OP(PLUS,plus)
  #undef PLUS

  R z;
}

K times(K a, K b)//TODO: Float results will respect intermediate OI or Oi. Other functions too. (& casts.)
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)

  #define TIMES(x, y) ((x) * (y))
  SCALAR_OP(TIMES,times)
  #undef TIMES

  R z;
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
    d=*kI(b); 
    if(d>0) DO(an, c=kI(a)[i]; e=d?c-d*(c/d):c; kI(z)[i]=e)
    else    DO(an, c=kI(a)[i]; e=d?c-d*floor(c/(F)d):c; kI(z)[i]=e) //TODO: casting to F is slow/wrong for big#. NB: floor does not equal truncate for negatives

  } 
  else if(0==at) DO(an, if(!(kK(z)[i]=mod(kK(a)[i],b))){cd(z);R 0;}) 
  R z;
}

K minus(K a, K b)
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)              

  #define MINUS(x, y) ((x) - (y))
  SCALAR_OP(MINUS,minus)
  #undef MINUS

  R z;
}

K negate(K x){K y,z; U(y=Ki(0)) z=minus(y,x); cd(y); R z;} //TODO: probably implemented using negation vector operations

K divide(K a, K b)//NB: Integral values promoted to float
{
  SCALAR_INIT(2)
  //if(1==zt*zt)zt*=2;//don't do because I%I is now I
  K z=newK(zt,zn);U(z)

  F u,d,y=FI;//nUmerator, Denominator, infinitY
  //TODO:nulls;is it necessary to check for inf? IEEE may handle it already everywhere
  //TODO: ensure that 1/inf==0 and 1/-inf ==0

  I s,t,w=II;

  if(1==ABS(at) && 1==ABS(bt))//save I from being cast to F for greater accuracy
  { 
    if (an==bn)      { DO(zn,s= kI(a)[i];t=kI(b)[i];kI(z)[i]=!t?!s?0:s>0?w:-w:s/t)}
    else if (an==1)  { DO(zn,s= kI(a)[0];t=kI(b)[i];kI(z)[i]=!t?!s?0:s>0?w:-w:s/t)}
    else /* bn==1 */ { DO(zn,s= kI(a)[i];t=kI(b)[0];kI(z)[i]=!t?!s?0:s>0?w:-w:s/t)}
    R z;
  }

  #define FDIVIDE kF(z)[i]=!d?!u?0:u>0?y:-y:u/d //0/0=0, 1/0=oo, -1/0=-oo, 1/2=0.5 
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
  I at=a->t, an=a->n;
  F(*h)(F)=g==ceil?floor:ceil;
  P(2 <ABS(at),TE)
  if(1==ABS(at))R ci(a);

  //TODO: oom
  K z=newK(at?SIGN(at):0,an);//Compress F {-2,2} into I {-1,1}
  F e,f;
  if(2==ABS(at)) DO(an, e=kF(a)[i]; f=FF(e); kI(z)[i]=(f>0&&!FC(f,1))||(f<0&&!FC(f,0))?h(e):g(e))
  else if(!at) DO(an, kK(z)[i]=floor_ceil(kK(a)[i],g))
  R z;
}

K floor_verb(K a){R floor_ceil(a,floor);}//K3.2  "_ -5 + 1.0 * 1 + -OI" yields -0I not Domain Error
