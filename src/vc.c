#include "incs.h"
#include "scalar.h"
#include "k.h"
#include "km.h"
#include "ko.h"
#include "vc.h"

/* comparison verbs */

Z K lessmore(K a,K b,I x);

K dp(K*z,K(*f)(K,K),K x,K y) //dyad promote
{
   x=promote(x); y=promote(y);
   M(x,y,*z)
   DO((*z)->n, if(!(kK(*z)[i]=f(kK(x)[i%xn],kK(y)[i%y->n]))){cd(*z);*z=ME;break;}) //TODO: optimization: remove these modulo % operations
   cd(x);cd(y); 
   R 0;
}

K equals(K a, K b)
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  if(at <=0 && bt <= 0 && an != bn) R LE;
  I AT=ABS(at), BT=ABS(bt);
  if(4<AT || 4<BT) R TE;//(this catches it before descending)
  //Type Error - No 0-list, Not both numeric, or both char, or both sym
  if(at && bt && !(2>=AT && 2>=BT) && !(3==AT && 3==BT) && !(4==AT && 4==BT) ) R TE;
  I t= (!at||!bt)?0:MIN(at,bt)<0?-1:1;//Any 0-list? Zero. Any vector? -1. Both atoms? 1.
  I zn=at>0?bn:an;
  K z=newK(t,zn); //oom
#define EQ(x, y) (x) == (y)
  if     (2==AT && 2==BT) SCALAR_EXPR_FUN(FC, kI(z), kF(a), kF(b), ?0:1)
  else if(2==AT && 1==BT) SCALAR_EXPR_FUN(FC, kI(z), kF(a), kI(b), ?0:1)
  else if(1==AT && 2==BT) SCALAR_EXPR_FUN(FC, kI(z), kI(a), kF(b), ?0:1)
  else if(1==AT && 1==BT) SCALAR_OP_CASE(EQ, kI(z), kI(a), kI(b))
  else if(3==AT && 3==BT) SCALAR_OP_CASE(EQ, kI(z), kC(a), kC(b))
  else if(4==AT && 4==BT) SCALAR_OP_CASE(EQ, kI(z), kS(a), kS(b)) //works because of interning
  else if(0==at || 0==bt) dp(&z,equals,a,b);
#undef EQ
  R z;
}

I matchI(K a, K b)
{
  if(!a||!b)R 0;//Using this in over adverb type stuff
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  I AT=ABS(at), BT=ABS(bt);
  K *c,*d;
  //if(an!=bn || (at!=bt && !(1==AT && 2==BT) && !(2==AT && 1==BT)))R 0; // 0 ~ 1.0 ~ 1
  if(an!=bn || at!=bt) R 0;
  if(4==AT)DO(an, if(kS(a)[i]!=kS(b)[i]) R 0 )
  if(3==AT)DO(an, if(kC(a)[i]!=kC(b)[i]) R 0 )
  if(2==AT && 2==BT)DO(an, if(FC(kF(a)[i],kF(b)[i])) R 0 )
  //if(2==AT && 1==BT)DO(an, if(FC(kF(a)[i],kI(b)[i])) R 0 ) 
  //if(1==AT && 2==BT)DO(an, if(FC(kI(a)[i],kF(b)[i])) R 0 )
  if(1==AT && 1==BT)DO(an, if(kI(a)[i]!=kI(b)[i]) R 0 )
  if(0==AT || 5==AT)DO(an, if(!matchI(kK(a)[i],kK(b)[i]))R 0)//Dictionary keys are ordered sets
  if(7==AT)  
  {
    if(a->n!=b->n) R 0;
    
    switch(a->n)
    {
      CS(1,
            an=kVC(a)->n-1;
            bn=kVC(b)->n-1;
            if(an!=bn) R 0;
            DO(an, c=kW(a)[i];d=kW(b)[i]; if(VA(c)||VA(d)){if(c!=d) R 0;} else if(!matchI(*c,*d)) R 0) //TODO: Projection (up above?)
        )
      CS(2, )//TODO
      CS(3, if(kV(a)[CONTeXT] != kV(b)[CONTeXT])R 0; R matchI(kV(a)[CODE],kV(b)[CODE])) //TODO: Projection (up above?)
    }
  }
  R 1;
}

K match(K a, K b){R Ki(matchI(a,b));}

Z K lessmore(K a, K b, I x)
{

  if (!x){K c=a;a=b;b=c; } //NB: If primitives modify a but not b (or vice-versa. e.g. reuse of refcount 1 objects) 
                           //this should be reviewed. in q it can effect dicts (borror). see backup for unfactored ver.
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  if(at <=0 && bt <= 0 && an != bn) R LE;
  I AT=ABS(at), BT=ABS(bt);
  if(4<AT || 4<BT) R TE;//also Type Error (this catches it before descending)
  // Type Error - No 0-list, Not both numeric, or both char, or both sym
  if(at && bt && !(2>=AT && 2>=BT) && !(3==AT && 3==BT) && !(4==AT && 4==BT) ) R TE;
  I t= (!at||!bt)?0:MIN(at,bt)<0?-1:1;//Any 0-list? Zero. Any vector? -1. Both atoms? 1.
  I zn=at>0?bn:an;
  K z=newK(t,zn);
  U(z)
  I*h=kI(z);

  if(0==at || 0==bt)
  {
   a=promote(a); b=promote(b); //copy-pasted from dp()
   M(a,b,z);
   DO(zn, if(!(kK(z)[i]=lessmore(kK(a)[i%an],kK(b)[i%b->n],x))){cd(z);z=ME;break;})
   cd(a);cd(b); 
  }
  else 
  {
#define GT(x, y) (x) > (y)
    if     (2==AT && 2==BT)  SCALAR_EXPR_FUN(FC, h, kF(a), kF(b), >0)
    else if(2==AT && 1==BT)  SCALAR_EXPR_FUN(FC, h, kF(a), kI(b), >0)
    else if(1==AT && 2==BT)  SCALAR_EXPR_FUN(FC, h, kI(a), kF(b), >0)
    else if(1==AT && 1==BT)  SCALAR_OP_CASE(GT, kI(z), kI(a), kI(b))
    else if(3==AT && 3==BT)  SCALAR_OP_CASE(GT, kI(z), kC(a), kC(b))
    else if(4==AT && 4==BT) {SCALAR_EXPR_FUN(SC, h, kS(a), kS(b), >0)}
#undef GT
  }
  
  R z;
}

K less(K a, K b){R lessmore(a,b,0);}
K more(K a, K b){R lessmore(a,b,1);}
