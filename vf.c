#include "incs.h"
#include "k.h"
#include "km.h"
#include "vf.h"

/* format */

Z I TNI(I p,C h);
Z I parseNI(S s,I n);

S CSK(K x){ R !x?0:4==xt?*kS(x):3==ABS(xt)?kC(x):0;}//non-allocating CSTRING from K. assumes +4,+-3 types are null-terminated

Z K formKsCS(S s) 
{
  //Could remove this function. It's equivalent to Ks(sp(s))
  S t=sp(s);
  if(!t)R 0; //oom
  K z=Ks(t); //oom
  if(!z)R 0;
  R z;
}

K formKiCS(S s) //  0 $ "123\000456\000" is 123 ('\0' char)
{
  C *p,q=0;
  I r=IN;

  I w=parseNI(s,strlen(s));
  if(w) r=NI[w]; 
  else if(*s)
  {
    r=strtoll(s,&p,10);
    errno=0; //is this ok to do?
    q=*p;
    if(IN==r)r=-II;//if r < -0I then r=-0I
  }
  P(q && !isblank(q),DOE)
  R Ki(r);//oom
}

K formKfCS(S s) // 0.0 $ "123\000456\000" is 123 ('\0' char)
{
  C *p,q=0;
  F r=FN;

  I w=parseNI(s,strlen(s));
  if(w) r=ni[w]; 
  else if(*s)
  {
    r=strtod(s,&p);
    errno=0;//is this ok to do?
    q=*p;
    if(isnan(r))r=-FI; //'r==FN' does not work
  }
  P(q && !isblank(q),DOE)
  R Kf(r); //oom
}

Z K formatS(S x)
{ I n=strlen(x);
  K z=newK(-3,n);
  if(z)sprintf(kC(z),"%s",x); //OK since 3/-3 is null-terminated
  R z;
}
Z K formatF(F x, I y, I c)
{ 
  int k=y;
  S b= 0==c?"%.*g":1==c?"%.*f":"%.*e";// %#.*g ?? 
  I n=snprintf(0,0,b,k,x);  
  K z=newK(-3,n);
  if(z)sprintf(kC(z),b,k,x);
  R z;
}
Z K formatI(I x)
{ 
  I n=snprintf(0,0,"%lld",x);
  K z=newK(-3,n);
  if(z)sprintf(kC(z),"%lld",x); 
  R z;
}
K format(K a) 
{
  I at=a->t, an=a->n;
  K z;
  if(3==ABS(at)){z=kclone(a); z->t=-3; R z;}
  else if(7==at)R 0;//TODO: wordfunc and charfunc and cfunc
  else if(6==at)R newK(-3,0);
  else if(5==at)R formatS(sp(".(..)"));//Beats me -- this has a similar signature to a _hash
  else if(4==at)R formatS(*kS(a));
  else if(2==at)R formatF(*kF(a),PP,0);
  else if(1==at)R formatI(*kI(a));
  z=newK(0,an);
  if     ( 0==at)DO(an, kK(z)[i]=format (kK(a)[i]))
  else if(-1==at)DO(an, kK(z)[i]=formatI(kI(a)[i]))
  else if(-2==at)DO(an, kK(z)[i]=formatF(kF(a)[i],PP,0))
  else if(-4==at)DO(an, kK(z)[i]=formatS(kS(a)[i]))
  R z;
}

I NI[]={0,IN,-II,II,II,-II,II};  //0N,-0I,0I,0n,-0i,0i maps to I
F ni[]={0,FN,-FI,FI,FN,-FI,FI};  //maps to F
Z I TNI(I p,C h) //transition function for parsing 0N -0I 0I 0n ...
{
  I c=isblank(h)?0:charpos(" -0NIni",h); //character classes
  if(0==c &&  7>=p)         R   p;
  if(1==c && (0==p || 7==p))R 7-p;
  if(2==c &&  0==p)         R   9;
  if(2==c &&  7==p)         R   8;
  if(3==c && (8==p || 9==p))R   1;
  if(4==c && (8==p || 9==p))R p-6;
  if(5==c && (8==p || 9==p))R   4;
  if(6==c && (8==p || 9==p))R p-3;
  R 10;
}

Z I parseNI(S s,I n){I i=0,p=0; while(i<n && *s)p=TNI(p,*s++); R p<7?p:0;}
Z F tround(F f){F d=FF(f); R (d>0&&!FC(d,1))||(d<0&&!FC(d,0))?ceil(f):floor(f);}

//TODO: Really weird:  run '`g $ 99' run '. _d' see entry '(`s4;99;) in the `.k K-Tree
//      also run '`s $ 1.0' -> domain error
//TODO: oom all
K dollar(K a, K b) //form/format_dyadic
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  K z;
  I x = (at <=0 && -3 != at), y = (bt <=0 && -3 != bt);
  P(x && y && an!=bn,LE)

  if(x || y)
  {
    a=x?promote(a):ci(a); //-3
    b=y?promote(b):ci(b); //-3
    z=a&&b?newK(0,x?a->n:b->n):0;
    if(z)DO(z->n, K q=dollar(x?kK(a)[i]:a,y?kK(b)[i]:b); M(q,z,a,b) kK(z)[i]=q)
    cd(a);cd(b);
    R demote(z);
  }

  if(1==at && *kI(a)) //"Format (Dyadic)"
  {
    K c;
    U(c=format(b))
    I m=*kI(a); 
    z=newK(-3,ABS(m));M(c,z)
    if(z->n < c->n) DO(z->n, kC(z)[i]='*')//K3.2
    else
    {
      I k=m>0?m-c->n:0;
      DO(z->n,kC(z)[i]=' ');
      DO(c->n,kC(z)[i+k]=kC(c)[i])
    } 
    cd(c);
    R z;
  }

  if(2==at) //"Format (Dyadic)"
  {
    F f=*kF(a);
    if(2==bt || 1==bt)
    {
      K c,d;
      U(c=Ki(f))
      d=formatF(2==bt?*kF(b):*kI(b), ((I)tround(fabs(f)*10))%10, signbit(f)?2:1);
      if(d)z=dollar(c,d);
      cd(c);cd(d);
      R z;
    }
  }

  if(3==ABS(bt)) //"Form"
  {
    if(3==bt) b=enlist(b);//mm/o

    if(4==at && !strlen(*kS(a))) R formKsCS(CSK(b));
    if(3==ABS(at))               R ci(b);
    if(2==at)                    R formKfCS(CSK(b)); //Had '&& 0.0 == *kF(a)' here but the manual is wrong
    if(1==at && !*kI(a))         R formKiCS(CSK(b));
    //if(5<=at)
    R 0;//TODO: Else parse-execute (6,7, looks like for 5, maybe 4 too???)
  }

  R TE;
}
