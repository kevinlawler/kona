#include "incs.h"

#include "km.h"
#include "r.h"
#include "v.h"
#include "vq.h"

/* question mark - find/function_inverse - what dyadic triadic */

K find(K a, K b)
{
  I at=a->t, an=a->n, bt=b->t;
  P(at>0,DOE)
  if(-4==at && 4==bt)DO(an, if(kS(a)[i]==*kS(b))R Ki(i)) 
  if(-3==at && 3==bt)DO(an, if(kC(a)[i]==*kC(b))R Ki(i))
  if(-2==at && 2==bt)DO(an, if(!FC(kF(a)[i],*kF(b)))R Ki(i))
  if(-2==at && 1==bt){F fb=I2F(*kI(b));DO(an, if(!FC(kF(a)[i],fb))R Ki(i));}
  if(-1==at && 2==bt)DO(an, if(!FC(I2F(kI(a)[i]),*kF(b)))R Ki(i))
  if(-1==at && 1==bt)DO(an, if(kI(a)[i]==*kI(b))R Ki(i))
  if(!at){
    if(2==an&&-5==kK(a)[1]->t)R hash_find(a,b);
    DO(an, if(matchI(kK(a)[i],b))R Ki(i))
  }
  R Ki(an);
}

Z F num_ex(K a, F x)//f-> monadic, numeric in&out
{ 
  F y=0;
  K b,g;
  P(!(b=Kf(x)),FN) //err

  if(!(g=newK(0,1))){cd(b); R FN;}//err
  *kK(g)=ci(b);
  K k=vf_ex(&a,g);

  if(!k || (k->t!=1 && k->t!=2))y=FN; //err
  else if(k->t==1) y=(F)*kI(k);
  else y=*kF(k);

  cd(b);
  cd(k);
  cd(g);
  R y;
}

Z I isShallowNumeric(K k)
{
  if(ABS(k->t) > 2) R 0;
  if(0==k->t) DO(k->n, I t=kK(k)[i]->t; if(t!=1 && t!=2) R 0)
  R 1;
}

Z F ithFloat(K k, I i) //made specific for what_triadic
{
  if(!k) R 0;
  I n=k->n;
  if(!k->t) {k=kK(k)[i%n]; i=0;}
  if(1==ABS(k->t)) R (F) kI(k)[i%n];
  R kF(k)[i%n];
}

Z F inverter(K a, K b, K c, I index)//secant method
{
  F y = ithFloat(b,index);

  I i,m=20;//max iterations
  F x[m+2], f[m+2];
  x[0]=0.9998;
  x[1]=0.9999;

  if(c)
  {
    F r=ithFloat(c,index);
    //TODO: r== 0n 0i etc ??
    x[0]=0.9999*r;
    x[1]=r;
  }

  DO(2, f[i]=num_ex(a,x[i])-y); //oom/err FN ?? how to catch
  F d, e=y?y*0.000001:0.000001;//y*1e-6

  for(i=0;i<m;i++)
  { d=(x[i+1]-x[i])/(f[i+1]-f[i])*f[i+1];
    x[i+2]=x[i+1]-d;
    f[i+2]= num_ex(a,x[i+2])-y; //oom/err FN ?? how to catch
    if(ABS(d) < e || !FC(f[i+2],0.)) break;
  }
  if(i>=m){ kerr("limit"); R 0;}
  R x[i+2];
}

K what_triadic(K a, K b, K c)//TODO: 0i -0i 0n
{
  //TODO:  {1}?1 -> 0n ??
  I bt=b->t, bn=b->n;
  if(!isShallowNumeric(b) || (c && !isShallowNumeric(c))) R TE;
  if((!bt && !bn) || (c && !c->t && !c->n)) R newK(0,0);
  if(0==bn || (c && 0==c->n)) R newK(-2,0);
  if(c && c->t < 1 && bt < 1 && c->n != b->n) R LE;

  I zn=bn, zt=2;
  if(bt<1 || (c && c->t < 1)) zt = -2;
  if(c) zn=MAX(zn,c->n);
  K z = newK(zt,zn);
  U(z)
  DO(zn, kF(z)[i] = inverter(a,b,c,i))
  R z;
}

Z K qrand(K a,K b)
{
  I at=a->t,bt=b->t;
  K y;
  P(1!=ABS(at)||(1!=bt&&2!=bt),IE)
  I c=*kI(a),n=ABS(c);
  P(1==bt && c<0 && *kI(b) < -c,LE)
  P(1==bt && *kI(b)<0,DOE)

  I j=0,k,s;
  U(y=newK(1==bt?-1:-2,n))

  if(2==bt){F f=*kF(b);DO(n,kF(y)[i]=RF()*f) R y;}
  I d=*kI(b);
  if(c>=0)DO(n,kI(y)[i]=d*RF())    //this could be better (small numerical error)
  else //deal
  {
    vitter(kI(y),y->n,d); //Vitter's algorithm
    for(j=n-1;j>0;j--){k=(1+j)*RF();s=kI(y)[j];kI(y)[j]=kI(y)[k];kI(y)[k]=s;} //Knuth Algorithm 3.4.2P
  }
  R y;
}

K sample(K x,K y)
{
  K a,b,z;
  if(!y->n) R take(x,y);
  U(b=Ki(countI(y)))
  a=qrand(x,b);
  M(a,b) cd(b);
  z=at_verb(y,a);
  cd(a);
  R z;
}

K what(K x, K y)
{
  if(7==xt)R what_triadic(x,y,0);
  if(1==xt) R atomI(y)?qrand(x,y):sample(x,y); 
  R find(x,y);
}
