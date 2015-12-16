#include "incs.h"
#include "k.h"
#include "kg.h"
#include "km.h"
#include "vc.h"
#include "vg.h"

#include "mt.h"

/* grade / grouping / sorting / shape verbs */

//TODO: sort type-0 lists, functions, symbols, etc.
//note: K appears to sort _reserved functions by alphabetical order, but this may be a side effect of
//      sorting by address (if the functions appear in alphabetical order in the source file)
//      though oddly enough it appears to hold across all underscore/reserved functions (not just math)
//      _in falls to the end since it is defined as some variation on a char func that works as an inline verb
//      If you look at the binary the symbols appear to be listed out of alphabetical order
//one interesting way to test how functions are sorted would be to load the interpreter twice,
//inputting the same collection of functions each time but in a different order
//if the sort order changes for each instance then sorting is probably based on pointer/reference value
//if that fails then it may be necessary to look at distinctions between wordfunc,charfunc, valence, proj, etc
#define DGT (1<<26)
#define MSB ((uI)IN)
Z I FtoI(F a){union{F f;I i;}u;if(isnan(a))R LLONG_MIN;u.f=a;R 0>u.i?LLONG_MIN-u.i:u.i;}
Z uI ItoU(I a){R 0x8000000000000000ULL^(uI)a;}

K grade_updown(K a, I r)
{
  I at=a->t, an=a->n;
  P(0< at, RE)
  if(-4==at)R symGrade(a,r);
  if(-3==at)R charGrade(a,r);
  if(-1==at||-2==at){
    K z;
    if(an<2){z=newK(-1,an);M(z);DO(an,kI(z)[i]=i);R z;}
    else{
      K x=0;uI y,u=(uI)-1,v=0,h=0,k;//MIN,MAX
      if(-2==at){x=newK(-1,an);M(x);}
      //trst();
      //elapsed("x=newK");
      if(-1==at)DO(an,y=kI(a)[i];h|=y;if(y<u)u=y;if(y>v)v=y)
      else DO(an,kU(x)[i]=(y=FtoI(kF(a)[i]));h|=y;if(y<u)u=y;if(y>v)v=y)
      //elapsed("fill x");
      //O("u:%016llx v:%016llx\n",u,v);
      if((r&&-1==at)||((u&MSB)!=(v&MSB))){
        u=(uI)-1;v=0;h=0;
        if(-1==at){
          x=newK(-1,an);M(x);
          DO(an,kU(x)[i]=(y=ItoU(kI(a)[i]));h|=y;if(y<u)u=y;if(y>v)v=y)}
        else DO(an,kU(x)[i]=(y=ItoU(kI(x)[i]));h|=y;if(y<u)u=y;if(y>v)v=y)}
      k=v-u;
           if(!k){z=newK(-1,an);M(z);DO(an,kI(z)[i]=i)}
      else if(an<IGT)z=insertGradeU(x?x:a,r);
      else if((k<DGT)&&((9*an+(1^19))>2*k))z=distributionGrade(x?x:a,r,u,v);
      else z=radixGrade(x?x:a,r,h);
      //elapsed("sort");
      cd(x); }
    R z; }
  R mergeGrade(a,r);
}
K grade_up(K a){R grade_updown(a,0);}
K grade_down(K a){R grade_updown(a,1);}

K enlist(K x)
{
  I t=(1<= xt && xt<=4)?-xt:0; //Atoms become vectors. Else becomes list.
  K z=newK(t,1);
  if(-4==t)*kS(z)=*kS(x);
  if(-3==t)*kC(z)=*kC(x);
  if(-2==t)*kF(z)=*kF(x);
  if(-1==t)*kI(z)=*kI(x);
  if( 0==t)*kK(z)=ci(x);
  R z;
}

Z K charRange(K a)
{
  I n=a->n,c[1+UCHAR_MAX],j=0;
  memset(c,0,(1+UCHAR_MAX)*sizeof(I));
  K z=newK(-3,n);M(z);
  DO(n,UC x=(UC)kC(a)[i];if(!c[x]){c[x]=-1;kC(z)[j++]=kC(a)[i];})
  if(n==j)R z;
  K y=newK(-3,j);M(z,y);
  memcpy(kC(y),kC(z),j*sizeof(C));cd(z);
  R y;
}

Z K symRange(K x)
{
  I j=0;
  K z=newK(-4,xn);M(z);
  setS(2,0);DO(xn,S s=kS(x)[i];if(!SV(s,2)){SV(s,2)=-1;kS(z)[j++]=s;})
  //O("u:%lld xn:%lld\n",u,xn);
  if(xn==j)R z;
  K y=newK(-4,j);M(z,y);
  memcpy(kS(y),kS(z),j*sizeof(S));cd(z);
  R y;
}

#define HFR 1
Z K newH(I n){ I m=1<<(HFR+cl2(n));K h=newK(-1,m);M(h);R h; }
Z I hg(K h,uI hk,I k,uI*p)
{
  I n=h->n,*d=kI(h);uI u=hk&(n-1);
  while(d[u]){
    if(k==d[u]){*p=u;R k;}
    if(++u==n)u=0;
  }*p=u;R 0;
}
#define hs(h,p,k) kI(h)[p]=(k)

Z uI hcc[8]={0,0,0,0,0,0,0,0};
Z void hcinit(){if(!hcc[0])DO(8,hcc[i]=genrand64_int64());}
Z uint32_t hc(uI u)
{
    DO(8,u^=hcc[i];u+=u>>8;)
    return (uint32_t)u^(u>>32);
}

Z K intRange(K x)
{
  hcinit();
  I j=0,h0=0,sa=0;uI m=0;
  K h=newH(xn);M(h);
  K z=newK(xt,xn);M(h,z);
  DO(xn,m|=kU(x)[i]);if(m)while(!(m&1)){m>>=1;sa++;}
  DO(xn,uI v=kU(x)[i];
      if(!v){if(!h0){h0=1;kI(z)[j++]=0;}}
      else{uI vsa=v>>sa;uI u=m<h->n?vsa:hc(vsa);uI p;
        if(!hg(h,u,vsa,&p)){hs(h,p,vsa);kI(z)[j++]=v;}})
  //O("u:%lld xn:%lld\n",u,xn);
  if(xn==j)GC;
  K y=newK(xt,j);if(!y)GC;
  memcpy(kI(y),kI(z),j*sizeof(I));
  cd(z);z=y;
cleanup:
  cd(h);
  R z;
}

Z I KEQ(K a, K b)//List Equal (K Equal)
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  I A=ABS(at);
  
  if(at!=bt)R 0;
  if(an!=bn)R 0;

  if     (7==A)R 0;//TODO: sort functions?
  else if(6==A)R 1;
  else if(5==A)R 0;//TODO: sort dictionaries?
  else if(4==A)DO(an, if(kS(a)[i]!=kS(b)[i])R 0)
  else if(3==A)DO(an, if(kC(a)[i]!=kC(b)[i])R 0)
  else if(2==A)DO(an, if(FC(kF(a)[i],kF(b)[i]))R 0)
  else if(1==A)DO(an, if(kI(a)[i]!=kI(b)[i])R 0)  
  else if(0==A)DO(an, if(!KEQ(kK(a)[i],kK(b)[i]))R 0)
  R 1;
}

Z K shg(K sh,uI hk,K k,uI*p)
{
  I n=sh->n;K*d=kK(sh);uI u=hk&(n-1);
  while(d[u]){
    if(KEQ(k,d[u])){*p=u;R k;}
    if(++u==n)u=0;
  }*p=u;R 0;
}
#define shs(sh,p,k) kK(sh)[p]=(k)

uint32_t fnv1a(UC *x,I n)//Fowler-Noll-Vo FNV-1a hash
{
  uint32_t h=2166136261UL;
  DO(n,h^=x[i];h*=16777619UL)R h;
}

Z uI hcode(K x)
{
  I t=ABS(xt);uI u=0;
  SW(t){
  CSR(7,R t)//nyi
  CSR(6,R (uI)&NIL)
  CSR(5,R t)//nyi
  CSR(4,DO(xn,S v=kS(x)[i];if(!SV(v,1))SV(v,1)=fnv1a((UC*)v,strlen(v));u+=SV(v,1))R xt+u)
  CSR(3,R xt+fnv1a((UC*)kC(x),xn))
  CSR(2,)
  CSR(1,DO(xn,uI v=kI(x)[i];u+=hc(v))R xt+u)
  CSR(0,DO(xn,K v=kK(x)[i];u+=hcode(v))R u)
  }
  R 0;
}

Z K listRange(K x)
{
  hcinit();
  I j=0;
  setS(1,0);
  K sh=newH(xn);M(sh);
  K z=newK(xt,xn);M(sh,z);
  DO(xn,uI p;K kv=kK(x)[i];
     uI u=hcode(kv);
     if(!shg(sh,u,kv,&p)){shs(sh,p,kv);kK(z)[j++]=ci(kv);})
  if(xn==j)GC;
  K y=newK(xt,j);if(!y)GC;
  DO(j,kK(y)[i]=ci(kK(z)[i]));cd(z);z=y;
cleanup:
  cd(sh);
  R z;
}

K range(K a)
{ 
  I t=a->t, n=a->n;
  K z=0,g=0,k=0;
  I u=n,*h=0,*m=0;
  P(t>0,RE)
  SW(-t){
  CSR(0,R listRange(a))
  CSR(1,)CSR(2,R intRange(a))
  CSR(3,R charRange(a))
  CSR(4,R symRange(a)) }

  g=grade_up(a);if(!g)GC;h=kI(g);
  k=newK(-1,n);if(!k)GC;m=kI(k);
  DO(n,m[h[i]]=i);

  //K3.2
  //v0:2.;v1:1.9999999999999
  //v0=v1 returns 1
  //v:v1,v0
  //<v    returns 0 1
  //?v    returns 2 2.0
  //=v    returns ,0 ,1
  DO(n-1, if(matchI(kK(a)[h[n-i-1]],kK(a)[h[n-i-2]])){h[n-i-1]=-1;--u;})

  z=newK(t,u); if(!z) GC;
  I x=0;

  DO(n, if(h[m[i]]>-1)kK(z)[x++]=ci(kK(a)[h[m[i]]]))

cleanup:
  cd(k);
  cd(g);
  R z;
}

Z K charGroup(K x)
{
  trst();
  I h[1+UCHAR_MAX],c[1+UCHAR_MAX],j=0;
  memset(h,0,(1+UCHAR_MAX)*sizeof(I));
  memset(c,0,(1+UCHAR_MAX)*sizeof(I));
  DO(xn,UC u=(UC)kC(x)[i];if(!h[u])h[u]=++j;I w=h[u]-1;c[w]++;)
  //elapsed(" cnt");
  K y=newK(0,j);M(y);
  DO(j,K z=newK(-1,c[i]);M(z,y);kK(y)[i]=z;c[i]=0;)
  //elapsed("newK");
  DO(xn,UC u=(UC)kC(x)[i];I w=h[u]-1;K z=kK(y)[w];kI(z)[c[w]++]=i)
  //elapsed("fill");
  R y;
}

Z K symGroup(K x)
{
  I j=0;
  K uk=newK(-1,xn);M(uk);I*u=kI(uk);
  setS(1,0);setS(2,0);
  DO(xn,S s=kS(x)[i];if(!SV(s,2)){u[j]=(I)s;SV(s,2)=++j;}SV(s,1)++)
  K y=newK(0,j);M(y,uk);
  DO(j,S s=(S)u[i];K z=newK(-1,SV(s,1));M(z,y,uk);kK(y)[i]=z;u[i]=0)
  DO(xn,S s=kS(x)[i];I w=SV(s,2)-1;K z=kK(y)[w];kI(z)[u[w]++]=i)
  cd(uk);
  R y;
}

Z K groupI(K x,K y,I n)//#x=#a;n=#?a
{
  K z=newK(0,n);M(z);I*c=kI(y);
  if(n<65537){
    DO(n,K v=newK(-1,c[i]);M(v,z);kK(z)[i]=v;c[i]=0)
    DO(xn,I w=kI(x)[i];K v=kK(z)[w];kI(v)[c[w]++]=i)
  }else{
    DO(n,K v=newK(-1,c[i]);M(v,z);kK(z)[i]=v;v->n=0)
    DO(xn,I w=kI(x)[i];K v=kK(z)[w];kI(v)[v->n++]=i)
  }
  cd(y);cd(x);
  R z;
}

Z K intGroup(K x)
{
  hcinit();
  I j=0,h0=0,sa=0;uI m=0;
  K h=newH(xn);M(h);K ok=newK(-1,h->n);M(ok,h);I*o=kI(ok);
  K xok=newK(-1,xn);M(xok,ok,h);I*xo=kI(xok);
  K ck=newK(-1,xn);M(ck,xok,ok,h);I*c=kI(ck);
  DO(xn,m|=kU(x)[i]);if(m)while(!(m&1)){m>>=1;sa++;}
  DO(xn,uI v=kU(x)[i];
      if(!v){if(!h0)h0=j++;xo[i]=h0;c[h0]++;}
      else{v>>=sa;uI u=m<h->n?v:hc(v);
      uI p;if(!hg(h,u,v,&p)){hs(h,p,v);o[p]=j++;}
      I w=o[p];xo[i]=w;c[w]++;})
  cd(ok);cd(h);
  K z=groupI(xok,ck,j);
  R z;
}

Z K listGroup(K x)
{
  hcinit();
  I j=0;
  K h=newH(xn);M(h);K ok=newK(-1,h->n);M(ok,h);I*o=kI(ok);
  K xok=newK(-1,xn);M(xok,ok,h);I*xo=kI(xok);
  K ck=newK(-1,xn);M(ck,xok,ok,h);I*c=kI(ck);
  DO(xn,K v=kK(x)[i];uI u=hcode(v);
      uI p;if(!shg(h,u,v,&p)){shs(h,p,v);o[p]=j++;}
      I w=o[p];xo[i]=w;c[w]++)
  cd(ok);cd(h);
  K z=groupI(xok,ck,j);
  R z;
}

K group(K x)
{
  I t=xt, n=xn;
  P(t>0,RE)
  I u=n,*g,*h;
  K z=0,b=0,c=0;

  SW(-t){
  CSR(0,R listGroup(x))
  CSR(1,)CSR(2,R intGroup(x))
  CSR(3,R charGroup(x))
  CSR(4,R symGroup(x)) }
  
  M(b=grade_up(x));g=kI(b);
  //Nastier code would eliminate this second sort.
  c=newK(-1,n);M(b,c);h=kI(c);
  DO(n,h[g[i]]=i);
  //Step through, on duplicate set uniques-=1, mark by inverting sign of corresponding index
  if( 0==t)DO(n-1,if(matchI(kK(x)[g[n-i-1]],kK(x)[g[n-i-2]])){--u;g[n-i-1]*=-1;})
 
  z=newK(0,u);
  M(b,c,z);
  I k=0,p=0,v;
  while(p<n && k<u)//This is a tricky algorithm.
  { //Dupes in g marked negative. h[p] is index of a[p] in sorted a
    for(v=1;p+v<n && g[h[p]+v]<0;v++);//Find the length of z[k]
    K s=newK(-1,v); 
    M(b,c,z,s)
    DO(v, kI(s)[i]=ABS(g[h[p]+i]))//ABS because duplicates marked negative
    kK(z)[k]=s;
    while(++p<n && g[h[p]]<0);
    k++;
  }
  cd(b);
  cd(c);
  R z;
}

I VAT(I i){R 1<=i && i<=4?i:0;} //vector atom type

K flip(K a)
{
  K x;I i,p=a->n,q=-1; 
  if(a->t || !p)R ci(a);//Identity on atoms/vectors && empty 0-list && 0-list of atoms
  DO(p, x=kK(a)[i]; if(x->t<1)q=x->n);
  if(-1==q)R ci(a);//Identity on 0-list of atoms
  DO(p, x=kK(a)[i]; if(x->t<1 && x->n!=q)R LE;)
  K z=newK(0,q);  //mmo
  for(i=0;i<q;i++)//This kind of thing is always tricky.
  {
    K* c=kK(a); K d=c[0];//Temporary variables
    I u,t=-ABS(d->t?d->t:VAT(u=kK(d)[i]->t)?u:0);//Starting type: vector's type or i-th-item-in-a-list's type
    DO2(p, d=c[j]; t=t==-ABS(d->t?d->t:VAT(u=kK(d)[i]->t)?u:0)?t:0 )//DO2: Get type. Flip won't pr0mote I to F
    K y=kK(z)[i]=newK(t,p); //oom
    if     (-4==t) DO2(p, d=c[j]; kS(y)[j]=d->t?kS(d)[i% d->n]:*kS(kK(d)[i]) )
    else if(-3==t) DO2(p, d=c[j]; kC(y)[j]=d->t?kC(d)[i% d->n]:*kC(kK(d)[i]) )
    else if(-2==t) DO2(p, d=c[j]; kF(y)[j]=d->t?kF(d)[i% d->n]:*kF(kK(d)[i]) )
    else if(-1==t) DO2(p, d=c[j]; kI(y)[j]=d->t?kI(d)[i% d->n]:*kI(kK(d)[i]) )
    else if( 0==t) DO2(p, d=c[j]; kK(y)[j]=itemAtIndex(d,i)) //mmo
  }
  R z;
}

K first(K a)
{ //Empty lists return prototypical atoms, e.g., *0#0.0 yields 0.0 
  I at=a->t, an=a->n;
  if(-4==at)R Ks(an?*kS(a):LS);
  if(-3==at)R Kc(an?*kC(a):' ');//Vectors
  if(-2==at)R Kf(an?*kF(a):0.0);
  if(-1==at)R Ki(an?*kI(a):0);
  if( 0==at)R an?ci(*kK(a)):_n();//Lists - *() yields _n
  R ci(a);//Atoms
}

K last(K a)
{ //Empty lists return prototypical atoms, e.g., *0#0.0 yields 0.0 
  I at=a->t, an=a->n;
  if(-4==at)R Ks(an?kS(a)[an-1]:LS);
  if(-3==at)R Kc(an?kC(a)[an-1]:' ');//Vectors
  if(-2==at)R Kf(an?kF(a)[an-1]:0.0);
  if(-1==at)R Ki(an?kI(a)[an-1]:0);
  if( 0==at)R an?ci(kK(a)[an-1]):_n();//Lists - *() yields _n
  R ci(a);//Atoms
}

Z K reshaper(K a, K b, I d, I f, I* p)
{ //a is non-empty int vector with: (0 0s, 0 -1s),(1 -1),or(1+ 0s)
  I bt=b->t, bn=b->n;
  I v=kI(a)[d];
  I g=!v||a->n==d+1?1:0;//hit bottom?
  I t= (g && bt<5)?-ABS(bt):0;// 2 3 4 0 #/: (_n;{x}) 
  I n=-1==v?f:v;//f is missing factor
  K z=newK(t,n); U(z)
  if(!g)DO(n,kK(z)[i]=reshaper(a,b,d+1,f,p))
  else if(4==ABS(bt))DO(n,kS(z)[i]=bn?kS(b)[++*p%bn]:LS)
  else if(3==ABS(bt))DO(n,kC(z)[i]=bn?kC(b)[++*p%bn]:' ')
  else if(2==ABS(bt))DO(n,kF(z)[i]=bn?kF(b)[++*p%bn]:0.0)
  else if(1==ABS(bt))DO(n,kI(z)[i]=bn?kI(b)[++*p%bn]:0)
  else if(0==ABS(bt))DO(n,kK(z)[i]=bn?ci(kK(b)[++*p%bn]):_n())
  else if(5<=    bt )DO(n,kK(z)[i]=ci(b))
  R z;
}

K reshape(K a, K b)
{
  I an=a->n, bn=b->n;
  if(!an)R first(b);//sic
  I ns=0,x,y=-1;
  DO(an, if(0>(x=kI(a)[i])){ns-=x;})//If any <0, only one -1
  P(ns < -1,DOE)
  I p=1; DO(an, p*=kI(a)[i])//Product over
  P(ns<0 && (!p || !bn || bn%p),LE)
  R reshaper(a,b,0,p?ABS(bn/p):0,&y);
}

K take(K a, K b)
{
 I bt=b->t, bn=b->n;
 I n=ABS(*kI(a)), m=MAX(1,bn), k= *kI(a) % m;
 k=k<0?bn+k:0;
 I t=bt<5?-ABS(bt):0;
 K z=newK(t,n);U(z)
 if     (4==ABS(bt))DO(n,kS(z)[i]=bn?kS(b)[(i+k)%m]:LS) //sp("")
 else if(3==ABS(bt))DO(n,kC(z)[i]=bn?kC(b)[(i+k)%m]:' ')
 else if(2==ABS(bt))DO(n,kF(z)[i]=bn?kF(b)[(i+k)%m]:0.0)
 else if(1==ABS(bt))DO(n,kI(z)[i]=bn?kI(b)[(i+k)%m]:0)
 else if(0==    bt )DO(n,kK(z)[i]=bn?ci(kK(b)[(i+k)%m]):_n()) 
 else if(5<=    bt )DO(n,kK(z)[i]=ci(b))
 R demote(z);
}

K take_reshape(K a, K b)
{ //K3.2 will accept empty lists that aren't type -1 (as left arg)
  P(a->n && 1!=ABS(a->t),IE)
  R 0<a->t?take(a,b):reshape(a,b);
}

Z void shapeCheck(K a, K p, I d)
{ //Descend through list a marking shape p as -1 where it doesn't correspond
  I at=a->t, an=a->n;
  if(at>0 || an!=kI(p)[d]) kI(p)[d]=-1;//Mismatch or atom means p length too long
  else if(at && d < p->n-1) kI(p)[d+1]=-1;//Another case of p being too long 
  else if(!at && an && kI(p)[d]!=-1 && d < p->n-1) DO(an, shapeCheck(kK(a)[i],p,d+1))
}
Z I firstDepth(K x){R (!x->t&&x->n)?1+firstDepth(*kK(x)):x->t>0?0:1;}//[Internal Function]

K shape(K a) //TODO: Thoroughly test this //TODO: oom
{ 
  K b=a, p=newK(-1, firstDepth(a));//Putative list. Mutable, Thrown away
  DO(p->n, kI(p)[i]=b->n; if(i<_i-1)b=*kK(b) )//Construct best-case shape
  shapeCheck(a,p,0);//Punch holes (-1) in shape-list where it fails
  I n=0; DO(p->n, if(kI(p)[i]==-1)break; n++)//See how far it made it
  K z=newK(-1,n);
  DO(n, kI(z)[i]=kI(p)[i])//Copy the good part. 
  cd(p);
  R z;//could instead shrink p into z
}

K rotate(K a, K b)
{
  I bt=b->t, bn=b->n;//Know 1==a->t and 0>=b->t
  I r=*kI(a) % MAX(1,bn);// x%0 -> division error
  r=r>0?r:bn+r;//Ensure mathematical definition of modulo
  K z=newK(bt,bn);U(z)
  if     (-4==bt)DO(bn, kS(z)[i]=kS(b)[(i+r)%bn])
  else if(-3==bt)DO(bn, kC(z)[i]=kC(b)[(i+r)%bn])
  else if(-2==bt)DO(bn, kF(z)[i]=kF(b)[(i+r)%bn])
  else if(-1==bt)DO(bn, kI(z)[i]=kI(b)[(i+r)%bn])
  else if( 0==bt)DO(bn, kK(z)[i]=ci(kK(b)[(i+r)%bn]))
  R z; 
}

K drop(K a, K b)
{
  I at=a->t, bt=b->t, bn=b->n;

  P(1!=at,IE)
  if(bt>0)R ci(b);//Drop always identity on atoms
  
  I v=*kI(a);
  I zn=MAX(0,bn-ABS(v));
  K z=newK(bt,zn);
  U(z)
  I c=v<1?0:MIN(v,bn);

  if     (-4==bt)DO(zn,kS(z)[i]=kS(b)[i+c])
  else if(-3==bt)DO(zn,kC(z)[i]=kC(b)[i+c])
  else if(-2==bt)DO(zn,kF(z)[i]=kF(b)[i+c])
  else if(-1==bt)DO(zn,kI(z)[i]=kI(b)[i+c])
  else if( 0==bt)DO(zn,kK(z)[i]=ci(kK(b)[i+c]))

  R demote(z);
}

K cut(K a, K b)
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  P(-1!=at,IE)

  DO(an, I x=kI(a)[i]; if(x<0|| x < kI(a)[i>0?i-1:0])R DOE; else if(x > bn)R LE )
  K z=newK(0,an);
  U(z)
  I zn=z->n;

  //TODO: oom
  #define FCUT I x=kI(a)[i],y=(i==z->n-1)?bn:kI(a)[i+1];
  if     (-4==bt) DO(zn, FCUT; K w=newK(-4,y-x); DO2(w->n,kS(w)[j]=kS(b)[x+j]); kK(z)[i]=w)
  else if(-3==bt) DO(zn, FCUT; K w=newK(-3,y-x); DO2(w->n,kC(w)[j]=kC(b)[x+j]); kK(z)[i]=w)
  else if(-2==bt) DO(zn, FCUT; K w=newK(-2,y-x); DO2(w->n,kF(w)[j]=kF(b)[x+j]); kK(z)[i]=w)
  else if(-1==bt) DO(zn, FCUT; K w=newK(-1,y-x); DO2(w->n,kI(w)[j]=kI(b)[x+j]); kK(z)[i]=w)
  else if( 0==bt) //Have to check if we have subsequences of similarly-typed atoms
  { I i;
    for(i=0;i<zn;i++)
    { FCUT;
      I sn=y-x;//Size of sublist
      I t=bt;//Empty sublists in z will inherit b's type
      if(sn && x < bn)t=kK(b)[x]->t;//Non-empty sublist, valid index?
      DO2(sn, if(t!=kK(b)[x+j]->t){t=0;break;})//Check for consistency
      t=-MAX(0,t); //Atom sequences become vectors, but nothing special for vectors
      K s=newK(t,sn);//Sublist for z
      if     (-4==t)DO2(sn,kS(s)[j]=*kS(kK(b)[x+j]))
      else if(-3==t)DO2(sn,kC(s)[j]=*kC(kK(b)[x+j]))
      else if(-2==t)DO2(sn,kF(s)[j]=*kF(kK(b)[x+j]))
      else if(-1==t)DO2(sn,kI(s)[j]=*kI(kK(b)[x+j]))
      else if( 0==t)DO2(sn,kK(s)[j]=ci(kK(b)[x+j]))
      kK(z)[i]=s;
    }
  }
  R z;
}

K drop_cut(K a, K b)
{
  if(1 != ABS(a->t) || (-1==a->t && 0<b->t))R IE;
  R 1==a->t?drop(a,b):cut(a,b);
}

K where(K x)
{
  P(!xn,newK(-1,0))
  P(1!=ABS(xt),IE)
  I zn=0,y,j,t=0;
  //DO(xn,if((y=kI(x)[i])<0)R DOE;zn+=y)
  DO(xn,if((y=kI(x)[i])<0)continue;zn+=y)//skip negatives instead of error
  K z=newK(-1,zn); U(z)
  DO(xn, for(j=0;j<kI(x)[i];j++)kI(z)[t++]=i)//Handles a-> == +-1 
  R z;
}

#define KSWAP(t,a) {t _t=a(z)[i];a(z)[i]=a(z)[an-i-1];a(z)[an-i-1]=_t;}
//TODO: The smarter way to do this is to write it in such a way that it can return the same input (e.g., if refcount == 1?, then use a temp holder and do the swaps in pairs)
K reverse(K a)
{
  I at=a->t,an=a->n;
  if(0<at)R ci(a);//Atoms
  K z=a;
  if (1==rc(a)){
    I n=an>>1;
    if     (-4==at) DO(n,KSWAP(S,kS)) //This could all be refactored
    else if(-3==at) DO(n,KSWAP(C,kC))
    else if(-2==at) DO(n,KSWAP(F,kF))
    else if(-1==at) DO(n,KSWAP(I,kI))
    else if( 0==at) DO(n,KSWAP(K,kK))
  }else{
    z=newK(at,an); U(z)
    if     (-4==at) DO(an,kS(z)[i]=kS(a)[an-i-1]) //This could all be refactored
    else if(-3==at) DO(an,kC(z)[i]=kC(a)[an-i-1])
    else if(-2==at) DO(an,kF(z)[i]=kF(a)[an-i-1])
    else if(-1==at) DO(an,kI(z)[i]=kI(a)[an-i-1])
    else if( 0==at) DO(an,kK(z)[i]=ci(kK(a)[an-i-1]))
  }
  R z;
}

I countI(K x){R xt>0?1:xn;}
K count(K x){R Ki(countI(x));}   //[sic] Should always be 1 for an atom (t of 5,7 may have different n)

K joinI(K*a, K y) {      //TODO: 5,6?
  K x=*a;
  I xk=countI(x), yk=countI(y), zt=0;
  if(ABS(xt)==ABS(yt)) zt=-ABS(xt);  //K-Improvement?: ABS(at)=1or2 && ABS(bt)==1or2 should yield zt==-2
  if(!xk) zt=-ABS(yt); 
  else if(!yk) zt=-ABS(xt);  //'else' is sic. In "K3.21 2006-02-01" right empty list takes precedence
  if(zt < -4) zt=0;

  if(1==rc(x)&&zt&&zt==xt)R ci(kapn(a,kV(y),yk));

  I zn=xk+yk;
  K z=newK(zt,zn);U(z)

#define JOIN(ta,t)	\
  memcpy(ta(z)   ,ta(x),xk*sizeof(t)); \
  memcpy(ta(z)+xk,ta(y),yk*sizeof(t))
  if     (-4==zt) {JOIN(kS,S);}
  else if(-3==zt) {JOIN(kC,C);}
  else if(-2==zt) {JOIN(kF,F);}
  else if(-1==zt) {JOIN(kI,I);}
#undef JOIN
  else if( 0==zt) {    //oom all here
    K c=promote(x); K d=promote(y);
    DO(xk,kK(z)[i]=ci(kK(c)[i]))
    DO(yk,kK(z)[xk+i]=ci(kK(d)[i]))
    cd(c); cd(d); }
  R z; }

K join(K x,K y){R joinI(&x,y);}

Z I _hg(K h,uI k,I v,K x,uI*p)
{
  I n=h->n;I*d=kI(h),i;uI u=k&(n-1);
  while(-1!=(i=d[u])){
    if(v==kI(x)[i]){*p=u;R i;}
    if(++u==n)u=0;
  }
  *p=u;R xn;
}

Z I _hgk(K h,uI k,K v,K x,uI*p)
{
  I n=h->n;I*d=kI(h),i;uI u=k&(n-1);
  while(-1!=(i=d[u])){
    if(KEQ(v,kK(x)[i])){*p=u;R i;}
    if(++u==n)u=0;
  }
  *p=u;R xn;
}

Z I _hgv(K h,uI k,V v,K x,uI*p)
{
  I n=h->n;I*d=kI(h),i;uI u=k&(n-1);
  while(-1!=(i=d[u])){
    if(v==kV(x)[i]){*p=u;R i;}
    if(++u==n)u=0;
  }
  *p=u;R xn;
}

K _hash(K x)
{
  P(xt>0,RE)
  uI p;K y=(-3==xt)?newK(-1,1+UCHAR_MAX):newH(xn);M(y);
  hcinit();
  DO(yn,kI(y)[i]=-1);
  SW(-xt){
  CS(0,DO(xn,K v=kK(x)[i];if(xn==_hgk(y,hcode(v),v,x,&p))hs(y,p,i)))
  CSR(1,)CS(2,DO(xn,uI v=kU(x)[i];if(xn==_hg(y,hc(v),(I)v,x,&p))hs(y,p,i)))
  CS(3,DO(xn,uI k=(UC)kC(x)[i];if(xn==kI(y)[k])kI(y)[k]=i))
  CS(4,setS(1,0);DO(xn,S v=kS(x)[i];if(!SV(v,1))SV(v,1)=fnv1a((UC*)v,strlen(v));if(xn==_hgv(y,SV(v,1),v,x,&p))hs(y,p,i)))}
  y->t=-5; R y;
}

K hash_find(K a,K b)
{
  K x=kK(a)[0],y=kK(a)[1];uI k,p;I i;
  P(xt>0,DOE)
  if(xt&&(xt+b->t))R Ki(xn);
  hcinit();
  SW(-xt){
  CS(0,i=_hgk(y,hcode(b),b,x,&p))
  CSR(1,)CS(2,{uI v=*kU(b);i=_hg(y,hc(v),(I)v,x,&p);})
  CS(3,k=(UC)*kC(b);i=kI(y)[k];if(i<0)i=xn)
  CS(4,{S v=*kS(b);k=fnv1a((UC*)v,strlen(v));i=_hgv(y,k,v,x,&p);}) }
  R Ki(i);
}
