#include "incs.h"
#include "k.h"
#include "kg.h"
#include "km.h"
#include "vc.h"
#include "vg.h"

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

Z K hashRange(K x)
{
  I j=0,sa=0,h0=0;uI m=0;
  K h=newH(xn);M(h);
  K z=newK(xt,xn);M(h,z);
  DO(xn,m|=kU(x)[i]);if(m)while(!(1&m)){m>>=1;sa++;}
  DO(xn,uI p;uI u;uI v=kU(x)[i];
      if(!v){if(!h0){h0=1;kI(z)[j++]=0;}}
      else{u=v>>sa;u^=(u>>32);
      if(!hg(h,u,v,&p)){hs(h,p,v);kI(z)[j++]=v;}})
  //O("u:%lld xn:%lld\n",u,xn);
  if(xn==j)GC;
  K y=newK(xt,j);if(!y)GC;
  memcpy(kI(y),kI(z),j*sizeof(I));
  cd(z);z=y;
cleanup:
  cd(h);
  R z;
}

Z K newSH(I n){ I m=1<<(HFR+cl2(n));K sh=newK(-1,m);M(sh);R sh; }
Z S shg(K sh,uI hk,S k,uI*p)
{
  I n=sh->n;S*d=kS(sh);uI u=hk&(n-1);
  while(d[u]){
    if(!SC(k,d[u])){*p=u;R k;}
    if(++u==n)u=0;
  }*p=u;R 0;
}
#define shs(sh,p,k) kS(sh)[p]=(k)

uint32_t fnv1a(UC *x,I n)//Fowler-Noll-Vo FNV-1a hash
{
  uint32_t h=2166136261UL;
  DO(n,h^=x[i];h*=16777619UL)R h;
}

Z K strRange(K x)
{
  I j=0;
  DO(xn,if(-3!=kK(x)[i]->t)R 0)
  K sh=newSH(xn);M(sh);
  K z=newK(xt,xn);M(sh,z);
  DO(xn,uI p;K kv=kK(x)[i];S v=kC(kv);
     if(!shg(sh,fnv1a((UC*)v,kv->n),v,&p)){shs(sh,p,v);kK(z)[j++]=ci(kv);})
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
  CSR(1,)
  CSR(2,R hashRange(a))
  CSR(3,R charRange(a))
  CSR(4,R symRange(a)) }

  if((z=strRange(a)))R z;
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
  I h[1+UCHAR_MAX],c[1+UCHAR_MAX],j=0;
  memset(h,0,(1+UCHAR_MAX)*sizeof(I));
  memset(c,0,(1+UCHAR_MAX)*sizeof(I));
  DO(xn,UC u=(UC)kC(x)[i];if(!h[u])h[u]=++j;I w=h[u]-1;c[w]++;)
  K y=newK(0,j);M(y);
  DO(j,K z=newK(-1,c[i]);M(z,y);kK(y)[i]=z;z->n=0)
  DO(xn,UC u=(UC)kC(x)[i];I w=h[u]-1;K z=kK(y)[w];kI(z)[z->n++]=i)
  R y;
}

Z K symGroup(K x)
{
  I j=0;
  K uk=newK(-1,xn);M(uk);I*u=kI(uk);
  setS(1,0);setS(2,0);
  DO(xn,S s=kS(x)[i];if(!SV(s,2)){u[j]=(I)s;SV(s,2)=++j;}SV(s,1)++)
  K y=newK(0,j);M(y,uk);
  DO(j,K z=newK(-1,SV((S)u[i],1));M(z,y,uk);z->n=0;kK(y)[i]=z)
  DO(xn,S s=kS(x)[i];I w=SV(s,2)-1;K z=kK(y)[w];kI(z)[z->n++]=i)
  cd(uk);
  R y;
}

K group(K x)
{
  I t=xt, n=xn;
  P(t>0,RE)
  I u=n,*g,*h;
  K z=0,b=0,c=0;

  if(-4==t)R symGroup(x);
  if(-3==t)R charGroup(x);
  
  M(b=grade_up(x));g=kI(b);
  //Nastier code would eliminate this second sort.
  c=newK(-1,n);M(b,c);h=kI(c);
  DO(n,h[g[i]]=i);
  //Step through, on duplicate set uniques-=1, mark by inverting sign of corresponding index
  //I *h=kI(c);
  if(-2==t)DO(n-1,if(kF(x)[g[n-i-1]]==kF(x)[g[n-i-2]])  {--u;g[n-i-1]*=-1;})
  if(-1==t)DO(n-1,if(kI(x)[g[n-i-1]]==kI(x)[g[n-i-2]])  {--u;g[n-i-1]*=-1;})
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

//TODO: The smarter way to do this is to write it in such a way that it can return the same input (e.g., if refcount == 1?, then use a temp holder and do the swaps in pairs)
K reverse(K a)
{
  I at=a->t,an=a->n;
  if(0<at)R ci(a);//Atoms
  K z=newK(at,an); U(z)
  if     (-4==at) DO(an,kS(z)[i]=kS(a)[an-i-1]) //This could all be refactored
  else if(-3==at) DO(an,kC(z)[i]=kC(a)[an-i-1])
  else if(-2==at) DO(an,kF(z)[i]=kF(a)[an-i-1])
  else if(-1==at) DO(an,kI(z)[i]=kI(a)[an-i-1])
  else if( 0==at) DO(an,kK(z)[i]=ci(kK(a)[an-i-1]))
  R z;
}

I countI(K x){R xt>0?1:xn;}
K count(K x){R Ki(countI(x));}   //[sic] Should always be 1 for an atom (t of 5,7 may have different n)

K join(K x, K y) {      //TODO: 5,6?
  I xk=countI(x), yk=countI(y), zt=0;
  if(ABS(xt)==ABS(yt)) zt=-ABS(xt);  //K-Improvement?: ABS(at)=1or2 && ABS(bt)==1or2 should yield zt==-2
  if(!xk) zt=-ABS(yt); 
  else if(!yk) zt=-ABS(xt);  //'else' is sic. In "K3.21 2006-02-01" right empty list takes precedence
  if(zt < -4) zt=0;
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
    cd(c); cd(d);  }
  R z; }
