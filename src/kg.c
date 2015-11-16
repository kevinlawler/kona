/* grading / sorting */

#include "incs.h"

#include "k.h"
#include "kg.h"
#include "km.h"

Z I gt=0;
Z I mergerComparer(K a, I r, I i, I j);

#define BITS_EM   0x7fffffffffffffffULL
#define BITS_0i   0x7ff0000000000000ULL
#define BITS_SUBN 0x0010000000000000ULL
#define Inan(x)   (BITS_0i<(BITS_EM&(x)))
#define Isubn(x)  (BITS_SUBN>(BITS_EM&(x)))

I FC(F a, F b)//Floating-Point Compare
{
#ifdef REFERENCE_FC
  F E=0.00000000000000000001; //This value seems to work, might should be a different one though

  if(isnan(a))R isnan(b)?0:-1;
  if(isnan(b))R isnan(a)?0: 1;
  if(isinf(a)) {
    if (isinf(b)) {
      R (a<0 && b<0)?0:(a>0 && b>0)?0:(a<0 && b>0)?-1:1;
    }
    R a<0?-1:1;
  }
  else if (isinf(b)) {
    R b>0?-1:1;
  }

  if(ABS(a-b) <= E*MAX(ABS(a),ABS(b)))R 0;
  R a<b?-1:1;
#else
  {
    // adaptive ULP
    union {I i;F f;} x,y;I xu,d,ad;
    x.f=a;y.f=b;xu=x.i|y.i;
    if(Inan(x.i))R Inan(y.i)?0:-1;
    else if(x.i<0)x.i=LLONG_MIN-x.i;
    if(Inan(y.i))R 1;
    else if(y.i<0)y.i=LLONG_MIN-y.i;
    ad=llabs(d=x.i-y.i);
    if(ad<1023){
      if(Isubn(xu))R d<0?-1:d?1:0;
      if(ad<513)R 0;
      // sxxx xxxx xxxx uuuu uuuu ....
      xu=513+((255&(xu>>44))<<1);
      if(ad<xu)R 0;
    }
    R d<0?-1:1;
  }
#endif
}

I KC(K a, K b)//List Compare (K Compare)
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  I A=ABS(at);
  
  // K3.2: <("aaa";"bb";,"c";"d") is 0 1 2 3
  if(3!=A){
    if(at<bt)R -1;
    if(at>bt)R 1;
    if(an<bn) R -1;
    if(an>bn) R 1;
  }

  I u,v;C c,d;
  if     (7==A)R 0;//TODO: sort functions?
  else if(6==A)R 0;
  else if(5==A)R 0;//TODO: sort dictionaries?
  else if(4==A)DO(an, u=SC(kS(a)[i],kS(b)[i]); if(u) R u)
  else if(3==A)DO(an, c=kC(a)[i]; d=kC(b)[i]; if(c<d)R -1; if(c>d) R 1)
  else if(2==A)DO(an, u=FC(kF(a)[i],kF(b)[i]); if(u)R u)
  else if(1==A)DO(an, u=kI(a)[i]; v=kI(b)[i]; if(u<v)R -1; if(u>v) R 1)  
  else if(0==A)DO(an, u=KC(kK(a)[i],kK(b)[i]); if(u) R u)
  R 0;
}

K distributionGrade(K a, I r, uI u, uI v)//u,v: precomputed min,max
{//Variation on Knuth Algorithm 5.2D Distribution counting
  if(gt)O("distributionGrade");
  I n=a->n, b=v-u+1, *c;
  K d=newK(-1,b);U(d) 
  c=kI(d); //assumes # slots are set to 0
  K s=newK(-1,n);  
  if(!s)GC;
  DO(n,c[kU(a)[i]-u]++)
  if(!r) DO(b-1,c[i+1]+=c[i])      //0==r: grade up
  else   DO(b-1,c[_i-i-1]+=c[_i-i-0])//1==r: grade down
  DO(n, kI(s)[-1+c[kU(a)[n-i-1]-u]--]=n-i-1)
cleanup:
  cd(d);
  R s;
}
K charGrade(K a, I r)
{//Variation on Knuth Algorithm 5.2D Distribution counting
  I n=a->n,c[1+UCHAR_MAX]; //assumes # slots are set to 0
  memset(c,0,(1+UCHAR_MAX)*sizeof(I));
  K s=newK(-1,n);  
  DO(n,c[(UC)kC(a)[i]]++)
  if(!r) DO(UCHAR_MAX,c[i+1]+=c[i])      //0==r: grade up
  //else DO(UCHAR_MAX,c[_i-i-2]+=c[_i-i-1])//1==r: grade down
  else DO(UCHAR_MAX,c[_i-i-1]+=c[_i-i-0])//1==r: grade down
  DO(n, kI(s)[-1+c[(UC)kC(a)[n-i-1]]--]=n-i-1)
  R s;
}
Z I mergerComparer(K a, I r, I i, I j)//Could unroll this
{
  I t=a->t; 
  //-3 has its own sort, won't be merged
  if     (-4==t && 0==r &&  1>SC(kS(a)[i],kS(a)[j])) R 1;
  else if(-4==t && 1==r && -1<SC(kS(a)[i],kS(a)[j])) R 1;
  else if(-2==t && 0==r &&  1>FC(kF(a)[i],kF(a)[j])) R 1;
  else if(-2==t && 1==r && -1<FC(kF(a)[i],kF(a)[j])) R 1;
  else if(-1==t && 0==r &&    kI(a)[i] <= kI(a)[j] ) R 1;
  else if(-1==t && 1==r &&    kI(a)[i] >= kI(a)[j] ) R 1;
  else if( 0==t && 0==r &&  1>KC(kK(a)[i],kK(a)[j])) R 1; 
  else if( 0==t && 1==r && -1<KC(kK(a)[i],kK(a)[j])) R 1; 
  R 0;
}
Z void merger(K a, I r, K x, K y, I s, I t, I m)
{
  I i,j,k;
  I *c=kI(x),*d=kI(y);
  // for(i=s;i<=t;i++)d[i]=c[i];
  memcpy(d+s,c+s,(t-s+1)*sizeof(I));
  i=s;j=m+1;k=s;
  while(i<=m && j<=t)
   if(mergerComparer(a,r,d[i],d[j]))c[k++]=d[i++];
   else c[k++]=d[j++];
  //while(i<=m)c[k++]=d[i++];
  if(i<=m)
    memcpy(c+k,d+i,(m-i+1)*sizeof(I));
}
Z void insertGrade(K a,I r,K x,K y,I s,I t)
{
  I i,*c=kI(x);
  for(i=s+1;i<=t;i++){
    I x=c[i],j=i;
    while(s<j&&!mergerComparer(a,r,c[j-1],x)){
      c[j]=c[j-1]; j--;
    }
    c[j]=x;
  }
}
Z void doMergeGrade(K a, I r, K x, K y, I s, I t)
{
  if(s >= t) R; //Faster: another sort when small |t-s| 
  I m=s+(t-s)/2; //sic
  if(m-s<IGT)insertGrade(a,r,x,y,s,m);
  else doMergeGrade(a,r,x,y,s,m);
  if(t-(m+1)<IGT)insertGrade(a,r,x,y,m+1,t);
  else doMergeGrade(a,r,x,y,m+1,t);
  merger(a,r,x,y,s,t,m);
}
Z uI StoU(S s,I n){ uI h=0;DO(8,h<<=8;if(i<n)h+=(UC)s[i])R h; }
Z K strGrade(K a,I r)
{
  uI h=0;I k,s=1;K z=0,x=newK(-1,a->n);M(x);
  DO(xn,K y=kK(a)[i];if(3!=ABS(yt)||yn>8){s=0;break;}kU(x)[i]=(k=StoU(kC(y),yn));h|=k)
  if(s)z=radixGrade(x,r,h);
  cd(x);R z;
}
K mergeGrade(K a, I r)
{
  K x=0,y=0;I n=a->n;
  if(gt)O("mergeGrade");
  if(0==a->t){
    if(x=strGrade(a,r))R x; 
  }
  x=newK(-1,n);//Indices
  y=newK(-1,n);//Temporary storage
  M(x,y)
  DO(n, kI(x)[i]=i)
  doMergeGrade(a,r,x,y,0,n-1);
  cd(y);
  R x;
}
K insertGradeU(K a,I r)
{
  if(gt)O("insertGrade");
  uI *u=kU(a);
  I n=a->n,i,*c;
  K x=newK(-1,n);//Indices
  M(x)
  DO(n, kI(x)[i]=i)
  c=kI(x);

  if(!r)
    for(i=1;i<=n-1;i++){
      I k=c[i],j=i;
      while(0<j&&(u[c[j-1]]>u[k])){
        c[j]=c[j-1]; j--;
      }
      c[j]=k; }
  else
    for(i=1;i<=n-1;i++){
      I k=c[i],j=i;
      while(0<j&&(u[c[j-1]]<u[k])){
        c[j]=c[j-1]; j--;
      }
      c[j]=k; }
  R x;
}
Z clock_t t0;
void trst(){t0=clock();}
void elapsed(S m){
  clock_t e=clock()-t0;
  I ms=(I)(1000.0*(double)e/CLOCKS_PER_SEC);
  O("%s %lld\n",m,ms);trst();}
#define N (65535)
Z void dGU(uI*a,I r,I*x,I*y,I n,I*c,I d)
{//Variation on Knuth Algorithm 5.2D Distribution counting
  I sa=16*d;
  DO(n,c[N&(a[i]>>sa)]++)
  // if(!r)
  DO(N,c[i+1]+=c[i])      //0==r: grade up
  // else   DO(N,c[_i-i-1]+=c[_i-i-0])//1==r: grade down
  DO(n,I k=x[n-i-1]; y[-1+c[N&(a[n-i-1]>>sa)]--]=k)
}
Z void radixGradeI(uI*a,uI*w,I r,I*u,I*v,I*c,I n,uI h)
{
  if(r){DO(n,a[i]=~a[i]);r=0;}
  // memcpy(w,a,n*sizeof(I));
  // trst();
  dGU(a,r,u,v,n,c,0);         //elapsed(" grade0"); // a,u => v
  DO(n,w[i]=a[v[i]]);         //elapsed(" order0"); // w:a@v
  memset(c,0,(1+N)*sizeof(I));//elapsed("memzero");
  dGU(w,r,v,u,n,c,1);         //elapsed(" grade1"); // w,v => u
  DO(n,w[i]=a[u[i]]);         //elapsed(" order1"); // w:a@u
  memset(c,0,(1+N)*sizeof(I));//elapsed("memzero");
  dGU(w,r,u,v,n,c,2);         //elapsed(" grade2"); // w,u => v
  DO(n,w[i]=a[v[i]]);         //elapsed(" order2"); // w:a@v
  memset(c,0,(1+N)*sizeof(I));//elapsed("memzero");
  dGU(w,r,v,u,n,c,3);         //elapsed(" grade3"); // w,v => u
}
K radixGrade(K a,I r,uI h)
{
  if(gt)O("radixGrade");
  I n=a->n;
  K x=newK(-1,n);//Indices
  K y=newK(-1,n);//Temporary storage
  K z=newK(-1,1+N);
  K w=newK(-1,n);
  M(x,y,z,w)
  DO(n, kI(x)[i]=i)
  radixGradeI(kU(a),kU(w),r,kI(x),kI(y),kI(z),n,h);
  cd(w);cd(z);cd(y);
  R x;
}
