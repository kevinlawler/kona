/* grading / sorting */

#include "incs.h"

#include "k.h"
#include "kg.h"
#include "km.h"

Z I mergerComparer(K a, I r, I i, I j);

#define BITS_EM   0x7fffffffffffffffULL
#define BITS_0i   0x7ff0000000000000ULL
#define BITS_SUBN 0x0010000000000000ULL
#define Inan(x)   (BITS_0i<(BITS_EM&(x)))
#define Isubn(x)  (BITS_SUBN>(BITS_EM&(x)))

I FC(F a, F b)//Floating-Point Compare
{
#if 0
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
  
  if(at<bt)R -1;
  if(at>bt)R 1;
  if(an<bn) R -1;
  if(an>bn) R 1;

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

K distributionGrade(K a, I r, I u, I v)//u,v: precomputed min,max
{//Variation on Knuth Algorithm 5.2D Distribution counting
  I n=a->n, b=v-u+1, *c;
  K d=newK(-1,b);U(d) 
  c=kI(d); //assumes # slots are set to 0
  K s=newK(-1,n);  
  if(!s)GC;
  DO(n,c[kI(a)[i]-u]++)
  if(!r) DO(b-1,c[i+1]+=c[i])      //0==r: grade up
  else   DO(b-1,c[_i-i-1]+=c[_i-i-0])//1==r: grade down
  DO(n, kI(s)[-1+c[kI(a)[n-i-1]-u]--]=n-i-1)
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
#define INS_GRADE_THRESHOLD 7
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
Z void radixGrade(K a,I r,K x,K y,I s,I t,I d)
{
  // O("radixGrade");
  C *w=(C*)kI(a);I n=t-s+1,*u=kI(x),*v=kI(y);
  I c[2+UCHAR_MAX],k;
  memset(c,0,(2+UCHAR_MAX)*sizeof(I));c[1+UCHAR_MAX]=n;
  DO(n,c[128+w[(u[s+i]<<3)+d]]++);
  if(!r) DO(UCHAR_MAX,c[i+1]+=c[i])
  else   DO(UCHAR_MAX,c[_i-i-1]+=c[_i-i-0]);
  // DO(2+UCHAR_MAX,O("%lld ",c[i]));O("\n");
  DO(n, k=u[t-i];v[-1+c[128+w[(k<<3)+d]]--]=k)
  // DO(2+UCHAR_MAX,O("%lld ",c[i]));O("\n");
  if(d){ I i=0;
    for(i=0;c[i]<n&&i<1+UCHAR_MAX;i++){
      I n=c[i+1]-c[i];
      if(!n)continue;
      if(1==n)u[c[i]]=v[c[i]];
      else if(n<INS_GRADE_THRESHOLD){
        insertGrade(a,r,y,x,c[i],c[i+1]-1);
        memcpy(u+c[i],v+c[i],n*sizeof(I));
      }
      else radixGrade(a,r,y,x,c[i],c[i+1]-1,d-1);
    }
  }
}
Z void doMergeGrade(K a, I r, K x, K y, I s, I t)
{
  if(s >= t) R; //Faster: another sort when small |t-s| 
  I m=s+(t-s)/2; //sic
  if(m-s<INS_GRADE_THRESHOLD)insertGrade(a,r,x,y,s,m);
  else doMergeGrade(a,r,x,y,s,m);
  if(t-(m+1)<INS_GRADE_THRESHOLD)insertGrade(a,r,x,y,m+1,t);
  else doMergeGrade(a,r,x,y,m+1,t);
  merger(a,r,x,y,s,t,m);
}
K mergeGrade(K a, I r)
{
  I n=a->n;
  K x=newK(-1,n);//Indices
  K y=newK(-1,n);//Temporary storage
  M(x,y)
  DO(n, kI(x)[i]=i)
  //if(-1==a->t)radixGrade(a,r,x,y,0,n-1,7);
  //else
  doMergeGrade(a,r,x,y,0,n-1);
  cd(y);
  R x;
}
