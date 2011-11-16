/* grading / sorting */

#include "incs.h"

#include "k.h"
#include "kg.h"
#include "km.h"

Z I mergerComparer(K a, I r, I i, I j);

I FC(F a, F b)//Floating-Point Compare
{
  F E=0.00000000000000000001; //This value seems to work, might should be a different one though

  if(isinf(a)&&isinf(b)) R 0;
  if(ABS(a-b) <= E*MAX(ABS(a),ABS(b)))R 0;
  R a<b?-1:1;
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
  else   DO(b-1,c[_i-i-1]+=c[_i-i])//1==r: grade down
  DO(n, kI(s)[-1+c[kI(a)[n-i-1]-u]--]=n-i-1)
cleanup:
  cd(d);
  R s;
}
K charGrade(K a, I r)
{//Variation on Knuth Algorithm 5.2D Distribution counting
  I n=a->n, b=1+UCHAR_MAX, *c;
  K d=newK(-1,b);U(d)
  c=kI(d); //assumes # slots are set to 0
  K s=newK(-1,n);  
  DO(n,c[(UC)kC(a)[i]]++)
  if(!r) DO(b-1,c[i+1]+=c[i])      //0==r: grade up
  else DO(b-1,c[_i-i-2]+=c[_i-i-1])//1==r: grade down
  DO(n, kI(s)[-1+c[(UC)kC(a)[n-i-1]]--]=n-i-1)
  cd(d);
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
  for(i=s;i<=t;i++)d[i]=c[i];
  i=s;j=m+1;k=s;
  while(i<=m && j<=t)
   if(mergerComparer(a,r,d[i],d[j]))c[k++]=d[i++];
   else c[k++]=d[j++];
  while(i<=m)c[k++]=d[i++]; 
}
Z void doMergeGrade(K a, I r, K x, K y, I s, I t)
{
  if(s >= t) R; //Faster: another sort when small |t-s| 
  I m=s+(t-s)/2; //sic
  doMergeGrade(a,r,x,y,s,m);
  doMergeGrade(a,r,x,y,m+1,t);
  merger(a,r,x,y,s,t,m);
}
K mergeGrade(K a, I r)
{
  I n=a->n;
  K x=newK(-1,n);//Indices
  K y=newK(-1,n);//Temporary storage
  M(x,y)
  DO(n, kI(x)[i]=i)
  doMergeGrade(a,r,x,y,0,n-1);
  cd(y);
  R x;
}
