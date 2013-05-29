/* memory management */

#if defined(__linux__)
#define _GNU_SOURCE 1
#endif

#include "incs.h"

#include "k.h"
#include "km.h"

//Notes on memory manager: seems like atoms (and small lists?) are not released
//by K4 (see Skelton's remark: only contiguous arrays greater than 32MB are
//returned to OS). Also: " Why do you think it is memory fragmentation? The
//allocator in kdb+ is designed specifically to avoid that by using fixed size
//buckets."
#define KP_MIN 5  //2^x, must be at least ceil(lg(sizeof(V)))
#define KP_MAX 25 //2^x, 25->32MB  //TODO: base on available memory at startup (fixed percent? is 32M/2G a good percent?)
V KP[sizeof(V)*8+1]; //KPOOL
I PG; //pagesize:  size_t page_size = (size_t) sysconf (_SC_PAGESIZE);

#if UINTPTR_MAX >= 0xffffffffffffffff //64 bit
#define MAX_OBJECT_LENGTH (((UI)1) << 45) //for catching obviously incorrect allocations
#else 
#define MAX_OBJECT_LENGTH (II - 1) //for catching obviously incorrect allocations
#endif
Z I cl2(I v);
Z I kexpander(K *p,I n);
Z K kapn_(K *a,V v,I n);
Z V amem(I k);
Z V kalloc(I k);
Z V unpool(I r);


I OOM_CD(I g, ...) //out-of-memory count-decrement 
{ va_list a; V v,o=(V)-1;
  va_start(a,g);while(o!=(v=va_arg(a,V)))if(!v)g=1; va_end(a);
  P(!g,1)//OK
  va_start(a,g);while(o!=(v=va_arg(a,V)))cd(v); va_end(a);
  R 0;
}
//Arthur says he doesn't use malloc or free. Andrei Moutchkine claims smallest unit is vm page (his truss says no malloc + add pages one at a time).
//Arthur not using malloc is probably true. No strdup & related functions in binary's strings. Note: Skelton references "different allocator" not in \w report
//This source would be improved by getting ridding of remaing malloc/calloc/realloc
K cd(K a)
{
  #ifdef DEBUG
  if(a && a->c <=0 ) { er(Tried to cd() already freed item) dd(tests) dd(a) dd(a->c) dd(a->t) dd(a->n) show(a); }
  #endif 
  if(!a || --a->c) R a;
  #ifdef DEBUG
  DO(kreci, if(a==krec[i]){krec[i]=0; break; })
  #endif 
  if(7==a->t){ DO(-2+TYPE_SEVEN_SIZE,cd(kV(a)[2+i]))} //-4 special trick: don't recurse on V members. assumes sizeof S==K==V.  (don't free CONTEXT or DEPTH)
  if(0==a->t || 5==a->t) DO(a->n, cd(kK(a)[a->n-i-1])) //repool in reverse, attempt to maintain order
  #ifdef DEBUG
  if(0)R 0; //for viewing K that have been over-freed
  #endif
  //assumes seven_type a->k is < PG
  I o=((size_t)a)&(PG-1);//file-mapped? 1:
  I k=sz(a->t,a->n), r=lsz(k);
  //assert file-maps have sizeof(V)==o and unpooled blocks never do (reasonable)
  if(sizeof(V)==o || r>KP_MAX)munmap(((V)a)-o,k+o); //(file-mapped or really big) do not go back into pool. 
  else repool(a,r);
  R 0;
}
K ci(K a){if(a)a->c++; R a;}

I bp(I t) {SW(ABS(t)){CSR(1, R sizeof(I)) CSR(2, R sizeof(F)) CSR(3, R sizeof(C)) CD: R sizeof(V); } } //Default 0/+-4/5/6/7  (assumes sizeof(K)==sizeof(S)==...)
I sz(I t,I n){R 3*sizeof(I)+(7==t?TYPE_SEVEN_SIZE:n)*bp(t)+(3==ABS(t));} //not recursive. assert sz() > 0:  Everything gets valid block for simplified munmap/(free)

Z I nearPG(I i){ I k=((size_t)i)&(PG-1);R k?i+PG-k:i;}//up 0,8,...,8,16,16,...

//This is an untested idea for avoiding all that goes on in backing out of memory allocations when an error occurs inside a function before everything is done:
//If you control the memory allocator one possibility is to work in "claimed" (sbreak) but "free" space and build the K data structure there.
//Doing ci() or something similar on it marks the space "used". on error you do nothing and the space remains "free" (mutex)

//Keyword "backingstore" in old k mailing list archives - extra KSWAP beyond regular swap space

K newK(I t, I n)
{ 
  K z;
  if(n>MAX_OBJECT_LENGTH)R ME;//coarse (ignores bytes per type). but sz can overflow
  I k=sz(t,n);
  U(z=kalloc(k))
  //^^ relies on MAP_ANON being zero-filled for 0==t || 5==t (cd() the half-complete), 3==ABS(t) kC(z)[n]=0 (+-3 types emulate c-string)
  z->c=1; z->t=t; z->n=n;
  #ifdef DEBUG
  krec[kreci++]=z;
  #endif
  R z;
}

Z V kalloc(I k) //bytes. assumes k>0
{
  I r=lsz(k);
  if(r>KP_MAX)R amem(k);// allocate for objects of sz > 2^KP_MAX
  R unpool(r);
}

Z V amem(I k){K z;if(MAP_FAILED==(z=mmap(0,k,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANON,-1,0)))R ME; R z;}
Z V unpool(I r)
{
  V*z;
  V*L=((V*)KP)+r;
  I k= ((I)1)<<r;
  if(!*L)
  {
    U(z=amem(k))
    if(k<PG)
    { 
      V y=z;
      while(y<(V)z+PG+-k){*(V*)y=y+k;y+=k;}
    }//Low lanes subdivide pages. no divide op
    *L=z;
  }
  z=*L;*L=*z;*z=0;
  R z;
}

Z I cl2(I v) //optimized 64-bit ceil(log_2(I)) 
{
    if(!v)R -1;// no bits set
    I e = 0;
    if(v & (v - 1ULL))e=1; //round up if not a power of two
    #if UINTPTR_MAX >= 0xffffffffffffffff
      if(v & 0xFFFFFFFF00000000ULL){e+=32;v>>=32;} //64-bit or more only
    #endif
    if(v & 0x00000000FFFF0000ULL){e+=16;v>>=16;}
    //short CL2_LUT[1<<16]; DO(1<<16,if(i) CL2_LUT[i]=log2(i));
    //to use lookup table: e+=CL2_LUT[v] and comment out below. 
    if(v & 0x000000000000FF00ULL){e+=8; v>>=8; }
    if(v & 0x00000000000000F0ULL){e+=4; v>>=4; }
    if(v & 0x000000000000000CULL){e+=2; v>>=2; }
    if(v & 0x0000000000000002ULL){e+=1; v>>=1; }
    R e;
}


I lsz(I k){R k<=((I)1)<<KP_MIN?KP_MIN:cl2(k);} //pool lane from size. Ignore everywhere lanes < KP_MIN. MAX() was eliminated as an optimization
I repool(V v,I r)//assert r < KP_MAX 
{
  memset(v,0,((I)1)<<r);
  *(V*)v=KP[r];
  KP[r]=v;
  R 0;
}
Z I kexpander(K*p,I n) //expand only. 
{
  K a=*p;
  V v; I c=sz(a->t,a->n),d=sz(a->t,n),e=nearPG(c),f=d-e;
  I r = lsz(c);
  if(r>KP_MAX) //Large anonymous mmapped structure - (simulate mremap)
  {
    if(f<=0) R 1;
#if defined(__linux__)
    V*w=mremap(a,c,d,MREMAP_MAYMOVE);
    if(MAP_FAILED!=w) {*p=w; R 1;}
#else  
    F m=f/(F)PG; I n=m, g=1; if(m>n) n++;
    DO(n, if(-1==msync(a+e+PG*i,1,MS_ASYNC)) {if(errno!=ENOMEM) {g=0; break;}}
          else {g=0; break;})
    if(g) if(MAP_FAILED!=mmap(a+e,f,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANON|MAP_FIXED,-1,0)) R 1; //Add pages to end
#endif
    U(v=amem(d))   memcpy(v,a,c); *p=v; munmap(a,c); R 1; //Couldn't add pages, copy to new space
  }
  //Standard pool object
  I s=lsz(d);
  if(r==s) R 1; //assert r<=s
  K x=unpool(s); U(x)
  memcpy(x,a,c);
  *p=x;
  repool(a,r);
  R 1;
}

Z K kapn_(K *a,V v,I n)
{
  if(!a||!n)R 0;
  K k=*a;
  I t=k->t,m=k->n,p=m+n;
  if(6==t)
  {
    K z=newK(0,p);U(z)
    K *zv=kK(z);
    *zv++=_n(); DO(n, zv[i]=_n());
    cd(k);
    *a=z;
    R z;
  }
  if(!kexpander(&k,p))R 0;
  if(k!=*a)
  {
    #ifdef DEBUG
    DO(kreci, if(*a==krec[i]){krec[i]=0; break; })
    #endif
    *a=k;
  }
  k->n=p;
  SW(ABS(t))
  {
    CSR(0,) CS(5, DO(n, kK(k)[i+m]=ci(((K*)v)[i])));
    CS(1, memcpy(kI(k)+m,v,n*sizeof(I)));
    CS(2, memcpy(kF(k)+m,v,n*sizeof(F)));
    CS(3, strncpy(kC(k)+m,(S)v,n); kC(k)[p]=0);
    CS(4, memcpy(kS(k)+m,v,n*sizeof(S)))
    CD:   R 0;
  }
  if(t>0&&t<5&&p>1)k->t*=-1;
  R *a;
}

extern K kapn(K *a,V v,I n){R kapn_(a,v,n);}

extern K kap(K*a,V v){R kapn_(a,v,1);}

N newN(){R unpool(lsz(sizeof(Node)));}
PDA newPDA(){PDA p=unpool(lsz(sizeof(Pda)));U(p) p->c=malloc(1); if(!p->c){ME;R 0;} R p;}
I push(PDA p, C c){R appender(&p->c,&p->n,&c,1);} 
C    peek(PDA p){I n=p->n; R n?p->c[n-1]:0;}
C     pop(PDA p){R p->n>0?p->c[--(p->n)]:0;}
C  bottom(PDA p){R p->n>0?p->c[0]:0;}
void pdafree(PDA p){free(p->c); repool(p,lsz(sizeof(PDA)));}

K Ki(I x){K z=newK(1,1);*kI(z)=x;R z;}
K Kf(F x){K z=newK(2,1);*kF(z)=x;R z;}
K Kc(C x){K z=newK(3,1);*kC(z)=x;R z;}
K Ks(S x){U(x) K z=newK(4,1);*kS(z)=x;R z;}//KDB+ >= 2.4 tries interning [sp()]  by default when generating sym atoms 
K Kd(   ){R   newK(5,0);}
K Kn(   ){R   newK(6,1);}//Should n instead be 0? (Won't affect #:) in k3.2 yes  //In K3.2 _n->n is overridden for error messages. 
K Kv(   ){K z=newK(7,TYPE_SEVEN_SIZE);U(z) z->n=1;kV(z)[CONTEXT]=__d; M(z,kV(z)[PARAMS]=Kd(),kV(z)[LOCALS]=Kd()) R z;} //z->n == 0-wd 1-wordfunc 2-cfunc 3-charfunc 4-:[] 5-if[] 6-while[] 7-do[]
//Optimization: It's better if Kv() doesn't set PARAMS and LOCALS. Only charfuncs should set params

K newEntry(S s){R newE(s,_n());}//assumes s came from sp()
K newE(S s, K k) //oom
{
  K z=newK(0,3); U(z)
  kK(z)[0]=Ks(s); // be careful -- s must have come from sp()
  kK(z)[1]=k;
  kK(z)[2]=_n();
  M(z,kK(z)[0],kK(z)[2]) //May want to redesign this function (& newEntry) to ci(k==kK(z)[1])
  R z;
}
I rp2(I v){v--;v|=v>>1;v|=v>>2;v|=v>>4;v|=v>>8;v|=v>>16;if(sizeof(V)>=8)v|=v>>32;v++;R MAX(1,v);}//round up to integer power of 2 (fails on upper 1/4 signed)
