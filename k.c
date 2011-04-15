//64-bit single-threaded implementation of K3.2.  Version is Kona 3.2.0
//todo abbreviations: mm/o = memory manage/optimize   lfop = localize for other platforms (eg needs ifdef changes)   oom = handle out-of-memory
#include "incs.h"

#include "k.h"
#include "r.h"
#include "kc.h"
#include "kex.h"
#include "kn.h"
#include "tests.h"
#include "v.h"
#include "va.h"
#include "vc.h"
#include "vd.h"
#include "vf.h"
#include "vg.h"
#include "vq.h"

//Notes on memory manager: seems like atoms (and small lists?) are not released
//by K4 (see Skelton's remark: only contiguous arrays greater than 32MB are
//returned to OS). Also: " Why do you think it is memory fragmentation? The
//allocator in kdb+ is designed specifically to avoid that by using fixed size
//buckets."
#define KP_MIN 5  //2^x, must be at least ceil(lg(sizeof(V)))
#define KP_MAX 25 //2^x, 25->32MB  //TODO: base on available memory at startup (fixed percent? is 32M/2G a good percent?)
V KP[sizeof(V)*8+1]; //KPOOL

C errmsg[256]; //TODO: pthread_getspecific (not __thread) thread-local storage (different for mac os x)
extern K kerr(cS s){ R snprintf(errmsg,256,"%s",s),(K)0;} 
I oerr(){R O("%s %s\n",errmsg,"error");}

N SYMBOLS;//immutable symbol interning
K KTREE;  //dictionary, the main/global variable storage area
I SEED;   //seed for PRNG
S __d;    //sym: handle of current K-Tree dictionary / mth: thread-local storage

S PORT;

K NIL;    //Useful to avoid actually allocating _n nils (use _n() instead)

S LS;     //special symbol for locals (repeated,invisble)
I PP=7;   //Print Precision Digits
I PPMAX=19;
C PPON=1;
I max(I a,I b){R a>b?a:b;}
I min(I a,I b){R a<b?a:b;}

K X(S s){R XN(s,strlen(s));}  
K XN(S s,I n){R ex(wd(s,n));} //asserts ex(x) has first-line U(x)
K KX(K x){R XN(CSK(x),xn);}  //assumes 3==ABS(xt)

//TODO: open() can set errno, everywhere
//TODO: central place for setting errno=0
//TODO: probably don't need many/most errno=0 lines. just handle it higher up in one place or so. arthur doesn't seem to have errno=0 everywhere.

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
  if(7==a->t){ DO(-1+TYPE_SEVEN_SIZE,cd(kV(a)[1+i]))} //-4 special trick: don't recurse on V members. assumes sizeof S==K==V
  if(0==a->t || 5==a->t) DO(a->n, cd(kK(a)[a->n-i-1]))
  #ifdef DEBUG
  if(0)R 0; //for viewing K that have been memory leaked
  #endif
  //assumes seven_type a->k is < PG()
  I o=((size_t)a)%PG();//file-mapped? 1:
  I k=sz(a->t,a->n), r=lsz(k);
  //assert file-maps have sizeof(V)==o and unpooled blocks never do (reasonable)
  if(sizeof(V)==o || r>KP_MAX)munmap(((V)a)-o,k+o); //(file-mapped or really big) do not go back into pool. 
  else repool(a,r);
  R 0;
}
K ci(K a){if(a)a->c++; R a;}

I bp(I t) {SW(ABS(t)){CSR(1, R sizeof(I)) CSR(2, R sizeof(F)) CSR(3, R sizeof(C)) default: R sizeof(V); } } //Default 0/+-4/5/6/7  (assumes sizeof(K)==sizeof(S)==...)
I sz(I t,I n){R 3*sizeof(I)+(7==t?TYPE_SEVEN_SIZE:n)*bp(t)+(3==ABS(t));} //not recursive. assert sz() > 0:  Everything gets valid block for simplified munmap/(free)
I PG(){R sysconf(_SC_PAGE_SIZE);} //pagesize:  size_t page_size = (size_t) sysconf (_SC_PAGESIZE);
I nearest(I i,I m){I k=i%m;R k?i+m-k:i;} //up 0,8,...,8,16,16,...
#define nearPG(i) nearest((i),PG())
//#define nearI(i) nearest((i),sizeof(I))

//This is an untested idea for avoiding all that goes on in backing out of memory allocations when an error occurs inside a function before everything is done:
//If you control the memory allocator one possibility is to work in "claimed" (sbreak) but "free" space and build the K data structure there.
//Doing ci() or something similar on it marks the space "used". on error you do nothing and the space remains "free" (mutex)

//Keyword "backingstore" in old k mailing list archives - extra KSWAP beyond regular swap space

K newK(I t, I n)
{ 
  K z;
  I k=sz(t,n);
  U(z=kalloc(k))
  //^^ relies on MAP_ANON being zero-filled for 0==t || 5==t (cd() the half-complete), 3==ABS(t) kC(z)[n]=0 (+-3 types emulate c-string)
  z->c=1; z->t=t; z->n=n;
  #ifdef DEBUG
  if(testtime) krec[kreci++]=z;
  #endif
  R z;
}
V kalloc(I k) //bytes. assumes k>0
{
  I r=lsz(k);
  if(r>KP_MAX)R amem(k);// allocate for objects of sz > 2^KP_MAX
  R unpool(r);
}
V amem(I k){K z;if(MAP_FAILED==(z=mmap(0,k,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANON,-1,0)))R ME; R z;}
V unpool(I r)
{
  V*z;
  V*L=((V*)KP)+r;
  I k=1<<r;
  if(!*L)
  {
    U(z=amem(k))
    if(k<PG()){I q=PG()/k;V y=z;DO(q-1, *(V*)y=y+k; y+=k) }//Low lanes subdivide pages
    *L=z;
  }
  z=*L;*L=*z;*z=0;
  R z;
}

I cl2(I v) //optimized 64-bit ceil(log_2(I)) 
{
    if(!v)R -1;// no bits set
    I e = 0;
    if(v & (v - 1ULL))e=1; //round up if not a power of two
    if (sizeof(V) >= 8)    //64-bit only
            if(v & 0xFFFFFFFF00000000ULL){e+=32;v>>=32;}
    if(v & 0x00000000FFFF0000ULL){e+=16;v>>=16;}
    //short CL2_LUT[1<<16]; DO(1<<16,if(i) CL2_LUT[i]=log2(i));
    //to use lookup table: e+=CL2_LUT[v] and comment out below. 
    if(v & 0x000000000000FF00ULL){e+=8; v>>=8; }
    if(v & 0x00000000000000F0ULL){e+=4; v>>=4; }
    if(v & 0x000000000000000CULL){e+=2; v>>=2; }
    if(v & 0x0000000000000002ULL){e+=1; v>>=1; }
    R e;
}


I lsz(I k){I r=cl2(k); R MAX(KP_MIN,r); } //pool lane from size. Ignore everywhere lanes < KP_MIN
I repool(V v,I r)//assert r < KP_MAX 
{
  memset(v,0,1<<r);
  *(V*)v=KP[r];
  KP[r]=v;
  R 0;
}
I kexpander(K*p,I n) //expand only. 
{
  K a=*p;
  V v; I c=sz(a->t,a->n),d=sz(a->t,n),e=nearPG(c),f=d-e;
  I r = lsz(c);
  if(r>KP_MAX) //Large anonymous mmapped structure - (simulate mremap)
  {
    if(f<=0) R 1;
    if(MAP_FAILED!=mmap(a+e,f,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANON|MAP_FIXED,-1,0)) R 1;//Add pages to end
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

K kapn_(K *a,V *v,I n)
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
    CS(1, memcpy(kI(k)+m,*v,n*sizeof(I)));
    CS(2, memcpy(kF(k)+m,*v,n*sizeof(F)));
    CS(3, strncpy(kC(k)+m,(S)*v,n); kC(k)[p]=0);
    CS(4, memcpy(kS(k)+m,*v,n*sizeof(S)))
    CD:   R 0;
  }
  if(t>0&&t<5&&p>1)k->t*=-1;
  R *a;
}

extern K kapn(K *a,V v,I n){R kapn_(a,&v,n);}

extern K kap(K*a,V v){R kapn_(a,&v,1);}

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

S sdup(S s){R strdupn(s,strlen(s));} //using this because "strdup" uses [used] dynamically linked malloc which fails with our static free
S strdupn (S s,I k) {S d=malloc(k+1);if(!d)R 0;d[k]=0;R memcpy(d,s,k);} // mm/o  (note: this can overallocate)
//I SC0N(S a,S b,I n) {I x=memcmp(a,b,n); R x<0?-1:x>0?1:a[n]?1:0; }// non-standard way to compare aaa\0 vs aaa
I strlenn(S s,I k){S t=memchr(s,'\0',k); R t?t-s:k;}

I FC(F a, F b)//Floating-Point Compare
{
  F E=0.00000000000000000001; //This value seems to work, might should be a different one though

  if(isinf(a)&&isinf(b)) R 0;
  if(ABS(a-b) <= E*MAX(ABS(a),ABS(b)))R 0;
  R a<b?-1:1;
}
F FF(F f){F F;R modf(f,&F);}//Floating-Point Fractional Part

I StoI(S s,I *n){S t; *n=strtol(s,&t,10); R !(errno!=0||t==s||*t!=0);}

I SC(S a,S b){I x=strcmp(a,b); R x<0?-1:x>0?1:0;}//String Compare: strcmp unfortunately does not draw from {-1,0,1}
S sp(S k)//symbol from phrase: string interning, Ks(sp("aaa")). This should be called before introducing any sym to the instance
{ //We are using this to ensure any two 'character-identical' symbols are in fact represented by the same pointer S
  //See Knuth Algorithm 6.2.2T
  #define LINK(n,x) (n)->c[((x)+1)/2] // -1 => 0 , 1 => 1
  if(!k)R 0;//used in glue. used in _2m_4. used in parse. Probably a good argument to keep since it's exposed for libraries via 2: dyadic
  N t=SYMBOLS, s=t->c[1],p=s,q=p,r; I a,x;
  if(!s){s=t->c[1]=newN();P(!s,(S)ME);s->k=sdup(k); if(!s->k){free(s);t->c[1]=0;ME;} R s->k;} // <-- strdup here and below 
  while(q)
  { if(!(a=SC(k,p->k))){R p->k;}//In the usual tree put: p->k=k,p->v=v before returning
    if(!(q=LINK(p,a))){q=newN();P(!q,(S)ME);q->k=sdup(k);if(!q->k){free(q);ME; R 0;} LINK(p,a)=q;break;}//Usual tree would q->v=v. mmo
    else if(q->b){t=p;s=q;}
    p=q;
  }
  a=0>SC(k,s->k)?-1:1;
  r=p=LINK(s,a);
  while(p!=q){x=SC(k,p->k); p->b=x;p=LINK(p,x);}
  if(!s->b){s->b=a;R p->k;}
  else if(s->b==-a){s->b=0; R p->k;}
  if(r->b==a){p=r; LINK(s,a)=LINK(r,-a); LINK(r,-a)=s; s->b=r->b=0;}
  else if(r->b==-a)
  { p=LINK(r,-a); LINK(r,-a)=LINK(p,a); 
    LINK(p,a)=r; LINK(s,a)=LINK(p,-a); LINK(p,-a)=s;
    if     (p->b== a){s->b=-a; r->b=0;}
    else if(p->b== 0){s->b= 0; r->b=0;}
    else if(p->b==-a){s->b= 0; r->b=a;}
    p->b=0;
  }
  t->c[s==t->c[1]?1:0]=p;
  R q->k; 
}

//S spkC(K a){S u=strdupn(kC(a),a->n),v=sp(u);free(u);R v;}
S spn(S s,I n){I k=0;while(k<n && s[k])k++; S u=strdupn(s,k); if(!u)R 0; S v=sp(u); free(u); R v;} //safer/memory-efficient strdupn

//pt(N t){N l=t->c[0],r=t->c[1];O("node: %s  balance: %d\n", t->k, t->b);O(" Lchild: %s\n",l?l->k:"null");O(" Rchild: %s\n",r?r->k:"null");if(l)pt(l);if(r)pt(r);}
I rp2(I v){v--;v|=v>>1;v|=v>>2;v|=v>>4;v|=v>>8;v|=v>>16;if(sizeof(V)>=8)v|=v>>32;v++;R MAX(1,v);}//round up to integer power of 2 (fails on upper 1/4 signed)

K kclone(K a)//Deep copy -- eliminate where possible
{
  if(!a) R 0;
  I t=a->t,n=a->n;
  K z= 7==t?Kv():newK(t,n);
  if     (4==ABS(t)) DO(n, kS(z)[i]=kS(a)[i])  //memcpy everywhere is better
  else if(3==ABS(t)) DO(n, kC(z)[i]=kC(a)[i]) 
  else if(2==ABS(t)) DO(n, kF(z)[i]=kF(a)[i]) 
  else if(1==ABS(t)) DO(n, kI(z)[i]=kI(a)[i]) 
  else if(0==    t ) DO(n, kK(z)[i]=kclone(kK(a)[i])) 
  else if(5==    t ) DO(n, kK(z)[i]=kclone(kK(a)[i]))
  else if(7==    t )
  {
    I k=0;

    z->t=a->t; 
    I vt=z->n = a->n;
    K kv;

    V*v;
    SW(vt)
    {
      CS(1, k=((K)kV(a)[CODE])->n-1;
            M(z,kv=newK(-4,k+1))
            v=(V*)kK(kv);
            //v[k]=0;//superfluous reminder
            DO(k, V w=kW(a)[i];
                  if(VA(w))v[i]=w;  //TODO: is this ok for NAMES? see similar code in capture()
                  else
                  {
                    K r=kclone(*(K*)w); //oom
                    V q=newE(LS,r); //oom
                    kap((K*) kV(z)+LOCALS,q);//oom
                    cd(q);//kap does ci
                    q=EVP(q); //oom free z etc. kap needs checking 
                    v[i]=q;
                  }
              )
      )
      CS(2, M(z,kv=newK(-4,3))
            v=(V*)kK(kv);
            memcpy(v,kW(a),3*sizeof(V));
        )
      CS(3,M(z,kv=kclone((K)kV(a)[CODE])))
    }
    kV(z)[CODE]=kv;
    kV(z)[CONTEXT]=kV(a)[CONTEXT];   
    cd(kV(z)[PARAMS]); kV(z)[PARAMS]=kclone(kV(a)[PARAMS]); //oom ; fill instead of kclone?
    cd(kV(z)[LOCALS]); kV(z)[LOCALS]=kclone(kV(a)[LOCALS]); //oom ; fill instead of kclone?
    kV(z)[CONJ]=kclone(kV(a)[CONJ]);  //oom
  }

  R z;
}

K collapse(K x) //oom
{
  K z;
  if(1==xn){ z=ci(*kK(x)); cd(x);} 
  else z=demote(x);
  R z;
}

K demote(K a)//Attempt to force unnaturally occurring lists into vectors
{ // change: (0;1;2) ->  0 1 2
  //   keep: (1;0.66667)  //numerics are not reconciled as you might guess
  // change: (1) -> ,1    //doesn't solve parenthetical expressions offhand
  if(!a) R a; //dollar() uses this
  I t=a->t, n=a->n;
  if(0!=t || 1>n) R a;
  I p=kK(a)[0]->t;
  DO(n, if(p!=kK(a)[i]->t)p=0) 
  if(!(1<=p && p <= 4))R a;
  K z=newK(-p,n); M(a,z) 
  if     (4==p)DO(n,kS(z)[i]=*kS(kK(a)[i])) //use memcpy instead
  else if(3==p)DO(n,kC(z)[i]=*kC(kK(a)[i]))
  else if(2==p)DO(n,kF(z)[i]=*kF(kK(a)[i]))
  else if(1==p)DO(n,kI(z)[i]=*kI(kK(a)[i]))
  cd(a);
  R z;
}
K promote(K a)//Identity on lists. Lists from vectors. Pseudo-enlist on atoms (always 0-lists).
{ //0 1 2 -> (0;1;2) 
  I at=a->t;
  if(0==at) R ci(a);
  if(4< at) {K z=newK(0,1); U(z); *kK(z)=ci(a); R z;}
  K z=newK(0,a->n); U(z);
  K x;
  I v=ABS(at);
  if     (4==v) DO(a->n, x=newK(v,1); M(x,z) *kS(x)=kS(a)[i]; kK(z)[i]=x ) 
  else if(3==v) DO(a->n, x=newK(v,1); M(x,z) *kC(x)=kC(a)[i]; kK(z)[i]=x ) 
  else if(2==v) DO(a->n, x=newK(v,1); M(x,z) *kF(x)=kF(a)[i]; kK(z)[i]=x ) 
  else if(1==v) DO(a->n, x=newK(v,1); M(x,z) *kI(x)=kI(a)[i]; kK(z)[i]=x ) 
  R z;
}

//Note: there is a difference between the symbols that require quotes when printed and valid names accepted by the parser ".k._a"
I simpleString(S a) //0 on any symbol's string that requires quotes, eg `"a - b!"
{
  I n=strlen(a);
  if(n && isdigit(*a))R 0; //cannot begin with a number 
  if(1==n && *a=='.') R 0; //cannot consist only of "."
  if(n>1 &&  a[n-1] == '.' && a[n-2] == '.') R 0;          //cannot end in two dots
  DO(n, if(!isalnum(a[i]) && a[i]!='_' && a[i]!='.') R 0)  //These rules are taken from parse() - anything that would fail to tokenize
  DO(n-1, if(a[i]=='.' && isdigit(a[i+1])) R 0)            //number cannot follow dot
  DO(n-2, if(a[i]=='.' && a[i+1]=='.' && a[i+2]=='.') R 0) //cannot have three dots
  R 1; 
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
I mergerComparer(K a, I r, I i, I j)//Could unroll this
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
void merger(K a, I r, K x, K y, I s, I t, I m)
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
void doMergeGrade(K a, I r, K x, K y, I s, I t)
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

K end(){R 0;} V ends[] = {end}; I bk(V p){R p==ends;} //break: is ; or \n

C ac[] = "/\\'";
K over(){R 0;} K scan(){R 0;} K each(){R 0;}
K eachright(){R 0;} K eachleft(){R 0;} K eachpair(){R 0;}
V adverbs[] = {over,scan,each,eachright,eachleft,eachpair};

C vc[]="+-*%|&^!<>=~@?_,#$.:";// was "!#$%&*+,-.<=>?@^_|~:";
#define _VERB1 flip,negate,first,reciprocal,reverse,where,shape,enumerate,grade_up,grade_down,group,not_attribute,atom,range,floor_verb,enlist,count,format,dot_monadic,colon_monadic
#define _VERB2 plus,minus,times,divide,max_or,min_and,power,rotate_mod,less,more,equals,match,at,what,drop_cut,join,take_reshape,dollar,dot,colon_dyadic
#define _0VERB1 _0m,_1m,_2m,_3m,_4m,_5m,_6m
#define _0VERB2 _0d,_1d,_2d,_3d,_4d,_5d,_6d //This has a dependency in the parser - magic number 6
V vm[]  = {_VERB1};
V vd[]  = {_VERB2};
V vm0[] = {_0VERB1};
V vd0[] = {_0VERB2};

V addressSSR, addressWhat, addressAt, addressDot, addressColon;

S IFS[3] = {"x","y","z"};
S IFP[3]; //Implicit function parameters sp(x),...

I stringHasChar(S s,C c){I i=0;while(s[i])if(c==s[i++])R 1;R 0;} //string never has '\0'
I charpos(S s,C c){I i=0;while(s[i] && c!=s[i])i++; R i;}

C verbsChar(V p)  {R in(p,vm)?vc[diff(p,vm)]:in(p,vd)?vc[diff(p,vd)]:'\0';}
I isCharVerb(C c) {R stringHasChar(vc,c);}
I charsVerb(C c)  {R charpos(vc,c);}

C adverbsChar(V p) { R in(p,adverbs)?ac[diff(p,adverbs)%3]:'\0';}

I isCharAdverb(C c){R stringHasChar(ac,c);}
I charsAdverb(C c) {R charpos(ac,c);}

I sva(V p) //simpleVerbArity: Use boundaries of arrays to determine verb class in O(1) constant time
{ 
  I k;
  if(in(p,vm ))R 1; // + -    (~40 of these)
  if(in(p,vd ))R 2;  
  if(in(p,vm0))R 1; // 0: 6:  (~14 of these)
  if(in(p,vd0))R 2;  

  if((k=diff(p,vm_))<vm_ct && k>=0)R 1; // _abs   (~46 of these)
  if((k=diff(p,vd_))<vd_ct && k>=0)R 2; 
  if((k=diff(p,vt_))<vt_ct && k>=0)R 3;  
  R 0;              // (~100 in total) 
}
I adverbClass(V p) { R in(p,adverbs)? 1+diff(p,adverbs)/3: 0; } //0: not an adverb, 1: / \ ', 2: /: \: ':

I specialValence(V p){ R (p==addressSSR||p==addressWhat)?3:(p==addressAt||p==addressDot)?4:0;}
I valence(V p)
{
  I a,i;
  a=specialValence(p);
  a=a?a:sva(p);
  if(a) R a;

  if(adverbClass(p)) R 0;

  K v=*(K*)p;
  if(!v || v->t != 7) R 0;

  //Remember, valence is computed independently of the number of items stored in the conjunction, e.g. +[1;2;3;4;;;] works but +[1;2;3] fails (?)
  V*w=kW(v);
  I t= v->n; 

  K b=kV(v)[CONJ];
  I c=0;
  if(b){ DO(b->n,if(kK(b)[i])c++)  R b->n-c; } //Valence becomes 'set' for @[;;] or @[;;;] after first projection
  
  if(1==t)
  {
    i=kVC(v)->n-1;
    V*k=kW(v)[i-1];
    // /: or \: or dyadic verb at end, 2, else 1 (other adverb,monadic-verb)
    if(*k==eachright || *k==eachleft)R 2; //todo: this looks off: eachright can be valence 1? as in +:/:  ?
    if(i>1 && *k==each || *k==over || *k==scan)  //for f'[x;y;z], f/[x;y;z], ...
    {
      V*q; I j=0,s;
      do q=kW(v)[i-2-(j++)]; while(*q==each || *q==over || *q==scan);

      s=sva(q);
      if(s && !specialValence(q)) R s - ((i-2-j)?0:1); // |+\ or +\   (leaves out |@\ and @\ ...or not...or intentional...?)

      if(j<i-2 ) R valence(q)-1; //eg  f:...0(0|+)\ (the zero binds on the left) 
      else 
      {
        R valence(q);
        //if(*kW(v)==q) R valence(q);
        //R valence(q)-1;
      }  //if(!VA(q) && (*q)->t==7) R valence(q);

    }
    if(adverbClass(k)) R 2;
    if(sva(k)>1 && i>1 && !VA(kW(v)[i-2]))R valence(k)-1; //NB: f:(7+);g:(1+|+); both dyad-plus, f valence 1, g valence 2. Rule is 1 for nd; 2 for vd;
    R valence(k);
  }
  if(2==t) R (I)w[0]; //could we have determined these types implicitly... ?
  if(3==t) R ((K)kV(v)[PARAMS])->n;
  
  R 0;
}

I VA(V p){R sva(p) || adverbClass(p);}  //Verb or Adverb?

I isescape(UC c) {R (c=='"'||c=='\\'||c=='\b'||c=='\n'||c=='\r'||c=='\t');}
I needspt0(F f){if(isnan(f)||-FI==f||FI==f)R 0; Z C b[512];snprintf(b,512,"%.*g",(int)PP,f); R !stringHasChar(b,'.') && !stringHasChar(b,'e');}//no better way I know

int splitprint(V u, const char *s, ...)  //print for either stdout or for 5: monadic (_5m)
{
  Z C b[512];
  va_list args;
  va_start (args, s);
  if(!u) vprintf (s, args); //stdout
  else //5: monadic
  { 
    I n=vsnprintf(b,512,s,args);
    if(!kapn(u,b,n)); //todo: err handling
  }
  va_end (args);
  R 0;
}

#define O_(...) splitprint(u,__VA_ARGS__)
void printAtDepth(V u, K a, I d, I x, I vdep, I b) //u {0=stdout or K* charvec }
{ //Only pass a bounded (<512?) number of chars at a time to O_ (ie don't use "%s",long_string )
  if(!a)R; //0==NULL internal K. NB: Lowercase _n is a valid K of type 6. 

  I t=a->t;//Has to go below null check

  if(x)DO(d,O_(" "))
  if(!u && d>19){O_("...");R;}//too deep for stdout
  if(5==t){O_(".");d+=1; t=0;}
  if(t<=0 && a->n==1)O_(",");

  //TODO: separate lines ("aaa";"bbb") but same line ("aaa";"bbb";"c")
  //K3.2 "c",,"aa"  --> prints one line not two

  I m=0;K s;//Exceptions, e.g. ("abc",0 1 2) yields ("a";"b";"c";0;1;2) 
  //s!=0 check is being nice here and letting bracket [] K with NULLS act as non-degenerate K
  if(0==t && !b)DO(a->n, s=kK(a)[i]; if(s && s->t <=0 && (s->n || -3==s->t)){m=1;break;} if(s && s->t==5){m=1;break;})//Set m?

  I enclose= (0==t && a->n!=1) || (t==7 && vdep);//verb_depth
  if(enclose)O_(b?"[":"(");

  I f;F g;
  
  I pmax = 500;//limit output on long lists. could be improved. would be better as a global variable with <= 0 indicating disabled
  #define CPMAX {if(!u && i>pmax){O_("...");break;}}

  if(0==    t )                            DO(a->n, CPMAX printAtDepth(u,kK(a)[i],d+1,i*m,0,0);O_(i<_i-1?m?"\n":";":""))
  if(1==ABS(t)) if(!a->n) O_("!0");    else DO(a->n, CPMAX f=kI(a)[i]; f==IN?O_("0N"):f==-II?O_("-0I"):f==II?O_("0I"):O_("%ld",f); if(i<_i-1)O_(" "))
  if(2==ABS(t)) if(!a->n) O_("0#0.0"); else DO(a->n, CPMAX g=kF(a)[i];isnan(g)?O_("0n"):g==-FI?O_("-0i"):g==FI?O_("0i"):O_("%.*g",(int)PP,g);if(i<_i-1)O_(" ");else if(needspt0(g))O_(".0"))
  if(3==ABS(t)) { O_("\"");                 DO(a->n, CPMAX UC c=kC(a)[i];
                                              if(isprint(c)&&(!isescape(c)))O_("%c",c);
                                              else if(isescape(c))
                                                SW(c){CS('"',O_("\\\""));CS('\\',O_("\\\\"));CS('\b',O_("\\b"));CS('\n',O_("\\n"));CS('\r',O_("\\r"));CS('\t',O_("\\t"));}
                                              else O_("\\%.3o",c) ) O_("\""); }
  if(4==ABS(t)) if(!a->n) O_("0#`");  
                else 
                { I ss=0,sl;S str;
                  DO(a->n, CPMAX str=kS(a)[i]; sl=strlen(str);ss=simpleString(str);
                           O_("`"); if(!ss) O_("\""); DO2(sl, O_("%c", str[j] )) O_(i<_i-1?" ":""); if(!ss) O_("\""); 
                    ) 
                }

  if(7==    t)
  {
    if(1==a->n)
    {
      I i,k; S s;
      V *v=kW(a),*p;
      for(i=0;p=v[i];i++)
      { //TODO: mute extraneous :
        if     (in(p,vd0)) O_("%ld:" , p-vd0);
        else if(in(p,vm0)) O_("%ld::", p-vm0);
        else if((k=diff(p,vt_)) < vt_ct && k>=0){s=vt_s[p-vt_]; k=strlen(s); DO(k,O_("%c",s[i]))}
        else if((k=diff(p,vd_)) < vd_ct && k>=0){s=vd_s[p-vd_]; k=strlen(s); DO(k,O_("%c",s[i]))}
        else if((k=diff(p,vm_)) < vm_ct && k>=0){s=vm_s[p-vm_]; k=strlen(s); DO(k,O_("%c",s[i]))} 
        else if(k=sva(p)) O_(2==k?"%c":"%c:",   verbsChar(p));
        else if(k=adverbClass(p)) O_(1==k?"%c":"%c:", adverbsChar(p));
        else printAtDepth(u,*(K*)p,d+1,0,1+vdep,0);
      }
    }
    else if(2==a->n){ R;} //TODO cfunc
    else if(3==a->n)
    {
      O_("{%s}", kC(kV(a)[CODE])); 
    }
    if(kV(a)[CONJ]){printAtDepth(u,kV(a)[CONJ],d+1,0,0,1);}
  } 
  if(enclose)O_(b?"]":")");
}

K show(K a)
{
  printAtDepth(0,a,0,0,0,0);
  if(a && a->t!=6)O("\n");  
  if(!a)oerr();
  R a;
}

V ptf(V v){ R 0;} //delme

int main(int argc,S*argv)
{
  kinit();
  args(argc,argv);
  boilerplate();
  attend(); //loop on stdin/inet
  R 0;
}

#ifdef DEBUG
void tf(N n){if(!n)R;DO(2,tf(n->c[i]))free(n->k);repool(n,lsz(sizeof(Node))); } //tree free
I kreci=0; 
V krec[1000000];
I CV(K v) { V a[1000]; I n=0; while(v) { dd(v); a[n++]=v; DO(n, DO2(n-i-1, if(a[i]==a[i+j+1]) R 1;)) if(!(7==v->t && 0==v->n)) R 0; V q=kW(v)[0]; v=0; if(q) v= *(K*)q; } R 0; }//seven_type contains cycle?
#endif

void finally()
{
#ifdef DEBUG   
tf(SYMBOLS); cd(KTREE); cd(KFIXED);
//valgrind --leak-check=full --show-reachable=yes /tmp/a.out
#endif
}

