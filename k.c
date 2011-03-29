//64-bit single-threaded implementation of K3.2.  Version is Kona 3.2.0
//todo abbreviations: mm/o = memory manage/optimize   lfop = localize for other platforms (eg needs ifdef changes)   oom = handle out-of-memory
#include "incs.h"

//Notes on memory manager: seems like atoms (and small lists?) are not released
//by K4 (see Skelton's remark: only contiguous arrays greater than 32MB are
//returned to OS). Also: " Why do you think it is memory fragmentation? The
//allocator in kdb+ is designed specifically to avoid that by using fixed size
//buckets."
#define KP_MIN 5  //2^x, must be at least ceil(lg(sizeof(V)))
#define KP_MAX 25 //2^x, 25->32MB  //TODO: base on available memory at startup (fixed percent? is 32M/2G a good percent?)
V KP[sizeof(V)*8+1]; //KPOOL

C errmsg[256]; //TODO: pthread_getspecific (not __thread) thread-local storage (different for mac os x)
extern K kerr(S s){ R snprintf(errmsg,256,"%s",s),(K)0;} 
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
  V z;
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
I rp2(I v){v--;v|=v>>1;v|=v>>2;v|=v>>4;v|=v>>8;v|=v>>16;v|=v>>32;v++;R MAX(1,v);}//round up to integer power of 2 (fails on upper 1/4 signed)

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
  if(4< at) {K z=newK(0,1); *kK(z)=ci(a); R z;}
  K z=newK(0,a->n);
  K x;
  I v=ABS(at);
  if     (4==v) DO(a->n, x=newK(v,1); *kS(x)=kS(a)[i]; kK(z)[i]=x ) //mm/o
  else if(3==v) DO(a->n, x=newK(v,1); *kC(x)=kC(a)[i]; kK(z)[i]=x ) 
  else if(2==v) DO(a->n, x=newK(v,1); *kF(x)=kF(a)[i]; kK(z)[i]=x ) 
  else if(1==v) DO(a->n, x=newK(v,1); *kI(x)=kI(a)[i]; kK(z)[i]=x ) 
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

K end(){} V ends[] = {end}; I bk(V p){R p==ends;} //break: is ; or \n

C ac[] = "/\\'";
K over(){} K scan(){} K each(){} K eachright(){} K eachleft(){} K eachpair(){}
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

//0: not a verb pointer, 1: monadic, 2: dyadic, 3: triadic
static I vn_ct, vm_ct, vd_ct, vt_ct;

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
  if(v->t != 7) R 0;

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
      V*q; I j=0;
      do q=kW(v)[i-2-(j++)]; while(*q==each || *q==over || *q==scan);
      
      if(!sva(q) || specialValence(q)) 
        if(j<i-2) R valence(q)-1; //eg  f:...0(0|+)\ (the zero binds on the left) 
        else R valence(q); //if(!VA(q) && (*q)->t==7) R valence(q);
    }
    if(adverbClass(k)) R 2;
    if(sva(k)>1 && i>1 && !VA(kW(v)[i-2]))R valence(k)-1; //NB: f:(7+);g:(1+|+); both dyad-plus, f valence 1, g valence 2. Rule is 1 for nd; 2 for vd;
    R valence(k);
  }
  if(2==t) R (I)w[0]; //could we have determined these types implicitly... ?
  if(3==t) R ((K)kV(v)[PARAMS])->n;
  
  R 0;
}

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


//TODO: Try (?) and grow adverb results as vectors before devolving to 0-type
//TODO: consider merging dv_ex with vf_ex
K dv_ex(K a, V *p, K b) 
{
  if(!p || !*p) R 0; //TODO: ???
  if(!b) R kerr("undefined"); //TODO: Projection?  'u v\' 

  V *o = p-1;

  //Arity of V?A_1...A_n-1 for X V?A_1...A_n Y; 0 for X Y, X A Y
  I k=adverbClass(*p)?adverbClass(*o)?1:sva(*o):sva(*p);
  k=adverbClass(*p)?adverbClass(*o)?1:valence(*o):valence(*p); //also t7 basic
  
  V adverb=*(V*)*p; //TODO: Implement adverb "Error Reports" error checking from manual
  I isSimpleVerb = sva(*p); 
  
  //k>2 --- ??? bound for special verbs ?.@ , etc.  ??? k=2 ??? valence is weird here
  //!(adver...  ---- added to let f/[;;;] through
  //if(k>2 && !(adverbClass(*p) && !VA(*o)))k=2; 
  if(k>2)k=2; 
 
  //TODO: for derived verbs like +/ you can add the sub-pieces in parallel
  if(2==k && adverb == over)
  {
      K u=0,v=0;
      K y=a?v=join(u=enlist(a),b):b; //oom u (TODO: need to unroll to 'x f/y' and 'f/y' to optimize?)
      I yt=y->t, yn=y->n;

      K z=0,g=0;

      if(yt  > 0){z=ci(y); GC;} 
      if(yn == 0) 
      {
        V **q=(V**)p-1,r; I s=-2==y->t;  
        if(VA(*q)) 
          if     (**q==plus)    z= s?Kf(  0):Ki(0);
          else if(**q==max_or)  z= s?Kf(-FI):Ki(0);
          else if(**q==times)   z= s?Kf(  1):Ki(1);
          else if(**q==min_and) z= s?Kf( FI):Ki(1);
          else  z=LE;
          GC;
      }
      K c=first(y),d;//mm/o
      //TODO: this reuse of g should be implemented in other adverbs 
      if(0 >yt) DO(yn-1, d=c; if(!g)g=newK(ABS(yt),1); memcpy(g->k,((V)y->k)+(i+1)*bp(yt),bp(yt)); c=dv_ex(d,p-1,g); if(2==g->c){cd(g);g=0;} cd(d); if(!c) GC;) //TODO: oom err/mmo unwind above - oom-g
      if(0==yt) DO(yn-1, d=c; c=dv_ex(d,p-1,kK(y)[i+1]); cd(d); if(!c) GC;) //TODO: err/mmo unwind above
      z=c;
cleanup:
      if(g)cd(g);
      if(u)cd(u);
      if(v)cd(v);
      R z;
  }

  if(2 > k && adverb == over)
  { //TODO: 'b f/ x'
    K u=b,c=0;I flag=0; 

    I useN=0,n=0;
    if(a && 1 == a->t){useN=1; n=*kI(a);}
    P(n<0,IE)

    if(useN)
    {
      DO(n, c=dv_ex(0,p-1,u); if(b!=u)cd(u); U(u=c))
      c=c?c:b;//mm/o
    }
    else while(1)
    {
      if(matchI(b,c) || (u!=b && matchI(u,c)))flag=1;
      if(u!=b) cd(u);
      if(flag)break;
      u=c?c:u;
      U(c=dv_ex(0,p-1,u))
    }
    R c;
  }

  if(2==k && adverb == scan)
  {
    K u=0; K y=a?join(u=enlist(a),b):ci(b); cd(u); //oom
    I yt=y->t, yn=y->n;
    if(yt  > 0 || yn == 0) R y;
    
    K z=newK(0,yn),c,d;
    kK(z)[0] = first(y);

    K g;
    if( 0 >yt) DO(yn-1, d=kK(z)[i]; g=newK(ABS(yt),1); memcpy(g->k,((V)y->k)+(i+1)*bp(yt),bp(yt)); c=dv_ex(d,p-1,g); cd(g); U(c) kK(z)[i+1]=c) //TODO: err/mmo  cd(y) - oom-g
    if( 0==yt) DO(yn-1, d=kK(z)[i]; c=dv_ex(d,p-1,kK(y)[i+1]); U(c) kK(z)[i+1]=c  ) //TODO: err/mmo  cd(y)
    cd(y);

    //This was to fix (there may be a better refactoring):  11+\1 -> 12 (1 K) but  11+\1 2 -> 11 12 14 (3 K)
    if(a&&atomI(b)) { y=z; M(z,u=Ki(1)) M(y,u,z=drop(u,z)) cd(y); cd(u); }

    R collapse(z);
  }

  if(2 > k && adverb == scan)
  { //TODO: 'b f\ x'
    K u=enlist(b),v,w,c=0,d;I flag=0;//TODO: optimize/memory manage enlists,firsts,reverses here
    U(u);

    I useN=0,n=0;
    if(a && 1 == a->t){useN=1; n=*kI(a);}
    P(n < 0,IE) //mmo

    if(useN) DO(n, d=first(reverse(u)); c=dv_ex(0,p-1,d); u=join(u,enlist(c));) //mm/o
    else while(1)
    {
      d=first(v=reverse(u));cd(v);
      if(matchI(b,c) || matchI(c,d))flag=1;
      if(!flag && c)
      {
        u=join(v=u,w=enlist(c));
        cd(v);cd(w);cd(d);
        d=c;
      }
      if(flag){cd(c);cd(d);break;}
      c=dv_ex(0,p-1,d);cd(d);
    }
    R u;
  }

  if(2==k && adverb == each)
  {
    if(!a) adverb= eachright;
    else if(a->t <= 0 && b->t <= 0 && a->n != b->n) R LE; 
    else if(a->t > 0 && b->t > 0) R dv_ex(a,p-1,b);
    else if (a->t > 0) adverb = eachright;
    else if(b->t > 0) adverb = eachleft;
    else
    {
     //a and b both lists/vectors of size an
      a=promote(a); //oom
      b=promote(b); //oom
      K z = newK(0,a->n); //oom
      DO(a->n, kK(z)[i]=dv_ex(kK(a)[i],p-1,kK(b)[i])) //oom/err
      cd(a);
      cd(b);
      R demote(z);
    }
  }

  if(2 > k && adverb == each)
  {
    I bt=b->t, bn=b->n;
    if(bt > 0) R dv_ex(0,p-1,b);
    else
    {
      K z = newK(0,bn),d=0; //oom
      K g;
      if(0 >bt) DO(bn, g=newK(ABS(bt),1); memcpy(g->k,((V)b->k)+i*bp(bt),bp(bt)); d=dv_ex(0,p-1,g); cd(g); U(d) kK(z)[i]=d) //TODO: err/mmo - cd(z) - oom-g
      if(0==bt) DO(bn, d=dv_ex(0,p-1,kK(b)[i]); U(d) kK(z)[i]=d) //TODO: err/mmo - cd(z)
      R demote(z);
    }
  }

  if(adverb == eachright) // {1}/:!9 is valid
  {
    I bt=b->t, bn=b->n;
    if(bt > 0) R dv_ex(a,p-1,b);
    K z = newK(0,bn), d;
    K g;
    if(0 >bt) DO(bn, g=newK(ABS(bt),1); memcpy(g->k,((V)b->k)+i*bp(bt),bp(bt)); d=dv_ex(a,p-1,g); cd(g); U(d) kK(z)[i]=d) //TODO: err/mmo oom-g
    if(0==bt) DO(bn, d=dv_ex(a,p-1,kK(b)[i]); U(d) kK(z)[i]=d)
    R demote(z);
  }

  if(adverb == eachleft)
  {
    if(!a) R VE; 
    I at=a->t, an=a->n;
    if(at > 0) R dv_ex(a,p-1,b);
    K z = newK(0,an),d;
    K g;
    if(0 >at) DO(an, g=newK(ABS(at),1); memcpy(g->k,((V)a->k)+i*bp(at),bp(at)); d=dv_ex(g,p-1,b); cd(g); U(d) kK(z)[i]=d) //TODO: err/mmo oom-g
    if(0==at) DO(an, d=dv_ex(kK(a)[i],p-1,b); U(d) kK(z)[i]=d) //TODO: err/mmo
    R demote(z);
  }

  if(adverb == eachpair) //2==k necessary?
  {
    I bt=b->t, bn=b->n;
    if(bt >  0) R dv_ex(b,p-1,b);
    if(bt <= 0)
    {
      if     (bn == 0 && !a) R LE; 
      else if(bn == 0 &&  a) R newK(0,0);//TODO: memory manage/ optimize in join with null ptr ?
      else if(bn < 2) R newK(0,0);//TODO: this newK and the above.....does empty list type depend on input?
    }

    K z = newK(0,bn-1),d=0; //oom
    K g,h;
    if(0 >bt)DO(bn-1, h=newK(ABS(bt),1); g=newK(ABS(bt),1); memcpy(h->k,((V)b->k)+(i)*bp(bt),bp(bt)); memcpy(g->k,((V)b->k)+(i+1)*bp(bt),bp(bt)); d=dv_ex(g,p-1,h); cd(g);cd(h);U(d) kK(z)[i]=d) //TODO: err/mmo - cd(z) - oom-g-h
    if(0==bt)DO(bn-1, d=dv_ex(kK(b)[i+1],p-1,kK(b)[i]); U(d) kK(z)[i]=d) //TODO: err/mmo - cd(z)

    z=demote(z); //oom

    if(a) //mmo
    {
      K u,v;
      u=enlist(a);//oom
      v=join(u,z);//oom
      cd(u);
      R v;
    }

    R z;
  }

  //this could be better ??
  I gn=0;
  if      (valence(*p)>=2 && a && b) gn=2;
  else if (a) R VE;  //?
  else if (b) gn=1;

  K g=newK(0,gn);U(g);
  if(gn > 1) kK(g)[1]=b;
  if(gn > 0) kK(g)[0]=a?a:b;

  K temp = vf_ex(*p,g);

  memset(kK(g),0,g->n*sizeof(K)); cd(g); //Special privileges here...don't ci() members beforehand
  R temp;
}

//1. Use PARAMETER list (or XYZ tuple) to merge CONJ and ARGS-G into LOCAL-DICT-TREE
//2. Execute as normal, except
//   a. The LOCAL-DICT-TREE acts as the "KTREE"
//   b. Double-colon assignment :: adds to the dictionary in CONTEXT

//Note:  a:{c::1}       <--- even without executing c is set (_n) in context
//       a:{{d::1}}     <--- d set in context (_n, if executed then 1)
//       a:{e:1 {e::2}} <--- e not set

//  X1   local vars
//  X2   _f self-reference
//  X3   a::2 global assignment 
//  X4   {[a;b;c]} args
//  X5   {x+y} implicit args
//  X6   execution {}[2]
//  X7   assigned variables wholly local: {b} (global/context) vs. {b:2} (local)
//  X8   projection {}[1;;3] --- 7-{1,2,3} types. Verb projections come for free
//   9   proper sub-functions (hint is the non-null f passed to wd_(). Inherit/copy active dict to kV()[LOCAL] )
//       Arthur: "subfunctions are just projections, eg  c:{[f;g]{f g x}} composition d:{[f;g]{[f;g;x]f g x}[f;g]} composition c[-:;%:] 3 ; d[-:;%:] 3  
//   10  {  :x  } early return
//   11  Reusably compiled

//For -7 (7-0) CONJ is unexecuted brackets. For 7-{1,2,3} it's 0-type with NULLs
//K3.2 Bug - {b:1_,/";a",/:$a:!x; "{[",b,"]a3}[" ,(1_,/";",/:$a ),"]" } 67890  --> Sometimes works, sometimes stack error, sometimes crash
K vf_ex(V q, K g) 
{
  V w=*(V*)q;
  if(!g)R 0; //??? R w converted to type7...or ?
  K z=0;
  U(g=promote(g))
  I gn=g->n;

  I k=sva(q);
  I n=-1,j=0;
  if(( k || ((K)w)->t==7) && (w && gn > (n=valence(q)) && !(!n && 1>=gn))){VE; GC;} //could remove 1>=gn condition ?
  I argc=0; DO(gn,if(kK(g)[i])argc++)

  K a=0,b=0,c=0,d=0;
  if(gn >0) a=kK(g)[0]; if(gn >1) b=kK(g)[1]; if(gn >2) c=kK(g)[2]; if(gn >3) d=kK(g)[3];

  //valence overloaded verbs 
  if(gn > 2 && (w==what || w==_ssr)){ z=(w==what?what_triadic:_ssr)(a,b,c); GC; }
  if(gn > 2 && (w==at   || w==dot )){ z= (w==at?at_tetradic:dot_tetradic)(a,b,c,d); GC;}
  //common verbs

  if(2==k && a && b){ z=((K(*)(K,K))w)(a,b); GC;}
  //? (+).1 -> err ; {[a;b]a+b} 1 -> err
  if(2==k && !a){VE; GC;} //Reachable? Projection?

  //Reachable: try "#'(1;1 2)" (the # is dyadic not monadic #:). We return projection (#[1;],#[1 2;]), K3.2 gives valence error
  if(2==k && !b)
  { K v = Kv(), kb = newK(-4,2); M(v,kb)
    kK(kb)[0]=q; 
    kK(kb)[1]=0;
    kV(v)[CODE] = kb;
    z = vf_ex(&v,g); //Punt and let another call to vf_ex handle projecting. Probably could build the projected-verb here instead.
    cd(v);
    GC;
  } //old comment: Projection? '(1+)' -> 1+  Build 7-verb? (Refactor with 'c' from []+: ex and maybe another place?)

  //+:[a] ... +:[a;b] handled above (valence err)
  if(1==k && a) { z= ((K(*)(K))w)(a); GC;}
  if(1==k && !a) GC; //Reachable? Projection?
  //Functions  7-{1,2,3}
  K f = (K) w; I ft=f->t;

  if(ft != 7){z=g?dot(f,g):f; GC;}//TODO: check this for !a and for dict. ternary is superfluous since g nonzero?
  I t=f->n;
  if(-1==n)n=valence(f); //don't compute twice

  //Projecting simple verbs works. The ex 7-type wrapper will catch simple verbs and they will make it back here. (except in above 2==k && a && !b case?)
  K o=kV(f)[CODE]; K p=kV(f)[PARAMS]; K s=kV(f)[LOCALS]; K r=kV(f)[CONJ]; 
  I special = 1==t && !r && (addressAt==*kW(f) || addressDot==*kW(f) || addressWhat==*kW(f)); //_ssr is not special (not overloaded)

  I ii=o->n-2; //not the terminating NULL, but the entry before
  V*u=(V*) kK(o)+ii;
  if(2==n && 1==adverbClass(*u) ) n=1; //   / \ '  but maybe should exclude '

  if(n && (argc < gn || (gn < n && (!special||gn<=1) ))) //Project. Move this ahead of verbs when finished
  {
    z=kclone(f); //Is this an opportunity to capture an under-referenced function? Consider if it could be in use as part of assignment, etc.
    if(!z)GC;
    K*m=(K*)kV(z)+CONJ;
    if(special)n=2; // .'"98" cases. allows a:.[+] then a 2 3  (. is forced 2-adic & not .[;;;]) is this a kluge?
    if(!*m) *m=newK(0,n);
    if(!*m){cd(z);GC;}
    K *q=kK(*m);
    DO((*m)->n, if(!q[i] && j<gn) q[i]=ci(kK(g)[j++]))   
    GC;
  }//K3.2 Projection {[a;b;c]}[;1][1;] returns self. Indicates different (7-0 style?) method
  
  V v;K tree;
  SW(t)
  {
    CS(1,//Executing a derived verb such as 1+2* or (+/)
      if(!r) {z=ex2(kW(f),g);GC;} //No CONJ
      K m=newK(0,r->n);           //CONJ
      if(!m)GC;
      K *q=kK(m);
      DO(m->n, q[i]=ci(kK(r)[i]); if(!q[i] && j<gn) q[i]=ci(kK(g)[j++]))   
      z=ex2(kW(f),m); 
      cd(m);
    )
    CS(2, //Executing a dynamically loaded library function from 2:
      v=kW(f)[1];
      K a[7]; if(r)memcpy(a,kK(r),MIN(r->n,7)*sizeof(V)); //MIN(.,7) is superfluous
      DO(7,if(!a[i] && j<gn)a[i]=kK(g)[j++])
      SW(n)
      {
        CS(0,z=((K(*)())v)())
        CS(1,z=((K(*)(K))v)(a[0]))
        CS(2,z=((K(*)(K,K))v)(a[0],a[1]))
        CS(3,z=((K(*)(K,K,K))v)(a[0],a[1],a[2]))
        CS(4,z=((K(*)(K,K,K,K))v)(a[0],a[1],a[2],a[3]))
        CS(5,z=((K(*)(K,K,K,K,K))v)(a[0],a[1],a[2],a[3],a[4]))
        CS(6,z=((K(*)(K,K,K,K,K,K))v)(a[0],a[1],a[2],a[3],a[4],a[5]))
        CS(7,z=((K(*)(K,K,K,K,K,K,K))v)(a[0],a[1],a[2],a[3],a[4],a[5],a[6]))
      }
    )
    CS(3, //Executing a {} character function such as {1+1}, {x+y+z-1}, or {[a;b] a+b}
      tree=newK(5,p->n+s->n); if(!tree) GC; //note: cleanup is unusual -- could turn into double labels
      DO(tree->n, if(!(kK(tree)[i]=newK(0,3))){cd(tree); GC;}) //shallow dict copy -- dictionary entry pool?
      DO(tree->n, DO2(3, kK(DI(tree,i))[j] = ci(kK((i<p->n?DI(p,i):DI(s,i-p->n)))[j])))//shallow copy
      I j=0; K*e;
      DO(p->n,e=EVP(DI(tree,i)); cd(*e); *e=0; if(r && i<r->n) *e=ci(kK(r)[i]); if(!*e && j<g->n) *e=ci(kK(g)[j++])) //merge in
      z=ex(wd_(kC(o),o->n,&tree,f)); 
      cd(tree);
    )
  }

cleanup:
  cd(g);
  R z;
}

I VA(V p){R sva(p) || adverbClass(p);}  //Verb or Adverb?

//Could probably fold ex0 into this function
V ex_(V a, I r)//Expand wd()->7-0 types, expand and evaluate brackets
{
  K x,y=0,z;

  if(!a || VA(a) || bk(a)) R a;
  if(!(x=*(K*)a) || 7!=xt || (0<xn && xn<4)) R ci(x); //assert xn>=4 -> conditionals or similar

  r=xn<4?r:xn; //suggests maybe r should be stored on 7type itself

  if(kV(x)[CONJ])
  {
    y=ex_(kV(x)+CONJ,2); //Use 0-type with NULLS if passing to function
    //U(y); oom/mmo ??? can this cause an infinite loop?
    if(y->t == 0 && y->n==0){cd(y); y=_n();}
  }
  z=ex0(kW(x),y,r);  //eval wd()
  cd(y);

  R z;
}

K ex(K a){ U(a); K z=ex_(&a,0); cd(a);R z;} //Input is 7-0 type from wd()


K ex0(V*v,K k,I r) //r: {0,1,2} -> {code, (code), [code]} Reverse execution/return multiple (paren not function or script) "list notation"  {4,5,6,7} -> {:,if,while,do}
{
  I n=0, e=1, i,a;
  while(v[n])if(bk(v[n++]))e++;

  K z=0, x;

  SW(r)
  {
    CS(0, for(i=-1;i<n;i++)if(-1==i||bk(v[i])){cd(z); U(x=ex1(v+1+i,0)) z=bk(x)?_n():x;})//  c:9;a+b;c:1 
    CS(4, for(i=-1;i<n;i++)if(-1==i||bk(v[i])){U(x=ex1(v+1+i,0)) x=bk(x)?_n():x; while(++i<n&&!bk(v[i])); if(i==n) R x; if(xt!=1){cd(x);R TE;} a=*kI(x);cd(x); if(a)R ex1(v+i+1,0); else while(i<n&&!bk(v[i]))i++; } R _n())
    CSR(5,)CSR(6,)CS(7, do{U(x=ex1(v,0)) x=bk(x)?_n():x; if(xt!=1){cd(x);R TE;}a=*kI(x);cd(x);i=0;while(++i<n&&!bk(v[i])); if(i>=n)break;SW(r){CSR(5,)CS(6,if(a)cd(ex0(v+i+1,0,0)))CS(7,DO2(a,cd(ex0(v+i+1,0,0))))}}while(6==r && a); R _n())
    CD:z=newK(0,n?e:0); if(n)for(i=n-1;i>=-1;i--)if(-1==i||bk(v[i])){x=ex1(v+1+i,0);M(x,z) kK(z)[--e]=bk(x)?2==r?0:_n():x;}// (c:9;a+b;c:1) oom
  }

  //Note on brackets: [] is _n, not (). Expression [1;1] (0-type with two atoms) is different from [1 1] (integer vector)

  if(1==r)z=collapse(z); 
  if(k)
  {
    I j=valence(&z);
    if(!j && 0==k->t) DO(k->n,if(!kK(k)[i])kK(k)[i]=_n()) //Fill in 0-type NULLs with Kn()

    if(z->t!=7 ||z->n!=1||(j<k->n && !(0==j && k->n==1))) { x=vf_ex(&z,k); cd(z); R z=x;} //(0==j untested) project if necessary, reuse vf_ex.
    else // checking if looks like f'[] or f/[] or ...
    {
      K p = kV(z)[CODE];
      I i=p->n-2; //not the terminating NULL, but the entry before
      V*q=(V*) kK(p)+i;

      if(k->n >1 && !sva(*q) && adverbClass(*q) )
      {
        x=bv_ex(q,k);
        cd(z);
        R x;
      }
      /////////////////////////
      x=vf_ex(&z,k); cd(z); z=x; //copy/paste
      /////////////////////////
    }
  } 

  R z;
}

K bv_ex(V*p,K k)
{
  V q=*(V*)*p;
  K x;

  //assert 0!=k->n
  //assert k==b->n (otherwise, projection/VE, which shouldn't reach here)
  I n=0;
  if(over==q)
  {
    DO(k->n-1, x=kK(k)[i+1]; if(!x->n)R ci(*kK(k)); if(!atomI(x))if(n&&n!=x->n)R LE;else n=x->n) //return x_0 if any empty list x_{i>0}
    n=MAX(1,n);//if nothing was a list set to 1
    K z=ci(*kK(k));
    K g=newK(0,k->n);
    M(z,g);
    DO(n,*kK(g)=z; DO2(g->n-1, x=itemAtIndex(kK(k)[j+1],i); M(x,z,g) kK(g)[j+1]=x;)
         x=bv_ex(p-1,g); M(x,z,g) DO2(g->n, cd(kK(g)[j]); kK(g)[j]=0 ) //set to 0 in case OOM happens
         z=x) 
    cd(g);
    R z;
  }

  if(scan==q)
  {
    DO(k->n-1, x=kK(k)[i+1]; if(!x->n)R ci(*kK(k)); if(!atomI(x))if(n&&n!=x->n)R LE;else n=x->n) //return x_0 if any empty list x_{i>0}
    if(!n) R bv_ex(p-1,k); //  {x+y+z}\[1;1;1] yields 1 but {x+y+z}\[1;1;1 1] yields (1 1;3 3;5 5)  
    n=MAX(1,n);//if nothing was a list set to 1
    K z=newK(0,1); 
    K g=newK(0,k->n);
    M(z,g);
    kK(z)[0]=ci(*kK(k));
    DO(n,*kK(g)=ci(kK(z)[z->n-1]); DO2(g->n-1, x=itemAtIndex(kK(k)[j+1],i); M(x,z,g) kK(g)[j+1]=x;)
         x=bv_ex(p-1,g); M(x,z,g) DO2(g->n, cd(kK(g)[j]); kK(g)[j]=0 ) //set to 0 in case OOM happens
         kap(&z,x); cd(x);) 
    cd(g);
    z=collapse(z); //unnecessary?
    R z;
  }

  if(each==q)
  {
    DO(k->n, x=kK(k)[i]; if(!x->n)R newK(0,0); if(!atomI(x))if(n&&n!=x->n)R LE;else n=x->n) //return () on any empty list
    n=MAX(1,n);//if nothing was a list set to 1
    K z=newK(0,n), g=newK(0,k->n); M(g,z)//break [;;...] into subpieces for f, store in g
    DO(n, K x; DO2(k->n, x=itemAtIndex(kK(k)[j],i); M(x,g,z) kK(g)[j]=x) x=bv_ex(p-1,g); M(x,z,g) kK(z)[i]=x; DO2(k->n, cd(kK(g)[j]); kK(g)[j]=0))//sic =0
    cd(g);
    z=collapse(z);
    R z;
  }

  if(eachright==q) R NYI;//todo: is this reachable?
  if(eachleft ==q) R NYI;//todo: is this reachable?
  if(eachpair ==q) R NYI;//todo: is this reachable?

  R vf_ex(*p,k);
}


K ex1(V*w,K k)//convert verb pieces (eg 1+/) to seven-types, default to ex2 (full pieces in between semicolons/newlines) 
{
  if(in(*w,adverbs))R NYI;//Adverb at beginning of snippet eg '1 2 3 or ;':1 2 3; or 4;\1+1;4
  I c=0; while(w[c] && !bk(w[c])){c++; if(addressColon==w[c-1])break;} //must break or assignment is n^2  (a:b:c:1)

  if(!c || !VA(w[c-1]) || (c>1 && addressColon==w[c-1] ) ) R ex2(w,k); //typical list for execution

  //K3.2 crash bug: ."1",123456#"+"
  // build a 7type1 from the words if they end in a verb or adverb
  //Note: A returned +7type1 can never have a bk (; or \n) in it
  //? May be able to grab verb list by ignoring colon (: assignment) and whatever immediately precedes it ?  (K3.2  1+|+a:-+ is 1+|+-+ )
  //grab things like 1+/ from the middle of wordlists eg (;1+/;)
  K a = Kv(), kb = newK(-4,1+c); M(a,kb)
  V*b = (V*)kK(kb);
  b[c]=0; //sic (why sic?)
  DO(c, I j=c-i-1; //counting down
        b[j]=w[j]; 
        if(VA(b[j])) continue; //partially copy pasted from clone(). This pattern occurs here, in clone(), at the end of capture(), and in capture's BRACKET handler
        K r = ex_(w[j],1); //oom
        V q=newE(LS,r); //oom
        kap((K*) kV(a)+LOCALS,q);//oom
        cd(q); //kap does ci
        q=EVP(q); //oom free z etc. kap needs checking 
        b[j]=q;
  )
  kV(a)[CODE] = kb;
  R a;
}
K ex2(V*v, K k)  //execute words --- all returns must be Ks. v: word list, k: conjunction?
{
  K t0,t2,t3,e,u;
  I i=0;

  //TODO: is this messed up ......we can't index like this for (|-+) ?? what about 0-NULL []
  //ci(k) was R 0; ...  put this here for f/[x;y;z]
  if(!v || !*v)R k?(1==k->n)?ci(kK(k)[0]):ci(k):(K)ends; //? '1 + _n' -> domain err, '1 +' -> 1+ . but '4: . ""' -> 6 

  if(bk(*v)) R *v;  // ; case

  if(!v[1] && !k){ R ex_(*v,1); }  // n case
  if(!v[1] && sva(*v)){ R vf_ex(*v,k);}  //TODO: (,/:) and (,\:) both valence 2  //vf_ex must handle adverb and work backwards
  //TODO: brackets may also appear as:     +/\/\[]    {x}/\/\[]    a/\/\[]    (!200)\\[10;20]   
  if(bk(v[1])) R ex_(*v,1);

  if(!VA(*v) && (addressColon == v[1] || (VA(v[1]) && addressColon==v[2]) ) ) //Handle assignment
  {
    K a=0,b=0,c=0,d=0;
    K*w=*v;
    a=*w;
    if(7==a->t && 0==a->n && (b=kV(a)[CONJ]) && 7==b->t && 0==b->n ) 
    {
      U(b=ex_(kV(a)+CONJ,2))
      w=*kW(a); //K temp=a;  //a=ci(*kW(a)); w=*kW(a); cd(temp);
    }
    if(!b)U(b=newK(0,0))
    c=Kv(); //mmo  Optimization: could use A struct instead, with array[] for CODE
    K kc=newK(-4,2); //assumes NULL terminating entry
    M(b,c,kc);
    kV(c)[CODE]=kc;
    *kW(c) = v[1]; //it's v[1] regardless of colon position

    if(1!=sva(v[1])){d=ex1(v+(addressColon==v[1]?2:3),k); }   // oom -- except it's ok for d to be 0 elsewhere
    d=bk(d)?0:d;
  
    K h=dot_tetradic_2(w,b,c,d);
    cd(c); cd(d); M(b,h)
    K j=of(h,b); 
    cd(b);
    R j;
  }

  while(v[1] && adverbClass(v[2+i])) i++;
  //TODO: Catch 0-returned-errors here and below
  if(!sva(v[0]) && (i || 2==sva(v[1])))   // na+. or nv. case  (n noun, a adverb, + means regex one+ and . means regex anything )
  {
    t2=ex2(v+2+i,k); //these cannot be placed into single function call b/c order of eval is unspecified
    t3=ex_(v[1],1);
      //if(v[1]!=t3) if(!VA(t3)) show(t3);//for use with below
      u=v[1]; //This u thing fixes repeated use of 7-1 subparen like f:|/0(0|+)\;f a;f b;.  Not thread-safe. Adding ex_ result to LOCALS on 7-1 is probably better. See below
    v[1]=VA(t3)?t3:(V)&t3;
    t0=ex_(*v,1);
    e= dv_ex(t0,v+1+i,t2); v[1]=u;
    cd(t0); cd(t2); if(!VA(t3)) cd(t3);
    R e; 
  }

  //vn. case
  i=0; while(adverbClass(v[1+i])) i++; //ALT'Y: i=adverbClass(b)?i+1:0;
  t2=ex2(v+1+i,k); //oom. these cannot be placed into single function call b/c order of eval is unspecified
  t3=ex_(*v,1);
    u=*v; //Fixes a bug, see above. Not thread-safe. Adding to LOCALS probably better
  *v=VA(t3)?t3:(V)&t3;
  e=dv_ex(0,v+i,t2); *v=u;
  cd(t2); if(!VA(t3)) cd(t3);
  R e; 
}

I randomBits(){I s;I f=open("/dev/urandom",0);read(f,&s,sizeof(s));close(f);R s;} //lfop
void seedPRNG(I s){SEED=s?s:randomBits(); init_genrand64(SEED);}

I prompt(I n){DO(n,O(">")) O("  ");fflush(stdout);}
I lines(FILE*f) {S a=0;I n=0;PDA p=0; while(-1!=line(f,&a,&n,&p));}//You could put lines(stdin) in main() to have not-multiplexed command-line-only input
I line(FILE*f, S*a, I*n, PDA*p) // just starting or just executed: *a=*n=*p=0,  intermediate is non-zero
{
  S s=0; I b=0,c=0,m=0;
  K k; F d;

  I o = isatty(STDIN) && f==stdin; //display results to stdout?

  if(-1==(c=getline(&s,&m,f))) GC;
  appender(a,n,s,c);//"strcat"(a,s)
  I v=complete(*a,*n,p,0); //will allocate if p is null
  b=parsedepth(*p);
  if(v==3){show(kerr("nest")); GC;} 
  if(v==2){show(kerr("unmatched")); GC;}
  if(v==1) goto done;//generally incomplete
  if(n && '\n'==(*a)[*n-1])(*a)[--*n]=0; //chop for getline
  RTIME(d,k=ex(wd(*a,*n)))
#ifdef DEBUG
  if(o&&k)O("Elapsed: %.7f\n",d);
#endif
  if(o)show(k);
  cd(k);
cleanup:
  if(*p)pdafree(*p);*p=0;
  if(*a)free(*a);*a=0;*n=0;
  if(s)free(s);s=0;
done:
  if(o)prompt(b); 
  R c;
}

void *get_in_addr(struct sockaddr *sa) { if (sa->sa_family == AF_INET) R &(((struct sockaddr_in*)sa)->sin_addr);R  &(((struct sockaddr_in6*)sa)->sin6_addr); } // get sockaddr, IPv4 or IPv6

M0 CP[FD_SETSIZE]; //Connection Pool (large array)

I wipe_tape(I i) { if(CP[i].k)cd(CP[i].k); memset(&CP[i],0,sizeof(CP[0])); } //safe to call >1 time
I close_tape(I i) { wipe_tape(i); close(i); FD_CLR(i, &master); }

K read_tape(I i, I type) // type in {0,1} -> {select loop, 4: resp reader}
{
  I c=CP[i].r, m=sizeof(M1),g; K z=0;
  S b = c<m?c+(S)&CP[i].m1:c+kC(CP[i].k); 
  g = c<m?m-c:CP[i].m1.n; 
  I nbytes = recv(i,b,g,0); 
  if(nbytes <= 0)
  {
    if (nbytes == 0);//printf("server: socket %ld hung up\n", i);
    else perror("recv");
    GC;
  }
  //fill struct data + k data 
  CP[i].r += nbytes; //DO(nbytes, O("b%ld : %o\n",i,(UC)b[i]))
  if(m == CP[i].r) //We've read enough bytes to fill our struct m1 with transmission data (it's also the _db header)
  {
    //TODO: so that we get the right sizes, etc, in the M1, rearrange bytes based on little-endianness indicator CP[i].m1.a
    //if(sizeof(M1)+CP[i].m1.n > 987654321) GC; //protect against too big?
    K k = newK(-3, m+CP[i].m1.n);
    if(!(CP[i].k=k))GC;
    memcpy(kC(k),&CP[i].m1,m); //cpy data from our struct to the corresponding spot on the '_bd' object
  }
  if(CP[i].r == m + CP[i].m1.n) //the k for the _db is completed. perform modified execution, potentially respond
  {
    //TODO: (here or in _db?) rearrange bytes based on little-endianness indicator CP[i].m1.a
    M1*p=(V)kC(CP[i].k);
    I msg_type = p->d; //p->d dissappears after wipe_tape
    K h = _db(CP[i].k);
    if(!h) GC;
    wipe_tape(i);

    //blocking read inside 4: receives response //response sent by server to client after a 4: request is not executed by client
    if(2==msg_type && 1==type) R h; 

    //Modified execution of received K value. First received transmission in a 3: or 4: 
    z=modified_execute(h);
    cd(h);
    //indicates received communication from 4: synchronous method which expects response
    if(z) if(1==msg_type && 0==type) ksender(i,z,2);
    cd(z); z=0;
  }

  R z;
cleanup:
  close_tape(i);
  R (K)-1;
}

K modified_execute(K x) //TODO: consider: this should be modified to use error trap. _4d should be modified to expect error trap output. 
{
  //K-Lite manual gives {:[4:x; .x; .[.;x]} as processing function
  if(4==xt || 3==ABS(xt)) R X(CSK(x));
  if(!xt && xn>0) R vf_ex(addressDot,x);
  R ci(x);
}


fd_set master; //select framework after beej's public domain c
I attend() //K3.2 uses fcntl somewhere
{
  S a=0;I n=0; PDA q=0; //command-line processing variables

  fd_set read_fds;
  int fdmax=STDIN;
  int listener=0;
  int newfd; //newly accepted socket descriptor
  struct sockaddr_storage remoteaddr; // client address
  socklen_t addrlen;

  int nbytes;
  char remoteIP[INET6_ADDRSTRLEN];
  I yes=1;	// for setsockopt() SO_REUSEADDR, below 
  int i, j, rv;
  struct addrinfo hints, *ai, *p;
  FD_ZERO(&master);	// clear the master and temp sets
  FD_ZERO(&read_fds);

  // get us a socket and bind it 
  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC; 
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;

  FD_SET(STDIN, &master);

  //TODO: do we need SO_KEEPALIVE or SO_LINGER

  if(PORT)
  {
    if ((rv = getaddrinfo(NULL, PORT, &hints, &ai)) != 0) { fprintf(stderr, "server: %s\n", gai_strerror(rv)); exit(1); }
    for(p = ai; p != NULL; p = p->ai_next)
    {
      listener = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
      if (listener < 0) continue;
      // lose the "address already in use" error message 
#if defined(__MACH__) && defined(__APPLE__) || defined(__FreeBSD__)
      setsockopt(listener, SOL_SOCKET, SO_REUSEADDR | SO_NOSIGPIPE , &yes, sizeof(I)); 
#endif
      if (bind(listener, p->ai_addr, p->ai_addrlen) < 0) { close(listener); continue; }
      break;
    }
    //K3.2 k aborts/exits if port is in use. k -i 1234. OK.  k -i 1234 ->  "i\nabort\n" exit;
    if (p == NULL) { fprintf(stderr, "server: failed to bind\n"); exit(2); } 
    freeaddrinfo(ai); 
    if (listen(listener, 10) == -1) { perror("listen"); exit(3); }
    FD_SET(listener, &master);
    fdmax = listener; 
  }


  for(;;) // main loop  
  {
    read_fds = master; // copy it 
    if (-1==select(fdmax+1,&read_fds,0,0,0)){perror("select");exit(4);} //null timeval -> select blocks
    // run through the existing connections looking for data to read 
    for(i = 0; i <= fdmax; i++) 
      if (FD_ISSET(i, &read_fds))
        if(i==STDIN)
        {
          nbytes=line(stdin,&a,&n,&q);
          if(nbytes<=0)
            if(!PORT) exit(0); //Catch CTRL+D 
            else FD_CLR(i,&master); 
        }
        else if(i == listener) // handle new connections 
        {
          addrlen = sizeof remoteaddr; 
          newfd = accept(listener, (struct sockaddr *)&remoteaddr, &addrlen);
          if (newfd == -1) perror("accept"); 
          else
          {
            wipe_tape(newfd); //new conn needs this since connections can die without notification (right?)
            FD_SET(newfd, &master); // add to master set 
            if (newfd > fdmax) fdmax = newfd;
            setsockopt(newfd, IPPROTO_TCP, TCP_NODELAY, &yes, sizeof(I)); //disable nagle
            //printf("server: new connection from %s on socket %d\n", inet_ntop(remoteaddr.ss_family, get_in_addr((struct sockaddr*)&remoteaddr), remoteIP, INET6_ADDRSTRLEN), newfd);
          }
        } 
        else if(a) continue; //K3.2 blocks if in the middle of processing the command-line (should we sleep here?)
        else read_tape(i,0);
  }

}

I args(int n,S*v)
{
  I c;
  while(-1!=(c=getopt(n,v,":h:i:")))SW(c)
  {
    CS('h',O("%d\n", atoi(optarg)))
    CS('i',PORT=optarg)
    CSR(':',)CS('?', O("%c ",optopt); show(kerr("opt")))
  }
  while(optind < n) load(v[optind++]);
}

K KFIXED;

I kinit() //oom (return bad)
{
  atexit(finally);
#define SETLEN(x) {I i; for(i=0; x[i]; i++)  x##ct = i+1; }
  SETLEN(vn_); SETLEN(vm_); SETLEN(vd_); SETLEN(vt_); 
  addressSSR  = vt_ + 0;
  addressWhat = vd+charpos(vc,'?'); addressAt    = vd+charpos(vc,'@');
  addressDot  = vd+charpos(vc,'.'); addressColon = vd+charpos(vc,':');
  kerr("undescribed");//initialize errmsg string to be non-null for more useful reporting
  SYMBOLS=newN(); //Initialize intern pool 
  seedPRNG(randomBits()); 
  KFIXED=newK(0,0);
  kap(&KFIXED,NIL=Kn());cd(NIL);
  __d = sp(".k"); LS=sp(""); DO(3,IFP[i]=sp(IFS[i]))
#ifdef DEBUG
  test();
#endif
  KTREE=Kd();//Initalize. Alt, KTREE=_(.,(`k;));
  K x;
  kap(&KTREE,x=newEntry(sp("k"))); cd(x);
  kap(&KTREE,x=newE(sp("t"),_dot_t())); cd(x);

  R 0;
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

