/* execution */

#include "incs.h"

#include "k.h"
#include "kc.h"
#include "ko.h"
#include "kx.h"
#include "km.h"
#include "v.h"

Z K bv_ex(V *p,K k);
Z K dv_ex(K a,V *p,K b);
Z K ex0(V *v,K k,I r);
Z K ex2(V *v,K k);
Z V ex_(V a,I r);
I cirRef(K p,K y);
I cirRef_(K p,K y,I f);

__thread I fer=0;    // Flag Early Return 
__thread I fwh=0;    // Flag While
__thread I stk=0;    // Stack counter
__thread I stk1=0;   // Additional stack counter
__thread I prj=0;    // Projection flag
__thread I prj2=0;   // 2nd Projection flag
__thread K prnt=0;   // Parent of Subfunction 
__thread I f1s=1;    // Flag 1 for Subfunctions
__thread I f2s=0;    // Flag 2 for Subfunctions
__thread K grnt=0;   // GrandParent of Subfunction
__thread K encf=0;   // Enclosing Function
__thread I encp=0;   // Enclosing Function Param
__thread I frg=0;    // Flag reset globals
         S fnc=0;    // Most recent function from Dispatch Table
         V fncp[128];// DT pointers of executed functions
         I fnci=0;   // indicator of next function pointer position

//TODO: for derived verbs like +/ you can add the sub-pieces in parallel
Z K overDyad(K a, V *p, K b)
{

  V *o=p-1; K(*f)(K,K); 

  K k=0;
  if(VA(*o) && (f=DT[(L)*o].alt_funcs.verb_over))k=f(a,b); //k==0 just means not handled. Errors are not set to come from alt_funcs
  P(k,k)

  K u=0,v=0;
  K y=a?v=join(u=enlist(a),b):b; //oom u (TODO: need to unroll to 'x f/y' and 'f/y' to optimize?)
  K z=0,g=0;
  if(yt  > 0){z=ci(y); GC;}
  if(yn == 0){if(VA(*o))z=LE; GC; } //Some verbs will handle this in alt_funcs
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

Z K scanDyad(K a, V *p, K b) //k4 has 1 +\ 2 3 yield 3 6 instead of 1 3 6
{
  V *o=p-1; K(*f)(K,K); 

  K k=0;
  if(VA(*o) && (f=DT[(L)*o].alt_funcs.verb_scan))k=f(a,b); //k==0 just means not handled. Errors are not set to come from alt_funcs
  P(k,k)

  K u=0; K y=a?join(u=enlist(a),b):ci(b); cd(u); //oom
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

Z K overMonad(K a, V *p, K b)
{
  K u=b,c=0;I flag=0;

  I useN=0,n=0,useB=0;
  if(a) {if(1 == a->t){useN=1; n=*kI(a);} else if(7==a->t){useB=1;}}
  P(n<0,IE)

  if(useN) //n f/x
  {
    I f=0;
    DO(n, c=dv_ex(0,p-1,u); if(b!=u)cd(u); if(f && b==c)cd(c); f=1; U(u=c))
    c=c?c:ci(b);
  }
  else if(useB) // b f/x
  {
    I t;
    do
    {
      K*aa=&a;
      K g=dv_ex(0,(V)&aa,u); U(g)
      t=(g->t==1 && *kI(g));
      cd(g);
      if(!t)break;
      c=dv_ex(0,p-1,u); if(b!=u)cd(u); U(u=c)
    }while(1);
    c=c?c:ci(b);
  }
  else while(1) // f/x
  {
    if(matchI(b,c) || (u!=b && matchI(u,c)))flag=1;
    if(u!=b) cd(u);
    if(flag)break;
    u=c?c:u;
    U(c=dv_ex(0,p-1,u))
    if(3==ABS(c->t)) flag=1;
  }
  R c;
}

Z K scanMonad(K a, V *p, K b)
{
  K u=enlist(b),v,w,c=0,d;I flag=0;//TODO: optimize/memory manage enlists,firsts,reverses here
  U(u);

  I useN=0,n=0,useB=0;
  if(a) {if(1 == a->t){useN=1; n=*kI(a);}else if(7==a->t)useB=1;}
  P(n < 0,IE) //mmo

  if(useN) DO(n, U(v=reverse(u)) d=first(v); cd(v); c=dv_ex(0,p-1,d); cd(d); U(c) U(v=enlist(c)) cd(c); u=join(w=u,v); cd(w); cd(v); U(u))
  else if(useB)
  {
    I t;
    do
    {
      U(v=reverse(u))
      d=first(v); cd(v);
      K*aa=&a;
      K g=dv_ex(0,(V)&aa,d); U(g)
      t=(1==g->t && *kI(g));
      cd(g);
      if(!t){cd(d); break;}
      c=dv_ex(0,p-1,d); cd(d);
      U(c) U(v=enlist(c)) cd(c);
      u=join(w=u,v); cd(w); cd(v); U(u)
    }while(1);
  }
  else while(1) //mm/o + error checking   eg if(!c) ... 
  {
    d=first(v=reverse(u));cd(v);
    if(matchI(b,c) || matchI(c,d))flag=1;
    if(!flag && c)
    {
      u=join(v=u,w=enlist(c));
      cd(v);cd(w);cd(d);
      d=c;
    }
    if(interrupted){interrupted=0;R BE;}
    if(flag){cd(c);cd(d);break;}
    c=dv_ex(0,p-1,d);cd(d);
    if(!c){cd(u); R c;}
  }
  R u;
}

Z K each2(K a, V *p, K b)
{
  I bt=b->t, bn=b->n;
  if(bt > 0) R dv_ex(0,p-1,b);
  else
  {
    K z = newK(0,bn),d=0; U(z)
    K g;
    if(0 >bt) DO(bn, g=newK(ABS(bt),1); M(g,z) memcpy(g->k,((V)b->k)+i*bp(bt),bp(bt)); d=dv_ex(0,p-1,g); cd(g); M(d,z) kK(z)[i]=d)
    if(0==bt) DO(bn, d=dv_ex(0,p-1,kK(b)[i]); if(grnt && !prnt)prnt=grnt; M(d,z) kK(z)[i]=d)
    R demote(z);
  }
}

Z K eachright2(K a, V *p, K b)
{
  I bt=b->t, bn=b->n;
  if(bt > 0) R dv_ex(a,p-1,b);
  K z = newK(0,bn), d;
  K g;
  if(0 >bt) DO(bn, g=newK(ABS(bt),1); memcpy(g->k,((V)b->k)+i*bp(bt),bp(bt)); d=dv_ex(a,p-1,g); cd(g); U(d) kK(z)[i]=d) //TODO: err/mmo oom-g
  if(0==bt) DO(bn, d=dv_ex(a,p-1,kK(b)[i]); U(d) kK(z)[i]=d)
  R demote(z);
}

Z K eachleft2(K a, V *p, K b)
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

Z K eachpair2(K a, V *p, K b)  //2==k necessary?
{
  V *o=p-1; K(*f)(K,K); 

  K k=0;
  if(VA(*o) && (f=DT[(L)*o].alt_funcs.verb_eachpair))k=f(a,b); //k==0 just means not handled. Errors are not set to come from alt_funcs
  P(k,k)

  I bt=b->t, bn=b->n;
  if(bt >  0) R dv_ex(a,p-1,b);
  if(bt <= 0)
  {
    if     (bn == 0 && !a) R LE;
    else if(bn == 0 &&  a) R newK(0,0);//TODO: memory manage/ optimize in join with null ptr ?
    else if(bn < 2) R newK(0,0);//TODO: this newK and the above.....does empty list type depend on input?
  }

  K z = newK(0,bn-1),d=0; U(z)
  K g,h;
  if(0 >bt)DO(bn-1, h=newK(ABS(bt),1); g=newK(ABS(bt),1); memcpy(h->k,((V)b->k)+(i)*bp(bt),bp(bt)); memcpy(g->k,((V)b->k)+(i+1)*bp(bt),bp(bt)); d=dv_ex(g,p-1,h); cd(g);cd(h);U(d) kK(z)[i]=d) //TODO: err/mmo - cd(z) - oom-g-h
  if(0==bt)DO(bn-1, d=dv_ex(kK(b)[i+1],p-1,kK(b)[i]); U(d) kK(z)[i]=d) //TODO: err/mmo - cd(z)

  z=demote(z);

  if(a)
  {
    K u,v,f,d;
    f=first(b);
    d=dv_ex(a,p-1,f);
    u=enlist(d);
    M(u,z)
    v=join(u,z);
    cd(u);cd(z);cd(f);cd(d);
    R v;
  }

  R z;
}

//TODO: Try (?) and grow adverb results as vectors before devolving to 0-type
//TODO: consider merging dv_ex with vf_ex
Z K dv_ex(K a, V *p, K b)
{
  if(!p || !*p) R 0; //TODO: ???
  U(b)
  V *o = p-1;

  //Arity of V?A_1...A_n-1 for X V?A_1...A_n Y; 0 for X Y, X A Y
  I k=adverbClass(*p)?adverbClass(*o)?1:sva(*o):sva(*p);
  k=adverbClass(*p)?adverbClass(*o)?1:valence(*o):valence(*p); //also t7 basic

  V adverb=*p; //TODO: Implement adverb "Error Reports" error checking from manual

  //k>2 --- ??? bound for special verbs ?.@ , etc.  ??? k=2 ??? valence is weird here
  //!(adver...  ---- added to let f/[;;;] through
  //if(k>2 && !(adverbClass(*p) && !VA(*o)))k=2;
  if(k>2)k=2;

  if(2==k)
  {
    if ((L)adverb == offsetOver) R overDyad(a, p, b);
    if ((L)adverb == offsetScan) R scanDyad(a, p, b);
    if ((L)adverb == offsetEach)
      {
      if(!a) adverb = (V)offsetEachright;
      else if(a->t <= 0 && b->t <= 0 && a->n != b->n) R LE;
      else if(a->t > 0 && b->t > 0) R dv_ex(a,p-1,b);
      else if (a->t > 0) adverb = (V)offsetEachright;
      else if(b->t > 0) adverb = (V)offsetEachleft;
      else
      {
        //a and b both lists/vectors of size an
        a=promote(a);
        b=promote(b);
        M(a,b)
        K z = newK(0,a->n);
        M(z,a,b)
        K k;
        DO(a->n, k=dv_ex(kK(a)[i],p-1,kK(b)[i]); M(k,z,a,b) kK(z)[i]=k)
        cd(a);
        cd(b);
        R demote(z);
      }
    }
  } else if(2 > k)
  {
    if ((L)adverb == offsetOver) R overMonad(a, p, b);
    if ((L)adverb == offsetScan) R scanMonad(a, p, b);
    if ((L)adverb == offsetEach) R each2(a, p, b);
  }

  if((L)adverb == offsetEachright) R eachright2(a, p, b);
  if((L)adverb == offsetEachleft) R eachleft2(a, p, b);
  if((L)adverb == offsetEachpair) R eachpair2(a, p, b);

  //this could be better ??
  I gn=0;
  if      (valence(*p)>=2 && a && b) gn=2;
  else if (a) R VE;  //?
  else if (b) gn=1;

  K g=newK(0,gn);U(g);
  if(gn > 1) kK(g)[1]=b;
  if(gn > 0) kK(g)[0]=a?a:b;

  K tmp; I flag=0;
  if(*p>(V)DT_SIZE && b->n){
    V*p1=*p;
    if(*p1>(V)DT_SIZE){
      K p2=*p1;
      if(7!=p2->t && -1!=p2->t) flag=1;
    }
  }
  if(flag) tmp=vf_ex(*p,b); 
  else{
    if(stk>2e6) R kerr("stack"); stk++;
    if(encp && (encp!=2 || (strchr(kC(kK(encf)[CODE]),"z"[0]))) && encp!=3 && (K*)DT_SIZE<(K*)*p)tmp=vf_ex(&encf,g);
    else tmp=vf_ex(*p,g);
    stk--; if(grnt && !prnt) prnt=grnt;
  }

  memset(kK(g),0,g->n*sizeof(K)); cd(g); //Special privileges here...don't ci() members beforehand
  R tmp;
}

//1. Use PARAMETER list (or XYZ tuple) to merge CONJ and ARGS-G into LOCAL-DICT-TREE
//2. Execute as normal, except
//   a. The LOCAL-DICT-TREE acts as the "KTREE"
//   b. Double-colon assignment :: adds to the dictionary in CONTeXT

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
//  X10  {  :x  } early return
//  X11  Reusably compiled

//For -7 (7-0) CONJ is unexecuted brackets. For 7-{1,2,3} it's 0-type with NULLs
//K3.2 Bug - {b:1_,/";a",/:$a:!x; "{[",b,"]a3}[" ,(1_,/";",/:$a ),"]" } 67890  --> Sometimes works, sometimes stack error, sometimes crash
K vf_ex(V q, K g) 
{
  if (interrupted) {interrupted=0; R BE;}

  //V w=(*(V*)q);

  if(!g)R 0; //??? R w converted to type7...or ?
  K z=0;
  U(g=promote(g))
  I gn=g->n;

  I k=sva(q);
  I n=-1,j=0;
  if(!k&&!(*(V*)q)){cd(g); R 0;}// (2="2") 2 err
  if(( k || ((K)(*(V*)q))->t==7) && ( ((L)q<DT_SIZE || (*(V*)q))  && gn > (n=valence(q)) && !(!n && 1>=gn))){VE; GC;} 
    //could remove 1>=gn condition ?
  I argc=0; DO(gn,if(kK(g)[i])argc++)

  K a=0,b=0,c=0,d=0;
  if(gn >0) a=kK(g)[0]; if(gn >1) b=kK(g)[1]; if(gn >2) c=kK(g)[2]; if(gn >3) d=kK(g)[3];


  //valence overloaded verbs 
  if(gn > 2 && (q==offsetWhat || q==offsetSSR)){ z=(q==offsetWhat?what_triadic:_ssr)(a,b,c); GC; }
  if(gn > 2 && (q==offsetAt   || q==offsetDot )){ z= (q==offsetAt?at_tetradic:dot_tetradic)(a,b,c,d); GC;}
  //common verbs

  if(2==k && a && b){ fnc=DT[(L)q].text; 
    if(fnci<127){fncp[fnci]=q; fnci++;} 
    z=((K(*)(K,K))DT[(L)q].func)(a,b); GC;}
  //? (+).1 -> err ; {[a;b]a+b} 1 -> err
  if(2==k && !a){VE; GC;} //Reachable? Projection?

  //Reachable: try "#'(1;1 2)" (the # is dyadic not monadic #:). We return projection (#[1;],#[1 2;]), K3.2 gives valence error
  if((2==k || q==offsetSSR) && !b)
  { K v = Kv(), kb = newK(-4,2); M(v,kb)
    kK(kb)[0]=q; 
    kK(kb)[1]=0;
    kV(v)[CODE] = kb;
    z = vf_ex(&v,g); //Punt and let another call to vf_ex handle projecting. Probably could build the projected-verb here instead.
    cd(v);
    GC;
  } //old comment: Projection? '(1+)' -> 1+  Build 7-verb? (Refactor with 'c' from []+: ex and maybe another place?)

  //+:[a] ... +:[a;b] handled above (valence err)
  if(1==k && a) { z= ((K(*)(K))DT[(L)q].func)(a); GC;}
  if(1==k && !a) GC; //Reachable? Projection?
  //Functions  7-{1,2,3}
  K f = (K) (*(V*)q); I ft=f->t;

  if(ft != 7){z=g?dot(f,g):f; GC;}//TODO: check this for !a and for dict. ternary is superfluous since g nonzero?
  I t=f->n;
  if(-1==n)n=valence(f); //don't compute twice

  //Projecting simple verbs works. The ex 7-type wrapper will catch simple verbs and they will make it back here. (except in above 2==k && a && !b case?)
  K o=kV(f)[CODE]; K p=kV(f)[PARAMS]; K s=kV(f)[LOCALS]; K r=kV(f)[CONJ]; 
  I special = 1==t && !r && (offsetAt==*kW(f) || offsetDot==*kW(f) || offsetWhat==*kW(f)); //_ssr is not special (not overloaded)

  if(o->t!=-3){
    I ii=o->n-2; //not the terminating NULL, but the entry before
    V*u=(V*) kK(o)+ii;
    if(2==n && 1==adverbClass(*u) ) n=gn; //   / \ '  but maybe should exclude '
  }

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
      if(prj){V*w=&kW(f)[1]; z=bv_ex(w,m);}
      else z=ex2(kW(f),m); 
      cd(m);
    )
    CS(2, //Executing a dynamically loaded library function from 2:
      v=kW(f)[1];
      K a[7]; DO(7,a[i]=0) 
      if(r)memcpy(a,kK(r),MIN(r->n,7)*sizeof(V)); //MIN(.,7) is superfluous
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

      if(((L)kV(f)[DEPTH]) > 500){kerr("stack"); GC; }
      if(stk > 2e6){kerr("stack"); GC; }
      stk++;

      I j=0; K*e; K fw;

      if(!(tree=kV(f)[CACHE_TREE]))  //could merge this and and CACHE_WD check by duplicating the arg merge DO
      {
        tree=newK(5,p->n+s->n); if(!tree) {stk--; GC;} //note: cleanup is unusual -- could turn into double labels
        DO(tree->n, if(!(kK(tree)[i]=newK(0,3))){cd(tree); stk--; GC;}) //shallow dict copy -- dictionary entry pool?
        DO(tree->n, DO2(3,  kK(DI(tree,i))[j] = ci(kK((i<p->n?DI(p,i):DI(s,i-p->n)))[j])))//shallow copy
        kV(f)[CACHE_TREE]=tree;
      }

        if(f2s && prnt && kV(prnt)[LOCALS] && kV(prnt)[CACHE_TREE]){
        K j0=dot_monadic(kV(prnt)[LOCALS]); K j1=dot_monadic(kV(prnt)[CACHE_TREE]);
        K j2=join(j0,j1); cd(kV(prnt)[CACHE_TREE]); kV(prnt)[CACHE_TREE]=dot_monadic(j2);
        cd(j0); cd(j1); cd(j2); tree=kV(prnt)[CACHE_TREE]; 
        cd(kV(prnt)[CACHE_WD]); kV(prnt)[CACHE_WD]=0; 
      }

      DO(p->n,e=EVP(DI(tree,i)); cd(*e); *e=0; if(r && i<r->n) *e=ci(kK(r)[i]); if(!*e && j<g->n) *e=ci(kK(g)[j++])) //merge in: CONJ with function args

      fw=kV(f)[CACHE_WD]; I t=0;
      if(!fw || (t=(V)kS(kK(fw)[CODE])[0] || (K*)kS(kK(fw)[CODE])[1]>(K*)DT_SIZE) )
      {
        if(t) cd(kV(f)[CACHE_WD]);
        K fc = kclone(f); //clone the function to pass for _f
        cd(kV(fc)[CONJ]);kV(fc)[CONJ]=0;
        kV(fc)[DEPTH]++;
        fw = wd_(kC(o),o->n,&tree,fc);
        kV(f)[CACHE_WD]=fw;
        cd(fc);
      }

      #ifdef DEBUG
      if(stk1>5) {cd(g); kerr("stack"); R _n();}
      #else
      if(stk1>1e3) {cd(g); kerr("stack"); R _n();}
      #endif
      ci(fw); stk1++; z=ex(fw); stk1--;
      DO(p->n,e=EVP(DI(tree,i)); cd(*e); *e=0; )
      stk--;
    )
  }

  if(encp==2){      // Access the parameters of an enclosing function
    I ff=0;
    if(z && z->t==7 && z->n==3 && kV(z)[CODE] && strchr(kC(kK(z)[CODE]),"z"[0]) && kV(z)[PARAMS] && kK(z)[PARAMS]->n){
      ff=1; DO(kK(z)[PARAMS]->n, if(!strcmp(*kS(kK(kK(kK(z)[PARAMS])[i])[0]),"z")){ff=0; break;} )
    }  
    if(ff){
      K d=kK(kK(KTREE)[0])[1]; K w=0;
      DO(d->n, if(!strcmp(*kS(kK(kK(d)[i])[0]),"z")){w=kclone(kK(d)[i]); break;})
      if(w){
        K p=kK(g)[0]; cd(kK(w)[1]); kK(w)[1]=kclone(p); K we=enlist(w);
        K j0=dot_monadic(kK(z)[CACHE_TREE]); K j2=join(j0,we);
        cd(kK(z)[CACHE_TREE]); kK(z)[CACHE_TREE]=dot_monadic(j2);
        cd(w); cd(we); cd(j0); cd(j2); encp=3;
      }
    }
  }
  if(encp==1){
    I ff=0;
    if(z && z->t==7 && z->n==3 && kV(z)[CODE] && strchr(kC(kK(z)[CODE]),"y"[0]) && kV(z)[PARAMS] && kK(z)[PARAMS]->n){ 
      ff=1; DO(kK(z)[PARAMS]->n, if(!strcmp(*kS(kK(kK(kK(z)[PARAMS])[i])[0]),"y")){ff=0; break;} )
    }  
    if(ff){
      K d=kK(kK(KTREE)[0])[1]; K y=0;
      DO(d->n, if(!strcmp(*kS(kK(kK(d)[i])[0]),"y")){y=kclone(kK(d)[i]); break;})
      if(y){
        K p=kK(g)[0]; cd(kK(y)[1]); kK(y)[1]=kclone(p); K ye=enlist(y);
        K j0=dot_monadic(kK(z)[CACHE_TREE]); K j2=join(j0,ye);
        cd(kK(z)[CACHE_TREE]); kK(z)[CACHE_TREE]=dot_monadic(j2); 
        cd(y); cd(ye); cd(j0); cd(j2); encp=2;
      }
    }
  }
  if(encp==0){
    I ff=0;
    if(z && z->t==7 && z->n==3 && kV(z)[CODE] && strchr(kC(kK(z)[CODE]),"x"[0]) && kV(z)[PARAMS] && kK(z)[PARAMS]->n){
      ff=1; DO(kK(z)[PARAMS]->n, if(!strcmp(*kS(kK(kK(kK(z)[PARAMS])[i])[0]),"x")){ff=0; break;} )
    }  
    if(ff){
      K d=kK(kK(KTREE)[0])[1]; K x=0;
      DO(d->n, if(!strcmp(*kS(kK(kK(d)[i])[0]),"x")){x=kclone(kK(d)[i]); break;})
      if(x){
        K p=kK(g)[0]; cd(kK(x)[1]); kK(x)[1]=kclone(p); K xe=enlist(x);
        K j0=dot_monadic(kK(z)[CACHE_TREE]); K j2=join(j0,xe);
        cd(kK(z)[CACHE_TREE]); kK(z)[CACHE_TREE]=dot_monadic(j2); 
        cd(x); cd(xe); cd(j0); cd(j2); encp=1;
      }
    }
  }

cleanup:
  cd(g);
  R z;
}

//Could probably fold ex0 into this function
Z V ex_(V a, I r)//Expand wd()->7-0 types, expand and evaluate brackets
{
  K x,y=0,z,tmp;

  if(!a || VA(a) || bk(a)) R a;
  if(!(x=*(K*)a) || 7!=xt || (0<xn && xn<4)) R ci(x); //assert xn>=4 -> conditionals or similar

  r=xn<4?r:xn; //suggests maybe r should be stored on 7type itself

  if(kV(x)[CONJ])
  {
    if((tmp=*(K*)(kV(x)+CONJ))) if(offsetColon==*kW(tmp) && *(kW(tmp)+1)>(V)DT_SIZE)fer=1;
    y=ex_(kV(x)+CONJ,2); //Use 0-type with NULLS if passing to function
    U(y); 
    if(y->t == 0 && y->n==0){cd(y); y=_n();}
    if(fer) R y;
  }
  z=ex0(kW(x),y,r);  //eval wd()
  cd(y);

  R z;
}

K ex(K a) {   //Input is (usually, but not always) 7-0 type from wd()
  U(a); if(a->t==7 && kVC(a)>(K)DT_SIZE && 7==kVC(a)->t && 6==kVC(a)->n)fwh=1;
  K z=ex_(&a,0); cd(a); fer=fwh=stk=stk1=prj=prj2=f2s=0; f1s=1; if(prnt && encp==3){cd(prnt); prnt=0;} else prnt=0; R z; 
}

Z K ex0(V*v,K k,I r) //r: {0,1,2} -> {code, (code), [code]} Reverse execution/return multiple (paren not function or script) "list notation"  {4,5,6,7} -> {:,if,while,do}
{
  I n=0, e=1, i,a,b;
  while(v[n])if(bk(v[n++]))e++;
  b=e>1;

  K z=0, x;

  SW(r)
  {
    CS(0, for(i=-1;i<n;i++)if(-1==i||bk(v[i])){cd(z); frg++; x=ex1(v+1+i,0,&i,n,1); frg--; if(!frg){encp=0; if(encf){cd(encf); encf=0;} if(grnt){cd(grnt); grnt=0;}} U(x) z=bk(x)?_n():x; if(fer)R z;})//  c:9;a+b;c:1 
    CS(4, for(i=-1;i<n;i++)if(-1==i||bk(v[i])){U(x=ex1(v+1+i,0,&i,n,1)) if(fer)R x; x=bk(x)?_n():x; while(++i<n&&!bk(v[i])); if(i==n) R x; z=delist(x); if(ABS(z->t)!=1 || z->n!=1){cd(z);R TE;}a=*kI(z);cd(z); if(a){x=ex1(v+i+1,0,&i,n,1); R x=bk(x)?_n():x;} else while(i<n&&!bk(v[i]))i++; } R _n())
    CSR(5,)CSR(6,)CS(7, do{I i=0; U(x=ex1(v,0,&i,0,1)) if(fer)R x; x=bk(x)?_n():x; z=delist(x); if(ABS(z->t)!=1 || z->n!=1){cd(z);R TE;}a=*kI(z);cd(z);i=0;if(b){while(++i<n&&!bk(v[i])); if(i>=n)break;}SW(r){CSR(5,)CS(6,if(a&&b){x=ex0(v+i+1,0,0); if(fer)R x; cd(x);})CS(7,DO2(a, x=ex0(v+i+1,0,0); if(fer)R x; cd(x);))}}while(6==r && a); R _n())
    CD: z=newK(0,n?e:0); if(n)for(i=n-1;i>=-1;i--)if(-1==i||bk(v[i])){if(offsetColon==(v+1+i)[0] && (v+1+i)[1]>(V)DT_SIZE)fer=1; x=ex1(v+1+i,0,&i,n,0); if(fer){cd(z); R x;} M(x,z) kK(z)[--e]=bk(x)?2==r?0:_n():x;}// (c:9;a+b;c:1) oom
  }

  //Note on brackets: [] is _n, not (). Expression [1;1] (0-type with two atoms) is different from [1 1] (integer vector)

  if(1==r)z=collapse(z); 
  if(k)
  {
    I j=valence(&z);

    if(!j && 0==k->t) DO(k->n,if(!kK(k)[i])kK(k)[i]=_n()) //Fill in 0-type NULLs with _n

    if(z->t!=7 ||z->n!=1||(j<k->n && !(0==j && k->n==1))) {    //(0==j untested) project if necessary, reuse vf_ex.
      if(encf && (K*)DT_SIZE<(K*)&z)x=vf_ex(&encf,k);
      else x=vf_ex(&z,k);
      if(encp!=3)cd(z);
      R z=x;
    } 
    else // checking if looks like f'[] or f/[] or ...
    {
      K p = kV(z)[CODE];
      I i=p->n-2; //not the terminating NULL, but the entry before
      V*q=(V*) kK(p)+i;

      K t=0; if(k->n==1) t=first(k);
      if((k->n>1 || (t && t->n==1)) && !sva(*q) && adverbClass(*q))
      {
        if(k->n==1 && !prj2)k->n=2;     
        prj2=1;
        DO(k->n, if(!kK(k)[i])prj=1) 
        if(!prj) //***** could be the _n() <-> ;;; replacement above *****
        {
          x=bv_ex(q,k);
          cd(z);
          R x;
        }
      }
      cd(t);
      /////////////////////////
      if(z->t==7 && z->n==1 && kK(kK(z)[CODE])[0]==offsetSSR && k->t==0 && k->n==3 && ABS(kK(k)[2]->t)==3){
        K x=kK(k)[2];
        if(xn==1 && xt==3){K y=enlist(x); kK(k)[2]=enlist(y); cd(y);}
        else kK(k)[2]=enlist(x);
        cd(x);
      }
      x=vf_ex(&z,k); cd(z); z=x; //copy/paste
      /////////////////////////
    }
  } 

  R z;
}

Z K bv_ex(V*p,K k)
{
  V q=*p;
  K x;

  //assert 0!=k->n
  //assert k==b->n (otherwise, projection/VE, which shouldn't reach here)
  I n=0;

 
  
  //This block may contribute to bv_ex subtriadic problems
  if(!adverbClass(*p) && valence(*p) < 3)
  {
    if(k->n < 2) { R VE; }
    R dv_ex(kK(k)[0],p,kK(k)[1]);
  }

  if(offsetOver==(L)q)
  {
    DO(k->n-1, x=kK(k)[i+1]; if(!x->n)R ci(*kK(k)); if(!atomI(x)){if(n&&n!=x->n)R LE;else n=x->n)} //return x_0 if any empty list x_{i>0}
    n=MAX(1,n);//if nothing was a list set to 1
    K z=ci(*kK(k));
    K g=newK(0,k->n);
    M(z,g);
    DO(n, *kK(g)=z;
          DO2(g->n-1, x=itemAtIndex(kK(k)[j+1],i); M(x,g) kK(g)[j+1]=x;)
          x=bv_ex(p-1,g);
          M(x,g)
          DO2(g->n, cd(kK(g)[j]); kK(g)[j]=0) //set to 0 in case OOM happens
          z=x) 
    cd(g);
    R z;
  }

  if(offsetScan==(L)q)
  {
    DO(k->n-1, x=kK(k)[i+1]; if(!x)continue; if(!x->n)R ci(*kK(k)); if(!atomI(x)){if(n&&n!=x->n)R LE;else n=x->n)} //return x_0 if any empty list x_{i>0}
    if(!n) R bv_ex(p-1,k); //  {x+y+z}\[1;1;1] yields 1 but {x+y+z}\[1;1;1 1] yields (1 1;3 3;5 5)  
    n=MAX(1,n);//if nothing was a list set to 1
    K z=newK(0,1); 
    K g=newK(0,k->n);
    M(z,g);
    kK(z)[0]=ci(*kK(k));
    DO(n,*kK(g)=ci(kK(z)[z->n-1]); DO2(g->n-1, x=itemAtIndex(kK(k)[j+1],i); M(x,z,g) kK(g)[j+1]=x;)
         x=bv_ex(p-1,g); M(x,z,g) DO2(g->n, cd(kK(g)[j]); kK(g)[j]=0 ) //set to 0 in case OOM happens
         kap(&z,&x); cd(x);) 
    cd(g);
    z=collapse(z); //unnecessary?
    R z;
  }

  if(offsetEach==(L)q)
  {
    DO(k->n, x=kK(k)[i];if(!x)continue; if(!x->n)R newK(0,0); if(!atomI(x)){if(n&&n!=x->n)R LE;else n=x->n)} //return () on any empty list
    I c=!n;//collapse needed
    n=MAX(1,n);//if nothing was a list set to 1
    K z=newK(0,n), g=newK(0,k->n); M(g,z)//break [;;...] into subpieces for f, store in g
    DO(n, K x; DO2(k->n, x=itemAtIndex(kK(k)[j],i); M(x,g,z) kK(g)[j]=x) x=bv_ex(p-1,g); M(x,z,g) kK(z)[i]=x; DO2(k->n, cd(kK(g)[j]); kK(g)[j]=0))//sic =0
    cd(g);
    if(c)z=collapse(z);else z=demote(z);
    R z;
  }

  if(offsetEachright==(L)q) 
  {
    P(k->n!=2,VE)
    K a=kK(k)[0],b=kK(k)[1];
    R eachright2(a,p,b);
  }
  if(offsetEachleft ==(L)q)
  {
    P(k->n!=2,VE)
    K a=kK(k)[0],b=kK(k)[1];
    R eachleft2(a,p,b);
  }
  if(offsetEachpair ==(L)q) R NYI;//todo: is this reachable?

  R vf_ex(*p,k);
}

K ex1(V*w,K k,I*i,I n,I f)//convert verb pieces (eg 1+/) to seven-types, default to ex2 (full pieces in between semicolons/newlines) 
{
  if(offsetColon==w[0] && (L)w[1]>DT_SIZE && (L)w[2]>DT_SIZE && fwh==0) 
    {fer=1; if(f)*i=n; else *i=-1; K tmp=*(K*)*(w+1); R ci(tmp); }
  //if(in(*w,adverbs)) R NYI;//Adverb at beginning of snippet eg '1 2 3 or ;':1 2 3; or 4;\1+1;4
  if( DT_ADVERB_OFFSET <= (L)*w && (L)*w < DT_VERB_OFFSET ) {
    if(offsetScan==(L)*w) {
      if(0==strcmp(fBreak,"n")) R ex2(w+1,k);
      if(0==strcmp(fBreak,"t")) {K xx=ex2(w+1,k); show(xx); R xx;}
      if(0==strcmp(fBreak,"s")) {K xx=ex2(w+1,k); fer=1; R xx;}
    }
    else  R NYI;
  }

  I c=0; while(w[c] && !bk(w[c])){c++; if(offsetColon==w[c-1])break;} //must break or assignment is n^2  (a:b:c:1)

  if(!c || !VA(w[c-1]) || (c>1 && offsetColon==w[c-1] ) ) R ex2(w,k); //typical list for execution

  if(w[0]==offsetColon && w[1]>(V)DT_SIZE){ 
    if(w[-1]!=offsetColon) fer=1; 
    I d=0; while(w[d] && !bk(w[d])){d++;} 
    K a=Kv(); a->n=0; K kb=newK(-4,d); M(a,kb) V*b=(V*)kK(kb); DO(d-1, b[i]=w[i+1];) b[d-1]=0; kV(a)[CODE]=kb; 
    V x=ex_(&a,0); cd(a); R x; }

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
        kap((K*) kV(a)+LOCALS,&q);//oom
        cd(q); //kap does ci
        q=EVP(q); //oom free z etc. kap needs checking 
        b[j]=q;
  )
  kV(a)[CODE] = kb;
  R a;
}

Z K ex2(V*v, K k)  //execute words --- all returns must be Ks. v: word list, k: conjunction?
{
  K t0,t2,t3,e,u;
  I i=0;

  //TODO: is this messed up ......we can't index like this for (|-+) ?? what about 0-NULL []
  //ci(k) was R 0; ...  put this here for f/[x;y;z]
  if(!v || !*v)R k?(1==k->n)?ci(kK(k)[0]):ci(k):(K)(L)DT_END_OFFSET; //? '1 + _n' -> domain err, '1 +' -> 1+ . but '4: . ""' -> 6 

  if(bk(*v)) R *v;  // ; case

  if(!v[1] && !k){  // n case
    K z=ex_(*v,1);
    if(z>(K)DT_SIZE && z->t==7 && z->n==3){
      if(prnt && f1s && kV(z)[PARAMS] && kV(prnt)[CACHE_TREE] && !kV(z)[CACHE_TREE] && kK(z)[PARAMS]->n){
        K j0=dot_monadic(kV(z)[PARAMS]); K j1=dot_monadic(kV(prnt)[CACHE_TREE]); K j2=join(j0,j1); 
        if(encp==0)kV(z)[CACHE_TREE]=dot_monadic(j2); if(encp==1)kV(z)[CACHE_TREE]=dot_monadic(j1);
        cd(j0); cd(j1); cd(j2); cd(kK(prnt)[CACHE_WD]); kV(prnt)[CACHE_WD]=0;
      }
      if(prnt && kV(prnt)[CODE] && kK(prnt)[CODE]->t==-3 && kC(kK(prnt)[CODE])[0]=="{"[0] &&
        kC(kK(prnt)[CODE])[kK(prnt)[CODE]->n-1]=="}"[0] && strchr(kC(kK(prnt)[CODE]),"y"[0])){encf=prnt; ci(encf);}
      if(encp!=2 || !prnt)prnt=z; 
      else {cd(z); R prnt;}
    }
    R z; 
  }

  if(!v[1] && sva(*v)) R vf_ex(*v,k);     //TODO: (,/:) and (,\:) both valence 2 
  //TODO: brackets may also appear as:     +/\/\[]    {x}/\/\[]    a/\/\[]    (!200)\\[10;20]

  if(bk(v[1])){
    K z= ex_(*v,1);
    if(z && prnt && z->t==7 && z->n==3 && 
       kV(prnt)[CACHE_TREE] && kK(prnt)[LOCALS]->n && !kK(prnt)[PARAMS]->n &&
       kV(z)[LOCALS] && !kK(z)[LOCALS]->n)
    {
      K j0=dot_monadic(kV(prnt)[CACHE_TREE]); K j1=dot_monadic(kV(z)[LOCALS]); 
      K j2=join(j0,j1); kV(z)[CACHE_TREE]=dot_monadic(j2);
      cd(j0); cd(j1); cd(j2); f1s=0; prnt=z; 
    }
    else if(z>(K)DT_SIZE && z->t==7 && z->n==3 && prnt && f1s && kK(prnt)[LOCALS]->n
         && kV(z)[PARAMS] && kV(prnt)[CACHE_TREE] && !kV(z)[CACHE_TREE] && kK(z)[PARAMS]->n)
    {
      K j0=dot_monadic(kV(z)[PARAMS]); K j1=dot_monadic(kV(prnt)[CACHE_TREE]); 
      K j2=join(j0,j1); kV(z)[CACHE_TREE]=dot_monadic(j2); cd(j0); cd(j1); cd(j2);
    }
    R z; 
  }

  if(!VA(*v) && (offsetColon == v[1] || (VA(v[1]) && offsetColon==v[2]) ) ) //Handle assignment
  {
    if(adverbClass(v[1]))R SYE;//Could potentially handle instead of erroring
    K a=0,b=0,c=0,d=0,p=0;
    K*w=*v;
    U(a=*w);
    if(7==a->t && 0==a->n && (b=kV(a)[CONJ]) && 7==b->t && 0==b->n ) 
    {
      U(b=ex_(kV(a)+CONJ,((L)*kW(b)==1 || (L)*(kW(b)+1)==1)?1:2))
      w=*kW(a); //K temp=a;  //a=ci(*kW(a)); w=*kW(a); cd(temp);
      if(b->t==0 && b->n==0) {   
        if(1e6<(UI)w) { 
          K r=*(K*)w;
          if(r->t==5) { p=enumerate(r); cd(b); b=enlist(p); cd(p); } } }
    }
    if(!b)U(b=newK(0,0))
    c=Kv(); //mmo  Optimization: could use A struct instead, with array[] for CODE
    K kc=newK(-4,2); //assumes NULL terminating entry
    M(b,c,kc);
    kV(c)[CODE]=kc;
    *kW(c) = v[1]; //it's v[1] regardless of colon position

    if(1!=sva(v[1])){d=ex1(v+(offsetColon==v[1]?2:3),k,0,0,1); }   // oom -- except it's ok for d to be 0 elsewhere
    d=bk(d)?0:d;

    if(fer) { cd(c); cd(d); cd(b); R _n(); }

    if(cirRef(*w,d) || (((*w)->t==6 && d) && (d->t==0 || d->t==5 || ABS(d->t)!=d->t)) ){
      K x = d;
      if(x->c) {d=kclone(x); cd(x);}
    }
    else if((*w)->t!=6){ 
      K x = *w;
      if(x->c>1) {*w=kclone(x); cd(x);}
    }

    K h=dot_tetradic_2(w,b,c,d);
    cd(c); cd(d); M(b,h)
    K j=of(h,b); 
    cd(b);
    R j;
  }

  while(v[1] && adverbClass(v[2+i])) i++;
  //TODO: Catch 0-returned-errors here and below
  if(!sva(v[0]) && (i || 2==sva(v[1]))){   // na+. or nv. case  (n noun, a adverb, + means regex one+ and . means regex anything )
    t2=ex2(v+2+i,k); if(fer && strcmp(errmsg,"undescribed")) R t2;
       //these cannot be placed into single function call b/c order of eval is unspecified
    t3=ex_(v[1],1);
    if(t3>(K)DT_SIZE && t3->t==7 && t3->n==3){
      if(prnt && kV(prnt)[CACHE_TREE] && kV(prnt)[CACHE_WD] && !kK(t3)[LOCALS]->n){
        if(kK(prnt)[CACHE_TREE]->n){
          K j0=dot_monadic(kV(t3)[PARAMS]); K j1=dot_monadic(kV(prnt)[CACHE_TREE]); 
          K j2=join(j0,j1); cd(kK(t3)[CACHE_TREE]); kV(t3)[CACHE_TREE]=dot_monadic(j2); 
          cd(j0); cd(j1); cd(j2); f2s=1;
        }
        else if(kV(prnt)[CONJ]) {
          K j0=dot_monadic(kV(t3)[PARAMS]); K j1=dot_monadic(kV(prnt)[CACHE_TREE]); 
          K j2=join(j0,j1); kV(t3)[CACHE_TREE]=dot_monadic(j2); cd(j0); cd(j1); cd(j2);
        }
      }
      prnt=t3; 
    }

      //if(v[1]!=t3) if(!VA(t3)) show(t3);//for use with below
      u=v[1]; //This u thing fixes repeated use of 7-1 subparen like f:|/0(0|+)\;f a;f b;.  
              //Not thread-safe. Adding ex_ result to LOCALS on 7-1 is probably better. See below
    v[1]=VA(t3)?t3:(V)&t3;
    t0=ex_(*v,1); if(fer && strcmp(errmsg,"undescribed")){cd(t2); R(t0);}
    if(!prnt && t0->t==7 && t0->n==3)prnt=t0;
    e= dv_ex(t0,v+1+i,t2); v[1]=u;
    cd(t0); cd(t2); if(!VA(t3)) cd(t3);
    R e; 
  }

  //vn. case
  i=0; while(adverbClass(v[1+i])) i++; //ALT'Y: i=adverbClass(b)?i+1:0;
  t2=ex2(v+1+i,k); //oom. these cannot be placed into single function call b/c order of eval is unspecified
  t3=ex_(*v,1);
  if(t3>(K)DT_SIZE && t3->t==7 && t3->n==3){
    if(prnt && kV(prnt)[CACHE_TREE] && kV(prnt)[CACHE_WD] && !kK(t3)[LOCALS]->n){
      if(kK(prnt)[CACHE_TREE]->n && kK(prnt)[LOCALS]->n){
        if(kV(t3)[CACHE_WD] && !kV(t3)[CACHE_TREE]){
          f2s=1; kK(t3)[CACHE_TREE]=kK(prnt)[CACHE_TREE]; ci(kK(t3)[CACHE_TREE]);
        }
        else if(kK(t3)[PARAMS]->n || grnt){
          K j0=dot_monadic(kV(t3)[PARAMS]); K j1=dot_monadic(kV(prnt)[CACHE_TREE]); K j2=join(j0,j1);
          if(kV(t3)[CACHE_TREE] && kK(t3)[CACHE_TREE]->n)cd(kK(t3)[CACHE_TREE]);
          kV(t3)[CACHE_TREE]=dot_monadic(j2); cd(j0); cd(j1); cd(j2);
        }
      }
      else {
        K j0=dot_monadic(kV(t3)[PARAMS]); K j1=dot_monadic(kV(prnt)[CACHE_TREE]); 
        K j2=join(j0,j1); kV(t3)[CACHE_TREE]=dot_monadic(j2); cd(j0); cd(j1); cd(j2);
      }
    }
    prnt=t3; 
  }

  u=*v; //Fixes a bug, see above. Not thread-safe. Adding to LOCALS probably better
  *v=VA(t3)?t3:(V)&t3;
  if(*(v+i)==(V)offsetEach)grnt=prnt;
  e=dv_ex(0,v+i,t2); *v=u;
  if(*(v+i)==(V)offsetEach)grnt=0;
  cd(t2); if(!VA(t3) && encp!=3) cd(t3);
  R e; 
}

I cirRef(K x,K y){
  I f=0;
  if(xt==6 || !y || (yt!=0 && yt!=5) || ABS((L)(x))<DT_SIZE) R 0;
  DO(yn, f=cirRef_(x,kK(y)[yn-i-1],f))
  R f;
}

I cirRef_(K x,K y,I f){
  if(x==y)f=1;
  DO(yn, if(!f && (yt==0 || yt==5)) f=cirRef_(x,kK(y)[yn-i-1],f))
  R f;
}
