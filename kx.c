/* execution */

#include "incs.h"

#include "k.h"
#include "kx.h"
#include "km.h"

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
        V **q=(V**)p-1; I s=-2==y->t;  
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
  { 
    K u=b,c=0;I flag=0; 

    I useN=0,n=0,useB=0;
    if(a) if(1 == a->t){useN=1; n=*kI(a);} else if(7==a->t){useB=1;}
    P(n<0,IE)

    if(useN) //n f/x
    {
      DO(n, c=dv_ex(0,p-1,u); if(b!=u)cd(u); U(u=c))
      c=c?c:ci(b);
    }
    else if(useB) // b f/x 
    {
      I t;
      do
      { 
        K g=vf_ex(&a,u); U(g)
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
  { 
    K u=enlist(b),v,w,c=0,d;I flag=0;//TODO: optimize/memory manage enlists,firsts,reverses here
    U(u);

    I useN=0,n=0,useB=0;
    if(a) if(1 == a->t){useN=1; n=*kI(a);}else if(7==a->t)useB=1;
    P(n < 0,IE) //mmo

    if(useN) DO(n, U(v=reverse(u)) d=first(v); cd(v); c=dv_ex(0,p-1,d); cd(d); U(c) U(v=enlist(c)) cd(c); u=join(w=u,v); cd(w); cd(v); U(u)) 
    else if(useB)
    {
      I t;
      do
      {
        U(v=reverse(u))
        d=first(v); cd(v);
        K g=vf_ex(&a,d); U(g)
        t=(1==g->t && *kI(g));
        cd(g);
        if(!t){cd(d); break;}
        c=dv_ex(0,p-1,d); cd(d);
        U(c) U(v=enlist(c)) cd(c);
        u=join(w=u,v); cd(w); cd(v); U(u) 
      }while(1);
    }
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

  if(2 > k && adverb == each)
  {
    I bt=b->t, bn=b->n;
    if(bt > 0) R dv_ex(0,p-1,b);
    else
    {
      K z = newK(0,bn),d=0; U(z)
      K g;
      if(0 >bt) DO(bn, g=newK(ABS(bt),1); M(g,z) memcpy(g->k,((V)b->k)+i*bp(bt),bp(bt)); d=dv_ex(0,p-1,g); cd(g); M(d,z) kK(z)[i]=d) 
      if(0==bt) DO(bn, d=dv_ex(0,p-1,kK(b)[i]); M(d,z) kK(z)[i]=d)
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
  if(!k&&!w){cd(g); R 0;}// (2="2") 2 err
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
    U(y); 
    if(y->t == 0 && y->n==0){cd(y); y=_n();}
  }
  z=ex0(kW(x),y,r);  //eval wd()
  cd(y);

  R z;
}

K ex(K a){ U(a); K z=ex_(&a,0); cd(a);R z;} //Input is 7-0 type from wd()

K ex0(V*v,K k,I r) //r: {0,1,2} -> {code, (code), [code]} Reverse execution/return multiple (paren not function or script) "list notation"  {4,5,6,7} -> {:,if,while,do}
{
  I n=0, e=1, i,a,b;
  while(v[n])if(bk(v[n++]))e++;
  b=e>1;

  K z=0, x;

  SW(r)
  {
    CS(0, for(i=-1;i<n;i++)if(-1==i||bk(v[i])){cd(z); U(x=ex1(v+1+i,0)) z=bk(x)?_n():x;})//  c:9;a+b;c:1 
    CS(4, for(i=-1;i<n;i++)if(-1==i||bk(v[i])){U(x=ex1(v+1+i,0)) x=bk(x)?_n():x; while(++i<n&&!bk(v[i])); if(i==n) R x; if(xt!=1){cd(x);R TE;} a=*kI(x);cd(x); if(a)R ex1(v+i+1,0); else while(i<n&&!bk(v[i]))i++; } R _n())
    CSR(5,)CSR(6,)CS(7, do{U(x=ex1(v,0)) x=bk(x)?_n():x; if(xt!=1){cd(x);R TE;}a=*kI(x);cd(x);i=0;if(b){while(++i<n&&!bk(v[i])); if(i>=n)break;}SW(r){CSR(5,)CS(6,if(a&&b)cd(ex0(v+i+1,0,0)))CS(7,DO2(a,cd(ex0(v+i+1,0,0))))}}while(6==r && a); R _n())
    CD: z=newK(0,n?e:0); if(n)for(i=n-1;i>=-1;i--)if(-1==i||bk(v[i])){x=ex1(v+1+i,0); M(x,z) kK(z)[--e]=bk(x)?2==r?0:_n():x;}// (c:9;a+b;c:1) oom
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
  //if(in(*w,adverbs)) R NYI;//Adverb at beginning of snippet eg '1 2 3 or ;':1 2 3; or 4;\1+1;4
  I d;
  if ((d=diff(*w,adverbs))<adverb_ct && d>=0) R NYI;
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
  if(!v[1] && sva(*v)){ R vf_ex(*v,k);}  //TODO: (,/:) and (,\:) both valence 2  
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
