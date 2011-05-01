#include "incs.h"

#include "k.h"
#include "km.h"
#include "p.h"
#include "r.h"
#include "v.h"
#include "vd.h"

/* dot monadic, dyadic, triadic, tetradic */

Z K dot_ref(K *p,K *x,K *z,I s,K c,K y);
Z K makeable(K a);

K of2(K d, K *x, K *y, I s)
{
  K f=*x;
  I dt=d->t, dn=d->n, ft=f->t, fn=f->n;

  if(0>=s)R at_verb(d,f); //Is it at_verb or at()...  ?

  K z;
  if(0==ft)
  {
    U(z=newK(0,fn))
    DO(fn, M(z,kK(z)[i]=of2(d,&kK(f)[i],y,s)))
  }
  else if(1==ABS(ft))
  {
    if(dt!=0)R 0;//TODO: Error - must be 0 if s!=0 ... ?
    I k;
    DO(fn, k=kI(f)[i]; P(k>=dn,XE))
    if(1==ft) R of2(kK(d)[*kI(f)], y, 1+y, s-1);//Don't increase depth, just move on
    U(z=newK(0,fn))
    DO(fn, M(z,kK(z)[i]=of2(kK(d)[kI(f)[i]], y, 1+y, s-1)))
  }
  else if(4==ABS(ft))
  {
    if(dt!=5)R 0;//TODO: Error - must be 0 if s!=0 ... ?
    if(4==ft) R of2(lookup(d,*kS(f)), y,1+y,s-1);//Don't increase depth  ;  mm/o lookups
    U(z=newK(0,fn))
    DO(fn, M(z,kK(z)[i]=of2(lookup(d,kS(f)[i]), y, 1+y, s-1))) 
  }
  else if(6==ft)
  {
    if     (0==dt){U(z=newK(0,dn)) DO(dn,M(z,kK(z)[i]=of2(kK(d)[i],y,1+y,s-1))) }
    else if(5==dt){U(z=newK(0,dn)) DO(dn,M(z,kK(z)[i]=of2(kK(kK(d)[i])[1],y,1+y,s-1))) }
    else R RE;
  }
  else R TE;
  if(z)z=demote(z);
  R z;
}

K of(K a, K b)  //TODO: oom all (see of2() for M(z,kK(z)[i]=...) pattern )
{

  //TODO: must implement Value/Execute '`k.b@"a+1"' same as '.(`k.b;"a+1")'

  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  if(0==b->t && 0==b->n) R ci(a);//Empty list is identity
  P(0<at && at<5 && 6!=bt,TE)
  //At is either <=0 or dict or nil. b is not ()
  K z;
  if(6==at)
  { // _n . x  for various x in K3.2
    if     (1==bt)z=ci(b);
    else if(4==ABS(bt))z=_n(); 
    else if(0==bn && (-1==bt || -2==bt))z=_n();
    else if(6==bt || 0==bt || (-3==bt && 0==bn))z=newK(0,0);
    else R TE;
  }
  else if(6==bt) 
  {
    if(5==at){ z=newK(0,an);DO(an,kK(z)[i]=ci(kK(kK(a)[i])[1])) z=demote(z); }//TODO: should demote be collapse? 
    else if(0>=at) z=ci(a);
    //Getting to here with a symbol atom for a is tricky. "x:`sym; `x . _n => rank error"
    else R RE;// a->t necessarily in {1,2,3,4}
  }
  else if(0>bt && 0==bn && -3!=bt)z=ci(a);
  else if(5==at || 0==at)
  {//Can't have bn==0 here
    if(0==bt){K *f=&kK(b)[0]; z=of2(a,f,bn>0?1+f:0,bn-1);} 
    else if(-1==bt || -4==bt){K k=promote(b); K *f=&kK(k)[0]; z=of2(a,f,1+f,bn-1); cd(k); } //mmo  U(k) ?  //This line added to fix test for (5 2.14;"abc") . 1 2  --- doesn't give me great confidence in the code
    else z=at_verb(a,b);  
  }
  else if(0 >at)
  {
    if(1==ABS(bt))z=at_verb(a,b);
    else if(0==bt){K k; P(bn!=1,TE) z=newK(0,bn);DO(bn,k=at_verb(a,kK(b)[i]); M(k,z) kK(z)[i]=k) z=collapse(z);} 
    else R TE;
  }
  R z;
}

K dot(K a, K b) //NB: b can be a cheating 0-type with NULLs .. ?
{ 
  //TODO: create dename without path-creation effect. will lookup correct handle or return a _n to use ... but won't create path. K at() also needs this.
  //if(4==a->t)a=retrieveByHandle(a); 

  if(7==a->t) R vf_ex(&a,b); //Verb: "Apply" //TODO: my guess is this fails everywhere vf_ex does (derived verbs?) (|+) . (0;1) ???
  R of(a,b); //TODO: vf_ex might/could implement this itself ?
}

//TODO: Is this a stable thing if my function mucks with the tree above me? No, but find 'reference error'
//TODO: Does this do the right thing for functions/nouns with valence > 2 ?
//TODO: k-tree elements with subelements whose refcount is >1 will bork????
//TODO: catch oom errors etc.
K dot_ref(K *p, K *x, K *z, I s, K c, K y)
{
  K d=*p, f=x?*x:0;
  I dt=d->t, dn=countI(d), ft, fn, yt, yn;

  if(f) {ft=f->t; fn=countI(f);}
  if(y) {yt=y->t; yn=countI(y);}

  if(-1==s && 0==fn && -3!=ft)
  {
    I argc = y?2:1;
    K args=newK(0,argc);U(args)//Cheating 0-type w/ NULLs 
    kK(args)[0]=ci(*p);
    if(argc > 1) kK(args)[1] = ci(y);
    K r = specialAmendDot(c,args);
    cd(args);
    U(r)
    cd(*p); 
    *p=r;
    R NULL;
  }
  //these may turn out to be the "ELSE" case
  if((1 <= dt && dt <= 4) || 7==dt || 7==ft) R RE;
  else if(6==dt && (0 >= ft)) R XE;
  else if(6==dt && 6 != ft && 4 != ft) R TE;
  if(5==dt && 123 == ft) R NULL; //TODO: Fill in dict errors
  //TODO: full error chart. at_ref will account for some of it

  if(0>=s) at_ref(p,f,c,y); //what errors will this take care of ?
  else if(0==ft)
  {
    if(!atomI(f) && y && !atomI(y) && fn != yn) R LE;
    I n = (atomI(f) && y)?yn:fn;
    if(y) U(y=promote(y))
    DO(n, dot_ref(p, kK(f)+(i%fn), z, s, c, kK(y)[i%yn]))
    cd(y);
  }
  else if(1==ABS(ft))
  {
    if(!atomI(f) && y && !atomI(y) && fn != yn) R LE;
    if( 1==ft && dt > 0) R TE; // (5,6)

    if(y && yt != 0 && !atomI(f)) U(y = promote(y))
    else ci(y);

    //TODO: .[.,(`a;2);0 0;*:] -> identity. (0->type err, 0 0 0-> rank err)
    if(dt != 0) R RE;

    DO(fn, I e=kI(f)[i]; if( e < 0 || dn <= e ) R XE; )//check is in advance
    DO(fn,
      K py=0;
      if(y) py=atomI(f)?y:kK(y)[i%yn];
      dot_ref(kK(d)+(kI(f)[i]),z,z+1,s-1,c,py);
    )
    cd(y);
  }
  else if(4==ABS(ft))
  {
    if(!atomI(f) && y && !atomI(y) && fn != yn) R LE;
    if( 4==ft && 0 >= dt) R TE;
    if(-4==ft && 0 >= dt) R IE;
    if(y && yt != 0 && !atomI(f)) U(y = promote(y))
    else ci(y);

    //Only 6/4, 5/4, 5/-4 at this point
    DO(fn,
      K py = 0;
      if(y) py=atomI(f)?y:kK(y)[i%yn]; //trying promote here instead of itemAtIndex like in at_ref
      S u = kS(f)[i];
      dot_ref(lookupEVOrCreate(p,u),z,z+1,s-1,c,py); //oom, cd(y),  ???
    )
    cd(y);
  }
  else if(6==ft)
  {
    if(6==dt) R NULL; //identity
    if(y && !atomI(y) &&  yn != d->n) R LE;
    if(y) U(y=promote(y))
    if(5==dt) DO(d->n, dot_ref(EVP(DI(d,i)),z,z+1,s-1,c,y?kK(y)[i%yn]:0))
    if(0>=dt) { K k=Ki(0); M(k,y?y:k);  DO(countI(d), *kI(k)=i; dot_ref(p,&k,z,s,c,y?kK(y)[i%yn]:0)) cd(k); }
    cd(y);
  }
  R 0;
}

K dot_tetradic_2(K *g, K b, K c, K y)
{
  I bt=b->t, bn=countI(b);

  if(0==bn || 6==bt)
  {
    dot_ref(g,&b,0,bn-1,c,y); //could factor further by promoting everything...
  }
  else if(0==bt || 1==ABS(bt) || 4==ABS(bt))
  {
    b=promote(b); bt=0; bn=countI(b); //oom
    K *f=kK(b); dot_ref(g,f,bn>0?1+f:0,bn-1,c,y); //bn!=0 ???? copy/paste comment
    cd(b);
  }
  else R TE; //Type Error  7,5,+-3,+-2 TODO: Move inside if possible... ?
  
  R *g;
}

//TODO: All this must be rewritten to handle function-local-dictionaries and global
K dot_tetradic(K a, K b, K c, K y)//Handles triadic and tetradic case
{
  if(isColonDyadic(c) && !y) //'Error Trap'
  {
    K d = newK(0,2); 
    K i = Ki(0);
    M(d,i)
    kK(d)[0] = i;
    K z = vf_ex(&a,b);
    kK(d)[1]=z;
    if(!z) 
    {
      *kI(i)=1;
      K e=newK(-3,strlen(errmsg));
      M(d,e);
      strcpy(kC(e),errmsg);
      kK(d)[1]=e;
    }
    R demote(d);
  }


  K q=0, *p=0;

//TODO: Index/Of claims to accept handles as sub-elements....is this true??? for Of and for DOT_TETRADIC etc... 
  if(a->t == 4)
  {
    //TODO: reference error <-  d.e.f:123;\d .k.d.e; .[`.k;`d;:;1]
    //TODO: ^^ note, whoever handles reference error will need to know about Context of the Parsed value being Executed
    //          because it doesn't matter if the \d directory changes in the middle:
    //          d.e.f:123;\d .k.d.e;\n\n\n a:1;."\\d .";.[`.k;`d;:;1];a:2 -> reference error (and then afterwards _d is `)

    //triadic & tetradic create dict path if not existing (even on errors). dyadic/monadic create nothing

    p = denameS(__d,*kS(a));
    U(p) //oom
  }
  else q = kclone(a); 

  K *g = q?&q:p;

  if(!dot_tetradic_2(g,b,c,y)) R 0; // bubble up err


  //monadic @[1;();:] -> (1;"rank")
  //        @[_n;1 2;:] -> (0; 1 2) 
  
  R q?q:ci(a);// sym not *p
}

K make(K a)//Assumes makeable() is true
{ 
  //TODO: this will need to set reference counts on all dictionary entries, etc.
  P(!makeable(a), RE)
  I n=a->n;
  K x,y; 
  K z=newK(5,n);
  DO(n, kK(z)[i]=newK(0,3);)
  DO(n, x=kK(z)[i]; y=kK(a)[i]; DO2(y->n,kK(x)[j]=y->t?Ks(kS(y)[j]):ci(kK(y)[j])) if(y->n<3)kK(x)[2]=_n())  //oom
  R z;
}
Z K unmake(K a){K z=kclone(a); z->t=0; R z;}//TODO: deep clone inefficient
Z K makeable(K a) //TODO: this has to be reworked. can't hang out raw in dot_monadic as it is currently
{
  I t=a->t, n=a->n;
  //All this was moved here from make(). not sure how to handle error checking when it's outside like this
  P(0!=t, 0)
  K x;
  //NB: .(`a`b;`c`d) is also a valid dictionary (sym vectors)
  DO(n, x=kK(a)[i]; if( (0!=x->t && -4!=x->t) || x->n < 2 || 3 < x->n || (-4==x->t && x->n != 2) )R 0)
  DO(n, x=kK(a)[i]; if(0==x->t) if( 4 != kK(x)[0]->t || (3==x->n && 5!=kK(x)[2]->t && 6!=kK(x)[2]->t)) R 0)
  R (K)1;
}
K dot_monadic(K x){R 3==ABS(xt)?KX(x):4==xt?ci(*denameS(__d,*kS(x))):5==xt?unmake(x):makeable(x)?make(x):vf_ex(addressDot,x); }  //TODO: mmo. untested everywhere. TODO: esp mmo _() underscore function
