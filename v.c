#include "incs.h"
#include "scalar.h"

#include "k.h"
#include "km.h"
#include "0.h"
#include "v.h"
#include "r.h"

/* misc verbs */

Z I updateIndex(K *p,I x,K r);
Z K lookupEntryOrCreate(K *p,S k);
Z S notsp(S a);
Z I isDotDyadic(K x);

K itemAtIndex(K a, I i)// Return i-th item from any type as K - TODO: oom wherever this is used
{
  I at=a->t;
  if( 0< at)R ci(a);
  if(-4==at)R Ks(kS(a)[i]);  //could refactor all this
  if(-3==at)R Kc(kC(a)[i]);
  if(-2==at)R Kf(kF(a)[i]);
  if(-1==at)R Ki(kI(a)[i]);
  R ci(kK(a)[i]);
}

//Glue will be useful when it comes time to implement \d ?
S glueSS(S c, S d)
{
  I x=strlen(c),y=strlen(d);
  S m = malloc(x+y+2); //oom
  sprintf(m,"%s.%s",c,d);
  R m;
}
K glue(K a, K b) { R Ks(sp(glueSS(*kS(a),*kS(b)))); } //oom

//Dictionary and Dictionary Entry utility functions and accessors
// currently no guards for 0 inputs ... should this change?
K DI(K d, I i){R kK(d)[i];}         //dictionary index, yields entry
S ES(K d){ R *kS(kK(d)[0]);}        //dictionary entry's symbol
K DE(K d,S b){DO(d->n,K x=DI(d,i);if(b==ES(x))R x)R 0;} //dictionary entry lookup
Z K* EIA(K a,I i){R kK(a)+i;}         //dictionary entry's address of i-th index
K* EVP(K e){R EIA(e,1);}            //dictionary entry's value-pointer address (K*)
Z K* EAP(K e){R EIA(e,2);}            //dictionary entry's attribute_dictionary-pointer address (K*)
K   EV(K e){R *EVP(e);}             //dictionary entry's stored value

//Potential K3/4 bug (won't spend time looking):  names in functions (params,
//locals, global:: assigns, or global .k.a) are done at creation. if the named
//dict entries are deleted (what method) from K TREE what happens the next time
//you run the function?  does function context affect this?

//Weird: Found some bug in K3.2 were running .` would add a copy of the entries in the root of the K tree every time. Not sure how to reproduce
//K* denameBig(K dir_sym,K name_sym){R denameS(*kS(dir_sym),*kS(name_sym));} //[unnecessary?] wrapper for K-object inputs
K* denameS(S dir_string, S t, I create)
{
  R denameD('.'==*t||!*t?&KTREE:denameD(&KTREE,dir_string,create),t,create);//duplicates '.' functionality in denameD to avoid dictionary initialization
}

Z K* denameRecurse(K*p,S t,I create);

K* denameD(K*d, S t, I create)
{
  if(!simpleString(t)) R 0; //some kind of error
  R denameRecurse('.'==*t||!*t?&KTREE:d,t,create);
}
Z K* denameRecurse(K*p,S t,I create) 
{
  if(!*t)R p; 
  if('.'==*t)t++;
  I c=0,a=(*p)->t;
  while(t[c] && '.'!=t[c])c++;
  S u=strdupn(t,c);//oom
  S k=sp(u); //oom
  free(u);
  t+=c;

  P('_'==*k,(K*)kerr("reserved"))// ... not positive this goes here. does it fit in LOC? or parser maybe?

  //Probably the below error check (and any others in front of LOC) should be moved into LOC
  //and LOC should have the potential to return 0 (indicating other errors as well, e.g. out of memory)
  P(!(6==a || 5==a),(K*)TE)

  K e=0;
  
  if(create)
  {
    e=lookupEntryOrCreate(p,k);  
    P(!e,(K*)ME)
  }
  else
  {
    K a=*p;
    if(5==a->t) e=DE(a,k);
    P(!e,&NIL)
  }

  if('.'==*t && (!t[1] || '.'==t[1])) 
  {
    t++;
    p=EAP(e);    //attribute dict
  }
  else p=EVP(e); //value

  R denameRecurse(p,t,create);
}

Z K lookupEntryOrCreate(K *p, S k) //****only *dict or *_n are passed to here
{
  K a=*p, x;

  if(5==a->t) if((x=DE(a,k))) R x;

  P(!strlen(k),TE) //TODO verify this noting `. is not `
  P(strchr(k,'.'),DOE)

  x=newEntry(k);
  if(6==a->t){cd(*p); *p=newK(5,0);} //mm/o is this done right?
  kap(p,&x); //oom
  cd(x);

  R x;
}

K* lookupEVOrCreate(K *p, S k){K x=lookupEntryOrCreate(p,k); R x?EVP(x):0; } //mm/o
K lookup(K a, S b){K x=DE(a,b); R x?EV(x):_n();} 


//TODO: oom at_verb everywhere
K at_verb(K a, K b)//[Internal Function]  TODO: should handle a is dict/directory & b is executable string like "1+1+c"
{//"of" depends on this even though @[d;i] = .[d;,i] <--- that equality doesn't always hold
  if(!b) R b;
  if(0==b->t && 0==b->n)R newK(0,0);//Overriding right-arg () 
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  K z;

  if(at==6)//Left side nil (sort of like empty dict?)
  { //K3.2 - complicated functionality. Leads me to believe this part was implemented some other way
    //2009.11.10 - probably it was. see how nil case was folded in in at_ref
    if( 1==ABS(bt))R ci(b);//Overrides
    if( 6==bt || (0>=bt && 0==bn)) R newK(0,0);//Careful: dicts can have 0==bn
    if( 4==bt)R _n();
    if(-4==bt){z=newK(0,bn); DO(bn,kK(z)[i]=_n()) R z;}//0#` handled above
    R TE;
  }

  if(1==ABS(bt))//Note switch to "b->t" here
  {
    P(0<at,TE) //Type/Rank/Length Error. (Several cases are eliminated before here.)
    I x; DO(bn, if((x=kI(b)[i]) >= an || x <0) R XE)

    z=newK(at*-bt,bn);
    if     (-4==at) DO(bn,kS(z)[i]=kS(a)[kI(b)[i]]) //TODO: memcpy
    else if(-3==at) DO(bn,kC(z)[i]=kC(a)[kI(b)[i]])
    else if(-2==at) DO(bn,kF(z)[i]=kF(a)[kI(b)[i]])
    else if(-1==at) DO(bn,kI(z)[i]=kI(a)[kI(b)[i]])
    else if( 0==at){DO(bn,kK(z)[i]=ci(kK(a)[kI(b)[i]])) if(bt==ABS(bt) || bn!=1)z=collapse(z);}
  }
  else if(4==ABS(bt))
  {
    P(5!=at,TE)
    z=newK(0,bn);
    DO(bn, kK(z)[i]=ci(lookup(a,kS(b)[i])))
    z=collapse(z);
  }
  else if(6==bt)
  {
    if     (0>=at)z=ci(a);
    else if(5==at){z=newK(0,an); DO(an, kK(z)[i]=ci(EV(DI(a,i)))) z=collapse(z);} //TODO: untested
    else R TE; // Type{3,4}/Rank{1,2} Error
  }
  else if(0==bt){z=newK(0,bn);U(z)  DO(bn,M(z,kK(z)[i]=at_verb(a,kK(b)[i]))) }
  else if(isDotDyadic(b) && at==5){z=newK(0,an); DO(an, kK(z)[i]=ci(*EAP(DI(a,i))))}
  else R TE;

  R z;
}

// "`k @ 0" ;  "`k @ `a" ; "`k @ \"a\"" ; "`.k @ \"a\"" ; "`.k @ `a" ; "`.k @ `gdfgdfg" ; "`.k @ 1.0" ; "`.k @ 1 2 3" but "`asdas @ 0 1" ; "`sasd @ \"a\"" ; " `.asasas @ \"f\""
K at(K x, K y)
{
  K a,z;
  if(4==xt)R NYI; //TODO: Value/Execute when 4==xt ... weird cases: (first a:1) 
  if(7!=xt)R at_verb(x,y);
  a=enlist(y);
  M(a)
  z=dot(x,a);
  cd(a);
  R z;
}

Z I updateIndex(K *p,I x, K r) //assert (*p)->t is <= 0 and valid x
{
  I pt=(*p)->t, rt=r->t;

  if(0==pt)
  {
    cd(kK(*p)[x]);
    kK(*p)[x] = ci(r);
    *p = demote(*p); //oom ?
  }
  else if(pt != -rt)
  {
    K t=promote(*p);
    //U(t) //oom
    cd(*p);
    *p=t;
    cd(kK(*p)[x]);
    kK(*p)[x] = ci(r);  
  }
  else
  {
    if(-4==pt) kS(*p)[x] = *kS(r);
    if(-3==pt) kC(*p)[x] = *kC(r);
    if(-2==pt) kF(*p)[x] = *kF(r);
    if(-1==pt) kI(*p)[x] = *kI(r);
  }

  R 0;
}

Z I isVerbDyadic(K x,V v){R xt==7 && kW(x)[0]==v && !kW(x)[1];}
I isColonDyadic(K x){R isVerbDyadic(x,offsetColon);}
Z I isDotDyadic(K x)  {R isVerbDyadic(x,offsetDot);}

K specialAmendDot(K c, K args) //If c is like colon_dyadic return args@1, else dot
{
  if(isColonDyadic(c)) R 2==args->n?ci(kK(args)[1]):_n();
  R vf_ex(&c,args);
}

//TODO: Is this a stable thing if my function mucks with the tree above me? No, but find 'reference error'
//TODO: Does this do the right thing for functions/nouns with valence > 2 ?
//TODO: k-tree elements with subelements whose refcount is >1 will bork???? 
//TODO: add ability to return error, catch errors in calling functions
K at_ref(K *p, K b, K c, K y) // @[`a;0 1;+;10 20]
{
  I pt = (*p)->t, pn = (*p)->n;
  P(pt > 0 && pt != 5 && pt != 6,RE)

  I bt=b->t, bn=b->n;

  if(0==bn && (-1==bt || 0==bt ||  5==pt || 6==pt)) R 0;//Identity TODO ????
  P(0==bn && bt <= 0,IE)

  P(y && !atomI(b) && !atomI(y) && bn != yn, LE )

  I n=(y && atomI(b))?yn:bn;
  I argc = y?2:1;

  if(1==ABS(bt))
  {
    P(5==pt || 6==pt,TE)
    //Now pt <= 0
    DO(bn, I x=kI(b)[i]; P(x<0 || x>=pn,XE))

    DO(atomI(b)?1:n,
      K args=newK(0,argc);U(args)//Cheating 0-type w/ NULLs 
      kK(args)[0]=itemAtIndex(*p,kI(b)[i%bn]); 
      if(argc > 1) kK(args)[1] = atomI(b)?ci(y):itemAtIndex(y,i%yn); 
      K r = specialAmendDot(c,args);
      M(r,args)
      updateIndex(p,kI(b)[i%bn],r);
      cd(r);
      cd(args);
    )
  }
  else if(4==ABS(bt))
  {
    P(5!=pt && 6!=pt,TE)
    DO(atomI(b)?1:n,
      K args=newK(0,argc);U(args)//Cheating 0-type w/ NULLs 
      S u = kS(b)[i%bn];
      kK(args)[0]= ci(*lookupEVOrCreate(p,u)); // ... mm/o? tricky
      if(argc > 1) kK(args)[1] = atomI(b)?ci(y):itemAtIndex(y,i%yn);
      K r = specialAmendDot(c,args);
      M(r,args)
      K *v = EVP(DE(*p,u)); 
      cd(*v);
      *v=r; //mm/o inc/dec r here ??
      cd(args);
   )
  }
  else if(6==bt)
  {
    P(y && !atomI(y) && yn != pn,LE)
    if(6==pt) R 0;//Identity TODO ??

    K k = 5==pt?Ks(LS):Ki(0);
    U(k)
    if(y) M(k,y = promote(y))
    
    if(5==pt) DO(pn, *kS(k)=ES(DI(*p,i)); at_ref(p,k,c,y?kK(y)[i%yn]:0) )
    else DO(pn, *kI(k)=i; at_ref(p,k,c,y?kK(y)[i%yn]:0))
    cd(k);cd(y);
  }
  else if(0==bt) DO(n, K e=0; if(y)U(e=itemAtIndex(y,i%yn)) at_ref(p,kK(b)[i%bn],c,e); cd(e) ) 
  else R TE;
  // @[.,(`a;10);1.0;:;9]
  R 0; 
}


//TODO: test here: the "enlist" shortcut had a bug in at(x,y)
K at_tetradic(K a, K b, K c, K y)
{
  K d=enlist(b); U(d)
  K e=dot_tetradic(a,d,c,y);
  cd(d);
  R e;
} 

K colon_monadic(K a){R ci(a);}
K colon_dyadic(K a, K b){R ci(b);}

Z S notsp(S a)
{//In terms of interned S:  Output `x. for input `x
  I b=strlen(a);
  S c=strcpy(malloc(b+2),a);
  U(c)
  c[b]='.'; c[b+1]='\0';
  S d=sp(c);
  free(c);
  R d;
}
K not_attribute(K a)
{
  I t=a->t, n=a->n;
  K z;
  if     (4==ABS(t)){U(z=newK(t,  n)) DO(n,if(!(kS(z)[i]=notsp(kS(a)[i]))){cd(z);R 0;}) }
  else if(2==ABS(t)){U(z=newK(t/2,n)) DO(n,kI(z)[i]= (0==kF(a)[i])?1:0;)}//sic
  else if(1==ABS(t)){U(z=newK(t,  n)) DO(n,kI(z)[i]= (0==kI(a)[i])?1:0;)}
  else if(0==ABS(t)){U(z=newK(t,  n)) DO(n,if(!(kK(z)[i]=not_attribute(kK(a)[i]))){cd(z);R 0;})}//if 0, valid list contains >0 syms
  else R TE; 
  R z;
}

Z K excl_mkdict(K a, K b) //make dict, dyadic `foo`bar`baz!(1 2 3;`a`b`c;"abc") version
{
  I n=a->n;
  K k, v, t, z;
  U(z=newK(5,n))  // key, value, tuple, result
  DO(n, M(z,k=Ks(kS(a)[i]),t=newK(0,3),v=ci(kK(b)[i])) kK(t)[0]=k; kK(t)[1]=v; kK(t)[2]=_n(); kK(z)[i]=t;);
  R z;
}

K rotate_mod(K a, K b)
{
  P(b->t > 2,TE)
  P(4==ABS(a->t) && 0==b->t && a->n == b->n, excl_mkdict(a,b));
  P(!(1==a->t || b->t > 0), IE)
  R (b->t < 1)?rotate(a,b):mod(a,b);
}

I atomI(K a){R a->t>0?1:0;}//_n is atom
K atom(K a){R Ki(atomI(a));}//_n is atom

static K enumerate_charvec(C *pth)
{
  K z;
  I len=strlen(pth);
  K p=newK(-3,len+3);
  strncpy(kC(p),"ls ", 3);
  strncpy(kC(p)+3,pth,len);
  z = popen_charvec(kC(p));
  cd(p);
  R z;
}

K enumerate(K a)
{
  I t=a->t;
  K z;
  if     (6==t)z=newK(-4,0);
  else if(5==t){I n=a->n; z=newK(-4,n);U(z) DO(n, kS(z)[i]=ES(DI(a,i)))}//TODO: test this accessor composition
  else if(-3==t || 3==t){ return enumerate_charvec(kC(a)); }
  else if(4==t)R NYI; //TODO: 4==t enumerate dictionary of sym on k-tree, other sym: nil =Kn()
  else if(-1==t) //odometer
  {
    I n=a->n,x,p=1;
    K e,r,s;
    DO(n,x=kI(a)[i];p*=x;if(x<0||p<0)R IE;)
    if(n==0)p=0;
    U(z=newK(0,p)) 
    if(p>0)
    {
      DO(p,e=newK(-1,a->n);M(e,z) kK(z)[i]=e)
      r = kK(z)[0];
      DO(r->n, kI(r)[i]=0)
      DO(p-1, r=kK(z)[i];s=kK(z)[i+1]; I carry = 1;
        DO2(s->n, x=(-1+s->n)-j;kI(s)[x]=kI(r)[x]; if(carry){kI(s)[x]++;carry=0;} if(kI(s)[x]>=kI(a)[x]){kI(s)[x]=0;carry=1;}))
    }
    R z;
  }
  else if(1==t || 2==t){I n= t==1?*kI(a):(I)*kF(a); P(n<0,DOE) z=newK(-1,n); U(z) DO(n,kI(z)[i]=i)}//could instead be (in)?tolerant ceil/floor
  else R DOE;
  R z;
}
