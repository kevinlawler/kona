#include "incs.h"

/* Init vars for scalar dyad */
#define SCALAR_INIT(maxt)               \
  I at=a->t, an=a->n, bt=b->t, bn=b->n; \
  I type = MAX(ABS(at),ABS(bt));        \
  P(at <= 0 && bt <= 0 && an != bn, LE) \
  P(type > maxt, TE ) /* > allowed types? */              \
  I zt=type;          /* Starting at worst known type */  \
  if(MIN(at,bt) < 1) zt=-zt; /* Plural? */                \
  if(!at || !bt) zt=0;       /* Generic list trumps */    \
  I zn=at>0?bn:an;

/* Macro case: N:N, 1:N, N:1 implicit looping for op */
#define SCALAR_OP_CASE(op, cres, ca, cb)                      \
   if (an==bn) { DO(zn,cres [i]= op (ca [i], cb [i])) }       \
   else if (an==1) { DO(zn,cres [i]= op (ca [0], cb [i])) }   \
   else /* bn==1 */ { DO(zn,cres [i]= op (ca [i], cb [0])) }

/* Scalar operator macro, with proper float/int/array treatment */
#define SCALAR_OP(op,verb)                                                      \
   if (2==ABS(at) && 2==ABS(bt)) { SCALAR_OP_CASE(op,kF(z),kF(a),kF(b)) }       \
   else if (2==ABS(at) && 1==ABS(bt)) { SCALAR_OP_CASE(op,kF(z),kF(a),kI(b)) }  \
   else if (1==ABS(at) && 2==ABS(bt)) { SCALAR_OP_CASE(op,kF(z),kI(a),kF(b)) }  \
   else if (1==ABS(at) && 1==ABS(bt)) { SCALAR_OP_CASE(op,kI(z),kI(a),kI(b)) }  \
   else if (0==at || 0==bt) { dp(&z,verb,a,b); }

/* Macro case: N:N, 1:N, N:1 implicit looping for expression */
#define SCALAR_EXPR_CASE(expr, cres, ca, cb, vx, vy)        \
   if (an==bn) { DO(zn, vx=ca [i];vy=cb [i]; expr); }       \
   else if (an==1) { vx=ca [0]; DO(zn, vy=cb [i]; expr); }  \
   else /* bn==1 */ { vy=cb [0]; DO(zn, vx=ca [i]; expr); }

/* Scalar expression macro, with proper float/int/array treatment */
#define SCALAR_EXPR(expr,verb,vx,vy)                                                      \
   if (2==ABS(at) && 2==ABS(bt)) { SCALAR_EXPR_CASE(expr,kF(z),kF(a),kF(b),vx,vy) }       \
   else if (2==ABS(at) && 1==ABS(bt)) { SCALAR_EXPR_CASE(expr,kF(z),kF(a),kI(b),vx,vy) }  \
   else if (1==ABS(at) && 2==ABS(bt)) { SCALAR_EXPR_CASE(expr,kF(z),kI(a),kF(b),vx,vy) }  \
   else if (1==ABS(at) && 1==ABS(bt)) { SCALAR_EXPR_CASE(expr,kI(z),kI(a),kI(b),vx,vy) }  \
   else if (0==at || 0==bt) { dp(&z,verb,a,b); }

/* Macro case: N:N, 1:N, N:1 implicit looping for fun call, w/ optional suffix */
#define SCALAR_EXPR_FUN(fun, cres, ca, cb, post)                       \
   if (an==bn) { DO(zn, cres [i]= fun (ca [i], cb [i]) post) }     \
   else if (an==1) { DO(zn, cres [i]= fun (ca [0], cb [i]) post) } \
   else /* bn==1 */ { DO(zn, cres [i]= fun (ca [i], cb [0]) post) }


S CSK(K x){ R !x?0:4==xt?*kS(x):3==ABS(xt)?kC(x):0;}//non-allocating CSTRING from K. assumes +4,+-3 types are null-terminated

K formKsCS(S s) 
{
  //Could remove this function. It's equivalent to Ks(sp(s))
  S t=sp(s);
  if(!t)R 0; //oom
  K z=Ks(t); //oom
  if(!z)R 0;
  R z;
}

K formKiCS(S s) //  0 $ "123\000456\000" is 123 ('\0' char)
{
  C *p,q=0;
  I r=IN;

  I w=parseNI(s,strlen(s));
  if(w) r=NI[w]; 
  else if(*s)
  {
    r=strtol(s,&p,10);
    errno=0; //is this ok to do?
    q=*p;
    if(IN==r)r=-II;//if r < -0I then r=-0I
  }
  P(q && !isblank(q),DOE)
  R Ki(r);//oom
}

K formKfCS(S s) // 0.0 $ "123\000456\000" is 123 ('\0' char)
{
  C *p,q=0;
  F r=FN;

  I w=parseNI(s,strlen(s));
  if(w) r=ni[w]; 
  else if(*s)
  {
    r=strtod(s,&p);
    errno=0;//is this ok to do?
    q=*p;
    if(isnan(r))r=-FI; //'r==FN' does not work
  }
  P(q && !isblank(q),DOE)
  R Kf(r); //oom
}

K formatS(S x)
{ I n=strlen(x);
  K z=newK(-3,n);
  if(z)sprintf(kC(z),"%s",x); //OK since 3/-3 is null-terminated
  R z;
}
K formatF(F x, I y, I c)
{ 
  S b= 0==c?"%.*g":1==c?"%.*f":"%.*e";// %#.*g ?? 
  I n=snprintf(0,0,b,y,x);  
  K z=newK(-3,n);
  if(z)sprintf(kC(z),b,y,x);
  R z;
}
K formatI(I x)
{ I n=snprintf(0,0,"%ld",x); 
  K z=newK(-3,n);
  if(z)sprintf(kC(z),"%ld",x);
  R z;
}
K format(K a) 
{
  I at=a->t, an=a->n;
  K z;
  if(3==ABS(at)){z=kclone(a); z->t=-3; R z;}
  else if(7==at)R 0;//TODO: wordfunc and charfunc and cfunc
  else if(6==at)R newK(-3,0);
  else if(5==at)R formatS(sp(".(..)"));//Beats me -- this has a similar signature to a _hash
  else if(4==at)R formatS(*kS(a));
  else if(2==at)R formatF(*kF(a),PP,0);
  else if(1==at)R formatI(*kI(a));
  z=newK(0,an);
  if     ( 0==at)DO(an, kK(z)[i]=format (kK(a)[i]))
  else if(-1==at)DO(an, kK(z)[i]=formatI(kI(a)[i]))
  else if(-2==at)DO(an, kK(z)[i]=formatF(kF(a)[i],PP,0))
  else if(-4==at)DO(an, kK(z)[i]=formatS(kS(a)[i]))
  R z;
}

I NI[]={0,IN,-II,II,II,-II,II};  //0N,-0I,0I,0n,-0i,0i maps to I
F ni[]={0,FN,-FI,FI,FN,-FI,FI};  //maps to F
I TNI(I p,C h) //transition function for parsing 0N -0I 0I 0n ...
{
  I c=isblank(h)?0:charpos(" -0NIni",h); //character classes
  if(0==c &&  7>=p)         R   p;
  if(1==c && (0==p || 7==p))R 7-p;
  if(2==c &&  0==p)         R   9;
  if(2==c &&  7==p)         R   8;
  if(3==c && (8==p || 9==p))R   1;
  if(4==c && (8==p || 9==p))R p-6;
  if(5==c && (8==p || 9==p))R   4;
  if(6==c && (8==p || 9==p))R p-3;
  R 10;
}
I parseNI(S s,I n){I i=0,p=0; while(i<n && *s)p=TNI(p,*s++); R p<7?p:0;}


//TODO: Really weird:  run '`g $ 99' run '. _d' see entry '(`s4;99;) in the `.k K-Tree
//      also run '`s $ 1.0' -> domain error
//TODO: oom all
K dollar(K a, K b) //form/format_dyadic
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  K z;
  I x = (at <=0 && -3 != at), y = (bt <=0 && -3 != bt);
  P(x && y && an!=bn,LE)

  if(x || y)
  {
    a=x?promote(a):ci(a); //-3
    b=y?promote(b):ci(b); //-3
    z=a&&b?newK(0,x?a->n:b->n):0;
    if(z)DO(z->n, K q=dollar(x?kK(a)[i]:a,y?kK(b)[i]:b); M(q,z,a,b) kK(z)[i]=q)
    cd(a);cd(b);
    R demote(z);
  }

  if(1==at && *kI(a)) //"Format (Dyadic)"
  {
    K c;
    U(c=format(b))
    I m=*kI(a); 
    z=newK(-3,ABS(m));M(c,z)
    if(z->n < c->n) DO(z->n, kC(z)[i]='*')//K3.2
    else
    {
      I k=m>0?m-c->n:0;
      DO(z->n,kC(z)[i]=' ');
      DO(c->n,kC(z)[i+k]=kC(c)[i])
    } 
    cd(c);
    R z;
  }

  if(2==at) //"Format (Dyadic)"
  {
    F f=*kF(a);
    if(2==bt || 1==bt)
    {
      K c,d;
      U(c=Ki(f))
      d=formatF(2==bt?*kF(b):*kI(b), ((I)tround(fabs(f)*10))%10, signbit(f)?2:1);
      if(d)z=dollar(c,d);
      cd(c);cd(d);
      R z;
    }
  }

  if(3==ABS(bt)) //"Form"
  {
    if(3==bt) b=enlist(b);//mm/o

    if(4==at && !strlen(*kS(a))) R formKsCS(CSK(b));
    if(3==ABS(at))               R ci(b);
    if(2==at)                    R formKfCS(CSK(b)); //Had '&& 0.0 == *kF(a)' here but the manual is wrong
    if(1==at && !*kI(a))         R formKiCS(CSK(b));
    //if(5<=at)
    R 0;//TODO: Else parse-execute (6,7, looks like for 5, maybe 4 too???)
  }

  R TE;
}


K itemAtIndex(K a, I i)// Return i-th item from any type as K - TODO: oom wherever this is used
{
  I at=a->t, an=a->n;   
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
K* EIA(K a,I i){R kK(a)+i;}         //dictionary entry's address of i-th index
K* EVP(K e){R EIA(e,1);}            //dictionary entry's value-pointer address (K*)
K* EAP(K e){R EIA(e,2);}            //dictionary entry's attribute_dictionary-pointer address (K*)
K   EV(K e){R *EVP(e);}             //dictionary entry's stored value

//Potential K3/4 bug (won't spend time looking):  names in functions (params,
//locals, global:: assigns, or global .k.a) are done at creation. if the named
//dict entries are deleted (what method) from K TREE what happens the next time
//you run the function?  does function context affect this?

//Weird: Found some bug in K3.2 were running .` would add a copy of the entries in the root of the K tree every time. Not sure how to reproduce
//K* denameBig(K dir_sym,K name_sym){R denameS(*kS(dir_sym),*kS(name_sym));} //[unnecessary?] wrapper for K-object inputs
K* denameS(S dir_string, S t)
{
  R denameD('.'==*t||!*t?&KTREE:denameD(&KTREE,dir_string),t);//duplicates '.' functionality in denameD to avoid dictionary initialization
}
K* denameD(K*d, S t)
{
  if(!simpleString(t)) R 0; //some kind of error
  R denameRecurse('.'==*t||!*t?&KTREE:d,t,1);
}
K* denameRecurse(K*p,S t,I create) 
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

  K e=lookupEntryOrCreate(p,k);  //To create a dename without path creation add a branch here that does lookup without create
  P(!e,(K*)ME)

  if('.'==*t && (!t[1] || '.'==t[1])) 
  {
    t++;
    p=EAP(e);    //attribute dict
  }
  else p=EVP(e); //value

  R denameRecurse(p,t,create);
}

K lookupEntryOrCreate(K *p, S k) //****only *dict or *_n are passed to here
{
  K a=*p, x;

  if(5==a->t) if(x=DE(a,k)) R x;

  P(!strlen(k),TE) //TODO verify this noting `. is not `
  P(strchr(k,'.'),DOE)

  x=newEntry(k);
  if(6==a->t){cd(*p); *p=newK(5,0);} //mm/o is this done right?
  kap(p,x); //oom
  cd(x);

  R x;
}

K* lookupEVOrCreate(K *p, S k){K x=lookupEntryOrCreate(p,k); R x?EVP(x):0; } //mm/o
K lookup(K a, S b){K x=DE(a,b); R x?EV(x):_n();} 


//TODO: oom at_verb everywhere
K at_verb(K a, K b)//[Internal Function]  TODO: should handle a is dict/directory & b is executable string like "1+1+c"
{//"of" depends on this even though @[d;i] = .[d;,i] <--- that equality doesn't always hold
  if(0==b->t && 0==b->n)R newK(0,0);//Overriding right-arg () 
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  K z,e;

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
    else if( 0==at){DO(bn,kK(z)[i]=ci(kK(a)[kI(b)[i]])) z=collapse(z);}
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

I updateIndex(K *p,I x, K r) //assert (*p)->t is <= 0 and valid x
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

I isColonDyadic(K x){R xt==7 && kW(x)[0] ==addressColon && !kW(x)[1];}

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
  I yt,yn; if(y){ yt=y->t; yn=y->n;}

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

    K k = 5==pt?Ks(0):Ki(0);
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
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
// DOT MONADIC, DYADIC, TRIADIC, TETRADIC
K of2(K d, K *x, K *y, I s)
{
  K f=*x;
  I dt=d->t, dn=d->n, ft=f->t, fn=f->n;

  if(0>=s)R at_verb(d,f); //Is it at_verb or at()...  ?

  K r=y?*y:0; //Don't dereference y if s is too small

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
    R;
  }
  //these may turn out to be the "ELSE" case
  if((1 <= dt && dt <= 4) || 7==dt || 7==ft) R RE;
  else if(6==dt && (0 >= ft)) R XE;
  else if(6==dt && 6 != ft && 4 != ft) R TE;
  if(5==dt && 123 == ft) R ; //TODO: Fill in dict errors
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
      I e = kI(f)[i];
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
    if(6==dt) R; //identity
    if(y && !atomI(y) &&  yn != d->n) R LE;
    if(y) U(y=promote(y))
    if(5==dt) DO(d->n, dot_ref(EVP(DI(d,i)),z,z+1,s-1,c,y?kK(y)[i%yn]:0))
    if(0>=dt) { K k=Ki(0); M(k,y?y:k);  DO(countI(d), *kI(k)=i; dot_ref(p,&k,z,s,c,y?kK(y)[i%yn]:0)) cd(k); }
    cd(y);
  }

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
  I t=a->t, n=a->n;
  K x,y; 
  K z=newK(5,n);
  DO(n, kK(z)[i]=newK(0,3);)
  DO(n, x=kK(z)[i]; y=kK(a)[i]; DO2(y->n,kK(x)[j]=y->t?Ks(kS(y)[j]):ci(kK(y)[j])) if(y->n<3)kK(x)[2]=_n())  //oom
  R z;
}
K unmake(K a){K z=kclone(a); z->t=0; R z;}//TODO: deep clone inefficient
K makeable(K a) //TODO: this has to be reworked. can't hang out raw in dot_monadic as it is currently
{
  I t=a->t, n=a->n;
  //All this was moved here from make(). not sure how to handle error checking when it's outside like this
  P(0!=t, 0)
  K x,y;
  //NB: .(`a`b;`c`d) is also a valid dictionary (sym vectors)
  DO(n, x=kK(a)[i]; if( (0!=x->t && -4!=x->t) || x->n < 2 || 3 < x->n || (-4==x->t && x->n != 2) )R 0)
  DO(n, x=kK(a)[i]; if(0==x->t) if( 4 != kK(x)[0]->t || (3==x->n && 5!=kK(x)[2]->t && 6!=kK(x)[2]->t)) R 0)
  R (K)1;
}
K dot_monadic(K x){K d; R 3==ABS(xt)?KX(x):4==xt?ci(*denameS(__d,*kS(x))):5==xt?unmake(x):makeable(x)?make(x):vf_ex(addressDot,x); }  //TODO: mmo. untested everywhere. TODO: esp mmo _() underscore function
//END DOT
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////

S notsp(S a)
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

//////////////////////////////////////////////////////////////////////////
//question mark - find/function_inverse - what dyadic triadic
K find(K a, K b)
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  P(at>0,DOE)
  if(-4==at && 4==bt)DO(an, if(kS(a)[i]==*kS(b))R Ki(i)) 
  if(-3==at && 3==bt)DO(an, if(kC(a)[i]==*kC(b))R Ki(i))
  if(-2==at && 2==bt)DO(an, if(!FC(kF(a)[i],*kF(b)))R Ki(i))
  if(-2==at && 1==bt)DO(an, if(!FC(kF(a)[i],*kI(b)))R Ki(i))
  if(-1==at && 2==bt)DO(an, if(!FC(kI(a)[i],*kF(b)))R Ki(i))
  if(-1==at && 1==bt)DO(an, if(kI(a)[i]==*kI(b))R Ki(i))
  if(!at)DO(an, if(matchI(kK(a)[i],b))R Ki(i))
  R Ki(an);
}

F num_ex(K a, F x)//f-> monadic, numeric in&out
{ 
  F y=0;
  K b,g;
  P(!(b=Kf(x)),FN) //err

  if(!(g=newK(0,1))){cd(b); R FN;}//err
  *kK(g)=ci(b);
  K k=vf_ex(&a,g);

  if(!k || (k->t!=1 && k->t!=2))y=FN; //err
  else if(k->t==1) y=(F)*kI(k);
  else y=*kF(k);

  cd(b);
  cd(k);
  cd(g);
  R y;
}

I isShallowNumeric(K k)
{
  if(ABS(k->t) > 2) R 0;
  if(0==k->t) DO(k->n, I t=kK(k)[i]->t; if(t!=1 && t!=2) R 0)
  R 1;
}

F ithFloat(K k, I i) //made specific for what_triadic
{
  if(!k) R 0;
  I n=k->n;
  if(!k->t) {k=kK(k)[i%n]; i=0;}
  if(1==ABS(k->t)) R (F) kI(k)[i%n];
  R kF(k)[i%n];
}

F inverter(K a, K b, K c, I index)//secant method
{
  F y = ithFloat(b,index);

  I i,m=20;//max iterations
  F x[m+2], f[m+2];
  x[0]=0.9998;
  x[1]=0.9999;

  if(c)
  {
    F r=ithFloat(c,index);
    //TODO: r== 0n 0i etc ??
    x[0]=0.9999*r;
    x[1]=r;
  }

  DO(2, f[i]=num_ex(a,x[i])-y); //oom/err FN ?? how to catch
  F d, e=y*0.000001;//y*1e-6

  for(i=0;i<m;i++)
  { d=(x[i+1]-x[i])/(f[i+1]-f[i])*f[i+1];
    x[i+2]=x[i+1]-d;
    f[i+2]= num_ex(a,x[i+2])-y; //oom/err FN ?? how to catch
    if(ABS(d) < e || 0==f[i+2]) break;
  }
  if(i>=m){ kerr("limit"); R 0;}
  R x[i+2];
}

K what_triadic(K a, K b, K c)//TODO: 0i -0i 0n
{
  //TODO:  {1}?1 -> 0n ??
  I bt=b->t, bn=b->n;
  if(!isShallowNumeric(b) || (c && !isShallowNumeric(c))) R TE;
  if((!bt && !bn) || (c && !c->t && !c->n)) R newK(0,0);
  if(0==bn || (c && 0==c->n)) R newK(-2,0);
  if(c && c->t < 1 && bt < 1 && c->n != b->n) R LE;

  I zn=bn, zt=2;
  if(bt<1 || (c && c->t < 1)) zt = -2;
  if(c) zn=MAX(zn,c->n);
  K z = newK(zt,zn);
  U(z)
  DO(zn, kF(z)[i] = inverter(a,b,c,i))
  R z;
}

K qrand(K a,K b)
{
  I at=a->t,an=a->n,bt=b->t,bn=b->n;
  K y;
  P(1!=ABS(at)||(1!=bt&&2!=bt),IE)
  I c=*kI(a),n=ABS(c);
  P(1==bt && c<0 && *kI(b) < -c,LE)
  P(1==bt && *kI(b)<0,DOE)

  I j=0,k,s;
  U(y=newK(1==bt?-1:-2,n))

  if(2==bt){F f=*kF(b);DO(n,kF(y)[i]=RF()*f) R y;}
  I d=*kI(b);
  if(c>=0)DO(n,kI(y)[i]=d*RF())    //this could be better (small numerical error)
  else //deal
  {
    vitter(kI(y),y->n,d); //Vitter's algorithm
    for(j=n-1;j>0;j--){k=(1+j)*RF();s=kI(y)[j];kI(y)[j]=kI(y)[k];kI(y)[k]=s;} //Knuth Algorithm 3.4.2P
  }
  R y;
}

K sample(K x,K y)
{
  K a,b,z;
  if(!y->n) R take(x,y);
  U(b=Ki(countI(y)))
  a=qrand(x,b);
  M(a,b) cd(b);
  z=at_verb(y,a);
  cd(a);
  R z;
}

K what(K x, K y)
{
  if(7==xt)R what_triadic(x,y,0);
  if(1==xt) R atomI(y)?qrand(x,y):sample(x,y); 
  R find(x,y);
}
//////////////////////////////////////////////////////////////////////////


I matchI(K a, K b)
{
  if(!a||!b)R 0;//Using this in over adverb type stuff
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  I AT=ABS(at), BT=ABS(bt);
  K *c,*d;
  //if(an!=bn || (at!=bt && !(1==AT && 2==BT) && !(2==AT && 1==BT)))R 0; // 0 ~ 1.0 ~ 1
  if(an!=bn || at!=bt) R 0;
  if(4==AT)DO(an, if(kS(a)[i]!=kS(b)[i]) R 0 )
  if(3==AT)DO(an, if(kC(a)[i]!=kC(b)[i]) R 0 )
  if(2==AT && 2==BT)DO(an, if(FC(kF(a)[i],kF(b)[i])) R 0 )
  //if(2==AT && 1==BT)DO(an, if(FC(kF(a)[i],kI(b)[i])) R 0 ) 
  //if(1==AT && 2==BT)DO(an, if(FC(kI(a)[i],kF(b)[i])) R 0 )
  if(1==AT && 1==BT)DO(an, if(kI(a)[i]!=kI(b)[i]) R 0 )
  if(0==AT || 5==AT)DO(an, if(!matchI(kK(a)[i],kK(b)[i]))R 0)//Dictionary keys are ordered sets
  if(7==AT)  
  {
    if(a->n!=b->n) R 0;
    
    switch(a->n)
    {
      CS(1,
            an=kVC(a)->n-1;
            bn=kVC(b)->n-1;
            if(an!=bn) R 0;
            DO(an, c=kW(a)[i];d=kW(b)[i]; if(VA(c)||VA(d)){if(c!=d) R 0;} else if(!matchI(*c,*d)) R 0) //TODO: Projection (up above?)
        )
      CS(2, )//TODO
      CS(3, if(kV(a)[CONTEXT] != kV(b)[CONTEXT])R 0; R matchI(kV(a)[CODE],kV(b)[CODE])) //TODO: Projection (up above?)
    }
  }
  R 1;
}
K enlist(K x)
{
  I t=(1<= xt && xt<=4)?-xt:0; //Atoms become vectors. Else becomes list.
  K z=newK(t,1);
  if(-4==t)*kS(z)=*kS(x);
  if(-3==t)*kC(z)=*kC(x);
  if(-2==t)*kF(z)=*kF(x);
  if(-1==t)*kI(z)=*kI(x);
  if( 0==t)*kK(z)=ci(x);
  R z;
}


//TODO: sort type-0 lists, functions, symbols, etc.
//note: K appears to sort _reserved functions by alphabetical order, but this may be a side effect of
//      sorting by address (if the functions appear in alphabetical order in the source file)
//      though oddly enough it appears to hold across all underscore/reserved functions (not just math)
//      _in falls to the end since it is defined as some variation on a char func that works as an inline verb
//      If you look at the binary the symbols appear to be listed out of alphabetical order
//one interesting way to test how functions are sorted would be to load the interpreter twice,
//inputting the same collection of functions each time but in a different order
//if the sort order changes for each instance then sorting is probably based on pointer/reference value
//if that fails then it may be necessary to look at distinctions between wordfunc,charfunc, valence, proj, etc
K grade_updown(K a, I r)
{
  I at=a->t, an=a->n;
  P(0< at, RE)
  if(-3==at) R charGrade(a,r);
  if(-1==at)
  {
    I x,u=II,v=-II;//MIN,MAX
    DO(an, x=kI(a)[i]; if(x<u)u=x; if(x>v)v=x;)  
    if(v-u < 87654321) R distributionGrade(a,r,u,v);//Magic Number
  }
  if(-1==at || -2==at)
  {
    //TODO: Attempt [Recursive] [Histogram] Bucket[sort] Grade if OK distribution
  }
  R mergeGrade(a,r);
}
K grade_up(K a){R grade_updown(a,0);}
K grade_down(K a){R grade_updown(a,1);}

K lessmore(K a, K b, I x)
{

  if (!x){K c=a;a=b;b=c; } //NB: If primitives modify a but not b (or vice-versa. e.g. reuse of refcount 1 objects) 
                           //this should be reviewed. in q it can effect dicts (borror). see backup for unfactored ver.
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  if(at <=0 && bt <= 0 && an != bn) R LE;
  I AT=ABS(at), BT=ABS(bt);
  if(4<AT || 4<BT) R TE;//also Type Error (this catches it before descending)
  // Type Error - No 0-list, Not both numeric, or both char, or both sym
  if(at && bt && !(2>=AT && 2>=BT) && !(3==AT && 3==BT) && !(4==AT && 4==BT) ) R TE;
  I t= (!at||!bt)?0:MIN(at,bt)<0?-1:1;//Any 0-list? Zero. Any vector? -1. Both atoms? 1.
  I zn=at>0?bn:an;
  K z=newK(t,zn);
  U(z)
  I*h=kI(z);

  if(0==at || 0==bt)
  {
   a=promote(a); b=promote(b); //copy-pasted from dp()
   M(a,b,z);
   DO(zn, if(!(kK(z)[i]=lessmore(kK(a)[i%an],kK(b)[i%b->n],x))){cd(z);z=ME;break;})
   cd(a);cd(b); 
  }
  else 
  {
#define GT(x, y) (x) > (y)
    if     (2==AT && 2==BT) DO(zn, h[i]=0<FC(kF(a)[i%an],kF(b)[i%bn]))
    else if(2==AT && 1==BT) DO(zn, h[i]=0<FC(kF(a)[i%an],kI(b)[i%bn]))
    else if(1==AT && 2==BT) DO(zn, h[i]=0<FC(kI(a)[i%an],kF(b)[i%bn]))
    else if(1==AT && 1==BT) SCALAR_OP_CASE(GT, kI(z), kI(a), kI(b))
    else if(3==AT && 3==BT) SCALAR_OP_CASE(GT, kI(z), kC(a), kC(b))
    else if(4==AT && 4==BT) DO(zn, h[i]=0<SC(kS(a)[i%an],kS(b)[i%bn]))
#undef GT
  }
  
  R z;
}

K less(K a, K b){R lessmore(a,b,0);}
K more(K a, K b){R lessmore(a,b,1);}

K match(K a, K b){R Ki(matchI(a,b));}

K range(K a)
{ 
  I t=a->t, n=a->n;
  P(t>0,RE)
  K z=0,g=0,k=0;
  
  I u=n;
  g=grade_up(a); if(!g) GC;
  k=grade_up(g); if(!k) GC;

  I *h=kI(g);
  if(-4==t)DO(n-1, if(kS(a)[h[n-i-1]]==kS(a)[h[n-i-2]])    {h[n-i-1]=-1;--u;})
  if(-3==t)DO(n-1, if(kC(a)[h[n-i-1]]==kC(a)[h[n-i-2]])    {h[n-i-1]=-1;--u;})
  if(-2==t)DO(n-1, if(!FC(kF(a)[h[n-i-1]],kF(a)[h[n-i-2]])){h[n-i-1]=-1;--u;})
  if(-1==t)DO(n-1, if(kI(a)[h[n-i-1]]==kI(a)[h[n-i-2]])    {h[n-i-1]=-1;--u;})
  if( 0==t)DO(n-1, if(matchI(kK(a)[h[n-i-1]],kK(a)[h[n-i-2]]))   {h[n-i-1]=-1;--u;})

  z=newK(t,u); if(!z) GC;
  I x=0;

  I *m=kI(k); //This could be refactored
  if(-4==t)DO(n, if(h[m[i]]>-1)kS(z)[x++]=kS(a)[h[m[i]]] )
  if(-3==t)DO(n, if(h[m[i]]>-1)kC(z)[x++]=kC(a)[h[m[i]]] )
  if(-2==t)DO(n, if(h[m[i]]>-1)kF(z)[x++]=kF(a)[h[m[i]]] )
  if(-1==t)DO(n, if(h[m[i]]>-1)kI(z)[x++]=kI(a)[h[m[i]]] )
  if( 0==t)DO(n, if(h[m[i]]>-1)kK(z)[x++]=ci(kK(a)[h[m[i]]]))

cleanup:
  cd(g);
  cd(k);
  R z;
}

K group(K x)
{
  I t=xt, n=xn;
  P(t>0,RE)
  
  I u=n;
  K z,b,c; //was K z=0,b=0,c=0;
  M(b=grade_up(x))
  M(b,c=grade_up(b))//Nastier code would eliminate this second sort.
  I *g=kI(b);//Step through, on duplicate set uniques-=1, mark by inverting sign of corresponding index
  I *h=kI(c);
  if(-4==t)DO(n-1, if(kS(x)[g[n-i-1]]==kS(x)[g[n-i-2]])       {--u;g[n-i-1]*=-1;})
  if(-3==t)DO(n-1, if(kC(x)[g[n-i-1]]==kC(x)[g[n-i-2]])       {--u;g[n-i-1]*=-1;})
  if(-2==t)DO(n-1, if(!FC(kF(x)[g[n-i-1]],kF(x)[g[n-i-2]]))   {--u;g[n-i-1]*=-1;})
  if(-1==t)DO(n-1, if(kI(x)[g[n-i-1]]==kI(x)[g[n-i-2]])       {--u;g[n-i-1]*=-1;})
  if( 0==t)DO(n-1, if(matchI(kK(x)[g[n-i-1]],kK(x)[g[n-i-2]])){--u;g[n-i-1]*=-1;})
 
  z=newK(0,u);
  M(b,c,z);
  I k=0,p=0,v;
  while(p<n && k<u)//This is a tricky algorithm.
  { //Dupes in g marked negative. h[p] is index of a[p] in sorted a
    for(v=1;p+v<n && g[h[p]+v]<0;v++);//Find the length of z[k]
    K s=newK(-1,v); 
    M(b,c,z,s)
    DO(v, kI(s)[i]=ABS(g[h[p]+i]))//ABS because duplicates marked negative
    kK(z)[k]=s;
    while(++p<n && g[h[p]]<0);
    k++;
  }
  cd(b);
  cd(c);
  R z;
}

I VAT(I i){R 1<=i && i<=4?i:0;} //vector atom type
K flip(K a)
{
  K x;I i,p=a->n,q=-1; 
  if(a->t || !p)R ci(a);//Identity on atoms/vectors && empty 0-list && 0-list of atoms
  DO(p, x=kK(a)[i]; if(x->t<1)q=x->n);
  if(-1==q)R ci(a);//Identity on 0-list of atoms
  DO(p, x=kK(a)[i]; if(x->t<1 && x->n!=q)R LE;)
  K z=newK(0,q);  //mmo
  for(i=0;i<q;i++)//This kind of thing is always tricky.
  {
    K* c=kK(a); K d=c[0];//Temporary variables
    I u,t=-ABS(d->t?d->t:VAT(u=kK(d)[i]->t)?u:0);//Starting type: vector's type or i-th-item-in-a-list's type
    DO2(p, d=c[j]; t=t==-ABS(d->t?d->t:VAT(u=kK(d)[i]->t)?u:0)?t:0 )//DO2: Get type. Flip won't pr0mote I to F
    K y=kK(z)[i]=newK(t,p); //oom
    if     (-4==t) DO2(p, d=c[j]; kS(y)[j]=d->t?kS(d)[i% d->n]:*kS(kK(d)[i]) )
    else if(-3==t) DO2(p, d=c[j]; kC(y)[j]=d->t?kC(d)[i% d->n]:*kC(kK(d)[i]) )
    else if(-2==t) DO2(p, d=c[j]; kF(y)[j]=d->t?kF(d)[i% d->n]:*kF(kK(d)[i]) )
    else if(-1==t) DO2(p, d=c[j]; kI(y)[j]=d->t?kI(d)[i% d->n]:*kI(kK(d)[i]) )
    else if( 0==t) DO2(p, d=c[j]; kK(y)[j]=itemAtIndex(d,i)) //mmo
  }
  R z;
}

K first(K a)
{ //Empty lists return prototypical atoms, e.g., *0#0.0 yields 0.0 
  I at=a->t, an=a->n;
  if(-4==at)R Ks(an?*kS(a):LS);
  if(-3==at)R Kc(an?*kC(a):' ');//Vectors
  if(-2==at)R Kf(an?*kF(a):0.0);
  if(-1==at)R Ki(an?*kI(a):0);
  if( 0==at)R an?ci(*kK(a)):_n();//Lists - *() yields _n
  R ci(a);//Atoms
}

K reshaper(K a, K b, I d, I f, I* p)
{ //a is non-empty int vector with: (0 0s, 0 -1s),(1 -1),or(1+ 0s)
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  I v=kI(a)[d];
  I g=!v||a->n==d+1?1:0;//hit bottom?
  I t= (g && bt<5)?-ABS(bt):0;// 2 3 4 0 #/: (_n;{x}) 
  I n=-1==v?f:v;//f is missing factor
  K z=newK(t,n); U(z)
  if(!g)DO(n,kK(z)[i]=reshaper(a,b,d+1,f,p))
  else if(4==ABS(bt))DO(n,kS(z)[i]=bn?kS(b)[++*p%bn]:LS)
  else if(3==ABS(bt))DO(n,kC(z)[i]=bn?kC(b)[++*p%bn]:' ')
  else if(2==ABS(bt))DO(n,kF(z)[i]=bn?kF(b)[++*p%bn]:0.0)
  else if(1==ABS(bt))DO(n,kI(z)[i]=bn?kI(b)[++*p%bn]:0)
  else if(0==ABS(bt))DO(n,kK(z)[i]=bn?ci(kK(b)[++*p%bn]):_n())
  else if(5<=    bt )DO(n,kK(z)[i]=ci(b))
  R z;
}

K reshape(K a, K b)
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  if(!an)R first(b);//sic
  I ns=0,np=-1,x,y=-1;
  DO(an, if(0>(x=kI(a)[i])){np=i;ns-=x;})//If any <0, only one -1
  P(ns < -1,DOE)
  I p=1; DO(an, p*=kI(a)[i])//Product over
  P(ns<0 && (!p || !bn || bn%p),LE)
  R reshaper(a,b,0,p?ABS(bn/p):0,&y);
}

K take(K a, K b)
{
 I at=a->t, an=a->n, bt=b->t, bn=b->n;
 I n=ABS(*kI(a)), m=MAX(1,bn), k= *kI(a) % m;
 k=k<0?bn+k:0;
 I t=bt<5?-ABS(bt):0;
 K z=newK(t,n);
 if     (4==ABS(bt))DO(n,kS(z)[i]=bn?kS(b)[(i+k)%m]:LS) //sp("")
 else if(3==ABS(bt))DO(n,kC(z)[i]=bn?kC(b)[(i+k)%m]:' ')
 else if(2==ABS(bt))DO(n,kF(z)[i]=bn?kF(b)[(i+k)%m]:0.0)
 else if(1==ABS(bt))DO(n,kI(z)[i]=bn?kI(b)[(i+k)%m]:0)
 else if(0==    bt )DO(n,kK(z)[i]=bn?ci(kK(b)[(i+k)%m]):_n()) 
 else if(5<=    bt )DO(n,kK(z)[i]=ci(b))
 R z;
}
K take_reshape(K a, K b)
{ //K3.2 will accept empty lists that aren't type -1 (as left arg)
  P(!a->n && 1!=ABS(a->t),IE)
  R 0<a->t?take(a,b):reshape(a,b);
}

void shapeCheck(K a, K p, I d)
{ //Descend through list a marking shape p as -1 where it doesn't correspond
  I at=a->t, an=a->n;
  if(at>0 || an!=kI(p)[d]) kI(p)[d]=-1;//Mismatch or atom means p length too long
  else if(at && d < p->n-1) kI(p)[d+1]=-1;//Another case of p being too long 
  else if(!at && an && kI(p)[d]!=-1 && d < p->n-1) DO(an, shapeCheck(kK(a)[i],p,d+1))
}
I firstDepth(K x){R (!x->t&&x->n)?1+firstDepth(*kK(x)):x->t>0?0:1;}//[Internal Function]

K shape(K a) //TODO: Thoroughly test this //TODO: oom
{ 
  I at=a->t, an=a->n;
  K b=a, p=newK(-1, firstDepth(a));//Putative list. Mutable, Thrown away
  DO(p->n, kI(p)[i]=b->n; if(i<_i-1)b=*kK(b) )//Construct best-case shape
  shapeCheck(a,p,0);//Punch holes (-1) in shape-list where it fails
  I n=0; DO(p->n, if(kI(p)[i]==-1)break; n++)//See how far it made it
  K z=newK(-1,n);
  DO(n, kI(z)[i]=kI(p)[i])//Copy the good part. 
  cd(p);
  R z;//could instead shrink p into z
}

F tround(F f){F d=FF(f); R (d>0&&!FC(d,1))||(d<0&&!FC(d,0))?ceil(f):floor(f);}

K floor_ceil(K a, F(*g)(F))
{
  I at=a->t, an=a->n;
  F(*h)(F)=g==ceil?floor:ceil;
  P(2 <ABS(at),TE)
  if(1==ABS(at))R ci(a);

  //TODO: oom
  K z=newK(at?SIGN(at):0,an);//Compress F {-2,2} into I {-1,1}
  F e,f;
  if(2==ABS(at)) DO(an, e=kF(a)[i]; f=FF(e); kI(z)[i]=(f>0&&!FC(f,1))||(f<0&&!FC(f,0))?h(e):g(e))
  else if(!at) DO(an, kK(z)[i]=floor_ceil(kK(a)[i],g))
  R z;
}

K floor_verb(K a){R floor_ceil(a,floor);}//K3.2  "_ -5 + 1.0 * 1 + -OI" yields -0I not Domain Error

K dp(K*z,K(*f)(K,K),K x,K y) //dyad promote
{
   x=promote(x); y=promote(y);
   M(x,y,*z)
   DO((*z)->n, if(!(kK(*z)[i]=f(kK(x)[i%xn],kK(y)[i%y->n]))){cd(*z);*z=ME;break;})
   cd(x);cd(y); 
}


K equals(K a, K b)
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  if(at <=0 && bt <= 0 && an != bn) R LE;
  I AT=ABS(at), BT=ABS(bt);
  if(4<AT || 4<BT) R TE;//(this catches it before descending)
  //Type Error - No 0-list, Not both numeric, or both char, or both sym
  if(at && bt && !(2>=AT && 2>=BT) && !(3==AT && 3==BT) && !(4==AT && 4==BT) ) R TE;
  I t= (!at||!bt)?0:MIN(at,bt)<0?-1:1;//Any 0-list? Zero. Any vector? -1. Both atoms? 1.
  I zn=at>0?bn:an;
  K z=newK(t,zn); //oom
#define EQ(x, y) (x) == (y)
  if     (2==AT && 2==BT) SCALAR_EXPR_FUN(FC, kI(z), kF(a), kF(b), ?0:1)
  else if(2==AT && 1==BT) SCALAR_EXPR_FUN(FC, kI(z), kF(a), kI(b), ?0:1)
  else if(1==AT && 2==BT) SCALAR_EXPR_FUN(FC, kI(z), kI(a), kF(b), ?0:1)
  else if(1==AT && 1==BT) SCALAR_OP_CASE(EQ, kI(z), kI(a), kI(b))
  else if(3==AT && 3==BT) SCALAR_OP_CASE(EQ, kI(z), kC(a), kC(b))
  else if(4==AT && 4==BT) SCALAR_OP_CASE(EQ, kI(z), kS(a), kS(b)) //works because of interning
  else if(0==at || 0==bt) dp(&z,equals,a,b);
#undef EQ
  R z;
}


K rotate(K a, K b)
{
  I bt=b->t, bn=b->n;//Know 1==a->t and 0>=b->t
  I r=*kI(a) % MAX(1,bn);// x%0 -> division error
  r=r>0?r:bn+r;//Ensure mathematical definition of modulo
  K z=newK(bt,bn);U(z)
  if     (-4==bt)DO(bn, kS(z)[i]=kS(b)[(i+r)%bn])
  else if(-3==bt)DO(bn, kC(z)[i]=kC(b)[(i+r)%bn])
  else if(-2==bt)DO(bn, kF(z)[i]=kF(b)[(i+r)%bn])
  else if(-1==bt)DO(bn, kI(z)[i]=kI(b)[(i+r)%bn])
  else if( 0==bt)DO(bn, kK(z)[i]=ci(kK(b)[(i+r)%bn]))
  R z; 
}

K mod(K a, K b) //In K4: {x-y*x div y}
{
  I at=a->t, an=a->n, bt=b->t;
  P(ABS(at) > 2,TE)
  //Know bt in 1,2  and  at in -2,-1,0,1,2
  I t=(0==at)?0:MAX(ABS(at),ABS(bt))*(at>0?1:-1);

  K z=newK(t,an); U(z)
  I c,d,e; F f,g,h;
  #define FMOD h=g?f-g*floor(f/g):f; kF(z)[i]=h;
  if     (2==ABS(at) && 2==bt) DO(an, f=kF(a)[i]; g=*kF(b); FMOD)
  else if(2==ABS(at) && 1==bt) DO(an, f=kF(a)[i]; g=*kI(b); FMOD)
  else if(1==ABS(at) && 2==bt) DO(an, f=kI(a)[i]; g=*kF(b); FMOD)
  else if(1==ABS(at) && 1==bt) DO(an, c=kI(a)[i]; d=*kI(b); e=d?c-d*floor(c/(F)d):c; kI(z)[i]=e) //TODO: casting to F is slow/wrong for big#
  else if(0==at) DO(an, if(!(kK(z)[i]=mod(kK(a)[i],b))){cd(z);R 0;}) 
  R z;
}

K rotate_mod(K a, K b)
{
  P(b->t > 2,TE)
  P(!(1==a->t || b->t > 0), IE)
  R (b->t < 1)?rotate(a,b):mod(a,b);
}

K drop(K a, K b)
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;

  P(1!=at,IE)
  if(bt>0)R ci(b);//Drop always identity on atoms
  
  I v=*kI(a);
  I zn=MAX(0,bn-ABS(v));
  K z=newK(bt,zn);
  U(z)
  I c=v<1?0:MIN(v,bn);

  if     (-4==bt)DO(zn,kS(z)[i]=kS(b)[i+c])
  else if(-3==bt)DO(zn,kC(z)[i]=kC(b)[i+c])
  else if(-2==bt)DO(zn,kF(z)[i]=kF(b)[i+c])
  else if(-1==bt)DO(zn,kI(z)[i]=kI(b)[i+c])
  else if( 0==bt)DO(zn,kK(z)[i]=ci(kK(b)[i+c]))

  R z;
}

K cut(K a, K b)
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  P(-1!=at,IE)

  DO(an, I x=kI(a)[i]; if(x<0|| x < kI(a)[i>0?i-1:0])R DOE; else if(x > bn)R LE )
  K z=newK(0,an);
  U(z)
  I zn=z->n;

  //TODO: oom
  #define FCUT I x=kI(a)[i],y=(i==z->n-1)?bn:kI(a)[i+1];
  if     (-4==bt) DO(zn, FCUT; K w=newK(-4,y-x); DO2(w->n,kS(w)[j]=kS(b)[x+j]); kK(z)[i]=w)
  else if(-3==bt) DO(zn, FCUT; K w=newK(-3,y-x); DO2(w->n,kC(w)[j]=kC(b)[x+j]); kK(z)[i]=w)
  else if(-2==bt) DO(zn, FCUT; K w=newK(-2,y-x); DO2(w->n,kF(w)[j]=kF(b)[x+j]); kK(z)[i]=w)
  else if(-1==bt) DO(zn, FCUT; K w=newK(-1,y-x); DO2(w->n,kI(w)[j]=kI(b)[x+j]); kK(z)[i]=w)
  else if( 0==bt) //Have to check if we have subsequences of similarly-typed atoms
  { I i;
    for(i=0;i<zn;i++)
    { FCUT;
      I sn=y-x;//Size of sublist
      I t=bt;//Empty sublists in z will inherit b's type
      if(sn && x < bn)t=kK(b)[x]->t;//Non-empty sublist, valid index?
      DO2(sn, if(t!=kK(b)[x+j]->t){t=0;break;})//Check for consistency
      t=-MAX(0,t); //Atom sequences become vectors, but nothing special for vectors
      K s=newK(t,sn);//Sublist for z
      if     (-4==t)DO2(sn,kS(s)[j]=*kS(kK(b)[x+j]))
      else if(-3==t)DO2(sn,kC(s)[j]=*kC(kK(b)[x+j]))
      else if(-2==t)DO2(sn,kF(s)[j]=*kF(kK(b)[x+j]))
      else if(-1==t)DO2(sn,kI(s)[j]=*kI(kK(b)[x+j]))
      else if( 0==t)DO2(sn,kK(s)[j]=ci(kK(b)[x+j]))
      kK(z)[i]=s;
    }
  }
  R z;
}

K drop_cut(K a, K b)
{
  if(1 != ABS(a->t) || (-1==a->t && 0<b->t))R IE;
  R 1==a->t?drop(a,b):cut(a,b);
}

K power(K a, K b)
{
  I at=a->t, an=a->n, bt=b->t, bn=b->n;
  I type = MAX(ABS(at),ABS(bt));

  P(at <= 0 && bt <= 0 && an != bn, LE)
  P(type > 2, TE);

  I zt=type;                    
  if(MIN(at,bt) < 1) zt=-zt;    
  if(!at || !bt) zt=0;          
  if(1==zt*zt)zt*=2;		
  I zn=at>0?bn:an;              
  K z=newK(zt,zn); U(z)             

  F x,y;
  //K3.2 silently yields 0n for -3^0.5 , even though some Kx documentation says domain error.
  #define FPOWER kF(z)[i]=(0==y)?1:(0==x)?0:pow(x,y); //x^0==1; 0^y==0 for y!=0; rest should be same as pow
  SCALAR_EXPR(FPOWER,power,x,y)

  R z;
}

K where(K x)
{
  P(!xn,newK(-1,0))
  P(1!=ABS(xt),IE)
  I zn=0,y,j,t=0;
  DO(xn,if((y=kI(x)[i])<0)R DOE;zn+=y)
  K z=newK(-1,zn); U(z)
  DO(xn, for(j=0;j<kI(x)[i];j++)kI(z)[t++]=i)//Handles a-> == +-1 
  R z;
}

//TODO: The smarter way to do this is to write it in such a way that it can return the same input (e.g., if refcount == 1?, then use a temp holder and do the swaps in pairs)
K reverse(K a)
{
  I at=a->t,an=a->n;
  if(0<at)R ci(a);//Atoms
  K z=newK(at,an); U(z)
  if     (-4==at) DO(an,kS(z)[i]=kS(a)[an-i-1]) //This could all be refactored
  else if(-3==at) DO(an,kC(z)[i]=kC(a)[an-i-1])
  else if(-2==at) DO(an,kF(z)[i]=kF(a)[an-i-1])
  else if(-1==at) DO(an,kI(z)[i]=kI(a)[an-i-1])
  else if( 0==at) DO(an,kK(z)[i]=ci(kK(a)[an-i-1]))
  R z;
}

I atomI(K a){R a->t>0?1:0;}//_n is atom
K atom(K a){R Ki(atomI(a));}//_n is atom
I countI(K a){R a->t>0?1:a->n;}
K count(K a){R Ki(countI(a));}//[sic] Should always be 1 for an atom (t of 5,7 may have different n)

K join(K a, K b)//TODO: 5,6?
{
  I at=a->t, ak=countI(a), bt=b->t, bk=countI(b);

  I zt=0;
  if(ABS(at)==ABS(bt))zt=-ABS(at);//K-Improvement?: ABS(at)=1or2 && ABS(bt)==1or2 should yield zt==-2
  if(!ak)zt=-ABS(bt); 
  else if(!bk)zt=-ABS(at);//'else' is sic. In "K3.21 2006-02-01" right empty list takes precedence
  if(zt < -4)zt=0;
  I zn=ak+bk;
  K z=newK(zt,zn);U(z)

  //TODO: all this should be replaced with memcpy calls
  if     (-4==zt){DO(ak,kS(z)[i]=kS(a)[i]) DO(bk,kS(z)[ak+i]=kS(b)[i])}
  else if(-3==zt){DO(ak,kC(z)[i]=kC(a)[i]) DO(bk,kC(z)[ak+i]=kC(b)[i])}
  else if(-2==zt){DO(ak,kF(z)[i]=kF(a)[i]) DO(bk,kF(z)[ak+i]=kF(b)[i])}
  else if(-1==zt){DO(ak,kI(z)[i]=kI(a)[i]) DO(bk,kI(z)[ak+i]=kI(b)[i])}
  else if( 0==zt)
  {
    //oom all here
    K c=promote(a);
    K d=promote(b);
    DO(ak,kK(z)[i]=ci(kK(c)[i])) DO(bk,kK(z)[ak+i]=ci(kK(d)[i]))
    cd(c);cd(d);
  }
  R z;
}

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
  else if(1==t || 2==t){I n= t==1?*kI(a):(I)*kF(a); P(n<0,DOE) z=newK(-1,n); U(z) DO(n,kI(z)[i]=i)}//could instead be (in)?tolerant ceil/floor
  else R DOE;//Domain Error? Type Error on '!1 2 3' 
  R z;
}

K plus(K a, K b) //compare plus() to times() or minus()
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)          //Finally, we know what we're going to make

  #define PLUS(x, y) ((x) + (y))
  SCALAR_OP(PLUS,plus)
  #undef PLUS

  R z;
}

K times(K a, K b)//TODO: Float results will respect intermediate OI or Oi. Other functions too. (& casts.)
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)

  #define TIMES(x, y) ((x) * (y))
  SCALAR_OP(TIMES,times)
  #undef TIMES

  R z;
}

K minus(K a, K b)
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)              

  #define MINUS(x, y) ((x) - (y))
  SCALAR_OP(MINUS,minus)
  #undef MINUS

  R z;
}

K negate(K x){K y,z; U(y=Ki(0)) z=minus(y,x); cd(y); R z;} //TODO: probably implemented using negation vector operations

K divide(K a, K b)//NB: Integral values promoted to float
{
  SCALAR_INIT(2)
  if(1==zt*zt)zt*=2;
  K z=newK(zt,zn);U(z)

  F u,d,y=FI;//nUmerator, Denominator, infinitY
  //TODO:nulls;is it necessary to check for inf? IEEE may handle it already everywhere
  //TODO: ensure that 1/inf==0 and 1/-inf ==0
  #define FDIVIDE kF(z)[i]=!d?!u?0:u>0?y:-y:u/d //0/0=0, 1/0=oo, -1/0=-oo, 1/2=0.5 
  SCALAR_EXPR(FDIVIDE,divide,u,d)

  R z;
}

K reciprocal(K x){K y,z; U(y=Kf(1)) z=divide(y,x); cd(y); R z;} 

K min_and(K a, K b)
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)

  F f,g; I x,y; 
  SCALAR_OP(MIN,min_and)

  R z;
}

K max_or(K a, K b)
{
  SCALAR_INIT(2)
  K z=newK(zt,zn);U(z)

  F f,g; I x,y; 
  SCALAR_OP(MAX,max_or)

  R z;
}


