//64-bit single-threaded implementation of K3.2.  Version is Kona 3.2.0
//todo abbreviations: mm/o = memory manage/optimize   lfop = localize for other platforms (eg needs ifdef changes)   oom = handle out-of-memory
#include "incs.h"

#include "k.h"
#include "r.h"
#include "kc.h"
#include "kx.h"
#include "kg.h"
#include "km.h"
#include "kn.h"
#include "ko.h"
#include "ks.h"
#include "tests.h"
#include "v.h"
#include "va.h"
#include "vc.h"
#include "vd.h"
#include "vf.h"
#include "vg.h"
#include "vq.h"

C errmsg[256]; //TODO: pthread_getspecific (not __thread) thread-local storage (different for mac os x)
extern K kerr(cS s){ R snprintf(errmsg,256,"%s",s),(K)0;} 
Z I oerr(){R O("%s %s\n",errmsg,"error");}

Z K XN(S s,I n);


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
Z K XN(S s,I n){R ex(wd(s,n));} //asserts ex(x) has first-line U(x)
K KX(K x){R XN(CSK(x),xn);}  //assumes 3==ABS(xt)

//TODO: open() can set errno, everywhere
//TODO: central place for setting errno=0
//TODO: probably don't need many/most errno=0 lines. just handle it higher up in one place or so. arthur doesn't seem to have errno=0 everywhere.

F FF(F f){F F;R modf(f,&F);}//Floating-Point Fractional Part

//pt(N t){N l=t->c[0],r=t->c[1];O("node: %s  balance: %d\n", t->k, t->b);O(" Lchild: %s\n",l?l->k:"null");O(" Rchild: %s\n",r?r->k:"null");if(l)pt(l);if(r)pt(r);}

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

K end(){R 0;}
I bk(V p){R p==DT_END_OFFSET;} //break: is ; or \n

C ac[] = "/\\'";
K over(){R 0;} K scan(){R 0;} K each(){R 0;}
K eachright(){R 0;} K eachleft(){R 0;} K eachpair(){R 0;}

C vc[]="+-*%|&^!<>=~@?_,#$.:";// was "!#$%&*+,-.<=>?@^_|~:";
#define _VERB1 flip,negate,first,reciprocal,reverse,where,shape,enumerate,grade_up,grade_down,group,not_attribute,atom,range,floor_verb,enlist,count,format,dot_monadic,colon_monadic
#define _VERB2 plus,minus,times,divide,max_or,min_and,power,rotate_mod,less,more,equals,match,at,what,drop_cut,join,take_reshape,dollar,dot,colon_dyadic
#define _0VERB1 _0m,_1m,_2m,_3m,_4m,_5m,_6m
#define _0VERB2 _0d,_1d,_2d,_3d,_4d,_5d,_6d //This has a dependency in the parser - magic number 6

//V vm[]  = {_VERB1};
//V vd[]  = {_VERB2};

V offsetSSR, offsetWhat, offsetAt, offsetDot, offsetColon;
I offsetOver, offsetScan, offsetEach, offsetEachright, offsetEachleft, offsetEachpair;

S IFS[3] = {"x","y","z"};
S IFP[3]; //Implicit function parameters sp(x),...

I stringHasChar(S s,C c){I i=0;while(s[i])if(c==s[i++])R 1;R 0;} //string never has '\0'
I charpos(S s,C c){I i=0;while(s[i] && c!=s[i])i++; R i;}

I isCharVerb(C c) {R stringHasChar(vc,c);}
I charsVerb(C c)  {R charpos(vc,c);}
C verbsChar(V p)  {R (p>=DT_VERB_OFFSET && p < DT_SPECIAL_VERB_OFFSET)?vc[((I)p-DT_VERB_OFFSET)/2]:'\0';}

C adverbsChar(V p){R (p>=DT_ADVERB_OFFSET)?ac[((I)p-DT_ADVERB_OFFSET)%3]:'\0';}
I charsAdverb(C c) {R charpos(ac,c);}

I sva(V p) //simpleVerbArity: Use boundaries of arrays to determine verb class in O(1) constant time
{ 
  UI q=p;
  if(q<DT_SIZE)R DT[q].arity;
  R 0;

}
I adverbClass(V p) //0: not an adverb, 1: / \ ', 2: /: \: ':
{ 
  UI q=p;
  if (q<DT_SIZE) R DT[q].adverbClass;
  R 0;
} 

Z I specialValence(V p){ R (p==offsetSSR||p==offsetWhat)?3:(p==offsetAt||p==offsetDot)?4:0;}
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

    if(k==offsetEachright || k==offsetEachleft) R 2; //todo: this looks off: eachright can be valence 1? as in +:/:  ?
    if(i>1 && k==offsetEach || k==offsetOver || k==offsetScan)  //for f'[x;y;z], f/[x;y;z], ...
    {
      V*q; I j=0,s;
      do q=kW(v)[i-2-(j++)]; while(q==offsetEach || q==offsetOver || q==offsetScan);

      s=sva(q);
      if(s && !specialValence(q)) R s - ((i-2-j >= 0)?1:0); // |+\ or +\   (leaves out |@\ and @\ ...or not...or intentional...?)

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

I VA(V p){R sva(p) || adverbClass(p);}  //(Verb or Adverb)?

Z I isescape(UC c) {R (c=='"'||c=='\\'||c=='\b'||c=='\n'||c=='\r'||c=='\t');}
Z I needspt0(F f){if(isnan(f)||-FI==f||FI==f)R 0; Z C b[512];snprintf(b,512,"%.*g",(int)PP,f); R !stringHasChar(b,'.') && !stringHasChar(b,'e');}//no better way I know

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
                  DO(a->n, CPMAX str=kS(a)[i];if(!str)continue; sl=strlen(str);ss=simpleString(str);
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

        I q=(I)p;
        if(q < DT_SIZE && q >= DT_SPECIAL_VERB_OFFSET)
        {  s=DT[q].text;
           k=strlen(s);
           DO(k,O_("%c",s[i]))
           if(s[k-1]==':' && 1==DT[q].arity) O_("%c",':'); //extra colon for monadic 0: verbs
        }
        else if(k=sva(p)) O_(2==k?"%c":"%c:",   verbsChar(p));
        else if(k=adverbClass(p)) O_(1==k?"%c":"%c:", adverbsChar(p));
        else printAtDepth(u,*(K*)p,d+1,0,1+vdep,0); //assert: null p won't reach here
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

K plus_scan(K x, K y)
{
  //Optimized scan forces you to pre-determine output type for (x,y), in contrast to optimized over
  I yt=y->t,yn=y->n;

  P(x && xt != 1 && xt != 2, 0) 
  P(yn<2 || !yt || ABS(yt) > 2 , 0)

  I t = -ABS(yt); if(x) t = -MAX(ABS(xt),ABS(t));
  I n = y->n + (x?1:0);

  K z;
  
  if(!x && y->c==1 && (yt==t || sizeof(I)==sizeof(F))) z=ci(y); //reuse vector you know will be discarded
  else z=newK(t,n);
  U(z)

  I j=0;
  if(x)
  {
    j=1;
    if     (-2==t && 2==xt)*kF(z)=*kF(x);
    else if(-2==t && 1==xt)*kF(z)=*kI(x);
    else if(-1==t && 1==xt)*kI(z)=*kI(x);
  }

  if     (-2==t && -2==yt) {kF(z)[j]=*kF(y)+(j?*kF(z):0); DO(yn-1, kF(z)[i+j+1] = kF(z)[i+j]+kF(y)[i+1])}
  else if(-2==t && -1==yt) {kF(z)[j]=*kI(y)+(j?*kF(z):0); DO(yn-1, kF(z)[i+j+1] = kF(z)[i+j]+kI(y)[i+1])}
  else if(-1==t && -1==yt) {kI(z)[j]=*kI(y)+(j?*kI(z):0); DO(yn-1, kI(z)[i+j+1] = kI(z)[i+j]+kI(y)[i+1])}

  R z;
}

K plus_over(K x,K y)
{
  I accI=0; F accF=0;
  I yt=y->t,yn=y->n;
  if((!yt&&yn) || ABS(yt) > 2 || (x && xt!=1 && xt!=2)) R 0;
  K z,r;
  SW(ABS(yt)) //May want to consider casting y->t==1 to F's when x && x->t is F
  {
    CS(2,if(yn)accF=*kF(y);DO(y->n-1, accF=accF+kF(y)[i+1]) z=Kf(accF)) //CS order matters for empty list ()
    CD:  if(yn)accI=*kI(y);DO(y->n-1, accI=accI+kI(y)[i+1]) z=Ki(accI); //CS 1
  }
  U(z)
  if(x){r=plus(x,z); cd(z); z=r;}
  R z;
}

K times_over(K x,K y)
{
  I accI=1; F accF=1;
  I yt=y->t,yn=y->n;
  if((!yt&&yn) || ABS(yt) > 2 || (x && xt!=1 && xt!=2)) R 0;
  K z,r;
  SW(ABS(yt)) //May want to consider casting y->t==1 to F's when x && x->t is F
  {
    CS(2,if(yn)accF=*kF(y);DO(y->n-1, accF=accF*kF(y)[i+1]) z=Kf(accF)) //CS order matters for empty list () 
    CD:  if(yn)accI=*kI(y);DO(y->n-1, accI=accI*kI(y)[i+1]) z=Ki(accI); //CS 1
  }
  U(z)
  if(x){r=times(x,z); cd(z); z=r;}
  R z;
}

K max_or_over(K x,K y)
{
  I accI=0; F accF=-FI;
  I yt=y->t, yn=y->n;
  if((!yt&&yn) || ABS(yt) > 2 || (x && xt!=1 && xt!=2)) R 0;
  K z,r;
  SW(ABS(yt)) //May want to consider casting y->t==1 to F's when x && x->t is F
  {
    CS(2,if(yn)accF=*kF(y);DO(y->n-1, accF=MAX(accF,kF(y)[i+1])) z=Kf(accF)) //CS order matters for empty list () 
    CD:  if(yn)accI=*kI(y);DO(y->n-1, accI=MAX(accI,kI(y)[i+1])) z=Ki(accI); //CS 1
  }
  U(z)
  if(x){r=max_or(x,z); cd(z); z=r;}
  R z;
}

K min_and_over(K x,K y)
{
  I accI=1; F accF=FI;
  I yt=y->t, yn=y->n;
  if((!yt&&yn) || ABS(yt) > 2 || (x && xt!=1 && xt!=2)) R 0;
  K z,r;
  SW(ABS(yt)) //May want to consider casting y->t==1 to F's when x && x->t is F
  {
    CS(2,if(yn)accF=*kF(y);DO(y->n-1, accF=MIN(accF,kF(y)[i+1])) z=Kf(accF)) //CS order matters for empty list () 
    CD:  if(yn)accI=*kI(y);DO(y->n-1, accI=MIN(accI,kI(y)[i+1])) z=Ki(accI); //CS 1
  }
  U(z)
  if(x){r=min_and(x,z); cd(z); z=r;}
  R z;
}

TR DT[] =  //Dispatch table is append-only. Reorder/delete/insert breaks backward compatibility with IO & inet
{
  {0, 0, 0,0,0}, //So no row index is confused with null pointer
  {0, 0, end,0,0}, // ; and such. convenience. (for ex(). not to be confused with last element of table)
  {0, 0, 0, 0,0},
  {1, 0, over,"/",0},
  {1, 0, scan,"\\",0},
  {1, 0, each,"'",0},
  {2, 0, eachright,"/:",0},
  {2, 0, eachleft,"\\:",0},
  {2, 0, eachpair,"':",0},
  {0, 0, 0,0,0},
  {0, 0, 0,0,0},
  {0, 0, 0,0,0},
  {0, 0, 0,0,0},
  {0, 0, 0,0,0},
  {0, 0, 0,0,0},
  {0, 0, 0,0,0},
  {0, 0, 0,0,0},
  {0, 1, flip,"+",0},
  {0, 2, plus,"+",{plus_over,plus_scan}},
  {0, 1, negate,"-",0},
  {0, 2, minus,"-",0},
  {0, 1, first,"*",0},
  {0, 2, times,"*",{times_over}},
  {0, 1, reciprocal,"%%",0},
  {0, 2, divide,"%%",0},
  {0, 1, reverse,"|",0},
  {0, 2, max_or,"|",{max_or_over}},
  {0, 1, where,"&",0},
  {0, 2, min_and,"&",{min_and_over}},
  {0, 1, shape,"^",0},
  {0, 2, power,"^",0},
  {0, 1, enumerate,"!",0},
  {0, 2, rotate_mod,"|",0},
  {0, 1, grade_up,"<",0},
  {0, 2, less,"<",0},
  {0, 1, grade_down,">",0},
  {0, 2, more,">",0},
  {0, 1, group,"=",0},
  {0, 2, equals,"=",0},
  {0, 1, not_attribute,"~",0},
  {0, 2, match,"~",0},
  {0, 1, atom,"@",0},
  {0, 2, at,"@",0},
  {0, 1, range,"?",0},
  {0, 2, what,"?",0},
  {0, 1, floor_verb,"_",0},
  {0, 2, drop_cut,"_",0},
  {0, 1, enlist,",",0},
  {0, 2, join,",",0},
  {0, 1, count,"#",0},
  {0, 2, take_reshape,"#",0},
  {0, 1, format,"$",0},
  {0, 2, dollar,"$",0},
  {0, 1, dot_monadic,".",0},
  {0, 2, dot,".",0},
  {0, 1, colon_monadic,":",0},
  {0, 2, colon_dyadic,":",0},
  {0, 1, _0m,"0:",0}, 
  {0, 2, _0d,"0:",0}, 
  {0, 1, _1m,"1:",0}, 
  {0, 2, _1d,"1:",0}, 
  {0, 1, _2m,"2:",0}, 
  {0, 2, _2d,"2:",0}, 
  {0, 1, _3m,"3:",0}, 
  {0, 2, _3d,"3:",0}, 
  {0, 1, _4m,"4:",0}, 
  {0, 2, _4d,"4:",0}, 
  {0, 1, _5m,"5:",0}, 
  {0, 2, _5d,"5:",0}, 
  {0, 1, _6m,"6:",0}, 
  {0, 2, _6d,"6:",0},  //do not add 7+ here. go to bottom. keep paired as before
  {0, 1, _acos,"_acos",0},
  {0, 1, _asin,"_asin",0},
  {0, 1, _atan,"_atan",0},
  {0, 1, _ceil,"_ceil",0},
  {0, 1, _cos,"_cos",0},
  {0, 1, _cosh,"_cosh",0},
  {0, 1, _exp,"_exp",0},
  {0, 1, _floor,"_floor",0},
  {0, 1, _log,"_log",0},
  {0, 1, _sin,"_sin",0},
  {0, 1, _sinh,"_sinh",0},
  {0, 1, _sqr,"_sqr",0},
  {0, 1, _sqrt,"_sqrt",0},
  {0, 1, _tan,"_tan",0},
  {0, 1, _tanh,"_tanh",0},
  {0, 1, _abs,"_abs",0},
  {0, 1, _bd,"_bd",0},
  {0, 1, _ceiling,"_ceiling",0},
  {0, 1, _ci,"_ci",0},
  {0, 1, _db,"_db",0},
  {0, 1, _dj,"_dj",0},
  {0, 1, _kona_exit,"_exit",0},
  {0, 1, _getenv,"_getenv",0},
  {0, 1, _gtime,"_gtime",0},
  {0, 1, _host,"_host",0},
  {0, 1, _ic,"_ic",0},
  {0, 1, _inv,"_inv",0},
  {0, 1, _jd,"_jd",0},
  {0, 1, _lt,"_lt",0},
  {0, 1, _ltime,"_ltime",0},
  {0, 1, _size,"_size",0},
  {0, 2, _bin,"_bin",0},
  {0, 2, _binl,"_binl",0},
  {0, 2, _di,"_di",0},
  {0, 2, _dot,"_dot",0},
  {0, 2, _draw,"_draw",0},
  {0, 2, _dv,"_dv",0},
  {0, 2, _dvl,"_dvl",0},
  {0, 2, _in,"_in",0},
  {0, 2, _lin,"_lin",0},
  {0, 2, _lsq,"_lsq",0},
  {0, 2, _mul,"_mul",0},
  {0, 2, _setenv,"_setenv",0},
  {0, 2, _sm,"_sm",0},
  {0, 2, _ss,"_ss",0},
  {0, 2, _sv,"_sv",0},
  {0, 2, _vs,"_vs",0},
  {0, 3, _ssr,"_ssr",0},
  //^^Add new rows here^^
  {-1,-1,TABLE_END,0,0} //sentinel
};

K TABLE_END(){R 0;}
I DT_SIZE=0;
I DT_END_OFFSET, DT_ADVERB_OFFSET, DT_VERB_OFFSET, DT_SPECIAL_VERB_OFFSET;
I DT_OFFSET(V v){I i=0; while(v!=DT[i].func)i++; R i;} //init only

int main(int argc,S*argv)
{
  kinit();
  args(argc,argv);
  boilerplate();
  attend(); //loop on stdin/inet
  R 0;
}

I kreci=0;  //should be inside DEBUG case but needed in r.c cached verbs, at least until caching method changes
#ifdef DEBUG
void tf(N n){if(!n)R;DO(2,tf(n->c[i]))free(n->k);repool(n,lsz(sizeof(Node))); } //tree free
V krec[1000000];
/* Z I CV(K v) { V a[1000]; I n=0; while(v) { dd(v); a[n++]=v; DO(n, DO2(n-i-1, if(a[i]==a[i+j+1]) R 1;)) if(!(7==v->t && 0==v->n)) R 0; V q=kW(v)[0]; v=0; if(q) v= *(K*)q; } R 0; }//seven_type contains cycle? */
#endif

void finally()
{
#ifdef DEBUG   
tf(SYMBOLS); cd(KTREE); cd(KFIXED); 
//valgrind --leak-check=full --show-reachable=yes /tmp/a.out
#endif
}

