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

//#define _VERB1 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20
//#define _VERB2 21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40
//#define _0VERB1 41,42,43,44,45,46,47
//#define _0VERB2 

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

Z I specialValence(V p){ R (p==addressSSR||p==addressWhat)?3:(p==addressAt||p==addressDot)?4:0;}
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

I VA(V p){R sva(p) || adverbClass(p);}  //Verb or Adverb?

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

