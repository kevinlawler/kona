#include "incs.h"

#include "k.h"
#include "km.h"
#include "c.h"

I fLoad=0;

S sp(S k);
K*denameD(K*d,S t,I create);
Z K filexist(S s);
Z K backslash_b(S s,I n);
Z K backslash_d(S s,I n,K*dict);
Z K backslash_e(S s,I n);
Z K backslash_s(S s);
Z K backslash_t(S s);
Z K backslash_v(S s,I n,K*dict);
Z K backslash_w(S s);
Z K precision_(void);
extern I scrLim;
I fError = 1;
I fWksp=0;

#ifdef DEBUG
S fBreak = "n";
#else
S fBreak = "t";
#endif

void boilerplate()
{
  #ifndef __MINGW32__
  if(!isatty(STDOUT) || !isatty(STDIN)) R;		//kluge:  isatty() fails using mingw-10.0 with msys2
  #endif
  O("kona      \\ for help. \\\\ to exit.\n\n"); }

//Q. What if a script is \loaded (via remote call) while the terminal is waiting with an open PDA for matching parentheses/quote marks?
//A. In K3.2 the call blocks until the command-line input is not in an intermediate state
//Q. What if remote calls are sent to the console while the console is still processing the scripts (while[1;`0:"busy "]) given as command line arguments?
//A. In K3.2 the call blocks until the processing finishes or is interrupted
//Q. What if K is receiving a large message from a client. Does it block?
//A. ?
//Q. What if K is sending a large message to a client or server. Does it block?
//A. ?

Z K filexist(S s){
  FILE*f;C b[PATH_MAX+1];S p;I n=0;
  strcpy(b,s);
recheck:
  if(strstr(b,".k")){
    f=fopen(b,"r"); if(f){p=b;GC;}}
  strcat(b,".k");
  f=fopen(b,"r"); if(f){p=b;GC;}
  if(!n){
    n=strlen(khome);if(n+strlen(s)+2>PATH_MAX)R 0;
    strcpy(b,khome);strcpy(b+n,s); goto recheck; }
  R 0;
cleanup:
  fclose(f);
  K kp=newK(-3,strlen(p));M(kp);
  strcpy(kC(kp),p);
  R kp; }

Z FILE *loadf(S s)
{
  FILE *f=0;
  K p=filexist(s);
  if(p)f=fopen(kC(p),"r");
  if(!p||!f)show(kerr("file"));
  cd(p);
  R f;
}

K load(S s)
{
  I ofLoad=fLoad,ofCmplt=fCmplt; //global state
  S old_;
  K r;
  fLoad=1;fCmplt=0; old_=d_;
  if(!filexist(s)){O("%s: file not found\n",s);r=_n();GC;}
  if(scrLim>124){O("limit\n");r=kerr("stack");GC;} scrLim++;
  FILE*f=loadf(s);
  if(!f){O("%s.k: file not found\n",s);r=FE;GC;}
  lines(f); if(fclose(f)){r=FE;GC;} scrLim--;
  if(fCmplt==1) { kerr("open-in-next-line"); oerr(); }
  kerr("(nil)"); fer=fer1=0;
  r=_n();
cleanup:
  fLoad=ofLoad;fCmplt=ofCmplt;
  d_=old_;
  R r;
}

I stepopt(S s,I n)
{
  if(n==1&&*s=='\n')R 0;
  else if((n==1&&*s=='/')||(n==2&&*s=='/'&&s[1]=='\n'))R 1;
  else if((n==1&&*s=='\\')||(n==2&&*s=='\\'&&s[1]=='\n'))R 2;
  else R 3;
}

K precision(UI n){if(n>PPMAX)R DOE; PPON=n!=0; PP=PPON?n:PPMAX; R _n();}

K precision_(void){R Ki(PPON?PP:0);}

K backslash(S s, I n, K*dict)
{
  S t; C b;
  if(1==n) //TODO: if error or waiting on matching )]} then \ will resolve
  {
    O("Backslash Commands:\n"
      "\\0        datatypes help\n"
      "\\+        verb help\n"
      "\\'        adverb help\n"
      "\\:        I/O verb help\n"
      "\\_        reserved word help\n"
      "\\.        assignment/amend, function, control flow help\n"
      "\\b [s|t]  show/set break mode (stop|trace|none)\n"
      "\\d [d|^]  change k directory (^=previous)\n"
      "\\e [n]    show/set error flag (0=off,1=on,2=exit)\n"
      "\\l f      load script f or f.k\n"
      "\\p [n]    show/set print precision (0=full)\n"
      "\\r [s]    show/set random seed (0=random)\n"
      "\\s f      step script f or f.k\n"
      "\\t [n]    show/set timer interval in msec (0=disable)\n"
      "          calls niladic .m.ts\n"
      "\\t e      measure runtime of some k expression\n"
      "\\v [d|^]  show k directory (^=previous)\n"
      "\\w        show workspace resources used\n"
      "\\[cmd]    system command (also \\[ cmd]), \\echo hello\n"
      "\\\\        exit (or ctrl+d)\n"
    );
    R _n();
  }
  else if( !(b=s[2]) || isspace(b))
  {
    t=s+(b?3:2);
    SW(s[1])
    {
      CS('\\',exit(0))
      CS('\'', O(
               "Adverbs / \\ ' /: \\: ':\n"
               "Use adverbs in conjunction with verbs/functions\n"
               "Over and scan behave differently depending on whether their left argument\n"
               "is monadic or dyadic.\n"
               "/  over dyad    +/1 2 3  or 4+/1 2 3    */1+!5\n"
              "\\  scan dyad    +\\3 4 5  or 6+\\3 4 5    *\\10#2\n"
               "/  over monad   apply until fixed    f/x    (%%[;2.0])/9999.0 \n"
               "/  over monad   apply n times      n f/x    4 (2+)/0 \n"
               "/  over monad   apply while true   b f/x    {x<10} {x+1}/ 3\n"
              "\\  scan monad   trace until repeat   f\\x    (1!)\\1 2 3 4\n"
              "\\  scan monad   trace n times      n f\\x    10(|+\\)\\1 1\n"
               "/  over         {x+y+z}/[1 2 3;4;7 8 9]  f/[x;y;z]\n"
              "\\  scan         {x+y+z}\[1 2 3;4;7 8 9]  f\\[x;y;z]\n"
               "/  join         \",\"/(\"a\";\"b\")\n"
              "\\  split        \",\"\\\"a,b\"\n"
               "'  each         \"abc\" ,' \"def\"    join each  \n"
               "'  each         !:' 2 3 4    enumerate each  \n"
               "/: eachright    2 #/: (\"alpha\";\"bravo\";\"charlie\")    take each right\n"
              "\\: eachleft     0 1 2 3 #\\: \"abc\"     take each left\n"
               "': eachpair     apply pairwise  -':1 3 4 8 10\n"
               ); R _n();
      )
      CS('+', O(
              "Verbs +-*%%|&^!<>=~@?_,#$.:\n"
              "Verbs work on all sensible types (ints, floats, strings, symbols, lists)\n"
              "Dyadic or monadic is determined from context, default is dyadic\n"
              "Add : after a verb to force the monadic form, + is plus, +: is flip\n"
              "+ monadic  flip. transpose a matrix (a depth-2 list)\n"
              "+ dyadic   plus. add numbers together\n"
              "- monadic  negate. invert sign\n"
              "- dyadic   minus. subtraction\n"
              "* monadic  first. first element from the list\n"
              "* dyadic   times. multiply two numbers\n"
              "%% monadic  reciprocal. 1 over x \n"
              "%% dyadic   divide. x divided by y (not mod) \n"
              "| monadic  reverse. reverse order of list\n"
              "| dyadic   max/or. MAX(x,y) or OR(x,y) \n"
              "& monadic  where. &0 0 1 0 3 yields 2 4 4 4. \n"
              "& dyadic   min/and. MIN(x,y) or AND(x,y) \n"
              "^ monadic  shape. #elts at each depth (min over) \n"
              "^ dyadic   power. x to the exponent y \n"
              "! monadic  enumerate. !4 yields 0 1 2 3\n"
              "! dyadic   mod/rotate. 5!3 yields 2;  1 ! 4 5 6 yields 5 6 4 \n"
              "< monadic  grade up. indices of list sorted ascending \n"
              "< dyadic   less. boolean is x less than y\n"
              "> monadic  grade down. indices of list sorted descending\n"
              "> dyadic   more. boolean is x greater than y\n"
              "= monadic  group. =3 4 3 4 4 yields (0 2;1 3 4)  \n"
              "= dyadic   equals. 1 2 3 = 1 2 4 yields 1 1 0 (tolerantly) \n"
              "~ monadic  not. ~ 0 1 0 2 0 yields 1 0 1 0 1 \n"
              "~ dyadic   match. 1 2 ~ 1 2 yields 1 (types must match)\n"
              "@ monadic  atom. boolean is arg an atom (as opposed to a list) \n"
              "@ dyadic   at. elts from x at indices y\n"
              "@ triadic  monadic amend. see \\. \n"
              "@ tetradic dyadic amend. see \\.\n"
              "? monadic  unique. distinct elts from a list \n"
              "? dyadic   find. x?y yields index of y in list x (or #x)\n"
              "? dyadic   invert. {x^2} ? 2 yields sqrt(2) \n"
              "? triadic  invert-guess. secant method with clues ?[{2^x};17;4]\n"
              "_ monadic  floor. tolerant floor function \n"
              "_ dyadic   drop/cut. lose x elts from list y / separate into pieces \n"
              ", monadic  enlist. put x inside a 1-element list \n"
              ", dyadic   join. \"ab\",\"cd\" yields \"abcd\"\n"
              "# monadic  count. number of elements in list \n"
              "# dyadic   take/reshape. fill x elts from y \n"
              "$ monadic  format. cast to string \n"
              "$ dyadic   form/format. cast \n"
              ". monadic  execute/make dictionary. .\"1+1\" yields 2 \n"
              ". dyadic   value. 1 2 3 . ,2 yields 3. see \\. \n"
              ". triadic  monadic amend. see \\. \n"
              ". tetradic dyadic amend. see \\. \n"
              ":          overloaded with many operations. \n"
              ); R _n();
      )
      CS('.',
             O(
              "Assign/Amend, Functions, Control Flow \n"
              "\nAssign/Amend (see https://github.com/kevinlawler/kona/wiki/Amend)\n"
              "a:1 is assignment\n"
              "a::1 is global assignment (works in functions)\n"
              "a+:1 is like a+=1, works in general\n"
              "a-: negates a in place, works in general\n"
              "a[]:1 sets all the entries of a to 1\n"
              "a[0]:1 sets the 0th entry of a to 1\n"
              "a[0;1]+:2 increments the 0th entry's 1st entry by 2\n"
              ".[a;();+;1] returns a+1 but does not affect a on the K Tree\n"
              ".[`a;();+;1] updates a in place, returns `a\n"
              "\nAmend Equivalence\n"
              "@[a;b;c;d] is .[a;,b;c;d]\n"
              "a:1        is .[`a;();:;1]\n"
              "a+:1       is .[`a;();+;1]\n"
              "a-:        is .[`a;();-:]\n"
              "a[]:1      is .[`a;_n;:;1]\n"
              "a[0]:1     is .[`a;0;:;1] or .[`a;,0;:;1] \n"
              "a[0;1]+:2  is .[`a;0 1;+;2]\n"
              "\nError Trap\n"
              "@[a;b;:] and .[a;b;:] are error trap\n"
              "\nFunctions\n"
              "f:{[a;b;c] a+b+c} defines a function. f[1;2;3] yields 6\n"
              "Functions may be anonymous.\n"
              "Functions may have default arguments x,y,z.\n"
              "So {x^2} 3 conveniently yields 9\n"
              "\nControl Flow\n"
              ":[x1;t1;x2;t2;...;xn;tn;else] evaluate xi until true and return ti, otherwise return else \n"
              "    :[0;10;0;20;1;30;40] yields 30\n"
              "if[x;e1;...;en] if x then evaluate all e. if[j>i;a:1;b:2] \n"
              "do[m;e1;...;en] do all e m times. do[100;f[i];i+:1] \n"
              "while[x;e1;...;en] while x do e.  while[a>b; f a; a-:1] \n"
              "/ starts a comment. Must begin a line or have a space before\n"
              "\\ is trace when beginning an expression inside a function (todo)\n"
              ": is early return when beginning an expression inside a function\n"
              "' is signal (todo)\n"
              ); R _n();
      )
      CS('_',
             O(
              "Constants:\n"
              "(Note: the K epoch is 2035-01-01T00:00:00)\n"
              "_T       [current UTC Julian day count]+[fraction complete]\n"
              "_a       arguments\n"
              "_c       message source address\n"
              "_d       K-Tree path / current working dictionary\n"
              "_f       anonymous reference to current function\n"
              "_h       host name\n"
              "_i       index of current amendment\n"
              "_k       build date as string\n"
              //"_m     \n"
              "_n       nil\n"
              "_p       host port\n"
              "_s       space used allocated mmapped max\n"
              "_t       current UTC time (int)\n"
              //"_u     \n"
              "_v       current global variable under amendment\n"
              "_w       message source handle\n"
              "\n"
              "Monadic Verbs:\n"
              "_acos    inverse cosine\n"
              "_asin    inverse sine\n"
              "_atan    inverse tangent\n"
              "_ceil    ceiling (intolerant)\n"
              "_ceiling ceiling (tolerant)\n"
              "_cos     cosine\n"
              "_cosh    hyperbolic cosine\n"
              "_exp     exponential\n"
              "_floor   largest previous integer (intolerant)\n"
              "_log     natural logarithm\n"
              "_sin     sine\n"
              "_sinh    hyperbolic sine\n"
              "_sqr     square\n"
              "_sqrt    square root\n"
              "_tan     tangent\n"
              "_tanh    hyperbolic tangent\n"
              "_abs     absolute value\n"
              "_bd      convert to binary representation\n"
              "_ci      char-of-int (octal if unprintable char)\n"
              "_db      construct from binary representation\n"
              "_dj      date from Julian day count\n"
              "_exit    exit with status x\n"
              "_getenv  get an environment variable\n"
              "_gtime   in UTC, ints: YYYMMDD,HHMMSS (_gtime _t)\n"
              "_host    host name IP address (int)\n"
              "_ic      int-of-char\n"
              "_inv     inverse of a matrix\n"
              "_jd      Julian day count from date _jd 20110315\n"
              "_lt      convert output of _t to local time\n"
              "_ltime   localized version of _gtime\n"
              "_size    size of file (bytes)\n"
              "\n"
              "Dyadic Verbs:\n"
              "_bin     index of element using binary search\n"
              "_binl    search for several elements\n"
              "_di      delete element at index\n"
              "_dot     dot product\n"
              "_draw    draw x random numbers from 0 to y-1, negative y indicates without replacement\n"
              "_dv      delete value\n"
              "_dvl     delete several values\n"
              "_hash    hash, (x;_hash x)?y\n"
              "_hat     caret/without, x _hat y\n"
              "_in      true if x is in y\n"
              "_lin     _in for several values\n"
              "_lsq     matrix division\n"
              "_mul     matrix multiplication\n"
              "_setenv  set environment variable\n"
              "_sm      string match\n"
              "_ss      positions of substring y in string x\n"
              "_sv      scalar from vector with base change\n"
              "_vs      vector from scalar with base change\n"
              "_vsx     vector from scalar with base change - extended\n"
              "\n"
              "Triadic Verbs:\n"
              "_ssr     string search & replace\n"
              ); R _n();
      )
      CS('0',
             O("Datatypes  -4 -3 -2 -1 0 1 2 3 4 5 6 7\n"
               "Monadic 4: reveals type, 4:1 2 3 yields -1\n"
               "-4 vector symbol     `a`b`c or ,`a\n"
               "-3 vector character  \"abc\" or ,\"c\" \n"
               "-2 vector float      1.0 2.0 3.33 or ,1.0\n"
               "-1 vector integer    1 2 3 or ,1\n"
               " 0 list   general    (`a;1 2 3) or (`a;(1 2 3;(3 4;\"c\")))\n"
               " 1 scalar integer    1\n"
               " 2 scalar float      1.0\n"
               " 3 scalar character  \"c\" \n"
               " 4 scalar symbol     `s\n"
               " 5 dictionary        .((`a;10;);(`b;20;))  or  .()  or  .,(`a;5;) \n"
               " 6 nil               _n or (;;) (list of 3 nils)  \n"
               " 7 verbs/functions   +  +: {1+x}  +[1;]  (|+)  {[a;b]1+a+b}  {x+y}[1;]\n"
               "Empty Lists:\n"
               "-4 0#`\n"
               "-3 \"\"\n"
               "-2 0#0.0\n"
               "-1 !0\n"
               " 0 ()\n"
               "Special numeric types:\n"
               " 0N null integer\n"
               " 0n null float\n"
               "-0I infinity integer negative\n"
               " 0I infinity integer positive\n"
               "-0i infinity float   negative\n"
               " 0i infinity float   positive\n"
               "Dictionaries:\n"
               "Start by making a dictionary d[`k]:4\n"
               "d[]   values\n"
               "!d    keys\n"
               "d[`k] value at k\n"
               "d@`k  value at k\n"
              ); R _n();
      )
      CS(':',
             O("I/O Verbs 0: 1: 2: 3: 4: 5: 6:\n"
               "\nDisk\n"
               "0: dyadic   write text file `f 0: \"hello\" \n"
               "0: monadic  read text file  0: `f\n"
               "1: dyadic   write binary file `f 1: 4 5 6 \n"
               "1: monadic  read binary file  1: `f (mmapped)\n"
               "2: monadic  read binary file  2: `f (copied to memory)\n"
               "3: dyadic   append to binary file, w/o sync `f 3: ,7\n"
               "5: dyadic   append to binary file `f 5: ,7\n"
               "6: dyadic   write raw byte string `f 6: \"\\01\\02\\03\"\n"
               "6: monadic  read raw byte string  6: `f\n"
               "\nNetwork\n"
               "Start k listening for IPC on port 1234  ./k -i 1234\n"
               "3: monadic  open handle    h: 3:(`\"192.168.1.101\";1234) \n"
               "3: monadic  close handle   3: h \n"
               "            exec .m.c expression\n"
               "3: dyadic   asynchronous send, returns _n      h 3: \"a:2\"\n"
               "            calls monadic .m.s msg handler\n"
               "4: dyadic   synchronous send, returns result   h 4: \"1+1\"\n"
               "            calls monadic .m.g msg handler\n"

               "\nOther\n"
               "0: dyadic   write to console `0: \"hello\\n\" \n"
               "2: dyadic   dynamically load C function  a:`libfile 2:(`func,3); a[1;2;3]\n"
               "4: monadic  type of data [-4,7],  4: \"c\" /returns 3\n"
               "5: monadic  printed representation. 5:1 2 3 /returns \"1 2 3\"\n"

               "\n0: and 1: both have versions for reading fields\n"
               "In all cases `f can instead be (`f;start;length)\n"
               "Read fixed-width fields: \"cbsijfd IFCSDTZMm\" (\" \" is ignore)\n"
               "(types;widths)0:`f    (\"IFC\";3 5 4)0:`f  /read rows like \"20 30.1 golf\\n\" \n"
               "(types;widths)1:`f \n"
               "\"c\" 1:`f for c in \"cid\", read bytes/ints/doubles\n"
               "Load delimited text file (no column names):\n"
               "(types;delim)0:`f    \n"
               "Load delimited text file (with column names):\n"
               "(types;,delim)0:`f   \n"
               "c 1-byte char, b 1-byte int, s 2-byte int, i 4-byte int, f 4-byte float,\n"
               "d 8-byte double, \" \" 1-byte skip, I int, F float, C string, S string (sym), DTZMm Y? \n"
             ); R _n();
      )
      CS('`',R NYI)
      CS('a',R NYI)
      CS('b',R backslash_b(s,n))
      CS('c',R NYI)
      CS('d',R backslash_d(s,n,dict))
      CS('e',R backslash_e(s,n))
      CS('i',R NYI)
      CS('l',R load(t))
      CS('m',R NYI) //shows nonstandard system commands
      CS('p',if(*t){I p; P(!StoI(t,&p),TE); R precision(p);} else R precision_();)
      CS('r',if(*t){I r; P(!StoI(t,&r),TE); seedPRNG(r); R _n();} else {seedPRNG(SEED); R Ki(SEED);})
      CS('s',R backslash_s(t))
      CS('t',R backslash_t(t)) //TODO: also \t [digits]
      CS('v',R backslash_v(s,n,dict))
      CS('w',R backslash_w(s)) //used,allocated,mapped. lfop: Linux & 'ps' use /proc/self/stat or /proc/<MY_PID>/stat
    }
    O("domain error\n"); R _n();
  }

  // \kr \cd  ?

  if(isspace(s[1]))s++; //Allow system commands to be executed without preceding space
#ifdef WIN32
  s++;
#endif
  R system(s)?DOE:_n();
}

Z K backslash_b(S s,I n) {
  if(n==2) {O("%s\n",fBreak); R _n();}
  if(n==4 && s[3]==*"n") { fBreak="n"; R _n(); }
  if(n==4 && s[3]==*"t") { fBreak="t"; R _n(); }
  if(n==4 && s[3]==*"s") { fBreak="s"; R _n(); }
  O("valid options are: n, s, t\n"); R _n();
}

Z K backslash_d(S s,I n,K*dict) {
  C z[256];
  //I len=strlen(d_); if(n==2){K r=newK(-3,len); strncpy(kC(r),d_,len); R r;}  // yields contents of d_ enclosed in quotes
  if(n==2) {O("%s\n",d_); R _n();}  // yields contents of d_ without quotes (same as k3.2)
  if(n==4 && s[3]=='.') { d_=(S)sp(""); R _n();}
  if(n==4 && s[3]=='^') {
    if(strlen(d_)==0) R _n();
    if(strlen(d_)==2) {d_=(S)sp(""); R _n();}
    if(strlen(d_)>3){ I c=0,i=0; for(i=0;i<strlen(d_);i++)if(d_[i]=='.')c=i; strcpy(z,d_); z[c]='\0'; d_=(S)sp(z); R _n(); } }
  if(n==5 && s[3]=='.' && s[4]=='k') { d_=(S)sp(".k"); R _n();}
  if(n==5 && s[3]=='.' && s[4]!='k') {O("absolute backslash-d should begin with .k\n"); R _n();}
  if(isalpha(s[3])){ denameD(dict,s+3,1); strcpy(z,d_); strcat(z,"."); strcat(z,s+3); d_=(S)sp(z); R _n(); }
  if(n>=6 && s[3]=='.' && s[4]=='k' && s[5]=='.'){denameD(&KTREE,s+3,1); d_=(S)sp(s+3); R _n();}
  if(s[3]=='.'){denameD(&KTREE,s+3,1); d_=(S)sp(s+3); R _n();}
  R NYI; }

Z K backslash_v(S s,I n,K*dict) {
  if(n>2 && s[2]=='\r')R SYE;
  C z[256]; z[0]='\0';
  if(2==n) strcpy(z,d_);
  if(4==n && s[3]==*"^") {
    if(strlen(d_)>3) {
      I c=0,i=0;
      for(i=0;i<strlen(d_);i++) if(d_[i]==*".")c=i;
      strcpy(z,d_); z[c]=*"\0"; }
    else R _n(); }
  if(isalpha(s[3])) {
    strcpy(z,d_); strcat(z,"."); strcat(z,s+3); }
  if(*z) {
    K x=*denameD(&KTREE,z,0);
    R 6==xt?_n():enumerate(x); }
  R NYI;
}

Z K backslash_e(S s,I n) {
  if(n==2) {O("%lld\n",fError); R _n();}
  if(n==4 && s[3]==*"0") { fError=0; R _n(); }
  if(n==4 && s[3]==*"1") { fError=1; R _n(); }
  if(n==4 && s[3]==*"2") { fError=2; R _n(); }
  O("valid options are: 0, 1, 2\n"); R _n();
}

Z K backslash_s(S s)
{
  S t,u=0,w; I c=0,d,n,m=0,l=0,r;
  FILE*f=loadf(s);
  K k=0,y=0,z=0;
  P(!f,_n());
  while(0<(c=wds(&y,f)))
  { n=y->n; t=kC(y);
    w=t; while(isspace(*w++))l++;
    if(l==n||!n){if(y)cd(y); y=0; continue;}
    O("%s ",t);
    if(-1==getline_(&u,&m,stdin))GC;
    d=stepopt(u,m);
    if(d==1){if(y)cd(y); y=0; continue;}else if(d==2)GC;
    show(k=ex(wd(t,n)));
    if(k){cd(k); k=0;}
    if(y){cd(y); y=0;}
    do{
      prompt(1);
      if(0>wds_(&z,stdin,1))GC;
      w=kC(z); l=z->n;
      d=stepopt(w,l);
      if(d==1){if(z)cd(z); z=0; break;}else if(d==2)GC;
      show(k=ex(wd(w,l)));
      if(k){cd(k); k=0;}
      if(z){cd(z); z=0;}
    }while(d==0||d==3);
  }
cleanup:
  r=fclose(f); if(r)R FE;
  free(u);
  if(k)cd(k);
  if(y)cd(y);
  if(z)cd(z);
  R _n();
}

Z K backslash_t(S s) {
  I r;
  if(*s){
    if(StoI(s,&r)){
      tmr_ival = r;
      R _n();
    }
    I d=clock();K z=X(s);d=(clock()-d)/((F)CLOCKS_PER_SEC/1000);
    cd(z);//it takes time
    R Ki(d);
  }
  R Ki(tmr_ival);
}

Z K backslash_w(S s) { fWksp=1;  R _n(); }
