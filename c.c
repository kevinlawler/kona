#include "incs.h"

void boilerplate()
{
 if(!isatty(STDOUT) || !isatty(STDIN)) R;
  O("K Console - Enter \\ for help\n");
  prompt(0);

}

//Q. What if a script is \loaded (via remote call) while the terminal is waiting with an open PDA for matching parentheses/quote marks? 
//A. In K3.2 the call blocks until the command-line input is not in an intermediate state
//Q. What if remote calls are sent to the console while the console is still processing the scripts (while[1;`0:"busy "]) given as command line arguments?
//A. In K3.2 the call blocks until the processing finishes or is interrupted
//Q. What if K is receiving a large message from a client. Does it block?
//A. ?
//Q. What if K is sending a large message to a client or server. Does it block?
//A. ?

I filexist(S s){FILE *f=fopen(s,"r"); if(f){fclose(f); R 1;}else R 0;}

K filename(S s)
{
  S z; K p;
  I b=!filexist(s),n=strlen(s);
  U(p=newK(-3,n+2*b))
  strcpy(kC(p),s);
  if(b)strcat(kC(p)+n,".k");
  R p;
}

FILE *loadf(S s)
{
  FILE *f;
  K p=filename(s);
  f=fopen(kC(p),"r");
  cd(p);
  R f;
}

K load(S s) //TODO: working dir is stable ... store then reset after reading scripts
{
  FILE*f=loadf(s);
  P(!f,_n())
  lines(f);
  fclose(f);
  R _n();
}

K backslash_s(S s)
{
  S t,u=0; I c,n,m=0;
  FILE*f=loadf(s);
  K k=0,z=0;
  P(!f,_n());
  if(-2==wdss(&z,f))GC;
  DO(z->n,
     n=kK(z)[i]->n; t=kC(kK(z)[i]);
     O("%s ",t);
     if(-1==getline_(&u,&m,stdin))GC;
     if(m==1&&*u=='\n')
     { show(k=ex(wd(t,n)));
       if(i==_i-1)GC;
       if(-1==getline_(&u,&m,stdin))GC;
       if(m==1&&*u=='\n')continue;
       else if(m==2&&*u=='\\')GC;
     }
     else if(m==2&&*u=='\\')GC;
     else if(m==2&&*u=='/')continue;
     if(k)cd(k);
  );
cleanup:
  fclose(f);
  if(u)free(u);
  if(z)cd(z);
  R _n();
}

K precision(UI n) {if(n>PPMAX)R DOE; PPON=n!=0; PP=PPON?n:PPMAX; R _n();}

K precision_(void){R PPON?Ki(PP):Ki(0);}

K backslash(S s, I n)
{
  S t; C b;
  if(1==n) //TODO: if error or waiting on matching )]} then \ will resolve
  {
    O("Backslash Commands:\n"
      "\\0      datatypes help\n"
      "\\+      verb help\n"
      "\\'      adverb help\n"
      "\\:      I/O verb help\n"
      "\\_      reserved word help\n"
      "\\.      assignment help\n"
      "\\d [todo]   directory command (todo) \n" 
      "\\l f    load script f or f.k\n"
      "\\p [n]  show/set print precision (0=full)\n"
      "\\r [s]  show/set random seed\n" 
      "\\s f    step script f or f.k\n" 
      "\\t [e]  measure runtime of some k expression\n"
      "\\w      show workspace resources used\n"
      "\\[cmd]  system command (also \\[ cmd]), \\echo hello\n"
      "\\\\      exit (or ctrl+d)\n"
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
              "\\  scan dyad    +\\3 4 5  or 6+\\3 4 5    +\\!10\n"
               "/  over monad   apply until fixed    f/x    (%%[;2.0])/9999.0 \n"
               "/  over monad   apply n times      n f/x    4 (2+)/0 \n"
               "/  over monad   apply while true   b f/x    todo\n"
              "\\  scan monad   trace until repeat   f\\x    (1!)\\1 2 3 4\n"
              "\\  scan monad   trace n times      n f\\x    10(|+\\)\\1 1\n"
               "/  over         todo. f/[x;y;z]\n"
              "\\  scan         todo. f\\[x;y;z]\n"
               "'  each         \"abc\" ,' \"def\"    join each  \n"
               "'  each         !:' 2 3 4    enumerate each  \n"
               "/: eachright    #:/:(2;2 2;2 2 2)    count each right\n"
               "/: eachright    2 #/: (\"alpha\";\"bravo\";\"charlie\")    take each right\n"
              "\\: eachleft     0 1 2 3 #\\: \"abc\"     take each left\n"
               "': eachpair     apply pairwise  -':1 3 4 8 10\n"
               );
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
              "? dyadic   draw. n?m yields n numbers < m \n"
              "? dyadic   deal. -n?m yields n ints < m without replacement\n"
              "? dyadic   sample. n?y yields n random elements from list y (-n for no replacement)"
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
              );
      )
      CS('.',R NYI)
      CS('_',
             O(
              "Constants:\n"
              "_T       [current UTC Julian day count]+[fraction complete]\n"
              //"_a     \n"
              "_d       K-Tree path / current working dictionary\n"
              "_f       anonymous reference to current function\n"
              "_h       host name\n"
              //"_i     \n"
              //"_k     \n"
              //"_m     \n"
              "_n       nil\n"
              //"_p     \n"
              //"_s     \n"
              "_t       current UTC time (int)\n"
              //"_u     \n"
              //"_v     \n"
              //"_w     \n"
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
              "_draw    draw x random numbers from 0 to y-1\n"
              "_dv      delete value\n"
              "_dvl     delete several values\n"
              "_in      true if x is in y\n"
              "_lin     _in for several values\n"
              "_lsq     matrix division\n"
              "_mul     matrix multiplication\n"
              "_setenv  set environment variable\n"
              "_sm      string match\n"
              "_ss      positions of substring y in string x\n"
              "_sv      scalar from vector with base change\n"
              "_vs      vector from scalar with base change\n"
              "\n"
              "Triadic Verbs:\n"
              "_ssr     string search & replace\n"
              );
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
              );
      )
      CS(':',
             O("I/O Verbs 0: 1: 2: 3: 4: 5: 6:\n"
               "\nDisk\n"
               "0: dyadic   write text file `f 0: \"hello\" \n"
               "0: monadic  read text file  0: `f\n"
               "1: dyadic   write binary file `f 1: 4 5 6 \n"
               "1: monadic  read binary file  1: `f (mmapped)\n"
               "2: monadic  read binary file  2: `f (copied to memory)\n"
               "5: dyadic   append to binary file `f 5: ,7\n"
               "6: dyadic   write raw byte string `f 6: \"\\01\\02\\03\"\n"
               "6: monadic  read raw byte string  6: `f\n"
               "\nNetwork\n"
               "Start k listening for IPC on port 1234  ./k -i 1234\n" 
               "3: monadic  open handle    h: 3:(`\"192.168.1.101\";1234) \n"
               "3: monadic  close handle   3: h \n"
               "3: dyadic   asynchronous send, returns _n      h 3: \"a:2\"\n"
               "4: dyadic   synchronous send, returns result   h 4: \"1+1\"\n"

               "\nOther\n"
               "0: dyadic   write to console `0: \"hello\\n\" \n"
               "2: dyadic   dynamically load C function  a:`libfile 2:(`func,3); a[1;2;3]\n"
               "4: monadic  type of data [-4,7],  4: \"c\" /returns 3\n"
               "5: monadic  todo. printed representation. 5:1 2 3 /returns \"1 2 3\"\n"

               "\n0: and 1: both have versions for reading fields\n"
               "In all cases `f can instead be (`f;start;length)\n"
               "Reading fixed-width fields: \"cbsijfd IFCSDTZMm\" (\" \" is ignore)\n"
               "(type;width)0:`f    (\"IFC\";3 5 4)0:`f  /read rows like \"20 30.1 golf\\n\" \n"
               "(type;width)1:`f \n" 
               "\"c\" 1:`f for c in \"cid\", read bytes/ints/doubles\n"
               "Reading delimited text fields:\n"
               "(type;[,]delim)0:`f    todo\n"
               "c 1-byte char, b 1-byte int, s 2-byte int, i 4-byte int, f 4-byte float,\n"
               "d 8-byte double, \" \" 1-byte skip, I int, F float, C string, S string (sym), DTZMm Y? todo\n"
             );
      )
      CS('`',R NYI)
      CS('a',R NYI)
      CS('b',R NYI)
      CS('c',R NYI)
      CS('d',R NYI) // have some functionality for \d ..  (possibly \d ^)
      CS('e',R NYI)
      CS('i',R NYI)
      CS('l',R load(t))
      CS('m',R NYI) //shows nonstandard system commands
      CS('p',if(*t){I p; P(!StoI(t,&p),TE); R precision(p);} else R precision_())
      CS('r',if(*t){I r; P(!StoI(t,&r),TE); seedPRNG(r); R _n();} else R Ki(SEED))
      CS('s',R backslash_s(t))
      CS('t',R backslash_t(t)) //TODO: also \t [digits]
      CS('v',R NYI)
      CS('w',R workspace(s)) //used,allocated,mapped. lfop: Linux & 'ps' use /proc/self/stat or /proc/<MY_PID>/stat
    }
    R _n();
  }

  // \kr \cd  ?

  if(isspace(s[1]))s++; //Allow system commands to be executed without preceding space
  R system(s)?DOE:_n();
}


K backslash_t(S s)
{
  I d=clock(); 
  cd(X(s));
  d=(clock()-d)/((F)CLOCKS_PER_SEC/1000);
  R Ki(d);
}


#ifdef __MACH__
#include <mach/mach_types.h> 
#endif
K workspace(S s)
{
  K z=newK(-1,3); U(z)

  #ifdef __MACH__
  struct task_basic_info t_info;
  mach_msg_type_number_t t_info_count = TASK_BASIC_INFO_COUNT;
  if(KERN_SUCCESS != task_info(mach_task_self(),TASK_BASIC_INFO,&t_info,&t_info_count)) R 0;
  kI(z)[0]=t_info.resident_size;
  kI(z)[1]=t_info.virtual_size;
  kI(z)[2]=0;
  #endif

  R z;
}

