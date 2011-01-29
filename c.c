void boilerplate()
{
//  #define EXPDATE  "-8890"
//  O("This test copy expires ");
//  cd(show(X("_dj " EXPDATE)));
//  K x=_(1);
//  K y=X(EXPDATE  " < _ _T");
//  if(matchI(x,y))exit((I)show(kerr("exp")));
//  cd(x); cd(y);
  if(!isatty(fileno(stdout))) R;
  O("K Console\n");
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

K load(S s) //TODO: working dir is stable ... store then reset after reading scripts //TODO: 'file' loads 'file.k'
{
  FILE*f=fopen(s,"r");
  P(!f,_n())
  lines(f);
  fclose(f);
  R _n();
}

K backslash(S s, I n)
{
  S t;
  if(1==n) O("Command Help NYI\n"); //TODO: if error or waiting on matching )]} then \ will resolve
  else if(!s[2] || isspace(s[2]))
  {
    t=s+(s[2]?3:2);
    SW(s[1])
    {
      CS('\\',exit(0))
      CS('\'',R NYI)
      CS('+',R NYI)
      CS('.',R NYI)
      CS('_',R NYI)
      CS('0',R NYI)
      CS(':',R NYI)
      CS('`',R NYI)
      CS('a',R NYI)
      CS('b',R NYI)
      CS('c',R NYI)
      CS('d',R NYI)
      CS('e',R NYI)
      CS('i',R NYI)
      CS('l',R load(t)) 
      CS('m',R NYI) //shows nonstandard system commands
      CS('p',R NYI)
      CS('r',R NYI) //see seedPRNG()
      CS('s',R NYI)
      CS('t',R backslash_t(t)) //TODO: also \t [digits]
      CS('v',R NYI)
      CS('w',R workspace(s)) //used,allocated,mapped. lfop: Linux & 'ps' use /proc/self/stat or /proc/<MY_PID>/stat
    }
  }

  // \kr \cd  ?

  if(isspace(s[1]))s++; //Allow system commands to be executed without preceding space
  R system(s)?DOE:_n();
}


K backslash_t(S s)
{
  I d=clock(); 
  cd(X(s));
  d=(clock()-d)/(CLOCKS_PER_SEC/1000);
  R Kf(d);
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

