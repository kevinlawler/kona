#include "incs.h"
#include "k.h"

int main(int argc,S*argv)
{
  kinit();
  args(argc,argv);
  prompt(0);
  attend(); //loop on stdin/inet
  R 0;
}
