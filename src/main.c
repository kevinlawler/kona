#include "incs.h"
#include "k.h"

int main(int argc,S*argv)
{
  kinit();
  args(argc,argv);
  boilerplate();
  attend(); //loop on stdin/inet
  R 0;
}
