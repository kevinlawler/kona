#include "incs.h"
#include "k.h"

int main(int argc,S*argv)
{
  kinit();
  boilerplate();
  args(argc,argv);
  attend(); //loop on stdin/inet
  R 0;
}
