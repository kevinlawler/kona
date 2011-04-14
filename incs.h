#ifndef INCS_H
#define INCS_H

#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>    //M()/OOM_CD()
#include <math.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <netdb.h>
#include <fnmatch.h>
#include <dlfcn.h>
#include <sys/mman.h>  //mmap
#include <unistd.h>    //sbrk,sysconf
#include <fcntl.h>     //O_RDWR etc
#include <pthread.h>

#include "ts.h" //data types + macros

extern S vm_s[];
extern S vd_s[];
extern S vt_s[];
extern I kreci;
extern V krec[1000000];
extern K _ssr(K a,K b,K c);

#endif
