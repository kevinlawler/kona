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

#define _exit __exit //stdlib.h already defines "_exit" but we need it for reserved r.c's _exit function

extern I kreci;
extern V krec[1000000];
extern K _ssr(K a,K b,K c);

#endif
