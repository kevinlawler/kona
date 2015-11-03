#include "incs.h"
#include "0.h"
#include "getline.h"

//Based on BSD's getdelim.c - [BSD License] (c) 2009 David Schultz <das@FreeBSD.org>

I expander(S *s, I n) //grow s? n - needs size
{
  S t; I q;

  //XXX: these lines are deactivated since dlmalloc was removed. the q=n line bypasses (delete if reactivated)
  //this lightly assumes realloc is efficient (nlogn) for shrinking and appending (nice when true)
  //if(n <= malloc_usable_size(*s)) R 0;
  //q = rp2(n);
  q = n;
  //

  t = realloc(*s, MAX(1,q)); //if you want to mremap this still have to avoid malloc_useable_size above... actually, maybe better to not pass mmapped here
  if(!t){ME; R -1;} //mm/o - failed
  *s=t;
  R 0;
}

I appender(S *s, I *n, S t, I k) //concatenate t to s
{
  if(expander(s,*n+k+1))R -1; //mm/o - failed
  memcpy(*s+*n,t,k);
  *n += k;
  (*s)[*n] = '\0';
  R 0;
}

I getline_(S *s,I *n,FILE *f){R getdelim_(s,n,'\n',f);}

I getdelim_(S *s,I *n,I d,FILE *f)
{
  I m; S z;size_t o=*n;
  if(getdelim(s,&o,d,f)==-1){*n=0; R -1;}
  *n=o;
  m=strlenn(*s,*n);
  if(1<m && '\n'==(*s)[m-1] && '\r'==(*s)[m-2]) {
    (*s)[--m]='\0'; (*s)[m-1]='\n'; }
  z=strdupn(*s,m);
  free(*s);
  *s=z;
  R *n=m;
}

#if defined(__OpenBSD__) || defined(__NetBSD__) ||  \
   (defined(__MACH__) && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 1070)
I getline(S *s,size_t*n, FILE *f){ R getdelim(s,n,'\n',f);}
I getdelim(S *s,size_t*n, I d, FILE *f)//target, current capacity, delimiter, file
{
  unsigned char *q;
  I w=0;

  flockfile(f);
  //ORIENT(f,-1)  //is this dangerous?

  if (!s) {errno = EINVAL; goto error;}

  if (f->_r <= 0 && __srefill(f)) 
  {
    /* If f is at EOF already, we just need space for the NUL. */
    if (__sferror(f) || expander(s, 1)) goto error;
    funlockfile(f);
    (*s)[0] = '\0';
    R *n=-1;
  }

  while ((q = memchr(f->_p, d, f->_r)) == NULL)
  {
    if (appender(s, &w, (S) f->_p, f->_r)) goto error;
    if (__srefill(f)) 
    {
      if (__sferror(f)) goto error;
      goto done;  /* hit EOF */
    }
  }
  q++;  /* snarf the delimiter, too */
  if (appender(s, &w, (S) f->_p, q - f->_p)) goto error;
  f->_r -= q - f->_p;
  f->_p = q;

  done:
    /* Invariant: *s has space for at least w+1 bytes. */
    (*s)[w] = '\0';
    funlockfile(f);
    R *n=w;

  error:
    f->_flags |= __SERR;
    funlockfile(f);
    R *n=-1;
}
#endif

#ifdef WIN32
I getline(S *s,size_t *n, FILE *f){ R getdelim(s,n,'\n',f);}
I getdelim(S *s,size_t *n, I d, FILE *f) {   //target, current capacity, delimiter, file
#if 0 
  // this code is MSVC runtime version specific
  char *q; I w=0;
  if (!s) {errno = EINVAL; goto error;}
  if (f->_cnt <= 0) {
    if (expander(s, 1)) goto error;
    (*s)[0] = '\0'; R *n=-1;
  }
  while ((q = memchr(f->_ptr, d, f->_cnt)) == NULL) {
    if (appender(s, &w, (S) f->_ptr, f->_cnt)) goto error;
    goto done;  /* hit EOF */
  }
  q++;  /* snarf the delimiter, too */
  if (appender(s, &w, (S) f->_ptr, q - f->_ptr)) goto error;
  f->_cnt -= q - f->_ptr; f->_ptr = q;
#endif
  I w=0;
  if (!s) {errno = EINVAL; goto error;}
  for(;;) {
    C c=fgetc(f);
    if (EOF == c) R -1;
    if (appender(s, &w, (S)&c, 1)) goto error;
    if (d==c) break;
  }
  (*s)[w] = '\0'; R *n=w;
  error: R *n=-1;
}
#endif
