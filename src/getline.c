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
  R 0;
}

I getline_(S *s,size_t * __restrict__ n,FILE *f){R getdelim_(s,n,'\n',f);}

I getdelim_(S *s,size_t * __restrict__ n,I d,FILE *f)
{
  I m; S z;
  if(getdelim(s,n,d,f)==-1){*n=0; R -1;}
  m=strlenn(*s,*n);
  z=strdupn(*s,m);
  free(*s);
  *s=z;
  R *n=m;
}

#if defined(__OpenBSD__) || defined(__NetBSD__) ||  \
   (defined(__MACH__) && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 1070)
I getdelim(S *s,I*n, I d, FILE *f);

I getline(S *s,I*n, FILE *f){ return getdelim(s,n,'\n',f);}
I getdelim(S *s,I*n, I d, FILE *f)//target, current capacity, delimiter, file
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
    return *n=-1;
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
    return *n=w;

  error:
    f->_flags |= __SERR;
    funlockfile(f);
    return *n=-1;
}
#endif

#ifdef WIN32
I getline(S *s,I*n, FILE *f){ return getdelim(s,n,'\n',f);}
I getdelim(S *s,I*n, I d, FILE *f) {   //target, current capacity, delimiter, file
  //unsigned char *q;
  O("s: %s\n",s); O("sizeof(s): %d\n",sizeof(s));
  char *q;
  I w=0;
  if (!s) {errno = EINVAL; goto error;}
  if (f->_cnt <= 0) {
    if (expander(s, 1)) goto error;
    (*s)[0] = '\0'; return *n=-1;
  }
  while ((q = memchr(f->_ptr, d, f->_cnt)) == NULL) {
    if (appender(s, &w, (S) f->_ptr, f->_cnt)) goto error;
    goto done;  /* hit EOF */
  }
  q++;  /* snarf the delimiter, too */
  if (appender(s, &w, (S) f->_ptr, q - f->_ptr)) goto error;
  f->_cnt -= q - f->_ptr;
  f->_ptr = q;
  done: (*s)[w] = '\0'; R *n=w;
  error: R *n=-1;
}
#endif
