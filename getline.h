K _p();
#if defined(__OpenBSD__) || \
   (defined(__MACH__) && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 1070)
I getline(S *s,I *n,FILE *f);
I getdelim(S *s,I *n,I d,FILE *f);
I getdelim(S *s,I *n,I d,FILE *f);
#endif
S strdupn(S s,I k);
I strlenn(S s,I k);
I getdelim_(S *s,I *n,I d,FILE *f);
I getline_(S *s,I *n,FILE *f);
I appender(S *s,I *n,S t,I k);
I expander(S *s,I n);
