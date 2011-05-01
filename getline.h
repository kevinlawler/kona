K _p();
#if defined(__MACH__) || defined(__OpenBSD__)
I getline(S *s,I *n,FILE *f);
#endif
S strdupn(S s,I k);
I strlenn(S s,I k);
#if defined(__MACH__) || defined(__OpenBSD__)
I getdelim(S *s,I *n,I d,FILE *f);
I getdelim(S *s,I *n,I d,FILE *f);
#endif
I getdelim_(S *s,I *n,I d,FILE *f);
I getline_(S *s,I *n,FILE *f);
I appender(S *s,I *n,S t,I k);
I expander(S *s,I n);
