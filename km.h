I rp2(I v);
K newE(S s,K k);
K newEntry(S s);
extern S __d;
K Kv();
K Kn();
K Kd();
K Ks(S x);
K Kc(C x);
K Kf(F x);
K Ki(I x);
void pdafree(PDA p);
C bottom(PDA p);
C pop(PDA p);
C peek(PDA p);
I appender(S *s,I *n,S t,I k);
I push(PDA p,C c);
PDA newPDA();
N newN();
K kap(K *a,V v);
K kapn(K *a,V v,I n);
K _n();
extern F testtime;
K newK(I t,I n);
I bp(I t);
K ci(K a);
I repool(V v,I r);
I lsz(I k);
I sz(I t,I n);
I PG();
#if defined(DEBUG)
extern V krec[1000000];
extern I kreci;
#endif
K show(K a);
extern I tests;
K cd(K a);
extern V KP[sizeof(V)*8+1];
I OOM_CD(I g, ...);
