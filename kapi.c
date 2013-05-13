#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include "kona.h"

extern K wd(S s, I n);
extern K dot(K,K);
extern K newK(I t, I n);
extern K newE(S s, K k);
extern I _jd(I);
extern I _dj(I);
extern I kinit();

K gi(I x) {K z=newK(1,1); Ki(z)=x; R z;}
K gf(F x) {K z=newK(2,1); Kf(z)=x; R z;}
K gc(C x) {K z=newK(3,1); Kc(z)=x; R z;}
K gs(S x) {K z=newK(4,1); Ks(z)=x; R z;}
K gn()    {K z=newK(6,1); R z;}

K gtn(I t, I n) { R newK(t,n); }

Z K gpn_(S s, I i) {K z=gtn(-3,i); memcpy(KC(z),s,i); R z; }
K gpn(S s, I i) {I n=strlen(s); if(i<n) R gpn_(s, i); else R 0;}
K gp(S s) {R gpn_(s,strlen(s));}

I sfn(S s, K(*f)(), I i) { R 0; } // XXX
I sdf(I i, I(*f)()) { R 0; } // XXX
I scd(I i) { R close(i);}

K gsk(S s, K k) { R newE(sp(s),k); } // XXX probablu should return k

I jd(I i) { R _jd(i);}
I dj(I i) { R _jd(i);}

K gnk(I n, ...)
{
	K z = gtn(0, n);
	va_list v;
	va_start(v, n);
	DO(n, z->k[i] = va_arg(v, K));
	va_end(v);
	R z;
}

K ksk(S s, K x)
{
	K z;
	if (!*s) { kinit(); R 0; }
	z = wd(s, strlen(s));
	R dot(z, x);
}
