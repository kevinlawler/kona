#ifndef	_KONA_H_
#define _KONA_H_

/*
 * Interface between C & K 
 * atom functions/accessors:
 * list functions/accessors:
 * misc functions: ci, cd
 */
#ifdef	__cplusplus
extern "C" {
#endif

/* abbreviations  */
#define	O printf
#define	R return
#define Z static

/* types */
typedef void* V;
typedef long long I;
typedef double F;
typedef char C;
typedef C* S;
typedef const C* cS;
typedef unsigned char UC;
typedef unsigned long UI;

/* the main struct */
typedef struct k0{I c,t,n;struct k0*k[1];}*K;

#define ke(x) (((K)x)->k)

/* list accessors */
#define KI(x) ((I*)((x)->k))
#define KF(x) ((F*)((x)->k))
#define KC(x) ((UC*)((x)->k))
#define KS(x) ((S*)((x)->k))
#define KK(x) ((K*)((x)->k))

#define KV(x) ((V*)ke(x))
extern K kap(K*,V);

/* atom accessors */
#define Ki(x)	(*KI(x))
#define Kf(x)	(*KF(x))
#define Kc(x)	(*KC(x))
#define Ks(x)	(*KS(x))

/* atom generators */
extern K gi(I);
extern K gf(F);
extern K gc(C);
extern K gs(S);
extern K gn();

/* list generators */
extern K gtn(I,I);
extern K gsk(S,K);
extern K gp(S);
extern K gpn(S,I);
extern K gnk(I,...);

extern K kerr(S);

/* Call k from c */
extern K ksk(S,K);
extern I sfn(S,K(*)(),I);


/* ref counting */
extern K ci(K);
extern I cd(K);

/* date conversion */
extern I jd(I);
extern I dj(I);

/* callbacks */
extern I sdf(I,I(*)());
extern I scd(I);

extern S sp(S);

#define DO(n,x) {I i,_n=(n);for(i=0;i<_n;++i){x;}}

#ifdef	__cplusplus
}
#endif

#endif/*_KONA_H_*/
