//For libraries dynamically loaded by 2: dyadic
#include <stdio.h>
typedef void* V;
typedef long long I;
typedef double F;
typedef char C;
typedef C* S;
typedef unsigned char UC;
typedef struct k0{I c,t,n;struct k0*k[1];}*K;
#define ke(x) (((K)x)->k)
#define kK(x) ((K*)ke(x))
#define kI(x) ((I*)ke(x))
#define kF(x) ((F*)ke(x))
#define kC(x) ((C*)ke(x))
#define kS(x) ((S*)ke(x))
#define O printf
#define R return
extern K Ki(I);
extern K Kf(F);
extern K newK(I t, I n);
