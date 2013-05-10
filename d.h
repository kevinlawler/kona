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

K f(K x,K y){R Ki(kI(x)[0]+ kI(y)[0]);}
K g(K x){R Kf(kF(x)[0]+1);} 
K h(K a,K b,K c,K d,K e,K f,K g,K h){R Ki(123);} 


