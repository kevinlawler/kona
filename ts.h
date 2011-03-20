#ifndef TS_H
#define TS_H

typedef void* V;
typedef long I; //there are cases where casting pointer arithmetic to signed int will fail
typedef double F;
typedef char C; //Store +-3 type '\0' terminated
typedef C* S;
typedef unsigned char UC;
typedef unsigned long UI;
typedef I veci __attribute__ ((vector_size (16)));
typedef struct k0{I c,t,n;struct k0*k[1];}*K; //main K object
typedef struct m1{char a,b,c[sizeof(I)-3],d;I n;} M1; //inet sent message header.  m.a?little-:big-endian,m.b is type???, m.d in {0,1,2}->{3:,4:,response}, m.n is size of nested K struct in bytes. c unknown, inserted [5] for 64b alignment
typedef struct m0{M1 m1;I r;K k;} M0; //r=read so far. inet message reader. there is probably a more elegant way to do this
enum TYPE_SEVEN_MEMBERS {CONTEXT,CODE,LOCALS,PARAMS,CONJ,TYPE_SEVEN_SIZE};  //sp(), code in {-4, -4, -4[3], -3, -4,-4,-4,-4}, Kd(), Kd(), Kv()/0-List-w/-NULLs
//Executable types: t-n is 7-n for n in {0,1,2,3,4,5,6,7}: 0: list of unexecuted types, 1: [derived] verb, 2: dynamically loaded function, 3: brace function{}, 4: ":[]", 5: if[], 6: while[], 7: do[]
typedef struct node{V k,v;I b;struct node *c[2];}Node;typedef Node*N;//Knuth's AVL tree
typedef struct pda{I i,s,n;S c;}Pda;typedef Pda*PDA; //holds parse state. pos in input, state, stacklength, stack
#define ke(x) (((K)x)->k)
#define kK(x) ke(x)
#define kI(x) ((I*)ke(x))
#define kF(x) ((F*)ke(x))
#define kC(x) ((C*)ke(x))//Chars/Char-strings (+3/-3) must have a terminal '\0' (uncounted), but may also contain them
#define kS(x) ((S*)ke(x))//Symbol pointers to interned strings ending at the first '\0'
#define kV(x) ((V*)ke(x))
#define kVC(x) ((K)kV(x)[CODE])
#define kW(x) ((V*)kS(kVC(x)))
#define II LONG_MAX //I Infinity (Use -II for I Negative Infinity)
#define IN LONG_MIN //I Null (one less than -II)
#define FI 1/0.     //IEEE should work everywhere 
#define FN 0/0.     //Alternate takes can be found in Arthur's "k.h"
#define Z static
#define O printf
#define R return
#define xt x->t
#define xn x->n
#define DO(n,x) {I i=0,_i=(n);for(;i<_i;++i){x;}}
#define DO2(n,x){I j=0,_j=(n);for(;j<_j;++j){x;}}
#define DO3(n,x){I k=0,_k=(n);for(;k<_k;++k){x;}}
#define CS(n,x) case n:x;break;
#define CSR(n,x) case n:x;
#define AE(x) (sizeof(x)/sizeof(x[0]))
#define SW switch
#define CD default
#define diff(x,y) (((V*)(x)) - (V*)(y))
#define in(x,y)   (diff(x,y) < AE(y))
#define ABS(x)    ((x) < 0 ? -(x) : (x))
#define SIGN(x)    ((x) < 0 ? -(1) : (1))

#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

#define _(...) X(#__VA_ARGS__)
#define GC goto cleanup
#define STDIN fileno(stdin)

#define P(x,y) {if(x)R(y);}
#define U(x) P(!(x),0)
#define M(...) U(OOM_CD(0,__VA_ARGS__,(V)-1)) //0 in (...)?  cd(...), R 0; Alternative to "goto cleanup", precursor to oom-handler in memory-manager
#define SE kerr(strerror(errno)) // not-reentrant, use strerror_r
#define ME kerr("wsfull") //In general only directly allocating functions should call this
#define TE kerr("type") //see http://kx.com/a/k/document/error.txt
#define VE kerr("valence") 
#define PE kerr("parse")
#define IE kerr("int") 
#define XE kerr("index") 
#define LE kerr("length") 
#define RE kerr("rank")
#define NE kerr("nonce")
#define DOE kerr("domain") 
#define NYI kerr("nyi")

#define RTIME(d,...) {d=clock();{__VA_ARGS__;}d=(clock()-d)/CLOCKS_PER_SEC;}
#define TIME(...) {F d; RTIME(d,__VA_ARGS__); O("Elapsed:%.7f\n",d);}
#define dump(x, fmt) {fprintf(stderr, "%s:%u: %s=" fmt "\n", __FILE__, __LINE__, #x, x);}
#define dd(x) dump((I)x,"%ld")
#define er(x) {fprintf(stderr, "%s:%u: %s\n",__FILE__, __LINE__, #x);}

#endif
