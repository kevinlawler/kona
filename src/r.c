#include "incs.h"

#if defined(__OpenBSD__) || defined(__FreeBSD__)  || defined(__NetBSD__) || defined(__ANDROID__)
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include <sys/time.h>

#include "km.h"
#include "r.h"
#include "va.h"
#include "vf.h"
#include "vg.h"

#include "kbuild.h"

#ifdef WIN32
#include "win/fnmatch.h"
#ifndef gmtime_r
struct tm*gmtime_r(const time_t*tp,struct tm*r)
{
  struct tm*p=gmtime(tp);
  memset(r,0,sizeof(*r));
  if(p){*r=*p;p=r;}
  R p;
}
#endif
#ifndef localtime_r
struct tm*localtime_r(const time_t*tp,struct tm*r)
{
  struct tm*p=localtime(tp);
  memset(r,0,sizeof(*r));
  if(p){*r=*p;p=r;}
  R p;
}
#endif
#ifndef getenv_s
int getenv_s(size_t*n,S buf,size_t nelt,cS v)
{
  if(!n||(!buf&&nelt>0)||!v)R EINVAL;
  S r=getenv(v);
  size_t req=0;
  if(r){
    req=1+strlen(r);
    if(n)*n=req;
  }
  if(nelt<req)R ERANGE;
  if(r)strcpy(buf,r);
  R 0;
}
#endif
#ifndef _putenv_s
int _putenv_s(cS nm,cS v)
{
  if(!nm||!v)R EINVAL;
  size_t req=strlen(nm)+strlen(v)+2;
  S buf=(S)alloc(req);
  if(!buf)R ENOMEM;
  sprintf(buf,"%s=%s",nm,v);
  int r=putenv(buf);
  free(buf);
  R r;
}
#endif
#endif

//Reserved verbs/functions (_verb)

K _vsx(K x,K y);
Z I CIX(K a,I i,K x);
Z I binr(K a,I b,I c,K x);
Z I date_from_jdn(I j);
Z I jdn_from_date(I year,I month,I day);
Z S rangematch(S p,C t,S r);

//'S' for [pre-]Scripted. These macros should be refactored/rewritten. Certainly don't need new K every time.
//The a=kreci stuff is a kluge we use since f##_KVAR == vs_KVAR (and KFIXED) live outside the test framework 
#define S_MONAD_(f,v,t) K v; K f(K x){I a=kreci; if(!v){U(v=X(t)) kap(&KFIXED,&v);cd(v);} K k=newK(0,1); U(k) kK(k)[0]=x; K z=vf_ex(&v,k); DO(k->n,kK(k)[i]=0) cd(k); kreci=a+1; R z; }
#define S_MONAD(f,x) S_MONAD_(_##f,f##_KVAR,x)
#define S_DYAD_(f,v,t) K v; K f(K x,K y){I a=kreci; if(!v){U(v=X(t)) kap(&KFIXED,&v);cd(v);} K k=newK(0,2); U(k) kK(k)[0]=x; kK(k)[1]=y; K z=vf_ex(&v,k); DO(k->n,kK(k)[i]=0) cd(k); kreci=a+1; R z; }
#define S_DYAD(f,x) S_DYAD_(_##f,f##_KVAR,x)
#define S_TRIAD_(f,v,t) K v; K f(K x,K y,K w){I a=kreci; if(!v){U(v=X(t)) kap(&KFIXED,&v);cd(v);} K k=newK(0,3); U(k) kK(k)[0]=x; kK(k)[1]=y; kK(k)[2]=w; K z=vf_ex(&v,k); DO(k->n,kK(k)[i]=0) cd(k); kreci=a+1; R z; }
#define S_TRIAD(f,x) S_TRIAD_(_##f,f##_KVAR,x)

S_MONAD(gtime, "{(_dj _ x % 86400; 100 _sv 24 60 60 _vsx x ! 86400)}") //will error until _sv works
S_MONAD(inv,   "{((2##*x)#1,&#*x)_lsq x}")

S_DYAD(binl, "{x _bin/: y}")
S_DYAD(dvl,  "{x@&(#y)=y?/:x}" ) 
// 151013AP S_DYAD(di,   "{:[@x;._f[. x;(!x)?/:y];x@&@[(#x)#1;y;:;0]]}") 
S_DYAD(di,   "{r::[@x;_n;(#x)#1];:[@x;. _f[. x;(!x)?/:y];x@&@[r;y;:;0]]}") 
S_DYAD(dot,  "+/*") //reimplementing this would make matrix math faster. See SSE dot call
S_DYAD(dv,   "{x _dvl ,y}") 
S_DYAD(in,   "{:[@y;x~y;:[~-2=4:y;1;~x~0n;1;0n=+/y];(#y)>y?x;0]}") 
// 151012AP S_DYAD(lin,  "{_in[;y]/:x}") 
S_DYAD(lin,  "{_in[;y]'x}") 
S_DYAD(mul,  "{x _dot\\:y}") 
S_DYAD(sv,   "{{z+y*x}/[0;x;y]}")  
S_DYAD(hat,  "{:[(1~4:x)|(2~4:x); _f[!x;y];:[@y;_f[x;,y]; x _dvl y]]}") //or "caret" or "without"

S_TRIAD(ssr, "{if[_n~x;:_n];i:1+2*!_.5*#x:(0,/(0,+/~+\\(>\':0,\"[\"=y)-<\':(\"]\"=y$:),0)+/:x _ss y)_ x;,/ :[7=4:z;@[x;i;z];4:z$:;@[x;i;:[;z]];@[x;i;:;z]]}") //missing a few things

#define W(x)      x
#define _SYSTEMN  W(T) W(a) W(d) W(f) W(h) W(i) W(k) W(m) W(n) W(p) W(s) W(t) W(u) W(v) W(w)
#define _MATH     W(acos) W(asin) W(atan) W(ceil) W(cos) W(cosh) W(exp) W(floor) W(log) W(sin) W(sinh) W(sqr) W(sqrt) W(tan) W(tanh)
//#define _SYSTEM1  _MATH W(abs) W(bd) W(ceiling) W(ci) W(db) W(dj) W(exit) W(getenv) W(gtime) W(host) W(ic) W(inv) W(jd) W(lt) W(ltime) W(size) 
//#define _SYSTEM2  W(bin) W(binl) W(di) W(dot) W(draw) W(dv) W(dvl) W(hat) W(in) W(lin) W(lsq) W(mul) W(setenv) W(sm) W(ss) W(sv) W(vsx)
//#define _SYSTEM3  W(ssr)


F sqr(F x){R pow(x,2);}
K math(F(*f)(F), K a)
{
  I at=a->t, n=a->n;
  P(ABS(at) > 2,TE)
  I t=1==ABS(at)?2*at:at;
  K e, z=newK(t,n);
  if     (0==    at )DO(n, e=kK(a)[i]; kK(z)[i]=math(f,e); if(!kK(z)[i]){cd(z);R 0;}) //should be no demote needed.
  else if(1==ABS(at))DO(n, kF(z)[i]=f(kI(a)[i]))
  else if(2==ABS(at))DO(n, kF(z)[i]=f(kF(a)[i]))
  R z;
}
K _kona_exit(K a){P(1!=ABS(a->t),TE) exit(*kI(a));}

#undef W
#define W(x) K _##x(K a){R math(x,a);}
_MATH //all the math functions
#define QUOTE(x) #x
#undef W
#define W(x) QUOTE(x)
S n_s = _SYSTEMN;         // _n type reserved: "Tadfhikmnpstuvw";
#undef W
//#define W(x) QUOTE(_##x),
//S vm_s[] = {_SYSTEM1 0}; S vd_s[] = {_SYSTEM2 0}; S vt_s[] = {_SYSTEM3 0};
//#undef W
#define W(x) _##x,
V vn_[] = {_SYSTEMN 0}; //niladic
//V vm_[] = {_SYSTEM1 0}; //monadic
//V vd_[] = {_SYSTEM2 0}; //dyadic
//V vt_[] = {_SYSTEM3 0}; //triadic

K _abs(K a) // _abs is separate from other math functions because it maintains type 1/-1 (and is stdlib.h not math.h)
{
  I t=a->t, n=a->n;
  P(ABS(t) > 2,TE)
  K z=newK(t,n);
  if     (0==    t )DO(n, kK(z)[i]=_abs(kK(a)[i])) //should be no demote needed
  else if(1==ABS(t))DO(n, kI(z)[i]=ABS(kI(a)[i]))
  else if(2==ABS(t))DO(n, kF(z)[i]=ABS(kF(a)[i]))
  R z;
}

I net(K x) {R sizeof(M1)+rep(x,0);}//#bytes in corresponding network message. see disk()
//Q/K4 uses -8!x method instead. it's still 32-bit headers, don't know how much of a speedup that is given minimum packet size / zipped IPC in kdb+2.7
K _bd(K x)//This differs from K3.2 in order to support 64-bit 
{
  I s = net(x);
  //P(s>1234567890L,LE) //"this message is too big" ? 
  K z=newK(-3,s); U(z)
  M1*m=(V)kK(z);
  I u=1;
  m->a=*(S)&u;//little-endian?
  m->n=s-sizeof(M1);
  wrep(x,sizeof(M1)+(V)m,0); //assert #bytes in x or z couldn't change
  R z; 
} 

K _ceiling(K a){R floor_ceil(a,ceil);}

K _ci(K a)
{
  I t=a->t,n=a->n;
  P(ABS(t) > 1,TE)
  K z=newK(t*3,n); 
  if(!t) DO(n,kK(z)[i]=_ci(kK(a)[i]))
  else   DO(n, kC(z)[i]=  (C) (UC) (kI(a)[i] % 256l) ); //TODO: more complete testing
  R z;
}

K _db(K x) //see _2m_r (maybe others?) I/O structure similar but not the same
{
  //TODO: do _bd and _db convert to and from network byte order? (probably not since 3: and 4: send endianness info. possibly handled closer to networking code)
  P(-3!=xt,TE)
  P(xn<sizeof(M1),LE)
  M1*m=(V)kC(x); //(m->d?little:big)-endian. TODO: support L->B and B->L conversions (or windows machine can't message sparc machine)
  P(m->n + sizeof(M1) != xn, LE)
  V p= sizeof(M1)+(V)m;
  I b=0;
  I u=1;C a=*(S)&u;
  R rrep(p,p + m->n,&b,0,m->a!=a);
}

K _dj(K a)
{
  I t=a->t,n=a->n;
  P(ABS(t) > 1,TE)
  K z=newK(t,n);
  if(!t) DO(n,kK(z)[i]=_dj(kK(a)[i]))
  else   DO(n, kI(z)[i]=date_from_jdn(kI(a)[i]));
  R z;
}

K _getenv(K a) //lfop
{
  S u=getenv(CSK(a));
  K z; I c;
  if(u) {c=strlen(u); U(z=newK(-3,c)) memcpy(kC(z),u,c);} //sic? Apparently you're not supposed to free(u=getenv())
  else z=_n();
  R z;
}


//_host addr <-> _host name {`"69.147.114.224" , `yahoo.com } <-> {1167291104}
K _host(K a) //lfop
{
  I t=a->t;

  if(4==t)
  {
    struct addrinfo b,*c;
    memset(&b,0,sizeof b);
    b.ai_family = AF_INET; //AF_INET - IPv4 ; AF_UNSPEC - unspecified ; AF_INET6 - force IPv6
    b.ai_socktype = SOCK_STREAM;

    if(!getaddrinfo(*kS(a),0,&b,&c))
    {
      I q = ntohl(((struct sockaddr_in *)c->ai_addr)->sin_addr.s_addr);
      freeaddrinfo(c);
      errno=0; 
      R Ki(q);
    }
  }
  else if(1==t)
  {
    struct sockaddr_in s;
    memset(&s,0,sizeof s);
    C host[1024];
    s.sin_family = AF_INET;
    s.sin_addr.s_addr = htonl(*kI(a)); //should work for 32-bits of ints at least

    if(!getnameinfo((struct sockaddr *)&s,sizeof s,host,sizeof host,0,0,0))
    {
      errno=0; //getnameinfo
      R Ks(sp(host)); 
    }
  }
  else R TE;

  errno=0;
  R kerr("value");
} 


K _ic(K a)
{
  I t=a->t,n=a->n;
  P(t && 3 != ABS(t),TE)
  K z=newK(t/3,n); 
  if(!t) DO(n,kK(z)[i]=_ic(kK(a)[i])) //TODO: more complete testing  "_ic _ci -300 + !605"
  else   DO(n, kI(z)[i]= (UC) (C) kC(a)[i] ); //TODO: this is weird? or escape parsing is. compare _ic "\477"  here-Ki(255) k3.2-Ki(63)
  R z;
}
K _jd(K a)
{
  I t=a->t,n=a->n,x;
  P(ABS(t) > 1,TE)
  K z=newK(t,n);
  if(!t) DO(n,kK(z)[i]=_jd(kK(a)[i]))
  else   DO(n, x=kI(a)[i]; kI(z)[i]=jdn_from_date(x/10000,(x/100)%100,x%100));
  R z;
}

K _lt(K a)
{
// see localtime_r ?
  I t=a->t,n=a->n;
  P(1<ABS(t),TE)
  const time_t b=0; struct tm c;
  localtime_r(&b,&c);
#if defined(__CYGWIN__) || defined(__WIN32)
  I d=_timezone;
#else
  I d=c.tm_gmtoff;
#endif
  K z=newK(t,n);
  if(!t) DO(n,kK(z)[i]=_lt(kK(a)[i]))
  else DO(n,kI(z)[i]=kI(a)[i]+d)
  R z;
} 
K _ltime(K a){R _gtime(_lt(a));} //TODO:mm/o

I stat_sz(S u, I*n)
{
  struct stat s; //lfop windows: GetFileSizeEx
  P(stat(u,&s),-1)
  *n=s.st_size;
  R 0;
}

K _size(K a) 
{
  I t=a->t, n=0;

  P(4!=t && 3!=ABS(t),TE)
  P(stat_sz(CSK(a),&n),SE)

  R Kf(n);
}
/////////////////////////////////////////
//Dyadic System Functions ///////////////
/////////////////////////////////////////
//K3.2 bug: (0 1;0.0 1.0) _bin 0 1 -> 2   but (0 1;0 1) _bin 0 1 -> 0
K _bin(K x,K y) 
{ P(xt>0,RE)
  R Ki(binr(x,0,xn-1,y));
} 

K _draw(K a,K b)
{
  I at=a->t,an=a->n,bt=b->t;
  K y,z;
  I c=*kI(b),n=1,j=0,k,s;
  P(1!=ABS(at)||1!=bt,IE)
  DO(an, n*=kI(a)[i]; P(n<0,IE)) //(gives wsfull - bug in k? thinks its 2^31-1 ?)
  P(c<0 && n > -c,LE)

  y=newK(c?-1:-2,n);

  //For more optimizations see Knuth Solution 3.4.2-8 (e.g. exploit > 1/2*N symmetry)
  if     (!c) DO(n,kF(y)[i]=RF())
  else if(c>0)DO(n,kI(y)[i]=c*RF()) //draw: this could be better (small numerical error)
  else if(c<0) //deal:
  {
    I d=-c;
    vitter(kI(y),y->n,d); //Vitter's algorithm
    //else DO(d,if((d-i)*RF()<(n-j))kI(y)[j++]=i; if(j==n)break;) //Knuth Algorithm 3.4.2S (better: Soln 3.4.2-8b)
    for(j=n-1;j>0;j--){k=(1+j)*RF();s=kI(y)[j];kI(y)[j]=kI(y)[k];kI(y)[k]=s;} //Knuth Algorithm 3.4.2P
  }
  z=take_reshape(a,y);
  cd(y);
  R z;
}

Z void vitter_a(I *a,I n,I N,I j) //Method A
{
  I S,i=0; 
  F top=N-n, Nreal=N, V, quot;
  while(n >= 2)
  {
    V = RF(); S=0; quot=top/Nreal;
    while (quot>V)
    {
      S++; top--; Nreal--;
      quot = (quot * top)/Nreal;
    }
    j+=S+1; 
    a[i++]=j;
    Nreal--; n--;
  }
  S = floor(round(Nreal) * RF());
  j+=S+1;
  a[i++]=j;
}

//Vitter, J.S. - An Efficient Algorithm for Sequential Random Sampling - ACM Trans. Math. Software 11 (1985), 37-57.
void vitter(I *a,I n,I N) //Method D
{
  I i=0,j=-1, t, qu1= -n+1+N, S, negalphainv=-13, threshold=-negalphainv*n;
  F nreal=n, Nreal=N, ninv=1.0/n, nmin1inv=1.0/(n-1), Vprime=exp(log(RF())*ninv),
    qu1real=-nreal+1.0+Nreal, negSreal, U, X, y1, y2, top, bottom,limit;

  while(n>1 && threshold < N)
  {
    nmin1inv=1.0/(-1.0+nreal);
    while(1)
    {
      while(1)
      {
        X = Nreal * (-Vprime + 1.0);
        S = floor(X);
        if(S<qu1) break;
        Vprime = exp(log(RF())*ninv);
      }
      U = RF(); negSreal=-S;
      y1=exp(log(U*Nreal/qu1real)*nmin1inv);
      Vprime = y1 * (-X/Nreal+1.0)*(qu1real/(negSreal+qu1real));
      if(Vprime <= 1.0) break;
      y2=1.0; top = -1.0+Nreal;
      if(-1+n > S){bottom=-nreal+Nreal;limit=-S+N;}
      else{bottom=-1.0+negSreal+Nreal; limit=qu1;} 
      for(t=N-1;t>=limit;t--)
      {
        y2=(y2*top)/bottom;
        top--; bottom--;
      }
      if(Nreal/(-X+Nreal) >= y1 * exp(log(y2)*nmin1inv)){Vprime=exp(log(RF())*nmin1inv);break;}
      Vprime = exp(log(RF()*ninv));
    }
    j+=S+1;
    a[i++]=j;
    N=-S+(-1+N); Nreal=negSreal+(-1.0+Nreal);
    n--; nreal--; ninv=nmin1inv;
    qu1=-S+qu1; qu1real=negSreal+qu1real;
    threshold+=negalphainv;
  }

  if(n>1) vitter_a(a+i,n,N,j); // if i>0 then n has been decremented
  else
  {
    S = floor(N*Vprime);
    j+=S+1;
    a[i++]=j;
  }
}

Z void svdcmp(F **a, I m, I n, F *w, F **v, F *t);

K _lsq(K a,K b)
{
  I at=a->t,an=a->n,bt=b->t,bn=b->n;
  F TOL = 1.0e-6,s;

  P(at > 0 || at < -2 || bt,TE)
  P(!an || !bn,LE)
  K x,y,z; I r=kK(b)[0]->n; P(r<=0,LE)
          DO(bn, y=kK(b)[i]; P(y->t != -1 && y->t != -2,TE) P(r != y->n,LE))
  if(!at) DO(an, y=kK(a)[i]; P(y->t != -1 && y->t != -2,TE) P(r != y->n,LE))
  else P(r != an,LE)

  I n=bn,m=MAX(r,n);

  F **u=alloc(m*  sizeof(F*)); //oom
  u[0] =alloc(n*m*sizeof(F )); //oom
  F  *w=alloc(n*  sizeof(F )); //oom
  F **v=alloc(n*  sizeof(F*)); //oom
  v[0] =alloc(n*n*sizeof(F )); //oom
  F  *t=alloc(n * sizeof(F )); //oom  (t for temp)
  DO(m,u[i]=u[0]+n*i)
  DO(n,v[i]=v[0]+n*i)
  DO(n*m,u[0][i]=0)//zero out any tacked on rows
  DO(r,DO2(n,y=kK(b)[j];u[i][j]=-2==y->t?kF(y)[i]:kI(y)[i])) //K matrix is tranposed (list is a column vector)

	svdcmp(u,m,n,w,v,t);
	F wmax=0.0;
	DO(n, if (w[i] > wmax) wmax=w[i])
	F thresh=TOL*wmax;
	DO(n, if (w[i] < thresh) w[i]=0.0)

  if(!at){z=newK(0,an); DO(an,kK(z)[i]=newK(-2,n))} //oom
  else z=newK(-2,n); //oom

  DO3(at?1:an,  //backsubstitution (see svdbksb from Numerical Recipes)
    y=at?a:kK(a)[k];
    x=at?z:kK(z)[k];
    DO(n,s=0.;if(w[i]){DO2(m,s+=u[j][i]*(-2==y->t?kF(y)[j]:kI(y)[j]))s/=w[i];}t[i]=s)
    DO(n,s=0.;DO2(n,s+=v[i][j]*t[j];kF(x)[i]=s)) 
  )

  free(u[0]);free(u);free(w);free(v[0]);free(v);free(t);

  R z;
}


//SVD stuff cribbed from TINA who cribbed from Numerical Recipes (this is ok license-wise)
#define Sign(u,v)               ( (v)>=0.0 ? ABS(u) : -ABS(u) )
Z F radius(F u, F v) //aka 'pythag' compute (a^2+b^2)^(1/2) without under-/over-flow
{
  F  Au, Av, Aw;
  Au = ABS(u);
  Av = ABS(v);
  if (Au > Av) { Aw = Av / Au; R Au * sqrt(1. + Aw * Aw); }
  if (Av != 0.0) { Aw = Au / Av; R Av * sqrt(1. + Aw * Aw); }
  R 0.0;
}

/*************************** SVDcmp *****************************************
* Given matrix A[m][n], m>=n, using svd decomposition A = U W V' to get     *
* U[m][n], W[n][n] and V[n][n], where U occupies the position of A.         *
* NOTE: if m<n, A should be filled up to square with zero rows.             *
*       A[m][n] has been destroyed by U[m][n] after the decomposition.      *
****************************************************************************/
Z void svdcmp(F **a, I m, I n, F *w, F **v, F *t)
{
    /* BUG `nm' may be used uninitialized in this function */
    I     flag, i, its, j, jj, k, l, nm, nm1 = n - 1, mm1 = m - 1;
    F  c, f, h, s, x, y, z;
    F  anorm = 0.0, g = 0.0, scale = 0.0;
    F *rv1;

    //if (m < n) O("SVDCMP: You must augment A with extra zero rows");//err, but we won't have m<n
    rv1 = t;

    /* Householder reduction to bidigonal form */
    for (i = 0; i < n; i++)
    {
        l = i + 1;
        rv1[i] = scale * g;
        g = s = scale = 0.0;
        if (i < m)
        {
            for (k = i; k < m; k++)
                scale += ABS(a[k][i]);
            if (scale)
            {
                for (k = i; k < m; k++)
                {
                    a[k][i] /= scale;
                    s += a[k][i] * a[k][i];
                }
                f = a[i][i];
                g = -Sign(sqrt(s), f);
                h = f * g - s;
                a[i][i] = f - g;
                if (i != nm1)
                {
                    for (j = l; j < n; j++)
                    {
                        for (s = 0.0, k = i; k < m; k++)
                            s += a[k][i] * a[k][j];
                        f = s / h;
                        for (k = i; k < m; k++)
                            a[k][j] += f * a[k][i];
                    }
                }
                for (k = i; k < m; k++)
                    a[k][i] *= scale;
            }
        }
        w[i] = scale * g;
        g = s = scale = 0.0;
        if (i < m && i != nm1)
        {
            for (k = l; k < n; k++)
                scale += ABS(a[i][k]);
            if (scale)
            {
                for (k = l; k < n; k++)
                {
                    a[i][k] /= scale;
                    s += a[i][k] * a[i][k];
                }
                f = a[i][l];
                g = -Sign(sqrt(s), f);
                h = f * g - s;
                a[i][l] = f - g;
                for (k = l; k < n; k++)
                    rv1[k] = a[i][k] / h;
                if (i != mm1)
                {
                    for (j = l; j < m; j++)
                    {
                        for (s = 0.0, k = l; k < n; k++)
                            s += a[j][k] * a[i][k];
                        for (k = l; k < n; k++)
                            a[j][k] += s * rv1[k];
                    }
                }
                for (k = l; k < n; k++)
                    a[i][k] *= scale;
            }
        }
        anorm = MAX(anorm, (ABS(w[i]) + ABS(rv1[i])));
    }

    /* Accumulation of right-hand transformations */
    for (i = n - 1; i >= 0; i--)
    {
        if (i < nm1)
        {
            if (g)
            {
                /* F division to avoid possible underflow */
                for (j = l; j < n; j++)
                    v[j][i] = (a[i][j] / a[i][l]) / g;
                for (j = l; j < n; j++)
                {
                    for (s = 0.0, k = l; k < n; k++)
                        s += a[i][k] * v[k][j];
                    for (k = l; k < n; k++)
                        v[k][j] += s * v[k][i];
                }
            }
            for (j = l; j < n; j++)
                v[i][j] = v[j][i] = 0.0;
        }
        v[i][i] = 1.0;
        g = rv1[i];
        l = i;
    }
    /* Accumulation of left-hand transformations */
    for (i = n - 1; i >= 0; i--)
    {
        l = i + 1;
        g = w[i];
        if (i < nm1)
            for (j = l; j < n; j++)
                a[i][j] = 0.0;
        if (g)
        {
            g = 1.0 / g;
            if (i != nm1)
            {
                for (j = l; j < n; j++)
                {
                    for (s = 0.0, k = l; k < m; k++)
                        s += a[k][i] * a[k][j];
                    f = (s / a[i][i]) * g;
                    for (k = i; k < m; k++)
                        a[k][j] += f * a[k][i];
                }
            }
            for (j = i; j < m; j++)
                a[j][i] *= g;
        } else
            for (j = i; j < m; j++)
                a[j][i] = 0.0;
        ++a[i][i];
    }
    /* diagonalization of the bidigonal form */
    for (k = n - 1; k >= 0; k--)
    {                           /* loop over singlar values */
        for (its = 0; its < 30; its++)
        {                       /* loop over allowed iterations */
            flag = 1;
            for (l = k; l >= 0; l--)
            {                   /* test for splitting */
                nm = l - 1;     /* note that rv1[l] is always zero */
                if (ABS(rv1[l]) + anorm == anorm)
                {
                    flag = 0;
                    break;
                }
                if (ABS(w[nm]) + anorm == anorm)
                    break;
            }
            if (flag)
            {
                c = 0.0;        /* cancellation of rv1[l], if l>1 */
                s = 1.0;
                for (i = l; i <= k; i++)
                {
                    f = s * rv1[i];
                    if (ABS(f) + anorm != anorm)
                    {
                        g = w[i];
                        h = radius(f, g);
                        w[i] = h;
                        h = 1.0 / h;
                        c = g * h;
                        s = (-f * h);
                        for (j = 0; j < m; j++)
                        {
                            y = a[j][nm];
                            z = a[j][i];
                            a[j][nm] = y * c + z * s;
                            a[j][i] = z * c - y * s;
                        }
                    }
                }
            }
            z = w[k];
            if (l == k)
            {                   /* convergence */
                if (z < 0.0)
                {
                    w[k] = -z;
                    for (j = 0; j < n; j++)
                        v[j][k] = (-v[j][k]);
                }
                break;
            }
            if (its == 30){ kerr("limit");  R;}  //O("No convergence in 30 SVDCMP iterations");
            x = w[l];           /* shift from bottom 2-by-2 minor */
            nm = k - 1;
            y = w[nm];
            g = rv1[nm];
            h = rv1[k];
            f = ((y - z) * (y + z) + (g - h) * (g + h)) / (2.0 * h * y);
            g = radius(f, 1.0);
            /* next QR transformation */
            f = ((x - z) * (x + z) + h * ((y / (f + Sign(g, f))) - h)) / x;
            c = s = 1.0;
            for (j = l; j <= nm; j++)
            {
                i = j + 1;
                g = rv1[i];
                y = w[i];
                h = s * g;
                g = c * g;
                z = radius(f, h);
                rv1[j] = z;
                c = f / z;
                s = h / z;
                f = x * c + g * s;
                g = g * c - x * s;
                h = y * s;
                y = y * c;
                for (jj = 0; jj < n; jj++)
                {
                    x = v[jj][j];
                    z = v[jj][i];
                    v[jj][j] = x * c + z * s;
                    v[jj][i] = z * c - x * s;
                }
                z = radius(f, h);
                w[j] = z;       /* rotation can be arbitrary id z=0 */
                if (z)
                {
                    z = 1.0 / z;
                    c = f * z;
                    s = h * z;
                }
                f = (c * g) + (s * y);
                x = (c * y) - (s * g);
                for (jj = 0; jj < m; jj++)
                {
                    y = a[jj][j];
                    z = a[jj][i];
                    a[jj][j] = y * c + z * s;
                    a[jj][i] = z * c - y * s;
                }
            }
            rv1[l] = 0.0;
            rv1[k] = f;
            w[k] = x;
        }
    }
}

#ifdef WIN32
I setenv(cS name, cS value, I overwrite)
{
    I errcode = 0;
    if(!overwrite) {
        size_t envsize = 0;
        errcode = getenv_s(&envsize, NULL, 0, name);
        if(errcode || envsize) R errcode;
    }
    R _putenv_s(name, value);
}
#endif

K _setenv(K a,K b) {
  I at=a->t, bt=b->t;
  P(at!=4 && bt!=-3,TE)//strictly these types
  I r=setenv(*kS(a),CSK(b),1);
  P(r,SE)
  R _n();
}

K _sm(K a,K b) //lfop: PathMatchSpec (or copy small BSD fnmatch.c code)
{
  //Support wildcards: ?*[^-]
  I at=a->t,an=a->n,bt=b->t,bn=b->n;

  P(0 != at && 3 != ABS(at) && 4 !=ABS(at),TE)
  P(0 != bt && 3 != ABS(bt) && 4 !=ABS(bt),TE)

  I x = (at <=0 && -3 != at), y = (bt <=0 && -3 != bt);
  P(x && y && an!=bn,LE)

  if(x || y)
  {
    a=x?promote(a):a;//mm/o (unnecessary for t=0)
    b=y?promote(b):b;//mm/o (unnecessary for t=0)
    K z=newK(0,x?a->n:b->n);
    DO(z->n, kK(z)[i]=_sm(x?kK(a)[i]:a,y?kK(b)[i]:b))
    cd(a); R demote(z);
  }

  I f=fnmatch(CSK(b),CSK(a),FNM_NOESCAPE)?0:1; //wildcard matching

  R Ki(f); //oom
}

//TODO: comprehensive tests for _ss "?[^-]"    "cool sleep fun" _ss "s[l][e][e]?"  word boundaries "15 0150 15" _ss `"15" etc.
K _ss(K a,K b) //Strong evidence K3.2 uses Boyer-Moore: wildcard at end of pattern appears to cause brute-force
{
  //K3.2, K4 - do not support * in patterns
  I at=a->t,an=a->n,bt=b->t,bn=b->n;

  P(0 != at && 3 != ABS(at) && 4 !=ABS(at),TE)
  P(0 != bt && 3 != ABS(bt) && 4 !=ABS(bt),TE)

  I x = (at <=0 && -3 != at), y = (bt <=0 && -3 != bt);
  P(x && y && an!=bn,LE)
  P((4==at || 3==ABS(at)) && !an,LE)

  if(x || y)
  {
    a=x?promote(a):ci(a);
    b=y?promote(b):ci(b);
    M(a,b)
    K z=newK(0,x?a->n:b->n);
    DO(z->n, M(a,b,z,kK(z)[i]=_ss(x?kK(a)[i]:a,y?kK(b)[i]:b)))
    cd(a);cd(b);
    R demote(z);
  }

  S t=CSK(a),p=CSK(b); //t text, p pattern

  I lp=strlen(p); 
  if(!lp)R LE;
  I *r=alloc(lp*sizeof(I)); //oom

  I n=3==ABS(a->t)?a->n:strlen(t);
  I m=0;

  C c,d;
  I occ[256];
  DO(256,occ[i]=-1); 

  C v[256],w[256];

  S q=p;
  while(*q)//Precompute bad character heuristics
  {
    r[m]=q-p;//build starting-point index into p of character/wildcard units  eg  p="h[e]llo" => r={0,1,4,5,6}

    if('?'==*q){q++; DO(256,occ[i]=m)}
    else if('['!=*q) occ[(I)*q++]=m; //construct bad char table
    else
    {
      q=rangematch(q+1,0,v);
      if(!q){free(p); R DOE;} 
      I any=0;
      DO(256,if(v[i]){occ[i]=m;any=1;})
      if(!any){free(p); R newK(-1,0);} //!0 or length err (as in "")? this pattern matches nothing (like "[^\000-\377]"). R here saves '?' logic later
    }
    m++;
  }

  K z=newK(-1,0);//oom
  I *f=alloc((m+1)*sizeof(I));//oom
  I *s=alloc((m+1)*sizeof(I));//oom
  DO(m+1,f[i]=s[i]=0)

  I i=m,j=m+1;
  f[i]=j;
  I flag;

  while(i>0) //Precompute strong good suffix heurisitics
  {
    while(j<=m) 
    {
      flag=0;
      c=p[r[i-1]]; d=p[r[j-1]];
      if(             '?'==c || '?'==d)break;
      else if(c==d && '['!=c && '['!=d)break;//The basic case is if(c==d)break; Everything else is for wildcards
      else if(        '['!=c && '['==d && rangematch(p+r[j-1]+1,c,0))break;
      else if(        '['==c && '['!=d && rangematch(p+r[i-1]+1,d,0))break;
      else if(        '['==c && '['==d){rangematch(p+r[i-1]+1,0,v); rangematch(p+r[j-1]+1,0,w);DO(256,if(v[i] && w[i]){flag=1;break;})if(flag)break;}
      if(s[j]==0) s[j]=j-i;
      j=f[j];
    }
    i--;j--;
    f[i]=j;
  }
  j=f[0];
  DO(m, if(s[i]==0)s[i]=j; if(i==j) j=f[j])

  i=0; //Begin search
  while(i<=n-m)
  {
    if(4==b->t)//if pattern is sym (eg `"123") break on word boundaries
    {
      while(i<n-m && (isalnum(t[i+m]) || (i && isalnum(t[i-1])))) i++; 
      if(i==n-m && i && isalnum(t[i-1]))break; //case "0 0 0pattern" _ss `pattern
    }

    j=m-1;
    while(j>=0)
    {
      C c=p[r[j]];
      if('?'==c);
      else if('['!=c && c!=t[i+j]) break;
      else if('['==c && !rangematch(p+r[j]+1,t[i+j],0))break;
      j--;
    }

    if(j<0) 
    {
      kap(&z,&i); //nlogn ... optimal is n (count first || allocate n/m space then trim) oom
      i+=m; //for non-overlapping matches 
      //i+=s[0]; //for overlapping matches
    }
    else i+= MAX(s[j+1],j-occ[(I)t[i+j]]);
  }

  free(r);free(f);free(s);
  R z;
}


Z S rangematch(S p, C t, S r) //BSD.  p pattern t testchar r represented. R 0 on malformed/mismatch
{
	I n, k=0; //negate, ok
	C c, d;
  if((n = '^'==*p)) ++p;
  if(r)DO(256,r[i]=n)
  if(']'==*p){if(']'==t)k=1; if(r)r[(UC)']']=!n; ++p;}
  while(']'!=(c=*p++))
  {
		if(!c)R 0;
		if('-'==*p && (d=*(p+1)) && ']'!=d)
    {
			p+=2;
			if(!d)R 0;
			if((UC)c<=(UC)t && (UC)t<=(UC)d) k=1;
      if(r)DO(1+(UC)d-(UC)c, r[i+(UC)c] = !n) 
		}
    else{if(c==t)k=1; if(r)r[(UC)c]=!n;}
	}
	R t&&k==n?0:p; //null t => ignore match
}

void Ireverse(K x){DO(x->n/2, I t=kI(x)[x->n-i-1]; kI(x)[x->n-i-1]=kI(x)[i]; kI(x)[i]=t)}

K _vsx(K x,K y) //vector from scalar (improved version), radix & clock arithmetic (unbounded & bounded)
{
  P(1 < ABS(y->t) || 1!=ABS(xt), TE)

  K z=0;

  if(0==y->t){U(z=newK(0,y->n)) DO(y->n, M(z,kK(z)[i]=_vsx(x,kK(y)[i])))} //eachright
  else if(-1==y->t)//eachright  (we deviate. K3.2 has a k implementation with values "flipped" with front zero-fill)
  {
    z = newK(0,y->n);
    K k = Ki(0);
    M(k,z)
    DO(y->n, *kI(k)=kI(y)[i]; M(z,k,kK(z)[i]=_vsx(x,k)))
    cd(k);
    z=demote(z);
  }
  else if(1==xt) // && 1==y->t /radix
  { //K3.2 mishandles 2 _vs 0 from our perspective (could be said to mishandle _vs, though see above) {(1_|{ _ y % x}[x]\y)!x} 
    P(*kI(x)<2, DOE)
    U(z=newK(-1,0))
    I a = *kI(x), b = *kI(y), c=b/a;
    while(!z->n || b!=c) //need 1, but lookahead to dodge any fixed points
    {
      kap(&z,&b);
      b = c;
      c = b/a;
    }
    DO(z->n, kI(z)[i] %= a)
    Ireverse(z);
  }
  else if(-1==xt) // && 1==y->t /clock
  {
    //{|(-1 _ j)-a*1 _ j:y(_%)\a:|x}
    DO(xn, if(kI(x)[i]<1) R DOE)
    U(z=newK(-1,xn))
    I a = *kI(y), n =z->n;
    if(a<0){I s=1;DO(xn,s*=kI(x)[i]) a = s - (-a % s);} //a nice property. maybe not crucial
    DO(n, kI(z)[i] = kI(x)[x->n-1-i])  // z:|x
    if(n) *kI(z) = *kI(z)? a / *kI(z):0; 
    DO(n-1, I d=kI(z)[i+1]; kI(z)[i+1] = d?kI(z)[i]/d:0) // divide scan
    DO(n-1, kI(z)[n-1-i] = kI(z)[n-2-i] - kI(z)[n-1-i] * kI(x)[i] ) 
    if(n) *kI(z) = a - *kI(z) * kI(x)[xn-1];
    Ireverse(z);
    R z;
  }

  R z;
}

K _vs(K x,K y) {   //vector from scalar (k3 version), radix & clock arithmetic (unbounded & bounded)
  P(1 < ABS(y->t) || 1!=ABS(xt), TE)
  K z=0;
  if(0==y->t){U(z=newK(0,y->n)) DO(y->n, M(z,kK(z)[i]=_vs(x,kK(y)[i])))} //eachright
  else if(-1==y->t) {
    z = newK(0,y->n);
    K k = Ki(0);
    M(k,z)
    DO(y->n, *kI(k)=kI(y)[i]; M(z,k,kK(z)[i]=_vs(x,k)))
    cd(k);
    z=demote(z); }
  else if(1==xt) {
    P(*kI(x)<2, DOE)
    P(*kI(y)==0, X("!0"))
    U(z=newK(-1,0))
    I a = *kI(x), b = *kI(y), c=b/a;
    while(!z->n || b!=c) {  //need 1, but lookahead to dodge any fixed points
      kap(&z,&b);
      b = c;
      c = b/a; }
    DO(z->n, kI(z)[i] %= a)
    Ireverse(z); }
  else if(-1==xt) {   // && 1==y->t /clock
    DO(xn, if(kI(x)[i]<1) R DOE)
    U(z=newK(-1,xn))
    I a = *kI(y), n =z->n;
    if(a<0){I s=1;DO(xn,s*=kI(x)[i]) a = s - (-a % s);} //a nice property. maybe not crucial
    DO(n, kI(z)[i] = kI(x)[x->n-1-i])  // z:|x
    if(n) *kI(z) = *kI(z)? a / *kI(z):0; 
    DO(n-1, I d=kI(z)[i+1]; kI(z)[i+1] = d?kI(z)[i]/d:0) // divide scan
    DO(n-1, kI(z)[n-1-i] = kI(z)[n-2-i] - kI(z)[n-1-i] * kI(x)[i] ) 
    if(n) *kI(z) = a - *kI(z) * kI(x)[xn-1];
    Ireverse(z);
    R z; }
  R z; }

/////////////////////////////////////////
//Niladic (Reserved Symbols) ////////////
/////////////////////////////////////////
K _t(){R Ki(time(0) + k_epoch_offset);}  
K _T()
{
  struct timeval t;
  time_t tr;
  struct tm u;
  gettimeofday(&t,0);
  tr = t.tv_sec;
  gmtime_r(&tr,&u);
  R Kf(jdn_from_date(1900+u.tm_year,1+u.tm_mon,u.tm_mday)+((u.tm_hour*60*60 + u.tm_min*60 + u.tm_sec + t.tv_usec/1.0e6)/86400.0));
}
K _n(){R ci(NIL);}  
K _h()
{
  C c[256];
  P(gethostname(c,256),SE)
  R Ks(sp(c));  //oom
}
K _d(){R Ks(d_);}
K _v(){R ci(KONA_GSET);}
K _i(){R ci(KONA_IDX);}
K _f(){R 0;} //Dummy function. Actual value computed inside of the parse function
K _s(){R mstat();}
K _p(){R ci(KONA_PORT);}
K _w(){R ci(KONA_WHO);}
K _u(){R NYI;}
K _a(){R ci(KONA_ARGS);}
K _k(){Z K x=0;if(!x){Z S d=KBUILD_DATE;x=newK(-3,strlen(d));M(x);strcpy(kC(x),d);}R ci(x);}
K _m(){R NYI;}

/////////////////////////////////////////
//Utility Functions /////////////////////
/////////////////////////////////////////
Z I CIX(K a,I i,K x) //compare a[i] vs x,  a->t <= 0
{
  I at=a->t;
  I t=x->t,r=0;
  K k=0;

  if(!at) k=ci(kK(a)[i]);
  else k=newK(-at,1); //mm/o replace this and above and below with itemAtIndex using ci() not cl0ne()  ?

  switch(k->t){CS(1,*kI(k)=kI(a)[i])
               CS(2,*kF(k)=kF(a)[i])
               CS(3,*kC(k)=kC(a)[i])
               CS(4,*kS(k)=kS(a)[i])
              } 

  //HACK: K3.2 handles x=1|2, a=-1|-2 case but not x=-1|-2. see K3.2 bug at _bin
  //Note: Comparing 64-bit integer to 64-bit double is going to cause problems (int overflows mantissa, double doesn't fit in int)
  if(1==k->t && 2==t) r=FC(I2F(*kI(k)),*kF(x));  // 0 2 3 _bin 1.0 -> 1 , unexpected
  else if (2==k->t && 1==t) r=FC(*kF(k),I2F(*kI(x)));
  else r=KC(k,x);

  if(k)cd(k);
  R r;
}

Z I binr(K a,I b,I c,K x)
{
  I i=b+(c-b)/2, r=CIX(a,i,x);//i is sic
  if(0==r) { if(i>0 && !CIX(a,i-1,x))r=1;else R i; }
  if(b>=c)R -1==r?1+i:i;//pos if you did insert into list
  R 0<r?binr(a,b,i-1,x):binr(a,i+1,c,x);
}

F RF(){ R genrand64_real2();}// [0,1) uniformly random double

I k_epoch_offset = -2051222400l; // -[dates 20350101 - 19700101 in seconds]
K _dot_t()
{
  struct timeval t;
  gettimeofday(&t,0);
  return Kf(k_epoch_offset + (F)t.tv_sec + (F)t.tv_usec/1.0e6);
}

Z I jdn_from_date(I year, I month, I day) //This and the other one Fair Use / Boost License Claus Tondering
{
  I a = (14-month)/12;
  I y = year+4800-a;
  I m = month + 12*a - 3;
  R day + (153*m+2)/5 + y*365 + y/4 - y/100 + y/400 - 32045 - 2464329; //Last constant shifts day 0 to 20350101 epoch
}

Z I date_from_jdn(I j)
{
  I b, c, d, e, m;
  I year,month,day;

  I a = j + 32044 + 2464329;//resynch epoch to 20350101
  b = (4*a+3)/146097;
  c = a - (b*146097)/4;

  d = (4*c+3)/1461;
  e = c - (1461*d)/4;
  m = (5*e+2)/153;

  day   = e - (153*m+2)/5 + 1;
  month = m + 3 - 12*(m/10);
  year  = b*100 + d - 4800 + m/10;
  
  R year*10000 + month*100 + day;
}


