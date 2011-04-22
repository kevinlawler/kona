/* K object management */

#include "incs.h"

#include "k.h"
#include "km.h"
#include "ko.h"

K kclone(K a)//Deep copy -- eliminate where possible
{
  if(!a) R 0;
  I t=a->t,n=a->n;
  K z= 7==t?Kv():newK(t,n);
  if     (4==ABS(t)) DO(n, kS(z)[i]=kS(a)[i])  //memcpy everywhere is better
  else if(3==ABS(t)) DO(n, kC(z)[i]=kC(a)[i]) 
  else if(2==ABS(t)) DO(n, kF(z)[i]=kF(a)[i]) 
  else if(1==ABS(t)) DO(n, kI(z)[i]=kI(a)[i]) 
  else if(0==    t ) DO(n, kK(z)[i]=kclone(kK(a)[i])) 
  else if(5==    t ) DO(n, kK(z)[i]=kclone(kK(a)[i]))
  else if(7==    t )
  {
    I k=0;

    z->t=a->t; 
    I vt=z->n = a->n;
    K kv;

    V*v;
    SW(vt)
    {
      CS(1, k=((K)kV(a)[CODE])->n-1;
            M(z,kv=newK(-4,k+1))
            v=(V*)kK(kv);
            //v[k]=0;//superfluous reminder
            DO(k, V w=kW(a)[i];
                  if(VA(w))v[i]=w;  //TODO: is this ok for NAMES? see similar code in capture()
                  else
                  {
                    K r=kclone(*(K*)w); //oom
                    V q=newE(LS,r); //oom
                    kap((K*) kV(z)+LOCALS,q);//oom
                    cd(q);//kap does ci
                    q=EVP(q); //oom free z etc. kap needs checking 
                    v[i]=q;
                  }
              )
      )
      CS(2, M(z,kv=newK(-4,3))
            v=(V*)kK(kv);
            memcpy(v,kW(a),3*sizeof(V));
        )
      CS(3,M(z,kv=kclone((K)kV(a)[CODE])))
    }
    kV(z)[CODE]=kv;
    kV(z)[DEPTH]=kV(a)[DEPTH];   
    kV(z)[CONTEXT]=kV(a)[CONTEXT];   
    cd(kV(z)[PARAMS]); kV(z)[PARAMS]=kclone(kV(a)[PARAMS]); //oom ; fill instead of kclone?
    cd(kV(z)[LOCALS]); kV(z)[LOCALS]=kclone(kV(a)[LOCALS]); //oom ; fill instead of kclone?
    kV(z)[CONJ]=kclone(kV(a)[CONJ]);  //oom
  }

  R z;
}

K collapse(K x) //oom
{
  K z;
  if(1==xn){ z=ci(*kK(x)); cd(x);} 
  else z=demote(x);
  R z;
}

K demote(K a)//Attempt to force unnaturally occurring lists into vectors
{ // change: (0;1;2) ->  0 1 2
  //   keep: (1;0.66667)  //numerics are not reconciled as you might guess
  // change: (1) -> ,1    //doesn't solve parenthetical expressions offhand
  if(!a) R a; //dollar() uses this
  I t=a->t, n=a->n;
  if(0!=t || 1>n) R a;
  I p=kK(a)[0]->t;
  DO(n, if(p!=kK(a)[i]->t)p=0) 
  if(!(1<=p && p <= 4))R a;
  K z=newK(-p,n); M(a,z) 
  if     (4==p)DO(n,kS(z)[i]=*kS(kK(a)[i])) //use memcpy instead
  else if(3==p)DO(n,kC(z)[i]=*kC(kK(a)[i]))
  else if(2==p)DO(n,kF(z)[i]=*kF(kK(a)[i]))
  else if(1==p)DO(n,kI(z)[i]=*kI(kK(a)[i]))
  cd(a);
  R z;
}
K promote(K a)//Identity on lists. Lists from vectors. Pseudo-enlist on atoms (always 0-lists).
{ //0 1 2 -> (0;1;2) 
  I at=a->t;
  if(0==at) R ci(a);
  if(4< at) {K z=newK(0,1); U(z); *kK(z)=ci(a); R z;}
  K z=newK(0,a->n); U(z);
  K x;
  I v=ABS(at);
  if     (4==v) DO(a->n, x=newK(v,1); M(x,z) *kS(x)=kS(a)[i]; kK(z)[i]=x ) 
  else if(3==v) DO(a->n, x=newK(v,1); M(x,z) *kC(x)=kC(a)[i]; kK(z)[i]=x ) 
  else if(2==v) DO(a->n, x=newK(v,1); M(x,z) *kF(x)=kF(a)[i]; kK(z)[i]=x ) 
  else if(1==v) DO(a->n, x=newK(v,1); M(x,z) *kI(x)=kI(a)[i]; kK(z)[i]=x ) 
  R z;
}
