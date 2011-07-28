/* networking */

#include "incs.h"

#include <netinet/tcp.h> //#include <sys/socket.h> //#include <netinet/in.h>

#if defined(__OpenBSD__) || defined(__FreeBSD__)
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include "k.h"
#include "kn.h"

Z I close_tape(I i);
Z K modified_execute(K x);


void *get_in_addr(struct sockaddr *sa) { if (sa->sa_family == AF_INET) R &(((struct sockaddr_in*)sa)->sin_addr);R  &(((struct sockaddr_in6*)sa)->sin6_addr); } // get sockaddr, IPv4 or IPv6

M0 CP[FD_SETSIZE]; //Connection Pool (large array)

I wipe_tape(I i) { if(CP[i].k)cd(CP[i].k); memset(&CP[i],0,sizeof(CP[0])); R 0;} //safe to call >1 time
Z I close_tape(I i) { wipe_tape(i); close(i); FD_CLR(i, &master); R 0;}

K read_tape(I i, I type) // type in {0,1} -> {select loop, 4: resp reader}
{
  I c=CP[i].r, m=sizeof(M1),g; K z=0;
  S b = c<m?c+(S)&CP[i].m1:c+kC(CP[i].k); 
  g = c<m?m-c:CP[i].m1.n; 
  I nbytes = recv(i,b,g,0); 
  if(nbytes <= 0)
  {
    if (nbytes == 0);//printf("server: socket %ld hung up\n", i);
    else perror("recv");
    GC;
  }
  //fill struct data + k data 
  CP[i].r += nbytes; //DO(nbytes, O("b%ld : %o\n",i,(UC)b[i]))
  if(m == CP[i].r) //We've read enough bytes to fill our struct m1 with transmission data (it's also the _db header)
  {
    //TODO: so that we get the right sizes, etc, in the M1, rearrange bytes based on little-endianness indicator CP[i].m1.a
    //if(sizeof(M1)+CP[i].m1.n > 987654321) GC; //protect against too big?
    K k = newK(-3, m+CP[i].m1.n);
    if(!(CP[i].k=k))GC;
    memcpy(kC(k),&CP[i].m1,m); //cpy data from our struct to the corresponding spot on the '_bd' object
  }
  if(CP[i].r == m + CP[i].m1.n) //the k for the _db is completed. perform modified execution, potentially respond
  {
    //TODO: (here or in _db?) rearrange bytes based on little-endianness indicator CP[i].m1.a
    M1*p=(V)kC(CP[i].k);
    I msg_type = p->d; //p->d dissappears after wipe_tape
    K h = _db(CP[i].k);
    if(!h) GC;
    wipe_tape(i);

    //blocking read inside 4: receives response //response sent by server to client after a 4: request is not executed by client
    if(2==msg_type && 1==type) R h; 

    //Modified execution of received K value. First received transmission in a 3: or 4: 
    z=modified_execute(h);
    cd(h);
    //indicates received communication from 4: synchronous method which expects response
    if(z) if(1==msg_type && 0==type) ksender(i,z,2);
    cd(z); z=0;
  }

  R z;
cleanup:
  close_tape(i);
  R (K)-1;
}

Z K modified_execute(K x) //TODO: consider: this should be modified to use error trap. _4d should be modified to expect error trap output. 
{
  //K-Lite manual gives {:[4:x; .x; .[.;x]} as processing function
  if(4==xt || 3==ABS(xt)) R X(CSK(x));
  if(!xt && xn>0) R vf_ex(offsetDot,x); R ci(x);
}
