/* networking */

#include "incs.h"
#include "k.h"
#include "kn.h"

#if defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__ANDROID__)
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#ifndef WIN32
#include <netinet/tcp.h> //#include <sys/socket.h> //#include <netinet/in.h>
M0 CP[FD_SETSIZE]; //Connection Pool (large array)
#else
M0 CP[10];
#endif

Z I close_tape(I i);
Z K modified_execute(K x);

void *get_in_addr(struct sockaddr *sa) {   //get sockaddr, IPv4 or IPv6
  if (sa->sa_family == AF_INET) R &(((struct sockaddr_in*)sa)->sin_addr);
  R  &(((struct sockaddr_in6*)sa)->sin6_addr); }

I wipe_tape(I i) { if(CP[i].k)cd(CP[i].k); memset(&CP[i],0,sizeof(CP[0])); R 0;} //safe to call >1 time
Z I close_tape(I i) { wipe_tape(i); close(i); FD_CLR(i, &master); R 0;}

#ifndef WIN32

C bx[128]={0},by[128]={0};
K read_tape(I i, I type) {   // type in {0,1} -> {select loop, 4: resp reader}
  I nbytes=0,n=0; C bz[128]={0},bn[1]={0};
  if(HTTP_PORT) {
    if(bx[0]=='\0')nbytes=recv(i,bx,128,0);
    else if(by[0]=='\0')nbytes=recv(i,by,128,0);
    else nbytes=recv(i,bz,128,0);
    if(nbytes<=0){if (nbytes==0)printf("server: socket %lld hung up\n", i); else perror("recv"); GC;}
    K h=*denameS(".",".m.h",0); 
    if(6==h->t){send(i,bx,nbytes,0); bx[0]='\0'; close_tape(i); R (K)0;}  //echo back only if .m.h does not exist
    else {
      if(7!=h->t && 3!=h->n) {
        I n=snprintf(bx,128,"%s",".m.h is not type 7-3"); if(n>=128)R WE;
        send(i,bx,strlen(bx),0); }
      else {   //have .m.h of type 7-3
        S f=kC(kK(h)[CODE]); I ax=0,ay=0,az=0,sf=strlen(f);
        DO(sf, if(f[i]=='x')ax=1; else if(f[i]=='y')ay=1; else if(f[i]=='z')az=1;) I na=maX(1,ax+ay+az);
        if(na==3) {
          if(bz[0]=='\0'){ send(i,bn,1,0); close_tape(i); R (K)0; }
          else {
            for(n=0;n<128;++n){if(bx[n]=='\r' || bx[n]=='\0')break;} bx[n]='\0';
            for(n=0;n<128;++n){if(by[n]=='\r' || by[n]=='\0')break;} by[n]='\0';
            for(n=0;n<128;++n){if(bz[n]=='\r')break;} bz[n]='\0';
            I sbx=strlen(bx); I sby=strlen(by); I sbz=strlen(bz); 
            C c[13+sf+sbx+sby+sbz]; c[0]='{'; c[1+sf]='}'; c[2+sf]='['; c[11+sf+sbx+sby+sbz]=']';
            c[3+sf]=c[4+sf+sbx]=c[6+sf+sbx]=c[7+sf+sbx+sby]=c[9+sf+sbx+sby]=c[10+sf+sbx+sby+sbz]='"';
            c[5+sf+sbx]=c[8+sf+sbx+sby]=';'; c[12+sf+sbx+sby+sbz]='\0';
            DO(sf,c[1+i]=f[i]) DO(sbx,c[4+sf+i]=bx[i]) DO(sby,c[7+sf+sbx+i]=by[i]) DO(sbz,c[10+sf+sbx+sby+i]=bz[i])
            K r=X(c); I w=128; C bck[w];
            switch(r->t) {
              CS(1, {n=snprintf(bck,w,"%lld",*kI(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
              CS(2, {n=snprintf(bck,w,"%f",*kF(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
              CS(3, {n=snprintf(bck,w,"%s",kC(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
              CS(-3,{n=snprintf(bck,w,"%s",kC(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
              default:{n=snprintf(bck,w,"%s","NYI: .m.h result of that type and count"); if(n>=w)R WE;} }
            send(i,bck,strlen(bck),0); bx[0]='\0'; by[0]='\0'; close_tape(i); R (K)0; } }
        if(na==2) {
          if(by[0]=='\0'){ send(i,bn,1,0); close_tape(i); R (K)0; }
          else {
            for(n=0;n<128;++n){if(bx[n]=='\r')break;} bx[n]='\0';
            for(n=0;n<128;++n){if(by[n]=='\r')break;} by[n]='\0';
            I sbx=strlen(bx); I sby=strlen(by);
            C c[10+sf+sbx+sby]; c[0]='{'; c[1+sf]='}'; c[2+sf]='['; c[8+sf+sbx+sby]=']';
            c[3+sf]=c[4+sf+sbx]=c[6+sf+sbx]=c[7+sf+sbx+sby]='"';
            c[5+sf+sbx]=';'; c[9+sf+sbx+sby]='\0';
            DO(sf,c[1+i]=f[i])  DO(sbx,c[4+sf+i]=bx[i]) DO(sby,c[7+sf+sbx+i]=by[i])
            K r=X(c); I w=128; C bck[w];
            switch(r->t) {
              CS(1, {n=snprintf(bck,w,"%lld",*kI(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
              CS(2, {n=snprintf(bck,w,"%f",*kF(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
              CS(3, {n=snprintf(bck,w,"%s",kC(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
              CS(-3,{n=snprintf(bck,w,"%s",kC(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
              default:{n=snprintf(bck,w,"%s","NYI: .m.h result of that type and count"); if(n>=w)R WE;} }
            send(i,bck,strlen(bck),0); bx[0]='\0'; by[0]='\0'; close_tape(i); R (K)0; } }
        for(n=0;n<128;n++){if(bx[n]=='\r' || bx[n]=='\0')break;}
        bx[n]='\0'; I sbx=strlen(bx);
        C c[7+sf+sbx]; c[0]='{'; c[1+sf]='}'; c[2+sf]='['; c[5+sf+sbx]=']';
        c[3+sf]=c[4+sf+sbx]='"'; c[6+sf+sbx]='\0';
        DO(sf,c[1+i]=f[i])  DO(sbx,c[4+sf+i]=bx[i])
        K r=X(c); I w=128; C bck[w];
        switch(r->t){
          CS(1, {n=snprintf(bck,w,"%lld",*kI(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
          CS(2, {n=snprintf(bck,w,"%f",*kF(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
          CS(3, {n=snprintf(bck,w,"%s",kC(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
          CS(-3,{n=snprintf(bck,w,"%s",kC(r)); if(n>=w){bck[w-4]=bck[w-3]=bck[w-2]='.';}})
          default:{n=snprintf(bck,w,"%s","NYI: .m.h result of that type and count"); if(n>=w)R WE;} }
        send(i,bck,strlen(bck),0); bx[0]='\0'; by[0]='\0'; close_tape(i); R (K)0; } } }
  I c=CP[i].r, m=sizeof(M1),g; K z=0;
  S b = c<m?c+(S)&CP[i].m1:c+kC(CP[i].k); 
  g = c<m?m-c:CP[i].m1.n; 
  nbytes = recv(i,b,g,0); 
  if(nbytes <= 0) {
    if (nbytes == 0);//printf("server: socket %lld hung up\n", i);
    else perror("recv");
    GC; }
  //fill struct data + k data
  CP[i].r += nbytes; //DO(nbytes, O("b%lld : %o\n",i,(UC)b[i])) 
  if(m == CP[i].r) { //We've read enough bytes to fill our struct m1 with transmission data (it's also the _db header)
    //TODO: so that we get the right sizes, etc, in the M1, rearrange bytes based on little-endianness indicator CP[i].m1.a
    //if(sizeof(M1)+CP[i].m1.n > 987654321) GC; //protect against too big?
    K k = newK(-3, m+CP[i].m1.n);
    if(!(CP[i].k=k))GC;
    memcpy(kC(k),&CP[i].m1,m); } //cpy data from our struct to the corresponding spot on the '_bd' object
  if(CP[i].r == m + CP[i].m1.n) {  //the k for the _db is completed. perform modified execution, potentially respond
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
    cd(z); z=0; }
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

#else

K read_tape(I i, I j,I type) // type in {0,1} -> {select loop, 4: resp reader}
{   // i:which connection pool item    j:which Socket
  i=1;
    // O("ENTER read_tape\n");
  I c=CP[i].r, m=sizeof(M1),g; K z=0;
    //O("i:%lld  j:%lld  FD_SETSIZE:%d  c:%lld  m:%lld\n",i,j,FD_SETSIZE,c,m);
  S b = c<m?c+(S)&CP[i].m1:c+kC(CP[i].k);
  g = c<m?m-c:CP[i].m1.n;
  I nbytes = recv(j,b,g,0);             // BLOCKS
    //O("buflen g:%lld  nbytes:%lld\n",g,nbytes);  
  if(nbytes > 0) ;     //O("kn.c (in read_tape): Bytes received: %d\n", nbytes);
  else if(nbytes == 0) O("Connection closing...\n");
  else { 
    //O("recv failed\n"); 
    GC; 
  }

  if(nbytes <= 0) {
    if (nbytes == 0) O("server: socket %lld hung up\n", i);
    else perror("recv");
    GC;
  }
  //fill struct data + k data
  CP[i].r += nbytes; //DO(nbytes, O("b%lld : %o\n",i,(UC)b[i])) 
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
    if(z) if(1==msg_type && 0==type) ksender(j,z,2);
    cd(z); z=0;
  }
  R z;
cleanup:
    //close_tape(i);  //causes error in Windows ... probably on close()
  R (K)-1;
}

Z K modified_execute(K x) //TODO: consider: this should be modified to use error trap. _4d should be modified to expect error trap output. 
{
  //K-Lite manual gives {:[4:x; .x; .[.;x]} as processing function
  int status;
  K a;
  status = pthread_mutex_lock(&execute_mutex); 
  if(status != 0) {perror("Lock mutex in mod_ex()"); abort();}
  if(4==xt || 3==ABS(xt)) {
    a = X(CSK(x));
    status = pthread_mutex_unlock(&execute_mutex); 
    if(status != 0) {perror("Unlock mutex in mod_ex()"); abort();}
    R a;
  }
  if(!xt && xn>0) {
    a = vf_ex(offsetDot,x);
    status = pthread_mutex_unlock(&execute_mutex); 
    if(status != 0) {perror("Unlock mutex in mod_ex()"); abort();}
    R a;
  }
  R ci(x);
}

#endif
