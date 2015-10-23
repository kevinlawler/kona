/* networking */

#include "incs.h"
#include "k.h"
#include "km.h"
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
K KONA_WHO,KONA_PORT;

void nfinish()
{
#ifdef WIN32
  extern I listener;
  if (IPC_PORT || HTTP_PORT)
    closesocket(listener);
  WSACleanup();
#endif
}

I ninit()
{
  static I _done = 0;
  
  if (!_done) {
#ifdef WIN32
    WSADATA wsaData;
    int err = WSAStartup(MAKEWORD(2,2), &wsaData);
    if(err != 0) O("WSAStartup failed with error: %d\n",err);
    if(LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2)
      { O("Could not find useable version of Winsock.dll\n"); exit(1); }
    atexit(nfinish);
#endif    
    _done = 1;
  }
  R _done;
}


void *get_in_addr(struct sockaddr *sa) {   //get sockaddr, IPv4 or IPv6
  if (sa->sa_family == AF_INET) R &(((struct sockaddr_in*)sa)->sin_addr);
  R  &(((struct sockaddr_in6*)sa)->sin6_addr); }

I wipe_tape(I i) { if(CP[i].k)cd(CP[i].k); memset(&CP[i],0,sizeof(CP[0])); R 0;} //safe to call >1 time
Z I close_tape(I i) {
  wipe_tape(i); I r=close(i); if(r)show(kerr("file")); FD_CLR(i, &master);
  K x=*denameS(".",".m.c",0); 
  if(6==xt)R O("ct-D\n"),0;
  if(3!=ABS(xt))R O("type error"),1;
  *kI(KONA_WHO)=i;
  KX(x); cd(x);
  *kI(KONA_WHO)=0;
  R 0; }

C bx[128]={0},by[128]={0};

Z K modified_execute(K x) //TODO: consider: this should be modified to use error trap. _4d should be modified to expect error trap output. 
{
  //K-Lite manual gives {:[4:x; .x; .[.;x]} as processing function
#ifdef WIN32
  I status = pthread_mutex_lock(&execute_mutex); 
  if(status != 0) {perror("Lock mutex in mod_ex()"); abort();}
#endif

  K a=(K)-1;
  if(4==xt || 3==ABS(xt)) a=X(CSK(x));
  if(!xt && xn>0) a=vf_ex(offsetDot,x);
  
  if((K)-1!=a){
#ifdef WIN32
    status = pthread_mutex_unlock(&execute_mutex); 
    if(status != 0) {perror("Unlock mutex in mod_ex()"); abort();}
#endif
    R a;
  }
  R ci(x);
}

#if 1

K read_tape(I i, I j, I type) {   // type in {0,1} -> {select loop, 4: resp reader}
  I nbytes=0,n=0; C bz[128]={0},bn[1]={0};
  if(HTTP_PORT) {
    if(bx[0]=='\0')nbytes=recv(j,bx,128,0);
    else if(by[0]=='\0')nbytes=recv(j,by,128,0);
    else nbytes=recv(j,bz,128,0);
    if(nbytes<=0){if (nbytes==0)printf("server: socket %lld hung up\n", j); else perror("recv"); GC;}
    K h=*denameS(".",".m.h",0); 
    if(6==h->t){send(j,bx,nbytes,0); bx[0]='\0'; close_tape(j); R (K)0;}  //echo back only if .m.h does not exist
    else {
      if(7!=h->t && 3!=h->n) {
        I n=snprintf(bx,128,"%s",".m.h is not type 7-3"); if(n>=128)R WE;
        send(j,bx,strlen(bx),0); bx[0]='\0'; close_tape(j); R (K)0;}
      else {   //have .m.h of type 7-3
        S f=kC(kK(h)[CODE]); I ax=0,ay=0,az=0,sf=strlen(f);
        DO(sf, if(f[i]=='x')ax=1; else if(f[i]=='y')ay=1; else if(f[i]=='z')az=1;) I na=maX(1,ax+ay+az);
        if(na==3) {
          if(bz[0]=='\0'){ send(j,bn,1,0); close_tape(j); R (K)0; }
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
            send(j,bck,strlen(bck),0); bx[0]='\0'; by[0]='\0'; close_tape(j); R (K)0; } }
        if(na==2) {
          if(by[0]=='\0'){ send(j,bn,1,0); close_tape(j); R (K)0; }
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
            send(j,bck,strlen(bck),0); bx[0]='\0'; by[0]='\0'; close_tape(j); R (K)0; } }
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
        send(j,bck,strlen(bck),0); bx[0]='\0'; by[0]='\0'; close_tape(j); R (K)0; } } }
  I c=CP[i].r, m=sizeof(M1),g; K z=0;
  S b = c<m?c+(S)&CP[i].m1:c+kC(CP[i].k); 
  g = c<m?m-c:CP[i].m1.n; 
  nbytes = recv(j,b,g,0); 
  if(nbytes <= 0) {
    if (nbytes == 0)O("server: socket %lld hung up\n", j);
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
    wipe_tape(j);

    //blocking read inside 4: receives response //response sent by server to client after a 4: request is not executed by client
    if(2==msg_type && 1==type){
      // (0;x) or (1;"errmsg")
  		if(h->t||2!=h->n)R NE;
  		K s=kK(h)[0],r=kK(h)[1];
  		if(1!=s->t)R NE;
  		if(*kI(s)) {
        if(3!=ABS(r->t))R NE;
  		  r=kerr(kC(r)); }
  		else ci(r);
  		cd(h);
      R r; }

    //Modified execution of received K value. First received transmission in a 3: or 4: 
    K m=_n();
    if(2>msg_type)m=*denameS(".",msg_type?".m.g":".m.s",0);
	  z=(6==m->t)?modified_execute(h):at(m,h);
	  if(msg_type){
  		K u=newK(0,2),s=Ki(0);
  		M(u,s)
  		if(!z){
        *kI(s)=1;
  		  z=newK(-3,strlen(errmsg));
  		  M(u,z);
  		  strcpy(kC(z),errmsg);
  		  kerr("undescribed"); }
      kK(u)[0]=s;kK(u)[1]=z;z=u; }
    else if(!z){
      O("%s error\n",errmsg);
      kerr("undescribed"); }
    cd(m); cd(h);
    //indicates received communication from 4: synchronous method which expects response
    if(z) if(1==msg_type && 0==type) ksender(j,z,2);
    cd(z); z=0; }
  R z;
cleanup:
  close_tape(j);
  R (K)-1;
}

#else

K Xread_tape(I i, I j,I type) { //type in {0,1} -> {select loop, 4: resp reader}
                               //i:which connection pool item    j:which Socket
  i=1;
  I nbytes=0,n=0; C bz[128]={0},bn[1]={0};
  if(HTTP_PORT) {
    if(bx[0]=='\0') nbytes=recv(j,bx,128,0);
    else if(by[0]=='\0') nbytes=recv(j,by,128,0);
    else nbytes=recv(j,bz,128,0);
    if(nbytes<=0){if (nbytes==0)printf("server: socket %lld hung up\n", j); else perror("recv"); GC;}
    K h=*denameS(".",".m.h",0); 
    if(6==h->t){ send(j,bx,nbytes,0); bx[0]='\0'; R (K)0; }  //echo back only if .m.h does not exist
    else {
      if(7!=h->t && 3!=h->n) {
        I n=snprintf(bx,128,"%s",".m.h is not type 7-3"); if(n>=128)R WE;
        send(j,bx,strlen(bx),0); bx[0]='\0'; R (K)0;}
      else {   //have .m.h of type 7-3
        S f=kC(kK(h)[CODE]); I ax=0,ay=0,az=0,sf=strlen(f);
        DO(sf, if(f[i]=='x')ax=1; else if(f[i]=='y')ay=1; else if(f[i]=='z')az=1;) I na=maX(1,ax+ay+az);
        if(na==3) {
          if(bz[0]=='\0'){ send(j,bn,1,0); R (K)0; }
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
            send(j,bck,strlen(bck),0); bx[0]='\0'; by[0]='\0'; R (K)0; } }
        if(na==2) {
          if(by[0]=='\0'){ send(j,bn,1,0); R (K)0; }
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
            send(j,bck,strlen(bck),0); bx[0]='\0'; by[0]='\0'; R (K)0; } }
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
        send(j,bck,strlen(bck),0); bx[0]='\0'; by[0]='\0'; R (K)0; } } }
  I c=CP[i].r, m=sizeof(M1),g; K z=0;
  S b = c<m?c+(S)&CP[i].m1:c+kC(CP[i].k);
  g = c<m?m-c:CP[i].m1.n;
  nbytes = recv(j,b,g,0);             // BLOCKS  
  if(nbytes > 0) ;     //O("kn.c (in read_tape): Bytes received: %d\n", nbytes);
  else if(nbytes == 0) O("Connection closing...\n");
  else GC;     //recv failed

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
  R (K)-1;
}

#endif
