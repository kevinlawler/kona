/* console input, main loop */

#include "incs.h"
#include "getline.h"
#include <signal.h>
#include "k.h"
#include "kc.h"

#if defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__ANDROID__)
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#ifndef WIN32
#include <netinet/tcp.h>
#include <pthread.h>
#ifndef	__FreeBSD__
#ifndef PTHREAD_MUTEX_RECURSIVE
#define PTHREAD_MUTEX_RECURSIVE PTHREAD_MUTEX_RECURSIVE_NP
#endif
#endif
#else
extern void win_usleep(unsigned int); //buggy TDMGCC64 usleep()
#include <sys/types.h>
#include <pthread.h>
;    // Need semicolon, probably missing from <pthread.h>.
#endif

I feci=0; //Flag error cast as integer

K KONA_GSET=0,KONA_IDX=0;

Z I randomBits();
I oerr(){R O("%s %s\n",errmsg,"error");}

volatile sig_atomic_t interrupted=0;
I scrLim=0;           //script load limit
I fCheck=0;
I fCmplt=0;
I fbr=0;              //flag brace or bracket
I fbs=0;              //backslash flag

I prompt(I n){DO(n,O(">")) O("  ");fflush(stdout);R 0;}

I wds(K* a,FILE*f){R wds_(a,f,0);}
I wds_(K*a,FILE*f,I l) {
  S s=0,t=0;  I b=0,c=0,m=0,n=0,v=0;  K z=0; PDA p=0;
  I o=isatty(STDIN)&&f==stdin;
  if(-1==(c=getline_(&s,&n,f)))GC;
  appender(&t,&m,s,n);
  while(1==(v=complete(t,m,&p,0))) {
    b=parsedepth(p);
    if(o)prompt(b+l);
    if(-1==(c=getline_(&s,&n,f)))GC;
    appender(&t,&m,s,n); }
  SW(v){CS(2,show(kerr("unmatched"));GC) CS(3,show(kerr("nest")); GC)}
  z=newK(-3,m-1);
  strncpy(kC(z),t,m-1);
 cleanup:
  free(s);
  free(t);
  if(p)pdafree(p);
  if((v||c==-1)&&z){cd(z); *a=0;}
  else *a=z;
  R v?-v:c; }    // -1 EOF, -2 unmatched, -3 nest

K KONA_ARGS; //saved values from argv[1:]

Z void multihomeini(S*x)
{
  Z C port[64+1];
  S s=*x;if(!s)R;
  S p=strchr(s,':');if(!p)R;
  strcpy(port,p+1);
  HOST_IFACE=spn(s,p-s);*x=port;
  K h=Ks(HOST_IFACE);
  cd(KONA_CLIENT);KONA_CLIENT=_host(h);cd(h);
}

I args(int n,S*v) {
  K a,k; I c,len,b=1; U(KONA_ARGS=newK(0, n))
  DO(n, len=strlen(v[i]);
        if(!(a=newK(-3, len))){cd(KONA_ARGS);R 0;}
        strncpy(kC(a),v[i],len);
        kK(KONA_ARGS)[i]=a )
  while(-1!=(c=getopt(n,v,":b:h:i:e:x:")))SW(c) {
    CS('h',  if(IPC_PORT)O("-i accepted, cannot also have -h\n"); else HTTP_PORT=optarg;)
    CS('i',  if(HTTP_PORT)O("-h accepted, cannot also have -i\n"); else {IPC_PORT=optarg;*kI(KONA_PORT)=atol(IPC_PORT);})
    CS('b',  b=0;)
    CS('e',  cd(X(optarg)); exit(0) )
    CS('x',  k=X(optarg); printAtDepth(0,k,0,0,0,0); O("\n"); cd(k); exit(0) )
    CSR(':', )
    CS('?',  O("%c\nabort",optopt); exit(0)) }
  if(b)boilerplate();
  multihomeini(IPC_PORT?&IPC_PORT:&HTTP_PORT);
  S h=getenv("KINIT");if(h) load(h);
  while(optind < n) load(v[optind++]);
  R 0; }

K KFIXED;

pthread_mutex_t execute_mutex;

C khome[PATH_MAX+1];
Z void khinit()
{
  I n;S h;
  khome[0]=0;
  if((h=getenv("KHOME"))){
    n=strlen(h);if(n+1>PATH_MAX)R;//doesn't fit
    strcpy(khome,h);strcpy(khome+n,"/");
  }else if((h=getenv("HOME"))){
    n=strlen(h);if(n+3>PATH_MAX)R;
    strcpy(khome,h);strcpy(khome+n,"/k/");
  }
}

I kinit() {       //oom (return bad)
  atexit(finally);

  #ifndef WIN32
  PG = sysconf(_SC_PAGE_SIZE);
  #else
  SYSTEM_INFO si; GetSystemInfo(&si); PG = si.dwPageSize;
  #endif

  if(PG&(PG-1)){er(Pagesize not power of 2); exit(1);}

  ninit();

  pthread_mutexattr_t mta;
  pthread_mutexattr_init(&mta);
  pthread_mutexattr_settype(&mta, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&execute_mutex, &mta);
  pthread_mutexattr_destroy(&mta);

  DT_SIZE                 = DT_OFFSET(TABLE_END);
  DT_END_OFFSET           = DT_OFFSET(end);
  DT_ADVERB_OFFSET        = DT_OFFSET(over);
  DT_VERB_OFFSET          = DT_OFFSET(flip);
  DT_SPECIAL_VERB_OFFSET  = DT_OFFSET(_0m);

  offsetOver      = DT_OFFSET(over);
  offsetScan      = DT_OFFSET(scan);
  offsetEach      = DT_OFFSET(each);
  offsetEachright = DT_OFFSET(eachright);
  offsetEachleft  = DT_OFFSET(eachleft);
  offsetEachpair  = DT_OFFSET(eachpair);

  //could probably delete these variables and create func if(x<DT_SIZE) && DT[x].func == what
  offsetWhat  = (V)DT_OFFSET(what); //equiv: DT_VERB_OFFSET+1+2*charpos(vc,'?');
  offsetAt    = (V)DT_OFFSET(at);
  offsetDot   = (V)DT_OFFSET(dot);
  offsetColon = (V)DT_OFFSET(colon_dyadic);
  offsetJoin  = (V)DT_OFFSET(join);
  offsetSSR   = (V)DT_OFFSET(_ssr);
  offset3m    = (V)DT_OFFSET(_3m);

  kerr("(nil)");
  SYMBOLS=newN(); //Initialize intern pool
  seedPRNG(-271828/*randomBits()*/);
  NIL=Kn();
  KFIXED=newK(0,0); kap(&KFIXED,&NIL);cd(NIL);
  d_ = sp(".k"); LS=sp(""); DO(3,IFP[i]=sp(IFS[i]))

  #ifdef DEBUG
  test();
  #endif

  KTREE=Kd();//Initalize. Alt, KTREE=_(.,(`k;));
  K x=newEntry(sp("k"));
  kap(&KTREE,&x); cd(x);
  x=newE(sp("t"),_dot_t());
  kap(&KTREE,&x); cd(x);
  KONA_WHO=newK(1,1);*kI(KONA_WHO)=0;
  KONA_PORT=newK(1,1);*kI(KONA_PORT)=0;
  KONA_GSET=_n();
  KONA_IDX=_n();
  KONA_CLIENT=_host(_h());
  khinit();
  R 0; }

Z I randomBits(){
  I s;I f=open("/dev/urandom",0);
  I r=read(f,&s,sizeof(s)); if(!r)show(kerr("read"));
  r=close(f); if(r)show(kerr("file")); R s; }

void seedPRNG(I s){SEED=s?s:randomBits(); init_genrand64(SEED);}

Z I nodeCount_(N n) {
  I l=0, r=0;
  if(n->k){ if(strlen((S)n->k)) O("%s ",(S)n->k); else O("(nil) "); }
  if(n->c[0]) l += nodeCount_(n->c[0]);
  if(n->c[1]) r += nodeCount_(n->c[1]);
  R 1+l+r; }
Z I nodeCount(N n) { R nodeCount_(n)-1; }

S recur(S s) {
  I sl=strlen(s); I f=0,i,j,k,p,q,t,c=1;
  for(i=1;i<sl-1;i++){if(s[i]==':' && s[i+1]=='{' && (isalnum(s[i-1]) || s[i-1]==' '))
    {f=1; break;} } //find begin :{ which is i
  if(!f) R NULL;
  for(j=i-1;j>=0;j--){ if(!isalnum(s[j])) break; } //find begin-name, which is j+1
  if(isdigit(s[j+1])) R NULL;
  for(k=i+2;k<sl;k++){ if(s[k]=='{')c++; if(s[k]=='}')c--; if(!c)break; } //find end-} which is k
  I n=1+(i-1)-(j+1); char nm[n+1]; strncpy(nm, s+i-n, n); nm[n]='\0'; //n is strlen(nm)
  I m=k-i-2; char st[m+1]; strncpy(st, s+i+2, m); st[m]='\0'; //m:strlen(st), st:string within outer braces
  f=0; for(p=0;p<strlen(st);p++)if(st[p]=='{'){f=1; break;}  //check for inner braces
  if(f){for(q=strlen(st)-1;q>0;q--)if(st[q]=='}')break; for(t=p+1;t<q;t++)st[t]=' ';}  //blank out inner brace
  S rem=strstr(st,nm);  //remainder of st beginning with nm (if it exists)
  if(rem && ('['==*(rem+strlen(nm)) ||' '==*(rem+strlen(nm)))) {
    I offset=rem-st; C prior=*(s+i+2+offset-1); S res;  //prior is character before rem in s
    if('_'!=prior && !isalnum(prior) && '"'!=prior) {
      res=alloc(1+sl+(2-n)); I ii,beg=k-strlen(rem);
      for(ii=0;ii<beg;ii++){res[ii]=s[ii];}
      res[beg]='_'; res[beg+1]='f';
      for(ii=n;ii<strlen(rem);ii++){res[beg+ii+2-n]=rem[ii];}
      for(ii=k;ii<sl+1;ii++){res[ii+2-n]=s[ii];}
      R res;}
    else R NULL; }
  else R NULL; }

Z void trim(S s){    //remove leading blanks
  I i,j;
  for(i=0;i<strlen(s);++i) { if(s[i]!=' ') break; }
  if(i){ for(j=0;j<1+strlen(s);++j) {s[j]=s[j+i];} } }

/*
Z void trim(S s) {
  // initial version of trim: also removed duplicate blanks (caused problems with quoted strings)
  // it was an attempt to avoid segfaults in corner cases when manipulating input line with recur()
  I b=0,c=0,d=0,f=0;
  for(f=0;f<1+strlen(s);f++){if(s[f]!=' ') break;}
  for(d=f;d<1+strlen(s);d++){
    if(s[d]!=' '){s[c]=s[d]; c++; b=0;}
    else if(!b) {s[c]=s[d]; c++; b=1;}
    if(c>2 && (s[c-1]==':' || s[c-1]=='{') && s[c-2]==' ' && s[c-3]!='/'){s[c-2]=s[c-1]; c--;} } }
*/

I check() {      //in suspended execution mode: allows checking of state at time of error
  I ofCheck=fCheck;
  kerr("(nil)"); prompt(++fCheck); S a=0;  I n=0;  PDA q=0;
  for(;;) {
    line(stdin, &a, &n, &q);
    if(fCheck==ofCheck)GC; }
  O("\n");
cleanup:
  fCheck=ofCheck;
  R 0; }

Z I fln=0;
I lines(FILE*f) {
  S a=0;I n=0;PDA p=0; fln=1; while(-1!=line(f,&a,&n,&p)){fln=0;} R 0;}
    //You could put lines(stdin) in main() to have not-multiplexed command-line-only input

I line(FILE*f, S*a, I*n, PDA*p) {  //just starting or just executed: *a=*n=*p=0,  intermediate is non-zero
  S s=0; I b=0,c=0,m=0,o=1; K k; F d; fbr=fer=feci=0; fam=1;

  if(-1==(c=getline_(&s,&m,f))) GC;
  if(fln&&(s[0]=='#' && s[1]=='!')) GC;
  if(s[0]=='\\' && s[1]=='\n') {
    if(!fCheck&&fLoad) { c=-1; GC; }   //escape file load
    if(fCheck) { fCheck--;R 0; }   //escape suspended execution with single backslash
    if(*a) GC; }                    //escape continue with single backslash
  appender(a,n,s,c);         //"strcat"(a,s)
  I v=complete(*a,*n,p,0);   //will allocate if p is null
  b=parsedepth(*p);
  if(v==3) { show(kerr("nest")); GC; }
  if(v==2) { show(kerr("unmatched")); b=0; GC; }
  if(v==1) { fCmplt=1; goto done; }         //generally incomplete
  if(v==0) fCmplt=0;
  if(n && '\n'==(*a)[*n-1]) (*a)[--*n]=0;   //chop for getline

  trim(*a); //remove leading blanks
  S newA=recur(*a); if(newA){ free(*a); *a=newA; }  //check & fix 'Named Recursion' (issue #288)
  *n=strlen(*a); //strlen might have been changed in 'trim' or in 'recur'
  if((*a)[0]=='\\')fbs=1; else fbs=0;

  if(pthread_mutex_lock(&execute_mutex)){
    perror("Lock mutex in line()"); abort();}

  RTIME(d,k=ex(wd(*a,*n)))

  if(pthread_mutex_unlock(&execute_mutex)){
    perror("Unlock mutex in line()"); abort();}

  #ifdef DEBUG
    if(o&&k)O("Elapsed: %.7f\n",d);
  #endif

  if(o && fam && !feci)show(k);

  cd(k);
 cleanup:
  if(fCheck && (strlen(s)==0 || s[strlen(s)-1]<0)) exit(0);
  S ptr=0;
  if(!strcmp(errmsg,"value"));
  else if(strcmp(errmsg,"(nil)") && fer!=-1) { oerr(); I ctl=0;
    if(fError){
      if(2==fError)exit(1);
      if(lineA){
        if(fnc){ I cnt=0,i;
          if(strlen(fnc)==1)for(i=0;i<strlen(lineA);i++) { if(lineA[i]==*fnc) cnt++; }
          else for(i=0;i<strlen(lineA)-1;i++) {if(lineA[i]==fnc[0]) if(lineA[i+1]==fnc[1]) {ptr=&lineA[i]; cnt++;}}
          if(cnt==1) { ctl=1; O("%s\n",lineA); if(!ptr)ptr=strchr(lineA,*fnc); DO(ptr-lineA,O(" ")) O("^\n"); }
          if(cnt>1 && fnci && fnci<127) { I num=0;
            for(i=0;i<fnci;i++) { if(fncp[i]==fncp[fnci-1])num++; }
            O("%s\n",lineA); O("at execution instance %lld of \"%s\"\n",num,fnc); }}}
      if(lineB && !ctl && strcmp(lineA,lineB)) {
        if(fnc) { I cnt=0,i; O("%s\n",lineB);
          for(i=0;i<strlen(lineB);i++) { if(lineB[i]==*fnc) cnt++; }
          if(cnt==1) { S ptr=strchr(lineB,*fnc); DO(ptr-lineB,O(" ")) O("^\n"); }
          if(cnt>1 && fnci && fnci<127) { I num=0;
            for(i=0;i<fnci;i++) { if(fncp[i]==fncp[fnci-1])num++; }
            O("at execution instance %lld of %s\n",num,fnc); }}}
      if(lineA || lineB)  check();          //enter suspended execution mode for checking
      if(!lineA && !lineB) O("%s\n",*a); }}
  if(*p)pdafree(*p);
  *p=0;
  free(*a);*a=0;*n=0;
  free(s);s=0;
 done:
  if(fWksp) { O("used now : %lld (%lld %lld)\n",(I)mUsed,(I)mAlloc,(I)mMap);
              O("max used : %lld\n",(I)mMax);
              O("symbols  : "); I cnt=nodeCount(SYMBOLS); O("\n");
              O("count    : %lld\n",cnt); fWksp=0; }
  if(o && !fLoad)prompt(b+fCheck);
  kerr("(nil)"); fll=fer=fer1=fnci=fom=feci=0; fnc=lineA=lineB=0; if(cls){cd(cls);cls=0;}
  R c; }

I tmr_ival=0;
V timer_thread(V arg)
{
  for(;;){
    if(tmr_ival){
      K a=_n(),h=*denameS(".",".m.ts",0),z=0;
      if(6!=h->t){
        if(pthread_mutex_lock(&execute_mutex)){
          perror("Lock mutex in timer_thread())"); abort();}
        z=at(h,a);
        if(pthread_mutex_unlock(&execute_mutex)){
          perror("Unlock mutex in timer_thread())"); abort();}
      }
      if(z)cd(z);
      cd(a);
    }
#ifdef WIN32
    win_usleep(tmr_ival?1000*tmr_ival:10000);
#else
    usleep(tmr_ival?1000*tmr_ival:10000);
#endif
  }
  R 0;
}

#ifndef WIN32

Z void handle_SIGINT(int sig) { interrupted = 1; }

fd_set master; //select framework after beej's public domain c
I attend() {  //K3.2 uses fcntl somewhere
  fer=0;
  S a=0;I n=0; PDA q=0; //command-line processing variables

  fd_set read_fds;
  int fdmax=STDIN;
  int listener=0;
  int newfd; //newly accepted socket descriptor
  struct sockaddr_storage remoteaddr; // client address
  socklen_t addrlen;

  int nbytes;
  //char remoteIP[INET6_ADDRSTRLEN];
  I yes=1;  // for setsockopt() SO_REUSEADDR, below
  int i, rv;
  struct addrinfo hints, *ai, *p;
  FD_ZERO(&master); // clear the master and temp sets
  FD_ZERO(&read_fds);

  // set up SIGINT handler, so C-c can break infinite loops cleanly
  struct sigaction sa;
  sa.sa_handler = handle_SIGINT;
  sa.sa_flags = SA_RESTART;
  sigemptyset(&sa.sa_mask);
  I res=sigaction(SIGINT, &sa, NULL); if(res){show(kerr("sigaction")); R -1;}

  // get us a socket and bind it
  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;

  FD_SET(STDIN, &master);

  //TODO: do we need SO_KEEPALIVE or SO_LINGER

  if(IPC_PORT || HTTP_PORT) {
    if((rv=getaddrinfo(HOST_IFACE, IPC_PORT?IPC_PORT:HTTP_PORT, &hints, &ai))) {fprintf(stderr, "server: %s\n", gai_strerror(rv)); exit(1);}
    for(p = ai; p != NULL; p = p->ai_next) {
      listener = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
      if (listener < 0) continue;
      // lose the "address already in use" error message

      #if defined(__MACH__) && defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__)
      setsockopt(listener, SOL_SOCKET, SO_REUSEADDR | SO_NOSIGPIPE , &yes, sizeof(I));
      #endif

      if (bind(listener, p->ai_addr, p->ai_addrlen) < 0){
        if(close(listener))show(kerr("file"));
        continue; }
      break; }
    //K3.2 k aborts/exits if port is in use. k -i 1234. OK.  k -i 1234 ->  "i\nabort\n" exit;
    if (!p) { fprintf(stderr, "server: failed to bind\n"); exit(2); }
    freeaddrinfo(ai);
    if (-1==listen(listener, 10)) { perror("listen"); exit(3); }
    FD_SET(listener, &master);
    fdmax = listener; }

  pthread_t thread;
  if(pthread_create(&thread, NULL, timer_thread, NULL)){
    perror("Create timer thread"); abort(); }

  fln=1;
  for(;;) { // main loop
    scrLim = 0;
    read_fds = master; // copy it
    if (-1==select(fdmax+1,&read_fds,0,0,0)) {  //null timeval -> select blocks
      if (errno == EINTR) { interrupted = 0; errno = 0; } //ignore, was interrupted by C-c
      else {perror("select");exit(4);} }

    // run through the existing connections looking for data to read
    for(i = 0; i <= fdmax; i++)
      if (FD_ISSET(i, &read_fds)) {
        if(i==STDIN) {
          nbytes=line(stdin,&a,&n,&q);
          fln=0;
          if(nbytes<=0){
            if(!IPC_PORT && !HTTP_PORT) exit(0); //Catch CTRL+D
            else FD_CLR(i,&master);} }
        else if(i == listener) {         // handle new connections
          addrlen = sizeof remoteaddr;
          newfd = accept(listener, (struct sockaddr *)&remoteaddr, &addrlen);
          if (newfd == -1) perror("accept");
          else  {
            wipe_tape(newfd); //new conn needs this since connections can die without notification (right?)
            FD_SET(newfd, &master); // add to master set
            if (newfd > fdmax) fdmax = newfd;
            setsockopt(newfd, IPPROTO_TCP, TCP_NODELAY, &yes, sizeof(I));
            CP[newfd].a=ntohl(((struct sockaddr_in*)&remoteaddr)->sin_addr.s_addr);
          } }//disable nagle
            //printf("server: new connection from %s on socket %d\n", inet_ntop(remoteaddr.ss_family,
            //        get_in_addr((struct sockaddr*)&remoteaddr), remoteIP, INET6_ADDRSTRLEN), newfd);
        else if(a) continue; //K3.2 blocks if in the middle of processing the command-line (should we sleep here?)
        else read_tape(i,i,0); } } }

#else

int listener=0;

PHANDLER_ROUTINE handle_SIGINT(int sig) {
  finally();
  //no point in setting "interrupted=1", as exit happens anyway.
  _Exit(0); }

fd_set master; int nfds;
void *socket_thread(void *arg) {
  fd_set read_fds;
  FD_ZERO(&master); FD_ZERO(&read_fds);
  int rv, i;

  struct sockaddr_storage remoteaddr; // client address
  socklen_t addrlen;

  // create socket for server
  I yes=1;struct addrinfo *result=NULL, *p=NULL, hints;
  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;

  // resolve local address and port
  if((rv=getaddrinfo(HOST_IFACE, IPC_PORT?IPC_PORT:HTTP_PORT, &hints, &result))){O("server: %s\n", gai_strerror(rv)); exit(1);}

  for(p = result; p != NULL; p = p->ai_next) {
    if(INVALID_SOCKET==(listener=socket(p->ai_family, p->ai_socktype, p->ai_protocol))) continue;
    setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, (char*)&yes, sizeof(I));

    // bind listener socket
    if(SOCKET_ERROR==bind(listener, p->ai_addr, (int)p->ai_addrlen)){
      if(closesocket(listener))show(kerr("file"));
      continue;}
    break; }

  if (!p) { fprintf(stderr, "server: failed to bind\n"); exit(2); }
  freeaddrinfo(result);

  if(SOCKET_ERROR==listen(listener, 10)){O("listen() failed with error: %d\n", WSAGetLastError()); exit(3);}
  else FD_SET(listener, &master);

  SOCKET SockSet[FD_SETSIZE];
  for(i=0;i<10;i++) SockSet[i]=INVALID_SOCKET;
  I nfd=1;   //Count of FDs including listener & clients
  I nca=0;   //Count of most clients ever activated
  I free=0;  //A previously used socket position is now free
  I nxt=0;   //Next socket position to use

  for(;listener;) {   // main loop for Windows clients (sockets)
    read_fds = master;
    i=select(nfd,&read_fds,0,0,0); if(-1==i) O("select error\n");
    if(FD_ISSET(listener, &read_fds)) {
      addrlen = sizeof remoteaddr;
      SockSet[nxt] = accept(listener, (struct sockaddr *)&remoteaddr, &addrlen);
      if(INVALID_SOCKET==SockSet[nxt]){O("accept() failed with %d\n",WSAGetLastError()); exit(4);}
      else {
        wipe_tape(nxt);
        FD_SET(SockSet[nxt], &master); nfd++;
        CP[nxt].a=ntohl(((struct sockaddr_in*)&remoteaddr)->sin_addr.s_addr);
        if(!free) {nca++; nxt=nca;} } }
    else {
      for(i=0; i<nca; i++) {
        if(FD_ISSET(SockSet[i], &read_fds)) {
          if ((K)-1==read_tape(i,SockSet[i],0)) {
            SockSet[i]=INVALID_SOCKET; nfd--; } } } }
    free=0;
    for(i=0; i<nca; i++) {if(INVALID_SOCKET==SockSet[i]) {free=1; nxt=i; break;} } }
  R 0; }

I attend() {
  S a=0;I n=0; PDA q=0; //command-line processing variables

  //set up SIGINT handler, so C-c can break infinite loops cleanly
  SetConsoleCtrlHandler((PHANDLER_ROUTINE)handle_SIGINT, TRUE);

  pthread_t thread;
  if(pthread_create(&thread, NULL, timer_thread, NULL)){
    perror("Create timer thread"); abort(); }

  if(IPC_PORT || HTTP_PORT) {
    if(pthread_create(&thread, NULL, socket_thread, NULL)){
      perror("Create socket thread"); abort();} }

  for(;;) {   // main loop for Windows stdin
    scrLim = 0;
    for(;;) {
      if (-1==line(stdin, &a, &n, &q)) exit(0);
      } }
  R 0; }

#endif
