/* console input, main loop */

#include "incs.h"
#include "getline.h"
#include <signal.h>
#include "k.h"
#include "kc.h"

#if defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__)
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#ifndef WIN32
#include <netinet/tcp.h>
#else
#include <sys/types.h>
#include <pthread.h>
;    // Need semicolon, probably missing from <pthread.h>.
#endif

Z I randomBits();
I oerr(){R O("%s %s\n",errmsg,"error");}

I interrupted=0;;
I scrLim=0;           // script load limit
I fCheck=0;
I fCmplt=0;

I prompt(I n){DO(n,O(">")) O("  ");fflush(stdout);R 0;}

I wds(K* a,FILE*f){R wds_(a,f,0);}
I wds_(K*a,FILE*f,I l) {
  S s=0,t=0;  I b=0,c=0,m=0,n=0,v=0;  K z=0; PDA p=0;
  I o=isatty(STDIN)&&f==stdin;
  if(-1==(c=getline_(&s,(size_t * __restrict__)&n,f)))GC;
  appender(&t,&m,s,n);
  while(1==(v=complete(t,m,&p,0)))
  { b=parsedepth(p);
    if(o)prompt(b+l);
    if(-1==(c=getline_(&s,(size_t * __restrict__)&n,f)))GC;
    appender(&t,&m,s,n);
  }
  SW(v){CS(2,show(kerr("unmatched"));GC) CS(3,show(kerr("nest")); GC)}
  z=newK(-3,m-1);
  strncpy(kC(z),t,m-1);
cleanup:
  if(s)free(s);
  if(t)free(t);
  if(p)pdafree(p);
  if((v||c==-1)&&z){cd(z); *a=0;}
  else *a=z;
  R v?-v:c; // -1 EOF, -2 unmatched, -3 nest
}

K KONA_ARGS; //saved values from argv[1:]

I args(int n,S*v)
{
  K a;
  I c, len;
  U(KONA_ARGS=newK(0, n))
  DO(n, { len=strlen(v[i]); 
          if(!(a=newK(-3, len))){cd(KONA_ARGS);R 0;} 
          strncpy(kC(a),v[i],len); 
          kK(KONA_ARGS)[i]=a;
        }
    );
  while(-1!=(c=getopt(n,v,":h:i:e:x:")))SW(c)
  {
    K k;
    CS('h',O("%d\n", atoi(optarg)))
    CS('i',PORT=optarg)
    CS('e', cd(X(optarg)); exit(0) )
    CS('x', k=X(optarg); printAtDepth(0,k,0,0,0,0); O("\n"); cd(k); exit(0) )
    CSR(':',)CS('?', O("%c ",optopt); show(kerr("opt")))
  }
  while(optind < n) load(v[optind++]);
  R 0;
}

K KFIXED;
I kinit() //oom (return bad)
{
  atexit(finally);
#ifndef WIN32
  PG = sysconf(_SC_PAGE_SIZE);
#else
  SYSTEM_INFO si; GetSystemInfo(&si); PG = si.dwPageSize;
#endif
  if(PG&(PG-1)){er(Pagesize not power of 2); exit(1);}

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
  offsetSSR   = (V)DT_OFFSET(_ssr);

  kerr("undescribed");
  SYMBOLS=newN(); //Initialize intern pool 
  seedPRNG(randomBits()); 
  NIL=Kn();
  KFIXED=newK(0,0); kap(&KFIXED,&NIL);cd(NIL);
  __d = sp(".k"); LS=sp(""); DO(3,IFP[i]=sp(IFS[i]))
#ifdef DEBUG
  test();
#endif
  KTREE=Kd();//Initalize. Alt, KTREE=_(.,(`k;));
  K x=newEntry(sp("k"));
  kap(&KTREE,&x); cd(x);
  x=newE(sp("t"),_dot_t());
  kap(&KTREE,&x); cd(x);
  
  R 0;
}

Z I randomBits(){
  I s;I f=open("/dev/urandom",0);
  I r=read(f,&s,sizeof(s)); if(!r)show(kerr("read"));
  close(f);R s;
}

void seedPRNG(I s){SEED=s?s:randomBits(); init_genrand64(SEED);}

Z I nodeCount_(N n) {
  I l=0, r=0;
  if(n->c[0]) l += nodeCount_(n->c[0]);
  if(n->c[1]) r += nodeCount_(n->c[1]);
  R 1+l+r;
}
Z I nodeCount(N n) {R nodeCount_(n)-1;}

#ifndef WIN32

I check() {      //in suspended execution mode: allows checking of state at time of error
  fCheck=1; kerr("undescribed"); prompt(1); S a=0;  I n=0;  PDA q=0;
  for(;;) { line(stdin, &a, &n, &q); if(fCheck==0)R 0; }
  O("\n"); fCheck=0; 
  R 0;
}

Z void handle_SIGINT(int sig) { interrupted = 1; }

S recur(S s){
  I sl=strlen(s); I f=0,i,j,k,c=1;
  for(i=0;i<sl-1;i++){ if(s[i]==':' && s[i+1]=='{') {f=1; break;} } //find begin :{ which is i
  if(!f) R NULL;
  for(j=i-1;j>=0;j--){ if(!isalnum(s[j])) break; } //find begin-name, which is j+1
  for(k=i+2;k<sl;k++){ if(s[k]=='{')c++; if(s[k]=='}')c--; if(!c)break; } //find end-} which is k
  I n=1+(i-1)-(j+1); char nm[n+1]; strncpy(nm, s+i-n, n); nm[n]='\0'; //n is strlen(nm)
  I m=k-i-2; char st[m+1]; strncpy(st, s+i+2, m); st[m]='\0'; //m is strlen(st)
  S rem=strstr(st,nm); S res;
  if(rem && ':'!=*(rem+strlen(nm))) {
    res=malloc(1+sl+(2-n)); I ii,beg=k-strlen(rem);
    for(ii=0;ii<beg;ii++){res[ii]=s[ii];}
    res[beg]='_'; res[beg+1]='f';
    for(ii=n;ii<strlen(rem);ii++){res[beg+ii+2-n]=rem[ii];}
    for(ii=k;ii<sl+1;ii++){res[ii+2-n]=s[ii];} }
  R res;
}

I lines(FILE*f) {S a=0;I n=0;PDA p=0; while(-1!=line(f,&a,&n,&p)){} R 0;}
    //You could put lines(stdin) in main() to have not-multiplexed command-line-only input
I line(FILE*f, S*a, I*n, PDA*p) // just starting or just executed: *a=*n=*p=0,  intermediate is non-zero
{
  S s=0; I b=0,c=0,m=0;
  K k; F d;

  //I o = isatty(STDIN) && f==stdin; //display results to stdout?
  I o = isatty(STDIN); //display results to stdout?

  if(-1==(c=getline(&s,(size_t * __restrict__)&m,f))) GC;
  if(fCheck==1) { if(s[0]==92 && s[1]==10) { fCheck=0; R 0; }}   //escape suspended exection with single backslash
  appender(a,n,s,c);         //"strcat"(a,s)
  I v=complete(*a,*n,p,0);   //will allocate if p is null
  b=parsedepth(*p);
  if(v==3) { show(kerr("nest")); GC; } 
  if(v==2) { show(kerr("unmatched")); b=0; GC; }
  if(v==1) { fCmplt=1; goto done; }         //generally incomplete
  if(n && '\n'==(*a)[*n-1]) (*a)[--*n]=0;   //chop for getline

  S newA=recur(*a); if(newA){ if(*a)free(*a);*a=0;*n=0; *a=newA; *n=strlen(newA); }

  RTIME(d,k=ex(wd(*a,*n)))
  #ifdef DEBUG
    if(o&&k)O("Elapsed: %.7f\n",d);
  #endif
  if(o)show(k); cd(k);
cleanup:
  if(fCheck && (strlen(s)==0 || s[strlen(s)-1]<0)) exit(0);
  if(strcmp(errmsg,"undescribed")) { oerr(); I ctl=0;
    if(fError) {
      if(lineA) {
        if(fnc) { I cnt=0,i; 
          for(i=0;i<strlen(lineA);i++) { if(lineA[i]==*fnc) cnt++; }
          if(cnt==1) { ctl=1; O("%s\n",lineA); S ptr=strchr(lineA,*fnc); DO(ptr-lineA,O(" ")) O("^\n"); }
          if(cnt>1 && fnci && fnci<127) { I num=0; 
            for(i=0;i<fnci;i++) { if(fncp[i]==fncp[fnci-1])num++; } 
            O("at execution instance %lld of %s\n",num,fnc); }}}
      if(lineB && !ctl && strcmp(lineA,lineB)) {
        if(fnc) { I cnt=0,i; O("%s\n",lineB);
          for(i=0;i<strlen(lineB);i++) { if(lineB[i]==*fnc) cnt++; }
          if(cnt==1) { S ptr=strchr(lineB,*fnc); DO(ptr-lineB,O(" ")) O("^\n"); }
          if(cnt>1 && fnci && fnci<127) { I num=0; 
            for(i=0;i<fnci;i++) { if(fncp[i]==fncp[fnci-1])num++; } 
            O("at execution instance %lld of %s\n",num,fnc); }}}
      if(lineA || lineB)  check();          //enter suspended execution mode for checking
      if(!lineA && !lineB) O("%s\n",*a); }}
  if(*p)pdafree(*p);*p=0;
  if(*a)free(*a);*a=0;*n=0;
  if(s)free(s);s=0;
done:
  if(fWksp) { O("used now : %lld\n",(I)mUsed); O("max used : %lld\n",(I)mMax); O("symbols  : %lld\n",nodeCount(SYMBOLS)); fWksp=0; }
  if(o && !fLoad)prompt(b+fCheck);
  kerr("undescribed"); fer=fnci=0; fnc=lineA=lineB=0;
  R c;
}

fd_set master; //select framework after beej's public domain c
I attend() //K3.2 uses fcntl somewhere
{
  S a=0;I n=0; PDA q=0; //command-line processing variables

  fd_set read_fds;
  int fdmax=STDIN;
  int listener=0;
  int newfd; //newly accepted socket descriptor
  struct sockaddr_storage remoteaddr; // client address
  socklen_t addrlen;

  int nbytes;
  //char remoteIP[INET6_ADDRSTRLEN];
  I yes=1;	// for setsockopt() SO_REUSEADDR, below 
  int i, rv;
  struct addrinfo hints, *ai, *p;
  FD_ZERO(&master);	// clear the master and temp sets
  FD_ZERO(&read_fds);

  // set up SIGINT handler, so C-c can break infinite loops cleanly
  struct sigaction sa;
  sa.sa_handler = handle_SIGINT;
  sa.sa_flags = SA_RESTART;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGINT, &sa, NULL);

  // get us a socket and bind it 
  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC; 
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;

  FD_SET(STDIN, &master);

  //TODO: do we need SO_KEEPALIVE or SO_LINGER

  if(PORT)
  {
    if ((rv = getaddrinfo(NULL, PORT, &hints, &ai)) != 0) { fprintf(stderr, "server: %s\n", gai_strerror(rv)); exit(1); }
    for(p = ai; p != NULL; p = p->ai_next)
    {
      listener = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
      if (listener < 0) continue;
      // lose the "address already in use" error message 
#if defined(__MACH__) && defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__)
      setsockopt(listener, SOL_SOCKET, SO_REUSEADDR | SO_NOSIGPIPE , &yes, sizeof(I)); 
#endif
      if (bind(listener, p->ai_addr, p->ai_addrlen) < 0) { close(listener); continue; }
      break;
    }
    //K3.2 k aborts/exits if port is in use. k -i 1234. OK.  k -i 1234 ->  "i\nabort\n" exit;
    if (p == NULL) { fprintf(stderr, "server: failed to bind\n"); exit(2); } 
    freeaddrinfo(ai); 
    if (listen(listener, 10) == -1) { perror("listen"); exit(3); }
    FD_SET(listener, &master);
    fdmax = listener; 
  }


  for(;;) // main loop  
  {
    scrLim = 0;
    read_fds = master; // copy it 
    if (-1==select(fdmax+1,&read_fds,0,0,0)) //null timeval -> select blocks
    {
      if (errno == EINTR) { interrupted = 0; errno = 0; } //ignore, was interrupted by C-c
      else {perror("select");exit(4);}
    }
    
    // run through the existing connections looking for data to read 
    for(i = 0; i <= fdmax; i++) 
      if (FD_ISSET(i, &read_fds))
      {
        if(i==STDIN)
        {
          nbytes=line(stdin,&a,&n,&q);
          if(nbytes<=0){
            if(!PORT) exit(0); //Catch CTRL+D 
            else FD_CLR(i,&master);} 
        }
        else if(i == listener) // handle new connections 
        {
          addrlen = sizeof remoteaddr; 
          newfd = accept(listener, (struct sockaddr *)&remoteaddr, &addrlen);
          if (newfd == -1) perror("accept"); 
          else
          {
            wipe_tape(newfd); //new conn needs this since connections can die without notification (right?)
            FD_SET(newfd, &master); // add to master set 
            if (newfd > fdmax) fdmax = newfd;
            setsockopt(newfd, IPPROTO_TCP, TCP_NODELAY, &yes, sizeof(I)); //disable nagle
            //printf("server: new connection from %s on socket %d\n", inet_ntop(remoteaddr.ss_family, get_in_addr((struct sockaddr*)&remoteaddr), remoteIP, INET6_ADDRSTRLEN), newfd);
          }
        } 
        else if(a) continue; //K3.2 blocks if in the middle of processing the command-line (should we sleep here?)
        else read_tape(i,0);
      }
  }
}

#else

I check() {
  fCheck=1;
  kerr("undescribed");
  prompt(1);
  char s[300];
  S a=0;  I n=0;  PDA q=0;
  for(;;) {
    fgets(s, sizeof(s), stdin);
    if(s[0]==4) exit(0);
    if(s[0]==92 && s[1]==10) { fCheck=0; R 0; }
    line(s, &a, &n, &q);
  }
  O("\n"); fCheck=0; R 0;
}

PHANDLER_ROUTINE handle_SIGINT(int sig) {
  //no point in setting "interrupted=1", as exit happens anyway. 
  //calling exit(0) explicitly cleans up with finally().
  exit(0);
}

I lines(FILE*f) {
  S a=0;  I n=0;  PDA p=0;  char s[300];
  while(NULL != fgets(s,sizeof(s),f)) line(s,&a,&n,&p);
  R 0;
}

pthread_mutex_t execute_mutex = PTHREAD_MUTEX_INITIALIZER;
I line(S s, S*a, I*n, PDA*p) {  // just starting or just executed: *a=*n=*p=0,  intermediate is non-zero
  I b=0,c=0;  int status;  K k;  F d;
  I o = isatty(STDIN); //display results to stdout?
  appender(a,n,s,c=strlen(s));//"strcat"(a,s)
  I v=complete(*a,*n,p,0); //will allocate if p is null
  b=parsedepth(*p);
  if(v==3){show(kerr("nest")); GC;} 
  if(v==2){show(kerr("unmatched")); b=0; GC;}
  if(v==1){fCmplt=1; goto done;}         //generally incomplete
  if(n && '\n'==(*a)[*n-1])(*a)[--*n]=0; //chop for getline
  status = pthread_mutex_lock(&execute_mutex); 
  if(status != 0) {perror("Lock mutex in line()"); abort();}
  RTIME(d,k=ex(wd(*a,*n)))
  status = pthread_mutex_unlock(&execute_mutex); 
  if(status != 0) {perror("Unlock mutex in line()"); abort();}
#ifdef DEBUG
  if(o&&k)O("Elapsed: %.7f\n",d);
#endif
  if(o)show(k); cd(k);
cleanup:
  if(strcmp(errmsg,"undescribed")) { oerr(); I ctl=0;
    if(fError) {
      if(lineA) {
        if(fnc) { I cnt=0,i; 
          for(i=0;i<strlen(lineA);i++) { if(lineA[i]==*fnc) cnt++; }
          if(cnt==1) { ctl=1; O("%s\n",lineA); S ptr=strchr(lineA,*fnc); DO(ptr-lineA,O(" ")) O("^\n"); }
          if(cnt>1 && fnci && fnci<127) { I num=0; 
            for(i=0;i<fnci;i++) { if(fncp[i]==fncp[fnci-1])num++; } 
            O("at execution instance %lld of %s\n",num,fnc); }}}
      if(lineB && !ctl && strcmp(lineA,lineB)) {
        if(fnc) { I cnt=0,i; O("%s\n",lineB);
          for(i=0;i<strlen(lineB);i++) { if(lineB[i]==*fnc) cnt++; }
          if(cnt==1) { S ptr=strchr(lineB,*fnc); DO(ptr-lineB,O(" ")) O("^\n"); }
          if(cnt>1 && fnci && fnci<127) { I num=0; 
            for(i=0;i<fnci;i++) { if(fncp[i]==fncp[fnci-1])num++; } 
            O("at execution instance %lld of %s\n",num,fnc); }}}
      if(lineA || lineB)  check();          //enter suspended execution mode for checking
      if(!lineA && !lineB) O("%s\n",*a); }}
  if(*p)pdafree(*p);*p=0;
  if(*a)free(*a);*a=0;*n=0;
  if(s)free(s);s=0;
done:
  if(fWksp) { O("used now : %lld\n",(I)mUsed); O("max used : %lld\n",(I)mMax); O("symbols  : %lld\n",nodeCount(SYMBOLS)); fWksp=0; }
  if(o && !fLoad)prompt(b+fCheck);
  kerr("undescribed"); fer=fnci=0; fnc=lineA=lineB=0;
  R c;
}

fd_set master;  int listener=0;  int fds[10],nfds;  
void *socket_thread(void *arg) {
  fd_set read_fds;
  FD_ZERO(&master); FD_ZERO(&read_fds);
  int rv, i;

  // initialize WinSock
  WSADATA wsaData;
  int err = WSAStartup(MAKEWORD(2,2), &wsaData);
  if(err != 0) O("WSAStartup failed with error: %d\n",err);
  if(LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2){  //verify
    O("Could not find useable version of Winsock.dll\n"); exit(1);
  }

  // create socket for server
  struct addrinfo *result=NULL, *ptr=NULL, hints; 
  memset(&hints, 0, sizeof hints);
  hints.ai_family =   AF_INET;           //AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = IPPROTO_TCP;
  hints.ai_flags =    AI_PASSIVE;

  // resolve local address and port 
  if ((rv = getaddrinfo(NULL, PORT, &hints, &result)) != 0) {
    O("server: %s\n", gai_strerror(rv)); exit(1); 
  }

  // create listener with socket()
  listener = socket(result->ai_family, result->ai_socktype, result->ai_protocol);
  if(listener == 0) {O("Error at socket(): %ld\n", WSAGetLastError()); exit(1);}

  // bind listener socket
  int bRes = bind(listener, result->ai_addr, (int)result->ai_addrlen);
  if(bRes == SOCKET_ERROR) {
     O("bind failed with error: %d\n", WSAGetLastError());
     freeaddrinfo(result); exit(1);
  }
  if (ptr == NULL)  ;  //eliminates unused variable warning
  freeaddrinfo(result);
    
  if (listen(listener, 10) == SOCKET_ERROR) { 
    O("listen() failed with error: %ld\n", WSAGetLastError()); exit(3); 
  } 
  else FD_SET(listener, &master);

  SOCKET SockSet[10]; 
  for(i=0;i<10;i++) SockSet[i]=INVALID_SOCKET;
  I nfd=1;   //Count of FDs including listener & clients
  I nca=0;   //Count of most clients ever activated
  I free=0;  //A previously used socket position is now free
  I nxt=0;   //Next socket position to use
    
  K z=0;
  for(;;) {   // main loop for Windows clients (sockets)
    read_fds = master;
    i=select(nfd,&read_fds,0,0,0); if(-1==i) O("select error\n");
    if(FD_ISSET(listener, &read_fds)) {
      SockSet[nxt] = accept(listener, NULL, NULL);
      if(SockSet[nxt]==INVALID_SOCKET) exit(4); 
      else {
        FD_SET(SockSet[nxt], &master); nfd++; 
        if(!free) {nca++; nxt=nca;}
      }
    } 
    else {
      for(i=0; i<nca; i++) {
        if(FD_ISSET(SockSet[i], &read_fds)) {
          z=read_tape(i,SockSet[i],0);
          if(z) {
            FD_CLR(SockSet[i], &master); FD_ZERO(&read_fds);
            closesocket(SockSet[i]); SockSet[i]=INVALID_SOCKET;
            wipe_tape(i); nfd--;
          }
        }
      }        
    }
    free=0;
    for(i=0; i<nca; i++) {
      if(SockSet[i]==INVALID_SOCKET) {free=1; nxt=i; break;}
    }
  }   
  R 0;     
}

I attend()
{
  S a=0;I n=0; PDA q=0; //command-line processing variables
  
  //set up SIGINT handler, so C-c can break infinite loops cleanly
  SetConsoleCtrlHandler((PHANDLER_ROUTINE)handle_SIGINT, TRUE);

  if(PORT) {
     int status;
     pthread_t thread;
     status = pthread_create(&thread, NULL, socket_thread, NULL);
     if (status != 0) {perror("Create socket thread"); abort();}
  }
  for(;;) {   // main loop for Windows stdin
    scrLim = 0;  
    char s[300]; 
    for(;;) {
      fgets(s, sizeof(s), stdin);
      if(s[0]==4) exit(0);
      line(s, &a, &n, &q);
    }    
  }
  R 0;
}     

#endif
