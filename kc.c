/* console input, main loop */

#include "incs.h"

#include <netinet/tcp.h> //#include <sys/socket.h> //#include <netinet/in.h>

#if defined(__OpenBSD__) || defined(__FreeBSD__)
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include <signal.h>

#include "k.h"
#include "kc.h"

Z I randomBits();
Z I wdss(K *a,FILE *f);

I interrupted = 0;

Z void handle_SIGINT(I sig) { interrupted = 1; }


//0: not a verb pointer, 1: monadic, 2: dyadic, 3: triadic
I vn_ct, vm_ct, vd_ct, vt_ct;
I adverb_ct;

I prompt(I n){DO(n,O(">")) O("  ");fflush(stdout);R 0;}

I wds(K* a,FILE*f){R wds_(a,f,0);}
I wds_(K*a,FILE*f,I l)
{
  S s=0,t=0; I b=0,c=0,m=0,n=0,v=0;
  K z=0; PDA p=0;
  I o=isatty(STDIN)&&f==stdin;
  if(-1==(c=getline_(&s,&n,f)))GC;
  appender(&t,&m,s,n);
  while(1==(v=complete(t,m,&p,0)))
  { b=parsedepth(p);
    if(o)prompt(b+l);
    if(-1==(c=getline_(&s,&n,f)))GC;
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

Z I wdss(K*a,FILE*f)
{
  I c=0,n=0;
  K k=0,z=newK(0,0);
  while(0<(c=wds(&k,f))){kap(&z,k); cd(k); n++;}
  *a=z;
  R c==-1?n:c;
}

I lines(FILE*f) {S a=0;I n=0;PDA p=0; while(-1!=line(f,&a,&n,&p));R 0;}//You could put lines(stdin) in main() to have not-multiplexed command-line-only input
I line(FILE*f, S*a, I*n, PDA*p) // just starting or just executed: *a=*n=*p=0,  intermediate is non-zero
{
  S s=0; I b=0,c=0,m=0;
  K k; F d;

  I o = isatty(STDIN) && f==stdin; //display results to stdout?

  if(-1==(c=getline(&s,&m,f))) GC;
  appender(a,n,s,c);//"strcat"(a,s)
  I v=complete(*a,*n,p,0); //will allocate if p is null
  b=parsedepth(*p);
  if(v==3){show(kerr("nest")); GC;} 
  if(v==2){show(kerr("unmatched")); GC;}
  if(v==1) goto done;//generally incomplete
  if(n && '\n'==(*a)[*n-1])(*a)[--*n]=0; //chop for getline
  RTIME(d,k=ex(wd(*a,*n)))
#ifdef DEBUG
  if(o&&k)O("Elapsed: %.7f\n",d);
#endif
  if(o)show(k);
  cd(k);
cleanup:
  if(*p)pdafree(*p);*p=0;
  if(*a)free(*a);*a=0;*n=0;
  if(s)free(s);s=0;
done:
  if(o)prompt(b); 
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
  sa.sa_mask = SIG_DFL;
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
#if defined(__MACH__) && defined(__APPLE__) || defined(__FreeBSD__)
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
    read_fds = master; // copy it 
    if (-1==select(fdmax+1,&read_fds,0,0,0)) //null timeval -> select blocks
    {
      if (errno == EINTR) errno = 0;   //ignore, was interrupted by C-c
      else {perror("select");exit(4);}
    }
    // run through the existing connections looking for data to read 
    for(i = 0; i <= fdmax; i++) 
      if (FD_ISSET(i, &read_fds))
        if(i==STDIN)
        {
          nbytes=line(stdin,&a,&n,&q);
          if(nbytes<=0)
            if(!PORT) exit(0); //Catch CTRL+D 
            else FD_CLR(i,&master); 
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

I args(int n,S*v)
{
  I c;
  while(-1!=(c=getopt(n,v,":h:i:")))SW(c)
  {
    CS('h',O("%d\n", atoi(optarg)))
    CS('i',PORT=optarg)
    CSR(':',)CS('?', O("%c ",optopt); show(kerr("opt")))
  }
  while(optind < n) load(v[optind++]);
  R 0;
}

K KFIXED;

I kinit() //oom (return bad)
{
  atexit(finally);
  I i;
#define SETLEN(x) {for(i=0; x[i]; i++)  x##ct = i+1; }
  SETLEN(vn_); SETLEN(vm_); SETLEN(vd_); SETLEN(vt_);
  for(i=0; adverbs[i]; i++) adverb_ct=i+1;  //adverbs not adverb_, make consistent?
  addressSSR  = vt_ + 0;
  addressWhat = vd+charpos(vc,'?'); addressAt    = vd+charpos(vc,'@');
  addressDot  = vd+charpos(vc,'.'); addressColon = vd+charpos(vc,':');
  kerr("undescribed");//initialize errmsg string to be non-null for more useful reporting
  SYMBOLS=newN(); //Initialize intern pool 
  seedPRNG(randomBits()); 
  KFIXED=newK(0,0); kap(&KFIXED,NIL=Kn());cd(NIL);
  __d = sp(".k"); LS=sp(""); DO(3,IFP[i]=sp(IFS[i]))
#ifdef DEBUG
  test();
#endif
  KTREE=Kd();//Initalize. Alt, KTREE=_(.,(`k;));
  K x;
  kap(&KTREE,x=newEntry(sp("k"))); cd(x);
  kap(&KTREE,x=newE(sp("t"),_dot_t())); cd(x);

  R 0;
}

Z I randomBits(){I s;I f=open("/dev/urandom",0);read(f,&s,sizeof(s));close(f);R s;} //lfop
void seedPRNG(I s){SEED=s?s:randomBits(); init_genrand64(SEED);}
