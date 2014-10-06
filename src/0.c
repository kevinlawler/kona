#include "incs.h"
#include "getline.h"

#ifndef WIN32
#include <netinet/tcp.h> //#include <sys/socket.h> //#include <netinet/in.h>
#include <dlfcn.h>
#else
#include <unistd.h>
#include "win/dlfcn.h"
#include "win/mman.h"
#endif

#if defined(__OpenBSD__) || defined(__FreeBSD__)  || defined(__NetBSD__)
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include "0.h"
#include "k.h"
#include "km.h"
#include "v.h"
#include "vf.h"

//Number verbs, monadic & dyadic

//TODO: Do the 0:,1:,5:,6: writes need explicit file level locks (two K3.2 instances, second process can't write to same file first is (error))
//TODO: do these speed things up? O_DIRECT O_FSYNC O_NOFOLLOW
//TODO: ftruncate without writing zeroes after can cause file fragmentation? Google mmap fragmentation
//TODO: for monadic/dyadic 0: the full range is " IFCSDTZ" ... "DTZ" are missing

//From K2.9 \Help:
// (type;[,]delim)0:f     [names+]delimited text( IFCSDTZ)
// (type;width)0:f        fixedwidth text( IFCSDTZ)
// (type;width)1:f        fixedwidth data(cbsijfd IFCSDTZMm)
// Blank skips. S strips. f can be (f;index;length).

/* prototypes */
Z K _0d_write(K a,K b);
Z K _0d_read(K a,K b);
Z K _0d_rdDsv(K c,K b);
Z K _0d_rdDsvWc(K a,K b);
Z K _1m_r(I f,V fixed, V v,V aft,I*b);
Z K _1d_char(K x,K y);
Z K _1d_read(K a,K b);
Z K _1d_write(K x,K y);
Z I disk(K x);
Z I rrep_4(S *z,S a,S t);
Z K readVector(K x,I t);
Z I sendall(I s,S b,I k);

Z V freopen_stdin() {
#if defined(__OpenBSD__)
  return freopen("/dev/stdin","r",stdin);
#else
  return freopen(0,"r",stdin);
#endif
}

K _0m(K a)
{
  I t=a->t;
  P(4!=t && 3!=ABS(t), TE)

  I b=0,s=0;
  S v=0;
  K z;
  if(4==t && !**kS(a)){
    b=getdelim_(&v,(size_t * __restrict__)&s,EOF,stdin);
    P(freopen_stdin() == NULL, FE)
    if(b==-1){z=newK(0,0); GC;}
  }
  else
  {
    I f=open(CSK(a),0);
    P(f<0,DOE)
    P(stat_sz(CSK(a),&s),SE)
    if(MAP_FAILED==(v=mmap(0,s,PROT_READ,MAP_SHARED,f,0)))R SE; //Should this be PRIVATE+NO_RESERVE ?
    close(f);
  }
  I c=s?1:0,d=0,e;
  DO(s, if('\n'==v[i] && i < s-1)c++) //1st run: count \n
  K k;
  z=newK(0,c);
  if(!z) GC;
  DO(s, if('\n'!=v[i])kK(z)[d]=(V)1+(L)kK(z)[d]; else d++) //2nd run: count lengths (cheat & use pointers' space)
  DO(c,e=(L)kK(z)[i]; k=newK(-3,e); if(!k){cd(z);z=0;GC;}  kK(z)[i]=k)
  e=0;
  DO(c, k=kK(z)[i]; memcpy(kC(k),v+e,k->n); e+=1+k->n;) //3rd run: populate 

cleanup:
  if(v){if(b)free(v);else munmap(v,s);}
  R z;
}

K _0d(K a,K b) //lfop
{
  I t=a->t;
  if(4==t || 3==ABS(t))R _0d_write(a,b);
  if(!t)R _0d_read(a,b);
  R TE;
}

Z I ok_0dw(K b) //b must be +-3, or 0 containing {+3,-3,()}
{
  I t=b->t,n=b->n; K k;
  if(3!=ABS(t)) {
    if(!t)DO(n, k=kK(b)[i]; if(3!=ABS(k->t) && (t || k->n)) R 0 )
    else R 0;
  }
  R 1;
}

Z K _0d_write(K a,K b) //assumes a->t in {3,-3,4}
{
  I t=b->t, n=b->n;
  K k;

  P(!ok_0dw(b),TE)

  S m=CSK(a);

  I s=0;

  if(3==ABS(t))s=n;
  else DO(n,s+=1+kK(b)[i]->n) //0-list adds newlines
 
  I f=m[0]?open(m,O_RDWR|O_CREAT|O_TRUNC,07777):1; //stdout if m is ` or "" or "\000..." (is O_TRUNC necessary when we have ftruncate below?)
  P(f<0,DOE)

  if(1==f) //write to stdout
  {
    I r;
    if(3==ABS(t)) {r=write(f,kC(b),s); if(!r)show(kerr("write"));} 
      //This is duplicated but I don't see how to factor it right now (choose write/memcpy funcs?)
    else DO(n, k=kK(b)[i]; 
      if(3==ABS(k->t)) {r=write(f,kC(k),k->n); if(!r)show(kerr("write"));} 
      r=write(f,"\n",1); if(!r)show(kerr("write"));)
  }
  else     //write to mmap'd file
  {
    P(ftruncate(f,s),SE)
    //below is from Advanced Programming in the Unix Environment ... kept because windows might need them
    //if (lseek(f,s-1,SEEK_SET) == -1) R 0; //lseek error
    //if (write(f,"",1) != 1) R 0; //write error

    S v;
    if(MAP_FAILED==(v=mmap(0,s,PROT_WRITE,MAP_SHARED,f,0)))R SE; //Should this be MAP_PRIVATE|MAP_NORESERVE?

    close(f);

    I c=0;
    if(3==ABS(t)) memcpy(v,kC(b),s);
    else DO(n, k=kK(b)[i]; if(3==ABS(k->t)){memcpy(v+c,kC(k),k->n); c+=k->n;} v[c++]='\n'; )

    //msync(v,s,MS_SYNC|MS_INVALIDATE); //slow
    munmap(v,s); 
  }

  R _n();
}

Z K _0d_read(K a,K b)   //K3.2 windows crash bug: (s;w) 0: (`f;0;1) where 1 is a bad length for `f
{
  //may assume !a->t
  K z=0;
  I an=a->n, bt=b->t, bn=b->n;
  P(an!=2,DOE)
  K c=kK(a)[0],d=kK(a)[1];
  I cn=c->n, dn=d->n;
  P(3 == d->t, _0d_rdDsv(a,b))
  P(-3 == d->t, _0d_rdDsvWc(a,b))
  P(3 != ABS(c->t) || 1 != ABS(d->t), TE )//(K3.2 strictly requires lists---bad)
  P(!cn || cn != dn, LE)
  P(3 != ABS(bt) && 4 != bt && 0 != bt, TE)

  I fc=0; //field count
  DO(cn,if(' '==kC(c)[i])continue; if(stringHasChar("IFCS",kC(c)[i])) fc++; else R TE)  
  I w=1,x;
  DO(dn, x=kI(d)[i]; P(x<=0, LE) w+=x) 

  K ff=b,k; I fb=0,fn=0; //mostly for bt!=0

  if(!bt)
  {
    P(3!=bn,LE)
    ff=kK(b)[0];
    P(3!=ABS(ff->t) && 4 != ff->t,TE)// (could refactor this {-3,3,4} check )
     k=kK(b)[1];
    P(1!=k->t && 2!=k->t,TE)
    fb=k->t-1?*kF(k):*kI(k);
     k=kK(b)[2];
    P(1!=k->t && 2!=k->t,TE)
    fn=k->t-1?*kF(k):*kI(k);
  }

  I s; P(stat_sz(CSK(ff),&s),SE)
 
  if(bt) fn=s;

  //if(fn<0 || fb+fb>s || fn%w) R 0; //length error. omitted to reduce errors

  if(fn<0) fn=0; //suppress error
  if(fb<0) fb=0; //in K3.2 fb<0 is ok
  fb=MIN(fb,MAX(0,s-1)); 
  if(fb+fn > s) fn = s-fb;

  I f=open(CSK(ff),0);
  P(f<0,DOE)

  S v;
  if(MAP_FAILED==(v=mmap(0,fn,PROT_READ,MAP_SHARED,f,fb)))R SE;
  close(f);

  I r=0,t=0;
  DO(fn, if(v[fb+i]=='\n')if(t==w-1){r++;t=0;}else t=0;else t++)//count valid rows
           
  //if(t || r != fn/w )R 0; //Omitted: K3.2 length error + mm/o

  z=newK(0,fc);
  if(!z) GC;
  I e=0; C g;
  DO(cn, g=kC(c)[i]; if(' '==g)continue; if(!(kK(z)[e++]=newK(-charpos("CIF S",g),r))){cd(z);z=0;GC;}) //0C, -1I, -2F, -4S

  S m;
  I u=0,y,p=0;
  for(;u<=fn-w;u+=t+1,t=0)//u marks start of sometimes valid row
  {
    while(u+t<fn && '\n'!=v[u+t])t++;
    if(t==w-1 && '\n'==v[u+t])
    {
      //read row 
      y=u;
      e=x=0;
      K q=0;
      DO(cn, x=kI(d)[i];
             k=kK(z)[e++]; 
             switch(kC(c)[i])
             {
               CS(' ', e--) 
                                                                                                  //TODO: errors should still unmap
               CS('I', m=strdupn(v+y,x); if(!m)R 0; q=formKiCS(m); kI(k)[p]=q?*kI(q):IN; free(m)) //oom m; q is ok because formKiCS unusual
               CS('F', m=strdupn(v+y,x); if(!m)R 0; q=formKfCS(m); kF(k)[p]=q?*kF(q):FN; free(m)) //oom m; q is ok because formKfCS unusual
               CS('C', q=newK(-3,x);     if(!q)R 0; memcpy(kC(q),v+y,x); kK(k)[p]=q; q=0;) //oom q
               CS('S', m=strdupn(v+y,x); if(!m)R 0; kS(k)[p]=sp(m); free(m);) //oom m
             }
             if(q && q->c<1000000)cd(q);
             y+=x;
         )
      p++;
    }
  }

cleanup:
  munmap(v,s);
  R z;
}

Z K _0d_rdDsv(K a,K b)     // read delim-sep-val-file (no column headings)  (s;",")0:f
{
  K z=0;
  I an=a->n, bt=b->t;
  P(an!=2,DOE)
  K c=kK(a)[0],d=kK(a)[1];
  I cn=c->n;
  P(3 != ABS(bt) && 4 != bt && 0 != bt, TE)
  C*x=kC(d); C w=*x; // delimiter

  I fb=0,fn=0,s; P(stat_sz(CSK(b),&s),SE)
  if(bt) fn=s;

  I f=open(CSK(b),0);
  P(f<0,DOE)
  S v; // fn: file length,  f: fd,  fb: offset
  if(MAP_FAILED==(v=mmap(0,fn,PROT_READ,MAP_SHARED,f,fb))){O("mmap failed\n"); R SE;}
  close(f);

  I fc=0; DO(cn,if(' '==kC(c)[i])continue; if(stringHasChar("IFCS",kC(c)[i])) fc++; else R TE) //field count
  I r=0; DO(fn, if(v[fb+i]=='\n')r++) // row count
  if(v[fn-1]!='\n')r++; // no final line feed

  z=newK(0,fc);
  if(!z) GC;
  I e=0; C g;
  DO(cn, g=kC(c)[i]; if(' '==g)continue; if(!(kK(z)[e++]=newK(-charpos("CIF S",g),r))){cd(z);z=0;GC;}) //0C, -1I, -2F, -4S

  S m; I u=0,t=0,p=0,n=0,h=0; C*tok; C y[2]; y[0]=w; K k;
  for(;u<=fn;u+=t+1,t=0) {
    while(u+t<=fn && '\n'!=v[u+t] && v[u+t]!=(L)NULL)t++;
    if(v[u+t]=='\n' || v[u+t]==(L)NULL) { 
      K q=0; e=h=0;
      m=strdupn(v+u,t); 
      if(!m) R 0;
      if(m[0]!=(L)NULL){
        tok=strtok(m,y);
        k=kK(z)[e++]; 
        switch(kC(c)[h++]) {
          CS(' ', e--)
          CS('I', q=formKiCS(tok); kI(k)[p]=q?*kI(q):IN;)
          CS('F', q=formKfCS(tok); kF(k)[p]=q?*kF(q):FN;)
          CS('C', q=newK(-3,n=strlen(tok)); if(!q)R 0; memcpy(kC(q),tok,n); kK(k)[p]=q; q=0;)
          CS('S', kS(k)[p]=sp(tok);)
	}
        if(q && q->c<1000000 && q->c>0)cd(q);
        while(tok != NULL){
          tok=strtok(NULL,y);
          if(tok!=NULL) {
            k=kK(z)[e++];
            switch(kC(c)[h++]) {
              CS(' ', e--)
              CS('I', q=formKiCS(tok); kI(k)[p]=q?*kI(q):IN;)
              CS('F', q=formKfCS(tok); kF(k)[p]=q?*kF(q):FN;)
              CS('C', q=newK(-3,n=strlen(tok)); if(!q)R 0; memcpy(kC(q),tok,n); kK(k)[p]=q; q=0;)
              CS('S', kS(k)[p]=sp(tok);)
	    }
          }
          if(q && q->c<1000000 && q->c>0)cd(q);
        }
      }
      free(m);
    }
    p++; 
  }

cleanup:
  munmap(v,s);
  R z;
}

Z K _0d_rdDsvWc(K a,K b)     // read delim-sep-val-file-with-columm-headings    (s;,",")0:f
{
  K z=0;
  I an=a->n, bt=b->t;
  P(an!=2,DOE)
  K c=kK(a)[0],d=kK(a)[1];
  I cn=c->n;
  P(3 != ABS(bt) && 4 != bt && 0 != bt, TE)
  C*x=kC(d); C w=*x; // delimiter

  I fb=0,fn=0,s; P(stat_sz(CSK(b),&s),SE)
  if(bt) fn=s;

  I f=open(CSK(b),0);
  P(f<0,DOE)
  S v; // fn: file length,  f: fd,  fb: offset
  if(MAP_FAILED==(v=mmap(0,fn,PROT_READ,MAP_SHARED,f,fb))){O("mmap failed\n"); R SE;}
  close(f);

  I fc=0; DO(cn,if(' '==kC(c)[i])continue; if(stringHasChar("IFCS",kC(c)[i])) fc++; else R TE) //field count
  I r=0; DO(fn, if(v[fb+i]=='\n')r++) // row count
  if(v[fn-1]!='\n')r++; // no final line feed

  z=newK(0,2); if(!z) GC;
  kK(z)[0]=newK(-4,fc); kK(z)[1]=newK(0,fc);
  I e=0; C g;
  DO(cn, g=kC(c)[i]; if(' '==g)continue; if(!(kK(kK(z)[1])[e++]=newK(-charpos("CIF S",g),r-1))){cd(z);z=0;GC;}) //0C, -1I, -2F, -4S

  S m; I u=0,t=0,p=0,n=0,h=0; C*tok; C y[2]; y[0]=w; K k;
  for(;u<=fn;u+=t+1,t=0) {
    while(u+t<=fn && '\n'!=v[u+t] && v[u+t]!=(L)NULL)t++;
    if(0==n++ && (v[u+t]=='\n' || v[u+t]==(L)NULL)) { 
      e=h=0;
      m=strdupn(v+u,t); if(!m) R 0;
      if(m[0]!=(L)NULL){
        tok=strtok(m,y);
        k=kK(z)[e++]; 
        if(kC(c)[h++]==' ') e--;
        else kS(kK(z)[0])[p++]=sp(tok);
        while(tok != NULL){
          tok=strtok(NULL,y);
          if(tok!=NULL) {
            k=kK(z)[e++];
            if(kC(c)[h++]==' ') e--;
            else kS(kK(z)[0])[p++]=sp(tok);
          }
        }
      }
      free(m); p=0;
    }
    if(n>1 && (v[u+t]=='\n' || v[u+t]==(L)NULL)) {
      K q=0; e=h=0;
      m=strdupn(v+u,t); if(!m) R 0;
      if(m[0]!=(L)NULL){
        tok=strtok(m,y);
        k=kK(kK(z)[1])[e++];
        switch(kC(c)[h++]) {
          CS(' ', e--)
          CS('I', q=formKiCS(tok); kI(k)[p]=q?*kI(q):IN;)
          CS('F', q=formKfCS(tok); kF(k)[p]=q?*kF(q):FN;)
          CS('C', q=newK(-3,n=strlen(tok)); if(!q)R 0; memcpy(kC(q),tok,n); kK(k)[p]=q; q=0;)
          CS('S', kS(k)[p]=sp(tok);)
	}
        if(q && q->c<1000000 && q->c>0)cd(q);
        while(tok != NULL){
          tok=strtok(NULL,y);
          if(tok!=NULL) {
            k=kK(kK(z)[1])[e++];
            switch(kC(c)[h++]) {
              CS(' ', e--)
              CS('I', q=formKiCS(tok); kI(k)[p]=q?*kI(q):IN;)
              CS('F', q=formKfCS(tok); kF(k)[p]=q?*kF(q):FN;)
              CS('C', q=newK(-3,n=strlen(tok)); if(!q)R 0; memcpy(kC(q),tok,n); kK(k)[p]=q; q=0;)
              CS('S', kS(k)[p]=sp(tok);)
	    }
          }
          if(q && q->c<1000000 && q->c>0)cd(q);
        }
      }
      free(m); p++;
    }
  }

cleanup:
  munmap(v,s);
  R z;
}

//TODO: what happens if you 0: a 1:'d file ? Should 0: be rewritten to detect if the file is already mapped by 1:? Or is mmap good enough not to "remap" ?
//If you mmap the same file twice you get different resulting addresses
//TODO: Should these write functions require file locks?

K _1m(K x) //Keeps binary files mapped
{
  //See 'scratch.txt' for an Arthur implementation of this

  //Largely Copy/pasted from various I/O functions
  P(4!=xt && 3!=ABS(xt),TE)

  S m=CSK(x); //looks for .K or .L extensions first
  I sm = strlen(m);
  S e= sm > 1 && '.'==m[sm-2] && 'K'==m[sm-1] ? strdupn(m,sm) : glueSS(m,"K");  //lfop (lower-case l on Windows -- differs from 'L' in manual)
  U(e)
  
  struct stat c; //lfop windows: GetFileSizeEx

  I f=open(e,0); //Try the extended version of the filename first
  if(f>=0) stat(e,&c);
  else {f=open(m,0); stat(m,&c);} //Then try the plain version
  free(e);

  P(f<0,DOE)

  I s = c.st_size;
  if(s < 4*sizeof(I)){close(f); R NE;} //malformed file

  S v;
  //These mmap arguments are present in Arthur's code. WRITE+PRIVATE lets reference count be modified without affecting file
#ifndef WIN32  
  if(MAP_FAILED==(v=mmap(0,s,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_NORESERVE,f,0)))R SE;
#else
  if(MAP_FAILED==(v=mmap(0,s,PROT_READ|PROT_WRITE,MAP_PRIVATE,f,0)))R SE;
#endif

  //TODO: verify that the file is valid K data. For -1,-2,-3 types (at least) you can avoid scanning the whole thing and check size
  I b=0;
  K z = _1m_r(f,v,v,v+s,&b);
  close(f);
  munmap(v,s); 
  R z;
} 

Z K _1m_r(I f,V fixed, V v,V aft,I*b) //File descriptor, moving * into mmap, fixed * to last mmapped+1, bytes read
{
  I s=aft-v; //subtle but signed not big enough to hold max difference here
  if(s < 4*sizeof(I)) R NE; // file is malformed

  I*w=(I*)v; 

  I t=w[2], n=w[3];
  if(t<-4||t>7||n<0) R NE; //malformed

  if(4==ABS(t) || 7==t || (1<=t && t<=3) ) R _2m_r(v,aft,b); //These are read and not mapped

  I r=4*sizeof(I); if(0!=t&&5!=t) r+=bp(t)*n+(-3==t)-(t>0)*sizeof(I); 

  //0!=t&&5!=t <=> ((-3<=t&&t<=3)||6==t)

  if(s<r) R NE; //malformed
  
  K z,x;
  if(0==t||5==t){z=newK(t,n); DO(n,x=_1m_r(f,fixed,v+r,aft,&r); if(!x){cd(z);R 0;} kK(z)[i]=x; ) }
  else   //map lists to file. atoms are allocated not mapped
  {
    S u;
    I length=r;
    I offset=v-fixed+(t>0?3:4)*sizeof(I);
    I mod = offset&(PG-1); //offset must be a multiple of the pagesize
    length+=mod;
    offset-=mod;
#ifndef WIN32
    if(MAP_FAILED==(u=mmap(0,length,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_NORESERVE,f,offset))){R SE;}
#else
    if(MAP_FAILED==(u=mmap(0,length,PROT_READ|PROT_WRITE,MAP_PRIVATE,f,offset))){R SE;}
#endif
    z=(K)(((V)u+mod)-3*sizeof(I)); //3*sizeof(I) for c,t,n
    //if(1<=t || 3<=t){dd(z->n)} // ???
  }

  *b+= MAX(r,4*sizeof(I));

  R z;
}


K _1d(K x,K y)
{
  I t=x->t;
  if(4==t || -3==t)R _1d_write(x,y); //char-vector but not char-atom
  if(!t)R _1d_read(x,y);
  if(3==t)R _1d_char(x,y);
  R TE;
}

//TODO: for testing this, use 1:write and 2:read (or 1:read) to confim items are the same before write & after read
Z K _1d_write(K x,K y)
{
  //Note: all file objects must be at least 4*sizeof(I) bytes...fixes bugs in K3.2, too
  //K3.2 Bug - "a"1:`a;2:"a" or 1:"a" - wsfull, tries to read sym but didn't write enough bytes?
  I n=disk(y);
  
  //Copy-pasted from 2:
  S m=CSK(x); 
  I sm = strlen(m);
  S e= sm > 1 && '.'==m[sm-2] && 'K'==m[sm-1] ? strdupn(m,sm) : glueSS(m,"K");  //lfop (lower-case l on Windows -- differs from 'L' in manual)
  U(e)

  //Largely copy-pasted from 6:dyadic
  I f=open(e,O_RDWR|O_CREAT|O_TRUNC,07777);
  free(e);
  P(f<0,SE)

  P(ftruncate(f,n),SE)
  //lfop: see 0: write for possible way to do ftruncate etc. on Windows
  S v;
  if(MAP_FAILED==(v=mmap(0,n,PROT_WRITE,MAP_SHARED,f,0)))R SE; // should this be MAP_PRIVATE|MAP_NORESERVE ?
  close(f);

  wrep(y,v,1);

  //msync(v,n,MS_SYNC|MS_INVALIDATE); //slow
  munmap(v,n); 

  R _n();
}

I wrep(K x,V v,I y)//write representation. see rep(). y in {0,1}->{net, disk}
{
  I t=xt, n=xn;
  I* w=(I*)v;

  I m=y?2:0;

  if(y){*w=-3; w[1]=1; w[2]=t; w[3]=n;}
  else{memcpy(w,&(x->t),sizeof(x->t)+sizeof(x->n));}

  V d=w+2+m; //disk/destination for lists/vectors

  I e=(2+m)*sizeof(I); if(0!=t&&5!=t&&-4!=t) e=rep(x,y); //don't rep() on nested structures -- O(n^2)

  I r=0,s;
  if(0==t||5==t) DO(n, V point = d+r; I delta = wrep(kK(x)[i],point,y); r+=delta )
  else if(-4==t) DO(n, s=1+strlen(kS(x)[i]); memcpy(d+r,kS(x)[i],s); r+=s )
  else if( '\007'==t || '\010'==t) //TODO: write seven_types to disk //TODO: calculate return length r optimally for seven_type since seven_type can nest 
  {
    //outdated
    //if(1==xn && 1==kVC(x)->n-1){ K k=*kW(x); I s=sva(k); w[m]=1==s?'\007':'\010';  w[1+m] = diff(k,s==1?vm:vd); }  //TODO: work for more than just unreserved monadic, dyadic verbs
  }
  else {V s=ke(x); I b=n*bp(t)+(3==ABS(t)); if(t>0)d-=sizeof(I); if(4==t){s=*kS(x); b=1+strlen(*kS(x)); } memcpy(d,s,b);}

  R e+r;
}

Z I disk(K x){R rep(x,1);}//how many bytes does this take on disk?
I rep(K x,I y) //#bytes in certain net/disk representations 
{
  //Notes on verbs/functions: (changes must go to rep(),wrep(),and rrep()
  //_bd (%:) is type 7
  //_bd (%) is type 8
  //_bd {x} is type 10 
  //_bd (+/) is type 20, -': -/ -\ all seem to have have their own types
  //projection causes nonce error

  I m=sizeof(I)*(y?4:2), r=m, n=xn, q=0;  //y crutch for factor {0,1}->{net size, disk size}
  SW(xt)
  {
    CSR(0,) CS(5, DO(xn,r+=rep(kK(x)[i],y)))
    CSR('\007',) CS('\010', if(1==xn);    ) //TODO - seven_types on disk  (1==xn --> no size increase)
    CS(-4, DO(n, r+=1+strlen(kS(x)[i])))
    CS(-3, r+= (1+n)*sizeof(C))
    CS(-2, r+=     n*sizeof(F))
    CS(-1, r+=     n*sizeof(I))
    CS( 4, q=1+strlen(*kS(x)); if(q>=sizeof(I))r+=q-sizeof(I))//without q check can cause trouble on 32-bit
  }
  R MAX(r,m);
}

K rrep(V v, V aft,I*b, I y)//why aft? maybe not the best? but invariant. size count changes. haven't closely compared elegance
{
  //y: kind of a crutch to prevent fork. {0,1}-> {_db type, _2m_r type}
  I m = y?2:0;  //addend to offset

  I s=aft-v;;//subtle error here but not really...filesize already assumed to fit in float in parent

  I*w=(I*)v; //Note: sizeof(I) probably 8, meaning this won't read old 32-bit K files

  I r=(2+m)*sizeof(I); //accumulates #bytes of the file required for this structure 

  P(s<r,NE)
  if(y)P(-3!=w[0],NE)

  //if(y)w[1]; //mmap reference count
  I t;
  memcpy(&t,w+m,sizeof(I)); //type
  I n;
  if(t<=0 || 5==t)memcpy(&n,w+1+m,sizeof(I));
  else if('\012'==t); //TODO: some verb/function types increase r or n size
  else n=1;

  if     (-1==t) r+=   n *sizeof(I);
  else if(-2==t) r+=   n *sizeof(F);
  else if(-3==t) r+=(1+n)*sizeof(C); //appears to need final '\0' or eval size limit ???
  if(s < r) R NE;//(could instead have these errors occuring individually in the switch statement)

  K z=(-4 <= t && t<= 6)? newK(t,n):Kv(); U(z)


  I c=0; // k->n counter
  switch(t) //most of this can be refactored into changing parameters to a single memcpy call
  {
    CSR( 0,)//fall through
    CS ( 5,while(v+r < aft && c < n) { K k=rrep(v+r,aft,&r,y); M(z,k) memcpy(&(kK(z)[c++]),&k,sizeof(K)); } if(c!=n){cd(z);R NE;} ) 

    CS(-4,while(v+r < aft && c < n) r+=rrep_4(kS(z)+c++,v+r,aft); P(c!=n,NE) ) //TODO: oom
    CS(-3,memcpy(kC(z),w+2+m,n*sizeof(C)))//K3.2 does not verify final '\0' (does not read any extra bytes at all)
    CS(-2,memcpy(kC(z),w+2+m,n*sizeof(F)))//maybe could factor above and below (but sizeof C != sizeof I/F)
    CS(-1,memcpy(kC(z),w+2+m,n*sizeof(I)))
    CS( 1,memcpy(kI(z),w+1+m,1*sizeof(I)))
    CS( 2,memcpy(kF(z),w+1+m,1*sizeof(F)))
    CS( 3,memcpy(kC(z),w+1+m,1*sizeof(C))) //K3.2 take first C but do not check remaining C values of full I at w[3]
    CS( 4,r+=rrep_4(kS(z),(S)(w+1+m),aft)-sizeof(I)) //TODO: oom. K3.2 reads to the end of the file no problem even if null is missing. K3.2 has bug on `x or `xx (<3)
    CS( 6,) //no-op
    //TODO: verb cases:  +, {x}, 2:("f",2)  (third case probably not supported but see). Do projections get written? Note: _bd (-); _bd (+); _bd (:); etc are revealing
    //using old K3 IO format, using outdated Kona internal verb representation:
    //CSR('\007',) CS('\010', f=newK(-4,2); M(z,f) kV(z)[CODE]=f; *kK(f)=(K)('\007'==t?vm+w[1+m]+1:(vd+w[1+m]));  r+=000000000000000;)
    CD: R NE; //unsupported type. was:  if(t<-4 || t>7 || n<0) R NE; //verbs actually have some weird types though. 8==\010, etc
  }
  
  *b+= MAX(r,(2+m)*sizeof(I));
  R z;
}

Z I rrep_4(S*z,S a,S t) //type4 reader for 2: monadic
{
  S d=a; 
  while(a<t && *a)a++;
  I c=a-d;
  S e=strdupn(d,c); //oom
  *z=sp(e); //TODO: oom
  if(e)free(e);
  R c + (a!=t);
}


//From http:/kx.com/q/c/c/readme.txt
//type: KBGHIJEFSCDTZ
//base: KGGHIJEFSCIIF
//size: *1124848*1448

Z K _1d_read(K a,K b)
{
  S types = "cbsijfdmIFCSDZM"; //Help has this with space ' ' as full list (with 'm' at end)? But "@cbsifdMmDTZIFSCY" (and N in k20.dll) from strings. Manual has outdated "cbsifd CS"
  I fixed[] = {sizeof(C),sizeof(int8_t),sizeof(int16_t),sizeof(int32_t), //1,1,2,4,8,4,8 on 64-bit arch
               sizeof(I),sizeof(float),sizeof(F),9};  //lowercase, must be at beginning of types string. TODO: Replace the 9 placeholder for 'm'
  I typelist[] = {-3,-1,-1,-1,-1,-2,-2,9,9,9,0,-4,9,9,9}; //TODO: replace 9s with proper corresponding values
  C g;
  //Largely copy/pasted from _0d_read
  //may assume !a->t
  K z=0;
  I an=a->n, bt=b->t, bn=b->n;
  P(an!=2,DOE)
  K c=kK(a)[0],d=kK(a)[1];
  I cn=c->n, dn=d->n;
  P(3 != ABS(c->t) || 1 != ABS(d->t),TE) // (K3.2 strictly requires lists---bad)
  P(!cn || cn != dn,LE)
  P(3 != ABS(bt) && 4 != bt && 0 != bt, TE)

  I fc=0; //field count
  DO(cn,g=kC(c)[i];if(' '==g)continue;if(stringHasChar(types,g))fc++;else R TE;if(islower(g)&&fixed[charpos(types,g)]!=kI(d)[i])R LE)

  I w=0,x; //Note: w=0 here, but w=1 in 0:dyadic
  DO(dn, x=kI(d)[i]; P(x<=0,LE) w+=x) 

  K ff=b,k; I fb=0,fn=0; //mostly for bt!=0

  if(!bt)
  {
    P(3!=bn,LE)
    ff=kK(b)[0];
    P(3!=ABS(ff->t) && 4 != ff->t,TE) //  (could refactor this {-3,3,4} check )
     k=kK(b)[1];
    P(1!=k->t && 2!=k->t,TE)
    fb=k->t-1?*kF(k):*kI(k);
     k=kK(b)[2];
    P(1!=k->t && 2!=k->t,TE)
    fn=k->t-1?*kF(k):*kI(k);
  }

  I s; P(stat_sz(CSK(ff),&s),SE)
 
  if(bt) fn=s;

  //if(fn<0 || fb+fn>s || fn%w) R 0; //length error. omitted to reduce errors

  if(fn<0) fn=0; //suppress error
  if(fb<0) fb=0; //in K3.2 fb<0 is ok
  fb=MIN(fb,MAX(0,s-1)); 
  if(fb+fn > s) fn = s-fb;

  I f=open(CSK(ff),0);
  P(f<0, DOE)

  S v;

  I fb_off_by  = fb % PG; //desired offset fb misaligned from page boundary by
  I map_length = fn + fb_off_by;
  I map_offset = fb - fb_off_by;

  if(MAP_FAILED==(v=mmap(0,map_length,PROT_READ,MAP_SHARED,f,map_offset)))R SE;
  close(f);

  //End of copy/paste

  I r=fn/w;//valid rows (if no error checking implemented for fn%w this code still works)

  z=newK(0,fc);
  if(!z) R 0;//TODO oom unmap
  I e=0;
  DO(cn, g=kC(c)[i]; if(' '==g)continue; kK(z)[e++]=newK(typelist[charpos(types,g)],r))

  //(kC(c)[], kI(d)[] ) 1: (ff,fb,fn)
  I i,j;
  V p=v + fb_off_by;

  //cbsijfdmIFCSDZM
  for(j=0;j<r;j++) //read row 
  {
    e=0;
    for(i=0;i<cn;i++)
    {
           x=kI(d)[i];
           K q=kK(z)[e++]; 
           S u;
           switch(g=kC(c)[i]) 
           {
             CS(' ', e--) 
             CS('c',kC(q)[j]=*((      C*)p)) 
             CS('b',kI(q)[j]=*(( int8_t*)p)) 
             CS('s',kI(q)[j]=*((int16_t*)p)) 
             CS('i',kI(q)[j]=*((int32_t*)p)) 
             CS('j',kI(q)[j]=*((      I*)p)) 
             CS('f',kF(q)[j]=*((  float*)p))
             CS('d',kF(q)[j]=*((      F*)p))
             CS('m', ) //TODO: fill in remaining character reads
             CS('I', )
             CS('F', )
             CS('C',k=newK(-3,x); if(!k)R 0; memcpy(kC(k),p,x); kK(q)[j]=k) //TODO: oom  unmap
             CS('S',if(!(u=spn(p,x)))R 0; kS(q)[j]=u ) //TODO: oom unmap
             CS('D', )
             CS('Z', )
             CS('M', )
           }
           p+=x;
    }
  }

  munmap(v,map_length);
  R z;
}

Z K _1d_char(K x, K y)
{
  C a=*kC(x);
  if('c'==a) R readVector(y,-3);
  if('d'==a) R readVector(y,-2);
  if('i'==a) R readVector(y,-1);
  R NE;
}


K _2m(K a) //again, minor copy/paste here
{
  I t=a->t;
  P(4!=t && 3!=ABS(t),TE)

  S m=CSK(a); //looks for .K or .L extensions first
  I sm = strlen(m);
  S e= sm > 1 && '.'==m[sm-2] && 'K'==m[sm-1] ? strdupn(m,sm) : glueSS(m,"K");  //lfop (lower-case l on Windows -- differs from 'L' in manual)
  U(e)
  
  I s,f=open(e,0); //Try the extended version of the filename first
  if(f>=0) P(stat_sz(e,&s),SE) 
  else {f=open(m,0); P(stat_sz(m,&s),SE)} //Then try the plain version
  free(e);
  P(f<0,DOE)

  S v;
  if(MAP_FAILED==(v=mmap(0,s,PROT_READ,MAP_SHARED,f,0)))R SE;
  close(f);

  //K3.2 Bug: does not check boundary and will segfault on bad binary data (e.g., char vec with lying size of >> pagesize)
  //reading past boundary will segfault. pass boundary? 
  I b=0;
  K z=_2m_r(v,v+s,&b);
  //if(!z); //continue to unmap
  munmap(v,s); 
  R z;
}

K _2m_r(V v, V aft,I*b){R rrep(v,aft,b,1);}



K _2d(K a,K b)
{
  //K3.2 dlopen,dlsym,dlerr but not dlclose
  //max 7 args
  //Mac OS X: gcc -m64 -flat_namespace -undefined suppress -dynamiclib file.c -o file.dylib
  K c,d;
  P((4!=a->t && 3!=ABS(a->t)) || b->t || b->n!=2 || (4!=(c=kK(b)[0])->t && 3!=ABS(c->t)) || 1!=(d=kK(b)[1])->t, TE)
  L v=*kI(d);
  P(v<0 || v > 7, VE)

  cS e;
  V x=dlopen(CSK(a),RTLD_LAZY|RTLD_LOCAL),y;

  e=dlerror();
  if (e) { O("error loading %s\nerr=%s\n", CSK(a), e); R 0; }
  if (!x){ O("error loading %s \n", CSK(a)); R 0; }
  y=dlsym(x,CSK(c));
  e=dlerror();
  P(e&&*e,kerr(e))
  P(!y,DOE)

  K z=Kv(), w=newK(-4,3); M(z,w); z->n=2;
  kK(w)[0]=(V)v;//valence
  kK(w)[1]=y;   //function*
  //kK(w)[2]=0;   //reminder
  kV(z)[CODE] = w;

  R z;
}

Z I sendall(I s,S b,I k){I t=0,r=k,n=0;while(t<k){n=send(s,b+t,r,0);if(-1==n)break;t+=n;r-=n;}R -1==n?n:0;}//from beej

I ksender(I sockfd,K y,I t)
{
  I r=0;
  K k; U(k=_bd(y))
  M1*m=(V)kC(k);
  m->d=t; //{0,1,2} -> {3:,4:,4: resp}
  if(-1==(r=sendall(sockfd,kC(k),k->n)))perror("conn: send error");
  cd(k);
  R r;
}


K _3d(K x,K y) //'async' TCP
{
  P(4==xt && !**kS(x), NYI) //TODO: `3:string   os set command, e.g. `3:"k bck"
  P(1!=xt, TE)
  P(-1==ksender(*kI(x),y,0),DOE)
  R _n();
  //Communicate with 32-bit K3.2, but see _3m where handshake is purposefully broken
  //C buf[]="\001\000\000\000\020\000\000\000\375\377\377\377\007\000\000\000`0:$`hi\000";
  //I n; //DO(10, if ((n = send(sockfd, buf, -1 + sizeof buf, 0)) == -1) { perror("send error"); R DOE; } else hi(OK))
}

K popen_charvec(C *cmd)
{
  FILE *f; K z; K l; S s=0; I n=0;
  f=popen(cmd,"r");
  P(!f,_n())
  z=newK(0,0); //oom
  while (getline_(&s, (size_t * __restrict__)&n, f) >= 0)
  { l=newK(-3,n-1);
    strncpy(kC(l),s,n-1);
    kap(&z,&l);
  }
  if(s)free(s);
  pclose(f);
  R z;
}


K _4d(K x,K y) //see _3d
{
  if (4==xt && !**kS(x) && -3==y->t) { // `4:"ls" -> lines from popen("ls", "r"), blocking
    return popen_charvec(kC(y));
  }

  P(1!=xt, TE)
  I sockfd = *kI(x);
  P(-1==ksender(sockfd,y,1),DOE)
  K z=0;
#ifndef WIN32
  while(!(z=read_tape(sockfd,1)));
#else
  while(!(z=read_tape(0,sockfd,1)));
#endif
  P(!z || z==(K)-1,DOE)
  R z;
}
K _4m(K x){R Ki(x->t);}


K _5m(K x)
{
  K z;
  U(z=newK(-3,0))
  printAtDepth(&z,x,0,0,0,0);
  R z;
}

//TODO Does 5:d need a filesize double? Trunc replace O_Creat? In other numeric i/o verbs? Note: trunc doesn't necessarily reserve disk space so can still have disk err
K _5d(K x,K y)
{
  //TODO: what if a file is mapped using 1: read and I use 5: write to append to it? does my variable holding the 1: map change?
  //K3.2 5:dyadic can create files for all types except +1 (uses 1:dyadic). +1 maybe a bug? fixed here
  //       TODO: update: it's not a bug, see K2.9   f 5:n   truncate file to n items
  //K3.2 5:dyadic can add to files for <=0 only. added list/vector must be exact same type (0==0, -1==-1, ...)
  //we add support for type5 dictionaries, easy to turn off, but keep in mind .((`a;1);(`b;2);(`a;3)) happens but d[`a]=1 as expected

  //can't write symbols/chars efficiently if #bytes rounded up. for this reason switched to using MAX(.,4*sizeof(I)) instead of nearI(.)

  //Largely Copy/pasted from 2:monadic
  if(4!=xt && 3!=ABS(xt)) R 0;//TODO: type error

  S m=CSK(x); //looks for .K or .L extensions first
  I sm = strlen(m);
  S e= sm > 1 && '.'==m[sm-2] && 'K'==m[sm-1] ? strdupn(m,sm) : glueSS(m,"K");  //TODO: lfop (lower-case l on Windows -- differs from 'L' in manual)
  if(!e)R 0; //TODO: oom
  
  struct stat c; //lfop windows: GetFileSizeEx

  I f=open(e,O_RDWR,07777); //Try the extended version of the filename first
  if(f>=0) stat(e,&c);
  else {f=open(m,O_RDWR,07777); stat(m,&c);} //Then try the plain version
  free(e);
  //End copy/paste

  //File doesn't exist so fall back to 1:
  if(f<0) R _1d_write(x,y); //manual says return count but that is incorrect/bug.

  I s = c.st_size;
  if(s < 4*sizeof(I)) R 0; //TODO: err, file is malformed

  //TODO: regular file read + rewind?
  I ft,fn;
#ifndef WIN32
  I g;
  g=pread(f,&ft,sizeof(ft),2*sizeof(I)); if(!g)show(kerr("pread"));
  g=pread(f,&fn,sizeof(ft),2*sizeof(I)+sizeof(ft)); if(!g)show(kerr("pread"));
#endif

  if( (yt>0&&yt!=5) || ft != yt) R 0; //TODO: type error
  
  I b = disk(y) - 4*sizeof(I) - (-3==yt); //-3 type overwrites preexisting '\0' terminator
  I n = s + b;

  //Mostly copy-pasted from 6:dyadic and 1:dyadic
  if(ftruncate(f,n))R 0; //TODO: error

  //lfop: see 0: write for possible way to do ftruncate etc. on Windows
  S v;
  if(MAP_FAILED==(v=mmap(0,n,PROT_WRITE,MAP_SHARED,f,0)))R SE;
  close(f);

  I*w=(I*)v;
  w[3]=fn+yn;

  I r=0;
  V d=v+s;
  //This is copy/pasted from 1:dyadic, with subtle differences (x->y, -3 char '\0')
  if(0==yt||5==yt) DO(yn, r+= wrep(kK(y)[i],d+r,1) )
  else if(-4==yt) DO(yn, s=1+strlen(kS(y)[i]); memcpy(d+r,kS(y)[i],s); r+=s )
  else if(-3==yt) {memcpy(d-1,ke(y),n*sizeof(C)); ((S)d)[yn]=0;} // ((S)d)[yn]=0 unnecessary?
  else if(-2==yt)  memcpy(d,ke(y),y->n*sizeof(F));
  else if(-1==yt)  memcpy(d,ke(y),y->n*sizeof(I)); 
 
  //msync(v,n,MS_SYNC|MS_INVALIDATE); //slow
  munmap(v,n); 

  R Ki(fn+yn); //mm/o 
}

K _6m(K x) { R readVector(x,-3);} //Believe 6:"file.K" to be equivalent to "c"1:"file.K"
Z K readVector(K x,I t)//This is largely copy/pasted from 0:. Written only for -1,-2,-3
{
  P(4!=xt && 3!=ABS(xt), TE)

  I s; P(stat_sz(CSK(x),&s),SE)
 
  I f=open(CSK(x),0);
  P(f<0, DOE)
  S v;

  if(MAP_FAILED==(v=mmap(0,s,PROT_READ,MAP_SHARED,f,0)))R SE; //Should this be PRIVATE+NO_RESERVE ?
  close(f);

  K z=newK(t,ceil(s/(F)bp(t)));//TODO: oom (unmap, etc.)
  memcpy(ke(z),v,s); //K3.2 on -1 and 2 leave garbage here for files not a multiple of sizeof(I) or sizeof(F) 
  munmap(v,s); 
  R z;
}


K _6d(K a,K b) //A lot of this is copy/paste from 0: dyadic write
{
  //K3.2 bug:  (,`) 6: "chars" -> prints ": No such device or address"
  //TODO?  (1.0) 6: "data" -> stop

  //-this is a lot like 0: write without the newline business
  //-if the first argument is an enlisted sym(4) or enlisted -3 then this is append and not overwrite  (,`a) 6: "aaa"  or (,"abc") 6: "aaa"
  I append = 0;

  K c=a;
  if(0==a->t && 1==a->n) {append=1; c=kK(a)[0];}
  else if (-4==c->t && 1==c->n) append = 1;

  if(4!=c->t && 3!=ABS(c->t) && (!append && -4==c->t))R 0; //TODO: err? (1)6:"a" -> nothing,  (1.0 or `a`b) 6: "a" -> stop

  I t=b->t, n=b->n;
  P(3!=ABS(t),TE)

  S m=CSK(c); //possible inputs reaching this point: 4,3,-3, and -4 (size 1)

  I f=m[0]?open(m,O_RDWR|O_CREAT|(append ?0:O_TRUNC),07777):1; //stdout if m is ` or "" or "\000..."
  I e=0;
  if(append && m[0]) //below 'open' : don't fail if file didn't exist prior to this append
    P(stat_sz(m,&e),SE)

  P(f<0,SE)

  if(1==f) {I r=write(f,kC(b),n); if(!r)show(kerr("write"));}       //write to stdout on empties
  else                       //write to mmap'd file
  {
    P(ftruncate(f,e+n),SE)
    //lfop: see 0: write for possible way to do ftruncate etc. on Windows
    S v;
    if(MAP_FAILED==(v=mmap(0,e+n,PROT_WRITE,MAP_SHARED,f,0)))R SE; //should this be MAP_PRIVATE|MAP_NORESERVE ?
    close(f);
    memcpy(v+e,kC(b),n);
    msync(v+e,n,MS_SYNC|MS_INVALIDATE); //keep msync for _6d ??? see issue 163
    munmap(v,e+n); 
  }

  R _n();
}

#ifndef WIN32

//TODO: Manual's section on interprocess communication. {-h, .m.u, .m.c, .m.g, .m.s, .m.h}
K _3m(K x)
{
  if(1==xt){I i=close(*kI(x)); R i?DOE:_n();} // 3: 1
  else P(xt|| xn!=2 || kK(x)[0]->t!=4 || kK(x)[1]->t!=1, TE)
  //3:`"999.999.999.999",1234   // same host: 3:`,1234
  S host=CSK(*kK(x)), errstr;
  char port[256];
  snprintf(port,256,"%lld",*kI(kK(x)[1]));

  int sockfd;
  struct addrinfo hints, *servinfo, *p; 
  int rv;
  memset(&hints, 0, sizeof hints); 
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;

  if ((rv = getaddrinfo(host, port, &hints, &servinfo)) != 0) { fprintf(stderr, "conn: %s\n", gai_strerror(rv)); R DOE; }
  // loop through all the results and connect to the first we can
  for(p = servinfo; p != NULL; p = p->ai_next) 
    if ((sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol)) == -1) {  continue; } //perror("client: socket");
    else if (connect(sockfd, p->ai_addr, p->ai_addrlen) == -1) { errstr=strerror(errno); close(sockfd);  continue; } //perror("client: connect");
    else break;

  if (p == NULL) { fprintf(stderr, "conn: failed to connect (%s)\n", errstr);freeaddrinfo(servinfo); R DOE; }

  //char s[INET6_ADDRSTRLEN];
  //inet_ntop(p->ai_family, get_in_addr((struct sockaddr *)p->ai_addr), s, sizeof s);
  //O("client: connecting to %s\n", s);
  I yes=1;
  setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY, &yes, sizeof(I));//disable nagle
#if defined(__MACH__) && defined(__APPLE__) || defined(__FreeBSD__)  || defined(__NetBSD__)
  setsockopt(sockfd, SOL_SOCKET,  SO_NOSIGPIPE,&yes, sizeof(I)); 
#endif
  freeaddrinfo(servinfo); 

  //Z C m[ 8]; if(-1==sendall(sockfd,(S)&m,sizeof m))R DOE; //not really sure what this handshake is for, 32-bit K3.2 requires it

  wipe_tape(sockfd);
  R Ki(sockfd);
}

#else

//TODO: Manual's section on interprocess communication. {-h, .m.u, .m.c, .m.g, .m.s, .m.h}
K _3m(K x)
{
  if(1==xt){I i=close(*kI(x)); R i?DOE:_n();} // 3: 1
  else P(xt|| xn!=2 || kK(x)[0]->t!=4 || kK(x)[1]->t!=1, TE)
  //3:`"999.999.999.999",1234   // same host: 3:`,1234
  S host=CSK(*kK(x)); char port[256]; snprintf(port,256,"%lld",*kI(kK(x)[1]));

  // initialize WinSock
  WSADATA wsaData;
  int err = WSAStartup(MAKEWORD(2,2), &wsaData);
  if(err != 0) O("WSAStartup failed with error: %d\n",err);
  if(LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2)
    { O("Could not find useable version of Winsock.dll\n"); exit(1); }
  PORT=port;  //need this for finally() to cleanup WinSock

  struct addrinfo *servinfo = NULL, *p = NULL, hints;
  ZeroMemory( &hints, sizeof(hints));
  hints.ai_family = AF_UNSPEC; hints.ai_socktype = SOCK_STREAM; hints.ai_protocol = IPPROTO_TCP;

  int rv = getaddrinfo(host, port, &hints, &servinfo);
  if(rv != 0) { O("getaddrinfo failed:%d\n", rv); exit(4); }
  //else O("getaddressinfo OK\n");

  SOCKET sockfd = INVALID_SOCKET;
  p=servinfo; sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
  if(sockfd == INVALID_SOCKET) { O("Error at socket():%ld]n", WSAGetLastError()); freeaddrinfo(servinfo); exit(4); } 
  //else O("socket() OK\n");

  // loop through all the results and connect to the first we can
  for(p = servinfo; p != NULL; p = p->ai_next) 
    if ((sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol)) == -1) 
      { perror("client: socket()"); O("client socket(): %ld\n", WSAGetLastError()); continue; }
    else if (connect(sockfd, p->ai_addr, p->ai_addrlen) == -1) 
      { O("client connect(): %d\n", WSAGetLastError()); close(sockfd); continue; }
    else break;

  if (p == NULL) { fprintf(stderr, "conn: failed to connect (%d)\n", WSAGetLastError());freeaddrinfo(servinfo); R DOE; }
  I yes=1; setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY, (cS)&yes, sizeof(I));//disable nagle
  freeaddrinfo(servinfo); 
   
  //wipe_tape(sockfd);
  R Ki(sockfd); 
}

#endif