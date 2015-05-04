K ci(K a);
K vf_ex(V q,K g);
S CSK(K x);
K X(S s);
I ksender(I sockfd,K y,I t);
K _db(K x);
K newK(I t,I n);
extern fd_set master;
K cd(K a);
I wipe_tape(I i);
void *get_in_addr(struct sockaddr *sa);
extern pthread_mutex_t execute_mutex;
extern S HTTP_PORT;

#ifndef WIN32
K read_tape(I i,I type);
extern M0 CP[FD_SETSIZE];
#else
K read_tape(I i,I j,I type);
extern M0 CP[10];
#endif
