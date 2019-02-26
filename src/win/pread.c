#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

ssize_t pread(int __fd,void* __buf,size_t __nbytes,off_t __offset)
{
	ssize_t ret;
	off_t old = lseek(__fd,0,SEEK_CUR);
	if(old==(off_t)-1)return -1;
	if(-1==lseek(__fd,__offset,SEEK_SET))return -1;
	ret=read(__fd,__buf,__nbytes);
	lseek(__fd,old,SEEK_SET);
	return ret;
}
