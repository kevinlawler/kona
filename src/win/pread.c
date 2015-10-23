#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

size_t pread(int __fd,void* __buf,size_t __nbyte,off_t __off)
{
	size_t ret;
	off_t old = lseek(__fd,0,SEEK_CUR);
	if(old==(off_t)-1)return -1;
	if(-1==lseek(__fd,__off,SEEK_SET))return -1;
	ret=read(__fd,__buf,__nbyte);
	lseek(__fd,old,SEEK_SET);
	return ret;
}
