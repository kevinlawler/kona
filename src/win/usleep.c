#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>

/* forum post by cyi823 5oct2005 at c-plusplus.net */
void win_usleep(unsigned int usec)
{
  HANDLE timer;
  LARGE_INTEGER due;

  due.QuadPart = -(10 * (__int64)usec);
  timer = CreateWaitableTimer(NULL, TRUE, NULL);
  SetWaitableTimer(timer, &due, 0, NULL, NULL, 0);
  WaitForSingleObject(timer, INFINITE);
  CloseHandle(timer);
}
