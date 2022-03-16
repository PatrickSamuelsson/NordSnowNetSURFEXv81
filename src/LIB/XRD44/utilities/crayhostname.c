#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
void crayhostname_(char *hostname, int len_hostname)
{
  int result;
  result = gethostname(hostname, len_hostname);
  return;
}
