#include "outp.h"
#include <stdio.h>

void outp(IN int *i)
{
  printf("outp: got %d %d\n", i[0], i[1]);
  fflush(stdout);
}
