#include "output.h"
#include <stdio.h>

void outputInit(void)
{
  // Nothing here
}

void output(IN int *i)
{
  printf("output: got %d\n", *i);
  fflush(stdout);
}
