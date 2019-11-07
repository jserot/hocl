#include "output.h"
#include <stdio.h>

void outputInit(void)
{
  // Nothing here
}

void output(IN int *i1, IN int *i2)
{
  printf("output: got %d and %d\n", *i1, *i2);
  fflush(stdout);
}
