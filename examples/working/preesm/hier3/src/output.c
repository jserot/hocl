#include "output.h"
#include <stdio.h>

void outputInit(void)
{
  // Nothing here
}

void output(IN int *i)
{
  printf("output(%d): got %d\n", k, *i);
  fflush(stdout);
}
