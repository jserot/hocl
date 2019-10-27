#include "output.h"

void outputInit(void)
{
  // Nothing here
}

void output(IN int *i)
{
  printf("output: got %n\n", *i);
  flush(stdout);
}
