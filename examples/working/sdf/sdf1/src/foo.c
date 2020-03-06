#include "foo.h"
#include <stdio.h>

void foo(IN int *i, OUT int *o)
{
  int k;
  int s=0;
  for ( int k=0; k<CONS_RATE; k++ ) s += i[k];
  for ( int k=0; k<PROD_RATE; k++ ) o[k] = s; // Replicate sum
}
