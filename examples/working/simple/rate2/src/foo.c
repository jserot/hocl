#include "foo.h"

void foo(int k, IN int *i, OUT int *o)
{
  int s=0;
  for ( int j=0; j<k; j++ ) s += i[j];
  *o = s;
}
