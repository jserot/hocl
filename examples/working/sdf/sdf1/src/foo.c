#include "foo.h"

void foo(IN int *i, OUT int *o)
{
  int s=0;
  for ( int j=0; j<NTOKS; j++ ) s += i[j];
  *o = s;
}
