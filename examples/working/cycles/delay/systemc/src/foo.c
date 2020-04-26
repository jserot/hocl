#include "foo.h"

void foo(IN int *x, IN int *z, OUT int *s, OUT int *zx)
{
  *s = *x+*z;
  *zx = *x; 
}
