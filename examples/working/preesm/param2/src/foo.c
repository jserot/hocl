#include "foo.h"

void foo(nat k, nat l, IN int *i, OUT int *o)
{
  *o = *i * k + l;
}
