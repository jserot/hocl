#include "foo.h"

void foo(nat k, IN int *i, OUT int *o)
{
  *o = *i * k;
}

void bar(IN int *i, OUT int *o)
{
  *o = *i + 1;
}
