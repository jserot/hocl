#include "foo.h"

void foo(IN int *i, OUT int *o)
{
  *o = *i * 2;
}

void bar(IN int *i, OUT int *o)
{
  *o = *i + 1;
}
