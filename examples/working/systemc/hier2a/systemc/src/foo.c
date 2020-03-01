#include "foo.h"

void foo(PARAM int k, IN int *i, OUT int *o)
{
  *o = *i * k;
}
