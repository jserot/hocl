#include "cond.h"

void is_even(IN int *i, OUT bool *o)
{
  *o = *i %2 == 0;
}

void incr(IN int *i, OUT int *o)
{
  *o = *i+1;
}

void mult2(IN int *i, OUT int *o)
{
  *o = *i*2;
}
