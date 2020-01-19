#include "input.h"

static int cnt = 0;

void inputInit(void)
{
  cnt = 0;
}

void input(OUT int *o)
{
  *o = cnt++;
}
