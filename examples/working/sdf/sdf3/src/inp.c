#include "inp.h"

static int cnt;

void inpInit(void)
{
  cnt = 1;
}

void inp(OUT int *o)
{
  o[0]=cnt++;
  o[1]=cnt++;
}
