#include <stdio.h>
#include "yuv.h"

int main(int argc, char **argv)
{
  // return yuv2pgm("akiyo_cif.yuv", 1, 352, 288);
  return yuv2ppm("akiyo_cif.yuv", 1, 352, 288);
}
