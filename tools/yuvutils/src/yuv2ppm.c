#include <stdio.h>
#include <stdlib.h>
#include "yuvutils.h"

int main(int argc, char **argv)
{
  if ( argc != 5 ) {
    fprintf(stderr, "usage: %s width height req_nframes ifile\n", argv[0]);
    exit(1);
    }
  int width, height, nframes;
  int cnt=0;
  cnt += sscanf(argv[1], "%d", &width);
  cnt += sscanf(argv[2], "%d", &height);
  cnt += sscanf(argv[3], "%d", &nframes);
  if ( cnt != 3 ) {
    fprintf(stderr, "%s: bad arguments\n", argv[0]);
    exit(2);
    }
  return yuv2ppm(argv[4], nframes, width, height);
}
