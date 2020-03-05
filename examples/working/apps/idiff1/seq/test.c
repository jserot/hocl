#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "yuvRead.h"
#include "yuvDisplay.h"
#include "difference.h"

int main(int argc, char **argv)
{
  int width = WIDTH;
  int height = HEIGHT;
  int id = 0;
  int cnt = 0;
  unsigned char ival = 0;
  unsigned char* yi = (unsigned char*)malloc(sizeof(char)*height*width);
  unsigned char* yo = (unsigned char*)malloc(sizeof(char)*height*width);
  unsigned char* yr = (unsigned char*)malloc(sizeof(char)*height*width);
  unsigned char* yp = (unsigned char*)malloc(sizeof(char)*height*width);
  unsigned char* u = (unsigned char*)malloc(sizeof(char)*height/2*width/2);
  unsigned char* v = (unsigned char*)malloc(sizeof(char)*height/2*width/2);
  initReadYUV(width, height);
  yuvDisplayInit(id, width, height);
  memset(yp, ival, height*width);  // Initial tokens
  while ( ! quitViewer ) {
    readYUV(width, height, yi, u, v);
    //printf("Read %dx%d frame #%3d\n", width, height, cnt); 
    difference(width, height, yi, yp, yr, yo);
    memcpy(yp, yr, height*width); // Current -> previous
    yuvDisplay(id, width, height, yo, u, v);
    cnt++;
    }
}
