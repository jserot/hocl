#include <stdio.h>
#include <stdlib.h>
#include "yuvRead.h"
#include "yuvDisplay.h"
#include "sobel.h"

int main(int argc, char **argv)
{
  int width = WIDTH;
  int height = HEIGHT;
  int id = 0;
  int cnt = 0;
  unsigned char* yi = (unsigned char*)malloc(sizeof(char)*height*width);
  unsigned char* yo = (unsigned char*)malloc(sizeof(char)*height*width);
  unsigned char* u = (unsigned char*)malloc(sizeof(char)*height/2*width/2);
  unsigned char* v = (unsigned char*)malloc(sizeof(char)*height/2*width/2);
  initReadYUV(width, height);
  yuvDisplayInit(id, width, height);
  while ( ! quitViewer ) {
    readYUV(width, height, yi, u, v);
    //printf("Read %dx%d frame #%3d\n", width, height, cnt); 
    sobel(width, height, yi, yo);
    yuvDisplay(id, width, height, yo, u, v);
    cnt++;
    }
}
