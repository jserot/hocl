#include "yuv.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int writePgm(char *ofname, int width, int height, unsigned char maxv, unsigned char *data)
{
  int i, j;
  unsigned char v;
  FILE *fpw = fopen(ofname, "w");
  if ( fpw == NULL ) { fprintf(stderr, "writePgm: cannot open file %s for writting\n", ofname); return 1; }
  fprintf(fpw,"%s\n%4d%4d\n%4d\n", "P5", width, height, maxv);
  for ( i=0; i<height; i++ ) {
    for ( j=0; j<width; j++ ) {
      v = data[i*width+j];
      // fprintf(fpw, "%3d%s", v, (j+1)%width==0 ? "\n" : " "); // for P2 (ascii) fmt
      fputc(v, fpw); // P5 (binary) format
     }
    }
  fclose(fpw); 
  return 0;
}

#define CLAMP255(v) ((unsigned char)(v<0?0:(v>255?255:v)))

int writePpm(char *ofname, int width, int height,
             unsigned char maxv, unsigned char *y, unsigned char *u, unsigned char *v)
{
  int i, j;
  FILE *fpw = fopen(ofname, "w");
  int Y, U, V, R, G, B;
  if ( fpw == NULL ) { fprintf(stderr, "writePpm: cannot open file %s for writting\n", ofname); return 1; }
  fprintf(fpw,"%s\n%4d%4d\n%4d\n", "P6", width, height, maxv);
  for ( i=0; i<height; i++ ) {
    for ( j=0; j<width; j++ ) {
      Y = y[i*width+j];
      U = u[i/2*width/2+j/2]; // subsampling
      V = v[i/2*width/2+j/2]; // subsampling
      // YUV -> RGB conversion (from: https://www.fourcc.org/fccyvrgb.php)
      R = 1.164*(Y-16)+1.596*(V-128);
      G = 1.164*(Y-16)-0.813*(V-128)-0.391*(U-128);
      B = 1.164*(Y-16)+2.018*(U-128);
      fputc(CLAMP255(R), fpw);
      fputc(CLAMP255(G), fpw);
      fputc(CLAMP255(B), fpw);
      }
    }
  fclose(fpw); 
  return 0;
}

FILE* openFile(char *ifname, int width, int height, int *nb_frames) {
  int total_size, frame_size;
  FILE *fp;
  if ( (fp = fopen(ifname, "rb") ) == NULL ) {
    fprintf(stderr,"libyuv.openFile: error: cannot open file %s\n", ifname);
    return NULL;
  }
  fseek(fp, 0, SEEK_END);
  total_size = ftell(fp);
  frame_size = width*height + width*height/2; // full size for Y channel, half size for U and V channels
  *nb_frames = total_size / frame_size;
  if ( *nb_frames > 0 ) {
    rewind(fp);
    return fp;
    }
  else {
    fprintf(stderr,"libyuv.openFile: error: not enough data in file %s\n", ifname);
    return NULL;
    }
}

typedef enum { PGM, PPM } output_fmt;

int convert(char *ifname, int nframes, int width, int height, output_fmt fmt) {
  int actual_nframes, frame;
  FILE *fp;
  unsigned char *y;
  unsigned char *u;
  unsigned char *v;
  char ofname[64];
  fp = openFile(ifname, width, height, &actual_nframes);
  if ( fp == NULL ) exit(1);
  if ( actual_nframes < nframes ) {
    fprintf(stderr,"libyuv.yuv2pgm: error: not enough frames in file %s (supplied: %d, required: %d)\n",
            ifname, actual_nframes, nframes);
    exit(2);
  }
  frame = 0;
  y = (unsigned char*)malloc(width*height);
  u = (unsigned char*)malloc(width*height/4);
  v = (unsigned char*)malloc(width*height/4);
  if ( y == NULL || u == NULL || v == NULL ) {
    fprintf(stderr,"libyuv.yuv2pgm: error: cannot allocate buffer(s)");
    exit(3);
  }
  while ( frame < nframes ) {
    // if ( ftell(fp)/(width*height + width*height/2) >= nb_frames) rewind(fp);
    int res = fread(y, sizeof(char), width * height, fp);
    res += fread(u, sizeof(char), width * height / 4, fp);
    res += fread(v, sizeof(char), width * height / 4, fp);
    if ( res <= 0 ) {
      fprintf(stderr, "libyuv.yuv2pgm: error while reading data");
      exit(4);
    }
    switch ( fmt ) {
    case PGM:
      sprintf(ofname, "%s_%d_y.pgm", ifname, frame);
      writePgm(ofname, width, height, 255, y);
      sprintf(ofname, "%s_%d_u.pgm", ifname, frame);
      writePgm(ofname, width/2, height/2, 255, u);
      sprintf(ofname, "%s_%d_v.pgm", ifname, frame);
      writePgm(ofname, width/2, height/2, 255, v);
      break;
    case PPM:
      sprintf(ofname, "%s_%d.ppm", ifname, frame);
      writePpm(ofname, width, height, 255, y, u, v);
      break;
    }
    frame++;
  }
  return 0;
}

int yuv2pgm(char *ifname, int nframes, int width, int height) {
  return convert(ifname, nframes, width, height, PGM);
}
int yuv2ppm(char *ifname, int nframes, int width, int height) {
  return convert(ifname, nframes, width, height, PPM);
}
