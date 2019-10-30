#ifndef _YUV2PGM_H
#define _YUV2PGM_H

int writePgm(char *ofname, int width, int height, unsigned char maxv, unsigned char *data);
int writePpm(char *ofname, int width, int height,
             unsigned char maxv, unsigned char *y, unsigned char *u, unsigned char *v);
int yuv2pgm(char *ifname, int nb_frames, int width, int height);
int yuv2ppm(char *ifname, int nb_frames, int width, int height);

#endif
