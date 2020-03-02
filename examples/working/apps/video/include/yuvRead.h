/*
	============================================================================
	Name        : readYUV.h
	Author      : kdesnos & mpelcat
    Modified by : jserot (for use in the HoCL framework)
	Version     : 1.1
	Copyright   : CECILL-C
	Description : Actor code to read a file from the hard drive
	============================================================================
*/

#ifndef READ_YUV_H
#define READ_YUV_H

#include "hocl.h"
#define PATH "../../../../data/akiyo_cif.yuv"
#define NB_FRAME 300

/**
* Initialize the readYUV actor.
* Open the YUV file at the given PATH and check its size.
*
* @param width
*        The width of the opened YUV file
* @param height
*        The heigth of the opened YUV file
*/
void yuvReadInit(int width, int height);

/**
* Read a new frame from the YUV file.
*
* @param width
*        The width of the opened YUV file
* @param height
*        The heigth of the opened YUV file
* param y
*       Destination of the Y component read from the file
* param u
*       Destination of the U component read from the file
* param v
*       Destination of the V component read from the file
*/
void yuvRead(PARAM int width, PARAM int height, OUT unsigned char *y, OUT unsigned char *u, OUT unsigned char *v);

#endif
