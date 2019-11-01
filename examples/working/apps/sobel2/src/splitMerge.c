/*
============================================================================
Name        : splitMerge.c
Author      : kdesnos
Modified by : jserot (draw a white line btw slices)
Version     : 1.2
Copyright   : CECILL-C
Description :
============================================================================
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "splitMerge.h"

void split(int nbSlice, int width, int height, unsigned char *input, unsigned char *output){
    printf("split: nbSlice=%d width=%d height=%d\n", nbSlice, width, height);
	if(output != NULL){
		int i;
		int sliceSize =  width*height/nbSlice;
		// Fill first and last line with 0
		memset(output,0,width);
		// First Slice
		memcpy(output+width, input, sliceSize);
		// Copy next line if several slice
		if (nbSlice > 1){
			memcpy(output +  width + sliceSize , input + sliceSize, width);
		}
		// Slice other than first and last
		for(i=1; i<nbSlice-1; i++){
			int destIndex = i*(sliceSize+2*width);
			memcpy(output + destIndex, input+i*sliceSize-width, sliceSize+2*width);
		}
		// Last Slice
		i = nbSlice-1;
		if(nbSlice > 1){
			// we have i = nbSlice -1;
			int destIndex = i*(sliceSize+2*width);
			memcpy(output + destIndex, input+i*sliceSize-width, sliceSize+width);
		}
		// Last line
		memset(output + (height+nbSlice*2-1)*width,0,width);
	} else {
		// Output has been splitted and is null
		// Fill first and last line with 0
		memset(input - width, 0, width);
		// Last line
		memset(input + height*width,0,width);
	}
    printf("split: done\n");
}


void merge(int nbSlice, int width, int height, unsigned char *input, unsigned char *output){
    int i;
    int sliceSize =  width*height/nbSlice;
    // Copy the slice content except the first and last lines
    for(i = 0; i< nbSlice; i++){
        int idx = i*(sliceSize+2*width);
        memcpy(output+i*sliceSize, input+idx+width, sliceSize);
    }
    for(i = 0; i< nbSlice; i++){
      memset(output+i*sliceSize, 255, width);
    }
}
