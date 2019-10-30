/*
============================================================================
Name        : displayYUV.c
Author      : mpelcat & kdesnos & jheulot
Version     :
Copyright   : CECILL-C
Description : Displaying YUV frames one next to another in a row
Modified by jserot for HoCL on Oct 25, 2019 (dump frames in file only)
============================================================================
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "yuvDisplay.h"
#ifndef SYSTEMC_TARGET
#include "clock.h"
#include <SDL.h>
#include <SDL_ttf.h>
#endif

#ifdef SYSTEMC_TARGET
#include "yuv.h"
#else
#define FPS_MEAN 49
extern int stopThreads;
#endif

/**
* Structure representing one display
*/

#ifdef SYSTEMC_TARGET
typedef struct YuvDisplay
{
  int id;
  int width;
  int height;
  int current_frame;
} YuvDisplay;
#else
typedef struct YuvDisplay
{
	SDL_Texture* textures[NB_DISPLAY];	    // One overlay per frame
	SDL_Window *screen;					    // SDL surface where to display
	SDL_Renderer *renderer;
	TTF_Font *text_font;
	int currentXMin;						// Position for next display
	int initialized;                        // Initialization done ?
	int stampId;
} YuvDisplay;
#endif


// Initialize
static YuvDisplay display;

#ifndef SYSTEMC_TARGET
int exitCallBack(void* userdata, SDL_Event* event){
	if (event->type == SDL_QUIT){
		printf("Exit request from GUI.\n");
		stopThreads = 1;
		return 0;
	}
	return 1;
}
#endif

/**
* Initializes a display frame. Be careful, once a window size has been chosen,
* all videos must share the same window size
*
* @param id display unique identifier
* @param width width
* @param height heigth
*/
void yuvDisplayInit(int id, int width, int height)
{
#ifdef SYSTEMC_TARGET
  display.id = id;
  display.width = width;
  display.height = height;
  display.current_frame = 0;
#else
	if (display.initialized == 0)
	{
		display.currentXMin = 0;
	}
	if (height > DISPLAY_H)
	{
		fprintf(stderr, "SDL screen is not high enough for display %d.\n", id);
		exit(1);
	}
	else if (id >= NB_DISPLAY)
	{
		fprintf(stderr, "The number of displays is limited to %d.\n", NB_DISPLAY);
		exit(1);
	}
	else if (display.currentXMin + width > DISPLAY_W)
	{
		fprintf(stderr, "The number is not wide enough for display %d.\n", NB_DISPLAY);
		exit(1);
	}
#ifdef VERBOSE
	printf("SDL screen height OK, width OK, number of displays OK.\n");
#endif
	if (display.initialized == 0)
	{
		// Generating window name
		char* name = "Display";
		display.initialized = 1;
		printf("SDL_Init_Start\n");
		if (SDL_Init(SDL_INIT_VIDEO))
		{
			fprintf(stderr, "Could not initialize SDL - %s\n", SDL_GetError());
			exit(1);
		}
		printf("SDL_Init_end\n");
		/* Initialize SDL TTF for text display */
		if (TTF_Init())
		{
			printf("TTF initialization failed: %s\n", TTF_GetError());
		}
		printf("TTF_Init\n");
		/* Initialize Font for text display */
		display.text_font = TTF_OpenFont(PATH_TTF, 20);
		if (!display.text_font)
		{
			printf("TTF_OpenFont: %s\n", TTF_GetError());
		}
		display.screen = SDL_CreateWindow(name, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
			DISPLAY_W, DISPLAY_H, SDL_WINDOW_SHOWN);
		if (!display.screen)
		{
			fprintf(stderr, "SDL: could not set video mode - exiting\n");
			exit(1);
		}
		display.renderer = SDL_CreateRenderer(display.screen, -1, SDL_RENDERER_ACCELERATED);
		if (!display.renderer)
		{
			fprintf(stderr, "SDL: could not create renderer - exiting\n");
			exit(1);
		}
	}
	if (display.textures[id] == NULL)
	{
		display.textures[id] = SDL_CreateTexture(display.renderer,
			SDL_PIXELFORMAT_IYUV,
			SDL_TEXTUREACCESS_STREAMING,
			width, height);

		if (!display.textures[id])
		{
			fprintf(stderr, "SDL: could not create texture - exiting\n");
			exit(1);
		}
		display.currentXMin += width;
	}
	display.stampId = 0;
	for (int i = 0; i<FPS_MEAN; i++){
		startTiming(i + 1);
	}
	printf("register\n");
	SDL_SetEventFilter(exitCallBack, NULL);
#endif
}

void yuvDisplay(int id, int width, int height, unsigned char *y, unsigned char *u, unsigned char *v){
	yuvDisplayWithNbSlice(id, -1, y, u, v);
}

void yuvDisplayWithNbSlice(int id, int nbSlice, unsigned char *y, unsigned char *u, unsigned char *v)
{
#ifdef SYSTEMC_TARGET
  char ofname[64];
  sprintf(ofname, "Display%d_%d_y.pgm", display.id, display.current_frame);
  writePgm(ofname, display.width, display.height, 255, y);
#ifdef VERBOSE
  printf("Wrote file %s\n", ofname);
#endif
#else
	SDL_Texture* texture = display.textures[id];
	int w, h;
	// Retrieve texture attribute
	SDL_QueryTexture(texture, NULL, NULL, &w, &h);
	SDL_UpdateYUVTexture(
		texture, NULL,
		y, w,
		u, w / 2,
		v, w / 2
		);
	SDL_Rect screen_rect;
	screen_rect.w = w;
	screen_rect.h = h;
	screen_rect.x = w*id;
	screen_rect.y = 0;
	SDL_RenderCopy(display.renderer, texture, NULL, &screen_rect);
	/* Draw FPS text */
	char fps_text[20];
	SDL_Color colorWhite = { 255, 255, 255, 255 };
	int time = stopTiming(display.stampId + 1);
	sprintf(fps_text, "FPS: %.2f", 1. / (time / 1000000. / FPS_MEAN));
	startTiming(display.stampId + 1);
	display.stampId = (display.stampId + 1) % FPS_MEAN;
	SDL_Surface* fpsText = TTF_RenderText_Blended(display.text_font, fps_text, colorWhite);
	SDL_Texture* fpsTexture = SDL_CreateTextureFromSurface(display.renderer, fpsText);
	int fpsWidth, fpsHeight;
	SDL_QueryTexture(fpsTexture, NULL, NULL, &fpsWidth, &fpsHeight);
	SDL_Rect fpsTextRect;
	fpsTextRect.x = 0;
	fpsTextRect.y = 0;
	fpsTextRect.w = fpsWidth;
	fpsTextRect.h = fpsHeight;
	SDL_RenderCopy(display.renderer, fpsTexture, NULL, &fpsTextRect);
	/* Free resources */
	SDL_FreeSurface(fpsText);
	SDL_DestroyTexture(fpsTexture);
	/* Draw NbSlice Text */
	if (nbSlice > 0){
		char slice_text[20];
		sprintf(slice_text, "nbSlice: %d", nbSlice);
		SDL_Surface* sliceText = TTF_RenderText_Blended(display.text_font, slice_text, colorWhite);
		SDL_Texture* sliceTexture = SDL_CreateTextureFromSurface(display.renderer, sliceText);
		int sliceWidth, sliceHeight;
		SDL_QueryTexture(sliceTexture, NULL, NULL, &sliceWidth, &sliceHeight);
		SDL_Rect sliceTextRect;
		sliceTextRect.x = 0;
		sliceTextRect.y = fpsHeight;
		sliceTextRect.w = sliceWidth;
		sliceTextRect.h = sliceHeight;
		SDL_RenderCopy(display.renderer, sliceTexture, NULL, &sliceTextRect);
		/* Free resources */
		SDL_FreeSurface(sliceText);
		SDL_DestroyTexture(sliceTexture);
	}
	SDL_RenderPresent(display.renderer);
	SDL_Event event;
	// Grab all the events off the queue.
	while (SDL_PollEvent(&event))
	{
		switch (event.type)
		{
		default:
			break;
		}
	}
#endif
}

void yuvFinalize(int id)
{
#ifndef SYSTEMC_TARGET
	SDL_DestroyTexture(display.textures[id]);
	SDL_DestroyRenderer(display.renderer);
	SDL_DestroyWindow(display.screen);
#endif
}
