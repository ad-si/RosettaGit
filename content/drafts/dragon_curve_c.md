+++
title = "Dragon curve/C"
description = ""
date = 2010-11-14T20:30:51Z
aliases = []
[extra]
id = 4993
[taxonomies]
categories = []
tags = []
+++

It uses an [[wp:L-system|L-system]] in order to generate the curve, the code from [[Raster graphics operations]] ("<tt>imglib.h</tt>") to produce the output in PPM format, and the code from [[Basic string manipulation functions]] (<tt>estrings.h</tt>) to manipulate strings keeping the ''evolution'' of the system.


```c>#include <stdio.h

#include <string.h>
#include "imglib.h"
#include "estrings.h"

#define WIDTH 600
#define HEIGHT 600

#define setStringFromCStr(S, SX) (void)setString((S), (SX), strlen((SX)))

void dragon_evolution(String s)
{
  size_t l;
  String t;
  String res;
  String Xr, Yr;

  res = newString();
  Xr = newString();
  Yr = newString();

  /* hard-encoded rules */
  setStringFromCStr(Xr, "X+YF+");
  setStringFromCStr(Yr, "-FX-Y");

  for(l=0; l < s->length; l++) {
    switch( s->bstring[l] ) {
    case 'X':
      t = joinStrings(res, Xr);
      (void)copyString(res, t);
      destroyString(t);
      break;
    case 'Y':
      t = joinStrings(res, Yr);
      (void)copyString(res, t);
      destroyString(t);
      break;
    default:
      (void)appendChar(res, s->bstring[l]);
      break;
    }
  }

  (void)copyString(s, res);
  destroyString(res);
  destroyString(Xr);
  destroyString(Yr);
}

#define DRAGON_FORWARD 2

int dirs[4][2] = {
  {1,0},{0,1},{-1,0},{0,-1},
};
typedef struct {
  int r, g, b;
} col_t;
#define NUM_COLS 6
col_t colors[NUM_COLS] = {
  { 0, 0, 255 },
  { 255, 0, 0 },
  { 0, 255, 0 },
  { 255, 255, 255 },
  { 255, 255, 0 },
  { 255, 0, 255 }
};
void interpret_dragon(image im, String s)
{
  int di = 1, i;
  /* handmade centering... */
  int x = 140, y = (HEIGHT >> 1) - 80;
  int nx, ny;
  int dr = 0;
  
  for(i=0; i < s->length; i++) {
    switch( s->bstring[i] ) {
    case '+':
      di = (di+1) % 4;
      break;
    case '-':
      di = (di+3) % 4;
      break;
    case 'F':
      nx = x + DRAGON_FORWARD * dirs[di][0];
      ny = y + DRAGON_FORWARD * dirs[di][1];
      if ( (nx >= 0) && (ny >= 0) &&
	   (nx <= WIDTH) && (ny <= HEIGHT) &&
	   (x >= 0) && ( y >= 0 ) &&
	   (x <= WIDTH) && (y <= HEIGHT)) {
	draw_line(im, x, y, 
		  nx, ny,
		  colors[dr].r, colors[dr].g, colors[dr].b);
	dr++; dr %= NUM_COLS;
      }
      x = nx;
      y = ny;
      break;
    }
  }
}

int main()
{
  image out;
  int i;
  String seed;

  out = alloc_img(WIDTH, HEIGHT);

  seed = newString();

  /* "seed" */
  setStringFromCStr(seed, "FX");

  for(i=0; i < 15; i++) dragon_evolution(seed);

  interpret_dragon(out, seed);

  output_ppm(stdout, out);
  free_img(out);
  destroyString(seed);
  return 0;
}
```


'''Note''': the color feature does not color "layers" of the run (like Algol); it just adds colors to the segments cyclically, making "noise" rather than delightful make up.
