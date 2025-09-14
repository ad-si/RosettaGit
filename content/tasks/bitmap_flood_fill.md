+++
title = "Bitmap/Flood fill"
description = ""
date = 2019-08-23T21:44:06Z
aliases = []
[extra]
id = 3562
[taxonomies]
categories = ["Raster graphics operations", "Graphics algorithms", "task"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "bbc_basic",
  "c",
  "cpp",
  "csharp",
  "d",
  "e",
  "erre",
  "euler_math_toolbox",
  "fbsl",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "hicest",
  "j",
  "java",
  "julia",
  "kotlin",
  "liberty_basic",
  "lingo",
  "perl",
  "phix",
  "picolisp",
  "pl_i",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "standard_ml",
  "tcl",
  "xpl0",
  "zkl",
]
+++

## Task
Implement a [[wp:flood fill|flood fill]].

A flood fill is a way of filling an area using ''color banks'' to define the contained area or a ''target color'' which "determines" the area (the ''valley'' that can be flooded; Wikipedia uses the term ''target color''). It works almost like a water flooding from a point towards the banks (or: inside the valley): if there's a hole in the banks, the flood is not contained and all the image (or all the "connected valleys") get filled.

To accomplish the task, you need to implement just one of the possible algorithms (examples are on [[wp:flood fill|Wikipedia]]). Variations on the ''theme'' are allowed (e.g. adding a tolerance parameter or argument for color-matching of the ''banks'' or ''target'' color).

[[Image:Unfilledcirc.png|128px|thumb|right]]
'''Testing''': the basic algorithm is not suitable for ''truecolor'' images; a possible test image is the one shown on the right box; you can try to fill the white area, or the black inner circle.



## Ada


```ada
procedure Flood_Fill
          (  Picture  : in out Image;
             From     : Point;
             Fill     : Pixel;
             Replace  : Pixel;
             Distance : Luminance := 20
          )  is
   function Diff (A, B : Luminance) return Luminance is
      pragma Inline (Diff);
   begin
      if A > B then
         return A - B;
      else
         return B - A;
      end if;
   end Diff;

   function "-" (A, B : Pixel) return Luminance is
      pragma Inline ("-");
   begin
      return Luminance'Max (Luminance'Max (Diff (A.R, B.R), Diff (A.G, B.G)), Diff (A.B, B.B));
   end "-";
   procedure Column (From : Point);
   procedure Row (From : Point);

   Visited : array (Picture'Range (1), Picture'Range (2)) of Boolean :=
      (others => (others => False));

   procedure Column (From : Point) is
      X1 : Positive := From.X;
      X2 : Positive := From.X;
   begin
      Visited (From.X, From.Y) := True;
      for X in reverse Picture'First (1)..From.X - 1 loop
         exit when Visited (X, From.Y);
         declare
            Color : Pixel renames Picture (X, From.Y);
         begin
            Visited (X, From.Y) := True;
            exit when Color - Replace > Distance;
            Color := Fill;
            X1    := X;
         end;
      end loop;
      for X in From.X + 1..Picture'Last (1) loop
         exit when Visited (X, From.Y);
         declare
            Color : Pixel renames Picture (X, From.Y);
         begin
            Visited (X, From.Y) := True;
            exit when Color - Replace > Distance;
            Color := Fill;
            X2    := X;
         end;
      end loop;
      for X in X1..From.X - 1 loop
         Row ((X, From.Y));
      end loop;
      for X in From.X + 1..X2 loop
         Row ((X, From.Y));
      end loop;
   end Column;

   procedure Row (From : Point) is
      Y1 : Positive := From.Y;
      Y2 : Positive := From.Y;
   begin
      Visited (From.X, From.Y) := True;
      for Y in reverse Picture'First (2)..From.Y - 1 loop
         exit when Visited (From.X, Y);
         declare
            Color : Pixel renames Picture (From.X, Y);
         begin
            Visited (From.X, Y) := True;
            exit when Color - Replace > Distance;
            Color := Fill;
            Y1    := Y;
         end;
      end loop;
      for Y in From.Y + 1..Picture'Last (2) loop
         exit when Visited (From.X, Y);
         declare
            Color : Pixel renames Picture (From.X, Y);
         begin
            Visited (From.X, Y) := True;
            exit when Color - Replace > Distance;
            Color := Fill;
            Y2    := Y;
         end;
      end loop;
      for Y in Y1..From.Y - 1 loop
         Column ((From.X, Y));
      end loop;
      for Y in From.Y + 1..Y2 loop
         Column ((From.X, Y));
      end loop;
   end Row;

   Color : Pixel renames Picture (From.X, From.Y);
begin
   if Color - Replace <= Distance then
      Visited (From.X, From.Y) := True;
      Color := Fill;
      Column (From);
   end if;
end Flood_Fill;
```

The procedure has the following parameters. ''Picture'' is the image to change. ''From'' is the point to start at. ''Fill'' is the color to fill with. ''Replace'' is the color to replace. ''Distance'' defines the range of color around ''Replace'' to replace as well. The distance is defined as a maximum of the differences of stimuli. The following code snippet reads the test file, fills the area between two circles red, and writes the result:

```ada
declare
   File : File_Type;
begin
   Open (File, In_File, "Unfilledcirc.ppm");
   declare
      Picture : Image := Get_PPM (File);
   begin
      Close (File);
      Flood_Fill
      (  Picture  => Picture,
         From     => (122, 30),
         Fill     => (255,0,0),
         Replace  => White
      );
      Create (File, Out_File, "Filledcirc.ppm");
      Put_PPM (File, Picture);
      Close (File);
   end;
end;
```



## AutoHotkey

* <code>x</code>, <code>y</code> are the initial coords (relative to screen unless the <code>relative</code> parameter is true).
* <code>target</code> is the BGR hex color code to replace.
* <code>replacement</code> is the BGR hex color code to replace <code>target</code> with.
* <code>mode</code> is 1 for a four-way fill, 2 for a five-way fill (hits each pixel doubly because each calls itself), 3 for an eight-way fill, or 4 for an eight-way fill that hits each pixel doubly because it calls itself double (default 1).
* <code>key</code> is a key to press to exit if the fill takes too long.


###  Recursive

This is limited to %StackSize% pixels.

```AutoHotkey
SetBatchLines, -1
CoordMode, Mouse
CoordMode, Pixel
CapsLock::
KeyWait, CapsLock
MouseGetPos, X, Y
PixelGetColor, color, X, Y
FloodFill(x, y, color, 0x000000, 1, "CapsLock")
MsgBox Done!
Return
FloodFill(x, y, target, replacement, mode=1, key="")
{
   If GetKeyState(key, "P")
      Return
   PixelGetColor, color, x, y
   If (color <> target || color = replacement || target = replacement)
      Return
   VarSetCapacity(Rect, 16, 0)
   NumPut(x, Rect, 0)
   NumPut(y, Rect, 4)
   NumPut(x+1, Rect, 8)
   NumPut(y+1, Rect, 12)
   hDC := DllCall("GetDC", UInt, 0)
   hBrush := DllCall("CreateSolidBrush", UInt, replacement)
   DllCall("FillRect", UInt, hDC, Str, Rect, UInt, hBrush)
   DllCall("ReleaseDC", UInt, 0, UInt, hDC)
   DllCall("DeleteObject", UInt, hBrush)
   FloodFill(x+1, y, target, replacement, mode, key)
   FloodFill(x-1, y, target, replacement, mode, key)
   FloodFill(x, y+1, target, replacement, mode, key)
   FloodFill(x, y-1, target, replacement, mode, key)
   If (mode = 2 || mode = 4)
      FloodFill(x, y, target, replacement, mode, key)
   If (Mode = 3 || mode = 4)
   {
      FloodFill(x+1, y+1, target, replacement, key)
      FloodFill(x-1, y+1, target, replacement, key)
      FloodFill(x+1, y-1, target, replacement, key)
      FloodFill(x-1, y-1, target, replacement, key)
   }
}
```



###  Iterative


```AutoHotkey
#NoEnv
#SingleInstance, Force

	SetBatchLines, -1
	CoordMode, Mouse
	CoordMode, Pixel
return

CapsLock::
	KeyWait, CapsLock
	MouseGetPos, X, Y
	PixelGetColor, color, X, Y
	FloodFill(x, y, color, 0x000000, 1, "Esc")
	MsgBox Done!
Return

FloodFill( 0x, 0y, target, replacement, mode=1, key="" )
{
	VarSetCapacity(Rect, 16, 0)
	hDC := DllCall("GetDC", UInt, 0)
	hBrush := DllCall("CreateSolidBrush", UInt, replacement)

	l := 0
	while l >= 0
	{
		if getkeystate(key, "P")
			return
		x := %l%x, y := %l%y
		%l%p++
		p := %l%p
		PixelGetColor, color, x, y
		if (color = target)
		{
			NumPut(x, Rect, 0)
			NumPut(y, Rect, 4)
			NumPut(x+1, Rect, 8)
			NumPut(y+1, Rect, 12)
			DllCall("FillRect", UInt, hDC, Str, Rect, UInt, hBrush)
		}
		else if (p = 1)
		{
			%l%x := %l%y := %l%p := "", l--
			continue
		}
		if (p < 5)
			ol := l++
			, %l%x := %ol%x + (p = 1 ? 1 : p = 2 ? -1 : 0)
			, %l%y := %ol%y + (p = 3 ? 1 : p = 4 ? -1 : 0)
		else
			%l%x := %l%y := %l%p := "", l--
	}

	DllCall("ReleaseDC", UInt, 0, UInt, hDC)
	DllCall("DeleteObject", UInt, hBrush)
}
```



## BBC BASIC

BBC BASIC has a built-in flood fill statement, but to satisfy the terms of the task it is not used in this example.

```bbcbasic
      MODE 8
      GCOL 15
      CIRCLE FILL 640, 512, 500
      GCOL 0
      CIRCLE FILL 500, 600, 200
      GCOL 3
      PROCflood(600, 200, 15)
      GCOL 4
      PROCflood(600, 700, 0)
      END

      DEF PROCflood(X%, Y%, C%)
      LOCAL L%, R%
      IF POINT(X%,Y%) <> C% ENDPROC
      L% = X%
      R% = X%
      WHILE POINT(L%-2,Y%) = C% : L% -= 2 : ENDWHILE
      WHILE POINT(R%+2,Y%) = C% : R% += 2 : ENDWHILE
      LINE L%,Y%,R%,Y%
      FOR X% = L% TO R% STEP 2
        PROCflood(X%, Y%+2, C%)
        PROCflood(X%, Y%-2, C%)
      NEXT
      ENDPROC
```



## C


### Simple and complete example in C89


```C
/*
 * RosettaCode: Bitmap/Flood fill, language C, dialects C89, C99, C11.
 *
 * This is an implementation of the recursive algorithm. For the sake of
 * simplicity, instead of reading files as JPEG, PNG, etc., the program
 * read and write Portable Bit Map (PBM) files in plain text format.
 * Portable Bit Map files can also be read and written with GNU GIMP.
 *
 * The program is just an example, so the image size is limited to 2048x2048,
 * the image can only be black and white, there is no run-time validation.
 *
 * Data is read from a standard input stream, the results are written to the
 * standard output file.
 *
 * In order for this program to work properly it is necessary to allocate
 * enough memory for the program stack. For example, in Microsoft Visual Studio,
 * the option /stack:134217728 declares a 128MB stack instead of the default
 * size of 1MB.
 */
#define _CRT_SECURE_NO_WARNINGS /* Unlock printf etc. in MSVC */
#include <stdio.h>
#include <stdlib.h>

#define MAXSIZE 2048
#define BYTE unsigned char

static int width, height;
static BYTE bitmap[MAXSIZE][MAXSIZE];
static BYTE oldColor;
static BYTE newColor;

void floodFill(int i, int j)
{
    if ( 0 <= i && i < height
    &&   0 <= j && j < width
    &&   bitmap[i][j] == oldColor )
    {
        bitmap[i][j] = newColor;
        floodFill(i-1,j);
        floodFill(i+1,j);
        floodFill(i,j-1);
        floodFill(i,j+1);
    }
}

/* *****************************************************************************
 * Input/output routines.
 */

void skipLine(FILE* file)
{
    while(!ferror(file) && !feof(file) && fgetc(file) != '\n')
        ;
}

void skipCommentLines(FILE* file)
{
    int c;
    int comment = '#';

    while ((c = fgetc(file)) == comment)
        skipLine(file);
    ungetc(c,file);
}

readPortableBitMap(FILE* file)
{
    int i,j;

    skipLine(file);
    skipCommentLines(file);  fscanf(file,"%d",&width);
    skipCommentLines(file);  fscanf(file,"%d",&height);
    skipCommentLines(file);

    if ( width <= MAXSIZE && height <= MAXSIZE )
        for ( i = 0; i < height; i++ )
            for ( j = 0; j < width; j++ )
                fscanf(file,"%1d",&(bitmap[i][j]));
    else exit(EXIT_FAILURE);
}

void writePortableBitMap(FILE* file)
{
    int i,j;
    fprintf(file,"P1\n");
    fprintf(file,"%d %d\n", width, height);
    for ( i = 0; i < height; i++ )
    {
        for ( j = 0; j < width; j++ )
            fprintf(file,"%1d", bitmap[i][j]);
        fprintf(file,"\n");
    }
}

/* *****************************************************************************
 * The main entry point.
 */

int main(void)
{
    oldColor = 1;
    newColor = oldColor ? 0 : 1;
    readPortableBitMap(stdin);
    floodFill(height/2,width/2);
    writePortableBitMap(stdout);
    return EXIT_SUCCESS;
}
```



### Second example


```c

// http://commons.wikimedia.org/wiki/File:Julia_immediate_basin_1_3.png

unsigned int f(unsigned int _iX, unsigned int _iY)
/*
   gives position of point (iX,iY) in 1D array  ; uses also global variables
   it does not check if index is good  so memory error is possible
*/
{return (_iX + (iYmax-_iY-1)*iXmax );}


int FillContour(int iXseed, int iYseed,  unsigned char color, unsigned char _data[])
{
  /*
     fills contour with black border ( color = iJulia)  using seed point inside contour
     and horizontal lines
     it starts from seed point, saves max right( iXmaxLocal) and max left ( iXminLocal) interior points of horizontal line,
     in new line ( iY+1 or iY-1) it computes new interior point  : iXmidLocal=iXminLocal + (iXmaxLocal-iXminLocal)/2;
     result is stored in _data array : 1D array of 1-bit colors ( shades of gray)
     it does not check if index of _data array is good  so memory error is possible
  */


  int iX, /* seed integer coordinate */
    iY=iYseed,
    /* most interior point of line iY */
    iXmidLocal=iXseed,
    /* min and max of interior points of horizontal line iY */
    iXminLocal,
    iXmaxLocal;
  int i ; /* index of _data array */;


  /* ---------  move up --------------- */
  do{
    iX=iXmidLocal;
    i =f(iX,iY); /* index of _data array */;

    /* move to right */
    while (_data[i]==iInterior)
      { _data[i]=color;
        iX+=1;
        i=f(iX,iY);
      }
    iXmaxLocal=iX-1;

    /* move to left */
    iX=iXmidLocal-1;
    i=f(iX,iY);
    while (_data[i]==iInterior)
      { _data[i]=color;
        iX-=1;
        i=f(iX,iY);
      }
    iXminLocal=iX+1;


    iY+=1; /* move up */
    iXmidLocal=iXminLocal + (iXmaxLocal-iXminLocal)/2; /* new iX inside contour */
    i=f(iXmidLocal,iY); /* index of _data array */;
    if ( _data[i]==iJulia)  break; /*  it should not cross the border */

  } while  (iY<iYmax);


  /* ------  move down ----------------- */
  iXmidLocal=iXseed;
  iY=iYseed-1;


  do{
    iX=iXmidLocal;
    i =f(iX,iY); /* index of _data array */;

    /* move to right */
    while (_data[i]==iInterior) /*  */
      { _data[i]=color;
        iX+=1;
        i=f(iX,iY);
      }
    iXmaxLocal=iX-1;

    /* move to left */
    iX=iXmidLocal-1;
    i=f(iX,iY);
    while (_data[i]==iInterior) /*  */
      { _data[i]=color;
        iX-=1; /* move to right */
        i=f(iX,iY);
      }
    iXminLocal=iX+1;

    iY-=1; /* move down */
    iXmidLocal=iXminLocal + (iXmaxLocal-iXminLocal)/2; /* new iX inside contour */
    i=f(iXmidLocal,iY); /* index of _data array */;
    if ( _data[i]==iJulia)  break; /*  it should not cross the border */
  } while  (0<iY);

  /* mark seed point by big pixel */
  const int iSide =iXmax/500; /* half of width or height of big pixel */
  for(iY=iYseed-iSide;iY<=iYseed+iSide;++iY){
    for(iX=iXseed-iSide;iX<=iXseed+iSide;++iX){
      i= f(iX,iY); /* index of _data array */
      _data[i]=10;}}

  return 0;
}


```



### Third example

{{improve|C|Very difficult to make it work, and still doesn't work correctly after that. Needs to be replaced with something sensible.}}

The <code>sys/queue.h</code> is not POSIX. (See [[FIFO#C|FIFO]])


```c
/* #include <sys/queue.h> */
typedef struct {
  color_component red, green, blue;
} rgb_color;
typedef rgb_color *rgb_color_p;

void floodfill(image img, int px, int py,
	       rgb_color_p bankscolor,
	       rgb_color_p rcolor);
```



```c
#include "imglib.h"

typedef struct _ffill_node {
  int px, py;
  TAILQ_ENTRY(_ffill_node) nodes;
} _ffill_node_t;
TAILQ_HEAD(_ffill_queue_s, _ffill_node);
typedef struct _ffill_queue_s _ffill_queue;

inline void _ffill_removehead(_ffill_queue *q)
{
  _ffill_node_t *n = q->tqh_first;
  if ( n != NULL ) {
    TAILQ_REMOVE(q, n, nodes);
    free(n);
  }
}

inline void _ffill_enqueue(_ffill_queue *q, int px, int py)
{
  _ffill_node_t *node;
  node = malloc(sizeof(_ffill_node_t));
  if ( node != NULL ) {
    node->px = px; node->py = py;
    TAILQ_INSERT_TAIL(q, node, nodes);
  }
}

inline double color_distance( rgb_color_p a, rgb_color_p b )
{
  return sqrt( (double)(a->red - b->red)*(a->red - b->red) +
	       (double)(a->green - b->green)*(a->green - b->green) +
	       (double)(a->blue - b->blue)*(a->blue - b->blue) ) / (256.0*sqrt(3.0));
}

inline void _ffill_rgbcolor(image img, rgb_color_p tc, int px, int py)
{
  tc->red = GET_PIXEL(img, px, py)[0];
  tc->green = GET_PIXEL(img, px, py)[1];
  tc->blue = GET_PIXEL(img, px, py)[2];
}


#define NSOE(X,Y) do {							\
    if ( ((X)>=0)&&((Y)>=0) && ((X)<img->width)&&((Y)<img->height)) {	\
      _ffill_rgbcolor(img, &thisnode, (X), (Y));			\
      if ( color_distance(&thisnode, bankscolor) > tolerance ) {	\
	if (color_distance(&thisnode, rcolor) > 0.0) { 			\
	  put_pixel_unsafe(img, (X), (Y), rcolor->red,			\
			   rcolor->green,				\
			   rcolor->blue);				\
	  _ffill_enqueue(&head, (X), (Y));				\
	  pixelcount++;							\
	}								\
      }									\
    }									\
  } while(0)


unsigned int floodfill(image img, int px, int py,
	       rgb_color_p bankscolor,
	       rgb_color_p rcolor)
{
  _ffill_queue head;
  rgb_color thisnode;
  unsigned int pixelcount = 0;
  double tolerance = 0.05;

  if ( (px < 0) || (py < 0) || (px >= img->width) || (py >= img->height) )
    return;

  TAILQ_INIT(&head);

  _ffill_rgbcolor(img, &thisnode, px, py);
  if ( color_distance(&thisnode, bankscolor) <= tolerance ) return;

  _ffill_enqueue(&head, px, py);
  while( head.tqh_first != NULL ) {
    _ffill_node_t *n = head.tqh_first;
    _ffill_rgbcolor(img, &thisnode, n->px, n->py);
    if ( color_distance(&thisnode, bankscolor) > tolerance ) {
      put_pixel_unsafe(img, n->px, n->py, rcolor->red, rcolor->green, rcolor->blue);
      pixelcount++;
    }
    int tx = n->px, ty = n->py;
    _ffill_removehead(&head);
    NSOE(tx - 1, ty);
    NSOE(tx + 1, ty);
    NSOE(tx, ty - 1);
    NSOE(tx, ty + 1);
  }
  return pixelcount;
}
```


The '''pixelcount''' could be used to know the area of the filled region. The ''internal'' parameter <code>tolerance</code> can be tuned to cope with antialiasing, bringing "sharper" resuts.


### =Usage example=


(Comments show changes to fill the white area instead of the black circle)


```c
#include <stdio.h>
#include <stdlib.h>
#include "imglib.h"

int main(int argc, char **argv)
{
  image animage;
  rgb_color ic;
  rgb_color rc;

  if ( argc > 1 ) {
    animage = read_image(argv[1]);
    if ( animage != NULL ) {
      ic.red = 255; /* = 0; */
      ic.green = 255; /* = 0; */
      ic.blue = 255; /* = 0; */
      rc.red = 0;
      rc.green = 255;
      rc.blue = 0;
      floodfill(animage, 100, 100, &ic, &rc);
                   /*    150, 150 */
      print_jpg(animage, 90);
      free(animage);
    }
  }
  return 0;
}
```



## C++

{{libheader|OpenCV}}

Input is the image, the starting node (x, y), the target color we want to fill, and the replacement color that will replace the target color. It implements a 4-way flood fill algorithm.

'''Interface'''

```cpp
#ifndef PROCESSING_FLOODFILLALGORITHM_H_
#define PROCESSING_FLOODFILLALGORITHM_H_

#include <opencv2/opencv.hpp>
#include <string.h>
#include <queue>

using namespace cv;
using namespace std;

class FloodFillAlgorithm {
public:
    FloodFillAlgorithm(Mat* image) :
        image(image) {
    }
    virtual ~FloodFillAlgorithm();

    void flood(Point startPoint, Scalar tgtColor, Scalar loDiff);
    void flood(Point startPoint, Mat* tgtMat);

protected:
    Mat* image;
private:
    bool insideImage(Point p);
};

#endif /* PROCESSING_FLOODFILLALGORITHM_H_ */

```

'''Implementation'''

```cpp
#include "FloodFillAlgorithm.h"

FloodFillAlgorithm::~FloodFillAlgorithm() {
}

void FloodFillAlgorithm::flood(Point startPoint, Scalar tgtColor, Scalar loDiff) {
    floodFill(*image, startPoint, tgtColor, 0, loDiff);
}

void FloodFillAlgorithm::flood(Point startPoint, Mat* tgtMat) {
    if (!insideImage(startPoint))
        return;

    Vec3b srcColor = image->at<Vec3b>(startPoint);

    if (image->at<Vec3b>(startPoint) == srcColor) {

        queue<Point> pointQueue;
	pointQueue.push(startPoint);

	while (!pointQueue.empty()) {
	    Point p = pointQueue.front();
	    pointQueue.pop();

	    if (insideImage(p)) {

		if ((image->at<Vec3b>(p) == srcColor)) {
		    image->at<Vec3b>(p) = tgtMat->at<Vec3b>(p);

		    pointQueue.push(Point(p.x + 1, p.y));
		    pointQueue.push(Point(p.x - 1, p.y));
		    pointQueue.push(Point(p.x, p.y + 1));
		    pointQueue.push(Point(p.x, p.y - 1));
		}
	    }

	}
    }
}

bool FloodFillAlgorithm::insideImage(Point p) {
    return (p.x >= 0) && (p.x < image->size().width) && (p.y >= 0) && (p.y < image->size().height);
}


```


## C#
{{works with|C#|3.0}}
{{libheader|System.Drawing}}

This implementation matches exact colours only. Since the example image has grey pixels around the edges of the circles, these will remain grey after the interiors are filled.


```c#

using System;
using System.Collections.Generic;
using System.Drawing;

namespace FloodFill
{
    class Program
    {
        private static bool ColorMatch(Color a, Color b)
        {
            return (a.ToArgb() & 0xffffff) == (b.ToArgb() & 0xffffff);
        }

        static void FloodFill(Bitmap bmp, Point pt, Color targetColor, Color replacementColor)
        {
            Queue<Point> q = new Queue<Point>();
            q.Enqueue(pt);
            while (q.Count > 0)
            {
                Point n = q.Dequeue();
                if (!ColorMatch(bmp.GetPixel(n.X, n.Y),targetColor))
                    continue;
                Point w = n, e = new Point(n.X + 1, n.Y);
                while ((w.X >= 0) && ColorMatch(bmp.GetPixel(w.X, w.Y),targetColor))
                {
                    bmp.SetPixel(w.X, w.Y, replacementColor);
                    if ((w.Y > 0) && ColorMatch(bmp.GetPixel(w.X, w.Y - 1),targetColor))
                        q.Enqueue(new Point(w.X, w.Y - 1));
                    if ((w.Y < bmp.Height - 1) && ColorMatch(bmp.GetPixel(w.X, w.Y + 1),targetColor))
                        q.Enqueue(new Point(w.X, w.Y + 1));
                    w.X--;
                }
                while ((e.X <= bmp.Width - 1) && ColorMatch(bmp.GetPixel(e.X, e.Y),targetColor))
                {
                    bmp.SetPixel(e.X, e.Y, replacementColor);
                    if ((e.Y > 0) && ColorMatch(bmp.GetPixel(e.X, e.Y - 1), targetColor))
                        q.Enqueue(new Point(e.X, e.Y - 1));
                    if ((e.Y < bmp.Height - 1) && ColorMatch(bmp.GetPixel(e.X, e.Y + 1), targetColor))
                        q.Enqueue(new Point(e.X, e.Y + 1));
                    e.X++;
                }
            }
        }

        static void Main(string[] args)
        {
            Bitmap bmp = new Bitmap("Unfilledcirc.bmp");
            FloodFill(bmp, new Point(200, 200), Color.White, Color.Red);
            FloodFill(bmp, new Point(100, 100), Color.Black, Color.Blue);
            bmp.Save("Filledcirc.bmp");
        }
    }
}

```



## D

This version uses the bitmap module from the Bitmap Task, matches exact colours only, and is derived from the Go version (to avoid stack overflow  because unlike Go the D stack is not segmented).


```d
import std.array, bitmap;

void floodFill(Color)(Image!Color img, in uint x, in uint y,
                      in Color color)
/*pure*/ nothrow in {
    assert (y < img.ny && x < img.nx);
} body {
    immutable target = img[x, y];
    static struct Pos { uint x, y; }
    auto stack = [Pos(x, y)];

    while (!stack.empty) {
        immutable p = stack.back;
        stack.popBack;
        if (p.y < img.ny && p.x < img.nx && img[p.x, p.y] == target) {
            img[p.x, p.y] = color;
            stack.assumeSafeAppend;
            stack ~= [Pos(p.x,     p.y + 1), Pos(p.x,     p.y - 1),
                      Pos(p.x + 1, p.y),     Pos(p.x - 1, p.y)];
        }
    }
}

void main() {
    Image!RGB img;
    loadPPM6(img, "unfilled_circ.ppm");
    img.floodFill(200, 200, RGB(127, 0, 0));
    img.savePPM6("unfilled_circ_flooded.ppm");
}
```



## E


Using the image type from [[Basic bitmap storage#E]].


```e
def floodFill(image, x, y, newColor) {
  def matchColor := image[x, y]
  def w := image.width()
  def h := image.height()

  /** For any given pixel x,y, this algorithm first fills a contiguous
      horizontal line segment of pixels containing that pixel, then
      recursively scans the two adjacent rows over the same horizontal
      interval. Let this be invocation 0, and the immediate recursive
      invocations be 1, 2, 3, ..., # be pixels of the wrong color, and
      * be where each scan starts; the fill ordering is as follows:

      --------------##########-------
      -...1111111111*11####*33333...-
      ###########000*000000000000...-
      -...2222222222*22222##*4444...-
      --------------------##---------

      Each invocation returns the x coordinate of the rightmost pixel it filled,
      or x0 if none were.

      Since it is recursive, this algorithm is unsuitable for large images
      with small stacks.
    */
  def fillScan(var x0, y) {
    if (y >= 0 && y < h && x0 >= 0 && x0 < w) {
      image[x0, y] := newColor
      var x1 := x0

      # Fill rightward
      while (x1 < w - 1 && image.test(x1 + 1, y, matchColor)) {
        x1 += 1
        image[x1, y] := newColor # This could be replaced with a horizontal-line drawing operation
      }

      # Fill leftward
      while (x0 > 0 && image.test(x0 - 1, y, matchColor)) {
        x0 -= 1
        image[x0, y] := newColor
      }

      if (x0 > x1) { return x0 } # Filled at most center

      # x0..x1 is now a run of newly-filled pixels.

      # println(`Filled $y $x0..$x1`)
      # println(image)

      # Scan the lines above and below
      for ynext in [y - 1, y + 1] {
        if (ynext >= 0 && ynext < h) {
          var x := x0
          while (x <= x1) {
            if (image.test(x, ynext, matchColor)) {
              x := fillScan(x, ynext)
            }
            x += 1
          }
        }
      }

      return x1
    } else {
      return x0
    }
  }

  fillScan(x, y)
}
```


[[File:Filledcirc-E.png|128px|thumb|right|Filled sample image]]Note that this does not make any attempt to smoothly fill 'banks' or have a tolerance; it matches exact colors only. This will fill the example image with red inside green, and there will be black/white fringes:


```e
{
  println("Read")
  def i := readPPM(<import:java.io.makeFileInputStream>(<file:Unfilledcirc.ppm>))
  println("Fill 1")
  floodFill(i, 100, 100, makeColor.fromFloat(1, 0, 0))
  println("Fill 2")
  floodFill(i, 200, 200, makeColor.fromFloat(0, 1, 0))
  println("Write")
  i.writePPM(<import:java.io.makeFileOutputStream>(<file:Filledcirc.ppm>))
  println("Done")
}
```



## ERRE

In "PC.LIB" library there is a FILL procedure that do the job, but the example program implements the algorithm in ERRE language using an iterative method. This program is taken from the distribution disk and works in 320x200 graphics.

```ERRE

PROGRAM MYFILL_DEMO

!VAR SP%

!$INTEGER

CONST IMAGE_WIDTH=320,IMAGE_HEIGHT=200

DIM STACK[6000,1]

FUNCTION QUEUE_COUNT(X)
   QUEUE_COUNT=SP
END FUNCTION

!$INCLUDE="PC.LIB"

PROCEDURE QUEUE_INIT
   SP=0
END PROCEDURE

PROCEDURE QUEUE_POP(->XX,YY)
    XX=STACK[SP,0]
    YY=STACK[SP,1]
    SP=SP-1
END PROCEDURE

PROCEDURE QUEUE_PUSH(XX,YY)
   SP=SP+1
   STACK[SP,0]=XX
   STACK[SP,1]=YY
END PROCEDURE

PROCEDURE FLOOD_FILL(XSTART,YSTART,COLORE_PRIMA,COLORE_RIEMP)
  LOCAL XEST,XWEST,YNORD,YSUD,X,Y
  QUEUE_INIT
  QUEUE_PUSH(XSTART,YSTART)
  WHILE (QUEUE_COUNT(0)>0) DO
    QUEUE_POP(->X,Y)
    XWEST=X
    XEST=X

    IF Y>0 THEN
      YNORD=Y-1
    ELSE
      YNORD=-1
    END IF

    IF Y<IMAGE_HEIGHT-1 THEN
      YSUD=Y+1
    ELSE
      YSUD=-1
    END IF

    LOOP
       POINT(XEST+1,Y->ZC%)
       EXIT IF NOT((XEST<IMAGE_WIDTH-1) AND (ZC%=COLORE_PRIMA))
       XEST=XEST+1
    END LOOP

    LOOP
       POINT(XWEST-1,Y->ZC%)
       EXIT IF NOT((XWEST>0) AND (ZC%=COLORE_PRIMA))
       XWEST=XWEST-1
    END LOOP

    FOR X=XWEST TO XEST DO
      PSET(X,Y,COLORE_RIEMP)
      POINT(X,YNORD->ZC%)
      IF YNORD>=0 AND ZC%=COLORE_PRIMA THEN
          QUEUE_PUSH(X,YNORD)
      END IF
      POINT(X,YSUD->ZC%)
      IF YSUD>=0 AND ZC%=COLORE_PRIMA THEN
          QUEUE_PUSH(X,YSUD)
      END IF
    END FOR
  END WHILE
END PROCEDURE ! Flood_Fill

BEGIN
   SCREEN(1)
   CIRCLE(100,100,75,2)
   CIRCLE(120,120,20,2)
   CIRCLE(80,80,15,2)
   CIRCLE(120,80,10,2)
   FLOOD_FILL(100,100,0,1)
END PROGRAM

```

Note: I haven't an "Upload files" item, so I can't show the resulting image!


## Euler Math Toolbox


Using an emulated stack. EMT's recursive stack space is limited. For the notebook with images see [http://www.euler-math-toolbox.de/renegrothmann/Flood%20Fill.html this page].

<lang>
>file="test.png";
>A=loadrgb(file); ...
>insrgb(A);
>function floodfill (A0,i,j,color,dist) ...
$  A=A0;
$  R=getred(A); G=getgreen(A); B=getblue(A);
$  d=sqrt((R-R[i,j])^2+(G-G[i,j])^2+(B-B[i,j])^2);
$  n=rows(A); m=cols(A);
$  V=zeros(n,m);
$  S=zeros(n*m,2); sn=0;
$  A[i,j]=color; V[i,j]=1;
$  repeat;
$    if fill(A,i+1,j,n,m,d,dist,V,S,sn,color) then i=i+1; continue;
$    elseif fill(A,i,j+1,n,m,d,dist,V,S,sn,color) then j=j+1; continue;
$    elseif fill(A,i-1,j,n,m,d,dist,V,S,sn,color) then i=i-1; continue;
$    elseif fill(A,i,j-1,n,m,d,dist,V,S,sn,color) then j=j-1; continue;
$    endif;
$    sn=sn-1; if sn==0 then break; endif;
$    i=S[sn,1]; j=S[sn,2];
$  end;
$  return A;
$endfunction
>function fill (A,i,j,n,m,d,dist,V,S,%sn,color) ...
$  if i<1 or i>n or j<1 or j>n then return 0; endif;
$  if V[i,j] || d[i,j]>dist then A[i,j]=color; return 0; endif;
$  V[i,j]=1;
$  A[i,j]=color;
$  %sn=%sn+1;
$  S[%sn]=[i,j];
$  return 1;
$endfunction
>B=floodfill(A,10,240,rgb(0.5,0,0),0.5);
>B=floodfill(B,10,10,rgb(0.5,0,0),0.5);
>B=floodfill(B,150,60,rgb(0,0.5,0),0.5);
>B=floodfill(B,200,200,rgb(0,0,0.5),0.5);
>insrgb(B);

```



## FBSL

'''Using pure FBSL's built-in graphics functions:'''

```qbasic
#DEFINE WM_LBUTTONDOWN 513
#DEFINE WM_CLOSE 16

FBSLSETTEXT(ME, "Before Flood Fill") ' Set form caption
FBSLSETFORMCOLOR(ME, RGB(0, 255, 255)) ' Cyan: persistent background color
FBSL.GETDC(ME) ' Use volatile FBSL.GETDC below to avoid extra assignments

RESIZE(ME, 0, 0, 220, 220)
CENTER(ME)
SHOW(ME)

DIM Breadth AS INTEGER, Height AS INTEGER
FBSL.GETCLIENTRECT(ME, 0, 0, Breadth, Height)

DrawCircles() ' Initialize circles

BEGIN EVENTS
	SELECT CASE CBMSG
		CASE WM_LBUTTONDOWN: FillCircles() ' Flood fill circles
		CASE WM_CLOSE: FBSL.RELEASEDC(ME, FBSL.GETDC) ' Clean up
	END SELECT
END EVENTS

SUB FillCircles()
	FILL(FBSL.GETDC, Breadth / 2, Height / 2, &HFFFF) ' Yellow: flood fill using intrinsics
	FOR DIM x = 0 TO Breadth / 2 ' Red: flood fill iteratively
		FOR DIM y = 0 TO Height / 2
			IF NOT POINT(FBSL.GETDC, x, y) THEN PSET(FBSL.GETDC, x, y, &HFF)
		NEXT
	NEXT
	FBSLSETTEXT(ME, "After Flood Fill") ' Reset form caption
END SUB

SUB DrawCircles() ' Concatenate function calls
	CIRCLE(FBSL.GETDC, Breadth / 2, Height / 2, 85, &HFFFFFF, 0, 360, 1, TRUE) _ ' White
	(FBSL.GETDC, Breadth / 3, Height / 3, 30, 0, 0, 360, 1, TRUE) ' Black
END SUB
```

'''Output:'''   [[File:FBSLFlood.PNG]]


## Forth

This simple recursive algorithm uses routines from [[Basic bitmap storage]].

```forth
: third 2 pick ;
: 3dup  third third third ;
: 4dup  2over 2over ;

: flood ( color x y bmp -- )
  3dup b@ >r  ( R: color to fill )
  4dup b!
  third 0 > if
    rot 1- -rot
    3dup b@ r@ = if recurse then
    rot 1+ -rot
  then
  third 1+ over bwidth < if
    rot 1+ -rot
    3dup b@ r@ = if recurse then
    rot 1- -rot
  then
  over 0 > if
    swap 1- swap
    3dup b@ r@ = if recurse then
    swap 1+ swap
  then
  over 1+ over bheight < if
    swap 1+ swap
    3dup b@ r@ = if recurse then
    swap 1- swap
  then
  r> drop ;
```



## Fortran

{{works with|Fortran|90 and later}}

Here the ''target color'' paradigm is used. Again the <code>matchdistance</code> parameter can be tuned to ignore small differences that could come because of antialiasing.


```fortran
module RCImageArea
  use RCImageBasic
  use RCImagePrimitive
  implicit none

  real, parameter, private :: matchdistance = 0.2

  private :: northsouth, eastwest

contains

  subroutine northsouth(img, p0, tcolor, fcolor)
    type(rgbimage), intent(inout) :: img
    type(point), intent(in) :: p0
    type(rgb), intent(in) :: tcolor, fcolor

    integer :: npy, spy, y
    type(rgb) :: pc

    npy = p0%y - 1
    do
       if ( inside_image(img, p0%x, npy) ) then
          call get_pixel(img, p0%x, npy, pc)
          if ( ((pc .dist. tcolor) > matchdistance ) .or. ( pc == fcolor ) ) exit
       else
          exit
       end if
       npy = npy - 1
    end do
    npy = npy + 1
    spy = p0%y + 1
    do
       if ( inside_image(img, p0%x, spy) ) then
          call get_pixel(img, p0%x, spy, pc)
          if ( ((pc .dist. tcolor) > matchdistance ) .or. ( pc == fcolor ) ) exit
       else
          exit
       end if
       spy = spy + 1
    end do
    spy = spy - 1
    call draw_line(img, point(p0%x, spy), point(p0%x, npy), fcolor)

    do y = min(spy, npy), max(spy, npy)
       if ( y == p0%y ) cycle
       call eastwest(img, point(p0%x, y), tcolor, fcolor)
    end do

  end subroutine northsouth


  subroutine eastwest(img, p0, tcolor, fcolor)
    type(rgbimage), intent(inout) :: img
    type(point), intent(in) :: p0
    type(rgb), intent(in) :: tcolor, fcolor

    integer :: npx, spx, x
    type(rgb) :: pc

    npx = p0%x - 1
    do
       if ( inside_image(img, npx, p0%y) ) then
          call get_pixel(img, npx, p0%y, pc)
          if ( ((pc .dist. tcolor) > matchdistance ) .or. ( pc == fcolor ) ) exit
       else
          exit
       end if
       npx = npx - 1
    end do
    npx = npx + 1
    spx = p0%x + 1
    do
       if ( inside_image(img, spx, p0%y) ) then
          call get_pixel(img, spx, p0%y, pc)
          if ( ((pc .dist. tcolor) > matchdistance ) .or. ( pc == fcolor ) ) exit
       else
          exit
       end if
       spx = spx + 1
    end do
    spx = spx - 1
    call draw_line(img, point(spx, p0%y), point(npx, p0%y), fcolor)

    do x = min(spx, npx), max(spx, npx)
       if ( x == p0%x ) cycle
       call northsouth(img, point(x, p0%y), tcolor, fcolor)
    end do

  end subroutine eastwest

  subroutine floodfill(img, p0, tcolor, fcolor)
    type(rgbimage), intent(inout) :: img
    type(point), intent(in) :: p0
    type(rgb), intent(in) :: tcolor, fcolor

    type(rgb) :: pcolor

    if ( .not. inside_image(img, p0%x, p0%y) ) return
    call get_pixel(img, p0%x, p0%y, pcolor)
    if ( (pcolor .dist. tcolor) > matchdistance ) return

    call northsouth(img, p0, tcolor, fcolor)
    call eastwest(img, p0, tcolor, fcolor)
  end subroutine floodfill

end module RCImageArea
```


Usage example excerpt (which on the test image will fill with green the inner black circle):


```fortran
  call floodfill(animage, point(100,100), rgb(0,0,0), rgb(0,255,0))
```


## FreeBASIC

{{trans|BBC BASIC}}

```freebasic
' version 04-11-2016
' compile with: fbc -s console

' the flood_fill needs to know the boundries of the window/screen
' without them the routine start to check outside the window
' this leads to crashes (out of stack)
' the Line routine has clipping it will not draw outside the window

Sub flood_fill(x As Integer, y As Integer, target As UInteger, fill_color As UInteger)

    Dim As Long x_max, y_max
    ScreenInfo x_max, y_max

    ' 0, 0 is top left corner
    If Point(x,y) <> target Then Exit Sub

    Dim As Long l = x, r = x

    While Point(l -1, y) = target AndAlso l -1 > -1
        l = l -1
    Wend

    While Point(r +1, y) = target AndAlso r +1 < x_max
        r = r +1
    Wend

    Line (l,y) - (r,y), fill_color

    For x = l To r
        If y +1 < y_max Then flood_fill(x, y +1, target, fill_color)
        If y -1 >    -1 Then flood_fill(x, y -1, target, fill_color)
    Next

End Sub

' ------=< MAIN >=------

Dim As ULong i, col, x, y

ScreenRes 400, 400, 32
Randomize Timer

For i As ULong = 1 To 5
    Circle(Rnd * 400 ,Rnd * 400), i * 40, Rnd * &hFFFFFF
Next

' hit a key to end or close window
Do
    x = Rnd * 400
    y = Rnd * 400
    col = Point(x, y)
    flood_fill(x, y, col, Rnd * &hFFFFFF )
    Sleep 2000
    If InKey <> "" OrElse InKey = Chr(255) + "k" Then End
Loop
```



## Go

An addition to code from the bitmap task:

```go
package raster

func (b *Bitmap) Flood(x, y int, repl Pixel) {
    targ, _ := b.GetPx(x, y)
    var ff func(x, y int)
    ff = func(x, y int) {
        p, ok := b.GetPx(x, y)
        if ok && p.R == targ.R && p.G == targ.G && p.B == targ.B {
            b.SetPx(x, y, repl)
            ff(x-1, y)
            ff(x+1, y)
            ff(x, y-1)
            ff(x, y+1)
        }
    }
    ff(x, y)
}
```

And a test program.  Works with code from read ppm and write ppm to pipe tasks.  For input, it uses a version of the test file converted by the Go solution to "Read an image through a pipe".  For output it uses the trick from "PPM conversion through a pipe" to write the .png suitable for uploading to RC.
[[File:Go_flood.png|right]]

```go
package main

import (
    "log"
    "os/exec"
    "raster"
)

func main() {
    b, err := raster.ReadPpmFile("Unfilledcirc.ppm")
    if err != nil {
        log.Fatal(err)
    }
    b.Flood(200, 200, raster.Pixel{127, 0, 0})
    c := exec.Command("convert", "ppm:-", "flood.png")
    pipe, err := c.StdinPipe()
    if err != nil {
        log.Fatal(err)
    }
    if err = c.Start(); err != nil {
        log.Fatal(err)
    }
    if err = b.WritePpmTo(pipe); err != nil {
        log.Fatal(err)
    }
    if err = pipe.Close(); err != nil {
        log.Fatal(err)
    }
}
```



## Haskell

This code uses the Bitmap and Bitmap.RGB modules defined [[Bitmap#Haskell|here]].

```Haskell
import Data.Array.ST
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Bitmap

-- Implementation of a stack in the ST monad
pushST :: STStack s a -> a -> ST s ()
pushST s e = do
    s2 <- readSTRef s
    writeSTRef s (e : s2)

popST :: STStack s a -> ST s (Stack a)
popST s = do
    s2 <- readSTRef s
    writeSTRef s $ tail s2
    return $ take 1 s2

isNotEmptySTStack :: STStack s a -> ST s Bool
isNotEmptySTStack s = do
    s2 <- readSTRef s
    return $ not $ null s2

emptySTStack :: ST s (STStack s a)
emptySTStack = newSTRef []

consumeSTStack :: STStack s a -> (a -> ST s ()) -> ST s ()
consumeSTStack s f = do
    check <- isNotEmptySTStack s
    when check $ do
        e <- popST s
        f $ head e
        consumeSTStack s f

type Spanning s = STRef s (Bool, Bool)

setSpanLeft :: Spanning s -> Bool -> ST s ()
setSpanLeft s v = do
    (_, r) <- readSTRef s
    writeSTRef s (v, r)

setSpanRight :: Spanning s -> Bool -> ST s ()
setSpanRight s v = do
    (l, _) <- readSTRef s
    writeSTRef s (l, v)

setSpanNone :: Spanning s -> ST s ()
setSpanNone s = writeSTRef s (False, False)

floodFillScanlineStack :: Color c => Image s c -> Pixel -> c -> ST s (Image s c)
floodFillScanlineStack b coords newC = do
    stack <- emptySTStack -- new empty stack holding pixels to fill
    spans <- newSTRef (False, False) -- keep track of spans in scanWhileX
    fFSS b stack coords newC spans -- function loop
    return b
    where
        fFSS b st c newC p = do
            oldC <- getPix b c
            unless (oldC == newC) $ do
                pushST st c -- store the coordinates in the stack
                (w, h) <- dimensions b
                consumeSTStack st (scanWhileY b p oldC >=>
                    scanWhileX b st p oldC newC (w, h))

        -- take a buffer, the span record, the color of the Color the fill is
        -- started from, a coordinate from the stack, and returns the coord
        -- of the next point to be filled in the same column
        scanWhileY b p oldC coords@(Pixel (x, y)) =
            if y >= 0
            then do
                z <- getPix b coords
                if z == oldC
                then scanWhileY b p oldC (Pixel (x, y - 1))
                else do
                    setSpanNone p
                    return (Pixel (x, y + 1))
            else do
                setSpanNone p
                return (Pixel (x, y + 1))

        -- take a buffer, a stack, a span record, the old and new color, the
        -- height and width of the buffer, and a coordinate.
        -- paint the point with the new color, check if the fill must expand
        -- to the left or right or both, and store those coordinates in the
        -- stack for subsequent filling
        scanWhileX b st p oldC newC (w, h) coords@(Pixel (x, y)) =
            when (y < h) $ do
                z <- getPix b coords
                when (z == oldC) $ do
                    setPix b coords newC
                    (spanLeft, spanRight) <- readSTRef p
                    when (not spanLeft && x > 0) $ do
                        z2 <- getPix b (Pixel (x - 1, y))
                        when (z2 == oldC) $ do
                            pushST st (Pixel (x - 1, y))
                            setSpanLeft p True
                    when (spanLeft && x > 0) $ do
                        z3 <- getPix b (Pixel (x - 1, y))
                        when (z3 /= oldC) $
                            setSpanLeft p False
                    when (not spanRight && x < (w - 1)) $ do
                        z4 <- getPix b (Pixel (x + 1, y))
                        when (z4 == oldC) $ do
                            pushST st (Pixel (x + 1, y))
                            setSpanRight p True
                    when (spanRight && x < (w - 1)) $ do
                        z5 <- getPix b (Pixel (x + 1, y))
                        when (z5 /= oldC) $
                            setSpanRight p False
                    scanWhileX b st p oldC newC (w, h) (Pixel (x, y + 1))

```



## HicEst

HicEst color fill is via the [http://www.HicEst.com/DeCoRation.htm decoration option of WRITE()]

```HicEst
WINDOW(WINdowhandle=wh, BaCkcolor=0, TItle="Rosetta test image")

WRITE(WIN=wh, DeCoRation="EL=14, BC=14") ! color 14 == bright yellow

RGB(128, 100, 0, 25) ! define color nr 25 as 128/255 red, 100/255 green, 0 blue
WRITE(WIN=wh, DeCoRation="L=1/4, R=1/2, T=1/4, B=1/2, EL=25, BC=25")

WINDOW(Kill=wh)
```



## J

'''Solution:'''

Uses <code>getPixels</code> and <code>setPixels</code> from [[Basic bitmap storage#J|Basic bitmap storage]].

```j
NB. finds and labels contiguous areas with the same numbers
NB. ref: http://www.jsoftware.com/pipermail/general/2005-August/023886.html
findcontig=: (|."1@|:@:>. (* * 1&(|.!.0)))^:4^:_@(* >:@i.@$)

NB.*getFloodpoints v Returns points to fill given starting point (x) and image (y)
getFloodpoints=: [: 4&$.@$. [ (] = getPixels) [: findcontig ] -:"1 getPixels

NB.*floodFill v Floods area, defined by point and color (x), of image (y)
NB. x is: 2-item list of (y x) ; (color)
floodFill=: (1&({::)@[ ;~ 0&({::)@[ getFloodpoints ]) setPixels ]
```


'''Example Usage:'''

The following draws the same image as for the [[Flood fill#Tcl|Tcl example image]] below.

Uses definitions from [[Basic bitmap storage#J|Basic bitmap storage]], [[Bresenham's line algorithm#J|Bresenham's line algorithm]] and [[Midpoint circle algorithm#J|Midpoint circle algorithm]].

```j
'white blue yellow black orange red'=: 255 255 255,0 0 255,255 255 0,0 0 0,255 165 0,:255 0 0
myimg=: white makeRGB 50 70
lines=: _2]\^:2 ] 0 0 25 0 , 25 0 25 35 , 25 35 0 35 , 0 35 0 0
myimg=: (lines;blue) drawLines myimg
myimg=: (3 3; yellow) floodFill myimg
myimg=: ((25 35 24 ,: 25 35 10);black) drawCircles myimg
myimg=: (5 34;orange) floodFill myimg
myimg=: (5 36;red) floodFill myimg
viewRGB myimg
```


'''Alternative findcontig:'''

The following alternative version of <code>findcontig</code> is less concise but is leaner, faster, works for n-dimensions and is not restricted to numerical arrays.

```j
NB. ref: http://www.jsoftware.com/pipermail/general/2005-August/024174.html
eq=:[:}:"1 [:($$[:([:+/\1:,}:~:}.),) ,&_"1                NB. equal numbers for atoms of y connected in first direction
eq_nd=: i.@#@$(<"0@[([:, |:^:_1"0 _)&> [:EQ&.> <@|:"0 _)] NB. n-dimensional eq, gives an #@$,*/@$ shaped matrix
repl=: (i.~{.){ {:@]                                      NB. replaces x by applying replace table y
cnnct=: [: |:@({."1<.//.]) [: ; <@(,.<./)/.~

findcontig_nd=: 3 : '($y)${.  ([:({.,~}:) ([ repl cnnct)/\.)^:([:+./@(~:/)2&{.)^:_ (,{.) eq_nd (i.~ ~.@,) y'
```



## Java

Input is the image, the starting node (x, y), the target color we want to fill, and the replacement color that will replace the target color. It implements a 4-way flood fill algorithm. For large images, the performance can be improved by drawing the scanlines instead of setting each pixel to the replacement color, or by working directly on the databuffer.

```java
import java.awt.Color;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.Deque;
import java.util.LinkedList;

public class FloodFill {
  public void floodFill(BufferedImage image, Point node, Color targetColor, Color replacementColor) {
    int width = image.getWidth();
    int height = image.getHeight();
    int target = targetColor.getRGB();
    int replacement = replacementColor.getRGB();
    if (target != replacement) {
      Deque<Point> queue = new LinkedList<Point>();
      do {
        int x = node.x;
        int y = node.y;
        while (x > 0 && image.getRGB(x - 1, y) == target) {
          x--;
        }
        boolean spanUp = false;
        boolean spanDown = false;
        while (x < width && image.getRGB(x, y) == target) {
          image.setRGB(x, y, replacement);
          if (!spanUp && y > 0 && image.getRGB(x, y - 1) == target) {
            queue.add(new Point(x, y - 1));
            spanUp = true;
          } else if (spanUp && y > 0 && image.getRGB(x, y - 1) != target) {
            spanUp = false;
          }
          if (!spanDown && y < height - 1 && image.getRGB(x, y + 1) == target) {
            queue.add(new Point(x, y + 1));
            spanDown = true;
          } else if (spanDown && y < height - 1 && image.getRGB(x, y + 1) != target) {
            spanDown = false;
          }
          x++;
        }
      } while ((node = queue.pollFirst()) != null);
    }
  }
}
```

And here is an example of how to replace the white color with red from the sample image (with starting node (50, 50)):

```java
import java.io.IOException;
import java.awt.Color;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.io.File;
import javax.imageio.ImageIO;

public class Test {
  public Test() throws IOException {
    BufferedImage image = ImageIO.read(new File("Unfilledcirc.png"));
    new FloodFill().floodFill(image, new Point(50, 50), Color.WHITE, Color.RED);
    ImageIO.write(image, "png", new File("output.png"));
  }

  public static void main(String[] args) throws IOException {
    new Test();
  }
}
```



## Julia

{{works with|Julia|0.6}}
Inspired to [[#Python | Python]] version.


```julia
using Images, FileIO

function floodfill!(img::Matrix{<:Color}, initnode::CartesianIndex{2}, target::Color, replace::Color)
    img[initnode] != target && return img
    # constants
    north = CartesianIndex(-1,  0)
    south = CartesianIndex( 1,  0)
    east  = CartesianIndex( 0,  1)
    west  = CartesianIndex( 0, -1)

    queue = [initnode]
    while !isempty(queue)
        node = pop!(queue)
        if img[node] == target
            wnode = node
            enode = node + east
        end
        # Move west until color of node does not match target color
        while checkbounds(Bool, img, wnode) && img[wnode] == target
            img[wnode] = replace
            if checkbounds(Bool, img, wnode + north) && img[wnode + north] == target
                push!(queue, wnode + north)
            end
            if checkbounds(Bool, img, wnode + south) && img[wnode + south] == target
                push!(queue, wnode + south)
            end
            wnode += west
        end
        # Move east until color of node does not match target color
        while checkbounds(Bool, img, enode) && img[enode] == target
            img[enode] = replace
            if checkbounds(Bool, img, enode + north) && img[enode + north] == target
                push!(queue, enode + north)
            end
            if checkbounds(Bool, img, enode + south) && img[enode + south] == target
                push!(queue, enode + south)
            end
            enode += east
        end
    end
    return img
end

img = Gray{Bool}.(load("data/unfilledcircle.png"))
floodfill!(img, CartesianIndex(100, 100), Gray(false), Gray(true))
save("data/filledcircle.png", img)
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.4-3

import java.awt.Color
import java.awt.Point
import java.awt.image.BufferedImage
import java.util.LinkedList
import java.io.File
import javax.imageio.ImageIO
import javax.swing.JOptionPane
import javax.swing.JLabel
import javax.swing.ImageIcon

fun floodFill(image: BufferedImage, node: Point, targetColor: Color, replColor: Color) {
    val target = targetColor.getRGB()
    val replacement = replColor.getRGB()
    if (target == replacement) return
    val width = image.width
    val height = image.height
    val queue = LinkedList<Point>()
    var nnode: Point? = node

    do {
        var x = nnode!!.x
        val y = nnode.y
        while (x > 0 && image.getRGB(x - 1, y) == target) x--
        var spanUp = false
        var spanDown = false

        while (x < width && image.getRGB(x, y) == target) {
            image.setRGB(x, y, replacement)

            if (!spanUp && y > 0 && image.getRGB(x, y - 1) == target) {
                queue.add(Point(x, y - 1))
                spanUp = true
            }
            else if (spanUp && y > 0 && image.getRGB(x, y - 1) != target) {
                spanUp = false
            }

            if (!spanDown && y < height - 1 && image.getRGB(x, y + 1) == target) {
                queue.add(Point(x, y + 1))
                spanDown = true
            }
            else if (spanDown && y < height - 1 && image.getRGB(x, y + 1) != target) {
                spanDown = false
            }
            x++
        }
        nnode = queue.pollFirst()
    }
    while (nnode != null)
}

fun main(args: Array<String>) {
   val image = ImageIO.read(File("Unfilledcirc.png"))
   floodFill(image, Point(50, 50), Color.white, Color.yellow)
   val title = "Floodfilledcirc.png"
   ImageIO.write(image, "png", File(title))
   JOptionPane.showMessageDialog(null, JLabel(ImageIcon(image)), title, JOptionPane.PLAIN_MESSAGE)
}
```



## Liberty BASIC


```lb
'This example requires the Windows API
NoMainWin
WindowWidth = 267.5
WindowHeight = 292.5
UpperLeftX=int((DisplayWidth-WindowWidth)/2)
UpperLeftY=int((DisplayHeight-WindowHeight)/2)

Global hDC : hDC = GetDC(0)
Struct point, x As long, y As long
Struct RGB, Red As long, Green As long, Blue As long
Struct rect, left As long, top As long, right As long, bottom As long

StyleBits #main.gbox, 0, _WS_BORDER, 0, 0
GraphicBox #main.gbox, 2.5, 2.5, 253, 252

Open "Flood Fill - Click a Color" For Window As #main
Print #main, "TrapClose quit"
Print #main.gbox, "Down; Fill Black; Place 125 125; BackColor White; " _
                  + "CircleFilled 115; Place 105 105; BackColor Black; CircleFilled 50; Flush"
Print #main.gbox, "When leftButtonUp gBoxClick"
Print #main.gbox, "Size 1"
Wait

    Sub quit handle$
        Call ReleaseDC 0, hDC
        Close #main
        End
    End Sub


    Sub gBoxClick handle$, MouseX, MouseY
        result = GetCursorPos()
        targetRGB = GetPixel(hDC, point.x.struct, point.y.struct)
        ColorDialog "", replacementColor$
        If replacementColor$ = "" Then Exit Sub
        Print #main.gbox, "Color " + Word$(replacementColor$, 1) + " " + Word$(replacementColor$, 2) + " " + Word$(replacementColor$, 3)
        result = FloodFill(MouseX, MouseY, targetRGB)
        Print #main.gbox, "DelSegment FloodFill"
        Print #main.gbox, "GetBMP FloodFill 0 0 500 500; CLS; DrawBMP FloodFill 0 0; Flush FloodFill"
        Notice "Complete!"
        UnLoadBMP "FloodFill"
    End Sub

    Sub ReleaseDC hWnd, hDC
        CallDLL #user32,"ReleaseDC", hWnd As uLong, hDC As uLong, ret As Long
    End Sub

    Function GetDC(hWnd)
        CallDLL #user32, "GetDC", hWnd As uLong, GetDC As uLong
    End Function

    Function GetCursorPos()
        CallDLL #user32, "GetCursorPos", point As struct, GetCursorPos As uLong
    End Function

    Function GetPixel(hDC, x, y)
        CallDLL #gdi32, "GetPixel", hDC As uLong, x As long, y As long, GetPixel As long
    End Function

    Function getLongRGB(RGB.Blue)
        getLongRGB = (RGB.Blue * (256 * 256))
    End Function

    Function GetWindowRect(hWnd)
        'Get ClientRectangle
        CallDLL #user32, "GetWindowRect", hWnd As ulong, rect As struct, GetWindowRect As ulong
    End Function

    Function FloodFill(mouseXX, mouseYY, targetColor)
        Scan
        result = GetWindowRect(Hwnd(#main.gbox))
        X = Int(mouseXX + rect.left.struct)
        Y = Int(mouseYY + rect.top.struct)
        If (GetPixel(hDC, X, Y) <> targetColor) Then
            Exit Function
        Else
            CLS
            Print str$(mouseXX) + "   " + str$(mouseYY)
            Print #main.gbox, "Set " + str$(mouseXX) + " " + str$(mouseYY)
        End If
        If (mouseXX > 0) And (mouseXX < 253) Then
            result = FloodFill((mouseXX - 1), mouseYY, targetColor)
            result = FloodFill((mouseXX + 1), mouseYY, targetColor)
        End If
        If (mouseYY > 0) And (mouseYY < 252) Then
            result = FloodFill(mouseXX, (mouseYY + 1), targetColor)
            result = FloodFill(mouseXX, (mouseYY - 1), targetColor)
        End If
    End Function
```



## Lingo

Lingo has built-in flood fill for image objects, so a custom implementation would be pointless:

```lingo
img.floodFill(x, y, rgb(r,g,b))
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
createMask[img_, pos_, tol_] :=
            RegionBinarize[img, Image[SparseArray[pos -> 1, ImageDimensions[img]]], tol];
floodFill[img_Image, pos_List, tol_Real, color_List] :=
            ImageCompose[
                 SetAlphaChannel[ImageSubtract[img, createMask[img, pos, tol]], 1],
                 SetAlphaChannel[Image[ConstantArray[color, ImageDimensions[img]]],
                                 Dilation[createMask[img, pos, tol],1]
                                ]
                        ]
```


{{out}}
Import the test image and fill the region containing the pixel at coordinate 100,100 with red (RGB 100%,0%,0%) using a tolerance of 1%

```txt
floodFill[Import["http://rosettacode.org/mw/images/0/0f/Unfilledcirc.png"], {100, 100}, 0.01, {1, 0, 0}]
```



## Perl


{{libheader|Imlib2}}

The <tt>fill</tt> of the Perl package Image::Imlib2 is a flood fill (so the documentatin of Image::Imlib2 says). The target colour is the one of the starting point pixel; the color set with <tt>set_color</tt> is the fill colour.


```perl
#! /usr/bin/perl

use strict;
use Image::Imlib2;

my $img = Image::Imlib2->load("Unfilledcirc.jpg");
$img->set_color(0, 255, 0, 255);
$img->fill(100,100);
$img->save("filledcirc.jpg");
exit 0;
```


A homemade implementation can be:


```perl
use strict;
use Image::Imlib2;

sub colordistance
{
    my ( $c1, $c2 ) = @_;

    my ( $r1, $g1, $b1 ) = @$c1;
    my ( $r2, $g2, $b2 ) = @$c2;
    return sqrt(( ($r1-$r2)**2 + ($g1-$g2)**2 + ($b1-$b2)**2 ))/(255.0*sqrt(3.0));
}

sub floodfill
{
    my ( $img, $x, $y, $r, $g, $b ) = @_;
    my $distparameter = 0.2;

    my %visited = ();
    my @queue = ();

    my @tcol = ( $r, $g, $b );
    my @col = $img->query_pixel($x, $y);
    if ( colordistance(\@tcol, \@col) > $distparameter ) { return; }
    push @queue, [$x, $y];
    while ( @queue ) {
	my $pointref = shift(@queue);
	( $x, $y ) = @$pointref;
	if ( ($x < 0) || ($y < 0) || ( $x >= $img->width ) || ( $y >= $img->height ) ) { next; }
	if ( ! exists($visited{"$x,$y"}) ) {
	    @col = $img->query_pixel($x, $y);
	    if ( colordistance(\@tcol, \@col) <= $distparameter ) {
		$img->draw_point($x, $y);
		$visited{"$x,$y"} = 1;
		push @queue, [$x+1, $y];
		push @queue, [$x-1, $y];
		push @queue, [$x, $y+1];
		push @queue, [$x, $y-1];
	    }
	}
    }
}

# usage example
my $img = Image::Imlib2->load("Unfilledcirc.jpg");
$img->set_color(0,255,0,255);
floodfill($img, 100,100, 0, 0, 0);
$img->save("filledcirc1.jpg");
exit 0;
```


This fills better than the Image::Imlib2 <tt>fill</tt> function the inner circle, since because of JPG compression and thanks to the <tt>$distparameter</tt>, it "sees" as black also pixel that are no more exactly black.


## Phix

{{Trans|Go}}
Requires read_ppm() from [[Bitmap/Read_a_PPM_file#Phix|Read_a_PPM_file]], write_ppm() from [[Bitmap/Write_a_PPM_file#Phix|Write_a_PPM_file]].
Uses the output of Bitmap_Circle.exw (Circle.ppm)
Working program is demo\rosetta\Bitmap_FloodFill.exw, results may be verified with demo\rosetta\viewppm.exw

```Phix
function ff(sequence img, integer x, integer y, integer colour, integer target)
    if x>=1 and x<=length(img)
    and y>=1 and y<=length(img[x])
    and img[x][y]=target then
        img[x][y] = colour
        img = ff(img,x-1,y,colour,target)
        img = ff(img,x+1,y,colour,target)
        img = ff(img,x,y-1,colour,target)
        img = ff(img,x,y+1,colour,target)
    end if
    return img
end function

function FloodFill(sequence img, integer x, integer y, integer colour)
integer target = img[x][y]
    return ff(img,x,y,colour,target)
end function

sequence img = read_ppm("Circle.ppm")
    img = FloodFill(img, 200, 100, blue)
    write_ppm("FloodIn.ppm",img)
    img = FloodFill(img, 10, 10, green)
    write_ppm("FloodOut.ppm",img)
```




## PicoLisp

Using the format of [[Bitmap#PicoLisp|Bitmap]], a minimal recursive solution:

```PicoLisp
(de ppmFloodFill (Ppm X Y Color)
   (let Target (get Ppm Y X)
      (recur (X Y)
         (when (= Target (get Ppm Y X))
            (set (nth Ppm Y X) Color)
            (recurse (dec X) Y)
            (recurse (inc X) Y)
            (recurse X (dec Y))
            (recurse X (inc Y)) ) ) )
   Ppm )
```

Test using 'ppmRead' from [[Bitmap/Read a PPM file#PicoLisp]] and 'ppmWrite' from [[Bitmap/Write a PPM file#PicoLisp]], filling the white area with red:

```txt
(ppmWrite
   (ppmFloodFill (ppmRead "Unfilledcirc.ppm") 192 128 (255 0 0))
   "Filledcirc.ppm" )
```



## PL/I


```PL/I
fill: procedure (x, y, fill_color) recursive; /* 12 May 2010 */
   declare (x, y) fixed binary;
   declare fill_color bit (24) aligned;

   if x <= lbound(image, 2) | x >= hbound(image, 2) then return;
   if y <= lbound(image, 1) | y >= hbound(image, 1) then return;

   pixel_color = image (x,y); /* Obtain the color of the current pixel. */
   if pixel_color ^= area_color then return;
      /* the pixel has already been filled with fill_color, */
      /* or we are not within the area to be filled.        */
   image(x, y) = fill_color; /* color the desired area. */

   pixel_color = image (x,y-1); /* Obtain the color of the pixel to the north. */
   if pixel_color = area_color then call fill (x, y-1, fill_color);

   pixel_color = image (x-1,y); /* Obtain the color of the pixel to the west. */
   if pixel_color = area_color then call fill (x-1, y, fill_color);

   pixel_color = image (x+1,y); /* Obtain the color of the pixel to the east. */
   if pixel_color = area_color then call fill (x+1, y, fill_color);

   pixel_color = image (x,y+1); /* Obtain the color of the pixel to the south. */
   if pixel_color = area_color then call fill (x, y+1, fill_color);

end fill;
```

The following PL/I statements change the color of the white area
of the sample image to red, and the central orb to green.
<lang>
   /* Fill the white area of the suggested image with red color. */
   area_color = (24)'1'b;
   call fill (125, 25, '000000000000000011111111'b );

   /* Fill the center orb of the suggested image with green color. */
   area_color = '0'b;
   call fill (125, 125, '000000001111111100000000'b );

```



## PureBasic

=== built-in ===

```PureBasic
FillArea(0,0,-1,$ff)
; Fills an Area in red
```



###  Iterative


```PureBasic
  Procedure Floodfill(x,y,new_color)
    old_color = Point(x,y)
    NewList stack.POINT()
    AddElement(stack()):stack()\x = x : stack()\y = y
    While(LastElement(stack()))
      x = stack()\x  : y = stack()\y
      DeleteElement(stack())
      If Point(x,y) = old_color
         Plot(x, y, new_color)
         AddElement(stack()):stack()\x = x    : stack()\y = y +1
         AddElement(stack()):stack()\x = x    : stack()\y = y -1
         AddElement(stack()):stack()\x = x +1 : stack()\y = y
         AddElement(stack()):stack()\x = x -1 : stack()\y = y
      EndIf
    Wend
  EndProcedure

  If OpenWindow(0, 0, 0, 200, 200, "Floodfill Beispiel", #PB_Window_SystemMenu | #PB_Window_ScreenCentered)
    StartDrawing(WindowOutput(0))
      Box(0, 0, 200, 200, RGB(255, 255, 255))
      DrawingMode(#PB_2DDrawing_Outlined )
      Circle(100, 100, 90, RGB(255 ,0,0)): Circle(120, 80, 30, RGB(255 ,0,0)): Circle(200,200, 70, RGB(255 ,0,0))

      Floodfill(40,40,RGB(0 ,255,0))

    StopDrawing()
    Repeat
      Event = WaitWindowEvent()
    Until Event = #PB_Event_CloseWindow
  EndIf
```



## Python


```python

import Image
def FloodFill( fileName, initNode, targetColor, replaceColor ):
   img = Image.open( fileName )
   pix = img.load()
   xsize, ysize = img.size
   Q = []
   if pix[ initNode[0], initNode[1] ] != targetColor:
      return img
   Q.append( initNode )
   while Q != []:
      node = Q.pop(0)
      if pix[ node[0], node[1] ] == targetColor:
         W = list( node )
         if node[0] + 1 < xsize:
            E = list( [ node[0] + 1, node[1] ] )
         else:
            E = list( node )
      # Move west until color of node does not match targetColor
      while pix[ W[0], W[1] ] == targetColor:
         pix[ W[0], W[1] ] = replaceColor
         if W[1] + 1 < ysize:
            if pix[ W[0], W[1] + 1 ] == targetColor:
               Q.append( [ W[0], W[1] + 1 ] )
         if W[1] - 1 >= 0:
            if pix[ W[0], W[1] - 1 ] == targetColor:
               Q.append( [ W[0], W[1] - 1 ] )
         if W[0] - 1 >= 0:
            W[0] = W[0] - 1
         else:
            break
      # Move east until color of node does not match targetColor
      while pix[ E[0], E[1] ] == targetColor:
         pix[ E[0], E[1] ] = replaceColor
         if E[1] + 1 < ysize:
            if pix[ E[0], E[1] + 1 ] == targetColor:
               Q.append( [ E[0], E[1] + 1 ] )
         if E[1] - 1 >= 0:
            if pix[ E[0], E[1] - 1 ] == targetColor:
               Q.append( [ E[0], E[1] -1 ] )
         if E[0] + 1 < xsize:
            E[0] = E[0] + 1
         else:
            break
      return img

```



### Usage example


```python

# "FloodFillClean.png" is name of input file
# [55,55] the x,y coordinate where fill starts
# (0,0,0,255) the target colour being filled( black in this example )
# (255,255,255,255) the final colour ( white in this case )
img = FloodFill( "FloodFillClean.png", [55,55], (0,0,0,255), (255,255,255,255) )
#The resulting image is saved as Filled.png
img.save( "Filled.png" )

```




## R

'''Stack-based recursive version'''

```R

library(png)
img <- readPNG("Unfilledcirc.png")
M <- img[ , , 1]
M <- ifelse(M < 0.5, 0, 1)
image(M, col = c(1, 0))

# https://en.wikipedia.org/wiki/Flood_fill
floodfill <- function(row, col, tcol, rcol) {
  if (tcol == rcol) return()
  if (M[row, col] != tcol) return()
  M[row, col] <<- rcol
  floodfill(row - 1, col    , tcol, rcol) # south
  floodfill(row + 1, col    , tcol, rcol) # north
  floodfill(row    , col - 1, tcol, rcol) # west
  floodfill(row    , col + 1, tcol, rcol) # east
  return("filling completed")
}

options(expressions = 10000)
startrow <- 100; startcol <- 100
floodfill(startrow, startcol, 0, 2)

image(M, col = c(1, 0, 2))

```

'''Queue-based version (Forest Fire algorithm)'''

```R

library(png)
img <- readPNG("Unfilledcirc.png")
M <- img[ , , 1]
M <- ifelse(M < 0.5, 0, 1)
M <- rbind(M, 0)
M <- cbind(M, 0)
image(M, col = c(1, 0))

# https://en.wikipedia.org/wiki/Flood_fill
floodfill <- function(row, col, tcol, rcol) {
  if (tcol == rcol) return()
  if (M[row, col] != tcol) return()
  Q <- matrix(c(row, col), 1, 2)
  while (dim(Q)[1] > 0) {
    n <- Q[1, , drop = FALSE]
    west  <- cbind(n[1]    , n[2] - 1)
    east  <- cbind(n[1]    , n[2] + 1)
    north <- cbind(n[1] + 1, n[2]    )
    south <- cbind(n[1] - 1, n[2]    )
    Q <- Q[-1, , drop = FALSE]
    if (M[n] == tcol) {
      M[n] <<- rcol
      if (M[west] == tcol)  Q <- rbind(Q, west)
      if (M[east] == tcol)  Q <- rbind(Q, east)
      if (M[north] == tcol) Q <- rbind(Q, north)
      if (M[south] == tcol) Q <- rbind(Q, south)
    }
  }
  return("filling completed")
}

startrow <- 100; startcol <- 100
floodfill(startrow, startcol, 0, 2)
startrow <- 50; startcol <- 50
floodfill(startrow, startcol, 1, 3)

image(M, col = c(1, 0, 2, 3))

```



## Racket


```racket

#lang racket

(require racket/draw)

;; flood-fill: bitmap<%> number number color color -> void
;; An example of flood filling a bitmap.
;;
;; We'll use a raw, byte-oriented interface here for demonstration
;; purposes.  Racket does provide get-pixel and set-pixel functions
;; which work on color% structures rather than bytes, but it's useful
;; to see that the byte approach works as well.
(define (flood-fill bm start-x start-y target-color replacement-color)
  ;; The main loop.
  ;; http://en.wikipedia.org/wiki/Flood_fill
  (define (iter x y)
    (when (and (in-bounds? x y) (target-color-at? x y))
      (replace-color-at! x y)
      (iter (add1 x) y)
      (iter (sub1 x) y)
      (iter x (add1 y))
      (iter x (sub1 y))))

  ;; With auxillary definitions below:
  (define width (send bm get-width))
  (define height (send bm get-height))

  (define buffer (make-bytes (* width height 4)))
  (send bm get-argb-pixels 0 0 width height buffer)

  (define-values (target-red target-green target-blue)
    (values (send target-color red)
            (send target-color green)
            (send target-color blue)))

  (define-values (replacement-red replacement-green replacement-blue)
    (values (send replacement-color red)
            (send replacement-color green)
            (send replacement-color blue)))

  (define (offset-at x y) (* 4 (+ (* y width) x)))

  (define (target-color-at? x y)
    (define offset (offset-at x y))
    (and (= (bytes-ref buffer (+ offset 1)) target-red)
         (= (bytes-ref buffer (+ offset 2)) target-green)
         (= (bytes-ref buffer (+ offset 3)) target-blue)))

  (define (replace-color-at! x y)
    (define offset (offset-at x y))
    (bytes-set! buffer (+ offset 1) replacement-red)
    (bytes-set! buffer (+ offset 2) replacement-green)
    (bytes-set! buffer (+ offset 3) replacement-blue))

  (define (in-bounds? x y)
    (and (<= 0 x) (< x width) (<= 0 y) (< y height)))

  ;; Finally, let's do the fill, and then store the
  ;; result back into the bitmap:
  (iter start-x start-y)
  (send bm set-argb-pixels 0 0 width height buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example: flood fill a hole shape.
(define bm (make-bitmap 100 100))
(define dc (send bm make-dc))

;; We intentionally set the smoothing of the dc to
;; aligned so that there are no gaps in the shape for the
;; flood to leak through.
(send dc set-smoothing 'aligned)
(send dc draw-rectangle 10 10 80 80)
(send dc draw-rounded-rectangle 20 20 50 50)
;; In DrRacket, we can print the bm to look at it graphically,
;; before the flood fill:
bm

(flood-fill bm 50 50
            (send the-color-database find-color "white")
            (send the-color-database find-color "DarkSeaGreen"))
;; ... and after:
bm

```





## REXX

{{trans|PL/I}}

```rexx
/*REXX program  demonstrates a  method  to  perform a  flood fill  of an area.          */
black= '000000000000000000000000'b               /*define the black color  (using bits).*/
red  = '000000000000000011111111'b               /*   "    "   red    "       "     "   */
green= '000000001111111100000000'b               /*   "    "  green   "       "     "   */
white= '111111111111111111111111'b               /*   "    "  white   "       "     "   */
                                                 /*image is defined to the test image.  */
hx=125;    hy=125                                /*define limits  (X,Y)  for the image. */
area=white;      call fill 125,  25, red         /*fill the white area in red.          */
area=black;      call fill 125, 125, green       /*fill the center orb in green.        */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fill: procedure expose image. hx hy area;  parse arg x,y,fill_color   /*obtain the args.*/
      if x<1 | x>hx | y<1 | y>hy   then return   /*X or Y  are outside of the image area*/
      pixel=image.x.y                            /*obtain the color of the  X,Y  pixel. */
      if pixel\==area  then return               /*the pixel has already been filled    */
                                                 /*with the  fill_color,  or we are not */
                                                 /*within the area to be filled.        */
      image.x.y=fill_color                       /*color desired area with fill_color.  */
      pixel=@(x  , y-1);    if pixel==area  then call fill x  , y-1, fill_color  /*north*/
      pixel=@(x-1, y  );    if pixel==area  then call fill x-1, y  , fill_color  /*west */
      pixel=@(x+1, y  );    if pixel==area  then call fill x+1, y  , fill_color  /*east */
      pixel=@(x  , y+1);    if pixel==area  then call fill x  , y+1, fill_color  /*south*/
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
@:    parse arg $x,$y;      return image.$x.$y   /*return with color of the  X,Y  pixel.*/
```






## Ruby


'''Note''' This code is not completely functional. Please add the remaining classes (Pixel, ..) and initializers. Or maybe a library to be included to make this work.


```ruby
class RGBColour
  def ==(a_colour)
    values == a_colour.values
  end
end

class Queue < Array
  alias_method :enqueue, :push
  alias_method :dequeue, :shift
end

class Pixmap
  def flood_fill(pixel, new_colour)
    current_colour = self[pixel.x, pixel.y]
    queue = Queue.new
    queue.enqueue(pixel)
    until queue.empty?
      p = queue.dequeue
      if self[p.x, p.y] == current_colour
        west = find_border(p, current_colour, :west)
        east = find_border(p, current_colour, :east)
        draw_line(west, east, new_colour)
        q = west
        while q.x <= east.x
          [:north, :south].each do |direction|
            n = neighbour(q, direction)
            queue.enqueue(n) if self[n.x, n.y] == current_colour
          end
          q = neighbour(q, :east)
        end
      end
    end
  end

  def neighbour(pixel, direction)
    case direction
    when :north then Pixel[pixel.x, pixel.y - 1]
    when :south then Pixel[pixel.x, pixel.y + 1]
    when :east  then Pixel[pixel.x + 1, pixel.y]
    when :west  then Pixel[pixel.x - 1, pixel.y]
    end
  end

  def find_border(pixel, colour, direction)
    nextp = neighbour(pixel, direction)
    while self[nextp.x, nextp.y] == colour
      pixel = nextp
      nextp = neighbour(pixel, direction)
    end
    pixel
  end
end

bitmap = Pixmap.new(300, 300)
bitmap.draw_circle(Pixel[149,149], 120, RGBColour::BLACK)
bitmap.draw_circle(Pixel[200,100], 40, RGBColour::BLACK)
bitmap.flood_fill(Pixel[140,160], RGBColour::BLUE)
```



## Rust



```rust

/* Naive Rust implementation of RosettaCode's Bitmap/Flood fill excercise.
 *
 * For the sake of simplicity this code reads PPM files (format specification can be found here: http://netpbm.sourceforge.net/doc/ppm.html ).
 * The program assumes that the image has been created by GIMP in PPM ASCII mode and panics at any error.
 *
 * Also this program expects the input file to be in the same directory as the executable and named
 * "input.ppm" and outputs a file in the same directory under the name "output.ppm".
 *
 */

use std::fs::File; // Used for creating/opening files.
use std::io::{BufReader, BufRead, Write}; // Used for reading/writing files.

fn read_image(filename: String) -> Vec<Vec<(u8,u8,u8)>> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    let _ = lines.next().unwrap(); // Skip P3 signature.
    let _ = lines.next().unwrap(); // Skip GIMP comment.

    let dimensions: (usize, usize) = {
        let line = lines.next().unwrap().unwrap();
        let values = line.split_whitespace().collect::<Vec<&str>>();

        // We turn the &str vector that holds the width & height of the image into an usize tuplet.
        (values[0].parse::<usize>().unwrap(),values[1].parse::<usize>().unwrap())
    };

    let _ = lines.next().unwrap(); // Skip maximum color value (we assume 255).

    let mut image_data = Vec::with_capacity(dimensions.1);

    for y in 0..dimensions.1 {
        image_data.push(Vec::new());
        for _ in 0..dimensions.0 {
            // We read the R, G and B components and put them in the image_data vector.
            image_data[y].push((lines.next().unwrap().unwrap().parse::<u8>().unwrap(),
                                lines.next().unwrap().unwrap().parse::<u8>().unwrap(),
                                lines.next().unwrap().unwrap().parse::<u8>().unwrap()));
        }
    }

    image_data
}

fn write_image(image_data: Vec<Vec<(u8,u8,u8)>>) {
    let mut file = File::create(format!("./output.ppm")).unwrap();

    // Signature, then width and height, then 255 as max color value.
    write!(file, "P3\n{} {}\n255\n", image_data.len(), image_data[0].len()).unwrap();

    for row in &image_data {
        // For performance reasons, we reserve a String with the necessary length for a line and
        // fill that up before writing it to the file.

        let mut line = String::with_capacity(row.len()*6); // 6 = r(space)g(space)b(space)
        for (r,g,b) in row {

            // &* is used to turn a String into a &str as needed by push_str.
            line.push_str(&*format!("{} {} {} ", r,g,b));
        }

        write!(file, "{}", line).unwrap();
    }

}

fn flood_fill(x: usize, y: usize, target: &(u8,u8,u8), replacement: &(u8,u8,u8), image_data: &mut Vec<Vec<(u8,u8,u8)>>) {
    if &image_data[y][x] == target {
        image_data[y][x] = *replacement;

        if y > 0 {flood_fill(x,y-1, &target, &replacement, image_data);}
        if x > 0 {flood_fill(x-1,y, &target, &replacement, image_data);}
        if y < image_data.len()-1 {flood_fill(x,y+1, &target, &replacement, image_data);}
        if x < image_data[0].len()-1 {flood_fill(x+1,y, &target, &replacement, image_data);}
    }
}

fn main() {
    let mut data = read_image(String::from("./input.ppm"));

    flood_fill(1,50, &(255,255,255), &(0,255,0), &mut data); // Fill the big white circle with green.
    flood_fill(40,35, &(0,0,0), &(255,0,0), &mut data); // Fill the small black circle with red.

    write_image(data);

}
```



## Scala


Based on Lode Vandevenne's algorithm linked to from Wikipedia, [http://lodev.org/cgtutor/floodfill.html#Scanline_Floodfill_Algorithm_With_Stack Scanline Floodfill Algorithm With Stack].

See [[Basic_bitmap_storage#Scala|Basic Bitmap Storage]] for RgbBitmap class.


```scala
import java.awt.Color
import scala.collection.mutable

object Flood {
  def floodFillStack(bm:RgbBitmap, x: Int, y: Int, targetColor: Color): Unit = {
    // validate
    if (bm.getPixel(x,y) == targetColor) return

    // vars
    val oldColor = bm.getPixel(x,y)
    val pixels = new mutable.Stack[(Int,Int)]

    // candy coating methods
    def paint(fx: Int, fy:Int) = bm.setPixel(fx,fy,targetColor)
    def old(cx: Int, cy: Int): Boolean = bm.getPixel(cx,cy) == oldColor
    def push(px: Int, py: Int) = pixels.push((px,py))

    // starting point
    push(x,y)

    // work
    while (pixels.nonEmpty) {
      val (x, y) = pixels.pop()
      var y1 = y
      while (y1 >= 0 && old(x, y1)) y1 -= 1
      y1 += 1
      var spanLeft = false
      var spanRight = false
      while (y1 < bm.height && old(x, y1)) {
        paint(x,y1)
        if (x > 0 && spanLeft != old(x-1,y1)) {
          if (old(x - 1, y1)) push(x - 1, y1)
          spanLeft = !spanLeft
        }
        if (x < bm.width - 1 && spanRight != old(x+1,y1)) {
          if (old(x + 1, y1)) push(x + 1, y1)
          spanRight = !spanRight
        }
        y1 += 1
      }
    }
  }
}
```



## Standard ML

This implementation is imperative, updating the pixels of the image as it goes.
Flood fill is somewhat difficult to make efficient if we were to use purely functional
data structures instead.


```sml
(* For simplicity, we're going to fill black-and-white images. Nothing
 * fundamental would change if we used more colors. *)
datatype color = Black | White
(* Represent an image as a 2D mutable array of pixels, since flood-fill
 * is naturally an imperative algorithm. *)
type image = color array array

(* Helper functions to construct images for testing. Map 0 -> White
 * and 1 -> Black so we can write images concisely as lists. *)
fun intToColor 0 = White
  | intToColor _ = Black

fun listToImage (LL : int list list) : image =
    Array.tabulate(List.length LL,
     fn i => Array.tabulate (List.length (hd LL),
       fn j => intToColor(List.nth(List.nth(LL,i),j))))

(* Is the given pixel within the image ? *)
fun inBounds (img : image) ((x,y) : int * int) : bool =
    x >= 0 andalso y >= 0 andalso y < Array.length img
    andalso x < Array.length (Array.sub(img, y))

(* Return an option containing the neighbors we should explore next, if any.*)
fun neighbors (img : image) (c : color) ((x,y) : int * int) : (int * int) list option =
    if inBounds img (x,y) andalso Array.sub(Array.sub(img,y),x) <> c
    then SOME [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
    else NONE

(* Update the given pixel of the image. *)
fun setPixel (img : image) ((x,y) : int * int) (c : color) : unit =
    Array.update (Array.sub(img,y),x,c)

(* Recursive fill around the given point using the given color. *)
fun fill (img : image) (c : color) ((x,y) : int * int) : unit =
    case neighbors img c (x,y) of
        SOME xys => (setPixel img (x,y) c; List.app (fill img c) xys)
      | NONE => ()

val test = listToImage
[[0,0,1,1,0,1,0],
 [1,0,1,0,1,0,0],
 [1,0,0,0,0,0,1],
 [0,1,0,0,0,1,0],
 [1,0,0,0,0,0,1],
 [0,0,1,1,1,0,0],
 [0,1,0,0,0,1,0]]

(* Fill the image with black starting at the center. *)
val () = fill test Black (3,3)
```



## Tcl

{{libheader|Tk}}
{{tcllib|struct::queue}}
Using code from [[Basic bitmap storage#Tcl|Basic bitmap storage]], [[Bresenham's line algorithm#Tcl|Bresenham's line algorithm]] and [[Midpoint circle algorithm#Tcl|Midpoint circle algorithm]]

```tcl
package require Tcl 8.5
package require Tk
package require struct::queue

proc floodFill {img colour point} {
    set new [colour2rgb $colour]
    set old [getPixel $img $point]
    struct::queue Q
    Q put $point
    while {[Q size] > 0} {
        set p [Q get]
        if {[getPixel $img $p] eq $old} {
            set w [findBorder $img $p $old west]
            set e [findBorder $img $p $old east]
            drawLine $img $new $w $e
            set q $w
            while {[x $q] <= [x $e]} {
                set n [neighbour $q north]
                if {[getPixel $img $n] eq $old} {Q put $n}
                set s [neighbour $q south]
                if {[getPixel $img $s] eq $old} {Q put $s}
                set q [neighbour $q east]
            }
        }
    }
    Q destroy
}

proc findBorder {img p colour dir} {
    set lookahead [neighbour $p $dir]
    while {[getPixel $img $lookahead] eq $colour} {
        set p $lookahead
        set lookahead [neighbour $p $dir]
    }
    return $p
}

proc x p {lindex $p 0}
proc y p {lindex $p 1}

proc neighbour {p dir} {
    lassign $p x y
    switch -exact -- $dir {
        west  {return [list [incr x -1] $y]}
        east  {return [list [incr x] $y]}
        north {return [list $x [incr y -1]]}
        south {return [list $x [incr y]]}
    }
}

proc colour2rgb {color_name} {
    foreach part [winfo rgb . $color_name] {
        append colour [format %02x [expr {$part >> 8}]]
    }
    return #$colour
}

set img [newImage 70 50]
fill $img white

drawLine $img blue {0 0} {0 25}
drawLine $img blue {0 25} {35 25}
drawLine $img blue {35 25} {35 0}
drawLine $img blue {35 0} {0 0}
floodFill $img yellow {3 3}

drawCircle $img black {35 25} 24
drawCircle $img black {35 25} 10
floodFill $img orange {34 5}
floodFill $img red {36 5}

toplevel .flood
label .flood.l -image $img
pack .flood.l
```

Results in:

[[Image:Tcl_flood_fill.png]]


## XPL0

[[File:FloodXPL0.gif|right|Output]]

```XPL0
include c:\cxpl\codes;

proc    Flood(X, Y, C, C0);     \Fill an area of color C0 with color C
int     X, Y,   \seed coordinate (where to start)
        C, C0;  \color to fill with and color to replace
def     S=8000; \size of queue (must be an even number)
int     Q(S),   \queue (FIFO)
        F, E;   \fill and empty indexes

        proc    EnQ(X, Y);      \Enqueue coordinate
        int     X, Y;
        [Q(F):= X;
        F:= F+1;
        Q(F):= Y;
        F:= F+1;
        if F >= S then F:= 0;
        ];      \EnQ

        proc    DeQ;            \Dequeue coordinate
        [X:= Q(E);
        E:= E+1;
        Y:= Q(E);
        E:= E+1;
        if E >= S then E:= 0;
        ];      \DeQ

[F:= 0;  E:= 0;
EnQ(X, Y);
while E # F do
        [DeQ;
        if ReadPix(X, Y) = C0 then
                [Point(X, Y, C);
                EnQ(X+1, Y);    \enqueue adjacent pixels
                EnQ(X-1, Y);
                EnQ(X, Y+1);
                EnQ(X, Y-1);
                ];
        ];
];      \Flood

def     Size = 30.0;
int     X, Y;
real    Ang, Dist;
[SetVid($101);                  \set 640x480 graphics with 256 colors

Ang:= 0.0;                      \draw some flower petals
repeat  Dist:= Size*(Cos(Ang*3.0) - 1.0);
        X:= fix(Dist*Cos(Ang));
        Y:= fix(Dist*Sin(Ang));
        Point(X+320, 240-Y, $F);
        Ang:= Ang + 0.001;      \draw dots close together to prevent leaks
until   Ang >= 2.0*3.14159;

Flood(330, 240, $2A, 0);        \color the petals
Flood(310, 230, $2C, 0);
Flood(310, 250, $2E, 0);

if ChIn(1) then [];             \wait for keystroke
SetVid(3);                      \restore normal text mode
]
```



## zkl

[[file:Flood_before.zkl.jpg|right]][[file:Flood.zkl.jpg|right]]
{{trans|D}}
Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl


```zkl
fcn flood(pixmap, x,y, repl){  // slow!
   targ,h,w:=pixmap[x,y], pixmap.h,pixmap.w;
   stack:=List(T(x,y));
   while(stack){
      x,y:=stack.pop();
      if((0<=y<h) and (0<=x<w)){
	 p:=pixmap[x,y];
	 if(p==targ){
	    pixmap[x,y]=repl;
	    stack.append( T(x-1,y), T(x+1,y), T(x, y-1), T(x, y+1) );
	 }
      }
   }
}
```


```zkl
pixmap:=PPM(250,302,0xFF|FF|FF);
pixmap.circle(101,200,100,0); pixmap.circle(75,100,25,0);

flood(pixmap,200,100, 0xF0|00|00);
flood(pixmap, 75,110, 0x00|F0|00);
flood(pixmap, 75,100, 0x00|00|F0);

pixmap.writeJPGFile("flood.zkl.jpg");
```


{{omit from|AWK}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|PARI/GP}}
