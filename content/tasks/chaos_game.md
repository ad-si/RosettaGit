+++
title = "Chaos game"
description = ""
date = 2019-09-09T18:45:38Z
aliases = []
[extra]
id = 20078
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "applesoft_basic",
  "basic",
  "basic256",
  "c",
  "chaos_game_sierpinski_triangle_2_16_17_aev",
  "common_lisp",
  "cpp",
  "csharp",
  "easylang",
  "fortran",
  "freebasic",
  "gnuplot",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "logo",
  "lua",
  "maple",
  "mathematica",
  "nim",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "processing",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "run_basic",
  "rust",
  "scala",
  "scilab",
  "sidef",
  "simula",
  "sinclair_zx81_basic",
  "yabasic",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

The [[wp:Chaos_game|Chaos Game]] is a method of generating the attractor of an iterated function system (IFS). One of the best-known and simplest examples creates a fractal, using a polygon and an initial point selected at random.




;Task
Play the Chaos Game using the corners of an equilateral triangle as the reference points. Add a starting point at random (preferably inside the triangle). Then add the next point halfway between the starting point and one of the reference points. This reference point is chosen at random.

After a sufficient number of iterations, the image of a Sierpinski Triangle should emerge.




;See also
* [http://www.geoastro.de/ChaosSpiel/ChaosEnglish.html The Game of Chaos]





## BASIC

This should require minimal adaptation to work with any of the older Microsoft-style BASICs. Users of other dialects will need to replace lines <tt>10</tt> and <tt>150</tt> with the appropriate statements to select a graphics output mode (if necessary) and to plot a pixel at <tt>x,y</tt> in colour <tt>v</tt>; they should also add <tt>LET</tt> throughout and <tt>170 END</tt> if their dialects require those things.

```basic
10 SCREEN 1
20 X = INT(RND(0) * 200)
30 Y = INT(RND(0) * 173)
40 FOR I=1 TO 20000
50 V = INT(RND(0) * 3) + 1
60 ON V GOTO 70,100,130
70 X = X/2
80 Y = Y/2
90 GOTO 150
100 X = 100 + (100-X)/2
110 Y = 173 - (173-Y)/2
120 GOTO 150
130 X = 200 - (200-X)/2
140 Y = Y/2
150 PSET X,Y,V
160 NEXT I
```

=
## Applesoft BASIC
=
Adapted from the code given above.

```basic
10 HGR2
20 X = INT(RND(1) * 200)
30 Y = INT(RND(1) * 173)
40 FOR I=1 TO 20000
50 V = INT(RND(1) * 3) + 1
60 ON V GOTO 70,100,130
70 X = X/2
80 Y = Y/2
90 GOTO 150
100 X = 100 + (100-X)/2
110 Y = 173 - (173-Y)/2
120 GOTO 150
130 X = 200 - (200-X)/2
140 Y = Y/2
150 HCOLOR=V+4
160 HPLOT X,Y
170 NEXT I
```


=
## BASIC256
=

```BASIC256

#Chaos game

ancho = 500 : alto = 300
x = Int(Rand * ancho)
y = Int(Rand * alto)

Clg
FastGraphics
Graphsize ancho , alto

For iteracion = 1 To 30000
	vertice = Int(Rand * 3) + 1
	Begin Case
		Case vertice = 1
			x = x / 2
			y = y / 2
			Color red
		Case vertice = 2
			x = (ancho/2) + ((ancho/2)-x) / 2
			y = alto - (alto-y) / 2
			Color green
		Case vertice = 3
			x = ancho - (ancho-x) / 2
			y = y / 2
			Color blue
	End Case
	#Pset (x,y),vertice
	Plot (x,y)
Next iteracion
Refresh
ImgSave "chaos_game.jpg", "jpg"
End

```


=
## Sinclair ZX81 BASIC
=
Adapted from the other BASIC versions. Monochrome and low-resolution, of course. Works with only 1k of RAM. If you like, you can try changing line <code>30</code> to go round the loop a different number of times.

Note that ZX81 BASIC does not have an explicit computed <code>GOTO</code>; we can, however, actually compute the value of an expression and then <code>GOTO</code> it as a line number.

```basic
 10 LET X=RND*46
 20 LET Y=RND*40
 30 FOR I=1 TO 5000
 40 LET VERTEX=INT (RND*3)
 50 GOTO 60+VERTEX*30
 60 LET X=X/2
 70 LET Y=Y/2
 80 GOTO 140
 90 LET X=23+(23-X)/2
100 LET Y=40-(40-Y)/2
110 GOTO 140
120 LET X=46-(46-X)/2
130 LET Y=Y/2
140 PLOT X,42-Y
150 NEXT I
```

{{out}}
Screenshot [http://www.edmundgriffiths.com/zx81chaosgame.jpg here]. As with most ZX81 graphics, you can obtain the very best results by making it quite small and looking at it from a long way away.

=
## ZX Spectrum Basic
=
The final <code>INK</code> statement sets the foreground colour back to black.

```basic
 10 LET x=RND*200
 20 LET y=RND*173
 30 FOR i=1 TO 20000
 40 LET vertex=INT (RND*3)
 50 IF vertex=1 THEN GO TO 100
 60 IF vertex=2 THEN GO TO 130
 70 LET x=x/2
 80 LET y=y/2
 90 GO TO 150
100 LET x=100+(100-x)/2
110 LET y=173-(173-y)/2
120 GO TO 150
130 LET x=200-(200-x)/2
140 LET y=y/2
150 INK vertex+1
160 PLOT x,y
170 NEXT i
180 INK 0
```



## C


Interactive code which asks the side length of the starting triangle and number of iterations as inputs, a larger number of iterations produces a more accurate approximation of the Sierpinski fractal. Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.


```C

#include<graphics.h>
#include<stdlib.h>
#include<stdio.h>
#include<math.h>
#include<time.h>

#define pi M_PI

int main(){

	time_t t;
	double side, vertices[3][3],seedX,seedY,windowSide;
	int i,iter,choice;

	printf("Enter triangle side length : ");
	scanf("%lf",&side);

	printf("Enter number of iterations : ");
	scanf("%d",&iter);

	windowSide = 10 + 2*side;

	initwindow(windowSide,windowSide,"Sierpinski Chaos");

	for(i=0;i<3;i++){
		vertices[i][0] = windowSide/2 + side*cos(i*2*pi/3);
		vertices[i][1] = windowSide/2 + side*sin(i*2*pi/3);
		putpixel(vertices[i][0],vertices[i][1],15);
	}

	srand((unsigned)time(&t));

	seedX = rand()%(int)(vertices[0][0]/2 + (vertices[1][0] + vertices[2][0])/4);
	seedY = rand()%(int)(vertices[0][1]/2 + (vertices[1][1] + vertices[2][1])/4);

	putpixel(seedX,seedY,15);

	for(i=0;i<iter;i++){
		choice = rand()%3;

		seedX = (seedX + vertices[choice][0])/2;
		seedY = (seedY + vertices[choice][1])/2;

		putpixel(seedX,seedY,15);
	}

	getch();

	closegraph();

	return 0;
}
```



## C++

This program will generate the Sierpinski Triangle and save it to your hard drive.

```cpp

#include <windows.h>
#include <ctime>
#include <string>
#include <iostream>

const int BMP_SIZE = 600;

class myBitmap {
public:
    myBitmap() : pen( NULL ), brush( NULL ), clr( 0 ), wid( 1 ) {}
    ~myBitmap() {
        DeleteObject( pen ); DeleteObject( brush );
        DeleteDC( hdc ); DeleteObject( bmp );
    }
    bool create( int w, int h ) {
        BITMAPINFO bi;
        ZeroMemory( &bi, sizeof( bi ) );
        bi.bmiHeader.biSize        = sizeof( bi.bmiHeader );
        bi.bmiHeader.biBitCount    = sizeof( DWORD ) * 8;
        bi.bmiHeader.biCompression = BI_RGB;
        bi.bmiHeader.biPlanes      = 1;
        bi.bmiHeader.biWidth       =  w;
        bi.bmiHeader.biHeight      = -h;

        HDC dc = GetDC( GetConsoleWindow() );
        bmp = CreateDIBSection( dc, &bi, DIB_RGB_COLORS, &pBits, NULL, 0 );
        if( !bmp ) return false;

        hdc = CreateCompatibleDC( dc );
        SelectObject( hdc, bmp );
        ReleaseDC( GetConsoleWindow(), dc );

        width = w; height = h;
        return true;
    }
    void clear( BYTE clr = 0 ) {
        memset( pBits, clr, width * height * sizeof( DWORD ) );
    }
    void setBrushColor( DWORD bClr ) {
        if( brush ) DeleteObject( brush );
        brush = CreateSolidBrush( bClr );
        SelectObject( hdc, brush );
    }
    void setPenColor( DWORD c ) {
        clr = c; createPen();
    }
    void setPenWidth( int w ) {
        wid = w; createPen();
    }
    void saveBitmap( std::string path ) {
        BITMAPFILEHEADER fileheader;
        BITMAPINFO       infoheader;
        BITMAP           bitmap;
        DWORD            wb;

        GetObject( bmp, sizeof( bitmap ), &bitmap );
        DWORD* dwpBits = new DWORD[bitmap.bmWidth * bitmap.bmHeight];

        ZeroMemory( dwpBits, bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD ) );
        ZeroMemory( &infoheader, sizeof( BITMAPINFO ) );
        ZeroMemory( &fileheader, sizeof( BITMAPFILEHEADER ) );

        infoheader.bmiHeader.biBitCount = sizeof( DWORD ) * 8;
        infoheader.bmiHeader.biCompression = BI_RGB;
        infoheader.bmiHeader.biPlanes = 1;
        infoheader.bmiHeader.biSize = sizeof( infoheader.bmiHeader );
        infoheader.bmiHeader.biHeight = bitmap.bmHeight;
        infoheader.bmiHeader.biWidth = bitmap.bmWidth;
        infoheader.bmiHeader.biSizeImage = bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD );

        fileheader.bfType    = 0x4D42;
        fileheader.bfOffBits = sizeof( infoheader.bmiHeader ) + sizeof( BITMAPFILEHEADER );
        fileheader.bfSize    = fileheader.bfOffBits + infoheader.bmiHeader.biSizeImage;

        GetDIBits( hdc, bmp, 0, height, ( LPVOID )dwpBits, &infoheader, DIB_RGB_COLORS );

        HANDLE file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
        WriteFile( file, &fileheader, sizeof( BITMAPFILEHEADER ), &wb, NULL );
        WriteFile( file, &infoheader.bmiHeader, sizeof( infoheader.bmiHeader ), &wb, NULL );
        WriteFile( file, dwpBits, bitmap.bmWidth * bitmap.bmHeight * 4, &wb, NULL );
        CloseHandle( file );

        delete [] dwpBits;
    }
    HDC getDC() const     { return hdc; }
    int getWidth() const  { return width; }
    int getHeight() const { return height; }
private:
    void createPen() {
        if( pen ) DeleteObject( pen );
        pen = CreatePen( PS_SOLID, wid, clr );
        SelectObject( hdc, pen );
    }
    HBITMAP bmp; HDC    hdc;
    HPEN    pen; HBRUSH brush;
    void    *pBits; int    width, height, wid;
    DWORD    clr;
};
class chaos {
public:
    void start() {
        POINT org;
        fillPts(); initialPoint( org ); initColors();
        int cnt = 0, i;
        bmp.create( BMP_SIZE, BMP_SIZE );
        bmp.clear( 255 );

        while( cnt++ < 1000000 ) {
            switch( rand() % 6 ) {
                case 0: case 3: i = 0; break;
                case 1: case 5: i = 1; break;
                case 2: case 4: i = 2;
            }
            setPoint( org, myPoints[i], i );
        }
        // --- edit this path --- //
        bmp.saveBitmap( "F:/st.bmp" );
    }
private:
    void setPoint( POINT &o, POINT v, int i ) {
        POINT z;
        o.x = ( o.x + v.x ) >> 1; o.y = ( o.y + v.y ) >> 1;
        SetPixel( bmp.getDC(), o.x, o.y, colors[i] );
    }
    void fillPts() {
        int a = BMP_SIZE - 1;
        myPoints[0].x = BMP_SIZE >> 1; myPoints[0].y = 0;
        myPoints[1].x = 0; myPoints[1].y = myPoints[2].x = myPoints[2].y = a;
    }
    void initialPoint( POINT& p ) {
        p.x = ( BMP_SIZE >> 1 ) + rand() % 2 ? rand() % 30 + 10 : -( rand() % 30 + 10 );
        p.y = ( BMP_SIZE >> 1 ) + rand() % 2 ? rand() % 30 + 10 : -( rand() % 30 + 10 );
    }
    void initColors() {
        colors[0] = RGB( 255, 0, 0 );
        colors[1] = RGB( 0, 255, 0 );
        colors[2] = RGB( 0, 0, 255 );
    }

    myBitmap bmp;
    POINT myPoints[3];
    COLORREF colors[3];
};
int main( int argc, char* argv[] ) {
    srand( ( unsigned )time( 0 ) );
    chaos c; c.start();
    return 0;
}

```


## C#


```c#
using System.Diagnostics;
using System.Drawing;

namespace RosettaChaosGame
{
    class Program
    {
        static void Main(string[] args)
        {
            var bm = new Bitmap(600, 600);

            var referencePoints = new Point[] {
                new Point(0, 600),
                new Point(600, 600),
                new Point(300, 81)
            };
            var r = new System.Random();
            var p = new Point(r.Next(600), r.Next(600));
            for (int count = 0; count < 10000; count++)
            {
                bm.SetPixel(p.X, p.Y, Color.Magenta);
                int i = r.Next(3);
                p.X = (p.X + referencePoints[i].X) / 2;
                p.Y = (p.Y + referencePoints[i].Y) / 2;
            }
            const string filename = "Chaos Game.png";
            bm.Save(filename);
            Process.Start(filename);
        }
    }
}
```



## Common Lisp

{{libheader|opticl}}

```lisp
(defpackage #:chaos
  (:use #:cl
        #:opticl))

(in-package #:chaos)

(defparameter *image-size* 600)
(defparameter *margin* 50)
(defparameter *edge-size* (- *image-size* *margin* *margin*))
(defparameter *iterations* 1000000)

(defun chaos ()
  (let ((image (make-8-bit-rgb-image *image-size* *image-size* :initial-element 255))
        (a (list (- *image-size* *margin*) *margin*))
        (b (list (- *image-size* *margin*) (- *image-size* *margin*)))
        (c (list (- *image-size* *margin* (round (* (tan (/ pi 3)) *edge-size*) 2))
                 (round *image-size* 2)))
        (point (list (+ (random *edge-size*) *margin*)
                     (+ (random *edge-size*) *margin*))))
    (dotimes (i *iterations*)
      (let ((ref (ecase (random 3)
                   (0 a)
                   (1 b)
                   (2 c))))
        (setf point (list (round (+ (first  point) (first  ref)) 2)
                          (round (+ (second point) (second ref)) 2))))
      (setf (pixel image (first point) (second point))
            (values 255 0 0)))
    (write-png-file "chaos.png" image)))
```


## EasyLang


[https://easylang.online/apps/run.html?code=x%5B%5D%20%3D%20%5B%200%20100%2050%20%5D%0Ay%5B%5D%20%3D%20%5B%2093%2093%207%20%5D%0Ac%5B%5D%20%3D%20%5B%20900%20090%20009%20%5D%0Afor%20i%20range%20100000%0Ah%20%3D%20random%203%0Ax%23%20%3D%20%28x%23%20%2B%20x%5Bh%5D%29%20/%202%0Ay%23%20%3D%20%28y%23%20%2B%20y%5Bh%5D%29%20/%202%0Acolor%20c%5Bh%5D%0Amove%20x%23%20y%23%0Arect%200.3%200.3%0A. Run it]

<lang>x[] = [ 0 100 50 ]
y[] = [ 93 93 7 ]
c[] = [ 900 090 009 ]
for i range 100000
  h = random 3
  x# = (x# + x[h]) / 2
  y# = (y# + y[h]) / 2
  color c[h]
  move x# y#
  rect 0.3 0.3
.
```



## Fortran

This FORTRAN code creates an output file which can be drawn with gnuplot.

```Fortran

PROGRAM CHAOS
 IMPLICIT NONE
 REAL, DIMENSION(3):: KA, KN ! Koordinates old/new
 REAL, DIMENSION(3):: DA, DB, DC ! Triangle
 INTEGER:: I, Z
 INTEGER, PARAMETER:: UT = 17
 ! Define corners of triangle
 DA = (/    0., 0.,   0. /)
 DB = (/  600., 0.,   0. /)
 DC = (/  500., 0., 400. /)
 ! Define starting point
 KA = (/  500., 0., 100. /)
 OPEN (UNIT = UT, FILE = 'aus.csv')
 DO I=1, 1000000
  Z = ZAHL()
  WRITE (UT, '(3(F12.6, ";"))') KA
  SELECT CASE (Z)
   CASE (1)
    CALL MITTELP(KA, DA, KN)
   CASE (2)
    CALL MITTELP(KA, DB, KN)
   CASE (3)
    CALL MITTELP(KA, DC, KN)
  END SELECT
  KA = KN
 END DO
 CLOSE (UT)
 CONTAINS
  ! Calculates center of two points
  SUBROUTINE MITTELP(P1, P2, MP)
   REAL, INTENT(IN), DIMENSION(3):: P1, P2
   REAL, INTENT(OUT), DIMENSION(3):: MP
   MP = (P1 + P2) / 2.
  END SUBROUTINE MITTELP
  ! Returns random number
  INTEGER FUNCTION ZAHL()
   REAL:: ZZ
   CALL RANDOM_NUMBER(ZZ)
   ZZ = ZZ * 3.
   ZAHL = FLOOR(ZZ) + 1
   IF (ZAHL .GT. 3) ZAHL = 3
  END FUNCTION ZAHL
END PROGRAM CHAOS

```

Gnuplot Code to draw file:

```Gnuplot

set terminal jpeg enhanced size 1600,960
set output 'chaos.jpg'
set nokey
set style line 1 lc rgb '#0060ad' lt 1 lw 3 pt 7 ps 0.3
plot 'aus.csv' using 1:3 with points ls 1 notitle

```




## FreeBASIC

{{trans|BASIC256}}

```freebasic

' Chaos game
Const ancho = 320, alto = 240
Dim As Integer x, y, iteracion, vertice
x = Int(Rnd * ancho)
y = Int(Rnd * alto)

Screenres ancho, alto, 8
Cls

For iteracion = 1 To 30000
	vertice = Int(Rnd * 3) + 1
	Select Case vertice
    Case 1
        x = x / 2
        y = y / 2
        vertice = 4 'red
    Case 2
        x = (ancho/2) + ((ancho/2)-x) / 2
        y = alto - (alto-y) / 2
        vertice = 2 'green
    Case 3
        x = ancho - (ancho-x) / 2
        y = y / 2
        vertice = 1 'blue
    End Select
	Pset (x,y),vertice
Next iteracion
Sleep
End

```



## Go

This writes a simple GIF animation of the method.

```Go
package main

import (
	"fmt"
	"image"
	"image/color"
	"image/draw"
	"image/gif"
	"log"
	"math"
	"math/rand"
	"os"
	"time"
)

var bwPalette = color.Palette{
	color.Transparent,
	color.White,
	color.RGBA{R: 0xff, A: 0xff},
	color.RGBA{G: 0xff, A: 0xff},
	color.RGBA{B: 0xff, A: 0xff},
}

func main() {
	const (
		width          = 160
		frames         = 100
		pointsPerFrame = 50
		delay          = 100 * time.Millisecond
		filename       = "chaos_anim.gif"
	)

	var tan60 = math.Sin(math.Pi / 3)
	height := int(math.Round(float64(width) * tan60))
	b := image.Rect(0, 0, width, height)
	vertices := [...]image.Point{
		{0, height}, {width, height}, {width / 2, 0},
	}

	// Make a filled triangle.
	m := image.NewPaletted(b, bwPalette)
	for y := b.Min.Y; y < b.Max.Y; y++ {
		bg := int(math.Round(float64(b.Max.Y-y) / 2 / tan60))
		for x := b.Min.X + bg; x < b.Max.X-bg; x++ {
			m.SetColorIndex(x, y, 1)
		}
	}

	// Pick starting point
	var p image.Point
	rand.Seed(time.Now().UnixNano())
	p.Y = rand.Intn(height) + b.Min.Y
	p.X = rand.Intn(width) + b.Min.X // TODO: make within triangle

	anim := newAnim(frames, delay)
	addFrame(anim, m)
	for i := 1; i < frames; i++ {
		for j := 0; j < pointsPerFrame; j++ {
			// Pick a random vertex
			vi := rand.Intn(len(vertices))
			v := vertices[vi]
			// Move p halfway there
			p.X = (p.X + v.X) / 2
			p.Y = (p.Y + v.Y) / 2
			m.SetColorIndex(p.X, p.Y, uint8(2+vi))
		}
		addFrame(anim, m)
	}
	if err := writeAnim(anim, filename); err != nil {
		log.Fatal(err)
	}
	fmt.Printf("wrote to %q\n", filename)
}

// Stuff for making a simple GIF animation.

func newAnim(frames int, delay time.Duration) *gif.GIF {
	const gifDelayScale = 10 * time.Millisecond
	g := &gif.GIF{
		Image: make([]*image.Paletted, 0, frames),
		Delay: make([]int, 1, frames),
	}
	g.Delay[0] = int(delay / gifDelayScale)
	return g
}
func addFrame(anim *gif.GIF, m *image.Paletted) {
	b := m.Bounds()
	dst := image.NewPaletted(b, m.Palette)
	draw.Draw(dst, b, m, image.ZP, draw.Src)
	anim.Image = append(anim.Image, dst)
	if len(anim.Delay) < len(anim.Image) {
		anim.Delay = append(anim.Delay, anim.Delay[0])
	}
}
func writeAnim(anim *gif.GIF, filename string) error {
	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	err = gif.EncodeAll(f, anim)
	if cerr := f.Close(); err == nil {
		err = cerr
	}
	return err
}
```



## Gnuplot

{{trans|PARI/GP}}
{{Works with|gnuplot|5.0 (patchlevel 3) and above}}
[[File:ChGS3Gnu1.png|right|thumb|Output ChGS3Gnu1.png]]


```gnuplot

## Chaos Game  (Sierpinski triangle) 2/16/17 aev
reset
fn="ChGS3Gnu1"; clr='"red"';
ttl="Chaos Game  (Sierpinski triangle)"
sz=600; sz1=sz/2; sz2=sz1*sqrt(3);
x=y=xf=yf=v=0;
dfn=fn.".dat"; ofn=fn.".png";
set terminal png font arial 12 size 640,640
set print dfn append
set output ofn
unset border; unset xtics; unset ytics; unset key;
set size square
set title ttl font "Arial:Bold,12"
lim=30000; max=100; x=y=xw=yw=p=0;
randgp(top) = floor(rand(0)*top)
x=randgp(sz); y=randgp(sz2);
do for [i=1:lim] {
  v=randgp(3);
  if (v==0) {x=x/2; y=y/2}
  if (v==1) {x=sz1+(sz1-x)/2; y=sz2-(sz2-y)/2}
  if (v==2) {x=sz-(sz-x)/2; y=y/2}
  xf=floor(x); yf=floor(y);
  if(!(xf<1||xf>sz||yf<1||yf>sz)) {print xf," ",yf};
}
plot dfn using 1:2 with points  pt 7 ps 0.5 lc @clr
set output
unset print

```

{{Output}}

```txt

File: ChGS3Gnu1.png

```



## Haskell



```haskell
import Control.Monad (replicateM)
import Control.Monad.Random (fromList)

type Point = (Float,Float)
type Transformations = [(Point -> Point, Float)] -- weighted transformations

-- realization of the game for given transformations
gameOfChaos :: MonadRandom m => Int -> Transformations -> Point -> m [Point]
gameOfChaos n transformations x = iterateA (fromList transformations) x
  where iterateA f x = scanr ($) x <$> replicateM n f
```


Some transformations:


```haskell
-- the Sierpinsky`s triangle
triangle = [ (mid (0, 0), 1)
           , (mid (1, 0), 1)
           , (mid (0.5, 0.86), 1) ]
  where mid (a,b) (x,y) = ((a+x)/2, (b+y)/2)

-- the Barnsley's fern
fern = [(f1, 1), (f2, 85), (f3, 7), (f4, 7)]
  where f1 (x,y) = (0, 0.16*y)
        f2 (x,y) = (0.85*x + 0.04*y, -0.04*x + 0.85*y + 1.6)
        f3 (x,y) = (0.2*x - 0.26*y, 0.23*x + 0.22*y + 1.6)
        f4 (x,y) = (-0.15*x + 0.28*y, 0.26*x + 0.24*y + 0.44)

-- A dragon curve
dragon = [(f1, 1), (f2, 1)]
  where f1 (x,y) = (0.5*x - 0.5*y, 0.5*x + 0.5*y)
        f2 (x,y) = (-0.5*x + 0.5*y+1, -0.5*x - 0.5*y)
```


Drawing the result:

```haskell
import Control.Monad.Random (getRandomR)
import Graphics.Gloss

main = do x <- getRandomR (0,1)
          y <- getRandomR (0,1)
          pts <- gameOfChaos 500000 triangle (x,y)
          display window white $ foldMap point pts
            where window = InWindow "Game of Chaos" (400,400) (0,0)
                  point (x,y) = translate (100*x) (100*y) $ circle 0.02
```



## J

[[File:j_chaos_game.png|300px|thumb|right]]

```j

Note 'plan, Working in complex plane'
  Make an equilateral triangle.
  Make a list of N targets
  Starting with a random point near the triangle,
    iteratively generate new points.
  plot the new points.

  j has a particularly rich notation for numbers.

    1ad_90 specifies a complex number with radius 1
    at an angle of negative 90 degrees.

    2p1 is 2 times (pi raised to the first power).
)

N=: 3000

require'plot'
TAU=: 2p1 NB. tauday.com
mean=: +/ % #

NB. equilateral triangle with vertices on unit circle
NB. rotated for fun.
TRIANGLE=: *(j./2 1 o.(TAU%6)*?0)*1ad_90 1ad150 1ad30

TARGETS=: (N ?@:# 3) { TRIANGLE

NB. start on unit circle
START=: j./2 1 o.TAU*?0

NEW_POINTS=: (mean@:(, {.) , ])/ TARGETS , START

'marker'plot NEW_POINTS

```



## Java

[[File:chaos_game.png|300px|thumb|right]]
{{works with|Java|8}}

```java
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.Timer;

public class ChaosGame extends JPanel {
    class ColoredPoint extends Point {
        int colorIndex;

        ColoredPoint(int x, int y, int idx) {
            super(x, y);
            colorIndex = idx;
        }
    }

    Stack<ColoredPoint> stack = new Stack<>();
    Point[] points = new Point[3];
    Color[] colors = {Color.red, Color.green, Color.blue};
    Random r = new Random();

    public ChaosGame() {
        Dimension dim = new Dimension(640, 640);
        setPreferredSize(dim);
        setBackground(Color.white);

        int margin = 60;
        int size = dim.width - 2 * margin;

        points[0] = new Point(dim.width / 2, margin);
        points[1] = new Point(margin, size);
        points[2] = new Point(margin + size, size);

        stack.push(new ColoredPoint(-1, -1, 0));

        new Timer(10, (ActionEvent e) -> {
            if (stack.size() < 50_000) {
                for (int i = 0; i < 1000; i++)
                    addPoint();
                repaint();
            }
        }).start();
    }

    private void addPoint() {
        try {
            int colorIndex = r.nextInt(3);
            Point p1 = stack.peek();
            Point p2 = points[colorIndex];
            stack.add(halfwayPoint(p1, p2, colorIndex));
        } catch (EmptyStackException e) {
            System.out.println(e);
        }
    }

    void drawPoints(Graphics2D g) {
        for (ColoredPoint p : stack) {
            g.setColor(colors[p.colorIndex]);
            g.fillOval(p.x, p.y, 1, 1);
        }
    }

    ColoredPoint halfwayPoint(Point a, Point b, int idx) {
        return new ColoredPoint((a.x + b.x) / 2, (a.y + b.y) / 2, idx);
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawPoints(g);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Chaos Game");
            f.setResizable(false);
            f.add(new ChaosGame(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

Plots the fractal on an HTML <tt>canvas</tt> element.

```javascript
<html


<head>

<meta charset="UTF-8">

<title>Chaos Game</title>

</head>

<body>

<p>
<canvas id="sierpinski" width=400 height=346></canvas>
</p>

<p>
<button onclick="chaosGame()">Click here to see a Sierpiński triangle</button>
</p>

<script>

function chaosGame() {
    var canv = document.getElementById('sierpinski').getContext('2d');
    var x = Math.random() * 400;
    var y = Math.random() * 346;
    for (var i=0; i<30000; i++) {
        var vertex = Math.floor(Math.random() * 3);
        switch(vertex) {
            case 0:
                x = x / 2;
                y = y / 2;
                canv.fillStyle = 'green';
                break;
            case 1:
                x = 200 + (200 - x) / 2
                y = 346 - (346 - y) / 2
                canv.fillStyle = 'red';
                break;
            case 2:
                x = 400 - (400 - x) / 2
                y = y / 2;
                canv.fillStyle = 'blue';
        }
        canv.fillRect(x,y, 1,1);
    }
}

</script>

</body>

</html>
```



## Julia

{{works with|Julia|0.6}}

```julia

using Luxor
width  = 1000;
height = 1000;
Drawing(width, height, "./chaos.png");
t = Turtle(0, 0, true, 0, (0., 0., 0.));

x = rand(1:width);
y = rand(1:height);

for l in 1:30_000
    v = rand(1:3);
    if v == 1
        x /= 2;
        y /= 2;
    elseif v == 2
        x = width/2 + (width/2 - x)/2;
        y = height - (height - y)/2;
    else
        x = width - (width - x)/2;
        y = y / 2;
    end
    Reposition(t, x, height-y);
    Circle(t, 3);
end

finish();
preview();

```



## Kotlin

{{trans|Java}}

```scala
//Version 1.1.51

import java.awt.*
import java.util.Stack
import java.util.Random
import javax.swing.JPanel
import javax.swing.JFrame
import javax.swing.Timer
import javax.swing.SwingUtilities

class ChaosGame : JPanel() {

    class ColoredPoint(x: Int, y: Int, val colorIndex: Int) : Point(x, y)

    val stack = Stack<ColoredPoint>()
    val points: List<Point>
    val colors = listOf(Color.red, Color.green, Color.blue)
    val r = Random()

    init {
        val dim = Dimension(640, 640)
        preferredSize = dim
        background = Color.white
        val margin = 60
        val size = dim.width - 2 * margin
        points = listOf(
            Point(dim.width / 2, margin),
            Point(margin, size),
            Point(margin + size, size)
        )
        stack.push(ColoredPoint(-1, -1, 0))

        Timer(10) {
            if (stack.size < 50_000) {
                for (i in 0 until 1000) addPoint()
                repaint()
            }
        }.start()
    }

    private fun addPoint() {
        val colorIndex = r.nextInt(3)
        val p1 = stack.peek()
        val p2 = points[colorIndex]
        stack.add(halfwayPoint(p1, p2, colorIndex))
    }

    fun drawPoints(g: Graphics2D) {
        for (cp in stack) {
            g.color = colors[cp.colorIndex]
            g.fillOval(cp.x, cp.y, 1, 1)
        }
    }

    fun halfwayPoint(a: Point, b: Point, idx: Int) =
        ColoredPoint((a.x + b.x) / 2, (a.y + b.y) / 2, idx)

    override fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                            RenderingHints.VALUE_ANTIALIAS_ON)
        drawPoints(g)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        with (f) {
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            title = "Chaos Game"
            isResizable = false
            add(ChaosGame(), BorderLayout.CENTER)
            pack()
            setLocationRelativeTo(null)
            isVisible = true
        }
    }
}
```


{{output}}

```txt

Same as Java entry

```



## Logo


```logo
to chaosgame :sidelength :iterations
    make "width :sidelength
    make "height (:sidelength/2 * sqrt 3)
    make "x (random :width)
    make "y (random :height)
    repeat :iterations [
        make "vertex (random 3)
        if :vertex = 0 [
            make "x (:x / 2)
            make "y (:y / 2)
            setpencolor "green
        ]
        if :vertex = 1 [
            make "x (:width / 2 + ((:width / 2 - :x) / 2))
            make "y (:height - ((:height - :y) / 2))
            setpencolor "red
        ]
        if :vertex = 2 [
            make "x (:width - ((:width - :x) / 2))
            make "y (:y / 2)
            setpencolor "blue
        ]
        penup
        setxy (:x - :width / 2) (:y - :height / 2)
        pendown
        forward 1
    ]
    hideturtle
end
```



## Lua

Needs L&Ouml;VE 2d Engine

```Lua

math.randomseed( os.time() )
colors, orig = { { 255, 0, 0 }, { 0, 255, 0 }, { 0, 0, 255 } }, {}

function love.load()
    wid, hei = love.graphics.getWidth(), love.graphics.getHeight()

    orig[1] = { wid / 2, 3 }
    orig[2] = { 3, hei - 3 }
    orig[3] = { wid - 3, hei - 3 }
    local w, h = math.random( 10, 40 ), math.random( 10, 40 )
    if math.random() < .5 then w = -w end
    if math.random() < .5 then h = -h end
    orig[4] = { wid / 2 + w, hei / 2 + h }

    canvas = love.graphics.newCanvas( wid, hei )
    love.graphics.setCanvas( canvas ); love.graphics.clear()
    love.graphics.setColor( 255, 255, 255 )
    love.graphics.points( orig )
    love.graphics.setCanvas()
end
function love.draw()
    local iter = 100 --> make this number bigger to speed up rendering
    for rp = 1, iter do
        local r, pts = math.random( 6 ), {}
        if r == 1 or r == 4 then
            pt = 1
        elseif r == 2 or r == 5 then
            pt = 2
        else
            pt = 3
        end
        local x, y = ( orig[4][1] + orig[pt][1] ) / 2, ( orig[4][2] + orig[pt][2] ) / 2
        orig[4][1] = x; orig[4][2] = y
        pts[1] = { x, y, colors[pt][1], colors[pt][2], colors[pt][3], 255 }
        love.graphics.setCanvas( canvas )
        love.graphics.points( pts )
    end
    love.graphics.setCanvas()
    love.graphics.draw( canvas )
end

```



## Mathematica


```Mathematica


points = 5000;
a = {0, 0};
b = {1, 0};
c = {0.5, 1};
d = {.7, .3};
S = {};
For[i = 1, i < points, i++, t = RandomInteger[2];
 If[t == 0, d = Mean[{a, d}],
  If[t == 1, d = Mean[{b, d}], d = Mean[{c, d}]]]; AppendTo[S, d]]
Graphics[Point[S]]

```



## Maple


```maple
chaosGame := proc(numPoints)
	local points, i;
	randomize();
	use geometry in
	RegularPolygon(triSideways, 3, point(cent, [0, 0]), 1);
	rotation(tri, triSideways, Pi/2, counterclockwise);
	randpoint(currentP, -1/2*sqrt(3)..1/2*sqrt(3), -1/2..1/2);
	points := [coordinates(currentP)];
	for i to numPoints do
		midpoint(mid, currentP, parse(cat("rotate_triSideways_", rand(1..3)(), "_tri")));
		points := [op(points), coordinates(mid)];
		point(currentP, coordinates(mid));
	end do:
	end use;
	use plottools in
		plots:-display( seq([plots:-display([seq(point(points[i]), i = 1..j)])], j = 1..numelems(points) ), insequence=true);
	end use;
end proc:
```



## Nim

{{libheader|rapid}}

```nim
import random

import rapid/gfx

var
  window = initRWindow()
    .title("Rosetta Code - Chaos Game")
    .open()
  surface = window.openGfx()
  sierpinski = window.newRCanvas()
  points: array[3, Vec2[float]]

for i in 0..<3:
  points[i] = vec2(cos(PI * 2 / 3 * i.float), sin(PI * 2 / 3 * i.float)) * 300

var point = vec2(rand(0.0..surface.width), rand(0.0..surface.height))

surface.vsync = false
surface.loop:
  draw ctx, step:
    let vertex = sample(points)
    point = (point + vertex) / 2
    ctx.renderTo(sierpinski):
      ctx.transform():
        ctx.translate(surface.width / 2, surface.height / 2)
        ctx.rotate(-PI / 2)
        ctx.begin()
        ctx.point((point.x, point.y))
        ctx.draw(prPoints)
    ctx.clear(gray(0))
    ctx.begin()
    ctx.texture = sierpinski
    ctx.rect(0, 0, surface.width, surface.height)
    ctx.draw()
    ctx.noTexture()
  update step:
    discard
```



## PARI/GP

Note: Find plotmat() here on RosettaCode Wiki.
{{Works with|PARI/GP|2.9.1 and above}}
[[File:SierpTri1.png|right|thumb|Output SierpTri1.png]]

```parigp

\\ Chaos Game  (Sierpinski triangle) 2/15/17 aev
pChaosGameS3(size,lim)={
my(sz1=size\2,sz2=sz1*sqrt(3),M=matrix(size,size),x,y,xf,yf,v);
x=random(size); y=random(sz2);
for(i=1,lim, v=random(3);
  if(v==0, x/=2; y/=2;);
  if(v==1, x=sz1+(sz1-x)/2; y=sz2-(sz2-y)/2;);
  if(v==2, x=size-(size-x)/2; y/=2;);
  xf=floor(x); yf=floor(y); if(xf<1||xf>size||yf<1||yf>size, next);
  M[xf,yf]=1;
);\\fend
plotmat(M);
}
\\ Test:
pChaosGameS3(600,30000); \\ SierpTri1.png

```

{{Output}}

```txt

> pChaosGameS3(600,30000); \\ SierpTri1.png
 *** matrix(600x600) 18696 DOTS
time = 751 ms.

```


## Pascal


```Pascal

program ChaosGame;

// FPC 3.0.2
uses
  Graph, windows, math;

// Return a point on a circle defined by angle and the circles radius
// Angle 0 = Radius points to the left
// Angle 90 = Radius points upwards
Function PointOfCircle(Angle: SmallInt; Radius: integer): TPoint;
var Ia: Double;
begin
  Ia:=DegToRad(-Angle);
  result.x:=round(cos(Ia)*Radius);
  result.y:=round(sin(Ia)*Radius);
end;

{ Main }

var
  GraphDev,GraphMode: smallint;
  Triangle: array[0..2] of Tpoint; // Corners of the triangle
  TriPnt: Byte;                    // Point in ^^^^
  Origin: TPoint;                  // Defines center of triangle
  Itterations: integer;            // Number of Itterations
  Radius: Integer;
  View: viewPorttype;
  CurPnt: TPoint;
  Rect: TRect;
  Counter: integer;
begin

  Repeat {forever}

    // Get the Itteration count 0=exit
    Write('Itterations: ');
    ReadLn(Itterations);

    if Itterations=0 then halt;

    // Set Up Graphics screen (everythings Auto detect)
    GraphDev:=Detect;
    GraphMode:=0;
    InitGraph(GraphDev,GraphMode,'');
    if GraphResult<>grok then
    begin
      Writeln('Graphics doesn''t work');
      Halt;
    end;

    // set Origin to center of the _Triangle_ (Not the creen)
    GetViewSettings(View);
    Rect.Create(View.x1,View.y1+10,View.x2,View.y2-10);
    Origin:=Rect.CenterPoint;
    Origin.Offset(0,Rect.Height div 6);  //  Center Triangle on screen

    // Define Equilateral triangle,
    Radius:=Origin.y;         // Radius of Circumscribed circle
    for Counter:=0 to 2 do
      Triangle[Counter]:=PointOfCircle((Counter*120)+90,Radius)+Origin;

    // Choose random starting point, in the incsribed circle of the triangle
    Radius:=Radius div 2;     // Radius of inscribed circle
    CurPnt:=PointOfCircle(random(360),random(Radius div 2))+Origin;

    // Play the Chaos Game
    for Counter:=0 to Itterations do
    begin
      TriPnt:=Random(3);                      // Select Triangle Point
      Rect.Create(Triangle[TriPnt],CurPnt);;  // Def. rect. between TriPnt and CurPnt
      CurPnt:=Rect.CenterPoint;               // New CurPnt is center of rectangle
      putPixel(CurPnt.x,CurPnt.y,cyan);       // Plot the new CurPnt
    end;

  until False;
end.


```



## Perl


```perl
use Imager;

my $width  = 1000;
my $height = 1000;

my @points = (
    [ $width/2,         0],
    [        0, $height-1],
    [$height-1, $height-1],
);

my $img = Imager->new(
                      xsize    => $width,
                      ysize    => $height,
                      channels => 3,
                     );

my $color = Imager::Color->new('#ff0000');
my $r = [int(rand($width)), int(rand($height))];

foreach my $i (1 .. 100000) {
    my $p = $points[rand @points];

    my $h = [
        int(($p->[0] + $r->[0]) / 2),
        int(($p->[1] + $r->[1]) / 2),
    ];

    $img->setpixel(
        x     => $h->[0],
        y     => $h->[1],
        color => $color,
    );

    $r = $h;
}

$img->write(file => 'chaos_game_triangle.png');
```



## Perl 6

{{works with|Rakudo|2018.10}}


```perl6
use Image::PNG::Portable;

my ($w, $h) = (640, 640);

my $png = Image::PNG::Portable.new: :width($w), :height($h);

my @vertex = [0, 0], [$w, 0], [$w/2, $h];

my @xy = [0,0], [0,0], [0,0], [0,0];

# :degree must be equal to or less than @xy elements.
(^1e5).race(:4degree).map: {
    my $p = ++$ % +@xy;
    @xy[$p] = do given @vertex.pick -> @v { ((@xy[$p] »+« @v) »/» 2)».Int };
    $png.set: |@xy[$p], 0, 255, 0;
}

$png.write: 'Chaos-game-perl6.png';
```



## Phix

Implements five of the fractals on the wikipedia page.
{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\Chaos_game.exw
--
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

enum TRI,SQ1,SQ2,SQ3,PENT

sequence descs = {"Sierpinsky Triangle",
                  "Square 1",
                  "Square 2",
                  "Square 3",
                  "Pentagon"}

integer mode = TRI

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    atom {w,h} = IupGetIntInt(canvas, "DRAWSIZE")
    atom {x,y} = {w*0.05,h*0.05}
    {w,h} = {w*0.9,h*0.9}
    sequence points = iff(mode<SQ1?{{x,y},{x+w/2,y+h},{x+w,y}}:
                      iff(mode<PENT?{{x,y},{x,y+h},{x+w,y+h},{x+w,y}}
                                   :{{x+w/6,y},{x,y+h*2/3},{x+w/2,y+h},{x+w,y+h*2/3},{x+w*5/6,y}}))
    cdCanvasActivate(cddbuffer)
    integer last = 0
    for i=1 to 1000 do
        integer r = rand(length(points))
        if mode=TRI or r!=last then
            atom {nx,ny} = points[r]
            {x,y} = {(x+nx)/2,(y+ny)/2}
            cdCanvasPixel(cddbuffer, x, y, CD_GREY)
            if mode=SQ2
            or mode=SQ3 then
                r = mod(r,length(points))+1
                if mode=SQ3 then
                    r = mod(r,length(points))+1
                end if
            end if
            last = r
        end if
    end for
    cdCanvasFlush(cddbuffer)
    IupSetStrAttribute(dlg, "TITLE", "Chaos Game (%s)", {descs[mode]})
    return IUP_DEFAULT
end function

function timer_cb(Ihandle /*ih*/)
    IupUpdate(canvas)
    return IUP_IGNORE
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    cdCanvasSetForeground(cddbuffer, CD_GRAY)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c=' ' then
        mode += 1
        if mode>PENT then
            mode = TRI
        end if
        cdCanvasClear(cddbuffer)
        IupRedraw(canvas)
    end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "640x640")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Chaos Game")
    IupSetCallback(dlg, "K_ANY", Icallback("esc_close"))

    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    Ihandle timer = IupTimer(Icallback("timer_cb"), 40)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## Processing


```java
size(300, 260);

background(#ffffff); // white

int x = floor(random(width));
int y = floor(random(height));

int colour = #ffffff;

for (int i=0; i<30000; i++) {
  int v = floor(random(3));
  switch (v) {
  case 0:
    x = x / 2;
    y = y / 2;
    colour = #00ff00; // green
    break;
  case 1:
    x = width/2 + (width/2 - x)/2;
    y = height - (height - y)/2;
    colour = #ff0000; // red
    break;
  case 2:
    x = width - (width - x)/2;
    y = y / 2;
    colour = #0000ff; // blue
  }
  set(x, height-y, colour);
}
```



## Python



```Python

import argparse
import random
import shapely.geometry as geometry
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation


def main(args):
    # Styles
    plt.style.use("ggplot")

    # Creating figure
    fig = plt.figure()
    line, = plt.plot([], [], ".")

    # Limit axes
    plt.xlim(0, 1)
    plt.ylim(0, 1)

    # Titles
    title = "Chaos Game"
    plt.title(title)
    fig.canvas.set_window_title(title)

    # Getting data
    data = get_data(args.frames)

    # Creating animation
    line_ani = animation.FuncAnimation(
        fig=fig,
        func=update_line,
        frames=args.frames,
        fargs=(data, line),
        interval=args.interval,
        repeat=False
    )

    # To save the animation install ffmpeg and uncomment
    # line_ani.save("chaos_game.gif")

    plt.show()


def get_data(n):
    """
    Get data to plot
    """
    leg = 1
    triangle = get_triangle(leg)
    cur_point = gen_point_within_poly(triangle)
    data = []
    for _ in range(n):
        data.append((cur_point.x, cur_point.y))
        cur_point = next_point(triangle, cur_point)
    return data


def get_triangle(n):
    """
    Create right triangle
    """
    ax = ay = 0.0
    a = ax, ay

    bx = 0.5  *  n
    by = 0.75 * (n ** 2)
    b = bx, by

    cx = n
    cy = 0.0
    c = cx, cy

    triangle = geometry.Polygon([a, b, c])
    return triangle


def gen_point_within_poly(poly):
    """
    Generate random point inside given polygon
    """
    minx, miny, maxx, maxy = poly.bounds
    while True:
        x = random.uniform(minx, maxx)
        y = random.uniform(miny, maxy)
        point = geometry.Point(x, y)
        if point.within(poly):
            return point


def next_point(poly, point):
    """
    Generate next point according to chaos game rules
    """
    vertices = poly.boundary.coords[:-1]  # Last point is the same as the first one
    random_vertex = geometry.Point(random.choice(vertices))
    line = geometry.linestring.LineString([point, random_vertex])
    return line.centroid


def update_line(num, data, line):
    """
    Update line with new points
    """
    new_data = zip(*data[:num]) or [(), ()]
    line.set_data(new_data)
    return line,


if __name__ == "__main__":
    arg_parser = argparse.ArgumentParser(description="Chaos Game by Suenweek (c) 2017")
    arg_parser.add_argument("-f", dest="frames", type=int, default=1000)
    arg_parser.add_argument("-i", dest="interval", type=int, default=10)

    main(arg_parser.parse_args())


```




## R

Note: Find plotmat() here on RosettaCode Wiki.
{{trans|PARI/GP}}
{{Works with|R|3.3.1 and above}}
[[File:SierpTriR1.png|right|thumb|Output SierpTriR1.png]]

```r

# Chaos Game  (Sierpinski triangle) 2/15/17 aev
# pChaosGameS3(size, lim, clr, fn, ttl)
# Where: size - defines matrix and picture size; lim - limit of the dots;
#   fn - file name (.ext will be added); ttl - plot title;
pChaosGameS3 <- function(size, lim, clr, fn, ttl)
{
  cat(" *** START:", date(), "size=",size, "lim=",lim, "clr=",clr, "\n");
  sz1=floor(size/2); sz2=floor(sz1*sqrt(3)); xf=yf=v=0;
  M <- matrix(c(0), ncol=size, nrow=size, byrow=TRUE);
  x <- sample(1:size, 1, replace=FALSE);
  y <- sample(1:sz2, 1, replace=FALSE);
  pf=paste0(fn, ".png");
  for (i in 1:lim) { v <- sample(0:3, 1, replace=FALSE);
    if(v==0) {x=x/2; y=y/2;}
    if(v==1) {x=sz1+(sz1-x)/2; y=sz2-(sz2-y)/2;}
    if(v==2) {x=size-(size-x)/2; y=y/2;}
    xf=floor(x); yf=floor(y); if(xf<1||xf>size||yf<1||yf>size) {next};
    M[xf,yf]=1;
  }
  plotmat(M, fn, clr, ttl, 0, size);
  cat(" *** END:",date(),"\n");
}
pChaosGameS3(600, 30000, "red", "SierpTriR1", "Sierpinski triangle")

```

{{Output}}

```txt

> pChaosGameS3(600, 30000, "red", "SierpTriR1", "Sierpinski triangle")
 *** START: Wed Feb 15 21:40:48 2017 size= 600 lim= 30000 clr= red
 *** Matrix( 600 x 600 ) 15442 DOTS
 *** END: Wed Feb 15 21:40:51 2017

```



## Racket


{{trans|Haskell}}


```racket
#lang racket

(require 2htdp/image)

(define SIZE 300)

(define (game-of-chaos fns WIDTH HEIGHT SIZE
                       #:offset-x [offset-x 0] #:offset-y [offset-y 0]
                       #:iters [iters 10000]
                       #:bg [bg 'white] #:fg [fg 'black])
  (define dot (square 1 'solid fg))
  (define all-choices (apply + (map first fns)))
  (for/fold ([image (empty-scene WIDTH HEIGHT bg)]
             [x (random)] [y (random)]
             #:result image)
            ([i (in-range iters)])
    (define picked (random all-choices))
    (define fn (for/fold ([acc 0] [result #f] #:result result) ([fn (in-list fns)])
                 #:break (> acc picked)
                 (values (+ (first fn) acc) (second fn))))
    (match-define (list x* y*) (fn x y))
    (values (place-image dot (+ offset-x (* SIZE x*)) (+ offset-y (* SIZE y*)) image)
            x* y*)))

(define (draw-triangle)
  (define ((mid a b) x y) (list (/ (+ a x) 2) (/ (+ b y) 2)))
  (define (triangle-height x) (* (sqrt 3) 0.5 x))
  (game-of-chaos (list (list 1 (mid 0 0))
                       (list 1 (mid 1 0))
                       (list 1 (mid 0.5 (triangle-height 1))))
                 SIZE (triangle-height SIZE) SIZE))

(define (draw-fern)
  (define (f1 x y) (list 0 (* 0.16 y)))
  (define (f2 x y) (list (+ (* 0.85 x) (* 0.04 y)) (+ (* -0.04 x) (* 0.85 y) 1.6)))
  (define (f3 x y) (list (+ (* 0.2 x) (* -0.26 y)) (+ (* 0.23 x) (* 0.22 y) 1.6)))
  (define (f4 x y) (list (+ (* -0.15 x) (* 0.28 y)) (+ (* 0.26 x) (* 0.24 y) 0.44)))
  (game-of-chaos (list (list 1 f1) (list 85 f2) (list 7 f3) (list 7 f4))
                 (/ SIZE 2) SIZE (/ SIZE 11) #:offset-x 70 #:offset-y 10
                 #:bg 'black #:fg 'white))

(define (draw-dragon)
  (game-of-chaos
   (list (list 1 (λ (x y) (list (+ (* 0.5 x) (* -0.5 y)) (+ (* 0.5 x) (* 0.5 y)))))
         (list 1 (λ (x y) (list (+ (* -0.5 x) (* 0.5 y) 1) (+ (* -0.5 x) (* -0.5 y))))))
   SIZE (* 0.8 SIZE) (/ SIZE 1.8) #:offset-x 64 #:offset-y 120))

(draw-triangle)
(draw-fern)
(draw-dragon)
```



## REXX


```rexx
/*REXX pgm draws a Sierpinski triangle by running the  chaos game  with a million points*/
parse value  scrsize()   with  sd  sw  .         /*obtain the depth and width of screen.*/
sw= sw - 2                                       /*adjust the screen width down by two. */
sd= sd - 4                                       /*   "    "     "   depth   "   " four.*/
parse arg pts chr seed .                         /*obtain optional arguments from the CL*/
if pts=='' | pts==","  then pts= 1000000         /*Not specified?  Then use the default.*/
if chr=='' | chr==","  then chr= '∙'             /* "      "         "   "   "      "   */
if datatype(seed,'W')  then call random ,,seed   /*Is  specified?    "   "  RANDOM seed.*/
x= sw;       hx= x % 2;     y= sd                /*define the initial starting position.*/
@.= ' '                                          /*   "   all screen points as a blank. */
        do pts;  ?= random(1, 3)                 /* [↓]  draw a # of (million?)  points.*/
                       select                    /*?:  will be a random number: 1 ──► 3.*/
                       when ?==1  then parse value          x%2          y%2   with   x  y
                       when ?==2  then parse value  hx+(hx-x)%2  sd-(sd-y)%2   with   x  y
                       otherwise       parse value  sw-(sw-x)%2          y%2   with   x  y
                       end   /*select*/
        @.x.y= chr                               /*set the    X, Y    point to a bullet.*/
        end   /*pts*/                            /* [↑]  one million points ≡ overkill? */
                                                 /* [↓]  display the points to the term.*/
        do      row=sd   to 0  by -1;   _=       /*display the points, one row at a time*/
             do col=0   for sw+2                 /*   "     a  row (one line) of image. */
             _= _ || @.col.row                   /*construct a  "    "    "    "   "    */
             end   /*col*/                       /*Note: display image from top──►bottom*/
                                                 /* [↑]  strip trailing blanks (output).*/
        say strip(_, 'T')                        /*display one row (line) of the image. */
        end        /*row*/                       /*stick a fork in it,  we're all done. */
```


This REXX program makes use of   '''SCRSIZE'''   REXX program (or
BIF)   which is used to determine the screen

width and depth of the terminal (console).    Some REXXes don't
have this BIF.

The   '''SCRSIZE.REX'''   REXX program is included
here   ───►   [[SCRSIZE.REX]].

(Shown at   '''<sup>1</sup>/<sub>10</sub>   size on a   <big>426</big>&times;<big>201</big>   screen.)

'''output'''   when using the following input:     <tt>   ,   █ </tt>

```txt
                                                                                                                                                                                                                   █
                                                                                                                                                                                                                  ███
                                                                                                                                                                                                                 █████
                                                                                                                                                                                                                ███████
                                                                                                                                                                                                               █████████
                                                                                                                                                                                                              █████ █████
                                                                                                                                                                                                             █████████████
                                                                                                                                                                                                            ███████████████
                                                                                                                                                                                                           █████       █████
                                                                                                                                                                                                          ███████     ███████
                                                                                                                                                                                                         █████████   █████████
                                                                                                                                                                                                        ████  █████ █████  ████
                                                                                                                                                                                                       █████████████████████████
                                                                                                                                                                                                      ███████████████████████████
                                                                                                                                                                                                    █████                     ████
                                                                                                                                                                                                    ██████                   ███████
                                                                                                                                                                                                  █████████                 █████████
                                                                                                                                                                                                 ████   ████               ████   ████
                                                                                                                                                                                                ██████ ██████             ██████ ██████
                                                                                                                                                                                               ███████████████           ███████████████
                                                                                                                                                                                              ████         ████         ████         ████
                                                                                                                                                                                             ██████       ██████       ██████       ██████
                                                                                                                                                                                            ████████     ████████     ████████     ████████
                                                                                                                                                                                           ███    ███   ████   ███   ███   ████   ███    ███
                                                                                                                                                                                          █████ ██████ ██████ ███████████ ██████ █████ ██████
                                                                                                                                                                                         █████████████████████████████████████████████████████
                                                                                                                                                                                        ███                                                 ███
                                                                                                                                                                                       █████                                               █████
                                                                                                                                                                                      ███████                                             ████████
                                                                                                                                                                                     ███   ███                                           ███    ███
                                                                                                                                                                                   ██████ █████                                         █████ ██████
                                                                                                                                                                                  ██████████████                                       ██████████████
                                                                                                                                                                                 ████████████████                                     ████████████████
                                                                                                                                                                                █████        █████                                   █████        █████
                                                                                                                                                                               ███████      ███████                                 ███████      ███████
                                                                                                                                                                              █████████    ██████████                              █████████    █████████
                                                                                                                                                                             █████  ████  █████  █████                           █████  █████  ████  █████
                                                                                                                                                                            ███████████████████████████                         ███████████████████████████
                                                                                                                                                                           █████████████████████████████                       █████████████████████████████
                                                                                                                                                                          ████                      █████                     █████                      ████
                                                                                                                                                                         ██████                    ███████                   ███████                    ██████
                                                                                                                                                                        ████████                  █████████                 █████████                  ████████
                                                                                                                                                                       ████  ████                ████   ████               ████   ████                ████  █████
                                                                                                                                                                     ██████████████             █████████████             ██████ ██████              █████████ ███
                                                                                                                                                                    ████████████████           ███████████████           ███████████████            ███████████████
                                                                                                                                                                   ████          ████         ████         ████         ████         ████          ████         ████
                                                                                                                                                                  ██████        ██████       ██████       ██████       ██████       ██████       ███████       ██████
                                                                                                                                                                 █████████     ████████     ████████     █████████    ████████     ████████     █████████     ████████
                                                                                                                                                                ████   ████  ████   ████   ████  ████   ████   ████  ████  ████   ████   ███   ████   ████   ███   ████
                                                                                                                                                               ██████ ████████████ ██████ ████████████ ██████ ██████████████████ ██████ ████████████ ████████████ ██████
                                                                                                                                                              ███████████████████████████████████████████████████████████████████████████████████████████████████████████
                                                                                                                                                             ███                                                                                                       ███
                                                                                                                                                            █████                                                                                                     █████
                                                                                                                                                           ███████                                                                                                   ███████
                                                                                                                                                          ███   ███                                                                                                 ███   ███
                                                                                                                                                         █████ █████                                                                                              ██████ █████
                                                                                                                                                       ██████████████                                                                                            ██████████████
                                                                                                                                                      ███          ███                                                                                          ███          ███
                                                                                                                                                     █████        █████                                                                                        █████        █████
                                                                                                                                                    ████████     ████████                                                                                     ███████      ████████
                                                                                                                                                   ██████████   ██████████                                                                                   ██████████   ██████████
                                                                                                                                                  █████  █████ █████  █████                                                                                 █████  █████ █████  █████
                                                                                                                                                 ███████████████████████████                                                                               ███████████████████████████
                                                                                                                                                █████████████████████████████                                                                             █████████████████████████████
                                                                                                                                               ████                      █████                                                                           █████                     █████
                                                                                                                                              ███████                   ███████                                                                         ███████                   ███████
                                                                                                                                             █████████                 █████████                                                                       █████████                 █████████
                                                                                                                                            ████  █████               █████  ████                                                                     ████  █████               █████  ████
                                                                                                                                          ██████████████             █████████████                                                                   █████████████             █████████████
                                                                                                                                         ████████████████           ███████████████                                                                ████████████████           ███████████████
                                                                                                                                        █████         ████         ████         ████                                                              █████         ████         ████         ████
                                                                                                                                       ███ ███       ██████       ██████       ███████                                                           ███ ███       ██████       ██████       ███████
                                                                                                                                      █████████     ████████     ████████     █████████                                                         █████████     ████████     ████████     █████████
                                                                                                                                     ████   ████  █████  ████   ████  ████   ████   ████                                                       ████   ████  ████   ████   ████  ████   ████   ████
                                                                                                                                    ██████ ███████████████████ ████████████ ██████ ██████                                                     ██████ ███████████████████ ███████████████████ ██████
                                                                                                                                   ███████████████████████████████████████████████████████                                                   ███████████████████████████████████████████████████████
                                                                                                                                  ████                                                 ████                                                 ███                                                  ████
                                                                                                                                 ██████                                               ██████                                               ██████                                               ██████
                                                                                                                                ████████                                             ████████                                             ████████                                             ████████
                                                                                                                               ███   ████                                           ███   ████                                           ███   ████                                           ███   ████
                                                                                                                              █████ ██████                                         ████████████                                        ██████ ██████                                         █████ ██████
                                                                                                                            ███████████████                                       ██████████████                                      ███████████████                                       ██████████████
                                                                                                                           ████          ███                                     ███          ███                                    ████          ███                                     ███          ███
                                                                                                                          ██████        █████                                   █████        █████                                  ██████        █████                                   █████        █████
                                                                                                                         ████████      ███████                                 ███████      ███████                                ████████      ███████                                 ███████      ███████
                                                                                                                        ███    ███   ████   ███                              ████   ███    ███   ████                             ███    ███   ████   ███                              ████   ███    ███   ████
                                                                                                                       █████  █████ ██████ █████                            ██████ █████  █████ ██████                           █████  █████ ██████ █████                            ██████ █████  █████ ██████
                                                                                                                      ███████████████████████████                          ████████████████████████████                         ███████████████████████████                          ████████████████████████████
                                                                                                                     █████████████████████████████                        ██████████████████████████████                       █████████████████████████████                        ██████████████████████████████
                                                                                                                    █████                      ████                      █████                      █████                     █████                     █████                      █████                      █████
                                                                                                                   ███████                   ████████                   ███████                    ███████                   ███████                   ████████                   ███████                    ███████
                                                                                                                  █████████                 ██████████                 █████████                  █████████                 █████████                 ██████████                 █████████                  █████████
                                                                                                                 ████  █████               █████  █████               █████  ████                ████  █████               ████  █████               █████  █████               █████ █████                ████  █████
                                                                                                                █████████████             ██████████████             █████████████             ██████████████             █████████████             ██████████████             █████████████             ██████████████
                                                                                                              ████████████████           ████████████████           ███████████████           ████████████████          ████████████████           ████████████████           ███████████████           ████████████████
                                                                                                             █████         ████         █████        █████         ████         █████        █████         ████        █████         ████         █████        █████         ████         ████         █████         ████
                                                                                                            ███████       ██████       ███████      ███████       ██████       ███████      ███████      ███████      ███████       ██████       ███████      ███████       ██████       ███████      ███████       ██████
                                                                                                           █████████     ████████     █████████    █████████    █████████     █████████    █████████    █████████    █████████     ████████     █████████    █████████    █████████     █████████    █████████    █████████
                                                                                                          ████   ████  █████  █████  ████  █████  ████   ████  █████  ████   ████  █████  ████   ████  █████  ████  ████   ████  █████  ████   ████   ████  ████   ████  █████  ████   ████  █████  ████   ████  █████  ████
                                                                                                         ██████████████████████████████████████████████████████████████████ █████████ █████████ █████████ █████████████████████████ █████████ ███████████████████ █████████ █████████ █████████ █████████ █████████ █████████
                                                                                                        ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
                                                                                                       ████                                                                                                                                                                                                                 ████
                                                                                                      ██████                                                                                                                                                                                                               ██████
                                                                                                     ████████                                                                                                                                                                                                             ████████
                                                                                                    ████  ████                                                                                                                                                                                                           ████  ████
                                                                                                   ████████████                                                                                                                                                                                                         ████████████
                                                                                                  ██████████████                                                                                                                                                                                                       ██████████████
                                                                                                 ████         ███                                                                                                                                                                                                     ████         ████
                                                                                                ██████       ██████                                                                                                                                                                                                  ██████       ██████
                                                                                               ████████     ████████                                                                                                                                                                                                ████████     ████████
                                                                                              ███   ████   ███   ████                                                                                                                                                                                              ███   ████   ███   ████
                                                                                             █████ ██████ █████ ██████                                                                                                                                                                                            █████ ██████ █████ ██████
                                                                                            ███████████████████████████                                                                                                                                                                                          ███████████████████████████
                                                                                           ███                       ███                                                                                                                                                                                        ███                       ███
                                                                                          █████                     █████                                                                                                                                                                                      █████                     █████
                                                                                        ████████                   ███████                                                                                                                                                                                   ████████                   ███████
                                                                                       ██████████                 █████████                                                                                                                                                                                 ██████████                 █████████
                                                                                      █████  █████               █████ █████                                                                                                                                                                               █████  █████               █████ █████
                                                                                     ██████████████            ██████████████                                                                                                                                                                             ██████████████            ██████████████
                                                                                    ████████████████          ████████████████                                                                                                                                                                           ████████████████          ████████████████
                                                                                   █████        █████        █████         ████                                                                                                                                                                         █████        █████        █████         ████
                                                                                  ███████      ███████      ███████       ███████                                                                                                                                                                      ███████      ███████      ███████       ███████
                                                                                 █████████    █████████    █████████     █████████                                                                                                                                                                    █████████    █████████    █████████     █████████
                                                                                ████   ████  █████  █████ █████  ████  █████  █████                                                                                                                                                                  █████  ████  █████  ████  █████  ████   ████  █████
                                                                               █████████████████████████████████████████████████████                                                                                                                                                                █████████████████████████████████████████████████████
                                                                              ███████████████████████████████████████████████████████                                                                                                                                                              ███████████████████████████████████████████████████████
                                                                             ████                                                 ████                                                                                                                                                            ████                                                 ████
                                                                            ██████                                              ███████                                                                                                                                                          ██████                                              ███████
                                                                          █████████                                            █████████                                                                                                                                                       █████████                                            █████████
                                                                         ████   ████                                          ████   ████                                                                                                                                                     █████  ████                                          ████   ████
                                                                        █████████████                                        █████████████                                                                                                                                                   ██████ ██████                                        █████████████
                                                                       ███████████████                                      ███████████████                                                                                                                                                 ███████████████                                      ███████████████
                                                                      ████         ████                                    ████         ████                                                                                                                                               ████         ████                                    ████         ████
                                                                     ██████       ██████                                  ██████       ██████                                                                                                                                             ██████       ██████                                  ██████       ██████
                                                                    ████████     ████████                                ████████     ████████                                                                                                                                           ████████     ████████                                ████████     ████████
                                                                   ████   ███   ████   ████                             ████   ███   ████   ███                                                                                                                                         ████   ███   ████   ███                              ████   ███   ████   ████
                                                                  ████████████ ██████ ██████                           ██████ █████ ██████ ██████                                                                                                                                      ██████ █████ ██████ ██████                           ████████████ ██████ ██████
                                                                 ████████████████████████████                         ████████████████████████████                                                                                                                                    ████████████████████████████                         ████████████████████████████
                                                                ███                       ████                       ███                       ████                                                                                                                                  ███                       ████                       ███                       ████
                                                               █████                     ██████                     █████                     ██████                                                                                                                                █████                     ██████                     █████                     ██████
                                                             ████████                   ████████                  ████████                   ████████                                                                                                                              ███████                   ████████                   ███████                   ████████
                                                            ████   ███                 ███    ███                ████   ███                 ███    ███                                                                                                                           ████   ███                 ███    ███                ████   ███                 ███    ███
                                                           ██████ █████               █████  █████              ██████ █████               █████  █████                                                                                                                         ██████ █████               █████  █████              ██████ █████               █████  █████
                                                          ██████████████             ██████████████            ██████████████             ██████████████                                                                                                                       ██████████████             ██████████████            ██████████████             ██████████████
                                                         ████████████████           ████████████████          ████████████████           ████████████████                                                                                                                     ████████████████           ████████████████          ████████████████           ████████████████
                                                        █████        ██████        █████        █████        █████        ██████        █████        █████                                                                                                                   █████        ██████        █████        █████        █████        █████         █████        █████
                                                       ███████      ████████      ███████      ███████      ███████      ████████      ███████      ███████                                                                                                                 ███████      ████████      ███████      ███████      ███████      ████████      ███████      ███████
                                                      █████████    ██████████    █████████    █████████    █████████    ██████████    █████████    █████████                                                                                                               █████████    ██████████    █████████    █████████    █████████    ██████████    █████████    █████████
                                                     █████  ████  █████  █████ █████  █████  █████  ████  █████  ████  █████  █████  ████  █████  █████ █████                                                                                                             █████  ████  █████  █████  ████  █████  █████  ████  █████  ████  █████  █████  ████  █████  █████ █████
                                                    ██████████████████████████████████████████████████████████████████████████████████████████████████████████                                                                                                           ██████████████████████████████████████████████████████████████████████████████████████████████████████████
                                                   █████████████████████████████████████████████████████████████████████████████████████████████████████████████                                                                                                        █████████████████████████████████████████████████████████████████████████████████████████████████████████████
                                                  ████                                                                                                      █████                                                                                                      ████                                                                                                      █████
                                                 ██████                                                                                                    ███████                                                                                                    ██████                                                                                                    ███████
                                                ████████                                                                                                  █████████                                                                                                  ████████                                                                                                  █████████
                                               ████  █████                                                                                               █████  ████                                                                                                ████  █████                                                                                               █████  ████
                                              █████████████                                                                                             █████████████                                                                                              █████████████                                                                                             ███ █████████
                                             ███████████████                                                                                           ███████████████                                                                                            ███████████████                                                                                           ███████████████
                                           █████         ████                                                                                         ████         ████                                                                                         █████         ████                                                                                         ████         ████
                                           ██████       ██████                                                                                       ██████       ██████                                                                                       ███████       ██████                                                                                       ██████       ██████
                                         █████████    █████████                                                                                     ████████     ████████                                                                                     █████████    █████████                                                                                     ████████     ████████
                                        ████   ████  ████   ████                                                                                   ████  ████   ████   ████                                                                                  ████   ████  ████   ████                                                                                   ████  ████   ████   ███
                                       ██████ ████████████ ██████                                                                                 ████████████ ██████ ██████                                                                                ██████ ████████████ ██████                                                                                 ████████████ ██████ ██████
                                      ████████████████████████████                                                                               ████████████████████████████                                                                              ████████████████████████████                                                                               ████████████████████████████
                                     ████                       ███                                                                             ███                       ████                                                                            ████                      ████                                                                             ███                       ████
                                    ██████                    ██████                                                                           █████                     ██████                                                                          ██████                    ██████                                                                          ██████                     ██████
                                   ████████                  ████████                                                                        ████████                   ████████                                                                        ████████                  ████████                                                                        ████████                   ████████
                                  ███   ████                ████   ███                                                                      ████   ███                 ███   ████                                                                      ███   ████                ████   ███                                                                      ████   ███                 ███   ████
                                 █████ ██████              ██████ █████                                                                    ██████ █████               █████ ██████                                                                    █████ ██████              ██████ █████                                                                    ██████ █████               █████ ██████
                                ██████████████            ███████████████                                                                 ███████████████            ██████████████                                                                  ██████████████            ███████████████                                                                 ███████████████            ██████████████
                               ███          ███          ███          ████                                                               ███          ████          ███          ███                                                                ███          ███          ███          ████                                                               ███          ████          ███          ███
                             ██████        █████        █████        ██████                                                             █████        ██████       ██████        █████                                                             ██████        █████        █████        ██████                                                             █████        ██████        █████        █████
                            ████████      ███████      ███████      ████████                                                           ███████      ████████     ████████      ███████                                                           ████████      ███████      ███████      ████████                                                           ███████      ████████     ████████      ███████
                           ██████████    █████████    ██████████   ██████████                                                         █████████    ██████████   ██████████    █████████                                                         ██████████    █████████    ██████████   ██████████                                                         ██████████   ██████████   ██████████    █████████
                          █████  █████  █████ █████  █████ ██████ █████  █████                                                       █████ ██████ █████  █████ █████  █████  █████ █████                                                       █████  █████  █████ █████  █████ █████  █████  █████                                                       █████ █████  █████  █████ █████  █████  █████ █████
                         ██████████████████████████████████████████████████████                                                     █████████████████████████████████████████████████████                                                     ██████████████████████████████████████████████████████                                                     █████████████████████████████████████████████████████
                        ████████████████████████████████████████████████████████                                                   ████████████████████████████████████████████████████████                                                  ████████████████████████████████████████████████████████                                                   ████████████████████████████████████████████████████████
                       █████                                                █████                                                 ████                                                 █████                                                █████                                                █████                                                 ████                                                 █████
                      ███████                                              ███████                                               ███████                                              ███████                                              ███████                                              ███████                                               ██████                                               ███████
                     █████████                                            █████████                                             █████████                                            █████████                                            █████████                                            █████████                                            ██████████                                            █████████
                    █████ █████                                          █████  ████                                          █████  █████                                          ████  █████                                          █████  ████                                          █████  ████                                          █████  █████                                          ████  █████
                   █████████████                                        ██████████████                                       ██████████████                                        █████████████                                        █████████████                                        █████████████                                        ██████████████                                        █████████████
                  ███████████████                                      ████████████████                                     ████████████████                                      ███████████████                                      ███████████████                                      ████████████████                                     ████████████████                                      ███████████████
                 ████         ████                                    ████         █████                                   █████        █████                                   █████         ████                                    ████         ████                                    ████         █████                                   █████        █████                                   █████         ████
                ██████       ██████                                  ██████       ███████                                 ███████      ███████                                 ███████       ██████                                  ██████       ██████                                  ██████       ███ ███                                 ███ ███      ███████                                 ███████       ██████
               ████████     ████████                                █████████    █████████                               █████████    █████████                               █████████     ████████                                ████████     ████████                                █████████    █████████                               █████████    █████████                               █████████     ████████
             █████  ████   ████  █████                             ████  █████  ████   ████                             ████   ████  ████   ████                             ████   ████  █████  ████                             █████  ████   ████  █████                             ████  █████  ████   ████                             ████   ████  ████   ████                             ████   ████  █████  ████
            █████████████ █████████████                           ███████████████████ ██████                           ██████ ████████████ ██████                           ██████ ███████████████████                           █████████████ █████████████                           ███████████████████ ██████                           ██████ ████████████ ██████                           ██████ ███████████████████
           █████████████████████████████                         ████████████████████████████                         ████████████████████████████                         █████████████████████████████                        █████████████████████████████                         ████████████████████████████                         ████████████████████████████                         █████████████████████████████
          ████                       ████                       ████                      ████                       ████                      ████                       ████                       ████                      ████                       ████                       ████                      ████                       ████                      ████                       ████                       ████
         ██████                     ██████                    ███████                    ██████                     ██████                    ██████                     ██████                     ██████                    ██████                     ██████                    ███████                    ██████                     ██████                     █████                     ██████                     ██████
        ████████                   ████████                  █████████                  ████████                   ████████                  ████████                   ████████                   ████████                  ████████                   ████████                  █████████                  ████████                   ████████                  █████████                  ████████                   ████████
       ████   ███                 ███   ████                ████   ████                ████   ███                 ███   ████                ████   ████                ████  ████                ████   ████                ████   ███                 ███   ████                ████   ████                ████   ███                 ███   ████                ████   ████                ████  ████                ████   ████
      ██████ █████               █████ ██████              ██████ ██████              ████████████              ██████ ██████              ██████ ██████              ████████████              ██████ ██████              ██████ ██████              ████████████              ██████ ██████              ████████████               █████ ██████              ██████ ██████              ████████████              ██████ ██████
     ███████████████            ██████████████            ███████████████            ███████████████            ██████████████            ███████████████            ██████████████            ███████████████            ███████████████            ██████████████            ███████████████            ███████████████           ███████████████            ███████████████            ██████████████            ███████████████
    ███          ████          ███          ███          ████         ████          ███          ████         ████          ███          ████         ████          ███          ███          ████          ███          ███          ████         ████          ███          ████         ████          ███          ████         ████          ███          ████         ████          ███          ███          ████          ███
   █████        ██████       ██████        █████        ██████       ██████        █████        ██████       ██████        █████        ██████       ██████        █████        ██████       ██████       ██████        █████        ██████       ██████        █████        ██████       ██████        █████        ██████       ██████        █████        ██████       ██████        █████        ██████       ██████        █████
  ████████     ████████     ████████      ████████     ████████     ████████      ███████      ████████     ████████      ███████      ████████     ████████      ███████      ████████     ████████     ████████      ████████     ████████     ████████      ████████     ████████     ████████      ███████      ████████     ████████      ███████      ████████     ████████     ████████      ████████     ████████     ████████
 ███   ███    ███    ███   ████   ███    ███   ████   ███    ███   ████   ███    ███   ████   ███    ███   ███    ███   ████   ███    ███   ████   ███    ███   ████   ███    ███   ████   ███    ███   ████   ███    ███   ████   ███    ███   ████   ███    ███   ████   ███    ███   ████   ███    ███   ███    ███   ████   ███    ███   ████   ███    ███    ███   ███    ███   ████   ███    ███   ████   ███    ███   ████   ███
█████ ██████ █████  █████ ██████ █████  █████ ██████ █████ ██████ ██████ █████  █████ ██████ █████ ██████ █████  █████ ██████ █████  █████ ██████ █████  █████ ██████ █████  █████ ██████ █████  █████ ██████ █████  █████ ██████ █████  █████ ██████ █████  █████ ██████ █████  █████ ██████ █████  █████ ██████ █████ ██████ ██████ █████ ██████ ██████ █████ ██████ █████  █████ ██████ █████  █████ ██████ █████  █████ ██████ █████

```

</b>


## Ring


```ring

# Project : Chaos game

load "guilib.ring"

paint = null

new qapp
       {
       win1 = new qwidget() {
                  setwindowtitle("Archimedean spiral")
                  setgeometry(100,100,500,600)
                  label1 = new qlabel(win1) {
                               setgeometry(10,10,400,400)
                               settext("")
                  }
                 new qpushbutton(win1) {
                        setgeometry(150,500,100,30)
                        settext("draw")
                        setclickevent("draw()")
                    }
               show()
       }
       exec()
       }

func draw
       p1 = new qpicture()
               color = new qcolor() {
               setrgb(0,0,255,255)
       }
       pen = new qpen() {
                setcolor(color)
                setwidth(1)
       }
       paint = new qpainter() {
                  begin(p1)
                  setpen(pen)

       x = floor(random(10)/10 * 200)
       y = floor(random(10/10) * 173)
       for i = 1 to 20000
           v = floor(random(10)/10 * 3) + 1
	   if v = 1
	      x = x/2
	      y = y/2
	   ok
	   if v = 2
	      x = 100 + (100-x)/2
	      y = 173 - (173-y)/2
	   ok
	   if v = 3
	      x = 200 - (200-x)/2
	      y = y/2
	   ok
	   drawpoint(x,y)
       next
       endpaint()
       }
       label1 {setpicture(p1) show()}

```

* [https://lh3.googleusercontent.com/-xqBO5MB8fpc/Wg05SvwaF9I/AAAAAAAABDA/UGI2goKdDoAR6nbbGZF0YcuwGG6tancvACLcBGAs/s1600/CalmoSoftChaos.jpg Chaos Game (image)]


## Run BASIC


```runbasic
x	= int(rnd(0) * 200)
y	= int(rnd(0) * 173)
graphic #g, 200,200
#g color("green")
for i =1 TO 20000
	v = int(rnd(0) * 3) + 1
	if v = 1 then
		x = x/2
		y = y/2
	end if
	if v = 2 then
		x = 100 + (100-x)/2
		y = 173 - (173-y)/2
	end if
	if v = 3 then
		x = 200 - (200-x)/2
		y = y/2
	end if
	#g set(x,y)
next
render #g
```



## Rust

Dependencies: image, rand

```rust
extern crate image;
extern crate rand;

use std::fs::File;
use rand::Rng;
use std::f32;

fn main() {
    let max_iterations = 50_000u32;
    let img_side = 800u32;
    let tri_size = 400f32;

    // Create a new ImgBuf
    let mut imgbuf = image::ImageBuffer::new(img_side, img_side);

    // Create triangle vertices
    let mut vertices: [[f32; 2]; 3] = [[0f32, 0f32]; 3];
    for i in 0..vertices.len() {
        vertices[i][0] =
            (img_side as f32 / 2.) + (tri_size / 2.) * (f32::consts::PI * i as f32 * 2. / 3.).cos();
        vertices[i][1] =
            (img_side as f32 / 2.) + (tri_size / 2.) * (f32::consts::PI * i as f32 * 2. / 3.).sin();
    }
    for v in &vertices {
        imgbuf.put_pixel(v[0] as u32, v[1] as u32, image::Luma([255u8]));
    }

    // Iterate chaos game
    let mut rng = rand::weak_rng();
    let mut x = img_side as f32 / 2.;
    let mut y = img_side as f32 / 2.;
    for _ in 0..max_iterations {
        let choice = rng.gen_range(0, vertices.len());
        x = (x + vertices[choice][0]) / 2.;
        y = (y + vertices[choice][1]) / 2.;

        imgbuf.put_pixel(x as u32, y as u32, image::Luma([255u8]));
    }

    // Save image
    let fout = &mut File::create("fractal.png").unwrap();
    image::ImageLuma8(imgbuf).save(fout, image::PNG).unwrap();
}
```



## Scala


### Java Swing Interoperability


```Scala
import javax.swing._
import java.awt._
import java.awt.event.ActionEvent

import scala.collection.mutable
import scala.util.Random

object ChaosGame extends App {
  SwingUtilities.invokeLater(() =>
    new JFrame("Chaos Game") {

      class ChaosGame extends JPanel {
        private val (dim, margin)= (new Dimension(640, 640), 60)
        private val sizez: Int = dim.width - 2 * margin
        private val (stack, r) = (new mutable.Stack[ColoredPoint], new Random)
        private val points = Seq(new Point(dim.width / 2, margin),
          new Point(margin, sizez),
          new Point(margin + sizez, sizez)
        )
        private val colors = Seq(Color.red, Color.green, Color.blue)

        override def paintComponent(gg: Graphics): Unit = {
          val g = gg.asInstanceOf[Graphics2D]

          def drawPoints(g: Graphics2D): Unit = {
            for (p <- stack) {
              g.setColor(colors(p.colorIndex))
              g.fillOval(p.x, p.y, 1, 1)
            }
          }

          super.paintComponent(gg)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          drawPoints(g)
        }


        private def addPoint(): Unit = {
          val colorIndex = r.nextInt(3)

          def halfwayPoint(a: Point, b: Point, idx: Int) =
            new ColoredPoint((a.x + b.x) / 2, (a.y + b.y) / 2, idx)

          stack.push(halfwayPoint(stack.top, points(colorIndex), colorIndex))
        }

        class ColoredPoint(x: Int, y: Int, val colorIndex: Int) extends Point(x, y)

        stack.push(new ColoredPoint(-1, -1, 0))
        new Timer(100, (_: ActionEvent) => {
          if (stack.size < 50000) {
            for (i <- 0 until 1000) addPoint()
            repaint()
          }
        }).start()
        setBackground(Color.white)
        setPreferredSize(dim)
      }

      add(new ChaosGame, BorderLayout.CENTER)
      pack()
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setLocationRelativeTo(null)
      setResizable(false)
      setVisible(true)
    }
  )

}
```


## Scilab

This script uses complex numbers to represent (x,y) coordinates: real part as x position, and imaginary part as y position.
<lang>//Input
n_sides = 3;
side_length = 1;
ratio = 0.5;
n_steps = 1.0d5;
first_step = 0;

if n_sides<3 then
    error("n_sides should be at least 3.");
end

//Calculating vertices' positions
theta = (2 * %pi) / n_sides;
alpha = (180 - (360/n_sides)) / 2 * (%pi/180);
radius = (sin(theta) / side_length) / sin(alpha);
vertices = zeros(1,n_sides);
for i=1:n_sides
    vertices(i) = radius * exp( %i * theta * (i-1) ); //equally spaced vertices over a circumference
                                                      //centered on 0 + 0i, or (0,0)
end
clear theta alpha radius i


//Iterations
tic();
points = zeros(1,n_steps);
points(1) = first_step;
i = 2;
while i <= n_steps
    random=grand(1,'prm',[1:n_sides]'); //sort vertices randomly
    random=random(1);                   //choose the first random vertices

    points(i) = ( vertices(random) - points(i-1) ) * (1-ratio) + points(i-1);

    i = i + 1;
end
time=toc();
disp('Time: '+string(time)+'s.');

//Ploting
scf(0); clf();
xname('Chaos game: '+string(n_sides)+'-sides polygon');
plot2d(real(points),imag(points),0)
plot2d(real(vertices),imag(vertices),-3);
set(gca(),'isoview','on');
```

{{out}}
It outputs a graphic window and prints on the console the time elapsed during iterations.

```txt
Time: 1.0424433s.
```



## Sidef


```ruby
require('Imager')

var width  = 600
var height = 600

var points = [
    [width//2,        0],
    [       0, height-1],
    [height-1, height-1],
]

var img = %O|Imager|.new(
                      xsize => width,
                      ysize => height,
                     )

var color = %O|Imager::Color|.new('#ff0000')
var r = [(width-1).irand, (height-1).irand]

30000.times {
    var p = points.rand

    r[] = (
        (p[0] + r[0]) // 2,
        (p[1] + r[1]) // 2,
    )

    img.setpixel(
        x     => r[0],
        y     => r[1],
        color => color,
    )
}

img.write(file => 'chaos_game.png')
```

Output image: [https://github.com/trizen/rc/blob/master/img/chaos-game-sidef.png Chaos game]


## Simula


```simula
BEGIN
    INTEGER U, COLUMNS, LINES;
    COLUMNS := 40;
    LINES := 80;
    U := ININT;
    BEGIN
        CHARACTER ARRAY SCREEN(0:LINES, 0:COLUMNS);
        INTEGER X, Y, I, VERTEX;

        FOR X := 0 STEP 1 UNTIL LINES-1 DO
            FOR Y := 0 STEP 1 UNTIL COLUMNS-1 DO
                SCREEN(X, Y) := ' ';

        X := RANDINT(0, LINES - 1, U);
        Y := RANDINT(0, COLUMNS - 1, U);

        FOR I := 1 STEP 1 UNTIL 5000 DO
        BEGIN
            VERTEX := RANDINT(1, 3, U);
            IF VERTEX = 1 THEN BEGIN X := X // 2;
                                     Y := Y // 2;
                               END ELSE
            IF VERTEX = 2 THEN BEGIN X := LINES // 2 + (LINES // 2 - X) // 2;
                                     Y := COLUMNS - (COLUMNS - Y) // 2;
                               END ELSE
            IF VERTEX = 3 THEN BEGIN X := LINES - (LINES - X) // 2;
                                     Y := Y // 2;
                               END ELSE ERROR("VERTEX OUT OF BOUNDS");
            SCREEN(X, Y) := 'X';
        END;

        FOR Y := 0 STEP 1 UNTIL COLUMNS-1 DO
        BEGIN
            FOR X := 0 STEP 1 UNTIL LINES-1 DO
                OUTCHAR(SCREEN(X, Y));
            OUTIMAGE;
        END;
    END;
END

```

{{in}}

```txt

678

```

{{out}}

```txt

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 XXXX XXX  XXXX XXXX XXX  XXXX XXX  XXXX  XXX XXXX XXXX  XXX  XXX  XXX XXXX XXXX
  XXXXXXX   XXXXXX    XXXXXXX    XXXXXX   XXXXXXX   XXXXXXX   XXXXXXX   XXXXXXX
   XXXX      XXXXX     XXXXX     XXXXX     XXXXX     XXXXX     XXXXX      XXXX
    XX        XXX       XX        XXX       XXX       XXX       XXX        XX
     XXXXXXXXXXX         XXXXXXXXXXX         XXXXXXXXXXX          XXXXXXXXXX
      XXXX XXX            XXXX XXXX           XXXX XXXX            XXX XXXX
       XXXXXX              XXXXXXX             XXXXXXX             XXXXXXX
        XXXXX               XX XX               XXXXX               XXXXX
         XXX                 XXX                 XXX                  XX
          XXXXXXXXXXXXXXXXXXXXX                   XXXXXXXXXXXXXXXXXXXXX
           XXXX XXXX XXXX XXXX                     XXXX XXXX XXXX XXXX
            XXXXXXX   XXXXXXX                       XXXX XX   XX XXXX
             XXXXX     XXXXX                         XXXXX     XXXXX
              XXX      XXXX                           XXX       XXX
               XXXXXXXXXXX                             XXXXXXXXXXX
                XXXX XXXX                               XXXXXXXXX
                XXXXX XX                                 XXXXXXX
                  XXXXX                                    XXXX
                   XXX                                     XXX
                    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                     XXXX XXXX XXXX XXXX XXXX XXXX XXXX XXXX
                      XX   XX   XXX  XXX  XX  XXX  XXX   XX
                       XXXXX     XXXXX     XXXXX    XXXXXX
                        XXXX      XXX       XXX       XXX
                         XXXXXXXXXXXX        XXXXXXXXXXX
                          XXXXXXXXX           XXXX XXXX
                           XXX  XX            XXX   XX
                            XXXXX              XXXXXX
                             XXX                XXXX
                              XXXXXXXXXXXXXXXXXXXXX
                               XXXXXXXXXXXXXXXXXXX
                                XXX  XXX XXX  XXX
                                 XXXXXX   XXXXXX
                                  XXXX     XXXX
                                 X XXXXXXXX XX
                                    XXXXXXXXX
                                     XXX XXX
                                      XXXXX
                                       XXX

```



## Yabasic


```Yabasic
width = 640 : height = 480
open window width, height
window origin "lb"

x = ran(width)
y = ran(height)

for i = 1 to 200000
    vertex = int(ran(3))
    if vertex = 1 then
        x = width / 2 + (width / 2 - x) / 2
        y = height - (height - y) / 2
    elseif vertex = 2 then
        x = width - (width - x) / 2
        y = y / 2
    else
        x = x / 2
        y = y / 2
    end if
    color 255 * (vertex = 0), 255 * (vertex = 1), 255 * (vertex = 2)
    dot x, y
next
```



## zkl

This is a half assed animated process - a bunch of pixels are drawn every couple of seconds and the pixmap written [to the file system]. So, if you open the output file ("chaosGame.jpg") it will [auto] update and show the progression of the image.

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
{{trans|Java}}
[[File:ChaosGame.zkl.jpg|240px|thumb|right]]

```zkl
w,h:=640,640;
bitmap:=PPM(w,h,0xFF|FF|FF);  // White background
colors:=T(0xFF|00|00,0x00|FF|00,0x00|00|FF);   // red,green,blue

margin,size:=60, w - 2*margin;
points:=T(T(w/2, margin), T(margin,size), T(margin + size,size) );
N,done:=Atomic.Int(0),Atomic.Bool(False);

Thread.HeartBeat('wrap(hb){  // a thread
   var a=List(-1,-1);

   if(N.inc()<50){
      do(500){
	 colorIndex:=(0).random(3);  // (0..2)
	 b,p:=points[colorIndex], halfwayPoint(a,b);
	 x,y:=p;
	 bitmap[x,y]=colors[colorIndex];
	 a=p;
      }
      bitmap.writeJPGFile("chaosGame.jpg",True);
   }
   else{ hb.cancel(); done.set(); }  // stop thread and signal done
},2).go();     // run every 2 seconds, starting now

fcn halfwayPoint([(ax,ay)], [(bx,by)]){ T((ax + bx)/2, (ay + by)/2) }

done.wait();  // don't exit until thread is done
println("Done");
```

