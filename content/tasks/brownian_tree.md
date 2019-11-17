+++
title = "Brownian tree"
description = ""
date = 2019-08-08T17:17:35Z
aliases = []
[extra]
id = 7145
[taxonomies]
categories = ["task"]
languages = [
  "applesoft_basic",
  "autohotkey",
  "bbc_basic",
  "c",
  "c#",
  "c++",
  "common_lisp",
  "d",
  "delphi",
  "easylang",
  "factor",
  "fantom",
  "fortran",
  "freebasic",
  "gnuplot",
  "go",
  "haskell",
  "icon",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "liberty_basic",
  "lua",
  "mathematica",
  "ocaml",
  "octave",
  "pari_gp",
  "perl_5",
  "perl_6",
  "phix",
  "picolisp",
  "processing",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sequencel",
  "sidef",
  "sinclair_zx81_basic",
  "tcl",
  "ti-83_basic",
  "unicon",
  "visual_basic",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
tags = ["geometry", "fractals", "graphics", "raster-graphics"]
+++

![Brownian tree](/images/Brownian_tree.jpg)


## Task

Generate and draw a Brownian tree.

A Brownian Tree is generated as a result of an initial seed,
followed by the interaction of two processes.

- The initial "seed" is placed somewhere within the field.
    Where is not particularly important;
    it could be randomized, or it could be a fixed point.
- Particles are injected into the field,
    and are individually given a (typically random) motion pattern.
- When a particle collides with the seed or tree, its position is fixed,
    and it's considered to be part of the tree.

Because of the lax rules governing the random nature of the particle's placement and motion,
no two resulting trees are really expected to be the same,
or even necessarily have the same general shape.


## Applesoft BASIC

Uses XDRAW to plot to Hi-res GRaphics, in fullscreen [POKE 49234,0] 280 x 192,
effectively 140 x 192 because colors stretch over two pixels, using a single pixel shape.
The POKEs create one shape in a shape table starting at address 768 and point addresses 232 and 233 to this address.
Address 234 is the collision counter which is used to detect if the randomly placed seed has hit anything and if the moving seed has collided with the tree.
Plotting the seed creates an animation effect of the seed moving around in it's Brownian way.

```applesoftbasic
0GOSUB2:FORQ=0TOTSTEP0:X=A:Y=B:FORO=0TOTSTEP0:XDRAWTATX,Y:X=INT(RND(T)*J)*Z:Y=INT(RND(T)*H):XDRAWTATX,Y:O=PEEK(C)>0:NEXTO:FORP=0TOTSTEP0:A=X:B=Y:R=INT(RND(T)*E):X=X+X(R):Y=Y+Y(R):IFX<0ORX>MORY<0ORY>NTHENNEXTQ
 1  XDRAW T AT X,Y:P =  NOT  PEEK (C): XDRAW T AT A,B: NEXT P: XDRAW T AT X,Y:Q = A = 0 OR A = M OR B = 0 OR B = N: NEXT Q: END
 2 T = 1:Z = 2:E = 8:C = 234
 3 W = 280:A = W / 2:J = A
 4 H = 192:B = H / 2:M = W - 2
 5 N = H - 1:U =  - 1:V =  - 2
 6 Y(0) = U:X(0) = V:Y(1) = U
 7 Y(2) = U:X(2) = 2:X(3) = 2
 8 Y(4) = 1:X(4) = 2:Y(5) = 1
 9 X(6) = V:Y(6) = 1:X(7) = V
 10  POKE 768,1: POKE 769,0
 11  POKE 770,4: POKE 771,0
 12  POKE 772,5: POKE 773,0
 13  POKE 232,0: POKE 233,3
 14  HGR : POKE 49234,0
 15  ROT= 0: SCALE= 1: RETURN
```



## AutoHotkey

Works with AutoHotkey_L
Takes a little while to run, be patient.
Requires the [http://www.autohotkey.com/forum/topic32238.html GDI+ Standard Library by Tic]

```AHK
SetBatchLines -1
Process, Priority,, high
size := 400
D    := .08
num  := size * size * d
field:= Object()
field[size//2, size//2] := true ; set the seed
lost := 0

Loop % num
{
	x := Rnd(1, size), y := Rnd(1, size)
	Loop
	{
		oldX := X, oldY := Y
		x += Rnd(-1, 1), y += Rnd(1, -1)
		If ( field[x, y] )
		{
			field[oldX, oldY] := true
			break
		}
		If ( X > Size ) or ( Y > Size) or ( X < 1 ) or ( Y < 1 )
		{
			lost++
			break
		}
	}
}

pToken  := Gdip_startup()
pBitmap := Gdip_CreateBitmap(size, size)
loop %size%
{
	x := A_index
	Loop %size%
	{
		If ( field[x, A_Index] )
		{
			Gdip_SetPixel(pBitmap, x, A_Index, 0xFF0000FF)
		}
	}
}
Gdip_SaveBitmapToFile(pBitmap, "brownian.png")
Gdip_DisposeImage(pBitmap)
Gdip_Shutdown(pToken)
Run brownian.png

MsgBox lost %lost%

Rnd(min, max){
	Random, r, min, max
	return r
}
```
Sample output file [http://www.autohotkey.net/~crazyfirex/Images/brownian.png here]


## BBC BASIC

Works with BBC BASIC for Windows

```bbcbasic
      SYS "SetWindowText", @hwnd%, "Brownian Tree"
      SIZE = 400

      VDU 23,22,SIZE;SIZE;8,16,16,0
      GCOL 10

      REM set the seed:
      PLOT SIZE, SIZE

      OFF
      REPEAT

        REM set particle's initial position:
        REPEAT
          X% = RND(SIZE)-1
          Y% = RND(SIZE)-1
        UNTIL POINT(2*X%,2*Y%) = 0

        REPEAT
          oldX% = X%
          oldY% = Y%
          X% += RND(3) - 2
          Y% += RND(3) - 2
        UNTIL POINT(2*X%,2*Y%)
        IF X%>=0 IF X%<SIZE IF Y%>=0 IF Y%<SIZE PLOT 2*oldX%,2*oldY%

      UNTIL FALSE


```

[[File:Brownian_BBC.gif]]


## C

```c
#include <string.h

#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <FreeImage.h>

#define NUM_PARTICLES  1000
#define SIZE           800

void draw_brownian_tree(int world[SIZE][SIZE]){
  int px, py; // particle values
  int dx, dy; // offsets
  int i;

  // set the seed
  world[rand() % SIZE][rand() % SIZE] = 1;

  for (i = 0; i < NUM_PARTICLES; i++){
    // set particle's initial position
    px = rand() % SIZE;
    py = rand() % SIZE;

    while (1){
      // randomly choose a direction
      dx = rand() % 3 - 1;
      dy = rand() % 3 - 1;

      if (dx + px < 0 || dx + px >= SIZE || dy + py < 0 || dy + py >= SIZE){
        // plop the particle into some other random location
        px = rand() % SIZE;
        py = rand() % SIZE;
      }else if (world[py + dy][px + dx] != 0){
        // bumped into something
        world[py][px] = 1;
        break;
      }else{
        py += dy;
        px += dx;
      }
    }
  }
}

int main(){
  int world[SIZE][SIZE];
  FIBITMAP * img;
  RGBQUAD rgb;
  int x, y;

  memset(world, 0, sizeof world);
  srand((unsigned)time(NULL));

  draw_brownian_tree(world);

  img = FreeImage_Allocate(SIZE, SIZE, 32, 0, 0, 0);

  for (y = 0; y < SIZE; y++){
    for (x = 0; x < SIZE; x++){
      rgb.rgbRed = rgb.rgbGreen = rgb.rgbBlue = (world[y][x] ? 255 : 0);
      FreeImage_SetPixelColor(img, x, y, &rgb);
    }
  }
  FreeImage_Save(FIF_BMP, img, "brownian_tree.bmp", 0);
  FreeImage_Unload(img);
}
```


### Alternative Version

Translated from D
This version writes the image as Portable Bit Map to stdout and doesn't move already set particles.

```c
#include <stdio.h

#include <stdlib.h>
#include <time.h>
#include <stdbool.h>

#define SIDE 600
#define NUM_PARTICLES 10000

bool W[SIDE][SIDE];

int main() {
    srand((unsigned)time(NULL));
    W[SIDE / 2][SIDE / 2] = true;

    for (int i = 0; i < NUM_PARTICLES; i++) {
        unsigned int x, y;
        OVER: do {
            x = rand() % (SIDE - 2) + 1;
            y = rand() % (SIDE - 2) + 1;
        } while (W[y][x]);

        while (W[y-1][x-1] + W[y-1][x] + W[y-1][x+1] +
               W[y][x-1]               + W[y][x+1] +
               W[y+1][x-1] + W[y+1][x] + W[y+1][x+1] == 0) {
            unsigned int dxy = rand() % 8;
            if (dxy > 3) dxy++;
            x += (dxy % 3) - 1;
            y += (dxy / 3) - 1;
            if (x < 1 || x >= SIDE - 1 || y < 1 || y >= SIDE - 1)
                goto OVER;
        }

        W[y][x] = true;
    }

    printf("P1\n%d %d\n", SIDE, SIDE);
    for (int r = 0; r < SIDE; r++) {
        for (int c = 0; c < SIDE; c++)
            printf("%d ", W[r][c]);
        putchar('\n');
    }
    return 0;
}
```

Run-time about 12.4 seconds with SIDE=600, NUM_PARTICLES=10000.


## C++

[[File:brownianTree_cpp.png|300px]]

For an animated version based on this same code see: [[Brownian tree/C++ animated]]

```cpp
#include <windows.h

#include <iostream>
#include <string>

//--------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------
enum states { SEED, GROWING, MOVING, REST };
enum treeStates { NONE, MOVER, TREE };
const int MAX_SIDE = 480, MAX_MOVERS = 511, MAX_CELLS = 15137;

//--------------------------------------------------------------------
class point
{
public:
    point()                  { x = y = 0; }
    point( int a, int b )    { x = a; y = b; }
    void set( int a, int b ) { x = a; y = b; }

    int x, y;
};
//--------------------------------------------------------------------
class movers
{
public:
    point pos;
    bool moving;
    movers() : moving( false ){}
};
//--------------------------------------------------------------------
class myBitmap
{
public:
    myBitmap() : pen( NULL ) {}
    ~myBitmap()
    {
	DeleteObject( pen );
	DeleteDC( hdc );
	DeleteObject( bmp );
    }

    bool create( int w, int h )
    {
	BITMAPINFO	bi;
	ZeroMemory( &bi, sizeof( bi ) );

	bi.bmiHeader.biSize        = sizeof( bi.bmiHeader );
	bi.bmiHeader.biBitCount	   = sizeof( DWORD ) * 8;
	bi.bmiHeader.biCompression = BI_RGB;
	bi.bmiHeader.biPlanes	   = 1;
	bi.bmiHeader.biWidth	   =  w;
	bi.bmiHeader.biHeight	   = -h;

	HDC dc = GetDC( GetConsoleWindow() );
	bmp = CreateDIBSection( dc, &bi, DIB_RGB_COLORS, &pBits, NULL, 0 );
	if( !bmp ) return false;

	hdc = CreateCompatibleDC( dc );
	SelectObject( hdc, bmp );
	ReleaseDC( GetConsoleWindow(), dc );

	width = w; height = h;

	return true;
    }

    void clear()
    {
	ZeroMemory( pBits, width * height * sizeof( DWORD ) );
    }

    void setPenColor( DWORD clr )
    {
	if( pen ) DeleteObject( pen );
	pen = CreatePen( PS_SOLID, 1, clr );
	SelectObject( hdc, pen );
    }

    void saveBitmap( string path )
    {
	BITMAPFILEHEADER fileheader;
	BITMAPINFO	 infoheader;
	BITMAP		 bitmap;
	DWORD*		 dwpBits;
	DWORD		 wb;
	HANDLE		 file;

	GetObject( bmp, sizeof( bitmap ), &bitmap );

	dwpBits = new DWORD[bitmap.bmWidth * bitmap.bmHeight];
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

	file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL,
                           CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
	WriteFile( file, &fileheader, sizeof( BITMAPFILEHEADER ), &wb, NULL );
	WriteFile( file, &infoheader.bmiHeader, sizeof( infoheader.bmiHeader ), &wb, NULL );
	WriteFile( file, dwpBits, bitmap.bmWidth * bitmap.bmHeight * 4, &wb, NULL );
	CloseHandle( file );

	delete [] dwpBits;
    }

    HDC getDC()     { return hdc; }
    int getWidth()  { return width; }
    int getHeight() { return height; }

private:
    HBITMAP bmp;
    HDC	    hdc;
    HPEN    pen;
    void    *pBits;
    int	    width, height;
};
//--------------------------------------------------------------------
class brownianTree
{
public:
    brownianTree()
    {
	_bmp.create( MAX_SIDE, MAX_SIDE );
	init();
    }

    void init()
    {
	_cellCount = 0;
	ZeroMemory( _grid, sizeof( _grid ) );
	_bmp.clear();
	_state = SEED;
    }

bool mainLoop()
    {
	switch( _state )
	{
	    case REST:    saveTree(); return false;
	    case SEED:    doSeed(); break;
	    case GROWING: startMovers(); break;
	    case MOVING:  moveMovers();
	}
	    return true;
	}

    myBitmap* getBmp() { return &_bmp; }

private:
    void saveTree()
    {
	for( int y = 0; y < MAX_SIDE; y++ )
	    for( int x = 0; x < MAX_SIDE; x++ )
		if( _grid[x][y] == TREE )
		    SetPixel( _bmp.getDC(), x, y, RGB( 255, 120, 0 ) );

        _bmp.saveBitmap( "f:\\rc\\tree.bmp" );
    }

    void doSeed()
    {
	int x = MAX_SIDE - MAX_SIDE / 2, y = MAX_SIDE / 4;
	_grid[rand() % x + y][rand() % x + y] = TREE;
	_cellCount++;
	_state = GROWING;
    }

    void addMover( movers* m )
    {
	m->moving = true;
	int x = MAX_SIDE - MAX_SIDE / 2, y = MAX_SIDE / 4, a, b;
	while( true )
	{
	    a = rand() % x + y; b = rand() % x + y;
	    if( _grid[a][b] == NONE ) break;
	}

	m->pos.set( a, b );
	_grid[a][b] = MOVER;
    }

    void startMovers()
    {
	movers* m;
	for( int c = 0; c < MAX_MOVERS; c++ )
	{
	    m = &_movers[c];
	    addMover( m );
	}
	_state = MOVING;
    }

    void addToTree( movers* m )
    {
	m->moving = false;
	_grid[m->pos.x][m->pos.y] = TREE;
	if( ++_cellCount >= MAX_CELLS ) _state = REST;

	COORD c = { 0, 1 };
	SetConsoleCursorPosition( GetStdHandle( STD_OUTPUT_HANDLE ), c );
	cout << "Cells added: " << _cellCount
             << " from " << MAX_CELLS << " => "
             <<  static_cast<float>( 100 * _cellCount ) /
                 static_cast<float>( MAX_CELLS )
             << "%              ";
    }

    bool moveIt( movers* m )
    {
	point f[8]; int ff = 0;
	for( int y = -1; y < 2; y++ )
	{
	    for( int x = -1; x < 2; x++ )
	    {
		if( !x && !y ) continue;
		int a = m->pos.x + x, b = m->pos.y + y;
		if( a < 0 || b < 0 || a >= MAX_SIDE || b >= MAX_SIDE )
		{
		    addToTree( m );
		    return true;
		}
		switch( _grid[a][b] )
		{
		    case TREE:
			addToTree( m );
			return true;
		    case NONE:
			f[ff++].set( a, b );
		}
	    }
        }

	if( ff < 1 ) return false;

	_grid[m->pos.x][m->pos.y] = NONE;
	m->pos = f[rand() % ff];
	_grid[m->pos.x][m->pos.y] = MOVER;

	return false;
    }

    void moveMovers()
    {
	movers* mm;
	for( int m = 0; m < MAX_MOVERS; m++ )
	{
	    mm = &_movers[m];
	    if( !mm->moving ) continue;
	    if( moveIt( mm ) && _cellCount < MAX_CELLS ) addMover( mm );
	}
    }

    states   _state;
    BYTE     _grid[MAX_SIDE][MAX_SIDE];
    myBitmap _bmp;
    int      _cellCount;
    movers   _movers[MAX_MOVERS];
};
//--------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );
    srand( GetTickCount() );

    brownianTree tree;

    DWORD now = GetTickCount();
    while( tree.mainLoop() );

    now = GetTickCount() - now;
    cout << endl << endl << "It took "
         << now / 1000
         << " seconds to complete the task!" << endl << endl;

    BitBlt( GetDC( GetConsoleWindow() ), 20, 90, MAX_SIDE, MAX_SIDE,
            tree.getBmp()->getDC(), 0, 0, SRCCOPY );

    system( "pause" );
    return 0;
}
//--------------------------------------------------------------------
```


## C\#

Works with C# 3.0

```csharp
using System;
using System.Drawing;

namespace BrownianTree
{
    class Program
    {
        static Bitmap BrownianTree(int size, int numparticles)
        {
            Bitmap bmp = new Bitmap(size, size);
            Rectangle bounds = new Rectangle { X = 0, Y = 0, Size = bmp.Size };
            using (Graphics g = Graphics.FromImage(bmp))
            {
                g.Clear(Color.Black);
            }
            Random rnd = new Random();
            bmp.SetPixel(rnd.Next(size), rnd.Next(size), Color.White);
            Point pt = new Point(), newpt = new Point();
            for (int i = 0; i < numparticles; i++)
            {
                pt.X = rnd.Next(size);
                pt.Y = rnd.Next(size);
                do
                {
                    newpt.X = pt.X + rnd.Next(-1, 2);
                    newpt.Y = pt.Y + rnd.Next(-1, 2);
                    if (!bounds.Contains(newpt))
                    {
                        pt.X = rnd.Next(size);
                        pt.Y = rnd.Next(size);
                    }
                    else if (bmp.GetPixel(newpt.X, newpt.Y).R > 0)
                    {
                        bmp.SetPixel(pt.X, pt.Y, Color.White);
                        break;
                    }
                    else
                    {
                        pt = newpt;
                    }
                } while (true);
            }
            return bmp;
        }

        static void Main(string[] args)
        {
            BrownianTree(300, 3000).Save("browniantree.png");
        }
    }
}
```


## Common Lisp

When the random walk lands on a set pixel it sets the pixel at the previous position.
An alternate method sets a pixel if the current position is vacant and at least one neighbour is set.
The former produces denser trees than the latter. If compiled with SBCL, providing a command line argument will invoke the latter method.
Requires Quicklisp library manager and the CL-GD package for producing PNG images.

```lisp
;;; brownian.lisp
;;; sbcl compile: first load and then (sb-ext:save-lisp-and-die "brownian" :executable t :toplevel #'brownian::main)
(ql:quickload "cl-gd")

(defpackage #:brownian
  (:use #:cl #:cl-gd))
(in-package #:brownian)

(defvar *size* 512)
(defparameter bitmap (make-array *size*))
(dotimes (i *size*)
  (setf (svref bitmap i) (make-array *size* :element-type 'bit)))

;;; is pixel at coord set? returns coord if so otherwise nil if not set or invalid
;;; type:pair->pair|nil
(defun set-p (coord)
  (and coord (= (sbit (svref bitmap (car coord)) (cdr coord)) 1) coord))

;;; valid coord predicate, return its arg if valid or nil otherwise
;;; type:pair->pair|nil
(defun coord-p (coord)
  (and ((lambda (v hi) (and (>= v 0) (< v hi))) (car coord) *size*)
       ((lambda (v hi) (and (>= v 0) (< v hi))) (cdr coord) *size*)
       coord))

;;; valid coord predicate for the ith neighbour, return its arg if valid or nil otherwise
;;; type:pair->pair|nil
(defun coordi-p (coord i)
  (coord-p (cons (+ (car coord) (nth i '(-1 -1 -1 0 0 1 1 1)))
                 (+ (cdr coord) (nth i '(-1 0 1 -1 1 -1 0 1))))))

;;; random walk until out of bounds or hit occupied pixel
;;; assumes start is valid vacant coord, return start or nil if off-grid
;;; type:pair->pair|nil
(defun random-walk (start)
  (let ((next (coordi-p start (random 8))))
    (if (set-p next) start
        (and next (random-walk next)))))

;;; random walk until out of bounds or or adjacent to occupied pixel
;;; assumes start is valid vacant coord, return start or nil if off-grid
;;; type:pair->pair|nil
(defun random-walk2 (start)
  (if (some #'set-p
            (remove-if #'null (mapcar (lambda (i) (coordi-p start i)) '(0 1 2 3 4 5 6 7))))
      start
      (let ((next (coordi-p start (random 8))))
        (and next (random-walk2 next)))))


(defparameter use-walk2 nil)
(defun main ()
  (setf *random-state* (make-random-state t)) ;; randomize
  (when (cdr sb-ext:*posix-argv*) (setf use-walk2 t)) ;; any cmd line arg -> use alt walk algorithm
  (with-image* (*size* *size*)
    (allocate-color 0 0 0) ; background color

    ;;; set the desired number of pixels in image as a pct (10%) of total
    (let ((target (truncate (* 0.10 (* *size* *size*))))
          (green (allocate-color 104 156 84)))

      (defun write-pixel (coord)
        (set-pixel (car coord) (cdr coord) :color green)
        (setf (sbit (svref bitmap (car coord)) (cdr coord)) 1)
        coord)

      ;; initial point set
      (write-pixel (cons (truncate (/ *size* 2)) (truncate (/ *size* 2))))

      ;; iterate until target # of pixels are set
      (do ((i 0 i)
           (seed (cons (random *size*) (random *size*))  (cons (random *size*) (random *size*))))
          ((= i target) )

        (let ((newcoord (and (not (set-p seed)) (if use-walk2 (random-walk2 seed) (random-walk seed)))))
          (when newcoord
            (write-pixel newcoord)
            (incf i)

            ;; report every 5% of progress
            (when (zerop (rem i (round (* target 0.05))))
              (format t "~A% done.~%" (round (/ i target 0.01))))))))

    (write-image-to-file "brownian.png"
                         :compression-level 6 :if-exists :supersede)))

```



## D

Uses the module of the Grayscale Image task.
Partially translated from PureBasic

```d
void main() {
    import core.stdc.stdio, std.random, grayscale_image;

    enum uint side = 600; // Square world side.
    enum uint num_particles = 10_000;
    static assert(side > 2 && num_particles < (side ^^ 2 * 0.7));

    auto rng = unpredictableSeed.Xorshift;
    ubyte[side][side] W;       // World.
    W[side / 2][side / 2] = 1; // Set tree root.

    foreach (immutable _; 0 .. num_particles) {
        // Random initial particle position.
        OVER: uint x, y;
        do {
            x = uniform(1, side - 1, rng);
            y = uniform(1, side - 1, rng);
        } while (W[y][x]); // Assure the chosen cell is empty.

        while (W[y-1][x-1] + W[y-1][x] + W[y-1][x+1] +
               W[y][x-1]               + W[y][x+1] +
               W[y+1][x-1] + W[y+1][x] + W[y+1][x+1] == 0) {
            // Randomly choose a direction (Moore neighborhood).
            uint dxy = uniform(0, 8, rng);
            if (dxy > 3) dxy++; // To avoid the center.
            x += (dxy % 3) - 1;
            y += (dxy / 3) - 1;
            if (x < 1 || x >= side - 1 || y < 1 || y >= side - 1)
                goto OVER;
        }

        W[y][x] = 1; // Touched, set the cell.
    }

    ubyte[] data = (&W[0][0])[0 .. side ^^ 2]; // Flat view.
    data[] += 255;
    Image!ubyte.fromData(data, side, side).savePGM("brownian_tree.pgm");
}
```

World side = 600, num_particles = 10_000,
cropped (about 20 seconds run-time with dmd, about 4.3 seconds with ldc2):
<center>[[File:Dla_10000_d.png]]</center>


## Delphi


```delphi
const
    SIZE = 256;
    NUM_PARTICLES = 1000;

procedure TForm1.Button1Click(Sender: TObject);
type
    TByteArray = array[0..0] of Byte;
    PByteArray = ^TByteArray;
var
    B: TBitmap;
    I: Integer;
    P, D: TPoint;
begin
    Randomize;
    B := TBitmap.Create;
    try
        B.Width := SIZE;
        B.Height := SIZE;
        B.PixelFormat := pf8bit;

        B.Canvas.Brush.Color := clBlack;
        B.Canvas.FillRect(B.Canvas.ClipRect);
        B.Canvas.Pixels[Random(SIZE), Random(SIZE)] := clWhite;

        For I := 0 to NUM_PARTICLES - 1 do
        Begin
            P.X := Random(SIZE);
            P.Y := Random(SIZE);

            While true do
            Begin
                D.X := Random(3) - 1;
                D.Y := Random(3) - 1;
                Inc(P.X, D.X);
                Inc(P.Y, D.Y);

                If ((P.X or P.Y) < 0) or (P.X >= SIZE) or (P.Y >= SIZE) Then
                Begin
                    P.X := Random(SIZE);
                    P.Y := Random(SIZE);
                end
                else if PByteArray(B.ScanLine[P.Y])^[P.X] <> 0 then
                begin
                    PByteArray(B.ScanLine[P.Y-D.Y])^[P.X-D.X] := $FF;
                    Break;
                end;
            end;
        end;

        Canvas.Draw(0, 0, B);
    finally
        FreeAndNil(B);
    end;
end;
```



## EasyLang


[https://easylang.online/apps/run.html?code=color%20999%0Alen%20f%5B%5D%20200%20%2A%20200%0Amove%20100%20100%0Arect%200.5%200.5%0Af%5B100%20%2A%20200%20%2B%20100%5D%20%3D%201%0An%20%3D%209000%0Awhile%20i%20%3C%20n%0Ax%20%3D%20random%20200%0Ay%20%3D%20random%20200%0Awhile%20f%5By%20%2A%20200%20%2B%20x%5D%20%3D%201%0Ax%20%3D%20random%20200%0Ay%20%3D%20random%20200%0A.%0Awhile%20x%20%3C%3E%20-1%0Axo%20%3D%20x%0Ayo%20%3D%20y%0Ax%20%2B%3D%20random%203%20-%201%0Ay%20%2B%3D%20random%203%20-%201%0Aif%20x%20%3C%200%20or%20y%20%3C%200%20or%20x%20%3E%3D%20200%20or%20y%20%3E%3D%20200%0Ax%20%3D%20-1%0Aelse%0Aif%20f%5By%20%2A%20200%20%2B%20x%5D%20%3D%201%0Amove%20xo%20/%202%20yo%20/%202%0Arect%200.5%200.5%0Af%5Byo%20%2A%20200%20%2B%20xo%5D%20%3D%201%0Ai%20%2B%3D%201%0Aif%20i%20mod%2016%20%3D%200%0Acolor_red%200.2%20%2B%20i%20/%20n%0Asleep%200%0A.%0Ax%20%3D%20-1%0A.%0A.%0A.%0A. Run it]

```easylang
color 999
len f[] 200 * 200
move 100 100
rect 0.5 0.5
f[100 * 200 + 100] = 1
n = 9000
while i < n
  x = random 200
  y = random 200
  while f[y * 200 + x] = 1
    x = random 200
    y = random 200
  .
  while x <> -1
    xo = x
    yo = y
    x += random 3 - 1
    y += random 3 - 1
    if x < 0 or y < 0 or x >= 200 or y >= 200
      x = -1
    else
      if f[y * 200 + x] = 1
        move xo / 2 yo / 2
        rect 0.5 0.5
        f[yo * 200 + xo] = 1
        i += 1
        if i mod 16 = 0
          color_red 0.2 + i / n
          sleep 0
        .
        x = -1
      .
    .
  .
.
```



## Factor

This example sets four spawn points, one in each corner of the image, giving the result a vague x-shaped appearance. For visual reasons, movement is restricted to diagonals. So be careful if you change the seed or spawns — they should all fall on the same diagonal.

```factor
USING: accessors images images.loader kernel literals math
math.vectors random sets ;
FROM: sets => in? ;
EXCLUDE: sequences => move ;
IN: rosetta-code.brownian-tree

CONSTANT: size 512
CONSTANT: num-particles 30000
CONSTANT: seed { 256 256 }
CONSTANT: spawns { { 10 10 } { 502 10 } { 10 502 } { 502 502 } }
CONSTANT: bg-color B{ 0 0 0 255 }
CONSTANT: fg-color B{ 255 255 255 255 }

: in-bounds? ( loc -- ? )
    dup { 0 0 } ${ size 1 - dup } vclamp = ;

: move ( loc -- loc' )
    dup 2 [ { 1 -1 } random ] replicate v+ dup in-bounds?
    [ nip ] [ drop ] if ;

: grow ( particles -- particles' )
    spawns random dup
    [ 2over swap in? ] [ drop dup move swap ] until nip
    swap [ adjoin ] keep ;

: brownian-data ( -- seq )
    HS{ $ seed } clone num-particles 1 - [ grow ] times { }
    set-like ;

: blank-bitmap ( -- bitmap )
    size sq [ bg-color ] replicate B{ } concat-as ;

: init-img ( -- img )
    <image>
    ${ size size }   >>dim
    BGRA             >>component-order
    ubyte-components >>component-type
    blank-bitmap     >>bitmap ;

: brownian-img ( -- img )
    init-img dup brownian-data
    [ swap [ fg-color swap first2 ] dip set-pixel-at ] with each
    ;

: save-brownian-tree-image ( -- )
    brownian-img "brownian.png" save-graphic-image ;

MAIN: save-brownian-tree-image
```

Output:

[https://i.imgur.com/qDVylB9.png image]


## Fantom



```fantom
using fwt
using gfx

class Main
{
  public static Void main ()
  {
    particles := Particles (300, 200)
    1000.times { particles.addParticle } // add 1000 particles
    Window // open up a display for the final tree
    {
      title = "Brownian Tree"
      EdgePane
      {
        center = ScrollPane { content = ParticleCanvas(particles) }
      },
    }.open
  }
}

class Particles
{
  Bool[][] image
  Int height
  Int width

  new make (Int height, Int width)
  {
    this.height = height
    this.width = width
    // set up initial image as an array of booleans with one set cell
    image = [,]
    width.times |w|
    {
      row := [,]
      height.times { row.add (false) }
      image.add (row)
    }
    image[Int.random(0..<width)][Int.random(0..<height)] = true
  }

  Bool get (Int w, Int h) { return image[w][h] }

  Void addParticle ()
  {
    x := Int.random(0..<width)
    y := Int.random(0..<height)

    Int dx := 0
    Int dy := 0
    while (!image[x][y]) // loop until hit existing part of the tree
    {
      dx = [-1,0,1].random
      dy = [-1,0,1].random

      if ((0..<width).contains(x + dx))
        x += dx
      else // did not change x, so set dx = 0
        dx = 0
      if ((0..<height).contains(y + dy))
        y += dy
      else
        dy = 0
    }

    // put x,y back to just before move onto existing part of tree
    x -= dx
    y -= dy

    image[x][y] = true
  }
}

class ParticleCanvas : Canvas
{
  Particles particles

  new make (Particles particles) { this.particles = particles }

  // provides canvas size for parent scrollpane
  override Size prefSize(Hints hints := Hints.defVal)
  {
    Size(particles.width, particles.height)
  }

  // repaint the display
  override Void onPaint (Graphics g)
  {
    g.brush = Color.black
    g.fillRect(0, 0, size.w, size.h)
    g.brush = Color.green
    particles.width.times |w|
    {
      particles.height.times |h|
      {
        if (particles.get(w, h)) // draw a 1x1 square for each set particle
          g.fillRect (w, h, 1, 1)
      }
    }
  }
}

```



## Fortran

Works with Fortran|95 and later

Translated from C

For RCImageBasic and RCImageIO, see [[Basic bitmap storage/Fortran]] and [[Write ppm file#Fortran]]


```fortran
program BrownianTree
  use RCImageBasic
  use RCImageIO

  implicit none

  integer, parameter :: num_particles = 1000
  integer, parameter :: wsize         = 800

  integer, dimension(wsize, wsize) :: world
  type(rgbimage) :: gworld
  integer :: x, y

  ! init seed
  call init_random_seed

  world = 0
  call draw_brownian_tree(world)

  call alloc_img(gworld, wsize, wsize)
  call fill_img(gworld, rgb(0,0,0))

  do y = 1, wsize
     do x = 1, wsize
        if ( world(x, y) /= 0 ) then
           call put_pixel(gworld, x, y, rgb(255, 255, 255))
        end if
     end do
  end do

  open(unit=10, file='browniantree.ppm', action='write')
  call output_ppm(10, gworld)
  close(10)

  call free_img(gworld)

contains

  ! this code is taken from the GNU gfortran online doc
  subroutine init_random_seed
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed

    call random_seed(size = n)
    allocate(seed(n))
    call system_clock(count = clock)
    seed = clock + 37 * (/ ( i - 1, i = 1, n) /)
    call random_seed(put = seed)
    deallocate(seed)
  end subroutine init_random_seed


  function randbetween(a, b) result(res) ! suppose a < b
    integer, intent(in) :: a, b
    integer :: res

    real :: r

    call random_number(r)

    res = a + int((b-a)*r + 0.5)

  end function randbetween

  function bounded(v, ll, ul) result(res)
    integer, intent(in) :: v, ll, ul
    logical res

    res = ( v >= ll ) .and. ( v <= ul )
  end function bounded


  subroutine draw_brownian_tree(w)
    integer, dimension(:,:), intent(inout) :: w

    integer :: px, py, dx, dy, i
    integer :: xsize, ysize

    xsize = size(w, 1)
    ysize = size(w, 2)

    w(randbetween(1, xsize), randbetween(1, ysize)) = 1

    do i = 1, num_particles
       px = randbetween(1, xsize)
       py = randbetween(1, ysize)

       do
          dx = randbetween(-1, 1)
          dy = randbetween(-1, 1)
          if ( .not. bounded(dx+px, 1, xsize) .or. .not. bounded(dy+py, 1, ysize) ) then
             px = randbetween(1, xsize)
             py = randbetween(1, ysize)
          else if ( w(px+dx, py+dy) /= 0 ) then
             w(px, py) = 1
             exit
          else
             py = py + dy
             px = px + dx
          end if
       end do
    end do

  end subroutine draw_brownian_tree

end program
```


## FreeBASIC


```freebasic
' version 16-03-2017
' compile with: fbc -s gui

Const As ULong w = 400
Const As ULong w1 = w -1

Dim As Long x, y, lastx, lasty
Dim As Long count, max = w * w \ 4

ScreenRes w, w, 8 ' windowsize 400 * 400, 8bit
' hit any key to stop or mouseclick on close window [X]
WindowTitle "hit any key to stop and close the window"

Palette 0, 0                   ' black
Palette 1, RGB(  1,   1,   1)  ' almost black
Palette 2, RGB(255, 255, 255)  ' white
Palette 3, RGB(  0, 255,   0)  ' green

Line (0, 0) - (w1, w1), 0, BF  ' make field black
Line (0, 0) - (w1, w1), 1, B   ' draw border in almost black color

Randomize Timer
x = Int(Rnd * 11) - 5
y = Int(Rnd * 11) - 5

PSet(w \ 2 + x, w \ 2 + y), 3 ' place seed near center

Do
    Do  ' create new particle
        x = Int(Rnd * w1) + 1
        y = Int(Rnd * w1) + 1
    Loop Until Point(x, y) = 0 ' black
    PSet(x, y), 2

    Do
        lastx = x
        lasty = y
        Do
            x = lastx + Int(Rnd * 3) -1
            y = lasty + Int(Rnd * 3) -1
        Loop Until Point(x, y) <> 1

        If Point(x, y) = 3 Then
            PSet(lastx, lasty), 3
            Exit Do ' exit do loop and create new particle
        End If

        PSet(lastx, lasty), 0
        PSet(x,y), 2

        If Inkey <> "" Or Inkey = Chr(255) + "k" Then
            End
        End If
    Loop

    count += 1
Loop Until count > max

Beep : Sleep 5000
End
```


## Gnuplot

Works with gnuplot|5.0 (patchlevel 3) and above


### Plotting helper file for load command

'''plotff.gp''' - Plotting from any data-file with 2 columns (space delimited), and writing to png-file.

Especially useful to plot colored fractals using points.

```gnuplot

## plotff.gp 11/27/16 aev
## Plotting from any data-file with 2 columns (space delimited), and writing to png-file.
## Especially useful to plot colored fractals using points.
## Note: assign variables: clr, filename and ttl (before using load command).
reset
set terminal png font arial 12 size 640,640
ofn=filename.".png"
set output ofn
unset border; unset xtics; unset ytics; unset key;
set size square
dfn=filename.".dat"
set title ttl font "Arial:Bold,12"
plot dfn using 1:2 with points  pt 7 ps 0.5 lc @clr
set output

```


### Plotting from PARI/GP generated dat-files

'''Note:''' dat-files are [[http://rosettacode.org/wiki/Brownian_tree#PARI.2FGP| PARI/GP]] generated output files.

[[File:BT1gp.png|right|thumb|Output BT1gp.png]]
[[File:BT2gp.png|right|thumb|Output BT2gp.png]]
[[File:BT3gp.png|right|thumb|Output BT3gp.png]]
[[File:BT41gp.png|right|thumb|Output BT41gp.png]]
[[File:BT42gp.png|right|thumb|Output BT42gp.png]]
[[File:BT43gp.png|right|thumb|Output BT43gp.png]]


```gnuplot

## BTff.gp 11/27/16 aev
## Plotting 6 Brownian tree pictures.
## dat-files are PARI/GP generated output files:
## http://rosettacode.org/wiki/Brownian_tree#PARI.2FGP
#cd 'C:\gnupData'

##BT1
clr = '"dark-green"'
filename = "BTAH1"
ttl = "Brownian Tree v.#1"
load "plotff.gp"

##BT2
clr = '"brown"'
filename = "BTOC1"
ttl = "Brownian Tree v.#2"
load "plotff.gp"

##BT3
clr = '"navy"'
filename = "BTSE1"
ttl = "Brownian Tree v.#3"
load "plotff.gp"

##BT41
clr = '"red"'
filename = "BTPB1"
ttl = "Brownian Tree v.#4-1"
load "plotff.gp"

##BT42
clr = '"red"'
filename = "BTPB2"
ttl = "Brownian Tree v.#4-2"
load "plotff.gp"

##BT43
clr = '"red"'
filename = "BTPB3"
ttl = "Brownian Tree v.#4-3"
load "plotff.gp"

```

Output:

```txt

1. All BTff.gp commands.
2. All plotted png-files:
   BT1gp.png, BT2gp.png, BT3gp.png, BT41gp.png, BT43gp.png, BT43gp.png.

```



## Go

[[file:GoTree.png|right|thumb|Output png]]
The interpretation here of "collide" in the case of a new particle generated on top of a pixel of the existing tree is not to ignore the particle, but to find a place for it nearby.  This properly increases the brightness of the area, reflecting that a particle was generated in the area.  Visually, it appears to strengthen existing spines of the tree.

Using standard image library:

```go
package main

import (
    "fmt"
    "image"
    "image/color"
    "image/png"
    "math/rand"
    "os"
)

const w = 400     // image width
const h = 300     // image height
const n = 15000   // number of particles to add
const frost = 255 // white

var g *image.Gray

func main() {
    g = image.NewGray(image.Rectangle{image.Point{0, 0}, image.Point{w, h}})
    // off center seed position makes pleasingly asymetrical tree
    g.SetGray(w/3, h/3, color.Gray{frost})
generate:
    for a := 0; a < n; {
        // generate random position for new particle
        rp := image.Point{rand.Intn(w), rand.Intn(h)}
        if g.At(rp.X, rp.Y).(color.Gray).Y == frost {
            // position is already set.  find a nearby free position.
            for {
                rp.X += rand.Intn(3) - 1
                rp.Y += rand.Intn(3) - 1
                // execpt if we run out of bounds, consider the particle lost.
                if !rp.In(g.Rect) {
                    continue generate
                }
                if g.At(rp.X, rp.Y).(color.Gray).Y != frost {
                    break
                }
            }
        } else {
            // else particle is in free space.  let it wander
            // until it touches tree
            for !hasNeighbor(rp) {
                rp.X += rand.Intn(3) - 1
                rp.Y += rand.Intn(3) - 1
                // but again, if it wanders out of bounds consider it lost.
                if !rp.In(g.Rect) {
                    continue generate
                }
            }
        }
        // x, y now specify a free position toucing the tree.
        g.SetGray(rp.X, rp.Y, color.Gray{frost})
        a++
        // progress indicator
        if a%100 == 0 {
            fmt.Println(a, "of", n)
        }
    }
    f, err := os.Create("tree.png")
    if err != nil {
        fmt.Println(err)
        return
    }
    err = png.Encode(f, g)
    if err != nil {
        fmt.Println(err)
    }
    f.Close()
}

var n8 = []image.Point{
    {-1, -1}, {-1, 0}, {-1, 1},
    {0, -1}, {0, 1},
    {1, -1}, {1, 0}, {1, 1}}

func hasNeighbor(p image.Point) bool {
    for _, n := range n8 {
        o := p.Add(n)
        if o.In(g.Rect) && g.At(o.X, o.Y).(color.Gray).Y == frost {
            return true
        }
    }
    return false
}
```

Nearly the same, version below works with code from the bitmap task:

```go
package main

// Files required to build supporting package raster are found in:
// * Bitmap
// * Grayscale image
// * Write a PPM file

import (
    "fmt"
    "math/rand"
    "raster"
)

const w = 400       // image width
const h = 300       // image height
const n = 15000     // number of particles to add
const frost = 65535 // white

var g *raster.Grmap

func main() {
    g = raster.NewGrmap(w, h)
    // off center seed position makes pleasingly asymetrical tree
    g.SetPx(w/3, h/3, frost)
    var x, y int
generate:
    for a := 0; a < n; {
        // generate random position for new particle
        x, y = rand.Intn(w), rand.Intn(h)
        switch p, ok := g.GetPx(x, y); p {
        case frost:
            // position is already set.  find a nearby free position.
            for p == frost {
                x += rand.Intn(3) - 1
                y += rand.Intn(3) - 1
                p, ok = g.GetPx(x, y)

                // execpt if we run out of bounds, consider the particle lost.
                if !ok {
                    continue generate
                }
            }
        default:
            // else particle is in free space.  let it wander
            // until it touches tree
            for !hasNeighbor(x, y) {
                x += rand.Intn(3) - 1
                y += rand.Intn(3) - 1
                // but again, if it wanders out of bounds consider it lost.
                _, ok = g.GetPx(x, y)
                if !ok {
                    continue generate
                }
            }
        }
        // x, y now specify a free position toucing the tree.
        g.SetPx(x, y, frost)
        a++
        // progress indicator
        if a%100 == 0 {
            fmt.Println(a, "of", n)
        }
    }
    g.Bitmap().WritePpmFile("tree.ppm")
}

var n8 = [][]int{
    {-1, -1}, {-1, 0}, {-1, 1},
    { 0, -1},          { 0, 1},
    { 1, -1}, { 1, 0}, { 1, 1}}

func hasNeighbor(x, y int) bool {
    for _, n := range n8 {
        if p, ok := g.GetPx(x+n[0], y+n[1]); ok && p == frost {
            return true
        }
    }
    return false
}
```



## Haskell


The modules <code>[[Bitmap#Haskell|Bitmap]]</code>,
<code>[[Bitmap/Write a PPM file#Haskell|Bitmap.Netpbm]]</code>,
and <code>[[Bitmap/Histogram#Haskell|Bitmap.BW]]</code> are on RosettaGit.
The commented-out type signatures require
[http://hackage.haskell.org/trac/haskell-prime/wiki/ScopedTypeVariables scoped type variables]
in order to function.


```haskell
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import System.Random
import Bitmap
import Bitmap.BW
import Bitmap.Netpbm

main = do
    g <- getStdGen
    (t, _) <- stToIO $ drawTree (50, 50) (25, 25) 300 g
    writeNetpbm "/tmp/tree.pbm" t

drawTree :: (Int, Int) -> (Int, Int) -> Int -> StdGen -> ST s (Image s BW, StdGen)
drawTree (width, height) start steps stdgen = do
    img <- image width height off
    setPix img (Pixel start) on
    gen <- newSTRef stdgen
    let -- randomElem :: [a] -> ST s a
        randomElem l = do
            stdgen <- readSTRef gen
            let (i, stdgen') = randomR (0, length l - 1) stdgen
            writeSTRef gen stdgen'
            return $ l !! i
        -- newPoint :: ST s (Int, Int)
        newPoint = do
            p <- randomElem border
            c <- getPix img $ Pixel p
            if c == off then return p else newPoint
        -- wander :: (Int, Int) -> ST s ()
        wander p = do
            next <- randomElem $ filter (inRange pointRange) $ adjacent p
            c <- getPix img $ Pixel next
            if c == on then setPix img (Pixel p) on else wander next
    replicateM_ steps $ newPoint >>= wander
    stdgen <- readSTRef gen
    return (img, stdgen)
  where pointRange = ((0, 0), (width - 1, height - 1))
        adjacent (x, y) = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                           (x - 1, y),                 (x + 1, y),
                           (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
        border = liftM2 (,) [0, width - 1] [0 .. height - 1] ++
                 liftM2 (,) [1 .. width - 2] [0, height - 1]
        off = black
        on = white
```


## Icon / Unicon

[[File:Brownian_tree_unicon.png|400px|thumb|right|400x400 PA=70% SA=50% D=8%]]
In this version the seed is randomly set within an inner area and particles are injected in an outer ring.


```Icon
link graphics,printf

procedure main()             # brownian tree

Density  := .08              # % particles to area
SeedArea := .5               # central area to confine seed
ParticleArea := .7           # central area to exclude particles appearing
Height := Width := 400       # canvas

Particles := Height * Width * Density
Field := list(Height)
every !Field := list(Width)

Size := sprintf("size=%d,%d",Width,Height)
Fg   := sprintf("fg=%s",?["green","red","blue"])
Label := sprintf("label=Brownian Tree %dx%d PA=%d%% SA=%d%% D=%d%%",
         Width,Height,ParticleArea*100,SeedArea*100,Density*100)
WOpen(Label,Size,Fg,"bg=black") | stop("Unable to open Window")

sx := Height * SetInside(SeedArea)
sy := Width  * SetInside(SeedArea)
Field[sx,sy] := 1
DrawPoint(sx,sy)             # Seed the field

Lost := 0

every 1 to Particles do {
   repeat {
      px := Height * SetOutside(ParticleArea)
      py := Width  * SetOutside(ParticleArea)
      if /Field[px,py] then
         break               # don't materialize in the tree
      }
   repeat {
      dx := delta()
      dy := delta()
      if not ( xy := Field[px+dx,py+dy] ) then {
         Lost +:= 1
         next                # lost try again
         }
      if \xy then
         break               # collision

      px +:= dx              # move to clear spot
      py +:= dy
      }
   Field[px,py] := 1
   DrawPoint(px,py)          # Stick the particle
   }
printf("Brownian Tree Complete: Particles=%d Lost=%d.\n",Particles,Lost)
WDone()
end

procedure delta()            #: return a random 1 pixel perturbation
   return integer(?0 * 3) - 1
end

procedure SetInside(core)    #: set coord inside area
   return core * ?0 + (1-core)/2
end

procedure SetOutside(core)   #: set coord outside area
   pt := ?0 * (1 - core)
   pt +:= ( pt > (1-core)/2, core)
   return pt
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/graphics.icn graphics.icn provides graphics]
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf]


## J

```j
brtr=:4 :0
  seed=. ?x
  clip=. 0 >. (<:x) <."1 ]
  near=. [: clip +"1/&(,"0/~i:1)
  p=.i.0 2
  mask=. 1 (<"1 near seed)} x$0
  field=.1 (<seed)} x$0
  for.i.y do.
    p=. clip (p +"1 <:?3$~$p),?x
    b=.(<"1 p) { mask
    fix=. b#p
    if.#fix do. NB. if. works around j602 bug: 0(0#a:)}i.0 0
      p=. (-.b)# p
      mask=. 1 (<"1 near fix)} mask
      field=. 1 (<"1 fix)} field
    end.
  end.
  field
)
```

Example use:

```j
   require'viewmat'
   viewmat 480 640 brtr 30000
```


Note that building a brownian tree like this takes a while and would be more interesting if this were animated.


## Java

```java
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.util.*;
import javax.swing.JFrame;

public class BrownianTree extends JFrame implements Runnable {

    BufferedImage I;
    private List<Particle> particles;
    static Random rand = new Random();

    public BrownianTree() {
        super("Brownian Tree");
        setBounds(100, 100, 400, 300);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        I = new BufferedImage(getWidth(), getHeight(), BufferedImage.TYPE_INT_RGB);
        I.setRGB(I.getWidth() / 2, I.getHeight() / 2, 0xff00);
        particles = new LinkedList<Particle>();
    }

    @Override
    public void paint(Graphics g) {
        g.drawImage(I, 0, 0, this);
    }

    public void run() {
        for (int i = 0; i < 20000; i++) {
            particles.add(new Particle());
        }
        while (!particles.isEmpty()) {
            for (Iterator<Particle> it = particles.iterator(); it.hasNext();) {
                if (it.next().move()) {
                    it.remove();
                }
            }
            repaint();
        }
    }

    public static void main(String[] args) {
        BrownianTree b = new BrownianTree();
        b.setVisible(true);
        new Thread(b).start();
    }

    private class Particle {

        private int x, y;

        private Particle() {
            x = rand.nextInt(I.getWidth());
            y = rand.nextInt(I.getHeight());
        }

        /* returns true if either out of bounds or collided with tree */
        private boolean move() {
            int dx = rand.nextInt(3) - 1;
            int dy = rand.nextInt(3) - 1;
            if ((x + dx < 0) || (y + dy < 0)
                    || (y + dy >= I.getHeight()) || (x + dx >= I.getWidth())) {
                return true;
            }
            x += dx;
            y += dy;
            if ((I.getRGB(x, y) & 0xff00) == 0xff00) {
                I.setRGB(x - dx, y - dy, 0xff00);
                return true;
            }
            return false;
        }
    }
}
```


This is an alternate version which is a port of most of the code here.
This code does not use a GUI and saves the output to image.png.

```Java
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

public class BasicBrownianTree {

    private int pixelsLost;
    private Point p;
    private Point nextP;
    private int pixelCount;
    private int width;
    private int height;
    private int color;
    private BufferedImage img;

    public BasicBrownianTree( int argb, int size, double density ) {
        pixelsLost = 0;
        p = new Point();
        nextP = new Point();
        width = size;
        height = size;
        color = argb;
        pixelCount = (int) ( width * height * density );
        img = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
    }

    public void generate() {
        // print text to the console
        System.out.println( "Drawing " + pixelCount + " pixels" );
        int background = img.getRGB( 0, 0 );
        img.setRGB( width / 2, height / 2, color );

        for( int i = 0; i < pixelCount; i++ ) {
            p.x = (int) ( Math.random() * width );
            p.y = (int) ( Math.random() * height );

            while ( true ) {
                int dx = (int) ( Math.random() * 3 ) - 1;
                int dy = (int) ( Math.random() * 3 ) - 1;
                nextP.setLocation( p.x + dx, p.y + dy );
                // handle out-of-bounds
                if ( nextP.x < 0 || nextP.x >= width || nextP.y < 0
                        || nextP.y >= height ) {
                        // increment the number of pixels lost and escape the loop
                    pixelsLost++;
                    break;
                }
                if ( img.getRGB( nextP.x, nextP.y ) != background ) {
                    img.setRGB( p.x, p.y, color );
                    break;
                }
                p.setLocation( nextP );
            }
            // Print a message every 2%
            if ( i % ( pixelCount / 50 ) == 0 ) {
                System.out.println( "Done with " + i + " pixels" );
            }
        }
        // We're done. Let the user know how many pixels were lost
        System.out.println( "Finished. Pixels lost = " + pixelsLost );
    }

    public BufferedImage getImage() {
        return img;
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    public static void main( String[] args ) {
        // create the new generator
        BasicBrownianTree generator = new BasicBrownianTree( 0x664444ff, 400, 0.4 );
        // generate the image
        generator.generate();
        try {
            // save the image to the file "image.png"
            ImageIO.write( generator.getImage(), "png", new File( "image.png" ) );
        } catch ( IOException e ) {
            e.printStackTrace();
        }
    }
}
```


## JavaScript

[http://switchb.org/kpreid/2010/brownian-tree/ Live version]
<!-- If changing this example, add note this link is outdated -->

```javascript
function brownian(canvasId, messageId) {
  var canvas = document.getElementById(canvasId);
  var ctx = canvas.getContext("2d");

  // Options
  var drawPos = true;
  var seedResolution = 50;
  var clearShade = 0; // 0..255

  // Static state
  var width = canvas.width;
  var height = canvas.height;
  var cx = width/2;
  var cy = height/2;
  var clearStyle = "rgba("+clearShade+", "+clearShade+", "+clearShade+", 1)";

  // Utilities
  function radius(x,y) {
    return Math.sqrt((x-cx)*(x-cy)+(y-cx)*(y-cy));
  }
  function test(x, y) {
    if (x < 0 || y < 0 || x >= width || y >= height)
      return false;
    var data = ctx.getImageData(x, y, 1, 1).data;
    return data[0] != clearShade || data[1] != clearShade || data[2] != clearShade;
  }
  var shade = 120;
  function setc(x, y, c) {
    //var imgd = ctx.createImageData(1, 1);
    //var pix = imgd.data;
    //pix[0] = pix[1] = pix[2] = c == 255 ? 255 : shade;
    //pix[3] = 255;
    //shade = (shade + 1) % 254;
    //ctx.putImageData(imgd, x, y);
    //ctx.fillStyle = "rgba("+c+", "+c+", "+c+", 1)";
    shade = (shade + 0.02) % 360;
    if (c) {
      ctx.fillStyle = "hsl("+shade+", 100%, 50%)";
    } else {
      ctx.fillStyle = clearStyle;
    }
    ctx.fillRect (x, y, 1, 1);
  }
  function set(x,y) {
    setc(x,y,true);
  }
  function clear(x,y) {
    setc(x,y,false);
  }

  // Initialize canvas to blank opaque
  ctx.fillStyle = clearStyle;
  ctx.fillRect (0, 0, width, height);

  // Current position
  var x;
  var y;

  // Farthest distance from center a particle has yet been placed.
  var closeRadius = 1;

  // Place seed
  set(cx, cy);

  // Choose a new random position for a particle (not necessarily unoccupied)
  function newpos() {
    // Wherever particles are injected, the tree will tend to grow faster
    // toward it. Ideally, particles wander in from infinity; the best we
    // could do is to have them wander in from the edge of the field.
    // But in order to have the rendering occur in a reasonable time when
    // the seed is small, without too much visible bias, we instead place
    // the particles in a coarse grid. The final tree will cover every
    // point on the grid.
    //
    // There's probably a better strategy than this.
    x = Math.floor(Math.random()*(width/seedResolution))*seedResolution;
    y = Math.floor(Math.random()*(height/seedResolution))*seedResolution;
  }
  newpos();

  var animation;
  animation = window.setInterval(function () {
    if (drawPos) clear(x,y);
    for (var i = 0; i < 10000; i++) {
      var ox = x;
      var oy = y;

      // Changing this to use only the first four directions will result
      // in a denser tree.
      switch (Math.floor(Math.random()*8)) {
        case 0: x++; break;
        case 1: x--; break;
        case 2: y++; break;
        case 3: y--; break;
        case 4: x++; y++; break;
        case 5: x--; y++; break;
        case 6: x++; y--; break;
        case 7: x--; y--; break;
      }
      if (x < 0 || y < 0 ||
          x >= width || y >= height ||
          radius(x,y) > closeRadius+seedResolution+2) {
        // wandered out of bounds or out of interesting range of the
        // tree, so pick a new spot
        var progress = 1000;
        do {
          newpos();
          progress--;
        } while ((test(x-1,y-1) || test(x,y-1) || test(x+1,y-1) ||
                  test(x-1,y  ) || test(x,y  ) || test(x+1,y  ) ||
                  test(x-1,y+1) || test(x,y+1) || test(x+1,y+1)) && progress > 0);
        if (progress <= 0) {
          document.getElementById(messageId).appendChild(
              document.createTextNode("Stopped for lack of room."));
          clearInterval(animation);
          break;
        }
      }
      if (test(x, y)) {
        // hit something, mark where we came from and pick a new spot
        set(ox,oy);
        closeRadius = Math.max(closeRadius, radius(ox,oy));
        newpos();
      }
   }
   if (drawPos) set(x,y);
  }, 1);

}
```


```html
<html>
 <head>
  <script src="brownian.js"></script>
 </head>
 <body onload="brownian('canvas', 'message')">
   <canvas id="canvas" width="402" height="402" style="border: 2px inset;"></canvas>
   <div id="message"></div>
 </body>
</html>
```


## Julia

Works with Julia 0.6
This solution puts the seed in the center of the canvas.
Motes are generated randomly in space and do a simple drunkard's walk until they hit the tree or leave the canvas (unless the sides are made <tt>side</tt>).
The Motes are colorized according to their &theta; in polar coordinates.


```julia
using Images, FileIO

function main(h::Integer, w::Integer, side::Bool=false)
    W0 = w >> 1
    H0 = h >> 1
    @inline function motecolor(x::Integer, y::Integer)
        h = clamp(180 * (atan2(y - H0, x - W0) / π + 1.0), 0.0, 360.0)
        return HSV(h, 0.5, 0.5)
    end

    img = zeros(RGB{N0f8}, h, w)
    img[H0, W0] = RGB(1, 1, 1)
    free = trues(h, w)
    free[H0, W0] = false
    for i in eachindex(img)
        x = rand(1:h)
        y = rand(1:w)
        free[x, y] || continue
        mc = motecolor(x, y)
        for j in 1:1000
            xp = x + rand(-1:1)
            yp = y + rand(-1:1)
            contained = checkbounds(Bool, img, xp, yp)
            if contained && free[xp, yp]
                x, y = xp, yp
                continue
            else
                if side || (contained && !free[xp, yp])
                    free[x, y] = false
                    img[x, y] = mc
                end
                break
            end
        end
    end
    return img
end

imgnoside = main(256, 256)
imgwtside = main(256, 256, true)
save("data/browniantree_noside.jpg", imgnoside)
save("data/browniantree_wtside.jpg", imgwtside)
```



## Kotlin

Translated from Java

```kotlin
// version 1.1.2

import java.awt.Graphics
import java.awt.image.BufferedImage
import java.util.*
import javax.swing.JFrame

class BrownianTree : JFrame("Brownian Tree"), Runnable {
    private val img: BufferedImage
    private val particles = LinkedList<Particle>()

    private companion object {
        val rand = Random()
    }

    private inner class Particle {
        private var x = rand.nextInt(img.width)
        private var y = rand.nextInt(img.height)

        /* returns true if either out of bounds or collided with tree */
        fun move(): Boolean {
            val dx = rand.nextInt(3) - 1
            val dy = rand.nextInt(3) - 1
            if ((x + dx < 0) || (y + dy < 0) || (y + dy >= img.height) ||
                (x + dx >= img.width)) return true
            x += dx
            y += dy
            if ((img.getRGB(x, y) and 0xff00) == 0xff00) {
                img.setRGB(x - dx, y - dy, 0xff00)
                return true
            }
            return false
        }
    }

    init {
        setBounds(100, 100, 400, 300)
        defaultCloseOperation = EXIT_ON_CLOSE
        img = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
        img.setRGB(img.width / 2, img.height / 2, 0xff00)
    }

    override fun paint(g: Graphics) {
        g.drawImage(img, 0, 0, this)
    }

    override fun run() {
        (0 until 20000).forEach { particles.add(Particle()) }

        while (!particles.isEmpty()) {
            val iter = particles.iterator()
            while (iter.hasNext()) {
                if (iter.next().move()) iter.remove()
            }
            repaint()
        }
    }
}

fun main(args: Array<String>) {
    val b = BrownianTree()
    b.isVisible = true
    Thread(b).start()
}
```



## Liberty BASIC


```lb
'[RC]Brownian motion tree
    nomainwin
    dim screen(600,600)
    WindowWidth = 600
    WindowHeight = 600
    open "Brownian" for graphics_nsb_nf as #1
    #1 "trapclose [quit]"
    #1 "down ; fill blue"
    rad=57.29577951
    particles=500

    'draw starting circle and mid point
    for n= 1 to 360
            x=300-(200*sin(n/rad))
            y=300-(200*cos(n/rad))
            #1, "color white ; set ";x;" ";y
            screen(x,y)=1
    next n
    #1, "color white ; set 300 300"
    screen(300,300)=1

    'set up initial particles
    dim particle(particles,9)'x y deltax deltay rotx roty
    for n = 1 to particles
        gosub [randomparticle]
    next

    'start timed drawing loop
    timer 17, [draw]
    wait


    [draw]
    #1 "discard"
    scan
    for n = 1 to particles
        oldx=particle(n,1)
        oldy=particle(n,2)

        'erase particle
        if not(screen(oldx,oldy)) then
            #1 "color blue ; set ";oldx;" ";oldy
        end if

        'move particle x
        particle(n,1)=particle(n,1)+int((sin(particle(n,6)/rad)*10)+particle(n,3))
        particle(n,5)=particle(n,5)+6 mod 360
        if particle(n,1)>599 or particle(n,1)<1 then gosub [randomparticle]

        'move particle y
        particle(n,2)=particle(n,2)+int((sin(particle(n,5)/rad)*10)+particle(n,4))
        particle(n,6)=particle(n,6)+6 mod 360
        if particle(n,2)>599 or particle(n,2)<1 then gosub [randomparticle]

        'checkhit
        x=particle(n,1)
        y=particle(n,2)
        if screen(x-1,y-1) or screen(x-1,y) or screen(x-1,y+1)_
        or screen(x,y-1) or screen(x,y) or screen(x,y+1)_
        or screen(x+1,y-1) or screen(x+1,y) or screen(x+1,y+1) then
            #1 "color white ; set ";particle(n,1);" ";particle(n,2)
            screen(particle(n,1),particle(n,2))=1
        else
            #1 "color red ; set ";particle(n,1);" ";particle(n,2)
        end if
    next
    wait



    [randomparticle]
    particle(n,1)=int(rnd(0)*599)+1
    particle(n,2)=int(rnd(0)*599)+1
    particle(n,3)=int(2-rnd(0)*4)
    particle(n,4)=int(2-rnd(0)*4)
    particle(n,5)=int(rnd(0)*360)
    particle(n,6)=int(rnd(0)*360)
    return

    [quit]
    timer 0
    close #1
    end
```


## Lua

The output is stored in as a ppm-image.
The source code of these output-functions is located at
[[Bitmap/Write a PPM file#Lua]],
[[Grayscale image#Lua]],
[[Basic bitmap storage#Lua]].

```lua
function SetSeed( f )
    for i = 1, #f[1] do         -- the whole boundary of the scene is used as the seed
        f[1][i]  = 1
        f[#f][i] = 1
    end
    for i = 1, #f do
        f[i][1]     = 1
        f[i][#f[1]] = 1
    end
end

function SetParticle( f )
    local pos_x, pos_y
    repeat
        pos_x = math.random( #f )
        pos_y = math.random( #f[1] )
    until f[pos_x][pos_y] == 0

    return pos_x, pos_y
end


function Iterate( f, num_particles )
    for i = 1, num_particles do
        local pos_x, pos_y = SetParticle( f )

        while true do
            local dx = math.random(5) - 3
            local dy = math.random(5) - 3

            if ( pos_x+dx >= 1 and pos_x+dx <= #f and pos_y+dy >= 1 and pos_y+dy <= #f[1] ) then
                if f[pos_x+dx][pos_y+dy] ~= 0 then
                    f[pos_x][pos_y] = 1
                    break
                else
                    pos_x = pos_x + dx
                    pos_y = pos_y + dy
                end
            end
        end
    end
end


size_x, size_y = 400, 400       -- size of the scene
num_particles  = 16000

math.randomseed( os.time() )

f = {}
for i = 1, size_x do
    f[i] = {}
    for j = 1, size_y do
        f[i][j] = 0
    end
end

SetSeed( f )
Iterate( f, num_particles )

-- prepare the data for writing into a ppm-image file
for i = 1, size_x do
    for j = 1, size_y do
        if f[i][j] == 1 then f[i][j] = 255 end
    end
end
Write_PPM( "brownian_tree.ppm", ConvertToColorImage(f) )
```



## Mathematica

There is a [http://demonstrations.wolfram.com/DiffusionLimitedAggregation/ prettier version] at the Mathematica demo site.
Its source code is also available there but it is not mine.

Loose translation from D.

```Mathematica
canvasdim = 1000;
n = 0.35*canvasdim^2;
canvas = ConstantArray[0, {canvasdim, canvasdim}];
init = Floor@(0.5*{canvasdim, canvasdim});  (*RandomInteger[canvasdim,2]*)
canvas[[init[[1]], init[[2]]]] = 1;         (*1st particle initialized to midpoint*)

Monitor[                                    (*Provides real-time intermediate result monitoring*)
 Do[
  particle = RandomInteger[canvasdim, 2];
  While[True,
   ds = RandomInteger[{-1, 1}, 2];
   While[                                   (*New Particle Domain Limit Section*)
    !And @@ (0 < (particle + ds)[[#]] <= canvasdim & /@ {1, 2}),
    particle = RandomInteger[canvasdim, 2];
    ];
                                            (* Particle Aggregation Section *)
   If[canvas[[(particle + ds)[[1]], (particle + ds)[[2]]]] > 0,
    canvas[[particle[[1]], particle[[2]]]] = i;
    Break[],
    particle += ds
    ];
   ],
  {i, n}],
 {i, (particle + ds), MatrixPlot@canvas}
 ]
MatrixPlot[canvas,FrameTicks->None,ColorFunction->"DarkRainbow",ColorRules->{0 -> None}]
```


Result:

[[File:BrownianTree.png]]


## OCaml

Translated from D


```ocaml
let world_width = 400
let world_height = 400
let num_particles = 20_000

let () =
  assert(num_particles > 0);
  assert(world_width * world_height > num_particles);
;;

let dla ~world =
  (* put the tree seed *)
  world.(world_height / 2).(world_width / 2) <- 1;

  for i = 1 to num_particles do
    (* looping helper function *)
    let rec aux px py =
      (* randomly choose a direction *)
      let dx = (Random.int 3) - 1  (* offsets *)
      and dy = (Random.int 3) - 1 in

      if dx + px < 0 || dx + px >= world_width ||
         dy + py < 0 || dy + py >= world_height then
        (* plop the particle into some other random location *)
        aux (Random.int world_width) (Random.int world_height)
      else if world.(py + dy).(px + dx) <> 0 then
        (* bumped into something, particle set *)
        world.(py).(px) <- 1
      else
        aux (px + dx) (py + dy)
    in
    (* set particle's initial position *)
    aux (Random.int world_width) (Random.int world_height)
  done

let to_pbm ~world =
  print_endline "P1";  (* Type=Portable bitmap, Encoding=ASCII *)
  Printf.printf "%d %d\n" world_width world_height;
  Array.iter (fun line ->
    Array.iter print_int line;
    print_newline()
  ) world

let () =
  Random.self_init();
  let world = Array.make_matrix world_width world_height 0 in
  dla ~world;
  to_pbm ~world;
;;
```


better to compile to native code to get a faster program:


```txt
$ ocamlopt -o brownian_tree.opt brownian_tree.ml
$ ./brownian_tree.opt | display -
```



## Octave

Translated from C


```octave
function r = browniantree(xsize, ysize = xsize, numparticle = 1000)
  r = zeros(xsize, ysize, "uint8");
  r(unidrnd(xsize), unidrnd(ysize)) = 1;

  for i = 1:numparticle
    px = unidrnd(xsize-1)+1;
    py = unidrnd(ysize-1)+1;
    while(1)
      dx = unidrnd(2) - 1;
      dy = unidrnd(2) - 1;
      if ( (dx+px < 1) || (dx+px > xsize) || (dy+py < 1) || (dy+py > ysize) )
	px = unidrnd(xsize-1)+1;
	py = unidrnd(ysize-1)+1;
      elseif ( r(px+dx, py+dy) != 0 )
	r(px, py) = 1;
	break;
      else
	px += dx;
	py += dy;
      endif
    endwhile
  endfor
endfunction

r = browniantree(200);
r( r > 0 ) = 255;
jpgwrite("browniantree.jpg", r, 100); % image package
```



## PARI/GP

All versions #1 - #4 are based on using 4 small plotting helper functions, which are allowing to unify
all upgraded BrownianTreeX() functions and make them shorter.

Note: all pictures are still almost the same after upgrading.
Works with PARI/GP|2.9.1 and above


### Plotting helper functions


```parigp

\\ 2 old plotting helper functions 3/2/16 aev
\\ insm(): Check if x,y are inside matrix mat (+/- p deep).
insm(mat,x,y,p=0)={my(xz=#mat[1,],yz=#mat[,1]);
  return(x+p>0 && x+p<=xz && y+p>0 && y+p<=yz && x-p>0 && x-p<=xz && y-p>0 && y-p<=yz)}
\\ plotmat(): Simple plotting using a square matrix mat (filled with 0/1).
plotmat(mat)={
  my(xz=#mat[1,],yz=#mat[,1],vx=List(),vy=vx,x,y);
  for(i=1,yz, for(j=1,xz, if(mat[i,j]==0, next, listput(vx,i); listput(vy,j))));
  print(" *** matrix(",xz,"x",yz,") ",#vy, " DOTS");
  plothraw(Vec(vx),Vec(vy));
}
\\ 2 new plotting helper functions 11/27/16 aev
\\ wrtmat(): Writing file fn containing X,Y coordinates from matrix mat.
\\ Created primarily for using file in Gnuplot, also for re-plotting.
wrtmat(mat, fn)={
  my(xz=#mat[1,],yz=#mat[,1],ws,d=0);
  for(i=1,yz, for(j=1,xz, if(mat[i,j]==0, next, d++; ws=Str(i," ",j); write(fn,ws))));
  print(" *** matrix(",xz,"x",yz,") ",d, " DOTS put in ",fn);
}
\\ plotff(): Plotting from a file written by the wrtmat().
\\ Saving possibly huge generation time if re-plotting needed.
plotff(fn)={
  my(F,nf,vx=List(),vy=vx,Vr);
  F=readstr(fn); nf=#F;
  print(" *** Plotting from: ", fn, " - ", nf, " DOTS");
  for(i=1,nf, Vr=stok(F[i],","); listput(vx,eval(Vr[1])); listput(vy,eval(Vr[2])));
  plothraw(Vec(vx),Vec(vy));
}

```


### Version #1. Translated from AutoHotkey.

Translated from AutoHotkey
[[File:BTAH1.png|right|thumb|Output BTAH1.png]]


```parigp

\\ Brownian tree v.#1. Translated from AutoHotkey
\\ 3/8/2016, upgraded 11/27/16 aev
\\ Where: size - size of a square matrix; lim - limit of testing dots;
\\        fn - file name (fn=""-only plot, fn!=""-only writing file)..
BrownianTree1(size,lim, fn="")={
  my(Myx=matrix(size,size),sz=size-1,sz2=sz\2,x,y,ox,oy);
  x=sz2; y=sz2; Myx[y,x]=1;  \\ seed in center
  print(" *** BT1 SEED: ",x,"/",y);
  for(i=1,lim,
    x=random(sz)+1; y=random(sz)+1;
    while(1,
      ox=x; oy=y;
      x+=random(3)-1; y+=random(3)-1;
      if(insm(Myx,x,y)&&Myx[y,x],
         if(insm(Myx,ox,oy), Myx[oy,ox]=1; break));
      if(!insm(Myx,x,y), break);
    );\\wend
  );\\ fend i
  if(fn=="", plotmat(Myx), wrtmat(Myx, fn));
}
\\ Executing 1 or 2 lines below:

BrownianTree1(400,15000);     \\BTAH1.png

{BrownianTree1(400,15000,"c:\\pariData\\BTAH1.dat");
plotff("c:\\pariData\\BTAH1.dat");}   \\BTAH1.png

```


Output:

```txt

> BrownianTree1(400,15000);     \\BTAH1.png
 *** BT1 SEED: 199/199
 *** matrix(400x400) 3723 DOTS
  ***   last result computed in 25min, 53,141 ms.

 *** BT1 SEED: 199/199
 *** matrix(400x400) 3723 DOTS put in c:\pariData\BTAH1.dat
 *** Plotting from: c:\pariData\BTAH1.dat - 3723 DOTS

```



### Version #2. Translated from Octave.

Translated from Octave
Octave: Translated from C
[[File:BTOC1.png|right|thumb|Output BTOC1.png]]


```parigp

\\ Brownian tree v.#2. Translated from Octave
\\ 3/8/2016, upgraded 11/27/16 aev
\\ Where: size - size of a square matrix; lim - limit of testing dots;
\\        fn - file name (fn=""-only plot, fn!=""-only writing file)..
BrownianTree2(size,lim, fn="")={
  my(Myx=matrix(size,size),sz=size-1,dx,dy,x,y);
  x=random(sz); y=random(sz); Myx[y,x]=1; \\ random seed
  print(" *** BT2 SEED: ",x,"/",y);
  for(i=1,lim,
    x=random(sz)+1; y=random(sz)+1;
    while(1,
      dx=random(3)-1; dy=random(3)-1;
      if(!insm(Myx,x+dx,y+dy), x=random(sz)+1; y=random(sz)+1,
         if(Myx[y+dy,x+dx], Myx[y,x]=1; break, x+=dx; y+=dy));
    );\\wend
  );\\fend i
  if(fn=="", plotmat(Myx), wrtmat(Myx, fn));
}
\\ Executing 1 or 2 lines below:

BrownianTree2(1000,3000);     \\BTOC1.png

{BrownianTree2(1000,3000,"c:\\pariData\\BTOC1.dat");
plotff("c:\\pariData\\BTOC1.dat");}   \\BTOC1.png

```


Output:

```txt

> BrownianTree2(1000,3000);     \\BTOC1.png
 *** BT2 SEED: 697/753
 *** matrix(1000x1000) 2984 DOTS
  ***   last result computed in 4h, 35min, 24,781 ms.

 *** BT2 SEED: 434/407
 *** matrix(1000x1000) 2981 DOTS put in c:\pariData\BTOC1.dat
 *** Plotting from: c:\pariData\BTOC1.dat - 2981 DOTS

```



### Version #3. Translated from Seed7.

Translated from Seed7
[[File:BTSE1.png|right|thumb|Output BTSE1.png]]


```parigp

\\ Brownian tree v.#3. Translated from Seed7
\\ 3/8/2016, upgraded 11/27/16 aev
\\ Where: size - size of a square matrix; lim - limit of testing dots;
\\        fn - file name (fn=""-only plot, fn!=""-only writing file)..
BrownianTree3(size,lim, fn="")={
  my(Myx=matrix(size,size),sz=size-2,x,y,dx,dy,b=0);
  x=random(sz); y=random(sz); Myx[y,x]=1; \\ random seed
  print("*** BT3 SEED: ", x,"/",y);
  for(i=1,lim,
    x=random(sz); y=random(sz);
    b=0; \\ bumped not
    while(!b,
       dx=random(3)-1; dy=random(3)-1;
       if(!insm(Myx,x+dx,y+dy), x=random(sz); y=random(sz),
          if(Myx[y+dy,x+dx]==1, Myx[y,x]=1; b=1, x+=dx; y+=dy);
         );
    );\\wend
  );\\fend i
  if(fn=="", plotmat(Myx), wrtmat(Myx, fn));
}
\\ Executing 1 or 2 lines below:

BrownianTree3(400,5000);     \\BTSE1.png

{BrownianTree3(400,5000,"c:\\pariData\\BTSE1.dat");
plotff("c:\\pariData\\BTSE1.dat");}   \\BTSE1.png

```


Output:

```txt

> BrownianTree3(400,5000);     \\BTSE1.png
 *** BT3 SEED: 367/60
 *** matrix(400x400) 4797 DOTS
  ***   last result computed in 57min, 57,375 ms.

 *** BT3 SEED: 46/293
 *** matrix(400x400) 4841 DOTS put in c:\pariData\BTSE1.dat
 *** Plotting from: c:\pariData\BTSE1.dat - 4841 DOTS

```



### Version #4. Translated from PureBasic.

Translated from PureBasic
[[File:BTPB1.png|right|thumb|Output BTPB1.png]]
[[File:BTPB2.png|right|thumb|Output BTPB2.png]]
[[File:BTPB3.png|right|thumb|Output BTPB3.png]]


```parigp

\\ Brownian tree v.#4. Translated from PureBasic
\\ 3/8/2016, upgraded 11/27/16 aev
\\ Where: size - size of a square matrix; lim - limit of testing dots;
\\        fn - file name (fn=""-only plot, fn!=""-only writing file)..
\\ s=1/2(random seed/seed in the center); p=0..n (level of the "deep" checking).
BrownianTree4(size,lim, fn="",s=1,p=0)={
  my(Myx=matrix(size,size),sz=size-3,x,y);
  \\ seed s=1 for BTPB1, s=2 for BTPB2, BTPB3
  if(s==1,x=random(sz); y=random(sz), x=sz\2; y=sz\2); Myx[y,x]=1;
  print(" *** BT4 SEED: ",x,"/",y);
  for(i=1,lim,
    if(!(i==1&&s==2), x=random(sz)+1; y=random(sz)+1);
    while(insm(Myx,x,y,1)&&
        (Myx[y+1,x+1]+Myx[y+1,x]+Myx[y+1,x-1]+Myx[y,x+1]+
         Myx[y-1,x-1]+Myx[y,x-1]+Myx[y-1,x]+Myx[y-1,x+1])==0,
      x+=random(3)-1; y+=random(3)-1;
      \\ p=0 for BTPB1, BTPB2; p=5 for BTPB3
      if(!insm(Myx,x,y,p), x=random(sz)+1; y=random(sz)+1;);
    );\\wend
    Myx[y,x]=1;
  );\\fend i
  if(fn=="", plotmat(Myx), wrtmat(Myx, fn));
}
\\ Executing 1 or 2 lines below:

BrownianTree4(200,4000);        \\BTPB1.png

{BrownianTree4(200,4000,"c:\\pariData\\BTPB1.dat");
plotff("c:\\pariData\\BTPB1.dat");}   \\BTPB1.png

BrownianTree4(200,4000,,2);     \\BTPB2.png

{BrownianTree4(200,4000,"c:\\pariData\\BTPB2.dat",2);
plotff("c:\\pariData\\BTPB2.dat");}   \\BTPB2.png

BrownianTree4(200,4000,,2,5);   \\BTPB3.png

{BrownianTree4(200,4000,"c:\\pariData\\BTPB3.dat",2,5);
plotff("c:\\pariData\\BTPB3.dat");}   \\BTPB3.png

```


Output:

```txt

> BrownianTree4(200,4000);     \\BTPB1.png
 *** BT4 SEED: 133/133
 *** matrix(200x200) 3813 DOTS
  ***   last result computed in 49,923 ms.

 *** BT4 SEED: 184/104
 *** matrix(200x200) 3805 DOTS put in c:\pariData\BTPB1.dat
 *** Plotting from: c:\pariData\BTPB1.dat - 3805 DOTS

> BrownianTree4(200,4000,,2);    \\BTPB2.png
 *** BT4 SEED: 98/98
 *** matrix(200x200) 3820 DOTS
  ***   last result computed in 40,047 ms.

 *** BT4 SEED: 98/98
 *** matrix(200x200) 3814 DOTS put in c:\pariData\BTPB2.dat
 *** Plotting from: c:\pariData\BTPB2.dat - 3814 DOTS

> BrownianTree4(200,4000,,2,5);  \\BTPB3.png
 *** BT4 SEED: 98/98
 *** matrix(200x200) 3622 DOTS
  ***   last result computed in 1min, 16,390 ms.

 *** BT4 SEED: 98/98
 *** matrix(200x200) 3641 DOTS put in c:\pariData\BTPB3.dat
 *** Plotting from: c:\pariData\BTPB3.dat - 3641 DOTS

```



## Perl 5

[[File:brownian-00.png|thumb]][[File:brownian-05.png|thumb]][[File:brownian-11.png|thumb]]
Simulation code. Showing three sample images with different STEP and ATTRACT parameters, to demonstrate how sensitive the result is to them.

Code runs until the tree reached specified radius.  Output is written to "test.eps" of wherever the current directory is.

```perl
sub PI() { atan2(1,1) * 4 }     # The, er, pi
sub STEP() { .5 }               # How far does the particle move each step. Affects
                                #       both speed and accuracy greatly
sub STOP_RADIUS() { 100 }       # When the tree reaches this far from center, end

# At each step, move this much towards center.  Bigger numbers help the speed because
# particles are less likely to wander off, but greatly affects tree shape.
# Should be between 0 and 1 ish.  Set to 0 for pain.
sub ATTRACT() { .2 }

my @particles = map([ map([], 0 .. 2 * STOP_RADIUS) ],  0 .. 2 * STOP_RADIUS);
push @{ $particles[STOP_RADIUS][STOP_RADIUS] }, [0, 0];

my $r_start = 3;
my $max_dist = 0;

sub dist2 {
        my ($dx, $dy) = ($_[0][0] - $_[1][0], $_[0][1] - $_[1][1]);
        $dx * $dx + $dy * $dy
}

sub move {
        my $p = shift;
        # moved too far, kill particle
        # return if dist2($p, [0, 0]) > 2 * $r_start * $r_start;
        $p->[0] += 2 * $r_start while $p->[0] < -$r_start;
        $p->[0] -= 2 * $r_start while $p->[0] >  $r_start;
        $p->[1] += 2 * $r_start while $p->[1] < -$r_start;
        $p->[1] -= 2 * $r_start while $p->[1] >  $r_start;

        my ($ix, $iy) = (int($p->[0]), int($p->[1]));
        my $dist = 2 * $r_start * $r_start;
        my $nearest;

        # see if the particle is close enough to stick to an exist one
        for ($ix - 1 .. $ix + 1) {
                my $idx = STOP_RADIUS + $_;
                next if $idx > 2 * STOP_RADIUS || $idx < 0;
                my $xs = $particles[ $idx ];
                for ($iy - 1 .. $iy + 1) {
                        my $idx = STOP_RADIUS + $_;
                        next if $idx > 2 * STOP_RADIUS || $idx < 0;
                        for (@{ $xs->[ $idx ] }) {
                                my $d = dist2($p, $_);
                                next if $d > 2;
                                next if $d > $dist;

                                $dist = $d;
                                $nearest = $_;
                        }
                }
        }

        # yes, found one
        if ($nearest) {
                my $displace = [ $p->[0] - $nearest->[0],
                                 $p->[1] - $nearest->[1] ];
                my $angle = atan2($displace->[1], $displace->[0]);
                $p->[0] = $nearest->[0] + cos($angle);
                $p->[1] = $nearest->[1] + sin($angle);

                push @{$particles[$ix + STOP_RADIUS][$iy + STOP_RADIUS]}, [ @$p ];
                $dist = sqrt dist2($p);

                if ($dist + 10 > $r_start && $r_start < STOP_RADIUS + 10) {
                        $r_start = $dist + 10
                }
                if (int($dist + 1) > $max_dist) {
                        $max_dist = int($dist + 1);
                        # write_eps();
                        # system('pstopnm -portrait -xborder 0 -yborder 0 test.eps 2> /dev/null');
                        # system('pnmtopng test.eps001.ppm 2>/dev/null > test.png');
                        return 3 if $max_dist >= STOP_RADIUS;
                }
                return 2;
        }

        # random walk
        my $angle = rand(2 * PI);
        $p->[0] += STEP * cos($angle);
        $p->[1] += STEP * sin($angle);

        # drag particle towards center by some distance
        my $nudge;
        if (sqrt(dist2($p, [0, 0])) > STOP_RADIUS + 1) {
                $nudge = 1;
        } else {
                $nudge = STEP * ATTRACT;
        }

        if ($nudge) {
                $angle = atan2($p->[1], $p->[0]);
                $p->[0] -= $nudge * cos($angle);
                $p->[1] -= $nudge * sin($angle);
        }

        return 1;
}

my $count;
PARTICLE: while (1) {
        my $a = rand(2 * PI);
        my $p = [ $r_start * cos($a), $r_start * sin($a) ];
        while (my $m = move($p)) {
                if    ($m == 1) { next }
                elsif ($m == 2) { $count++; last; }
                elsif ($m == 3) { last PARTICLE }
                else            { last }
        }
        print STDERR "$count $max_dist/@{[int($r_start)]}/@{[STOP_RADIUS]}\r" unless $count% 7;
}

sub write_eps {
        my $size = 128;
        my $p = $size / (STOP_RADIUS * 1.05);
        my $b = STOP_RADIUS * $p;
        if ($p < 1) {
                $size = STOP_RADIUS * 1.05;
                $b = STOP_RADIUS;
                $p = 1;
        }

        my $hp = $p / 2;

        open OUT, ">", "test.eps";

        # print EPS to standard out
        print OUT <<"HEAD";
%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 0 0 @{[$size*2, $size*2]}
$size $size translate
/l{ rlineto }def
/c{ $hp 0 360 arc fill }def
-$size -$size moveto
$size 2 mul 0 l
0 $size 2 mul l
-$size 2 mul 0 l
closepath
0 setgray fill
0 setlinewidth .1 setgray 0 0 $b 0 360 arc stroke
.8 setgray /TimesRoman findfont 16 scalefont setfont
-$size 10 add $size -16 add moveto
(Step = @{[STEP]}  Attract = @{[ATTRACT]}) show
0 1 0 setrgbcolor newpath
HEAD

        for (@particles) {
                for (@$_) {
                        printf OUT "%.3g %.3g c ", map { $_ * $p } @$_ for @$_;
                }
        }
        print OUT "\n%%EOF";
        close OUT;
}

write_eps;
```



## Perl 6


[[File:Brownian_tree_perl6.png|thumb]]

This solution spawns new Particles at a growing square border and displays the Tree every 50 particles and at the end using unicode UPPER/LOWER HALF BLOCK and FULL BLOCK.

Works with Rakudo|2015.12


```perl6
constant size = 100;
constant particlenum = 1_000;


constant mid = size div 2;

my $spawnradius = 5;
my @map;

sub set($x, $y) {
    @map[$x][$y] = True;
}

sub get($x, $y) {
    return @map[$x][$y] || False;
}

set(mid, mid);
my @blocks = " ","\c[UPPER HALF BLOCK]", "\c[LOWER HALF BLOCK]","\c[FULL BLOCK]";

sub infix:<█>($a, $b) {
    @blocks[$a + 2 * $b]
}

sub display {
    my $start = 0;
    my $end = size;
    say (for $start, $start + 2 ... $end -> $y {
        (for $start..$end -> $x {
            if abs(($x&$y) - mid) < $spawnradius {
                get($x, $y) █ get($x, $y+1);
            } else {
                " "
            }
        }).join
    }).join("\n")
}

for ^particlenum -> $progress {
    my Int $x;
    my Int $y;
    my &reset = {
        repeat {
            ($x, $y) = (mid - $spawnradius..mid + $spawnradius).pick, (mid - $spawnradius, mid + $spawnradius).pick;
            ($x, $y) = ($y, $x) if (True, False).pick();
        } while get($x,$y);
    }
    reset;

    while not get($x-1|$x|$x+1, $y-1|$y|$y+1) {
        $x = ($x-1, $x, $x+1).pick;
        $y = ($y-1, $y, $y+1).pick;
        if (False xx 3, True).pick {
            $x = $x >= mid ?? $x - 1 !! $x + 1;
            $y = $y >= mid ?? $y - 1 !! $y + 1;
        }
        if abs(($x | $y) - mid) > $spawnradius {
            reset;
        }
    }
    set($x,$y);
    if $spawnradius < mid && abs(($x|$y) - mid) > $spawnradius - 5 {
        $spawnradius = $spawnradius + 1;
    }
}

display;
```



## Phix

As-is, runs in about 2s, but can be very slow when bigger or (even worse) resize-able.
{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\BrownianTree.exw
--
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
integer x,y,ox,oy
integer {width, height} = IupGetIntInt(canvas, "DRAWSIZE")
sequence grid = repeat(repeat(0,width),height)
integer xy = floor(width*height*0.8)
--atom t = time()+1
    grid[floor(width/2)][floor(height/2)] = 1
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    for i=1 to xy do
        x = rand(width) y = rand(height)
        ox = x          oy = y
        while x>=1 and x<=width
          and y>=1 and y<=height do
            if grid[y][x] then
                grid[oy][ox] = 1
                cdCanvasPixel(cddbuffer, ox, oy, #00FF00)
                exit
            end if
            ox = x  x += rand(3)-2
            oy = y  y += rand(3)-2
        end while
--      -- if making the canvas bigger/resizeable,
--      --  put this in so that you can kill it.
--      if time()>=t then
--          ?{i,xy}
--          t = time()+1
--      end if
    end for
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    cdCanvasSetForeground(cddbuffer, CD_RED)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "200x200") -- fixed size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas, "RESIZE=NO")
    IupSetAttribute(dlg, "TITLE", "Brownian Tree")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    IupMap(dlg)
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")

(de brownianTree (File Size Cnt)
   (let Img (grid Size Size)
      (put Img (/ Size 2) (/ Size 2) 'pix T)
      (use (P Q)
         (do Cnt
            (setq P (get Img (rand 1 Size) (rand 1 Size)))
            (loop
               (setq Q ((if2 (rand T) (rand T) north east south west) P))
               (T (; Q pix) (put P 'pix T))
               (setq P (or Q (get Img (rand 1 Size) (rand 1 Size)))) ) ) )
      (out "img.pbm"
         (prinl "P1")
         (prinl Size " " Size)
         (for L Img
            (for This L
               (prin (if (: pix) 1 0)) )
            (prinl) ) ) ) )
```

Use:

```txt
(brownianTree "img.pbm" 300 9000)
(call 'display "img.pbm")
```



## Processing


```java
boolean SIDESTICK = false;
boolean[][] isTaken;

void setup() {
  size(512, 512);
  isTaken = new boolean[width][height];
  isTaken[width/2][height/2] = true;
}

void draw() {
  for (int i = 0; i < width*height; i++) {
    int x = floor(random(width));
    int y = floor(random(height));
    if (isTaken[x][y]) { continue; }
    while (true) {
      int xp = x + floor(random(-1, 2));
      int yp = y + floor(random(-1, 2));
      boolean iscontained = (
        0 <= xp && xp < width  &&
        0 <= yp && yp < height
      );
      if (iscontained && !isTaken[xp][yp]) {
        x = xp;
        y = yp;
        continue;
      }
      else {
        if (SIDESTICK || (iscontained && isTaken[xp][yp])) {
          isTaken[x][y] = true;
          set(x, y, #000000);
        }
        break;
      }
    }
  }
  noLoop();
}
```



## PureBasic


```PureBasic
#Window1   = 0
#Image1    = 0
#ImgGadget = 0

#NUM_PARTICLES = 3000
#width         = 200
#height        = 200
#xmax          = #width  -3
#ymax          = #height -3
Define.i Event ,i ,x,y

If OpenWindow(#Window1, 0, 0, #width, #height, "Brownian Tree PureBasic Example", #PB_Window_SystemMenu )
   If CreateImage(#Image1, #width, #height)
      ImageGadget(#ImgGadget, 0, 0, #width, #height, ImageID(#Image1))
      StartDrawing(ImageOutput(#Image1))
      FrontColor($FFFFFF)
      Plot( Random(#xmax) , Random(#ymax ))
      StopDrawing()
      SetGadgetState(#ImgGadget, ImageID(#Image1))
      For i = 1 To #NUM_PARTICLES
          x = Random(#xmax)+1 : y = Random (#ymax)+1
          StartDrawing(ImageOutput(#Image1))
          While Point(x+1, y+1) + Point(x, y+1)+Point(x+1, y)+Point(x-1, y-1)+Point(x-1, y)+Point(x, y-1) = 0
              x = x + (Random(2)-1) : y = y + (Random(2)-1)
              If x < 1 Or x > #xmax Or y < 1 Or y > #ymax
                  x = Random(#xmax)+1 : y = Random (#ymax)+1
              EndIf
          Wend
          Plot(x,y)
          StopDrawing()
          SetGadgetState(#ImgGadget, ImageID(#Image1))
      Next

   EndIf

    Repeat
      Event = WaitWindowEvent()
    Until Event = #PB_Event_CloseWindow
EndIf
```
[[File:BrownianTree.pb.png]]



## Python

```python
import pygame, sys, os
from pygame.locals import *
from random import randint
pygame.init()

MAXSPEED = 15
SIZE = 3
COLOR = (45, 90, 45)
WINDOWSIZE = 400
TIMETICK = 1
MAXPART = 50

freeParticles = pygame.sprite.Group()
tree = pygame.sprite.Group()

window = pygame.display.set_mode((WINDOWSIZE, WINDOWSIZE))
pygame.display.set_caption("Brownian Tree")

screen = pygame.display.get_surface()


class Particle(pygame.sprite.Sprite):
    def __init__(self, vector, location, surface):
        pygame.sprite.Sprite.__init__(self)
        self.vector = vector
        self.surface = surface
        self.accelerate(vector)
        self.add(freeParticles)
        self.rect = pygame.Rect(location[0], location[1], SIZE, SIZE)
        self.surface.fill(COLOR, self.rect)

    def onEdge(self):
        if self.rect.left <= 0:
            self.vector = (abs(self.vector[0]), self.vector[1])
        elif self.rect.top <= 0:
            self.vector = (self.vector[0], abs(self.vector[1]))
        elif self.rect.right >= WINDOWSIZE:
            self.vector = (-abs(self.vector[0]), self.vector[1])
        elif self.rect.bottom >= WINDOWSIZE:
            self.vector = (self.vector[0], -abs(self.vector[1]))

    def update(self):
        if freeParticles in self.groups():
            self.surface.fill((0,0,0), self.rect)
            self.remove(freeParticles)
            if pygame.sprite.spritecollideany(self, freeParticles):
                self.accelerate((randint(-MAXSPEED, MAXSPEED),
                                 randint(-MAXSPEED, MAXSPEED)))
                self.add(freeParticles)
            elif pygame.sprite.spritecollideany(self, tree):
                self.stop()
            else:
                self.add(freeParticles)

            self.onEdge()

            if (self.vector == (0,0)) and tree not in self.groups():
                self.accelerate((randint(-MAXSPEED, MAXSPEED),
                                 randint(-MAXSPEED, MAXSPEED)))
            self.rect.move_ip(self.vector[0], self.vector[1])
        self.surface.fill(COLOR, self.rect)

    def stop(self):
        self.vector = (0,0)
        self.remove(freeParticles)
        self.add(tree)

    def accelerate(self, vector):
        self.vector = vector

NEW = USEREVENT + 1
TICK = USEREVENT + 2

pygame.time.set_timer(NEW, 50)
pygame.time.set_timer(TICK, TIMETICK)


def input(events):
    for event in events:
        if event.type == QUIT:
            sys.exit(0)
        elif event.type == NEW and (len(freeParticles) < MAXPART):
            Particle((randint(-MAXSPEED,MAXSPEED),
                      randint(-MAXSPEED,MAXSPEED)),
                     (randint(0, WINDOWSIZE), randint(0, WINDOWSIZE)),
                     screen)
        elif event.type == TICK:
            freeParticles.update()


half = WINDOWSIZE/2
tenth = WINDOWSIZE/10

root = Particle((0,0),
                (randint(half-tenth, half+tenth),
                 randint(half-tenth, half+tenth)), screen)
root.stop()

while True:
    input(pygame.event.get())
    pygame.display.flip()
```



## R

All versions #1 - #4 are based on using 2 small plotting helper functions, which are allowing to unify
all gpBrownianTreeX() functions and make them shorter.

;Note:
* All pictures are ready to be uploaded when it would be allowed again.
Translated from PARI/GP
Works with R|3.3.1 and above
[[File:BT1R.png|right|thumb|Output BT1R.png]]
[[File:BT2R.png|right|thumb|Output BT2R.png]]
[[File:BT2aR.png|right|thumb|Output BT2aR.png]]
[[File:BT3R.png|right|thumb|Output BT3R.png]]
[[File:BT4R.png|right|thumb|Output BT4R.png]]

### Plotting helper functions

;Note:
* All plotting helper functions are using a square matrix mat or 2 vectors X,Y from the dump file created by plotmat()
* The file names used are without extension (which will be added as ".png", ".dmp" and ".dat" when needed).
* Requesting dump file is useful if the generating/plotting time
is big. Having a dump file makes it easy and fast to repeat plotting
with different colors, titles, etc.
* If number of generated dots is very big then plotting from a dump
file could be very slow too. Actually, plotv2() shows almost "pure" plotting time.


```r

# plotmat(): Simple plotting using a square matrix mat (filled with 0/1). v. 8/31/16
# Where: mat - matrix; fn - file name; clr - color; ttl - plot title;
#        dflg - writing dump file flag (0-no/1-yes): psz - picture size.
plotmat <- function(mat, fn, clr, ttl, dflg=0, psz=600) {
  m <- nrow(mat); d <- 0;
  X=NULL; Y=NULL;
  pf = paste0(fn, ".png"); df = paste0(fn, ".dmp");
  for (i in 1:m) {
    for (j in 1:m) {if(mat[i,j]==0){next} else {d=d+1; X[d] <- i; Y[d] <- j;} }
  };
  cat(" *** Matrix(", m,"x",m,")", d, "DOTS\n");
  # Dumping if requested (dflg=1).
  if (dflg==1) {dump(c("X","Y"), df); cat(" *** Dump file:", df, "\n")};
  # Plotting
  plot(X,Y, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, pch=20);
  dev.copy(png, filename=pf, width=psz, height=psz);
  # Cleaning
  dev.off(); graphics.off();
}

# plotv2(): Simple plotting using 2 vectors (dumped into ".dmp" file by plotmat()).
# Where: fn - file name; clr - color; ttl - plot title; psz - picture size.
# v. 8/31/16
plotv2 <- function(fn, clr, ttl, psz=600) {
  cat(" *** START:", date(), "clr=", clr, "psz=", psz, "\n");
  cat(" *** File name -", fn, "\n");
  pf = paste0(fn, ".png"); df = paste0(fn, ".dmp");
  source(df);
  d <- length(X);
  cat(" *** Source dump-file:", df, d, "DOTS\n");
  cat(" *** Plot file -", pf, "\n");
  # Plotting
  plot(X, Y, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, pch=20);
  # Writing png-file
  dev.copy(png, filename=pf, width=psz, height=psz);
  # Cleaning
  dev.off(); graphics.off();
  cat(" *** END:", date(), "\n");
}

```


Versions #1- #4:

All functions are translated from [[Brownian_tree#PARI.2FGP| PARI/GP]].
;Note:
* All generating functions are using a square matrix mat to fill it with 0/1.


### Version #1


```r

# Generate and plot Brownian tree. Version #1.
# 7/27/16 aev
# gpBrownianTree1(m, n, clr, fn, ttl, dflg, psz)
# Where: m - defines matrix m x m; n - limit of the number of moves;
#   fn - file name (.ext will be added); ttl - plot title; dflg - 0-no dump,
#   1-dump: psz - picture size.
gpBrownianTree1 <- function(m, n, clr, fn, ttl, dflg=0, psz=600)
{
  cat(" *** START:", date(), "m=",m, "n=",n, "clr=",clr, "psz=", psz, "\n");
  M <- matrix(c(0), ncol=m, nrow=m, byrow=TRUE);
  # Seed in center
  x <- m%/%2; y <- m%/%2;
  M[x,y]=1;
  pf=paste0(fn, ".png");
  cat(" *** Plot file -", pf, "\n");
  # Main loops
  for (i in 1:n) {
    if(i>1) {
      x <- sample(1:m, 1, replace=FALSE)
      y <- sample(1:m, 1, replace=FALSE)}
    while(1) {
      ox = x; oy = y;
      x <- x + sample(-1:1, 1, replace=FALSE);
      y <- y + sample(-1:1, 1, replace=FALSE);
      if(x<=m && y<=m && x>0 && y>0 && M[x,y])
        {if(ox<=m && oy<=m && ox>0 && oy>0) {M[ox,oy]=1; break}}
      if(!(x<=m && y<=m && x>0 && y>0)) {break}
    }
  }
  plotmat(M, fn, clr, ttl, dflg, psz);
  cat(" *** END:",date(),"\n");
}
gpBrownianTree1(400,15000,"red", "BT1R", "Brownian Tree v.1", 1);

```


Output:

```txt

> gpBrownianTree1(400,15000,"red", "BT1R", "Brownian Tree v.1", 1);
 *** START: Mon Sep 05 13:07:27 2016 m= 400 n= 15000 clr= red psz= 600
 *** Plot file - BT1R.png
 *** Matrix( 400 x 400 ) 3639 DOTS
 *** Dump file: BT1R.dmp
 *** END: Mon Sep 05 14:06:55 2016

```



### Version #2


```r

# Generate and plot Brownian tree. Version #2.
# 7/27/16 aev
# gpBrownianTree2(m, n, clr, fn, ttl, dflg, psz)
# Where: m - defines matrix m x m; n - limit of the number of moves;
#   fn - file name (.ext will be added); ttl - plot title; dflg - 0-no dump,
#   1-dump; psz - picture size.
gpBrownianTree2 <- function(m, n, clr, fn, ttl, dflg=0, psz=600)
{
  cat(" *** START:", date(), "m=",m, "n=",n, "clr=",clr, "psz=", psz, "\n");
  M <- matrix(c(0), ncol=m, nrow=m, byrow=TRUE);
  # Random seed always
  x <- sample(1:m, 1, replace=FALSE); y <- sample(1:m, 1, replace=FALSE);
  M[x,y]=1;
  pf=paste0(fn,".png");
  cat(" *** Plot file -",pf,"Seed:",x,"/",y,"\n");
  # Main loops
  for (i in 1:n) {
    if(i>1) {
      x <- sample(1:m, 1, replace=FALSE)
      y <- sample(1:m, 1, replace=FALSE)}
    while(1) {
      dx <- sample(-1:1, 1, replace=FALSE);
      dy <- sample(-1:1, 1, replace=FALSE);
      nx=x+dx; ny=y+dy;
      if(!(nx<=m && ny<=m && nx>0 && ny>0)) {
        x <- sample(1:m, 1, replace=FALSE); y <- sample(1:m, 1, replace=FALSE)}
      else {if(M[nx,ny]) {M[x,y]=1; break}
        else{x=nx; y=ny;}}
    }
  }
  plotmat(M, fn, clr, ttl, dflg, psz);
  cat(" *** END:",date(),"\n");
}
gpBrownianTree2(400,5000,"brown", "BT2R", "Brownian Tree v.2", 1);

## Rename BT2R.dmp to BT2aR.dmp
plotv2("BT2aR", "orange", "Brownian Tree v.2a", 640)

```


Output:


```txt

> gpBrownianTree2(400,5000,"brown", "BT2R", "Brownian Tree v.2", 1);
 *** START: Mon Sep 05 20:11:02 2016 m= 400 n= 5000 clr= brown psz= 600
 *** Plot file - BT2R.png Seed: 371 / 135
 *** Matrix( 400 x 400 ) 4824 DOTS
 *** Dump file: BT2R.dmp
 *** END: Mon Sep 05 22:32:09 2016

> plotv2("BT2aR", "orange", "Brownian Tree v.2a", 640)
 *** START: Mon Sep 05 22:21:26 2017 clr= orange psz= 640
 *** File name - BT2aR
 *** Source dump-file: BT2aR.dmp 4824 DOTS
 *** Plot file - BT2aR.png
 *** END: Mon Sep 05 22:21:27 2017

```



### Version #3


```r

# Generate and plot Brownian tree. Version #3.
# 7/27/16 aev
# gpBrownianTree3(m, n, clr, fn, ttl, dflg, seed, psz):
# Where: m - defines matrix m x m; n - limit of the number of moves;
#   fn - file name (.ext will be added); ttl - plot title; dflg - 0-no dump,
#   1-dump; seed - 0-center, 1-random: psz - picture size.
gpBrownianTree3 <- function(m, n, clr, fn, ttl, dflg=0, seed=0, psz=600)
{
  cat(" *** START:", date(),"m=",m,"n=",n,"clr=",clr, "psz=",psz, "\n");
  M <- matrix(c(0), ncol=m, nrow=m, byrow=TRUE);
  # Random seed
  if(seed==1)
    {x <- sample(1:m, 1, replace=FALSE);y <- sample(1:m, 1, replace=FALSE)}
  # Seed in center
  else {x <- m%/%2; y <- m%/%2}
  M[x,y]=1;
  pf=paste0(fn,". png");
  cat(" *** Plot file -", pf, "Seed:",x,"/",y, "\n");
  # Main loops
  for (i in 1:n) {
    if(i>1) {
      x <- sample(1:m, 1, replace=FALSE)
      y <- sample(1:m, 1, replace=FALSE)}
    b <- 0;
    while(b==0) {
      dx <- sample(-1:1, 1, replace=FALSE)
      dy <- sample(-1:1, 1, replace=FALSE)
      if(!(x+dx<=m && y+dy<=m && x+dx>0 && y+dy>0))
        { x <- sample(1:m, 1, replace=FALSE)
          y <- sample(1:m, 1, replace=FALSE)
        }
      else{if(M[x+dx,y+dy]==1) {M[x,y]=1; b=1}
        else {x=x+dx; y=y+dy;} }
    }
  }
  plotmat(M, fn, clr, ttl, dflg, psz);
  cat(" *** END:", date(), "\n");
}
gpBrownianTree3(400,5000,"dark green", "BT3R", "Brownian Tree v.3", 1);

```


Output:


```txt

> gpBrownianTree3(400,5000,"dark green", "BT3R", "Brownian Tree v.3", 1);
 *** START: Mon Sep 05 10:06:18 2016 m= 400 n= 5000 clr= dark green psz= 600
 *** Plot file - BT3R. png Seed: 200 / 200
 *** Matrix( 400 x 400 ) 4880 DOTS
 *** Dump file: BT3R.dmp
 *** END: Mon Sep 05 11:21:54 2016

```



### Version #4


```r

# Generate and plot Brownian tree. Version #4.
# 7/27/16 aev
# gpBrownianTree4(m, n, clr, fn, ttl, dflg, seed, psz)
# Where: m - defines matrix m x m; n - limit of the number of moves;
#   fn - file name (.ext will be added); ttl - plot title; dflg - 0-no dump,
#   1-dump; seed - 0-center, 1-random: psz - picture size.
gpBrownianTree4 <- function(m, n, clr, fn, ttl, dflg=0, seed=0, psz=600)
{
  cat(" *** START:", date(), "m=",m, "n=",n, "clr=",clr, "psz=",psz, "\n");
  M <- matrix(c(0), ncol=m, nrow=m, byrow=TRUE);
  # Random seed
  if(seed==1)
    {x <- sample(1:m, 1, replace=FALSE);y <- sample(1:m, 1, replace=FALSE)}
  # Seed in center
  else {x <- m%/%2; y <- m%/%2}
  M[x,y]=1;
  pf=paste0(fn,".png");
  cat(" *** Plot file -",pf,"Seed:",x,"/",y,"\n");
  # Main loops
  for (i in 1:n) {
    if(i>1) {
      x <- sample(1:m, 1, replace=FALSE)
      y <- sample(1:m, 1, replace=FALSE)}
    while((x<=m && y<=m && x>0 && y>0)) {
      if(!(x+1<=m && y+1<=m && x-1>0 && y-1>0)) {break;}
      b=M[x+1,y+1]+M[x,y+1]+M[x-1,y+1]+M[x+1,y];
      b=b+M[x-1,y-1]+M[x-1,y]+M[x,y-1]+M[x+1,y-1];
      if(b!=0) {break;}
      x <- x + sample(-1:1, 1, replace=FALSE)
      y <- y + sample(-1:1, 1, replace=FALSE)
      if(!(x<=m && y<=m && x>0 && y>0))
        { x <- sample(1:m, 1, replace=FALSE)
          y <- sample(1:m, 1, replace=FALSE)
        }
    }
    M[x,y]=1;
  }
  plotmat(M, fn, clr, ttl, dflg, psz);
  cat(" *** END:",date(),"\n");
}
gpBrownianTree4(400,15000,"navy", "BT4R", "Brownian Tree v.4", 1);

```


Output:


```txt

> gpBrownianTree4(400,15000,"navy", "BT4R", "Brownian Tree v.4", 1);
 *** START: Mon Sep 05 11:12:39 2016 m= 400 n= 15000 clr= navy psz= 600
 *** Plot file - BT4R.png Seed: 200 / 200
 *** Matrix( 400 x 400 ) 14327 DOTS
 *** Dump file: BT4R.dmp
 *** END: Mon Sep 05 11:50:47 2016

```



## Racket


```racket
#lang racket
(require 2htdp/image)

;; The unsafe fixnum ops are faster than the checked ones,
;; but if you get anything wrong with them, they'll bite.
;; If you experience any problems reactivate the
;; (require racket/fixnum) and instead of the unsafe requirement
;; below...

;; we have tested this...
#;(require racket/fixnum)
;; so we can use this...
(require racket/require
           (only-in racket/fixnum make-fxvector in-fxvector)
           (filtered-in
            (? (name) (regexp-replace #rx"unsafe-" name ""))
            racket/unsafe/ops))

;;; This implementation uses a 1d, mutable, fixnum vector
;;; there's a lot of work done making the tree, so this optimisation
;;; at the expense of clarity has been made. Sorry, guys!
(define (brownian-tree w h collisions n-particles seed-tree
                       generate-particle walk-particle)
  (define w*h (fx* w h))
  (define V (make-fxvector w*h))
  (define (collision? x.y) (fx> (fxvector-ref V x.y) 0))

  ;; The main loop
  (define (inner-b-t collisions particles)
    (cond
      [(fx= 0 collisions) V]
      [else
       (define-values (new-particles new-collisions)
         (for/fold
             ((prtcls null)
              (clsns 0))
           ((x.y particles)
            #:break (fx= collisions clsns))
           (define new-particle (walk-particle x.y w h w*h))
           (cond
             [(not new-particle) ; it died!
              (values (cons (generate-particle V w h w*h) prtcls) clsns)]
             [(collision? new-particle)
              (fxvector-set! V x.y 1)
              (values (cons (generate-particle V w h w*h) prtcls) (add1 clsns))]
             [else
              (values (cons new-particle prtcls) clsns)])))
       (when (fx> new-collisions 0)
         (define remain (fx- collisions new-collisions))
         (unless (fx= (exact-floor (* 10 (log collisions)))
                      (exact-floor (* 10 (log (fxmax 1 remain)))))
           (eprintf "~a (e^~a)~%" remain (log (fxmax 1 remain))))
         (log-info "~a new collisions: ~a remain~%" new-collisions remain))
       (inner-b-t (fxmax 0 (fx- collisions new-collisions)) new-particles)]))

  ;; Seed the tree
  (seed-tree V w h)
  (inner-b-t collisions
             (build-list n-particles
                         (lambda (x) (generate-particle V w h w*h)))))

;; See below for why we do the (fxremainder ...) test
(define (uniform-particle-generator v w h w*h)
  (define x.y (random w*h))
  (define valid-x.y?
    (and
     (fx= (fxvector-ref v x.y) 0) ; start on empty cell
     (fx> (fxremainder x.y w) 0))) ; not on left edge
  ; if it's valid take it otherwise regenerate
  (if valid-x.y? x.y (uniform-particle-generator v w h w*h)))

;; The boundaries to the walker are to remain within the limits of
;; the vector... however, unless we stop particles going off the
;; east/west edges, the tree will be formed on a cylinder as the
;; particles wrap. So we kill particles that reach the left edge
;; either by decrement from the right or by incrementing and wrapping.
;; This is is tested with (= 0 (remainder x.y w)).
(define (brownian-particle-walker x.y w h w*h)
  (define dx (fx- (random 3) 1))
  (define dy (fx- (random 3) 1))
  (define new-x.y (fx+ x.y (fx+ dx (fx* w dy))))
  (and (fx> new-x.y 0) (fx< new-x.y w*h)
       (fx> (fxremainder new-x.y w) 0)
       new-x.y))

;; These seed functions modify v however you want!
(define (seed-middle v w h)
  (fxvector-set! v (+ (quotient w 2) (* w (quotient h 2))) 1))

(define (seed-circle v w h)
  (for ((a (in-range 0 360 120)))
    (define x (exact-floor (* w 1/8 (+ 4 (sin (* pi 1/180 a))))))
    (define y (exact-floor (* h 1/8 (+ 4 (cos (* pi 1/180 a))))))
    (fxvector-set! v (+ x (* w y)) 1)))

;; SCALE is a general purpose knob for modifying the size of the problem
;;       complexity increases with the sqaure of SCALE (at least)
(define SCALE 1)
(define tree-W (* SCALE 320))
(define tree-H (* SCALE 240))
(define tree-W.H (* tree-W tree-H))
;; play with tree-PARTICLES -- small values will lead to a smaller tree
;; as the tree moves towards the edges, more particles might affect its shape
(define tree-PARTICLES (quotient tree-W.H 4))
;; these are the particles that are bimbling around at any one time. If it's
;; too low, you might get bored waiting for a collision... if it's too high
;; you might get inappropriate collisions
(define working-PARTICLES (quotient tree-W.H 300))

(define b-t (time
             (brownian-tree
              tree-W tree-H tree-PARTICLES working-PARTICLES
              seed-middle
              uniform-particle-generator
              brownian-particle-walker)))

(define (b-t-value->color c) (case c ((1) "black") (else "white")))
(define img (color-list->bitmap
             (for*/list ((x (in-fxvector b-t)))
               (b-t-value->color x))
             tree-W tree-H))

img
(save-image img "brownian-tree.png")
```



## REXX

A large part of the REXX program's prologue was to handle the various options.


With a little more REXX code, a   ''petri dish''   option could be added, that is, when a particle hits the edge,

it "bounces" back.   Also, the field could then be displayed as a round area   (like a petri dish).

REXX code was added to display snapshots of the field, either after so many cycles, and/or after some

elapsed time has elapsed (whole seconds only).   This makes for some fascinating observations.

Program note:   to keep things simple, the (system) command to clear the screen was hard-coded as   '''CLS'''.

```rexx
/*REXX program animates and displays Brownian motion of dust in a field (with one seed).*/
mote     = '·'                                   /*character for a loose mote (of dust).*/
hole     = ' '                                   /*    "      "  an empty spot in field.*/
clearScr = 'CLS'                                 /*(DOS)  command to clear the screen.  */
eons     = 1000000                               /*number cycles for  Brownian movement.*/
snapshot = 0                                     /*every   N  winks, display a snapshot.*/
snaptime = 1                                     /*  "     "   secs,    "    "     "    */
seedPos  = 30 45                                 /*place a seed in this field position. */
seedPos  = 0                                     /*if =0,  then use middle of the field.*/
                                                 /* " -1,    "   "   a random placement.*/
                                                 /*otherwise, place the seed at seedPos.*/
                                                 /*use RANDSEED for RANDOM repeatability*/
parse arg height width motes tree randSeed .     /*obtain optional arguments from the CL*/
if height=='' | height==","  then height=0       /*Not specified?  Then use the default.*/
if  width=='' |  width==","  then  width=0       /* "      "         "   "   "      "   */
if  motes=='' |  motes==","  then  motes='10%'   /*The  %  dust motes in the field,     */
                                                 /* [↑]  either a #  -or-  a # with a %.*/
if   tree=='' |  tree==mote  then tree='*'       /*the character used to show the tree. */
if length(tree)==2           then tree=x2c(tree) /*tree character was specified in hex. */
if datatype(randSeed,'W')  then call random ,,randSeed    /*if an integer, use the seed.*/
                                                 /* [↑]  set the first  random  number. */
if height==0 | width==0 then _=scrsize()         /*Note: not all REXXes have SCRSIZE BIF*/
if height==0            then height=word(_, 1)-3 /*adjust useable height for the border.*/
if             width==0 then  width=word(_, 2)-1 /*   "      "    width   "   "     "   */
                     seedAt=seedPos
if seedPos== 0  then seedAt=width%2 height%2     /*if it's a zero,  start in the middle.*/
if seedPos==-1  then seedAt=random(1, width) random(1,height)  /*if negative, use random*/
parse  var  seedAt    xs  ys  .                  /*obtain the  X and Y  seed coördinates*/
                                                 /* [↓]  if right-most ≡ '%', then use %*/
if right(motes, 1) == '%'  then motes=height * width * strip(motes, , '%')   % 100
@.=hole                                          /*create the Brownian field, all empty.*/

  do j=1  for motes                              /*sprinkle a  # of dust motes randomly.*/
  rx=random(1, width);         ry=random(1, height);            @.rx.ry=mote
  end   /*j*/                                    /* [↑]  place a mote at random in field*/
                                                 /*plant the seed from which the tree   */
                                                 /*      will grow from dust motes that */
@.xs.ys=tree                                     /*      affixed themselves to others.  */
call show                                        /*show field before we mess it up again*/
tim=0                                            /*the time in seconds of last display. */
loX=1;  hiX= width                               /*used to optimize the  mote searching.*/
loY=1;  hiY=height                               /*  "   "     "      "    "      "     */
     /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ soooo, this is Brownian motion.*/
  do winks=1  for eons  until \motion            /*EONs is used instead of ∞; close 'nuf*/
  motion=0                                       /*turn off the  Brownian motion  flag. */
  if snapshot\==0  then  if winks//snapshot==0  then call show
  if snaptime\==0  then  do;   t=time('S')
                               if t\==tim & t//snaptime==0  then do;   tim=t;    call show
                                                                 end
                         end
  minX=loX;     maxX=hiX                         /*as the tree grows, the search for    */
  minY=loY;     maxY=hiY                         /*             dust motes gets faster. */
  loX= width;   hiX=1                            /*used to limit the mote searching.    */
  loY=height;   hiY=1                            /*  "   "   "    "    "      "         */

    do x  =minX  to maxX;    xm=x-1;        xp=x+1         /*a couple handy-dandy values*/
      do y=minY  to maxY;    if @.x.y\==mote  then iterate /*Not a mote:  keep looking. */
      if x<loX  then loX=x;  if x>hiX  then hiX=x          /*faster than: hiX=max(X,hiX)*/
      if y<loY  then loY=y;  if y>hiY  then hiY=y          /*faster than: hiY=max(y,hiY)*/
      if @.xm.y ==tree  then do; @.x.y=tree; iterate; end  /*there a neighbor of tree?  */
      if @.xp.y ==tree  then do; @.x.y=tree; iterate; end  /*there a neighbor of tree?  */
             ym=y-1
      if @.x.ym ==tree  then do; @.x.y=tree; iterate; end  /*there a neighbor of tree?  */
      if @.xm.ym==tree  then do; @.x.y=tree; iterate; end  /*there a neighbor of tree?  */
      if @.xp.ym==tree  then do; @.x.y=tree; iterate; end  /*there a neighbor of tree?  */
             yp=y+1
      if @.x.yp ==tree  then do; @.x.y=tree; iterate; end  /*there a neighbor of tree?  */
      if @.xm.yp==tree  then do; @.x.y=tree; iterate; end  /*there a neighbor of tree?  */
      if @.xp.yp==tree  then do; @.x.y=tree; iterate; end  /*there a neighbor of tree?  */
      motion=1                                   /* [↓]  Brownian motion is coming.     */
      xb=x + random(1, 3)  - 2                   /*     apply Brownian motion for  X.   */
      yb=y + random(1, 3)  - 2                   /*       "       "       "    "   Y.   */
      if @.xb.yb\==hole  then iterate            /*can the mote actually move to there ?*/
      @.x.y=hole                                 /*"empty out"  the old mote position.  */
      @.xb.yb=mote                               /*move the mote  (or possibly not).    */
      if xb<loX  then loX=max(1, xb);     if xb>hiX  then hiX=min( width, xb)
      if yb<loY  then loY=max(1, yb);     if yb>hiY  then hiY=min(height, yb)
      end   /*y*/                                /* [↑]  limit mote's movement to field.*/
    end     /*x*/

  call crop                                      /*crops (or truncates)  the mote field.*/
  end       /*winks*/     /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒*/

call show
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
crop: if loX>1  &  hiX<width  &  loY>1  &  hiY<height  then return    /*are we cropping?*/
                                                 /* [↓]  delete motes (moved off field).*/
              do yc=-1  to height+1  by height+2
                  do xc=-1  to width+1;   if @.xc.yc==hole  then iterate;     @.xc.yc=hole
                  end   /*xc*/
              end       /*yc*/
                                                 /* [↓]  delete motes (moved off field).*/
              do xc=-1  to width+1   by width+2
                  do yc=-1  to height+1;  if @.xc.yc==hole  then iterate;     @.xc.yc=hole
                  end   /*yc*/
              end       /*xc*/
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: clearScr                                   /*¬ necessary, but everything speeds up*/
              do ys=height      for height  by -1;   aRow=
                       do xs=1  for width;           aRow=aRow || @.xs.ys
                       end   /*xs*/
              say aRow
              end            /*ys*/
      return
```

This REXX program makes use of '''scrsize''' REXX program (or BIF) which is used to determine the screen size of the terminal (console).

The '''SCRSIZE.REX''' REXX program is included at [[SCRSIZE.REX]].

Final output when using the following inputs (screen size was 160&times;160):
<tt> ,   ,   ,   fe </tt>
(Shown at one-sixthth size.)

```txt
                                                                        ■
                                                                       ■ ■              ■
                                                                    ■ ■   ■            ■
                                                                     ■     ■          ■ ■
                                                                          ■■           ■
                                                                        ■■ ■            ■■
                                                                            ■  ■        ■    ■
                                                                            ■ ■■       ■■ ■ ■
                                                                             ■  ■        ■ ■    ■
                                                                            ■            ■     ■          ■
                                                                     ■     ■ ■         ■■ ■   ■          ■
                                                                      ■   ■ ■         ■ ■     ■       ■ ■
                                                                     ■ ■   ■        ■  ■ ■  ■ ■     ■ ■■
                                                                    ■  ■  ■ ■        ■■   ■■  ■  ■■■ ■  ■■
                                                                        ■    ■ ■    ■ ■  ■  ■■ ■■   ■
                                                                     ■ ■      ■ ■  ■ ■           ■
                                                                      ■ ■■    ■  ■■
                                                                       ■  ■  ■     ■                                ■
                                                                           ■■                                       ■■
                                                                             ■■■                                   ■
                                                                            ■  ■                                  ■
                                                                  ■      ■ ■                                       ■
                                                                 ■ ■      ■■                                      ■
                                                                  ■ ■    ■■                                 ■    ■
                                                                     ■  ■             ■                     ■   ■
                                                  ■                  ■■ ■           ■■                      ■■  ■
                                                 ■■    ■               ■ ■          ■            ■            ■ ■     ■
                                                   ■  ■              ■■ ■            ■   ■ ■    ■              ■    ■■
                                                    ■■              ■    ■        ■ ■■    ■     ■ ■           ■     ■  ■■
                                                     ■    ■■              ■ ■  ■   ■     ■   ■ ■ ■             ■   ■   ■ ■
                                                      ■   ■              ■ ■  ■     ■■   ■    ■                ■ ■■     ■     ■■
                                                       ■■ ■            ■■   ■■        ■■■     ■                ■ ■ ■   ■    ■ ■
                          ■   ■        ■                ■■                ■  ■        ■  ■   ■                 ■■ ■ ■■■■   ■ ■■
                   ■       ■ ■          ■                ■                ■ ■        ■■      ■    ■  ■        ■         ■ ■
                    ■  ■    ■            ■               ■                ■ ■         ■     ■    ■ ■■ ■       ■          ■
                   ■■ ■    ■              ■               ■■■  ■        ■■ ■      ■ ■  ■■ ■■      ■■          ■           ■■
                     ■■   ■               ■    ■       ■■■   ■■■          ■        ■  ■  ■      ■ ■           ■■         ■ ■■■■
                       ■ ■               ■ ■■   ■         ■■■   ■        ■         ■■■           ■         ■■   ■■   ■         ■■
                        ■      ■         ■■■   ■         ■   ■          ■           ■ ■          ■     ■    ■   ■  ■■
                         ■     ■            ■   ■           ■            ■           ■            ■   ■      ■ ■  ■  ■
                     ■   ■   ■■            ■ ■  ■  ■       ■ ■   ■      ■   ■        ■■        ■   ■ ■ ■  ■■■ ■ ■ ■
                     ■  ■      ■ ■            ■■ ■■ ■       ■ ■   ■■    ■■ ■         ■          ■■■ ■ ■  ■       ■ ■
                   ■■ ■■ ■■     ■                    ■       ■   ■ ■■  ■  ■         ■ ■■ ■ ■   ■   ■   ■■     ■■ ■  ■■
          ■       ■        ■■  ■                      ■■    ■     ■■  ■                 ■ ■■  ■ ■        ■■  ■      ■
          ■■           ■     ■■ ■                       ■  ■■       ■■                   ■ ■■■             ■■
           ■■■        ■ ■   ■ ■                        ■ ■   ■     ■ ■     ■             ■ ■■ ■■           ■
              ■   ■     ■  ■  ■                         ■ ■  ■  ■ ■   ■■  ■■   ■        ■■   ■             ■
           ■   ■ ■ ■■   ■■■  ■ ■                       ■  ■   ■■ ■      ■ ■   ■   ■  ■ ■    ■ ■            ■
         ■■■■■■■  ■  ■■■■     ■■                         ■■    ■ ■       ■    ■  ■ ■  ■        ■ ■■ ■      ■■
                ■■ ■   ■■■      ■                        ■ ■■■■         ■ ■ ■■■■■   ■■■         ■  ■      ■  ■
               ■       ■        ■■■  ■                  ■    ■        ■■   ■   ■      ■ ■          ■          ■
                       ■       ■  ■ ■                    ■           ■      ■ ■        ■ ■          ■          ■■■
                             ■■    ■ ■   ■             ■ ■         ■■      ■  ■        ■  ■■                  ■■■
                                      ■ ■ ■  ■        ■ ■        ■■■      ■    ■          ■■                  ■
                                     ■ ■   ■■■■■ ■  ■■   ■      ■            ■■             ■■               ■
                                 ■ ■ ■■   ■     ■ ■ ■ ■   ■■     ■            ■             ■
                                ■ ■ ■    ■         ■ ■     ■                   ■            ■
                               ■       ■■■         ■        ■                 ■ ■            ■
                              ■                   ■         ■             ■  ■   ■   ■       ■
                                                   ■                       ■■ ■   ■ ■       ■ ■■
                          ■                         ■                         ■    ■■■      ■   ■
                 ■        ■                          ■■                      ■■■  ■  ■■        ■ ■  ■■■
              ■ ■          ■                       ■■                          ■      ■         ■■■■
             ■ ■ ■■■       ■■     ■                ■■                         ■ ■     ■■            ■
                ■   ■        ■■■ ■      ■       ■ ■  ■                 ■     ■  ■    ■  ■           ■  ■   ■
               ■     ■      ■ ■ ■ ■■     ■      ■■                      ■ ■■■   ■  ■■    ■        ■■ ■■   ■ ■  ■
                      ■   ■■   ■  ■■  ■ ■       ■ ■                     ■■   ■          ■        ■   ■ ■ ■   ■■ ■
                       ■  ■    ■    ■■ ■ ■   ■ ■ ■■                    ■  ■  ■          ■  ■        ■   ■ ■ ■ ■
                     ■ ■■ ■        ■      ■   ■  ■                    ■■  ■  ■          ■ ■ ■■      ■   ■  ■   ■
                      ■  ■                 ■■■  ■                    ■  ■     ■        ■■■    ■    ■       ■
                       ■  ■               ■ ■    ■■■                  ■ ■      ■       ■          ■         ■
       ■■    ■ ■ ■       ■           ■   ■■     ■■                   ■ ■               ■           ■■       ■      ■
      ■  ■  ■ ■ ■■■      ■            ■ ■     ■■ ■                  ■   ■■          ■ ■ ■          ■         ■    ■ ■
          ■■   ■  ■     ■        ■■   ■ ■■     ■                   ■              ■  ■ ■                      ■■ ■■
         ■       ■ ■   ■■       ■  ■■■ ■  ■     ■               ■ ■               ■■■                        ■  ■
        ■       ■   ■■■  ■         ■  ■                        ■ ■                   ■                         ■
               ■   ■   ■         ■■    ■■                    ■■  ■■                  ■                          ■
              ■       ■            ■   ■                  ■ ■      ■                 ■  ■                      ■■■■■■
                     ■ ■          ■  ■  ■                  ■        ■                ■ ■                    ■ ■    ■
                    ■   ■       ■■    ■■                   ■        ■■                ■                      ■■    ■
                    ■          ■  ■     ■■                 ■        ■                ■ ■■                      ■    ■
                    ■          ■       ■  ■■            ■ ■ ■      ■                ■ ■ ■■■                   ■■     ■
                ■  ■          ■ ■       ■  ■             ■   ■■     ■              ■     ■■                   ■       ■
             ■   ■■          ■   ■     ■    ■             ■ ■■                      ■   ■■                   ■
             ■  ■           ■ ■   ■   ■      ■          ■■                          ■   ■                     ■
            ■ ■■               ■   ■        ■■        ■■  ■                         ■   ■                 ■■■■ ■■
             ■  ■              ■    ■                    ■ ■  ■                  ■ ■     ■               ■ ■ ■
         ■ ■ ■  ■             ■      ■                   ■  ■■                    ■       ■                ■  ■
          ■■■  ■             ■      ■                    ■                     ■ ■       ■                 ■
              ■ ■           ■      ■                     ■                      ■         ■               ■ ■ ■■
             ■■             ■     ■                     ■ ■                   ■ ■                         ■  ■■
            ■  ■           ■     ■ ■                       ■                 ■ ■■                          ■
           ■  ■ ■        ■■■    ■  ■                       ■■                   ■■
          ■■ ■        ■ ■   ■      ■                       ■                   ■ ■ ■
          ■■           ■     ■      ■  ■                   ■                  ■   ■
         ■■ ■         ■              ■■                                     ■■■   ■
        ■           ■■           ■  ■       ■■■                             ■      ■
        ■          ■■             ■■ ■     ■                               ■      ■ ■
         ■     ■    ■■            ■ ■   ■ ■ ■                                        ■
        ■     ■ ■■ ■             ■   ■  ■■
                 ■■ ■          ■■■■   ■■  ■
                  ■  ■        ■  ■   ■  ■  ■
                 ■    ■      ■ ■     ■ ■
                ■    ■■      ■  ■   ■
               ■    ■  ■        ■
               ■   ■  ■ ■       ■
                ■ ■     ■      ■ ■■
                ■  ■   ■      ■ ■  ■
                       ■        ■ ■
                                ■  ■
                                ■

```



## Ring


```ring

# Project : Brownian tree

load "stdlib.ring"
load "guilib.ring"

paint = null

new qapp
        {
        win1 = new qwidget() {
                  setwindowtitle("")
                  setgeometry(100,100,800,600)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,800,600)
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
                   color = new qcolor()
                   color.setrgb(255,0,0,255)
                   pen = new qpen() {
                   setcolor(color)
                   setwidth(1)}
                   setpen(pen)

        browniantree()

        endpaint()
        }
        label1 { setpicture(p1) show() }
        return

func browniantree()
        numparticles = 3000
        canvas = newlist(210,210)
        canvas[randomf() * 100][randomf() * 200] = 1
        for i = 1 to numparticles
             x = floor((randomf() * 199)) + 1
             y = floor((randomf() * 199)) + 1
             if x = 1
                x = 2
             ok
             if y = 1
                y = 2
             ok
             while canvas[x+1][y+1]+canvas[x][y+1]+canvas[x+1][y]+canvas[x-1][y-1]+canvas[x-1][y]+canvas[x][y-1] = 0
                     x = x + floor((randomf() * 2)) + 1
                     y = y + floor((randomf() * 2)) + 1
                     if x = 1
                        x = 2
                     ok
                     if y = 1
                        y = 2
                     ok
                     if x < 1 or x > 200 or y < 1 or y > 200
                        x = floor((randomf()  * 199)) + 1
                        y = floor((randomf()  * 199)) + 1
                        if x = 1
                           x = 2
                        ok
                        if y = 1
                           y = 2
                        ok
                     ok
             end
             canvas[x][y] = 1
             paint.drawpoint(x,y)
             paint.drawpoint(x,y+1)
             paint.drawpoint(x,y+2)
        next

func randomf()
       decimals(10)
       str = "0."
       for i = 1 to 10
            nr = random(9)
            str = str + string(nr)
       next
       return number(str)

```

Output:

[https://www.dropbox.com/s/a22tu6wf0ibu502/BrownianTree.jpg?dl=0 Brownian tree]


## Ruby

```ruby
require 'rubygems'
require 'RMagick'

NUM_PARTICLES = 1000
SIZE          = 800

def draw_brownian_tree world
  # set the seed
  world[rand SIZE][rand SIZE] = 1

  NUM_PARTICLES.times do
    # set particle's position
    px = rand SIZE
    py = rand SIZE

    loop do
      # randomly choose a direction
      dx = rand(3) - 1
      dy = rand(3) - 1

      if dx + px < 0 or dx + px >= SIZE or dy + py < 0 or dy + py >= SIZE
        # plop the particle into some other random location
        px = rand SIZE
        py = rand SIZE
      elsif world[py + dy][px + dx] != 0
        # bumped into something
        world[py][px] = 1
        break
      else
        py += dy
        px += dx
      end
    end
  end
end

world = Array.new(SIZE) { Array.new(SIZE, 0) }
srand Time.now.to_i

draw_brownian_tree world

img = Magick::Image.new(SIZE, SIZE) do
  self.background_color = "black"
end

draw = Magick::Draw.new
draw.fill "white"

world.each_with_index do |row, y|
  row.each_with_index do |colour, x|
    draw.point x, y if colour != 0
  end
end

draw.draw img
img.write "brownian_tree.bmp"
```



## Run BASIC

[[File:BrownianTreeKokenge.png|thumb|right|]]

```runbasic
numParticles = 3000
dim canvas(201,201)
graphic #g, 200,200
#g fill("blue")
canvas(rnd(1) * 100 , rnd(1) * 200) = 1 'start point
for i = 1 To numParticles
    x = (rnd(1) * 199) + 1
    y = (rnd(1) * 199) + 1
    while canvas(x+1, y+1)+canvas(x, y+1)+canvas(x+1, y)+canvas(x-1, y-1)+canvas(x-1, y)+canvas(x, y-1) = 0
        x = x + (rnd(1)* 2) + 1
        y = y + (rnd(1)* 2) + 1
        If x < 1 Or x > 200 Or y < 1 Or y > 200 then
            x = (rnd(1) * 199) + 1
            y = (rnd(1) * 199) + 1
        end if
    wend
   canvas(x,y) = 1
   #g "color green ; set "; x; " "; y
next i
render #g
#g "flush"
```



## Rust

Translated from D
{{libheader|rand}}
{{libheader|image}}

```rust

extern crate image;
extern crate rand;

use image::ColorType;
use rand::distributions::{IndependentSample, Range};
use std::cmp::{min, max};
use std::env;
use std::path::Path;
use std::process;

fn help() {
    println!("Usage: brownian_tree <output_path> <mote_count> <edge_length>");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut output_path = Path::new("out.png");
    let mut mote_count: u32 = 10000;
    let mut width: usize = 512;
    let mut height: usize = 512;

    match args.len() {
        1 => {}
        4 => {
            output_path = Path::new(&args[1]);
            mote_count = args[2].parse::<u32>().unwrap();
            width = args[3].parse::<usize>().unwrap();
            height = width;
        }
        _ => {
            help();
            process::exit(0);
        }
    }

    assert!(width >= 2);

    // Base 1d array
    let mut field_raw = vec![0u8; width * height];
    populate_tree(&mut field_raw, width, height, mote_count);

    // Balance image for 8-bit grayscale
    let our_max = field_raw.iter().fold(0u8, |champ, e| max(champ, *e));
    let fudge = std::u8::MAX / our_max;
    let balanced: Vec<u8> = field_raw.iter().map(|e| e * fudge).collect();

    match image::save_buffer(output_path,
                             &balanced,
                             width as u32,
                             height as u32,
                             ColorType::Gray(8)) {
        Err(e) => println!("Error writing output image:\n{}", e),
        Ok(_) => println!("Output written to:\n{}", output_path.to_str().unwrap()),
    }
}


fn populate_tree(raw: &mut Vec<u8>, width: usize, height: usize, mc: u32) {
    // Vector of 'width' elements slices
    let mut field_base: Vec<_> = raw.as_mut_slice().chunks_mut(width).collect();
    // Addressable 2d vector
    let mut field: &mut [&mut [u8]] = field_base.as_mut_slice();

    // Seed mote
    field[width / 2][height / 2] = 1;

    let walk_range = Range::new(-1i32, 2i32);
    let x_spawn_range = Range::new(1usize, width - 1);
    let y_spawn_range = Range::new(1usize, height - 1);
    let mut rng = rand::thread_rng();

    for i in 0..mc {
        if i % 100 == 0 {
            println!("{}", i)
        }

        // Spawn mote
        let mut x = x_spawn_range.ind_sample(&mut rng);
        let mut y = y_spawn_range.ind_sample(&mut rng);

        // Increment field value when motes spawn on top of the structure
        if field[x][y] > 0 {
            field[x][y] = min(field[x][y] as u32 + 1, std::u8::MAX as u32) as u8;
            continue;
        }

        loop {
            let contacts = field[x - 1][y - 1] + field[x][y - 1] + field[x + 1][y - 1] +
                           field[x - 1][y] + field[x + 1][y] +
                           field[x - 1][y + 1] + field[x][y + 1] +
                           field[x + 1][y + 1];

            if contacts > 0 {
                field[x][y] = 1;
                break;
            } else {
                let xw = walk_range.ind_sample(&mut rng) + x as i32;
                let yw = walk_range.ind_sample(&mut rng) + y as i32;
                if xw < 1 || xw >= (width as i32 - 1) || yw < 1 || yw >= (height as i32 - 1) {
                    break;
                }
                x = xw as usize;
                y = yw as usize;
            }
        }
    }
}
```

For a 512 x 512 field and 65k motes, run time is 25 s on ~ 2011 hardware (Phenom II X4).
<center>[[File:Rust-Brownian-512-20k.png]]</center>


## Scala


### Java Swing Interoperability


```Scala
import java.awt.Graphics
import java.awt.image.BufferedImage

import javax.swing.JFrame

import scala.collection.mutable.ListBuffer

object BrownianTree extends App {
  val rand = scala.util.Random

  class BrownianTree extends JFrame("Brownian Tree") with Runnable {
    setBounds(100, 100, 400, 300)
    val img = new BufferedImage(getWidth, getHeight, BufferedImage.TYPE_INT_RGB)

    override def paint(g: Graphics): Unit = g.drawImage(img, 0, 0, this)

    override def run(): Unit = {
      class Particle(var x: Int = rand.nextInt(img.getWidth),
                     var y: Int = rand.nextInt(img.getHeight)) {

        /* returns false if either out of bounds or collided with tree */
        def move: Boolean = {
          val (dx, dy) = (rand.nextInt(3) - 1, rand.nextInt(3) - 1)
          if ((x + dx < 0) || (y + dy < 0) ||
            (y + dy >= img.getHeight) || (x + dx >= img.getWidth)) false
          else {
            x += dx
            y += dy
            if ((img.getRGB(x, y) & 0xff00) == 0xff00) {
              img.setRGB(x - dx, y - dy, 0xff00)
               false
            } else true
          }
        }
      }

      var particles = ListBuffer.fill(20000)(new Particle)
      while (particles.nonEmpty) {
        particles = particles.filter(_.move)
        repaint()
      }
    }

    setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    img.setRGB(img.getWidth / 2, img.getHeight / 2, 0xff00)
    setVisible(true)
  }

  new Thread(new BrownianTree).start()
}
```



## Scheme

Works with Guile

```scheme
; Save bitmap to external file
(define (save-pbm bitmap filename)
	(define f (open-output-file filename))
	(simple-format f "P1\n~A ~A\n"
		(list-ref (array-dimensions bitmap) 0)
		(list-ref (array-dimensions bitmap) 1))
	(do ((c 0 (+ c 1))) ((eqv? c (list-ref (array-dimensions bitmap) 1)))
	(do ((r 0 (+ r 1))) ((eqv? r (list-ref (array-dimensions bitmap) 0)))
		(display (array-ref bitmap r c) f))
		(newline f))
	(close-output-port f)
)

; Return a random coordinate in the bitmap that isn't filled yet along with a direction
(define (new-particle bitmap)
	(define x (random (list-ref (array-dimensions bitmap) 0)))
	(define y (random (list-ref (array-dimensions bitmap) 1)))
	(define dx (- (random 3) 1))
	(define dy (- (random 3) 1))
	;Repeat until we find an unused location
	(if (> (array-ref bitmap x y) 0)
		(new-particle bitmap)
		(list (list x y) (list dx dy))))

; Check neighboring coordinates to see if a collision occured
(define (collision-check bitmap p)
	(define c #f)
	(define oob #f)
	(define x (list-ref (car p) 0))
	(define y (list-ref (car p) 1))
	(define dx (list-ref (cadr p) 0))
	(define dy (list-ref (cadr p) 1))
	(define w (list-ref (array-dimensions bitmap) 0))
	(define h (list-ref (array-dimensions bitmap) 1))

	; If the particle hasn't gone out of bounds keep checking for a collision
	(if (or (> 0 x) (> 0 y) (<= w x) (<= h y))
		(set! oob #t)
		(do ((x (- (list-ref (car p) 0) 1) (+ x 1))) ((eqv? x (+ (list-ref (car p) 0) 2)))
		(do ((y (- (list-ref (car p) 1) 1) (+ y 1))) ((eqv? y (+ (list-ref (car p) 1) 2)))
			; Check existing neighbors for collisions
			(if (and (<= 0 x) (<= 0 y) (> w x) (> h y))
				(if (not (zero? (array-ref bitmap x y)))
					(set! c #t))))))
	(if oob
		#f	; Return false if out of bounds
		(if c
			p ; Return the point of collision if a collision occured
			(if (and (zero? dx) (zero? dy))
				#f ; Return false if particle is motionless with no collision
				(collision-check bitmap (particle-move p))))))

; Plot a particle on the bitmap
(define (particle-plot! bitmap p)
	(array-set! bitmap 1 (list-ref (car p) 0) (list-ref (car p) 1)))

; Move a particle along its slope
(define (particle-move p)
	(list (list
		(+ (list-ref (car p) 0) (list-ref (cadr p) 0))
		(+ (list-ref (car p) 1) (list-ref (cadr p) 1)))
		(cadr p)))

; Grow a brownian tree
(define (grow-brownian-tree! bitmap collisions)
	(define w (list-ref (array-dimensions bitmap) 0))
	(define h (list-ref (array-dimensions bitmap) 1))

	; Generate a new particle at a random location
	(define p (new-particle bitmap))

	; Find a collision or lack of one and plot it on the bitmap
	(set! p (collision-check bitmap p))
	(if p (begin
			; Display collision number and the place it happened
			(display collisions)(display ": ")(display (car p))(newline)
			(set! collisions (- collisions 1))
			; Plot the point
			(particle-plot! bitmap p)))

	; If we're done say so
	(if (zero? collisions)
		(display "Done\n"))

	; Keep going until we have enough collisions
	; or have filled the bitmap
	(if (and (< 0 collisions) (memq 0 (array->list (array-contents bitmap))))
		(grow-brownian-tree! bitmap collisions)))

; Plot a random point to seed the brownian tree
(define (seed-brownian-tree! bitmap)
	(define p (new-particle bitmap))
	(particle-plot! bitmap p))

;;; Example usage ;;;
; Seed the random number generator
(let ((time (gettimeofday)))
	(set! *random-state*
		(seed->random-state (+ (car time) (cdr time)))))

; Generate a tree with 320*240 collisions on a bitmap of the size 640x480
; The bitmap is zeroed to start and written with a one where a collision occurs
(define bitmap (make-array 0 640 480))
(seed-brownian-tree! bitmap)
(grow-brownian-tree! bitmap (* 320 240))

; Save to a portable bitmap file
(save-pbm bitmap "brownian-tree.pbm")
```

[[File:Scheme-guile-brownian-tree-large.png]]


## Seed7

[[File:browniantree.png|300px|thumb|right|Simple brownian tree produced with Seed7 program]]
The program below generates a small brownian tree. You can watch how it grows.


```seed7
$ include "seed7_05.s7i";
  include "draw.s7i";
  include "keybd.s7i";

const integer: SIZE is 300;
const integer: SCALE is 1;

const proc: genBrownianTree (in integer: fieldSize, in integer: numParticles) is func
  local
    var array array integer: world is 0 times 0 times 0;
    var integer: px is 0;
    var integer: py is 0;
    var integer: dx is 0;
    var integer: dy is 0;
    var integer: i is 0;
    var boolean: bumped is FALSE;
  begin
    world := fieldSize times fieldSize times 0;
    world[rand(1, fieldSize)][rand(1, fieldSize)] := 1;  # Set the seed
    for i range 1 to numParticles do
      # Set particle's initial position
      px := rand(1, fieldSize);
      py := rand(1, fieldSize);
      bumped := FALSE;
      repeat
        # Randomly choose a direction
        dx := rand(-1, 1);
        dy := rand(-1, 1);
        if dx + px < 1 or dx + px > fieldSize or dy + py < 1 or dy + py > fieldSize then
          # Plop the particle into some other random location
          px := rand(1, fieldSize);
          py := rand(1, fieldSize);
        elsif world[py + dy][px + dx] <> 0 then
          # Bumped into something
          world[py][px] := 1;
          rect(SCALE * pred(px), SCALE * pred(py), SCALE, SCALE, white);
          DRAW_FLUSH;
          bumped := TRUE;
        else
          py +:= dy;
          px +:= dx;
        end if;
      until bumped;
    end for;
  end func;

const proc: main is func
  begin
    screen(SIZE * SCALE, SIZE * SCALE);
    KEYBOARD := GRAPH_KEYBOARD;
    genBrownianTree(SIZE, 20000);
    readln(KEYBOARD);
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/graphic.htm#brownian_tree]


## SequenceL

'''SequenceL Code:'''


```sequencel>import <Utilities/Random.sl
;
import <Utilities/Sequence.sl>;

POINT ::= (X: int, Y: int);
RET_VAL ::= (World: int(2), Rand: RandomGenerator<int, int>, Point: POINT);

randomWalk(x, y, world(2), rand) :=
	let
		randX := getRandom(rand);
		randY := getRandom(randX.Generator);
		nextX := x + (randX.Value mod 3) - 1;
		nextY := y + (randY.Value mod 3) - 1;
		newStartX := (randX.Value mod (size(world) - 2)) + 2;
		newStartY := (randY.Value mod (size(world) - 2)) + 2;

		numNeighbors := world[y-1,x-1] + world[y-1,x] +	world[y-1,x+1] +
						world[y,x-1] +                  world[y,x+1] +
						world[y+1,x-1] + world[y+1,x] +	world[y+1,x+1];

		outOfBounds := nextX <= 1 or nextY <= 1 or nextX >= size(world) or nextY >= size(world);
	in
		randomWalk(newStartX, newStartY, world, randY.Generator) when world[y,x] = 1 or outOfBounds
	else
		(X: x, Y: y) when numNeighbors > 0
	else
		randomWalk(nextX, nextY, world, randY.Generator);

step(rand, world(2)) :=
	let
		walkSeed := getRandom(rand);
		newParticle := randomWalk(size(world)/2,size(world)/2, world, seedRandom(walkSeed.Value));

		newWorld[j] :=
				world[j] when j /= newParticle.Y
			else
				setElementAt(world[j], newParticle.X, 1);
	in
		(World: newWorld, Rand: walkSeed.Generator, Point: newParticle);


initialWorld(worldSize, seed) :=
	let
		world[i,j] := 1 when i = worldSize / 2 and j = worldSize / 2 else 0
					  foreach i within 1 ... worldSize,
							  j within 1 ... worldSize;
	in
		(World: world, Rand: seedRandom(seed), Point: (X: worldSize / 2, Y: worldSize / 2));
```


C++ Driver Code:


```c
#include <time.h

#include <cstdlib>
#include "CImg.h"
#include "SL_Generated.h"

using namespace std;
using namespace cimg_library;

int main(int argc, char ** argv)
{
	int threads = 0;
	int worldSize = 300; if(argc > 1) worldSize = atoi(argv[1]);
	int seed = time(NULL); if(argc > 2) seed = atoi(argv[2]);
	int scale = 2; if(argc > 3) scale = atoi(argv[3]);

	sl_init(threads);

	_sl_RET_VAL current;
	_sl_RET_VAL result;

	const unsigned char black[] = {0};

	CImg<unsigned char> visu(worldSize * scale, worldSize * scale, 1, 1, 0);
	CImgDisplay draw_disp(visu);
	cout << "Brownian Tree in SequenceL" << endl << "Threads: " << threads << endl;
	draw_disp.set_title("Brownian Tree in SequenceL: %d Threads", threads);

	visu.fill(255);

	sl_initialWorld(worldSize, seed, threads, current);

	while(!draw_disp.is_closed())
	{
		visu.draw_circle((current.Point.val().Y - 1) * scale, (current.Point.val().X - 1) * scale, scale/2, black, 1);
		visu.display(draw_disp);
		sl_step(current.Rand.val(), current.World, threads, result);
		current = result;
		draw_disp.wait(1);
	}

	sl_done();

	return 0;
}
```


Output:

[http://i.imgur.com/OrB9tLI.gifv Output Video]


## Sidef

Translated from Perl 6

```ruby
const size = 100
const mid = size>>1
const particlenum = 1000

var map = []
var spawnradius = 5

func set(x, y) {
    map[x][y] = 1
}

func get(x, y) {
    map[x][y] \\ 0
}

set(mid, mid)

var blocks = [
    " ",
    "\N{UPPER HALF BLOCK}",
    "\N{LOWER HALF BLOCK}",
    "\N{FULL BLOCK}"
]

func block(a, b) {
    blocks[2*b + a]
}

func display {
    0..size `by` 2 -> map {|y|
        0..size -> map {|x|
            if ([x, y].all { .-mid < spawnradius }) {
                block(get(x, y), get(x, y+1))
            } else { " " }
        }.join
    }.join("\n").say
}

for progress in (^particlenum) {
    var (x=0, y=0)

    var reset = {
        do {
            (x, y) = (
                (mid-spawnradius .. mid+spawnradius  -> pick),
                [mid-spawnradius,   mid+spawnradius] -> pick
            )
            (x, y) = (y, x) if (1.rand < 0.5)
        } while(get(x, y))
    }

    reset.run

    while ([[-1, 0, 1]]*2 -> cartesian.any {|pair|
        get(x+pair[0], y+pair[1])
    } -> not) {
        x = [x-1, x, x+1].pick
        y = [y-1, y, y+1].pick

        if (1.rand < 0.25) {
            x = (x >= mid ? (x-1) : (x+1))
            y = (y >= mid ? (y-1) : (y+1))
        }

        if ([x,y].any { .-mid > spawnradius }) {
            reset.run
        }
    }

    set(x, y)
    display() if (progress %% 50)

    if ((spawnradius < mid) && [x,y].any { .-mid > spawnradius-5 }) {
        ++spawnradius
    }
}

display()
```

Output:


```txt

                                 ▄               ▄▀          ▄  ▀▄
                         ▀█▄▄▀ █  █▄█▀        ▄  █▄          ▀▄█ █
                        ▄▀  ▀▄█      █       ▄▀ ▄▀   █ ▄    ▄▀  ▀ ▀  ▄ ▄▀
                              ▀▀▄  ▄▀        █▄▀█     ▀█ ▄ ▄▀        ▄█▄▄
                              ▄▄▄▀ ▄▀      ▀▀ ▀  ▀▄   ▀▄▀ ▀        ▀▀▄▀  ▀
                             ▄▀ ▀▄▀▄           ▄ ▄▄▀   █▄▄     ▄ ▄  █
                                    █▄         ▄▀█     █  ▄  █▀ ▀▄█▀
                                 ██▀ ▄▀▄▄▀▄   ▀▀█      ▄▀█  █   ▀
                  ▀▄ ▀▄     ▀▀█          ▀▄  ▄▄▄ █▀     ▀▄▀▀
                ▄ █▄▄▄▀     █▄▄▀     ▄▀▀▄ █▄▀▄ ▄▀▀▄  ▀▄▄█      ▄  ▄
                ▀▀█▄▀          ▀▄▄     ▀▄▀  ██▄ ▀▄▀    █▀▄▄ ▄▄█ ▀▀
               ▀▄▀▄█ ▄ ▄▀  ▄█    ▀█▀▄     ▄▀ ▄▀▄▀ █▀ █▀▀ ▄ ▄▀
               ▄█  ▄▀▀▀▄ ▄ ▀▄ █ ▄  █        ▄ ▀█▀▄▀  █ ▄▄█▀▀      ▀▄▀
                     ▀█▄▀▀▄▀▄  ▀▄▄▀▀  ▄▄    ▀▄▀ ▀▄ ▄█▀█   ▀▄  ▄ ▄▄▀
                   ▄▄▀  ▀    █▄  ▀▀▄▀ ▄█   █      ▀▄▄ ▄       ▄█  ▀▄▀      █
                 ▄    ▄  ▄▄▀▄▀ ▄ ▄▄▄█  ▄█ ▄▀      █▀██▀▄ ▄▀ ▄▀  ▄▄  ▀▀   ▄▀  ▄
            ▄▄▄▄▀     ▀▄     ▀▀▄   ▀ ▀▀▄ ▀ ▀█▀ ▀█▄▀  ▄   ▄▀▀   █▄▄     ▄▄▀▄▄▀    ▄▀
                ▀▄▄▄ █▀ ▀▀▄█    ▀▄▀▄▄ ▀▄▄▄  ▄█   ▀█▀▀▄▄ █    ▄▀▄▄ ▄▄  ▄▀        ▄▀
              ▀▄▀  ▄▀   ▄▄▄▄▀▄▄▄▄ ██▄  ▄▄▀▄▀▄▄▀ ▄▀ ▄▄▄▀▄▄█▄▄▀▀   █ ▄▄▀▄█▄    ▄ █▄▄▄
          ▀▄ █▀     ▀█▄▀ █▄▀ ▀▀ ▀█▄▀ ▀▄▄█▀ ▀▄▀█▄▄▀▀▄ ▀   ▄█▄█▀█   ▀   █ ▀█▄▀▀▄▀▀ ▄▀
           ▀▀       ▀ ▄  ▀▄▀     ▀▄ ▄▄▀ ▀ ▄▀  ▀  ▀▄        ▄▀  ▀▄ ▄     ▀ █▄▄
                   ▄▄█ ▀█▀     ▄  ▄▀█ ▀  █      ██ ▄▀     ▀▄▀ ▄▀▄▀▄▀█      ▀ ▀
                 ▀▀   █    ▄   ▀█▀ █   ▄▀   ▄▄█▀ ▀▀▄        ▀  █   ▄▀▄  ▄
              ▀▀▄▄          ▀▄ █     ▄▀▄   █   ▀█▄█ ▀▄▄▀▄▄      ▀ ▀   ▀▀▄
                 ▄█▄   ▀▄▀▄▄ ▄▀▀      ▀ ▀▄ ▄▀   ▄▀▄    █ █▄  ▄▀
                   ▄▀▄█▀ █ █▀█           ▄    ▄▀ ▀▄▀      ▄█▀ █▄▄▀▀▄
          ▄   ▄▀▄▄█     ▀▄   ▄▀▄        ▀ ▀▀▄▀   █       ▀ ▄█ ▀▄▀▄  █
           ▀▄▀  ▀▀█     █▄▄ ▀  ▄▀         █▀    ▀ ▀▄▀▀▄▄▀  █  ▄▀  ▀▄ ▀▄
          ▀▀       ▀   ██▀ ▀▄ ▄█       ▄▄▀▄▀      █▀                ▀
                     ▄▀  █   ▀▄         ▄▀   ▀▄ ▄█ ▀▀
                    ▀▄  ▀█▄          ▄▄▀▄   ▄▀▀▀▄▄▄
                      ▀    ▀            ▀       ▄▀ █▀  ▄
                                                  ▄▄▀█▄▀▄
                                                ▄▀▄   █  ▀
                                              ▄▀▄▄▄▀
                                             ▀   █ █
                                                 ▀█

```



## Sinclair ZX81 BASIC

Requires at least 2k of RAM. If you have more, you can plot it on a larger grid—up to and including full-screen, provided you don't mind spending literally hours watching the first few dots maunder about without hitting anything.

```basic
 10 DIM A$(20,20)
 20 LET A$(10,10)="1"
 30 FOR Y=42 TO 23 STEP -1
 40 FOR X=0 TO 19
 50 PLOT X,Y
 60 NEXT X
 70 NEXT Y
 80 UNPLOT 9,33
 90 FOR I=1 TO 80
100 LET X=INT (RND*18)+2
110 LET Y=INT (RND*18)+2
120 IF A$(X,Y)="1" THEN GOTO 100
130 UNPLOT X-1,43-Y
140 IF A$(X+1,Y+1)="1" OR A$(X+1,Y)="1" OR A$(X+1,Y-1)="1" OR A$(X,Y+1)="1" OR A$(X,Y-1)="1" OR A$(X-1,Y+1)="1" OR A$(X-1,Y)="1" OR A$(X-1,Y-1)="1" THEN GOTO 230
150 PLOT X-1,43-Y
160 LET X=X+INT (RND*3)-1
170 LET Y=Y+INT (RND*3)-1
180 IF X=1 THEN LET X=19
190 IF X=20 THEN LET X=2
200 IF Y=1 THEN LET Y=19
210 IF Y=20 THEN LET Y=2
220 GOTO 130
230 LET A$(X,Y)="1"
240 NEXT I
```

Output:

Screenshot [http://www.edmundgriffiths.com/zx81browniantree.jpg here].


## Tcl

```tcl
package require Tcl 8.5
package require Tk

set SIZE 300

image create photo brownianTree -width $SIZE -height $SIZE
interp alias {} plot {} brownianTree put white -to
brownianTree put black -to 0 0 [expr {$SIZE-1}] [expr {$SIZE-1}]
proc rnd {range} {expr {int(rand() * $range)}}

proc makeBrownianTree count {
    global SIZE
    # Set the seed
    plot [rnd $SIZE] [rnd $SIZE]
    for {set i 0} {$i<$count} {incr i} {
	# Set a random particle's initial position
	set px [rnd $SIZE]
	set py [rnd $SIZE]

	while 1 {
	    # Randomly choose a direction
	    set dx [expr {[rnd 3] - 1}]
	    set dy [expr {[rnd 3] - 1}]

	    # If we are going out of bounds...
	    if {$px+$dx < 0 || $px+$dx >= $SIZE || $py+$dy < 0 || $py+$dy>=$SIZE} {
		# Out of bounds, so move back in
		set dx [expr {[rnd 3] - 1}]
		set dy [expr {[rnd 3] - 1}]
		continue
	    }

	    set ox $px
	    set oy $py
	    # Move/see if we would hit anything
	    incr px $dx
	    incr py $dy
	    if {[lindex [brownianTree get $px $py] 0]} {
		# Hit something, so plot where we were
		plot $ox $oy
		break
	    }
	}
	## For display while things are processing, uncomment next line
	#update;puts -nonewline .;flush stdout
    }
}

pack [label .l -image brownianTree]
update
makeBrownianTree 1000
brownianTree write tree.ppm
```


## TI-83 BASIC

```ti83b
:StoreGDB 0
:ClrDraw
:FnOff
:AxesOff
:Pxl-On(31,47)
:For(I,1,50)
:randInt(1,93)→X
:randInt(1,61)→Y
:1→A
:While A
:randInt(1,4)→D
:Pxl-Off(Y,X)
:If D=1 and Y≥2
:Y-1→Y
:If D=2 and X≤92
:X+1→X
:If D=3 and Y≤60
:Y+1→Y
:If D=4 and X≥2
:X-1→X
:Pxl-On(Y,X)
:If pxl-Test(Y+1,X) or pxl-Test(Y+1,X+1) or pxl-Test(Y+1,X-1) or pxl-Test(Y,X+1) or pxl-Test(Y,X-1) or pxl-Test(Y-1,X) or pxl-Test(Y-1,X-1) or pxl-Test(Y-1,X+1)
:0→A
:End
:End
:Pause
:RecallGDB 0
```



## Visual Basic .NET

Windows Forms Application.


```vbnet

Imports System.Drawing.Imaging

Public Class Form1

  ReadOnly iCanvasColor As Integer = Color.Black.ToArgb
  ReadOnly iSeedColor As Integer = Color.White.ToArgb

  Dim iCanvasWidth As Integer = 0
  Dim iCanvasHeight As Integer = 0

  Dim iPixels() As Integer = Nothing

  Private Sub BrownianTree()

    Dim oCanvas As Bitmap = Nothing
    Dim oRandom As New Random(Now.Millisecond)
    Dim oXY As Point = Nothing
    Dim iParticleCount As Integer = 0

    iCanvasWidth = ClientSize.Width
    iCanvasHeight = ClientSize.Height

    oCanvas = New Bitmap(iCanvasWidth, iCanvasHeight, Imaging.PixelFormat.Format24bppRgb)

    Graphics.FromImage(oCanvas).Clear(Color.FromArgb(iCanvasColor))

    iPixels = GetData(oCanvas)

    ' We'll use about 10% of the total number of pixels in the canvas for the particle count.
    iParticleCount = CInt(iPixels.Length * 0.1)

    ' Set the seed to a random location on the canvas.
    iPixels(oRandom.Next(iPixels.Length)) = iSeedColor

    ' Run through the particles.
    For i As Integer = 0 To iParticleCount
      Do
        ' Find an open pixel.
        oXY = New Point(oRandom.Next(oCanvas.Width), oRandom.Next(oCanvas.Height))
      Loop While iPixels(oXY.Y * oCanvas.Width + oXY.X) = iSeedColor

      ' Jitter until the pixel bumps another.
      While Not CheckAdjacency(oXY)
        oXY.X += oRandom.Next(-1, 2)
        oXY.Y += oRandom.Next(-1, 2)

        ' Make sure we don't jitter ourselves out of bounds.
        If oXY.X < 0 Then oXY.X = 0 Else If oXY.X >= oCanvas.Width Then oXY.X = oCanvas.Width - 1
        If oXY.Y < 0 Then oXY.Y = 0 Else If oXY.Y >= oCanvas.Height Then oXY.Y = oCanvas.Height - 1
      End While

      iPixels(oXY.Y * oCanvas.Width + oXY.X) = iSeedColor

      ' If you'd like to see updates as each particle collides and becomes
      ' part of the tree, uncomment the next 4 lines (it does slow it down slightly).
      ' SetData(oCanvas, iPixels)
      ' BackgroundImage = oCanvas
      ' Invalidate()
      ' Application.DoEvents()
    Next

    oCanvas.Save("BrownianTree.bmp")
    BackgroundImage = oCanvas

  End Sub

  ' Check adjacent pixels for an illuminated pixel.
  Private Function CheckAdjacency(ByVal XY As Point) As Boolean

    Dim n As Integer = 0

    For y As Integer = -1 To 1
      ' Make sure not to drop off the top or bottom of the image.
      If (XY.Y + y < 0) OrElse (XY.Y + y >= iCanvasHeight) Then Continue For

      For x As Integer = -1 To 1
        ' Make sure not to drop off the left or right of the image.
        If (XY.X + x < 0) OrElse (XY.X + x >= iCanvasWidth) Then Continue For

        ' Don't run the test on the calling pixel.
        If y <> 0 AndAlso x <> 0 Then
          n = (XY.Y + y) * iCanvasWidth + (XY.X + x)
          If iPixels(n) = iSeedColor Then Return True
        End If
      Next
    Next

    Return False

  End Function

  Private Function GetData(ByVal Map As Bitmap) As Integer()

    Dim oBMPData As BitmapData = Nothing
    Dim oData() As Integer = Nothing

    oBMPData = Map.LockBits(New Rectangle(0, 0, Map.Width, Map.Height), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)

    Array.Resize(oData, Map.Width * Map.Height)

    Runtime.InteropServices.Marshal.Copy(oBMPData.Scan0, oData, 0, oData.Length)

    Map.UnlockBits(oBMPData)

    Return oData

  End Function

  Private Sub SetData(ByVal Map As Bitmap, ByVal Data As Integer())

    Dim oBMPData As BitmapData = Nothing

    oBMPData = Map.LockBits(New Rectangle(0, 0, Map.Width, Map.Height), ImageLockMode.WriteOnly, PixelFormat.Format32bppArgb)

    Runtime.InteropServices.Marshal.Copy(Data, 0, oBMPData.Scan0, Data.Length)

    Map.UnlockBits(oBMPData)

  End Sub

  Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
    DoubleBuffered = True
    BackgroundImageLayout = ImageLayout.Center
    Show()
    Activate()
    Application.DoEvents()
    BrownianTree()
  End Sub
End Class

```

{{out|Final output}}
[[File:SH_BrownianTree.jpg]]


## XPL0

[[File:BrownXPL0.gif|right]]

```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
def W=128, H=W;         \width and height of field
int X, Y;
[SetVid($13);           \set 320x200 graphic video mode
Point(W/2, H/2, 6\brown\);               \place seed in center of field
loop    [repeat X:= Ran(W);  Y:= Ran(H); \inject particle
        until   ReadPix(X,Y) = 0;        \ in an empty location
        loop    [Point(X, Y, 6\brown\);  \show particle
                if ReadPix(X-1,Y) or ReadPix(X+1,Y) or \particle collided
                   ReadPix(X,Y-1) or ReadPix(X,Y+1) then quit;
                Point(X, Y, 0\black\);   \erase particle
                X:= X + Ran(3)-1;        \(Brownian) move particle
                Y:= Y + Ran(3)-1;
                if X<0 or X>=W or Y<0 or Y>=H then quit; \out of bounds
                ];
        if KeyHit then [SetVid(3);  quit]; \restore text mode
        ];
]
```



## zkl

This grows rather slowly, so I've added a circle for barnacles to attach to. It looks like tendrils growing from the center to the circle and vice versa. The tree type is similar to that shown in the XPLO and Visual Basic .NET solutions.
Also, the image is written to disk as each particle attaches so EventViewer will auto update to show the progression.

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
[[File:Brownian.zkl.jpg|250px|thumb|right]]

```zkl
w:=h:=400; numParticles:=20000;
bitmap:=PPM(w+2,h+2,0);  // add borders as clip regions

bitmap[w/2,h/2]=0xff|ff|ff;          	// plant seed
bitmap.circle(w/2,h/2,h/2,0x0f|0f|0f);	// plant seeds

fcn touching(x,y,bitmap){ // is (x,y) touching another pixel?
   // (x,y) isn't on the border/edge of bitmap so no edge conditions
   var [const] box=T(T(-1,-1),T(0,-1),T(1,-1),
		     T(-1, 0),        T(1, 0),
		     T(-1, 1),T(0, 1),T(1, 1));
   box.filter1('wrap([(a,b)]){ bitmap[a+x,b+y] }); //-->False: not touching, (a,b) if is
}

while(numParticles){
   c:=(0x1|00|00).random(0x1|00|00|00) + (0x1|00).random(0x1|00|00) + (0x1).random(0x1|00);
   reg x,y;
   do{ x=(1).random(w); y=(1).random(h); }while(bitmap[x,y]); // find empty spot
   while(1){  // stagger around until bump into a particle, then attach barnicle
      if(touching(x,y,bitmap)){
         bitmap[x,y]=c;
	 bitmap.write(f:=File("foo.ppm","wb"));  // tell ImageViewer to update image
	 numParticles-=1;
	 break;
      }
      x+=(-1).random(2); y+=(-1).random(2); // [-1,0,1]
      if( not ((0<x<w) and (0<y<h)) ){ // next to border --> color border
         bitmap[x,y]=c;
	 break;
      }
   }
}
bitmap.write(File("foo.ppm","wb"));  // the final image
```



## ZX Spectrum Basic

Translated from Run BASIC
Very, very slow on a ZX Spectrum (even emulate and at maximum speed). Best use SpecBAS, changing the value of the variable np to 6000.

```zxbasic
10 LET np=1000
20 PAPER 0: INK 4: CLS
30 PLOT 128,88
40 FOR i=1 TO np
50 GO SUB 1000
60 IF NOT ((POINT (x+1,y+1)+POINT (x,y+1)+POINT (x+1,y)+POINT (x-1,y-1)+POINT (x-1,y)+POINT (x,y-1))=0) THEN GO TO 100
70 LET x=x+RND*2-1: LET y=y+RND*2-1
80 IF x<1 OR x>254 OR y<1 OR y>174 THEN GO SUB 1000
90 GO TO 60
100 PLOT x,y
110 NEXT i
120 STOP
1000 REM Calculate new pos
1010 LET x=RND*254
1020 LET y=RND*174
1030 RETURN

```
