+++
title = "Sierpinski triangle/Graphical"
description = ""
date = 2019-06-05T18:32:11Z
aliases = []
[extra]
id = 9550
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "actionscript",
  "asymptote",
  "ats",
  "autohotkey",
  "bbc_basic",
  "c",
  "cpp",
  "d",
  "erlang",
  "erre",
  "executing",
  "factor",
  "freebasic",
  "gnuplot",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "liberty_basic",
  "logo",
  "lua",
  "mathematica",
  "matlab",
  "objeck",
  "ocaml",
  "ord_order_fn_file_name_ttl_plot_title_clr_color",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "plotting_sierpinski_triangle_aev_4_1_17",
  "postscript",
  "prolog",
  "python",
  "r",
  "racket",
  "ring",
  "ruby",
  "run_basic",
  "seed7",
  "sidef",
  "tcl",
  "xpl0",
  "yabasic",
  "zkl",
]
+++

Produce a graphical representation of a [[wp:Sierpinski triangle|Sierpinski triangle]] of order N in any orientation.

An example of Sierpinski's triangle (order = 8) looks like this: <br/><br/>
[[File:Sierpinski_Triangle_Unicon.PNG]]


## ActionScript

SierpinskiTriangle class:

```ActionScript3

package {

    import flash.display.GraphicsPathCommand;
    import flash.display.Sprite;

    /**
     * A Sierpinski triangle.
     */
    public class SierpinskiTriangle extends Sprite {

        /**
         * Creates a new SierpinskiTriangle object.
         *
         * @param n The order of the Sierpinski triangle.
         * @param c1 The background colour.
         * @param c2 The foreground colour.
         * @param width The width of the triangle.
         * @param height The height of the triangle.
         */
        public function SierpinskiTriangle(n:uint, c1:uint, c2:uint, width:Number, height:Number):void {
            _init(n, c1, c2, width, height);
        }

        /**
         * Generates the triangle.
         *
         * @param n The order of the Sierpinski triangle.
         * @param c1 The background colour.
         * @param c2 The foreground colour.
         * @param width The width of the triangle.
         * @param height The height of the triangle.
         * @private
         */
        private function _init(n:uint, c1:uint, c2:uint, width:Number, height:Number):void {

            if ( n <= 0 )
                return;

            // Draw the outer triangle.

            graphics.beginFill(c1);
            graphics.moveTo(width / 2, 0);
            graphics.lineTo(0, height);
            graphics.lineTo(width, height);
            graphics.lineTo(width / 2, 0);

            // Draw the inner triangle.

            graphics.beginFill(c2);
            graphics.moveTo(width / 4, height / 2);
            graphics.lineTo(width * 3 / 4, height / 2);
            graphics.lineTo(width / 2, height);
            graphics.lineTo(width / 4, height / 2);

            if ( n == 1 )
                return;

            // Recursively generate three Sierpinski triangles of half the size and order n - 1 and position them appropriately.

            var sub1:SierpinskiTriangle = new SierpinskiTriangle(n - 1, c1, c2, width / 2, height / 2);
            var sub2:SierpinskiTriangle = new SierpinskiTriangle(n - 1, c1, c2, width / 2, height / 2);
            var sub3:SierpinskiTriangle = new SierpinskiTriangle(n - 1, c1, c2, width / 2, height / 2);

            sub1.x = width / 4;
            sub1.y = 0;
            sub2.x = 0;
            sub2.y = height / 2;
            sub3.x = width / 2;
            sub3.y = height / 2;

            addChild(sub1);
            addChild(sub2);
            addChild(sub3);

        }

    }

}

```


Document class:

```ActionScript3

package {

    import flash.display.Sprite;
    import flash.events.Event;

    public class Main extends Sprite {

        public function Main():void {
            if ( stage ) init();
            else addEventListener(Event.ADDED_TO_STAGE, init);
        }

        private function init(e:Event = null):void {
            var s:SierpinskiTriangle = new SierpinskiTriangle(5, 0x0000FF, 0xFFFF00, 300, 150 * Math.sqrt(3));
            // Equilateral triangle (blue and yellow)
            s.x = s.y = 20;
            addChild(s);
        }

    }

}

```



## Asymptote

This simple-minded recursive apporach doesn't scale well to large orders, but neither would your PostScript viewer, so there's nothing to gain from a more efficient algorithm. Thus are the perils of vector graphics.


```asymptote
path subtriangle(path p, real node) {
    return
        point(p, node) --
        point(p, node + 1/2) --
        point(p, node - 1/2) --
        cycle;
}

void sierpinski(path p, int order) {
    if (order == 0)
        fill(p);
    else {
        sierpinski(subtriangle(p, 0), order - 1);
        sierpinski(subtriangle(p, 1), order - 1);
        sierpinski(subtriangle(p, 2), order - 1);
    }
}

sierpinski((0, 0) -- (5 inch, 1 inch) -- (2 inch, 6 inch) -- cycle, 10);
```



## ATS

```ATS
// patscc -O2 -flto -D_GNU_SOURCE -DATS_MEMALLOC_LIBC sierpinski.dats -o sierpinski -latslib -lSDL2
#include "share/atspre_staload.hats"

typedef point = (int, int)

extern fun midpoint(A: point, B: point): point = "mac#"

extern fun sierpinski_draw(n: int, A: point, B: point, C: point): void = "mac#"

extern fun triangle_remove(A: point, B: point, C: point): void = "mac#"

extern fun sdl_drawline(x1: int, y1: int, x2: int, y2: int): void = "ext#sdl_drawline"

extern fun line(A: point, B: point): void

extern fun ats_tredraw(): void = "mac#ats_tredraw"

implement midpoint(A, B) = (xmid, ymid) where {
  val xmid = (A.0 + B.0) / 2
  val ymid = (A.1 + B.1) / 2
}

implement triangle_remove(A, B, C) = (
  line(A, B);
  line(B, C);
  line(C, A);
)

implement sierpinski_draw(n, A, B, C) =
  if n > 0 then
    let
      val AB = midpoint(A, B)
      val BC = midpoint(B, C)
      val CA = midpoint(C, A)
    in
      triangle_remove(AB, BC, CA);
      sierpinski_draw(n-1, A, AB, CA);
      sierpinski_draw(n-1, B, BC, AB);
      sierpinski_draw(n-1, C, CA, BC);
    end

implement line(A, B) = sdl_drawline(A.0, A.1, B.0, B.1)

extern fun SDL_Init(): void = "ext#sdl_init"
extern fun SDL_Quit(): void = "ext#sdl_quit"
extern fun SDL_Loop(): void = "ext#sdl_loop"

implement ats_tredraw() = sierpinski_draw(7, (320, 0), (0, 480), (640, 480))

implement main0() = (
  SDL_Init();
  SDL_Loop();
  SDL_Quit();
)

%{
#include <SDL2/SDL.h>
#include <unistd.h>
extern void ats_tredraw();
SDL_Window *sdlwin;
SDL_Renderer *sdlren;
void sdl_init() {
  if (SDL_Init(SDL_INIT_VIDEO)) {
    exit(1);
  }
  if ((sdlwin = SDL_CreateWindow("sierpinski triangles", 100, 100, 640, 480, SDL_WINDOW_SHOWN)) == NULL) {
    SDL_Quit();
    exit(2);
  }
  if ((sdlren = SDL_CreateRenderer(sdlwin, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC)) == NULL) {
    SDL_DestroyWindow(sdlwin);
    SDL_Quit();
    exit(3);
  }
}
void sdl_clear() {
  SDL_SetRenderDrawColor(sdlren, 0, 0, 0, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(sdlren);
  SDL_SetRenderDrawColor(sdlren, 255, 255, 255, SDL_ALPHA_OPAQUE);
}
void sdl_loop() {
  SDL_Event event;
  while (1) {
    sdl_clear();
    ats_tredraw();
    SDL_RenderPresent(sdlren);
    while (SDL_PollEvent(&event)) {
      if (event.type == SDL_QUIT) {
        return;
      }
    }
  }
}
void sdl_quit() {
    SDL_DestroyRenderer(sdlren);
    SDL_DestroyWindow(sdlwin);
    SDL_Quit();
}

void sdl_drawline(int x1, int y1, int x2, int y2) {
  SDL_RenderDrawLine(sdlren, x1, y1, x2, y2);
}
%}
```



## AutoHotkey

```AutoHotkey
#NoEnv
#SingleInstance, Force
SetBatchLines, -1

; Parameters
Width := 512, Height := Width/2*3**0.5, n := 8 ; iterations = 8

; Uncomment if Gdip.ahk is not in your standard library
#Include ..\lib\Gdip.ahkl

If !pToken := Gdip_Startup() ; Start gdi+
{
	MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
	ExitApp
}
; I've added a simple new function here, just to ensure if anyone is having any problems then to make sure they are using the correct library version
if (Gdip_LibraryVersion() < 1.30)
{
	MsgBox, 48, Version error!, Please download the latest version of the gdi+ library
	ExitApp
}
OnExit, Exit

; Create a layered window (+E0x80000 : must be used for UpdateLayeredWindow to work!) that is always on top (+AlwaysOnTop), has no taskbar entry or caption
Gui, -Caption +E0x80000 +LastFound +OwnDialogs +Owner +AlwaysOnTop
Gui, Show
hwnd1 := WinExist()
OnMessage(0x201, "WM_LBUTTONDOWN")

, hbm := CreateDIBSection(Width, Height)
, hdc := CreateCompatibleDC()
, obm := SelectObject(hdc, hbm)
, G := Gdip_GraphicsFromHDC(hdc)
, Gdip_SetSmoothingMode(G, 4)

; Sierpinski triangle by subtracting triangles
, pBrushBlack := Gdip_BrushCreateSolid(0xff000000)
, rectangle := 0 "," 0 "|" 0 "," Height "|" Width "," Height "|" Width "," 0
, Gdip_FillPolygon(G, pBrushBlack,  rectangle, FillMode=0)

, pBrushBlue := Gdip_BrushCreateSolid(0xff0000ff)
, triangle := Width/2 "," 0 "|" 0 "," Height "|" Width "," Height
, Gdip_FillPolygon(G, pBrushBlue,  triangle, FillMode=0)
, Gdip_DeleteBrush(pBrushBlue)

, UpdateLayeredWindow(hwnd1, hdc, (A_ScreenWidth-Width)/2, (A_ScreenHeight-Height)/2, Width, Height)

, k:=2, x:=0, y:=0, i:=1
Loop, % n
{
	Sleep 0.5*1000
	While x*y<Width*Height
	{
		triangle := x "," y "|" x+Width/2/k "," y+Height/k "|" x+Width/k "," y
		, Gdip_FillPolygon(G, pBrushBlack,  triangle, FillMode=0)
		, x += Width/k
		, (x >= Width) ? (x := i*Width/2/k, y += Height/k, i:=!i) : ""
	}
	UpdateLayeredWindow(hwnd1, hdc, (A_ScreenWidth-Width)/2, (A_ScreenHeight-Height)/2, Width, Height)
	, k*=2, x:=0, y:=0, i:=1
}

Gdip_DeleteBrush(pBrushBlack)

, UpdateLayeredWindow(hwnd1, hdc, (A_ScreenWidth-Width)/2, (A_ScreenHeight-Height)/2, Width, Height)
Sleep, 1*1000

; Bonus: Sierpinski triangle by random dots
Gdip_GraphicsClear(G, 0xff000000)
, pBrushBlue := Gdip_BrushCreateSolid(0xff0000ff)
, x1:=Width/2, y1:=0, x2:=0, y2:=Height, x3:=Width, y3:=Height
, x:= Width/2, y:=Height/2 ; I'm to lazy to pick a random point.
Loop, % n
{
	Loop, % 10*10**(A_Index/2)
	{
		Random, rand, 1, 3
		x := abs(x+x%rand%)/2
		, y := abs(y+y%rand%)/2
		, Gdip_FillEllipse(G, pBrushBlue, x, y, 1, 1)
	}
	UpdateLayeredWindow(hwnd1, hdc, (A_ScreenWidth-Width)/2, (A_ScreenHeight-Height)/2, Width, Height)
	Sleep, 0.5*1000
}
SelectObject(hdc, obm)
, DeleteObject(hbm)
, DeleteDC(hdc)
, Gdip_DeleteGraphics(G)
Return

Exit:
	Gdip_Shutdown(pToken)
	ExitApp

WM_LBUTTONDOWN()
{
	If (A_Gui = 1)
	PostMessage, 0xA1, 2
}
```



## BBC BASIC

```bbcbasic
      order% = 8
      size% = 2^order%
      VDU 23,22,size%;size%;8,8,16,128
      FOR Y% = 0 TO size%-1
        FOR X% = 0 TO size%-1
          IF (X% AND Y%)=0 PLOT X%*2,Y%*2
        NEXT
      NEXT Y%

```

[[File:sierpinski_triangle_bbc.gif]]


## C

[[file:sierp-tri-c.png|thumb|center|128px]]Code lifted from [[Dragon curve]].  Given a depth n, draws a triangle of size 2^n in a PNM file to the standard output.  Usage: <code>gcc -lm stuff.c -o sierp; ./sierp 9 > triangle.pnm</code>.  Sample image generated with depth 9.  Generated image's size depends on the depth: it plots dots, but does not draw lines, so a large size with low depth is not possible.


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

long long x, y, dx, dy, scale, clen, cscale;
typedef struct { double r, g, b; } rgb;
rgb ** pix;

void sc_up()
{
	scale *= 2; x *= 2; y *= 2;
	cscale *= 3;
}

void h_rgb(long long x, long long y)
{
	rgb *p = &pix[y][x];

#	define SAT 1
	double h = 6.0 * clen / cscale;
	double VAL = 1;
	double c = SAT * VAL;
	double X = c * (1 - fabs(fmod(h, 2) - 1));

	switch((int)h) {
	case 0: p->r += c; p->g += X; return;
	case 1:	p->r += X; p->g += c; return;
	case 2: p->g += c; p->b += X; return;
	case 3: p->g += X; p->b += c; return;
	case 4: p->r += X; p->b += c; return;
	default:
		p->r += c; p->b += X;
	}
}

void iter_string(const char * str, int d)
{
	long long len;
	while (*str != '\0') {
		switch(*(str++)) {
		case 'X':
			if (d)	iter_string("XHXVX", d - 1);
			else{
				clen ++;
				h_rgb(x/scale, y/scale);
				x += dx;
				y -= dy;
			}
			continue;
		case 'V':
			len = 1LLU << d;
			while (len--) {
				clen ++;
				h_rgb(x/scale, y/scale);
				y += dy;
			}
			continue;
		case 'H':
			len = 1LLU << d;
			while(len --) {
				clen ++;
				h_rgb(x/scale, y/scale);
				x -= dx;
			}
			continue;
		}
	}
}

void sierp(long leng, int depth)
{
	long i;
	long h = leng + 20, w = leng + 20;

	/* allocate pixel buffer */
	rgb *buf = malloc(sizeof(rgb) * w * h);
	pix = malloc(sizeof(rgb *) * h);
	for (i = 0; i < h; i++)
		pix[i] = buf + w * i;
	memset(buf, 0, sizeof(rgb) * w * h);

        /* init coords; scale up to desired; exec string */
	x = y = 10; dx = leng; dy = leng; scale = 1; clen = 0; cscale = 3;
	for (i = 0; i < depth; i++) sc_up();
	iter_string("VXH", depth);

	/* write color PNM file */
	unsigned char *fpix = malloc(w * h * 3);
	double maxv = 0, *dbuf = (double*)buf;

	for (i = 3 * w * h - 1; i >= 0; i--)
		if (dbuf[i] > maxv) maxv = dbuf[i];
	for (i = 3 * h * w - 1; i >= 0; i--)
		fpix[i] = 255 * dbuf[i] / maxv;

	printf("P6\n%ld %ld\n255\n", w, h);
	fflush(stdout); /* printf and fwrite may treat buffer differently */
	fwrite(fpix, h * w * 3, 1, stdout);
}

int main(int c, char ** v)
{
	int size, depth;

	depth  = (c > 1) ? atoi(v[1]) : 10;
	size = 1 << depth;

	fprintf(stderr, "size: %d depth: %d\n", size, depth);
	sierp(size, depth + 2);

	return 0;
}
```



## C++

[[file:STriCpp.png|thumb|right|200px]]

```cpp

#include <windows.h>
#include <string>
#include <iostream>

const int BMP_SIZE = 612;

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
        HANDLE file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS,
                                  FILE_ATTRIBUTE_NORMAL, NULL );
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
class sierpinski {
public:
    void draw( int o ) {
        colors[0] = 0xff0000; colors[1] = 0x00ff33; colors[2] = 0x0033ff;
        colors[3] = 0xffff00; colors[4] = 0x00ffff; colors[5] = 0xffffff;
        bmp.create( BMP_SIZE, BMP_SIZE ); HDC dc = bmp.getDC();
        drawTri( dc, 0, 0, ( float )BMP_SIZE, ( float )BMP_SIZE, o / 2 );
        bmp.setPenColor( colors[0] ); MoveToEx( dc, BMP_SIZE >> 1, 0, NULL );
        LineTo( dc, 0, BMP_SIZE - 1 ); LineTo( dc, BMP_SIZE - 1, BMP_SIZE - 1 );
        LineTo( dc, BMP_SIZE >> 1, 0 ); bmp.saveBitmap( "./st.bmp" );
    }
private:
    void drawTri( HDC dc, float l, float t, float r, float b, int i ) {
        float w = r - l, h = b - t, hh = h / 2.f, ww = w / 4.f;
        if( i ) {
            drawTri( dc, l + ww, t, l + ww * 3.f, t + hh, i - 1 );
            drawTri( dc, l, t + hh, l + w / 2.f, t + h, i - 1 );
            drawTri( dc, l + w / 2.f, t + hh, l + w, t + h, i - 1 );
        }
        bmp.setPenColor( colors[i % 6] );
        MoveToEx( dc, ( int )( l + ww ),          ( int )( t + hh ), NULL );
        LineTo  ( dc, ( int )( l + ww * 3.f ),    ( int )( t + hh ) );
        LineTo  ( dc, ( int )( l + ( w / 2.f ) ), ( int )( t + h ) );
        LineTo  ( dc, ( int )( l + ww ),          ( int )( t + hh ) );
    }
    myBitmap bmp;
    DWORD colors[6];
};
int main(int argc, char* argv[]) {
    sierpinski s; s.draw( 12 );
    return 0;
}

```



## D

The output image is the same as the Go version. This requires the module from the Grayscale image Task.
```d
void main() {
    import grayscale_image;

    enum order = 8,
         margin = 10,
         width = 2 ^^ order;

    auto im = new Image!Gray(width + 2 * margin, width + 2 * margin);
    im.clear(Gray.white);

    foreach (immutable y; 0 .. width)
        foreach (immutable x; 0 .. width)
            if ((x & y) == 0)
                im[x + margin, y + margin] = Gray.black;
    im.savePGM("sierpinski.pgm");
}
```



## Erlang


```Erlang

-module(sierpinski).
-author("zduchac").
-export([start/0]).

sierpinski(DC, Order) ->
    Size = 1 bsl Order,
    sierpinski(DC, Order, Size, 0, 0).

sierpinski(_, _, Size, _, Y) when Y =:= Size ->
    ok;
sierpinski(DC, Order, Size, X, Y) when X =:= Size ->
    sierpinski(DC, Order, Size, 0, Y + 1);
sierpinski(DC, Order, Size, X, Y) when X band Y =:= 0 ->
    wxDC:drawPoint(DC, {X, Y}),
    sierpinski(DC, Order, Size, X + 1, Y);
sierpinski(DC, Order, Size, X, Y) ->
    sierpinski(DC, Order, Size, X + 1, Y).

start() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Raytracer", []),
    wxFrame:connect(Frame, paint, [{callback,
				    fun(_Evt, _Obj) ->
					    DC = wxPaintDC:new(Frame),
					    sierpinski(DC, 8),
					    wxPaintDC:destroy(DC)
				    end
				   }]),
    wxFrame:show(Frame).

```



## ERRE


```ERRE

PROGRAM SIERPINSKY

!$INCLUDE="PC.LIB"

BEGIN
   ORDER%=8
   SIZE%=2^ORDER%
   SCREEN(9)
   GR_WINDOW(0,0,520,520)
   FOR Y%=0 TO SIZE%-1 DO
     FOR X%=0 TO SIZE%-1 DO
        IF (X% AND Y%)=0 THEN PSET(X%*2,Y%*2,2) END IF
     END FOR
   END FOR
   GET(K$)
END PROGRAM

```



## Factor


```factor
USING: accessors images images.loader kernel literals math
math.bits math.functions make sequences ;
IN: rosetta-code.sierpinski-triangle-graphical

CONSTANT: black B{ 33  33  33  255 }
CONSTANT: white B{ 255 255 255 255 }
CONSTANT: size  $[ 2 8 ^ ]           ! edit 8 to change order

! Generate Sierpinksi's triangle sequence. This is sequence
! A001317 in OEIS.

: sierpinski ( n -- seq )
    [ [ 1 ] dip [ dup , dup 2 * bitxor ] times ] { } make nip ;

! Convert a number to binary, then append a black pixel for each
! set bit or a white pixel for each unset bit to the image being
! built by make.

: expand ( n -- ) make-bits [ black white ? % ] each ;

! Append white pixels until the end of the row in the image
! being built by make.

: pad ( n -- ) [ size ] dip 1 + - [ white % ] times ;

! Generate the image data for a sierpinski triangle of a given
! size in pixels. The image is square so its dimensions are
! n x n.

: sierpinski-img ( n -- seq )
    sierpinski [ [ [ expand ] dip pad ] each-index ] B{ } make ;

: main ( -- )
    <image>
    ${ size size }      >>dim
    BGRA                >>component-order
    ubyte-components    >>component-type
    size sierpinski-img >>bitmap
    "sierpinski-triangle.png" save-graphic-image ;

MAIN: main
```

[https://i.imgur.com/wjwCrvL.png]


## FreeBASIC


```FreeBASIC
' version 06-07-2015
' compile with: fbc -s console or with: fbc -s gui

#Define black  0
#Define white RGB(255,255,255)

Dim As Integer x, y
Dim As Integer order = 9
Dim As Integer size = 2 ^ order

ScreenRes size, size, 32
Line (0,0) - (size -1, size -1), black, bf

For y = 0 To size -1
    For x = 0 To size -1
        If (x And y) = 0 Then PSet(x, y)    ' ,white
    Next
Next

' empty keyboard buffer
While Inkey <> "" : Wend
WindowTitle "Hit any key to end program"
Sleep
End
```



## gnuplot

Generating X,Y coordinates by the ternary digits of parameter t.


```gnuplot
# triangle_x(n) and triangle_y(n) return X,Y coordinates for the
# Sierpinski triangle point number n, for integer n.
triangle_x(n) = (n > 0 ? 2*triangle_x(int(n/3)) + digit_to_x(int(n)%3) : 0)
triangle_y(n) = (n > 0 ? 2*triangle_y(int(n/3)) + digit_to_y(int(n)%3) : 0)
digit_to_x(d) = (d==0 ? 0 : d==1 ? -1 : 1)
digit_to_y(d) = (d==0 ? 0 : 1)

# Plot the Sierpinski triangle to "level" many replications.
# "trange" and "samples" are chosen so the parameter t runs through
# integers t=0 to 3**level-1, inclusive.
#
level=6
set trange [0:3**level-1]
set samples 3**level
set parametric
set key off
plot triangle_x(t), triangle_y(t) with points
```



## Go

[[file:GoSierpinski.png|right|thumb|Output png]]
```go
package main

import (
    "fmt"
    "image"
    "image/color"
    "image/draw"
    "image/png"
    "os"
)

func main() {
    const order = 8
    const width = 1 << order
    const margin = 10
    bounds := image.Rect(-margin, -margin, width+2*margin, width+2*margin)
    im := image.NewGray(bounds)
    gBlack := color.Gray{0}
    gWhite := color.Gray{255}
    draw.Draw(im, bounds, image.NewUniform(gWhite), image.ZP, draw.Src)

    for y := 0; y < width; y++ {
        for x := 0; x < width; x++ {
            if x&y == 0 {
                im.SetGray(x, y, gBlack)
            }
        }
    }
    f, err := os.Create("sierpinski.png")
    if err != nil {
        fmt.Println(err)
        return
    }
    if err = png.Encode(f, im); err != nil {
        fmt.Println(err)
    }
    if err = f.Close(); err != nil {
        fmt.Println(err)
    }
}
```



## Haskell

This program uses the [http://hackage.haskell.org/package/diagrams diagrams] package to produce the Sierpinski triangle.  The package implements an embedded [http://en.wikipedia.org/wiki/EDSL#Usage_patterns DSL] for producing vector graphics.  Depending on the command-line arguments, the program can generate SVG, PNG, PDF or PostScript output.

For fun, we take advantage of Haskell's layout rules, and the operators provided by the diagrams package, to give the <tt>reduce</tt> function the shape of a triangle.  It could also be written as <tt>reduce t = t === (t ||| t)</tt>.

The command to produce the SVG output is <tt>sierpinski -o Sierpinski-Haskell.svg</tt>.

[[File:Sierpinski-Haskell.svg|thumb|Sierpinski Triangle]]

```haskell
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

triangle = eqTriangle # fc black # lw 0

reduce t =     t
              ===
           (t ||| t)

sierpinski = iterate reduce triangle

main = defaultMain $ sierpinski !! 7

```


=={{header|Icon}} and {{header|Unicon}}==
The following code is adapted from a program by Ralph Griswold that demonstrates an interesting way to draw the Sierpinski Triangle.  Given an argument of the order it will calculate the canvas size needed with margin. It will not stop you from asking for a triangle larger than you display.  For an explanation, see "Chaos and Fractals", Heinz-Otto Peitgen, Harmut Jurgens, and Dietmar Saupe, Springer-Verlag, 1992, pp. 132-134.

[[File:Sierpinski_Triangle_Unicon.PNG|thumb|Sample Output for order=8]]

```Icon
link wopen

procedure main(A)
   local width, margin, x, y

   width := 2 ^ (order := (0 < integer(\A[1])) | 8)
   wsize := width + 2 * (margin := 30 )
   WOpen("label=Sierpinski", "size="||wsize||","||wsize) |
      stop("*** cannot open window")

   every y := 0 to width - 1 do
      every x := 0 to width - 1 do
         if iand(x, y) = 0 then DrawPoint(x + margin, y + margin)

  Event()
end
```

[http://www.cs.arizona.edu/icon/library/src/gprogs/sier1.icn Original source IPL Graphics/sier1.]

=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Triangle.bas"
110 SET VIDEO MODE 1:SET VIDEO COLOR 0:SET VIDEO X 40:SET VIDEO Y 27
120 OPEN #101:"video:"
130 DISPLAY #101:AT 1 FROM 1 TO 27
140 CALL SIERP(896,180,50)
150 DEF SIERP(W,X,Y)
160   IF W>28 THEN
170     CALL SIERP(W/2,X,Y)
180     CALL SIERP(W/2,X+W/4,Y+W/2)
190     CALL SIERP(W/2,X+W/2,Y)
200   ELSE
210     PLOT X,Y;X+W/2,Y+W;X+W,Y;X,Y
220   END IF
230 END DEF
```



## J

'''Solution:'''

```j
   load 'viewmat'
   'rgb'viewmat--. |. (~:_1&|.)^:(<@#) (2^8){.1

```


or


```j

load'viewmat'
viewmat(,~,.~)^:8,1

```



## Java

'''Solution:'''

```java
import javax.swing.*;
import java.awt.*;

/**
* SierpinskyTriangle.java
* Draws a SierpinskyTriangle in a JFrame
* The order of complexity is given from command line, but
* defaults to 3
*
* @author Istarnion
*/

class SierpinskyTriangle {

	public static void main(String[] args) {
		int i = 3;		// Default to 3
		if(args.length >= 1) {
			try {
				i = Integer.parseInt(args[0]);
			}
			catch(NumberFormatException e) {
				System.out.println("Usage: 'java SierpinskyTriangle [level]'\nNow setting level to "+i);
			}
		}
		final int level = i;

		JFrame frame = new JFrame("Sierpinsky Triangle - Java");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel panel = new JPanel() {
			@Override
			public void paintComponent(Graphics g) {
				g.setColor(Color.BLACK);
				drawSierpinskyTriangle(level, 20, 20, 360, (Graphics2D)g);
			}
		};

		panel.setPreferredSize(new Dimension(400, 400));

		frame.add(panel);
		frame.pack();
		frame.setResizable(false);
		frame.setLocationRelativeTo(null);
		frame.setVisible(true);
	}

	private static void drawSierpinskyTriangle(int level, int x, int y, int size, Graphics2D g) {
		if(level <= 0) return;

		g.drawLine(x, y, x+size, y);
		g.drawLine(x, y, x, y+size);
		g.drawLine(x+size, y, x, y+size);

		drawSierpinskyTriangle(level-1, x, y, size/2, g);
		drawSierpinskyTriangle(level-1, x+size/2, y, size/2, g);
		drawSierpinskyTriangle(level-1, x, y+size/2, size/2, g);
	}
}
```



### Animated version

```java
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.geom.Path2D;
import javax.swing.*;

public class SierpinskiTriangle extends JPanel {
    private final int dim = 512;
    private final int margin = 20;

    private int limit = dim;

    public SierpinskiTriangle() {
        setPreferredSize(new Dimension(dim + 2 * margin, dim + 2 * margin));
        setBackground(Color.white);
        setForeground(Color.green.darker());

        new Timer(2000, (ActionEvent e) -> {
            limit /= 2;
            if (limit <= 2)
                limit = dim;
            repaint();
        }).start();
    }

    void drawTriangle(Graphics2D g, int x, int y, int size) {
        if (size <= limit) {
            Path2D p = new Path2D.Float();
            p.moveTo(x, y);
            p.lineTo(x + size / 2, y + size);
            p.lineTo(x - size / 2, y + size);
            g.fill(p);
        } else {
            size /= 2;
            drawTriangle(g, x, y, size);
            drawTriangle(g, x + size / 2, y + size, size);
            drawTriangle(g, x - size / 2, y + size, size);
        }
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        g.translate(margin, margin);
        drawTriangle(g, dim / 2, 0, dim);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Sierpinski Triangle");
            f.setResizable(false);
            f.add(new SierpinskiTriangle(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

;Note:
* "Order" to calculate a size of resulting plot/matrix is not used in this algorithm, Instead, construction is done in accordance to a square   m x m matrix. In our case it should be equal to a size of the square canvas.
* Change canvas setting from size "640" to "1280". You will discover that density of dots in plotted triangle is stable for this algorithm. Size of the plotted figure is constantly increasing in the S-E direction. Also, the number of all triangles in N-W triangular part of the canvas is always the same.
* So, in this case it could be called: "Sierpinski ever-expanding field of triangles".


[[File:SierpTRjs.png|200px|right|thumb|Output SierpTRjs.png]]

```html

<!-- SierpinskiTriangle.html -->
<html>
<head><title>Sierpinski Triangle Fractal</title>
<script>
// HF#1 Like in PARI/GP: return random number 0..max-1
function randgp(max) {return Math.floor(Math.random()*max)}
// HF#2 Random hex color
function randhclr() {
  return "#"+
  ("00"+randgp(256).toString(16)).slice(-2)+
  ("00"+randgp(256).toString(16)).slice(-2)+
  ("00"+randgp(256).toString(16)).slice(-2)
}
// HFJS#3: Plot any matrix mat (filled with 0,1)
function pmat01(mat, color) {
  // DCLs
  var cvs = document.getElementById('cvsId');
  var ctx = cvs.getContext("2d");
  var w = cvs.width; var h = cvs.height;
  var m = mat[0].length; var n = mat.length;
  // Cleaning canvas and setting plotting color
  ctx.fillStyle="white"; ctx.fillRect(0,0,w,h);
  ctx.fillStyle=color;
  // MAIN LOOP
  for(var i=0; i<m; i++) {
    for(var j=0; j<n; j++) {
      if(mat[i][j]==1) { ctx.fillRect(i,j,1,1)};
    }//fend j
  }//fend i
}//func end

// Prime function
// Plotting Sierpinski triangle. aev 4/9/17
// ord - order, fn - file name, ttl - plot title, clr - color
function pSierpinskiT() {
  var cvs=document.getElementById("cvsId");
  var ctx=cvs.getContext("2d");
  var w=cvs.width, h=cvs.height;
  var R=new Array(w);
  for (var i=0; i<w; i++) {R[i]=new Array(w)
    for (var j=0; j<w; j++) {R[i][j]=0}
  }
  ctx.fillStyle="white"; ctx.fillRect(0,0,w,h);
  for (var y=0; y<w; y++) {
    for (var x=0; x<w; x++) {
      if((x & y) == 0 ) {R[x][y]=1}
  }}
  pmat01(R, randhclr());
}
</script></head>
<body style="font-family: arial, helvatica, sans-serif;">
  <b>Please click to start and/or change color: </b>
  <input type="button" value=" Plot it! " onclick="pSierpinskiT();">
  <h3>Sierpinski triangle fractal</h3>
  <canvas id="cvsId" width="640" height="640" style="border: 2px inset;"></canvas>
  <!--canvas id="cvsId" width="1280" height="1280" style="border: 2px inset;"></canvas-->
</body></html>

```

```txt

Page with Sierpinski triangle fractal. Plotting color is changing randomly.
Right clicking on canvas with image allows you to save it as png-file, for example.

```



## Julia

Produces a png graphic on a transparent background. The brushstroke used for fill might need to be modified for a white background.

```julia

using Luxor

function sierpinski(txy, levelsyet)
    nxy = zeros(6)
    if levelsyet > 0
        for i in 1:6
            pos = i < 5 ? i + 2 : i - 4
            nxy[i] = (txy[i] + txy[pos]) / 2.0
        end
        sierpinski([txy[1],txy[2],nxy[1],nxy[2],nxy[5],nxy[6]], levelsyet-1)
        sierpinski([nxy[1],nxy[2],txy[3],txy[4],nxy[3],nxy[4]], levelsyet-1)
        sierpinski([nxy[5],nxy[6],nxy[3],nxy[4],txy[5],txy[6]], levelsyet-1)
    else
        poly([Point(txy[1],txy[2]),Point(txy[3],txy[4]),Point(txy[5],txy[6])], :fill ,close=true)
    end
end

Drawing(800, 800)
sierpinski([400., 100., 700., 500., 100., 500.], 7)
finish()
preview()

```



## Kotlin

'''From Java code:'''

```scala
import java.awt.*
import javax.swing.JFrame
import javax.swing.JPanel

fun main(args: Array<String>) {
    var i = 8     // Default
    if (args.any()) {
        try {
            i = args.first().toInt()
        } catch (e: NumberFormatException) {
            i = 8
            println("Usage: 'java SierpinskyTriangle [level]'\nNow setting level to $i")
        }
    }

    object : JFrame("Sierpinsky Triangle - Kotlin") {
        val panel = object : JPanel() {
            val size = 800

            init {
                preferredSize = Dimension(size, size)
            }

            public override fun paintComponent(g: Graphics) {
                g.color = Color.BLACK
                if (g is Graphics2D) {
                    g.drawSierpinskyTriangle(i, 20, 20, size - 40)
                }
            }
        }

        init {
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            add(panel)
            pack()
            isResizable = false
            setLocationRelativeTo(null)
            isVisible = true
        }
    }
}

internal fun Graphics2D.drawSierpinskyTriangle(level: Int, x: Int, y: Int, size: Int) {
    if (level > 0) {
        drawLine(x, y, x + size, y)
        drawLine(x, y, x, y + size)
        drawLine(x + size, y, x, y + size)

        drawSierpinskyTriangle(level - 1, x, y, size / 2)
        drawSierpinskyTriangle(level - 1, x + size / 2, y, size / 2)
        drawSierpinskyTriangle(level - 1, x, y + size / 2, size / 2)
    }
}
```



## Liberty BASIC

The ability of LB to handle very large integers makes the Pascal triangle method very attractive. If you alter the rem'd line you can ask it to print the last, central term...

```lb

nomainwin

open "test" for graphics_nsb_fs as #gr

#gr "trapclose quit"
#gr "down; home"
#gr "posxy cx cy"

order =10

w =cx *2: h =cy *2

dim a( h, h)  'line, col

#gr "trapclose quit"
#gr "down; home"

a( 1, 1) =1

for i = 2 to 2^order -1
    scan
    a( i, 1) =1
    a( i, i) =1
    for j = 2 to i -1
        'a(i,j)=a(i-1,j-1)+a(i-1,j)         'LB is quite capable for crunching BIG numbers
        a( i, j) =(a( i -1, j -1) +a( i -1, j)) mod 2  'but for this task, last bit is enough (and it much faster)
    next
    for j = 1 to i
        if a( i, j) mod 2 then #gr "set "; cx +j -i /2; " "; i
    next
next
#gr "flush"

wait

sub quit handle$
    close #handle$
    end
end sub

```

Up to order 10 displays on a 1080 vertical pixel screen.



## Logo

This will draw a graphical Sierpinski gasket using turtle graphics.

```logo
to sierpinski :n :length
  if :n = 0 [stop]
  repeat 3 [sierpinski :n-1 :length/2  fd :length rt 120]
end
seth 30 sierpinski 5 200
```



## Lua

```Lua
-- The argument 'tri' is a list of co-ords: {x1, y1, x2, y2, x3, y3}
function sierpinski (tri, order)
    local new, p, t = {}
    if order > 0 then
        for i = 1, #tri do
            p = i + 2
            if p > #tri then p = p - #tri end
            new[i] = (tri[i] + tri[p]) / 2
        end
        sierpinski({tri[1],tri[2],new[1],new[2],new[5],new[6]}, order-1)
        sierpinski({new[1],new[2],tri[3],tri[4],new[3],new[4]}, order-1)
        sierpinski({new[5],new[6],new[3],new[4],tri[5],tri[6]}, order-1)
    else
        love.graphics.polygon("fill", tri)
    end
end

-- Callback function used to draw on the screen every frame
function love.draw ()
    sierpinski({400, 100, 700, 500, 100, 500}, 7)
end
```

[[File:Love2D-Sierpinski.jpg]]


## Mathematica


```Mathematica
Sierpinski[n_] :=Nest[Flatten[Table[{{
       #[[i, 1]], (#[[i, 1]] + #[[i, 2]])/2, (#[[i, 1]] + #[[i, 3]])/
        2}, {(#[[i, 1]] + #[[i, 2]])/2, #[[i,
        2]], (#[[i, 2]] + #[[i, 3]])/2}, {(#[[i, 1]] + #[[i, 3]])/
        2, (#[[i, 2]] + #[[i, 3]])/2, #[[i, 3]]}}, {i, Length[#]}],
    1] &, {{{0, 0}, {1/2, 1}, {1, 0}}}, n]

Show[Graphics[{Opacity[1], Black, Map[Polygon, Sierpinski[8], 1]}, AspectRatio -> 1]]
```



```Mathematica
sierpinski[v_, 0] := Polygon@v;
sierpinski[v_, n_] := sierpinski[#, n - 1] & /@ (Mean /@ # & /@ v~Tuples~2~Partition~3);
Graphics@sierpinski[N@{{0, 0}, {1, 0}, {.5, .8}}, 3]
```



```Mathematica
sierpinski = Map[Mean, Partition[Tuples[#, 2], 3], {2}] &;
p = Nest[Join @@ sierpinski /@ # &, {{{0, 0}, {1, 0}, {.5, .8}}}, 3];
Graphics[Polygon@p]
```


[[File:MmaSierpinski.png]]


## MATLAB


### Basic Version


```MATLAB
[x, x0] = deal(cat(3, [1 0]', [-1 0]', [0 sqrt(3)]'));
for k = 1 : 6
  x = x(:,:) + x0 * 2 ^ k / 2;
end
patch('Faces', reshape(1 : 3 * 3 ^ k, 3, '')', 'Vertices', x(:,:)')
```

Fail to upload output image, use the one of PostScript:

[[File:Sierpinski-PS.png]]


### Bit Operator Version


```MATLAB
t = 0 : 2^16 - 1;
plot(t, bitand(t, bitshift(t, -8)), 'k.')
```



## Objeck


```objeck
use Game.SDL2;
use Game.Framework;

class Test {
  @framework : GameFramework;
  @colors : Color[];
  @step : Int;

  function : Main(args : String[]) ~ Nil {
    Test->New()->Run();
  }

  New() {
    @framework := GameFramework->New(GameConsts->SCREEN_WIDTH, GameConsts->SCREEN_HEIGHT, "Sierpinski Triangle");
    @framework->SetClearColor(Color->New(0,0,0));
    @colors := Color->New[1];
    @colors[0] := Color->New(178,34,34);
  }

  method : Run() ~ Nil {
    if(@framework->IsOk()) {
      e := @framework->GetEvent();

      quit := false;
      while(<>quit) {
        # process input
        while(e->Poll() <> 0) {
          if(e->GetType() = EventType->SDL_QUIT) {
            quit := true;
          };
        };

        @framework->FrameStart();
        @framework->Clear();
        Render(8, 20, 20, 450);
        @framework->Show();
        @framework->FrameEnd();
      };
    }
    else {
      "--- Error Initializing Environment ---"->ErrorLine();
      return;
    };

    leaving {
      @framework->Quit();
    };
  }

  method : Render(level : Int, x : Int, y : Int, size : Int) ~ Nil {
    if(level > -1) {
      renderer := @framework->GetRenderer();

      renderer->LineColor(x, y, x+size, y, @colors[0]);
      renderer->LineColor(x, y, x, y+size, @colors[0]);
      renderer->LineColor(x+size, y, x, y+size, @colors[0]);

      Render(level-1, x, y, size/2);
      Render(level-1, x+size/2, y, size/2);
      Render(level-1, x, y+size/2, size/2);
    };
  }
}

consts GameConsts {
  SCREEN_WIDTH := 640,
  SCREEN_HEIGHT := 480
}

```



## OCaml



```ocaml
open Graphics

let round v =
  int_of_float (floor (v +. 0.5))

let middle (x1, y1) (x2, y2) =
  ((x1 +. x2) /. 2.0,
   (y1 +. y2) /. 2.0)

let draw_line (x1, y1) (x2, y2) =
  moveto (round x1) (round y1);
  lineto (round x2) (round y2);
;;

let draw_triangle (p1, p2, p3) =
  draw_line p1 p2;
  draw_line p2 p3;
  draw_line p3 p1;
;;

let () =
  open_graph "";
  let width = float (size_x ()) in
  let height = float (size_y ()) in
  let pad = 20.0 in
  let initial_triangle =
    ( (pad, pad),
      (width -. pad, pad),
      (width /. 2.0, height -. pad) )
  in
  let rec loop step tris =
    if step <= 0 then tris else
      loop (pred step) (
        List.fold_left (fun acc (p1, p2, p3) ->
          let m1 = middle p1 p2
          and m2 = middle p2 p3
          and m3 = middle p3 p1 in
          let tri1 = (p1, m1, m3)
          and tri2 = (p2, m2, m1)
          and tri3 = (p3, m3, m2) in
          tri1 :: tri2 :: tri3 :: acc
        ) [] tris
      )
  in
  let res = loop 6 [ initial_triangle ] in
  List.iter draw_triangle res;
  ignore (read_key ())
```


run with:
 ocaml graphics.cma sierpinski.ml


## PARI/GP

[[File:SierpT9.png|right|thumb|Output SierpT9.png]]


```parigp

\\ Sierpinski triangle fractal
\\ Note: plotmat() can be found here on
\\ http://rosettacode.org/wiki/Brownian_tree#PARI.2FGP page.
\\ 6/3/16 aev
pSierpinskiT(n)={
my(sz=2^n,M=matrix(sz,sz),x,y);
for(y=1,sz, for(x=1,sz, if(!bitand(x,y),M[x,y]=1);));\\fends
plotmat(M);
}
{\\ Test:
pSierpinskiT(9); \\ SierpT9.png
}

```


```txt

> pSierpinskiT(9); \\ SierpT9.png
 *** matrix(512x512) 19682 DOTS

```



## Perl

```perl
my $levels = 6;
my $side   = 512;
my $height = get_height($side);

sub get_height { my($side) = @_; $side * sqrt(3) / 2 }

sub triangle {
    my($x1, $y1, $x2, $y2, $x3, $y3, $fill, $animate) = @_;
    my $svg;
    $svg .= qq{<polygon points="$x1,$y1 $x2,$y2 $x3,$y3"};
    $svg .= qq{ style="fill: $fill; stroke-width: 0;"} if $fill;
    $svg .= $animate
        ? qq{>\n  <animate attributeType="CSS" attributeName="opacity"\n  values="1;0;1" keyTimes="0;.5;1" dur="20s" repeatCount="indefinite" />\n</polygon>\n}
        : ' />';
    return $svg;
}

sub fractal {
    my( $x1, $y1, $x2, $y2, $x3, $y3, $r ) = @_;
    my $svg;
    $svg .= triangle( $x1, $y1, $x2, $y2, $x3, $y3 );
    return $svg unless --$r;
    my $side = abs($x3 - $x2) / 2;
    my $height = get_height($side);
    $svg .= fractal( $x1, $y1-$height*2, $x1-$side/2, $y1-3*$height, $x1+$side/2, $y1-3*$height, $r);
    $svg .= fractal( $x2, $y1, $x2-$side/2, $y1-$height, $x2+$side/2, $y1-$height, $r);
    $svg .= fractal( $x3, $y1, $x3-$side/2, $y1-$height, $x3+$side/2, $y1-$height, $r);
}

open my $fh, '>', 'run/sierpinski_triangle.svg';
print $fh <<'EOD',
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg width="100%" height="100%" version="1.1" xmlns="http://www.w3.org/2000/svg">
<defs>
  <radialGradient id="basegradient" cx="50%" cy="65%" r="50%" fx="50%" fy="65%">
    <stop offset="10%" stop-color="#ff0" />
    <stop offset="60%" stop-color="#f00" />
    <stop offset="99%" stop-color="#00f" />
  </radialGradient>
</defs>
EOD

triangle( $side/2, 0, 0, $height, $side, $height, 'url(#basegradient)' ),
triangle( $side/2, 0, 0, $height, $side, $height, '#000', 'animate' ),
'<g style="fill: #fff; stroke-width: 0;">',
fractal( $side/2, $height, $side*3/4, $height/2, $side/4, $height/2, $levels ),
'</g></svg>';
```

[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/sierpinski_triangle.svg See sierpinski_triangle] (offsite .svg image)


## Perl 6

[[File:Sierpinski-perl6.svg|thumb]]
This is a recursive solution. It is not really practical for more than 8 levels of recursion, but anything more than 7 is barely visible anyway.

```perl6
my $levels = 8;
my $side   = 512;
my $height = get_height($side);

sub get_height ($side) { $side * 3.sqrt / 2 }

sub triangle ( $x1, $y1, $x2, $y2, $x3, $y3, $fill?, $animate? ) {
    my $svg;
    $svg ~= qq{<polygon points="$x1,$y1 $x2,$y2 $x3,$y3"};
    $svg ~= qq{ style="fill: $fill; stroke-width: 0;"} if $fill;
    $svg ~= $animate
        ?? qq{>\n  <animate attributeType="CSS" attributeName="opacity"\n  values="1;0;1" keyTimes="0;.5;1" dur="20s" repeatCount="indefinite" />\n</polygon>}
        !! ' />';
    return $svg;
}

sub fractal ( $x1, $y1, $x2, $y2, $x3, $y3, $r is copy ) {
    my $svg;
    $svg ~= triangle( $x1, $y1, $x2, $y2, $x3, $y3 );
    return $svg unless --$r;
    my $side = abs($x3 - $x2) / 2;
    my $height = get_height($side);
    $svg ~= fractal( $x1, $y1-$height*2, $x1-$side/2, $y1-3*$height, $x1+$side/2, $y1-3*$height, $r);
    $svg ~= fractal( $x2, $y1, $x2-$side/2, $y1-$height, $x2+$side/2, $y1-$height, $r);
    $svg ~= fractal( $x3, $y1, $x3-$side/2, $y1-$height, $x3+$side/2, $y1-$height, $r);
}

my $fh = open('sierpinski_triangle.svg', :w) orelse .die;
$fh.print: qq:to/EOD/,
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg width="100%" height="100%" version="1.1" xmlns="http://www.w3.org/2000/svg">
<defs>
  <radialGradient id="basegradient" cx="50%" cy="65%" r="50%" fx="50%" fy="65%">
    <stop offset="10%" stop-color="#ff0" />
    <stop offset="60%" stop-color="#f00" />
    <stop offset="99%" stop-color="#00f" />
  </radialGradient>
</defs>
EOD

triangle( $side/2, 0, 0, $height, $side, $height, 'url(#basegradient)' ),
triangle( $side/2, 0, 0, $height, $side, $height, '#000', 'animate' ),
'<g style="fill: #fff; stroke-width: 0;">',
fractal( $side/2, $height, $side*3/4, $height/2, $side/4, $height/2, $levels ),
'</g></svg>';
```



## Phix

Can resize, and change the level from 1 to 12 (press +/-).
```Phix
--
-- demo\rosetta\SierpinskyTriangle.exw
--
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

procedure SierpinskyTriangle(integer level, atom x, atom y, atom w, atom h)
    atom w2 = w/2, w4 = w/4, h2 = h/2
    if level=1 then
        cdCanvasBegin(cddbuffer,CD_CLOSED_LINES)
        cdCanvasVertex(cddbuffer, x, y)
        cdCanvasVertex(cddbuffer, x+w2, y+h)
        cdCanvasVertex(cddbuffer, x+w, y)
        cdCanvasEnd(cddbuffer)
    else
        SierpinskyTriangle(level-1, x,    y,    w2, h2)
        SierpinskyTriangle(level-1, x+w4, y+h2, w2, h2)
        SierpinskyTriangle(level-1, x+w2, y,    w2, h2)
    end if
end procedure

integer level = 7

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    integer {w, h} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    SierpinskyTriangle(level, w*0.05, h*0.05, w*0.9, h*0.9)
    cdCanvasFlush(cddbuffer)
    IupSetStrAttribute(dlg, "TITLE", "Sierpinsky Triangle (level %d)",{level})
    return IUP_DEFAULT
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
    if find(c,"+-") then
        level = max(1,min(12,level+','-c))
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
    IupSetAttribute(dlg, "TITLE", "Sierpinsky Triangle")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))

    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    IupMainLoop()
    IupClose()
end procedure
main()
```



## PicoLisp

[[File:Pil_sierpinski.png|thumb|right]]
Slight modification of the [[Sierpinski_triangle#PicoLisp|text version]], requires ImageMagick's display:

```PicoLisp
(de sierpinski (N)
   (let (D '("1")  S "0")
      (do N
         (setq
            D (conc
               (mapcar '((X) (pack S X S)) D)
               (mapcar '((X) (pack X "0" X)) D) )
            S (pack S S) ) )
      D ) )

(out '(display -)
   (let Img (sierpinski 7)
      (prinl "P1")
      (prinl (length (car Img)) " " (length Img))
      (mapc prinl Img) ) )

```



## PostScript

[[File:Sierpinski-PS.png|thumb|right]]

```PostScript
%!PS

/sierp { % level ax ay bx by cx cy
    6 cpy triangle
    sierpr
} bind def

/sierpr {
    12 cpy
    10 -4 2 {
        5 1 roll exch 4 -1 roll
        add 0.5 mul 3 1 roll
        add 0.5 mul 3 -1 roll
        2 roll
    } for       % l a b c bc ac ab
    13 -1 roll dup 0 gt {
        1 sub
        dup 4 cpy 18 -2 roll sierpr
        dup 7 index 7 index 2 cpy 16 -2 roll sierpr
        9 3 roll 1 index 1 index 2 cpy 13 4 roll sierpr
    } { 13 -6 roll 7 { pop } repeat } ifelse
    triangle
} bind def

/cpy { { 5 index } repeat } bind def

/triangle {
    newpath moveto lineto lineto closepath stroke
} bind def

6 50 100 550 100 300 533 sierp
showpage
```



## Prolog

Works with SWI-Prolog and XPCE.



### Recursive version


[[File : prolog_sierpinski_3.png|thumb|right]]

[[File : prolog_sierpinski_8.png|thumb|right]]

Works up to sierpinski(13).

```Prolog
sierpinski(N) :-
	sformat(A, 'Sierpinski order ~w', [N]),
	new(D, picture(A)),
	draw_Sierpinski(D, N, point(350,50), 600),
	send(D, size, size(690,690)),
	send(D, open).

draw_Sierpinski(Window, 1, point(X, Y), Len) :-
	X1 is X - round(Len/2),
	X2 is X + round(Len/2),
	Y1 is Y + Len * sqrt(3) / 2,
	send(Window, display, new(Pa, path)),
        (
	   send(Pa, append, point(X, Y)),
	   send(Pa, append, point(X1, Y1)),
	   send(Pa, append, point(X2, Y1)),
	   send(Pa, closed, @on),
	   send(Pa, fill_pattern,  colour(@default, 0, 0, 0))
	).


draw_Sierpinski(Window, N, point(X, Y), Len) :-
	Len1 is round(Len/2),
	X1 is X - round(Len/4),
	X2 is X + round(Len/4),
	Y1 is Y + Len * sqrt(3) / 4,
	N1 is N - 1,
	draw_Sierpinski(Window, N1, point(X, Y), Len1),
	draw_Sierpinski(Window, N1, point(X1, Y1), Len1),
	draw_Sierpinski(Window, N1, point(X2, Y1), Len1).
```



### Iterative version


```Prolog
:- dynamic top/1.

sierpinski_iterate(N) :-
	retractall(top(_)),
	sformat(A, 'Sierpinski order ~w', [N]),
	new(D, picture(A)),
	draw_Sierpinski_iterate(D, N, point(550, 50)),
	send(D, open).

draw_Sierpinski_iterate(Window, N, point(X,Y)) :-
	assert(top([point(X,Y)])),
	NbTours is 2 ** (N  - 1),
	% Size is given here to preserve the "small" triangles when N is big
	Len is 10,
	forall(between(1, NbTours, _I),
	       (   retract(top(Lst)),
		   assert(top([])),
		   forall(member(P, Lst),
			  draw_Sierpinski(Window, P, Len)))).

draw_Sierpinski(Window, point(X, Y), Len) :-
	X1 is X - round(Len/2),
	X2 is X + round(Len/2),
	Y1 is Y + round(Len * sqrt(3) / 2),
	send(Window, display, new(Pa, path)),
        (
	   send(Pa, append, point(X, Y)),
	   send(Pa, append, point(X1, Y1)),
	   send(Pa, append, point(X2, Y1)),
	   send(Pa, closed, @on),
	   send(Pa, fill_pattern,  colour(@default, 0, 0, 0))
	),
	retract(top(Lst)),
	(   member(point(X1, Y1), Lst) -> select(point(X1,Y1), Lst, Lst1)
	;   Lst1 = [point(X1, Y1)|Lst]),

	(   member(point(X2, Y1), Lst1) -> select(point(X2,Y1), Lst1, Lst2)
	;   Lst2 = [point(X2, Y1)|Lst1]),

	assert(top(Lst2)).
```



## Python

```python

# likely the simplest possible version?
import turtle as t
def sier(n,length):
    if (n==0):
        return
    for i in range(3):
        sier(n-1, length/2)
        t.fd(length)
        t.rt(120)

```


[[File:SierpinskiTriangle-turtle.png|thumb|right]]

```python
#!/usr/bin/env python
##########################################################################################
# a very complicated version
# import necessary modules
# ------------------------
from numpy import *
import turtle

##########################################################################################
#	Functions defining the drawing actions
#       (used by the function DrawSierpinskiTriangle).
#	----------------------------------------------
def Left(turn, point, fwd, angle, turt):
	turt.left(angle)
	return [turn, point, fwd, angle, turt]
def Right(turn, point, fwd, angle, turt):
	turt.right(angle)
	return [turn, point, fwd, angle, turt]
def Forward(turn, point, fwd, angle, turt):
	turt.forward(fwd)
	return [turn, point, fwd, angle, turt]

```


```python
##########################################################################################
#		The drawing function
#		--------------------
#
# level		level of Sierpinski triangle (minimum value = 1)
# ss		screensize (Draws on a screen of size ss x ss. Default value = 400.)
#-----------------------------------------------------------------------------------------
def DrawSierpinskiTriangle(level, ss=400):
	# typical values
	turn = 0		# initial turn (0 to start horizontally)
	angle=60.0 		# in degrees

	# Initialize the turtle
	turtle.hideturtle()
	turtle.screensize(ss,ss)
	turtle.penup()
	turtle.degrees()

	# The starting point on the canvas
	fwd0         = float(ss)
	point=array([-fwd0/2.0, -fwd0/2.0])

	# Setting up the Lindenmayer system
	# Assuming that the triangle will be drawn in the following way:
	#	1.) Start at a point
	#	2.) Draw a straight line - the horizontal line (H)
	#	3.) Bend twice by 60 degrees to the left (--)
	#	4.) Draw a straight line - the slanted line (X)
	#	5.) Bend twice by 60 degrees to the left (--)
	#	6.) Draw a straight line - another slanted line (X)
	# 		This produces the triangle in the first level. (so the axiom to begin with is H--X--X)
	#	7.) For the next level replace each horizontal line using
	#		X->XX
	#		H -> H--X++H++X--H
	#			The lengths will be halved.


	decode    = {'-':Left, '+':Right, 'X':Forward, 'H':Forward}
	axiom     = 'H--X--X'

	# Start the drawing
	turtle.goto(point[0], point[1])
	turtle.pendown()
	turtle.hideturtle()
	turt=turtle.getpen()
	startposition=turt.clone()

	# Get the triangle in the Lindenmayer system
	fwd       = fwd0/(2.0**level)
	path      = axiom
	for i in range(0,level):
		path=path.replace('X','XX')
		path=path.replace('H','H--X++H++X--H')

	# Draw it.
	for i in path:
		[turn, point, fwd, angle, turt]=decode[i](turn, point, fwd, angle, turt)
##########################################################################################

DrawSierpinskiTriangle(5)

```



## R

Note: Find plotmat() here on RC [[User:AnatolV/Helper_Functions| R Helper Functions page]].
[[File:SierpTRo6.png|200px|right|thumb|Output SierpTRo6.png]]
[[File:SierpTRo8.png|200px|right|thumb|Output SierpTRo8.png]]

```r

## Plotting Sierpinski triangle. aev 4/1/17
## ord - order, fn - file name, ttl - plot title, clr - color
pSierpinskiT <- function(ord, fn="", ttl="", clr="navy") {
  m=640; abbr="STR"; dftt="Sierpinski triangle";
  n=2^ord; M <- matrix(c(0), ncol=n, nrow=n, byrow=TRUE);
  cat(" *** START", abbr, date(), "\n");
  if(fn=="") {pf=paste0(abbr,"o", ord)} else {pf=paste0(fn, ".png")};
  if(ttl!="") {dftt=ttl}; ttl=paste0(dftt,", order ", ord);
  cat(" *** Plot file:", pf,".png", "title:", ttl, "\n");
  for(y in 1:n) {
    for(x in 1:n) {
      if(bitwAnd(x, y)==0) {M[x,y]=1}
    ##if(bitwAnd(x, y)>0) {M[x,y]=1}   ## Try this for "reversed" ST
  }}
  plotmat(M, pf, clr, ttl);
  cat(" *** END", abbr, date(), "\n");
}
## Executing:
pSierpinskiT(6,,,"red");
pSierpinskiT(8);

```

```txt

> pSierpinskiT(6,,,"red");
 *** START STR Sat Apr 01 21:45:23 2017
 *** Plot file: STRo6 .png title: Sierpinski triangle, order 6
 *** Matrix( 64 x 64 ) 728 DOTS
 *** END STR Sat Apr 01 21:45:23 2017
> pSierpinskiT(8)
 *** START STR Sat Apr 01 21:59:06 2017
 *** Plot file: STRo8 .png title: Sierpinski triangle, order 8
 *** Matrix( 256 x 256 ) 6560 DOTS
 *** END STR Sat Apr 01 21:59:07 2017

```



## Racket

[[File : RacketSierpinski.png|thumb|right]]

```Racket

#lang racket
(require 2htdp/image)
(define (sierpinski n)
  (if (zero? n)
    (triangle 2 'solid 'red)
    (let ([t (sierpinski (- n 1))])
      (freeze (above t (beside t t))))))

```

Test:

```racket

;; the following will show the graphics if run in DrRacket
(sierpinski 8)
;; or use this to dump the image into a file, shown on the right
(require file/convertible)
(display-to-file (convert (sierpinski 8) 'png-bytes) "sierpinski.png")

```



## Ring


```ring

load "guilib.ring"

new qapp
    {
    win1 = new qwidget() {
           setwindowtitle("drawing using qpainter")
           setgeometry(100,100,500,500)
           label1 = new qlabel(win1) {
                    setgeometry(10,10,400,400)
                    settext("")
                    }
           new qpushbutton(win1) {
               setgeometry(200,400,100,30)
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
     new qpainter() {
         begin(p1)
         setpen(pen)

         order = 7
         size = pow(2,order)
         for y = 0 to size-1
             for x = 0 to size-1
                 if (x & y)=0 drawpoint(x*2,y*2) ok
             next
         next
         endpaint()
         }
         label1 { setpicture(p1) show() }

```


Output:

[[File:CalmoSoftSierpinski.jpg]]


## Ruby

[[File : sierpinski.shoes.png|thumb|right]]

```ruby
Shoes.app(:height=>540,:width=>540, :title=>"Sierpinski Triangle") do
  def triangle(slot, tri, color)
    x, y, len = tri
    slot.append do
      fill color
      shape do
        move_to(x,y)
        dx = len * Math::cos(Math::PI/3)
        dy = len * Math::sin(Math::PI/3)
        line_to(x-dx, y+dy)
        line_to(x+dx, y+dy)
        line_to(x,y)
      end
    end
  end
  @s = stack(:width => 520, :height => 520) {}
  @s.move(10,10)

  length = 512
  @triangles = [[length/2,0,length]]
  triangle(@s, @triangles[0], rgb(0,0,0))

  @n = 1
  animate(1) do
    if @n <= 7
      @triangles = @triangles.inject([]) do |sum, (x, y, len)|
        dx = len/2 * Math::cos(Math::PI/3)
        dy = len/2 * Math::sin(Math::PI/3)
        triangle(@s, [x, y+2*dy, -len/2], rgb(255,255,255))
        sum += [[x, y, len/2], [x-dx, y+dy, len/2], [x+dx, y+dy, len/2]]
      end
    end
    @n += 1
  end

  keypress do |key|
    case key
    when :control_q, "\x11" then exit
    end
  end
end
```



## Run BASIC

[[File : SierpinskiRunBasic.png|thumb|right]]

```runbasic
graphic #g, 300,300
order = 8
width = 100
w     = width * 11
dim canvas(w,w)
canvas(1,1) = 1

for x = 2 to 2^order -1
    canvas(x,1) = 1
    canvas(x,x) = 1
    for y = 2 to x -1
        canvas( x, y) = (canvas(x -1,y -1) + canvas(x -1, y)) mod 2
        if canvas(x,y) mod 2 then #g "set "; width + (order*3) + y - x / 2;" "; x
    next y
next x
render #g
#g "flush"
wait
```



## Seed7

[[File : SierpinskiSeed7.png|thumb|right]]

```seed7
$ include "seed7_05.s7i";
  include "draw.s7i";
  include "keybd.s7i";
  include "bin64.s7i";

const proc: main is func
  local
    const integer: order is 8;
    const integer: width is 1 << order;
    const integer: margin is 10;
    var integer: x is 0;
    var integer: y is 0;
  begin
    screen(width + 2 * margin, width + 2 * margin);
    clear(curr_win, white);
    KEYBOARD := GRAPH_KEYBOARD;
    for y range 0 to pred(width) do
      for x range 0 to pred(width) do
        if bin64(x) & bin64(y) = bin64(0) then
          point(margin + x, margin + y, black);
        end if;
      end for;
    end for;
    ignore(getc(KEYBOARD));
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/graphic.htm#sierpinski]


## Sidef

[[File:Sierpinski_triangle_sidef.png|200px|thumb|right]]

```ruby
func sierpinski_triangle(n) -> Array {
  var triangle = ['*']
  { |i|
    var sp = (' ' * 2**i)
    triangle = (triangle.map {|x| sp + x + sp} +
                triangle.map {|x| x + ' ' + x})
  } * n
  triangle
}

class Array {
  method to_png(scale=1, bgcolor='white', fgcolor='black') {

    static gd = require('GD::Simple')
    var width = self.max_by{.len}.len
    self.map!{|r| "%-#{width}s" % r}

    var img = gd.new(width * scale, self.len * scale)

    for i in ^self {
      for j in RangeNum(i*scale, i*scale + scale) {
        img.moveTo(0, j)
        for line in (self[i].scan(/(\s+|\S+)/)) {
          img.fgcolor(line.contains(/\S/) ? fgcolor : bgcolor)
          img.line(scale * line.len)
        }
      }
    }
    img.png
  }
}

var triangle = sierpinski_triangle(8)
var raw_png = triangle.to_png(bgcolor:'black', fgcolor:'red')
File('triangle.png').write(raw_png, :raw)
```



## Tcl

This code maintains a queue of triangles to cut out; though a stack works just as well, the observed progress is more visually pleasing when a queue is used.
```tcl
package require Tcl 8.5
package require Tk

proc mean args {expr {[::tcl::mathop::+ {*}$args] / [llength $args]}}
proc sierpinski {canv coords order} {
    $canv create poly $coords -fill black -outline {}
    set queue [list [list {*}$coords $order]]
    while {[llength $queue]} {
	lassign [lindex $queue 0] x1 y1 x2 y2 x3 y3 order
	set queue [lrange $queue 1 end]
	if {[incr order -1] < 0} continue
	set x12 [mean $x1 $x2]; set y12 [mean $y1 $y2]
	set x23 [mean $x2 $x3]; set y23 [mean $y2 $y3]
	set x31 [mean $x3 $x1]; set y31 [mean $y3 $y1]
	$canv create poly $x12 $y12 $x23 $y23 $x31 $y31 -fill white -outline {}
	update idletasks;	# So we can see progress
	lappend queue [list $x1 $y1 $x12 $y12 $x31 $y31 $order] \
	    [list $x12 $y12 $x2 $y2 $x23 $y23 $order] \
	    [list $x31 $y31 $x23 $y23 $x3 $y3 $order]
    }
}

pack [canvas .c -width 400 -height 400 -background white]
update;				# So we can see progress
sierpinski .c {200 10 390 390 10 390} 7
```


=={{header|TI-83 BASIC}}==

```ti83b
:1→X:1→Y
:Zdecimal
:Horizontal 3.1
:Vertical -4.5
:While 1
:X+1→X
:DS<(Y,1
:While 0
:X→Y
:1→X
:End
:If pxl-Test(Y-1,X) xor (pxl-Test(Y,X-1
:PxlOn(Y,X
:End
```

This could be made faster, but I just wanted to use the DS<( command


## XPL0

[[File:TriangXPL0.gif|right]]

```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
def Order=7, Size=1<<Order;
int X, Y;
[SetVid($13);                   \set 320x200 graphics video mode
for Y:= 0 to Size-1 do
    for X:= 0 to Size-1 do
        if (X&Y)=0 then Point(X, Y, 4\red\);
X:= ChIn(1);                    \wait for keystroke
SetVid(3);                      \restore normal text display
]
```



## Yabasic

[http://retrogamecoding.org/board/index.php?action=dlattach;topic=753.0;attach=1800;image Sierpinski Triangle 3D.png]

3D version.

```Yabasic
// Adpated from non recursive sierpinsky.bas for SmallBASIC 0.12.6 [B+=MGA] 2016-05-19 with demo mod 2016-05-29

//Sierpinski triangle gasket drawn with lines from any 3 given points
//                WITHOUT RECURSIVE Calls


//first a sub, given 3 points of a triangle draw the traiangle within
//from the midpoints of each line forming the outer triangle
//this is the basic Sierpinski Unit that is repeated at greater depths
//3 points is 6 arguments to function plus a depth level

xmax=800:ymax=600
open window xmax,ymax
backcolor 0,0,0
color 255,0,0
clear window

sub SierLineTri(x1, y1, x2, y2, x3, y3, maxDepth)
  local mx1, mx2, mx3, my1, my2, my3, ptcount, depth, i, X, Y
  Y = 1

  //load given set of 3 points into oa = outer triangles array, ia = inner triangles array
  ptCount = 3
  depth = 1

  dim oa(ptCount - 1, 1) //the outer points array
  oa(0, X) = x1
  oa(0, Y) = y1
  oa(1, X) = x2
  oa(1, Y) = y2
  oa(2, X) = x3
  oa(2, Y) = y3

  dim ia(3 * ptCount - 1, 1) //the inner points array
  iaIndex = 0

while(depth <= maxDepth)
  for i=0 to ptCount-1 step 3 //draw outer triangles at this level
    if depth = 1 then
      line oa(i,X),     oa(i,Y), oa(i+1,X), oa(i+1,Y)
      line oa(i+1,X), oa(i+1,Y), oa(i+2,X), oa(i+2,Y)
      line oa(i,X),     oa(i,Y), oa(i+2,X), oa(i+2,Y)
    end if

    if oa(i+1,X) < oa(i,X)   then mx1 = (oa(i,X) - oa(i+1,X))/2 + oa(i+1,X) else mx1 = (oa(i+1,X) - oa(i,X))/2 + oa(i,X) endif
    if oa(i+1,Y) < oa(i,Y)   then my1 = (oa(i,Y) - oa(i+1,Y))/2 + oa(i+1,Y) else my1 = (oa(i+1,Y) - oa(i,Y))/2 + oa(i,Y) endif
    if oa(i+2,X) < oa(i+1,X) then mx2 = (oa(i+1,X)-oa(i+2,X))/2 + oa(i+2,X) else mx2 = (oa(i+2,X)-oa(i+1,X))/2 + oa(i+1,X) endif
    if oa(i+2,Y) < oa(i+1,Y) then my2 = (oa(i+1,Y)-oa(i+2,Y))/2 + oa(i+2,Y) else my2 = (oa(i+2,Y)-oa(i+1,Y))/2 + oa(i+1,Y) endif
    if oa(i+2,X) < oa(i,X)   then mx3 = (oa(i,X) - oa(i+2,X))/2 + oa(i+2,X) else mx3 = (oa(i+2,X) - oa(i,X))/2 + oa(i,X) endif
    if oa(i+2,Y) < oa(i,Y)   then my3 = (oa(i,Y) - oa(i+2,Y))/2 + oa(i+2,Y) else my3 = (oa(i+2,Y) - oa(i,Y))/2 + oa(i,Y) endif

    //color 9 //testing
    //draw all inner triangles
    line mx1, my1, mx2, my2
    line mx2, my2, mx3, my3
    line mx1, my1, mx3, my3

    //x1, y1 with mx1, my1 and mx3, my3
    ia(iaIndex,X) = oa(i,X)
    ia(iaIndex,Y) = oa(i,Y) : iaIndex = iaIndex + 1
    ia(iaIndex,X) = mx1
    ia(iaIndex,Y) = my1     : iaIndex = iaIndex + 1
    ia(iaIndex,X) = mx3
    ia(iaIndex,Y) = my3     : iaIndex = iaIndex + 1

    //x2, y2 with mx1, my1 and mx2, my2
    ia(iaIndex,X) = oa(i+1,X)
    ia(iaIndex,Y) = oa(i+1,Y) : iaIndex = iaIndex + 1
    ia(iaIndex,X) = mx1
    ia(iaIndex,Y) = my1       : iaIndex = iaIndex + 1
    ia(iaIndex,X) = mx2
    ia(iaIndex,Y) = my2       : iaIndex = iaIndex + 1

    //x3, y3 with mx3, my3 and mx2, my2
    ia(iaIndex,X) = oa(i+2,X)
    ia(iaIndex,Y) = oa(i+2,Y) : iaIndex = iaIndex + 1
    ia(iaIndex,X) = mx2
    ia(iaIndex,Y) = my2       : iaIndex = iaIndex + 1
    ia(iaIndex,X) = mx3
    ia(iaIndex,Y) = my3       : iaIndex = iaIndex + 1

  next i

  //update and prepare for next level
  ptCount = ptCount * 3
  depth = depth + 1
  redim oa(ptCount - 1, 1 )
  for i = 0 to ptCount - 1
    oa(i, X) = ia(i, X)
    oa(i, Y) = ia(i, Y)
  next i
  redim ia(3 * ptCount - 1, 1)
  iaIndex = 0
wend
end sub

//Test Demo for the sub (NEW as 2016 - 05 - 29 !!!!!)
cx=xmax/2
cy=ymax/2
r=cy - 20
N=3
for i = 0 to 2
  color 64+42*i,64+42*i,64+42*i
  SierLineTri(cx, cy, cx+r*cos(2*pi/N*i), cy +r*sin(2*pi/N*i), cx + r*cos(2*pi/N*(i+1)), cy + r*sin(2*pi/N*(i+1)), 5)
next i

```


Simple recursive version

```Yabasic
w = 800 : h = 600
open window w, h
window origin "lb"

sub SierpinskyTriangle(level, x, y, w, h)
    local w2, w4, h2

    w2 = w/2 : w4 = w/4 : h2 = h/2
    if level=1 then
        new curve
        line to x, y
        line to x+w2, y+h
        line to x+w, y
        line to x, y
    else
        SierpinskyTriangle(level-1, x,    y,    w2, h2)
        SierpinskyTriangle(level-1, x+w4, y+h2, w2, h2)
        SierpinskyTriangle(level-1, x+w2, y,    w2, h2)
    end if
end sub

SierpinskyTriangle(7, w*0.05, h*0.05, w*0.9, h*0.9)
```



## zkl

[[File:SierpinskiTriangle.zkl.jpg|150px|thumb|right]]
Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
```zkl
const Order=8, Size=(1).shiftLeft(Order);
img:=PPM(300,300);
foreach y,x in (Size,Size){ if(x.bitAnd(y)==0) img[x,y]=0xff0000 }
img.write(File("sierpinskiTriangle.ppm","wb"));
```



