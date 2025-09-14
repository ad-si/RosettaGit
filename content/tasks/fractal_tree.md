+++
title = "Fractal tree"
description = ""
date = 2019-09-20T15:46:58Z
aliases = []
[extra]
id = 7175
[taxonomies]
categories = ["task", "Fractals"]
tags = []
languages = [
  "autohotkey",
  "basic",
  "basic256",
  "bbc_basic",
  "c",
  "ceylon",
  "clojure",
  "common_lisp",
  "cpp",
  "d",
  "easylang",
  "executing",
  "fantom",
  "freebasic",
  "frege",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "lingo",
  "logo",
  "lua",
  "netrexx",
  "ocaml",
  "ord_order_depth_c_scale_xsh_x_shift_fn_file_name",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "plotting_fractal_tree_aev_3_27_17",
  "postscript",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "recursive_ft_plotting",
  "related_tasks",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "scilab",
  "seed7",
  "sidef",
  "smalltalk",
  "svg",
  "swift",
  "tcl",
  "ttl_plot_title",
  "tuscript",
  "typescript",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Generate and draw a fractal tree.

# Draw the trunk
# At the end of the trunk, split by some angle and draw two branches
# Repeat at the end of each branch until a sufficient level of branching is reached


## Related tasks

* [[Pythagoras_tree|Pythagoras Tree]]





## AutoHotkey

[http://i.imgur.com/H7iJOde.png Image] - Link, since uploads seem to be disabled currently.
```AutoHotkey
#SingleInstance, Force
#NoEnv
SetBatchLines, -1

; Uncomment if Gdip.ahk is not in your standard library
; #Include, Gdip.ahk

FileOut		:= A_Desktop "\MyNewFile.png"
TreeColor	:= 0xff0066ff	; ARGB
TrunkWidth 	:= 10		; Pixels
TrunkLength	:= 80		; Pixels
Angle 		:= 60		; Degrees
ImageWidth 	:= 670		; Pixels
ImageHeight 	:= 450		; Pixels
Branches	:= 13
Decrease	:= 0.81

Angle := (Angle * 0.01745329252) / 2
	, Points := {}
	, Points[1, "Angle"] := 0
	, Points[1, "X"] := ImageWidth // 2
	, Points[1, "Y"] := ImageHeight - TrunkLength

if (!pToken := Gdip_Startup()) {
	MsgBox, 48, Gdiplus error!, Gdiplus failed to start. Please ensure you have Gdiplus on your system.
	ExitApp
}
OnExit, Exit

pBitmap := Gdip_CreateBitmap(ImageWidth, ImageHeight)
	, G := Gdip_GraphicsFromImage(pBitmap)
	, Gdip_SetSmoothingMode(G, 4)
	, pBrush := Gdip_BrushCreateSolid(0xff000000)
	, Gdip_FillRectangle(G, pBrush, -5, -5, ImageWidth + 10, ImageHeight + 10)
	, Gdip_DeleteBrush(pBrush)
	, pPen := Gdip_CreatePen(TreeColor, TrunkWidth/Decrease)
	, Gdip_DrawLine(G, pPen, Points.1.X, Points.1.Y, Points.1.X, ImageHeight)
	, Gdip_DeletePen(pPen)

Loop, % Branches {
	NewPoints := {}
	pPen := Gdip_CreatePen(TreeColor, TrunkWidth)
	for Each, Point in Points {
		N1 := A_Index * 2
			, N2 := (A_Index * 2) + 1
			, NewPoints[N1, "X"] := Point.X + (TrunkLength * Sin(NewPoints[N1, "Angle"] := Point.Angle - Angle))
			, NewPoints[N1, "Y"] := Point.Y - (TrunkLength * Cos(NewPoints[N1].Angle))
			, NewPoints[N2, "X"] := Point.X + (TrunkLength * Sin(NewPoints[N2, "Angle"] := Point.Angle + Angle))
			, NewPoints[N2, "Y"] := Point.Y - (TrunkLength * Cos(NewPoints[N2].Angle))
			, Gdip_DrawLine(G, pPen, Point.X, Point.Y, NewPoints[N1].X, NewPoints[N1].Y)
			, Gdip_DrawLine(G, pPen, Point.X, Point.Y, NewPoints[N2].X, NewPoints[N2].Y)
	}
	TrunkWidth *= Decrease
		, TrunkLength *= Decrease
		, Points := NewPoints
		, Gdip_DeletePen(pPen)
}

Gdip_SaveBitmapToFile(pBitmap, FileOut)
	, Gdip_DisposeImage(pBitmap)
	, Gdip_DeleteGraphics(G)
Run, % FileOut

Exit:
Gdip_Shutdown(pToken)
ExitApp
```



## BASIC


=
## BASIC256
=
[[File:Fractal tree BASIC-256.png|thumb|right|Asymmetric fractal tree image created by the BASIC-256 script]]

```basic256
graphsize 300,300

level = 12 : len =63		# initial values
x = 230: y = 285
rotation = pi/2

A1 = pi/27 : A2 = pi/8		# constants which determine shape
C1 = 0.7 : C2 = 0.85

dim xs(level+1) : dim ys(level+1)	# stacks

fastgraphics
color black
rect 0,0,graphwidth,graphheight
refresh
color green
gosub tree
refresh
imgsave "Fractal_tree_BASIC-256.png", "PNG"
end

tree:
	xs[level] = x : ys[level] = y
	gosub putline
	if level>0 then
		level = level - 1
		len = len*C1
		rotation = rotation - A1
		gosub tree
		len = len/C1*C2
		rotation = rotation + A1 + A2
		gosub tree
		rotation = rotation - A2
		len = len/C2
		level = level + 1
	end if
	x = xs[level] : y = ys[level]
	return

putline:
	yn = -sin(rotation)*len + y
	xn = cos(rotation)*len + x
	line x,y,xn,yn
	x = xn : y = yn
	return
```


=
## Run BASIC
=

```Run BASIC
 'Fractal Tree - for Run Basic - 29 Apr 2018
 'from BASIC256 - http://rosettacode.org/wiki/Fractal_tree#BASIC256
 'copy this text and go to http://www.runbasic.com

WindowWidth  = 500  'Run Basic max size 800 x 600
WindowHeight = 350
c = 255  '255 for white '0 for black

 graphic #w, WindowWidth, WindowHeight
 #w cls("black")  'black background color
 #w color(c,c,c)  'changes color to white

level = 10             ' initial values
leng = 50
x = 230: y = 325       ' initial values x = 230: y = 285
pi = 3.1415
rotation = 3.1415/2

'A1 = pi/27 : A2 = pi/8	    ' constants which determine shape
'C1 = 0.7 : C2 = 0.85       ' tree is drifted left

A1 = pi/9 : A2 = pi/9	' constants which determine shape
C1 = 0.85 : C2 = 0.85   ' Symmetrical Tree

dim xs(level+1) : dim ys(level+1)	' stacks

print : print "Welcome to the Run BASIC Fractal Tree Program"
#w color("green") 'color green
gosub [tree]
 render #w
' imgsave "Fractal_tree_BASIC-256.png", "PNG"
Print "Thank you and goodbye"
end

[tree]
	xs(level) = x : ys(level) = y
	gosub [putline]
	if level>0 then
		level = level - 1
		leng = leng*C1
		rotation = rotation - A1
		gosub [tree]
		leng = leng/C1*C2
		rotation = rotation + A1 + A2
		gosub [tree]
		rotation = rotation - A2
		leng = leng/C2
		level = level + 1
	end if
	x = xs(level) : y = ys(level)
	return

[putline]
	yn = -1*sin(rotation)*leng + y
	xn = cos(rotation)*leng + x
                #w line(x,y,xn,yn)
	x = xn : y = yn
	return
'end of code
End
```


=
## BBC BASIC
=
{{Out}}[[Image:fractal_tree_bbc.gif|right]]















```bbcbasic

      Spread = 25
      Scale = 0.76
      SizeX% = 400
      SizeY% = 300
      Depth% = 10

```

```bbcbasic

      VDU 23,22,SizeX%;SizeY%;8,16,16,128

      PROCbranch(SizeX%, 0, SizeY%/2, 90, Depth%)
      END

      DEF PROCbranch(x1, y1, size, angle, depth%)
      LOCAL x2, y2
      x2 = x1 + size * COSRAD(angle)
      y2 = y1 + size * SINRAD(angle)
      VDU 23,23,depth%;0;0;0;
      LINE x1, y1, x2, y2
      IF depth% > 0 THEN
        PROCbranch(x2, y2, size * Scale, angle - Spread, depth% - 1)
        PROCbranch(x2, y2, size * Scale, angle + Spread, depth% - 1)
      ENDIF
      ENDPROC
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Tree.bas"
110 OPTION ANGLE DEGREES
120 GRAPHICS HIRES 2
130 SET PALETTE 0,170
140 PLOT 640,10;ANGLE 90;
150 CALL TREE(200)
160 DEF TREE(N)
170   IF N<24 THEN EXIT DEF
180   PLOT FORWARD N;RIGHT 25;
190   CALL TREE(N*.75)
200   PLOT LEFT 50;
210   CALL TREE(N*.75)
220   PLOT RIGHT 25,BACK N,
230 END DEF
```



## C

```c
#include <SDL/SDL.h>
#ifdef WITH_CAIRO
#include <cairo.h>
#else
#include <SDL/sge.h>
#endif
#include <cairo.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#ifdef WITH_CAIRO
#define PI 3.1415926535
#endif

#define SIZE           800   // determines size of window
#define SCALE          5     // determines how quickly branches shrink (higher value means faster shrinking)
#define BRANCHES       14    // number of branches
#define ROTATION_SCALE 0.75  // determines how slowly the angle between branches shrinks (higher value means slower shrinking)
#define INITIAL_LENGTH 50    // length of first branch

double rand_fl(){
  return (double)rand() / (double)RAND_MAX;
}

void draw_tree(SDL_Surface * surface, double offsetx, double offsety,
               double directionx, double directiony, double size,
               double rotation, int depth) {
#ifdef WITH_CAIRO
  cairo_surface_t *surf = cairo_image_surface_create_for_data( surface->pixels,
                                                               CAIRO_FORMAT_RGB24,
							       surface->w, surface->h,
							       surface->pitch );
  cairo_t *ct = cairo_create(surf);

  cairo_set_line_width(ct, 1);
  cairo_set_source_rgba(ct, 0,0,0,1);
  cairo_move_to(ct, (int)offsetx, (int)offsety);
  cairo_line_to(ct, (int)(offsetx + directionx * size), (int)(offsety + directiony * size));
  cairo_stroke(ct);
#else
  sge_AALine(surface,
      (int)offsetx, (int)offsety,
      (int)(offsetx + directionx * size), (int)(offsety + directiony * size),
      SDL_MapRGB(surface->format, 0, 0, 0));
#endif
  if (depth > 0){
    // draw left branch
    draw_tree(surface,
        offsetx + directionx * size,
        offsety + directiony * size,
        directionx * cos(rotation) + directiony * sin(rotation),
        directionx * -sin(rotation) + directiony * cos(rotation),
        size * rand_fl() / SCALE + size * (SCALE - 1) / SCALE,
        rotation * ROTATION_SCALE,
        depth - 1);

    // draw right branch
    draw_tree(surface,
        offsetx + directionx * size,
        offsety + directiony * size,
        directionx * cos(-rotation) + directiony * sin(-rotation),
        directionx * -sin(-rotation) + directiony * cos(-rotation),
        size * rand_fl() / SCALE + size * (SCALE - 1) / SCALE,
        rotation * ROTATION_SCALE,
        depth - 1);
  }
}

void render(SDL_Surface * surface){
  SDL_FillRect(surface, NULL, SDL_MapRGB(surface->format, 255, 255, 255));
  draw_tree(surface,
      surface->w / 2.0,
      surface->h - 10.0,
      0.0, -1.0,
      INITIAL_LENGTH,
      PI / 8,
      BRANCHES);
  SDL_UpdateRect(surface, 0, 0, 0, 0);
}

int main(){
  SDL_Surface * screen;
  SDL_Event evt;

  SDL_Init(SDL_INIT_VIDEO);

  srand((unsigned)time(NULL));

  screen = SDL_SetVideoMode(SIZE, SIZE, 32, SDL_HWSURFACE);

  render(screen);
  while(1){
    if (SDL_PollEvent(&evt)){
      if(evt.type == SDL_QUIT) break;
    }
    SDL_Delay(1);
  }
  SDL_Quit();
  return 0;
}
```




## C++

[[File:fracTree_cpp.png|320px]]

```cpp

#include <windows.h>
#include <string>
#include <math.h>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const float PI = 3.1415926536f;

//--------------------------------------------------------------------------------------------------
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
	void		*pBits;
	ZeroMemory( &bi, sizeof( bi ) );
	bi.bmiHeader.biSize	   = sizeof( bi.bmiHeader );
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

    void setPenColor( DWORD clr )
    {
	if( pen ) DeleteObject( pen );
	pen = CreatePen( PS_SOLID, 1, clr );
	SelectObject( hdc, pen );
    }

    void saveBitmap( string path )
    {
	BITMAPFILEHEADER	fileheader;
	BITMAPINFO			infoheader;
	BITMAP				bitmap;
	DWORD*				dwpBits;
	DWORD				wb;
	HANDLE				file;

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

	file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
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
    int     width, height;
};
//--------------------------------------------------------------------------------------------------
class vector2
{
public:
    vector2() { x = y = 0; }
    vector2( int a, int b ) { x = a; y = b; }
    void set( int a, int b ) { x = a; y = b; }
    void rotate( float angle_r )
    {
	float _x = static_cast<float>( x ),
	      _y = static_cast<float>( y ),
	       s = sinf( angle_r ),
	       c = cosf( angle_r ),
	       a = _x * c - _y * s,
	       b = _x * s + _y * c;

	x = static_cast<int>( a );
	y = static_cast<int>( b );
    }

    int x, y;
};
//--------------------------------------------------------------------------------------------------
class fractalTree
{
public:
    fractalTree()		      { _ang = DegToRadian( 24.0f ); }
    float DegToRadian( float degree ) { return degree * ( PI / 180.0f ); }

    void create( myBitmap* bmp )
    {
	_bmp = bmp;
	float line_len = 130.0f;

	vector2 sp( _bmp->getWidth() / 2, _bmp->getHeight() - 1 );
	MoveToEx( _bmp->getDC(), sp.x, sp.y, NULL );
	sp.y -= static_cast<int>( line_len );
	LineTo( _bmp->getDC(), sp.x, sp.y);

	drawRL( &sp, line_len, 0, true );
	drawRL( &sp, line_len, 0, false );
    }

private:
    void drawRL( vector2* sp, float line_len, float a, bool rg )
    {
	line_len *= .75f;
	if( line_len < 2.0f ) return;

	MoveToEx( _bmp->getDC(), sp->x, sp->y, NULL );
	vector2 r( 0, static_cast<int>( line_len ) );

        if( rg ) a -= _ang;
        else a += _ang;

	r.rotate( a );
	r.x += sp->x; r.y = sp->y - r.y;

	LineTo( _bmp->getDC(), r.x, r.y );

	drawRL( &r, line_len, a, true );
	drawRL( &r, line_len, a, false );
    }

    myBitmap* _bmp;
    float     _ang;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );

    myBitmap bmp;
    bmp.create( 640, 512 );
    bmp.setPenColor( RGB( 255, 255, 0 ) );

    fractalTree tree;
    tree.create( &bmp );

    BitBlt( GetDC( GetConsoleWindow() ), 0, 20, 648, 512, bmp.getDC(), 0, 0, SRCCOPY );

    bmp.saveBitmap( "f://rc//fracTree.bmp" );

    system( "pause" );

    return 0;
}
//--------------------------------------------------------------------------------------------------

```



## Ceylon

Be sure to import java.desktop and ceylon.numeric in your module.ceylon file.

```ceylon
import javax.swing {

	JFrame { exitOnClose }
}
import java.awt {

	Color { white, black },
	Graphics
}
import ceylon.numeric.float {

	cos,
	toRadians,
	sin
}

shared void run() {

    value fractalTree = object extends JFrame("fractal tree") {

        background = black;
        setBounds(100, 100, 800, 600);
        resizable = false;
        defaultCloseOperation = exitOnClose;

        shared actual void paint(Graphics g) {

            void drawTree(Integer x1, Integer y1, Float angle, Integer depth) {
                if (depth <= 0) {
                    return;
                }
                value x2 = x1 + (cos(toRadians(angle)) * depth * 10.0).integer;
                value y2 = y1 + (sin(toRadians(angle)) * depth * 10.0).integer;
                g.drawLine(x1, y1, x2, y2);
                drawTree(x2, y2, angle - 20, depth - 1);
                drawTree(x2, y2, angle + 20, depth - 1);
            }

            g.color = white;
            drawTree(400, 500, -90.0, 9);
        }
    };

    fractalTree.visible = true;
}
```



## Clojure

```Clojure
(import '[java.awt Color Graphics]
	'javax.swing.JFrame)

(defn deg-to-radian [deg] (* deg Math/PI 1/180))
(defn cos-deg [angle] (Math/cos (deg-to-radian angle)))
(defn sin-deg [angle] (Math/sin (deg-to-radian angle)))

(defn draw-tree [^Graphics g, x y angle depth]
  (when (pos? depth)
    (let [x2 (+ x (int (* depth 10 (cos-deg angle))))
	  y2 (+ y (int (* depth 10 (sin-deg angle))))]
      (.drawLine g x y x2 y2)
      (draw-tree g x2 y2 (- angle 20) (dec depth))
      (recur     g x2 y2 (+ angle 20) (dec depth)))))

(defn fractal-tree [depth]
  (doto (proxy [JFrame] []
	  (paint [g]
		 (.setColor g Color/BLACK)
		 (draw-tree g 400 500 -90 depth)))
    (.setBounds 100 100 800 600)
    (.setResizable false)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.show)))

(fractal-tree 9)
```


## Common Lisp

```lisp
;; (require :lispbuilder-sdl)

(defun deg-to-radian (deg)
  "converts degrees to radians"
  (* deg pi 1/180))

(defun cos-deg (angle)
  "returns cosin of the angle expressed in degress"
  (cos (deg-to-radian angle)))

(defun sin-deg (angle)
  "returns sin of the angle expressed in degress"
  (sin (deg-to-radian angle)))

(defun draw-tree (surface x y angle depth)
  "draws a branch of the tree on the sdl-surface"
  (when (plusp depth)
    (let ((x2 (+ x (round (* depth 10 (cos-deg angle)))))
	  (y2 (+ y (round (* depth 10 (sin-deg angle))))))
      (sdl:draw-line-* x y x2 y2 :surface surface :color sdl:*green*)
      (draw-tree surface x2 y2 (- angle 20) (1- depth))
      (draw-tree surface x2 y2 (+ angle 20) (1- depth)))))

(defun fractal-tree (depth)
  "shows a window with a fractal tree"
  (sdl:with-init ()
    (sdl:window 800 600 :title-caption "fractal-tree")
    (sdl:clear-display sdl:*black*)
    (draw-tree sdl:*default-surface* 400 500 -90 depth)
    (sdl:update-display)
    (sdl:with-events ()
      (:video-expose-event ()
			   (sdl:update-display))
      (:quit-event ()
		   t))))

(fractal-tree 9)

```


## D


### SVG Version

```d
import std.stdio, std.math;

enum width = 1000, height = 1000; // Image dimension.
enum length = 400;                // Trunk size.
enum scale = 6.0 / 10;            // Branch scale relative to trunk.

void tree(in double x, in double y, in double length, in double angle) {
    if (length < 1)
        return;
    immutable x2 = x + length * angle.cos;
    immutable y2 = y + length * angle.sin;
    writefln("<line x1='%f' y1='%f' x2='%f' y2='%f' " ~
             "style='stroke:black;stroke-width:1'/>", x, y, x2, y2);
    tree(x2, y2, length * scale, angle + PI / 5);
    tree(x2, y2, length * scale, angle - PI / 5);
}

void main() {
    "<svg width='100%' height='100%' version='1.1'
     xmlns='http://www.w3.org/2000/svg'>".writeln;
    tree(width / 2.0, height, length, 3 * PI / 2);
    "</svg>".writeln;
}
```



### Turtle Version

This uses the turtle module from the Dragon Curve task, and the module from the Grayscale Image task.
```d
import grayscale_image, turtle;

void tree(Color)(Image!Color img, ref Turtle t, in uint depth,
                 in real step, in real scale, in real angle) {
    if (depth == 0) return;
    t.forward(img, step);
    t.right(angle);
    img.tree(t, depth - 1, step * scale, scale, angle);
    t.left(2 * angle);
    img.tree(t, depth - 1, step * scale, scale, angle);
    t.right(angle);
    t.forward(img, -step);
}

void main() {
    auto img = new Image!Gray(330, 300);
    auto t = Turtle(165, 270, -90);
    img.tree(t, 10, 80, 0.7, 30);
    img.savePGM("fractal_tree.pgm");
}
```



### Alternative version

Using DFL.

```d
import dfl.all;
import std.math;

class FractalTree: Form {

    private immutable DEG_TO_RAD = PI / 180.0;

    this() {
        width = 600;
        height = 500;
        text = "Fractal Tree";
        backColor = Color(0xFF, 0xFF, 0xFF);
        startPosition = FormStartPosition.CENTER_SCREEN;
        formBorderStyle = FormBorderStyle.FIXED_DIALOG;
        maximizeBox = false;
    }

    private void drawTree(Graphics g, Pen p, int x1, int y1, double angle, int depth) {
        if (depth == 0) return;
        int x2 = x1 + cast(int) (cos(angle * DEG_TO_RAD) * depth * 10.0);
        int y2 = y1 + cast(int) (sin(angle * DEG_TO_RAD) * depth * 10.0);
        g.drawLine(p, x1, y1, x2, y2);
        drawTree(g, p, x2, y2, angle - 20, depth - 1);
        drawTree(g, p, x2, y2, angle + 20, depth - 1);
    }

    protected override void onPaint(PaintEventArgs ea){
        super.onPaint(ea);
        Pen p = new Pen(Color(0, 0xAA, 0));
        drawTree(ea.graphics, p, 300, 450, -90, 9);
    }
}

int main() {
    int result = 0;
    try {
        Application.run(new FractalTree);
    } catch(Exception e) {
        msgBox(e.msg, "Fatal Error", MsgBoxButtons.OK, MsgBoxIcon.ERROR);
        result = 1;
    }
    return result;
}
```


## EasyLang


[https://easylang.online/apps/fractal-tree.html Run it]

<lang>floatvars
func tree x y angle depth . .
  if depth > 0
    linewidth depth * 0.4
    xn = x + cos angle * depth * 1.4
    yn = y + sin angle * depth * 1.4
    move x y
    line xn yn
    call tree xn yn angle - 20 depth - 1
    call tree xn yn angle + 20 depth - 1
  .
.
call tree 50 90 -90 10
```


=={{header|F_Sharp|F#}}==
```fsharp
let (cos, sin, pi) = System.Math.Cos, System.Math.Sin, System.Math.PI

let (width, height) = 1000., 1000. // image dimension
let scale = 6./10.                 // branch scale relative to trunk
let length = 400.                  // trunk size

let rec tree x y length angle =
    if length >= 1. then
        let (x2, y2) = x + length * (cos angle),  y + length * (sin angle)
        printfn "<line x1='%f' y1='%f' x2='%f' y2='%f' style='stroke:rgb(0,0,0);stroke-width:1'/>"
            x y x2 y2
        tree x2 y2 (length*scale) (angle + pi/5.)
        tree x2 y2 (length*scale) (angle - pi/5.)

printfn "<?xml version='1.0' encoding='utf-8' standalone='no'?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'
'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>
<svg width='100%%' height='100%%' version='1.1'
xmlns='http://www.w3.org/2000/svg'>"
tree (width/2.) height length (3.*pi/2.)
printfn "</svg>"
```



## Fantom


```fantom

using fwt
using gfx

class FractalCanvas : Canvas
{
  new make () : super() {}

  Void drawTree (Graphics g, Int x1, Int y1, Int angle, Int depth)
  {
    if (depth == 0) return
    Int x2 := x1 + (angle.toFloat.toRadians.cos * depth * 10.0).toInt;
    Int y2 := y1 + (angle.toFloat.toRadians.sin * depth * 10.0).toInt;
    g.drawLine(x1, y1, x2, y2);
    drawTree(g, x2, y2, angle - 20, depth - 1);
    drawTree(g, x2, y2, angle + 20, depth - 1);
  }

  override Void onPaint (Graphics g)
  {
    drawTree (g, 400, 500, -90, 9)
  }
}

class FractalTree
{
  public static Void main ()
  {
    Window
    {
      title = "Fractal Tree"
      size = Size(800, 600)
      FractalCanvas(),
    }.open
  }
}

```


## FreeBASIC

```freebasic
' version 17-03-2017
' compile with: fbc -s gui

Const As Double deg2rad = Atn(1) / 45
Dim Shared As Double scale = 0.76
Dim Shared As Double spread = 25 * deg2rad ' convert degree's to rad's

Sub branch(x1 As ULong, y1 As ULong, size As ULong, angle As Double, depth As ULong)

    Dim As ULong x2, y2

    x2 = x1 + size * Cos(angle)
    y2 = y1 + size * Sin(angle)

    Line (x1,y1) - (x2,y2), 2  ' palette color green
    If depth > 0 Then
        branch(x2, y2, size * scale, angle - spread, depth -1)
        branch(x2, y2, size * scale, angle + spread, depth -1)
    End If

End Sub

' ------=< MAIN >=-----

Dim As Double angle = -90 * deg2rad ' make sure that the tree grows up
Dim As ULong  SizeX = 800
Dim As ULong  SizeY = SizeX * 3 \ 4
Dim As Double  size = SizeY \ 4
Dim As ULong  depth = 11

ScreenRes SizeX, SizeY, 8
WindowTitle ("Fractal Tree")

branch(SizeX\2, SizeY, size, angle, depth)

' empty keyboard buffer
While InKey <> "" : Wend
windowtitle ("Fractal Tree, hit any key to end program")
Sleep
End
```



## Frege


```frege
module FractalTree where

import Java.IO
import Prelude.Math

data AffineTransform = native java.awt.geom.AffineTransform where
  native new :: () -> STMutable s AffineTransform
  native clone :: Mutable s AffineTransform -> STMutable s AffineTransform
  native rotate :: Mutable s AffineTransform -> Double -> ST s ()
  native scale :: Mutable s AffineTransform -> Double -> Double -> ST s ()
  native translate :: Mutable s AffineTransform -> Double -> Double -> ST s ()

data BufferedImage = native java.awt.image.BufferedImage where
  pure native type_3byte_bgr "java.awt.image.BufferedImage.TYPE_3BYTE_BGR" :: Int
  native new :: Int -> Int -> Int -> STMutable s BufferedImage
  native createGraphics :: Mutable s BufferedImage -> STMutable s Graphics2D

data Color = pure native java.awt.Color where
  pure native black "java.awt.Color.black" :: Color
  pure native green "java.awt.Color.green" :: Color
  pure native white "java.awt.Color.white" :: Color
  pure native new :: Int -> Color

data BasicStroke = pure native java.awt.BasicStroke where
  pure native new :: Float -> BasicStroke

data RenderingHints = native java.awt.RenderingHints where
  pure native key_antialiasing "java.awt.RenderingHints.KEY_ANTIALIASING" :: RenderingHints_Key
  pure native value_antialias_on "java.awt.RenderingHints.VALUE_ANTIALIAS_ON" :: Object

data RenderingHints_Key = pure native java.awt.RenderingHints.Key

data Graphics2D = native java.awt.Graphics2D where
  native drawLine :: Mutable s Graphics2D -> Int -> Int -> Int -> Int -> ST s ()
  native drawOval :: Mutable s Graphics2D -> Int -> Int -> Int -> Int -> ST s ()
  native fillRect :: Mutable s Graphics2D -> Int -> Int -> Int -> Int -> ST s ()
  native setColor :: Mutable s Graphics2D -> Color -> ST s ()
  native setRenderingHint :: Mutable s Graphics2D -> RenderingHints_Key -> Object -> ST s ()
  native setStroke :: Mutable s Graphics2D -> BasicStroke -> ST s ()
  native setTransform :: Mutable s Graphics2D -> Mutable s AffineTransform -> ST s ()

data ImageIO = mutable native javax.imageio.ImageIO where
  native write "javax.imageio.ImageIO.write" :: MutableIO BufferedImage -> String -> MutableIO File -> IO Bool throws IOException

drawTree :: Mutable s Graphics2D -> Mutable s AffineTransform -> Int -> ST s ()
drawTree g t i = do
  let len = 10 -- ratio of length to thickness
      shrink = 0.75
      angle = 0.3 -- radians
      i' = i - 1
  g.setTransform t
  g.drawLine 0 0 0 len
  when (i' > 0) $ do
    t.translate 0 (fromIntegral len)
    t.scale shrink shrink
    rt <- t.clone
    t.rotate angle
    rt.rotate (-angle)
    drawTree g t i'
    drawTree g rt i'

main = do
  let width = 900
      height = 800
      initScale = 20
      halfWidth = fromIntegral width / 2
  buffy <- BufferedImage.new width height BufferedImage.type_3byte_bgr
  g <- buffy.createGraphics
  g.setRenderingHint RenderingHints.key_antialiasing RenderingHints.value_antialias_on
  g.setColor Color.black
  g.fillRect 0 0 width height
  g.setColor Color.green
  t <- AffineTransform.new ()
  t.translate halfWidth (fromIntegral height)
  t.scale initScale initScale
  t.rotate pi
  drawTree g t 16
  f <- File.new "FractalTreeFrege.png"
  void $ ImageIO.write buffy "png" f
```


Output is [http://funwithsoftware.org/images/2016-FractalTreeFrege.png here] due to [[User talk:Short Circuit#Is file uploading blocked forever?|Is file uploading blocked forever?]]


## Go

[[file:GoFtree.png|right|thumb|png converted from output ppm]]

```go
package main

// Files required to build supporting package raster are found in:
// * Bitmap
// * Grayscale image
// * Xiaolin Wu's line algorithm
// * Write a PPM file

import (
    "math"
    "raster"
)

const (
    width  = 400
    height = 300
    depth  = 8
    angle  = 12
    length = 50
    frac   = .8
)

func main() {
    g := raster.NewGrmap(width, height)
    ftree(g, width/2, height*9/10, length, 0, depth)
    g.Bitmap().WritePpmFile("ftree.ppm")
}

func ftree(g *raster.Grmap, x, y, distance, direction float64, depth int) {
    x2 := x + distance*math.Sin(direction*math.Pi/180)
    y2 := y - distance*math.Cos(direction*math.Pi/180)
    g.AaLine(x, y, x2, y2)
    if depth > 0 {
        ftree(g, x2, y2, distance*frac, direction-angle, depth-1)
        ftree(g, x2, y2, distance*frac, direction+angle, depth-1)
    }
}
```



## Haskell

An elegant yet universal monoidal solution.
```haskell
import Graphics.Gloss

type Model = [Picture -> Picture]

fractal :: Int -> Model -> Picture -> Picture
fractal n model pict = pictures $ take n $ iterate (mconcat model) pict

tree1 _ = fractal 10 branches $ Line [(0,0),(0,100)]
  where branches = [ Translate 0 100 . Scale 0.75 0.75 . Rotate 30
                   , Translate 0 100 . Scale 0.5 0.5 . Rotate (-30) ]

main = animate (InWindow "Tree" (800, 800) (0, 0)) white $ tree1 . (* 60)
```


The solution gives rise to a variety of fractal geometric structures. Each one can be used by substituting <code>tree1</code> in the <code>main</code> function by the desired one.

```haskell
--animated tree
tree2 t = fractal 8 branches $ Line [(0,0),(0,100)]
  where branches = [ Translate 0 100 . Scale 0.75 0.75 . Rotate t
                   , Translate 0 100 . Scale 0.6 0.6 . Rotate 0
                   , Translate 0 100 . Scale 0.5 0.5 . Rotate (-2*t) ]

--animated fractal clock
circles t = fractal 10 model $ Circle 100
  where model = [ Translate 0 50 . Scale 0.5 0.5 . Rotate t
                , Translate 0 (-50) . Scale 0.5 0.5 . Rotate (-2*t) ]

--Pythagoras tree
pithagor _ = fractal 10 model $ rectangleWire 100 100
  where model = [ Translate 50 100 . Scale s s . Rotate 45
                , Translate (-50) 100 . Scale s s . Rotate (-45)]
        s = 1/sqrt 2

--Sierpinski pentagon
pentaflake _ = fractal 5 model $ pentagon
  where model =  map copy [0,72..288]
        copy a = Scale s s . Rotate a . Translate 0 x
        pentagon = Line [ (sin a, cos a) | a <- [0,2*pi/5..2*pi] ]
        x = 2*cos(pi/5)
        s = 1/(1+x)
```


'''Alternative solution'''

Using the method of the J contribution.

```haskell
import Graphics.HGL.Window
import Graphics.HGL.Run
import Control.Arrow
import Control.Monad
import Data.List

enumBase :: Int -> Int -> [[Int]]
enumBase n = mapM (enumFromTo 0). replicate n. pred

psPlus (a,b) (p,q) = (a+p, b+q)

toInt :: Double -> Int
toInt = fromIntegral.round

intPoint = toInt *** toInt

pts n =
  map (map (intPoint.psPlus (100,0)). ((0,300):). scanl1 psPlus. ((r,300):). zipWith (\h a -> (h*cos a, h*sin a)) rs) hs
  where
    [r,h,sr,sh] = [50, pi/5, 0.9, 0.75]
    rs   = take n $ map (r*) $ iterate(*sr) sr
    lhs  = map (map (((-1)**).fromIntegral)) $ enumBase n 2
    rhs  = take n $ map (h*) $ iterate(*sh) 1
    hs   = map (scanl1 (+). zipWith (*)rhs) lhs

fractalTree :: Int -> IO ()
fractalTree n =
   runWindow "Fractal Tree" (500,600)
    (\w -> setGraphic w (overGraphics ( map polyline $ pts (n-1))) >> getKey w)

main = fractalTree 10
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
WOpen("size=800,600", "bg=black", "fg=white") | stop("*** cannot open window")
drawtree(400,500,-90,9)
WDone()
end

link WOpen

procedure drawtree(x,y,angle,depth)
if depth > 0 then {
   x2 := integer(x + cos(dtor(angle)) * depth * 10)
   y2 := integer(y + sin(dtor(angle)) * depth * 10)
   DrawLine(x,y,x2,y2)
   drawtree(x2,y2,angle-20, depth-1)
   drawtree(x2,y2,angle+20, depth-1)
   }
return
end
```


[http://www.cs.arizona.edu/icon/library/src/gprocs/WOpen.icn WOpen provides graphics I/O]

## J



```j
require'gl2'

L0=: 50           NB. initial length
A0=: 1r8p1        NB. initial angle: pi divided by 8
dL=: 0.9          NB. shrink factor for length
dA=: 0.75         NB. shrink factor for angle
N=: 14            NB. number of branches

L=: L0*dL^1+i.N  NB. lengths of line segments

NB. relative angles of successive line segments
A=: A0*(dA^i.N) +/\@:*("1) _1 ^ #:i.2 ^ N

NB. end points for each line segment
P=: 0 0+/\@,"2 +.*.inv (L0,0),"2 L,"0"1 A

P_C_paint=: gllines_jgl2_ bind (10 + ,/"2 P-"1<./,/P)
wd 0 :0
 pc P closeok;
 xywh 0 0 250 300;
 cc C isigraph rightmove bottommove;
 pas 0 0;
 pshow;
)
```


See the [[Talk:Fractal tree#J Explanation|talk page]] for some implementation notes.


## Java

```java
import java.awt.Color;
import java.awt.Graphics;
import javax.swing.JFrame;

public class FractalTree extends JFrame {

    public FractalTree() {
        super("Fractal Tree");
        setBounds(100, 100, 800, 600);
        setResizable(false);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
    }

    private void drawTree(Graphics g, int x1, int y1, double angle, int depth) {
        if (depth == 0) return;
        int x2 = x1 + (int) (Math.cos(Math.toRadians(angle)) * depth * 10.0);
        int y2 = y1 + (int) (Math.sin(Math.toRadians(angle)) * depth * 10.0);
        g.drawLine(x1, y1, x2, y2);
        drawTree(g, x2, y2, angle - 20, depth - 1);
        drawTree(g, x2, y2, angle + 20, depth - 1);
    }

    @Override
    public void paint(Graphics g) {
        g.setColor(Color.BLACK);
        drawTree(g, 400, 500, -90, 9);
    }

    public static void main(String[] args) {
        new FractalTree().setVisible(true);
    }
}
```



## JavaScript

Implementation using HTML5 canvas element to draw tree structure.

```JavaScript><html

<body>
<canvas id="canvas" width="600" height="500"></canvas>

<script type="text/javascript">
var elem = document.getElementById('canvas');
var context = elem.getContext('2d');

context.fillStyle = '#C0C0C0';
context.lineWidth = 1;

var deg_to_rad = Math.PI / 180.0;
var depth = 9;

function drawLine(x1, y1, x2, y2, brightness){
  context.moveTo(x1, y1);
  context.lineTo(x2, y2);
}

function drawTree(x1, y1, angle, depth){
  if (depth !== 0){
    var x2 = x1 + (Math.cos(angle * deg_to_rad) * depth * 10.0);
    var y2 = y1 + (Math.sin(angle * deg_to_rad) * depth * 10.0);
    drawLine(x1, y1, x2, y2, depth);
    drawTree(x2, y2, angle - 20, depth - 1);
    drawTree(x2, y2, angle + 20, depth - 1);
  }
}

context.beginPath();
drawTree(300, 500, -90, depth);
context.closePath();
context.stroke();
</script>

</body>
</html>
```



## jq

The following generates SVG, which can be viewed by following the link below.

```jq
# width and height define the outer dimensions;
# len defines the trunk size;
# scale defines the branch length relative to the trunk;
def main(width; height; len; scale):

  def PI: (1|atan)*4;

  def precision(n):
    def pow(k): . as $in | reduce range(0;k) as $i (1; .*$in);
    if . < 0 then - (-. | precision(n))
    else
      (10|pow(n)) as $power
    | (. * 10 * $power) | floor as $x | ($x % 10) as $r
    | ((if $r < 5 then $x else $x + 5 end) / 10 | floor) / $power
    end;

  def p2: precision(2);

  def tree(x; y; len; angle):
    if len < 1 then empty
    else
      (x + len * (angle|cos)) as $x2
    | (y + len * (angle|sin)) as $y2
    | (if len < 10 then 1 else 2 end) as $swidth
    | (if len < 10 then "blue" else "black" end) as $stroke
    | "<line x1='\(x|p2)' y1='\(y|p2)' x2='\($x2|p2)' y2='\($y2|p2)' style='stroke:\($stroke); stroke-width:\($swidth)'/>",
      tree($x2; $y2; len * scale; angle + PI / 5),
      tree($x2; $y2; len * scale; angle - PI / 5)
    end
  ;

  "<svg width='100%' height='100%' version='1.1'
        xmlns='http://www.w3.org/2000/svg'>",
        tree(width / 2; height; len; 3 * PI / 2),
  "</svg>"
;

main(1000; 1000; 400; 6/10)
```

$ jq -r -n -r -f Fractal_tree_svg.jq > Fractal_tree.svg

[https://drive.google.com/file/d/0BwMI1gZaY2-MWEI4d1kxNHZ4cGs/view?usp=sharing Fractal_tree.svg]


## Julia

```julia

const width = height = 1000.0
const trunklength = 400.0
const scalefactor = 0.6
const startingangle = 1.5 * pi
const deltaangle = 0.2 * pi

function tree(fh, x, y, len, theta)
   if len >= 1.0
       x2 = x + len * cos(theta)
       y2 = y + len * sin(theta)
       write(fh, "<line x1='$x' y1='$y' x2='$x2' y2='$y2' style='stroke:rgb(0,0,0);stroke-width:1'/>\n")
       tree(fh, x2, y2, len * scalefactor, theta + deltaangle)
       tree(fh, x2, y2, len * scalefactor, theta - deltaangle)
    end
end

outsvg = open("tree.svg", "w")
write(outsvg,
    """<?xml version='1.0' encoding='utf-8' standalone='no'?>
    <!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'
    'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>
    <svg width='100%%' height='100%%' version='1.1'
    xmlns='http://www.w3.org/2000/svg'>\n""")

tree(outsvg, 0.5 * width, height, trunklength, startingangle)

write(outsvg, "</svg>\n") # view file tree.svg in browser

```



## Kotlin

```scala
// version 1.1.2

import java.awt.Color
import java.awt.Graphics
import javax.swing.JFrame

class FractalTree : JFrame("Fractal Tree") {
    init {
        background = Color.black
        setBounds(100, 100, 800, 600)
        isResizable = false
        defaultCloseOperation = EXIT_ON_CLOSE
    }

    private fun drawTree(g: Graphics, x1: Int, y1: Int, angle: Double, depth: Int) {
        if (depth == 0) return
        val x2 = x1 + (Math.cos(Math.toRadians(angle)) * depth * 10.0).toInt()
        val y2 = y1 + (Math.sin(Math.toRadians(angle)) * depth * 10.0).toInt()
        g.drawLine(x1, y1, x2, y2)
        drawTree(g, x2, y2, angle - 20, depth - 1)
        drawTree(g, x2, y2, angle + 20, depth - 1)
    }

    override fun paint(g: Graphics) {
        g.color = Color.white
        drawTree(g, 400, 500, -90.0, 9)
    }
}

fun main(args: Array<String>) {
    FractalTree().isVisible = true
}
```



## Liberty BASIC

LB includes Logo-type turtle commands, so can be drawn that way as well as that shown here.

```lb

 NoMainWin
sw = 640 :   sh = 480
WindowWidth  = sw+8 : WindowHeight = sh+31
UpperLeftX = (DisplayWidth -sw)/2
UpperLeftY = (DisplayHeight-sh)/2
Open"Fractal Tree" For Graphics_nf_nsb As #g
#g "Down; Color darkgreen; TrapClose halt"
h$ = "#g"

'initial assignments
initAngle = Acs(-1)*1.5 'radian equivalent of 270 degrees
    theta = 29 * (Acs(-1)/180) 'convert 29 degrees to radians
   length = 110 'length in pixels
    depth = 25   'max recursion depth
    'draw the tree
    Call tree h$, 320, 470, initAngle, theta, length, depth
    #g "Flush; when leftButtonDown halt" 'L-click to exit
    Wait

Sub halt handle$
    Close #handle$
    End
End Sub

Sub tree h$, x, y, initAngle, theta, length, depth
    Scan
    newX = Cos(initAngle) * length + x
    newY = Sin(initAngle) * length + y
    #h$ "Line ";x;" ";y;" ";newX;" ";newY
    length = length * .78
    depth = depth - 1
    If depth > 0 Then
        Call tree h$, newX, newY, initAngle-theta, theta, length, depth
        Call tree h$, newX, newY, initAngle+theta, theta, length, depth
    End If
End Sub

```



## Lingo


```lingo
----------------------------------------
-- Creates an image of a fractal tree
-- @param {integer} width
-- @param {integer} height
-- @param {integer} fractalDepth
-- @param {integer|float} initSize
-- @param {float} spreadAngle
-- @param {float} [scaleFactor=1.0]
-- @return {image}
----------------------------------------
on fractalTree (width, height, fractalDepth, initSize, spreadAngle, scaleFactor)
  if voidP(scaleFactor) then scaleFactor = 1.0
  img = image(width, height, 24)
  img.fill(img.rect, rgb(0,0,0))
  _drawTree(img, width/2, height, -PI/2, fractalDepth, initSize, spreadAngle, scaleFactor)
  return img
end

on _drawTree (img, x1, y1, angle, depth, size, spreadAngle, scaleFactor)
  if (depth) then
    x2 = x1 + cos(angle)*depth*size
    y2 = y1 + sin(angle)*depth*size
    img.draw(x1, y1, x2, y2, [#color:rgb(255,255,255)])
    _drawTree(img, x2, y2, angle-spreadAngle, depth-1, size*ScaleFactor, spreadAngle, scaleFactor)
    _drawTree(img, x2, y2, angle+spreadAngle, depth-1, size*ScaleFactor, spreadAngle, scaleFactor)
  end if
end
```

Usage:

```lingo
fractalDepth = 10
initSize = 7.0
spreadAngle = 35*PI/180
scaleFactor = 0.95
img = fractalTree(480, 380, fractalDepth, initSize, spreadAngle, scaleFactor)
```



## Logo


```logo
to tree :depth :length :scale :angle
  if :depth=0 [stop]
  setpensize round :depth/2
  forward :length
  right :angle
  tree :depth-1 :length*:scale :scale :angle
  left 2*:angle
  tree :depth-1 :length*:scale :scale :angle
  right :angle
  back :length
end

clearscreen
tree 10 80 0.7 30
```



## Lua

Needs L&Ouml;VE 2D Engine

```lua

g, angle = love.graphics, 26 * math.pi / 180
wid, hei = g.getWidth(), g.getHeight()
function rotate( x, y, a )
  local s, c = math.sin( a ), math.cos( a )
  local a, b = x * c - y * s, x * s + y * c
  return a, b
end
function branches( a, b, len, ang, dir )
  len = len * .76
  if len < 5 then return end
  g.setColor( len * 16, 255 - 2 * len , 0 )
  if dir > 0 then ang = ang - angle
  else ang = ang + angle
  end
  local vx, vy = rotate( 0, len, ang )
  vx = a + vx; vy = b - vy
  g.line( a, b, vx, vy )
  branches( vx, vy, len, ang, 1 )
  branches( vx, vy, len, ang, 0 )
end
function createTree()
  local lineLen = 127
  local a, b = wid / 2, hei - lineLen
  g.setColor( 160, 40 , 0 )
  g.line( wid / 2, hei, a, b )
  branches( a, b, lineLen, 0, 1 )
  branches( a, b, lineLen, 0, 0 )
end
function love.load()
  canvas = g.newCanvas( wid, hei )
  g.setCanvas( canvas )
  createTree()
  g.setCanvas()
end
function love.draw()
  g.draw( canvas )
end

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
fractalTree[
  pt : {_, _}, \[Theta]orient_: \[Pi]/2, \[Theta]sep_: \[Pi]/9,
  depth_Integer: 9] := Module[{pt2},
  If[depth == 0, Return[]];
  pt2 = pt + {Cos[\[Theta]orient], Sin[\[Theta]orient]}*depth;
  DeleteCases[
   Flatten@{
     Line[{pt, pt2}],
     fractalTree[pt2, \[Theta]orient - \[Theta]sep, \[Theta]sep,
      depth - 1],
     fractalTree[pt2, \[Theta]orient + \[Theta]sep, \[Theta]sep,
      depth - 1]
     },
   Null
   ]
  ]
Graphics[fractalTree[{0, 0}, \[Pi]/2, \[Pi]/9]]

```

[[File:MathFractalTree.png]]


## NetRexx

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import java.awt.Color
import java.awt.Graphics
import javax.swing.JFrame

class RFractalTree public extends JFrame
  properties constant
    isTrue  = (1 == 1)
    isFalse = \isTrue
  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method RFractalTree() public
    super('Fractal Tree')
    setBounds(100, 100, 800, 600)
    setResizable(isFalse)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    return
  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method drawTree(g = Graphics, x1 = int, y1 = int, angle = double, depth = int) private
    if depth \= 0 then do
      x2 = x1 + (int Math.cos(Math.toRadians(angle)) * depth * 10.0)
      y2 = y1 + (int Math.sin(Math.toRadians(angle)) * depth * 10.0)
      g.drawLine(x1, y1, x2, y2)
      drawTree(g, x2, y2, angle - 20, depth - 1)
      drawTree(g, x2, y2, angle + 20, depth - 1)
      end
    return
  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method paint(g = Graphics) public
    g.setColor(Color.BLACK)
    drawTree(g, 400, 500, -90, 9)
    return
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method main(args = String[])public static
    RFractalTree().setVisible(isTrue)
    return

```



## OCaml

```ocaml
#directory "+cairo"
#load "bigarray.cma"
#load "cairo.cma"

let img_name = "/tmp/fractree.png"
let width  = 480
let height = 640

let level = 9
let line_width = 4.0

let color = (1.0, 0.5, 0.0)

let pi = 4.0 *. atan 1.0

let angle_split = pi *. 0.12
let angle_rand  = pi *. 0.12

let () =
  Random.self_init();
  let surf = Cairo.image_surface_create Cairo.FORMAT_RGB24 ~width ~height in
  let ctx = Cairo.create surf in
  Cairo.set_antialias ctx Cairo.ANTIALIAS_SUBPIXEL;
  Cairo.set_line_cap ctx Cairo.LINE_CAP_ROUND;

  let draw_line (x,y) (dx,dy) =
    Cairo.move_to ctx x  (float height -. y);
    Cairo.line_to ctx dx (float height -. dy);
    Cairo.stroke ctx;
  in
  let set_color (r,g,b) v =
    Cairo.set_source_rgb ctx ~red:(r *. v) ~green:(g *. v) ~blue:(b *. v);
  in
  let trans_pos (x,y) len angle =
    let _x = cos angle
    and _y = sin angle in
    (x +. (_x *. len),
     y +. (_y *. len))
  in

  let rec loop ~level ~pos ~line_width ~line_len
               ~angle ~angle_split ~angle_rand ~intc =
    if level > 0 then begin
      (* draw the current segment *)
      Cairo.set_line_width ctx line_width;
      set_color color intc;
      let pos_to = trans_pos pos line_len angle in
      draw_line pos pos_to;
      (* evolution of the parameters *)
      let line_width = line_width *. 0.8
      and line_len   = line_len   *. 0.62
      and angle_split = angle_split *. 1.02
      and angle_rand  = angle_rand  *. 1.02
      and intc = intc *. 0.9
      in
      let next_loop =
        loop ~level:(pred level) ~pos:pos_to ~intc
             ~line_width ~line_len ~angle_split ~angle_rand
      in
      (* split *)
      let angle_left  = angle +. angle_split +. Random.float angle_rand
      and angle_right = angle -. angle_split -. Random.float angle_rand
      in
      next_loop ~angle:angle_left;
      next_loop ~angle:angle_right
    end
  in

  let pos = (float width *. 0.5, float height *. 0.1)
  and line_len = float height *. 0.3
  in
  loop ~level ~pos ~angle:(pi /. 2.0)
       ~angle_split ~angle_rand
       ~line_width ~line_len ~intc:1.0;

  Cairo_png.surface_write_to_file surf img_name
  (*Cairo_png.surface_write_to_channel surf stdout*)
```



## PARI/GP

[[File:FracTree1.png|right|thumb|Output FracTree1.png]]
[[File:FracTree2.png|right|thumb|Output FracTree2.png]]
[[File:FracTree3.png|right|thumb|Output FracTree3.png]]

This version with recursion, in general, is a translation of JavaScript version.
Some tweaks and options were added to make it reusable and outputting different size of a tree.

```parigp

\\ Fractal tree (w/recursion)
\\ 4/10/16 aev
plotline(x1,y1,x2,y2)={plotmove(0, x1,y1);plotrline(0,x2-x1,y2-y1);}

plottree(x,y,a,d)={
my(x2,y2,d2r=Pi/180.0,a1=a*d2r,d1);
if(d<=0, return(););
if(d>0, d1=d*10.0;
    x2=x+cos(a1)*d1;
    y2=y+sin(a1)*d1;
    plotline(x,y,x2,y2);
    plottree(x2,y2,a-20,d-1);
    plottree(x2,y2,a+20,d-1),
    return();
  );
}

FractalTree(depth,size)={
my(dx=1,dy=0,ttlb="Fractal Tree, depth ",ttl=Str(ttlb,depth));
print1(" *** ",ttl); print(", size ",size);
plotinit(0);
plotcolor(0,6); \\green
plotscale(0, -size,size, 0,size);
plotmove(0, 0,0);
plottree(0,0,90,depth);
plotdraw([0,size,size]);
}

{\\ Executing:
FractalTree(9,500);     \\FracTree1.png
FractalTree(12,1100);   \\FracTree2.png
FractalTree(15,1500);   \\FracTree3.png
}

```


```txt


 *** Fractal Tree, depth 9, size 500
 ***   last result computed in 140 ms.

 *** Fractal Tree, depth 12, size 1100
 ***   last result computed in 236 ms.

 *** Fractal Tree, depth 15, size 1500
 ***   last result computed in 1,095 ms

```




## Perl

using the [http://search.cpan.org/~lds/GD-2.45/GD/Simple.pm GD::Simple] module.

```perl
use GD::Simple;

my ($width, $height) = (1000,1000); # image dimension
my $scale = 6/10; # branch scale relative to trunk
my $length = 400; # trunk size

my $img = GD::Simple->new($width,$height);
$img->fgcolor('black');
$img->penSize(1,1);

tree($width/2, $height, $length, 270);

print $img->png;


sub tree
{
        my ($x, $y, $len, $angle) = @_;

        return if $len < 1;

        $img->moveTo($x,$y);
        $img->angle($angle);
        $img->line($len);

        ($x, $y) = $img->curPos();

        tree($x, $y, $len*$scale, $angle+35);
        tree($x, $y, $len*$scale, $angle-35);
}
```




## Perl 6

Image is created in [[wp:SVG|SVG]] format.

```perl6
my ($width, $height) = (1000,1000); # image dimension
my $scale = 6/10; # branch scale relative to trunk
my $length = 400; # trunk size

say "<?xml version='1.0' encoding='utf-8' standalone='no'?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'
'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>
<svg width='100%' height='100%' version='1.1'
xmlns='http://www.w3.org/2000/svg'>";

tree($width/2, $height, $length, 3*pi/2);

say "</svg>";

multi tree($, $, $length where { $length < 1}, $) {}
multi tree($x, $y, $length, $angle)
{
	my ($x2, $y2) = ( $x + $length * $angle.cos, $y + $length * $angle.sin);
	say "<line x1='$x' y1='$y' x2='$x2' y2='$y2' style='stroke:rgb(0,0,0);stroke-width:1'/>";
	tree($x2, $y2, $length*$scale, $angle + pi/5);
	tree($x2, $y2, $length*$scale, $angle - pi/5);
}
```



## Phix

```Phix
--
-- demo\rosetta\FractalTree.exw
--
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

procedure drawTree(integer level, atom angle, atom len, integer x, integer y)
integer xn = x + floor(len*cos(angle))
integer yn = y + floor(len*sin(angle))
integer red = 255-level*8
integer grn = level*12+100
    cdCanvasSetForeground(cddbuffer, red*#10000 + grn*#100)
    cdCanvasLineWidth(cddbuffer,floor(5-level/3))
    cdCanvasLine(cddbuffer, x, 480-y, xn, 480-yn)
    if level<12 then
        drawTree(level+1, angle-0.4, len*0.8, xn, yn)   --left
        drawTree(level+1, angle+0.1, len*0.8, xn, yn)   --right
    end if
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    drawTree(0, -PI/2.0, 80.0, 360, 460)
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_PARCHMENT)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "640x480")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    dlg = IupDialog(canvas,"RESIZE=NO")
    IupSetAttribute(dlg, "TITLE", "Fractal Tree")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))

    IupShow(dlg)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PHP

Image is created with GD module. Code adapted from the JavaScript version.

```php

<?php
header("Content-type: image/png");

$width = 512;
$height = 512;
$img = imagecreatetruecolor($width,$height);
$bg = imagecolorallocate($img,255,255,255);
imagefilledrectangle($img, 0, 0, $width, $width, $bg);

$depth = 8;
function drawTree($x1, $y1, $angle, $depth){

    global $img;

    if ($depth != 0){
        $x2 = $x1 + (int)(cos(deg2rad($angle)) * $depth * 10.0);
        $y2 = $y1 + (int)(sin(deg2rad($angle)) * $depth * 10.0);

        imageline($img, $x1, $y1, $x2, $y2, imagecolorallocate($img,0,0,0));

        drawTree($x2, $y2, $angle - 20, $depth - 1);
        drawTree($x2, $y2, $angle + 20, $depth - 1);
    }
}

drawTree($width/2, $height, -90, $depth);

imagepng($img);
imagedestroy($img);
?>

```



## PicoLisp

This uses the 'brez' line drawing function from
[[Bitmap/Bresenham's line algorithm#PicoLisp]].

```PicoLisp
(load "@lib/math.l")

(de fractalTree (Img X Y A D)
   (unless (=0 D)
      (let (R (*/ A pi 180.0)  DX (*/ (cos R) D 0.2)  DY (*/ (sin R) D 0.2))
         (brez Img X Y DX DY)
         (fractalTree Img (+ X DX) (+ Y DY) (+ A 30.0) (dec D))
         (fractalTree Img (+ X DX) (+ Y DY) (- A 30.0) (dec D)) ) ) )

(let Img (make (do 300 (link (need 400 0))))       # Create image 400 x 300
   (fractalTree Img 200 300 -90.0 10)              # Draw tree
   (out "img.pbm"                                  # Write to bitmap file
      (prinl "P1")
      (prinl 400 " " 300)
      (mapc prinl Img) ) )
```



## PostScript


```postscript
%!PS
%%BoundingBox: 0 0 300 300
%%EndComments
/origstate save def
/ld {load def} bind def
/m /moveto ld /g /setgray ld /t /translate ld
/r /rotate ld /l /lineto ld
/rl /rlineto ld /s /scale ld
%%EndProlog
/PerturbateAngle {} def
/PerturbateLength {} def
% ** To add perturbations, define properly PerturbateAngle and PerturbateLength, e.g.
% /PerturbateAngle {realtime 20 mod realtime 2 mod 1 eq {add} {sub} ifelse} def
% /PerturbateLength {realtime 10 mod 100 div realtime 2 mod 1 eq {add} {sub} ifelse} def
/fractree { % [INITLENGTH, SPLIT, SFACTOR, BRANCHES]
  dup 3 get 0 gt
  {
    0 0 m dup 0 get 0 exch l
    gsave
      dup 0 get 0 exch t
      dup 1 get PerturbateAngle r
      dup 2 get dup PerturbateLength s
      dup aload pop 1 sub 4 array astore fractree stroke
    grestore
    gsave
      dup 0 get 0 exch t
      dup 1 get neg PerturbateAngle r
      dup 2 get dup PerturbateLength s
      dup aload pop 1 sub 4 array astore fractree stroke
    grestore
  } if pop
} def
%
/BRANCHES 14 def
/INITLENGTH 50 def
/SPLIT 35 def
/SFACTOR .75 def
%
% BB check
%0 0 m 300 0 rl 0 300 rl -300 0 rl closepath stroke
%
0 g 150 0 t
[INITLENGTH SPLIT SFACTOR BRANCHES] fractree stroke
%
showpage origstate restore
%%EOF
```


Shorter version:
```postscript
%!PS-Adobe-3.0
%%BoundingBox: 0 0 300 300
/!0 { dup 1 sub dup 0 gt } def
/trunk { 0 0 moveto 0 60 translate 0 0 lineto stroke } def

/branch { gsave scale rotate dup d exch sub d div setgray tree grestore } def
/L { 30 .8 .8 branch } def
/M {-10 .7 .7 branch } def
/R {-35 .7 .7 branch } def
/tree { trunk !0 { L M R } if pop } def

/d 10 def 5 setlinewidth 1 setlinecap 170 20 translate d tree pop
%%EOF
```


=={{header|POV-Ray}}==

```povray
#include "colors.inc"
#include "transforms.inc"

#declare CamLoc = <0, 5, 0>;
#declare CamLook = <0,0,0>;
camera
{
  location CamLoc
  look_at CamLook
  rotate y*90
}

light_source
{
  CamLoc
  color White
}

#declare Init_Height    = 10;
#declare Spread_Ang     = 35;
#declare Branches       = 14;
#declare Scaling_Factor = 0.75;

#macro Stick(P0, P1)
  cylinder {
    P0, P1, 0.02
    texture { pigment { Green } }
  }
#end

#macro FractalTree(O, D, S, R, B)
  #if (B > 0)
    Stick(O, O+D*S)
    FractalTree(O+D*S, vtransform(D, transform{rotate y*R}),
      S*Scaling_Factor, R, B-1)
    FractalTree(O+D*S, vtransform(D, transform{rotate -y*R}),
      S*Scaling_Factor, R, B-1)
  #end
#end

union {
  FractalTree(<-2,0,0>, <1,0,0>, 1, Spread_Ang, Branches)
}
```


## Prolog

SWI-Prolog has a graphic interface : XPCE.

```Prolog
fractal :-
	new(D, window('Fractal')),
	send(D, size, size(800, 600)),
	drawTree(D, 400, 500, -90, 9),
	send(D, open).


drawTree(_D, _X, _Y, _Angle, 0).

drawTree(D, X1, Y1, Angle, Depth) :-
        X2 is X1 + cos(Angle * pi / 180.0) * Depth * 10.0,
        Y2 is Y1 + sin(Angle * pi / 180.0) * Depth * 10.0,
	new(Line, line(X1, Y1, X2, Y2, none)),
	send(D, display, Line),
	A1 is Angle - 30,
	A2 is Angle + 30,
	De is Depth - 1,
        drawTree(D, X2, Y2, A1, De),
        drawTree(D, X2, Y2, A2, De).


```


## PureBasic


```PureBasic
#Spread_Ang     = 35
#Scaling_Factor = 0.75
#Deg_to_Rad = #PI / 180
#SizeH = 500
#SizeV = 375
#Init_Size = 100

Procedure drawTree(x1, y1, Size, theta, depth)
  Protected x2 = x1 + Cos(theta * #Deg_to_Rad) * Size, y2 = y1 + Sin(theta * #Deg_to_Rad) * Size
  LineXY(x1, y1, x2, y2, RGB(255, 255, 255))
  If depth <= 0
    ProcedureReturn
  EndIf
  ;draw left branch
  drawTree(x2, y2, Size * #Scaling_Factor, theta - #Spread_Ang, depth - 1)
  ;draw right branch
  drawTree(x2, y2, Size * #Scaling_Factor, theta + #Spread_Ang, depth - 1)
EndProcedure


OpenWindow(0, 0, 0, #SizeH, #SizeV, "Fractal Tree", #PB_Window_SystemMenu)
Define fractal = CreateImage(#PB_Any, #SizeH, #SizeV, 32)
ImageGadget(0, 0, 0, 0, 0, ImageID(fractal))

If StartDrawing(ImageOutput(fractal))
    drawTree(#SizeH / 2, #SizeV, #Init_Size, -90, 9)
  StopDrawing()
  SetGadgetState(0, ImageID(fractal))
EndIf

Repeat: Until WaitWindowEvent(10) = #PB_Event_CloseWindow
```

[[Image:PB_FractalTree.png]]


## Python

[[File:fractal-tree-python.png|right|thumb]]
```python
import pygame, math

pygame.init()
window = pygame.display.set_mode((600, 600))
pygame.display.set_caption("Fractal Tree")
screen = pygame.display.get_surface()

def drawTree(x1, y1, angle, depth):
    if depth:
        x2 = x1 + int(math.cos(math.radians(angle)) * depth * 10.0)
        y2 = y1 + int(math.sin(math.radians(angle)) * depth * 10.0)
        pygame.draw.line(screen, (255,255,255), (x1, y1), (x2, y2), 2)
        drawTree(x2, y2, angle - 20, depth - 1)
        drawTree(x2, y2, angle + 20, depth - 1)

def input(event):
    if event.type == pygame.QUIT:
        exit(0)

drawTree(300, 550, -90, 9)
pygame.display.flip()
while True:
    input(pygame.event.wait())
```



## R

[[File:FRTR9.png|200px|right|thumb|Output FRTR9.png]]
[[File:FRTR12.png|200px|right|thumb|Output FRTR12.png]]
[[File:FRTR15.png|200px|right|thumb|Output FRTR15.png]]

```r

## Recursive FT plotting
plotftree <- function(x, y, a, d, c) {
x2=y2=0; d2r=pi/180.0; a1 <- a*d2r; d1=0;
if(d<=0) {return()}
if(d>0)
  { d1=d*10.0;
    x2=x+cos(a1)*d1;
    y2=y+sin(a1)*d1;
    segments(x*c, y*c, x2*c, y2*c, col='darkgreen');
    plotftree(x2,y2,a-20,d-1,c);
    plotftree(x2,y2,a+20,d-1,c);
    #return(2);
  }
}
## Plotting Fractal Tree. aev 3/27/17
## ord - order/depth, c - scale, xsh - x-shift, fn - file name,
##  ttl - plot title.
pFractalTree <- function(ord, c=1, xsh=0, fn="", ttl="") {
  cat(" *** START FRT:", date(), "\n");
  m=640;
  if(fn=="") {pf=paste0("FRTR", ord, ".png")} else {pf=paste0(fn, ".png")};
  if(ttl=="") {ttl=paste0("Fractal tree, order - ", ord)};
  cat(" *** Plot file -", pf, "title:", ttl, "\n");
  ##plot(NA, xlim=c(0,m), ylim=c(-m,0), xlab="", ylab="", main=ttl);
  plot(NA, xlim=c(0,m), ylim=c(0,m), xlab="", ylab="", main=ttl);
  plotftree(m/2+xsh,100,90,ord,c);
  dev.copy(png, filename=pf, width=m, height=m);
  dev.off(); graphics.off();
  cat(" *** END FRT:",date(),"\n");
}
## Executing:
pFractalTree(9);
pFractalTree(12,0.6,210);
pFractalTree(15,0.35,600);

```

```txt

> pFractalTree(9);
 *** START FRT: Tue Mar 28 16:49:49 2017
 *** Plot file - FRTR9.png title: Fractal tree, order - 9
 *** END FRT: Tue Mar 28 16:49:50 2017
> pFractalTree(12,0.6,210);
 *** START FRT: Tue Mar 28 17:32:15 2017
 *** Plot file - FRTR12.png title: Fractal tree, order - 12
 *** END FRT: Tue Mar 28 17:32:16 2017
> pFractalTree(15,0.35,600);
 *** START FRT: Tue Mar 28 17:38:34 2017
 *** Plot file - FRTR15.png title: Fractal tree, order - 15
 *** END FRT: Tue Mar 28 17:38:41 2017

```



## Racket

[[File:tree-racket.png|right|thumb]]

```racket

#lang racket
(require graphics/turtles)

(define (tree n)
  (when (> n 1)
    (draw (/ n 2))
    (tprompt (split* (turn 60) (turn -60))
             (tree (/ n 2)))
    (draw (/ n 2))
    (turn 5)
    (tree (- n 1))))

(turtles #t) (move 100) (turn 90) (move -200)
(tree 35)
(save-turtle-bitmap "tree.png" 'png)

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
               draw()
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

        sizex = 400
        sizey = 200
        depth = 10

        tree(self, sizex, 0, sizey/2, 90, depth)

        endpaint()
        }
        label1 { setpicture(p1) show() }

        func tree myObj, x1, y1, size, angle, depth
             myObj{
             scale = 0.76
             spread = 25
             x2 = x1 + size * cos(angle)
             y2 = y1 + size * sin(angle)
             drawline(x1, y1, x2, y2)
             if depth > 0
             tree(self, x2, y2, size * scale, angle - spread, depth - 1)
             tree(self, x2, y2, size * scale, angle + spread, depth - 1) ok}

```

Output:

[[File:CalmoSoftFractalTree.jpg]]


## Ruby

```Ruby
Shoes.app(:title => "Fractal Tree", :width => 600, :height => 600) do
  background "#fff"
  stroke "#000"
  @deg_to_rad = Math::PI / 180.0

  def drawTree(x1, y1, angle, depth)
    if depth != 0
      x2 = x1 + (Math.cos(angle * @deg_to_rad) * depth * 10.0).to_i
      y2 = y1 + (Math.sin(angle * @deg_to_rad) * depth * 10.0).to_i

      line x1, y1, x2, y2

      drawTree(x2, y2, angle - 20, depth - 1)
      drawTree(x2, y2, angle + 20, depth - 1)
    end
  end

  drawTree(300,550,-90,9)
end
```



## Rust

```Rust
//Cargo deps :
//  piston = "0.35.0"
//  piston2d-graphics = "0.23.0"
//  piston2d-opengl_graphics = "0.49.0"
//  pistoncore-glutin_window = "0.42.0"

extern crate piston;
extern crate graphics;
extern crate opengl_graphics;
extern crate glutin_window;

use piston::window::WindowSettings;
use piston::event_loop::{Events, EventSettings};
use piston::input::RenderEvent;
use glutin_window::GlutinWindow as Window;
use opengl_graphics::{GlGraphics, OpenGL};
use graphics::{clear, line, Context};

const ANG: f64 = 20.0;
const COLOR: [f32; 4] = [1.0, 0.0, 0.5, 1.0];
const LINE_THICKNESS: f64 = 5.0;
const DEPTH: u32 = 11;

fn main() {
    let mut window: Window = WindowSettings::new("Fractal Tree", [1024, 768])
        .opengl(OpenGL::V3_2)
        .exit_on_esc(true)
        .build()
        .unwrap();
    let mut gl = GlGraphics::new(OpenGL::V3_2);

    let mut events = Events::new(EventSettings::new());
    while let Some(e) = events.next(&mut window) {
        if let Some(args) = e.render_args() {
            gl.draw(args.viewport(), |c, g| {
                clear([1.0, 1.0, 1.0, 1.0], g);
                draw_fractal_tree(512.0, 700.0, 0.0, DEPTH, c, g);
            });
        }
    }
}

fn draw_fractal_tree(x1: f64, y1: f64, angle: f64, depth: u32, c: Context, g: &mut GlGraphics) {
    let x2 = x1 + angle.to_radians().sin() * depth as f64 * 10.0;
    let y2 = y1 - angle.to_radians().cos() * depth as f64 * 10.0;
    line(
        COLOR,
        LINE_THICKNESS * depth as f64 * 0.2,
        [x1, y1, x2, y2],
        c.transform,
        g,
    );
    if depth > 0 {
        draw_fractal_tree(x2, y2, angle - ANG, depth - 1, c, g);
        draw_fractal_tree(x2, y2, angle + ANG, depth - 1, c, g);
    }
}

```



## Scala

Adapted from the Java version. Screenshot below.

```scala
import swing._
import java.awt.{RenderingHints, BasicStroke, Color}

object FractalTree extends SimpleSwingApplication {
  val DEPTH = 9

  def top = new MainFrame {
    contents = new Panel {
      preferredSize = new Dimension(600, 500)

      override def paintComponent(g: Graphics2D) {
        draw(300, 460, -90, DEPTH)

        def draw(x1: Int, y1: Int, angle: Double, depth: Int) {
          if (depth > 0) {
            val x2 = x1 + (math.cos(angle.toRadians) * depth * 10).toInt
            val y2 = y1 + (math.sin(angle.toRadians) * depth * 10).toInt

            g.setColor(Color.getHSBColor(0.25f - depth * 0.125f / DEPTH, 0.9f, 0.6f))
            g.setStroke(new BasicStroke(depth))
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
            g.drawLine(x1, y1, x2, y2)

            draw(x2, y2, angle - 20, depth - 1)
            draw(x2, y2, angle + 20, depth - 1)
          }
        }
      }
    }
  }
}
```

[[File:scalaTree.png]]


## Scheme


The tree is created as a list of line segments, which can then be drawn on a required device.  For this program, the tree is output to an eps file.


```scheme

(import (scheme base)
        (scheme file)
        (scheme inexact)
        (scheme write))

(define *scale* 10) ; controls overall size of tree
(define *split* 20) ; controls angle of split (in degrees)

;; construct lines for tree as list of 5-tuples (x1 y1 x2 y2 depth)
;; - x1 y1 is start point
;; - angle of this line, in radians
;; - depth, depth within tree (controls length of line)
(define (create-tree x1 y1 angle depth)
  (define (degrees->radians d)
    (let ((pi 3.14159265358979323846264338327950288419716939937510582097))
      (* d pi 1/180)))
  ;
  (if (zero? depth)
    '()
    (let ((x2 (+ x1 (* (cos (degrees->radians angle)) depth *scale*)))
          (y2 (+ y1 (* (sin (degrees->radians angle)) depth *scale*))))
      (append (list (map truncate (list x1 y1 x2 y2 depth)))
              (create-tree x2 y2 (- angle *split*) (- depth 1))
              (create-tree x2 y2 (+ angle *split*) (- depth 1))))))

;; output the tree to an eps file
(define (output-tree-as-eps filename tree)
  (when (file-exists? filename) (delete-file filename))
  (with-output-to-file
    filename
    (lambda ()
      (display "%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: 0 0 800 800\n")

      ;; add each line - sets linewidth based on depth in tree
      (for-each (lambda (line)
                  (display
                    (string-append "newpath\n"
                                   (number->string (list-ref line 0)) " "
                                   (number->string (list-ref line 1)) " "
                                   "moveto\n"
                                   (number->string (list-ref line 2)) " "
                                   (number->string (list-ref line 3)) " "
                                   "lineto\n"
                                   (number->string (truncate (/ (list-ref line 4) 2)))
                                   " setlinewidth\n"
                                   "stroke\n"
                                   )))
                tree)
      (display "\n%%EOF"))))

(output-tree-as-eps "fractal.eps" (create-tree 400 200 90 9))

```



## Scilab


===L-System approach===
This script uses complex numbers to represent (x,y) coordinates: real part as x position, and imaginary part as y position. The tree is generated using an L-system approach, and the lines are then drawn by interpreting the resulting sentence. The output is plotted onto graphic window.

<lang>trunk = 1;                  //trunk length
ratio = 0.8;                //size ratio between two consecutive branches
depth = 9;                  //final number of branch levels
orign = 0;                  //origin of the tree (should be complex)
angle = 45*%pi/180;         //angle between two branches [rad]
trunk_angle = 90*%pi/180;   //angle between trunk and X-axis [rad]

right_angle = angle/2;      //angles to the right or to the left
left_angle = 0.8*angle;     //can be set independently or
                            //as function of 'angle'

//L-system definition:
//Alphabet: FBD[]+-
    //F: go forward             B: go backwards
    //[: start new branch       ]: end current branch
    //+: branch to the right    -: branch to the left
    //D: double line (forward then backward)
//Axiom:    D
//Rule:     D -> F[+D-D]B

//L-system sentence generation
sentence = 'D'
rule = 'F[+D-D]B';
for i=1:depth
    sentence = strsubst(sentence,'D',rule);
end
sentence = strsplit(sentence)';

//Empty tree
tree_size = 1.0...
            + length(find(sentence=='F'|sentence=='B'))...
            + 2 * length(find(sentence=='D'));
tree=zeros(tree_size,1);

//Drawing the tree
branch_level = 0;
curr_angle = trunk_angle;
curr_pos = 1;

for ind = 1:size(sentence,'c')
    charac = sentence(ind);

    select charac
        case 'F' then //Draw line forward
            tree(curr_pos+1) = tree(curr_pos)...
                               + trunk * ratio^branch_level * exp(curr_angle*%i);
            curr_pos = curr_pos + 1;

        case 'B' then //Draw line backwards
            tree(curr_pos+1) = tree(curr_pos)...
                               + trunk * ratio^branch_level * exp((%pi+curr_angle)*%i);
            curr_pos = curr_pos + 1;

        case '[' then //New branch
            branch_level = branch_level + 1;

        case '+' then //Turn right
            curr_angle = curr_angle - right_angle;

        case '-' then //Turn left
            curr_angle = curr_angle + right_angle + left_angle;

        case ']' then //End of branch
            branch_level = branch_level - 1;
            curr_angle = curr_angle - left_angle;

        case 'D' then //Double line
            tree(curr_pos+1) = tree(curr_pos)...
                               + trunk * ratio^branch_level * exp(curr_angle*%i);
            tree(curr_pos+2) = tree(curr_pos+1)...
                               + trunk * ratio^branch_level * exp((%pi+curr_angle)*%i);
            curr_pos = curr_pos + 2;
    end
end

scf(); clf();
xname('Fractal tree: '+string(depth)+' levels')
plot2d(real(tree),imag(tree),14);
set(gca(),'isoview','on');
set(gca(),'axes_visible',['off','off','off']);
```



### Recursive approach

<lang>width = 512;
height = 512;
img=scf();
set(img,'figure_size',[width,height]);

function drawTree(x1, y1, angle, depth)
    if depth ~= 0 then
        x2 = x1 + cos(angle * %pi/180) * depth * 10;
        y2 = y1 + sin(angle * %pi/180) * depth * 10;
        plot2d([x1 x2],[y1 y2],14);
        drawTree(x2, y2, angle - 20, depth - 1);
        drawTree(x2, y2, angle + 20, depth - 1);
    end
endfunction

drawTree(width/2,height,90,10);
set(gca(),'isoview','on');
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";
  include "draw.s7i";
  include "keybd.s7i";

const float: DEG_TO_RAD is PI / 180.0;

const proc: drawTree (in integer: x1, in integer: y1, in float: angle, in integer: depth) is func
  local
    var integer: x2 is 0;
    var integer: y2 is 0;
  begin
    if depth <> 0 then
      x2 := x1 + trunc(cos(angle * DEG_TO_RAD) * flt(depth * 10));
      y2 := y1 + trunc(sin(angle * DEG_TO_RAD) * flt(depth * 10));
      lineTo(x1, y1, x2, y2, white);
      drawTree(x2, y2, angle - 20.0, depth - 1);
      drawTree(x2, y2, angle + 20.0, depth - 1);
    end if;
  end func;

const proc: main is func
  begin
    screen(600, 500);
    clear(curr_win, black);
    KEYBOARD := GRAPH_KEYBOARD;
    drawTree(300, 470, -90.0, 9);
    ignore(getc(KEYBOARD));
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/graphic.htm#fractree]


## Sidef

```ruby
func tree(img, x, y, scale=6/10, len=400, angle=270) {

    len < 1 && return()

    img.moveTo(x, y)
    img.angle(angle)
    img.line(len)

    var (x1, y1) = img.curPos
    tree(img, x1, y1, scale, len*scale, angle+35)
    tree(img, x1, y1, scale, len*scale, angle-35)
}

require('GD::Simple')

var (width=1000, height=1000)
var img = %s|GD::Simple|.new(width, height)
img.fgcolor('black')
img.penSize(1, 1)

tree(img, width/2, height)

File('tree.png').write(img.png, :raw)
```



## Smalltalk


This example is coded for Squeak Smalltalk.


```smalltalk

Object subclass: #FractalTree
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'RosettaCode'

```


Methods for FractalTree class:


```smalltalk

tree: aPoint length: aLength angle: anAngle
    | p a |

    (aLength > 10) ifTrue: [
        p := Pen new.
        p up.
        p goto: aPoint.
        p turn: anAngle.
        p down.
        5 timesRepeat: [
            p go: aLength / 5.
            p turn: 5.
        ].
        a := anAngle - 30.
        3 timesRepeat: [
            self tree: p location length: aLength * 0.7 angle: a.
            a := a + 30.
        ]
    ].

draw
    Display restoreAfter: [
        Display fillWhite.
        self tree: 700@700 length: 200 angle: 0.
    ]

```


Now open a new Workspace and enter:


```smalltalk

FractalTree new draw.

```




## SVG


[[File:Fractal tree.svg|thumb|right]]
In the same style as [[Dragon curve#SVG]]. SVG has no parameterized definitions, so the recursion must be unrolled.

<div style="clear:both;"></div>

```xml
<?xml version="1.0" standalone="yes"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 20010904//EN"
 "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
<svg xmlns="http://www.w3.org/2000/svg"
     xmlns:xlink="http://www.w3.org/1999/xlink"
     width="400" height="320">
  <style type="text/css"><![CDATA[
    line { stroke: black; stroke-width: .05; }
    circle { fill: black; }
  ]]></style>

<defs>
  <g id="stem"> <line x1="0" y1="0" x2="0" y2="-1"/> </g>

  <g id="l0"><use xlink:href="#stem"/></g>
  <!-- These are identical except for the id and href. -->
  <g id="l1"> <use xlink:href="#l0" transform="translate(0, -1) rotate(-35) scale(.7)"/>
              <use xlink:href="#l0" transform="translate(0, -1) rotate(+35) scale(.7)"/>
              <use xlink:href="#stem"/></g>
  <g id="l2"> <use xlink:href="#l1" transform="translate(0, -1) rotate(-35) scale(.7)"/>
              <use xlink:href="#l1" transform="translate(0, -1) rotate(+35) scale(.7)"/>
              <use xlink:href="#stem"/></g>
  <g id="l3"> <use xlink:href="#l2" transform="translate(0, -1) rotate(-35) scale(.7)"/>
              <use xlink:href="#l2" transform="translate(0, -1) rotate(+35) scale(.7)"/>
              <use xlink:href="#stem"/></g>
  <g id="l4"> <use xlink:href="#l3" transform="translate(0, -1) rotate(-35) scale(.7)"/>
              <use xlink:href="#l3" transform="translate(0, -1) rotate(+35) scale(.7)"/>
              <use xlink:href="#stem"/></g>
  <g id="l5"> <use xlink:href="#l4" transform="translate(0, -1) rotate(-35) scale(.7)"/>
              <use xlink:href="#l4" transform="translate(0, -1) rotate(+35) scale(.7)"/>
              <use xlink:href="#stem"/></g>
  <g id="l6"> <use xlink:href="#l5" transform="translate(0, -1) rotate(-35) scale(.7)"/>
              <use xlink:href="#l5" transform="translate(0, -1) rotate(+35) scale(.7)"/>
              <use xlink:href="#stem"/></g>
  <g id="l7"> <use xlink:href="#l6" transform="translate(0, -1) rotate(-35) scale(.7)"/>
              <use xlink:href="#l6" transform="translate(0, -1) rotate(+35) scale(.7)"/>
              <use xlink:href="#stem"/></g>
  <g id="l8"> <use xlink:href="#l7" transform="translate(0, -1) rotate(-35) scale(.7)"/>
              <use xlink:href="#l7" transform="translate(0, -1) rotate(+35) scale(.7)"/>
              <use xlink:href="#stem"/></g>
  <g id="l9"> <use xlink:href="#l8" transform="translate(0, -1) rotate(-35) scale(.7)"/>
              <use xlink:href="#l8" transform="translate(0, -1) rotate(+35) scale(.7)"/>
              <use xlink:href="#stem"/></g>
</defs>

<g transform="translate(200, 320) scale(100)">
  <use xlink:href="#l9"/>
</g>

</svg>
```


## Swift

[http://i.imgur.com/F8Fyn1i.png Image] - Link, since uploads seem to be disabled currently.
In a playground:

```swift
extension CGFloat {
  func degrees_to_radians() -> CGFloat {
    return CGFloat(M_PI) * self / 180.0
  }
}

extension Double {
  func degrees_to_radians() -> Double {
    return Double(M_PI) * self / 180.0
  }
}


class Tree: UIView {


  func drawTree(x1: CGFloat, y1: CGFloat, angle: CGFloat, depth:Int){
    if depth == 0 {
      return
    }
    let ang = angle.degrees_to_radians()
    let x2:CGFloat = x1 + ( cos(ang) as CGFloat) * CGFloat(depth) * (self.frame.width / 60)
    let y2:CGFloat = y1 + ( sin(ang) as CGFloat) * CGFloat(depth) * (self.frame.width / 60)

    let line = drawLine(x1, y1: y1, x2: x2, y2: y2)

    line.stroke()
    drawTree(x2, y1: y2, angle: angle - 20, depth: depth - 1)
    drawTree(x2, y1: y2, angle: angle + 20, depth: depth - 1)
  }

  func drawLine(x1:CGFloat, y1:CGFloat, x2:CGFloat, y2:CGFloat) -> UIBezierPath
  {

    let path = UIBezierPath()
    path.moveToPoint(CGPoint(x: x1,y: y1))
    path.addLineToPoint(CGPoint(x: x2,y: y2))
    path.lineWidth = 1
    return path
  }

  override func drawRect(rect: CGRect) {

    let color = UIColor(red: 1.0, green: 0.0, blue: 0.0, alpha: 1.0)
    color.set()
    drawTree(self.frame.width / 2 , y1: self.frame.height * 0.8, angle: -90 , depth: 9 )
  }
}


let tree = Tree(frame: CGRectMake(0, 0, 300, 300))
tree

```



## Tcl

```tcl
package require Tk

set SIZE	800
set SCALE	4.0
set BRANCHES	14
set ROTATION_SCALE 0.85
set INITIAL_LENGTH 50.0

proc draw_tree {w x y dx dy size theta depth} {
    global SCALE ROTATION_SCALE
    $w create line $x $y [expr {$x + $dx*$size}] [expr {$y + $dy*$size}]
    if {[incr depth -1] >= 0} {
	set x [expr {$x + $dx*$size}]
	set y [expr {$y + $dy*$size}]
	set ntheta [expr {$theta * $ROTATION_SCALE}]

	# Draw left branch
	draw_tree $w $x $y \
	    [expr {$dx*cos($theta) + $dy*sin($theta)}] \
	    [expr {$dy*cos($theta) - $dx*sin($theta)}] \
	    [expr {$size * (rand() + $SCALE - 1) / $SCALE}] $ntheta $depth
	# Draw right branch
	draw_tree $w $x $y \
	    [expr {$dx*cos(-$theta) + $dy*sin(-$theta)}] \
	    [expr {$dy*cos(-$theta) - $dx*sin(-$theta)}] \
	    [expr {$size * (rand() + $SCALE - 1) / $SCALE}] $ntheta $depth
    }
}

pack [canvas .c -width $SIZE -height $SIZE]
draw_tree .c [expr {$SIZE/2}] [expr {$SIZE-10}] 0.0 -1.0 $INITIAL_LENGTH \
    [expr {3.1415927 / 8}] $BRANCHES
```


## TUSCRIPT

Image is created in SVG-format

```tuscript

$$ MODE TUSCRIPT
dest="fracaltree.svg"
ERROR/STOP CREATE (dest,fdf-o,-std-)
ACCESS d: WRITE/ERASE/RECORDS/UTF8 $dest s,text
MODE DATA
$$ header=*
<?xml version="1.0" standalone="yes"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 20010904//EN"
 "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
<svg xmlns="http://www.w3.org/2000/svg"
 xmlns:xlink="http://www.w3.org/1999/xlink"
 width="400" height="320">
  <style type="text/css"><![CDATA[
  line { stroke: brown; stroke-width: .05; }
  ]]></style>
$$ WRITE/NEXT d header
$$ defsbeg=*
<defs>
  <g id="stem"> <line x1="0" y1="0" x2="0" y2="-1"/> </g>
  <g id="l"><use xlink:href="#stem"/></g>
$$ WRITE/NEXT d defsbeg
$$ LOOP n=10,21
$$ id=n+1,lastnr=VALUE(n)
$$ g=*
  <g id="{id}"> <use xlink:href="#{n}" transform="translate(0, -1) rotate(-35) scale(.7)"/>
  <use xlink:href="#{n}" transform="translate(0, -1) rotate(+35) scale(.7)"/> <use xlink:href="#stem"/></g>
$$ WRITE/NEXT d g
$$ ENDLOOP
$$ defsend = *
</defs>
<g transform="translate(200, 320) scale(100)">
  <use xlink:href="#{lastnr}"/>
</g>
$$ MODE TUSCRIPT
WRITE/NEXT d defsend
WRITE/NEXT d "</svg>"
ENDACCESS d

```



## TypeScript

```JavaScript
// Set up canvas for drawing
var canvas: HTMLCanvasElement = document.createElement('canvas')
canvas.width = 600
canvas.height = 500
document.body.appendChild(canvas)
var ctx: CanvasRenderingContext2D = canvas.getContext('2d')
ctx.fillStyle = '#000'
ctx.lineWidth = 1

// constants
const degToRad: number = Math.PI / 180.0
const totalDepth: number = 9

/** Helper function that draws a line on the canvas */
function drawLine(x1: number, y1: number, x2: number, y2: number): void {
    ctx.moveTo(x1, y1)
    ctx.lineTo(x2, y2)
}

/** Draws a branch at the given point and angle and then calls itself twice */
function drawTree(x1: number, y1: number, angle: number, depth: number): void {
    if (depth !== 0) {
        let x2: number = x1 + (Math.cos(angle * degToRad) * depth * 10.0)
        let y2: number = y1 + (Math.sin(angle * degToRad) * depth * 10.0)
        drawLine(x1, y1, x2, y2)
        drawTree(x2, y2, angle - 20, depth - 1)
        drawTree(x2, y2, angle + 20, depth - 1)
    }
}

// actual drawing of tree
ctx.beginPath()
drawTree(300, 500, -90, totalDepth)
ctx.closePath()
ctx.stroke()


```



## XPL0

[[File:FtreeXPL0.png|200px|thumb|right|Output]]

```XPL0
include c:\cxpl\codes;

proc DrawBranch(Lev, Dir, Len, X, Y);
int  Lev; real Dir, Len; int X, Y;
int  Red, Grn;
[Move(X, Y);
X:= X + fix(Len*Cos(Dir));
Y:= Y + fix(Len*Sin(Dir));
Red:= 255-Lev*8;  Grn:= Lev*12+100;
Line(X, Y, Red<<16+Grn<<8);
if Lev < 12 then                \limit level of recursion
        [DrawBranch(Lev+1, Dir-0.4, Len*0.8, X, Y);     \left
         DrawBranch(Lev+1, Dir+0.1, Len*0.8, X, Y);     \right
        ];
];

[SetVid($112);                  \set 640x480x24 video graphics mode
DrawBranch(0, -3.14159/2.0, 80.0, 360, 460);
if ChIn(1) then [];             \wait for keystroke
SetVid(3);                      \restore normal text mode
]
```



## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
[[File:FractalTree.zkl.jpg|250px|thumb|right]]

```zkl
fcn fractalTree(){
   scale:=0.76;
   sizeX:=400; sizeY:=300;
   bitmap:=PPM(sizeX*2,sizeY*2,0xFF|FF|FF);
   branch:='wrap(x1,y1,size,angle,depth){
      ar:=angle.toRad();
      x2:=x1 - size*ar.cos();
      y2:=y1 + size*ar.sin();
      color:=(0xff-depth*8).shiftLeft(16) + (depth*12+100).shiftLeft(8);
      bitmap.line(x1,y1, x2,y2, color);
      if(depth){
         self.fcn(x2,y2,scale*size,angle - 30,depth - 1,vm.pasteArgs(5));
	 self.fcn(x2,y2,scale*size,angle + 8, depth - 1,vm.pasteArgs(5));
      }
   };
   branch(sizeX,0,sizeY/2,90.0,10);
   bitmap.write(File("foo.ppm","wb"));
}();
```

The funkyness (pasteArgs) in the recursion (self.fcn) is due to the closure ('wrap): the closed over args are stashed in the arglist, they need to be added to the parameters when recursing.


## ZX Spectrum Basic

```zxbasic
10 LET level=12: LET long=45
20 LET x=127: LET y=0
30 LET rotation=PI/2
40 LET a1=PI/9: LET a2=PI/9
50 LET c1=0.75: LET c2=0.75
60 DIM x(level): DIM y(level)
70 BORDER 0: PAPER 0: INK 4: CLS
80 GO SUB 100
90 STOP
100 REM Tree
110 LET x(level)=x: LET y(level)=y
120 GO SUB 1000
130 IF level=1 THEN GO TO 240
140 LET level=level-1
150 LET long=long*c1
160 LET rotation=rotation-a1
170 GO SUB 100
180 LET long=long/c1*c2
190 LET rotation=rotation+a1+a2
200 GO SUB 100
210 LET rotation=rotation-a2
220 LET long=long/c2
230 LET level=level+1
240 LET x=x(level): LET y=y(level)
250 RETURN
1000 REM Draw
1010 LET yn=-SIN rotation*long+y
1020 LET xn=COS rotation*long+x
1030 PLOT x,y: DRAW xn-x,y-yn
1040 LET x=xn: LET y=yn
1050 RETURN
```


