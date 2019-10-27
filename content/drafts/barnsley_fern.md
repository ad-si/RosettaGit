+++
title = "Barnsley fern"
description = ""
date = 2019-10-14T01:20:09Z
aliases = []
[extra]
id = 20079
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[File:BFCpp.png|600px||right]]

A [[wp:Barnsley_fern|Barnsley fern]] is a fractal named after British mathematician Michael Barnsley and can be created using an iterated function system (IFS).


;Task:
Create this fractal fern, using the following transformations:
* ƒ1   (chosen 1% of the time)
         xn + 1 = 0
         yn + 1 = 0.16 yn

* ƒ2   (chosen 85% of the time)
         xn + 1 = 0.85 xn + 0.04 yn
         yn + 1 = −0.04 xn + 0.85 yn + 1.6

* ƒ3   (chosen 7% of the time)
         xn + 1 = 0.2 xn − 0.26 yn
         yn + 1 = 0.23 xn + 0.22 yn + 1.6

* ƒ4   (chosen 7% of the time)
         xn + 1 = −0.15 xn + 0.28 yn
         yn + 1 = 0.26 xn + 0.24 yn + 0.44.

Starting position: x = 0, y = 0





## ALGOL 68

{{works with|ALGOL 68G|any with non-standard ''establish'' routine}}

This program generates a [https://en.wikipedia.org/wiki/Netpbm_format PBM file].

```algol68

BEGIN
  INT iterations = 300000;
  LONG REAL scale x = 40, scale y = 40;
  [0:400,-200:200]CHAR canvas;

  LONG REAL x := 0, y := 0;

  FOR i FROM 1 LWB canvas TO 1 UPB canvas DO
    FOR j FROM 2 LWB canvas TO 2 UPB canvas DO
      canvas[i,j] := "0"
  OD OD;

  canvas[0, 0] := "1";
  TO iterations DO
    REAL choice := random;
    LONG REAL xn = x, yn = y;

    IF choice < 0.01 THEN
      x := 0;
      y := 0.16 * yn
    ELIF (choice -:= 0.01) < 0.85 THEN
      x :=  0.85 * xn + 0.04 * yn;
      y := -0.04 * xn + 0.85 * yn + 1.6
    ELIF (choice -:= 0.85) < 0.07 THEN
      x := 0.2  * xn - 0.26 * yn;
      y := 0.23 * xn + 0.22 * yn + 1.6
    ELSE
      x := -0.15 * xn + 0.28 * yn;
      y :=  0.26 * xn + 0.24 * yn + 0.44
    FI;

    INT px = SHORTEN ROUND (x * scale x),
        py = SHORTEN ROUND (y * scale y);
    IF px < 2 LWB canvas OR px > 2 UPB canvas OR
       py < 1 LWB canvas OR py > 1 UPB canvas
    THEN
      print(("resize canvas. px=", px, ", py=", py, new line));
      leave
    FI;

    canvas[py, px] := "1"
  OD;

  FILE f;
  IF establish(f, "fern.pbm", stand out channel) /= 0 THEN
    print("error creating file!"); leave
  FI;
  put(f, "P1"); new line(f);
  put(f, (whole((2 UPB canvas) - (2 LWB canvas) + 1, 0), " ",
          whole((1 UPB canvas) - (1 LWB canvas) + 1, 0), new line));
  FOR i FROM 1 UPB canvas BY -1 TO 1 LWB canvas DO
    put(f, canvas[i,]); new line(f)
  OD;
  close(f);
  leave: SKIP
END

```



## Applesoft BASIC


```ApplesoftBasic
 100  LET YY(1) = .16
 110  XX(2) =    .85:XY(2) = .04
 120  YX(2) =  - .04:YY(2) = .85
 130  LET Y(2) = 1.6
 140  XX(3) = .20:XY(3) =  - .26
 150  YX(3) = .23:YY(3) =    .22
 160  LET Y(3) = 1.6
 170  XX(4) =  - .15:XY(4) = .28
 180  YX(4) =    .26:YY(4) = .24
 190  LET Y(4) = .44
 200  HGR :I =  PEEK (49234)
 210  HCOLOR= 1
 220  LET X = 0:Y = 0
 230  FOR I = 1 TO 100000
 240      R =  INT ( RND (1) * 100)
 250      F = (R < 7) + (R < 14) + 2
 260      F = F - (R = 99)
 270      X = XX(F) * X + XY(F) * Y
 280      Y = YX(F) * X + YY(F) * Y
 290      Y = Y + Y(F)
 300      X% = 62 + X * 27.9
 320      Y% = 192 - Y * 19.1
 330      HPLOT X% * 2 + 1,Y%
 340  NEXT

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      GCOL 2 : REM Green Graphics Color
      X=0 : Y=0
      FOR I%=1 TO 100000
        R%=RND(100)
        CASE TRUE OF
          WHEN R% == 1 NewX= 0                 : NewY= .16 * Y
          WHEN R% <  9 NewX= .20 * X - .26 * Y : NewY= .23 * X + .22 * Y + 1.6
          WHEN R% < 16 NewX=-.15 * X + .28 * Y : NewY= .26 * X + .24 * Y + .44
          OTHERWISE    NewX= .85 * X + .04 * Y : NewY=-.04 * X + .85 * Y + 1.6
        ENDCASE
        X=NewX : Y=NewY
        PLOT 1000 + X * 130 , Y * 130
      NEXT
      END
```



## C

This implementation requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library. Iteration starts from (0,0) as required by the task however before plotting the point is translated and scaled as negative co-ordinates are not supported by the graphics window, scaling is necessary as otherwise the fern is tiny even for large iterations ( > 1000000).

```C

#include<graphics.h>
#include<stdlib.h>
#include<stdio.h>
#include<time.h>

void barnsleyFern(int windowWidth, unsigned long iter){
	
	double x0=0,y0=0,x1,y1;
	int diceThrow;
	time_t t;
	srand((unsigned)time(&t));
	
	while(iter>0){
		diceThrow = rand()%100;
		
		if(diceThrow==0){
			x1 = 0;
			y1 = 0.16*y0;
		}
		
		else if(diceThrow>=1 && diceThrow<=7){
			x1 = -0.15*x0 + 0.28*y0;
			y1 = 0.26*x0 + 0.24*y0 + 0.44;
		}
		
		else if(diceThrow>=8 && diceThrow<=15){
			x1 = 0.2*x0 - 0.26*y0;
			y1 = 0.23*x0 + 0.22*y0 + 1.6;
		}
		
		else{
			x1 = 0.85*x0 + 0.04*y0;
			y1 = -0.04*x0 + 0.85*y0 + 1.6;
		}
		
		putpixel(30*x1 + windowWidth/2.0,30*y1,GREEN);
		
		x0 = x1;
		y0 = y1;
		
		iter--;
	}

}

int main()
{
	unsigned long num;
	
	printf("Enter number of iterations : ");
	scanf("%ld",&num);
	
	initwindow(500,500,"Barnsley Fern");
	
	barnsleyFern(500,num);
	
	getch();
	
	closegraph();
	
	return 0;
}

```



## C++

[[File:BFCpp.png|200px|thumb|right]]

```cpp

#include <windows.h>
#include <ctime>
#include <string>

const int BMP_SIZE = 600, ITERATIONS = static_cast<int>( 15e5 );

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
class fern {
public:
    void draw() {
        bmp.create( BMP_SIZE, BMP_SIZE );
        float x = 0, y = 0; HDC dc = bmp.getDC();
        int hs = BMP_SIZE >> 1;
        for( int f = 0; f < ITERATIONS; f++ ) {
            SetPixel( dc, hs + static_cast<int>( x * 55.f ), 
                      BMP_SIZE - 15 - static_cast<int>( y * 55.f ), 
                      RGB( static_cast<int>( rnd() * 80.f ) + 20, 
                           static_cast<int>( rnd() * 128.f ) + 128, 
                           static_cast<int>( rnd() * 80.f ) + 30 ) ); 
            getXY( x, y );
        }
        bmp.saveBitmap( "./bf.bmp" );
    }
private:
    void getXY( float& x, float& y ) {
        float g, xl, yl;
        g = rnd();
        if( g < .01f ) { xl = 0; yl = .16f * y; } 
        else if( g < .07f ) {
            xl = .2f * x - .26f * y;
            yl = .23f * x + .22f * y + 1.6f;
        } else if( g < .14f ) {
            xl = -.15f * x + .28f * y;
            yl = .26f * x + .24f * y + .44f;
        } else {
            xl = .85f * x + .04f * y;
            yl = -.04f * x + .85f * y + 1.6f;
        }
        x = xl; y = yl;
    }
    float rnd() {
        return static_cast<float>( rand() ) / static_cast<float>( RAND_MAX );
    }
    myBitmap bmp;
};
int main( int argc, char* argv[]) {
    srand( static_cast<unsigned>( time( 0 ) ) );
    fern f; f.draw(); return 0;    
}

```


=={{header|C sharp|C#}}==


```csharp
using System;
using System.Diagnostics;
using System.Drawing;

namespace RosettaBarnsleyFern
{
    class Program
    {
        static void Main(string[] args)
        {
            const int w = 600;
            const int h = 600;
            var bm = new Bitmap(w, h);
            var r = new Random();
            double x = 0;
            double y = 0;
            for (int count = 0; count < 100000; count++)
            {
                bm.SetPixel((int)(300 + 58 * x), (int)(58 * y), Color.ForestGreen);
                int roll = r.Next(100);
                double xp = x;
                if (roll < 1)
                {
                    x = 0;
                    y = 0.16 * y;
                } else if (roll < 86)
                {
                    x = 0.85 * x + 0.04 * y;
                    y = -0.04 * xp + 0.85 * y + 1.6;
                } else if (roll < 93)
                {
                    x = 0.2 * x - 0.26 * y;
                    y = 0.23 * xp + 0.22 * y + 1.6;
                } else
                {
                    x = -0.15 * x + 0.28 * y;
                    y = 0.26 * xp + 0.24 * y + 0.44;
                }
            }
            const string filename = "Fern.png";
            bm.Save(filename);
            Process.Start(filename);
        }
    }
}
```



## Common Lisp

This code uses the <code>opticl</code> package for generating an image and saving it as a PNG file.

```lisp
(defpackage #:barnsley-fern
  (:use #:cl
        #:opticl))

(in-package #:barnsley-fern)

(defparameter *width* 800)
(defparameter *height* 800)
(defparameter *factor* (/ *height* 13))
(defparameter *x-offset* (/ *width* 2))
(defparameter *y-offset* (/ *height* 10))

(defun f1 (x y)
  (declare (ignore x))
  (values 0 (* 0.16 y)))

(defun f2 (x y)
  (values (+ (*  0.85 x) (* 0.04 y))
          (+ (* -0.04 x) (* 0.85 y) 1.6)))

(defun f3 (x y)
  (values (+ (* 0.2  x) (* -0.26 y))
          (+ (* 0.23 x) (*  0.22 y) 1.6)))

(defun f4 (x y)
  (values (+ (* -0.15 x) (* 0.28 y))
          (+ (*  0.26 x) (* 0.24 y) 0.44)))

(defun choose-transform ()
  (let ((r (random 1.0)))
    (cond ((< r 0.01) #'f1)
          ((< r 0.86) #'f2)
          ((< r 0.93) #'f3)
          (t          #'f4))))

(defun set-pixel (image x y)
  (let ((%x (round (+ (* *factor* x) *x-offset*)))
        (%y (round (- *height* (* *factor* y) *y-offset*))))
    (setf (pixel image %y %x) (values 0 255 0))))

(defun fern (filespec &optional (iterations 10000000))
  (let ((image (make-8-bit-rgb-image *height* *width* :initial-element 0))
        (x 0)
        (y 0))
    (dotimes (i iterations)
      (set-pixel image x y)
      (multiple-value-setq (x y) (funcall (choose-transform) x y)))
    (write-png-file filespec image)))
```



## Delphi

{{trans|Java}}
Hint: After putting a TPaintBox on the main form align it to alClient. Client width / height of the main form should be no less than 640 x 480.

```delphi
unit Unit1;

interface

uses
  Windows, SysUtils, Graphics, Forms, Controls, Classes, ExtCtrls;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure CreateFern(const w, h: integer);
var r, x, y: double;
    tmpx, tmpy: double;
    i: integer;
begin
    x := 0;
    y := 0;
    randomize();

    for i := 0 to 200000 do begin
        r := random(100000000) / 99999989;
        if r <= 0.01 then begin
            tmpx := 0;
            tmpy := 0.16 * y;
        end
        else if r <= 0.08 then begin
            tmpx := 0.2 * x - 0.26 * y;
            tmpy := 0.23 * x + 0.22 * y + 1.6;
        end
        else if r <= 0.15 then begin
            tmpx := -0.15 * x + 0.28 * y;
            tmpy := 0.26 * x + 0.24 * y + 0.44;
        end
        else begin
            tmpx := 0.85 * x + 0.04 * y;
            tmpy := -0.04 * x + 0.85 * y + 1.6;
        end;
        x := tmpx;
        y := tmpy;

        Form1.PaintBox1.Canvas.Pixels[round(w / 2 + x * w / 11), round(h - y * h / 11)] := clGreen;
    end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
    CreateFern(Form1.ClientWidth, Form1.ClientHeight);
end;

end.
```



## EasyLang


[https://easylang.online/apps/barnsley-fern.html Run it]

<lang>floatvars
color 060
for i% range 200000
  r = randomf
  if r < 0.01
    nx = 0
    ny = 0.16 * y
  elif r < 0.08
    nx = 0.2 * x - 0.26 * y
    ny = 0.23 * x + 0.22 * y + 1.6
  elif r < 0.15
    nx = -0.15 * x + 0.28 * y
    ny = 0.26 * x + 0.24 * y + 0.44
  else
    nx = 0.85 * x + 0.04 * y
    ny = -0.04 * x + 0.85 * y + 1.6
  .
  x = nx
  y = ny
  move 50 + x * 15 100 - y * 10
  rect 0.3 0.3
.
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Barnsley_fern this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran


```fortran

!Generates an output file "plot.dat" that contains the x and y coordinates 
!for a scatter plot that can be visualized with say, GNUPlot
program BarnsleyFern
implicit none

double precision :: p(4), a(4), b(4), c(4), d(4), e(4), f(4), trx, try, prob
integer :: itermax, i

!The probabilites and coefficients can be modified to generate other
!fractal ferns, e.g. http://www.home.aone.net.au/~byzantium/ferns/fractal.html
!probabilities
p(1) = 0.01; p(2) = 0.85; p(3) = 0.07; p(4) = 0.07

!coefficients
a(1) =  0.00; a(2) =  0.85; a(3) =  0.20; a(4) = -0.15
b(1) =  0.00; b(2) =  0.04; b(3) = -0.26; b(4) =  0.28
c(1) =  0.00; c(2) = -0.04; c(3) =  0.23; c(4) =  0.26
d(1) =  0.16; d(2) =  0.85; d(3) =  0.22; d(4) =  0.24
e(1) =  0.00; e(2) =  0.00; e(3) =  0.00; e(4) =  0.00
f(1) =  0.00; f(2) =  1.60; f(3) =  1.60; f(4) =  0.44

itermax = 100000

trx = 0.0D0
try = 0.0D0

open(1, file="plot.dat")
write(1,*) "#X            #Y"
write(1,'(2F10.5)') trx, try

do i = 1, itermax
  call random_number(prob)
  if (prob < p(1)) then
    trx = a(1) * trx + b(1) * try + e(1)
    try = c(1) * trx + d(1) * try + f(1)
  else if(prob < (p(1) + p(2))) then
    trx = a(2) * trx + b(2) * try + e(2)
    try = c(2) * trx + d(2) * try + f(2)
  else if ( prob < (p(1) + p(2) + p(3))) then
    trx = a(3) * trx + b(3) * try + e(3)
    try = c(3) * trx + d(3) * try + f(3)
  else
    trx = a(4) * trx + b(4) * try + e(4)
    try = c(4) * trx + d(4) * try + f(4)
  end if
  write(1,'(2F10.5)') trx, try
end do
close(1)
end program BarnsleyFern

```



## FreeBASIC


```freebasic
' version 10-10-2016
' compile with: fbc -s console

Sub barnsley(height As UInteger)

  Dim As Double x, y, xn, yn
  Dim As Double f = height / 10.6
  Dim As UInteger offset_x = height \ 4 - height \ 40
  Dim As UInteger n, r

  ScreenRes height \ 2, height, 32

  For n = 1 To height * 50

    r = Int(Rnd * 100)      ' f from 0 to 99

    Select Case As Const r
      Case 0 To 84
        xn  =  0.85 * x + 0.04 * y
        yn  = -0.04 * x + 0.85 * y + 1.6
      Case 85 To 91
        xn  = 0.2  * x - 0.26 * y
        yn  = 0.23 * x + 0.22 * y + 1.6
      Case 92 To 98
        xn  = -0.15 * x + 0.28 * y
        yn  =  0.26 * x + 0.24 * y + 0.44
      Case Else
        xn = 0
        yn = 0.16 * y
    End Select

    x = xn : y = yn
    PSet( offset_x + x * f, height - y * f), RGB(0, 255, 0)

  Next
' remove comment (') in next line to save window as .bmp file
' BSave "barnsley_fern_" + Str(height) + ".bmp", 0

End Sub


' ------=< MAIN >=------

' adjustable window height
' call the subroutine with the height you want
' it's possible to have a window that's large than your display
 barnsley(800)


' empty keyboard buffer
While Inkey <> "" : Wend
Windowtitle "hit any key to end program"
Sleep
End
```


=={{header|G'MIC}}==

```c

# Put this into a new file 'fern.gmic' and invoke it from the command line, like this:
# $ gmic fern.gmic -barnsley_fern

barnsley_fern :
  1024,2048
  -skip {"
      f1 = [ 0,0,0,0.16 ];           g1 = [ 0,0 ];
      f2 = [ 0.2,-0.26,0.23,0.22 ];  g2 = [ 0,1.6 ];
      f3 = [ -0.15,0.28,0.26,0.24 ]; g3 = [ 0,0.44 ];
      f4 = [ 0.85,0.04,-0.04,0.85 ]; g4 = [ 0,1.6 ];
      xy = [ 0,0 ];
      for (n = 0, n<2e6, ++n,
        r = u(100);
        xy = r<=1?((f1**xy)+=g1):
             r<=8?((f2**xy)+=g2):
             r<=15?((f3**xy)+=g3):
                   ((f4**xy)+=g4);
        uv = xy*200 + [ 480,0 ];
        uv[1] = h - uv[1];
        I(uv) = 0.7*I(uv) + 0.3*255;
      )"}
  -r 40%,40%,1,1,2

```



## gnuplot

{{trans|PARI/GP}}
{{Works with|gnuplot|5.0 (patchlevel 3) and above}}
[[File:BarnsleyFernGnu.png|right|thumb|Output BarnsleyFernGnu.png]]


```gnuplot

## Barnsley fern fractal 2/17/17 aev
reset
fn="BarnsleyFernGnu"; clr='"green"';
ttl="Barnsley fern fractal"
dfn=fn.".dat"; ofn=fn.".png";
set terminal png font arial 12 size 640,640
set print dfn append
set output ofn
unset border; unset xtics; unset ytics; unset key;
set size square 
set title ttl font "Arial:Bold,12"
n=100000; max=100; x=y=xw=yw=p=0;
randgp(top) = floor(rand(0)*top)
do for [i=1:n] {
  p=randgp(max);
  if (p==1) {xw=0;yw=0.16*y;}
  if (1<p&&p<=8) {xw=0.2*x-0.26*y;yw=0.23*x+0.22*y+1.6;}
  if (8<p&&p<=15) {xw=-0.15*x+0.28*y;yw=0.26*x+0.24*y+0.44;}
  if (p>15) {xw=0.85*x+0.04*y;yw=-0.04*x+0.85*y+1.6;}
  x=xw;y=yw; print x," ",y;
}
plot dfn using 1:2 with points  pt 7 ps 0.5 lc @clr
set output
unset print

```
 
{{Output}}

```txt

File: BarnsleyFernGnu.png
  (also BarnsleyFernGnu.dat)

```



## Go


```go
package main

import (
    "image"
    "image/color"
    "image/draw"
    "image/png"
    "log"
    "math/rand"
    "os"
)

// values from WP
const (
    xMin = -2.1820
    xMax = 2.6558
    yMin = 0.
    yMax = 9.9983
)

// parameters
var (
    width = 200
    n     = int(1e6)
    c     = color.RGBA{34, 139, 34, 255} // forest green
)

func main() {
    dx := xMax - xMin
    dy := yMax - yMin
    fw := float64(width)
    fh := fw * dy / dx
    height := int(fh)
    r := image.Rect(0, 0, width, height)
    img := image.NewRGBA(r)
    draw.Draw(img, r, &image.Uniform{color.White}, image.ZP, draw.Src)
    var x, y float64
    plot := func() {
        // transform computed float x, y to integer image coordinates
        ix := int(fw * (x - xMin) / dx)
        iy := int(fh * (yMax - y) / dy)
        img.SetRGBA(ix, iy, c)
    }
    plot()
    for i := 0; i < n; i++ {
        switch s := rand.Intn(100); {
        case s < 85:
            x, y =
                .85*x+.04*y,
                -.04*x+.85*y+1.6
        case s < 85+7:
            x, y =
                .2*x-.26*y,
                .23*x+.22*y+1.6
        case s < 85+7+7:
            x, y =
                -.15*x+.28*y,
                .26*x+.24*y+.44
        default:
            x, y = 0, .16*y
        }
        plot()
    }
    // write img to png file
    f, err := os.Create("bf.png")
    if err != nil {
        log.Fatal(err)
    }
    if err := png.Encode(f, img); err != nil {
        log.Fatal(err)
    }
}
```



## Haskell


```haskell
import Data.List (scanl')
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude
import System.Random

type Pt  = (Double, Double)

-- Four affine transformations used to produce a Barnsley fern.
f1, f2, f3, f4 :: Pt -> Pt
f1 (x, y) = (                       0,             0.16 * y)
f2 (x, y) = ( 0.85 * x + 0.04 * y    , -0.04 * x + 0.85 * y + 1.60)
f3 (x, y) = ( 0.20 * x - 0.26 * y    ,  0.23 * x + 0.22 * y + 1.60)
f4 (x, y) = (-0.15 * x + 0.28 * y    ,  0.26 * x + 0.24 * y + 0.44)

-- Given a random number in [0, 1) transform an initial point by a randomly
-- chosen function.
func :: Pt -> Double -> Pt
func p r | r < 0.01  = f1 p
         | r < 0.86  = f2 p
         | r < 0.93  = f3 p
         | otherwise = f4 p

-- Using a sequence of uniformly distributed random numbers in [0, 1) return
-- the same number of points in the fern.
fern :: [Double] -> [Pt]
fern = scanl' func (0, 0)

-- Given a supply of random values and a count, generate a diagram of a fern
-- composed of that number of points.
drawFern :: [Double] -> Int -> Diagram B
drawFern rs n = frame 0.5 . diagramFrom . take n $ fern rs
  where diagramFrom = flip atPoints (repeat dot) . map p2
        dot = circle 0.005 # lc green

-- To generate a PNG image of a fern, call this program like:
--
--   fern -o fern.png -w 640 -h 640 50000
--
-- where the arguments specify the width, height and number of points in the
-- image.
main :: IO ()
main = do
  rand <- getStdGen
  mainWith $ drawFern (randomRs (0, 1) rand)
```



## J

[[File:jfern.png|200px|thumb|right]]

```j
require 'plot'
 
f=: |: 0 ". ];._2 noun define
  0     0     0    0.16   0 0      0.01
  0.85 -0.04  0.04 0.85   0 1.60   0.85
  0.20  0.23 -0.26 0.22   0 1.60   0.07
 -0.15  0.26  0.28 0.24   0 0.44   0.07
)
 
fm=: {&(|: 2 2 $ f)
fa=: {&(|: 4 5 { f)
prob=: (+/\ 6 { f) I. ?@0:
 
ifs=: (fa@] + fm@] +/ .* [) prob
getPoints=: ifs^:(<200000)
plotFern=: 'dot;grids 0 0;tics 0 0;labels 0 0;color green' plot ;/@|:
 
   plotFern getPoints 0 0
```



## Java

[[File:barnsley_fern.png|200px|thumb|right]]
{{works with|Java|8}}

```java
import java.awt.*;
import java.awt.image.BufferedImage;
import javax.swing.*;

public class BarnsleyFern extends JPanel {

    BufferedImage img;

    public BarnsleyFern() {
        final int dim = 640;
        setPreferredSize(new Dimension(dim, dim));
        setBackground(Color.white);
        img = new BufferedImage(dim, dim, BufferedImage.TYPE_INT_ARGB);
        createFern(dim, dim);
    }

    void createFern(int w, int h) {
        double x = 0;
        double y = 0;

        for (int i = 0; i < 200_000; i++) {
            double tmpx, tmpy;
            double r = Math.random();

            if (r <= 0.01) {
                tmpx = 0;
                tmpy = 0.16 * y;
            } else if (r <= 0.08) {
                tmpx = 0.2 * x - 0.26 * y;
                tmpy = 0.23 * x + 0.22 * y + 1.6;
            } else if (r <= 0.15) {
                tmpx = -0.15 * x + 0.28 * y;
                tmpy = 0.26 * x + 0.24 * y + 0.44;
            } else {
                tmpx = 0.85 * x + 0.04 * y;
                tmpy = -0.04 * x + 0.85 * y + 1.6;
            }
            x = tmpx;
            y = tmpy;

            img.setRGB((int) Math.round(w / 2 + x * w / 11),
                    (int) Math.round(h - y * h / 11), 0xFF32CD32);
        }
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        g.drawImage(img, 0, 0, null);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Barnsley Fern");
            f.setResizable(false);
            f.add(new BarnsleyFern(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

{{trans|PARI/GP}}
[[File:BarnsleyFernjs.png|right|thumb|Output BarnsleyFernjs.png]]


```javascript

// Barnsley fern fractal
//6/17/16 aev
function pBarnsleyFern(canvasId,lim) {
  // DCLs
  var canvas = document.getElementById(canvasId);
  var ctx = canvas.getContext("2d");
  var w = canvas.width;
  var h = canvas.height;
  var x=0.,y=0.,xw=0.,yw=0.,r;
  // Like in PARI/GP: return random number 0..max-1
  function randgp(max) {return Math.floor(Math.random()*max)}
  // Clean canvas
  ctx.fillStyle="white"; ctx.fillRect(0,0,w,h);
  // MAIN LOOP
  for(var i=0; i<lim; i++) {
    r=randgp(100);
    if (r<=1) {xw=0;yw=0.16*y;}
    else if (r<=8) {xw=0.2*x-0.26*y;yw=0.23*x+0.22*y+1.6;}
    else if (r<=15) {xw=-0.15*x+0.28*y;yw=0.26*x+0.24*y+0.44;}
    else {xw=0.85*x+0.04*y;yw=-0.04*x+0.85*y+1.6;}
    x=xw;y=yw; ctx.fillStyle="green"; ctx.fillRect(x*50+260,-y*50+540,1,1);
  }//fend i
}

```
 
'''Executing:'''

```html

<html>
 <head><script src="BarnsleyFern.js"></script></head>
 <body onload="pBarnsleyFern('canvas', 100000)">
   <br /> <h3>Barnsley fern fractal</h3>
   <canvas id="canvas" width="540" height="540" style="border: 2px inset;"></canvas>
 </body>
</html>

```
 
{{Output}}

```txt

Page with BarnsleyFernjs.png

```




## Julia

{{works with|Julia|0.6}}


```julia
function barnsleyfern(n::Integer)
    funs = (
        (x, y) -> (0, 0.16y),
        (x, y) -> (0.85x + 0.04y, -0.04x + 0.85y + 1.6),
        (x, y) -> (0.2x - 0.26y, 0.23x + 0.22y + 1.6),
        (x, y) -> (-0.15x + 0,28y, 0.26x + 0.24y + 0.44))
    rst = Matrix{Float64}(n, 2)
    rst[1, :] = 0.0
    for row in 2:n
        r = rand(0:99)
        if r < 1;      f = 1;
        elseif r < 86; f = 2;
        elseif r < 93; f = 3;
        else           f = 4; end
        rst[row, 1], rst[row, 2] = funs[f](rst[row-1, 1], rst[row-1, 2])
    end
    return rst
end
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.0

import java.awt.*
import java.awt.image.BufferedImage
import javax.swing.*
 
class BarnsleyFern(private val dim: Int) : JPanel() {
    private val img: BufferedImage
    
    init {
        preferredSize = Dimension(dim, dim)
        background = Color.black
        img = BufferedImage(dim, dim, BufferedImage.TYPE_INT_ARGB)
        createFern(dim, dim)
    }

    private fun createFern(w: Int, h: Int) {
        var x = 0.0
        var y = 0.0
        for (i in 0 until 200_000) {
            var tmpx: Double
            var tmpy: Double
            val r = Math.random()
            if (r <= 0.01) {
                tmpx = 0.0
                tmpy = 0.16 * y
            }
            else if (r <= 0.86) {
                tmpx =  0.85 * x + 0.04 * y
                tmpy = -0.04 * x + 0.85 * y + 1.6
            }
            else if (r <= 0.93) {
                tmpx = 0.2  * x - 0.26 * y
                tmpy = 0.23 * x + 0.22 * y + 1.6
            } 
            else {
                tmpx = -0.15 * x + 0.28 * y
                tmpy =  0.26 * x + 0.24 * y + 0.44
            }               
            x = tmpx
            y = tmpy
            img.setRGB(Math.round(w / 2.0 + x * w / 11.0).toInt(),
                       Math.round(h - y * h / 11.0).toInt(), 0xFF32CD32.toInt())
        }
    }

    override protected fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON) 
        g.drawImage(img, 0, 0, null)
    }   
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        f.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        f.title = "Barnsley Fern"
        f.setResizable(false)
        f.add(BarnsleyFern(640), BorderLayout.CENTER)
        f.pack()
        f.setLocationRelativeTo(null)
        f.setVisible(true)
    }
}
```



## Lua

Needs L&Ouml;VE 2D Engine

```Lua

g = love.graphics
wid, hei = g.getWidth(), g.getHeight()

function choose( i, j ) 
  local r = math.random()
  if r < .01 then return 0, .16 * j 
    elseif r < .07 then return .2 * i - .26 * j, .23 * i + .22 * j + 1.6
    elseif r < .14 then return -.15 * i + .28 * j, .26 * i + .24 * j + .44
    else return .85 * i + .04 * j, -.04 * i + .85 * j + 1.6
  end
end
function createFern( iterations )
  local hw, x, y, scale = wid / 2, 0, 0, 45
  local pts = {}
  for k = 1, iterations do
    pts[1] = { hw + x * scale, hei - 15 - y * scale, 
               20 + math.random( 80 ), 
               128 + math.random( 128 ), 
               20 + math.random( 80 ), 150 }
    g.points( pts )
    x, y = choose( x, y )
  end
end
function love.load()
  math.randomseed( os.time() )
  canvas = g.newCanvas( wid, hei )
  g.setCanvas( canvas )
  createFern( 15e4 )
  g.setCanvas()
end
function love.draw()
  g.draw( canvas )
end

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica

BarnsleyFern[{x_, y_}] := Module[{},
   i = RandomInteger[{1, 100}];
   If[i <= 1, {xt = 0, yt = 0.16*y},
    If[i <= 8, {xt = 0.2*x - 0.26*y, yt = 0.23*x + 0.22*y + 1.6},
     If[i <= 15, {xt = -0.15*x + 0.28*y, yt = 0.26*x + 0.24*y + 0.44},
      {xt = 0.85*x + 0.04*y, yt = -0.04*x + 0.85*y + 1.6}]]];
   {xt, yt}];
points = NestList[BarnsleyFern, {0,0}, 100000];
Show[Graphics[{Hue[.35, 1, .7], PointSize[.001], Point[#] & /@ points}]]

```



## Nim


```Nim

import nimPNG, random

randomize()

const
  width = 640
  height = 640
  minX = -2.1815
  maxX = 2.6556
  minY = 0.0
  maxY = 9.9982
  iterations = 1_000_000

var img: array[width * height * 3, char]

proc floatToPixel(x,y:float): tuple[a:int,b:int] = 
  var px = abs(x - minX) / abs(maxX - minX)
  var py = abs(y - minY) / abs(maxY - minY)

  var a:int = (int)(width * px)
  var b:int = (int)(height * py)

  a = a.clamp(0, width-1)
  b = b.clamp(0, height-1)
  # flip the y axis
  (a:a,b:height-b-1)

proc pixelToOffset(a,b: int): int =
  b * width * 3 + a * 3

proc toString(a: openArray[char]): string =
  result = newStringOfCap(a.len)

  for ch in items(a):
    result.add(ch)

proc drawPixel(x,y:float) = 
  var (a,b) = floatToPixel(x,y)
  var offset = pixelToOffset(a,b)

  #img[offset] = 0 # red channel
  img[offset+1] = char(250) # green channel
  #img[offset+2] = 0 # blue channel

# main
var x, y: float = 0.0

for i in 1..iterations:
  var r = random(101)
  var nx, ny: float
  if r <= 85:
    nx = 0.85 * x + 0.04 * y
    ny = -0.04 * x + 0.85 * y + 1.6
  elif r <= 85 + 7:
    nx = 0.2 * x - 0.26 * y
    ny = 0.23 * x + 0.22 * y + 1.6
  elif r <= 85 + 7 + 7:
    nx = -0.15 * x + 0.28 * y
    ny = 0.26 * x + 0.24 * y + 0.44
  else:
    nx = 0
    ny = 0.16 * y

  x = nx
  y = ny

  drawPixel(x,y)

discard savePNG24("fern.png",img.toString, width, height)

```


=={{header|Oberon-2}}==
[[File:Barnsleyfern-oberon2.png|250px|thumb|right]]


```oberon2

MODULE BarnsleyFern;
(**
	Oxford Oberon-2
**)

	IMPORT Random, XYplane;

	VAR
		a1, b1, c1, d1, e1, f1, p1: REAL;
		a2, b2, c2, d2, e2, f2, p2: REAL;
		a3, b3, c3, d3, e3, f3, p3: REAL;
		a4, b4, c4, d4, e4, f4, p4: REAL;
		X, Y: REAL;
		x0, y0, e: INTEGER;

	PROCEDURE Draw;
		VAR x, y: REAL; xi, eta: INTEGER; rn: REAL;
	BEGIN
		REPEAT
			rn := Random.Uniform();
			IF rn < p1 THEN
				x := a1 * X + b1 * Y + e1; y := c1 * X + d1 * Y + f1
			ELSIF rn < (p1 + p2) THEN
				x := a2 *X + b2 * Y + e2; y := c2 * X + d2 * Y + f2
			ELSIF rn < (p1 + p2 + p3) THEN
				x := a3 * X + b3 * Y + e3; y := c3 * X + d3 * Y + f3
			ELSE
				x := a4 * X + b4 * Y + e4; y := c4 * X + d4 * Y + f4
			END;
			X := x; xi := x0 + SHORT(ENTIER(X * e));
			Y := y; eta := y0 + SHORT(ENTIER(Y * e));
			XYplane.Dot(xi, eta, XYplane.draw)
		UNTIL "s" = XYplane.Key()
	END Draw;

	PROCEDURE Init;
	BEGIN
		X := 0; Y := 0;
		x0 := 120; y0 := 0; e := 25;

		a1 := 0.00; a2 :=  0.85; a3 :=  0.20; a4 := -0.15;
		b1 := 0.00; b2 :=  0.04; b3 := -0.26; b4 :=  0.28;
		c1 := 0.00; c2 := -0.04; c3 :=	0.23; c4 :=  0.26;
		d1 := 0.16; d2 :=  0.85; d3 :=	0.22; d4 :=	 0.24;
		e1 := 0.00; e2 :=  0.00; e3 :=	0.00; e4 :=  0.00;
		f1 := 0.00; f2 :=  1.60; f3 :=	1.60; f4 :=  0.44;
		p1 := 0.01; p2 :=  0.85; p3 :=	0.07; p4 :=	 0.07;
		XYplane.Open;
	END Init;

BEGIN
	Init;Draw
END BarnsleyFern.

```



## PARI/GP

{{trans|zkl}}
{{Works with|PARI/GP|2.7.4 and above}}
[[File:BarnsleyFern.png|right|thumb|Output BarnsleyFern.png]]


```parigp

\\ Barnsley fern fractal
\\ 6/17/16 aev
pBarnsleyFern(size,lim)={
my(X=List(),Y=X,x=y=xw=yw=0.0,r);
print(" *** Barnsley Fern, size=",size," lim=",lim);
plotinit(0); plotcolor(0,6); \\green
plotscale(0, -3,3, 0,10); plotmove(0, 0,0);
for(i=1, lim,
  r=random(100);
  if(r<=1, xw=0;yw=0.16*y,
    if(r<=8, xw=0.2*x-0.26*y;yw=0.23*x+0.22*y+1.6,
      if(r<=15, xw=-0.15*x+0.28*y;yw=0.26*x+0.24*y+0.44,
        xw=0.85*x+0.04*y;yw=-0.04*x+0.85*y+1.6)));
  x=xw;y=yw; listput(X,x); listput(Y,y);
);\\fend i
plotpoints(0,Vec(X),Vec(Y));
plotdraw([0,-3,-0]);
}
{\\ Executing:
pBarnsleyFern(530,100000);  \\ BarnsleyFern.png
}

```
 
   
{{Output}}


```txt

> pBarnsleyFern(530,100000);  \\ BarnsleyFern.png
 *** Barnsley Fern, size=530 lim=100000

```



## Perl

[[File:BarnsleyFernPerl.png|250px|thumb|right]]

```perl
use Imager;

my $w = 640;
my $h = 640;

my $img = Imager->new(xsize => $w, ysize => $h, channels => 3);
my $green = Imager::Color->new('#00FF00');

my ($x, $y) = (0, 0);

foreach (1 .. 2e5) {
  my $r = rand(100);
  ($x, $y) = do {
    if    ($r <=  1) { ( 0.00 * $x - 0.00 * $y,  0.00 * $x + 0.16 * $y + 0.00) }
    elsif ($r <=  8) { ( 0.20 * $x - 0.26 * $y,  0.23 * $x + 0.22 * $y + 1.60) }
    elsif ($r <= 15) { (-0.15 * $x + 0.28 * $y,  0.26 * $x + 0.24 * $y + 0.44) }
    else             { ( 0.85 * $x + 0.04 * $y, -0.04 * $x + 0.85 * $y + 1.60) }
  };
  $img->setpixel(x => $w / 2 + $x * 60, y => $y * 60, color => $green);
}

$img->flip(dir => 'v');
$img->write(file => 'barnsleyFern.png');
```



## Perl 6

{{works with|Rakudo|2016.03}}
{{trans|Perl}}
[[File:Barnsley-fern-perl6.png|250px|thumb|right]]

```perl6
use Image::PNG::Portable;

my ($w, $h) = (640, 640);

my $png = Image::PNG::Portable.new: :width($w), :height($h);

my ($x, $y) = (0, 0);

for ^2e5 {
    my $r = 100.rand;
    ($x, $y) = do given $r {
        when  $r <=  1 { (                     0,              0.16 * $y       ) }
        when  $r <=  8 { ( 0.20 * $x - 0.26 * $y,  0.23 * $x + 0.22 * $y + 1.60) }
        when  $r <= 15 { (-0.15 * $x + 0.28 * $y,  0.26 * $x + 0.24 * $y + 0.44) }
        default        { ( 0.85 * $x + 0.04 * $y, -0.04 * $x + 0.85 * $y + 1.60) }
    };
    $png.set(($w / 2 + $x * 60).Int, $h - ($y * 60).Int, 0, 255, 0);
}

$png.write: 'Barnsley-fern-perl6.png';
```



## Phix

{{libheader|pGUI}}
This file is included in the distro as 
[[File:PhixBarnsleyFern.png|250px|thumb|right]]

```Phix
--
-- demo\rosetta\BarnsleyFern.exw
--
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
atom {x,y,r} @= 0
integer {width, height} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    for i=1 to 20000 do
        r = rand(100)
        {x, y} = iff(r<=1? {             0,        0.16*y     } :
                 iff(r<=8? { 0.20*x-0.26*y, 0.23*x+0.22*y+1.60} :
                 iff(r<=15?{-0.15*x+0.28*y, 0.26*x+0.24*y+0.44} :
                           { 0.85*x+0.04*y,-0.04*x+0.85*y+1.60})))
        cdCanvasPixel(cddbuffer, width/2+x*60, y*60, #00FF00) 
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
    IupSetAttribute(canvas, "RASTERSIZE", "340x620") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Barnsley Fern")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL) -- release the minimum limitation
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PicoLisp


```PicoLisp
`(== 64 64)
(seed (in "/dev/urandom" (rd 8)))
(scl 20)
(de gridX (X)
   (*/ (+ 320.0 (*/ X 58.18 1.0)) 1.0) )
(de gridY (Y)
   (*/ (- 640.0 (*/ Y 58.18 1.0)) 1.0) )
(de calc (R X Y)
   (cond
      ((< R 1) (list 0 (*/ Y 0.16 1.0)))
      ((< R 86)
         (list
            (+ (*/ 0.85 X 1.0) (*/ 0.04 Y 1.0))
            (+ (*/ -0.04 X 1.0) (*/ 0.85 Y 1.0) 1.6) ) )
      ((< R 93)
         (list
            (- (*/ 0.2 X 1.0) (*/ 0.26 Y 1.0))
            (+ (*/ 0.23 X 1.0) (*/ 0.22 Y 1.0) 1.6) ) ) 
      (T
         (list
            (+ (*/ -0.15 X 1.0) (*/ 0.28 Y 1.0))
            (+ (*/ 0.26 X 1.0) (*/ 0.24 Y 1.0) 0.44) ) ) ) )
(let
   (X 0
      Y 0 
      G (make (do 640 (link (need 640 0)))) )
   (do 100000
      (let ((A B) (calc (rand 0 99) X Y))
         (setq X A  Y B)
         (set (nth G (gridY Y) (gridX X)) 1) ) ) 
   (out "fern.pbm"
      (prinl "P1")
      (prinl 640 " " 640)
      (mapc prinl G) ) )
```



## Processing


```java
void setup() {
  size(640, 640);
  background(0, 0, 0);
}

float x = 0;
float y = 0;

void draw() {
  for (int i = 0; i < 100000; i++) {

    float xt = 0;
    float yt = 0;

    float r = random(100);

    if (r <= 1) {
      xt = 0;
      yt = 0.16*y;
    } else if (r <= 8) {
      xt = 0.20*x - 0.26*y;
      yt = 0.23*x + 0.22*y + 1.60;
    } else if (r <= 15) {
      xt = -0.15*x + 0.28*y;
      yt =  0.26*x + 0.24*y + 0.44;
    } else {
      xt =  0.85*x + 0.04*y;
      yt = -0.04*x + 0.85*y + 1.60;
    }

    x = xt;
    y = yt;

    int m = round(width/2 + 60*x);
    int n = height-round(60*y);

    set(m, n, #00ff00);
  }
  noLoop();
}
```



## PureBasic


```PureBasic
EnableExplicit
DisableDebugger

DataSection
  R84:  : Data.d 0.85,0.04,-0.04,0.85,1.6
  R91:  : Data.d 0.2,-0.26,0.23,0.22,1.6
  R98:  : Data.d -0.15,0.28,0.26,0.24,0.44
  R100: : Data.d 0.0,0.0,0.0,0.16,0.0
EndDataSection

Procedure Barnsley(height.i) 
  Define x.d, y.d, xn.d, yn.d, v1.d, v2.d, v3.d, v4.d, v5.d,
         f.d=height/10.6,
         offset.i=Int(height/4-height/40),
         n.i, r.i  
  For n=1 To height*50
    r=Random(99,0)
    Select r
      Case 0 To 84  : Restore R84
      Case 85 To 91 : Restore R91
      Case 92 To 98 : Restore R98
      Default       : Restore R100
    EndSelect
    Read.d v1 : Read.d v2 : Read.d v3 : Read.d v4 : Read.d v5
    xn=v1*x+v2*y : yn=v3*x+v4*y+v5    
    x=xn : y=yn
    Plot(offset+x*f,height-y*f,RGB(0,255,0))
  Next
EndProcedure

Define w1.i=400,
       h1.i=800

If OpenWindow(0,#PB_Ignore,#PB_Ignore,w1,h1,"Barnsley fern")
  If CreateImage(0,w1,h1,24,0) And StartDrawing(ImageOutput(0))        
    Barnsley(h1)
    StopDrawing()
  EndIf
  ImageGadget(0,0,0,0,0,ImageID(0))  
  Repeat : Until WaitWindowEvent(50)=#PB_Event_CloseWindow
EndIf
End
```



## Python


```Python


import random
from PIL import Image


class BarnsleyFern(object):
    def __init__(self, img_width, img_height, paint_color=(0, 150, 0),
                 bg_color=(255, 255, 255)):
        self.img_width, self.img_height = img_width, img_height
        self.paint_color = paint_color
        self.x, self.y = 0, 0
        self.age = 0

        self.fern = Image.new('RGB', (img_width, img_height), bg_color)
        self.pix = self.fern.load()
        self.pix[self.scale(0, 0)] = paint_color

    def scale(self, x, y):
        h = (x + 2.182)*(self.img_width - 1)/4.8378
        k = (9.9983 - y)*(self.img_height - 1)/9.9983
        return h, k

    def transform(self, x, y):
        rand = random.uniform(0, 100)
        if rand < 1:
            return 0, 0.16*y
        elif 1 <= rand < 86:
            return 0.85*x + 0.04*y, -0.04*x + 0.85*y + 1.6
        elif 86 <= rand < 93:
            return 0.2*x - 0.26*y, 0.23*x + 0.22*y + 1.6
        else:
            return -0.15*x + 0.28*y, 0.26*x + 0.24*y + 0.44

    def iterate(self, iterations):
        for _ in range(iterations):
            self.x, self.y = self.transform(self.x, self.y)
            self.pix[self.scale(self.x, self.y)] = self.paint_color
        self.age += iterations

fern = BarnsleyFern(500, 500)
fern.iterate(1000000)
fern.fern.show()


```



## R

{{trans|PARI/GP}}
[[File:BarnsleyFernR.png|right|thumb|Output BarnsleyFernR.png]]


```r

## pBarnsleyFern(fn, n, clr, ttl, psz=600): Plot Barnsley fern fractal.
## Where: fn - file name; n - number of dots; clr - color; ttl - plot title;
## psz - picture size.
## 7/27/16 aev
pBarnsleyFern <- function(fn, n, clr, ttl, psz=600) {
  cat(" *** START:", date(), "n=", n, "clr=", clr, "psz=", psz, "\n");
  cat(" *** File name -", fn, "\n");
  pf = paste0(fn,".png"); # pf - plot file name
  A1 <- matrix(c(0,0,0,0.16,0.85,-0.04,0.04,0.85,0.2,0.23,-0.26,0.22,-0.15,0.26,0.28,0.24), ncol=4, nrow=4, byrow=TRUE);
  A2 <- matrix(c(0,0,0,1.6,0,1.6,0,0.44), ncol=2, nrow=4, byrow=TRUE);
  P <- c(.01,.85,.07,.07);
  # Creating matrices M1 and M2.
  M1=vector("list", 4); M2 = vector("list", 4);
  for (i in 1:4) {
    M1[[i]] <- matrix(c(A1[i,1:4]), nrow=2);
    M2[[i]] <- matrix(c(A2[i, 1:2]), nrow=2);
  }
  x <- numeric(n); y <- numeric(n);
  x[1] <- y[1] <- 0;
  for (i in 1:(n-1)) {
    k <- sample(1:4, prob=P, size=1);
    M <- as.matrix(M1[[k]]);
    z <- M%*%c(x[i],y[i]) + M2[[k]]; 
    x[i+1] <- z[1]; y[i+1] <- z[2];
  }
  plot(x, y, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, cex=0.1);
  # Writing png-file
  dev.copy(png, filename=pf,width=psz,height=psz);
  # Cleaning 
  dev.off(); graphics.off();
  cat(" *** END:",date(),"\n");
}
## Executing:
pBarnsleyFern("BarnsleyFernR", 100000, "dark green", "Barnsley Fern Fractal", psz=600)

```
 

{{Output}}


```txt

> pBarnsleyFern("BarnsleyFernR", 100000, "dark green", "Barnsley Fern Fractal", psz=600)
 *** START: Wed Jul 27 13:50:49 2016 n= 1e+05 clr= dark green psz= 600 
 *** File name - BarnsleyFernR 
 *** END: Wed Jul 27 13:50:56 2016 
+ BarnsleyFernR.png file

```



## Racket

[[File:racket-barnsley-fern.png]] : file uploading broken :-(

```racket
#lang racket

(require racket/draw)

(define fern-green (make-color #x32 #xCD #x32 0.66))

(define (fern dc n-iterations w h)
  (for/fold ((x #i0) (y #i0))
            ((i n-iterations))
    (define-values (x′ y′)
      (let ((r (random)))
        (cond
          [(<= r 0.01) (values 0
                               (* y 16/100))]
          [(<= r 0.08) (values (+ (* x 20/100) (* y -26/100))
                               (+ (* x 23/100) (* y 22/100) 16/10))]
          [(<= r 0.15) (values (+ (* x -15/100) (* y 28/100))
                               (+ (* x 26/100) (* y 24/100) 44/100))]
          [else (values (+ (* x 85/100) (* y 4/100))
                        (+ (* x -4/100) (* y 85/100) 16/10))])))

    (define px (+ (/ w 2) (* x w 1/11)))
    (define py (- h (* y h 1/11)))
    (send dc set-pixel (exact-round px) (exact-round py) fern-green)                   
    (values x′ y′)))


(define bmp (make-object bitmap% 640 640 #f #t 2))

(fern (new bitmap-dc% [bitmap bmp]) 200000 640 640)

bmp
(send bmp save-file "images/racket-barnsley-fern.png" 'png)
```



## REXX

This REXX version is modeled after the   '''Fortran'''   entry;   it
generates an output file   ("BARNSLEY.DAT")   that 

contains the   '''X'''   and   '''Y'''   coördinates for a scatter plot that can be
visualized with a plotting program.

```rexx
/*REXX pgm gens X & Y coördinates for a scatter plot to be used to show a Barnsley fern.*/
parse arg N FID seed .                           /*obtain optional arguments from the CL*/
if   N=='' |   N==","  then   N= 100000          /*Not specified?   Then use the default*/
if FID=='' | FID==","  then FID= 'BARNSLEY.DAT'  /* "      "          "   "   "     "   */
if datatype(seed,'W')  then call random ,,seed   /*if specified, then use random seed.  */
call lineout FID, , 1                            /*just set the file ptr to the 1st line*/
x=0                                              /*set the initial value for  X  coörd. */
y=0                                              /* "   "     "      "    "   Y    "    */
    do #=1  for N                                /*generate   N   number of plot points.*/
    ?=random(, 100)                              /*generate a random number: 0 ≤ ? ≤ 100*/
       select
       when ?==0  then do;   xx=   0           ;    yy=            .16*y         ;     end
       when ?< 8  then do;   xx=  .2 *x - .26*y;    yy=  .23*x  +  .22*y  +  1.6 ;     end
       when ?<15  then do;   xx= -.15*x + .28*y;    yy=  .26*x  +  .24*y  +   .44;     end
       otherwise             xx=  .85*x + .04*y;    yy= -.04*x  +  .85*y  +  1.6
       end   /*select*/
                             x=xx;                     y=yy
    if #==1  then  do;    minx= x;  maxx= x;        miny= y;  maxy= y
                   end
                          minx= min(minx, x);       miny= min(miny, y)
                          maxx= max(maxx, x);       maxy= max(maxy, y)
    call lineout FID, x","y
    end      /*#*/                               /* [↓]  close the file (safe practice).*/
call lineout FID                                 /*stick a fork in it,  we're all done. */
```

{{out|output|text=  is generated to an output file:   BARNSLEY.DAT   which contains the   '''X'''   and   '''Y'''   coördinates of a scatter plot.}}




## Ring


```Ring


Load "guilib.ring"

/*
 +---------------------------------------------------------------------------
 +        Program Name : Draw Barnsley Fern
 +        Purpose      : Draw Fern using Quadratic Equation and Random Number
 +---------------------------------------------------------------------------
*/

###-------------------------------
### DRAW CHART  size 400 x 500
###-------------------------------


New qapp {
	win1 = new qwidget() {
			### Position and Size on Screen
			setwindowtitle("Drawing using QPainter")
			setgeometry( 10, 25, 400, 500)

			### Draw within this Win Box
			label1 = new qlabel(win1) {
					### Label Position and Size
					setgeometry(10, 10, 400, 500)
					settext(" ")
			}

			buttonFern = new qpushbutton(win1) {
					### Button DrawFern
					setgeometry(10, 10, 80, 20)
					settext("Draw Fern")
					setclickevent("DrawFern()")     ### Call DRAW function
			}

			show()
	}
	exec()
}

###------------------------
### FUNCTIONS
###------------------------

Func DrawFern
		p1 = new qpicture()
		
		colorGreen = new qcolor() { setrgb(0,255,0,255) }
		penGreen   = new qpen()   { setcolor(colorGreen)    setwidth(1) }
				 
		new qpainter() {
			begin(p1)
			setpen(penGreen)
															 
				###-------------------------------------
				### Quadratic equation matrix of arrays
				
				a = [ 0,    0.85,  0.2,  -0.15 ]
				b = [ 0,    0.04, -0.26,  0.28 ]
				c = [ 0,   -0.04,  0.23,  0.26 ]
				d = [ 0.16, 0.85,  0.22,  0.24 ]
				e = [ 0,    0,     0,     0    ]
				f = [ 0,    1.6,   1.6,   0.44 ]

				### Initialize x, y points
				
				xf = 0.0
				yf = 0.0

				### Size of output screen
				
				MaxX = 400
				MaxY = 500
				MaxIterations = MaxY * 200
				Count = 0

				###------------------------------------------------
				
				while ( Count <= MaxIterations )
				
					### NOTE *** RING *** starts at Index 1, 
					### Do NOT use Random K=0 result
					
					k = random() % 100
					k = k +1    
					
					### if  (k = 0)                  k = 1  ok   ### Do NOT use 
					 
						if ((k > 0)  and (k <= 85))  k = 2  ok
						if ((k > 85) and (k <= 92))  k = 3  ok
						if  (k > 92)                 k = 4  ok
						
					TempX = ( a[k] * xf ) + ( b[k] * yf ) + e[k]                            
					TempY = ( c[k] * xf ) + ( d[k] * yf ) + f[k]

					xf = TempX
					yf = TempY

					if( (Count >= MaxIterations) or (Count != 0) )                        
						xPoint = (floor(xf *  MaxY / 11) + floor(MaxX / 2))
						yPoint = (floor(yf * -MaxY / 11) + MaxY )                             
						drawpoint( xPoint , yPoint  )
					ok  
					
					Count++                           
				end

				###----------------------------------------------------
				
			endpaint()
		}
		
		label1 { setpicture(p1) show() }
return                      
 

```



## Run BASIC


```runbasic
'Barnsley Fern - Run BASIC
  'http://rosettacode.org/wiki/Barnsley_fern#Run_BASIC
  'copy code and run it at http://www.runbasic.com
  '
  ' -----------------------------------
  ' Barnsley Fern
  ' -----------------------------------maxpoints	= 20000
graphic #g, 200, 200
#g fill("blue")
FOR n	= 1 TO maxpoints
p	= RND(0)*100
IF p <= 1 THEN 
	nx	= 0 
	ny	= 0.16 * y
else if p <= 8 THEN 
	nx	= 0.2 * x - 0.26 * y
	ny	= 0.23 * x + 0.22 * y + 1.6
else if p <= 15 THEN 
	nx	= -0.15 * x + 0.28 * y
	ny	= 0.26 * x + 0.24 * y + 0.44
else
	nx	= 0.85 * x +0.04 * y
	ny	= -0.04 * x +0.85 * y + 1.6
end if
x	= nx
y	= ny
#g "color green ; set "; x * 17 + 100; " "; y * 17

NEXT n
render #g
#g "flush"
```



## Rust

{{trans|Java}}
{{libheader|rand}}


```rust
extern crate rand;
extern crate raster;

use rand::Rng;

fn main() {
    let max_iterations = 200_000u32;
    let height = 640i32;
    let width = 640i32;

    let mut rng = rand::thread_rng();
    let mut image = raster::Image::blank(width, height);
    raster::editor::fill(&mut image, raster::Color::white()).unwrap();

    let mut x = 0.;
    let mut y = 0.;
    for _ in 0..max_iterations {
        let r = rng.gen::<f32>();
        let cx: f64;
        let cy: f64;

        if r <= 0.01 {
            cx = 0f64;
            cy = 0.16 * y as f64;
        } else if r <= 0.08 {
            cx = 0.2 * x as f64 - 0.26 * y as f64;
            cy = 0.23 * x as f64 + 0.22 * y as f64 + 1.6;
        } else if r <= 0.15 {
            cx = -0.15 * x as f64 + 0.28 * y as f64;
            cy = 0.26 * x as f64 + 0.26 * y as f64 + 0.44;
        } else {
            cx = 0.85 * x as f64 + 0.04 * y as f64;
            cy = -0.04 * x as f64 + 0.85 * y as f64 + 1.6;
        }
        x = cx;
        y = cy;

        let _ = image.set_pixel(
            ((width as f64) / 2. + x * (width as f64) / 11.).round() as i32,
            ((height as f64) - y * (height as f64) / 11.).round() as i32,
            raster::Color::rgb(50, 205, 50));
    }

    raster::save(&image, "fractal.png").unwrap();
}
```



## Scala


### Java Swing Interoperability


```Scala
import java.awt._
import java.awt.image.BufferedImage

import javax.swing._

object BarnsleyFern extends App {

  SwingUtilities.invokeLater(() =>
    new JFrame("Barnsley Fern") {

      private class BarnsleyFern extends JPanel {
        val dim = 640
        val img = new BufferedImage(dim, dim, BufferedImage.TYPE_INT_ARGB)

        private def createFern(w: Int, h: Int): Unit = {
          var x, y = 0.0
          for (i <- 0 until 200000) {
            var tmpx, tmpy = .0
            val r = math.random
            if (r <= 0.01) {
              tmpx = 0
              tmpy = 0.16 * y
            }
            else if (r <= 0.08) {
              tmpx = 0.2 * x - 0.26 * y
              tmpy = 0.23 * x + 0.22 * y + 1.6
            }
            else if (r <= 0.15) {
              tmpx = -0.15 * x + 0.28 * y
              tmpy = 0.26 * x + 0.24 * y + 0.44
            }
            else {
              tmpx = 0.85 * x + 0.04 * y
              tmpy = -0.04 * x + 0.85 * y + 1.6
            }
            x = tmpx
            y = tmpy
            img.setRGB((w / 2 + tmpx * w / 11).round.toInt,
              (h - tmpy * h / 11).round.toInt, 0xFF32CD32)
          }
        }

        override def paintComponent(gg: Graphics): Unit = {
          super.paintComponent(gg)
          val g = gg.asInstanceOf[Graphics2D]
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          g.drawImage(img, 0, 0, null)
        }

        setBackground(Color.white)
        setPreferredSize(new Dimension(dim, dim))
        createFern(dim, dim)
      }

      add(new BarnsleyFern, BorderLayout.CENTER)
      pack()
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setLocationRelativeTo(null)
      setResizable(false)
      setVisible(true)
    })

}
```


## Scheme

This version creates a list of points, defining the fern, which are then rescaled and output to an eps file.

```scheme
(import (scheme base)
        (scheme cxr)
        (scheme file)
        (scheme inexact)
        (scheme write)
        (srfi 27))     ; for random numbers

(define (create-fern x y num-points)
  (define (new-point xn yn)
    (let ((r (* 100 (random-real))))
      (cond ((< r 1) ; f1
             (list 0 (* 0.16 yn)))
            ((< r 86) ; f2
             (list (+ (* 0.85 xn) (* 0.04 yn))
                   (+ (* -0.04 xn) (* 0.85 yn) 1.6)))
            ((< r 93) ; f3
             (list (- (* 0.2 xn) (* 0.26 yn))
                   (+ (* 0.23 xn) (* 0.22 yn) 1.6)))
            (else ; f4
              (list (+ (* -0.15 xn) (* 0.28 yn))
                    (+ (* 0.26 xn) (* 0.24 yn) 0.44))))))
  ;
  (random-source-randomize! default-random-source)
  (do ((i 0 (+ i 1))
       (pts (list (list x y)) (cons (new-point (caar pts) (cadar pts)) pts)))
    ((= i num-points) pts)))

;; output the fern to an eps file
(define (output-fern-as-eps filename fern)
  (when (file-exists? filename) (delete-file filename))
  (with-output-to-file
    filename
    (lambda ()
      (let* ((width 600)
             (height 800)
             (min-x (apply min (map car fern)))
             (max-x (apply max (map car fern)))
             (min-y (apply min (map cadr fern)))
             (max-y (apply max (map cadr fern)))
             (scale-x (/ (- width 50) (- max-x min-x)))
             (scale-y (/ (- height 50) (- max-y min-y)))
             (scale-points (lambda (point)
                             (list (truncate (+ 20 (* scale-x (- (car point) min-x))))
                                   (truncate (+ 20 (* scale-y (- (cadr point) min-y))))))))

        (display 
          (string-append "%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: 0 0 "
                         (number->string width) " " (number->string height) "\n"))

        ;; add each point in fern as an arc - sets linewidth based on depth in tree
        (for-each (lambda (point)
                    (display
                      (string-append (number->string (list-ref point 0))
                                     " "
                                     (number->string (list-ref point 1))
                                     " 0.1 0 360 arc\nstroke\n")))
                  (map scale-points fern))
        (display "\n%%EOF")))))

(output-fern-as-eps "barnsley.eps" (create-fern 0 0 50000))
```



## Scilab

{{Works with|Scilab|5.4.0 and above}}
This version creates a list of points, defining the fern, and shows them on a graphic window which can then be saved to a file via the GUI or the console by the user.
<lang>
iteractions=1.0d6;

XY=zeros(2,iteractions+1);
x=0;
y=0;

i=2;
while i<iteractions+2
    random_numbers=rand();
    xp=x;
    if random_numbers(1) < 0.01 then
        x = 0;
        y = 0.16 * y;
    elseif random_numbers(1) >= 0.01 & random_numbers(1) < 0.01+0.85 then
        x = 0.85 * x + 0.04 * y;
        y = -0.04 * xp + 0.85 * y + 1.6;
    elseif random_numbers(1) >= 0.86 & random_numbers(1) < 0.86+0.07 then
        x = 0.2 * x - 0.26 * y;
        y = 0.23 * xp + 0.22 * y + 1.6;
    else
        x = -0.15 * x + 0.28 * y;
        y = 0.26 * xp + 0.24 * y + 0.44;
    end
    
    XY(1,i)=x;
    XY(2,i)=y;
    
    i=i+1;
end

scf(0);
clf();
xname('Barnsley fern');
plot2d(XY(1,:),XY(2,:),-0)
axes=gca();
axes.isoview="on";
axes.children.children.mark_foreground=13;

```



## SequenceL

'''Tail-Recursive SequenceL Code:'''


```sequencel>import <Utilities/Math.sl
;
import <Utilities/Random.sl>;

transform(p(1), rand) := 
    let
        x := p[1]; y := p[2];
    in
        [0.0, 0.16*y] when rand <= 0.01
    else
        [0.85*x + 0.04*y, -0.04*x + 0.85*y + 1.6] when rand <= 0.86
    else
        [0.2*x - 0.26*y, 0.23*x + 0.22*y + 1.6] when rand <= 0.93
    else
        [-0.15*x + 0.28*y, 0.26*x + 0.24*y + 0.44];
        
barnsleyFern(rand, count, result(2)) :=
    let
        nextRand := getRandom(rand);
        next := transform(result[size(result)], nextRand.value / 2147483647.0);
    in
        result when count <= 0
    else
        barnsleyFern(nextRand.generator, count - 1, result ++ [next]);

scale(p(1), width, height) := [round((p[1] + 2.182) * width / 4.8378),
                               round((9.9983 - p[2]) * height / 9.9983)];
        
entry(seed, count, width, height) :=
    let
        fern := barnsleyFern(seedRandom(seed), count, [[0.0,0.0]]);
    in
        scale(fern, width, height);
```


'''C++ Driver Code:'''

{{libheader|CImg}}

```c
#include "SL_Generated.h"
#include "CImg.h"

using namespace cimg_library;

int main(int argc, char** argv)
{
    int threads = 0; if(argc > 1) threads = atoi(argv[1]);
    int width = 300; if(argc > 2) width = atoi(argv[2]);
    int height = 600; if(argc > 3) height = atoi(argv[3]);
    int steps = 10000; if(argc > 4) steps = atoi(argv[4]);
    int seed = 314159; if(argc > 5) seed = atoi(argv[5]);
    
    CImg<unsigned char> visu(width, height, 1, 3, 0);
    Sequence< Sequence<int> > result;

    sl_init(threads);

    sl_entry(seed, steps, width-1, height-1, threads, result);
    
    visu.fill(0);
    for(int i = 1; i <= result.size(); i++)
        visu(result[i][1], result[i][2],1) = 255;
    
    CImgDisplay draw_disp(visu);
    draw_disp.set_title("Barnsley Fern in SequenceL");
    visu.display(draw_disp);
    
    while(!draw_disp.is_closed()) draw_disp.wait();

    sl_done();

    return 0;
}
```


{{out}}
[https://i.imgur.com/zerRZo8.png Output Screenshot]



## Sidef


```ruby
require('Imager')

var w = 640
var h = 640

var img   = %O<Imager>.new(xsize => w, ysize => h, channels => 3)
var green = %O<Imager::Color>.new('#00FF00')

var (x, y) = (0.float, 0.float)

1e5.times {
  var r = 100.rand
  (x, y) = (
    if    (r <=  1) { ( 0.00*x - 0.00*y,  0.00*x + 0.16*y + 0.00) }
    elsif (r <=  8) { ( 0.20*x - 0.26*y,  0.23*x + 0.22*y + 1.60) }
    elsif (r <= 15) { (-0.15*x + 0.28*y,  0.26*x + 0.24*y + 0.44) }
    else            { ( 0.85*x + 0.04*y, -0.04*x + 0.85*y + 1.60) }
  )
  img.setpixel(x => w/2 + 60*x, y => 60*y, color => green)
}

img.flip(dir => 'v')
img.write(file => 'barnsleyFern.png')
```

Output image: [https://github.com/trizen/rc/blob/master/img/barnsley-fern-sidef.png Barnsley fern]


## SPL


```spl
w,h = #.scrsize()
x,y = 0
>
  r = #.rnd(100)
  ? r<85, x,y = f2(x,y)
  ? r!<85 & r<92, x,y = f3(x,y)
  ? r!<92 & r<99, x,y = f4(x,y)
  ? r!<99, x,y = f1(y)
  #.drawpoint(x/10*w+w/2,h-y/10*h,0,0.5,0,0.1)
<
f1(y) <= 0, 0.16*y
f2(x,y) <= 0.85*x+0.04*y, -0.04*x+0.85*y+1.6
f3(x,y) <= 0.2*x-0.26*y, 0.23*x+0.22*y+1.6
f4(x,y) <= -0.15*x+0.28*y, 0.26*x+0.24*y+0.44
```



## Swift

Output is viewable in a playground.


```swift
import UIKit
import CoreImage
import PlaygroundSupport

let imageWH = 300
let context = CGContext(data: nil,
                        width: imageWH,
                        height: imageWH,
                        bitsPerComponent: 8,
                        bytesPerRow: 0,
                        space: CGColorSpace(name: CGColorSpace.sRGB)!,
                        bitmapInfo: CGImageAlphaInfo.premultipliedFirst.rawValue)!
var x0 = 0.0
var x1 = 0.0
var y0 = 0.0
var y1 = 0.0

context.setFillColor(#colorLiteral(red: 0, green: 0, blue: 0, alpha: 1))
context.fill(CGRect(x: 0, y: 0, width: imageWH, height: imageWH))
context.setFillColor(#colorLiteral(red: 0.539716677, green: 1, blue: 0.265400682, alpha: 1))

for _ in 0..<100_000 {
    switch Int(arc4random()) % 100 {
    case 0:
        x1 = 0
        y1 = 0.16 * y0
    case 1...7:
        x1 = -0.15 * x0 + 0.28 * y0
        y1 = 0.26 * x0 + 0.24 * y0 + 0.44
    case 8...15:
        x1 = 0.2 * x0 - 0.26 * y0
        y1 = 0.23 * x0 + 0.22 * y0 + 1.6
    default:
        x1 = 0.85 * x0 + 0.04 * y0
        y1 = -0.04 * x0 + 0.85 * y0 + 1.6
    }
    
    context.fill(CGRect(x: 30 * x1 + Double(imageWH) / 2.0, y: 30 * y1,
                        width: 1, height: 1))
    
    (x0, y0) = (x1, y1)
}

let uiImage = UIImage(cgImage: context.makeImage()!)
```



## VBA


```vb
Private Sub plot_coordinate_pairs(x As Variant, y As Variant)
    Dim chrt As Chart
    Set chrt = ActiveSheet.Shapes.AddChart.Chart
    With chrt
        .ChartType = xlXYScatter
        .HasLegend = False
        .SeriesCollection.NewSeries
        .SeriesCollection.Item(1).XValues = x
        .SeriesCollection.Item(1).Values = y
    End With
End Sub
Public Sub barnsley_fern()
    Const MAX = 50000
    Dim x(MAX) As Double
    Dim y(MAX) As Double
    x(0) = 0: y(0) = 0
    For i = 1 To MAX
        Select Case CInt(100 * Rnd)
            Case 0 To 1
                x(i) = 0
                y(i) = 0.16 * y(i - 1)
            Case 2 To 85
                x(i) = 0.85 * x(i - 1) + 0.04 * y(i - 1)
                y(i) = -0.04 * x(i - 1) + 0.85 * y(i - 1) + 1.6
            Case 86 To 92
                x(i) = 0.2 * x(i - 1) - 0.26 * y(i - 1)
                y(i) = 0.23 * x(i - 1) + 0.22 * y(i - 1) + 1.6
            Case 93 To 100
                x(i) = -0.15 * x(i - 1) + 0.28 * y(i - 1)
                y(i) = 0.26 * x(i - 1) + 0.24 * y(i - 1) + 0.44
        End Select
    Next i
    plot_coordinate_pairs x, y
End Sub
```


## Yabasic

{{trans|ZX Spectrum Basic}}
Classic style

```Yabasic
10 REM Fractal Fern
20 LET wid = 800 : LET hei = 600 : open window wid, hei : window origin "cb"
25 backcolor 0, 0, 0 : color 0, 255, 0 : clear window
30 LET maxpoints=wid*hei/2: LET x=0: LET y=0
40 FOR n=1 TO maxpoints
50 LET p=RAN(100)
60 IF p<=1 LET nx=0: LET ny=0.16*y: GOTO 100
70 IF p<=8 LET nx=0.2*x-0.26*y: LET ny=0.23*x+0.22*y+1.6: GOTO 100
80 IF p<=15 LET nx=-0.15*x+0.28*y: LET ny=0.26*x+0.24*y+0.44: GOTO 100
90 LET nx=0.85*x+0.04*y: LET ny=-0.04*x+0.85*y+1.6
100 LET x=nx: LET y=ny
110 DOT x*wid/12,y*hei/12
120 NEXT n
```

Modern style 
```Yabasic
REM Fractal Fern
wid = 800 : hei = 600 : open window wid, hei : window origin "cb"
backcolor 0, 0, 0 : color 0, 255, 0 : clear window
maxpoints = wid * hei / 2 : x = 0 : y = 0
for n = 1 to maxpoints
   p = ran(100)
   if p <= 1 then nx = 0 : ny = 0.16 * y
   elseif p <= 8 then nx = 0.2 * x - 0.26 * y : ny = 0.23 * x + 0.22 * y + 1.6
   elseif p <= 15 then nx = -0.15 * x + 0.28 * y : ny = 0.26 * x + 0.24 * y + 0.44
   else nx = 0.85 * x + 0.04 * y : ny = -0.04 * x + 0.85 * y + 1.6
   end if
   x = nx : y = ny
   dot x * wid / 12, y * hei / 12
next
```



## zkl

[[File:BarnsleyFern.zkl.jpg|250px|thumb|right]]
Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
{{trans|Java}}

```zkl
fcn barnsleyFern(){
   w,h:=640,640;
   bitmap:=PPM(w+1,h+1,0xFF|FF|FF);  // White background

   x,y, nx,ny:=0.0, 0.0, 0.0, 0.0;
   do(0d100_000){
      r:=(0).random(100);  // [0..100)%
      if     (r<= 1) nx,ny= 0, 		      0.16*y;
      else if(r<= 8) nx,ny= 0.2*x  - 0.26*y,  0.23*x + 0.22*y + 1.6;
      else if(r<=15) nx,ny=-0.15*x + 0.28*y,  0.26*x + 0.24*y + 0.44;
      else           nx,ny= 0.85*x + 0.04*y, -0.04*x + 0.85*y + 1.6;
      x,y=nx,ny;
      bitmap[w/2 + x*60, y*60] = 0x00|FF|00;  // Green dot
   }
   bitmap.writeJPGFile("barnsleyFern.jpg");
}();
```



## ZX Spectrum Basic

{{trans|zkl}}

```zxbasic
10 REM Fractal Fern
20 PAPER 7: BORDER 7: BRIGHT 1: INK 4: CLS 
30 LET maxpoints=20000: LET x=0: LET y=0
40 FOR n=1 TO maxpoints
50 LET p=RND*100
60 IF p<=1 THEN LET nx=0: LET ny=0.16*y: GO TO 100
70 IF p<=8 THEN LET nx=0.2*x-0.26*y: LET ny=0.23*x+0.22*y+1.6: GO TO 100
80 IF p<=15 THEN LET nx=-0.15*x+0.28*y: LET ny=0.26*x+0.24*y+0.44: GO TO 100
90 LET nx=0.85*x+0.04*y: LET ny=-0.04*x+0.85*y+1.6
100 LET x=nx: LET y=ny
110 PLOT x*17+127,y*17
120 NEXT n

```

It is recommended to run on an emulator that supports running at full speed.
