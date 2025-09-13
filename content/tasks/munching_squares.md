+++
title = "Munching squares"
description = ""
date = 2019-10-14T01:24:13Z
aliases = []
[extra]
id = 10938
[taxonomies]
categories = ["task", "Raster graphics operations"]
tags = []
+++

## Task

Render a graphical pattern where each pixel is colored by the value of 'x xor y' from an arbitrary [[wp:color table|color table]].


## Ada

Uses the Cairo component of GtkAda to create and save as png

```Ada
with Cairo; use Cairo;
with Cairo.Png; use Cairo.Png;
with Cairo.Image_Surface; use Cairo.Image_Surface;
procedure XorPattern is
   type xorable is mod 256;
   Surface : Cairo_Surface;
   Data : RGB24_Array_Access;
   Status : Cairo_Status;
   Num : Byte;
begin
   Data := new RGB24_Array(0..256*256-1);
   for x in Natural range 0..255 loop
      for y in Natural range 0..255 loop
         Num := Byte(xorable(x) xor xorable(y));
         Data(x+256*y) := RGB24_Data'(Num,0,Num);
      end loop;
   end loop;
   Surface := Create_For_Data_RGB24(Data, 256, 256);
   Status := Write_To_Png (Surface, "AdaXorPattern.png");
   pragma Assert (Status = Cairo_Status_Success);
end XorPattern;
```

{{out}} [[Image:AdaXorPattern.png|Ada Output|200px]]


## AWK

This program generates a PPM image, that you can view/convert using The GIMP or ImageMagick

```awk

BEGIN {
    # square size
    s = 256
    # the PPM image header needs 3 lines:
    # P3
    # width height
    # max colors number (per channel)
    print("P3\n", s, s, "\n", s - 1)
    # and now we generate pixels as a RGB pair in a relaxed
    # form "R G B\n"
    for (x = 0; x < s; x++) {
        for (y = 0; y < s; y++) {
            p = xor(x, y)
            print(0, p, p)
        }
    }
}

```



## BBC BASIC

```bbcbasic
      size% = 256

      VDU 23,22,size%;size%;8,8,16,0
      OFF

      DIM coltab%(size%-1)
      FOR I% = 0 TO size%-1
        coltab%(I%) = ((I% AND &FF) * &010101) EOR &FF0000
      NEXT

      GCOL 1
      FOR I% = 0 TO size%-1
        FOR J% = 0 TO size%-1
          C% = coltab%(I% EOR J%)
          COLOUR 1, C%, C%>>8, C%>>16
          PLOT I%*2, J%*2
        NEXT
      NEXT I%

      REPEAT WAIT 1 : UNTIL FALSE

```



## Befunge


Writes the image to stdout using the PPM format.


```befunge
55+::"3P",,,28*:*::..\,:.\,:v
>2%*28*:**-2/\1-:v<:8:-1<_@ v
^\-1*2%2/*:*82::\_$0.0..:^:*<
```



## Burlesque



```burlesque

blsq ) 0 25r@{0 25r@\/{$$Sh2' P[}\/+]m[}m[sp
 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
 1  0  3  2  5  4  7  6  9  8 11 10 13 12 15 14 17 16 19 18 21 20 23 22 25 24
 2  3  0  1  6  7  4  5 10 11  8  9 14 15 12 13 18 19 16 17 22 23 20 21 26 27
 3  2  1  0  7  6  5  4 11 10  9  8 15 14 13 12 19 18 17 16 23 22 21 20 27 26
 4  5  6  7  0  1  2  3 12 13 14 15  8  9 10 11 20 21 22 23 16 17 18 19 28 29
 5  4  7  6  1  0  3  2 13 12 15 14  9  8 11 10 21 20 23 22 17 16 19 18 29 28
 6  7  4  5  2  3  0  1 14 15 12 13 10 11  8  9 22 23 20 21 18 19 16 17 30 31
 7  6  5  4  3  2  1  0 15 14 13 12 11 10  9  8 23 22 21 20 19 18 17 16 31 30
 8  9 10 11 12 13 14 15  0  1  2  3  4  5  6  7 24 25 26 27 28 29 30 31 16 17
 9  8 11 10 13 12 15 14  1  0  3  2  5  4  7  6 25 24 27 26 29 28 31 30 17 16
10 11  8  9 14 15 12 13  2  3  0  1  6  7  4  5 26 27 24 25 30 31 28 29 18 19
11 10  9  8 15 14 13 12  3  2  1  0  7  6  5  4 27 26 25 24 31 30 29 28 19 18
12 13 14 15  8  9 10 11  4  5  6  7  0  1  2  3 28 29 30 31 24 25 26 27 20 21
13 12 15 14  9  8 11 10  5  4  7  6  1  0  3  2 29 28 31 30 25 24 27 26 21 20
14 15 12 13 10 11  8  9  6  7  4  5  2  3  0  1 30 31 28 29 26 27 24 25 22 23
15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0 31 30 29 28 27 26 25 24 23 22
16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31  0  1  2  3  4  5  6  7  8  9
17 16 19 18 21 20 23 22 25 24 27 26 29 28 31 30  1  0  3  2  5  4  7  6  9  8
18 19 16 17 22 23 20 21 26 27 24 25 30 31 28 29  2  3  0  1  6  7  4  5 10 11
19 18 17 16 23 22 21 20 27 26 25 24 31 30 29 28  3  2  1  0  7  6  5  4 11 10
20 21 22 23 16 17 18 19 28 29 30 31 24 25 26 27  4  5  6  7  0  1  2  3 12 13
21 20 23 22 17 16 19 18 29 28 31 30 25 24 27 26  5  4  7  6  1  0  3  2 13 12
22 23 20 21 18 19 16 17 30 31 28 29 26 27 24 25  6  7  4  5  2  3  0  1 14 15
23 22 21 20 19 18 17 16 31 30 29 28 27 26 25 24  7  6  5  4  3  2  1  0 15 14
24 25 26 27 28 29 30 31 16 17 18 19 20 21 22 23  8  9 10 11 12 13 14 15  0  1
25 24 27 26 29 28 31 30 17 16 19 18 21 20 23 22  9  8 11 10 13 12 15 14  1  0

```


Must be converted to an image with a seperate program.


## C


```cpp
#include <iostream>
#include <stdio.h>
#include <math.h>
#include <string.h>

void hue_to_rgb(double hue, double sat, unsigned char *p)
{
	double x;
	int c = 255 * sat;
	hue /= 60;
	x = (1 - fabs(fmod(hue, 2) - 1)) * 255;

	switch((int)hue) {
	case 0:	p[0] = c; p[1] = x; p[2] = 0; return;
	case 1:	p[0] = x; p[1] = c; p[2] = 0; return;
	case 2:	p[0] = 0; p[1] = c; p[2] = x; return;
	case 3:	p[0] = 0; p[1] = x; p[2] = c; return;
	case 4:	p[0] = x; p[1] = 0; p[2] = c; return;
	case 5:	p[0] = c; p[1] = 0; p[2] = x; return;
	}
}

int main(void)
{
	const int size = 512;
	int i, j;
	unsigned char *colors = malloc(size * 3);
	unsigned char *pix = malloc(size * size * 3), *p;
	FILE *fp;

	for (i = 0; i < size; i++)
		hue_to_rgb(i * 240. / size, i * 1. / size, colors + 3 * i);

	for (i = 0, p = pix; i < size; i++)
		for (j = 0; j < size; j++, p += 3)
			memcpy(p, colors + (i ^ j) * 3, 3);

	fp = fopen("xor.ppm", "wb");
	fprintf(fp, "P6\n%d %d\n255\n", size, size);
	fwrite(pix, size * size * 3, 1, fp);
	fclose(fp);

	return 0;
}
```

{{out}} [[Image:Xor_pattern_c.png|C output|200px]]


## C++

[[File:msquares_cpp2.png|300px]]

```cpp

#include <windows.h>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int BMP_SIZE = 512;

//--------------------------------------------------------------------------------------------------
class myBitmap
{
public:
    myBitmap() : pen( NULL ), brush( NULL ), clr( 0 ), wid( 1 ) {}
    ~myBitmap()
    {
	DeleteObject( pen );
	DeleteObject( brush );
	DeleteDC( hdc );
	DeleteObject( bmp );
    }

    bool create( int w, int h )
    {
	BITMAPINFO    bi;
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

    void clear( BYTE clr = 0 )
    {
	memset( pBits, clr, width * height * sizeof( DWORD ) );
    }

    void setBrushColor( DWORD bClr )
    {
	if( brush ) DeleteObject( brush );
	brush = CreateSolidBrush( bClr );
	SelectObject( hdc, brush );
    }

    void setPenColor( DWORD c ) { clr = c; createPen(); }

    void setPenWidth( int w )   { wid = w; createPen(); }

    void saveBitmap( string path )
    {
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
    void createPen()
    {
	if( pen ) DeleteObject( pen );
	pen = CreatePen( PS_SOLID, wid, clr );
	SelectObject( hdc, pen );
    }

    HBITMAP bmp;
    HDC     hdc;
    HPEN    pen;
    HBRUSH  brush;
    void    *pBits;
    int     width, height, wid;
    DWORD   clr;
};
//--------------------------------------------------------------------------------------------------
class mSquares
{
public:
    mSquares()
    {
        bmp.create( BMP_SIZE, BMP_SIZE );
        createPallete();
    }

    void draw()
    {
	HDC dc = bmp.getDC();
	for( int y = 0; y < BMP_SIZE; y++ )
	    for( int x = 0; x < BMP_SIZE; x++ )
	    {
		int c = ( x ^ y ) % 256;
		SetPixel( dc, x, y, clrs[c] );
	    }

	BitBlt( GetDC( GetConsoleWindow() ), 30, 30, BMP_SIZE, BMP_SIZE, dc, 0, 0, SRCCOPY );
	//bmp.saveBitmap( "f:\\rc\\msquares_cpp.bmp" );
    }

private:
    void createPallete()
    {
	for( int x = 0; x < 256; x++ )
	clrs[x] = RGB( x<<1, x, x<<2 );//rand() % 180 + 50, rand() % 200 + 50, rand() % 180 + 50 );
    }

    unsigned int clrs[256];
    myBitmap bmp;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );
    srand( GetTickCount() );
    mSquares s; s.draw();
    return system( "pause" );
}
//--------------------------------------------------------------------------------------------------

```



## C#


```c#
using System.Drawing;
using System.Drawing.Imaging;
using System.Linq;

class XORPattern
{
    static void Main()
    {
        var size = 0x100;
        var black = Color.Black.ToArgb();
        var palette = Enumerable.Range(black, size).Select(Color.FromArgb).ToArray();
        using (var image = new Bitmap(size, size))
        {
            for (var x = 0; x < size; x++)
            {
                for (var y = 0; y < size; y++)
                {
                    image.SetPixel(x, y, palette[x ^ y]);
                }
            }
            image.Save("XORPatternCSharp.png", ImageFormat.Png);
        }
    }
}
```

[[File:XORPatternCSharp.png|XORPatternCSharp.png]]


## D


```d
void main() {
    import std.stdio;

    enum width = 512, height = 512;

    auto f = File("xor_pattern.ppm", "wb");
    f.writefln("P6\n%d %d\n255", width, height);
    foreach (immutable y; 0 .. height)
        foreach (immutable x; 0 .. width) {
            immutable c = (x ^ y) & ubyte.max;
            immutable ubyte[3] u3 = [255 - c, c / 2, c];
            f.rawWrite(u3);
        }
}
```



## EchoLisp

Use the '''plot''' library, hsv->rgb ((x xor y) modulo m) as color table, and see the nice results here : http://www.echolalie.org/echolisp/help.html#bit-map .

```scheme

(lib 'types)
(lib 'plot)
(plot-size 512 512) ;; for example

;; use m = 16, 32, 44, .. to change the definition (number of losanges)
(define (plot-munch (m 256))
	(define PIX (pixels->int32-vector)) ;; get canvas image
	(define (pcolor x y) ;; color at (x,y)
		(hsv->rgb
			(// (bitwise-xor (modulo x m) (modulo y m)) m)
			0.9
			0.9))
	(pixels-map pcolor PIX)
	(vector->pixels PIX)) ;; draw canvas image

(plot-much) ;; ESC to see tge drawing

```



## Factor


```factor
USING: accessors images images.loader kernel math sequences ;
IN: rosetta-code.munching-squares

: img-data ( -- seq ) 256 sq [ B{ 0 0 0 255 } ] replicate ;

: (munching) ( elt index -- elt' )
    256 /mod bitxor [ rest ] dip prefix ;

: munching ( -- seq )
    img-data [ (munching) ] map-index B{ } concat-as ;

: <munching-img> ( -- img )
    <image>
    { 256 256 }      >>dim
    BGRA             >>component-order
    ubyte-components >>component-type
    munching         >>bitmap ;

: main ( -- ) <munching-img> "munching.png" save-graphic-image ;

MAIN: main
```

Output image is identical to the Racket version.
[[File:munching-racket.png]]

=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Munching_squares this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## FreeBASIC


```freebasic
' version 03-11-2016
' compile with: fbc -s gui

Dim As ULong x, y, r, w = 256

ScreenRes w, w, 32

For x = 0 To w -1
    For y = 0 To w -1
        r =(x Xor y) And 255
        PSet(x, y), RGB(r, r , r)         ' gray scale
        ' PSet(x, y), RGB(r, 255 - r, 0)  ' red + green
        ' PSet(x, y), RGB(r, 0, 0)        ' red
    Next
Next

' empty keyboard buffer
While Inkey <> "" : Wend
WindowTitle "Close window or hit any key to end program"
Sleep
End
```



## GLSL

This is an example that will work directly on shadertoy.com, Example [https://www.shadertoy.com/view/Mss3Rs]

```GLSL
vec3 color;
float c,p;
vec2 b;

void main(void)
{
	vec2 uv = gl_FragCoord.xy / iResolution.xy;
	float scale = iResolution.x / iResolution.y;
	uv = uv-0.5;
	uv.y/=scale;

	b    = uv*256.0+256.0;
	c = 0.0;


	for(float i=16.0;i>=1.0;i-=1.0)
	{
		p = pow(2.0,i);

		if((p < b.x) ^^
		   (p < b.y))
		{
			c += p;
		}

		if(p < b.x)
		{
			b.x -= p;
		}

		if(p < b.y)
		{
			b.y -= p;
		}

	}

	c=mod(c/128.0,1.0);

	color = vec3(sin(c+uv.x*cos(uv.y*1.2)), tan(c+uv.y-0.3)*1.1, cos(c-uv.y+0.9));

	gl_FragColor = vec4(color,1.0);
}
```



## Gnuplot



```gnuplot
set pm3d map
set size square
set isosamples 255,255
splot [0:255][0:255]-(floor(x)^floor(y))
```

{{out}} [[Image:gnuplot_xor.png|Gnuplot output|200px]]


## Haskell


```haskell
import qualified Data.ByteString as BY (writeFile, pack)

import Data.Bits (xor)

main :: IO ()
main =
  BY.writeFile
    "out.pgm"
    (BY.pack
       (fmap (fromIntegral . fromEnum) "P5\n256 256\n256\n" ++
        [ x `xor` y
        | x <- [0 .. 255]
        , y <- [0 .. 255] ]))
```



## Go


```go
package main

import (
    "image"
    "image/png"
    "os"
)

func main() {
    g := image.NewGray(image.Rect(0, 0, 256, 256))
    for i := range g.Pix {
        g.Pix[i] = uint8(i>>8 ^ i)
    }
    f, _ := os.Create("xor.png")
    png.Encode(f, g)
    f.Close()
}
```


=={{header|Icon}} and {{header|Unicon}}==
[[File:XORimage-unicon-GR512.png|thumb|right|512x512 bit green and red]]

```Icon
link printf

procedure main(A)   #: XOR graphic
   wsize := 512
   cmax  := 32768
   wparms := ["Xmas Xor Graphic","g",sprintf("size=%d,%d",wsize),"bg=black"]
   &window := open!wparms | stop("Unable to open window")

   every y := 0 to wsize - 1 do
      every x := 0 to wsize - 1 do {
         c := cmax/wsize * iand(wsize-1,ixor(x,y))
         Fg(sprintf("%d,%d,%d",c,cmax-c,0))
         DrawPoint(x,y)
         }

  until Event() == &lpress     # wait for left button to quit
  close(&window)
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]


## J


```J
   require 'viewmat'
   viewmat ~:"1/&.#: ~ i.256
```



## Java

This example will repeat the pattern if you expand the window.

```java
import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class XorPattern extends JFrame{
    private JPanel xorPanel;

    public XorPattern(){
        xorPanel = new JPanel(){
            @Override
            public void paint(Graphics g) {
                for(int y = 0; y < getHeight();y++){
                    for(int x = 0; x < getWidth();x++){
                        g.setColor(new Color(0, (x ^ y) % 256, 0));
                        g.drawLine(x, y, x, y);
                    }
                }
            }
        };
        add(xorPanel);
        setSize(300, 300);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setVisible(true);
    }

    public static void main(String[] args){
        new XorPattern();
    }
}
```

[[Image:Xor pattern Java.png|200px]]


## jq

The following is an adaptation of the Ruby entry, but generates an SVG image file:

```sh
jq -n -r -f Munching_squares.jq > Munching_squares.svg
```

'''Part 1: Infrastructure'''

```jq
# Convert the input integer to an array of bits with lsb first
def integer_to_lsb:
  [recurse(if . > 0 then ./2|floor else empty end) | . % 2] ;

# input array of bits (with lsb first) is converted to an integer
def lsb_to_integer:
  reduce .[] as $bit
    # state: [power, ans]
    ([1,0]; (.[0] * 2) as $b | [$b, .[1] + (.[0] * $bit)])
  | .[1];

def xor(x;y):
   def lxor(a;b):  # a and/or b may be null
     if a == 1 then if b==1 then 0 else 1 end
     elif b==1 then if a==1 then 0 else 1 end
     else 0
     end;
   (x|integer_to_lsb) as $s
   | (y|integer_to_lsb) as $t
   | ([$s|length, $t|length] | max) as $length
   | reduce range(0;$length) as $i
      ([]; . + [ lxor($s[$i]; $t[$i]) ] )
   | lsb_to_integer;
```

'''Part 2: SVG'''

```jq
def rgb2rgb:
  def p: (. + 0.5) | floor;  # to nearest integer
  "rgb(\(.red|p),\(.green|p),\(.blue|p))";

def svg(width; height):
  "<svg width='\(width // "100%")' height='\(height // "100%")'
           xmlns='http://www.w3.org/2000/svg'>";

def pixel(x; y; color):
  (color | if type == "string" then . else rgb2rgb end) as $c
  | "<circle r='1' cx='\(x)' cy='\(y)' fill='\($c)' />";
```

'''Part 3: xor pattern'''

```jq
# rgb is a JSON object: { "red": _, "green": _, "blue": _}

def xor_pattern(width; height; rgb1; rgb2):
    # create colour table
    256 as $size
    | (reduce range(0;$size) as $i
        ([]; . + [
        {"red":   (rgb1.red + (rgb2.red - rgb1.red) * $i / $size),
         "green": (rgb1.green + (rgb2.green - rgb1.green) * $i / $size),
         "blue":  (rgb1.blue + (rgb2.blue - rgb1.blue) * $i / $size) }])
      )  as $colours
    # create the image
    | svg(width; height),
      ( (range(0;width) as $x
        | range(0;height) as $y
        |   pixel($x; $y; $colours[ xor($x; $y) % $size] ) ) ),
     "</svg>" ;
```

'''Part 4: Example'''

```jq
def black: { "red": 0, "green": 0, "blue": 0};
def red: black + { "red": 255 };
def yellow: red + { "green": 255 };

xor_pattern(384; 384; red; yellow)
```




## Julia


```julia
using Gtk, Colors, PerceptualColourMaps

function munchingsquares(ctx, w, h)
    extent = min(max(w, h), 256)
    colors = cmap("R1", N=extent)
    for i in 1:2:w-2, j in 1:2:h-2
        rectangle(ctx, i, j, i + 2, j + 2)
        c = colors[((UInt(i) ^ UInt(j)) % extent) + 1]
        set_source_rgb(ctx, red(c), blue(c), green(c))
        fill(ctx)
    end
end

const can = @GtkCanvas()
const win = GtkWindow(can, "Munching Squares", 720, 360)

@guarded draw(can) do widget
    ctx = getgc(can)
    h = height(can)
    w = width(can)
    munchingsquares(ctx, w, h)
end

show(can)
const cond = Condition()
endit(w) = notify(cond)
signal_connect(endit, win, :destroy)
wait(cond)

```



## Kotlin


```scala
// version 1.1.4-3

import javax.swing.JFrame
import javax.swing.JPanel
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Color
import java.awt.Dimension
import java.awt.BorderLayout
import java.awt.RenderingHints
import javax.swing.SwingUtilities

class XorPattern : JPanel() {

    init {
        preferredSize = Dimension(256, 256)
        background = Color.white
    }

    override fun paint(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON)
        for (y in 0 until width) {
            for (x in 0 until height) {
                g.color = Color(0, (x xor y) % 256, 255)
                g.drawLine(x, y, x, y)
            }
        }
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        with (f) {
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            title = "Munching squares"
            isResizable = false
            add(XorPattern(), BorderLayout.CENTER)
            pack()
            setLocationRelativeTo(null)
            isVisible = true
        }
    }
}
```



## Liberty BASIC


```lb

    nomainwin

    w =512
    '   allow for title bar and window border
    WindowWidth  =w +2
    WindowHeight =w +34

    open "XOR Pattern" for graphics_nsb_nf as #w

    #w "trapclose quit"

    #w "down"

    for x =0 to w -1
        for y =0 to w -1
            b =( x xor y) and 255
            print b
            #w "color "; 255 -b; " "; b /2; " "; b
            #w "set "; x; " "; w -y -1
            scan
        next y
    next x

    #w "flush"

    wait

    sub quit j$
    close #w
    end
    end sub

```

Image available at [[http://www.diga.me.uk/xorRC.gif]]


## Lua

Needs L&Ouml;VE 2D Engine

```lua

local clr =  {}
function drawMSquares()
    local pts = {}
    for j = 0, hei - 1 do
        for i = 0, wid - 1 do
            idx = bit.bxor( i, j ) % 256
            pts[1] = { i, j, clr[idx][1], clr[idx][2], clr[idx][3], 255 }
            love.graphics.points( pts )
        end
    end
end
function createPalette()
    for i = 0, 255 do
        clr[i] = { bit.band( i * 2.8, 255 ), bit.band( i * 3.2, 255 ), bit.band( i * 1.5, 255 ) }
    end
end
function love.load()
    wid, hei = love.graphics.getWidth(), love.graphics.getHeight()
    canvas = love.graphics.newCanvas( wid, hei )
    love.graphics.setCanvas( canvas ); love.graphics.clear()
    love.graphics.setColor( 255, 255, 255 )
    createPalette(); drawMSquares();
    love.graphics.setCanvas()
end
function love.draw()
    love.graphics.draw( canvas )
end

```


## Mathematica


```Mathematica
ListDensityPlot[
 Table[Table[
   FromDigits[BitXor[IntegerDigits[x, 2, 8], IntegerDigits[y, 2, 8]],
    2], {x, 0, 255}], {y, 0, 255}]]
```

[[File:xorpattern3.png|Mathematica output #1|200px]]


```Mathematica
ArrayPlot[Array[BitXor, {511, 511}]]
```

[[File:xorpattern4.png|Mathematica output #2|200px]]


## MATLAB


```matlab
size = 256;
[x,y] = meshgrid([0:size-1]);

c = bitxor(x,y);

colormap bone(size);
image(c);
axis equal;
```

{{out}} [[File:matlab_xor.png|MATLAB output|200px]]


## Microsoft Small Basic


```smallbasic
' Munching squares - smallbasic  - 27/07/2018
  size=256
  GraphicsWindow.Width=size
  GraphicsWindow.Height=size
  For i=0 To size-1
    For j=0 To size-1
      BitXor() 'color=i Xor j
      GraphicsWindow.SetPixel(i,j,GraphicsWindow.GetColorFromRGB(0,color,color))
    EndFor
  EndFor

Sub BitXor '(i,j)->color
  n=i
  Int2Bit()
  ib=ret
  n=j
  Int2Bit()
  jb=ret
  color=0
  For k=1 to 8
    ki=Text.GetSubText(ib,k,1)
    kj=Text.GetSubText(jb,k,1)
    If ki="1" Or kj="1" Then
      kk="1"
    Else
      kk="0"
    EndIf
    If ki="1" And kj="1" Then
      kk="0"
    EndIf
    color=2*color+kk
  EndFor
EndSub

Sub Int2Bit 'n->ret
  x=n
  ret=""
  For k=1 to 8
    t=Math.Floor(x/2)
    r=Math.Remainder(x,2)
    ret=Text.Append(r,ret)
    x=t
  EndFor
EndSub
```

[https://github.com/Pat-Garrett/RC/blob/master/Munching%20squares%20-%20vbnet.jpg Munching squares - SmallBasic]


## MiniScript

This version runs in Mini Micro (for the graphics).  Note that because MiniScript does not currently have any bit operations (all numbers are floating-point), we have to implement an <code>xor</code> function the hard way.


```MiniScript
xor = function(a, b)
	result = 0
	bit = 1
	while a > 0 or b > 0
		if (a%2 == 0) != (b%2 == 0) then result = result + bit
		bit = bit * 2
		a = floor(a/2)
		b = floor(b/2)
	end while
	return result
end function

for x in range(0,255)
	for y in range(0,255)
		gfx.setPixel x, y, color.rgb(0, xor(x,y), 0)
	end for
end for
```


[[File:xor_pattern_miniscript.png|MiniScript output|254px]]


## OCaml



```ocaml
open Graphics

let () =
  open_graph "";
  resize_window 256 256;
  for y = 0 to pred (size_y()) do
    for x = 0 to pred (size_x()) do
      let v = (x lxor y) land 0xFF in
      set_color (rgb v (255 - v) 0);
      plot x y
    done;
  done;
  ignore(read_key())
```


Run with:
 $ ocaml graphics.cma xor_pattern.ml

[[File:xor_pattern_ocaml.png|OCaml output|200px]]


## Octave


```Octave
size = 256;
[x,y] = meshgrid([0:size-1]);

c = bitxor(x,y);

colormap(jet(size));
image(c);
axis equal;
```

{{out}} [[File:Xor_pattern_octave.png|Octave output|320px]]


## Perl


```perl
use GD;

my $img = new GD::Image(256, 256, 1);

for my $y(0..255) {
        for my $x(0..255) {
                my $color = $img->colorAllocate( abs(255 - $x - $y),  (255-$x) ^ $y , $x ^ (255-$y));
                $img->setPixel($x, $y, $color);
        }
}

print $img->png
```

{{out}} [[File:perl_xor_pattern.png|Perl output|200px]]


## Perl 6

Here's one simple way:


```perl6
my $ppm = open("munching0.ppm", :w) orelse .die;

$ppm.print(q :to 'EOT');
P3
256 256
255
EOT

for 0 .. 255 -> $row {
    for 0 .. 255 -> $col {
        my $color = $row +^ $col;
        $ppm.print("0 $color 0 ");
    }
    $ppm.say();
}

$ppm.close();

```


Another way:


```perl6
my @colors = map -> $r, $g, $b { Buf.new: $r, $g, $b },
		map -> $x { floor ($x/256) ** 3 * 256 },
		    (flat (0...255) Z
		     (255...0) Z
		     flat (0,2...254),(254,252...0));


my $PPM = open "munching1.ppm", :w orelse .die;

$PPM.print: qq:to/EOH/;
    P6
    # munching.pgm
    256 256
    255
    EOH

$PPM.write: @colors[$_] for ^256 X+^ ^256;

$PPM.close;
```

[[File:perl_6_xor_pattern.png|Perl 6 output|200px]]


## Phix

```Phix
-- demo\rosetta\Munching_squares.exw
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
integer {width, height} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    for y=0 to height-1 do
        for x=0 to width-1 do
            cdCanvasPixel(cddbuffer, x, y, xor_bits(x,y))
        end for
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
    IupSetAttribute(canvas, "RASTERSIZE", "250x250")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Munching squares")
    IupSetAttribute(dlg, "RESIZE", "NO")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    IupMap(dlg)
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PHP


```php
header("Content-Type: image/png");

$w = 256;
$h = 256;

$im = imagecreate($w, $h)
    or die("Cannot Initialize new GD image stream");

$color = array();
for($i=0;$i<256;$i++)
{
        array_push($color,imagecolorallocate($im,sin(($i)*(2*3.14/256))*128+128,$i/2,$i));
}

for($i=0;$i<$w;$i++)
{
        for($j=0;$j<$h;$j++)
        {
                imagesetpixel($im,$i,$j,$color[$i^$j]);
        }
}

imagepng($im);
imagedestroy($im);
```

[[File:xor_pattern_php.png|PHP output|200px]]


## PL/I


```PL/I
munch: procedure options (main); /* 21 May 2014 */

   declare screen (0:255, 0:255) bit(24) aligned;
   declare b bit(8) aligned;
   declare (x, y) unsigned fixed binary (8);

   do x = 0 upthru hbound(screen,2);
      do y = 0 upthru hbound(screen,1);
         b = unspec(x) ^ unspec(y);
         screen(x,y) = b;
      end;
   end;
   call writeppm(screen);
end munch;
```



## Prolog

Works with SWI-Prolog and his GUI XPCE.

```Prolog
xor_pattern :-
	new(D, window('XOR Pattern')),
	send(D, size, size(512,512)),
	new(Img, image(@nil, width := 512, height := 512 , kind := pixmap)),

	forall(between(0,511, I),
	       (   forall(between(0,511, J),
			  (   V is I xor J,
			      R is (V * 1024) mod 65536,
			      G is (65536 - V * 1024) mod 65536,
			      (	  V mod 2 =:= 0
			      ->  B is  (V * 4096) mod 65536
			      ;	   B is  (65536 - (V * 4096)) mod 65536),
			      send(Img, pixel(I, J, colour(@default, R, G, B))))))),

	new(Bmp, bitmap(Img)),
	send(D, display, Bmp, point(0,0)),
	send(D, open).

```

[[File:Prolog_xor_pattern.png|200px]]


## PureBasic


```purebasic
#palletteSize = 128
Procedure.f XorPattern(x, y) ;compute the gradient value from the pixel values
  Protected result = x ! y
  ProcedureReturn Mod(result, #palletteSize) / #palletteSize
EndProcedure

Procedure drawPattern()
  StartDrawing(ImageOutput(0))
    DrawingMode(#PB_2DDrawing_Gradient)
    CustomGradient(@XorPattern())
    ;specify a gradient pallette from which only specific indexes will be used
    For i = 1 To #palletteSize
      GradientColor(1 / i, i * $BACE9B) ; or alternatively use $BEEFDEAD
    Next
    Box(0, 0, ImageWidth(0), ImageHeight(0))
  StopDrawing()
EndProcedure

If OpenWindow(0, 0, 0, 128, 128, "XOR Pattern", #PB_Window_SystemMenu)
  CreateImage(0, WindowWidth(0), WindowHeight(0))
  drawPattern()
  ImageGadget(0, 0, 0, ImageWidth(0), ImageHeight(0), ImageID(0))
  Repeat
    event = WaitWindowEvent(20)
  Until event = #PB_Event_CloseWindow
EndIf
```

[[File:PureBasic_XOR_Pattern.png|Sample display of PureBasic solution|200px]]


## Python

```Python
import Image, ImageDraw

image = Image.new("RGB", (256, 256))
drawingTool = ImageDraw.Draw(image)

for x in range(256):
    for y in range(256):
        drawingTool.point((x, y), (0, x^y, 0))

del drawingTool
image.save("xorpic.png", "PNG")
```

[[File:PythonXORPic.png|Sample produced by the above code|200px]]


## Racket

[[File:munching-racket.png|thumb|right]]

```racket

#lang racket
(require racket/draw)
(define palette (for/vector ([x 256]) (make-object color% 0 0 x)))
(define bm (make-object bitmap% 256 256))
(define dc (new bitmap-dc% [bitmap bm]))
(for* ([x 256] [y 256])
  (define c (vector-ref palette (bitwise-xor x y)))
  (send dc set-pixel x y c))
bm

```



## REXX

```rexx
/*REXX program renders a  graphical pattern  by  coloring  each pixel   with   x XOR y  */
/*─────────────────────────────────────────  from an arbitrary constructed color table. */
rows=25                                          /*the number of rows in the color table*/
cols=50                                          /* "     "    " cols  "  "    "     "  */

       do row  =0 for rows*3                     /*construct a color table, size  25x50.*/
         do col=0 for cols*3
                             $= (row+col) // 255
         @.row.col= x2b( d2x($+0, 2) ) ||,       /*ensure $ is converted──►2 hex nibbles*/
                    x2b( d2x($+1, 2) ) ||,
                    x2b( d2x($+2, 2) )
         end   /*col*/                           /* [↑]  construct a three-byte pixel.  */
       end     /*row*/

       do   x=0  for cols                        /*create a graphical pattern with XORs.*/
         do y=0  for rows
         @.x.y=bitxor(@.x, @.y)                  /*renders 3 bytes (a pixel) at a time. */
         end   /*y*/
       end     /*x*/
                                                 /*stick a fork in it,  we're all done. */
```

Must be converted to an image with a separate program.




## Ring


```ring

# Project : Munching squares

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

        w = 100
        for x = 0 to w
             for y = 0 to w
                   b = (x ^ y)
                   color = new qcolor()
                   color.setrgb(255 -b,b /2,b,255)
                   pen.setcolor(color)
                   setpen(pen)
                   drawpoint(x,w -y -1)
             next
         next

        endpaint()
        }
        label1 { setpicture(p1) show() }
        return

```

Output:

https://www.dropbox.com/s/wvdqyihtxralviz/Squares.jpg?dl=0


## Ruby

Uses [[Raster graphics operations/Ruby]]
[[File:xorpattern_rb.png|thumb|right|Sample output from Ruby program]]

```ruby
load 'raster_graphics.rb'

class Pixmap
  def self.xor_pattern(width, height, rgb1, rgb2)
    # create colour table
    size = 256
    colours = Array.new(size) do |i|
      RGBColour.new(
        (rgb1.red + (rgb2.red - rgb1.red) * i / size),
        (rgb1.green + (rgb2.green - rgb1.green) * i / size),
        (rgb1.blue + (rgb2.blue - rgb1.blue) * i / size),
      )
    end

    # create the image
    pixmap = new(width, height)
    pixmap.each_pixel do |x, y|
      pixmap[x,y] = colours[(x^y)%size]
    end
    pixmap
  end
end

img = Pixmap.xor_pattern(384, 384, RGBColour::RED, RGBColour::YELLOW)
img.save_as_png('xorpattern.png')
```



## Run BASIC


```runbasic
w = 100
graphic #g, w,w
for x = 0 to w
  for y = 0 to w
    b = (x xor y) and 255
    #g color(255 -b,b /2,b)
    #g "set "; x; " "; w -y -1
  next y
next x
render #g
#g "flush"
```



## Rust


```rust
extern crate image;

use image::{ImageBuffer, Pixel, Rgb};

fn main() {
    let mut img = ImageBuffer::new(256, 256);

    for x in 0..256 {
        for y in 0..256 {
            let pixel = Rgb::from_channels(0, x as u8 ^ y as u8, 0, 0);
            img.put_pixel(x, y, pixel);
        }
    }

    let _ = img.save("output.png");
}
```



## Scala


### Scala Swing

```Scala
import scala.swing.Swing.pair2Dimension
import scala.swing.{Color, Graphics2D, MainFrame, Panel, SimpleSwingApplication}

object XorPattern extends SimpleSwingApplication {

  def top = new MainFrame {
    preferredSize = (300, 300)
    title = "Rosetta Code >>> Task: Munching squares | Language: Scala"
    contents = new Panel {

      protected override def paintComponent(g: Graphics2D) = {
        super.paintComponent(g)
        for {
          y <- 0 until size.getHeight.toInt
          x <- 0 until size.getWidth.toInt
        } {
          g.setColor(new Color(0, (x ^ y) % 256, 0))
          g.drawLine(x, y, x, y)
        }
      }
    }

    centerOnScreen()
  }
}
```



## Sidef

```ruby
require('Imager')

var img = %O<Imager>.new(xsize => 256, ysize => 256)

for y=(^256), x=(^256) {
    var rgb = [(255 - x - y).abs, (255-x)^y, x^(255-y)]
    img.setpixel(x => x, y => y, color => rgb)
}

img.write(file => 'xor.png')
```

Output image: [https://github.com/trizen/rc/blob/master/img/munching-squares-sidef.png Munching squares]


## Tcl

```tcl
package require Tk

proc xorImage {img table} {
    set data {}
    set h [image height $img]
    set w [image width $img]
    for {set y 0} {$y < $h} {incr y} {
	set row {}
	for {set x 0} {$x < $w} {incr x} {
	    lappend row [lindex $table [expr {($x^$y) % [llength $table]}]]
	}
	lappend data $row
    }
    $img put $data
}
proc inRange {i f t} {expr {$f + ($t-$f)*$i/255}}
proc mkTable {rf rt gf gt bf bt} {
    for {set i 0} {$i < 256} {incr i} {
	lappend tbl [format "#%02x%02x%02x" \
	    [inRange $i $rf $rt] [inRange $i $gf $gt] [inRange $i $bf $bt]]
    }
    return $tbl
}

set img [image create photo -width 512 -height 512]
xorImage $img [mkTable 0 255 64 192 255 0]
pack [label .l -image $img]
```


=={{header|TI-83 BASIC}}==
Due to the TI-83's 1 bit black and white display, this program uses the home screen and a
gradient of characters. Since the TI-83 does not use a standard encoding,
the first Sto→ to Str1 may be subjectively interpreted.


<lang ti-83b>PROGRAM:XORPATT
" •.-,+-°-1+o*:πOX"→Str1

ClrHome

{0,0,0,0}→L1
{0,0,0,0)→L2

For(I,1,8,1)
For(J,1,16,1)
J→A
I→B

If A>8
Then
A-8→A
1→L1(1)
Else
0→L1(1)
End

If A>4
Then
A-4→A
1→L1(2)
Else
0→L1(2)
End

If A>2
Then
A-2→A
1→L1(3)
Else
0→L1(3)
End

If A>1
Then
1→L1(4)
Else
0→L1(4)
End

0→L2(1)

If B>4
Then
B-4→B
1→L2(2)
Else
0→L2(2)
End

If B>2
Then
B-2→B
1→L2(3)
Else
0→L2(3)
End

If B>1
Then
1→L2(4)
Else
0→L2(4)
End

L1≠L2→L3
8L3(1)+4L3(2)+2L3(3)+L3(4)→C
Output(I,J,sub(Str1,C+1,1))

End
End
Pause

```



## Visual Basic .NET

```vbnet
' Munching squares - 27/07/2018
Public Class MunchingSquares
    Const xsize = 256
    Dim BMP As New Drawing.Bitmap(xsize, xsize)
    Dim GFX As Graphics = Graphics.FromImage(BMP)

    Private Sub MunchingSquares_Paint(sender As Object, e As PaintEventArgs) Handles Me.Paint
        'draw
        Dim MyGraph As Graphics = Me.CreateGraphics
        Dim nColor As Color
        Dim i, j, cp As Integer
        xPictureBox.Image = BMP
        For i = 0 To xsize - 1
            For j = 0 To xsize - 1
                cp = i Xor j
                nColor = Color.FromArgb(cp, 0, cp)
                BMP.SetPixel(i, j, nColor)
            Next j
        Next i
    End Sub 'Paint

End Class
```

[https://github.com/Pat-Garrett/RC/blob/7e9842513d361a5b4241bc6bb28f9985c2bfe161/Munching%20squares%20-%20vbnet.jpg Munching squares - vbnet]


## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
int X, Y;
[SetVid($101);                  \set 640x480 graphics with 8-bit color
port($3C8):= 0;                 \set color registers with beautiful shades
for X:= 0 to 256-1 do
        [port($3C9):= X>>1;     \red
         port($3C9):= X>>3;     \green
         port($3C9):= X;        \blue
         ];
for Y:= 0 to 256-1 do           \"color table" is array of 256 registers
    for X:= 0 to 256-1 do
        Point(X, Y, X|Y);       \"|" = XOR, not OR which is "!"
X:= ChIn(1);                    \wait for keystroke
SetVid(3);                      \restore normal text mode
]
```

{{out}} [[File:MunchXPL0.png]]


## Yabasic

```Yabasic
w = 256

open window w, w

For x = 0 To w-1
    For y = 0 To w-1
        r =and(xor(x, y), 255)
        color r, and(r*2, 255), and(r*3, 255)
        dot x, y
    Next
Next
```



## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
For a kaleidoscopic image, play with coolness.

```zkl
fcn muncher{
   bitmap:=PPM(256,256);
   coolness:=(1).random(0x10000);  // 55379, 18180, 40, 51950, 57619, 43514, 65465
   foreach y,x in ([0 .. 255],[0 .. 255]){
      b:=x.bitXor(y);	// shades of blue
//      rgb:=b*coolness;                 // kaleidoscopic image
//      rgb:=(b*coolness + b)*coolness + b;  // more coolness
      rgb:=(b*0x10000 + b)*0x10000 + b;  // copy ADA image
      bitmap[x,y]=rgb;
   }
   bitmap.write(File("foo.ppm","wb"));
}();
```

For a cheap slide show (on Linux):

```zkl
while(1){ muncher(); Atomic.sleep(3); }
```

run ImageViewer on foo.ppm and watch it [auto] update as the image changes.
Same as the ADA image:
[[Image:AdaXorPattern.png|Ada Output|200px]]
