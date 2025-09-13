+++
title = "Pythagoras tree"
description = ""
date = 2019-08-07T08:19:33Z
aliases = []
[extra]
id = 10734
[taxonomies]
categories = ["task"]
tags = []
+++

[[File:Pythagoras_tree_java.png|600px||right]]

The [[wp:Pythagoras_tree_%28fractal%29|Pythagoras tree]] is a fractal tree constructed from squares.  It is named after Pythagoras because each triple of touching squares encloses a right triangle, in a configuration traditionally used to represent the Pythagorean theorem.



## Task

Construct a Pythagoras tree of order 7 using only vectors (no rotation or trigonometric functions).



## Related tasks

* [[Fractal_tree|Fractal tree]]




## BASIC256

```BASIC256

Subroutine pythagoras_tree(x1, y1, x2, y2, depth)
	If depth > 10 Then Return

	dx = x2 - x1 : dy = y1 - y2
	x3 = x2 - dy : y3 = y2 - dx
	x4 = x1 - dy : y4 = y1 - dx
	x5 = x4 + (dx - dy) / 2
	y5 = y4 - (dx + dy) / 2
	#draw the box
	Line x1, y1, x2, y2 : Line x2, y2, x3, y3
	Line x3, y3, x4, y4 : Line x4, y4, x1, y1

	Call pythagoras_tree(x4, y4, x5, y5, depth +1)
	Call pythagoras_tree(x5, y5, x3, y3, depth +1)
End Subroutine

w = 800 : h = w * 11 \ 16
w2 = w \ 2 : diff = w \ 12

Clg
FastGraphics
Graphsize w, h
Color green
Call pythagoras_tree(w2 - diff, h - 10, w2 + diff, h - 10, 0)
Refresh
ImgSave "pythagoras_tree.jpg", "jpg"
End

```



## C

A Pythagoras tree constructed from an initial square of side length L, fits exactly in a bounding box of length 6L and width 4L([http://ecademy.agnesscott.edu/~lriddle/ifs/pythagorean/pythSize.htm Proof]). That's why the window dimensions are set to 6L x 4L, where L is entered by the user. The squares increase rapidly, an iteration value of 30 takes 'forever' for a single branch to complete. The colours are picked randomly thus producing the effect of a Pythagorean Christmas Tree. :)

Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.

```C

#include<graphics.h>
#include<stdlib.h>
#include<stdio.h>
#include<time.h>

typedef struct{
	double x,y;
}point;

void pythagorasTree(point a,point b,int times){

	point c,d,e;

	c.x = b.x - (a.y -  b.y);
	c.y = b.y - (b.x - a.x);

	d.x = a.x - (a.y -  b.y);
	d.y = a.y - (b.x - a.x);

	e.x = d.x +  ( b.x - a.x - (a.y -  b.y) ) / 2;
	e.y = d.y -  ( b.x - a.x + a.y -  b.y ) / 2;

	if(times>0){
		setcolor(rand()%15 + 1);

		line(a.x,a.y,b.x,b.y);
		line(c.x,c.y,b.x,b.y);
		line(c.x,c.y,d.x,d.y);
		line(a.x,a.y,d.x,d.y);

		pythagorasTree(d,e,times-1);
		pythagorasTree(e,c,times-1);
	}
}

int main(){

	point a,b;
	double side;
	int iter;

	time_t t;

	printf("Enter initial side length : ");
	scanf("%lf",&side);

	printf("Enter number of iterations : ");
	scanf("%d",&iter);

	a.x = 6*side/2 - side/2;
	a.y = 4*side;
	b.x = 6*side/2 + side/2;
	b.y = 4*side;

	initwindow(6*side,4*side,"Pythagoras Tree ?");

	srand((unsigned)time(&t));

	pythagorasTree(a,b,iter);

	getch();

	closegraph();

	return 0;

}
```



## C++

[[File:pythagoras_treeCpp.png|300px|thumb|]]
Windows version
```cpp
#include <windows.h>
#include <string>
#include <iostream>

const int BMP_SIZE = 720, LINE_LEN = 120, BORDER = 100;

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
class tree {
public:
    tree() {
        bmp.create( BMP_SIZE, BMP_SIZE ); bmp.clear();
        clr[0] = RGB( 90, 30, 0 );   clr[1] = RGB( 255, 255, 0 );
        clr[2] = RGB( 0, 255, 255 ); clr[3] = RGB( 255, 255, 255 );
        clr[4] = RGB( 255, 0, 0 );   clr[5] = RGB( 0, 100, 190 );
    }
    void draw( int it, POINT a, POINT b ) {
        if( !it ) return;
        bmp.setPenColor( clr[it % 6] );
        POINT df = { b.x - a.x, a.y -  b.y }; POINT c = { b.x - df.y, b.y - df.x };
        POINT d = { a.x - df.y, a.y - df.x };
        POINT e = { d.x + ( ( df.x - df.y ) / 2 ), d.y - ( ( df.x + df.y ) / 2 )};
        drawSqr( a, b, c, d ); draw( it - 1, d, e ); draw( it - 1, e, c );
    }
    void save( std::string p ) { bmp.saveBitmap( p ); }
private:
    void drawSqr( POINT a, POINT b, POINT c, POINT d ) {
        HDC dc = bmp.getDC();
        MoveToEx( dc, a.x, a.y, NULL );
        LineTo( dc, b.x, b.y );
        LineTo( dc, c.x, c.y );
        LineTo( dc, d.x, d.y );
        LineTo( dc, a.x, a.y );
    }
    myBitmap bmp;
    DWORD clr[6];
};
int main( int argc, char* argv[] ) {
    POINT ptA = { ( BMP_SIZE >> 1 ) - ( LINE_LEN >> 1 ), BMP_SIZE - BORDER },
          ptB = { ptA.x + LINE_LEN, ptA.y };
    tree t; t.draw( 12, ptA, ptB );
    // change this path
    t.save( "?:/pt.bmp" );
    return 0;
}
```



## EasyLang


[https://easylang.online/apps/run.html?code=floatvars%0Afunc%20tree%20x1%20y1%20x2%20y2%20depth%20.%20.%0Aif%20depth%20%3C%208%0Adx%20%3D%20x2%20-%20x1%0Ady%20%3D%20y1%20-%20y2%0Ax3%20%3D%20x2%20-%20dy%0Ay3%20%3D%20y2%20-%20dx%0Ax4%20%3D%20x1%20-%20dy%0Ay4%20%3D%20y1%20-%20dx%0Ax5%20%3D%20x4%20%2B%200.5%20%2A%20%28dx%20-%20dy%29%0Ay5%20%3D%20y4%20-%200.5%20%2A%20%28dx%20%2B%20dy%29%0Acolor_green%200.2%20%2B%20depth%20/%2018%0Afill%20%5B%20x1%20y1%20x2%20y2%20x3%20y3%20x4%20y4%20%5D%0Afill%20%5B%20x3%20y3%20x4%20y4%20x5%20y5%20%5D%0Acall%20tree%20x4%20y4%20x5%20y5%20depth%20%2B%201%0Acall%20tree%20x5%20y5%20x3%20y3%20depth%20%2B%201%0A.%0A.%0Acolor_red%200.3%0Acolor_blue%200.1%0Acall%20tree%2041%2090%2059%2090%200 Run it]

<lang>floatvars
func tree x1 y1 x2 y2 depth . .
  if depth < 8
    dx = x2 - x1
    dy = y1 - y2
    x3 = x2 - dy
    y3 = y2 - dx
    x4 = x1 - dy
    y4 = y1 - dx
    x5 = x4 + 0.5 * (dx - dy)
    y5 = y4 - 0.5 * (dx + dy)
    color_green 0.2 + depth / 18
    fill [ x1 y1 x2 y2 x3 y3 x4 y4 ]
    fill [ x3 y3 x4 y4 x5 y5 ]
    call tree x4 y4 x5 y5 depth + 1
    call tree x5 y5 x3 y3 depth + 1
  .
.
color_red 0.3
color_blue 0.1
call tree 41 90 59 90 0
```


=={{header|F_Sharp|F#}}==
<p>Creating an HTML file with an inline SVG. The generation of the tree is done breadth first.</p>

```fsharp
type Point = { x:float; y:float }
type Line = { left : Point; right : Point }

let draw_start_html = """<!DOCTYPE html>
<html><head><title>Phytagoras tree</title>
<style type="text/css">polygon{fill:none;stroke:black;stroke-width:1}</style>
</head><body>
<svg width="640" height="640">"""

let draw_end_html = """Sorry, your browser does not support inline SVG.
</svg></body></html>"""

let svg_square x1 y1 x2 y2 x3 y3 x4 y4 =
    sprintf """<polygon points="%i %i %i %i %i %i %i %i" />"""
        (int x1) (int y1) (int x2) (int y2) (int x3) (int y3) (int x4) (int y4)

let out (x : string) = System.Console.WriteLine(x)

let sprout line =
    let dx = line.right.x - line.left.x
    let dy = line.left.y - line.right.y
    let line2 = {
        left = { x = line.left.x - dy; y = line.left.y - dx };
        right = { x = line.right.x - dy ; y = line.right.y - dx }
    }
    let triangleTop = {
        x = line2.left.x + (dx - dy) / 2.;
        y = line2.left.y - (dx + dy) / 2.
    }
    [
        { left = line2.left; right = triangleTop }
        { left = triangleTop; right = line2.right }
    ]

let draw_square line =
    let dx = line.right.x - line.left.x
    let dy = line.left.y - line.right.y
    svg_square line.left.x line.left.y line.right.x line.right.y
               (line.right.x - dy) (line.right.y - dx) (line.left.x - dy) (line.left.y - dx)

let rec generate lines = function
| 0 -> ()
| n ->
    let next =
        lines
        |> List.collect (fun line ->
            (draw_square >> out) line
            sprout line
        )
    generate next (n-1)


[<EntryPoint>]
let main argv =
    let depth = 1 + if argv.Length > 0 then (System.UInt32.Parse >> int) argv.[0] else 2
    out draw_start_html
    generate [{ left = { x = 275.; y = 500. }; right = { x = 375.; y = 500. } }] depth
    out draw_end_html
    0
```


## FreeBASIC

```freebasic
' version 03-12-2016
' compile with: fbc -s gui
' or fbc -s console

Sub pythagoras_tree(x1 As Double, y1 As Double, x2 As Double, y2 As Double, depth As ULong)

    If depth > 10 Then Return

    Dim As Double dx = x2 - x1, dy = y1 - y2
    Dim As Double x3 = x2 - dy, y3 = y2 - dx
    Dim As Double x4 = x1 - dy, y4 = y1 - dx
    Dim As Double x5 = x4 + (dx - dy) / 2
    Dim As Double y5 = y4 - (dx + dy) / 2
    'draw the box
    Line (x1, y1) - (x2, y2) : Line - (x3, y3)
    Line - (x4, y4) : Line - (x1, y1)

    pythagoras_tree(x4, y4, x5, y5, depth +1)
    pythagoras_tree(x5, y5, x3, y3, depth +1)

End Sub

' ------=< MAIN >=------
' max for w is about max screensize - 500
Dim As ULong w = 800, h = w * 11 \ 16
Dim As ULong w2 = w \ 2, diff = w \ 12

ScreenRes w, h, 8
pythagoras_tree(w2 - diff, h -10 , w2 + diff , h -10 , 0)
' BSave "pythagoras_tree.bmp",0



' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```



## Go


```Go
package main

import (
	"image"
	"image/color"
	"image/draw"
	"image/png"
	"log"
	"os"
)

const (
	width, height = 800, 600
	maxDepth      = 11                    // how far to recurse, between 1 and 20 is reasonable
	colFactor     = uint8(255 / maxDepth) // adjusts the colour so leaves get greener further out
	fileName      = "pythagorasTree.png"
)

func main() {
	img := image.NewNRGBA(image.Rect(0, 0, width, height)) // create new image
	bg := image.NewUniform(color.RGBA{255, 255, 255, 255}) // prepare white for background
	draw.Draw(img, img.Bounds(), bg, image.ZP, draw.Src)   // fill the background

	drawSquares(340, 550, 460, 550, img, 0) // start off near the bottom of the image

	imgFile, err := os.Create(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer imgFile.Close()
	if err := png.Encode(imgFile, img); err != nil {
		imgFile.Close()
		log.Fatal(err)
	}
}

func drawSquares(ax, ay, bx, by int, img *image.NRGBA, depth int) {
	if depth > maxDepth {
		return
	}
	dx, dy := bx-ax, ay-by
	x3, y3 := bx-dy, by-dx
	x4, y4 := ax-dy, ay-dx
	x5, y5 := x4+(dx-dy)/2, y4-(dx+dy)/2
	col := color.RGBA{0, uint8(depth) * colFactor, 0, 255}
	drawLine(ax, ay, bx, by, img, col)
	drawLine(bx, by, x3, y3, img, col)
	drawLine(x3, y3, x4, y4, img, col)
	drawLine(x4, y4, ax, ay, img, col)
	drawSquares(x4, y4, x5, y5, img, depth+1)
	drawSquares(x5, y5, x3, y3, img, depth+1)
}

func drawLine(x0, y0, x1, y1 int, img *image.NRGBA, col color.RGBA) {
	dx := abs(x1 - x0)
	dy := abs(y1 - y0)
	var sx, sy int = -1, -1
	if x0 < x1 {
		sx = 1
	}
	if y0 < y1 {
		sy = 1
	}
	err := dx - dy
	for {
		img.Set(x0, y0, col)
		if x0 == x1 && y0 == y1 {
			break
		}
		e2 := 2 * err
		if e2 > -dy {
			err -= dy
			x0 += sx
		}
		if e2 < dx {
			err += dx
			y0 += sy
		}
	}
}
func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
```



## Haskell


Haskell allows us to make highly modular solution.

Firstly, we define a function <code>mkBranches</code> that produces a pair of minor squares based on a given square. Each square is represented as a list of points.


```haskell
mkBranches :: [(Float,Float)] -> [[(Float,Float)]]
mkBranches [a, b, c, d] = let d  = 0.5 <*> (b <+> (-1 <*> a))
                              l1 = d <+> orth d
                              l2 = orth l1
                    in
                      [ [a <+> l2, b <+> (2 <*> l2), a <+> l1, a]
                      , [a <+> (2 <*> l1), b <+> l1, b, b <+> l2] ]
  where
    (a, b) <+> (c, d) = (a+c, b+d)
    n <*> (a, b) = (a*n, b*n)
    orth (a, b) = (-b, a)
```


We then create <code>squares</code> using <code>mkBranches</code> to build a list representing the set of squares. In order to apply this function iteratively to form a 10-generation tree, we also have to define the monadic iteration <code>iterateM</code> within <code>squares</code>.


```haskell
squares = concat $ take 10 $ iterateM mkBranches start
  where start = [(0,100),(100,100),(100,0),(0,0)]
        iterateM f x = iterate (>>= f) (pure x)
```


The raw result returned by <code>squares</code> should be used in the <code>main</code> function in order to be displayed in a new window, saved directly to a SVG file, or printed to a bitmap file.

'''Window output'''
```haskell
--import should go to the top of the code
import Graphics.Gloss

main = display (InWindow "Pithagoras tree" (400, 400) (0, 0)) white tree
  where tree = foldMap lineLoop squares
```


'''SVG file'''

```haskell
main = writeFile "pith.svg" svg
  where svg = "<svg " ++ attrs ++ foldMap (mkLine . close) squares ++ "</svg>"
        attrs = "fill='none' stroke='black' height='400' width='600'>"
        mkLine path = "<polyline points ='" ++ foldMap mkPoint path ++ "'/>"
        mkPoint (x,y) = show (250+x) ++ "," ++ show (400-y) ++ " "
        close lst = lst ++ [head lst]
```


'''Bitmap image'''
```haskell
--import should go to the top of the code
import Graphics.EasyPlot

--change PNG by the desired format
main = plot (PNG "pith.png") $ map (mkLine . close) squares
  where mkLine = Data2D [Style Lines, Color Black,Title ""] []
        close lst = lst ++ [head lst]
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Pythagor.bas"
110 OPTION ANGLE DEGREES
120 LET SQ2=SQR(2)
130 SET VIDEO MODE 1:SET VIDEO COLOUR 0:SET VIDEO X 42:SET VIDEO Y 25
140 OPEN #101:"video:"
150 SET PALETTE 0,141
160 DISPLAY #101:AT 1 FROM 1 TO 25
170 PLOT 580,20;ANGLE 90;
180 CALL BROCCOLI(225,10)
190 DO
200 LOOP WHILE INKEY$=""
210 TEXT
220 DEF BROCCOLI(X,Y)
230   IF X<Y THEN EXIT DEF
240   CALL SQUARE(X)
250   PLOT FORWARD X,LEFT 45,
260   CALL BROCCOLI(X/SQ2,Y)
270   PLOT RIGHT 90,FORWARD X/SQ2,
280   CALL BROCCOLI(X/SQ2,Y)
290   PLOT BACK X/SQ2,LEFT 45,BACK X,
300 END DEF
310 DEF SQUARE(X)
320   FOR I=1 TO 4
330     PLOT FORWARD X;RIGHT 90;
340   NEXT
350 END DEF
```



## J

Using the bash shell, gnuplot for graphics, with ijconsole installed on the PATH, and having saved the program in the file /tmp/pt.ijs the following command plots the Pythagoras tree:


```txt

gnuplot --persist -e 'plot"<ijconsole /tmp/pt.ijs"w l'

```



```J

NB. use on linux:  gnuplot --persist -e 'plot"< ijconsole /tmp/pt.ijs"w l'

NB. translated from c

ex=: {.
why=: {:
but_first=: &             NB. just for fun
append=: ,
subtract=: -

X=: adverb def ' ex m'
Y=: adverb def 'why m'

pt=: dyad define

 'a b'=. y

 NB. c.x = b.x - (a.y -  b.y);
 NB. c.y = b.y - (b.x - a.x);
 c=. (b X , a append but_first why b) ,&(-/) (b Y , b ,&ex a)

 NB. d.x = a.x - (a.y -  b.y);
 NB. d.y = a.y - (b.x - a.x);
 d=. (a X , a append but_first why b) ,&(-/) (a Y , b ,&ex a)

 NB. e.x = d.x +  ( b.x - a.x - (a.y -  b.y) ) / 2;
 NB. e.y = d.y -  ( b.x - a.x + a.y -  b.y ) / 2;
 e=. (d X + -: (b -&ex a) - a subtract but_first why b) , d Y - -: -/ b X , a X , a Y , b Y

 if. 0 < x do.
  NB. line(a.x,a.y,b.x,b.y); line(c.x,c.y,b.x,b.y); line(c.x,c.y,d.x,d.y); line(a.x,a.y,d.x,d.y);
  echo (a ,: b) , (c ,: b) , (c ,: d) ,: (a ,: d)
  echo ''
  (<: x) pt"2 (d ,: e) ,: (e ,: c)  NB. pythagorasTree(d,e,times-1);pythagorasTree(e,c,times-1);
 end.
)


	NB.   a.x = 6*side/2 - side/2;
	NB.   a.y = 4*side;
	NB.   b.x = 6*side/2 + side/2;
	NB.   b.y = 4*side;
petri=: 7&$: :(empty@:(pt (x:inv 5r2 7r2 ,. 4)&*))

petri 1
exit 0

```



## Java

[[File:pythagoras_tree.png|300px|thumb|right]]
```java
import java.awt.*;
import java.awt.geom.Path2D;
import javax.swing.*;

public class PythagorasTree extends JPanel {
    final int depthLimit = 7;
    float hue = 0.15f;

    public PythagorasTree() {
        setPreferredSize(new Dimension(640, 640));
        setBackground(Color.white);
    }

    private void drawTree(Graphics2D g, float x1, float y1, float x2, float y2,
            int depth) {

        if (depth == depthLimit)
            return;

        float dx = x2 - x1;
        float dy = y1 - y2;

        float x3 = x2 - dy;
        float y3 = y2 - dx;
        float x4 = x1 - dy;
        float y4 = y1 - dx;
        float x5 = x4 + 0.5F * (dx - dy);
        float y5 = y4 - 0.5F * (dx + dy);

        Path2D square = new Path2D.Float();
        square.moveTo(x1, y1);
        square.lineTo(x2, y2);
        square.lineTo(x3, y3);
        square.lineTo(x4, y4);
        square.closePath();

        g.setColor(Color.getHSBColor(hue + depth * 0.02f, 1, 1));
        g.fill(square);
        g.setColor(Color.lightGray);
        g.draw(square);

        Path2D triangle = new Path2D.Float();
        triangle.moveTo(x3, y3);
        triangle.lineTo(x4, y4);
        triangle.lineTo(x5, y5);
        triangle.closePath();

        g.setColor(Color.getHSBColor(hue + depth * 0.035f, 1, 1));
        g.fill(triangle);
        g.setColor(Color.lightGray);
        g.draw(triangle);

        drawTree(g, x4, y4, x5, y5, depth + 1);
        drawTree(g, x5, y5, x3, y3, depth + 1);
    }

    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        drawTree((Graphics2D) g, 275, 500, 375, 500, 0);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Pythagoras Tree");
            f.setResizable(false);
            f.add(new PythagorasTree(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

```javascript
<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8">
    <style>
        canvas {
            position: absolute;
            top: 45%;
            left: 50%;
            width: 640px;
            height: 640px;
            margin: -320px 0 0 -320px;
        }
    </style>
</head>

<body>
    <canvas></canvas>
    <script>
        'use strict';
        var canvas = document.querySelector('canvas');
        canvas.width = 640;
        canvas.height = 640;

        var g = canvas.getContext('2d');

        var depthLimit = 7;
        var hue = 0.15;

        function drawTree(x1, y1, x2, y2, depth) {

            if (depth == depthLimit)
                return;

            var dx = x2 - x1;
            var dy = y1 - y2;

            var x3 = x2 - dy;
            var y3 = y2 - dx;
            var x4 = x1 - dy;
            var y4 = y1 - dx;
            var x5 = x4 + 0.5 * (dx - dy);
            var y5 = y4 - 0.5 * (dx + dy);

            g.beginPath();
            g.moveTo(x1, y1);
            g.lineTo(x2, y2);
            g.lineTo(x3, y3);
            g.lineTo(x4, y4);
            g.closePath();

            g.fillStyle = HSVtoRGB(hue + depth * 0.02, 1, 1);
            g.fill();
            g.strokeStyle = "lightGray";
            g.stroke();

            g.beginPath();
            g.moveTo(x3, y3);
            g.lineTo(x4, y4);
            g.lineTo(x5, y5);
            g.closePath();

            g.fillStyle = HSVtoRGB(hue + depth * 0.035, 1, 1);
            g.fill();
            g.strokeStyle = "lightGray";
            g.stroke();

            drawTree(x4, y4, x5, y5, depth + 1);
            drawTree(x5, y5, x3, y3, depth + 1);
        }

        /* copied from stackoverflow */
        function HSVtoRGB(h, s, v) {
            var r, g, b, i, f, p, q, t;

            i = Math.floor(h * 6);
            f = h * 6 - i;
            p = v * (1 - s);
            q = v * (1 - f * s);
            t = v * (1 - (1 - f) * s);
            switch (i % 6) {
                case 0: r = v, g = t, b = p; break;
                case 1: r = q, g = v, b = p; break;
                case 2: r = p, g = v, b = t; break;
                case 3: r = p, g = q, b = v; break;
                case 4: r = t, g = p, b = v; break;
                case 5: r = v, g = p, b = q; break;
            }
            return "rgb("
                + Math.round(r * 255) + ","
                + Math.round(g * 255) + ","
                + Math.round(b * 255) + ")";
        }

        function draw() {
            g.clearRect(0, 0, canvas.width, canvas.height);
            drawTree(275, 500, 375, 500, 0);
        }
        draw();
    </script>

</body>

</html>
```



## Julia

```Julia
using Gadfly
using DataFrames

const xarray = zeros(Float64, 80000)
const yarray = zeros(Float64, 80000)
const arraypos = ones(Int32,1)
const maxdepth = zeros(Int32, 1)


function addpoints(x1, y1, x2, y2)
    xarray[arraypos[1]] = x1
    xarray[arraypos[1]+1] = x2
    yarray[arraypos[1]] = y1
    yarray[arraypos[1]+1] = y2
    arraypos[1] += 2
end


function pythtree(ax, ay, bx, by, depth)
    if(depth > maxdepth[1])
        return
    end
    dx=bx-ax; dy=ay-by;
    x3=bx-dy; y3=by-dx;
    x4=ax-dy; y4=ay-dx;
    x5=x4+(dx-dy)/2; y5=y4-(dx+dy)/2;
    addpoints(ax, ay, bx, by)
    addpoints(bx, by, x3, y3)
    addpoints(x3, y3, x4, y4)
    addpoints(x4, y4, ax, ay)
    pythtree(x4, y4, x5, y5, depth + 1)
    pythtree(x5, y5, x3, y3, depth + 1)
end


function pythagorastree(x1, y1, x2, y2, size, maxdep)
    maxdepth[1] = maxdep
    println("Pythagoras Tree, depth $(maxdepth[1]), size $size, starts at ($x1, $y1, $x2, $y2)");
    pythtree(x1, y1, x2, y2, 0);
    df = DataFrame(x=xarray[1:arraypos[1]-1], y=-yarray[1:arraypos[1]-1])
    plot(df, x=:x, y=:y, Geom.path(), Theme(default_color="green", point_size=0.4mm))
end

pythagorastree(275.,500.,375.,500.,640., 9)
```



## Kotlin

```scala
// version 1.1.2

import java.awt.*
import java.awt.geom.Path2D
import javax.swing.*

class PythagorasTree : JPanel() {
    val depthLimit = 7
    val hue = 0.15f

    init {
        preferredSize = Dimension(640, 640)
        background = Color.white
    }

    private fun drawTree(g: Graphics2D, x1: Float, y1: Float,
                                        x2: Float, y2: Float, depth: Int) {
        if (depth == depthLimit) return

        val dx = x2 - x1
        val dy = y1 - y2

        val x3 = x2 - dy
        val y3 = y2 - dx
        val x4 = x1 - dy
        val y4 = y1 - dx
        val x5 = x4 + 0.5f * (dx - dy)
        val y5 = y4 - 0.5f * (dx + dy)

        val square = Path2D.Float()
        with (square) {
            moveTo(x1, y1)
            lineTo(x2, y2)
            lineTo(x3, y3)
            lineTo(x4, y4)
            closePath()
        }

        g.color = Color.getHSBColor(hue + depth * 0.02f, 1.0f, 1.0f)
        g.fill(square)
        g.color = Color.lightGray
        g.draw(square)

        val triangle = Path2D.Float()
        with (triangle) {
            moveTo(x3, y3)
            lineTo(x4, y4)
            lineTo(x5, y5)
            closePath()
        }

        g.color = Color.getHSBColor(hue + depth * 0.035f, 1.0f, 1.0f)
        g.fill(triangle)
        g.color = Color.lightGray
        g.draw(triangle)

        drawTree(g, x4, y4, x5, y5, depth + 1)
        drawTree(g, x5, y5, x3, y3, depth + 1)
    }

    override fun paintComponent(g: Graphics) {
        super.paintComponent(g)
        drawTree(g as Graphics2D, 275.0f, 500.0f, 375.0f, 500.0f, 0)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        with (f) {
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            title = "Pythagoras Tree"
            isResizable = false
            add(PythagorasTree(), BorderLayout.CENTER)
            pack()
            setLocationRelativeTo(null);
            setVisible(true)
        }
    }
}
```


## M2000 Interpreter


### Cartesian Coordinates

```M2000 Interpreter

MODULE Pythagoras_tree {
	CLS 5, 0  ' MAGENTA, NO SPLIT SCREEN
	PEN 14   ' YELLOW
	\\ code from zkl/Free Basic
	LET w = scale.x, h = w * 11 div 16
	LET w2 = w div 2, diff = w div 12
	LET TreeOrder = 6
	pythagoras_tree(w2 - diff, h -10, w2 + diff, h -10, 0)

	SUB pythagoras_tree(x1, y1, x2, y2, depth)

	    IF depth > TreeOrder THEN EXIT SUB

	    LOCAL dx = x2 - x1, dy = y1 - y2
	    LOCAL x3 = x2 - dy, y3 = y2 - dx
	    LOCAL x4 = x1 - dy, y4 = y1 - dx
	    LOCAL x5 = x4 + (dx - dy) / 2
	    LOCAL y5 = y4 - (dx + dy) / 2
	    MOVE x1, y1
	    DRAW TO x2, y2
	    DRAW TO x3, y3
	    DRAW TO x4, y4
	    DRAW TO x1, y1
	    pythagoras_tree(x4, y4, x5, y5, depth +1)
	    pythagoras_tree(x5, y5, x3, y3, depth +1)

	END SUB
}
Pythagoras_tree

```


### Polar Coordinates


```M2000 Interpreter

MODULE Pythagoras_Example{
	CLS 5, 0  ' MAGENTA, split line = 0
	PEN 14  ' YELLOW
	\\ Linux smoothing not work (we can use the statement but without effect)
	IF ISWINE ELSE SMOOTH ON
	\\ PYTHAGORAS TREE
	\\ by definition all variables ar type of a double
	GLOBAL p=7, p4=PI/4, p2=PI/2, s2=SQRT(2)/2
	MODULE center_p (r, t){
		MODULE pythagoras_tree (r, dx, depth) {
			r2=r-p2
			DRAW ANGLE r, dx
			DRAW ANGLE r2, dx
			DRAW ANGLE r, -dx
			DRAW ANGLE r2, -dx
			IF depth>10 THEN EXIT
			s3=dx*s2
			depth++
			STEP ANGLE r+p4, s3*2
			CALL pythagoras_tree r-p4,  s3, depth
			STEP ANGLE r, -dx-s3
			STEP ANGLE r, s3
			STEP ANGLE r+p4, -s3
			CALL pythagoras_tree r+p4,  s3, depth
			STEP ANGLE r-p4, s3
		}
		MOVE SCALE.X/2, SCALE.Y/2
		STEP ANGLE PI-p4+r, t*s2
		CALL pythagoras_tree r, t, 1
	}
	r=PI/3
	pixels=100
	center_p r, 100*TWIPSX
	center_p r+PI, 100*TWIPSX
	CopyImageToClipboard()

	Sub CopyImageToClipboard()
		LOCAL Scr$=""
		MOVE 0,0
		COPY SCALE.X, SCALE.Y TO Scr$
		CLIPBOARD Scr$
	END SUB
}
Pythagoras_Example

```




## PARI/GP

[[File:PythTree1.png|right|thumb|Output PythTree1.png]]

This version with recursion, in general, is a translation of zkl version.
Almost "as is", so, outputting upside-down tree.

```parigp
\\ Pythagoras Tree (w/recursion)
\\ 4/11/16 aev
plotline(x1,y1,x2,y2)={plotmove(0, x1,y1);plotrline(0,x2-x1,y2-y1);}

pythtree(ax,ay,bx,by,d=0)={
my(dx,dy,x3,y3,x4,y4,x5,y5);
if(d>10, return());
dx=bx-ax; dy=ay-by;
x3=bx-dy; y3=by-dx;
x4=ax-dy; y4=ay-dx;
x5=x4+(dx-dy)\2; y5=y4-(dx+dy)\2;
plotline(ax,ay,bx,by);
plotline(bx,by,x3,y3);
plotline(x3,y3,x4,y4);
plotline(x4,y4,ax,ay);
pythtree(x4,y4,x5,y5,d+1);
pythtree(x5,y5,x3,y3,d+1);
}

PythagorTree(x1,y1,x2,y2,depth=9,size)={
my(dx=1,dy=0,ttlb="Pythagoras Tree, depth ",ttl=Str(ttlb,depth));
print1(" *** ",ttl); print(", size ",size);
print(" *** Start: ",x1,",",y1,",",x2,",",y2);
plotinit(0);
plotcolor(0,6); \\green
plotscale(0, -size,size, size,0 );
plotmove(0, 0,0);
pythtree(x1,y1, x2,y2);
plotdraw([0,size,size]);
}

{\\ Executing:
PythagorTree(275,500,375,500,9,640);    \\PythTree1.png
}
```

```txt
 *** Pythagoras Tree, depth 9, size 640
 *** Start: 275,500,375,500
```



## Perl

```perl
use Imager;

sub tree {
    my ($img, $x1, $y1, $x2, $y2, $depth) = @_;

    return () if $depth <= 0;

    my $dx = ($x2 - $x1);
    my $dy = ($y1 - $y2);

    my $x3 = ($x2 - $dy);
    my $y3 = ($y2 - $dx);
    my $x4 = ($x1 - $dy);
    my $y4 = ($y1 - $dx);
    my $x5 = ($x4 + 0.5 * ($dx - $dy));
    my $y5 = ($y4 - 0.5 * ($dx + $dy));

    # Square
    $img->polygon(
        points => [
            [$x1, $y1],
            [$x2, $y2],
            [$x3, $y3],
            [$x4, $y4],
        ],
        color => [0, 255 / $depth, 0],
    );

    # Triangle
    $img->polygon(
        points => [
            [$x3, $y3],
            [$x4, $y4],
            [$x5, $y5],
        ],
        color => [0, 255 / $depth, 0],
    );

    tree($img, $x4, $y4, $x5, $y5, $depth - 1);
    tree($img, $x5, $y5, $x3, $y3, $depth - 1);
}

my ($width, $height) = (1920, 1080);
my $img = Imager->new(xsize => $width, ysize => $height);
$img->box(filled => 1, color => 'white');
tree($img, $width/2.3, $height, $width/1.8, $height, 10);
$img->write(file => 'pythagoras_tree.png');
```



## Perl 6

We'll generate a SVG image.

```perl6
class Square {
    has Complex ($.position, $.edge);
    method size { $!edge.abs }
    method svg-polygon {
	qq[<polygon points="{join ' ', map
	{ ($!position + $_ * $!edge).reals.join(',') },
	0, 1, 1+1i, 1i}" style="fill:lime;stroke=black" />]
    }
    method left-child {
	self.new:
	position => $!position + i*$!edge,
	edge => sqrt(2)/2*cis(pi/4)*$!edge;
    }
    method right-child {
	self.new:
	position => $!position + i*$!edge + self.left-child.edge,
	edge => sqrt(2)/2*cis(-pi/4)*$!edge;
    }
}

BEGIN say '<svg width="500" height="500">';
END   say '</svg>';

sub tree(Square $s, $level = 0) {
    return if $level > 8;
    say $s.svg-polygon;
    tree($s.left-child, $level+1);
    tree($s.right-child, $level+1);
}

tree Square.new: :position(250+0i), :edge(60+0i);
```



## Phix

```Phix
---- demo\rosetta\PythagorasTree.exw
--
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

function rgb(integer r, integer g, integer b)
    return r*#10000 + g*#100 + b
end function

procedure drawTree(atom x1, atom y1, atom x2, atom y2, integer depth)
atom dx = x2 - x1
atom dy = y1 - y2

atom x3 = x2 - dy
atom y3 = y2 - dx
atom x4 = x1 - dy
atom y4 = y1 - dx
atom x5 = x4 + 0.5 * (dx - dy)
atom y5 = y4 - 0.5 * (dx + dy)

integer r = 250-depth*20

    cdCanvasSetForeground(cddbuffer, rgb(r,#FF,0))
    cdCanvasBegin(cddbuffer,CD_FILL)
    cdCanvasVertex(cddbuffer, x1, 640-y1)
    cdCanvasVertex(cddbuffer, x2, 640-y2)
    cdCanvasVertex(cddbuffer, x3, 640-y3)
    cdCanvasVertex(cddbuffer, x4, 640-y4)
    cdCanvasEnd(cddbuffer)

    cdCanvasSetForeground(cddbuffer, CD_GRAY)
    cdCanvasBegin(cddbuffer,CD_CLOSED_LINES)
    cdCanvasVertex(cddbuffer, x1, 640-y1)
    cdCanvasVertex(cddbuffer, x2, 640-y2)
    cdCanvasVertex(cddbuffer, x3, 640-y3)
    cdCanvasVertex(cddbuffer, x4, 640-y4)
    cdCanvasEnd(cddbuffer)

    cdCanvasSetForeground(cddbuffer, rgb(r-depth*10,#FF,0))
    cdCanvasBegin(cddbuffer,CD_FILL)
    cdCanvasVertex(cddbuffer, x3, 640-y3)
    cdCanvasVertex(cddbuffer, x4, 640-y4)
    cdCanvasVertex(cddbuffer, x5, 640-y5)
    cdCanvasEnd(cddbuffer)

    cdCanvasSetForeground(cddbuffer, CD_GRAY)
    cdCanvasBegin(cddbuffer,CD_CLOSED_LINES)
    cdCanvasVertex(cddbuffer, x3, 640-y3)
    cdCanvasVertex(cddbuffer, x4, 640-y4)
    cdCanvasVertex(cddbuffer, x5, 640-y5)
    cdCanvasEnd(cddbuffer)

    if depth<8 then
        drawTree(x4, y4, x5, y5, depth + 1)
        drawTree(x5, y5, x3, y3, depth + 1)
    end if
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    cdCanvasActivate(cddbuffer)
    drawTree(275, 500, 375, 500, 0)
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
    IupSetAttribute(canvas, "RASTERSIZE", "640x640")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    dlg = IupDialog(canvas,"RESIZE=NO")
    IupSetAttribute(dlg, "TITLE", "Pythagoras Tree")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))

    IupShow(dlg)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## Processing

```java
void tree(float x1, float y1, float x2, float y2, int depth) {

  if (depth <= 0) {
    return;
  }

  float dx = (x2 - x1);
  float dy = (y1 - y2);

  float x3 = (x2 - dy);
  float y3 = (y2 - dx);
  float x4 = (x1 - dy);
  float y4 = (y1 - dx);
  float x5 = (x4 + 0.5*(dx - dy));
  float y5 = (y4 - 0.5*(dx + dy));

  // square
  beginShape();
  fill(0.0, 255.0/depth, 0.0);
  vertex(x1, y1);
  vertex(x2, y2);
  vertex(x3, y3);
  vertex(x4, y4);
  vertex(x1, y1);
  endShape();

  // triangle
  beginShape();
  fill(0.0, 255.0/depth, 0.0);
  vertex(x3, y3);
  vertex(x4, y4);
  vertex(x5, y5);
  vertex(x3, y3);
  endShape();

  tree(x4, y4, x5, y5, depth-1);
  tree(x5, y5, x3, y3, depth-1);
}

void setup() {
  size(1920, 1080);
  background(255);
  stroke(0, 255, 0);
  tree(width/2.3, height, width/1.8, height, 10);
}
```



## PureBasic

```PureBasic
EnableExplicit
DisableDebugger

Procedure.d maxXY(a.d,b.d,c.d,d.d)
  If a<b : Swap a,b : EndIf
  If a<c : Swap a,c : EndIf
  If a<d : Swap a,d : EndIf
  ProcedureReturn a
EndProcedure

Procedure.d minXY(a.d,b.d,c.d,d.d)
  If a>b : Swap a,b : EndIf
  If a>c : Swap a,c : EndIf
  If a>d : Swap a,d : EndIf
  ProcedureReturn a
EndProcedure

Procedure Ptree(x1.d, y1.d, x2.d, y2.d, d.i=0)
  If d>10 : ProcedureReturn : EndIf

  Define dx.d=x2-x1,
         dy.d=y1-y2,
         x3.d=x2-dy,
         y3.d=y2-dx,
         x4.d=x1-dy,
         y4.d=y1-dx,
         x5.d=x4+(dx-dy)/2.0,
         y5.d=y4-(dx+dy)/2.0,
         p1.d=(maxXY(x1,x2,x3,x4)+minXY(x1,x2,x3,x4))/2.0,
         p2.d=(maxXY(y1,y2,y3,y4)+minXY(y1,y2,y3,y4))/2.0,
         p3.d=(maxXY(x1,x2,x3,x4)-minXY(x1,x2,x3,x4))

  FrontColor(RGB(Random(125,1),Random(255,125),Random(125,1)))
  LineXY(x1,y1,x2,y2)
  LineXY(x2,y2,x3,y3)
  LineXY(x3,y3,x4,y4)
  LineXY(x4,y4,x1,y1)
  BoxedGradient(minXY(x1,x2,x3,x4),minXY(y1,y2,y3,y4),p3,p3)
  FillArea(p1,p2,-1)

  Ptree(x4,y4,x5,y5,d+1)
  Ptree(x5,y5,x3,y3,d+1)

EndProcedure

Define w1.i=800,
       h1.i=w1*11/16,
       w2.i=w1/2,
       di.i=w1/12

If OpenWindow(0,#PB_Ignore,#PB_Ignore,w1,h1,"Pythagoras tree")
  If CreateImage(0,w1,h1,24,0) And StartDrawing(ImageOutput(0))
    DrawingMode(#PB_2DDrawing_Gradient)
    BackColor($000000)
    Ptree(w2-di,h1-10,w2+di,h1-10)
    StopDrawing()
  EndIf
  ImageGadget(0,0,0,0,0,ImageID(0))
  Repeat : Until WaitWindowEvent(50)=#PB_Event_CloseWindow
EndIf
End
```



## Python

Using [https://docs.python.org/3/library/turtle.html turtle graphics] and the Zkl example for the calculations.

```python
from turtle import goto, pu, pd, color, done

def level(ax, ay, bx, by, depth=0):
    if depth > 0:
        dx,dy = bx-ax, ay-by
        x3,y3 = bx-dy, by-dx
        x4,y4 = ax-dy, ay-dx
        x5,y5 = x4 + (dx - dy)/2, y4 - (dx + dy)/2
        goto(ax, ay), pd()
        for x, y in ((bx, by), (x3, y3), (x4, y4), (ax, ay)):
            goto(x, y)
        pu()
        level(x4,y4, x5,y5, depth - 1)
        level(x5,y5, x3,y3, depth - 1)

if __name__ == '__main__':
    color('red', 'yellow')
    pu()
    level(-100, 500, 100, 500, depth=8)
    done()
```



## R

[[File:PYTHTR9.png|200px|right|thumb|Output PYTHTR9.png]]
[[File:PYTHTR7.png|200px|right|thumb|Output PYTHTR7.png]]

```r
## Recursive PT plotting
pythtree <- function(ax,ay,bx,by,d) {
  if(d<0) {return()}; clr="darkgreen";
  dx=bx-ax; dy=ay-by;
  x3=bx-dy; y3=by-dx;
  x4=ax-dy; y4=ay-dx;
  x5=x4+(dx-dy)/2; y5=y4-(dx+dy)/2;
  segments(ax,-ay,bx,-by, col=clr);
  segments(bx,-by,x3,-y3, col=clr);
  segments(x3,-y3,x4,-y4, col=clr);
  segments(x4,-y4,ax,-ay, col=clr);
  pythtree(x4,y4,x5,y5,d-1);
  pythtree(x5,y5,x3,y3,d-1);
}
## Plotting Pythagoras Tree. aev 3/27/17
## x1,y1,x2,y2 - starting position
## ord - order/depth, fn - file name, ttl - plot title.
pPythagorasT <- function(x1, y1,x2, y2, ord, fn="", ttl="") {
  cat(" *** START PYTHT:", date(), "\n");
  m=640; i=j=k=m1=m-2; x=y=d=dm=0;
  if(fn=="") {pf=paste0("PYTHTR", ord, ".png")} else {pf=paste0(fn, ".png")};
  if(ttl=="") {ttl=paste0("Pythagoras tree, order - ", ord)};
  cat(" *** Plot file -", pf, "title:", ttl, "\n");
  plot(NA, xlim=c(0,m), ylim=c(-m,0), xlab="", ylab="", main=ttl);
  pythtree(x1,y1, x2,y2, ord);
  dev.copy(png, filename=pf, width=m, height=m);
  dev.off(); graphics.off();
  cat(" *** END PYTHT:",date(),"\n");
}
## Executing:
pPythagorasT(275,500,375,500,9)
pPythagorasT(275,500,375,500,7)
```

```txt
> pPythagorasT(275,500,375,500,9)
 *** START PYTHT: Tue Mar 28 15:57:19 2017
 *** Plot file - PYTHTR9.png title: Pythagoras tree, order - 9
 *** END PYTHT: Tue Mar 28 15:57:20 2017
> pPythagorasT(275,500,375,500,7)
 *** START PYTHT: Tue Mar 28 15:59:25 2017
 *** Plot file - PYTHTR7.png title: Pythagoras tree, order - 7
 *** END PYTHT: Tue Mar 28 15:59:25 2017
```



## Racket


```racket
#lang racket
(require racket/draw pict)

(define (draw-pythagoras-tree order x0 y0 x1 y1)
  (λ (the-dc dx dy)
    (define (inr order x0 y0 x1 y1)
      (when (positive? order)
        (let* ((y0-1 (- y0 y1))
               (x1-0 (- x1 x0))
               (x2 (+ x1 y0-1))
               (y2 (+ y1 x1-0))
               (x3 (+ x0 y0-1))
               (y3 (+ y0 x1-0))
               (x4 (+ x2 x3 (/ (+ x0 x2) -2)))
               (y4 (+ y2 y3 (/ (+ y0 y2) -2)))
               (path (new dc-path%)))
          (send* path [move-to x0 y0]
            [line-to x1 y1] [line-to x2 y2] [line-to x3 y3]
            [close])
          (send the-dc draw-path path dx dy)
          (inr (sub1 order) x3 y3 x4 y4)
          (inr (sub1 order) x4 y4 x2 y2))))

    (define old-brush (send the-dc get-brush))
    (define old-pen (send the-dc get-pen))
    (send the-dc set-pen (new pen% [width 1] [color "black"]))
    (inr (add1 order) x0 y0 x1 y1)
    (send the-dc set-brush old-brush)
    (send the-dc set-pen old-pen)))

(dc (draw-pythagoras-tree 7 (+ 200 32) 255 (- 200 32) 255) 400 256)
```



## Ring


```ring
# Project : Pythagoras tree

load "guilib.ring"

paint = null

new qapp
        {
        win1 = new qwidget() {
                  setwindowtitle("Pythagoras tree")
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
                  setpen(pen)

        w = 800
        h = floor(w*11/16)
        w2 = floor(w/2)
        diff = floor(w/12)

        pythagorastree(w2 - diff,h -10,w2 + diff ,h -10 ,0)

        endpaint()
        }
        label1 { setpicture(p1) show() }
        return


func pythagorastree(x1,y1,x2,y2,depth)
        if depth > 10
           return
        ok
        dx = x2 - x1
        dy = y1 - y2
        x3 = x2 - dy
        y3 = y2 - dx
        x4 = x1 - dy
        y4 = y1 - dx
        x5 = x4 + floor((dx - dy) / 2)
        y5 = y4 - floor((dx + dy) / 2)
        paint.drawline(x1,y1,x2,y2)
        paint.drawline(x2,y2,x3,y3)
        paint.drawline(x4,y4,x1,y1)
        pythagorastree(x4, y4, x5, y5, depth +1)
        pythagorastree(x5, y5, x3, y3, depth +1)
```

Output:
https://www.dropbox.com/s/a1gtue7tvmaj2je/PythagorasTree.jpg?dl=0


## Scala


### Java Swing Interoperability


```Scala
import java.awt._
import java.awt.geom.Path2D

import javax.swing.{JFrame, JPanel, SwingUtilities, WindowConstants}

object PythagorasTree extends App {

  SwingUtilities.invokeLater(() => {
    new JFrame {

      class PythagorasTree extends JPanel {
        setPreferredSize(new Dimension(640, 640))
        setBackground(Color.white)

        override def paintComponent(g: Graphics): Unit = {
          val (depthLimit, hue) = (7, 0.15f)

          def drawTree(g: Graphics2D, x1: Float, y1: Float, x2: Float, y2: Float, depth: Int): Unit = {
            if (depth == depthLimit) return
            val (dx, dy) = (x2 - x1, y1 - y2)
            val (x3, y3) = (x2 - dy, y2 - dx)
            val (x4, y4) = (x1 - dy, y1 - dx)
            val (x5, y5) = (x4 + 0.5F * (dx - dy), y4 - 0.5F * (dx + dy))
            val square = new Path2D.Float {
              moveTo(x1, y1); lineTo(x2, y2); lineTo(x3, y3); lineTo(x4, y4); closePath()
            }
            val triangle = new Path2D.Float {
              moveTo(x3, y3); lineTo(x4, y4); lineTo(x5, y5); closePath()
            }
            g.setColor(Color.getHSBColor(hue + depth * 0.02f, 1, 1))
            g.fill(square)
            g.setColor(Color.lightGray)
            g.draw(square)
            g.setColor(Color.getHSBColor(hue + depth * 0.035f, 1, 1))
            g.fill(triangle)
            g.setColor(Color.lightGray)
            g.draw(triangle)
            drawTree(g, x4, y4, x5, y5, depth + 1)
            drawTree(g, x5, y5, x3, y3, depth + 1)
          }

          super.paintComponent(g)
          drawTree(g.asInstanceOf[Graphics2D], 275, 500, 375, 500, 0)
        }
      }

      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setTitle("Pythagoras Tree")
      setResizable(false)
      add(new PythagorasTree, BorderLayout.CENTER)
      pack()
      setLocationRelativeTo(null)
      setVisible(true)
    }
  })

}
```


## Scilab

===L-System approach===
This solution uses complex numbers to represent vectors, and it draws the contour of the tree. By "uncommenting" the six commented lines inside the <code>select</code> structure, it will also draw the triangles between the squares. The output is a new graphic window.
<lang>side = 1;       //side length of the square
depth = 8;      //final number of branch levels

//L-system definition:
//Alphabet: UTDB+-[]
    //U: go upwards             T: top of the square
    //D: go downwards           B: bottom of the square
    //[: start new branch       ]: end current branch
    //+: branch to the right    -: branch to the left
//Axiom:    UTDB
//Rule:     T -> [+UTD-UTD]

//L-system sentence generation
sentence = 'UTDB'
rule = '[+UTD-UTD]';
for i=1:depth
    sentence = strsubst(sentence,'T',rule);
end
sentence = strsplit(sentence)';

//Empty tree
tree_size = 1.0...
            + length(find(sentence == "U" | sentence == "T" |...
                          sentence == "D" | sentence == "B"))...
            + 2 * length(find(sentence == "]" | sentence == "-" |...
                              sentence == "+"));
tree=zeros(tree_size,1);

//Vectorial operation to calculate a new point in the tree
deff('z = new_point(origin,rho,theta)',...
     'z = origin + rho * exp(%i*theta)');

//Drawing the tree
curr_angle = %pi/2;
curr_pos = 1;
ratio = 1/sqrt(2);
for ind = 1:size(sentence,'c')
    charac = sentence(ind);

    select charac
    case 'U' then //Draw line upwards
        tree(curr_pos+1) = new_point(tree(curr_pos),side,curr_angle);
        curr_pos = curr_pos + 1;

    case 'T' then //Draw top of the square
        curr_angle = curr_angle - %pi/2;
        tree(curr_pos+1) = new_point(tree(curr_pos),side,curr_angle);
        curr_pos = curr_pos + 1;

    case 'D' then //Draw line downwards
        curr_angle = curr_angle - %pi/2;
        tree(curr_pos+1) = new_point(tree(curr_pos),side,curr_angle);
        curr_pos = curr_pos + 1;

    case 'B' then //Draw the bottom
        curr_angle = curr_angle - %pi/2;
        tree(curr_pos+1) = new_point(tree(curr_pos),side,curr_angle);
        curr_pos = curr_pos + 1;

    case '[' then //Start branch
        side = side * ratio;

    case '+' then //Start going to the left
        curr_angle = curr_angle - %pi/4;
//        tree(curr_pos+1) = new_point(tree(curr_pos),side,curr_angle);
//        tree(curr_pos+2) = new_point(tree(curr_pos+1),side,%pi+curr_angle);
//        curr_pos = curr_pos + 2;
        curr_angle = curr_angle + %pi/2;

    case '-' then //Start going to the left
//        tree(curr_pos+1) = new_point(tree(curr_pos),side,curr_angle);
//        tree(curr_pos+2) = new_point(tree(curr_pos+1),side,%pi+curr_angle);
//        curr_pos = curr_pos + 2;
        curr_angle = curr_angle + %pi/2;
    case ']' then
        side = side / ratio;
        curr_angle = curr_angle - %pi/4;
//        tree(curr_pos+1) = new_point(tree(curr_pos),side,curr_angle);
//        tree(curr_pos+2) = new_point(tree(curr_pos+1),side,%pi+curr_angle);
//        curr_pos = curr_pos + 2;
        curr_angle = curr_angle + %pi;

    else
        error('L-system sentence error');
    end
end

scf(); clf();
xname('Pythagoras tree: '+string(depth)+' levels')
plot2d(real(tree),imag(tree),14);
set(gca(),'isoview','on');
set(gca(),'axes_visible',['off','off','off']);
```


### Recursive approach

A minor change was made so that the final depth of the tree is an argument of <code>fcn</code>, and not a condition set within itself.
<lang>function []=fcn(bitmap,ax,ay,bx,by,depth)
    if depth < 0 then
        return
    end

    dx = bx - ax; dy = ay - by;
    x3 = bx + dy; y3 = by + dx;
    x4 = ax + dy; y4 = ay + dx;
    x5 = x4 + (dx + dy)/2; y5 = y4 + (dx - dy)/2;

    scf(bitmap);
    plot2d([x3 x4 x5],[y3 y4 y5],-2)
    plot2d([ax bx],[ay by]); plot2d([bx x3],[by y3]);
    plot2d([x3 x4],[y3 y4]); plot2d([x4 ax],[y4 ay]);

    fcn(bitmap,x4,y4,x5,y5,depth-1);
    fcn(bitmap,x5,y5,x3,y3,depth-1);
endfunction

plot_win = scf();
final_depth = 8;
clf();

fcn(plot_win,275,500,375,500,final_depth)

scf(plot_win);
xname('Pythagoras tree: '+string(final_depth)+' levels');
set(gca(),'isoview','on');
set(gca(),'axes_visible',['off','off','off']);
```



## Sidef

```ruby
require('Imager')

func tree(img, x1, y1, x2, y2, depth) {

    depth <= 0 && return()

    var dx = (x2 - x1)
    var dy = (y1 - y2)

    var x3 = (x2 - dy)
    var y3 = (y2 - dx)
    var x4 = (x1 - dy)
    var y4 = (y1 - dx)
    var x5 = (x4 + 0.5*(dx - dy))
    var y5 = (y4 - 0.5*(dx + dy))

    # square
    img.polygon(
        points => [
            [x1, y1],
            [x2, y2],
            [x3, y3],
            [x4, y4],
        ],
        color => [0, 255/depth, 0],
    )

    # triangle
    img.polygon(
        points => [
            [x3, y3],
            [x4, y4],
            [x5, y5],
        ],
        color => [0, 255/depth, 0],
    )

    tree(img, x4, y4, x5, y5, depth - 1)
    tree(img, x5, y5, x3, y3, depth - 1)
}

var (width=1920, height=1080)
var img = %O<Imager>.new(xsize => width, ysize => height)
img.box(filled => 1, color => 'white')
tree(img, width/2.3, height, width/1.8, height, 10)
img.write(file => 'pythagoras_tree.png')
```

Output image: [https://github.com/trizen/rc/blob/master/img/pythagoras-tree-sidef.png Pythagoras tree]


## Yabasic

```Yabasic
Sub pythagoras_tree(x1, y1, x2, y2, depth)
    local dx, dy, x3, y3, x4, y4, x5, y5

    If depth > limit Return

    dx = x2 - x1 : dy = y1 - y2
    x3 = x2 - dy : y3 = y2 - dx
    x4 = x1 - dy : y4 = y1 - dx
    x5 = x4 + (dx - dy) / 2
    y5 = y4 - (dx + dy) / 2
    //draw the box
    color 255 - depth * 20, 255, 0
    fill triangle x1, y1, x2, y2, x3, y3
    fill triangle x3, y3, x4, y4, x1, y1
    fill triangle x4, y4, x5, y5, x3, y3

    pythagoras_tree(x4, y4, x5, y5, depth +1)
    pythagoras_tree(x5, y5, x3, y3, depth +1)

End Sub

// ------=< MAIN >=------
w = 800 : h = int(w * 11 / 16)
w2 = int(w / 2) : diff = int(w / 12)
limit = 12

open window w, h
//backcolor 0, 0, 0
clear window

pythagoras_tree(w2 - diff, h -10 , w2 + diff , h -10 , 1)
```



## zkl

I added green crosses at three of the vertexes of the new square to simulate leaves on the tree.
Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
[[File:PythagorasTreeWithLeafs.zkl.jpg|300px|thumb|right]]

```zkl
fcn pythagorasTree{
   bitmap:=PPM(640,640,0xFF|FF|FF);  // White background

   fcn(bitmap, ax,ay, bx,by, depth=0){
      if(depth>10) return();
      dx,dy:=bx-ax, ay-by;
      x3,y3:=bx-dy, by-dx;
      x4,y4:=ax-dy, ay-dx;
      x5,y5:=x4 + (dx - dy)/2, y4 - (dx + dy)/2;
      bitmap.cross(x3,y3);bitmap.cross(x4,y4);bitmap.cross(x5,y5);
      bitmap.line(ax,ay, bx,by, 0); bitmap.line(bx,by, x3,y3, 0);
      bitmap.line(x3,y3, x4,y4, 0); bitmap.line(x4,y4, ax,ay, 0);

      self.fcn(bitmap,x4,y4, x5,y5, depth+1);
      self.fcn(bitmap,x5,y5, x3,y3, depth+1);
   }(bitmap,275,500, 375,500);

   bitmap.writeJPGFile("pythagorasTree.jpg",True);
}();
```

