+++
title = "Bitmap/Bézier curves/Quadratic"
description = ""
date = 2018-07-13T15:12:07Z
aliases = []
[extra]
id = 3213
[taxonomies]
categories = ["task", "Raster graphics operations"]
tags = []
languages = [
  "ada",
  "bbc_basic",
  "c",
  "d",
  "factor",
  "fbsl",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "julia",
  "kotlin",
  "matlab",
  "ocaml",
  "perl_6",
  "phix",
  "picolisp",
  "purebasic",
  "python",
  "r",
  "racket",
  "ruby",
  "tcl",
  "vedit_macro_language",
  "xpl0",
  "zkl",
]
+++

## Task

Using the data storage type defined [[Basic_bitmap_storage|on this page]] for raster images, and the <tt>draw_line</tt> function defined in [[Bresenham's_line_algorithm|this one]], draw a ''quadratic bezier curve''
([[wp:Bezier_curves#Quadratic_B.C3.A9zier_curves|definition on Wikipedia]]).



## Ada


```ada
procedure Quadratic_Bezier
          (  Picture    : in out Image;
             P1, P2, P3 : Point;
             Color      : Pixel;
             N          : Positive := 20
          )  is
   Points : array (0..N) of Point;
begin
   for I in Points'Range loop
      declare
         T : constant Float := Float (I) / Float (N);
         A : constant Float := (1.0 - T)**2;
         B : constant Float := 2.0 * T * (1.0 - T);
         C : constant Float := T**2;
      begin
         Points (I).X := Positive (A * Float (P1.X) + B * Float (P2.X) + C * Float (P3.X));
         Points (I).Y := Positive (A * Float (P1.Y) + B * Float (P2.Y) + C * Float (P3.Y));
      end;
   end loop;
   for I in Points'First..Points'Last - 1 loop
      Line (Picture, Points (I), Points (I + 1), Color);
   end loop;
end Quadratic_Bezier;
```

The following test

```ada
   X : Image (1..16, 1..16);
begin
   Fill (X, White);
   Quadratic_Bezier (X, (8, 2), (13, 8), (2, 15), Black);
   Print (X);
```

should produce;

```txt


              H
             H
             H
            H
           H
          HH
 HH      H
  HH  HHH
    HH







```


## BBC BASIC

[[Image:bezierquad_bbc.gif|right]]

```bbcbasic
      Width% = 200
      Height% = 200

      REM Set window size:
      VDU 23,22,Width%;Height%;8,16,16,128

      REM Draw quadratic Bézier curve:
      PROCbezierquad(10,100, 250,270, 150,20, 20, 0,0,0)
      END

      DEF PROCbezierquad(x1,y1,x2,y2,x3,y3,n%,r%,g%,b%)
      LOCAL i%, t, t1, a, b, c, p{()}
      DIM p{(n%) x%,y%}

      FOR i% = 0 TO n%
        t = i% / n%
        t1 = 1 - t
        a = t1^2
        b = 2 * t * t1
        c = t^2
        p{(i%)}.x% = INT(a * x1 + b * x2 + c * x3 + 0.5)
        p{(i%)}.y% = INT(a * y1 + b * y2 + c * y3 + 0.5)
      NEXT

      FOR i% = 0 TO n%-1
        PROCbresenham(p{(i%)}.x%,p{(i%)}.y%,p{(i%+1)}.x%,p{(i%+1)}.y%, \
        \             r%,g%,b%)
      NEXT
      ENDPROC

      DEF PROCbresenham(x1%,y1%,x2%,y2%,r%,g%,b%)
      LOCAL dx%, dy%, sx%, sy%, e
      dx% = ABS(x2% - x1%) : sx% = SGN(x2% - x1%)
      dy% = ABS(y2% - y1%) : sy% = SGN(y2% - y1%)
      IF dx% < dy% e = dx% / 2 ELSE e = dy% / 2
      REPEAT
        PROCsetpixel(x1%,y1%,r%,g%,b%)
        IF x1% = x2% IF y1% = y2% EXIT REPEAT
        IF dx% > dy% THEN
          x1% += sx% : e -= dy% : IF e < 0 e += dx% : y1% += sy%
        ELSE
          y1% += sy% : e -= dx% : IF e < 0 e += dy% : x1% += sx%
        ENDIF
      UNTIL FALSE
      ENDPROC

      DEF PROCsetpixel(x%,y%,r%,g%,b%)
      COLOUR 1,r%,g%,b%
      GCOL 1
      LINE x%*2,y%*2,x%*2,y%*2
      ENDPROC
```



## C


Interface (to be added to all other to make the final <tt>imglib.h</tt>):


```c
void quad_bezier(
        image img,
        unsigned int x1, unsigned int y1,
        unsigned int x2, unsigned int y2,
        unsigned int x3, unsigned int y3,
        color_component r,
        color_component g,
        color_component b );
```


Implementation:


```c
#include <math.h>

/* number of segments for the curve */
#define N_SEG 20

#define plot(x, y) put_pixel_clip(img, x, y, r, g, b)
#define line(x0,y0,x1,y1) draw_line(img, x0,y0,x1,y1, r,g,b)

void quad_bezier(
        image img,
        unsigned int x1, unsigned int y1,
        unsigned int x2, unsigned int y2,
        unsigned int x3, unsigned int y3,
        color_component r,
        color_component g,
        color_component b )
{
    unsigned int i;
    double pts[N_SEG+1][2];
    for (i=0; i <= N_SEG; ++i)
    {
        double t = (double)i / (double)N_SEG;
        double a = pow((1.0 - t), 2.0);
        double b = 2.0 * t * (1.0 - t);
        double c = pow(t, 2.0);
        double x = a * x1 + b * x2 + c * x3;
        double y = a * y1 + b * y2 + c * y3;
        pts[i][0] = x;
        pts[i][1] = y;
    }

#if 0
    /* draw only points */
    for (i=0; i <= N_SEG; ++i)
    {
        plot( pts[i][0],
              pts[i][1] );
    }
#else
    /* draw segments */
    for (i=0; i < N_SEG; ++i)
    {
        int j = i + 1;
        line( pts[i][0], pts[i][1],
              pts[j][0], pts[j][1] );
    }
#endif
}
#undef plot
#undef line
```



## D

This solution uses two modules, from the Grayscale image and the Bresenham's line algorithm Tasks.

```d
import grayscale_image, bitmap_bresenhams_line_algorithm;

struct Pt { int x, y; } // Signed.

void quadraticBezier(size_t nSegments=20, Color)
                    (Image!Color im, in Pt p1, in Pt p2, in Pt p3,
                     in Color color)
pure nothrow @nogc if (nSegments > 0) {
    Pt[nSegments + 1] points = void;

    foreach (immutable i, ref p; points) {
        immutable double t = i / double(nSegments),
                         a = (1.0 - t) ^^ 2,
                         b = 2.0 * t * (1.0 - t),
                         c = t ^^ 2;
        p = Pt(cast(typeof(Pt.x))(a * p1.x + b * p2.x + c * p3.x),
               cast(typeof(Pt.y))(a * p1.y + b * p2.y + c * p3.y));
    }

    foreach (immutable i, immutable p; points[0 .. $ - 1])
        im.drawLine(p.x, p.y, points[i + 1].x, points[i + 1].y, color);
}

void main() {
    auto im = new Image!Gray(20, 20);
    im.clear(Gray.white);
    im.quadraticBezier(Pt(1,10), Pt(25,27), Pt(15,2), Gray.black);
    im.textualShow();
}
```

```txt
....................
....................
...............#....
...............#....
...............#....
................#...
................#...
.................#..
.................#..
.................#..
.#...............#..
..##.............#..
....##...........#..
......#..........#..
.......#.........#..
........###......#..
...........######...
....................
....................
....................
```



## FBSL

Windows' graphics origin is located at the bottom-left corner of device bitmap.

'''Translation of BBC BASIC using pure FBSL's built-in graphics functions:'''

```qbasic
#DEFINE WM_LBUTTONDOWN 513
#DEFINE WM_CLOSE 16

FBSLSETTEXT(ME, "Bezier Quadratic")
FBSLSETFORMCOLOR(ME, RGB(0, 255, 255)) ' Cyan: persistent background color
DRAWWIDTH(5) ' Adjust point size
FBSL.GETDC(ME) ' Use volatile FBSL.GETDC below to avoid extra assignments

RESIZE(ME, 0, 0, 235, 235)
CENTER(ME)
SHOW(ME)

DIM Height AS INTEGER
FBSL.GETCLIENTRECT(ME, 0, 0, 0, Height)

BEGIN EVENTS
	SELECT CASE CBMSG
		CASE WM_LBUTTONDOWN: BezierQuad(10, 100, 250, 270, 150, 20, 20) ' Draw
		CASE WM_CLOSE: FBSL.RELEASEDC(ME, FBSL.GETDC) ' Clean up
	END SELECT
END EVENTS

SUB BezierQuad(x1, y1, x2, y2, x3, y3, n)
	TYPE POINTAPI
		x AS INTEGER
		y AS INTEGER
	END TYPE

	DIM t, t1, a, b, c, p[n] AS POINTAPI

	FOR DIM i = 0 TO n
		t = i / n: t1 = 1 - t
		a = t1 ^ 2
		b = 2 * t * t1
		c = t ^ 2
		p[i].x = a * x1 + b * x2 + c * x3 + 0.5
		p[i].y = Height - (a * y1 + b * y2 + c * y3 + 0.5)
	NEXT

	FOR i = 0 TO n - 1
		Bresenham(p[i].x, p[i].y, p[i + 1].x, p[i + 1].y)
	NEXT

	SUB Bresenham(x0, y0, x1, y1)
		DIM dx = ABS(x0 - x1), sx = SGN(x0 - x1)
		DIM dy = ABS(y0 - y1), sy = SGN(y0 - y1)
		DIM tmp, er = IIF(dx > dy, dx, -dy) / 2

		WHILE NOT (x0 = x1 ANDALSO y0 = y1)
			PSET(FBSL.GETDC, x0, y0, &HFF) ' Red: Windows stores colors in BGR order
			tmp = er
			IF tmp > -dx THEN: er = er - dy: x0 = x0 + sx: END IF
			IF tmp < +dy THEN: er = er + dx: y0 = y0 + sy: END IF
		WEND
	END SUB
END SUB
```

'''Output:'''   [[File:FBSLBezierQuad.PNG]]


## Factor

Some code is shared with the cubic bezier task, but I put it here again to make it simple (hoping the two version don't diverge)
Same remark as with cubic bezier, the points could go into a sequence to simplify stack shuffling

```factor
USING: arrays kernel locals math math.functions
 rosettacode.raster.storage sequences ;
IN: rosettacode.raster.line

! This gives a function
:: (quadratic-bezier) ( P0 P1 P2 -- bezier )
    [ :> x
        1 x - sq P0 n*v
        2 1 x - x * * P1 n*v
        x sq P2 n*v
        v+ v+ ] ; inline

! Same code from the cubic bezier task
: t-interval ( x -- interval )
    [ iota ] keep 1 - [ / ] curry map ;
: points-to-lines ( seq -- seq )
    dup rest [ 2array ] 2map ;
: draw-lines ( {R,G,B} points image -- )
    [ [ first2 ] dip draw-line ] curry with each ;
:: bezier-lines ( {R,G,B} P0 P1 P2 image -- )
    100 t-interval P0 P1 P2 (quadratic-bezier) map
    points-to-lines
    {R,G,B} swap image draw-lines ;

```



## Fortran

(This subroutine must be inside the <code>RCImagePrimitive</code> module, see [[Bresenham's line algorithm#Fortran|here]])


```fortran
subroutine quad_bezier(img, p1, p2, p3, color)
  type(rgbimage), intent(inout) :: img
  type(point), intent(in) :: p1, p2, p3
  type(rgb), intent(in) :: color

  integer :: i, j
  real :: pts(0:N_SEG,0:1), t, a, b, c, x, y

  do i = 0, N_SEG
     t = real(i) / real(N_SEG)
     a = (1.0 - t)**2.0
     b = 2.0 * t * (1.0 - t)
     c = t**2.0
     x = a * p1%x + b * p2%x + c * p3%x
     y = a * p1%y + b * p2%y + c * p3%y
     pts(i,0) = x
     pts(i,1) = y
  end do

  do i = 0, N_SEG-1
     j = i + 1
     call draw_line(img, point(pts(i,0), pts(i,1)), &
                    point(pts(j,0), pts(j,1)), color)
  end do

end subroutine quad_bezier
```


## FreeBASIC

```freebasic
' version 01-11-2016
' compile with: fbc -s console

' translation from Bitmap/Bresenham's line algorithm C entry
Sub Br_line(x0 As Integer, y0 As Integer, x1 As Integer, y1 As Integer, _
                                            Col As UInteger = &HFFFFFF)

    Dim As Integer dx = Abs(x1 - x0), dy = Abs(y1 - y0)
    Dim As Integer sx = IIf(x0 < x1, 1, -1)
    Dim As Integer sy = IIf(y0 < y1, 1, -1)
    Dim As Integer er = IIf(dx > dy, dx, -dy) \ 2, e2

    Do
        PSet(x0, y0), col
        If (x0 = x1) And (y0 = y1) Then Exit Do
        e2 = er
        If e2 > -dx Then Er -= dy : x0 += sx
        If e2 <  dy Then Er += dx : y0 += sy
    Loop

End Sub

' Bitmap/Bézier curves/Quadratic BBC BASIC entry
Sub bezierquad(x1 As Double, y1 As Double, x2 As Double, y2 As Double, _
    x3 As Double, y3 As Double, n As ULong, col As UInteger = &HFFFFFF)

    Type point_
        x As Integer
        y As Integer
    End Type

    Dim As ULong i
    Dim As Double t, t1, a, b, c, d
    Dim As point_ p(n)

    For i = 0 To n
        t = i / n
        t1 = 1 - t
        a = t1 ^ 2
        b = t * t1 * 2
        c = t ^ 2
        p(i).x = Int(a * x1 + b * x2  + c * x3 + .5)
        p(i).y = Int(a * y1 + b * y2  + c * y3 + .5)
    Next

    For i = 0 To n -1
        Br_line(p(i).x, p(i).y, p(i +1).x, p(i +1).y, col)
    Next

End Sub

' ------=< MAIN >=------

ScreenRes 250, 250, 32  ' 0,0 in top left corner

bezierquad(10, 100, 250, 270, 150, 20, 20)


' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```



## Go

```go
package raster

const b2Seg = 20

func (b *Bitmap) Bézier2(x1, y1, x2, y2, x3, y3 int, p Pixel) {
    var px, py [b2Seg + 1]int
    fx1, fy1 := float64(x1), float64(y1)
    fx2, fy2 := float64(x2), float64(y2)
    fx3, fy3 := float64(x3), float64(y3)
    for i := range px {
        c := float64(i) / b2Seg
        a := 1 - c
        a, b, c := a*a, 2 * c * a, c*c
        px[i] = int(a*fx1 + b*fx2 + c*fx3)
        py[i] = int(a*fy1 + b*fy2 + c*fy3)
    }
    x0, y0 := px[0], py[0]
    for i := 1; i <= b2Seg; i++ {
        x1, y1 := px[i], py[i]
        b.Line(x0, y0, x1, y1, p)
        x0, y0 = x1, y1
    }
}

func (b *Bitmap) Bézier2Rgb(x1, y1, x2, y2, x3, y3 int, c Rgb) {
    b.Bézier2(x1, y1, x2, y2, x3, y3, c.Pixel())
}
```

Demonstration program:
[[File:GoBez2.png|thumb|right]]

```go
package main

import (
    "fmt"
    "raster"
)

func main() {
    b := raster.NewBitmap(400, 300)
    b.FillRgb(0xdfffef)
    b.Bézier2Rgb(20, 150, 500, -100, 300, 280, raster.Rgb(0x3f8fef))
    if err := b.WritePpmFile("bez2.ppm"); err != nil {
        fmt.Println(err)
    }
}
```



## Haskell


```haskell
{-# LANGUAGE
    FlexibleInstances, TypeSynonymInstances,
    ViewPatterns #-}

import Bitmap
import Bitmap.Line
import Control.Monad
import Control.Monad.ST

type Point = (Double, Double)
fromPixel (Pixel (x, y)) = (toEnum x, toEnum y)
toPixel (x, y) = Pixel (round x, round y)

pmap :: (Double -> Double) -> Point -> Point
pmap f (x, y) = (f x, f y)

onCoordinates :: (Double -> Double -> Double) -> Point -> Point -> Point
onCoordinates f (xa, ya) (xb, yb) = (f xa xb, f ya yb)

instance Num Point where
    (+) = onCoordinates (+)
    (-) = onCoordinates (-)
    (*) = onCoordinates (*)
    negate = pmap negate
    abs = pmap abs
    signum = pmap signum
    fromInteger i = (i', i')
      where i' = fromInteger i

bézier :: Color c =>
    Image s c -> Pixel -> Pixel -> Pixel -> c -> Int ->
    ST s ()
bézier
  i
  (fromPixel -> p1) (fromPixel -> p2) (fromPixel -> p3)
  c samples =
    zipWithM_ f ts (tail ts)
  where ts = map (/ top) [0 .. top]
          where top = toEnum $ samples - 1
        curvePoint t =
            pt (t' ^^ 2) p1 +
            pt (2 * t * t') p2 +
            pt (t ^^ 2) p3
          where t' = 1 - t
                pt n p = pmap (*n) p
        f (curvePoint -> p1) (curvePoint -> p2) =
            line i (toPixel p1) (toPixel p2) c
```



## J

See [[Cubic bezier curves#J|Cubic bezier curves]] for a generalized solution.


## Julia

See [[Cubic bezier curves#Julia]] for a generalized solution.


## Kotlin

This incorporates code from other relevant tasks in order to provide a runnable example.

```scala
// Version 1.2.40

import java.awt.Color
import java.awt.Graphics
import java.awt.image.BufferedImage
import kotlin.math.abs
import java.io.File
import javax.imageio.ImageIO

class Point(var x: Int, var y: Int)

class BasicBitmapStorage(width: Int, height: Int) {
    val image = BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    fun fill(c: Color) {
        val g = image.graphics
        g.color = c
        g.fillRect(0, 0, image.width, image.height)
    }

    fun setPixel(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB())

    fun getPixel(x: Int, y: Int) = Color(image.getRGB(x, y))

    fun drawLine(x0: Int, y0: Int, x1: Int, y1: Int, c: Color) {
        val dx = abs(x1 - x0)
        val dy = abs(y1 - y0)
        val sx = if (x0 < x1) 1 else -1
        val sy = if (y0 < y1) 1 else -1
        var xx = x0
        var yy = y0
        var e1 = (if (dx > dy) dx else -dy) / 2
        var e2: Int
        while (true) {
            setPixel(xx, yy, c)
            if (xx == x1 && yy == y1) break
            e2 = e1
            if (e2 > -dx) { e1 -= dy; xx += sx }
            if (e2 <  dy) { e1 += dx; yy += sy }
        }
    }

    fun quadraticBezier(p1: Point, p2: Point, p3: Point, clr: Color, n: Int) {
        val pts = List(n + 1) { Point(0, 0) }
        for (i in 0..n) {
            val t = i.toDouble() / n
            val u = 1.0 - t
            val a = u * u
            val b = 2.0 * t * u
            val c = t * t
            pts[i].x = (a * p1.x + b * p2.x + c * p3.x).toInt()
            pts[i].y = (a * p1.y + b * p2.y + c * p3.y).toInt()
            setPixel(pts[i].x, pts[i].y, clr)
        }
        for (i in 0 until n) {
            val j = i + 1
            drawLine(pts[i].x, pts[i].y, pts[j].x, pts[j].y, clr)
        }
    }
}

fun main(args: Array<String>) {
    val width = 320
    val height = 320
    val bbs = BasicBitmapStorage(width, height)
    with (bbs) {
        fill(Color.cyan)
        val p1 = Point(10, 100)
        val p2 = Point(250, 270)
        val p3 = Point(150, 20)
        quadraticBezier(p1, p2, p3, Color.black, 20)
        val qbFile = File("quadratic_bezier.jpg")
        ImageIO.write(image, "jpg", qbFile)
    }
}
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
pts = {{0, 0}, {1, -1}, {2, 1}};
Graphics[{BSplineCurve[pts], Green, Line[pts], Red, Point[pts]}]
```

Second solution using built-in function BezierCurve.

```Mathematica
pts = {{0, 0}, {1, -1}, {2, 1}};
Graphics[{BezierCurve[pts], Green, Line[pts], Red, Point[pts]}]
```

[[File:MmaQuadraticBezier.png]]


## MATLAB

Note: Store this function in a file named "bezierQuad.mat" in the @Bitmap folder for the Bitmap class defined [[Bitmap#MATLAB|here]].

```MATLAB

function bezierQuad(obj,pixel_0,pixel_1,pixel_2,color,varargin)

    if( isempty(varargin) )
        resolution = 20;
    else
        resolution = varargin{1};
    end

    %Calculate time axis
    time = (0:1/resolution:1)';
    timeMinus = 1-time;

    %The formula for the curve is expanded for clarity, the lack of
    %loops is because its calculation has been vectorized
    curve = (timeMinus.^2)*pixel_0; %First term of polynomial
    curve = curve + (2.*time.*timeMinus)*pixel_1; %second term of polynomial
    curve = curve + (time.^2)*pixel_2; %third term of polynomial

    curve = round(curve); %round each of the points to the nearest integer

    %connect each of the points in the curve with a line using the
    %Bresenham Line algorithm
    for i = (1:length(curve)-1)
        obj.bresenhamLine(curve(i,:),curve(i+1,:),color);
    end

    assignin('caller',inputname(1),obj); %saves the changes to the object

end

```


Sample usage:
This will generate the image example for the Go solution.

```MATLAB

>> img = Bitmap(400,300);
>> img.fill([223 255 239]);
>> img.bezierQuad([20 150],[500 -100],[300 280],[63 143 239],21);
>> disp(img)

```




## OCaml



```ocaml
let quad_bezier ~img ~color
        ~p1:(_x1, _y1)
        ~p2:(_x2, _y2)
        ~p3:(_x3, _y3) =
  let (x1, y1, x2, y2, x3, y3) =
    (float _x1, float _y1, float _x2, float _y2, float _x3, float _y3)
  in
  let bz t =
    let a = (1.0 -. t) ** 2.0
    and b = 2.0 *. t *. (1.0 -. t)
    and c = t ** 2.0
    in
    let x = a *. x1 +. b *. x2 +. c *. x3
    and y = a *. y1 +. b *. y2 +. c *. y3
    in
    (int_of_float x, int_of_float y)
  in
  let rec loop _t acc =
    if _t > 20 then acc else
    begin
      let t = (float _t) /. 20.0 in
      let x, y = bz t in
      loop (succ _t) ((x,y)::acc)
    end
  in
  let pts = loop 0 [] in

  (*
  (* draw only points *)
  List.iter (fun (x, y) -> put_pixel img color x y) pts;
  *)

  (* draw segments *)
  let line = draw_line ~img ~color in
  let by_pair li f =
    ignore (List.fold_left (fun prev x -> f prev x; x) (List.hd li) (List.tl li))
  in
  by_pair pts (fun p0 p1 -> line ~p0 ~p1);
;;
```



## Perl 6

Uses pieces from [[Bitmap#Perl_6| Bitmap]], and [[Bitmap/Bresenham's_line_algorithm#Perl_6| Bresenham's line algorithm]] tasks. They are included here to make a complete, runnable program.


```perl6
class Pixel { has UInt ($.R, $.G, $.B) }
class Bitmap {
    has UInt ($.width, $.height);
    has Pixel @!data;

    method fill(Pixel $p) {
        @!data = $p.clone xx ($!width*$!height)
    }
    method pixel(
	  $i where ^$!width,
	  $j where ^$!height
	  --> Pixel
    ) is rw { @!data[$i + $j * $!width] }

    method set-pixel ($i, $j, Pixel $p) {
        return if $j >= $!height;
        self.pixel($i, $j) = $p.clone;
    }
    method get-pixel ($i, $j) returns Pixel {
	    self.pixel($i, $j);
    }

    method line(($x0 is copy, $y0 is copy), ($x1 is copy, $y1 is copy), $pix) {
        my $steep = abs($y1 - $y0) > abs($x1 - $x0);
        if $steep {
            ($x0, $y0) = ($y0, $x0);
            ($x1, $y1) = ($y1, $x1);
        }
        if $x0 > $x1 {
            ($x0, $x1) = ($x1, $x0);
            ($y0, $y1) = ($y1, $y0);
        }
        my $Δx = $x1 - $x0;
        my $Δy = abs($y1 - $y0);
        my $error = 0;
        my $Δerror = $Δy / $Δx;
        my $y-step = $y0 < $y1 ?? 1 !! -1;
        my $y = $y0;
        for $x0 .. $x1 -> $x {
            if $steep {
                self.set-pixel($y, $x, $pix);
            } else {
                self.set-pixel($x, $y, $pix);
            }
            $error += $Δerror;
            if $error >= 0.5 {
                $y += $y-step;
                $error -= 1.0;
            }
        }
    }

    method dot (($px, $py), $pix, $radius = 2) {
        for $px - $radius .. $px + $radius -> $x {
            for $py - $radius .. $py + $radius -> $y {
                self.set-pixel($x, $y, $pix) if ( $px - $x + ($py - $y) * i ).abs <= $radius;
            }
        }
    }

    method quadratic ( ($x1, $y1), ($x2, $y2), ($x3, $y3), $pix, $segments = 30 ) {
        my @line-segments = map -> $t {
            my \a = (1-$t)²;
            my \b = $t * (1-$t) * 2;
            my \c = $t²;
            (a*$x1 + b*$x2 + c*$x3).round(1),(a*$y1 + b*$y2 + c*$y3).round(1)
        }, (0, 1/$segments, 2/$segments ... 1);
        for @line-segments.rotor(2=>-1) -> ($p1, $p2) { self.line( $p1, $p2, $pix) };
    }

    method data { @!data }
}

role PPM {
    method P6 returns Blob {
	"P6\n{self.width} {self.height}\n255\n".encode('ascii')
	~ Blob.new: flat map { .R, .G, .B }, self.data
    }
}

sub color( $r, $g, $b) { Pixel.new(R => $r, G => $g, B => $b) }

my Bitmap $b = Bitmap.new( width => 600, height => 400) but PPM;

$b.fill( color(2,2,2) );

my @points = (65,25), (85,380), (570,15);

my %seen;
my $c = 0;
for @points.permutations -> @this {
    %seen{@this.reverse.join.Str}++;
    next if %seen{@this.join.Str};
    $b.quadratic( |@this, color(255-$c,127,$c+=80) );
}

@points.map: { $b.dot( $_, color(255,0,0), 3 )}

$*OUT.write: $b.P6;
```


See [https://github.com/thundergnat/rc/blob/master/img/Bezier-quadratic-perl6.png example image here], (converted to a .png as .ppm format is not widely supported).


## Phix

Output similar to [[Bitmap/Bézier_curves/Quadratic#Mathematica|Mathematica]]
Requires new_image() from [[Bitmap#Phix|Bitmap]], bresLine() from [[Bitmap/Bresenham's_line_algorithm#Phix|Bresenham's_line_algorithm]], write_ppm() from [[Bitmap/Write_a_PPM_file#Phix|Write_a_PPM_file]].
Included as demo\rosetta\Bitmap_BezierQuadratic.exw, results may be verified with demo\rosetta\viewppm.exw

```Phix
function quadratic_bezier(sequence img, atom x1, atom y1, atom x2, atom y2, atom x3, atom y3, integer colour, integer segments)
atom t, t1, a, b, c
sequence pts = repeat(0,segments*2)

    for i=0 to segments*2-1 by 2 do
        t = i/segments
        t1 = 1-t
        a = power(t1,2)
        b = 2*t*t1
        c = power(t,2)
        pts[i+1] = floor(a*x1+b*x2+c*x3)
        pts[i+2] = floor(a*y1+b*y2+c*y3)
    end for
    for i=1 to segments*2-2 by 2 do
        img = bresLine(img, pts[i], pts[i+1], pts[i+2], pts[i+3], colour)
    end for
    return img
end function

sequence img = new_image(200,200,black)
    img = quadratic_bezier(img, 0,100, 100,200, 200,0, white, 40)
    img = bresLine(img,0,100,100,200,green)
    img = bresLine(img,100,200,200,0,green)
    img[1][100] = red
    img[100][200] = red
    img[200][1] = red
    write_ppm("BézierQ.ppm",img)
```



## PicoLisp

This uses the 'brez' line drawing function from
[[Bitmap/Bresenham's line algorithm#PicoLisp]].

```PicoLisp
(scl 6)

(de quadBezier (Img N X1 Y1 X2 Y2 X3 Y3)
   (let (R (* N N)  X X1  Y Y1  DX 0  DY 0)
      (for I N
         (let (J (- N I)  A (*/ 1.0 J J R)  B (*/ 2.0 I J R)  C (*/ 1.0 I I R))
            (brez Img X Y
               (setq DX (- (+ (*/ A X1 1.0) (*/ B X2 1.0) (*/ C X3 1.0)) X))
               (setq DY (- (+ (*/ A Y1 1.0) (*/ B Y2 1.0) (*/ C Y3 1.0)) Y)) )
            (inc 'X DX)
            (inc 'Y DY) ) ) ) )
```

Test:

```PicoLisp
(let Img (make (do 200 (link (need 300 0))))       # Create image 300 x 200
   (quadBezier Img 12 20 100 300 -80 260 180)
   (out "img.pbm"                                  # Write to bitmap file
      (prinl "P1")
      (prinl 300 " " 200)
      (mapc prinl Img) ) )

(call 'display "img.pbm")
```



## PureBasic


```PureBasic
Procedure quad_bezier(img, p1x, p1y, p2x, p2y, p3x, p3y, Color, n_seg)
  Protected i
  Protected.f T, t1, a, b, c, d
  Dim pts.POINT(n_seg)

  For i = 0 To n_seg
    T = i / n_seg
    t1 = 1.0 - T
    a = Pow(t1, 2)
    b = 2.0 * T * t1
    c = Pow(T, 2)
    pts(i)\x = a * p1x + b * p2x + c * p3x
    pts(i)\y = a * p1y + b * p2y + c * p3y
  Next

  StartDrawing(ImageOutput(img))
    FrontColor(Color)
    For i = 0 To n_seg - 1
      BresenhamLine(pts(i)\x, pts(i)\y, pts(i + 1)\x, pts(i + 1)\y)
    Next
  StopDrawing()
EndProcedure

Define w, h, img
w = 200: h = 200: img = 1
CreateImage(img, w, h) ;img is internal id of the image

OpenWindow(0, 0, 0, w, h,"Bezier curve, quadratic", #PB_Window_SystemMenu)
quad_bezier(1, 80,20, 130,80, 20,150, RGB(255, 255, 255), 20)
ImageGadget(0, 0, 0, w, h, ImageID(1))

Define event
Repeat
  event = WaitWindowEvent()
Until event = #PB_Event_CloseWindow

```



## Python

See [[Cubic bezier curves#Python]] for a generalized solution.


## R

See [[Cubic bezier curves#R]] for a generalized solution.


## Racket


```racket

#lang racket
(require racket/draw)

(define (draw-line dc p q)
  (match* (p q) [((list x y) (list s t)) (send dc draw-line x y s t)]))

(define (draw-lines dc ps)
  (void
   (for/fold ([p0 (first ps)]) ([p (rest ps)])
     (draw-line dc p0 p)
     p)))

(define (int t p q)
  (define ((int1 t) x0 x1) (+ (* (- 1 t) x0) (* t x1)))
  (map (int1 t) p q))

(define (bezier-points p0 p1 p2)
  (for/list ([t (in-range 0.0 1.0 (/ 1.0 20))])
    (int t (int t p0 p1) (int t p1 p2))))

(define bm (make-object bitmap% 17 17))
(define dc (new bitmap-dc% [bitmap bm]))
(send dc set-smoothing 'unsmoothed)
(send dc set-pen "red" 1 'solid)
(draw-lines dc (bezier-points '(16 1) '(1 4) '(3 16)))
bm

```



## Ruby

See [[Cubic bezier curves#Ruby]] for a generalized solution.


## Tcl

See [[Cubic bezier curves#Tcl]] for a generalized solution.

=={{header|TI-89 BASIC}}==

```ti89b
Define cubic(p1,p2,p3,segs) = Prgm
  Local i,t,u,prev,pt
  0 → pt
  For i,1,segs+1
    (i-1.0)/segs → t   © Decimal to avoid slow exact arithetic
    (1-t) → u
    pt → prev
    u^2*p1 + 2*t*u*p2 + t^2*p3 → pt
    If i>1 Then
      PxlLine floor(prev[1,1]), floor(prev[1,2]), floor(pt[1,1]), floor(pt[1,2])
    EndIf
  EndFor
EndPrgm
```



## Vedit macro language

This implementation uses de Casteljau's algorithm to recursively split the Bezier curve into two smaller segments until the segment is short enough to be approximated with a straight line.
The advantage of this method is that only integer calculations are needed, and the most complex operations are addition and shift right. (I have used multiplication and division here for clarity.)

Constant recursion depth is used here. Recursion depth of 5 seems to give accurate enough result in most situations. In real world implementations, some adaptive method is often used to decide when to stop recursion.


```vedit
// Daw a Cubic bezier curve
//  #20, #30 = Start point
//  #21, #31 = Control point 1
//  #22, #32 = Control point 2
//  #23, #33 = end point
//  #40 = depth of recursion

:CUBIC_BEZIER:
if (#40 > 0) {
    #24 = (#20+#21)/2;              #34 = (#30+#31)/2
    #26 = (#22+#23)/2;              #36 = (#32+#33)/2
    #27 = (#20+#21*2+#22)/4;        #37 = (#30+#31*2+#32)/4
    #28 = (#21+#22*2+#23)/4;        #38 = (#31+#32*2+#33)/4
    #29 = (#20+#21*3+#22*3+#23)/8;  #39 = (#30+#31*3+#32*3+#33)/8
    Num_Push(20,40)
        #21 = #24; #31 = #34    // control 1
        #22 = #27; #32 = #37    // control 2
        #23 = #29; #33 = #39    // end point
        #40--
        Call("CUBIC_BEZIER")    // Draw "left" part
    Num_Pop(20,40)
    Num_Push(20,40)
        #20 = #29; #30 = #39    // start point
        #21 = #28; #31 = #38    // control 1
        #22 = #26; #32 = #36    // control 2
        #40--
        Call("CUBIC_BEZIER")    // Draw "right" part
    Num_Pop(20,40)
} else {
    #1=#20; #2=#30; #3=#23; #4=#33
    Call("DRAW_LINE")
}
return
```


## XPL0

[[File:QuadXPL0.png|right]]

```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

proc Bezier(P0, P1, P2);        \Draw quadratic Bezier curve
real P0, P1, P2;
def  Segments = 8;
int  I;
real T, A, B, C, X, Y;
[Move(fix(P0(0)), fix(P0(1)));
for I:= 1 to Segments do
        [T:= float(I)/float(Segments);
        A:= sq(1.-T);
        B:= 2.*T*(1.-T);
        C:= sq(T);
        X:= A*P0(0) + B*P1(0) + C*P2(0);
        Y:= A*P0(1) + B*P1(1) + C*P2(1);
        Line(fix(X), fix(Y), $00FFFF);          \cyan line segments
        ];
Point(fix(P0(0)), fix(P0(1)), $FF0000);         \red control points
Point(fix(P1(0)), fix(P1(1)), $FF0000);
Point(fix(P2(0)), fix(P2(1)), $FF0000);
];

[SetVid($112);          \set 640x480x24 video graphics
Bezier([0., 0.], [80., 100.], [160., 20.]);
if ChIn(1) then [];     \wait for keystroke
SetVid(3);              \restore normal text display
]
```



## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

Add this to the PPM class:

```zkl
   fcn qBezier(p0x,p0y, p1x,p1y, p2x,p2y, rgb, numPts=500){
      numPts.pump(Void,'wrap(t){ // B(t)
      	 t=t.toFloat()/numPts; t1:=(1.0 - t);
	 a:=t1*t1; b:=t*t1*2; c:=t*t;
	 x:=a*p0x + b*p1x + c*p2x + 0.5;
	 y:=a*p0y + b*p1y + c*p2y + 0.5;
	 __sSet(rgb,x,y);
      });
   }
```

Doesn't use line segments, they don't seem like an improvement.

```zkl
bitmap:=PPM(200,200,0xff|ff|ff);
bitmap.qBezier(10,100, 250,270, 150,20, 0);
bitmap.write(File("foo.ppm","wb"));
```

Same as the BBC BASIC image:[[Image:bezierquad_bbc.gif]]

