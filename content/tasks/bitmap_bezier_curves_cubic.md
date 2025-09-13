+++
title = "Bitmap/Bézier curves/Cubic"
description = ""
date = 2018-05-02T14:14:33Z
aliases = []
[extra]
id = 3219
[taxonomies]
categories = ["task", "Raster graphics operations"]
tags = []
+++

## Task

Using the data storage type defined [[Basic_bitmap_storage|on this page]] for raster images, and the <tt>draw_line</tt> function defined in [[Bresenham's_line_algorithm|this other one]], draw a '''cubic bezier curve'''
([[wp:Bezier_curves#Cubic_B.C3.A9zier_curves|definition on Wikipedia]]).



## Ada


```ada
procedure Cubic_Bezier
          (  Picture        : in out Image;
             P1, P2, P3, P4 : Point;
             Color          : Pixel;
             N              : Positive := 20
          )  is
   Points : array (0..N) of Point;
begin
   for I in Points'Range loop
      declare
         T : constant Float := Float (I) / Float (N);
         A : constant Float := (1.0 - T)**3;
         B : constant Float := 3.0 * T * (1.0 - T)**2;
         C : constant Float := 3.0 * T**2 * (1.0 - T);
         D : constant Float := T**3;
      begin
         Points (I).X := Positive (A * Float (P1.X) + B * Float (P2.X) + C * Float (P3.X) + D * Float (P4.X));
         Points (I).Y := Positive (A * Float (P1.Y) + B * Float (P2.Y) + C * Float (P3.Y) + D * Float (P4.Y));
      end;
   end loop;
   for I in Points'First..Points'Last - 1 loop
      Line (Picture, Points (I), Points (I + 1), Color);
   end loop;
end Cubic_Bezier;
```

The following test

```ada
   X : Image (1..16, 1..16);
begin
   Fill (X, White);
   Cubic_Bezier (X, (16, 1), (1, 4), (3, 16), (15, 11), Black);
   Print (X);
```

should produce output:

```txt





       HH
     HH  HH
    H      H
    H      H
   H       H
  H        H
 H         H
 H         H
 H         H
 H         H
H         H
H

```


## ALGOL 68

'''File: prelude/Bitmap/Bezier_curves/Cubic.a68'''
```algol68
# -*- coding: utf-8 -*- #

cubic bezier OF class image :=
          (  REF IMAGE picture,
             POINT p1, p2, p3, p4,
             PIXEL color,
             UNION(INT, VOID) in n
          )VOID:
BEGIN
   INT n = (in n|(INT n):n|20); # default 20 #
   [0:n]POINT points;
   FOR i FROM LWB points TO UPB points DO
         REAL t = i / n,
              a = (1 - t)**3,
              b = 3 * t * (1 - t)**2,
              c = 3 * t**2 * (1 - t),
              d = t**3;
         x OF points [i] := ENTIER (0.5 + a * x OF p1 + b * x OF p2 + c * x OF p3 + d * x OF p4);
         y OF points [i] := ENTIER (0.5 + a * y OF p1 + b * y OF p2 + c * y OF p3 + d * y OF p4)
   OD;
   FOR i FROM LWB points TO UPB points - 1 DO
      (line OF class image)(picture, points (i), points (i + 1), color)
   OD
END # cubic bezier #;

SKIP
```
'''File: test/Bitmap/Bezier_curves/Cubic.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

PR READ "prelude/Bitmap.a68" PR; # c.f. [[rc:Bitmap]] #
PR READ "prelude/Bitmap/Bresenhams_line_algorithm.a68" PR; # c.f. [[rc:Bitmap/Bresenhams_line_algorithm]] #
PR READ "prelude/Bitmap/Bezier_curves/Cubic.a68" PR;

# The following test #
test:(
   REF IMAGE x = INIT LOC[16,16]PIXEL;
   (fill OF class image)(x, (white OF class image));
   (cubic bezier OF class image)(x, (16, 1), (1, 4), (3, 16), (15, 11), (black OF class image), EMPTY);
   (print OF class image) (x)
)
```
'''Output:'''

```txt

ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffff000000000000ffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffff000000000000ffffffffffff000000000000ffffffffffffffffffffffffffffff
ffffffffffffffffffffffff000000ffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffff
ffffffffffffffffffffffff000000ffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffff
ffffffffffffffffff000000ffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffff
ffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffff
ffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffff
ffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffff
ffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffff
ffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffff
000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffffffffff
000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

```



## BBC BASIC

[[Image:beziercubic_bbc.gif|right]]

```bbcbasic
      Width% = 200
      Height% = 200

      REM Set window size:
      VDU 23,22,Width%;Height%;8,16,16,128

      REM Draw cubic Bézier curve:
      PROCbeziercubic(160,150, 10,120, 30,0, 150,50, 20, 0,0,0)
      END

      DEF PROCbeziercubic(x1,y1,x2,y2,x3,y3,x4,y4,n%,r%,g%,b%)
      LOCAL i%, t, t1, a, b, c, d, p{()}
      DIM p{(n%) x%,y%}

      FOR i% = 0 TO n%
        t = i% / n%
        t1 = 1 - t
        a = t1^3
        b = 3 * t * t1^2
        c = 3 * t^2 * t1
        d = t^3
        p{(i%)}.x% = INT(a * x1 + b * x2 + c * x3 + d * x4 + 0.5)
        p{(i%)}.y% = INT(a * y1 + b * y2 + c * y3 + d * y4 + 0.5)
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

"Interface" <tt>imglib.h</tt>.


```c
void cubic_bezier(
       	image img,
        unsigned int x1, unsigned int y1,
        unsigned int x2, unsigned int y2,
        unsigned int x3, unsigned int y3,
        unsigned int x4, unsigned int y4,
        color_component r,
        color_component g,
        color_component b );
```



```c
#include <math.h>

/* number of segments for the curve */
#define N_SEG 20

#define plot(x, y) put_pixel_clip(img, x, y, r, g, b)
#define line(x0,y0,x1,y1) draw_line(img, x0,y0,x1,y1, r,g,b)

void cubic_bezier(
       	image img,
        unsigned int x1, unsigned int y1,
        unsigned int x2, unsigned int y2,
        unsigned int x3, unsigned int y3,
        unsigned int x4, unsigned int y4,
        color_component r,
        color_component g,
        color_component b )
{
    unsigned int i;
    double pts[N_SEG+1][2];
    for (i=0; i <= N_SEG; ++i)
    {
        double t = (double)i / (double)N_SEG;

        double a = pow((1.0 - t), 3.0);
        double b = 3.0 * t * pow((1.0 - t), 2.0);
        double c = 3.0 * pow(t, 2.0) * (1.0 - t);
        double d = pow(t, 3.0);

        double x = a * x1 + b * x2 + c * x3 + d * x4;
        double y = a * y1 + b * y2 + c * y3 + d * y4;
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

This solution uses two modules, from the Grayscale image and Bresenham's line algorithm Tasks.

```d
import grayscale_image, bitmap_bresenhams_line_algorithm;

struct Pt { int x, y; } // Signed.

void cubicBezier(size_t nSegments=20, Color)
                (Image!Color im,
                 in Pt p1, in Pt p2, in Pt p3, in Pt p4,
                 in Color color)
pure nothrow @nogc if (nSegments > 0) {
    Pt[nSegments + 1] points = void;

    foreach (immutable i, ref p; points) {
        immutable double t = i / double(nSegments),
                         a = (1.0 - t) ^^ 3,
                         b = 3.0 * t * (1.0 - t) ^^ 2,
                         c = 3.0 * t ^^ 2 * (1.0 - t),
                         d = t ^^ 3;

        alias T = typeof(Pt.x);
        p = Pt(cast(T)(a * p1.x + b * p2.x + c * p3.x + d * p4.x),
               cast(T)(a * p1.y + b * p2.y + c * p3.y + d * p4.y));
    }

    foreach (immutable i, immutable p; points[0 .. $ - 1])
        im.drawLine(p.x, p.y, points[i + 1].x, points[i + 1].y, color);
}

void main() {
    auto im = new Image!Gray(17, 17);
    im.clear(Gray.white);
    im.cubicBezier(Pt(16, 1), Pt(1, 4), Pt(3, 16), Pt(15, 11),
                   Gray.black);
    im.textualShow();
}
```

```txt
.................
.............####
.........####....
........#........
.......#.........
......#..........
......#..........
.....#...........
.....#...........
.....#...........
.....#...........
......##....####.
........####.....
.................
.................
.................
.................
```


=={{header|F Sharp|F#}}==

```f#

/// Uses Vector<float> from Microsoft.FSharp.Math (in F# PowerPack)
module CubicBezier

/// Create bezier curve from p1 to p4, using the control points p2, p3
/// Returns the requested number of segments
let cubic_bezier (p1:vector) (p2:vector) (p3:vector) (p4:vector) segments =
    [0 .. segments - 1]
        |> List.map(fun i ->
            let t = float i / float segments
            let a = (1. - t) ** 3.
            let b = 3. * t * ((1. - t) ** 2.)
            let c = 3. * (t ** 2.) * (1. - t)
            let d = t ** 3.
            let x = a * p1.[0] + b * p2.[0] + c * p3.[0] + d * p4.[0]
            let y = a * p1.[1] + b * p2.[1] + c * p3.[1] + d * p4.[1]
            vector [x; y])


```


```f#

// For rendering..
let drawPoints points (canvas:System.Windows.Controls.Canvas) =
    let addLineToScreen (v1:vector) (v2:vector) =
        canvas.Children.Add(new System.Windows.Shapes.Line(X1 = v1.[0],
                                        Y1 = -v1.[1],
                                        X2 = v2.[0],
                                        Y2 = -v2.[1],
                                        StrokeThickness = 2.)) |> ignore
    let renderPoint (previous:vector) (current:vector) =
        addLineToScreen previous current
        current

    points |> List.fold renderPoint points.Head

```



## FBSL

Windows' graphics origin is located at the bottom-left corner of device bitmap.

'''Translation of BBC BASIC using pure FBSL's built-in graphics functions:'''

```qbasic
#DEFINE WM_LBUTTONDOWN 513
#DEFINE WM_CLOSE 16

FBSLSETTEXT(ME, "Bezier Cubic")
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
		CASE WM_LBUTTONDOWN: BezierCube(160, 150, 10, 120, 30, 0, 150, 50, 20) ' Draw
		CASE WM_CLOSE: FBSL.RELEASEDC(ME, FBSL.GETDC) ' Clean up
	END SELECT
END EVENTS

SUB BezierCube(x1, y1, x2, y2, x3, y3, x4, y4, n)
	TYPE POINTAPI
		x AS INTEGER
		y AS INTEGER
	END TYPE

	DIM t, t1, a, b, c, d, p[n] AS POINTAPI

	FOR DIM i = 0 TO n
		t = i / n: t1 = 1 - t
		a = t1 ^ 3
		b = 3 * t * t1 ^ 2
		c = 3 * t ^ 2 * t1
		d = t ^ 3
		p[i].x = a * x1 + b * x2 + c * x3 + d * x4 + 0.5
		p[i].y = Height - (a * y1 + b * y2 + c * y3 + d * y4 + 0.5)
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

'''Output:'''   [[File:FBSLBezierCube.PNG]]


## Factor

The points should probably be in a sequence...

```factor
USING: arrays kernel locals math math.functions
 rosettacode.raster.storage sequences ;
IN: rosettacode.raster.line

! this gives a function
:: (cubic-bezier) ( P0 P1 P2 P3 -- bezier )
    [ :> x
        1 x - 3 ^ P0 n*v
        1 x - sq 3 * x * P1 n*v
        1 x - 3 * x sq * P2 n*v
        x 3 ^ P3 n*v
        v+ v+ v+ ] ; inline
! gives an interval of x from 0 to 1 to map the bezier function
: t-interval ( x -- interval )
    [ iota ] keep 1 - [ / ] curry map ;
! turns a list of points into the list of lines between them
: points-to-lines ( seq -- seq )
    dup rest [ 2array ] 2map ;
: draw-lines ( {R,G,B} points image -- )
    [ [ first2 ] dip draw-line ] curry with each ;
:: bezier-lines ( {R,G,B} P0 P1 P2 P3 image -- )
    ! 100 is an arbitrary value.. could be given as a parameter..
    100 t-interval P0 P1 P2 P3 (cubic-bezier) map
    points-to-lines
    {R,G,B} swap image draw-lines ;
```



## Fortran

This subroutine should go inside the <code>RCImagePrimitive</code> module (see [[Bresenham's line algorithm]])


```fortran
subroutine cubic_bezier(img, p1, p2, p3, p4, color)
  type(rgbimage), intent(inout) :: img
  type(point), intent(in) :: p1, p2, p3, p4
  type(rgb), intent(in) :: color

  integer :: i, j
  real :: pts(0:N_SEG,0:1), t, a, b, c, d, x, y

  do i = 0, N_SEG
     t = real(i) / real(N_SEG)
     a = (1.0 - t)**3.0
     b = 3.0 * t * (1.0 - t)**2
     c = 3.0 * (1.0 - t) * t**2
     d = t**3.0
     x = a * p1%x + b * p2%x + c * p3%x + d * p4%x
     y = a * p1%y + b * p2%y + c * p3%y + d * p4%y
     pts(i,0) = x
     pts(i,1) = y
  end do

  do i = 0, N_SEG-1
     j = i + 1
     call draw_line(img, point(pts(i,0), pts(i,1)), &
                    point(pts(j,0), pts(j,1)), color)
  end do

end subroutine cubic_bezier
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

' Bitmap/Bézier curves/Cubic BBC BASIC entry
Sub beziercubic(x1 As Double, y1 As Double, x2 As Double, y2 As Double, _
                x3 As Double, y3 As Double, x4 As Double, y4 As Double, _
                                n As ULong, col As UInteger = &HFFFFFF)

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
        a = t1 ^ 3
        b = t * t1 * t1 * 3
        c = t * t * t1 * 3
        d = t ^ 3
        p(i).x = Int(a * x1 + b * x2  + c * x3 + d * x4 + .5)
        p(i).y = Int(a * y1 + b * y2  + c * y3 + d * y4 + .5)
    Next

    For i = 0 To n -1
        Br_line(p(i).x, p(i).y, p(i +1).x, p(i +1).y, col)
    Next

End Sub

' ------=< MAIN >=------

ScreenRes 250,250,32     ' 0,0 in top left corner

beziercubic(160, 150, 10, 120, 30, 0, 150, 50, 20)


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```



## Go

```go
package raster

const b3Seg = 30

func (b *Bitmap) Bézier3(x1, y1, x2, y2, x3, y3, x4, y4 int, p Pixel) {
    var px, py [b3Seg + 1]int
    fx1, fy1 := float64(x1), float64(y1)
    fx2, fy2 := float64(x2), float64(y2)
    fx3, fy3 := float64(x3), float64(y3)
    fx4, fy4 := float64(x4), float64(y4)
    for i := range px {
        d := float64(i) / b3Seg
        a := 1 - d
        b, c := a * a, d * d
        a, b, c, d = a*b, 3*b*d, 3*a*c, c*d
        px[i] = int(a*fx1 + b*fx2 + c*fx3 + d*fx4)
        py[i] = int(a*fy1 + b*fy2 + c*fy3 + d*fy4)
    }
    x0, y0 := px[0], py[0]
    for i := 1; i <= b3Seg; i++ {
        x1, y1 := px[i], py[i]
        b.Line(x0, y0, x1, y1, p)
        x0, y0 = x1, y1
    }
}

func (b *Bitmap) Bézier3Rgb(x1, y1, x2, y2, x3, y3, x4, y4 int, c Rgb) {
    b.Bézier3(x1, y1, x2, y2, x3, y3, x4, y4, c.Pixel())
}
```

Demonstration program:
[[File:GoBez3.png|thumb|right]]

```go
package main

import (
    "fmt"
    "raster"
)

func main() {
    b := raster.NewBitmap(400, 300)
    b.FillRgb(0xffefbf)
    b.Bézier3Rgb(20, 200, 700, 50, -300, 50, 380, 150, raster.Rgb(0x3f8fef))
    if err := b.WritePpmFile("bez3.ppm"); err != nil {
        fmt.Println(err)
    }
}
```



## J

'''Solution:'''

See the [[J:Essays/Bernstein Polynomials|Bernstein Polynomials essay]] on the [[J:|J Wiki]].

Uses code from [[Basic_bitmap_storage#J|Basic bitmap storage]], [[Bresenham's_line_algorithm#J|Bresenham's line algorithm]] and [[Midpoint_circle_algorithm#J|Midpoint circle algorithm]].

```j
require 'numeric'

bik=: 2 : '((*&(u!v))@(^&u * ^&(v-u)@-.))'
basiscoeffs=: <: 4 : 'x bik y t. i.>:y'"0~ i.
linearcomb=: basiscoeffs@#@[
evalBernstein=: ([ +/ .* linearcomb) p. ]        NB. evaluate Bernstein Polynomial (general)

NB.*getBezierPoints v Returns points for bezier curve given control points (y)
NB. eg: getBezierPoints controlpoints
NB. y is: y0 x0, y1 x1, y2 x2 ...
getBezierPoints=: monad define
  ctrlpts=. (/: {:"1)  _2]\ y  NB. sort ctrlpts for increasing x
  xvals=. ({: ,~ {. + +:@:i.@<.@-:@-~/) ({:"1) 0 _1{ctrlpts
  tvals=.  ((] - {.) % ({: - {.)) xvals
  xvals ,.~ ({."1 ctrlpts) evalBernstein tvals
)

NB.*drawBezier v Draws bezier curve defined by (x) on image (y)
NB. eg: (42 40 10 30 186 269 26 187;255 0 0) drawBezier myimg
NB. x is: 2-item list of boxed (controlpoints) ; (color)
drawBezier=: (1&{:: ;~ 2 ]\ [: roundint@getBezierPoints"1 (0&{::))@[ drawLines ]
```


'''Example usage:'''

```j
myimg=: 0 0 255 makeRGB 300 300
]randomctrlpts=: ,3 2 ?@$ }:$ myimg                               NB. 3 control points - quadratic
]randomctrlpts=: ,4 2 ?@$ }:$ myimg                               NB. 4 control points - cubic
myimg=: ((2 ,.~ _2]\randomctrlpts);255 0 255) drawCircles myimg   NB. draw control points
viewRGB (randomctrlpts; 255 255 0) drawBezier myimg               NB. display image with bezier line
```



## JavaScript


```javascript

  function draw() {
    var canvas = document.getElementById("container");
    context = canvas.getContext("2d");

    bezier3(20, 200, 700, 50, -300, 50, 380, 150);
//    bezier3(160, 10, 10, 40, 30, 160, 150, 110);
//    bezier3(0,149, 30,50, 120,130, 160,30, 0);
  }

  // http://rosettacode.org/wiki/Cubic_bezier_curves#C
  function bezier3(x1, y1, x2, y2, x3, y3, x4, y4) {
    var px = [], py = [];
    for (var i = 0; i <= b3Seg; i++) {
      var d = i / b3Seg;
      var a = 1 - d;
      var b = a * a;
      var c = d * d;
      a = a * b;
      b = 3 * b * d;
      c = 3 * a * c;
      d = c * d;
      px[i] = parseInt(a * x1 + b * x2 + c * x3 + d * x4);
      py[i] = parseInt(a * y1 + b * y2 + c * y3 + d * y4);
    }
    var x0 = px[0];
    var y0 = py[0];
    for (i = 1; i <= b3Seg; i++) {
      var x = px[i];
      var y = py[i];
      drawPolygon(context, [[x0, y0], [x, y]], "red", "red");
      x0 = x;
      y0 = y;
    }
  }
function drawPolygon(context, polygon, strokeStyle, fillStyle) {
  context.strokeStyle = strokeStyle;
  context.beginPath();

  context.moveTo(polygon[0][0],polygon[0][1]);
  for (i = 1; i < polygon.length; i++)
    context.lineTo(polygon[i][0],polygon[i][1]);

  context.closePath();
  context.stroke();

  if (fillStyle == undefined)
    return;
  context.fillStyle = fillStyle;
  context.fill();
}

```



## Julia

```julia
using Images

function cubicbezier!(xy::Matrix,
                      img::Matrix = fill(RGB(255.0, 255.0, 255.0), 17, 17),
                      col::ColorTypes.Color = convert(eltype(img), Gray(0.0)),
                      n::Int = 20)
    t = collect(0:n) ./ n
    M = hcat((1 .- t) .^ 3, # a
             3t .* (1 .- t) .^ 2, # b
             3t .^ 2 .* (1 .- t), # c
             t .^ 3) # d
    p = floor.(Int, M * xy)
    for i in 1:n
        drawline!(img, p[i, :]..., p[i+1, :]..., col)
    end
    return img
end

xy = [16 1; 1 4; 3 16; 15 11]
cubicbezier!(xy)
```



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

    fun cubicBezier(p1: Point, p2: Point, p3: Point, p4: Point, clr: Color, n: Int) {
        val pts = List(n + 1) { Point(0, 0) }
        for (i in 0..n) {
            val t = i.toDouble() / n
            val u = 1.0 - t
            val a = u * u * u
            val b = 3.0 * t * u * u
            val c = 3.0 * t * t * u
            val d = t * t * t
            pts[i].x = (a * p1.x + b * p2.x + c * p3.x + d * p4.x).toInt()
            pts[i].y = (a * p1.y + b * p2.y + c * p3.y + d * p4.y).toInt()
            setPixel(pts[i].x, pts[i].y, clr)
        }
        for (i in 0 until n) {
            val j = i + 1
            drawLine(pts[i].x, pts[i].y, pts[j].x, pts[j].y, clr)
        }
    }
}

fun main(args: Array<String>) {
    val width = 200
    val height = 200
    val bbs = BasicBitmapStorage(width, height)
    with (bbs) {
        fill(Color.cyan)
        val p1 = Point(0, 149)
        val p2 = Point(30, 50)
        val p3 = Point(120, 130)
        val p4 = Point(160, 30)
        cubicBezier(p1, p2, p3, p4, Color.black, 20)
        val cbFile = File("cubic_bezier.jpg")
        ImageIO.write(image, "jpg", cbFile)
    }
}
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
points= {{0, 0}, {1, 1}, {2, -1}, {3, 0}};
Graphics[{BSplineCurve[points], Green, Line[points], Red, Point[points]}]
```

[[File:MmaCubicBezier.png]]


## MATLAB

Note: Store this function in a file named "bezierCubic.mat" in the @Bitmap folder for the Bitmap class defined [[Bitmap#MATLAB|here]].

```MATLAB

function bezierCubic(obj,pixel_0,pixel_1,pixel_2,pixel_3,color,varargin)

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
    curve = (timeMinus).^3*pixel_0; %First term of polynomial
    curve = curve + (3.*time.*timeMinus.^2)*pixel_1; %second term of polynomial
    curve = curve + (3.*timeMinus.*time.^2)*pixel_2; %third term of polynomial
    curve = curve + time.^3*pixel_3; %Fourth term of polynomial

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
This will generate the image example for the PHP solution.

```MATLAB

>> img = Bitmap(200,200);
>> img.fill([255 255 255]);
>> img.bezierCubic([160 10],[10 40],[30 160],[150 110],[255 0 0],110);
>> disp(img)

```



## OCaml



```ocaml
let cubic_bezier ~img ~color
        ~p1:(_x1, _y1)
        ~p2:(_x2, _y2)
        ~p3:(_x3, _y3)
        ~p4:(_x4, _y4) =
  let x1, y1, x2, y2, x3, y3, x4, y4 =
    (float _x1, float _y1,
     float _x2, float _y2,
     float _x3, float _y3,
     float _x4, float _y4)
  in
  let bz t =
    let a = (1.0 -. t) ** 3.0
    and b = 3.0 *. t *. ((1.0 -. t) ** 2.0)
    and c = 3.0 *. (t ** 2.0) *. (1.0 -. t)
    and d = t ** 3.0
    in
    let x = a *. x1 +. b *. x2 +. c *. x3 +. d *. x4
    and y = a *. y1 +. b *. y2 +. c *. y3 +. d *. y4
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

    method cubic ( ($x1, $y1), ($x2, $y2), ($x3, $y3), ($x4, $y4), $pix, $segments = 30 ) {
        my @line-segments = map -> $t {
            my \a = (1-$t)³;
            my \b = $t  * (1-$t)² * 3;
            my \c = $t² * (1-$t)  * 3;
            my \d = $t³;
            (a*$x1 + b*$x2 + c*$x3 + d*$x4).round(1),(a*$y1 + b*$y2 + c*$y3 + d*$y4).round(1)
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

my @points = (85,390), (5,5), (580,370), (270,10);

my %seen;
my $c = 0;
for @points.permutations -> @this {
    %seen{@this.reverse.join.Str}++;
    next if %seen{@this.join.Str};
    $b.cubic( |@this, color(255-$c,127,$c+=22) );
}

@points.map: { $b.dot( $_, color(255,0,0), 3 )}

$*OUT.write: $b.P6;
```


See [https://github.com/thundergnat/rc/blob/master/img/Bezier-cubic-perl6.png example image here], (converted to a .png as .ppm format is not widely supported).


## Phix

Output similar to [[Bitmap/Bézier_curves/Cubic#Mathematica|Mathematica]]
Requires new_image() from [[Bitmap#Phix|Bitmap]], bresLine() from [[Bitmap/Bresenham's_line_algorithm#Phix|Bresenham's_line_algorithm]], write_ppm() from [[Bitmap/Write_a_PPM_file#Phix|Write_a_PPM_file]].
Included as demo\rosetta\Bitmap_BezierCubic.exw, results may be verified with demo\rosetta\viewppm.exw

```Phix
function cubic_bezier(sequence img, atom x1, atom y1, atom x2, atom y2, atom x3, atom y3, atom x4, atom y4, integer colour, integer segments)
atom t, t1, a, b, c, d
sequence pts = repeat(0,segments*2)

    for i=0 to segments*2-1 by 2 do
        t = i/segments
        t1 = 1-t
        a = power(t1,3)
        b = 3*t*power(t1,2)
        c = 3*power(t,2)*t1
        d = power(t,3)
        pts[i+1] = floor(a*x1+b*x2+c*x3+d*x4)
        pts[i+2] = floor(a*y1+b*y2+c*y3+d*y4)
    end for
    for i=1 to segments*2-2 by 2 do
        img = bresLine(img, pts[i], pts[i+1], pts[i+2], pts[i+3], colour)
    end for
    return img
end function

sequence img = new_image(300,200,black)
    img = cubic_bezier(img, 0,100, 100,0, 200,200, 300,100, white, 40)
    img = bresLine(img,0,100,100,0,green)
    img = bresLine(img,100,0,200,200,green)
    img = bresLine(img,200,200,300,100,green)
    img[1][100] = red
    img[100][1] = red
    img[200][200] = red
    img[300][100] = red
    write_ppm("Bézier.ppm",img)
```



## PHP

[[Image:Cubic bezier curve PHP.png|right]]

Outputs image to the right directly to browser or stdout.


```php
<?

$image = imagecreate(200, 200);
// The first allocated color will be the background color:
imagecolorallocate($image, 255, 255, 255);
$color = imagecolorallocate($image, 255, 0, 0);
cubicbezier($image, $color, 160, 10, 10, 40, 30, 160, 150, 110);
imagepng($image);

function cubicbezier($img, $col, $x0, $y0, $x1, $y1, $x2, $y2, $x3, $y3, $n = 20) {
	$pts = array();

	for($i = 0; $i <= $n; $i++) {
		$t = $i / $n;
		$t1 = 1 - $t;
		$a = pow($t1, 3);
		$b = 3 * $t * pow($t1, 2);
		$c = 3 * pow($t, 2) * $t1;
		$d = pow($t, 3);

		$x = round($a * $x0 + $b * $x1 + $c * $x2 + $d * $x3);
		$y = round($a * $y0 + $b * $y1 + $c * $y2 + $d * $y3);
		$pts[$i] = array($x, $y);
	}

	for($i = 0; $i < $n; $i++) {
		imageline($img, $pts[$i][0], $pts[$i][1], $pts[$i+1][0], $pts[$i+1][1], $col);
	}
}

```



## PicoLisp

This uses the 'brez' line drawing function from
[[Bitmap/Bresenham's line algorithm#PicoLisp]].

```PicoLisp
(scl 6)

(de cubicBezier (Img N X1 Y1 X2 Y2 X3 Y3 X4 Y4)
   (let (R (* N N N)  X X1  Y Y1  DX 0  DY 0)
      (for I N
         (let
            (J (- N I)
               A (*/ 1.0 J J J R)
               B (*/ 3.0 I J J R)
               C (*/ 3.0 I I J R)
               D (*/ 1.0 I I I R) )
            (brez Img
               X
               Y
               (setq DX
                  (-
                     (+ (*/ A X1 1.0) (*/ B X2 1.0) (*/ C X3 1.0) (*/ D X4 1.0))
                     X ) )
               (setq DY
                  (-
                     (+ (*/ A Y1 1.0) (*/ B Y2 1.0) (*/ C Y3 1.0) (*/ D Y4 1.0))
                     Y ) ) )
            (inc 'X DX)
            (inc 'Y DY) ) ) ) )
```

Test:

```PicoLisp
(let Img (make (do 200 (link (need 300 0))))       # Create image 300 x 200
   (cubicBezier Img 24 20 120 540 33 -225 33 285 100)
   (out "img.pbm"                                  # Write to bitmap file
      (prinl "P1")
      (prinl 300 " " 200)
      (mapc prinl Img) ) )

(call 'display "img.pbm")
```



## PureBasic


```PureBasic
Procedure cubic_bezier(img, p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y, Color, n_seg)
  Protected i
  Protected.f t, t1, a, b, c, d
  Dim pts.POINT(n_seg)

  For i = 0 To n_seg
    t = i / n_seg
    t1 = 1.0 - t
    a = Pow(t1, 3)
    b = 3.0 * t * Pow(t1, 2)
    c = 3.0 * Pow(t, 2) * t1
    d = Pow(t, 3)
    pts(i)\x = a * p1x + b * p2x + c * p3x + d * p4x
    pts(i)\y = a * p1y + b * p2y + c * p3y + d * p4y
  Next

  StartDrawing(ImageOutput(img))
    FrontColor(Color)
    For i = 0 To n_seg - 1
      BresenhamLine(pts(i)\x, pts(i)\y, pts(i + 1)\x, pts(i + 1)\y) ;this calls the implementation of a draw_line routine
    Next
  StopDrawing()
EndProcedure

Define w, h, img
w = 200: h = 200: img = 1
CreateImage(img, w, h) ;img is internal id of the image

OpenWindow(0, 0, 0, w, h,"Bezier curve, cubic", #PB_Window_SystemMenu)
cubic_bezier(1, 160,10, 10,40, 30,160, 150,110, RGB(255, 255, 255), 20)
ImageGadget(0, 0, 0, w, h, ImageID(1))

Define event
Repeat
  event = WaitWindowEvent()
Until event = #PB_Event_CloseWindow
```



## Python

Extending the example given [[Bresenham's line algorithm#Python|here]] and using the algorithm from the C solution above:

```python
def cubicbezier(self, x0, y0, x1, y1, x2, y2, x3, y3, n=20):
    pts = []
    for i in range(n+1):
        t = i / n
        a = (1. - t)**3
        b = 3. * t * (1. - t)**2
        c = 3.0 * t**2 * (1.0 - t)
        d = t**3

        x = int(a * x0 + b * x1 + c * x2 + d * x3)
        y = int(a * y0 + b * y1 + c * y2 + d * y3)
        pts.append( (x, y) )
    for i in range(n):
        self.line(pts[i][0], pts[i][1], pts[i+1][0], pts[i+1][1])
Bitmap.cubicbezier = cubicbezier

bitmap = Bitmap(17,17)
bitmap.cubicbezier(16,1, 1,4, 3,16, 15,11)
bitmap.chardisplay()


'''
The origin, 0,0; is the lower left, with x increasing to the right,
and Y increasing upwards.

The chardisplay above produces the following output :
+-----------------+
|                 |
|                 |
|                 |
|                 |
|         @@@@    |
|      @@@    @@@ |
|     @           |
|     @           |
|     @           |
|     @           |
|      @          |
|      @          |
|       @         |
|        @        |
|         @@@@    |
|             @@@@|
|                 |
+-----------------+
'''
```



## R


```R
# x, y: the x and y coordinates of the hull points
# n: the number of points in the curve.
bezierCurve <- function(x, y, n=10)
	{
	outx <- NULL
	outy <- NULL

	i <- 1
	for (t in seq(0, 1, length.out=n))
		{
		b <- bez(x, y, t)
		outx[i] <- b$x
		outy[i] <- b$y

		i <- i+1
		}

	return (list(x=outx, y=outy))
	}

bez <- function(x, y, t)
	{
	outx <- 0
	outy <- 0
	n <- length(x)-1
	for (i in 0:n)
		{
		outx <- outx + choose(n, i)*((1-t)^(n-i))*t^i*x[i+1]
		outy <- outy + choose(n, i)*((1-t)^(n-i))*t^i*y[i+1]
		}

	return (list(x=outx, y=outy))
	}

# Example usage
x <- c(4,6,4,5,6,7)
y <- 1:6
plot(x, y, "o", pch=20)
points(bezierCurve(x,y,20), type="l", col="red")
```



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

(define (bezier-points p0 p1 p2 p3)
  (for/list ([t (in-range 0.0 1.0 (/ 1.0 20))])
    (int t (int t p0 p1) (int t p2 p3))))

(define bm (make-object bitmap% 17 17))
(define dc (new bitmap-dc% [bitmap bm]))
(send dc set-smoothing 'unsmoothed)
(send dc set-pen "red" 1 'solid)
(draw-lines dc (bezier-points '(16 1) '(1 4) '(3 16) '(15 11)))
bm

```



## Ruby

Requires code from the [[Bitmap#Ruby|Bitmap]] and [[Bitmap/Bresenham's line algorithm#Ruby Bresenham's line algorithm]] tasks


```ruby
class Pixmap
  def draw_bezier_curve(points, colour)
    # ensure the points are increasing along the x-axis
    points = points.sort_by {|p| [p.x, p.y]}
    xmin = points[0].x
    xmax = points[-1].x
    increment = 2
    prev = points[0]
    ((xmin + increment) .. xmax).step(increment) do |x|
      t = 1.0 * (x - xmin) / (xmax - xmin)
      p = Pixel[x, bezier(t, points).round]
      draw_line(prev, p, colour)
      prev = p
    end
  end
end

# the generalized n-degree Bezier summation
def bezier(t, points)
  n = points.length - 1
  points.each_with_index.inject(0.0) do |sum, (point, i)|
    sum += n.choose(i) * (1-t)**(n - i) * t**i * point.y
  end
end

class Fixnum
  def choose(k)
    self.factorial / (k.factorial * (self - k).factorial)
  end
  def factorial
    (2 .. self).reduce(1, :*)
  end
end

bitmap = Pixmap.new(400, 400)
points = [
  Pixel[40,100], Pixel[100,350], Pixel[150,50],
  Pixel[150,150], Pixel[350,250], Pixel[250,250]
]
points.each {|p| bitmap.draw_circle(p, 3, RGBColour::RED)}
bitmap.draw_bezier_curve(points, RGBColour::BLUE)
```



## Tcl

This solution can be applied to any number of points.  Uses code from [[Basic_bitmap_storage#Tcl|Basic bitmap storage]] (<tt>newImage</tt>, <tt>fill</tt>), [[Bresenham's_line_algorithm#Tcl|Bresenham's line algorithm]] (<tt>drawLine</tt>), and [[Midpoint_circle_algorithm#Tcl|Midpoint circle algorithm]] (<tt>drawCircle</tt>)

```tcl
package require Tcl 8.5
package require Tk

proc drawBezier {img colour args} {
    # ensure the points are increasing along the x-axis
    set points [lsort -real -index 0 $args]

    set xmin [x [lindex $points 0]]
    set xmax [x [lindex $points end]]
    set prev [lindex $points 0]
    set increment 2
    for {set x [expr {$xmin + $increment}]} {$x <= $xmax} {incr x $increment} {
        set t [expr {1.0 * ($x - $xmin) / ($xmax - $xmin)}]
        set this [list $x [::tcl::mathfunc::round [bezier $t $points]]]
        drawLine $img $colour $prev $this
        set prev $this
    }
}

# the generalized n-degree Bezier summation
proc bezier {t points} {
    set n [expr {[llength $points] - 1}]
    for {set i 0; set sum 0.0} {$i <= $n} {incr i} {
        set sum [expr {$sum + [C $n $i] * (1-$t)**($n - $i) * $t**$i * [y [lindex $points $i]]}]
    }
    return $sum
}

proc C {n i} {expr {[ifact $n] / ([ifact $i] * [ifact [expr {$n - $i}]])}}
proc ifact n {
    for {set i $n; set sum 1} {$i >= 2} {incr i -1} {
        set sum [expr {$sum * $i}]
    }
    return $sum
}

proc x p {lindex $p 0}
proc y p {lindex $p 1}

proc newbezier {n w} {
    set size 400
    set bezier [newImage $size $size]
    fill $bezier white
    for {set i 1} {$i <= $n} {incr i} {
        set point [list [expr {int($size*rand())}] [expr {int($size*rand())}]]
        lappend points $point
        drawCircle $bezier red $point 3
    }
    puts $points

    drawBezier $bezier blue {*}$points

    $w configure -image $bezier
}

set degree 4 ;# cubic bezier -- for quadratic, use 3
label .img
button .new -command [list newbezier $degree .img] -text New
button .exit -command exit -text Exit
pack .new .img .exit -side top
```

Results in:

[[Image:Tcl_cubic_bezier.png]]

=={{header|TI-89 BASIC}}==

```ti89b
Define cubic(p1,p2,p3,p4,segs) = Prgm
  Local i,t,u,prev,pt
  0 → pt
  For i,1,segs+1
    (i-1.0)/segs → t   © Decimal to avoid slow exact arithetic
    (1-t) → u
    pt → prev
    u^3*p1 + 3t*u^2*p2 + 3t^2*u*p3 + t^3*p4 → pt
    If i>1 Then
      PxlLine floor(prev[1,1]), floor(prev[1,2]), floor(pt[1,1]), floor(pt[1,2])
    EndIf
  EndFor
EndPrgm
```



## XPL0

[[File:CubicXPL0.png|right]]

```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

proc Bezier(P0, P1, P2, P3);    \Draw cubic Bezier curve
real P0, P1, P2, P3;
def  Segments = 8;
int  I;
real S1, T, T2, T3, U, U2, U3, B, C, X, Y;
[Move(fix(P0(0)), fix(P0(1)));
S1:= 1./float(Segments);
T:= 0.;
for I:= 1 to Segments-1 do
        [T:= T+S1;
        T2:= T*T;
        T3:= T2*T;
        U:= 1.-T;
        U2:= U*U;
        U3:= U2*U;
        B:= 3.*T*U2;
        C:= 3.*T2*U;
        X:= U3*P0(0) + B*P1(0) + C*P2(0) + T3*P3(0);
        Y:= U3*P0(1) + B*P1(1) + C*P2(1) + T3*P3(1);
        Line(fix(X), fix(Y), $00FFFF);          \cyan line segments
        ];
 Line(fix(P3(0)), fix(P3(1)), $00FFFF);
Point(fix(P0(0)), fix(P0(1)), $FF0000);         \red control points
Point(fix(P1(0)), fix(P1(1)), $FF0000);
Point(fix(P2(0)), fix(P2(1)), $FF0000);
Point(fix(P3(0)), fix(P3(1)), $FF0000);
];

[SetVid($112);          \set 640x480x24 video graphics
Bezier([0., 0.], [30., 100.], [120., 20.], [160., 120.]);
if ChIn(1) then [];     \wait for keystroke
SetVid(3);              \restore normal text display
]
```



## zkl

[[File:CubicXPL0.png|right]]
Image cribbed from XPL0

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

Add this to the PPM class:

```zkl
   fcn cBezier(p0x,p0y, p1x,p1y, p2x,p2y, p3x,p3y, rgb, numPts=500){
      numPts.pump(Void,'wrap(t){ // B(t)
      	 t=t.toFloat()/numPts; t1:=(1.0 - t);
	 a:=t1*t1*t1; b:=t*t1*t1*3; c:=t1*t*t*3; d:=t*t*t;
	 x:=a*p0x + b*p1x + c*p2x + d*p3x + 0.5;
	 y:=a*p0y + b*p1y + c*p2y + d*p3y + 0.5;
	 __sSet(rgb,x,y);
      });
   }
```

Doesn't use line segments, they don't seem like an improvement.

```zkl
bitmap:=PPM(200,150,0xff|ff|ff);
bitmap.cBezier(0,149, 30,50, 120,130, 160,30, 0);
bitmap.write(File("foo.ppm","wb"));
```


