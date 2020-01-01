+++
title = "Bitmap/Midpoint circle algorithm"
description = ""
date = 2018-04-01T21:23:07Z
aliases = []
[extra]
id = 3218
[taxonomies]
categories = []
tags = []
+++

{{task|Raster graphics operations}}Using the data storage type defined [[Basic_bitmap_storage|on this page]] for raster images,
write an implementation of the '''midpoint circle algorithm'''
(also known as '''Bresenham's circle algorithm'''). <BR>
([[wp:Midpoint_circle_algorithm|definition on Wikipedia]]).


## Ada


```ada
procedure Circle
          (  Picture : in out Image;
             Center  : Point;
             Radius  : Natural;
             Color   : Pixel
          )  is
   F     : Integer := 1 - Radius;
   ddF_X : Integer := 0;
   ddF_Y : Integer := -2 * Radius;
   X     : Integer := 0;
   Y     : Integer := Radius;
begin
   Picture (Center.X, Center.Y + Radius) := Color;
   Picture (Center.X, Center.Y - Radius) := Color;
   Picture (Center.X + Radius, Center.Y) := Color;
   Picture (Center.X - Radius, Center.Y) := Color;
   while X < Y loop
      if F >= 0 then
         Y := Y - 1;
         ddF_Y := ddF_Y + 2;
         F := F + ddF_Y;
      end if;
      X := X + 1;
      ddF_X := ddF_X + 2;
      F := F + ddF_X + 1;
      Picture (Center.X + X, Center.Y + Y) := Color;
      Picture (Center.X - X, Center.Y + Y) := Color;
      Picture (Center.X + X, Center.Y - Y) := Color;
      Picture (Center.X - X, Center.Y - Y) := Color;
      Picture (Center.X + Y, Center.Y + X) := Color;
      Picture (Center.X - Y, Center.Y + X) := Color;
      Picture (Center.X + Y, Center.Y - X) := Color;
      Picture (Center.X - Y, Center.Y - X) := Color;
   end loop;
end Circle;
```

The following illustrates use:

```ada
   X : Image (1..16, 1..16);
begin
   Fill (X, White);
   Circle (X, (8, 8), 5, Black);
   Print (X);
```

{{out}}

```txt



     HHHHH
    H     H
   H       H
  H         H
  H         H
  H         H
  H         H
  H         H
   H       H
    H     H
     HHHHH




```


## ALGOL 68

{{trans|Ada}}
{{works with|ALGOL 68|Revision 1 - one minor extension to language used - PRAGMA READ, similar to C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.6 algol68g-2.6].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: prelude/Bitmap/Midpoint_circle_algorithm.a68'''
```algol68
# -*- coding: utf-8 -*- #

circle OF class image :=
          (  REF IMAGE picture,
             POINT center,
             INT radius,
             PIXEL color
          )VOID:
BEGIN
   INT f     := 1 - radius,
   POINT ddf := (0, -2 * radius),
         df := (0, radius);
   picture [x OF center, y OF center + radius] :=
   picture [x OF center, y OF center - radius] :=
   picture [x OF center + radius, y OF center] :=
   picture [x OF center - radius, y OF center] := color;
   WHILE x OF df < y OF df DO
      IF f >= 0 THEN
         y OF df -:= 1;
         y OF ddf +:= 2;
         f +:= y OF ddf
      FI;
      x OF df +:= 1;
      x OF ddf +:= 2;
      f +:= x OF ddf + 1;
      picture [x OF center + x OF df, y OF center + y OF df] :=
      picture [x OF center - x OF df, y OF center + y OF df] :=
      picture [x OF center + x OF df, y OF center - y OF df] :=
      picture [x OF center - x OF df, y OF center - y OF df] :=
      picture [x OF center + y OF df, y OF center + x OF df] :=
      picture [x OF center - y OF df, y OF center + x OF df] :=
      picture [x OF center + y OF df, y OF center - x OF df] :=
      picture [x OF center - y OF df, y OF center - x OF df] := color
   OD
END # circle #;

SKIP
```
'''File: test/Bitmap/Midpoint_circle_algorithm.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

PR READ "prelude/Bitmap.a68" PR; # c.f. [[rc:Bitmap]] #
PR READ "prelude/Bitmap/Bresenhams_line_algorithm.a68" PR; # c.f. [[rc:Bitmap/Bresenhams_line_algorithm]] #
PR READ "prelude/Bitmap/Midpoint_circle_algorithm.a68" PR;

# The following illustrates use: #

test:(
   REF IMAGE x = INIT LOC [1:16, 1:16] PIXEL;
   (fill OF class image)(x, (white OF class image));
   (circle OF class image)(x, (8, 8), 5, (black OF class image));
   (print OF class image)(x)
)
```

{{out}}

```txt

ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffff000000000000000000000000000000ffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffff000000ffffffffffffffffffffffffffffff000000ffffffffffffffffffffffffffffff
ffffffffffffffffff000000ffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffff
ffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffff
ffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffff
ffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffff
ffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffff
ffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffff
ffffffffffffffffff000000ffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffff
ffffffffffffffffffffffff000000ffffffffffffffffffffffffffffff000000ffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffff000000000000000000000000000000ffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

```


## BASIC256


```basic256
fastgraphics
clg
color red
call DrawCircle(150,100,100)
refresh
color blue
call DrawCircle(200,200,50)
refresh

	#Function DrawCircle
	#1st param = X-coord of center
	#2nd param = Y-coord of center
	#3rd param = radius
Function DrawCircle(x0,y0,radius)
	x=radius
	y=0
	decisionOver2=1-x

	while x>=y
		plot( x + x0,  y + y0)
		plot( y + x0,  x + y0)
		plot(-x + x0,  y + y0)
		plot(-y + x0,  x + y0)
		plot(-x + x0, -y + y0)
		plot(-y + x0, -x + y0)
		plot( x + x0, -y + y0)
		plot( y + x0, -x + y0)

		y++

		if decisionOver2<=0 then
			decisionOver2+=2*y+1
		else
			x--
      			decisionOver2+=2*(y-x)+1
		end if
	end while
	return 0
End Function
```

{{Out}}
[http://s16.postimg.org/ge0ndfs9h/Output.jpg http://s16.postimg.org/ge0ndfs9h/Output.jpg]


## bash


```bash
#! /bin/bash
# Based on https://en.wikipedia.org/wiki/Midpoint_circle_algorithm

function putpixel {
    echo -en "\e[$2;$1H#"
}

function drawcircle {
    x0=$1
    y0=$2
    radius=$3

    for y in $( seq $((y0-radius)) $((y0+radius)) )
    do
        echo -en "\e[${y}H"
    	for x in $( seq $((x0+radius)) )
	do
		echo -n "-"
	done
    done

    x=$((radius-1))
    y=0
    dx=1
    dy=1
    err=$((dx-(radius<<1)))

    while [ $x -ge $y ]
    do
        putpixel $(( x0 + x )) $(( y0 + y ))
        putpixel $(( x0 + y )) $(( y0 + x ))
        putpixel $(( x0 - y )) $(( y0 + x ))
        putpixel $(( x0 - x )) $(( y0 + y ))
        putpixel $(( x0 - x )) $(( y0 - y ))
        putpixel $(( x0 - y )) $(( y0 - x ))
        putpixel $(( x0 + y )) $(( y0 - x ))
        putpixel $(( x0 + x )) $(( y0 - y ))

        if [ $err -le 0 ]
        then
	    ((++y))
            ((err+=dy))
            ((dy+=2))
        fi
        if [ $err -gt 0 ]
	then
            ((--x))
            ((dx+=2))
            ((err+=dx-(radius<<1)))
        fi
    done
}

clear
drawcircle 13 13 11
echo -en "\e[H"

```

{{Out}}

```txt
------#########------
----##---------##----
---#-------------#---
--#---------------#--
-#-----------------#-
-#-----------------#-
#-------------------#
#-------------------#
#-------------------#
#-------------------#
#-------------------#
#-------------------#
#-------------------#
#-------------------#
#-------------------#
-#-----------------#-
-#-----------------#-
--#---------------#--
---#-------------#---
----##---------##----
------#########------
```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion

	%== Initializations ==%
set width=50
set height=30

set /a allowance=height+2
mode %width%,%allowance%
echo Rendering...

set "outp="
for /l %%i in (1,1,%height%) do (
	for /l %%j in (1,1,%width%) do (
		set "c[%%i][%%j]= "
	)
)

	%== Set the parameters for making circle ==%
call :DrawCircle 20 20 10
call :DrawCircle 10 30 15

	%== Output result ==%
for /l %%i in (1,1,%height%) do (
	for /l %%j in (1,1,%width%) do (
		set "outp=!outp!!c[%%i][%%j]!"
	)
)
cls
echo !outp!
pause>nul
exit /b

	%== The main function ==%
:DrawCircle
	set x0=%1
	set y0=%2
	set radius=%3

	set x=!radius!
	set y=0
	set /a decisionOver2 = 1 - !x!

	:circle_loop
	if !x! geq !y! (
		set /a "hor=x + x0","ver=y + y0"
		set "c[!hor!][!ver!]=Û"
		set /a "hor=y + x0","ver=x + y0"
		set "c[!hor!][!ver!]=Û"
		set /a "hor=-x + x0","ver=y + y0"
		set "c[!hor!][!ver!]=Û"
		set /a "hor=-y + x0","ver=x + y0"
		set "c[!hor!][!ver!]=Û"
		set /a "hor=-x + x0","ver=-y + y0"
		set "c[!hor!][!ver!]=Û"
		set /a "hor=-y + x0","ver=-x + y0"
		set "c[!hor!][!ver!]=Û"
		set /a "hor=x + x0","ver=-y + y0"
		set "c[!hor!][!ver!]=Û"
		set /a "hor=y + x0","ver=-x + y0"
		set "c[!hor!][!ver!]=Û"

		set /a y+=1
		if !decisionOver2! leq 0 (
			set /a "decisionOver2 = !decisionOver2! + (2 * y^) + 1"
		) else (
			set /a x-=1
			set /a "decisionOver2 = !decisionOver2! + 2 * (y - x^) + 1"
		)
		goto circle_loop
	)
goto :EOF
```

{{Out}}

```txt
                 █                       █
                █                         █
                █                         █
               █                           █
               █                           █
               █                           █
              █                             █
              █                             █
              █                             █
              █ ███████                     █
              ██       ██                   █
             ██          █                  █
            █ █           █                 █
           █   █           █               █
          █    █            █              █
          █    █            █              █
         █      █            █            █
         █      █            █            █
         █       █           █           █
         █        █          █          █
         █         █         █         █
         █          █        █        █
         █           ██      █      ██
          █            ███  █    ███
          █               ███████
           █               █
            █             █
             █           █
              ██       ██
                ███████

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
[[Image:circle_bbc.gif|right]]

```bbcbasic
      Width% = 200
      Height% = 200

      REM Set window size:
      VDU 23,22,Width%;Height%;8,16,16,128

      REM Draw circles:
      PROCcircle(100,100,40, 0,0,0)
      PROCcircle(100,100,80, 255,0,0)
      END

      DEF PROCcircle(cx%,cy%,r%,R%,G%,B%)
      LOCAL f%, x%, y%, ddx%, ddy%
      f% = 1 - r% : y% = r% : ddy% = - 2*r%
      PROCsetpixel(cx%, cy%+r%, R%,G%,B%)
      PROCsetpixel(cx%, cy%-r%, R%,G%,B%)
      PROCsetpixel(cx%+r%, cy%, R%,G%,B%)
      PROCsetpixel(cx%-r%, cy%, R%,G%,B%)
      WHILE x% < y%
        IF f% >= 0 THEN
          y% -= 1
          ddy% += 2
          f% += ddy%
        ENDIF
        x% += 1
        ddx% += 2
        f% += ddx% + 1
        PROCsetpixel(cx%+x%, cy%+y%, R%,G%,B%)
        PROCsetpixel(cx%-x%, cy%+y%, R%,G%,B%)
        PROCsetpixel(cx%+x%, cy%-y%, R%,G%,B%)
        PROCsetpixel(cx%-x%, cy%-y%, R%,G%,B%)
        PROCsetpixel(cx%+y%, cy%+x%, R%,G%,B%)
        PROCsetpixel(cx%-y%, cy%+x%, R%,G%,B%)
        PROCsetpixel(cx%+y%, cy%-x%, R%,G%,B%)
        PROCsetpixel(cx%-y%, cy%-x%, R%,G%,B%)
      ENDWHILE
      ENDPROC

      DEF PROCsetpixel(x%,y%,r%,g%,b%)
      COLOUR 1,r%,g%,b%
      GCOL 1
      LINE x%*2,y%*2,x%*2,y%*2
      ENDPROC
```


== {{header|C}} ==

Interface:


```c
void raster_circle(
        image img,
        unsigned int x0,
        unsigned int y0,
        unsigned int radius,
        color_component r,
        color_component g,
        color_component b );
```


Implementation:


```c
#define plot(x, y) put_pixel_clip(img, x, y, r, g, b)

void raster_circle(
        image img,
        unsigned int x0,
        unsigned int y0,
        unsigned int radius,
        color_component r,
        color_component g,
        color_component b )
{
    int f = 1 - radius;
    int ddF_x = 0;
    int ddF_y = -2 * radius;
    int x = 0;
    int y = radius;

    plot(x0, y0 + radius);
    plot(x0, y0 - radius);
    plot(x0 + radius, y0);
    plot(x0 - radius, y0);

    while(x < y)
    {
        if(f >= 0)
        {
            y--;
            ddF_y += 2;
            f += ddF_y;
        }
        x++;
        ddF_x += 2;
        f += ddF_x + 1;
        plot(x0 + x, y0 + y);
        plot(x0 - x, y0 + y);
        plot(x0 + x, y0 - y);
        plot(x0 - x, y0 - y);
        plot(x0 + y, y0 + x);
        plot(x0 - y, y0 + x);
        plot(x0 + y, y0 - x);
        plot(x0 - y, y0 - x);
    }
}
#undef plot
```


== {{header|C sharp}} ==

This extension method extends GenericImage which is very similar to [http://rosettacode.org/wiki/Bitmap#C.23 Bitmap] but instead of using a SetPixel method it uses a "Color this[int x, int y] { get; set; }" property to get and set pixels.


```c#

        /// <summary>
        /// Draws a circle.
        /// </summary>
        /// <param name="image">
        /// The destination image.
        /// </param>
        /// <param name="centerX">
        /// The x center position of the circle.
        /// </param>
        /// <param name="centerY">
        /// The y center position of the circle.
        /// </param>
        /// <param name="radius">
        /// The radius of the circle.
        /// </param>
        /// <param name="color">
        /// The color to use.
        /// </param>
        public static void DrawCircle(this GenericImage image, int centerX, int centerY, int radius, Color color)
        {
            int d = (5 - radius * 4) / 4;
            int x = 0;
            int y = radius;

            do
            {
                // ensure index is in range before setting (depends on your image implementation)
                // in this case we check if the pixel location is within the bounds of the image before setting the pixel
                if (centerX + x >= 0 && centerX + x <= image.Width - 1 && centerY + y >= 0 && centerY + y <= image.Height - 1) image[centerX + x, centerY + y] = color;
                if (centerX + x >= 0 && centerX + x <= image.Width - 1 && centerY - y >= 0 && centerY - y <= image.Height - 1) image[centerX + x, centerY - y] = color;
                if (centerX - x >= 0 && centerX - x <= image.Width - 1 && centerY + y >= 0 && centerY + y <= image.Height - 1) image[centerX - x, centerY + y] = color;
                if (centerX - x >= 0 && centerX - x <= image.Width - 1 && centerY - y >= 0 && centerY - y <= image.Height - 1) image[centerX - x, centerY - y] = color;
                if (centerX + y >= 0 && centerX + y <= image.Width - 1 && centerY + x >= 0 && centerY + x <= image.Height - 1) image[centerX + y, centerY + x] = color;
                if (centerX + y >= 0 && centerX + y <= image.Width - 1 && centerY - x >= 0 && centerY - x <= image.Height - 1) image[centerX + y, centerY - x] = color;
                if (centerX - y >= 0 && centerX - y <= image.Width - 1 && centerY + x >= 0 && centerY + x <= image.Height - 1) image[centerX - y, centerY + x] = color;
                if (centerX - y >= 0 && centerX - y <= image.Width - 1 && centerY - x >= 0 && centerY - x <= image.Height - 1) image[centerX - y, centerY - x] = color;
                if (d < 0)
                {
                    d += 2 * x + 1;
                }
                else
                {
                    d += 2 * (x - y) + 1;
                    y--;
                }
                x++;
            } while (x <= y);
        }

```



## Clojure

Based upon the Common Lisp version.

```clojure
(defn draw-circle [draw-function x0 y0 radius]
  (letfn [(put [x y m]
            (let [x+ (+ x0 x)
                  x- (- x0 x)
                  y+ (+ y0 y)
                  y- (- y0 y)
                  x0y+ (+ x0 y)
                  x0y- (- x0 y)
                  xy0+ (+ y0 x)
                  xy0- (- y0 x)]
              (draw-function x+ y+)
              (draw-function x+ y-)
              (draw-function x- y+)
              (draw-function x- y-)
              (draw-function x0y+ xy0+)
              (draw-function x0y+ xy0-)
              (draw-function x0y- xy0+)
              (draw-function x0y- xy0-)
              (let [[y m] (if (pos? m)
                            [(dec y) (- m (* 8 y))]
                            [y m])]
                (when (<= x y)
                  (put (inc x)
                       y
                       (+ m 4 (* 8 x)))))))]
    (put 0 radius (- 5 (* 4 radius)))))
```


```clojure
(let [circle-points (atom [])]
    (letfn [(draw-fn [x y]
              (swap! circle-points #(conj % [x y])))]
      (draw-circle draw-fn 10 10 7))
    (let [empty-grid (vec (repeat 20 (vec (repeat 20 " "))))
          grid       (reduce (fn [grid xy] (assoc-in grid xy "x"))
                             empty-grid
                             @circle-points)]
      (doseq [line grid]
        (println (clojure.string/join line)))))
```


```txt



       xxxxxxx
     xx       xx
    x           x
    x           x
   x             x
   x             x
   x             x
   x             x
   x             x
   x             x
   x             x
    x           x
    x           x
     xx       xx
       xxxxxxx


nil

```


## Common Lisp

Based upon the OCaml version.


```lisp
(defun draw-circle (draw-function x0 y0 radius)
  (labels ((foo (x y)
             (funcall draw-function x y))
           (put (x y m)
             (let ((x+ (+ x0 x))
                   (x- (- x0 x))
                   (y+ (+ y0 y))
                   (y- (- y0 y))
                   (x0y+ (+ x0 y))
                   (x0y- (- x0 y))
                   (xy0+ (+ y0 x))
                   (xy0- (- y0 x)))
               (foo x+ y+)
               (foo x+ y-)
               (foo x- y+)
               (foo x- y-)
               (foo x0y+ xy0+)
               (foo x0y+ xy0-)
               (foo x0y- xy0+)
               (foo x0y- xy0-)
               (multiple-value-bind (y m) (if (plusp m)
                                              (values (1- y) (- m (* 8 y)))
                                              (values y m))
                 (when (<= x y)
                   (put (1+ x)
                        y
                        (+ m 4 (* 8 x))))))))
    (put 0 radius (- 5 (* 4 radius)))
    (values)))
```


```lisp
CL-USER> (let ((buffer (make-array '(30 30)
                                    :element-type 'bit)))
           (draw-circle (lambda (x y)
                          (setf (bit buffer x y) 1)) 15 15 10)
           buffer)
```


```txt
;; edited for your convenience
((                                                           )
 (                        1 1 1 1 1 1 1                      )
 (                  1 1 1               1 1 1                )
 (                1                           1              )
 (              1                               1            )
 (            1                                   1          )
 (            1                                   1          )
 (            1                                   1          )
 (          1                                       1        )
 (          1                                       1        )
 (          1                                       1        )
 (          1                                       1        )
 (          1                                       1        )
 (          1                                       1        )
 (          1                                       1        )
 (            1                                   1          )
 (            1                                   1          )
 (            1                                   1          )
 (              1                               1            )
 (                1                           1              )
 (                  1 1 1               1 1 1                )
 (                        1 1 1 1 1 1 1                      )
 (                                                           ))

```


## D

Uses the bitmap module from the Bitmap Task.

```d
import bitmap: Image, RGB;

void circle(Color)(Image!Color img, in int x0, in int y0,
                   in int radius, in Color color)
pure nothrow @nogc @safe {
    int f = 1 - radius;
    int ddfX = 1;
    int ddfY = -2 * radius;
    int x = 0;
    int y = radius;
    img[x0, y0 + radius] = color;
    img[x0, y0 - radius] = color;
    img[x0 + radius, y0] = color;
    img[x0 - radius, y0] = color;

    while (x < y) {
        if (f >= 0) {
            y--;
            ddfY += 2;
            f += ddfY;
        }
        x++;
        ddfX += 2;
        f += ddfX;
        img[x0 + x, y0 + y] = color;
        img[x0 - x, y0 + y] = color;
        img[x0 + x, y0 - y] = color;
        img[x0 - x, y0 - y] = color;
        img[x0 + y, y0 + x] = color;
        img[x0 - y, y0 + x] = color;
        img[x0 + y, y0 - x] = color;
        img[x0 - y, y0 - x] = color;
    }
}

void main() @safe {
    auto img = new Image!RGB(25, 25);
    img.clear(RGB.white);
    circle(img, 12, 12, 12, RGB.black);
    img.textualShow;
}
```

{{out}}

```txt
.........#######.........
.......##.......##.......
.....##...........##.....
....#...............#....
...#.................#...
..#...................#..
..#...................#..
.#.....................#.
.#.....................#.
#.......................#
#.......................#
#.......................#
#.......................#
#.......................#
#.......................#
#.......................#
.#.....................#.
.#.....................#.
..#...................#..
..#...................#..
...#.................#...
....#...............#....
.....##...........##.....
.......##.......##.......
.........#######.........
```



## ERRE


```ERRE
PROGRAM BCircle

!$INCLUDE="PC.LIB"

PROCEDURE BCircle(cx%,cy%,r%)
      local f%,x%,y%,ddx%,ddy%
      f%=1-r%  y%=r%  ddy%=-2*r%
      PSET(cx%,cy%+r%,1)
      PSET(cx%,cy%-r%,1)
      PSET(cx%+r%,cy%,1)
      PSET(cx%-r%,cy%,1)
      WHILE x%<y% DO
        IF f%>=0 THEN
          y%=y%-1
          ddy%=ddy%+2
          f%=f%+ddy%
        END IF
        x%=x%+1
        ddx%=ddx%+2
        f%=f%+ddx%+1
        PSET(cx%+x%,cy%+y%,1)
        PSET(cx%-x%,cy%+y%,1)
        PSET(cx%+x%,cy%-y%,1)
        PSET(cx%-x%,cy%-y%,1)
        PSET(cx%+y%,cy%+x%,1)
        PSET(cx%-y%,cy%+x%,1)
        PSET(cx%+y%,cy%-x%,1)
        PSET(cx%-y%,cy%-x%,1)
      END WHILE
END PROCEDURE

BEGIN
      SCREEN(1)
      ! Draw circles
      BCircle(100,100,40)
      BCircle(100,100,80)
END PROGRAM

```



## FBSL

'''Using pure FBSL's built-in graphics functions:'''

```qbasic
#DEFINE WM_LBUTTONDOWN 513
#DEFINE WM_CLOSE 16

FBSLSETTEXT(ME, "Bresenham Circle") ' Set form caption
FBSLSETFORMCOLOR(ME, RGB(0, 255, 255)) ' Cyan: persistent background color
FBSL.GETDC(ME) ' Use volatile FBSL.GETDC below to avoid extra assignments

RESIZE(ME, 0, 0, 220, 220)
CENTER(ME)
SHOW(ME)

DIM Breadth AS INTEGER, Height AS INTEGER
FBSL.GETCLIENTRECT(ME, 0, 0, Breadth, Height)

BEGIN EVENTS ' Main message loop
	SELECT CASE CBMSG
		CASE WM_LBUTTONDOWN: MidpointCircle() ' Draw
		CASE WM_CLOSE: FBSL.RELEASEDC(ME, FBSL.GETDC) ' Clean up
	END SELECT
END EVENTS

SUB MidpointCircle()
	BresenhamCircle(Breadth \ 2, Height \ 2, 80, &HFF) ' Red: Windows stores colors in BGR order
	BresenhamCircle(Breadth \ 2, Height \ 2, 40, 0) ' Black

	SUB BresenhamCircle(cx, cy, radius, colour)
		DIM x = 0, y = radius, f = 1 - radius, dx = 0, dy = -2 * radius

		PSET(FBSL.GETDC, cx, cy + radius, colour)(FBSL.GETDC, cx, cy - radius, colour)
		PSET(FBSL.GETDC, cx + radius, cy, colour)(FBSL.GETDC, cx - radius, cy, colour)

		WHILE x < y
			IF f >= 0 THEN: DECR(y): INCR(dy, 2)(f, dy): END IF ' Try also "IF f THEN" :)
			INCR(x)(dx, 2)(f, dx + 1)
			PSET(FBSL.GETDC, cx + x, cy + y, colour)(FBSL.GETDC, cx - x, cy + y, colour)
			PSET(FBSL.GETDC, cx + x, cy - y, colour)(FBSL.GETDC, cx - x, cy - y, colour)
			PSET(FBSL.GETDC, cx + y, cy + x, colour)(FBSL.GETDC, cx - y, cy + x, colour)
			PSET(FBSL.GETDC, cx + y, cy - x, colour)(FBSL.GETDC, cx - y, cy - x, colour)
		WEND
	END SUB
END SUB
```

'''Ouptut:'''   [[File:FBSLMidpoint.PNG]]


## Forth


```forth
: circle { x y r color bmp -- }
  1 r -  0 r 2* negate  0 r  { f ddx ddy dx dy }
  color x     y r + bmp b!
  color x     y r - bmp b!
  color x r + y     bmp b!
  color x r - y     bmp b!
  begin dx dy < while
    f 0< 0= if
      dy  1-      to dy
      ddy 2 + dup to ddy
      f +         to f
    then
    dx 1+       to dx
    ddx 2 + dup to ddx
    f 1+ +      to f
    color x dx + y dy + bmp b!
    color x dx - y dy + bmp b!
    color x dx + y dy - bmp b!
    color x dx - y dy - bmp b!
    color x dy + y dx + bmp b!
    color x dy - y dx + bmp b!
    color x dy + y dx - bmp b!
    color x dy - y dx - bmp b!
  repeat ;

12 12 bitmap value test
0 test bfill
6 6 5 blue test circle
test bshow cr
```



## Fortran


This code should be inside <tt>RCImagePrimitive</tt> (see [[Bresenham's line algorithm#Fortran|here]]). The private subroutine <code>draw_circle_toch</code>, which writes to a ''channel'', is used by both <code>draw_circle_rgb</code> and <code>draw_circle_sc</code> and the interface allows to use <code>draw_circle</code> with ''[[Basic bitmap storage#Fortran|rgb]]'' images and [[Grayscale image#Fortran|grayscale images]].


```fortran
interface draw_circle
   module procedure draw_circle_sc, draw_circle_rgb
end interface

private :: plot, draw_circle_toch
```



```fortran
subroutine plot(ch, p, v)
  integer, dimension(:,:), intent(out) :: ch
  type(point), intent(in) :: p
  integer, intent(in) :: v

  integer :: cx, cy
  ! I've kept the default 1-based array, but top-left corner pixel
  ! is labelled as (0,0).
  cx = p%x + 1
  cy = p%y + 1

  if ( (cx > 0) .and. (cx <= ubound(ch,1)) .and. &
       (cy > 0) .and. (cy <= ubound(ch,2)) ) then
     ch(cx,cy) = v
  end if
end subroutine plot

subroutine draw_circle_toch(ch, c, radius, v)
  integer, dimension(:,:), intent(out) :: ch
  type(point), intent(in) :: c
  integer, intent(in) :: radius, v

  integer :: f, ddf_x, ddf_y, x, y

  f = 1 - radius
  ddf_x = 0
  ddf_y = -2 * radius
  x = 0
  y = radius

  call plot(ch, point(c%x, c%y + radius), v)
  call plot(ch, point(c%x, c%y - radius), v)
  call plot(ch, point(c%x + radius, c%y), v)
  call plot(ch, point(c%x - radius, c%y), v)

  do while ( x < y )
     if ( f >= 0 ) then
        y = y - 1
        ddf_y = ddf_y + 2
        f = f + ddf_y
     end if
     x = x + 1
     ddf_x = ddf_x + 2
     f = f + ddf_x + 1
     call plot(ch, point(c%x + x, c%y + y), v)
     call plot(ch, point(c%x - x, c%y + y), v)
     call plot(ch, point(c%x + x, c%y - y), v)
     call plot(ch, point(c%x - x, c%y - y), v)
     call plot(ch, point(c%x + y, c%y + x), v)
     call plot(ch, point(c%x - y, c%y + x), v)
     call plot(ch, point(c%x + y, c%y - x), v)
     call plot(ch, point(c%x - y, c%y - x), v)
  end do

end subroutine draw_circle_toch

subroutine draw_circle_rgb(img, c, radius, color)
  type(rgbimage), intent(out) :: img
  type(point), intent(in) :: c
  integer, intent(in) :: radius
  type(rgb), intent(in) :: color

  call draw_circle_toch(img%red, c, radius, color%red)
  call draw_circle_toch(img%green, c, radius, color%green)
  call draw_circle_toch(img%blue, c, radius, color%blue)
end subroutine draw_circle_rgb

subroutine draw_circle_sc(img, c, radius, lum)
  type(scimage), intent(out) :: img
  type(point), intent(in) :: c
  integer, intent(in) :: radius, lum

  call draw_circle_toch(img%channel, c, radius, lum)
end subroutine draw_circle_sc
```


## FreeBASIC


```FreeBASIC
' version 15-10-2016
' compile with: fbc -s gui

' Variant with Integer-Based Arithmetic from Wikipedia page:
' Midpoint circle algorithm
Sub circle_(x0 As Integer, y0 As Integer , radius As Integer, Col As Integer)

  Dim As Integer x = radius
  Dim As Integer y
  ' Decision criterion divided by 2 evaluated at x=r, y=0
  Dim As Integer decisionOver2 = 1 - x

  While(x >= y)
    PSet(x0 + x, y0 + y), col
    PSet(x0 - x, y0 + y), col
    PSet(x0 + x, y0 - y), col
    PSet(x0 - x, y0 - y), col
    PSet(x0 + y, y0 + x), col
    PSet(x0 - y, y0 + x), col
    PSet(x0 + y, y0 - x), col
    PSet(x0 - y, y0 - x), col
    y = y +1
    If decisionOver2 <= 0 Then
      decisionOver2 += y * 2 +1 ' Change in decision criterion for y -> y +1
    Else
      x = x -1
      decisionOver2 += (y - x) * 2 +1  '  Change for y -> y +1, x -> x -1
    End If
  Wend

End Sub

' ------=< MAIN >=------

ScreenRes 600, 600, 32
Dim As Integer w, h, depth
Randomize Timer

ScreenInfo w, h

For i As Integer = 1 To 10
  circle_(Rnd * w,  Rnd * h , Rnd * 200 , Int(Rnd *&hFFFFFF))
Next


'save screen to BMP file
BSave "Name.BMP", 0


' empty keyboard buffer
While Inkey <> "" : Wend
WindowTitle "hit any key to end program"
Sleep
End
```



## Go

This produces identical results to the C code in the WP article, but with more compact code.

```go
package raster

// Circle plots a circle with center x, y and radius r.
// Limiting behavior:
// r < 0 plots no pixels.
// r = 0 plots a single pixel at x, y.
// r = 1 plots four pixels in a diamond shape around the center pixel at x, y.
func (b *Bitmap) Circle(x, y, r int, p Pixel) {
    if r < 0 {
        return
    }
    // Bresenham algorithm
    x1, y1, err := -r, 0, 2-2*r
    for {
        b.SetPx(x-x1, y+y1, p)
        b.SetPx(x-y1, y-x1, p)
        b.SetPx(x+x1, y-y1, p)
        b.SetPx(x+y1, y+x1, p)
        r = err
        if r > x1 {
            x1++
            err += x1*2 + 1
        }
        if r <= y1 {
            y1++
            err += y1*2 + 1
        }
        if x1 >= 0 {
            break
        }
    }
}

func (b *Bitmap) CircleRgb(x, y, r int, c Rgb) {
    b.Circle(x, y, r, c.Pixel())
}
```

Demonstration program:
<lang>package main

// Files required to build supporting package raster are found in:
// * This task (immediately above)
// * Bitmap
// * Write a PPM file

import (
    "raster"
    "fmt"
)

func main() {
    b := raster.NewBitmap(400, 300)
    b.FillRgb(0xffdf20) // yellow
    // large circle, demonstrating clipping to image boundaries
    b.CircleRgb(300, 249, 200, 0xff2020) // red
    if err := b.WritePpmFile("circle.ppm"); err != nil {
        fmt.Println(err)
    }
}
```



## Haskell

The basic algorithm can be implemented generically.

```haskell
module Circle where

import Data.List

type Point = (Int, Int)

-- Takes the center of the circle and radius, and returns the circle points
generateCirclePoints :: Point -> Int -> [Point]
generateCirclePoints (x0, y0) radius
  -- Four initial points, plus the generated points
  = (x0, y0 + radius) : (x0, y0 - radius) : (x0 + radius, y0) : (x0 - radius, y0) : points
    where
      -- Creates the (x, y) octet offsets, then maps them to absolute points in all octets.
      points = concatMap generatePoints $ unfoldr step initialValues
      generatePoints (x, y)
        = [(xop x0 x', yop y0 y') | (x', y') <- [(x, y), (y, x)], xop <- [(+), (-)], yop <- [(+), (-)]]

      -- The initial values for the loop
      initialValues = (1 - radius, 1, (-2) * radius, 0, radius)

      -- One step of the loop. The loop itself stops at Nothing.
      step (f, ddf_x, ddf_y, x, y) | x >= y = Nothing
                                   | otherwise = Just ((x', y'), (f', ddf_x', ddf_y', x', y'))
                                     where
                                       (f', ddf_y', y') | f >= 0 = (f + ddf_y' + ddf_x', ddf_y + 2, y - 1)
                                                        | otherwise = (f + ddf_x, ddf_y, y)
                                       ddf_x' = ddf_x + 2
                                       x' = x + 1

```

An example using regular 2d arrays of characters to represent a bitmap:

```haskell
module CircleArrayExample where

import Circle

-- A surface is just a 2d array of characters for the purposes of this example
type Colour = Char
type Surface = Array (Int, Int) Colour

-- Returns a surface of the given width and height filled with the colour
blankSurface :: Int -> Int -> Colour -> Surface
blankSurface width height filler = listArray bounds (repeat filler)
  where
    bounds = ((0, 0), (width - 1, height - 1))

-- Generic plotting function. Plots points onto a surface with the given colour.
plotPoints :: Surface -> Colour -> [Point] -> Surface
plotPoints surface colour points = surface // zip points (repeat colour)

-- Draws a circle of the given colour on the surface given a center and radius
drawCircle :: Surface -> Colour -> Point -> Int -> Surface
drawCircle surface colour center radius
  = plotPoints surface colour (generateCirclePoints center radius)

-- Converts a surface to a string
showSurface image = unlines [[image ! (x, y) | x <- xRange] | y <- yRange]
  where
    ((xLow, yLow), (xHigh, yHigh)) = bounds image
    (xRange, yRange) = ([xLow..xHigh], [yLow..yHigh])

-- Converts a surface to a string and prints it
printSurface = putStrLn . showSurface

```

Using the Image type from the Bitmap module defined [[Basic_bitmap_storage|here]]:

```haskell
module CircleBitmapExample where

import Circle
import Bitmap
import Control.Monad.ST

drawCircle :: (Color c) => Image s c -> c -> Point -> Int -> ST s (Image s c)
drawCircle image colour center radius = do
  let pixels = map Pixel (generateCirclePoints center radius)
  forM_ pixels $ \pixel -> setPix image pixel colour
  return image

```



## J

'''Solution:'''

Using definitions from [[Basic bitmap storage#J|Basic bitmap storage]].  (Note that viewRGB is at the bottom of the entry - separate from the rest of the definitions.)

```j
NB.*getBresenhamCircle v Returns points for a circle given center and radius
NB. y is: y0 x0 radius
getBresenhamCircle=: monad define
  'y0 x0 radius'=. y
  x=. 0
  y=. radius
  f=. -. radius
  pts=. 0 2$0
  while. x <: y do.
    pts=. pts , y , x
    if. f >: 0 do.
      y=. <:y
      f=. f + _2 * y
    end.
    x=. >:x
    f =. f + >: 2 * x
  end.
  offsets=. (,|."1) (1 _1 {~ #: i.4) *"1"1 _ pts
  ~.,/ (y0,x0) +"1 offsets
)

NB.*drawCircles v Draws circle(s) (x) on image (y)
NB. x is: 2-item list of boxed (y0 x0 radius) ; (color)
drawCircles=: (1&{:: ;~ [: ; [: <@getBresenhamCircle"1 (0&{::))@[ setPixels ]
```


'''Example usage:'''

```j
myimg=: 0 255 0 makeRGB 25 25                              NB. 25 by 25 green image
myimg=: (12 12 12 ; 255 0 0) drawCircles myimg              NB. draw red circle with radius 12
viewRGB ((12 12 9 ,: 12 12 6) ; 0 0 255) drawCircles myimg  NB. draw two more concentric circles
```


== {{header|Java}} ==


```java

import java.awt.Color;

public class MidPointCircle {
	private BasicBitmapStorage image;

	public MidPointCircle(final int imageWidth, final int imageHeight) {
		this.image = new BasicBitmapStorage(imageWidth, imageHeight);
	}

	private void drawCircle(final int centerX, final int centerY, final int radius) {
		int d = (5 - r * 4)/4;
		int x = 0;
		int y = radius;
		Color circleColor = Color.white;

		do {
			image.setPixel(centerX + x, centerY + y, circleColor);
			image.setPixel(centerX + x, centerY - y, circleColor);
			image.setPixel(centerX - x, centerY + y, circleColor);
			image.setPixel(centerX - x, centerY - y, circleColor);
			image.setPixel(centerX + y, centerY + x, circleColor);
			image.setPixel(centerX + y, centerY - x, circleColor);
			image.setPixel(centerX - y, centerY + x, circleColor);
			image.setPixel(centerX - y, centerY - x, circleColor);
			if (d < 0) {
				d += 2 * x + 1;
			} else {
				d += 2 * (x - y) + 1;
				y--;
			}
			x++;
		} while (x <= y);

	}
}

```



## Julia

{{works with|Julia|0.6}}

```julia
function drawcircle!(img::Matrix{T}, col::T, x0::Int, y0::Int, radius::Int) where T
    x = radius - 1
    y = 0
    δx = δy = 1
    er = δx - (radius << 1)

    s = x + y
    while x ≥ y
        for opx in (+, -), opy in (+, -), el in (x, y)
            @inbounds img[opx(x0, el) + 1, opy(y0, s - el) + 1] = col
        end
        if er ≤ 0
            y  += 1
            er += δy
            δy += 2
        end
        if er > 0
            x  -= 1
            δx += 2
            er += (-radius << 1) + δx
        end
        s = x + y
    end
    return img
end

# Test
using Images

img = fill(Gray(255.0), 25, 25);
drawcircle!(img, Gray(0.0), 12, 12, 12)
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.4-3

import java.awt.Color
import java.awt.Graphics
import java.awt.image.BufferedImage
import javax.swing.JOptionPane
import javax.swing.JLabel
import javax.swing.ImageIcon

class BasicBitmapStorage(width: Int, height: Int) {
    val image = BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    fun fill(c: Color) {
        val g = image.graphics
        g.color = c
        g.fillRect(0, 0, image.width, image.height)
    }

    fun setPixel(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB())

    fun getPixel(x: Int, y: Int) = Color(image.getRGB(x, y))
}

fun drawCircle(bbs: BasicBitmapStorage, centerX: Int, centerY: Int, radius: Int, circleColor: Color) {
    var d = (5 - radius * 4) / 4
    var x = 0
    var y = radius

    do {
        with(bbs) {
            setPixel(centerX + x, centerY + y, circleColor)
            setPixel(centerX + x, centerY - y, circleColor)
            setPixel(centerX - x, centerY + y, circleColor)
            setPixel(centerX - x, centerY - y, circleColor)
            setPixel(centerX + y, centerY + x, circleColor)
            setPixel(centerX + y, centerY - x, circleColor)
            setPixel(centerX - y, centerY + x, circleColor)
            setPixel(centerX - y, centerY - x, circleColor)
        }
        if (d < 0) {
            d += 2 * x + 1
        }
        else {
            d += 2 * (x - y) + 1
            y--
        }
        x++
    }
    while (x <= y)
}

fun main(args: Array<String>) {
    val bbs = BasicBitmapStorage(400, 400)
    bbs.fill(Color.pink)
    drawCircle(bbs, 200, 200, 100, Color.black)
    drawCircle(bbs, 200, 200,  50, Color.white)
    val label = JLabel(ImageIcon(bbs.image))
    val title = "Bresenham's circle algorithm"
    JOptionPane.showMessageDialog(null, label, title, JOptionPane.PLAIN_MESSAGE)
}
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
SetAttributes[drawcircle, HoldFirst];
drawcircle[img_, {x0_, y0_}, r_, color_: White] :=
 Module[{f = 1 - r, ddfx = 1, ddfy = -2 r, x = 0, y = r,
   pixels = {{0, r}, {0, -r}, {r, 0}, {-r, 0}}},
  While[x < y,
   If[f >= 0, y--; ddfy += 2; f += ddfy];
   x++; ddfx += 2; f += ddfx;
   pixels = Join[pixels, {{x, y}, {x, -y}, {-x, y}, {-x, -y},
      {y, x}, {y, -x}, {-y, x}, {-y, -x}}]];
  img = ReplacePixelValue[img, {x0, y0} + # -> color & /@ pixels]]
```

Example usage（it will draw a circle on Lena's face.）:

```mathematica
img = ExampleData[{"TestImage", "Lena"}];
drawcircle[img, {250, 250}, 100]
```


=={{header|Modula-3}}==

```modula3
INTERFACE Circle;

IMPORT Bitmap;

PROCEDURE Draw(
  img: Bitmap.T;
  center: Bitmap.Point;
  radius: CARDINAL;
  color: Bitmap.Pixel);

END Circle.
```


```modula3
MODULE Circle;

IMPORT Bitmap;

PROCEDURE Draw(
  img: Bitmap.T;
  center: Bitmap.Point;
  radius: CARDINAL;
  color: Bitmap.Pixel) =
  VAR f := 1 - radius;
      ddfx := 0;
      ddfy := - 2 * radius;
      x := 0;
      y := radius;
  BEGIN
    Bitmap.SetPixel(img, Bitmap.Point{center.x, center.y + radius}, color);
    Bitmap.SetPixel(img, Bitmap.Point{center.x, center.y - radius}, color);
    Bitmap.SetPixel(img, Bitmap.Point{center.x + radius, center.y}, color);
    Bitmap.SetPixel(img, Bitmap.Point{center.x - radius, center.y}, color);
    WHILE x < y DO
      IF f >= 0 THEN
        y := y - 1;
        ddfy := ddfy + 2;
        f := f + ddfy;
      END;
      x := x + 1;
      ddfx := ddfx + 2;
      f := f + ddfx + 1;
      Bitmap.SetPixel(img, Bitmap.Point{center.x + x, center.y + y}, color);
      Bitmap.SetPixel(img, Bitmap.Point{center.x - x, center.y + y}, color);
      Bitmap.SetPixel(img, Bitmap.Point{center.x + x, center.y - y}, color);
      Bitmap.SetPixel(img, Bitmap.Point{center.x - x, center.y - y}, color);
      Bitmap.SetPixel(img, Bitmap.Point{center.x + y, center.y + x}, color);
      Bitmap.SetPixel(img, Bitmap.Point{center.x - y, center.y + x}, color);
      Bitmap.SetPixel(img, Bitmap.Point{center.x + y, center.y - x}, color);
      Bitmap.SetPixel(img, Bitmap.Point{center.x - y, center.y - x}, color);
    END;
  END Draw;

BEGIN
END Circle.
```


Example (outputs a [[Write_ppm_file | PPM]] image):

```modula3
MODULE Main;

IMPORT Circle, Bitmap, PPM;

VAR testpic: Bitmap.T;

BEGIN
  testpic := Bitmap.NewImage(32, 32);
  Bitmap.Fill(testpic, Bitmap.White);
  Circle.Draw(testpic, Bitmap.Point{16, 16}, 10, Bitmap.Black);
  PPM.Create("testpic.ppm", testpic);
END Main.
```



## Perl 6

{{trans|C}}
We'll augment the Pixel and Bitmap classes from the [[Bitmap#Perl_6|Bitmap]] task.


```perl6
use MONKEY-TYPING;

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
}

augment class Pixel { method Str { "$.R $.G $.B" } }
augment class Bitmap {
    method P3 {
        join "\n", «P3 "$.width $.height" 255»,
        do for ^$.height { join ' ', @.data[]»[$_] }
    }
    method raster-circle ( $x0, $y0, $r, Pixel $value ) {
        my $f = 1 - $r;
        my $ddF_x = 0;
        my $ddF_y = -2 * $r;
        my ($x, $y) = 0, $r;
        self.set-pixel($x0, $y0 + $r, $value);
        self.set-pixel($x0, $y0 - $r, $value);
        self.set-pixel($x0 + $r, $y0, $value);
        self.set-pixel($x0 - $r, $y0, $value);
        while $x < $y {
            if $f >= 0 {
                $y--;
                $ddF_y += 2;
                $f += $ddF_y;
            }
            $x++;
            $ddF_x += 2;
            $f += $ddF_x + 1;
            self.set-pixel($x0 + $x, $y0 + $y, $value);
            self.set-pixel($x0 - $x, $y0 + $y, $value);
            self.set-pixel($x0 + $x, $y0 - $y, $value);
            self.set-pixel($x0 - $x, $y0 - $y, $value);
            self.set-pixel($x0 + $y, $y0 + $x, $value);
            self.set-pixel($x0 - $y, $y0 + $x, $value);
            self.set-pixel($x0 + $y, $y0 - $x, $value);
            self.set-pixel($x0 - $y, $y0 - $x, $value);
        }
    }
}
```


== {{Header|OCaml}} ==


```ocaml
let raster_circle ~img ~color ~c:(x0, y0) ~r =
  let plot = put_pixel img color in
  let x = 0
  and y = r
  and m = 5 - 4 * r
  in
  let rec loop x y m =
    plot (x0 + x) (y0 + y);
    plot (x0 + y) (y0 + x);
    plot (x0 - x) (y0 + y);
    plot (x0 - y) (y0 + x);
    plot (x0 + x) (y0 - y);
    plot (x0 + y) (y0 - x);
    plot (x0 - x) (y0 - y);
    plot (x0 - y) (y0 - x);
    let y, m =
      if m > 0
      then (y - 1), (m - 8 * y)
      else y, m
    in
    if x <= y then
      let x = x + 1 in
      let m = m + 8 * x + 4 in
      loop x y m
  in
  loop x y m
;;
```



## Phix

{{Trans|Go}}
Requires new_image() from [[Bitmap#Phix|Bitmap]], write_ppm() from [[Bitmap/Write_a_PPM_file#Phix|Write_a_PPM_file]].
Included as demo\rosetta\Bitmap_Circle.exw, results may be verified with demo\rosetta\viewppm.exw

```Phix
constant red = 0xff2020,
         yellow = 0xffdf20

function SetPx(sequence img, atom x, atom y, integer colour)
    if x>=1 and x<=length(img)
    and y>=1 and y<=length(img[x]) then
        img[x][y] = colour
    end if
    return img
end function

function Circle(sequence img, atom x, atom y, atom r, integer colour)
atom x1 = -r,
     y1 = 0,
     err = 2-2*r
    if r>=0 then
        -- Bresenham algorithm
        while 1 do
            img = SetPx(img, x-x1, y+y1, colour)
            img = SetPx(img, x-y1, y-x1, colour)
            img = SetPx(img, x+x1, y-y1, colour)
            img = SetPx(img, x+y1, y+x1, colour)
            r = err
            if r>x1 then
                x1 += 1
                err += x1*2 + 1
            end if
            if r<=y1 then
                y1 += 1
                err += y1*2 + 1
            end if
            if x1>=0 then exit end if
        end while
    end if
    return img
end function

sequence img = new_image(400,300,yellow)
    img = Circle(img, 200, 150, 100, red)
    write_ppm("Circle.ppm",img)
```



## PicoLisp


```PicoLisp
(de midPtCircle (Img CX CY Rad)
   (let (F (- 1 Rad)  DdFx 0  DdFy (* -2 Rad)  X 0  Y Rad)
      (set (nth Img (+ CY Rad) CX) 1)
      (set (nth Img (- CY Rad) CX) 1)
      (set (nth Img CY (+ CX Rad)) 1)
      (set (nth Img CY (- CX Rad)) 1)
      (while (> Y X)
         (when (ge0 F)
            (dec 'Y)
            (inc 'F (inc 'DdFy 2)) )
         (inc 'X)
         (inc 'F (inc (inc 'DdFx 2)))
         (set (nth Img (+ CY Y) (+ CX X)) 1)
         (set (nth Img (+ CY Y) (- CX X)) 1)
         (set (nth Img (- CY Y) (+ CX X)) 1)
         (set (nth Img (- CY Y) (- CX X)) 1)
         (set (nth Img (+ CY X) (+ CX Y)) 1)
         (set (nth Img (+ CY X) (- CX Y)) 1)
         (set (nth Img (- CY X) (+ CX Y)) 1)
         (set (nth Img (- CY X) (- CX Y)) 1) ) ) )

(let Img (make (do 120 (link (need 120 0))))       # Create image 120 x 120
   (midPtCircle Img 60 60 50)                      # Draw circle
   (out "img.pbm"                                  # Write to bitmap file
      (prinl "P1")
      (prinl 120 " " 120)
      (mapc prinl Img) ) )
```



## PL/I


```PL/I

/* Plot three circles. */

CIRCLE: PROCEDURE OPTIONS (MAIN);
   declare image (-20:20, -20:20) character (1);
   declare j fixed binary;

   image = '.';
   image(0,*) = '-';
   image(*,0) = '|';
   image(0,0) = '+';

   CALL DRAW_CIRCLE (0, 0, 11);
   CALL DRAW_CIRCLE (0, 0, 8);
   CALL DRAW_CIRCLE (0, 0, 19);

   do j = hbound(image,1) to lbound(image,1) by -1;
      put skip edit (image(j,*)) (a(1));
   end;

draw_circle: procedure (x0, y0, radius);       /* 14 May 2010. */
   declare ( x0, y0, radius ) fixed binary;
   declare ( ddfx, ddfy, x, y, f ) fixed binary;
   declare debug bit (1) aligned static initial ('0'b);

   f    = 1-radius;
   ddfx = 1;
   ddfy = -2*radius;
   x = 0;
   y = radius;
   image(x0, y0+radius) = '*';  /* Octet 0. */
   image(x0+radius, y0) = '*';  /* Octet 1. */
   image(x0, y0-radius) = '*';  /* Octet 2. */
   image(x0-radius, y0) = '*';  /* Octet 3. */

   do while (x < y);
      if f >= 0 then
         do; y = y - 1; ddfy = ddfy +2; f = f + ddfy; end;
      x = x + 1;
      ddfx = ddfx + 2;
      f = f + ddfx;
      image(x0+x, y0+y) = '0';  /* Draws octant 0. */
            image(x0+y, y0+x) = '1';  /* Draws octant 1. */
            image(x0+y, y0-x) = '2';  /* Draws octant 2. */
            image(x0+x, y0-y) = '3';  /* Draws octant 3. */
            image(x0-x, y0-y) = '4';  /* Draws octant 4. */
            image(x0-y, y0-x) = '5';  /* Draws octant 5. */
            image(x0-y, y0+x) = '6';  /* Draws octant 6. */
            image(x0-x, y0+y) = '7';  /* Draws octant 7. */
   end;
end draw_circle;

END CIRCLE;

```

{{out}} for three circles centered at the origin.

```txt

....................|....................
................2222*1111................
.............222....|....111.............
...........22.......|.......11...........
..........2.........|.........1..........
........22..........|..........11........
.......3............|............0.......
......2.............|.............1......
.....3..............|..............0.....
.....3...........222*111...........0.....
....3..........22...|...11..........0....
...3..........2.....|.....1..........0...
...3........32....22*11....11........0...
..3.........3...22..|..11...0.........0..
..3........3...3....|....0...0........0..
..3.......3...2.....|.....1...0.......0..
.3........3..3......|......0..0........0.
.3.......3...3......|......0...0.......0.
.3.......3..3.......|.......0..0.......0.
.3.......3..3.......|.......0..0.......0.
-*-------*--*-------+-------*--*-------*-
.4.......4..4.......|.......7..7.......7.
.4.......4..4.......|.......7..7.......7.
.4.......4...4......|......7...7.......7.
.4........4..4......|......7..7........7.
..4.......4...5.....|.....6...7.......7..
..4........4...4....|....7...7........7..
..4.........4...55..|..66...7.........7..
...4........55....55*66....67........7...
...4..........5.....|.....6..........7...
....4..........55...|...66..........7....
.....4...........555*666...........7.....
.....4..............|..............7.....
......5.............|.............6......
.......4............|............7.......
........55..........|..........66........
..........5.........|.........6..........
...........55.......|.......66...........
.............555....|....666.............
................5555*6666................
....................|....................

```



## PureBasic


```PureBasic
Procedure rasterCircle(cx, cy, r, Color)
  ;circle must lie completely within the image boundaries
  Protected f= 1 - r
  Protected ddF_X, ddF_Y = -2 * r
  Protected x, y = r

  Plot(cx, cy + r, Color)
  Plot(cx, cy - r, Color)
  Plot(cx + r, cy, Color)
  Plot(cx - r, cy, Color)
  While x < y
    If f >= 0
      y - 1
      ddF_Y + 2
      f + ddF_Y
    EndIf
    x + 1
    ddF_X + 2
    f + ddF_X + 1
    Plot(cx + x, cy + y, Color)
    Plot(cx - x, cy + y, Color)
    Plot(cx + x, cy - y, Color)
    Plot(cx - x, cy - y, Color)
    Plot(cx + y, cy + x, Color)
    Plot(cx - y, cy + x, Color)
    Plot(cx + y, cy - x, Color)
    Plot(cx - y, cy - x, Color)
  Wend
EndProcedure

OpenWindow(0, 0, 0, 100, 100, "MidPoint Circle Algorithm", #PB_Window_SystemMenu)
CreateImage(0, 100, 100, 32)
StartDrawing(ImageOutput(0))
  Box(0, 0, 100, 100, RGB(0, 0, 0))
  rasterCircle(25, 25, 20, RGB(255, 255, 255))
  rasterCircle(50, 50, 40, RGB(255, 0, 0))
StopDrawing()
ImageGadget(0, 0, 0, 0, 0, ImageID(0))

Repeat: Until WaitWindowEvent() = #PB_Event_CloseWindow
```



## Python

{{works with|Python|3.1}}

Extending the example given [[Basic_bitmap_storage#Alternative_version|here]]

```python
def circle(self, x0, y0, radius, colour=black):
    f = 1 - radius
    ddf_x = 1
    ddf_y = -2 * radius
    x = 0
    y = radius
    self.set(x0, y0 + radius, colour)
    self.set(x0, y0 - radius, colour)
    self.set(x0 + radius, y0, colour)
    self.set(x0 - radius, y0, colour)

    while x < y:
        if f >= 0:
            y -= 1
            ddf_y += 2
            f += ddf_y
        x += 1
        ddf_x += 2
        f += ddf_x
        self.set(x0 + x, y0 + y, colour)
        self.set(x0 - x, y0 + y, colour)
        self.set(x0 + x, y0 - y, colour)
        self.set(x0 - x, y0 - y, colour)
        self.set(x0 + y, y0 + x, colour)
        self.set(x0 - y, y0 + x, colour)
        self.set(x0 + y, y0 - x, colour)
        self.set(x0 - y, y0 - x, colour)
Bitmap.circle = circle

bitmap = Bitmap(25,25)
bitmap.circle(x0=12, y0=12, radius=12)
bitmap.chardisplay()

'''
The origin, 0,0; is the lower left, with x increasing to the right,
and Y increasing upwards.

The program above produces the following display :

+-------------------------+
|         @@@@@@@         |
|       @@       @@       |
|     @@           @@     |
|    @               @    |
|   @                 @   |
|  @                   @  |
|  @                   @  |
| @                     @ |
| @                     @ |
|@                       @|
|@                       @|
|@                       @|
|@                       @|
|@                       @|
|@                       @|
|@                       @|
| @                     @ |
| @                     @ |
|  @                   @  |
|  @                   @  |
|   @                 @   |
|    @               @    |
|     @@           @@     |
|       @@       @@       |
|         @@@@@@@         |
+-------------------------+
'''

```



## Racket

Port of the Pyhton solution.


```racket

#lang racket
(require racket/draw)

(define-syntax ⊕ (syntax-rules () [(_ id e) (set! id (+ id e))]))

(define (draw-point dc x y)
  (send dc draw-point x y))

(define (draw-circle dc x0 y0 r)
  (define f (- 1 r))
  (define ddf_x 1)
  (define ddf_y (* -2 r))
  (define x 0)
  (define y r)
  (draw-point dc    x0    (+ y0 r))
  (draw-point dc    x0    (- y0 r))
  (draw-point dc (+ x0 r)    y0)
  (draw-point dc (- x0 r)    y0)
  (let loop ()
    (when (< x y)
      (when (>= f 0)
        (⊕ y -1)
        (⊕ ddf_y 2)
        (⊕ f ddf_y))
      (⊕ x 1)
      (⊕ ddf_x 2)
      (⊕ f ddf_x)
      (draw-point dc (+ x0 x) (+ y0 y))
      (draw-point dc (- x0 x) (+ y0 y))
      (draw-point dc (+ x0 x) (- y0 y))
      (draw-point dc (- x0 x) (- y0 y))
      (draw-point dc (+ x0 y) (+ y0 x))
      (draw-point dc (- x0 y) (+ y0 x))
      (draw-point dc (+ x0 y) (- y0 x))
      (draw-point dc (- x0 y) (- y0 x))
      (loop))))

(define bm (make-object bitmap% 25 25))
(define dc (new bitmap-dc% [bitmap bm]))
(send dc set-smoothing 'unsmoothed)
(send dc set-pen "red" 1 'solid)
(draw-circle dc 12 12 12)
bm

```



## REXX

Programming note:   because of character output to a terminal screen, a circle appears to be elongated in the

vertical direction because characters are "taller" than they're "wide", so this REXX version attempts to maintain

a good aspect ratio.

The program automatically shows all of the plot's points by finding the minimum and maximum X,Y coördinates.

```rexx
/*REXX program  plots  three circles  using  midpoint/Bresenham's circle algorithm.     */
@.= '·'                                          /*fill the array with middle─dots char.*/
minX=0;     maxX=0;      minY=0;     maxY=0      /*initialize the minimums and maximums.*/
call drawCircle   0,   0,   8,   '#'             /*plot 1st circle with pound character.*/
call drawCircle   0,   0,  11,   '$'             /*  "  2nd    "     "  dollar    "     */
call drawCircle   0,   0,  19,   '@'             /*  "  3rd    "     "  commercial at.  */
border=2                                         /*BORDER:  shows  N  extra grid points.*/
minX=minX-border*2;   maxX=maxX+border*2         /*adjust min and max  X  to show border*/
minY=minY-border  ;   maxY=maxY+border           /*   "    "   "   "   Y   "   "     "  */
if @.0.0==@.  then @.0.0='┼'                     /*maybe define the plot's axis origin. */
                                                 /*define the plot's horizontal grid──┐ */
  do h=minX  to maxX;  if @.h.0==@.  then @.h.0='─';  end              /* ◄───────────┘ */
  do v=minY  to maxY;  if @.0.v==@.  then @.0.v='│';  end              /* ◄──────────┐  */
                                                 /*define the plot's vertical grid───┘  */
     do y=maxY  by -1  to minY;   _=             /* [↓]  draw grid from  top ──► bottom.*/
             do x=minX  to maxX;  _=_ || @.x.y   /* ◄───   "    "    "  left ──► right. */
             end   /*x*/                         /* [↑]  a grid row should be finished. */
     say _                                       /*display a single row of the grid.    */
     end           /*y*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
drawCircle: procedure expose @. minX maxX minY maxY
            parse arg xx,yy,r 1 y,plotChar;     fx=1;     fy=-2*r  /*get X,Y coördinates*/
            f=1-r
                    do x=0  while  x<y  /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒*/
                    if f>=0  then  do;  y=y-1;  fy=fy+2;  f=f+fy;  end               /*▒*/
                                                fx=fx+2;  f=f+fx                     /*▒*/
                    call plotPoint  xx+x,  yy+y                                      /*▒*/
                    call plotPoint  xx+y,  yy+x                                      /*▒*/
                    call plotPoint  xx+y,  yy-x                                      /*▒*/
                    call plotPoint  xx+x,  yy-y                                      /*▒*/
                    call plotPoint  xx-y,  yy+x                                      /*▒*/
                    call plotPoint  xx-x,  yy+y                                      /*▒*/
                    call plotPoint  xx-x,  yy-y                                      /*▒*/
                    call plotPoint  xx-y,  yy-x                                      /*▒*/
                    end   /*x*/         /* [↑]  place plot points ══► plot.▒▒▒▒▒▒▒▒▒▒▒▒▒*/
            return
/*──────────────────────────────────────────────────────────────────────────────────────*/
plotPoint:  parse arg c,r;     @.c.r=plotChar    /*assign a character to be plotted.    */
            minX=min(minX,c);  maxX=max(maxX,c)  /*determine the minimum and maximum  X.*/
            minY=min(minY,r);  maxY=max(maxY,r)  /*    "      "     "     "     "     Y.*/
            return
```

'''output'''

```txt

·······················│·······················
·······················│·······················
····················@@@@@@@····················
·················@@@···│···@@@·················
···············@@······│······@@···············
··············@········│········@··············
············@@·········│·········@@············
···········@···········│···········@···········
··········@············│············@··········
·········@·············│·············@·········
········@············$$$$$············@········
········@··········$$··│··$$··········@········
·······@··········$····│····$··········@·······
······@·········$$····###····$$·········@······
······@········$····##·│·##····$········@······
·····@·········$···#···│···#···$·········@·····
·····@········$···#····│····#···$········@·····
·····@·······$···#·····│·····#···$·······@·····
····@········$··#······│······#··$········@····
····@·······$···#······│······#···$·······@····
····@·······$··#·······│·······#··$·······@····
────@───────$──#───────┼───────#──$───────@────
····@·······$··#·······│·······#··$·······@····
····@·······$···#······│······#···$·······@····
····@········$··#······│······#··$········@····
·····@·······$···#·····│·····#···$·······@·····
·····@········$···#····│····#···$········@·····
·····@·········$···#···│···#···$·········@·····
······@········$····##·│·##····$········@······
······@·········$$····###····$$·········@······
·······@··········$····│····$··········@·······
········@··········$$··│··$$··········@········
········@············$$$$$············@········
·········@·············│·············@·········
··········@············│············@··········
···········@···········│···········@···········
············@@·········│·········@@············
··············@········│········@··············
···············@@······│······@@···············
·················@@@···│···@@@·················
····················@@@@@@@····················
·······················│·······················
·······················│·······················

```



## Ruby


```ruby
Pixel = Struct.new(:x, :y)

class Pixmap
  def draw_circle(pixel, radius, colour)
    validate_pixel(pixel.x, pixel.y)

    self[pixel.x, pixel.y + radius] = colour
    self[pixel.x, pixel.y - radius] = colour
    self[pixel.x + radius, pixel.y] = colour
    self[pixel.x - radius, pixel.y] = colour

    f = 1 - radius
    ddF_x = 1
    ddF_y = -2 * radius
    x = 0
    y = radius
    while x < y
      if f >= 0
        y -= 1
        ddF_y += 2
        f += ddF_y
      end
      x += 1
      ddF_x += 2
      f += ddF_x
      self[pixel.x + x, pixel.y + y] = colour
      self[pixel.x + x, pixel.y - y] = colour
      self[pixel.x - x, pixel.y + y] = colour
      self[pixel.x - x, pixel.y - y] = colour
      self[pixel.x + y, pixel.y + x] = colour
      self[pixel.x + y, pixel.y - x] = colour
      self[pixel.x - y, pixel.y + x] = colour
      self[pixel.x - y, pixel.y - x] = colour
    end
  end
end

bitmap = Pixmap.new(30, 30)
bitmap.draw_circle(Pixel[14,14], 12, RGBColour::BLACK)
```



## Scala

Uses the [[Basic_bitmap_storage#Scala|Scala Basic Bitmap Storage]] class.

```scala
object BitmapOps {
   def midpoint(bm:RgbBitmap, x0:Int, y0:Int, radius:Int, c:Color)={
      var f=1-radius
      var ddF_x=1
      var ddF_y= -2*radius
      var x=0
      var y=radius

      bm.setPixel(x0, y0+radius, c)
      bm.setPixel(x0, y0-radius, c)
      bm.setPixel(x0+radius, y0, c)
      bm.setPixel(x0-radius, y0, c)

      while(x < y)
      {
         if(f >= 0)
         {
           y-=1
           ddF_y+=2
           f+=ddF_y
         }
         x+=1
         ddF_x+=2
         f+=ddF_x

         bm.setPixel(x0+x, y0+y, c)
         bm.setPixel(x0-x, y0+y, c)
         bm.setPixel(x0+x, y0-y, c)
         bm.setPixel(x0-x, y0-y, c)
         bm.setPixel(x0+y, y0+x, c)
         bm.setPixel(x0-y, y0+x, c)
         bm.setPixel(x0+y, y0-x, c)
         bm.setPixel(x0-y, y0-x, c)
      }
   }
}
```



## Tcl

{{libheader|Tk}}
ref [[Basic bitmap storage#Tcl]] and [[Assertions#Tcl]]

```tcl
package require Tcl 8.5
package require Tk

proc drawCircle {image colour point radius} {
    lassign $point x0 y0

    setPixel $image $colour [list $x0 [expr {$y0 + $radius}]]
    setPixel $image $colour [list $x0 [expr {$y0 - $radius}]]
    setPixel $image $colour [list [expr {$x0 + $radius}] $y0]
    setPixel $image $colour [list [expr {$x0 - $radius}] $y0]

    set f [expr {1 - $radius}]
    set ddF_x 1
    set ddF_y [expr {-2 * $radius}]
    set x 0
    set y $radius

    while {$x < $y} {
        assert {$ddF_x == 2 * $x + 1}
        assert {$ddF_y == -2 * $y}
        assert {$f == $x*$x + $y*$y - $radius*$radius + 2*$x - $y + 1}
        if {$f >= 0} {
            incr y -1
            incr ddF_y 2
            incr f $ddF_y
        }
        incr x
        incr ddF_x 2
        incr f $ddF_x
        setPixel $image $colour [list [expr {$x0 + $x}] [expr {$y0 + $y}]]
        setPixel $image $colour [list [expr {$x0 - $x}] [expr {$y0 + $y}]]
        setPixel $image $colour [list [expr {$x0 + $x}] [expr {$y0 - $y}]]
        setPixel $image $colour [list [expr {$x0 - $x}] [expr {$y0 - $y}]]
        setPixel $image $colour [list [expr {$x0 + $y}] [expr {$y0 + $x}]]
        setPixel $image $colour [list [expr {$x0 - $y}] [expr {$y0 + $x}]]
        setPixel $image $colour [list [expr {$x0 + $y}] [expr {$y0 - $x}]]
        setPixel $image $colour [list [expr {$x0 - $y}] [expr {$y0 - $x}]]

    }
}

# create the image and display it
set img [newImage 200 100]
label .l -image $img
pack .l

fill $img black
drawCircle $img blue {100 50} 49
```



## Vedit macro language


```vedit

//  Draw a circle using Bresenham's circle algorithm.
//  #21 = center x, #22 = center y; #23 = radius

:DRAW_CIRCLE:
#30 = 1 - #23		// f
#31 = 0			// ddF_x
#32 = -2 * #23		// ddF_y
#41 = 0			// x
#42 = #23		// y

while (#41 <= #42) {
    #1 = #21+#41; #2 = #22+#42; Call("DRAW_PIXEL")
    #1 = #21-#41; #2 = #22+#42; Call("DRAW_PIXEL")
    #1 = #21+#41; #2 = #22-#42; Call("DRAW_PIXEL")
    #1 = #21-#41; #2 = #22-#42; Call("DRAW_PIXEL")
    #1 = #21+#42; #2 = #22+#41; Call("DRAW_PIXEL")
    #1 = #21-#42; #2 = #22+#41; Call("DRAW_PIXEL")
    #1 = #21+#42; #2 = #22-#41; Call("DRAW_PIXEL")
    #1 = #21-#42; #2 = #22-#41; Call("DRAW_PIXEL")
    if (#30 >= 0) {
	#42--
	#32 += 2
	#30 += #32
    }
    #41++
    #31 += 2
    #30 += #31 + 1
}

return

```



## XPL0


```XPL0
include c:\cxpl\codes;          \include 'code' declarations

proc Circle(X0, Y0, Radius, Color);     \Display a circle
int  X0, Y0,    \coordinates of center
     Radius,    \radius in (pixels)
     Color;     \line color
int  X, Y, E, U, V;

        proc PlotOctants;               \Segment
        [Point(X0+Y, Y0+X, Color);      \ 0
        Point(X0+X, Y0+Y, Color);       \ 1
        Point(X0-X, Y0+Y, Color);       \ 2
        Point(X0-Y, Y0+X, Color);       \ 3
        Point(X0-Y, Y0-X, Color);       \ 4
        Point(X0-X, Y0-Y, Color);       \ 5
        Point(X0+X, Y0-Y, Color);       \ 6
        Point(X0+Y, Y0-X, Color);       \ 7
        ]; \PlotOctants

[X:= 0;  Y:= Radius;
U:= 1;
V:= 1 -Radius -Radius;
E:= 1 -Radius;
while X < Y do
        [PlotOctants;
        if E < 0 then
                [U:= U+2;  V:= V+2;  E:= E+U]
        else    [U:= U+2;  V:= V+4;  E:= E+V;  Y:= Y-1];
        X:= X+1;
        ];
if X = Y then PlotOctants;
]; \Circle

[SetVid($112);          \640x480 in 24-bit RGB color
Circle(110, 110, 50, $FFFF00);
if ChIn(1) then [];     \wait for keystroke
SetVid(3);              \restore normal text mode
]
```



## zkl

Image cribbed from the BBC BASIC entry. Algorithm from Wikipedia article.<br/>
Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
[[Image:circle_bbc.gif|right]]
This is the code from the PPM class:

```zkl
   fcn circle(x0,y0,r,rgb){
      x:=r; y:=0; radiusError:=1-x;
      while(x >= y){
         __sSet(rgb, x + x0,  y + y0);
	 __sSet(rgb, y + x0,  x + y0);
	 __sSet(rgb,-x + x0,  y + y0);
	 __sSet(rgb,-y + x0,  x + y0);
	 self[-x + x0, -y + y0]=rgb;	// or do it this way, __sSet gets called as above
	 self[-y + x0, -x + y0]=rgb;
	 self[ x + x0, -y + y0]=rgb;
	 self[ y + x0, -x + y0]=rgb;
	 y+=1;
	 if (radiusError<0) radiusError+=2*y + 1;
	 else{ x-=1; radiusError+=2*(y - x + 1); }
      }
   }
```


```zkl
ppm:=PPM(200,200,0xFF|FF|FF);
ppm.circle(100,100,40,00);         // black circle
ppm.circle(100,100,80,0xFF|00|00); // red circle

ppm.write(File("foo.ppm","wb"));
```


{{omit from|AWK}}
{{omit from|PARI/GP}}

[[Category:Geometry]]
