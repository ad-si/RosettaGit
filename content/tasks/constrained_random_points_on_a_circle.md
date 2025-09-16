+++
title = "Constrained random points on a circle"
description = ""
date = 2019-02-04T12:04:49Z
aliases = []
[extra]
id = 8230
[taxonomies]
categories = ["task", "Probability and statistics"]
languages = [
  "11l",
  "ada",
  "algol_68",
  "autohotkey",
  "basic",
  "bbc_basic",
  "free_basic",
  "c",
  "cpp",
  "csharp",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "d",
  "echo_lisp",
  "elixir",
  "euphoria",
  "falcon",
  "fortran",
  "gnuplot",
  "go",
  "haskell",
  "hy",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "liberty_basic",
  "locomotive_basic",
  "maple",
  "matlab",
  "maxima",
  "nim",
  "ocaml",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
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
  "sidef",
  "systemverilog",
  "tcl",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
tags = []
+++

## Task

Generate 100 <x,y> coordinate pairs such that x and y are integers sampled from the uniform distribution with the condition that
<math>10 \leq \sqrt{ x^2 + y^2 } \leq 15 </math>.
Then display/plot them. The outcome should be a "fuzzy" circle. The actual number of points plotted may be less than 100, given that some pairs may be generated more than once.

There are several possible approaches to accomplish this. Here are two possible algorithms.

1) Generate random pairs of integers and filter out those that don't satisfy this condition:
:<math>10 \leq \sqrt{ x^2 + y^2 } \leq 15 </math>.

2) Precalculate the set of all possible points (there are 404 of them) and select randomly from this set.





## 11l

```11l
F print_circle(lo, hi, ndots)
   V canvas = [[0B] * (2*hi+1)] * (2*hi+1)
   V i = 0
   L i < ndots
      V x = random:(-hi..hi)
      V y = random:(-hi..hi)
      I x^2 + y^2 C lo^2 .. hi^2
         canvas[x + hi][y + hi] = 1B
         i++

   L(i) 0 .. 2*hi
      print(canvas[i].map(j -> I j {‘♦ ’} E ‘  ’).join(‘’))

print_circle(10, 15, 100)
```



## Ada



```Ada
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
procedure Circle is
   -- extreme coordinate values are -15:0, 15:0, 0:-15, 0:15
   subtype Coordinate is Integer range -15 .. 15;
   type Point is record
      X, Y : Coordinate;
   end record;
   type Point_List is array (Positive range <>) of Point;

   function Acceptable (Position : Point) return Boolean is
      Squared_Sum : Natural := Position.X ** 2 + Position.Y ** 2;
   begin
      return 10 ** 2 <= Squared_Sum and Squared_Sum <= 15 ** 2;
   end Acceptable;

   -- first algorithm
   function Generate_Random_Points
     (Count : Positive := 100)
      return  Point_List
   is
      package RNG is new Ada.Numerics.Discrete_Random (Coordinate);
      Generator  : RNG.Generator;
      Next_Point : Point;
      Result     : Point_List (1 .. Count);
   begin
      RNG.Reset (Generator);
      for N in Result'Range loop
         loop
            Next_Point.X := RNG.Random (Generator);
            Next_Point.Y := RNG.Random (Generator);
            exit when Acceptable (Next_Point);
         end loop;
         Result (N) := Next_Point;
      end loop;
      return Result;
   end Generate_Random_Points;

   -- second algorithm
   function Choose_Precalculated
     (Count : Positive := 100)
      return  Point_List
   is
      subtype Possible_Points is Positive range 1 .. 404;
      package RNG is new Ada.Numerics.Discrete_Random (Possible_Points);
      Generator  : RNG.Generator;
      Point_Pool : Point_List (Possible_Points);
      Next_Point : Point;
      Next_Index : Possible_Points := 1;
      Result     : Point_List (1 .. Count);
   begin
      -- precalculate
      Precalculate : for X in Coordinate'Range loop
         Next_Point.X := X;
         for Y in Coordinate'Range loop
            Next_Point.Y := Y;
            if Acceptable (Next_Point) then
               Point_Pool (Next_Index) := Next_Point;
               exit Precalculate when Next_Index = Possible_Points'Last;
               Next_Index := Next_Index + 1;
            end if;
         end loop;
      end loop Precalculate;
      -- choose
      RNG.Reset (Generator);
      for N in Result'Range loop
         Result (N) := Point_Pool (RNG.Random (Generator));
      end loop;
      return Result;
   end Choose_Precalculated;

   procedure Print_Points (Points : Point_List) is
      Output_String : array (Coordinate, Coordinate) of Character :=
        (others => (others => ' '));
   begin
      for N in Points'Range loop
         Output_String (Points (N).X, Points (N).Y) := '*';
      end loop;
      for Line in Output_String'Range (2) loop
         for Column in Output_String'Range (1) loop
            Ada.Text_IO.Put (Output_String (Column, Line));
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Print_Points;

   My_Circle_Randomly      : Point_List := Generate_Random_Points;
   My_Circle_Precalculated : Point_List := Choose_Precalculated;
begin
   Ada.Text_IO.Put_Line ("Randomly generated:");
   Print_Points (My_Circle_Randomly);
   Ada.Text_IO.Put_Line ("Chosen from precalculated:");
   Print_Points (My_Circle_Precalculated);
end Circle;
```


Output:

```txt
Randomly generated:

              **
          *  *    * *
               * * *  **
     *     * *      *
    **  *              * *
   *     *           *  *
      *              *   * *
   *                   **
  *    *               *
    *
    *                       *
   *                      * *
  * *                        *
                             *

   * *                       *
  *
     *                       *
  ***                      *
     *                  *    *
                            *
   *                       *
                     **    *
         **
     *        *   **     *
         * *        * *
      ***  * *         **
        * *   * ***
               *  *

Chosen from precalculated:

            *
            *    *   *
           * **   ** ** *
           * *  * *
                      *
   *  ** *              *
    *    *               * *
  *    *                ***
  *  ***                 *
                        *  * *
                            *

  *                       * *
    *                      **

   *
 *  **                     ***
                           *
                         *   *
  * **
   *                     *
     *
    *   *
         **           *
    *  *     *  *       * *
       **  *       *  * *
      *   **
        *
           *    * ***

```



## ALGOL 68

{{trans|C}} - note: This specimen retains the original [[#C|C]] coding style.

```algol68
PROC clrscr = VOID:
        printf(($g"[2J"$,REPR 27)); # ansi.sys #

PROC gotoxy = (INT x,y)VOID:
        printf(($g"["g(0)";"g(0)"H"$,REPR 27, y,x)); # ansi.sys #

MODE POINT = STRUCT(
    INT x,y
);

INT radius = 15;
INT inside radius = 10;

POINT center = (radius+1, radius+1);

FLEX[0]POINT set;

PROC swap with last set = (INT position,INT where last set)VOID:
(
        INT temp := x OF set[position];
        x OF set[position]:=x OF set[where last set];
        x OF set[where last set] := temp;

        temp := y OF set[position];
        y OF set[position]:=y OF set[where last set];
        y OF set[where last set] := temp
);

PROC create set = VOID:
(
        set := HEAP[(2*radius+1)**2]POINT;
        INT x,y,i:=LWB set;

        FOR x FROM -radius TO radius DO
                FOR y FROM -radius TO radius DO
                        IF sqrt(x*x+y*y)>=inside radius AND sqrt(x*x+y*y)<=radius THEN
                                x OF set[i] := x;
                                y OF set[i] := y;
                                i+:=1
                        FI
                OD
        OD;

        set:=set[:i-1]
);

PROC plot fuzzy set = (CHAR ch)VOID:
(
        INT pos,i;

        TO UPB set DO
                pos := ENTIER(random * UPB set) + 1;

                gotoxy(x OF center + x OF set[pos],y OF center + y OF set[pos]);

                print(ch);

                swap with last set(pos,UPB set)

        OD
);

main:
(
        # srand((INT)time(NIL)); #

        clrscr;
        create set;
        plot fuzzy set("*");
        gotoxy(2*radius+1, 2*radius+1);
        newline(stand in)
)
```

Sample output:

```txt

           *  * **
        * * *  * ** **
      ***** **   ***** **
     ** **  *  * * * ***
      *  ******** *** *** *
   ** *****         ** * ***
        *            ***** *
   ** **               **** *
   * * *               ****
  ****                  * ****
 * **                    *** *
 ** **                    **
 **                      * * *
  ****                   **  *
 ** *                    ****
 ****                     ** *
 *  **                   *  **
  * **                    *  *
 * ***                    *  *
 ******                 * * **
  * * **               **** *
   ** *                ***  *
   **** **           *    **
    ** ***            * ***
    * *  *** * **  *** ***
      *  *  ** ***** ****
      ** ******* *  *
        * ** ** *******
          *  ****** *
               *

```



## AutoHotkey

Requires the GDI+ standard library by tic: http://www.autohotkey.com/forum/viewtopic.php?t=32238<br /> Works with individual pixels.
[[File:Ahk_fuzzycircle.png|thumb|right]]
```AHK
z=100 ; x = x-coord; y = y-coord; z = count; pBitmap = a pointer to the image; f = filename

pToken	:= Gdip_Startup()
pBitmap := Gdip_CreateBitmap(31, 32)

While z
{
	Random, x, -20, 20
	Random, y, -20,20
	If ( t := sqrt(x**2 + y**2) ) >= 10 && t <= 15
		Gdip_SetPixel(pBitmap, x+15, y+16, 255<<24), z--
}

Gdip_SaveBitmapToFile(pBitmap, f := A_ScriptDir "\ahk_fuzzycircle.png")
run % f

Gdip_DisposeImage(pBitmap)
Gdip_Shutdown(pToken)
```


## BASIC


=
## BBC BASIC
=

```bbcbasic
      MODE 8
      ORIGIN 640, 512
      FOR i% = 1 TO 1000
        x% = RND(31)-16
        y% = RND(31)-16
        r = SQR(x%^2 + y%^2)
        IF r >= 10 IF r <= 15 PLOT x%*2, y%*2
      NEXT
```


=
## FreeBASIC
=
Pre calculate and plot 100 points to the console

```FreeBASIC
'Free Basic version .9

#define Intrange(f,l) int(Rnd*(((l)+1)-(f))+(f))

Type pair
    As Integer x,y
End Type

Operator =(a As pair,b As pair) As Integer
Return a.x=b.x And a.y=b.y
End Operator

Function NotInArray(a() As pair,n As pair) As Integer
    For z As Integer=Lbound(a) To Ubound(a)
        If a(z)=n Then Return 0
    Next z
    Return -1
End Function

Redim As pair pts(0)
Dim As Integer x,y,counter
Do
    counter=counter+1
    x=IntRange(-20,20)
    y=IntRange(-20,20)
    var root=Sqr(x*x+y*y)
    If 10<= root And root<=15 Then
        If NotInArray(pts(),Type<pair>(x,y)) Then
            Redim Preserve pts(1 To Ubound(pts)+1)
            pts(Ubound(pts))=Type<pair>(x,y)
        End If
    End If
Loop Until counter=100000

'
### =========== Plot to Console ===================


dim as integer yres=hiword(width)
dim as integer xres=loword(width)

#define map(a,b,x,c,d)  ((d)-(c))*((x)-(a))/((b)-(a))+(c)
#define _X(num) int( map(0,xres,(num),0,loword(width)))
#define _Y(num) int( map(0,yres,(num),0,hiword(width)))

counter=0
For n As Integer=Lbound(pts) To Ubound(pts)
    counter=counter+1
    if counter <=100 then
    var xpos=map(-20,20,pts(n).x,0,xres)
    var ypos=map(-20,20,pts(n).y,0,yres)
    locate _Y(ypos),_X(xpos)
    print "*"
    end if
Next n

print
locate 1,1
Print "Total number of points "; counter
print "Total number plotted   ";100
print "done"
Sleep

```

Console output:

```txt

Total number of points  404
Total number plotted    100
done                                           * *
                         *                 *       *
                           *     * * *       *           *
                 * * * *     *                 * * *     *     *
                 * *                               *   * *
             *   * * * *                                         *
                                                             *
             * *                                           *     * *
           * *   *                                         * *     *
               *                                             * *   *
             * * *                                           *     *

                   *                                         * * * *
               *   * *                                     * * *
               *           *                         *
                 *         *         *   *     * *     *     *
                               *         *     * *
                     *   *   *   *   * * *   *   *     * *
                                       * *   * * *



```



## C


```c
#include <stdio.h>
#include <stdlib.h>

inline
int randn(int m)
{
	int rand_max = RAND_MAX - (RAND_MAX % m);
	int r;
	while ((r = rand()) > rand_max);
	return r / (rand_max / m);
}

int main()
{
	int i, x, y, r2;
	unsigned long buf[31] = {0}; /* could just use 2d array */

	for (i = 0; i < 100; ) {
		x = randn(31) - 15;
		y = randn(31) - 15;
		r2 = x * x + y * y;
		if (r2 >= 100 && r2 <= 225) {
			buf[15 + y] |= 1 << (x + 15);
			i++;
		}
	}

	for (y = 0; y < 31; y++) {
		for (x = 0; x < 31; x++)
			printf((buf[y] & 1 << x) ? ". " : "  ");
		printf("\n");
	}

	return 0;
}
```
Output<lang>                            .   .     . .
                    .           .         .
                  .           .           . .
                              .             .     .
                    .       .       .     . .
          .                                 .       .
          .       .                               .
          .                                     .
                                                . .
                                                  .   .
      . .
        . .                                       . .
  .       .                                               .

                                                    .
  .

      . .                                                 .
    .                                                 . .
      .   . .
      .   .                                   .         .
        .                                       . . .   .
          .       .
      . .     .                                 .
          .   .     .   .   .         .           .
                    .       . .         .
              . . .   . . .       .
                  . .     .               .
                                        .
                              .
```



## C++

[[File:constrained_rnd_pts_on_circle.png]]

```cpp

#include <windows.h>
#include <list>
#include <iostream>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class point
{
public:
    int x, y;
    point()                  { x = y = 0; }
    point( int a, int b )    { x = a; y = b; }
    void set( int a, int b ) { x = a; y = b; }
};
//--------------------------------------------------------------------------------------------------
class rndCircle
{
public:
    void draw()
    {
	createPoints();
	drawPoints();
    }

private:
    void createPoints()
    {
	point pt;
	for( int x = 0; x < 200; x++ )
	{
	    int a, b, c;
	    while( true )
	    {
		a = rand() % 31 - 15;
		b = rand() % 31 - 15;
		c = a * a + b * b;
		if( c >= 100 && c <= 225 ) break;
	    }
	    pt.set( a, b );
	    _ptList.push_back( pt );
	}
    }

    void drawPoints()
    {
	HDC dc = GetDC( GetConsoleWindow() );
	for( list<point>::iterator it = _ptList.begin(); it != _ptList.end(); it++ )
	    SetPixel( dc, 300 + 10 * ( *it ).x, 300 + 10 * ( *it ).y, RGB( 255, 255, 0 ) );
    }

    list<point> _ptList;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );
    srand( GetTickCount() );
    rndCircle c;
    c.draw();
    system( "pause" );
    return 0;
}
//--------------------------------------------------------------------------------------------------

```


## C#


```c#
using System;
using System.Diagnostics;
using System.Drawing;

namespace RosettaConstrainedRandomCircle
{
    class Program
    {
        static void Main(string[] args)
        {
            var points = new Point[404];
            int i = 0;
            for (int y = -15; y <= 15; y++)
                for (int x = -15; x <= 15 && i < 404; x++)
                {
                    var c = Math.Sqrt(x * x + y * y);
                    if (10 <= c && c <= 15)
                    {
                        points[i++] = new Point(x, y);
                    }
                }

            var bm = new Bitmap(600, 600);
            var g = Graphics.FromImage(bm);
            var brush = new SolidBrush(Color.Magenta);

            var r = new System.Random();
            for (int count = 0; count < 100; count++)
            {
                var p = points[r.Next(403)];
                g.FillEllipse(brush, new Rectangle(290 + 19 * p.X, 290 + 19 * p.Y, 10, 10));
            }
            const string filename = "Constrained Random Circle.png";
            bm.Save(filename);
            Process.Start(filename);
        }
    }
}
```



## Clojure


```Clojure
(ns rosettacode.circle-random-points
  (:import [java.awt Color Graphics Dimension]
           [javax.swing JFrame JPanel]))

(let [points (->> (for [x (range -15 16), y (range -15 16)
			:when (<= 10 (Math/hypot x y) 15)]
		    [(+ x 15) (+ y 15)])
		  shuffle
		  (take 100))]
  (doto (JFrame.)
    (.add (doto (proxy [JPanel] []
		  (paint [^Graphics g]
                    (doseq [[x y] points]
                      (.fillRect g (* 10 x) (* 10 y) 10 10))))
	    (.setPreferredSize (Dimension. 310 310))))
    (.setResizable false)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    .pack
    .show))
```



## COBOL


```COBOL

       identification division.
       program-id. circle.
       environment division.
       input-output section.
       file-control.
           select plot-file assign "circle.txt".
       data division.
       file section.
       fd plot-file report plot.
       working-storage section.
       1 binary.
        2 seed pic 9(18).
        2 x pic s9(4).
        2 y pic s9(4).
        2 i pic 9(4).
        2 dot-count pic 9(4) value 0.
        2 dot-count-save pic 9(4) value 0.
        2 temp-points.
         3 pic s9(4) occurs 2.
        2 xy-table.
         3 point-pair occurs 0 to 404 depending dot-count.
          4 x-point pic s9(4).
          4 y-point pic s9(4).
       1 plot-table value all "0".
        2 occurs 31.
         3 dot pic 9 occurs 31.
       1 cur-date-time.
        2 yyyymmdd pic 9(8).
        2 hh pic 9(2).
        2 mm pic 9(2).
        2 ss pic 9(2).
       1 plot-work.
        2 plot-item pic xb occurs 31.
       report section.
       rd plot.
       1 plot-line type de.
        2 line plus 1.
         3 column is 1 source is plot-work pic x(62).
       procedure division.
       begin.
           perform compute-seed
           perform find-all-valid-points
           perform shuffle-point-pairs
           perform select-100-dots
           perform print-dots
           stop run
           .

       find-all-valid-points.
           perform varying x from -15 by 1 until x > +15
               perform varying y from -15 by 1 until y > +15
                   if (function sqrt (x ** 2 + y ** 2))
                       >= 10 and <= 15
                   then
                       move 1 to dot (x + 16 y + 16)
                       add 1 to dot-count
                       compute x-point (dot-count) = x + 16
                       compute y-point (dot-count) = y + 16
                   end-if
               end-perform
           end-perform
           display "Total points: " dot-count
           .

       shuffle-point-pairs.
           move dot-count to dot-count-save
           compute i = function random (seed) * dot-count + 1
           perform varying dot-count from dot-count by -1
           until dot-count < 2
               move point-pair (i) to temp-points
               move point-pair (dot-count) to point-pair (i)
               move temp-points  to point-pair (dot-count)
               compute i = function random * dot-count + 1
           end-perform
           move dot-count-save to dot-count
           .

       select-100-dots.
           perform varying i from 1 by 1
           until i > 100
               compute x = x-point (i)
               compute y = y-point (i)
               move 2 to dot (x y)
           end-perform
           .

       print-dots.
           open output plot-file
           initiate plot
           perform varying y from 1 by 1 until y > 31
               move spaces to plot-work
               perform varying x from 1 by 1 until x > 31
                   if dot (x y) = 2
                       move "o" to plot-item (x)
                   end-if
               end-perform
               generate plot-line
           end-perform
           terminate plot
           close plot-file
           .

       compute-seed.
           unstring function current-date into
               yyyymmdd hh mm ss
           compute seed =
               (function integer-of-date (yyyymmdd) * 86400)
           compute seed = seed
                 + (hh * 3600) + (mm * 60) + ss
           compute seed = function mod (seed 32768)
           .

       end program circle.

```


```txt


                      o   o           o
                o       o   o     o
            o     o   o o
            o         o     o       o o   o   o
        o     o   o o o o                           o
      o   o   o o   o                   o o       o
          o o                               o   o
        o                                               o
          o                                   o
  o                                             o o     o
      o                                                 o
                                                        o
      o                                           o       o
                                                        o o
o       o o                                           o
    o   o                                         o
          o                                       o o
      o
      o                                           o     o
      o   o
      o
    o     o   o                                 o
                                            o   o
      o       o                                   o
          o                     o           o o     o
          o   o                   o
            o o     o o                 o   o
                              o o           o
                            o   o     o
                              o

```



## CoffeeScript


```coffeescript

NUM_POINTS = 100
MIN_R = 10
MAX_R = 15

random_circle_points = ->
  rand_point = ->
    Math.floor (Math.random() * (MAX_R * 2 + 1) - MAX_R)

  points = {}
  cnt = 0
  while cnt < 100
    x = rand_point()
    y = rand_point()
    continue unless MIN_R * MIN_R <= x*x + y*y <= MAX_R * MAX_R
    points["#{x},#{y}"] = true
    cnt += 1
  points

plot = (points) ->
  range = [-1 * MAX_R .. MAX_R]
  for y in range
    s = ''
    for x in range
      s += if points["#{x},#{y}"] then '*' else ' '
    console.log s

plot random_circle_points()

```


The output may be a bit distorted, since even monospace fonts take more vertical space per character than horizontal space.
<lang>
> coffee foo.coffee

          **    *
         * ** *     *
          *  * *      *
     *     **           *
      *  *      *       *
    *               *
                      *
    *                    *
  ***
     **                 **** *
  * *
     *                      *
  **
                          *

    *                     **
 *  *                    *
   **
   *                      * **
     *                     *
  *                         *
                       *    *
     *  *
                       ***
      *** * *     *     * *
     ***           *
      * *       * *  *
               * *   *

```



## Common Lisp


```lisp
(flet ((good-p (x y) (<= 100 (+ (* x x) (* y y)) 255)))
  (loop with x with y with cnt = 0
	with scr = (loop repeat 31 collect (loop repeat 31 collect "  "))
	while (< cnt 100)
	do (when (good-p (- (setf x (random 31)) 15)
			 (- (setf y (random 31)) 15))
	     (setf (elt (elt scr y) x) "@ ")
	     (incf cnt))
	finally (mapc #'(lambda (row) (format t "~{~a~^~}~%" row)) scr)))
```



## D

This uses std.complex because D built-in complex numbers will be deprecated.

```d
import std.stdio, std.random, std.math, std.complex;

void main() {
    char[31][31] table = ' ';

    foreach (immutable _; 0 .. 100) {
        int x, y;
        do {
            x = uniform(-15, 16);
            y = uniform(-15, 16);
        } while(abs(12.5 - complex(x, y).abs) > 2.5);
        table[x + 15][y + 15] = '*';
    }

    writefln("%-(%s\n%)", table);
}
```

```txt

                   *
          * **  *
                  * *
     **  * *   **  *  *
    *    *            **  *
        **            **  *
                       *   *
                        *
       *                 ***
 *  * *
   * *                     *
   *                        *
     *                      *
   **                    * *
  *                       *
                            *
                             *
  *                          *

   *
     *                     **
    * *                  *
         *           * *  *
   *     *             **
    *     *  *  *     * *
     **     *  **   **   *
          **
        *     *  *  *
               **
```




## EchoLisp

Using the '''plot''' library. For a greater visual appeal, points are plotted as circles of random radius and color. The resulting image is at [http://www.echolalie.org/echolisp/images/circle.png].

```scheme

(lib 'math)
(lib 'plot)

(define (points (n 100) (radius 10) (rmin 10) (rmax 15)   (x) (y))
	(plot-clear)
	(plot-x-minmax (- rmax))
	(plot-y-minmax( - rmax))

	(for [(i n)]
	(set! x (round (* (random -1) rmax)))
	(set! y (round (* (random -1) rmax)))
	#:when (in-interval? (pythagore x y) rmin rmax)
	;; add a little bit of randomness : dots color and radius
	(plot-fill-color   (hsv->rgb (random) 0.9 0.9))
	(plot-circle x y (random radius)))
	(plot-edit))

```



## Elixir


### Algorithm 1: Generate random pairs

```elixir
defmodule Random do
  defp generate_point(0, _, _, set), do: set
  defp generate_point(n, f, condition, set) do
    point = {x,y} = {f.(), f.()}
    if x*x + y*y in condition and not point in set,
      do:   generate_point(n-1, f, condition, MapSet.put(set, point)),
      else: generate_point(n,   f, condition, set)
  end

  def circle do
    f = fn -> :rand.uniform(31) - 16 end
    points = generate_point(100, f, 10*10..15*15, MapSet.new)
    range = -15..15
    for x <- range do
      for y <- range do
        IO.write if {x,y} in points, do: "x", else: " "
      end
      IO.puts ""
    end
  end
end

Random.circle
```


'''Example output:'''

```txt

               x
           x x xx    x
      x x  x  x xxx x
         x x x   xx    x
    x  x  x   x     x   x
        xx               x
    xx  x             x
                          xxx
     xxx
                          xxx
 x                           x

 x                          xx
   xx                        x
 x
                            xx
   x                     x  x
  xx
  x                      x
  xx x                   x   x
  x x                     x
       x                 x
      x               xx  xx
     x                x  x
          x  x      x   x
       x xx          x xx
          x    xx x    x
           x xx  x
                x  xx

```



### Algorithm 2: Precalculate

```elixir
defmodule Constrain do
  def circle do
    range = -15..15
    r2 = 10*10..15*15
    all_points = for x <- range, y <- range, x*x+y*y in r2, do: {x,y}
    IO.puts "Precalculate: #{length(all_points)}"
    points = Enum.take_random(all_points, 100)
    Enum.each(range, fn x ->
      IO.puts Enum.map(range, fn y -> if {x,y} in points, do: "o ", else: "  " end)
    end)
  end
end

Constrain.circle
```


```txt

Precalculate: 404

                      o       o
                                          o
                  o o o     o         o         o
          o o         o                   o o
            o   o o o     o             o o
      o o   o                                   o
        o o o o                           o
    o         o                                 o       o
    o o                                       o   o     o
    o   o                                               o

                                                  o   o   o
                                                  o     o
  o       o                                               o
o     o                                               o   o o
                                                    o
      o                                           o       o
      o   o                                       o o   o o
                                                        o
                                                  o       o
    o o                                         o
            o                                   o     o
                                          o o   o o   o
            o                           o     o
                o                 o o o o       o o
          o   o           o               o o   o
                                      o
                  o o
                      o       o


```



## Euphoria

This program generates the set of 404 possible points in the ring. It randomly chooses 100 pairs from the set. The 100 pairs are a subset of that set because duplicates are discarded.

```euphoria
include std/console.e

sequence validpoints = {}
sequence discardedpoints = {}
sequence rand100points = {}
atom coordresult
integer randindex

--scan for all possible values. store discarded ones in another sequence, for extra reference.
for y = -15 to 15 do
    for x = -15 to 15 do

        coordresult = sqrt( x * x + y * y )

        if coordresult >= 10 and coordresult <= 15 then --if it would fall in the ring area
            validpoints &= {{x, y, coordresult}} --concatenate (add to the end) the coordinate pair x, y and the
            -- result into a subsequence of sequence validpoints
            else
                discardedpoints &= {{x, y, coordresult}} --else put it in the discarded sequence
        end if

    end for
end for

for i = 1 to 100 label "oneofhundred" do --make 100 random coordinate pairs
    randindex = rand(length(validpoints) ) --random value from 1 to the number of 3 value subsequences in validpoints (the data)

    if length(rand100points) = 0 then --if rand100points sequence is empty, add the first subsequence to it.
        rand100points &= {validpoints[randindex]}

        else --if it isn't empty, then..
            for j = 1 to length(rand100points) do --loop through each "data chunk" in rand100points

                if equal(validpoints[randindex], rand100points[j]) = 1 then --if any are the same as the randomly chosen chunk in
                    retry "oneofhundred" -- validpoints, then retry from one line below the "oneofhundred" loop without incrementing i.
                end if --the continue keyword would increment i instead.

            end for

            rand100points &= {validpoints[randindex]} --length of rand100points isnt 0 and no data chunks match ones that the program
            --already picked before, so add this subsequence chunk to rand100points.
    end if

end for

for i = 1 to 32 do --32 lines
    printf(1,"\n")
    for j = 1 to 32 label "xscan" do --32 characters on each line

        for k = 1 to length(rand100points) do --for every subsequence in this
            if rand100points[k][1]+16 = j and rand100points[k][2]+16 = i then --if the x and y coordinates in the picked points
                printf(1, 178) --(adjusted to minimum of 1,1) are at the same place as in the console output grid
                continue "xscan" --print a funny character and continue to the next "xscan"
            end if
        end for

        printf(1, 176) --if no picked points were there, print another funny character to represent a blank space

    end for
end for

printf(1, "\nNumber of valid coordinate pairs %d :", length(validpoints) )
printf(1, "\nNumber of discarded coordinate pairs : %d", length(discardedpoints) )
printf(1, "\nNumber of randomly picked coordinate pairs : %d\n", length(rand100points) )
any_key()
```

Output:

```txt

░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░▓░░░░░░░░░░░░░░
░░░░░░░░░░░░░▓░░░░░░░░░░░░░░░░░░
░░░░░░░░░▓░░░▓▓░▓░░░▓░░░░░░░░░░░
░░░░░░▓░░░░░░▓░░░▓░░░░░▓░▓░░░░░░
░░░░░░░░░▓░▓░░░░░░░▓░░▓░░▓▓░░░░░
░░░▓░░░░▓░▓░░░░░░░░░░░░▓░░░░░░░░
░░░░░░░░░▓░░░░░░░░░░░▓░░░▓░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░▓▓░░▓░░░
░░░░░░▓▓░░░░░░░░░░░░░░░▓░░░░░░░░
░░░░░░▓░░░░░░░░░░░░░░░░░░░░░░░░░
░▓░░▓▓░░░░░░░░░░░░░░░░░░░░░░░▓░░
░░░░░▓░░░░░░░░░░░░░░░░░░░░░░▓░░░
░░▓░░░░░░░░░░░░░░░░░░░░░░▓▓▓▓░░░
░▓░░▓░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░▓░░░░░░░░░░░░░░░░░░░░▓▓▓░▓░░
░░░▓░░░░░░░░░░░░░░░░░░░░░▓░▓░▓░░
░▓▓░░░░░░░░░░░░░░░░░░░░░░░▓▓░░░░
░░░▓░▓░░░░░░░░░░░░░░░░░░░▓░░░░░░
░░░▓░░░░░░░░░░░░░░░░░░░░░▓░░░▓░░
░░▓░▓░░░░░░░░░░░░░░░░░░░▓░░░▓░░░
░░░░░░░▓░░░░░░░░░░░░░░░▓░░░░░░░░
░░░▓░░░░░░░░░░░░░░░░░░░▓░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░▓░░░░░░░
░░░░░░▓░░▓░░░░░░░░░░░▓░▓░░░░░░░░
░░░░░░░░░░░░░░░░░▓░░░░░░░░░░░░░░
░░░░░░░▓░░▓░░░░░░░░░░░░▓░░░░░░░░
░░░░░░░░░░░░░▓░▓▓▓▓░░░▓▓░░░░░░░░
░░░░░░░░░▓▓░░▓░░▓░░▓▓░░░░░░░░░░░
░░░░░░░░░░▓░░▓░▓▓▓░░▓░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
Number of valid coordinate pairs 404 :
Number of discarded coordinate pairs : 557
Number of randomly picked coordinate pairs : 100
Press Any Key to continue...
```
Extra EuSDL code :
```euphoria

for i = 1 to length(validpoints) do --simple each pixel output to screen surface
    dummy=pixelColor(surface,validpoints[i][1]+18,validpoints[i][2]+18,#AA0202FF) --i is index number of each subsequence 'chunk'.
    --index 1 is x, index 2 is y, inside that chunk.
end for

for i = 1 to length(discardedpoints) do
    dummy=pixelColor(surface,discardedpoints[i][1]+18,discardedpoints[i][2]+52,#0202AAFF)
end for

for i = 1 to length(rand100points) do
    dummy=pixelColor(surface,rand100points[i][1]+55,rand100points[i][2]+52,#02AA02FF)
end for

dummy=boxColor(surface,0,71,395,111,#232323FF) --background box
dummy=stringColor(surface,0,73,sprintf("Number of valid coordinate pairs %d :", length(validpoints) ),#AA0202FF)

dummy=stringColor(surface,0,83,sprintf("Number of discarded coordinate pairs : %d", length(discardedpoints) ),#0202AAFF)

dummy=stringColor(surface,0,93,sprintf("Number of randomly picked coordinate pairs : %d", length(rand100points) ),#02AA02FF)
```
SDL Output :
[[File:Fuzzy_circle_Euphoria.png]] That particular program used a -16 to +16 square area, so more was discarded.

=={{header|F_Sharp|F#}}==
This version uses method 1 from the task description and just calculates 100 suitable points to plot.
The INTERACTIVE bit just permits this code in a .fsx file to be run with the interactive interpreter or compiled to an exe.

```fsharp
module CirclePoints =
    let main args =
        let rnd = new System.Random()
        let rand size = rnd.Next(size) - size/2
        let size = 30
        let gen n =
            let rec f (x,y) =
                let t = (int (sqrt (float (x*x + y*y)) ))
                if 10 <= t && t <= 15 then (x,y) else f (rand size, rand size)
            f (rand size, rand size)
        let plot = Array.init 100 (fun n -> gen n)
        for row in 0 .. size-1 do
            let chars = Array.create (size+1) ' '
            Array.choose (fun (x,y) -> if y = (row-size/2) then Some(x) else None) plot
            |> Array.iter (fun x -> chars.[x+size/2] <- 'o')
            printfn "%s" (new string(chars))
        0

#if INTERACTIVE
CirclePoints.main fsi.CommandLineArgs
#else
[<EntryPoint>]
let main args = CirclePoints.main args
#endif
```

An example of the output:

```txt

          o  o oo
              o       o

         o o        o o
    o   o o oo o o       o o
   o                      o
     o  oo             oooo
     o                  oo
                       o  o
   oo                      o
  oooo                   o  o
 o                         o
    o
  o
  o                       oo
 o                          o
   o
     o
                         o
      o
   oo                    oo  o
                           o
   o  o  o                o
    o                   o o
    o         o  o  oo
       oo         o oo    o
     o   o      o  o
      o o      o oo   o
              o   o
```




## Falcon


```falcon

// Generate points in [min,max]^2 with constraint
function random_point (min, max, constraint)
  [x, y] = [random(min, max), random(min, max)]
  return constraint(x, y) ? [x, y] : random_point(min, max, constraint)
end

// Generate point list
in_circle = { x, y => 10**2 <= x**2 + y**2 and x**2 + y**2 <= 15**2 }
points = [].comp([0:100], {__ => random_point(-15, 15, in_circle)})

// Show points
for i in [-15:16]
  for j in [-15:16]
    >> [i, j] in points ? "x" : " "
  end
  >
end

```

Example output:

```txt

          xxx  x
        xx x  x  xx
          x    xx      xx
      x x     x x x x x x
              x      x
     xx   x          x
   x    x            x
     xx
  x  x                   xx
      x                 x
                         x   x
 x                        xx
                             x

                           xx
     x                   xx
 x                       xx  x
    x

  xx  x                     xx
  xx                    x   x

         x                 x
       x x           x    x
         x     x   x
     x     x x   x  x    x
                    x x
              x x x x

```



## Fortran

```fortran
program Constrained_Points
  implicit none

  integer, parameter :: samples = 100
  integer :: i, j, n, randpoint
  real :: r

  type points
    integer :: x, y
  end type

  type(points) :: set(500), temp

! Create set of valid points
  n = 0
  do i = -15, 15
    do j = -15, 15
      if(sqrt(real(i*i + j*j)) >= 10.0 .and. sqrt(real(i*i + j*j)) <= 15.0) then
        n = n + 1
        set(n)%x = i
        set(n)%y = j
      end if
    end do
  end do

! create 100 random points
! Choose a random number between 1 and n inclusive and swap point at this index with point at index 1
! Choose a random number between 2 and n inclusive and swap point at this index with point at index 2
! Continue in this fashion until 100 points have been selected
  call random_seed
  do i = 1, samples
    call random_number(r)
    randpoint = r * (n + 1 - i) + i
    temp = set(i)
    set(i) = set(randpoint)
    set(randpoint) = temp
  end do

! In order to facilitate printing sort random points into ascending order
! sort x in ascending order
  do i = 2, samples
     j = i - 1
     temp = set(i)
        do while (j>=1 .and. set(j)%x > temp%x)
           set(j+1) = set(j)
           j = j - 1
        end do
     set(j+1) = temp
  end do

! sort y in ascending order for same x
  do i = 2, samples
     j = i - 1
     temp = set(i)
        do while (j>=1 .and. set(j)%x == temp%x .and. set(j)%y > temp%y)
           set(j+1) = set(j)
           j = j - 1
        end do
     set(j+1) = temp
  end do

! print circle
  write(*,"(a,a)", advance="no") repeat(" ", set(1)%y+15), "*"
  do i = 2, samples
    if(set(i)%x == set(i-1)%x) then
      write(*,"(a,a)", advance="no") repeat(" ", set(i)%y - set(i-1)%y-1), "*"
    else
      n = set(i)%x - set(i-1)%x
      do j = 1, n
        write(*,*)
      end do
      write(*,"(a,a)", advance="no") repeat(" ", set(i)%y+15), "*"
    end if
  end do

end program
```

Output

```txt

                  * *
        *   *         *
      ** **    * **    *
                **
     *  **   * **    *
   ***   **               *
    *    *           *
                           *
  *                      *  *
     **                   *
   *                      *
  *  *                   *
    *                        *
    *
    **                   ***  *
    *                    *
    **                   *
    *                    *
   *                      *
  *   *                      *
      **                 *  *
    * *                *  **
   ** * *               *
     *
          ***         * * *
     **         *   *    *
         *  * * *
         *     *  ** *
            *
```


## gnuplot

[[File:RingRandPntsGnu.png|right|thumb|Output RingRandPntsGnu.png]]


```gnuplot

## Ring of random points 2/18/17 aev
reset
fn="RingRandPntsGnu";
ttl="Ring of random points"
ofn=fn.".png"; lim=1000;
randgp(top) = floor(rand(0)*top)
set terminal png font arial 12 size 640,640
set output ofn
set title ttl font "Arial:Bold,12"
unset key;
set size square
set parametric
set xrange [-20:20]; set yrange [-20:20];
set style line 1 lt rgb "red"
$rring << EOD
EOD
set print $rring append
do for [i=1:lim] {
  x=randgp(30); y=randgp(30);
  r=sqrt(x**2+y**2);
  if (r>=10&&r<=15) \
    {print x," ",y; print -x," ",-y;print x," ",-y; print -x," ",y;}
}
plot [0:2*pi] sin(t)*10,cos(t)*10, sin(t)*15,cos(t)*15 ls 1,\
$rring using 1:2 with points  pt 7 ps 0.5 lc "black"
set output
unset print

```

```txt

File: RingRandPntsGnu.png

```



## Go

'''Algorithm 1:'''

```go
package main

import (
    "bytes"
    "fmt"
    "math/rand"
    "time"
)

const (
    nPts = 100
    rMin = 10
    rMax = 15
)

func main() {
    rand.Seed(time.Now().Unix())
    span := rMax + 1 + rMax
    rows := make([][]byte, span)
    for r := range rows {
        rows[r] = bytes.Repeat([]byte{' '}, span*2)
    }
    u := 0 // count unique points
    min2 := rMin * rMin
    max2 := rMax * rMax
    for n := 0; n < nPts; {
        x := rand.Intn(span) - rMax
        y := rand.Intn(span) - rMax
        // x, y is the generated coordinate pair
        rs := x*x + y*y
        if rs < min2 || rs > max2 {
            continue
        }
        n++ // count pair as meeting condition
        r := y + rMax
        c := (x + rMax) * 2
        if rows[r][c] == ' ' {
            rows[r][c] = '*'
            u++
        }
    }
    for _, row := range rows {
        fmt.Println(string(row))
    }
    fmt.Println(u, "unique points")
}
```

'''Algorithm 2:'''

```go
package main

import (
    "bytes"
    "fmt"
    "math/rand"
    "time"
)

const (
    nPts = 100
    rMin = 10
    rMax = 15
)

func main() {
    rand.Seed(time.Now().Unix())
    var poss []struct{ x, y int }
    min2 := rMin * rMin
    max2 := rMax * rMax
    for y := -rMax; y <= rMax; y++ {
        for x := -rMax; x <= rMax; x++ {
            if r2 := x*x + y*y; r2 >= min2 && r2 <= max2 {
                poss = append(poss, struct{ x, y int }{x, y})
            }
        }
    }
    fmt.Println(len(poss), "possible points")
    span := rMax + 1 + rMax
    rows := make([][]byte, span)
    for r := range rows {
        rows[r] = bytes.Repeat([]byte{' '}, span*2)
    }
    u := 0
    for n := 0; n < nPts; n++ {
        i := rand.Intn(len(poss))
        r := poss[i].y + rMax
        c := (poss[i].x + rMax) * 2
        if rows[r][c] == ' ' {
            rows[r][c] = '*'
            u++
        }
    }
    for _, row := range rows {
        fmt.Println(string(row))
    }
    fmt.Println(u, "unique points")
}
```

```txt

404 possible points

                            * *       *
                          *           * *
                    *     * *       * *
            *   *   * * *             *   *
          *   *     *   *     *     *       *
                                              * * *
            *   *                                 * * *
    * *   *                                       *
    * *   *                                     * *
        *
      *                                             *     *

      *                                               * *
      * *
* *                                                       *
    *

      * *
    *                                               *   *
      *                                           *
                                                    *
    *       *
          * *                                       *
              *                         *
                  * *         *     *   *     * *
            *     *   * *
                            *     *     *     *
                      *   *       *
                      *
                              *
90 unique points

```



## Haskell

Using [[Knuth shuffle#Haskell|Knuth Shuffle]]

```haskell
import Data.List
import Control.Monad
import Control.Arrow
import Rosetta.Knuthshuffle

task = do
  let blanco = replicate (31*31) "  "
      cs = sequence [[-15,-14..15],[-15,-14..15]] :: [[Int]]
      constraint = uncurry(&&).((<= 15*15) &&& (10*10 <=)). sum. map (join (*))
-- select and randomize all circle points
  pts <- knuthShuffle $ filter constraint cs
-- 'paint' first 100 randomized circle points on canvas
  let canvas = foldl (\cs [x,y] -> replaceAt (31*(x+15)+y+15) "/ " cs ) blanco (take 100 pts)
-- show canvas
  mapM_ (putStrLn.concat). takeWhile(not.null). unfoldr (Just . splitAt 31) $ canvas
```

Output (added a trailing space per 'pixel'

```txt
*Main> task

                      /             / /
                          /     /       /
              /                 /
          /     /               /       / / /   /
            /                 / /       /
      /     /     /                       /   /
      /         /                           /
        /   / /                                     /   /
        / / /                                 /       / /
                                                    /
    /     /                                       /
      / /                                             /
      /                                               /   /
        /
/ /                                                   /     /
                                                      /   /
                                                      /   /
      /
      /                                             /
  /                                                   /
                                                /     /
    /     /                                       /
              / /                         / /         /
        /   /       /                             /
                          / /     / / /   / /
          / /             /                   /
                            / /
                /   /   / / /   /       / /
                      /             / /
```



## Hy


```lisp
(import
  [math [sqrt]]
  [random [choice]]
  [matplotlib.pyplot :as plt])

(setv possible-points (list-comp (, x y)
  [x (range -15 16) y (range -15 16)]
  (<= 10 (sqrt (+ (** x 2) (** y 2))) 15)))

(setv [xs ys] (apply zip (list-comp (choice possible-points) [_ (range 100)])))
  ; We can't use random.sample because that samples without replacement.
(plt.plot xs ys "bo")
(plt.show)
```


=={{header|Icon}} and {{header|Unicon}}==
Generate random points in the bounded by the outside edge. Reject any found out of the prescribed bounds and stop when the required numbers of points have been generated. [[File:Fuzzycircle-unicon.PNG|thumb|plot of 2000, 100, 120]]

```Icon
link graphics

procedure main(A)  # points, inside r, outside r in pixels - default to task values

if \A[1] == "help" then stop("Usage: plot #points inside-radius outside-radius")
points  := \A[1] | 100
outside := \A[2] | 15
inside  := \A[3] | 10
if inside > outside then inside :=: outside

wsize   := integer(2.2*outside)
wsize  <:= 150
center  := wsize/2

WOpen("size="||wsize||","||wsize,"bg=black","fg=white") | stop("Unable to open window")

until(points -:= 1) <= 0 do {
   x := ?(2*outside)-outside   # random x
   y := ?(2*outside)-outside   # and y
   if (inside <= integer(sqrt(x^2+y^2)) ) <= outside then
      DrawPoint(x + center,y + center)
   }
WDone()

end
```



## J


This version deals 100 distinct coordinates from the set of acceptable coordinates (much like dealing cards from a shuffled deck):


```j
gen=: ({~ 100?#)bind((#~ 1=99 225 I.+/"1@:*:),/,"0/~i:15)
```


Example use (<code><nowiki>gen''</nowiki></code> generates the points, the rest of the example code deals with rendering them as a text array):

```j
   '*' (<"1]15+gen '')} 31 31$' '

          *
               *
           *  *  * * * *
     *   *      *  *   *
                   *   ***
    **    *         *     **
    * **
       *                   **
  * *                  *   *
 *   *                  **
 * **                       **
 ** *                      ***
 * **                    *   *
 **                        *
    *                    **  *
 **  *
 * **                       *
 *                           *
    *                        *
  *                       *
   **  *
       *                  **
      *
      * *            *    *
     *       ** *     * *
       *                *
       **
         *   *       *
            **  *
```


## Java


```java
import java.util.Random;

public class FuzzyCircle {
	static final Random rnd = new Random();
	public static void main(String[] args){
		char[][] field = new char[31][31];
		for(int i = 0; i < field.length; i++){
			for(int j = 0; j < field[i].length; j++){
				field[i][j] = ' ';
			}
		}
		int pointsInDisc = 0;
		while(pointsInDisc < 100){
			int x = rnd.nextInt(31) - 15;
			int y = rnd.nextInt(31) - 15;
			double dist = Math.hypot(x, y);
			if(dist >= 10 && dist <= 15 && field[x + 15][y + 15] == ' '){
				field[x + 15][y + 15] = 'X';
				pointsInDisc++;
			}
		}
		for(char[] row:field){
			for(char space:row){
				System.out.print(space);
			}
			System.out.println();
		}
	}
}
```

Output:

```txt

            XX X
        X X    X  X   X
       X        XX  X
     XXXX      X        X
    X    X      X  XXX
        X             X
    X                 X  XXX
    X                     X
       X               X   XX
 X   X                       X
    XX                    X
                           X

 X                         X
 XXXXX                   X   X
                          X X
                         X   X
                          X X
 X   X                    X
 X                        X
    XX                 X
    XX                  X
        X            XX   X
     X XX              X
    X       X X  X      X
     XX   X     X      XXX
        X    X X X     XX
            X         X
               X
               X
```



## JavaScript

JavaScript embedded in HTML, using canvas:

```javascript
<html><head><title>Circle</title></head

<body>
<canvas id="cv" width="320" height="320"></canvas>
<script type="application/javascript">

var cv = document.getElementById('cv');
var ctx = cv.getContext('2d');

var w = cv.width;
var h = cv.height;

//draw circles
ctx.fillStyle = 'rgba(0, 255, 200, .3)';
ctx.strokeStyle = 'rgba(0,0,0,.1)';
ctx.beginPath();
ctx.arc(w/2, h/2, 150, 0, Math.PI*2, true);
ctx.arc(w/2, h/2, 100, 0, Math.PI*2, false);
ctx.closePath();
ctx.fill();

// draw grids
ctx.beginPath();
for (var i = 10; i < w; i += 10) {
	ctx.moveTo(i, 0);
	ctx.lineTo(i, h);
	ctx.moveTo(0, i);
	ctx.lineTo(w, i);
}
ctx.closePath();
ctx.stroke();

//draw points
ctx.fillStyle = 'navy';
var pts = 0;
while (pts < 100) {
	var x = Math.floor(Math.random() * 31) - 15;
	var y = Math.floor(Math.random() * 31) - 15;
	var r = x * x + y * y;
	if (r < 100 || r > 225) continue;
	x = x * 10 + w/2;
	y = y * 10 + h/2;
	ctx.fillRect(x - 2, y - 2, 4, 4);
	pts++;
}

</script></body></html>
```



## Julia

This solution uses the "pick random x, y and cull" rather than the "calculate valid and choose randomly" approach.

```julia
function printcircle(lo::Integer, hi::Integer, ndots::Integer; pad::Integer = 2)
    canvas = falses(2hi + 1, 2hi + 1)
    i = 0
    while i < ndots
        x, y = rand(-hi:hi, 2)
        if lo ^ 2 - 1 < x ^ 2 + y ^ 2 < hi ^ 2 + 1
            canvas[x + hi + 1, y + hi + 1] = true
            i += 1
        end
    end
    # print
    for i in 1:(2hi + 1)
        row = map(j -> j ? "\u25cf " : "  ", canvas[i, :])
        println(" " ^ pad, join(row))
    end
    return canvas
end

printcircle(10, 15, 100)
```


```txt


                          ●                 ●
                          ●     ● ● ●           ●
                  ●         ● ●                 ●
                ●           ●                 ●     ●
                    ●           ●               ● ●
                  ●                                 ●
                    ●                         ●       ●
        ●       ●                                     ●
        ●                                               ●
                                                        ● ● ● ●
              ●                                       ●
          ●
        ● ● ●                                             ●   ●
                                                        ●     ●

          ●                                             ●   ●
                                                          ●
      ●                                                 ● ●
            ●                                         ●
      ●   ●
          ● ●     ●                                 ●       ●
                ● ●                                   ●   ●
                      ●
            ● ●       ● ●                       ●
              ●         ● ● ● ●   ●             ● ● ● ●
                ●   ●           ●   ● ● ●
                            ● ●   ●
                        ●             ●
                            ● ●


```



## Kotlin


```scala
// version 1.1.3

fun main(args: Array<String>) {
    val r = java.util.Random()
    val points = Array(31) { CharArray(31) { ' ' } }
    var count = 0
    while (count < 100) {
        val x = r.nextInt(31) - 15
        val y = r.nextInt(31) - 15
        val h = x * x + y * y
        if (h in 100..225) {
            points[x + 15][y + 15] = 'o'
            count++
        }
    }
    for (i in 0..30) println(points[i].joinToString(""))
}
```


Sample output:

```txt

            ooo oo  o
           o
               oo   oo
       o      oo      o
      o   ooo oo
                    o    oo
     o  o               o
     o o                 oo o
     o o
                         o o
     o                   ooo
    o                    oo
  o oo
  o                       o
  o                         o o
  o o
     o                   ooo
 o  o                    o
   o
 oo                      o  oo
   oo                      o
   o  o                 o

                     ooo o
     oo               o
     o         o   o
          oo  o     o
                    o o
                 o  o

```



## Liberty BASIC


```lb
'   RC Constrained Random Points on a Circle

nomainwin

WindowWidth  =400
WindowHeight =430

open "Constrained Random Points on a Circle" for graphics_nsb as #w

#w "trapclose [quit]"
#w "down ; size 7 ; color red ; fill black"

for i =1 to 1000
  do
     x =int( 30 *rnd( 1)) -15
     y =int( 30 *rnd( 1)) -15
  loop until IsInRange( x, y) =1
  #w "set "; 200 +10 *x; " "; 200 - 10 *y
next

wait

function IsInRange( x, y)
  z =sqr( x*x +y*y)
  if 10 <=z and z <=15 then IsInRange =1 else IsInRange =0
end function

[quit]
close #w
end
```



## Locomotive Basic



```locobasic
10 MODE 1:RANDOMIZE TIME
20 FOR J=1 TO 100
30 X=INT(RND*30-15)
40 Y=INT(RND*30-15)
50 D=X*X+Y*Y
60 IF D<100 OR D>225 THEN GOTO 40
70 PLOT 320+10*X,200+10*Y:LOCATE 1,1:PRINT J
80 NEXT
90 CALL &BB06 ' wait for key press
```


[[File:Points on a circle locomotive basic.png]]


## Maple


```maple
 a := table():
 i := 1:
 while i < 100 do
 ba := (rand(-15 .. 15))():
 bb := (rand(-15 .. 15))():
 b := evalf(sqrt(ba^2+bb^2)):
 if b <= 15 and b >= 10
  then a[i] := [ba, bb]:
  i := i+1:
 end if:
 end do:
 plots:-pointplot(convert(a,list));
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
This algorithm generates 500 pairs of random integers between +/- 15, picks out the ones that satisfy the inequality, and then takes the first 100 of those. It oversamples to reduce the chance of having less than 100 "candidates", which is not impossible, though extremely unlikely.

```Mathematica
sample = Take[Cases[RandomInteger[{-15, 15}, {500, 2}], {x_, y_} /; 10 <= Sqrt[x^2 + y^2] <= 15], 100];

Show[{RegionPlot[10 <= Sqrt[x^2 + y^2] <= 15, {x, -16, 16}, {y, -16, 16}, Axes -> True], ListPlot[sample]}]
```



## MATLAB

Uses the Monte-Carlo method described above.


```MATLAB
function [xCoordinates,yCoordinates] = randomDisc(numPoints)

    xCoordinates = [];
    yCoordinates = [];

    %Helper function that samples a random integer from the uniform
    %distribution between -15 and 15.
    function nums = randInt(n)
        nums = round((31*rand(n,1))-15.5);
    end

    n = numPoints;

    while n > 0

        x = randInt(n);
        y = randInt(n);

        norms = sqrt((x.^2) + (y.^2));
        inBounds = find((10 <= norms) & (norms <= 15));

        xCoordinates = [xCoordinates; x(inBounds)];
        yCoordinates = [yCoordinates; y(inBounds)];

        n = numPoints - numel(xCoordinates);
    end

    xCoordinates(numPoints+1:end) = [];
    yCoordinates(numPoints+1:end) = [];

end
```


Output:

```matlab
>
 [x,y] = randomDisc(100);
>> plot(x,y,'.')
```

[[File:Matlab-randomDisc-output.png]]


## Maxima


```Maxima
randomDisc(numPoints):= block([p: []],
  local(goodp, random_int),
  goodp(x, y):=block([r: sqrt(x^2+y^2)],
    r>=10 and r<=15
    ),
  random_int():= block([m: 15], m - random(2*(m+1)-1)),
  while length(p)<numPoints do block (
    [x: random_int(), y : random_int()],
    if goodp(x, y) then (
      p: cons([x, y], p)
      )
    ),
  p)$

p: randomDisc(100)$
plot2d(['discrete, p], ['style, 'points]);
```



## Nim

```nim
import tables, math, strutils, complex, random

proc random[T](a: openarray[T]): T =
  result = a[rand(low(a)..len(a))]

type Point = tuple[x, y: int]

var world = initCountTable[Point]()
var possiblePoints = newSeq[Point]()

for x in -15..15:
  for y in -15..15:
    if abs((x.float, y.float)) in 10.0..15.0:
      possiblePoints.add((x,y))

randomize()
for i in 0..100: world.inc possiblePoints.random

for x in -15..15:
  for y in -15..15:
    let key = (x, y)
    if key in world and world[key] > 0:
      stdout.write min(9, world[key])
    else:
      stdout.write ' '
  echo ""
```

Output:

```txt
               1
          1211      1

      1 1 3       1   1
                3     11
    1  11    1   1    1
       122           21  1

                        1 1 2
                        1
      1                   1
 11                      11
   1 1                   1
   1
  11                      1
  1  1                      11
 2   1
                           1
     1                       1
                         1 1
                           11
                       11
    1 1                   111
    1                1 1 1
       1            11
                 1 1     1
     12 2        11   1  1
              1 1 1 1 1
        11 1
              1
```



## OCaml



```ocaml
let p x y =
  let d = sqrt(x ** 2.0 +. y ** 2.0) in
  10.0 <= d && d <= 15.0

let () =
  Random.self_init();
  let rec aux i acc =
    if i >= 100 then acc else
      let x = (Random.float 40.0) -. 20.0
      and y = (Random.float 40.0) -. 20.0 in
      if (p x y)
      then aux (succ i) ((x,y)::acc)
      else aux i acc
  in
  let points = aux 0 [] in
  let g = Array.init 40 (fun _ -> String.make 40 ' ') in
  List.iter (fun (x,y) ->
    let x = (int_of_float x) + 20
    and y = (int_of_float y) + 20 in
    g.(y).[x] <- 'o'
  ) points;
  Array.iter print_endline g
```



                 o   o     o
                 o
            oo      oo     oooo
                o      o   oo o
         oo
           o                o   o
          o
       oo  o
        oo o                 oo  o
                               oo
                               oo
          o                     o
       oooo                     o o
         oo                  o o
         o                    oo
                                  o
       o  o                  o  o
         o  o                 o o
        o  o
           o                   o
         o    o              oo
             o             oo
          o            o
              ooo o o       o
              o  ooo     o
               o
                  oo  o



## PARI/GP


```parigp
crpc()={
  my(v=vector(404),t=0,i=0,vx=vy=vector(100));
  for(x=1,14,for(y=1,14,
    t=x^2+y^2;
    if(t>99&t<226,
      v[i++]=[x,y];
      v[i++]=[x,-y];
      v[i++]=[-x,y];
      v[i++]=[-x,-y]
    )
  ));
  for(x=10,15,
    v[i++]=[x,0];
    v[i++]=[-x,0];
    v[i++]=[0,x];
    v[i++]=[0,-x]
  );
  for(i=1,#vx,
    t=v[random(#v)+1];
    vx[i]=t[1];
    vy[i]=t[2];
  );
  plothraw(vx,vy)
};
```



## Perl


### Graphical output


```perl
my @points;
while (@points < 100) {
        my ($x, $y) = (int(rand(31))-15, int(rand(31)) - 15);
        my $r2 = $x*$x + $y*$y;
        next if $r2 < 100 || $r2 > 225;
        push @points, [$x, $y];
}

print << 'HEAD';
%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox 0 0 400 400
200 200 translate 10 10 scale
0 setlinewidth
1 0 0 setrgbcolor
0 0 10 0 360 arc stroke
0 0 15 360 0 arcn stroke
0 setgray
/pt { .1 0 360 arc fill } def
HEAD

print "@$_ pt\n" for @points;
print "%%EOF";
```


Randomly generates points and reject ones not in the ring.  Writes an EPS file.

===Plain-text output===

```perl
@range = -15..16;

for $x (@range) {
    for $y (@range) {
        $radius = sqrt $x**2 + $y**2;
        push @points, [$x,$y] if 10 <= $radius and $radius <= 15
    }
}

push @sample, @points[int rand @points] for 1..100;
push @matrix, ' ' x @range for 1..@range;
substr $matrix[15+$$_[1]], 15+$$_[0], 1, '*' for @sample;
print join(' ', split '', $_) . "\n" for @matrix;
```

```txt
                                  *   * *
                              *     *   *
                        *     *   *
                                  *   *
          *                 * *               *
                *                         *
      *       * *                                 *
    *                                             * *
          *   *
  *                                                       *
      *   *                                       * * * * *
                                                      *

        *                                         * *
      *
  *                                                 *   * *
                                                        * *
    * *                                           *
  *                                                     * *
  * *     *                                             *
        * *   *
        *                                     *   *     *
                *                         *
              *                             *
                              *         *       *   *
          * *       *       *           *
            *   *     *                   *   *
                  *       *
                          *
```



## Perl 6

```perl6
my @range = -15..16;

my @points = gather for @range X @range -> ($x, $y) {
    take [$x,$y] if 10 <= sqrt($x*$x+$y*$y) <= 15
}
my @samples = @points.roll(100); # or .pick(100) to get distinct points

# format and print
my %matrix;
for @range X @range -> ($x, $y) { %matrix{$y}{$x} = ' ' }
%matrix{.[1]}{.[0]} = '*' for @samples;
%matrix{$_}{@range}.join(' ').say for @range;
```

```txt
                                  *
                    *                 *
              *     *   *         *         *
            * *         *               *
                      *           *       *   *     *
              *   *
            *     *                                   *
      *     *                                 *       *
                                              *
            *                                   * *   *
      *   *                                         *
      * *                                         *     *
      *
  *     *
                                                      * * *
  * *   * *
      *                                             *   *
    *
    *     *
  *     *
            * *                                       *
      *                                           *   *
                                          *         *
          *                                   *       *
              *         *       *         * * *     *
          *   *     *                   *     * *
                              *     *   *   *
                  *                 *
                                *     * *
```

Turning that program completely inside-out and reducing to a single statement with a single non-parameter variable, we get another version that also works.

This uses, among other things, a 0-based matrix rather than a hash, a <tt>given</tt> on the first line that allows us to print the final value of the matrix straight from its initial declaration, a <tt>for</tt> statement feeding a <tt>for</tt> statement modifier, a lambda that unpacks a single x-y argument into two variables, the functional form of pick rather than the method form, a quasi-list comprehension in the middle loop that filters each <tt>given</tt> with a <tt>when</tt>, precalculated squared limits so we don't have to take the square root, use of X- and X** to subtract and exponentiate both <tt>$x</tt> and <tt>$y</tt> in parallel.

After the <tt>given do</tt> has loaded up <tt>@matrix</tt> with our circle, the <tt>map</tt> on the first line substitutes a space for any undefined matrix element, and the extra space between elements is supplied by the stringification of the list value, performed by the prefix <tt>~</tt> operator, the unary equivalent of concatenation in Perl 6.

At this point you would be justified in concluding that we are completely mad.  <tt>:-)</tt>


```perl6
(say ~.map: { $_ // ' ' } for my @matrix) given do
    -> [$x, $y] { @matrix[$x][$y] = '*' } for pick 100, do
        for ^32 X ^32 -> ($x, $y) {
            [$x,$y] when 100..225 given [+] ($x,$y X- 15) X** 2;
        }

```

```txt
                       *   *     *
                        * *     *
                          *       * * *   *     *
                    *   * * *         *         *
        *     *             *         *     *     *
          *     *                             *
      *                                   * * *   *
          * *                                 *     *
      * *
                                                    *   *
                                                    *
  *                                               *     *

    *                                               *
* *                                                 * * *   *
      *   *

        *                                         * *   *
          *                                         * *
      * *   *                                         *
                                                  *   *
    *   *
        * *                                     *
      * *   *                           * *           *
              *               *         *         *
              * *       *   *           *     *
              * * *                         *
                  * *   *       * *
                    *       * *
```



## Phix


```Phix
sequence screen = repeat(repeat(' ',31),31)
integer x, y, count = 0
atom r
while 1 do
    x = rand(31)
    y = rand(31)
    r = sqrt(power(x-16,2)+power(y-16,2))
    if r>=10 and r<=15 then
        screen[x][y] = 'x'
        count += 1
        if count>=100 then exit end if
    end if
end while
puts(1,join(screen,"\n"))
```

```txt

                x  xx
              x   x   x
         x x   x  x    x
          x     x   x   x
     x     x      xx
    x                x x  x
   xx xx              x
                         x
       x
  x
  x                       x
                           x
    x                    x  x
 xx                      xxxx
    x                       x
                          x  x
                           x
  x x                     x
     x
  x                       x
  xx x                   x x
   x                     x
        x             x   x
       x  x         x   x
          x x xx          x
      x  x       x  xx xx
                   xx x
          x    xx
             x   x  x

```



## PicoLisp


```PicoLisp
(let Area (make (do 31 (link (need 31 " "))))
   (use (X Y)
      (do 100
         (until
            (>=
               15
               (sqrt
                  (+
                     (* (setq X (rand -15 15)) X)
                     (* (setq Y (rand -15 15)) Y) ) )
               10 ) )
         (set (nth Area (+ 16 X) (+ 16 Y)) "#") ) )
   (mapc prinl Area) )
```

Output:

```txt
           #
        ##
           #  #  #  #
         ## #    #  #   #
       #      #   # # #
    #       #     # #   #
      #   #         #    #
      #                #   #
                       #
       #               #
     ##                      #
                          #
   #                      # #
  ###                      #
 #                         # #
##                          # #
  #                        #
 #                        #
 ## #
                           #
   #
     #                 #
     #
   ###                  #   #
      ###           # #    #
    #      #
         #  #   ##
                 # #
      #     #           #
                # #  #
```



## PL/I


### version 1


```PL/I

constrain: procedure options (main);
   declare 1 point (100),
            2 x fixed binary,
            2 y fixed binary;
   declare (i, j, a, b, c) fixed binary;

   j = 0;
   do i = 1 to 100;
      a = 30*random()-15; b = 30*random()-15;
      c = sqrt(a**2 + b**2);
      if abs(c) >= 10 & abs(c) <= 15 then
         do; j = j + 1; x(j) = a; y(j) = b; end;
   end;

   /* PLOT */
   declare table(-15:15, -15:15) character (1);
   table = ' ';
   do i = 1 to j;
      table(x(i), y(i)) = '*';
   end;
   do i = -15 to 15;
      put skip;
      do j = -15 to 15;
         put edit (table(i,j)) (a);
      end;
   end;
end constrain;

```

Output:

```txt

        **  *
                *   *

    **
         **

                         *
                           ***
      *


                         *
    **

                           *
 * **


                        * *
  **                        *
     * *

                     ***
                         ***
       ***
            *
                     *

```


### version 2


```PL/I
*process source attributed xref or(!);
 annulus: procedure options (main);
 /* version 1 does not handle (0/15) etc. this does. */
 /* we show 1000 points here                         */
   declare 1 point(10000),
            2 x fixed binary,
            2 y fixed binary;
   declare (i, j, a, b, a2, b2, c) fixed binary(31);
   j = 0;
   do i = 1 to 1000;
      r=rand(31); a=r-16;
      r=rand(31); b=r-16;
      a2=a*a;
      b2=b*b;
      c2=a2+b2;
      if c2>= 100 & c2 <= 225 then
         do; j = j + 1; x(j) = a; y(j) = b;
         /* put Edit(a,b,c)(3(F(3))); */ end;
   end;
   /* PLOT */
   declare table(-15:15, -15:15) character (2);
   table = ' ';
   do i = 1 to j;
      table(x(i), y(i)) = '*';
   end;
   do i = -15 to 15;
      put skip;
      do j = -15 to 15;
         put edit (table(i,j)) (a);
      end;
   end;

 rand: Proc(n) Returns(Bin Fixed(31));
 /*--------------------------------------------------------------------
 * Return a random integer between 1 and n
 *-------------------------------------------------------------------*/
 Dcl r Bin Float(31);
 Dcl (n,d) Bin Fixed(31);
 r=random();
 d=r*n+1;
 Return(d);
 End;
 End annulus;
```

'''output'''

```txt
                             *
                      * *   *   * *   * *
                  * * * * *   * *   * * *
            * * * * *   * * * *     * *   * * * *
          * * * * * * * *       * * * *   * * * *
        * * * * * *     * *   *     * *       * * * *
      *     * *     *                   * * *     *   *
          *   * *                           * *   *   *
        * *   *                               * *   * * *
      * * * * *                               * *     * *
        *   *                                     *     * *
  *   * * *                                       * *   * *
  * * *   *                                       *   *   *
  * * * *                                         * * * * *
    *   *                                         * *     *
* * * *   *                                       * *   * * *
  * * *                                           * * *
    *   * *                                       *   * *
    *   * *                                       *   * * *
    * * *                                         * * *   *
  *   * *   *                                     *     * *
      * * * * *                                         *
      * *                                     * * * * * *
      * *   * * * *                       * * * *     *
          * * * *   *                     * *     * * *
        * * * * *   * *   *     * *   *   *   *     *
          *       * * * *   * *   * * * *   *   *
              * *   *   * * *   *     * * * *   *
                  * * * *     * *     * *
                        * * *   *     * *

```



## PowerShell

```PowerShell
$MinR2 = 10 * 10
$MaxR2 = 15 * 15

$Points = @{}

While ( $Points.Count -lt 100 )
    {
    $X = Get-Random -Minimum -16 -Maximum 17
    $Y = Get-Random -Minimum -16 -Maximum 17
    $R2 = $X * $X + $Y * $Y

    If ( $R2 -ge $MinR2 -and $R2 -le $MaxR2 -and "$X,$Y" -notin $Points.Keys )
        {
        $Points += @{ "$X,$Y" = 1 }
        }
    }

ForEach ( $Y in -16..16 ) { ( -16..16 | ForEach { ( " ", "*" )[[int]$Points["$_,$Y"]] } ) -join '' }
```

```txt
            ***
           *     **   *
           *     ***     *
           * * * ** *    *
     *    *    *     *
           *          *     *
    * *               ***  *
    *                    *****
      **                * *
   * *                    ***
  ** *                    ***
  *                        * *
     *
  *                       *   *
      *
    *
    **                        *
    * *                    **
     *
                           *

        *                **
         *            * * *
    * *   **           *
      *   **          **   *
        * * *       * *
         *
                      **
             ** *
```



## Prolog

Works with SWI-Prolog

```Prolog
:- use_module(library(clpfd)).

circle :-
	bagof([X,Y], init(X,Y), BL),
	length(BL, N),
	length(L, 100),
	maplist(choose(BL, N), L),
	draw_circle(L).


% point selection
choose(BL, N, V) :-
	I is random(N),
	nth0(I, BL, V).

% to find all couples of numbers verifying
% 100 <= x^2 + y^2 <= 225
init(X1, Y1) :-
	X in -15..15,
	Y in -15..15,
	X*X + Y*Y #>= 100,
	X*X + Y*Y #=< 225,
	label([X,Y]),
	X1 is 10 * X + 200,
	Y1 is 10 * Y + 200.


draw_circle(L) :-
	new(D, window('Circle')),
	send(D, size,size(400,400)),
	forall(member([X,Y], L),
	       (   new(C, circle(4)),
		   send(C, fill_pattern, colour(@default, 0, 0, 0)),
		   send(C, center(point(X,Y))),
		   send(D, display, C))),
	send(D, open).

```

[[FILE:Prolog-Circle.jpg‎ ]]


## PureBasic


```PureBasic
CreateImage(0,31,31)
StartDrawing(ImageOutput(0))
  For i=1 To 100
    Repeat
      x=Random(30)-15
      y=Random(30)-15
      R.f=Sqr(x*x+y*y)
    Until 10<=R And R<=15
    Plot(x+15,y+15,#Red)
  Next
StopDrawing()

Title$="PureBasic Plot"
Flags=#PB_Window_SystemMenu
OpenWindow(0,#PB_Ignore,#PB_Ignore,ImageWidth(0),ImageHeight(0),Title$,Flags)
ImageGadget(0,0,0,ImageWidth(0),ImageHeight(0),ImageID(0))
Repeat: Until WaitWindowEvent()=#PB_Event_CloseWindow
```

[[File:PureBasic_Circle_plot.png‎|155px]]


## Python

Note that the diagram shows the number of points at any given position (up to a maximum of 9 points).

```python
>>
 from collections import defaultdict
>>> from random import choice
>>> world = defaultdict(int)
>>> possiblepoints = [(x,y) for x in range(-15,16)
		  for y in range(-15,16)
		  if 10 <= abs(x+y*1j) <= 15]
>>> for i in range(100): world[choice(possiblepoints)] += 1

>>> for x in range(-15,16):
	print(''.join(str(min([9, world[(x,y)]])) if world[(x,y)] else ' '
			  for y in range(-15,16)))



             1     1
          1 1
      11 1     1  1     1
     111  1     1211
      1   2    1 1    11
      1  11         21
     1   1            11  1
   1  2                1 1

 1  2
   1 1                      1
   1 1
   2                      11
  1                         1
                         1


  1                          1
                         1
                         2
                            1
     1                  1 1
      1                2   1
   1   3            11  2
    11   1    1      1   2
            1   1    2
        1  1
         1      1     1
          2 2   1
               1
```

If the number of samples is increased to 1100:

```python
>>
 for i in range(1000): world[choice(possiblepoints)] += 1

>>> for x in range(-15,16):
	print(''.join(str(min([9, world[(x,y)]])) if world[(x,y)] else ' '
			  for y in range(-15,16)))


               2
          41341421333
        5133333131253 1
      5231514 14214721 24
     326 21222143234122322
    54235153132123344125 22
   32331432         2422 33
   5453135           4144344
  132595               323123
  4 6353               432224
 5 4323                 3 5313
 23214                   41433
 42454                   33342
 332 4                   34314
 142 1                   35 53
124211                   53131
 22221                   152 4
 22213                   34562
 654 4                   4 212
 24354                   52232
 544222                 283323
  411123               453325
  251321               124332
   2124134           2443226
   2 113315         64324334
    2412452 324 32121132363
      4222434324635 5433
      3113333123432112633
        2131181233  424
          47414232164
               4
```



## R


```R

RMin <- 10
RMax <- 15
NPts <- 100

# instead of a for loop, we generate what should be enough points
# also take care to have enough range to avoid rounding inaccuracies
nBlock <- NPts * ((RMax/RMin) ^ 2)
nValid <- 0
while (nValid < NPts) {
	X <- round(runif(nBlock, -RMax - 1, RMax + 1))
	Y <- round(runif(nBlock, -RMax - 1, RMax + 1))
	R <-  sqrt(X^2 + Y^2)
	Valid <- ( (R >= RMin) & (R <= RMax) )
	nValid <- sum(Valid)
	nBlock <- 2 * nBlock
}
plot(X[Valid][1:NPts],Y[Valid][1:NPts], pch=19, cex=0.25, col="blue",
	xlab="x",ylab="y",main="Fuzzy circle", xlim=c(-RMax,RMax), ylim=c(-RMax,RMax) )

```


Example of solution

[[File:FuzzyCircle.jpg]]

## Racket


```racket
#lang racket

(require plot plot/utils)

(plot (points (for*/lists (result)
                ([_ (in-naturals)]
                 #:break (= 100 (length result))
                 [xy (in-value (v- (vector (random 31) (random 31))
                                   #(15 15)))]
                 #:when (<= 10 (vmag xy) 15))
                xy)))
```



## REXX

===version 0, without aspect adjustment===
No aspect adjustment is done in version of the REXX program.

Both version '''0''' and version '''1''' suppress the displaying of blank lines at the top and bottom of the plot.

```rexx
/*REXX program  generates  100  random points  in an  annulus:   10  ≤  √(x²≤y²)  ≤  15 */
parse arg points low high .                      /*obtain optional args from the C.L.   */
if points==''  then points=100
if    low==''  then  low=10;   low2= low**2      /*define a shortcut for squaring  LOW. */
if   high==''  then high=15;  high2=high**2      /*   "   "    "      "     "      HIGH.*/
$=
   do x=-high;        x2=x*x                     /*generate all possible annulus points.*/
   if x<0 & x2>high2  then iterate
   if x>0 & x2>high2  then leave
         do y=-high;          s=x2+y*y
         if (y<0 & s>high2) | s<low2  then iterate
         if  y>0 & s>high2            then leave
         $=$ x','y                               /*add a point─set to the  $  list.     */
         end   /*y*/
   end         /*x*/

plotChar='Θ';        minY=high2;       maxY=-minY;       ap=words($);      @.=

   do j=1  for points                            /*define the  x,y points [character O].*/
   parse value word($,random(1,ap)) with x ',' y /*pick a  random point  in the annulus.*/
   @.y=overlay(plotChar, @.y, x+high+1)          /*define:  the data point.             */
   minY=min(minY,y);    maxY=max(maxY,y)         /*perform the plot point restricting.  */
   end   /*j*/
                                                 /* [↓]  only show displayable section. */
 do y=minY  to maxY;  say @.y;  end              /*display the annulus to the terminal. */
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   (without aspect adjustment)   when using the default input:

```txt

                Θ
            ΘΘ     ΘΘ
     ΘΘ         Θ Θ     Θ
        Θ              Θ Θ
    ΘΘ Θ  Θ               Θ
    Θ                Θ  Θ

  ΘΘ   Θ                Θ  Θ
   Θ Θ                   Θ
 Θ Θ                        ΘΘ
 Θ Θ                      Θ
                          Θ
 Θ                         Θ
Θ                           Θ Θ
 Θ ΘΘ
                          Θ
                             Θ
  ΘΘ
 Θ                      Θ Θ
                          Θ
  Θ  Θ                    Θ
    Θ  Θ             Θ     Θ
    Θ Θ                    Θ
         Θ  ΘΘ    Θ    Θ
       ΘΘΘΘ   Θ  Θ      Θ
      Θ Θ Θ      Θ    ΘΘ
        Θ Θ Θ        Θ
            Θ  Θ

```


===version 1, with aspect adjustment===
Aspect adjustment is done in this version of the REXX program.

```rexx
/*REXX program  generates  100  random points  in an  annulus:   10  ≤  √(x²≤y²)  ≤  15 */
parse arg points low high .                      /*obtain optional args from the C.L.   */
if points==''  then points=100
if    low==''  then  low=10;   low2= low**2      /*define a shortcut for squaring  LOW. */
if   high==''  then high=15;  high2=high**2      /*   "   "    "      "     "      HIGH.*/
$=
   do x=-high;        x2=x*x                     /*generate all possible annulus points.*/
   if x<0 & x2>high2  then iterate
   if x>0 & x2>high2  then leave
         do y=-high;          s=x2+y*y
         if (y<0 & s>high2) | s<low2  then iterate
         if  y>0 & s>high2            then leave
         $=$ x','y                               /*add a point─set to the  $  list.     */
         end   /*y*/
   end         /*x*/

plotChar='Θ';        minY=high2;       maxY=-minY;       ap=words($);      @.=

   do j=1  for points                            /*define the  x,y points [character O].*/
   parse value word($,random(1,ap)) with x ',' y /*pick a  random point  in the annulus.*/
   @.y=overlay(plotChar, @.y, 2*x+2*high+1)      /*define:  the data point.             */
   minY=min(minY,y);    maxY=max(maxY,y)         /*perform the plot point restricting.  */
   end   /*j*/
                                                 /* [↓]  only show displayable section. */
 do y=minY  to maxY;  say @.y;  end              /*display the annulus to the terminal. */
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   (with aspect adjustment)   when using the default input:

```txt

                              Θ
                Θ   Θ               Θ
                    Θ Θ       Θ         Θ   Θ Θ
          Θ Θ   Θ           Θ Θ               Θ Θ
        Θ         Θ             Θ   Θ     Θ   Θ
                                                    Θ
                                          Θ     Θ Θ
            Θ Θ                               Θ       Θ Θ
          Θ
    Θ Θ Θ Θ                                             Θ Θ
      Θ                                             Θ
        Θ                                                 Θ

      Θ                                             Θ     Θ
                                                        Θ Θ
        Θ
      Θ
  Θ                                               Θ     Θ
  Θ     Θ                                           Θ     Θ
      Θ     Θ                                             Θ
    Θ         Θ                                       Θ
      Θ                                         Θ Θ

                    Θ                   Θ         Θ   Θ
                      Θ               Θ     Θ Θ Θ
            Θ Θ
                      Θ     Θ           Θ     Θ
                Θ Θ                     Θ Θ

                              Θ

```



### version 2


```rexx
/* REXX ---------------------------------------------------------------
* show 100 random points of an annulus with radius 10 to 15
* 18.06.2014 Walter Pachl 'derived/simplified' from REXX version 1
*--------------------------------------------------------------------*/
  Parse Arg points low high scale . /* allow parms from command line.*/
  If points=='' Then  points=100    /* number of points              */
  If low==''    Then  low=10        /* inner radius                  */
  If high==''   Then  high=15       /* outer radius                  */
  If scale==''  Then  scale=2       /* horizontal scaling            */
  low2=low**2
  high2=high**2
  /* first compute all possible points                               */
  point.=0
  Do x=-high To high
    x2=x*x
    Do y=-high To high
      y2=y*y
      s=x2+y2
      If s>=low2 &s<=high2 Then Do
        z=point.0+1
        point.z=x y
        point.0=z
        End
      End
    End
  plotchar='O'
  line.=''
  np=point.0                           /* available points           */
  Do j=1 To points                     /* pick the needed points     */
    r=random(1,np)
    Parse Var point.r x y              /* coordinates                */
    line.y=overlay(plotchar,line.y,scale*(x+high)+1) /* put into line*/
    point.r=point.np                   /* replace taken point by last*/
    np=np-1                            /* reduce available points    */
    If np=0 Then Leave                 /* all possible points taken  */
    End
/* now draw the picture                                              */
  Do y=-high To high
    Say line.y
    End
```

'''output''' using default parameters

```txt

                                  O
                    O O           O
                          O O         O     O   O
          O O O       O   O O       O         O   O
        O     O       O             O O O O O
                    O                     O O O O O O
      O         O
        O
        O                                         O
  O         O
          O
    O   O O                                             O O
      O   O                                         O
                                                        O
  O       O                                           O
    O
                                                  O
        O
          O                                             O
      O   O O                                   O
      O                                         O       O
        O   O                                     O   O O
              O                               O O   O O
                O                               O
                      O   O O O   O
            O                                   O
              O O O   O O O               O O   O
                  O       O O             O O
                    O               O


```

'''output''' using rexx fcaa 100 3 4 2

```txt
        O
    O O O O O
  O           O
  O           O
O O           O O
  O           O
  O           O
    O O O O O
        O
```


### version 3


```rexx
/* REXX ---------------------------------------------------------------
* 19.06.2014 Walter Pachl alternate algorithm
* the idea: yl is a list of y coordinates which may have unused points
* one of the y's is picked at random
* Then we look for unused x coordinates in this line
* we pick one at random or drop the y from yl if none is found
* When yl becomes empty, all points are used and we stop
*--------------------------------------------------------------------*/
Parse Arg n r rr scale
If r=''     Then r=10
If rr=''    Then rr=15
If n=''     Then n=100
If scale='' Then scale=2
r2=r*r
rr2=rr*rr
ymin=0
ymax=rr*2
ol=''
pp.=0
used.=0
yl=''                                  /* list of available y values */
Do y=-rr To rr
  yl=yl y
  End
Do Until pp.0=n                        /*look for the required points*/
  If yl='' Then Do                     /* no more points available   */
    Say 'all points filled'
    Leave
    End
  yi=random(1,words(yl))               /* pick a y                   */
  y=word(yl,yi)
  y2=y*y
  p.=0
  Do x=0 To rr                         /* Loop through possible x's  */
    x2=x*x
    xy2=x2+y2
    If xy2>=r2&xy2<=rr2 Then Do        /* within the annulus         */
      Call take x y
      Call take (-x) y
      End
    End
  If p.0>0 Then Do                     /* some x's found (or just 1) */
    xi=random(1,p.0)                   /* pick an x                  */
    z=pp.0+1
    pp.z=p.xi
    pp.0=z
    Parse Var pp.z xa ya
    used.xa.ya=1                       /* remember it's taken        */
    End
  Else Do                              /* no x for this y            */
    yi=wordpos(y,yl)                   /* remove y from yl           */
    Select
      When yi=1 Then yl=subword(yl,yi+1)
      When yi=words(yl) Then yl=subword(yl,1,yi-1)
      Otherwise yl=subword(yl,1,yi-1) subword(yl,yi+1)
      End
    End
  End
line.=''                               /* empty the raster           */
Do i=1 To pp.0                         /* place the points           */
  Parse Var pp.i x y
  line.y=overlay('+',line.y,scale*(rr+x)+1)
  End
Do y=-rr To rr                         /* show the result            */
  Say line.y
  End
say pp.0 'points filled'
Exit
Return

take: Procedure Expose p. used.        /* add x to p. if its not used*/
  Parse Arg x y
  If used.x.y=0 Then Do
    z=p.0+1
    p.z=x y
    p.0=z
    End
  Return
```

'''output''' using rexx fcaa 100 3 5 2

```txt
all points filled
          +
    + + + + + + +
  + + + + + + + + +
  + +           + +
  + +           + +
+ + +           + + +
  + +           + +
  + +           + +
  + + + + + + + + +
    + + + + + + +
          +
56 points filled
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

        for i = 1 to 1000
            x = random(31)-16
            y = random(31)-16
            r = sqrt (pow(x,2) + pow(y,2))
            if r >= 10 if r <= 15 drawpoint(x*2, y*2) ok ok
        next

        endpaint()
        }
        label1 { setpicture(p1) show() }

```


Output:

[[File:CalmoSoftDrawCircle.jpg]]


## Ruby

Create the image with [[Raster graphics operations/Ruby]]

```Ruby
points = (1..100).map do
  # choose a random radius and angle
  angle = rand * 2.0 * Math::PI
  rad   = rand * 5.0 + 10.0
  # convert back from polar to cartesian coordinates
  [rad * Math::cos(angle), rad * Math::sin(angle)].map(&:round)
end

(-15..15).each do |row|
  puts (-15..15).map { |col| points.include?([row, col]) ? "X" : " " }.join
end

load 'raster_graphics.rb'

pixmap = Pixmap.new(321,321)
pixmap.draw_circle(Pixel.new(160,160),90,RGBColour::BLACK)
pixmap.draw_circle(Pixel.new(160,160),160,RGBColour::BLACK)
points.each {|(x,y)| pixmap[10*(x+16),10*(y+16)] = RGBColour::BLACK}
pngfile = __FILE__
pngfile[/\.rb/] = ".png"
pixmap.save_as_png(pngfile)
```


[[File:constrainedrandompointsonacircle.png|thumg|right|Sample output from Ruby program]]

```txt
          X X
         X      XX
           X XXX
         XX X  X  X X  X
               XXXXX
      XX            XX
   X  X                    X
    X                   X
   X                   X
     X                   X XXX
     X
 XX X                     X
  X                           X
 X                        X
   XXX                      X
XX  X
  XXX                     X  X

  XX X                   X X
   XX                  X   X
   X                      X
                            X
  XX  X                   X
   X               X      X
      X        X   X
              X
             X       X X
          X    X     X
            X      X
```



### algorithm 2:


```ruby
r2 = 10*10..15*15
range = (-15..15).to_a
points = range.product(range).select {|i,j| r2.cover?(i*i + j*j)}

puts "Precalculate: #{points.size}"
pt = Hash.new("  ")
points.sample(100).each{|ij| pt[ij] = " o"}
puts range.map{|i| range.map{|j| pt[[i,j]]}.join}
```


```txt

Precalculate: 404

                               o o o
                 o o o     o
                 o           o     o o       o
           o   o     o                 o         o o
               o       o             o   o           o
                                               o     o
       o                                       o
                                                   o o o
     o         o                                     o
     o o                                           o       o
       o                                             o
                                                           o
         o
           o                                             o
         o                                               o   o

                                                           o
   o o   o                                         o   o
   o       o                                         o
   o   o     o                                         o
                                                   o
             o                                   o     o
                 o                                   o o
       o       o                           o   o   o   o
           o o     o   o     o       o     o o   o
                         o           o   o o   o
             o     o o   o                 o o
                   o o   o o             o
                         o o
                               o

```



## Run BASIC


```runbasic
w = 320
h = 320
dim canvas(w,h)
for pts = 1 to 1000
  x = (rnd(1) * 31) - 15
  y = (rnd(1) * 31) - 15
  r = x * x + y * y
  if (r > 100) and (r < 225) then
    x = int(x * 10 + w/2)
    y = int(y * 10 + h/2)
    canvas(x,y) = 1
  end if
next pts

' -----------------------------
' display the graphic
' -----------------------------
graphic #g, w,h
for x = 1 to w
  for y = 1 to h
     if canvas(x,y) = 1 then  #g "color green ; set "; x; " "; y else #g "color blue ; set "; x; " "; y
  next y
next x
render #g
#g "flush"
```



## Rust


```Rust
#![feature(inclusive_range_syntax)]

extern crate rand;

use rand::Rng;

const POINTS_N: usize = 100;

fn generate_point<R: Rng>(rng: &mut R) -> (i32, i32) {
    loop {
        let x = rng.gen_range(-15, 16); // exclusive
        let y = rng.gen_range(-15, 16);

        let r2 = x * x + y * y;
        if r2 >= 100 && r2 <= 225 {
            return (x, y);
        }
    }
}

fn filtering_method<R: Rng>(rng: &mut R) {
    let mut rows = [[" "; 62]; 31];

    // Generate points
    for _ in 0..POINTS_N {
        let (x, y) = generate_point(rng);
        rows[(y + 15) as usize][(x + 15) as usize * 2] = "*";
    }

    // draw the points
    for row in &rows {
        println!("{}", row.concat());
    }
}

fn precalculating_method<R: Rng>(rng: &mut R) {
    // Generate all possible points
    let mut possible_points = Vec::with_capacity(404);
    for y in -15..=15 {
        for x in -15..=15 {
            let r2 = x * x + y * y;
            if r2 >= 100 && r2 <= 225 {
                possible_points.push((x, y));
            }
        }
    }

    // A truncated Fisher-Yates shuffle
    let len = possible_points.len();
    for i in (len - POINTS_N..len).rev() {
        let j = rng.gen_range(0, i + 1);
        possible_points.swap(i, j);
    }

    // turn the selected points into "pixels"
    let mut rows = [[" "; 62]; 31];
    for &(x, y) in &possible_points[len - POINTS_N..] {
        rows[(y + 15) as usize][(x + 15) as usize * 2] = "*";
    }

    // draw the "pixels"
    for row in &rows {
        println!("{}", row.concat());
    }
}

fn main() {
    let mut rng = rand::weak_rng();

    filtering_method(&mut rng);

    precalculating_method(&mut rng);
}
```



## Scala

```Scala
import java.awt.{ Color, geom,Graphics2D ,Rectangle}
import scala.math.hypot
import scala.swing.{MainFrame,Panel,SimpleSwingApplication}
import scala.swing.Swing.pair2Dimension
import scala.util.Random

object CirculairConstrainedRandomPoints extends SimpleSwingApplication {
  //min/max of display-x resp. y
  val dx0, dy0 = 30; val dxm, dym = 430
  val prefSizeX, prefSizeY = 480

  val palet = Map("b" -> Color.blue, "g" -> Color.green, "r" -> Color.red, "s" -> Color.black)
  val cs = List((0, 0, 10, "b"), (0, 0, 15, "g")) //circle position and color
  val xmax, ymax = 20; val xmin, ymin = -xmax

  class Coord(x: Double, y: Double) {
    def dx = (((dxm - dx0) / 2 + x.toDouble / xmax * (dxm - dx0) / 2) + dx0).toInt
    def dy = (((dym - dy0) / 2 - y.toDouble / ymax * (dym - dy0) / 2) + dy0).toInt
  }

  object Coord {
    def apply(x: Double, y: Double) = new Coord(x, y)
  }

  //points:
  val points =
    new Iterator[Int] { val r = new Random;def next = r.nextInt(31) - 15; def hasNext = true }.toStream.
      zip(new Iterator[Int] { val r = new Random; def next = r.nextInt(31) - 15; def hasNext = true }.toStream).
      map { case (x, y) => (x, y, hypot(x, y)) }.filter { case (x, y, r) => r >= 10 && r <= 15 }.take(100).toSeq.
      map { case (x, y, r) => new Rectangle(Coord(x, y).dx - 2, Coord(x, y).dy - 2, 4, 4) }

  private def ui = new Panel {
    background = Color.white
    preferredSize = (prefSizeX, prefSizeY)

    class Circle(center: Coord, r: Double, val color: Color) {
      val dr = (Coord(r, 0).dx - pcentre.dx) * 2
      val dx = center.dx - dr / 2
      val dy = center.dy - dr / 2
    }

    object Circle {
      def apply(x: Double, y: Double, r: Double, color: Color) =
        new Circle(Coord(x, y), r, color)
    }

    val pcentre = Coord(0, 0)
    val pxmax = Coord(xmax, 0); val pxmin = Coord(xmin, 0)
    val pymax = Coord(0, ymax); val pymin = Coord(0, ymin)

    //axes:
    val a_path = new geom.GeneralPath
    a_path.moveTo(pxmin.dx, pxmin.dy); a_path.lineTo(pxmax.dx, pxmax.dy) //x-axis
    a_path.moveTo(pymin.dx, pymin.dy); a_path.lineTo(pymax.dx, pymax.dy) //y-axis

    //labeling:
    val labels = List(-20, -15, -10, -5, 5, 10, 15, 20)
    labels.foreach { x => { val p = Coord(x, 0); a_path.moveTo(p.dx, p.dy - 3); a_path.lineTo(p.dx, p.dy + 3) } }
    labels.foreach { y => { val p = Coord(0, y); a_path.moveTo(p.dx - 3, p.dy); a_path.lineTo(p.dx + 3, p.dy) } }
    val xlabels = labels.map(x => { val p = Coord(x, 0); Triple(x.toString, p.dx - 3, p.dy + 20) })
    val ylabels = labels.map(y => { val p = Coord(0, y); Triple(y.toString, p.dx - 20, p.dy + 5) })

    //circles:
    val circles = cs.map { case (x, y, r, c) => Circle(x, y, r, palet(c)) }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      circles.foreach { c => { g.setColor(c.color); g.drawOval(c.dx, c.dy, c.dr, c.dr) } }
      g.setColor(palet("r")); points.foreach(g.draw(_))
      g.setColor(palet("s")); g.draw(a_path)
      xlabels.foreach { case (text, px, py) => g.drawString(text, px, py) }
      ylabels.foreach { case (text, px, py) => g.drawString(text, px, py) }
    }
  } // def ui

  def top = new MainFrame {
    title = "Rosetta Code >>> Task: Constrained random points on a circle | Language: Scala"
    contents = ui
  }
}
```



## Sidef

Generates an EPS file.

```ruby
var points = [];
while (points.len < 100) {
    var (x, y) = 2.of{31.rand.int - 15}...;
    var r2 = (x**2 + y**2);
    if ((r2 >= 100) && (r2 <= 225)) {
        points.append([x, y]);
    }
}

print <<'HEAD';
%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox 0 0 400 400
200 200 translate 10 10 scale
0 setlinewidth
1 0 0 setrgbcolor
0 0 10 0 360 arc stroke
0 0 15 360 0 arcn stroke
0 setgray
/pt { .1 0 360 arc fill } def
HEAD

points.each { |pt| say "#{pt.join(' ')} pt" };
print '%%EOF';
```



## SystemVerilog


```SystemVerilog
program main;

  bit [39:0] bitmap [40];

  class Point;
    rand bit signed [4:0] x;
    rand bit signed [4:0] y;

    constraint on_circle_edge {
      (10*10) <= (x*x + y*y);
      (x*x + y*y) <= (15*15);
    };

    function void do_point();
      randomize;
      bitmap[x+20][y+20] = 1;
    endfunction
  endclass

  initial begin
    Point p = new;
    repeat (100) p.do_point;
    foreach (bitmap[row]) $display( "%b", bitmap[row]);
  end

endprogram
```


Piping the output through sed to improve the contrast of the output:


```txt

% vcs -sverilog -R circle.sv | sed 's/0/ /g'

                   1
                11 1  1
            1 1  1    11
          1     1   1   11
            1           1  1
             1      1      1
            1
       1   1                  1
      1    1               1
                             1
       11
                                11
         1
     11  1                     1 1
         1                   1
    1   1                    1   1
     1                        1  1
     11 1                       1
                             11
     1111                        1
      1 111                  1
       11 1                111  1
       11
                          1  1
            1            1   1
                   1
             1  11
                          1
                   1    1
               11  1 1



```



## Tcl


```tcl
package require Tcl 8.5

# Generate random point at specified distance from the centre
proc getPoint {range from to} {
    set r2 [expr {$range / 2}]
    set f2 [expr {$from ** 2}]
    set t2 [expr {$to ** 2}]
    while 1 {
	set x [expr {int($range * rand())}]
	set y [expr {int($range * rand())}]
	set d2 [expr {($x-$r2)**2 + ($y-$r2)**2}]
	if {$d2 >= $f2 && $d2 <= $t2} {
	    return [list $y $x]
	}
    }
}

# Make somewhere to store the counters
set ary [lrepeat 31 [lrepeat 31 0]]

# Generate 100 random points
for {set i 0} {$i < 100} {incr i} {
    set location [getPoint 31 10 15]
    # Increment the counter for the point
    lset ary $location [expr {1 + [lindex $ary $location]}]
}

# Simple renderer
foreach line $ary {
    foreach c $line {
	puts -nonewline [expr {$c == 0 ? " " : $c > 9 ? "X" : $c}]
    }
    puts ""
}
```

Example output:

```txt

               1
           1  1

         1 1 1   2   1 1
        11   1        1  1
       11 1            1  1
   1     1
   1    12               1
     1 1               1
  1 1                    1
      1                    1
   1                       1 1
                          1  2
                           1
 1                         1
 2   1                    2
  2                         1
                             1
    1                    11
     1                   1
      1
  1                       1
     2                 1    1
   1                     1
   1 1   1          11     1
     2  1  1        11
        11      1      1 1
      1 2   1       11
         121   1  1
           1  1   1
               1

```



## XPL0

[[File:DonutXPL0.gif|right]]

```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
int X, Y, C, R2;
[SetVid($13);           \set 320x200x8 graphics mode
C:= 0;                  \initialize point counter
repeat  X:= Ran(31)-15; \range -15..+15
        Y:= Ran(31)-15;
        R2:= X*X + Y*Y;
        if R2>=10*10 & R2<=15*15 then
                [Point(X+160, Y+100, $F);  C:= C+1];
until C >= 100;
C:= ChIn(1);            \wait for keystroke
SetVid(3);              \restore normal text mode
]
```



## zkl


```zkl
xy:=(0).walker(*).tweak(fcn{  // generate infinite random pairs (lazy)
   x:=(-15).random(16); y:=(-15).random(16);
   if(not (100<=(x*x + y*y)<=225)) Void.Skip else T(x,y)
});

const N=31;  // [-15..15] includes 0
array:=(" ,"*N*N).split(",").copy();  // bunch of spaces (list)

xy.walk(100).apply2(fcn([(x,y)],array){array[x+15 + N*(y+15)]="*"},array);
foreach n in ([0..30]){ array[n*N,30].concat().println(); }
```

```txt


          *   *     *
               **   ***
         *  *  * **     *
       * **
        *      *    *     *
       *                *  *
    *                *   ***
       *
      *                   *
  * *                    *   *
 *                       *
     *                     *
                           *
                         *   *
  ****                      **
                           *
   *
 *  *
                          *  *
 **  *                       *
   * *
                       * *  *
        *             *
   *      *
            ** * **
         *         *  ***
       * *        *
        *    *
           *

```



## ZX Spectrum Basic

```zxbasic
10 FOR i=1 TO 1000
20 LET x=RND*31-16
30 LET y=RND*31-16
40 LET r=SQR (x*x+y*y)
50 IF (r>=10) AND (r<=15) THEN PLOT 127+x*2,88+y*2
60 NEXT i
```
