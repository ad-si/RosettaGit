+++
title = "Shoelace formula for polygonal area"
description = ""
date = 2019-10-13T04:05:53Z
aliases = []
[extra]
id = 21557
[taxonomies]
categories = []
tags = []
+++

{{task}}
Given the <code>n + 1</code> vertices <code>x[0], y[0] .. x[N], y[N]</code> of a simple polygon described in a clockwise direction, then the polygon's area can be calculated by:

```txt

abs( (sum(x[0]*y[1] + ... x[n-1]*y[n]) + x[N]*y[0]) -
     (sum(x[1]*y[0] + ... x[n]*y[n-1]) + x[0]*y[N])
   ) / 2
```

(Where <code>abs</code> returns the absolute value)

;Task:
Write a function/method/routine to use the the [[wp:Shoelace formula|Shoelace formula]] to calculate the area of the polygon described by the ordered points:
     <big> (3,4), (5,11), (12,8), (9,5), and (5,6) </big>


Show the answer here, on this page.





## 360 Assembly


```360asm
*        SHOELACE                  25/02/2019
SHOELACE CSECT
         USING  SHOELACE,R15       base register
         MVC    SUPS(8),POINTS     x(nt+1)=x(1); y(nt+1)=y(1)
         LA     R9,0               area=0
         LA     R7,POINTS          @x(1)
         LA     R6,NT              do i=1 to nt
LOOP     L      R3,0(R7)             x(i)
         M      R2,12(R7)            *y(i+1)
         L      R5,8(R7)             x(i+1)
         M      R4,4(R7)             *y(i)
         SR     R3,R5                x(i)*y(i+1)-x(i+1)*y(i)
         AR     R9,R3                area=area+x(i)*y(i+1)-x(i+1)*y(i)
         LA     R7,8(R7)             @x(i++)
         BCT    R6,LOOP            enddo
         LPR    R9,R9              area=abs(area)
         SRA    R9,1               area=area/2
         XDECO  R9,PG              edit area
         XPRNT  PG,L'PG            print area
         BR     R14                return to caller
NT       EQU    (SUPS-POINTS)/8    nt  number of points
POINTS   DC     F'3',F'4',F'5',F'11',F'12',F'8',F'9',F'5',F'5',F'6'
SUPS     DS     2F                 x(nt+1),y(nt+1)
PG       DC     CL12' '            buffer
         REGEQU
         END    SHOELACE
```

{{out}}

```txt

          30

```



## Ada

{{works with|Ada|Ada|83}}


```Ada
with Ada.Text_IO;

procedure Shoelace_Formula_For_Polygonal_Area
is
   type Point is record
      x, y : Float;
   end record;

   type Polygon is array (Positive range <>) of Point;

   function Shoelace(input : in Polygon) return Float
   is
      sum_1 : Float := 0.0;
      sum_2 : Float := 0.0;
      tmp : constant Polygon := input & input(input'First);
   begin
      for I in tmp'First .. tmp'Last - 1 loop
         sum_1 := sum_1 + tmp(I).x * tmp(I+1).y;
         sum_2 := sum_2 + tmp(I+1).x * tmp(I).y;
      end loop;
      return abs(sum_1 - sum_2) / 2.0;
   end Shoelace;

   my_polygon : constant Polygon :=
     ((3.0, 4.0),
      (5.0, 11.0),
      (12.0, 8.0),
      (9.0, 5.0),
      (5.0, 6.0));
begin
   Ada.Text_IO.Put_Line(Shoelace(my_polygon)'Img);
end Shoelace_Formula_For_Polygonal_Area;
```

{{out}}

```txt
 3.00000E+01

```



## ALGOL 60

Optimized version:
 '''begin'''
     '''comment''' Shoelace formula for polygonal area - Algol 60;
     '''real''' '''array''' x[1:33],y[1:33];
     '''integer''' i,n;
     '''real''' a;
     ininteger(0,n);
     '''for''' i:=1 '''step''' 1 '''until''' n '''do'''
     '''begin'''
         inreal(0,x[i]);
         inreal(0,y[i])
     '''end''';
     x[i]:=x[1];
     y[i]:=y[1];
     a:=0;
     '''for''' i:=1 '''step''' 1 '''until''' n '''do'''
         a:=a+x[i]*y[i+1]-x[i+1]*y[i];
     a:=abs(a/2.);
     outreal(1,a)
 '''end'''
{{out}}

```txt

     30.00

```

Non-optimized version:
 '''begin'''
     '''comment''' Shoelace formula for polygonal area - Algol 60;
     '''real''' '''array''' x[1:32],y[1:32];
     '''integer''' i,j,n;
     '''real''' a;
     ininteger(0,n);
     '''for''' i:=1 '''step''' 1 '''until''' n '''do'''
     '''begin'''
         inreal(0,x[i]); inreal(0,y[i])
     '''end''';
     a:=0;
     '''for''' i:=1 '''step''' 1 '''until''' n '''do'''
     '''begin'''
         j:='''if''' i=n '''then''' 1 '''else''' i+1;
         a:=a+x[i]*y[j]-x[j]*y[i]
     '''end''';
     a:=abs(a/2.);
     outreal(1,a)
 '''end'''
{{out}}

```txt

     30.00

```



## ALGOL 68


```algol68
BEGIN
    # returns the area of the polygon defined by the points p using the Shoelace formula #
    OP  AREA = ( [,]REAL p )REAL:
        BEGIN
            [,]REAL points = p[ AT 1, AT 1 ]; # normalise array bounds to start at 1 #
            IF 2 UPB points /= 2 THEN
                # the points do not have 2 coordinates #
                -1
            ELSE
                REAL   result := 0;
                INT    n       = 1 UPB points;
                IF n > 1 THEN
                    # there at least two points #
                    []REAL x   = points[ :, 1 ];
                    []REAL y   = points[ :, 2 ];
                    FOR i TO 1 UPB points - 1 DO
                        result +:= x[ i     ] * y[ i + 1 ];
                        result -:= x[ i + 1 ] * y[ i     ]
                    OD;
                    result     +:= x[ n ] * y[ 1 ];
                    result     -:= x[ 1 ] * y[ n ]
                FI;
                ( ABS result ) / 2
            FI
        END # AREA # ;

    # test case as per the task #
    print( ( fixed( AREA [,]REAL( ( 3.0, 4.0 ), ( 5.0, 11.0 ), ( 12.0, 8.0 ), ( 9.0, 5.0 ), ( 5.0, 6.0 ) ), -6, 2 ), newline ) )
END

```

{{out}}

```txt

 30.00

```



## C

Reads the points from a file whose name is supplied via the command line, prints out usage if invoked incorrectly.

```C

#include<stdlib.h>
#include<stdio.h>
#include<math.h>

typedef struct{
	double x,y;
}point;

double shoelace(char* inputFile){
	int i,numPoints;
	double leftSum = 0,rightSum = 0;

	point* pointSet;
	FILE* fp = fopen(inputFile,"r");

	fscanf(fp,"%d",&numPoints);

	pointSet = (point*)malloc((numPoints + 1)*sizeof(point));

	for(i=0;i<numPoints;i++){
		fscanf(fp,"%lf %lf",&pointSet[i].x,&pointSet[i].y);
	}

	fclose(fp);

	pointSet[numPoints] = pointSet[0];

	for(i=0;i<numPoints;i++){
		leftSum += pointSet[i].x*pointSet[i+1].y;
		rightSum += pointSet[i+1].x*pointSet[i].y;
	}

	free(pointSet);

	return 0.5*fabs(leftSum - rightSum);
}

int main(int argC,char* argV[])
{
	if(argC==1)
		printf("\nUsage : %s <full path of polygon vertices file>",argV[0]);

	else
		printf("The polygon area is %lf square units.",shoelace(argV[1]));

	return 0;
}

```

Input file, first line specifies number of points followed by the ordered vertices set with one vertex on each line.

```txt

5
3 4
5 11
12 8
9 5
5 6

```

Invocation and output :

```txt

C:\rosettaCode>shoelace.exe polyData.txt
The polygon area is 30.000000 square units.

```



## C++

{{trans|D}}

```cpp
#include <iostream>
#include <tuple>
#include <vector>

using namespace std;

double shoelace(vector<pair<double, double>> points) {
	double leftSum = 0.0;
	double rightSum = 0.0;

	for (int i = 0; i < points.size(); ++i) {
		int j = (i + 1) % points.size();
		leftSum  += points[i].first * points[j].second;
		rightSum += points[j].first * points[i].second;
	}

	return 0.5 * abs(leftSum - rightSum);
}

void main() {
	vector<pair<double, double>> points = {
		make_pair( 3,  4),
		make_pair( 5, 11),
		make_pair(12,  8),
		make_pair( 9,  5),
		make_pair( 5,  6),
	};

	auto ans = shoelace(points);
	cout << ans << endl;
}
```

{{out}}

```txt
30
```


## C#
{{trans|Java}}

```c#
using System;
using System.Collections.Generic;

namespace ShoelaceFormula {
    using Point = Tuple<double, double>;

    class Program {
        static double ShoelaceArea(List<Point> v) {
            int n = v.Count;
            double a = 0.0;
            for (int i = 0; i < n - 1; i++) {
                a += v[i].Item1 * v[i + 1].Item2 - v[i + 1].Item1 * v[i].Item2;
            }
            return Math.Abs(a + v[n - 1].Item1 * v[0].Item2 - v[0].Item1 * v[n - 1].Item2) / 2.0;
        }

        static void Main(string[] args) {
            List<Point> v = new List<Point>() {
                new Point(3,4),
                new Point(5,11),
                new Point(12,8),
                new Point(9,5),
                new Point(5,6),
            };
            double area = ShoelaceArea(v);
            Console.WriteLine("Given a polygon with vertices [{0}],", string.Join(", ", v));
            Console.WriteLine("its area is {0}.", area);
        }
    }
}
```

{{out}}

```txt
Given a polygon with vertices [(3, 4), (5, 11), (12, 8), (9, 5), (5, 6)],
its area is 30.
```



## D


```D
import std.stdio;

Point[] pnts = [{3,4}, {5,11}, {12,8}, {9,5}, {5,6}];

void main() {
    auto ans = shoelace(pnts);
    writeln(ans);
}

struct Point {
    real x, y;
}

real shoelace(Point[] pnts) {
    real leftSum = 0, rightSum = 0;

    for (int i=0; i<pnts.length; ++i) {
        int j = (i+1) % pnts.length;
        leftSum  += pnts[i].x * pnts[j].y;
        rightSum += pnts[j].x * pnts[i].y;
    }

    import std.math : abs;
    return 0.5 * abs(leftSum - rightSum);
}

unittest {
    auto ans = shoelace(pnts);
    assert(ans == 30);
}
```

{{out}}

```txt
30
```


=={{header|F_Sharp|F#}}==

```fsharp

// Shoelace formula for area of polygon. Nigel Galloway: April 11th., 2018
let fN(n::g) = abs(List.pairwise(n::g@[n])|>List.fold(fun n ((nα,gα),(nβ,gβ))->n+(nα*gβ)-(gα*nβ)) 0.0)/2.0
printfn "%f" (fN [(3.0,4.0); (5.0,11.0); (12.0,8.0); (9.0,5.0); (5.0,6.0)])
```

{{out}}

```txt

30.000000

```



## Factor

By constructing a <code>circular</code> from a sequence, we can index elements beyond the length of the sequence, wrapping around to the beginning. We can also change the beginning of the sequence to an arbitrary index. This allows us to use <code>2map</code> to cleanly obtain a sum.

```factor
USING: circular kernel math prettyprint sequences ;
IN: rosetta-code.shoelace

CONSTANT: input { { 3 4 } { 5 11 } { 12 8 } { 9 5 } { 5 6 } }

: align-pairs ( pairs-seq -- seq1 seq2 )
    <circular> dup clone [ 1 ] dip
    [ change-circular-start ] keep ;

: shoelace-sum ( seq1 seq2 -- n )
    [ [ first ] [ second ] bi* * ] 2map sum ;

: shoelace-area ( pairs-seq -- area )
    [ align-pairs ] [ align-pairs swap ] bi
    [ shoelace-sum ] 2bi@ - abs 2 / ;

input shoelace-area .
```

{{out}}

```txt

30

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Shoelace_formula_for_polygonal_area this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran


### Fortran 90

Except for the use of "END FUNCTION ''name'' instead of just END, and the convenient function SUM with array span expressions (so SUM(P) rather than a DO-loop to sum the elements of array P), both standardised with F90, this would be acceptable to F66, which introduced complex number arithmetic. Otherwise, separate X and Y arrays would be needed, but complex numbers seemed convenient seeing as (x,y) pairs are involved. But because the MODULE facility of F90 has not been used, routines invoking functions must declare the type of the function names, especially if the default types are unsuitable, as here. In function AREA, the x and y parts are dealt with together, but in AREASL they might be better as separate arrays, thus avoiding the DIMAG and DBLE functions to extract the x and y parts. Incidentally, the x and y parts can be interchanged and the calculation still works. Comparing the two resulting areas might give some indication of their accuracy.

If the MODULE protocol were used, the size of an array parameter is passed as a secret additional parameter accessible via the special function UBOUND, but otherwise it must be passed as an explicit parameter. A quirk of the compiler requires that N be declared before it appears in <code>DOUBLE COMPLEX P(N)</code> so as it is my practice to declare parameters in the order specified, here N comes before P. However, it is not clear whether specifying P(N) does much good (as in array index checking) as an alternative is to specify P(*) meaning merely that the array has one dimension, or even P(12345) to the same effect, with no attention to the actual numerical value. See for example [[Array_length#Fortran]]
```Fortran
      DOUBLE PRECISION FUNCTION AREA(N,P)	!Calculates the area enclosed by the polygon P.
C   Uses the mid-point rule for integration. Consider the line joining (x1,y1) to (x2,y2)
C The area under that line (down to the x-axis) is the y-span midpoint (y1 + y2)/2 times the width (x2 - x1)
C This is the trapezoidal rule for a single interval, and follows from simple geometry.
C Now consider a sequence of such points heading in the +x direction: each successive interval's area is positive.
C Follow with a sequence of points heading in the -x direction, back to the first point: their areas are all negative.
C The resulting sum is the area below the +x sequence and above the -x sequence: the area of the polygon.
C   The point sequence can wobble as it wishes and can meet the other side, but it must not cross itself
c as would be done in a figure 8 drawn with a crossover instead of a meeting.
C   A clockwise traversal (as for an island) gives a positive area; use anti-clockwise for a lake.
       INTEGER N		!The number of points.
       DOUBLE COMPLEX P(N)	!The points.
       DOUBLE COMPLEX PP,PC	!Point Previous and Point Current.
       DOUBLE COMPLEX W		!Polygon centre. Map coordinates usually have large offsets.
       DOUBLE PRECISION A	!The area accumulator.
       INTEGER I		!A stepper.
        IF (N.LT.3) STOP "Area: at least three points are needed!"	!Good grief.
        W = (P(1) + P(N/3) + P(2*N/3))/3	!An initial working average.
        W = SUM(P(1:N) - W)/N + W	!A good working average is the average itself.
        A = 0			!The area enclosed by the point sequence.
        PC = P(N) - W		!The last point is implicitly joined to the first.
        DO I = 1,N		!Step through the positions.
          PP = PC			!Previous position.
          PC = P(I) - W			!Current position.
          A = (DIMAG(PC) + DIMAG(PP))*(DBLE(PC) - DBLE(PP)) + A	!Area integral component.
        END DO			!On to the next position.
        AREA = A/2		!Divide by two once.
      END FUNCTION AREA		!The units are those of the points.

      DOUBLE PRECISION FUNCTION AREASL(N,P)	!Area enclosed by polygon P, by the "shoelace" method.
       INTEGER N		!The number of points.
       DOUBLE COMPLEX P(N)	!The points.
       DOUBLE PRECISION A	!A scratchpad.
        A = SUM(DBLE(P(1:N - 1)*DIMAG(P(2:N)))) + DBLE(P(N))*DIMAG(P(1))
     1    - SUM(DBLE(P(2:N)*DIMAG(P(1:N - 1)))) - DBLE(P(1))*DIMAG(P(N))
        AREASL = A/2		!The midpoint formula requires a halving.
      END FUNCTION AREASL	!Negative for clockwise, positive for anti-clockwise.

      INTEGER ENUFF
      DOUBLE PRECISION AREA,AREASL	!The default types are not correct.
      DOUBLE PRECISION A1,A2		!Scratchpads, in case of a debugging WRITE within the functions.
      PARAMETER (ENUFF = 5)		!The specification.
      DOUBLE COMPLEX POINT(ENUFF)	!Could use X and Y arrays instead.
      DATA POINT/(3D0,4D0),(5D0,11D0),(12D0,8D0),(9D0,5D0),(5D0,6D0)/	!"D" for double precision.

      WRITE (6,*) POINT
      A1 = AREA(5,POINT)
      A2 = AREASL(5,POINT)
      WRITE (6,*) "A=",A1,A2
      END
```


Output: WRITE (6,*) means write to output unit six (standard output) with free-format (the *). Note the different sign convention.

```txt

 (3.00000000000000,4.00000000000000) (5.00000000000000,11.0000000000000)
 (12.0000000000000,8.00000000000000) (9.00000000000000,5.00000000000000)
 (5.00000000000000,6.00000000000000)
 A=   30.0000000000000       -30.0000000000000

```


The "shoelace" method came as a surprise to me, as I've always used what I had thought the "obvious" method. Note that function AREA makes one pass through the point data not two, and because map coordinate values often have large offsets a working average is used to reduce the loss of precision. This requires faith that <code>SUM(P(1:N) - W)</code> will be evaluated as written, not as <code>SUM(P(1:N)) - N*W</code> with even greater optimisation opportunity awaiting in cancelling further components of the expression. For example, the New Zealand metric grid has (2510000,6023150) as (Easting,Northing) or (x,y) at its central point of 41°S 173°E rather than (0,0) so seven digits of precision are used up. If anyone wants a copy of a set of point sequences for NZ (30,000 positions, 570KB) with lots of islands and lakes, even a pond in an island in a lake in the North Island...


### Fortran I

In orginal FORTRAN 1957:

```fortran

C SHOELACE FORMULA FOR POLYGONAL AREA
      DIMENSION X(33),Y(33)
      READ 101,N
      DO 1 I=1,N
   1    READ 102,X(I),Y(I)
      X(I)=X(1)
      Y(I)=Y(1)
      A=0
      DO 2 I=1,N
   2    A=A+X(I)*Y(I+1)-X(I+1)*Y(I)
      A=ABSF(A/2.)
      PRINT 303,A
      STOP
 101  FORMAT(I2)
 102  FORMAT(2F6.2)
 303  FORMAT(F10.2)


```

{{in}}

```txt

 5
  3.00  4.00
  5.00 11.00
 12.00  8.00
  9.00  5.00
  5.00  6.00

```

{{out}}

```txt

     30.00

```



## FreeBASIC


```freebasic
' version 18-08-2017
' compile with: fbc -s console

Type _point_
    As Double x, y
End Type

Function shoelace_formula(p() As _point_ ) As Double

    Dim As UInteger i
    Dim As Double sum

    For i = 1 To UBound(p) -1
        sum += p(i   ).x * p(i +1).y
        sum -= p(i +1).x * p(i   ).y
    Next
    sum += p(i).x * p(1).y
    sum -= p(1).x * p(i).y

    Return Abs(sum) / 2
End Function

' ------=< MAIN >=------

Dim As _point_ p_array(1 To ...) = {(3,4), (5,11), (12,8), (9,5), (5,6)}

Print "The area of the polygon ="; shoelace_formula(p_array())

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
The area of the polygon = 30
```



## Go


```go
package main

import "fmt"

type point struct{ x, y float64 }

func shoelace(pts []point) float64 {
    sum := 0.
    p0 := pts[len(pts)-1]
    for _, p1 := range pts {
        sum += p0.y*p1.x - p0.x*p1.y
        p0 = p1
    }
    return sum / 2
}

func main() {
    fmt.Println(shoelace([]point{{3, 4}, {5, 11}, {12, 8}, {9, 5}, {5, 6}}))
}
```

{{out}}

```txt

30

```



## Haskell


```Haskell
main :: IO ()
main = print (shoelace [(3, 4), (5, 11), (12, 8), (9, 5), (5, 6)])

-- Calculate the area of a polygon formed by the list of coordinates
-- Coordinates are of the form (x, y).
shoelace :: [(Double, Double)] -> Double
shoelace ps = 0.5 * abs (leftSum - rightSum)
  where
    (leftSum, rightSum) = foldr calcSums (0, 0) interlaced
    calcSums ((xi, yi), (nxi, nyi)) (l, r) = (l + xi * nyi, r + nxi * yi)
    interlaced = zip ps (tail (cycle ps))
```

{{out}}

```txt
30.0
```



## J


Implementation:


```J
shoelace=:verb define
  0.5*|+/((* 1&|.)/ - (* _1&|.)/)|:y
)
```


Task example:


```J
   shoelace 3 4,5 11,12 8,9 5,:5 6
30
```


Exposition:

We start with our list of coordinate pairs


```J
   3 4,5 11,12 8,9 5,:5 6
 3  4
 5 11
12  8
 9  5
 5  6
```


But the first thing we do is transpose them so that x coordinates and y coordinates are the two items we are working with:


```j
   |:3 4,5 11,12 8,9 5,:5 6
3  5 12 9 5
4 11  8 5 6
```


We want to rotate the y list by one (in each direction) and multiply the x list items by the corresponding y list items. Something like this, for example:


```j
   3 5 12 9 5* 1|.4 11 8 5 6
33 40 60 54 20
```


Or, rephrased:


```j
   (* 1&|.)/|:3 4,5 11,12 8,9 5,:5 6
33 40 60 54 20
```


We'll be subtracting what we get when we rotate in the other direction, which looks like this:


```j
   ((* 1&|.)/ - (* _1&|.)/)|:3 4,5 11,12 8,9 5,:5 6
15 20 _72 _18 _5
```


Finally, we add up that list, take the absolute value (there are contexts where signed area is interesting - for example, some graphics application - but that was not a part of this task) and divide that by 2.


## Java

{{trans|Kotlin}}
{{works with|Java|9}}

```Java
import java.util.List;

public class ShoelaceFormula {
    private static class Point {
        int x, y;

        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public String toString() {
            return String.format("(%d, %d)", x, y);
        }
    }

    private static double shoelaceArea(List<Point> v) {
        int n = v.size();
        double a = 0.0;
        for (int i = 0; i < n - 1; i++) {
            a += v.get(i).x * v.get(i + 1).y - v.get(i + 1).x * v.get(i).y;
        }
        return Math.abs(a + v.get(n - 1).x * v.get(0).y - v.get(0).x * v.get(n - 1).y) / 2.0;
    }

    public static void main(String[] args) {
        List<Point> v = List.of(
            new Point(3, 4),
            new Point(5, 11),
            new Point(12, 8),
            new Point(9, 5),
            new Point(5, 6)
        );
        double area = shoelaceArea(v);
        System.out.printf("Given a polygon with vertices %s,%n", v);
        System.out.printf("its area is %f,%n", area);
    }
}
```

{{out}}

```txt
Given a polygon with vertices [(3, 4), (5, 11), (12, 8), (9, 5), (5, 6)],
its area is 30.000000,
```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}


```julia
"""
Assumes x,y points go around the polygon in one direction.
"""
shoelacearea(x, y) =
    abs(sum(i * j for (i, j) in zip(x, append!(y[2:end], y[1]))) -
        sum(i * j for (i, j) in zip(append!(x[2:end], x[1]), y))) / 2

x, y = [3, 5, 12, 9, 5], [4, 11, 8, 5, 6]
@show x y shoelacearea(x, y)
```


{{out}}

```txt
x = [3, 5, 12, 9, 5]
y = [4, 11, 8, 5, 6]
shoelacearea(x, y) = 30.0
```



## Kotlin


```scala
// version 1.1.3

class Point(val x: Int, val y: Int) {
    override fun toString() = "($x, $y)"
}

fun shoelaceArea(v: List<Point>): Double {
    val n = v.size
    var a = 0.0
    for (i in 0 until n - 1) {
        a += v[i].x * v[i + 1].y - v[i + 1].x * v[i].y
    }
    return Math.abs(a + v[n - 1].x * v[0].y - v[0].x * v[n -1].y) / 2.0
}

fun main(args: Array<String>) {
    val v = listOf(
        Point(3, 4), Point(5, 11), Point(12, 8), Point(9, 5), Point(5, 6)
    )
    val area = shoelaceArea(v)
    println("Given a polygon with vertices at $v,")
    println("its area is $area")
}
```


{{out}}

```txt

Given a polygon with vertices at [(3, 4), (5, 11), (12, 8), (9, 5), (5, 6)],
its area is 30.0

```



## Lua


```lua
function shoeArea(ps)
  local function det2(i,j)
    return ps[i][1]*ps[j][2]-ps[j][1]*ps[i][2]
  end
  local sum = #ps>2 and det2(#ps,1) or 0
  for i=1,#ps-1 do sum = sum + det2(i,i+1)end
  return math.abs(0.5 * sum)
end
```

Using an accumulator helper inner function

```lua
function shoeArea(ps)
  local function ssum(acc, p1, p2, ...)
    if not p2 or not p1 then
      return math.abs(0.5 * acc)
    else
      return ssum(acc + p1[1]*p2[2]-p1[2]*p2[1], p2, ...)
    end
  end
  return ssum(0, ps[#ps], table.unpack(ps))
end

local p = {{3,4}, {5,11}, {12,8}, {9,5}, {5,6}}
print(shoeArea(p))-- 30
```

both version handle special cases of less than 3 point as 0 area result.


## Mathematica

Geometry objects built-in in the Wolfram Language

```Mathematica
Area[Polygon[{{3, 4}, {5, 11}, {12, 8}, {9, 5}, {5, 6}}]]
```

{{out}}

```txt
30
```



## min

{{works with|min|0.19.3}}

```min
((((first) map) ((last) map)) cleave) :dezip
(((first) (rest)) cleave append) :rotate
((0 <) (-1 *) when) :abs

(
  =b =a a size :n 0 :i () =list
  (i n <) (
    a i get b i get ' prepend list append #list
    i succ @i
  ) while list
) :rezip

(rezip (-> *) map sum) :cross-sum

(
  ((dezip rotate) (dezip swap rotate)) cleave
  ((id) (cross-sum) (id) (cross-sum)) spread
  - abs 2 /
) :shoelace

((3 4) (5 11) (12 8) (9 5) (5 6)) shoelace print
```

{{out}}

```txt

30.0

```



## MiniScript


```MiniScript
shoelace = function(vertices)
    sum = 0
    points = vertices.len

    for i in range(0,points-2)
        sum = sum + vertices[i][0]*vertices[i+1][1]
    end for
    sum = sum + vertices[points-1][0]*vertices[0][1]

    for i in range(points-1,1)
        sum = sum - vertices[i][0]*vertices[i-1][1]
    end for
    sum = sum - vertices[0][0]*vertices[points-1][1]

    return abs(sum)/2
end function

verts = [[3,4],[5,11],[12,8],[9,5],[5,6]]

print "The polygon area is "  + shoelace(verts)

```

{{out}}

```txt

The polygon area is 30

```


=={{header|Modula-2}}==

```modula2
MODULE ShoelaceFormula;
FROM RealStr IMPORT RealToStr;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE
    Point = RECORD
        x,y : INTEGER;
    END;

PROCEDURE PointToString(self : Point; VAR buf : ARRAY OF CHAR);
BEGIN
    FormatString("(%i, %i)", buf, self.x, self.y);
END PointToString;

PROCEDURE ShoelaceArea(v : ARRAY OF Point) : REAL;
VAR
    a : REAL;
    i,n : INTEGER;
BEGIN
    n := HIGH(v);
    a := 0.0;
    FOR i:=0 TO n-1 DO
        a := a + FLOAT(v[i].x * v[i+1].y - v[i+1].x * v[i].y);
    END;
    RETURN ABS(a + FLOAT(v[n].x * v[0].y - v[0].x * v[n].y)) / 2.0;
END ShoelaceArea;

VAR
    v : ARRAY[0..4] OF Point;
    buf : ARRAY[0..63] OF CHAR;
    area : REAL;
    i : INTEGER;
BEGIN
    v[0] := Point{3,4};
    v[1] := Point{5,11};
    v[2] := Point{12,8};
    v[3] := Point{9,5};
    v[4] := Point{5,6};
    area := ShoelaceArea(v);

    WriteString("Given a polygon with verticies ");
    FOR i:=0 TO HIGH(v) DO
        PointToString(v[i], buf);
        WriteString(buf);
        WriteString(" ");
    END;
    WriteLn;

    RealToStr(area, buf);
    WriteString("its area is ");
    WriteString(buf);
    WriteLn;

    ReadChar;
END ShoelaceFormula.
```



## Perl


```perl
use strict;
use warnings;
use feature 'say';

sub area_by_shoelace {
    my $area;
    our @p;
    $#_ > 0 ? @p = @_ : (local *p = shift);
    $area += $p[$_][0] * $p[($_+1)%@p][1] for 0 .. @p-1;
    $area -= $p[$_][1] * $p[($_+1)%@p][0] for 0 .. @p-1;
    return abs $area/2;
}

my @poly = ( [3,4], [5,11], [12,8], [9,5], [5,6] );

say area_by_shoelace(   [3,4], [5,11], [12,8], [9,5], [5,6]   );
say area_by_shoelace( [ [3,4], [5,11], [12,8], [9,5], [5,6] ] );
say area_by_shoelace(  @poly );
say area_by_shoelace( \@poly );
```

{{out}}

```txt
30
30
30
30
```



## Perl 6


### Index and mod offset

{{works with|Rakudo|2017.07}}


```perl6
sub area-by-shoelace(@p) {
    (^@p).map({@p[$_;0] * @p[($_+1)%@p;1] - @p[$_;1] * @p[($_+1)%@p;0]}).sum.abs / 2
}

say area-by-shoelace( [ (3,4), (5,11), (12,8), (9,5), (5,6) ] );
```

{{out}}

```txt
30
```



### Slice and rotation

{{works with|Rakudo|2017.07}}

```perl6
sub area-by-shoelace ( @p ) {
    my @x := @p».[0];
    my @y := @p».[1];

    my $s := ( @x Z* @y.rotate( 1) ).sum
           - ( @x Z* @y.rotate(-1) ).sum;

    return $s.abs / 2;
}

say area-by-shoelace( [ (3,4), (5,11), (12,8), (9,5), (5,6) ] );

```

{{out}}

```txt
30
```



## Phix


```Phix
enum X, Y
function shoelace(sequence s)
    atom t = 0
    if length(s)>2 then
        s = append(s,s[1])
        for i=1 to length(s)-1 do
            t += s[i][X]*s[i+1][Y] - s[i+1][X]*s[i][Y]
        end for
    end if
    return abs(t)/2
end function

constant test = {{3,4},{5,11},{12,8},{9,5},{5,6}}
?shoelace(test)
```

{{out}}

```txt

30

```



## PowerBASIC

{{Trans|Visual Basic}}

```powerbasic
#COMPILE EXE
#DIM ALL
#COMPILER PBCC 6

FUNCTION ShoelaceArea(x() AS DOUBLE, y() AS DOUBLE) AS DOUBLE
LOCAL i, j AS LONG
LOCAL Area AS DOUBLE

  j = UBOUND(x())
  FOR i = LBOUND(x()) TO UBOUND(x())
    Area += (y(j) + y(i)) * (x(j) - x(i))
    j = i
  NEXT i
  FUNCTION = ABS(Area) / 2
END FUNCTION

FUNCTION PBMAIN () AS LONG
  REDIM x(0 TO 4) AS DOUBLE, y(0 TO 4) AS DOUBLE
  ARRAY ASSIGN x() = 3, 5, 12, 9, 5
  ARRAY ASSIGN y() = 4, 11, 8, 5, 6
  CON.PRINT STR$(ShoelaceArea(x(), y()))
  CON.WAITKEY$
END FUNCTION
```

{{out}}

```txt
30
```



## Python


```python>>>
 def area_by_shoelace(x, y):
    "Assumes x,y points go around the polygon in one direction"
    return abs( sum(i * j for i, j in zip(x,             y[1:] + y[:1]))
               -sum(i * j for i, j in zip(x[1:] + x[:1], y            ))) / 2

>>> points = [(3,4), (5,11), (12,8), (9,5), (5,6)]
>>> x, y = zip(*points)
>>> area_by_shoelace(x, y)
30.0
>>>
```



Or, defined in terms of '''reduce''' and '''cycle''':
{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Polygonal area by shoelace formula'''

from itertools import (cycle, islice)
from functools import (reduce)


# shoelaceArea :: [(Float, Float)] -> Float
def shoelaceArea(xys):
    '''Area of polygon with vertices
       at (x, y) points in xys.
    '''
    def go(a, tpl):
        l, r = a
        (x, y), (dx, dy) = tpl
        return (l + x * dy, r + y * dx)

    (nl, nr) = reduce(
        go,
        zip(xys, tail(cycle(xys))),
        (0, 0)
    )
    return abs(nl - nr) / 2


# TEST ----------------------------------------------------
# main :: IO()
def main():
    '''Sample calculation'''

    ps = [(3, 4), (5, 11), (12, 8), (9, 5), (5, 6)]
    print(__doc__ + ':')
    print(repr(ps) + '  ->  ' + str(shoelaceArea(ps)))


# GENERIC -------------------------------------------------

# tail :: [a] -> [a]
# tail :: Gen [a] -> [a]
def tail(xs):
    '''The elements following the head of a
       (non-empty) list or generator stream.
    '''
    if isinstance(xs, list):
        return xs[1:]
    else:
        list(islice(xs, 1))  # First item dropped.
        return xs


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Polygonal area by shoelace formula:
[(3, 4), (5, 11), (12, 8), (9, 5), (5, 6)]  ->  30.0
```



## Racket


```racket
#lang racket/base

(struct P (x y))

(define (area . Ps)
  (define (A P-a P-b)
    (+ (for/sum ((p_i Ps)
                 (p_i+1 (in-sequences (cdr Ps)
                                      (in-value (car Ps)))))
         (* (P-a p_i) (P-b p_i+1)))))
  (/ (abs (- (A P-x P-y) (A P-y P-x))) 2))

(module+ main
  (area (P 3 4) (P 5 11) (P 12 8) (P 9 5) (P 5 6)))
```


{{out}}

```txt
30
```



## REXX


### endpoints as exceptions


```rexx
/*REXX program uses a  Shoelace  formula to calculate the area of an  N-sided  polygon. */
parse arg pts;           $polygon = 'polygon area of '  /*get optional args from the CL.*/
if pts=''  then pts= '(3,4),(5,11),(12,8),(9,5),(5,6)'  /*Not specified?   Use default. */
                         @= pts                         /*elide extra blanks;  save pts.*/
           do #=1  until @=''                           /*perform destructive parse on @*/
           parse var @  '('  x.#  ","  y.#  ')'  ","  @ /*obtain  X  and  Y  coördinates*/
           end   /*#*/
A= 0                                                    /*initialize the  area  to zero.*/
           do j=1  for #;  jp=j+1;  if jp>#   then jp=1 /*adjust for  J  for overflow.  */
                           jm=j-1;  if jm==0  then jm=# /*   "    "   "   "  underflow. */
           A=A + x.j * (y.jp - y.jm)                    /*compute a part of the area.   */
           end   /*j*/
say $polygon  #  " points: " pts '  is ───► '  abs(A/2) /*stick a fork in it, we're done*/
```

{{out|output|text=  when using the default input:}}

```txt

polygon area of  5  points:  (3,4),(5,11),(12,8),(9,5),(5,6)   is ───►  30

```


===endpoints as wrap-around===
This REXX version uses a different method to define the   <big>'''X<sub>0</sub>,   Y<sub>0</sub>'''</big>,   and   <big>'''X<sub>n+1</sub>''',   '''Y<sub>n+1</sub>'''</big>   data points   (and not treat them as exceptions).

When calculating the area for many polygons   (or where the number of polygon sides is large),   this method would be faster.

```rexx
/*REXX program uses  a  Shoelace  formula to calculate the area of an  N-sided  polygon.*/
parse arg pts;           $polygon = 'polygon area of '  /*get optional args from the CL.*/
if pts=''  then pts= "(3,4),(5,11),(12,8),(9,5),(5,6)"  /*Not specified?   Use default. */
A= 0;                    @= space(pts, 0)               /*init A; elide blanks from pts.*/
           do #=1  until @==''                          /*perform destructive parse on @*/
           parse var @  '('  x.#  ","  y.#  ')'  ","  @ /*obtain  X  and  Y  coördinates*/
           end   /*n*/
e= #+1;                parse value y.1 y.# with y.e y.0 /*define  Y.n+1  &  Y.0  points.*/
           do j=1  for #;     jm= j - 1;     jp= j + 1  /*compute  J-1  &  J+1  indices.*/
           A=A   +   x.j * (y.jm  -  y.jp)              /*compute a portion of the area.*/
           end   /*j*/                                  /* [↓]  use one half of  | A |  */
say $polygon  #  " points: " pts '  is ───► '  abs(A/2) /*stick a fork in it, we're done*/
```

{{out|output|text=  is the same as the 1<sup>st</sup> REXX version.}}




### somewhat simplified

reformatted and suitable for ooRexx. (x.0 etc. not needed)
<lang>/*REXX program uses a  Shoelace  formula to calculate the area of an  N-sided  polygon. */
parse arg pts                                    /*obtain optional arguments from the CL*/
if pts='' then pts= '(3,4),(5,11),(12,8),(9,5),(5,6)'   /*Not specified?   Use default. */
pts=space(pts,0); z=pts                                 /*elide extra blanks;  save pts.*/
do n=1 until z=''                                       /*perform destructive parse on z*/
  parse var z '(' x.n ',' y.n ')' ',' z                 /*obtain X and Y coördinates    */
  end
z=n+1; y.z=y.1                                          /* take care of end points      */
       y.0=y.n
A=0                                                     /*initialize the  area  to zero.*/
do j=1 for n;
  jp=j+1;
  jm=j-1;
  A=A+x.j*(y.jp-y.jm)                                   /*compute a part of the area.   */
  end
A=abs(A/2)                                              /*obtain half of the  ¦ A ¦  sum*/
say 'polygon area of' n 'points:' pts 'is --->' A
```

{{out}}

```txt
polygon area of 5 points: (3,4),(5,11),(12,8),(9,5),(5,6) is ---> 30
```



### even simpler

Using the published algorithm
<lang>/*REXX program uses a  Shoelace  formula to calculate the area of an  N-sided  polygon. */
parse arg pts                                    /*obtain optional arguments from the CL*/
if pts='' then pts= '(3,4),(5,11),(12,8),(9,5),(5,6)'   /*Not specified?   Use default. */
pts=space(pts,0); z=pts                                 /*elide extra blanks;  save pts.*/
do n=1 until z=''                                       /*perform destructive parse on z*/
  parse var z '(' x.n ',' y.n ')' ',' z                 /*obtain X and Y coördinates    */
  end
a=0
Do i=1 To n-1
  j=i+1
  a=a+x.i*y.j-x.j*y.i
  End
a=a+x.n*y.1-x.1*y.n
a=abs(a)/2
say 'polygon area of' n 'points:' pts 'is --->' a
```

{{out}}

```txt
polygon area of 5 points: (3,4),(5,11),(12,8),(9,5),(5,6) is ---> 30
```



## Ring


```ring

# Project : Shoelace formula for polygonal area

p = [[3,4], [5,11], [12,8], [9,5], [5,6]]
see "The area of the polygon = " + shoelace(p)

func shoelace(p)
        sum = 0
        for i = 1 to len(p) -1
             sum = sum + p[i][1] * p[i +1][2]
             sum = sum - p[i +1][1] * p[i][2]
        next
        sum = sum + p[i][1] * p[1][2]
        sum = sum - p[1][1] * p[i][2]
        return fabs(sum) / 2

```

Output:

```txt

The area of the polygon = 30

```



## Ruby


```ruby

Point = Struct.new(:x,:y) do

  def shoelace(other)
    x * other.y - y * other.x
  end

end

class Polygon

  def initialize(*coords)
    @points = coords.map{|c| Point.new(*c) }
  end

  def area
    points = @points + [@points.first]
    points.each_cons(2).sum{|p1,p2| p1.shoelace(p2) }.abs.fdiv(2)
  end

end

puts Polygon.new([3,4], [5,11], [12,8], [9,5], [5,6]).area  # => 30.0

```



## Scala


```scala
case class Point( x:Int,y:Int ) { override def toString = "(" + x + "," + y + ")" }

case class Polygon( pp:List[Point] ) {
  require( pp.size > 2, "A Polygon must consist of more than two points" )

  override def toString = "Polygon(" + pp.mkString(" ", ", ", " ") + ")"

  def area = {

    // Calculate using the Shoelace Formula
    val xx = pp.map( p => p.x )
    val yy = pp.map( p => p.y )
    val overlace = xx zip yy.drop(1)++yy.take(1)
    val underlace = yy zip xx.drop(1)++xx.take(1)

    (overlace.map( t => t._1 * t._2 ).sum - underlace.map( t => t._1 * t._2 ).sum).abs / 2.0
  }
}

// A little test...
{
val p = Polygon( List( Point(3,4), Point(5,11), Point(12,8), Point(9,5), Point(5,6) ) )

assert( p.area == 30.0 )

println( "Area of " + p + " = " + p.area )
}

```

{{out}}

```txt
Area of Polygon( (3,4), (5,11), (12,8), (9,5), (5,6) ) = 30.0
```



## Sidef

{{trans|Perl 6}}

```ruby
func area_by_shoelace (*p) {
    var x = p.map{_[0]}
    var y = p.map{_[1]}

    var s = (
        (x ~Z* y.rotate(+1)).sum -
        (x ~Z* y.rotate(-1)).sum
    )

    s.abs / 2
}

say area_by_shoelace([3,4], [5,11], [12,8], [9,5], [5,6])
```

{{out}}

```txt

30

```



## Swift


{{trans|Scala}}


```swift
import Foundation

struct Point {
  var x: Double
  var y: Double
}

extension Point: CustomStringConvertible {
  var description: String {
    return "Point(x: \(x), y: \(y))"
  }
}

struct Polygon {
  var points: [Point]

  var area: Double {
    let xx = points.map({ $0.x })
    let yy = points.map({ $0.y })
    let overlace = zip(xx, yy.dropFirst() + yy.prefix(1)).map({ $0.0 * $0.1 }).reduce(0, +)
    let underlace = zip(yy, xx.dropFirst() + xx.prefix(1)).map({ $0.0 * $0.1 }).reduce(0, +)

    return abs(overlace - underlace) / 2
  }

  init(points: [Point]) {
    self.points = points
  }

  init(points: [(Double, Double)]) {
    self.init(points: points.map({ Point(x: $0.0, y: $0.1) }))
  }
}

let poly = Polygon(points: [
  (3,4),
  (5,11),
  (12,8),
  (9,5),
  (5,6)
])

print("\(poly) area = \(poly.area)")
```


{{out}}


```txt
Polygon(points: [Point(x: 3.0, y: 4.0), Point(x: 5.0, y: 11.0), Point(x: 12.0, y: 8.0), Point(x: 9.0, y: 5.0), Point(x: 5.0, y: 6.0)]) area = 30.0
```


=={{header|TI-83 BASIC}}==
{{works with|TI-83 BASIC|TI-84Plus 2.55MP}}

```ti83b
[[3,4][5,11][12,8][9,5][5,6]]->[A]
Dim([A])->N:0->A
For(I,1,N)
    I+1->J:If J>N:Then:1->J:End
    A+[A](I,1)*[A](J,2)-[A](J,1)*[A](I,2)->A
End
Abs(A)/2->A
```

{{out}}

```txt

          30

```




## VBA

{{trans|Phix}}
```vb
Option Base 1
Public Enum axes
    u = 1
    v
End Enum
Private Function shoelace(s As Collection) As Double
    Dim t As Double
    If s.Count > 2 Then
        s.Add s(1)
        For i = 1 To s.Count - 1
            t = t + s(i)(u) * s(i + 1)(v) - s(i + 1)(u) * s(i)(v)
        Next i
    End If
    shoelace = Abs(t) / 2
End Function

Public Sub polygonal_area()
    Dim task() As Variant
    task = [{3,4;5,11;12,8;9,5;5,6}]
    Dim tcol As New Collection
    For i = 1 To UBound(task)
        tcol.Add Array(task(i, u), task(i, v))
    Next i
    Debug.Print shoelace(tcol)
End Sub
```
{{out}}

```txt
30
```



## VBScript


```vb
' Shoelace formula for polygonal area - VBScript
    Dim points, x(),y()
    points = Array(3,4, 5,11, 12,8, 9,5, 5,6)
    n=(UBound(points)+1)\2
    Redim x(n+1),y(n+1)
    j=0
    For i = 1 To n
        x(i)=points(j)
        y(i)=points(j+1)
        j=j+2
    Next 'i
    x(i)=points(0)
    y(i)=points(1)
    For i = 1 To n
        area = area + x(i)*y(i+1) - x(i+1)*y(i)
    Next 'i
    area = Abs(area)/2
    msgbox area,,"Shoelace formula"
```

{{out}}

```txt

30

```



## Visual Basic

{{works with|Visual Basic|5}}
{{works with|Visual Basic|6}}
{{works with|VBA|Access 97}}
{{works with|VBA|6.5}}
{{works with|VBA|7.1}}

```vb
Option Explicit

Public Function ShoelaceArea(x() As Double, y() As Double) As Double
Dim i As Long, j As Long
Dim Area As Double

  j = UBound(x())
  For i = LBound(x()) To UBound(x())
    Area = Area + (y(j) + y(i)) * (x(j) - x(i))
    j = i
  Next i
  ShoelaceArea = Abs(Area) / 2
End Function

Sub Main()
Dim v As Variant
Dim n As Long, i As Long, j As Long
  v = Array(3, 4, 5, 11, 12, 8, 9, 5, 5, 6)
  n = (UBound(v) - LBound(v) + 1) \ 2 - 1
  ReDim x(0 To n) As Double, y(0 To n) As Double
  j = 0
  For i = 0 To n
    x(i) = v(j)
    y(i) = v(j + 1)
    j = j + 2
  Next i
  Debug.Print ShoelaceArea(x(), y())
End Sub
```

{{out}}

```txt
30
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Option Strict On

Imports Point = System.Tuple(Of Double, Double)

Module Module1

    Function ShoelaceArea(v As List(Of Point)) As Double
        Dim n = v.Count
        Dim a = 0.0
        For i = 0 To n - 2
            a += v(i).Item1 * v(i + 1).Item2 - v(i + 1).Item1 * v(i).Item2
        Next
        Return Math.Abs(a + v(n - 1).Item1 * v(0).Item2 - v(0).Item1 * v(n - 1).Item2) / 2.0
    End Function

    Sub Main()
        Dim v As New List(Of Point) From {
            New Point(3, 4),
            New Point(5, 11),
            New Point(12, 8),
            New Point(9, 5),
            New Point(5, 6)
        }
        Dim area = ShoelaceArea(v)
        Console.WriteLine("Given a polygon with vertices [{0}],", String.Join(", ", v))
        Console.WriteLine("its area is {0}.", area)
    End Sub

End Module
```

{{out}}

```txt
Given a polygon with vertices [(3, 4), (5, 11), (12, 8), (9, 5), (5, 6)],
its area is 30.
```



## zkl

By the "book":

```zkl
fcn areaByShoelace(points){	// ( (x,y),(x,y)...)
   xs,ys:=Utils.Helpers.listUnzip(points); // (x,x,...), (y,y,,,)
   ( xs.zipWith('*,ys[1,*]).sum(0) + xs[-1]*ys[0] -
     xs[1,*].zipWith('*,ys).sum(0) - xs[0]*ys[-1] )
   .abs().toFloat()/2;
}
```

or an iterative solution:

```zkl
fcn areaByShoelace2(points){	// ( (x,y),(x,y)...)
   xs,ys:=Utils.Helpers.listUnzip(points); // (x,x,...), (y,y,,,)
   N:=points.len();
   N.reduce('wrap(s,n){ s + xs[n]*ys[(n+1)%N] - xs[(n+1)%N]*ys[n] },0)
   .abs().toFloat()/2;
}
```


```zkl
points:=T(T(3,4), T(5,11), T(12,8), T(9,5), T(5,6));
areaByShoelace(points).println();
areaByShoelace2(points).println();
```

{{out}}

```txt

30
30

```

