+++
title = "Find the intersection of two lines"
description = ""
date = 2019-08-05T11:43:00Z
aliases = []
[extra]
id = 21226
[taxonomies]
categories = []
tags = []
+++

[[Category:Geometry]]
[[Category:Collision detection]]
{{task}}Finding the intersection of two lines that are in the same plane is an important topic in collision detection.<ref>[http://mathworld.wolfram.com/Line-LineIntersection.html]</ref>


;Task:
Find the point of intersection of two lines in 2D.


The 1<sup>st</sup>     line passes though   <big> (4,0) </big>   and   <big> (6,10)</big> .

The 2<sup>nd</sup> line passes though   <big> (0,3) </big>   and   <big> (10,7)</big> .





## 360 Assembly

{{trans|Rexx}}

```360asm
*        Intersection of two lines   01/03/2019
INTERSEC CSECT
         USING  INTERSEC,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LE     F0,XA              xa
       IF    CE,F0,EQ,XB THEN      if xa=xb then
         STE    F0,X1                x1=xa
         LE     F0,YA
       IF    CE,F0,EQ,YB THEN        if ya=yb then
         MVI    MSG,C'='               msg='='
       ENDIF    ,                    endif
       ELSE     ,                  else
         MVI    FK1,X'01'            fk1=true
         LE     F0,YB
         SE     F0,YA                yb-ya
         LE     F2,XB
         SE     F2,XA                xb-xa
         DER    F0,F2                /
         STE    F0,K1                k1=(yb-ya)/(xb-xa)
         ME     F0,XA                k1*xa
         LE     F2,YA                ya
         SER    F2,F0                -
         STE    F2,D1                d1=ya-k1*xa
       ENDIF    ,                  endif
         LE     F0,XC
       IF    CE,F0,EQ,XD THEN      if xc=xd then
         STE    F0,X2                x2=xc
         LE     F4,YC                yc
       IF    CE,F4,EQ,YD THEN        if yc=yd then
         MVI    MSG,C'='               msg='='
       ENDIF    ,                    endif
       ELSE     ,                  else
         MVI    FK2,X'01'            fk2=true
         LE     F0,YD
         SE     F0,YC                yd-yc
         LE     F2,XD
         SE     F2,XC                xd-xc
         DER    F0,F2                /
         STE    F0,K2                k2=(yd-yc)/(xd-xc)
         ME     F0,XC                k2*xc
         LE     F2,YC                yc
         SER    F2,F0                -
         STE    F2,D2                d2=yc-k2*xc
       ENDIF    ,                  endif
       IF   CLI,MSG,EQ,C' ' THEN   if msg=' ' then
       IF   CLI,FK1,EQ,X'00' THEN    if not fk1 then
       IF   CLI,FK2,EQ,X'00' THEN      if not fk2 then
         LE     F4,X1
       IF    CE,F4,EQ,X2                 if x1=x2 then
         MVI    MSG,C'='                   msg='='
       ELSE     ,                        else
         MVI    MSG,C'/'                   msg='/'
       ENDIF    ,                        endif
       ELSE     ,                      else
         LE     F0,X1
         STE    F0,X                     x=x1
         LE     F0,K2                    k2
         ME     F0,X                     *x
         AE     F0,D2                    +d2
         STE    F0,Y                     y=k2*x+d2
       ENDIF    ,                      endif
       ELSE     ,                    else
       IF    CLI,FK2,EQ,X'00' THEN     if not fk2 then
         LE     F0,X2
         STE    F0,X                     x=x2
         LE     F0,K1                    k1
         ME     F0,X                     *x
         AE     F0,D1                    +d1
         STE    F0,Y                     y=k1*x+d1
       ELSE     ,                      else
         LE     F4,K1
       IF    CE,F4,EQ,K2 THEN            if k1=k2 then
         LE     F4,D1
       IF    CE,F4,EQ,D2 THEN              if d1=d2 then
         MVI    MSG,C'='                     msg='=';
       ELSE     ,                          else
         MVI    MSG,C'/'                     msg='/';
       ENDIF    ,                          endif
       ELSE     ,                        else
         LE     F0,D2                      d2
         SE     F0,D1                      -d1
         LE     F2,K1                      k1
         SE     F2,K2                      -k2
         DER    F0,F2                      /
         STE    F0,X                       x=(d2-d1)/(k1-k2)
         LE     F0,K1                      k1
         ME     F0,X                       *x
         AE     F0,D1                      +d1
         STE    F0,Y                       y=k1*x+d1
       ENDIF    ,                        endif
       ENDIF    ,                      endif
       ENDIF    ,                    endif
       ENDIF    ,                  endif
       IF   CLI,MSG,EQ,C' ' THEN   if msg=' ' then
         LE     F0,X                 x
         LA     R0,3                 decimal=3
         BAL    R14,FORMATF          format x
         MVC    PG+0(13),0(R1)       output x
         LE     F0,Y                 y
         LA     R0,3                 decimal=3
         BAL    R14,FORMATF          format y
         MVC    PG+13(13),0(R1)      output y
       ENDIF    ,                  endif
         MVC    PG+28(1),MSG       output msg
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
         COPY   plig\$_FORMATF.MLC
XA       DC     E'4.0'             point A
YA       DC     E'0.0'
XB       DC     E'6.0'             point B
YB       DC     E'10.0'
XC       DC     E'0.0'             point C
YC       DC     E'3.0'
XD       DC     E'10.0'            point D
YD       DC     E'7.0'
X        DS     E
Y        DS     E
X1       DS     E
X2       DS     E
K1       DS     E
K2       DS     E
D1       DS     E
D2       DS     E
FK1      DC     X'00'
FK2      DC     X'00'
MSG      DC     C' '
PG       DC     CL80' '
         REGEQU
         END    INTERSEC
```

{{out}}

```txt

        5.000        5.000

```




## Ada

{{works with|Ada|Ada|2005}}


```Ada
with Ada.Text_IO;

procedure Intersection_Of_Two_Lines
is
   Do_Not_Intersect : exception;

   type Line is record
      a : Float;
      b : Float;
   end record;

   type Point is record
      x : Float;
      y : Float;
   end record;

   function To_Line(p1, p2 : in Point) return Line
   is
      a : constant Float := (p1.y - p2.y) / (p1.x - p2.x);
      b : constant Float := p1.y - (a * p1.x);
   begin
      return (a,b);
   end To_Line;

   function Intersection(Left, Right : in Line) return Point is
   begin
      if Left.a = Right.a then
         raise Do_Not_Intersect with "The two lines do not intersect.";
      end if;

      declare
         b : constant Float := (Right.b - Left.b) / (Left.a - Right.a);
      begin
         return (b, Left.a * b + Left.b);
      end;
   end Intersection;

   A1 : constant Line := To_Line((4.0, 0.0), (6.0, 10.0));
   A2 : constant Line := To_Line((0.0, 3.0), (10.0, 7.0));
   p : constant Point := Intersection(A1, A2);
begin
   Ada.Text_IO.Put(p.x'Img);
   Ada.Text_IO.Put_Line(p.y'Img);
end Intersection_Of_Two_Lines;

```

{{out}}

```txt
 5.00000E+00 5.00000E+00

```



## ALGOL 68

Using "school maths".

```algol68
BEGIN
    # mode to hold a point #
    MODE POINT = STRUCT( REAL x, y );
    # mode to hold a line expressed as y = mx + c #
    MODE LINE  = STRUCT( REAL m, c );
    # returns the line that passes through p1 and p2 #
    PROC find line = ( POINT p1, p2 )LINE:
         IF x OF p1 = x OF p2 THEN
             # the line is vertical                 #
             LINE( 0, x OF p1 )
         ELSE
             # the line is not vertical             #
             REAL m = ( y OF p1 - y OF p2 ) / ( x OF p1 - x OF p2 );
             LINE( m, y OF p1 - ( m * x OF p1 ) )
         FI # find line # ;

    # returns the intersection of two lines - the lines must be distinct and not parallel #
    PRIO INTERSECTION = 5;
    OP   INTERSECTION = ( LINE l1, l2 )POINT:
         BEGIN
             REAL x = ( c OF l2 - c OF l1 ) / ( m OF l1 - m OF l2 );
             POINT( x, ( m OF l1 * x ) + c OF l1 )
         END # INTERSECTION # ;

    # find the intersection of the lines as per the task #
    POINT i = find line( POINT( 4.0, 0.0 ), POINT( 6.0, 10.0 ) )
              INTERSECTION find line( ( 0.0, 3.0 ), ( 10.0, 7.0 ) );
    print( ( fixed( x OF i, -8, 4 ), fixed( y OF i, -8, 4 ), newline ) )

END
```

{{out}}

```txt

  5.0000  5.0000

```


## AWK


```AWK

# syntax: GAWK -f FIND_THE_INTERSECTION_OF_TWO_LINES.AWK
# converted from Ring
BEGIN {
    intersect(4,0,6,10,0,3,10,7)
    exit(0)
}
function intersect(xa,ya,xb,yb,xc,yc,xd,yd,  errors,x,y) {
    printf("the 1st line passes through (%g,%g) and (%g,%g)\n",xa,ya,xb,yb)
    printf("the 2nd line passes through (%g,%g) and (%g,%g)\n",xc,yc,xd,yd)
    if (xb-xa == 0) { print("error: xb-xa=0") ; errors++ }
    if (xd-xc == 0) { print("error: xd-xc=0") ; errors++ }
    if (errors > 0) {
      print("")
      return(0)
    }
    printf("the two lines are:\n")
    printf("yab=%g+x*%g\n",ya-xa*((yb-ya)/(xb-xa)),(yb-ya)/(xb-xa))
    printf("ycd=%g+x*%g\n",yc-xc*((yd-yc)/(xd-xc)),(yd-yc)/(xd-xc))
    x = ((yc-xc*((yd-yc)/(xd-xc)))-(ya-xa*((yb-ya)/(xb-xa))))/(((yb-ya)/(xb-xa))-((yd-yc)/(xd-xc)))
    printf("x=%g\n",x)
    y = ya-xa*((yb-ya)/(xb-xa))+x*((yb-ya)/(xb-xa))
    printf("yab=%g\n",y)
    printf("ycd=%g\n",yc-xc*((yd-yc)/(xd-xc))+x*((yd-yc)/(xd-xc)))
    printf("intersection: %g,%g\n\n",x,y)
    return(1)
}

```

{{out}}

```txt

the 1st line passes through (4,0) and (6,10)
the 2nd line passes through (0,3) and (10,7)
the two lines are:
yab=-20+x*5
ycd=3+x*0.4
x=5
yab=5
ycd=5
intersection: 5,5

```



## BASIC

=
## Sinclair ZX81 BASIC
=
{{trans|REXX}} (version 1)
Works with 1k of RAM.

```basic
 10 LET XA=4
 20 LET YA=0
 30 LET XB=6
 40 LET YB=10
 50 LET XC=0
 60 LET YC=3
 70 LET XD=10
 80 LET YD=0
 90 PRINT "THE TWO LINES ARE:"
100 PRINT "YAB=";YA-XA*((YB-YA)/(XB-XA));"+X*";((YB-YA)/(XB-XA))
110 PRINT "YCD=";YC-XC*((YD-YC)/(XD-XC));"+X*";((YD-YC)/(XD-XC))
120 LET X=((YC-XC*((YD-YC)/(XD-XC)))-(YA-XA*((YB-YA)/(XB-XA))))/(((YB-YA)/(XB-XA))-((YD-YC)/(XD-XC)))
130 PRINT "X=";X
140 LET Y=YA-XA*((YB-YA)/(XB-XA))+X*((YB-YA)/(XB-XA))
150 PRINT "YAB=";Y
160 PRINT "YCD=";YC-XC*((YD-YC)/(XD-XC))+X*((YD-YC)/(XD-XC))
170 PRINT "INTERSECTION: ";X;",";Y
```

{{out}}

```txt
THE TWO LINES ARE:
YAB=-20+X*5
YCD=3+X*0.4
X=5
YAB=5
YCD=5
INTERSECTION: 5,5
```



## C

This implementation is generic and considers any two lines in the XY plane and not just the specified example. Usage is printed on incorrect invocation.

```C

#include<stdlib.h>
#include<stdio.h>
#include<math.h>

typedef struct{
	double x,y;
}point;

double lineSlope(point a,point b){

	if(a.x-b.x == 0.0)
		return NAN;
	else
		return (a.y-b.y)/(a.x-b.x);
}

point extractPoint(char* str){
	int i,j,start,end,length;
	char* holder;
	point c;

	for(i=0;str[i]!=00;i++){
		if(str[i]=='(')
			start = i;
		if(str[i]==','||str[i]==')')
		{
			end = i;

			length = end - start;

			holder = (char*)malloc(length*sizeof(char));

			for(j=0;j<length-1;j++)
				holder[j] = str[start + j + 1];
			holder[j] = 00;

			if(str[i]==','){
				start = i;
				c.x = atof(holder);
			}
			else
				c.y = atof(holder);
		}
	}

	return c;
}

point intersectionPoint(point a1,point a2,point b1,point b2){
	point c;

	double slopeA = lineSlope(a1,a2), slopeB = lineSlope(b1,b2);

	if(slopeA==slopeB){
		c.x = NAN;
		c.y = NAN;
	}
	else if(slopeA==NAN && slopeB!=NAN){
		c.x = a1.x;
		c.y = (a1.x-b1.x)*slopeB + b1.y;
	}
	else if(slopeB==NAN && slopeA!=NAN){
		c.x = b1.x;
		c.y = (b1.x-a1.x)*slopeA + b1.y;
	}
	else{
		c.x = (slopeA*a1.x - slopeB*b1.x + b1.y - a1.y)/(slopeA - slopeB);
		c.y = slopeB*(c.x - b1.x) + b1.y;
	}

	return c;
}

int main(int argC,char* argV[])
{
	point c;

	if(argC < 5)
		printf("Usage : %s <four points specified as (x,y) separated by a space>",argV[0]);
	else{
		c = intersectionPoint(extractPoint(argV[1]),extractPoint(argV[2]),extractPoint(argV[3]),extractPoint(argV[4]));

		if(c.x==NAN)
			printf("The lines do not intersect, they are either parallel or co-incident.");
		else
			printf("Point of intersection : (%lf,%lf)",c.x,c.y);
	}

	return 0;
}

```

Invocation and output:

```txt

C:\rosettaCode>lineIntersect.exe (4,0) (6,10) (0,3) (10,7)
Point of intersection : (5.000000,5.000000)

```



## C#


```c#
using System;
using System.Drawing;
public class Program
{
    static PointF FindIntersection(PointF s1, PointF e1, PointF s2, PointF e2) {
        float a1 = e1.Y - s1.Y;
        float b1 = s1.X - e1.X;
        float c1 = a1 * s1.X + b1 * s1.Y;

        float a2 = e2.Y - s2.Y;
        float b2 = s2.X - e2.X;
        float c2 = a2 * s2.X + b2 * s2.Y;

        float delta = a1 * b2 - a2 * b1;
        //If lines are parallel, the result will be (NaN, NaN).
        return delta == 0 ? new PointF(float.NaN, float.NaN)
            : new PointF((b2 * c1 - b1 * c2) / delta, (a1 * c2 - a2 * c1) / delta);
    }

    static void Main() {
        Func<float, float, PointF> p = (x, y) => new PointF(x, y);
        Console.WriteLine(FindIntersection(p(4f, 0f), p(6f, 10f), p(0f, 3f), p(10f, 7f)));
        Console.WriteLine(FindIntersection(p(0f, 0f), p(1f, 1f), p(1f, 2f), p(4f, 5f)));
    }
}
```

{{out}}

```txt

{X=5, Y=5}
{X=NaN, Y=NaN}

```



## C++



```cpp
#include <iostream>
#include <cmath>
#include <assert.h>
using namespace std;

/** Calculate determinant of matrix:
	[a b]
	[c d]
*/
inline double Det(double a, double b, double c, double d)
{
	return a*d - b*c;
}

///Calculate intersection of two lines.
///\return true if found, false if not found or error
bool LineLineIntersect(double x1, double y1, //Line 1 start
	double x2, double y2, //Line 1 end
	double x3, double y3, //Line 2 start
	double x4, double y4, //Line 2 end
	double &ixOut, double &iyOut) //Output
{
	double detL1 = Det(x1, y1, x2, y2);
	double detL2 = Det(x3, y3, x4, y4);
	double x1mx2 = x1 - x2;
	double x3mx4 = x3 - x4;
	double y1my2 = y1 - y2;
	double y3my4 = y3 - y4;

	double xnom = Det(detL1, x1mx2, detL2, x3mx4);
	double ynom = Det(detL1, y1my2, detL2, y3my4);
	double denom = Det(x1mx2, y1my2, x3mx4, y3my4);
	if(denom == 0.0)//Lines don't seem to cross
	{
		ixOut = NAN;
		iyOut = NAN;
		return false;
	}

	ixOut = xnom / denom;
	iyOut = ynom / denom;
	if(!isfinite(ixOut) || !isfinite(iyOut)) //Probably a numerical issue
		return false;

	return true; //All OK
}

int main()
{
	// **Simple crossing diagonal lines**

	//Line 1
	double x1=4.0, y1=0.0;
	double x2=6.0, y2=10.0;

	//Line 2
	double x3=0.0, y3=3.0;
	double x4=10.0, y4=7.0;

	double ix = -1.0, iy = -1.0;
	bool result = LineLineIntersect(x1, y1, x2, y2, x3, y3, x4, y4, ix, iy);
	cout << "result " <<  result << "," << ix << "," << iy << endl;

	double eps = 1e-6;
	assert(result == true);
	assert(fabs(ix - 5.0) < eps);
	assert(fabs(iy - 5.0) < eps);

}
```


{{out}}

```txt
result 1,5,5
```



## D

{{trans|Kotlin}}

```D
import std.stdio;

struct Point {
    real x, y;

    void toString(scope void delegate(const(char)[]) sink) const {
        import std.format;
        sink("{");
        sink.formattedWrite!"%f"(x);
        sink(", ");
        sink.formattedWrite!"%f"(y);
        sink("}");
    }
}

struct Line {
    Point s, e;
}

Point findIntersection(Line l1, Line l2) {
    auto a1 = l1.e.y - l1.s.y;
    auto b1 = l1.s.x - l1.e.x;
    auto c1 = a1 * l1.s.x + b1 * l1.s.y;

    auto a2 = l2.e.y - l2.s.y;
    auto b2 = l2.s.x - l2.e.x;
    auto c2 = a2 * l2.s.x + b2 * l2.s.y;

    auto delta = a1 * b2 - a2 * b1;
    // If lines are parallel, intersection point will contain infinite values
    return Point((b2 * c1 - b1 * c2) / delta, (a1 * c2 - a2 * c1) / delta);
}

void main() {
    auto l1 = Line(Point(4.0, 0.0), Point(6.0, 10.0));
    auto l2 = Line(Point(0f, 3f), Point(10f, 7f));
    writeln(findIntersection(l1, l2));
    l1 = Line(Point(0.0, 0.0), Point(1.0, 1.0));
    l2 = Line(Point(1.0, 2.0), Point(4.0, 5.0));
    writeln(findIntersection(l1, l2));
}
```


{{out}}

```txt
{5.000000, 5.000000}
{-inf, -inf}
```


=={{header|F_Sharp|F#}}==

```fsharp

(*
Find the point of intersection of 2 lines.
Nigel Galloway May 20th., 2017
*)
type Line={a:float;b:float;c:float} member N.toS=sprintf "%.2fx + %.2fy = %.2f" N.a N.b N.c
let intersect (n:Line) g = match (n.a*g.b-g.a*n.b) with
                           |0.0 ->printfn "%s does not intersect %s" n.toS g.toS
                           |ng  ->printfn "%s intersects %s at x=%.2f y=%.2f" n.toS g.toS ((g.b*n.c-n.b*g.c)/ng) ((n.a*g.c-g.a*n.c)/ng)
let fn (i,g) (e,l) = {a=g-l;b=e-i;c=(e-i)*g+(g-l)*i}
intersect (fn (4.0,0.0) (6.0,10.0)) (fn (0.0,3.0) (10.0,7.0))
intersect {a=3.18;b=4.23;c=7.13} {a=6.36;b=8.46;c=9.75}

```

{{out}}

```txt

-10.00x + 2.00y = -40.00 intersects -4.00x + 10.00y = 30.00 at x=5.00 y=5.00
3.18x + 4.23y = 7.13 does not intersect 6.36x + 8.46y = 9.75

```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program intersect_two_lines
  implicit none

  type point
    real::x,y
  end type point

  integer, parameter :: n = 4
  type(point)        :: p(n)

  p(1)%x = 4; p(1)%y = 0; p(2)%x = 6;  p(2)%y = 10 ! fist line
  p(3)%x = 0; p(3)%y = 3; p(4)%x = 10; p(4)%y = 7  ! second line

  call intersect(p, n)

  contains

  subroutine intersect(p,m)
  integer, intent(in)       :: m
  type(point), intent(in)   :: p(m)
  integer   :: i
  real      :: a(2), b(2) ! y = a*x + b, for each line
  real      :: x, y       ! intersect point
  real      :: dx,dy      ! working variables

  do i = 1, 2
    dx = p(2*i-1)%x - p(2*i)%x
    dy = p(2*i-1)%y - p(2*i)%y
    if( dx == 0.) then    ! in case this line is of the form y = b
        a(i) = 0.
        b(i) = p(2*i-1)%y
    else
        a(i)= dy / dx
        b(i) = p(2*i-1)%y - a(i)*p(2*i-1)%x
    endif
  enddo

  if( a(1) - a(2) == 0. ) then
    write(*,*)"lines are not intersecting"
    return
  endif

  x = ( b(2) - b(1) ) / ( a(1) - a(2) )
  y = a(1) * x + b(1)
  write(*,*)x,y
  end subroutine intersect
end program intersect_two_lines
```

{{out}}

```txt
 5.00000000       5.00000000
```



## FreeBASIC


```freebasic
' version 16-08-2017
' compile with: fbc -s console
#Define NaN 0 / 0   ' FreeBASIC returns -1.#IND

Type _point_
    As Double x, y
End Type

Function l_l_intersect(s1 As _point_, e1 As _point_, s2 As _point_, e2 As _point_) As _point_

    Dim As Double a1 = e1.y - s1.y
    Dim As Double b1 = s1.x - e1.x
    Dim As Double c1 = a1 * s1.x + b1 * s1.y
    Dim As Double a2 = e2.y - s2.y
    Dim As Double b2 = s2.x - e2.x
    Dim As Double c2 = a2 * s2.x + b2 * s2.y
    Dim As Double det = a1 * b2 - a2 * b1

    If det = 0 Then
        Return Type(NaN, NaN)
    Else
        Return Type((b2 * c1 - b1 * c2) / det, (a1 * c2 - a2 * c1) / det)
    End If

End Function

' ------=< MAIN >=------

Dim As _point_ s1, e1, s2, e2, answer

s1.x = 4.0 : s1.y = 0.0 : e1.x =  6.0 : e1.y = 10.0  ' start and end of first line
s2.x = 0.0 : s2.y = 3.0 : e2.x = 10.0 : e2.y =  7.0  ' start and end of second line
answer = l_l_intersect(s1, e1, s2, e2)
Print answer.x, answer.y

s1.x = 0.0 : s1.y = 0.0 : e1.x =  0.0 : e1.y =  0.0  ' start and end of first line
s2.x = 0.0 : s2.y = 3.0 : e2.x = 10.0 : e2.y =  7.0  ' start and end of second line
answer = l_l_intersect(s1, e1, s2, e2)
Print answer.x, answer.y

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
 5              5
-1.#IND       -1.#IND
```


## Go


```go

package main

import (
	"fmt"
	"errors"
)

type Point struct {
	x float64
	y float64
}

type Line struct {
	slope float64
	yint float64
}

func CreateLine (a, b Point) Line {
	slope := (b.y-a.y) / (b.x-a.x)
	yint := a.y - slope*a.x
	return Line{slope, yint}
}

func EvalX (l Line, x float64) float64 {
	return l.slope*x + l.yint
}

func Intersection (l1, l2 Line) (Point, error) {
	if l1.slope == l2.slope {
		return Point{}, errors.New("The lines do not intersect")
	}
	x := (l2.yint-l1.yint) / (l1.slope-l2.slope)
	y := EvalX(l1, x)
	return Point{x, y}, nil
}

func main() {
	l1 := CreateLine(Point{4, 0}, Point{6, 10})
	l2 := CreateLine(Point{0, 3}, Point{10, 7})
	if result, err := Intersection(l1, l2); err == nil {
		fmt.Println(result)
	} else {
		fmt.Println("The lines do not intersect")
	}
}

```

{{Out}}

```txt
{5 5}
```



## Haskell


```haskell
type Line = (Point, Point)

type Point = (Float, Float)

intersection :: Line -> Line -> Either String Point
intersection ab pq =
  case determinant of
    0 -> Left "(Parallel lines – no intersection)"
    _ ->
      let [abD, pqD] = (\(a, b) -> diff ([fst, snd] <*> [a, b])) <$> [ab, pq]
          [ix, iy] =
            [\(ab, pq) -> diff [abD, ab, pqD, pq] / determinant] <*>
            [(abDX, pqDX), (abDY, pqDY)]
      in Right (ix, iy)
  where
    delta f x = f (fst x) - f (snd x)
    diff [a, b, c, d] = a * d - b * c
    [abDX, pqDX, abDY, pqDY] = [delta fst, delta snd] <*> [ab, pq]
    determinant = diff [abDX, abDY, pqDX, pqDY]

-- TEST ----------------------------------------------------------------
ab :: Line
ab = ((4.0, 0.0), (6.0, 10.0))

pq :: Line
pq = ((0.0, 3.0), (10.0, 7.0))

interSection :: Either String Point
interSection = intersection ab pq

main :: IO ()
main =
  putStrLn $
  case interSection of
    Left x -> x
    Right x -> show x
```

{{Out}}

```txt
(5.0,5.0)
```



## J

{{trans|C++}}
'''Solution:'''

```j
det=: -/ .*   NB. calculate determinant
findIntersection=: (det ,."1 [: |: -/"2) %&det -/"2
```


'''Examples:'''

```j
   line1=: 4 0 ,: 6 10
   line2=: 0 3 ,: 10 7
   line3=: 0 3 ,: 10 7.1
   line4=: 0 0 ,: 1 1
   line5=: 1 2 ,: 4 5
   line6=: 1 _1 ,: 4 4
   line7=: 2 5 ,: 3 _2

   findIntersection line1 ,: line2
5 5
   findIntersection line1 ,: line3
5.01089 5.05447
   findIntersection line4 ,: line5
__ __
   findIntersection line6 ,: line7
2.5 1.5
```



## Java

{{trans|Kotlin}}

```Java
public class Intersection {
    private static class Point {
        double x, y;

        Point(double x, double y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public String toString() {
            return String.format("{%f, %f}", x, y);
        }
    }

    private static class Line {
        Point s, e;

        Line(Point s, Point e) {
            this.s = s;
            this.e = e;
        }
    }

    private static Point findIntersection(Line l1, Line l2) {
        double a1 = l1.e.y - l1.s.y;
        double b1 = l1.s.x - l1.e.x;
        double c1 = a1 * l1.s.x + b1 * l1.s.y;

        double a2 = l2.e.y - l2.s.y;
        double b2 = l2.s.x - l2.e.x;
        double c2 = a2 * l2.s.x + b2 * l2.s.y;

        double delta = a1 * b2 - a2 * b1;
        return new Point((b2 * c1 - b1 * c2) / delta, (a1 * c2 - a2 * c1) / delta);
    }

    public static void main(String[] args) {
        Line l1 = new Line(new Point(4, 0), new Point(6, 10));
        Line l2 = new Line(new Point(0, 3), new Point(10, 7));
        System.out.println(findIntersection(l1, l2));

        l1 = new Line(new Point(0, 0), new Point(1, 1));
        l2 = new Line(new Point(1, 2), new Point(4, 5));
        System.out.println(findIntersection(l1, l2));
    }
}
```

{{out}}

```txt
{5.000000, 5.000000}
{-Infinity, -Infinity}
```



## JavaScript

{{Trans|Haskell}}

### ES6


```JavaScript
(() => {
    'use strict';
    // INTERSECTION OF TWO LINES ----------------------------------------------

    // intersection :: Line -> Line -> Either String (Float, Float)
    const intersection = (ab, pq) => {
        const
            delta = f => x => f(fst(x)) - f(snd(x)),
            [abDX, pqDX, abDY, pqDY] = apList(
                [delta(fst), delta(snd)], [ab, pq]
            ),
            determinant = abDX * pqDY - abDY * pqDX;

        return determinant !== 0 ? Right((() => {
            const [abD, pqD] = map(
                ([a, b]) => fst(a) * snd(b) - fst(b) * snd(a),
                [ab, pq]
            );
            return apList(
                [([pq, ab]) =>
                    (abD * pq - ab * pqD) / determinant
                ], [
                    [pqDX, abDX],
                    [pqDY, abDY]
                ]
            );
        })()) : Left('(Parallel lines – no intersection)');
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // Left :: a -> Either a b
    const Left = x => ({
        type: 'Either',
        Left: x
    });

    // Right :: b -> Either a b
    const Right = x => ({
        type: 'Either',
        Right: x
    });

    // A list of functions applied to a list of arguments
    // <*> :: [(a -> b)] -> [a] -> [b]
    const apList = (fs, xs) => //
        [].concat.apply([], fs.map(f => //
            [].concat.apply([], xs.map(x => [f(x)]))));

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // show :: a -> String
    const show = x => JSON.stringify(x); //, null, 2);


    // TEST --------------------------------------------------

    // lrIntersection ::Either String Point
    const lrIntersection = intersection([
        [4.0, 0.0],
        [6.0, 10.0]
    ], [
        [0.0, 3.0],
        [10.0, 7.0]
    ]);
    return show(lrIntersection.Left || lrIntersection.Right);
})();
```

{{Out}}

```txt
[5,5]
```



## jq

The implementation closely follows the zkl entry but uses the JSON array [x,y]
to represent the point (x,y), and an array [P1,P2] to represent the line through
points P1 and P2.  Array destructuring is used for simplicity.


```jq
# determinant of 2x2 matrix
def det(a;b;c;d): a*d - b*c ;

# Input: an array representing a line (L1)
# Output: the intersection of L1 and L2 unless the lines are judged to be parallel
# This implementation uses "destructuring" to assign local variables
def lineIntersection(L2):
  .    as [[$ax,$ay], [$bx,$by]]
  | L2 as [[$cx,$cy], [$dx,$dy]]
  | {detAB: det($ax;$ay; $bx;$by),
     detCD: det($cx;$cy; $dx;$dy),
     abDx: ($ax - $bx),
     cdDx: ($cx - $dx),
     abDy: ($ay - $by),
     cdDy: ($cy - $dy)}
  | . + {xnom:  det(.detAB;.abDx;.detCD;.cdDx),
         ynom:  det(.detAB;.abDy;.detCD;.cdDy),
         denom: det(.abDx; .abDy;.cdDx; .cdDy) }
  | if (.denom|length < 10e-6)  # length/0 emits the absolute value
    then error("lineIntersect: parallel lines")
    else [.xnom/.denom, .ynom/.denom]
    end ;
```

Example:

```jq
[[4.0, 0.0], [6.0,10.0]] | lineIntersection([[0.0, 3.0], [10.0, 7.0]])
```

{{out}}

```jq
[5,5]
```



## Julia

{{works with|Julia|0.6}}
{{trans|Kotlin}}


```julia
struct Point{T}
    x::T
    y::T
end

struct Line{T}
    s::Point{T}
    e::Point{T}
end

function intersection(l1::Line{T}, l2::Line{T}) where T<:Real
    a1 = l1.e.y - l1.s.y
    b1 = l1.s.x - l1.e.x
    c1 = a1 * l1.s.x + b1 * l1.s.y

    a2 = l2.e.y - l2.s.y
    b2 = l2.s.x - l2.e.x
    c2 = a2 * l2.s.x + b2 * l2.s.y

    Δ = a1 * b2 - a2 * b1
    # If lines are parallel, intersection point will contain infinite values
    return Point((b2 * c1 - b1 * c2) / Δ, (a1 * c2 - a2 * c1) / Δ)
end

l1 = Line(Point{Float64}(4, 0), Point{Float64}(6, 10))
l2 = Line(Point{Float64}(0, 3), Point{Float64}(10, 7))
println(intersection(l1, l2))

l1 = Line(Point{Float64}(0, 0), Point{Float64}(1, 1))
l2 = Line(Point{Float64}(1, 2), Point{Float64}(4, 5))
println(intersection(l1, l2))
```


{{out}}

```txt
Point{Float64}(5.0, 5.0)
Point{Float64}(-Inf, -Inf)
```



## Kotlin

{{trans|C#}}

```kotlin
// version 1.1.2

class PointF(val x: Float, val y: Float) {
    override fun toString() = "{$x, $y}"
}

class LineF(val s: PointF, val e: PointF)

fun findIntersection(l1: LineF, l2: LineF): PointF {
    val a1 = l1.e.y - l1.s.y
    val b1 = l1.s.x - l1.e.x
    val c1 = a1 * l1.s.x + b1 * l1.s.y

    val a2 = l2.e.y - l2.s.y
    val b2 = l2.s.x - l2.e.x
    val c2 = a2 * l2.s.x + b2 * l2.s.y

    val delta = a1 * b2 - a2 * b1
    // If lines are parallel, intersection point will contain infinite values
    return PointF((b2 * c1 - b1 * c2) / delta, (a1 * c2 - a2 * c1) / delta)
}

fun main(args: Array<String>) {
    var l1 = LineF(PointF(4f, 0f), PointF(6f, 10f))
    var l2 = LineF(PointF(0f, 3f), PointF(10f, 7f))
    println(findIntersection(l1, l2))
    l1 = LineF(PointF(0f, 0f), PointF(1f, 1f))
    l2 = LineF(PointF(1f, 2f), PointF(4f, 5f))
    println(findIntersection(l1, l2))
}
```


{{out}}

```txt

{5.0, 5.0}
{-Infinity, -Infinity}

```



## Lua

{{trans|C#}}

```lua
function intersection (s1, e1, s2, e2)
  local d = (s1.x - e1.x) * (s2.y - e2.y) - (s1.y - e1.y) * (s2.x - e2.x)
  local a = s1.x * e1.y - s1.y * e1.x
  local b = s2.x * e2.y - s2.y * e2.x
  local x = (a * (s2.x - e2.x) - (s1.x - e1.x) * b) / d
  local y = (a * (s2.y - e2.y) - (s1.y - e1.y) * b) / d
  return x, y
end

local line1start, line1end = {x = 4, y = 0}, {x = 6, y = 10}
local line2start, line2end = {x = 0, y = 3}, {x = 10, y = 7}
print(intersection(line1start, line1end, line2start, line2end))
```

{{out}}

```txt
5       5
```



## Maple


```Maple
with(geometry):
line(L1, [point(A,[4,0]), point(B,[6,10])]):
line(L2, [point(C,[0,3]), point(E,[10,7])]):
coordinates(intersection(x,L1,L2));
```

{{Output|Out}}

```txt
[5, 5]
```



## MATLAB


```MATLAB

function cross=intersection(line1,line2)
    a=polyfit(line1(:,1),line1(:,2),1);
    b=polyfit(line2(:,1),line2(:,2),1);
    cross=[a(1) -1; b(1) -1]\[-a(2);-b(2)];
end

```

{{out}}

```txt
line1=[4 0; 6 10]; line2=[0 3; 10 7]; cross=intersection(line1,line2)
cross =

    5.0000
    5.0000

```

=={{header|Modula-2}}==

```modula2
MODULE LineIntersection;
FROM RealStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE
    Point = RECORD
        x,y : REAL;
    END;

PROCEDURE PrintPoint(p : Point);
VAR buf : ARRAY[0..31] OF CHAR;
BEGIN
    WriteString("{");
    RealToStr(p.x, buf);
    WriteString(buf);
    WriteString(", ");
    RealToStr(p.y, buf);
    WriteString(buf);
    WriteString("}");
END PrintPoint;

TYPE
    Line = RECORD
        s,e : Point;
    END;

PROCEDURE FindIntersection(l1,l2 : Line) : Point;
VAR a1,b1,c1,a2,b2,c2,delta : REAL;
BEGIN
    a1 := l1.e.y - l1.s.y;
    b1 := l1.s.x - l1.e.x;
    c1 := a1 * l1.s.x + b1 * l1.s.y;

    a2 := l2.e.y - l2.s.y;
    b2 := l2.s.x - l2.e.x;
    c2 := a2 * l2.s.x + b2 * l2.s.y;

    delta := a1 * b2 - a2 * b1;
    RETURN Point{(b2 * c1 - b1 * c2) / delta, (a1 * c2 - a2 * c1) / delta};
END FindIntersection;

VAR
    l1,l2 : Line;
    result : Point;
BEGIN
    l1 := Line{{4.0,0.0}, {6.0,10.0}};
    l2 := Line{{0.0,3.0}, {10.0,7.0}};
    PrintPoint(FindIntersection(l1,l2));
    WriteLn;

    l1 := Line{{0.0,0.0}, {1.0,1.0}};
    l2 := Line{{1.0,2.0}, {4.0,5.0}};
    PrintPoint(FindIntersection(l1,l2));
    WriteLn;

    ReadChar;
END LineIntersection.
```



## Perl

{{trans|C#}}
If warning are enabled the second print will issue a warning since we are trying to print out an undef


```perl

sub intersect {
  my ($x1, $y1, $x2, $y2, $x3, $y3, $x4, $y4) = @_;
  my $a1 = $y2 - $y1;
  my $b1 = $x1 - $x2;
  my $c1 = $a1 * $x1 + $b1 * $y1;
  my $a2 = $y4 - $y3;
  my $b2 = $x3 - $x4;
  my $c2 = $a2 * $x3 + $b2 * $y3;
  my $delta = $a1 * $b2 - $a2 * $b1;
  return (undef, undef) if $delta == 0;
  # If delta is 0, i.e. lines are parallel then the below will fail
  my $ix = ($b2 * $c1 - $b1 * $c2) / $delta;
  my $iy = ($a1 * $c2 - $a2 * $c1) / $delta;
  return ($ix, $iy);
}

my ($ix, $iy) = intersect(4, 0, 6, 10, 0, 3, 10, 7);
print "$ix $iy\n";
($ix, $iy) = intersect(0, 0, 1, 1, 1, 2, 4, 5);
print "$ix $iy\n";

```



## Perl 6

{{works with|Rakudo|2016.11}}
{{trans|zkl}}


```perl6
sub intersection (Real $ax, Real $ay, Real $bx, Real $by,
                  Real $cx, Real $cy, Real $dx, Real $dy ) {

    sub term:<|AB|> { determinate($ax, $ay, $bx, $by) }
    sub term:<|CD|> { determinate($cx, $cy, $dx, $dy) }

    my $ΔxAB = $ax - $bx;
    my $ΔyAB = $ay - $by;
    my $ΔxCD = $cx - $dx;
    my $ΔyCD = $cy - $dy;

    my $x-numerator = determinate( |AB|, $ΔxAB, |CD|, $ΔxCD );
    my $y-numerator = determinate( |AB|, $ΔyAB, |CD|, $ΔyCD );
    my $denominator = determinate( $ΔxAB, $ΔyAB, $ΔxCD, $ΔyCD );

    return 'Lines are parallel' if $denominator == 0;

    ($x-numerator/$denominator, $y-numerator/$denominator);
}

sub determinate ( Real $a, Real $b, Real $c, Real $d ) { $a * $d - $b * $c }

# TESTING
say 'Intersection point: ', intersection( 4,0, 6,10, 0,3, 10,7 );
say 'Intersection point: ', intersection( 4,0, 6,10, 0,3, 10,7.1 );
say 'Intersection point: ', intersection( 0,0, 1,1, 1,2, 4,5 );
```

{{out}}

```txt
Intersection point: (5 5)
Intersection point: (5.010893 5.054466)
Intersection point: Lines are parallel

```



## Phix


```Phix
enum X, Y

function abc(sequence s,e)
-- yeilds {a,b,c}, corresponding to ax+by=c
    atom a = e[Y]-s[Y], b = s[X]-e[X], c = a*s[X]+b*s[Y]
    return {a,b,c}
end function

procedure intersect(sequence s1, e1, s2, e2)
    atom {a1,b1,c1} = abc(s1,e1),
         {a2,b2,c2} = abc(s2,e2),
         delta = a1*b2 - a2*b1,
         x = b2*c1 - b1*c2,
         y = a1*c2 - a2*c1
    ?iff(delta=0?"parallel lines/do not intersect"
                :{x/delta, y/delta})
end procedure

intersect({4,0},{6,10},{0,3},{10,7})        -- {5,5}
intersect({4,0},{6,10},{0,3},{10,7.1})      -- {5.010893246,5.054466231}
intersect({0,0},{0,0},{0,3},{10,7})         -- "parallel lines/do not intersect"
intersect({0,0},{1,1},{1,2},{4,5})          -- "parallel lines/do not intersect"
intersect({1,-1},{4,4},{2,5},{3,-2})        -- {2.5,1.5}
```



## Python


Using external [https://shapely.readthedocs.io/en/latest/manual.html Shapely] library

```python
from shapely.geometry import LineString

if __name__ == "__main__":
    line1 = LineString([(4, 0), (6, 10)])
    line2 = LineString([(0, 3), (10, 7)])
    print(line1.intersection(line2))
```

{{out}}

```txt
POINT (5 5)
```


Or, one approach to doing this by hand, without importing special libraries:
{{Works with|Python|3.7}}

```python
'''The intersection of two lines.'''

from functools import (reduce)


# intersection :: Line -> Line -> Either String Point
def intersection(ab):
    '''Either the point at which the lines ab and pq
       intersect, or a message string indicating that
       they are parallel and have no intersection.'''
    def delta(f):
        return lambda x: f(fst(x)) - f(snd(x))

    def prodDiff(abcd):
        [a, b, c, d] = abcd
        return (a * d) - (b * c)

    def go(pq):
        [abDX, pqDX, abDY, pqDY] = ap(
            [delta(fst), delta(snd)]
        )([ab, pq])
        determinant = prodDiff([abDX, abDY, pqDX, pqDY])

        def point():
            [abD, pqD] = map(
                lambda xy: prodDiff(
                    ap([fst, snd])([fst(xy), snd(xy)])
                ), [ab, pq]
            )
            return ap(
                [lambda abpq: prodDiff(
                    [abD, fst(abpq), pqD, snd(abpq)]) / determinant]
            )(
                [(abDX, pqDX), (abDY, pqDY)]
            )
        return Right(point()) if 0 != determinant else Left(
            '( Parallel lines - no intersection )'
        )

    return lambda pq: bindLR(go(pq))(
        lambda xs: Right((fst(xs), snd(xs)))
    )


# TEST ----------------------------------------------------
# main :: IO()
def main():
    '''Test'''

    # Left(message - no intersection) or Right(point)
    # lrPoint :: Either String Point
    lrPoint = intersection(
        ((4.0, 0.0), (6.0, 10.0))
    )(
        ((0.0, 3.0), (10.0, 7.0))
    )
    print(
        lrPoint['Left'] or lrPoint['Right']
    )


# GENERIC FUNCTIONS ---------------------------------------

# Left :: a -> Either a b
def Left(x):
    '''Constructor for an empty Either (option type) value
       with an associated string.'''
    return {'type': 'Either', 'Right': None, 'Left': x}


# Right :: b -> Either a b
def Right(x):
    '''Constructor for a populated Either (option type) value'''
    return {'type': 'Either', 'Left': None, 'Right': x}


# ap (<*>) :: [(a -> b)] -> [a] -> [b]
def ap(fs):
    '''The application of each of a list of functions,
       to each of a list of values.'''
    return lambda xs: reduce(
        lambda a, f: a + reduce(
            lambda a, x: a + [f(x)], xs, []
        ), fs, []
    )


# bindLR (>>=) :: Either a -> (a -> Either b) -> Either b
def bindLR(m):
    '''Either monad injection operator.
       Two computations sequentially composed,
       with any value produced by the first
       passed as an argument to the second.'''
    return lambda mf: (
        mf(m.get('Right')) if None is m.get('Left') else m
    )


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
(5.0, 5.0)
```



## Racket

{{trans|C++}}

```racket
#lang racket/base
(define (det a b c d) (- (* a d) (* b c))) ; determinant

(define (line-intersect ax ay bx by cx cy dx dy) ; --> (values x y)
  (let* ((det.ab (det ax ay bx by))
         (det.cd (det cx cy dx dy))
         (abΔx (- ax bx))
         (cdΔx (- cx dx))
         (abΔy (- ay by))
         (cdΔy (- cy dy))
         (xnom (det det.ab abΔx det.cd cdΔx))
         (ynom (det det.ab abΔy det.cd cdΔy))
         (denom (det abΔx abΔy cdΔx cdΔy)))
    (when (zero? denom)
      (error 'line-intersect "parallel lines"))
    (values (/ xnom denom) (/ ynom denom))))

(module+ test (line-intersect 4 0 6 10
                              0 3 10 7))
```


{{out}}

```txt
5
5
```



## REXX


### version 1

Naive implementation.
To be improved for parallel lines and degenerate lines such as y=5 or x=8.

```rexx
/* REXX */
Parse Value '(4.0,0.0)'  With '(' xa ',' ya ')'
Parse Value '(6.0,10.0)' With '(' xb ',' yb ')'
Parse Value '(0.0,3.0)'  With '(' xc ',' yc ')'
Parse Value '(10.0,7.0)' With '(' xd ',' yd ')'

Say 'The two lines are:'
Say 'yab='ya-xa*((yb-ya)/(xb-xa))'+x*'||((yb-ya)/(xb-xa))
Say 'ycd='yc-xc*((yd-yc)/(xd-xc))'+x*'||((yd-yc)/(xd-xc))

x=((yc-xc*((yd-yc)/(xd-xc)))-(ya-xa*((yb-ya)/(xb-xa))))/,
                         (((yb-ya)/(xb-xa))-((yd-yc)/(xd-xc)))
Say 'x='||x
        y=ya-xa*((yb-ya)/(xb-xa))+x*((yb-ya)/(xb-xa))
Say 'yab='y
Say 'ycd='yc-xc*((yd-yc)/(xd-xc))+x*((yd-yc)/(xd-xc))
Say 'Intersection: ('||x','y')'
```

{{out}}

```txt
The two lines are:
yab=-20.0+x*5
ycd=3.0+x*0.4
x=5
yab=5.0
ycd=5.0
Intersection: (5,5.0)
```



### version 2

complete implementation taking care of all possibilities.


Variables are named after the Austrian notation for a straight line: y=k*x+d

```rexx
say ggx1('4.0 0.0 6.0 10.0 0.0 3.0 10.0 7.0')
say ggx1('0.0 0.0 0.0 10.0 0.0 3.0 10.0 7.0')
say ggx1('0.0 0.0 0.0 10.0 0.0 3.0 10.0 7.0')
say ggx1('0.0 0.0 0.0  1.0 1.0 0.0  1.0 7.0')
say ggx1('0.0 0.0 0.0  0.0 0.0 3.0 10.0 7.0')
say ggx1('0.0 0.0 3.0  3.0 0.0 0.0  6.0 6.0')
say ggx1('0.0 0.0 3.0  3.0 0.0 1.0  6.0 7.0')
Exit

ggx1: Procedure
/*---------------------------------------------------------------------
* find the intersection of the lines AB and CD
*--------------------------------------------------------------------*/
Parse Arg xa  ya  xb  yb   xc  yc  xd   yd
Say 'A=('xa'/'ya') B=('||xb'/'yb') C=('||xc'/'yc') D=('||xd'/'yd')'
res=''
If xa=xb Then Do                    /* AB is a vertical line         */
  k1='*'                            /* slope is infinite             */
  x1=xa                             /* intersection's x is xa        */
  If ya=yb Then                     /* coordinates are equal         */
    res='Points A and B are identical' /* special case               */
  End
Else Do                             /* AB is not a vertical line     */
  k1=(yb-ya)/(xb-xa)                /* compute the slope of AB       */
  d1=ya-k1*xa                /* and its intersection with the y-axis */
  End
If xc=xd Then Do                    /* CD is a vertical line         */
  k2='*'                            /* slope is infinite             */
  x2=xc                             /* intersection's x is xc        */
  If yc=yd Then                     /* coordinates are equal         */
    res='Points C and D are identical' /* special case               */
  End
Else Do                             /* CD is not a vertical line     */
  k2=(yd-yc)/(xd-xc)                /* compute the slope of CD       */
  d2=yc-k2*xc                /* and its intersection with the y-axis */
  End

If res='' Then Do                   /* no special case so far        */
  If k1='*' Then Do                 /* AB is vertical                */
    If k2='*' Then Do               /* CD is vertical                */
      If x1=x2 Then                 /* and they are identical        */
        res='Lines AB and CD are identical'
      Else                          /* not identical                 */
        res='Lines AB and CD are parallel'
      End
    Else Do
      x=x1                          /* x is taken from AB            */
      y=k2*x+d2                     /* y is computed from CD         */
      End
    End
  Else Do                           /* AB is not verical             */
    If k2='*' Then Do               /* CD is vertical                */
      x=x2                          /* x is taken from CD            */
      y=k1*x+d1                     /* y is computed from AB         */
      End
    Else Do                         /* AB and CD are not vertical    */
      If k1=k2 Then Do              /* identical slope               */
        If d1=d2 Then               /* same intersection with x=0    */
          res='Lines AB and CD are identical'
        Else                        /* otherwise                     */
          res='Lines AB and CD are parallel'
        End
      Else Do                       /* finally the normal case       */
        x=(d2-d1)/(k1-k2)           /* compute x                     */
        y=k1*x+d1                   /* and y                         */
        End
      End
    End
  End
  If res='' Then                    /* not any special case          */
    res='Intersection is ('||x'/'y')'  /* that's the result          */
  Return '  -->' res
```

{{out}}

```txt
A=(4.0/0.0) B=(6.0/10.0) C=(0.0/3.0) D=(10.0/7.0)
  --> Intersection is (5/5.0)
A=(0.0/0.0) B=(0.0/10.0) C=(0.0/3.0) D=(10.0/7.0)
  --> Intersection is (0.0/3.0)
A=(0.0/0.0) B=(0.0/10.0) C=(0.0/3.0) D=(10.0/7.0)
  --> Intersection is (0.0/3.0)
A=(0.0/0.0) B=(0.0/1.0) C=(1.0/0.0) D=(1.0/7.0)
  --> Lines AB and CD are parallel
A=(0.0/0.0) B=(0.0/0.0) C=(0.0/3.0) D=(10.0/7.0)
  --> Points A and B are identical
A=(0.0/0.0) B=(3.0/3.0) C=(0.0/0.0) D=(6.0/6.0)
  --> Lines AB and CD are identical
A=(0.0/0.0) B=(3.0/3.0) C=(0.0/1.0) D=(6.0/7.0)
  --> Lines AB and CD are parallel
```



## Ring


```ring

# Project : Find the intersection of two lines

xa=4
ya=0
xb=6
yb=10
xc=0
yc=3
xd=10
yd=7
see "the two lines are:" + nl
see "yab=" + (ya-xa*((yb-ya)/(xb-xa))) + "+x*" + ((yb-ya)/(xb-xa)) + nl
see "ycd=" + (yc-xc*((yd-yc)/(xd-xc))) + "+x*" + ((yd-yc)/(xd-xc)) + nl
x=((yc-xc*((yd-yc)/(xd-xc)))-(ya-xa*((yb-ya)/(xb-xa))))/(((yb-ya)/(xb-xa))-((yd-yc)/(xd-xc)))
see "x=" + x + nl
y=ya-xa*((yb-ya)/(xb-xa))+x*((yb-ya)/(xb-xa))
see "yab=" + y + nl
see "ycd=" + (yc-xc*((yd-yc)/(xd-xc))+x*((yd-yc)/(xd-xc))) + nl
see "intersection: " + x + "," + y + nl

```

Output:

```txt

the two lines are:
yab=-20+x*5
ycd=3+x*0.4
x=5
yab=5
ycd=5
intersection: 5,5

```



## Ruby


```ruby
Point = Struct.new(:x, :y)

class Line
  attr_reader :a, :b

  def initialize(point1, point2)
    @a = (point1.y - point2.y).fdiv(point1.x - point2.x)
    @b = point1.y - @a*point1.x
  end

  def intersect(other)
    return nil if @a == other.a
    x = (other.b - @b).fdiv(@a - other.a)
    y = @a*x + @b
    Point.new(x,y)
  end

  def to_s
    "y = #{@a}x + #{@b}"
  end

end

l1 = Line.new(Point.new(4, 0), Point.new(6, 10))
l2 = Line.new(Point.new(0, 3), Point.new(10, 7))

puts "Line #{l1} intersects line #{l2} at #{l1.intersect(l2)}."

```

{{out}}

```txt
Line y = 5.0x + -20.0 intersects line y = 0.4x + 3.0 at #<struct Point x=5.0, y=5.0>.
```



## Rust


```Rust
#[derive(Copy, Clone, Debug)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    pub fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }
}

#[derive(Copy, Clone, Debug)]
struct Line(Point, Point);

impl Line {
    pub fn intersect(self, other: Self) -> Option<Point> {
        let a1 = self.1.y - self.0.y;
        let b1 = self.0.x - self.1.x;
        let c1 = a1 * self.0.x + b1 * self.0.y;

        let a2 = other.1.y - other.0.y;
        let b2 = other.0.x - other.1.x;
        let c2 = a2 * other.0.x + b2 * other.0.y;

        let delta = a1 * b2 - a2 * b1;

        if delta == 0.0 {
            return None;
        }

        Some(Point {
            x: (b2 * c1 - b1 * c2) / delta,
            y: (a1 * c2 - a2 * c1) / delta,
        })
    }
}

fn main() {
    let l1 = Line(Point::new(4.0, 0.0), Point::new(6.0, 10.0));
    let l2 = Line(Point::new(0.0, 3.0), Point::new(10.0, 7.0));
    println!("{:?}", l1.intersect(l2));

    let l1 = Line(Point::new(0.0, 0.0), Point::new(1.0, 1.0));
    let l2 = Line(Point::new(1.0, 2.0), Point::new(4.0, 5.0));
    println!("{:?}", l1.intersect(l2));
}
```

{{Out}}

```txt

Some(Point { x: 5.0, y: 5.0 })
None

```



## Scala


```Scala
object Intersection extends App {
  val (l1, l2) = (LineF(PointF(4, 0), PointF(6, 10)), LineF(PointF(0, 3), PointF(10, 7)))

  def findIntersection(l1: LineF, l2: LineF): PointF = {
    val a1 = l1.e.y - l1.s.y
    val b1 = l1.s.x - l1.e.x
    val c1 = a1 * l1.s.x + b1 * l1.s.y

    val a2 = l2.e.y - l2.s.y
    val b2 = l2.s.x - l2.e.x
    val c2 = a2 * l2.s.x + b2 * l2.s.y

    val delta = a1 * b2 - a2 * b1
    // If lines are parallel, intersection point will contain infinite values
    PointF((b2 * c1 - b1 * c2) / delta, (a1 * c2 - a2 * c1) / delta)
  }

  def l01 = LineF(PointF(0f, 0f), PointF(1f, 1f))
  def l02 = LineF(PointF(1f, 2f), PointF(4f, 5f))

  case class PointF(x: Float, y: Float) {
    override def toString = s"{$x, $y}"
  }

  case class LineF(s: PointF, e: PointF)

  println(findIntersection(l1, l2))
  println(findIntersection(l01, l02))

}
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/DAqMtEx/0 (JavaScript)]
or by [https://scastie.scala-lang.org/WQOqakOlQnaBRFBa1PuRYw Scastie (JVM)].

## Sidef

{{trans|Perl 6}}


```ruby
func det(a, b, c, d) { a*d - b*c }

func intersection(ax, ay, bx, by,
                  cx, cy, dx, dy) {

    var detAB = det(ax,ay, bx,by)
    var detCD = det(cx,cy, dx,dy)

    var ΔxAB = (ax - bx)
    var ΔyAB = (ay - by)
    var ΔxCD = (cx - dx)
    var ΔyCD = (cy - dy)

    var x_numerator = det(detAB, ΔxAB, detCD, ΔxCD)
    var y_numerator = det(detAB, ΔyAB, detCD, ΔyCD)
    var denominator = det( ΔxAB, ΔyAB,  ΔxCD, ΔyCD)

    denominator == 0 && return 'lines are parallel'
    [x_numerator / denominator, y_numerator / denominator]
}

say ('Intersection point: ', intersection(4,0, 6,10, 0,3, 10,7))
say ('Intersection point: ', intersection(4,0, 6,10, 0,3, 10,7.1))
say ('Intersection point: ', intersection(0,0, 1,1, 1,2, 4,5))
```

{{out}}

```txt

Intersection point: [5, 5]
Intersection point: [2300/459, 2320/459]
Intersection point: lines are parallel

```



## Swift


```swift
struct Point {
  var x: Double
  var y: Double
}

struct Line {
  var p1: Point
  var p2: Point

  var slope: Double {
    guard p1.x - p2.x != 0.0 else { return .nan }

    return (p1.y-p2.y) / (p1.x-p2.x)
  }

  func intersection(of other: Line) -> Point? {
    let ourSlope = slope
    let theirSlope = other.slope

    guard ourSlope != theirSlope else { return nil }

    if ourSlope.isNaN && !theirSlope.isNaN {
      return Point(x: p1.x, y: (p1.x - other.p1.x) * theirSlope + other.p1.y)
    } else if theirSlope.isNaN && !ourSlope.isNaN {
      return Point(x: other.p1.x, y: (other.p1.x - p1.x) * ourSlope + p1.y)
    } else {
      let x = (ourSlope*p1.x - theirSlope*other.p1.x + other.p1.y - p1.y) / (ourSlope - theirSlope)
      return Point(x: x, y: theirSlope*(x - other.p1.x) + other.p1.y)
    }
  }
}

let l1 = Line(p1: Point(x: 4.0, y: 0.0), p2: Point(x: 6.0, y: 10.0))
let l2 = Line(p1: Point(x: 0.0, y: 3.0), p2: Point(x: 10.0, y: 7.0))

print("Intersection at : \(l1.intersection(of: l2)!)")
```

{{out}}

```txt
Intersection at : Point(x: 5.0, y: 5.0)
```


=={{header|TI-83 BASIC}}==
{{works with|TI-83 BASIC|TI-84Plus 2.55MP}}
{{trans|Rexx}}
Simple version:

```ti83b
[[4,0][6,10][0,3][10,7]]→[A]
([A](2,2)-[A](1,2))/([A](2,1)-[A](1,1))→B
[A](1,2)-[A](1,1)*B→A
([A](4,2)-[A](3,2))/([A](4,1)-[A](3,1))→D
[A](3,2)-[A](3,1)*D→C
(C-A)/(B-D)→X
A+X*B→Y
C+X*D→Z
Disp {X,Y}
```

{{out}}

```txt

       {5 5}

```

Full version:

```ti83b
[[4,0][6,10][0,3][10,7]]→[A]
{4,2}→Dim([B])
0→M
If [A](1,1)=[A](2,1)
Then
  [A](1,1)→[B](3,1)
  If [A](1,2)=[A](2,2):1→M
Else
  1→[B](4,1)
  ([A](2,2)-[A](1,2))/([A](2,1)-[A](1,1))→[B](1,1)
  [A](1,2)-[B](1,1)*[A](1,1)→[B](2,1)
End
If [A](3,1)=[A](4,1)
Then
  [A](3,1)→[B](3,2)
  If [A](3,2)=[A](4,2):2→M
Else
  1→[B](4,2)
  ([A](4,2)-[A](3,2))/([A](4,1)-[A](3,1))→[B](1,2)
  [A](3,2)-[B](1,2)*[A](3,1)→[B](2,2)
End
If M=0 Then
  If [B](4,1)=0
  Then
    If [B](4,2)=0
    Then
      If [B](3,1)=[B](3,2)
      Then:3→M
      Else:4→M
      End
    Else
      [B](3,1)→X
      [B](1,2)*X+[B](2,2)→Y
    End
  Else
    If [B](4,2)=0
    Then
      [B](3,2)→X
      [B](1,1)*X+[B](2,1)→Y
    Else
      If [B](1,1)=[B](1,2)
      Then
        If [B](2,1)=[B](2,2)
        Then:5→M
        Else:6→M
        End
      Else
        ([B](2,2)-[B](2,1))/([B](1,1)-[B](1,2))→X
        [B](1,1)*X+[B](2,1)→Y
      End
    End
  End
End
Disp {X,Y,M}
```

{{out}}

```txt

       {5 5}

```



## Visual Basic

{{works with|Visual Basic|5}}
{{works with|Visual Basic|6}}
{{works with|VBA|6.5}}
{{works with|VBA|7.1}}

```vb
Option Explicit

Public Type Point
  x As Double
  y As Double
  invalid As Boolean
End Type

Public Type Line
  s As Point
  e As Point
End Type

Public Function GetIntersectionPoint(L1 As Line, L2 As Line) As Point
Dim a1 As Double
Dim b1 As Double
Dim c1 As Double
Dim a2 As Double
Dim b2 As Double
Dim c2 As Double
Dim det As Double

  a1 = L1.e.y - L1.s.y
  b1 = L1.s.x - L1.e.x
  c1 = a1 * L1.s.x + b1 * L1.s.y
  a2 = L2.e.y - L2.s.y
  b2 = L2.s.x - L2.e.x
  c2 = a2 * L2.s.x + b2 * L2.s.y
  det = a1 * b2 - a2 * b1

  If det Then
    With GetIntersectionPoint
      .x = (b2 * c1 - b1 * c2) / det
      .y = (a1 * c2 - a2 * c1) / det
    End With
  Else
    GetIntersectionPoint.invalid = True
  End If
End Function

Sub Main()
Dim ln1 As Line
Dim ln2 As Line
Dim ip As Point

  ln1.s.x = 4
  ln1.s.y = 0
  ln1.e.x = 6
  ln1.e.y = 10
  ln2.s.x = 0
  ln2.s.y = 3
  ln2.e.x = 10
  ln2.e.y = 7
  ip = GetIntersectionPoint(ln1, ln2)
  Debug.Assert Not ip.invalid
  Debug.Assert ip.x = 5 And ip.y = 5

  LSet ln2.s = ln2.e
  ip = GetIntersectionPoint(ln1, ln2)
  Debug.Assert ip.invalid

  LSet ln2 = ln1
  ip = GetIntersectionPoint(ln1, ln2)
  Debug.Assert ip.invalid

End Sub
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Drawing

Module Module1

    Function FindIntersection(s1 As PointF, e1 As PointF, s2 As PointF, e2 As PointF) As PointF
        Dim a1 = e1.Y - s1.Y
        Dim b1 = s1.X - e1.X
        Dim c1 = a1 * s1.X + b1 * s1.Y

        Dim a2 = e2.Y - s2.Y
        Dim b2 = s2.X - e2.X
        Dim c2 = a2 * s2.X + b2 * s2.Y

        Dim delta = a1 * b2 - a2 * b1

        'If lines are parallel, the result will be (NaN, NaN).
        Return If(delta = 0, New PointF(Single.NaN, Single.NaN), New PointF((b2 * c1 - b1 * c2) / delta, (a1 * c2 - a2 * c1) / delta))
    End Function

    Sub Main()
        Dim p = Function(x As Single, y As Single) New PointF(x, y)
        Console.WriteLine(FindIntersection(p(4.0F, 0F), p(6.0F, 10.0F), p(0F, 3.0F), p(10.0F, 7.0F)))
        Console.WriteLine(FindIntersection(p(0F, 0F), p(1.0F, 1.0F), p(1.0F, 2.0F), p(4.0F, 5.0F)))
    End Sub

End Module
```

{{out}}

```txt
{X=5, Y=5}
{X=NaN, Y=NaN}
```



## zkl

{{trans|C++}}

```zkl
fcn lineIntersect(ax,ay, bx,by,   cx,cy, dx,dy){	// --> (x,y)
   detAB,detCD := det(ax,ay, bx,by), det(cx,cy, dx,dy);
   abDx,cdDx := ax - bx, cx - dx;	// delta x
   abDy,cdDy := ay - by, cy - dy;	// delta y

   xnom,ynom := det(detAB,abDx, detCD,cdDx), det(detAB,abDy, detCD,cdDy);
   denom     := det(abDx,abDy, cdDx,cdDy);
   if(denom.closeTo(0.0, 0.0001))
      throw(Exception.MathError("lineIntersect: Parallel lines"));

   return(xnom/denom, ynom/denom);
}
fcn det(a,b,c,d){ a*d - b*c }	// determinant
```


```zkl
lineIntersect(4.0,0.0, 6.0,10.0,  0.0,3.0, 10.0,7.0).println();
```

{{out}}

```txt

L(5,5)

```


'''References'''

<references/>
