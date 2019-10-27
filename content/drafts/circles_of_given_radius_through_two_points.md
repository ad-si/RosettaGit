+++
title = "Circles of given radius through two points"
description = ""
date = 2019-09-27T22:31:57Z
aliases = []
[extra]
id = 13311
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[File:2 circles through 2 points.jpg|500px||right|2 circles with a given radius through 2 points in 2D space.]]

Given two points on a plane and a radius, usually two circles of given radius can be drawn through the points. 
;Exceptions:
# r==0.0 should be treated as never describing circles (except in the case where the points are coincident).
# If the points are coincident then an infinite number of circles with the point on their circumference can be drawn, unless r==0.0 as well which then collapses the circles to a point.
# If the points form a diameter then return two identical circles ''or'' return a single circle, according to which is the most natural mechanism for the implementation language.
# If the points are too far apart then no circles can be drawn.


;Task detail:
* Write a function/subroutine/method/... that takes two points and a radius and returns the two circles through those points, ''or some indication of special cases where two, possibly equal, circles cannot be returned''.
* Show here the output for the following inputs:

```txt

      p1                p2           r
0.1234, 0.9876    0.8765, 0.2345    2.0
0.0000, 2.0000    0.0000, 0.0000    1.0
0.1234, 0.9876    0.1234, 0.9876    2.0
0.1234, 0.9876    0.8765, 0.2345    0.5
0.1234, 0.9876    0.1234, 0.9876    0.0

```



;Related task:
*   [[Total circles area]].


;See also:
*   [http://mathforum.org/library/drmath/view/53027.html Finding the Center of a Circle from 2 Points and Radius] from Math forum @ Drexel





## ALGOL 68

Calculations based on the C solution.

```algol68
# represents a point                                                 #
MODE POINT = STRUCT( REAL x, REAL y );
# returns TRUE if p1 is the same point as p2, FALSE otherwise        #
OP = = ( POINT p1, POINT p2 )BOOL: x OF p1 = x OF p2 AND y OF p1 = y OF p2;

# represents a circle with centre c and radius r                     #
MODE CIRCLE = STRUCT( POINT c, REAL r );
# returns the difference in x-coordinate of two points               #
PRIO XDIFF = 5;
OP   XDIFF = ( POINT p1, POINT p2 )REAL: x OF p1 - x OF p2;
# returns the difference in y-coordinate of two points               #
PRIO YDIFF = 5;
OP   YDIFF = ( POINT p1, POINT p2 )REAL: y OF p1 - y OF p2;
# returns the distance between two points                            #
OP   -     = ( POINT p1, POINT p2 )REAL: 
     BEGIN
        REAL x diff   = p1 XDIFF p2;
        REAL y diff   = p1 YDIFF p2;
        sqrt( ( x diff * xdiff ) + ( y diff * y diff ) )
     END; # - #
# generate a human-readable version of the circle c                  #
OP TOSTRING = ( CIRCLE c )STRING:
       ( "radius:"
       + fixed(      r OF c, -8, 4 )
       + " @("
       + fixed( x OF c OF c, -8, 4 )
       + ", "
       + fixed( y OF c OF c, -8, 4 )
       + ")"
       );

# modes to represent the results of the circles procedure ...        #
# infinite number of circles                                         #
MODE INFINITECIRCLES = STRUCT( STRING t, REAL r );
# two possible circles                                               #
MODE TWOCIRCLES      = STRUCT( CIRCLE a, CIRCLE b );
# one possible circle results in a CIRCLE                            #
# no possible circles                                                #
MODE NOCIRCLES       = STRUCT( STRING reason, POINT p1, POINT p2, REAL r );
# mode returned by the circles procedure                             #
MODE POSSIBLECIRCLES = UNION( INFINITECIRCLES, TWOCIRCLES, CIRCLE, NOCIRCLES );

# returns the circles of radius r that can be drawn through          #
#         points p1 and p2                                           #
PROC circles = ( POINT p1, POINT p2, REAL r )POSSIBLECIRCLES:
     IF r < 0 THEN # negative radius - there are no circles          #
         NOCIRCLES( "negative radius", p1, p2, r )
     ELIF p1 = p2 THEN # coincident points                           #
         IF r = 0.0 THEN
             # only one circle of radius 0 is possible               #
             CIRCLE( p1, 0.0 )
         ELSE
             # an infinite number of circles can be drawn through    #
             # the point                                             #
             INFINITECIRCLES( "infinite", r )
         FI
     ELSE # two possible circles                                     #
         REAL distance = p1 - p2;
         IF   distance > 2 * r THEN
             # the points are too far apart                          #
             NOCIRCLES( "points too far apart", p1, p2, r )
         ELIF distance = 2 * r THEN
             # the points are on the diameter of the circle          #
             CIRCLE( POINT( x OF p1 + ( ( p2 XDIFF p1 ) / 2 )
                          , y OF p1 + ( ( p2 YDIFF p1 ) / 2 )
                          )
                   , r
                   )
         ELSE
             # it is possible to draw two circles through the points #
             REAL half x sum      = ( x OF p1 + x OF p2 ) / 2;
             REAL half y sum      = ( y OF p1 + y OF p2 ) / 2;
             REAL mirror distance = sqrt( ( r * r ) - ( ( distance * distance ) / 4 ) );
             REAL x mirror        = ( mirror distance * ( y OF p1 - y OF p2 ) ) / distance;
             REAL y mirror        = ( mirror distance * ( x OF p2 - x OF p1 ) ) / distance;
             TWOCIRCLES( CIRCLE( POINT( half x sum + y mirror, half y sum + x mirror ), r )
                       , CIRCLE( POINT( half x sum - y mirror, half y sum - x mirror ), r )
                       )
         FI
     FI; # circles #

# test the circles procedure with the examples from the task    #

PROC print circles = ( REAL x1, y1, x2, y2, r )VOID:
     BEGIN
        CASE circles( POINT( x1, y1 ), POINT( x2, y2 ), r )
          IN ( NOCIRCLES       n ): print( ( "No circles : ", reason OF n ) )
           , ( TWOCIRCLES      t ): print( ( "Two circles: "
                                           , TOSTRING a OF t
                                           , ", "
                                           , TOSTRING b OF t
                                           )
                                         )
           , ( CIRCLE          c ): print( ( "One circle : ", TOSTRING c ) )
           , ( INFINITECIRCLES i ): print( ( "Infinite circles" ) )
         OUT BEGIN
                 print( ( "Unexpected circles result", newline ) );
                 stop
             END
        ESAC;
        print( ( newline ) )
     END; # print circles #
print circles( 0.1234, 0.9876,    0.8765, 0.2345,    2.0 );
print circles( 0.0000, 2.0000,    0.0000, 0.0000,    1.0 );
print circles( 0.1234, 0.9876,    0.1234, 0.9876,    2.0 );
print circles( 0.1234, 0.9876,    0.8765, 0.2345,    0.5 );
print circles( 0.1234, 0.9876,    0.1234, 0.9876,    0.0 )
```

{{out}}

```txt

Two circles: radius:  2.0000 @(  1.8631,   1.9742), radius:  2.0000 @( -0.8632,  -0.7521)
One circle : radius:  1.0000 @(  0.0000,   1.0000)
Infinite circles
No circles : points too far apart
One circle : radius:  0.0000 @(  0.1234,   0.9876)

```



## AutoHotkey


```AutoHotkey
CircleCenter(x1, y1, x2, y2, r){
	d := sqrt((x2-x1)**2 + (y2-y1)**2)
	x3 := (x1+x2)/2	, y3 := (y1+y2)/2
	cx1 := x3 + sqrt(r**2-(d/2)**2)*(y1-y2)/d , 	cy1:= y3 + sqrt(r**2-(d/2)**2)*(x2-x1)/d
	cx2 := x3 - sqrt(r**2-(d/2)**2)*(y1-y2)/d , 	cy2:= y3 - sqrt(r**2-(d/2)**2)*(x2-x1)/d
	if (d = 0)
		return "No circles can be drawn, points are identical"
	if (d = r*2)
		return "points are opposite ends of a diameter center = " cx1 "," cy1
	if (d = r*2)
		return "points are too far"
	if (r <= 0)
		return "radius is not valid"
	if !(cx1 && cy1 && cx2 && cy2)
		return "no solution"
	return cx1 "," cy1 " & " cx2 "," cy2
}
```

Examples:
```AutoHotkey
data = 
(
0.1234 0.9876 0.8765 0.2345 2.0
0.0000 2.0000 0.0000 0.0000 1.0
0.1234 0.9876 0.1234 0.9876 2.0
0.1234 0.9876 0.8765 0.2345 0.5
0.1234 0.9876 0.1234 0.9876 0.0
)

loop, parse, data, `n
{
	obj := StrSplit(A_LoopField, " ")
	MsgBox, % CircleCenter(obj[1], obj[2], obj[3], obj[4], obj[5])
}
```

{{out}}

```txt
0.1234 0.9876 0.8765 0.2345 2.0 > 1.863112,1.974212 & -0.863212,-0.752112
0.0000 2.0000 0.0000 0.0000 1.0 > points are opposite ends of a diameter center = 0.000000,1.000000
0.1234 0.9876 0.1234 0.9876 2.0 > No circles can be drawn, points are identical
0.1234 0.9876 0.8765 0.2345 0.5 > no solution
0.1234 0.9876 0.1234 0.9876 0.0 > No circles can be drawn, points are identical

```


## AWK


```AWK

# syntax: GAWK -f CIRCLES_OF_GIVEN_RADIUS_THROUGH_TWO_POINTS.AWK
# converted from PL/I
BEGIN {
    split("0.1234,0,0.1234,0.1234,0.1234",m1x,",")
    split("0.9876,2,0.9876,0.9876,0.9876",m1y,",")
    split("0.8765,0,0.1234,0.8765,0.1234",m2x,",")
    split("0.2345,0,0.9876,0.2345,0.9876",m2y,",")
    leng = split("2,1,2,0.5,0",r,",")
    print("     x1      y1      x2      y2    r   cir1x   cir1y   cir2x   cir2y")
    print("------- ------- ------- ------- ---- ------- ------- ------- -------")
    for (i=1; i<=leng; i++) {
      printf("%7.4f %7.4f %7.4f %7.4f %4.2f %s\n",m1x[i],m1y[i],m2x[i],m2y[i],r[i],main(m1x[i],m1y[i],m2x[i],m2y[i],r[i]))
    }
    exit(0)
}
function main(m1x,m1y,m2x,m2y,r,  bx,by,pb,x,x1,y,y1) {
    if (r == 0) { return("radius of zero gives no circles") }
    x = (m2x - m1x) / 2
    y = (m2y - m1y) / 2
    bx = m1x + x
    by = m1y + y
    pb = sqrt(x^2 + y^2)
    if (pb == 0) { return("coincident points give infinite circles") }
    if (pb > r) { return("points are too far apart for the given radius") }
    cb = sqrt(r^2 - pb^2)
    x1 = y * cb / pb
    y1 = x * cb / pb
    return(sprintf("%7.4f %7.4f %7.4f %7.4f",bx-x1,by+y1,bx+x1,by-y1))
}

```

{{out}}

```txt

     x1      y1      x2      y2    r   cir1x   cir1y   cir2x   cir2y
------- ------- ------- ------- ---- ------- ------- ------- -------
 0.1234  0.9876  0.8765  0.2345 2.00  1.8631  1.9742 -0.8632 -0.7521
 0.0000  2.0000  0.0000  0.0000 1.00  0.0000  1.0000  0.0000  1.0000
 0.1234  0.9876  0.1234  0.9876 2.00 coincident points give infinite circles
 0.1234  0.9876  0.8765  0.2345 0.50 points are too far apart for the given radius
 0.1234  0.9876  0.1234  0.9876 0.00 radius of zero gives no circles

```



## BASIC

{{works with|FreeBASIC}}

```freebasic
Type Point
    As Double x,y
    Declare Property length As Double
End Type

Property point.length As Double
Return Sqr(x*x+y*y)
End Property

Sub circles(p1 As Point,p2 As Point,radius As Double)
    Print "Points ";"("&p1.x;","&p1.y;"),("&p2.x;","&p2.y;")";", Rad ";radius
    Var ctr=Type<Point>((p1.x+p2.x)/2,(p1.y+p2.y)/2)
    Var half=Type<Point>(p1.x-ctr.x,p1.y-ctr.y)
    Var lenhalf=half.length
    If radius<lenhalf Then Print "Can't solve":Print:Exit Sub
    If lenhalf=0 Then Print "Points are the same":Print:Exit Sub
    Var dist=Sqr(radius^2-lenhalf^2)/lenhalf
    Var rot= Type<Point>(-dist*(p1.y-ctr.y) +ctr.x,dist*(p1.x-ctr.x) +ctr.y)
    Print " -> Circle 1 ("&rot.x;","&rot.y;")"
    rot= Type<Point>(-(rot.x-ctr.x) +ctr.x,-((rot.y-ctr.y)) +ctr.y)
    Print" -> Circle 2 ("&rot.x;","&rot.y;")"
    Print
End Sub


Dim As Point p1=(.1234,.9876),p2=(.8765,.2345)
circles(p1,p2,2)
p1=Type<Point>(0,2):p2=Type<Point>(0,0)
circles(p1,p2,1)
p1=Type<Point>(.1234,.9876):p2=p1
circles(p1,p2,2)
p1=Type<Point>(.1234,.9876):p2=Type<Point>(.8765,.2345)
circles(p1,p2,.5)
p1=Type<Point>(.1234,.9876):p2=p1
circles(p1,p2,0)

Sleep
```

{{out}}

```txt
Points (0.1234,0.9876),(0.8765,0.2345), Rad  2
 -> Circle 1 (-0.8632118016581893,-0.7521118016581889)
 -> Circle 2 (1.863111801658189,1.974211801658189)

Points (0,2),(0,0), Rad  1
 -> Circle 1 (0,1)
 -> Circle 2 (0,1)

Points (0.1234,0.9876),(0.1234,0.9876), Rad  2
Points are the same

Points (0.1234,0.9876),(0.8765,0.2345), Rad  0.5
Can't solve

Points (0.1234,0.9876),(0.1234,0.9876), Rad  0
Points are the same
```



## C


```C>#include<stdio.h

#include<math.h>

typedef struct{
	double x,y;
	}point;
	
double distance(point p1,point p2)
{
	return sqrt((p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y));
}
	
void findCircles(point p1,point p2,double radius)
{
	double separation = distance(p1,p2),mirrorDistance;
	
	if(separation == 0.0)
	{
		radius == 0.0 ? printf("\nNo circles can be drawn through (%.4f,%.4f)",p1.x,p1.y):
							 printf("\nInfinitely many circles can be drawn through (%.4f,%.4f)",p1.x,p1.y);
	}
	
	else if(separation == 2*radius)
	{
		printf("\nGiven points are opposite ends of a diameter of the circle with center (%.4f,%.4f) and radius %.4f",(p1.x+p2.x)/2,(p1.y+p2.y)/2,radius); 
	}
	
	else if(separation > 2*radius)
	{
		printf("\nGiven points are farther away from each other than a diameter of a circle with radius %.4f",radius);
	}   
	
	else
	{
		mirrorDistance =sqrt(pow(radius,2) - pow(separation/2,2));
		
		printf("\nTwo circles are possible.");
		printf("\nCircle C1 with center (%.4f,%.4f), radius %.4f and Circle C2 with center (%.4f,%.4f), radius %.4f",(p1.x+p2.x)/2 + mirrorDistance*(p1.y-p2.y)/separation,(p1.y+p2.y)/2 + mirrorDistance*(p2.x-p1.x)/separation,radius,(p1.x+p2.x)/2 - mirrorDistance*(p1.y-p2.y)/separation,(p1.y+p2.y)/2 - mirrorDistance*(p2.x-p1.x)/separation,radius);
	}
}

int main()
{
    int i;

    point cases[] = 	
    {	{0.1234, 0.9876},    {0.8765, 0.2345},  
	{0.0000, 2.0000},    {0.0000, 0.0000},   
	{0.1234, 0.9876},    {0.1234, 0.9876},   
	{0.1234, 0.9876},    {0.8765, 0.2345},    
	{0.1234, 0.9876},    {0.1234, 0.9876}
    };

    double radii[] = {2.0,1.0,2.0,0.5,0.0};

    for(i=0;i<5;i++)
    {	
	printf("\nCase %d)",i+1);
	findCircles(cases[2*i],cases[2*i+1],radii[i]);
    }

    return 0;
}

```

{{out|test run}}

```txt

Case 1)
Two circles are possible.
Circle C1 with center (1.8631,1.9742), radius 2.0000 and Circle C2 with center (-0.8632,-0.7521), radius 2.0000
Case 2)
Given points are opposite ends of a diameter of the circle with center (0.0000,1.0000) and radius 1.0000
Case 3)
Infinitely many circles can be drawn through (0.1234,0.9876)
Case 4)
Given points are farther away from each other than a diameter of a circle with radius 0.5000
Case 5)
No circles can be drawn through (0.1234,0.9876)

```



## C sharp

{{works with|C sharp|6}}

```csharp
using System;
public class CirclesOfGivenRadiusThroughTwoPoints
{
    public static void Main()
    {
        double[][] values = new double[][] {
            new [] { 0.1234, 0.9876, 0.8765, 0.2345,   2 },
            new [] { 0.0,       2.0,    0.0,    0.0,   1 },
            new [] { 0.1234, 0.9876, 0.1234, 0.9876,   2 },
            new [] { 0.1234, 0.9876, 0.8765, 0.2345, 0.5 },
            new [] { 0.1234, 0.9876, 0.1234, 0.9876,   0 }
        };
		
        foreach (var a in values) {
            var p = new Point(a[0], a[1]);
            var q = new Point(a[2], a[3]);
            Console.WriteLine($"Points {p} and {q} with radius {a[4]}:");
            try {
                var centers = FindCircles(p, q, a[4]);
                Console.WriteLine("\t" + string.Join(" and ", centers));
            } catch (Exception ex) {
                Console.WriteLine("\t" + ex.Message);
            }
        }
    }
	
    static Point[] FindCircles(Point p, Point q, double radius) {
        if(radius < 0) throw new ArgumentException("Negative radius.");
        if(radius == 0) {
            if(p == q) return new [] { p };
            else throw new InvalidOperationException("No circles.");
        }
        if (p == q) throw new InvalidOperationException("Infinite number of circles.");
		
        double sqDistance = Point.SquaredDistance(p, q);
        double sqDiameter = 4 * radius * radius;
        if (sqDistance > sqDiameter) throw new InvalidOperationException("Points are too far apart.");
		
        Point midPoint = new Point((p.X + q.X) / 2, (p.Y + q.Y) / 2);
        if (sqDistance == sqDiameter) return new [] { midPoint };
		
        double d = Math.Sqrt(radius * radius - sqDistance / 4);
        double distance = Math.Sqrt(sqDistance);
        double ox = d * (q.X - p.X) / distance, oy = d * (q.Y - p.Y) / distance;
        return new [] {
            new Point(midPoint.X - oy, midPoint.Y + ox),
            new Point(midPoint.X + oy, midPoint.Y - ox)
        };
    }
	
    public struct Point
    {
        public Point(double x, double y) : this() {
            X = x;
            Y = y;
        }
	
        public double X { get; }
        public double Y { get; }
	
        public static bool operator ==(Point p, Point q) => p.X == q.X && p.Y == q.Y;
        public static bool operator !=(Point p, Point q) => p.X != q.X || p.Y != q.Y;
	
        public static double SquaredDistance(Point p, Point q) {
            double dx = q.X - p.X, dy = q.Y - p.Y;
            return dx * dx + dy * dy;
        }
		
        public override string ToString() => $"({X}, {Y})";
		
    }	
}
```

{{out}}

```txt

Points (0.1234, 0.9876) and (0.8765, 0.2345) with radius 2:
    (1.86311180165819, 1.97421180165819) and (-0.86321180165819, -0.752111801658189)
Points (0, 2) and (0, 0) with radius 1:
    (0, 1)
Points (0.1234, 0.9876) and (0.1234, 0.9876) with radius 2:
    Infinite number of circles.
Points (0.1234, 0.9876) and (0.8765, 0.2345) with radius 0.5:
    Points are too far apart.
Points (0.1234, 0.9876) and (0.1234, 0.9876) with radius 0:
    (0.1234, 0.9876)
```



## C++

{{works with|C++11}}

```cpp

#include <iostream>
#include <cmath>
#include <tuple>

struct point { double x, y; };

bool operator==(const point& lhs, const point& rhs)
{ return std::tie(lhs.x, lhs.y) == std::tie(rhs.x, rhs.y); }

enum result_category { NONE, ONE_COINCEDENT, ONE_DIAMETER, TWO, INFINITE };

using result_t = std::tuple<result_category, point, point>;

double distance(point l, point r)
{ return std::hypot(l.x - r.x, l.y - r.y); }

result_t find_circles(point p1, point p2, double r)
{
    point ans1 { 1/0., 1/0.}, ans2 { 1/0., 1/0.};
    if (p1 == p2) {
        if(r == 0.) return std::make_tuple(ONE_COINCEDENT, p1,   p2  );
        else        return std::make_tuple(INFINITE,       ans1, ans2);
    }
    point center { p1.x/2 + p2.x/2, p1.y/2 + p2.y/2};
    double half_distance = distance(center, p1);
    if(half_distance > r)      return std::make_tuple(NONE,         ans1,   ans2);
    if(half_distance - r == 0) return std::make_tuple(ONE_DIAMETER, center, ans2);
    double root = std::hypot(r, half_distance) / distance(p1, p2);
    ans1.x = center.x + root * (p1.y - p2.y);
    ans1.y = center.y + root * (p2.x - p1.x);
    ans2.x = center.x - root * (p1.y - p2.y);
    ans2.y = center.y - root * (p2.x - p1.x);
    return std::make_tuple(TWO, ans1, ans2);
}

void print(result_t result, std::ostream& out = std::cout)
{
    point r1, r2; result_category res;
    std::tie(res, r1, r2) = result;
    switch(res) {
      case NONE:
        out << "There are no solutions, points are too far away\n"; break;
      case ONE_COINCEDENT: case ONE_DIAMETER:
        out << "Only one solution: " << r1.x << ' ' << r1.y << '\n'; break;
      case INFINITE:
        out << "Infinitely many circles can be drawn\n"; break;
      case TWO:
        out << "Two solutions: " << r1.x << ' ' << r1.y << " and " << r2.x << ' ' << r2.y << '\n'; break;
    }
}

int main()
{
    constexpr int size = 5;
    const point points[size*2] = {
        {0.1234, 0.9876}, {0.8765, 0.2345}, {0.0000, 2.0000}, {0.0000, 0.0000},
        {0.1234, 0.9876}, {0.1234, 0.9876}, {0.1234, 0.9876}, {0.8765, 0.2345},
        {0.1234, 0.9876}, {0.1234, 0.9876}
    };
    const double radius[size] = {2., 1., 2., .5, 0.};

    for(int i = 0; i < size; ++i)
        print(find_circles(points[i*2], points[i*2 + 1], radius[i]));
}
```

{{out}}

```txt

Two solutions: 1.96344 2.07454 and -0.963536 -0.852436
Only one solution: 0 1
Infinitely many circles can be drawn
There are no solutions, points are too far away
Only one solution: 0.1234 0.9876

```



## D

{{trans|Python}}

```d
import std.stdio, std.typecons, std.math;

class ValueException : Exception {
    this(string msg_) pure { super(msg_); }
}

struct V2 { double x, y; }
struct Circle { double x, y, r; }

/**Following explanation at:
http://mathforum.org/library/drmath/view/53027.html
*/
Tuple!(Circle, Circle)
circlesFromTwoPointsAndRadius(in V2 p1, in V2 p2, in double r)
pure in {
    assert(r >= 0, "radius can't be negative");
} body {
    enum nBits = 40;

    if (r.abs < (1.0 / (2.0 ^^ nBits)))
        throw new ValueException("radius of zero");

    if (feqrel(p1.x, p2.x) >= nBits &&
        feqrel(p1.y, p2.y) >= nBits)
        throw new ValueException("coincident points give" ~
                                 " infinite number of Circles");

    // Delta between points.
    immutable d = V2(p2.x - p1.x, p2.y - p1.y);

    // Distance between points.
    immutable q = sqrt(d.x ^^ 2 + d.y ^^ 2);
    if (q > 2.0 * r)
        throw new ValueException("separation of points > diameter");

    // Halfway point.
    immutable h = V2((p1.x + p2.x) / 2, (p1.y + p2.y) / 2);

    // Distance along the mirror line.
    immutable dm = sqrt(r ^^ 2 - (q / 2) ^^ 2);

    return typeof(return)(
        Circle(h.x - dm * d.y / q, h.y + dm * d.x / q, r.abs),
        Circle(h.x + dm * d.y / q, h.y - dm * d.x / q, r.abs));
}

void main() {
    foreach (immutable t; [
                 tuple(V2(0.1234, 0.9876), V2(0.8765, 0.2345), 2.0),
                 tuple(V2(0.0000, 2.0000), V2(0.0000, 0.0000), 1.0),
                 tuple(V2(0.1234, 0.9876), V2(0.1234, 0.9876), 2.0),
                 tuple(V2(0.1234, 0.9876), V2(0.8765, 0.2345), 0.5),
                 tuple(V2(0.1234, 0.9876), V2(0.1234, 0.9876), 0.0)]) {
        writefln("Through points:\n  %s   %s  and radius %f\n" ~
                 "You can construct the following circles:", t[]);
        try {
            writefln("  %s\n  %s\n",
                     circlesFromTwoPointsAndRadius(t[])[]);
        } catch (ValueException v)
            writefln("  ERROR: %s\n", v.msg);
    }
}
```

{{out}}

```txt
Through points:
  immutable(V2)(0.1234, 0.9876)   immutable(V2)(0.8765, 0.2345)  and radius 2.000000
You can construct the following circles:
  Circle(1.86311, 1.97421, 2)
  Circle(-0.863212, -0.752112, 2)

Through points:
  immutable(V2)(0, 2)   immutable(V2)(0, 0)  and radius 1.000000
You can construct the following circles:
  Circle(0, 1, 1)
  Circle(0, 1, 1)

Through points:
  immutable(V2)(0.1234, 0.9876)   immutable(V2)(0.1234, 0.9876)  and radius 2.000000
You can construct the following circles:
  ERROR: coincident points give infinite number of Circles

Through points:
  immutable(V2)(0.1234, 0.9876)   immutable(V2)(0.8765, 0.2345)  and radius 0.500000
You can construct the following circles:
  ERROR: separation of points > diameter

Through points:
  immutable(V2)(0.1234, 0.9876)   immutable(V2)(0.1234, 0.9876)  and radius 0.000000
You can construct the following circles:
  ERROR: radius of zero

```



## Elixir

{{trans|Ruby}}

```elixir
defmodule RC do
  def circle(p, p, r) when r>0.0 do
    raise ArgumentError, message: "Infinite number of circles, points coincide."
  end
  def circle(p, p, r) when r==0.0 do
    {px, py} = p
    [{px, py, r}]
  end
  def circle({p1x,p1y}, {p2x,p2y}, r) do
    {dx, dy} = {p2x-p1x, p2y-p1y}
    q = :math.sqrt(dx*dx + dy*dy)
    if q > 2*r do
      raise ArgumentError, message: "Distance of points > diameter."
    else
      {x3, y3} = {(p1x+p2x) / 2, (p1y+p2y) / 2}
      d = :math.sqrt(r*r - q*q/4)
      Enum.uniq([{x3 - d*dy/q, y3 + d+dx/q, r}, {x3 + d*dy/q, y3 - d*dx/q, r}])
    end
  end
end

data = [{{0.1234, 0.9876}, {0.8765, 0.2345}, 2.0},
        {{0.0000, 2.0000}, {0.0000, 0.0000}, 1.0},
        {{0.1234, 0.9876}, {0.1234, 0.9876}, 2.0},
        {{0.1234, 0.9876}, {0.8765, 0.2345}, 0.5},
        {{0.1234, 0.9876}, {0.1234, 0.9876}, 0.0}]

Enum.each(data, fn {p1, p2, r} ->
  IO.write "Given points:\n  #{inspect p1},\n  #{inspect p2}\n  and radius #{r}\n"
  try do
    circles = RC.circle(p1, p2, r)
    IO.puts "You can construct the following circles:"
    Enum.each(circles, fn circle -> IO.puts "  #{inspect circle}" end)
  rescue
    e in ArgumentError -> IO.inspect e
  end
  IO.puts ""
end)
```


{{out}}

```txt

Given points:
  {0.1234, 0.9876},
  {0.8765, 0.2345}
  and radius 2.0
You can construct the following circles:
  {1.8631118016581893, 3.2459586888005014, 2.0}
  {-0.8632118016581896, -0.7521118016581892, 2.0}

Given points:
  {0.0, 2.0},
  {0.0, 0.0}
  and radius 1.0
You can construct the following circles:
  {0.0, 1.0, 1.0}

Given points:
  {0.1234, 0.9876},
  {0.1234, 0.9876}
  and radius 2.0
%ArgumentError{message: "Infinite number of circles, points coincide."}

Given points:
  {0.1234, 0.9876},
  {0.8765, 0.2345}
  and radius 0.5
%ArgumentError{message: "Distance of points > diameter."}

Given points:
  {0.1234, 0.9876},
  {0.1234, 0.9876}
  and radius 0.0
You can construct the following circles:
  {0.1234, 0.9876, 0.0}

```



## ERRE

<lang>
PROGRAM CIRCLES

!
! for rosettacode.org
!

PROCEDURE CIRCLE_CENTER(X1,Y1,X2,Y2,R->MSG$)
  LOCAL D,W,X3,Y3

        D=SQR((X2-X1)^2+(Y2-Y1)^2)
        IF D=0 THEN
             MSG$="NO CIRCLES CAN BE DRAWN, POINTS ARE IDENTICAL"
             EXIT PROCEDURE
        END IF
        X3=(X1+X2)/2  Y3=(Y1+Y2)/2

        W=R^2-(D/2)^2
        IF W<0 THEN
             MSG$="NO SOLUTION"
             EXIT PROCEDURE
        END IF
        CX1=X3+SQR(W)*(Y1-Y2)/D   CY1=Y3+SQR(W)*(X2-X1)/D
        CX2=X3-SQR(W)*(Y1-Y2)/D   CY2=Y3-SQR(W)*(X2-X1)/D
        IF D=R*2 THEN
             MSG$="POINTS ARE OPPOSITE ENDS OF A DIAMETER CENTER = "+STR$(CX1)+","+STR$(CY1)
             EXIT PROCEDURE
        END IF
        IF D>R*2 THEN
             MSG$="POINTS ARE TOO FAR"
             EXIT PROCEDURE
        END IF
        IF R<=0 THEN
             MSG$="RADIUS IS NOT VALID"
             EXIT PROCEDURE
        END IF
        MSG$=STR$(CX1)+","+STR$(CY1)+" & "+STR$(CX2)+","+STR$(CY2)
END PROCEDURE

BEGIN
DATA(0.1234,0.9876,0.8765,0.2345,2.0)
DATA(0.0000,2.0000,0.0000,0.0000,1.0)
DATA(0.1234,0.9876,0.1234,0.9876,2.0)
DATA(0.1234,0.9876,0.8765,0.2345,0.5)
DATA(0.1234,0.9876,0.1234,0.9876,0.0)

FOR I%=1 TO 5 DO
   READ(PX,PY,QX,QY,RADIUS)
   CIRCLE_CENTER(PX,PY,QX,QY,RADIUS->MSG$)
   PRINT(MSG$)
END FOR
END PROGRAM

```


=={{header|F#|F sharp}}==

```fsharp
open System

let add (a:double, b:double) (x:double, y:double) = (a + x, b + y)
let sub (a:double, b:double) (x:double, y:double) = (a - x, b - y)
let magSqr (a:double, b:double) = a * a + b * b
let mag a:double = Math.Sqrt(magSqr a)
let mul (a:double, b:double) c = (a * c, b * c)
let div2 (a:double, b:double) c = (a / c, b / c)
let perp (a:double, b:double) = (-b, a)
let norm a = div2 a (mag a)

let circlePoints p q (radius:double) =
    let diameter = radius * 2.0
    let pq = sub p q
    let magPQ = mag pq
    let midpoint = div2 (add p q) 2.0
    let halfPQ = magPQ / 2.0
    let magMidC = Math.Sqrt(Math.Abs(radius * radius - halfPQ * halfPQ))
    let midC = mul (norm (perp pq)) magMidC
    let center1 = add midpoint midC
    let center2 = sub midpoint midC
    if radius = 0.0 then None
    else if p = q then None
    else if diameter < magPQ then None
    else Some (center1, center2)

[<EntryPoint>]
let main _ = 
    printfn "%A" (circlePoints (0.1234, 0.9876) (0.8765, 0.2345) 2.0)
    printfn "%A" (circlePoints (0.0, 2.0) (0.0, 0.0) 1.0)
    printfn "%A" (circlePoints (0.1234, 0.9876) (0.1234, 0.9876) 2.0)
    printfn "%A" (circlePoints (0.1234, 0.9876) (0.8765, 0.2345) 0.5)
    printfn "%A" (circlePoints (0.1234, 0.9876) (0.1234, 0.1234) 0.0)

    0 // return an integer exit code
```

{{out}}

```txt
Some ((-0.8632118017, -0.7521118017), (1.863111802, 1.974211802))
Some ((0.0, 1.0), (0.0, 1.0))
<null>
<null>
<null>
```



## Factor


```factor
USING: accessors combinators combinators.short-circuit
formatting io kernel literals locals math math.distances
math.functions prettyprint sequences strings ;
IN: rosetta-code.circles
DEFER: find-circles

! 
###  Input =================================================


TUPLE: input p1 p2 r ;

CONSTANT: test-cases {
    T{ input f { 0.1234 0.9876 } { 0.8765 0.2345 } 2 }
    T{ input f { 0 2 } { 0 0 } 1 }
    T{ input f { 0.1234 0.9876 } { 0.1234 0.9876 } 2 }
    T{ input f { 0.1234 0.9876 } { 0.8765 0.2345 } 0.5 }
    T{ input f { 0.1234 0.9876 } { 0.1234 0.9876 } 0 }
}

! 
###  Edge case handling ====================================


CONSTANT: infinite
    "there could be an infinite number of circles."

CONSTANT: too-far
    "points are too far apart to form circles."

: coincident? ( input -- ? ) [ p1>> ] [ p2>> ] bi = ;

: degenerate? ( input -- ? )
    { [ r>> zero? ] [ coincident? ] } 1&& ;

: infinite? ( input -- ? )
    { [ r>> zero? not ] [ coincident? ] } 1&& ;

: too-far? ( input -- ? )
    [ r>> 2 * ] [ p1>> ] [ p2>> ] tri euclidian-distance < ;

: degenerate ( input -- str )
    p1>> [ first ] [ second ] bi
    "one degenerate circle found at (%.4f, %.4f).\n" sprintf ;

: check-input ( input -- obj )
    {
        { [ dup infinite?   ] [ drop infinite ] }
        { [ dup too-far?    ] [ drop too-far  ] }
        { [ dup degenerate? ] [ degenerate    ] }
        [ find-circles ]
    } cond ;

! 
###  Program Logic =========================================


:: (circle-coords) ( a b c r q quot -- x )
    a r sq q 2 / sq - sqrt b c - * q / quot call ; inline

: circle-coords ( quot -- x y )
    [ + ] over [ - ] [ [ call ] dip (circle-coords) ] 2bi@ ;
    inline

:: find-circles ( input -- circles )
    input [ r>> ] [ p1>> ] [ p2>> ] tri    :> ( r p1 p2 )
    p1 p2 [ [ first ] [ second ] bi ] bi@  :> ( x1 y1 x2 y2 )
    x1 x2 y1 y2 [ + 2 / ] 2bi@             :> ( x3 y3 )
    p1 p2 euclidian-distance               :> q
    [ x3 y1 y2 r q ]
    [ y3 x2 x1 r q ] [ circle-coords ] bi@ :> ( x w y z )
    { x y } { w z } = { { x y } } { { w z } { x y } } ? ;

! 
###  Output ================================================


: .point ( seq -- )
    [ first ] [ second ] bi "(%.4f, %.4f)" printf ;

: .given ( input -- )
    [ r>> ] [ p2>> ] [ p1>> ] tri
    "Given points " write .point ", " write .point
    ", and radius %.2f,\n" printf ;

: .one ( seq -- )
    first "one circle found at " write .point "." print ;

: .two ( seq -- )
    [ first ] [ second ] bi "two circles found at " write 
    .point " and " write .point "." print ;

: .circles ( seq -- ) dup length 1 = [ .one ] [ .two ] if ;

! 
###  Main word =============================================


: circles-demo ( -- )
    test-cases [
        dup .given check-input dup string?
        [ print ] [ .circles ] if nl
    ] each ;

MAIN: circles-demo
```

{{out}}

```txt

Given points (0.1234, 0.9876), (0.8765, 0.2345), and radius 2.00,
two circles found at (1.8631, 1.9742) and (-0.8632, -0.7521).

Given points (0.0000, 2.0000), (0.0000, 0.0000), and radius 1.00,
one circle found at (0.0000, 1.0000).

Given points (0.1234, 0.9876), (0.1234, 0.9876), and radius 2.00,
there could be an infinite number of circles.

Given points (0.1234, 0.9876), (0.8765, 0.2345), and radius 0.50,
points are too far apart to form circles.

Given points (0.1234, 0.9876), (0.1234, 0.9876), and radius 0.00,
one degenerate circle found at (0.1234, 0.9876).

```



## Fortran


```fortran

! Implemented by Anant Dixit (Nov. 2014)
! Transpose elements in find_center to obtain correct results. R.N. McLean (Dec 2017)
program circles
implicit none
double precision :: P1(2), P2(2), R

P1 = (/0.1234d0, 0.9876d0/)
P2 = (/0.8765d0,0.2345d0/)
R = 2.0d0
call print_centers(P1,P2,R)

P1 = (/0.0d0, 2.0d0/)
P2 = (/0.0d0,0.0d0/)
R = 1.0d0
call print_centers(P1,P2,R)

P1 = (/0.1234d0, 0.9876d0/)
P2 = (/0.1234d0, 0.9876d0/)
R = 2.0d0
call print_centers(P1,P2,R)

P1 = (/0.1234d0, 0.9876d0/)
P2 = (/0.8765d0, 0.2345d0/)
R = 0.5d0
call print_centers(P1,P2,R)

P1 = (/0.1234d0, 0.9876d0/)
P2 = (/0.1234d0, 0.9876d0/)
R = 0.0d0
call print_centers(P1,P2,R)
end program circles

subroutine print_centers(P1,P2,R)
implicit none
double precision :: P1(2), P2(2), R, Center(2,2)
integer :: Res
call test_inputs(P1,P2,R,Res)
write(*,*)
write(*,'(A10,F7.4,A1,F7.4)') 'Point1  : ', P1(1), ' ', P1(2)
write(*,'(A10,F7.4,A1,F7.4)') 'Point2  : ', P2(1), ' ', P2(2)
write(*,'(A10,F7.4)') 'Radius  : ', R
if(Res.eq.1) then
  write(*,*) 'Same point because P1=P2 and r=0.'
elseif(Res.eq.2) then
  write(*,*) 'No circles can be drawn because r=0.'
elseif(Res.eq.3) then
  write(*,*) 'Infinite circles because P1=P2 for non-zero radius.'
elseif(Res.eq.4) then
  write(*,*) 'No circles with given r can be drawn because points are far apart.'
elseif(Res.eq.0) then
  call find_center(P1,P2,R,Center)
  if(Center(1,1).eq.Center(2,1) .and. Center(1,2).eq.Center(2,2)) then
    write(*,*) 'Points lie on the diameter. A single circle can be drawn.'
    write(*,'(A10,F7.4,A1,F7.4)') 'Center  : ', Center(1,1), ' ', Center(1,2)
  else
    write(*,*) 'Two distinct circles found.'
    write(*,'(A10,F7.4,A1,F7.4)') 'Center1 : ', Center(1,1), ' ', Center(1,2)
    write(*,'(A10,F7.4,A1,F7.4)') 'Center2 : ', Center(2,1), ' ', Center(2,2)
  end if
elseif(Res.lt.0) then
  write(*,*) 'Incorrect value for r.'
end if
write(*,*)
end subroutine print_centers

subroutine test_inputs(P1,P2,R,Res)
implicit none
double precision :: P1(2), P2(2), R, dist
integer :: Res
if(R.lt.0.0d0) then
  Res = -1
  return
elseif(R.eq.0.0d0 .and. P1(1).eq.P2(1) .and. P1(2).eq.P2(2)) then
  Res = 1
  return
elseif(R.eq.0.0d0) then
  Res = 2
  return
elseif(P1(1).eq.P2(1) .and. P1(2).eq.P2(2)) then
  Res = 3
  return
else
  dist = sqrt( (P1(1)-P2(1))**2 + (P1(2)-P2(2))**2 )
  if(dist.gt.2.0d0*R) then
    Res = 4
    return
  else
    Res = 0
    return
  end if
end if
end subroutine test_inputs

subroutine find_center(P1,P2,R,Center)
implicit none
double precision :: P1(2), P2(2), MP(2), Center(2,2), R, dm, dd
MP = (P1 + P2)/2.0d0
dm = sqrt((P1(1) - P2(1))**2 + (P1(2) - P2(2))**2)
dd = sqrt(R**2 - (dm/2.0d0)**2)
Center(1,1) = MP(1) - dd*(P2(2) - P1(2))/dm
Center(1,2) = MP(2) + dd*(P2(1) - P1(1))/dm

Center(2,1) = MP(1) + dd*(P2(2) - P1(2))/dm
Center(2,2) = MP(2) - dd*(P2(1) - P1(1))/dm
end subroutine find_center
```


{{out}}

```txt


Point1  :  0.1234  0.9876
Point2  :  0.8765  0.2345
Radius  :  2.0000
 Two distinct circles found.
Center1 :  1.8631  1.9742
Center2 : -0.8632 -0.7521


Point1  :  0.0000  2.0000
Point2  :  0.0000  0.0000
Radius  :  1.0000
 Points lie on the diameter. A single circle can be drawn.
Center  :  0.0000  1.0000


Point1  :  0.1234  0.9876
Point2  :  0.1234  0.9876
Radius  :  2.0000
 Infinite circles because P1=P2 for non-zero radius.


Point1  :  0.1234  0.9876
Point2  :  0.8765  0.2345
Radius  :  0.5000
 No circles with given r can be drawn because points are far apart.


Point1  :  0.1234  0.9876
Point2  :  0.1234  0.9876
Radius  :  0.0000
 Same point because P1=P2 and r=0.

```



### Using complex numbers

Fortran 66 made standard the availability of complex number arithmetic. This version however takes advantage of facilities offered in F90 so as to perform some array-based arithmetic, though the opportunities in this small routine are thin: two statements become one (look for CMPLX). More seriously, the MODULE facility allows the definition of an array SQUAWK which contains an explanatory text associated with each return code. The routine has a troublesome variety of possible odd conditions to report. An older approach would be to have a return message CHARACTER variable to present the remark, at the cost of filling up that variable with text every time. By returning an integer code, less effort is required, but there is no explication of the return codes. One could still have an array of messages (and prior to F90, array index counting started at one only, so no starting with -3 for errorish  codes) but making that array available would require some sort of COMMON storage. The MODULE facility eases this problem.

```Fortran
      MODULE GEOMETRY	!Limited scope.
       CHARACTER*(*) SQUAWK(-3:2)	!Holds a schedule of complaints.
       PARAMETER (SQUAWK = (/		!According to what might go wrong.
     3  "No circles: points are more than 2R apart.",
     2  "Innumerable circles: co-incident points, R > 0.",
     1  "One 'circle', centred on the co-incident points. R is zero!",
     o  "No circles! R is negative!",
     1  "One circle: points are 2R apart.",
     2  "Two circles."/))		!This last is the hoped-for state.
      CONTAINS	!Now for the action.
       SUBROUTINE BUBBLE(P,R,N)	!Finds circles of radius R passing through two points.
        COMPLEX P(2)	!The two points. Results returned here.
        REAL R		!The specified radius.
        INTEGER N	!Indicates how many centres are valid.
        COMPLEX MID,DP	!Geometrical assistants.
         DP = (P(2) - P(1))/2	!Or, the other way around.
         D = ABS(DP)		!Half the separation is useful.
         IF (R.LT.0) THEN	!Is the specified radius silly?
           N =  0			!Yes. No circles, then.
         ELSE IF (D.EQ.0) THEN	!Any distance between the points?
           IF (R.EQ.0) THEN		!No. Zero radius?
             N = -1				!Yes. So a degenerate "circle" of zero radius.
            ELSE			!A negative radius being tested for above,
             N = -2				!A swirl of circles around the midpoint.
           END IF		!So much for co-incident points.
         ELSE IF (D.GT.R) THEN	!Points too far apart?
           N = -3			!A circle of radius R can't reach them.
         ELSE IF (D.EQ.R) THEN	!Maximum separation for R?
           N = 1			!Yes. The two circles lie atop each other.
           P(1) = (P(1) + P(2))/2	!Both centres are on the midpoint, but N = 1.
         ELSE			!Finally, the ordinary case.
           N = 2			!Two circles.
           MID = (P(1) + P(2))/2	!Midway between the two points.
           D = SQRT((R/D)**2 - 1)	!Rescale vector DP.
           P = MID + DP*CMPLX(0,(/+D,-D/))	!Array (0,+D), (0,-D)
         END IF				!P(1) = DP*CMPLX(0,+D) and P(2) = DP*CMPLX(0,-D)
       END SUBROUTINE BUBBLE	!Careful! P and N are modified.
      END MODULE GEOMETRY	!Not much.

      PROGRAM POKE	!A tester.
      USE GEOMETRY	!Useful to I. Newton.
      COMPLEX P(2)		!A pair of points.
      REAL PP(4)		!Also a pair.
      EQUIVALENCE (P,PP)	!Since free-format input likes (x,y), not x,y
      REAL R			!This is not complex.
      INTEGER MSG,IN		!I/O unit numbers.
      MSG = 6			!Standard output.
      OPEN (MSG, RECL = 120)	!For "formatted" files, this length is in characters.
      IN = 10			!For the disc file holding the test data.
      WRITE (MSG,1)		!Announce.
    1 FORMAT ("Given two points and a radius, find the centres "
     1 "of circles of that radius passing through those points.")

      OPEN (IN,FILE="Circle.csv", STATUS = "OLD", ACTION="READ")	!Have data, will compute.
   10 READ (IN,*,END = 20) PP,R		!Get two points and a radius.
      WRITE (MSG,*)			!Set off.
      WRITE (MSG,*) P,R			!Show the input.
      CALL BUBBLE(P,R,N)		!Calculate.
      WRITE (MSG,*) P(1:N),SQUAWK(N)	!Show results.
      GO TO 10				!Try it again.

   20 CLOSE(IN)		!Finihed with input.
      END	!Finished. 
```


Results: little attempt has been made to present a fancy layout, "free-format" output does well enough. Notably, complex numbers are presented in brackets with a comma as ''(x,y)''; a FORMAT statement version would have to supply those decorations. Free-format input also expects such bracketing when reading complex numbers. The supplied data format however does ''not'' include the brackets and so is improper. Suitable data would be

```txt

(0.1234, 0.9876)    (0.8765, 0.2345)    2.0
(0.0000, 2.0000)    (0.0000, 0.0000)    1.0
(0.1234, 0.9876)    (0.1234, 0.9876)    2.0
(0.1234, 0.9876)    (0.8765, 0.2345)    0.5
(0.1234, 0.9876)    (0.1234, 0.9876)    0.0

```

The free-format input style allows spaces, a comma (with or without spaces), and even a tab as delimiters between data, but does not allow implicit delimiters so a sequence such as 2017-12-29 (a standard date format) would be rejected. Because the style of the supplied data does not include the brackets, when complex numbers are read from such an input stream, they are taken to be real numbers only so that each real number is deemed a complex number of the form (x,0); in this case the second number would be taken as being the real part of the second complex number. A mess results.

By using the EQUIVALENCE statement, array PP can be read via the free-format protocol, and so the first four numbers will be placed in array PP, which just happens to be the same storage area as the array P of complex numbers. This of course means that should proper bracketed complex numbers be presented as input, a different mess results.

Output:

```txt

Given two points and a radius, find the centres of circles of that radius passing through those points.

 (0.1234000,0.9876000) (0.8765000,0.2345000)   2.000000
 (1.863112,1.974212) (-0.8632119,-0.7521119) Two circles.

 (0.0000000E+00,2.000000) (0.0000000E+00,0.0000000E+00)   1.000000
 (0.0000000E+00,1.000000) One circle: points are 2R apart.

 (0.1234000,0.9876000) (0.1234000,0.9876000)   2.000000
 Innumerable circles: co-incident points, R > 0.

 (0.1234000,0.9876000) (0.8765000,0.2345000)  0.5000000
 No circles: points are more than 2R apart.

 (0.1234000,0.9876000) (0.1234000,0.9876000)  0.0000000E+00
 One 'circle', centred on the co-incident points. R is zero!

```



## Go


```go
package main

import (
    "fmt"
    "math"
)

var (
    Two  = "Two circles."
    R0   = "R==0.0 does not describe circles."
    Co   = "Coincident points describe an infinite number of circles."
    CoR0 = "Coincident points with r==0.0 describe a degenerate circle."
    Diam = "Points form a diameter and describe only a single circle."
    Far  = "Points too far apart to form circles."
)

type point struct{ x, y float64 }

func circles(p1, p2 point, r float64) (c1, c2 point, Case string) {
    if p1 == p2 {
        if r == 0 {
            return p1, p1, CoR0
        }
        Case = Co
        return
    }
    if r == 0 {
        return p1, p2, R0
    }
    dx := p2.x - p1.x
    dy := p2.y - p1.y
    q := math.Hypot(dx, dy)
    if q > 2*r {
        Case = Far
        return
    }
    m := point{(p1.x + p2.x) / 2, (p1.y + p2.y) / 2}
    if q == 2*r {
        return m, m, Diam
    }
    d := math.Sqrt(r*r - q*q/4)
    ox := d * dx / q
    oy := d * dy / q
    return point{m.x - oy, m.y + ox}, point{m.x + oy, m.y - ox}, Two
}

var td = []struct {
    p1, p2 point
    r      float64
}{
    {point{0.1234, 0.9876}, point{0.8765, 0.2345}, 2.0},
    {point{0.0000, 2.0000}, point{0.0000, 0.0000}, 1.0},
    {point{0.1234, 0.9876}, point{0.1234, 0.9876}, 2.0},
    {point{0.1234, 0.9876}, point{0.8765, 0.2345}, 0.5},
    {point{0.1234, 0.9876}, point{0.1234, 0.9876}, 0.0},
}

func main() {
    for _, tc := range td {
        fmt.Println("p1: ", tc.p1)
        fmt.Println("p2: ", tc.p2)
        fmt.Println("r: ", tc.r)
        c1, c2, Case := circles(tc.p1, tc.p2, tc.r)
        fmt.Println("  ", Case)
        switch Case {
        case CoR0, Diam:
            fmt.Println("   Center: ", c1)
        case Two:
            fmt.Println("   Center 1: ", c1)
            fmt.Println("   Center 2: ", c2)
        }
        fmt.Println()
    }
}
```

{{out}}

```txt

p1:  {0.1234 0.9876}
p2:  {0.8765 0.2345}
r:  2
   Two circles.
   Center 1:  {1.8631118016581891 1.974211801658189}
   Center 2:  {-0.8632118016581893 -0.752111801658189}

p1:  {0 2}
p2:  {0 0}
r:  1
   Points form a diameter and describe only a single circle.
   Center:  {0 1}

p1:  {0.1234 0.9876}
p2:  {0.1234 0.9876}
r:  2
   Coincident points describe an infinite number of circles.

p1:  {0.1234 0.9876}
p2:  {0.8765 0.2345}
r:  0.5
   Points too far apart to form circles.

p1:  {0.1234 0.9876}
p2:  {0.1234 0.9876}
r:  0
   Coincident points with r==0.0 describe a degenerate circle.
   Center:  {0.1234 0.9876}

```



## Haskell


```Haskell
add (a, b) (x, y) = (a + x, b + y)
sub (a, b) (x, y) = (a - x, b - y)
magSqr (a, b)     = (a ^^ 2) + (b ^^ 2)
mag a             = sqrt $ magSqr a
mul (a, b) c      = (a * c, b * c)
div2 (a, b) c     = (a / c, b / c)
perp (a, b)       = (negate b, a)
norm a            = a `div2` mag a

circlePoints :: (Ord a, Floating a) =>
                (a, a) -> (a, a) -> a -> Maybe ((a, a), (a, a))
circlePoints p q radius
  | radius == 0      = Nothing
  | p == q           = Nothing
  | diameter < magPQ = Nothing
  | otherwise        = Just (center1, center2)
  where
    diameter = radius * 2
    pq       = p `sub` q
    magPQ    = mag pq
    midpoint = (p `add` q) `div2` 2
    halfPQ   = magPQ / 2
    magMidC  = sqrt . abs $ (radius ^^ 2) - (halfPQ ^^ 2)
    midC     = (norm $ perp pq) `mul` magMidC
    center1  = midpoint `add` midC
    center2  = midpoint `sub` midC

uncurry3 f (a, b, c) = f a b c

main :: IO ()
main = mapM_ (print . uncurry3 circlePoints)
  [((0.1234, 0.9876), (0.8765, 0.2345), 2),
   ((0     , 2     ), (0     , 0     ), 1),
   ((0.1234, 0.9876), (0.1234, 0.9876), 2),
   ((0.1234, 0.9876), (0.8765, 0.2345), 0.5),
   ((0.1234, 0.9876), (0.1234, 0.1234), 0)]
```

{{out}}

```txt
Just ((-0.8632118016581896,-0.7521118016581892),(1.8631118016581893,1.974211801658189))
Just ((0.0,1.0),(0.0,1.0))
Nothing
Nothing
Nothing
```

=={{header|Icon}} and {{header|Unicon}}==
{{trans|AutoHotKey}}
Works in both languages.

```unicon
procedure main()
    A := [ [0.1234, 0.9876,   0.8765, 0.2345,   2.0],
           [0.0000, 2.0000,   0.0000, 0.0000,   1.0],
           [0.1234, 0.9876,   0.1234, 0.9876,   2.0],
           [0.1234, 0.9876,   0.9765, 0.2345,   0.5],
           [0.1234, 0.9876,   0.1234, 0.9876,   0.0] ]
    every write(cCenter!!A)
end

procedure cCenter(x1,y1, x2,y2, r)
    if r <= 0 then return "Illegal radius"
    r2 := r*2
    d := ((x2-x1)^2 + (y2-y1)^2)^0.5
    if d = 0 then return "Identical points, infinite number of circles"
    if d > r2 then return "No circles possible"
    z   := (r^2-(d/2.0)^2)^0.5
    x3  := (x1+x2)/2.0;     y3 := (y1+y2)/2.0
    cx1 := x3+z*(y1-y2)/d; cy1 := y3+z*(x2-x1)/d
    cx2 := x3-z*(y1-y2)/d; cy2 := y3-z*(x2-x1)/d
    if d = r2 then return "Single circle at ("||cx1||","||cy1||")"
    return "("||cx1||","||cy1||") and ("||cx2||","||cy2||")"
end
```


{{out}}

```txt

->cgr
(1.863111801658189,1.974211801658189) and (-0.8632118016581896,-0.7521118016581892)
Single circle at (0.0,1.0)
Identical points, infinite number of circles
No circles possible
Illegal radius
->

```



## J


2D computations are often easier using the complex plane.

```J
average =: +/ % #

circles =: verb define"1
 'P0 P1 R' =. (j./"1)_2[\y NB. Use complex plane
 C =. P0 average@:, P1
 BAD =: ":@:+. C
 SEPARATION =. P0 |@- P1
 if. 0 = SEPARATION do.
  if. 0 = R do. 'Degenerate point at ' , BAD
  else. 'Any center at a distance ' , (":R) , ' from ' , BAD , ' works.'
  end.
 elseif. SEPARATION (> +:) R do. 'No solutions.'
 elseif. SEPARATION (= +:) R do. 'Duplicate solutions with center at ' , BAD
 elseif. 1 do.
  ORTHOGONAL_DISTANCE =. R * 1 o. _2 o. R %~ | C - P0
  UNIT =: P1 *@:- P0
  OFFSETS =: ORTHOGONAL_DISTANCE * UNIT * j. _1 1
  C +.@:+ OFFSETS
 end.
)

INPUT=: ".;._2]0 :0
 0.1234 0.9876 0.8765 0.2345   2
      0      2      0      0   1
 0.1234 0.9876 0.1234 0.9876   2
 0.1234 0.9876 0.8765 0.2345 0.5
 0.1234 0.9876 0.1234 0.9876   0
)

   ('x0 y0 x1 y1 r' ; 'center'),(;circles)"1 INPUT
┌───────────────────────────────┬────────────────────────────────────────────────────┐
│x0 y0 x1 y1 r                  │center                                              │
├───────────────────────────────┼────────────────────────────────────────────────────┤
│0.1234 0.9876 0.8765 0.2345 2  │_0.863212 _0.752112                                 │
│                               │  1.86311   1.97421                                 │
├───────────────────────────────┼────────────────────────────────────────────────────┤
│0 2 0 0 1                      │Duplicate solutions with center at 0 1              │
├───────────────────────────────┼────────────────────────────────────────────────────┤
│0.1234 0.9876 0.1234 0.9876 2  │Any center at a distance 2 from 0.1234 0.9876 works.│
├───────────────────────────────┼────────────────────────────────────────────────────┤
│0.1234 0.9876 0.8765 0.2345 0.5│No solutions.                                       │
├───────────────────────────────┼────────────────────────────────────────────────────┤
│0.1234 0.9876 0.1234 0.9876 0  │Degenerate point at 0.1234 0.9876                   │
└───────────────────────────────┴────────────────────────────────────────────────────┘

```



## Java

{{trans|Kotlin}}

```Java
import java.util.Objects;

public class Circles {
    private static class Point {
        private final double x, y;

        public Point(Double x, Double y) {
            this.x = x;
            this.y = y;
        }

        public double distanceFrom(Point other) {
            double dx = x - other.x;
            double dy = y - other.y;
            return Math.sqrt(dx * dx + dy * dy);
        }

        @Override
        public boolean equals(Object other) {
            if (this == other) return true;
            if (other == null || getClass() != other.getClass()) return false;
            Point point = (Point) other;
            return x == point.x && y == point.y;
        }

        @Override
        public String toString() {
            return String.format("(%.4f, %.4f)", x, y);
        }
    }

    private static Point[] findCircles(Point p1, Point p2, double r) {
        if (r < 0.0) throw new IllegalArgumentException("the radius can't be negative");
        if (r == 0.0 && p1 != p2) throw new IllegalArgumentException("no circles can ever be drawn");
        if (r == 0.0) return new Point[]{p1, p1};
        if (Objects.equals(p1, p2)) throw new IllegalArgumentException("an infinite number of circles can be drawn");
        double distance = p1.distanceFrom(p2);
        double diameter = 2.0 * r;
        if (distance > diameter) throw new IllegalArgumentException("the points are too far apart to draw a circle");
        Point center = new Point((p1.x + p2.x) / 2.0, (p1.y + p2.y) / 2.0);
        if (distance == diameter) return new Point[]{center, center};
        double mirrorDistance = Math.sqrt(r * r - distance * distance / 4.0);
        double dx = (p2.x - p1.x) * mirrorDistance / distance;
        double dy = (p2.y - p1.y) * mirrorDistance / distance;
        return new Point[]{
            new Point(center.x - dy, center.y + dx),
            new Point(center.x + dy, center.y - dx)
        };
    }

    public static void main(String[] args) {
        Point[] p = new Point[]{
            new Point(0.1234, 0.9876),
            new Point(0.8765, 0.2345),
            new Point(0.0000, 2.0000),
            new Point(0.0000, 0.0000)
        };
        Point[][] points = new Point[][]{
            {p[0], p[1]},
            {p[2], p[3]},
            {p[0], p[0]},
            {p[0], p[1]},
            {p[0], p[0]},
        };
        double[] radii = new double[]{2.0, 1.0, 2.0, 0.5, 0.0};
        for (int i = 0; i < radii.length; ++i) {
            Point p1 = points[i][0];
            Point p2 = points[i][1];
            double r = radii[i];
            System.out.printf("For points %s and %s with radius %f\n", p1, p2, r);
            try {
                Point[] circles = findCircles(p1, p2, r);
                Point c1 = circles[0];
                Point c2 = circles[1];
                if (Objects.equals(c1, c2)) {
                    System.out.printf("there is just one circle with center at %s\n", c1);
                } else {
                    System.out.printf("there are two circles with centers at %s and %s\n", c1, c2);
                }
            } catch (IllegalArgumentException ex) {
                System.out.println(ex.getMessage());
            }
            System.out.println();
        }
    }
}
```

{{out}}

```txt
For points (0.1234, 0.9876) and (0.8765, 0.2345) with radius 2.000000
there are two circles with centers at (1.8631, 1.9742) and (-0.8632, -0.7521)

For points (0.0000, 2.0000) and (0.0000, 0.0000) with radius 1.000000
there is just one circle with center at (0.0000, 1.0000)

For points (0.1234, 0.9876) and (0.1234, 0.9876) with radius 2.000000
an infinite number of circles can be drawn

For points (0.1234, 0.9876) and (0.8765, 0.2345) with radius 0.500000
the points are too far apart to draw a circle

For points (0.1234, 0.9876) and (0.1234, 0.9876) with radius 0.000000
there is just one circle with center at (0.1234, 0.9876)
```



## JavaScript



### =ES6=



```JavaScript
const hDist = (p1, p2) => Math.hypot(...p1.map((e, i) => e - p2[i])) / 2;
const pAng = (p1, p2) => Math.atan(p1.map((e, i) => e - p2[i]).reduce((p, c) => c / p, 1));
const solveF = (p, r) => t => [r*Math.cos(t) + p[0], r*Math.sin(t) + p[1]];
const diamPoints = (p1, p2) => p1.map((e, i) => e + (p2[i] - e) / 2);

const findC = (...args) => {
  const [p1, p2, s] = args;
  const solve = solveF(p1, s);
  const halfDist = hDist(p1, p2);

  let msg = `p1: ${p1}, p2: ${p2}, r:${s} Result: `;
  switch (Math.sign(s - halfDist)) {
    case 0:
      msg += s ? `Points on diameter. Circle at: ${diamPoints(p1, p2)}` :
        'Radius Zero';
      break;
    case 1:
      if (!halfDist) {
        msg += 'Coincident point. Infinite solutions';
      }
      else {
        let theta = pAng(p1, p2);
        let theta2 = Math.acos(halfDist / s);
        [1, -1].map(e => solve(theta + e * theta2)).forEach(
          e => msg += `Circle at ${e} `);
      }
      break;
    case -1:
      msg += 'No intersection. Points further apart than circle diameter';
      break;
  }
  return msg;
};


[
  [[0.1234, 0.9876], [0.8765, 0.2345], 2.0],
  [[0.0000, 2.0000], [0.0000, 0.0000], 1.0],
  [[0.1234, 0.9876], [0.1234, 0.9876], 2.0],
  [[0.1234, 0.9876], [0.8765, 0.2345], 0.5],
  [[0.1234, 0.9876], [0.1234, 0.9876], 0.0]
].forEach((t,i) => console.log(`Test: ${i}: ${findC(...t)}`));

```
 

Output:

```JavaScript

Test: 0: p1: 0.1234,0.9876, p2: 0.8765,0.2345, r:2 Result: Circle at 1.8631118016581891,1.974211801658189 Circle at -0.863211801658189,-0.7521118016581889 
Test: 1: p1: 0,2, p2: 0,0, r:1 Result: Points on diameter. Circle at: 0,1
Test: 2: p1: 0.1234,0.9876, p2: 0.1234,0.9876, r:2 Result: Coincident point. Infinite solutions
Test: 3: p1: 0.1234,0.9876, p2: 0.8765,0.2345, r:0.5 Result: No intersection. Points further apart than circle diameter
Test: 4: p1: 0.1234,0.9876, p2: 0.1234,0.9876, r:0 Result: Radius Zero

```
 


## jq

{{works with|jq|1.4}}
In this section, a point in the plane will be represented by its Cartesian co-ordinates expressed as a JSON array: [x,y].

```jq
# circle_centers is defined here as a filter.
# Input should be an array [x1, y1, x2, y2, r] giving the co-ordinates
# of the two points and a radius.
# If there is one solution, the output is the circle center;
# if there are two solutions centered at [x1, y1] and [x2, y2],
# then the output is [x1, y1, x2, y2];
# otherwise an explanatory string is returned.

def circle_centers:
  def sq: .*.;
  def c(x3; y1; y2; r; d): x3 + ((r|sq - ((d/2)|sq)) | sqrt) * (y1-y2)/d;
      
  .[0] as $x1 | .[1] as $y1 | .[2] as $x2 | .[3] as $y2 | .[4] as $r
  | ((($x2-$x1)|sq) + (($y2-$y1)|sq) | sqrt) as $d
  | (($x1+$x2)/2) as $x3
  | (($y1+$y2)/2) as $y3
  | c($x3; $y1; $y2; $r; $d) as $cx1
  | c($y3; $x2; $x2; $r; $d) as $cy1
  | (- c(-$x3; $y1; $y2; $r; $d)) as $cx2
  | (- c(-$y3; $x2; $x2; $r; $d)) as $cy2
  | if   $d == 0 and $r == 0 then [$x1, $y1]  # special case
    elif $d == 0     then "infinitely many circles can be drawn"
    elif $d >  $r*2  then "points are too far from each other"
    elif  0 >  $r    then "radius is not valid"
    elif ($cx1 and $cy1 and $cx2 and $cy2) | not then "no solution"
    else  [$cx1, $cy1, $cx2, $cy2 ]
    end;
```

'''Examples''':

```jq
(
 [0.1234,    0.9876,    0.8765,    0.2345,    2],
 [0.0000,    2.0000,    0.0000,    0.0000,    1],
 [0.1234,    0.9876,    0.1234,    0.9876,    2],
 [0.1234,    0.9876,    0.8765,    0.2345,  0.5],
 [0.1234,    0.9876,    0.1234,    0.9876,    0] 
 )
 | "\(.) ───► \(circle_centers)"
```

 
{{out}}

```sh
$ jq -n -c -r -f /Users/peter/jq/circle_centers.jq

[0.1234,0.9876,0.8765,0.2345,2] ───► [1.8631118016581893,1.974211801658189,-0.8632118016581896,-0.7521118016581892]
[0,2,0,0,1] ───► [0,1,0,1]
[0.1234,0.9876,0.1234,0.9876,2] ───► infinitely many circles can be drawn
[0.1234,0.9876,0.8765,0.2345,0.5] ───► points are too far from each other
[0.1234,0.9876,0.1234,0.9876,0] ───► [0.1234,0.9876]
```



## Julia

This solution uses the package [https://github.com/timholy/AffineTransforms.jl AffineTransforms.jl] to introduce a coordinate system (u, v) centered on the midpoint between the two points and rotated so that these points are on the u-axis.  In this system, solving for the circles' centers is trivial.  The two points are cast as complex numbers to aid in determining the location of the midpoint and rotation angle.

'''Types and Functions'''

```Julia

immutable Point{T<:FloatingPoint}
    x::T
    y::T
end

immutable Circle{T<:FloatingPoint}
    c::Point{T}
    r::T
end
Circle{T<:FloatingPoint}(a::Point{T}) = Circle(a, zero(T))

using AffineTransforms

function circlepoints{T<:FloatingPoint}(a::Point{T}, b::Point{T}, r::T)
    cp = Circle{T}[]
    r >= 0 || return (cp, "No Solution, Negative Radius")
    if a == b
        if abs(r) < 2eps(zero(T))
            return (push!(cp, Circle(a)), "Point Solution, Zero Radius")
        else
            return (cp, "Infinite Solutions, Indefinite Center")
        end
    end
    ca = Complex(a.x, a.y)
    cb = Complex(b.x, b.y)
    d = (ca + cb)/2
    tfd = tformtranslate([real(d), imag(d)])
    tfr = tformrotate(angle(cb-ca))
    tfm = tfd*tfr
    u = abs(cb-ca)/2
    r-u > -5eps(r) || return(cp, "No Solution, Radius Too Small")
    if r-u < 5eps(r)
        push!(cp, Circle(apply(Point, tfm*[0.0, 0.0]), r))
        return return (cp, "Single Solution, Degenerate Centers")
    end
    v = sqrt(r^2 - u^2)
    for w in [v, -v]
        push!(cp, Circle(apply(Point, tfm*[0.0, w]), r))
    end
    return (cp, "Two Solutions")
end

```


'''Main'''

```Julia

tp = [Point(0.1234, 0.9876),
      Point(0.0000, 2.0000),
      Point(0.1234, 0.9876),
      Point(0.1234, 0.9876),
      Point(0.1234, 0.9876)]

tq = [Point(0.8765, 0.2345),
      Point(0.0000, 0.0000),
      Point(0.1234, 0.9876),
      Point(0.8765, 0.2345),
      Point(0.1234, 0.9876)]

tr = [2.0, 1.0, 2.0, 0.5, 0.0]

println("Testing circlepoints:")
for i in 1:length(tp)
    p = tp[i]
    q = tq[i]
    r = tr[i]
    (cp, rstatus) = circlepoints(p, q, r)
    println(@sprintf("(%.4f, %.4f), (%.4f, %.4f), %.4f => %s",
                     p.x, p.y, q.x, q.y, r, rstatus))
    for c in cp
        println(@sprintf("    (%.4f, %.4f), %.4f",
                         c.c.x, c.c.y, c.r))
    end
end

```


{{out}}

```txt

Testing circlepoints:
(0.1234, 0.9876), (0.8765, 0.2345), 2.0000 => Two Solutions
    (1.8631, 1.9742), 2.0000
    (-0.8632, -0.7521), 2.0000
(0.0000, 2.0000), (0.0000, 0.0000), 1.0000 => Single Solution, Degenerate Centers
    (0.0000, 1.0000), 1.0000
(0.1234, 0.9876), (0.1234, 0.9876), 2.0000 => Infinite Solutions, Indefinite Center
(0.1234, 0.9876), (0.8765, 0.2345), 0.5000 => No Solution, Radius Too Small
(0.1234, 0.9876), (0.1234, 0.9876), 0.0000 => Point Solution, Zero Radius
    (0.1234, 0.9876), 0.0000

```



## Kotlin


```scala
// version 1.1.51

typealias IAE = IllegalArgumentException

class Point(val x: Double, val y: Double) {
    fun distanceFrom(other: Point): Double {
        val dx = x - other.x
        val dy = y - other.y
        return Math.sqrt(dx * dx + dy * dy )
    }

    override fun equals(other: Any?): Boolean {
        if (other == null || other !is Point) return false
        return (x == other.x && y == other.y)
    }

    override fun toString() = "(%.4f, %.4f)".format(x, y)
}

fun findCircles(p1: Point, p2: Point, r: Double): Pair<Point, Point> {
    if (r < 0.0) throw IAE("the radius can't be negative")
    if (r == 0.0 && p1 != p2) throw IAE("no circles can ever be drawn")
    if (r == 0.0) return p1 to p1
    if (p1 == p2) throw IAE("an infinite number of circles can be drawn")
    val distance = p1.distanceFrom(p2)
    val diameter = 2.0 * r
    if (distance > diameter) throw IAE("the points are too far apart to draw a circle")
    val center = Point((p1.x + p2.x) / 2.0, (p1.y + p2.y) / 2.0)
    if (distance == diameter) return center to center
    val mirrorDistance = Math.sqrt(r * r - distance * distance / 4.0)
    val dx =  (p2.x - p1.x) * mirrorDistance / distance
    val dy =  (p2.y - p1.y) * mirrorDistance / distance
    return Point(center.x - dy, center.y + dx) to
           Point(center.x + dy, center.y - dx)
}

fun main(args: Array<String>) {
    val p = arrayOf(
        Point(0.1234, 0.9876),
        Point(0.8765, 0.2345),
        Point(0.0000, 2.0000),
        Point(0.0000, 0.0000)
    )
    val points = arrayOf(
        p[0] to p[1], p[2] to p[3], p[0] to p[0], p[0] to p[1], p[0] to p[0]
    )
    val radii = doubleArrayOf(2.0, 1.0, 2.0, 0.5, 0.0)
    for (i in 0..4) {
        try {
            val (p1, p2) = points[i]            
            val r  = radii[i]
            println("For points $p1 and $p2 with radius $r")
            val (c1, c2) = findCircles(p1, p2, r)
            if (c1 == c2)
                println("there is just one circle with center at $c1")
            else
                println("there are two circles with centers at $c1 and $c2")
        }
        catch(ex: IllegalArgumentException) {
            println(ex.message)
        }
        println()
    }
}
```


{{out}}

```txt

For points (0.1234, 0.9876) and (0.8765, 0.2345) with radius 2.0
there are two circles with centers at (1.8631, 1.9742) and (-0.8632, -0.7521)

For points (0.0000, 2.0000) and (0.0000, 0.0000) with radius 1.0
there is just one circle with center at (0.0000, 1.0000)

For points (0.1234, 0.9876) and (0.1234, 0.9876) with radius 2.0
an infinite number of circles can be drawn

For points (0.1234, 0.9876) and (0.8765, 0.2345) with radius 0.5
the points are too far apart to draw a circle

For points (0.1234, 0.9876) and (0.1234, 0.9876) with radius 0.0
there is just one circle with center at (0.1234, 0.9876)

```



## Liberty BASIC


```lb

'[RC] Circles of given radius through two points
for i = 1 to 5
    read x1, y1, x2, y2,r
    print i;") ";x1, y1, x2, y2,r
    call twoCircles x1, y1, x2, y2,r
next
end

'p1                p2           r
data 0.1234, 0.9876,    0.8765, 0.2345,    2.0
data 0.0000, 2.0000,    0.0000, 0.0000,    1.0
data 0.1234, 0.9876,    0.1234, 0.9876,    2.0
data 0.1234, 0.9876,    0.8765, 0.2345,    0.5
data 0.1234, 0.9876,    0.1234, 0.9876,    0.0

sub  twoCircles  x1, y1, x2, y2,r

    if x1=x2 and y1=y2 then '2.If the points are coincident
        if r=0 then ' unless r==0.0
            print "It will be a single point (";x1;",";y1;") of radius 0"
            exit sub
        else
            print "There are any number of circles via single point (";x1;",";y1;") of radius ";r
            exit sub
        end if
    end if
    r2 = sqr((x1-x2)^2+(y1-y2)^2)/2 'half distance between points
    if r<r2 then
        print "Points are too far apart (";2*r2;") - there are no circles of radius ";r
        exit sub
    end if

    'else, calculate two centers
    cx=(x1+x2)/2 'middle point
    cy=(y1+y2)/2
    'should move from middle point along perpendicular by dd2
    dd2=sqr(r^2-r2^2)   'perpendicular distance
    dx1=x2-cx   'vector to middle point
    dy1=y2-cy
    dx = 0-dy1/r2*dd2   'perpendicular:
    dy = dx1/r2*dd2     'rotate and scale
    print "(";cx+dy;",";cy+dx;")"   'two points, with (+)
    print "(";cx-dy;",";cy-dx;")"   'and (-)

end sub

```


Output:

```text

1) 0.1234     0.9876        0.8765        0.2345        2
(1.8631118,1.9742118)
(-0.8632118,-0.7521118)
2) 0          2             0             0             1
(0,1)
(0,1)
3) 0.1234     0.9876        0.1234        0.9876        2
There are any number of circles via single point (0.1234,0.9876) of radius 2
4) 0.1234     0.9876        0.8765        0.2345        0.5
Points are too far apart (1.06504423) - there are no circles of radius 0.5
5) 0.1234     0.9876        0.1234        0.9876        0
It will be a single point (0.1234,0.9876) of radius 0

```



## Lua

{{trans|C}}

```lua
function distance(p1, p2)
    local dx = (p1.x-p2.x)
    local dy = (p1.y-p2.y)
    return math.sqrt(dx*dx + dy*dy)
end

function findCircles(p1, p2, radius)
    local seperation = distance(p1, p2)
    if seperation == 0.0 then
        if radius == 0.0 then
            print("No circles can be drawn through ("..p1.x..", "..p1.y..")")
        else
            print("Infinitely many circles can be drawn through ("..p1.x..", "..p1.y..")")
        end
    elseif seperation == 2*radius then
        local cx = (p1.x+p2.x)/2
        local cy = (p1.y+p2.y)/2
        print("Given points are opposite ends of a diameter of the circle with center ("..cx..", "..cy..") and radius "..radius)
    elseif seperation > 2*radius then
        print("Given points are further away from each other than a diameter of a circle with radius "..radius)
    else
        local mirrorDistance = math.sqrt(math.pow(radius,2) - math.pow(seperation/2,2))
        local dx = p2.x - p1.x
        local dy = p1.y - p2.y
        local ax = (p1.x + p2.x) / 2
        local ay = (p1.y + p2.y) / 2
        local mx = mirrorDistance * dx / seperation
        local my = mirrorDistance * dy / seperation
        c1 = {x=ax+my, y=ay+mx}
        c2 = {x=ax-my, y=ay-mx}

        print("Two circles are possible.")
        print("Circle C1 with center ("..c1.x..", "..c1.y.."), radius "..radius)
        print("Circle C2 with center ("..c2.x..", "..c2.y.."), radius "..radius)
    end
    print()
end

cases = {
    {x=0.1234, y=0.9876},   {x=0.8765, y=0.2345},
    {x=0.0000, y=2.0000},   {x=0.0000, y=0.0000},
    {x=0.1234, y=0.9876},   {x=0.1234, y=0.9876},
    {x=0.1234, y=0.9876},   {x=0.8765, y=0.2345},
    {x=0.1234, y=0.9876},   {x=0.1234, y=0.9876}
}
radii = { 2.0, 1.0, 2.0, 0.5, 0.0 }
for i=1, #radii do
    print("Case "..i)
    findCircles(cases[i*2-1], cases[i*2], radii[i])
end
```

{{out}}

```txt
Case 1
Two circles are possible.
Circle C1 with center (1.8631118016582, 1.9742118016582), radius 2
Circle C2 with center (-0.86321180165819, -0.75211180165819), radius 2

Case 2
Given points are opposite ends of a diameter of the circle with center (0, 1) and radius 1

Case 3
Infinitely many circles can be drawn through (0.1234, 0.9876)

Case 4
Given points are further away from each other than a diameter of a circle with radius 0.5

Case 5
No circles can be drawn through (0.1234, 0.9876)
```



## Maple


```maple
drawCircles := proc(x1, y1, x2, y2, r, $)
	local c1, c2, p1, p2;
	use geometry in
		if x1 = x2 and y1 = y2 then
			if r = 0 then
				printf("The circle is a point at [%a, %a].\n", x1, y1);
			else
				printf("The two points are the same. Infinite circles can be drawn.\n");
			end if;
		elif evalf(distance(point(A, x1, y1), point(B, x2, y2))) >r*2 then
				printf("The two points are too far apart. No circles can be drawn.\n");
		else
			circle(P1Cir, [A, r]);#make a circle around the first point
			circle(P2Cir, [B, r]);#make a circle around the second point
			intersection('i', P1Cir, P2Cir);
			#the intersection of the above 2 circles should give you the centers of the two circles you need to draw
			c1 := plottools[circle](coordinates(`if`(type(i, list), i[1], i)), r);#make the first circle 
			c2 := plottools[circle](coordinates(`if`(type(i, list), i[2], i)), r);#make the second circle
			plots[display](c1, c2, scaling = constrained);#draw
		end if;
	end use;
end proc:

drawCircles(0.1234, 0.9876, 0.8765, 0.2345, 2.0);
drawCircles(0.0000, 2.0000, 0.0000, 0.0000, 1.0);
drawCircles(0.1234, 0.9876, 0.1234, 0.9876, 2.0);
drawCircles(0.1234, 0.9876, 0.8765, 0.2345, 0.5);
drawCircles(0.1234, 0.9876, 0.1234, 0.9876, 0.0);
```

{{out}}
[[File:Circles1_Maple.png]]

[[File:Circles2_Maple.png]]

```txt

The two points are the same. Infinite circles can be drawn.
The two points are too far apart. No circles can be drawn.
The circle is a point at [.1234, .9876].

```



## Mathematica


```Mathematica
Off[Solve::ratnz];
circs::invrad = "The radius is invalid.";
circs::equpts = "The given points (`1`, `2`) are equivalent.";
circs::dist = 
  "The given points (`1`, `2`) and (`3`, `4`) are too far apart for \
radius `5`.";
circs[_, _, 0.] := Message[circs::invrad];
circs[{p1x_, p1y_}, {p1x_, p1y_}, _] := 
  Message[circs::equpts, p1x, p1y];
circs[p1 : {p1x_, p1y_}, p2 : {p2x_, p2y_}, r_] /; 
  EuclideanDistance[p1, p2] > 2 r := 
 Message[circs::dist, p1x, p1y, p2x, p2y, r]; 
circs[p1 : {p1x_, p1y_}, p2 : {p2x_, p2y_}, r_] := 
 Values /@ 
  Solve[Abs[x - p1x]^2 + Abs[y - p1y]^2 == 
    Abs[x - p2x]^2 + Abs[y - p2y]^2 == r^2, {x, y}];
```

{{out}}

```txt
In[2]:= circs[{.1234, .9876}, {.8765, .2345}, 2.]

Out[2]= {{-0.863212, -0.752112}, {1.86311, 1.97421}}

In[3]:= circs[{.1234, .9876}, {.1234, .9876}, 2.]

circs::equpts: The given points (0.1234`, 0.9876`) are equivalent.

In[4]:= circs[{.1234, .9876}, {.8765, .2345}, .5]

circs::dist: The given points (0.1234`, 0.9876`) and (0.8765`, 0.2345`) are too
    far apart for radius 0.5`.

In[5]:= circs[{.1234, .9876}, {.1234, .9876}, 0.]

circs::invrad: The radius is invalid.
```



## Maxima


```Maxima
/* define helper function */
vabs(a):= sqrt(a.a);
realp(e):=freeof(%i, e);

/* get a general solution */
sol: block(
  [p1: [x1, y1], p2: [x2, y2], c:  [x0, y0], eq],
  local(r),
  eq: [vabs(p1-c) = r, vabs(p2-c) = r],
  load(to_poly_solve),
  assume(r>0),
  args(to_poly_solve(eq, c, use_grobner = true)))$

/* use general solution for concrete case */
getsol(sol, x1, y1, x2, y2, r):=block([n, lsol],
  if [x1, y1]=[x2, y2] then (
    print("infinity many solutions"),
    return('infmany)),
  lsol: sublist(''sol, 'realp),
  n: length(lsol),
  if n=0 then (
    print("no solutions"),
    [])
  else if n=1 then (
    print("single solution"),
    lsol[1])
  else if [assoc('x0, lsol[1]), assoc('y0, lsol[1])]=[assoc('x0, lsol[2]), assoc('y0, lsol[2])] then (
    print("single solution"),
    lsol[1])
  else (
    print("two solutions"),
    lsol))$

/* [x1, y1, x2, y2, r] */
d[1]: [0.1234, 0.9876,    0.8765, 0.2345,    2];
d[2]: [0.0000, 2.0000,    0.0000, 0.0000,    1];
d[3]: [0,      0,         0,      1,         0.4];
d[4]: [0,      0,         0,      0,         0.4];

apply('getsol, cons(sol, d[1]));
apply('getsol, cons(sol, d[2]));
apply('getsol, cons(sol, d[3]));
apply('getsol, cons(sol, d[4]));
```

{{out}}
<lang>apply('getsol, cons(sol, d[1]));
two solutions 
(%o9) [[x0 = 1.86311180165819, y0 = 1.974211801658189], 
                            [x0 = - 0.86321180165819, y0 = - 0.75211180165819]]
(%i10) apply('getsol, cons(sol, d[2]));
single solution 
(%o10)                       [x0 = 0.0, y0 = 1.0]
(%i11) apply('getsol, cons(sol, d[3]));
no solutions 
(%o11)                                []
(%i12) apply('getsol, cons(sol, d[4]));
infinity many solutions 
(%o12)                              infmany
```


=={{header|МК-61/52}}==
<lang>П0	С/П	П1	С/П	П2	С/П	П3	С/П	П4
ИП2	ИП0	-	x^2	ИП3	ИП1	-	x^2	+	КвКор	П5
ИП0	ИП2	+	2	/	П6	ИП1	ИП3	+	2	/	П7
ИП4	x^2	ИП5	2	/	x^2	-	КвКор	ИП5	/	П8
ИП6	ИП1	ИП3	-	ИП8	*	П9	+	ПA
ИП6	ИП9	-	ПC
ИП7	ИП2	ИП0	-	ИП8	*	П9	+	ПB
ИП7	ИП9	-	ПD
ИП5	x#0	97	8	4	ИНВ	С/П
ИП4	2	*	ИП5	-	ПE	x#0	97	ИПB	ИПA	8	5	ИНВ	С/П
ИПE	x>=0	97	8	3	ИНВ	С/П
ИПD	ИПC	ИПB	ИПA	С/П
```


{{in}}

```txt

 В/О x1 С/П y1 С/П x2 С/П y2 С/П radius С/П

```

{{out}}

```txt

"8.L" if the points are coincident; "8.-" if the points are opposite ends of a diameter of the circle, РY and РZ are coordinates of the center; "8.Г" if the points are farther away from each other than a diameter of a circle; else РX, РY and РZ, РT are coordinates of the circles centers.

```


=={{header|Modula-2}}==

```modula2
MODULE Circles;
FROM EXCEPTIONS IMPORT AllocateSource,ExceptionSource,GetMessage,RAISE;
FROM FormatString IMPORT FormatString;
FROM LongMath IMPORT sqrt;
FROM LongStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

VAR
    TextWinExSrc : ExceptionSource;

TYPE
    Point = RECORD
        x,y : LONGREAL;
    END;
    Pair = RECORD
        a,b : Point;
    END;

PROCEDURE Distance(p1,p2 : Point) : LONGREAL;
VAR dx,dy : LONGREAL;
BEGIN
    dx := p1.x - p2.x;
    dy := p1.y - p2.y;
    RETURN sqrt(dx*dx + dy*dy)
END Distance;

PROCEDURE Equal(p1,p2 : Point) : BOOLEAN;
BEGIN
    RETURN (p1.x=p2.x) AND (p1.y=p2.y)
END Equal;

PROCEDURE WritePoint(p : Point);
VAR buf : ARRAY[0..63] OF CHAR;
BEGIN
    WriteString("(");
    RealToStr(p.x, buf);
    WriteString(buf);
    WriteString(", ");
    RealToStr(p.y, buf);
    WriteString(buf);
    WriteString(")");
END WritePoint;

PROCEDURE FindCircles(p1,p2 : Point; r : LONGREAL) : Pair;
VAR
    distance,diameter,mirrorDistance,dx,dy : LONGREAL;
    center : Point;
BEGIN
    IF r < 0.0 THEN RAISE(TextWinExSrc, 0, "the radius can't be negative") END;
    IF (r = 0.0) AND NOT Equal(p1,p2) THEN RAISE(TextWinExSrc, 0, "No circles can ever be drawn") END;
    IF r = 0.0 THEN RETURN Pair{p1,p1} END;
    IF Equal(p1,p2) THEN RAISE(TextWinExSrc, 0, "an infinite number of circles can be drawn") END;
    distance := Distance(p1,p2);
    diameter := 2.0 * r;
    IF distance > diameter THEN RAISE(TextWinExSrc, 0, "the points are too far apart to draw a circle") END;
    center := Point{(p1.x + p2.x) / 2.0, (p1.y + p2.y) / 2.0};
    IF distance = diameter THEN RETURN Pair{center, center} END;
    mirrorDistance := sqrt(r * r - distance * distance / 4.0);
    dx := (p2.x - p1.x) * mirrorDistance / distance;
    dy := (p2.y - p1.y) * mirrorDistance / distance;
    RETURN Pair{
        {center.x - dy, center.y + dx},
        {center.x + dy, center.y - dx}
    }
END FindCircles;

PROCEDURE Print(p1,p2 : Point; r : LONGREAL) : BOOLEAN;
VAR
    buf : ARRAY[0..63] OF CHAR;
    result : Pair;
BEGIN
    WriteString("For points ");
    WritePoint(p1);
    WriteString(" and ");
    WritePoint(p2);
    WriteString(" with radius ");
    RealToStr(r, buf);
    WriteString(buf);
    WriteLn;

    result := FindCircles(p1,p2,r);
    IF Equal(result.a, result.b) THEN
        WriteString("there is just one circle with the center at ");
        WritePoint(result.a);
        WriteLn;
    ELSE
        WriteString("there are two circles with centers at ");
        WritePoint(result.a);
        WriteString(" and ");
        WritePoint(result.b);
        WriteLn;
    END;
    WriteLn;
    RETURN TRUE
EXCEPT
    GetMessage(buf);
    WriteString(buf);
    WriteLn;
    WriteLn;
    RETURN FALSE
END Print;

VAR p0,p1,p2,p3 : Point;
BEGIN
    AllocateSource(TextWinExSrc);
    p0 := Point{0.1234,0.9876};
    p1 := Point{0.8765,0.2345};
    p2 := Point{0.0000,2.0000};
    p3 := Point{0.0000,0.0000};

    Print(p0,p1,2.0);
    Print(p2,p3,1.0);
    Print(p0,p0,2.0);
    Print(p0,p1,0.5);
    Print(p0,p0,0.0);

    ReadChar
END Circles.
```



## Nim

{{trans|Python}}

```nim
import math

type
  Point = tuple[x, y: float]
  Circle = tuple[x, y, r: float]

proc circles(p1, p2: Point, r: float): tuple[c1, c2: Circle] =
  if r == 0: raise newException(ValueError,
    "radius of zero")
  if p1 == p2: raise newException(ValueError,
    "coincident points gives infinite number of Circles")

  # delta x, delta y between points
  let (dx, dy) = (p2.x - p1.x, p2.y - p1.y)
  # dist between points
  let q = sqrt(dx*dx + dy*dy)
  if q > 2.0*r: raise newException(ValueError,
    "separation of points > diameter")

  # halfway point
  let p3: Point = ((p1.x+p2.x)/2, (p1.y+p2.y)/2)
  # distance along the mirror line
  let d = sqrt(r*r - (q/2)*(q/2))
  # One answer
  result.c1 = (p3.x - d*dy/q, p3.y + d*dx/q, abs(r))
  # The other answer
  result.c2 = (p3.x + d*dy/q, p3.y - d*dx/q, abs(r))

const tries: seq[tuple[p1, p2: Point, r: float]] =
  @[((0.1234, 0.9876), (0.8765, 0.2345), 2.0),
    ((0.0000, 2.0000), (0.0000, 0.0000), 1.0),
    ((0.1234, 0.9876), (0.1234, 0.9876), 2.0),
    ((0.1234, 0.9876), (0.8765, 0.2345), 0.5),
    ((0.1234, 0.9876), (0.1234, 0.9876), 0.0)]

for p1, p2, r in tries.items:
  echo "Through points:"
  echo "  ", p1
  echo "  ", p2
  echo "  and radius ", r
  echo "You can construct the following circles:"
  try:
    let (c1, c2) = circles(p1, p2, r)
    echo "  ", c1
    echo "  ", c2
  except ValueError:
    echo "  ERROR: ", getCurrentExceptionMsg()
  echo ""
```

{{out}}

```txt
Through points:
  (x: 0.1234, y: 0.9876)
  (x: 0.8764999999999999, y: 0.2345)
  and radius 2.0
You can construct the following circles:
  (x: 1.863111801658189, y: 1.974211801658189, r: 2.0)
  (x: -0.8632118016581896, y: -0.7521118016581892, r: 2.0)

Through points:
  (x: 0.0, y: 2.0)
  (x: 0.0, y: 0.0)
  and radius 1.0
You can construct the following circles:
  (x: 0.0, y: 1.0, r: 1.0)
  (x: 0.0, y: 1.0, r: 1.0)

Through points:
  (x: 0.1234, y: 0.9876)
  (x: 0.1234, y: 0.9876)
  and radius 2.0
You can construct the following circles:
  ERROR: coincident points gives infinite number of Circles

Through points:
  (x: 0.1234, y: 0.9876)
  (x: 0.8764999999999999, y: 0.2345)
  and radius 0.5
You can construct the following circles:
  ERROR: separation of points > diameter

Through points:
  (x: 0.1234, y: 0.9876)
  (x: 0.1234, y: 0.9876)
  and radius 0.0
You can construct the following circles:
  ERROR: radius of zero
```



## Oforth



```oforth
: circleCenter(x1, y1, x2, y2, r)
| d xmid ymid r1 md | 
   x2 x1 - sq  y2 y1 - sq + sqrt -> d
   x1 x2 + 2 / -> xmid
   y1 y2 + 2 / -> ymid   
   2 r * -> r1

   d 0.0 == ifTrue: [ "Infinite number of circles" . return ]
   d r1 == ifTrue:  [ System.Out "One circle: (" << xmid << ", " << ymid << ")" << cr return ]
   d r1  > ifTrue:  [ "No circle" . return ]

   r sq d 2 / sq - sqrt ->md    

   System.Out "C1 : (" << xmid y1 y2 - md * d / + << ", " << ymid x2 x1 - md * d / + << ")" << cr 
   System.Out "C2 : (" << xmid y1 y2 - md * d / - << ", " << ymid x2 x1 - md * d / - << ")" << cr 
;
```


{{out}}

```txt


>0.1234 0.9876 0.8765 0.2345 2 circleCenter
C1 : (1.86311180165819, 1.97421180165819)
C2 : (-0.86321180165819, -0.752111801658189)
ok

>0 2 0 0 1 circleCenter
One cirlce: (0, 1)
ok

>0.1234 0.9876 0.8765 0.2345 0.5 circleCenter
No circle ok

>0.1234 0.9876 0.1234 0.9876 0 circleCenter
Infinite number of circles ok


```



## ooRexx

{{trans|REXX}}

```oorexx
/*REXX pgm finds 2 circles with a specific radius given two (X,Y) points*/
  a.=''
  a.1=0.1234 0.9876 0.8765 0.2345 2
  a.2=0.0000 2.0000 0.0000 0.0000 1
  a.3=0.1234 0.9876 0.1234 0.9876 2
  a.4=0.1234 0.9876 0.8765 0.2345 0.5
  a.5=0.1234 0.9876 0.1234 0.9876 0

  Say '     x1      y1      x2      y2  radius   cir1x   cir1y   cir2x   cir2y'
  Say ' ------  ------  ------  ------  ------  ------  ------  ------  ------'
  Do j=1 By 1 While a.j<>''
    Do k=1 For 4
      w.k=f(word(a.j,k))
      End
    Say w.1 w.2 w.3 w.4 format(word(a.j,5),5,1)  twocircles(a.j)
    End
  Exit

twocircles: Procedure
  Parse Arg px py qx qy r .
  If r=0 Then
    Return ' radius of zero gives no circles.'
  x=(qx-px)/2
  y=(qy-py)/2
  bx=px+x
  by=py+y
  pb=rxCalcsqrt(x**2+y**2)
  If pb=0 Then
    Return ' coincident points give infinite circles'
  If pb>r Then
    Return ' points are too far apart for the given radius'
  cb=rxCalcsqrt(r**2-pb**2)
  x1=y*cb/pb
  y1=x*cb/pb
  Return f(bx-x1) f(by+y1) f(bx+x1) f(by-y1)

f: Return format(arg(1),2,4) /* format a number with 4 dec dig.*/

::requires 'rxMath' library
```

{{out}}

```txt
     x1      y1      x2      y2  radius   cir1x   cir1y   cir2x   cir2y
 ------  ------  ------  ------  ------  ------  ------  ------  ------
 0.1234  0.9876  0.8765  0.2345     2.0  1.8631  1.9742 -0.8632 -0.7521
 0.0000  2.0000  0.0000  0.0000     1.0  0.0000  1.0000  0.0000  1.0000
 0.1234  0.9876  0.1234  0.9876     2.0  coincident points give infinite circles
 0.1234  0.9876  0.8765  0.2345     0.5  points are too far apart for the given radius
 0.1234  0.9876  0.1234  0.9876     0.0  radius of zero gives no circles.
```



## PARI/GP


```parigp
circ(a, b, r)={
  if(a==b, return("impossible"));
  my(h=(b-a)/2,t=sqrt(r^2-abs(h)^2)/abs(h)*h);
  [a+h+t*I,a+h-t*I]
};
circ(0.1234 + 0.9876*I, 0.8765 + 0.2345*I, 2)
circ(0.0000 + 2.0000*I, 0.0000 + 0.0000*I, 1)
circ(0.1234 + 0.9876*I, 0.1234 + 0.9876*I, 2)
circ(0.1234 + 0.9876*I, 0.8765 + 0.2345*I, .5)
circ(0.1234 + 0.9876*I, 0.1234 + 0.9876*I, 0)
```

{{out}}

```txt
%1 = [1.86311180 + 1.97421180*I, -0.863211802 - 0.752111802*I]
%2 = [0.E-9 + 1.00000000*I, 0.E-9 + 1.00000000*I]
%3 = "impossible"
%4 = [0.370374144 + 0.740625856*I, 0.629525856 + 0.481474144*I]
%5 = "impossible"
```




## Perl

{{trans|Python}}

```perl
use strict;

sub circles {
    my ($x1, $y1, $x2, $y2, $r) = @_;

    return "Radius is zero" if $r == 0;
    return "Coincident points gives infinite number of circles" if $x1 == $x2 and $y1 == $y2;

    # delta x, delta y between points
    my ($dx, $dy) = ($x2 - $x1, $y2 - $y1);
    my $q = sqrt($dx**2 + $dy**2);
    return "Separation of points greater than diameter" if $q > 2*$r;

    # halfway point
    my ($x3, $y3) = (($x1 + $x2) / 2, ($y1 + $y2) / 2);
    # distance along the mirror line
    my $d = sqrt($r**2-($q/2)**2);

    # pair of solutions
    sprintf '(%.4f, %.4f) and (%.4f, %.4f)',
        $x3 - $d*$dy/$q, $y3 + $d*$dx/$q,
        $x3 + $d*$dy/$q, $y3 - $d*$dx/$q;
}

my @arr = (
    [0.1234, 0.9876, 0.8765, 0.2345, 2.0],
    [0.0000, 2.0000, 0.0000, 0.0000, 1.0],
    [0.1234, 0.9876, 0.1234, 0.9876, 2.0],
    [0.1234, 0.9876, 0.8765, 0.2345, 0.5],
    [0.1234, 0.9876, 0.1234, 0.9876, 0.0]
);

printf "(%.4f, %.4f) and (%.4f, %.4f) with radius %.1f: %s\n", @$_[0..4], circles @$_ for @arr;
```

{{out}}

```txt
(0.1234, 0.9876) and (0.8765, 0.2345) with radius 2.0: (1.8631, 1.9742) and (-0.8632, -0.7521)
(0.0000, 2.0000) and (0.0000, 0.0000) with radius 1.0: (0.0000, 1.0000) and (0.0000, 1.0000)
(0.1234, 0.9876) and (0.1234, 0.9876) with radius 2.0: Coincident points gives infinite number of circles
(0.1234, 0.9876) and (0.8765, 0.2345) with radius 0.5: Separation of points greater than diameter
(0.1234, 0.9876) and (0.1234, 0.9876) with radius 0.0: Radius is zero
```



## Perl 6


```Perl6
multi sub circles (@A, @B where ([and] @A Z== @B), 0.0) { 'Degenerate point' }
multi sub circles (@A, @B where ([and] @A Z== @B), $)   { 'Infinitely many share a point' }
multi sub circles (@A, @B, $radius) {
    my @middle = (@A Z+ @B) X/ 2;
    my @diff = @A Z- @B;
    my $q = sqrt [+] @diff X** 2;
    return 'Too far apart' if $q > $radius * 2;

    my @orth = -@diff[0], @diff[1] X* sqrt($radius ** 2 - ($q / 2) ** 2) / $q;
    return (@middle Z+ @orth), (@middle Z- @orth);
}

my @input =
    ([0.1234, 0.9876],  [0.8765, 0.2345],   2.0),
    ([0.0000, 2.0000],  [0.0000, 0.0000],   1.0),
    ([0.1234, 0.9876],  [0.1234, 0.9876],   2.0),
    ([0.1234, 0.9876],  [0.8765, 0.2345],   0.5),
    ([0.1234, 0.9876],  [0.1234, 0.9876],   0.0),
    ;

for @input {
    say .list.perl, ': ', circles(|$_).join(' and ');
}
```

{{out}}

```txt
([0.1234, 0.9876], [0.8765, 0.2345], 2.0): 1.86311180165819 1.97421180165819 and -0.863211801658189 -0.752111801658189
([0.0, 2.0], [0.0, 0.0], 1.0): 0 1 and 0 1
([0.1234, 0.9876], [0.1234, 0.9876], 2.0): Infinitely many share a point
([0.1234, 0.9876], [0.8765, 0.2345], 0.5): Too far apart
([0.1234, 0.9876], [0.1234, 0.9876], 0.0): Degenerate point
```


Another possibility is to use the Complex plane, 
for it often makes calculations easier with plane geometry:


```perl6
multi sub circles ($a, $b where $a == $b, 0.0) { 'Degenerate point' }
multi sub circles ($a, $b where $a == $b, $)   { 'Infinitely many share a point' }
multi sub circles ($a, $b, $r) {
    my $h = ($b - $a) / 2;
    my $l = sqrt($r**2 - $h.abs**2);
    return 'Too far apart' if $l.isNaN;
    return map { $a + $h + $l * $_ * $h / $h.abs }, i, -i;
}

my @input =
    (0.1234 + 0.9876i,  0.8765 + 0.2345i,   2.0),
    (0.0000 + 2.0000i,  0.0000 + 0.0000i,   1.0),
    (0.1234 + 0.9876i,  0.1234 + 0.9876i,   2.0),
    (0.1234 + 0.9876i,  0.8765 + 0.2345i,   0.5),
    (0.1234 + 0.9876i,  0.1234 + 0.9876i,   0.0),
    ;

for @input {
    say .join(', '), ': ', circles(|$_).join(' and ');
}
```


{{out}}

```txt
0.1234+0.9876i, 0.8765+0.2345i, 2: 1.86311180165819+1.97421180165819i and -0.863211801658189-0.752111801658189i
0+2i, 0+0i, 1: 0+1i and 0+1i
0.1234+0.9876i, 0.1234+0.9876i, 2: Infinitely many share a point
0.1234+0.9876i, 0.8765+0.2345i, 0.5: Too far apart
0.1234+0.9876i, 0.1234+0.9876i, 0: Degenerate point
```



## Phix


```Phix
constant tests = {{0.1234, 0.9876, 0.8765, 0.2345, 2.0},
                  {0.0000, 2.0000, 0.0000, 0.0000, 1.0},
                  {0.1234, 0.9876, 0.1234, 0.9876, 2.0},
                  {0.1234, 0.9876, 0.8765, 0.2345, 0.5},
                  {0.1234, 0.9876, 0.1234, 0.9876, 0.0}}
for i=1 to length(tests) do
    atom {x1,y1,x2,y2,r} = tests[i],
         xd = x2-x1, yd = y1-y2,
         s2 = xd*xd+yd*yd,
         sep = sqrt(s2),
         xh = (x1+x2)/2,
         yh = (y1+y2)/2
    string txt
    if sep=0 then
        txt = "same points/"&iff(r=0?"radius is zero":"infinite solutions")
    elsif sep=2*r then
        txt = sprintf("opposite ends of diameter with centre {%.4f,%.4f}",{xh,yh})
    elsif sep>2*r then
        txt = sprintf("too far apart (%.4f > %.4f)",{sep,2*r})
    else
        atom md = sqrt(r*r-s2/4),
             xs = md*xd/sep,
             ys = md*yd/sep
        txt = sprintf("{%.4f,%.4f} and {%.4f,%.4f}",{xh+ys,yh+xs,xh-ys,yh-xs})
    end if
    printf(1,"points {%.4f,%.4f}, {%.4f,%.4f} with radius %.1f ==> %s\n",{x1,y1,x2,y2,r,txt})
end for
```

{{out}}

```txt

points {0.1234,0.9876}, {0.8765,0.2345} with radius 2.0 ==> {1.8631,1.9742} and {-0.8632,-0.7521}
points {0.0000,2.0000}, {0.0000,0.0000} with radius 1.0 ==> opposite ends of diameter with centre {0.0000,1.0000}
points {0.1234,0.9876}, {0.1234,0.9876} with radius 2.0 ==> same points/infinite solutions
points {0.1234,0.9876}, {0.8765,0.2345} with radius 0.5 ==> too far apart (1.0650 > 1.0000)
points {0.1234,0.9876}, {0.1234,0.9876} with radius 0.0 ==> same points/radius is zero

```



## PL/I

{{trans|REXX}}
 
```PL/I
twoci: Proc Options(main);
 Dcl 1 *(5),
      2 m1x Dec Float Init(0.1234,     0,0.1234,0.1234,0.1234),
      2 m1y Dec Float Init(0.9876,     2,0.9876,0.9876,0.9876),
      2 m2x Dec Float Init(0.8765,     0,0.1234,0.8765,0.1234),
      2 m2y Dec Float Init(0.2345,     0,0.9876,0.2345,0.9876),
      2 r   Dec Float Init(     2,     1,     2,0.5   ,     0);
 Dcl i Bin Fixed(31);
 Put Edit('     x1     y1     x2     y2  r '||
          '  cir1x   cir1y   cir2x   cir2y')(Skip,a);
 Put Edit(' 
### === ====== ====== ===
  = '||
          ' 
### ===  ======  ======  ===
')(Skip,a);
 Do i=1 To 5;
   Put Edit(m1x(i),m1y(i),m2x(i),m2y(i),r(i))
           (Skip,4(f(7,4)),f(3));
   Put Edit(twocircles(m1x(i),m1y(i),m2x(i),m2y(i),r(i)))(a);
   End;

 twoCircles: proc(m1x,m1y,m2x,m2y,r) Returns(Char(50) Var);
 Dcl (m1x,m1y,m2x,m2y,r) Dec Float;
 Dcl (cx,cy,bx,by,pb,x,y,x1,y1) Dec Float;
 Dcl res Char(50) Var;
 If r=0 then return(' radius of zero gives no circles.');
 x=(m2x-m1x)/2;
 y=(m2y-m1y)/2;
 bx=m1x+x;
 by=m1y+y;
 pb=sqrt(x**2+y**2);
 cx=(m2x-m1x)/2;
 cy=(m2y-m1y)/2;
 bx=m1x+x;
 by=m1y+y;
 pb=sqrt(x**2+y**2)
 if pb=0 then return(' coincident points give infinite circles');
 if pb>r then return(' points are too far apart for the given radius');
 cb=sqrt(r**2-pb**2);
 x1=y*cb/pb;
 y1=x*cb/pb
 Put String(res) Edit((bx-x1),(by+y1),(bx+x1),(by-y1))(4(f(8,4)));
 Return(res);
 End;
 End;
```
    
{{out}}

```txt
     x1     y1     x2     y2  r   cir1x   cir1y   cir2x   cir2y
 
### === ====== ====== ======  =  ======  ======  ======  ===

 0.1234 0.9876 0.8765 0.2345  2  1.8631  1.9742 -0.8632 -0.7521
 0.0000 2.0000 0.0000 0.0000  1  0.0000  1.0000  0.0000  1.0000
 0.1234 0.9876 0.1234 0.9876  2 coincident points give infinite circles
 0.1234 0.9876 0.8765 0.2345  1 points are too far apart for the given radius
 0.1234 0.9876 0.1234 0.9876  0 radius of zero gives no circles.                     

```



## Python

The function raises the ValueError exception for the special cases 
and uses try - except to catch these and extract the exception detail.


```python
from collections import namedtuple
from math import sqrt

Pt = namedtuple('Pt', 'x, y')
Circle = Cir = namedtuple('Circle', 'x, y, r')

def circles_from_p1p2r(p1, p2, r):
    'Following explanation at http://mathforum.org/library/drmath/view/53027.html'
    if r == 0.0:
        raise ValueError('radius of zero')
    (x1, y1), (x2, y2) = p1, p2
    if p1 == p2:
        raise ValueError('coincident points gives infinite number of Circles')
    # delta x, delta y between points
    dx, dy = x2 - x1, y2 - y1
    # dist between points
    q = sqrt(dx**2 + dy**2)
    if q > 2.0*r:
        raise ValueError('separation of points > diameter')
    # halfway point
    x3, y3 = (x1+x2)/2, (y1+y2)/2
    # distance along the mirror line
    d = sqrt(r**2-(q/2)**2)
    # One answer
    c1 = Cir(x = x3 - d*dy/q,
             y = y3 + d*dx/q,
             r = abs(r))
    # The other answer
    c2 = Cir(x = x3 + d*dy/q,
             y = y3 - d*dx/q,
             r = abs(r))
    return c1, c2

if __name__ == '__main__':
    for p1, p2, r in [(Pt(0.1234, 0.9876), Pt(0.8765, 0.2345), 2.0),
                      (Pt(0.0000, 2.0000), Pt(0.0000, 0.0000), 1.0),
                      (Pt(0.1234, 0.9876), Pt(0.1234, 0.9876), 2.0),
                      (Pt(0.1234, 0.9876), Pt(0.8765, 0.2345), 0.5),
                      (Pt(0.1234, 0.9876), Pt(0.1234, 0.9876), 0.0)]:
        print('Through points:\n  %r,\n  %r\n  and radius %f\nYou can construct the following circles:'
              % (p1, p2, r))
        try:
            print('  %r\n  %r\n' % circles_from_p1p2r(p1, p2, r))
        except ValueError as v:
            print('  ERROR: %s\n' % (v.args[0],))
```


{{out}}

```txt
Through points:
  Pt(x=0.1234, y=0.9876),
  Pt(x=0.8765, y=0.2345)
  and radius 2.000000
You can construct the following circles:
  Circle(x=1.8631118016581893, y=1.974211801658189, r=2.0)
  Circle(x=-0.8632118016581896, y=-0.7521118016581892, r=2.0)

Through points:
  Pt(x=0.0, y=2.0),
  Pt(x=0.0, y=0.0)
  and radius 1.000000
You can construct the following circles:
  Circle(x=0.0, y=1.0, r=1.0)
  Circle(x=0.0, y=1.0, r=1.0)

Through points:
  Pt(x=0.1234, y=0.9876),
  Pt(x=0.1234, y=0.9876)
  and radius 2.000000
You can construct the following circles:
  ERROR: coincident points gives infinite number of Circles

Through points:
  Pt(x=0.1234, y=0.9876),
  Pt(x=0.8765, y=0.2345)
  and radius 0.500000
You can construct the following circles:
  ERROR: separation of points > diameter

Through points:
  Pt(x=0.1234, y=0.9876),
  Pt(x=0.1234, y=0.9876)
  and radius 0.000000
You can construct the following circles:
  ERROR: radius of zero
```




## Racket

Using library `plot/utils` for simple vector operations.


```racket

#lang racket
(require plot/utils)

(define (circle-centers p1 p2 r)
  (when (zero? r) (err "zero radius."))
  (when (equal? p1 p2) (err "the points coinside."))
  ; the midle point
  (define m (v/ (v+ p1 p2) 2))
  ; the vector connecting given points
  (define d (v/ (v- p1 p2) 2))
  ; the distance between the center of the circle and the middle point
  (define ξ (- (sqr r) (vmag^2 d)))
  (when (negative? ξ) (err "given radius is less then the distance between points."))
  ; the unit vector orthogonal to the delta
  (define n (vnormalize (orth d)))
  ; the shift along the direction orthogonal to the delta
  (define x (v* n (sqrt ξ)))
  (values (v+ m x) (v- m x)))

;; error message
(define (err m) (error "Impossible to build a circle:" m))

;; returns a vector which is orthogonal to the geven one
(define orth (match-lambda [(vector x y) (vector y (- x))]))

```


{{out|Testing}}

```txt

> (circle-centers #(0.1234 0.9876) #(0.8765 0.2345) 2.0)
'#(1.8631118016581893 1.974211801658189)
'#(-0.8632118016581896 -0.7521118016581892)

> (circle-centers #(0.0000 2.0000) #(0.0000 0.0000) 1.0)
'#(0.0 1.0)
'#(0.0 1.0)

> (circle-centers #(0.1234 0.9876) #(0.1234 0.9876) 2.0)
. . Impossible to find a circle: "the points coinside."

> (circle-centers #(0.1234 0.9876) #(0.8765 0.2345) 0.5)
. . Impossible to find a circle: "given radius is less then the distance between points."

> (circle-centers #(0.1234 0.9876) #(0.1234 0.9876) 0.0)
. . Impossible to find a circle: "zero radius."

```


Drawing circles:


```racket

(require 2htdp/image)

(define/match (point v) 
  [{(vector x y)} (λ (s) (place-image (circle 2 "solid" "black") x y s))])

(define/match (circ v r)
  [{(vector x y) r} (λ (s) (place-image (circle r "outline" "red") x y s))])

(define p1 #(40 50))
(define p2 #(60 30))
(define r 20)
(define-values (x1 x2) (circle-centers p1 p2 r))

((compose (point p1) (point p2) (circ x1 r) (circ x2 r))
 (empty-scene 100 100))

```



## REXX

{{trans|XPL0}}

The REXX language doesn't have a   '''sqrt'''   function,   so one is included below.

```rexx
/*REXX pgm finds two circles with a specific radius given two  (X1,Y1) & (X2,Y2)  points*/
@.=; @.1= 0.1234   0.9876    0.8765    0.2345    2
     @.2= 0        2         0         0         1
     @.3= 0.1234   0.9876    0.1234    0.9876    2
     @.4= 0.1234   0.9876    0.8765    0.2345    0.5
     @.5= 0.1234   0.9876    0.1234    0.9876    0
say '     x1        y1        x2        y2     radius          circle1x  circle1y  circle2x  circle2y'
say '  ════════  ════════  ════════  ════════  ══════          ════════  ════════  ════════  ════════'
       do  j=1  while  @.j\=='';  parse var @.j  p1 p2 p3 p4 r           /*points, radii*/
       say f(p1)  f(p2)  f(p3)  f(p4)       center(r/1, 9)      "───► "        2circ(@.j)
       end      /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
2circ: procedure; parse arg px py qx qy r .;  x=(qx-px)/2;   y=(qy-py)/2
                                             bx=px + x;     by=py + y;  pb=sqrt(x**2+y**2)
       if r = 0  then return  'radius of zero yields no circles.'
       if pb==0  then return  'coincident points give infinite circles.'
       if pb >r  then return  'points are too far apart for the specified radius.'
       cb=sqrt(r**2 - pb**2);      x1=y * cb / pb;                  y1=x * cb / pb
                      return  f(bx-x1)   f(by+y1)   f(bx+x1)   f(by-y1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
f:     arg f;   f= right( format(f, , 4), 9);         _= f  /*format # with 4 dec digits*/
       if pos(.,f)>0 & pos('E',f)=0  then f= strip(f,'T',0) /*strip trailing 0s if .& ¬E*/
       return left( strip(f, 'T', .), length(_) )           /*strip trailing dec point. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt:  procedure; arg x; if x=0  then return 0;  d=digits(); numeric digits;  h=d+6;  m.=9
       numeric form;  parse value format(x,2,1,,0) 'E0'  with  g "E" _ .;  g=g *.5'e'_ % 2
         do j=0  while h>9;      m.j=h;               h=h%2+1;       end  /*j*/
         do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;  end  /*k*/;  return g
```

{{out|output}} 

```txt

     x1        y1        x2        y2     radius          circle1x  circle1y  circle2x  circle2y
  ════════  ════════  ════════  ════════  ══════          ════════  ════════  ════════  ════════
   0.1234    0.9876    0.8765    0.2345     2     ───►     1.8631    1.9742   -0.8632   -0.7521
   0         2         0         0          1     ───►     0         1         0         1
   0.1234    0.9876    0.1234    0.9876     2     ───►  coincident points give infinite circles.
   0.1234    0.9876    0.8765    0.2345    0.5    ───►  points are too far apart for the given radius.
   0.1234    0.9876    0.1234    0.9876     0     ───►  radius of zero gives no circles.

```



## Ring


```ring

# Project : Circles of given radius through two points

decimals(4)
x1 = 0.1234
y1 = 0.9876
x2 = 0.8765
y2 = 0.2345
r = 2.0
see "1 : " + x1 + " " + y1 + " " + x2 + " " + y2 + " " + r + nl
twocircles(x1, y1, x2, y2, r)

x1 = 0.0000
y1 = 2.0000
x2 = 0.0000
y2 = 0.0000
r = 1.0
see "2 : " + x1 + " " + y1 + " " + x2 + " " + y2 + " " + r + nl
twocircles(x1, y1, x2, y2, r)

x1 = 0.1234
y1 = 0.9876
x2 = 0.1234
y2 = 0.9876
r = 2.0
see "3 : " + x1 + " " + y1 + " " + x2 + " " + y2 + " " + r + nl
twocircles(x1, y1, x2, y2, r)

x1 = 0.1234
y1 = 0.9876
x2 = 0.8765
y2 = 0.2345
r = 0.5
see "4 : " + x1 + " " + y1 + " " + x2 + " " + y2 + " " + r + nl
twocircles(x1, y1, x2, y2, r)

x1 = 0.1234
y1 = 0.9876
x2 = 0.1234
y2 = 0.9876
r= 0.0
see "5 : " + x1 + " " + y1 + " " + x2 + " " + y2 + " " + r + nl
twocircles(x1, y1, x2, y2, r)        
 
func twocircles(x1, y1, x2, y2, r)
        if x1=x2 and y1=y2 
           if r=0 
              see "It will be a single point (" + x1 + "," + y1 + ") of radius 0" + nl + nl
              return
           else
              see "There are any number of circles via single point (" + x1 + "," + y1 + ") of radius " + r + nl + nl
              return
           ok
        ok
        r2 = sqrt(pow((x1-x2),2)+pow((y1-y2),2))/2
        if r<r2
           see "Points are too far apart (" + 2*r2 + ") - there are no circles of radius " + r + nl + nl
           return
        ok 
        cx=(x1+x2)/2 
        cy=(y1+y2)/2
        dd2=sqrt(pow(r,2)-pow(r2,2))  
        dx1=x2-cx  
        dy1=y2-cy
        dx = 0-dy1/r2*dd2
        dy = dx1/r2*dd2  
        see "(" + (cx+dy) + ", " + (cy+dx) + ")" + nl
        see "(" + (cx-dy) + ", " + (cy-dx) + ")" + nl + nl

```

Output:

```txt

1 : 0.1234 0.9876 0.8765 0.2345 2
(1.8631, 1.9742)
(-0.8632, -0.7521)

2 : 0 2 0 0 1
(0, 1)
(0, 1)

3 : 0.1234 0.9876 0.1234 0.9876 2
There are any number of circles via single point (0.1234,0.9876) of radius 2

4 : 0.1234 0.9876 0.8765 0.2345 0.5000
Points are too far apart (1.0650) - there are no circles of radius 0.5000

5 : 0.1234 0.9876 0.1234 0.9876 0
It will be a single point (0.1234,0.9876) of radius 0

```



## Ruby

{{trans|Python}}

```ruby
Pt     = Struct.new(:x, :y)
Circle = Struct.new(:x, :y, :r)

def circles_from(pt1, pt2, r)
  raise ArgumentError, "Infinite number of circles, points coincide." if pt1 == pt2 && r > 0
  # handle single point and r == 0
  return [Circle.new(pt1.x, pt1.y, r)] if pt1 == pt2 && r == 0
  dx, dy = pt2.x - pt1.x, pt2.y - pt1.y
  # distance between points
  q = Math.hypot(dx, dy)
  # Also catches pt1 != pt2 && r == 0
  raise ArgumentError, "Distance of points > diameter." if q > 2.0*r
  # halfway point
  x3, y3 = (pt1.x + pt2.x)/2.0, (pt1.y + pt2.y)/2.0
  d = (r**2 - (q/2)**2)**0.5
  [Circle.new(x3 - d*dy/q, y3 + d*dx/q, r),
   Circle.new(x3 + d*dy/q, y3 - d*dx/q, r)].uniq
end

# Demo:
ar = [[Pt.new(0.1234, 0.9876), Pt.new(0.8765, 0.2345), 2.0],
      [Pt.new(0.0000, 2.0000), Pt.new(0.0000, 0.0000), 1.0],
      [Pt.new(0.1234, 0.9876), Pt.new(0.1234, 0.9876), 2.0],
      [Pt.new(0.1234, 0.9876), Pt.new(0.8765, 0.2345), 0.5],
      [Pt.new(0.1234, 0.9876), Pt.new(0.1234, 0.9876), 0.0]]

ar.each do |p1, p2, r|
  print "Given points:\n  #{p1.values},\n  #{p2.values}\n  and radius #{r}\n"
  begin
    circles = circles_from(p1, p2, r)
    puts "You can construct the following circles:"
    circles.each{|c| puts "  #{c}"}
  rescue ArgumentError => e
    puts e
  end
  puts
end
```

{{out}}

```txt

Given points:
  [0.1234, 0.9876],
  [0.8765, 0.2345]
  and radius 2.0
You can construct the following circles:
  #<struct Circle x=1.8631118016581891, y=1.974211801658189, r=2.0>
  #<struct Circle x=-0.8632118016581893, y=-0.752111801658189, r=2.0>

Given points:
  [0.0, 2.0],
  [0.0, 0.0]
  and radius 1.0
You can construct the following circles:
  #<struct Circle x=0.0, y=1.0, r=1.0>

Given points:
  [0.1234, 0.9876],
  [0.1234, 0.9876]
  and radius 2.0
Infinite number of circles, points coincide.

Given points:
  [0.1234, 0.9876],
  [0.8765, 0.2345]
  and radius 0.5
Distance of points > diameter.

Given points:
  [0.1234, 0.9876],
  [0.1234, 0.9876]
  and radius 0.0
You can construct the following circles:
  #<struct Circle x=0.1234, y=0.9876, r=0.0>

```



## Run BASIC


```rnbasic

html "<TABLE border=1>"
html "<tr bgcolor=wheat align=center><td>No.</td><td>x1</td><td>y1</td><td>x2</td><td>y2</td><td>r</td><td>cir x1</td><td>cir y1</td><td>cir x2</td><td>cir y2</td></tr>"
for i = 1 to 5
    read x1, y1, x2, y2,r
html "<tr align=right><td>";i;"</td><td>";x1;"</td><td>";y1;"</td><td>";x2;"</td><td>";y2;"</td><td>";r;"</td>"
    gosub [twoCircles]
next
html "</table>"
end

'p1                p2           r
data 0.1234, 0.9876,    0.8765, 0.2345,    2.0
data 0.0000, 2.0000,    0.0000, 0.0000,    1.0
data 0.1234, 0.9876,    0.1234, 0.9876,    2.0
data 0.1234, 0.9876,    0.8765, 0.2345,    0.5
data 0.1234, 0.9876,    0.1234, 0.9876,    0.0

[twoCircles]

    if x1=x2 and y1=y2 then '2.If the points are coincident
        if r=0 then ' unless r==0.0
            html "<td colspan=4 align=left>It will be a single point (";x1;",";y1;") of radius 0</td></tr>"
            RETURN
        else
            html "<td colspan=4 align=left>There are any number of circles via single point (";x1;",";y1;") of radius ";r;"</td></tr>"
            RETURN
        end if
    end if
    r2 = sqr((x1-x2)^2+(y1-y2)^2)/2			'half distance between points
    if r<r2 then
        html "<td colspan=4 align=left>Points are too far apart (";2*r2;") - there are no circles of radius ";r
        RETURN
    end if

    'else, calculate two centers
    cx=(x1+x2)/2 					'middle point
    cy=(y1+y2)/2
    'should move from middle point along perpendicular by dd2
    dd2=sqr(r^2-r2^2)					'perpendicular distance
    dx1=x2-cx   					'vector to middle point
    dy1=y2-cy
    dx = 0-dy1/r2*dd2   				'perpendicular:
    dy = dx1/r2*dd2     				'rotate and scale
    html "<td>";cx+dy;"</td><td>";cy+dx;"</td>"   	'two points, with (+)
    html "<td>";cx-dy;"</td><td>";cy-dx;"</td></TR>" 	'and (-)
RETURN
```

{{Out}}<TABLE BORDER="1">
<TR ALIGN="CENTER" BGCOLOR="wheat"><TD>No.</TD><TD>x1</TD><TD>y1</TD><TD>x2
</TD><TD>y2</TD><TD>r</TD><TD>cir x1</TD><TD>cir y1</TD><TD>cir x2</TD><TD>cir y2</TD></TR>
<TR ALIGN="RIGHT">
<TD>1</TD><TD>0.1234</TD><TD>0.9876</TD><TD>0.8765</TD><TD>0.2345</TD><TD>2.0</TD><TD>1.8631118</TD><TD>1.9742118</TD><TD>-0.863211802</TD><TD>-0.752111802</TD></TR>
<TR ALIGN="RIGHT"><TD>2</TD><TD>0.0d</TD><TD>2.0</TD><TD>0.0d</TD><TD>0.0d</TD><TD>1.0</TD><TD>0.0d</TD><TD>1.0</TD><TD>0.0d</TD><TD>1.0</TD></TR>
<TR ALIGN="RIGHT"><TD>3</TD><TD>0.1234</TD><TD>0.9876</TD><TD>0.1234</TD><TD>0.9876</TD><TD>2.0</TD>
<TD ALIGN="LEFT" COLSPAN="4">There are any number of circles via single point (0.1234,0.9876) of radius 2.0</TD></TR>
<TR ALIGN="RIGHT"><TD>4</TD><TD>0.1234</TD><TD>0.9876</TD><TD>0.8765</TD><TD>0.2345</TD><TD>0.5</TD>
<TD ALIGN="LEFT" COLSPAN="4">Points are too far apart (1.06504423) - there are no circles of radius 0.5</TD></TR>
<TR ALIGN="RIGHT"><TD>5</TD><TD>0.1234</TD><TD>0.9876</TD><TD>0.1234</TD><TD>0.9876</TD><TD>0.0d</TD>
<TD ALIGN="LEFT" COLSPAN="4">It will be a single point (0.1234,0.9876) of radius 0</TD></TR>
</TABLE>


## Rust

{{trans|C}}

```rust
use std::fmt;

#[derive(Clone,Copy)]
struct Point {
    x: f64,
    y: f64
}

fn distance (p1: Point, p2: Point) -> f64 {
    ((p1.x - p2.x).powi(2) + (p1.y - p2.y).powi(2)).sqrt()
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:.4}, {:.4})", self.x, self.y)
    }
}

fn describe_circle(p1: Point, p2: Point, r: f64) {
    let sep = distance(p1, p2);

    if sep == 0. {
        if r == 0. {
            println!("No circles can be drawn through {}", p1);
        } else {
            println!("Infinitely many circles can be drawn through {}", p1);
        }
    } else if sep == 2.0 * r {
        println!("Given points are opposite ends of a diameter of the circle with center ({:.4},{:.4}) and r {:.4}",
                (p1.x+p2.x) / 2.0, (p1.y+p2.y) / 2.0, r);
    } else if sep > 2.0 * r {
        println!("Given points are farther away from each other than a diameter of a circle with r {:.4}", r);
    } else {
        let mirror_dist = (r.powi(2) - (sep / 2.0).powi(2)).sqrt();

        println!("Two circles are possible.");
        println!("Circle C1 with center ({:.4}, {:.4}), r {:.4} and Circle C2 with center ({:.4}, {:.4}), r {:.4}",
                ((p1.x + p2.x) / 2.0) + mirror_dist * (p1.y-p2.y)/sep, (p1.y+p2.y) / 2.0 + mirror_dist*(p2.x-p1.x)/sep,
                r,
                (p1.x+p2.x) / 2.0 - mirror_dist*(p1.y-p2.y)/sep, (p1.y+p2.y) / 2.0 - mirror_dist*(p2.x-p1.x)/sep, r);
    }
}

fn main() {
    let points: Vec<(Point, Point)> = vec![
        (Point { x: 0.1234, y: 0.9876 }, Point { x: 0.8765, y: 0.2345 }),
        (Point { x: 0.0000, y: 2.0000 }, Point { x: 0.0000, y: 0.0000 }),
        (Point { x: 0.1234, y: 0.9876 }, Point { x: 0.1234, y: 0.9876 }),
        (Point { x: 0.1234, y: 0.9876 }, Point { x: 0.8765, y: 0.2345 }),
        (Point { x: 0.1234, y: 0.9876 }, Point { x: 0.1234, y: 0.9876 })
    ];
    let radii: Vec<f64> = vec![2.0, 1.0, 2.0, 0.5, 0.0];

    for (p, r) in points.into_iter().zip(radii.into_iter()) {
        println!("\nPoints: ({}, {}), Radius: {:.4}", p.0, p.1, r);
        describe_circle(p.0, p.1, r);
    }
}
```

{{out}}

```txt
Points: ((0.1234, 0.9876), (0.8765, 0.2345)), Radius: 2.0000
Two circles are possible.
Circle C1 with center (1.8631, 1.9742), r 2.0000 and Circle C2 with center (-0.8632, -0.7521), r 2.0000

Points: ((0.0000, 2.0000), (0.0000, 0.0000)), Radius: 1.0000
Given points are opposite ends of a diameter of the circle with center (0.0000,1.0000) and r 1.0000

Points: ((0.1234, 0.9876), (0.1234, 0.9876)), Radius: 2.0000
Infinitely many circles can be drawn through (0.1234, 0.9876)

Points: ((0.1234, 0.9876), (0.8765, 0.2345)), Radius: 0.5000
Given points are farther away from each other than a diameter of a circle with r 0.5000

Points: ((0.1234, 0.9876), (0.1234, 0.9876)), Radius: 0.0000
No circles can be drawn through (0.1234, 0.9876)
```



## Scala


```scala
import org.scalatest.FunSuite
import math._

case class V2(x: Double, y: Double) {
  val distance = hypot(x, y)
  def /(other: V2) = V2((x+other.x) / 2.0, (y+other.y) / 2.0)
  def -(other: V2) = V2(x-other.x,y-other.y)
  override def equals(other: Any) = other match {
    case p: V2 => abs(x-p.x) <  0.0001 && abs(y-p.y) <  0.0001
    case _ => false
  }
  override def toString = f"($x%.4f, $y%.4f)"
}

case class Circle(center: V2, radius: Double)

class PointTest extends FunSuite {
  println("       p1               p2         r    result")
  Seq(
    (V2(0.1234, 0.9876), V2(0.8765, 0.2345), 2.0, Seq(Circle(V2(1.8631, 1.9742), 2.0), Circle(V2(-0.8632, -0.7521), 2.0))),
    (V2(0.0000, 2.0000), V2(0.0000, 0.0000), 1.0, Seq(Circle(V2(0.0, 1.0), 1.0))),
    (V2(0.1234, 0.9876), V2(0.1234, 0.9876), 2.0, "coincident points yields infinite circles"),
    (V2(0.1234, 0.9876), V2(0.8765, 0.2345), 0.5, "radius is less then the distance between points"),
    (V2(0.1234, 0.9876), V2(0.1234, 0.9876), 0.0, "radius of zero yields no circles")
  ) foreach { v =>
    print(s"${v._1} ${v._2}  ${v._3}: ")
    circles(v._1, v._2, v._3) match {
      case Right(list) => println(list mkString ",")
        assert(list === v._4)
      case Left(error) => println(error)
        assert(error === v._4)
    }
  }

  def circles(p1: V2, p2: V2, radius: Double) = if (radius == 0.0) {
      Left("radius of zero yields no circles")
    } else if (p1 == p2) {
      Left("coincident points yields infinite circles")
    } else if (radius * 2 < (p1-p2).distance) {
      Left("radius is less then the distance between points")
    } else {
      Right(circlesThruPoints(p1, p2, radius))
    } ensuring { result =>
      result.isLeft || result.right.get.nonEmpty
    }

  def circlesThruPoints(p1: V2, p2: V2, radius: Double): Seq[Circle] = {
    val diff = p2 - p1
    val d = pow(pow(radius, 2) - pow(diff.distance / 2, 2), 0.5)
    val mid = p1 / p2
    Seq(
      Circle(V2(mid.x - d * diff.y / diff.distance, mid.y + d * diff.x / diff.distance), abs(radius)),
      Circle(V2(mid.x + d * diff.y / diff.distance, mid.y - d * diff.x / diff.distance), abs(radius))).distinct
  }
}
```

{{out}}

```txt
       p1               p2         r    result
(0.1234, 0.9876) (0.8765, 0.2345)  2.0: Circle((1.8631, 1.9742),2.0),Circle((-0.8632, -0.7521),2.0)
(0.0000, 2.0000) (0.0000, 0.0000)  1.0: Circle((0.0000, 1.0000),1.0)
(0.1234, 0.9876) (0.1234, 0.9876)  2.0: coincident points yields infinite circles
(0.1234, 0.9876) (0.8765, 0.2345)  0.5: radius is less then the distance between points
(0.1234, 0.9876) (0.1234, 0.9876)  0.0: radius of zero yields no circlesEmpty test suite.
```



## Scheme



```scheme

(import (scheme base)
        (scheme inexact)
        (scheme write))

;; c1 and c2 are pairs (x y), r a positive radius
(define (find-circles c1 c2 r)
  (define x-coord car) ; for easier to read coordinate extraction from list
  (define y-coord cadr)
  (define (approx= a b) (< (- a b) 0.000001)) ; equal within tolerance
  (define (avg a b) (/ (+ a b) 2))
  (define (distance pt1 pt2)
    (sqrt (+ (square (- (x-coord pt1) (x-coord pt2)))
             (square (- (y-coord pt1) (y-coord pt2))))))
  (define (equal-points? pt1 pt2)
    (and (approx= (x-coord pt1) (x-coord pt2))
         (approx= (y-coord pt1) (y-coord pt2))))
  (define (delete-duplicate pts) ; assume no more than two points in list
    (if (and (= 2 (length pts))
             (equal-points? (car pts) (cadr pts)))
      (list (car pts)) ; keep the first only
      pts))
  ;
  (let ((d (distance c1 c2)))
    (cond ((equal-points? c1 c2) ; coincident points
           (if (> r 0)
             'infinite   ; r > 0
             (list c1))) ; else r = 0
          ((< (* 2 r) d) 
           '()) ; circle cannot reach both points, as too far apart
          ((approx= r 0.0) ; r = 0, no circles, as points differ
           '()) 
          (else ; find up to two circles meeting c1 and c2
            (let* ((mid-pt (list (avg (x-coord c1) (x-coord c2))
                                 (avg (y-coord c1) (y-coord c2))))
                   (offset (sqrt (- (square r) 
                                    (square (* 0.5 d)))))
                   (delta-cx (/ (- (x-coord c1) (x-coord c2)) d))
                   (delta-cy (/ (- (y-coord c1) (y-coord c2)) d)))
              (delete-duplicate
                (list (list (- (x-coord mid-pt) (* offset delta-cx))
                            (+ (y-coord mid-pt) (* offset delta-cy)))
                      (list (+ (x-coord mid-pt) (* offset delta-cx))
                            (- (y-coord mid-pt) (* offset delta-cy))))))))))

;; work through the input examples, outputting results
(for-each 
  (lambda (c1 c2 r)
    (let ((result (find-circles c1 c2 r)))
      (display "p1: ") (display c1)
      (display " p2: ") (display c2)
      (display " r: ") (display (number->string r))
      (display " => ")
      (cond ((eq? result 'infinite)
             (display "Infinite number of circles"))
            ((null? result)
             (display "No circles"))
            (else
              (display result)))
      (newline)))
  '((0.1234 0.9876) (0.0000 2.0000) (0.1234 0.9876) (0.1234 0.9876) (0.1234 0.9876))
  '((0.8765 0.2345) (0.0000 0.0000) (0.1234 0.9876) (0.8765 0.2345) (0.1234 0.9876))
  '(2.0 1.0 2.0 0.5 0.0))

```


{{out}}

```txt

p1: (0.1234 0.9876) p2: (0.8765 0.2345) r: 2.0 => ((1.86311180165819 1.97421180165819) (-0.863211801658189 -0.752111801658189))
p1: (0.0 2.0) p2: (0.0 0.0) r: 1.0 => ((0.0 1.0))
p1: (0.1234 0.9876) p2: (0.1234 0.9876) r: 2.0 => Infinite number of circles
p1: (0.1234 0.9876) p2: (0.8765 0.2345) r: 0.5 => No circles
p1: (0.1234 0.9876) p2: (0.1234 0.9876) r: 0.0 => ((0.1234 0.9876))

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";

const type: point is new struct
    var float: x is 0.0;
    var float: y is 0.0;
  end struct;

const func point: point (in float: x, in float: y) is func
  result
    var point: aPoint is point.value;
  begin
    aPoint.x := x;
    aPoint.y := y;
  end func;

const func float: distance (in point: p1, in point: p2) is
  return sqrt((p1.x - p2.x) ** 2 + (p1.y - p2.y) ** 2);

const proc: findCircles (in point: p1, in point: p2, in float: radius) is func
  local
    var float: separation is 0.0;
    var float: mirrorDistance is 0.0;
  begin
    separation := distance(p1, p2);
    if separation = 0.0 then
      if radius = 0.0 then
        write("Radius of zero. No circles can be drawn through (");
      else
        write("Infinitely many circles can be drawn through (");
      end if;
      writeln(p1.x digits 4 <& ", " <& p1.y digits 4 <& ")");
    elsif separation = 2.0 * radius then
      writeln("Given points are opposite ends of a diameter of the circle with center (" <&
              (p1.x + p2.x) / 2.0 digits 4 <& ", " <& (p1.y + p2.y) / 2.0 digits 4 <& ") and radius " <&
              radius digits 4); 
    elsif separation > 2.0 * radius then
      writeln("Given points are farther away from each other than a diameter of a circle with radius " <&
              radius digits 4);
    else
      mirrorDistance := sqrt(radius ** 2 - (separation / 2.0) ** 2);
      writeln("Two circles are possible.");
      writeln("Circle C1 with center (" <&
              (p1.x + p2.x) / 2.0 + mirrorDistance*(p1.y - p2.y) / separation digits 4 <& ", " <&
              (p1.y + p2.y) / 2.0 + mirrorDistance*(p2.x - p1.x) / separation digits 4 <& "), radius " <&
              radius digits 4);
      writeln("Circle C2 with center (" <&
              (p1.x + p2.x) / 2.0 - mirrorDistance*(p1.y - p2.y) / separation digits 4 <& ", " <&
              (p1.y + p2.y) / 2.0 - mirrorDistance*(p2.x - p1.x) / separation digits 4 <& "), radius " <&
              radius digits 4);
    end if;
  end func;

const proc: main is func
  local
    const array array float: cases is [] (
        [] (0.1234, 0.9876, 0.8765, 0.2345, 2.0),
        [] (0.0000, 2.0000, 0.0000, 0.0000, 1.0),
        [] (0.1234, 0.9876, 0.1234, 0.9876, 2.0),
        [] (0.1234, 0.9876, 0.8765, 0.2345, 0.5),
        [] (0.1234, 0.9876, 0.1234, 0.9876, 0.0));
    var integer: index is 0;
  begin
    for index range 1 to 5 do
      writeln("Case " <& index <& ":");
      findCircles(point(cases[index][1], cases[index][2]),
                  point(cases[index][3], cases[index][4]), cases[index][5]);
    end for;
  end func;
```


{{out}}

```txt

Case 1:
Two circles are possible.
Circle C1 with center (1.8631, 1.9742), radius 2.0000
Circle C2 with center (-0.8632, -0.7521), radius 2.0000
Case 2:
Given points are opposite ends of a diameter of the circle with center (0.0000, 1.0000) and radius 1.0000
Case 3:
Infinitely many circles can be drawn through (0.1234, 0.9876)
Case 4:
Given points are farther away from each other than a diameter of a circle with radius 0.5000
Case 5:
Radius of zero. No circles can be drawn through (0.1234, 0.9876)

```



## Sidef

{{trans|Perl 6}}

```ruby
func circles(a, b, r) {

    if (a == b) {
        if (r == 0) {
            return ['Degenerate point']
        }
        else {
            return ['Infinitely many share a point']
        }
    }

    var h = (b-a)/2

    if (r**2 < h.norm) {
        return ['Too far apart']
    }

    var l = sqrt(r**2 - h.norm)

    [1i, -1i].map {|i|
        a + h + (l*i*h / h.abs) -> round(-16)
    }
}

var input = [
    [0.1234 + 0.9876i,  0.8765 + 0.2345i, 2.0],
    [0.0000 + 2.0000i,  0.0000 + 0.0000i, 1.0],
    [0.1234 + 0.9876i,  0.1234 + 0.9876i, 2.0],
    [0.1234 + 0.9876i,  0.8765 + 0.2345i, 0.5],
    [0.1234 + 0.9876i,  0.1234 + 0.9876i, 0.0],
]

input.each {|a|
    say (a.join(', '), ': ', circles(a...).join(' and '))
}
```

{{out}}

```txt

0.1234+0.9876i, 0.8765+0.2345i, 2: 1.8631118016581891+1.9742118016581891i and -0.8632118016581891-0.7521118016581891i
2i, 0, 1: i and i
0.1234+0.9876i, 0.1234+0.9876i, 2: Infinitely many share a point
0.1234+0.9876i, 0.8765+0.2345i, 0.5: Too far apart
0.1234+0.9876i, 0.1234+0.9876i, 0: Degenerate point

```



## Stata

Each circle center is the image of B by the composition of a rotation and homothecy centered at A. It's how the centers are computed in this implementation. The coordinates are returned as the columns of a 2x2 matrix. When the solution is not unique or does not exist, this matrix contains only missing values.


```stata
real matrix centers(real colvector a, real colvector b, real scalar r) {
	real matrix rot
	real scalar d, u, v
	d = norm(b-a)
	if (r == 0 | d == 0) {
		if (r == 0 & d == 0) {
			return((a,a))
		} else {
			return(J(2, 2, .))
		}
	} else if (d <= 2*r) {
		u = d/(2*r)
		v = sqrt(1-u^2)
		rot = u,-v\v,u
		return((rot*(b-a),rot'*(b-a))*r/d:+a)
	} else {
		return(J(2, 2, .))
	}
}
```


Examples:


```stata
:a=0.1234\0.9876
:b=0.8765\0.2345
: centers(a,b,2)
                  1              2
    +-------------------------------+
  1 |   1.863111802   -.8632118017  |
  2 |   1.974211802   -.7521118017  |
    +-------------------------------+

: centers((0\2),(0\0),1)
       1   2
    +---------+
  1 |  0   0  |
  2 |  1   1  |
    +---------+

: centers(a,a,2)
[symmetric]
       1   2
    +---------+
  1 |  .      |
  2 |  .   .  |
    +---------+

: centers(a,b,0.5)
[symmetric]
       1   2
    +---------+
  1 |  .      |
  2 |  .   .  |
    +---------+

: centers(a,a,0)
           1       2
    +-----------------+
  1 |  .1234   .1234  |
  2 |  .9876   .9876  |
    +-----------------+
```



## Swift


{{trans|F#}}


```swift
import Foundation

struct Point: Equatable {
    var x: Double
    var y: Double
}

struct Circle {
  var center: Point
  var radius: Double

  static func circleBetween(
    _ p1: Point,
    _ p2: Point,
    withRadius radius: Double
  ) -> (Circle, Circle?)? {
    func applyPoint(_ p1: Point, _ p2: Point, op: (Double, Double) -> Double) -> Point {
      return Point(x: op(p1.x, p2.x), y: op(p1.y, p2.y))
    }

    func mul2(_ p: Point, mul: Double) -> Point {
      return Point(x: p.x * mul, y: p.y * mul)
    }

    func div2(_ p: Point, div: Double) -> Point {
      return Point(x: p.x / div, y: p.y / div)
    }

    func norm(_ p: Point) -> Point {
      return div2(p, div: (p.x * p.x + p.y * p.y).squareRoot())
    }

    guard radius != 0, p1 != p2 else {
      return nil
    }

    let diameter = 2 * radius
    let pq = applyPoint(p1, p2, op: -)
    let magPQ = (pq.x * pq.x + pq.y * pq.y).squareRoot()

    guard diameter >= magPQ else {
      return nil
    }

    let midpoint = div2(applyPoint(p1, p2, op: +), div: 2)
    let halfPQ = magPQ / 2
    let magMidC = abs(radius * radius - halfPQ * halfPQ).squareRoot()
    let midC = mul2(norm(Point(x: -pq.y, y: pq.x)), mul: magMidC)
    let center1 = applyPoint(midpoint, midC, op: +)
    let center2 = applyPoint(midpoint, midC, op: -)

    if center1 == center2 {
      return (Circle(center: center1, radius: radius), nil)
    } else {
      return (Circle(center: center1, radius: radius), Circle(center: center2, radius: radius))
    }
  }
}

let testCases = [
  (Point(x: 0.1234, y: 0.9876), Point(x: 0.8765, y: 0.2345), 2.0),
  (Point(x: 0.0000, y: 2.0000), Point(x: 0.0000, y: 0.0000), 1.0),
  (Point(x: 0.1234, y: 0.9876), Point(x: 0.1234, y: 0.9876), 2.0),
  (Point(x: 0.1234, y: 0.9876), Point(x: 0.8765, y: 0.2345), 0.5),
  (Point(x: 0.1234, y: 0.9876), Point(x: 0.1234, y: 0.9876), 0.0)
]

for testCase in testCases {
  switch Circle.circleBetween(testCase.0, testCase.1, withRadius: testCase.2) {
  case nil:
    print("No ans")
  case (let circle1, nil)?:
    print("One ans: \(circle1)")
  case (let circle1, let circle2?)?:
    print("Two ans: \(circle1) \(circle2)")
  }
}


```


{{out}}


```txt
Two ans: Circle(center: Point(x: -0.8632118016581896, y: -0.7521118016581892), radius: 2.0) Circle(center: Point(x: 1.8631118016581893, y: 1.974211801658189), radius: 2.0)
One ans: Circle(center: Point(x: 0.0, y: 1.0), radius: 1.0)
No ans
No ans
No ans
```



## Tcl

{{trans|Python}}

```tcl
proc findCircles {p1 p2 r} {
    lassign $p1 x1 y1
    lassign $p2 x2 y2
    # Special case: coincident & zero size
    if {$x1 == $x2 && $y1 == $y2 && $r == 0.0} {
	return [list [list $x1 $y1 0.0]]
    }
    if {$r <= 0.0} {
	error "radius must be positive for sane results"
    }
    if {$x1 == $x2 && $y1 == $y2} {
	error "no sane solution: points are coincident"
    }

    # Calculate distance apart and separation vector
    set dx [expr {$x2 - $x1}]
    set dy [expr {$y2 - $y1}]
    set q [expr {hypot($dx, $dy)}]
    if {$q > 2*$r} {
	error "no solution: points are further apart than required diameter"
    }

    # Calculate midpoint
    set x3 [expr {($x1+$x2)/2.0}]
    set y3 [expr {($y1+$y2)/2.0}]
    # Fractional distance along the mirror line
    set f [expr {($r**2 - ($q/2.0)**2)**0.5 / $q}]
    # The two answers
    set c1 [list [expr {$x3 - $f*$dy}] [expr {$y3 + $f*$dx}] $r]
    set c2 [list [expr {$x3 + $f*$dy}] [expr {$y3 - $f*$dx}] $r]
    return [list $c1 $c2]
}
```


{{out|Demo}}

```tcl
foreach {p1 p2 r} {
    {0.1234 0.9876} {0.8765 0.2345} 2.0
    {0.0000 2.0000} {0.0000 0.0000} 1.0
    {0.1234 0.9876} {0.1234 0.9876} 2.0
    {0.1234 0.9876} {0.8765 0.2345} 0.5
    {0.1234 0.9876} {0.1234 0.9876} 0.0
} {
    puts "p1:([join $p1 {, }]) p2:([join $p2 {, }]) r:$r =>"
    if {[catch {
	foreach c [findCircles $p1 $p2 $r] {
	    puts "\tCircle:([join $c {, }])"
	}
    } msg]} {
	puts "\tERROR: $msg"
    }
}
```

{{out}}

```txt

p1:(0.1234, 0.9876) p2:(0.8765, 0.2345) r:2.0 =>
	Circle:(1.863111801658189, 1.974211801658189, 2.0)
	Circle:(-0.8632118016581891, -0.752111801658189, 2.0)
p1:(0.0000, 2.0000) p2:(0.0000, 0.0000) r:1.0 =>
	Circle:(0.0, 1.0, 1.0)
	Circle:(0.0, 1.0, 1.0)
p1:(0.1234, 0.9876) p2:(0.1234, 0.9876) r:2.0 =>
	ERROR: no sane solution: points are coincident
p1:(0.1234, 0.9876) p2:(0.8765, 0.2345) r:0.5 =>
	ERROR: no solution: points are further apart than required diameter
p1:(0.1234, 0.9876) p2:(0.1234, 0.9876) r:0.0 =>
	Circle:(0.1234, 0.9876, 0.0)

```



## VBA

{{trans|Phix}}
```vb
Public Sub circles()
    tests = [{0.1234, 0.9876, 0.8765, 0.2345, 2.0; 0.0000, 2.0000, 0.0000, 0.0000, 1.0; 0.1234, 0.9876, 0.1234, 0.9876, 2.0; 0.1234, 0.9876, 0.8765, 0.2345, 0.5; 0.1234, 0.9876, 0.1234, 0.9876, 0.0}]
    For i = 1 To UBound(tests)
        x1 = tests(i, 1)
        y1 = tests(i, 2)
        x2 = tests(i, 3)
        y2 = tests(i, 4)
        R = tests(i, 5)
        xd = x2 - x1
        yd = y1 - y2
        s2 = xd * xd + yd * yd
        sep = Sqr(s2)
        xh = (x1 + x2) / 2
        yh = (y1 + y2) / 2
        Dim txt As String
        If sep = 0 Then
            txt = "same points/" & IIf(R = 0, "radius is zero", "infinite solutions")
        Else
            If sep = 2 * R Then
                txt = "opposite ends of diameter with centre " & xh & ", " & yh & "."
            Else
                If sep > 2 * R Then
                    txt = "too far apart " & sep & " > " & 2 * R
                Else
                    md = Sqr(R * R - s2 / 4)
                    xs = md * xd / sep
                    ys = md * yd / sep
                    txt = "{" & Format(xh + ys, "0.0000") & ", " & Format(yh + xs, "0.0000") & _
                    "} and {" & Format(xh - ys, "0.0000") & ", " & Format(yh - xs, "0.0000") & "}"
                End If
            End If
        End If
        Debug.Print "points " & "{" & x1 & ", " & y1 & "}" & ", " & "{" & x2 & ", " & y2 & "}" & " with radius " & R & " ==> " & txt
    Next i
End Sub
```
{{out}}

```txt
points {0,1234, 0,9876}, {0,8765, 0,2345} with radius 2 ==> {1,8631, 1,9742} and {-0,8632, -0,7521}
points {0, 2}, {0, 0} with radius 1 ==> opposite ends of diameter with centre 0, 1.
points {0,1234, 0,9876}, {0,1234, 0,9876} with radius 2 ==> same points/infinite solutions
points {0,1234, 0,9876}, {0,8765, 0,2345} with radius 0,5 ==> too far apart 1,06504423382318 > 1
points {0,1234, 0,9876}, {0,1234, 0,9876} with radius 0 ==> same points/radius is zero
```


## Visual Basic .NET

{{trans|C#}}

```vbnet
Public Class CirclesOfGivenRadiusThroughTwoPoints
    Public Shared Sub Main()
        For Each valu In New Double()() {
        New Double() {0.1234, 0.9876, 0.8765, 0.2345, 2},
        New Double() {0.0, 2.0, 0.0, 0.0, 1},
        New Double() {0.1234, 0.9876, 0.1234, 0.9876, 2},
        New Double() {0.1234, 0.9876, 0.8765, 0.2345, 0.5},
        New Double() {0.1234, 0.9876, 0.1234, 0.9876, 0},
        New Double() {0.1234, 0.9876, 0.2345, 0.8765, 0}}
            Dim p = New Point(valu(0), valu(1)), q = New Point(valu(2), valu(3))
            Console.WriteLine($"Points {p} and {q} with radius {valu(4)}:")
            Try
                Console.WriteLine(vbTab & String.Join(" and ", FindCircles(p, q, valu(4))))
            Catch ex As Exception
                Console.WriteLine(vbTab & ex.Message)
            End Try
        Next
        If System.Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub

    Private Shared Function FindCircles(ByVal p As Point, ByVal q As Point, ByVal rad As Double) As Point()
        If rad < 0 Then Throw New ArgumentException("Negative radius.")
        If rad = 0 Then Throw New InvalidOperationException(If(p = q,
            String.Format("{0} (degenerate circle)", {p}), "No circles."))
        If p = q Then Throw New InvalidOperationException("Infinite number of circles.")
        Dim dist As Double = Point.Distance(p, q), sqDist As Double = dist * dist,
            sqDiam As Double = 4 * rad * rad
        If sqDist > sqDiam Then Throw New InvalidOperationException(
            String.Format("Points are too far apart (by {0}).", sqDist - sqDiam))
        Dim midPoint As Point = New Point((p.X + q.X) / 2, (p.Y + q.Y) / 2)
        If sqDist = sqDiam Then Return {midPoint}
        Dim d As Double = Math.Sqrt(rad * rad - sqDist / 4),
            a As Double = d * (q.X - p.X) / dist, b As Double = d * (q.Y - p.Y) / dist
        Return {New Point(midPoint.X - b, midPoint.Y + a), New Point(midPoint.X + b, midPoint.Y - a)}
    End Function

    Public Structure Point
        Public ReadOnly Property X As Double
        Public ReadOnly Property Y As Double

        Public Sub New(ByVal ix As Double, ByVal iy As Double)
            Me.New() : X = ix : Y = iy
        End Sub

        Public Shared Operator =(ByVal p As Point, ByVal q As Point) As Boolean
            Return p.X = q.X AndAlso p.Y = q.Y
        End Operator

        Public Shared Operator <>(ByVal p As Point, ByVal q As Point) As Boolean
            Return p.X <> q.X OrElse p.Y <> q.Y
        End Operator

        Public Shared Function SquaredDistance(ByVal p As Point, ByVal q As Point) As Double
            Dim dx As Double = q.X - p.X, dy As Double = q.Y - p.Y
            Return dx * dx + dy * dy
        End Function

        Public Shared Function Distance(ByVal p As Point, ByVal q As Point) As Double
            Return Math.Sqrt(SquaredDistance(p, q))
        End Function

        Public Overrides Function ToString() As String
            Return $"({X}, {Y})"
        End Function
    End Structure
End Class
```

{{out}}

```txt
Points (0.1234, 0.9876) and (0.8765, 0.2345) with radius 2:
        (1.86311180165819, 1.97421180165819) and (-0.86321180165819, -0.752111801658189)
Points (0, 2) and (0, 0) with radius 1:
        (0, 1)
Points (0.1234, 0.9876) and (0.1234, 0.9876) with radius 2:
        Infinite number of circles.
Points (0.1234, 0.9876) and (0.8765, 0.2345) with radius 0.5:
        Points are too far apart (by 0.13431922).
Points (0.1234, 0.9876) and (0.1234, 0.9876) with radius 0:
        (0.1234, 0.9876) (degenerate circle)
Points (0.1234, 0.9876) and (0.2345, 0.8765) with radius 0:
        No circles.
```



## Visual FoxPro

Translation of BASIC.

```vfp

LOCAL p1 As point, p2 As point, rr As Double
CLOSE DATABASES ALL
SET FIXED ON
SET DECIMALS TO 4
CLEAR
CREATE CURSOR circles (xc1 B(4), yc1 B(4), xc2 B(4), yc2 B(4), rad B(4))
INSERT INTO circles VALUES (0.1234, 0.9876, 0.8765, 0.2345, 2.0)
INSERT INTO circles VALUES (0.0000, 2.0000, 0.0000, 0.0000, 1.0)
INSERT INTO circles VALUES (0.1234, 0.9876, 0.1234, 0.9876, 2.0)
INSERT INTO circles VALUES (0.1234, 0.9876, 0.8765, 0.2345, 0.5)
INSERT INTO circles VALUES (0.1234, 0.9876, 0.1234, 0.9876, 0.0)
GO TOP

p1 = NEWOBJECT("point")
p2 = NEWOBJECT("point")
SCAN
    p1.SetPoints(xc1, yc1)
    p2.SetPoints(xc2, yc2)
    rr = rad
    GetCircles(p1, p2, rr)
    ?
ENDSCAN 	

SET DECIMALS TO 
SET FIXED OFF

PROCEDURE GetCircles(op1 As point, op2 As point, r As Double)
LOCAL ctr As point, half As point, lenhalf As Double, dist As Double, rot As point, c As String
ctr = NEWOBJECT("point")
half = NEWOBJECT("point")
ctr.SetPoints((op1.xc + op2.xc)/2, (op1.yc + op2.yc)/2)
half.SetPoints(op1.xc - ctr.xc, op1.yc - ctr.yc)
lenhalf = half.nLength
PrintPoints(op1, op2, r)
IF r < lenhalf
    ? "Cannot solve for these parameters."
    RETURN
ENDIF
IF lenhalf = 0
    ? "Points are coincident."
    RETURN
ENDIF
dist = SQRT(r^2 - lenhalf^2)/lenhalf
rot = NEWOBJECT("point")
rot.SetPoints(-dist*(op1.yc - ctr.yc) + ctr.xc, dist*(op1.xc - ctr.xc) + ctr.yc)
TEXT TO c TEXTMERGE NOSHOW PRETEXT 3
    Circle 1 (<<rot.xc>>, <<rot.yc>>)
ENDTEXT
? c
rot.SetPoints(-(rot.xc - ctr.xc) + ctr.xc, -((rot.yc - ctr.yc)) + ctr.yc)
TEXT TO c TEXTMERGE NOSHOW PRETEXT 3
    Circle 2 (<<rot.xc>>, <<rot.yc>>)
ENDTEXT
? c
ENDPROC

PROCEDURE PrintPoints(op1 As point, op2 As point, r As Double)
LOCAL lcTxt As String
TEXT TO lcTxt TEXTMERGE NOSHOW PRETEXT 3
    Points (<<op1.xc>>,<<op1.yc>>), (<<op2.xc>>,<<op2.yc>>) Radius <<r>>.
ENDTEXT
? lcTxt
ENDPROC	

DEFINE CLASS point As Custom
xc = 0
yc = 0
nLength = 0

PROCEDURE Init
DODEFAULT()
ENDPROC

PROCEDURE SetPoints(tnx As Double, tny As Double)
THIS.xc = tnx
THIS.yc = tny
THIS.nLength = THIS.GetLength()
ENDPROC

FUNCTION GetLength()
RETURN SQRT(THIS.xc*THIS.xc + THIS.yc*THIS.yc)
ENDFUNC

ENDDEFINE

```

{{out}}

```txt


Points (0.1234,0.9876), (0.8765,0.2345) Radius 2.0000.
Points (0.1234,0.9876), (0.8765,0.2345) Radius 2.0000.         
Circle 1 (-0.8632, -0.7521)
Circle 1 (-0.8632, -0.7521)  
Circle 2 (1.8631, 1.9742)
Circle 2 (1.8631, 1.9742)   

Points (0.0000,2.0000), (0.0000,0.0000) Radius 1.0000.
Points (0.0000,2.0000), (0.0000,0.0000) Radius 1.0000.         
Circle 1 (0.0000, 1.0000)
Circle 1 (0.0000, 1.0000)   
Circle 2 (0.0000, 1.0000)
Circle 2 (0.0000, 1.0000)   

Points (0.1234,0.9876), (0.1234,0.9876) Radius 2.0000.
Points (0.1234,0.9876), (0.1234,0.9876) Radius 2.0000.         
Points are coincident.  

Points (0.1234,0.9876), (0.8765,0.2345) Radius 0.5000.
Points (0.1234,0.9876), (0.8765,0.2345) Radius 0.5000.         
Cannot solve for these parameters.     

Points (0.1234,0.9876), (0.1234,0.9876) Radius 0.0000.
Points (0.1234,0.9876), (0.1234,0.9876) Radius 0.0000.         
Points are coincident.  

```



## XPL0

An easy way to solve this:
translate the coordinates so that one point is at the origin. 
Then rotate the coordinate frame so that the second point is on the X-axis. 
The circles' X coordinate is then half the distance to the second point. 
The circles' Y coordinates are easily seen as +/-sqrt(radius^2 - circleX^2). 
Now undo the rotation and translation.
The method used here is a streamlining of these steps.


```XPL0
include c:\cxpl\codes;

proc Circles; real Data; \Show centers of circles, given points P & Q and radius
real Px, Py, Qx, Qy, R, X, Y, X1, Y1, Bx, By, PB, CB;
[Px:= Data(0); Py:= Data(1); Qx:= Data(2); Qy:= Data(3); R:= Data(4);
if R = 0.0 then [Text(0, "Radius = zero gives no circles^M^J"); return];
X:= (Qx-Px)/2.0;  Y:= (Qy-Py)/2.0;
Bx:= Px+X;  By:= Py+Y;
PB:= sqrt(X*X + Y*Y);
if PB = 0.0 then [Text(0, "Coincident points give infinite circles^M^J"); return];
if PB > R   then [Text(0, "Points are too far apart for radius^M^J"); return];
CB:= sqrt(R*R - PB*PB);
X1:= Y*CB/PB; Y1:= X*CB/PB;
RlOut(0, Bx-X1); ChOut(0, ^,); RlOut(0, By+Y1); ChOut(0, 9\tab\);
RlOut(0, Bx+X1); ChOut(0, ^,); RlOut(0, By-Y1); CrLf(0);
];

real Tbl; int I;
[Tbl:=[[0.1234, 0.9876,    0.8765, 0.2345,    2.0],
       [0.0000, 2.0000,    0.0000, 0.0000,    1.0],
       [0.1234, 0.9876,    0.1234, 0.9876,    2.0],
       [0.1234, 0.9876,    0.8765, 0.2345,    0.5],
       [0.1234, 0.9876,    0.1234, 0.9876,    0.0]];
for I:= 0 to 4 do Circles(Tbl(I));
]
```


{{out}}

```txt

    1.86311,    1.97421    -0.86321,   -0.75211
    0.00000,    1.00000     0.00000,    1.00000
Coincident points give infinite circles
Points are too far apart for radius
Radius = zero gives no circles

```



## zkl

{{trans|C}}

```zkl
fcn findCircles(a,b, c,d, r){ //-->T(T(x,y,r) [,T(x,y,r)]))
   delta:=(a-c).hypot(b-d);
   switch(delta){	// could just catch MathError
      case(0.0){"singularity"}  // should use epsilon test
      case(r*2){T(T((a+c)/2,(b+d)/2,r))}
      else{
	 if(delta > 2*r) "Point delta > diameter";
	 else{
	    md:=(r.pow(2) - (delta/2).pow(2)).sqrt();
	    T(T((a+c)/2 + md*(b-d)/delta,(b+d)/2 + md*(c-b)/delta,r),
	      T((a+c)/2 - md*(b-d)/delta,(b+d)/2 - md*(c-b)/delta,r));
	  }
       }
    }
}

data:=T(
   T(0.1234, 0.9876,    0.8765, 0.2345,    2.0),
   T(0.0000, 2.0000,    0.0000, 0.0000,    1.0),
   T(0.1234, 0.9876,    0.1234, 0.9876,    2.0),
   T(0.1234, 0.9876,    0.8765, 0.2345,    0.5),
   T(0.1234, 0.9876,    0.1234, 0.9876,    0.0),
);

ppFmt:="(%2.4f,%2.4f)";
pprFmt:=ppFmt+" r=%2.1f";
foreach a,b, c,d, r in (data){
   println("Points: ",ppFmt.fmt(a,b),", ",pprFmt.fmt(c,d,r));
   print("   Circles: ");
   cs:=findCircles(a,b,c,d,r);
   if(List.isType(cs))
       print(cs.pump(List,'wrap(c){pprFmt.fmt(c.xplode())}).concat(", "));
   else print(cs);
   println();
}
```

{{out}}

```txt

Points: (0.1234,0.9876), (0.8765,0.2345) r=2.0
   Circles: (1.8631,1.9742) r=2.0, (-0.8632,-0.7521) r=2.0
Points: (0.0000,2.0000), (0.0000,0.0000) r=1.0
   Circles: (0.0000,1.0000) r=1.0
Points: (0.1234,0.9876), (0.1234,0.9876) r=2.0
   Circles: singularity
Points: (0.1234,0.9876), (0.8765,0.2345) r=0.5
   Circles: Point delta > diameter
Points: (0.1234,0.9876), (0.1234,0.9876) r=0.0
   Circles: singularity

```



## ZX Spectrum Basic

{{trans|Liberty BASIC}}

```zxbasic
10 FOR i=1 TO 5
20 READ x1,y1,x2,y2,r
30 PRINT i;") ";x1;" ";y1;" ";x2;" ";y2;" ";r
40 GO SUB 1000
50 NEXT i
60 STOP 
70 DATA 0.1234,0.9876,0.8765,0.2345,2.0
80 DATA 0.0000,2.0000,0.0000,0.0000,1.0
90 DATA 0.1234,0.9876,0.1234,0.9876,2.0
100 DATA 0.1234,0.9876,0.8765,0.2345,0.5
110 DATA 0.1234,0.9876,0.1234,0.9876,0.0
1000 IF NOT (x1=x2 AND y1=y2) THEN GO TO 1090
1010 IF r=0 THEN PRINT "It will be a single point (";x1;",";y1;") of radius 0": RETURN 
1020 PRINT "There are any number of circles via single point (";x1;",";y1;") of radius ";r: RETURN 
1090 LET p1=(x1-x2): LET p2=(y1-y2)
1100 LET r2=SQR (p1*p1+p2*p2)/2
1110 IF r<r2 THEN PRINT "Points are too far apart (";2*r2;") - there are no circles of radius ";r: RETURN 
1120 LET cx=(x1+x2)/2
1130 LET cy=(y1+y2)/2
1140 LET dd2=SQR (r^2-r2^2)
1150 LET dx1=x2-cx
1160 LET dy1=y2-cy
1170 LET dx=0-dy1/r2*dd2
1180 LET dy=dx1/r2*dd2
1190 PRINT "(";cx+dy;",";cy+dx;")"
1200 PRINT "(";cx-dy;",";cy-dx;")"
1210 RETURN
```


[[Category:Geometry]]
