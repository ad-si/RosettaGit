+++
title = "Haversine formula"
description = ""
date = 2019-08-22T15:56:25Z
aliases = []
[extra]
id = 10990
[taxonomies]
categories = []
tags = []
+++

{{task}}
{{Wikipedia}}



The '''haversine formula''' is an equation important in navigation, giving great-circle distances between two points on a sphere from their longitudes and latitudes. 

It is a special case of a more general formula  in spherical trigonometry, the '''law of haversines''', relating the sides and angles of spherical "triangles".


;Task:
Implement a great-circle distance function, or use a library function, 
to show the great-circle distance between:
* Nashville International Airport (BNA)   in Nashville, TN, USA,   which is: 
   <big><big> '''N''' 36°7.2',   '''W''' 86°40.2'     (36.12,   -86.67) </big></big>          -and-
* Los Angeles International Airport (LAX)  in Los Angeles, CA, USA,   which is:
   <big><big> '''N''' 33°56.4',  '''W''' 118°24.0'    (33.94,  -118.40) </big></big>  




```txt

User Kaimbridge clarified on the Talk page:

 -- 6371.0 km is the authalic radius based on/extracted from surface area;
 -- 6372.8 km is an approximation of the radius of the average circumference
    (i.e., the average great-elliptic or great-circle radius), where the
     boundaries are the meridian (6367.45 km) and the equator (6378.14 km).

Using either of these values results, of course, in differing distances:

 6371.0 km -> 2886.44444283798329974715782394574671655 km;
 6372.8 km -> 2887.25995060711033944886005029688505340 km;
 (results extended for accuracy check:  Given that the radii are only
  approximations anyways, .01' ≈ 1.0621333 km and .001" ≈ .00177 km,
  practical precision required is certainly no greater than about
  .0000001——i.e., .1 mm!)

As distances are segments of great circles/circumferences, it is
recommended that the latter value (r = 6372.8 km) be used (which
most of the given solutions have already adopted, anyways). 

```


Most of the examples below adopted Kaimbridge's recommended value of
6372.8 km for the earth radius.  However, the derivation of this
[http://math.wikia.com/wiki/Ellipsoidal_quadratic_mean_radius ellipsoidal quadratic mean radius]
is wrong (the averaging over azimuth is biased).  When applying these
examples in real applications, it is better to use the
[https://en.wikipedia.org/wiki/Earth_radius#Mean_radius mean earth radius],
6371 km.  This value is recommended by the International Union of
Geodesy and Geophysics and it minimizes the RMS relative error between the
great circle and geodesic distance.





## ABAP


```abap

  DATA: X1 TYPE F, Y1 TYPE F,
        X2 TYPE F, Y2 TYPE F, YD TYPE F,
        PI TYPE F,
        PI_180 TYPE F,
        MINUS_1 TYPE F VALUE '-1'.

PI     = ACOS( MINUS_1 ).
PI_180 = PI / 180.

LATITUDE1 = 36,12 . LONGITUDE1 = -86,67 .
LATITUDE2 = 33,94 . LONGITUDE2 = -118,4 .

  X1 = LATITUDE1  * PI_180.
  Y1 = LONGITUDE1 * PI_180.
  X2 = LATITUDE2  * PI_180.
  Y2 = LONGITUDE2 * PI_180.
  YD = Y2 - Y1.

  DISTANCE = 20000 / PI *
    ACOS( SIN( X1 ) * SIN( X2 ) + COS( X1 ) * COS( X2 ) * COS( YD ) ).

WRITE : 'Distance between given points = ' , distance , 'km .' .

```


{{out}}

```txt

Distance between given points = 2.884,2687 km . 

```



## Ada


```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Haversine_Formula is

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Long_Float); use Math;

   -- Compute great circle distance, given latitude and longitude of two points, in radians
   function Great_Circle_Distance (lat1, long1, lat2, long2 : Long_Float) return Long_Float is
      Earth_Radius : constant := 6371.0; -- in kilometers
      a : Long_Float := Sin (0.5 * (lat2 - lat1));
      b : Long_Float := Sin (0.5 * (long2 - long1));
   begin
      return 2.0 * Earth_Radius * ArcSin (Sqrt (a * a + Cos (lat1) * Cos (lat2) * b * b));
   end Great_Circle_Distance;

   -- convert degrees, minutes and seconds to radians
   function DMS_To_Radians (Deg, Min, Sec : Long_Float := 0.0) return Long_Float is
      Pi_Over_180 : constant := 0.017453_292519_943295_769236_907684_886127;
   begin
      return (Deg + Min/60.0 + Sec/3600.0) * Pi_Over_180;
   end DMS_To_Radians;

begin
   Put_Line("Distance in kilometers between BNA and LAX");
   Put (Great_Circle_Distance (
         DMS_To_Radians (36.0, 7.2), DMS_To_Radians (86.0, 40.2),       -- Nashville International Airport (BNA)
         DMS_To_Radians (33.0, 56.4), DMS_To_Radians (118.0, 24.0)),    -- Los Angeles International Airport (LAX)
      Aft=>3, Exp=>0);
end Haversine_Formula;
```



## ALGOL 68

{{trans|C}}
{{works with|ALGOL 68|Revision 1.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.3.5 algol68g-2.3.5].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: Haversine_formula.a68'''
```algol68
#!/usr/local/bin/a68g --script #

REAL r = 20 000/pi + 6.6 # km #,
     to rad = pi/180;

PROC dist = (REAL th1 deg, ph1 deg, th2 deg, ph2 deg)REAL:
(
        REAL ph1 = (ph1 deg - ph2 deg) * to rad,
             th1 = th1 deg * to rad, th2 = th2 deg * to rad,

             dz = sin(th1) - sin(th2),
             dx = cos(ph1) * cos(th1) - cos(th2),
             dy = sin(ph1) * cos(th1);
        arc sin(sqrt(dx * dx + dy * dy + dz * dz) / 2) * 2 * r
);

main:
(
        REAL d = dist(36.12, -86.67, 33.94, -118.4);
        # Americans don't know kilometers #
        printf(($"dist: "g(0,1)" km ("g(0,1)" mi.)"l$, d, d / 1.609344))
)
```

{{out}}

```txt

dist: 2887.3 km (1794.1 mi.)

```



## AMPL


```AMPL

set location;
set geo;

param coord{i in location, j in geo};
param dist{i in location, j in location};

data;

set location := BNA LAX;
set geo := LAT LON;

param coord:
               LAT      LON :=
      BNA    36.12   -86.67
      LAX    33.94   -118.4
;

let dist['BNA','LAX'] := 2 * 6372.8 * asin (sqrt(sin(atan(1)/45*(coord['LAX','LAT']-coord['BNA','LAT'])/2)^2 + cos(atan(1)/45*coord['BNA','LAT']) * cos(atan(1)/45*coord['LAX','LAT']) * sin(atan(1)/45*(coord['LAX','LON'] - coord
['BNA','LON'])/2)^2));

printf "The distance between the two points is approximately %f km.\n", dist['BNA','LAX'];

```

{{out}}

```txt

The distance between the two points is approximately 2887.259951 km.

```



## APL


```apl
r←6371
hf←{(p q)←○⍺ ⍵÷180 ⋄ 2×r×¯1○(+/(2*⍨1○(p-q)÷2)×1(×/2○⊃¨p q))*÷2}
36.12 ¯86.67 hf 33.94 ¯118.40
```

{{out}}

```txt
2886.44
```



## ATS


```ATS

#include
"share/atspre_staload.hats"

staload "libc/SATS/math.sats"
staload _ = "libc/DATS/math.dats"
staload "libc/SATS/stdio.sats"
staload "libc/SATS/stdlib.sats"

#define R 6372.8
#define TO_RAD (3.1415926536 / 180)

typedef d = double

fun
dist
(
  th1: d, ph1: d, th2: d, ph2: d
) : d = let
  val ph1 = ph1 - ph2
  val ph1 = TO_RAD * ph1
  val th1 = TO_RAD * th1
  val th2 = TO_RAD * th2
  val dz = sin(th1) - sin(th2)
  val dx = cos(ph1) * cos(th1) - cos(th2)
  val dy = sin(ph1) * cos(th1)
in
  asin(sqrt(dx*dx + dy*dy + dz*dz)/2)*2*R
end // end of [dist]

implement
main0((*void*)) = let
  val d = dist(36.12, ~86.67, 33.94, ~118.4);
  /* Americans don't know kilometers */
in
  $extfcall(void, "printf", "dist: %.1f km (%.1f mi.)\n", d, d / 1.609344)
end // end of [main0]

```


{{out}}

```txt

dist: 2887.3 km (1794.1 mi.)

```



## AutoHotkey


```AutoHotkey
MsgBox, % GreatCircleDist(36.12, 33.94, -86.67, -118.40, 6372.8, "km")

GreatCircleDist(La1, La2, Lo1, Lo2, R, U) {
	return, 2 * R * ASin(Sqrt(Hs(Rad(La2 - La1)) + Cos(Rad(La1)) * Cos(Rad(La2)) * Hs(Rad(Lo2 - Lo1)))) A_Space U
}

Hs(n) {
	return, (1 - Cos(n)) / 2
}

Rad(Deg) {
	return, Deg * 4 * ATan(1) / 180
}
```

{{out}}

```txt
2887.259951 km
```



## AWK


```AWK

# syntax: GAWK -f HAVERSINE_FORMULA.AWK
# converted from Python
BEGIN {
    distance(36.12,-86.67,33.94,-118.40) # BNA to LAX
    exit(0)
}
function distance(lat1,lon1,lat2,lon2,  a,c,dlat,dlon) {
    dlat = radians(lat2-lat1)
    dlon = radians(lon2-lon1)
    lat1 = radians(lat1)
    lat2 = radians(lat2)
    a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
    c = 2 * atan2(sqrt(a),sqrt(1-a))
    printf("distance: %.4f km\n",6372.8 * c)
}
function radians(degree) { # degrees to radians
    return degree * (3.1415926 / 180.)
}

```

{{out}}

```txt

distance: 2887.2599 km

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
Uses BBC BASIC's '''MOD(array())''' function which calculates 
the square-root of the sum of the squares of the elements of an array.

```bbcbasic
      PRINT "Distance = " ; FNhaversine(36.12, -86.67, 33.94, -118.4) " km"
      END
      
      DEF FNhaversine(n1, e1, n2, e2)
      LOCAL d() : DIM d(2)
      d() = COSRAD(e1-e2) * COSRAD(n1) - COSRAD(n2), \
      \     SINRAD(e1-e2) * COSRAD(n1), \
      \     SINRAD(n1) - SINRAD(n2)
      = ASN(MOD(d()) / 2) * 6372.8 * 2
```

{{out}}

```txt

Distance = 2887.25995 km

```



## C


```c>#include <stdio.h

#include <stdlib.h>
#include <math.h>

#define R 6371
#define TO_RAD (3.1415926536 / 180)
double dist(double th1, double ph1, double th2, double ph2)
{
	double dx, dy, dz;
	ph1 -= ph2;
	ph1 *= TO_RAD, th1 *= TO_RAD, th2 *= TO_RAD;

	dz = sin(th1) - sin(th2);
	dx = cos(ph1) * cos(th1) - cos(th2);
	dy = sin(ph1) * cos(th1);
	return asin(sqrt(dx * dx + dy * dy + dz * dz) / 2) * 2 * R;
}

int main()
{
	double d = dist(36.12, -86.67, 33.94, -118.4);
	/* Americans don't know kilometers */
	printf("dist: %.1f km (%.1f mi.)\n", d, d / 1.609344);

	return 0;
}
```



## C++


```cpp

#define _USE_MATH_DEFINES

#include <math.h>
#include <iostream>

const static double EarthRadiusKm = 6372.8;

inline double DegreeToRadian(double angle)
{
	return M_PI * angle / 180.0;
}

class Coordinate
{
public:
	Coordinate(double latitude ,double longitude):myLatitude(latitude), myLongitude(longitude)
	{}

	double Latitude() const
	{
		return myLatitude;
	}

	double Longitude() const
	{
		return myLongitude;
	}

private:

	double myLatitude;
	double myLongitude;
};

double HaversineDistance(const Coordinate& p1, const Coordinate& p2)
{
	double latRad1 = DegreeToRadian(p1.Latitude());
	double latRad2 = DegreeToRadian(p2.Latitude());
	double lonRad1 = DegreeToRadian(p1.Longitude());
	double lonRad2 = DegreeToRadian(p2.Longitude());

	double diffLa = latRad2 - latRad1;
	double doffLo = lonRad2 - lonRad1;

	double computation = asin(sqrt(sin(diffLa / 2) * sin(diffLa / 2) + cos(latRad1) * cos(latRad2) * sin(doffLo / 2) * sin(doffLo / 2)));
	return 2 * EarthRadiusKm * computation;
}

int main()
{
	Coordinate c1(36.12, -86.67);
	Coordinate c2(33.94, -118.4);

	std::cout << "Distance = " << HaversineDistance(c1, c2) << std::endl;
	return 0;
}

```


=={{header|c sharp|C#}}==
{{trans|Groovy}}

```csharp
public static class Haversine {
  public static double calculate(double lat1, double lon1, double lat2, double lon2) {
    var R = 6372.8; // In kilometers
    var dLat = toRadians(lat2 - lat1);
    var dLon = toRadians(lon2 - lon1);
    lat1 = toRadians(lat1);
    lat2 = toRadians(lat2);
   
    var a = Math.Sin(dLat / 2) * Math.Sin(dLat / 2) + Math.Sin(dLon / 2) * Math.Sin(dLon / 2) * Math.Cos(lat1) * Math.Cos(lat2);
    var c = 2 * Math.Asin(Math.Sqrt(a));
    return R * 2 * Math.Asin(Math.Sqrt(a));
  }
  
  public static double toRadians(double angle) {
    return Math.PI * angle / 180.0;
  }
}

void Main() {
  Console.WriteLine(String.Format("The distance between coordinates {0},{1} and {2},{3} is: {4}", 36.12, -86.67, 33.94, -118.40, Haversine.calculate(36.12, -86.67, 33.94, -118.40)));
}

// Returns: The distance between coordinates 36.12,-86.67 and 33.94,-118.4 is: 2887.25995060711

```


=={{header|clojure|Clojure}}==
{{trans|Java}}

```clojure

(defn haversine
  [{lon1 :longitude lat1 :latitude} {lon2 :longitude lat2 :latitude}]
  (let [R 6372.8 ; kilometers
        dlat (Math/toRadians (- lat2 lat1))
        dlon (Math/toRadians (- lon2 lon1))
        lat1 (Math/toRadians lat1)
        lat2 (Math/toRadians lat2)
        a (+ (* (Math/sin (/ dlat 2)) (Math/sin (/ dlat 2))) (* (Math/sin (/ dlon 2)) (Math/sin (/ dlon 2)) (Math/cos lat1) (Math/cos lat2)))]
    (* R 2 (Math/asin (Math/sqrt a)))))

(haversine {:latitude 36.12 :longitude -86.67} {:latitude 33.94 :longitude -118.40})
;=> 2887.2599506071106

```



## CoffeeScript

{{trans|JavaScript}}

```coffee
haversine = (args...) -> 
  R = 6372.8; # km
  radians = args.map (deg) -> deg/180.0 * Math.PI
  lat1 = radians[0]; lon1 = radians[1]; lat2 = radians[2]; lon2 = radians[3]
  dLat = lat2 - lat1
  dLon = lon2 - lon1
  a = Math.sin(dLat / 2) * Math.sin(dLat / 2) + Math.sin(dLon / 2) * Math.sin(dLon / 2) * Math.cos(lat1) * Math.cos(lat2)
  R * 2 * Math.asin(Math.sqrt(a))

console.log haversine(36.12, -86.67, 33.94, -118.40)
```

{{out}}

```txt
2887.2599506071124
```



## Common Lisp


```lisp
(defparameter *earth-radius* 6372.8)

(defparameter *rad-conv* (/ pi 180))

(defun deg->rad (x)
  (* x *rad-conv*))

(defun haversine (x)
  (expt (sin (/ x 2)) 2))

(defun dist-rad (lat1 lng1 lat2 lng2)
  (let* ((hlat (haversine (- lat2 lat1)))
         (hlng (haversine (- lng2 lng1)))
         (root (sqrt (+ hlat (* (cos lat1) (cos lat2) hlng)))))
    (* 2 *earth-radius* (asin root))))

(defun dist-deg (lat1 lng1 lat2 lng2)
  (dist-rad (deg->rad lat1)
            (deg->rad lng1)
            (deg->rad lat2)
            (deg->rad lng2)))
```

{{out}}

```txt
CL-USER> (format t "~%The distance between BNA and LAX is about ~$ km.~%" 
		 (dist-deg 36.12 -86.67 33.94 -118.40))

The distance between BNA and LAX is about 2887.26 km.
```



## Crystal

{{trans|Python}}

```ruby
include Math
 
def haversine(lat1, lon1, lat2, lon2)
    r = 6372.8        # Earth radius in kilometers
    deg2rad = PI/180  # convert degress to radians
 
    dLat = (lat2 - lat1) * deg2rad
    dLon = (lon2 - lon1) * deg2rad
    lat1 = lat1 * deg2rad
    lat2 = lat2 * deg2rad
 
    a = sin(dLat / 2)**2 + cos(lat1) * cos(lat2) * sin(dLon / 2)**2
    c = 2 * asin(sqrt(a))
    r * c
end

puts "distance is #{haversine(36.12, -86.67, 33.94, -118.40)} km "

```

{{out}}

```txt

distance is 2887.2599506071106 km 

```



## D


```d
import std.stdio, std.math;

real haversineDistance(in real dth1, in real dph1,
                       in real dth2, in real dph2)
pure nothrow @nogc {
    enum real R = 6371;
    enum real TO_RAD = PI / 180;

    alias imr = immutable real;
    imr ph1d = dph1 - dph2;
    imr ph1 = ph1d * TO_RAD;
    imr th1 = dth1 * TO_RAD;
    imr th2 = dth2 * TO_RAD;

    imr dz = th1.sin - th2.sin;
    imr dx = ph1.cos * th1.cos - th2.cos;
    imr dy = ph1.sin * th1.cos;
    return asin(sqrt(dx ^^ 2 + dy ^^ 2 + dz ^^ 2) / 2) * 2 * R;
}

void main() {
    writefln("Haversine distance: %.1f km",
             haversineDistance(36.12, -86.67, 33.94, -118.4));
}
```

{{out}}

```txt
Haversine distance: 2887.3 km
```



### Alternative Version

An alternate direct implementation of the haversine formula as shown at [[wp:Haversine formula|wikipedia]]. The same length, but perhaps a little more clear about what is being done.


```d
import std.stdio, std.math;

real toRad(in real degrees) pure nothrow @safe @nogc {
    return degrees * PI / 180;
}

real haversin(in real theta) pure nothrow @safe @nogc {
    return (1 - theta.cos) / 2;
}

real greatCircleDistance(in real lat1, in real lng1,
                         in real lat2, in real lng2,
                         in real radius)
pure nothrow @safe @nogc {
    immutable h = haversin(lat2.toRad - lat1.toRad) +
                  lat1.toRad.cos * lat2.toRad.cos *
                  haversin(lng2.toRad - lng1.toRad);
    return 2 * radius * h.sqrt.asin;
}

void main() {
    enum real earthRadius = 6372.8L; // Average earth radius.

    writefln("Great circle distance: %.1f km",
             greatCircleDistance(36.12, -86.67, 33.94, -118.4,
                                 earthRadius));
}
```

{{out}}

```txt
Great circle distance: 2887.3 km

```



## Dart

{{trans|Java}}

```dart
import 'dart:math';

class Haversine {
  static final R = 6372.8; // In kilometers

  static double haversine(double lat1, lon1, lat2, lon2) {
    double dLat = _toRadians(lat2 - lat1);
    double dLon = _toRadians(lon2 - lon1);
    lat1 = _toRadians(lat1);
    lat2 = _toRadians(lat2);
    double a = pow(sin(dLat / 2), 2) + pow(sin(dLon / 2), 2) * cos(lat1) * cos(lat2);
    double c = 2 * asin(sqrt(a));
    return R * c;
  }

  static double _toRadians(double degree) {
    return degree * pi / 180;
  }

  static void main() {
    print(haversine(36.12, -86.67, 33.94, -118.40));
  }
}

```

{{out}}

```txt
2887.2599506071106
```



## Delphi


```delphi
program HaversineDemo;
uses Math;

function HaversineDist(th1, ph1, th2, ph2:double):double;
const diameter = 2 * 6372.8;
var   dx, dy, dz:double;
begin
  ph1    := degtorad(ph1 - ph2);
  th1    := degtorad(th1);
  th2    := degtorad(th2);

  dz     := sin(th1) - sin(th2);
  dx     := cos(ph1) * cos(th1) - cos(th2);
  dy     := sin(ph1) * cos(th1);
  Result := arcsin(sqrt(sqr(dx) + sqr(dy) + sqr(dz)) / 2) * diameter;
end;

begin
  Writeln('Haversine distance: ', HaversineDist(36.12, -86.67, 33.94, -118.4):7:2, ' km.');
end.
```

{{out}}

```txt
Haversine distance: 2887.26 km.
```


## Elena

ELENA 4.x:

```elena
import extensions;
import system'math;
 
Haversine(lat1,lon1,lat2,lon2)
{
    var R := 6372.8r;
    var dLat := (lat2 - lat1).Radian;
    var dLon := (lon2 - lon1).Radian;
 
    var dLat1 := lat1.Radian;
    var dLat2 := lat2.Radian;
 
    var a := (dLat / 2).sin() * (dLat / 2).sin() + (dLon / 2).sin() * (dLon / 2).sin() * dLat1.cos() * dLat2.cos();
 
    ^ R * 2 * a.sqrt().arcsin()
}
 
public program()
{
    console.printLineFormatted("The distance between coordinates {0},{1} and {2},{3} is: {4}", 36.12r, -86.67r, 33.94r, -118.40r, 
        Haversine(36.12r, -86.67r, 33.94r, -118.40r))
}
```

{{out}}

```txt

The distance between coordinates 36.12,-86.67 and 33.94,-118.4 is: 2887.259950607

```



## Elixir


```elixir
defmodule Haversine do
  @v  :math.pi / 180
  @r  6372.8            # km for the earth radius
  def distance({lat1, long1}, {lat2, long2}) do
    dlat  = :math.sin((lat2 - lat1) * @v / 2)
    dlong = :math.sin((long2 - long1) * @v / 2)
    a = dlat * dlat + dlong * dlong * :math.cos(lat1 * @v) * :math.cos(lat2 * @v)
    @r * 2 * :math.asin(:math.sqrt(a))
  end
end

bna = {36.12,  -86.67}
lax = {33.94, -118.40}
IO.puts Haversine.distance(bna, lax)
```


{{out}}

```txt

2887.2599506071106

```



## Elm



```elm
haversine : ( Float, Float ) -> ( Float, Float ) -> Float
haversine ( lat1, lon1 ) ( lat2, lon2 ) =
    let
        r =
            6372.8

        dLat =
            degrees (lat2 - lat1)

        dLon =
            degrees (lon2 - lon1)

        a =
            (sin (dLat / 2))
                ^ 2
                + (sin (dLon / 2))
                ^ 2
                * cos (degrees lat1)
                * cos (degrees lat2)
    in
        r * 2 * asin (sqrt a)

view =
    Html.div []
      [ Html.text (toString (haversine ( 36.12, -86.67 ) ( 33.94, -118.4 )))
      ]

```


{{out}}

```txt
2887.2599506071106

```



## Erlang


```erlang
% Implementer by Arjun Sunel
-module(haversine).
-export([main/0]).

main() ->
	haversine(36.12, -86.67, 33.94, -118.40).

haversine(Lat1, Long1, Lat2, Long2) ->
	V 	         =   math:pi()/180,
	R 		 =   6372.8, 	% In kilometers
	Diff_Lat 	 =   (Lat2 - Lat1)*V ,	
	Diff_Long	 =   (Long2 - Long1)*V,	
	NLat 		 =   Lat1*V,
	NLong 		 =   Lat2*V,
	A 		 =   math:sin(Diff_Lat/2) * math:sin(Diff_Lat/2) + math:sin(Diff_Long/2) * math:sin(Diff_Long/2) * math:cos(NLat) * math:cos(NLong),
	C 		 =   2 * math:asin(math:sqrt(A)),
	R*C.

```

{{out}}

```txt
2887.2599506071106

```



## ERRE


```ERRE
% Implemented by Claudio Larini

PROGRAM HAVERSINE_DEMO

!$DOUBLE

CONST DIAMETER=12745.6

FUNCTION DEG2RAD(X)
    DEG2RAD=X*π/180
END FUNCTION

FUNCTION RAD2DEG(X)
    RAD2DEG=X*180/π
END FUNCTION

PROCEDURE HAVERSINE_DIST(TH1,PH1,TH2,PH2->RES)
    LOCAL DX,DY,DZ
    PH1=DEG2RAD(PH1-PH2)
    TH1=DEG2RAD(TH1)
    TH2=DEG2RAD(TH2)
    DZ=SIN(TH1)-SIN(TH2)
    DX=COS(PH1)*COS(TH1)-COS(TH2)
    DY=SIN(PH1)*COS(TH1)
    RES=ASN(SQR(DX^2+DY^2+DZ^2)/2)*DIAMETER
END PROCEDURE

BEGIN
    HAVERSINE_DIST(36.12,-86.67,33.94,-118.4->RES)
    PRINT("HAVERSINE DISTANCE: ";RES;" KM.")
END PROGRAM

```

Using double-precision variables output is 2887.260209071741 km, while using single-precision variable output is 2887.261 Km.


## Euler Math Toolbox


Euler has a package for spherical geometry, which is used in the following code. The distances are then computed with the average radius between the two positions. Overwriting the rearth function with the given value yields the known result.


```txt

>load spherical
 Spherical functions for Euler. 
>TNA=[rad(36,7.2),-rad(86,40.2)];
>LAX=[rad(33,56.4),-rad(118,24)];
>esdist(TNA,LAX)->km
 2886.48817482
>type esdist
 function esdist (frompos: vector, topos: vector)
     r1=rearth(frompos[1]); 
     r2=rearth(topos[1]);
     xfrom=spoint(frompos)*r1; 
     xto=spoint(topos)*r2;
     delta=xto-xfrom;
     return asin(norm(delta)/(r1+r2))*(r1+r2);
 endfunction
>function overwrite rearth (x) := 6372.8*km$
>esdist(TNA,LAX)->km
 2887.25995061

```


=={{header|F_Sharp|F#}}==
{{trans|Go}} using units of measure

```fsharp
open System

[<Measure>] type deg
[<Measure>] type rad
[<Measure>] type km

let haversine (θ: float<rad>) = 0.5 * (1.0 - Math.Cos(θ/1.0<rad>))

let radPerDeg =  (Math.PI / 180.0) * 1.0<rad/deg>

type pos(latitude: float<deg>, longitude: float<deg>) =
    member this.φ = latitude * radPerDeg
    member this.ψ = longitude * radPerDeg

let rEarth = 6372.8<km>

let hsDist (p1: pos) (p2: pos) =
    2.0 * rEarth *
        Math.Asin(Math.Sqrt(haversine(p2.φ - p1.φ)+
                    Math.Cos(p1.φ/1.0<rad>)*Math.Cos(p2.φ/1.0<rad>)*haversine(p2.ψ - p1.ψ)))

[<EntryPoint>]
let main argv =
    printfn "%A" (hsDist (pos(36.12<deg>, -86.67<deg>)) (pos(33.94<deg>, -118.40<deg>)))
    0
```

{{out}}

```txt
2887.259951
```



## Factor

{{trans|J}}

```factor
USING: arrays kernel math math.constants math.functions math.vectors sequences ;

: haversin ( x -- y ) cos 1 swap - 2 / ;
: haversininv ( y -- x ) 2 * 1 swap - acos ;
: haversineDist ( as bs -- d )
[ [ 180 / pi * ] map ] bi@
  [ [ swap - haversin ] 2map ]
  [ [ first cos ] bi@ * 1 swap 2array ]
  2bi
v.
haversininv R_earth * ;
```


```factor
( scratchpad ) { 36.12 -86.67 } { 33.94 -118.4 } haversineDist .
2887.259950607113
```



## FBSL

Based on the Fortran and Groovy versions.

```qbasic
#APPTYPE CONSOLE

PRINT "Distance = ", Haversine(36.12, -86.67, 33.94, -118.4), " km"
PAUSE

FUNCTION Haversine(DegLat1 AS DOUBLE, DegLon1 AS DOUBLE, DegLat2 AS DOUBLE, DegLon2 AS DOUBLE) AS DOUBLE
    CONST radius = 6372.8
    DIM dLat AS DOUBLE = D2R(DegLat2 - DegLat1)
    DIM dLon AS DOUBLE = D2R(DegLon2 - DegLon1)
    DIM lat1 AS DOUBLE = D2R(DegLat1)
    DIM lat2 AS DOUBLE = D2R(DegLat2)
    DIM a AS DOUBLE = SIN(dLat / 2) * SIN(dLat / 2) + SIN(dLon / 2) * SIN(dLon / 2) * COS(lat1) * COS(lat2)
    DIM c AS DOUBLE = 2 * ASIN(SQRT(a))
    RETURN radius * c
END FUNCTION

```

{{out}}
 Distance = 2887.25995060711 km
 Press any key to continue...


## Forth


```forth>: s>f s>d d
f ;
: deg>rad 174532925199433e-16 f* ;
: difference f- deg>rad 2 s>f f/ fsin fdup f* ;

: haversine                            ( lat1 lon1 lat2 lon2 -- haversine)
  frot difference                      ( lat1 lat2 dLon^2)
  frot frot fover fover                ( dLon^2 lat1 lat2 lat1 lat2)
  fswap difference                     ( dLon^2 lat1 lat2 dLat^2)
  fswap deg>rad fcos                   ( dLon^2 lat1 dLat^2 lat2)
  frot  deg>rad fcos f*                ( dLon^2 dLat2 lat1*lat2)
  frot  f* f+                          ( lat1*lat2*dLon^2+dLat^2)
  fsqrt fasin 127456 s>f f* 10 s>f f/  ( haversine)
;

36.12e -86.67e 33.94e -118.40e haversine cr f.
```

{{out}}

```txt

2887.25995060711

```



## Fortran


```Fortran

program example
implicit none
real :: d

d = haversine(36.12,-86.67,33.94,-118.40) ! BNA to LAX
print '(A,F9.4,A)', 'distance: ',d,' km' ! distance: 2887.2600 km

contains

      function to_radian(degree) result(rad)
          ! degrees to radians
          real,intent(in) :: degree
          real, parameter :: deg_to_rad = atan(1.0)/45 ! exploit intrinsic atan to generate pi/180 runtime constant
          real :: rad

          rad = degree*deg_to_rad
      end function to_radian
 
      function haversine(deglat1,deglon1,deglat2,deglon2) result (dist)
          ! great circle distance -- adapted from Matlab 
          real,intent(in) :: deglat1,deglon1,deglat2,deglon2
          real :: a,c,dist,dlat,dlon,lat1,lat2
          real,parameter :: radius = 6372.8 

          dlat = to_radian(deglat2-deglat1)
          dlon = to_radian(deglon2-deglon1)
          lat1 = to_radian(deglat1)
          lat2 = to_radian(deglat2)
          a = (sin(dlat/2))**2 + cos(lat1)*cos(lat2)*(sin(dlon/2))**2
          c = 2*asin(sqrt(a))
          dist = radius*c
      end function haversine

end program example

```


## FreeBASIC


```freebasic
' version 09-10-2016
' compile with: fbc -s console

' Nashville International Airport (BNA) in Nashville, TN, USA,
' N 36°07.2',  W  86°40.2' (36.12,  -86.67)
' Los Angeles International Airport (LAX) in Los Angeles, CA, USA,
' N 33°56.4', W 118°24.0'  (33.94, -118.40).
' 6372.8 km is an approximation of the radius of the average circumference

#Define Pi Atn(1) * 4        ' define Pi = 3.1415..
#Define deg2rad Pi / 180     ' define deg to rad 0.01745..
#Define earth_radius 6372.8  ' earth radius in km.

Function Haversine(lat1 As Double, long1 As Double, lat2 As Double, _
                                long2 As Double , radius As Double) As Double

  Dim As Double d_long = deg2rad * (long1 - long2)
  Dim As Double theta1 = deg2rad * lat1
  Dim As Double theta2 = deg2rad * lat2
  Dim As Double dx = Cos(d_long) * Cos(theta1) - Cos(theta2)
  Dim As Double dy = Sin(d_long) * Cos(theta1)
  Dim As Double dz = Sin(theta1) - Sin(theta2)
  Return Asin(Sqr(dx*dx + dy*dy + dz*dz) / 2) * radius * 2

End Function

Print
Print " Haversine distance between BNA and LAX = "; _
      Haversine(36.12, -86.67, 33.94, -118.4, earth_radius); " km."


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
 Haversine distance between BNA and LAX =  2887.259950607111 km.
```



## Free Pascal

Here is a Free Pascal version, works in most Pascal dialects, but also note the Delphi entry that also works in Free Pascal.

```pascal
program HaversineDemo;
uses 
  Math;

function HaversineDistance(const lat1, lon1, lat2, lon2:double):double;inline;
const 
  rads = pi / 180;
  dia  = 2 * 6372.8;
begin
  HaversineDistance := dia * arcsin(sqrt(sqr(cos(rads * (lon1 - lon2)) * cos(rads * lat1)  
                     - cos(rads * lat2)) + sqr(sin(rads * (lon1 - lon2)) 
                     * cos(rads * lat1)) + sqr(sin(rads * lat1) - sin(rads * lat2))) / 2);
end;

begin
  Writeln('Haversine distance between BNA and LAX: ', HaversineDistance(36.12, -86.67, 33.94, -118.4):7:2, ' km.');
end.
```



## Frink


```frink

haversine[theta] := (1-cos[theta])/2

dist[lat1, long1, lat2, long2] := 2 earthradius arcsin[sqrt[haversine[lat2-lat1] + cos[lat1] cos[lat2] haversine[long2-long1]]]

d = dist[36.12 deg, -86.67 deg, 33.94 deg, -118.40 deg]
println[d-> "km"]

```


Note that physical constants like degrees, kilometers, and the average radius of the earth (as well as the polar and equatorial radii) are already known to Frink.  Also note that units of measure are tracked throughout all calculations, and results can be displayed in a huge number of units of distance (miles, km, furlongs, chains, feet, statutemiles, etc.) by changing the final <code>"km"</code> to something like <code>"miles"</code>.

However, Frink's library/sample program [http://futureboy.us/fsp/colorize.fsp?fileName=navigation.frink navigation.frink] (included in larger distributions) contains a much higher-precision calculation that uses ellipsoidal (not spherical) calculations to determine the distance on earth's geoid with far greater accuracy:


```frink

use navigation.frink

d = earthDistance[36.12 deg North, 86.67 deg West, 33.94 deg North, 118.40 deg West]
println[d-> "km"]

```



## FunL


```funl
import math.*

def haversin( theta ) = (1 - cos( theta ))/2

def radians( deg ) = deg Pi/180

def haversine( (lat1, lon1), (lat2, lon2) ) =
  R = 6372.8
  h = haversin( radians(lat2 - lat1) ) + cos( radians(lat1) ) cos( radians(lat2) ) haversin( radians(lon2 - lon1) )
  2R asin( sqrt(h) )

println( haversine((36.12, -86.67), (33.94, -118.40)) )
```


{{out}}


```txt

2887.259950607111

```



## FutureBasic

Note: The Haversine function returns an approximate theoretical value of the Great Circle Distance between two points because it does not factor the ellipsoidal shape of Earth -- fat in the middle from centrifugal force, and squashed at the ends. Navigators once relied on trigonometric functions like versine (versed sine) where angle A is 1-cos(A), and haversine (half versine) or ( 1-cos(A) ) / 2.
Also, the radius of the Earth varies, at least depending on who you talk to. Here's NASA's take on it: http://nssdc.gsfc.nasa.gov/planetary/factsheet/earthfact.html

Since it was trivial, this functions returns the distance in miles and kilometers. 

```futurebasic

include "ConsoleWindow"

local fn Haversine( lat1 as double, lon1 as double, lat2 as double, lon2 as double, miles as ^double, kilometers as ^double )
dim as double deg2rad, dLat, dLon, a, c, earth_radius_miles, earth_radius_kilometers

earth_radius_miles = 3959.0 // Radius of the Earth in miles
earth_radius_kilometers = 6372.8 // Radius of the Earth in kilometers
deg2rad = Pi / 180 // Pi is predefined in FutureBasic
  
dLat = deg2rad * ( lat2  - lat1 )  
dLon = deg2rad * ( lon2 - lon1 )  
a = sin( dLat / 2 ) * sin( dLat / 2 ) + cos( deg2rad * lat1 ) * cos( deg2rad * lat2 ) * sin( dLon / 2 ) * sin( dLon / 2 )  
c = 2 * asin( sqr(a) )  

miles.nil# =  earth_radius_miles * c
kilometers.nil# = earth_radius_kilometers * c
end fn

dim as double miles, kilometers
fn Haversine( 36.12, -86.67, 33.94, -118.4, @miles, @kilometers )

print "Distance in miles between BNA and LAX: "; using "####.####"; miles; " miles."
print "Distance in kilometers between BNA LAX: "; using "####.####"; kilometers; " km."


```

Output:

```txt

Distance in miles between BNA and LAX: 1793.6640 miles.
Distance in kilometers between BNA LAX: 2887.2600 km.

```



## Go


```go
package main

import (
    "fmt"
    "math"
)

func haversine(θ float64) float64 {
    return .5 * (1 - math.Cos(θ))
}

type pos struct {
    φ float64 // latitude, radians
    ψ float64 // longitude, radians
}

func degPos(lat, lon float64) pos {
    return pos{lat * math.Pi / 180, lon * math.Pi / 180}
}

const rEarth = 6372.8 // km

func hsDist(p1, p2 pos) float64 {
    return 2 * rEarth * math.Asin(math.Sqrt(haversine(p2.φ-p1.φ)+
        math.Cos(p1.φ)*math.Cos(p2.φ)*haversine(p2.ψ-p1.ψ)))
}

func main() {
    fmt.Println(hsDist(degPos(36.12, -86.67), degPos(33.94, -118.40)))
}
```

{{out}}

```txt

2887.2599506071097

```



## Groovy


```Groovy
def haversine(lat1, lon1, lat2, lon2) {
  def R = 6372.8
  // In kilometers
  def dLat = Math.toRadians(lat2 - lat1)
  def dLon = Math.toRadians(lon2 - lon1)
  lat1 = Math.toRadians(lat1)
  lat2 = Math.toRadians(lat2)

  def a = Math.sin(dLat / 2) * Math.sin(dLat / 2) + Math.sin(dLon / 2) * Math.sin(dLon / 2) * Math.cos(lat1) * Math.cos(lat2)
  def c = 2 * Math.asin(Math.sqrt(a))
  R * c
}

haversine(36.12, -86.67, 33.94, -118.40)

> 2887.25995060711
```



## Haskell


```Haskell
import Text.Printf
import Control.Arrow ((***))

-- The haversine of an angle.
haversine :: Float -> Float
haversine = (^ 2) . sin . (/ 2)

-- The approximate distance, in kilometers, between two points on Earth.
-- The latitude and longtitude are assumed to be in degrees.
earthDist :: (Float, Float) -> (Float, Float) -> Float
earthDist = distDeg 6371
  where
    distDeg radius p1 p2 = distRad radius (deg2rad p1) (deg2rad p2)
    distRad radius (lat1, lng1) (lat2, lng2) =
      (2 * radius) *
      asin
        (min
           1.0
           (sqrt $
            haversine (lat2 - lat1) +
            ((cos lat1 * cos lat2) * haversine (lng2 - lng1))))
    deg2rad = d2r *** d2r
      where
        d2r = (/ 180) . (pi *)

main :: IO ()
main =
  printf
    "The distance between BNA and LAX is about %0.f km.\n"
    (earthDist bna lax)
  where
    bna = (36.12, -86.67)
    lax = (33.94, -118.40)
```

{{Out}}

```txt
The distance between BNA and LAX is about 2886 km.
```


=={{header|Icon}} and {{header|Unicon}}==
{{trans|C}}

```Icon
link printf

procedure main()  #: Haversine formula  
   printf("BNA to LAX is %d km (%d miles)\n",
      d := gcdistance([36.12, -86.67],[33.94, -118.40]),d*3280/5280)  # with cute km2mi conversion
end

procedure gcdistance(a,b)
	a[2] -:= b[2]
   every (x := a|b)[i := 1 to 2] := dtor(x[i])
	dz := sin(a[1]) - sin(b[1])
	dx := cos(a[2]) * cos(a[1]) - cos(b[1])
	dy := sin(a[2]) * cos(a[1])
	return asin(sqrt(dx * dx + dy * dy + dz * dz) / 2) * 2 * 6371
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting] 

{{out}}

```txt
BNA to LAX is 2886 km (1793 miles)
```



## Idris

{{trans|Haskell}}

```Idris
module Main
 
-- The haversine of an angle.
hsin : Double -> Double
hsin t = let u = sin (t/2) in u*u
 
-- The distance between two points, given by latitude and longtitude, on a
-- circle.  The points are specified in radians.
distRad : Double -> (Double, Double) -> (Double, Double) -> Double
distRad radius (lat1, lng1) (lat2, lng2) =
  let hlat = hsin (lat2 - lat1)
      hlng = hsin (lng2 - lng1)
      root = sqrt (hlat + cos lat1 * cos lat2 * hlng)
  in 2 * radius * asin (min 1.0 root)
 
-- The distance between two points, given by latitude and longtitude, on a
-- circle.  The points are specified in degrees.
distDeg : Double -> (Double, Double) -> (Double, Double) -> Double 
distDeg radius p1 p2 = distRad radius (deg2rad p1) (deg2rad p2)
  where 
        d2r : Double -> Double
        d2r t = t * pi / 180 
        deg2rad (t, u) = (d2r t, d2r u)
 
-- The approximate distance, in kilometers, between two points on Earth.  
-- The latitude and longtitude are assumed to be in degrees.
earthDist : (Double, Double) -> (Double, Double) -> Double
earthDist = distDeg 6372.8

main : IO () 
main = putStrLn $ "The distance between BNA and LAX is about " ++ show (floor dst) ++ " km."
 where 
      bna : (Double, Double)
      bna = (36.12,  -86.67)

      lax : (Double, Double)
      lax = (33.94, -118.40)

      dst : Double
      dst = earthDist bna lax

```

{{out}}

```txt
The distance between BNA and LAX is about 2887 km.
```



## J

'''Solution:'''

```j
require 'trig'
haversin=: 0.5 * 1 - cos
Rearth=: 6372.8
haversineDist=: Rearth * haversin^:_1@((1 , *&(cos@{.)) +/ .* [: haversin -)&rfd

```

Note: J derives the inverse haversin ( <code>haversin^:_1</code> ) 
from the definition of haversin.

'''Example Use:'''

```j
   36.12 _86.67 haversineDist 33.94 _118.4
2887.26
```



## Java

{{trans|Groovy}}

```java
public class Haversine {
    public static final double R = 6372.8; // In kilometers
    public static double haversine(double lat1, double lon1, double lat2, double lon2) {
        double dLat = Math.toRadians(lat2 - lat1);
        double dLon = Math.toRadians(lon2 - lon1);
        lat1 = Math.toRadians(lat1);
        lat2 = Math.toRadians(lat2);

        double a = Math.pow(Math.sin(dLat / 2),2) + Math.pow(Math.sin(dLon / 2),2) * Math.cos(lat1) * Math.cos(lat2);
        double c = 2 * Math.asin(Math.sqrt(a));
        return R * c;
    }
    public static void main(String[] args) {
        System.out.println(haversine(36.12, -86.67, 33.94, -118.40));
    }
}
```

{{out}}

```txt
2887.2599506071106
```



## JavaScript


### ES5

{{trans|Java}}

```javascript
function haversine() {
       var radians = Array.prototype.map.call(arguments, function(deg) { return deg/180.0 * Math.PI; });
       var lat1 = radians[0], lon1 = radians[1], lat2 = radians[2], lon2 = radians[3];
       var R = 6372.8; // km
       var dLat = lat2 - lat1;
       var dLon = lon2 - lon1;
       var a = Math.sin(dLat / 2) * Math.sin(dLat /2) + Math.sin(dLon / 2) * Math.sin(dLon /2) * Math.cos(lat1) * Math.cos(lat2);
       var c = 2 * Math.asin(Math.sqrt(a));
       return R * c;
}
console.log(haversine(36.12, -86.67, 33.94, -118.40));
```

{{out}}

```txt
2887.2599506071124
```



### ES6


```JavaScript
((x, y) => {
    'use strict';

    // haversine :: (Num, Num) -> (Num, Num) -> Num
    const haversine = ([lat1, lon1], [lat2, lon2]) => {
        // Math lib function names
        const [pi, asin, sin, cos, sqrt, pow, round] = [
            'PI', 'asin', 'sin', 'cos', 'sqrt', 'pow', 'round'
        ]
        .map(k => Math[k]),

            // degrees as radians
            [rlat1, rlat2, rlon1, rlon2] = [lat1, lat2, lon1, lon2]
            .map(x => x / 180 * pi),

            dLat = rlat2 - rlat1,
            dLon = rlon2 - rlon1,
            radius = 6372.8; // km

        // km
        return round(
            radius * 2 * asin(
                sqrt(
                    pow(sin(dLat / 2), 2) +
                    pow(sin(dLon / 2), 2) *
                    cos(rlat1) * cos(rlat2)
                )
            ) * 100
        ) / 100;
    };

    // TEST
    return haversine(x, y);

    // --> 2887.26

})([36.12, -86.67], [33.94, -118.40]);
```

{{Out}}

```txt
2887.26
```



## jq


```jq
def haversine(lat1;lon1; lat2;lon2):
  def radians: . * (1|atan)/45;
  def sind: radians|sin;
  def cosd: radians|cos;
  def sq: . * .;

    (((lat2 - lat1)/2) | sind | sq) as $dlat
  | (((lon2 - lon1)/2) | sind | sq) as $dlon
  | 2 * 6372.8 * (( $dlat + (lat1|cosd) * (lat2|cosd) * $dlon ) | sqrt | asin) ;
```

'''Example''':
 haversine(36.12; -86.67; 33.94; -118.4)
 # 2887.2599506071106


## Jsish

From Javascript, ES5, except the ''arguments'' value is an Array in jsish, not an Object.

```javascript
/* Haversine formula, in Jsish */
function haversine() {
       var radians = arguments.map(function(deg) { return deg/180.0 * Math.PI; });
       var lat1 = radians[0], lon1 = radians[1], lat2 = radians[2], lon2 = radians[3];
       var R = 6372.8; // km
       var dLat = lat2 - lat1;
       var dLon = lon2 - lon1;
       var a = Math.sin(dLat / 2) * Math.sin(dLat /2) + Math.sin(dLon / 2) * Math.sin(dLon /2) * Math.cos(lat1) * Math.cos(lat2);
       var c = 2 * Math.asin(Math.sqrt(a));
       return R * c;
}

;haversine(36.12, -86.67, 33.94, -118.40);

/*
=!EXPECTSTART!=
haversine(36.12, -86.67, 33.94, -118.40) ==> 2887.259950607112
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u haversineFormula.jsi
[PASS] haversineFormula.jsi
```



## Julia

{{works with|Julia|0.6}}


```julia
haversine(lat1, lon1, lat2, lon2) =
    2 * 6372.8 * asin(sqrt(sind((lat2 - lat1) / 2) ^ 2 +
    cosd(lat1) * cosd(lat2) * sind((lon2 - lon1) / 2) ^ 2))

@show haversine(36.12, -86.67, 33.94, -118.4)
```


{{out}}

```txt
haversine(36.12, -86.67, 33.94, -118.4) = 2887.2599506071106
```



## Kotlin

{{trans|Groovy}}
Use Unicode characters.

```scala
import java.lang.Math.*

const val R = 6372.8 // in kilometers

fun haversine(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double {
    val λ1 = toRadians(lat1)
    val λ2 = toRadians(lat2)
    val Δλ = toRadians(lat2 - lat1)
    val Δφ = toRadians(lon2 - lon1)
    return 2 * R * asin(sqrt(pow(sin(Δλ / 2), 2.0) + pow(sin(Δφ / 2), 2.0) * cos(λ1) * cos(λ2)))
}

fun main(args: Array<String>) = println("result: " + haversine(36.12, -86.67, 33.94, -118.40))
```



## Liberty BASIC


```lb
print "Haversine distance: "; using( "####.###########", havDist( 36.12, -86.67, 33.94, -118.4)); " km."
end
function havDist( th1, ph1, th2, ph2)
  degtorad   = acs(-1)/180
  diameter   = 2 * 6372.8
    LgD      = degtorad  * (ph1 - ph2)
    th1      = degtorad  * th1
    th2      = degtorad  * th2
    dz       = sin( th1) - sin( th2)
    dx       = cos( LgD) * cos( th1) - cos( th2)
    dy       = sin( LgD) * cos( th1)
    havDist  = asn( ( dx^2 +dy^2 +dz^2)^0.5 /2) *diameter
end function
```


```txt
Haversine distance: 2887.25995060711  km.
```



## LiveCode


```LiveCode
function radians n
    return n * (3.1415926 / 180)
end radians

function haversine lat1, lng1, lat2, lng2
    local radiusEarth 
    local lat3, lng3
    local lat1Rad, lat2Rad, lat3Rad
    local lngRad1, lngRad2, lngRad3
    local haver
    put 6372.8 into radiusEarth
    put (lat2 - lat1) into lat3
    put (lng2 - lng1) into lng3
    put radians(lat1) into lat1Rad
    put radians(lat2) into lat2Rad
    put radians(lat3) into lat3Rad
    put radians(lng1) into lngRad1
    put radians(lng2) into lngRad2
    put radians(lng3) into lngRad3
    
    put (sin(lat3Rad/2.0)^2) + (cos(lat1Rad)) \
          * (cos(lat2Rad)) \
          * (sin(lngRad3/2.0)^2) \
          into haver 
    return (radiusEarth * (2.0 * asin(sqrt(haver))))
    
end haversine
```

Test

```LiveCode
haversine(36.12, -86.67, 33.94, -118.40)
2887.259923
```



## Lua


```lua
local function haversine(x1, y1, x2, y2)
r=0.017453292519943295769236907684886127;
x1= x1*r; x2= x2*r; y1= y1*r; y2= y2*r; dy = y2-y1; dx = x2-x1;
a = math.pow(math.sin(dx/2),2) + math.cos(x1) * math.cos(x2) * math.pow(math.sin(dy/2),2); c = 2 * math.asin(math.sqrt(a)); d = 6372.8 * c;
return d;
end
```

Usage:

```lua
print(haversine(36.12, -86.67, 33.94, -118.4));
```

Output:

```txt
2887.2599506071
```



## Maple

Inputs assumed to be in radians.

```Maple
distance := (theta1, phi1, theta2, phi2)->2*6378.14*arcsin( sqrt((1-cos(theta2-theta1))/2 + cos(theta1)*cos(theta2)*(1-cos(phi2-phi1))/2) );
```

If you prefer, you can define a haversine function to clarify the definition:
```Maple
haversin := theta->(1-cos(theta))/2;
distance := (theta1, phi1, theta2, phi2)->2*6378.14*arcsin( sqrt(haversin(theta2-theta1) + cos(theta1)*cos(theta2)*haversin(phi2-phi1)) );
```


Usage:

```txt
distance(0.6304129261, -1.512676863, 0.5923647483, -2.066469834)
```

{{out}}

```txt
2889.679287
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Inputs assumed in degrees. Sin and Haversine expect arguments in radians; the built-in variable 'Degree' converts from degrees to radians.

```Mathematica

distance[{theta1_, phi1_}, {theta2_, phi2_}] := 
 2*6378.14 ArcSin@
   Sqrt[Haversine[(theta2 - theta1) Degree] + 
     Cos[theta1*Degree] Cos[theta2*Degree] Haversine[(phi2 - phi1) Degree]]

```

Usage:

```txt
distance[{36.12, -86.67}, {33.94, -118.4}]
```

{{out}}

```txt
2889.68
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
function rad = radians(degree) 
% degrees to radians
    rad = degree .* pi / 180;
end; 

function [a,c,dlat,dlon]=haversine(lat1,lon1,lat2,lon2)
% HAVERSINE_FORMULA.AWK - converted from AWK 
    dlat = radians(lat2-lat1);
    dlon = radians(lon2-lon1);
    lat1 = radians(lat1);
    lat2 = radians(lat2);
    a = (sin(dlat./2)).^2 + cos(lat1) .* cos(lat2) .* (sin(dlon./2)).^2;
    c = 2 .* asin(sqrt(a));
    arrayfun(@(x) printf("distance: %.4f km\n",6372.8 * x), c);
end;

[a,c,dlat,dlon] = haversine(36.12,-86.67,33.94,-118.40); % BNA to LAX
```

{{out}}

```txt
distance: 2887.2600 km
```



## Maxima


```maxima
dms(d, m, s) := (d + m/60 + s/3600)*%pi/180$

great_circle_distance(lat1, long1, lat2, long2) :=
   12742*asin(sqrt(sin((lat2 - lat1)/2)^2 + cos(lat1)*cos(lat2)*sin((long2 - long1)/2)^2))$

/* Coordinates are found here:
      http://www.airport-data.com/airport/BNA/
      http://www.airport-data.com/airport/LAX/   */

great_circle_distance(dms( 36,  7, 28.10), -dms( 86, 40, 41.50),
                      dms( 33, 56, 32.98), -dms(118, 24, 29.05)), numer;
/* 2886.326609413624 */
```



## MySQL


```MySQL
DELIMITER $$

CREATE FUNCTION haversine (
		lat1 FLOAT, lon1 FLOAT,
		lat2 FLOAT, lon2 FLOAT
	) RETURNS FLOAT
	NO SQL DETERMINISTIC
BEGIN
	DECLARE r FLOAT unsigned DEFAULT 6372.8;
	DECLARE dLat FLOAT unsigned;
	DECLARE dLon FLOAT unsigned;
	DECLARE a FLOAT unsigned;
	DECLARE c FLOAT unsigned;
	
	SET dLat = RADIANS(lat2 - lat1);
	SET dLon = RADIANS(lon2 - lon1);
	SET lat1 = RADIANS(lat1);
	SET lat2 = RADIANS(lat2);
	
	SET a = POW(SIN(dLat / 2), 2) + COS(lat1) * COS(lat2) * POW(SIN(dLon / 2), 2);
	SET c = 2 * ASIN(SQRT(a));
	
	RETURN (r * c);
END$$

DELIMITER ;
```


Usage:

```txt
SELECT haversine(36.12, -86.67, 33.94, -118.4);
```

{{out}}

```txt
2887.260009765625
```


=={{header|МК-61/52}}==
<lang>П3	->	П2	->	П1	->	П0
пи	1	8	0	/	П4
ИП1	МГ	ИП3	МГ	-	ИП4	*	П1	ИП0	МГ	ИП4	*	П0	ИП2	МГ	ИП4	*	П2
ИП0	sin	ИП2	sin	-	П8
ИП1	cos	ИП0	cos	*	ИП2	cos	-	П6
ИП1	sin	ИП0	cos	*	П7
ИП6	x^2	ИП7	x^2	ИП8	x^2	+	+	КвКор	2	/	arcsin	2	*	ИП5	*	С/П
```


''Input'': 6371,1 as a radius of the Earth, taken as the ball, or 6367,554 as an average radius of the Earth, or 6367,562 as an approximation of the radius of the average circumference (by Krasovsky's ellipsoid) to Р5; В/О ''lat<sub>1</sub>'' С/П ''long<sub>1</sub>'' С/П ''lat<sub>2</sub>'' С/П ''long<sub>2</sub>'' С/П; the coordinates must be entered as ''degrees,minutes'' (example: 46°50' as 46,5).

Test:

* N 36°7.2', W 86°40.2' - N 33°56.4', W 118°24.0' (Nashville - Los Angeles):
: ''Input'': 6371,1 П5 36,072 С/П -86,402 С/П 33,564 С/П -118,24 С/П
: ''Output'': 2886,4897.

* N 54°43', E 20°3' - N 43°07', E 131°54' (Kaliningrad - Vladivostok):
: ''Input'': 6371,1 П5 54,43 С/П 20,3 С/П 43,07 С/П 131,54 С/П
: ''Output'': 7357,4526.


## Nim


```nim
import math

proc radians(x): float = x * Pi / 180

proc haversine(lat1, lon1, lat2, lon2): float =
  const r = 6372.8 # Earth radius in kilometers
  let
    dLat = radians(lat2 - lat1)
    dLon = radians(lon2 - lon1)
    lat1 = radians(lat1)
    lat2 = radians(lat2)

    a = sin(dLat/2)*sin(dLat/2) + cos(lat1)*cos(lat2)*sin(dLon/2)*sin(dLon/2)
    c = 2*arcsin(sqrt(a))

  result = r * c

echo haversine(36.12, -86.67, 33.94, -118.40)
```

{{out}}

```txt
2.8872599506071115e+03
```


=={{header|Oberon-2}}==
Works with oo2c version2

```oberon2

MODULE Haversines;
IMPORT 
  LRealMath,
  Out;
  
  PROCEDURE Distance(lat1,lon1,lat2,lon2: LONGREAL): LONGREAL;
  CONST
    r = 6372.8D0; (* Earth radius as LONGREAL *)
    to_radians = LRealMath.pi / 180.0D0;
  VAR
    d,ph1,th1,th2: LONGREAL;
    dz,dx,dy: LONGREAL;
  BEGIN
    d := lon1 - lon2;
    ph1 := d * to_radians;
    th1 := lat1 * to_radians;
    th2 := lat2 * to_radians;
    
    dz := LRealMath.sin(th1) - LRealMath.sin(th2);
    dx := LRealMath.cos(ph1) * LRealMath.cos(th1) - LRealMath.cos(th2);
    dy := LRealMath.sin(ph1) * LRealMath.cos(th1);
    
    RETURN LRealMath.arcsin(LRealMath.sqrt(LRealMath.power(dx,2.0) + LRealMath.power(dy,2.0) + LRealMath.power(dz,2.0)) / 2.0) * 2.0 * r;
  END Distance;
BEGIN
  Out.LongRealFix(Distance(36.12,-86.67,33.94,-118.4),6,10);Out.Ln
END Haversines.

```

Output:

```txt

2887.2602975600

```



## Objeck


```objeck

bundle Default {
  class Haversine {
    function : Dist(th1 : Float, ph1 : Float, th2 : Float, ph2 : Float) ~ Float {
      ph1 -= ph2;
      ph1 := ph1->ToRadians();
      th1 := th1->ToRadians();
      th2 := th2->ToRadians();

      dz := th1->Sin()- th2->Sin();
      dx := ph1->Cos() * th1->Cos() - th2->Cos();
      dy := ph1->Sin() * th1->Cos();

      return ((dx * dx + dy * dy + dz * dz)->SquareRoot() / 2.0)->ArcSin() * 2 * 6371.0;
    }

    function : Main(args : String[]) ~ Nil {
      IO.Console->Print("distance: ")->PrintLine(Dist(36.12, -86.67, 33.94, -118.4));
    }
  }
}

```

{{out}}

```txt

distance: 2886.44

```


=={{header|Objective-C}}==

```objc
+ (double) distanceBetweenLat1:(double)lat1 lon1:(double)lon1
                          lat2:(double)lat2 lon2:(double)lon2 {
    //degrees to radians
    double lat1rad = lat1 * M_PI/180; 
    double lon1rad = lon1 * M_PI/180;
    double lat2rad = lat2 * M_PI/180;
    double lon2rad = lon2 * M_PI/180;
    
    //deltas
    double dLat = lat2rad - lat1rad;
    double dLon = lon2rad - lon1rad;

    double a = sin(dLat/2) * sin(dLat/2) + sin(dLon/2) * sin(dLon/2) * cos(lat1rad) * cos(lat2rad);
    double c = 2 * asin(sqrt(a));
    double R = 6372.8;
    return R * c;
}
```



## OCaml


The core calculation is fairly straightforward, 
but with an eye toward generality and reuse, 
this is how I might start:

```ocaml
(* Preamble -- some math, and an "angle" type which might be part of a common library. *)
let pi = 4. *. atan 1.
let radians_of_degrees = ( *. ) (pi /. 180.)
let haversin theta = 0.5 *. (1. -. cos theta)

(* The angle type can track radians or degrees, which I'll use for automatic conversion. *)
type angle = Deg of float | Rad of float
let as_radians = function
  | Deg d -> radians_of_degrees d
  | Rad r -> r

(* Demonstrating use of a module, and record type. *)
module LatLong = struct
  type t = { lat: float; lng: float }
  let of_angles lat lng = { lat = as_radians lat; lng = as_radians lng }
  let sub a b = { lat = a.lat-.b.lat; lng = a.lng-.b.lng }

  let dist radius a b =
    let d = sub b a in
    let h = haversin d.lat +. haversin d.lng *. cos a.lat *. cos b.lat in
    2. *. radius *. asin (sqrt h)
end

(* Now we can use the LatLong module to construct coordinates and calculate
 * great-circle distances.
 * NOTE radius and resulting distance are in the same measure, and units could
 * be tracked for this too... but who uses miles? ;) *)
let earth_dist = LatLong.dist 6372.8
and bna = LatLong.of_angles (Deg 36.12) (Deg (-86.67))
and lax = LatLong.of_angles (Deg 33.94) (Deg (-118.4))
in
earth_dist bna lax;;
```


If the above is fed to the REPL, the last line will produce this:

```txt

# earth_dist bna lax;;
- : float = 2887.25995060711102

```



## Oforth



```Oforth
import: math

: haversine(lat1, lon1, lat2, lon2)
| lat lon |

   lat2 lat1 - asRadian ->lat
   lon2 lon1 - asRadian ->lon

   lon 2 / sin sq lat1 asRadian cos * lat2 asRadian cos * 
   lat 2 / sin sq + sqrt asin 2 * 6372.8 * ;

haversine(36.12, -86.67, 33.94, -118.40) println
```


{{out}}

```txt

2887.25995060711

```



## ooRexx

{{trans|REXX}}
The rxmath library provides the required functions.

```oorexx
/*REXX pgm calculates distance between Nashville & Los Angles airports. */
say " Nashville:  north 36º  7.2', west  86º 40.2'   =   36.12º,  -86.67º"
say "Los Angles:  north 33º 56.4', west 118º 24.0'   =   33.94º, -118.40º"
say
dist=surfaceDistance(36.12,  -86.67,  33.94,  -118.4)
kdist=format(dist/1       ,,2)         /*show 2 digs past decimal point.*/
mdist=format(dist/1.609344,,2)         /*  "  "   "    "     "      "   */
ndist=format(mdist*5280/6076.1,,2)     /*  "  "   "    "     "      "   */
say ' distance between=  '  kdist  " kilometers,"
say '               or   '  mdist  " statute miles,"
say '               or   '  ndist  " nautical or air miles."
exit                                   /*stick a fork in it, we're done.*/
/*----------------------------------SURFACEDISTANCE subroutine----------*/
surfaceDistance: arg th1,ph1,th2,ph2   /*use haversine formula for dist.*/
  radius = 6372.8                      /*earth's mean radius in km      */
  ph1 = ph1-ph2
  x = cos(ph1) * cos(th1) - cos(th2)
  y = sin(ph1) * cos(th1)
  z = sin(th1) - sin(th2)
  return radius * 2 * aSin(sqrt(x**2+y**2+z**2)/2 )

cos: Return RxCalcCos(arg(1))
sin: Return RxCalcSin(arg(1))
asin: Return RxCalcArcSin(arg(1),,'R')
sqrt: Return RxCalcSqrt(arg(1))
::requires rxMath library
```

{{out}}

```txt
 Nashville:  north 36º  7.2', west  86º 40.2'   =   36.12º,  -86.67º
Los Angles:  north 33º 56.4', west 118º 24.0'   =   33.94º, -118.40º

 distance between=   2887.26  kilometers,
               or    1794.06  statute miles,
               or    1559.00  nautical or air miles.
```

              

## PARI/GP


```parigp
dist(th1, th2, ph)={
  my(v=[cos(ph)*cos(th1)-cos(th2),sin(ph)*cos(th1),sin(th1)-sin(th2)]);
  asin(sqrt(norml2(v))/2)
};
distEarth(th1, ph1, th2, ph2)={
  my(d=12742, deg=Pi/180); \\ Authalic diameter of the Earth
  d*dist(th1*deg, th2*deg, (ph1-ph2)*deg)
};
distEarth(36.12, -86.67, 33.94, -118.4)
```

{{out}}

```txt
%1 = 2886.44444
```



## Pascal

{{works with|Free_Pascal}} {{libheader|Math}}

```pascal
Program HaversineDemo(output);

uses
  Math;

function haversineDist(th1, ph1, th2, ph2: double): double;
  const
   diameter = 2 * 6372.8;
  var
    dx, dy, dz: double;
  begin
    ph1 := degtorad(ph1 - ph2);
    th1 := degtorad(th1);
    th2 := degtorad(th2);
 
    dz := sin(th1) - sin(th2);
    dx := cos(ph1) * cos(th1) - cos(th2);
    dy := sin(ph1) * cos(th1);
    haversineDist := arcsin(sqrt(dx**2 + dy**2 + dz**2) / 2) * diameter;
  end;

begin
  writeln ('Haversine distance: ', haversineDist(36.12, -86.67, 33.94, -118.4):7:2, ' km.');
end.
```

{{out}}

```txt
Haversine distance: 2887.26 km.

```



## Perl

{{libheader|ntheory}}

```perl
use ntheory qw/Pi/;

sub asin { my $x = shift; atan2($x, sqrt(1-$x*$x)); }

sub surfacedist {
  my($lat1, $lon1, $lat2, $lon2) = @_;
  my $radius = 6372.8;
  my $radians = Pi() / 180;;
  my $dlat = ($lat2 - $lat1) * $radians;
  my $dlon = ($lon2 - $lon1) * $radians;
  $lat1 *= $radians;
  $lat2 *= $radians;
  my $a = sin($dlat/2)**2 + cos($lat1) * cos($lat2) * sin($dlon/2)**2;
  my $c = 2 * asin(sqrt($a));
  return $radius * $c;
}

printf "Distance: %.3f km\n", surfacedist(36.12, -86.67, 33.94, -118.4);
```

{{out}}

```txt
Distance: 2887.260 km
```



## Perl 6


```perl6
class EarthPoint {
        has $.lat; # latitude
        has $.lon; # longitude

        has $earth_radius = 6371; # mean earth radius
        has $radian_ratio = pi / 180;

        # accessors for radians
        method latR { $.lat * $radian_ratio }
        method lonR { $.lon * $radian_ratio }

        method haversine-dist(EarthPoint $p) {

                my EarthPoint $arc .= new(
                        lat => $!lat - $p.lat,
                        lon => $!lon - $p.lon );

                my $a = sin($arc.latR/2) ** 2 + sin($arc.lonR/2) ** 2
                        * cos($.latR) * cos($p.latR);
                my $c = 2 * asin( sqrt($a) );

                return $earth_radius * $c;
        }
}

my EarthPoint $BNA .= new(lat => 36.12, lon => -86.67);
my EarthPoint $LAX .= new(lat => 33.94, lon => -118.4);

say $BNA.haversine-dist($LAX); # 2886.44444099822
```



## Phix


```Phix
constant MER = 6371         -- mean earth radius(km)
constant DEG_TO_RAD = PI/180

function haversine(atom lat1, long1, lat2, long2)
    lat1 *= DEG_TO_RAD
    lat2 *= DEG_TO_RAD
    long1 *= DEG_TO_RAD
    long2 *= DEG_TO_RAD
    return MER*arccos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(long2-long1))
end function

atom d = haversine(36.12,-86.67,33.94,-118.4)
printf(1,"Distance is %f km (%f miles)\n",{d,d/1.609344})
```

{{out}}

```txt

Distance is 2886.444443 km (1793.553425 miles)

```



## PHP


```php
class POI {
    private $latitude;
    private $longitude;
    public function __construct($latitude, $longitude) {
        $this->latitude = deg2rad($latitude);
        $this->longitude = deg2rad($longitude);
    }
    public function getLatitude() return $this->latitude;
    public function getLongitude() return $this->longitude;
    public function getDistanceInMetersTo(POI $other) {
        $radiusOfEarth = 6371000;// Earth's radius in meters.
        $diffLatitude = $other->getLatitude() - $this->latitude;
        $diffLongitude = $other->getLongitude() - $this->longitude;
        $a = sin($diffLatitude / 2) * sin($diffLatitude / 2) +
            cos($this->latitude) * cos($other->getLatitude()) *
            sin($diffLongitude / 2) * sin($diffLongitude / 2);
        $c = 2 * asin(sqrt($a));
        $distance = $radiusOfEarth * $c;
        return $distance;
    }
}
```

Test:

```php
$user = new POI($_GET["latitude"], $_GET["longitude"]);
$poi = new POI(19,69276, -98,84350); // Piramide del Sol, Mexico
echo $user->getDistanceInMetersTo($poi);
```



## PicoLisp


```PicoLisp
(scl 12)
(load "@lib/math.l")

(de haversine (Th1 Ph1 Th2 Ph2)
   (setq
      Ph1 (*/ (- Ph1 Ph2) pi 180.0)
      Th1 (*/ Th1 pi 180.0)
      Th2 (*/ Th2 pi 180.0) )
   (let
      (DX (- (*/ (cos Ph1) (cos Th1) 1.0) (cos Th2))
         DY (*/ (sin Ph1) (cos Th1) 1.0)
         DZ (- (sin Th1) (sin Th2)) )
      (* `(* 2 6371)
         (asin
            (/
               (sqrt (+ (* DX DX) (* DY DY) (* DZ DZ)))
               2 ) ) ) ) )
```

Test:

```PicoLisp
(prinl
   "Haversine distance: "
   (round (haversine 36.12 -86.67 33.94 -118.4))
   " km" )
```

{{out}}

```txt
Haversine distance: 2,886.444 km
```



## PL/I


```PL/I
test: procedure options (main); /* 12 January 2014.  Derived from Fortran version */
   declare d float;

   d = haversine(36.12, -86.67, 33.94, -118.40);  /* BNA to LAX */
   put edit ( 'distance: ', d, ' km') (A, F(10,3)); /* distance: 2887.2600 km */


degrees_to_radians: procedure (degree) returns (float);
   declare degree float nonassignable;
   declare pi float (15) initial ( (4*atan(1.0d0)) );

   return ( degree*pi/180 );
end degrees_to_radians;
 
haversine: procedure (deglat1, deglon1, deglat2, deglon2) returns (float);
   declare (deglat1, deglon1, deglat2, deglon2) float nonassignable;
   declare (a, c, dlat, dlon, lat1, lat2) float;
   declare radius float value (6372.8);

   dlat = degrees_to_radians(deglat2-deglat1);
   dlon = degrees_to_radians(deglon2-deglon1);
   lat1 = degrees_to_radians(deglat1);
   lat2 = degrees_to_radians(deglat2);
   a = (sin(dlat/2))**2 + cos(lat1)*cos(lat2)*(sin(dlon/2))**2;
   c = 2*asin(sqrt(a));
   return ( radius*c );
end haversine;

end test;
```

{{out}}

```txt

distance:   2887.260 km

```



## PowerShell

{{works with|PowerShell|3}}

```PowerShell

Add-Type -AssemblyName System.Device
 
$BNA = New-Object System.Device.Location.GeoCoordinate 36.12, -86.67
$LAX = New-Object System.Device.Location.GeoCoordinate 33.94, -118.40
 
$BNA.GetDistanceTo( $LAX ) / 1000

```

{{out}}

```txt

2888.93627213254

```

{{works with|PowerShell|2}}

```PowerShell

function Get-GreatCircleDistance ( $Coord1, $Coord2 )
    {
    #  Convert decimal degrees to radians
    $Lat1  = $Coord1[0] / 180 * [math]::Pi
    $Long1 = $Coord1[1] / 180 * [math]::Pi
    $Lat2  = $Coord2[0] / 180 * [math]::Pi
    $Long2 = $Coord2[1] / 180 * [math]::Pi
 
    #  Mean Earth radius (km)
    $R = 6371
   
    #  Haversine formula
    $ArcLength = 2 * $R *
                    [math]::Asin(
                        [math]::Sqrt(
                            [math]::Sin( ( $Lat1 - $Lat2 ) / 2 ) *
                            [math]::Sin( ( $Lat1 - $Lat2 ) / 2 ) +
                            [math]::Cos( $Lat1 ) *
                            [math]::Cos( $Lat2 ) *
                            [math]::Sin( ( $Long1 - $Long2 ) / 2 ) *
                            [math]::Sin( ( $Long1 - $Long2 ) / 2 ) ) )
    return $ArcLength
    }
 
$BNA = 36.12,  -86.67
$LAX = 33.94, -118.40
 
Get-GreatCircleDistance $BNA $LAX

```

{{out}}

```txt

2886.44444283799

```



## Pure Data


Up until now there is no 64bit float in Pure Data, so the result of the calculation might not be completely accurate.

```txt

#N canvas 527 1078 450 686 10;
#X obj 28 427 atan2;
#X obj 28 406 sqrt;
#X obj 62 405 sqrt;
#X obj 28 447 * 2;
#X obj 62 384 -;
#X msg 62 362 1 \$1;
#X obj 28 339 t f f;
#X obj 28 210 sin;
#X obj 83 207 sin;
#X obj 138 206 cos;
#X obj 193 206 cos;
#X obj 28 179 / 2;
#X obj 83 182 / 2;
#X obj 28 74 unpack f f;
#X obj 28 98 t f f;
#X obj 28 301 expr $f1 + ($f2 * $f3 * $f4);
#X obj 28 148 deg2rad;
#X obj 83 149 deg2rad;
#X obj 138 148 deg2rad;
#X obj 193 149 deg2rad;
#X obj 28 232 t f f;
#X obj 28 257 *;
#X obj 83 232 t f f;
#X obj 83 257 *;
#X obj 83 98 t f b;
#X obj 28 542 * 6372.8;
#X obj 193 120 f 33.94;
#X obj 28 125 - 33.94;
#X msg 28 45 36.12 -86.67;
#X obj 83 123 - -118.4;
#X floatatom 28 577 8 0 0 0 - - -, f 8;
#X connect 0 0 3 0;
#X connect 1 0 0 0;
#X connect 2 0 0 1;
#X connect 3 0 25 0;
#X connect 4 0 2 0;
#X connect 5 0 4 0;
#X connect 6 0 1 0;
#X connect 6 1 5 0;
#X connect 7 0 20 0;
#X connect 8 0 22 0;
#X connect 9 0 15 2;
#X connect 10 0 15 3;
#X connect 11 0 7 0;
#X connect 12 0 8 0;
#X connect 13 0 14 0;
#X connect 13 1 24 0;
#X connect 14 0 27 0;
#X connect 14 1 18 0;
#X connect 15 0 6 0;
#X connect 16 0 11 0;
#X connect 17 0 12 0;
#X connect 18 0 9 0;
#X connect 19 0 10 0;
#X connect 20 0 21 0;
#X connect 20 1 21 1;
#X connect 21 0 15 0;
#X connect 22 0 23 0;
#X connect 22 1 23 1;
#X connect 23 0 15 1;
#X connect 24 0 29 0;
#X connect 24 1 26 0;
#X connect 25 0 30 0;
#X connect 26 0 19 0;
#X connect 27 0 16 0;
#X connect 28 0 13 0;
#X connect 29 0 17 0;

```



## PureBasic

{{trans|Pascal}}

```PureBasic
#DIA=2*6372.8

Procedure.d Haversine(th1.d,ph1.d,th2.d,ph2.d)
  Define dx.d,
         dy.d,
         dz.d
  
  ph1=Radian(ph1-ph2)
  th1=Radian(th1)
  th2=Radian(th2)
  
  dz=Sin(th1)-Sin(th2)
  dx=Cos(ph1)*Cos(th1)-Cos(th2)
  dy=Sin(ph1)*Cos(th1)
  ProcedureReturn ASin(Sqr(Pow(dx,2)+Pow(dy,2)+Pow(dz,2))/2)*#DIA
EndProcedure

OpenConsole("Haversine distance")
Print("Haversine distance: ")
Print(StrD(Haversine(36.12,-86.67,33.94,-118.4),7)+" km.")
Input()
```

{{out}}

```txt
Haversine distance: 2887.2599506 km.
```



## Python


```python
from math import radians, sin, cos, sqrt, asin


def haversine(lat1, lon1, lat2, lon2):
    R = 6372.8  # Earth radius in kilometers

    dLat = radians(lat2 - lat1)
    dLon = radians(lon2 - lon1)
    lat1 = radians(lat1)
    lat2 = radians(lat2)

    a = sin(dLat / 2)**2 + cos(lat1) * cos(lat2) * sin(dLon / 2)**2
    c = 2 * asin(sqrt(a))

    return R * c

>>> haversine(36.12, -86.67, 33.94, -118.40)
2887.2599506071106
>>> 
```



## R


```r
dms_to_rad <- function(d, m, s) (d + m / 60 + s / 3600) * pi / 180

# Volumetric mean radius is 6371 km, see http://nssdc.gsfc.nasa.gov/planetary/factsheet/earthfact.html
# The diameter is thus 12742 km

great_circle_distance <- function(lat1, long1, lat2, long2) {
   a <- sin(0.5 * (lat2 - lat1))
   b <- sin(0.5 * (long2 - long1))
   12742 * asin(sqrt(a * a + cos(lat1) * cos(lat2) * b * b))
}

# Coordinates are found here:
#     http://www.airport-data.com/airport/BNA/
#     http://www.airport-data.com/airport/LAX/

great_circle_distance(
   dms_to_rad(36,  7, 28.10), dms_to_rad( 86, 40, 41.50),   # Nashville International Airport (BNA)
   dms_to_rad(33, 56, 32.98), dms_to_rad(118, 24, 29.05))  # Los Angeles International Airport (LAX)

# Output:  2886.327
```



## Racket

Almost the same as the Scheme version.

```racket

#lang racket
(require math)
(define earth-radius 6371)

(define (distance lat1 long1 lat2 long2)
  (define (h a b) (sqr (sin (/ (- b a) 2))))
  (* 2 earth-radius 
     (asin (sqrt (+ (h lat1 lat2) 
                    (* (cos lat1) (cos lat2) (h long1 long2)))))))

(define (deg-to-rad d m s) 
  (* (/ pi 180) (+ d (/ m 60) (/ s 3600))))

(distance (deg-to-rad 36  7.2 0) (deg-to-rad  86 40.2 0)
          (deg-to-rad 33 56.4 0) (deg-to-rad 118 24.0 0))

```

{{out}}

```txt

2886.444442837984

```



## Raven

{{trans|Groovy}}

```Raven
define PI 
  -1 acos

define toRadians use $degree
  $degree PI * 180 /

define haversine use $lat1, $lon1, $lat2, $lon2
  6372.8 as $R
  # In kilometers
  $lat2 $lat1 - toRadians   as $dLat
  $lon2 $lon1 - toRadians   as $dLon
  $lat1 toRadians  as $lat1
  $lat2 toRadians  as $lat2
 
  $dLat 2 /  sin 
  $dLat 2 /  sin *
  $dLon 2 /  sin
  $dLon 2 /  sin *
  $lat1 cos * 
  $lat2 cos * +        as $a
  $a sqrt  asin  2 *   as $c
  $R $c *
}
 
-118.40 33.94 -86.67 36.12 haversine "haversine: %.15g\n" print
```

{{out}}

```txt
haversine: 2887.25995060711
```



## REXX

The use of normalization for angles isn't required for the Haversine formula, but those normalization functions were included 

herein anyway   (to support normalization of input arguments to the trigonometric functions for the general case).  

```rexx
/*REXX program  calculates  the  distance between  Nashville  and  Los Angles  airports.*/
call pi;  numeric digits length(pi)%2            /*use half of decimal digits  of  PI.  */
say "       Nashville:  north 36º  7.2', west  86º 40.2'   =   36.12º,  -86.67º"
say "      Los Angles:  north 33º 56.4', west 118º 24.0'   =   33.94º, -118.40º"
@using_radius= 'using the mean radius of the earth as '            /*a literal for  SAY.*/
radii.=.;    radii.1=6372.8;     radii.2=6371    /*mean radii of the earth in kilometers*/
say;                         m=1/0.621371192237  /*M:   one statute mile  in      "     */
    do radius=1  while radii.radius\==.          /*calc. distance using specific radii. */
    d=surfaceDistance( 36.12,    -86.67,    33.94,   -118.4,    radii.radius);         say
    say center(@using_radius     radii.radius         ' kilometers', 75, '─')
    say ' Distance between:  '   format(d/1            ,,2)    " kilometers,"
    say '               or   '   format(d/m            ,,2)    " statute miles,"
    say '               or   '   format(d/m*5280/6076.1,,2)    " nautical (or air miles)."
    end   /*radius*/                             /*show──┘   2 dec. digs past dec. point*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Acos: return pi()  *  .5  - aSin( arg(1) )       /*calculate the ArcCos of an argument. */
d2d:  return arg(1)               //  360        /*normalize degrees to a  unit circle. */
d2r:  return r2r(  arg(1) * pi()  /   180)       /*normalize and convert deg ──► radians*/
r2d:  return d2d( (arg(1) * 180   /   pi()))     /*normalize and convert rad ──► degrees*/
r2r:  return arg(1)               // (pi() * 2)  /*normalize radians to a  unit circle. */
pi:   pi= 3.141592653589793238462643383279502884197169399375105820975;           return pi
/*──────────────────────────────────────────────────────────────────────────────────────*/
surfaceDistance: parse arg th1,ph1,th2,ph2,r     /*use  haversine  formula for distance.*/
         numeric digits digits() * 2             /*double the number of decimal digits. */
            ph1= d2r(ph1 - ph2)                  /*convert degrees ──► radians & reduce.*/
            th1= d2r(th1);      th2 =  d2r(th2)  /*   "       "     "     "    "    "   */
              x= cos(ph1) * cos(th1) - cos(th2)
              y= sin(ph1) * cos(th1)
              z= sin(th1) - sin(th2);  return Asin(sqrt( x**2 + y**2 + z**2) / 2)  * r * 2
/*──────────────────────────────────────────────────────────────────────────────────────*/
Asin: procedure; parse arg x 1 z 1 o 1 p;     a= abs(x);            aa= a * a
         if a>=sqrt(2) * .5  then  return sign(x) * Acos( sqrt(1 - aa) )
                  do j=2  by 2  until p=z;    p= z;   o= o*aa* (j-1) / j;   z= z + o/(j+1)
                  end   /*j*/;     return z      /* [↑]  compute until no more noise.   */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cos:  procedure; parse arg x;       x= r2r(x);       a= abs(x);               Hpi= pi * .5
         numeric fuzz min(6, digits()  - 3) ;    if a=pi    then return -1
         if a=Hpi | a=Hpi*3  then return  0 ;    if a=pi/3  then return .5
         if a=pi * 2 / 3     then return -.5;    q= x*x;    p= 1;     z= 1;       _= 1
           do k=2  by 2; _=-_*q/(k*(k-1)); z=z+_; if z=p  then leave; p=z; end;   return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
sin:  procedure; parse arg x;  x= r2r(x);         numeric fuzz min(5, digits() - 3)
         if abs(x)=pi  then  return 0;            q= x*x;    p= x;     z= x;      _= x
           do k=2  by 2; _=-_*q/(k*(k+1)); z=z+_; if z=p  then leave; p=z; end;   return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x; if x=0  then return 0; d=digits(); m.=9; numeric form; h=d+6
      numeric digits;  parse value format(x,2,1,,0) 'E0' with g "E" _ .;  g=g * .5'e'_ % 2
        do j=0  while h>9;      m.j=h;               h=h%2+1;       end  /*j*/
        do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;  end  /*k*/;   return g
```

REXX doesn't have most of the higher math functions, so they are included here (above) as subroutines (functions).

```txt

       ╔════════════════════════════════════════════════════════════════════════╗
       ║ A note on built─in functions:  REXX doesn't have a lot of mathematical ║
       ║ or  (particularly) trigonometric functions,  so REXX programmers have  ║
       ║ to write their own.  Usually, this is done once, or most likely,  one  ║
       ║ is borrowed from another program.  Knowing this, the one that is used  ║
       ║ has a lot of boilerplate in it.                                        ║
       ║                                                                        ║
       ║ Programming note:  the  "general 1─liner"  subroutines are taken from  ║
       ║ other programs that I wrote, but I broke up their one line of source   ║
       ║ so it can be viewed without shifting the viewing window.               ║
       ║                                                                        ║
       ║ The    pi    constant  (as used here)  is actually a much more robust  ║
       ║ function and will return up to one million digits in the real version. ║
       ║                                                                        ║
       ║ One bad side effect is that, like a automobile without a hood, you see ║
       ║ all the dirty stuff going on.    Also, don't visit a sausage factory.  ║
       ╚════════════════════════════════════════════════════════════════════════╝ 

```

{{out|output|text=  when using the in-line defaults:}}

```txt

       Nashville:  north 36º  7.2', west  86º 40.2'   =   36.12º,  -86.67º
      Los Angles:  north 33º 56.4', west 118º 24.0'   =   33.94º, -118.40º


─────────using the mean radius of the earth as  6372.8  kilometers─────────
 Distance between:   2887.26  kilometers,
               or    1794.06  statute miles,
               or    1559.00  nautical (or air miles).

──────────using the mean radius of the earth as  6371  kilometers──────────
 Distance between:   2886.44  kilometers,
               or    1793.55  statute miles,
               or    1558.56  nautical (or air miles).

```



## Ring


```ring

decimals(8)
see haversine(36.12, -86.67, 33.94, -118.4) + nl

func haversine x1, y1, x2, y2
     r=0.01745
     x1= x1*r
     x2= x2*r
     y1= y1*r
     y2= y2*r
     dy = y2-y1
     dx = x2-x1
     a = pow(sin(dx/2),2) + cos(x1) * cos(x2) * pow(sin(dy/2),2)
     c = 2 * asin(sqrt(a))
     d = 6372.8 * c
     return d

```



## Ruby


```ruby
include Math

Radius = 6371  # rough radius of the Earth, in kilometers

def spherical_distance(start_coords, end_coords)
  lat1, long1 = deg2rad *start_coords
  lat2, long2 = deg2rad *end_coords
  2 * Radius * asin(sqrt(sin((lat2-lat1)/2)**2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)**2))
end

def deg2rad(lat, long)
  [lat * PI / 180, long * PI / 180]
end

bna = [36.12, -86.67]
lax = [33.94, -118.4]

puts "%.1f" % spherical_distance(bna, lax)
```


{{out}}

```txt
2886.4
```


Alternativley:

{{trans|Python}}

```ruby
include Math
 
def haversine(lat1, lon1, lat2, lon2)
    r = 6372.8        # Earth radius in kilometers
    deg2rad = PI/180  # convert degress to radians
 
    dLat = (lat2 - lat1) * deg2rad
    dLon = (lon2 - lon1) * deg2rad
    lat1 = lat1 * deg2rad
    lat2 = lat2 * deg2rad
 
    a = sin(dLat / 2)**2 + cos(lat1) * cos(lat2) * sin(dLon / 2)**2
    c = 2 * asin(sqrt(a))
    r * c
end

puts "distance is #{haversine(36.12, -86.67, 33.94, -118.40)} km "

```

{{out}}

```txt

distance is 2887.2599506071106 km 

```



## Run BASIC


```runbasic
    D2R = atn(1)/45
    diam  = 2 * 6372.8
Lg1m2  = ((-86.67)-(-118.4)) * D2R
Lt1    = 36.12 * D2R ' degrees to rad
Lt2    = 33.94 * D2R
    dz    = sin(Lt1) - sin(Lt2)
    dx    = cos(Lg1m2) * cos(Lt1) - cos(Lt2)
    dy    = sin(Lg1m2) * cos(Lt1)
    hDist = asn((dx^2 + dy^2 + dz^2)^0.5 /2) * diam
print "Haversine distance: ";using("####.#############",hDist);" km."

 'Tips: ( 36 deg 7 min 12 sec ) = print 36+(7/60)+(12/3600).  Produces: 36.12 deg.
 '
 '      http://maps.google.com
 '      Search   36.12,-86.67
 '      Earth.
 '      Center the pin, zoom airport.
 '      Directions (destination).
 '      36.12.-86.66999
 '      Distance is 35.37 inches.
```
Output 
```txt
Haversine distance: 2887.2599506071104 km.
```



## Rust


```rust

use std::f64;

static R: f64 = 6372.8;

fn haversine_dist(mut th1: f64, mut ph1: f64, mut th2: f64, ph2: f64) -> f64 {
    ph1 -= ph2;
    ph1 = ph1.to_radians();
    th1 = th1.to_radians();
    th2 = th2.to_radians();
    let dz: f64 = th1.sin() - th2.sin();
    let dx: f64 = ph1.cos() * th1.cos() - th2.cos();
    let dy: f64 = ph1.sin() * th1.cos();
    ((dx * dx + dy * dy + dz * dz).sqrt() / 2.0).asin() * 2.0 * R
}

fn main() {
    let d: f64 = haversine_dist(36.12, -86.67, 33.94, -118.4);
    println!("Distance: {} km ({} mi)", d, d / 1.609344);
}


```
Output 
```txt
Distance: 2887.2599506071106 km (1794.060157807846 mi)
```



## SAS


```SAS

options minoperator;

%macro haver(lat1, long1, lat2, long2, type=D, dist=K);

	%if %upcase(&type) in (D DEG DEGREE DEGREES) %then %do;
		%let convert = constant('PI')/180;
		%end;
	%else %if %upcase(&type) in (R RAD RADIAN RADIANS) %then %do;
		%let convert = 1;
		%end;
	%else %do;
		%put ERROR - Enter RADIANS or DEGREES for type.;
		%goto exit;
		%end;

	%if %upcase(&dist) in (M MILE MILES) %then %do;
		%let distrat = 1.609344;
		%end;
	%else %if %upcase(&dist) in (K KM KILOMETER KILOMETERS) %then %do;
		%let distrat = 1;
		%end;
	%else %do;
		%put ERROR - Enter M on KM for dist;
		%goto exit;
		%end;
		
		data _null_;
			convert = &convert;
			lat1 = &lat1 * convert;
			lat2 = &lat2 * convert;
			long1 = &long1 * convert;
			long2 = &long2 * convert;

			diff1 = lat2 - lat1;
			diff2 = long2 - long1;

			part1 = sin(diff1/2)**2;
			part2 = cos(lat1)*cos(lat2);
			part3 = sin(diff2/2)**2;

			root = sqrt(part1 + part2*part3);

			dist = 2 * 6372.8 / &distrat * arsin(root);

			put "Distance is " dist "%upcase(&dist)";
		run;

	%exit:
%mend;

%haver(36.12, -86.67, 33.94, -118.40); 

```

{{out}}

```txt
Distance is 2887.2599506 K
```



## Scala


```scala
import math._

object Haversine {
   val R = 6372.8  //radius in km

   def haversine(lat1:Double, lon1:Double, lat2:Double, lon2:Double)={
      val dLat=(lat2 - lat1).toRadians
      val dLon=(lon2 - lon1).toRadians
 
      val a = pow(sin(dLat/2),2) + pow(sin(dLon/2),2) * cos(lat1.toRadians) * cos(lat2.toRadians)
      val c = 2 * asin(sqrt(a))
      R * c
   }
	
   def main(args: Array[String]): Unit = {
      println(haversine(36.12, -86.67, 33.94, -118.40))
  }
}
```

{{out}}

```txt
2887.2599506071106
```



## Scheme


```scheme
(define earth-radius 6371)
(define pi (acos -1))

(define (distance lat1 long1 lat2 long2)
(define (h a b) (expt (sin (/ (- b a) 2)) 2))
(* 2 earth-radius (asin (sqrt (+ (h lat1 lat2) (* (cos lat1) (cos lat2) (h long1 long2)))))))

(define (deg-to-rad d m s) (* (/ pi 180) (+ d (/ m 60) (/ s 3600))))

(distance (deg-to-rad 36  7.2 0) (deg-to-rad  86 40.2 0)
          (deg-to-rad 33 56.4 0) (deg-to-rad 118 24.0 0))
; 2886.444442837984
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";

const func float: greatCircleDistance (in float: latitude1, in float: longitude1,
    in float: latitude2, in float: longitude2) is func
  result
    var float: distance is 0.0;
  local
    const float: EarthRadius is 6372.8;  # Average great-elliptic or great-circle radius in kilometers
  begin
    distance := 2.0 * EarthRadius * asin(sqrt(sin(0.5 * (latitude2 - latitude1)) ** 2 +
                                              cos(latitude1) * cos(latitude2) *
                                              sin(0.5 * (longitude2 - longitude1)) ** 2));
  end func;

const func float: degToRad (in float: degrees) is
  return degrees * 0.017453292519943295769236907684886127;

const proc: main is func
  begin
    writeln("Distance in kilometers between BNA and LAX");
    writeln(greatCircleDistance(degToRad(36.12), degToRad(-86.67),  # Nashville International Airport (BNA)
                                degToRad(33.94), degToRad(-118.4))  # Los Angeles International Airport (LAX)
            digits 2);
  end func;
```


{{out}}

```txt

2887.26

```



## Sidef

{{trans|Perl 6}}

```ruby
class EarthPoint(lat, lon) {

    const earth_radius = 6371       # mean earth radius
    const radian_ratio = Num.pi/180

    # accessors for radians
    method latR { self.lat * radian_ratio }
    method lonR { self.lon * radian_ratio }

    method haversine_dist(EarthPoint p) {
        var arc = EarthPoint(
              self.lat - p.lat,
              self.lon - p.lon,
        )

        var a = Math.sum(
                  (arc.latR / 2).sin**2,
                  (arc.lonR / 2).sin**2 *
                    self.latR.cos * p.latR.cos
                )

        earth_radius * a.sqrt.asin * 2
    }
}

var BNA = EarthPoint.new(lat: 36.12, lon: -86.67)
var LAX = EarthPoint.new(lat: 33.94, lon: -118.4)

say BNA.haversine_dist(LAX)   #=> 2886.444442837983299747157823945746716...
```



## Stata

First, a program to add a distance variable to a dataset, given variables for LAT/LON of two points.


```stata
program spheredist
	version 15.0
	syntax varlist(min=4 max=4 numeric), GENerate(namelist max=1) ///
		[Radius(real 6371) ALTitude(real 0) LABel(string)]
	confirm new variable `generate'
	local lat1 : word 1 of `varlist'
	local lon1 : word 2 of `varlist'
	local lat2 : word 3 of `varlist'
	local lon2 : word 4 of `varlist'
	local r=2*(`radius'+`altitude'/1000)
	local k=_pi/180
	gen `generate'=`r'*asin(sqrt(sin((`lat2'-`lat1')*`k'/2)^2+ ///
		cos(`lat1'*`k')*cos(`lat2'*`k')*sin((`lon2'-`lon1')*`k'/2)^2))
	if `"`label'"' != "" {
		label variable `generate' `"`label'"'
	}
end
```


Illustration with a sample dataset.


```stata
import delimited airports.csv, clear
format %9.4f l*
list

     +----------------------------------------------------------------------------------------------------+
     | iata                                   airport          city         country       lat         lon |
     |----------------------------------------------------------------------------------------------------|
  1. |  AMS                Amsterdam Airport Schiphol     Amsterdam     Netherlands   52.3086      4.7639 |
  2. |  BNA           Nashville International Airport     Nashville   United States   36.1245    -86.6782 |
  3. |  CDG   Charles de Gaulle International Airport         Paris          France   49.0128      2.5500 |
  4. |  CGN                      Cologne Bonn Airport       Cologne         Germany   50.8659      7.1427 |
  5. |  LAX         Los Angeles International Airport   Los Angeles   United States   33.9425   -118.4080 |
     |----------------------------------------------------------------------------------------------------|
  6. |  MEM             Memphis International Airport       Memphis   United States   35.0424    -89.9767 |
     +----------------------------------------------------------------------------------------------------+
```


MEM/CGN joins two Fedex Express hubs. The line AMS/LAX is operated by KLM Royal Dutch Airlines.
We will compute the distance between each pair of airports, both at sea level and at typical cruising flight level (35000 ft).

Bear in mind that the actual route of an airliner is usually not a piece of great circle, so this will only give an idea. For instance, according to [http://flightaware.com/ FlightAware], the route of a Fedex flight from Memphis to Paris is 7852 km long, at FL300 altitude (9150 m). The program given here would yield 7328.33 km instead.


```stata
keep iata lat lon
rename (iata lat lon) =2
gen k=0
tempfile tmp
save "`tmp'"
rename *2 *1
joinby k using `tmp'
drop if iata1>=iata2
drop k
list

     +-----------------------------------------------------------+
     | iata1      lat1        lon1   iata2      lat2        lon2 |
     |-----------------------------------------------------------|
  1. |   AMS   52.3086      4.7639     BNA   36.1245    -86.6782 |
  2. |   AMS   52.3086      4.7639     CGN   50.8659      7.1427 |
  3. |   AMS   52.3086      4.7639     LAX   33.9425   -118.4080 |
  4. |   AMS   52.3086      4.7639     CDG   49.0128      2.5500 |
  5. |   AMS   52.3086      4.7639     MEM   35.0424    -89.9767 |
     |-----------------------------------------------------------|
  6. |   BNA   36.1245    -86.6782     CGN   50.8659      7.1427 |
  7. |   BNA   36.1245    -86.6782     CDG   49.0128      2.5500 |
  8. |   BNA   36.1245    -86.6782     LAX   33.9425   -118.4080 |
  9. |   BNA   36.1245    -86.6782     MEM   35.0424    -89.9767 |
 10. |   CDG   49.0128      2.5500     LAX   33.9425   -118.4080 |
     |-----------------------------------------------------------|
 11. |   CDG   49.0128      2.5500     MEM   35.0424    -89.9767 |
 12. |   CDG   49.0128      2.5500     CGN   50.8659      7.1427 |
 13. |   CGN   50.8659      7.1427     LAX   33.9425   -118.4080 |
 14. |   CGN   50.8659      7.1427     MEM   35.0424    -89.9767 |
 15. |   LAX   33.9425   -118.4080     MEM   35.0424    -89.9767 |
     +-----------------------------------------------------------+
```


Now compute the distances and print the result.


```stata
spheredist lat1 lon1 lat2 lon2, gen(dist) lab(Distance at sea level)
spheredist lat1 lon1 lat2 lon2, gen(fl350) alt(10680) lab(Distance at FL350 altitude)
format %9.2f dist fl350
list iata* dist fl350

     +-----------------------------------+
     | iata1   iata2      dist     fl350 |
     |-----------------------------------|
  1. |   AMS     CGN    229.64    230.03 |
  2. |   AMS     CDG    398.27    398.94 |
  3. |   AMS     MEM   7295.19   7307.56 |
  4. |   AMS     BNA   7004.61   7016.48 |
  5. |   AMS     LAX   8955.95   8971.13 |
     |-----------------------------------|
  6. |   BNA     LAX   2886.32   2891.21 |
  7. |   BNA     CGN   7222.75   7234.99 |
  8. |   BNA     CDG   7018.39   7030.29 |
  9. |   BNA     MEM    321.62    322.16 |
 10. |   CDG     LAX   9102.51   9117.94 |
     |-----------------------------------|
 11. |   CDG     CGN    387.82    388.48 |
 12. |   CDG     MEM   7317.82   7330.23 |
 13. |   CGN     LAX   9185.47   9201.04 |
 14. |   CGN     MEM   7514.96   7527.70 |
 15. |   LAX     MEM   2599.71   2604.12 |
     +-----------------------------------+
```


Notice that the distance from Nashville to Los Angeles is given as 2886.32 km, which is slightly different from the task description. The coordinates come from [https://openflights.org/html/apsearch OpenFlights] and are supposably more accurate. Using the data in the task description, one gets 2886.44 as expected.


## Swift

{{trans|Objective-C}}

```Swift
import Foundation

func haversine(lat1:Double, lon1:Double, lat2:Double, lon2:Double) -> Double {
    let lat1rad = lat1 * Double.pi/180
    let lon1rad = lon1 * Double.pi/180
    let lat2rad = lat2 * Double.pi/180
    let lon2rad = lon2 * Double.pi/180
    
    let dLat = lat2rad - lat1rad
    let dLon = lon2rad - lon1rad
    let a = sin(dLat/2) * sin(dLat/2) + sin(dLon/2) * sin(dLon/2) * cos(lat1rad) * cos(lat2rad)
    let c = 2 * asin(sqrt(a))
    let R = 6372.8
    
    return R * c
}

print(haversine(lat1:36.12, lon1:-86.67, lat2:33.94, lon2:-118.40))
```

{{out}}

```txt

2887.25995060711

```



## tbas


```qbasic

option angle radians ' the default
sub haversine(lat1, lon1, lat2, lon2)
	dim EarthRadiusKm = 6372.8        ' Earth radius in kilometers
	dim latRad1 = RAD(lat1)
	dim latRad2 = RAD(lat2)
	dim lonRad1 = RAD(lon1)
	dim lonRad2 = RAD(lon2)
	dim _diffLa = latRad2 - latRad1
	dim _doffLo = lonRad2 - lonRad1
	dim sinLaSqrd = sin(_diffLa / 2) ^ 2
	dim sinLoSqrd = sin(_doffLo / 2) ^ 2
	dim computation = asin(sqrt(sinLaSqrd + cos(latRad1) * cos(latRad2) * sinLoSqrd))
	return 2 * EarthRadiusKm * computation
end sub

print using "Nashville International Airport to Los Angeles International Airport ####.########### km", haversine(36.12, -86.67, 33.94, -118.40)
print using "Perth, WA Australia to Baja California, Mexico #####.########### km", haversine(-31.95, 115.86, 31.95, -115.86)

```


```txt

Nashville International Airport to Los Angeles International Airport  2887.25995060712 km
Perth, WA Australia to Baja California, Mexico 15188.70229560390 km


```



## Tcl

{{trans|Groovy}}

```tcl
package require Tcl 8.5
proc haversineFormula {lat1 lon1 lat2 lon2} {
    set rads [expr atan2(0,-1)/180]
    set R 6372.8    ;# In kilometers

    set dLat [expr {($lat2-$lat1) * $rads}]
    set dLon [expr {($lon2-$lon1) * $rads}]
    set lat1 [expr {$lat1 * $rads}]
    set lat2 [expr {$lat2 * $rads}]

    set a [expr {sin($dLat/2)**2 + sin($dLon/2)**2*cos($lat1)*cos($lat2)}]
    set c [expr {2*asin(sqrt($a))}]
    return [expr {$R * $c}]
}

# Don't bother with too much inappropriate accuracy!
puts [format "distance=%.1f km" [haversineFormula 36.12 -86.67 33.94 -118.40]]
```

{{out}}

```txt
distance=2887.3 km
```



## TechBASIC


```TechBASIC

FUNCTION HAVERSINE
!---------------------------------------------------------------
!*** Haversine Formula - Calculate distances by LAT/LONG
!

!*** LAT/LON of the two locations and Unit of measure are GLOBAL
!*** as they are defined in the main logic of the program, so they
!*** available for use in the Function.
!*** Usage: X=HAVERSINE

    
    Radius=6378.137
    Lat1=(Lat1*MATH.PI/180)
    Lon1=(Lon1*MATH.PI/180)
    Lat2=(Lat2*MATH.PI/180)
    Lon2=(Lon2*MATH.PI/180)
    DLon=Lon1-Lon2
    ANSWER=ACOS(SIN(Lat1)*SIN(Lat2)+COS(Lat1)*COS(Lat2)*COS(DLon))*Radius

    DISTANCE="kilometers"
    SELECT CASE UNIT
           CASE "M"
                HAVERSINE=ANSWER*0.621371192
                Distance="miles"
           CASE "N"
                HAVERSINE=ANSWER*0.539956803
                Distance="nautical miles"
    END SELECT       

END FUNCTION

```



The following is the main code that invokes the function. It takes your location and determines how far away you are from Tampa, Florida. You can change UNIT to either M=Miles, N=Nautical Miles, or K (or leave blank) as default is in Kilometers:


```txt

!*** In techBASIC, all variables defined in the main program act as GLOBAL
!*** variables and are available to all SUBROUTINES and FUNCTIONS. So in the
!*** HAVERSINE Function being used, no paramaters need to be passed to it, so
!*** it acts as a variable when I use it as Result=HAVERSINE. The way that
!*** the Function is setup, it returns its value back as HAVERSINE.

BASE 1

!*** Get the GPS LAT/LONG of current location
location = sensors.location(30)
Lat1=location(1) 
Lon1=location(2) 

!*** LAT/LONG For Tampa, FL
Lat2=27.9506
Lon2=-82.4572

!*** Units: K=kilometers  M=miles  N=nautical miles
DIM UNIT      AS STRING 
DIM Distance  AS STRING
DIM Result    AS SINGLE
UNIT = "M"	

!*** Calculate distance using Haversine Function
Result=HAVERSINE

PRINT "The distance from your current location to Tampa, FL in ";Distance;" is: ";
PRINT USING "#,###.##";Result;"."

STOP

```


<B>OUTPUT:</B> *** NOTE: When I run this, I am in my house in Venice, Florida, and that distance is correct (as the crow flies). ***

```txt

The distance from your current location to Tampa, FL in miles is:    57.94

```



## Teradata Stored Procedure


```SQL

# syntax: call SP_HAVERSINE(36.12,33.94,-86.67,-118.40,x);

CREATE PROCEDURE SP_HAVERSINE
(
IN lat1 FLOAT,
IN lat2 FLOAT,
IN lon1 FLOAT,
IN lon2 FLOAT,
OUT distance FLOAT)
 
BEGIN 
    DECLARE dLat FLOAT;
    DECLARE dLon FLOAT;
    DECLARE c FLOAT;
    DECLARE a FLOAT;    
    DECLARE km FLOAT;

    SET dLat = RADIANS(lat2-lat1);
    SET dLon = RADIANS(lon2-lon1);

    SET a = SIN(dLat / 2) * SIN(dLat / 2) + SIN(dLon / 2) * SIN(dLon / 2) * COS(RADIANS(lat1)) * COS(RADIANS(lat2));
    SET c = 2 * ASIN(SQRT(a));
    SET km = 6372.8 * c;
    
    select km into distance;
END;

```

{{out}}

```txt

distance: 2887.2599 km

```



## TypeScript

{{trans|Matlab}}

```Typescript

let radians = function (degree: number) {

    // degrees to radians
    let rad: number = degree * Math.PI / 180;

    return rad;
}

export const haversine = (lat1: number, lon1: number, lat2: number, lon2: number) => {

    // var dlat: number, dlon: number, a: number, c: number, R: number;
    let dlat, dlon, a, c, R: number;

    R = 6372.8; // km
    dlat = radians(lat2 - lat1);
    dlon = radians(lon2 - lon1);
    lat1 = radians(lat1);
    lat2 = radians(lat2);
    a = Math.sin(dlat / 2) * Math.sin(dlat / 2) + Math.sin(dlon / 2) * Math.sin(dlon / 2) * Math.cos(lat1) * Math.cos(lat2)
    c = 2 * Math.asin(Math.sqrt(a));
    return R * c;
}

console.log("Distance:" + haversine(36.12, -86.67, 33.94, -118.40));

```


{{out}}

```txt

Distance: 2887.2599506071106

```


=={{header|Transact-SQL}}==
{{trans|C#}}

```SQL
CREATE FUNCTION [dbo].[Haversine](@Lat1 AS DECIMAL(9,7), @Lon1 AS DECIMAL(10,7), @Lat2 AS DECIMAL(9,7), @Lon2 AS DECIMAL(10,7))
RETURNS DECIMAL(12,7)
AS
BEGIN
	DECLARE @R	DECIMAL(11,7);
	DECLARE @dLat	DECIMAL(9,7);
	DECLARE @dLon	DECIMAL(10,7);
	DECLARE @a	DECIMAL(10,7);
	DECLARE @c	DECIMAL(10,7);

	SET @R		= 6372.8;
	SET @dLat	= RADIANS(@Lat2 - @Lat1);
	SET @dLon	= RADIANS(@Lon2 - @Lon1);
	SET @Lat1	= RADIANS(@Lat1);
	SET @Lat2	= RADIANS(@Lat2);
	SET @a		= SIN(@dLat / 2) * SIN(@dLat / 2) + SIN(@dLon / 2) * SIN(@dLon / 2) * COS(@Lat1) * COS(@Lat2);
	SET @c		= 2 * ASIN(SQRT(@a));

	RETURN @R * @c;
END
GO

SELECT dbo.Haversine(36.12,-86.67,33.94,-118.4)

```

{{out}}

```txt

 2887.2594934

```



## UBASIC


```basic

   10  Point 7    'Sets decimal display to 32 places (0+.1^56)
   20  Rf=#pi/180 'Degree -> Radian Conversion
  100 ?Using(,7),.DxH(36+7.2/60,-(86+40.2/60),33+56.4/60,-(118+24/60));" km"
  999  End
 1000 '*** Haversine Distance Function ***
 1010 .DxH(Lat_s,Long_s,Lat_f,Long_f)
 1020  L_s=Lat_s*rf:L_f=Lat_f*rf:LD=L_f-L_s:MD=(Long_f-Long_s)*rf
 1030  Return(12745.6*asin( (sin(.5*LD)^2+cos(L_s)*cos(L_f)*sin(.5*MD)^2)^.5))
 '' ''

 Run
  2887.2599506 km
 OK

```



## VBA

{{trans|Phix}}
```vb
Const MER = 6371         '-- mean earth radius(km)
Public DEG_TO_RAD As Double
 
Function haversine(lat1 As Double, long1 As Double, lat2 As Double, long2 As Double) As Double
    lat1 = lat1 * DEG_TO_RAD
    lat2 = lat2 * DEG_TO_RAD
    long1 = long1 * DEG_TO_RAD
    long2 = long2 * DEG_TO_RAD
    haversine = MER * WorksheetFunction.Acos(Sin(lat1) * Sin(lat2) + Cos(lat1) * Cos(lat2) * Cos(long2 - long1))
End Function
 
Public Sub main()
    DEG_TO_RAD = WorksheetFunction.Pi / 180
    d = haversine(36.12, -86.67, 33.94, -118.4)
    Debug.Print "Distance is "; Format(d, "#.######"); " km ("; Format(d / 1.609344, "#.######"); " miles)."
End Sub
```
{{out}}

```txt
Distance is 2886,444443 km (1793,553425 miles).
```



## Visual Basic .NET

{{trans|C#}}If you read the fine print in the Wikipedia article, you will find that the Haversine method of finding distances may have an error of up to 0.5%.  This could lead one to believe that discussion about whether to use 6371.0 km or 6372.8 km for an approximation of the Earth's radius is moot.

```vbnet
Imports System.Math

Module Module1

  Const deg2rad As Double = PI / 180

  Structure AP_Loc
    Public IATA_Code As String, Lat As Double, Lon As Double

    Public Sub New(ByVal iata_code As String, ByVal lat As Double, ByVal lon As Double)
      Me.IATA_Code = iata_code : Me.Lat = lat * deg2rad : Me.Lon = lon * deg2rad
    End Sub

    Public Overrides Function ToString() As String
      Return String.Format("{0}: ({1}, {2})", IATA_Code, Lat / deg2rad, Lon / deg2rad)
    End Function
  End Structure

  Function Sin2(ByVal x As Double) As Double
    Return Pow(Sin(x / 2), 2)
  End Function

  Function calculate(ByVal one As AP_Loc, ByVal two As AP_Loc) As Double
    Dim R As Double = 6371, ' In kilometers, (as recommended by the International Union of Geodesy and Geophysics)
        a As Double = Sin2(two.Lat - one.Lat) + Sin2(two.Lon - one.Lon) * Cos(one.Lat) * Cos(two.Lat)
    Return R * 2 * Asin(Sqrt(a))
  End Function

  Sub ShowOne(pntA As AP_Loc, pntB as AP_Loc)
    Dim adst As Double = calculate(pntA, pntB), sfx As String = "km"
    If adst < 1000 Then adst *= 1000 : sfx = "m"
    Console.WriteLine("The approximate distance between airports {0} and {1} is {2:n2} {3}.", pntA, pntB, adst, sfx)
    Console.WriteLine("The uncertainty is under 0.5%, or {0:n1} {1}." & vbLf, adst / 200, sfx)
  End Sub

' Airport coordinate data excerpted from the data base at http://www.partow.net/miscellaneous/airportdatabase/

' The four additional airports are the furthest and closest pairs, according to the "Fun Facts..." section.

' KBNA, BNA, NASHVILLE INTERNATIONAL, NASHVILLE, USA, 036, 007, 028, N, 086, 040, 041, W, 00183, 36.124, -86.678
' KLAX, LAX, LOS ANGELES INTERNATIONAL, LOS ANGELES, USA, 033, 056, 033, N, 118, 024, 029, W, 00039, 33.942, -118.408
' SKNV, NVA, BENITO SALAS, NEIVA, COLOMBIA, 002, 057, 000, N, 075, 017, 038, W, 00439, 2.950, -75.294
' WIPP, PLM, SULTAN MAHMUD BADARUDDIN II, PALEMBANG, INDONESIA, 002, 053, 052, S, 104, 042, 004, E, 00012, -2.898, 104.701 
' LOWL, LNZ, HORSCHING INTERNATIONAL AIRPORT (AUS - AFB), LINZ, AUSTRIA, 048, 014, 000, N, 014, 011, 000, E, 00096, 48.233, 14.183
' LOXL, N/A, LINZ, LINZ, AUSTRIA, 048, 013, 059, N, 014, 011, 015, E, 00299, 48.233, 14.188

  Sub Main()
    ShowOne(New AP_Loc("BNA", 36.124, -86.678),  New AP_Loc("LAX", 33.942, -118.408))
    ShowOne(New AP_Loc("NVA",  2.95,  -75.294),  New AP_Loc("PLM", -2.898,  104.701))
    ShowOne(New AP_Loc("LNZ", 48.233,  14.183),  New AP_Loc("N/A", 48.233,   14.188))
  End Sub
End Module
```

{{out}}

```txt
The approximate distance between airports BNA: (36.124, -86.678) and LAX: (33.942, -118.408) is 2,886.36 km.
The uncertainty is under 0.5%, or 14.4 km.

The approximate distance between airports NVA: (2.95, -75.294) and PLM: (-2.898, 104.701) is 20,009.28 km.
The uncertainty is under 0.5%, or 100.0 km.

The approximate distance between airports LNZ: (48.233, 14.183) and N/A: (48.233, 14.188) is 370.34 m.
The uncertainty is under 0.5%, or 1.9 m.
```
Looking at the altitude difference between the last two airports, (299 - 96 = 203), the reported distance of 370 meters ought to be around 422 meters if you actually went there and saw it for yourself.


## X86 Assembly

Assemble with tasm /m /l; tlink /t

```asm
0000                                 .model  tiny
0000                                 .code
                                     .486
                                     org     100h            ;.com files start here
0100  9B DB E3               start:  finit                   ;initialize floating-point unit (FPU)
                             ;Great circle distance =
                             ; 2.0*Radius * ASin( sqrt( Haversine(Lat2-Lat1) +
                             ;                          Haversine(Lon2-Lon1)*Cos(Lat1)*Cos(Lat2) ) )
0103  D9 06 0191r                    fld     Lat2            ;push real onto FPU stack
0107  D8 26 018Dr                    fsub    Lat1            ;subtract real from top of stack (st(0) = st)
010B  E8 0070                        call    Haversine       ;(1.0-cos(st)) / 2.0
010E  D9 06 0199r                    fld     Lon2            ;repeat for longitudes
0112  D8 26 0195r                    fsub    Lon1
0116  E8 0065                        call    Haversine       ;st(1)=Lats; st=Lons
0119  D9 06 018Dr                    fld     Lat1
011D  D9 FF                          fcos                    ;replace st with its cosine
011F  D9 06 0191r                    fld     Lat2
0123  D9 FF                          fcos            ;st=cos(Lat2); st(1)=cos(Lat1); st(2)=Lats; st(3)=Lons
0125  DE C9                          fmul            ;st=cos(Lat2)*cos(Lat1); st(1)=Lats; st(2)=Lons
0127  DE C9                          fmul            ;st=cos(Lat2)*cos(Lat1)*Lats; st(1)=Lons
0129  DE C1                          fadd            ;st=cos(Lat2)*cos(Lat1)*Lats + Lons
012B  D9 FA                          fsqrt                   ;replace st with its square root
                             ;asin(x) = atan(x/sqrt(1-x^2))
012D  D9 C0                          fld     st              ;duplicate tos
012F  D8 C8                          fmul    st, st          ;x^2
0131  D9 E8                          fld1                    ;get 1.0
0133  DE E1                          fsubr                   ;1 - x^2
0135  D9 FA                          fsqrt                   ;sqrt(1-x^2)
0137  D9 F3                          fpatan                  ;take atan(st(1)/st)
0139  D8 0E 019Dr                    fmul    Radius2         ;*2.0*Radius

                             ;Display value in FPU's top of stack (st)
      =0004                  before  equ     4               ;places before
      =0002                  after   equ     2               ; and after decimal point
      =0001                  scaler  =       1               ;"=" allows scaler to be redefined, unlike equ
                                     rept    after           ;repeat block "after" times
                             scaler  =       scaler*10
                                     endm                    ;scaler now = 10^after

013D  66| 6A 64                      push    dword ptr scaler;use stack for convenient memory location
0140  67| DA 0C 24                   fimul   dword ptr [esp] ;st:= st*scaler
0144  67| DB 1C 24                   fistp   dword ptr [esp] ;round st to nearest integer
0148  66| 58                         pop     eax             ; and put it into eax

014A  66| BB 0000000A                mov     ebx, 10         ;set up for idiv instruction
0150  B9 0006                        mov     cx, before+after;set up loop counter
0153  66| 99                 ro10:   cdq                     ;convert double to quad; i.e: edx:= 0
0155  66| F7 FB                      idiv    ebx             ;eax:= edx:eax/ebx; remainder in edx
0158  52                             push    dx              ;save least significant digit on stack
0159  E2 F8                          loop    ro10            ;cx--; loop back if not zero

015B  B1 06                          mov     cl, before+after;(ch=0)
015D  B3 00                          mov     bl, 0           ;used to suppress leading zeros
015F  58                     ro20:   pop     ax              ;get digit
0160  0A D8                          or      bl, al          ;turn off suppression if not a zero
0162  80 F9 03                       cmp     cl, after+1     ;is digit immediately to left of decimal point?
0165  75 01                          jne     ro30            ;skip if not
0167  43                              inc    bx              ;turn off leading zero suppression
0168  04 30                  ro30:   add     al, '0'         ;if leading zero then ' ' else add 0
016A  84 DB                          test    bl, bl
016C  75 02                          jne     ro40
016E  B0 20                           mov    al, ' '
0170  CD 29                  ro40:   int     29h             ;display character in al register
0172  80 F9 03                       cmp     cl, after+1     ;is digit immediately to left of decimal point?
0175  75 04                          jne     ro50            ;skip if not
0177  B0 2E                           mov    al, '.'         ;display decimal point
0179  CD 29                           int    29h
017B  E2 E2                  ro50:   loop    ro20            ;loop until all digits displayed
017D  C3                             ret                     ;return to OS

017E                         Haversine:                      ;return (1.0-Cos(Ang)) / 2.0 in st
017E  D9 FF                          fcos
0180  D9 E8                          fld1
0182  DE E1                          fsubr
0184  D8 36 0189r                    fdiv    N2
0188  C3                             ret

0189  40000000               N2      dd       2.0
018D  3F21628D               Lat1    dd       0.63041        ;36.12*pi/180
0191  3F17A4E8               Lat2    dd       0.59236        ;33.94*pi/180
0195  BFC19F80               Lon1    dd      -1.51268        ;-86.67*pi/180
0199  C004410B               Lon2    dd      -2.06647        ;-118.40*pi/180
019D  46472666               Radius2 dd      12745.6         ;6372.8 average radius of Earth (km) times 2
                             ;(TASM isn't smart enough to do floating point constant calculations)
                                     end     start

```

{{out}}

```txt

2887.25

```



## XPL0


```XPL0
include c:\cxpl\codes;                  \intrinsic 'code' declarations

func real Haversine(Ang);
real Ang;
return (1.0-Cos(Ang)) / 2.0;

func real Dist(Lat1, Lat2, Lon1, Lon2); \Great circle distance
real Lat1, Lat2, Lon1, Lon2;
def R = 6372.8;                         \average radius of Earth (km)
return 2.0*R * ASin( sqrt( Haversine(Lat2-Lat1) +
       Cos(Lat1)*Cos(Lat2)*Haversine(Lon2-Lon1) ));

def D2R = 3.141592654/180.0;            \degrees to radians
RlOut(0, Dist(36.12*D2R, 33.94*D2R, -86.67*D2R, -118.40*D2R ));
```


{{out}}

```txt

 2887.25995

```



## XQuery


```XQuery
declare namespace xsd = "http://www.w3.org/2001/XMLSchema";
declare namespace math = "http://www.w3.org/2005/xpath-functions/math";

declare function local:haversine($lat1 as xsd:float, $lon1 as xsd:float, $lat2 as xsd:float, $lon2 as xsd:float)
    as xsd:float
{
    let $dlat  := ($lat2 - $lat1) * math:pi() div 180
    let $dlon  := ($lon2 - $lon1) * math:pi() div 180
    let $rlat1 := $lat1 * math:pi() div 180
    let $rlat2 := $lat2 * math:pi() div 180
    let $a     := math:sin($dlat div 2) * math:sin($dlat div 2) + math:sin($dlon div 2) * math:sin($dlon div 2) * math:cos($rlat1) * math:cos($rlat2)
    let $c     := 2 * math:atan2(math:sqrt($a), math:sqrt(1-$a))
    return xsd:float($c * 6371.0)
};

local:haversine(36.12, -86.67, 33.94, -118.4)
```


{{out}}

```txt

 2886.444

```



## zkl

{{trans|Erlang}}

```zkl
haversine(36.12, -86.67, 33.94, -118.40).println();
 
fcn haversine(Lat1, Long1, Lat2, Long2){
   const R = 6372.8; 	// In kilometers;
   Diff_Lat  := (Lat2  - Lat1) .toRad();
   Diff_Long := (Long2 - Long1).toRad();
   NLat      := Lat1.toRad();
   NLong     := Lat2.toRad();
   A 	     := (Diff_Lat/2) .sin().pow(2) + 
                (Diff_Long/2).sin().pow(2) * 
		NLat.cos() * NLong.cos();
   C 	     := 2.0 * A.sqrt().asin();
   R*C;
}
```

{{out}}

```txt

2887.26

```



## ZX Spectrum Basic

{{trans|Run_BASIC}}

```zxbasic
10 LET diam=2*6372.8
20 LET Lg1m2=FN r((-86.67)-(-118.4))
30 LET Lt1=FN r(36.12)
40 LET Lt2=FN r(33.94)
50 LET dz=SIN (Lt1)-SIN (Lt2)
60 LET dx=COS (Lg1m2)*COS (Lt1)-COS (Lt2)
70 LET dy=SIN (Lg1m2)*COS (Lt1)
80 LET hDist=ASN ((dx*dx+dy*dy+dz*dz)^0.5/2)*diam
90 PRINT "Haversine distance: ";hDist;" km."
100 STOP 
1000 DEF FN r(a)=a*0.017453293: REM convert degree to radians
```


[[Category:Geometry]]
