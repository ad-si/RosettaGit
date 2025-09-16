+++
title = "Angles (geometric), normalization and conversion"
description = ""
date = 2019-09-19T13:02:36Z
aliases = []
[extra]
id = 22427
[taxonomies]
categories = ["geometry", "task"]
tags = []
languages = [
  "c",
  "csharp",
  "factor",
  "go",
  "julia",
  "perl",
  "perl_6",
  "phix",
  "python",
  "rexx",
  "zkl",
]
+++

## Task

This task is about the normalization and/or conversion of (geometric) angles using
some common scales.


The angular scales that will be used in this task are:
::*   degree
::*   gradian
::*   mil
::*   radian



;Definitions:
The angular scales used or referenced here:
::*   '''turn'''      is a full turn or 360 degrees, also shown as 360º
::*   '''degree'''    is   <big>'''<sup>1</sup>/<sub>360</sub>'''</big>                                       of a turn
::*   '''gradian'''   is   <big>'''<sup>1</sup>/<sub>400</sub>'''</big>                                       of a turn
::*   '''mil'''       is   <big>'''<sup>1</sup>/<sub>6400</sub>'''</big>                                      of a turn
::*   '''radian'''    is   <big>'''<sup>1</sup>/<sub>2<big><big><math>\pi</math></big></big></sub></big>'''   of a turn   (or   <big>'''<sup>0.5</sup>/<sub><big><big><math>\pi</math></big></big></sub>'''</big>   of a turn)


Or, to put it another way,   for a full circle:
::*   there are     '''360'''   degrees
::*   there are     '''400'''   gradians
::*   there are   '''6,400'''   mils
::*   there are   '''2<big><big><math>\pi</math></big></big>'''   radians   (roughly equal to '''6.283<small>+</small>''')


A   '''mil'''   is approximately equal to a   ''milliradian''   (which is   <big>'''<sup>1</sup>/<sub>1000</sub>'''</big>   of a radian).

There is another definition of a   '''mil'''   which
is   '''<sup>1</sup>/<sub>1000</sub>'''   of a radian   ─── this
definition <u>won't</u> be used in this Rosetta Code task.


'''Turns'''   are sometimes known or shown as:
:::*   turn(s)
:::*   360 degrees
:::*   unit circle
:::*   a (full) circle


'''Degrees'''   are sometimes known or shown as:
:::*   degree(s)
:::*   deg
:::*   º       (a symbol)
:::*   °       (a symbol)


'''Gradians'''   are sometimes known or shown as:
:::*   gradian(s)
:::*   grad(s)
:::*   grade(s)
:::*   gon(s)
:::*   metric degree(s)


'''Mils'''   are sometimes known or shown as:
:::*   mil(s)
:::*   NATO mil(s)


'''Radians'''   are sometimes known or shown as:
:::*   radian(s)
:::*   rad(s)



;Notes:
In continental Europe, the French term   '''centigrade'''   was used
for   '''<sup>1</sup>/<sub>100</sub>'''   of a grad (grade);   this was
one reason for the adoption of the term   '''Celsius'''   to
replace   '''centigrade'''   as the name of a temperature scale.

Gradians were commonly used in civil engineering.

Mils were normally used for artillery   (elevations for ranging).


;Positive and negative angles:
Although the definition of the measurement of an angle doesn't support the
concept of a negative angle,   it's frequently useful to impose a convention that
allows positive and negative angular values to represent orientations and/or rotations
in opposite directions relative to some reference.   It is this reason that
negative angles will keep their sign and <u>not</u> be normalized to positive angles.


;Normalization:
Normalization   (for this Rosetta Code task)   will keep the same
sign,   but it will reduce the magnitude to less than a full circle;   in
other words, less than 360º.

Normalization   <u>shouldn't</u>   change   '''-45º'''   to   '''315º''',

An angle of   '''0º''',   '''+0º''',   '''0.000000''',    or   '''-0º'''   should be
shown as   '''0º'''.


;Task:
::*   write a function (or equivalent) to do the normalization for each scale
:::::* Suggested names:
:::::* '''d2d''',   '''g2g''',   '''m2m''',   and  '''r2r'''
::*   write a function (or equivalent) to convert one scale to another
:::::* Suggested names for comparison of different computer language function names:
:::::* '''d2g''',   '''d2m''',   and   '''d2r'''   for degrees
:::::* '''g2d''',   '''g2m''',   and   '''g2r'''   for gradians
:::::* '''m2d''',   '''m2g''',   and   '''m2r'''   for mils
:::::* '''r2d''',   '''r2g''',   and   '''r2m'''   for radians
::*   normalize all angles used   (except for the "original" or "base" angle)
::*   show the angles in every scale and convert them to all other scales
::*   show all output here on this page


For the (above) conversions,   use these dozen numbers   (in the order shown):
:*   '''-2   -1   0   1   2   6.2831853   16   57.2957795   359   399   6399   1000000'''





## C


```c
#define PI 3.141592653589793
#define TWO_PI 6.283185307179586

double normalize2deg(double a) {
  while (a < 0) a += 360;
  while (a >= 360) a -= 360;
  return a;
}
double normalize2grad(double a) {
  while (a < 0) a += 400;
  while (a >= 400) a -= 400;
  return a;
}
double normalize2mil(double a) {
  while (a < 0) a += 6400;
  while (a >= 6400) a -= 6400;
  return a;
}
double normalize2rad(double a) {
  while (a < 0) a += TWO_PI;
  while (a >= TWO_PI) a -= TWO_PI;
  return a;
}

double deg2grad(double a) {return a * 10 / 9;}
double deg2mil(double a) {return a * 160 / 9;}
double deg2rad(double a) {return a * PI / 180;}

double grad2deg(double a) {return a * 9 / 10;}
double grad2mil(double a) {return a * 16;}
double grad2rad(double a) {return a * PI / 200;}

double mil2deg(double a) {return a * 9 / 160;}
double mil2grad(double a) {return a / 16;}
double mil2rad(double a) {return a * PI / 3200;}

double rad2deg(double a) {return a * 180 / PI;}
double rad2grad(double a) {return a * 200 / PI;}
double rad2mil(double a) {return a * 3200 / PI;}
```




## C#


```c#
using System;

public static class Angles
{
    public static void Main() => Print(-2, -1, 0, 1, 2, 6.2831853, 16, 57.2957795, 359, 6399, 1_000_000);

    public static void Print(params double[] angles) {
        string[] names = { "Degrees", "Gradians", "Mils", "Radians" };
        Func<double, double> rnd = a => Math.Round(a, 4);
        Func<double, double>[] normal = { NormalizeDeg, NormalizeGrad, NormalizeMil, NormalizeRad };

        Func<double, double>[,] convert = {
            { a => a, DegToGrad, DegToMil, DegToRad },
            { GradToDeg, a => a, GradToMil, GradToRad },
            { MilToDeg, MilToGrad, a => a, MilToRad },
            { RadToDeg, RadToGrad, RadToMil, a => a }
        };

        Console.WriteLine($@"{"Angle",-12}{"Normalized",-12}{"Unit",-12}{
            "Degrees",-12}{"Gradians",-12}{"Mils",-12}{"Radians",-12}");

        foreach (double angle in angles) {
            for (int i = 0; i < 4; i++) {
                double nAngle = normal[i](angle);

                Console.WriteLine($@"{
                    rnd(angle),-12}{
                    rnd(nAngle),-12}{
                    names[i],-12}{
                    rnd(convert[i, 0](nAngle)),-12}{
                    rnd(convert[i, 1](nAngle)),-12}{
                    rnd(convert[i, 2](nAngle)),-12}{
                    rnd(convert[i, 3](nAngle)),-12}");
            }
        }
    }

    public static double NormalizeDeg(double angle) => Normalize(angle, 360);
    public static double NormalizeGrad(double angle) => Normalize(angle, 400);
    public static double NormalizeMil(double angle) => Normalize(angle, 6400);
    public static double NormalizeRad(double angle) => Normalize(angle, 2 * Math.PI);

    private static double Normalize(double angle, double N) {
        while (angle <= -N) angle += N;
        while (angle >= N) angle -= N;
        return angle;
    }

    public static double DegToGrad(double angle) => angle * 10 / 9;
    public static double DegToMil(double angle) => angle * 160 / 9;
    public static double DegToRad(double angle) => angle * Math.PI / 180;

    public static double GradToDeg(double angle) => angle * 9 / 10;
    public static double GradToMil(double angle) => angle * 16;
    public static double GradToRad(double angle) => angle * Math.PI / 200;

    public static double MilToDeg(double angle) => angle * 9 / 160;
    public static double MilToGrad(double angle) => angle / 16;
    public static double MilToRad(double angle) => angle * Math.PI / 3200;

    public static double RadToDeg(double angle) => angle * 180 / Math.PI;
    public static double RadToGrad(double angle) => angle * 200 / Math.PI;
    public static double RadToMil(double angle) => angle * 3200 / Math.PI;
}
```

{{out}}

```txt

Angle       Normalized  Unit        Degrees     Gradians    Mils        Radians
-2          -2          Degrees     -2          -2.2222     -35.5556    -0.0349
-2          -2          Gradiens    -1.8        -2          -32         -0.0314
-2          -2          Mils        -0.1125     -0.125      -2          -0.002
-2          -2          Radians     -114.5916   -127.324    -2037.1833  -2
-1          -1          Degrees     -1          -1.1111     -17.7778    -0.0175
-1          -1          Gradiens    -0.9        -1          -16         -0.0157
-1          -1          Mils        -0.0562     -0.0625     -1          -0.001
-1          -1          Radians     -57.2958    -63.662     -1018.5916  -1
0           0           Degrees     0           0           0           0
0           0           Gradiens    0           0           0           0
0           0           Mils        0           0           0           0
0           0           Radians     0           0           0           0
1           1           Degrees     1           1.1111      17.7778     0.0175
1           1           Gradiens    0.9         1           16          0.0157
1           1           Mils        0.0562      0.0625      1           0.001
1           1           Radians     57.2958     63.662      1018.5916   1
2           2           Degrees     2           2.2222      35.5556     0.0349
2           2           Gradiens    1.8         2           32          0.0314
2           2           Mils        0.1125      0.125       2           0.002
2           2           Radians     114.5916    127.324     2037.1833   2
6.2832      6.2832      Degrees     6.2832      6.9813      111.7011    0.1097
6.2832      6.2832      Gradiens    5.6549      6.2832      100.531     0.0987
6.2832      6.2832      Mils        0.3534      0.3927      6.2832      0.0062
6.2832      6.2832      Radians     360         400         6400        6.2832
16          16          Degrees     16          17.7778     284.4444    0.2793
16          16          Gradiens    14.4        16          256         0.2513
16          16          Mils        0.9         1           16          0.0157
16          3.4336      Radians     196.7325    218.5916    3497.4662   3.4336
57.2958     57.2958     Degrees     57.2958     63.662      1018.5916   1
57.2958     57.2958     Gradiens    51.5662     57.2958     916.7325    0.9
57.2958     57.2958     Mils        3.2229      3.581       57.2958     0.0562
57.2958     0.7471      Radians     42.8063     47.5626     761.0018    0.7471
359         359         Degrees     359         398.8889    6382.2222   6.2657
359         359         Gradiens    323.1       359         5744        5.6392
359         359         Mils        20.1938     22.4375     359         0.3524
359         0.8584      Radians     49.1848     54.6498     874.3972    0.8584
6399        279         Degrees     279         310         4960        4.8695
6399        399         Gradiens    359.1       399         6384        6.2675
6399        6399        Mils        359.9438    399.9375    6399        6.2822
6399        2.7174      Radians     155.6931    172.9923    2767.8774   2.7174
1000000     280         Degrees     280         311.1111    4977.7778   4.8869
1000000     0           Gradiens    0           0           0           0
1000000     1600        Mils        90          100         1600        1.5708
1000000     5.9256      Radians     339.5132    377.2368    6035.7895   5.9256
```



## Factor

Radians and degrees are already defined in the <code>units.si</code> vocabulary. Gradiens and mils are defined in terms of degrees. Conversions from unit to unit are handled by inverse functions; <code>[undo]</code> knows how to deconstruct units in terms of other units. (Assuming, of course, new units are defined entirely with words that have inverses.)

```factor
USING: accessors combinators formatting inverse kernel math
math.constants quotations qw sequences units.si ;
IN: rosetta-code.angles

ALIAS: degrees arc-deg
: gradiens ( n -- d ) 9/10 * degrees ;
: mils ( n -- d ) 9/160 * degrees ;
: normalize ( d -- d' ) [ 2 pi * mod ] change-value ;
CONSTANT: units { degrees gradiens mils radians }

: .row ( angle unit -- )
    2dup "%-12u%-12s" printf ( x -- x ) execute-effect
    normalize units [ 1quotation [undo] call( x -- x ) ] with
    map "%-12.4f%-12.4f%-12.4f%-12.4f\n" vprintf ;

: .header ( -- )
    qw{ angle unit } units append
    "%-12s%-12s%-12s%-12s%-12s%-12s\n" vprintf ;

: angles ( -- )
    .header
    { -2 -1 0 1 2 6.2831853 16 57.2957795 359 399 6399 1000000 }
    units [ .row ] cartesian-each ;

MAIN: angles
```

{{out}}

```txt

angle       unit        degrees     gradiens    mils        radians
-2          degrees     -2.0000     -2.2222     -35.5556    -0.0349
-2          gradiens    -1.8000     -2.0000     -32.0000    -0.0314
-2          mils        -0.1125     -0.1250     -2.0000     -0.0020
-2          radians     -114.5916   -127.3240   -2037.1833  -2.0000
-1          degrees     -1.0000     -1.1111     -17.7778    -0.0175
-1          gradiens    -0.9000     -1.0000     -16.0000    -0.0157
-1          mils        -0.0563     -0.0625     -1.0000     -0.0010
-1          radians     -57.2958    -63.6620    -1018.5916  -1.0000
0           degrees     0.0000      0.0000      0.0000      0.0000
0           gradiens    0.0000      0.0000      0.0000      0.0000
0           mils        0.0000      0.0000      0.0000      0.0000
0           radians     0.0000      0.0000      0.0000      0.0000
1           degrees     1.0000      1.1111      17.7778     0.0175
1           gradiens    0.9000      1.0000      16.0000     0.0157
1           mils        0.0563      0.0625      1.0000      0.0010
1           radians     57.2958     63.6620     1018.5916   1.0000
2           degrees     2.0000      2.2222      35.5556     0.0349
2           gradiens    1.8000      2.0000      32.0000     0.0314
2           mils        0.1125      0.1250      2.0000      0.0020
2           radians     114.5916    127.3240    2037.1833   2.0000
6.2831853   degrees     6.2832      6.9813      111.7011    0.1097
6.2831853   gradiens    5.6549      6.2832      100.5310    0.0987
6.2831853   mils        0.3534      0.3927      6.2832      0.0062
6.2831853   radians     360.0000    400.0000    6400.0000   6.2832
16          degrees     16.0000     17.7778     284.4444    0.2793
16          gradiens    14.4000     16.0000     256.0000    0.2513
16          mils        0.9000      1.0000      16.0000     0.0157
16          radians     196.7325    218.5916    3497.4662   3.4336
57.2957795  degrees     57.2958     63.6620     1018.5916   1.0000
57.2957795  gradiens    51.5662     57.2958     916.7325    0.9000
57.2957795  mils        3.2229      3.5810      57.2958     0.0562
57.2957795  radians     42.8063     47.5626     761.0018    0.7471
359         degrees     359.0000    398.8889    6382.2222   6.2657
359         gradiens    323.1000    359.0000    5744.0000   5.6392
359         mils        20.1938     22.4375     359.0000    0.3524
359         radians     49.1848     54.6498     874.3972    0.8584
399         degrees     39.0000     43.3333     693.3333    0.6807
399         gradiens    359.1000    399.0000    6384.0000   6.2675
399         mils        22.4438     24.9375     399.0000    0.3917
399         radians     181.0160    201.1289    3218.0627   3.1593
6399        degrees     279.0000    310.0000    4960.0000   4.8695
6399        gradiens    359.1000    399.0000    6384.0000   6.2675
6399        mils        359.9438    399.9375    6399.0000   6.2822
6399        radians     155.6931    172.9923    2767.8774   2.7174
1000000     degrees     280.0000    311.1111    4977.7778   4.8869
1000000     gradiens    0.0000      0.0000      0.0000      0.0000
1000000     mils        90.0000     100.0000    1600.0000   1.5708
1000000     radians     339.5131    377.2368    6035.7881   5.9256

```



## Go


```go
package main

import (
    "fmt"
    "math"
    "strconv"
    "strings"
)

func d2d(d float64) float64 { return math.Mod(d, 360) }

func g2g(g float64) float64 { return math.Mod(g, 400) }

func m2m(m float64) float64 { return math.Mod(m, 6400) }

func r2r(r float64) float64 { return math.Mod(r, 2*math.Pi) }

func d2g(d float64) float64 { return d2d(d) * 400 / 360 }

func d2m(d float64) float64 { return d2d(d) * 6400 / 360 }

func d2r(d float64) float64 { return d2d(d) * math.Pi / 180 }

func g2d(g float64) float64 { return g2g(g) * 360 / 400 }

func g2m(g float64) float64 { return g2g(g) * 6400 / 400 }

func g2r(g float64) float64 { return g2g(g) * math.Pi / 200 }

func m2d(m float64) float64 { return m2m(m) * 360 / 6400 }

func m2g(m float64) float64 { return m2m(m) * 400 / 6400 }

func m2r(m float64) float64 { return m2m(m) * math.Pi / 3200 }

func r2d(r float64) float64 { return r2r(r) * 180 / math.Pi }

func r2g(r float64) float64 { return r2r(r) * 200 / math.Pi }

func r2m(r float64) float64 { return r2r(r) * 3200 / math.Pi }

// Aligns number to decimal point assuming 7 characters before and after.
func s(f float64) string {
    wf := strings.Split(strconv.FormatFloat(f, 'g', 15, 64), ".")
    if len(wf) == 1 {
        return fmt.Sprintf("%7s        ", wf[0])
    }
    le := len(wf[1])
    if le > 7 {
        le = 7
    }
    return fmt.Sprintf("%7s.%-7s", wf[0], wf[1][:le])
}

func main() {
    angles := []float64{-2, -1, 0, 1, 2, 6.2831853, 16, 57.2957795,
        359, 399, 6399, 1000000}
    ft := "%s %s %s %s %s\n"
    fmt.Printf(ft, "    degrees    ", "normalized degs", "    gradians   ", "     mils      ", "     radians")
    for _, a := range angles {
        fmt.Printf(ft, s(a), s(d2d(a)), s(d2g(a)), s(d2m(a)), s(d2r(a)))
    }
    fmt.Printf(ft, "\n   gradians    ", "normalized grds", "    degrees    ", "     mils      ", "     radians")
    for _, a := range angles {
        fmt.Printf(ft, s(a), s(g2g(a)), s(g2d(a)), s(g2m(a)), s(g2r(a)))
    }
    fmt.Printf(ft, "\n     mils      ", "normalized mils", "    degrees    ", "   gradians    ", "     radians")
    for _, a := range angles {
        fmt.Printf(ft, s(a), s(m2m(a)), s(m2d(a)), s(m2g(a)), s(m2r(a)))
    }
    fmt.Printf(ft, "\n    radians    ", "normalized rads", "    degrees    ", "   gradians    ", "      mils  ")
    for _, a := range angles {
        fmt.Printf(ft, s(a), s(r2r(a)), s(r2d(a)), s(r2g(a)), s(r2m(a)))
    }
}
```


{{out}}

```txt

    degrees     normalized degs     gradians         mils            radians
     -2              -2              -2.2222222     -35.5555555      -0.0349065
     -1              -1              -1.1111111     -17.7777777      -0.0174532
      0               0               0               0               0
      1               1               1.1111111      17.7777777       0.0174532
      2               2               2.2222222      35.5555555       0.0349065
      6.2831853       6.2831853       6.981317      111.701072        0.1096622
     16              16              17.7777777     284.4444444       0.2792526
     57.2957795      57.2957795      63.6619772    1018.5916355       0.9999999
    359             359             398.8888888    6382.2222222       6.2657320
    399              39              43.3333333     693.3333333       0.6806784
   6399             279             310            4960               4.8694686
1000000             280             311.1111111    4977.7777777       4.8869219

   gradians     normalized grds     degrees          mils            radians
     -2              -2              -1.8           -32              -0.0314159
     -1              -1              -0.9           -16              -0.0157079
      0               0               0               0               0
      1               1               0.9            16               0.0157079
      2               2               1.8            32               0.0314159
      6.2831853       6.2831853       5.6548667     100.5309648       0.0986960
     16              16              14.4           256               0.2513274
     57.2957795      57.2957795      51.5662015     916.732472        0.8999999
    359             359             323.1          5744               5.6391588
    399             399             359.1          6384               6.2674773
   6399             399             359.1          6384               6.2674773
1000000               0               0               0               0

     mils       normalized mils     degrees        gradians          radians
     -2              -2              -0.1125         -0.125          -0.0019634
     -1              -1              -0.05625        -0.0625         -0.0009817
      0               0               0               0               0
      1               1               0.05625         0.0625          0.0009817
      2               2               0.1125          0.125           0.0019634
      6.2831853       6.2831853       0.3534291       0.3926990       0.0061685
     16              16               0.9             1               0.0157079
     57.2957795      57.2957795       3.2228875       3.5809862       0.0562499
    359             359              20.19375        22.4375          0.3524474
    399             399              22.44375        24.9375          0.3917173
   6399            6399             359.94375       399.9375          6.2822035
1000000            1600              90             100               1.5707963

    radians     normalized rads     degrees        gradians           mils
     -2              -2            -114.5915590    -127.3239544   -2037.1832715
     -1              -1             -57.2957795     -63.6619772   -1018.5916357
      0               0               0               0               0
      1               1              57.2957795      63.6619772    1018.5916357
      2               2             114.5915590     127.3239544    2037.1832715
      6.2831853       6.2831853     359.9999995     399.9999995    6399.9999926
     16               3.4336293     196.7324722     218.5916357    3497.4661726
     57.2957795       0.7471117      42.8063492      47.5626102     761.0017646
    359               0.8584374      49.1848451      54.6498279     874.3972479
    399               3.1593256     181.0160257     201.1289174    3218.0626794
   6399               2.7173572     155.6931042     172.9923380    2767.8774082
1000000               5.9256211     339.5130823     377.2367581    6035.7881301

```




## Julia


```julia
using Formatting

d2d(d) = d % 360
g2g(g) = g % 400
m2m(m) = m % 6400
r2r(r) = r % 2π
d2g(d) = d2d(d) * 10 / 9
d2m(d) = d2d(d) * 160 / 9
d2r(d) = d2d(d) * π / 180
g2d(g) = g2g(g) * 9 / 10
g2m(g) = g2g(g) * 16
g2r(g) = g2g(g) * π / 200
m2d(m) = m2m(m) * 9 / 160
m2g(m) = m2m(m) / 16
m2r(m) = m2m(m) * π / 3200
r2d(r) = r2r(r) * 180 / π
r2g(r) = r2r(r) * 200 / π
r2m(r) = r2r(r) * 3200 / π

fmt(x::Real, width=16) = Int(round(x)) == x ? rpad(Int(x), width) :
                                              rpad(format(x, precision=7), width)
fmt(x::String, width=16) = rpad(x, width)

const t2u = Dict("degrees" => [d2d, d2g, d2m, d2r],
    "gradians" => [g2d, g2g, g2m, g2r], "mils" => [m2d, m2g, m2m, m2r],
    "radians" => [r2d, r2g, r2m, r2r])

function testconversions(arr)
    println("Number          Units           Degrees          Gradians        Mils            Radians")
    for num in arr, units in ["degrees", "gradians", "mils", "radians"]
        print(fmt(num), fmt(units))
        for f in t2u[units]
            print(fmt(f(num)))
        end
        println()
    end
end

testconversions([-2, -1, 0, 1, 2, 6.2831853, 16, 57.2957795, 359, 399, 6399, 1000000])

```
{{out}}

```txt

Number          Units           Degrees          Gradians        Mils            Radians
-2              degrees         -2              -2.2222222      -35.5555556     -0.0349066
-2              gradians        -1.8000000      -2              -32             -0.0314159
-2              mils            -0.1125000      -0.1250000      -2              -0.0019635
-2              radians         -114.5915590    -127.3239545    -2037.1832716   -2
-1              degrees         -1              -1.1111111      -17.7777778     -0.0174533
-1              gradians        -0.9000000      -1              -16             -0.0157080
-1              mils            -0.0562500      -0.0625000      -1              -0.0009817
-1              radians         -57.2957795     -63.6619772     -1018.5916358   -1
0               degrees         0               0               0               0
0               gradians        0               0               0               0
0               mils            0               0               0               0
0               radians         0               0               0               0
1               degrees         1               1.1111111       17.7777778      0.0174533
1               gradians        0.9000000       1               16              0.0157080
1               mils            0.0562500       0.0625000       1               0.0009817
1               radians         57.2957795      63.6619772      1018.5916358    1
2               degrees         2               2.2222222       35.5555556      0.0349066
2               gradians        1.8000000       2               32              0.0314159
2               mils            0.1125000       0.1250000       2               0.0019635
2               radians         114.5915590     127.3239545     2037.1832716    2
6.2831853       degrees         6.2831853       6.9813170       111.7010720     0.1096623
6.2831853       gradians        5.6548668       6.2831853       100.5309648     0.0986960
6.2831853       mils            0.3534292       0.3926991       6.2831853       0.0061685
6.2831853       radians         359.9999996     399.9999995     6399.9999927    6.2831853
16              degrees         16              17.7777778      284.4444444     0.2792527
16              gradians        14.4000000      16              256             0.2513274
16              mils            0.9000000       1               16              0.0157080
16              radians         196.7324722     218.5916358     3497.4661726    3.4336294
57.2957795      degrees         57.2957795      63.6619772      1018.5916356    1.0000000
57.2957795      gradians        51.5662016      57.2957795      916.7324720     0.9000000
57.2957795      mils            3.2228876       3.5809862       57.2957795      0.0562500
57.2957795      radians         42.8063493      47.5626103      761.0017647     0.7471117
359             degrees         359             398.8888889     6382.2222222    6.2657320
359             gradians        323.1000000     359             5744            5.6391588
359             mils            20.1937500      22.4375000      359             0.3524474
359             radians         49.1848452      54.6498280      874.3972479     0.8584375
399             degrees         39              43.3333333      693.3333333     0.6806784
399             gradians        359.1000000     399             6384            6.2674773
399             mils            22.4437500      24.9375000      399             0.3917173
399             radians         181.0160257     201.1289175     3218.0626795    3.1593256
6399            degrees         279             310             4960            4.8694686
6399            gradians        359.1000000     399             6384            6.2674773
6399            mils            359.9437500     399.9375000     6399            6.2822036
6399            radians         155.6931042     172.9923380     2767.8774082    2.7173573
1000000         degrees         280             311.1111111     4977.7777778    4.8869219
1000000         gradians        0               0               0               0
1000000         mils            90              100             1600            1.5707963
1000000         radians         339.5130823     377.2367581     6035.7881302    5.9256211

```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use feature 'say';
use POSIX 'fmod';

my $tau = 2 * 4*atan2(1, 1);
my @units = (
    { code => 'd', name => 'degrees' , number =>  360 },
    { code => 'g', name => 'gradians', number =>  400 },
    { code => 'm', name => 'mills'   , number => 6400 },
    { code => 'r', name => 'radians' , number => $tau },
);

my %cvt;
for my $a (@units) {
  for my $b (@units) {
    $cvt{ "${$a}{code}2${$b}{code}" } = sub {
        my($angle) = shift;
        my $norm = fmod($angle,${$a}{number}); # built-in '%' returns only integers
        $norm -= ${$a}{number} if $angle < 0;
        $norm * ${$b}{number} / ${$a}{number}
        }
  }
}

printf '%s'. '%12s'x4 . "\n", '     Angle Unit    ', <Degrees Gradians Mills Radians>;
for my $angle (-2, -1, 0, 1, 2, $tau, 16, 360/$tau, 360-1, 400-1, 6400-1, 1_000_000) {
    print "\n";
    for my $from (@units) {
        my @sub_keys = map { "${$from}{code}2${$_}{code}" } @units;
        my @results  = map { &{$cvt{$_}}($angle) } @sub_keys;
        printf '%10g %-8s' . '%12g'x4 . "\n", $angle, ${$from}{name}, @results;
    }
}
```

{{out}}

```txt
     Angle Unit         Degrees    Gradians       Mills     Radians

        -2 degrees         -362    -402.222    -6435.56    -6.31809
        -2 gradians      -361.8        -402       -6432     -6.3146
        -2 mills       -360.113    -400.125       -6402    -6.28515
        -2 radians     -474.592    -527.324    -8437.18    -8.28319

        -1 degrees         -361    -401.111    -6417.78    -6.30064
        -1 gradians      -360.9        -401       -6416    -6.29889
        -1 mills       -360.056    -400.062       -6401    -6.28417
        -1 radians     -417.296    -463.662    -7418.59    -7.28319

         0 degrees            0           0           0           0
         0 gradians           0           0           0           0
         0 mills              0           0           0           0
         0 radians            0           0           0           0

         1 degrees            1     1.11111     17.7778   0.0174533
         1 gradians         0.9           1          16    0.015708
         1 mills        0.05625      0.0625           1 0.000981748
         1 radians      57.2958      63.662     1018.59           1

         2 degrees            2     2.22222     35.5556   0.0349066
         2 gradians         1.8           2          32   0.0314159
         2 mills         0.1125       0.125           2   0.0019635
         2 radians      114.592     127.324     2037.18           2

   6.28319 degrees      6.28319     6.98132     111.701    0.109662
   6.28319 gradians     5.65487     6.28319     100.531    0.098696
   6.28319 mills       0.353429    0.392699     6.28319   0.0061685
   6.28319 radians            0           0           0           0

        16 degrees           16     17.7778     284.444    0.279253
        16 gradians        14.4          16         256    0.251327
        16 mills            0.9           1          16    0.015708
        16 radians      196.732     218.592     3497.47     3.43363

   57.2958 degrees      57.2958      63.662     1018.59           1
   57.2958 gradians     51.5662     57.2958     916.732         0.9
   57.2958 mills        3.22289     3.58099     57.2958     0.05625
   57.2958 radians      42.8064     47.5626     761.002    0.747112

       359 degrees          359     398.889     6382.22     6.26573
       359 gradians       323.1         359        5744     5.63916
       359 mills        20.1938     22.4375         359    0.352447
       359 radians      49.1848     54.6498     874.397    0.858437

       399 degrees           39     43.3333     693.333    0.680678
       399 gradians       359.1         399        6384     6.26748
       399 mills        22.4438     24.9375         399    0.391717
       399 radians      181.016     201.129     3218.06     3.15933

      6399 degrees          279         310        4960     4.86947
      6399 gradians       359.1         399        6384     6.26748
      6399 mills        359.944     399.938        6399      6.2822
      6399 radians      155.693     172.992     2767.88     2.71736

     1e+06 degrees          280     311.111     4977.78     4.88692
     1e+06 gradians           0           0           0           0
     1e+06 mills             90         100        1600      1.5708
     1e+06 radians      339.513     377.237     6035.79     5.92562
```



## Perl 6


```perl6

my @units =
    { code => 'd', name => 'degrees' , number =>  360 },
    { code => 'g', name => 'gradians', number =>  400 },
    { code => 'm', name => 'mills'   , number => 6400 },
    { code => 'r', name => 'radians' , number =>  tau },
;

my Code %cvt = (@units X @units).map: -> ($a, $b) {
    "{$a.<code>}2{$b.<code>}" => sub ($angle) {
        my $norm = $angle % $a.<number>
                 - ( $a.<number> if $angle < 0 );
        $norm * $b.<number> / $a.<number>
    }
}

say '     Angle Unit     ', @units».<name>».tc.fmt('%11s');

for -2, -1, 0, 1, 2, tau, 16, 360/tau, 360-1, 400-1, 6400-1, 1_000_000 -> $angle {
    say '';
    for @units -> $from {
        my @sub_keys = @units.map: { "{$from.<code>}2{.<code>}" };

        my @results = %cvt{@sub_keys}».($angle);

        say join ' ', $angle      .fmt('%10g'),
                      $from.<name>.fmt('%-8s'),
                      @results    .fmt('%11g');
    }
}
```

{{out}}

```txt
     Angle Unit         Degrees    Gradians       Mills     Radians

        -2 degrees           -2    -2.22222    -35.5556  -0.0349066
        -2 gradians        -1.8          -2         -32  -0.0314159
        -2 mills        -0.1125      -0.125          -2  -0.0019635
        -2 radians     -114.592    -127.324    -2037.18          -2

        -1 degrees           -1    -1.11111    -17.7778  -0.0174533
        -1 gradians        -0.9          -1         -16   -0.015708
        -1 mills       -0.05625     -0.0625          -1 -0.000981748
        -1 radians     -57.2958     -63.662    -1018.59          -1

         0 degrees            0           0           0           0
         0 gradians           0           0           0           0
         0 mills              0           0           0           0
         0 radians            0           0           0           0

         1 degrees            1     1.11111     17.7778   0.0174533
         1 gradians         0.9           1          16    0.015708
         1 mills        0.05625      0.0625           1 0.000981748
         1 radians      57.2958      63.662     1018.59           1

         2 degrees            2     2.22222     35.5556   0.0349066
         2 gradians         1.8           2          32   0.0314159
         2 mills         0.1125       0.125           2   0.0019635
         2 radians      114.592     127.324     2037.18           2

   6.28319 degrees      6.28319     6.98132     111.701    0.109662
   6.28319 gradians     5.65487     6.28319     100.531    0.098696
   6.28319 mills       0.353429    0.392699     6.28319   0.0061685
   6.28319 radians            0           0           0           0

        16 degrees           16     17.7778     284.444    0.279253
        16 gradians        14.4          16         256    0.251327
        16 mills            0.9           1          16    0.015708
        16 radians      196.732     218.592     3497.47     3.43363

   57.2958 degrees      57.2958      63.662     1018.59           1
   57.2958 gradians     51.5662     57.2958     916.732         0.9
   57.2958 mills        3.22289     3.58099     57.2958     0.05625
   57.2958 radians      42.8064     47.5626     761.002    0.747112

       359 degrees          359     398.889     6382.22     6.26573
       359 gradians       323.1         359        5744     5.63916
       359 mills        20.1938     22.4375         359    0.352447
       359 radians      49.1848     54.6498     874.397    0.858437

       399 degrees           39     43.3333     693.333    0.680678
       399 gradians       359.1         399        6384     6.26748
       399 mills        22.4438     24.9375         399    0.391717
       399 radians      181.016     201.129     3218.06     3.15933

      6399 degrees          279         310        4960     4.86947
      6399 gradians       359.1         399        6384     6.26748
      6399 mills        359.944     399.938        6399      6.2822
      6399 radians      155.693     172.992     2767.88     2.71736

   1000000 degrees          280     311.111     4977.78     4.88692
   1000000 gradians           0           0           0           0
   1000000 mills             90         100        1600      1.5708
   1000000 radians      339.513     377.237     6035.79     5.92562
```


Alternately, implemented as a series of postfix operators:

''(Much of the complexity is due to the requirement that negative angles must normalize to a negative number.)''


```perl6
sub postfix:<t
( $t ) { my $a = $t % 1 * τ;           $t >=0 ?? $a !! $a - τ }
sub postfix:<d>( $d ) { my $a = $d % 360 * τ / 360;   $d >=0 ?? $a !! $a - τ }
sub postfix:<g>( $g ) { my $a = $g % 400 * τ / 400;   $g >=0 ?? $a !! $a - τ }
sub postfix:<m>( $m ) { my $a = $m % 6400 * τ / 6400; $m >=0 ?? $a !! $a - τ }
sub postfix:<r>( $r ) { my $a = $r % τ;               $r >=0 ?? $a !! $a - τ }

sub postfix:«->t» ($angle) { my $a = $angle / τ;        ($angle < 0 and $a == -1)    ?? -0 !! $a }
sub postfix:«->d» ($angle) { my $a = $angle / τ * 360;  ($angle < 0 and $a == -360)  ?? -0 !! $a }
sub postfix:«->g» ($angle) { my $a = $angle / τ * 400;  ($angle < 0 and $a == -400)  ?? -0 !! $a }
sub postfix:«->m» ($angle) { my $a = $angle / τ * 6400; ($angle < 0 and $a == -6400) ?? -0 !! $a }
sub postfix:«->r» ($angle) { my $a = $angle;            ($angle < 0 and $a == -τ)    ?? -0 !! $a }

for <-2 -1 0 1 2 6.2831853 16 57.2957795 359 399 6399 1000000> -> $a {
    say '';
    say '  Quantity  Unit      ', <turns degrees gradians mils radians>.fmt('%15s');
    for 'turns', &postfix:«t», 'degrees', &postfix:«d», 'gradians', &postfix:«g»,
        'mils',  &postfix:«m», 'radians', &postfix:«r»
      -> $unit, &f {
            printf "%10s %-10s %15s %15s %15s %15s %15s\n", $a, $unit,
            |($a.&f->t, $a.&f->d, $a.&f->g, $a.&f->m, $a.&f->r)».round(.00000001);
    }
}
```



```txt
 Quantity  Unit                turns         degrees        gradians            mils         radians
        -2 turns                    0               0               0               0               0
        -2 degrees        -0.00555556              -2     -2.22222222    -35.55555556     -0.03490659
        -2 gradians            -0.005            -1.8              -2             -32     -0.03141593
        -2 mils             -0.000313         -0.1125          -0.125              -2      -0.0019635
        -2 radians        -0.31830989   -114.59155903   -127.32395447  -2037.18327158              -2

  Quantity  Unit                turns         degrees        gradians            mils         radians
        -1 turns                    0               0               0               0               0
        -1 degrees        -0.00277778              -1     -1.11111111    -17.77777778     -0.01745329
        -1 gradians           -0.0025            -0.9              -1             -16     -0.01570796
        -1 mils             -0.000156        -0.05625         -0.0625              -1     -0.00098175
        -1 radians        -0.15915494    -57.29577951    -63.66197724  -1018.59163579              -1

  Quantity  Unit                turns         degrees        gradians            mils         radians
         0 turns                    0               0               0               0               0
         0 degrees                  0               0               0               0               0
         0 gradians                 0               0               0               0               0
         0 mils                     0               0               0               0               0
         0 radians                  0               0               0               0               0

  Quantity  Unit                turns         degrees        gradians            mils         radians
         1 turns                    0               0               0               0               0
         1 degrees         0.00277778               1      1.11111111     17.77777778      0.01745329
         1 gradians            0.0025             0.9               1              16      0.01570796
         1 mils              0.000156         0.05625          0.0625               1      0.00098175
         1 radians         0.15915494     57.29577951     63.66197724   1018.59163579               1

  Quantity  Unit                turns         degrees        gradians            mils         radians
         2 turns                    0               0               0               0               0
         2 degrees         0.00555556               2      2.22222222     35.55555556      0.03490659
         2 gradians             0.005             1.8               2              32      0.03141593
         2 mils              0.000313          0.1125           0.125               2       0.0019635
         2 radians         0.31830989    114.59155903    127.32395447   2037.18327158               2

  Quantity  Unit                turns         degrees        gradians            mils         radians
 6.2831853 turns            0.2831853      101.946708       113.27412      1812.38592      1.77930572
 6.2831853 degrees         0.01745329       6.2831853        6.981317      111.701072      0.10966227
 6.2831853 gradians        0.01570796      5.65486677       6.2831853     100.5309648      0.09869604
 6.2831853 mils            0.00098175      0.35342917      0.39269908       6.2831853       0.0061685
 6.2831853 radians                  1    359.99999959    399.99999954   6399.99999269       6.2831853

  Quantity  Unit                turns         degrees        gradians            mils         radians
        16 turns                    0               0               0               0               0
        16 degrees         0.04444444              16     17.77777778    284.44444444      0.27925268
        16 gradians              0.04            14.4              16             256      0.25132741
        16 mils                0.0025             0.9               1              16      0.01570796
        16 radians         0.54647909    196.73247221    218.59163579   3497.46617261      3.43362939

  Quantity  Unit                turns         degrees        gradians            mils         radians
57.2957795 turns            0.2957795       106.48062        118.3118       1892.9888      1.85843741
57.2957795 degrees         0.15915494      57.2957795     63.66197722   1018.59163556               1
57.2957795 gradians        0.14323945     51.56620155      57.2957795      916.732472             0.9
57.2957795 mils            0.00895247       3.2228876      3.58098622      57.2957795         0.05625
57.2957795 radians         0.11890653     42.80634926     47.56261029    761.00176466      0.74711174

  Quantity  Unit                turns         degrees        gradians            mils         radians
       359 turns                    0               0               0               0               0
       359 degrees         0.99722222             359    398.88888889   6382.22222222      6.26573201
       359 gradians            0.8975           323.1             359            5744      5.63915881
       359 mils              0.056094        20.19375         22.4375             359      0.35244743
       359 radians         0.13662457      49.1848452       54.649828    874.39724794      0.85843749

  Quantity  Unit                turns         degrees        gradians            mils         radians
       399 turns                    0               0               0               0               0
       399 degrees         0.10833333              39     43.33333333    693.33333333      0.68067841
       399 gradians            0.9975           359.1             399            6384      6.26747734
       399 mils              0.062344        22.44375         24.9375             399      0.39171733
       399 radians         0.50282229    181.01602572    201.12891747   3218.06267946      3.15932565

  Quantity  Unit                turns         degrees        gradians            mils         radians
      6399 turns                    0               0               0               0               0
      6399 degrees              0.775             279             310            4960      4.86946861
      6399 gradians            0.9975           359.1             399            6384      6.26747734
      6399 mils              0.999844       359.94375        399.9375            6399      6.28220356
      6399 radians         0.43248085    155.69310421    172.99233802   2767.87740825      2.71735729

  Quantity  Unit                turns         degrees        gradians            mils         radians
   1000000 turns                    0               0               0               0               0
   1000000 degrees         0.77777778             280    311.11111111   4977.77777778      4.88692191
   1000000 gradians                 0               0               0               0               0
   1000000 mils                  0.25              90             100            1600      1.57079633
   1000000 radians          0.9430919    339.51308233    377.23675814   6035.78813022      5.92562114
```



## Phix

Obviously if preferred you could define a long list of routines such as function d2g(atom a) return remainder(a/360,1)*400 end function

```Phix
constant units = {"degrees","gradians","mils","radians"},
         turns = {1/360,1/400,1/6400,0.5/PI}

function convert(atom a, integer fdx, tdx)
    return remainder(a*turns[fdx],1)/turns[tdx]
end function

constant tests = {-2,-1,0,1,2,6.2831853,16,57.2957795,359,399,6399,1000000},
printf(1,"    angle unit     %9s %9s %9s %9s\n",units)
for i=1 to length(tests) do
    for fdx=1 to length(units) do
        printf(1,"%9g %-8s",{tests[i],units[fdx]})
        for tdx=1 to length(units) do
            printf(1," %9g",convert(tests[i],fdx,tdx))
        end for
        puts(1,"\n")
    end for
    puts(1,"\n")
end for
```

{{out}}

```txt

    angle unit       degrees  gradians      mils   radians
       -2 degrees         -2  -2.22222  -35.5556 -0.034907
       -2 gradians      -1.8        -2       -32 -0.031416
       -2 mils       -0.1125    -0.125        -2 -0.001963
       -2 radians   -114.592  -127.324  -2037.18        -2

       -1 degrees         -1  -1.11111  -17.7778 -0.017453
       -1 gradians      -0.9        -1       -16 -0.015708
       -1 mils      -0.05625   -0.0625        -1 -0.000982
       -1 radians   -57.2958   -63.662  -1018.59        -1

        0 degrees          0         0         0         0
        0 gradians         0         0         0         0
        0 mils             0         0         0         0
        0 radians          0         0         0         0

        1 degrees          1   1.11111   17.7778  0.017453
        1 gradians       0.9         1        16  0.015708
        1 mils       0.05625    0.0625         1  0.000982
        1 radians    57.2958    63.662   1018.59         1

        2 degrees          2   2.22222   35.5556  0.034907
        2 gradians       1.8         2        32  0.031416
        2 mils        0.1125     0.125         2  0.001963
        2 radians    114.592   127.324   2037.18         2

  6.28319 degrees    6.28319   6.98132   111.701  0.109662
  6.28319 gradians   5.65487   6.28319   100.531  0.098696
  6.28319 mils      0.353429  0.392699   6.28319  0.006169
  6.28319 radians        360       400      6400   6.28319

       16 degrees         16   17.7778   284.444  0.279253
       16 gradians      14.4        16       256  0.251327
       16 mils           0.9         1        16  0.015708
       16 radians    196.732   218.592   3497.47   3.43363

  57.2958 degrees    57.2958    63.662   1018.59         1
  57.2958 gradians   51.5662   57.2958   916.732       0.9
  57.2958 mils       3.22289   3.58099   57.2958   0.05625
  57.2958 radians    42.8063   47.5626   761.002  0.747112

      359 degrees        359   398.889   6382.22   6.26573
      359 gradians     323.1       359      5744   5.63916
      359 mils       20.1937   22.4375       359  0.352447
      359 radians    49.1848   54.6498   874.397  0.858437

      399 degrees         39   43.3333   693.333  0.680678
      399 gradians     359.1       399      6384   6.26748
      399 mils       22.4438   24.9375       399  0.391717
      399 radians    181.016   201.129   3218.06   3.15933

     6399 degrees        279       310      4960   4.86947
     6399 gradians     359.1       399      6384   6.26748
     6399 mils       359.944   399.938      6399    6.2822
     6399 radians    155.693   172.992   2767.88   2.71736

     1e+6 degrees        280   311.111   4977.78   4.88692
     1e+6 gradians         0         0         0         0
     1e+6 mils            90       100      1600    1.5708
     1e+6 radians    339.513   377.237   6035.79   5.92562

```



## Python


```python
PI = 3.141592653589793
TWO_PI = 6.283185307179586

def normalize2deg(a):
  while a < 0: a += 360
  while a >= 360: a -= 360
  return a
def normalize2grad(a):
  while a < 0: a += 400
  while a >= 400: a -= 400
  return a
def normalize2mil(a):
  while a < 0: a += 6400
  while a >= 6400: a -= 6400
  return a
def normalize2rad(a):
  while a < 0: a += TWO_PI
  while a >= TWO_PI: a -= TWO_PI
  return a

def deg2grad(a): return a * 10.0 / 9.0
def deg2mil(a): return a * 160.0 / 9.0
def deg2rad(a): return a * PI / 180.0

def grad2deg(a): return a * 9.0 / 10.0
def grad2mil(a): return a * 16.0
def grad2rad(a): return a * PI / 200.0

def mil2deg(a): return a * 9.0 / 160.0
def mil2grad(a): return a / 16.0
def mil2rad(a): return a * PI / 3200.0

def rad2deg(a): return a * 180.0 / PI
def rad2grad(a): return a * 200.0 / PI
def rad2mil(a): return a * 3200.0 / PI
```



## REXX


```rexx
/*REXX pgm normalizes an angle (in a scale), or converts angles from a scale to another.*/
numeric digits length( pi() )   -   length(.)    /*use the "length" of pi for precision.*/
parse arg x                                      /*obtain optional arguments from the CL*/
if x='' | x=","  then x= '-2 -1 0 1 2 6.2831853 16 57.2957795 359 399 6399 1000000'
w= 20;                                 w7= w+7   /*W:  # dec digits past the dec. point.*/
@deg = 'degrees';       @grd= "gradians";        @mil = 'mils';           @rad = "radians"
# = words(x)
call hdr @deg @grd @mil @rad
      do j=1  for #;            y= word(x,j)
      say shw(y)        fmt(d2d(y))              fmt(d2g(y))    fmt(d2m(y))    fmt(d2r(y))
      end   /*j*/

call hdr @grd @deg @mil @rad
      do j=1  for #;            y= word(x,j)
      say shw(y)        fmt(g2g(y))              fmt(g2d(y))    fmt(g2m(y))    fmt(g2r(y))
      end   /*j*/

call hdr @mil @deg @grd @rad
      do j=1  for #;            y= word(x,j)
      say shw(y)        fmt(m2m(y))              fmt(m2d(y))    fmt(m2g(y))    fmt(m2r(y))
      end   /*j*/

call hdr @rad @deg @grd @mil
      do j=1  for #;            y= word(x,j)
      say shw(y)        fmt(r2r(y))              fmt(r2d(y))    fmt(r2g(y))    fmt(r2m(y))
      end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fmt: _= format(arg(1), 6,w);  L= length(_);  return left(format(_/1, 6),L)   /*align a #*/
shw: _= format(arg(1),12,9);  L= length(_);  return left(format(_/1,12),L)   /*  "   " "*/
pi:  pi= 3.1415926535897932384626433832795028841971693993751058209749445923078;  return pi
d2g: return d2d(arg(1)) *   10  /    9           /*convert degrees   ───► gradians.     */
d2m: return d2d(arg(1)) *  160  /    9           /*convert degrees   ───► mils.         */
d2r: return d2d(arg(1)) * pi()  /  180           /*convert degrees   ───► radians.      */
g2d: return g2g(arg(1)) *    0.9                 /*convert gradians  ───► degrees.      */
g2m: return g2g(arg(1)) *   16                   /*convert gradians  ───► mils.         */
g2r: return g2g(arg(1)) * pi()  *    0.005       /*convert gradians  ───► radians.      */
m2d: return m2m(arg(1)) *    9  *    0.00625     /*convert mils      ───► degrees.      */
m2g: return m2m(arg(1)) /   16                   /*convert mils      ───► gradians.     */
m2r: return m2m(arg(1)) * pi()  / 3200           /*convert mils      ───► radians.      */
r2d: return r2r(arg(1)) *  180  / pi()           /*convert radians   ───► degrees.      */
r2g: return r2r(arg(1)) *  200  / pi()           /*convert radians   ───► gradians.     */
r2m: return r2r(arg(1)) * 3200  / pi()           /*convert radians   ───► mils.         */
d2d: return     arg(1) //  360                   /*normalize degrees ───► a unit circle.*/
g2g: return     arg(1) //  400                   /*normalize gradians───► a unit circle.*/
m2m: return     arg(1) // 6400                   /*normalize mils    ───► a unit circle.*/
r2r: return     arg(1) // (pi() * 2)             /*normalize radians ───► a unit circle.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
hdr: parse arg #o #a #b #c .;         _= '═';              say   /* [↓]  the header line*/
                                @n  = 'normalized'
     say center(#o,23  )  center(@n #o,w7) center(#a,w7  ) center(#b,w7  ) center(#c,w7  )
     say center('',23,_)  center('',w7, _) center('',w7,_) center('',w7,_) center('',w7,_)
     return                                                      /* '↑'  seperator line.*/
```

{{out|output|text=  when using the default input:}}

```txt

        degrees             normalized degrees               gradians                      mils                       radians
═══════════════════════ ═══════════════════════════ ═══════════════════════════ ═══════════════════════════ ═══════════════════════════
          -2               -2                          -2.22222222222222222222    -35.55555555555555555556     -0.03490658503988659154
          -1               -1                          -1.11111111111111111111    -17.77777777777777777778     -0.01745329251994329577
           0                0                           0                           0                           0
           1                1                           1.11111111111111111111     17.77777777777777777778      0.01745329251994329577
           2                2                           2.22222222222222222222     35.55555555555555555556      0.03490658503988659154
           6.2831853        6.2831853                   6.981317                  111.701072                    0.10966227099790767281
          16               16                          17.77777777777777777778    284.44444444444444444444      0.27925268031909273231
          57.2957795       57.2957795                  63.66197722222222222222   1018.59163555555555555556      0.9999999997716704269
         359              359                         398.88888888888888888889   6382.22222222222222222222      6.26573201465964318116
         399               39                          43.33333333333333333333    693.33333333333333333333      0.680678408277788535
        6399              279                         310                        4960                           4.86946861306417951962
     1000000              280                         311.11111111111111111111   4977.77777777777777777778      4.88692190558412281539

       gradians             normalized gradians               degrees                      mils                       radians
═══════════════════════ ═══════════════════════════ ═══════════════════════════ ═══════════════════════════ ═══════════════════════════
          -2               -2                          -1.8                       -32                          -0.03141592653589793238
          -1               -1                          -0.9                       -16                          -0.01570796326794896619
           0                0                           0                           0                           0
           1                1                           0.9                        16                           0.01570796326794896619
           2                2                           1.8                        32                           0.03141592653589793238
           6.2831853        6.2831853                   5.65486677                100.5309648                   0.09869604389811690553
          16               16                          14.4                       256                           0.25132741228718345908
          57.2957795       57.2957795                  51.56620155                916.732472                    0.89999999979450338421
         359              359                         323.1                      5744                           5.63915881319367886304
         399              399                         359.1                      6384                           6.26747734391163751073
        6399              399                         359.1                      6384                           6.26747734391163751073
     1000000                0                           0                           0                           0

         mils                 normalized mils                 degrees                    gradians                     radians
═══════════════════════ ═══════════════════════════ ═══════════════════════════ ═══════════════════════════ ═══════════════════════════
          -2               -2                          -0.1125                     -0.125                      -0.00196349540849362077
          -1               -1                          -0.05625                    -0.0625                     -0.00098174770424681039
           0                0                           0                           0                           0
           1                1                           0.05625                     0.0625                      0.00098174770424681039
           2                2                           0.1125                      0.125                       0.00196349540849362077
           6.2831853        6.2831853                   0.353429173125              0.39269908125               0.0061685027436323066
          16               16                           0.9                         1                           0.01570796326794896619
          57.2957795       57.2957795                   3.222887596875              3.58098621875               0.05624999998715646151
         359              359                          20.19375                    22.4375                      0.35244742582460492894
         399              399                          22.44375                    24.9375                      0.39171733399447734442
        6399             6399                         359.94375                   399.9375                      6.28220355947533966654
     1000000             1600                          90                         100                           1.57079632679489661923

        radians             normalized radians                degrees                    gradians                      mils
═══════════════════════ ═══════════════════════════ ═══════════════════════════ ═══════════════════════════ ═══════════════════════════
          -2               -2                        -114.5915590261646417536    -127.32395447351626861511  -2037.18327157626029784171
          -1               -1                         -57.2957795130823208768     -63.66197723675813430755  -1018.59163578813014892086
           0                0                           0                           0                           0
           1                1                          57.2957795130823208768      63.66197723675813430755   1018.59163578813014892086
           2                2                         114.5915590261646417536     127.32395447351626861511   2037.18327157626029784171
           6.2831853        6.2831853                 359.99999958863999622298    399.99999954293332913665   6399.99999268693326618633
          16                3.43362938564082704615    196.73247220931713402877    218.59163578813014892086   3497.4661726100823827337
          57.2957795        0.74711173538372170767     42.80634926218202230527     47.56261029131335811697    761.00176466101372987153
         359                0.85843749076357081526     49.18484519655319477054     54.64982799617021641171    874.39724793872346258733
         399                3.15932564768605195371    181.01602571984602984246    201.12891746649558871385   3218.06267946392941942158
        6399                2.71735729118096649006    155.69310421377129063139    172.99233801530143403488   2767.87740824482294455809
     1000000                5.92562114009385143291    339.51308232087679815481    377.23675813430755350535   6035.78813014892085608558

```



## zkl

{{trans|Perl6}}

```zkl
var [const]
   tau=(0.0).pi*2,
   units=Dictionary(	// code:(name, units in circle)
      "d", T("degrees", 360.0), "g",T("gradians",400.0),
      "m", T("mills",  6400.0), "r",T("radians", tau) ),
   cvtNm="%s-->%s".fmt,
   cvt=  // "d-->r":fcn(angle){}, "r-->d":fcn(angle){} ...
      Walker.cproduct(units.keys,units.keys).pump(Dictionary(),fcn([(a,b)]){
	 return(cvtNm(a,b),  // "d-->r"
	    'wrap(angle){ angle=angle.toFloat();
	       u:=units[a][1];
	       (angle%u)*units[b][1] / u
	    })
	  })
;
```


```zkl
codes:=units.keys;
println("     Angle Unit     ",
   codes.apply(fcn(k){ units[k][0] }).apply("%11s".fmt).concat(" "));
foreach angle in (T(-2.0,-1, 0, 1, 2, tau, 16, 360.0/tau, 360-1, 400-1, 6400-1, 1_000_000)){
   println();
   foreach from in (codes){
      subKeys:=codes.apply(cvtNm.fp(from)); // ("d-->d","d-->g","d-->m","d-->r")
      r:=subKeys.pump(List,'wrap(k){ cvt[k](angle) });
      println("%10g %-8s %s".fmt(angle,units[from][0],
         r.apply("%12g".fmt).concat(" ")));
   }
}
```

{{out}}

```txt

     Angle Unit         degrees    gradians       mills     radians

        -2 degrees            -2     -2.22222     -35.5556   -0.0349066
        -2 gradians         -1.8           -2          -32   -0.0314159
        -2 mills         -0.1125       -0.125           -2   -0.0019635
        -2 radians      -114.592     -127.324     -2037.18           -2

        -1 degrees            -1     -1.11111     -17.7778   -0.0174533
        -1 gradians         -0.9           -1          -16    -0.015708
        -1 mills        -0.05625      -0.0625           -1 -0.000981748
        -1 radians      -57.2958      -63.662     -1018.59           -1

         0 degrees             0            0            0            0
         0 gradians            0            0            0            0
         0 mills               0            0            0            0
         0 radians             0            0            0            0

         1 degrees             1      1.11111      17.7778    0.0174533
         1 gradians          0.9            1           16     0.015708
         1 mills         0.05625       0.0625            1  0.000981748
         1 radians       57.2958       63.662      1018.59            1

         2 degrees             2      2.22222      35.5556    0.0349066
         2 gradians          1.8            2           32    0.0314159
         2 mills          0.1125        0.125            2    0.0019635
         2 radians       114.592      127.324      2037.18            2

   6.28319 degrees       6.28319      6.98132      111.701     0.109662
   6.28319 gradians      5.65487      6.28319      100.531     0.098696
   6.28319 mills        0.353429     0.392699      6.28319    0.0061685
   6.28319 radians             0            0            0            0

        16 degrees            16      17.7778      284.444     0.279253
        16 gradians         14.4           16          256     0.251327
        16 mills             0.9            1           16     0.015708
        16 radians       196.732      218.592      3497.47      3.43363

   57.2958 degrees       57.2958       63.662      1018.59            1
   57.2958 gradians      51.5662      57.2958      916.732          0.9
   57.2958 mills         3.22289      3.58099      57.2958      0.05625
   57.2958 radians       42.8064      47.5626      761.002     0.747112

       359 degrees           359      398.889      6382.22      6.26573
       359 gradians        323.1          359         5744      5.63916
       359 mills         20.1938      22.4375          359     0.352447
       359 radians       49.1848      54.6498      874.397     0.858437

       399 degrees            39      43.3333      693.333     0.680678
       399 gradians        359.1          399         6384      6.26748
       399 mills         22.4438      24.9375          399     0.391717
       399 radians       181.016      201.129      3218.06      3.15933

      6399 degrees           279          310         4960      4.86947
      6399 gradians        359.1          399         6384      6.26748
      6399 mills         359.944      399.938         6399       6.2822
      6399 radians       155.693      172.992      2767.88      2.71736

     1e+06 degrees           280      311.111      4977.78      4.88692
     1e+06 gradians            0            0            0            0
     1e+06 mills              90          100         1600       1.5708
     1e+06 radians       339.513      377.237      6035.79      5.92562

```

