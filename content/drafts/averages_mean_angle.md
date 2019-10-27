+++
title = "Averages/Mean angle"
description = ""
date = 2019-10-06T04:56:04Z
aliases = []
[extra]
id = 11983
[taxonomies]
categories = []
tags = []
+++

{{task}}

When calculating the [[wp:Mean of circular quantities|average or mean of an angle]] one has to take into account how angles wrap around so that any angle in degrees plus any integer multiple of 360 degrees is a measure of the same angle.

If one wanted an average direction of the wind over two readings where the first reading was of 350 degrees and the second was of 10 degrees then the average of the numbers is 180 degrees, whereas if you can note that 350 degrees is equivalent to -10 degrees and so you have two readings at 10 degrees either side of zero degrees leading to a more fitting mean angle of zero degrees.

To calculate the mean angle of several angles:
# Assume all angles are on the unit circle and convert them to complex numbers expressed in real and imaginary form.
# Compute the mean of the complex numbers.
# Convert the complex mean to polar coordinates whereupon the phase of the complex mean is the required angular mean.



(Note that, since the mean is the sum divided by the number of numbers, and division by a positive real number does not affect the angle, you can also simply compute the sum for step 2.)

You can alternatively use this formula:

: Given the angles <math>\alpha_1,\dots,\alpha_n</math> the mean is computed by

::<math>\bar{\alpha} = \operatorname{atan2}\left(\frac{1}{n}\cdot\sum_{j=1}^n \sin\alpha_j, \frac{1}{n}\cdot\sum_{j=1}^n \cos\alpha_j\right) </math>

{{task heading}}

# write a function/method/subroutine/... that given a list of angles in degrees returns their mean angle. 
 (You should use a built-in function if you have one that does this for degrees or radians). 
# Use the function to compute the means of these lists of angles (in degrees): 
#*   [350, 10]  
#*   [90, 180, 270, 360] 
#*   [10, 20, 30]
# Show your output here.

{{task heading|See also}}

{{Related tasks/Statistical measures}}


<hr>


## 11l

{{trans|C#}}

```11l
F mean_angle(angles)
   V x = sum(angles.map(a -> cos(radians(a)))) / angles.len
   V y = sum(angles.map(a -> sin(radians(a)))) / angles.len
   R degrees(atan2(y, x))

print(mean_angle([350, 10]))
print(mean_angle([90, 180, 270, 360]))
print(mean_angle([10, 20, 30]))
```

{{out}}

```txt

-1.61481e-15
-90
20

```



## Ada

An implementation based on the formula using the "Arctan" (atan2) function, thus avoiding complex numbers:

```Ada
with Ada.Text_IO, Ada.Numerics.Generic_Elementary_Functions;

procedure Mean_Angles is

   type X_Real is digits 4;      -- or more digits for improved precision
   subtype Real is X_Real range 0.0 .. 360.0; -- the range of interest
   type Angles is array(Positive range <>) of Real;

   procedure Put(R: Real) is
      package IO is new Ada.Text_IO.Float_IO(Real);
   begin
      IO.Put(R, Fore => 3, Aft => 2, Exp => 0);
   end Put;

   function Mean_Angle(A: Angles) return Real is
      Sin_Sum, Cos_Sum: X_Real := 0.0; -- X_Real since sums might exceed 360.0
      package Math is new Ada.Numerics.Generic_Elementary_Functions(Real);
      use Math;
   begin
      for I in A'Range loop
        Sin_Sum := Sin_Sum + Sin(A(I), Cycle => 360.0);
        Cos_Sum := Cos_Sum + Cos(A(I), Cycle => 360.0);
      end loop;
      return Arctan(Sin_Sum / X_Real(A'Length), Cos_Sum / X_Real(A'Length),
                    Cycle => 360.0);
        -- may raise Ada.Numerics.Argument_Error if inputs are
        -- numerically instable, e.g., when Cos_Sum is 0.0
   end Mean_Angle;

begin
   Put(Mean_Angle((10.0, 20.0, 30.0)));     Ada.Text_IO.New_Line;    -- 20.00
   Put(Mean_Angle((10.0, 350.0)));          Ada.Text_IO.New_Line;    --  0.00
   Put(Mean_Angle((90.0, 180.0, 270.0, 360.0))); -- Ada.Numerics.Argument_Error!
end Mean_Angles;
```

{{out}}

```txt
 20.00
  0.00

raised ADA.NUMERICS.ARGUMENT_ERROR : a-ngelfu.adb:427 instantiated at mean_angles.adb:17
```


## ALGOL 68

{{works with|ALGOL 68|Revision 1}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.8.3 algol68g-2.8.3].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
{{trans|C|Note: This specimen retains the original [[#C|C]] coding style}}
'''File: Averages_Mean_angle.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

PROC mean angle = ([]#LONG# REAL angles)#LONG# REAL:
(
  INT size = UPB angles - LWB angles + 1;
  #LONG# REAL y part := 0, x part := 0;
  FOR i FROM LWB angles TO UPB angles DO
      x part +:= #long# cos (angles[i] * #long# pi / 180);
      y part +:= #long# sin (angles[i] * #long# pi / 180)
  OD;
 
  #long# arc tan2 (y part / size, x part / size) * 180 / #long# pi
);
 
main:
(
  []#LONG# REAL angle set 1 = ( 350, 10 );
  []#LONG# REAL angle set 2 = ( 90, 180, 270, 360);
  []#LONG# REAL angle set 3 = ( 10, 20, 30);
 
  FORMAT summary fmt=$"Mean angle for "g" set :"-zd.ddddd" degrees"l$;
  printf ((summary fmt,"1st", mean angle (angle set 1)));
  printf ((summary fmt,"2nd", mean angle (angle set 2)));
  printf ((summary fmt,"3rd", mean angle (angle set 3)))
)
```
{{out}}

```txt

Mean angle for 1st set : -0.00000 degrees
Mean angle for 2nd set :-90.00000 degrees
Mean angle for 3rd set : 20.00000 degrees

```



## Aime


```aime
real
mean(list l)
{
    integer i;
    real x, y;

    x = y = 0;

    i = 0;
    while (i < l_length(l)) {
        x += Gcos(l[i]);
        y += Gsin(l[i]);
        i += 1;
    }

    return Gatan2(y / l_length(l), x / l_length(l));
}

integer
main(void)
{
    o_form("mean of 1st set: /d6/\n", mean(l_effect(350, 10)));
    o_form("mean of 2nd set: /d6/\n", mean(l_effect(90, 180, 270, 360)));
    o_form("mean of 3rd set: /d6/\n", mean(l_effect(10, 20, 30)));

    return 0;
}
```

{{out}}

```txt
mean of 1st set: -.000000
mean of 2nd set: -90
mean of 3rd set: 19.999999
```



## AutoHotkey

{{works with|AutoHotkey 1.1}}

```AutoHotkey
Angles :=  [[350, 10], [90, 180, 270, 360], [10, 20, 30]]
MsgBox, % MeanAngle(Angles[1]) "`n"
	. MeanAngle(Angles[2]) "`n" 
	. MeanAngle(Angles[3]) 

MeanAngle(a, x=0, y=0) {
	static c := ATan(1) / 45
	for k, v in a {
		x += Cos(v * c) / a.MaxIndex()
		y += Sin(v * c) / a.MaxIndex()
	}
	return atan2(x, y) / c
}

atan2(x, y) {
   return dllcall("msvcrt\atan2", "Double",y, "Double",x, "CDECL Double")
}
```

'''Output:'''

```txt
-0.000000
-90.000000
20.000000
```



## AWK


```AWK
#!/usr/bin/awk -f
{
    PI = atan2(0,-1);
    x=0.0; y=0.0;
    for (i=1; i<=NF; i++) {	
	p = $i*PI/180.0;
	x += sin(p);
	y += cos(p);
    }
    p = atan2(x,y)*180.0/PI;	
    if (p<0) p += 360;
    print p;
}
```


```txt
 echo 350 10 | ./mean_angle.awk 
360
 echo 10 20 30 | ./mean_angle.awk 
20
 echo 90 180 270 360  | ./mean_angle.awk 
270
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      *FLOAT 64
      DIM angles(3)
      angles() = 350,10
      PRINT FNmeanangle(angles(), 2)
      angles() = 90,180,270,360
      PRINT FNmeanangle(angles(), 4)
      angles() = 10,20,30
      PRINT FNmeanangle(angles(), 3)
      END
      
      DEF FNmeanangle(angles(), N%)
      LOCAL I%, sumsin, sumcos
      FOR I% = 0 TO N%-1
        sumsin += SINRADangles(I%)
        sumcos += COSRADangles(I%)
      NEXT
      = DEGFNatan2(sumsin, sumcos)
      
      DEF FNatan2(y,x) : ON ERROR LOCAL = SGN(y)*PI/2
      IF x>0 THEN = ATN(y/x) ELSE IF y>0 THEN = ATN(y/x)+PI ELSE = ATN(y/x)-PI
```

{{out}}

```txt

-1.61480993E-15
       -90
        20

```



## C


```c>#include<math.h

#include<stdio.h>

double
meanAngle (double *angles, int size)
{
  double y_part = 0, x_part = 0;
  int i;

  for (i = 0; i < size; i++)
    {
      x_part += cos (angles[i] * M_PI / 180);
      y_part += sin (angles[i] * M_PI / 180);
    }

  return atan2 (y_part / size, x_part / size) * 180 / M_PI;
}

int
main ()
{
  double angleSet1[] = { 350, 10 };
  double angleSet2[] = { 90, 180, 270, 360};
  double angleSet3[] = { 10, 20, 30};

  printf ("\nMean Angle for 1st set : %lf degrees", meanAngle (angleSet1, 2));
  printf ("\nMean Angle for 2nd set : %lf degrees", meanAngle (angleSet2, 4));
  printf ("\nMean Angle for 3rd set : %lf degrees\n", meanAngle (angleSet3, 3));
  return 0;
}
```

{{out}}

```txt
Mean Angle for 1st set : -0.000000 degrees
Mean Angle for 2nd set : -90.000000 degrees
Mean Angle for 3rd set : 20.000000 degrees
```



## C++

{{trans|C#}}

```cpp>#include <iomanip

#include <iostream>
#include <vector>

#define _USE_MATH_DEFINES
#include <math.h>

template<typename C>
double meanAngle(const C& c) {
    auto it = std::cbegin(c);
    auto end = std::cend(c);

    double x = 0.0;
    double y = 0.0;
    double len = 0.0;
    while (it != end) {
        x += cos(*it * M_PI / 180);
        y += sin(*it * M_PI / 180);
        len++;

        it = std::next(it);
    }

    return atan2(y, x) * 180 / M_PI;
}

void printMean(std::initializer_list<double> init) {
    std::cout << std::fixed << std::setprecision(3) << meanAngle(init) << '\n';
}

int main() {
    printMean({ 350, 10 });
    printMean({ 90, 180, 270, 360 });
    printMean({ 10, 20, 30 });

    return 0;
}
```

{{out}}

```txt
-0.000
-90.000
20.000
```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Linq;
using static System.Math;
class Program
{
    static double MeanAngle(double[] angles)
    {
        var x = angles.Sum(a => Cos(a * PI / 180)) / angles.Length;
        var y = angles.Sum(a => Sin(a * PI / 180)) / angles.Length;
        return Atan2(y, x) * 180 / PI;
    }
    static void Main()
    {
        Action<double[]> printMean = x => Console.WriteLine("{0:0.###}", MeanAngle(x));
        printMean(new double[] { 350, 10 });
        printMean(new double[] { 90, 180, 270, 360 });
        printMean(new double[] { 10, 20, 30 });
    }
}
```

{{out}}

```txt
0
-90
20
```



## Clojure


```clojure
(defn mean-fn
  [k coll]
  (let [n (count coll)
        trig (get {:sin #(Math/sin %) :cos #(Math/cos %)} k)]
    (* (/ 1 n) (reduce + (map trig coll)))))

(defn mean-angle
  [degrees]
  (let [radians (map #(Math/toRadians %) degrees)
        a (mean-fn :sin radians)
        b (mean-fn :cos radians)]
    (Math/toDegrees (Math/atan2 a b))))
```

Example:

```clojure
(mean-angle [350 10])
;=> -1.614809932057922E-15

(mean-angle [90 180 270 360])
;=> -90.0

(mean-angle [10 20 30])
;=> 19.999999999999996
```



## Common Lisp


```lisp
(defun average (list)
  (/ (reduce #'+ list) (length list)))

(defun radians (angle)
  (* pi 1/180 angle))

(defun degrees (angle)
  (* 180 (/ 1 pi) angle))

(defun mean-angle (angles)
  (let* ((angles (map 'list #'radians angles))
	 (cosines (map 'list #'cos angles))
	 (sines (map 'list #'sin angles)))
    (degrees (atan (average sines) (average cosines)))))

(loop for angles in '((350 10) (90 180 270 360) (10 20 30))
   do (format t "~&The mean angle of ~a is ~$°." angles (mean-angle angles)))
```

{{out}}

```txt
The mean angle of (350 10) is -0.00°.
The mean angle of (90 180 270 360) is -90.00°.
The mean angle of (10 20 30) is 20.00°.
```



## D


```d
import std.stdio, std.algorithm, std.complex;
import std.math: PI;

auto radians(T)(in T d) pure nothrow @nogc { return d * PI / 180; }
auto degrees(T)(in T r) pure nothrow @nogc { return r * 180 / PI; }

real meanAngle(T)(in T[] D) pure nothrow @nogc {
    immutable t = reduce!((a, d) => a + d.radians.expi)(0.complex, D);
    return (t / D.length).arg.degrees;
}

void main() {
    foreach (angles; [[350, 10], [90, 180, 270, 360], [10, 20, 30]])
        writefln("The mean angle of %s is: %.2f degrees",
                 angles, angles.meanAngle);
}
```

{{out}}

```txt
The mean angle of [350, 10] is: -0.00 degrees
The mean angle of [90, 180, 270, 360] is: 90.00 degrees
The mean angle of [10, 20, 30] is: 20.00 degrees
```




## EchoLisp


```scheme

(define-syntax-rule  (deg->radian deg) (* deg 1/180 PI))
(define-syntax-rule  (radian->deg rad) (* 180 (/ PI) rad))

(define (mean-angles angles)
	(radian->deg 
          (angle 
	    (for/sum ((a angles)) (make-polar 1 (deg->radian a))))))

(mean-angles '( 350 10))
    → -0
(mean-angles '[90 180 270 360])
    → -90
(mean-angles '[10 20 30])
    → 20

```



## Elixir


```Elixir

defmodule MeanAngle do
  def mean_angle(angles) do
    rad_angles = Enum.map(angles, &deg_to_rad/1)
    sines = rad_angles |> Enum.map(&:math.sin/1) |> Enum.sum
    cosines = rad_angles |> Enum.map(&:math.cos/1) |> Enum.sum

    rad_to_deg(:math.atan2(sines, cosines))
  end

  defp deg_to_rad(a) do
    (:math.pi/180) * a
  end

  defp rad_to_deg(a) do
    (180/:math.pi) * a
  end
end

IO.inspect MeanAngle.mean_angle([10, 350])
IO.inspect MeanAngle.mean_angle([90, 180, 270, 360])
IO.inspect MeanAngle.mean_angle([10, 20, 30])

```

{{out}}

```txt

-1.614809932057922e-15
-90.0
19.999999999999996

```



## Erlang

The function from_degrees/1 is used to solve [[Averages/Mean_time_of_day]]. Please keep backwards compatibility when editing. Or update the other module, too.

```Erlang

-module( mean_angle ).
-export( [from_degrees/1, task/0] ).

from_degrees( Angles ) ->
	Radians = [radians(X) || X <- Angles],
	Sines = [math:sin(X) || X <- Radians],
	Coses = [math:cos(X) || X <- Radians],
	degrees( math:atan2( average(Sines), average(Coses) ) ).

task() ->
	Angles = [[350, 10], [90, 180, 270, 360], [10, 20, 30]],
	[io:fwrite( "Mean angle of ~p is: ~p~n", [X, erlang:round(from_degrees(X))] ) || X <- Angles].


average( List ) -> lists:sum( List ) / erlang:length( List ).

degrees( Radians ) -> Radians * 180 / math:pi().

radians( Degrees ) -> Degrees * math:pi() / 180.

```

{{out}}

```txt

16> mean_angle:task().
Mean angle of [350,10] is: 0
Mean angle of [90,180,270,360] is: -90
Mean angle of [10,20,30] is: 20

```



## Euler Math Toolbox


```EulerMathToolbox>
function meanangle (a) ...
$  z=sum(exp(rad(a)*I));
$  if z~=0 then error("Not meaningful");
$  else return deg(arg(z))
$  endfunction
>meanangle([350,10])
 0
>meanangle([90,180,270,360])
 Error : Not meaningful
 
 Error generated by error() command
 
 Error in function meanangle in line
 if z~=0 then error("Not meaningful");
>meanangle([10,20,30])
 20
```




## Euphoria

{{works with|OpenEuphoria}}

```Euphoria

include std/console.e
include std/mathcons.e

sequence AngleList = {{350,10},{90,180,270,360},{10,20,30}}

function atan2(atom y, atom x)
	return 2*arctan((sqrt(power(x,2)+power(y,2)) - x)/y)
end function

function MeanAngle(sequence angles)
	atom x = 0, y = 0
	integer l = length(angles)
	
	for i = 1 to length(angles) do
		x += cos(angles[i] * PI / 180)
		y += sin(angles[i] * PI / 180)
	end for
	
	return atan2(y / l, x / l) * 180 / PI
end function 

for i = 1 to length(AngleList) do
	printf(1,"Mean Angle for set %d:  %3.5f\n",{i,MeanAngle(AngleList[i])})
end for

if getc(0) then end if

```

{{out}}

```txt

Mean Angle for set 1:  0.00000
Mean Angle for set 2:  -90.00000
Mean Angle for set 3:  20.00000

```


=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.Numerics

let deg2rad d = d * Math.PI / 180.
let rad2deg r = r * 180. / Math.PI

[<EntryPoint>]
let main argv =
    let makeComplex = fun r ->  Complex.FromPolarCoordinates(1., r)
    argv
    |> Seq.map (Double.Parse >> deg2rad >> makeComplex)
    |> Seq.fold (fun x y -> Complex.Add(x,y)) Complex.Zero
    |> fun c -> c.Phase |> rad2deg
    |> printfn "Mean angle for [%s]: %g°" (String.Join("; ",argv))
    0
```

{{out}}

```txt
>RosettaCode 350 10
Mean angle for [350; 10]: -2.74518e-14°

>RosettaCode 10 20 30
Mean angle for [10; 20; 30]: 20°

>RosettaCode 90 180 270 360
Mean angle for [90; 180; 270; 360]: -90°

```



## Factor


```factor
USING: formatting kernel math math.functions math.libm math.trig
sequences ;

: mean-angle ( seq -- x )
    [ deg>rad ] map [ [ sin ] map-sum ] [ [ cos ] map-sum ]
    [ length ] tri recip [ * ] curry bi@ fatan2 rad>deg ;

: show ( seq -- )
    dup mean-angle "The mean angle of %u is: %f°\n" printf ;

{ { 350 10 } { 90 180 270 360 } { 10 20 30 } } [ show ] each
```

{{out}}

```txt

The mean angle of { 350 10 } is: -0.000000°
The mean angle of { 90 180 270 360 } is: -90.000000°
The mean angle of { 10 20 30 } is: 20.000000°

```



## Fortran

Please find the example output along with the build instructions in the comments at the start of the FORTRAN 2008 source. Compiler: gfortran from the GNU compiler collection. Command interpreter: bash.

```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Mon Jun  3 18:07:59
!
!a=./f && make $a && OMP_NUM_THREADS=2 $a
!gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o f
!  -7.80250048E-06         350          10
!   90.0000000              90         180         270         360
!   19.9999962              10          20          30
!
!Compilation finished at Mon Jun  3 18:07:59

program average_angles
  !real(kind=8), parameter :: TAU = 6.283185307179586232 ! http://tauday.com/
  !integer, dimension(13), parameter :: test_data = (/2,350,10, 4,90,180,270,360, 3,10,20,30, 0/)
  !integer :: i, j, n
  !complex(kind=16) :: some
  !real(kind=8) :: angle
  real, parameter :: TAU = 6.283185307179586232 ! http://tauday.com/
  integer, dimension(13), parameter :: test_data = (/2,350,10, 4,90,180,270,360, 3,10,20,30, 0/)
  integer :: i, j, n
  complex :: some
  real :: angle
  i = 1
  n = int(test_data(i))
  do while (0 .lt. n)
    some = 0
    do j = 1, n
      angle = (TAU/360)*test_data(i+j)
      some = some + cmplx(cos(angle), sin(angle))
    end do
    some = some / n
    write(6,*)(360/TAU)*atan2(aimag(some), real(some)),test_data(i+1:i+n)
    i = i + n + 1
    n = int(test_data(i))
  end do
end program average_angles

```



## FreeBASIC


```freebasic

' FB 1.05.0 Win64

Const PI As Double = 3.1415926535897932

Function MeanAngle(angles() As Double) As Double
  Dim As Integer length = Ubound(angles) - Lbound(angles) + 1
  Dim As Double sinSum = 0.0
  Dim As Double cosSum = 0.0
  For i As Integer = LBound(angles) To UBound(angles)
    sinSum += Sin(angles(i) * PI / 180.0)
    cosSum += Cos(angles(i) * PI / 180.0)    
  Next
  Return Atan2(sinSum / length, cosSum / length) * 180.0 / PI
End Function

Dim As Double angles1(1 To 2) = {350, 10}
Dim As Double angles2(1 To 4) = {90, 180, 270, 360}
Dim As Double angles3(1 To 3) = {10, 20, 30}

Print Using "Mean for angles 1 is : ####.## degrees"; MeanAngle(angles1())
Print Using "Mean for angles 2 is : ####.## degrees"; MeanAngle(angles2())
Print Using "Mean for angles 3 is : ####.## degrees"; MeanAngle(angles3())
Print
Print "Press any key to quit the program"
Sleep

```


{{out}}

```txt

Mean for angles 1 is :   -0.00 degrees
Mean for angles 2 is :  -90.00 degrees
Mean for angles 3 is :   20.00 degrees

```



## Go


### Complex


```go
package main

import (
	"fmt"
	"math"
	"math/cmplx"
)

func deg2rad(d float64) float64 { return d * math.Pi / 180 }
func rad2deg(r float64) float64 { return r * 180 / math.Pi }

func mean_angle(deg []float64) float64 {
	sum := 0i
	for _, x := range deg {
		sum += cmplx.Rect(1, deg2rad(x))
	}
	return rad2deg(cmplx.Phase(sum))
}

func main() {
	for _, angles := range [][]float64{
		{350, 10},
		{90, 180, 270, 360},
		{10, 20, 30},
	} {
		fmt.Printf("The mean angle of %v is: %f degrees\n", angles, mean_angle(angles))
	}
}
```

{{out}}

```txt

The mean angle of [350 10] is: -0.000000 degrees
The mean angle of [90 180 270 360] is: -90.000000 degrees
The mean angle of [10 20 30] is: 20.000000 degrees

```


### Atan2

A mean_angle function that could be substituted above.  Functions deg2rad and rad2deg are not used here but there is no runtime advantage either way to using them or not.  Inlining should result in eqivalent code being generated.  Also the Go Atan2 library function has no limits on the arguments so there is no need to divide by the number of elements.


```go
func mean_angle(deg []float64) float64 {
	var ss, sc float64
	for _, x := range deg {
		s, c := math.Sincos(x * math.Pi / 180)
		ss += s
		sc += c
	}
	return math.Atan2(ss, sc) * 180 / math.Pi
}
```



## Groovy


```groovy
import static java.lang.Math.*
def meanAngle = {
    atan2( it.sum { sin(it * PI / 180) } / it.size(), it.sum { cos(it * PI / 180) } / it.size()) * 180 / PI
}
```

Test:

```groovy
def verifyAngle = { angles ->
    def ma = meanAngle(angles)
    printf("Mean Angle for $angles: %5.2f%n", ma)
    round(ma * 100) / 100.0
}
assert verifyAngle([350, 10]) == -0
assert verifyAngle([90, 180, 270, 360]) == -90
assert verifyAngle([10, 20, 30]) == 20
```

{{out}}

```txt
Mean Angle for [350, 10]: -0.00
Mean Angle for [90, 180, 270, 360]: -90.00
Mean Angle for [10, 20, 30]: 20.00
```



## Haskell


```haskell
import Data.Complex (cis, phase)

meanAngle
  :: RealFloat c
  => [c] -> c
meanAngle = (/ pi) . (* 180) . phase . sum . map (cis . (/ 180) . (* pi))

main :: IO ()
main =
  mapM_
    (\angles ->
        putStrLn $
        "The mean angle of " ++
        show angles ++ " is: " ++ show (meanAngle angles) ++ " degrees")
    [[350, 10], [90, 180, 270, 360], [10, 20, 30]]
```

{{out}}

```txt

The mean angle of [350.0,10.0] is: -2.745176884498468e-14 degrees
The mean angle of [90.0,180.0,270.0,360.0] is: -90.0 degrees
The mean angle of [10.0,20.0,30.0] is: 19.999999999999996 degrees

```



Alternative Solution: This solution gives an insight about using factoring, many small functions like Forth and using function composition.


```haskell


-- file: trigdeg.fs

deg2rad deg = deg*pi/180.0
rad2deg rad = rad*180.0/pi

sind = sin . deg2rad
cosd = cos . deg2rad
tand = tan . deg2rad
atand = rad2deg . atan
atan2d y x = rad2deg (atan2 y x )

avg_angle angles = atan2d y x
    where
    y = mean (map sind angles) 
    x = mean (map cosd angles)

-- End of trigdeg.fs --------

```


{{out}}

```txt


-- GHCI shell  $ ghci

Prelude> :load trigdeg.hs 
[1 of 1] Compiling Main             ( trigdeg.hs, interpreted )
Ok, modules loaded: Main.
*Main> 
*Main> avg_angle [350.0, 10.0]
-2.745176884498468e-14
*Main> 
*Main> avg_angle [90.0, 180.0, 270.0, 360.0 ]
-90.0
*Main> mean [10.0, 20.0, 30.0]
20.0
*Main>

```


=={{header|Icon}} and {{header|Unicon}}==


```unicon
procedure main(A)
    write("Mean angle is ",meanAngle(A))
end

procedure meanAngle(A)
    every (sumSines := 0.0) +:= sin(dtor(!A))
    every (sumCosines := 0.0) +:= cos(dtor(!A))
    return rtod(atan(sumSines/*A,sumCosines/*A))
end
```


Sample runs:

```txt

->ama 10 350
Mean angle is -2.745176884498468e-14
->ama 90 180 270 360
Mean angle is -90.0
->ama 10 20 30
Mean angle is 20.0

```



## J


```J
avgAngleD=: 360|(_1 { [: (**|)&.+.@(+/ % #)&.(*.inv) 1,.])&.(1r180p1&*)
```

This verb can be represented as simpler component verbs, for example:

```J
rfd=: 1r180p1&*                                          NB. convert angle to radians from degrees
toComplex=: *.inv                                        NB. maps integer pairs as length, complex angle (in radians)
mean=: +/ % #                                            NB. calculate arithmetic mean
roundComplex=: (* * |)&.+.                               NB. discard an extraneous least significant bit of precision from a complex value whose magnitude is in the vicinity of 1
avgAngleR=: _1 { [: roundComplex@mean&.toComplex 1 ,. ]  NB. calculate average angle in radians
avgAngleD=: 360|avgAngleR&.rfd                           NB. calculate average angle in degrees
```

Example use:

```J
   avgAngleD 10 350
0
   avgAngleD 90 180 270 360 NB. result not meaningful
0
   avgAngleD 10 20 30
20
   avgAngleD 20 350
5
   avgAngleD 10 340
355
```


Notes: 

<code>(**|)&.+.</code> is an expression to discard an extraneous least significant bit of precision from a complex value whose magnitude is in the vicinity of 1.

<code>(+/ % #)</code> finds the (Pythagorean) mean

<code>verb&.(*.inv)</code> maps integer pairs as length,complex angle (in radians) and uses the verb in the domain of complex numbers, and then maps the result back to length,angle.

<code>(1,.])</code> prefixes every value in a list with 1 (forming a two column table)

<code>(_1 { verb)</code> takes the last item from the result of the verb (and note that after we average our complex values and convert them back to length/angle format, we will be working with a list of two elements: length and angle -- and we do not care about length, which will usually be less than 1).

<code>verb&.(1r180p1&*)</code> converts its argument from degrees to radians, uses the verb in the radian domain, then converts the result of that argument back to degrees.

<code>360|verb</code> ensures that the result is not negative and is also less than 360


## Java

{{trans|NetRexx}}
{{works with|Java|7+}}

```java5
import java.util.Arrays;

public class AverageMeanAngle {

    public static void main(String[] args) {
        printAverageAngle(350.0, 10.0);
        printAverageAngle(90.0, 180.0, 270.0, 360.0);
        printAverageAngle(10.0, 20.0, 30.0);
        printAverageAngle(370.0);
        printAverageAngle(180.0);
    }

    private static void printAverageAngle(double... sample) {
        double meanAngle = getMeanAngle(sample);
        System.out.printf("The mean angle of %s is %s%n", Arrays.toString(sample), meanAngle);
    }

    public static double getMeanAngle(double... anglesDeg) {
        double x = 0.0;
        double y = 0.0;

        for (double angleD : anglesDeg) {
            double angleR = Math.toRadians(angleD);
            x += Math.cos(angleR);
            y += Math.sin(angleR);
        }
        double avgR = Math.atan2(y / anglesDeg.length, x / anglesDeg.length);
        return Math.toDegrees(avgR);
    }
}
```

{{out}}

```txt
The mean angle of [350.0, 10.0] is -1.614809932057922E-15
The mean angle of [90.0, 180.0, 270.0, 360.0] is -90.0
The mean angle of [10.0, 20.0, 30.0] is 19.999999999999996
The mean angle of [370.0] is 9.999999999999977
The mean angle of [180.0] is 180.0
```



## JavaScript


### atan2


```javascript
function sum(a) {
    var s = 0;
    for (var i = 0; i < a.length; i++) s += a[i];
    return s;
} 

function degToRad(a) {
    return Math.PI / 180 * a;
}

function meanAngleDeg(a) {
    return 180 / Math.PI * Math.atan2(
        sum(a.map(degToRad).map(Math.sin)) / a.length,
        sum(a.map(degToRad).map(Math.cos)) / a.length
    );
}

var a = [350, 10], b = [90, 180, 270, 360],  c = [10, 20, 30];
console.log(meanAngleDeg(a));
console.log(meanAngleDeg(b));
console.log(meanAngleDeg(c));
```

{{out}}

```txt
-1.614809932057922e-15
-90
19.999999999999996
```



## jq


{{works with|jq|1.4}}

To avoid anomalies, the following is designed to assign the null value as the mean angle in cases such as [0, 180].

'''Generic Infrastructure'''

```jq
def pi: 4 * (1|atan);

def deg2rad: . * pi / 180;
 
def rad2deg: if . == null then null else . * 180 / pi end;

# Input: [x,y] (special handling of x==0)
# Output: [r, theta] where theta may be null
def to_polar:
  if .[0] == 0
  then [1, if .[1] > 5e-14 then pi/2 elif .[1] < -5e-14 then -pi/2 else null end]
  else [1, ((.[1]/.[0]) | atan)]
  end;

def from_polar: .[1] | [ cos, sin];

def abs: if . < 0 then - . else . end;
  
def summation(f): map(f) | add;
```


'''Mean Angle'''

```jq
# input: degrees
def mean_angle:
  def round:
    if . == null then null
    elif . < 0 then -1 * ((- .) | round) | if . == -0 then 0 else . end
    else ((. + 3e-14) | floor) as $x
    | if ($x - .) | abs < 3e-14 then $x else . end
    end;

  map( [1, deg2rad] | from_polar)
  | [ summation(.[0]), summation(.[1]) ]
  | to_polar
  | .[1]
  | rad2deg
  | round;
```

'''Examples'''

```jq
([350, 10], [90, 180, 270, 360], [10, 20, 30])
| "The mean angle of \(.) is: \(mean_angle)"
```


{{out}}

```sh
jq -r -n -f Mean_angle.jq
The mean angle of [350,10] is: 0
The mean angle of [90,180,270,360] is: null
The mean angle of [10,20,30] is: 20
```



## Julia

Julia has built-in functions <code>sind</code> and <code>cosd</code> to compute the sine and cosine of angles specified in degrees accurately (avoiding the roundoff errors incurred in conversion to radians), and a built-in function to convert radians to degrees (or vice versa).  Using these:

```julia
using Statistics
meandegrees(degrees) = radians2degrees(atan2(mean(sind(degrees)), mean(cosd(degrees))))
```

The output is:

```julia>julia
 meandegrees([350, 10])
0.0

julia> meandegrees([90, 180, 270, 360]])
0.0

julia> meandegrees([10, 20, 30]])
19.999999999999996
```

(Note that the mean of 90°, 180°, 270°, and 360° gives zero because of the lack of roundoff errors in the <code>sind</code> function, since the standard-library <code>atan2(0,0)</code> value is zero.  Many of the other languages report an average of 90° or –90° in this case due to rounding errors.)


## Kotlin


```scala
// version 1.0.5-2

fun meanAngle(angles: DoubleArray): Double {
    val sinSum = angles.sumByDouble {  Math.sin(it * Math.PI / 180.0) }
    val cosSum = angles.sumByDouble {  Math.cos(it * Math.PI / 180.0) }
    return Math.atan2(sinSum / angles.size, cosSum / angles.size) * 180.0 / Math.PI
}

fun main(args: Array<String>) {
    val angles1 = doubleArrayOf(350.0, 10.0)
    val angles2 = doubleArrayOf(90.0, 180.0, 270.0, 360.0)
    val angles3 = doubleArrayOf(10.0, 20.0, 30.0)
    val fmt  = "%.2f degrees" // format results to 2 decimal places
    println("Mean for angles 1 is ${fmt.format(meanAngle(angles1))}")
    println("Mean for angles 2 is ${fmt.format(meanAngle(angles2))}")
    println("Mean for angles 3 is ${fmt.format(meanAngle(angles3))}")
}
```


{{out}}

```txt

Mean for angles 1 is -0.00 degrees
Mean for angles 2 is -90.00 degrees
Mean for angles 3 is 20.00 degrees

```



## Liberty BASIC


```lb
global Pi
Pi =3.1415926535

print "Mean Angle( "; chr$( 34); "350,10"; chr$( 34); ") = ";         using( "###.#", meanAngle( "350,10")); " degrees."             '          0
print "Mean Angle( "; chr$( 34); "90,180,270,360"; chr$( 34); ") = "; using( "###.#", meanAngle( "90,180,270,360")); " degrees."  '        -90
print "Mean Angle( "; chr$( 34); "10,20,30"; chr$( 34); ") = ";       using( "###.#", meanAngle( "10,20,30")); " degrees."        '         20

end

function meanAngle( angles$)
   term =1
   while word$( angles$, term, ",") <>""
      angle  =val( word$( angles$, term, ","))
      sumSin = sumSin +sin( degRad( angle))
      sumCos = sumCos +cos( degRad( angle))
      term =term +1
   wend
   meanAngle= radDeg( atan2( sumSin, sumCos))
   if abs( sumSin) +abs( sumCos) <0.001 then notice "Not Available." +_
     chr$( 13) +"(" +angles$ +")" +_
     chr$( 13) +"Result nearly equals zero vector." +_
     chr$( 13) +"Displaying '666'.": meanAngle =666

end function

function degRad( theta)
    degRad =theta *Pi /180
end function

function radDeg( theta)
    radDeg =theta *180 /Pi
end function

function atan2( y, x)
    if x >0           then at =atn( y /x)
    if y >=0 and x <0 then at =atn( y /x) +pi
    if y <0  and x <0 then at =atn( y /x) -pi
    if y >0  and x =0 then at =    pi /2
    if y <0  and x =0 then at = 0 -pi /2
    if y =0  and x =0 then notice "undefined": end
    atan2 =at
end function
```

{{out}}

```txt

Mean Angle( "350,10") = 0.0 degrees.
Mean Angle( "90,180,270,360") = 666.0 degrees.
Mean Angle( "10,20,30") = 20.0 degrees.

```


## Logo


```logo
to mean_angle :angles
   local "avgsin
   make "avgsin quotient apply "sum map "sin :angles count :angles
   local "avgcos
   make "avgcos quotient apply "sum map "cos :angles count :angles
   output (arctan :avgcos :avgsin)
end

foreach [[350 10] [90 180 270 360] [10 20 30]] [
  print (sentence [The average of \(] ? [\) is] (mean_angle ?))
]

bye

```


{{Out}}

```txt
The average of ( 350 10 ) is 0
The average of ( 90 180 270 360 ) is 0
The average of ( 10 20 30 ) is 20
```



## Lua

{{trans|Tcl}}
{{works with|Lua|5.1}}

```Lua
function meanAngle (angleList)
  local sumSin, sumCos = 0, 0
  for i, angle in pairs(angleList) do
    sumSin = sumSin + math.sin(math.rad(angle))
    sumCos = sumCos + math.cos(math.rad(angle))
  end
  local result = math.deg(math.atan2(sumSin, sumCos))
  return string.format("%.2f", result)
end

print(meanAngle({350, 10}))
print(meanAngle({90, 180, 270, 360}))
print(meanAngle({10, 20, 30}))
```

{{out}}

```txt

-0.00
-90.00
20.00

```



## Maple

The following procedure takes a list of numeric degrees (with attached units) such as

```Maple>
 [ 350, 10 ] *~ Unit(arcdeg);
                      [350 [arcdeg], 10 [arcdeg]]
```

as input.  (We could use "degree" instead of "arcdeg", since "degree" is taken, by default, to mean angle measure, but it seems best to avoid the ambiguity.)

```Maple
MeanAngle := proc( L )
        uses Units:-Standard; # for unit-awareness
        local   u;
        evalf( convert( argument( add( u, u = exp~( I *~ L ) ) ), 'units', 'radian', 'degree' ) )
end proc:
```

Applying this to the given data sets, we obtain:

```Maple>
 MeanAngle( [ 350, 10 ] *~ Unit(arcdeg) );
                                   0.

> MeanAngle( [ 90, 180, 270, 360 ] *~ Unit(arcdeg) );
                                   0.

> MeanAngle( [ 10, 20, 30 ] *~ Unit(arcdeg) );
                              20.00000000
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
meanAngle[data_List] := N@Arg[Mean[Exp[I data Degree]]]/Degree
```

{{out}}

```txt
meanAngle /@ {{350, 10}, {90, 180, 270, 360}, {10, 20, 30}}
{0., Interval[{-180., 180.}], 20.}
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function u = mean_angle(phi)
	u = angle(mean(exp(i*pi*phi/180)))*180/pi;
end
```


```txt
 mean_angle([350, 10])
ans = -2.7452e-14
 mean_angle([90, 180, 270, 360])
ans = -90
 mean_angle([10, 20, 30])
ans =  20.000

```



## NetRexx

{{trans|C}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary
numeric digits 80

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getMeanAngle(angles) private static binary
  x_component = double 0.0
  y_component = double 0.0
  aK = int angles.words()
  loop a_ = 1 to aK
    angle_d = double angles.word(a_)
    angle_r = double Math.toRadians(angle_d)
    x_component = x_component + Math.cos(angle_r)
    y_component = y_component + Math.sin(angle_r)
    end a_
  x_component = x_component / aK
  y_component = y_component / aK
  avg_r = Math.atan2(y_component, x_component)
  avg_d = Math.toDegrees(avg_r)
  return avg_d

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  angleSet = [ '350 10', '90 180 270 360', '10 20 30', '370', '180' ]
  loop angles over angleSet
    say 'The mean angle of' angles.space(1, ',') 'is:'
    say '  'getMeanAngle(angles).format(6, 6)
    say
    end angles
  return

```

{{out}}

```txt

The mean angle of 350,10 is:
       0.000000

The mean angle of 90,180,270,360 is:
     -90.000000

The mean angle of 10,20,30 is:
      20.000000

The mean angle of 370 is:
      10.000000

The mean angle of 180 is:
     180.000000

```



## Nim


```nim
import math, complex
 
proc rect(r, phi: float): Complex = (r * cos(phi), sin(phi))
proc phase(c: Complex): float = arctan2(c.im, c.re)
 
proc radians(x: float): float = (x * Pi) / 180.0
proc degrees(x: float): float = (x * 180.0) / Pi
 
proc meanAngle(deg: openArray[float]): float =
  var c: Complex
  for d in deg:
    c += rect(1.0, radians(d))
  degrees(phase(c / float(deg.len)))
 
echo "The 1st mean angle is: ", meanAngle([350.0, 10.0]), " degrees"
echo "The 2nd mean angle is: ", meanAngle([90.0, 180.0, 270.0, 360.0]), " degrees"
echo "The 3rd mean angle is: ", meanAngle([10.0, 20.0, 30.0]), " degrees"
```

Output:

```txt
The 1st mean angle is: -2.745176884498468e-14 degrees
The 2nd mean angle is: -90.0 degrees
The 3rd mean angle is: 20.0 degrees
```


=={{header|Oberon-2}}==
{{works with|oo2c}}

```oberon2

MODULE MeanAngle;
IMPORT
  M := LRealMath,
  Out;
CONST
  toRads = M.pi / 180;
  toDegs = 180 / M.pi;
VAR
  grades: ARRAY 64 OF LONGREAL;

PROCEDURE Mean(g: ARRAY OF LONGREAL): LONGREAL;
VAR
  i: INTEGER;
  sumSin, sumCos: LONGREAL;
BEGIN
  i := 0;sumSin := 0.0;sumCos := 0.0;
  WHILE g[i] # 0.0 DO
    sumSin := sumSin + M.sin(g[i] * toRads);
    sumCos := sumCos + M.cos(g[i] * toRads);  
    INC(i)
  END;
  RETURN M.arctan2(sumSin / i,sumCos / i);
END Mean;

BEGIN
  grades[0] := 350.0;grades[1] := 10.0;
  Out.LongRealFix(Mean(grades) * toDegs,15,9);Out.Ln;
  grades[0] := 90.0;grades[1] := 180.0;grades[2] := 270.0;grades[3] := 360.0;
  Out.LongRealFix(Mean(grades) * toDegs,15,9);Out.Ln;
  grades[0] := 10.0;grades[1] := 20.0;grades[2] := 30.0;grades[3] := 0.0;
  Out.LongRealFix(Mean(grades) * toDegs,15,9);Out.Ln; 
END MeanAngle.

```

{{out}}

```txt

   -0.000000000
  -90.000000000
   20.000000000

```



## OCaml


```ocaml
let pi = 3.14159_26535_89793_23846_2643

let deg2rad d =
  d *. pi /. 180.0
 
let rad2deg r =
  r *. 180.0 /. pi

let mean_angle angles =
  let rad_angles = List.map deg2rad angles in
  let sum_sin = List.fold_left (fun sum a -> sum +. sin a) 0.0 rad_angles
  and sum_cos = List.fold_left (fun sum a -> sum +. cos a) 0.0 rad_angles
  in
  rad2deg (atan2 sum_sin sum_cos)

let test angles =
  Printf.printf "The mean angle of [%s] is: %g degrees\n"
    (String.concat "; " (List.map (Printf.sprintf "%g") angles))
    (mean_angle angles)

let () =
  test [350.0; 10.0];
  test [90.0; 180.0; 270.0; 360.0];
  test [10.0; 20.0; 30.0];
;;
```

or using the <code>Complex</code> module:

```ocaml
open Complex

let mean_angle angles =
  let sum =
    List.fold_left (fun sum a -> add sum (polar 1.0 (deg2rad a))) zero angles
  in
    rad2deg (arg sum)
```

{{out}}

```txt

The mean angle of [350; 10] is: -2.74518e-14 degrees
The mean angle of [90; 180; 270; 360] is: -90 degrees
The mean angle of [10; 20; 30] is: 20 degrees

```



## ooRexx

{{trans|REXX}}

```oorexx
/*REXX program computes the  mean angle  (angles expressed in degrees). */
numeric digits 50                      /*use fifty digits of precision, */
       showDig=10                      /*··· but only display 10 digits.*/
xl = 350 10            ;      say showit(xl, meanAngleD(xl) )
xl = 90 180 270 360    ;      say showit(xl, meanAngleD(xl) )
xl = 10 20 30          ;      say showit(xl, meanAngleD(xl) )
exit                                   /*stick a fork in it, we're done.*/
/*----------------------------------MEANANGD subroutine-----------------*/
meanAngleD:  procedure;  parse arg xl;   numeric digits digits()+digits()%4
sum.=0
n=words(xl)
do j=1 to n
  sum.0sin+=rxCalcSin(word(xl,j))
  sum.0cos+=rxCalcCos(word(xl,j))
  End
If sum.0cos=0 Then
  Return sign(sum.0sin/n)*90
Else
  Return rxCalcArcTan((sum.0sin/n)/(sum.0cos/n))

showit: procedure expose showDig;  numeric digits showDig;  parse arg a,mA
        return left('angles='a,30)  'mean angle='  format(mA,,showDig,0)/1

::requires rxMath library;
```

{{out}}

```txt
angles=350 10                  mean angle= 0
angles=90 180 270 360          mean angle= 0
angles=10 20 30                mean angle= 20
```
  
      

## PARI/GP


```parigp
meanAngle(v)=atan(sum(i=1,#v,sin(v[i]))/sum(i=1,#v,cos(v[i])))%(2*Pi)
meanDegrees(v)=meanAngle(v*Pi/180)*180/Pi
apply(meanDegrees,[[350, 10], [90, 180, 270, 360], [10, 20, 30]])
```

{{out}}

```txt
[360.000000, 296.565051, 20.0000000]
```


## Pascal

uses library math for sincos, a function of FPU80x87, atan2 and DegToRad.
Tested with free pascal.
Try to catch very small cos values and set to 0.0 degrees " Error : Not meaningful" as http://rosettacode.org/wiki/Averages/Mean_angle#Euler_Math_Toolbox complains.

```pascal
program MeanAngle;
{$IFDEF DELPHI}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  math;// sincos and atan2
type
  tAngles = array of double;

function MeanAngle(const a:tAngles;cnt:longInt):double;
// calculates mean angle.
// returns 0.0 if direction is not sure.
const
  eps = 1e-10;

var
  i : LongInt;
  s,c,
  Sumsin,SumCos : extended;
begin
  IF cnt = 0 then
  Begin
    MeanAngle := 0.0;
    EXIT;
  end;

  SumSin:= 0;
  SumCos:= 0;
  For i := Cnt-1 downto 0 do
  Begin
    sincos(DegToRad(a[i]),s,c);
    Sumsin := sumSin+s;
    SumCos := sumCos+c;
  end;
  s := SumSin/cnt;
  c := sumCos/cnt;
  IF c > eps then
    MeanAngle := RadToDeg(arctan2(s,c))
  else
    // Not meaningful
    MeanAngle := 0.0;
end;

Procedure OutMeanAngle(const a:tAngles;cnt:longInt);
var
  i : longInt;
Begin
  IF cnt > 0 then
  Begin
    write('The mean angle of [');
    For i := 0 to Cnt-2 do
      write(a[i]:0:2,',');
    write(a[Cnt-1]:0:2,'] => ');
    writeln(MeanAngle(a,cnt):0:16);
  end;
end;

var
  a:tAngles;
Begin
  setlength(a,4);

  a[0] := 350;a[1] := 10;
  OutMeanAngle(a,2);
  a[0] := 90;a[1] := 180;a[2] := 270;a[3] := 360;
  OutMeanAngle(a,4);
  a[0] := 10;a[1] := 20;a[2] := 30;
  OutMeanAngle(a,3);

  setlength(a,0);
end.
```

;output:

```txt

The mean angle of [350.00,10.00] => 0.0000000000000000
The mean angle of [90.00,180.00,270.00,360.00] => 0.0000000000000000
The mean angle of [10.00,20.00,30.00] => 20.0000000000000000
```



## Perl


```perl
sub Pi () { 3.1415926535897932384626433832795028842 }

sub meanangle {
  my($x, $y) = (0,0);
  ($x,$y) = ($x + sin($_), $y + cos($_)) for @_;
  my $atan = atan2($x,$y);
  $atan += 2*Pi while $atan < 0;    # Ghetto fmod
  $atan -= 2*Pi while $atan > 2*Pi;
  $atan;
}

sub meandegrees {
  meanangle( map { $_ * Pi/180 } @_ ) * 180/Pi;
}

print "The mean angle of [@$_] is: ", meandegrees(@$_), " degrees\n"
  for ([350,10], [90,180,270,360], [10,20,30]);
```

{{out}}

```txt

The mean angle of [350 10] is: 360 degrees
The mean angle of [90 180 270 360] is: 270 degrees
The mean angle of [10 20 30] is: 20 degrees

```



## Perl 6

{{works with|Rakudo|2015.12}}
This solution refuses to return an answer when the angles cancel out to a tiny magnitude.

```perl6
# Of course, you can still use pi and 180.
sub deg2rad { $^d * tau / 360 }
sub rad2deg { $^r * 360 / tau }

sub phase ($c)  {
    my ($mag,$ang) = $c.polar;
    return NaN if $mag < 1e-16;
    $ang;
}

sub meanAngle { rad2deg phase [+] map { cis deg2rad $_ }, @^angles }

say meanAngle($_).fmt("%.2f\tis the mean angle of "), $_ for
    [350, 10],
    [90, 180, 270, 360],
    [10, 20, 30];
```

{{out}}

```txt
-0.00	is the mean angle of 350 10
NaN	is the mean angle of 90 180 270 360
20.00	is the mean angle of 10 20 30
```



## Phix

Copied from [[Euphoria]], and slightly improved

```Phix
function atan2(atom y, atom x)
    return 2*arctan((sqrt(power(x,2)+power(y,2))-x)/y)
end function

function MeanAngle(sequence angles)
atom x=0, y=0, ai_rad
integer l=length(angles)

    for i=1 to l do
        ai_rad = angles[i]*PI/180
        x += cos(ai_rad)
        y += sin(ai_rad)
    end for
    if abs(x)<1e-16 then return "not meaningful" end if
    return sprintf("%9.5f",atan2(y,x)*180/PI)
end function

constant AngleLists = {{350,10},{90,180,270,360},{10,20,30},{180},{0,180}}
sequence ai
for i=1 to length(AngleLists) do
    ai = AngleLists[i]
    printf(1,"%+16s: Mean Angle is %s\n",{sprint(ai),MeanAngle(ai)})
end for
{} = wait_key()
```

{{out}}

```txt

        {350,10}: Mean Angle is   0.00000
{90,180,270,360}: Mean Angle is not meaningful
      {10,20,30}: Mean Angle is  20.00000
           {180}: Mean Angle is 180.00000
         {0,180}: Mean Angle is not meaningful

```



## PHP

{{trans|C}}

```php
<?php
$samples = array( 
	'1st' => array(350, 10),
	'2nd' => array(90, 180, 270, 360),
	'3rd' => array(10, 20, 30)
);

foreach($samples as $key => $sample){
	echo 'Mean angle for ' . $key . ' sample: ' . meanAngle($sample) . ' degrees.' . PHP_EOL;
} 

function meanAngle ($angles){
	$y_part = $x_part = 0;
	$size = count($angles);
	for ($i = 0; $i < $size; $i++){
		$x_part += cos(deg2rad($angles[$i]));
		$y_part += sin(deg2rad($angles[$i]));
	}
	$x_part /= $size;
	$y_part /= $size;
	return rad2deg(atan2($y_part, $x_part));
}
?>
```

{{out}}

```txt
Mean angle for 1st sample: -1.6148099320579E-15 degrees.
Mean angle for 2nd sample: -90 degrees.
Mean angle for 3rd sample: 20 degrees.
```



## PicoLisp


```PicoLisp
(load "@lib/math.l")

(de meanAngle (Lst)
   (*/
      (atan2
         (sum '((A) (sin (*/ A pi 180.0))) Lst)
         (sum '((A) (cos (*/ A pi 180.0))) Lst) )
      180.0 pi ) )

(for L '((350.0 10.0) (90.0 180.0 270.0 360.0) (10.0 20.0 30.0))
   (prinl
      "The mean angle of ["
      (glue ", " (mapcar round L '(0 .)))
      "] is: " (round (meanAngle L))) )
```

{{out}}

```txt
The mean angle of [350, 10] is: 0.000
The mean angle of [90, 180, 270, 360] is: 90.000
The mean angle of [10, 20, 30] is: 20.000
```



## PL/I


```PL/I
averages: procedure options (main);             /* 31 August 2012 */
   declare b1(2) fixed initial (350, 10);
   declare b2(4) fixed initial (90, 180, 270, 360);
   declare b3(3) fixed initial (10, 20, 30);

   put edit ( b1) (f(7));
   put edit ( ' mean=', mean(b1) )   (a, f(7) );
   put skip edit ( b3) (f(7));
   put edit ( ' mean=', mean(b3) )   (a, f(7) );
   put skip edit ( b2) (f(7));
   put edit ( ' mean=', mean(b2) )   (a, f(7) );

mean: procedure (a) returns (fixed);
   declare a(*) float (18);
   return ( atand(sum(sind(a))/hbound(a), sum(cosd(a))/hbound(a) ) );
end mean;

end averages;
```

Results (the final one brings up an error in inverse tangent):

```txt

    350     10 mean=      0
     10     20     30 mean=     20
     90    180    270    360 mean=
IBM0683I  ONCODE=1521  X or Y in ATAN(X,Y) or ATAND(X,Y) was invalid.
   At offset +000009CC in procedure with entry AVERAGES

```



## PowerShell


```PowerShell

function Get-MeanAngle ([double[]]$Angles)
{
    $x = ($Angles | ForEach-Object {[Math]::Cos($_ * [Math]::PI / 180)} | Measure-Object -Average).Average
    $y = ($Angles | ForEach-Object {[Math]::Sin($_ * [Math]::PI / 180)} | Measure-Object -Average).Average

    [Math]::Atan2($y, $x) * 180 / [Math]::PI
}

```



```PowerShell

@(350, 10), @(90, 180, 270, 360), @(10, 20, 30) | ForEach-Object {Get-MeanAngle $_}

```


{{Out}}

```txt

-2.74517688449847E-14
-90
20

```



## Python

{{works with|Python|2.6+}}

```python>>>
 from cmath import rect, phase
>>> from math import radians, degrees
>>> def mean_angle(deg):
...     return degrees(phase(sum(rect(1, radians(d)) for d in deg)/len(deg)))
... 
>>> for angles in [[350, 10], [90, 180, 270, 360], [10, 20, 30]]:
...     print('The mean angle of', angles, 'is:', round(mean_angle(angles), 12), 'degrees')
...     
The mean angle of [350, 10] is: -0.0 degrees
The mean angle of [90, 180, 270, 360] is: -90.0 degrees
The mean angle of [10, 20, 30] is: 20.0 degrees
>>> 
```



## Racket

The formula given above can be straightforwardly transcribed into a program:

```racket

#lang racket

(define (mean-angle αs)
  (radians->degrees
   (mean-angle/radians
    (map degrees->radians αs))))

(define (mean-angle/radians αs)
  (define n (length αs))
  (atan (* (/ 1 n) (for/sum ([α_j αs]) (sin α_j)))
        (* (/ 1 n) (for/sum ([α_j αs]) (cos α_j)))))

(mean-angle '(350 0 10))
(mean-angle '(90 180 270 360))
(mean-angle '(10 20 30))

```

{{out}}

```txt

-1.0710324872062297e-15
-90.0
19.999999999999996

```



## REXX

This REXX version uses an   '''ATAN2'''   solution.

The REXX language doesn't have most of the higher mathematical functions
(like '''sqrt'''), and
none of the trigonometric

functions,   so all of them have to be included as
RYO   ('''R'''oll-'''Y'''our-'''O'''wn).

```txt

Note that the second set of angles:    90    180   270   360

is the same as:                        90    180   -90     0
           and:                      -270   -180   -90  -360

and other combinations.

```

All the trigonometric functions use normalization   (converting the
angle to a unit circle)   before computation,   and most

of them use shortcuts for some exact values,   so there is a minimum of
errors due to rounding for   ''near values''   for some

common values.

The consequence is the trig functions may return exact values such
as   '''0'''   (zero)   for   '''sin(-2<big><big><math>\pi</math></big></big>)'''   instead
of   '''-8.154E-61'''.

This very small difference (almost inconsequential) makes a significant difference
when that value is used for a parameter

for the   '''ATAN2'''   function;   in
particular, the   ''sign''   of the value.

There isn't much difference between:
            -8.154e-61   and
            +8.154e-61
in magnitude, but the   '''ATAN2'''   function treats those two numbers
very differently as the angle will be in different quadrants,

thereby yielding a different value.

Usually this just results in an angle of   '''-90º'''   instead
of   '''+270º'''   (both angles are equivalent).

Also note that the REXX subroutines are largely not commented as they provide a
support structure that's normally present

in other computer programming languages as
BIFs   ('''B'''uilt-'''I'''n-'''F'''unctions);   to add comments and expand the
REXX statements

into single lines would increase the program's bulk and detract from the main program.

```rexx
/*REXX program  computes  the   mean angle   for a  group of angles  (expressed in degrees). */
call pi                                          /*define the value of  pi  to some accuracy.*/
numeric digits length(pi) - 1;                   /*use PI width  decimal digits of precision,*/
        showDig= 10                              /*only display  ten  decimal digits.        */
#= 350 10         ;   say show(#, meanAngleD(#)) /*display mean angle (in degrees), 1st case.*/
#= 90 180 270 360 ;   say show(#, meanAngleD(#)) /*   "      "    "     "    "      2nd   "  */
#= 10 20 30       ;   say show(#, meanAngleD(#)) /*   "      "    "     "    "      3rd   "  */
exit                                             /*stick a fork in it, we're all done with it*/
/*───────────────────────────────────────────────────────────────────────────────────────────*/
.sinCos: arg z,_,i; x=x*x;  do k=2 by 2 until p=z; p=z; _=-_*x/(k*(k+i)); z=z+_; end;  return z
$fuzz:  return  min(arg(1), max(1, digits() - arg(2) ) )
Acos:   procedure; parse arg x;        return pi() * .5 - Asin(x)
Atan:   parse arg x; if abs(x)=1  then return pi()*.25 * sign(x);  return Asin(x/sqrt(1 + x*x))
d2d:    return arg(1)           //  360
d2r:    return r2r(d2d(arg(1))  /   180   * pi() )
r2d:    return d2d((r2r(arg(1)) /   pi()) * 180)
r2r:    return arg(1)           // (pi()  *   2)
pi: pi=3.1415926535897932384626433832795028841971693993751058209749445923078164062862;return pi
/*───────────────────────────────────────────────────────────────────────────────────────────*/
Asin:   procedure;  parse arg x 1 z 1 o 1 p;          a= abs(x);                aa= a * a
        if a>1  then call AsinErr x                            /*argument X  is out of range.*/
        if a >= sqrt(2) * .5  then  return sign(x) * acos( sqrt(1 - aa),  '-ASIN')
          do j=2  by 2  until p=z;  p= z;  o= o * aa * (j-1) / j;   z= z +o / (j+1);   end
        return  z                                              /* [↑]  compute until no noise*/
/*───────────────────────────────────────────────────────────────────────────────────────────*/
Atan2:  procedure; parse arg y,x;                     call pi;        s= sign(y)
          select
          when x=0  then  z= s * pi * .5
          when x<0  then  if  y=0  then z= pi;  else z= s * (pi - abs( Atan(y/x) ) )
          otherwise       z= s * Atan(y / x)
          end   /*select*/;                           return z
/*───────────────────────────────────────────────────────────────────────────────────────────*/
cos:    procedure; parse arg x;      x= r2r(x);       numeric fuzz  $fuzz(6, 3)
        a= abs(x);      if a=0  then return   1;      if a=pi    then return -1
        if a=pi*.5 | a= pi*1.5  then return   0;      if a=pi/3  then return .5
                  if a= pi*2/3  then return -.5;                      return .sinCos(1, 1, -1)
/*───────────────────────────────────────────────────────────────────────────────────────────*/
meanAngleD: procedure;  parse arg x;                  numeric digits digits() + digits() % 4
            n= words(x);                              _sin= 0;              _cos= 0
               do j=1  for n;  != d2r( word(x, j));   _sin= _sin + sin(!);  _cos= _cos + cos(!)
               end   /*j*/
            return  r2d( Atan2( _sin/n, _cos/n) )
/*───────────────────────────────────────────────────────────────────────────────────────────*/
show:   parse arg a,mA;                               _= format(ma, , showDig, 0) / 1
        return left('angles='a, 30)   "mean angle="   right(_, max(4, length(_) ) )
/*───────────────────────────────────────────────────────────────────────────────────────────*/
sin:    procedure; parse arg x;   x= r2r(x);          numeric fuzz  $fuzz(5, 3)
        if x=pi * .5         then return 1;           if x==pi * 1.5  then return -1
        if abs(x)=pi | x=0   then return 0;           return .sinCos(x, x, +1)
/*───────────────────────────────────────────────────────────────────────────────────────────*/
sqrt:   procedure; parse arg x; if x=0  then return 0;  d=digits();  m.=9; numeric form; h= d+6
        numeric digits;  parse value format(x,2,1,,0) 'E0'  with  g "E" _ .;  g= g * .5'e'_ % 2
                 do j=0  while h>9;       m.j= h;               h= h % 2  +  1;     end  /*j*/
                 do k=j+5  to 0  by -1;   numeric digits m.k;   g= (g + x/g) * .5;  end  /*k*/
        return g
```

{{out|output|text=  when using the default internal inputs:}}

```txt

angles=350 10                  mean angle=    0
angles=90 180 270 360          mean angle=  -90
angles=10 20 30                mean angle=   20

```

Note that with the increase in decimal digit precision, the 2<sup>nd</sup> mean angle changed dramatically from an earlier result. 




## Ring


```ring

# Project : Averages/Mean angle

load "stdlib.ring"
decimals(6)
pi = 3.1415926535897
angles = [350,10]
see meanangle(angles, len(angles)) + nl
angles = [90,180,270,360]
see meanangle(angles, len(angles)) + nl
angles = [10,20,30]
see meanangle(angles, len(angles)) + nl
  
func meanangle(angles, n)
        sumsin = 0
        sumcos = 0
        for i = 1 to n
             sumsin = sumsin + sin(angles[i]*pi/180)
             sumcos = sumcos + cos(angles[i]*pi/180)
        next
        return 180/pi*atan3(sumsin, sumcos)
 
func atan3(y,x)
        if x <= 0
           return sign(y)*pi/2
        ok
        if x>0
           return atan(y/x)
        else
           if y>0 
              return atan(y/x)+pi
           else 
              return atan(y/x)-pi
           ok
        ok

```

Output:

```txt

-0.000000
-90
20.000000

```



## Ruby


```ruby
require 'complex' # Superfluous in Ruby >= 2.0; complex is added to core.

def deg2rad(d)
  d * Math::PI / 180
end
 
def rad2deg(r)
  r * 180 / Math::PI
end

def mean_angle(deg)
  rad2deg((deg.inject(0) {|z, d| z + Complex.polar(1, deg2rad(d))} / deg.length).arg)
end

[[350, 10], [90, 180, 270, 360], [10, 20, 30]].each {|angles|
  puts "The mean angle of %p is: %f degrees" % [angles, mean_angle(angles)]
}
```

{{out}}

```txt

The mean angle of [350, 10] is: -0.000000 degrees
The mean angle of [90, 180, 270, 360] is: -90.000000 degrees
The mean angle of [10, 20, 30] is: 20.000000 degrees

```



## Rust



```rust

use std::f64;
// the macro is from
// http://stackoverflow.com/questions/30856285/assert-eq-with-floating-
// point-numbers-and-delta
fn mean_angle(angles: &[f64]) -> f64 {
    let length: f64 = angles.len() as f64;
    let cos_mean: f64 = angles.iter().fold(0.0, |sum, i| sum + i.to_radians().cos()) / length;
    let sin_mean: f64 = angles.iter().fold(0.0, |sum, i| sum + i.to_radians().sin()) / length;
    (sin_mean).atan2(cos_mean).to_degrees()
}

fn main() {
    let angles1 = [350.0_f64, 10.0];
    let angles2 = [90.0_f64, 180.0, 270.0, 360.0];
    let angles3 = [10.0_f64, 20.0, 30.0];
    println!("Mean Angle for {:?}  is {:.5} degrees",
             &angles1,
             mean_angle(&angles1));
    println!("Mean Angle for {:?}  is {:.5} degrees",
             &angles2,
             mean_angle(&angles2));
    println!("Mean Angle for {:?}  is {:.5} degrees",
             &angles3,
             mean_angle(&angles3));
}

macro_rules! assert_diff{
    ($x: expr,$y : expr, $diff :expr)=>{
        if ( $x - $y ).abs() > $diff {
            panic!("floating point difference is to big {}", $x - $y );
        }
    }
}

#[test]
fn calculate() {
    let angles1 = [350.0_f64, 10.0];
    let angles2 = [90.0_f64, 180.0, 270.0, 360.0];
    let angles3 = [10.0_f64, 20.0, 30.0];
    assert_diff!(0.0, mean_angle(&angles1), 0.001);
    assert_diff!(-90.0, mean_angle(&angles2), 0.001);
    assert_diff!(20.0, mean_angle(&angles3), 0.001);
}

```



## Scala

{{libheader|Scala}}
```Scala
trait MeanAnglesComputation {
  import scala.math.{Pi, atan2, cos, sin}

  def meanAngle(angles: List[Double], convFactor: Double = 180.0 / Pi) = {
    val sums = angles.foldLeft((.0, .0))((r, c) => {
      val rads = c / convFactor
      (r._1 + sin(rads), r._2 + cos(rads))
    })
    val result = atan2(sums._1, sums._2)
    (result + (if (result.signum == -1) 2 * Pi else 0.0)) * convFactor
  }
}

object MeanAngles extends App with MeanAnglesComputation {
  assert(meanAngle(List(350, 10), 180.0 / math.Pi).round == 360, "Unexpected result with 350, 10")
  assert(meanAngle(List(90, 180, 270, 360)).round == 270, "Unexpected result with 90, 180, 270, 360")
  assert(meanAngle(List(10, 20, 30)).round == 20, "Unexpected result with 10, 20, 30")
  println("Successfully completed without errors.")
}
```



## Scheme


{{trans|Common Lisp}}


```scheme

(import (srfi 1 lists))  ;; use 'fold' from library

(define (average l)
  (/ (fold + 0 l) (length l)))

(define pi 3.14159265358979323846264338327950288419716939937510582097) 

(define (radians a)
  (* pi 1/180 a))

(define (degrees a)
  (* 180 (/ 1 pi) a))

(define (mean-angle angles)
  (let* ((angles (map radians angles))
         (cosines (map cos angles))
         (sines (map sin angles)))
    (degrees (atan (average sines) (average cosines)))))

(for-each (lambda (angles)
            (display "The mean angle of ") (display angles) 
            (display " is ") (display (mean-angle angles)) (newline))
          '((350 10) (90 180 270 360) (10 20 30)))

```



```txt

The mean angle of (350 10) is -1.614809932057922E-15
The mean angle of (90 180 270 360) is -90.0
The mean angle of (10 20 30) is 19.999999999999996

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";
  include "complex.s7i";

const func float: deg2rad (in float: degree) is
  return degree * PI / 180.0;

const func float: rad2deg (in float: rad) is
  return rad * 180.0 / PI;

const func float: meanAngle (in array float: degrees) is func
  result
    var float: mean is 0.0;
  local
    var float: degree is 0.0;
    var complex: sum is complex.value;
  begin
    for degree range degrees do
      sum +:= polar(1.0, deg2rad(degree));
    end for;
    mean := rad2deg(arg(sum / complex conv length(degrees)));
  end func;

const proc: main is func
  begin
    writeln(meanAngle([] (350.0, 10.0)) digits 4);
    writeln(meanAngle([] (90.0, 180.0, 270.0, 360.0)) digits 4);
    writeln(meanAngle([] (10.0, 20.0, 30.0)) digits 4);
  end func;
```
{{out}}
 0.0000
 90.0000
 20.0000


## Sidef


```ruby
func mean_angle(angles) {
    atan2(
        Math.avg(angles.map{ .deg2rad.sin }...),
        Math.avg(angles.map{ .deg2rad.cos }...),
    ) -> rad2deg
}

[[350,10], [90,180,270,360], [10,20,30]].each { |angles|
  say "The mean angle of #{angles.dump} is: #{ '%.2f' % mean_angle(angles)} degrees"
}
```

{{out}}

```txt

The mean angle of [350, 10] is: 0.00 degrees
The mean angle of [90, 180, 270, 360] is: -90.00 degrees
The mean angle of [10, 20, 30] is: 20.00 degrees

```



## Stata


```stata
mata
function meanangle(a) {
	return(arg(sum(exp(C(0,a)))))
}

deg=pi()/180

meanangle((350,10)*deg)/deg
  -1.61481e-15
meanangle((90,180,270,360)*deg)/deg
  0
meanangle((10,20,30)*deg)/deg
  20
end
```



## Tcl


```tcl
proc meanAngle {angles} {
    set toRadians [expr {atan2(0,-1) / 180}]
    set sumSin [set sumCos 0.0]
    foreach a $angles {
	set sumSin [expr {$sumSin + sin($a * $toRadians)}]
	set sumCos [expr {$sumCos + cos($a * $toRadians)}]
    }
    # Don't need to divide by counts; atan2() cancels that out
    return [expr {atan2($sumSin, $sumCos) / $toRadians}]
}
```

Demonstration code:

```tcl
# A little pretty-printer
proc printMeanAngle {angles} {
    puts [format "mean angle of \[%s\] = %.2f" \
	      [join $angles ", "] [meanAngle $angles]]
}

printMeanAngle {350 10}
printMeanAngle {90 180 270 360}
printMeanAngle {10 20 30}
```

{{out}}

```txt

mean angle of [350, 10] = -0.00
mean angle of [90, 180, 270, 360] = -90.00
mean angle of [10, 20, 30] = 20.00

```



## XPL0


```XPL0
include c:\cxpl\codes;

def Pi  = 3.14159265358979323846;
def D2R = Pi/180.0;             \coefficient to convert degrees to radians

func real MeanAng(A);           \Return the mean of the given list of angles
int  A;
real X, Y;
int  I;
[X:= 0.0;  Y:= 0.0;
for I:= 1 to A(0) do
    [X:= X + Cos(D2R*float(A(I)));
     Y:= Y + Sin(D2R*float(A(I)));
    ];
return ATan2(Y,X)/D2R;
];

[Format(5, 15);
RlOut(0, MeanAng([2, 350, 10]));  CrLf(0);
RlOut(0, MeanAng([4, 90, 180, 270, 360]));  CrLf(0);
RlOut(0, MeanAng([3, 10, 20, 30]));  CrLf(0);
]
```

{{out}}

```txt

   -0.000000000000003
  -90.000000000000000
   20.000000000000000

```


The second example is interesting because it computes ATan2(0.,0.), which is undefined in mathematics (like division by zero), but the floating point processor in IBM-PC-type computers defines it as zero. The reason -90 gets displayed instead is due to small rounding errors (and another extra-mathematical concept, -0). In fact almost any angle can result because of slight rounding errors as Y and X both approach zero.


## VBA


```vb
Option Base 1
Private Function mean_angle(angles As Variant) As Double
    Dim sins() As Double, coss() As Double
    ReDim sins(UBound(angles))
    ReDim coss(UBound(angles))
    For i = LBound(angles) To UBound(angles)
        sins(i) = Sin(WorksheetFunction.Radians(angles(i)))
        coss(i) = Cos(WorksheetFunction.Radians(angles(i)))
    Next i
    mean_angle = WorksheetFunction.Degrees( _
        WorksheetFunction.Atan2( _
        WorksheetFunction.sum(coss), _
        WorksheetFunction.sum(sins)))
End Function
Public Sub main()
    Debug.Print Format(mean_angle([{350,10}]), "##0")
    Debug.Print Format(mean_angle([{90, 180, 270, 360}]), "##0")
    Debug.Print Format(mean_angle([{10, 20, 30}]), "##0")
End Sub
```
{{out}}

```txt
0
-90
20
```


## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Math

Module Module1

    Function MeanAngle(angles As Double()) As Double
        Dim x = angles.Sum(Function(a) Cos(a * PI / 180)) / angles.Length
        Dim y = angles.Sum(Function(a) Sin(a * PI / 180)) / angles.Length
        Return Atan2(y, x) * 180 / PI
    End Function

    Sub Main()
        Dim printMean = Sub(x As Double()) Console.WriteLine("{0:0.###}", MeanAngle(x))
        printMean({350, 10})
        printMean({90, 180, 270, 360})
        printMean({10, 20, 30})
    End Sub

End Module
```

{{out}}

```txt
0
-90
20
```



## zkl


```zkl
fcn meanA(a1,a2,etc){ 
   as:=vm.arglist.pump(List,"toFloat","toRad");
   n:=as.len();
   (as.apply("sin").sum(0.0)/n)
   .atan2(as.apply("cos").sum(0.0)/n)
   .toDeg()
}
```

{{out}}

```txt

zkl: meanA(350,10)
-1.61481e-15
zkl: meanA(90,180,270,360)
-90
meanA(10,20,30)
20

```


[[Category:Geometry]]
