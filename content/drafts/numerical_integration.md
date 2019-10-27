+++
title = "Numerical integration"
description = ""
date = 2019-09-07T10:12:56Z
aliases = []
[extra]
id = 2428
[taxonomies]
categories = []
tags = []
+++

{{task|Arithmetic operations}}

Write functions to calculate the definite integral of a function <big><big> {{math|1=''ƒ(x)''}} </big></big> using ''all'' five of the following methods:
:* [[wp:Rectangle_method|rectangular]] 
:**  left
:**  right
:**  midpoint
:* [[wp:Trapezoidal_rule|trapezium]]
:* [[wp:Simpson%27s_rule|Simpson's]]
:** composite

Your functions should take in the upper and lower bounds ({{math|''a''}} and {{math|''b''}}), and the number of approximations to make in that range ({{math|''n''}}). 

Assume that your example already has a function that gives values for <big> {{math|1=''ƒ(x)''}} </big>.

Simpson's method is defined by the following pseudo-code:
{| class="mw-collapsible mw-collapsed"
|+ Pseudocode: Simpson's method, composite
|-
|
 '''procedure''' quad_simpson_composite(f, a, b, n)
     h := (b - a) / n
     sum1 := f(a + h/2)
     sum2 := 0
 
     loop on i from 1 to (n - 1)
         sum1 := sum1 + f(a + h * i + h/2)
         sum2 := sum2 + f(a + h * i)

     ''answer'' := (h / 6) * (f(a) + f(b) + 4*sum1 + 2*sum2)
|}


Demonstrate your function by showing the results for:
* {{math|1=ƒ(x) = x<sup>3</sup>}},   where   '''x'''   is  [0,1],  with 100 approximations.  The exact result is  1/4,  or  0.25.
* {{math|1=ƒ(x) = 1/x}},   where  '''x'''  is  [1,100],  with 1,000 approximations.  The exact result is the natural log of 100,  or about  4.605170
* {{math|1=ƒ(x) = x}},    where  '''x'''  is  [0,5000],  with 5,000,000 approximations.  The exact result is  12,500,000.
* {{math|1=ƒ(x) = x}},    where  '''x'''  is  [0,6000],  with 6,000,000 approximations.  The exact result is  18,000,000.

<br/>
'''See also'''
* [[Active object]] for integrating a function of real time.
* [[Special:PrefixIndex/Numerical integration]] for other integration methods.

<br/>


## ActionScript

Integration functions:

```ActionScript
function leftRect(f:Function, a:Number, b:Number, n:uint):Number 
{
	var sum:Number = 0;
	var dx:Number = (b-a)/n;
	for (var x:Number = a; n > 0; n--, x += dx)
		sum += f(x);
	return sum * dx;
}

function rightRect(f:Function, a:Number, b:Number, n:uint):Number
{
	var sum:Number = 0;
	var dx:Number = (b-a)/n;
	for (var x:Number = a + dx; n > 0; n--, x += dx)
		sum += f(x);
	return sum * dx;
}

function midRect(f:Function, a:Number, b:Number, n:uint):Number
{
	var sum:Number = 0;
	var dx:Number = (b-a)/n;
	for (var x:Number = a + (dx / 2); n > 0; n--, x += dx)
		sum += f(x);
	return sum * dx;
}
function trapezium(f:Function, a:Number, b:Number, n:uint):Number
{
	var dx:Number = (b-a)/n;
	var x:Number = a;
	var sum:Number = f(a);
	for(var i:uint = 1; i < n; i++)
	{
		a += dx;
		sum += f(a)*2;
	}
	sum += f(b);
	return 0.5 * dx * sum;
}
function simpson(f:Function, a:Number, b:Number, n:uint):Number
{
	var dx:Number = (b-a)/n;
	var sum1:Number = f(a + dx/2);
	var sum2:Number = 0;
	for(var i:uint = 1; i < n; i++)
	{
		sum1 += f(a + dx*i + dx/2);
		sum2 += f(a + dx*i);
	}
	return (dx/6) * (f(a) + f(b) + 4*sum1 + 2*sum2);
}
```

Usage:

```ActionScript
function f1(n:Number):Number {
	return (2/(1+ 4*(n*n)));
}
trace(leftRect(f1, -1, 2, 4));
trace(rightRect(f1, -1, 2, 4));
trace(midRect(f1, -1, 2, 4));
trace(trapezium(f1, -1, 2 ,4 ));
trace(simpson(f1, -1, 2 ,4 ));

```


## Ada

Specification of a generic package implementing the five specified kinds of numerical integration:

```ada
generic
   type Scalar is digits <>;
   with function F (X : Scalar) return Scalar;
package Integrate is
   function Left_Rectangular     (A, B : Scalar; N : Positive) return Scalar;
   function Right_Rectangular    (A, B : Scalar; N : Positive) return Scalar;
   function Midpoint_Rectangular (A, B : Scalar; N : Positive) return Scalar;
   function Trapezium            (A, B : Scalar; N : Positive) return Scalar;
   function Simpsons             (A, B : Scalar; N : Positive) return Scalar;
end Integrate;
```

An alternative solution is to pass a function reference to the integration function.  This solution is probably slightly faster, and works even with Ada83.  One could also make each integration function generic, instead of making the whole package generic.

Body of the package implementing numerical integration:

```ada
package body Integrate is
   function Left_Rectangular (A, B : Scalar; N : Positive) return Scalar is
      H   : constant Scalar := (B - A) / Scalar (N);
      Sum : Scalar := 0.0;
      X   : Scalar;
   begin
      for I in 0 .. N - 1 loop
         X := A + Scalar (I) * H;
         Sum := Sum + H * F (X);
      end loop;
      return Sum;
   end Left_Rectangular;

   function Right_Rectangular (A, B : Scalar; N : Positive) return Scalar is
      H   : constant Scalar := (B - A) / Scalar (N);
      Sum : Scalar := 0.0;
      X   : Scalar;
   begin
      for I in 1 .. N loop
         X := A + Scalar (I) * H;
         Sum := Sum + H * F (X);
      end loop;
      return Sum;
   end Right_Rectangular;

   function Midpoint_Rectangular (A, B : Scalar; N : Positive) return Scalar is
      H   : constant Scalar := (B - A) / Scalar (N);
      Sum : Scalar := 0.0;
      X   : Scalar;
   begin
      for I in 1 .. N loop
         X := A + Scalar (I) * H - 0.5 * H;
         Sum := Sum + H * F (X);
      end loop;
      return Sum;
   end Midpoint_Rectangular;

   function Trapezium (A, B : Scalar; N : Positive) return Scalar is
      H   : constant Scalar := (B - A) / Scalar (N);
      Sum : Scalar := F(A) + F(B);
      X   : Scalar := 1.0;
   begin
      while X <= Scalar (N) - 1.0 loop
         Sum := Sum + 2.0 * F (A + X * (B - A) / Scalar (N));
         X := X + 1.0;
      end loop;
      return (B - A) / (2.0 * Scalar (N)) * Sum;
   end Trapezium;

   function Simpsons (A, B : Scalar; N : Positive) return Scalar is
      H     : constant Scalar := (B - A) / Scalar (N);
      Sum_1 : Scalar := 0.0;
      Sum_2 : Scalar := 0.0;
   begin
      for I in 0 .. N - 1 loop
         Sum_1 := Sum_1 + F (A + H * Scalar (I) + 0.5 * H);
         Sum_2 := Sum_2 + F (A + H * Scalar (I));
      end loop;
      return H / 6.0 * (F (A) + F (B) + 4.0 * Sum_1 + 2.0 * Sum_2);
   end Simpsons;
end Integrate;
```


Test driver:

```ada
with Ada.Text_IO, Ada.Integer_Text_IO;
with Integrate;

procedure Numerical_Integration is
   type Scalar is digits 18;
   package Scalar_Text_IO is new Ada.Text_IO.Float_IO (Scalar);

   generic
      with function F (X : Scalar) return Scalar;
      Name     : String;
      From, To : Scalar;
      Steps    : Positive;
   procedure Test;

   procedure Test is
      package Integrate_Scalar_F is new Integrate (Scalar, F);
      use Ada.Text_IO, Ada.Integer_Text_IO, Integrate_Scalar_F, Scalar_Text_IO;
   begin
      Put (Name & " integrated from ");
      Put (From);
      Put (" to ");
      Put (To);
      Put (" in ");
      Put (Steps);
      Put_Line (" steps:");

      Put ("Rectangular (left):     ");
      Put (Left_Rectangular (From, To, Steps));
      New_Line;

      Put ("Rectangular (right):    ");
      Put (Right_Rectangular (From, To, Steps));
      New_Line;

      Put ("Rectangular (midpoint): ");
      Put (Midpoint_Rectangular (From, To, Steps));
      New_Line;

      Put ("Trapezium:              ");
      Put (Trapezium (From, To, Steps));
      New_Line;

      Put ("Simpson's:              ");
      Put (Simpsons (From, To, Steps));
      New_Line;

      New_Line;
   end Test;
begin
   Ada.Integer_Text_IO.Default_Width := 0;
   Scalar_Text_IO.Default_Fore := 0;
   Scalar_Text_IO.Default_Exp  := 0;

Cubed:
   declare
      function F (X : Scalar) return Scalar is
      begin
         return X ** 3;
      end F;
      procedure Run is new Test (F     => F,
                                 Name  => "x^3",
                                 From  => 0.0,
                                 To    => 1.0,
                                 Steps => 100);
   begin
      Run;
   end Cubed;

One_Over_X:
   declare
      function F (X : Scalar) return Scalar is
      begin
         return 1.0 / X;
      end F;
      procedure Run is new Test (F     => F,
                                 Name  => "1/x",
                                 From  => 1.0,
                                 To    => 100.0,
                                 Steps => 1_000);
   begin
      Run;
   end One_Over_X;

X:
   declare
      function F (X : Scalar) return Scalar is
      begin
         return X;
      end F;
      procedure Run_1 is new Test (F     => F,
                                   Name  => "x",
                                   From  => 0.0,
                                   To    => 5_000.0,
                                   Steps => 5_000_000);
      procedure Run_2 is new Test (F     => F,
                                   Name  => "x",
                                   From  => 0.0,
                                   To    => 6_000.0,
                                   Steps => 6_000_000);
   begin
      Run_1;
      Run_2;
   end X;
end Numerical_Integration;

```



## ALGOL 68


```algol68
MODE F = PROC(LONG REAL)LONG REAL;

###############
## left rect ##
###############

PROC left rect = (F f, LONG REAL a, b, INT n) LONG REAL:
BEGIN
   LONG REAL h= (b - a) / n;
   LONG REAL sum:= 0;
   LONG REAL x:= a;
   WHILE x <= b - h DO
      sum := sum + (h * f(x));
      x +:= h
   OD;
   sum
END # left rect #; 

#################
## right rect  ##
#################

PROC right rect = (F f, LONG REAL a, b, INT n) LONG REAL:
BEGIN
   LONG REAL h= (b - a) / n;
   LONG REAL sum:= 0;
   LONG REAL x:= a + h;
   WHILE x <= b DO
      sum := sum + (h * f(x));
      x +:= h
   OD;
   sum
END # right rect #;

###############
## mid rect  ##
###############

PROC mid rect = (F f, LONG REAL a, b, INT n) LONG REAL:
BEGIN
   LONG REAL h= (b - a) / n;
   LONG REAL sum:= 0;
   LONG REAL x:= a;
   WHILE x <= b - h DO
      sum := sum + h * f(x + h / 2);
      x +:= h
   OD;
   sum
END # mid rect #;

###############
## trapezium ##
############### 

PROC trapezium = (F f, LONG REAL a, b, INT n) LONG REAL:
BEGIN
   LONG REAL h= (b - a) / n;
   LONG REAL sum:= f(a) + f(b);
   LONG REAL x:= 1;
   WHILE x <= n - 1 DO
      sum := sum + 2 * f(a + x * h );
      x +:= 1
   OD;
   (b - a) / (2 * n) * sum
END # trapezium #; 

#############
## simpson ##
############# 

PROC simpson = (F f, LONG REAL a, b, INT n) LONG REAL:
BEGIN
   LONG REAL h= (b - a) / n;
   LONG REAL sum1:= 0;
   LONG REAL sum2:= 0;
   INT limit:= n - 1;
   FOR i FROM 0 TO limit DO
      sum1 := sum1 + f(a + h * LONG REAL(i) + h / 2)
   OD;
   FOR i FROM 1 TO limit DO
      sum2 +:= f(a + h * LONG REAL(i))
   OD;
   h / 6 * (f(a) + f(b) + 4 * sum1 + 2 * sum2)
END # simpson #;

# test the above procedures #
PROC test integrators = ( STRING     legend
                        , F          function
                        , LONG REAL  lower limit
                        , LONG REAL  upper limit
                        , INT        iterations
                        ) VOID:
BEGIN
    print( ( legend
           , fixed(  left rect( function, lower limit, upper limit, iterations ), -20, 6 )
           , fixed( right rect( function, lower limit, upper limit, iterations ), -20, 6 )
           , fixed(   mid rect( function, lower limit, upper limit, iterations ), -20, 6 )
           , fixed(  trapezium( function, lower limit, upper limit, iterations ), -20, 6 )
           , fixed(    simpson( function, lower limit, upper limit, iterations ), -20, 6 )
           , newline
           )
         )
END; # test integrators #
print( ( "   "
       , "           left rect"
       , "          right rect"
       , "            mid rect"
       , "           trapezium"
       , "             simpson"
       , newline
       )
     );
test integrators( "x^3", ( LONG REAL x )LONG REAL: x * x * x, 0,     1,       100 );
test integrators( "1/x", ( LONG REAL x )LONG REAL: 1 / x,     1,   100,     1 000 );
test integrators( "x  ", ( LONG REAL x )LONG REAL: x,         0, 5 000, 5 000 000 );
test integrators( "x  ", ( LONG REAL x )LONG REAL: x,         0, 6 000, 6 000 000 );

SKIP
```

{{out}}

```txt

              left rect          right rect            mid rect           trapezium             simpson
x^3            0.245025            0.255025            0.249988            0.250025            0.250000
1/x            4.654991            4.556981            4.604763            4.605986            4.605170
x       12499997.500000     12500002.500000     12500000.000000     12500000.000000     12500000.000000
x       17999997.000000     18000003.000000     18000000.000000     18000000.000000     18000000.000000
```



## AutoHotkey

ahk [http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=139 discussion]

```autohotkey
MsgBox % Rect("fun", 0, 1, 10,-1) ; 0.45 left
MsgBox % Rect("fun", 0, 1, 10)    ; 0.50 mid
MsgBox % Rect("fun", 0, 1, 10, 1) ; 0.55 right
MsgBox % Trapez("fun", 0, 1, 10)  ; 0.50
MsgBox % Simpson("fun", 0, 1, 10) ; 0.50

Rect(f,a,b,n,side=0) { ; side: -1=left, 0=midpoint, 1=right
   h := (b - a) / n
   sum := 0, a += (side-1)*h/2
   Loop %n%
      sum += %f%(a + h*A_Index)
   Return h*sum
}

Trapez(f,a,b,n) {
   h := (b - a) / n
   sum := 0
   Loop % n-1
      sum += %f%(a + h*A_Index)
   Return h/2 * (%f%(a) + %f%(b) + 2*sum)
}

Simpson(f,a,b,n) {
   h := (b - a) / n
   sum1 := sum2 := 0, ah := a - h/2
   Loop %n%
      sum1 += %f%(ah + h*A_Index)
   Loop % n-1
      sum2 += %f%(a + h*A_Index)
   Return h/6 * (%f%(a) + %f%(b) + 4*sum1 + 2*sum2)
}

fun(x) { ; linear test function
   Return x
}
```
 


## BASIC

{{works with|QuickBasic|4.5}}
{{trans|Java}}

```qbasic
FUNCTION leftRect(a, b, n)
     h = (b - a) / n
     sum = 0
     FOR x = a TO b - h STEP h
        sum = sum + h * (f(x))
     NEXT x
     leftRect = sum
END FUNCTION

FUNCTION rightRect(a, b, n)
     h = (b - a) / n
     sum = 0
     FOR x = a + h TO b STEP h
        sum = sum + h * (f(x))
     NEXT x
     rightRect = sum
END FUNCTION

FUNCTION midRect(a, b, n)
     h = (b - a) / n
     sum = 0
     FOR x = a + h / 2 TO b - h / 2 STEP h
        sum = sum + h * (f(x))
     NEXT x
     midRect = sum
END FUNCTION

FUNCTION trap(a, b, n)
     h = (b - a) / n
     sum = f(a) + f(b)
     FOR i = 1 TO n-1
        sum = sum + 2 * f((a + i * h))
     NEXT i
     trap = h / 2 * sum
END FUNCTION

FUNCTION simpson(a, b, n)
     h = (b - a) / n
     sum1 = 0
     sum2 = 0

     FOR i = 0 TO n-1
        sum1 = sum1 + f(a + h * i + h / 2)
     NEXT i

     FOR i = 1 TO n - 1
        sum2 = sum2 + f(a + h * i)
     NEXT i

     simpson = h / 6 * (f(a) + f(b) + 4 * sum1 + 2 * sum2)
END FUNCTION
```



## BBC BASIC


```bbcbasic
      *FLOAT64
      @% = 12 : REM Column width
      
      PRINT "Function     Range          L-Rect      R-Rect      M-Rect      Trapeze     Simpson"
      FOR func% = 1 TO 4
        READ x$, l, h, s%
        PRINT x$, ; l " - " ; h, FNlrect(x$, l, h, s%) FNrrect(x$, l, h, s%) ;
        PRINT FNmrect(x$, l, h, s%) FNtrapeze(x$, l, h, s%) FNsimpson(x$, l, h, s%)
      NEXT
      END
      
      DATA "x^3", 0,    1,     100
      DATA "1/x", 1,  100,    1000
      DATA "x",   0, 5000, 5000000
      DATA "x",   0, 6000, 6000000
      
      DEF FNlrect(x$, a, b, n%)
      LOCAL i%, d, s, x
      d = (b - a) / n%
      x = a
      FOR i% = 1 TO n%
        s += d * EVAL(x$)
        x += d
      NEXT
      = s
      
      DEF FNrrect(x$, a, b, n%)
      LOCAL i%, d, s, x
      d = (b - a) / n%
      x = a
      FOR i% = 1 TO n%
        x += d
        s += d * EVAL(x$)
      NEXT
      = s
      
      DEF FNmrect(x$, a, b, n%)
      LOCAL i%, d, s, x
      d = (b - a) / n%
      x = a
      FOR i% = 1 TO n%
        x += d/2
        s += d * EVAL(x$)
        x += d/2
      NEXT
      = s
      
      DEF FNtrapeze(x$, a, b, n%)
      LOCAL i%, d, f, s, x
      d = (b - a) / n%
      x = b : f = EVAL(x$)
      x = a : s = d * (f + EVAL(x$)) / 2
      FOR i% = 1 TO n%-1
        x += d
        s += d * EVAL(x$)
      NEXT
      = s
      
      DEF FNsimpson(x$, a, b, n%)
      LOCAL i%, d, f, s1, s2, x
      d = (b - a) / n%
      x = b : f = EVAL(x$)
      x = a + d/2 : s1 = EVAL(x$)
      FOR i% = 1 TO n%-1
        x += d/2
        s2 += EVAL(x$)
        x += d/2
        s1 += EVAL(x$)
      NEXT
      x = a
      = (d / 6) * (f + EVAL(x$) + 4 * s1 + 2 * s2)
```

'''Output:'''

```txt

Function     Range          L-Rect      R-Rect      M-Rect      Trapeze     Simpson
x^3         0 - 1           0.245025    0.255025   0.2499875    0.250025        0.25
1/x         1 - 100       4.65499106  4.55698106  4.60476255  4.60598606  4.60517038
x           0 - 5000      12499997.5  12500002.5    12500000    12500000    12500000
x           0 - 6000        17999997    18000003    18000000    18000000    18000000

```



## C


```c>#include <stdio.h

#include <stdlib.h>
#include <math.h>

double int_leftrect(double from, double to, double n, double (*func)())
{
   double h = (to-from)/n;
   double sum = 0.0, x;
   for(x=from; x <= (to-h); x += h)
      sum += func(x);
   return h*sum;
}

double int_rightrect(double from, double to, double n, double (*func)())
{
   double h = (to-from)/n;
   double sum = 0.0, x;
   for(x=from; x <= (to-h); x += h)
     sum += func(x+h);
   return h*sum;
}

double int_midrect(double from, double to, double n, double (*func)())
{
   double h = (to-from)/n;
   double sum = 0.0, x;
   for(x=from; x <= (to-h); x += h)
     sum += func(x+h/2.0);
   return h*sum;
}

double int_trapezium(double from, double to, double n, double (*func)())
{
   double h = (to - from) / n;
   double sum = func(from) + func(to);
   int i;
   for(i = 1;i < n;i++)
       sum += 2.0*func(from + i * h);
   return  h * sum / 2.0;
}

double int_simpson(double from, double to, double n, double (*func)())
{
   double h = (to - from) / n;
   double sum1 = 0.0;
   double sum2 = 0.0;
   int i;

   double x;
   
   for(i = 0;i < n;i++)
      sum1 += func(from + h * i + h / 2.0);

   for(i = 1;i < n;i++)
      sum2 += func(from + h * i);

   return h / 6.0 * (func(from) + func(to) + 4.0 * sum1 + 2.0 * sum2);
}
```



```c
/* test */
double f3(double x)
{
  return x;
}

double f3a(double x)
{
  return x*x/2.0;
}

double f2(double x)
{
  return 1.0/x;
}

double f2a(double x)
{
  return log(x);
}

double f1(double x)
{
  return x*x*x;
}

double f1a(double x)
{
  return x*x*x*x/4.0;
}

typedef double (*pfunc)(double, double, double, double (*)());
typedef double (*rfunc)(double);

#define INTG(F,A,B) (F((B))-F((A)))

int main()
{
     int i, j;
     double ic;
     
     pfunc f[5] = { 
       int_leftrect, int_rightrect,
       int_midrect,  int_trapezium,
       int_simpson 
     };
     const char *names[5] = {
       "leftrect", "rightrect", "midrect",
       "trapezium", "simpson" 
     };
     rfunc rf[] = { f1, f2, f3, f3 };
     rfunc If[] = { f1a, f2a, f3a, f3a };
     double ivals[] = { 
       0.0, 1.0,
       1.0, 100.0,
       0.0, 5000.0,
       0.0, 6000.0
     };
     double approx[] = { 100.0, 1000.0, 5000000.0, 6000000.0 };
          
     for(j=0; j < (sizeof(rf) / sizeof(rfunc)); j++)
     {
       for(i=0; i < 5 ; i++)
       {
         ic = (*f[i])(ivals[2*j], ivals[2*j+1], approx[j], rf[j]);
         printf("%10s [ 0,1] num: %+lf, an: %lf\n",
                names[i], ic, INTG((*If[j]), ivals[2*j], ivals[2*j+1]));
       }
       printf("\n");
     }
}
```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Interval
{
    public Interval(double leftEndpoint, double size)
    {
        LeftEndpoint = leftEndpoint;
        RightEndpoint = leftEndpoint + size;
    }

    public double LeftEndpoint
    {
        get;
        set;
    }

    public double RightEndpoint
    {
        get;
        set;
    }

    public double Size
    {
        get
        {
            return RightEndpoint - LeftEndpoint;
        }
    }

    public double Center
    {
        get
        {
            return (LeftEndpoint + RightEndpoint) / 2;
        }
    }

    public IEnumerable<Interval> Subdivide(int subintervalCount)
    {
        double subintervalSize = Size / subintervalCount;
        return Enumerable.Range(0, subintervalCount).Select(index => new Interval(LeftEndpoint + index * subintervalSize, subintervalSize));
    }
}

public class DefiniteIntegral
{
    public DefiniteIntegral(Func<double, double> integrand, Interval domain)
    {
        Integrand = integrand;
        Domain = domain;
    }

    public Func<double, double> Integrand
    {
        get;
        set;
    }

    public Interval Domain
    {
        get;
        set;
    }

    public double SampleIntegrand(ApproximationMethod approximationMethod, Interval subdomain)
    {
        switch (approximationMethod)
        {
            case ApproximationMethod.RectangleLeft:
                return Integrand(subdomain.LeftEndpoint);
            case ApproximationMethod.RectangleMidpoint:
                return Integrand(subdomain.Center);
            case ApproximationMethod.RectangleRight:
                return Integrand(subdomain.RightEndpoint);
            case ApproximationMethod.Trapezium:
                return (Integrand(subdomain.LeftEndpoint) + Integrand(subdomain.RightEndpoint)) / 2;
            case ApproximationMethod.Simpson:
                return (Integrand(subdomain.LeftEndpoint) + 4 * Integrand(subdomain.Center) + Integrand(subdomain.RightEndpoint)) / 6;
            default:
                throw new NotImplementedException();
        }
    }

    public double Approximate(ApproximationMethod approximationMethod, int subdomainCount)
    {
        return Domain.Size * Domain.Subdivide(subdomainCount).Sum(subdomain => SampleIntegrand(approximationMethod, subdomain)) / subdomainCount;
    }

    public enum ApproximationMethod
    {
        RectangleLeft,
        RectangleMidpoint,
        RectangleRight,
        Trapezium,
        Simpson
    }
}

public class Program
{
    private static void TestApproximationMethods(DefiniteIntegral integral, int subdomainCount)
    {
        foreach (DefiniteIntegral.ApproximationMethod approximationMethod in Enum.GetValues(typeof(DefiniteIntegral.ApproximationMethod)))
        {
            Console.WriteLine(integral.Approximate(approximationMethod, subdomainCount));
        }
    }

    public static void Main()
    {
        TestApproximationMethods(new DefiniteIntegral(x => x * x * x, new Interval(0, 1)), 10000);
        TestApproximationMethods(new DefiniteIntegral(x => 1 / x, new Interval(1, 99)), 1000);
        TestApproximationMethods(new DefiniteIntegral(x => x, new Interval(0, 5000)), 500000);
        TestApproximationMethods(new DefiniteIntegral(x => x, new Interval(0, 6000)), 6000000);
    }
}
```

Output:
<lang>0.2499500025
0.24999999875
0.2500500025
0.250000002499999
0.25
4.65499105751468
4.60476254867838
4.55698105751468
4.60598605751468
4.60517038495713
12499975
12500000
12500025
12500000
12500000
17999997
18000000
18000003
18000000
18000000
```


## C++


Due to their similarity, it makes sense to make the integration method a policy.

```cpp
// the integration routine
template<typename Method, typename F, typename Float>
 double integrate(F f, Float a, Float b, int steps, Method m)
{
  double s = 0;
  double h = (b-a)/steps;
  for (int i = 0; i < steps; ++i)
    s += m(f, a + h*i, h);
  return h*s;
}

// methods
class rectangular
{
public:
  enum position_type { left, middle, right };
  rectangular(position_type pos): position(pos) {}
  template<typename F, typename Float>
   double operator()(F f, Float x, Float h) const
  {
    switch(position)
    {
    case left:
      return f(x);
    case middle:
      return f(x+h/2);
    case right:
      return f(x+h);
    }
  }
private:
  const position_type position;
};

class trapezium
{
public:
  template<typename F, typename Float>
   double operator()(F f, Float x, Float h) const
  {
    return (f(x) + f(x+h))/2;
  }
};

class simpson
{
public:
  template<typename F, typename Float>
   double operator()(F f, Float x, Float h) const
  {
    return (f(x) + 4*f(x+h/2) + f(x+h))/6;
  }
};

// sample usage
double f(double x) { return x*x; }

// inside a function somewhere:
double rl = integrate(f, 0.0, 1.0, 10, rectangular(rectangular::left));
double rm = integrate(f, 0.0, 1.0, 10, rectangular(rectangular::middle));
double rr = integrate(f, 0.0, 1.0, 10, rectangular(rectangular::right));
double t  = integrate(f, 0.0, 1.0, 10, trapezium());
double s  = integrate(f, 0.0, 1.0, 10, simpson());
```



## Chapel


```chapel

proc f1(x:real):real {
  return x**3;
}

proc f2(x:real):real {
  return 1/x;
}

proc f3(x:real):real {
  return x;
}

proc leftRectangleIntegration(a: real, b: real, N: int, f): real{
  var h: real = (b - a)/N;
  var sum: real = 0.0;
  var x_n: real;
  for n in 0..N-1 {
    x_n = a + n * h;
    sum = sum + f(x_n);
  }
  return h * sum;
}

proc rightRectangleIntegration(a: real, b: real, N: int, f): real{
  var h: real = (b - a)/N;
  var sum: real = 0.0;
  var x_n: real;
  for n in 0..N-1 {
    x_n = a + (n + 1) * h;
    sum = sum + f(x_n);
  }
  return h * sum;
}

proc midpointRectangleIntegration(a: real, b: real, N: int, f): real{
  var h: real = (b - a)/N;
  var sum: real = 0.0;
  var x_n: real;
  for n in 0..N-1 {
    x_n = a + (n + 0.5) * h;
    sum = sum + f(x_n);
  }
  return h * sum;
}

proc trapezoidIntegration(a: real(64), b: real(64), N: int(64), f): real{
  var h: real(64) = (b - a)/N;
  var sum: real(64) = f(a) + f(b);
  var x_n: real(64);
  for n in 1..N-1 {
    x_n = a + n * h;
    sum = sum + 2.0 * f(x_n);
  }
  return (h/2.0) * sum;
}

proc simpsonsIntegration(a: real(64), b: real(64), N: int(64), f): real{
  var h: real(64) = (b - a)/N;
  var sum: real(64) = f(a) + f(b);
  var x_n: real(64);
  for n in 1..N-1 by 2 {
    x_n = a + n * h;
    sum = sum + 4.0 * f(x_n);
  }
  for n in 2..N-2 by 2 {
    x_n = a + n * h;
    sum = sum + 2.0 * f(x_n);
  }
  return (h/3.0) * sum;
}

var exact:real;
var calculated:real;

writeln("f(x) = x**3 with 100 steps from 0 to 1");
exact = 0.25;
calculated = leftRectangleIntegration(a = 0.0, b = 1.0, N = 100, f = f1);
writeln("leftRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = rightRectangleIntegration(a = 0.0, b = 1.0, N = 100, f = f1);
writeln("rightRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = midpointRectangleIntegration(a = 0.0, b = 1.0, N = 100, f = f1);
writeln("midpointRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = trapezoidIntegration(a = 0.0, b = 1.0, N = 100, f = f1);
writeln("trapezoidIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = simpsonsIntegration(a = 0.0, b = 1.0, N = 100, f = f1);
writeln("simpsonsIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
writeln();

writeln("f(x) = 1/x with 1000 steps from 1 to 100");
exact = 4.605170;
calculated = leftRectangleIntegration(a = 1.0, b = 100.0, N = 1000, f = f2);
writeln("leftRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = rightRectangleIntegration(a = 1.0, b = 100.0, N = 1000, f = f2);
writeln("rightRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = midpointRectangleIntegration(a = 1.0, b = 100.0, N = 1000, f = f2);
writeln("midpointRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = trapezoidIntegration(a = 1.0, b = 100.0, N = 1000, f = f2);
writeln("trapezoidIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = simpsonsIntegration(a = 1.0, b = 100.0, N = 1000, f = f2);
writeln("simpsonsIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
writeln();

writeln("f(x) = x with 5000000 steps from 0 to 5000");
exact = 12500000;
calculated = leftRectangleIntegration(a = 0.0, b = 5000.0, N = 5000000, f = f3);
writeln("leftRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = rightRectangleIntegration(a = 0.0, b = 5000.0, N = 5000000, f = f3);
writeln("rightRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = midpointRectangleIntegration(a = 0.0, b = 5000.0, N = 5000000, f = f3);
writeln("midpointRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = trapezoidIntegration(a = 0.0, b = 5000.0, N = 5000000, f = f3);
writeln("trapezoidIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = simpsonsIntegration(a = 0.0, b = 5000.0, N = 5000000, f = f3);
writeln("simpsonsIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
writeln();

writeln("f(x) = x with 6000000 steps from 0 to 6000");
exact = 18000000;
calculated = leftRectangleIntegration(a = 0.0, b = 6000.0, N = 6000000, f = f3);
writeln("leftRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = rightRectangleIntegration(a = 0.0, b = 6000.0, N = 6000000, f = f3);
writeln("rightRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = midpointRectangleIntegration(a = 0.0, b = 6000.0, N = 6000000, f = f3);
writeln("midpointRectangleIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = trapezoidIntegration(a = 0.0, b = 6000.0, N = 6000000, f = f3);
writeln("trapezoidIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
calculated = simpsonsIntegration(a = 0.0, b = 6000.0, N = 6000000, f = f3);
writeln("simpsonsIntegration: calculated = ", calculated, "; exact = ", exact, "; difference = ", abs(calculated - exact));
writeln();

```

output
<lang>
f(x) = x**3 with 100 steps from 0 to 1
leftRectangleIntegration: calculated = 0.245025; exact = 0.25; difference = 0.004975
rightRectangleIntegration: calculated = 0.255025; exact = 0.25; difference = 0.005025
midpointRectangleIntegration: calculated = 0.249988; exact = 0.25; difference = 1.25e-05
trapezoidIntegration: calculated = 0.250025; exact = 0.25; difference = 2.5e-05
simpsonsIntegration: calculated = 0.25; exact = 0.25; difference = 5.55112e-17

f(x) = 1/x with 1000 steps from 1 to 100
leftRectangleIntegration: calculated = 4.65499; exact = 4.60517; difference = 0.0498211
rightRectangleIntegration: calculated = 4.55698; exact = 4.60517; difference = 0.0481889
midpointRectangleIntegration: calculated = 4.60476; exact = 4.60517; difference = 0.000407451
trapezoidIntegration: calculated = 4.60599; exact = 4.60517; difference = 0.000816058
simpsonsIntegration: calculated = 4.60517; exact = 4.60517; difference = 3.31627e-06

f(x) = x with 5000000 steps from 0 to 5000
leftRectangleIntegration: calculated = 1.25e+07; exact = 1.25e+07; difference = 2.5
rightRectangleIntegration: calculated = 1.25e+07; exact = 1.25e+07; difference = 2.5
midpointRectangleIntegration: calculated = 1.25e+07; exact = 1.25e+07; difference = 0.0
trapezoidIntegration: calculated = 1.25e+07; exact = 1.25e+07; difference = 1.86265e-09
simpsonsIntegration: calculated = 1.25e+07; exact = 1.25e+07; difference = 3.72529e-09

f(x) = x with 6000000 steps from 0 to 6000
leftRectangleIntegration: calculated = 1.8e+07; exact = 1.8e+07; difference = 3.0
rightRectangleIntegration: calculated = 1.8e+07; exact = 1.8e+07; difference = 3.0
midpointRectangleIntegration: calculated = 1.8e+07; exact = 1.8e+07; difference = 7.45058e-09
trapezoidIntegration: calculated = 1.8e+07; exact = 1.8e+07; difference = 3.72529e-09
simpsonsIntegration: calculated = 1.8e+07; exact = 1.8e+07; difference = 0.0

```


## CoffeeScript

{{trans|python}}

```coffeescript

rules =
  left_rect: (f, x, h) -> f(x)
  mid_rect: (f, x, h) -> f(x+h/2)
  right_rect: (f, x, h) -> f(x+h)
  trapezium: (f, x, h) -> (f(x) + f(x+h)) / 2
  simpson: (f, x, h) -> (f(x) + 4 * f(x + h/2) + f(x+h)) / 6

functions =
  cube: (x) -> x*x*x
  reciprocal: (x) -> 1/x
  identity: (x) -> x
 
sum = (list) -> list.reduce ((a, b) -> a+b), 0
 
integrate = (f, a, b, steps, meth) ->
   h = (b-a) / steps
   h * sum(meth(f, a+i*h, h) for i in [0...steps])
 
# Tests
tests = [
  [0, 1, 100, 'cube']
  [1, 100, 1000, 'reciprocal']
  [0, 5000, 5000000, 'identity']
  [0, 6000, 6000000, 'identity']
]

for test in tests
  [a, b, steps, func_name] = test
  func = functions[func_name]
  console.log "-- tests for #{func_name} with #{steps} steps from #{a} to #{b}"
  for rule_name, rule of rules
    result = integrate func, a, b, steps, rule
    console.log rule_name, result

```

output
<lang>
> coffee numerical_integration.coffee 
-- tests for cube with 100 steps from 0 to 1
left_rect 0.24502500000000005
mid_rect 0.24998750000000006
right_rect 0.25502500000000006
trapezium 0.250025
simpson 0.25
-- tests for reciprocal with 1000 steps from 1 to 100
left_rect 4.65499105751468
mid_rect 4.604762548678376
right_rect 4.55698105751468
trapezium 4.605986057514676
simpson 4.605170384957133
-- tests for identity with 5000000 steps from 0 to 5000
left_rect 12499997.5
mid_rect 12500000
right_rect 12500002.5
trapezium 12500000
simpson 12500000
-- tests for identity with 6000000 steps from 0 to 6000
left_rect 17999997.000000004
mid_rect 17999999.999999993
right_rect 18000003.000000004
trapezium 17999999.999999993
simpson 17999999.999999993

```



## Comal

{{works with|OpenComal on Linux}}

```Comal

     1000 PRINT "F(X)";" FROM";"   TO";"       L-Rect";"       M-Rect";"       R-Rect ";"      Trapez";"      Simpson"
     1010 fromval:=0
     1020 toval:=1
     1030 PRINT "X^3 ";
     1040 PRINT USING "#####": fromval;
     1050 PRINT USING "#####": toval;
     1060 PRINT USING "###.#########": numint(f1, "L", fromval, toval, 100);
     1070 PRINT USING "###.#########": numint(f1, "R", fromval, toval, 100);
     1080 PRINT USING "###.#########": numint(f1, "M", fromval, toval, 100);
     1090 PRINT USING "###.#########": numint(f1, "T", fromval, toval, 100);
     1100 PRINT USING "###.#########": numint(f1, "S", fromval, toval, 100)
     1110 //
     1120 fromval:=1
     1130 toval:=100
     1140 PRINT "1/X ";
     1150 PRINT USING "#####": fromval;
     1160 PRINT USING "#####": toval;
     1170 PRINT USING "###.#########": numint(f2, "L", fromval, toval, 1000);
     1180 PRINT USING "###.#########": numint(f2, "R", fromval, toval, 1000);
     1190 PRINT USING "###.#########": numint(f2, "M", fromval, toval, 1000);
     1200 PRINT USING "###.#########": numint(f2, "T", fromval, toval, 1000);
     1210 PRINT USING "###.#########": numint(f2, "S", fromval, toval, 1000)
     1220 fromval:=0
     1230 toval:=5000
     1240 PRINT "X   ";
     1250 PRINT USING "#####": fromval;
     1260 PRINT USING "#####": toval;
     1270 PRINT USING "#########.###": numint(f3, "L", fromval, toval, 5000000);
     1280 PRINT USING "#########.###": numint(f3, "R", fromval, toval, 5000000);
     1290 PRINT USING "#########.###": numint(f3, "M", fromval, toval, 5000000);
     1300 PRINT USING "#########.###": numint(f3, "T", fromval, toval, 5000000);
     1310 PRINT USING "#########.###": numint(f3, "S", fromval, toval, 5000000)
     1320 //
     1330 fromval:=0
     1340 toval:=6000
     1350 PRINT "X   ";
     1360 PRINT USING "#####": fromval;
     1370 PRINT USING "#####": toval;
     1380 PRINT USING "#########.###": numint(f3, "L", fromval, toval, 6000000);
     1390 PRINT USING "#########.###": numint(f3, "R", fromval, toval, 6000000);
     1400 PRINT USING "#########.###": numint(f3, "M", fromval, toval, 6000000);
     1410 PRINT USING "#########.###": numint(f3, "T", fromval, toval, 6000000);
     1420 PRINT USING "#########.###": numint(f3, "S", fromval, toval, 6000000)
     1430 END
     1440 //
     1450 FUNC numint(FUNC f, type$, lbound, rbound, iters) CLOSED
     1460   delta:=(rbound-lbound)/iters
     1470   integral:=0
     1480   CASE type$ OF
     1490   WHEN "L", "T", "S"
     1500     actval:=lbound
     1510   WHEN "M"
     1520     actval:=lbound+delta/2
     1530   WHEN "R"
     1540     actval:=lbound+delta
     1550   OTHERWISE
     1560     actval:=lbound
     1570   ENDCASE
     1580   FOR n:=0 TO iters-1 DO
     1590     CASE type$ OF
     1600     WHEN "L", "M", "R"
     1610       integral:+f(actval+n*delta)*delta
     1620     WHEN "T"
     1630       integral:+delta*(f(actval+n*delta)+f(actval+(n+1)*delta))/2
     1640     WHEN "S"
     1650       IF n=0 THEN
     1660         sum1:=f(lbound+delta/2)
     1670         sum2:=0
     1680       ELSE
     1690         sum1:+f(actval+n*delta+delta/2)
     1700         sum2:+f(actval+n*delta)
     1710       ENDIF
     1720     OTHERWISE
     1730       integral:=0
     1740     ENDCASE
     1750   ENDFOR
     1760   IF type$="S" THEN
     1770     RETURN (delta/6)*(f(lbound)+f(rbound)+4*sum1+2*sum2)
     1780   ELSE
     1790     RETURN integral
     1800   ENDIF
     1810 ENDFUNC
     1820 //
     1830 FUNC f1(x) CLOSED
     1840   RETURN x^3
     1850 ENDFUNC
     1860 //
     1870 FUNC f2(x) CLOSED
     1880   RETURN 1/x
     1890 ENDFUNC
     1900 //
     1910 FUNC f3(x) CLOSED
     1920   RETURN x
     1930 ENDFUNC

```

{{out}}

```txt

F(X) FROM   TO       L-Rect       M-Rect       R-Rect       Trapez      Simpson
X^3     0    1  0.245025000  0.255025000  0.249987500  0.250025000  0.250000000
1/X     1  100  4.654991058  4.556981058  4.604762549  4.605986058  4.605170385
X       0 5000 12499997.500 12500002.500 12500000.000 12500000.000 12500000.000
X       0 6000 17999997.000 18000003.000 18000000.000 18000000.000 18000000.000

```



## Common Lisp



```lisp
(defun left-rectangle (f a b n &aux (d (/ (- b a) n)))
  (* d (loop for x from a below b by d summing (funcall f x))))

(defun right-rectangle (f a b n &aux (d (/ (- b a) n)))
  (* d (loop for x from b above a by d summing (funcall f x))))

(defun midpoint-rectangle (f a b n &aux (d (/ (- b a) n)))
  (* d (loop for x from (+ a (/ d 2)) below b by d summing (funcall f x))))

(defun trapezium (f a b n &aux (d (/ (- b a) n)))
  (* (/ d 2)
     (+ (funcall f a)
        (* 2 (loop for x from (+ a d) below b by d summing (funcall f x)))
        (funcall f b))))

(defun simpson (f a b n)
  (loop with h = (/ (- b a) n)
        with sum1 = (funcall f (+ a (/ h 2)))
        with sum2 = 0
        for i from 1 below n
        do (incf sum1 (funcall f (+ a (* h i) (/ h 2))))
        do (incf sum2 (funcall f (+ a (* h i))))
        finally (return (* (/ h 6)
                           (+ (funcall f a)
                              (funcall f b)
                              (* 4 sum1)
                              (* 2 sum2))))))
```



## D


```d
import std.stdio, std.typecons, std.typetuple;

template integrate(alias method) {
    double integrate(F, Float)(in F f, in Float a,
                               in Float b, in int steps) {
        double s = 0.0;
        immutable double h = (b - a) / steps;
        foreach (i; 0 .. steps)
            s += method(f, a + h * i, h);
        return h * s;
    }
}

double rectangularLeft(F, Float)(in F f, in Float x, in Float h)
pure nothrow {
    return f(x);
}

double rectangularMiddle(F, Float)(in F f, in Float x, in Float h)
pure nothrow {
    return f(x + h / 2);
}

double rectangularRight(F, Float)(in F f, in Float x, in Float h)
pure nothrow {
    return f(x + h);
}

double trapezium(F, Float)(in F f, in Float x, in Float h)
pure nothrow {
    return (f(x) + f(x + h)) / 2;
}

double simpson(F, Float)(in F f, in Float x, in Float h)
pure nothrow {
    return (f(x) + 4 * f(x + h / 2) + f(x + h)) / 6;
}

void main() {
    immutable args = [
        tuple((double x) => x ^^ 3, 0.0, 1.0, 10),
        tuple((double x) => 1 / x, 1.0, 100.0, 1000),
        tuple((double x) => x, 0.0, 5_000.0, 5_000_000),
        tuple((double x) => x, 0.0, 6_000.0, 6_000_000)];

    alias TypeTuple!(integrate!rectangularLeft,
                     integrate!rectangularMiddle,
                     integrate!rectangularRight,
                     integrate!trapezium,
                     integrate!simpson) ints;

    alias TypeTuple!("rectangular left:   ",
                     "rectangular middle: ",
                     "rectangular right:  ",
                     "trapezium:          ",
                     "simpson:            ") names;

    foreach (a; args) {
        foreach (i, n; names)
            writefln("%s %f", n, ints[i](a.tupleof));
        writeln();
    }
}
```

Output:

```txt
rectangular left:    0.202500
rectangular middle:  0.248750
rectangular right:   0.302500
trapezium:           0.252500
simpson:             0.250000

rectangular left:    4.654991
rectangular middle:  4.604763
rectangular right:   4.556981
trapezium:           4.605986
simpson:             4.605170

rectangular left:    12499997.500000
rectangular middle:  12500000.000000
rectangular right:   12500002.500000
trapezium:           12500000.000000
simpson:             12500000.000000

rectangular left:    17999997.000000
rectangular middle:  18000000.000000
rectangular right:   18000003.000000
trapezium:           18000000.000000
simpson:             18000000.000000
```


### A faster version

This version avoids function pointers and delegates, same output:

```d
import std.stdio, std.typecons, std.typetuple;

template integrate(alias method) {
    template integrate(alias f) {
        double integrate(Float)(in Float a, in Float b,
                                in int steps) pure nothrow {
            Float s = 0.0;
            immutable Float h = (b - a) / steps;
            foreach (i; 0 .. steps)
                s += method!(f, Float)(a + h * i, h);
            return h * s;
        }
    }
}

double rectangularLeft(alias f, Float)(in Float x, in Float h)
pure nothrow {
    return f(x);
}

double rectangularMiddle(alias f, Float)(in Float x, in Float h)
pure nothrow {
    return f(x + h / 2);
}

double rectangularRight(alias f, Float)(in Float x, in Float h)
pure nothrow {
    return f(x + h);
}

double trapezium(alias f, Float)(in Float x, in Float h)
pure nothrow {
    return (f(x) + f(x + h)) / 2;
}

double simpson(alias f, Float)(in Float x, in Float h)
pure nothrow {
    return (f(x) + 4 * f(x + h / 2) + f(x + h)) / 6;
}

void main() {
    static double f1(in double x) pure nothrow { return x ^^ 3; }
    static double f2(in double x) pure nothrow { return 1 / x; }
    static double f3(in double x) pure nothrow { return x; }
    alias TypeTuple!(f1, f2, f3, f3) funcs;

    alias TypeTuple!("rectangular left:   ",
                     "rectangular middle: ",
                     "rectangular right:  ",
                     "trapezium:          ",
                     "simpson:            ") names;

    alias TypeTuple!(integrate!rectangularLeft,
                     integrate!rectangularMiddle,
                     integrate!rectangularRight,
                     integrate!trapezium,
                     integrate!simpson) ints;

    immutable args = [tuple(0.0,     1.0,        10),
                      tuple(1.0,   100.0,     1_000),
                      tuple(0.0, 5_000.0, 5_000_000),
                      tuple(0.0, 6_000.0, 6_000_000)];

    foreach (i, f; funcs) {
        foreach (j, n; names) {
            alias ints[j] integ;
            writefln("%s %f", n, integ!f(args[i].tupleof));
        }
        writeln();
    }
}
```



## E


{{trans|Python}}


```e
pragma.enable("accumulator")

def leftRect(f, x, h) {
  return f(x)
}
 
def midRect(f, x, h) {
  return f(x + h/2)
}
 
def rightRect(f, x, h) {
  return f(x + h)
}
 
def trapezium(f, x, h) {
  return (f(x) + f(x+h)) / 2
}
 
def simpson(f, x, h) {
  return (f(x) + 4 * f(x + h / 2) + f(x+h)) / 6
}
 
def integrate(f, a, b, steps, meth) {
   def h := (b-a) / steps
   return h * accum 0 for i in 0..!steps { _ + meth(f, a+i*h, h) }
}
```

 

```e
? integrate(fn x { x ** 2 }, 3.0, 7.0, 30, simpson) 
# value: 105.33333333333334

? integrate(fn x { x ** 9 }, 0, 1, 300, simpson) 
# value: 0.10000000002160479
```



## Elixir


```elixir
defmodule Numerical do
  @funs  ~w(leftrect midrect rightrect trapezium simpson)a
 
  def  leftrect(f, left,_right), do: f.(left)
  def   midrect(f, left, right), do: f.((left+right)/2)
  def rightrect(f,_left, right), do: f.(right)
  def trapezium(f, left, right), do: (f.(left)+f.(right))/2
  def   simpson(f, left, right), do: (f.(left) + 4*f.((left+right)/2.0) + f.(right)) / 6.0
 
  def integrate(f, a, b, steps) when is_integer(steps) do
    delta = (b - a) / steps
    Enum.each(@funs, fn fun ->
      total = Enum.reduce(0..steps-1, 0, fn i, acc ->
        left = a + delta * i
        acc + apply(Numerical, fun, [f, left, left+delta])
      end)
      :io.format "~10s : ~.6f~n", [fun, total * delta]
    end)
  end
end
 
f1 = fn x -> x * x * x end
IO.puts "f(x) = x^3, where x is [0,1], with 100 approximations."
Numerical.integrate(f1, 0, 1, 100)
 
f2 = fn x -> 1 / x end
IO.puts "\nf(x) = 1/x, where x is [1,100], with 1,000 approximations. "
Numerical.integrate(f2, 1, 100, 1000)
 
f3 = fn x -> x end
IO.puts "\nf(x) = x, where x is [0,5000], with 5,000,000 approximations."
Numerical.integrate(f3, 0, 5000, 5_000_000)
 
f4 = fn x -> x end
IO.puts "\nf(x) = x, where x is [0,6000], with 6,000,000 approximations."
Numerical.integrate(f4, 0, 6000, 6_000_000)
```


{{out}}

```txt

f(x) = x^3, where x is [0,1], with 100 approximations.
  leftrect : 0.245025
   midrect : 0.249988
 rightrect : 0.255025
 trapezium : 0.250025
   simpson : 0.250000

f(x) = 1/x, where x is [1,100], with 1,000 approximations.
  leftrect : 4.654991
   midrect : 4.604763
 rightrect : 4.556981
 trapezium : 4.605986
   simpson : 4.605170

f(x) = x, where x is [0,5000], with 5,000,000 approximations.
  leftrect : 12499997.500000
   midrect : 12500000.000000
 rightrect : 12500002.500000
 trapezium : 12500000.000000
   simpson : 12500000.000000

f(x) = x, where x is [0,6000], with 6,000,000 approximations.
  leftrect : 17999997.000000
   midrect : 18000000.000000
 rightrect : 18000003.000000
 trapezium : 18000000.000000
   simpson : 18000000.000000

```



## Euphoria


```euphoria
function int_leftrect(sequence bounds, integer n, integer func_id)
    atom h, sum
    h = (bounds[2]-bounds[1])/n
    sum = 0
    for x = bounds[1] to bounds[2]-h by h do
        sum += call_func(func_id, {x})
    end for
    return h*sum
end function

function int_rightrect(sequence bounds, integer n, integer func_id)
    atom h, sum
    h = (bounds[2]-bounds[1])/n
    sum = 0
    for x = bounds[1] to bounds[2]-h by h do
        sum += call_func(func_id, {x+h})
    end for
    return h*sum
end function

function int_midrect(sequence bounds, integer n, integer func_id)
    atom h, sum
    h = (bounds[2]-bounds[1])/n
    sum = 0
    for x = bounds[1] to bounds[2]-h by h do
        sum += call_func(func_id, {x+h/2})
    end for
    return h*sum
end function

function int_trapezium(sequence bounds, integer n, integer func_id)
    atom h, sum
    h = (bounds[2]-bounds[1])/n
    sum = call_func(func_id, {bounds[1]}) + call_func(func_id, {bounds[2]})
    for x = bounds[1] to bounds[2]-h by h do
        sum += 2*call_func(func_id, {x})
    end for
    return h * sum / 2
end function

function int_simpson(sequence bounds, integer n, integer func_id)
    atom h, sum1, sum2
    h = (bounds[2]-bounds[1])/n
    sum1 = call_func(func_id, {bounds[1] + h/2})
    sum2 = 0
    for i = 1 to n-1 do
        sum1 += call_func(func_id, {bounds[1] + h * i + h / 2})
        sum2 += call_func(func_id, {bounds[1] + h * i})
    end for
    return h/6 * (call_func(func_id, {bounds[1]}) +
                  call_func(func_id, {bounds[2]}) + 4*sum1 + 2*sum2)
end function

function xp2d2(atom x)
    return x*x/2
end function

function logx(atom x)
    return log(x)
end function

function x(atom x)
    return x
end function

? int_leftrect({-1,1},1000,routine_id("xp2d2"))
? int_rightrect({-1,1},1000,routine_id("xp2d2"))
? int_midrect({-1,1},1000,routine_id("xp2d2"))
? int_simpson({-1,1},1000,routine_id("xp2d2"))
puts(1,'\n')
? int_leftrect({1,2},1000,routine_id("logx"))
? int_rightrect({1,2},1000,routine_id("logx"))
? int_midrect({1,2},1000,routine_id("logx"))
? int_simpson({1,2},1000,routine_id("logx"))
puts(1,'\n')
? int_leftrect({0,10},1000,routine_id("x"))
? int_rightrect({0,10},1000,routine_id("x"))
? int_midrect({0,10},1000,routine_id("x"))
? int_simpson({0,10},1000,routine_id("x"))
```


Output:

```txt
0.332337996
0.332334
0.332334999
0.3333333333

0.3859477459
0.386640893
0.386294382
0.3862943611

49.95
50.05
50
50

```



## Factor


```factor

USE: math.functions
IN: scratchpad 0 1 [ 3 ^ ] integrate-simpson .
1/4
IN: scratchpad 1000 num-steps set-global
IN: scratchpad 1.0 100 [ -1 ^ ] integrate-simpson .
4.605173316272971
IN: scratchpad 5000000 num-steps set-global
IN: scratchpad 0 5000 [ ] integrate-simpson .
12500000
IN: scratchpad 6000000 num-steps set-global
IN: scratchpad 0 6000 [ ] integrate-simpson .
18000000
```



## Forth


```forth
fvariable step

defer method ( fn F: x -- fn[x] )

: left                    execute ;
: right  step f@       f+ execute ;
: mid    step f@ 2e f/ f+ execute ;
: trap
  dup fdup  left
      fswap right f+  2e f/ ;
: simpson
  dup fdup  left
  dup fover mid 4e f* f+
      fswap right f+  6e f/ ;
     
: set-step ( n F: a b -- n F: a )
  fover f- dup 0 d>f f/ step f! ;

: integrate ( xt n F: a b -- F: sigma )
  set-step
  0e
  0 do
    dup fover method f+
    fswap step f@ f+ fswap
  loop
  drop fnip
  step f@ f* ;
 \ testing similar to the D example
: test
  ' is method  ' 4 -1e 2e integrate f. ;

: fn1  fsincos f+ ;
: fn2  fdup f* 4e f* 1e f+ 2e fswap f/ ;

7 set-precision
test left    fn2  \ 2.456897
test right   fn2  \ 2.245132
test mid     fn2  \ 2.496091
test trap    fn2  \ 2.351014
test simpson fn2  \ 2.447732
```



## Fortran

In ISO Fortran 95 and later if function f() is not already defined to be "elemental", define an elemental wrapper function around it to allow for array-based initialization:

```fortran
elemental function elemf(x)
   real :: elemf, x
   elemf = f(x)
end function elemf
```


Use Array Initializers, Pointers, Array invocation of Elemental functions, Elemental array-array and array-scalar arithmetic, and the SUM intrinsic function. Methods are collected into a single function in a module.

```fortran
module Integration
  implicit none

contains

  ! function, lower limit, upper limit, steps, method
  function integrate(f, a, b, in, method)
    real :: integrate
    real, intent(in) :: a, b
    integer, optional, intent(in) :: in
    character(len=*), intent(in), optional :: method
    interface
       elemental function f(ra)
         real :: f
         real, intent(in) :: ra
       end function f
    end interface

    integer :: n, i, m
    real :: h
    real, dimension(:), allocatable :: xpoints
    real, dimension(:), target, allocatable :: fpoints
    real, dimension(:), pointer :: fleft, fmid, fright

    if ( present(in) ) then
       n = in
    else
       n = 20
    end if

    if ( present(method) ) then
       select case (method)
       case ('leftrect')
          m = 1
       case ('midrect')
          m = 2
       case ('rightrect')
          m = 3
       case ( 'trapezoid' )
          m = 4
       case default
          m = 0
       end select
    else
       m = 0
    end if

    h = (b - a) / n

    allocate(xpoints(0:2*n), fpoints(0:2*n))

    xpoints = (/ (a + h*i/2, i = 0,2*n) /)

    fpoints = f(xpoints)
    fleft  => fpoints(0 : 2*n-2 : 2)
    fmid   => fpoints(1 : 2*n-1 : 2)
    fright => fpoints(2 : 2*n   : 2)

    select case (m)
    case (0) ! simpson
       integrate = h / 6.0 * sum(fleft + fright + 4.0*fmid)
    case (1) ! leftrect
       integrate = h * sum(fleft)
    case (2) ! midrect
       integrate = h * sum(fmid)
    case (3) ! rightrect
       integrate = h * sum(fright)
    case (4) ! trapezoid
       integrate = h * sum(fleft + fright) / 2
    end select

    deallocate(xpoints, fpoints)
  end function integrate

end module Integration
```


Usage example:

```fortran
program IntegrationTest
  use Integration
  use FunctionHolder
  implicit none

  print *, integrate(afun, 0., 3**(1/3.), method='simpson')
  print *, integrate(afun, 0., 3**(1/3.), method='leftrect')
  print *, integrate(afun, 0., 3**(1/3.), method='midrect')  
  print *, integrate(afun, 0., 3**(1/3.), method='rightrect')
  print *, integrate(afun, 0., 3**(1/3.), method='trapezoid')

end program IntegrationTest
```


The FunctionHolder module:


```fortran
module FunctionHolder
  implicit none
  
contains

  pure function afun(x)
    real :: afun
    real, intent(in) :: x

    afun = x**2
  end function afun
  
end module FunctionHolder
```


## FreeBASIC

Based on the BASIC entry and the BBC BASIC entry

```freebasic
' version 17-09-2015
' compile with: fbc -s console

#Define screen_width 1024
#Define screen_height 256
ScreenRes screen_width, screen_height, 8
Width screen_width\8, screen_height\16

Function f1(x As Double) As Double
    Return x^3
End Function

Function f2(x As Double) As Double
    Return 1/x
End Function

Function f3(x As Double) As Double
    Return x
End Function

Function leftrect(a As Double, b As Double, n As Double, _
ByVal f As Function (ByVal As Double) As Double) As Double

    Dim As Double sum, x = a, h = (b - a) / n

    For i As UInteger = 1 To n
        sum = sum + h * f(x)
        x = x + h
    Next

    leftrect = sum
End Function

Function rightrect(a As Double, b As Double, n As Double, _
ByVal f As Function (ByVal As Double) As Double) As Double

    Dim As Double sum, x = a, h = (b - a) / n

    For i As UInteger = 1 To n
        x = x + h
        sum = sum + h * f(x)
    Next

    rightrect = sum
End Function

Function midrect(a As Double, b As Double, n As Double, _
ByVal f As Function (ByVal As Double) As Double) As Double

    Dim As Double sum, h = (b - a) / n, x = a + h / 2

    For i As UInteger = 1 To n
        sum = sum + h * f(x)
        x = x + h
    Next

    midrect = sum
End Function

Function trap(a As Double, b As Double, n As Double, _
ByVal f As Function (ByVal As Double) As Double) As Double

    Dim As Double x = a, h = (b - a) / n
    Dim As Double sum = h * (f(a) + f(b)) / 2

    For i As UInteger = 1 To n -1
        x = x + h
        sum = sum + h * f(x)
    Next

    trap = sum
End Function

Function simpson(a As Double, b As Double, n As Double, _
ByVal f As Function (ByVal As Double) As Double) As Double

    Dim As UInteger i
    Dim As Double sum1, sum2
    Dim As Double h = (b - a) / n

    For i = 0 To n -1
        sum1 = sum1 + f(a + h * i + h / 2)
    Next i

    For i = 1 To n -1
        sum2 = sum2 + f(a + h * i)
    Next i

    simpson = h / 6 * (f(a) + f(b) + 4 * sum1 + 2 * sum2)
End Function

' ------=< main >=------

Dim As Double y
Dim As String frmt = " ##.##########"

Print
Print "function     range       steps  leftrect      midrect       " + _
                              "rightrect     trap          simpson "

Print "f(x) = x^3   0 - 1         100";
Print Using frmt; leftrect(0, 1, 100, @f1); midrect(0, 1, 100, @f1); _
rightrect(0, 1, 100, @f1); trap(0, 1, 100, @f1); simpson(0, 1, 100, @f1)

Print "f(x) = 1/x   1 - 100      1000";
Print Using frmt; leftrect(1, 100, 1000, @f2); midrect(1, 100, 1000, @f2); _
                    rightrect(1, 100, 1000, @f2); trap(1, 100, 1000, @f2); _
                    simpson(1, 100, 1000, @f2)

frmt = " #########.###"
Print "f(x) = x     0 - 5000  5000000";
Print Using frmt; leftrect(0, 5000, 5000000, @f3); midrect(0, 5000, 5000000, @f3); _
                    rightrect(0, 5000, 5000000, @f3); trap(0, 5000, 5000000, @f3); _
                    simpson(0, 5000, 5000000, @f3)

Print "f(x) = x     0 - 6000  6000000";
Print Using frmt; leftrect(0, 6000, 6000000, @f3); midrect(0, 6000, 6000000, @f3); _
                    rightrect(0, 6000, 6000000, @f3); trap(0, 6000, 6000000, @f3); _
                    simpson(0, 6000, 6000000, @f3)

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
function   range      steps  leftrect      midrect       rightrect     trap          simpson 
f(x) = x^3 0 - 1        100  0.2450250000  0.2499875000  0.2550250000  0.2500250000  0.2500000000
f(x) = 1/x 1 - 100     1000  4.6549910575  4.6047625487  4.5569810575  4.6059860575  4.6051703850
f(x) = x   0 - 5000 5000000  12499997.501  12500000.001  12500002.501  12500000.001  12500000.000
f(x) = x   0 - 6000 6000000  17999997.001  18000000.001  18000003.001  18000000.001  18000000.000
```



## Go


```go
package main

import (
    "fmt"
    "math"
)

// specification for an integration
type spec struct {
    lower, upper float64               // bounds for integration
    n            int                   // number of parts
    exact        float64               // expected answer
    fs           string                // mathematical description of function
    f            func(float64) float64 // function to integrate
}

// test cases per task description
var data = []spec{
    spec{0, 1, 100, .25, "x^3", func(x float64) float64 { return x * x * x }},
    spec{1, 100, 1000, float64(math.Log(100)), "1/x",
        func(x float64) float64 { return 1 / x }},
    spec{0, 5000, 5e5, 12.5e6, "x", func(x float64) float64 { return x }},
    spec{0, 6000, 6e6, 18e6, "x", func(x float64) float64 { return x }},
}

// object for associating a printable function name with an integration method
type method struct {
    name      string
    integrate func(spec) float64
}

// integration methods implemented per task description
var methods = []method{
    method{"Rectangular (left)    ", rectLeft},
    method{"Rectangular (right)   ", rectRight},
    method{"Rectangular (midpoint)", rectMid},
    method{"Trapezium             ", trap},
    method{"Simpson's             ", simpson},
}

func rectLeft(t spec) float64 {
    var a adder
    r := t.upper - t.lower
    nf := float64(t.n)
    x0 := t.lower
    for i := 0; i < t.n; i++ {
        x1 := t.lower + float64(i+1)*r/nf
        // x1-x0 better than r/nf.
        // (with r/nf, the represenation error accumulates)
        a.add(t.f(x0) * (x1 - x0))
        x0 = x1
    }
    return a.total()
}

func rectRight(t spec) float64 {
    var a adder
    r := t.upper - t.lower
    nf := float64(t.n)
    x0 := t.lower
    for i := 0; i < t.n; i++ {
        x1 := t.lower + float64(i+1)*r/nf
        a.add(t.f(x1) * (x1 - x0))
        x0 = x1
    }
    return a.total()
}

func rectMid(t spec) float64 {
    var a adder
    r := t.upper - t.lower
    nf := float64(t.n)
    // there's a tiny gloss in the x1-x0 trick here.  the correct way
    // would be to compute x's at division boundaries, but we don't need
    // those x's for anything else.  (the function is evaluated on x's
    // at division midpoints rather than division boundaries.)  so, we
    // reuse the midpoint x's, knowing that they will average out just
    // as well.  we just need one extra point, so we use lower-.5.
    x0 := t.lower - .5*r/nf
    for i := 0; i < t.n; i++ {
        x1 := t.lower + (float64(i)+.5)*r/nf
        a.add(t.f(x1) * (x1 - x0))
        x0 = x1
    }
    return a.total()
}

func trap(t spec) float64 {
    var a adder
    r := t.upper - t.lower
    nf := float64(t.n)
    x0 := t.lower
    f0 := t.f(x0)
    for i := 0; i < t.n; i++ {
        x1 := t.lower + float64(i+1)*r/nf
        f1 := t.f(x1)
        a.add((f0 + f1) * .5 * (x1 - x0))
        x0, f0 = x1, f1
    }
    return a.total()
}

func simpson(t spec) float64 {
    var a adder
    r := t.upper - t.lower
    nf := float64(t.n)
    // similar to the rectangle midpoint logic explained above,
    // we play a little loose with the values used for dx and dx0.
    dx0 := r / nf
    a.add(t.f(t.lower) * dx0)
    a.add(t.f(t.lower+dx0*.5) * dx0 * 4)
    x0 := t.lower + dx0
    for i := 1; i < t.n; i++ {
        x1 := t.lower + float64(i+1)*r/nf
        xmid := (x0 + x1) * .5
        dx := x1 - x0
        a.add(t.f(x0) * dx * 2)
        a.add(t.f(xmid) * dx * 4)
        x0 = x1
    }
    a.add(t.f(t.upper) * dx0)
    return a.total() / 6
}

func sum(v []float64) float64 {
    var a adder
    for _, e := range v {
        a.add(e)
    }
    return a.total()
}

type adder struct {
    sum, e float64
}

func (a *adder) total() float64 {
    return a.sum + a.e
}

func (a *adder) add(x float64) {
    sum := a.sum + x
    e := sum - a.sum
    a.e += a.sum - (sum - e) + (x - e)
    a.sum = sum
}

func main() {
    for _, t := range data {
        fmt.Println("Test case: f(x) =", t.fs)
        fmt.Println("Integration from", t.lower, "to", t.upper,
            "in", t.n, "parts")
        fmt.Printf("Exact result            %.7e     Error\n", t.exact)
        for _, m := range methods {
            a := m.integrate(t)
            e := a - t.exact
            if e < 0 {
                e = -e
            }
            fmt.Printf("%s  %.7e  %.7e\n", m.name, a, e)
        }
        fmt.Println("")
    }
}
```

{{out}}

```txt

Integration from 0 to 1 in 100 parts
Exact result            2.5000000e-01     Error
Rectangular (left)      2.4502500e-01  4.9750000e-03
Rectangular (right)     2.5502500e-01  5.0250000e-03
Rectangular (midpoint)  2.4998750e-01  1.2500000e-05
Trapezium               2.5002500e-01  2.5000000e-05
Simpson's               2.5000000e-01  0.0000000e+00

Test case: f(x) = 1/x
Integration from 1 to 100 in 1000 parts
Exact result            4.6051702e+00     Error
Rectangular (left)      4.6549911e+00  4.9820872e-02
Rectangular (right)     4.5569811e+00  4.8189128e-02
Rectangular (midpoint)  4.6047625e+00  4.0763731e-04
Trapezium               4.6059861e+00  8.1587153e-04
Simpson's               4.6051704e+00  1.9896905e-07

Test case: f(x) = x
Integration from 0 to 5000 in 500000 parts
Exact result            1.2500000e+07     Error
Rectangular (left)      1.2499975e+07  2.5000000e+01
Rectangular (right)     1.2500025e+07  2.5000000e+01
Rectangular (midpoint)  1.2500000e+07  0.0000000e+00
Trapezium               1.2500000e+07  0.0000000e+00
Simpson's               1.2500000e+07  0.0000000e+00

Test case: f(x) = x
Integration from 0 to 6000 in 6000000 parts
Exact result            1.8000000e+07     Error
Rectangular (left)      1.7999997e+07  3.0000000e+00
Rectangular (right)     1.8000003e+07  3.0000000e+00
Rectangular (midpoint)  1.8000000e+07  0.0000000e+00
Trapezium               1.8000000e+07  0.0000000e+00
Simpson's               1.8000000e+07  0.0000000e+00

```



## Groovy

Solution:

```groovy
def assertBounds = { List bounds, int nRect ->
    assert (bounds.size() == 2) && (bounds[0] instanceof Double) && (bounds[1] instanceof Double) && (nRect > 0)
}

def integral = { List bounds, int nRectangles, Closure f, List pointGuide, Closure integralCalculator->
    double a = bounds[0], b = bounds[1], h = (b - a)/nRectangles
    def xPoints = pointGuide.collect { double it -> a + it*h }
    def fPoints = xPoints.collect { x -> f(x) }
    integralCalculator(h, fPoints)
}

def leftRectIntegral = { List bounds, int nRect, Closure f ->
    assertBounds(bounds, nRect)
    integral(bounds, nRect, f, (0..<nRect)) { h, fPoints -> h*fPoints.sum() }
}

def rightRectIntegral = { List bounds, int nRect, Closure f ->
    assertBounds(bounds, nRect)
    integral(bounds, nRect, f, (1..nRect)) { h, fPoints -> h*fPoints.sum() }
}

def midRectIntegral = { List bounds, int nRect, Closure f ->
    assertBounds(bounds, nRect)
    integral(bounds, nRect, f, ((0.5d)..nRect)) { h, fPoints -> h*fPoints.sum() }
}

def trapezoidIntegral = { List bounds, int nRect, Closure f ->
    assertBounds(bounds, nRect)
    integral(bounds, nRect, f, (0..nRect)) { h, fPoints ->
        def fLeft  = fPoints[0..<nRect]
        def fRight = fPoints[1..nRect]
        h/2*(fLeft + fRight).sum()
    }
}

def simpsonsIntegral = { List bounds, int nSimpRect, Closure f ->
    assertBounds(bounds, nSimpRect)
    integral(bounds, nSimpRect*2, f, (0..(nSimpRect*2))) { h, fPoints ->
        def fLeft  = fPoints[(0..<nSimpRect*2).step(2)]
        def fMid   = fPoints[(1..<nSimpRect*2).step(2)]
        def fRight = fPoints[(2..nSimpRect*2).step(2)]
        h/3*((fLeft + fRight).sum() + 4*(fMid.sum()))
    }
}
```


Test:

Each "nRect" (number of rectangles) value given below is the minimum value that meets the tolerance condition for the given circumstances (function-to-integrate, integral-type and integral-bounds).

```groovy
double tolerance = 0.0001 // allowable "wrongness", ensures accuracy to 1 in 10,000

double sinIntegralCalculated = -(Math.cos(Math.PI) - Math.cos(0d))
assert  (leftRectIntegral([0d, Math.PI], 129, Math.&sin) - sinIntegralCalculated).abs() < tolerance
assert (rightRectIntegral([0d, Math.PI], 129, Math.&sin) - sinIntegralCalculated).abs() < tolerance
assert   (midRectIntegral([0d, Math.PI],  91, Math.&sin) - sinIntegralCalculated).abs() < tolerance
assert (trapezoidIntegral([0d, Math.PI], 129, Math.&sin) - sinIntegralCalculated).abs() < tolerance
assert  (simpsonsIntegral([0d, Math.PI],   6, Math.&sin) - sinIntegralCalculated).abs() < tolerance

double cubeIntegralCalculated = 1d/4d *(10d**4 - 0d**4)
assert  ((leftRectIntegral([0d, 10d], 20000) { it**3 } - cubeIntegralCalculated)/cubeIntegralCalculated).abs() < tolerance
assert ((rightRectIntegral([0d, 10d], 20001) { it**3 } - cubeIntegralCalculated)/cubeIntegralCalculated).abs() < tolerance
assert   ((midRectIntegral([0d, 10d],    71) { it**3 } - cubeIntegralCalculated)/cubeIntegralCalculated).abs() < tolerance
assert ((trapezoidIntegral([0d, 10d],   101) { it**3 } - cubeIntegralCalculated)/cubeIntegralCalculated).abs() < tolerance
// I can name that tune in one note!
assert  (simpsonsIntegral([0d,         10d], 1) { it**3 } == cubeIntegralCalculated)
assert  (simpsonsIntegral([0d,     Math.PI], 1) { it**3 } == (1d/4d *(Math.PI**4 - 0d**4)))
assert  (simpsonsIntegral([-7.23d, Math.PI], 1) { it**3 } == (1d/4d *(Math.PI**4 - (-7.23d)**4)))

double quarticIntegralCalculated = 1d/5d *(10d**5 - 0d**5)
assert  ((leftRectIntegral([0d, 10d], 25000) { it**4 } - quarticIntegralCalculated)/quarticIntegralCalculated).abs() < tolerance
assert ((rightRectIntegral([0d, 10d], 25001) { it**4 } - quarticIntegralCalculated)/quarticIntegralCalculated).abs() < tolerance
assert   ((midRectIntegral([0d, 10d],    92) { it**4 } - quarticIntegralCalculated)/quarticIntegralCalculated).abs() < tolerance
assert ((trapezoidIntegral([0d, 10d],   130) { it**4 } - quarticIntegralCalculated)/quarticIntegralCalculated).abs() < tolerance
assert  ((simpsonsIntegral([0d, 10d],     5) { it**4 } - quarticIntegralCalculated)/quarticIntegralCalculated).abs() < tolerance

def cubicPoly = { it**3 + 2*it**2 + 7*it + 12d }
def cubicPolyAntiDeriv = { 1/4*it**4 + 2/3*it**3 + 7/2*it**2 + 12*it }
double cubicPolyIntegralCalculated = (cubicPolyAntiDeriv(10d) - cubicPolyAntiDeriv(0d))
assert  ((leftRectIntegral([0d, 10d], 20000, cubicPoly) - cubicPolyIntegralCalculated)/cubicPolyIntegralCalculated).abs() < tolerance
assert ((rightRectIntegral([0d, 10d], 20001, cubicPoly) - cubicPolyIntegralCalculated)/cubicPolyIntegralCalculated).abs() < tolerance
assert   ((midRectIntegral([0d, 10d],    71, cubicPoly) - cubicPolyIntegralCalculated)/cubicPolyIntegralCalculated).abs() < tolerance
assert ((trapezoidIntegral([0d, 10d],   101, cubicPoly) - cubicPolyIntegralCalculated)/cubicPolyIntegralCalculated).abs() < tolerance
// I can name that tune in one note!
assert  ((simpsonsIntegral([0d, 10d],     1, cubicPoly) - cubicPolyIntegralCalculated)/cubicPolyIntegralCalculated).abs() < tolerance**2.75 // 1 in 100 billion

double cpIntegralCalc0ToPI = (cubicPolyAntiDeriv(Math.PI) - cubicPolyAntiDeriv(0d))
assert  ((simpsonsIntegral([0d, Math.PI], 1, cubicPoly) -         cpIntegralCalc0ToPI)/        cpIntegralCalc0ToPI).abs() < tolerance**2.75 // 1 in 100 billion
double cpIntegralCalcMinusEToPI = (cubicPolyAntiDeriv(Math.PI) - cubicPolyAntiDeriv(-Math.E))
assert  ((simpsonsIntegral([-Math.E, Math.PI], 1, cubicPoly) - cpIntegralCalcMinusEToPI)/ cpIntegralCalcMinusEToPI).abs() < tolerance**2.5  // 1 in 10 billion
```


Requested Demonstrations:

```groovy
println "f(x) = x**3, where x is [0,1], with 100 approximations. The exact result is 1/4, or 0.25."
println ([" LeftRect":  leftRectIntegral([0d, 1d], 100) { it**3 }])
println (["RightRect": rightRectIntegral([0d, 1d], 100) { it**3 }])
println (["  MidRect":   midRectIntegral([0d, 1d], 100) { it**3 }])
println (["Trapezoid": trapezoidIntegral([0d, 1d], 100) { it**3 }])
println ([" Simpsons":  simpsonsIntegral([0d, 1d], 100) { it**3 }])
println ()

println "f(x) = 1/x, where x is [1, 100], with 1,000 approximations. The exact result is the natural log of 100, or about 4.605170."
println ([" LeftRect":  leftRectIntegral([1d, 100d], 1000) { 1/it }])
println (["RightRect": rightRectIntegral([1d, 100d], 1000) { 1/it }])
println (["  MidRect":   midRectIntegral([1d, 100d], 1000) { 1/it }])
println (["Trapezoid": trapezoidIntegral([1d, 100d], 1000) { 1/it }])
println ([" Simpsons":  simpsonsIntegral([1d, 100d], 1000) { 1/it }])
println ()

println "f(x) = x, where x is [0,5000], with 5,000,000 approximations. The exact result is 12,500,000."
println ([" LeftRect":  leftRectIntegral([0d, 5000d], 5000000) { it }])
println (["RightRect": rightRectIntegral([0d, 5000d], 5000000) { it }])
println (["  MidRect":   midRectIntegral([0d, 5000d], 5000000) { it }])
println (["Trapezoid": trapezoidIntegral([0d, 5000d], 5000000) { it }])
println ([" Simpsons":  simpsonsIntegral([0d, 5000d], 5000000) { it }])
println ()

println "f(x) = x, where x is [0,6000], with 6,000,000 approximations. The exact result is 18,000,000."
println ([" LeftRect":  leftRectIntegral([0d, 6000d], 6000000) { it }])
println (["RightRect": rightRectIntegral([0d, 6000d], 6000000) { it }])
println (["  MidRect":   midRectIntegral([0d, 6000d], 6000000) { it }])
println (["Trapezoid": trapezoidIntegral([0d, 6000d], 6000000) { it }])
println ([" Simpsons":  simpsonsIntegral([0d, 6000d], 6000000) { it }])
println ()
```


Output:

```txt
f(x) = x**3, where x is [0,1], with 100 approximations. The exact result is 1/4, or 0.25.
[ LeftRect:0.24502500000000002]
[RightRect:0.255025]
[  MidRect:0.24998750000000008]
[Trapezoid:0.250025]
[ Simpsons:0.25000000000000006]

f(x) = 1/x, where x is [1, 100], with 1,000 approximations. The exact result is the natural log of 100, or about 4.605170.
[ LeftRect:4.65499105751468]
[RightRect:4.55698105751468]
[  MidRect:4.604762548678376]
[Trapezoid:4.605986057514673]
[ Simpsons:4.605170384957142]

f(x) = x, where x is [0,5000], with 5,000,000 approximations. The exact result is 12,500,000.
[ LeftRect:1.24999975E7]
[RightRect:1.25000025E7]
[  MidRect:1.25E7]
[Trapezoid:1.25E7]
[ Simpsons:1.25E7]

f(x) = x, where x is [0,6000], with 6,000,000 approximations. The exact result is 18,000,000.
[ LeftRect:1.7999997000000004E7]
[RightRect:1.8000003000000004E7]
[  MidRect:1.7999999999999993E7]
[Trapezoid:1.7999999999999996E7]
[ Simpsons:1.7999999999999993E7]
```



## Haskell


Different approach from most of the other examples: First, the function ''f'' might be expensive to calculate, and so it should not be evaluated several times. So, ideally, we want to have positions ''x'' and weights ''w'' for each method and then just calculate the approximation of the integral by 


```haskell
approx f xs ws = sum [w * f x | (x,w) <- zip xs ws]
```


Second, let's to generalize all integration methods into one scheme. The methods can all be characterized by the coefficients ''vs'' they use in a particular interval. These will be fractions, and for terseness, we extract the denominator as an extra argument ''v''.

Now there are the closed formulas (which include the endpoints) and the open formulas (which exclude them). Let's do the open formulas first, because then the coefficients don't overlap:
 

```haskell>integrateOpen :: Fractional a =
 a -> [a] -> (a -> a) -> a -> a -> Int -> a
integrateOpen v vs f a b n = approx f xs ws * h / v where
  m = fromIntegral (length vs) * n
  h = (b-a) / fromIntegral m
  ws = concat $ replicate n vs
  c = a + h/2
  xs = [c + h * fromIntegral i | i <- [0..m-1]]
```


Similarly for the closed formulas, but we need an additional function ''overlap'' which sums the coefficients overlapping at the interior interval boundaries:
 

```haskell>integrateClosed :: Fractional a =
 a -> [a] -> (a -> a) -> a -> a -> Int -> a
integrateClosed v vs f a b n = approx f xs ws * h / v where
  m = fromIntegral (length vs - 1) * n
  h = (b-a) / fromIntegral m
  ws = overlap n vs
  xs = [a + h * fromIntegral i | i <- [0..m]]

overlap :: Num a => Int -> [a] -> [a]
overlap n []  = []
overlap n (x:xs) = x : inter n xs where
  inter 1 ys     = ys
  inter n []     = x : inter (n-1) xs
  inter n [y]    = (x+y) : inter (n-1) xs
  inter n (y:ys) = y : inter n ys
```


And now we can just define


```haskell
intLeftRect  = integrateClosed  1 [1,0]     
intRightRect = integrateClosed  1 [0,1]     
intMidRect   = integrateOpen    1 [1]  
intTrapezium = integrateClosed  2 [1,1]   
intSimpson   = integrateClosed  3 [1,4,1]
```


or, as easily, some additional schemes:


```haskell
intMilne     = integrateClosed 45 [14,64,24,64,14]
intOpen1     = integrateOpen    2 [3,3]
intOpen2     = integrateOpen    3 [8,-4,8]
```


Some examples:

 *Main> intLeftRect  (\x -> x*x) 0 1 10  
 0.2850000000000001
 *Main> intRightRect (\x -> x*x) 0 1 10  
 0.38500000000000006
 *Main> intMidRect   (\x -> x*x) 0 1 10 
 0.3325
 *Main> intTrapezium (\x -> x*x) 0 1 10 
 0.3350000000000001
 *Main> intSimpson   (\x -> x*x) 0 1 10 
 0.3333333333333334

The whole program:


```haskell
approx
  :: Fractional a
  => (a1 -> a) -> [a1] -> [a] -> a
approx f xs ws =
  sum
    [ w * f x
    | (x, w) <- zip xs ws ]

integrateOpen
  :: Fractional a
  => a -> [a] -> (a -> a) -> a -> a -> Int -> a
integrateOpen v vs f a b n = approx f xs ws * h / v
  where
    m = fromIntegral (length vs) * n
    h = (b - a) / fromIntegral m
    ws = concat $ replicate n vs
    c = a + h / 2
    xs =
      [ c + h * fromIntegral i
      | i <- [0 .. m - 1] ]

integrateClosed
  :: Fractional a
  => a -> [a] -> (a -> a) -> a -> a -> Int -> a
integrateClosed v vs f a b n = approx f xs ws * h / v
  where
    m = fromIntegral (length vs - 1) * n
    h = (b - a) / fromIntegral m
    ws = overlap n vs
    xs =
      [ a + h * fromIntegral i
      | i <- [0 .. m] ]

overlap
  :: Num a
  => Int -> [a] -> [a]
overlap n [] = []
overlap n (x:xs) = x : inter n xs
  where
    inter 1 ys = ys
    inter n [] = x : inter (n - 1) xs
    inter n [y] = (x + y) : inter (n - 1) xs
    inter n (y:ys) = y : inter n ys

uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t) -> (t1, t2, t3, t4) -> t
uncurry4 f ~(a, b, c, d) = f a b c d

-- TEST ----------------------------------------------------------------------
ms
  :: Fractional a
  => [(String, (a -> a) -> a -> a -> Int -> a)]
ms =
  [ ("rectangular left", integrateClosed 1 [1, 0])
  , ("rectangular middle", integrateOpen 1 [1])
  , ("rectangular right", integrateClosed 1 [0, 1])
  , ("trapezium", integrateClosed 2 [1, 1])
  , ("simpson", integrateClosed 3 [1, 4, 1])
  ]

integrations
  :: (Fractional a, Num t, Num t1, Num t2)
  => [(String, (a -> a, t, t1, t2))]
integrations =
  [ ("x^3", ((^ 3), 0, 1, 100))
  , ("1/x", ((1 /), 1, 100, 1000))
  , ("x", (id, 0, 5000, 500000))
  , ("x", (id, 0, 6000, 600000))
  ]

main :: IO ()
main =
  mapM_
    (\(s, e@(_, a, b, n)) -> do
       putStrLn
         (concat
            [ indent 20 ("f(x) = " ++ s)
            , show [a, b]
            , "  ("
            , show n
            , " approximations)"
            ])
       mapM_
         (\(s, integration) ->
             putStrLn (indent 20 (s ++ ":") ++ show (uncurry4 integration e)))
         ms
       putStrLn [])
    integrations
  where
    indent n = take n . (++ replicate n ' ')
```

{{Out}}

```txt
f(x) = x^3          [0.0,1.0]  (100 approximations)
rectangular left:   0.24502500000000005
rectangular middle: 0.24998750000000006
rectangular right:  0.25502500000000006
trapezium:          0.25002500000000005
simpson:            0.25000000000000006

f(x) = 1/x          [1.0,100.0]  (1000 approximations)
rectangular left:   4.65499105751468
rectangular middle: 4.604762548678376
rectangular right:  4.55698105751468
trapezium:          4.605986057514681
simpson:            4.605170384957135

f(x) = x            [0.0,5000.0]  (500000 approximations)
rectangular left:   1.2499975000000006e7
rectangular middle: 1.2499999999999993e7
rectangular right:  1.2500025000000006e7
trapezium:          1.2500000000000006e7
simpson:            1.2499999999999998e7

f(x) = x            [0.0,6000.0]  (600000 approximations)
rectangular left:   1.7999970000000004e7
rectangular middle: 1.7999999999999993e7
rectangular right:  1.8000030000000004e7
trapezium:          1.8000000000000004e7
simpson:            1.7999999999999996e7
```

Runtime: about 7 seconds.


## J


### Solution:


```j
integrate=: adverb define
  'a b steps'=. 3{.y,128
  size=. (b - a)%steps
  size * +/ u |: 2 ]\ a + size * i.>:steps
)

rectangle=: adverb def 'u -: +/ y'

trapezium=: adverb def '-: +/ u y'

simpson  =: adverb def '6 %~ +/ 1 1 4 * u y, -:+/y'
```


### Example usage


### =Required Examples=


```j
   Ir=: rectangle integrate
   It=: trapezium integrate
   Is=: simpson integrate
   
   ^&3 Ir 0 1 100
0.249987
   ^&3 It 0 1 100
0.250025
   ^&3 Is 0 1 100
0.25
   % Ir 1 100 1000
4.60476
   % It 1 100 1000
4.60599
   % Is 1 100 1000
4.60517
   ] Ir 0 5000 5e6
1.25e7
   ] It 0 5000 5e6
1.25e7
   ] Is 0 5000 5e6
1.25e7
   ] Ir 0 6000 6e6
1.8e7
   ] It 0 6000 6e6
1.8e7
   ] Is 0 6000 6e6
1.8e7
```


### =Older Examples=

Integrate <code>square</code> (<code>*:</code>) from 0 to &pi; in 10 steps using various methods.

```j
   *: rectangle integrate 0 1p1 10        
10.3095869962
   *: trapezium integrate 0 1p1 10
10.3871026879
   *: simpson integrate 0 1p1 10
10.3354255601
```

Integrate <code>sin</code> from 0 to &pi; in 10 steps using various methods.

```j
   sin=: 1&o.
   sin rectangle integrate 0 1p1 10
2.00824840791
   sin trapezium integrate 0 1p1 10
1.98352353751
   sin simpson integrate 0 1p1 10
2.00000678444
```


### Aside

Note that J has a primitive verb <code>p..</code> for integrating polynomials. For example the integral of <math>x^2</math> (which can be described in terms of its coefficients as <code>0 0 1</code>) is:

```j
   0 p.. 0 0 1
0 0 0 0.333333333333
   0 p.. 0 0 1x        NB. or using rationals
0 0 0 1r3
```

That is: <math>0x^0 + 0x^1 + 0x^2 + \tfrac{1}{3}x^3</math>

So to integrate <math>x^2</math> from 0 to &pi; :

```j
   0 0 1 (0&p..@[ -~/@:p. ]) 0 1p1 
10.3354255601
```


That said, J also has <code>d.</code> which can [http://www.jsoftware.com/help/dictionary/dddot.htm integrate] suitable functions.


```j
   *:d._1]1p1
10.3354
```



## Java


```java5
class NumericalIntegration
{

  interface FPFunction
  {
    double eval(double n);
  }
  
  public static double rectangularLeft(double a, double b, int n, FPFunction f)
  {
    return rectangular(a, b, n, f, 0);
  }
  
  public static double rectangularMidpoint(double a, double b, int n, FPFunction f)
  {
    return rectangular(a, b, n, f, 1);
  }
  
  public static double rectangularRight(double a, double b, int n, FPFunction f)
  {
    return rectangular(a, b, n, f, 2);
  }
  
  public static double trapezium(double a, double b, int n, FPFunction f)
  {
    double range = checkParamsGetRange(a, b, n);
    double nFloat = (double)n;
    double sum = 0.0;
    for (int i = 1; i < n; i++)
    {
      double x = a + range * (double)i / nFloat;
      sum += f.eval(x);
    }
    sum += (f.eval(a) + f.eval(b)) / 2.0;
    return sum * range / nFloat;
  }
  
  public static double simpsons(double a, double b, int n, FPFunction f)
  {
    double range = checkParamsGetRange(a, b, n);
    double nFloat = (double)n;
    double sum1 = f.eval(a + range / (nFloat * 2.0));
    double sum2 = 0.0;
    for (int i = 1; i < n; i++)
    {
      double x1 = a + range * ((double)i + 0.5) / nFloat;
      sum1 += f.eval(x1);
      double x2 = a + range * (double)i / nFloat;
      sum2 += f.eval(x2);
    }
    return (f.eval(a) + f.eval(b) + sum1 * 4.0 + sum2 * 2.0) * range / (nFloat * 6.0);
  }
  
  private static double rectangular(double a, double b, int n, FPFunction f, int mode)
  {
    double range = checkParamsGetRange(a, b, n);
    double modeOffset = (double)mode / 2.0;
    double nFloat = (double)n;
    double sum = 0.0;
    for (int i = 0; i < n; i++)
    {
      double x = a + range * ((double)i + modeOffset) / nFloat;
      sum += f.eval(x);
    }
    return sum * range / nFloat;
  }
  
  private static double checkParamsGetRange(double a, double b, int n)
  {
    if (n <= 0)
      throw new IllegalArgumentException("Invalid value of n");
    double range = b - a;
    if (range <= 0)
      throw new IllegalArgumentException("Invalid range");
    return range;
  }
  
  
  private static void testFunction(String fname, double a, double b, int n, FPFunction f)
  {
    System.out.println("Testing function \"" + fname + "\", a=" + a + ", b=" + b + ", n=" + n);
    System.out.println("rectangularLeft: " + rectangularLeft(a, b, n, f));
    System.out.println("rectangularMidpoint: " + rectangularMidpoint(a, b, n, f));
    System.out.println("rectangularRight: " + rectangularRight(a, b, n, f));
    System.out.println("trapezium: " + trapezium(a, b, n, f));
    System.out.println("simpsons: " + simpsons(a, b, n, f));
    System.out.println();
    return;
  }
  
  public static void main(String[] args)
  {
    testFunction("x^3", 0.0, 1.0, 100, new FPFunction() {
        public double eval(double n) {
          return n * n * n;
        }
      }
    );
    
    testFunction("1/x", 1.0, 100.0, 1000, new FPFunction() {
        public double eval(double n) {
          return 1.0 / n;
        }
      }
    );
    
    testFunction("x", 0.0, 5000.0, 5000000, new FPFunction() {
        public double eval(double n) {
          return n;
        }
      }
    );
    
    testFunction("x", 0.0, 6000.0, 6000000, new FPFunction() {
        public double eval(double n) {
          return n;
        }
      }
    );
    
    return;
  }
}

```



## Julia

{{works with|Julia|0.6}}


```julia
function simpson(f::Function, a::Number, b::Number, n::Integer)
    h = (b - a) / n
    s = f(a + h / 2)
    for i in 1:(n-1)
        s += f(a + h * i + h / 2) + f(a + h * i) / 2
    end
    return h/6 * (f(a) + f(b) + 4*s)
end

rst =
    simpson(x -> x ^ 3, 0, 1, 100),
    simpson(x -> 1 / x, 1, 100, 1000),
    simpson(x -> x, 0, 5000, 5_000_000),
    simpson(x -> x, 0, 6000, 6_000_000)

@show rst
```


{{out}}

```txt
rst = (0.25000000000000006, 4.605170384957135, 1.25e7, 1.8e7)
```



## Kotlin


```scala
// version 1.1.2

typealias Func = (Double) -> Double

fun integrate(a: Double, b: Double, n: Int, f: Func) {
    val h = (b - a) / n
    val sum = DoubleArray(5)
    for (i in 0 until n) {
        val x = a + i * h
        sum[0] += f(x)
        sum[1] += f(x + h / 2.0)
        sum[2] += f(x + h)
        sum[3] += (f(x) + f(x + h)) / 2.0
        sum[4] += (f(x) + 4.0 * f(x + h / 2.0) + f(x + h)) / 6.0
    }
    val methods = listOf("LeftRect ", "MidRect  ", "RightRect", "Trapezium", "Simpson  ")
    for (i in 0..4) println("${methods[i]} = ${"%f".format(sum[i] * h)}")
    println()
}

fun main(args: Array<String>) {
    integrate(0.0, 1.0, 100) { it * it * it }
    integrate(1.0, 100.0, 1_000) { 1.0 / it }
    integrate(0.0, 5000.0, 5_000_000) { it }
    integrate(0.0, 6000.0, 6_000_000) { it }
}
```


{{out}}

```txt

LeftRect  = 0.245025
MidRect   = 0.249988
RightRect = 0.255025
Trapezium = 0.250025
Simpson   = 0.250000

LeftRect  = 4.654991
MidRect   = 4.604763
RightRect = 4.556981
Trapezium = 4.605986
Simpson   = 4.605170

LeftRect  = 12499997.500000
MidRect   = 12500000.000000
RightRect = 12500002.500000
Trapezium = 12500000.000000
Simpson   = 12500000.000000

LeftRect  = 17999997.000000
MidRect   = 18000000.000000
RightRect = 18000003.000000
Trapezium = 18000000.000000
Simpson   = 18000000.000000

```



## Liberty BASIC

Running the big loop value would take a VERY long time & seems unnecessary.
```lb

while 1
    read x$
    if x$ ="end" then print "**Over**": end

    read a, b, N, knownValue

    print " Function y ="; x$; " from "; a; " to "; b; " in "; N; " steps"
    print " Known exact value ="; knownValue

    areaLR = IntegralByLeftRectangle(   x$, a, b, N)
    areaRR = IntegralByRightRectangle(  x$, a, b, N)
    areaMR = IntegralByMiddleRectangle( x$, a, b, N)
    areaTr = IntegralByTrapezium(       x$, a, b, N)
    areaSi = IntegralBySimpsonRule(     x$, a, b, N)

    print "Left rectangle method   "; using( "##########.##########", areaLR); " diff "; knownValue-areaLR; tab(70); (knownValue-areaLR)/knownValue*100;" %"
    print "Right rectangle method  "; using( "##########.##########", areaRR); " diff "; knownValue-areaRR; tab(70); (knownValue-areaRR)/knownValue*100;" %"
    print "Middle rectangle method "; using( "##########.##########", areaMR); " diff "; knownValue-areaMR; tab(70); (knownValue-areaMR)/knownValue*100;" %"
    print "Trapezium  method       "; using( "##########.##########", areaTr); " diff "; knownValue-areaTr; tab(70); (knownValue-areaTr)/knownValue*100;" %"
    print "Simpson's Rule          "; using( "##########.##########", areaSi); " diff "; knownValue-areaSi; tab(70); (knownValue-areaSi)/knownValue*100;" %"

    print

wend

end

'------------------------------------------------------
    'we have N sizes, that gives us N+1 points
    'point 0 is a
    'point N is b
    'point i is xi =a +i *h
    'Often, precision is (sharper?) then single step area
    'So there should be EXACT number of steps, hence loop by integer i.

function IntegralByLeftRectangle( x$, a, b, N)
    h = ( b -a) /N
    s = 0
    for i = 0 to N -1
        x = a +i *h
        s = s + h *eval( x$)
    next
    IntegralByLeftRectangle = s
end function

function IntegralByRightRectangle( x$, a, b, N)
    h =( b -a) /N
    s = 0
    for i =1 to N
        x = a +i *h
        s = s + h *eval( x$)
    next
    IntegralByRightRectangle = s
end function

function IntegralByMiddleRectangle( x$, a, b, N)
    h =( b -a) /N
    s = 0
    for i =0 to N -1
        x = a +i *h +h /2
        s = s + h *eval( x$)
    next
    IntegralByMiddleRectangle = s
end function

function IntegralByTrapezium( x$, a, b, N)
'Formula is h*((f(a)+f(b))/2 + sum_{i=1}^{N-1} (f(x_i)))
    h  =( b -a) /N
    x  = a
    fa =eval( x$)
    x  =b
    fb =eval( x$)
    s = h *( fa +fb) /2
    for i =1 to N -1
        x = a +i *h
        s = s + h *eval( x$)
    next
    IntegralByTrapezium = s
end function

function IntegralBySimpsonRule( x$, a, b, N)
    'Simpson
    'N should be even.
    if N mod 2 then N =N +1
    'It really doesn't look right to double number of points from N to 2N -
    ' - this method is most accurate of all presented!
    'So we use NN as N/2, and N will be 2NN
    'Formula is h/6*( f(a)+f(b) + 4*(f(x_1)+f(x_3)+...+f(x_{2NN-1})+ 2*(f(x_2)+f(x_4)+...+f(x_{2NN-2})) )
    'Somehow I messed up h/6, h/3 and what is h, regarding "n=number of double intervals of size 2h"
    NN =N /2

    h  =( b -a) /N
    x  =a
    fa =eval (x$)
    x  =b
    fb =eval( x$)
    s = h /3 *( fa +fb)
    for i =1 to 2 *NN -1 step 2
        x = a +i *h
        s = s + h /3 *4 *eval( x$)  'odd points
    next
    for i =2 to 2 *NN -2 step 2
        x = a +i *h
        s = s + h /3 *2 *eval( x$)  'even points
    next

    IntegralBySimpsonRule = s
end function

'
### =================================================

data "x^3",  0,    1,     100,          0.25
data "x^-1", 1,  100,    1000,          4.605170
data "x",    0, 5000,    1000,   12500000.0   '   should use 5 000 000 steps
data "x",    0, 6000,    1000,   18000000.0   '   should use 6 000 000 steps
data "end"

end

```


 Numerical integration
 Function y =x^3 from 0 to 1 in 100 steps
 Known exact value =0.25
 Left rectangle method            0.2450250000 diff 0.004975          1.99 %
 Right rectangle method           0.2550250000 diff -0.005025         -2.01 %
 Middle rectangle method          0.2499875000 diff 0.0000125         0.005 %
 Trapezium  method                0.2500250000 diff -0.000025         -0.01 %
 Simpson's Rule                   0.2500000000 diff 0.0               0.0 %

 Function y =x^-1 from 1 to 100 in 1000 steps
 Known exact value =4.60517
 Left rectangle method            4.6549910575 diff -0.49821058e-1    -1.08185056 %
 Right rectangle method           4.5569810575 diff 0.48188942e-1     1.04640963 %
 Middle rectangle method          4.6047625487 diff 0.40745132e-3     0.88476934e-2 %
 Trapezium  method                4.6059860575 diff -0.81605751e-3    -0.17720464e-1 %
 Simpson's Rule                   4.6051733163 diff -0.3316273e-5     -0.72011956e-4 %

 Function y =x from 0 to 5000 in 1000 steps
 Known exact value =12500000
 Left rectangle method     12487500.0000000000 diff 12500             0.1 %
 Right rectangle method    12512500.0000000000 diff -12500            -0.1 %
 Middle rectangle method   12500000.0000000000 diff 0                 0 %
 Trapezium  method         12500000.0000000000 diff 0                 0 %
 Simpson's Rule            12500000.0000000000 diff 0                 0 %

 Function y =x from 0 to 6000 in 1000 steps
 Known exact value =18000000
 Left rectangle method     17982000.0000000000 diff 18000             0.1 %
 Right rectangle method    18018000.0000000000 diff -18000            -0.1 %
 Middle rectangle method   18000000.0000000000 diff 0                 0 %
 Trapezium  method         18000000.0000000000 diff 0                 0 %
 Simpson's Rule            18000000.0000000000 diff 0                 0 %


## Logo


```logo
to i.left :fn :x :step
  output invoke :fn :x
end
to i.right :fn :x :step
  output invoke :fn :x + :step
end
to i.mid :fn :x :step
  output invoke :fn :x + :step/2
end
to i.trapezium :fn :x :step
  output ((i.left :fn :x :step) + (i.right :fn :x :step)) / 2
end
to i.simpsons :fn :x :step
  output ( (i.left :fn :x :step)
         + (i.mid :fn :x :step) * 4
         + (i.right :fn :x :step) ) / 6
end

to integrate :method :fn :steps :a :b
  localmake "step (:b - :a) / :steps
  localmake "sigma 0
  ; for [x :a :b-:step :step] [make "sigma :sigma + apply :method (list :fn :x :step)]
  repeat :steps [
    make "sigma :sigma + (invoke :method :fn :a :step)
    make "a :a + :step ]
  output :sigma * :step
end

to fn2 :x
  output 2 / (1 + 4 * :x * :x)
end
print integrate "i.left      "fn2 4 -1 2  ; 2.456897
print integrate "i.right     "fn2 4 -1 2  ; 2.245132
print integrate "i.mid       "fn2 4 -1 2  ; 2.496091
print integrate "i.trapezium "fn2 4 -1 2  ; 2.351014
print integrate "i.simpsons  "fn2 4 -1 2  ; 2.447732
```


## Lua


```lua
function leftRect( f, a, b, n )
    local h = (b - a) / n
    local x = a
    local sum = 0
    
    for i = 1, 100 do
        sum = sum + a + f(x)
        x = x + h
    end

    return sum * h
end

function rightRect( f, a, b, n )
    local h = (b - a) / n
    local x = b
    local sum = 0
    
    for i = 1, 100 do
        sum = sum + a + f(x)
        x = x - h
    end
    
    return sum * h
end

function midRect( f, a, b, n )
    local h = (b - a) / n
    local x = a + h/2
    local sum = 0
    
    for i = 1, 100 do
        sum = sum + a + f(x)
        x = x + h
    end

    return sum * h
end

function trapezium( f, a, b, n )
    local h = (b - a) / n
    local x = a
    local sum = 0
    
    for i = 1, 100 do
        sum = sum + f(x)*2
        x = x + h
    end

    return (b - a) * sum / (2 * n)
end

function simpson( f, a, b, n )
    local h = (b - a) / n
    local sum1 = f(a + h/2)
    local sum2 = 0

    for i = 1, n-1 do
        sum1 = sum1 + f(a + h * i + h/2)
        sum2 = sum2 + f(a + h * i)
    end    

    return (h/6) * (f(a) + f(b) + 4*sum1 + 2*sum2)
end


int_methods = { leftRect, rightRect, midRect, trapezium, simpson }
for i = 1, 5 do
    print( int_methods[i]( function(x) return x^3 end, 0, 1, 100 ) ) 
    print( int_methods[i]( function(x) return 1/x end, 1, 100, 1000 ) )
    print( int_methods[i]( function(x) return x end, 0, 5000, 5000000 ) )
    print( int_methods[i]( function(x) return x end, 0, 6000, 6000000 ) )
end
```



## Mathematica


```Mathematica
leftRect[f_, a_Real, b_Real, N_Integer] := 
 Module[{sum = 0, dx = (b - a)/N, x = a, n = N} ,
  For[n = N, n > 0, n--, x += dx; sum += f[x];];
  Return [ sum*dx ]]

rightRect[f_, a_Real, b_Real, N_Integer] := 
 Module[{sum = 0, dx = (b - a)/N, x = a + (b - a)/N, n = N} ,
  For[n = N, n > 0, n--, x += dx; sum += f[x];];
  Return [ sum*dx ]]

midRect[f_, a_Real, b_Real, N_Integer] := 
 Module[{sum = 0, dx = (b - a)/N, x = a + (b - a)/(2 N), n = N} ,
  For[n = N, n > 0, n--, x += dx; sum += f[x];];
  Return [ sum*dx ]]

trapezium[f_, a_Real, b_Real, N_Integer] := 
 Module[{sum = f[a], dx = (b - a)/N, x = a, n = N} ,
  For[n = 1, n < N, n++, x += dx; sum += 2 f[x];];
  sum += f[b];
  Return [ 0.5*sum*dx ]]

simpson[f_, a_Real, b_Real, N_Integer] := 
 Module[{sum1 = f[a + (b - a)/(2 N)], sum2 = 0, dx = (b - a)/N, x = a, n = N} ,
  For[n = 1, n < N, n++, sum1 += f[a + dx*n + dx/2]; 
   sum2 += f[a + dx*n];];
  Return [(dx/6)*(f[a] + f[b] + 4*sum1 + 2*sum2)]]
```


```txt
f[x_] := x^3
g[x_] := 1/x
h[x_] := x
Compare[t_] := Apply[ #1, t] & /@ {leftRect, rightRect, midRect, trapezium, simpson}

AccountingForm[
 Compare /@ {{f, 0., 1., 100}, {g, 1., 100., 1000}, 
{h, 0., 5000., 5000000}, {h, 0., 6000., 6000000}}]

->
{{0.255025, 0.265328,  0.260138,  0.250025,  0.25},
{4.55698,   4.46789,   4.51142,   4.60599,   4.60517},
{12500003., 12500008., 12500005., 12500000., 12500000.},
{18000003., 18000009., 18000006., 18000000., 18000000.}}
```


=={{header|MATLAB}} / {{header|Octave}}==

For all of the examples given, the function that is passed to the method as parameter f is a function handle.

Function for performing left rectangular integration: leftRectIntegration.m

```MATLAB
function integral = leftRectIntegration(f,a,b,n)

    format long;
    width = (b-a)/n; %calculate the width of each devision
    x = linspace(a,b,n); %define x-axis
    integral = width * sum( f(x(1:n-1)) );
    
end
```


Function for performing right rectangular integration: rightRectIntegration.m

```MATLAB
function integral = rightRectIntegration(f,a,b,n)

    format long;
    width = (b-a)/n; %calculate the width of each devision
    x = linspace(a,b,n); %define x-axis
    integral = width * sum( f(x(2:n)) );
    
end
```


Function for performing mid-point rectangular integration: midPointRectIntegration.m

```MATLAB
function integral = midPointRectIntegration(f,a,b,n)

    format long;
    width = (b-a)/n; %calculate the width of each devision
    x = linspace(a,b,n); %define x-axis
    integral = width * sum( f( (x(1:n-1)+x(2:n))/2 ) );
    
end
```


Function for performing trapezoidal integration: trapezoidalIntegration.m

```MATLAB
function integral = trapezoidalIntegration(f,a,b,n)

    format long;
    x = linspace(a,b,n); %define x-axis
    integral = trapz( x,f(x) );
    
end
```


Simpson's rule for numerical integration is already included in MATLAB as "quad()". It is not the same as the above examples, instead of specifying the amount of points to divide the x-axis into, the programmer passes the acceptable error tolerance for the calculation (parameter "tol").

```MATLAB
integral = quad(f,a,b,tol)
```


Using anonymous functions


```MATLAB
trapezoidalIntegration(@(x)( exp(-(x.^2)) ),0,10,100000)

ans =

   0.886226925452753
```


Using predefined functions

Built-in MATLAB function sin(x):

```MATLAB
quad(@sin,0,pi,1/1000000000000)

ans =

   2.000000000000000
```


User defined scripts and functions:
fermiDirac.m

```MATLAB
function answer = fermiDirac(x)
    k = 8.617343e-5; %Boltazmann's Constant in eV/K
    answer = 1./( 1+exp( (x)/(k*2000) ) ); %Fermi-Dirac distribution with mu = 0 and T = 2000K
end
```



```MATLAB
 rightRectIntegration(@fermiDirac,-1,1,1000000)

ans =

   0.999998006023282
```



## Maxima


```maxima
right_rect(e, x, a, b, n) := block([h: (b - a) / n, s: 0],
   for i from 1 thru n do s: s + subst(x = a + i * h, e),
   s * h)$
   
left_rect(e, x, a, b, n) := block([h: (b - a) / n, s: 0],
   for i from 1 thru n do s: s + subst(x = a + (i - 1) * h, e),
   s * h)$

mid_rect(e, x, a, b, n) := block([h: (b - a) / n, s: 0],
   for i from 1 thru n do s: s + subst(x = a + (i - 1/2) * h, e),
   s * h)$

trapezium(e, x, a, b, n) := block([h: (b - a) / n, s: 0],
   for i from 1 thru n - 1 do s: s + subst(x = a + i * h, e),
   ((subst(x = a, e) + subst(x = b, e)) / 2 + s) * h)$

simpson(e, x, a, b, n) := block([h: (b - a) / n, s: 0],
   for i from 1 thru n do
      s: s + subst(x = a + i * h, e) + 2 * subst(x = a + (i - 1/2) * h, e),
   (subst(x = a, e) - subst(x = b, e) + 2 * s) * h / 6)$

/* some tests */

simpson(log(x), x, 1, 2, 20), bfloat;
2 * log(2) - 1 - %, bfloat;

trapezium(1/x, x, 1, 100, 10000) - log(100), bfloat;
```



## Nim

{{trans|Python}}

```nim
type Function = proc(x: float): float
type Rule = proc(f: Function; x, h: float): float

proc leftRect(f: Function; x, h: float): float =
  f(x)

proc midRect(f: Function; x, h: float): float =
  f(x + h/2.0)

proc rightRect(f: Function; x, h: float): float =
  f(x + h)

proc trapezium(f: Function; x, h: float): float =
  (f(x) + f(x+h)) / 2.0

proc simpson(f: Function, x, h: float): float =
  (f(x) + 4.0*f(x+h/2.0) + f(x+h)) / 6.0

proc cube(x: float): float =
  x * x *x

proc reciprocal(x: float): float =
  1.0 / x

proc identity(x: float): float =
  x

proc integrate(f: Function; a, b: float; steps: int; meth: Rule): float =
  let h = (b-a) / float(steps)
  for i in 0 .. <steps:
    result += meth(f, a+float(i)*h, h)
  result = h * result

for fName, a, b, steps, fun in items(
   [("cube", 0, 1, 100, cube),
    ("reciprocal", 1, 100, 1000, reciprocal),
    ("identity", 0, 5000, 5_000_000, identity),
    ("identity", 0, 6000, 6_000_000, identity)]):

  for rName, rule in items({"leftRect": leftRect, "midRect": midRect,
      "rightRect": rightRect, "trapezium": trapezium, "simpson": simpson}):

    echo fName, " integrated using ", rName
    echo "  from ", a, " to ", b, " (", steps, " steps) = ",
      integrate(fun, float(a), float(b), steps, rule)
```

Output:

```txt
cube integrated using leftRect
  from 0 to 1 (100 steps) = 2.4502500000000005e-01
cube integrated using midRect
  from 0 to 1 (100 steps) = 2.4998750000000006e-01
cube integrated using rightRect
  from 0 to 1 (100 steps) = 2.5502500000000006e-01
cube integrated using trapezium
  from 0 to 1 (100 steps) = 2.5002500000000000e-01
cube integrated using simpson
  from 0 to 1 (100 steps) = 2.5000000000000000e-01
reciprocal integrated using leftRect
  from 1 to 100 (1000 steps) = 4.6549910575146800e+00
reciprocal integrated using midRect
  from 1 to 100 (1000 steps) = 4.6047625486783756e+00
reciprocal integrated using rightRect
  from 1 to 100 (1000 steps) = 4.5569810575146796e+00
reciprocal integrated using trapezium
  from 1 to 100 (1000 steps) = 4.6059860575146763e+00
reciprocal integrated using simpson
  from 1 to 100 (1000 steps) = 4.6051703849571330e+00
identity integrated using leftRect
  from 0 to 5000 (5000000 steps) = 1.2499997500000000e+07
identity integrated using midRect
  from 0 to 5000 (5000000 steps) = 1.2500000000000000e+07
identity integrated using rightRect
  from 0 to 5000 (5000000 steps) = 1.2500002500000000e+07
identity integrated using trapezium
  from 0 to 5000 (5000000 steps) = 1.2500000000000000e+07
identity integrated using simpson
  from 0 to 5000 (5000000 steps) = 1.2500000000000000e+07
identity integrated using leftRect
  from 0 to 6000 (6000000 steps) = 1.7999997000000004e+07
identity integrated using midRect
  from 0 to 6000 (6000000 steps) = 1.7999999999999993e+07
identity integrated using rightRect
  from 0 to 6000 (6000000 steps) = 1.8000003000000004e+07
identity integrated using trapezium
  from 0 to 6000 (6000000 steps) = 1.7999999999999993e+07
identity integrated using simpson
  from 0 to 6000 (6000000 steps) = 1.7999999999999993e+07
```



## OCaml

The problem can be described as integrating using each of a set of methods, over a set of functions, so let us just build the solution in this modular way.
First define the integration function:
```ocaml
let integrate f a b steps meth =
  let h = (b -. a) /. float_of_int steps in
  let rec helper i s =
    if i >= steps then s
    else helper (succ i) (s +. meth f (a +. h *. float_of_int i) h)
  in
  h *. helper 0 0.
```
Then list the methods:
```ocaml
let methods = [
  ( "rect_l", fun f x _ -> f x);
  ( "rect_m", fun f x h -> f (x +. h /. 2.) );
  ( "rect_r", fun f x h -> f (x +. h) );
  ( "trap",   fun f x h -> (f x +. f (x +. h)) /. 2. );
  ( "simp",   fun f x h -> (f x +. 4. *. f (x +. h /. 2.) +. f (x +. h)) /. 6. )
]
```
 and functions (with limits and steps)
```ocaml
let functions = [
  ( "cubic", (fun x -> x*.x*.x), 0.0, 1.0, 100);
  ( "recip", (fun x -> 1.0/.x), 1.0, 100.0, 1000);
  ( "x to 5e3", (fun x -> x), 0.0, 5000.0, 5_000_000);
  ( "x to 6e3", (fun x -> x), 0.0, 6000.0, 6_000_000)
]
```
and finally iterate the integration over both lists:
```ocaml
let () = 
  List.iter (fun (s,f,lo,hi,n) ->
    Printf.printf "Testing function %s:\n" s;
    List.iter (fun (name,meth) ->
      Printf.printf "  method %s gives %.15g\n" name (integrate f lo hi n meth)
    ) methods
  ) functions
```
Giving the output:

```txt

Testing function cubic:
  method rect_l gives 0.245025
  method rect_m gives 0.2499875
  method rect_r gives 0.255025
  method trap gives 0.250025
  method simp gives 0.25
Testing function recip:
  method rect_l gives 4.65499105751468
  method rect_m gives 4.60476254867838
  method rect_r gives 4.55698105751468
  method trap gives 4.60598605751468
  method simp gives 4.60517038495713
Testing function x to 5e3:
  method rect_l gives 12499997.5
  method rect_m gives 12500000
  method rect_r gives 12500002.5
  method trap gives 12500000
  method simp gives 12500000
Testing function x to 6e3:
  method rect_l gives 17999997
  method rect_m gives 18000000
  method rect_r gives 18000003
  method trap gives 18000000
  method simp gives 18000000

```



## PARI/GP

Note also that double exponential integration is available as <code>intnum(x=a,b,f(x))</code> and Romberg integration is available as <code>intnumromb(x=a,b,f(x))</code>.

```parigp
rectLeft(f, a, b, n)={
  sum(i=0,n-1,f(a+(b-a)*i/n), 0.)*(b-a)/n
};
rectMid(f, a, b, n)={
  sum(i=1,n,f(a+(b-a)*(i-.5)/n), 0.)*(b-a)/n
};
rectRight(f, a, b, n)={
  sum(i=1,n,f(a+(b-a)*i/n), 0.)*(b-a)/n
};
trapezoidal(f, a, b, n)={
  sum(i=1,n-1,f(a+(b-a)*i/n), f(a)/2+f(b)/2.)*(b-a)/n
};
Simpson(f, a, b, n)={
  my(h=(b - a)/n, s);
  s = 2*sum(i=1,n-1,
    2*f(a + h * (i+1/2)) + f(a + h * i)
  , 0.) + 4*f(a + h/2) + f(a) + f(b);
  s * h / 6
};
test(f, a, b, n)={
  my(v=[rectLeft, rectMid, rectRight, trapezoidal, Simpson]);
  print("Testing function "f" on ",[a,b]," with "n" intervals:");
  for(i=1,#v, print("\t"v[i](f, a, b, n)))
};
# \\ Turn on timer
test(x->x^3, 0, 1, 100)
test(x->1/x, 1, 100, 1000)
test(x->x, 0, 5000, 5000000)
test(x->x, 0, 6000, 6000000)
```


Results:

```txt
Testing function (x)->x^3 on [0, 1] with 100 intervals:
        0.2450249999999999998
        0.2499874999999999998
        0.2550249999999999998
        0.2500249999999999998
        0.2499999999999999999
time = 0 ms.
Testing function (x)->1/x on [1, 100] with 1000 intervals:
        4.654991057514676000
        4.604762548678375026
        4.556981057514676011
        4.605986057514676146
        4.605170384957142170
time = 15 ms.
Testing function (x)->x on [0, 5000] with 5000000 intervals:
        12499997.49999919783
        12499999.99999917123
        12500002.49999919783
        12499999.99999919783
        12499999.99999923745
time = 29,141 ms.
Testing function (x)->x on [0, 6000] with 6000000 intervals:
        17999996.99999869563
        17999999.99999864542
        18000002.99999869563
        17999999.99999869563
        17999999.99999863097
time = 34,820 ms.
```



## Pascal


```pascal
function RectLeft(function f(x: real): real; xl, xr: real): real;
 begin
  RectLeft := f(xl)
 end;

function RectMid(function f(x: real): real; xl, xr: real) : real;
 begin
  RectMid := f((xl+xr)/2)
 end;

function RectRight(function f(x: real): real; xl, xr: real): real;
 begin
  RectRight := f(xr)
 end;

function Trapezium(function f(x: real): real; xl, xr: real): real;
 begin
  Trapezium := (f(xl) + f(xr))/2
 end;

function Simpson(function f(x: real): real; xl, xr: real): real;
 begin
  Simpson := (f(xl) + 4*f((xl+xr)/2) + f(xr))/6
 end;

function integrate(function method(function f(x: real): real; xl, xr: real): real;
                   function f(x: real): real;
                   a, b: real;
                   n: integer);
 var
  integral, h: real;
  k: integer;
 begin
  integral := 0;
  h := (b-a)/n;
  for k := 0 to n-1 do
   begin
    integral := integral + method(f, a + k*h, a + (k+1)*h)
   end;
  integrate := integral
 end;
```



## Perl

{{trans|Perl 6}}

```perl
use feature 'say';

sub leftrect {
    my($func, $a, $b, $n) = @_;
    my $h = ($b - $a) / $n;
    my $sum = 0;
    for ($_ = $a; $_ < $b; $_ += $h) { $sum += $func->($_) }
    $h * $sum
}

sub rightrect {
    my($func, $a, $b, $n) = @_;
    my $h = ($b - $a) / $n;
    my $sum = 0;
    for ($_ = $a+$h; $_ < $b+$h; $_ += $h) { $sum += $func->($_) }
    $h * $sum
}

sub midrect {
    my($func, $a, $b, $n) = @_;
    my $h = ($b - $a) / $n;
    my $sum = 0;
    for ($_ = $a + $h/2; $_ < $b; $_ += $h) { $sum += $func->($_) }
    $h * $sum
}

sub trapez {
    my($func, $a, $b, $n) = @_;
    my $h = ($b - $a) / $n;
    my $sum = $func->($a) + $func->($b);
    for ($_ = $a+$h; $_ < $b; $_ += $h) { $sum += 2 * $func->($_) }
    $h/2 * $sum
}
sub simpsons {
    my($func, $a, $b, $n) = @_;
    my $h = ($b - $a) / $n;
    my $h2 = $h/2;
    my $sum1 = $func->($a + $h2);
    my $sum2 = 0;

    for ($_ = $a+$h; $_ < $b; $_ += $h) {
        $sum1 += $func->($_ + $h2);
        $sum2 += $func->($_);
    }
    $h/6 * ($func->($a) + $func->($b) + 4*$sum1 + 2*$sum2)
}

# round where needed, display in a reasonable format
sub sig {
    my($value) = @_;
    my $rounded;
    if ($value < 10) {
        $rounded = sprintf '%.6f', $value;
        $rounded =~ s/(\.\d*[1-9])0+$/$1/;
        $rounded =~ s/\.0+$//;
    } else {
        $rounded = sprintf "%.1f", $value;
        $rounded =~ s/\.0+$//;
    }
    return $rounded;
}

sub integrate {
    my($func, $a, $b, $n, $exact) = @_;

    my $f = sub { local $_ = shift; eval $func };

    my @res;
    push @res, "$func\n   in [$a..$b] / $n";
    push @res, '              exact result: ' . rnd($exact);
    push @res, '     rectangle method left: ' . rnd( leftrect($f, $a, $b, $n));
    push @res, '    rectangle method right: ' . rnd(rightrect($f, $a, $b, $n));
    push @res, '      rectangle method mid: ' . rnd(  midrect($f, $a, $b, $n));
    push @res, 'composite trapezoidal rule: ' . rnd(   trapez($f, $a, $b, $n));
    push @res, '   quadratic simpsons rule: ' . rnd( simpsons($f, $a, $b, $n));
    @res;
}
say for integrate('$_ ** 3', 0, 1, 100, 0.25); say '';
say for integrate('1 / $_', 1, 100, 1000, log(100)); say '';
say for integrate('$_', 0, 5_000, 5_000_000, 12_500_000); say '';
say for integrate('$_', 0, 6_000, 6_000_000, 18_000_000);
```

{{out}}

```txt
$_ ** 3
   in [0..1] / 100
              exact result: 0.25
     rectangle method left: 0.245025
    rectangle method right: 0.255025
      rectangle method mid: 0.249988
composite trapezoidal rule: 0.250025
   quadratic simpsons rule: 0.25

1 / $_
   in [1..100] / 1000
              exact result: 4.60517
     rectangle method left: 4.654991
    rectangle method right: 4.556981
      rectangle method mid: 4.604763
composite trapezoidal rule: 4.605986
   quadratic simpsons rule: 4.60517

$_
   in [0..5000] / 5000000
              exact result: 12500000
     rectangle method left: 12499997.5
    rectangle method right: 12500002.5
      rectangle method mid: 12500000
composite trapezoidal rule: 12500000
   quadratic simpsons rule: 12500000

$_
   in [0..6000] / 6000000
              exact result: 18000000
     rectangle method left: 17999997
    rectangle method right: 18000003
      rectangle method mid: 18000000
composite trapezoidal rule: 18000000
   quadratic simpsons rule: 18000000
```



## Perl 6

The addition of <tt>'''Promise'''</tt>/<tt>'''await'''</tt> allows for concurrent computation, and brings a significant speed-up in running time. Which is not to say that it makes this code fast, but it does make it less slow.

Note that these integrations are done with rationals rather than floats, so should be fairly precise (though of course with so few iterations they are not terribly accurate (except when they are)).  Some of the sums do overflow into <tt>Num</tt> (floating point)--currently Rakudo allows 64-bit denominators--but at least all of the interval arithmetic is exact.
{{works with|Rakudo|2018.09}}


```perl6
use MONKEY-SEE-NO-EVAL;

sub leftrect(&f, $a, $b, $n) {
    my $h = ($b - $a) / $n;
    $h * [+] do f($_) for $a, $a+$h ... $b-$h;
}
 
sub rightrect(&f, $a, $b, $n) {
    my $h = ($b - $a) / $n;
    $h * [+] do f($_) for $a+$h, $a+$h+$h ... $b;
}
 
sub midrect(&f, $a, $b, $n) {
    my $h = ($b - $a) / $n;
    $h * [+] do f($_) for $a+$h/2, $a+$h+$h/2 ... $b-$h/2;
}
 
sub trapez(&f, $a, $b, $n) {
    my $h = ($b - $a) / $n;
    my $partial-sum += f($_) * 2 for $a+$h, $a+$h+$h ... $b-$h;
    $h / 2 * [+] f($a), f($b), $partial-sum;
}
 
sub simpsons(&f, $a, $b, $n) {
    my $h = ($b - $a) / $n;
    my $h2 = $h/2;
    my $sum1 = f($a + $h2);
    my $sum2 = 0;
 
    for $a+$h, *+$h ... $b-$h {
        $sum1 += f($_ + $h2);
        $sum2 += f($_);
    }
    ($h / 6) * (f($a) + f($b) + 4*$sum1 + 2*$sum2);
}
 
sub integrate($f, $a, $b, $n, $exact) {
    my @r0;
    my $e = 0.000001;
    @r0.push: "$f\n   in [$a..$b] / $n\n";
    @r0.push: '             exact result: '~ $exact.round($e);

    my (@r1,@r2,@r3,@r4,@r5);
    my &f;
    EVAL "&f = $f";
    my $p1 = Promise.start( { @r1.push: '     rectangle method left: '~  leftrect(&f, $a, $b, $n).round($e) } );
    my $p2 = Promise.start( { @r2.push: '    rectangle method right: '~ rightrect(&f, $a, $b, $n).round($e) } );
    my $p3 = Promise.start( { @r3.push: '      rectangle method mid: '~   midrect(&f, $a, $b, $n).round($e) } );
    my $p4 = Promise.start( { @r4.push: 'composite trapezoidal rule: '~    trapez(&f, $a, $b, $n).round($e) } );
    my $p5 = Promise.start( { @r5.push: '   quadratic simpsons rule: '~  simpsons(&f, $a, $b, $n).round($e) } );

    await $p1, $p2, $p3, $p4, $p5;
    @r0, @r1, @r2, @r3, @r4, @r5;
}
 
.say for integrate '{ $_ ** 3 }', 0,     1,       100,       0.25; say '';
.say for integrate '1 / *',       1,   100,      1000,   log(100); say '';
.say for integrate '*.self',      0, 5_000, 5_000_000, 12_500_000; say '';
.say for integrate '*.self',      0, 6_000, 6_000_000, 18_000_000;
```

{{out}}

```txt
{ $_ ** 3 }
   in [0..1] / 100
              exact result: 0.25
     rectangle method left: 0.245025
    rectangle method right: 0.255025
      rectangle method mid: 0.249988
composite trapezoidal rule: 0.250025
   quadratic simpsons rule: 0.25

1 / *
   in [1..100] / 1000
              exact result: 4.60517
     rectangle method left: 4.654991
    rectangle method right: 4.556981
      rectangle method mid: 4.604763
composite trapezoidal rule: 4.605986
   quadratic simpsons rule: 4.60517

*.self
   in [0..5000] / 5000000
              exact result: 12500000
     rectangle method left: 12499997.5
    rectangle method right: 12500002.5
      rectangle method mid: 12500000
composite trapezoidal rule: 12500000
   quadratic simpsons rule: 12500000

*.self
   in [0..6000] / 6000000
              exact result: 18000000
     rectangle method left: 17999997
    rectangle method right: 18000003
      rectangle method mid: 18000000
composite trapezoidal rule: 18000000
   quadratic simpsons rule: 18000000
```



## Phix


```Phix
function rect_left(integer rid, atom x, atom h)
    if atom(h) then end if  -- suppress warning
    return call_func(rid,{x})
end function

function rect_mid(integer rid, atom x, atom h)
    return call_func(rid,{x+h/2})
end function

function rect_right(integer rid, atom x, atom h)
    return call_func(rid,{x+h})
end function

function trapezium(integer rid, atom x, atom h)
    return (call_func(rid,{x})+call_func(rid,{x+h}))/2
end function

function simpson(integer rid, atom x, atom h)
    return (call_func(rid,{x})+4*call_func(rid,{x+h/2})+call_func(rid,{x+h}))/6
end function

function cubed(atom x)
    return power(x,3)
end function
 
function recip(atom x)
    return 1/x
end function

function ident(atom x)
    return x
end function

function integrate(integer m_id, integer f_id, atom a, atom b, integer steps)
atom accum = 0,
     h = (b-a)/steps 
    for i=0 to steps-1 do
        accum += call_func(m_id,{f_id,a+h*i,h})
    end for
    return h*accum
end function

function smartp(atom N)
string res
    if N=floor(N) then return sprintf("%d",N) end if
    res = sprintf("%12f",round(N,1000000))
    if find('.',res) then
        res = trim_tail(res,"0")
        res = trim_tail(res,".")
    end if
    return res
end function

procedure test(sequence tests)
string name
atom a, b, steps, rid
    printf(1,"Function     Range     Iterations       L-Rect       M-Rect       R-Rect      Trapeze      Simpson\n")
    for i=1 to length(tests) do
        {name,a,b,steps,rid} = tests[i]
        printf(1,"  %-5s %6d - %-5d %10d  %12s %12s %12s %12s %12s\n",{name,a,b,steps,
                            smartp(integrate(routine_id("rect_left"),   rid,a,b,steps)),
                            smartp(integrate(routine_id("rect_mid"),    rid,a,b,steps)),
                            smartp(integrate(routine_id("rect_right"),  rid,a,b,steps)),
                            smartp(integrate(routine_id("trapezium"),   rid,a,b,steps)),
                            smartp(integrate(routine_id("simpson"),     rid,a,b,steps))})
    end for
end procedure

constant tests = {{"x^3", 0,    1,     100, routine_id("cubed")},
                  {"1/x", 1,  100,    1000, routine_id("recip")},
                  {"x",   0, 5000, 5000000, routine_id("ident")},
                  {"x",   0, 6000, 6000000, routine_id("ident")}}

test(tests)
```

{{out}}

```txt

Function     Range     Iterations       L-Rect       M-Rect       R-Rect      Trapeze      Simpson
  x^3        0 - 1            100      0.245025     0.249988     0.255025     0.250025         0.25
  1/x        1 - 100         1000      4.654991     4.604763     4.556981     4.605986      4.60517
  x          0 - 5000     5000000    12499997.5     12500000   12500002.5     12500000     12500000
  x          0 - 6000     6000000      17999997     18000000     18000003     18000000     18000000

```



## PL/I


```PL/I
integrals: procedure options (main);   /* 1 September 2019 */

f: procedure (x, function) returns (float(18));
   declare x float(18), function fixed binary;
   select (function);
      when (1) return (x**3);
      when (2) return (1/x);
      when (3) return (x);
      when (4) return (x);
   end;
end f;

   declare (a, b) fixed decimal (10);
   declare (rect_area, trap_area, Simpson) float(18);
   declare (d, dx) float(18);
   declare (S1, S2) float(18);
   declare N fixed decimal (15), function fixed binary;
   declare k fixed decimal (7,2);

   put ('     Rectangle-left           Rectangle-mid            Rectangle-right' ||
        '        Trapezoid                 Simpson');
   do function = 1 to 4;
      select(function);
         when (1) do; N = 100;     a = 0; b = 1;    end;
         when (2) do; N = 1000;    a = 1; b = 100;  end;
         when (3) do; N = 5000000; a = 0; b = 5000; end;
         when (4) do; N = 6000000; a = 0; b = 6000; end;
      end;
   
      dx = (b-a)/float(N);

      /* Rectangle method, left-side */
      rect_area = 0;
      do d = 0 to N-1;
         rect_area = rect_area + dx*f(a + d*dx, function);
      end;
      put skip edit (rect_area) (E(25, 15));

      /* Rectangle method, mid-point */
      rect_area = 0;
      do d = 0 to N-1;
         rect_area = rect_area + dx*f(a + d*dx + dx/2, function);
      end;
      put edit (rect_area) (E(25, 15));

      /* Rectangle method, right-side */
      rect_area = 0;
      do d = 1 to N;
         rect_area = rect_area + dx*f(a + d*dx, function);
      end;
      put edit (rect_area) (E(25, 15));

      /* Trapezoid method */
      trap_area = 0;
      do d = 0 to N-1;
         trap_area = trap_area + dx*(f(a+d*dx, function) + f(a+(d+1)*dx, function))/2;
      end;
      put edit (trap_area) (X(1), E(25, 15));

      /* Simpson's Rule */
      S1 = f(a+dx/2, function);
      S2 = 0;
      do d = 1 to N-1;
         S1 = S1 + f(a+d*dx+dx/2, function);
         S2 = S2 + f(a+d*dx, function);
      end;
      Simpson = dx * (f(a, function) + f(b, function) + 4*S1 + 2*S2) / 6;
      put edit (Simpson) (X(1), E(25, 15));
   end;

end integrals;

```


```txt

     Rectangle-left           Rectangle-mid            Rectangle-right        Trapezoid                 Simpson 
  2.450250000000000E-0001  2.499875000000000E-0001  2.550250000000000E-0001   2.500250000000000E-0001   2.500000000000000E-0001
  4.654991057514676E+0000  4.604762548678375E+0000  4.556981057514676E+0000   4.605986057514676E+0000   4.605170384957142E+0000
  1.249999750000000E+0007  1.250000000000000E+0007  1.250000250000000E+0007   1.250000000000000E+0007   1.250000000000000E+0007
  1.799999700000000E+0007  1.800000000000000E+0007  1.800000300000000E+0007   1.800000000000000E+0007   1.800000000000000E+0007

```



## PicoLisp


```PicoLisp
(scl 6)

(de leftRect (Fun X)
   (Fun X) )

(de rightRect (Fun X H)
   (Fun (+ X H)) )

(de midRect (Fun X H)
   (Fun (+ X (/ H 2))) )

(de trapezium (Fun X H)
   (/ (+ (Fun X) (Fun (+ X H))) 2) )

(de simpson (Fun X H)
   (*/
      (+
         (Fun X)
         (* 4 (Fun (+ X (/ H 2))))
         (Fun (+ X H)) )
      6 ) )

(de square (X)
   (*/ X X 1.0) )

(de integrate (Fun From To Steps Meth)
   (let (H (/ (- To From) Steps)  Sum 0)
      (for (X From  (>= (- To H) X)  (+ X H))
         (inc 'Sum (Meth Fun X H)) )
      (*/ H Sum 1.0) ) )

(prinl (round (integrate square 3.0 7.0 30 simpson)))
```

Output:

```txt
105.333
```



## PureBasic



```PureBasic
Prototype.d TestFunction(Arg.d)

Procedure.d LeftIntegral(Start, Stop, Steps, *func.TestFunction)
  Protected.d n=(Stop-Start)/Steps, sum, x=Start
  While x <= Stop-n
    sum + n * *func(x)
    x + n
  Wend
  ProcedureReturn sum
EndProcedure

Procedure.d MidIntegral(Start, Stop, Steps, *func.TestFunction)
  Protected.d n=(Stop-Start)/Steps, sum, x=Start
  While x <= Stop-n
    sum + n * *func(x+n/2)
    x + n
  Wend
  ProcedureReturn sum
EndProcedure

Procedure.d RightIntegral(Start, Stop, Steps, *func.TestFunction)
  Protected.d n=(Stop-Start)/Steps, sum, x=Start
  While x < Stop
    x + n
    sum + n * *func(x)
  Wend
  ProcedureReturn sum
EndProcedure

Procedure.d Trapezium(Start, Stop, Steps, *func.TestFunction)
  Protected.d n=(Stop-Start)/Steps, sum, x=Start
  While x<=Stop
    sum + n * (*func(x) + *func(x+n))/2
    x+n
  Wend
  ProcedureReturn sum
EndProcedure

Procedure.d Simpson(Start, Stop, Steps, *func.TestFunction)
  Protected.d n=(Stop-Start)/Steps, sum1, sum2, x=Start
  Protected i
  For i=0 To steps-1
    sum1+ *func(Start+n*i+n/2)
  Next
  For i=1 To Steps-1
    sum2+ *func(Start+n*i)
  Next
  ProcedureReturn n * (*func(Start)+ *func(Stop)+4*sum1+2*sum2) / 6
EndProcedure

;- Set up functions to integrate
Procedure.d Test1(n.d)
  ProcedureReturn n*n*n  
EndProcedure

Procedure.d Test2(n.d)
  ProcedureReturn 1/n  
EndProcedure

; This function should be integrated as a integer function, but for 
; comparably this will stay as a float.
Procedure.d Test3(n.d)
  ProcedureReturn n
EndProcedure

;- Test the code & present the results
CompilerIf #PB_Compiler_Debugger
  MessageRequester("Notice!","Running this program in Debug-mode will be slow")
CompilerEndIf

; = 0.25
Define Answer$
Answer$="Left     ="+StrD(LeftIntegral (0,1,100,@Test1()))+#CRLF$
Answer$+"Mid      ="+StrD(MidIntegral  (0,1,100,@Test1()))+#CRLF$
Answer$+"Right    ="+StrD(RightIntegral(0,1,100,@Test1()))+#CRLF$
Answer$+"Trapezium="+StrD(Trapezium    (0,1,100,@Test1()))+#CRLF$
Answer$+"Simpson  ="+StrD(Simpson      (0,1,100,@Test1()))
MessageRequester("Answer should be 1/4",Answer$)

; = Ln(100) e.g. ~4.60517019...
Answer$="Left     ="+StrD(LeftIntegral  (1,100,1000,@Test2()))+#CRLF$
Answer$+"Mid      ="+StrD(MidIntegral   (1,100,1000,@Test2()))+#CRLF$
Answer$+"Right    ="+StrD(RightIntegral (1,100,1000,@Test2()))+#CRLF$
Answer$+"Trapezium="+StrD(Trapezium     (1,100,1000,@Test2()))+#CRLF$
Answer$+"Simpson  ="+StrD(Simpson       (1,100,1000,@Test2()))
MessageRequester("Answer should be Ln(100), e.g. ~4.60517019",Answer$)

; 12,500,000
Answer$="Left     ="+StrD(LeftIntegral  (0,5000,5000000,@Test3()))+#CRLF$
Answer$+"Mid      ="+StrD(MidIntegral   (0,5000,5000000,@Test3()))+#CRLF$
Answer$+"Right    ="+StrD(RightIntegral (0,5000,5000000,@Test3()))+#CRLF$
Answer$+"Trapezium="+StrD(Trapezium     (0,5000,5000000,@Test3()))+#CRLF$
Answer$+"Simpson  ="+StrD(Simpson       (0,5000,5000000,@Test3()))
MessageRequester("Answer should be 12,500,000",Answer$)

; 18,000,000
Answer$="Left     ="+StrD(LeftIntegral  (0,6000,6000000,@Test3()))+#CRLF$
Answer$+"Mid      ="+StrD(MidIntegral   (0,6000,6000000,@Test3()))+#CRLF$
Answer$+"Right    ="+StrD(RightIntegral (0,6000,6000000,@Test3()))+#CRLF$
Answer$+"Trapezium="+StrD(Trapezium     (0,6000,6000000,@Test3()))+#CRLF$
Answer$+"Simpson  ="+StrD(Simpson       (0,6000,6000000,@Test3()))
MessageRequester("Answer should be 18,000,000",Answer$) 
```


```txt
Left     =0.2353220100
Mid      =0.2401367513
Right    =0.2550250000
Trapezium=0.2500250000
Simpson  =0.2500000000

Left     =4.6540000764
Mid      =4.6037720584
Right    =4.5569810575
Trapezium=4.6059860575
Simpson  =4.6051703850

Left     =12499992.5007297550
Mid      =12499995.0007292630
Right    =12500002.5007287540
Trapezium=12500000.0007287620
Simpson  =12500000.0000000000

Left     =17999991.0013914930
Mid      =17999994.0013910230
Right    =18000003.0013904940
Trapezium=18000000.0013905240
Simpson  =17999999.9999999960
```



## Python

Answers are first given using floating point arithmatic, then using fractions, only converted to floating point on output.

```python
from fractions import Fraction

def left_rect(f,x,h):
  return f(x)
 
def mid_rect(f,x,h):
  return f(x + h/2)
 
def right_rect(f,x,h):
  return f(x+h)
 
def trapezium(f,x,h):
  return (f(x) + f(x+h))/2.0
 
def simpson(f,x,h):
  return (f(x) + 4*f(x + h/2) + f(x+h))/6.0
 
def cube(x):
  return x*x*x
 
def reciprocal(x):
  return 1/x
 
def identity(x):
  return x
 
def integrate( f, a, b, steps, meth):
   h = (b-a)/steps
   ival = h * sum(meth(f, a+i*h, h) for i in range(steps))
   return ival  

# Tests
for a, b, steps, func in ((0., 1., 100, cube), (1., 100., 1000, reciprocal)):
    for rule in (left_rect, mid_rect, right_rect, trapezium, simpson):
        print('%s integrated using %s\n  from %r to %r (%i steps) = %r' %
              (func.__name__, rule.__name__, a, b, steps,
               integrate( func, a, b, steps, rule)))
    a, b = Fraction.from_float(a), Fraction.from_float(b)
    for rule in (left_rect, mid_rect, right_rect, trapezium, simpson):
        print('%s integrated using %s\n  from %r to %r (%i steps and fractions) = %r' %
              (func.__name__, rule.__name__, a, b, steps,
               float(integrate( func, a, b, steps, rule))))

# Extra tests (compute intensive)
for a, b, steps, func in ((0., 5000., 5000000, identity),
                          (0., 6000., 6000000, identity)):
    for rule in (left_rect, mid_rect, right_rect, trapezium, simpson):
        print('%s integrated using %s\n  from %r to %r (%i steps) = %r' %
              (func.__name__, rule.__name__, a, b, steps,
               integrate( func, a, b, steps, rule)))
    a, b = Fraction.from_float(a), Fraction.from_float(b)
    for rule in (left_rect, mid_rect, right_rect, trapezium, simpson):
        print('%s integrated using %s\n  from %r to %r (%i steps and fractions) = %r' %
              (func.__name__, rule.__name__, a, b, steps,
               float(integrate( func, a, b, steps, rule))))
```
  

'''Tests'''

```python
for a, b, steps, func in ((0., 1., 100, cube), (1., 100., 1000, reciprocal)):
    for rule in (left_rect, mid_rect, right_rect, trapezium, simpson):
        print('%s integrated using %s\n  from %r to %r (%i steps) = %r' %
              (func.__name__, rule.__name__, a, b, steps,
               integrate( func, a, b, steps, rule)))
    a, b = Fraction.from_float(a), Fraction.from_float(b)
    for rule in (left_rect, mid_rect, right_rect, trapezium, simpson):
        print('%s integrated using %s\n  from %r to %r (%i steps and fractions) = %r' %
              (func.__name__, rule.__name__, a, b, steps,
               float(integrate( func, a, b, steps, rule))))

# Extra tests (compute intensive)
for a, b, steps, func in ((1., 5000., 5000000, identity),
                          (1., 6000., 6000000, identity)):
    for rule in (left_rect, mid_rect, right_rect, trapezium, simpson):
        print('%s integrated using %s\n  from %r to %r (%i steps) = %r' %
              (func.__name__, rule.__name__, a, b, steps,
               integrate( func, a, b, steps, rule)))
    a, b = Fraction.from_float(a), Fraction.from_float(b)
    for rule in (left_rect, mid_rect, right_rect, trapezium, simpson):
        print('%s integrated using %s\n  from %r to %r (%i steps and fractions) = %r' %
              (func.__name__, rule.__name__, a, b, steps,
               float(integrate( func, a, b, steps, rule))))
```


'''Sample test Output'''

```txt
cube integrated using left_rect
  from 0.0 to 1.0 (100 steps) = 0.24502500000000005
cube integrated using mid_rect
  from 0.0 to 1.0 (100 steps) = 0.24998750000000006
cube integrated using right_rect
  from 0.0 to 1.0 (100 steps) = 0.25502500000000006
cube integrated using trapezium
  from 0.0 to 1.0 (100 steps) = 0.250025
cube integrated using simpson
  from 0.0 to 1.0 (100 steps) = 0.25
cube integrated using left_rect
  from Fraction(0, 1) to Fraction(1, 1) (100 steps and fractions) = 0.245025
cube integrated using mid_rect
  from Fraction(0, 1) to Fraction(1, 1) (100 steps and fractions) = 0.2499875
cube integrated using right_rect
  from Fraction(0, 1) to Fraction(1, 1) (100 steps and fractions) = 0.255025
cube integrated using trapezium
  from Fraction(0, 1) to Fraction(1, 1) (100 steps and fractions) = 0.250025
cube integrated using simpson
  from Fraction(0, 1) to Fraction(1, 1) (100 steps and fractions) = 0.25
reciprocal integrated using left_rect
  from 1.0 to 100.0 (1000 steps) = 4.65499105751468
reciprocal integrated using mid_rect
  from 1.0 to 100.0 (1000 steps) = 4.604762548678376
reciprocal integrated using right_rect
  from 1.0 to 100.0 (1000 steps) = 4.55698105751468
reciprocal integrated using trapezium
  from 1.0 to 100.0 (1000 steps) = 4.605986057514676
reciprocal integrated using simpson
  from 1.0 to 100.0 (1000 steps) = 4.605170384957133
reciprocal integrated using left_rect
  from Fraction(1, 1) to Fraction(100, 1) (1000 steps and fractions) = 4.654991057514676
reciprocal integrated using mid_rect
  from Fraction(1, 1) to Fraction(100, 1) (1000 steps and fractions) = 4.604762548678376
reciprocal integrated using right_rect
  from Fraction(1, 1) to Fraction(100, 1) (1000 steps and fractions) = 4.556981057514676
reciprocal integrated using trapezium
  from Fraction(1, 1) to Fraction(100, 1) (1000 steps and fractions) = 4.605986057514677
reciprocal integrated using simpson
  from Fraction(1, 1) to Fraction(100, 1) (1000 steps and fractions) = 4.605170384957134
identity integrated using left_rect
  from 0.0 to 5000.0 (5000000 steps) = 12499997.5
identity integrated using mid_rect
  from 0.0 to 5000.0 (5000000 steps) = 12500000.0
identity integrated using right_rect
  from 0.0 to 5000.0 (5000000 steps) = 12500002.5
identity integrated using trapezium
  from 0.0 to 5000.0 (5000000 steps) = 12500000.0
identity integrated using simpson
  from 0.0 to 5000.0 (5000000 steps) = 12500000.0
identity integrated using left_rect
  from Fraction(0, 1) to Fraction(5000, 1) (5000000 steps and fractions) = 12499997.5
identity integrated using mid_rect
  from Fraction(0, 1) to Fraction(5000, 1) (5000000 steps and fractions) = 12500000.0
identity integrated using right_rect
  from Fraction(0, 1) to Fraction(5000, 1) (5000000 steps and fractions) = 12500002.5
identity integrated using trapezium
  from Fraction(0, 1) to Fraction(5000, 1) (5000000 steps and fractions) = 12500000.0
identity integrated using simpson
  from Fraction(0, 1) to Fraction(5000, 1) (5000000 steps and fractions) = 12500000.0
identity integrated using left_rect
  from 0.0 to 6000.0 (6000000 steps) = 17999997.000000004
identity integrated using mid_rect
  from 0.0 to 6000.0 (6000000 steps) = 17999999.999999993
identity integrated using right_rect
  from 0.0 to 6000.0 (6000000 steps) = 18000003.000000004
identity integrated using trapezium
  from 0.0 to 6000.0 (6000000 steps) = 17999999.999999993
identity integrated using simpson
  from 0.0 to 6000.0 (6000000 steps) = 17999999.999999993
identity integrated using left_rect
  from Fraction(0, 1) to Fraction(6000, 1) (6000000 steps and fractions) = 17999997.0
identity integrated using mid_rect
  from Fraction(0, 1) to Fraction(6000, 1) (6000000 steps and fractions) = 18000000.0
identity integrated using right_rect
  from Fraction(0, 1) to Fraction(6000, 1) (6000000 steps and fractions) = 18000003.0
identity integrated using trapezium
  from Fraction(0, 1) to Fraction(6000, 1) (6000000 steps and fractions) = 17999999.999999993
identity integrated using simpson
  from Fraction(0, 1) to Fraction(6000, 1) (6000000 steps and fractions) = 17999999.999999993
```


A faster Simpson's rule integrator is

```python
def faster_simpson(f, a, b, steps):
   h = (b-a)/float(steps)
   a1 = a+h/2
   s1 = sum( f(a1+i*h) for i in range(0,steps))
   s2 = sum( f(a+i*h) for i in range(1,steps))
   return (h/6.0)*(f(a)+f(b)+4.0*s1+2.0*s2)
```



## R

{{works with|R|2.11.0}}

These presume that f can take a vector argument.


```R
integrate.rect <- function(f, a, b, n, k=0) {
  #k = 0 for left, 1 for right, 0.5 for midpoint
  h <- (b-a)/n
  x <- seq(a, b, len=n+1)
  sum(f(x[-1]-h*(1-k)))*h
}

integrate.trapezoid <- function(f, a, b, n) {
  h <- (b-a)/n
  x <- seq(a, b, len=n+1)
  fx <- f(x)
  sum(fx[-1] + fx[-length(x)])*h/2
}

integrate.simpsons <- function(f, a, b, n) {
  h <- (b-a)/n 
  x <- seq(a, b, len=n+1)
  fx <- f(x)
  sum(fx[-length(x)] + 4*f(x[-1]-h/2) + fx[-1]) * h/6
}

f1 <- (function(x) {x^3})
f2 <- (function(x) {1/x})
f3 <- (function(x) {x})
f4 <- (function(x) {x})

integrate.simpsons(f1,0,1,100) #0.25
integrate.simpsons(f2,1,100,1000) # 4.60517
integrate.simpsons(f3,0,5000,5000000) # 12500000
integrate.simpsons(f4,0,6000,6000000) # 1.8e+07

integrate.rect(f1,0,1,100,0) #TopLeft 0.245025
integrate.rect(f1,0,1,100,0.5) #Mid 0.2499875
integrate.rect(f1,0,1,100,1) #TopRight 0.255025

integrate.trapezoid(f1,0,1,100) # 0.250025
```



## Racket


```racket

#lang racket
(define (integrate f a b steps meth)
  (define h (/ (- b a) steps))
  (* h (for/sum ([i steps])
         (meth f (+ a (* h i)) h))))
 
(define (left-rect f x h) (f x))
(define (mid-rect f x h)  (f (+ x (/ h 2))))
(define (right-rect f x h)(f (+ x h)))
(define (trapezium f x h) (/ (+ (f x) (f (+ x h))) 2))
(define (simpson f x h)   (/ (+ (f x) (* 4 (f (+ x (/ h 2)))) (f (+ x h))) 6))

(define (test f a b s n)
  (displayln n)
  (for ([meth (list left-rect mid-rect right-rect trapezium simpson)]
        [name '(    left-rect mid-rect right-rect trapezium simpson)])
    (displayln (~a name ":\t" (integrate f a b s meth))))
  (newline))

(test (λ(x) (* x x x)) 0.    1.     100 "CUBED")
(test (λ(x) (/ x))     1.  100.    1000 "RECIPROCAL")
(test (λ(x) x)         0. 5000. 5000000 "IDENTITY")
(test (λ(x) x)         0. 6000. 6000000 "IDENTITY")

```

Output:

```racket

CUBED
left-rect:	0.24502500000000005
mid-rect:	0.24998750000000006
right-rect:	0.25502500000000006
trapezium:	0.250025
simpson:	0.25

RECIPROCAL
left-rect:	4.65499105751468
mid-rect:	4.604762548678376
right-rect:	4.55698105751468
trapezium:	4.605986057514676
simpson:	4.605170384957133

IDENTITY
left-rect:	12499997.5
mid-rect:	12500000.0
right-rect:	12500002.5
trapezium:	12500000.0
simpson:	12500000.0

IDENTITY
left-rect:	17999997.000000004
mid-rect:	17999999.999999993
right-rect:	18000003.000000004
trapezium:	17999999.999999993
simpson:	17999999.999999993

```



## REXX

Note:   there was virtually no difference between   '''numeric digits 9'''   (the default) and   '''numeric digits 20'''.     

```rexx
/*REXX pgm performs numerical integration using 5 different algorithms and show results.*/
numeric digits 20                                /*use twenty decimal digits precision. */

     do test=1  for 4                            /*perform the 4 different test suites. */
     if test==1  then do;    L= 0;     H=    1;     i=     100;     end
     if test==2  then do;    L= 1;     H=  100;     i=    1000;     end
     if test==3  then do;    L= 0;     H= 5000;     i= 5000000;     end
     if test==4  then do;    L= 0;     H= 6000;     i= 5000000;     end
     say
     say center('test' test, 65, "─")            /*display a header for the test suite. */
     say '      left rectangular('L", "H', 'i")  ──► "        left_rect(L, H, i)
     say '  midpoint rectangular('L", "H', 'i")  ──► "    midpoint_rect(L, H, i)
     say '     right rectangular('L", "H', 'i")  ──► "       right_rect(L, H, i)
     say '               Simpson('L", "H', 'i")  ──► "          Simpson(L, H, i)
     say '             trapezium('L", "H', 'i")  ──► "        trapezium(L, H, i)
     end   /*test*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
f:   if test==1  then return arg(1) **3          /*choose the    cube     function.     */
     if test==2  then return 1 / arg(1)          /*   "    "  reciprocal     "          */
                      return arg(1)              /*   "    "    "as-is"      "          */
/*──────────────────────────────────────────────────────────────────────────────────────*/
left_rect:     procedure expose test;  parse arg a,b,n;              h= (b-a) / n
               $= 0
                               do x=a  by h  for n;      $=$+f(x);   end  /*x*/
               return $*h/1
/*──────────────────────────────────────────────────────────────────────────────────────*/
midpoint_rect: procedure expose test;  parse arg a,b,n;              h= (b-a) / n
               $= 0
                               do x=a+h/2  by h  for n;  $=$+f(x);   end  /*x*/
               return $*h/1
/*──────────────────────────────────────────────────────────────────────────────────────*/
right_rect:    procedure expose test;  parse arg a,b,n;              h= (b-a) / n
               $= 0
                               do x=a+h    by h  for n;  $=$+f(x);   end  /*x*/
               return $*h/1
/*──────────────────────────────────────────────────────────────────────────────────────*/
Simpson:       procedure expose test;  parse arg a,b,n;              h= (b-a) / n
               $= f(a + h/2)
               @= 0;           do x=1  for n-1; $=$+f(a+h*x+h*.5); @=@+f(a+x*h); end /*x*/

               return h*(f(a) + f(b) + 4*$ + 2*@)  /  6
/*──────────────────────────────────────────────────────────────────────────────────────*/
trapezium:     procedure expose test;   parse arg a,b,n;              h=(b-a)/n
               $= 0
                               do x=a  by h  for n;      $=$+(f(x)+f(x+h));  end  /*x*/
               return $*h/2
```

{{out|output|text=  when using the default inputs:}}

```txt

─────────────────────────────test 1──────────────────────────────
      left rectangular(0, 1, 100)  ──►  0.245025
  midpoint rectangular(0, 1, 100)  ──►  0.2499875
     right rectangular(0, 1, 100)  ──►  0.255025
               Simpson(0, 1, 100)  ──►  0.25
             trapezium(0, 1, 100)  ──►  0.250025

─────────────────────────────test 2──────────────────────────────
      left rectangular(1, 100, 1000)  ──►  4.6549910575146761473
  midpoint rectangular(1, 100, 1000)  ──►  4.604762548678375185
     right rectangular(1, 100, 1000)  ──►  4.5569810575146761472
               Simpson(1, 100, 1000)  ──►  4.6051703849571421725
             trapezium(1, 100, 1000)  ──►  4.605986057514676146

─────────────────────────────test 3──────────────────────────────
      left rectangular(0, 5000, 5000000)  ──►  12499997.5
  midpoint rectangular(0, 5000, 5000000)  ──►  12500000
     right rectangular(0, 5000, 5000000)  ──►  12500002.5
               Simpson(0, 5000, 5000000)  ──►  12500000
             trapezium(0, 5000, 5000000)  ──►  12500000

─────────────────────────────test 4──────────────────────────────
      left rectangular(0, 6000, 5000000)  ──►  17999996.4
  midpoint rectangular(0, 6000, 5000000)  ──►  18000000
     right rectangular(0, 6000, 5000000)  ──►  18000003.6
               Simpson(0, 6000, 5000000)  ──►  18000000
             trapezium(0, 6000, 5000000)  ──►  18000000

```



## Ring


```ring

# Project : Numerical integration

decimals(8)
data = [["pow(x,3)",0,1,100], ["1/x",1, 100,1000], ["x",0,5000,5000000], ["x",0,6000,6000000]]
see "Function   Range   L-Rect   R-Rect   M-Rect   Trapeze   Simpson" + nl
for p = 1 to 4
     d1 = data[p][1]
     d2 = data[p][2]
     d3 = data[p][3]
     d4 = data[p][4]
     see "" + d1 + "   " + d2  + " - " + d3  + "   " + lrect(d1, d2, d3, d4) + "   " + rrect(d1, d2, d3, d4) 
     see "  " + mrect(d1, d2, d3, d4) + "   " + trapeze(d1, d2, d3, d4) + "   " + simpson(d1, d2, d3, d4) + nl
next
 
func lrect(x2, a, b, n)
       s = 0
       d = (b - a) / n
       x = a
       for i = 1 to n
       eval("result = " + x2)       
            s = s + d * result
            x = x + d
       next
       return s
 
func rrect(x2, a, b, n)
       s = 0
       d = (b - a) / n
       x = a
       for i = 1 to n
            x = x + d
            eval("result = " + x2)  
            s = s + d *result
       next
       return s
 
func mrect(x2, a, b, n)
       s = 0
       d = (b - a) / n
       x = a
       for i = 1 to n
            x = x + d/2
            eval("result = " + x2)  
            s = s + d * result
            x =  x +d/2
       next
       return s
 
func trapeze(x2, a, b, n)
       s = 0
       d = (b - a) / n
       x = b
       eval("result = " + x2)  
       f = result
       x = a
       eval("result = " + x2)
       s = d * (f + result) / 2
       for i = 1 to n-1
            x = x + d
           eval("result = " + x2)
            s = s + d * result
       next
       return s
 
func simpson(x2, a, b, n)
        s1 = 0
        s = 0
        d = (b - a) / n
        x = b 
        eval("result = " + x2)  
        f = result
        x = a + d/2 
       eval("result = " + x2)
        s1 = result
        for i = 1 to n-1
            x = x + d/2
            eval("result = " + x2)
            s = s + result
            x = x + d/2
            eval("result = " + x2)
            s1 = s1 + result
        next
        x = a
        eval("result = " + x2)
        return (d / 6) * (f + result + 4 * s1 + 2 * s)

```

Output:

```txt

Function     Range          L-Rect      R-Rect      M-Rect      Trapeze     Simpson
pow(x,3)     0 - 1          0.245025    0.255025    0.2499875   0.250025    0.25
1/x          1 - 100        4.65499106  4.55698106  4.60476255  4.60598606  4.60517038
x            0 - 5000       12499997.5  12500002.5  12500000    12500000    12500000
x            0 - 6000       17999997    18000003    18000000    18000000    18000000

```



## Ruby

{{trans|Tcl}}

```ruby
def leftrect(f, left, right)
  f.call(left)
end
 
def midrect(f, left, right)
  f.call((left+right)/2.0)
end
 
def rightrect(f, left, right)
  f.call(right)
end
 
def trapezium(f, left, right)
  (f.call(left) + f.call(right)) / 2.0
end
 
def simpson(f, left, right)
  (f.call(left) + 4*f.call((left+right)/2.0) + f.call(right)) / 6.0
end
 
def integrate(f, a, b, steps, method)
  delta = 1.0 * (b - a) / steps
  total = 0.0
  steps.times do |i|
    left = a + i*delta
    right = left + delta
    total += delta * send(method, f, left, right)
  end
  total
end
 
def square(x)
  x**2
end
 
def def_int(f, a, b)
  l = case f.to_s
      when /sin>/
        lambda {|x| -Math.cos(x)}
      when /square>/
        lambda {|x| (x**3)/3.0}
      end
  l.call(b) - l.call(a)
end
 
a = 0
b = Math::PI
steps = 10
 
for func in [method(:square), Math.method(:sin)]
  puts "integral of #{func} from #{a} to #{b} in #{steps} steps"
  actual = def_int(func, a, b)
  for method in [:leftrect, :midrect, :rightrect, :trapezium, :simpson]
    int = integrate(func, a, b, steps, method)
    diff = (int - actual) * 100.0 / actual
    printf "   %-10s  %s\t(%.1f%%)\n", method, int, diff
  end
end
```

outputs

```txt
integral of #<Method: Object#square> from 0 to 3.14159265358979 in 10 steps
   leftrect    8.83678885388545	(-14.5%)
   midrect     10.3095869961997	(-0.2%)
   rightrect   11.9374165219154	(15.5%)
   trapezium   10.3871026879004	(0.5%)
   simpson     10.3354255600999	(0.0%)
integral of #<Method: Math.sin> from 0 to 3.14159265358979 in 10 steps
   leftrect    1.98352353750945	(-0.8%)
   midrect     2.00824840790797	(0.4%)
   rightrect   1.98352353750945	(-0.8%)
   trapezium   1.98352353750945	(-0.8%)
   simpson     2.0000067844418	(0.0%)
```




## Rust

This is a partial solution and only implements trapezium integration.

```rust>fn integral<F
(f: F, range: std::ops::Range<f64>, n_steps: u32) -> f64
    where F: Fn(f64) -> f64
{
    let step_size = (range.end - range.start)/n_steps as f64;

    let mut integral = (f(range.start) + f(range.end))/2.;
    let mut pos = range.start + step_size;
    while pos < range.end {
        integral += f(pos);
        pos += step_size;
    }
    integral * step_size
}

fn main() {
    println!("{}", integral(|x| x.powi(3), 0.0..1.0, 100));
    println!("{}", integral(|x| 1.0/x, 1.0..100.0, 1000));
    println!("{}", integral(|x| x, 0.0..5000.0, 5_000_000));
    println!("{}", integral(|x| x, 0.0..6000.0, 6_000_000));
}
```


{{out}}

```txt
0.2500250000000004
4.605986057514688
12500000.000728702
18000000.001390498
```



## Scala


```scala
object NumericalIntegration {  
  def leftRect(f:Double=>Double, a:Double, b:Double)=f(a)
  def midRect(f:Double=>Double, a:Double, b:Double)=f((a+b)/2)
  def rightRect(f:Double=>Double, a:Double, b:Double)=f(b)
  def trapezoid(f:Double=>Double, a:Double, b:Double)=(f(a)+f(b))/2
  def simpson(f:Double=>Double, a:Double, b:Double)=(f(a)+4*f((a+b)/2)+f(b))/6;

  def fn1(x:Double)=x*x*x
  def fn2(x:Double)=1/x
  def fn3(x:Double)=x
  
  type Method = (Double=>Double, Double, Double) => Double 
  def integrate(f:Double=>Double, a:Double, b:Double, steps:Double, m:Method)={
    val delta:Double=(b-a)/steps
    delta*(a until b by delta).foldLeft(0.0)((s,x) => s+m(f, x, x+delta))
  }

  def print(f:Double=>Double, a:Double, b:Double, steps:Double)={
    println("rectangular left   : %f".format(integrate(f, a, b, steps, leftRect)))
    println("rectangular middle : %f".format(integrate(f, a, b, steps, midRect)))
    println("rectangular right  : %f".format(integrate(f, a, b, steps, rightRect)))
    println("trapezoid          : %f".format(integrate(f, a, b, steps, trapezoid)))
    println("simpson            : %f".format(integrate(f, a, b, steps, simpson)))
  }
  
  def main(args: Array[String]): Unit = {
    print(fn1, 0, 1, 100)
    println("------")
    print(fn2, 1, 100, 1000)
    println("------")
    print(fn3, 0, 5000, 5000000)
    println("------")
    print(fn3, 0, 6000, 6000000)
  }
}
```

Output:

```txt
rectangular left   : 0,245025
rectangular middle : 0,249988
rectangular right  : 0,255025
trapezoid          : 0,250025
simpson            : 0,250000
------
rectangular left   : 4,654991
rectangular middle : 4,604763
rectangular right  : 4,556981
trapezoid          : 4,605986
simpson            : 4,605170
------
rectangular left   : 12499997,500729
rectangular middle : 12500000,000729
rectangular right  : 12500002,500729
trapezoid          : 12500000,000729
simpson            : 12500000,000729
------
rectangular left   : 17999997,001390
rectangular middle : 18000000,001391
rectangular right  : 18000003,001390
trapezoid          : 18000000,001391
simpson            : 18000000,001391
```



## Scheme



```scheme
(define (integrate f a b steps meth)
  (define h (/ (- b a) steps))
  (* h
     (let loop ((i 0) (s 0))
       (if (>= i steps)
           s
           (loop (+ i 1) (+ s (meth f (+ a (* h i)) h)))))))

(define (left-rect f x h) (f x))
(define (mid-rect f x h) (f (+ x (/ h 2))))
(define (right-rect f x h) (f (+ x h)))
(define (trapezium f x h) (/ (+ (f x) (f (+ x h))) 2))
(define (simpson f x h) (/ (+ (f x) (* 4 (f (+ x (/ h 2)))) (f (+ x h))) 6))

(define (square x) (* x x))

(define rl (integrate square 0 1 10 left-rect))
(define rm (integrate square 0 1 10 mid-rect))
(define rr (integrate square 0 1 10 right-rect))
(define t (integrate square 0 1 10 trapezium))
(define s (integrate square 0 1 10 simpson))
```



## Sidef

{{trans|Perl 6}}

```ruby
func sum(f, start, from, to) {
    var s = 0;
    RangeNum(start, to, from-start).each { |i|
        s += f(i);
    }
    return s
}

func leftrect(f, a, b, n) {
    var h = ((b - a) / n);
    h * sum(f, a, a+h, b-h);
}

func rightrect(f, a, b, n) {
    var h = ((b - a) / n);
    h * sum(f, a+h, a + 2*h, b);
}

func midrect(f, a, b, n) {
    var h = ((b - a) / n);
    h * sum(f, a + h/2, a + h + h/2, b - h/2)
}

func trapez(f, a, b, n) {
    var h = ((b - a) / n);
    h/2 * (f(a) + f(b) + sum({ f(_)*2 }, a+h, a + 2*h, b-h));
}

func simpsons(f, a, b, n) {
    var h = ((b - a) / n);
    var h2 = h/2;

    var sum1 = f(a + h2);
    var sum2 = 0;

    sum({|i| sum1 += f(i + h2); sum2 += f(i); 0 }, a+h, a+h+h, b-h);
    h/6 * (f(a) + f(b) + 4*sum1 + 2*sum2);
}

func tryem(label, f, a, b, n, exact) {
    say "\n#{label}\n   in [#{a}..#{b}] / #{n}";

    say('              exact result: ', exact);
    say('     rectangle method left: ', leftrect(f, a, b, n));
    say('    rectangle method right: ', rightrect(f, a, b, n));
    say('      rectangle method mid: ', midrect(f, a, b, n));
    say('composite trapezoidal rule: ', trapez(f, a, b, n));
    say('   quadratic simpsons rule: ', simpsons(f, a, b, n));
}

tryem('x^3', { _ ** 3 }, 0, 1, 100, 0.25);
tryem('1/x', { 1 / _ }, 1, 100, 1000, log(100));
tryem('x', { _ }, 0, 5_000, 5_000_000, 12_500_000);
tryem('x', { _ }, 0, 6_000, 6_000_000, 18_000_000);
```



## SequenceL


```sequencel>import <Utilities/Conversion.sl
;
import <Utilities/Sequence.sl>;

integrateLeft(f, a, b, n) :=
    let
        h := (b - a) / n;
        vals[x] := f(x) foreach x within (0 ... (n-1)) * h + a; 
    in
        h * sum(vals);

integrateRight(f, a, b, n) :=
    let
        h := (b - a) / n;
        vals[x] := f(x+h) foreach x within (0 ... (n-1)) * h + a; 
    in
        h * sum(vals);

integrateMidpoint(f, a, b, n) :=
    let
        h := (b - a) / n;
        vals[x] := f(x+h/2.0) foreach x within (0 ... (n-1)) * h + a; 
    in
        h * sum(vals);

integrateTrapezium(f, a, b, n) :=
    let
        h := (b - a) / n;
        vals[i] := 2.0 * f(a + i * h) foreach i within 1 ... n-1; 
    in
        h * (sum(vals) + f(a) + f(b)) / 2.0;

integrateSimpsons(f, a, b, n) :=
    let
        h := (b - a) / n;
        vals1[i] := f(a + h * i + h / 2.0) foreach i within 0 ... n-1;
        vals2[i] :=  f(a + h * i) foreach i within 1 ... n-1;
    in
        h / 6.0 * (f(a) + f(b) + 4.0 * sum(vals1) + 2.0 * sum(vals2));

xCubed(x) := x^3;
xInverse(x) := 1/x;
identity(x) := x;

tests[method] := 
    [method(xCubed, 0.0, 1.0, 100),
     method(xInverse, 1.0, 100.0, 1000),
     method(identity, 0.0, 5000.0, 5000000),
     method(identity, 0.0, 6000.0, 6000000)]
     foreach method within [integrateLeft, integrateRight, integrateMidpoint, integrateTrapezium, integrateSimpsons];

//String manipulation for ouput display.
main := 
    let
        heading := [["Func", "Range\t", "L-Rect\t", "R-Rect\t", "M-Rect\t", "Trapezium", "Simpson"]];
        ranges := [["0 - 1\t", "1 - 100\t", "0 - 5000", "0 - 6000"]];
        funcs := [["x^3", "1/x", "x", "x"]];
    in
        delimit(delimit(heading ++ transpose(funcs ++ ranges ++ trimEndZeroes(floatToString(tests, 8))), '\t'), '\n');

trimEndZeroes(x(1)) := x when size(x) = 0 else x when x[size(x)] /= '0' else trimEndZeroes(x[1...size(x)-1]);
```


{{out}}
<pre style="height: 25ex; overflow: scroll">
"Func	Range		L-Rect		R-Rect		M-Rect		Trapezium	Simpson
x^3	0 - 1		0.245025	0.255025	0.2499875	0.250025	0.25
1/x	1 - 100		4.65499106	4.55698106	4.60476255	4.60598606	4.60517038
x	0 - 5000	12499997.5	12500002.5	12500000.	12500000.	12500000.
x	0 - 6000	17999997.	18000003.	18000000.	18000000.	18000000."

```



## Standard ML


```sml
fun integrate (f, a, b, steps, meth) = let
  val h = (b - a) / real steps
  fun helper (i, s) =
    if i >= steps then s
    else helper (i+1, s + meth (f, a + h * real i, h))
in
  h * helper (0, 0.0)
end

fun leftRect  (f, x, _) = f x
fun midRect   (f, x, h) = f (x + h / 2.0)
fun rightRect (f, x, h) = f (x + h)
fun trapezium (f, x, h) = (f x + f (x + h)) / 2.0
fun simpson   (f, x, h) = (f x + 4.0 * f (x + h / 2.0) + f (x + h)) / 6.0

fun square x = x * x


val rl = integrate (square, 0.0, 1.0, 10, left_rect )
val rm = integrate (square, 0.0, 1.0, 10, mid_rect  )
val rr = integrate (square, 0.0, 1.0, 10, right_rect)
val t  = integrate (square, 0.0, 1.0, 10, trapezium )
val s  = integrate (square, 0.0, 1.0, 10, simpson   )
```



## Stata

<lang>mata
function integrate(f,a,b,n,u,v) {
	s = 0
	h = (b-a)/n
	m = length(u)
	for (i=0; i<n; i++) {
		x = a+i*h
		for (j=1; j<=m; j++) s = s+v[j]*(*f)(x+h*u[j])
	}
	return(s*h)
}

function log_(x) {
	return(log(x))
}

function id(x) {
	return(x)
}

function cube(x) {
	return(x*x*x)
}

function inv(x) {
	return(1/x)
}

function test(f,a,b,n) {
	return(integrate(f,a,b,n,(0,1),(1,0)),
	       integrate(f,a,b,n,(0,1),(0,1)),
	       integrate(f,a,b,n,(0.5),(1)),
	       integrate(f,a,b,n,(0,1),(0.5,0.5)),
	       integrate(f,a,b,n,(0,1/2,1),(1/6,4/6,1/6)))
}

test(&cube(),0,1,100)
test(&inv(),1,100,1000)
test(&id(),0,5000,5000000)
test(&id(),0,6000,6000000)
end
```


'''Output'''


```txt
              1          2          3          4          5
    +--------------------------------------------------------+
  1 |   .245025    .255025   .2499875    .250025        .25  |
    +--------------------------------------------------------+

                 1             2             3             4             5
    +-----------------------------------------------------------------------+
  1 |  4.654991058   4.556981058   4.604762549   4.605986058   4.605170385  |
    +-----------------------------------------------------------------------+

                1            2            3            4            5
    +------------------------------------------------------------------+
  1 |  12499997.5   12500002.5     12500000     12500000     12500000  |
    +------------------------------------------------------------------+

              1          2          3          4          5
    +--------------------------------------------------------+
  1 |  17999997   18000003   18000000   18000000   18000000  |
    +--------------------------------------------------------+
```



## Swift


```swift
public enum IntegrationType : CaseIterable {
  case rectangularLeft
  case rectangularRight
  case rectangularMidpoint
  case trapezium
  case simpson
}

public func integrate(
  from: Double,
  to: Double,
  n: Int,
  using: IntegrationType = .simpson,
  f: (Double) -> Double
) -> Double {
  let integrationFunc: (Double, Double, Int, (Double) -> Double) -> Double

  switch using {
  case .rectangularLeft:
    integrationFunc = integrateRectL
  case .rectangularRight:
    integrationFunc = integrateRectR
  case .rectangularMidpoint:
    integrationFunc = integrateRectMid
  case .trapezium:
    integrationFunc = integrateTrapezium
  case .simpson:
    integrationFunc = integrateSimpson
  }

  return integrationFunc(from, to, n, f)
}

private func integrateRectL(from: Double, to: Double, n: Int, f: (Double) -> Double) -> Double {
  let h = (to - from) / Double(n)
  var x = from
  var sum = 0.0

  while x <= to - h {
    sum += f(x)
    x += h
  }

  return h * sum
}

private func integrateRectR(from: Double, to: Double, n: Int, f: (Double) -> Double) -> Double {
  let h = (to - from) / Double(n)
  var x = from
  var sum = 0.0

  while x <= to - h {
    sum += f(x + h)
    x += h
  }

  return h * sum
}

private func integrateRectMid(from: Double, to: Double, n: Int, f: (Double) -> Double) -> Double {
  let h = (to - from) / Double(n)
  var x = from
  var sum = 0.0

  while x <= to - h {
    sum += f(x + h / 2.0)
    x += h
  }

  return h * sum
}

private func integrateTrapezium(from: Double, to: Double, n: Int, f: (Double) -> Double) -> Double {
  let h = (to - from) / Double(n)
  var sum = f(from) + f(to)

  for i in 1..<n {
    sum += 2 * f(from + Double(i) * h)
  }

  return h * sum / 2
}

private func integrateSimpson(from: Double, to: Double, n: Int, f: (Double) -> Double) -> Double {
  let h = (to - from) / Double(n)
  var sum1 = 0.0
  var sum2 = 0.0

  for i in 0..<n {
    sum1 += f(from + h * Double(i) + h / 2.0)
  }

  for i in 1..<n {
    sum2 += f(from + h * Double(i))
  }

  return h / 6.0 * (f(from) + f(to) + 4.0 * sum1 + 2.0 * sum2)
}

let types = IntegrationType.allCases

print("f(x) = x^3:", types.map({ integrate(from: 0, to: 1, n: 100, using: $0, f: { pow($0, 3) }) }))
print("f(x) = 1 / x:", types.map({ integrate(from: 1, to: 100, n: 1000, using: $0, f: { 1 / $0 }) }))
print("f(x) = x, 0 -> 5_000:", types.map({ integrate(from: 0, to: 5_000, n: 5_000_000, using: $0, f: { $0 }) }))
print("f(x) = x, 0 -> 6_000:", types.map({ integrate(from: 0, to: 6_000, n: 6_000_000, using: $0, f: { $0 }) }))
```


{{out}}

```txt
f(x) = x^3: [0.2450250000000004, 0.23532201000000041, 0.2401367512500004, 0.25002500000000005, 0.25000000000000006]
f(x) = 1 / x: [4.55599105751469, 4.654000076443428, 4.603772058385689, 4.60598605751468, 4.605170384957145]
f(x) = x, 0 -> 5_000: [12499997.500728704, 12499992.500729704, 12499995.000729209, 12500000.000000002, 12500000.0]
f(x) = x, 0 -> 6_000: [17999997.001390498, 17999991.0013915, 17999994.001391016, 18000000.000000004, 17999999.999999993]
```



## Tcl


```tcl
package require Tcl 8.5

proc leftrect {f left right} {
    $f $left
}
proc midrect {f left right} {
    set mid [expr {($left + $right) / 2.0}]
    $f $mid
}
proc rightrect {f left right} {
    $f $right
}
proc trapezium {f left right} {
    expr {([$f $left] + [$f $right]) / 2.0}
}
proc simpson {f left right} {
    set mid [expr {($left + $right) / 2.0}]
    expr {([$f $left] + 4*[$f $mid] + [$f $right]) / 6.0}
}

proc integrate {f a b steps method} {
    set delta [expr {1.0 * ($b - $a) / $steps}]
    set total 0.0
    for {set i 0} {$i < $steps} {incr i} {
        set left [expr {$a + $i * $delta}]
        set right [expr {$left + $delta}]
        set total [expr {$total + $delta * [$method $f $left $right]}]
    }
    return $total
}

interp alias {} sin {} ::tcl::mathfunc::sin
proc square x {expr {$x*$x}}
proc def_int {f a b} {
    switch -- $f {
        sin    {set lambda {x {expr {-cos($x)}}}}
        square {set lambda {x {expr {$x**3/3.0}}}}
    }
    return [expr {[apply $lambda $b] - [apply $lambda $a]}]
}

set a 0
set b [expr {4*atan(1)}]
set steps 10

foreach func {square sin} {
    puts "integral of ${func}(x) from $a to $b in $steps steps"
    set actual [def_int $func $a $b]
    foreach method {leftrect midrect rightrect trapezium simpson} {
        set int [integrate $func $a $b $steps $method]
        set diff [expr {($int - $actual) * 100.0 / $actual}]
        puts [format "   %-10s  %s\t(%.1f%%)" $method $int $diff]
    }
}
```


```txt
integral of square(x) from 0 to 3.141592653589793 in 10 steps
   leftrect    8.836788853885448	(-14.5%)
   midrect     10.30958699619969	(-0.2%)
   rightrect   11.93741652191543	(15.5%)
   trapezium   10.387102687900438	(0.5%)
   simpson     10.335425560099939	(0.0%)
integral of sin(x) from 0 to 3.141592653589793 in 10 steps
   leftrect    1.9835235375094544	(-0.8%)
   midrect     2.0082484079079745	(0.4%)
   rightrect   1.9835235375094544	(-0.8%)
   trapezium   1.9835235375094546	(-0.8%)
   simpson     2.0000067844418012	(0.0%)
```


=={{header|TI-89 BASIC}}==
<!-- Not put into category so that it's considered unimplemented. -->

TI-89 BASIC has built-in numerical integration with the ∫ operator, but no control over the method used is available so it doesn't really correspond to this task.

<math>\overbrace{{\textstyle\int}(\mathrm{expr},x,a,b)}^{\mathrm{TI-89}} = \overbrace{\int_{a}^b \mathrm{expr}\ dx}^{\text{Math notation}}</math>

An explicit numerical integration program should be written here. [[Category:TI-89 BASIC examples needing attention]]


## Ursala

A higher order function parameterized by a method <math>m</math> returns
a function that integrates by that method. The method <math>m</math> is meant to
specify whether it's rectangular, trapezoidal, etc.. 
The integrating function constructed from a given method takes a quadruple <math>(f,a,b,n)</math> containing
the integrand <math>f</math>, the bounds <math>(a,b)</math>, and the number of intervals <math>n</math>.


```Ursala
#import std
#import nat
#import flo

(integral_by "m") ("f","a","b","n") =

iprod ^(* ! div\float"n" minus/"b" "a",~&) ("m" "f")*ytp (ari successor "n")/"a" "b"
```

An alternative way of defining this function shown below prevents redundant evaluations of the integrand
at the cost of building a table-driven finite map in advance.

```Ursala
(integral_by "m") ("f","a","b","n") =

iprod ^(* ! div\float"n" minus/"b" "a",~&) ^H(*+ "m"+ -:"f"+ * ^/~& "f",~&ytp) (ari successor "n")/"a" "b"
```

As mentioned in the Haskell solution, the latter choice is preferable if evaluating the integrand
is expensive.
An integrating function is defined for each method as follows.

```Ursala
left       = integral_by "f". ("l","r"). "f" "l"
right      = integral_by "f". ("l","r"). "f" "r"
midpoint   = integral_by "f". ("l","r"). "f" div\2. plus/"l" "r"
trapezium  = integral_by "f". ("l","r"). div\2. plus "f"~~/"l" "r"
simpson    = integral_by "f". ("l","r"). div\6. plus:-0. <"f" "l",times/4. "f" div\2. plus/"l" "r","f" "r">
```

As shown above, the method passed to the <code>integral_by</code> function
is itself a higher order function taking an integrand <math>f</math> as an argument and
returning a function that operates on the pair of left and right interval endpoints.
Here is a test program showing the results of integrating the square from zero to <math>\pi</math> in ten intervals
by all five methods.

```Ursala
#cast %eL

examples = <.left,midpoint,rignt,trapezium,simpson> (sqr,0.,pi,10)
```

output:

```txt

<
   8.836789e+00,
   1.030959e+01,
   1.193742e+01,
   1.038710e+01,
   1.033543e+01>

```

(The GNU Scientific Library integration routines are also callable in Ursala, and
are faster and more accurate.)


## VBA

The following program does not follow the task requirement on two points: first, the same function is used for all quadrature methods, as they are really the same thing with different parameters (abscissas and weights). And since it's getting rather slow for a large number of intervals, the last two are integrated with resp. 50,000 and 60,000 intervals. It does not make sense anyway to use more, for such a simple function (and if really it were difficult to integrate, one would rely one more sophistcated methods).


```vb
Option Explicit
Option Base 1

Function Quad(ByVal f As String, ByVal a As Double, _
        ByVal b As Double, ByVal n As Long, _
        ByVal u As Variant, ByVal v As Variant) As Double
    Dim m As Long, h As Double, x As Double, s As Double, i As Long, j As Long
    m = UBound(u)
    h = (b - a) / n
    s = 0#
    For i = 1 To n
        x = a + (i - 1) * h
        For j = 1 To m
            s = s + v(j) * Application.Run(f, x + h * u(j))
        Next
    Next
    Quad = s * h
End Function

Function f1fun(x As Double) As Double
    f1fun = x ^ 3
End Function

Function f2fun(x As Double) As Double
    f2fun = 1 / x
End Function

Function f3fun(x As Double) As Double
    f3fun = x
End Function

Sub Test()
    Dim fun, f, coef, c
    Dim i As Long, j As Long, s As Double

    fun = Array(Array("f1fun", 0, 1, 100, 1 / 4), _
        Array("f2fun", 1, 100, 1000, Log(100)), _
        Array("f3fun", 0, 5000, 50000, 5000 ^ 2 / 2), _
        Array("f3fun", 0, 6000, 60000, 6000 ^ 2 / 2))

    coef = Array(Array("Left rect.  ", Array(0, 1), Array(1, 0)), _
        Array("Right rect. ", Array(0, 1), Array(0, 1)), _
        Array("Midpoint    ", Array(0.5), Array(1)), _
        Array("Trapez.     ", Array(0, 1), Array(0.5, 0.5)), _
        Array("Simpson     ", Array(0, 0.5, 1), Array(1 / 6, 4 / 6, 1 / 6)))
        
    For i = 1 To UBound(fun)
        f = fun(i)
        Debug.Print f(1)
        For j = 1 To UBound(coef)
            c = coef(j)
            s = Quad(f(1), f(2), f(3), f(4), c(2), c(3))
            Debug.Print "  " + c(1) + ": ", s, (s - f(5)) / f(5)
        Next j
    Next i
End Sub
```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

func real Func(FN, X);          \Return F(X) for function number FN
int  FN;  real X;
[case FN of
  1:  return X*X*X;
  2:  return 1.0/X;
  3:  return X
other return 0.0;
];

func Integrate(A, B, FN, N);    \Display area under curve for function FN
real A, B;  int FN, N;          \limits A, B, and number of slices N
real DX, X, Area;               \delta X
int  I;
[DX:= (B-A)/float(N);
X:= A;  Area:= 0.0;             \rectangular left
for I:= 1 to N do
    [Area:= Area + Func(FN,X)*DX;  X:= X+DX];
RlOut(0, Area);
X:= A;  Area:= 0.0;             \rectangular right
for I:= 1 to N do
    [X:= X+DX;  Area:= Area + Func(FN,X)*DX];
RlOut(0, Area);
X:= A+DX/2.0;  Area:= 0.0;      \rectangular mid point
for I:= 1 to N do
    [Area:= Area + Func(FN,X)*DX;  X:= X+DX];
RlOut(0, Area);
X:= A;  Area:= 0.0;             \trapezium
for I:= 1 to N do
    [Area:= Area + (Func(FN,X)+Func(FN,X+DX))/2.0*DX;  X:= X+DX];
RlOut(0, Area);
X:= A;  Area:= 0.0;             \Simpson's rule
for I:= 1 to N do
    [Area:= Area +
        DX/6.0*(Func(FN,X) + 4.0*Func(FN,(X+X+DX)/2.0) + Func(FN,X+DX));
    X:= X+DX];
RlOut(0, Area);
CrLf(0);
];

[Format(9,6);
Integrate(0.0, 1.0, 1, 100);
Integrate(1.0, 100.0, 2, 1000);
Integrate(0.0, 5000.0, 3, 5_000_000);
Integrate(0.0, 6000.0, 3, 6_000_000);
]
```


Interestingly, the small rounding errors creep in when millions of
approximations are done. If the five and six millions are changed to five
and six thousands then the rounding errors disappear. (They could have
been hidden by using scientific notation for the output format.)
{{out}}

```txt

        0.245025        0.255025        0.249988        0.250025        0.250000
        4.654991        4.556981        4.604763        4.605986        4.605170
 12499997.500729 12500002.500729 12500000.000729 12500000.000729 12500000.000729
 17999997.001391 18000003.001391 18000000.001391 18000000.001391 18000000.001391

```



## zkl

{{trans|D}}

```zkl
fcn integrate(F,f,a,b,steps){
   h:=(b - a) / steps;
   h*(0).reduce(steps,'wrap(s,i){ F(f, h*i + a, h) + s },0.0);
}
 
fcn rectangularLeft(f,x)    { f(x) }
fcn rectangularMiddle(f,x,h){ f(x+h/2) }
fcn rectangularRight(f,x,h) { f(x+h) }
fcn trapezium(f,x,h)        { (f(x) + f(x+h))/2 }
fcn simpson(f,x,h)	    { (f(x) + 4.0*f(x+h/2) + f(x+h))/6 }
 
args:=T( T(fcn(x){ x.pow(3) }, 0.0, 1.0,   10),
         T(fcn(x){ 1.0 / x },  1.0, 100.0, 1000),
         T(fcn(x){ x },        0.0, 5000.0, 0d5_000_000),
         T(fcn(x){ x },        0.0, 6000.0, 0d6_000_000) );
fs:=T(rectangularLeft,rectangularMiddle,rectangularRight,
      trapezium,simpson);
names:=fs.pump(List,"name",'+(":"),"%-18s".fmt);

foreach a in (args){
   names.zipWith('wrap(nm,f){
      "%s %f".fmt(nm,integrate(f,a.xplode())).println() }, fs);
   println();
}
```

{{out}}

```txt

rectangularLeft:   0.202500
rectangularMiddle: 0.248750
rectangularRight:  0.302500
trapezium:         0.252500
simpson:           0.250000

rectangularLeft:   4.654991
rectangularMiddle: 4.604763
rectangularRight:  4.556981
trapezium:         4.605986
simpson:           4.605170

rectangularLeft:   12499997.500000
rectangularMiddle: 12500000.000000
rectangularRight:  12500002.500000
trapezium:         12500000.000000
simpson:           12500000.000000

rectangularLeft:   17999997.000000
rectangularMiddle: 18000000.000000
rectangularRight:  18000003.000000
trapezium:         18000000.000000
simpson:           18000000.000000

```



{{omit from|GUISS}}
{{omit from|M4}}

[[Category:Arithmetic]]
[[Category:Mathematics]]
