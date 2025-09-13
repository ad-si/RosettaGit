+++
title = "Runge-Kutta method"
description = ""
date = 2019-04-23T20:24:14Z
aliases = []
[extra]
id = 11520
[taxonomies]
categories = ["task"]
tags = []
+++

Given the example Differential equation:
:<math>y'(t) = t \times \sqrt {y(t)}</math>
With initial condition:
:<math>t_0 = 0</math> and <math>y_0 = y(t_0) = y(0) = 1</math>
This equation has an exact solution:
:<math>y(t) = \tfrac{1}{16}(t^2 +4)^2</math>
## Task

Demonstrate the commonly used explicit   [[wp:Runge–Kutta_methods#Common_fourth-order_Runge.E2.80.93Kutta_method|fourth-order Runge–Kutta method]]   to solve the above differential equation.
* Solve the given differential equation over the range <math>t = 0 \ldots 10</math> with a step value of <math>\delta t=0.1</math> (101 total points, the first being given)
* Print the calculated values of <math>y</math> at whole numbered <math>t</math>'s (<math>0.0, 1.0, \ldots 10.0</math>) along with error as compared to the exact solution.
;Method summary
Starting with a given <math>y_n</math> and <math>t_n</math> calculate:
:<math>\delta y_1 = \delta t\times y'(t_n, y_n)\quad</math>
:<math>\delta y_2 = \delta t\times y'(t_n + \tfrac{1}{2}\delta t , y_n + \tfrac{1}{2}\delta y_1)</math>
:<math>\delta y_3 = \delta t\times y'(t_n + \tfrac{1}{2}\delta t , y_n + \tfrac{1}{2}\delta y_2)</math>
:<math>\delta y_4 = \delta t\times y'(t_n + \delta t , y_n + \delta y_3)\quad</math>
then:
:<math>y_{n+1} = y_n + \tfrac{1}{6} (\delta y_1 + 2\delta y_2 + 2\delta y_3 + \delta y_4)</math>
:<math>t_{n+1} = t_n + \delta t\quad</math>


## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
procedure RungeKutta is
   type Floaty is digits 15;
   type Floaty_Array is array (Natural range <>) of Floaty;
   package FIO is new Ada.Text_IO.Float_IO(Floaty); use FIO;
   type Derivative is access function(t, y : Floaty) return Floaty;
   package Math is new Ada.Numerics.Generic_Elementary_Functions (Floaty);
   function calc_err (t, calc : Floaty) return Floaty;

   procedure Runge (yp_func : Derivative; t, y : in out Floaty_Array;
                    dt : Floaty) is
      dy1, dy2, dy3, dy4 : Floaty;
   begin
      for n in t'First .. t'Last-1 loop
         dy1 := dt * yp_func(t(n), y(n));
         dy2 := dt * yp_func(t(n) + dt / 2.0, y(n) + dy1 / 2.0);
         dy3 := dt * yp_func(t(n) + dt / 2.0, y(n) + dy2 / 2.0);
         dy4 := dt * yp_func(t(n) + dt, y(n) + dy3);
         t(n+1) := t(n) + dt;
         y(n+1) := y(n) + (dy1 + 2.0 * (dy2 + dy3) + dy4) / 6.0;
      end loop;
   end Runge;

   procedure Print (t, y : Floaty_Array; modnum : Positive) is begin
      for i in t'Range loop
         if i mod modnum = 0 then
            Put("y(");   Put (t(i), Exp=>0, Fore=>0, Aft=>1);
            Put(") = "); Put (y(i), Exp=>0, Fore=>0, Aft=>8);
            Put(" Error:"); Put (calc_err(t(i),y(i)), Aft=>5);
            New_Line;
         end if;
      end loop;
   end Print;

   function yprime (t, y : Floaty) return Floaty is begin
      return t * Math.Sqrt (y);
   end yprime;
   function calc_err (t, calc : Floaty) return Floaty is
      actual : constant Floaty := (t**2 + 4.0)**2 / 16.0;
   begin return abs(actual-calc);
   end calc_err;

   dt : constant Floaty := 0.10;
   N : constant Positive := 100;
   t_arr, y_arr : Floaty_Array(0 .. N);
begin
   t_arr(0) := 0.0;
   y_arr(0) := 1.0;
   Runge (yprime'Access, t_arr, y_arr, dt);
   Print (t_arr, y_arr, 10);
end RungeKutta;
```

```txt
y(0.0) = 1.00000000 Error: 0.00000E+00
y(1.0) = 1.56249985 Error: 1.45722E-07
y(2.0) = 3.99999908 Error: 9.19479E-07
y(3.0) = 10.56249709 Error: 2.90956E-06
y(4.0) = 24.99999377 Error: 6.23491E-06
y(5.0) = 52.56248918 Error: 1.08197E-05
y(6.0) = 99.99998341 Error: 1.65946E-05
y(7.0) = 175.56247648 Error: 2.35177E-05
y(8.0) = 288.99996843 Error: 3.15652E-05
y(9.0) = 451.56245928 Error: 4.07232E-05
y(10.0) = 675.99994902 Error: 5.09833E-05
```


## ALGOL 68


```ALGOL68

BEGIN
   PROC rk4 = (PROC (REAL, REAL) REAL f, REAL y, x, dx) REAL :
   BEGIN  CO Fourth-order Runge-Kutta method CO
      REAL dy1 = dx * f(x, y);
      REAL dy2 = dx * f(x + dx / 2.0, y + dy1 / 2.0);
      REAL dy3 = dx * f(x + dx / 2.0, y + dy2 / 2.0);
      REAL dy4 = dx * f(x + dx, y + dy3);
      y + (dy1 + 2.0 * dy2 + 2.0 * dy3 + dy4) / 6.0
   END;
   REAL x0 = 0, x1 = 10, y0 = 1.0;			CO Boundary conditions. CO
   REAL dx = 0.1;					CO Step size. CO
   INT num points = ENTIER ((x1 - x0) / dx + 0.5);	CO Add 0.5 for rounding errors. CO
   [0:num points]REAL y;   y[0] := y0;			CO Grid and starting point.CO
   PROC dy by dx = (REAL x, y) REAL : x * sqrt(y);	CO Differential equation. CO
   FOR i TO num points
   DO
      y[i] := rk4 (dy by dx, y[i-1], x0 + dx * (i - 1), dx)
   OD;
   print (("   x              true y         calc y       relative error", newline));
   FOR i FROM 0 BY 10 TO  num points
   DO
      REAL x = x0 + dx * i;
      REAL true y = (x * x + 4.0) ^ 2 / 16.0;
      printf (($3(-zzd.7dxxx), -d.4de-ddl$, x, true y, y[i], y[i] / true y - 1.0))
   OD
END

```

```txt

   x              true y         calc y       relative error
   0.0000000      1.0000000      1.0000000    0.0000e 00
   1.0000000      1.5625000      1.5624999   -9.3262e-08
   2.0000000      4.0000000      3.9999991   -2.2987e-07
   3.0000000     10.5625000     10.5624971   -2.7546e-07
   4.0000000     25.0000000     24.9999938   -2.4940e-07
   5.0000000     52.5625000     52.5624892   -2.0584e-07
   6.0000000    100.0000000     99.9999834   -1.6595e-07
   7.0000000    175.5625000    175.5624765   -1.3396e-07
   8.0000000    289.0000000    288.9999684   -1.0922e-07
   9.0000000    451.5625000    451.5624593   -9.0183e-08
  10.0000000    676.0000000    675.9999490   -7.5419e-08

```



## APL


```APL

      ∇RK4[⎕]∇
    ∇
[0]   Z←R(Y¯ RK4)Y;T;YN;TN;∆T;∆Y1;∆Y2;∆Y3;∆Y4
[1]   (T R ∆T)←R
[2]  LOOP:→(R≤TN←¯1↑T)/EXIT
[3]   ∆Y1←∆T×TN Y¯ YN←¯1↑Y
[4]   ∆Y2←∆T×(TN+∆T÷2)Y¯ YN+∆Y1÷2
[5]   ∆Y3←∆T×(TN+∆T÷2)Y¯ YN+∆Y2÷2
[6]   ∆Y4←∆T×(TN+∆T)Y¯ YN+∆Y3
[7]   Y←Y,YN+(∆Y1+(2×∆Y2)+(2×∆Y3)+∆Y4)÷6
[8]   T←T,TN+∆T
[9]   →LOOP
[10] EXIT:Z←T,[⎕IO+.5]Y
    ∇

      ∇PRINT[⎕]∇
    ∇
[0]   PRINT;TABLE
[1]   TABLE←0 10 .1({⍺×⍵*.5}RK4)1
[2]   ⎕←'T' 'RK4 Y' 'ERROR'⍪TABLE,TABLE[;2]-{((4+⍵*2)*2)÷16}TABLE[;1]
    ∇

```

```txt

      PRINT
    T           RK4 Y              ERROR
  0       1               0.000000000E0
  0.1     1.005006249    ¯1.303701147E¯9
  0.2     1.020099995    ¯5.215366805E¯9
  0.3     1.045506238    ¯1.174457109E¯8
  0.4     1.081599979    ¯2.093284546E¯8
  0.5     1.128906217    ¯3.288601591E¯8
  0.6     1.188099952    ¯4.780736740E¯8
  0.7     1.260006184    ¯6.602350622E¯8
  0.8     1.345599912    ¯8.799725681E¯8
  0.9     1.446006136    ¯1.143253423E¯7
  . . .

```



## AWK


```AWK

# syntax: GAWK -f RUNGE-KUTTA_METHOD.AWK
# converted from BBC BASIC
BEGIN {
    print(" t    y         error")
    y = 1
    for (i=0; i<=100; i++) {
      t = i / 10
      if (t == int(t)) {
        actual = ((t^2+4)^2) / 16
        printf("%2d %12.7f %g\n",t,y,actual-y)
      }
      k1 = t * sqrt(y)
      k2 = (t + 0.05) * sqrt(y + 0.05 * k1)
      k3 = (t + 0.05) * sqrt(y + 0.05 * k2)
      k4 = (t + 0.10) * sqrt(y + 0.10 * k3)
      y += 0.1 * (k1 + 2 * (k2 + k3) + k4) / 6
    }
    exit(0)
}

```

```txt

 t    y         error
 0    1.0000000 0
 1    1.5624999 1.45722e-007
 2    3.9999991 9.19479e-007
 3   10.5624971 2.90956e-006
 4   24.9999938 6.23491e-006
 5   52.5624892 1.08197e-005
 6   99.9999834 1.65946e-005
 7  175.5624765 2.35177e-005
 8  288.9999684 3.15652e-005
 9  451.5624593 4.07232e-005
10  675.9999490 5.09833e-005

```



## BASIC

=
## BBC BASIC
=

```bbcbasic
      y = 1.0
      FOR i% = 0 TO 100
        t = i% / 10

        IF t = INT(t) THEN
          actual = ((t^2 + 4)^2) / 16
          PRINT "y("; t ") = "; y TAB(20) "Error = ";  actual - y
        ENDIF

        k1 =  t * SQR(y)
        k2 = (t + 0.05) * SQR(y + 0.05 * k1)
        k3 = (t + 0.05) * SQR(y + 0.05 * k2)
        k4 = (t + 0.10) * SQR(y + 0.10 * k3)
        y += 0.1 * (k1 + 2 * (k2 + k3) + k4) / 6
      NEXT i%
```

```txt
y(0) = 1            Error = 0
y(1) = 1.56249985   Error = 1.45721892E-7
y(2) = 3.99999908   Error = 9.19479201E-7
y(3) = 10.5624971   Error = 2.90956245E-6
y(4) = 24.9999938   Error = 6.23490936E-6
y(5) = 52.5624892   Error = 1.08196974E-5
y(6) = 99.9999834   Error = 1.65945964E-5
y(7) = 175.562476   Error = 2.35177287E-5
y(8) = 288.999968   Error = 3.15652015E-5
y(9) = 451.562459   Error = 4.07231605E-5
y(10) = 675.999949  Error = 5.09832905E-5

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Runge.bas"
110 LET Y=1
120 FOR T=0 TO 10 STEP .1
130   IF T=INT(T) THEN PRINT "y(";STR$(T);") =";Y;TAB(21);"Error =";((T^2+4)^2)/16-Y
140   LET K1=T*SQR(Y)
150   LET K2=(T+.05)*SQR(Y+.05*K1)
160   LET K3=(T+.05)*SQR(Y+.05*K2)
170   LET K4=(T+.1)*SQR(Y+.1*K3)
180   LET Y=Y+.1*(K1+2*(K2+K3)+K4)/6
190 NEXT
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double rk4(double(*f)(double, double), double dx, double x, double y)
{
	double	k1 = dx * f(x, y),
		k2 = dx * f(x + dx / 2, y + k1 / 2),
		k3 = dx * f(x + dx / 2, y + k2 / 2),
		k4 = dx * f(x + dx, y + k3);
	return y + (k1 + 2 * k2 + 2 * k3 + k4) / 6;
}

double rate(double x, double y)
{
	return x * sqrt(y);
}

int main(void)
{
	double *y, x, y2;
	double x0 = 0, x1 = 10, dx = .1;
	int i, n = 1 + (x1 - x0)/dx;
	y = (double *)malloc(sizeof(double) * n);

	for (y[0] = 1, i = 1; i < n; i++)
		y[i] = rk4(rate, dx, x0 + dx * (i - 1), y[i-1]);

	printf("x\ty\trel. err.\n------------\n");
	for (i = 0; i < n; i += 10) {
		x = x0 + dx * i;
		y2 = pow(x * x / 4 + 1, 2);
		printf("%g\t%g\t%g\n", x, y[i], y[i]/y2 - 1);
	}

	return 0;
}
```

{{out}} (errors are relative)

```txt

x       y       rel. err.
------------
0       1       0
1       1.5625  -9.3262e-08
2       4       -2.2987e-07
3       10.5625 -2.75462e-07
4       25      -2.49396e-07
5       52.5625 -2.05844e-07
6       100     -1.65946e-07
7       175.562 -1.33956e-07
8       289     -1.09222e-07
9       451.562 -9.01828e-08
10      676     -7.54191e-08

```



## C#


```c#

using System;

namespace RungeKutta
{
    class Program
    {
        static void Main(string[] args)
        {
            //Incrementers to pass into the known solution
            double t = 0.0;
            double T = 10.0;
            double dt = 0.1;

            // Assign the number of elements needed for the arrays
            int n = (int)(((T - t) / dt)) + 1;

            // Initialize the arrays for the time index 's' and estimates 'y' at each index 'i'
            double[] y = new double[n];
            double[] s = new double[n];

            // RK4 Variables
            double dy1;
            double dy2;
            double dy3;
            double dy4;

            // RK4 Initializations
            int i = 0;
            s[i] = 0.0;
            y[i] = 1.0;

            Console.WriteLine("
### ===============================
 ");
            Console.WriteLine(" Beging 4th Order Runge Kutta Method ");
            Console.WriteLine("
### ===============================
 ");

            Console.WriteLine();
            Console.WriteLine(" Given the example Differential equation: \n");
            Console.WriteLine("     y' = t*sqrt(y) \n");
            Console.WriteLine(" With the initial conditions: \n");
            Console.WriteLine("     t0 = 0" + ", y(0) = 1.0 \n");
            Console.WriteLine(" Whose exact solution is known to be: \n");
            Console.WriteLine("     y(t) = 1/16*(t^2 + 4)^2 \n");
            Console.WriteLine(" Solve the given equations over the range t = 0...10 with a step value dt = 0.1 \n");
            Console.WriteLine(" Print the calculated values of y at whole numbered t's (0.0,1.0,...10.0) along with the error \n");
            Console.WriteLine();

            Console.WriteLine(" y(t) " +"RK4" + " ".PadRight(18) + "Absolute Error");
            Console.WriteLine(" -------------------------------------------------");
            Console.WriteLine(" y(0) " + y[i] + " ".PadRight(20) + (y[i] - solution(s[i])));

            // Iterate and implement the Rk4 Algorithm
            while (i < y.Length - 1)
            {

                dy1 = dt * equation(s[i], y[i]);
                dy2 = dt * equation(s[i] + dt / 2, y[i] + dy1 / 2);
                dy3 = dt * equation(s[i] + dt / 2, y[i] + dy2 / 2);
                dy4 = dt * equation(s[i] + dt, y[i] + dy3);

                s[i + 1] = s[i] + dt;
                y[i + 1] = y[i] + (dy1 + 2 * dy2 + 2 * dy3 + dy4) / 6;

                double error = Math.Abs(y[i + 1] - solution(s[i + 1]));
                double t_rounded = Math.Round(t + dt, 2);

                if (t_rounded % 1 == 0)
                {
                    Console.WriteLine(" y(" + t_rounded + ")" + " " + y[i + 1] + " ".PadRight(5) + (error));
                }

                i++;
                t += dt;

            };//End Rk4

            Console.ReadLine();
        }

        // Differential Equation
        public static double equation(double t, double y)
        {
            double y_prime;
            return y_prime = t*Math.Sqrt(y);
        }

        // Exact Solution
        public static double solution(double t)
        {
            double actual;
            actual = Math.Pow((Math.Pow(t, 2) + 4), 2)/16;
            return actual;
        }
    }
}
```



## C++

Using Lambdas

```cpp
/*
 * compiled with gcc 5.4:
 * g++-mp-5 -std=c++14 -o rk4 rk4.cc
 *
 */
# include <iostream>
# include <math.h>
using namespace std;

auto rk4(double f(double, double))
{
        return
        [       f            ](double t, double y, double dt ) -> double{ return
        [t,y,dt,f            ](                    double dy1) -> double{ return
        [t,y,dt,f,dy1        ](                    double dy2) -> double{ return
        [t,y,dt,f,dy1,dy2    ](                    double dy3) -> double{ return
        [t,y,dt,f,dy1,dy2,dy3](                    double dy4) -> double{ return
        ( dy1 + 2*dy2 + 2*dy3 + dy4 ) / 6   ;} (
        dt * f( t+dt  , y+dy3   )          );} (
        dt * f( t+dt/2, y+dy2/2 )          );} (
        dt * f( t+dt/2, y+dy1/2 )          );} (
        dt * f( t     , y       )          );} ;
}

int main(void)
{
        const double TIME_MAXIMUM = 10.0, WHOLE_TOLERANCE = 1e-12 ;
        const double T_START = 0.0, Y_START = 1.0, DT = 0.10;

        auto eval_diff_eqn = [               ](double t, double y)->double{ return t*sqrt(y)                         ; } ;
        auto eval_solution = [               ](double t          )->double{ return pow(t*t+4,2)/16                   ; } ;
        auto find_error    = [eval_solution  ](double t, double y)->double{ return fabs(y-eval_solution(t))          ; } ;
        auto is_whole      = [WHOLE_TOLERANCE](double t          )->bool  { return fabs(t-round(t)) < WHOLE_TOLERANCE; } ;

        auto dy = rk4( eval_diff_eqn ) ;

        double y = Y_START, t = T_START ;

        while(t <= TIME_MAXIMUM) {
          if (is_whole(t)) { printf("y(%4.1f)\t=%12.6f \t error: %12.6e\n",t,y,find_error(t,y)); }
          y += dy(t,y,DT) ; t += DT;
        }
        return 0;
}
```



## Common Lisp



```lisp
(defun runge-kutta (f x y x-end n)
    (let ((h (float (/ (- x-end x) n) 1d0))
          k1 k2 k3 k4)
        (setf x (float x 1d0)
              y (float y 1d0))
        (cons (cons x y)
            (loop for i below n do
                (setf k1 (* h (funcall f x y))
                      k2 (* h (funcall f (+ x (* 0.5d0 h)) (+ y (* 0.5d0 k1))))
                      k3 (* h (funcall f (+ x (* 0.5d0 h)) (+ y (* 0.5d0 k2))))
                      k4 (* h (funcall f (+ x h) (+ y k3)))
                      x (+ x h)
                      y (+ y (/ (+ k1 k2 k2 k3 k3 k4) 6)))
                collect (cons x y)))))

(let ((sol (runge-kutta (lambda (x y) (* x (sqrt y))) 0 1 10 100)))
    (loop for n from 0
          for (x . y) in sol
          when (zerop (mod n 10))
          collect (list x y (- y (/ (expt (+ 4 (* x x)) 2) 16)))))

((0.0d0 1.0d0 0.0d0)
 (0.9999999999999999d0 1.562499854278108d0 -1.4572189210859676d-7)
 (2.0000000000000004d0 3.9999990805207988d0 -9.194792029987298d-7)
 (3.0000000000000013d0 10.562497090437557d0 -2.9095624576314094d-6)
 (4.000000000000002d0 24.999993765090643d0 -6.234909392333066d-6)
 (4.999999999999998d0 52.56248918030259d0 -1.081969734428867d-5)
 (5.999999999999995d0 99.9999834054036d0 -1.659459609015812d-5)
 (6.999999999999991d0 175.56247648227117d0 -2.3517728038768837d-5)
 (7.999999999999988d0 288.9999684347983d0 -3.156520000402452d-5)
 (8.999999999999984d0 451.56245927683887d0 -4.072315812209126d-5)
 (9.99999999999998d0 675.9999490167083d0 -5.0983286655537086d-5))
```



## Crystal

```ruby
y, t = 1, 0
while t <= 10
   k1	=  t         * Math.sqrt(y)
   k2	= (t + 0.05) * Math.sqrt(y + 0.05 * k1)
   k3	= (t + 0.05) * Math.sqrt(y + 0.05 * k2)
   k4	= (t + 0.1)  * Math.sqrt(y + 0.1  * k3)

   printf("y(%4.1f)\t= %12.6f \t error: %12.6e\n", t, y, (((t**2 + 4)**2 / 16) - y )) if (t.round - t).abs < 1.0e-5
   y += 0.1 * (k1 + 2 * (k2 + k3) + k4) / 6
   t += 0.1
end

```


```txt

y( 0.0) =     1.000000   error: 0.000000e+00
y( 1.0) =     1.562500   error: 1.457219e-07
y( 2.0) =     3.999999   error: 9.194792e-07
y( 3.0) =    10.562497   error: 2.909562e-06
y( 4.0) =    24.999994   error: 6.234909e-06
y( 5.0) =    52.562489   error: 1.081970e-05
y( 6.0) =    99.999983   error: 1.659460e-05
y( 7.0) =   175.562476   error: 2.351773e-05
y( 8.0) =   288.999968   error: 3.156520e-05
y( 9.0) =   451.562459   error: 4.072316e-05
y(10.0) =   675.999949   error: 5.098329e-05

```



## D

```d
import std.stdio, std.math, std.typecons;

alias FP = real;
alias FPs = Typedef!(FP[101]);

void runge(in FP function(in FP, in FP)
           pure nothrow @safe @nogc yp_func,
           ref FPs t, ref FPs y, in FP dt) pure nothrow @safe @nogc {
    foreach (immutable n; 0 .. t.length - 1) {
        immutable FP
            dy1 = dt * yp_func(t[n], y[n]),
            dy2 = dt * yp_func(t[n] + dt / 2.0, y[n] + dy1 / 2.0),
            dy3 = dt * yp_func(t[n] + dt / 2.0, y[n] + dy2 / 2.0),
            dy4 = dt * yp_func(t[n] + dt, y[n] + dy3);
        t[n + 1] = t[n] + dt;
        y[n + 1] = y[n] + (dy1 + 2.0 * (dy2 + dy3) + dy4) / 6.0;
    }
}

FP calc_err(in FP t, in FP calc) pure nothrow @safe @nogc {
    immutable FP actual = (t ^^ 2 + 4.0) ^^ 2 / 16.0;
    return abs(actual - calc);
}

void main() {
    enum FP dt = 0.10;
    FPs t_arr, y_arr;

    t_arr[0] = 0.0;
    y_arr[0] = 1.0;
    runge((t, y) => t * y.sqrt, t_arr, y_arr, dt);

    foreach (immutable i; 0 .. t_arr.length)
        if (i % 10 == 0)
            writefln("y(%.1f) = %.8f Error: %.6g",
                     t_arr[i], y_arr[i],
                     calc_err(t_arr[i], y_arr[i]));
}
```

```txt
y(0.0) = 1.00000000 Error: 0
y(1.0) = 1.56249985 Error: 1.45722e-07
y(2.0) = 3.99999908 Error: 9.19479e-07
y(3.0) = 10.56249709 Error: 2.90956e-06
y(4.0) = 24.99999377 Error: 6.23491e-06
y(5.0) = 52.56248918 Error: 1.08197e-05
y(6.0) = 99.99998341 Error: 1.65946e-05
y(7.0) = 175.56247648 Error: 2.35177e-05
y(8.0) = 288.99996843 Error: 3.15652e-05
y(9.0) = 451.56245928 Error: 4.07232e-05
y(10.0) = 675.99994902 Error: 5.09833e-05
```



## Dart


```dart
import 'dart:math' as Math;

num RungeKutta4(Function f, num t, num y, num dt){
  num k1 = dt * f(t,y);
  num k2 = dt * f(t+0.5*dt, y + 0.5*k1);
  num k3 = dt * f(t+0.5*dt, y + 0.5*k2);
  num k4 = dt * f(t + dt, y + k3);
  return y + (1/6) * (k1 + 2*k2 + 2*k3 + k4);
}

void main(){
  num t  = 0;
  num dt = 0.1;
  num tf = 10;
  num totalPoints = ((tf-t)/dt).floor()+1;
  num y  = 1;
  Function f  = (num t, num y) => t * Math.sqrt(y);
  Function actual = (num t) => (1/16) * (t*t+4)*(t*t+4);
  for (num i = 0; i <= totalPoints; i++){
    num relativeError = (actual(t) - y)/actual(t);
    if (i%10 == 0){
      print('y(${t.round().toStringAsPrecision(3)}) = ${y.toStringAsPrecision(11)}  Error = ${relativeError.toStringAsPrecision(11)}');
    }
    y  = RungeKutta4(f, t, y, dt);
    t += dt;
  }
}
```

```txt

y(0.00) = 1.0000000000  Error = 0.0000000000
y(1.00) = 1.5624998543  Error = 9.3262010950e-8
y(2.00) = 3.9999990805  Error = 2.2986980086e-7
y(3.00) = 10.562497090  Error = 2.7546153479e-7
y(4.00) = 24.999993765  Error = 2.4939637555e-7
y(5.00) = 52.562489180  Error = 2.0584442034e-7
y(6.00) = 99.999983405  Error = 1.6594596090e-7
y(7.00) = 175.56247648  Error = 1.3395644308e-7
y(8.00) = 288.99996843  Error = 1.0922214534e-7
y(9.00) = 451.56245928  Error = 9.0182772312e-8
y(10.0) = 675.99994902  Error = 7.5419063100e-8

```



## ERRE


```ERRE

PROGRAM RUNGE_KUTTA

CONST DELTA_T=0.1

FUNCTION Y1(T,Y)
     Y1=T*SQR(Y)
END FUNCTION

BEGIN
   Y=1.0
   FOR I%=0 TO 100 DO
      T=I%*DELTA_T

      IF T=INT(T) THEN           ! print every tenth
          ACTUAL=((T^2+4)^2)/16  ! exact solution
          PRINT("Y(";T;")=";Y;TAB(20);"Error=";ACTUAL-Y)
      END IF

      K1=Y1(T,Y)
      K2=Y1(T+DELTA_T/2,Y+DELTA_T/2*K1)
      K3=Y1(T+DELTA_T/2,Y+DELTA_T/2*K2)
      K4=Y1(T+DELTA_T,Y+DELTA_T*K3)
      Y+=DELTA_T*(K1+2*(K2+K3)+K4)/6
   END FOR
END PROGRAM
```

```txt

Y( 0 )= 1          Error= 0
Y( 1 )= 1.5625     Error= 2.384186E-07
Y( 2 )= 3.999999   Error= 7.152558E-07
Y( 3 )= 10.5625    Error= 1.907349E-06
Y( 4 )= 25         Error= 3.814697E-06
Y( 5 )= 52.56249   Error= 7.629395E-06
Y( 6 )= 100        Error= 0
Y( 7 )= 175.5625   Error= 0
Y( 8 )= 289        Error= 0
Y( 9 )= 451.5625   Error= 0
Y( 10 )= 676.0001  Error=-6.103516E-05

```



## F Sharp

```F Sharp

open System

let y'(t,y) = t * sqrt(y)

let RungeKutta4 t0 y0 t_max dt =

    let dy1(t,y) = dt * y'(t,y)
    let dy2(t,y) = dt * y'(t+dt/2.0, y+dy1(t,y)/2.0)
    let dy3(t,y) = dt * y'(t+dt/2.0, y+dy2(t,y)/2.0)
    let dy4(t,y) = dt * y'(t+dt, y+dy3(t,y))

    (t0,y0) |> Seq.unfold (fun (t,y) ->
        if ( t <= t_max) then Some((t,y), (Math.Round(t+dt, 6), y + ( dy1(t,y) + 2.0*dy2(t,y) + 2.0*dy3(t,y) + dy4(t,y))/6.0))
        else None
        )

let y_exact t = (pown (pown t 2 + 4.0) 2)/16.0

RungeKutta4 0.0 1.0 10.0 0.1
    |> Seq.filter (fun (t,y) -> t % 1.0 = 0.0 )
    |> Seq.iter (fun (t,y) -> Console.WriteLine("y({0})={1}\t(relative error:{2})", t, y, (y / y_exact(t))-1.0) )
```


```txt

y(0)=1			(relative error:0)
y(1)=1.56249985427811	(relative error:-9.32620110027926E-08)
y(2)=3.9999990805208	(relative error:-2.29869800194571E-07)
y(3)=10.5624970904376	(relative error:-2.75461533583155E-07)
y(4)=24.9999937650906	(relative error:-2.49396374552013E-07)
y(5)=52.5624891803026	(relative error:-2.05844421730106E-07)
y(6)=99.9999834054036	(relative error:-1.65945964192282E-07)
y(7)=175.562476482271	(relative error:-1.33956447156969E-07)
y(8)=288.999968434799	(relative error:-1.09222150213029E-07)
y(9)=451.56245927684	(relative error:-9.01827772459285E-08)
y(10)=675.99994901671	(relative error:-7.54190684348899E-08)

```



## Fortran


```fortran
program rungekutta
implicit none
real(kind=kind(1.0D0)) :: t,dt,tstart,tstop
real(kind=kind(1.0D0)) :: y,k1,k2,k3,k4
tstart =0.0D0 ; tstop =10.0D0 ; dt = 0.1D0
y = 1.0D0
t = tstart
write(6,'(A,f4.1,A,f12.8,A,es13.6)') 'y(',t,') = ',y,' Error = '&
           &,abs(y-(t**2+4.0d0)**2/16.0d0)
do; if ( t .ge. tstop ) exit
   k1 = f (t           , y                 )
   k2 = f (t+0.5D0 * dt, y +0.5D0 * dt * k1)
   k3 = f (t+0.5D0 * dt, y +0.5D0 * dt * k2)
   k4 = f (t+        dt, y +        dt * k3)
   y = y + dt *( k1 + 2.0D0 *( k2 + k3 ) + k4 )/6.0D0
   t = t + dt
   if(abs(real(nint(t))-t) .le. 1.0D-12) then
      write(6,'(A,f4.1,A,f12.8,A,es13.6)') 'y(',t,') = ',y,' Error = '&
           &,abs(y-(t**2+4.0d0)**2/16.0d0)
   end if
end do
contains
  function f (t,y)
    implicit none
    real(kind=kind(1.0D0)),intent(in) :: y,t
    real(kind=kind(1.0D0)) :: f
    f = t*sqrt(y)
  end function f
end program rungekutta

```

```txt

y( 0.0) =   1.00000000 Error =  0.000000E+00
y( 1.0) =   1.56249985 Error =  1.457219E-07
y( 2.0) =   3.99999908 Error =  9.194792E-07
y( 3.0) =  10.56249709 Error =  2.909562E-06
y( 4.0) =  24.99999377 Error =  6.234909E-06
y( 5.0) =  52.56248918 Error =  1.081970E-05
y( 6.0) =  99.99998341 Error =  1.659460E-05
y( 7.0) = 175.56247648 Error =  2.351773E-05
y( 8.0) = 288.99996843 Error =  3.156520E-05
y( 9.0) = 451.56245928 Error =  4.072316E-05
y(10.0) = 675.99994902 Error =  5.098329E-05

```


## FreeBASIC

```freebasic
' version 03-10-2015
' compile with: fbc -s console
' translation of BBC BASIC

Dim As Double y = 1, t, actual, k1, k2, k3, k4

Print

For i As Integer = 0 To 100

    t = i / 10

    If t = Int(t) Then
        actual = ((t ^ 2 + 4) ^ 2) / 16
        Print  "y("; Str(t); ") ="; y ; Tab(27); "Error = "; actual - y
    End If

    k1 = t * Sqr(y)
    k2 = (t + 0.05) * Sqr(y + 0.05 * k1)
    k3 = (t + 0.05) * Sqr(y + 0.05 * k2)
    k4 = (t + 0.10) * Sqr(y + 0.10 * k3)
    y += 0.1 * (k1 + 2 * (k2 + k3) + k4) / 6

Next i


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
y(0) = 1                  Error =  0
y(1) = 1.562499854278108  Error =  1.457218921085968e-007
y(2) = 3.999999080520799  Error =  9.194792012223729e-007
y(3) = 10.56249709043755  Error =  2.909562448749625e-006
y(4) = 24.99999376509064  Error =  6.234909363911356e-006
y(5) = 52.56248918030259  Error =  1.081969741534294e-005
y(6) = 99.99998340540358  Error =  1.659459641700778e-005
y(7) = 175.5624764822713  Error =  2.351772874931157e-005
y(8) = 288.9999684347985  Error =  3.156520148195341e-005
y(9) = 451.5624592768396  Error =  4.072316039582802e-005
y(10) = 675.9999490167097 Error =  5.098329029351589e-005
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

def tab 9

local fn dydx( x as double, y as double ) as double
end fn = x * sqr(y)

local fn exactY( x as long ) as double
end fn = ( x ^2 + 4 ) ^2 / 16

dim as long i
dim as double h, k1, k2, k3, k4, x, y, result

h = 0.1
y = 1
for i = 0 to 100
x = i * h
if x == int(x)
result = fn exactY( x )
print "y("; mid$( str$(x), 2, len(str$(x) )); ") = "; y, "Error = "; result - y
end if

k1 = h * fn dydx( x, y )
k2 = h * fn dydx( x + h / 2, y + k1 / 2 )
k3 = h * fn dydx( x + h / 2, y + k2 / 2 )
k4 = h * fn dydx( x + h,     y + k3     )

y = y + 1 / 6 * ( k1 + 2 * k2 + 2 * k3 + k4 )
next

```

Output:

```txt

y(0) =  1         Error =  0
y(1) =  1.5624998543       Error =  1.45721892e-7
y(2) =  3.9999990805       Error =  9.19479201e-7
y(3) =  10.5624970904      Error =  2.90956245e-6
y(4) =  24.9999937651      Error =  6.23490936e-6
y(5) =  52.56248918        Error =  1.08196974e-5
y(6) =  99.999983405       Error =  1.65945964e-5
y(7) =  175.562476482      Error =  2.35177287e-5
y(8) =  288.99996843       Error =  3.15652014e-5
y(9) =  451.56245928       Error =  4.07231603e-5
y(10) =  675.99994902      Error =  5.09832903e-5

```



## Go

```go
package main

import (
    "fmt"
    "math"
)

type ypFunc func(t, y float64) float64
type ypStepFunc func(t, y, dt float64) float64

// newRKStep takes a function representing a differential equation
// and returns a function that performs a single step of the forth-order
// Runge-Kutta method.
func newRK4Step(yp ypFunc) ypStepFunc {
    return func(t, y, dt float64) float64 {
        dy1 := dt * yp(t, y)
        dy2 := dt * yp(t+dt/2, y+dy1/2)
        dy3 := dt * yp(t+dt/2, y+dy2/2)
        dy4 := dt * yp(t+dt, y+dy3)
        return y + (dy1+2*(dy2+dy3)+dy4)/6
    }
}

// example differential equation
func yprime(t, y float64) float64 {
    return t * math.Sqrt(y)
}

// exact solution of example
func actual(t float64) float64 {
    t = t*t + 4
    return t * t / 16
}

func main() {
    t0, tFinal := 0, 10 // task specifies times as integers,
    dtPrint := 1        // and to print at whole numbers.
    y0 := 1.            // initial y.
    dtStep := .1        // step value.

    t, y := float64(t0), y0
    ypStep := newRK4Step(yprime)
    for t1 := t0 + dtPrint; t1 <= tFinal; t1 += dtPrint {
        printErr(t, y) // print intermediate result
        for steps := int(float64(dtPrint)/dtStep + .5); steps > 1; steps-- {
            y = ypStep(t, y, dtStep)
            t += dtStep
        }
        y = ypStep(t, y, float64(t1)-t) // adjust step to integer time
        t = float64(t1)
    }
    printErr(t, y) // print final result
}

func printErr(t, y float64) {
    fmt.Printf("y(%.1f) = %f Error: %e\n", t, y, math.Abs(actual(t)-y))
}
```

```txt

y(0.0) = 1.000000 Error: 0.000000e+00
y(1.0) = 1.562500 Error: 1.457219e-07
y(2.0) = 3.999999 Error: 9.194792e-07
y(3.0) = 10.562497 Error: 2.909562e-06
y(4.0) = 24.999994 Error: 6.234909e-06
y(5.0) = 52.562489 Error: 1.081970e-05
y(6.0) = 99.999983 Error: 1.659460e-05
y(7.0) = 175.562476 Error: 2.351773e-05
y(8.0) = 288.999968 Error: 3.156520e-05
y(9.0) = 451.562459 Error: 4.072316e-05
y(10.0) = 675.999949 Error: 5.098329e-05

```



## Groovy


```Groovy

class Runge_Kutta{
static void main(String[] args){
def y=1.0,t=0.0,counter=0;
def dy1,dy2,dy3,dy4;
def real;
while(t<=10)
{if(counter%10==0)
{real=(t*t+4)*(t*t+4)/16;
println("y("+t+")="+ y+ " Error:"+ (real-y));
}

dy1=dy(dery(y,t));
dy2=dy(dery(y+dy1/2,t+0.05));
dy3=dy(dery(y+dy2/2,t+0.05));
dy4=dy(dery(y+dy3,t+0.1));

y=y+(dy1+2*dy2+2*dy3+dy4)/6;
t=t+0.1;
counter++;
}
}
static def dery(def y,def t){return t*(Math.sqrt(y));}
static def dy(def x){return x*0.1;}
}

```

```txt

y(0.0)=1.0 Error:0.0000
y(1.0)=1.562499854278108 Error:1.4572189210859676E-7
y(2.0)=3.999999080520799 Error:9.194792007782837E-7
y(3.0)=10.562497090437551 Error:2.9095624487496252E-6
y(4.0)=24.999993765090636 Error:6.234909363911356E-6
y(5.0)=52.562489180302585 Error:1.0819697415342944E-5
y(6.0)=99.99998340540358 Error:1.659459641700778E-5
y(7.0)=175.56247648227125 Error:2.3517728749311573E-5
y(8.0)=288.9999684347986 Error:3.156520142510999E-5
y(9.0)=451.56245927683966 Error:4.07231603389846E-5
y(10.0)=675.9999490167097 Error:5.098329029351589E-5

```



## Haskell


Using GHC 7.4.1.


```haskell
dv
  :: Floating a
  => a -> a -> a
dv = (. sqrt) . (*)

fy t = 1 / 16 * (4 + t ^ 2) ^ 2

rk4
  :: (Enum a, Fractional a)
  => (a -> a -> a) -> a -> a -> a -> [(a, a)]
rk4 fd y0 a h = zip ts $ scanl (flip fc) y0 ts
  where
    ts = [a,h ..]
    fc t y =
      sum . (y :) . zipWith (*) [1 / 6, 1 / 3, 1 / 3, 1 / 6] $
      scanl
        (\k f -> h * fd (t + f * h) (y + f * k))
        (h * fd t y)
        [1 / 2, 1 / 2, 1]

task =
  mapM_
    (print . (\(x, y) -> (truncate x, y, fy x - y)))
    (filter (\(x, _) -> 0 == mod (truncate $ 10 * x) 10) $
     take 101 $ rk4 dv 1.0 0 0.1)
```


Example executed in GHCi:

```haskell
*Main> task
(0,1.0,0.0)
(1,1.5624998542781088,1.4572189122041834e-7)
(2,3.9999990805208006,9.194792029987298e-7)
(3,10.562497090437557,2.909562461184123e-6)
(4,24.999993765090654,6.234909399438493e-6)
(5,52.56248918030265,1.0819697635611192e-5)
(6,99.99998340540378,1.6594596999652822e-5)
(7,175.56247648227165,2.3517730085131916e-5)
(8,288.99996843479926,3.1565204153594095e-5)
(9,451.562459276841,4.0723166534917254e-5)
(10,675.9999490167125,5.098330132113915e-5)
```


(See [[Euler method#Haskell]] for implementation of simple general ODE-solver)


Or, disaggregated a little, and expressed in terms of a single scanl:

```haskell
rk4 :: Double -> Double -> Double -> Double
rk4 y x dx =
  let f x y = x * sqrt y
      k1 = dx * f x y
      k2 = dx * f (x + dx / 2.0) (y + k1 / 2.0)
      k3 = dx * f (x + dx / 2.0) (y + k2 / 2.0)
      k4 = dx * f (x + dx) (y + k3)
  in y + (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0

actual :: Double -> Double
actual x = (1 / 16) * (x * x + 4) * (x * x + 4)

step :: Double
step = 0.1

ixs :: [Int]
ixs = [0 .. 100]

xys :: [(Double, Double)]
xys =
  scanl
    (\(x, y) _ -> (((x * 10) + (step * 10)) / 10, rk4 y x step))
    (0.0, 1.0)
    ixs

samples :: [(Double, Double, Double)]
samples =
  zip ixs xys >>=
  (\(i, (x, y)) ->
      [ (x, y, actual x - y)
      | 0 == mod i 10 ])

main :: IO ()
main =
  (putStrLn . unlines) $
  (\(x, y, v) ->
      unwords
        [ "y" ++ justifyRight 3 ' ' ('(' : show (round x)) ++ ") = "
        , justifyLeft 19 ' ' (show y)
        , '±' : show v
        ]) <$>
  samples
  where
    justifyLeft n c s = take n (s ++ replicate n c)
    justifyRight n c s = drop (length s) (replicate n c ++ s)
```

```txt
y (0) =  1.0                 ±0.0
y (1) =  1.562499854278108   ±1.4572189210859676e-7
y (2) =  3.999999080520799   ±9.194792007782837e-7
y (3) =  10.562497090437551  ±2.9095624487496252e-6
y (4) =  24.999993765090636  ±6.234909363911356e-6
y (5) =  52.562489180302585  ±1.0819697415342944e-5
y (6) =  99.99998340540358   ±1.659459641700778e-5
y (7) =  175.56247648227125  ±2.3517728749311573e-5
y (8) =  288.9999684347986   ±3.156520142510999e-5
y (9) =  451.56245927683966  ±4.07231603389846e-5
y(10) =  675.9999490167097   ±5.098329029351589e-5
```



## J

'''Solution:'''

```j
NB.*rk4 a Solve function using Runge-Kutta method
NB. y is: y(ta) , ta , tb , tstep
NB. u is: function to solve
NB. eg: fyp rk4 1 0 10 0.1
rk4=: adverb define
 'Y0 a b h'=. 4{. y
 T=. a + i.@>:&.(%&h) b - a
 Y=. Yt=. Y0
 for_t. }: T do.
   ty=. t,Yt
   k1=. h * u ty
   k2=. h * u ty + -: h,k1
   k3=. h * u ty + -: h,k2
   k4=. h * u ty + h,k3
   Y=. Y, Yt=. Yt + (%6) * 1 2 2 1 +/@:* k1, k2, k3, k4
 end.
T ,. Y
)
```

'''Example:'''

```j
   fy=: (%16) * [: *: 4 + *:             NB. f(t,y)
   fyp=: (* %:)/                         NB. f'(t,y)
   report_whole=: (10 * i. >:10)&{       NB. report at whole-numbered t values
   report_err=: (, {: - [: fy {.)"1      NB. report errors

   report_err report_whole fyp rk4 1 0 10 0.1
 0       1           0
 1  1.5625 _1.45722e_7
 2       4 _9.19479e_7
 3 10.5625 _2.90956e_6
 4      25 _6.23491e_6
 5 52.5625 _1.08197e_5
 6     100 _1.65946e_5
 7 175.562 _2.35177e_5
 8     289 _3.15652e_5
 9 451.562 _4.07232e_5
10     676 _5.09833e_5
```


'''Alternative solution:'''

The following solution replaces the for loop as well as the calculation of the increments (ks) with an accumulating suffix.

```j
rk4=: adverb define
 'Y0 a b h'=. 4{. y
 T=. a + i.@>:&.(%&h) b-a
 (,. [: h&(u nextY)@,/\. Y0 ,~ }.)&.|. T
)

NB. nextY a Calculate Yn+1 of a function using Runge-Kutta method
NB. y is: 2-item numeric list of time t and y(t)
NB. u is: function to use
NB. x is: step size
NB. eg: 0.001 fyp nextY 0 1
nextY=: adverb define
:
 tableau=. 1 0.5 0.5, x * u y
 ks=. (x * [: u y + (* x&,))/\. tableau
 ({:y) + 6 %~ +/ 1 2 2 1 * ks
)
```


Use:
 report_err report_whole fyp rk4 1 0 10 0.1


## Java

Translation of [[Runge-Kutta_method#Ada|Ada]] via [[Runge-Kutta_method#D|D]]
```java
import static java.lang.Math.*;
import java.util.function.BiFunction;

public class RungeKutta {

    static void runge(BiFunction<Double, Double, Double> yp_func, double[] t,
            double[] y, double dt) {

        for (int n = 0; n < t.length - 1; n++) {
            double dy1 = dt * yp_func.apply(t[n], y[n]);
            double dy2 = dt * yp_func.apply(t[n] + dt / 2.0, y[n] + dy1 / 2.0);
            double dy3 = dt * yp_func.apply(t[n] + dt / 2.0, y[n] + dy2 / 2.0);
            double dy4 = dt * yp_func.apply(t[n] + dt, y[n] + dy3);
            t[n + 1] = t[n] + dt;
            y[n + 1] = y[n] + (dy1 + 2.0 * (dy2 + dy3) + dy4) / 6.0;
        }
    }

    static double calc_err(double t, double calc) {
        double actual = pow(pow(t, 2.0) + 4.0, 2) / 16.0;
        return abs(actual - calc);
    }

    public static void main(String[] args) {
        double dt = 0.10;
        double[] t_arr = new double[101];
        double[] y_arr = new double[101];
        y_arr[0] = 1.0;

        runge((t, y) -> t * sqrt(y), t_arr, y_arr, dt);

        for (int i = 0; i < t_arr.length; i++)
            if (i % 10 == 0)
                System.out.printf("y(%.1f) = %.8f Error: %.6f%n",
                        t_arr[i], y_arr[i],
                        calc_err(t_arr[i], y_arr[i]));
    }
}
```



```txt
y(0,0) = 1,00000000 Error: 0,000000
y(1,0) = 1,56249985 Error: 0,000000
y(2,0) = 3,99999908 Error: 0,000001
y(3,0) = 10,56249709 Error: 0,000003
y(4,0) = 24,99999377 Error: 0,000006
y(5,0) = 52,56248918 Error: 0,000011
y(6,0) = 99,99998341 Error: 0,000017
y(7,0) = 175,56247648 Error: 0,000024
y(8,0) = 288,99996843 Error: 0,000032
y(9,0) = 451,56245928 Error: 0,000041
y(10,0) = 675,99994902 Error: 0,000051
```



## JavaScript


### ES5


```JavaScript

function rk4(y, x, dx, f) {
    var k1 = dx * f(x, y),
        k2 = dx * f(x + dx / 2.0,   +y + k1 / 2.0),
        k3 = dx * f(x + dx / 2.0,   +y + k2 / 2.0),
        k4 = dx * f(x + dx,         +y + k3);

    return y + (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0;
}

function f(x, y) {
    return x * Math.sqrt(y);
}

function actual(x) {
    return (1/16) * (x*x+4)*(x*x+4);
}

var y = 1.0,
    x = 0.0,
    step = 0.1,
    steps = 0,
    maxSteps = 101,
    sampleEveryN = 10;

while (steps < maxSteps) {
    if (steps%sampleEveryN === 0) {
        console.log("y(" + x + ") =  \t" + y + "\t ± " + (actual(x) - y).toExponential());
    }

    y = rk4(y, x, step, f);

    // using integer math for the step addition
    // to prevent floating point errors as 0.2 + 0.1 != 0.3
    x = ((x * 10) + (step * 10)) / 10;
    steps += 1;
}

```

```txt

y(0) =  	1	                 ± 0e+0
y(1) =  	1.562499854278108	 ± 1.4572189210859676e-7
y(2) =  	3.999999080520799	 ± 9.194792007782837e-7
y(3) =  	10.562497090437551	 ± 2.9095624487496252e-6
y(4) =  	24.999993765090636	 ± 6.234909363911356e-6
y(5) =  	52.562489180302585	 ± 1.0819697415342944e-5
y(6) =  	99.99998340540358	 ± 1.659459641700778e-5
y(7) =  	175.56247648227125	 ± 2.3517728749311573e-5
y(8) =  	288.9999684347986	 ± 3.156520142510999e-5
y(9) =  	451.56245927683966	 ± 4.07231603389846e-5
y(10) =  	675.9999490167097	 ± 5.098329029351589e-5

```



### ES6


```javascript
(() => {
  'use strict';

  // rk4 :: (Double -> Double -> Double) ->
  //          Double -> Double -> Double -> Double
  const rk4 = f => (y, x, dx) => {
    const
      k1 = dx * f(x, y),
      k2 = dx * f(x + dx / 2.0, y + k1 / 2.0),
      k3 = dx * f(x + dx / 2.0, y + k2 / 2.0),
      k4 = dx * f(x + dx, y + k3);
    return y + (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0;
  };

  // rk :: Double -> Double -> Double -> Double
  const rk = rk4((x, y) => x * Math.sqrt(y));

  // actual :: Double -> Double
  const actual = x => (1 / 16) * ((x * x) + 4) * ((x * x) + 4);


  // TEST -------------------------------------------------

  // main :: IO ()
  const main = () => {
    const
      step = 0.1,
      ixs = enumFromTo(0, 100),
      xys = scanl(
        xy => Tuple(
          ((xy[0] * 10) + (step * 10)) / 10, rk(xy[1], xy[0], step)
        ),
        Tuple(0.0, 1.0),
        ixs
      );

    // samples :: [(Double, Double, Double)]
    const samples = concatMap(
      tpl => 0 === tpl[0] % 10 ? (() => {
        const [x, y] = Array.from(tpl[1]);
        return [TupleN(x, y, actual(x) - y)];
      })() : [],
      zip(ixs, xys)
    );

    console.log(
      unlines(map(
        tpl => {
          const [x, y, v] = Array.from(tpl),
            [sn, sm] = splitOn('.', y.toString());
          return unwords([
            'y' + justifyRight(3, ' ', '(' + Math.round(x).toString()) +
            ') =',
            justifyRight(3, ' ', sn) + '.' + justifyLeft(15, ' ', sm || '0'),
            '± ' + v.toExponential()
          ]);
        },
        samples
      ))
    );
  };


  // GENERIC FUNCTIONS ----------------------------

  // Tuple (,) :: a -> b -> (a, b)
  const Tuple = (a, b) => ({
    type: 'Tuple',
    '0': a,
    '1': b,
    length: 2
  });

  // TupleN :: a -> b ...  -> (a, b ... )
  function TupleN() {
    const
      args = Array.from(arguments),
      lng = args.length;
    return lng > 1 ? Object.assign(
      args.reduce((a, x, i) => Object.assign(a, {
        [i]: x
      }), {
        type: 'Tuple' + (2 < lng ? lng.toString() : ''),
        length: lng
      })
    ) : args[0];
  };

  // concatMap :: (a -> [b]) -> [a] -> [b]
  const concatMap = (f, xs) =>
    xs.reduce((a, x) => a.concat(f(x)), []);

  // enumFromTo :: Int -> Int -> [Int]
  const enumFromTo = (m, n) =>
    Array.from({
      length: 1 + n - m
    }, (_, i) => m + i)

  // justifyLeft :: Int -> Char -> String -> String
  const justifyLeft = (n, cFiller, s) =>
    n > s.length ? (
      s.padEnd(n, cFiller)
    ) : s;

  // justifyRight :: Int -> Char -> String -> String
  const justifyRight = (n, cFiller, s) =>
    n > s.length ? (
      s.padStart(n, cFiller)
    ) : s;

  // Returns Infinity over objects without finite length
  // this enables zip and zipWith to choose the shorter
  // argument when one is non-finite, like cycle, repeat etc

  // length :: [a] -> Int
  const length = xs => xs.length || Infinity;

  // map :: (a -> b) -> [a] -> [b]
  const map = (f, xs) => xs.map(f);

  // scanl :: (b -> a -> b) -> b -> [a] -> [b]
  const scanl = (f, startValue, xs) =>
    xs.reduce((a, x) => {
      const v = f(a[0], x);
      return Tuple(v, a[1].concat(v));
    }, Tuple(startValue, [startValue]))[1];

  // splitOn :: String -> String -> [String]
  const splitOn = (pat, src) => src.split(pat);

  // take :: Int -> [a] -> [a]
  // take :: Int -> String -> String
  const take = (n, xs) =>
    xs.constructor.constructor.name !== 'GeneratorFunction' ? (
      xs.slice(0, n)
    ) : [].concat.apply([], Array.from({
      length: n
    }, () => {
      const x = xs.next();
      return x.done ? [] : [x.value];
    }));

  // unlines :: [String] -> String
  const unlines = xs => xs.join('\n');

  // unwords :: [String] -> String
  const unwords = xs => xs.join(' ');

  // Use of `take` and `length` here allows for zipping with non-finite
  // lists - i.e. generators like cycle, repeat, iterate.

  // zip :: [a] -> [b] -> [(a, b)]
  const zip = (xs, ys) => {
    const lng = Math.min(length(xs), length(ys));
    return Infinity !== lng ? (() => {
      const bs = take(lng, ys);
      return take(lng, xs).map((x, i) => Tuple(x, bs[i]));
    })() : zipGen(xs, ys);
  };

  // MAIN ---
  return main();
})();
```

```txt
y (0) =   1.0               ± 0e+0
y (1) =   1.562499854278108 ± 1.4572189210859676e-7
y (2) =   3.999999080520799 ± 9.194792007782837e-7
y (3) =  10.562497090437551 ± 2.9095624487496252e-6
y (4) =  24.999993765090636 ± 6.234909363911356e-6
y (5) =  52.562489180302585 ± 1.0819697415342944e-5
y (6) =  99.99998340540358  ± 1.659459641700778e-5
y (7) = 175.56247648227125  ± 2.3517728749311573e-5
y (8) = 288.9999684347986   ± 3.156520142510999e-5
y (9) = 451.56245927683966  ± 4.07231603389846e-5
y(10) = 675.9999490167097   ± 5.098329029351589e-5
```



## jq

In this section, two solutions are presented.
They use "while" and/or "until" as defined in recent versions of jq (after version 1.4).
To use either of the two programs with jq 1.4, simply include the lines in the following block:

```jq
def until(cond; next):
  def _until: if cond then . else (next|_until) end;
  _until;

def while(cond; update):
  def _while:  if cond then ., (update | _while) else empty end;
  _while;
```



### The Example Differential Equation and its Exact Solution


```jq
# yprime maps [t,y] to a number, i.e. t * sqrt(y)
def yprime: .[0] * (.[1] | sqrt);

# The exact solution of yprime:
def actual:
  . as $t
  | (( $t*$t) + 4 )
  | . * . / 16;
```



### dy/dt

The first solution presented here uses the terminology and style of the Perl 6 version.

'''Generic filters:'''

```jq
# n is the number of decimal places of precision
def round(n):
  (if . < 0 then -1 else 1 end) as $s
  | $s*10*.*n | if (floor % 10) > 4 then (.+5) else . end | ./10 | floor/n | .*$s;

def abs: if . < 0 then -. else . end;

# Is the input an integer?
def integerq: ((. - ((.+.01) | floor)) | abs) < 0.01;
```


'''dy(f)'''

```jq
def dt: 0.1;

# Input: [t, y]; yp is a filter that accepts [t,y] as input
def runge_kutta(yp):
  .[0] as $t | .[1] as $y
  | (dt * yp) as $a
  | (dt * ([ ($t + (dt/2)), $y + ($a/2) ] | yp)) as $b
  | (dt * ([ ($t + (dt/2)), $y + ($b/2) ] | yp)) as $c
  | (dt * ([ ($t + dt)    , $y + $c     ] | yp)) as $d
  | ($a + (2*($b + $c)) + $d) / 6
;

# Input: [t,y]
def dy(f): runge_kutta(f);
```

''' Example''':

```jq
# state: [t,y]
[0,1]
| while( .[0] <= 10;
         .[0] as $t | .[1] as $y
         | [$t + dt, $y + dy(yprime) ] )
| .[0] as $t | .[1] as $y
| if $t | integerq then
     "y(\($t|round(1))) = \($y|round(10000)) ± \( ($t|actual) - $y | abs)"
  else empty
  end
```

```sh
$ time jq -r -n -f rk4.pl.jq
y(0) = 1 ± 0
y(1) = 1.5625 ± 1.4572189210859676e-07
y(2) = 4 ± 9.194792029987298e-07
y(3) = 10.5625 ± 2.9095624576314094e-06
y(4) = 25 ± 6.234909392333066e-06
y(5) = 52.5625 ± 1.081969734428867e-05
y(6) = 100 ± 1.659459609015812e-05
y(7) = 175.5625 ± 2.3517728038768837e-05
y(8) = 289 ± 3.156520000402452e-05
y(9) = 451.5625 ± 4.072315812209126e-05
y(10) = 675.9999 ± 5.0983286655537086e-05

real	0m0.048s
user	0m0.013s
sys	0m0.006s
```


### newRK4Step

The second solution follows the nomenclature and style of the Go solution on this page.

In the following notes:
* ypFunc denotes the type of a jq filter that maps [t, y] to a number;
* ypStepFunc denotes the type of a jq filter that maps [t, y, dt] to a number.

The heart of the program is the filter newRK4Step(yp), which is of type ypStepFunc and performs a single
step of the fourth-order Runge-Kutta method, provided yp is of type ypFunc.

```jq
# Input: [t, y, dt]
def newRK4Step(yp):
  .[0] as $t | .[1] as $y | .[2] as $dt
  | ($dt * ([$t, $y]|yp))              as $dy1
  | ($dt * ([$t+$dt/2, $y+$dy1/2]|yp)) as $dy2
  | ($dt * ([$t+$dt/2, $y+$dy2/2]|yp)) as $dy3
  | ($dt * ([$t+$dt, $y+$dy3]    |yp)) as $dy4
  | $y + ($dy1+2*($dy2+$dy3)+$dy4)/6
;


def printErr: # input: [t, y]
  def abs: if . < 0 then -. else . end;
  .[0] as $t | .[1] as $y
  | "y(\($t)) = \($y) with error: \( (($t|actual) - $y) | abs )"
;

def main(t0; y0; tFinal; dtPrint):

  def ypStep: newRK4Step(yprime) ;

  0.1 as $dtStep     # step value
  # [ t, y] is the state vector
  | [ t0, y0 ]
  | while( .[0] <= tFinal;
           .[0] as $t | .[1] as $y
	   | ($t + dtPrint) as $t1
	   | (((dtPrint/$dtStep) + 0.5) | floor) as $steps
	   | [$steps, $t, $y]  # state vector
           | until( .[0] <= 1;
	            .[0] as $steps
		    | .[1] as $t
		    | .[2] as $y
		    | [ ($steps - 1), ($t + $dtStep), ([$t, $y, $dtStep]|ypStep) ]
                  )
	   | .[1] as $t | .[2] as $y
	   | [$t1, ([ $t, $y, ($t1-$t)] | ypStep)]  # adjust step to integer time
         )
   | printErr # print results
;

# main(t0; y0; tFinal; dtPrint)
main(0; 1; 10; 1)
```

```sh
$ time jq -n -r -f runge-kutta.jq
y(0) = 1 with error: 0
y(1) = 1.562499854278108 with error: 1.4572189210859676e-07
y(2) = 3.9999990805207974 with error: 9.194792025546406e-07
y(3) = 10.562497090437544 with error: 2.9095624558550526e-06
y(4) = 24.999993765090615 with error: 6.234909385227638e-06
y(5) = 52.562489180302656 with error: 1.081969734428867e-05
y(6) = 99.99998340540387 with error: 1.6594596132790684e-05
y(7) = 175.56247648227188 with error: 2.3517728124033965e-05
y(8) = 288.9999684347997 with error: 3.156520028824161e-05
y(9) = 451.56245927684154 with error: 4.0723158463151776e-05
y(10) = 675.9999490167129 with error: 5.0983287110284436e-05

real	0m0.023s
user	0m0.014s
sys	0m0.006s
```



## Julia

== Using lambda expressions ==
```julia
f(x, y) = x * sqrt(y)
theoric(t) = (t ^ 2 + 4.0) ^ 2 / 16.0

rk4(f) = (t, y, δt) ->  # 1st (result) lambda
         ((δy1) ->      # 2nd lambda
         ((δy2) ->      # 3rd lambda
         ((δy3) ->      # 4th lambda
         ((δy4) -> ( δy1 + 2δy2 + 2δy3 + δy4 ) / 6 # 5th and deepest lambda: calc y_{n+1}
         )(δt * f(t + δt, y + δy3))         # calc δy₄
         )(δt * f(t + δt / 2, y + δy2 / 2)) # calc δy₃
         )(δt * f(t + δt / 2, y + δy1 / 2)) # calc δy₂
         )(δt * f(t, y))                    # calc δy₁

δy = rk4(f)
t₀, δt, tmax = 0.0, 0.1, 10.0
y₀ = 1.0

t, y = t₀, y₀
while t ≤ tmax
    if t ≈ round(t) @printf("y(%4.1f) = %10.6f\terror: %12.6e\n", t, y, abs(y - theoric(t))) end
    y += δy(t, y, δt)
    t += δt
end
```


```txt
y( 0.0) =   1.000000	error: 0.000000e+00
y( 1.0) =   1.562500	error: 1.457219e-07
y( 2.0) =   3.999999	error: 9.194792e-07
y( 3.0) =  10.562497	error: 2.909562e-06
y( 4.0) =  24.999994	error: 6.234909e-06
y( 5.0) =  52.562489	error: 1.081970e-05
y( 6.0) =  99.999983	error: 1.659460e-05
y( 7.0) = 175.562476	error: 2.351773e-05
y( 8.0) = 288.999968	error: 3.156520e-05
y( 9.0) = 451.562459	error: 4.072316e-05
y(10.0) = 675.999949	error: 5.098329e-05
```


== Alternative version ==
```julia
function rk4(f::Function, x₀::Float64, y₀::Float64, x₁::Float64, n)
    vx = Vector{Float64}(n + 1)
    vy = Vector{Float64}(n + 1)
    vx[1] = x = x₀
    vy[1] = y = y₀
    h = (x₁ - x₀) / n
    for i in 1:n
        k₁ = h * f(x, y)
        k₂ = h * f(x + 0.5h, y + 0.5k₁)
        k₃ = h * f(x + 0.5h, y + 0.5k₂)
        k₄ = h * f(x + h, y + k₃)
        vx[i + 1] = x = x₀ + i * h
        vy[i + 1] = y = y + (k₁ + 2k₂ + 2k₃ + k₄) / 6
    end
    return vx, vy
end

vx, vy = rk4(f, 0.0, 1.0, 10.0, 100)
for (x, y) in Iterators.take(zip(vx, vy), 10)
    @printf("%4.1f %10.5f %+12.4e\n", x, y, y - theoric(x))
end
```



## Kotlin


```scala
// version 1.1.2

typealias Y  = (Double) -> Double
typealias Yd = (Double, Double) -> Double

fun rungeKutta4(t0: Double, tz: Double, dt: Double, y: Y, yd: Yd) {
    var tn = t0
    var yn = y(tn)
    val z = ((tz  - t0) / dt).toInt()
    for (i in 0..z) {
        if (i % 10 == 0) {
            val exact = y(tn)
            val error = yn - exact
            println("%4.1f  %10f  %10f  %9f".format(tn, yn, exact, error))
        }
        if (i == z) break
        val dy1 = dt * yd(tn, yn)
        val dy2 = dt * yd(tn + 0.5 * dt, yn + 0.5 * dy1)
        val dy3 = dt * yd(tn + 0.5 * dt, yn + 0.5 * dy2)
        val dy4 = dt * yd(tn + dt, yn + dy3)
        yn += (dy1 + 2.0 * dy2 + 2.0 * dy3 + dy4) / 6.0
        tn += dt
    }
}

fun main(args: Array<String>) {
    println("  T        RK4        Exact      Error")
    println("----  ----------  ----------  ---------")
    val y = fun(t: Double): Double {
        val x = t * t + 4.0
        return x * x / 16.0
    }
    val yd = fun(t: Double, yt: Double) = t * Math.sqrt(yt)
    rungeKutta4(0.0, 10.0, 0.1, y, yd)
}
```


```txt

  T        RK4        Exact      Error
----  ----------  ----------  ---------
 0.0    1.000000    1.000000   0.000000
 1.0    1.562500    1.562500  -0.000000
 2.0    3.999999    4.000000  -0.000001
 3.0   10.562497   10.562500  -0.000003
 4.0   24.999994   25.000000  -0.000006
 5.0   52.562489   52.562500  -0.000011
 6.0   99.999983  100.000000  -0.000017
 7.0  175.562476  175.562500  -0.000024
 8.0  288.999968  289.000000  -0.000032
 9.0  451.562459  451.562500  -0.000041
10.0  675.999949  676.000000  -0.000051

```



## Liberty BASIC


```lb

'[RC] Runge-Kutta method
'initial conditions
x0 = 0
y0 = 1
'step
h = 0.1
'number of points
N=101

y=y0
FOR i = 0 TO N-1
    x = x0+ i*h
    IF x = INT(x) THEN
        actual = exactY(x)
        PRINT "y("; x ;") = "; y; TAB(20); "Error = ";  actual - y
    END IF

    k1 = h*dydx(x,y)
    k2 = h*dydx(x+h/2,y+k1/2)
    k3 = h*dydx(x+h/2,y+k2/2)
    k4 = h*dydx(x+h,y+k3)
    y = y + 1/6 * (k1 + 2*k2 + 2*k3 + k4)
NEXT i

function dydx(x,y)
    dydx=x*sqr(y)
end function

function exactY(x)
    exactY=(x^2 + 4)^2 / 16
end function

```

```txt

y(0) = 1           Error = 0
y(1) = 1.56249985  Error = 0.14572189e-6
y(2) = 3.99999908  Error = 0.9194792e-6
y(3) = 10.5624971  Error = 0.29095624e-5
y(4) = 24.9999938  Error = 0.62349094e-5
y(5) = 52.5624892  Error = 0.10819697e-4
y(6) = 99.9999834  Error = 0.16594596e-4
y(7) = 175.562476  Error = 0.23517729e-4
y(8) = 288.999968  Error = 0.31565201e-4
y(9) = 451.562459  Error = 0.4072316e-4
y(10) = 675.999949 Error = 0.5098329e-4

```



## Mathematica


```Mathematica
(* Symbolic solution *)
DSolve[{y'[t] == t*Sqrt[y[t]], y[0] == 1}, y, t]
Table[{t, 1/16 (4 + t^2)^2}, {t, 0, 10}]

(* Numerical solution I (not RK4) *)
Table[{t, y[t], Abs[y[t] - 1/16*(4 + t^2)^2]}, {t, 0, 10}] /.
 First@NDSolve[{y'[t] == t*Sqrt[y[t]], y[0] == 1}, y, {t, 0, 10}]

(* Numerical solution II (RK4) *)
f[{t_, y_}] := {1, t Sqrt[y]}
h = 0.1;
phi[y_] := Module[{k1, k2, k3, k4},
  k1 = h*f[y];
  k2 = h*f[y + 1/2 k1];
  k3 = h*f[y + 1/2 k2];
  k4 = h*f[y + k3];
  y + k1/6 + k2/3 + k3/3 + k4/6]
solution = NestList[phi, {0, 1}, 101];
Table[{y[[1]], y[[2]], Abs[y[[2]] - 1/16 (y[[1]]^2 + 4)^2]},
  {y,  solution[[1 ;; 101 ;; 10]]}]

```



## MATLAB

The normally-used built-in solver is the ode45 function, which uses a non-fixed-step solver with 4th/5th order Runge-Kutta methods. The MathWorks Support Team released a [http://www.mathworks.com/matlabcentral/answers/98293-is-there-a-fixed-step-ordinary-differential-equation-ode-solver-in-matlab-8-0-r2012b#answer_107643 package of fixed-step RK method ODE solvers] on MATLABCentral. The ode4 function contained within uses a 4th-order Runge-Kutta method. Here is code that tests both ode4 and my own function, shows that they are the same, and compares them to the exact solution.

```MATLAB
function testRK4Programs
    figure
    hold on
    t = 0:0.1:10;
    y = 0.0625.*(t.^2+4).^2;
    plot(t, y, '-k')
    [tode4, yode4] = testODE4(t);
    plot(tode4, yode4, '--b')
    [trk4, yrk4] = testRK4(t);
    plot(trk4, yrk4, ':r')
    legend('Exact', 'ODE4', 'RK4')
    hold off
    fprintf('Time\tExactVal\tODE4Val\tODE4Error\tRK4Val\tRK4Error\n')
    for k = 1:10:length(t)
        fprintf('%.f\t\t%7.3f\t\t%7.3f\t%7.3g\t%7.3f\t%7.3g\n', t(k), y(k), ...
            yode4(k), abs(y(k)-yode4(k)), yrk4(k), abs(y(k)-yrk4(k)))
    end
end

function [t, y] = testODE4(t)
    y0 = 1;
    y = ode4(@(tVal,yVal)tVal*sqrt(yVal), t, y0);
end

function [t, y] = testRK4(t)
    dydt = @(tVal,yVal)tVal*sqrt(yVal);
    y = zeros(size(t));
    y(1) = 1;
    for k = 1:length(t)-1
        dt = t(k+1)-t(k);
        dy1 = dt*dydt(t(k), y(k));
        dy2 = dt*dydt(t(k)+0.5*dt, y(k)+0.5*dy1);
        dy3 = dt*dydt(t(k)+0.5*dt, y(k)+0.5*dy2);
        dy4 = dt*dydt(t(k)+dt, y(k)+dy3);
        y(k+1) = y(k)+(dy1+2*dy2+2*dy3+dy4)/6;
    end
end
```

```txt

Time	ExactVal	ODE4Val		ODE4Error	RK4Val		RK4Error
0	  1.000		  1.000		      0		  1.000		      0
1	  1.563		  1.562		1.46e-007	  1.562		1.46e-007
2	  4.000		  4.000		9.19e-007	  4.000		9.19e-007
3	 10.563		 10.562		2.91e-006	 10.562		2.91e-006
4	 25.000		 25.000		6.23e-006	 25.000		6.23e-006
5	 52.563		 52.562		1.08e-005	 52.562		1.08e-005
6	100.000		100.000		1.66e-005	100.000		1.66e-005
7	175.563		175.562		2.35e-005	175.562		2.35e-005
8	289.000		289.000		3.16e-005	289.000		3.16e-005
9	451.563		451.562		4.07e-005	451.562		4.07e-005
10	676.000		676.000		5.10e-005	676.000		5.10e-005

```



## Maxima


```maxima
/* Here is how to solve a differential equation */
'diff(y, x) = x * sqrt(y);
ode2(%, y, x);
ic1(%, x = 0, y = 1);
factor(solve(%, y)); /* [y = (x^2 + 4)^2 / 16] */

/* The Runge-Kutta solver is builtin */

load(dynamics)$
sol: rk(t * sqrt(y), y, 1, [t, 0, 10, 1.0])$
plot2d([discrete, sol])$

/* An implementation of RK4 for one equation */

rk4(f, x0, y0, x1, n) := block([h, x, y, vx, vy, k1, k2, k3, k4],
   h: bfloat((x1 - x0) / (n - 1)),
   x: x0,
   y: y0,
   vx: makelist(0, n + 1),
   vy: makelist(0, n + 1),
   vx[1]: x0,
   vy[1]: y0,
   for i from 1 thru n do (
      k1: bfloat(h * f(x, y)),
      k2: bfloat(h * f(x + h / 2, y + k1 / 2)),
      k3: bfloat(h * f(x + h / 2, y + k2 / 2)),
      k4: bfloat(h * f(x + h, y + k3)),
      vy[i + 1]: y: y + (k1 + 2 * k2 + 2 * k3 + k4) / 6,
      vx[i + 1]: x: x + h
   ),
   [vx, vy]
)$

[x, y]: rk4(lambda([x, y], x * sqrt(y)), 0, 1, 10, 101)$

plot2d([discrete, x, y])$

s: map(lambda([x], (x^2 + 4)^2 / 16), x)$

for i from 1 step 10 thru 101 do print(x[i], " ", y[i], " ", y[i] - s[i]);
```


=={{header|MK-61/52}}==

```txt
ПП	38	П1	ПП	30	П2	ПП	35	П3	2
*	ПП	30	ИП2	ИП3	+	2	*	+	ИП1
+	3	/	ИП7	+	П7	П8	С/П	БП	00
ИП6	ИП5	+	П6	<->	ИП7	+	П8

ИП8	КвКор	ИП6	*

ИП5	*	В/О
```


''Input:'' 1/2 (h/2) - Р5, 1 (y<sub>0</sub>) - Р8 and Р7, 0 (t<sub>0</sub>) - Р6.



## Nim


```nim
import math

proc fn(t, y: float): float =
    result = t * math.sqrt(y)

proc solution(t: float): float =
    result = (t^2 + 4)^2 / 16

proc rk(start, stop, step: float) =
    let nsteps = int(round((stop - start) / step)) + 1
    let delta = (stop - start) / float(nsteps - 1)
    var cur_y = 1.0
    for i in 0..(nsteps - 1):
        let cur_t = start + delta * float(i)

        if abs(cur_t - math.round(cur_t)) < 1e-5:
            echo "y(", cur_t, ") = ", cur_y, ", error = ", solution(cur_t) - cur_y

        let dy1 = step * fn(cur_t, cur_y)
        let dy2 = step * fn(cur_t + 0.5 * step, cur_y + 0.5 * dy1)
        let dy3 = step * fn(cur_t + 0.5 * step, cur_y + 0.5 * dy2)
        let dy4 = step * fn(cur_t + step, cur_y + dy3)

        cur_y += (dy1 + 2.0 * (dy2 + dy3) + dy4) / 6.0

rk(start=0.0, stop=10.0, step=0.1)
```

```txt
y(0.0) = 1.0, error = 0.0
y(1.0) = 1.562499854278108, error = 1.457218921085968e-007
y(2.0) = 3.9999990805208, error = 9.194792003341945e-007
y(3.0) = 10.56249709043755, error = 2.909562448749625e-006
y(4.0) = 24.99999376509064, error = 6.234909363911356e-006
y(5.0) = 52.56248918030259, error = 1.081969741534294e-005
y(6.0) = 99.99998340540358, error = 1.659459641700778e-005
y(7.0) = 175.5624764822713, error = 2.351772874931157e-005
y(8.0) = 288.9999684347986, error = 3.156520142510999e-005
y(9.0) = 451.5624592768397, error = 4.07231603389846e-005
y(10.0) = 675.9999490167097, error = 5.098329029351589e-005
```



## Objeck


```objeck
class RungeKuttaMethod {
  function : Main(args : String[]) ~ Nil {
    x0 := 0.0; x1 := 10.0; dx := .1;

    n := 1 + (x1 - x0)/dx;
    y := Float->New[n->As(Int)];

    y[0] := 1;
    for(i := 1; i < n; i++;) {
      y[i] := Rk4(Rate(Float, Float) ~ Float, dx, x0 + dx * (i - 1), y[i-1]);
    };

    for(i := 0; i < n; i += 10;) {
      x := x0 + dx * i;
      y2 := (x * x / 4 + 1)->Power(2.0);

      x_value := x->As(Int);
      y_value := y[i];
      rel_value := y_value/y2 - 1.0;
      "y({$x_value})={$y_value}; error: {$rel_value}"->PrintLine();
    };
  }

  function : native : Rk4(f : (Float, Float) ~ Float, dx : Float, x : Float, y : Float) ~ Float {
    k1 := dx * f(x, y);
    k2 := dx * f(x + dx / 2, y + k1 / 2);
    k3 := dx * f(x + dx / 2, y + k2 / 2);
    k4 := dx * f(x + dx, y + k3);

    return y + (k1 + 2 * k2 + 2 * k3 + k4) / 6;
  }

  function : native : Rate(x : Float, y : Float) ~ Float {
    return x * y->SquareRoot();
  }
}
```


Output:

```txt

y(0)=1.0; error: 0.0
y(1)=1.563; error: -0.0000000933
y(2)=3.1000; error: -0.000000230
y(3)=10.563; error: -0.000000275
y(4)=24.1000; error: -0.000000249
y(5)=52.563; error: -0.000000206
y(6)=99.1000; error: -0.000000166
y(7)=175.563; error: -0.000000134
y(8)=288.1000; error: -0.000000109
y(9)=451.563; error: -0.0000000902
y(10)=675.1000; error: -0.0000000754

```



## OCaml


```ocaml
let y' t y = t *. sqrt y
let exact t = let u = 0.25*.t*.t +. 1.0 in u*.u

let rk4_step (y,t) h =
  let k1 = h *. y' t y in
  let k2 = h *. y' (t +. 0.5*.h) (y +. 0.5*.k1) in
  let k3 = h *. y' (t +. 0.5*.h) (y +. 0.5*.k2) in
  let k4 = h *. y' (t +. h) (y +. k3) in
  (y +. (k1+.k4)/.6.0 +. (k2+.k3)/.3.0, t +. h)

let rec loop h n (y,t) =
  if n mod 10 = 1 then
    Printf.printf "t = %f,\ty = %f,\terr = %g\n" t y (abs_float (y -. exact t));
  if n < 102 then loop h (n+1) (rk4_step (y,t) h)

let _ = loop 0.1 1 (1.0, 0.0)
```

```txt
t = 0.000000,	y = 1.000000,	err = 0
t = 1.000000,	y = 1.562500,	err = 1.45722e-07
t = 2.000000,	y = 3.999999,	err = 9.19479e-07
t = 3.000000,	y = 10.562497,	err = 2.90956e-06
t = 4.000000,	y = 24.999994,	err = 6.23491e-06
t = 5.000000,	y = 52.562489,	err = 1.08197e-05
t = 6.000000,	y = 99.999983,	err = 1.65946e-05
t = 7.000000,	y = 175.562476,	err = 2.35177e-05
t = 8.000000,	y = 288.999968,	err = 3.15652e-05
t = 9.000000,	y = 451.562459,	err = 4.07232e-05
t = 10.000000,	y = 675.999949,	err = 5.09833e-05
```



## Octave


```octave

#Applying the Runge-Kutta method (This code must be implement on a different file than the main one).

function temp = rk4(func,x,pvi,h)
    K1 = h*func(x,pvi);
    K2 = h*func(x+0.5*h,pvi+0.5*K1);
    K3 = h*func(x+0.5*h,pvi+0.5*K2);
    K4 = h*func(x+h,pvi+K3);
    temp = pvi + (K1 + 2*K2 + 2*K3 + K4)/6;
endfunction

#Main Program.

f  = @(t) (1/16)*((t.^2 + 4).^2);
df = @(t,y) t*sqrt(y);

pvi = 1.0;
h   = 0.1;
Yn  = pvi;

for x = 0:h:10-h
    pvi = rk4(df,x,pvi,h);
    Yn = [Yn pvi];
endfor

fprintf('Time \t Exact Value \t ODE4 Value \t Num. Error\n');

for i=0:10
    fprintf('%d \t %.5f \t %.5f \t %.4g \n',i,f(i),Yn(1+i*10),f(i)-Yn(1+i*10));
endfor

```

```txt

Time     Exact Value     ODE4 Value      Num. Error
0        1.00000         1.00000         0
1        1.56250         1.56250         1.457e-007
2        4.00000         4.00000         9.195e-007
3        10.56250        10.56250        2.91e-006
4        25.00000        24.99999        6.235e-006
5        52.56250        52.56249        1.082e-005
6        100.00000       99.99998        1.659e-005
7        175.56250       175.56248       2.352e-005
8        289.00000       288.99997       3.157e-005
9        451.56250       451.56246       4.072e-005
10       676.00000       675.99995       5.098e-005
```



## PARI/GP

```parigp
rk4(f,dx,x,y)={
  my(k1=dx*f(x,y), k2=dx*f(x+dx/2,y+k1/2), k3=dx*f(x+dx/2,y+k2/2), k4=dx*f(x+dx,y+k3));
  y + (k1 + 2*k2 + 2*k3 + k4) / 6
};
rate(x,y)=x*sqrt(y);
go()={
  my(x0=0,x1=10,dx=.1,n=1+(x1-x0)\dx,y=vector(n));
  y[1]=1;
  for(i=2,n,y[i]=rk4(rate, dx, x0 + dx * (i - 1), y[i-1]));
  print("x\ty\trel. err.\n------------");
  forstep(i=1,n,10,
    my(x=x0+dx*i,y2=(x^2/4+1)^2);
    print(x "\t" y[i] "\t" y[i]/y2 - 1)
  )
};
go()
```

```txt
x       y       rel. err.
------------
0.100000000     1       -0.00498131231
1.10000000      1.68999982      -0.00383519474
2.10000000      4.40999894      -0.00237694942
3.10000000      11.5599968      -0.00146924588
4.10000000      27.0399933      -0.000961094862
5.10000000      56.2499884      -0.000666538719
6.10000000      106.089982      -0.000485427212
7.10000000      184.959975      -0.000367681962
8.10000000      302.759966      -0.000287408941
9.10000000      470.889955      -0.000230470905
```



## Pascal

This code has been compiled using Free Pascal 2.6.2.


```pascal
program RungeKuttaExample;

uses sysutils;

type
    TDerivative = function (t, y : Real) : Real;

procedure RungeKutta(yDer : TDerivative;
                     var t, y : array of Real;
                     dt   : Real);
var
    dy1, dy2, dy3, dy4 : Real;
    idx                : Cardinal;

begin
    for idx := Low(t) to High(t) - 1 do
    begin
        dy1 := dt * yDer(t[idx],            y[idx]);
        dy2 := dt * yDer(t[idx] + dt / 2.0, y[idx] + dy1 / 2.0);
        dy3 := dt * yDer(t[idx] + dt / 2.0, y[idx] + dy2 / 2.0);
        dy4 := dt * yDer(t[idx] + dt,       y[idx] + dy3);

        t[idx + 1] := t[idx] + dt;
        y[idx + 1] := y[idx] + (dy1 + 2.0 * (dy2 + dy3) + dy4) / 6.0;
    end;
end;

function CalcError(t, y : Real) : Real;
var
    trueVal : Real;

begin
    trueVal := sqr(sqr(t) + 4.0) / 16.0;
    CalcError := abs(trueVal - y);
end;

procedure Print(t, y : array of Real;
                modnum : Integer);
var
    idx : Cardinal;

begin
    for idx := Low(t) to High(t) do
    begin
        if idx mod modnum = 0 then
        begin
            WriteLn(Format('y(%4.1f) = %12.8f  Error: %12.6e',
                [t[idx], y[idx], CalcError(t[idx], y[idx])]));
        end;
    end;
end;

function YPrime(t, y : Real) : Real;
begin
    YPrime := t * sqrt(y);
end;

const
    dt = 0.10;
    N = 100;

var
    tArr, yArr : array [0..N] of Real;

begin
    tArr[0] := 0.0;
    yArr[0] := 1.0;

    RungeKutta(@YPrime, tArr, yArr, dt);
    Print(tArr, yArr, 10);
end.
```

```txt
y( 0.0) =   1.00000000  Error: 0.00000E+000
y( 1.0) =   1.56249985  Error: 1.45722E-007
y( 2.0) =   3.99999908  Error: 9.19479E-007
y( 3.0) =  10.56249709  Error: 2.90956E-006
y( 4.0) =  24.99999377  Error: 6.23491E-006
y( 5.0) =  52.56248918  Error: 1.08197E-005
y( 6.0) =  99.99998341  Error: 1.65946E-005
y( 7.0) = 175.56247648  Error: 2.35177E-005
y( 8.0) = 288.99996843  Error: 3.15652E-005
y( 9.0) = 451.56245928  Error: 4.07232E-005
y(10.0) = 675.99994902  Error: 5.09833E-005

```



## Perl


There are many ways of doing this.  Here we define the runge_kutta function
as a function of <math>y'</math> and <math>\delta t</math>, returning a closure
which itself takes <math>(t, y)</math> as argument and returns the next <math>(t, y)</math>.

Notice how we have to use sprintf to deal with floating point rounding.  See perlfaq4.

```perl
sub runge_kutta {
    my ($yp, $dt) = @_;
    sub {
	my ($t, $y) = @_;
	my @dy =  $dt * $yp->( $t        , $y );
	push @dy, $dt * $yp->( $t + $dt/2, $y + $dy[0]/2 );
	push @dy, $dt * $yp->( $t + $dt/2, $y + $dy[1]/2 );
	push @dy, $dt * $yp->( $t + $dt  , $y + $dy[2] );
	return $t + $dt, $y + ($dy[0] + 2*$dy[1] + 2*$dy[2] + $dy[3]) / 6;
    }
}

my $RK = runge_kutta sub { $_[0] * sqrt $_[1] }, .1;

for(
    my ($t, $y) = (0, 1);
    sprintf("%.0f", $t) <= 10;
    ($t, $y) = $RK->($t, $y)
) {
    printf "y(%2.0f) = %12f ± %e\n", $t, $y, abs($y - ($t**2 + 4)**2 / 16)
    if sprintf("%.4f", $t) =~ /0000$/;
}
```


```txt
y( 0) =     1.000000 ± 0.000000e+00
y( 1) =     1.562500 ± 1.457219e-07
y( 2) =     3.999999 ± 9.194792e-07
y( 3) =    10.562497 ± 2.909562e-06
y( 4) =    24.999994 ± 6.234909e-06
y( 5) =    52.562489 ± 1.081970e-05
y( 6) =    99.999983 ± 1.659460e-05
y( 7) =   175.562476 ± 2.351773e-05
y( 8) =   288.999968 ± 3.156520e-05
y( 9) =   451.562459 ± 4.072316e-05
y(10) =   675.999949 ± 5.098329e-05
```



## Perl 6

```perl6
sub runge-kutta(&yp) {
    return -> \t, \y, \δt {
        my $a = δt * yp( t, y );
        my $b = δt * yp( t + δt/2, y + $a/2 );
        my $c = δt * yp( t + δt/2, y + $b/2 );
        my $d = δt * yp( t + δt, y + $c );
        ($a + 2*($b + $c) + $d) / 6;
    }
}

constant δt = .1;
my &δy = runge-kutta { $^t * sqrt($^y) };

loop (
    my ($t, $y) = (0, 1);
    $t <= 10;
    ($t, $y) »+=« (δt, δy($t, $y, δt))
) {
    printf "y(%2d) = %12f ± %e\n", $t, $y, abs($y - ($t**2 + 4)**2 / 16)
    if $t %% 1;
}
```

```txt
y( 0) =     1.000000 ± 0.000000e+00
y( 1) =     1.562500 ± 1.457219e-07
y( 2) =     3.999999 ± 9.194792e-07
y( 3) =    10.562497 ± 2.909562e-06
y( 4) =    24.999994 ± 6.234909e-06
y( 5) =    52.562489 ± 1.081970e-05
y( 6) =    99.999983 ± 1.659460e-05
y( 7) =   175.562476 ± 2.351773e-05
y( 8) =   288.999968 ± 3.156520e-05
y( 9) =   451.562459 ± 4.072316e-05
y(10) =   675.999949 ± 5.098329e-05
```



## Phix

```Phix
constant dt = 0.1
atom y = 1.0
printf(1,"  x    true/actual y   calculated y    relative error\n")
printf(1," ---   -------------   -------------   --------------\n")
for i=0 to 100 do
    atom t = i*dt
    if integer(t) then
        atom act = power(t*t+4,2)/16
        printf(1,"%4.1f  %14.9f  %14.9f   %.9e\n",{t,act,y,abs(y-act)})
    end if
    atom k1 = t*sqrt(y),
         k2 = (t+dt/2)*sqrt(y+dt/2*k1),
         k3 = (t+dt/2)*sqrt(y+dt/2*k2),
         k4 = (t+dt)*sqrt(y+dt*k3)
    y += dt*(k1+2*(k2+k3)+k4)/6
end for
```

```txt

  x    true/actual y   calculated y    relative error
 ---   -------------   -------------   --------------
 0.0     1.000000000     1.000000000   0.000000000e+0
 1.0     1.562500000     1.562499854   1.457218921e-7
 2.0     4.000000000     3.999999081   9.194791999e-7
 3.0    10.562500000    10.562497090   2.909562447e-6
 4.0    25.000000000    24.999993765   6.234909363e-6
 5.0    52.562500000    52.562489180   1.081969741e-5
 6.0   100.000000000    99.999983405   1.659459641e-5
 7.0   175.562500000   175.562476482   2.351772874e-5
 8.0   289.000000000   288.999968435   3.156520142e-5
 9.0   451.562500000   451.562459277   4.072316033e-5
10.0   676.000000000   675.999949017   5.098329030e-5

```



## PL/I


```PL/I

Runge_Kutta: procedure options (main);           /* 10 March 2014 */
   declare (y, dy1, dy2, dy3, dy4) float (18);
   declare t fixed decimal (10,1);
   declare dt float (18) static initial (0.1);

   y = 1;
   do t = 0 to 10 by 0.1;
      dy1 = dt * ydash(t, y);
      dy2 = dt * ydash(t + dt/2, y + dy1/2);
      dy3 = dt * ydash(t + dt/2, y + dy2/2);
      dy4 = dt * ydash(t + dt,   y + dy3);

      if mod(t, 1.0) = 0 then
         put skip edit('y(', trim(t), ')=', y, ', error = ', abs(y - (t**2 + 4)**2 / 16 ))
                      (3 a, column(9), f(16,10), a, f(13,10));
      y = y + (dy1 + 2*dy2 + 2*dy3 + dy4)/6;
   end;


ydash: procedure (t, y) returns (float(18));
   declare (t, y) float (18) nonassignable;
   return ( t*sqrt(y) );
end ydash;

end Runge_kutta;

```

```txt

y(0.0)=     1.0000000000, error =  0.0000000000
y(1.0)=     1.5624998543, error =  0.0000001457
y(2.0)=     3.9999990805, error =  0.0000009195
y(3.0)=    10.5624970904, error =  0.0000029096
y(4.0)=    24.9999937651, error =  0.0000062349
y(5.0)=    52.5624891803, error =  0.0000108197
y(6.0)=    99.9999834054, error =  0.0000165946
y(7.0)=   175.5624764823, error =  0.0000235177
y(8.0)=   288.9999684348, error =  0.0000315652
y(9.0)=   451.5624592768, error =  0.0000407232
y(10.0)=  675.9999490167, error =  0.0000509833

```



## PowerShell


```PowerShell

function Runge-Kutta (${function:F}, ${function:y}, $y0, $t0, $dt, $tEnd) {
    function RK ($tn,$yn)  {
        $y1 = $dt*(F -t $tn -y $yn)
        $y2 = $dt*(F -t ($tn + (1/2)*$dt) -y ($yn + (1/2)*$y1))
        $y3 = $dt*(F -t ($tn + (1/2)*$dt) -y ($yn + (1/2)*$y2))
        $y4 = $dt*(F -t ($tn + $dt) -y ($yn + $y3))
        $yn + (1/6)*($y1 + 2*$y2 + 2*$y3 + $y4)
    }
    function time ($t0, $dt, $tEnd)  {
        $end = [MATH]::Floor(($tEnd - $t0)/$dt)
        foreach ($_ in 0..$end) { $_*$dt + $t0 }
    }
    $time, $yn, $t = (time $t0 $dt $tEnd), $y0, 0
    foreach ($tn in $time) {
        if($t -eq $tn) {
            [pscustomobject]@{
                t = "$tn"
                y = "$yn"
                error = "$([MATH]::abs($yn - (y $tn)))"
            }
            $t += 1
        }
        $yn = RK $tn $yn
    }
}
function F ($t,$y)  {
    $t * [MATH]::Sqrt($y)
}
function y ($t)  {
    (1/16) * [MATH]::Pow($t*$t + 4,2)
}
$y0 = 1
$t0 = 0
$dt = 0.1
$tEnd = 10
Runge-Kutta  F y $y0 $t0  $dt  $tEnd

```

<b>Output:</b>

```txt

t                                    y                                    error
-                                    -                                    -----
0                                    1                                    0
1                                    1.56249985427811                     1.45721892108597E-07
2                                    3.9999990805208                      9.19479200778284E-07
3                                    10.5624970904376                     2.90956244874963E-06
4                                    24.9999937650906                     6.23490936391136E-06
5                                    52.5624891803026                     1.08196974153429E-05
6                                    99.9999834054036                     1.65945964170078E-05
7                                    175.562476482271                     2.35177287493116E-05
8                                    288.999968434799                     3.156520142511E-05
9                                    451.56245927684                      4.07231603389846E-05
10                                   675.99994901671                      5.09832902935159E-05

```


## PureBasic

```PureBasic
EnableExplicit
Define.i i
Define.d y=1.0, k1=0.0, k2=0.0, k3=0.0, k4=0.0, t=0.0

If OpenConsole()
  For i=0 To 100
    t=i/10
    If Not i%10
      PrintN("y("+RSet(StrF(t,0),2," ")+") ="+RSet(StrF(y,4),9," ")+#TAB$+"Error ="+RSet(StrF(Pow(Pow(t,2)+4,2)/16-y,10),14," "))
    EndIf
    k1=t*Sqr(y)
    k2=(t+0.05)*Sqr(y+0.05*k1)
    k3=(t+0.05)*Sqr(y+0.05*k2)
    k4=(t+0.10)*Sqr(y+0.10*k3)
    y+0.1*(k1+2*(k2+k3)+k4)/6
  Next
  Print("Press return to exit...") : Input()
EndIf
End
```

```txt
y( 0) =   1.0000        Error =  0.0000000000
y( 1) =   1.5625        Error =  0.0000001457
y( 2) =   4.0000        Error =  0.0000009195
y( 3) =  10.5625        Error =  0.0000029096
y( 4) =  25.0000        Error =  0.0000062349
y( 5) =  52.5625        Error =  0.0000108197
y( 6) = 100.0000        Error =  0.0000165946
y( 7) = 175.5625        Error =  0.0000235177
y( 8) = 289.0000        Error =  0.0000315652
y( 9) = 451.5625        Error =  0.0000407232
y(10) = 675.9999        Error =  0.0000509833
Press return to exit...
```



## Python


### using lambda


```Python
def RK4(f):
    return lambda t, y, dt: (
            lambda dy1: (
            lambda dy2: (
            lambda dy3: (
            lambda dy4: (dy1 + 2*dy2 + 2*dy3 + dy4)/6
            )( dt * f( t + dt  , y + dy3   ) )
	    )( dt * f( t + dt/2, y + dy2/2 ) )
	    )( dt * f( t + dt/2, y + dy1/2 ) )
	    )( dt * f( t       , y         ) )

def theory(t): return (t**2 + 4)**2 /16

from math import sqrt
dy = RK4(lambda t, y: t*sqrt(y))

t, y, dt = 0., 1., .1
while t <= 10:
    if abs(round(t) - t) < 1e-5:
	print("y(%2.1f)\t= %4.6f \t error: %4.6g" % ( t, y, abs(y - theory(t))))
    t, y = t + dt, y + dy( t, y, dt )


```

```txt
y(0.0)	= 1.000000 	 error:    0
y(1.0)	= 1.562500 	 error: 1.45722e-07
y(2.0)	= 3.999999 	 error: 9.19479e-07
y(3.0)	= 10.562497 	 error: 2.90956e-06
y(4.0)	= 24.999994 	 error: 6.23491e-06
y(5.0)	= 52.562489 	 error: 1.08197e-05
y(6.0)	= 99.999983 	 error: 1.65946e-05
y(7.0)	= 175.562476 	 error: 2.35177e-05
y(8.0)	= 288.999968 	 error: 3.15652e-05
y(9.0)	= 451.562459 	 error: 4.07232e-05
y(10.0)	= 675.999949 	 error: 5.09833e-05
```



###  Alternate solution



```python
from math import sqrt

def rk4(f, x0, y0, x1, n):
    vx = [0] * (n + 1)
    vy = [0] * (n + 1)
    h = (x1 - x0) / float(n)
    vx[0] = x = x0
    vy[0] = y = y0
    for i in range(1, n + 1):
        k1 = h * f(x, y)
        k2 = h * f(x + 0.5 * h, y + 0.5 * k1)
        k3 = h * f(x + 0.5 * h, y + 0.5 * k2)
        k4 = h * f(x + h, y + k3)
        vx[i] = x = x0 + i * h
        vy[i] = y = y + (k1 + k2 + k2 + k3 + k3 + k4) / 6
    return vx, vy

def f(x, y):
    return x * sqrt(y)

vx, vy = rk4(f, 0, 1, 10, 100)
for x, y in list(zip(vx, vy))[::10]:
    print("%4.1f %10.5f %+12.4e" % (x, y, y - (4 + x * x)**2 / 16))

 0.0    1.00000  +0.0000e+00
 1.0    1.56250  -1.4572e-07
 2.0    4.00000  -9.1948e-07
 3.0   10.56250  -2.9096e-06
 4.0   24.99999  -6.2349e-06
 5.0   52.56249  -1.0820e-05
 6.0   99.99998  -1.6595e-05
 7.0  175.56248  -2.3518e-05
 8.0  288.99997  -3.1565e-05
 9.0  451.56246  -4.0723e-05
10.0  675.99995  -5.0983e-05
```



## R



```r
rk4 <- function(f, x0, y0, x1, n) {
    vx <- double(n + 1)
    vy <- double(n + 1)
    vx[1] <- x <- x0
    vy[1] <- y <- y0
    h <- (x1 - x0)/n
    for(i in 1:n) {
        k1 <- h*f(x, y)
        k2 <- h*f(x + 0.5*h, y + 0.5*k1)
        k3 <- h*f(x + 0.5*h, y + 0.5*k2)
        k4 <- h*f(x + h, y + k3)
        vx[i + 1] <- x <- x0 + i*h
        vy[i + 1] <- y <- y + (k1 + k2 + k2 + k3 + k3 + k4)/6
    }
    cbind(vx, vy)
}

sol <- rk4(function(x, y) x*sqrt(y), 0, 1, 10, 100)
cbind(sol, sol[, 2] - (4 + sol[, 1]^2)^2/16)[seq(1, 101, 10), ]

      vx         vy
 [1,]  0   1.000000  0.000000e+00
 [2,]  1   1.562500 -1.457219e-07
 [3,]  2   3.999999 -9.194792e-07
 [4,]  3  10.562497 -2.909562e-06
 [5,]  4  24.999994 -6.234909e-06
 [6,]  5  52.562489 -1.081970e-05
 [7,]  6  99.999983 -1.659460e-05
 [8,]  7 175.562476 -2.351773e-05
 [9,]  8 288.999968 -3.156520e-05
[10,]  9 451.562459 -4.072316e-05
[11,] 10 675.999949 -5.098329e-05
```



## Racket


See [[Euler method#Racket]] for implementation of simple general ODE-solver.

The Runge-Kutta method

```racket

(define (RK4 F δt)
  (λ (t y)
    (define δy1 (* δt (F t y)))
    (define δy2 (* δt (F (+ t (* 1/2 δt)) (+ y (* 1/2 δy1)))))
    (define δy3 (* δt (F (+ t (* 1/2 δt)) (+ y (* 1/2 δy2)))))
    (define δy4 (* δt (F (+ t δt) (+ y δy1))))
    (list (+ t δt)
          (+ y (* 1/6 (+ δy1 (* 2 δy2) (* 2 δy3) δy4))))))

```


The method modifier which divides each time-step into ''n'' sub-steps:

```racket

(define ((step-subdivision n method) F h)
  (λ (x . y) (last (ODE-solve F (cons x y)
                              #:x-max (+ x h)
                              #:step (/ h n)
                              #:method method))))

```


Usage:

```racket

(define (F t y) (* t (sqrt y)))

(define (exact-solution t) (* 1/16 (sqr (+ 4 (sqr t)))))

(define numeric-solution
    (ODE-solve F '(0 1) #:x-max 10 #:step 1 #:method (step-subdivision 10 RK4)))

(for ([s numeric-solution])
  (match-define (list t y) s)
  (printf "t=~a\ty=~a\terror=~a\n" t y (- y (exact-solution t))))

```

```txt

t=0	y=1	                error=0
t=1	y=1.562499854278108	error=-1.4572189210859676e-07
t=2	y=3.999999080520799	error=-9.194792007782837e-07
t=3	y=10.562497090437551	error=-2.9095624487496252e-06
t=4	y=24.999993765090636	error=-6.234909363911356e-06
t=5	y=52.562489180302585	error=-1.0819697415342944e-05
t=6	y=99.99998340540358	error=-1.659459641700778e-05
t=7	y=175.56247648227125	error=-2.3517728749311573e-05
t=8	y=288.9999684347986	error=-3.156520142510999e-05
t=9	y=451.56245927683966	error=-4.07231603389846e-05
t=10	y=675.9999490167097	error=-5.098329029351589e-05

```


Graphical representation:


```racket

> (require plot)
> (plot (list (function exact-solution 0 10 #:label "Exact solution")
              (points numeric-solution #:label "Runge-Kutta method"))
   #:x-label "t" #:y-label "y(t)")

```

[[File:runge-kutta.png]]


## REXX


```txt

       The Runge─Kutta method is used to solve the following differential equation:

  ╔═══════════════╗             ______     ╔══ the exact solution:  y(t)= (t²+4)²/16 ══╗
  ╚═══════════════╝   y'(t)=t² √ y(t)      ╚═══════════════════════════════════════════╝

```


```rexx
/*REXX program uses the  Runge─Kutta  method to solve the equation:   y'(t)=t² √[y(t)]  */
numeric digits 40;       f=digits() % 4          /*use 40 decimal digs, but only show 10*/
x0=0;          x1=10;    dx= .1                  /*define variables:    X0   X1   DX    */
n=1 + (x1-x0) / dx
y.=1;                    do m=1  for n-1;   p=m-1;    y.m=RK4(dx,  x0 + dx*p,  y.p)
                         end   /*m*/             /*   [↑]  use 4th order Runge─Kutta.   */
w=digits() % 2                                   /*W: width used for displaying numbers.*/
say center('X', f, "═")  center('Y', w+2, "═")  center("relative error", w+8, '═') /*hdr*/

                do i=0  to n-1  by 10;  x=(x0 + dx*i) / 1;           $=y.i/(x*x/4+1)**2 -1
                say  center(x, f)     fmt(y.i)     left('', 2 + ($>=0) )        fmt($)
                end   /*i*/                      /*└┴┴┴───◄─────── aligns positive #'s. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fmt:  z=right( format( arg(1), w, f), w);     hasE=pos('E', z)\==0;     has.=pos(., z)\==0
      jus=has. & \hasE;        if jus  then z=left( strip( strip(z, 'T', 0),  "T", .),  w)
      return translate(right(z, (z>=0) +  w  +  5*hasE  +  2*(jus & (z<0) ) ),  'e',  "E")
/*──────────────────────────────────────────────────────────────────────────────────────*/
RK4:  procedure; parse arg dx,x,y;   dxH=dx/2;    k1= dx  *  (x      )  *  sqrt(y       )
                                                  k2= dx  *  (x + dxH)  *  sqrt(y + k1/2)
                                                  k3= dx  *  (x + dxH)  *  sqrt(y + k2/2)
                                                  k4= dx  *  (x + dx )  *  sqrt(y + k3  )
      return y + (k1 + k2*2 + k3*2 + k4) / 6
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x; if x=0  then return 0; d=digits(); m.=9; numeric form; h=d+6
      numeric digits;  parse value format(x,2,1,,0) 'E0' with g 'E' _ .;  g=g * .5'e'_ % 2
        do j=0  while h>9;      m.j=h;               h=h%2+1;       end /*j*/
        do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;  end /*k*/;    return g
```

Programming note:   the   '''fmt'''   function is used to
align the output with attention paid to the different ways some

REXXes format numbers that are in floating point representation.


```txt

════X═════ ══════════Y═══════════ ═══════relative error═══════
    0               1                         0
    1               1.5624998543             -9.3262010935e-8
    2               3.9999990805             -2.2986980019e-7
    3              10.5624970904             -2.7546153356e-7
    4              24.9999937651             -2.4939637459e-7
    5              52.5624891803             -2.0584442174e-7
    6              99.9999834054             -1.6594596403e-7
    7             175.5624764823             -1.3395644713e-7
    8             288.9999684348             -1.0922215040e-7
    9             451.5624592768             -9.0182777476e-8
    10            675.9999490167             -7.5419068846e-8

```

```txt

════X═════ ══════════Y═══════════ ═══════relative error═══════
    0               1                         0
    1               1.5624998543             -0.0000000933
    2               3.9999990805             -0.0000002299
    3              10.5624970904             -0.0000002755
    4              24.9999937651             -0.0000002494
    5              52.5624891803             -0.0000002058
    6              99.9999834054             -0.0000001659
    7             175.5624764823             -0.000000134
    8             288.9999684348             -0.0000001092
    9             451.5624592768             -0.0000000902
    10            675.9999490167             -0.0000000754

```



## Ring


```ring

decimals(8)
y = 1.0
for i = 0 to 100
    t = i  / 10
    if t = floor(t)
       actual = (pow((pow(t,2) + 4),2)) / 16
       see "y(" + t + ") = " + y + "  error = " + (actual - y) + nl ok
    k1 =  t * sqrt(y)
    k2 = (t + 0.05) * sqrt(y + 0.05 * k1)
    k3 = (t + 0.05) * sqrt(y + 0.05 * k2)
    k4 = (t + 0.10) * sqrt(y + 0.10 * k3)
    y += 0.1 * (k1 + 2 * (k2 + k3) + k4) / 6
next

```


Output:

```txt

y(0) = 1  error = 0
y(1) = 1.56249985  error = 0.00000015
y(2) = 3.99999908  error = 0.00000092
y(3) = 10.56249709  error = 0.00000291
y(4) = 24.99999377  error = 0.00000623
y(5) = 52.56248918  error = 0.00001082
y(6) = 99.99998341  error = 0.00001659
y(7) = 175.56247648  error = 0.00002352
y(8) = 288.99996843  error = 0.00003157
y(9) = 451.56245928  error = 0.00004072
y(10) = 675.99994902  error = 0.00005098

```



## Ruby


```ruby
def calc_rk4(f)
  return ->(t,y,dt){
         ->(dy1   ){
         ->(dy2   ){
         ->(dy3   ){
         ->(dy4   ){ ( dy1 + 2*dy2 + 2*dy3 + dy4 ) / 6 }.call(
           dt * f.call( t + dt  , y + dy3   ))}.call(
           dt * f.call( t + dt/2, y + dy2/2 ))}.call(
           dt * f.call( t + dt/2, y + dy1/2 ))}.call(
           dt * f.call( t       , y         ))}
end

TIME_MAXIMUM, WHOLE_TOLERANCE = 10.0, 1.0e-5
T_START, Y_START, DT          =  0.0, 1.0, 0.10

def my_diff_eqn(t,y) ; t * Math.sqrt(y)                    ; end
def my_solution(t  ) ; (t**2 + 4)**2 / 16                  ; end
def  find_error(t,y) ; (y - my_solution(t)).abs            ; end
def   is_whole?(t  ) ; (t.round - t).abs < WHOLE_TOLERANCE ; end

dy = calc_rk4( ->(t,y){my_diff_eqn(t,y)} )

t, y = T_START, Y_START
while t <= TIME_MAXIMUM
  printf("y(%4.1f)\t= %12.6f \t error: %12.6e\n",t,y,find_error(t,y)) if is_whole?(t)
  t, y = t + DT, y + dy.call(t,y,DT)
end
```

```txt

y( 0.0)	=     1.000000 	 error: 0.000000e+00
y( 1.0)	=     1.562500 	 error: 1.457219e-07
y( 2.0)	=     3.999999 	 error: 9.194792e-07
y( 3.0)	=    10.562497 	 error: 2.909562e-06
y( 4.0)	=    24.999994 	 error: 6.234909e-06
y( 5.0)	=    52.562489 	 error: 1.081970e-05
y( 6.0)	=    99.999983 	 error: 1.659460e-05
y( 7.0)	=   175.562476 	 error: 2.351773e-05
y( 8.0)	=   288.999968 	 error: 3.156520e-05
y( 9.0)	=   451.562459 	 error: 4.072316e-05
y(10.0)	=   675.999949 	 error: 5.098329e-05

```



## Run BASIC


```Runbasic
y	= 1
while t <= 10
   k1	=  t        * sqr(y)
   k2	= (t + .05) * sqr(y + .05 * k1)
   k3	= (t + .05) * sqr(y + .05 * k2)
   k4	= (t + .1)  * sqr(y + .1  * k3)

if right$(using("##.#",t),1) = "0" then  print "y(";using("##",t);") ="; using("####.#######", y);chr$(9);"Error ="; (((t^2 + 4)^2) /16) -y
    y = y + .1 *(k1 + 2 * (k2 + k3) + k4) / 6
   t = t + .1
wend
end
```

```txt
y( 0) =   1.0000000	Error =0
y( 1) =   1.5624999	Error =1.45721892e-7
y( 2) =   3.9999991	Error =9.19479203e-7
y( 3) =  10.5624971	Error =2.90956246e-6
y( 4) =  24.9999938	Error =6.23490939e-6
y( 5) =  52.5624892	Error =1.08196973e-5
y( 6) =  99.9999834	Error =1.65945961e-5
y( 7) = 175.5624765	Error =2.3517728e-5
y( 8) = 288.9999684	Error =3.15652e-5
y( 9) = 451.5624593	Error =4.07231581e-5
y(10) = 675.9999490	Error =5.09832864e-5

```



## Rust

This is a translation of the javascript solution with some minor differences.

```rust
fn runge_kutta4(fx: &Fn(f64, f64) -> f64, x: f64, y: f64, dx: f64) -> f64 {
    let k1 = dx * fx(x, y);
    let k2 = dx * fx(x + dx / 2.0, y + k1 / 2.0);
    let k3 = dx * fx(x + dx / 2.0, y + k2 / 2.0);
    let k4 = dx * fx(x + dx, y + k3);

    y + (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0
}

fn f(x: f64, y: f64) -> f64 {
    x * y.sqrt()
}

fn actual(x: f64) -> f64 {
    (1.0 / 16.0) * (x * x + 4.0).powi(2)
}

fn main() {
    let mut y = 1.0;
    let mut x = 0.0;
    let step = 0.1;
    let max_steps = 101;
    let sample_every_n = 10;

    for steps in 0..max_steps {
        if steps % sample_every_n == 0 {
            println!("y({}):\t{:.10}\t\t {:E}", x, y, actual(x) - y)
        }

        y = runge_kutta4(&f, x, y, step);

        x = ((x * 10.0) + (step * 10.0)) / 10.0;
    }
}
```


```txt

y(0):	1.0000000000		 0E0
y(1):	1.5624998543		 1.4572189210859676E-7
y(2):	3.9999990805		 9.194792007782837E-7
y(3):	10.5624970904		 2.9095624487496252E-6
y(4):	24.9999937651		 6.234909363911356E-6
y(5):	52.5624891803		 1.0819697415342944E-5
y(6):	99.9999834054		 1.659459641700778E-5
y(7):	175.5624764823		 2.3517728749311573E-5
y(8):	288.9999684348		 3.156520142510999E-5
y(9):	451.5624592768		 4.07231603389846E-5
y(10):	675.9999490167		 5.098329029351589E-5

```



## Scala


```scala
object Main extends App {
   val f = (t: Double, y: Double) => t * Math.sqrt(y) // Runge-Kutta solution
   val g = (t: Double) => Math.pow(t * t + 4, 2) / 16 // Exact solution
   new Calculator(f, Some(g)).compute(100, 0, .1, 1)
}

class Calculator(f: (Double, Double) => Double, g: Option[Double => Double] = None) {
   def compute(counter: Int, tn: Double, dt: Double, yn: Double): Unit = {
      if (counter % 10 == 0) {
         val c = (x: Double => Double) => (t: Double) => {
            val err = Math.abs(x(t) - yn)
            f" Error: $err%7.5e"
         }
         val s = g.map(c(_)).getOrElse((x: Double) => "") // If we don't have exact solution, just print nothing
         println(f"y($tn%4.1f) = $yn%12.8f${s(tn)}") // Else, print Error estimation here
      }
      if (counter > 0) {
         val dy1 = dt * f(tn, yn)
         val dy2 = dt * f(tn + dt / 2, yn + dy1 / 2)
         val dy3 = dt * f(tn + dt / 2, yn + dy2 / 2)
         val dy4 = dt * f(tn + dt, yn + dy3)
         val y = yn + (dy1 + 2 * dy2 + 2 * dy3 + dy4) / 6
         val t = tn + dt
         compute(counter - 1, t, dt, y)
      }
   }
}
```


```txt

y( 0.0) =   1.00000000 Error: 0.00000e+00
y( 1.0) =   1.56249985 Error: 1.45722e-07
y( 2.0) =   3.99999908 Error: 9.19479e-07
y( 3.0) =  10.56249709 Error: 2.90956e-06
y( 4.0) =  24.99999377 Error: 6.23491e-06
y( 5.0) =  52.56248918 Error: 1.08197e-05
y( 6.0) =  99.99998341 Error: 1.65946e-05
y( 7.0) = 175.56247648 Error: 2.35177e-05
y( 8.0) = 288.99996843 Error: 3.15652e-05
y( 9.0) = 451.56245928 Error: 4.07232e-05
y(10.0) = 675.99994902 Error: 5.09833e-05

```



## Sidef

```ruby
func runge_kutta(yp) {
    func (t, y, δt) {
        var a = (δt * yp(t, y));
        var b = (δt * yp(t + δt/2, y + a/2));
        var c = (δt * yp(t + δt/2, y + b/2));
        var d = (δt * yp(t + δt, y + c));
        (a + 2*(b + c) + d) / 6;
    }
}

define δt = 0.1;
var δy = runge_kutta(func(t, y) { t * y.sqrt });

var(t, y) = (0, 1);
loop {
    t.is_int &&
        printf("y(%2d) = %12f ± %e\n", t, y, abs(y - ((t**2 + 4)**2 / 16)));
    t <= 10 || break;
    y += δy(t, y, δt);
    t += δt;
}
```

```txt

y( 0) =     1.000000 ± 0.000000e+00
y( 1) =     1.562500 ± 1.457219e-07
y( 2) =     3.999999 ± 9.194792e-07
y( 3) =    10.562497 ± 2.909562e-06
y( 4) =    24.999994 ± 6.234909e-06
y( 5) =    52.562489 ± 1.081970e-05
y( 6) =    99.999983 ± 1.659460e-05
y( 7) =   175.562476 ± 2.351773e-05
y( 8) =   288.999968 ± 3.156520e-05
y( 9) =   451.562459 ± 4.072316e-05
y(10) =   675.999949 ± 5.098329e-05

```



## Standard ML


```sml
fun step y' (tn,yn) dt =
    let
        val dy1 = dt * y'(tn,yn)
        val dy2 = dt * y'(tn + 0.5 * dt, yn + 0.5 * dy1)
        val dy3 = dt * y'(tn + 0.5 * dt, yn + 0.5 * dy2)
        val dy4 = dt * y'(tn + dt, yn + dy3)
    in
        (tn + dt, yn + (1.0 / 6.0) * (dy1 + 2.0*dy2 + 2.0*dy3 + dy4))
    end

(* Suggested test case *)
fun testy' (t,y) =
    t * Math.sqrt y

fun testy t =
    (1.0 / 16.0) * Math.pow(Math.pow(t,2.0) + 4.0, 2.0)

(* Test-runner that iterates the step function and prints the results. *)
fun test t0 y0 dt steps print_freq y y' =
    let
        fun loop i (tn,yn) =
            if i = steps then ()
            else
                let
                    val (t1,y1) = step y' (tn,yn) dt
                    val y1' = y tn
                    val () = if i mod print_freq = 0 then
                                 (print ("Time: " ^ Real.toString tn ^ "\n");
                                  print ("Exact: " ^ Real.toString y1' ^ "\n");
                                  print ("Approx: " ^ Real.toString yn ^ "\n");
                                  print ("Error: " ^ Real.toString (y1' - yn) ^ "\n\n"))
                             else ()
                 in
                     loop (i+1) (t1,y1)
                end
    in
        loop 0 (t0,y0)
    end

(* Run the suggested test case *)
val () = test 0.0 1.0 0.1 101 10 testy testy'
```

```txt
Time: 0.0
Exact: 1.0
Approx: 1.0
Error: ~1.11022302463E~16

Time: 1.0
Exact: 1.5625
Approx: 1.56249985428
Error: 1.45722452549E~07

Time: 2.0
Exact: 4.0
Approx: 3.99999908052
Error: 9.19479203443E~07

Time: 3.0
Exact: 10.5625
Approx: 10.5624970904
Error: 2.90956245586E~06

Time: 4.0
Exact: 25.0
Approx: 24.9999937651
Error: 6.23490938878E~06

Time: 5.0
Exact: 52.5625
Approx: 52.5624891803
Error: 1.08196973727E~05

Time: 6.0
Exact: 100.0
Approx: 99.9999834054
Error: 1.65945961186E~05

Time: 7.0
Exact: 175.5625
Approx: 175.562476482
Error: 2.35177280956E~05

Time: 8.0
Exact: 289.0
Approx: 288.999968435
Error: 3.15651997767E~05

Time: 9.0
Exact: 451.5625
Approx: 451.562459277
Error: 4.07231581221E~05

Time: 10.0
Exact: 676.0
Approx: 675.999949017
Error: 5.09832866555E~05
```



## Stata


```stata
function rk4(f, t0, y0, t1, n) {
	h = (t1-t0)/(n-1)
	a = J(n, 2, 0)
	a[1, 1] = t = t0
	a[1, 2] = y = y0
	for (i=2; i<=n; i++) {
		k1 = h*(*f)(t, y)
		k2 = h*(*f)(t+0.5*h, y+0.5*k1)
		k3 = h*(*f)(t+0.5*h, y+0.5*k2)
		k4 = h*(*f)(t+h, y+k3)
		t = t+h
		y = y+(k1+2*k2+2*k3+k4)/6
		a[i, 1] = t
		a[i, 2] = y
	}
	return(a)
}

function f(t, y) {
	return(t*sqrt(y))
}

a = rk4(&f(), 0, 1, 10, 101)
t = a[., 1]
a = a, a[., 2]:-(t:^2:+4):^2:/16
a[range(1,101,10), .]

                   1              2              3
     +----------------------------------------------+
   1 |             0              1              0  |
   2 |             1    1.562499854   -1.45722e-07  |
   3 |             2    3.999999081   -9.19479e-07  |
   4 |             3    10.56249709   -2.90956e-06  |
   5 |             4    24.99999377   -6.23491e-06  |
   6 |             5    52.56248918   -.0000108197  |
   7 |             6    99.99998341   -.0000165946  |
   8 |             7    175.5624765   -.0000235177  |
   9 |             8    288.9999684   -.0000315652  |
  10 |             9    451.5624593   -.0000407232  |
  11 |            10     675.999949   -.0000509833  |
     +----------------------------------------------+
```



## Swift

```Swift
import Foundation

func rk4(dx: Double, x: Double, y: Double, f: (Double, Double) -> Double) -> Double {
    let k1 = dx * f(x, y)
    let k2 = dx * f(x + dx / 2, y + k1 / 2)
    let k3 = dx * f(x + dx / 2, y + k2 / 2)
    let k4 = dx * f(x + dx, y + k3)

    return y + (k1 + 2 * k2 + 2 * k3 + k4) / 6
}

var y = [Double]()
var x: Double = 0.0
var y2: Double = 0.0

var x0: Double = 0.0
var x1: Double = 10.0
var dx: Double = 0.1

var i = 0
var n = Int(1 + (x1 - x0) / dx)

y.append(1)
for i in 1..<n {
    y.append(rk4(dx, x: x0 + dx * (Double(i) - 1), y: y[i - 1]) { (x: Double, y: Double) -> Double in
        return x * sqrt(y)
    })
}

print(" x         y        rel. err.")
print("------------------------------")

for (var i = 0; i < n; i += 10) {
    x = x0 + dx * Double(i)
    y2 = pow(x * x / 4 + 1, 2)

    print(String(format: "%2g  %11.6g    %11.5g", x, y[i], y[i]/y2 - 1))
}
```


```txt
 x         y        rel. err.
------------------------------
 0            1              0
 1       1.5625    -9.3262e-08
 2            4    -2.2987e-07
 3      10.5625    -2.7546e-07
 4           25     -2.494e-07
 5      52.5625    -2.0584e-07
 6          100    -1.6595e-07
 7      175.562    -1.3396e-07
 8          289    -1.0922e-07
 9      451.562    -9.0183e-08
10          676    -7.5419e-08
```



## Tcl


```tcl
package require Tcl 8.5

# Hack to bring argument function into expression
proc tcl::mathfunc::dy {t y} {upvar 1 dyFn dyFn; $dyFn $t $y}

proc rk4step {dyFn y* t* dt} {
    upvar 1 ${y*} y ${t*} t
    set dy1 [expr {$dt * dy($t,       $y)}]
    set dy2 [expr {$dt * dy($t+$dt/2, $y+$dy1/2)}]
    set dy3 [expr {$dt * dy($t+$dt/2, $y+$dy2/2)}]
    set dy4 [expr {$dt * dy($t+$dt,   $y+$dy3)}]
    set y [expr {$y + ($dy1 + 2*$dy2 + 2*$dy3 + $dy4)/6.0}]
    set t [expr {$t + $dt}]
}

proc y {t} {expr {($t**2 + 4)**2 / 16}}
proc δy {t y} {expr {$t * sqrt($y)}}

proc printvals {t y} {
    set err [expr {abs($y - [y $t])}]
    puts [format "y(%.1f) = %.8f\tError: %.8e" $t $y $err]
}

set t 0.0
set y 1.0
set dt 0.1
printvals $t $y
for {set i 1} {$i <= 101} {incr i} {
    rk4step  δy  y t  $dt
    if {$i%10 == 0} {
	printvals $t $y
    }
}
```

```txt
y(0.0) = 1.00000000	Error: 0.00000000e+00
y(1.0) = 1.56249985	Error: 1.45721892e-07
y(2.0) = 3.99999908	Error: 9.19479203e-07
y(3.0) = 10.56249709	Error: 2.90956245e-06
y(4.0) = 24.99999377	Error: 6.23490939e-06
y(5.0) = 52.56248918	Error: 1.08196973e-05
y(6.0) = 99.99998341	Error: 1.65945961e-05
y(7.0) = 175.56247648	Error: 2.35177280e-05
y(8.0) = 288.99996843	Error: 3.15652000e-05
y(9.0) = 451.56245928	Error: 4.07231581e-05
y(10.0) = 675.99994902	Error: 5.09832864e-05
```



## zkl

```zkl
fcn yp(t,y) { t * y.sqrt() }
fcn exact(t){ u:=0.25*t*t + 1.0; u*u }

fcn rk4_step([(y,t)],h){
   k1:=h * yp(t,y);
   k2:=h * yp(t + 0.5*h, y + 0.5*k1);
   k3:=h * yp(t + 0.5*h, y + 0.5*k2);
   k4:=h * yp(t + h, y + k3);
   T(y + (k1+k4)/6.0 + (k2+k3)/3.0, t + h);
}

fcn loop(h,n,[(y,t)]){
   if(n % 10 == 1)
      print("t = %f,\ty = %f,\terr = %g\n".fmt(t,y,(y - exact(t)).abs()));
   if(n < 102) return(loop(h,(n+1),rk4_step(T(y,t),h))) //tail recursion
}
```

```txt

loop(0.1,1,T(1.0, 0.0))
t = 0.000000,	y = 1.000000,	err = 0
t = 1.000000,	y = 1.562500,	err = 1.45722e-07
t = 2.000000,	y = 3.999999,	err = 9.19479e-07
t = 3.000000,	y = 10.562497,	err = 2.90956e-06
t = 4.000000,	y = 24.999994,	err = 6.23491e-06
t = 5.000000,	y = 52.562489,	err = 1.08197e-05
t = 6.000000,	y = 99.999983,	err = 1.65946e-05
t = 7.000000,	y = 175.562476,	err = 2.35177e-05
t = 8.000000,	y = 288.999968,	err = 3.15652e-05
t = 9.000000,	y = 451.562459,	err = 4.07232e-05
t = 10.000000,	y = 675.999949,	err = 5.09833e-05

```


