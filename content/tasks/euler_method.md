+++
title = "Euler method"
description = ""
date = 2019-07-20T21:24:01Z
aliases = []
[extra]
id = 9333
task = """
  Implement a routine of Euler's method
  and then to use it to solve the given example of Newton's cooling law
  with it for three different step sizes of:

  - 2 s
  - 5 s
  - 10 s

  Then compare it with the analytical solution.
"""
[taxonomies]
categories = ["task"]
languages = [
  "11l",
  "ada",
  "algol_68",
  "basic",
  "bbc_basic",
  "freebasic",
  "run_basic",
  "c",
  "cpp",
  "csharp",
  "clay",
  "cobol",
  "clojure",
  "common_lisp",
  "d",
  "delphi",
  "elixir",
  "erlang",
  "euler_math_toolbox",
  "fsharp",
  "factor",
  "forth",
  "fortran",
  "futhark",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "wolfram_language",
  "maxima",
  "мк_61_52",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sequencel",
  "sidef",
  "smalltalk",
  "tcl",
  "vba",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
tags = ["math"]
+++

Euler's method numerically approximates solutions of first-order ordinary differential equations
(ODEs) with a given initial value.
It is an explicit method for solving initial value problems (IVPs),
as described in [[wp:Euler method|the wikipedia page]].

The ODE has to be provided in the following form:

<math>\frac{dy(t)}{dt} = f(t,y(t))</math>

with an initial value

<math>y(t_0) = y_0</math>

To get a numeric solution, we replace the derivative on the   LHS   with a finite difference approximation:

<math>\frac{dy(t)}{dt}  \approx \frac{y(t+h)-y(t)}{h}</math>

then solve for <math>y(t+h)</math>:

<math>y(t+h) \approx y(t) + h \, \frac{dy(t)}{dt}</math>

which is the same as

<math>y(t+h) \approx y(t) + h \, f(t,y(t))</math>

The iterative solution rule is then:

<math>y_{n+1} = y_n + h \, f(t_n, y_n)</math>

where <math>h</math> is the step size, the most relevant parameter for accuracy of the solution.
A smaller step size increases accuracy but also the computation cost,
so it has always has to be hand-picked according to the problem at hand.


'''Example: Newton's Cooling Law'''

Newton's cooling law describes how an object of initial temperature <math>T(t_0) = T_0</math> cools down in an environment of temperature <math>T_R</math>:

<math>\frac{dT(t)}{dt} = -k \, \Delta T</math>
or
<math>\frac{dT(t)}{dt} = -k \, (T(t) - T_R)</math>



It says that the cooling rate <math>\frac{dT(t)}{dt}</math> of the object is proportional to the current temperature difference <math>\Delta T = (T(t) - T_R)</math> to the surrounding environment.

The analytical solution, which we will compare to the numerical approximation, is
<math>T(t) = T_R + (T_0 - T_R) \; e^{-k t}</math>


Initial values:

- Initial temperature <math>T_0</math> shall be 100 °C
- Room temperature <math>T_R</math> shall be 20 °C
- Cooling constant <math>k</math> shall be 0.07
- Time interval to calculate shall be from 0 s ──► 100 s



A reference solution ([[#Common Lisp|Common Lisp]]) can be seen below.   We see that bigger step sizes lead to reduced approximation accuracy.
[[Image:Euler_Method_Newton_Cooling.png|center|750px]]


## 11l

Translated from Python

```11l
F euler(f, y0, a, b, h)
   V t = a
   V y = y0
   L t <= b
      print(‘#2.3 #2.3’.format(t, y))
      t += h
      y += h * f(t, y)

V newtoncooling = (time, temp) -> -0.07 * (temp - 20)

euler(newtoncooling, 100.0, 0.0, 100.0, 10.0)
```

Output:

```txt

 0.000 100.000
10.000 44.000
20.000 27.200
30.000 22.160
40.000 20.648
50.000 20.194
60.000 20.058
70.000 20.017
80.000 20.005
90.000 20.002
100.000 20.000

```



## Ada

The solution is generic, usable for any floating point type. The package specification:

```Ada

generic
   type Number is digits <>;
package Euler is
   type Waveform is array (Integer range <>) of Number;
   function Solve
            (  F      : not null access function (T, Y : Number) return Number;
               Y0     : Number;
               T0, T1 : Number;
               N      : Positive
            )  return Waveform;
end Euler;

```

The function Solve returns the solution of the differential equation for each of N+1 points, starting from the point T0. The implementation:

```Ada

package body Euler is
   function Solve
            (  F      : not null access function (T, Y : Number) return Number;
               Y0     : Number;
               T0, T1 : Number;
               N      : Positive
            )  return Waveform is
      dT : constant Number := (T1 - T0) / Number (N);
   begin
      return Y : Waveform (0..N) do
         Y (0) := Y0;
         for I in 1..Y'Last loop
            Y (I) := Y (I - 1) + dT * F (T0 + dT * Number (I - 1), Y (I - 1));
         end loop;
      end return;
   end Solve;
end Euler;

```

The test program:

```Ada

with Ada.Text_IO;  use Ada.Text_IO;
with Euler;

procedure Test_Euler_Method is
   package Float_Euler is new Euler (Float);
   use Float_Euler;

   function Newton_Cooling_Law (T, Y : Float) return Float is
   begin
      return -0.07 * (Y - 20.0);
   end Newton_Cooling_Law;

   Y : Waveform := Solve (Newton_Cooling_Law'Access, 100.0, 0.0, 100.0, 10);
begin
   for I in Y'Range loop
      Put_Line (Integer'Image (10 * I) & ":" & Float'Image (Y (I)));
   end loop;
end Test_Euler_Method;

```

Sample output:

```txt

 0: 1.00000E+02
 10: 4.40000E+01
 20: 2.72000E+01
 30: 2.21600E+01
 40: 2.06480E+01
 50: 2.01944E+01
 60: 2.00583E+01
 70: 2.00175E+01
 80: 2.00052E+01
 90: 2.00016E+01
 100: 2.00005E+01

```


## ALGOL 68

Translated from D}} Note: This specimen retains the original [[#D|D]] coding styl
Works with ALGOL 68|Revision 1 - no extensions to language used.
Works with ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
#
Approximates y(t) in y'(t)=f(t,y) with y(a)=y0 and
t=a..b and the step size h.
#
PROC euler = (PROC(REAL,REAL)REAL f, REAL y0, a, b, h)REAL: (
    REAL y := y0,
         t := a;
    WHILE t < b DO
      printf(($g(-6,3)": "g(-7,3)l$, t, y));
      y +:= h * f(t, y);
      t +:= h
    OD;
    printf($"done"l$);
    y
);

# Example: Newton's cooling law #
PROC newton cooling law = (REAL time, t)REAL: (
    -0.07 * (t - 20)
);

main: (
   euler(newton cooling law, 100, 0, 100,  10)
)
```

Ouput:

```txt

 0.000: 100.000
10.000:  44.000
20.000:  27.200
30.000:  22.160
40.000:  20.648
50.000:  20.194
60.000:  20.058
70.000:  20.017
80.000:  20.005
90.000:  20.002
done

```



## BASIC

=
## BBC BASIC
=

```bbcbasic
      PROCeuler("-0.07*(y-20)", 100, 0, 100, 2)
      PROCeuler("-0.07*(y-20)", 100, 0, 100, 5)
      PROCeuler("-0.07*(y-20)", 100, 0, 100, 10)
      END

      DEF PROCeuler(df$, y, a, b, s)
      LOCAL t, @%
      @% = &2030A
      t = a
      WHILE t <= b
        PRINT t, y
        y += s * EVAL(df$)
        t += s
      ENDWHILE
      ENDPROC
```

'''Output:'''

```txt

     0.000   100.000
     2.000    88.800
     4.000    79.168
     6.000    70.884
     8.000    63.761
    10.000    57.634
...
     0.000   100.000
    10.000    44.000
    20.000    27.200
    30.000    22.160
    40.000    20.648
    50.000    20.194
    60.000    20.058
    70.000    20.017
    80.000    20.005
    90.000    20.002
   100.000    20.000

```


=
## FreeBASIC
=

```FreeBASIC
'Freebasic .9
'Custom rounding
#define round(x,N) Rtrim(Rtrim(Left(Str((x)+(.5*Sgn((x)))/(10^(N))),Instr(Str((x)+(.5*Sgn((x)))/(10^(N))),".")+(N)),"0"),".")

#macro Euler(fn,_y,min,max,h,printoption)
Print "Step ";#h;":":Print
Print "time","Euler"," Analytic"
If printoption<>"print" Then Print "Data omitted ..."
Scope
    Dim As Double temp=(min),y=(_y)
    Do
        If printoption="print" Then Print temp,round(y,3),20+80*Exp(-0.07*temp)
        y=y+(h)*(fn)
        temp=temp+(h)
    Loop Until temp>(max)
    Print"________________"
    Print
End Scope
#endmacro

Euler(-.07*(y-20),100,0,100,2,"don't print")
Euler(-.07*(y-20),100,0,100,5,"print")
Euler(-.07*(y-20),100,0,100,10,"print")
Sleep

```

outputs (steps 5 and 10)

```txt

Step 2:

time          Euler          Analytic
Data omitted ...
________________

Step 5:

time          Euler          Analytic
 0            100            100
 5            72             76.37504717749707
 10           53.8           59.72682430331276
 15           41.97          47.99501992889243
 20           34.281         39.72775711532852
 25           29.282         33.90191547603561
 30           26.034         29.79651426023855
 35           23.922         26.90348691994964
 40           22.549         24.86480501001743
 45           21.657         23.42817014936322
 50           21.077         22.41579067378548
 55           20.7           21.70237891507017
 60           20.455         21.19964614563822
 65           20.296         20.84537635070821
 70           20.192         20.59572664567395
 75           20.125         20.41980147193451
 80           20.081         20.29582909731863
 85           20.053         20.20846724147268
 90           20.034         20.14690438216231
 95           20.022         20.10352176843727
 100          20.014         20.07295055724436
________________

Step 10:

time          Euler          Analytic
 0            100            100
 10           44             59.72682430331276
 20           27.2           39.72775711532852
 30           22.16          29.79651426023855
 40           20.648         24.86480501001743
 50           20.194         22.41579067378548
 60           20.058         21.19964614563822
 70           20.017         20.59572664567395
 80           20.005         20.29582909731863
 90           20.002         20.14690438216231
 100          20             20.07295055724436
________________


```


=
## Run BASIC
=

```rinbasic
x = euler(-0.07,-20, 100, 0, 100, 2)
x = euler-0.07,-20, 100, 0, 100, 5)
x = euler(-0.07,-20, 100, 0, 100, 10)
end

FUNCTION euler(da,db, y, a, b, s)
print "===== da:";da;" db:";db;" y:";y;" a:";a;" b:";b;" s:";s;"
### =============
"
t = a
WHILE t <= b
   PRINT t;chr$(9);y
   y = y + s * (da * (y + db))
   t = t + s
WEND
END FUNCTION
```


```txt
===== da:-0.07 db:-20 y:100 a:0 b:100 s:2
### =============

0	100
2	88.8
4	79.168
6	70.88448
8	63.7606528
10	57.6341614
12	52.3653788
14	47.8342258
......
===== da:-0.07 db:-20 y:100 a:0 b:100 s:10
### =============

0	100
10	44.0
20	27.2
30	22.16
40	20.648
50	20.1944
60	20.05832
70	20.017496
80	20.0052488
```



## C


```c
#include <stdio.h>
#include <math.h>

typedef double (*deriv_f)(double, double);
#define FMT " %7.3f"

void ivp_euler(deriv_f f, double y, int step, int end_t)
{
	int t = 0;

	printf(" Step %2d: ", (int)step);
	do {
		if (t % 10 == 0) printf(FMT, y);
		y += step * f(t, y);
	} while ((t += step) <= end_t);
	printf("\n");
}

void analytic()
{
	double t;
	printf("    Time: ");
	for (t = 0; t <= 100; t += 10) printf(" %7g", t);
	printf("\nAnalytic: ");

	for (t = 0; t <= 100; t += 10)
		printf(FMT, 20 + 80 * exp(-0.07 * t));
	printf("\n");
}

double cooling(double t, double temp)
{
	return -0.07 * (temp - 20);
}

int main()
{
	analytic();
	ivp_euler(cooling, 100, 2, 100);
	ivp_euler(cooling, 100, 5, 100);
	ivp_euler(cooling, 100, 10, 100);

	return 0;
}

```txt
Time:        0      10      20      30      40      50      60      70      80      90     100
Analytic:  100.000  59.727  39.728  29.797  24.865  22.416  21.200  20.596  20.296  20.147  20.073
 Step  2:  100.000  57.634  37.704  28.328  23.918  21.843  20.867  20.408  20.192  20.090  20.042
 Step  5:  100.000  53.800  34.280  26.034  22.549  21.077  20.455  20.192  20.081  20.034  20.014
 Step 10:  100.000  44.000  27.200  22.160  20.648  20.194  20.058  20.017  20.005  20.002  20.000
```



## C++

Translated from D

```cpp
#include <iomanip>
#include <iostream>

typedef double F(double,double);

/*
Approximates y(t) in y'(t)=f(t,y) with y(a)=y0 and
t=a..b and the step size h.
*/
void euler(F f, double y0, double a, double b, double h)
{
    double y = y0;
    for (double t = a; t < b; t += h)
    {
        std::cout << std::fixed << std::setprecision(3) << t << " " << y << "\n";
        y += h * f(t, y);
    }
    std::cout << "done\n";
}

// Example: Newton's cooling law
double newtonCoolingLaw(double, double t)
{
    return -0.07 * (t - 20);
}

int main()
{
    euler(newtonCoolingLaw, 100, 0, 100,  2);
    euler(newtonCoolingLaw, 100, 0, 100,  5);
    euler(newtonCoolingLaw, 100, 0, 100, 10);
}
```

Last part of output:

```txt

...
0.000 100.000
10.000 44.000
20.000 27.200
30.000 22.160
40.000 20.648
50.000 20.194
60.000 20.058
70.000 20.017
80.000 20.005
90.000 20.002
done

```


## C#


```c#
using System;

namespace prog
{
	class MainClass
	{
		const float T0 = 100f;
		const float TR = 20f;
		const float k = 0.07f;
		readonly static float[] delta_t = {2.0f,5.0f,10.0f};
		const int n = 100;

		public delegate float func(float t);
		static float NewtonCooling(float t)
		{
			return -k * (t-TR);
		}

		public static void Main (string[] args)
		{
			func f = new func(NewtonCooling);
			for(int i=0; i<delta_t.Length; i++)
			{
				Console.WriteLine("delta_t = " + delta_t[i]);
				Euler(f,T0,n,delta_t[i]);
			}
		}

		public static void Euler(func f, float y, int n, float h)
		{
			for(float x=0; x<=n; x+=h)
			{
				Console.WriteLine("\t" + x + "\t" + y);
				y += h * f(y);
			}
		}
	}
}
```



## Clay



```Clay

import printer.formatter as pf;

euler(f, y, a, b, h) {
    while (a < b) {
        println(pf.rightAligned(2, a), " ", y);
        a += h;
        y += h * f(y);
    }
}

main() {
    for (i in [2.0, 5.0, 10.0]) {
        println("\nFor delta = ", i, ":");
        euler((temp) => -0.07 * (temp - 20), 100.0, 0.0, 100.0, i);
    }
}

```


Example output:

```txt

For delta = 10:
 0 100
10 43.99999999999999
20 27.2
30 22.16
40 20.648
50 20.1944
60 20.05832
70 20.017496
80 20.0052488
90 20.00157464

```



## COBOL


Translated from C#
Works with Visual COBOL
The following is in the Managed COBOL dialect:

```cobol
       DELEGATE-ID func.
       PROCEDURE DIVISION USING VALUE t AS FLOAT-LONG
           RETURNING ret AS FLOAT-LONG.
       END DELEGATE.

       CLASS-ID. MainClass.

       78  T0                     VALUE 100.0.
       78  TR                     VALUE 20.0.
       78  k                      VALUE 0.07.

       01  delta-t                INITIALIZE ONLY STATIC
                                  FLOAT-LONG OCCURS 3 VALUES 2.0, 5.0, 10.0.

       78  n                      VALUE 100.

       METHOD-ID NewtonCooling STATIC.
       PROCEDURE DIVISION USING VALUE t AS FLOAT-LONG
               RETURNING ret AS FLOAT-LONG.
           COMPUTE ret = - k * (t - TR)
       END METHOD.

       METHOD-ID Main STATIC.
           DECLARE f AS TYPE func
           SET f TO METHOD self::NewtonCooling

           DECLARE delta-t-len AS BINARY-LONG
           MOVE delta-t::Length TO delta-t-len
           PERFORM VARYING i AS BINARY-LONG FROM 1 BY 1
                   UNTIL i > delta-t-len
               DECLARE elt AS FLOAT-LONG = delta-t (i)
               INVOKE TYPE Console::WriteLine("delta-t = {0:F4}", elt)
               INVOKE self::Euler(f, T0, n, elt)
           END-PERFORM
       END METHOD.

       METHOD-ID Euler STATIC.
       PROCEDURE DIVISION USING VALUE f AS TYPE func, y AS FLOAT-LONG,
               n AS BINARY-LONG, h AS FLOAT-LONG.
           PERFORM VARYING x AS BINARY-LONG FROM 0 BY h UNTIL x >= n
               INVOKE TYPE Console::WriteLine("x = {0:F4}, y = {1:F4}", x, y)
               COMPUTE y = y + h * RUN f(y)
           END-PERFORM
       END METHOD.
       END CLASS.
```


Example output:

```txt

delta-t = 10.0000
x = 0.0000, y = 100.0000
x = 10.0000, y = 44.0000
x = 20.0000, y = 27.2000
x = 30.0000, y = 22.1600
x = 40.0000, y = 20.6480
x = 50.0000, y = 20.1944
x = 60.0000, y = 20.0583
x = 70.0000, y = 20.0175
x = 80.0000, y = 20.0052
x = 90.0000, y = 20.0016

```



## Clojure

Translated from Python

```lisp
(ns newton-cooling
  (:gen-class))

(defn euler [f y0 a b h]
  "Euler's Method.
  Approximates y(time) in y'(time)=f(time,y) with y(a)=y0 and t=a..b and the step size h."
  (loop [t a
         y y0
         result []]
    (if (<= t b)
        (recur (+ t h) (+ y (* (f (+ t h) y) h)) (conj result [(double t) (double y)]))
        result)))

(defn newton-coolling [t temp]
  "Newton's cooling law, f(t,T) = -0.07*(T-20)"
  (* -0.07 (- temp 20)))

; Run for case h = 10
(println "Example output")
(doseq [q (euler newton-coolling 100 0 100 10)]
  (println (apply format "%.3f %.3f" q)))

```

{{Output}}

```txt

Example output
0.000 100.000
10.000 44.000
20.000 27.200
30.000 22.160
40.000 20.648
50.000 20.194
60.000 20.058
70.000 20.017
80.000 20.005
90.000 20.002
100.000 20.000

```



## Common Lisp


```lisp
;; 't' usually means "true" in CL, but we need 't' here for time/temperature.
(defconstant true 'cl:t)
(shadow 't)


;; Approximates y(t) in y'(t)=f(t,y) with y(a)=y0 and t=a..b and the step size h.
(defun euler (f y0 a b h)

  ;; Set the initial values and increments of the iteration variables.
  (do ((t a  (+ t h))
       (y y0 (+ y (* h (funcall f t y)))))

      ;; End the iteration when t reaches the end b of the time interval.
      ((>= t b) 'DONE)

      ;; Print t and y(t) at every step of the do loop.
      (format true "~6,3F  ~6,3F~%" t y)))


;; Example: Newton's cooling law, f(t,T) = -0.07*(T-20)
(defun newton-cooling (time T) (* -0.07 (- T 20)))

;; Generate the data for all three step sizes (2,5 and 10).
(euler #'newton-cooling 100 0 100  2)
(euler #'newton-cooling 100 0 100  5)
(euler #'newton-cooling 100 0 100 10)
```



```lisp
;; slightly more idiomatic Common Lisp version

(defun newton-cooling (time temperature)
  "Newton's cooling law, f(t,T) = -0.07*(T-20)"
  (declare (ignore time))
  (* -0.07 (- temperature 20)))

(defun euler (f y0 a b h)
  "Euler's Method.
Approximates y(time) in y'(time)=f(time,y) with y(a)=y0 and t=a..b and the step size h."
  (loop for time from a below b by h
        for y = y0 then (+ y (* h (funcall f time y)))
        do (format t "~6,3F  ~6,3F~%" time y)))
```



```txt

Example output:

 0.000  100.000
10.000  44.000
20.000  27.200
30.000  22.160
40.000  20.648
50.000  20.194
60.000  20.058
70.000  20.017
80.000  20.005
90.000  20.002

```



## D


```d
import std.stdio, std.range, std.traits;

/// Approximates y(t) in y'(t)=f(t,y) with y(a)=y0 and t=a..b and the step size h.
void euler(F)(in F f, in double y0, in double a, in double b, in double h) @safe
if (isCallable!F && __traits(compiles, { real r = f(0.0, 0.0); })) {
    double y = y0;
    foreach (immutable t; iota(a, b, h)) {
        writefln("%.3f  %.3f", t, y);
        y += h * f(t, y);
    }
    "done".writeln;
}

void main() {
    /// Example: Newton's cooling law.
    enum newtonCoolingLaw = (in double time, in double t)
        pure nothrow @safe @nogc => -0.07 * (t - 20);

    euler(newtonCoolingLaw, 100, 0, 100,  2);
    euler(newtonCoolingLaw, 100, 0, 100,  5);
    euler(newtonCoolingLaw, 100, 0, 100, 10);
}
```

Last part of the output:

```txt
...
0.000  100.000
10.000  44.000
20.000  27.200
30.000  22.160
40.000  20.648
50.000  20.194
60.000  20.058
70.000  20.017
80.000  20.005
90.000  20.002
done
```



## Delphi


[[Euler_method#Pascal | Pascal]]


## Elixir

Translated from Ruby

```elixir
defmodule Euler do
  def method(_, _, t, b, _) when t>b, do: :ok
  def method(f, y, t, b, h) do
    :io.format "~7.3f ~7.3f~n", [t,y]
    method(f, y + h * f.(t,y), t + h, b, h)
  end
end

f = fn _time, temp -> -0.07 * (temp - 20) end
Enum.each([10, 5, 2], fn step ->
  IO.puts "\nStep = #{step}"
  Euler.method(f, 100.0, 0.0, 100.0, step)
end)
```


Output:

```txt
Step = 10
  0.000 100.000
 10.000  44.000
 20.000  27.200
 30.000  22.160
 40.000  20.648
 50.000  20.194
 60.000  20.058
 70.000  20.017
 80.000  20.005
 90.000  20.002
100.000  20.000

Step = 5
  0.000 100.000
  5.000  72.000
 10.000  53.800
 15.000  41.970
 20.000  34.280
 25.000  29.282
 30.000  26.034
 35.000  23.922
 40.000  22.549
 45.000  21.657
 50.000  21.077
 55.000  20.700
 60.000  20.455
 65.000  20.296
 70.000  20.192
 75.000  20.125
 80.000  20.081
 85.000  20.053
 90.000  20.034
 95.000  20.022
100.000  20.014

Step = 2
  0.000 100.000
  2.000  88.800
  4.000  79.168
  6.000  70.884
  8.000  63.761
 10.000  57.634
 12.000  52.365
 14.000  47.834
 16.000  43.937
 18.000  40.586
 20.000  37.704
 22.000  35.226
 24.000  33.094
 26.000  31.261
 28.000  29.684
 30.000  28.328
 32.000  27.163
 34.000  26.160
 36.000  25.297
 38.000  24.556
 40.000  23.918
 42.000  23.369
 44.000  22.898
 46.000  22.492
 48.000  22.143
 50.000  21.843
 52.000  21.585
 54.000  21.363
 56.000  21.172
 58.000  21.008
 60.000  20.867
 62.000  20.746
 64.000  20.641
 66.000  20.551
 68.000  20.474
 70.000  20.408
 72.000  20.351
 74.000  20.302
 76.000  20.259
 78.000  20.223
 80.000  20.192
 82.000  20.165
 84.000  20.142
 86.000  20.122
 88.000  20.105
 90.000  20.090
 92.000  20.078
 94.000  20.067
 96.000  20.057
 98.000  20.049
100.000  20.042

```



## Erlang



```erlang

-module(euler).
-export([main/0, euler/5]).

cooling(_Time, Temperature) ->
	(-0.07)*(Temperature-20).

euler(_, Y, T, _, End) when End == T ->
	io:fwrite("\n"),
	Y;

euler(Func, Y, T, Step, End) ->
	if
		T rem 10 == 0 ->
			io:fwrite("~.3f  ",[float(Y)]);
		true ->
			ok
	end,
	euler(Func, Y + Step * Func(T, Y), T + Step, Step, End).

analytic(T, End) when T == End ->
	io:fwrite("\n"),
	T;

analytic(T, End) ->
	Y = (20 + 80 * math:exp(-0.07 * T)),
	io:fwrite("~.3f  ", [Y]),
	analytic(T+10, End).

main() ->
	io:fwrite("Analytic:\n"),
	analytic(0, 100),
	io:fwrite("Step 2:\n"),
	euler(fun cooling/2, 100, 0, 2, 100),
	io:fwrite("Step 5:\n"),
	euler(fun cooling/2, 100, 0, 5, 100),
	io:fwrite("Step 10:\n"),
	euler(fun cooling/2, 100, 0, 10, 100),
	ok.

```

Output:

```txt

Analytic:
100.000  59.727  39.728  29.797  24.865  22.416  21.200  20.596  20.296  20.147
Step 2:
100.000  57.634  37.704  28.328  23.918  21.843  20.867  20.408  20.192  20.090
Step 5:
100.000  53.800  34.280  26.034  22.549  21.077  20.455  20.192  20.081  20.034
Step 10:
100.000  44.000  27.200  22.160  20.648  20.194  20.058  20.017  20.005  20.002
ok

```



## Euler Math Toolbox



```Euler Math Toolbox

>function dgleuler (f,x,y0) ...
$  y=zeros(size(x)); y[1]=y0;
$  for i=2 to cols(y);
$  y[i]=y[i-1]+f(x[i-1],y[i-1])*(x[i]-x[i-1]);
$  end;
$  return y;
$endfunction
>function f(x,y) := -k*(y-TR)
>k=0.07; TR=20; TS=100;
>x=0:1:100; dgleuler("f",x,TS)[-1]
 20.0564137335
>x=0:2:100; dgleuler("f",x,TS)[-1]
 20.0424631834
>TR+(TS-TR)*exp(-k*TS)
 20.0729505572
>x=0:5:100; plot2d(x,dgleuler("f",x,TS)); ...
>  plot2d(x,TR+(TS-TR)*exp(-k*x),>add,color=red);
>ode("f",x,TS)[-1] // Euler default solver LSODA
 20.0729505568
>adaptiverunge("f",x,TS)[-1] // Adaptive Runge Method
 20.0729505572

```


## F\#

```fsharp
let euler f (h : float) t0 y0 =
    (t0, y0)
    |> Seq.unfold (fun (t, y) -> Some((t,y), ((t + h), (y + h * (f t y)))))

let newtonCoolíng _ y = -0.07 * (y - 20.0)

[<EntryPoint>]
let main argv =
    let f  = newtonCoolíng
    let a = 0.0
    let y0 = 100.0
    let b = 100.0
    let h = 10.0
    (euler newtonCoolíng h a y0)
    |> Seq.takeWhile (fun (t,_) -> t <= b)
    |> Seq.iter (printfn "%A")
    0
```

Output for the above (step size 10)

```txt
(0.0, 100.0)
(10.0, 44.0)
(20.0, 27.2)
(30.0, 22.16)
(40.0, 20.648)
(50.0, 20.1944)
(60.0, 20.05832)
(70.0, 20.017496)
(80.0, 20.0052488)
(90.0, 20.00157464)
(100.0, 20.00047239)
```



## Factor


```factor
USING: formatting fry io kernel locals math math.ranges
sequences ;
IN: rosetta-code.euler-method

:: euler ( quot y! a b h -- )
    a b h <range> [
        :> t
        t y "%7.3f %7.3f\n" printf
        t y quot call h * y + y!
    ] each ; inline

: cooling ( t y -- x ) nip 20 - -0.07 * ;

: euler-method-demo ( -- )
   2 5 10 [ '[ [ cooling ] 100 0 100 _ euler ] call nl ] tri@ ;

MAIN: euler-method-demo
```

Output:

```txt

. . .
  0.000 100.000
 10.000  44.000
 20.000  27.200
 30.000  22.160
 40.000  20.648
 50.000  20.194
 60.000  20.058
 70.000  20.017
 80.000  20.005
 90.000  20.002
100.000  20.000

```



## Forth


```forth
: newton-cooling-law ( f: temp -- f: temp' )
  20e f-  -0.07e f* ;

: euler ( f: y0  xt step end -- )
  1+ 0 do
    cr i . fdup f.
    fdup over execute
    dup s>f f* f+
  dup +loop
  2drop fdrop ;

100e  ' newton-cooling-law  2 100 euler cr
100e  ' newton-cooling-law  5 100 euler cr
100e  ' newton-cooling-law 10 100 euler cr
```



## Fortran

Works with Fortran 2008

```fortran
program euler_method
use iso_fortran_env, only: real64
implicit none

abstract interface
  ! a derivative dy/dt as function of y and t
  function derivative(y, t)
    use iso_fortran_env, only: real64
    real(real64) :: derivative
    real(real64), intent(in) :: t, y
  end function
end interface

real(real64), parameter :: T_0 = 100, T_room = 20, k = 0.07, a = 0, b = 100, &
    h(3) = [2.0, 5.0, 10.0]

integer :: i

! loop over all step sizes
do i = 1, 3
  call euler(newton_cooling, T_0, a, b, h(i))
end do

contains

! Approximates y(t) in y'(t) = f(y, t) with y(a) = y0 and t = a..b and the
! step size h.
subroutine euler(f, y0, a, b, h)
  procedure(derivative) :: f
  real(real64), intent(in) :: y0, a, b, h
  real(real64) :: t, y

  if (a > b) return
  if (h <= 0) stop "negative step size"

  print '("# h = ", F0.3)', h

  y = y0
  t = a

  do
    print *, t, y
    t = t + h
    if (t > b) return
    y = y + h * f(y, t)
  end do
end subroutine


! Example: Newton's cooling law, f(T, _) = -k*(T - T_room)
function newton_cooling(T, unused) result(dTdt)
  real(real64) :: dTdt
  real(real64), intent(in) :: T, unused
  dTdt = -k * (T - T_room)
end function

end program
```

Output for <code>h = 10</code>:

```txt

# h = 10.000
   0.0000000000000000        100.00000000000000
   10.000000000000000        43.999999761581421
   20.000000000000000        27.199999856948853
   30.000000000000000        22.159999935626985
   40.000000000000000        20.647999974250794
   50.000000000000000        20.194399990344049
   60.000000000000000        20.058319996523856
   70.000000000000000        20.017495998783350
   80.000000000000000        20.005248799582862
   90.000000000000000        20.001574639859214
   100.00000000000000        20.000472391953071

```



## Futhark


Specialised to the cooling function.  We produce an array of the
temperature at each step subtracted from the analytically
determined temperature (so we are computing the error).


```Futhark

let analytic(t0: f64) (time: f64): f64 =
  20.0 + (t0 - 20.0) * f64.exp(-0.07*time)

let cooling(_time: f64) (temperature: f64): f64 =
  -0.07 * (temperature-20.0)

let main(t0: f64) (a: f64) (b: f64) (h: f64): []f64 =
  let steps = i32.f64 ((b-a)/h)
  let temps = replicate steps 0.0
  let (_,temps) = loop (t,temps)=(t0,temps) for i < steps do
    let x = a + f64.i32 i * h
    let temps[i] = f64.abs(t-analytic t0 x)
    in (t + h * cooling x t,
        temps)
 in temps

```



## Go


```go
package main

import (
    "fmt"
    "math"
)

// fdy is a type for function f used in Euler's method.
type fdy func(float64, float64) float64

// eulerStep computes a single new value using Euler's method.
// Note that step size h is a parameter, so a variable step size
// could be used.
func eulerStep(f fdy, x, y, h float64) float64 {
    return y + h*f(x, y)
}

// Definition of cooling rate.  Note that this has general utility and
// is not specific to use in Euler's method.

// newCoolingRate returns a function that computes cooling rate
// for a given cooling rate constant k.
func newCoolingRate(k float64) func(float64) float64 {
    return func(deltaTemp float64) float64 {
        return -k * deltaTemp
    }
}

// newTempFunc returns a function that computes the analytical solution
// of cooling rate integrated over time.
func newTempFunc(k, ambientTemp, initialTemp float64) func(float64) float64 {
    return func(time float64) float64 {
        return ambientTemp + (initialTemp-ambientTemp)*math.Exp(-k*time)
    }
}

// newCoolingRateDy returns a function of the kind needed for Euler's method.
// That is, a function representing dy(x, y(x)).
//
// Parameters to newCoolingRateDy are cooling constant k and ambient
// temperature.
func newCoolingRateDy(k, ambientTemp float64) fdy {
    crf := newCoolingRate(k)
    // note that result is dependent only on the object temperature.
    // there are no additional dependencies on time, so the x parameter
    // provided by eulerStep is unused.
    return func(_, objectTemp float64) float64 {
        return crf(objectTemp - ambientTemp)
    }
}

func main() {
    k := .07
    tempRoom := 20.
    tempObject := 100.
    fcr := newCoolingRateDy(k, tempRoom)
    analytic := newTempFunc(k, tempRoom, tempObject)
    for _, deltaTime := range []float64{2, 5, 10} {
        fmt.Printf("Step size = %.1f\n", deltaTime)
        fmt.Println(" Time Euler's Analytic")
        temp := tempObject
        for time := 0.; time <= 100; time += deltaTime {
            fmt.Printf("%5.1f %7.3f %7.3f\n", time, temp, analytic(time))
            temp = eulerStep(fcr, time, temp, deltaTime)
        }
        fmt.Println()
    }
}
```

Output, truncated:

```txt

...
 85.0  20.053  20.208
 90.0  20.034  20.147
 95.0  20.022  20.104
100.0  20.014  20.073

Step size = 10.0
 Time Euler's Analytic
  0.0 100.000 100.000
 10.0  44.000  59.727
 20.0  27.200  39.728
 30.0  22.160  29.797
 40.0  20.648  24.865
 50.0  20.194  22.416
 60.0  20.058  21.200
 70.0  20.017  20.596
 80.0  20.005  20.296
 90.0  20.002  20.147
100.0  20.000  20.073

```



## Groovy

'''Generic Euler Method Solution'''

The following is a general solution for using the Euler method to produce a finite discrete sequence of points approximating the ODE solution for ''y'' as a function of ''x''.


In the ''eulerStep'' closure argument list: ''x<sub>n</sub>'' and ''y<sub>n</sub>'' together are the previous point in the sequence. ''h'' is the step distance to the next point's ''x'' value. ''dydx'' is a closure representing the derivative of ''y'' as a function of ''x'', itself expressed (as required by the method) as a function of ''x'' and ''y(x)''.


The ''eulerMapping'' closure produces an entire approximating sequence, expressed as a Map object. Here, ''x<sub>0</sub>'' and ''y<sub>0</sub>'' together are the first point in the sequence, the ODE initial conditions. ''h'' and ''dydx'' are again the step distance and the derivative closure. ''stopCond'' is a closure representing a "stop condition" that causes the the ''eulerMapping'' closure to stop after a finite number of steps; the given default value causes ''eulerMapping'' to stop after 100 steps.

```groovy
def eulerStep = { xn, yn, h, dydx ->
    (yn + h * dydx(xn, yn)) as BigDecimal
}

Map eulerMapping = { x0, y0, h, dydx, stopCond = { xx, yy, hh, xx0 -> abs(xx - xx0) > (hh * 100)  }.rcurry(h, x0) ->
    Map yMap = [:]
    yMap[x0] = y0 as BigDecimal
    def x = x0
    while (!stopCond(x, yMap[x])) {
        yMap[x + h] = eulerStep(x, yMap[x], h, dydx)
        x += h
    }
    yMap
}
assert eulerMapping.maximumNumberOfParameters == 5
```



'''Specific Euler Method Solution for the "Temperature Diffusion" Problem''' (with Newton's derivative formula and constants for environment temperature and object conductivity given)

```groovy
def dtdsNewton = { s, t, tR, k ->  k * (tR - t) }
assert dtdsNewton.maximumNumberOfParameters == 4

def dtds = dtdsNewton.rcurry(20, 0.07)
assert dtds.maximumNumberOfParameters == 2

def tEulerH = eulerMapping.rcurry(dtds) { s, t -> s >= 100 }
assert tEulerH.maximumNumberOfParameters == 3
```



'''Newton's Analytic Temperature Diffusion Solution''' (for comparison)

```groovy
def tNewton = { s, s0, t0, tR, k ->
    tR + (t0 - tR) * Math.exp(k * (s0 - s))
}
assert tNewton.maximumNumberOfParameters == 5

def tAnalytic = tNewton.rcurry(0, 100, 20, 0.07)
assert tAnalytic.maximumNumberOfParameters == 1
```



'''Specific solutions for 3 step sizes''' (and initial time and temperature)

```groovy
[10, 5, 2].each { h ->
    def tEuler = tEulerH.rcurry(h)
    assert tEuler.maximumNumberOfParameters == 2
    println """
STEP SIZE == ${h}
  time   analytic   euler   relative
(seconds)  (°C)     (°C)     error
-------- -------- -------- ---------"""
    tEuler(0, 100).each { BigDecimal s, tE ->
        def tA = tAnalytic(s)
        def relError = ((tE - tA)/(tA - 20)).abs()
        printf('%5.0f    %8.4f %8.4f %9.6f\n', s, tA, tE, relError)
    }
}
```



'''Selected output'''

```txt
STEP SIZE == 10
  time   analytic   euler   relative
(seconds)  (°C)     (°C)     error
-------- -------- -------- ---------
    0    100.0000 100.0000  0.000000
   10     59.7268  44.0000  0.395874
   20     39.7278  27.2000  0.635032
   30     29.7965  22.1600  0.779513
   40     24.8648  20.6480  0.866798
   50     22.4158  20.1944  0.919529
   60     21.1996  20.0583  0.951386
   70     20.5957  20.0175  0.970631
   80     20.2958  20.0052  0.982257
   90     20.1469  20.0016  0.989281
  100     20.0730  20.0005  0.993524

STEP SIZE == 5
  time   analytic   euler   relative
(seconds)  (°C)     (°C)     error
-------- -------- -------- ---------
    0    100.0000 100.0000  0.000000
     ... yada, yada, yada ...
  100     20.0730  20.0145  0.801240

STEP SIZE == 2
  time   analytic   euler   relative
(seconds)  (°C)     (°C)     error
-------- -------- -------- ---------
    0    100.0000 100.0000  0.000000
     ... yada, yada, yada ...
  100     20.0730  20.0425  0.417918
```

Notice how the relative error in the Euler method sequences increases over time in spite of the fact that all three the Euler approximations and the analytic solution are approaching the same asymptotic limit of 20°C.


Notice also how smaller step size reduces the relative error in the approximation.


## Haskell

Modular solution which separates the solver and a method. Moreover it works on a given mesh which can be irregular.

```Haskell
-- the solver
dsolveBy _ _ [] _ = error "empty solution interval"
dsolveBy method f mesh x0 = zip mesh results
  where results = scanl (method f) x0 intervals
        intervals = zip mesh (tail mesh)
```


It is better to use strict <code>Data.List.scanl'</code> in the solver but avoiding highlighting problems we leave lazy <code>scanl</code> function.

Some possible methods:


```haskell
-- 1-st order Euler
euler f x (t1,t2) = x + (t2 - t1) * f t1 x

-- 2-nd order Runge-Kutta
rk2 f x (t1,t2) = x + h * f (t1 + h/2) (x + h/2*f t1 x)
  where h = t2 - t1

-- 4-th order Runge-Kutta
rk4 f x (t1,t2) = x + h/6 * (k1 + 2*k2 + 2*k3 + k4)
  where k1 = f t1 x
        k2 = f (t1 + h/2) (x + h/2*k1)
        k3 = f (t1 + h/2) (x + h/2*k2)
        k4 = f (t1 + h) (x + h*k3)
        h = t2 - t1
```


Graphical output, using EasyPlot:


```haskell
import Graphics.EasyPlot

newton t temp = -0.07 * (temp - 20)

exactSolution t = 80*exp(-0.07*t)+20

test1 = plot (PNG "euler1.png")
  [ Data2D [Title "Step 10", Style Lines] [] sol1
  , Data2D [Title "Step 5", Style Lines] [] sol2
  , Data2D [Title "Step 1", Style Lines] [] sol3
  , Function2D [Title "exact solution"] [Range 0 100] exactSolution ]
 where sol1 = dsolveBy euler newton [0,10..100] 100
       sol2 = dsolveBy euler newton [0,5..100] 100
       sol3 = dsolveBy euler newton [0,1..100] 100

test2 = plot (PNG "euler2.png")
  [ Data2D [Title "Euler"] [] sol1
  , Data2D [Title "RK2"] [] sol2
  , Data2D [Title "RK4"] [] sol3
  , Function2D [Title "exact solution"] [Range 0 100] exactSolution ]
 where sol1 = dsolveBy euler newton [0,10..100] 100
       sol2 = dsolveBy rk2 newton [0,10..100] 100
       sol3 = dsolveBy rk4 newton [0,10..100] 100
```


=={{header|Icon}} and {{header|Unicon}}==

Translated from Common Lisp

This solution works in both Icon and Unicon.  It takes advantage of the <code>proc</code> procedure, which converts a string naming a procedure into a call to that procedure.


```Icon

invocable "newton_cooling" # needed to use the 'proc' procedure

procedure euler (f, y0, a, b, h)
  t := a
  y := y0
  until (t >= b) do {
    write (right(t, 4) || " " || left(y, 7))
    t +:= h
    y +:= h * (proc(f) (t, y)) # 'proc' applies procedure named in f to (t, y)
  }
  write ("DONE")
end

procedure newton_cooling (time, T)
  return -0.07 * (T - 20)
end

procedure main ()
  # generate data for all three step sizes [2, 5, 10]
  every (step_size := ![2,5,10]) do
    euler ("newton_cooling", 100, 0, 100,  step_size)
end

```


Sample output:

```txt

   0 100
  10 44.0
  20 27.2
  30 22.16
  40 20.648
  50 20.1944
  60 20.0583
  70 20.0174
  80 20.0052
  90 20.0015
DONE


```



## J

'''Solution:'''

```j
NB.*euler a Approximates Y(t) in Y'(t)=f(t,Y) with Y(a)=Y0 and t=a..b and step size h.
euler=: adverb define
 'Y0 a b h'=. 4{. y
 t=. i.@>:&.(%&h) b - a
 Y=. (+ h * u)^:(<#t) Y0
 t,.Y
)

ncl=: _0.07 * -&20  NB. Newton's Cooling Law

```

'''Example:'''

```j
   ncl euler 100 0 100 2
...                       NB. output redacted for brevity
   ncl euler 100 0 100 5
...                       NB. output redacted for brevity
   ncl euler 100 0 100 10
  0     100
 10      44
 20    27.2
 30   22.16
 40  20.648
 50 20.1944
 60 20.0583
 70 20.0175
 80 20.0052
 90 20.0016
100 20.0005
```



## Java



```java

public class Euler {
  private static void euler (Callable f, double y0, int a, int b, int h) {
    int t = a;
    double y = y0;
    while (t < b) {
      System.out.println ("" + t + " " + y);
      t += h;
      y += h * f.compute (t, y);
    }
    System.out.println ("DONE");
  }

  public static void main (String[] args) {
    Callable cooling = new Cooling ();
    int[] steps = {2, 5, 10};
    for (int stepSize : steps) {
      System.out.println ("Step size: " + stepSize);
      euler (cooling, 100.0, 0, 100, stepSize);
    }
  }
}

// interface used so we can plug in alternative functions to Euler
interface Callable {
  public double compute (int time, double t);
}

// class to implement the newton cooling equation
class Cooling implements Callable {
  public double compute (int time, double t) {
    return -0.07 * (t - 20);
  }
}

```


Output for step = 10;

```txt

Step size: 10
0 100.0
10 43.99999999999999
20 27.199999999999996
30 22.159999999999997
40 20.648
50 20.194399999999998
60 20.05832
70 20.017496
80 20.0052488
90 20.00157464
DONE

```



## JavaScript

Translated from Python

```javascript

// Function that takes differential-equation, initial condition,
// ending x, and step size as parameters
function eulersMethod(f, x1, y1, x2, h) {
	// Header
	console.log("\tX\t|\tY\t");
	console.log("------------------------------------");

	// Initial Variables
	var x=x1, y=y1;

	// While we're not done yet
	// Both sides of the OR let you do Euler's Method backwards
	while ((x<x2 && x1<x2) || (x>x2 && x1>x2)) {
		// Print what we have
		console.log("\t" + x + "\t|\t" + y);

		// Calculate the next values
		y += h*f(x, y)
		x += h;
	}

	return y;
}

function cooling(x, y) {
	return -0.07 * (y-20);
}

eulersMethod(cooling, 0, 100, 100, 10);

```


## jq

Works with jq 1.4

```jq
# euler_method takes a filter (df), initial condition
# (x1,y1), ending x (x2), and step size as parameters;
# it emits the y values at each iteration.
# df must take [x,y] as its input.
def euler_method(df; x1; y1; x2; h):
  h as $h
  | [x1, y1]
  | recurse( if ((.[0] < x2 and x1 < x2) or
                 (.[0] > x2 and x1 > x2)) then
  		[ (.[0] + $h), (.[1] + $h*df) ]
             else empty
             end )
  | .[1] ;


# We could now solve the task by writing for each step-size, $h
# euler_method(-0.07 * (.[1]-20); 0; 100; 100; $h)
# but for clarity, we shall define a function named "cooling":

# [x,y] is input
def cooling: -0.07 * (.[1]-20);

# The following solves the task:
# (2,5,10) | [., [ euler_method(cooling; 0; 100; 100; .) ] ]

```

For brevity, we modify euler_method so that it only shows the final value of y:

```jq

def euler_solution(df; x1; y1; x2; h):
  def recursion(exp): reduce recurse(exp) as $x (.; $x);
  h as $h
  | [x1, y1]
  | recursion( if ((.[0] < x2 and x1 < x2) or
                   (.[0] > x2 and x1 > x2)) then
  		[ (.[0] + $h), (.[1] + $h*df) ]
             else empty
             end )
  | .[1] ;
```

'''Example''':

```jq
(1,2,5,10,20) | [., [ euler_solution(cooling; 0; 100; 100; .) ] ]
```

Output:

```sh
$ jq -M -n -c -f Euler_method.jq
[1,[20.05641373347389]]
[2,[20.0424631833732]]
[5,[20.01449963666907]]
[10,[20.000472392]]
[20,[19.180799999999998]]
```



## Julia

Works with Julia 0.6

```julia
euler(f::Function, T::Number, t0::Int, t1::Int, h::Int) = collect(begin T += h * f(T); T end for t in t0:h:t1)

# Prints a series of arbitrary values in a tabular form, left aligned in cells with a given width
tabular(width, cells...) = println(join(map(s -> rpad(s, width), cells)))

# prints the table according to the task description for h=5 and 10 sec
for h in (5, 10)
    print("Step $h:\n\n")
    tabular(15, "Time", "Euler", "Analytic")
    t = 0
    for T in euler(y -> -0.07 * (y - 20.0), 100.0, 0, 100, h)
        tabular(15, t, round(T,6), round(20.0 + 80.0 * exp(-0.07t), 6))
        t += h
    end
    println()
end
```


Output:

```txt
Step 5:

Time           Euler          Analytic
0              72.0           100.0
5              53.8           76.375047
10             41.97          59.726824
15             34.2805        47.99502
20             29.282325      39.727757
25             26.033511      33.901915
30             23.921782      29.796514
35             22.549159      26.903487
40             21.656953      24.864805
45             21.077019      23.42817
50             20.700063      22.415791
55             20.455041      21.702379
60             20.295776      21.199646
65             20.192255      20.845376
70             20.124966      20.595727
75             20.081228      20.419801
80             20.052798      20.295829
85             20.034319      20.208467
90             20.022307      20.146904
95             20.0145        20.103522
100            20.009425      20.072951

Step 10:

Time           Euler          Analytic
0              44.0           100.0
10             27.2           59.726824
20             22.16          39.727757
30             20.648         29.796514
40             20.1944        24.864805
50             20.05832       22.415791
60             20.017496      21.199646
70             20.005249      20.595727
80             20.001575      20.295829
90             20.000472      20.146904
100            20.000142      20.072951
```



## Kotlin

Translated from C

```scala
// version 1.1.2

typealias Deriv = (Double) -> Double  // only one parameter needed here

const val FMT = " %7.3f"

fun euler(f: Deriv, y: Double, step: Int, end: Int) {
    var yy = y
    print(" Step %2d: ".format(step))
    for (t in 0..end step step) {
        if (t % 10 == 0) print(FMT.format(yy))
        yy += step * f(yy)
    }
    println()
}

fun analytic() {
    print("    Time: ")
    for (t in 0..100 step 10) print(" %7d".format(t))
    print("\nAnalytic: ")
    for (t in 0..100 step 10)
        print(FMT.format(20.0 + 80.0 * Math.exp(-0.07 * t)))
    println()
}

fun cooling(temp: Double) = -0.07 * (temp - 20.0)

fun main(args: Array<String>) {
    analytic()
    for (i in listOf(2, 5, 10))
        euler(::cooling, 100.0, i, 100)
}
```


Output:

```txt

    Time:        0      10      20      30      40      50      60      70      80      90     100
Analytic:  100.000  59.727  39.728  29.797  24.865  22.416  21.200  20.596  20.296  20.147  20.073
 Step  2:  100.000  57.634  37.704  28.328  23.918  21.843  20.867  20.408  20.192  20.090  20.042
 Step  5:  100.000  53.800  34.280  26.034  22.549  21.077  20.455  20.192  20.081  20.034  20.014
 Step 10:  100.000  44.000  27.200  22.160  20.648  20.194  20.058  20.017  20.005  20.002  20.000

```



## Lua


```lua
T0 = 100
TR = 20
k = 0.07
delta_t = { 2, 5, 10 }
n = 100

NewtonCooling = function( t ) return -k * ( t - TR ) end


function Euler( f, y0, n, h )
    local y = y0
    for x = 0, n, h do
	print( "", x, y )
 	y = y + h * f( y )
    end
end


for i = 1, #delta_t do
    print( "delta_t = ", delta_t[i] )
    Euler( NewtonCooling, T0, n, delta_t[i] )
end

```


## Mathematica / Wolfram Language

Better methods for differential equation solving are built into Mathematica, so the typical user would omit the Method and StartingStepSize options in the code below.
However since the task requests Eulers method, here is the bad solution...

```Mathematica

euler[step_, val_] := NDSolve[{T'[t] == -0.07 (T[t] - 20), T[0] == 100}, T, {t, 0, 100}, Method -> "ExplicitEuler", StartingStepSize -> step][[1, 1, 2]][val]

```

Output:

```txt
euler[2, 100]
20.0425

euler[5, 100]
20.0145

euler[10, 100]
20.0005
```



## Maxima


```Maxima
euler_method(f, y0, a, b, h):= block(
  [t: a, y: y0, tg: [a], yg: [y0]],
  unless t>=b do (
    t: t + h,
    y: y + f(t, y)*h,
    tg: endcons(t, tg),
    yg: endcons(y, yg)
    ),
  [tg, yg]
  );

/* initial temperature */
T0: 100;

/* environment of temperature */
Tr: 20;

/* the cooling constant */
k: 0.07;

/* end of integration */
tmax: 100;

/* analytical solution */
Tref(t):= Tr + (T0 - Tr)*exp(-k*t);

/* cooling rate */
dT(t, T):= -k*(T-Tr);

/* get numerical solution */
h: 10;
[tg, yg]: euler_method('dT, T0, 0, tmax,  h);

/* plot analytical and numerical solution */
plot2d([Tref, [discrete, tg, yg]], ['t, 0, tmax],
  [legend, "analytical", concat("h = ", h)],
  [xlabel, "t / seconds"],
  [ylabel, "Temperature / C"]);

```


## МК-61/52

```mk-61-52
П2	С/П	П3	С/П	П4	ПП	19	ИП3	*	ИП4
+	П4	С/П	ИП2	ИП3	+	П2	БП	05	...
...	...	...	...	...	...	...	...	...	В/О
```

Instead of dots typed calculation program equation ''f(u, t)'',
where the arguments are ''t'' = Р2, ''u'' = Р4.

Input: ''Initial time'' С/П ''Time step'' С/П ''Initial value'' С/П.

The result is displayed on the indicator.


## Nim


```nim
import strutils

proc euler(f: proc (x,y: float): float; y0, a, b, h: float) =
  var (t,y) = (a,y0)
  while t < b:
    echo formatFloat(t, ffDecimal, 3), " ", formatFloat(y, ffDecimal, 3)
    t += h
    y += h * f(t,y)

proc newtoncooling(time, temp): float =
  -0.07 * (temp - 20)

euler(newtoncooling, 100.0, 0.0, 100.0, 10.0)
```

Output:

```txt
0.000 100.000
10.000 44.000
20.000 27.200
30.000 22.160
40.000 20.648
50.000 20.194
60.000 20.058
70.000 20.017
80.000 20.005
90.000 20.002
```



## Objeck


```objeck

class EulerMethod {
  T0 : static : Float;
  TR : static : Float;
  k : static : Float;
  delta_t : static : Float[];
  n : static : Float;

  function : Main(args : String[]) ~ Nil {
    T0 := 100;
    TR := 20;
    k := 0.07;
    delta_t := [2.0, 5.0, 10.0];
    n := 100;

    f := NewtonCooling(Float) ~ Float;
    for(i := 0; i < delta_t->Size(); i+=1;) {
      IO.Console->Print("delta_t = ")->PrintLine(delta_t[i]);
      Euler(f, T0, n->As(Int), delta_t[i]);
    };
  }

  function : native : NewtonCooling(t : Float) ~ Float {
    return -1 * k * (t-TR);
  }

  function : native : Euler(f : (Float) ~ Float, y : Float, n : Int, h : Float) ~ Nil {
    for(x := 0; x<=n; x+=h;) {
      IO.Console->Print("\t")->Print(x)->Print("\t")->PrintLine(y);
      y += h * f(y);
    };
  }
}

```


Output:

```txt

delta_t = 2
        0       100
        2       88.8
        4       79.168
        6       70.88448
        ...
delta_t = 10
        0       100
        10      44
        20      27.2
        30      22.16
        40      20.648

```



## OCaml



```OCaml
(* Euler integration by recurrence relation.
 * Given a function, and stepsize, provides a function of (t,y) which
 * returns the next step: (t',y'). *)
let euler f ~step (t,y) = ( t+.step, y +. step *. f t y )

(* newton_cooling doesn't use time parameter, so _ is a placeholder *)
let newton_cooling ~k ~tr _ y = -.k *. (y -. tr)

(* analytic solution for Newton cooling *)
let analytic_solution ~k ~tr ~t0 t = tr +. (t0 -. tr) *. exp (-.k *. t)
```


Using the above functions to produce the task results:

```OCaml
(* Wrapping up the parameters in a "cool" function: *)
let cool = euler (newton_cooling ~k:0.07 ~tr:20.)

(* Similarly for the analytic solution: *)
let analytic = analytic_solution ~k:0.07 ~tr:20. ~t0:100.

(* (Just a loop) Apply recurrence function on state, until some condition *)
let recur ~until f state =
  let rec loop s =
    if until s then ()
    else loop (f s)
  in loop state

(* 'results' generates the specified output starting from initial values t=0, temp=100C; ending at t=100s *)
let results fn =
  Printf.printf "\t  time\t euler\tanalytic\n%!";
  let until (t,y) =
    Printf.printf "\t%7.3f\t%7.3f\t%9.5f\n%!" t y (analytic t);
    t >= 100.
  in recur ~until fn (0.,100.)

results (cool ~step:10.)
results (cool ~step:5.)
results (cool ~step:2.)
```


Example output:

```txt

# results (cool ~step:10.);;
	  time	 euler	analytic
	  0.000	100.000	100.00000
	 10.000	 44.000	 59.72682
	 20.000	 27.200	 39.72776
	 30.000	 22.160	 29.79651
	 40.000	 20.648	 24.86481
	 50.000	 20.194	 22.41579
	 60.000	 20.058	 21.19965
	 70.000	 20.017	 20.59573
	 80.000	 20.005	 20.29583
	 90.000	 20.002	 20.14690
	100.000	 20.000	 20.07295
- : unit = ()

```


## Oforth

```oforth
: euler(f, y, a, b, h)
| t |
   a b h step: t [
      System.Out t <<wjp(6, JUSTIFY_RIGHT, 3) " : " << y << cr
      t y f perform h * y + ->y
      ] ;
```


Usage :


```oforth
: newtonCoolingLaw(t, y)
   y 20 - -0.07 * ;

: test
   euler(#newtonCoolingLaw, 100.0, 0.0, 100.0,  2)
   euler(#newtonCoolingLaw, 100.0, 0.0, 100.0,  5)
   euler(#newtonCoolingLaw, 100.0, 0.0, 100.0, 10) ;
```


Output:

```txt

....
     0 : 100
    10 : 44
    20 : 27.2
    30 : 22.16
    40 : 20.648
    50 : 20.1944
    60 : 20.05832
    70 : 20.017496
    80 : 20.0052488
    90 : 20.00157464
   100 : 20.000472392

```



## Pascal

Translated from C

Euler code for Free Pascal - Delphi mode.
Apart from the function-pointer calling convention for the NewtonCooling method,
this example is ISO-7185 standard Pascal.


```Pascal
{$mode delphi}
PROGRAM Euler;

TYPE TNewtonCooling = FUNCTION (t: REAL) : REAL;

CONST	T0	: REAL = 100.0;
CONST	TR	: REAL = 20.0;
CONST	k 	: REAL = 0.07;
CONST	time    : INTEGER = 100;
CONST	step    : INTEGER = 10;
CONST	dt	: ARRAY[0..3] of REAL = (1.0,2.0,5.0,10.0);

VAR	i	: INTEGER;

FUNCTION NewtonCooling(t: REAL) : REAL;
	BEGIN
		NewtonCooling := -k * (t-TR);
	END;

PROCEDURE Euler(F: TNewtonCooling; y, h : REAL; n: INTEGER);
	VAR i: INTEGER = 0;
	BEGIN
		WRITE('dt=',trunc(h):2,':');
		REPEAT
			IF (i mod 10 = 0) THEN WRITE(' ',y:2:3);
			INC(i,trunc(h));
			y := y + h * F(y);
		UNTIL (i >= n);
		WRITELN;
	END;

PROCEDURE Sigma;
	VAR t: INTEGER = 0;
	BEGIN
		WRITE('Sigma:');
		REPEAT
			WRITE(' ',(20 + 80 * exp(-0.07 * t)):2:3);
			INC(t,step);
		UNTIL (t>=time);
		WRITELN;
	END;

BEGIN
	WRITELN('Newton cooling function: Analytic solution (Sigma) with 3 Euler approximations.');
	WRITELN('Time: ',0:7,10:7,20:7,30:7,40:7,50:7,60:7,70:7,80:7,90:7);
	Sigma;
	FOR i := 1 to 3 DO
		Euler(NewtonCooling,T0,dt[i],time);
END.


```



Output:

```txt

Newton cooling function: Analytic solution (Sigma) with 3 Euler approximations.
Time:       0     10     20     30     40     50     60     70     80     90
Sigma: 100.000 59.727 39.728 29.797 24.865 22.416 21.200 20.596 20.296 20.147
dt= 2: 100.000 57.634 37.704 28.328 23.918 21.843 20.867 20.408 20.192 20.090
dt= 5: 100.000 53.800 34.280 26.034 22.549 21.077 20.455 20.192 20.081 20.034
dt=10: 100.000 44.000 27.200 22.160 20.648 20.194 20.058 20.017 20.005 20.002


```



## Perl


```Perl
sub euler_method {
        my ($t0, $t1, $k, $step_size) = @_;
        my @results = ( [0, $t0] );

        for (my $s = $step_size; $s <= 100; $s += $step_size) {
                $t0 -= ($t0 - $t1) * $k * $step_size;
                push @results, [$s, $t0];
        }

        return @results;
}

sub analytical {
        my ($t0, $t1, $k, $time) = @_;
        return ($t0 - $t1) * exp(-$time * $k) + $t1
}

my ($T0, $T1, $k) = (100, 20, .07);
my @r2  = grep { $_->[0] % 10 == 0 } euler_method($T0, $T1, $k, 2);
my @r5  = grep { $_->[0] % 10 == 0 } euler_method($T0, $T1, $k, 5);
my @r10 = grep { $_->[0] % 10 == 0 } euler_method($T0, $T1, $k, 10);

print "Time\t      2     err(%)      5     err(%)    10      err(%)  Analytic\n", "-" x 76, "\n";
for (0 .. $#r2) {
        my $an = analytical($T0, $T1, $k, $r2[$_][0]);
        printf "%4d\t".("%9.3f" x 7)."\n",
                $r2 [$_][0],
                $r2 [$_][1], ($r2 [$_][1] / $an) * 100 - 100,
                $r5 [$_][1], ($r5 [$_][1] / $an) * 100 - 100,
                $r10[$_][1], ($r10[$_][1] / $an) * 100 - 100,
                $an;
}

```

Output:
```txt
Time          2     err(%)      5     err(%)    10      err(%)  Analytic
----------------------------------------------------------------------------
   0      100.000    0.000  100.000    0.000  100.000    0.000  100.000
  10       57.634   -3.504   53.800   -9.923   44.000  -26.331   59.727
  20       37.704   -5.094   34.280  -13.711   27.200  -31.534   39.728
  30       28.328   -4.927   26.034  -12.629   22.160  -25.629   29.797
  40       23.918   -3.808   22.549   -9.313   20.648  -16.959   24.865
  50       21.843   -2.555   21.077   -5.972   20.194   -9.910   22.416
  60       20.867   -1.569   20.455   -3.512   20.058   -5.384   21.200
  70       20.408   -0.912   20.192   -1.959   20.017   -2.808   20.596
  80       20.192   -0.512   20.081   -1.057   20.005   -1.432   20.296
  90       20.090   -0.281   20.034   -0.559   20.002   -0.721   20.147
 100       20.042   -0.152   20.014   -0.291   20.000   -0.361   20.073
```



## Perl 6


```perl6
sub euler ( &f, $y0, $a, $b, $h ) {
    my $y = $y0;
    my @t_y;
    for $a, * + $h ... * > $b -> $t {
        @t_y[$t] = $y;
        $y += $h * f( $t, $y );
    }
    return @t_y;
}

constant COOLING_RATE = 0.07;
constant AMBIENT_TEMP =   20;
constant INITIAL_TEMP =  100;
constant INITIAL_TIME =    0;
constant FINAL_TIME   =  100;

sub f ( $time, $temp ) {
    return -COOLING_RATE * ( $temp - AMBIENT_TEMP );
}

my @e;
@e[$_] = euler( &f, INITIAL_TEMP, INITIAL_TIME, FINAL_TIME, $_ ) for 2, 5, 10;

say 'Time Analytic   Step2   Step5  Step10     Err2     Err5    Err10';

for INITIAL_TIME, * + 10 ... * >= FINAL_TIME -> $t {

    my $exact = AMBIENT_TEMP + (INITIAL_TEMP - AMBIENT_TEMP)
                              * (-COOLING_RATE * $t).exp;

    my $err = sub { @^a.map: { 100 * abs( $_ - $exact ) / $exact } }

    my ( $a, $b, $c ) = map { @e[$_][$t] }, 2, 5, 10;

    say $t.fmt('%4d '), ( $exact, $a, $b, $c )».fmt(' %7.3f'),
                           $err.([$a, $b, $c])».fmt(' %7.3f%%');
}
```


Output:
```txt
Time Analytic   Step2   Step5  Step10     Err2     Err5    Err10
   0  100.000 100.000 100.000 100.000   0.000%   0.000%   0.000%
  10   59.727  57.634  53.800  44.000   3.504%   9.923%  26.331%
  20   39.728  37.704  34.281  27.200   5.094%  13.711%  31.534%
  30   29.797  28.328  26.034  22.160   4.927%  12.629%  25.629%
  40   24.865  23.918  22.549  20.648   3.808%   9.313%  16.959%
  50   22.416  21.843  21.077  20.194   2.555%   5.972%   9.910%
  60   21.200  20.867  20.455  20.058   1.569%   3.512%   5.384%
  70   20.596  20.408  20.192  20.017   0.912%   1.959%   2.808%
  80   20.296  20.192  20.081  20.005   0.512%   1.057%   1.432%
  90   20.147  20.090  20.034  20.002   0.281%   0.559%   0.721%
 100   20.073  20.042  20.014  20.000   0.152%   0.291%   0.361%
```



## Phix

Translated from C

```Phix
constant FMT = " %7.3f"

procedure ivp_euler(integer f, atom y, integer step, integer end_t)
integer t = 0;

    printf(1, " Step %2d: ", step);
    while t<=end_t do
        if remainder(t,10)==0 then printf(1, FMT, y) end if
        y += step * call_func(f,{t, y});
        t += step
    end while
    printf(1, "\n");
end procedure

procedure analytic()
    printf(1, "    Time: ");
    for t = 0 to 100 by 10 do printf(1," %7g", t) end for
    printf(1, "\nAnalytic: ");
    for t = 0 to 100 by 10 do
        printf(1, FMT, 20 + 80 * exp(-0.07 * t))
    end for
    printf(1,"\n");
end procedure

function cooling(atom /*t*/, atom temp)
    return -0.07 * (temp - 20);
end function
constant r_cooling = routine_id("cooling")

analytic();
ivp_euler(r_cooling, 100, 2, 100);
ivp_euler(r_cooling, 100, 5, 100);
ivp_euler(r_cooling, 100, 10, 100);
```

Output:

```txt

    Time:        0      10      20      30      40      50      60      70      80      90     100
Analytic:  100.000  59.727  39.728  29.797  24.865  22.416  21.200  20.596  20.296  20.147  20.073
 Step  2:  100.000  57.634  37.704  28.328  23.918  21.843  20.867  20.408  20.192  20.090  20.042
 Step  5:  100.000  53.800  34.280  26.034  22.549  21.077  20.455  20.192  20.081  20.034  20.014
 Step 10:  100.000  44.000  27.200  22.160  20.648  20.194  20.058  20.017  20.005  20.002  20.000

```



## PicoLisp


```PicoLisp
(load "@lib/math.l")

(de euler (F Y A B H)
   (while (> B A)
      (prinl (round A) " " (round Y))
      (inc 'Y (*/ H (F A Y) 1.0))
      (inc 'A H) ) )

(de newtonCoolingLaw (A B)
   (*/ -0.07 (- B 20.) 1.0) )

(euler newtonCoolingLaw 100.0 0 100.0 2.0)
(euler newtonCoolingLaw 100.0 0 100.0 5.0)
(euler newtonCoolingLaw 100.0 0 100.0 10.0)
```

Output:

```txt
...
0.000 100.000
10.000 44.000
20.000 27.200
30.000 22.160
40.000 20.648
50.000 20.194
60.000 20.058
70.000 20.018
80.000 20.005
90.000 20.002
```



## PL/I


```pli
test: procedure options (main); /* 3 December 2012 */

   declare (x, y, z) float;
   declare (T0 initial (100), Tr initial (20)) float;
   declare k float initial (0.07);
   declare t fixed binary;
   declare h fixed binary;

   x, y, z = T0;
   /* Step size is 2 seconds */
   h = 2;
   put skip data (h);
   put skip list ('  t    By formula', 'By Euler');
   do t = 0 to 100 by 2;
      put skip edit (t, Tr + (T0 - Tr)/exp(k*t), x) (f(3), 2 f(17,10));
      x = x + h*f(t, x);
   end;

   /* Step size is 5 seconds */
   h = 5;
   put skip data (h);
   put skip list ('  t    By formula', 'By Euler');
   do t = 0 to 100 by 5;
      put skip edit ( t, Tr + (T0 - Tr)/exp(k*t), y) (f(3), 2 f(17,10));
      y = y + h*f(t, y);
   end;

   /* Step size is 10 seconds */
   h = 10;
   put skip data (h);
   put skip list ('  t    By formula', 'By Euler');
   do t = 0 to 100 by 10;
      put skip edit (t, Tr + (T0 - Tr)/exp(k*t), z) (f(3), 2 f(17,10));
      z = z + h*f(t, z);
   end;

f: procedure (dummy, T) returns (float);
   declare dummy  fixed binary;
   declare T float;

   return ( -k*(T - Tr) );
end f;

end test;
```


Only the final two outputs are shown, for brevity.

```txt

H=        5;
  t    By formula       By Euler
  0   100.0000000000   100.0000000000
  5    76.3750457764    72.0000000000
 10    59.7268257141    53.7999992371
 15    47.9950218201    41.9700012207
 20    39.7277565002    34.2805023193
 25    33.9019165039    29.2823257446
 30    29.7965145111    26.0335121155
 35    26.9034862518    23.9217834473
 40    24.8648052216    22.5491600037
 45    23.4281692505    21.6569538116
 50    22.4157905579    21.0770206451
 55    21.7023792267    20.7000637054
 60    21.1996459961    20.4550418854
 65    20.8453769684    20.2957763672
 70    20.5957260132    20.1922550201
 75    20.4198017120    20.1249656677
 80    20.2958297729    20.0812282562
 85    20.2084674835    20.0527992249
 90    20.1469039917    20.0343189240
 95    20.1035213470    20.0223064423
100    20.0729503632    20.0144996643
H=       10;
  t    By formula       By Euler
  0   100.0000000000   100.0000000000
 10    59.7268257141    44.0000000000
 20    39.7277565002    27.2000007629
 30    29.7965145111    22.1599998474
 40    24.8648052216    20.6480007172
 50    22.4157905579    20.1944007874
 60    21.1996459961    20.0583209991
 70    20.5957260132    20.0174961090
 80    20.2958297729    20.0052490234
 90    20.1469039917    20.0015754700
100    20.0729503632    20.0004730225

```




## PowerShell

Works with PowerShell 4.0

```PowerShell
function euler (${f}, ${y}, $y0, $t0, $tEnd) {
    function f-euler ($tn, $yn, $h)  {
        $yn + $h*(f $tn $yn)
    }
    function time ($t0, $h, $tEnd)  {
        $end = [MATH]::Floor(($tEnd - $t0)/$h)
        foreach ($_ in 0..$end) { $_*$h + $t0 }
    }
    $time = time $t0 10 $tEnd
    $time5 = time $t0 5 $tEnd
    $time2 = time $t0 2 $tEnd
    $yn10 = $yn5 = $yn2 = $y0
    $i2 = $i5 = 0
    foreach ($tn10 in $time) {
        while($time2[$i2] -ne $tn10) {
            $i2++
            $yn2 = (f-euler $time2[$i2] $yn2 2)
        }
        while($time5[$i5] -ne $tn10) {
            $i5++
            $yn5 = (f-euler $time5[$i5] $yn5 5)
        }
        [pscustomobject]@{
            t = "$tn10"
            Analytical = "$("{0:N5}" -f (y $tn10))"
            "Euler h = 2" = "$("{0:N5}" -f $yn2)"
            "Euler h = 5" = "$("{0:N5}" -f $yn5)"
            "Euler h = 10" = "$("{0:N5}" -f $yn10)"
            "Error h = 2" = "$("{0:N5}" -f [MATH]::abs($yn2 - (y $tn10)))"
            "Error h = 5" = "$("{0:N5}" -f [MATH]::abs($yn5 - (y $tn10)))"
            "Error h = 10" = "$("{0:N5}" -f [MATH]::abs($yn10 - (y $tn10)))"
        }
        $yn10 = (f-euler $tn10 $yn10 10)
    }
}
$k, $yr, $y0, $t0, $tEnd = 0.07, 20, 100, 0, 100
function f ($t, $y)  {
    -$k *($y - $yr)
}
function y ($t)  {
    $yr + ($y0 - $yr)*[MATH]::Exp(-$k*$t)
}
euler f y $y0 $t0 $tEnd | Format-Table -AutoSize

```

Output:

```txt

t   Analytical Euler h = 2 Euler h = 5 Euler h = 10 Error h = 2 Error h = 5 Error h = 10
-   ---------- ----------- ----------- ------------ ----------- ----------- ------------
0   100.00000  100.00000   100.00000   100.00000    0.00000     0.00000     0.00000
10  59.72682   57.63416    53.80000    44.00000     2.09266     5.92682     15.72682
20  39.72776   37.70413    34.28050    27.20000     2.02363     5.44726     12.52776
30  29.79651   28.32850    26.03351    22.16000     1.46801     3.76300     7.63651
40  24.86481   23.91795    22.54916    20.64800     0.94685     2.31565     4.21681
50  22.41579   21.84311    21.07702    20.19440     0.57268     1.33877     2.22139
60  21.19965   20.86705    20.45504    20.05832     0.33260     0.74461     1.14133
70  20.59573   20.40788    20.19225    20.01750     0.18784     0.40347     0.57823
80  20.29583   20.19188    20.08123    20.00525     0.10395     0.21460     0.29058
90  20.14690   20.09027    20.03432    20.00157     0.05664     0.11259     0.14533
100 20.07295   20.04246    20.01450    20.00047     0.03049     0.05845     0.07248

```



## PureBasic


```PureBasic
Define.d
Prototype.d Func(Time, t)

Procedure.d Euler(*F.Func, y0, a, b, h)
  Protected y=y0, t=a
  While t<=b
    PrintN(RSet(StrF(t,3),7)+" "+RSet(StrF(y,3),7))
    y + h * *F(t,y)
    t + h
  Wend
EndProcedure

Procedure.d newtonCoolingLaw(Time, t)
  ProcedureReturn -0.07*(t-20)
EndProcedure


If OpenConsole()
  Euler(@newtonCoolingLaw(), 100, 0, 100, 2)
  Euler(@newtonCoolingLaw(), 100, 0, 100, 5)
  Euler(@newtonCoolingLaw(), 100, 0, 100,10)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```


```txt
...
 85.000  20.053
 90.000  20.034
 95.000  20.022
100.000  20.014
  0.000 100.000
 10.000  44.000
 20.000  27.200
 30.000  22.160
 40.000  20.648
 50.000  20.194
 60.000  20.058
 70.000  20.017
 80.000  20.005
 90.000  20.002
100.000  20.000
```



## Python

Translated from Common Lisp

```python
def euler(f,y0,a,b,h):
	t,y = a,y0
	while t <= b:
		print "%6.3f %6.3f" % (t,y)
		t += h
		y += h * f(t,y)

def newtoncooling(time, temp):
	return -0.07 * (temp - 20)

euler(newtoncooling,100,0,100,10)

```

Output:

```txt

 0.000 100.000
10.000 44.000
20.000 27.200
30.000 22.160
40.000 20.648
50.000 20.194
60.000 20.058
70.000 20.017
80.000 20.005
90.000 20.002
100.000 20.000

```



## R

Translated from Python

```rsplus
euler <- function(f, y0, a, b, h)
{
  t <- a
  y <- y0

  while (t < b)
  {
    cat(sprintf("%6.3f %6.3f\n", t, y))
    t <- t + h
    y <- y + h*f(t, y)
  }
}

newtoncooling <- function(time, temp)
  return(-0.07*(temp-20))

euler(newtoncooling, 100, 0, 100, 10)
```

Output:

```txt
 0.000 100.000
10.000 44.000
20.000 27.200
30.000 22.160
40.000 20.648
50.000 20.194
60.000 20.058
70.000 20.017
80.000 20.005
90.000 20.002
```



## Racket


The ODE solver:

```racket

(define (ODE-solve f init
                   #:x-max x-max
                   #:step h
                   #:method (method euler))
  (reverse
   (iterate-while (λ (x . y) (<= x x-max)) (method f h) init)))

```


It uses the default integration method <tt>euler</tt>, defined separately.


```racket

(define (euler F h)
  (λ (x y) (list (+ x h) (+ y (* h (F x y))))))

```


A general-purpose procedure which evalutes a given function ''f'' repeatedly starting with argument ''x'', while all results satisfy a predicate ''test''. Returns a list of iterations.


```racket

(define (iterate-while test f x)
  (let next ([result x]
             [list-of-results '()])
    (if (apply test result)
        (next (apply f result) (cons result list-of-results))
        list-of-results)))

```


Textual output:

```racket

> (define (newton-cooling t T)
   (* -0.07 (- T 20)))
> (ODE-solve newton-cooling '(0 100) #:x-max 100 #:step 10)
'((0 100)
  (10 44.)
  (20 27.2)
  (30 22.16)
  (40 20.648)
  (50 20.1944)
  (60 20.05832)
  (70 20.017496)
  (80 20.0052488)
  (90 20.00157464)
  (100 20.000472392))

```


Plotting results:

```racket

> (require plot)
> (plot
   (map (λ (h c)
          (lines
           (ODE-solve newton-cooling '(0 100) #:x-max 100 #:step h)
           #:color c #:label (format "h=~a" h)))
        '(10 5 1)
        '(red blue black))
   #:legend-anchor 'top-right)

```

[[File:euler1.jpg]]

High modularity of the program allows to implement very different solution metods. For example
[http://en.wikipedia.org/wiki/Midpoint_method 2-nd order Runge-Kutta method]:


```racket

(define (RK2 F h)
  (λ (x y)
    (list (+ x h) (+ y (* h (F (+ x (* 1/2 h))
                               (+ y (* 1/2 h (F x y)))))))))

```


[http://en.wikipedia.org/wiki/Adams_method#Two-step_Adams.E2.80.93Bashforth Two-step Adams–Bashforth method]

```racket

(define (adams F h)
  (case-lambda
    ; first step using Runge-Kutta method
    [(x y) (append ((RK2 F h) x y) (list (F x y)))]
    [(x y f′)
     (let ([f (F x y)])
       (list (+ x h) (+ y (* 3/2 h f) (* -1/2 h f′)) f))]))

```


[http://en.wikipedia.org/wiki/Adaptive_stepsize Adaptive one-step method] modifier using absolute accuracy ''ε''

```racket

(define ((adaptive method ε) F h0)
  (case-lambda
    [(x y) (((adaptive method ε) F h0) x y h0)]
    [(x y h)
     (match-let* ([(list x0 y0) ((method F h) x y)]
                  [(list x1 y1) ((method F (/ h 2)) x y)]
                  [(list x1 y1) ((method F (/ h 2)) x1 y1)]
                  [τ  (abs (- y1 y0))]
                  [h′ (if (< τ ε) (min h h0) (* 0.9 h (/ ε τ)))])
       (list x1 (+ y1 τ) (* 2 h′)))]))


```


Comparison of different integration methods

```racket

> (define (solve-newton-cooling-by m)
    (ODE-solve newton-cooling '(0 100)
               #:x-max 100 #:step 10 #:method m))
> (plot
   (list
    (function (λ (t) (+ 20 (* 80 (exp (* -0.07 t))))) 0 100
              #:color 'black #:label "analytical")
    (lines (solve-newton-cooling-by euler)
           #:color 'red #:label "Euler")
    (lines (solve-newton-cooling-by RK2)
           #:color 'blue #:label "Runge-Kutta")
    (lines (solve-newton-cooling-by adams)
           #:color 'purple #:label "Adams")
    (points (solve-newton-cooling-by (adaptive euler 0.5))
            #:color 'red #:label "Adaptive Euler")
    (points (solve-newton-cooling-by (adaptive RK2 0.5))
            #:color 'blue #:label "Adaptive Runge-Kutta"))
   #:legend-anchor 'top-right)

```


[[File:euler2.jpg]]

See also [[Runge-Kutta method#Racket]]


## REXX


### version 1

Translated from PLI

```rexx
/* REXX ***************************************************************
* 24.05.2013 Walter Pachl  translated from PL/I
**********************************************************************/
  Numeric Digits 100
  T0=100
  Tr=20
  k=0.07

  h=2
  x=t0
  Call head
  do t=0 to 100 by 2
    Select
      When t<=4 | t>=96 Then
        call o x
      When t=8 Then
        Say '...'
      Otherwise
        Nop
      End
    x=x+h*f(x)
    end

  h=5
  y=t0
  Call head
  do t=0 to 100 by 5
    call o y
    y=y+h*f(y)
    end

  h=10
  z=t0
  Call head
  do t=0 to 100 by 10
    call o z
    z=z+h*f(z)
    end
  Exit

f: procedure Expose k Tr
  Parse Arg t
  return -k*(T-Tr)

head:
  Say 'h='h
  Say '  t    By formula       By Euler'
  Return

o:
  Parse Arg v
  Say right(t,3) format(Tr+(T0-Tr)/exp(k*t),5,10) format(v,5,10)
  Return

exp: Procedure
  Parse Arg x,prec
  If prec<9 Then prec=9
  Numeric Digits (2*prec)
  Numeric Fuzz   3
  o=1
  u=1
  r=1
  Do i=1 By 1
    ra=r
    o=o*x
    u=u*i
    r=r+(o/u)
    If r=ra Then Leave
    End
  Numeric Digits (prec)
  r=r+0
  Return r

```

Output:

```txt

h=2
  t    By formula       By Euler
  0   100.0000000000   100.0000000000
  2    89.5486587628    88.8000000000
  4    80.4626994233    79.1680000000
...
 96    20.0965230572    20.0574137147
 98    20.0839131147    20.0493757946
100    20.0729505571    20.0424631834
h=5
  t    By formula       By Euler
  0   100.0000000000   100.0000000000
  5    76.3750471216    72.0000000000
 10    59.7268242534    53.8000000000
 15    47.9950199099    41.9700000000
 20    39.7277571000    34.2805000000
 25    33.9019154664    29.2823250000
 30    29.7965142633    26.0335112500
 35    26.9034869314    23.9217823125
 40    24.8648050015    22.5491585031
 45    23.4281701466    21.6569530270
 50    22.4157906708    21.0770194676
 55    21.7023789162    20.7000626539
 60    21.1996461464    20.4550407250
 65    20.8453763508    20.2957764713
 70    20.5957266443    20.1922547063
 75    20.4198014729    20.1249655591
 80    20.2958290978    20.0812276134
 85    20.2084672415    20.0527979487
 90    20.1469043822    20.0343186667
 95    20.1035217684    20.0223071333
100    20.0729505571    20.0144996367
h=10
  t    By formula       By Euler
  0   100.0000000000   100.0000000000
 10    59.7268242534    44.0000000000
 20    39.7277571000    27.2000000000
 30    29.7965142633    22.1600000000
 40    24.8648050015    20.6480000000
 50    22.4157906708    20.1944000000
 60    21.1996461464    20.0583200000
 70    20.5957266443    20.0174960000
 80    20.2958290978    20.0052488000
 90    20.1469043822    20.0015746400
100    20.0729505571    20.0004723920
```



### version 2

This REXX version allows values to be specified via the command line (CL).


It also shows the percentage difference (analytic vs. Euler's method) for each calculation.

```rexx
/*REXX pgm solves example of Newton's cooling law via Euler's method (diff. step sizes).*/
e=2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138
numeric digits length(e)   -  length(.)          /*use the number of decimal digits in E*/
parse arg Ti Tr cc tt ss                         /*obtain optional arguments from the CL*/
if Ti='' | Ti=","  then Ti= 100                  /*given?  Default:  initial temp in ºC.*/
if Tr='' | Tr=","  then Tr=  20                  /*  "         "       room    "   "  " */
if cc='' | cc=","  then cc=   0.07               /*  "         "     cooling constant.  */
if tt='' | tt=","  then tt= 100                  /*  "         "    total time seconds. */
if ss='' | ss=","  then ss=   2  5  10           /*  "         "      the step sizes.   */
@= '═'                                           /*the character used in title separator*/
     do sSize=1  for words(ss);    say;    say;    say center('time in'     , 11)
     say center('seconds' , 11, @)                     center('Euler method', 16, @) ,
         center('analytic', 18, @)                     center('difference'  , 14, @)
     $=Ti;                  inc= word(ss, sSize) /*the 1st value;  obtain the increment.*/
          do t=0  to Ti  by inc                  /*step through calculations by the inc.*/
          a= format(Tr + (Ti-Tr)/exp(cc*t),6,10) /*calculate the analytic (exact) value.*/
          say center(t,11)  format($,6,3)  'ºC '  a  "ºC"  format(abs(a-$)/a*100,6,2)  '%'
          $= $   +   inc * cc * (Tr-$)           /*calc. next value via Euler's method. */
          end   /*t*/
     end        /*sSize*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
exp: procedure expose e; arg x; ix= x%1; if abs(x-ix)>.5  then ix=ix+sign(x); x= x-ix; z=1
     _=1;  w=1;    do j=1;  _= _*x/j;    z= (z+_)/1;      if z==w  then leave;         w=z
                   end  /*j*/;           if z\==0  then z= e**ix * z;             return z
```

Output:

```txt
  time in
══seconds══ ══Euler method══ ═════analytic═════ ══difference══
     0         100.000 ºC     100.0000000000 ºC      0.00 %
     2          88.800 ºC      89.5486588319 ºC      0.84 %
     4          79.168 ºC      80.4626993165 ºC      1.61 %
     6          70.884 ºC      72.5637455852 ºC      2.31 %
     8          63.761 ºC      65.6967251079 ºC      2.95 %
    10          57.634 ºC      59.7268243033 ºC      3.50 %
    12          52.365 ºC      54.5368418743 ºC      3.98 %
    14          47.834 ºC      50.0248879081 ºC      4.38 %
    16          43.937 ºC      46.1023835698 ºC      4.70 %
    18          40.586 ºC      42.6923221200 ºC      4.93 %
    20          37.704 ºC      39.7277571153 ºC      5.09 %
    22          35.226 ºC      37.1504881142 ºC      5.18 %
    24          33.094 ºC      34.9099180832 ºC      5.20 %
    26          31.261 ºC      32.9620600747 ºC      5.16 %
    28          29.684 ºC      31.2686736737 ºC      5.07 %
    30          28.328 ºC      29.7965142602 ºC      4.93 %
    32          27.163 ºC      28.5166803503 ºC      4.75 %
    34          26.160 ºC      27.4040462008 ºC      4.54 %
    36          25.297 ºC      26.4367685400 ºC      4.31 %
    38          24.556 ºC      25.5958577396 ºC      4.06 %
    40          23.918 ºC      24.8648050100 ºC      3.81 %
    42          23.369 ºC      24.2292582991 ºC      3.55 %
    44          22.898 ºC      23.6767405319 ºC      3.29 %
    46          22.492 ºC      23.1964046609 ºC      3.04 %
    48          22.143 ºC      22.7788207156 ºC      2.79 %
    50          21.843 ºC      22.4157906738 ºC      2.55 %
    52          21.585 ºC      22.1001875173 ºC      2.33 %
    54          21.363 ºC      21.8258153140 ºC      2.12 %
    56          21.172 ºC      21.5872875795 ºC      1.92 %
    58          21.008 ºC      21.3799215292 ºC      1.74 %
    60          20.867 ºC      21.1996461456 ºC      1.57 %
    62          20.746 ºC      21.0429222563 ºC      1.41 %
    64          20.641 ºC      20.9066730524 ºC      1.27 %
    66          20.551 ºC      20.7882236849 ºC      1.14 %
    68          20.474 ºC      20.6852487518 ºC      1.02 %
    70          20.408 ºC      20.5957266457 ºC      0.91 %
    72          20.351 ºC      20.5178998655 ºC      0.81 %
    74          20.302 ºC      20.4502405132 ºC      0.73 %
    76          20.259 ºC      20.3914202980 ºC      0.65 %
    78          20.223 ºC      20.3402844596 ºC      0.58 %
    80          20.192 ºC      20.2958290973 ºC      0.51 %
    82          20.165 ºC      20.2571814620 ºC      0.45 %
    84          20.142 ºC      20.2235828220 ºC      0.40 %
    86          20.122 ºC      20.1943735676 ºC      0.36 %
    88          20.105 ºC      20.1689802617 ºC      0.32 %
    90          20.090 ºC      20.1469043822 ºC      0.28 %
    92          20.078 ºC      20.1277125344 ºC      0.25 %
    94          20.067 ºC      20.1110279436 ºC      0.22 %
    96          20.057 ºC      20.0965230571 ºC      0.19 %
    98          20.049 ºC      20.0839131146 ºC      0.17 %
    100         20.042 ºC      20.0729505572 ºC      0.15 %


  time in
══seconds══ ══Euler method══ ═════analytic═════ ══difference══
     0         100.000 ºC     100.0000000000 ºC      0.00 %
     5          72.000 ºC      76.3750471775 ºC      5.73 %
    10          53.800 ºC      59.7268243033 ºC      9.92 %
    15          41.970 ºC      47.9950199289 ºC     12.55 %
    20          34.281 ºC      39.7277571153 ºC     13.71 %
    25          29.282 ºC      33.9019154760 ºC     13.63 %
    30          26.034 ºC      29.7965142602 ºC     12.63 %
    35          23.922 ºC      26.9034869199 ºC     11.08 %
    40          22.549 ºC      24.8648050100 ºC      9.31 %
    45          21.657 ºC      23.4281701494 ºC      7.56 %
    50          21.077 ºC      22.4157906738 ºC      5.97 %
    55          20.700 ºC      21.7023789151 ºC      4.62 %
    60          20.455 ºC      21.1996461456 ºC      3.51 %
    65          20.296 ºC      20.8453763507 ºC      2.64 %
    70          20.192 ºC      20.5957266457 ºC      1.96 %
    75          20.125 ºC      20.4198014719 ºC      1.44 %
    80          20.081 ºC      20.2958290973 ºC      1.06 %
    85          20.053 ºC      20.2084672415 ºC      0.77 %
    90          20.034 ºC      20.1469043822 ºC      0.56 %
    95          20.022 ºC      20.1035217684 ºC      0.40 %
    100         20.014 ºC      20.0729505572 ºC      0.29 %


  time in
══seconds══ ══Euler method══ ═════analytic═════ ══difference══
     0         100.000 ºC     100.0000000000 ºC      0.00 %
    10          44.000 ºC      59.7268243033 ºC     26.33 %
    20          27.200 ºC      39.7277571153 ºC     31.53 %
    30          22.160 ºC      29.7965142602 ºC     25.63 %
    40          20.648 ºC      24.8648050100 ºC     16.96 %
    50          20.194 ºC      22.4157906738 ºC      9.91 %
    60          20.058 ºC      21.1996461456 ºC      5.38 %
    70          20.017 ºC      20.5957266457 ºC      2.81 %
    80          20.005 ºC      20.2958290973 ºC      1.43 %
    90          20.002 ºC      20.1469043822 ºC      0.72 %
    100         20.000 ºC      20.0729505572 ºC      0.36 %

```


## Ring

```ring

decimals(3)
see euler("return -0.07*(y-20)", 100, 0, 100, 2) + nl
see euler("return -0.07*(y-20)", 100, 0, 100, 5) + nl
see euler("return -0.07*(y-20)", 100, 0, 100, 10) + nl

func euler df, y, a, b, s
     t = a
     while t <= b
           see "" + t + " " + y + nl
           y += s * eval(df)
           t += s
     end
     return y
```

Output:

```txt

0 100
2 88.800
4 79.168
6 70.884
8 63.761
10 57.634

```



## Ruby

Translated from Python

```ruby
def euler(y, a, b, h)
  a.step(b,h) do |t|
    puts "%7.3f %7.3f" % [t,y]
    y += h * yield(t,y)
  end
end

[10, 5, 2].each do |step|
  puts "Step = #{step}"
  euler(100,0,100,step) {|time, temp| -0.07 * (temp - 20) }
  puts
end
```

Output:

```txt
Step = 10
  0.000 100.000
 10.000  44.000
 20.000  27.200
 30.000  22.160
 40.000  20.648
 50.000  20.194
 60.000  20.058
 70.000  20.017
 80.000  20.005
 90.000  20.002
100.000  20.000

Step = 5
  0.000 100.000
  5.000  72.000
 10.000  53.800
 15.000  41.970
 20.000  34.280
 25.000  29.282
 30.000  26.034
 35.000  23.922
 40.000  22.549
 45.000  21.657
 50.000  21.077
 55.000  20.700
 60.000  20.455
 65.000  20.296
 70.000  20.192
 75.000  20.125
 80.000  20.081
 85.000  20.053
 90.000  20.034
 95.000  20.022
100.000  20.014

Step = 2
  0.000 100.000
  2.000  88.800
  4.000  79.168
  6.000  70.884
  8.000  63.761
 10.000  57.634
 12.000  52.365
 14.000  47.834
 16.000  43.937
 18.000  40.586
 20.000  37.704
 22.000  35.226
 24.000  33.094
 26.000  31.261
 28.000  29.684
 30.000  28.328
 32.000  27.163
 34.000  26.160
 36.000  25.297
 38.000  24.556
 40.000  23.918
 42.000  23.369
 44.000  22.898
 46.000  22.492
 48.000  22.143
 50.000  21.843
 52.000  21.585
 54.000  21.363
 56.000  21.172
 58.000  21.008
 60.000  20.867
 62.000  20.746
 64.000  20.641
 66.000  20.551
 68.000  20.474
 70.000  20.408
 72.000  20.351
 74.000  20.302
 76.000  20.259
 78.000  20.223
 80.000  20.192
 82.000  20.165
 84.000  20.142
 86.000  20.122
 88.000  20.105
 90.000  20.090
 92.000  20.078
 94.000  20.067
 96.000  20.057
 98.000  20.049
100.000  20.042

```



## Rust

Translated from Kotlin

```Rust
fn header() {
    print!("    Time: ");
    for t in (0..100).step_by(10) {
        print!(" {:7}", t);
    }
    println!();
}

fn analytic() {
    print!("Analytic: ");
    for t in (0..=100).step_by(10) {
        print!(" {:7.3}", 20.0 + 80.0 * (-0.07 * f64::from(t)).exp());
    }
    println!();
}

fn euler<F: Fn(f64) -> f64>(f: F, mut y: f64, step: usize, end: usize) {
    print!(" Step {:2}: ", step);
    for t in (0..=end).step_by(step) {
        if t % 10 == 0 {
            print!(" {:7.3}", y);
        }
        y += step as f64 * f(y);
    }
    println!();
}

fn main() {
    header();
    analytic();
    for &i in &[2, 5, 10] {
        euler(|temp| -0.07 * (temp - 20.0), 100.0, i, 100);
    }
}
```

Output:

```txt
    Time:        0      10      20      30      40      50      60      70      80      90
Analytic:  100.000  59.727  39.728  29.797  24.865  22.416  21.200  20.596  20.296  20.147  20.073
 Step  2:  100.000  57.634  37.704  28.328  23.918  21.843  20.867  20.408  20.192  20.090  20.042
 Step  5:  100.000  53.800  34.280  26.034  22.549  21.077  20.455  20.192  20.081  20.034  20.014
 Step 10:  100.000  44.000  27.200  22.160  20.648  20.194  20.058  20.017  20.005  20.002  20.000
```


## Scala

```scala

object App{

  def main(args : Array[String]) = {

    def cooling( step : Int ) = {
      eulerStep( (step , y) => {-0.07 * (y - 20)} ,
        100.0,0,100,step)
    }
    cooling(10)
    cooling(5)
    cooling(2)
  }
  def eulerStep( func : (Int,Double) => Double,y0 : Double,
    begin : Int, end : Int , step : Int) = {

    println("Step size: %s".format(step))

    var current : Int = begin
    var y : Double = y0
    while( current <= end){
      println( "%d %.5f".format(current,y))
      current += step
      y += step * func(current,y)
    }

    println("DONE")
  }

}

```


Output for step = 10;

```txt

Step size: 10
0 100.00000
10 44.00000
20 27.20000
30 22.16000
40 20.64800
50 20.19440
60 20.05832
70 20.01750
80 20.00525
90 20.00157
DONE

```


## SequenceL

```sequencel
import <Utilities/Conversion.sl;
import <Utilities/Sequence.sl>;

T0 := 100.0;
TR := 20.0;
k := 0.07;

main(args(2)) :=
	let
		results[i] := euler(newtonCooling, T0, 100, stringToInt(args[i]), 0, "delta_t = " ++ args[i]);
	in
		delimit(results, '\n');

newtonCooling(t) := -k * (t - TR);

euler: (float -> float) * float * int * int * int * char(1) -> char(1);
euler(f, y, n, h, x, output(1)) :=
	let
		newOutput := output ++ "\n\t" ++ intToString(x) ++ "\t" ++ floatToString(y, 3);
		newY := y + h * f(y);
		newX := x + h;
	in
			output when x > n
		else
			euler(f, newY, n, h, newX, newOutput);
```

Based on C# version [http://rosettacode.org/wiki/Euler_method#C.23] but using tail recursion instead of looping.

Output:

For step size 10:

```txt

main.exe 10
"delta_t = 10
        0       100.000
        10      44.000
        20      27.200
        30      22.160
        40      20.648
        50      20.194
        60      20.058
        70      20.017
        80      20.005
        90      20.002
        100     20.000"

```



## Sidef

Translated from Perl

```ruby
func euler_method(t0, t1, k, step_size) {
    var results = [[0, t0]]
    for s in (step_size..100 -> by(step_size)) {
        t0 -= ((t0 - t1) * k * step_size)
        results << [s, t0]
    }
    return results;
}

func analytical(t0, t1, k, time) {
    (t0 - t1) * exp(-time * k) + t1
}

var (T0, T1, k) = (100, 20, .07)
var r2  = euler_method(T0, T1, k,  2).grep { _[0] %% 10 }
var r5  = euler_method(T0, T1, k,  5).grep { _[0] %% 10 }
var r10 = euler_method(T0, T1, k, 10).grep { _[0] %% 10 }

say "Time\t      2     err(%)      5     err(%)    10      err(%)  Analytic"
say "-"*76

r2.range.each { |i|
    var an = analytical(T0, T1, k, r2[i][0])
    printf("%4d\t#{'%9.3f' * 7}\n",
                 r2[i][0],
                 r2[i][1], ( r2[i][1] / an) * 100 - 100,
                 r5[i][1], ( r5[i][1] / an) * 100 - 100,
                r10[i][1], (r10[i][1] / an) * 100 - 100,
                an)
}
```

Output:

```txt

Time	      2     err(%)      5     err(%)    10      err(%)  Analytic
----------------------------------------------------------------------------
   0	  100.000    0.000  100.000    0.000  100.000    0.000  100.000
  10	   57.634   -3.504   53.800   -9.923   44.000  -26.331   59.727
  20	   37.704   -5.094   34.281  -13.711   27.200  -31.534   39.728
  30	   28.328   -4.927   26.034  -12.629   22.160  -25.629   29.797
  40	   23.918   -3.808   22.549   -9.313   20.648  -16.959   24.865
  50	   21.843   -2.555   21.077   -5.972   20.194   -9.910   22.416
  60	   20.867   -1.569   20.455   -3.512   20.058   -5.384   21.200
  70	   20.408   -0.912   20.192   -1.959   20.017   -2.808   20.596
  80	   20.192   -0.512   20.081   -1.057   20.005   -1.432   20.296
  90	   20.090   -0.281   20.034   -0.559   20.002   -0.721   20.147
 100	   20.042   -0.152   20.014   -0.291   20.000   -0.361   20.073

```



## Smalltalk


```smalltalk>ODESolver>
eulerOf: f init: y0 from: a to: b step: h
	| t y |
	t := a.
	y := y0.
	[ t < b ]
		whileTrue: [
			Transcript
				show: t asString, ' ' , (y printShowingDecimalPlaces: 3);
				cr.
			t := t + h.
			y := y + (h * (f value: t value: y)) ]

ODESolver new eulerOf: [:time :temp| -0.07 * (temp - 20)]  init: 100 from: 0 to: 100 step: 10

```

Transcript:

```txt

0 100.000
10 44.000
20 27.200
30 22.160
40 20.648
50 20.194
60 20.058
70 20.017
80 20.005
90 20.002

```



## Tcl

Translated from C++

```tcl
proc euler {f y0 a b h} {
    puts "computing $f over \[$a..$b\], step $h"
    set y [expr {double($y0)}]
    for {set t [expr {double($a)}]} {$t < $b} {set t [expr {$t + $h}]} {
	puts [format "%.3f\t%.3f" $t $y]
	set y [expr {$y + $h * double([$f $t $y])}]
    }
    puts "done"
}
```

Demonstration with the Newton Cooling Law:

```tcl
proc newtonCoolingLaw {time temp} {
    expr {-0.07 * ($temp - 20)}
}

euler newtonCoolingLaw 100 0 100 2
euler newtonCoolingLaw 100 0 100 5
euler newtonCoolingLaw 100 0 100 10
```

End of output:

```txt

...
computing newtonCoolingLaw over [0..100], step 10
0.000	100.000
10.000	44.000
20.000	27.200
30.000	22.160
40.000	20.648
50.000	20.194
60.000	20.058
70.000	20.017
80.000	20.005
90.000	20.002
done

```



## VBA

Translated from Phix
```vb
Private Sub ivp_euler(f As String, y As Double, step As Integer, end_t As Integer)
    Dim t As Integer
    Debug.Print " Step "; step; ": ",
    Do While t <= end_t
        If t Mod 10 = 0 Then Debug.Print Format(y, "0.000"),
        y = y + step * Application.Run(f, y)
        t = t + step
    Loop
    Debug.Print
End Sub

Sub analytic()
    Debug.Print "    Time: ",
    For t = 0 To 100 Step 10
        Debug.Print " "; t,
    Next t
    Debug.Print
    Debug.Print "Analytic: ",
    For t = 0 To 100 Step 10
        Debug.Print Format(20 + 80 * Exp(-0.07 * t), "0.000"),
    Next t
    Debug.Print
End Sub

Private Function cooling(temp As Double) As Double
    cooling = -0.07 * (temp - 20)
End Function

Public Sub euler_method()
    Dim r_cooling As String
    r_cooling = "cooling"
    analytic
    ivp_euler r_cooling, 100, 2, 100
    ivp_euler r_cooling, 100, 5, 100
    ivp_euler r_cooling, 100, 10, 100
End Sub
```
Output:

```txt
    Time:       0             10            20            30            40            50            60            70            80            90            100
Analytic:     100,000       59,727        39,728        29,797        24,865        22,416        21,200        20,596        20,296        20,147        20,073
 Step  2 :    100,000       57,634        37,704        28,328        23,918        21,843        20,867        20,408        20,192        20,090        20,042
 Step  5 :    100,000       53,800        34,281        26,034        22,549        21,077        20,455        20,192        20,081        20,034        20,014
 Step  10 :   100,000       44,000        27,200        22,160        20,648        20,194        20,058        20,017        20,005        20,002        20,000
```


## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

proc Euler(Step);       \Display cooling temperatures using Euler's method
int  Step;
int  Time;  real Temp;
[Text(0, "Step ");  IntOut(0, Step);  Text(0, " ");
Time:= 0;  Temp:= 100.0;
repeat  if rem(Time/10) = 0 then RlOut(0, Temp);
        Temp:= Temp + float(Step) * (-0.07*(Temp-20.0));
        Time:= Time + Step;
until   Time > 100;
CrLf(0);
];

real Time, Temp;
[Format(6,0);                   \display time heading
Text(0, "Time   ");
Time:= 0.0;
while Time <= 100.1 do          \(.1 avoids possible rounding error)
        [RlOut(0, Time);
        Time:= Time + 10.0;
        ];
CrLf(0);

Format(3,2);                    \display cooling temps using differential eqn.
Text(0, "Dif eq ");             \ dTemp(time)/dtime = -k*Temp
Time:= 0.0;
while Time <= 100.1 do
        [Temp:= 20.0 + (100.0-20.0) * Exp(-0.07*Time);
        RlOut(0, Temp);
        Time:= Time + 10.0;
        ];
CrLf(0);

Euler(2);                       \display cooling temps for various time steps
Euler(5);
Euler(10);
]
```


Output:

```txt

Time         0    10    20    30    40    50    60    70    80    90   100
Dif eq  100.00 59.73 39.73 29.80 24.86 22.42 21.20 20.60 20.30 20.15 20.07
Step 2  100.00 57.63 37.70 28.33 23.92 21.84 20.87 20.41 20.19 20.09 20.04
Step 5  100.00 53.80 34.28 26.03 22.55 21.08 20.46 20.19 20.08 20.03 20.01
Step 10 100.00 44.00 27.20 22.16 20.65 20.19 20.06 20.02 20.01 20.00 20.00

```



## zkl

Translated from C

```zkl
const FMT=" %7.3f";

fcn ivp_euler(f,y,step,end_t){
   print(" Step %2d: ".fmt(step));
   foreach t in ([0..end_t,step]){
      if (t % 10 == 0) print(FMT.fmt(y));
      y += f(t,y) * step;
   }
   println();
}

fcn analytic{
   print("    Time: ");
   foreach t in  ([0..100,10]){ print(" %7g".fmt(t)) }
   print("\nAnalytic: ");
   foreach t in ([0..100,10]){ print(FMT.fmt(20.0 + 80.0 * (-0.07 * t).exp())) }
   println();
}

fcn cooling(_,temp){ return(-0.07 * (temp - 20)) }

analytic();
ivp_euler(cooling, 100.0, 2,  100);
ivp_euler(cooling, 100.0, 5,  100);
ivp_euler(cooling, 100.0, 10, 100);
```

Output:

```txt

    Time:        0      10      20      30      40      50      60      70      80      90     100
Analytic:  100.000  59.727  39.728  29.797  24.865  22.416  21.200  20.596  20.296  20.147  20.073
 Step  2:  100.000  57.634  37.704  28.328  23.918  21.843  20.867  20.408  20.192  20.090  20.042
 Step  5:  100.000  53.800  34.280  26.034  22.549  21.077  20.455  20.192  20.081  20.034  20.014
 Step 10:  100.000  44.000  27.200  22.160  20.648  20.194  20.058  20.017  20.005  20.002  20.000

```



## ZX Spectrum Basic

Translated from BBC_BASIC

```zxbasic
10 LET d$="-0.07*(y-20)": LET y=100: LET a=0: LET b=100: LET s=10
20 LET t=a
30 IF t<=b THEN PRINT t;TAB 10;y: LET y=y+s*VAL d$: LET t=t+s: GO TO 30
```

