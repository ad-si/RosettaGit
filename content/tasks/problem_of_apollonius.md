+++
title = "Problem of Apollonius"
description = ""
date = 2018-09-28T02:22:27Z
aliases = []
[extra]
id = 7941
[taxonomies]
categories = ["task", "arithmetic operations"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "bbc_basic",
  "c",
  "coffeescript",
  "csharp",
  "d",
  "elixir",
  "fortran",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "mathematica",
  "mumps",
  "nim",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "swift",
  "tcl",
  "vba",
  "zkl",
]
+++

[[File:Apollonius.png|400px|Two solutions to the problem of Apollonius|right]]

## Task

Implement a solution to the Problem of Apollonius   ([[wp:Problem_of_Apollonius|description on Wikipedia]])   which is the problem of finding the circle that is tangent to three specified circles.   There is an [[wp:Problem_of_Apollonius#Algebraic_solutions|algebraic solution]] which is pretty straightforward.

The solutions to the example in the code are shown in the image (below and right).   The red circle is "internally tangent" to all three black circles, and the green circle is "externally tangent" to all three black circles.





## Ada

apollonius.ads:

```Ada
package Apollonius is
   type Point is record
      X, Y : Long_Float := 0.0;
   end record;

   type Circle is record
      Center : Point;
      Radius : Long_Float := 0.0;
   end record;

   type Tangentiality is (External, Internal);

   function Solve_CCC
     (Circle_1, Circle_2, Circle_3 : Circle;
      T1, T2, T3                   : Tangentiality := External)
      return                         Circle;
end Apollonius;
```

apollonius.adb:

```Ada
with Ada.Numerics.Generic_Elementary_Functions;

package body Apollonius is
   package Math is new Ada.Numerics.Generic_Elementary_Functions
     (Long_Float);

   function Solve_CCC
     (Circle_1, Circle_2, Circle_3 : Circle;
      T1, T2, T3                   : Tangentiality := External)
      return                         Circle
   is
      S1 : Long_Float := 1.0;
      S2 : Long_Float := 1.0;
      S3 : Long_Float := 1.0;

      X1 : Long_Float renames Circle_1.Center.X;
      Y1 : Long_Float renames Circle_1.Center.Y;
      R1 : Long_Float renames Circle_1.Radius;

      X2 : Long_Float renames Circle_2.Center.X;
      Y2 : Long_Float renames Circle_2.Center.Y;
      R2 : Long_Float renames Circle_2.Radius;

      X3 : Long_Float renames Circle_3.Center.X;
      Y3 : Long_Float renames Circle_3.Center.Y;
      R3 : Long_Float renames Circle_3.Radius;
   begin
      if T1 = Internal then
         S1 := -S1;
      end if;
      if T2 = Internal then
         S2 := -S2;
      end if;
      if T3 = Internal then
         S3 := -S3;
      end if;

      declare
         V11 : constant Long_Float := 2.0 * X2 - 2.0 * X1;
         V12 : constant Long_Float := 2.0 * Y2 - 2.0 * Y1;
         V13 : constant Long_Float :=
           X1 * X1 - X2 * X2 + Y1 * Y1 - Y2 * Y2 - R1 * R1 + R2 * R2;
         V14 : constant Long_Float := 2.0 * S2 * R2 - 2.0 * S1 * R1;

         V21 : constant Long_Float := 2.0 * X3 - 2.0 * X2;
         V22 : constant Long_Float := 2.0 * Y3 - 2.0 * Y2;
         V23 : constant Long_Float :=
           X2 * X2 - X3 * X3 + Y2 * Y2 - Y3 * Y3 - R2 * R2 + R3 * R3;
         V24 : constant Long_Float := 2.0 * S3 * R3 - 2.0 * S2 * R2;

         W12 : constant Long_Float := V12 / V11;
         W13 : constant Long_Float := V13 / V11;
         W14 : constant Long_Float := V14 / V11;

         W22 : constant Long_Float := V22 / V21 - W12;
         W23 : constant Long_Float := V23 / V21 - W13;
         W24 : constant Long_Float := V24 / V21 - W14;

         P   : constant Long_Float := -W23 / W22;
         Q   : constant Long_Float := W24 / W22;
         M   : constant Long_Float := -W12 * P - W13;
         N   : constant Long_Float := W14 - W12 * Q;

         A   : constant Long_Float := N * N + Q * Q - 1.0;
         B   : constant Long_Float :=
           2.0 * M * N -
             2.0 * N * X1 +
               2.0 * P * Q -
                 2.0 * Q * Y1 +
                   2.0 * S1 * R1;
         C   : constant Long_Float :=
           X1 * X1 +
             M * M -
               2.0 * M * X1 +
                 P * P +
                   Y1 * Y1 -
                     2.0 * P * Y1 -
                       R1 * R1;

         D   : constant Long_Float := B * B - 4.0 * A * C;
         RS  : constant Long_Float := (-B - Math.Sqrt (D)) / (2.0 * A);
      begin
         return (Center => (X => M + N * RS, Y => P + Q * RS), Radius => RS);
      end;
   end Solve_CCC;
end Apollonius;
```


example test_apollonius.adb:

```Ada
with Ada.Text_IO;
with Apollonius;

procedure Test_Apollonius is
   use Apollonius;
   package Long_Float_IO is new Ada.Text_IO.Float_IO (Long_Float);

   C1 : constant Circle := (Center => (X => 0.0, Y => 0.0), Radius => 1.0);
   C2 : constant Circle := (Center => (X => 4.0, Y => 0.0), Radius => 1.0);
   C3 : constant Circle := (Center => (X => 2.0, Y => 4.0), Radius => 2.0);

   R1 : Circle := Solve_CCC (C1, C2, C3, External, External, External);
   R2 : Circle := Solve_CCC (C1, C2, C3, Internal, Internal, Internal);
begin
   Ada.Text_IO.Put_Line ("R1:");
   Long_Float_IO.Put (R1.Center.X, Aft => 3, Exp => 0);
   Long_Float_IO.Put (R1.Center.Y, Aft => 3, Exp => 0);
   Long_Float_IO.Put (R1.Radius, Aft => 3, Exp => 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("R2:");
   Long_Float_IO.Put (R2.Center.X, Aft => 3, Exp => 0);
   Long_Float_IO.Put (R2.Center.Y, Aft => 3, Exp => 0);
   Long_Float_IO.Put (R2.Radius, Aft => 3, Exp => 0);
   Ada.Text_IO.New_Line;
end Test_Apollonius;
```


output:

```txt
R1:
 2.000 2.100 3.900
R2:
 2.000 0.833 1.167
```



## AutoHotkey

```AutoHotkey
#NoEnv
#SingleInstance, Force
SetBatchLines, -1

; Uncomment if Gdip.ahk is not in your standard library
;#Include, Gdip.ahk

; Start gdi+
If !pToken := Gdip_Startup()
{
	MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
	ExitApp
}
OnExit, Exit
; I've added a simple new function here, just to ensure if anyone is having any problems then to make sure they are using the correct library version
If (Gdip_LibraryVersion() < 1.30)
{
	MsgBox, 48, version error!, Please download the latest version of the gdi+ library
	ExitApp
}
x1:=300,y1:=500,r1:=50,x2:=200,y2:=200,r2:=150,x3:=600,y3:=400,r3:=100,s1:=-1,s2:=-1,s3:=-1,xs:=0,ys:=0,rs:=0
, Apollonius(x1,y1,r1,x2,y2,r2,x3,y3,r3,s1,s2,s3,xs,ys,rs)
, Width:=max(x1+r1 "," x2+r2 "," x3+r3 "," xs+rs)*1.1
, Height:=max(y1+r1 "," y2+r2 "," y3+r3 "," ys+rs)*1.1

Gui, -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, Show
hwnd1 := WinExist()
, hbm := CreateDIBSection(Width, Height)
, hdc := CreateCompatibleDC()
, obm := SelectObject(hdc, hbm)
, G := Gdip_GraphicsFromHDC(hdc)
, Gdip_SetSmoothingMode(G, 4)
, bWhite := Gdip_BrushCreateSolid(0xffffffff)
, Gdip_FillRectangle(G, bWhite, 0, 0, Width, Height)
, pRed := Gdip_CreatePen(0x88ff0000, 3)
, pGreen := Gdip_CreatePen(0x8800ff00, 3)
, pBlue := Gdip_CreatePen(0x880000ff, 3)
, pBlack := Gdip_CreatePen(0x88000000, 3)
, Gdip_DrawCircle(G, pRed, x1, y1, r1)
, Gdip_DrawCircle(G, pGreen, x2, y2, r2)
, Gdip_DrawCircle(G, pBlue, x3, y3, r3)
, Gdip_DrawCircle(G, pBlack, xs, ys, rs)
, Gdip_DeletePen(pRed)
, Gdip_DeletePen(pGreen)
, Gdip_DeletePen(pBlue)
, Gdip_DeletePen(pBlack)
, UpdateLayeredWindow(hwnd1, hdc, 0, 0, Width, Height)
Return

GuiEscape:
GuiClose:
Exit:
	SelectObject(hdc, obm)
	, DeleteObject(hbm)
	, DeleteDC(hdc)
	, Gdip_DeleteGraphics(G)
	, Gdip_Shutdown(pToken)
	ExitApp

Apollonius(x1=300,y1=500,r1=50,x2=200,y2=200,r2=150,x3=600,y3=400,r3=100,s1=1,s2=1,s3=1,ByRef xs=0, ByRef ys=0, ByRef rs=0) {
v11 := 2*x2 - 2*x1
v12 := 2*y2 - 2*y1
v13 := x1**2 - x2**2 + y1**2 - y2**2 - r1**2 + r2**2
v14 := 2*s2*r2 - 2*s1*r1

v21 := 2*x3 - 2*x2
v22 := 2*y3 - 2*y2
v23 := x2**2 - x3**2 + y2**2 - y3**2 - r2**2 + r3**2
v24 := 2*s3*r3 - 2*s2*r2

w12 := v12/v11
w13 := v13/v11
w14 := v14/v11

w22 := v22/v21 - w12
w23 := v23/v21 - w13
w24 := v24/v21 - w14

p := -w23/w22
q := w24/w22
m := -w12*p - w13
n := w14 - w12*q

a := n**2 + q**2 - 1
b := 2*m*n - 2*n*x1 + 2*p*q - 2*q*y1 + 2*s1*r1
c := x1**2 + m**2 - 2*m*x1 + p**2 + y1**2 - 2*p*y1 - r1**2

d := b**2 - 4*a*c
rs := (-b - d**0.5)/(2*a)
xs := m + n*rs
ys := p + q*rs
}

; from http://rosettacode.org/wiki/Greatest_element_of_a_list#AutoHotkey
max(list) {
	Loop Parse, list, `,
	x := x < A_LoopField ? A_LoopField : x
	Return x
}

; Gdip helper function
Gdip_DrawCircle(G, pPen, x, y, r) {
	Return Gdip_DrawEllipse(G, pPen, x-r, y-r, r*2, r*2)
}
```



## BBC BASIC

Note use made of array arithmetic.

```bbcbasic
      DIM Circle{x, y, r}
      DIM Circles{(2)} = Circle{}
      Circles{(0)}.x = 0 : Circles{(0)}.y = 0 : Circles{(0)}.r = 1
      Circles{(1)}.x = 4 : Circles{(1)}.y = 0 : Circles{(1)}.r = 1
      Circles{(2)}.x = 2 : Circles{(2)}.y = 4 : Circles{(2)}.r = 2

      @% = &2030A
      REM Solution for internal circle:
      PROCapollonius(Circle{}, Circles{()}, -1, -1, -1)
      PRINT "Internal: x = ";Circle.x ", y = ";Circle.y ", r = ";Circle.r
      REM Solution for external circle:
      PROCapollonius(Circle{}, Circles{()}, 1, 1, 1)
      PRINT "External: x = ";Circle.x ", y = ";Circle.y ", r = ";Circle.r
      END

      DEF PROCapollonius(c{}, c{()}, s0, s1, s2)
      LOCAL x0, x1, x2, y0, y1, y2, r0, r1, r2, a, b, c
      LOCAL u(), v(), w() : DIM u(2), v(2), w(2)
      x0 = c{(0)}.x : y0 = c{(0)}.y : r0 = c{(0)}.r
      x1 = c{(1)}.x : y1 = c{(1)}.y : r1 = c{(1)}.r
      x2 = c{(2)}.x : y2 = c{(2)}.y : r2 = c{(2)}.r

      u() = 2*y1-2*y0, x0*x0-x1*x1+y0*y0-y1*y1-r0*r0+r1*r1, 2*s1*r1-2*s0*r0
      v() = 2*y2-2*y1, x1*x1-x2*x2+y1*y1-y2*y2-r1*r1+r2*r2, 2*s2*r2-2*s1*r1
      w() = u() / (2*x1 - 2*x0)
      u() = v() / (2*x2 - 2*x1) - w()
      u() /= u(0)
      w(1) -= w(0)*u(1)
      w(2) -= w(0)*u(2)
      a = w(2)*w(2) + u(2)*u(2) - 1
      b = -2*w(1)*w(2) - 2*w(2)*x1 - 2*u(1)*u(2) - 2*u(2)*y1 + 2*s1*r1
      c = x1*x1 + w(1)*w(1) + 2*w(1)*x1 + u(1)*u(1) + y1*y1 + 2*u(1)*y1 - r1*r1

      c.r = (-b - SQR(b^2 - 4*a*c)) / (2*a)
      c.x = c.r * w(2) - w(1)
      c.y = c.r * u(2) - u(1)
      ENDPROC
```

'''Output:'''

```txt

Internal: x = 2.000, y = 0.833, r = 1.167
External: x = 2.000, y = 2.100, r = 3.900

```



## C

C99.  2D vectors are actually complex numbers.  The method here is unothordox if not insane.  I can't prove that it should work, though it does seem to give correct answers for test cases I tried.

```c
#include <stdio.h>
#include <tgmath.h>

#define VERBOSE 0
#define for3 for(int i = 0; i < 3; i++)

typedef complex double vec;
typedef struct { vec c; double r; } circ;

#define re(x) creal(x)
#define im(x) cimag(x)
#define cp(x) re(x), im(x)
#define CPLX "(%6.3f,%6.3f)"
#define CPLX3 CPLX" "CPLX" "CPLX

double cross(vec a, vec b) { return re(a) * im(b) - im(a) * re(b); }
double abs2(vec a) { return a * conj(a); }

int apollonius_in(circ aa[], int ss[], int flip, int divert)
{
	vec n[3], x[3], t[3], a, b, center;
	int s[3], iter = 0, res = 0;
	double diff = 1, diff_old = -1, axb, d, r;

	for3 {
		s[i] = ss[i] ? 1 : -1;
		x[i] = aa[i].c;
	}

	while (diff > 1e-20) {
		a = x[0] - x[2], b = x[1] - x[2];
		diff = 0;
		axb = -cross(a, b);
		d = sqrt(abs2(a) * abs2(b) * abs2(a - b));

		if (VERBOSE) {
			const char *z = 1 + "-0+";
			printf("%c%c%c|%c%c|",
				z[s[0]], z[s[1]], z[s[2]], z[flip], z[divert]);
			printf(CPLX3, cp(x[0]), cp(x[1]), cp(x[2]));
		}

		/* r and center represent an arc through points x[i].  Each step,
		   we'll deform this arc by pushing or pulling some point on it
		   towards the edge of each given circle. */
		r = fabs(d / (2 * axb));
		center = (abs2(a)*b - abs2(b)*a) / (2 * axb) * I + x[2];

 		/* maybe the "arc" is actually straight line; then we have two
		   choices in defining "push" and "pull", so try both */
		if (!axb && flip != -1 && !divert) {
			if (!d) { /* generally means circle centers overlap */
				printf("Given conditions confused me.\n");
				return 0;
			}

			if (VERBOSE) puts("\n[divert]");
			divert = 1;
			res = apollonius_in(aa, ss, -1, 1);
		}

 		/* if straight line, push dir is its norm; else it's away from center */
		for3 n[i] = axb ? aa[i].c - center : a * I * flip;
		for3 t[i] = aa[i].c + n[i] / cabs(n[i]) * aa[i].r * s[i];

		/* diff: how much tangent points have moved since last iteration */
		for3 diff += abs2(t[i] - x[i]), x[i] = t[i];

		if (VERBOSE) printf(" %g\n", diff);

 		/* keep an eye on the total diff: failing to converge means no solution */
		if (diff >= diff_old && diff_old >= 0)
			if (iter++ > 20) return res;

		diff_old = diff;
	}

	printf("found: ");
	if (axb) printf("circle "CPLX", r = %f\n", cp(center), r);
	else	 printf("line "CPLX3"\n", cp(x[0]), cp(x[1]), cp(x[2]));

	return res + 1;
}

int apollonius(circ aa[])
{
	int s[3], i, sum = 0;
	for (i = 0; i < 8; i++) {
		s[0] = i & 1, s[1] = i & 2, s[2] = i & 4;

		/* internal or external results of a zero-radius circle are the same */
		if (s[0] && !aa[0].r) continue;
		if (s[1] && !aa[1].r) continue;
		if (s[2] && !aa[2].r) continue;
		sum += apollonius_in(aa, s, 1, 0);
	}
	return sum;
}

int main()
{
	circ a[3] = {{0, 1}, {4, 1}, {2 + 4 * I, 1}};
	circ b[3] = {{-3, 2}, {0, 1}, {3, 2}};
	circ c[3] = {{-2, 1}, {0, 1}, {2 * I, 1}};
	//circ c[3] = {{0, 1}, {0, 2}, {0, 3}}; <-- a fun one

	puts("set 1"); apollonius(a);
	puts("set 2"); apollonius(b);
	puts("set 3"); apollonius(c);
}
```


## C#
This code finds all 8 possible circles touching the three given circles.

```c#

using System;

namespace ApolloniusProblemCalc
{
    class Program
    {
        static float rs = 0;
        static float xs = 0;
        static float ys = 0;

        public static void Main(string[] args)
        {
            float gx1;
            float gy1;
            float gr1;
            float gx2;
            float gy2;
            float gr2;
            float gx3;
            float gy3;
            float gr3;

            //----------Enter values for the given circles here----------
            gx1 = 0;
            gy1 = 0;
            gr1 = 1;
            gx2 = 4;
            gy2 = 0;
            gr2 = 1;
            gx3 = 2;
            gy3 = 4;
            gr3 = 2;
            //-----------------------------------------------------------

            for (int i = 1; i <= 8; i++)
            {
                SolveTheApollonius(i, gx1, gy1, gr1, gx2, gy2, gr2, gx3, gy3, gr3);


                if (i == 1)
                {
                    Console.WriteLine("X of point of the " + i + "st solution: " + xs.ToString());
                    Console.WriteLine("Y of point of the " + i + "st solution: " + ys.ToString());
                    Console.WriteLine(i + "st Solution circle's radius: " + rs.ToString());
                }
                else if (i == 2)
                {
                    Console.WriteLine("X of point of the " + i + "ed solution: " + xs.ToString());
                    Console.WriteLine("Y of point of the " + i + "ed solution: " + ys.ToString());
                    Console.WriteLine(i + "ed Solution circle's radius: " + rs.ToString());
                }
                else if(i == 3)
                {
                    Console.WriteLine("X of point of the " + i + "rd solution: " + xs.ToString());
                    Console.WriteLine("Y of point of the " + i + "rd solution: " + ys.ToString());
                    Console.WriteLine(i + "rd Solution circle's radius: " + rs.ToString());
                }
                else
                {
                    Console.WriteLine("X of point of the " + i + "th solution: " + xs.ToString());
                    Console.WriteLine("Y of point of the " + i + "th solution: " + ys.ToString());
                    Console.WriteLine(i + "th Solution circle's radius: " + rs.ToString());
                }

                Console.WriteLine();
            }


            Console.ReadKey(true);
        }

        private static void SolveTheApollonius(int calcCounter, float x1, float y1, float r1, float x2, float y2, float r2, float x3, float y3, float r3)
        {
            float s1 = 1;
            float s2 = 1;
            float s3 = 1;

            if (calcCounter == 2)
            {
                s1 = -1;
                s2 = -1;
                s3 = -1;
            }
            else if (calcCounter == 3)
            {
                s1 = 1;
                s2 = -1;
                s3 = -1;
            }
            else if (calcCounter == 4)
            {
                s1 = -1;
                s2 = 1;
                s3 = -1;
            }
            else if (calcCounter == 5)
            {
                s1 = -1;
                s2 = -1;
                s3 = 1;
            }
            else if (calcCounter == 6)
            {
                s1 = 1;
                s2 = 1;
                s3 = -1;
            }
            else if (calcCounter == 7)
            {
                s1 = -1;
                s2 = 1;
                s3 = 1;
            }
            else if (calcCounter == 8)
            {
                s1 = 1;
                s2 = -1;
                s3 = 1;
            }

            //This calculation to solve for the solution circles is cited from the Java version
            float v11 = 2 * x2 - 2 * x1;
            float v12 = 2 * y2 - 2 * y1;
            float v13 = x1 * x1 - x2 * x2 + y1 * y1 - y2 * y2 - r1 * r1 + r2 * r2;
            float v14 = 2 * s2 * r2 - 2 * s1 * r1;

            float v21 = 2 * x3 - 2 * x2;
            float v22 = 2 * y3 - 2 * y2;
            float v23 = x2 * x2 - x3 * x3 + y2 * y2 - y3 * y3 - r2 * r2 + r3 * r3;
            float v24 = 2 * s3 * r3 - 2 * s2 * r2;

            float w12 = v12 / v11;
            float w13 = v13 / v11;
            float w14 = v14 / v11;

            float w22 = v22 / v21 - w12;
            float w23 = v23 / v21 - w13;
            float w24 = v24 / v21 - w14;

            float P = -w23 / w22;
            float Q = w24 / w22;
            float M = -w12 * P - w13;
            float N = w14 - w12 * Q;

            float a = N * N + Q * Q - 1;
            float b = 2 * M * N - 2 * N * x1 + 2 * P * Q - 2 * Q * y1 + 2 * s1 * r1;
            float c = x1 * x1 + M * M - 2 * M * x1 + P * P + y1 * y1 - 2 * P * y1 - r1 * r1;

            float D = b * b - 4 * a * c;

            rs = (-b - float.Parse(Math.Sqrt(D).ToString())) / (2 * float.Parse(a.ToString()));
            xs = M + N * rs;
            ys = P + Q * rs;
        }
    }
}

```



## CoffeeScript

```coffeescript

class Circle
  constructor: (@x, @y, @r) ->

apollonius = (c1, c2, c3, s1=1, s2=1, s3=1) ->
  [x1, y1, r1] = [c1.x, c1.y, c1.r]
  [x2, y2, r2] = [c2.x, c2.y, c2.r]
  [x3, y3, r3] = [c3.x, c3.y, c3.r]

  sq = (n) -> n*n

  v11 = 2*x2 - 2*x1
  v12 = 2*y2 - 2*y1
  v13 = sq(x1) - sq(x2) + sq(y1) - sq(y2) - sq(r1) + sq(r2)
  v14 = 2*s2*r2 - 2*s1*r1

  v21 = 2*x3 - 2*x2
  v22 = 2*y3 - 2*y2
  v23 = sq(x2) - sq(x3) + sq(y2) - sq(y3) - sq(r2) + sq(r3)
  v24 = 2*s3*r3 - 2*s2*r2

  w12 = v12/v11
  w13 = v13/v11
  w14 = v14/v11

  w22 = v22/v21 - w12
  w23 = v23/v21 - w13
  w24 = v24/v21 - w14

  p = -w23/w22
  q = w24/w22
  m = -w12*p - w13
  n = w14 - w12*q

  a = sq(n) + sq(q) - 1
  b = 2*m*n - 2*n*x1 + 2*p*q - 2*q*y1 + 2*s1*r1
  c = sq(x1) + sq(m) - 2*m*x1 + sq(p) + sq(y1) - 2*p*y1 - sq(r1)

  d = sq(b) - 4*a*c
  rs = (-b - Math.sqrt(d)) / (2*a)
  xs = m + n*rs
  ys = p + q*rs

  new Circle(xs, ys, rs)


console.log c1 = new Circle(0, 0, 1)
console.log c2 = new Circle(2, 4, 2)
console.log c3 = new Circle(4, 0, 1)

console.log apollonius(c1, c2, c3)
console.log apollonius(c1, c2, c3, -1, -1, -1)


```

output
<lang>
> coffee foo.coffee
{ x: 0, y: 0, r: 1 }
{ x: 2, y: 4, r: 2 }
{ x: 4, y: 0, r: 1 }
{ x: 2, y: 2.1, r: 3.9 }
{ x: 2, y: 0.8333333333333333, r: 1.1666666666666667 }

```



## D

```d
import std.stdio, std.math;

immutable struct Circle { double x, y, r; }
enum Tangent { externally, internally }

/**
Solves the Problem of Apollonius (finding a circle tangent to three
other circles in the plane).

Params:
  c1 = First circle of the problem.
  c2 = Second circle of the problem.
  c3 = Third circle of the problem.
  t1 = How is the solution tangent (externally or internally) to c1.
  t2 = How is the solution tangent (externally or internally) to c2.
  t3 = How is the solution tangent (externally or internally) to c3.

Returns: The Circle that is tangent to c1, c2 and c3.
*/
Circle solveApollonius(in Circle c1, in Circle c2, in Circle c3,
                       in Tangent t1, in Tangent t2, in Tangent t3)
pure nothrow @safe @nogc {
    alias Imd = immutable(double);
    Imd s1 = (t1 == Tangent.externally) ? 1.0 : -1.0;
    Imd s2 = (t2 == Tangent.externally) ? 1.0 : -1.0;
    Imd s3 = (t3 == Tangent.externally) ? 1.0 : -1.0;

    Imd v11 = 2 * c2.x - 2 * c1.x;
    Imd v12 = 2 * c2.y - 2 * c1.y;
    Imd v13 = c1.x ^^ 2 - c2.x ^^ 2 +
              c1.y ^^ 2 - c2.y ^^ 2 -
              c1.r ^^ 2 + c2.r ^^ 2;
    Imd v14 = 2 * s2 * c2.r - 2 * s1 * c1.r;

    Imd v21 = 2 * c3.x - 2 * c2.x;
    Imd v22 = 2 * c3.y - 2 * c2.y;
    Imd v23 = c2.x ^^ 2 - c3.x ^^ 2 +
              c2.y ^^ 2 - c3.y ^^ 2 -
              c2.r ^^ 2 + c3.r ^^ 2;
    Imd v24 = 2 * s3 * c3.r - 2 * s2 * c2.r;

    Imd w12 = v12 / v11;
    Imd w13 = v13 / v11;
    Imd w14 = v14 / v11;

    Imd w22 = v22 / v21 - w12;
    Imd w23 = v23 / v21 - w13;
    Imd w24 = v24 / v21 - w14;

    Imd P = -w23 / w22;
    Imd Q =  w24 / w22;
    Imd M = -w12 * P - w13;
    Imd N =  w14 - w12 * Q;

    Imd a = N * N + Q ^^ 2 - 1;
    Imd b = 2 * M * N - 2 * N * c1.x +
            2 * P * Q - 2 * Q * c1.y +
            2 * s1 * c1.r;
    Imd c = c1.x ^^ 2 + M ^^ 2 - 2 * M * c1.x +
            P ^^ 2 + c1.y ^^ 2 - 2 * P * c1.y - c1.r ^^ 2;

    // find a root of a quadratic equation.
    // This requires the circle centers not to be e.g. colinear
    Imd D = b ^^ 2 - 4 * a * c;
    Imd rs = (-b - D.sqrt) / (2 * a);

    return Circle(M + N * rs, P + Q * rs, rs);
}

void main() {
    immutable c1 = Circle(0.0, 0.0, 1.0);
    immutable c2 = Circle(4.0, 0.0, 1.0);
    immutable c3 = Circle(2.0, 4.0, 2.0);

    alias Te = Tangent.externally;
    solveApollonius(c1, c2, c3, Te, Te, Te).writeln;

    alias Ti = Tangent.internally;
    solveApollonius(c1, c2, c3, Ti, Ti, Ti).writeln;
}
```

```txt
immutable(Circle)(2, 2.1, 3.9)
immutable(Circle)(2, 0.833333, 1.16667)

```



## Elixir

```elixir
defmodule Circle do
  def apollonius(c1, c2, c3, s1, s2, s3) do
    {x1, y1, r1} = c1
    {w12, w13, w14} = calc(c1, c2, s1, s2)
    {u22, u23, u24} = calc(c2, c3, s2, s3)
    {w22, w23, w24} = {u22 - w12, u23 - w13, u24 - w14}

    p = -w23 / w22
    q = w24 / w22
    m = -w12 * p - w13
    n = w14 - w12 * q

    a = n*n + q*q - 1
    b = 2*m*n - 2*n*x1 + 2*p*q - 2*q*y1 + 2*s1*r1
    c = x1*x1 + m*m - 2*m*x1 + p*p + y1*y1 - 2*p*y1 - r1*r1

    d = b*b - 4*a*c
    rs = (-b - :math.sqrt(d)) / (2*a)
    {m + n*rs, p + q*rs, rs}
  end

  defp calc({x1, y1, r1}, {x2, y2, r2}, s1, s2) do
    v1 = x2 - x1
    {(y2 - y1) / v1, (x1*x1 - x2*x2 + y1*y1 - y2*y2 - r1*r1 + r2*r2) / (2*v1), (s2*r2 - s1*r1) / v1}
  end
end

c1 = {0, 0, 1}
c2 = {2, 4, 2}
c3 = {4, 0, 1}

IO.inspect Circle.apollonius(c1, c2, c3, 1, 1, 1)
IO.inspect Circle.apollonius(c1, c2, c3, -1, -1, -1)
```


```txt

{2.0, 2.1, 3.9}
{2.0, 0.8333333333333333, 1.1666666666666667}

```


=={{header|F_Sharp|F#}}==
```fsharp
type point = { x:float; y:float }
type circle = { center: point; radius: float; }

let new_circle x y r =
   { center = { x=x; y=y }; radius = r }

let print_circle c =
   printfn "Circle(x=%.2f, y=%.2f, r=%.2f)"
     c.center.x c.center.y c.radius

let xyr c = c.center.x, c.center.y, c.radius

let solve_apollonius c1 c2 c3
                     s1 s2 s3 =

   let x1, y1, r1 = xyr c1
   let x2, y2, r2 = xyr c2
   let x3, y3, r3 = xyr c3

   let v11 = 2. * x2 - 2. * x1
   let v12 = 2. * y2 - 2. * y1
   let v13 = x1*x1 - x2*x2 + y1*y1 - y2*y2 - r1*r1 + r2*r2
   let v14 = (2. * s2 * r2) - (2. * s1 * r1)

   let v21 = 2. * x3 - 2. * x2
   let v22 = 2. * y3 - 2. * y2
   let v23 = x2*x2 - x3*x3 + y2*y2 - y3*y3 - r2*r2 + r3*r3
   let v24 = (2. * s3 * r3) - (2. * s2 * r2)

   let w12 = v12 / v11
   let w13 = v13 / v11
   let w14 = v14 / v11

   let w22 = v22 / v21 - w12
   let w23 = v23 / v21 - w13
   let w24 = v24 / v21 - w14

   let p = - w23 / w22
   let q = w24 / w22
   let m = - w12 * p - w13
   let n = w14 - w12 * q

   let a = n*n + q*q - 1.
   let b = 2.*m*n - 2.*n*x1 + 2.*p*q - 2.*q*y1 + 2.*s1*r1
   let c = x1*x1 + m*m - 2.*m*x1 + p*p + y1*y1 - 2.*p*y1 - r1*r1

   let d = b * b - 4. * a * c
   let rs = (- b - (sqrt d)) / (2. * a)

   let xs = m + n * rs
   let ys = p + q * rs

   new_circle xs ys rs


[<EntryPoint>]
let main argv =
  let c1 = new_circle 0. 0. 1.
  let c2 = new_circle 4. 0. 1.
  let c3 = new_circle 2. 4. 2.

  let r1 = solve_apollonius c1 c2 c3 1. 1. 1.
  print_circle r1

  let r2 = solve_apollonius c1 c2 c3 (-1.) (-1.) (-1.)
  print_circle r2
  0
```

```txt
Circle(x=2.00, y=2.10, r=3.90)
Circle(x=2.00, y=0.83, r=1.17)
```



## Fortran

```fortran
program Apollonius
  implicit none

  integer, parameter :: dp = selected_real_kind(15)

  type circle
    real(dp) :: x
    real(dp) :: y
    real(dp) :: radius
  end type

  type(circle) :: c1 , c2, c3, r

  c1 = circle(0.0, 0.0, 1.0)
  c2 = circle(4.0, 0.0, 1.0)
  c3 = circle(2.0, 4.0, 2.0)

  write(*, "(a,3f12.8))") "External tangent:", SolveApollonius(c1, c2, c3, 1, 1, 1)
  write(*, "(a,3f12.8))") "Internal tangent:", SolveApollonius(c1, c2, c3, -1, -1, -1)

contains

function SolveApollonius(c1, c2, c3, s1, s2, s3) result(res)
  type(circle) :: res
  type(circle), intent(in) :: c1, c2, c3
  integer, intent(in) :: s1, s2, s3

  real(dp) :: x1, x2, x3, y1, y2, y3, r1, r2, r3
  real(dp) :: v11, v12, v13, v14
  real(dp) :: v21, v22, v23, v24
  real(dp) :: w12, w13, w14
  real(dp) :: w22, w23, w24
  real(dp) :: p, q, m, n, a, b, c, det

  x1 = c1%x; x2 = c2%x; x3 = c3%x
  y1 = c1%y; y2 = c2%y; y3 = c3%y
  r1 = c1%radius; r2 = c2%radius; r3 = c3%radius

  v11 = 2*x2 - 2*x1
  v12 = 2*y2 - 2*y1
  v13 = x1*x1 - x2*x2 + y1*y1 - y2*y2 - r1*r1 + r2*r2
  v14 = 2*s2*r2 - 2*s1*r1

  v21 = 2*x3 - 2*x2
  v22 = 2*y3 - 2*y2
  v23 = x2*x2 - x3*x3 + y2*y2 - y3*y3 - r2*r2 + r3*r3
  v24 = 2*s3*r3 - 2*s2*r2

  w12 = v12/v11
  w13 = v13/v11
  w14 = v14/v11

  w22 = v22/v21-w12
  w23 = v23/v21-w13
  w24 = v24/v21-w14

  p = -w23/w22
  q = w24/w22
  m = -w12*P - w13
  n = w14 - w12*q

  a = n*n + q*q - 1
  b = 2*m*n - 2*n*x1 + 2*p*q - 2*q*y1 + 2*s1*r1
  c = x1*x1 + m*m - 2*m*x1 + p*p + y1*y1 - 2*p*y1 - r1*r1

  det = b*b - 4*a*c
  res%radius = (-b-sqrt(det)) / (2*a)
  res%x = m + n*res%radius
  res%y = p + q*res%radius

end function
end program
```

Output

```txt
External tangent:  2.00000000  2.10000000  3.90000000
Internal tangent:  2.00000000  0.83333333  1.16666667
```


## Go

Simplified to produce only the fully interior and fully exterior solutions.

```go
package main

import (
    "fmt"
    "math"
)

type circle struct {
    x, y, r float64
}

func main() {
    c1 := circle{0, 0, 1}
    c2 := circle{4, 0, 1}
    c3 := circle{2, 4, 2}
    fmt.Println(ap(c1, c2, c3, true))
    fmt.Println(ap(c1, c2, c3, false))
}

func ap(c1, c2, c3 circle, s bool) circle {
    x1sq := c1.x * c1.x
    y1sq := c1.y * c1.y
    r1sq := c1.r * c1.r
    x2sq := c2.x * c2.x
    y2sq := c2.y * c2.y
    r2sq := c2.r * c2.r
    x3sq := c3.x * c3.x
    y3sq := c3.y * c3.y
    r3sq := c3.r * c3.r
    v11 := 2 * (c2.x - c1.x)
    v12 := 2 * (c2.y - c1.y)
    v13 := x1sq - x2sq + y1sq - y2sq - r1sq + r2sq
    v14 := 2 * (c2.r - c1.r)
    v21 := 2 * (c3.x - c2.x)
    v22 := 2 * (c3.y - c2.y)
    v23 := x2sq - x3sq + y2sq - y3sq - r2sq + r3sq
    v24 := 2 * (c3.r - c2.r)
    if s {
        v14 = -v14
        v24 = -v24
    }
    w12 := v12 / v11
    w13 := v13 / v11
    w14 := v14 / v11
    w22 := v22/v21 - w12
    w23 := v23/v21 - w13
    w24 := v24/v21 - w14
    p := -w23 / w22
    q := w24 / w22
    m := -w12*p - w13
    n := w14 - w12*q
    a := n*n + q*q - 1
    b := m*n - n*c1.x + p*q - q*c1.y
    if s {
        b -= c1.r
    } else {
        b += c1.r
    }
    b *= 2
    c := x1sq + m*m - 2*m*c1.x + p*p + y1sq - 2*p*c1.y - r1sq
    d := b*b - 4*a*c
    rs := (-b - math.Sqrt(d)) / (2 * a)
    return circle{m + n*rs, p + q*rs, rs}
}
```

Output:

```txt

{2 0.8333333333333333 1.1666666666666667}
{2 2.1 3.9}

```



## Haskell

```haskell
data Circle = Circle { x, y, r :: Double } deriving (Show, Eq)
data Tangent = Externally | Internally deriving Eq

{--
Solves the Problem of Apollonius (finding a circle tangent to three
other circles in the plane).

Params:
  c1 = First circle of the problem.
  c2 = Second circle of the problem.
  c3 = Third circle of the problem.
  t1 = How is the solution tangent (externally or internally) to c1.
  t2 = How is the solution tangent (externally or internally) to c2.
  t3 = How is the solution tangent (externally or internally) to c3.

Returns: The Circle that is tangent to c1, c2 and c3.
--}
solveApollonius :: Circle -> Circle -> Circle ->
                   Tangent -> Tangent -> Tangent ->
                   Circle
solveApollonius c1 c2 c3 t1 t2 t3 =
    Circle (m + n * rs) (p + q * rs) rs
    where
        s1 = if t1 == Externally then 1.0 else -1.0
        s2 = if t2 == Externally then 1.0 else -1.0
        s3 = if t3 == Externally then 1.0 else -1.0

        v11 = 2 * x c2 - 2 * x c1
        v12 = 2 * y c2 - 2 * y c1
        v13 = x c1 ^ 2 - x c2 ^ 2 +
              y c1 ^ 2 - y c2 ^ 2 -
              r c1 ^ 2 + r c2 ^ 2
        v14 = 2 * s2 * r c2 - 2 * s1 * r c1

        v21 = 2 * x c3 - 2 * x c2
        v22 = 2 * y c3 - 2 * y c2
        v23 = x c2 ^ 2 - x c3 ^ 2 +
              y c2 ^ 2 - y c3 ^ 2 -
              r c2 ^ 2 + r c3 ^ 2;
        v24 = 2 * s3 * r c3 - 2 * s2 * r c2

        w12 = v12 / v11
        w13 = v13 / v11
        w14 = v14 / v11

        w22 = v22 / v21 - w12
        w23 = v23 / v21 - w13
        w24 = v24 / v21 - w14

        p = -w23 / w22
        q =  w24 / w22
        m = -w12 * p - w13
        n =  w14 - w12 * q

        a = n * n + q ^ 2 - 1
        b = 2 * m * n - 2 * n * x c1 +
            2 * p * q - 2 * q * y c1 +
            2 * s1 * r c1
        c = x c1 ^ 2 + m ^ 2 - 2 * m * x c1 +
            p ^ 2 + y c1 ^ 2 - 2 * p * y c1 - r c1 ^ 2

        -- Find a root of a quadratic equation.
        -- This requires the circle centers not to be e.g. colinear.
        d = b ^ 2 - 4 * a * c
        rs = (-b - sqrt d) / (2 * a)

main = do
    let c1 = Circle 0.0 0.0 1.0
    let c2 = Circle 4.0 0.0 1.0
    let c3 = Circle 2.0 4.0 2.0
    let te = Externally
    print $ solveApollonius c1 c2 c3 te te te

    let ti = Internally
    print $ solveApollonius c1 c2 c3 ti ti ti
```

```txt
Circle {x = 2.0, y = 2.1, r = 3.9}
Circle {x = 2.0, y = 0.8333333333333333, r = 1.1666666666666667}
```


=={{header|Icon}} and {{header|Unicon}}==
This is a translation of the Java version.  [[File:Apollonius-unicon.png|thumb|Solution for Apollonius]]


```Icon
link graphics

record circle(x,y,r)
global scale,xoffset,yoffset,yadjust

procedure main()

WOpen("size=400,400") | stop("Unable to open Window")
scale := 28
xoffset := WAttrib("width") / 2
yoffset := ( yadjust := WAttrib("height")) / 2


WC(c1 := circle(0,0,1),"black")
WC(c2 := circle(4,0,1),"black")
WC(c3 := circle(2,4,2),"black")
WC(c4 := Apollonius(c1,c2,c3,1,1,1),"green")    #/ Expects "Circle[x=2.00,y=2.10,r=3.90]" (green circle in image)
WC(c5 := Apollonius(c1,c2,c3,-1,-1,-1),"red")   #/ Expects "Circle[x=2.00,y=0.83,r=1.17]" (red circle in image)


WAttrib("fg=blue")
DrawLine( 0*scale+xoffset, yadjust-(-1*scale+yoffset),  0*scale+xoffset, yadjust-(4*scale+yoffset) )
DrawLine( -1*scale+xoffset, yadjust-(0*scale+yoffset),  4*scale+xoffset, yadjust-(0*scale+yoffset) )
WDone()
end

procedure WC(c,fg)  # write and plot circle
WAttrib("fg="||fg)
DrawCircle(c.x*scale+xoffset, yadjust-(c.y*scale+yoffset), c.r*scale)
return write("Circle(x,y,r) := (",c.x,", ",c.y,", ",c.r,")")
end

procedure Apollonius(c1,c2,c3,s1,s2,s3)  # solve Apollonius

  v11 := 2.*(c2.x - c1.x)
  v12 := 2.*(c2.y - c1.y)
  v13 := c1.x^2 - c2.x^2 + c1.y^2 - c2.y^2 - c1.r^2 + c2.r^2
  v14 := 2.*(s2*c2.r - s1*c1.r)

  v21 := 2.*(c3.x - c2.x)
  v22 := 2.*(c3.y - c2.y)
  v23 := c2.x^2 - c3.x^2 + c2.y^2 - c3.y^2 - c2.r^2 + c3.r^2
  v24 := 2.*(s3*c3.r - s2*c2.r)

  w12 := v12/v11
  w13 := v13/v11
  w14 := v14/v11

  w22 := v22/v21-w12
  w23 := v23/v21-w13
  w24 := v24/v21-w14

  P := -w23/w22
  Q := w24/w22
  M := -w12*P-w13
  N := w14 - w12*Q

  a := N*N + Q*Q - 1
  b := 2*M*N - 2*N*c1.x + 2*P*Q - 2*Q*c1.y + 2*s1*c1.r
  c := c1.x*c1.x + M*M - 2*M*c1.x + P*P + c1.y*c1.y - 2*P*c1.y - c1.r*c1.r

  #// Find a root of a quadratic equation. This requires the circle centers not to be e.g. colinear
  D := b*b-4*a*c
  rs := (-b-sqrt(D))/(2*a)
  xs := M + N * rs
  ys := P + Q * rs
  return circle(xs,ys,rs)
end
```


Output:
```txt
Circle(x,y,r) := (0, 0, 1)
Circle(x,y,r) := (4, 0, 1)
Circle(x,y,r) := (2, 4, 2)
Circle(x,y,r) := (2.0, 2.1, 3.9)
Circle(x,y,r) := (2.0, 0.8333333333333333, 1.166666666666667)
```



## J

'''Solution'''

```j
require 'math/misc/amoeba'

NB.*apollonius v solves Apollonius problems
NB. y is Cx0 Cy0 R0,  Cx1 Cy1 R1,:  Cx2 Cy2 R2
NB. x are radius scale factors to control which circles are included
NB.   in the common tangent circle.  1 to surround, _1 to exclude.
NB. returns Cxs Cys Rs
apollonius =: verb define"1 _
 1 apollonius y
:
 centers=. 2{."1 y
 radii=. x * {:"1 y
 goal=. 1e_20                               NB. goal simplex volume
 dist=. radii + [: +/"1&.:*: centers -"1 ]  NB. distances to tangents
 'soln err'=. ([: +/@:*:@, -/~@dist) f. amoeba goal centers
 if. err > 10 * goal do. '' return. end.    NB. no solution found
 avg=. +/ % #
 (, avg@dist) soln
)
```


'''Usage'''

```j
   ]rctst=: 0 0 1,4 0 1,:2 4 2           NB. Task circles
0 0 1
4 0 1
2 4 2
   (_1 _1 _1 ,: 1 1 1) apollonius rctst  NB. internally & externally tangent solutions
2 0.83333333 1.1666667
2        2.1       3.9

```



## Java


```Java
public class Circle
{
 public double[] center;
 public double radius;
 public Circle(double[] center, double radius)
 {
  this.center = center;
  this.radius = radius;
 }
 public String toString()
 {
  return String.format("Circle[x=%.2f,y=%.2f,r=%.2f]",center[0],center[1],
		       radius);
 }
}

public class ApolloniusSolver
{
/** Solves the Problem of Apollonius (finding a circle tangent to three other
  * circles in the plane). The method uses approximately 68 heavy operations
  * (multiplication, division, square-roots).
  * @param c1 One of the circles in the problem
  * @param c2 One of the circles in the problem
  * @param c3 One of the circles in the problem
  * @param s1 An indication if the solution should be externally or internally
  *           tangent (+1/-1) to c1
  * @param s2 An indication if the solution should be externally or internally
  *           tangent (+1/-1) to c2
  * @param s3 An indication if the solution should be externally or internally
  *           tangent (+1/-1) to c3
  * @return The circle that is tangent to c1, c2 and c3.
  */
 public static Circle solveApollonius(Circle c1, Circle c2, Circle c3, int s1,
				      int s2, int s3)
 {
  float x1 = c1.center[0];
  float y1 = c1.center[1];
  float r1 = c1.radius;
  float x2 = c2.center[0];
  float y2 = c2.center[1];
  float r2 = c2.radius;
  float x3 = c3.center[0];
  float y3 = c3.center[1];
  float r3 = c3.radius;

  //Currently optimized for fewest multiplications. Should be optimized for
  //readability
  float v11 = 2*x2 - 2*x1;
  float v12 = 2*y2 - 2*y1;
  float v13 = x1*x1 - x2*x2 + y1*y1 - y2*y2 - r1*r1 + r2*r2;
  float v14 = 2*s2*r2 - 2*s1*r1;

  float v21 = 2*x3 - 2*x2;
  float v22 = 2*y3 - 2*y2;
  float v23 = x2*x2 - x3*x3 + y2*y2 - y3*y3 - r2*r2 + r3*r3;
  float v24 = 2*s3*r3 - 2*s2*r2;

  float w12 = v12/v11;
  float w13 = v13/v11;
  float w14 = v14/v11;

  float w22 = v22/v21-w12;
  float w23 = v23/v21-w13;
  float w24 = v24/v21-w14;

  float P = -w23/w22;
  float Q = w24/w22;
  float M = -w12*P-w13;
  float N = w14 - w12*Q;

  float a = N*N + Q*Q - 1;
  float b = 2*M*N - 2*N*x1 + 2*P*Q - 2*Q*y1 + 2*s1*r1;
  float c = x1*x1 + M*M - 2*M*x1 + P*P + y1*y1 - 2*P*y1 - r1*r1;

  // Find a root of a quadratic equation. This requires the circle centers not
  // to be e.g. colinear
  float D = b*b-4*a*c;
  float rs = (-b-Math.sqrt(D))/(2*a);
  float xs = M + N * rs;
  float ys = P + Q * rs;
  return new Circle(new double[]{xs,ys}, rs);
 }
 public static void main(final String[] args)
 {
  Circle c1 = new Circle(new double[]{0,0}, 1);
  Circle c2 = new Circle(new double[]{4,0}, 1);
  Circle c3 = new Circle(new double[]{2,4}, 2);
  // Expects "Circle[x=2.00,y=2.10,r=3.90]" (green circle in image)
  System.out.println(solveApollonius(c1,c2,c3,1,1,1));
  // Expects "Circle[x=2.00,y=0.83,r=1.17]" (red circle in image)
  System.out.println(solveApollonius(c1,c2,c3,-1,-1,-1));
 }
}
```




## jq

```jq
def circle:
  {"x": .[0], "y": .[1], "r": .[2]};

# Find the interior or exterior Apollonius circle of three circles:
# ap(circle, circle, circle, boolean)
# Specify s as true for interior; false for exterior
def ap(c1; c2; c3; s):
  def sign: if s then -. else . end;
    (c1.x * c1.x) as $x1sq
  | (c1.y * c1.y) as $y1sq
  | (c1.r * c1.r) as $r1sq
  | (c2.x * c2.x) as $x2sq
  | (c2.y * c2.y) as $y2sq
  | (c2.r * c2.r) as $r2sq
  | (c3.x * c3.x) as $x3sq
  | (c3.y * c3.y) as $y3sq
  | (c3.r * c3.r) as $r3sq

  | (2 * (c2.x - c1.x)) as $v11
  | (2 * (c2.y - c1.y)) as $v12
  | ($x1sq - $x2sq + $y1sq - $y2sq - $r1sq + $r2sq) as $v13

  | (2 * (c2.r - c1.r) | sign) as $v14
  | (2 * (c3.x - c2.x)) as $v21
  | (2 * (c3.y - c2.y)) as $v22

  | ($x2sq - $x3sq + $y2sq - $y3sq - $r2sq + $r3sq) as $v23
  | ( 2 * c3.r - c2.r | sign) as $v24
  | ($v12 / $v11) as $w12
  | ($v13 / $v11) as $w13
  | ($v14 / $v11) as $w14

  | (($v22 / $v21) - $w12) as $w22
  | (($v23 / $v21) - $w13) as $w23
  | (($v24 / $v21) - $w14) as $w24

  | (-$w23 / $w22) as $p
  | ( $w24 / $w22) as $q
  | ((-$w12*$p) - $w13) as $m
  | ( $w14 - ($w12*$q)) as $n

  | ( $n*$n + $q*$q - 1 ) as $a
  | (2 * (($m*$n - $n*c1.x + $p*$q - $q*c1.y) + (c1.r|sign))) as $b
  | ($x1sq + $m*$m - 2*$m*c1.x + $p*$p + $y1sq - 2*$p*c1.y - $r1sq) as $c

  | ( $b*$b - 4*$a*$c ) as $d                   # discriminant
  | (( -$b - (($d|sqrt))) / (2 * $a)) as $rs    # root

  | [$m + ($n*$rs),  $p + ($q*$rs),  $rs]
  | circle
;
```

'''The task''':

```jq
def task:
     ([0, 0, 1] | circle) as $c1
  |  ([4, 0, 1] | circle) as $c2
  |  ([2, 4, 2] | circle) as $c3
  | ( ap($c1; $c2; $c3; true),      # interior
      ap($c1; $c2; $c3; false) )    # exterior
;
```

```sh
$ jq -n -c -f apollonius.jq
{"x":2,"y":0.8333333333333333,"r":1.1666666666666667}
{"x":2,"y":2.1,"r":3.9}
```



## Julia

This solution follows the algebraic solution from [http://mathworld.wolfram.com/ApolloniusProblem.html Weisstein, Eric W. "Apollonius' Problem." From MathWorld--A Wolfram Web Resource].  The [https://github.com/Keno/Polynomials.jl Polynomials] package is used to solve the quadratic equation for the radius (equation 1 in the reference) rather than hard coding it.

The <code>enc</code> array passed to the <code>apollonius</code> function, specifies which of the three defining circles are to be enclosed in the solution.  For this task only the "internal" (<code>enc=[]</code>) and "external" (<code>enc=[1:3]</code>) are called for.

'''Module''':

```julia
module ApolloniusProblems

using Polynomials
export Circle

struct Point{T<:Real}
    x::T
    y::T
end

xcoord(p::Point) = p.x
ycoord(p::Point) = p.y

struct Circle{T<:Real}
    c::Point{T}
    r::T
end
Circle(x::T, y::T, r::T) where T<:Real = Circle(Point(x, y), r)

radius(c::Circle) = c.r
center(c::Circle) = c.c
xcenter(c::Circle) = xcoord(center(c))
ycenter(c::Circle) = ycoord(center(c))

Base.show(io::IO, c::Circle) =
    @printf(io, "centered at (%0.4f, %0.4f) with radius %0.4f",
        xcenter(c), ycenter(c), radius(c))

function solve(ap::Vector{Circle{T}}, enc=()) where T<:Real
    length(ap) == 3 || error("This Apollonius problem needs 3 circles.")
    x = @. xcenter(ap)
    y = @. ycenter(ap)
    r = map(u -> ifelse(u ∈ enc, -1, 1), 1:3) .* radius.(ap)
    @views begin
        a = 2x[1] .- 2x[2:3]
        b = 2y[1] .- 2y[2:3]
        c = 2r[1] .- 2r[2:3]
        d = (x[1] ^ 2 + y[1] ^ 2 - r[1] ^ 2) .- (x[2:3] .^ 2 .+ y[2:3] .^ 2 .- r[2:3] .^ 2)
    end
    u = Poly([-det([b d]), det([b c])] ./ det([a b]))
    v = Poly([det([a d]), -det([a c])] ./ det([a b]))
    w = Poly([r[1], 1.0]) ^ 2
    s = (u - x[1]) ^ 2 + (v - y[1]) ^ 2 - w
    r = filter(x -> iszero(imag(x)) && x > zero(x), roots(s))
    length(r) <  2 || error("The solution is not unique.")
    length(r) == 1 || error("There is no solution.")
    r = r[1]
    return Circle(polyval(u, r), polyval(v, r), r)
end

end  # module ApolloniusProblem
```


'''Main''':

```julia
include("module.jl")
using ApolloniusProblems

let test = [Circle(0.0, 0.0, 1.0), Circle(4.0, 0.0, 1.0), Circle(2.0, 4.0, 2.0)]
    println("The defining circles are: \n - ", join(test, "\n - "))
    println("The internal circle is:\n\t", ApolloniusProblems.solve(test))
    println("The external circle is:\n\t", ApolloniusProblems.solve(test, 1:3))
end
```


```txt
The defining circles are:
 - centered at (0.0000, 0.0000) with radius 1.0000
 - centered at (4.0000, 0.0000) with radius 1.0000
 - centered at (2.0000, 4.0000) with radius 2.0000
The internal circle is:
    centered at (2.0000, 0.8333) with radius 1.1667
The external circle is:
    centered at (2.0000, 2.1000) with radius 3.9000
```



## Kotlin

```scala
// version 1.1.3

data class Circle(val x: Double, val y: Double, val r: Double)

val Double.sq get() = this * this

fun solveApollonius(c1: Circle, c2: Circle, c3: Circle,
                    s1: Int, s2: Int, s3: Int): Circle {
    val (x1, y1, r1) = c1
    val (x2, y2, r2) = c2
    val (x3, y3, r3) = c3

    val v11 = 2 * x2 - 2 * x1
    val v12 = 2 * y2 - 2 * y1
    val v13 = x1.sq - x2.sq + y1.sq - y2.sq - r1.sq + r2.sq
    val v14 = 2 * s2 * r2 - 2 * s1 * r1

    val v21 = 2 * x3 - 2 * x2
    val v22 = 2 * y3 - 2 * y2
    val v23 = x2.sq - x3.sq + y2.sq - y3.sq - r2.sq + r3.sq
    val v24 = 2 * s3 * r3 - 2 * s2 * r2

    val w12 = v12 / v11
    val w13 = v13 / v11
    val w14 = v14 / v11

    val w22 = v22 / v21 - w12
    val w23 = v23 / v21 - w13
    val w24 = v24 / v21 - w14

    val p = -w23 / w22
    val q =  w24 / w22
    val m = -w12 * p - w13
    val n =  w14 - w12 * q

    val a = n.sq +  q.sq - 1
    val b = 2 * m * n - 2 * n * x1 + 2 * p * q - 2 * q * y1 + 2 * s1 * r1
    val c = x1.sq + m.sq - 2 * m * x1 + p.sq + y1.sq - 2 * p * y1 - r1.sq

    val d = b.sq - 4 * a * c
    val rs = (-b - Math.sqrt(d)) / (2 * a)
    val xs = m + n * rs
    val ys = p + q * rs
    return Circle(xs, ys, rs)
}

fun main(args: Array<String>) {
    val c1 = Circle(0.0, 0.0, 1.0)
    val c2 = Circle(4.0, 0.0, 1.0)
    val c3 = Circle(2.0, 4.0, 2.0)
    println(solveApollonius(c1, c2, c3, 1, 1, 1))
    println(solveApollonius(c1, c2, c3,-1,-1,-1))
}
```


```txt

Circle(x=2.0, y=2.1, r=3.9)
Circle(x=2.0, y=0.8333333333333333, r=1.1666666666666667)

```



## Lasso

```Lasso
define solveApollonius(c1, c2, c3, s1, s2, s3) => {
	local(
		x1 = decimal(#c1->get(1)),
		y1 = decimal(#c1->get(2)),
		r1 = decimal(#c1->get(3))
	)
	local(
		x2 = decimal(#c2->get(1)),
		y2 = decimal(#c2->get(2)),
		r2 = decimal(#c2->get(3))
	)
	local(
		x3 = decimal(#c3->get(1)),
		y3 = decimal(#c3->get(2)),
		r3 = decimal(#c3->get(3))
	)
    local(
    	v11 = 2*#x2 - 2*#x1,
    	v12 = 2*#y2 - 2*#y1,
    	v13 = #x1*#x1 - #x2*#x2 + #y1*#y1 - #y2*#y2 - #r1*#r1 + #r2*#r2,
    	v14 = 2*#s2*#r2 - 2*#s1*#r1,

		v21 = 2*#x3 - 2*#x2,
		v22 = 2*#y3 - 2*#y2,
		v23 = #x2*#x2 - #x3*#x3 + #y2*#y2 - #y3*#y3 - #r2*#r2 + #r3*#r3,
		v24 = 2*#s3*#r3 - 2*#s2*#r2,

		w12 = #v12/#v11,
		w13 = #v13/#v11,
		w14 = #v14/#v11,

		w22 = #v22/#v21-#w12,
		w23 = #v23/#v21-#w13,
		w24 = #v24/#v21-#w14,

		P = -#w23/#w22,
		Q = #w24/#w22,
		M = -#w12*#P-#w13,
		N = #w14 - #w12*#Q,

		a = #N*#N + #Q*#Q - 1,
		b = 2*#M*#N - 2*#N*#x1 + 2*#P*#Q - 2*#Q*#y1 + 2*#s1*#r1,
		c = #x1*#x1 + #M*#M - 2*#M*#x1 + #P*#P + #y1*#y1 - 2*#P*#y1 - #r1*#r1

	)

	// Find a root of a quadratic equation. This requires the circle centers not to be e.g. colinear
	local(
		D = #b*#b-4*#a*#c,
		rs = (-#b - #D->sqrt)/(2*#a),

		xs = #M+#N*#rs,
		ys = #P+#Q*#rs
	)
	return (:#xs, #ys, #rs)
}
// Tests:
solveApollonius((:0, 0, 1), (:4, 0, 1), (:2, 4, 2), 1,1,1)
solveApollonius((:0, 0, 1), (:4, 0, 1), (:2, 4, 2), -1,-1,-1)

```

```txt
staticarray(2.000000, 2.100000, 3.900000)
staticarray(2.000000, 0.833333, 1.166667)
```



## Liberty BASIC

Uses the string Circle$ to hold  "xPos, yPos, radius" as csv data. A GUI representation is very easily added.

```lb

  circle1$ ="  0.000,  0.000,  1.000"
  circle2$ ="  4.000,  0.000,  1.000"
  circle3$ ="  2.000,  4.000,  2.000"

  print "  x_pos   y_pos   radius"
  print circle1$
  print circle2$
  print circle3$
  print
  print ApolloniusSolver$( circle1$, circle2$, circle3$,  1,  1,  1)
  print ApolloniusSolver$( circle1$, circle2$, circle3$, -1, -1, -1)

  end

function ApolloniusSolver$( c1$, c2$, c3$, s1, s2, s3)
  x1     =val( word$( c1$, 1, ",")): y1 =val( word$( c1$, 2, ",")): r1 =val( word$( c1$, 3, ","))
  x2     =val( word$( c2$, 1, ",")): y2 =val( word$( c2$, 2, ",")): r2 =val( word$( c2$, 3, ","))
  x3     =val( word$( c3$, 1, ",")): y3 =val( word$( c3$, 2, ",")): r3 =val( word$( c3$, 3, ","))

  v11     = 2 *x2 -2 *x1
  v12     = 2 *y2 -2*y1
  v13     = x1 *x1 - x2 *x2 + y1 *y1 - y2 *y2  -r1 *r1 +r2 *r2
  v14     = 2 *s2 *r2 -2 *s1 *r1

  v21     = 2 *x3 -2 *x2
  v22     = 2 *y3 -2*y2
  v23     = x2 *x2 -x3 *x3 + y2 *y2 -y3 *y3 -r2 *r2 +r3 *r3
  v24     = 2 *s3 *r3 - 2 *s2 *r2

  w12     = v12 /v11
  w13     = v13 /v11
  w14     = v14 /v11

  w22     = v22 /v21 -w12
  w23     = v23 /v21 -w13
  w24     = v24 /v21 -w14

  P       = 0 -w23 /w22
  Q       =    w24 /w22
  M       = 0 -w12 *P -w13
  N       =    w14 -w12 *Q

  a       = N *N + Q *Q -1
  b       = 2 *M *N -2 *N *x1 + 2 *P *Q -2 *Q *y1 +2 *s1 *r1
  c       = x1 *x1 +M *M -2 *M *x1 +P *P +y1 *y1 -2 *P *y1 -r1 *r1

  D       = b *b -4 *a *c

  Radius  =( 0 -b -Sqr( D)) /( 2 *a)
  XPos    =M +N *Radius
  YPos    =P +Q *Radius

  ApolloniusSolver$ =using( "###.###", XPos) +"," +using( "###.###", YPos) +using( "###.###", Radius)
end function

```


 x_pos y_pos radius
 0.000, 0.000, 1.000
 4.000, 0.000, 1.000
 2.000, 4.000, 2.000

 2.000, 2.100, 3.900
 2.000, 0.833, 1.167




## Mathematica


```Mathematica
Apolonius[a1_,b1_,c1_,a2_,b2_,c2_,a3_,b3_,c3_,S1_,S2_ ,S3_ ]:=
Module[{x1=a1,y1=b1,r1=c1,x2=a2,y2=b2,r2=c2,x3=a3,y3=b3,r3=c3,s1=S1,s2=S2,s3=S3},
v11 = 2*x2 - 2*x1; v12 = 2*y2 - 2*y1;
v13 = x1^2 - x2^2 + y1^2 - y2^2 - r1^2 + r2^2;
v14 = 2*s2*r2 - 2*s1*r1;

v21 = 2*x3-2*x2 ; v22 = 2*y3 - 2*y2;
v23 = x2^2 - x3^2 + y2^2 - y3^2 - r2^2 + r3^2;
v24 = 2*s3*r3 - 2*s2*r2;

w12 = v12/v11; w13 = v13/v11; w14 = v14/v11;

w22 = v22/v21 - w12;
w23 = v23/v21 - w13;
w24 = v24/v21 - w14;

p = -w23/w22; q=w24/w22;
m = -w12*p - w13; n=w14 - w12*q;

a = n^2 + q^2-1;
b = 2*m*n - 2*n*x1 + 2*p*q - 2*q*y1 + 2*s1*r1;
c = x1^2+m^2 - 2*m*x1 + p^2+y1^2 - 2*p*y1 - r1^2;

d= b^2 - 4*a*c;
rs = (-b -Sqrt[d])/(2*a);
xs = m + n*rs; ys = p + q*rs;
Map[N,{xs, ys, rs} ]]
```



```txt
Apolonius[0,0,1,2,4,2,4,0,1,1,1,1]
->{2.,2.1,3.9}

Apolonius[0,0,1,2,4,2,4,0,1,-1,-1,-1]
->{2.,0.833333,1.16667}
```



## MUMPS

```MUMPS
APOLLONIUS(CIR1,CIR2,CIR3,S1,S2,S3)
 ;Circles are passed in as strings with three parts with a "^" separator in the order x^y^r
 ;The three circles are CIR1, CIR2, and CIR3
 ;The S1, S2, and S3 parameters determine if the solution will be internally or externally
 ;tangent to the circle. (+1 external, -1 internal)
 ;CIRR is the circle returned in the same format as the input circles
 ;
 ;Xn, Yn, and Rn are the values for a circle n - following the precedents from the
 ;other examples because doing $Pieces would make this confusing to read
 NEW X1,X2,X3,Y1,Y2,Y3,R1,R2,R3,RS,V11,V12,V13,V14,V21,V22,V23,V24,W12,W13,W14,W22,W23,W24,P,M,N,Q,A,B,C,D
 NEW CIRR
 SET X1=$PIECE(CIR1,"^",1),X2=$PIECE(CIR2,"^",1),X3=$PIECE(CIR3,"^",1)
 SET Y1=$PIECE(CIR1,"^",2),Y2=$PIECE(CIR2,"^",2),Y3=$PIECE(CIR3,"^",2)
 SET R1=$PIECE(CIR1,"^",3),R2=$PIECE(CIR2,"^",3),R3=$PIECE(CIR3,"^",3)
 SET V11=(2*X2)-(2*X1)
 SET V12=(2*Y2)-(2*Y1)
 SET V13=(X1*X1)-(X2*X2)+(Y1*Y1)-(Y2*Y2)-(R1*R1)+(R2*R2)
 SET V14=(2*S2*R2)-(2*S1*R1)
 SET V21=(2*X3)-(2*X2)
 SET V22=(2*Y3)-(2*Y2)
 SET V23=(X2*X2)-(X3*X3)+(Y2*Y2)-(Y3*Y3)-(R2*R2)+(R3*R3)
 SET V24=(2*S3*R3)-(2*S2*R2)
 SET W12=V12/V11
 SET W13=V13/V11
 SET W14=V14/V11
 SET W22=(V22/V21)-W12 ;Parentheses for insurance - MUMPS evaluates left to right
 SET W23=(V23/V21)-W13
 SET W24=(V24/V21)-W14
 SET P=-W23/W22
 SET Q=W24/W22
 SET M=-(W12*P)-W13
 SET N=W14-(W12*Q)
 SET A=(N*N)+(Q*Q)-1
 SET B=(2*M*N)-(2*N*X1)+(2*P*Q)-(2*Q*Y1)+(2*S1*R1)
 SET C=(X1*X1)+(M*M)+(2*M*X1)+(P*P)+(Y1*Y1)-(2*P*Y1)-(R1*R1)
 SET D=(B*B)-(4*A*C)
 SET RS=(-B-(D**.5))/(2*A)
 SET $PIECE(CIRR,"^",1)=M+(N*RS)
 SET $PIECE(CIRR,"^",2)=P+(Q*RS)
 SET $PIECE(CIRR,"^",3)=RS
 KILL X1,X2,X3,Y1,Y2,Y3,R1,R2,R3,RS,V11,V12,V13,V14,V21,V22,V23,V24,W12,W13,W14,W22,W23,W24,P,M,N,Q,A,B,C,D
 QUIT CIRR
```

In use:
```txt

USER>WRITE C1
0^0^1
USER>WRITE C2
4^0^1
USER>WRITE C3
2^4^2
USER>WRITE $$APOLLONIUS^ROSETTA(C1,C2,C3,1,1,1)
2^2.1^3.9
USER>WRITE $$APOLLONIUS^ROSETTA(C1,C2,C3,-1,-1,-1)
2^.833333333333333333^1.166666666666666667

```



## Nim

```nim
import math

type Circle = tuple[x, y, r: float]

proc solveApollonius(c1, c2, c3: Circle; s1, s2, s3: float): Circle =
  let
    v11 = 2*c2.x - 2*c1.x
    v12 = 2*c2.y - 2*c1.y
    v13 = c1.x*c1.x - c2.x*c2.x + c1.y*c1.y - c2.y*c2.y - c1.r*c1.r + c2.r*c2.r
    v14 = 2*s2*c2.r - 2*s1*c1.r

    v21 = 2*c3.x - 2*c2.x
    v22 = 2*c3.y - 2*c2.y
    v23 = c2.x*c2.x - c3.x*c3.x + c2.y*c2.y - c3.y*c3.y - c2.r*c2.r + c3.r*c3.r
    v24 = 2*s3*c3.r - 2*s2*c2.r

    w12 = v12/v11
    w13 = v13/v11
    w14 = v14/v11

    w22 = v22/v21-w12
    w23 = v23/v21-w13
    w24 = v24/v21-w14

    p = -w23/w22
    q = w24/w22
    m = -w12*p-w13
    n = w14 - w12*q

    a = n*n + q*q - 1
    b = 2*m*n - 2*n*c1.x + 2*p*q - 2*q*c1.y + 2*s1*c1.r
    c = c1.x*c1.x + m*m - 2*m*c1.x + p*p + c1.y*c1.y - 2*p*c1.y - c1.r*c1.r

    d = b*b-4*a*c
    rs = (-b-sqrt(d))/(2*a)

    xs = m+n*rs
    ys = p+q*rs

  return (xs, ys, rs)

let
  c1: Circle = (0.0, 0.0, 1.0)
  c2: Circle = (4.0, 0.0, 1.0)
  c3: Circle = (2.0, 4.0, 2.0)

echo solveApollonius(c1, c2, c3, 1.0, 1.0, 1.0)
echo solveApollonius(c1, c2, c3, -1.0, -1.0, -1.0)
```

Output:

```txt
(x: 2.0000000000000000e+00, y: 2.1000000000000001e+00, r: 3.8999999999999999e+00)
(x: 2.0000000000000000e+00, y: 8.3333333333333326e-01, r: 1.1666666666666667e+00)
```



## OCaml

```ocaml
type point = { x:float; y:float }
type circle = {
  center: point;
  radius: float;
}

let new_circle ~x ~y ~r =
  { center = { x=x; y=y };
    radius = r }

let print_circle ~c =
  Printf.printf "Circle(x=%.2f, y=%.2f, r=%.2f)\n"
    c.center.x c.center.y c.radius

let defxyr c =
  (c.center.x,
   c.center.y,
   c.radius)

let solve_apollonius ~c1 ~c2 ~c3
                     ~s1 ~s2 ~s3 =
  let ( * ) = ( *. ) in
  let ( / ) = ( /. ) in
  let ( + ) = ( +. ) in
  let ( - ) = ( -. ) in

  let x1, y1, r1 = defxyr c1
  and x2, y2, r2 = defxyr c2
  and x3, y3, r3 = defxyr c3 in

  let v11 = 2.0 * x2 - 2.0 * x1
  and v12 = 2.0 * y2 - 2.0 * y1
  and v13 = x1*x1 - x2*x2 + y1*y1 - y2*y2 - r1*r1 + r2*r2
  and v14 = (2.0 * s2 * r2) - (2.0 * s1 * r1)

  and v21 = 2.0 * x3 - 2.0 * x2
  and v22 = 2.0 * y3 - 2.0 * y2
  and v23 = x2*x2 - x3*x3 + y2*y2 - y3*y3 - r2*r2 + r3*r3
  and v24 = (2.0 * s3 * r3) - (2.0 * s2 * r2) in

  let w12 = v12 / v11
  and w13 = v13 / v11
  and w14 = v14 / v11 in

  let w22 = v22 / v21 - w12
  and w23 = v23 / v21 - w13
  and w24 = v24 / v21 - w14 in

  let p = -. w23 / w22
  and q = w24 / w22 in
  let m = -. w12 * p - w13
  and n = w14 - w12 * q in

  let a = n*n + q*q - 1.0
  and b = 2.0*m*n - 2.0*n*x1 + 2.0*p*q - 2.0*q*y1 + 2.0*s1*r1
  and c = x1*x1 + m*m - 2.0*m*x1 + p*p + y1*y1 - 2.0*p*y1 - r1*r1 in

  let d = b * b - 4.0 * a * c in
  let rs = (-. b - (sqrt d)) / (2.0 * a) in

  let xs = m + n * rs
  and ys = p + q * rs in

  (new_circle xs ys rs)

let () =
  let c1 = new_circle 0.0 0.0 1.0
  and c2 = new_circle 4.0 0.0 1.0
  and c3 = new_circle 2.0 4.0 2.0 in

  let r1 = solve_apollonius c1 c2 c3 1.0 1.0 1.0 in
  print_circle r1;

  let r2 = solve_apollonius c1 c2 c3 (-1.) (-1.) (-1.) in
  print_circle r2;
;;
```



## Perl

Using the module <code>Math::Cartesian::Product</code> to generate the values to allow iteration through all solutions.
```perl
use utf8;
use Math::Cartesian::Product;

package Circle;

sub new {
    my ($class, $args) = @_;
    my $self = {
        x => $args->{x},
        y => $args->{y},
        r => $args->{r},
    };
    bless $self, $class;
}

sub show {
    my ($self, $args) = @_;
    sprintf "x =%7.3f  y =%7.3f  r =%7.3f\n", $args->{x}, $args->{y}, $args->{r};
}

package main;

sub circle {
    my($x,$y,$r) = @_;
    Circle->new({ x => $x, y=> $y, r => $r });
}

sub solve_Apollonius {
    my($c1, $c2, $c3, $s1, $s2, $s3) = @_;

    my $𝑣11 = 2 * $c2->{x} - 2 * $c1->{x};
    my $𝑣12 = 2 * $c2->{y} - 2 * $c1->{y};
    my $𝑣13 = $c1->{x}**2 - $c2->{x}**2 + $c1->{y}**2 - $c2->{y}**2 - $c1->{r}**2 + $c2->{r}**2;
    my $𝑣14 = 2 * $s2 * $c2->{r} - 2 * $s1 * $c1->{r};

    my $𝑣21 = 2 * $c3->{x} - 2 * $c2->{x};
    my $𝑣22 = 2 * $c3->{y} - 2 * $c2->{y};
    my $𝑣23 = $c2->{x}**2 - $c3->{x}**2 + $c2->{y}**2 - $c3->{y}**2 - $c2->{r}**2 + $c3->{r}**2;
    my $𝑣24 = 2 * $s3 * $c3->{r} - 2 * $s2 * $c2->{r};

    my $𝑤12 = $𝑣12 / $𝑣11;
    my $𝑤13 = $𝑣13 / $𝑣11;
    my $𝑤14 = $𝑣14 / $𝑣11;

    my $𝑤22 = $𝑣22 / $𝑣21 - $𝑤12;
    my $𝑤23 = $𝑣23 / $𝑣21 - $𝑤13;
    my $𝑤24 = $𝑣24 / $𝑣21 - $𝑤14;

    my $𝑃 = -$𝑤23 / $𝑤22;
    my $𝑄 = $𝑤24 / $𝑤22;
    my $𝑀 = -$𝑤12 * $𝑃 - $𝑤13;
    my $𝑁 = $𝑤14 - $𝑤12 * $𝑄;

    my $𝑎 = $𝑁**2 + $𝑄**2 - 1;
    my $𝑏 = 2 * $𝑀 * $𝑁 - 2 * $𝑁 * $c1->{x} + 2 * $𝑃 * $𝑄 - 2 * $𝑄 * $c1->{y} + 2 * $s1 * $c1->{r};
    my $𝑐 = $c1->{x}**2 + $𝑀**2 - 2 * $𝑀 * $c1->{x} + $𝑃**2 + $c1->{y}**2 - 2 * $𝑃 * $c1->{y} - $c1->{r}**2;

    my $𝐷 = $𝑏**2 - 4 * $𝑎 * $𝑐;
    my $rs = (-$𝑏 - sqrt $𝐷) / (2 * $𝑎);

    my $xs = $𝑀 + $𝑁 * $rs;
    my $ys = $𝑃 + $𝑄 * $rs;

    circle($xs, $ys, $rs);
}

$c1 = circle(0, 0, 1);
$c2 = circle(4, 0, 1);
$c3 = circle(2, 4, 2);

for (cartesian {@_} ([-1,1])x3) {
    print Circle->show( solve_Apollonius $c1, $c2, $c3, @$_);
}
```

```txt
x =  2.000  y =  0.833  r =  1.167
x =  2.000  y =  3.214  r =  2.786
x =  3.002  y =  0.123  r =  2.005
x =  4.127  y =  3.252  r =  4.255
x =  0.998  y =  0.123  r =  2.005
x = -0.127  y =  3.252  r =  4.255
x =  2.000  y = -1.500  r =  3.500
x =  2.000  y =  2.100  r =  3.900
```



## Perl 6

This program is written mostly in the "sigilless" style for several reasons.  First, sigils tend to imply variables, and these sigilless symbols are not variables, but readonly bindings to values that are calculated only once, so leaving off the sigil emphasizes the fact that they are not variables, but merely named intermediate results.

Second, it looks more like the original mathematical formulas to do it this way.

Third, together with the use of Unicode, we are emphasizing the social contract between the writer and the reader, which has a clause in it that indicates code is read much more often than it is written, therefore the writer is obligated to undergo vicarious suffering on behalf of the reader to make things clear.  If the reader doesn't understand, it's the writer's fault, in other words.  Or in other other words, figure out how to type those Unicode characters, even if it's hard.  And you should type them whenever it makes things clearer to the reader.

Finally, writing in an [https://en.wikipedia.org/wiki/Static_single_assignment_form SSA style] tends to help the optimizer.


```perl6
class Circle {
   has $.x;
   has $.y;
   has $.r;
   method gist { sprintf "%s =%7.3f " xx 3, (:$!x,:$!y,:$!r)».kv }
}

sub circle($x,$y,$r) { Circle.new: :$x, :$y, :$r }

sub solve-Apollonius([\c1, \c2, \c3], [\s1, \s2, \s3]) {
    my \𝑣11 = 2 * c2.x - 2 * c1.x;
    my \𝑣12 = 2 * c2.y - 2 * c1.y;
    my \𝑣13 = c1.x² - c2.x² + c1.y² - c2.y² - c1.r² + c2.r²;
    my \𝑣14 = 2 * s2 * c2.r - 2 * s1 * c1.r;

    my \𝑣21 = 2 * c3.x - 2 * c2.x;
    my \𝑣22 = 2 * c3.y - 2 * c2.y;
    my \𝑣23 = c2.x² - c3.x² + c2.y² - c3.y² - c2.r² + c3.r²;
    my \𝑣24 = 2 * s3 * c3.r - 2 * s2 * c2.r;

    my \𝑤12 = 𝑣12 / 𝑣11;
    my \𝑤13 = 𝑣13 / 𝑣11;
    my \𝑤14 = 𝑣14 / 𝑣11;

    my \𝑤22 = 𝑣22 / 𝑣21 - 𝑤12;
    my \𝑤23 = 𝑣23 / 𝑣21 - 𝑤13;
    my \𝑤24 = 𝑣24 / 𝑣21 - 𝑤14;

    my \𝑃 = -𝑤23 / 𝑤22;
    my \𝑄 = 𝑤24 / 𝑤22;
    my \𝑀 = -𝑤12 * 𝑃 - 𝑤13;
    my \𝑁 = 𝑤14 - 𝑤12 * 𝑄;

    my \𝑎 = 𝑁² + 𝑄² - 1;
    my \𝑏 = 2 * 𝑀 * 𝑁 - 2 * 𝑁 * c1.x + 2 * 𝑃 * 𝑄 - 2 * 𝑄 * c1.y + 2 * s1 * c1.r;
    my \𝑐 = c1.x² + 𝑀² - 2 * 𝑀 * c1.x + 𝑃² + c1.y² - 2 * 𝑃 * c1.y - c1.r²;

    my \𝐷 = 𝑏² - 4 * 𝑎 * 𝑐;
    my \rs = (-𝑏 - sqrt 𝐷) / (2 * 𝑎);

    my \xs = 𝑀 + 𝑁 * rs;
    my \ys = 𝑃 + 𝑄 * rs;

    circle(xs, ys, rs);
}

my @c = circle(0, 0, 1), circle(4, 0, 1), circle(2, 4, 2);
for ([X] [-1,1] xx 3) -> @i {
    say (solve-Apollonius @c, @i).gist;
}
```

```txt
x =  2.000  y =  0.833  r =  1.167
x =  2.000  y =  3.214  r =  2.786
x =  3.002  y =  0.123  r =  2.005
x =  4.127  y =  3.252  r =  4.255
x =  0.998  y =  0.123  r =  2.005
x = -0.127  y =  3.252  r =  4.255
x =  2.000  y = -1.500  r =  3.500
x =  2.000  y =  2.100  r =  3.900
```



## Phix


```Phix
function Apollonius(sequence calc, circles)

    integer {s1,s2,s3} = calc

    atom {x1,y1,r1} = circles[1],
         {x2,y2,r2} = circles[2],
         {x3,y3,r3} = circles[3],

         v11 = 2*x2 - 2*x1,
         v12 = 2*y2 - 2*y1,
         v13 = x1*x1 - x2*x2 + y1*y1 - y2*y2 - r1*r1 + r2*r2,
         v14 = 2*s2*r2 - 2*s1*r1,

         v21 = 2*x3 - 2*x2,
         v22 = 2*y3 - 2*y2,
         v23 = x2*x2 - x3*x3 + y2*y2 - y3*y3 - r2*r2 + r3*r3,
         v24 = 2*s3*r3 - 2*s2*r2,

         w12 = v12 / v11,
         w13 = v13 / v11,
         w14 = v14 / v11,

         w22 = v22 / v21 - w12,
         w23 = v23 / v21 - w13,
         w24 = v24 / v21 - w14,

         P = -w23 / w22,
         Q =  w24 / w22,
         M = -w12*P - w13,
         N =  w14 - w12*Q,

         a = N*N + Q*Q - 1,
         b = 2*M*N - 2*N*x1 + 2*P*Q - 2*Q*y1 + 2*s1*r1,
         c = x1*x1 + M*M - 2*M*x1 + P*P + y1*y1 - 2*P*y1 - r1*r1,

         d = b*b - 4*a*c,

         rs = (-b-sqrt(d)) / (2*a),

         xs = M + N*rs,
         ys = P + Q*rs

    return {xs,ys,rs}
end function

constant circles = {{0,0,1},
                    {4,0,1},
                    {2,4,2}}

-- +1: externally tangental, -1: internally tangental
constant calcs = {{+1,+1,+1},
                  {-1,-1,-1},
                  {+1,-1,-1},
                  {-1,+1,-1},
                  {-1,-1,+1},
                  {+1,+1,-1},
                  {-1,+1,+1},
                  {+1,-1,+1}}
for i=1 to 8 do
    atom {xs,ys,rs} = Apollonius(calcs[i],circles)
    string th = {"st (external)","nd (internal)","rd","th"}[min(i,4)]
    printf(1,"%d%s solution: x=%+f, y=%+f, r=%f\n",{i,th,xs,ys,rs})
end for
```

```txt

1st (external) solution: x=+2.000000, y=+2.100000, r=3.900000
2nd (internal) solution: x=+2.000000, y=+0.833333, r=1.166667
3rd solution: x=+0.997502, y=+0.122502, r=2.004996
4th solution: x=+3.002498, y=+0.122502, r=2.004996
5th solution: x=+2.000000, y=+3.214286, r=2.785714
6th solution: x=+2.000000, y=-1.500000, r=3.500000
7th solution: x=+4.127498, y=+3.252498, r=4.254996
8th solution: x=-0.127498, y=+3.252498, r=4.254996

```



## PL/I


```PL/I
Apollonius: procedure options (main); /* 29 October 2013 */

   define structure
    1 circle,
      2 x float (15),
      2 y float (15),
      2 radius float (15);

   declare (c1 , c2, c3, result) type (circle);

   c1.x = 0; c1.y = 0; c1.radius = 1;
   c2.x = 4; c2.y = 0; c2.radius = 1;
   c3.x = 2; c3.y = 4; c3.radius = 2;

   result = Solve_Apollonius(c1, c2, c3,  1,  1,  1);
   put skip edit ('External tangent:', result.x, result.y, result.radius) (a, 3 f(12,8));

   result = Solve_Apollonius(c1, c2, c3, -1, -1, -1);
   put skip edit ('Internal tangent:', result.x, result.y, result.radius) (a, 3 f(12,8));



Solve_Apollonius: procedure (c1, c2, c3, s1, s2, s3) returns(type(circle));
   declare (c1, c2, c3) type(circle);
   declare res type (circle);
   declare (s1, s2, s3) fixed binary;

   declare (
             v11, v12, v13, v14,
             v21, v22, v23, v24,
             w12, w13, w14,
             w22, w23, w24,
             p, q, m, n, a, b, c, det) float (15);

   v11 = 2*c2.x - 2*c1.x;
   v12 = 2*c2.y - 2*c1.y;
   v13 = c1.x**2 - c2.x**2 + c1.y**2 - c2.y**2 - c1.radius**2 + c2.radius**2;
   v14 = 2*s2*c2.radius - 2*s1*c1.radius;

   v21 = 2*c3.x - 2*c2.x;
   v22 = 2*c3.y - 2*c2.y;
   v23 = c2.x**2 - c3.x**2 + c2.y**2 - c3.y**2 - c2.radius**2 + c3.radius**2;
   v24 = 2*s3*c3.radius - 2*s2*c2.radius;

   w12 = v12/v11;
   w13 = v13/v11;
   w14 = v14/v11;

   w22 = v22/v21-w12;
   w23 = v23/v21-w13;
   w24 = v24/v21-w14;

   p = -w23/w22;
   q = w24/w22;
   m = -w12*P - w13;
   n = w14 - w12*q;

   a = n*n + q*q - 1;
   b = 2*m*n - 2*n*c1.x + 2*p*q - 2*q*c1.y + 2*s1*c1.radius;
   c = c1.x**2 + m*m - 2*m*c1.x + p*p + c1.y**2 - 2*p*c1.y - c1.radius**2;

   det = b*b - 4*a*c;
   res.radius = (-b-sqrt(det)) / (2*a);
   res.x = m + n*res.radius;
   res.y = p + q*res.radius;

   return (res);
end Solve_Apollonius;
end Apollonius;
```

Results:

```txt

External tangent:  2.00000000  2.10000000  3.90000000
Internal tangent:  2.00000000  0.83333333  1.16666667
```



## PowerShell

```PowerShell

function Measure-Apollonius
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [int]$Counter,
        [double]$x1,
        [double]$y1,
        [double]$r1,
        [double]$x2,
        [double]$y2,
        [double]$r2,
        [double]$x3,
        [double]$y3,
        [double]$r3
    )

    switch ($Counter)
    {
        {$_ -eq 2} {$s1 = -1; $s2 = -1; $s3 = -1; break}
        {$_ -eq 3} {$s1 =  1; $s2 = -1; $s3 = -1; break}
        {$_ -eq 4} {$s1 = -1; $s2 =  1; $s3 = -1; break}
        {$_ -eq 5} {$s1 = -1; $s2 = -1; $s3 =  1; break}
        {$_ -eq 6} {$s1 =  1; $s2 =  1; $s3 = -1; break}
        {$_ -eq 7} {$s1 = -1; $s2 =  1; $s3 =  1; break}
        {$_ -eq 8} {$s1 =  1; $s2 = -1; $s3 =  1; break}
        Default    {$s1 =  1; $s2 =  1; $s3 =  1; break}
    }

    [double]$v11 = 2 * $x2 - 2 * $x1
    [double]$v12 = 2 * $y2 - 2 * $y1
    [double]$v13 = $x1 * $x1 - $x2 * $x2 + $y1 * $y1 - $y2 * $y2 - $r1 * $r1 + $r2 * $r2
    [double]$v14 = 2 * $s2 * $r2 - 2 * $s1 * $r1

    [double]$v21 = 2 * $x3 - 2 * $x2
    [double]$v22 = 2 * $y3 - 2 * $y2
    [double]$v23 = $x2 * $x2 - $x3 * $x3 + $y2 * $y2 - $y3 * $y3 - $r2 * $r2 + $r3 * $r3
    [double]$v24 = 2 * $s3 * $r3 - 2 * $s2 * $r2

    [double]$w12 = $v12 / $v11
    [double]$w13 = $v13 / $v11
    [double]$w14 = $v14 / $v11

    [double]$w22 = $v22 / $v21 - $w12
    [double]$w23 = $v23 / $v21 - $w13
    [double]$w24 = $v24 / $v21 - $w14

    [double]$P = -$w23 / $w22
    [double]$Q = $w24 / $w22
    [double]$M = -$w12 * $P - $w13
    [double]$N = $w14 - $w12 * $Q

    [double]$a = $N * $N + $Q * $Q - 1
    [double]$b = 2 * $M * $N - 2 * $N * $x1 + 2 * $P * $Q - 2 * $Q * $y1 + 2 * $s1 * $r1
    [double]$c = $x1 * $x1 + $M * $M - 2 * $M * $x1 + $P * $P + $y1 * $y1 - 2 * $P * $y1 - $r1 * $r1

    [double]$D = $b * $b - 4 * $a * $c

    [double]$rs = (-$b - [Double]::Parse([Math]::Sqrt($D).ToString())) / (2 * [Double]::Parse($a.ToString()))
    [double]$xs = $M + $N * $rs
    [double]$ys = $P + $Q * $rs

    [PSCustomObject]@{
        X      = $xs
        Y      = $ys
        Radius = $rs
    }
}

```


```PowerShell

for ($i = 1; $i -le 8; $i++)
{
    Measure-Apollonius -Counter $i -x1 0 -y1 0 -r1 1 -x2 4 -y2 0 -r2 1 -x3 2 -y3 4 -r3 2
}

```

```txt

                 X                 Y           Radius
                 -                 -           ------
                 2               2.1              3.9
                 2 0.833333333333333 1.16666666666667
 0.997501996806385 0.122501996806385 2.00499600638723
  3.00249800319362 0.122501996806385 2.00499600638723
                 2  3.21428571428571 2.78571428571429
                 2              -1.5              3.5
  4.12749800319362  3.25249800319362 4.25499600638723
-0.127498003193615  3.25249800319362 4.25499600638723

```



## PureBasic

```PureBasic
Structure Circle
  XPos.f
  YPos.f
  Radius.f
EndStructure

Procedure ApolloniusSolver(*c1.Circle,*c2.Circle,*c3.Circle, s1, s2, s3)
  Define.f  ; This tells the compiler that all non-specified new variables
            ; should be of float type (.f).
  x1=*c1\XPos:  y1=*c1\YPos:  r1=*c1\Radius
  x2=*c2\XPos:  y2=*c2\YPos:  r2=*c2\Radius
  x3=*c3\XPos:  y3=*c3\YPos:  r3=*c3\Radius

  v11 = 2*x2 - 2*x1
  v12 = 2*y2 - 2*y1
  v13 = x1*x1 - x2*x2 + y1*y1 - y2*y2 - r1*r1 + r2*r2
  v14 = 2*s2*r2 - 2*s1*r1

  v21 = 2*x3 - 2*x2
  v22 = 2*y3 - 2*y2
  v23 = x2*x2 - x3*x3 + y2*y2 - y3*y3 - r2*r2 + r3*r3
  v24 = 2*s3*r3 - 2*s2*r2

  w12 = v12/v11
  w13 = v13/v11
  w14 = v14/v11

  w22 = v22/v21-w12
  w23 = v23/v21-w13
  w24 = v24/v21-w14

  P = -w23/w22
  Q =  w24/w22
  M = -w12*P-w13
  N =  w14-w12*Q

  a = N*N + Q*Q - 1
  b = 2*M*N - 2*N*x1 + 2*P*Q - 2*Q*y1 + 2*s1*r1
  c = x1*x1 + M*M - 2*M*x1 + P*P + y1*y1 - 2*P*y1 - r1*r1

  D= b*b - 4*a*c

  Define *result.Circle=AllocateMemory(SizeOf(Circle))
  ; Allocate memory for a returned Structure of type Circle.
  ; This memory should be freed later but if not, PureBasic’s
  ; internal framework will do so when the program shuts down.
  If *result
    *result\Radius=(-b-Sqr(D))/(2*a)
    *result\XPos  =M+N * *result\Radius
    *result\YPos  =P+Q * *result\Radius
  EndIf
  ProcedureReturn *result ; Sending back a pointer
EndProcedure

If OpenConsole()
  Define.Circle c1, c2, c3
  Define *c.Circle  ; '*c' is defined as a pointer to a circle-structure.
  c1\Radius=1
  c2\XPos=4:  c2\Radius=1
  c3\XPos=2:  c3\YPos=4:  c3\Radius=2

  *c=ApolloniusSolver(@c1, @c2, @c3, 1, 1, 1)
  If *c ; Verify that *c got allocated
    PrintN("Circle [x="+StrF(*c\XPos,2)+", y="+StrF(*c\YPos,2)+", r="+StrF(*c\Radius,2)+"]")
    FreeMemory(*c)  ; We are done with *c for the first calculation
  EndIf

  *c=ApolloniusSolver(@c1, @c2, @c3,-1,-1,-1)
  If *c
    PrintN("Circle [x="+StrF(*c\XPos,2)+", y="+StrF(*c\YPos,2)+", r="+StrF(*c\Radius,2)+"]")
    FreeMemory(*c)
  EndIf
  Print("Press ENTER to exit"): Input()
EndIf
```


```txt
Circle [x=2.00, y=2.10, r=3.90]
Circle [x=2.00, y=0.83, r=1.17]
Press ENTER to exit
```



## Python

{{trans|Java}}. Although a Circle class is defined, the solveApollonius function is defined in such a way that any three valued tuple or list could be used instead of c1, c2, and c3. The function calls near the end use instances of the Circle class, whereas the docstring shows how the same can be achieved using simple tuples. (And also serves as a simple [[wp:Doctest|doctest]])

```python

from collections import namedtuple
import math

Circle = namedtuple('Circle', 'x, y, r')

def solveApollonius(c1, c2, c3, s1, s2, s3):
    '''
    >>> solveApollonius((0, 0, 1), (4, 0, 1), (2, 4, 2), 1,1,1)
    Circle(x=2.0, y=2.1, r=3.9)
    >>> solveApollonius((0, 0, 1), (4, 0, 1), (2, 4, 2), -1,-1,-1)
    Circle(x=2.0, y=0.8333333333333333, r=1.1666666666666667)
    '''
    x1, y1, r1 = c1
    x2, y2, r2 = c2
    x3, y3, r3 = c3

    v11 = 2*x2 - 2*x1
    v12 = 2*y2 - 2*y1
    v13 = x1*x1 - x2*x2 + y1*y1 - y2*y2 - r1*r1 + r2*r2
    v14 = 2*s2*r2 - 2*s1*r1

    v21 = 2*x3 - 2*x2
    v22 = 2*y3 - 2*y2
    v23 = x2*x2 - x3*x3 + y2*y2 - y3*y3 - r2*r2 + r3*r3
    v24 = 2*s3*r3 - 2*s2*r2

    w12 = v12/v11
    w13 = v13/v11
    w14 = v14/v11

    w22 = v22/v21-w12
    w23 = v23/v21-w13
    w24 = v24/v21-w14

    P = -w23/w22
    Q = w24/w22
    M = -w12*P-w13
    N = w14 - w12*Q

    a = N*N + Q*Q - 1
    b = 2*M*N - 2*N*x1 + 2*P*Q - 2*Q*y1 + 2*s1*r1
    c = x1*x1 + M*M - 2*M*x1 + P*P + y1*y1 - 2*P*y1 - r1*r1

    # Find a root of a quadratic equation. This requires the circle centers not to be e.g. colinear
    D = b*b-4*a*c
    rs = (-b-math.sqrt(D))/(2*a)

    xs = M+N*rs
    ys = P+Q*rs

    return Circle(xs, ys, rs)

if __name__ == '__main__':
    c1, c2, c3 = Circle(0, 0, 1), Circle(4, 0, 1), Circle(2, 4, 2)
    print(solveApollonius(c1, c2, c3, 1, 1, 1))    #Expects "Circle[x=2.00,y=2.10,r=3.90]" (green circle in image)
    print(solveApollonius(c1, c2, c3, -1, -1, -1)) #Expects "Circle[x=2.00,y=0.83,r=1.17]" (red circle in image)
```

'''Sample Output'''

```txt
Circle(x=2.0, y=2.1, r=3.9)
Circle(x=2.0, y=0.8333333333333333, r=1.1666666666666667)
```



## Racket

```Racket

#lang slideshow

(struct circle (x y r) #:prefab)

(define (apollonius c1 c2 c3 s1 s2 s3)
  (define x1 (circle-x c1))
  (define y1 (circle-y c1))
  (define r1 (circle-r c1))
  (define x2 (circle-x c2))
  (define y2 (circle-y c2))
  (define r2 (circle-r c2))
  (define x3 (circle-x c3))
  (define y3 (circle-y c3))
  (define r3 (circle-r c3))

  (define v11 (- (* 2 x2) (* 2 x1)))
  (define v12 (- (* 2 y2) (* 2 y1)))
  (define v13 (+ (- (* x1 x1) (* x2 x2))
                 (- (* y1 y1) (* y2 y2))
                 (- (* r2 r2) (* r1 r1))))
  (define v14 (- (* 2 s2 r2) (* 2 s1 r1)))

  (define v21 (- (* 2 x3) (* 2 x2)))
  (define v22 (- (* 2 y3) (* 2 y2)))
  (define v23 (+ (- (* x2 x2) (* x3 x3))
                 (- (* y2 y2) (* y3 y3))
                 (- (* r3 r3) (* r2 r2))))
  (define v24 (- (* 2 s3 r3) (* 2 s2 r2)))

  (define w12 (/ v12 v11))
  (define w13 (/ v13 v11))
  (define w14 (/ v14 v11))

  (define w22 (- (/ v22 v21) w12))
  (define w23 (- (/ v23 v21) w13))
  (define w24 (- (/ v24 v21) w14))

  (define P (- (/ w23 w22)))
  (define Q (/ w24 w22))
  (define M (- (+ (* w12 P) w13)))
  (define N (- w14 (* w12 Q)))

  (define a (+ (* N N) (* Q Q) -1))
  (define b (+ (- (* 2 M N) (* 2 N x1))
               (- (* 2 P Q) (* 2 Q y1))
               (* 2 s1 r1)))
  (define c (- (+ (* x1 x1) (* M M) (* P P) (* y1 y1))
               (+ (* 2 M x1) (* 2 P y1) (* r1 r1))))

  (define D (- (* b b) (* 4 a c)))
  (define rs (/ (- (+ b (sqrt D))) (* 2 a)))
  (define xs (+ M (* N rs)))
  (define ys (+ P (* Q rs)))
  (circle xs ys rs))

(define c1 (circle 0.0 0.0 1.0))
(define c2 (circle 4.0 0.0 1.0))
(define c3 (circle 2.0 4.0 2.0))

;; print solutions
(apollonius c1 c2 c3 1.0 1.0 1.0)
(apollonius c1 c2 c3 -1.0 -1.0 -1.0)

;; visualize solutions
(require racket/gui/base)
(define (show-circles . circles+colors)
  (define f (new frame% [label "Apollonius"] [width 300] [height 300]))
  (define c
    (new canvas% [parent f]
      [paint-callback
       (lambda (canvas dc)
         (send* dc (set-origin 100 100)
                   (set-scale 20 20)
                   (set-pen "black" 1/10 'solid)
                   (set-brush "white" 'transparent))
         (for ([x circles+colors])
           (if (string? x)
             (send dc set-pen x 1/5 'solid)
             (let ([x (circle-x x)] [y (circle-y x)] [r (circle-r x)])
               (send dc draw-ellipse (- x r) (- y r) (* 2 r) (* 2 r))))))]))
  (send f show #t))
(show-circles "black" c1 c2 c3
              "green" (apollonius c1 c2 c3 1.0 1.0 1.0)
              "red"   (apollonius c1 c2 c3 -1.0 -1.0 -1.0))

```



## REXX

Programming note:   REXX has no   '''sqrt'''   (square root) function, so a RYO version is included here.

```rexx
/*REXX program solves the problem of Apollonius, named after the Greek Apollonius of    */
/*────────────────────────────────────── Perga [Pergæus]   (circa 262 BCE ──► 190 BCE). */
numeric digits 15;        x1= 0;       y1= 0;        r1= 1
                          x2= 4;       y2= 0;        r2= 1
                          x3= 2;       y3= 4;        r3= 2
call tell  'external tangent:   ',     Apollonius( 1,  1,  1)
call tell  'internal tangent:   ',     Apollonius(-1, -1, -1)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Apollonius: parse arg s1,s2,s3                   /*could be internal or external tangent*/
      numeric digits digits() * 3                /*reduce rounding with thrice digits.  */
      va= x2*2     - x1*2;                        vb= y2*2 - y1*2
      vc= x1**2    - x2**2 + y1**2 - y2**2 - r1**2 + r2**2
      vd= s2*r2*2  - s1*r1*2;                     ve= x3*2 - x2*2;   vf= y3*2    - y2*2
      vg= x2**2    - x3**2 + y2**2 - y3**2 - r2**2 + r3**2;          vh= s3*r3*2 - s2*r2*2
      vj= vb/va;               vk= vc/va;         vm=  vd/va;        vn= vf/ve   - vj
      vp= vg/ve    - vk;       vr= vh/ve - vm;    p = -vp/vn;        q = vr/vn
      m =  -vj*p   - vk;       n = vm - vj*q
      a =  n**2 + q**2   - 1
      b =  (m*n    - n*x1    + p*q    - q*y1   + s1*r1)  * 2
      c =  x1**2   + y1**2   + m**2   - r1**2  + p**2    - (m*x1 + p*y1)  * 2
                                         $r= (-b - sqrt(b**2 - a*c*4) ) / (a+a)
      return  (m + n*$r)   (p + q*$r)   ($r)                       /*return 3 arguments.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x;  if x=0  then return 0;  d=digits();  h=d+6;  numeric digits
      m.=9; numeric form; parse value format(x,2,1,,0) 'E0'  with  g 'E' _ .; g=g*.5'e'_%2
                  do j=0  while h>9;      m.j=h;               h=h%2 + 1;       end  /*j*/
                  do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g) * .5;  end  /*k*/
      return g
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell: parse arg _,a b c; w=digits()+4; say _ left(a/1,w%2) left(b/1,w) left(c/1,w); return
```

Programming note:   in REXX, dividing by unity normalizes the number.

```txt

external tangent:    2         2.1                 3.9
internal tangent:    2         0.833333333333333   1.16666666666667

```



## Ruby

```ruby
class Circle
  def initialize(x, y, r)
    @x, @y, @r = [x, y, r].map(&:to_f)
  end
  attr_reader :x, :y, :r

  def self.apollonius(c1, c2, c3, s1=1, s2=1, s3=1)
    x1, y1, r1 = c1.x, c1.y, c1.r
    x2, y2, r2 = c2.x, c2.y, c2.r
    x3, y3, r3 = c3.x, c3.y, c3.r

    v11 = 2*x2 - 2*x1
    v12 = 2*y2 - 2*y1
    v13 = x1**2 - x2**2 + y1**2 - y2**2 - r1**2 + r2**2
    v14 = 2*s2*r2 - 2*s1*r1

    v21 = 2*x3 - 2*x2
    v22 = 2*y3 - 2*y2
    v23 = x2**2 - x3**2 + y2**2 - y3**2 - r2**2 + r3**2
    v24 = 2*s3*r3 - 2*s2*r2

    w12 = v12/v11
    w13 = v13/v11
    w14 = v14/v11

    w22 = v22/v21 - w12
    w23 = v23/v21 - w13
    w24 = v24/v21 - w14

    p = -w23/w22
    q = w24/w22
    m = -w12*p - w13
    n = w14 - w12*q

    a = n**2 + q**2 - 1
    b = 2*m*n - 2*n*x1 + 2*p*q - 2*q*y1 + 2*s1*r1
    c = x1**2 + m**2 - 2*m*x1 + p**2 + y1**2 - 2*p*y1 - r1**2

    d = b**2 - 4*a*c
    rs = (-b - Math.sqrt(d)) / (2*a)
    xs = m + n*rs
    ys = p + q*rs

    self.new(xs, ys, rs)
  end

  def to_s
    "Circle: x=#{@x}, y=#{@y}, r=#{@r}"
  end
end

puts c1 = Circle.new(0, 0, 1)
puts c2 = Circle.new(2, 4, 2)
puts c3 = Circle.new(4, 0, 1)

puts Circle.apollonius(c1, c2, c3)
puts Circle.apollonius(c1, c2, c3, -1, -1, -1)
```


```txt

Circle: x=0.0, y=0.0, r=1.0
Circle: x=2.0, y=4.0, r=2.0
Circle: x=4.0, y=0.0, r=1.0
Circle: x=2.0, y=2.1, r=3.9
Circle: x=2.0, y=0.8333333333333333, r=1.1666666666666667

```



## Scala


```scala
object ApolloniusSolver extends App {
  case class Circle(x: Double, y: Double, r: Double)
  object Tangent extends Enumeration {
	type Tangent = Value
	val intern = Value(-1)
	val extern = Value(1)
  }

  import Tangent._
  import scala.Math._

  val solveApollonius: (Circle, Circle, Circle, Triple[Tangent, Tangent, Tangent]) => Circle = (c1, c2, c3, tangents) => {
    val fv: (Circle, Circle, Int, Int) => Tuple4[Double, Double, Double, Double] = (c1, c2, s1, s2) => {
      val v11 = 2 * c2.x - 2 * c1.x
      val v12 = 2 * c2.y - 2 * c1.y
      val v13 = pow(c1.x, 2) - pow(c2.x, 2) + pow(c1.y, 2) - pow(c2.y, 2) - pow(c1.r, 2) + pow(c2.r, 2)
      val v14 = 2 * s2 * c2.r - 2 * s1 * c1.r
      Tuple4(v11, v12, v13, v14)
    }
    val (s1, s2, s3) = (tangents._1.id, tangents._2.id, tangents._3.id)

    val (v11, v12, v13, v14) = fv(c1, c2, s1, s2)
    val (v21, v22, v23, v24) = fv(c2, c3, s2, s3)

    val w12 = v12 / v11
    val w13 = v13 / v11
    val w14 = v14 / v11

    val w22 = v22 / v21 - w12
    val w23 = v23 / v21 - w13
    val w24 = v24 / v21 - w14

    val P = -w23 / w22
    val Q =  w24 / w22
    val M = -w12 * P - w13
    val N =  w14 - w12 * Q

    val a = N*N + Q*Q - 1
    val b = 2*M*N - 2*N*c1.x +
            2*P*Q - 2*Q*c1.y +
            2*s1*c1.r
    val c = pow(c1.x, 2) + M*M - 2*M*c1.x +
            P*P + pow(c1.y, 2) - 2*P*c1.y - pow(c1.r, 2)

    // Find a root of a quadratic equation. This requires the circle centers not to be e.g. colinear
    val D = b*b - 4*a*c
    val rs = (-b - sqrt(D)) / (2*a)

    Circle(x=M + N*rs, y=P + Q*rs, r=rs)
  }

  val c1 = Circle(x=0.0, y=0.0, r=1.0)
  val c2 = Circle(x=4.0, y=0.0, r=1.0)
  val c3 = Circle(x=2.0, y=4.0, r=2.0)

  println("c1: "+c1)
  println("c2: "+c2)
  println("c3: "+c3)

  println{
    val tangents = Triple(intern, intern, intern)
    "red circle:   tangents="+tangents+" cs=" + solveApollonius(c1, c2, c3, tangents)
  }
  println{
    val tangents = Triple(extern, extern, extern)
    "green circle: tangents="+tangents+" cs=" + solveApollonius(c1, c2, c3, tangents)
  }

  println("all combinations:")
  for ( ti <- Tangent.values)
    for ( tj <- Tangent.values)
      for ( tk <- Tangent.values) {
        println{
          val format: Circle => String = c => {
            "Circle(x=%8.5f, y=%8.5f, r=%8.5f)".format(c.x, c.y, c.r)
          }
          val tangents = Triple(ti, tj, tk)
          "tangents: " + tangents + " -> cs=" + format(solveApollonius(c1, c2, c3, tangents))
        }
      }
}
```

Output:

```txt

c1: Circle(0.0,0.0,1.0)
c2: Circle(4.0,0.0,1.0)
c3: Circle(2.0,4.0,2.0)
red circle:   tangents=(intern,intern,intern) cs=Circle(2.0,0.8333333333333333,1.1666666666666667)
green circle: tangents=(extern,extern,extern) cs=Circle(2.0,2.1,3.9)
all combinations:
tangents: (intern,intern,intern) -> cs=Circle(x= 2,00000, y= 0,83333, r= 1,16667)
tangents: (intern,intern,extern) -> cs=Circle(x= 2,00000, y= 3,21429, r= 2,78571)
tangents: (intern,extern,intern) -> cs=Circle(x= 3,00250, y= 0,12250, r= 2,00500)
tangents: (intern,extern,extern) -> cs=Circle(x= 4,12750, y= 3,25250, r= 4,25500)
tangents: (extern,intern,intern) -> cs=Circle(x= 0,99750, y= 0,12250, r= 2,00500)
tangents: (extern,intern,extern) -> cs=Circle(x=-0,12750, y= 3,25250, r= 4,25500)
tangents: (extern,extern,intern) -> cs=Circle(x= 2,00000, y=-1,50000, r= 3,50000)
tangents: (extern,extern,extern) -> cs=Circle(x= 2,00000, y= 2,10000, r= 3,90000)

```



## Sidef

```ruby
class Circle(x,y,r) {
    method to_s { "Circle(#{x}, #{y}, #{r})" }
}

func solve_apollonius(c, s) {

    var(c1, c2, c3) = c...;
    var(s1, s2, s3) = s...;

    var 𝑣11 = (2*c2.x - 2*c1.x);
    var 𝑣12 = (2*c2.y - 2*c1.y);
    var 𝑣13 = (c1.x**2 - c2.x**2 + c1.y**2 - c2.y**2 - c1.r**2 + c2.r**2);
    var 𝑣14 = (2*s2*c2.r - 2*s1*c1.r);

    var 𝑣21 = (2*c3.x - 2*c2.x);
    var 𝑣22 = (2*c3.y - 2*c2.y);
    var 𝑣23 = (c2.x**2 - c3.x**2 + c2.y**2 - c3.y**2 - c2.r**2 + c3.r**2);
    var 𝑣24 = (2*s3*c3.r - 2*s2*c2.r);

    var 𝑤12 = (𝑣12 / 𝑣11);
    var 𝑤13 = (𝑣13 / 𝑣11);
    var 𝑤14 = (𝑣14 / 𝑣11);

    var 𝑤22 = (𝑣22/𝑣21 - 𝑤12);
    var 𝑤23 = (𝑣23/𝑣21 - 𝑤13);
    var 𝑤24 = (𝑣24/𝑣21 - 𝑤14);

    var 𝑃 = (-𝑤23 / 𝑤22);
    var 𝑄 = (𝑤24 / 𝑤22);
    var 𝑀 = (-𝑤12*𝑃 - 𝑤13);
    var 𝑁 = (𝑤14 - 𝑤12*𝑄);

    var 𝑎 = (𝑁**2 + 𝑄**2 - 1);
    var 𝑏 = (2*𝑀*𝑁 - 2*𝑁*c1.x + 2*𝑃*𝑄 - 2*𝑄*c1.y + 2*s1*c1.r);
    var 𝑐 = (c1.x**2 + 𝑀**2 - 2*𝑀*c1.x + 𝑃**2 + c1.y**2 - 2*𝑃*c1.y - c1.r**2);

    var 𝐷 = (𝑏**2 - 4*𝑎*𝑐);
    var rs = ((-𝑏 - 𝐷.sqrt) / 2*𝑎);

    var xs = (𝑀 + 𝑁*rs);
    var ys = (𝑃 + 𝑄*rs);

    Circle(xs, ys, rs);
}

var c = [Circle(0, 0, 1), Circle(4, 0, 1), Circle(2, 4, 2)];
say solve_apollonius(c, %n<1 1 1>);
say solve_apollonius(c, %n<-1 -1 -1>);
```

```txt

Circle(2, 2.1, 3.9)
Circle(2, 0.83333333333333333333333333333333333333325, 1.166666666666666666666666666666666666667)

```



## Swift

```Swift
import Foundation

struct Circle {
    let center:[Double]!
    let radius:Double!

    init(center:[Double], radius:Double) {
        self.center = center
        self.radius = radius
    }

    func toString() -> String {
        return "Circle[x=\(center[0]),y=\(center[1]),r=\(radius)]"
    }
}

func solveApollonius(c1:Circle, c2:Circle, c3:Circle,
    s1:Double, s2:Double, s3:Double) -> Circle {

        let x1 = c1.center[0]
        let y1 = c1.center[1]
        let r1 = c1.radius
        let x2 = c2.center[0]
        let y2 = c2.center[1]
        let r2 = c2.radius
        let x3 = c3.center[0]
        let y3 = c3.center[1]
        let r3 = c3.radius

        let v11 = 2*x2 - 2*x1
        let v12 = 2*y2 - 2*y1
        let v13 = x1*x1 - x2*x2 + y1*y1 - y2*y2 - r1*r1 + r2*r2
        let v14 = 2*s2*r2 - 2*s1*r1

        let v21 = 2*x3 - 2*x2
        let v22 = 2*y3 - 2*y2
        let v23 = x2*x2 - x3*x3 + y2*y2 - y3*y3 - r2*r2 + r3*r3
        let v24 = 2*s3*r3 - 2*s2*r2

        let w12 = v12/v11
        let w13 = v13/v11
        let w14 = v14/v11

        let w22 = v22/v21-w12
        let w23 = v23/v21-w13
        let w24 = v24/v21-w14

        let P = -w23/w22
        let Q = w24/w22
        let M = -w12*P-w13
        let N = w14 - w12*Q

        let a = N*N + Q*Q - 1
        let b = 2*M*N - 2*N*x1 + 2*P*Q - 2*Q*y1 + 2*s1*r1
        let c = x1*x1 + M*M - 2*M*x1 + P*P + y1*y1 - 2*P*y1 - r1*r1

        let D = b*b-4*a*c

        let rs = (-b - sqrt(D)) / (2*a)
        let xs = M + N * rs
        let ys = P + Q * rs

        return  Circle(center: [xs,ys], radius: rs)

}

let c1 = Circle(center: [0,0], radius: 1)
let c2 = Circle(center: [4,0], radius: 1)
let c3 = Circle(center: [2,4], radius: 2)

println(solveApollonius(c1,c2,c3,1,1,1).toString())
println(solveApollonius(c1,c2,c3,-1,-1,-1).toString())
```

```txt
Circle[x=2.0,y=2.1,r=3.9]
Circle[x=2.0,y=0.833333333333333,r=1.16666666666667]
```



## Tcl

```tcl
package require TclOO; # Just so we can make a circle class

oo::class create circle {
    variable X Y Radius
    constructor {x y radius} {
	namespace import ::tcl::mathfunc::double
	set X [double $x]; set Y [double $y]; set Radius [double $radius]
    }
    method values {} {list $X $Y $Radius}
    method format {} {
	format "Circle\[o=(%.2f,%.2f),r=%.2f\]" $X $Y $Radius
    }
}

proc solveApollonius {c1 c2 c3 {s1 1} {s2 1} {s3 1}} {
    if {abs($s1)!=1||abs($s2)!=1||abs($s3)!=1} {
	error "wrong sign; must be 1 or -1"
    }

    lassign [$c1 values] x1 y1 r1
    lassign [$c2 values] x2 y2 r2
    lassign [$c3 values] x3 y3 r3

    set v11 [expr {2*($x2 - $x1)}]
    set v12 [expr {2*($y2 - $y1)}]
    set v13 [expr {$x1**2 - $x2**2 + $y1**2 - $y2**2 - $r1**2 + $r2**2}]
    set v14 [expr {2*($s2*$r2 - $s1*$r1)}]

    set v21 [expr {2*($x3 - $x2)}]
    set v22 [expr {2*($y3 - $y2)}]
    set v23 [expr {$x2**2 - $x3**2 + $y2**2 - $y3**2 - $r2**2 + $r3**2}]
    set v24 [expr {2*($s3*$r3 - $s2*$r2)}]

    set w12 [expr {$v12 / $v11}]
    set w13 [expr {$v13 / $v11}]
    set w14 [expr {$v14 / $v11}]

    set w22 [expr {$v22 / $v21 - $w12}]
    set w23 [expr {$v23 / $v21 - $w13}]
    set w24 [expr {$v24 / $v21 - $w14}]

    set P [expr {-$w23 / $w22}]
    set Q [expr {$w24 / $w22}]
    set M [expr {-$w12 * $P - $w13}]
    set N [expr {$w14 - $w12 * $Q}]

    set a [expr {$N**2 + $Q**2 - 1}]
    set b [expr {2*($M*$N - $N*$x1 + $P*$Q - $Q*$y1 + $s1*$r1)}]
    set c [expr {($x1-$M)**2 + ($y1-$P)**2 - $r1**2}]

    set rs [expr {(-$b - sqrt($b**2 - 4*$a*$c)) / (2*$a)}]
    set xs [expr {$M + $N*$rs}]
    set ys [expr {$P + $Q*$rs}]

    return [circle new $xs $ys $rs]
}
```

Demonstration code:

```tcl
set c1 [circle new 0 0 1]
set c2 [circle new 4 0 1]
set c3 [circle new 2 4 2]
set sA [solveApollonius $c1 $c2 $c3]
set sB [solveApollonius $c1 $c2 $c3 -1 -1 -1]
puts [$sA format]
puts [$sB format]
```

Output:

```txt

Circle[o=(2.00,2.10),r=3.90]
Circle[o=(2.00,0.83),r=1.17]

```

Note that the Tcl code uses the <code>**</code> (exponentiation) operator to shorten and simplify some operations, and that the <code>circle</code> class is forcing the interpretation of every circle's coordinates as double-precision floating-point numbers.

## VBA



```VBA/VBasic 6.0

Option Explicit
Option Base 0

Private Const intBase As Integer = 0

Private Type tPoint
	X As Double
	Y As Double
End Type
Private Type tCircle
	Centre As tPoint
	Radius As Double
End Type

Private Sub sApollonius()
    Dim Circle1 As tCircle
    Dim Circle2 As tCircle
    Dim Circle3 As tCircle
    Dim CTanTanTan(intBase + 0 to intBase + 7) As tCircle

    With Circle1
        With .Centre
            .X = 0
            .Y = 0
        End With
        .Radius = 1
    End With

    With Circle2
        With .Centre
            .X = 4
            .Y = 0
        End With
        .Radius = 1
    End With

    With Circle3
        With .Centre
            .X = 2
            .Y = 4
        End With
        .Radius = 2
    End With

    Call fApollonius(Circle1,Circle2,Circle3,CTanTanTan()))

End Sub

Public Function fApollonius(ByRef C1 As tCircle, _
                            ByRef C2 As tCircle, _
                            ByRef C3 As tCircle, _
                            ByRef CTanTanTan() As tCircle) As Boolean
' Solves the Problem of Apollonius (finding a circle tangent to three other circles in the plane)
' (x_s - x_1)^2 + (y_s - y_1)^2 = (r_s - Tan_1 * r_1)^2
' (x_s - x_2)^2 + (y_s - y_2)^2 = (r_s - Tan_2 * r_2)^2
' (x_s - x_3)^2 + (y_s - y_3)^2 = (r_s - Tan_3 * r_3)^2
' x_s = M + N * r_s
' y_s = P + Q * r_s

' Parameters:
'   C1, C2, C3 (circles in the problem)
'   Tan1 := An indication if the solution should be externally or internally tangent (+1/-1) to Circle1 (C1)
'   Tan2 := An indication if the solution should be externally or internally tangent (+1/-1) to Circle2 (C2)
'   Tan3 := An indication if the solution should be externally or internally tangent (+1/-1) to Circle3 (C3)

    Dim Tangent(intBase + 0 To intBase + 7, intBase + 0 To intBase + 2) As Integer
    Dim lgTangent As Long
    Dim Tan1 As Integer
    Dim Tan2 As Integer
    Dim Tan3 As Integer

    Dim v11 As Double
    Dim v12 As Double
    Dim v13 As Double
    Dim v14 As Double
    Dim v21 As Double
    Dim v22 As Double
    Dim v23 As Double
    Dim v24 As Double
    Dim w12 As Double
    Dim w13 As Double
    Dim w14 As Double
    Dim w22 As Double
    Dim w23 As Double
    Dim w24 As Double

    Dim p As Double
    Dim Q As Double
    Dim M As Double
    Dim N As Double

    Dim A As Double
    Dim b As Double
    Dim c As Double
    Dim D As Double

    'Check if circle centers are colinear
    If fColinearPoints(C1.Centre, C2.Centre, C3.Centre) Then
        fApollonius = False
        Exit Function
    End If

    Tangent(intBase + 0, intBase + 0) = -1
    Tangent(intBase + 0, intBase + 1) = -1
    Tangent(intBase + 0, intBase + 2) = -1

    Tangent(intBase + 1, intBase + 0) = -1
    Tangent(intBase + 1, intBase + 1) = -1
    Tangent(intBase + 1, intBase + 2) = 1

    Tangent(intBase + 2, intBase + 0) = -1
    Tangent(intBase + 2, intBase + 1) = 1
    Tangent(intBase + 2, intBase + 2) = -1

    Tangent(intBase + 3, intBase + 0) = -1
    Tangent(intBase + 3, intBase + 1) = 1
    Tangent(intBase + 3, intBase + 2) = 1

    Tangent(intBase + 4, intBase + 0) = 1
    Tangent(intBase + 4, intBase + 1) = -1
    Tangent(intBase + 4, intBase + 2) = -1

    Tangent(intBase + 5, intBase + 0) = 1
    Tangent(intBase + 5, intBase + 1) = -1
    Tangent(intBase + 5, intBase + 2) = 1

    Tangent(intBase + 6, intBase + 0) = 1
    Tangent(intBase + 6, intBase + 1) = 1
    Tangent(intBase + 6, intBase + 2) = -1

    Tangent(intBase + 7, intBase + 0) = 1
    Tangent(intBase + 7, intBase + 1) = 1
    Tangent(intBase + 7, intBase + 2) = 1

    For lgTangent = LBound(Tangent) To UBound(Tangent)
        Tan1 = Tangent(lgTangent, intBase + 0)
        Tan2 = Tangent(lgTangent, intBase + 1)
        Tan3 = Tangent(lgTangent, intBase + 2)

        v11 = 2 * (C2.Centre.X - C1.Centre.X)
        v12 = 2 * (C2.Centre.Y - C1.Centre.Y)
        v13 = (C1.Centre.X * C1.Centre.X) _
            - (C2.Centre.X * C2.Centre.X) _
            + (C1.Centre.Y * C1.Centre.Y) _
            - (C2.Centre.Y * C2.Centre.Y) _
            - (C1.Radius * C1.Radius) _
            + (C2.Radius * C2.Radius)
        v14 = 2 * (Tan2 * C2.Radius - Tan1 * C1.Radius)

        v21 = 2 * (C3.Centre.X - C2.Centre.X)
        v22 = 2 * (C3.Centre.Y - C2.Centre.Y)
        v23 = (C2.Centre.X * C2.Centre.X) _
            - (C3.Centre.X * C3.Centre.X) _
            + (C2.Centre.Y * C2.Centre.Y) _
            - (C3.Centre.Y * C3.Centre.Y) _
            - (C2.Radius * C2.Radius) _
            + (C3.Radius * C3.Radius)
        v24 = 2 * ((Tan3 * C3.Radius) - (Tan2 * C2.Radius))

        w12 = v12 / v11
        w13 = v13 / v11
        w14 = v14 / v11

        w22 = (v22 / v21) - w12
        w23 = (v23 / v21) - w13
        w24 = (v24 / v21) - w14

        p = -w23 / w22
        Q = w24 / w22
        M = -(w12 * p) - w13
        N = w14 - (w12 * Q)

        A = (N * N) + (Q * Q) - 1
        b = 2 * ((M * N) - (N * C1.Centre.X) + (p * Q) - (Q * C1.Centre.Y) + (Tan1 * C1.Radius))
        c = (C1.Centre.X * C1.Centre.X) _
          + (M * M) _
          - (2 * M * C1.Centre.X) _
          + (p * p) _
          + (C1.Centre.Y * C1.Centre.Y) _
          - (2 * p * C1.Centre.Y) _
          - (C1.Radius * C1.Radius)

        'Find a root of a quadratic equation (requires the circle centers not to be e.g. colinear)
        D = (b * b) - (4 * A * c)

        With CTanTanTan(lgTangent)
            .Radius = (-b - VBA.Sqr(D)) / (2 * A)
            .Centre.X = M + (N * .Radius)
            .Centre.Y = p + (Q * .Radius)
        End With

    Next lgTangent

    fApollonius = True

End Function


```



## zkl

```zkl
class Circle{
   fcn init(xpos,ypos,radius){
      var [const] x=xpos.toFloat(), y=ypos.toFloat(),r=radius.toFloat();
   }
   fcn toString{ "Circle(%f,%f,%f)".fmt(x,y,r) }
   fcn apollonius(c2,c3,outside=True){
      s1:=s2:=s3:=outside and 1 or -1;

      v11:=2.0*(c2.x - x);
      v12:=2.0*(c2.y - y);
      v13:=x.pow(2) - c2.x.pow(2) +
	   y.pow(2) - c2.y.pow(2) -
	   r.pow(2) + c2.r.pow(2);
      v14:=2.0*(s2*c2.r - s1*r);

      v21:=2.0*(c3.x - c2.x);
      v22:=2.0*(c3.y - c2.y);
      v23:=c2.x.pow(2) - c3.x.pow(2) +
	   c2.y.pow(2) - c3.y.pow(2) -
	   c2.r.pow(2) + c3.r.pow(2);
      v24:=2.0*(s3*c3.r - s2*c2.r);

      w12,w13,w14:=v12/v11,       v13/v11,       v14/v11;
      w22,w23,w24:=v22/v21 - w12, v23/v21 - w13, v24/v21 - w14;

      P:=-w23/w22;
      Q:= w24/w22;
      M:=-w12*P - w13;
      N:= w14 - w12*Q;

      a:=N*N + Q*Q - 1;
      b:=2.0*(M*N - N*x + P*Q - Q*y + s1*r);
      c:=x*x + M*M - 2.0*M*x + P*P + y*y - 2.0*P*y - r*r;

      // find a root of a quadratic equation.
      // This requires the circle centers not to be e.g. colinear
      D:=b*b - 4.0*a*c;
      rs:=(-b - D.sqrt())/(2.0*a);

      Circle(M + N*rs, P + Q*rs, rs);
   }
}
```


```zkl
a,b,c:=Circle(0,0,1), Circle(4,0,1), Circle(2,4,2);
a.apollonius(b,c).println(" Outside");
a.apollonius(b,c,False).println(" Inside");
```

```txt

Circle(2.000000,2.100000,3.900000) Outside
Circle(2.000000,0.833333,1.166667) Inside

```


