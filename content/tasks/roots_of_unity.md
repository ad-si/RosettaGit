+++
title = "Roots of unity"
description = ""
date = 2019-09-08T15:11:06Z
aliases = []
[extra]
id = 2419
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "awk",
  "basic",
  "bbc_basic",
  "c",
  "coffeescript",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "echolisp",
  "erre",
  "forth",
  "fortran",
  "funl",
  "futurebasic",
  "gap",
  "go",
  "groovy",
  "haskell",
  "idl",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "lua",
  "maple",
  "mathematica",
  "matlab",
  "maxima",
  "miniscript",
  "nim",
  "ocaml",
  "octave",
  "oorexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "rlab",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "sparkling",
  "stata",
  "tcl",
  "ursala",
  "vba",
  "zkl",
]
+++

The purpose of this task is to explore working with   [https://en.wikipedia.org/wiki/Complex_number complex numbers].


## Task

Given   <tt>n</tt>,   find the   <tt>n</tt>-th   [[wp:Roots of unity|roots of unity]].





## Ada


```ada
with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Float_Text_IO;           use Ada.Float_Text_IO;
with Ada.Numerics.Complex_Types;  use Ada.Numerics.Complex_Types;

procedure Roots_Of_Unity is
   Root : Complex;
begin
   for N in 2..10 loop
      Put_Line ("N =" & Integer'Image (N));
      for K in 0..N - 1 loop
         Root :=
             Compose_From_Polar
             (  Modulus  => 1.0,
                Argument => Float (K),
                Cycle    => Float (N)
             );
            -- Output
         Put ("   k =" & Integer'Image (K) & ", ");
         if Re (Root) < 0.0 then
            Put ("-");
         else
            Put ("+");
         end if;
         Put (abs Re (Root), Fore => 1, Exp => 0);
         if Im (Root) < 0.0 then
            Put ("-");
         else
            Put ("+");
         end if;
         Put (abs Im (Root), Fore => 1, Exp => 0);
         Put_Line ("i");
      end loop;
   end loop;
end Roots_Of_Unity;
```

[[Ada]] provides a direct implementation of polar composition of complex numbers ''x e''<sup>2&pi;''i y''</sup>. The function Compose_From_Polar is used to compose roots. The third argument of the function is the cycle. Instead of the standard cycle 2&pi;, N is used. Sample output:
<pre style="height:25ex;overflow:scroll">
N = 2
   k = 0, +1.00000+0.00000i
   k = 1, -1.00000+0.00000i
N = 3
   k = 0, +1.00000+0.00000i
   k = 1, -0.50000+0.86603i
   k = 2, -0.50000-0.86603i
N = 4
   k = 0, +1.00000+0.00000i
   k = 1, +0.00000+1.00000i
   k = 2, -1.00000+0.00000i
   k = 3, +0.00000-1.00000i
N = 5
   k = 0, +1.00000+0.00000i
   k = 1, +0.30902+0.95106i
   k = 2, -0.80902+0.58779i
   k = 3, -0.80902-0.58779i
   k = 4, +0.30902-0.95106i
N = 6
   k = 0, +1.00000+0.00000i
   k = 1, +0.50000+0.86603i
   k = 2, -0.50000+0.86603i
   k = 3, -1.00000+0.00000i
   k = 4, -0.50000-0.86603i
   k = 5, +0.50000-0.86603i
N = 7
   k = 0, +1.00000+0.00000i
   k = 1, +0.62349+0.78183i
   k = 2, -0.22252+0.97493i
   k = 3, -0.90097+0.43388i
   k = 4, -0.90097-0.43388i
   k = 5, -0.22252-0.97493i
   k = 6, +0.62349-0.78183i
N = 8
   k = 0, +1.00000+0.00000i
   k = 1, +0.70711+0.70711i
   k = 2, +0.00000+1.00000i
   k = 3, -0.70711+0.70711i
   k = 4, -1.00000+0.00000i
   k = 5, -0.70711-0.70711i
   k = 6, +0.00000-1.00000i
   k = 7, +0.70711-0.70711i
N = 9
   k = 0, +1.00000+0.00000i
   k = 1, +0.76604+0.64279i
   k = 2, +0.17365+0.98481i
   k = 3, -0.50000+0.86603i
   k = 4, -0.93969+0.34202i
   k = 5, -0.93969-0.34202i
   k = 6, -0.50000-0.86603i
   k = 7, +0.17365-0.98481i
   k = 8, +0.76604-0.64279i
N = 10
   k = 0, +1.00000+0.00000i
   k = 1, +0.80902+0.58779i
   k = 2, +0.30902+0.95106i
   k = 3, -0.30902+0.95106i
   k = 4, -0.80902+0.58779i
   k = 5, -1.00000+0.00000i
   k = 6, -0.80902-0.58779i
   k = 7, -0.30902-0.95106i
   k = 8, +0.30902-0.95106i
   k = 9, +0.80902-0.58779i

```



## ALGOL 68

```algol68
FORMAT complex fmt=$g(-6,4)"⊥"g(-6,4)$;
FOR root FROM 2 TO 10 DO
  printf(($g(4)$,root));
  FOR n FROM 0 TO root-1 DO
    printf(($xf(complex fmt)$,complex exp( 0 I 2*pi*n/root)))
  OD;
  printf($l$)
OD
```

Output:

```txt

  +2 1.0000⊥0.0000 -1.000⊥0.0000
  +3 1.0000⊥0.0000 -.5000⊥0.8660 -.5000⊥-.8660
  +4 1.0000⊥0.0000 0.0000⊥1.0000 -1.000⊥0.0000 -.0000⊥-1.000
  +5 1.0000⊥0.0000 0.3090⊥0.9511 -.8090⊥0.5878 -.8090⊥-.5878 0.3090⊥-.9511
  +6 1.0000⊥0.0000 0.5000⊥0.8660 -.5000⊥0.8660 -1.000⊥0.0000 -.5000⊥-.8660 0.5000⊥-.8660
  +7 1.0000⊥0.0000 0.6235⊥0.7818 -.2225⊥0.9749 -.9010⊥0.4339 -.9010⊥-.4339 -.2225⊥-.9749 0.6235⊥-.7818
  +8 1.0000⊥0.0000 0.7071⊥0.7071 0.0000⊥1.0000 -.7071⊥0.7071 -1.000⊥0.0000 -.7071⊥-.7071 -.0000⊥-1.000 0.7071⊥-.7071
  +9 1.0000⊥0.0000 0.7660⊥0.6428 0.1736⊥0.9848 -.5000⊥0.8660 -.9397⊥0.3420 -.9397⊥-.3420 -.5000⊥-.8660 0.1736⊥-.9848 0.7660⊥-.6428
 +10 1.0000⊥0.0000 0.8090⊥0.5878 0.3090⊥0.9511 -.3090⊥0.9511 -.8090⊥0.5878 -1.000⊥0.0000 -.8090⊥-.5878 -.3090⊥-.9511 0.3090⊥-.9511 0.8090⊥-.5878

```



## AutoHotkey

ahk forum: [http://www.autohotkey.com/forum/post-276712.html#276712 discussion]

```AutoHotkey
n := 8, a := 8*atan(1)/n
Loop %n%
   i := A_Index-1, t .= cos(a*i) ((s:=sin(a*i))<0 ? " - i*" . -s : " + i*" . s) "`n"
Msgbox % t
```


## AWK


```AWK

# syntax: GAWK -f ROOTS_OF_UNITY.AWK
BEGIN {
    pi = 3.1415926
    for (n=2; n<=5; n++) {
      printf("%d: ",n)
      for (root=0; root<=n-1; root++) {
        real = cos(2 * pi * root / n)
        imag = sin(2 * pi * root / n)
        printf("%8.5f %8.5fi",real,imag)
        if (root != n-1) { printf(", ") }
      }
      printf("\n")
    }
    exit(0)
}

```

```txt

2:  1.00000  0.00000i, -1.00000  0.00000i
3:  1.00000  0.00000i, -0.50000  0.86603i, -0.50000 -0.86603i
4:  1.00000  0.00000i,  0.00000  1.00000i, -1.00000  0.00000i, -0.00000 -1.00000i
5:  1.00000  0.00000i,  0.30902  0.95106i, -0.80902  0.58779i, -0.80902 -0.58779i,  0.30902 -0.95106i

```



## BASIC

For high n's, this may repeat the root of 1 + 0*i.

```qbasic
 CLS
 PI = 3.1415926#
 n = 5 'this can be changed for any desired n
 angle = 0 'start at angle 0
 DO
 	real = COS(angle) 'real axis is the x axis
 	IF (ABS(real) < 10 ^ -5) THEN real = 0 'get rid of annoying sci notation
 	imag = SIN(angle) 'imaginary axis is the y axis
 	IF (ABS(imag) < 10 ^ -5) THEN imag = 0 'get rid of annoying sci notation
 	PRINT real; "+"; imag; "i" 'answer on every line
 	angle = angle + (2 * PI) / n
 'all the way around the circle at even intervals
 LOOP WHILE angle < 2 * PI
```



## BBC BASIC


```bbcbasic
      @% = &20408
      FOR n% = 2 TO 5
        PRINT STR$(n%) ": " ;
        FOR root% = 0 TO n%-1
          real = COS(2*PI * root% / n%)
          imag = SIN(2*PI * root% / n%)
          PRINT real imag "i" ;
          IF root% <> n%-1 PRINT "," ;
        NEXT
        PRINT
      NEXT n%
```

'''Output:'''

```txt

2:   1.0000  0.0000i, -1.0000  0.0000i
3:   1.0000  0.0000i, -0.5000  0.8660i, -0.5000 -0.8660i
4:   1.0000  0.0000i,  0.0000  1.0000i, -1.0000  0.0000i, -0.0000 -1.0000i
5:   1.0000  0.0000i,  0.3090  0.9511i, -0.8090  0.5878i, -0.8090 -0.5878i,  0.3090 -0.9511i

```



## C


```c
#include <stdio.h>
#include <math.h>

int main()
{
	double a, c, s, PI2 = atan2(1, 1) * 8;
	int n, i;

	for (n = 1; n < 10; n++) for (i = 0; i < n; i++) {
		c = s = 0;
		if (!i )		c =  1;
		else if(n == 4 * i)	s =  1;
		else if(n == 2 * i)	c = -1;
		else if(3 * n == 4 * i)	s = -1;
		else
			a = i * PI2 / n, c = cos(a), s = sin(a);

		if (c) printf("%.2g", c);
		printf(s == 1 ? "i" : s == -1 ? "-i" : s ? "%+.2gi" : "", s);
		printf(i == n - 1 ?"\n":",  ");
	}

	return 0;
}
```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

class Program
{
    static IEnumerable<Complex> RootsOfUnity(int degree)
    {
        return Enumerable
            .Range(0, degree)
            .Select(element => Complex.FromPolarCoordinates(1, 2 * Math.PI * element / degree));
    }

    static void Main()
    {
        var degree = 3;
        foreach (var root in RootsOfUnity(degree))
        {
            Console.WriteLine(root);
        }
    }
}
```

Output:

```txt
(1, 0)
(-0,5, 0,866025403784439)
(-0,5, -0,866025403784438)
```



## C++


```cpp
#include <complex>
#include <cmath>
#include <iostream>

double const pi = 4 * std::atan(1);

int main()
{
  for (int n = 2; n <= 10; ++n)
  {
    std::cout << n << ": ";
    for (int k = 0; k < n; ++k)
      std::cout << std::polar(1, 2*pi*k/n) << " ";
    std::cout << std::endl;
  }
}
```



## CoffeeScript

Most of the effort here is in formatting the results, and the output is still a bit clumsy.

```coffeescript
# Find the n nth-roots of 1
nth_roots_of_unity = (n) ->
  (complex_unit_vector(2*Math.PI*i/n) for i in [1..n])

complex_unit_vector = (rad) ->
  new Complex(Math.cos(rad), Math.sin(rad))

class Complex
  constructor: (@real, @imag) ->
  toString: ->
    round_z = (n) ->
      if Math.abs(n) < 0.00005 then 0 else n
    fmt = (n) -> n.toFixed(3)
    real = round_z @real
    imag = round_z @imag
    s = ''
    if real and imag
      "#{fmt real}+#{fmt imag}i"
    else if real or !imag
      "#{fmt real}"
    else
      "#{fmt imag}i"

do ->
  for n in [2..5]
    console.log "---1 to the 1/#{n}"
    for root in nth_roots_of_unity n
      console.log root.toString()
```

output

```txt

> coffee nth_roots.coffee
---1 to the 1/2
-1.000
1.000
---1 to the 1/3
-0.500+0.866i
-0.500+-0.866i
1.000
---1 to the 1/4
1.000i
-1.000
-1.000i
1.000
---1 to the 1/5
0.309+0.951i
-0.809+0.588i
-0.809+-0.588i
0.309+-0.951i
1.000

```



## Common Lisp


```lisp
(defun roots-of-unity (n)
 (loop for i below n
       collect (cis (* pi (/ (* 2 i) n)))))
```

The expression is slightly more complicated than necessary in order to preserve exact rational arithmetic until multiplying by pi. The author of this example is not a floating point expert and not sure whether this is actually useful; if not, the simpler expression is <tt>(cis (/ (* 2 pi i) n))</tt>.


## Crystal

```ruby
require "complex"

def roots_of_unity(n)
  (0...n).map { |k| (2 * Math::PI * k / n).i.exp }
end

p roots_of_unity(3)

```

Or alternative

```ruby

def roots_of_unity(n)
  (0...n).map { |k| Complex.new(Math.cos(2 * Math::PI * k / n), Math.sin(2 * Math::PI * k / n)) }
end

```

```txt

[(1+0.0i), (-0.4999999999999998+0.8660254037844387i), (-0.5000000000000004-0.8660254037844384i)]

```



## D

Using std.complex:

```d
import std.stdio, std.range, std.algorithm, std.complex;
import std.math: PI;

auto nthRoots(in int n) pure nothrow {
    return n.iota.map!(k => expi(PI * 2 * (k + 1) / n));
}

void main() {
    foreach (immutable i; 1 .. 6)
        writefln("#%d: [%(%5.2f, %)]", i, i.nthRoots);
}
```

```txt
#1: [ 1.00+ 0.00i]
#2: [-1.00+-0.00i,  1.00+ 0.00i]
#3: [-0.50+ 0.87i, -0.50+-0.87i,  1.00+ 0.00i]
#4: [-0.00+ 1.00i, -1.00+-0.00i,  0.00+-1.00i,  1.00+ 0.00i]
#5: [ 0.31+ 0.95i, -0.81+ 0.59i, -0.81+-0.59i,  0.31+-0.95i,  1.00+ 0.00i]
```



## EchoLisp


```scheme

(define (roots-1 n)
   (define theta (// (* 2 PI) n))
   (for/list ((i n))
      (polar 1. (* theta i))))

(roots-1 2)
    → (1+0i -1+0i)
(roots-1 3)
    → (1+0i -0.4999999999999998+0.8660254037844388i -0.5000000000000004-0.8660254037844384i)
(roots-1 4)
    → (1+0i 0+i -1+0i 0-i)

```



## ERRE

<lang>
PROGRAM UNITY_ROOTS

!
! for rosettacode.org
!

BEGIN
   PRINT(CHR$(12);) !CLS
   N=5                                       ! this can be changed for any desired n
   ANGLE=0                                   ! start at ANGLE 0
   REPEAT
     REAL=COS(ANGLE)                         ! real axis is the x axis
     IF (ABS(REAL)<10^-5) THEN REAL=0 END IF ! get rid of annoying sci notation
     IMAG=SIN(ANGLE)                         ! imaginary axis is the y axis
     IF (ABS(IMAG)<10^-5) THEN IMAG=0 END IF ! get rid of annoying sci notation
     PRINT(REAL;"+";IMAG;"i")                ! answer on every line
     ANGLE+=(2*π)/N
                                             ! all the way around the circle at even intervals
   UNTIL ANGLE>=2*π
END PROGRAM

```

Note: Adapted from Qbasic version. π is the predefined constant Greek Pi.


## Forth

Complex numbers are not a native type in Forth, so we calculate the roots by hand.

```forth
: f0. ( f -- )
  fdup 0e 0.001e f~ if fdrop 0e then f. ;
: .roots ( n -- )
  dup 1 do
    pi i 2* 0 d>f f* dup 0 d>f f/          ( F: radians )
    fsincos cr ." real " f0. ." imag " f0.
  loop drop ;

3 set-precision
5 .roots
```

On the other hand, complex numbers are implemented by the FSL.
```forth
require fsl-util.fs
require fsl/complex.fs

: abs= 1E-12 F~ ;
: clamp-to-0 FDUP 0E0 abs= IF FDROP 0E0 THEN ;
: zclamp-to-0
  clamp-to-0 FSWAP
  clamp-to-0 FSWAP ;
: .roots
  1+ 2 DO
    I . ." : "
    I 0 DO
      1E0 2E0 PI F* I S>F F* J S>F F/ polar> zclamp-to-0 z. SPACE
    LOOP
    CR
  LOOP ;
3 SET-PRECISION
5 .roots
```



## Fortran


### Sin/Cos + Scalar Loop

```fortran
PROGRAM Roots

  COMPLEX :: root
  INTEGER :: i, n
  REAL :: angle, pi

  pi = 4.0 * ATAN(1.0)
  DO n = 2, 7
    angle = 0.0
    WRITE(*,"(I1,A)", ADVANCE="NO") n,": "
    DO i = 1, n
      root = CMPLX(COS(angle), SIN(angle))
      WRITE(*,"(SP,2F7.4,A)", ADVANCE="NO") root, "j  "
      angle = angle + (2.0*pi / REAL(n))
    END DO
    WRITE(*,*)
  END DO

END PROGRAM Roots
```

Output
 2: +1.0000+0.0000j  -1.0000+0.0000j
 3: +1.0000+0.0000j  -0.5000+0.8660j  -0.5000-0.8660j
 4: +1.0000+0.0000j  +0.0000+1.0000j  -1.0000+0.0000j  +0.0000-1.0000j
 5: +1.0000+0.0000j  +0.3090+0.9511j  -0.8090+0.5878j  -0.8090-0.5878j  +0.3090-0.9511j
 6: +1.0000+0.0000j  +0.5000+0.8660j  -0.5000+0.8660j  -1.0000+0.0000j  -0.5000-0.8660j  +0.5000-0.8660j
 7: +1.0000+0.0000j  +0.6235+0.7818j  -0.2225+0.9749j  -0.9010+0.4339j  -0.9010-0.4339j  -0.2225-0.9749j  +0.6235-0.7818j

===Exp + Array-valued Statement===
```fortran
program unity
     real, parameter :: pi = 3.141592653589793
     complex, parameter :: i = (0, 1)
     complex, dimension(0:7-1) :: unit_circle
     integer :: n, j

     do n = 2, 7
          !!!! KEY STEP, does all the calculations in one statement !!!!
        unit_circle(0:n-1) = exp(2*i*pi/n * (/ (j, j=0, n-1) /) )

        write(*,"(i1,a)", advance="no") n, ": "
        write(*,"(sp,2f7.4,a)", advance="no") (unit_circle(j), "j  ", j = 0, n-1)
        write(*,*)
     end do
 end program unity
```



## FunL

FunL has built-in support for complex numbers.  <code>i</code> is predefined to represent the imaginary unit.

```funl
import math.{exp, Pi}

def rootsOfUnity( n ) = {exp( 2Pi i k/n ) | k <- 0:n}

println( rootsOfUnity(3) )
```


```txt

{1.0, -0.4999999999999998+0.8660254037844387i, -0.5000000000000004-0.8660254037844385i}

```





## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as long n, root
dim as double real, imag

for n = 2 to 7
print n;":" ;
for root = 0 to n-1
real = cos( 2 * pi * root / n)
imag = sin( 2 * pi * root / n)
print using "-##.#####"; real;using "-##.#####"; imag; "i";
if root <> n-1 then print ",";
next
print
next

```

Output:

```txt

 2:  1.00000  0.00000i, -1.00000  0.00000i
 3:  1.00000  0.00000i, -0.50000  0.86603i, -0.50000 -0.86603i
 4:  1.00000  0.00000i,  0.00000  1.00000i, -1.00000  0.00000i, -0.00000 -1.00000i
 5:  1.00000  0.00000i,  0.30902  0.95106i, -0.80902  0.58779i, -0.80902 -0.58779i,  0.30902 -0.95106i
 6:  1.00000  0.00000i,  0.50000  0.86603i, -0.50000  0.86603i, -1.00000  0.00000i, -0.50000 -0.86603i,  0.50000 -0.86603i
 7:  1.00000  0.00000i,  0.62349  0.78183i, -0.22252  0.97493i, -0.90097  0.43388i, -0.90097 -0.43388i, -0.22252 -0.97493i,  0.62349 -0.78183i

```



## GAP


```gap
roots := n -> List([0 .. n-1], k -> E(n)^k);

r:=roots(7);
# [ 1, E(7), E(7)^2, E(7)^3, E(7)^4, E(7)^5, E(7)^6 ]

List(r, x -> x^7);
# [ 1, 1, 1, 1, 1, 1, 1 ]
```



## Go


```go
package main

import (
    "fmt"
    "math"
    "math/cmplx"
)

func main() {
    for n := 2; n <= 5; n++ {
        fmt.Printf("%d roots of 1:\n", n)
        for _, r := range roots(n) {
            fmt.Printf("  %18.15f\n", r)
        }
    }
}

func roots(n int) []complex128 {
    r := make([]complex128, n)
    for i := 0; i < n; i++ {
        r[i] = cmplx.Rect(1, 2*math.Pi*float64(i)/float64(n))
    }
    return r
}
```

Output:

```txt
2 roots of 1:
  ( 1.000000000000000+0.000000000000000i)
  (-1.000000000000000+0.000000000000000i)
3 roots of 1:
  ( 1.000000000000000+0.000000000000000i)
  (-0.500000000000000+0.866025403784439i)
  (-0.500000000000000-0.866025403784438i)
4 roots of 1:
  ( 1.000000000000000+0.000000000000000i)
  ( 0.000000000000000+1.000000000000000i)
  (-1.000000000000000+0.000000000000000i)
  (-0.000000000000000-1.000000000000000i)
5 roots of 1:
  ( 1.000000000000000+0.000000000000000i)
  ( 0.309016994374948+0.951056516295154i)
  (-0.809016994374947+0.587785252292473i)
  (-0.809016994374947-0.587785252292473i)
  ( 0.309016994374947-0.951056516295154i)
```



## Groovy

Because the Groovy language does not provide a built-in facility for complex arithmetic, this example relies on the Complex class defined in the [[Complex_numbers#Groovy|Complex numbers]] example.

```groovy
/** The following closure creates a list of n evenly-spaced points around the unit circle,
  * useful in FFT calculations, among other things */
def rootsOfUnity = { n ->
    (0..<n).collect {
        Complex.fromPolar(1, 2 * Math.PI * it / n)
    }
}
```

Test program:

```groovy
def tol = 0.000000001  // tolerance: acceptable "wrongness" to account for rounding error

((1..6) + [16]). each { n ->
    println "rootsOfUnity(${n}):"
    def rou = rootsOfUnity(n)
    rou.each { println it }
    assert rou[0] == 1
    def actual = n > 1 ? rou[Math.floor(n/2) as int] : rou[0]
    def expected = n > 1 ? (n%2 == 0) ? -1 : ~rou[Math.ceil(n/2) as int] : rou[0]
    def message = n > 1 ? (n%2 == 0) ? 'middle-most root should be -1' : 'two middle-most roots should be conjugates' : ''
    assert (actual - expected).abs() < tol : message
    assert rou.every { (it.rho - 1) < tol } : 'all roots should have magnitude 1'
    println()
}
```

Output:
<pre style="height:25ex;overflow:scroll;">rootsOfUnity(1):
1.0

rootsOfUnity(2):
1.0
-1.0 + 1.2246467991473532E-16i

rootsOfUnity(3):
1.0
-0.4999999998186198 + 0.8660254038891585i
-0.5000000003627604 - 0.8660254035749988i

rootsOfUnity(4):
1.0
6.123233995736766E-17 + i
-1.0 + 1.2246467991473532E-16i
-1.8369701987210297E-16 - i

rootsOfUnity(5):
1.0
0.30901699437494745 + 0.9510565162951535i
-0.8090169943749473 + 0.5877852522924732i
-0.8090169943749475 - 0.587785252292473i
0.30901699437494723 - 0.9510565162951536i

rootsOfUnity(6):
1.0
0.4999999998186201 + 0.8660254038891584i
-0.5000000003627598 + 0.8660254035749991i
-1.0 - 6.283181638240517E-10i
-0.4999999992744804 - 0.8660254042033175i
0.5000000009068993 - 0.8660254032608401i

rootsOfUnity(16):
1.0
0.9238795325112867 + 0.3826834323650898i
0.7071067811865476 + 0.7071067811865475i
0.38268343236508984 + 0.9238795325112867i
6.123233995736766E-17 + i
-0.3826834323650897 + 0.9238795325112867i
-0.7071067811865475 + 0.7071067811865476i
-0.9238795325112867 + 0.3826834323650899i
-1.0 + 1.2246467991473532E-16i
-0.9238795325112868 - 0.38268343236508967i
-0.7071067811865477 - 0.7071067811865475i
-0.38268343236509034 - 0.9238795325112865i
-1.8369701987210297E-16 - i
0.38268343236509 - 0.9238795325112866i
0.7071067811865474 - 0.7071067811865477i
0.9238795325112865 - 0.3826834323650904i
```



## Haskell


```haskell
import Data.Complex (Complex, cis)

rootsOfUnity :: (Enum a, Floating a) => a -> [Complex a]
rootsOfUnity n =
  [ cis (2 * pi * k / n)
  | k <- [0 .. n - 1] ]

main :: IO ()
main = mapM_ print $ rootsOfUnity 3
```

```haskell
1.0 :+ 0.0
(-0.4999999999999998) :+ 0.8660254037844388
(-0.5000000000000004) :+ (-0.8660254037844384)
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
   roots(10)
end

procedure roots(n)
   every n := 2 to 10 do
       every writes(n | (str_rep((0 to (n-1)) * 2 * &pi / n)) | "\n")
end

procedure str_rep(k)
  return " " || cos(k) || "+" || sin(k) || "i"
end
```

Notes:
* The [[:Category:Icon_Programming_Library|The Icon Programming Library]] implements a complex type but not a polar type


## IDL

For some example <tt>n</tt>:

```idl
n = 5
print,  exp( dcomplex( 0, 2*!dpi/n) ) ^ ( 1 + indgen(n) )
```

Outputs:

```idl
( 0.30901699, 0.95105652)( -0.80901699, 0.58778525)( -0.80901699, -0.58778525)( 0.30901699, -0.95105652)( 1.0000000, -1.1102230e-16)
```



## J


```j
   rou=: [: ^ 0j2p1 * i. % ]

   rou 4
1 0j1 _1 0j_1

   rou 5
1 0.309017j0.951057 _0.809017j0.587785 _0.809017j_0.587785 0.309017j_0.951057
```

The computation can also be written as a loop, shown here for comparison only.

```j
rou1=: 3 : 0
 z=. 0 $ r=. ^ o. 0j2 % y [ e=. 1
 for. i.y do.
  z=. z,e
  e=. e*r
 end.
 z
)
```



## Java

Java doesn't have a nice way of dealing with complex numbers, so the real and imaginary parts are calculated separately based on the angle and printed together. There are also checks in this implementation to get rid of extremely small values (< 1.0E-3 where scientific notation sets in for <tt>Double</tt>s). Instead, they are simply represented as 0. To remove those checks (for very high <tt>n</tt>'s), remove both if statements.

```java
import java.util.Locale;

public class Test {

    public static void main(String[] a) {
        for (int n = 2; n < 6; n++)
            unity(n);
    }

    public static void unity(int n) {
        System.out.printf("%n%d: ", n);

        //all the way around the circle at even intervals
        for (double angle = 0; angle < 2 * Math.PI; angle += (2 * Math.PI) / n) {

            double real = Math.cos(angle); //real axis is the x axis

            if (Math.abs(real) < 1.0E-3)
                real = 0.0; //get rid of annoying sci notation

            double imag = Math.sin(angle); //imaginary axis is the y axis

            if (Math.abs(imag) < 1.0E-3)
                imag = 0.0;

            System.out.printf(Locale.US, "(%9f,%9f) ", real, imag);
        }
    }
}
```



```txt
2: ( 1.000000, 0.000000) (-1.000000, 0.000000)
3: ( 1.000000, 0.000000) (-0.500000, 0.866025) (-0.500000,-0.866025)
4: ( 1.000000, 0.000000) ( 0.000000, 1.000000) (-1.000000, 0.000000) ( 0.000000,-1.000000)
5: ( 1.000000, 0.000000) ( 0.309017, 0.951057) (-0.809017, 0.587785) (-0.809017,-0.587785) ( 0.309017,-0.951057)
```



## JavaScript


```javascript
function Root(angle) {
	with (Math) { this.r = cos(angle); this.i = sin(angle) }
}

Root.prototype.toFixed = function(p) {
	return this.r.toFixed(p) + (this.i >= 0 ? '+' : '') + this.i.toFixed(p) + 'i'
}

function roots(n) {
	var rs = [], teta = 2*Math.PI/n
	for (var angle=0, i=0; i<n; angle+=teta, i+=1) rs.push( new Root(angle) )
	return rs
}

for (var n=2; n<8; n+=1) {
	document.write(n, ': ')
	var rs=roots(n); for (var i=0; i<rs.length; i+=1) document.write( i ? ', ' : '', rs[i].toFixed(5) )
	document.write('
')
}

```

```txt
2: 1.00000+0.00000i, -1.00000+0.00000i
3: 1.00000+0.00000i, -0.50000+0.86603i, -0.50000-0.86603i
4: 1.00000+0.00000i, 0.00000+1.00000i, -1.00000+0.00000i, -0.00000-1.00000i
5: 1.00000+0.00000i, 0.30902+0.95106i, -0.80902+0.58779i, -0.80902-0.58779i, 0.30902-0.95106i
6: 1.00000+0.00000i, 0.50000+0.86603i, -0.50000+0.86603i, -1.00000+0.00000i, -0.50000-0.86603i, 0.50000-0.86603i
7: 1.00000+0.00000i, 0.62349+0.78183i, -0.22252+0.97493i, -0.90097+0.43388i, -0.90097-0.43388i, -0.22252-0.97493i, 0.62349-0.78183i
```



## jq

Using the same example as in the Julia section, and representing x + i*y as [x,y]:

```jq
def nthroots(n):
  (8 * (1|atan)) as $twopi
  | range(0;n) | (($twopi * .) / n) as $angle | [ ($angle | cos), ($angle | sin) ];

nthroots(10)
```

```jq
$ uname -a
Darwin Mac-mini 13.3.0 Darwin Kernel Version 13.3.0: Tue Jun  3 21:27:35 PDT 2014; root:xnu-2422.110.17~1/RELEASE_X86_64 x86_64

$ time jq -c -n -f Roots_of_unity.jq
[1,0]
[0.8090169943749475,0.5877852522924731]
[0.30901699437494745,0.9510565162951535]
[-0.30901699437494734,0.9510565162951536]
[-0.8090169943749473,0.5877852522924732]
[-1,1.2246467991473532e-16]
[-0.8090169943749475,-0.587785252292473]
[-0.30901699437494756,-0.9510565162951535]
[0.30901699437494723,-0.9510565162951536]
[0.8090169943749473,-0.5877852522924732]

real	0m0.015s
user	0m0.004s
sys	0m0.004s

```



## Julia


```julia
nthroots(n::Integer) = [ cospi(2k/n)+sinpi(2k/n)im for k = 0:n-1 ]
```

(One could also use complex exponentials or other formulations.) For example, `nthroots(10)` gives:

```txt

10-element Array{Complex{Float64},1}:
            1.0+0.0im
  0.809017+0.587785im
  0.309017+0.951057im
 -0.309017+0.951057im
 -0.809017+0.587785im
           -1.0+0.0im
 -0.809017-0.587785im
 -0.309017-0.951057im
  0.309017-0.951057im
  0.809017-0.587785im

```



## Kotlin


```scala
import java.lang.Math.*

data class Complex(val r: Double, val i: Double) {
    override fun toString() = when {
        i == 0.0 -> r.toString()
        r == 0.0 -> i.toString() + 'i'
        else -> "$r + ${i}i"
    }
}

fun unity_roots(n: Number) = (1..n.toInt() - 1).map {
    val a = it * 2 * PI / n.toDouble()
    var r = cos(a); if (abs(r) < 1e-6) r = 0.0
    var i = sin(a); if (abs(i) < 1e-6) i = 0.0
    Complex(r, i)
}

fun main(args: Array<String>) {
    (1..4).forEach { println(listOf(1) + unity_roots(it)) }
    println(listOf(1) + unity_roots(5.0))
}
```

```txt
[1]
[1, -1.0]
[1, -0.4999999999999998 + 0.8660254037844387i, -0.5000000000000004 + -0.8660254037844385i]
[1, 1.0i, -1.0, -1.0i]
[1, 0.30901699437494745 + 0.9510565162951535i, -0.8090169943749473 + 0.5877852522924732i, -0.8090169943749475 + -0.587785252292473i, 0.30901699437494723 + -0.9510565162951536i]
```



## Liberty BASIC


```lb
WindowWidth  =400
WindowHeight =400

'nomainwin

open "N'th Roots of One" for graphics_nsb_nf as #w

#w "trapclose [quit]"

for n =1 To 10
    angle =0
    #w "font arial 16 bold"
    print n; "th roots."
    #w "cls"
    #w "size 1 ; goto 200 200 ; down ; color lightgray ; circle 150 ; size 10 ; set 200 200 ; size 2"
    #w "up ; goto 200 0 ; down ; goto 200 400 ; up ; goto 0 200 ; down ; goto 400 200"
    #w "up ; goto 40 20 ; down ; color black"
    #w "font arial 6"
    #w "\"; n; " roots of 1."

    for i = 1 To n
        x = cos( Radian( angle))
        y = sin( Radian( angle))

        print using( "##", i); ":  ( " + using( "##.######", x);_
          " +i *" +using( "##.######", y); ")      or     e^( i *"; i -1; " *2 *Pi/ "; n; ")"

        #w "color "; 255 *i /n; " 0 "; 256 -255 *i /n
        #w "up ; goto 200 200"
        #w "down ; goto "; 200 +150 *x; " "; 200 -150 *y
        #w "up   ; goto "; 200 +165 *x; " "; 200 -165 *y
        #w "\"; str$( i)
        #w "up"

        angle =angle +360 /n

    next i

    timer 500, [on]
    wait
  [on]
    timer 0
next n

wait

[quit]
    close #w

    end

function Radian( theta)
    Radian =theta *3.1415926535 /180
end function
```



## Lua

Complex numbers from the Lua implementation on the complex numbers page.

```lua
--defines addition, subtraction, negation, multiplication, division, conjugation, norms, and a conversion to strgs.
complex = setmetatable({
__add = function(u, v) return complex(u.real + v.real, u.imag + v.imag) end,
__sub = function(u, v) return complex(u.real - v.real, u.imag - v.imag) end,
__mul = function(u, v) return complex(u.real * v.real - u.imag * v.imag, u.real * v.imag + u.imag * v.real) end,
__div = function(u, v) return u * complex(v.real / v.norm, -v.imag / v.norm) end,
__unm = function(u) return complex(-u.real, -u.imag) end,
__concat = function(u, v)
    if type(u) == "table" then return u.real .. " + " .. u.imag .. "i" .. v
	elseif type(u) == "string" or type(u) == "number" then return u .. v.real .. " + " .. v.imag .. "i"
	end end,
__index = function(u, index)
  local operations = {
    norm = function(u) return u.real ^ 2 + u.imag ^ 2 end,
    conj = function(u) return complex(u.real, -u.imag) end,
  }
  return operations[index] and operations[index](u)
end,
__newindex = function() error() end
}, {
__call = function(z, realpart, imagpart) return setmetatable({real = realpart, imag = imagpart}, complex) end
} )
n = io.read() + 0
val = complex(math.cos(2*math.pi / n), math.sin(2*math.pi / n))
root = complex(1, 0)
for i = 1, n do
  root = root * val
  print(root .. "")
end
```



## Maple


```Maple
RootsOfUnity := proc( n )
    solve(z^n = 1, z);
end proc:
```


```Maple
for i from 2 to 6 do
    printf( "%d: %a\n", i, [ RootsOfUnity(i) ] );
end do;
```

Output:

```Maple
2: [1, -1]
3: [1, -1/2-1/2*I*3^(1/2), -1/2+1/2*I*3^(1/2)]
4: [1, -1, I, -I]
5: [1, 1/4*5^(1/2)-1/4+1/4*I*2^(1/2)*(5+5^(1/2))^(1/2), -1/4*5^(1/2)-1/4+1/4*I*2^(1/2)*(5-5^(1/2))^(1/2), -1/4*5^(1/2)-1/4-1/4*I*2^(1/2)*(5-5^(1/2))^(1/2), 1/4*5^(1/2)-1/4-1/4*I*2^(1/2)*(5+5^(1/2))^(1/2)]
6: [1, -1, 1/2*(-2-2*I*3^(1/2))^(1/2), -1/2*(-2-2*I*3^(1/2))^(1/2), 1/2*(-2+2*I*3^(1/2))^(1/2), -1/2*(-2+2*I*3^(1/2))^(1/2)]
```



## Mathematica

Setting this up in Mathematica is easy, because it already handles complex numbers:

```Mathematica
RootsUnity[nthroot_Integer?Positive] := Table[Exp[2 Pi I i/nthroot], {i, 0, nthroot - 1}]
```

Note that Mathematica will keep the expression as exact as possible. Simplifications can be made to more known (trigonometric) functions by using the function ExpToTrig. If only a numerical approximation is necessary the function N will transform the exact result to a numerical approximation. Examples (exact not simplified, exact simplified, approximated):

```txt
RootsUnity[2]
RootsUnity[3]
RootsUnity[4]
RootsUnity[5]

RootsUnity[2]//ExpToTrig
RootsUnity[3]//ExpToTrig
RootsUnity[4]//ExpToTrig
RootsUnity[5]//ExpToTrig

RootsUnity[2]//N
RootsUnity[3]//N
RootsUnity[4]//N
RootsUnity[5]//N
```

gives back:
<div style="height:35ex;overflow:scroll">
<math>\{1,-1\}</math>

<math>\left\{1,e^{\frac{2 i \pi }{3}},e^{-\frac{2 i \pi }{3}}\right\}</math>

<math>\{1,i,-1,-i\}</math>

<math>\left\{1,e^{\frac{2 i \pi }{5}},e^{\frac{4 i \pi }{5}},e^{-\frac{4 i \pi }{5}},e^{-\frac{2 i \pi }{5}}\right\}</math>


<math>\{1,-1\}</math>

<math>\left\{1,-\frac{1}{2}+\frac{i \sqrt{3}}{2},-\frac{1}{2}-\frac{i \sqrt{3}}{2}\right\}</math>

<math>\{1,i,-1,-i\}</math>

<math>\left\{1,-\frac{1}{4}+\frac{\sqrt{5}}{4}+i \sqrt{\frac{5}{8}+\frac{\sqrt{5}}{8}},-\frac{1}{4}-\frac{\sqrt{5}}{4}+i
   \sqrt{\frac{5}{8}-\frac{\sqrt{5}}{8}},-\frac{1}{4}-\frac{\sqrt{5}}{4}-i \sqrt{\frac{5}{8}-\frac{\sqrt{5}}{8}},-\frac{1}{4}+\frac{\sqrt{5}}{4}-i
   \sqrt{\frac{5}{8}+\frac{\sqrt{5}}{8}}\right\}</math>


<math>\{1.,-1.\}</math>

<math>\{1.,-0.5+0.866025 i,-0.5-0.866025 i\}</math>

<math>\{1.,0.+1. i,-1.,0.-1. i\}</math>

<math>\{1.,0.309017+0.951057 i,-0.809017+0.587785 i,-0.809017-0.587785 i,0.309017-0.951057 i\}</math>
</div>


## MATLAB


```MATLAB
function z = rootsOfUnity(n)

    assert(n >= 1,'n >= 1');
    z = roots([1 zeros(1,n-1) -1]);

end
```

Sample Output:

```MATLAB>>
 rootsOfUnity(3)

ans =

 -0.500000000000000 + 0.866025403784439i
 -0.500000000000000 - 0.866025403784439i
  1.000000000000000
```



## Maxima


```maxima
solve(1 = x^n, x)
```

Demonstration:

```maxima
for n:1 thru 5 do display(solve(1 = x^n, x));
```

Output:

```maxima
solve(1 = x, x) = [x = 1]
solve(1 = x^2, x) = [x = -1, x = 1]
solve(1 = x^3, x) = [x = (sqrt(3)*%i-1)/2, x = -(sqrt(3)*%i+1)/2, x = 1]
solve(1 = x^4, x) = [x = %i, x = -1, x = -%i, x = 1]
solve(1 = x^5, x) = [x = %e^((2*%i*%pi)/5), x = %e^((4*%i*%pi)/5), x = %e^(-(4*%i*%pi)/5), x = %e^(-(2*%i*%pi)/5), x = 1]
```



## MiniScript


```MiniScript

complexRoots = function(n)
    result = []
    for i in range(0, n-1)
        real = cos(2*pi * i/n)
        if abs(real) < 1e-6 then real = 0
        imag = sin(2*pi * i/n)
        if abs(imag) < 1e-6 then imag = 0
        result.push real + " " + "+" * (imag>=0) + imag + "i"
    end for
    return result
end function

for i in range(2,5)
    print i + ": " + complexRoots(i).join(", ")
end for
```


```txt
2: 1 +0i, -1 +0i
3: 1 +0i, -0.5 +0.866025i, -0.5 -0.866025i
4: 1 +0i, 0 +1i, -1 +0i, 0 -1i
5: 1 +0i, 0.309017 +0.951057i, -0.809017 +0.587785i, -0.809017 -0.587785i, 0.309017 -0.951057i
```


=={{header|MK-61/52}}==
<lang>П0	0	П1	ИП1	sin	ИП1	cos	С/П	2	пи
*	ИП0	/	ИП1	+	П1	БП	03
```



## Nim

```nim
import complex, math

proc rect(r, phi: float): Complex = (r * cos(phi), sin(phi))

proc croots(n): seq[Complex] =
  result = @[]
  if n <= 0: return
  for k in 0 .. < n:
    result.add rect(1, 2 * k.float * Pi / n.float)

for nr in 2..10:
  echo nr, " ", croots(nr)
```

Output:

```txt
2 @[(1.0, 0.0), (-1.0, 1.224646799147353e-16)]
3 @[(1.0, 0.0), (-0.4999999999999998, 0.8660254037844387), (-0.5000000000000004, -0.8660254037844384)]
4 @[(1.0, 0.0), (6.123233995736766e-17, 1.0), (-1.0, 1.224646799147353e-16), (-1.83697019872103e-16, -1.0)]
5 @[(1.0, 0.0), (0.3090169943749475, 0.9510565162951535), (-0.8090169943749473, 0.5877852522924732), (-0.8090169943749476, -0.587785252292473), (0.3090169943749472, -0.9510565162951536)]
6 @[(1.0, 0.0), (0.5000000000000001, 0.8660254037844386), (-0.4999999999999998, 0.8660254037844387), (-1.0, 1.224646799147353e-16), (-0.5000000000000004, -0.8660254037844384), (0.5000000000000001, -0.8660254037844386)]
7 @[(1.0, 0.0), (0.6234898018587336, 0.7818314824680298), (-0.2225209339563143, 0.9749279121818236), (-0.900968867902419, 0.4338837391175582), (-0.9009688679024191, -0.433883739117558), (-0.2225209339563146, -0.9749279121818236), (0.6234898018587334, -0.7818314824680299)]
8 @[(1.0, 0.0), (0.7071067811865476, 0.7071067811865475), (6.123233995736766e-17, 1.0), (-0.7071067811865475, 0.7071067811865476), (-1.0, 1.224646799147353e-16), (-0.7071067811865477, -0.7071067811865475), (-1.83697019872103e-16, -1.0), (0.7071067811865474, -0.7071067811865477)]
9 @[(1.0, 0.0), (0.766044443118978, 0.6427876096865393), (0.1736481776669304, 0.984807753012208), (-0.4999999999999998, 0.8660254037844387), (-0.9396926207859083, 0.3420201433256689), (-0.9396926207859084, -0.3420201433256687), (-0.5000000000000004, -0.8660254037844384), (0.17364817766693, -0.9848077530122081), (0.7660444431189778, -0.6427876096865396)]
10 @[(1.0, 0.0), (0.8090169943749475, 0.5877852522924731), (0.3090169943749475, 0.9510565162951535), (-0.3090169943749473, 0.9510565162951536), (-0.8090169943749473, 0.5877852522924732), (-1.0, 1.224646799147353e-16), (-0.8090169943749476, -0.587785252292473), (-0.3090169943749476, -0.9510565162951535), (0.3090169943749472, -0.9510565162951536), (0.8090169943749473, -0.5877852522924734)]
```



## OCaml


```ocaml
open Complex

let pi = 4. *. atan 1.

let () =
  for n = 1 to 10 do
    Printf.printf "%2d " n;
    for k = 1 to n do
      let ret = polar 1. (2. *. pi *. float_of_int k /. float_of_int n) in
        Printf.printf "(%f + %f i)" ret.re ret.im
    done;
    print_newline ()
  done
```



## Octave


```octave
for j = 2 : 10
  printf("*** %d\n", j);
  for n = 1 : j
    disp(exp(2i*pi*n/j));
  endfor
  disp("");
endfor
```



## OoRexx

```oorexx
/*REXX program computes the  K  roots of unity  (which include complex roots).*/
parse Version v
Say v
parse arg n frac .                     /*get optional arguments from the C.L. */
if n==''    then n=1                   /*Not specified?  Then use the default.*/
if frac=''  then frac=5                /* "      "         "   "   "     "    */
start=abs(n)                           /*assume only one  K  is wanted.       */
if n<0      then start=1               /*Negative?  Then use a range of  K's. */
                                       /*display unity roots for a range,  or */
  do k=start  to abs(n)                /*                   just for one  K.  */
  say right(k 'roots of unity',40,"-") /*display a pretty separator with title*/
     do angle=0  by 360/k  for k       /*compute the angle for each root.     */
     rp=adjust(rxCalcCos(angle,,'D'))  /*compute real part via  COS  function.*/
     if left(rp,1)\=='-' then rp=" "rp /*not negative?  Then pad with a blank.*/
     ip=adjust(rxCalcSin(angle,,'D'))  /*compute imaginary part via SIN funct.*/
     if left(ip,1)\=='-' then ip="+"ip /*Not negative?  Then pad with  + char.*/
     if ip=0  then say rp              /*Only real part? Ignore imaginary part*/
              else say left(rp,frac+4)ip'i'   /*show the real & imaginary part*/
     end  /*angle*/
  end      /*k*/
exit                                   /*stick a fork in it,  we're all done. */
/*----------------------------------------------------------------------------*/
adjust: parse arg x; near0='1e-' || (digits()-digits()%10)   /*compute small #*/
        if abs(x)<near0  then x=0            /*if near zero, then assume zero.*/
        return format(x,,frac)/1             /*fraction digits past dec point.*/
::requires rxMath library
```

```txt
D:\>rexx nrootoo 5
REXX-ooRexx_4.2.0(MT)_64-bit 6.04 22 Feb 2014
------------------------5 roots of unity
 1
 0.30902 +0.95106i
-0.80902 +0.58779i
-0.80902 -0.58779i
 0.30902 -0.95106i
```



## PARI/GP


```parigp
vector(n,k,exp(2*Pi*I*k/n))
```


<code>sqrtn()</code> can give the first n'th root, from which the others by multiplying or powering.


```parigp
nth_roots(n) = my(z);sqrtn(1,n,&z); vector(n,i, z^i);
```


Both the above give floating point complex numbers even when a root could be exact, like <code>-1</code> or fourth root <code>I</code>.

<code>quadgen()</code> can be used for an exact 6th root.  (Quads cannot be mixed with ordinary complex numbers, and they always print as <code>w</code>.)


```parigp
sixth_root = quadgen(-3);   /* 6th root of unity, exact */
vector(6,n, sixth_root^n)   /* all the 6'th roots */
```



## Pascal

```pascal
Program Roots;

var
  root: record  // poor man's complex type.
    r: real;
    i: real;
  end;
  i, n:  integer;
  angle: real;

begin
  for n := 2 to 7 do
  begin
    angle := 0.0;
    write(n, ': ');
    for i := 1 to n do
    begin
      root.r := cos(angle);
      root.i := sin(angle);
      write(root.r:8:5, root.i:8:5, 'i ');
      angle := angle + (2.0 * pi / n);
    end;
    writeln;
  end;
end.
```

Output:

```txt

2:  1.00000 0.00000i -1.00000 0.00000i
3:  1.00000 0.00000i -0.50000 0.86603i -0.50000-0.86603i
4:  1.00000 0.00000i  0.00000 1.00000i -1.00000 0.00000i -0.00000-1.00000i
5:  1.00000 0.00000i  0.30902 0.95106i -0.80902 0.58779i -0.80902-0.58779i  0.30902-0.95106i
6:  1.00000 0.00000i  0.50000 0.86603i -0.50000 0.86603i -1.00000-0.00000i -0.50000-0.86603i  0.50000-0.86603i
7:  1.00000 0.00000i  0.62349 0.78183i -0.22252 0.97493i -0.90097 0.43388i -0.90097-0.43388i -0.22252-0.97493i  0.62349-0.78183i

```



## Perl

The <code>root()</code> function returns a list of the N many N'th roots of any complex Z, in this case 1.


```perl
use Math::Complex;

foreach my $n (2 .. 10) {
  printf "%2d", $n;
  my @roots = root(1,$n);
  foreach my $root (@roots) {
    $root->display_format(style => 'cartesian', format => '%.3f');
    print " $root";
  }
  print "\n";
}
```

Output:

```txt

 2 1.000 -1.000+0.000i
 3 1.000 -0.500+0.866i -0.500-0.866i
 4 1.000 0.000+1.000i -1.000+0.000i -0.000-1.000i
 5 1.000 0.309+0.951i -0.809+0.588i -0.809-0.588i 0.309-0.951i
 6 1.000 0.500+0.866i -0.500+0.866i -1.000+0.000i -0.500-0.866i 0.500-0.866i
 7 1.000 0.623+0.782i -0.223+0.975i -0.901+0.434i -0.901-0.434i -0.223-0.975i 0.623-0.782i
 8 1.000 0.707+0.707i 0.000+1.000i -0.707+0.707i -1.000+0.000i -0.707-0.707i -0.000-1.000i 0.707-0.707i
 9 1.000 0.766+0.643i 0.174+0.985i -0.500+0.866i -0.940+0.342i -0.940-0.342i -0.500-0.866i 0.174-0.985i 0.766-0.643i
10 1.000 0.809+0.588i 0.309+0.951i -0.309+0.951i -0.809+0.588i -1.000+0.000i -0.809-0.588i -0.309-0.951i 0.309-0.951i 0.809-0.588i

```



## Perl 6

Perl 6 has a built-in function <tt>cis</tt> which returns a unitary complex number given its phase.  Perl 6 also defines the <tt>tau = 2*pi</tt> constant.  Thus the k-th n-root of unity can simply be written <tt>cis(k*τ/n)</tt>.


```perl6
constant n = 10;
for ^n -> \k {
    say cis(k*τ/n);
}
```


```txt
1+0i
0.809016994374947+0.587785252292473i
0.309016994374947+0.951056516295154i
-0.309016994374947+0.951056516295154i
-0.809016994374947+0.587785252292473i
-1+1.22464679914735e-16i
-0.809016994374948-0.587785252292473i
-0.309016994374948-0.951056516295154i
0.309016994374947-0.951056516295154i
0.809016994374947-0.587785252292473i
```



## Phix

```Phix
for n=2 to 10 do
    printf(1,"%2d:",n)
    for root=0 to n-1 do
        atom real = cos(2*PI*root/n)
        atom imag = sin(2*PI*root/n)
        printf(1,"%s %6.3f %6.3fi",{iff(root?",":""),real,imag})
    end for
    printf(1,"\n")
end for
```

<pre style="font-size: 10px">
 2:  1.000  0.000i, -1.000  0.000i
 3:  1.000  0.000i, -0.500  0.866i, -0.500 -0.866i
 4:  1.000  0.000i,  0.000  1.000i, -1.000  0.000i, -0.000 -1.000i
 5:  1.000  0.000i,  0.309  0.951i, -0.809  0.588i, -0.809 -0.588i,  0.309 -0.951i
 6:  1.000  0.000i,  0.500  0.866i, -0.500  0.866i, -1.000  0.000i, -0.500 -0.866i,  0.500 -0.866i
 7:  1.000  0.000i,  0.623  0.782i, -0.223  0.975i, -0.901  0.434i, -0.901 -0.434i, -0.223 -0.975i,  0.623 -0.782i
 8:  1.000  0.000i,  0.707  0.707i,  0.000  1.000i, -0.707  0.707i, -1.000  0.000i, -0.707 -0.707i, -0.000 -1.000i,  0.707 -0.707i
 9:  1.000  0.000i,  0.766  0.643i,  0.174  0.985i, -0.500  0.866i, -0.940  0.342i, -0.940 -0.342i, -0.500 -0.866i,  0.174 -0.985i,  0.766 -0.643i
10:  1.000  0.000i,  0.809  0.588i,  0.309  0.951i, -0.309  0.951i, -0.809  0.588i, -1.000  0.000i, -0.809 -0.588i, -0.309 -0.951i,  0.309 -0.951i,  0.809 -0.588i

```



## PL/I


```PL/I
complex_roots:
   procedure (N);
   declare N fixed binary nonassignable;
   declare x float, c fixed decimal (10,8) complex;
   declare twopi float initial ((4*asin(1.0)));

   do x = 0 to twopi by twopi/N;
      c = complex(cos(x), sin(x));
      put skip list (c);
   end;
end complex_roots;

   1.00000000+0.00000000I
   0.80901700+0.58778524I
   0.30901697+0.95105654I
  -0.30901703+0.95105648I
  -0.80901706+0.58778518I
  -1.00000000-0.00000008I
  -0.80901694-0.58778536I
  -0.30901709-0.95105648I
   0.30901712-0.95105648I
   0.80901724-0.58778494I
```



## PicoLisp

```PicoLisp
(load "@lib/math.l")

(for N (range 2 10)
   (let Angle 0.0
      (prin N ": ")
      (for I N
         (let Ipart (sin Angle)
            (prin
               (round (cos Angle) 4)
               (if (lt0 Ipart) "-" "+")
               "j"
               (round (abs Ipart) 4)
               "  " ) )
         (inc 'Angle (*/ 2 pi N)) )
      (prinl) ) )
```



## PureBasic


```Purebasic
OpenConsole()
For n = 2 To 10
  angle = 0
  PrintN(Str(n))
  For i = 1 To n
    x.f = Cos(Radian(angle))
    y.f = Sin(Radian(angle))
    PrintN( Str(i) + ":  " + StrF(x, 6) +  " / " + StrF(y, 6))
    angle = angle + (360 / n)
  Next
Next
Input()
```



## Python

```python
import cmath


class Complex(complex):
    def __repr__(self):
        rp = '%7.5f' % self.real if not self.pureImag() else ''
        ip = '%7.5fj' % self.imag if not self.pureReal() else ''
        conj = '' if (
            self.pureImag() or self.pureReal() or self.imag < 0.0
        ) else '+'
        return '0.0' if (
            self.pureImag() and self.pureReal()
        ) else rp + conj + ip

    def pureImag(self):
        return abs(self.real) < 0.000005

    def pureReal(self):
        return abs(self.imag) < 0.000005


def croots(n):
    if n <= 0:
        return None
    return (Complex(cmath.rect(1, 2 * k * cmath.pi / n)) for k in range(n))
    # in pre-Python 2.6:
    #   return (Complex(cmath.exp(2j*k*cmath.pi/n)) for k in range(n))


for nr in range(2, 11):
    print(nr, list(croots(nr)))
```

```txt
2 [1.00000, -1.00000]
3 [1.00000, -0.50000+0.86603j, -0.50000-0.86603j]
4 [1.00000, 1.00000j, -1.00000, -1.00000j]
5 [1.00000, 0.30902+0.95106j, -0.80902+0.58779j, -0.80902-0.58779j, 0.30902-0.95106j]
6 [1.00000, 0.50000+0.86603j, -0.50000+0.86603j, -1.00000, -0.50000-0.86603j, 0.50000-0.86603j]
7 [1.00000, 0.62349+0.78183j, -0.22252+0.97493j, -0.90097+0.43388j, -0.90097-0.43388j, -0.22252-0.97493j, 0.62349-0.78183j]
8 [1.00000, 0.70711+0.70711j, 1.00000j, -0.70711+0.70711j, -1.00000, -0.70711-0.70711j, -1.00000j, 0.70711-0.70711j]
9 [1.00000, 0.76604+0.64279j, 0.17365+0.98481j, -0.50000+0.86603j, -0.93969+0.34202j, -0.93969-0.34202j, -0.50000-0.86603j, 0.17365-0.98481j, 0.76604-0.64279j]
10 [1.00000, 0.80902+0.58779j, 0.30902+0.95106j, -0.30902+0.95106j, -0.80902+0.58779j, -1.00000, -0.80902-0.58779j, -0.30902-0.95106j, 0.30902-0.95106j, 0.80902-0.58779j]
```



## R


```R
for(j in 2:10) {
  r <- sprintf("%d: ", j)
  for(n in 1:j) {
    r <- paste(r, format(exp(2i*pi*n/j), digits=4), ifelse(n<j, ",", ""))
  }
  print(r)
}
```

Output:

```txt

[1] "2:  -1+0i , 1-0i "
[1] "3:  -0.5+0.866i , -0.5-0.866i , 1-0i "
[1] "4:  0+1i , -1+0i , 0-1i , 1-0i "
[1] "5:  0.309+0.9511i , -0.809+0.5878i , -0.809-0.5878i , 0.309-0.9511i , 1-0i "
[1] "6:  0.5+0.866i , -0.5+0.866i , -1+0i , -0.5-0.866i , 0.5-0.866i , 1-0i "
[1] "7:  0.6235+0.7818i , -0.2225+0.9749i , -0.901+0.4339i , -0.901-0.4339i , -0.2225-0.9749i , 0.6235-0.7818i , 1-0i "
[1] "8:  0.7071+0.7071i , 0+1i , -0.7071+0.7071i , -1+0i , -0.7071-0.7071i , 0-1i , 0.7071-0.7071i , 1-0i "
[1] "9:  0.766+0.6428i , 0.1736+0.9848i , -0.5+0.866i , -0.9397+0.342i , -0.9397-0.342i , -0.5-0.866i , 0.1736-0.9848i , 0.766-0.6428i , 1-0i "
[1] "10:  0.809+0.5878i , 0.309+0.9511i , -0.309+0.9511i , -0.809+0.5878i , -1+0i , -0.809-0.5878i , -0.309-0.9511i , 0.309-0.9511i , 0.809-0.5878i , 1-0i "

```



## Racket


```Racket
#lang racket

(define (roots-of-unity n)
  (for/list ([k n])
    (make-polar 1 (* k (/ (* 2 pi) n)))))
```

Will produce a list of roots, for example:

```txt

> (for ([r (roots-of-unity 3)]) (displayln r))
1
-0.4999999999999998+0.8660254037844388i
-0.5000000000000004-0.8660254037844384i
```



## REXX

REXX doesn't have complex arithmetic, so the (real) values of   '''cos'''   and   '''sin'''   of multiples of   <big>2 pi</big>   radians (divided by K) are used.

Also, REXX doesn't have the   '''pi'''   constant defined, nor a   '''sin'''   or   '''cos'''   function, so they are included below within the REXX program.

Note:   this REXX version only   ''displays''   '''5'''   significant digits past the decimal point,   but this can be overridden by specifying the 2<sup>nd</sup> argument when invoking the REXX program.
(See the value of the REXX variable   '''frac''',   4<sup>th</sup> line).

```rexx
/*REXX program computes the  K  roots of  unity  (which usually includes complex roots).*/
parse arg n frac .                               /*get optional arguments from the C.L. */
if   n=='' |    n==","  then     n= 1            /*Not specified?  Then use the default.*/
if frac='' | frac==","  then  frac= 5            /* "      "         "   "   "     "    */
start= abs(n)                                    /*assume only one  K  is wanted.       */
if n<0                  then start= 1            /*Negative?  Then use a range of  K's. */
numeric digits length( pi() )  - 1               /*use number of decimal digits in  pi. */
pi2= pi + pi                                     /*obtain the value of   pi  doubled.   */
                                                 /*display unity roots for a range,  or */
     do #=start  to abs(n)                       /*                   just for one  K.  */
     say right(# 'roots of unity', 40, "─")  ' (showing' frac "fractional decimal digits)"
         do angle=0  by pi2/#  for #             /*compute the angle for each root.     */
         rp= adjust( cos(angle) )                /*compute real part via  COS  function.*/
         if left(rp, 1) \== '-'  then rp=" "rp   /*not negative?  Then pad with a blank.*/
         ip= adjust( sin(angle) )                /*compute imaginary part via SIN funct.*/
         if left(ip, 1) \== '-'  then ip="+"ip   /*Not negative?  Then pad with  + char.*/
         if ip=0  then say rp                    /*Only real part? Ignore imaginary part*/
                  else say left(rp, frac+4)ip'i' /*display the real and imaginary part. */
         end  /*angle*/
     end      /*#*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
pi:  pi=3.141592653589793238462643383279502884197169399375105820974944592307816; return pi
r2r: return arg(1)   //   ( pi() * 2 )           /*reduce #radians: -2pi──► +2pi radians*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
adjust: parse arg x; near0= '1e-' || (digits() - digits() % 10) /*compute a tiny number.*/
        if abs(x) < near0  then x= 0             /*if it's near zero,  then assume zero.*/
        return format(x, , frac)   /   1         /*fraction digits past decimal point.  */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cos: procedure; parse arg x;     x= r2r(x);   a= abs(x);   numeric fuzz min(9, digits()-9)
     if a=pi/3  then return .5;  if a=pi/2|a=pi*2  then return 0
     if a=pi    then return -1;  if a=pi*2/3  then return -.5;  z=1;  _=1;       $x= x * x
       do k=2  by 2  until p=z;  p=z;  _= - _*$x / (k*(k-1));   z= z+ _;  end;   return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
sin: procedure; parse arg x;     x= r2r(x);                numeric fuzz min(5, digits()-3)
     if abs(x)=pi  then return 0;                               z= x; _=x;       $x= x * x
       do k=2  by 2  until p=z;  p=z;  _= - _*$x / (k*(k+1));   z= z+ _;  end;   return z
```

```txt

────────────────────────5 roots of unity  (showing 5 fractional decimal digits)
 1
 0.30902 +0.95106i
-0.80902 +0.58779i
-0.80902 -0.58779i
 0.30902 -0.95106i

```

```txt

───────────────────────10 roots of unity  (showing 36 fractional decimal digits)
 1
 0.809016994374947424102293417182819059 +0.587785252292473129168705954639072769i
 0.309016994374947424102293417182819059 +0.951056516295153572116439333379382143i
-0.309016994374947424102293417182819059 +0.951056516295153572116439333379382143i
-0.809016994374947424102293417182819059 +0.587785252292473129168705954639072769i
-1
-0.809016994374947424102293417182819059 -0.587785252292473129168705954639072769i
-0.309016994374947424102293417182819059 -0.951056516295153572116439333379382143i
 0.309016994374947424102293417182819059 -0.951056516295153572116439333379382143i
 0.809016994374947424102293417182819059 -0.587785252292473129168705954639072769i

```

(Shown at five-sixths size.)

<pre style="font-size:84%;height:85ex">
────────────────────────1 roots of unity  (showing 5 fractional decimal digits)
 1
────────────────────────2 roots of unity  (showing 5 fractional decimal digits)
 1
-1
────────────────────────3 roots of unity  (showing 5 fractional decimal digits)
 1
-0.5     +0.86603i
-0.5     -0.86603i
────────────────────────4 roots of unity  (showing 5 fractional decimal digits)
 1
 0       +1i
-1
 0       -1i
────────────────────────5 roots of unity  (showing 5 fractional decimal digits)
 1
 0.30902 +0.95106i
-0.80902 +0.58779i
-0.80902 -0.58779i
 0.30902 -0.95106i
────────────────────────6 roots of unity  (showing 5 fractional decimal digits)
 1
 0.5     +0.86603i
-0.5     +0.86603i
-1
-0.5     -0.86603i
 0.5     -0.86603i
────────────────────────7 roots of unity  (showing 5 fractional decimal digits)
 1
 0.62349 +0.78183i
-0.22252 +0.97493i
-0.90097 +0.43388i
-0.90097 -0.43388i
-0.22252 -0.97493i
 0.62349 -0.78183i
────────────────────────8 roots of unity  (showing 5 fractional decimal digits)
 1
 0.70711 +0.70711i
 0       +1i
-0.70711 +0.70711i
-1
-0.70711 -0.70711i
 0       -1i
 0.70711 -0.70711i
────────────────────────9 roots of unity  (showing 5 fractional decimal digits)
 1
 0.76604 +0.64279i
 0.17365 +0.98481i
-0.5     +0.86603i
-0.93969 +0.34202i
-0.93969 -0.34202i
-0.5     -0.86603i
 0.17365 -0.98481i
 0.76604 -0.64279i
───────────────────────10 roots of unity  (showing 5 fractional decimal digits)
 1
 0.80902 +0.58779i
 0.30902 +0.95106i
-0.30902 +0.95106i
-0.80902 +0.58779i
-1
-0.80902 -0.58779i
-0.30902 -0.95106i
 0.30902 -0.95106i
 0.80902 -0.58779i
───────────────────────11 roots of unity  (showing 5 fractional decimal digits)
 1
 0.84125 +0.54064i
 0.41542 +0.90963i
-0.14231 +0.98982i
-0.65486 +0.75575i
-0.95949 +0.28173i
-0.95949 -0.28173i
-0.65486 -0.75575i
-0.14231 -0.98982i
 0.41542 -0.90963i
 0.84125 -0.54064i
───────────────────────12 roots of unity  (showing 5 fractional decimal digits)
 1
 0.86603 +0.5i
 0.5     +0.86603i
 0       +1i
-0.5     +0.86603i
-0.86603 +0.5i
-1
-0.86603 -0.5i
-0.5     -0.86603i
 0       -1i
 0.5     -0.86603i
 0.86603 -0.5i

```



## Ring


```ring

decimals(4)
for n = 2 to 5
    see string(n) + " : "
    for root = 0 to n-1
        real = cos(2*3.14 * root / n)
        imag = sin(2*3.14 * root / n)
        see "" + real + " " + imag + "i"
        if root != n-1 see ", " ok
    next
    see nl
next

```



## RLaB

RLaB can find the n-roots of unity by solving the polynomial equation
:<math>x^n - 1 = 0.</math>
It uses the solver ''polyroots''. Interested user is recommended to check the rlabplus manual for details on the solver and the parameters that tune the solver performance.

```RLaB
// specify polynomial
>> n = 10;
>> a = zeros(1,n+1); a[1] = 1; a[n+1] = -1;
>> polyroots(a)
   radius               roots           success
>> polyroots(a).roots
   -0.309016994 + 0.951056516i
   -0.809016994 + 0.587785252i
          -1 + 5.95570041e-23i
   -0.809016994 - 0.587785252i
   -0.309016994 - 0.951056516i
    0.309016994 - 0.951056516i
    0.809016994 - 0.587785252i
                        1 + 0i
    0.809016994 + 0.587785252i
    0.309016994 + 0.951056516i
```



## Ruby


```ruby
def roots_of_unity(n)
  (0...n).map {|k| Complex.polar(1, 2 * Math::PI * k / n)}
end

p roots_of_unity(3)
```


```txt

 [(1+0.0i), (-0.4999999999999998+0.8660254037844387i), (-0.5000000000000004-0.8660254037844384i)]

```



## Run BASIC


```runbasic
PI = 3.1415926535
FOR n = 2 TO 5
  PRINT n;":" ;
   FOR root = 0 TO n-1
     real = COS(2*PI * root / n)
     imag = SIN(2*PI * root / n)
     PRINT using("-##.#####",real);using("-##.#####",imag);"i";
     IF root <> n-1 then PRINT "," ;
  NEXT
  PRINT
NEXT

```

Output:

```txt

2:   1.00000   0.00000i, -1.00000   0.00000i
3:   1.00000   0.00000i, -0.50000   0.86603i, -0.50000  -0.86603i
4:   1.00000   0.00000i,  0.00000   1.00000i, -1.00000   0.00000i,  0.00000  -1.00000i
5:   1.00000   0.00000i,  0.30902   0.95106i, -0.80902   0.58779i, -0.80902  -0.58779i,  0.30902  -0.95106i
```




## Rust

Here we demonstrate initialization from polar complex coordinate, radius 1, e^πi/n, and raising the resulting complex number to the power 2k for k in 0..n-1, which generates approximate roots (see the Mathematica answer for a nice display of exact vs approximate). This code will require adding the num crate to one's rust project, typically in Cargo.toml <i>[dependencies] \n num="0.2.0";</i>

```C
use num::Complex;
fn main() {
    let n = 8;
    let z = Complex::from_polar(&1.0,&(1.0*std::f64::consts::PI/n as f64));
    for k in 0..=n-1 {
        println!("e^{:2}πi/{} ≈ {:>14.3}",2*k,n,z.powf(2.0*k as f64));
    }
}
```


```txt

e^ 0πi/8 ≈   1.000+0.000i
e^ 2πi/8 ≈   0.707+0.707i
e^ 4πi/8 ≈   0.000+1.000i
e^ 6πi/8 ≈  -0.707+0.707i
e^ 8πi/8 ≈  -1.000+0.000i
e^10πi/8 ≈  -0.707-0.707i
e^12πi/8 ≈  -0.000-1.000i
e^14πi/8 ≈   0.707-0.707i

```



## Scala

Using [[Arithmetic/Complex#Scala|Complex]] class from task Arithmetic/Complex.

```scala
def rootsOfUnity(n:Int)=for(k <- 0 until n) yield Complex.fromPolar(1.0, 2*math.Pi*k/n)
```

Usage:

```txt
rootsOfUnity(3) foreach println

1.0+0.0i
-0.4999999999999998+0.8660254037844387i
-0.5000000000000004-0.8660254037844385i

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "complex.s7i";

const proc: main is func
  local
    var integer: n is 0;
    var integer: k is 0;
  begin
    for n range 2 to 10 do
      write(n lpad 2 <& ": ");
      for k range 0 to pred(n) do
        write(polar(1.0, 2.0 * PI * flt(k) / flt(n)) digits 4 lpad 15 <& " ");
      end for;
      writeln;
    end for;
  end func;
```

Output:

```seed7
2:  1.0000+0.0000i -1.0000+0.0000i
 3:  1.0000+0.0000i -0.5000+0.8660i -0.5000-0.8660i
 4:  1.0000+0.0000i  0.0000+1.0000i -1.0000+0.0000i  0.0000-1.0000i
 5:  1.0000+0.0000i  0.3090+0.9511i -0.8090+0.5878i -0.8090-0.5878i  0.3090-0.9511i
 6:  1.0000+0.0000i  0.5000+0.8660i -0.5000+0.8660i -1.0000+0.0000i -0.5000-0.8660i  0.5000-0.8660i
 7:  1.0000+0.0000i  0.6235+0.7818i -0.2225+0.9749i -0.9010+0.4339i -0.9010-0.4339i -0.2225-0.9749i  0.6235-0.7818i
 8:  1.0000+0.0000i  0.7071+0.7071i  0.0000+1.0000i -0.7071+0.7071i -1.0000+0.0000i -0.7071-0.7071i  0.0000-1.0000i  0.7071-0.7071i
 9:  1.0000+0.0000i  0.7660+0.6428i  0.1736+0.9848i -0.5000+0.8660i -0.9397+0.3420i -0.9397-0.3420i -0.5000-0.8660i  0.1736-0.9848i  0.7660-0.6428i
10:  1.0000+0.0000i  0.8090+0.5878i  0.3090+0.9511i -0.3090+0.9511i -0.8090+0.5878i -1.0000+0.0000i -0.8090-0.5878i -0.3090-0.9511i  0.3090-0.9511i  0.8090-0.5878i
```



## Scheme


```scheme
(define pi (* 4 (atan 1)))

(do ((n 2 (+ n 1)))
    ((> n 10))
    (display n)
    (do ((k 0 (+ k 1)))
        ((>= k n))
        (display " ")
        (display (make-polar 1 (* 2 pi (/ k n)))))
    (newline))
```



## Sidef

```ruby
func roots_of_unity(n) {
    n.of { |j|
        exp(2i * Num.pi / n * j)
    }
}

roots_of_unity(5).each { |c|
    printf("%+.5f%+.5fi\n", c.reals)
}
```

```txt

+1.00000+0.00000i
+0.30902+0.95106i
-0.80902+0.58779i
-0.80902-0.58779i
+0.30902-0.95106i

```



## Sparkling


```sparkling
function unity_roots(n) {
	// nth-root(1) = cos(2 * k * pi / n) + i * sin(2 * k * pi / n)
	return map(range(n), function(idx, k) {
		return {
			"re": cos(2 * k * M_PI / n),
			"im": sin(2 * k * M_PI / n)
		};
	});
}

// pirnt 6th roots of unity
foreach(unity_roots(6), function(k, v) {
	printf("%.3f%+.3fi\n", v.re, v.im);
});
```



## Stata



```stata
n=7
exp(2i*pi()/n*(0::n-1))
                               1
    +-----------------------------+
  1 |                          1  |
  2 |   .623489802 + .781831482i  |
  3 |  -.222520934 + .974927912i  |
  4 |  -.900968868 + .433883739i  |
  5 |  -.900968868 - .433883739i  |
  6 |  -.222520934 - .974927912i  |
  7 |   .623489802 - .781831482i  |
    +-----------------------------+
```



## Tcl


```Tcl
package require Tcl 8.5
namespace import tcl::mathfunc::*

set pi 3.14159265
for {set n 2} {$n <= 10} {incr n} {
    set angle 0.0
    set row $n:
    for {set i 1} {$i <= $n} {incr i} {
        lappend row [format %5.4f%+5.4fi [cos $angle] [sin $angle]]
        set angle [expr {$angle + 2*$pi/$n}]
    }
    puts $row
}
```


=={{header|TI-89 BASIC}}==

```ti89b
cZeros(x^n - 1, x)
```

For n=3 in exact mode, the results are

```ti89b
{-1/2+√(3)/2*i, -1/2-√(3)/2*i, 1}
```



## Ursala

The roots function takes a number n to the nth root of -1, squares it, and iteratively makes a list of its first n powers (oblivious to roundoff error). Complex functions cpow and mul are used, which are called from the host system's standard C library.

```Ursala
#import std
#import nat
#import flo

roots = ~&htxPC+ c..mul:-0^*DlSiiDlStK9\iota c..mul@iiX+ c..cpow/-1.+ div/1.+ float

#cast %jLL

tests = roots* <1,2,3,4,5,6>
```

The output is a list of lists of complex numbers.

```txt

<
   <1.000e+00-2.449e-16j>,
   <
      1.000e+00-2.449e-16j,
      -1.000e+00+1.225e-16j>,
   <
      1.000e+00-8.327e-16j,
      -5.000e-01+8.660e-01j,
      -5.000e-01-8.660e-01j>,
   <
      1.000e+00-8.882e-16j,
      2.220e-16+1.000e+00j,
      -1.000e+00+4.441e-16j,
      -6.661e-16-1.000e+00j>,
   <
      1.000e+00-5.551e-17j,
      3.090e-01+9.511e-01j,
      -8.090e-01+5.878e-01j,
      -8.090e-01-5.878e-01j,
      3.090e-01-9.511e-01j>,
   <
      1.000e+00-1.221e-15j,
      5.000e-01+8.660e-01j,
      -5.000e-01+8.660e-01j,
      -1.000e+00+6.106e-16j,
      -5.000e-01-8.660e-01j,
      5.000e-01-8.660e-01j>>
```



## VBA


```vb
Public Sub roots_of_unity()
    For n = 2 To 9
        Debug.Print n; "th roots of 1:"
        For r00t = 0 To n - 1
            Debug.Print "   Root "; r00t & ": "; WorksheetFunction.Complex(Cos(2 * WorksheetFunction.Pi() * r00t / n), _
                Sin(2 * WorksheetFunction.Pi() * r00t / n))
        Next r00t
        Debug.Print
    Next n
End Sub
```
```txt
 2 th roots of 1:
   Root 0: 1
   Root 1: -1+1.22460635382238E-16i

 3 th roots of 1:
   Root 0: 1
   Root 1: -0.5+0.866025403784439i
   Root 2: -0.5-0.866025403784438i

 4 th roots of 1:
   Root 0: 1
   Root 1: 6.12303176911189E-17+i
   Root 2: -1+1.22460635382238E-16i
   Root 3: -1.83690953073357E-16-i

 5 th roots of 1:
   Root 0: 1
   Root 1: 0.309016994374947+0.951056516295154i
   Root 2: -0.809016994374947+0.587785252292473i
   Root 3: -0.809016994374948-0.587785252292473i
   Root 4: 0.309016994374947-0.951056516295154i

 6 th roots of 1:
   Root 0: 1
   Root 1: 0.5+0.866025403784439i
   Root 2: -0.5+0.866025403784439i
   Root 3: -1+1.22460635382238E-16i
   Root 4: -0.5-0.866025403784438i
   Root 5: 0.5-0.866025403784439i

 7 th roots of 1:
   Root 0: 1
   Root 1: 0.623489801858734+0.78183148246803i
   Root 2: -0.222520933956314+0.974927912181824i
   Root 3: -0.900968867902419+0.433883739117558i
   Root 4: -0.900968867902419-0.433883739117558i
   Root 5: -0.222520933956315-0.974927912181824i
   Root 6: 0.623489801858733-0.78183148246803i

 8 th roots of 1:
   Root 0: 1
   Root 1: 0.707106781186548+0.707106781186547i
   Root 2: 6.12303176911189E-17+i
   Root 3: -0.707106781186547+0.707106781186548i
   Root 4: -1+1.22460635382238E-16i
   Root 5: -0.707106781186548-0.707106781186547i
   Root 6: -1.83690953073357E-16-i
   Root 7: 0.707106781186547-0.707106781186548i

 9 th roots of 1:
   Root 0: 1
   Root 1: 0.766044443118978+0.642787609686539i
   Root 2: 0.17364817766693+0.984807753012208i
   Root 3: -0.5+0.866025403784439i
   Root 4: -0.939692620785908+0.342020143325669i
   Root 5: -0.939692620785908-0.342020143325669i
   Root 6: -0.5-0.866025403784438i
   Root 7: 0.17364817766693-0.984807753012208i
   Root 8: 0.766044443118978-0.64278760968654i
```


## zkl

```zkl
PI2:=(0.0).pi*2;
foreach n,i in ([1..9],n){
   c:=s:=0;
   if(not i)         c =  1;
   else if(n==4*i)   s =  1;
   else if(n==2*i)   c = -1;
   else if(3*n==4*i) s = -1;
   else a,c,s:=PI2*i/n,a.cos(),a.sin();

   if(c) print("%.2g".fmt(c));
   print( (s==1 and "i") or (s==-1 and "-i" or (s and "%+.2gi" or"")).fmt(s));
   print( (i==n-1) and "\n" or ",  ");
}
```

```txt

1
1,  -1
1,  -0.5+0.87i,  -0.5-0.87i
1,  i,  -1,  -i
1,  0.31+0.95i,  -0.81+0.59i,  -0.81-0.59i,  0.31-0.95i
1,  0.5+0.87i,  -0.5+0.87i,  -1,  -0.5-0.87i,  0.5-0.87i
1,  0.62+0.78i,  -0.22+0.97i,  -0.9+0.43i,  -0.9-0.43i,  -0.22-0.97i,  0.62-0.78i
1,  0.71+0.71i,  i,  -0.71+0.71i,  -1,  -0.71-0.71i,  -i,  0.71-0.71i
1,  0.77+0.64i,  0.17+0.98i,  -0.5+0.87i,  -0.94+0.34i,  -0.94-0.34i,  -0.5-0.87i,  0.17-0.98i,  0.77-0.64i

```

