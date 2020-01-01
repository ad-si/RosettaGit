+++
title = "Greatest common divisor"
description = ""
date = 2019-10-21T17:09:00Z
aliases = []
[extra]
id = 2422
task = "Find the greatest common divisor of two integers."
[taxonomies]
categories = []
tags = ["math", "recursion"]
+++

## 360 Assembly

Translated from FORTRAN.

For maximum compatibility, this program uses only the basic instruction set (S/360)
with 2 ASSIST macros (XDECO,XPRNT).

```360asm
*        Greatest common divisor   04/05/2016
GCD      CSECT
         USING  GCD,R15            use calling register
         L      R6,A               u=a
         L      R7,B               v=b
LOOPW    LTR    R7,R7              while v<>0
         BZ     ELOOPW               leave while
         LR     R8,R6                t=u
         LR     R6,R7                u=v
         LR     R4,R8                t
         SRDA   R4,32                shift to next reg
         DR     R4,R7                t/v
         LR     R7,R4                v=mod(t,v)
         B      LOOPW              end while
ELOOPW   LPR    R9,R6              c=abs(u)
         L      R1,A               a
         XDECO  R1,XDEC            edit a
         MVC    PG+4(5),XDEC+7     move a to buffer
         L      R1,B               b
         XDECO  R1,XDEC            edit b
         MVC    PG+10(5),XDEC+7    move b to buffer
         XDECO  R9,XDEC            edit c
         MVC    PG+17(5),XDEC+7    move c to buffer
         XPRNT  PG,80              print buffer
         XR     R15,R15            return code =0
         BR     R14                return to caller
A        DC     F'1071'            a
B        DC     F'1029'            b
PG       DC     CL80'gcd(00000,00000)=00000'  buffer
XDEC     DS     CL12               temp for edit
         YREGS
         END    GCD
```

Output:

```txt

gcd( 1071, 1029)=   21

```


## 8th

```forth
: gcd \ a b -- gcd
	dup 0 n:= if drop ;; then
	tuck \ b a b
	n:mod \ b a-mod-b
	recurse ;

: demo \ a b --
	2dup "GCD of " . . " and " . . " = " . gcd . ;

100    5 demo cr
  5  100 demo cr
  7   23 demo cr

bye
```

Output:

```txt
GCD of 5 and 100 = 5
GCD of 100 and 5 = 5
GCD of 23 and 7 = 1
```


## ACL2

```Lisp
(include-book "arithmetic-3/floor-mod/floor-mod" :dir :system)

(defun gcd$ (x y)
   (declare (xargs :guard (and (natp x) (natp y))))
   (cond ((or (not (natp x)) (< y 0))
          nil)
         ((zp y) x)
         (t (gcd$ y (mod x y)))))
```


## ActionScript

```ActionScript
//Euclidean algorithm
function gcd(a:int,b:int):int
{
	var tmp:int;
	//Swap the numbers so a >= b
	if(a < b)
	{
		tmp = a;
		a = b;
		b = tmp;
	}
	//Find the gcd
	while(b != 0)
	{
		tmp = a % b;
		a = b;
		b = tmp;
	}
	return a;
}
```


## Ada

```ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Gcd_Test is
   function Gcd (A, B : Integer) return Integer is
      M : Integer := A;
      N : Integer := B;
      T : Integer;
   begin
      while N /= 0 loop
         T := M;
         M := N;
         N := T mod N;
      end loop;
      return M;
   end Gcd;

begin
   Put_Line("GCD of 100, 5 is" & Integer'Image(Gcd(100, 5)));
   Put_Line("GCD of 5, 100 is" & Integer'Image(Gcd(5, 100)));
   Put_Line("GCD of 7, 23 is" & Integer'Image(Gcd(7, 23)));
end Gcd_Test;
```

Output:

```txt
GCD of 100, 5 is 5
GCD of 5, 100 is 5
GCD of 7, 23 is 1
```


## Aime

```aime
o_integer(gcd(33, 77));
o_byte('\n');
o_integer(gcd(49865, 69811));
o_byte('\n');
```


## ALGOL 68

Works with ALGOL 68|Revision 1 - no extensions to language used

Works with ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
PROC gcd = (INT a, b) INT: (
  IF a = 0 THEN
    b
  ELIF b = 0 THEN
    a
  ELIF a > b  THEN
    gcd(b, a MOD b)
  ELSE
    gcd(a, b MOD a)
  FI
);
test:(
  INT a = 33, b = 77;
  printf(($x"The gcd of"g" and "g" is "gl$,a,b,gcd(a,b)));
  INT c = 49865, d = 69811;
  printf(($x"The gcd of"g" and "g" is "gl$,c,d,gcd(c,d)))
)
```

Output:

```txt
The gcd of        +33 and         +77 is         +11
The gcd of     +49865 and      +69811 is       +9973
```


## ALGOL W

```algolw
begin
    % iterative Greatest Common Divisor routine                               %
    integer procedure gcd ( integer value m, n ) ;
    begin
        integer a, b, newA;
        a := abs( m );
        b := abs( n );
        if a = 0 then begin
            b
            end
        else begin
            while b not = 0 do begin
                newA := b;
                b    := a rem b;
                a    := newA;
            end;
            a
        end
    end gcd ;

    write( gcd( -21, 35 ) );
end.
```


## Alore

```Alore
def gcd(a as Int, b as Int) as Int
   while b != 0
      a,b = b, a mod b
   end
   return Abs(a)
end
```


## AntLang

AntLang has a built-in gcd function.

```AntLang
gcd[33; 77]
```

It is not recommended, but possible to implement it on your own.

```AntLang
/Unoptimized version
gcd':{a:x;b:y;last[{(0 eq a mod x) min (0 eq b mod x)} hfilter {1 + x} map range[a max b]]}
```

## APL

Works with Dyalog APL

```apl
        33 49865 ∨ 77 69811
 11 9973
```

If you're interested in how you'd write GCD in Dyalog,
if Dyalog didn't have a primitive for it,
(i.e. using other algorithms mentioned on this page: iterative, recursive, binary recursive),
see [http://www.dyalog.com/dfnsdws/n_gcd.htm different ways to write GCD in Dyalog].

Works with APL2

```apl
        ⌈/(^/0=A∘.|X)/A←⍳⌊/X←49865 69811
 9973
```


## AppleScript

By recursion:

```AppleScript
-- gcd :: Int -> Int -> Int
on gcd(a, b)
    if b ≠ 0 then
        gcd(b, a mod b)
    else
        if a < 0 then
            -a
        else
            a
        end if
    end if
end gcd

```


## Applesoft BASIC

```ApplesoftBasic
0 A = ABS(INT(A))
1 B = ABS(INT(B))
2 GCD = A * NOT NOT B
3 FOR B = B + A * NOT B TO 0 STEP 0
4     A = GCD
5     GCD = B
6     B = A - INT (A / GCD) * GCD
7 NEXT B
```


## Arendelle

```arendelle
&lt; a , b &gt;

( r , @a )

[ @r != 0 ,

        ( r , @a % @b )

        { @r != 0 ,

                ( a , @b )
                ( b , @r )

        }
]

( return , @b )
```


## Arturo

```arturo
print $(gcd #(10 15))
```

Output:

```txt
5
```


## AutoHotkey

Contributed by Laszlo on the ahk
[forum](http://www.autohotkey.com/forum/post-276379.html#276379)

```AutoHotkey
GCD(a,b) {
   Return b=0 ? Abs(a) : Gcd(b,mod(a,b))
}
```

Significantly faster than recursion:

```AutoHotkey
GCD(a, b) {
    while b
        b := Mod(a | 0x0, a := b)
    return a
}
```


## AutoIt

```autoit
_GCD(18, 12)
_GCD(1071, 1029)
_GCD(3528, 3780)

Func _GCD($ia, $ib)
	Local $ret = "GCD of " & $ia & " : " & $ib & " = "
	Local $imod
	While True
		$imod = Mod($ia, $ib)
		If $imod = 0 Then Return ConsoleWrite($ret & $ib & @CRLF)
		$ia = $ib
		$ib = $imod
	WEnd
EndFunc   ;==>_GCD
```

Output:

```txt
GCD of 18 : 12 = 6
GCD of 1071 : 1029 = 21
GCD of 3528 : 3780 = 252
```


## AWK

The following scriptlet defines the gcd() function,
then reads pairs of numbers from stdin, and reports their gcd on stdout.

```awk
$ awk 'function gcd(p,q){return(q?gcd(q,(p%q)):p)}{print gcd($1,$2)}'
12 16
4
22 33
11
45 67
1
```


## Axe

```axe
Lbl GCD
r₁→A
r₂→B
!If B
 A
 Return
End
GCD(B,A^B)
```


## Batch File

Recursive method

```dos
:: gcd.cmd
@echo off
:gcd
if "%2" equ "" goto :instructions
if "%1" equ "" goto :instructions

if %2 equ 0 (
	set final=%1
	goto :done
)
set /a res = %1 %% %2
call :gcd %2 %res%
goto :eof

:done
echo gcd=%final%
goto :eof

:instructions
echo Syntax:
echo 	GCD {a} {b}
echo.
```


## BASIC

Works with QuickBasic 4.5


### Iterative


```qbasic
function gcd(a%, b%)
   if a > b then
      factor = a
   else
      factor = b
   end if
   for l = factor to 1 step -1
      if a mod l = 0 and b mod l = 0 then
         gcd = l
      end if
   next l
   gcd = 1
end function
```


### Recursive


```qbasic
function gcd(a%, b%)
   if a = 0  gcd = b
   if b = 0  gcd = a
   if a > b  gcd = gcd(b, a mod b)
   gcd = gcd(a, b mod a)
end function
```


## IS-BASIC

```IS-BASIC
100 DEF GCD(A,B)
110   DO WHILE B>0
120     LET T=B
130     LET B=MOD(A,B)
140     LET A=T
150   LOOP
160   LET GCD=A
170 END DEF
180 PRINT GCD(12,16)
```


## Sinclair ZX81 BASIC

```basic
 10 LET M=119
 20 LET N=544
 30 LET R=M-N*INT (M/N)
 40 IF R=0 THEN GOTO 80
 50 LET M=N
 60 LET N=R
 70 GOTO 30
 80 PRINT N
```

Output:

```txt
17
```


## BBC BASIC

```bbcbasic
      DEF FN_GCD_Iterative_Euclid(A%, B%)
      LOCAL C%
      WHILE B%
        C% = A%
        A% = B%
        B% = C% MOD B%
      ENDWHILE
      = ABS(A%)
```


## Bc

Works with GNU bc.
Translated from C.

Utility functions:

```bc
define even(a)
{
  if ( a % 2 == 0 ) {
    return(1);
  } else {
    return(0);
  }
}

define abs(a)
{
  if (a<0) {
    return(-a);
  } else {
    return(a);
  }
}
```

'''Iterative (Euclid)'''

```bc
define gcd_iter(u, v)
{
  while(v) {
    t = u;
    u = v;
    v = t % v;
  }
  return(abs(u));
}
```

'''Recursive'''

```bc
define gcd(u, v)
{
  if (v) {
    return ( gcd(v, u%v) );
  } else {
    return (abs(u));
  }
}
```

'''Iterative (Binary)'''

```bc
define gcd_bin(u, v)
{
  u = abs(u);
  v = abs(v);
  if ( u < v ) {
    t = u; u = v; v = t;
  }
  if ( v == 0 ) { return(u); }
  k = 1;
  while (even(u) && even(v)) {
    u = u / 2; v = v / 2;
    k = k * 2;
  }
  if ( even(u) ) {
    t = -v;
  } else {
    t = u;
  }
  while (t) {
    while (even(t)) {
      t = t / 2;
    }

    if (t > 0) {
      u = t;
    } else {
      v = -t;
    }
    t = u - v;
  }
  return (u * k);
}
```


## Befunge

```befunge
#v&<     @.$<
:<\g05%p05:_^#
```


## Bracmat

Bracmat uses the Euclidean algorithm to simplify fractions.
The `den` function extracts the denominator from a fraction.

```bracmat
(gcd=a b.!arg:(?a.?b)&!b*den$(!a*!b^-1)^-1);
```

Example:

```txt
{?} gcd$(49865.69811)
{!} 9973
```


## C

### Iterative Euclid algorithm

```c
int
gcd_iter(int u, int v) {
  if (u < 0) u = -u;
  if (v < 0) v = -v;
  if (v) while ((u %= v) && (v %= u));
  return (u + v);
}
```


### Recursive Euclid algorithm

```c
int gcd(int u, int v) {
return (v != 0)?gcd(v, u%v):u;
}
```


### Iterative binary algorithm

```c
int
gcd_bin(int u, int v) {
  int t, k;

  u = u < 0 ? -u : u; /* abs(u) */
  v = v < 0 ? -v : v;
  if (u < v) {
    t = u;
    u = v;
    v = t;
  }
  if (v == 0)
    return u;

  k = 1;
  while (u & 1 == 0 && v & 1 == 0) { /* u, v - even */
    u >>= 1; v >>= 1;
    k <<= 1;
  }

  t = (u & 1) ? -v : u;
  while (t) {
    while (t & 1 == 0)
      t >>= 1;

    if (t > 0)
      u = t;
    else
      v = -t;

    t = u - v;
  }
  return u * k;
}
```


## C++

```c++
#include <iostream

#include <numeric>

int main() {
    std::cout << "The greatest common divisor of 12 and 18 is " << std::gcd(12, 18) << " !\n";
}
```

```c++
#include <boost/math/common_factor.hpp>
#include <iostream>

int main() {
   std::cout << "The greatest common divisor of 12 and 18 is " << boost::math::gcd(12, 18) << "!\n";
}
```


Output:

```txt
The greatest common divisor of 12 and 18 is 6!
```


## C\#

### Iterative


```c#
static void Main()
{
	Console.WriteLine("GCD of {0} and {1} is {2}", 1, 1, gcd(1, 1));
	Console.WriteLine("GCD of {0} and {1} is {2}", 1, 10, gcd(1, 10));
	Console.WriteLine("GCD of {0} and {1} is {2}", 10, 100, gcd(10, 100));
	Console.WriteLine("GCD of {0} and {1} is {2}", 5, 50, gcd(5, 50));
	Console.WriteLine("GCD of {0} and {1} is {2}", 8, 24, gcd(8, 24));
	Console.WriteLine("GCD of {0} and {1} is {2}", 36, 17, gcd(36, 17));
	Console.WriteLine("GCD of {0} and {1} is {2}", 36, 18, gcd(36, 18));
	Console.WriteLine("GCD of {0} and {1} is {2}", 36, 19, gcd(36, 19));
	for (int x = 1; x < 36; x++)
	{
		Console.WriteLine("GCD of {0} and {1} is {2}", 36, x, gcd(36, x));
	}
	Console.Read();
}

/// <summary>
/// Greatest Common Denominator using Euclidian Algorithm
/// </summary>
static int gcd(int a, int b)
{
    while (b != 0) b = a % (a = b);
    return a;
}
```


Example output:

```txt
GCD of 1 and 1 is 1
GCD of 1 and 10 is 1
GCD of 10 and 100 is 10
GCD of 5 and 50 is 5
GCD of 8 and 24 is 8
GCD of 36 and 1 is 1
GCD of 36 and 2 is 2
..
GCD of 36 and 16 is 4
GCD of 36 and 17 is 1
GCD of 36 and 18 is 18
..
..
GCD of 36 and 33 is 3
GCD of 36 and 34 is 2
GCD of 36 and 35 is 1
```


### Recursive

```c#
static void Main(string[] args)
{
	Console.WriteLine("GCD of {0} and {1} is {2}", 1, 1, gcd(1, 1));
	Console.WriteLine("GCD of {0} and {1} is {2}", 1, 10, gcd(1, 10));
	Console.WriteLine("GCD of {0} and {1} is {2}", 10, 100, gcd(10, 100));
	Console.WriteLine("GCD of {0} and {1} is {2}", 5, 50, gcd(5, 50));
	Console.WriteLine("GCD of {0} and {1} is {2}", 8, 24, gcd(8, 24));
	Console.WriteLine("GCD of {0} and {1} is {2}", 36, 17, gcd(36, 17));
	Console.WriteLine("GCD of {0} and {1} is {2}", 36, 18, gcd(36, 18));
	Console.WriteLine("GCD of {0} and {1} is {2}", 36, 19, gcd(36, 19));
	for (int x = 1; x < 36; x++)
	{
		Console.WriteLine("GCD of {0} and {1} is {2}", 36, x, gcd(36, x));
	}
	Console.Read();
}

// Greatest Common Denominator using Euclidian Algorithm
// Gist: https://gist.github.com/SecretDeveloper/6c426f8993873f1a05f7
static int gcd(int a, int b)
{
	return b==0 ? a : gcd(b, a % b);
}
```

Example output:

```txt
GCD of 1 and 1 is 1
GCD of 1 and 10 is 1
GCD of 10 and 100 is 10
GCD of 5 and 50 is 5
GCD of 8 and 24 is 8
GCD of 36 and 1 is 1
GCD of 36 and 2 is 2
..
GCD of 36 and 16 is 4
GCD of 36 and 17 is 1
GCD of 36 and 18 is 18
..
..
GCD of 36 and 33 is 3
GCD of 36 and 34 is 2
GCD of 36 and 35 is 1
```


## Clojure

### Euclid's Algorithm

```lisp
(defn gcd
  "(gcd a b) computes the greatest common divisor of a and b."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))
```


That <code>recur</code> call is the same as <code>(gcd b (mod a b))</code>,
but makes use of Clojure's explicit tail call optimization.

This can be easily extended to work with variadic arguments:

```lisp
(defn gcd*
  "greatest common divisor of a list of numbers"
  [& lst]
  (reduce gcd
          lst))
```


### Stein's Algorithm (Binary GCD)

```lisp
(defn stein-gcd [a b]
  (cond
    (zero? a) b
    (zero? b) a
    (and (even? a) (even? b)) (* 2 (stein-gcd (unsigned-bit-shift-right a 1) (unsigned-bit-shift-right b 1)))
    (and (even? a) (odd? b)) (recur (unsigned-bit-shift-right a 1) b)
    (and (odd? a) (even? b)) (recur a (unsigned-bit-shift-right b 1))
    (and (odd? a) (odd? b)) (recur (unsigned-bit-shift-right (Math/abs (- a b)) 1) (min a b))))
```


## COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GCD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A        PIC 9(10)   VALUE ZEROES.
       01 B        PIC 9(10)   VALUE ZEROES.
       01 TEMP     PIC 9(10)   VALUE ZEROES.

       PROCEDURE DIVISION.
       Begin.
           DISPLAY "Enter first number, max 10 digits."
           ACCEPT A
           DISPLAY "Enter second number, max 10 digits."
           ACCEPT B
           IF A < B
             MOVE B TO TEMP
             MOVE A TO B
             MOVE TEMP TO B
           END-IF

           PERFORM UNTIL B = 0
             MOVE A TO TEMP
             MOVE B TO A
             DIVIDE TEMP BY B GIVING TEMP REMAINDER B
           END-PERFORM
           DISPLAY "The gcd is " A
           STOP RUN.
```


## Cobra

```cobra

class Rosetta
	def gcd(u as number, v as number) as number
		u, v = u.abs, v.abs
		while v > 0
			u, v = v, u % v
		return u

	def main
		print "gcd of [12] and [8] is [.gcd(12, 8)]"
		print "gcd of [12] and [-8] is [.gcd(12, -8)]"
		print "gcd of [96] and [27] is [.gcd(27, 96)]"
		print "gcd of [51] and [34] is [.gcd(34, 51)]"

```

Output:

```txt

gcd of 12 and 8 is 4
gcd of 12 and -8 is 4
gcd of 96 and 27 is 3
gcd of 51 and 34 is 17

```


## CoffeeScript

Simple recursion

```coffeescript

gcd = (x, y) ->
  if y == 0 then x else gcd y, x % y

```

Since JS has no TCO, here's a version with no recursion

```coffeescript

gcd = (x, y) ->
  [1..(Math.min x, y)].reduce (acc, v) ->
    if x % v == 0 and y % v == 0 then v else acc

```


## Common Lisp

Common Lisp provides a ''gcd'' function.

```lisp
CL-USER> (gcd 2345 5432)
7
```

Here is an implementation using the do macro.
We call the function `gcd*` so as not to conflict with `common-lisp:gcd`.


```lisp
(defun gcd* (a b)
  (do () ((zerop b) (abs a))
    (shiftf a b (mod a b))))
```

Here is a tail-recursive implementation.


```lisp
(defun gcd* (a b)
  (if (zerop b)
       a
      (gcd2 b (mod a b))))
```

The last implementation is based on the loop macro.

```lisp
(defun gcd* (a b)
  (loop for x = a then y
        and y = b then (mod x y)
        until (zerop y)
        finally (return x)))
```


## Component Pascal

BlackBox Component Builder

```oberon2
MODULE Operations;
IMPORT StdLog,Args,Strings;

PROCEDURE Gcd(a,b: LONGINT):LONGINT;
VAR
	r: LONGINT;
BEGIN
	LOOP
		r := a MOD b;
		IF r = 0 THEN RETURN b END;
		a := b;b := r
	END
END Gcd;

PROCEDURE DoGcd*;
VAR
	x,y,done: INTEGER;
	p: Args.Params;
BEGIN
	Args.Get(p);
	IF p.argc >= 2 THEN
		Strings.StringToInt(p.args[0],x,done);
		Strings.StringToInt(p.args[1],y,done);
		StdLog.String("gcd("+p.args[0]+","+p.args[1]+")=");StdLog.Int(Gcd(x,y));StdLog.Ln
	END
END DoGcd;

END Operations.
```

Execute:

```txt
^Q Operations.DoGcd 12 8 ~
^Q Operations.DoGcd 100 5 ~
^Q Operations.DoGcd 7 23 ~
^Q Operations.DoGcd 24 -112 ~
```

Output:

```txt
gcd(12 ,8 )= 4
gcd(100 ,5 )= 5
gcd(7 ,23 )= 1
gcd(24 ,-112 )= -8
```


## D

```d
import std.stdio, std.numeric;

long myGCD(in long x, in long y) pure nothrow @nogc {
    if (y == 0)
        return x;
    return myGCD(y, x % y);
}

void main() {
    gcd(15, 10).writeln; // From Phobos.
    myGCD(15, 10).writeln;
}
```

Output:

```txt
5
5
```


## Dc

```dc
[dSa%Lard0<G]dsGx+
```

This code assumes that there are two integers on the stack.

```txt
dc -e'28 24 [dSa%Lard0<G]dsGx+ p'
```


## Delphi

See [[#Pascal / Delphi / Free Pascal]].


## DWScript

```delphi
PrintLn(Gcd(231, 210));
```

Output:

```txt
21
```


## Dyalect

Translated from Go.

```dyalect
func gcd(a, b) {
    func bgcd(a, b, res) {
        if a == b {
            return res * a
        } else if a % 2 == 0 && b % 2 == 0 {
            return bgcd(a/2, b/2, 2*res)
        } else if a % 2 == 0 {
            return bgcd(a/2, b, res)
        } else if b % 2 == 0 {
            return bgcd(a, b/2, res)
        } else if a > b {
            return bgcd(a-b, b, res)
        } else {
            return bgcd(a, b-a, res)
        }
    }
    return bgcd(a, b, 1)
}

var testdata = [
    (a: 33, b: 77),
    (a: 49865, b: 69811)
]

for v in testdata {
    print("gcd(\(v.a), \(v.b)) = \(gcd(v.a, v.b))")
}
```

Output:

```txt
gcd(33, 77) = 11
gcd(49865, 69811) = 9973
```


## E

Translated from Python.

```e
def gcd(var u :int, var v :int) {
    while (v != 0) {
        def r := u %% v
        u := v
        v := r
    }
    return u.abs()
}
```


## EasyLang

```easylang
func gcd a b . res .
  while b <> 0
    h = b
    b = a mod b
    a = h
  .
  res = a
.
call gcd 120 35 r
print r
```


## EDSAC order code

The EDSAC had no built-in division.
For this task, a subroutine is needed to return A mod B,
where A and B are positive 35-bit integers.
The rest of the program is fairly straightforward.

```edsac
 [Greatest common divisor for RosettaGit.
  Program for EDSAC, Initial Orders 2]

 [Set up pairs of integers for demo]
            T   45 K [store address in location 45;
                      values are then accessed by code letter H]
            P  220 F [<------ address here]

 [Library subroutine R2. Reads positive integers during input of orders,
    and is then overwritten (so doesn't take up any memory).
  Each integer is followed by 'F', except the last is followed by '#TZ'.]
  GKT20FVDL8FA40DUDTFI40FA40FS39FG@S2FG23FA5@T5@E4@E13Z
            T     #H  [Tell R2 the storage location defined above

  Integers to be read by R2. First item is count, then pairs for GCD algo.]
  4F1066F2019F1815F1914F103785682F167928761F109876463F177777648#TZ

  [----------------------------------------------------------------------
  Library subroutine P7.
  Prints long strictly positive integer at 0D.
  10 characters, right justified, padded left with spaces.
  Closed, even; 35 storage locations; working position 4D.]
            T   56 K
  GKA3FT26@H28#@NDYFLDT4DS27@TFH8@S8@T1FV4DAFG31@SFLDUFOFFFSFL4F
  T4DA1FA27@G11@XFT28#ZPFT27ZP1024FP610D@524D!FO30@SFL8FE22@

  [----------------------------------------------------------------------
  Subroutine to return  a mod b, where a and b are
  positive 35-bit integers (maximum 2^34 - 1).
  Input: a at 4D, b at 6D.
  Output: a mod b at 4D; does not change 6D.
  Working location 0D.]
            T  100 K
            G      K
            A    3 F  [plant link]
            T   26 @
            A    6 D  [load divisor]
      [3]   T      D  [initialize shifted divisor]
            A    4 D  [load dividend]
            R      D  [shift 1 right]
            S      D  [shifted divisor > dividend/2 yet?]
            G   12 @  [yes, start subtraction]
            T   27 @  [no, clear acc]
            A      D  [shift divisor 1 more]
            L      D
            E    3 @  [loop back (always, since acc = 0)]
     [12]   T   27 @  [clear acc]
     [13]   A    4 D  [load remainder (initially = dividend)]
            S      D  [trial subtraction]
            G   17 @  [skip if can't subtract]
            T    4 D  [update remainder]
     [17]   T   27 @  [clear acc]
            A    6 D  [load original divisor]
            S      D  [is shifted divisor back to original?]
            E   26 @  [yes, exit (with accumulator = 0,
                       in accordance with EDSAC convention)]
            T   27 @  [no, clear acc]
            A      D  [shift divisor 1 right]
            R      D
            T      D
            E   13 @  [loop back (always, since acc = 0)]
     [26]   E      F
     [27]   P      F  [junk word, to clear accumulator]

 [----------------------------------------------------------------------
  Subroutine to find GCD of two positive integers at 4D and 6D.
  Returns result in 6D.]
            T  130 K
            G      K
            A    3 F [plant link]
            T   12 @
      [2]   A    2 @ [set up return from subroutine]
            G  100 F [4D := 4D mod 6D]
            S    4 D [load negative of 4D]
            E   12 @ [exit if 4D = 0]
            T      D [else swap with 6D, using 0D as temp store]
            A    6 D
            T    4 D
            S      D [change back to positive]
            T    6 D
            E    2 @ [loop back (always, since acc = 0)]
     [12]   E      F

  [----------------------------------------------------------------------
  Main routine]
            T  150 K
            G      K
  [Variable]
      [0]   P      F
  [Constants]
      [1]   P      D [single-word 1]
      [2]   A    2#H [order to load first number of first pair]
      [3]   P    2 F [to inc addresses by 2]
      [4]   #      F [figure shift]
      [5]   K 2048 F [letter shift]
      [6]   G      F [letters to print 'GCD']
      [7]   C      F
      [8]   D      F
      [9]   V      F [equals sign (in firures mode)]
     [10]   !      F [space]
     [11]   @      F [carriage return]
     [12]   &      F [line feed]
     [13]   K 4096 F [null char]

           [Enter here with acc = 0]
     [14]   O    4 @ [set teleprinter to figures]
            S      H [negative of number of pairs]
            T      @ [initialize counter]
            A    2 @ [initial load order]
     [18]   U   23 @ [plant order to load 1st integer]
            U   32 @
            A    3 @ [inc address by 2]
            U   28 @ [plant order to load 2nd integer]
            T   34 @
     [23]   A     #H [load 1st integer (order set up at runtime)]
            T      D [to 0D for printing]
            A   25 @ [for return from print subroutine]
            G   56 F [print 1st number]
            O   10 @ [followed by space]
     [28]   A     #H [load 2nd integer (order set up at runtime)]
            T      D [to 0D for printing]
            A   30 @ [for return from print subroutine]
            G   56 F [print 2nd number]
     [32]   A     #H [load 1st integer (order set up at runtime)]
            T    4 D [to 4D for GCD subroutine]
     [34]   A     #H [load 2nd integer (order set up at runtime)]
            T    6 D [to 4D for GCD subroutine]
     [36]   A   36 @ [for return from subroutine]
            G  130 F [call subroutine for GCD]
           [Cosmetic printing, add '  GCD = ']
            O   10 @
            O   10 @
            O    5 @
            O    6 @
            O    7 @
            O    8 @
            O    4 @
            O   10 @
            O    9 @
            O   10 @
            A    6 D [load GCD]
            T      D [to 0D for printing]
            A   50 @ [for return from print subroutine]
            G   56 F [print GCD]
            O   11 @ [followed by new line]
            O   12 @
           [On to next pair]
            A      @ [load negative count of pairs]
            A    1 @ [add 1]
            E   62 @ [exit if count = 0]
            T      @ [store back]
            A   23 @ [order to load first of pair]
            A    3 @ [inc address by 4 for next pair]
            A    3 @
            G   18 @ [loop back (always, since 'A' < 0)]
     [62]   O   13 @ [null char to flush teleprinter buffer]
            Z      F [stop]
            E   14 Z [define entry point]
            P      F [acc = 0 on entry]

```

Output:

```txt

      1066       2019  GCD =          1
      1815       1914  GCD =         33
 103785682  167928761  GCD =       1001
 109876463  177777648  GCD =    1234567

```


## Eiffel

Translated from D.

```eiffel

class
	APPLICATION

create
	make

feature -- Implementation

	gcd (x: INTEGER y: INTEGER): INTEGER
		do
			if y = 0 then
				Result := x
			else
				Result := gcd (y, x \\ y);
			end
		end

feature {NONE} -- Initialization

	make
			-- Run application.
		do
			print (gcd (15, 10))
			print ("%N")
		end

end

```


## Elena

Translated from C#.

ELENA 4.x :

```elena
import system'math;
import extensions;

gcd(a,b)
{
    var i := a;
    var j := b;
    while(j != 0)
    {
        var tmp := i;
        i := j;
        j := tmp.mod(j)
    };

    ^ i
}

printGCD(a,b)
{
    console.printLineFormatted("GCD of {0} and {1} is {2}", a, b, gcd(a,b))
}

public program()
{
    printGCD(1,1);
    printGCD(1,10);
    printGCD(10,100);
    printGCD(5,50);
    printGCD(8,24);
    printGCD(36,17);
    printGCD(36,18);
    printGCD(36,19);
    printGCD(36,33);
}
```

Output:

```txt
GCD of 1 and 1 is 1
GCD of 1 and 10 is 1
GCD of 10 and 100 is 10
GCD of 5 and 50 is 5
GCD of 8 and 24 is 8
GCD of 36 and 17 is 1
GCD of 36 and 18 is 18
GCD of 36 and 19 is 1
GCD of 36 and 33 is 3
```


## Elixir

```elixir
defmodule RC do
  def gcd(a,0), do: abs(a)
  def gcd(a,b), do: gcd(b, rem(a,b))
end

IO.puts RC.gcd(1071, 1029)
IO.puts RC.gcd(3528, 3780)
```

Output:

```txt
21
252
```


## Emacs Lisp

```lisp
(defun gcd (a b)
    (cond
     ((< a b) (gcd a (- b a)))
     ((> a b) (gcd (- a b) b))
     (t a)))

```


## Erlang

```erlang
% Implemented by Arjun Sunel
-module(gcd).
-export([main/0]).

main() ->gcd(-36,4).

gcd(A, 0) -> A;

gcd(A, B) -> gcd(B, A rem B).
```

Output:

```txt
4
```


## ERRE

This is a iterative version.

```ERRE
PROGRAM EUCLIDE
! calculate G.C.D. between two integer numbers
! using Euclidean algorithm

!VAR J%,K%,MCD%,A%,B%

BEGIN
  PRINT(CHR$(12);"Input two numbers : ";)  !CHR$(147) in C-64 version
  INPUT(J%,K%)
  A%=J% B%=K%
  WHILE A%<>B% DO
    IF A%>B%
       THEN
         A%=A%-B%
       ELSE
         B%=B%-A%
    END IF
  END WHILE
  MCD%=A%
  PRINT("G.C.D. between";J%;"and";K%;"is";MCD%)
END PROGRAM
```

Output:
 Input two numbers : ? 112,44
 G.C.D. between 112 and 44 is 4


## Euler Math Toolbox

Non-recursive version in Euler Math Toolbox.
Note, that there is a built-in command.

```txt
>ggt(123456795,1234567851)
 33
>function myggt (n:index, m:index) ...
$  if n<m then {n,m}={m,n}; endif;
$  repeat
$    k=mod(n,m);
$    if k==0 then return m; endif;
$    if k==1 then return 1; endif;
$    {n,m}={m,k};
$  end;
$  endfunction
>myggt(123456795,1234567851)
 33
```


## Euphoria

Translated from C/C++.


### Iterative Euclid algorithm


```euphoria
function gcd_iter(integer u, integer v)
    integer t
    while v do
        t = u
        u = v
        v = remainder(t, v)
    end while
    if u < 0 then
        return -u
    else
        return u
    end if
end function
```


### Recursive Euclid algorithm

```euphoria
function gcd(integer u, integer v)
    if v then
        return gcd(v, remainder(u, v))
    elsif u < 0 then
        return -u
    else
        return u
    end if
end function
```


### Iterative binary algorithm

```euphoria
function gcd_bin(integer u, integer v)
    integer t, k
    if u < 0 then -- abs(u)
        u = -u
    end if
    if v < 0 then -- abs(v)
        v = -v
    end if
    if u < v then
        t = u
        u = v
        v = t
    end if
    if v = 0 then
        return u
    end if
    k = 1
    while and_bits(u,1) = 0 and and_bits(v,1) = 0 do
        u = floor(u/2) -- u >>= 1
        v = floor(v/2) -- v >>= 1
        k *= 2 -- k <<= 1
    end while
    if and_bits(u,1) then
        t = -v
    else
        t = u
    end if
    while t do
        while and_bits(t, 1) = 0 do
            t = floor(t/2)
        end while
        if t > 0 then
            u = t
        else
            v = -t
        end if
        t = u - v
    end while
    return u * k
end function
```


## Excel

Excel's GCD can handle multiple values.
Type in a cell:

```excel
=GCD(A1:E1)
```

This will get the GCD of the first 5 cells of the first row.

```txt
30	10	500	25	1000
5
```


## Ezhil

```Ezhil

## இந்த நிரல் இரு எண்களுக்கு இடையிலான மீச்சிறு பொது மடங்கு (LCM), மீப்பெரு பொது வகுத்தி (GCD) என்ன என்று கணக்கிடும்

நிரல்பாகம் மீபொவ(எண்1, எண்2)

	@(எண்1 == எண்2) ஆனால்

  ## இரு எண்களும் சமம் என்பதால், அந்த எண்ணேதான் அதன் மீபொவ

		பின்கொடு எண்1

	@(எண்1 > எண்2) இல்லைஆனால்

		சிறியது = எண்2
		பெரியது = எண்1

	இல்லை

		சிறியது = எண்1
		பெரியது = எண்2

	முடி

	மீதம் = பெரியது % சிறியது

	@(மீதம் == 0) ஆனால்

  ## பெரிய எண்ணில் சிறிய எண் மீதமின்றி வகுபடுவதால், சிறிய எண்தான் மீப்பெரு பொதுவகுத்தியாக இருக்கமுடியும்

		பின்கொடு சிறியது

	இல்லை

		தொடக்கம் = சிறியது - 1

		நிறைவு = 1

		@(எண் = தொடக்கம், எண் >= நிறைவு, எண் = எண் - 1) ஆக

			மீதம்1 = சிறியது % எண்

			மீதம்2 = பெரியது % எண்

   ## இரு எண்களையும் மீதமின்றி வகுக்கக்கூடிய பெரிய எண்ணைக் கண்டறிகிறோம்

			@((மீதம்1 == 0) && (மீதம்2 == 0)) ஆனால்

				பின்கொடு எண்

			முடி

		முடி

	முடி

முடி

அ = int(உள்ளீடு("ஓர் எண்ணைத் தாருங்கள் "))
ஆ = int(உள்ளீடு("இன்னோர் எண்ணைத் தாருங்கள் "))

பதிப்பி "நீங்கள் தந்த இரு எண்களின் மீபொவ (மீப்பெரு பொது வகுத்தி, GCD) = ", மீபொவ(அ, ஆ)
```


## Free Pascal

See [[#Pascal / Delphi / Free Pascal]].


## Frege

```fsharp
module gcd.GCD where

pure native parseInt java.lang.Integer.parseInt :: String -> Int

gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

main args = do
    (a:b:_) = args
    println $ gcd' (parseInt a) (parseInt b)

```


## F\#

```fsharp

let rec gcd a b =
  if b = 0
    then abs a
  else gcd b (a % b)

>gcd 400 600
val it : int = 200
```


## Factor

```factor
: gcd ( a b -- c )
    [ abs ] [
        [ nip ] [ mod ] 2bi gcd
    ] if-zero ;
```


## FALSE

```false
10 15$ [0=~][$@$@$@\/*-$]#%. { gcd(10,15)=5 }
```


## Fantom

```fantom

class Main
{
  static Int gcd (Int a, Int b)
  {
    a = a.abs
    b = b.abs
    while (b > 0)
    {
      t := a
      a = b
      b = t % b
    }
    return a
  }

  public static Void main()
  {
    echo ("GCD of 51, 34 is: " + gcd(51, 34))
  }
}

```


## Forth

```forth
: gcd ( a b -- n )
  begin dup while tuck mod repeat drop ;
```


## Fortran

Works with Fortran 95 and later


### Recursive Euclid algorithm

```fortran
recursive function gcd_rec(u, v) result(gcd)
    integer             :: gcd
    integer, intent(in) :: u, v

    if (mod(u, v) /= 0) then
        gcd = gcd_rec(v, mod(u, v))
    else
        gcd = v
    end if
end function gcd_rec
```


### Iterative Euclid algorithm

```fortran
subroutine gcd_iter(value, u, v)
  integer, intent(out) :: value
  integer, intent(inout) :: u, v
  integer :: t

  do while( v /= 0 )
     t = u
     u = v
     v = mod(t, v)
  enddo
  value = abs(u)
end subroutine gcd_iter
```

A different version, and implemented as function

```fortran
function gcd(v, t)
  integer :: gcd
  integer, intent(in) :: v, t
  integer :: c, b, a

  b = t
  a = v
  do
     c = mod(a, b)
     if ( c == 0) exit
     a = b
     b = c
  end do
  gcd = b ! abs(b)
end function gcd
```


### Iterative binary algorithm

```fortran
subroutine gcd_bin(value, u, v)
  integer, intent(out) :: value
  integer, intent(inout) :: u, v
  integer :: k, t

  u = abs(u)
  v = abs(v)
  if( u < v ) then
     t = u
     u = v
     v = t
  endif
  if( v == 0 ) then
     value = u
     return
  endif
  k = 1
  do while( (mod(u, 2) == 0).and.(mod(v, 2) == 0) )
     u = u / 2
     v = v / 2
     k = k * 2
  enddo
  if( (mod(u, 2) == 0) ) then
     t = u
  else
     t = -v
  endif
  do while( t /= 0 )
     do while( (mod(t, 2) == 0) )
        t = t / 2
     enddo
     if( t > 0 ) then
        u = t
     else
        v = -t
     endif
     t = u - v
  enddo
  value = u * k
end subroutine gcd_bin
```


### Notes on performance

<tt>gcd_iter(40902, 24140)</tt> takes us about '''2.8''' µsec

<tt>gcd_bin(40902, 24140)</tt> takes us about '''2.5''' µsec


## FreeBASIC

```FreeBASIC
' version 17-06-2015
' compile with: fbc -s console

Function gcd(x As ULongInt, y As ULongInt) As ULongInt

    Dim As ULongInt t

    While y
        t = y
        y = x Mod y
        x = t
    Wend

    Return x

End Function

' ------=< MAIN >=------

Dim As ULongInt a = 111111111111111
Dim As ULongInt b = 11111

Print : Print "GCD(";a;", ";b;") = "; gcd(a, b)
Print : Print "GCD(";a;", 111) = "; gcd(a, 111)

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print : Print "hit any key to end program"
Sleep
End
```

Output:

```txt
GCD(111111111111111, 11111) = 11111
GCD(111111111111111, 111) = 111
```


## Frink

Frink has a builtin `gcd[x,y]` function
that returns the GCD of two integers (which can be arbitrarily large.)

```frink
println[gcd[12345,98765]]
```


## FunL

FunL has pre-defined function `gcd` in module `integers` defined as:

```funl
def
  gcd( 0, 0 ) = error( 'integers.gcd: gcd( 0, 0 ) is undefined' )
  gcd( a, b ) =
    def
      _gcd( a, 0 ) = a
      _gcd( a, b ) = _gcd( b, a%b )

    _gcd( abs(a), abs(b) )
```


## FutureBasic

```futurebasic
local fn gcd( a as long, b as long )
dim as long result

if ( b != 0 )
   result = fn gcd( b, a mod b)
else
   result = abs(a)
end if
end fn = result
```



## GAP

```gap
# Built-in
GcdInt(35, 42);
# 7

# Euclidean algorithm
GcdInteger := function(a, b)
    local c;
    a := AbsInt(a);
    b := AbsInt(b);
    while b > 0 do
        c := a;
        a := b;
        b := RemInt(c, b);
    od;
    return a;
end;

GcdInteger(35, 42);
# 7
```


## Genyris

### Recursive

```genyris
def gcd (u v)
    u = (abs u)
    v = (abs v)
    cond
       (equal? v 0) u
       else (gcd v (% u v))
```


### Iterative

```genyris
def gcd (u v)
    u = (abs u)
    v = (abs v)
    while (not (equal? v 0))
       var tmp (% u v)
       u = v
       v = tmp
    u
```


## GFA Basic

```basic
'
' Greatest Common Divisor
'
a%=24
b%=112
PRINT "GCD of ";a%;" and ";b%;" is ";@gcd(a%,b%)
'
' Function computes gcd
'
FUNCTION gcd(a%,b%)
  LOCAL t%
  '
  WHILE b%<>0
    t%=a%
    a%=b%
    b%=t% MOD b%
  WEND
  '
  RETURN ABS(a%)
ENDFUNC
```


## GML

```GML
var n,m,r;
n = max(argument0,argument1);
m = min(argument0,argument1);
while (m != 0)
{
  r = n mod m;
  n = m;
  m = r;
}
return a;
```


## Gnuplot

```gnuplot
gcd (a, b) = b == 0 ? a : gcd (b, a % b)
```

Example:

```gnuplot
print gcd (111111, 1111)
```

Output:

```txt
11
```


## Go

### Binary Euclidian

```go
package main

import "fmt"

func gcd(a, b int) int {
    var bgcd func(a, b, res int) int

    bgcd = func(a, b, res int) int {
	switch {
	case a == b:
	    return res * a
	case a % 2 == 0 && b % 2 == 0:
	    return bgcd(a/2, b/2, 2*res)
	case a % 2 == 0:
	    return bgcd(a/2, b, res)
	case b % 2 == 0:
	    return bgcd(a, b/2, res)
	case a > b:
	    return bgcd(a-b, b, res)
	default:
	    return bgcd(a, b-a, res)
	}
    }

    return bgcd(a, b, 1)
}

func main() {
    type pair struct {
	a int
	b int
    }

    var testdata []pair = []pair{
	pair{33, 77},
	pair{49865, 69811},
    }

    for _, v := range testdata {
	fmt.Printf("gcd(%d, %d) = %d\n", v.a, v.b, gcd(v.a, v.b))
    }
}
```

Output for Binary Euclidian algorithm:

```txt
gcd(33, 77) = 11
gcd(49865, 69811) = 9973
```


### Iterative

```go
package main

import "fmt"

func gcd(x, y int) int {
    for y != 0 {
        x, y = y, x%y
    }
    return x
}

func main() {
    fmt.Println(gcd(33, 77))
    fmt.Println(gcd(49865, 69811))
}
```


### Builtin

(This is just a wrapper for <tt>big.GCD</tt>)

```go
package main

import (
    "fmt"
    "math/big"
)

func gcd(x, y int64) int64 {
    return new(big.Int).GCD(nil, nil, big.NewInt(x), big.NewInt(y)).Int64()
}

func main() {
    fmt.Println(gcd(33, 77))
    fmt.Println(gcd(49865, 69811))
}
```

Output in either case

```txt
11
9973
```


## Groovy

### Recursive

```groovy
def gcdR
gcdR = {
  m, n -> m = m.abs();
  n = n.abs();
  n == 0 ? m : m%n == 0 ? n : gcdR(n, m%n)
}
```


### Iterative

```groovy
def gcdI = {
  m, n -> m = m.abs();
  n = n.abs();
  n == 0 ? m : { while(m%n != 0) { t=n; n=m%n; m=t }; n }()
}
```

Test program:

```groovy
println "                R     I"
println "gcd(28, 0)   = ${gcdR(28, 0)} == ${gcdI(28, 0)}"
println "gcd(0, 28)   = ${gcdR(0, 28)} == ${gcdI(0, 28)}"
println "gcd(0, -28)  = ${gcdR(0, -28)} == ${gcdI(0, -28)}"
println "gcd(70, -28) = ${gcdR(70, -28)} == ${gcdI(70, -28)}"
println "gcd(70, 28)  = ${gcdR(70, 28)} == ${gcdI(70, 28)}"
println "gcd(28, 70)  = ${gcdR(28, 70)} == ${gcdI(28, 70)}"
println "gcd(800, 70) = ${gcdR(800, 70)} == ${gcdI(800, 70)}"
println "gcd(27, -70) =  ${gcdR(27, -70)} ==  ${gcdI(27, -70)}"
```

Output:

```txt
                R     I
gcd(28, 0)   = 28 == 28
gcd(0, 28)   = 28 == 28
gcd(0, -28)  = 28 == 28
gcd(70, -28) = 14 == 14
gcd(70, 28)  = 14 == 14
gcd(28, 70)  = 14 == 14
gcd(800, 70) = 10 == 10
gcd(27, -70) =  1 ==  1
```


## Haskell

That is already available as the function `gcd` in the Prelude.
Here's the implementation:

```haskell
gcd :: (Integral a) => a -> a -> a
gcd x y = gcd_ (abs x) (abs y)
  where
    gcd_ a 0 = a
    gcd_ a b = gcd_ b (a `rem` b)
```


## HicEst

```hicest
FUNCTION gcd(a, b)
   IF(b == 0) THEN
     gcd = ABS(a)
   ELSE
     aa = a
     gcd = b
     DO i = 1, 1E100
       r = ABS(MOD(aa, gcd))
       IF( r == 0 ) RETURN
       aa = gcd
       gcd = r
     ENDDO
   ENDIF
 END
```


## Icon and Unicon

```Icon
link numbers   # gcd is part of the Icon Programming Library
procedure main(args)
    write(gcd(arg[1], arg[2])) | "Usage: gcd n m")
end
```

[numbers](http://www.cs.arizona.edu/icon/library/procs/numbers.htm)
implements this as:

```Icon
procedure gcd(i,j)		#: greatest common divisor
   local r

   if (i | j) < 1 then runerr(501)

   repeat {
      r := i % j
      if r = 0 then return j
      i := j
      j := r
      }
end
```


## J

```J
x+.y
```

For example:

```J
   12 +. 30
6
```

Note that `+.` is a single, two character token.
GCD is a primitive in J (and anyone that has studied
the right kind of mathematics should instantly recognize
why the same operation is used for both GCD and OR -- among other things,
GCD and boolean OR both have the same identity element:
0, and of course they produce the same numeric results on the same arguments
(when we are allowed to use the usual 1 bit implementation
of 0 and 1 for false and true) -
more than that, though,
GCD corresponds to George Boole's original "Boolean Algebra"
(as it was later called).
The redefinition of "Boolean algebra" to include logical negation
came much later, in the 20th century).

gcd could also be defined recursively,
if you do not mind a little inefficiency:

```J
gcd=: (| gcd [)^:(0<[)&|
```


## Java

### Iterative

```java
public static long gcd(long a, long b){
   long factor= Math.min(a, b);
   for(long loop= factor;loop > 1;loop--){
      if(a % loop == 0 && b % loop == 0){
         return loop;
      }
   }
   return 1;
}
```


### Iterative Euclid's Algorithm

```java
public static int gcd(int a, int b) //valid for positive integers.
{
  while(b > 0)
  {
    int c = a % b;
    a = b;
    b = c;
  }
  return a;
}
```


### Optimized Iterative

```java
static int gcd(int a,int b)
{
  int min=a>b?b:a,max=a+b-min, div=min;
  for(int i=1;i<min;div=min/++i)
    if(min%div==0&&max%div==0)
      return div;
  return 1;
}
```


### Iterative binary algorithm

Translated from C/C++.

```java
public static long gcd(long u, long v){
  long t, k;

  if (v == 0) return u;

  u = Math.abs(u);
  v = Math.abs(v);
  if (u < v){
    t = u;
    u = v;
    v = t;
  }

  for(k = 1; (u & 1) == 0 && (v & 1) == 0; k <<= 1){
    u >>= 1; v >>= 1;
  }

  t = (u & 1) != 0 ? -v : u;
  while (t != 0){
    while ((t & 1) == 0) t >>= 1;

    if (t > 0)
      u = t;
    else
      v = -t;

    t = u - v;
  }
  return u * k;
}
```


### Recursive

```java
public static long gcd(long a, long b){
   if(a == 0) return b;
   if(b == 0) return a;
   if(a > b) return gcd(b, a % b);
   return gcd(a, b % a);
}
```

### Built-in

```java
import java.math.BigInteger;

public static long gcd(long a, long b){
   return BigInteger.valueOf(a).gcd(BigInteger.valueOf(b)).longValue();
}
```


## JavaScript

Iterative implementation:

```javascript
function gcd(a,b) {
  a = Math.abs(a);
  b = Math.abs(b);

  if (b > a) {
    var temp = a;
    a = b;
    b = temp;
  }

  while (true) {
    a %= b;
    if (a === 0) { return b; }
    b %= a;
    if (b === 0) { return a; }
  }
}
```

Recursive:

```javascript
function gcd_rec(a, b) {
  return b ? gcd_rec(b, a % b) : Math.abs(a);
}
```

Implementation that works on an array of integers:

```javascript
function GCD(arr) {
  var i, y,
      n = arr.length,
      x = Math.abs(arr[0]);

  for (i = 1; i < n; i++) {
    y = Math.abs(arr[i]);

    while (x && y) {
      (x > y) ? x %= y : y %= x;
    }
    x += y;
  }
  return x;
}

//For example:
GCD([57,0,-45,-18,90,447]); //=> 3

```


## Joy

```joy
DEFINE gcd == [0
] [dup rollup rem] while pop.
```


## jq

```jq
def recursive_gcd(a; b):
  if b == 0 then a
  else recursive_gcd(b; a % b)
  end ;
```

Recent versions of jq include support for tail recursion optimization
for arity-0 filters
(which can be thought of as arity-1 functions),
so here is an implementation that takes advantage of that optimization.
Notice that the subfunction, rgcd, can be easily derived
from recursive_gcd above by moving the arguments to the input:

```jq
def gcd(a; b):
  # The subfunction expects [a,b] as input
  # i.e. a ~ .[0] and b ~ .[1]
  def rgcd: if .[1] == 0 then .[0]
         else [.[1], .[0] % .[1]] | rgcd
         end;
  [a,b] | rgcd ;
```


## Julia

Julia includes a built-in `gcd` function:

```txt
julia> gcd(4,12)
4
julia> gcd(6,12)
6
julia> gcd(7,12)
1
```

The actual implementation of this function in Julia 0.2's standard library
is reproduced here:

```julia
function gcd{T<:Integer}(a::T, b::T)
    neg = a < 0
    while b != 0
        t = b
        b = rem(a, b)
        a = t
    end
    g = abs(a)
    neg ? -g : g
end
```

For arbitrary-precision integers,
Julia calls a different implementation from the GMP library.


## K

```K
gcd:{:[~x;y;_f[y;x!y]]}
```


## Klong

```K
gcd::{:[~x;y:|~y;x:|x>y;.f(y;x!y);.f(x;y!x)]}
```


## Kotlin

Recursive one line solution:

```kotlin
fun gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
```


## LabVIEW

Translated from AutoHotkey.

It may be helpful to read about
[Recursion in LabVIEW](http://digital.ni.com/public.nsf/allkb/7140920082C3AC15862572840015A81E).

[[File:LabVIEW Greatest common divisor.png]]


## LFE

Translated from Clojure.

```lisp
> (defun gcd
  "Get the greatest common divisor."
  ((a 0) a)
  ((a b) (gcd b (rem a b))))
```

Usage:

```txt
> (gcd 12 8)
4
> (gcd 12 -8)
4
> (gcd 96 27)
3
> (gcd 51 34)
17
```


## Liberty BASIC

```lb
'iterative Euclid algorithm
print GCD(-2,16)
end

function GCD(a,b)
    while b
        c = a
        a = b
        b = c mod b
    wend
    GCD = abs(a)
    end function

```


## Limbo

```Limbo
gcd(x: int, y: int): int
{
	if(y == 0)
		return x;
	return gcd(y, x % y);
}

```


## LiveCode

```LiveCode
function gcd x,y
   repeat until y = 0
      put x mod y into z
      put y into x
      put z into y
   end repeat
   return x
end gcd
```


## Logo

```logo
to gcd :a :b
  if :b = 0 [output :a]
  output gcd :b  modulo :a :b
end
```


## Lua

Translated from C.

```lua
function gcd(a,b)
	if b ~= 0 then
		return gcd(b, a % b)
	else
		return math.abs(a)
	end
end

function demo(a,b)
	print("GCD of " .. a .. " and " .. b .. " is " .. gcd(a, b))
end

demo(100, 5)
demo(5, 100)
demo(7, 23)
```

Output:

```txt
GCD of 100 and 5 is 5
GCD of 5 and 100 is 5
GCD of 7 and 23 is 1
```

Faster iterative solution of Euclid:

```lua
function gcd(a,b)
    while b~=0 do
        a,b=b,a%b
    end
    return math.abs(a)
end
```


## Lucid

### dataflow algorithm

```lucid
gcd(n,m) where
   z = [% n, m %] fby if x > y then [% x - y, y %] else [% x, y - x%] fi;
   x = hd(z);
   y = hd(tl(z));
   gcd(n, m) = (x asa x*y eq 0) fby eod;
end
```


## Luck

```luck
function gcd(a: int, b: int): int = (
   if a==0 then b
   else if b==0 then a
   else if a>b then gcd(b, a % b)
   else gcd(a, b % a)
)
```


## M2000 Interpreter

```M2000 Interpreter
gcd=lambda (u as long, v as long) -> {
           =if(v=0&->abs(u), lambda(v, u mod v))
}
gcd_Iterative= lambda (m as long, n as long) -> {
   while m  {
       let old_m = m
       m = n mod m
       n = old_m
   }
   =abs(n)
}
Module CheckGCD (f){
      Print f(49865, 69811)=9973
      Def ExpType$(x)=Type$(x)
      Print ExpType$(f(49865, 69811))="Long"
}
CheckGCD gcd
CheckGCD gcd_Iterative
```


## Maple

To compute the greatest common divisor of two integers in Maple,
use the procedure igcd.

```Maple
igcd( a, b )
```

For example,

```Maple
> igcd( 24, 15 );
                3
```


## Mathematica / Wolfram Language

```mathematica
GCD[a, b]
```


## MATLAB

```Matlab
function [gcdValue] = greatestcommondivisor(integer1, integer2)
   gcdValue = gcd(integer1, integer2);
```


## Maxima

```maxima
/* There is a function gcd(a, b) in Maxima, but one can rewrite it */
gcd2(a, b) := block([a: abs(a), b: abs(b)], while b # 0 do [a, b]: [b, mod(a, b)], a)$

/* both will return 2^97 * 3^48 */
gcd(100!, 6^100), factor;
gcd2(100!, 6^100), factor;
```


## MAXScript

### Iterative Euclid algorithm

```maxscript
fn gcdIter a b =
(
    while b > 0 do
    (
        c = mod a b
        a = b
        b = c
    )
    abs a
)
```


### Recursive Euclid algorithm

```maxscript
fn gcdRec a b =
(
    if b > 0 then gcdRec b (mod a b) else abs a
)
```


## Mercury

### Recursive Euclid algorithm

```Mercury
:- module gcd.

:- interface.
:- import_module integer.

:- func gcd(integer, integer) = integer.

:- implementation.

:- pragma memo(gcd/2).
gcd(A, B) = (if B = integer(0) then A else gcd(B, A mod B)).
```

An example console program to demonstrate the gcd module:

```Mercury
:- module test_gcd.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module gcd.
:- import_module integer.
:- import_module list.
:- import_module string.

main(!IO) :-
    command_line_arguments(Args, !IO),
    filter(is_all_digits, Args, CleanArgs),

    Arg1 = list.det_index0(CleanArgs, 0),
    Arg2 = list.det_index0(CleanArgs, 1),
    A = integer.det_from_string(Arg1),
    B = integer.det_from_string(Arg2),

    Fmt = integer.to_string,
    GCD = gcd(A, B),
    io.format("gcd(%s, %s) = %s\n", [s(Fmt(A)), s(Fmt(B)), s(Fmt(GCD))], !IO).
```

Example output:

```Bash
gcd(70000000000000000000000, 60000000000000000000000000) = 10000000000000000000000
```


## MINIL

```minil
// Greatest common divisor
00 0E  GCD:   ENT  R0
01 1E         ENT  R1
02 21  Again: R2 = R1
03 10  Loop:  R1 = R0
04 02         R0 = R2
05 2D  Minus: DEC  R2
06 8A         JZ   Stop
07 1D         DEC  R1
08 C5         JNZ  Minus
09 83         JZ   Loop
0A 1D  Stop:  DEC  R1
0B C2         JNZ  Again
0C 80         JZ   GCD   // Display GCD in R0
```


## MIPS Assembly

```mips
gcd:
  # a0 and a1 are the two integer parameters
  # return value is in v0
  move $t0, $a0
  move $t1, $a1
loop:
  beq $t1, $0, done
  div $t0, $t1
  move $t0, $t1
  mfhi $t1
  j loop
done:
  move $v0, $t0
  jr $ra
```

## МК-61/52

```mk-61/52
ИПA	ИПB	/	П9	КИП9	ИПA	ИПB	ПA	ИП9	*
-	ПB	x=0	00	ИПA	С/П
```

Enter: `n = РA, m = РB (n > m)`.


## ML

### mLite

```ocaml
fun gcd (a, 0) = a
      | (0, b) = b
      | (a, b) where (a < b)
               = gcd (a, b rem a)
      | (a, b) = gcd (b, a rem b)
```


### Standard ML

```sml
fun gcd a 0 = a
  | gcd a b = gcd b (a mod b)
```


## Modula-2

```modula2
MODULE ggTkgV;

FROM    InOut           IMPORT  ReadCard, WriteCard, WriteLn, WriteString, WriteBf;

VAR   x, y, u, v        : CARDINAL;

BEGIN
  WriteString ("x = ");         WriteBf;        ReadCard (x);
  WriteString ("y = ");         WriteBf;        ReadCard (y);
  u := x;
  v := y;
  WHILE  x # y  DO
    (*  ggT (x, y) = ggT (x0, y0), x * v + y * u = 2 * x0 * y0          *)
    IF  x > y  THEN
      x := x - y;
      u := u + v
    ELSE
      y := y - x;
      v := v + u
    END
  END;
  WriteString ("ggT =");        WriteCard (x, 6);               WriteLn;
  WriteString ("kgV =");        WriteCard ((u+v) DIV 2, 6);     WriteLn;
  WriteString ("u =");          WriteCard (u, 6);               WriteLn;
  WriteString ("v =");          WriteCard (v , 6);              WriteLn
END ggTkgV.
```

Producing the output:

```txt
jan@Beryllium:~/modula/Wirth/PIM$ ggtkgv
x = 12
y = 20
ggT =     4
kgV =    60
u =    44
v =    76
jan@Beryllium:~/modula/Wirth/PIM$ ggtkgv
x = 123
y = 255
ggT =     3
kgV = 10455
u = 13773
v =  7137
```


## Modula-3

```modula3
MODULE GCD EXPORTS Main;

IMPORT IO, Fmt;

PROCEDURE GCD(a, b: CARDINAL): CARDINAL =
  BEGIN
    IF a = 0 THEN
      RETURN b;
    ELSIF b = 0 THEN
      RETURN a;
    ELSIF a > b THEN
      RETURN GCD(b, a MOD b);
    ELSE
      RETURN GCD(a, b MOD a);
    END;
  END GCD;

BEGIN
  IO.Put("GCD of 100, 5 is " & Fmt.Int(GCD(100, 5)) & "\n");
  IO.Put("GCD of 5, 100 is " & Fmt.Int(GCD(5, 100)) & "\n");
  IO.Put("GCD of 7, 23 is " & Fmt.Int(GCD(7, 23)) & "\n");
END GCD.
```

Output:

```txt
GCD of 100, 5 is 5
GCD of 5, 100 is 5
GCD of 7, 23 is 1
```


## MUMPS

```MUMPS
GCD(A,B)
 QUIT:((A/1)'=(A\1))!((B/1)'=(B\1)) 0
 SET:A<0 A=-A
 SET:B<0 B=-B
 IF B'=0
 FOR  SET T=A#B,A=B,B=T QUIT:B=0 ;ARGUEMENTLESS FOR NEEDS TWO SPACES
 QUIT A
```

Ouput:

```txt
CACHE>S X=$$GCD^ROSETTA(12,24) W X
12
CACHE>S X=$$GCD^ROSETTA(24,-112) W X
8
CACHE>S X=$$GCD^ROSETTA(24,-112.2) W X
0
```


## MySQL

```mysql
DROP FUNCTION IF EXISTS gcd;
DELIMITER |

CREATE FUNCTION gcd(x INT, y INT)
RETURNS INT
BEGIN
  SET @dividend=GREATEST(ABS(x),ABS(y));
  SET @divisor=LEAST(ABS(x),ABS(y));
  IF @divisor=0 THEN
    RETURN @dividend;
  END IF;
  SET @gcd=NULL;
  SELECT gcd INTO @gcd FROM
    (SELECT @tmp:=@dividend,
            @dividend:=@divisor AS gcd,
            @divisor:=@tmp % @divisor AS remainder
       FROM mysql.help_relation WHERE @divisor>0) AS x
    WHERE remainder=0;
  RETURN @gcd;
END;|

DELIMITER ;

SELECT gcd(12345, 9876);
```

```txt
+------------------+
| gcd(12345, 9876) |
+------------------+
|             2469 |
+------------------+
1 row in set (0.00 sec)
```


## NetRexx

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numeric digits 2000
runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Euclid's algorithm - iterative implementation
method gcdEucidI(a_, b_) public static
  loop while b_ > 0
    c_ = a_ // b_
    a_ = b_
    b_ = c_
    end
  return a_

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Euclid's algorithm - recursive implementation
method gcdEucidR(a_, b_) public static
  if b_ \= 0 then a_ = gcdEucidR(b_, a_ // b_)
  return a_

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  -- pairs of numbers, each number in the pair separated by a colon, each pair separated by a comma
  parse arg tests
  if tests = '' then
    tests = '0:0, 6:4, 7:21, 12:36, 33:77, 41:47, 99:51, 100:5, 7:23, 1989:867, 12345:9876, 40902:24140, 49865:69811, 137438691328:2305843008139952128'

  -- most of what follows is for formatting
  xiterate = 0
  xrecurse = 0
  ll_ = 0
  lr_ = 0
  lgi = 0
  lgr = 0
  loop i_ = 1 until tests = ''
    xiterate[0] = i_
    xrecurse[0] = i_
    parse tests pair ',' tests
    parse pair l_ ':' r_ .

    -- get the GCDs
    gcdi = gcdEucidI(l_, r_)
    gcdr = gcdEucidR(l_, r_)

    xiterate[i_] = l_ r_ gcdi
    xrecurse[i_] = l_ r_ gcdr
    ll_ = ll_.max(l_.strip.length)
    lr_ = lr_.max(r_.strip.length)
    lgi = lgi.max(gcdi.strip.length)
    lgr = lgr.max(gcdr.strip.length)
    end i_
  -- save formatter sizes in stems
  xiterate[-1] = ll_ lr_ lgi
  xrecurse[-1] = ll_ lr_ lgr

  -- present results
  showResults(xiterate, 'Euclid''s algorithm - iterative')
  showResults(xrecurse, 'Euclid''s algorithm - recursive')
  say
  if verifyResults(xiterate, xrecurse) then
    say 'Success: Results of iterative and recursive methods match'
  else
    say 'Error:   Results of iterative and recursive methods do not match'
  say
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method showResults(stem, title) public static
  say
  say title
  parse stem[-1] ll lr lg
  loop v_ = 1 to stem[0]
    parse stem[v_] lv rv gcd .
    say lv.right(ll)',' rv.right(lr) ':' gcd.right(lg)
    end v_
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method verifyResults(stem1, stem2) public static returns boolean
  if stem1[0] \= stem2[0] then signal BadArgumentException
  T = (1 == 1)
  F = \T
  verified = T
  loop i_ = 1 to stem1[0]
    if stem1[i_] \= stem2[i_] then do
      verified = F
      leave i_
      end
    end i_
  return verified

```

Output:

```txt
Euclid's algorithm - iterative
           0,                   0 :      0
           6,                   4 :      2
           7,                  21 :      7
          12,                  36 :     12
          33,                  77 :     11
          41,                  47 :      1
          99,                  51 :      3
         100,                   5 :      5
           7,                  23 :      1
        1989,                 867 :     51
       12345,                9876 :   2469
       40902,               24140 :     34
       49865,               69811 :   9973
137438691328, 2305843008139952128 : 262144

Euclid's algorithm - recursive
           0,                   0 :      0
           6,                   4 :      2
           7,                  21 :      7
          12,                  36 :     12
          33,                  77 :     11
          41,                  47 :      1
          99,                  51 :      3
         100,                   5 :      5
           7,                  23 :      1
        1989,                 867 :     51
       12345,                9876 :   2469
       40902,               24140 :     34
       49865,               69811 :   9973
137438691328, 2305843008139952128 : 262144

Success: Results of iterative and recursive methods match
```


## NewLISP

```NewLISP
(gcd 12 36)
  → 12
```


## Nial

Nial provides gcd in the standard lib.

```nial
|loaddefs 'niallib/gcd.ndf'
|gcd 6 4
=2
```

defining it for arrays

```nial
# red is the reduction operator for a sorted list
# one is termination condition
red is cull filter (0 unequal) link [mod [rest, first] , first]
one is or [= [1 first, tally], > [2 first,  first]]
gcd is fork [one, first, gcd red] sort <=
```

Using it

```nial
|gcd 9 6 3
=3
```


## Nim

Ported from Pascal example


### Recursive Euclid algorithm

```nim
proc gcd_recursive(u, v: int64): int64 =
    if u %% v != 0:
        result = gcd_recursive(v, u %% v)
    else:
        result = v
```


### Iterative Euclid algorithm

```nim
proc gcd_iterative(u1, v1: int64): int64 =
  var t: int64 = 0
  var u = u1
  var v = v1
  while v != 0:
      t = u
      u = v
      v = t %% v
  result = abs(u)
```


### Iterative binary algorithm

```nim
proc gcd_binary(u1, v1: int64): int64 =
  var t, k: int64
  var u = u1
  var v = v1
  u = abs(u)
  v = abs(v)
  if u < v:
      t = u
      u = v
      v = t
  if v == 0:
    result = u
  else:
    k = 1
    while (u %% 2 == 0) and (v %% 2 == 0):
      u = u shl 1
      v = v shl 1
      k = k shr 1
    if (u %% 2) == 0:
      t = u
    else:
      t = -v
    while t != 0:
      while (t %% 2) == 0:
        t = t div 2
      if t > 0:
        u = t
      else:
        v = -t
      t = u - v
    result = u * k

echo ("GCD(", 49865, ", ", 69811, "): ", gcd_iterative(49865, 69811), " (iterative)")
echo ("GCD(", 49865, ", ", 69811, "): ", gcd_recursive(49865, 69811), " (recursive)")
echo ("GCD(", 49865, ", ", 69811, "): ", gcd_binary   (49865, 69811), " (binary)")
```

Output:

```txt
GCD(49865, 69811): 9973 (iterative)
GCD(49865, 69811): 9973 (recursive)
GCD(49865, 69811): 9973 (binary)
```


## Oberon-2

Works with oo2c version 2

```oberon2
MODULE GCD;
(* Greatest Common Divisor *)
IMPORT
  Out;

  PROCEDURE Gcd(a,b: LONGINT):LONGINT;
  VAR
    r: LONGINT;
  BEGIN
    LOOP
      r := a MOD b;
      IF r = 0 THEN RETURN b END;
      a := b;b := r
    END
  END Gcd;
BEGIN
  Out.String("GCD of    12 and     8 : ");Out.LongInt(Gcd(12,8),4);Out.Ln;
  Out.String("GCD of   100 and     5 : ");Out.LongInt(Gcd(100,5),4);Out.Ln;
  Out.String("GCD of     7 and    23 : ");Out.LongInt(Gcd(7,23),4);Out.Ln;
  Out.String("GCD of    24 and  -112 : ");Out.LongInt(Gcd(12,8),4);Out.Ln;
  Out.String("GCD of 40902 and 24140 : ");Out.LongInt(Gcd(40902,24140),4);Out.Ln
END GCD.
```

Output:

```txt
GCD of    12 and     8 :    4
GCD of   100 and     5 :    5
GCD of     7 and    23 :    1
GCD of    24 and  -112 :    4
GCD of 40902 and 24140 :   34
```


## Objeck

```objeck
bundle Default {
  class GDC {
    function : Main(args : String[]), Nil {
      for(x := 1; x < 36; x += 1;) {
        IO.Console->GetInstance()->Print("GCD of ")->Print(36)->Print(" and ")->Print(x)->Print(" is ")->PrintLine(GDC(36, x));
      };
    }

    function : native : GDC(a : Int, b : Int), Int {
      t : Int;

      if(a > b) {
        t := b;  b := a;  a := t;
      };

      while (b <> 0) {
        t := a % b;  a := b;  b := t;
      };

      return a;
    }
  }
}
```


## OCaml

```ocaml
let rec gcd a b =
  if      a = 0 then b
  else if b = 0 then a
  else if a > b then gcd b (a mod b)
  else               gcd a (b mod a)
```

A little more idiomatic version:

```ocaml
let rec gcd1 a b =
  match (a mod b) with
    0 -> b
  | r -> gcd1 b r
```


### Built-in

```ocaml
#load "nums.cma";;
open Big_int;;
let gcd a b =
  int_of_big_int (gcd_big_int (big_int_of_int a) (big_int_of_int b))
```


## Octave

```octave
r = gcd(a, b)
```


## Oforth

gcd is already defined into Integer class :

```Oforth
128 96 gcd
```

Source of this method is (see Integer.of file) :

```Oforth
Integer method: gcd  self while ( dup ) [ tuck mod ] drop ;
```


## Ol

```scheme
(print (gcd 1071 1029))
; ==> 21
```


## Order

Translated from bc.

```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8gcd ORDER_PP_FN( \
8fn(8U, 8V,                            \
    8if(8isnt_0(8V), 8gcd(8V, 8remainder(8U, 8V)), 8U)))
// No support for negative numbers
```


## Oz

```oz
declare
  fun {UnsafeGCD A B}
     if B == 0 then
        A
     else
        {UnsafeGCD B A mod B}
     end
  end

  fun {GCD A B}
     if A == 0 andthen B == 0 then
        raise undefined(gcd 0 0) end
     else
        {UnsafeGCD {Abs A} {Abs B}}
     end
  end
in
  {Show {GCD 456 ~632}}
```


## PARI/GP

```parigp
gcd(a,b)
```

```pascal
program GCF (INPUT, OUTPUT);
  var
    a,b,c:integer;
  begin
    writeln('Enter 1st number');
    read(a);
    writeln('Enter 2nd number');
    read(b);
    while (a*b<>0)
      do
      begin
        c:=a;
        a:=b mod a;
        b:=c;
      end;
    writeln('GCF :=', a+b );
  end.
```

## Pascal / Delphi / Free Pascal

### Recursive Euclid algorithm

```pascal
function gcd_recursive(u, v: longint): longint;
  begin
    if u mod v <> 0 then
        gcd_recursive := gcd_recursive(v, u mod v)
    else
        gcd_recursive := v;
  end;
```


### Iterative Euclid algorithm

```fortran
function gcd_iterative(u, v: longint): longint;
  var
    t: longint;
  begin
    while v <> 0 do
    begin
      t := u;
      u := v;
      v := t mod v;
    end;
    gcd_iterative := abs(u);
  end;
```


### Iterative binary algorithm

```Pascal
function gcd_binary(u, v: longint): longint;
  var
    t, k: longint;
  begin
    u := abs(u);
    v := abs(v);
    if u < v then
    begin
      t := u;
      u := v;
      v := t;
    end;
    if v = 0 then
      gcd_binary := u
    else
    begin
      k := 1;
      while (u mod 2 = 0) and (v mod 2 = 0) do
      begin
        u := u >> 1;
        v := v >> 1;
	k := k << 1;
      end;
      if u mod 2 = 0 then
        t := u
      else
        t := -v;
      while t <> 0 do
      begin
        while t mod 2 = 0 do
          t := t div 2;
        if t > 0 then
          u := t
        else
          v := -t;
        t := u - v;
      end;
      gcd_binary := u * k;
    end;
  end;
```

Demo program:

```pascal
Program GreatestCommonDivisorDemo(output);
begin
  writeln ('GCD(', 49865, ', ', 69811, '): ', gcd_iterative(49865, 69811), ' (iterative)');
  writeln ('GCD(', 49865, ', ', 69811, '): ', gcd_recursive(49865, 69811), ' (recursive)');
  writeln ('GCD(', 49865, ', ', 69811, '): ', gcd_binary   (49865, 69811), ' (binary)');
end.
```

Output:

```txt
GCD(49865, 69811): 9973 (iterative)
GCD(49865, 69811): 9973 (recursive)
GCD(49865, 69811): 9973 (binary)
```


## Perl

### Iterative Euclid algorithm

```perl
sub gcd_iter($$) {
  my ($u, $v) = @_;
  while ($v) {
    ($u, $v) = ($v, $u % $v);
  }
  return abs($u);
}
```


### Recursive Euclid algorithm

```perl
sub gcd($$) {
  my ($u, $v) = @_;
  if ($v) {
    return gcd($v, $u % $v);
  } else {
    return abs($u);
  }
}
```


### Iterative binary algorithm

```perl
sub gcd_bin($$) {
  my ($u, $v) = @_;
  $u = abs($u);
  $v = abs($v);
  if ($u < $v) {
    ($u, $v) = ($v, $u);
  }
  if ($v == 0) {
    return $u;
  }
  my $k = 1;
  while ($u & 1 == 0 && $v & 1 == 0) {
    $u >>= 1;
    $v >>= 1;
    $k <<= 1;
  }
  my $t = ($u & 1) ? -$v : $u;
  while ($t) {
    while ($t & 1 == 0) {
      $t >>= 1;
    }
    if ($t > 0) {
      $u = $t;
    } else {
      $v = -$t;
    }
    $t = $u - $v;
  }
  return $u * $k;
}
```


### Modules

All three modules will take large integers as input, e.g.
<tt>gcd("68095260063025322303723429387", "51306142182612010300800963053")</tt>.
Other possibilities are Math::Cephes euclid, Math::GMPz gcd and gcd_ui.

```perl
# Fastest, takes multiple inputs
use Math::Prime::Util "gcd";
$gcd = gcd(49865, 69811);

# In CORE.  Slowest, takes multiple inputs,
result is a Math::BigInt unless converted
use Math::BigInt;
$gcd = Math::BigInt::bgcd(49865, 69811)->numify;

# Result is a Math::Pari object unless converted
use Math::Pari "gcd";
$gcd = gcd(49865, 69811)->pari2iv
```


### Notes on performance

```perl
use Benchmark qw(cmpthese);
use Math::BigInt;
use Math::Pari;
use Math::Prime::Util;

my $u = 40902;
my $v = 24140;
cmpthese(-5, {
  'gcd_rec' => sub { gcd($u, $v); },
  'gcd_iter' => sub { gcd_iter($u, $v); },
  'gcd_bin' => sub { gcd_bin($u, $v); },
  'gcd_bigint' => sub { Math::BigInt::bgcd($u,$v)->numify(); },
  'gcd_pari' => sub { Math::Pari::gcd($u,$v)->pari2iv(); },
  'gcd_mpu' => sub { Math::Prime::Util::gcd($u,$v); },
});
```

Output on 'Intel i3930k 4.2GHz' / Linux / Perl 5.20:

```txt
                Rate gcd_bigint   gcd_bin   gcd_rec  gcd_iter gcd_pari   gcd_mpu
gcd_bigint   39939/s         --      -83%      -94%      -95%     -98%      -99%
gcd_bin     234790/s       488%        --      -62%      -70%     -88%      -97%
gcd_rec     614750/s      1439%      162%        --      -23%     -68%      -91%
gcd_iter    793422/s      1887%      238%       29%        --     -58%      -89%
gcd_pari   1896544/s      4649%      708%      209%      139%       --      -73%
gcd_mpu    7114798/s     17714%     2930%     1057%      797%     275%        --
```


## Perl 6

### Iterative

```perl6
sub gcd (Int $a is copy, Int $b is copy) {
   $a & $b == 0 and fail;
   ($a, $b) = ($b, $a % $b) while $b;
   return abs $a;
}
```


### Recursive

```perl6
multi gcd (0,      0)      { fail }
multi gcd (Int $a, 0)      { abs $a }
multi gcd (Int $a, Int $b) { gcd $b, $a % $b }
```


### Concise

```perl6
my &gcd = { ($^a.abs, $^b.abs, * % * ... 0)[*-2] }
```


### Built-in infix

```perl6
my $gcd = $a gcd $b;
```

Because it's an infix, you can use it with various meta-operators:

```perl6
[gcd] @list;         # reduce with gcd
@alist Zgcd @blist;  # lazy zip with gcd
@alist Xgcd @blist;  # lazy cross with gcd
@alist »gcd« @blist; # parallel gcd
```


## Phix

result is always positive, except for gcd(0,0) which is 0

atom parameters allow greater precision,
but any fractional parts are immediately and deliberately discarded.

Actually, it is an autoinclude, reproduced below.
The first parameter can be a sequence,
in which case the second parameter (if provided) is ignored.

```Phix
function gcd(object u, atom v=0)
atom t
    if sequence(u) then
        v = u[1]                        -- (for the typecheck)
        t = floor(abs(v))
        for i=2 to length(u) do
            v = u[i]                    -- (for the typecheck)
            t = gcd(t,v)
        end for
        return t
    end if
    u = floor(abs(u))
    v = floor(abs(v))
    while v do
        t = u
        u = v
        v = remainder(t, v)
    end while
    return u
end function
```

Sample results:

```txt
gcd(0,0)            -- 0
gcd(24,-112)        -- 8
gcd(0, 10)          -- 10
gcd(10, 0)          -- 10
gcd(-10, 0)         -- 10
gcd(0, -10)         -- 10
gcd(9, 6)           -- 3
gcd(6, 9)           -- 3
gcd(-6, 9)          -- 3
gcd(9, -6)          -- 3
gcd(6, -9)          -- 3
gcd(-9, 6)          -- 3
gcd(40902, 24140)   -- 34
gcd(70000000000000000000,
    60000000000000000000000)
 -- 10000000000000000000
gcd({57,0,-45,-18,90,447}) -- 3
```


## PicoLisp

```PicoLisp
(de gcd (A B)
   (until (=0 B)
      (let M (% A B)
         (setq A B B M) ) )
   (abs A) )
```


## PHP

### Iterative

```php
function gcdIter($n, $m) {
    while(true) {
        if($n == $m) {
            return $m;
        }
        if($n > $m) {
            $n -= $m;
        } else {
            $m -= $n;
        }
    }
}
```


### Recursive

```php
function gcdRec($n, $m)
{
    if($m > 0)
        return gcdRec($m, $n % $m);
    else
        return abs($n);
}
```


## PL/I

```PL/I
GCD: procedure (a, b) returns (fixed binary (31)) recursive;
   declare (a, b) fixed binary (31);

   if b = 0 then return (a);

   return (GCD (b, mod(a, b)) );

end GCD;
```


## Pop11

### Built-in gcd

```pop11
gcd_n(15, 12, 2) =>
```

Note: the last argument gives the number of other arguments
(in this case 2).


### Iterative Euclid algorithm

```pop11
define gcd(k, l) -> r;
    lvars k , l, r = l;
    abs(k) -> k;
    abs(l) -> l;
    if k < l then (k, l) -> (l, k) endif;
    while l /= 0 do
        (l, k rem l) -> (k, l)
    endwhile;
    k -> r;
enddefine;
```


## PostScript

```postscript
/gcd {
{
    {0 gt} {dup rup mod} {pop exit} ifte
} loop
}.
```

With no external lib, recursive

```postscript
/gcd {
   dup 0 ne {
      dup 3 1 roll mod gcd
   } { pop } ifelse
} def
```


## PowerShell

### Recursive Euclid Algorithm

```powershell
function Get-GCD ($x, $y)
{
  if ($x -eq $y) { return $y }
  if ($x -gt $y) {
    $a = $x
    $b = $y
  }
  else {
    $a = $y
    $b = $x
  }
  while ($a % $b -ne 0) {
    $tmp = $a % $b
    $a = $b
    $b = $tmp
  }
  return $b
}
```

or shorter (taken from Python implementation)

```powershell
function Get-GCD ($x, $y) {
  if ($y -eq 0) { $x } else { Get-GCD $y ($x%$y) }
}
```


### Iterative Euclid Algorithm

Based on Python implementation

```powershell
Function Get-GCD( $x, $y ) {
    while ($y -ne 0) {
        $x, $y = $y, ($x % $y)
    }
    [Math]::abs($x)
}
```


## Prolog

### Recursive Euclid Algorithm

```prolog
gcd(X, 0, X):- !.
gcd(0, X, X):- !.
gcd(X, Y, D):- X > Y, !, Z is X mod Y, gcd(Y, Z, D).
gcd(X, Y, D):- Z is Y mod X, gcd(X, Z, D).
```


### Repeated Subtraction

```prolog
gcd(X, 0, X):- !.
gcd(0, X, X):- !.
gcd(X, Y, D):- X =< Y, !, Z is Y - X, gcd(X, Z, D).
gcd(X, Y, D):- gcd(Y, X, D).
```


## PureBasic

'''Iterative'''

```PureBasic
Procedure GCD(x, y)
  Protected r
  While y <> 0
    r = x % y
    x = y
    y = r
  Wend
  ProcedureReturn y
EndProcedure
```

'''Recursive'''

```PureBasic
Procedure GCD(x, y)
  Protected r
  r = x % y
  If (r > 0)
    y = GCD(y, r)
  EndIf
  ProcedureReturn y
EndProcedure
```


## Purity

```Purity
data Iterate = f => FoldNat <const id, g => $g . $f>

data Sub = Iterate Pred
data IsZero = <const True, const False> . UnNat

data Eq = FoldNat
          <
              const IsZero,
              eq => n => IfThenElse (IsZero $n)
                         False
                         ($eq (Pred $n))
          >

data step = gcd => n => m =>
                    IfThenElse (Eq $m $n)
                        (Pair $m $n)
                        (IfThenElse (Compare Leq $n $m)
                            ($gcd (Sub $m $n) $m)
                            ($gcd (Sub $n $m) $n))

data gcd = Iterate (gcd => uncurry (step (curry $gcd)))
```


## Python

### Built-in

Works with Python 2.6+

```python
from fractions import gcd
```

Works with Python 3.7
(Note that `fractions.gcd` is now deprecated in Python 3)

```python
from math import gcd
```


### Iterative Euclid algorithm

```python
def gcd_iter(u, v):
    while v:
        u, v = v, u % v
    return abs(u)
```


### Recursive Euclid algorithm

Interpreter: Python 2.5

```python
def gcd(u, v):
    return gcd(v, u % v) if v else abs(u)
```


### Tests

```txt
>>> gcd(0,0)
0
>>> gcd(0, 10) == gcd(10, 0) == gcd(-10, 0) == gcd(0, -10) == 10
True
>>> gcd(9, 6) == gcd(6, 9) == gcd(-6, 9) == gcd(9, -6) == gcd(6, -9) == gcd(-9, 6) == 3
True
>>> gcd(8, 45) == gcd(45, 8) == gcd(-45, 8) == gcd(8, -45) == gcd(-8, 45) == gcd(45, -8) == 1
True
>>> gcd(40902, 24140) # check Knuth :)
34
```


### Iterative binary algorithm

See [[The Art of Computer Programming]] by Knuth (Vol.2)

```python
def gcd_bin(u, v):
    u, v = abs(u), abs(v) # u >= 0, v >= 0
    if u < v:
        u, v = v, u # u >= v >= 0
    if v == 0:
        return u

    # u >= v > 0
    k = 1
    while u & 1 == 0 and v & 1 == 0: # u, v - even
        u >>= 1; v >>= 1
        k <<= 1

    t = -v if u & 1 else u
    while t:
        while t & 1 == 0:
            t >>= 1
        if t > 0:
            u = t
        else:
            v = -t
        t = u - v
    return u * k
```


### Notes on performance

<tt>gcd(40902, 24140)</tt> takes about '''17''' µsec (Euclid, not built-in)

<tt>gcd_iter(40902, 24140)</tt> takes about '''11''' µsec

<tt>gcd_bin(40902, 24140)</tt>  takes about '''41''' µsec


## Qi

```Qi
(define gcd
  A 0 -> A
  A B -> (gcd B (MOD A B)))
```


## R

Recursive:

```R
"%gcd%" <- function(u, v) {
  ifelse(u %% v != 0, v %gcd% (u%%v), v)
}
```

Iterative:

```R
"%gcd%" <- function(v, t) {
  while ( (c <- v%%t) != 0 ) {
    v <- t
    t <- c
  }
  t
}
```

Output:

```txt
> print(50 %gcd% 75)
[1] 25
```


## Racket

Racket provides a built-in gcd function.
Here's a program that computes the gcd of 14 and 63:

```racket
#lang racket

(gcd 14 63)
```

Here's an explicit implementation.
Note that since Racket is tail-calling,
the memory behavior of this program is "loop-like",
in the sense that this program will consume no more memory
than a loop-based implementation.


```racket
#lang racket

;; given two nonnegative integers, produces their greatest
;; common divisor using Euclid's algorithm
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

;; some test cases!
(module+ test
  (require rackunit)
  (check-equal? (gcd (* 2 3 3 7 7)
                     (* 3 3 7 11))
                (* 3 3 7))
  (check-equal? (gcd 0 14) 14)
  (check-equal? (gcd 13 0) 13))
```


## Rascal

### Iterative Euclidean algorithm

```rascal
public int gcd_iterative(int a, b){
	if(a == 0) return b;
	while(b != 0){
		if(a > b) a -= b;
		else b -= a;}
	return a;
}
```

An example:

```rascal
rascal>gcd_iterative(1989, 867)
int: 51
```


### Recursive Euclidean algorithm

```rascal
public int gcd_recursive(int a, b){
	return (b == 0) ? a : gcd_recursive(b, a%b);
}
```

An example:

```rascal
rascal>gcd_recursive(1989, 867)
int: 51
```


## Raven

### Recursive Euclidean algorithm

```Raven
define gcd use $u, $v
   $v 0 > if
      $u $v %   $v  gcd
   else
      $u abs

24140 40902 gcd
```

Output:

```txt
34
```


## REBOL

```rebol
gcd: func [
    {Returns the greatest common divisor of m and n.}
    m [integer!]
    n [integer!]
    /local k
] [
    ; Euclid's algorithm
    while [n > 0] [
        k: m
        m: n
        n: k // m
    ]
    m
]
```


## Retro

This is from the math extensions library.

```Retro
: gcd ( ab-n ) [ tuck mod dup ] while drop ;
```


## REXX

### version 1

The GCD subroutine can handle any number of arguments,
it can also handle any number of integers within any

argument(s), making it easier to use when computing Frobenius numbers
(also known as ''postage stamp'' or ''coin'' numbers).

```rexx
/*REXX program calculates the  GCD (Greatest Common Divisor)  of any number of integers.*/
numeric digits 2000                              /*handle up to 2k decimal dig integers.*/
call gcd 0 0            ;    call gcd 55 0     ;       call gcd 0    66
call gcd 7,21           ;    call gcd 41,47    ;       call gcd 99 , 51
call gcd 24, -8         ;    call gcd -36, 9   ;       call gcd -54, -6
call gcd 14 0 7         ;    call gcd 14 7 0   ;       call gcd 0  14 7
call gcd 15 10 20 30 55 ;    call gcd 137438691328  2305843008139952128 /*◄──2 perfect#s*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gcd: procedure;  $=;              do i=1 for  arg();  $=$ arg(i);  end       /*arg list.*/
     parse var $ x z .;  if x=0  then x=z;   x=abs(x)                        /* 0 case? */

        do j=2  to words($);   y=abs(word($,j));       if y=0  then iterate  /*is zero? */
              do until _==0;  _=x//y;  x=y;  y=_;  end /* ◄────────── the heavy lifting.*/
        end   /*j*/

     say 'GCD (Greatest Common Divisor) of '   translate(space($),",",' ')   "  is  "   x
     return x
```

Output:

```txt
GCD (Greatest Common Divisor) of  0,0   is   0
GCD (Greatest Common Divisor) of  55,0   is   55
GCD (Greatest Common Divisor) of  0,66   is   66
GCD (Greatest Common Divisor) of  7,21   is   7
GCD (Greatest Common Divisor) of  41,47   is   1
GCD (Greatest Common Divisor) of  99,51   is   3
GCD (Greatest Common Divisor) of  24,-8   is   8
GCD (Greatest Common Divisor) of  -36,9   is   9
GCD (Greatest Common Divisor) of  -54,-6   is   6
GCD (Greatest Common Divisor) of  14,0,7   is   7
GCD (Greatest Common Divisor) of  14,7,0   is   7
GCD (Greatest Common Divisor) of  0,14,7   is   7
GCD (Greatest Common Divisor) of  15,10,20,30,55   is   5
GCD (Greatest Common Divisor) of  137438691328,2305843008139952128   is   262144
```


### version 2

Recursive function (as in PL/I):

```REXX
/* REXX ***************************************************************
* using PL/I code extended to many arguments
* 17.08.2012 Walter Pachl
* 18.08.2012 gcd(0,0)=0
**********************************************************************/
numeric digits 300                  /*handle up to 300 digit numbers.*/
Call test  7,21     ,'7 '
Call test  4,7      ,'1 '
Call test 24,-8     ,'8'
Call test 55,0      ,'55'
Call test 99,15     ,'3 '
Call test 15,10,20,30,55,'5'
Call test 496,8128  ,'16'
Call test 496,8128  ,'8'            /* test wrong expectation        */
Call test 0,0       ,'0'            /* by definition                 */
Exit

test:
/**********************************************************************
* Test the gcd function
**********************************************************************/
n=arg()                             /* Number of arguments           */
gcde=arg(n)                         /* Expected result               */
gcdx=gcd(arg(1),arg(2))             /* gcd of the first 2 numbers    */
Do i=2 To n-2                       /* proceed with all the others   */
  If arg(i+1)<>0 Then
    gcdx=gcd(gcdx,arg(i+1))
  End
If gcdx=arg(arg()) Then             /* result is as expected         */
  tag='as expected'
Else                                /* result is not correct         */
  Tag='*** wrong. expected:' gcde
numbers=arg(1)                      /* build string to show the input*/
Do i=2 To n-1
  numbers=numbers 'and' arg(i)
  End
say left('the GCD of' numbers 'is',45) right(gcdx,3) tag
Return

GCD: procedure
/**********************************************************************
* Recursive procedure as shown in PL/I
**********************************************************************/
Parse Arg a,b
if b = 0 then return abs(a)
return GCD(b,a//b)
```

Output:

```txt
the GCD of 7 and 21 is                          7 as expected
the GCD of 4 and 7 is                           1 as expected
the GCD of 24 and -8 is                         8 as expected
the GCD of 55 and 0 is                         55 as expected
the GCD of 99 and 15 is                         3 as expected
the GCD of 15 and 10 and 20 and 30 and 55 is    5 as expected
the GCD of 496 and 8128 is                     16 as expected
the GCD of 496 and 8128 is                     16 *** wrong. expected: 8
the GCD of 0 and 0 is                           0 as expected
```


### version 3

Translated from REXX}} using different argument handlin.

Use as `gcd(a,b,c,---)`
Considerably faster than version 1 (and version 2)

See <http://rosettacode.org/wiki/Least_common_multiple#REXX> for reasoning.

```rexx
gcd: procedure
x=abs(arg(1))
do j=2 to arg()
  y=abs(arg(j))
  If y<>0 Then Do
    do until z==0
      z=x//y
      x=y
      y=z
      end
    end
  end
return x
```


## Ring

```ring
see gcd (24, 32)
func gcd gcd, b
     while b
           c   = gcd
           gcd = b
           b   = c % b
     end
     return gcd

```


## Ruby

That is already available as the `gcd` method of integers:

```ruby
40902.gcd(24140)  # => 34
```

Here's an implementation:

```ruby
def gcd(u, v)
  u, v = u.abs, v.abs
  while v > 0
    u, v = v, u % v
  end
  u
end
```


## Run BASIC

```Runbasic
print abs(gcd(-220,160))
function gcd(gcd,b)
    while b
        c   = gcd
        gcd = b
        b   = c mod b
    wend
end function
```


## Rust

### num crate

```rust
extern crate num;
use num::integer::gcd;
```


### Iterative Euclid algorithm

```rust
fn gcd(mut m: i32, mut n: i32) -> i32 {
   while m != 0 {
       let old_m = m;
       m = n % m;
       n = old_m;
   }
   n.abs()
}
```


### Recursive Euclid algorithm

```rust
fn gcd(m: i32, n: i32) -> i32 {
   if m == 0 {
      n.abs()
   } else {
      gcd(n % m, m)
   }
}
```

### Stein's Algorithm

Stein's algorithm is very much like Euclid's
except that it uses bitwise operators
(and consequently slightly more performant)
and the integers must be unsigned.
The following is a recursive implementation
that leverages Rust's pattern matching.

```rust
use std::cmp::{min, max};
fn gcd(a: usize, b: usize) -> usize {
    match ((a, b), (a & 1, b & 1)) {
        ((x, y), _) if x == y               => y,
        ((0, x), _) | ((x, 0), _)           => x,
        ((x, y), (0, 1)) | ((y, x), (1, 0)) => gcd(x >> 1, y),
        ((x, y), (0, 0))                    => gcd(x >> 1, y >> 1) << 1,
        ((x, y), (1, 1))                    => { let (x, y) = (min(x, y), max(x, y));
                                                 gcd((y - x) >> 1, x)
                                               }
        _                                   => unreachable!(),
    }
}
```


### Tests

```rust
   println!("{}",gcd(399,-3999));
   println!("{}",gcd(0,3999));
   println!("{}",gcd(13*13,13*29));

3
3999
13
```


## Sather

Translated from bc.

```sather
class MATH is

  gcd_iter(u, v:INT):INT is
    loop while!( v.bool );
      t ::= u; u := v; v := t % v;
    end;
    return u.abs;
  end;

  gcd(u, v:INT):INT is
    if v.bool then return gcd(v, u%v); end;
    return u.abs;
  end;


  private swap(inout a, inout b:INT) is
    t ::= a;
    a := b;
    b := t;
  end;

  gcd_bin(u, v:INT):INT is
    t:INT;

    u := u.abs; v := v.abs;
    if u < v then swap(inout u, inout v); end;
    if v = 0 then return u; end;
    k ::= 1;
    loop while!( u.is_even and v.is_even );
      u := u / 2; v := v / 2;
      k := k * 2;
    end;
    if u.is_even then
      t := -v;
    else
      t := u;
    end;
    loop while!( t.bool );
      loop while!( t.is_even );
        t := t / 2;
      end;
      if t > 0 then
        u := t;
      else
        v := -t;
      end;
      t := u - v;
    end;
    return u * k;
  end;

end;
```

```sather
class MAIN is
  main is
    a ::= 40902;
    b ::= 24140;
    #OUT + MATH::gcd_iter(a, b) + "\n";
    #OUT + MATH::gcd(a, b) + "\n";
    #OUT + MATH::gcd_bin(a, b) + "\n";
    -- built in
    #OUT + a.gcd(b) + "\n";
  end;
end;
```


## Sass/SCSS

Iterative Euclid's Algorithm

```coffeescript
@function gcd($a,$b) {
	@while $b > 0 {
		$c: $a % $b;
		$a: $b;
		$b: $c;
	}
	@return $a;
}
```


## Scala

```scala
def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
```

Using pattern matching

```scala
@tailrec
def gcd(a: Int, b: Int): Int = {
  b match {
    case 0 => a
    case _ => gcd(b, (a % b))
  }
}
```


## Scheme

```scheme
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))
```

or using the standard function included with Scheme (takes any number of arguments):

```scheme
(gcd a b)
```


## Sed

```sed
#! /bin/sed -nf

# gcd.sed Copyright (c) 2010        by Paweł Zuzelski <pawelz@pld-linux.org>
# dc.sed  Copyright (c) 1995 - 1997 by Greg Ubben <gsu@romulus.ncsc.mil>

# usage:
#
#     echo N M | ./gcd.sed
#
# Computes the greatest common divisor of N and M integers using euclidean
# algorithm.

s/^/|P|K0|I10|O10|?~/

s/$/ [lalb%sclbsalcsblb0<F]sF sasblFxlap/

:next
s/|?./|?/
s/|?#[	 -}]*/|?/
/|?!*[lLsS;:<>=]\{0,1\}$/N
/|?!*[-+*/%^<>=]/b binop
/^|.*|?[dpPfQXZvxkiosStT;:]/b binop
/|?[_0-9A-F.]/b number
/|?\[/b string
/|?l/b load
/|?L/b Load
/|?[sS]/b save
/|?c/ s/[^|]*//
/|?d/ s/[^~]*~/&&/
/|?f/ s//&[pSbz0<aLb]dSaxsaLa/
/|?x/ s/\([^~]*~\)\(.*|?x\)~*/\2\1/
/|?[KIO]/ s/.*|\([KIO]\)\([^|]*\).*|?\1/\2~&/
/|?T/ s/\.*0*~/~/
#  a slow, non-stackable array implementation in dc, just for completeness
#  A fast, stackable, associative array implementation could be done in sed
#  (format: {key}value{key}value...), but would be longer, like load & save.
/|?;/ s/|?;\([^{}]\)/|?~[s}s{L{s}q]S}[S}l\1L}1-d0>}s\1L\1l{xS\1]dS{xL}/
/|?:/ s/|?:\([^{}]\)/|?~[s}L{s}L{s}L}s\1q]S}S}S{[L}1-d0>}S}l\1s\1L\1l{xS\1]dS{x/
/|?[ ~	cdfxKIOT]/b next
/|?\n/b next
/|?[pP]/b print
/|?k/ s/^\([0-9]\{1,3\}\)\([.~].*|K\)[^|]*/\2\1/
/|?i/ s/^\(-\{0,1\}[0-9]*\.\{0,1\}[0-9]\{1,\}\)\(~.*|I\)[^|]*/\2\1/
/|?o/ s/^\(-\{0,1\}[1-9][0-9]*\.\{0,1\}[0-9]*\)\(~.*|O\)[^|]*/\2\1/
/|?[kio]/b pop
/|?t/b trunc
/|??/b input
/|?Q/b break
/|?q/b quit
h
/|?[XZz]/b count
/|?v/b sqrt
s/.*|?\([^Y]\).*/\1 is unimplemented/
s/\n/\\n/g
l
g
b next

:print
/^-\{0,1\}[0-9]*\.\{0,1\}[0-9]\{1,\}~.*|?p/!b Print
/|O10|/b Print

#  Print a number in a non-decimal output base.  Uses registers a,b,c,d.
#  Handles fractional output bases (O<-1 or O>=1), unlike other dc's.
#  Converts the fraction correctly on negative output bases, unlike
#  UNIX dc.  Also scales the fraction more accurately than UNIX dc.
#
s,|?p,&KSa0kd[[-]Psa0la-]Sad0>a[0P]sad0=a[A*2+]saOtd0>a1-ZSd[[[[ ]P]sclb1\
!=cSbLdlbtZ[[[-]P0lb-sb]sclb0>c1+]sclb0!<c[0P1+dld>c]scdld>cscSdLbP]q]Sb\
[t[1P1-d0<c]scd0<c]ScO_1>bO1!<cO[16]<bOX0<b[[q]sc[dSbdA>c[A]sbdA=c[B]sbd\
B=c[C]sbdC=c[D]sbdD=c[E]sbdE=c[F]sb]xscLbP]~Sd[dtdZOZ+k1O/Tdsb[.5]*[.1]O\
X^*dZkdXK-1+ktsc0kdSb-[Lbdlb*lc+tdSbO*-lb0!=aldx]dsaxLbsb]sad1!>a[[.]POX\
+sb1[SbO*dtdldx-LbO*dZlb!<a]dsax]sadXd0<asbsasaLasbLbscLcsdLdsdLdLak[]pP,
b next

:Print
/|?p/s/[^~]*/&\
~&/
s/\(.*|P\)\([^|]*\)/\
\2\1/
s/\([^~]*\)\n\([^~]*\)\(.*|P\)/\1\3\2/
h
s/~.*//
/./{ s/.//; p; }
#  Just s/.//p would work if we knew we were running under the -n option.
#  Using l vs p would kind of do \ continuations, but would break strings.
g

:pop
s/[^~]*~//
b next

:load
s/\(.*|?.\)\(.\)/\20~\1/
s/^\(.\)0\(.*|r\1\([^~|]*\)~\)/\1\3\2/
s/.//
b next

:Load
s/\(.*|?.\)\(.\)/\2\1/
s/^\(.\)\(.*|r\1\)\([^~|]*~\)/|\3\2/
/^|/!i\
register empty
s/.//
b next

:save
s/\(.*|?.\)\(.\)/\2\1/
/^\(.\).*|r\1/ !s/\(.\).*|/&r\1|/
/|?S/ s/\(.\).*|r\1/&~/
s/\(.\)\([^~]*~\)\(.*|r\1\)[^~|]*~\{0,1\}/\3\2/
b next

:quit
t quit
s/|?[^~]*~[^~]*~/|?q/
t next
#  Really should be using the -n option to avoid printing a final newline.
s/.*|P\([^|]*\).*/\1/
q

:break
s/[0-9]*/&;987654321009;/
:break1
s/^\([^;]*\)\([1-9]\)\(0*\)\([^1]*\2\(.\)[^;]*\3\(9*\).*|?.\)[^~]*~/\1\5\6\4/
t break1
b pop

:input
N
s/|??\(.*\)\(\n.*\)/|?\2~\1/
b next

:count
/|?Z/ s/~.*//
/^-\{0,1\}[0-9]*\.\{0,1\}[0-9]\{1,\}$/ s/[-.0]*\([^.]*\)\.*/\1/
/|?X/ s/-*[0-9A-F]*\.*\([0-9A-F]*\).*/\1/
s/|.*//
/~/ s/[^~]//g

s/./a/g
:count1
	s/a\{10\}/b/g
	s/b*a*/&a9876543210;/
	s/a.\{9\}\(.\).*;/\1/
	y/b/a/
/a/b count1
G
/|?z/ s/\n/&~/
s/\n[^~]*//
b next

:trunc
#  for efficiency, doesn't pad with 0s, so 10k 2 5/ returns just .40
#  The X* here and in a couple other places works around a SunOS 4.x sed bug.
s/\([^.~]*\.*\)\(.*|K\([^|]*\)\)/\3;9876543210009909:\1,\2/
:trunc1
	s/^\([^;]*\)\([1-9]\)\(0*\)\([^1]*\2\(.\)[^:]*X*\3\(9*\)[^,]*\),\([0-9]\)/\1\5\6\4\7,/
t trunc1
s/[^:]*:\([^,]*\)[^~]*/\1/
b normal

:number
s/\(.*|?\)\(_\{0,1\}[0-9A-F]*\.\{0,1\}[0-9A-F]*\)/\2~\1~/
s/^_/-/
/^[^A-F~]*~.*|I10|/b normal
/^[-0.]*~/b normal
s:\([^.~]*\)\.*\([^~]*\):[Ilb^lbk/,\1\2~0A1B2C3D4E5F1=11223344556677889900;.\2:
:digit
    s/^\([^,]*\),\(-*\)\([0-F]\)\([^;]*\(.\)\3[^1;]*\(1*\)\)/I*+\1\2\6\5~,\2\4/
t digit
s:...\([^/]*.\)\([^,]*\)[^.]*\(.*|?.\):\2\3KSb[99]k\1]SaSaXSbLalb0<aLakLbktLbk:
b next

:string
/|?[^]]*$/N
s/\(|?[^]]*\)\[\([^]]*\)]/\1|{\2|}/
/|?\[/b string
s/\(.*|?\)|{\(.*\)|}/\2~\1[/
s/|{/[/g
s/|}/]/g
b next

:binop
/^[^~|]*~[^|]/ !i\
stack empty
//!b next
/^-\{0,1\}[0-9]*\.\{0,1\}[0-9]\{1,\}~/ !s/[^~]*\(.*|?!*[^!=<>]\)/0\1/
/^[^~]*~-\{0,1\}[0-9]*\.\{0,1\}[0-9]\{1,\}~/ !s/~[^~]*\(.*|?!*[^!=<>]\)/~0\1/
h
/|?\*/b mul
/|?\//b div
/|?%/b rem
/|?^/b exp

/|?[+-]/ s/^\(-*\)\([^~]*~\)\(-*\)\([^~]*~\).*|?\(-\{0,1\}\).*/\2\4s\3o\1\3\5/
s/\([^.~]*\)\([^~]*~[^.~]*\)\(.*\)/<\1,\2,\3|=-~.0,123456789<></
/^<\([^,]*,[^~]*\)\.*0*~\1\.*0*~/ s/</=/
:cmp1
	s/^\(<[^,]*\)\([0-9]\),\([^,]*\)\([0-9]\),/\1,\2\3,\4/
t cmp1
/^<\([^~]*\)\([^~]\)[^~]*~\1\(.\).*|=.*\3.*\2/ s/</>/
/|?/{
	s/^\([<>]\)\(-[^~]*~-.*\1\)\(.\)/\3\2/
	s/^\(.\)\(.*|?!*\)\1/\2!\1/
	s/|?![^!]\(.\)/&l\1x/
	s/[^~]*~[^~]*~\(.*|?\)!*.\(.*\)|=.*/\1\2/
	b next
}
s/\(-*\)\1|=.*/;9876543210;9876543210/
/o-/ s/;9876543210/;0123456789/
s/^>\([^~]*~\)\([^~]*~\)s\(-*\)\(-*o\3\(-*\)\)/>\2\1s\5\4/

s/,\([0-9]*\)\.*\([^,]*\),\([0-9]*\)\.*\([0-9]*\)/\1,\2\3.,\4;0/
:right1
	s/,\([0-9]\)\([^,]*\),;*\([0-9]\)\([0-9]*\);*0*/\1,\2\3,\4;0/
t right1
s/.\([^,]*\),~\(.*\);0~s\(-*\)o-*/\1~\30\2~/

:addsub1
	s/\(.\{0,1\}\)\(~[^,]*\)\([0-9]\)\(\.*\),\([^;]*\)\(;\([^;]*\(\3[^;]*\)\).*X*\1\(.*\)\)/\2,\4\5\9\8\7\6/
	s/,\([^~]*~\).\{10\}\(.\)[^;]\{0,9\}\([^;]\{0,1\}\)[^;]*/,\2\1\3/
#	  could be done in one s/// if we could have >9 back-refs...
/^~.*~;/!b addsub1

:endbin
s/.\([^,]*\),\([0-9.]*\).*/\1\2/
G
s/\n[^~]*~[^~]*//

:normal
s/^\(-*\)0*\([0-9.]*[0-9]\)[^~]*/\1\2/
s/^[^1-9~]*~/0~/
b next

:mul
s/\(-*\)\([0-9]*\)\.*\([0-9]*\)~\(-*\)\([0-9]*\)\.*\([0-9]*\).*|K\([^|]*\).*/\1\4\2\5.!\3\6,|\2<\3~\5>\6:\7;9876543210009909/

:mul1
    s/![0-9]\([^<]*\)<\([0-9]\{0,1\}\)\([^>]*\)>\([0-9]\{0,1\}\)/0!\1\2<\3\4>/
    /![0-9]/ s/\(:[^;]*\)\([1-9]\)\(0*\)\([^0]*\2\(.\).*X*\3\(9*\)\)/\1\5\6\4/
/<~[^>]*>:0*;/!t mul1

s/\(-*\)\1\([^>]*\).*/;\2^>:9876543210aaaaaaaaa/

:mul2
    s/\([0-9]~*\)^/^\1/
    s/<\([0-9]*\)\(.*[~^]\)\([0-9]*\)>/\1<\2>\3/

    :mul3
	s/>\([0-9]\)\(.*\1.\{9\}\(a*\)\)/\1>\2;9\38\37\36\35\34\33\32\31\30/
	s/\(;[^<]*\)\([0-9]\)<\([^;]*\).*\2[0-9]*\(.*\)/\4\1<\2\3/
	s/a[0-9]/a/g
	s/a\{10\}/b/g
	s/b\{10\}/c/g
    /|0*[1-9][^>]*>0*[1-9]/b mul3

    s/;/a9876543210;/
    s/a.\{9\}\(.\)[^;]*\([^,]*\)[0-9]\([.!]*\),/\2,\1\3/
    y/cb/ba/
/|<^/!b mul2
b endbin

:div
#  CDDET
/^[-.0]*[1-9]/ !i\
divide by 0
//!b pop
s/\(-*\)\([0-9]*\)\.*\([^~]*~-*\)\([0-9]*\)\.*\([^~]*\)/\2.\3\1;0\4.\5;0/
:div1
	s/^\.0\([^.]*\)\.;*\([0-9]\)\([0-9]*\);*0*/.\1\2.\3;0/
	s/^\([^.]*\)\([0-9]\)\.\([^;]*;\)0*\([0-9]*\)\([0-9]\)\./\1.\2\30\4.\5/
t div1
s/~\(-*\)\1\(-*\);0*\([^;]*[0-9]\)[^~]*/~123456789743222111~\2\3/
s/\(.\(.\)[^~]*\)[^9]*\2.\{8\}\(.\)[^~]*/\3~\1/
s,|?.,&SaSadSaKdlaZ+LaX-1+[sb1]Sbd1>bkLatsbLa[dSa2lbla*-*dLa!=a]dSaxsakLasbLb*t,
b next

:rem
s,|?%,&Sadla/LaKSa[999]k*Lak-,
b next

:exp
#  This decimal method is just a little faster than the binary method done
#  totally in dc:  1LaKLb [kdSb*LbK]Sb [[.5]*d0ktdSa<bkd*KLad1<a]Sa d1<a kk*
/^[^~]*\./i\
fraction in exponent ignored
s,[^-0-9].*,;9d**dd*8*d*d7dd**d*6d**d5d*d*4*d3d*2lbd**1lb*0,
:exp1
	s/\([0-9]\);\(.*\1\([d*]*\)[^l]*\([^*]*\)\(\**\)\)/;dd*d**d*\4\3\5\2/
t exp1
G
s,-*.\{9\}\([^9]*\)[^0]*0.\(.*|?.\),\2~saSaKdsaLb0kLbkK*+k1\1LaktsbkLax,
s,|?.,&SadSbdXSaZla-SbKLaLadSb[0Lb-d1lb-*d+K+0kkSb[1Lb/]q]Sa0>a[dk]sadK<a[Lb],
b next

:sqrt
#  first square root using sed:  8k2v at 1:30am Dec 17, 1996
/^-/i\
square root of negative number
/^[-0]/b next
s/~.*//
/^\./ s/0\([0-9]\)/\1/g
/^\./ !s/[0-9][0-9]/7/g
G
s/\n/~/
s,|?.,&K1+k KSbSb[dk]SadXdK<asadlb/lb+[.5]*[sbdlb/lb+[.5]*dlb>a]dsaxsasaLbsaLatLbk K1-kt,
b next

#  END OF GSU dc.sed
```


## Seed7

```seed7
const func integer: gcd (in var integer: a, in var integer: b) is func
  result
    var integer: gcd is 0;
  local
    var integer: help is 0;
  begin
    while a <> 0 do
      help := b rem a;
      b := a;
      a := help;
    end while;
    gcd := b;
  end func;
```

Original source: <http://seed7.sourceforge.net/algorith/math.htm#gcd>


## SequenceL

Tail Recursive Greatest Common Denominator using Euclidian Algorithm

```sequencel
gcd(a, b) :=
		a when b = 0
	else
		gcd(b, a mod b);
```


## SETL

```setl
a := 33; b := 77;
print(" the gcd of",a," and ",b," is ",gcd(a,b));

c := 49865; d := 69811;
print(" the gcd of",c," and ",d," is ",gcd(c,d));

proc gcd (u, v);
  return if v = 0 then abs u else gcd (v, u mod v) end;
end;
```

Output:

```setl
the gcd of 33  and  77  is  11
the gcd of 49865  and  69811  is  9973
```


## Sidef

### Built-in

```ruby
var arr = [100, 1_000, 10_000, 20];
say Math.gcd(arr...);
```


### Recursive Euclid algorithm

```ruby
func gcd(a, b) {
    b.is_zero ? a.abs : gcd(b, a % b);
}
```


## Simula

For a recursive variant,
see [[Sum multiples of 3 and 5#Simula|Sum multiples of 3 and 5]].

```algolw
BEGIN
    INTEGER PROCEDURE GCD(a, b); INTEGER a, b;
    BEGIN
        IF a = 0 THEN a := b
        ELSE
            WHILE 0 < b DO BEGIN INTEGER i;
                i := MOD(a, b); a := b; b := i;
            END;
        GCD := a
    END;

    INTEGER a, b;
    !outint(SYSOUT.IMAGE.MAIN.LENGTH, 0);!OUTIMAGE;!OUTIMAGE;
    !SYSOUT.IMAGE :- BLANKS(132);  ! this may or may not work;
    FOR b := 1 STEP 5 UNTIL 37 DO BEGIN
        FOR a := 0 STEP 2 UNTIL 21 DO BEGIN
            OUTTEXT("  ("); OUTINT(a, 0);
            OUTCHAR(','); OUTINT(b, 2);
            OUTCHAR(')'); OUTINT(GCD(a, b), 3);
        END;
        OUTIMAGE
    END
END
```

Output:

```txt
(0, 1)  1  (2, 1)  1  (4, 1)  1  (6, 1)  1  (8, 1)  1  (10, 1)  1  (12, 1)  1  (14, 1)  1  (16, 1)  1  (18, 1)  1  (20, 1)  1
(0, 6)  6  (2, 6)  2  (4, 6)  2  (6, 6)  6  (8, 6)  2  (10, 6)  2  (12, 6)  6  (14, 6)  2  (16, 6)  2  (18, 6)  6  (20, 6)  2
(0,11) 11  (2,11)  1  (4,11)  1  (6,11)  1  (8,11)  1  (10,11)  1  (12,11)  1  (14,11)  1  (16,11)  1  (18,11)  1  (20,11)  1
(0,16) 16  (2,16)  2  (4,16)  4  (6,16)  2  (8,16)  8  (10,16)  2  (12,16)  4  (14,16)  2  (16,16) 16  (18,16)  2  (20,16)  4
(0,21) 21  (2,21)  1  (4,21)  1  (6,21)  3  (8,21)  1  (10,21)  1  (12,21)  3  (14,21)  7  (16,21)  1  (18,21)  3  (20,21)  1
(0,26) 26  (2,26)  2  (4,26)  2  (6,26)  2  (8,26)  2  (10,26)  2  (12,26)  2  (14,26)  2  (16,26)  2  (18,26)  2  (20,26)  2
(0,31) 31  (2,31)  1  (4,31)  1  (6,31)  1  (8,31)  1  (10,31)  1  (12,31)  1  (14,31)  1  (16,31)  1  (18,31)  1  (20,31)  1
(0,36) 36  (2,36)  2  (4,36)  4  (6,36)  6  (8,36)  4  (10,36)  2  (12,36) 12  (14,36)  2  (16,36)  4  (18,36) 18  (20,36)  4

```


## Slate

Slate's Integer type has gcd defined:

```slate
40902 gcd: 24140
```


### Iterative Euclid algorithm

```slate
x@(Integer traits) gcd: y@(Integer traits)
"Euclid's algorithm for finding the greatest common divisor."
[| n m temp |
  n: x.
  m: y.
  [n isZero] whileFalse: [temp: n. n: m \\ temp. m: temp].
  m abs
].
```


### Recursive Euclid algorithm

```slate
x@(Integer traits) gcd: y@(Integer traits)
[
  y isZero
    ifTrue: [x]
    ifFalse: [y gcd: x \\ y]
].
```


## Smalltalk

The <tt>Integer</tt> class has its <tt>gcd</tt> method.

```smalltalk
(40902 gcd: 24140) displayNl
```

An reimplementation of the Iterative Euclid's algorithm would be:

```smalltalk
|gcd_iter|

gcd_iter := [ :a :b |
  |u v|
   u := a. v := b.
   [ v > 0 ]
     whileTrue: [ |t|
        t := u.
        u := v.
        v := t rem: v
     ].
   u abs
].

(gcd_iter value: 40902 value: 24140) printNl.
```


## SNOBOL4

```snobol
	define('gcd(i,j)')	:(gcd_end)
gcd	?eq(i,0)	:s(freturn)
	?eq(j,0)	:s(freturn)

loop	gcd = remdr(i,j)
	gcd = ?eq(gcd,0) j	:s(return)
	i = j
	j = gcd			:(loop)
gcd_end

	output = gcd(1071,1029)
end
```


## Sparkling

```sparkling
function factors(n) {
	var f = {};

	for var i = 2; n > 1; i++ {
		while n % i == 0 {
			n /= i;
			f[i] = f[i] != nil ? f[i] + 1 : 1;
		}
	}

	return f;
}

function GCD(n, k) {
	let f1 = factors(n);
	let f2 = factors(k);

	let fs = map(f1, function(factor, multiplicity) {
		let m = f2[factor];
		return m == nil ? 0 : min(m, multiplicity);
	});

	let rfs = {};
	foreach(fs, function(k, v) {
		rfs[sizeof rfs] = pow(k, v);
	});

	return reduce(rfs, 1, function(x, y) { return x * y; });
}

function LCM(n, k) {
	return n * k / GCD(n, k);
}
```


## SQL

Demonstration of Oracle 12c WITH Clause Enhancements

```SQL
drop table tbl;
create table tbl
(
        u       number,
        v       number
);

insert into tbl ( u, v ) values ( 20, 50 );
insert into tbl ( u, v ) values ( 21, 50 );
insert into tbl ( u, v ) values ( 21, 51 );
insert into tbl ( u, v ) values ( 22, 50 );
insert into tbl ( u, v ) values ( 22, 55 );

commit;

with
        function gcd ( ui in number, vi in number )
        return number
        is
                u number := ui;
                v number := vi;
                t number;
        begin
                while v > 0
                loop
                        t := u;
                        u := v;
                        v:= mod(t, v );
                end loop;
                return abs(u);
        end gcd;
        select u, v, gcd ( u, v )
        from tbl
/

```

Output:

```txt
Table dropped.

Table created.

1 row created.

1 row created.

1 row created.

1 row created.

1 row created.

Commit complete.

         U          V   GCD(U,V)
---------- ---------- ----------
        20         50         10
        21         50          1
        21         51          3
        22         50          2
        22         55         11
```

Demonstration of SQL Server 2008

```SQL
CREATE FUNCTION gcd (
  @ui INT,
  @vi INT
) RETURNS INT

AS

BEGIN
    DECLARE @t INT
    DECLARE @u INT
    DECLARE @v INT

    SET @u = @ui
    SET @v = @vi

    WHILE @v > 0
    BEGIN
        SET @t = @u;
        SET @u = @v;
        SET @v = @t % @v;
    END;
    RETURN abs( @u );
END

GO

CREATE TABLE tbl (
  u INT,
  v INT
);

INSERT INTO tbl ( u, v ) VALUES ( 20, 50 );
INSERT INTO tbl ( u, v ) VALUES ( 21, 50 );
INSERT INTO tbl ( u, v ) VALUES ( 21, 51 );
INSERT INTO tbl ( u, v ) VALUES ( 22, 50 );
INSERT INTO tbl ( u, v ) VALUES ( 22, 55 );

SELECT u, v, dbo.gcd ( u, v )
  FROM tbl;

DROP TABLE tbl;

DROP FUNCTION gcd;
```

PostgreSQL function using a recursive common table expression

```SQL
CREATE FUNCTION gcd(integer, integer)
RETURNS integer
LANGUAGE sql
AS $function$
WITH RECURSIVE x (u, v) AS (
  SELECT ABS($1), ABS($2)
  UNION
  SELECT v, u % v FROM x WHERE v > 0
)
SELECT min(u) FROM x;
$function$
```

Output:

```txt
postgres> select gcd(40902, 24140);
gcd
-----
34
SELECT 1
Time: 0.012s
```


## Stata

```stata
function gcd(a_,b_) {
	a = abs(a_)
	b = abs(b_)
	while (b>0) {
		a = mod(a,b)
		swap(a,b)
	}
	return(a)
}
```


## Swift

```Swift
// Iterative

func gcd(var a: Int, var b: Int) -> Int {

    a = abs(a); b = abs(b)

    if (b > a) { swap(&a, &b) }

    while (b > 0) { (a, b) = (b, a % b) }

    return a
}

// Recursive

func gcdr (var a: Int, var b: Int) -> Int {

    a = abs(a); b = abs(b)

    if (b > a) { swap(&a, &b) }

    return gcd_rec(a,b)
}


private func gcd_rec(a: Int, b: Int) -> Int {

    return b == 0 ? a : gcd_rec(b, a % b)
}


for (a,b) in [(1,1), (100, -10), (10, -100), (-36, -17), (27, 18), (30, -42)] {

    println("Iterative: GCD of \(a) and \(b) is \(gcd(a, b))")
    println("Recursive: GCD of \(a) and \(b) is \(gcdr(a, b))")
}
```

Output:

```txt
Iterative: GCD of 1 and 1 is 1
Recursive: GCD of 1 and 1 is 1
Iterative: GCD of 100 and -10 is 10
Recursive: GCD of 100 and -10 is 10
Iterative: GCD of 10 and -100 is 10
Recursive: GCD of 10 and -100 is 10
Iterative: GCD of -36 and -17 is 1
Recursive: GCD of -36 and -17 is 1
Iterative: GCD of 27 and 18 is 9
Recursive: GCD of 27 and 18 is 9
Iterative: GCD of 30 and -42 is 6
Recursive: GCD of 30 and -42 is 6
```


## Tcl

### Iterative Euclid algorithm

```tcl
package require Tcl 8.5
namespace path {::tcl::mathop ::tcl::mathfunc}
proc gcd_iter {p q} {
    while {$q != 0} {
        lassign [list $q [% $p $q]] p q
    }
    abs $p
}
```


### Recursive Euclid algorithm

```tcl
proc gcd {p q} {
    if {$q == 0} {
        return $p
    }
    gcd $q [expr {$p % $q}]
}
```

With Tcl 8.6, this can be optimized slightly to:

```tcl
proc gcd {p q} {
    if {$q == 0} {
        return $p
    }
    tailcall gcd $q [expr {$p % $q}]
}
```

(Tcl does not perform automatic tail-call optimization introduction
because that makes any potential error traces less informative.)


### Iterative binary algorithm

```tcl
package require Tcl 8.5
namespace path {::tcl::mathop ::tcl::mathfunc}
proc gcd_bin {p q} {
    if {$p == $q} {return [abs $p]}
    set p [abs $p]
    if {$q == 0} {return $p}
    set q [abs $q]
    if {$p < $q} {lassign [list $q $p] p q}
    set k 1
    while {($p & 1) == 0 && ($q & 1) == 0} {
        set p [>> $p 1]
        set q [>> $q 1]
        set k [<< $k 1]
    }
    set t [expr {$p & 1 ? -$q : $p}]
    while {$t} {
        while {$t & 1 == 0} {set t [>> $t 1]}
        if {$t > 0} {set p $t} {set q [- $t]}
        set t [- $p $q]
    }
    return [* $p $k]
}
```


### Notes on performance

```tcl
foreach proc {gcd_iter gcd gcd_bin} {
    puts [format "%-8s - %s" $proc [time {$proc $u $v} 100000]]
}
```

Outputs:

```txt
gcd_iter - 4.46712 microseconds per iteration
gcd      - 5.73969 microseconds per iteration
gcd_bin  - 9.25613 microseconds per iteration
```


## TI-83 BASIC, TI-89 BASIC

```basic
 gcd(A,B)
```

The `)` can be omitted in TI-83 basic


## TSE SAL

```TSE SAL
INTEGER PROC FNMathGetGreatestCommonDivisorI( INTEGER x1I, INTEGER x2I )
 //
 IF ( x2I == 0 )
  //
  RETURN( x1I )
  //
 ENDIF
 //
 RETURN( FNMathGetGreatestCommonDivisorI( x2I, x1I MOD x2I ) )
 //
END

PROC Main()
 STRING s1[255] = "353"
 STRING s2[255] = "46"
 REPEAT
  IF ( NOT ( Ask( " = ", s1, _EDIT_HISTORY_ ) ) AND ( Length( s1 ) > 0 ) ) RETURN() ENDIF
  IF ( NOT ( Ask( " = ", s2, _EDIT_HISTORY_ ) ) AND ( Length( s2 ) > 0 ) ) RETURN() ENDIF
  Warn( FNMathGetGreatestCommonDivisorI( Val( s1 ), Val( s2 ) ) ) // gives e.g. 1
 UNTIL FALSE
END
```


## TXR

```bash
$ txr -p '(gcd (expt 2 123) (expt 6 49))'
562949953421312
```


## TypeScript

Iterative implementation

```javascript
function gcd(a: number, b: number) {
  a = Math.abs(a);
  b = Math.abs(b);

  if (b > a) {
    let temp = a;
    a = b;
    b = temp;
  }

  while (true) {
    a %= b;
    if (a === 0) { return b; }
    b %= a;
    if (b === 0) { return a; }
  }
}
```

Recursive:

```javascript
function gcd_rec(a: number, b: number) {
  return b ? gcd_rec(b, a % b) : Math.abs(a);
}
```


## uBasic/4tH

Translated from BBC BASIC.

```basic
Print "GCD of 18 : 12 = "; FUNC(_GCD_Iterative_Euclid(18,12))
Print "GCD of 1071 : 1029 = "; FUNC(_GCD_Iterative_Euclid(1071,1029))
Print "GCD of 3528 : 3780 = "; FUNC(_GCD_Iterative_Euclid(3528,3780))

End

_GCD_Iterative_Euclid Param(2)
  Local (1)
  Do While b@
    c@ = a@
    a@ = b@
    b@ = c@ % b@
  Loop
Return (Abs(a@))
```

Output:

```txt
GCD of 18 : 12 = 6
GCD of 1071 : 1029 = 21
GCD of 3528 : 3780 = 252

0 OK, 0:205
```


## UNIX Shell

Works with Bourne Shell

```bash
gcd() {
	# Calculate $1 % $2 until $2 becomes zero.
	until test 0 -eq "$2"; do
		# Parallel assignment: set -- 1 2
		set -- "$2" "`expr "$1" % "$2"`"
	done

	# Echo absolute value of $1.
	test 0 -gt "$1" && set -- "`expr 0 - "$1"`"
	echo "$1"
}

gcd -47376 87843
# => 987
```


## C Shell

```csh
alias gcd eval \''set gcd_args=( \!*:q )	\\
	@ gcd_u=$gcd_args[2]			\\
	@ gcd_v=$gcd_args[3]			\\
	while ( $gcd_v != 0 )			\\
		@ gcd_t = $gcd_u % $gcd_v	\\
		@ gcd_u = $gcd_v		\\
		@ gcd_v = $gcd_t		\\
	end					\\
	if ( $gcd_u < 0 ) @ gcd_u = - $gcd_u	\\
	@ $gcd_args[1]=$gcd_u			\\
'\'

gcd result -47376 87843
echo $result
# => 987
```


## Ursa

```ursa
import "math"
out (gcd 40902 24140) endl console
```

Output:

```txt
34
```


## Ursala

This doesn't need to be defined because it's a library function, but
it can be defined like this based on a recursive implementation of
Euclid's algorithm. This isn't the simplest possible solution because
it includes a bit shifting optimization that happens when both operands
are even.

```Ursala
#import nat

gcd = ~&B?\~&Y ~&alh^?\~&arh2faltPrXPRNfabt2RCQ @a ~&ar^?\~&al ^|R/~& ^/~&r remainder
```

test program:

```Ursala
#cast %nWnAL

test = ^(~&,gcd)* <(25,15),(36,16),(120,45),(30,100)>
```

Output:

```txt
<
   (25,15): 5,
   (36,16): 4,
   (120,45): 15,
   (30,100): 10>
```


## V

Like joy


### iterative

```v
 [gcd
    [0 >] [dup rollup %]
    while
    pop
 ].
```

### recursive

like python

```v
 [gcd
    [zero?] [pop]
       [swap [dup] dip swap %]
    tailrec].
```

same with view: (swap [dup] dip swap % is replaced with a destructuring view)

```v
 [gcd
    [zero?] [pop]
      [[a b : [b a b %]] view i]
    tailrec].
```

Running it:

```txt
 |1071 1029 gcd
 =21
```


## VBA

```vb
Function gcd(u As Long, v As Long) As Long
    Dim t As Long
    Do While v
        t = u
        u = v
        v = t Mod v
    Loop
    gcd = u
End Function
```

This function uses repeated subtractions.
Simple but not very efficient.

```VBA
Public Function GCD(a As Long, b As Long) As Long
While a <> b
  If a > b Then a = a - b Else b = b - a
Wend
GCD = a
End Function
```

Output:

```txt
print GCD(1280, 240)
 80
print GCD(3475689, 23566319)
 7
a=123456789
b=234736437
print GCD((a),(b))
 3
```

A note on the last example: using brackets forces a and b
to be evaluated before GCD is called.
Not doing this will cause a compile error,
because a and b are not the same type as in the function declaration
(they are Variant, not Long).
Alternatively you can use the conversion function CLng
as in print GCD(CLng(a),CLng(b))


## VBScript

```VBScript
Function GCD(a,b)
	Do
		If a Mod b > 0 Then
			c = a Mod b
			a = b
			b = c
		Else
			GCD = b
			Exit Do
		End If
	Loop
End Function

WScript.Echo "The GCD of 48 and 18 is " & GCD(48,18) & "."
WScript.Echo "The GCD of 1280 and 240 is " & GCD(1280,240) & "."
WScript.Echo "The GCD of 1280 and 240 is " & GCD(3475689,23566319) & "."
WScript.Echo "The GCD of 1280 and 240 is " & GCD(123456789,234736437) & "."
```

Output:

```txt
The GCD of 48 and 18 is 6.
The GCD of 1280 and 240 is 80.
The GCD of 1280 and 240 is 7.
The GCD of 1280 and 240 is 3.
```


## Verilog

```Verilog
module gcd
  (
  input reset_l,
  input clk,

  input [31:0] initial_u,
  input [31:0] initial_v,
  input load,

  output reg [31:0] result,
  output reg busy
  );

reg [31:0] u, v;

always @(posedge clk or negedge reset_l)
  if (!reset_l)
    begin
      busy <= 0;
      u <= 0;
      v <= 0;
    end
  else
    begin

      result <= u + v; // Result (one of them will be zero)

      busy <= u && v; // We're still busy...

      // Repeatedly subtract smaller number from larger one
      if (v <= u)
        u <= u - v;
      else if (u < v)
        v <= v - u;

      if (load) // Load new problem when high
        begin
          u <= initial_u;
          v <= initial_v;
          busy <= 1;
        end

    end

endmodule
```


## Visual Basic

Works with Visual Basic 5.
Works with Visual Basic 6.
Works with VBA 6.5.
Works with VBA 7.1.

```vb
Function GCD(ByVal a As Long, ByVal b As Long) As Long
Dim h As Long

    If a Then
        If b Then
            Do
                h = a Mod b
                a = b
                b = h
            Loop While b
        End If
        GCD = Abs(a)
    Else
        GCD = Abs(b)
    End If

End Function

Sub Main()
' testing the above function

  Debug.Assert GCD(12, 18) = 6
  Debug.Assert GCD(1280, 240) = 80
  Debug.Assert GCD(240, 1280) = 80
  Debug.Assert GCD(-240, 1280) = 80
  Debug.Assert GCD(240, -1280) = 80
  Debug.Assert GCD(0, 0) = 0
  Debug.Assert GCD(0, 1) = 1
  Debug.Assert GCD(1, 0) = 1
  Debug.Assert GCD(3475689, 23566319) = 7
  Debug.Assert GCD(123456789, 234736437) = 3
  Debug.Assert GCD(3780, 3528) = 252

End Sub
```


## Wortel

Operator

```wortel
@gcd a b
```

Number expression

```wortel
!#~kg a b
```

Iterative

```wortel
&[a b] [@vars[t] @while b @:{t b b %a b a t} a]
```

Recursive

```wortel
&{gcd a b} ?{b !!gcd b %a b @abs a}
```


## x86 Assembly

Using GNU Assembler syntax:

```8086 Assembly
.text
.global pgcd

pgcd:
        push    %ebp
        mov     %esp, %ebp

        mov     8(%ebp), %eax
        mov     12(%ebp), %ecx
        push    %edx

.loop:
        cmp     $0, %ecx
        je      .end
        xor     %edx, %edx
        div     %ecx
        mov     %ecx, %eax
        mov     %edx, %ecx
        jmp     .loop

.end:
        pop     %edx
        leave
        ret
```


## XLISP

`GCD` is a built-in function.
If we wanted to reimplement it,
one (tail-recursive) way would be like this:

```lisp
(defun greatest-common-divisor (x y)
	(if (= y 0)
		x
		(greatest-common-divisor y (mod x y)) ) )
```


## XPL0

```XPL0
include c:\cxpl\codes;

func GCD(U, V); \Return the greatest common divisor of U and V
int  U, V;
int  T;
[while V do     \Euclid's method
    [T:= U;  U:= V;  V:= rem(T/V)];
return abs(U);
];

\Display the GCD of two integers entered on command line
IntOut(0, GCD(IntIn(8), IntIn(8)))
```


## Yabasic

```Yabasic
sub gcd(u, v)
    local t

    u = int(abs(u))
    v = int(abs(v))
    while(v)
        t = u
        u = v
        v = mod(t, v)
    wend
    return u
end sub

print "Greatest common divisor: ", gcd(12345, 9876)
```


## Z80 Assembly

Uses the iterative subtraction implementation of Euclid's algorithm,
because the Z80 does not implement modulus or division opcodes.

```z80
; Inputs: a, b
; Outputs: a = gcd(a, b)
; Destroys: c
; Assumes: a and b are positive one-byte integers
gcd:
    cp b
    ret z                   ; while a != b

    jr c, else              ; if a > b

    sub b                   ; a = a - b

    jr gcd

else:
    ld c, a                 ; Save a
    ld a, b                 ; Swap b into a so we can do the subtraction
    sub c                   ; b = b - a
    ld b, a                 ; Put a and b back where they belong
    ld a, c

    jr gcd
```


## zkl

This is a method on integers:

```zkl
(123456789).gcd(987654321) //-->9
```

Using the gnu big num library (GMP):

```zkl
var BN=Import("zklBigNum");
BN(123456789).gcd(987654321) //-->9
```

or

```zkl
fcn gcd(a,b){ while(b){ t:=a; a=b; b=t%b } a.abs() }
```


## ZX Spectrum Basic

```zxbasic
10 FOR n=1 TO 3
20 READ a,b
30 PRINT "GCD of ";a;" and ";b;" = ";
40 GO SUB 70
50 NEXT n
60 STOP
70 IF b=0 THEN PRINT ABS (a): RETURN
80 LET c=a: LET a=b: LET b=FN m(c,b): GO TO 70
90 DEF FN m(a,b)=a-INT (a/b)*b
100 DATA 12,16,22,33,45,67
```
