+++
title = "Modular inverse"
description = ""
date = 2019-10-09T15:23:36Z
aliases = []
[extra]
id = 12637
[taxonomies]
categories = ["task"]
tags = []
+++

From [http://en.wikipedia.org/wiki/Modular_multiplicative_inverse Wikipedia]:

In [[wp:modular arithmetic|modular arithmetic]],   the '''modular multiplicative inverse''' of an [[integer]]   <big> ''a'' </big>   [[wp:modular arithmetic|modulo]]   <big> ''m'' </big>   is an integer   <big> ''x'' </big>   such that

::<math>a\,x \equiv 1 \pmod{m}.</math>

Or in other words, such that:

::<math>\exists k \in\Z,\qquad a\, x = 1 + k\,m</math>

It can be shown that such an inverse exists   if and only if   <big> ''a'' </big>   and   <big> ''m'' </big>   are [[wp:coprime|coprime]],   but we will ignore this for this task.


## Task

Either by implementing the algorithm, by using a dedicated library or by using a built-in function in
your language,   compute the modular inverse of   42 modulo 2017.




## 8th


```Forth

\ return "extended gcd" of a and b; The result satisfies the equation:
\     a*x + b*y = gcd(a,b)
: n:xgcd \ a b --  gcd x y
  dup 0 n:= if
    1 swap            \ -- a 1 0
  else
    tuck n:/mod
    -rot recurse
    tuck 4 roll
    n:* n:neg n:+
  then ;

\ Return modular inverse of n modulo mod, or null if it doesn't exist (n and mod
\ not coprime):
: n:invmod \ n mod -- invmod
  dup >r
  n:xgcd rot 1 n:= not if
    2drop null
  else
    drop dup 0 n:< if r@ n:+ then
  then
  rdrop ;

42 2017 n:invmod . cr bye

```

```txt

1969

```



## Ada



```Ada

with Ada.Text_IO;use Ada.Text_IO;
procedure modular_inverse is
  -- inv_mod calculates the inverse of a mod n. We should have n>0 and, at the end, the contract is a*Result=1 mod n
  -- If this is false then we raise an exception (don't forget the -gnata option when you compile
  function inv_mod (a : Integer; n : Positive) return Integer with post=> (a * inv_mod'Result) mod n = 1 is
    -- To calculate the inverse we do as if we would calculate the GCD with the Euclid extended algorithm
    -- (but we just keep the coefficient on a)
    function inverse (a, b, u, v : Integer) return Integer is
     (if b=0 then u else inverse (b, a mod b, v, u-(v*a)/b));
  begin
    return inverse (a, n, 1, 0);
  end inv_mod;
begin
  -- This will output -48 (which is correct)
  Put_Line (inv_mod (42,2017)'img);
  -- The further line will raise an exception since the GCD will not be 1
  Put_Line (inv_mod (42,77)'img);
  exception when others => Put_Line ("The inverse doesn't exist.");
end modular_inverse;

```



## ALGOL 68


```algol68

BEGIN
   PROC modular inverse = (INT a, m) INT :
   BEGIN
      PROC extended gcd = (INT x, y) []INT :
CO
   Algol 68 allows us to return three INTs in several ways.  A [3]INT
   is used here but it could just as well be a STRUCT.
CO
      BEGIN
	 INT v := 1, a := 1, u := 0, b := 0, g := x, w := y;
	 WHILE w>0
	 DO
	    INT q := g % w, t := a - q * u;
	    a := u; u := t;
	    t := b - q * v;
	    b := v; v := t;
	    t := g - q * w;
	    g := w; w := t
	 OD;
	 a PLUSAB (a < 0 | u | 0);
	 (a, b, g)
      END;
      [] INT egcd = extended gcd (a, m);
      (egcd[3] > 1 | 0 | egcd[1] MOD m)
   END;
   printf (($"42 ^ -1 (mod 2017) = ", g(0)$, modular inverse (42, 2017)))
CO
   Note that if ϕ(m) is known, then a^-1 = a^(ϕ(m)-1) mod m which
   allows an alternative implementation in terms of modular
   exponentiation but, in general, this requires the factorization of
   m.  If m is prime the factorization is trivial and ϕ(m) = m-1.
   2017 is prime which may, or may not, be ironic within the context
   of the Rosetta Code conditions.
CO
END

```

```txt
42 ^ -1 (mod 2017) = 1969

```



## AutoHotkey

Translation of [http://rosettacode.org/wiki/Modular_inverse#C C].

```AutoHotkey
MsgBox, % ModInv(42, 2017)

ModInv(a, b) {
	if (b = 1)
		return 1
	b0 := b, x0 := 0, x1 :=1
	while (a > 1) {
		q := a // b
		, t  := b
		, b  := Mod(a, b)
		, a  := t
		, t  := x0
		, x0 := x1 - q * x0
		, x1 := t
	}
	if (x1 < 0)
		x1 += b0
	return x1
}
```

```txt
1969
```



## AWK


```AWK

# syntax: GAWK -f MODULAR_INVERSE.AWK
# converted from C
BEGIN {
    printf("%s\n",mod_inv(42,2017))
    exit(0)
}
function mod_inv(a,b,  b0,t,q,x0,x1) {
    b0 = b
    x0 = 0
    x1 = 1
    if (b == 1) {
      return(1)
    }
    while (a > 1) {
      q = int(a / b)
      t = b
      b = int(a % b)
      a = t
      t = x0
      x0 = x1 - q * x0
      x1 = t
    }
    if (x1 < 0) {
      x1 += b0
    }
    return(x1)
}

```

```txt

1969

```



## Batch File

Based from C's second implementation

```dos
@echo off
setlocal enabledelayedexpansion
	%== Calls the "function" ==%
call :ModInv 42 2017 result
echo !result!
call :ModInv 40 1 result
echo !result!
call :ModInv 52 -217 result
echo !result!
call :ModInv -486 217 result
echo !result!
call :ModInv 40 2018 result
echo !result!
pause>nul
exit /b 0

	%== The "function" ==%
:ModInv
	set a=%1
	set b=%2

	if !b! lss 0 (set /a b=-b)
	if !a! lss 0 (set /a a=b - ^(-a %% b^))

	set t=0&set nt=1&set r=!b!&set /a nr=a%%b

	:while_loop
	if !nr! neq 0 (
		set /a q=r/nr
		set /a tmp=nt
		set /a nt=t - ^(q*nt^)
		set /a t=tmp

		set /a tmp=nr
		set /a nr=r - ^(q*nr^)
		set /a r=tmp
		goto while_loop
	)

	if !r! gtr 1 (set %3=-1&goto :EOF)
	if !t! lss 0 set /a t+=b
	set %3=!t!
	goto :EOF
```

```txt
1969
0
96
121
-1
```



## Bracmat

```bracmat
( ( mod-inv
  =   a b b0 x0 x1 q
    .   !arg:(?a.?b)
      & ( !b:1
        |   (!b.0.1):(?b0.?x0.?x1)
          &   whl
            ' ( !a:>1
              & div$(!a.!b):?q
              & (!b.mod$(!a.!b)):(?a.?b)
              & (!x1+-1*!q*!x0.!x0):(?x0.?x1)
              )
          & (!x:>0|!x1+!b0)
        )
  )
& out$(mod-inv$(42.2017))
};
```

Output

```txt
1969
```



## C


```c
#include <stdio.h>

int mul_inv(int a, int b)
{
	int b0 = b, t, q;
	int x0 = 0, x1 = 1;
	if (b == 1) return 1;
	while (a > 1) {
		q = a / b;
		t = b, b = a % b, a = t;
		t = x0, x0 = x1 - q * x0, x1 = t;
	}
	if (x1 < 0) x1 += b0;
	return x1;
}

int main(void) {
	printf("%d\n", mul_inv(42, 2017));
	return 0;
}
```


The above method has some problems.  Most importantly, when given a pair (a,b) with no solution, it generates an FP exception.  When given <tt>b=1</tt>, it returns 1 which is not a valid result mod 1.  When given negative a or b the results are incorrect.  The following generates results that should match Pari/GP for numbers in the int range.
```c
#include <stdio.h>

int mul_inv(int a, int b)
{
        int t, nt, r, nr, q, tmp;
        if (b < 0) b = -b;
        if (a < 0) a = b - (-a % b);
        t = 0;  nt = 1;  r = b;  nr = a % b;
        while (nr != 0) {
          q = r/nr;
          tmp = nt;  nt = t - q*nt;  t = tmp;
          tmp = nr;  nr = r - q*nr;  r = tmp;
        }
        if (r > 1) return -1;  /* No inverse */
        if (t < 0) t += b;
        return t;
}
int main(void) {
        printf("%d\n", mul_inv(42, 2017));
        printf("%d\n", mul_inv(40, 1));
        printf("%d\n", mul_inv(52, -217));  /* Pari semantics for negative modulus */
        printf("%d\n", mul_inv(-486, 217));
        printf("%d\n", mul_inv(40, 2018));
        return 0;
}
```

```txt

1969
0
96
121
-1

```



## C++

```cpp
#include <iostream>
 using namespace std;

int mul_inv(int a, int b)
{
	int b0 = b, t, q;
	int x0 = 0, x1 = 1;
	if (b == 1) return 1;
	while (a > 1) {
		q = a / b;
		t = b, b = a % b, a = t;
		t = x0, x0 = x1 - q * x0, x1 = t;
	}
	if (x1 < 0) x1 += b0;
	return x1;
}

int main(void) {
	cout<<mul_inv(42, 2017)<<endl;
	return 0;
}

```


Recursive implementation

```cpp
#include <iostream>

short ObtainMultiplicativeInverse(int a, int b, int s0 = 1, int s1 = 0)
{
    return b==0? s0: ObtainMultiplicativeInverse(b, a%b, s1, s0 - s1*(a/b));
}

int main(int argc, char* argv[])
{
    std::cout << ObtainMultiplicativeInverse(42, 2017) << std::endl;
    return 0;
}

```



## C#


```c#
public class Program
{
    static void Main()
    {
        System.Console.WriteLine(42.ModInverse(2017));
    }
}

public static class IntExtensions
{
    public static int ModInverse(this int a, int m)
    {
        if (m == 1) return 0;
        int m0 = m;
        (int x, int y) = (1, 0);

        while (a > 1) {
            int q = a / m;
            (a, m) = (m, a % m);
            (x, y) = (y, x - q * y);
        }
        return x < 0 ? x + m0 : x;
    }
}
```



## Clojure


```lisp
(ns test-p.core
  (:require [clojure.math.numeric-tower :as math]))

(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn mul_inv
  " Get inverse using extended gcd.  Extended GCD returns
    gcd followed by bezout coefficients. We want the 1st coefficients
   (i.e. second of extend-gcd result).  We compute mod base so result
    is between 0..(base-1) "
  [a b]
  (let [b (if (neg? b) (- b) b)
        a (if (neg? a) (- b (mod (- a) b)) a)
        egcd (extended-gcd a b)]
      (if (= (first egcd) 1)
        (mod (second egcd) b)
        (str "No inverse since gcd is: " (first egcd)))))


(println (mul_inv 42 2017))
(println (mul_inv 40 1))
(println (mul_inv 52 -217))
(println (mul_inv -486 217))
(println (mul_inv 40 2018))


```


'''Output:'''

```txt

1969
0
96
121
No inverse since gcd is: 2

```



## Common Lisp


```lisp

;;
;; Calculates the GCD of a and b based on the Extended Euclidean Algorithm. The function also returns
;; the Bézout coefficients s and t, such that gcd(a, b) = as + bt.
;;
;; The algorithm is described on page http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Iterative_method_2
;;
(defun egcd (a b)
  (do ((r (cons b a) (cons (- (cdr r) (* (car r) q)) (car r))) ; (r+1 r) i.e. the latest is first.
       (s (cons 0 1) (cons (- (cdr s) (* (car s) q)) (car s))) ; (s+1 s)
       (u (cons 1 0) (cons (- (cdr u) (* (car u) q)) (car u))) ; (t+1 t)
       (q nil))
      ((zerop (car r)) (values (cdr r) (cdr s) (cdr u)))       ; exit when r+1 = 0 and return r s t
    (setq q (floor (/ (cdr r) (car r))))))                     ; inside loop; calculate the q

;;
;; Calculates the inverse module for a = 1 (mod m).
;;
;; Note: The inverse is only defined when a and m are coprimes, i.e. gcd(a, m) = 1.”
;;
(defun invmod (a m)
  (multiple-value-bind (r s k) (egcd a m)
    (unless (= 1 r) (error "invmod: Values ~a and ~a are not coprimes." a m))
     s))

```


```txt

* (invmod 42 2017)

-48
* (mod -48 2017)

1969

```



## D

```d
T modInverse(T)(T a, T b) pure nothrow {
    if (b == 1)
        return 1;
    T b0 = b,
      x0 = 0,
      x1 = 1;

    while (a > 1) {
        immutable q = a / b;
        auto t = b;
        b = a % b;
        a = t;
        t = x0;
        x0 = x1 - q * x0;
        x1 = t;
    }
    return (x1 < 0) ? (x1 + b0) : x1;
}

void main() {
    import std.stdio;
    writeln(modInverse(42, 2017));
}
```

```txt
1969
```



## EchoLisp


```scheme

(lib 'math) ;; for egcd = extended gcd

(define (mod-inv x m)
    (define-values (g inv q) (egcd x m))
    (unless (= 1 g) (error 'not-coprimes (list x m) ))
    (if (< inv 0) (+ m inv) inv))

(mod-inv 42 2017)  → 1969
(mod-inv 42 666)
🔴 error: not-coprimes (42 666)

```



## Elixir

```elixir
defmodule Modular do
  def extended_gcd(a, b) do
    {last_remainder, last_x} = extended_gcd(abs(a), abs(b), 1, 0, 0, 1)
    {last_remainder, last_x * (if a < 0, do: -1, else: 1)}
  end

  defp extended_gcd(last_remainder, 0, last_x, _, _, _), do: {last_remainder, last_x}
  defp extended_gcd(last_remainder, remainder, last_x, x, last_y, y) do
    quotient   = div(last_remainder, remainder)
    remainder2 = rem(last_remainder, remainder)
    extended_gcd(remainder, remainder2, x, last_x - quotient*x, y, last_y - quotient*y)
  end

  def inverse(e, et) do
      {g, x} = extended_gcd(e, et)
      if g != 1, do: raise "The maths are broken!"
      rem(x+et, et)
    end
  end

IO.puts Modular.inverse(42,2017)
```


```txt

1969

```



## ERRE


```ERRE
PROGRAM MOD_INV

!$INTEGER

PROCEDURE MUL_INV(A,B->T)
  LOCAL NT,R,NR,Q,TMP
  IF B<0 THEN B=-B
  IF A<0 THEN A=B-(-A MOD B)
  T=0  NT=1  R=B  NR=A MOD B
  WHILE NR<>0 DO
      Q=R DIV NR
      TMP=NT  NT=T-Q*NT  T=TMP
      TMP=NR  NR=R-Q*NR  R=TMP
  END WHILE
  IF (R>1) THEN T=-1 EXIT PROCEDURE  ! NO INVERSE
  IF (T<0) THEN T+=B
END PROCEDURE


BEGIN
     MUL_INV(42,2017->T) PRINT(T)
     MUL_INV(40,1->T) PRINT(T)
     MUL_INV(52,-217->T) PRINT(T)    ! pari semantics for negative modulus
     MUL_INV(-486,217->T)  PRINT(T)
     MUL_INV(40,2018->T) PRINT(T)
END PROGRAM

```

```txt

 1969
 0
 96
 121
-1

```


=={{header|F_Sharp|F#}}==

```fsharp

//Calculate the Modular Inverse: Nigel Galloway: April 3rd., 2018
let MI n g =
  let rec fN n i g e l a =
    match e with
    | 0 -> g
    | _ -> let o = n/e
           fN e l a (n-o*e) (i-o*l) (g-o*a)
  (n+(fN n 1 0 g 0 1))%n

```

```txt

MI 2017 42 -> 1969

```


## Factor

<lang>USE: math.functions
42 2017 mod-inv
```

```txt

1969

```


## Forth

ANS Forth with double-number word set

```forth

: invmod { a m | v b c -- inv }
  m to v
  1 to c
  0 to b
  begin a
  while v a / >r
     c b s>d c s>d r@ 1 m*/ d- d>s to c to b
     a v s>d a s>d r> 1 m*/ d- d>s to a to v
  repeat b m mod dup to b 0<
  if m b + else b then ;

```

ANS Forth version without locals

```forth

: modinv ( a m - inv)
  dup 1-              \ a m (m != 1)?
  if                  \ a m
    tuck 1 0          \ m0 a m 1 0
    begin             \ m0 a m inv x0
      2>r over 1 >    \ m0 a m (a > 1)?       R: inv x0
    while             \ m0 a m                R: inv x0
      tuck /mod       \ m0 m (a mod m) (a/m)  R: inv x0
      r> tuck *       \ m0 a' m' x0 (a/m)*x0  R: inv
      r> swap -       \ m0 a' m' x0 (inv-q)   R:
    repeat            \ m0 a' m' inv' x0'
    2drop             \ m0                    R: inv x0
    2r> drop          \ m0 inv                R:
    dup 0<            \ m0 inv (inv < 0)?
    if over + then    \ m0 (inv + m0)
  then                \ x inv'
  nip                 \ inv
;

```


```txt

42 2017 invmod . 1969
42 2017 modinv . 1969

```



## FreeBASIC


```freebasic
' version 10-07-2018
' compile with: fbc -s console

Type ext_euclid
    Dim As Integer a, b
End Type

' "Table method" aka "The Magic Box"
Function magic_box(x As Integer, y As Integer) As ext_euclid

    Dim As Integer a(1 To 128), b(1 To 128), d(1 To 128), k(1 To 128)

    a(1) = 1 : b(1) = 0 : d(1) = x
    a(2) = 0 : b(2) = 1 : d(2) = y : k(2) = x \ y

    Dim As Integer i = 2

    While Abs(d(i)) <> 1
        i += 1
        a(i) = a(i -2) - k(i -1) * a(i -1)
        b(i) = b(i -2) - k(i -1) * b(i -1)
        d(i) = d(i -2) Mod d(i -1)
        k(i) = d(i -1) \ d(i)
        'Print a(i),b(i),d(i),k(i)
        If d(i -1) Mod d(i) = 0 Then Exit While
    Wend

    If d(i) = -1 Then '  -1 * (ab + by) = -1 * -1 ==> -ab -by = 1
        a(i) = -a(i)
        b(i) = -b(i)
    End If

    Function = Type( a(i), b(i) )

End Function
' ------=< MAIN >=------

Dim As Integer x, y, gcd
Dim As ext_euclid result

Do
    Read x, y
    If x = 0 AndAlso y = 0 Then Exit Do
    result = magic_box(x, y)
    With result
        gcd = .a * x + .b * y
        Print "a * "; Str(x); " + b * "; Str(y);
        Print " = GCD("; Str(x); ", "; Str(y); ") ="; gcd
        If gcd > 1 Then
            Print "No solution, numbers are not coprime"
        Else
            Print "a = "; .a; ", b = ";.b
            Print "The Modular inverse of "; x; " modulo "; y; " = ";
            While .a < 0 : .a += IIf(y > 0, y, -y) : Wend
            Print .a
            'Print "The Modular inverse of "; y; " modulo "; x; " = ";
            'While .b < 0 : .b += IIf(x > 0, x, -x) : Wend
            'Print .b
        End if
    End With
    Print
Loop

Data 42, 2017
Data 40, 1
Data 52, -217
Data -486, 217
Data 40, 2018
Data 0, 0

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
a * 42 + b * 2017 = GCD(42, 2017) = 1
a = -48, b =  1
The Modular inverse of  42 modulo  2017 =  1969

a * 40 + b * 1 = GCD(40, 1) = 1
a =  0, b =  1
The Modular inverse of  40 modulo  1 =  0

a * 52 + b * -217 = GCD(52, -217) = 1
a =  96, b =  23
The Modular inverse of  52 modulo -217 =  96

a * -486 + b * 217 = GCD(-486, 217) = 1
a = -96, b = -215
The Modular inverse of -486 modulo  217 =  121

a * 40 + b * 2018 = GCD(40, 2018) = 2
No solution, numbers are not coprime
```


## FunL


```funl
import integers.egcd

def modinv( a, m ) =
    val (g, x, _) = egcd( a, m )

    if g != 1 then error( a + ' and ' + m + ' not coprime' )

    val res = x % m

    if res < 0 then res + m else res

println( modinv(42, 2017) )
```


```txt

1969

```


## Go

The standard library function uses the extended Euclidean algorithm internally.

```go
package main

import (
	"fmt"
	"math/big"
)

func main() {
	a := big.NewInt(42)
	m := big.NewInt(2017)
	k := new(big.Int).ModInverse(a, m)
	fmt.Println(k)
}
```

```txt

1969

```


=={{header|GW-BASIC}}==
```qbasic

10 ' Modular inverse
20 LET E% = 42
30 LET T% = 2017
40 GOSUB 1000
50 PRINT MODINV%
60 END

990  ' increments e stp (step) times until bal is greater than t
992  ' repeats until bal = 1 (mod = 1) and returns count
994  ' bal will not be greater than t + e
1000 LET D% = 0
1010 IF E% >= T% THEN GOTO 1140
1020  LET BAL% = E%
1025  ' At least one iteration is necessary
1030  LET STP% = ((T% - BAL%) \ E%) + 1
1040  LET BAL% = BAL% + STP% * E%
1050  LET COUNT% = 1 + STP%
1060  LET BAL% = BAL% - T%
1070  WHILE BAL% <> 1
1080   LET STP% = ((T% - BAL%) \ E%) + 1
1090   LET BAL% = BAL% + STP% * E%
1100   LET COUNT% = COUNT% + STP%
1110   LET BAL% = BAL% - T%
1120  WEND
1130  LET D% = COUNT%
1140 LET MODINV% = D%
1150 RETURN

```

```txt

 1969

```



## Haskell


```haskell
-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv :: Int -> Int -> Maybe Int
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Int -> Int -> (Int, Int, Int)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

main :: IO ()
main = mapM_ print [2 `modInv` 4, 42 `modInv` 2017]
```

```txt
Nothing
Just 1969
```


=={{header|Icon}} and {{header|Unicon}}==
```unicon
procedure main(args)
    a := integer(args[1]) | 42
    b := integer(args[2]) | 2017
    write(mul_inv(a,b))
end

procedure mul_inv(a,b)
    if b == 1 then return 1
    (b0 := b, x0 := 0, x1 := 1)
    while a > 1 do {
        q := a/b
        (t := b, b := a%b, a := t)
        (t := x0, x0 := x1-q*x0, x1 := t)
        }
    return if (x1 > 0) then x1 else x1+b0
end
```


```txt

->mi
1969
->

```


Adding a coprime test:


```unicon
link numbers

procedure main(args)
    a := integer(args[1]) | 42
    b := integer(args[2]) | 2017
    write(mul_inv(a,b))
end

procedure mul_inv(a,b)
    if b == 1 then return 1
    if gcd(a,b) ~= 1 then return "not coprime"
    (b0 := b, x0 := 0, x1 := 1)
    while a > 1 do {
        q := a/b
        (t := b, b := a%b, a := t)
        (t := x0, x0 := x1-q*x0, x1 := t)
        }
    return if (x1 > 0) then x1 else x1+b0
end
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PRINT MODINV(42,2017)
120 DEF MODINV(A,B)
130   LET B=ABS(B)
140   IF A<0 THEN LET A=B-MOD(-A,B)
150   LET T=0:LET NT=1:LET R=B:LET NR=MOD(A,B)
160   DO WHILE NR<>0
170     LET Q=INT(R/NR)
180     LET TMP=NT:LET NT=T-Q*NT:LET T=TMP
190     LET TMP=NR:LET NR=R-Q*NR:LET R=TMP
200   LOOP
210   IF R>1 THEN
220     LET MODINV=-1
230   ELSE IF T<0 THEN
240     LET MODINV=T+B
250   ELSE
260     LET MODINV=T
270   END IF
280 END DEF
```



## J

'''Solution''':
```j
   modInv =: dyad def 'x y&|@^ <: 5 p: y'"0
```

'''Example''':
```j
   42 modInv 2017
1969
```

'''Notes''':

* Calculates the modular inverse as <tt>a^( totient(m) - 1 ) mod m</tt>.

* 5 p: y is Euler's totient function of y.

* J has a fast implementation of modular exponentiation (which avoids the exponentiation altogether), invoked with the form  <tt>m&|@^</tt> (hence, we use explicitly-named arguments for this entry, as opposed to the "variable free" tacit style:  the m&| construct must freeze the value before it can be used but we want to use different values in that expression at different times...).


## Java

The <code>BigInteger</code> library has a method for this:

```java
System.out.println(BigInteger.valueOf(42).modInverse(BigInteger.valueOf(2017)));
```

```txt
1969
```



## JavaScript

Using brute force.

```javascript
var modInverse = function(a, b) {
    a %= b;
    for (var x = 1; x < b; x++) {
        if ((a*x)%b == 1) {
            return x;
        }
    }
}
```



## Julia

===Built-in===
Julia includes a built-in function for this:

```julia
invmod(a, b)
```



### C translation

The following code works on any integer type.
To maximize performance, we ensure (via a promotion rule) that the operands are the same type (and use built-ins <code>zero(T)</code> and <code>one(T)</code> for initialization of temporary variables to ensure that they remain of the same type throughout execution).

```julia
function modinv{T<:Integer}(a::T, b::T)
    b0 = b
    x0, x1 = zero(T), one(T)
    while a > 1
        q = div(a, b)
        a, b = b, a % b
        x0, x1 = x1 - q * x0, x0
    end
    x1 < 0 ? x1 + b0 : x1
end
modinv(a::Integer, b::Integer) = modinv(promote(a,b)...)
```


```txt

julia> invmod(42, 2017)
1969

julia> modinv(42, 2017)
1969

```



## Kotlin


```scala
// version 1.0.6

import java.math.BigInteger

fun main(args: Array<String>) {
    val a = BigInteger.valueOf(42)
    val m = BigInteger.valueOf(2017)
    println(a.modInverse(m))
}
```


```txt

1969

```



## Maple



```Maple

1/42 mod 2017;

```

```txt

                                    1969

```



## Mathematica

The built-in function <code>FindInstance</code> works well for this

```Mathematica
modInv[a_, m_] :=
 Block[{x,k}, x /. FindInstance[a x == 1 + k m, {x, k}, Integers]]
```


Another way by using the built-in function <code>PowerMod</code> :

```Mathematica
PowerMod[a,-1,m]
```

For example :

```txt
modInv[42, 2017]

{1969}

PowerMod[42, -1, 2017]

1969
```


=={{header|MK-61/52}}==
<lang>П1	П2	<->	П0	0	П5	1	П6	ИП1	1
-	x=0	14	С/П	ИП0	1	-	/-/	x<0	50
ИП0	ИП1	/	[x]	П4	ИП1	П3	ИП0	^	ИП1
/	[x]	ИП1	*	-	П1	ИП3	П0	ИП5	П3
ИП6	ИП4	ИП5	*	-	П5	ИП3	П6	БП	14
ИП6	x<0	55	ИП2	+	С/П
```


=={{header|Modula-2}}==
<lang Modula-2>MODULE ModularInverse;
  FROM InOut IMPORT WriteString, WriteInt, WriteLn;

  TYPE Data = RECORD x : INTEGER;
                     y : INTEGER
              END;

  VAR c  : INTEGER;
      ab : ARRAY [1..5] OF Data;

PROCEDURE mi(VAR a, b : INTEGER): INTEGER;
  VAR t, nt, r, nr, q, tmp : INTEGER;

BEGIN
  b := ABS(b);
  IF a < 0 THEN a := b - (-a MOD b) END;
  t := 0; nt := 1; r := b; nr := a MOD b;
  WHILE (nr # 0) DO
    q := r / nr;
    tmp := nt; nt := t - q * nt; t := tmp;
    tmp := nr; nr := r - q * nr; r := tmp;
  END;
  IF (r > 1) THEN RETURN -1 END;
  IF (t < 0) THEN RETURN t + b END;
  RETURN t;
END mi;

BEGIN
  ab[1].x := 42;   ab[1].y := 2017;
  ab[2].x := 40;   ab[2].y := 1;
  ab[3].x := 52;   ab[3].y := -217;
  ab[4].x := -486; ab[4].y := 217;
  ab[5].x := 40;   ab[5].y := 2018;
  WriteLn;
  WriteString("Modular inverse");
  WriteLn;
  FOR c := 1 TO 5 DO
    WriteInt(ab[c].x, 6); WriteString(", ");
    WriteInt(ab[c].y, 6); WriteString(" = ");
    WriteInt(mi(ab[c].x, ab[c].y),6);
    WriteLn;
  END;
END ModularInverse.
```

```txt
Modular inverse
    42,   2017 =   1969
    40,      1 =      0
    52,   -217 =     96
  -486,    217 =    121
    40,   2018 =     -1
```


## newLISP


```NewLisp

(define (modular-multiplicative-inverse a n)
    (if (< n 0)
        (setf n (abs n)))

    (if (< a 0)
        (setf a (- n (% (- 0 a) n))))

    (setf t 0)
    (setf nt 1)
    (setf r n)
    (setf nr (mod a n))

    (while (not (zero? nr))
        (setf q (int (div r nr)))
        (setf tmp nt)
        (setf nt (sub t (mul q nt)))
        (setf t tmp)
        (setf tmp nr)
        (setf nr (sub r (mul q nr)))
        (setf r tmp))

    (if (> r 1)
        (setf retvalue nil))

    (if (< t 0)
        (setf retvalue (add t n))
        (setf retvalue t))
    retvalue)

(println (modular-multiplicative-inverse 42 2017))

```


Output:

```txt

1969

```



## Nim

```nim

proc modInv(a0, b0: int): int =
  var (a, b, x0) = (a0, b0, 0)
  result = 1
  if b == 1: return
  while a > 1:
    result = result - (a div b) * x0
    a = a mod b
    swap a, b
    swap x0, result
  if result < 0: result += b0

echo modInv(42, 2017)

```

```txt

1969

```



## OCaml


==={{trans|C}}===

```ocaml
let mul_inv a = function 1 -> 1 | b ->
  let rec aux a b x0 x1 =
    if a <= 1 then x1 else
    if b = 0 then failwith "mul_inv" else
    aux b (a mod b) (x1 - (a / b) * x0) x0
  in
  let x = aux a b 0 1 in
  if x < 0 then x + b else x
```


Testing:

```txt

# mul_inv 42 2017 ;;
- : int = 1969

```


==={{trans|Haskell}}===


```ocaml
let rec gcd_ext a = function
  | 0 -> (1, 0, a)
  | b ->
      let s, t, g = gcd_ext b (a mod b) in
      (t, s - (a / b) * t, g)

let mod_inv a m =
  let mk_pos x = if x < 0 then x + m else x in
  match gcd_ext a m with
  | i, _, 1 -> mk_pos i
  | _ -> failwith "mod_inv"
```


Testing:

```txt

# mod_inv 42 2017 ;;
- : int = 1969

```



## Oforth


Usage : a modulus invmod


```Oforth
// euclid ( a b -- u v r )
//    Return r = gcd(a, b) and (u, v) / r = au + bv

: euclid(a, b)
| q u u1 v v1 |

   b 0 < ifTrue: [ b neg ->b ]
   a 0 < ifTrue: [ b a neg b mod - ->a ]

   1 dup ->u ->v1
   0 dup ->v ->u1

   while(b) [
      b a b /mod ->q ->b ->a
      u1 u u1 q * - ->u1 ->u
      v1 v v1 q * - ->v1 ->v
      ]
   u v a ;

: invmod(a, modulus)
   a modulus euclid 1 == ifFalse: [ drop drop null return ]
   drop dup 0 < ifTrue: [ modulus + ] ;
```


```txt

42 2017 invmod println
1969

```



## PARI/GP


```parigp
Mod(1/42,2017)
```



## Pascal


```Pascal

// increments e step times until bal is greater than t
// repeats until bal = 1 (mod = 1) and returns count
// bal will not be greater than t + e

function modInv(e, t : integer) : integer;
  var
    d : integer;
    bal, count, step : integer;
  begin
    d := 0;
    if e < t then
      begin
        count := 1;
        bal := e;
        repeat
          step := ((t-bal) DIV e)+1;
          bal := bal + step * e;
          count := count + step;
          bal := bal - t;
        until bal = 1;
        d := count;
      end;
    modInv := d;
  end;
```


Testing:

```txt

    Writeln(modInv(42,2017));

```

```txt
1969
```



## Perl

Various CPAN modules can do this, such as:

```perl
use bigint; say 42->bmodinv(2017);
# or
use Math::ModInt qw/mod/;  say mod(42, 2017)->inverse->residue;
# or
use Math::Pari qw/PARI lift/; say lift PARI "Mod(1/42,2017)";
# or
use Math::GMP qw/:constant/; say 42->bmodinv(2017);
# or
use ntheory qw/invmod/; say invmod(42, 2017);
```

or we can write our own:

```perl
sub invmod {
  my($a,$n) = @_;
  my($t,$nt,$r,$nr) = (0, 1, $n, $a % $n);
  while ($nr != 0) {
    # Use this instead of int($r/$nr) to get exact unsigned integer answers
    my $quot = int( ($r - ($r % $nr)) / $nr );
    ($nt,$t) = ($t-$quot*$nt,$nt);
    ($nr,$r) = ($r-$quot*$nr,$nr);
  }
  return if $r > 1;
  $t += $n if $t < 0;
  $t;
}

say invmod(42,2017);
```

'''Notes''': Special cases to watch out for include (1) where the inverse doesn't exist, such as invmod(14,28474), which should return undef or raise an exception, not return a wrong value.   (2) the high bit of a or n is set, e.g. invmod(11,2**63), (3) negative first arguments, e.g. invmod(-11,23).
The modules and code above handle these cases,
but some other language implementations for this task do not.


## Perl 6


```perl6
sub inverse($n, :$modulo) {
    my ($c, $d, $uc, $vc, $ud, $vd) = ($n % $modulo, $modulo, 1, 0, 0, 1);
    my $q;
    while $c != 0 {
        ($q, $c, $d) = ($d div $c, $d % $c, $c);
        ($uc, $vc, $ud, $vd) = ($ud - $q*$uc, $vd - $q*$vc, $uc, $vc);
    }
    return $ud % $modulo;
}

say inverse 42, :modulo(2017)
```



## Phix

```Phix
function mul_inv(integer a, n)
    if n<0 then n = -n end if
    if a<0 then a = n - mod(-a,n) end if
    integer t = 0,  nt = 1,
            r = n,  nr = a;
    while nr!=0 do
        integer q = floor(r/nr)
        {t, nt} = {nt, t-q*nt}
        {r, nr} = {nr, r-q*nr}
    end while
    if r>1 then return "a is not invertible" end if
    if t<0 then t += n end if
    return t
end function

?mul_inv(42,2017)
?mul_inv(40, 1)
?mul_inv(52, -217)  /* Pari semantics for negative modulus */
?mul_inv(-486, 217)
?mul_inv(40, 2018)
```

```txt

1969
0
96
121
"a is not invertible"

```



## PHP

'''Algorithm Implementation'''

```php
<?php
function invmod($a,$n){
        if ($n < 0) $n = -$n;
        if ($a < 0) $a = $n - (-$a % $n);
	$t = 0; $nt = 1; $r = $n; $nr = $a % $n;
	while ($nr != 0) {
		$quot= intval($r/$nr);
		$tmp = $nt;  $nt = $t - $quot*$nt;  $t = $tmp;
		$tmp = $nr;  $nr = $r - $quot*$nr;  $r = $tmp;
	}
	if ($r > 1) return -1;
	if ($t < 0) $t += $n;
	return $t;
}
	printf("%d\n", invmod(42, 2017));
?>
```

```txt
1969
```



## PicoLisp

```PicoLisp
(de modinv (A B)
   (let (B0 B  X0 0  X1 1  Q 0  T1 0)
      (while (< 1 A)
         (setq
            Q (/ A B)
            T1 B
            B (% A B)
            A T1
            T1 X0
            X0 (- X1 (* Q X0))
            X1 T1 ) )
      (if (lt0 X1) (+ X1 B0) X1) ) )

(println
   (modinv 42 2017) )

(bye)
```



## PL/I

```pli
*process source attributes xref or(!);
 /*--------------------------------------------------------------------
 * 13.07.2015 Walter Pachl
 *-------------------------------------------------------------------*/
 minv: Proc Options(main);
 Dcl (x,y) Bin Fixed(31);
 x=42;
 y=2017;
 Put Edit('modular inverse of',x,' by ',y,' ---> ',modinv(x,y))
         (Skip,3(a,f(4)));
 modinv: Proc(a,b) Returns(Bin Fixed(31));
 Dcl (a,b,ob,ox,d,t) Bin Fixed(31);
 ob=b;
 ox=0;
 d=1;

 If b=1 Then;
 Else Do;
   Do While(a>1);
     q=a/b;
     r=mod(a,b);
     a=b;
     b=r;
     t=ox;
     ox=d-q*ox;
     d=t;
     End;
   End;
 If d<0 Then
   d=d+ob;
 Return(d);
 End;
 End;
```

```txt
modular inverse of  42 by 2017 ---> 1969
```



## PowerShell


```powershell
function invmod($a,$n){
        if ([int]$n -lt 0) {$n = -$n}
        if ([int]$a -lt 0) {$a = $n - ((-$a) % $n)}

	$t = 0
	$nt = 1
	$r = $n
	$nr = $a % $n
	while ($nr -ne 0) {
		$q = [Math]::truncate($r/$nr)
		$tmp = $nt
		$nt = $t - $q*$nt
		$t = $tmp
		$tmp = $nr
		$nr = $r - $q*$nr
		$r = $tmp
	}
	if ($r -gt 1) {return -1}
	if ($t -lt 0) {$t += $n}
	return $t
}

invmod 42 2017
```

```txt
PS> .\INVMOD.PS1
1969
PS>
```



## PureBasic

Using brute force.

```PureBasic
EnableExplicit
Declare main()
Declare.i mi(a.i, b.i)

If OpenConsole("MODULAR-INVERSE")
  main() : Input() : End
EndIf

Macro ModularInverse(a, b)
  PrintN(~"\tMODULAR-INVERSE(" + RSet(Str(a),5) + "," +
         RSet(Str(b),5)+") = " +
         RSet(Str(mi(a, b)),5))
EndMacro

Procedure main()
  ModularInverse(42, 2017)  ; = 1969
  ModularInverse(40, 1)     ; = 0
  ModularInverse(52, -217)  ; = 96
  ModularInverse(-486, 217) ; = 121
  ModularInverse(40, 2018)  ; = -1
EndProcedure

Procedure.i mi(a.i, b.i)
  Define x.i = 1,
         y.i = Int(Abs(b)),
         r.i = 0
  If y = 1 : ProcedureReturn 0 : EndIf
  While x < y
    r = (a * x) % b
    If r = 1 Or (y + r) = 1
      Break
    EndIf
    x + 1
  Wend
  If x > y - 1 : x = -1 : EndIf
  ProcedureReturn x
EndProcedure
```

```txt
        MODULAR-INVERSE(   42, 2017) =  1969
        MODULAR-INVERSE(   40,    1) =     0
        MODULAR-INVERSE(   52, -217) =    96
        MODULAR-INVERSE( -486,  217) =   121
        MODULAR-INVERSE(   40, 2018) =    -1
```


## Python


===Iteration and error-handling===
Implementation of this [http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Iterative_method_2 pseudocode] with [https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm#Modular_inverse this].

```python>>>
 def extended_gcd(aa, bb):
    lastremainder, remainder = abs(aa), abs(bb)
    x, lastx, y, lasty = 0, 1, 1, 0
    while remainder:
        lastremainder, (quotient, remainder) = remainder, divmod(lastremainder, remainder)
        x, lastx = lastx - quotient*x, x
        y, lasty = lasty - quotient*y, y
    return lastremainder, lastx * (-1 if aa < 0 else 1), lasty * (-1 if bb < 0 else 1)

>>> def modinv(a, m):
	g, x, y = extended_gcd(a, m)
	if g != 1:
		raise ValueError
	return x % m

>>> modinv(42, 2017)
1969
>>>
```



### Recursion and an option type

Or, using functional composition as an alternative to iterative mutation, and wrapping the resulting value in an option type, to allow for the expression of computations which establish the '''absence''' of a modular inverse:


```python
from functools import (reduce)
from itertools import (chain)


# modInv :: Int -> Int -> Maybe Int
def modInv(a):
    return lambda m: (
        lambda ig=gcdExt(a)(m): (
            lambda i=ig[0]: (
                Just(i + m if 0 > i else i) if 1 == ig[2] else (
                    Nothing()
                )
            )
        )()
    )()


# gcdExt :: Int -> Int -> (Int, Int, Int)
def gcdExt(x):
    def go(a, b):
        if 0 == b:
            return (1, 0, a)
        else:
            (q, r) = divmod(a, b)
            (s, t, g) = go(b, r)
        return (t, s - q * t, g)
    return lambda y: go(x, y)


#  TEST ---------------------------------------------------

# Numbers between 2010 and 2015 which do yield modular inverses for 42:

# main :: IO ()
def main():
    print (
        mapMaybe(
            lambda y: bindMay(modInv(42)(y))(
                lambda mInv: Just((y, mInv))
            )
        )(
            enumFromTo(2010)(2025)
        )
    )

# -> [(2011, 814), (2015, 48), (2017, 1969), (2021, 1203)]


# GENERIC ABSTRACTIONS ------------------------------------


# enumFromTo :: Int -> Int -> [Int]
def enumFromTo(m):
    return lambda n: list(range(m, 1 + n))


# bindMay (>>=) :: Maybe  a -> (a -> Maybe b) -> Maybe b
def bindMay(m):
    return lambda mf: (
        m if m.get('Nothing') else mf(m.get('Just'))
    )


# Just :: a -> Maybe a
def Just(x):
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# mapMaybe :: (a -> Maybe b) -> [a] -> [b]
def mapMaybe(mf):
    return lambda xs: reduce(
        lambda a, x: maybe(a)(lambda j: a + [j])(mf(x)),
        xs,
        []
    )


# maybe :: b -> (a -> b) -> Maybe a -> b
def maybe(v):
    return lambda f: lambda m: v if m.get('Nothing') else (
        f(m.get('Just'))
    )


# Nothing :: Maybe a
def Nothing():
    return {'type': 'Maybe', 'Nothing': True}


# MAIN ---
main()
```

```txt

[(2011, 814), (2015, 48), (2017, 1969), (2021, 1203)]
```



## Racket


```racket

(require math)
(modular-inverse 42 2017)

```

```racket

1969

```



## REXX


```rexx
/*REXX program calculates and displays the  modular inverse  of an integer  X  modulo Y.*/
parse arg x y .                                  /*obtain two integers from the C.L.    */
if x=='' | x==","  then x=   42                  /*Not specified?  Then use the default.*/
if y=='' | y==","  then y= 2017                  /* "      "         "   "   "     "    */
say  'modular inverse of '      x       " by "       y        ' ───► '         modInv(x,y)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
modInv: parse arg a,b 1 ob;     z=0              /*B & OB are obtained from the 2nd arg.*/
        $=1
        if b\=1     then    do  while a>1
                            parse value  a/b  a//b  b  z       with      q  b  a  t
                            z=$ - q*z;              $=trunc(t)
                            end   /*while*/
        if $<0  then $=$+ob
        return $
```

'''output'''   when using the default inputs of:   <tt> 42   2017 </tt>

```txt

modular inverse of  42  by  2017  ───►  1969

```



## Ring


```ring

see "42 %! 2017 = " + multInv(42, 2017) + nl

func multInv a,b
     b0 = b
     x0 = 0
     multInv = 1
     if b = 1 return 0 ok
     while a > 1
           q = floor(a / b)
           t = b
           b = a % b
           a = t
           t = x0
           x0 = multInv - q * x0
           multInv = t
     end
     if multInv < 0 multInv = multInv + b0 ok
     return multInv

```

Output:

```txt

42 %! 2017 = 1969

```



## Ruby


```ruby
#based on pseudo code from http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Iterative_method_2 and from translating the python implementation.
def extended_gcd(a, b)
  last_remainder, remainder = a.abs, b.abs
  x, last_x, y, last_y = 0, 1, 1, 0
  while remainder != 0
    last_remainder, (quotient, remainder) = remainder, last_remainder.divmod(remainder)
    x, last_x = last_x - quotient*x, x
    y, last_y = last_y - quotient*y, y
  end

  return last_remainder, last_x * (a < 0 ? -1 : 1)
end

def invmod(e, et)
  g, x = extended_gcd(e, et)
  if g != 1
    raise 'The maths are broken!'
  end
  x % et
end
```


```txt

> invmod(42,2017)
=> 1969
```


Simplified equivalent implementation

```ruby

def modinv(a, m) # compute a^-1 mod m if possible
  raise "NO INVERSE - #{a} and #{m} not coprime" unless a.gcd(m) == 1
  return m if m == 1
  m0, inv, x0 = m, 1, 0
  while a > 1
    inv -= (a / m) * x0
    a, m = m, a % m
    inv, x0 = x0, inv
  end
  inv += m0 if inv < 0
  inv
end

```


```txt

> modinv(42,2017)
=> 1969

```



## Run BASIC


```runbasic
print multInv(42, 2017)
end

function multInv(a,b)
	b0	= b
	multInv	= 1
	if b = 1 then goto [endFun]
	while a > 1
		q	= a / b
		t	= b
		b	= a mod b
		a	= t
		t	= x0
		x0	= multInv - q * x0
		multInv	= int(t)
	wend
	if multInv < 0 then multInv = multInv + b0
[endFun]
end function
```


```txt

1969

```



## Rust


```rust
fn mod_inv(a: isize, module: isize) -> isize {
  let mut mn = (module, a);
  let mut xy = (0, 1);

  while mn.1 != 0 {
    xy = (xy.1, xy.0 - (mn.0 / mn.1) * xy.1);
    mn = (mn.1, mn.0 % mn.1);
  }

  while xy.0 < 0 {
    xy.0 += module;
  }
  xy.0
}

fn main() {
  println!("{}", mod_inv(42, 2017))
}
```


```txt

1969

```


Alternative implementation

```rust

fn modinv(a0: isize, m0: isize) -> isize {
    if m0 == 1 { return 1 }

    let (mut a, mut m, mut x0, mut inv) = (a0, m0, 0, 1);

    while a > 1 {
        inv -= (a / m) * x0;
        a = a % m;
        std::mem::swap(&mut a, &mut m);
        std::mem::swap(&mut x0, &mut inv);
    }

    if inv < 0 { inv += m0 }
    inv
}

fn main() {
  println!("{}", modinv(42, 2017))
}
```


```txt

1969

```



## Scala

Based on the ''Handbook of Applied Cryptography'', Chapter 2. See http://cacr.uwaterloo.ca/hac/ .

```scala

def gcdExt(u: Int, v: Int): (Int, Int, Int) = {
  @tailrec
  def aux(a: Int, b: Int, x: Int, y: Int, x1: Int, x2: Int, y1: Int, y2: Int): (Int, Int, Int) = {
    if(b == 0) (x, y, a) else {
      val (q, r) = (a / b, a % b)
      aux(b, r, x2 - q * x1, y2 - q * y1, x, x1, y, y1)
    }
  }
  aux(u, v, 1, 0, 0, 1, 1, 0)
}

def modInv(a: Int, m: Int): Option[Int] = {
  val (i, j, g) = gcdExt(a, m)
  if (g == 1) Option(if (i < 0) i + m else i) else Option.empty
}
```


Translated from C++ (on this page)

```scala

def modInv(a: Int, m: Int, x:Int = 1, y:Int = 0) : Int = if (m == 0) x else modInv(m, a%m, y, x - y*(a/m))

```


```txt
scala> modInv(2,4)
res1: Option[Int] = None

scala> modInv(42, 2017)
res2: Option[Int] = Some(1976)

```



## Seed7

The library [http://seed7.sourceforge.net/libraries/bigint.htm bigint.s7i]
defines the [http://seed7.sourceforge.net/manual/types.htm#bigInteger bigInteger]
function [http://seed7.sourceforge.net/libraries/bigint.htm#modInverse%28in_var_bigInteger,in_var_bigInteger%29 modInverse].
It returns the modular multiplicative inverse of a modulo b when a and b are coprime (gcd(a, b) = 1).
If a and b are not coprime (gcd(a, b) <> 1) the exception RANGE_ERROR is raised.


```seed7
const func bigInteger: modInverse (in var bigInteger: a,
    in var bigInteger: b) is func
  result
    var bigInteger: modularInverse is 0_;
  local
    var bigInteger: b_bak is 0_;
    var bigInteger: x is 0_;
    var bigInteger: y is 1_;
    var bigInteger: lastx is 1_;
    var bigInteger: lasty is 0_;
    var bigInteger: temp is 0_;
    var bigInteger: quotient is 0_;
  begin
    if b < 0_ then
      raise RANGE_ERROR;
    end if;
    if a < 0_ and b <> 0_ then
      a := a mod b;
    end if;
    b_bak := b;
    while b <> 0_ do
      temp := b;
      quotient := a div b;
      b := a rem b;
      a := temp;

      temp := x;
      x := lastx - quotient * x;
      lastx := temp;

      temp := y;
      y := lasty - quotient * y;
      lasty := temp;
    end while;
    if a = 1_ then
      modularInverse := lastx;
      if modularInverse < 0_ then
        modularInverse +:= b_bak;
      end if;
    else
      raise RANGE_ERROR;
    end if;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#modInverse]


## Sidef

Built-in:

```ruby
say 42.modinv(2017)
```


Algorithm implementation:

```ruby
func invmod(a, n) {
  var (t, nt, r, nr) = (0, 1, n, a % n)
  while (nr != 0) {
    var quot = int((r - (r % nr)) / nr);
    (nt, t) = (t - quot*nt, nt);
    (nr, r) = (r - quot*nr, nr);
  }
  r > 1 && return()
  t < 0 && (t += n)
  t
}

say invmod(42, 2017)
```

```txt

1969

```



## Tcl

```tcl
proc gcdExt {a b} {
    if {$b == 0} {
	return [list 1 0 $a]
    }
    set q [expr {$a / $b}]
    set r [expr {$a % $b}]
    lassign [gcdExt $b $r] s t g
    return [list $t [expr {$s - $q*$t}] $g]
}
proc modInv {a m} {
    lassign [gcdExt $a $m] i -> g
    if {$g != 1} {
	return -code error "no inverse exists of $a %! $m"
    }
    while {$i < 0} {incr i $m}
    return $i
}
```

Demonstrating

```tcl
puts "42 %! 2017 = [modInv 42 2017]"
catch {
    puts "2 %! 4 = [modInv 2 4]"
} msg; puts $msg
```

```txt

42 %! 2017 = 1969
no inverse exists of 2 %! 4

```



## tsql


```tsql
;WITH Iterate(N,A,B,X0,X1)
	AS (
		SELECT
			1
			,CASE WHEN @a < 0 THEN @b-(-@a % @b) ELSE @a END
			,CASE WHEN @b < 0 THEN -@b ELSE @b END
			,0
			,1
		UNION ALL
		SELECT
			N+1
			,B
			,A%B
			,X1-((A/B)*X0)
			,X0
		FROM Iterate
		WHERE A != 1 AND B != 0
	),
	ModularInverse(Result)
	AS (
		SELECT
			-1
			FROM Iterate
			WHERE A != 1 AND B = 0
		UNION ALL
		SELECT
			TOP(1)
			CASE WHEN X1 < 0 THEN X1+@b ELSE X1 END AS Result
			FROM Iterate
			WHERE (SELECT COUNT(*) FROM Iterate WHERE A != 1 AND B = 0) = 0
			ORDER BY N DESC
	)
	SELECT *
	FROM ModularInverse
```



## uBasic/4tH

<lang>Print FUNC(_MulInv(42, 2017))
End

_MulInv Param(2)
  Local(5)

  c@ = b@
  f@ = 0
  g@ = 1

  If b@ = 1 Then Return

  Do While a@ > 1
    e@ = a@ / b@
    d@ = b@
    b@ = a@ % b@
    a@ = d@

    d@ = f@
    f@ = g@ - e@ * f@
    g@ = d@
  Loop

  If g@ < 0 Then g@ = g@ + c@
Return (g@)
```

<lang>Print FUNC(_mul_inv(42, 2017))
Print FUNC(_mul_inv(40, 1))
Print FUNC(_mul_inv(52, -217))
Print FUNC(_mul_inv(-486, 217))
Print FUNC(_mul_inv(40, 2018))

End

_mul_inv Param(2)
  Local(6)

  If (b@ < 0) b@ = -b@
  If (a@ < 0) a@ = b@ - (-a@ % b@)
  c@ = 0 : d@ = 1 :  e@ = b@ :  f@ = a@ % b@

  Do Until (f@ = 0)
    g@ = e@/f@
    h@ = d@ :  d@ = c@ - g@*d@ :  c@ = h@
    h@ = f@ :  f@ = e@ - g@*f@ :  e@ = h@
  Loop

  If (e@ > 1) Return (-1)  ' No inverse'
  If (c@ < 0) c@ = c@ + b@
Return (c@)
```

```txt
1969
0
96
121
-1

0 OK, 0:156
```



## VBA

```vb

Private Function mul_inv(a As Long, n As Long) As Variant
    If n < 0 Then n = -n
    If a < 0 Then a = n - ((-a) Mod n)
    Dim t As Long: t = 0
    Dim nt As Long: nt = 1
    Dim r As Long: r = n
    Dim nr As Long: nr = a
    Dim q As Long
    Do While nr <> 0
        q = r \ nr
        tmp = t
        t = nt
        nt = tmp - q * nt
        tmp = r
        r = nr
        nr = tmp - q * nr
    Loop
    If r > 1 Then
        mul_inv = "a is not invertible"
    Else
        If t < 0 Then t = t + n
        mul_inv = t
    End If
End Function
Public Sub mi()
    Debug.Print mul_inv(42, 2017)
    Debug.Print mul_inv(40, 1)
    Debug.Print mul_inv(52, -217) '/* Pari semantics for negative modulus */
    Debug.Print mul_inv(-486, 217)
    Debug.Print mul_inv(40, 2018)
End Sub
```
```txt
 1969
 0
 96
 121
a is not invertible
```


## XPL0


```XPL0
code IntOut=11, Text=12;
int  X;
def  A=42, M=2017;
[for X:= 2 to M-1 do
    if rem(A*X/M) = 1 then [IntOut(0, X);  exit];
Text(0, "Does not exist");
]
```

```txt

1969

```



## zkl

```zkl
fcn gcdExt(a,b){
   if(b==0) return(1,0,a);
   q,r:=a.divr(b); s,t,g:=gcdExt(b,r); return(t,s-q*t,g);
}
fcn modInv(a,m){i,_,g:=gcdExt(a,m); if(g==1) {if(i<0)i+m} else Void}
```

divr(a,b) is [integer] (a/b,remainder)
```txt

modInv(2,4)  //-->Void
modInv(42,2017)  //-->1969

```

