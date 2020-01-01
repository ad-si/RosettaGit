+++
title = "Evaluate binomial coefficients"
description = ""
date = 2019-10-06T16:38:02Z
aliases = []
[extra]
id = 6924
[taxonomies]
categories = []
tags = []
+++

{{task|Mathematical operations}}
This programming task, is to calculate ANY binomial coefficient.

However, it has to be able to output   <big><big><math>\binom{5}{3}</math></big></big>,   which is   '''10'''.

This formula is recommended:
<big><big>
:: <math>\binom{n}{k} = \frac{n!}{(n-k)!k!} = \frac{n(n-1)(n-2)\ldots(n-k+1)}{k(k-1)(k-2)\ldots 1}</math>
</big></big>


'''See Also:'''
* [[Combinations and permutations]]
* [[Pascal's triangle]]
{{Template:Combinations and permutations}}




## 11l

{{trans|Python}}

```11l
F binomial_coeff(n, k)
   V result = 1
   L(i) 1..k
      result = result * (n - i + 1) / i
   R result

print(binomial_coeff(5, 3))
```


{{out}}

```txt

10

```



## 360 Assembly

{{trans|ABAP}}
Very compact version.

```360asm
*        Evaluate binomial coefficients - 29/09/2015
BINOMIAL CSECT
         USING  BINOMIAL,R15       set base register
         SR     R4,R4              clear for mult and div
         LA     R5,1               r=1
         LA     R7,1               i=1
         L      R8,N               m=n
LOOP     LR     R4,R7              do while i<=k
         C      R4,K               i<=k
         BH     LOOPEND            if not then exit while
         MR     R4,R8              r*m
         DR     R4,R7              r=r*m/i
         LA     R7,1(R7)           i=i+1
         BCTR   R8,0               m=m-1
         B      LOOP               loop while
LOOPEND  XDECO  R5,PG              edit r
         XPRNT  PG,12              print r
         XR     R15,R15            set return code
         BR     R14                return to caller
N        DC     F'10'              <== input value
K        DC     F'4'               <== input value
PG       DS     CL12               buffer
         YREGS
         END    BINOMIAL
```

{{out}}

```txt

         210

```



## ABAP


```ABAP
CLASS lcl_binom DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      calc
        IMPORTING n               TYPE i
                  k               TYPE i
        RETURNING VALUE(r_result) TYPE f.

ENDCLASS.

CLASS lcl_binom IMPLEMENTATION.

  METHOD calc.

    r_result = 1.
    DATA(i) = 1.
    DATA(m) = n.

    WHILE i <= k.
      r_result = r_result * m / i.
      i = i + 1.
      m = m - 1.
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
```

{{Out}}

```txt
lcl_binom=>calc( n = 5 k = 3 )
1,0000000000000000E+01
lcl_binom=>calc( n = 60 k = 30 )
1,1826458156486142E+17

```



## ACL2


```Lisp
(defun fac (n)
   (if (zp n)
       1
       (* n (fac (1- n)))))

(defun binom (n k)
   (/ (fac n) (* (fac (- n k)) (fac k)))
```



## Ada


```Ada

with Ada.Text_IO;  use Ada.Text_IO;
procedure Test_Binomial is
   function Binomial (N, K : Natural) return Natural is
      Result : Natural := 1;
      M      : Natural;
   begin
      if N < K then
         raise Constraint_Error;
      end if;
      if K > N/2 then -- Use symmetry
         M := N - K;
      else
         M := K;
      end if;
      for I in 1..M loop
         Result := Result * (N - M + I) / I;
      end loop;
      return Result;
   end Binomial;
begin
   for N in 0..17 loop
      for K in 0..N loop
         Put (Integer'Image (Binomial (N, K)));
      end loop;
      New_Line;
   end loop;
end Test_Binomial;

```

{{Out}}

```txt

 1
 1 1
 1 2 1
 1 3 3 1
 1 4 6 4 1
 1 5 10 10 5 1
 1 6 15 20 15 6 1
 1 7 21 35 35 21 7 1
 1 8 28 56 70 56 28 8 1
 1 9 36 84 126 126 84 36 9 1
 1 10 45 120 210 252 210 120 45 10 1
 1 11 55 165 330 462 462 330 165 55 11 1
 1 12 66 220 495 792 924 792 495 220 66 12 1
 1 13 78 286 715 1287 1716 1716 1287 715 286 78 13 1
 1 14 91 364 1001 2002 3003 3432 3003 2002 1001 364 91 14 1
 1 15 105 455 1365 3003 5005 6435 6435 5005 3003 1365 455 105 15 1
 1 16 120 560 1820 4368 8008 11440 12870 11440 8008 4368 1820 560 120 16 1
 1 17 136 680 2380 6188 12376 19448 24310 24310 19448 12376 6188 2380 680 136 17 1

```



## ALGOL 68

===Iterative - unoptimised ===
{{trans|C}} - note: This specimen retains the original [[Evaluate binomial coefficients#C|C]] coding style.

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}


```algol68
PROC factorial = (INT n)INT:
(
        INT result;

        result := 1;
        FOR i  TO n DO
                result *:= i
        OD;

        result
);

PROC choose = (INT n, INT k)INT:
(
        INT result;

# Note: code can be optimised here as k < n #
        result := factorial(n) OVER (factorial(k) * factorial(n - k));

        result
);

test:(
        print((choose(5, 3), new line))
)
```

{{Out}}

```txt

        +10

```



## ALGOL W


```algolw
begin
    % calculates n!/k!                                                       %
    integer procedure factorialOverFactorial( integer value n, k ) ;
        if      k > n then 0
        else if k = n then 1
        else %  k < n %    begin
            integer f;
            f := 1;
            for i := k + 1 until n do f := f * i;
            f
        end factorialOverFactorial ;

    % calculates n!                                                          %
    integer procedure factorial( integer value n ) ;
        begin
            integer f;
            f := 1;
            for i := 2 until n do f := f * i;
            f
        end factorial ;

    % calculates the binomial coefficient of (n k)                           %
    % uses the factorialOverFactorial procedure for a slight optimisation    %
    integer procedure binomialCoefficient( integer value n, k ) ;
        if ( n - k ) > k
        then factorialOverFactorial( n, n - k ) div factorial(   k   )
        else factorialOverFactorial( n,   k   ) div factorial( n - k );

    % display the binomial coefficient of (5 3)                              %
    write( binomialCoefficient( 5, 3 ) )

end.
```



## AppleScript


### Imperative


```AppleScript
set n to 5
set k to 3

on calculateFactorial(val)
	set partial_factorial to 1 as integer
	repeat with i from 1 to val
		set factorial to i * partial_factorial
		set partial_factorial to factorial
	end repeat
	return factorial
end calculateFactorial

set n_factorial to calculateFactorial(n)
set k_factorial to calculateFactorial(k)
set n_minus_k_factorial to calculateFactorial(n - k)

return n_factorial / (n_minus_k_factorial) * 1 / (k_factorial) as integer

```



### Functional


Using a little more abstraction for readability, and currying for ease of both re-use and refactoring:


```applescript
-- factorial :: Int -> Int
on factorial(n)
    product(enumFromTo(1, n))
end factorial


-- binomialCoefficient :: Int -> Int -> Int
on binomialCoefficient(n, k)
    factorial(n) div (factorial(n - k) * (factorial(k)))
end binomialCoefficient

-- Or, by reduction:

-- binomialCoefficient2 :: Int -> Int -> Int
on binomialCoefficient2(n, k)
    product(enumFromTo(1 + k, n)) div (factorial(n - k))
end binomialCoefficient2


-- TEST -----------------------------------------------------
on run

    {binomialCoefficient(5, 3), binomialCoefficient2(5, 3)}

    --> {10, 10}
end run


-- GENERAL -------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m ≤ n then
        set lst to {}
        repeat with i from m to n
            set end of lst to i
        end repeat
        return lst
    else
        return {}
    end if
end enumFromTo


-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if script is class of f then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- product :: [Num] -> Num
on product(xs)
    script multiply
        on |λ|(a, b)
            a * b
        end |λ|
    end script

    foldl(multiply, 1, xs)
end product
```

{{Out}}

```txt
{10, 10}
```



## AutoHotkey


```autohotkey
MsgBox, % Round(BinomialCoefficient(5, 3))

;---------------------------------------------------------------------------
BinomialCoefficient(n, k) {
;---------------------------------------------------------------------------
    r := 1
    Loop, % k < n - k ? k : n - k {
        r *= n - A_Index + 1
        r /= A_Index
    }
    Return, r
}
```

Message box shows:

```txt
10
```


## AWK


```AWK

# syntax: GAWK -f EVALUATE_BINOMIAL_COEFFICIENTS.AWK
BEGIN {
    main(5,3)
    main(100,2)
    main(33,17)
    exit(0)
}
function main(n,k,  i,r) {
    r = 1
    for (i=1; i<k+1; i++) {
      r *= (n - i + 1) / i
    }
    printf("%d %d = %d\n",n,k,r)
}

```

{{out}}

```txt

5 3 = 10
100 2 = 4950
33 17 = 1166803110

```



## Batch File


```dos
@echo off & setlocal

if "%~2"=="" ( echo Usage: %~nx0 n k && goto :EOF )

call :binom binom %~1 %~2
1>&2 set /P "=%~1 choose %~2 = "<NUL
echo %binom%

goto :EOF

:binom <var_to_set> <N> <K>
setlocal
set /a coeff=1, nk=%~2 - %~3 + 1
for /L %%I in (%nk%, 1, %~2) do set /a coeff *= %%I
for /L %%I in (1, 1, %~3) do set /a coeff /= %%I
endlocal && set "%~1=%coeff%"
goto :EOF
```


{{Out}}

```txt

> binom.bat 5 3
5 choose 3 = 10

> binom.bat 100 2
100 choose 2 = 4950

```


The string <code>n choose k = </code> is output to stderr, while the result is echoed to stdout.  This should allow capturing the result with a <code>for /f</code> loop without needing to define tokens or delims.

But...


```txt

> binom.bat 33 17
33 choose 17 = 0

> binom.bat 15 10
15 choose 10 = -547

```

The Windows cmd console only handles 32-bit integers.  If a factoral exceeds 2147483647 at any point, <code>set /a</code> will choke and roll over to a negative value, giving unexpected results.  Unfortunately, this is as good as it gets for pure batch.


## BBC BASIC


```bbcbasic
      @%=&1010

      PRINT "Binomial (5,3) = "; FNbinomial(5, 3)
      PRINT "Binomial (100,2) = "; FNbinomial(100, 2)
      PRINT "Binomial (33,17) = "; FNbinomial(33, 17)
      END

      DEF FNbinomial(N%, K%)
      LOCAL R%, D%
      R% = 1 : D% = N% - K%
      IF D% > K% THEN K% = D% : D% = N% - K%
      WHILE N% > K%
        R% *= N%
        N% -= 1
        WHILE D% > 1 AND (R% MOD D%) = 0
          R% /= D%
          D% -= 1
        ENDWHILE
      ENDWHILE
      = R%

```

{{Out}}

```txt
Binomial (5,3) = 10
Binomial (100,2) = 4950
Binomial (33,17) = 1166803110
```



## Bracmat


```bracmat
(binomial=
  n k coef
.   !arg:(?n,?k)
  & (!n+-1*!k:<!k:?k|)
  & 1:?coef
  &   whl
    ' ( !k:>0
      & !coef*!n*!k^-1:?coef
      & !k+-1:?k
      & !n+-1:?n
      )
  & !coef
);

binomial$(5,3)
10

```



## Burlesque



```burlesque

blsq ) 5 3nr
10

```



## C


```c
#include <stdio.h>
#include <limits.h>

/* We go to some effort to handle overflow situations */

static unsigned long gcd_ui(unsigned long x, unsigned long y) {
  unsigned long t;
  if (y < x) { t = x; x = y; y = t; }
  while (y > 0) {
    t = y;  y = x % y;  x = t;  /* y1 <- x0 % y0 ; x1 <- y0 */
  }
  return x;
}

unsigned long binomial(unsigned long n, unsigned long k) {
  unsigned long d, g, r = 1;
  if (k == 0) return 1;
  if (k == 1) return n;
  if (k >= n) return (k == n);
  if (k > n/2) k = n-k;
  for (d = 1; d <= k; d++) {
    if (r >= ULONG_MAX/n) {  /* Possible overflow */
      unsigned long nr, dr;  /* reduced numerator / denominator */
      g = gcd_ui(n, d);  nr = n/g;  dr = d/g;
      g = gcd_ui(r, dr);  r = r/g;  dr = dr/g;
      if (r >= ULONG_MAX/nr) return 0;  /* Unavoidable overflow */
      r *= nr;
      r /= dr;
      n--;
    } else {
      r *= n--;
      r /= d;
    }
  }
  return r;
}

int main() {
    printf("%lu\n", binomial(5, 3));
    printf("%lu\n", binomial(40, 19));
    printf("%lu\n", binomial(67, 31));
    return 0;
}
```

{{out}}

```txt
10
131282408400
11923179284862717872
```



## C++


```cpp
double Factorial(double nValue)
   {
       double result = nValue;
       double result_next;
       double pc = nValue;
       do
       {
           result_next = result*(pc-1);
           result = result_next;
           pc--;
       }while(pc>2);
       nValue = result;
       return nValue;
   }

double binomialCoefficient(double n, double k)
   {
       if (abs(n - k) < 1e-7 || k  < 1e-7) return 1.0;
       if( abs(k-1.0) < 1e-7 || abs(k - (n-1)) < 1e-7)return n;
       return Factorial(n) /(Factorial(k)*Factorial((n - k)));
   }

```


Implementation:

```cpp
int main()
{
    cout<<"The Binomial Coefficient of 5, and 3, is equal to: "<< binomialCoefficient(5,3);
    cin.get();
}
```


{{Out}}

```txt
The Binomial Coefficient of 5, and 3, is equal to: 10
```


## C#

```c#
using System;

namespace BinomialCoefficients
{
    class Program
    {
        static void Main(string[] args)
        {
            ulong n = 1000000, k = 3;
            ulong result = biCoefficient(n, k);
            Console.WriteLine("The Binomial Coefficient of {0}, and {1}, is equal to: {2}", n, k, result);
            Console.ReadLine();
        }

        static int fact(int n)
        {
            if (n == 0) return 1;
            else return n * fact(n - 1);
        }

        static ulong biCoefficient(ulong n, ulong k)
        {
            if (k > n - k)
            {
                k = n - k;
            }

            ulong c = 1;
            for (uint i = 0; i < k; i++)
            {
                c = c * (n - i);
                c = c / (i + 1);
            }
            return c;
        }
    }
}
```



## Clojure


```clojure
(defn binomial-coefficient [n k]
  (let [rprod (fn [a b] (reduce * (range a (inc b))))]
    (/ (rprod (- n k -1) n) (rprod 1 k))))
```



## CoffeeScript


```coffeescript

binomial_coefficient = (n, k) ->
  result = 1
  for i in [0...k]
    result *= (n - i) / (i + 1)
  result

n = 5
for k in [0..n]
  console.log "binomial_coefficient(#{n}, #{k}) = #{binomial_coefficient(n,k)}"

```


{{Out}}
```txt

> coffee binomial.coffee
binomial_coefficient(5, 0) = 1
binomial_coefficient(5, 1) = 5
binomial_coefficient(5, 2) = 10
binomial_coefficient(5, 3) = 10
binomial_coefficient(5, 4) = 5
binomial_coefficient(5, 5) = 1

```



## Common Lisp


```lisp

(defun choose (n k)
  (labels ((prod-enum (s e)
	     (do ((i s (1+ i)) (r 1 (* i r))) ((> i e) r)))
	   (fact (n) (prod-enum 1 n)))
    (/ (prod-enum (- (1+ n) k) n) (fact k))))

```



## D


```d
T binomial(T)(in T n, T k) pure nothrow {
    if (k > (n / 2))
        k = n - k;
    T bc = 1;
    foreach (T i; T(2) .. k + 1)
        bc = (bc * (n - k + i)) / i;
    return bc;
}

void main() {
    import std.stdio, std.bigint;

    foreach (const d; [[5, 3], [100, 2], [100, 98]])
        writefln("(%3d %3d) = %s", d[0], d[1], binomial(d[0], d[1]));
    writeln("(100  50) = ", binomial(100.BigInt, 50.BigInt));
}
```

{{out}}

```txt
(  5   3) = 2
(100   2) = 50
(100  98) = 50
(100  50) = 1976664223067613962806675336
```


The above wouldn't work for me (100C50 correctly gives 100891344545564193334812497256). This next one is a translation of C#:

```d
T BinomialCoeff(T)(in T n, in T k)
{
    T nn = n, kk = k, c = cast(T)1;

    if (kk > nn - kk) kk = nn - kk;

    for (T i = cast(T)0; i < kk; i++)
    {
        c = c * (nn - i);
        c = c / (i + cast(T)1);
    }

    return c;
}

void main()
{
    import std.stdio, std.bigint;

    BinomialCoeff(10UL, 3UL).writeln;
    BinomialCoeff(100.BigInt, 50.BigInt).writeln;
}
```

{{out}}

```txt
120
100891344545564193334812497256
```



## dc


```dc
[sx1q]sz[d0=zd1-lfx*]sf[skdlfxrlk-lfxlklfx*/]sb
```


Demonstration:


```dc>5 3lbxp</lang

<tt>10</tt>

Annotated version:


```dc
[ macro z: factorial base case when n is (z)ero ]sx
[sx     [ x is our dump register; get rid of extraneous copy of n we no longer need]sx
 1      [ return value is 1 ]sx
 q]     [ abort processing of calling macro ]sx
sz

[ macro f: factorial ]sx [
  d       [ duplicate the input (n) ]sx
  0 =z    [ if n is zero, call z, which stops here and returns 1 ]sx
  d       [ otherwise, duplicate n again ]sx
  1 -     [ subtract 1 ]sx
  lfx     [ take the factorial ]sx
  *       [ we have (n-1)!; multiply it by the copy of n to get n! ]sx
] sf

[ macro b(n,k): binomial function (n choose k).
  straightforward RPN version of formula.]sx [
  sk      [ remember k. stack:              n       ]sx
  d       [ duplicate:             n        n       ]sx
  lfx     [ call factorial:        n        n!      ]sx
  r       [ swap:                  n!       n       ]sx
  lk      [ load k:           n!   n        k       ]sx
  -       [ subtract:              n!      n-k      ]sx
  lfx     [ call factorial:        n!     (n-k)!    ]sx
  lk      [ load k:           n! (n-k)!     k       ]sx
  lfx     [ call factorial;   n! (n-k)!     k!      ]sx
  *       [ multiply:              n!    (n-k)!k!   ]sx
  /       [ divide:                     n!/(n-k)!k! ]sx
] sb

5 3 lb x p  [print(5 choose 3)]sx
```



## Delphi



```Delphi
program Binomial;

{$APPTYPE CONSOLE}

function BinomialCoff(N, K: Cardinal): Cardinal;
var
  L: Cardinal;

begin
  if N < K then
    Result:= 0      // Error
  else begin
    if K > N - K then
      K:= N - K;    // Optimization
    Result:= 1;
    L:= 0;
    while L < K do begin
      Result:= Result * (N - L);
      Inc(L);
      Result:= Result div L;
    end;
  end;
end;

begin
  Writeln('C(5,3) is ', BinomialCoff(5, 3));
  ReadLn;
end.
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule RC do
  def choose(n,k) when is_integer(n) and is_integer(k) and n>=0 and k>=0 and n>=k do
    if k==0, do: 1, else: choose(n,k,1,1)
  end

  def choose(n,k,k,acc), do: div(acc * (n-k+1), k)
  def choose(n,k,i,acc), do: choose(n, k, i+1, div(acc * (n-i+1), i))
end

IO.inspect RC.choose(5,3)
IO.inspect RC.choose(60,30)
```


{{out}}

```txt

10
118264581564861424

```



## Erlang


```erlang

choose(N, 0) -> 1;
choose(N, K) when is_integer(N), is_integer(K), (N >= 0), (K >= 0), (N >= K) ->
  choose(N, K, 1, 1).

choose(N, K, K, Acc) ->
  (Acc * (N-K+1)) div K;
choose(N, K, I, Acc) ->
  choose(N, K, I+1, (Acc * (N-I+1)) div I).

```



## ERRE

<lang>PROGRAM BINOMIAL

!$DOUBLE

PROCEDURE BINOMIAL(N,K->BIN)
      LOCAL R,D
      R=1 D=N-K
      IF D>K THEN K=D D=N-K  END IF
      WHILE N>K DO
        R*=N
        N-=1
        WHILE D>1 AND (R-D*INT(R/D))=0 DO
          R/=D
          D-=1
        END WHILE
      END WHILE
      BIN=R
END PROCEDURE

BEGIN
   BINOMIAL(5,3->BIN)
   PRINT("Binomial (5,3) = ";BIN)
   BINOMIAL(100,2->BIN)
   PRINT("Binomial (100,2) = ";BIN)
   BINOMIAL(33,17->BIN)
   PRINT("Binomial (33,17) = ";BIN)
END PROGRAM

```

{{out}}

```txt
Binomial (5,3) =  10
Binomial (100,2) =  4950
Binomial (33,17) =  1166803110

```



## Factor


```factor

: fact ( n -- n-factorial )
    dup 0 = [ drop 1 ] [ dup 1 - fact * ] if ;

: choose ( n k -- n-choose-k )
    2dup - [ fact ] tri@ * / ;

! outputs 10
5 3 choose .

! alternative using folds
USE: math.ranges

! (product [n..k+1] / product [n-k..1])
: choose-fold ( n k -- n-choose-k )
    2dup 1 + [a,b] product -rot - 1 [a,b] product / ;

```


=={{header|F Sharp|F#}}==

```fsharp

let choose n k = List.fold (fun s i -> s * (n-i+1)/i ) 1 [1..k]

```



## Forth


```forth
: choose ( n k -- nCk ) 1 swap 0 ?do over i - i 1+ */ loop nip ;

 5  3 choose .   \ 10
33 17 choose .   \ 1166803110
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program test_choose

  implicit none

  write (*, '(i0)') choose (5, 3)

contains

  function factorial (n) result (res)

    implicit none
    integer, intent (in) :: n
    integer :: res
    integer :: i

    res = product ((/(i, i = 1, n)/))

  end function factorial

  function choose (n, k) result (res)

    implicit none
    integer, intent (in) :: n
    integer, intent (in) :: k
    integer :: res

    res = factorial (n) / (factorial (k) * factorial (n - k))

  end function choose

end program test_choose
```

{{Out}}
```txt
10
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function factorial(n As Integer) As Integer
  If n < 1 Then Return 1
  Dim product As Integer = 1
  For i As Integer = 2 To n
    product *= i
  Next
  Return Product
End Function

Function binomial(n As Integer, k As Integer) As Integer
  If n < 0 OrElse k < 0 OrElse n <= k Then Return 1
  Dim product As Integer = 1
  For i As Integer = n - k + 1 To n
    Product *= i
  Next
  Return product \ factorial(k)
End Function

For n As Integer =  0 To 14
  For k As Integer = 0 To n
    Print Using "####"; binomial(n, k);
    Print" ";
  Next k
  Print
Next n

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

   1
   1    1
   1    2    1
   1    3    3    1
   1    4    6    4    1
   1    5   10   10    5    1
   1    6   15   20   15    6    1
   1    7   21   35   35   21    7    1
   1    8   28   56   70   56   28    8    1
   1    9   36   84  126  126   84   36    9    1
   1   10   45  120  210  252  210  120   45   10    1
   1   11   55  165  330  462  462  330  165   55   11    1
   1   12   66  220  495  792  924  792  495  220   66   12    1
   1   13   78  286  715 1287 1716 1716 1287  715  286   78   13    1
   1   14   91  364 1001 2002 3003 3432 3003 2002 1001  364   91   14    1

```



## Frink

Frink has a built-in efficient function to find binomial coefficients.
It produces arbitrarily-large integers.

```frink

println[binomial[5,3]]

```



## FunL

FunL has pre-defined function <code>choose</code> in module <code>integers</code>, which is defined as:

```funl
def
  choose( n, k ) | k < 0 or k > n = 0
  choose( n, 0 ) = 1
  choose( n, n ) = 1
  choose( n, k ) = product( [(n - i)/(i + 1) | i <- 0:min( k, n - k )] )

println( choose(5, 3) )
println( choose(60, 30) )
```

{{out}}

```txt

10
118264581564861424

```


Here it is defined using the recommended formula for this task.

```funl
import integers.factorial

def
  binomial( n, k ) | k < 0 or k > n = 0
  binomial( n, k ) = factorial( n )/factorial( n - k )/factorial( k )
```



## GAP


```gap
# Built-in
Binomial(5, 3);
# 10
```



## Go


```go
package main
import "fmt"
import "math/big"

func main() {
  fmt.Println(new(big.Int).Binomial(5, 3))
  fmt.Println(new(big.Int).Binomial(60, 30))
}
```

{{out}}

```txt

10
118264581564861424

```



## Golfscript

Actually evaluating n!/(k! (n-k)!):

```golfscript
;5 3 # Set up demo input
{),(;{*}*}:f; # Define a factorial function
.f@.f@/\@-f/
```

But Golfscript is meant for golfing, and it's shorter to calculate <math>\prod_{i=0}^{k-1} \frac{n-i}{i+1}</math>:


```golfscript
;5 3 # Set up demo input
1\,@{1$-@\*\)/}+/
```



## Groovy

Solution:

```groovy
def factorial = { x ->
    assert x > -1
    x == 0 ? 1 : (1..x).inject(1G) { BigInteger product, BigInteger factor -> product *= factor }
}

def combinations = { n, k ->
    assert k >= 0
    assert n >= k
    factorial(n).intdiv(factorial(k)*factorial(n-k))
}
```


Test:

```groovy
assert combinations(20, 0) == combinations(20, 20)
assert combinations(20, 10) == (combinations(19, 9) + combinations(19, 10))
assert combinations(5, 3) == 10
println combinations(5, 3)
```


{{Out}}

```txt
10
```



## Haskell

The only trick here is realizing that everything's going to divide nicely, so we can use div instead of (/).


```haskell

choose :: (Integral a) => a -> a -> a
choose n k = product [k+1..n] `div` product [1..n-k]

```



```haskell>
 5 `choose` 3
10
```


Or, generate the binomial coefficients iteratively to avoid computing with big numbers:


```haskell

choose :: (Integral a) => a -> a -> a
choose n k = foldl (\z i -> (z * (n-i+1)) `div` i) 1 [1..k]

```


Or using "caching":


```haskell
coeffs = iterate next [1]
  where
    next ns = zipWith (+) (0:ns) $ ns ++ [0]

main = print $ coeffs !! 5 !! 3
```



## HicEst


```HicEst
WRITE(Messagebox) BinomCoeff( 5, 3) ! displays 10

FUNCTION factorial( n )
   factorial = 1
   DO i = 1, n
      factorial = factorial * i
   ENDDO
END

FUNCTION BinomCoeff( n, k )
   BinomCoeff = factorial(n)/factorial(n-k)/factorial(k)
END
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link math, factors

procedure main()
write("choose(5,3)=",binocoef(5,3))
end
```

{{Out}}

```txt
choose(5,3)=10
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/math.icn math provides binocoef] and
[http://www.cs.arizona.edu/icon/library/src/procs/factors.icn factors provides factorial].


```Icon
procedure binocoef(n, k)	#: binomial coefficient

   k := integer(k) | fail
   n := integer(n) | fail

   if (k = 0) | (n = k) then return 1

   if 0 <= k <= n then
      return factorial(n) / (factorial(k) * factorial(n - k))
   else fail

end

procedure factorial(n)			#: return n! (n factorial)
   local i

   n := integer(n) | runerr(101, n)

   if n < 0 then fail

   i := 1

   every i *:= 1 to n

   return i

end
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Binomial.bas"
110 PRINT "Binomial (5,3) =";BINOMIAL(5,3)
120 DEF BINOMIAL(N,K)
130   LET R=1:LET D=N-K
140   IF D>K THEN LET K=D:LET D=N-K
150   DO WHILE N>K
160     LET R=R*N:LET N=N-1
170     DO WHILE D>1 AND MOD(R,D)=0
180       LET R=R/D:LET D=D-1
190     LOOP
200   LOOP
210   LET BINOMIAL=R
220 END DEF
```



## J

'''Solution:'''

The dyadic form of the primitive <code>!</code> ([[http://www.jsoftware.com/help/dictionary/d410.htm Out of]]) evaluates binomial coefficients directly.

'''Example usage:'''

```j
   3 ! 5
10
```



## Java


```java
public class Binomial {

    // precise, but may overflow and then produce completely incorrect results
    private static long binomialInt(int n, int k) {
        if (k > n - k)
            k = n - k;

        long binom = 1;
        for (int i = 1; i <= k; i++)
            binom = binom * (n + 1 - i) / i;
        return binom;
    }

    // same as above, but with overflow check
    private static Object binomialIntReliable(int n, int k) {
        if (k > n - k)
            k = n - k;

        long binom = 1;
        for (int i = 1; i <= k; i++) {
            try {
                binom = Math.multiplyExact(binom, n + 1 - i) / i;
            } catch (ArithmeticException e) {
                return "overflow";
            }
        }
        return binom;
    }

    // using floating point arithmetic, larger numbers can be calculated,
    // but with reduced precision
    private static double binomialFloat(int n, int k) {
        if (k > n - k)
            k = n - k;

        double binom = 1.0;
        for (int i = 1; i <= k; i++)
            binom = binom * (n + 1 - i) / i;
        return binom;
    }

    // slow, hard to read, but precise
    private static BigInteger binomialBigInt(int n, int k) {
        if (k > n - k)
            k = n - k;

        BigInteger binom = BigInteger.ONE;
        for (int i = 1; i <= k; i++) {
            binom = binom.multiply(BigInteger.valueOf(n + 1 - i));
            binom = binom.divide(BigInteger.valueOf(i));
        }
        return binom;
    }

    private static void demo(int n, int k) {
        List<Object> data = Arrays.asList(
                n,
                k,
                binomialInt(n, k),
                binomialIntReliable(n, k),
                binomialFloat(n, k),
                binomialBigInt(n, k));

        System.out.println(data.stream().map(Object::toString).collect(Collectors.joining("\t")));
    }

    public static void main(String[] args) {
        demo(5, 3);
        demo(1000, 300);
    }
}
```

{{Out}}

```txt
5	3	10	10	10.0	10
1000	300	-8357011479171942	overflow	5.428250046406143E263	542825004640614064815358503892902599588060075560435179852301016412253602009800031872232761420804306539976220810204913677796961128392686442868524741815732892024613137013599170443939815681313827516308854820419235457578544489551749630302863689773725905288736148678480
```


Recursive version, without overflow check:


```java
public class Binomial
{
    private static long binom(int n, int k)
    {
        if (k==0)
            return 1;
        else if (k>n-k)
            return binom(n, n-k);
        else
            return binom(n-1, k-1)*n/k;
    }

    public static void main(String[] args)
    {
        System.out.println(binom(5, 3));
    }
}
```

{{Out}}

```txt
10
```



## JavaScript


```javascript
function binom(n, k) {
    var coeff = 1;
    var i;

    if (k < 0 || k > n) return 0;

    for (i = 0; i < k; i++) {
        coeff = coeff * (n - i) / (i + 1);
    }

    return coeff;
}

console.log(binom(5, 3));
```

{{Out}}

```txt
10
```



## jq


```jq># nCk assuming n
= k
def binomial(n; k):
  if k > n / 2 then binomial(n; n-k)
  else reduce range(1; k+1) as $i (1; . * (n - $i + 1) / $i)
  end;

def task:
  .[0] as $n | .[1] as $k
  | "\($n) C \($k) = \(binomial( $n; $k) )";
;

([5,3], [100,2], [ 33,17]) | task

```

{{out}}
 5 C 3 = 10
 100 C 2 = 4950
 33 C 17 = 1166803110


## Julia

{{works with|Julia|1.2}}

'''Built-in'''

```julia
@show binomial(5, 3)
```


'''Recursive version''':

```julia
function binom(n::Integer, k::Integer)
    n ≥ k || return 0 # short circuit base cases
    (n == 1 || k == 0) && return 1

    n * binom(n - 1, k - 1) ÷ k
end

@show binom(5, 3)
```


{{out}}

```txt
binomial(5, 3) = 10
binom(5, 3) = 10
```



## K


```K
   {[n;k]_(*/(k-1)_1+!n)%(*/1+!k)} . 5 3
10
```


Alternative version:

```K
   {[n;k]i:!(k-1);_*/((n-i)%(i+1))} . 5 3
10
```


Using Pascal's triangle:

```K
   pascal:{x{+':0,x,0}\1}
   pascal 5
(1
 1 1
 1 2 1
 1 3 3 1
 1 4 6 4 1
 1 5 10 10 5 1)

   {[n;k](pascal n)[n;k]} . 5 3
10
```



## Kotlin


```scala
// version 1.0.5-2

fun factorial(n: Int) = when {
    n < 0 -> throw IllegalArgumentException("negative numbers not allowed")
    else  -> {
        var ans = 1L
        for (i in 2..n) ans *= i
        ans
    }
}

fun binomial(n: Int, k: Int) = when {
    n < 0 || k < 0 -> throw IllegalArgumentException("negative numbers not allowed")
    n == k         -> 1L
    else           -> {
        var ans = 1L
        for (i in n - k + 1..n) ans *= i
        ans / factorial(k)
    }
}

fun main(args: Array<String>) {
    for (n in 0..14) {
        for (k in 0..n)
            print("%4d ".format(binomial(n, k)))
        println()
    }
}
```


{{out}}

```txt

   1
   1    1
   1    2    1
   1    3    3    1
   1    4    6    4    1
   1    5   10   10    5    1
   1    6   15   20   15    6    1
   1    7   21   35   35   21    7    1
   1    8   28   56   70   56   28    8    1
   1    9   36   84  126  126   84   36    9    1
   1   10   45  120  210  252  210  120   45   10    1
   1   11   55  165  330  462  462  330  165   55   11    1
   1   12   66  220  495  792  924  792  495  220   66   12    1
   1   13   78  286  715 1287 1716 1716 1287  715  286   78   13    1
   1   14   91  364 1001 2002 3003 3432 3003 2002 1001  364   91   14    1

```



## Lasso


```Lasso
define binomial(n::integer,k::integer) => {
	#k == 0 ? return 1
	local(result = 1)
	loop(#k) => {
		#result = #result * (#n - loop_count + 1) / loop_count
	}
	return #result
}
// Tests
binomial(5, 3)
binomial(5, 4)
binomial(60, 30)
```


{{Out}}

```txt
10
5
118264581564861424
```



## Logo


```logo
to choose :n :k
  if :k = 0 [output 1]
  output (choose :n :k-1) * (:n - :k + 1) / :k
end

show choose 5 3   ; 10
show choose 60 30 ; 1.18264581564861e+17
```


## Lua


```lua
function Binomial( n, k )
    if k > n then return nil end
    if k > n/2 then k = n - k end       --   (n k) = (n n-k)

    numer, denom = 1, 1
    for i = 1, k do
        numer = numer * ( n - i + 1 )
        denom = denom * i
    end
    return numer / denom
end
```


Additive recursion with memoization by hashing 2 input integer.
Lua 5.3 support bit-wise operation; assume 64 bit integer implementation here.

```lua
local Binomial = setmetatable({},{
 __call = function(self,n,k)
   local hash = (n<<32) | (k & 0xffffffff)
   local ans = self[hash]
   if not ans then
    if n<0 or k>n then
      return 0 -- not save
    elseif n<=1 or k==0 or k==n then
      ans = 1
    else
      if 2*k > n then
        ans = self(n, n - k)
      else
        local lhs = self(n-1,k)
        local rhs = self(n-1,k-1)
        local sum = lhs + rhs
        if sum<0 or not math.tointeger(sum)then
          -- switch to double
          ans = lhs/1.0 + rhs/1.0 -- approximate
        else
          ans = sum
        end
      end
    end
    rawset(self,hash,ans)
   end
   return ans
 end
})
print( Binomial(100,50)) -- 1.0089134454556e+029

```



## Liberty BASIC


```lb

    '   [RC] Binomial Coefficients

    print "Binomial Coefficient of "; 5; " and "; 3; " is ",BinomialCoefficient( 5, 3)
    n =1 +int( 10 *rnd( 1))
    k =1 +int( n *rnd( 1))
    print "Binomial Coefficient of "; n; " and "; k; " is ",BinomialCoefficient( n, k)

    end

    function BinomialCoefficient( n, k)
        BinomialCoefficient =factorial( n) /factorial( n -k) /factorial( k)
    end function

    function factorial( n)
        if n <2 then
            f =1
        else
            f =n *factorial( n -1)
        end if
    factorial =f
    end function


```




## Maple


```Maple
convert(binomial(n,k),factorial);

binomial(5,3);
```

{{Out}}

```txt
                         factorial(n)
                 -----------------------------
                 factorial(k) factorial(n - k)

                               10
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
(Local) In[1]:= Binomial[5,3]
(Local) Out[1]= 10
```


=={{header|MATLAB}} / {{header|Octave}}==
This is a built-in function in MATLAB called "nchoosek(n,k)". But, this will only work for scalar inputs. If "n" is a vector then "nchoosek(v,k)" finds all combinations of choosing "k" elements out of the "v" vector (see [[Combinations#MATLAB]]).

Solution:

```MATLAB>>
 nchoosek(5,3)
ans =
    10
```


Alternative implementations are:


```MATLAB
function r = binomcoeff1(n,k)
    r = diag(rot90(pascal(n+1))); % vector of all binomial coefficients for order n
    r = r(k);
end;
```



```MATLAB
function r = binomcoeff2(n,k)
   prod((n-k+1:n)./(1:k))
end;
```



```MATLAB
function r = binomcoeff3(n,k)
   m = pascal(max(n-k,k)+1);
   r = m(n-k+1,k+1);
end;
```


If you want a vectorized function that returns multiple binomial coefficients given vector inputs, you must define that function yourself. A sample implementation is given below. This function takes either scalar or vector inputs for "n" and "v" and returns either a: scalar, vector, or matrix. Where the columns are indexed by the "k" vector and the rows indexed by the "n" vector.
binomialCoeff.m:

```MATLAB
function coefficients = binomialCoeff(n,k)

    coefficients = zeros(numel(n),numel(k)); %Preallocate memory

    columns = (1:numel(k)); %Preallocate row and column counters
    rows = (1:numel(n));

    %Iterate over every row and column. The rows represent the n number,
    %and the columns represent the k number. If n is ever greater than k,
    %the nchoosek function will throw an error. So, we test to make sure
    %it isn't, if it is then we leave that entry in the coefficients matrix
    %zero. Which makes sense combinatorically.
    for row = rows
        for col = columns
            if k(col) <= n(row)
                coefficients(row,col) = nchoosek(n(row),k(col));
            end
        end
    end

end %binomialCoeff
```

Sample Usage:

```MATLAB>>
 binomialCoeff((0:5),(0:5))

ans =

     1     0     0     0     0     0
     1     1     0     0     0     0
     1     2     1     0     0     0
     1     3     3     1     0     0
     1     4     6     4     1     0
     1     5    10    10     5     1

>> binomialCoeff([1 0 3 2],(0:3))

ans =

     1     1     0     0
     1     0     0     0
     1     3     3     1
     1     2     1     0

>> binomialCoeff(3,(0:3))

ans =

     1     3     3     1

>> binomialCoeff((0:3),2)

ans =

     0
     0
     1
     3

>> binomialCoeff(5,3)

ans =

    10
```



## Maxima


```maxima
binomial( 5,  3);      /* 10 */
binomial(-5,  3);      /* -35 */
binomial( 5, -3);      /* 0 */
binomial(-5, -3);      /* 0 */
binomial( 3,  5);      /* 0 */

binomial(x, 3);        /* ((x - 2)*(x - 1)*x)/6 */

binomial(3, 1/2);      /* binomial(3, 1/2) */
makegamma(%);          /* 32/(5*%pi) */

binomial(a, b);        /* binomial(a, b) */
makegamma(%);          /* gamma(a + 1)/(gamma(-b + a + 1)*gamma(b + 1)) */
```



## min

{{works with|min|0.19.3}}

```min
((dup 0 ==) 'succ (dup pred) '* linrec) :fact
('dup dip dup ((fact) () (- fact) (fact * div)) spread) :binomial

5 3 binomial puts!
```

{{out}}

```txt

10

```


=={{header|МК-61/52}}==
<lang>П1	<->	П0	ПП	22	П2	 ИП1	ПП	22	П3
ИП0	ИП1	-	ПП	22	ИП3	*	П3	ИП2	ИП3
/	С/П	ВП	П0	1	ИП0	*	L0	25	В/О
```


''Input'': ''n'' ^ ''k'' В/О С/П.


## MINIL


```minil
// Number of combinations nCr
00 0E  Go:    ENT  R0   // n
01 1E         ENT  R1   // r
02 2C         CLR  R2
03 2A  Loop:  ADD1 R2
04 0D         DEC  R0
05 1D         DEC  R1
06 C3         JNZ  Loop
07 3C         CLR  R3   // for result
08 3A         ADD1 R3
09 0A  Next:  ADD1 R0
0A 1A         ADD1 R1
0B 50         R5 = R0
0C 5D         DEC  R5
0D 63         R6 = R3
0E 46  Mult:  R4 = R6
0F 3A  Add:   ADD1 R3
10 4D         DEC  R4
11 CF         JNZ  Add
12 5D         DEC  R5
13 CE         JNZ  Mult
14 61  Divide:R6 = R1
15 5A         ADD1 R5
16 3D  Sub:   DEC  R3
17 9B         JZ   Exact
18 6D         DEC  R6
19 D6         JNZ  Sub
1A 94         JZ   Divide
1B 35  Exact: R3 = R5
1C 2D         DEC  R2
1D C9         JNZ  Next
1E 03         R0 = R3
1F 80         JZ   Go   // Display result
```


This uses the recursive definition:

ncr(n, r) = 1 if r = 0

ncr(n, r) = n/r * ncr(n-1, r-1) otherwise

which results from the definition of ncr in terms of factorials.


## Nim


```nim
proc binomialCoeff(n, k: int): int =
  result = 1
  for i in 1..k:
    result = result * (n-i+1) div i

echo binomialCoeff(5, 3)
```

{{Out}}

```txt
10
```



## Oberon

{{works with|oo2c}}

```oberon2

MODULE Binomial;
IMPORT
  Out;

PROCEDURE For*(n,k: LONGINT): LONGINT;
VAR
  i,m,r: LONGINT;

BEGIN
  ASSERT(n > k);
  r := 1;
  IF k > n DIV 2 THEN m := n - k ELSE m := k END;
  FOR i := 1 TO m DO
    r := r * (n - m + i) DIV i
  END;
  RETURN r
END For;

BEGIN
  Out.Int(For(5,2),0);Out.Ln
END Binomial.

```

{{Out}}

```txt
10
```



## OCaml


```OCaml

let binomialCoeff n p =
  let p = if p < n -. p then p else n -. p in
  let rec cm res num denum =
    (* this method partially prevents overflow.
     * float type is choosen to have increased domain on 32-bits computer,
     * however algorithm ensures an integral result as long as it is possible
     *)
    if denum <= p then cm ((res *. num) /. denum) (num -. 1.) (denum +. 1.)
    else res in
  cm 1. n 1.

```


###  Alternate version using big integers


```ocaml
#load "nums.cma";;
open Num;;

let binomial n p =
   let m = min p (n - p) in
   if m < 0 then Int 0 else
   let rec a j v =
      if j = m then v
      else a (succ j) ((v */ (Int (n - j))) // (Int (succ j)))
   in a 0 (Int 1)
;;
```



###  Simple recursive version


```OCaml
open Num;;
let rec binomial n k = if n = k then Int 1 else ((binomial (n-1) k) */ Int n) // Int (n-k)
```




## Oforth



```Oforth
: binomial(n, k)  | i |  1 k loop: i [ n i - 1+ * i / ] ;
```


{{out}}

```txt

>5 3 binomial .
10

```



## Oz

{{trans|Python}}

```oz
declare
  fun {BinomialCoeff N K}
     {List.foldL {List.number 1 K 1}
      fun {$ Z I}
         Z * (N-I+1) div I
      end
      1}
  end
in
  {Show {BinomialCoeff 5 3}}
```



## PARI/GP


```parigp
binomial(5,3)
```



## Pascal

See [[Evaluate_binomial_coefficients#Delphi | Delphi]]


## Perl


```perl
sub binomial {
    use bigint;
    my ($r, $n, $k) = (1, @_);
    for (1 .. $k) { $r *= $n--; $r /= $_ }
    $r;
}

print binomial(5, 3);
```


{{out}}

```txt
10
```


Since the bigint module already has a binomial method, this could also be written as:

```perl
sub binomial {
    use bigint;
    my($n,$k) = @_;
    (0+$n)->bnok($k);
}
```


For better performance, especially with large inputs, one can also use something like:
{{libheader|ntheory}}

```perl
use ntheory qw/binomial/;
print length(binomial(100000,50000)), "\n";
```

{{out}}

```txt
30101
```


The Math::Pari module also has binomial, but it needs large amounts of added stack space for large arguments (this is due to using a very old version of the underlying Pari library).


## Perl 6

For a start, you can get the length of the corresponding list of combinations:

```perl6
say combinations(5, 3).elems;
```

{{out}}

```txt
10
```


This method is efficient, as Perl 6 will not actually compute each element of the list, since it actually uses an iterator with a defined <tt>count-only</tt> method.  Such method performs computations in a way similar to the following infix operator:


```perl6>sub infix:<choose
 { [*] ($^n ... 0) Z/ 1 .. $^p }
say 5 choose 3;
```


A possible optimization would use a symmetry property of the binomial coefficient:


```perl6>sub infix:<choose
 { [*] ($^n ... 0) Z/ 1 .. min($n - $^p, $p) }
```


One drawback of this method is that it returns a Rat, not an Int.  So we actually may want to enforce the conversion:

```perl6>sub infix:<choose
 { ([*] ($^n ... 0) Z/ 1 .. min($n - $^p, $p)).Int }
```


And ''this'' is exactly what the <tt>count-only</tt> method does.


## Phix

A naive version:

```Phix
function binom(integer n, k)
    return factorial(n)/(factorial(k)*factorial(n-k))
end function

?binom(5,3)
```

{{out}}

```txt

10

```

However errors will creep in should any result or interim value exceed 9,007,199,254,740,992 (on 32-bit), so (and using a different algorithm just for kicks):
{{libheader|mpfr}}

```Phix
include builtins\mpfr.e

function mpz_binom(integer n, k)
mpz r = mpz_init(1)
    for i=1 to k do
        mpz_mul_si(r,r,n-i+1)
        if mpz_fdiv_q_ui(r, r, i)!=0 then ?9/0 end if
--      r = ba_divide(ba_multiply(r,n-i+1),i)
    end for
    return mpz_get_str(r)
end function

?mpz_binom(5,3)
?mpz_binom(100,50)
?mpz_binom(60,30)
?mpz_binom(1200,120)
```

{{out}}

```txt

"10"
"100891344545564193334812497256"
"118264581564861424"
"1004576581793084916475353119318331966507299414258370667602185866686463289093457468590558508056798211449853806741873396451735444387513582540860551330127062642417424083600"

```



## PHP


```PHP
<?php
$n=5;
$k=3;
function factorial($val){
    for($f=2;$val-1>1;$f*=$val--);
    return $f;
}
$binomial_coefficient=factorial($n)/(factorial($k)*factorial($n-$k));
echo $binomial_coefficient;
?>
```


Alternative version, not based on factorial

```PHP

function binomial_coefficient($n, $k) {
  if ($k == 0) return 1;
  $result = 1;
  foreach (range(0, $k - 1) as $i) {
    $result *= ($n - $i) / ($i + 1);
  }
  return $result;
}

```



## PicoLisp


```PicoLisp
(de binomial (N K)
   (let f
      '((N)
         (if (=0 N) 1 (apply * (range 1 N))) )
      (/
         (f N)
         (* (f (- N K)) (f K)) ) ) )
```

{{Out}}

```txt
: (binomial 5 3)
-> 10
```



## PL/I


```PL/I

binomial_coefficients:
   procedure options (main);
      declare (n, k) fixed;

   get (n, k);
   put (coefficient(n, k));

coefficient: procedure (n, k) returns (fixed decimal (15));
   declare (n, k) fixed;
   return (fact(n)/ (fact(n-k) * fact(k)) );
end coefficient;

fact: procedure (n) returns (fixed decimal (15));
   declare n fixed;
   declare i fixed, f fixed decimal (15);
   f = 1;
   do i = 1 to n;
      f = f * i;
   end;
   return (f);
end fact;
end binomial_coefficients;

```

{{Out}}

```txt

                10

```



## PowerShell


```PowerShell

function choose($n,$k) {
    if($k -le $n -and 0 -le $k) {
        $numerator = $denominator = 1
        0..($k-1) | foreach{
            $numerator *= ($n-$_)
            $denominator *= ($_ + 1)
        }
        $numerator/$denominator
    } else {
        "$k is greater than $n or lower than 0"
    }
}
choose 5 3
choose 2 1
choose 10 10
choose 10 2
choose 10 8

```

<b>Output:</b>

```txt

10
2
1
45
45

```



## PureBasic


```PureBasic
Procedure Factor(n)
  Protected Result=1
  While n>0
    Result*n
    n-1
  Wend
  ProcedureReturn Result
EndProcedure

Macro C(n,k)
  (Factor(n)/(Factor(k)*factor(n-k)))
EndMacro

If OpenConsole()
  Print("Enter value n: "): n=Val(Input())
  Print("Enter value k: "): k=Val(Input())
  PrintN("C(n,k)= "+str(C(n,k)))

  Print("Press ENTER to quit"): Input()
  CloseConsole()
EndIf
```

'''Example
 Enter value n: 5
 Enter value k: 3
 C(n,k)= 10


## Python


### Imperative


```python
def binomialCoeff(n, k):
    result = 1
    for i in range(1, k+1):
        result = result * (n-i+1) / i
    return result

if __name__ == "__main__":
    print(binomialCoeff(5, 3))
```

{{Out}}

```txt
10
```



### Functional


```python
from operator import mul
from functools import reduce


def comb(n,r):
    ''' calculate nCr - the binomial coefficient
    >>> comb(3,2)
    3
    >>> comb(9,4)
    126
    >>> comb(9,6)
    84
    >>> comb(20,14)
    38760
    '''

    if r > n-r:
        # r = n-r   for smaller intermediate values during computation
        return ( reduce( mul, range((n - (n-r) + 1), n + 1), 1)
                 // reduce( mul, range(1, (n-r) + 1), 1) )
    else:
        return ( reduce( mul, range((n - r + 1), n + 1), 1)
                 // reduce( mul, range(1, r + 1), 1) )
```



Or, abstracting a little more for legibility and ease of reuse, while currying for ease of mapping and general composition:

{{Works with|Python|3.7}}

```python
'''Evaluation of binomial coefficients'''

from functools import reduce


# binomialCoefficient :: Int -> Int -> Int
def binomialCoefficient(n):
    '''n choose k, expressed in terms of
       product and factorial functions.
    '''
    return lambda k: product(
        enumFromTo(1 + k)(n)
    ) // factorial(n - k)


# TEST ----------------------------------------------------
# main :: IO()
def main():
    '''Tests'''

    print(
        binomialCoefficient(5)(3)
    )

    # k=0 to k=5, where n=5
    print(
        list(map(
            binomialCoefficient(5),
            enumFromTo(0)(5)
        ))
    )


# GENERIC -------------------------------------------------

# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# factorial :: Int -> Int
def factorial(x):
    '''The factorial of x, where
       x is a positive integer.
    '''
    return product(enumFromTo(1)(x))


# product :: [Num] -> Num
def product(xs):
    '''The product of a list of
       numeric values.
    '''
    return reduce(lambda a, b: a * b, xs, 1)


# TESTS ---------------------------------------------------
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
10
[1, 5, 10, 10, 5, 1]
```


Compare the use of Python comments, (above); with the use of Python type hints, (below).


```python
from typing import (Callable, List, Any)
from functools import reduce
from operator import mul


def binomialCoefficient(n: int) -> Callable[[int], int]:
    return lambda k: product(enumFromTo(1 + k)(n)) // factorial(n - k)


def enumFromTo(m: int) -> Callable[[int], List[Any]]:
    return lambda n: list(range(m, 1 + n))


def factorial(x: int) -> int:
    return product(enumFromTo(1)(x))


def product(xs: List[Any]) -> int:
    return reduce(mul, xs, 1)


if __name__ == '__main__':
    print(binomialCoefficient(5)(3))
    # k=0 to k=5, where n=5
    print(list(map(binomialCoefficient(5), enumFromTo(0)(5))))
```

{{Out}}

```txt
10
[1, 5, 10, 10, 5, 1]
```



## R

R's built-in choose() function evaluates binomial coefficients:

```r
choose(5,3)
```


{{Out}}

```txt
[1] 10
```



## Racket


```racket

#lang racket
(require math)
(binomial 10 5)

```



## REXX

The task is to compute ANY binomial coefficient(s), but these REXX examples are limited to 100k digits.

### idiomatic


```rexx
/*REXX program calculates   binomial coefficients  (also known as  combinations).       */
numeric digits 100000                            /*be able to handle gihugeic numbers.  */
parse arg n k .                                  /*obtain  N  and  K   from the C.L.    */
say 'combinations('n","k')='  comb(n,k)          /*display the number of combinations.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
comb: procedure;  parse arg x,y;         return !(x) % (!(x-y) * !(y))
!:    procedure;  !=1;           do j=2  to arg(1);  !=!*j;  end  /*j*/;          return !
```

'''output''' when using the input of:   <tt> 5   3 </tt>

```txt

combinations(5,3)= 10

```

'''output'''   when using the input of:   <tt> 1200   120 </tt>

```txt

combinations(1200,120)= 1004576581793084916475353119318331966507299414258370667602185866686463289093457468590558508056798211449853806741873396451735444387513582540860551330127062642417424083600

```



### optimized

This REXX version takes advantage of reducing the size (product) of the numerator, and also,

only two (factorial) products need be calculated.

```rexx
/*REXX program calculates   binomial coefficients  (also known as  combinations).       */
numeric digits 100000                            /*be able to handle gihugeic numbers.  */
parse arg n k .                                  /*obtain  N  and  K   from the C.L.    */
say 'combinations('n","k')='  comb(n,k)          /*display the number of combinations.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
comb:  procedure;  parse arg x,y;        return pfact(x-y+1, x)  %  pfact(2, y)
/*──────────────────────────────────────────────────────────────────────────────────────*/
pfact: procedure;  !=1;        do j=arg(1)  to arg(2);  !=!*j;  end  /*j*/;       return !
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.

It is (around average) about ten times faster than the 1<sup>st</sup> version for   <code> 200,20 </code>   and   <code> 100,10</code>.

For   <code>100,80 </code>   it is about 30% faster.



## Ring


```ring

numer = 0
binomial(5,3)
see "(5,3) binomial = " + numer + nl

func binomial n, k
     if k > n return nil ok
     if k > n/2 k = n - k ok
     numer = 1
     for i = 1 to k
         numer = numer * ( n - i + 1 ) / i
     next
     return numer

```



## Ruby

{{trans|Tcl}}

{{works with|Ruby|1.8.7+}}

```ruby
class Integer
  # binomial coefficient: n C k
  def choose(k)
    # n!/(n-k)!
    pTop = (self-k+1 .. self).inject(1, &:*)
    # k!
    pBottom = (2 .. k).inject(1, &:*)
    pTop / pBottom
  end
end

p 5.choose(3)
p 60.choose(30)
```

result

```txt
10
118264581564861424
```


another implementation:


```ruby

def c n, r
  (0...r).inject(1) do |m,i| (m * (n - i)) / (i + 1) end
end

```
Ruby's Arrays have a combination method which result in a (lazy) enumerator. This Enumerator has a "size" method, which returns the size of the enumerator, or nil if it can’t be calculated lazily. (Since Ruby 2.0)

```ruby
(1..60).to_a.combination(30).size  #=> 118264581564861424
```



## Run BASIC


```runbasic
print "binomial (5,1) = "; binomial(5, 1)
print "binomial (5,2) = "; binomial(5, 2)
print "binomial (5,3) = "; binomial(5, 3)
print "binomial (5,4) = "; binomial(5,4)
print "binomial (5,5) = "; binomial(5,5)
end

function binomial(n,k)
 coeff = 1
 for i = n - k + 1 to n
   coeff = coeff * i
 next i
 for i = 1 to k
   coeff = coeff / i
 next i
binomial = coeff
end function
```

{{Out}}

```txt
binomial (5,1) = 5
binomial (5,2) = 10
binomial (5,3) = 10
binomial (5,4) = 5
binomial (5,5) = 1
```



## Rust


```rust
fn fact(n:u32) -> u64 {
  let mut f:u64 = n as u64;
  for i in 2..n {
    f *= i as u64;
  }
  return f;
}

fn choose(n: u32, k: u32)  -> u64 {
   let mut num:u64 = n as u64;
   for i in 1..k {
     num *= (n-i) as u64;
   }
   return num / fact(k);
}

fn main() {
  println!("{}", choose(5,3));
}
```

{{Out}}

```txt
10
```


Alternative version, using functional style:


```rust
fn choose(n:u64,k:u64)->u64 {
   let factorial=|x| (1..=x).fold(1, |a, x| a * x);
   factorial(n) / factorial(k) / factorial(n - k)
}
```



## Scala


```scala
object Binomial {
   def main(args: Array[String]): Unit = {
      val n=5
      val k=3
      val result=binomialCoefficient(n,k)
      println("The Binomial Coefficient of %d and %d equals %d.".format(n, k, result))
   }

   def binomialCoefficient(n:Int, k:Int)=fact(n) / (fact(k) * fact(n-k))
   def fact(n:Int):Int=if (n==0) 1 else n*fact(n-1)
}
```

{{Out}}

```txt
The Binomial Coefficient of 5 and 3 equals 10.
```


Another (more flexible and efficient) implementation. n and k are taken from command line. The use of BigInts allows to compute coefficients of arbitrary size:


```scala
object Binomial extends App {
  def binomialCoefficient(n: Int, k: Int) =
    (BigInt(n - k + 1) to n).product /
    (BigInt(1) to k).product

  val Array(n, k) = args.map(_.toInt)
  println("The Binomial Coefficient of %d and %d equals %,3d.".format(n, k, binomialCoefficient(n, k)))
}
```


{{Out}}

```txt
java Binomial 100 30
The Binomial Coefficient of 100 and 30 equals 29,372,339,821,610,944,823,963,760.
```


Using recursive formula <code>C(n,k) = C(n-1,k-1) + C(n-1,k)</code>:

```scala
  def bico(n: Long, k: Long): Long = (n, k) match {
    case (n, 0) => 1
    case (0, k) => 0
    case (n, k) => bico(n - 1, k - 1) + bico(n - 1, k)
  }
  println("bico(5,3) = " + bico(5, 3))
```

{{Out}}

```txt
bico(5,3) = 10
```



## Scheme

{{Works with|Scheme|R<math>^5</math>RS}}

```scheme
(define (factorial n)
  (define (*factorial n acc)
    (if (zero? n)
        acc
        (*factorial (- n 1) (* acc n))))
  (*factorial n 1))

(define (choose n k)
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

(display (choose 5 3))
(newline)
```

{{Out}}

```txt
10
```


Alternatively a recursive implementation can be constructed from Pascal's Triangle:


```scheme
(define (pascal i j)
  (cond ((= i 0) 1)
        ((= j 0) 1)
        (else (+
               (pascal (- i 1) j)
               (pascal i (- j 1))))))

(define (choose n k)
  (pascal (- n k) k)))

(display (choose 5 3))
(newline)
```

{{Out}}

```txt
10
```



## Seed7

The infix operator [http://seed7.sourceforge.net/libraries/integer.htm#%28in_integer%29!%28in_integer%29 !] computes the binomial coefficient.
E.g.: <tt>5 ! 3</tt> evaluates to 10. The binomial coefficient operator works also for negative values of n.
E.g.: <tt>(-6) ! 10</tt> evaluates to 3003.


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: n is 0;
    var integer: k is 0;
  begin
    for n range 0 to 66 do
      for k range 0 to n do
         write(n ! k <& " ");
      end for;
      writeln;
    end for;
  end func;
```


{{Out}}

```txt

1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
1 6 15 20 15 6 1
1 7 21 35 35 21 7 1
1 8 28 56 70 56 28 8 1
1 9 36 84 126 126 84 36 9 1
1 10 45 120 210 252 210 120 45 10 1
1 11 55 165 330 462 462 330 165 55 11 1
1 12 66 220 495 792 924 792 495 220 66 12 1
1 13 78 286 715 1287 1716 1716 1287 715 286 78 13 1
1 14 91 364 1001 2002 3003 3432 3003 2002 1001 364 91 14 1
1 15 105 455 1365 3003 5005 6435 6435 5005 3003 1365 455 105 15 1
1 16 120 560 1820 4368 8008 11440 12870 11440 8008 4368 1820 560 120 16 1
1 17 136 680 2380 6188 12376 19448 24310 24310 19448 12376 6188 2380 680 136 17 1
1 18 153 816 3060 8568 18564 31824 43758 48620 43758 31824 18564 8568 3060 816 153 18 1
1 19 171 969 3876 11628 27132 50388 75582 92378 92378 75582 50388 27132 11628 3876 969 171 19 1
1 20 190 1140 4845 15504 38760 77520 125970 167960 184756 167960 125970 77520 38760 15504 4845 1140 190 20 1
...

```


The library [http://seed7.sourceforge.net/libraries/bigint.htm bigint.s7i] contains a definition of the binomial coefficient operator
[http://seed7.sourceforge.net/libraries/bigint.htm#%28in_bigInteger%29!%28in_var_bigInteger%29 !]
for the type [http://seed7.sourceforge.net/manual/types.htm#bigInteger bigInteger]:


```seed7
const func bigInteger: (in bigInteger: n) ! (in var bigInteger: k) is func
  result
    var bigInteger: binom is 0_;
  local
    var bigInteger: numerator is 0_;
    var bigInteger: denominator is 0_;
  begin
    if n >= 0_ and k > n >> 1 then
      k := n - k;
    end if;
    if k < 0_ then
      binom := 0_;
    elsif k = 0_ then
      binom := 1_;
    else
      binom := n;
      numerator := pred(n);
      denominator := 2_;
      while denominator <= k do
        binom *:= numerator;
        binom := binom div denominator;
        decr(numerator);
        incr(denominator);
      end while;
    end if;
  end func;

```


Original source [http://seed7.sourceforge.net/algorith/math.htm#binomial_coefficient].


## SequenceL


Simplest Solution:

```sequenceL

choose(n, k) := product(k + 1 ... n) / product(1 ... n - k);

```


Tail-Recursive solution to avoid arithmetic with large integers:

```sequenceL


choose(n,k) := binomial(n, k, 1, 1);

binomial(n, k, i, result) :=
	result when i > k else
	binomial(n, k, i + 1, (result * (n - i + 1)) / i);

```



## Sidef

Straightforward translation of the formula:

```ruby
func binomial(n,k) {
    n! / ((n-k)! * k!)
}

say binomial(400, 200)
```


Alternatively, by using the ''Number.nok()'' method:

```ruby
say 400.nok(200)
```



## Stata

Use the [http://www.stata.com/help.cgi?comb comb] function. Notice the result is a missing value if k>n or k<0.


```stata
. display comb(5,3)
10
```



## Tcl

This uses exact arbitrary precision integer arithmetic.

```tcl
package require Tcl 8.5
proc binom {n k} {
    # Compute the top half of the division; this is n!/(n-k)!
    set pTop 1
    for {set i $n} {$i > $n - $k} {incr i -1} {
	set pTop [expr {$pTop * $i}]
    }

    # Compute the bottom half of the division; this is k!
    set pBottom 1
    for {set i $k} {$i > 1} {incr i -1} {
	set pBottom [expr {$pBottom * $i}]
    }

    # Integer arithmetic divide is correct here; the factors always cancel out
    return [expr {$pTop / $pBottom}]
}
```

Demonstrating:

```tcl
puts "5_C_3 = [binom 5 3]"
puts "60_C_30 = [binom 60 30]"
```

{{Out}}

```txt
5_C_3 = 10
60_C_30 = 118264581564861424
```


=={{header|TI-83 BASIC}}==
Builtin operator nCr gives the number of combinations.

```ti83b>10 nCr 4</lang

{{out}}

```txt

210

```


=={{header|TI-89 BASIC}}==

Builtin function.


```ti89b
nCr(n,k)
```



## TXR


nCk is a built-in function, along with the one for permutations, nPk:


```sh
$ txr -p '(n-choose-k 20 15)'
15504
```



```sh
$ txr -p '(n-perm-k 20 15)'
20274183401472000
```



## UNIX Shell



```sh
#!/bin/sh
n=5;
k=3;
calculate_factorial(){
partial_factorial=1;
for (( i=1; i<="$1"; i++ ))
do
    factorial=$(expr $i \* $partial_factorial)
    partial_factorial=$factorial

done
echo $factorial
}

n_factorial=$(calculate_factorial $n)
k_factorial=$(calculate_factorial $k)
n_minus_k_factorial=$(calculate_factorial `expr $n - $k`)
binomial_coefficient=$(expr $n_factorial \/ $k_factorial \* 1 \/ $n_minus_k_factorial )

echo "Binomial Coefficient ($n,$k) = $binomial_coefficient"
```





## Ursala

A function for computing binomial coefficients (<code>choose</code>) is included in a standard library, but if it weren't, it could be defined in one of the following ways, starting from the most idiomatic.

```Ursala
#import nat

choose = ~&ar^?\1! quotient^\~&ar product^/~&al ^|R/~& predecessor~~
```

The standard library functions <code>quotient</code>, <code>product</code> and <code>predecessor</code> pertain to natural numbers in the obvious way.
* <code>choose</code> is defined using the recursive conditional combinator (<code>^?</code>) as a function taking a pair of numbers, with the predicate <code>~&ar</code> testing whether the number on the right side of the pair is non-zero.
* If the predicate does not hold (implying the right side is zero), then a constant value of 1 is returned.
* If the predicate holds, the function given by the rest of the expression executes as follows.
* First the <code>predecessor</code> of both sides (<code>~~</code>) of the argument is taken.
* Then a recursive call (<code>^|R</code>) is made to the whole function (<code>~&</code>) with the pair of predecessors passed to it as an argument.
* The result returned by the recursive call is multiplied (<code>product</code>) by the left side of the original argument (<code>~&al</code>).
* The product of these values is then divided (<code>quotient</code>) by the right side (<code>~&ar</code>) of the original argument and returned as the result.
Here is a less efficient implementation more closely following the formula above.

```Ursala
choose = quotient^/factorial@l product+ factorial^~/difference ~&r
```

* <code>choose</code> is defined as the <code>quotient</code> of the results of a pair (<code>^</code>) of functions.
* The left function contributing to the quotient is the  <code>factorial</code> of the left side (<code>@l</code>) of the argument, which is assumed to be a pair of natural numbers. The <code>factorial</code> function is provided in a standard library.
* The right function contributing to the quotient is the function given by the rest of the expression, which applies to the whole pair as follows.
* It begins by forming a pair of numbers from the argument, the left being their <code>difference</code> obtained by subtraction, and the right being the a copy of the right (<code>~&r</code>) side of the argument.
* The <code>factorial</code> function is applied separately to both results (<code>^~</code>).
* A composition (<code>+</code>) of this function with the <code>product</code> function effects the multiplication of the two factorials, to complete the other input to the quotient.
Here is an equivalent implementation using pattern matching, dummy variables, and only the apply-to-both (<code>~~</code>) operator.

```Ursala
choose("n","k") = quotient(factorial "n",product factorial~~ (difference("n","k"),"k"))
```

test program:

```Ursala
#cast %nL

main = choose* <(5,3),(60,30)>
```

{{Out}}

```txt
<10,118264581564861424>
```



## VBScript


```vb
Function binomial(n,k)
	binomial = factorial(n)/(factorial(n-k)*factorial(k))
End Function

Function factorial(n)
	If n = 0 Then
		factorial = 1
	Else
		For i = n To 1 Step -1
			If i = n Then
				factorial = n
			Else
				factorial = factorial * i
			End If
		Next
	End If
End Function

'calling the function
WScript.StdOut.Write "the binomial coefficient of 5 and 3 = " & binomial(5,3)
WScript.StdOut.WriteLine
```


{{Out}}

```txt
the binomial coefficient of 5 and 3 = 10
```




## XPL0


```XPL0
code ChOut=8, CrLf=9, IntOut=11;

func Binomial(N, K);
int  N, K;
int  M, B, I;
[M:= K;
if K>N/2 the M:= N-K;
B:=1;
for I:= 1 to M do
    B:= B*(N-M+I)/I;
return B;
];

int N, K;
[for N:= 0 to 9 do
    [for K:= 0 to 9 do
        [if N>=K then IntOut(0, Binomial(N,K));
        ChOut(0, 9\tab\);
        ];
    CrLf(0);
    ];
] \Mr. Pascal's triangle!
```


{{Out}}

```txt

1
1       1
1       2       1
1       3       3       1
1       4       6       4       1
1       5       10      10      5       1
1       6       15      20      15      6       1
1       7       21      35      35      21      7       1
1       8       28      56      70      56      28      8       1
1       9       36      84      126     126     84      36      9       1

```



## zkl

Using 64 bit ints:

```zkl
fcn binomial(n,k){ (1).reduce(k,fcn(p,i,n){ p*(n-i+1)/i },1,n) }
```

{{out}}

```txt

zkl: binomial(5,3)
10
zkl: binomial(60,30)
118264581564861424

```



## ZX Spectrum Basic

{{trans|BBC_BASIC}}

```zxbasic
10 LET n=33: LET k=17: PRINT "Binomial ";n;",";k;" = ";
20 LET r=1: LET d=n-k
30 IF d>k THEN LET k=d: LET d=n-k
40 IF n<=k THEN GO TO 90
50 LET r=r*n
60 LET n=n-1
70 IF (d>1) AND (FN m(r,d)=0) THEN LET r=r/d: LET d=d-1: GO TO 70
80 GO TO 40
90 PRINT r
100 DEF FN m(a,b)=a-INT (a/b)*b
```

