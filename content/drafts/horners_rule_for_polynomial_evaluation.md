+++
title = "Horner's rule for polynomial evaluation"
description = ""
date = 2019-10-18T10:47:13Z
aliases = []
[extra]
id = 6677
[taxonomies]
categories = []
tags = []
+++

{{task}}
A fast scheme for evaluating a polynomial such as:
: <math>-19+7x-4x^2+6x^3\,</math>
when
: <math>x=3\;</math>.
is to arrange the computation as follows:
: <math>((((0) x + 6) x + (-4)) x + 7) x + (-19)\;</math>
And compute the result from the innermost brackets outwards as in this pseudocode:
 coefficients ''':=''' [-19, 7, -4, 6] ''# list coefficients of all x^0..x^n in order''
 x ''':=''' 3
 accumulator ''':=''' 0
 '''for''' i '''in''' ''length''(coefficients) '''downto''' 1 '''do'''
     ''# Assumes 1-based indexing for arrays''
     accumulator ''':=''' ( accumulator * x ) + coefficients[i]
 '''done'''
 ''# accumulator now has the answer''

'''Task Description'''
:Create a routine that takes a list of coefficients of a polynomial in order of increasing powers of x; together with a value of x to compute its value at, and return the value of the polynomial at that value using [http://www.physics.utah.edu/~detar/lessons/c++/array/node1.html Horner's rule].

Cf. [[Formal power series]]


## 360 Assembly


```360asm
*        Horner's rule for polynomial evaluation - 07/10/2015
HORNER   CSECT
         USING  HORNER,R15         set base register
         SR     R5,R5              accumulator=0
         LA     R2,N               i=number_of_coeff
LOOP     M      R4,X               accumulator=accumulator*x
         LR     R1,R2              i
         SLA    R1,2               i*4
         L      R3,COEF-4(R1)      coef(i)
         AR     R5,R3              accumulator=accumulator+coef(i)
         BCT    R2,LOOP            i=i-1; loop n times
         XDECO  R5,PG              edit accumulator
         XPRNT  PG,12              print buffer
         XR     R15,R15            set return code
         BR     R14                return to caller
COEF     DC     F'-19',F'7',F'-4',F'6'    <== input values
X        DC     F'3'                      <== input value
N        EQU    (X-COEF)/4         number of coefficients
PG       DS     CL12               buffer
         YREGS
         END    HORNER
```

{{out}}

```txt

         128

```



## ACL2


```Lisp
(defun horner (ps x)
   (if (endp ps)
       0
       (+ (first ps)
          (* x (horner (rest ps) x)))))
```



## Ada


```Ada
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Horners_Rule is
   type Coef is array(Positive range <>) of Float;

   function Horner(Coeffs: Coef; Val: Float) return Float is
      Res : Float := 0.0;
   begin
      for P in reverse Coeffs'Range loop
         Res := Res*Val + Coeffs(P);
      end loop;
      return Res;
   end Horner;

begin
   Put(Horner(Coeffs => (-19.0, 7.0, -4.0, 6.0), Val => 3.0), Aft=>1, Exp=>0);
end Horners_Rule;
```

Output:

```txt
128.0
```



## Aime


```aime
real
horner(list coeffs, real x)
{
    real c, z;

    z = 0;

    for (, c of coeffs) {
        z *= x;
        z += c;
    }

    z;
}


integer
main(void)
{
    o_(horner(list(-19r, 7.0, -4r, 6r), 3), "\n");

    0;
}
```



## ALGOL 68

{{works with|ALGOL 68G}}

```algol68
PROC horner = ([]REAL c, REAL x)REAL :
(
  REAL res := 0.0;
  FOR i FROM UPB c BY -1 TO LWB c DO
    res := res * x + c[i]
  OD;
  res
);

main:(
  [4]REAL coeffs := (-19.0, 7.0, -4.0, 6.0);
  print( horner(coeffs, 3.0) )
)
```



## ATS


```ATS
#include
"share/atspre_staload.hats"

fun
horner
(
  x: int, cs: List int
) : int = let
//
implement
list_foldright$fopr<int><int> (a, b) = a + b * x
//
in
  list_foldright<int><int> (cs, 0)
end // end of [horner]

implement
main0 () = let
  val x = 3
  val cs = $list{int}(~19, 7, ~4, 6)
  val res = horner (x, cs)
in
  println! (res)
end // end of [main0]
```



## AutoHotkey


```autohotkey
Coefficients = -19, 7, -4, 6
x := 3

MsgBox, % EvalPolynom(Coefficients, x)



;---------------------------------------------------------------------------
EvalPolynom(Coefficients, x) { ; using Horner's rule
;---------------------------------------------------------------------------
    StringSplit, Co, coefficients, `,, %A_Space%
    Result := 0
    Loop, % Co0
        i := Co0 - A_Index + 1, Result := Result * x + Co%i%
    Return, Result
}
```

Message box shows:

```txt
128
```



## AWK


```awk
#!/usr/bin/awk -f
function horner(x, A) {
	acc = 0;
	for (i = length(A); 0<i; i--) {
		acc = acc*x + A[i];
	}
	return acc;
}
BEGIN {
        split(p,P);
	print horner(x,P);
}
```


{{out}}

```txt

   awk  -v X=3 -v p="-19  7 -4  6" -f horner.awk
   128

```



## Batch File


```dos

@echo off

call:horners a:-19 b:7 c:-4 d:6 x:3
call:horners x:3 a:-19 c:-4 d:6 b:7
pause>nul
exit /b

:horners
setlocal enabledelayedexpansion
set a=0
set b=0
set c=0
set d=0
set x=0

for %%i in (%*) do (
  for /f "tokens=1,2 delims=:" %%j in ("%%i") do (
    set %%j=%%k
  )
)
set /a return=((((0)*%x%+%d%)*%x%+(%c%))*%x%+%b%)*%x%+(%a%)
echo %return%
exit /b

```

{{out}}

```txt

>a:-19 b:7 c:-4 d:6 x:3
128
>x:3 a:-19 c:-4 d:6 b:7
128

```




## BBC BASIC


```bbcbasic
      DIM coefficients(3)
      coefficients() = -19, 7, -4, 6
      PRINT FNhorner(coefficients(), 3)
      END

      DEF FNhorner(coeffs(), x)
      LOCAL i%, v
      FOR i% = DIM(coeffs(), 1) TO 0 STEP -1
        v = v * x + coeffs(i%)
      NEXT
      = v
```



## Bracmat


```bracmat
( ( Horner
  =   accumulator coefficients x coeff
    .   !arg:(?coefficients.?x)
      & 0:?accumulator
      &   whl
        ' ( !coefficients:?coefficients #%@?coeff
          & !accumulator*!x+!coeff:?accumulator
          )
      & !accumulator
  )
& Horner$(-19 7 -4 6.3)
);
```

Output:

```txt
128
```



## C

{{trans|Fortran}}

```c
#include <stdio.h>

double horner(double *coeffs, int s, double x)
{
  int i;
  double res = 0.0;

  for(i=s-1; i >= 0; i--)
  {
    res = res * x + coeffs[i];
  }
  return res;
}


int main()
{
  double coeffs[] = { -19.0, 7.0, -4.0, 6.0 };

  printf("%5.1f\n", horner(coeffs, sizeof(coeffs)/sizeof(double), 3.0));
  return 0;
}
```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Linq;

class Program
{
    static double Horner(double[] coefficients, double variable)
    {
        return coefficients.Reverse().Aggregate(
                (accumulator, coefficient) => accumulator * variable + coefficient);
    }

    static void Main()
    {
        Console.WriteLine(Horner(new[] { -19.0, 7.0, -4.0, 6.0 }, 3.0));
    }
}
```

Output:

```txt
128
```



## C++

The same C function works too, but another solution could be:


```cpp
#include <iostream>
#include <vector>

using namespace std;

double horner(vector<double> v, double x)
{
  double s = 0;

  for( vector<double>::const_reverse_iterator i = v.rbegin(); i != v.rend(); i++ )
    s = s*x + *i;
  return s;
}

int main()
{
  double c[] = { -19, 7, -4, 6 };
  vector<double> v(c, c + sizeof(c)/sizeof(double));
  cout << horner(v, 3.0) << endl;
  return 0;
}
```


Yet another solution, which is more idiomatic in C++ and works on any bidirectional sequence:


```cpp

#include <iostream>

template<typename BidirIter>
 double horner(BidirIter begin, BidirIter end, double x)
{
  double result = 0;
  while (end != begin)
    result = result*x + *--end;
  return result;
}

int main()
{
  double c[] = { -19, 7, -4, 6 };
  std::cout << horner(c, c + 4, 3) << std::endl;
}

```



## Clojure


```clojure
(defn horner [coeffs x]
  (reduce #(-> %1 (* x) (+ %2)) (reverse coeffs)))

(println (horner [-19 7 -4 6] 3))
```



## CoffeeScript


```coffeescript

eval_poly = (x, coefficients) ->
  # coefficients are for ascending powers
  return 0 if coefficients.length == 0
  ones_place = coefficients.shift()
  x * eval_poly(x, coefficients) + ones_place

console.log eval_poly 3, [-19, 7, -4, 6] # 128
console.log eval_poly 10, [4, 3, 2, 1] # 1234
console.log eval_poly 2, [1, 1, 0, 0, 1] # 19

```



## Common Lisp


```lisp
(defun horner (coeffs x)
  (reduce #'(lambda (coef acc) (+ (* acc x) coef))
	  coeffs :from-end t :initial-value 0))
```


Alternate version using LOOP. Coefficients are passed in a vector.


```lisp
(defun horner (x a)
    (loop :with y = 0
          :for i :from (1- (length a)) :downto 0
          :do (setf y (+ (aref a i) (* y x)))
          :finally (return y)))

(horner 1.414 #(-2 0 1))
```



## D

The poly() function of the standard library std.math module uses Horner's rule:

```d
void main() {
  void main() {
    import std.stdio, std.math;
 double x = 3.0;
static real[] pp = [-19,7,-4,6];

    poly(x,pp).writeln;
}
}
```

Basic implementation:

```d
import std.stdio, std.traits;

CommonType!(U, V) horner(U, V)(U[] p, V x) pure nothrow @nogc {
    typeof(return) accumulator = 0;
    foreach_reverse (c; p)
        accumulator = accumulator * x + c;
    return accumulator;
}

void main() {
    [-19, 7, -4, 6].horner(3.0).writeln;
}
```

More functional style:

```d
import std.stdio, std.algorithm, std.range;

auto horner(T, U)(in T[] p, in U x) pure nothrow @nogc {
    return reduce!((a, b) => a * x + b)(U(0), p.retro);
}

void main() {
    [-19, 7, -4, 6].horner(3.0).writeln;
}
```



## E



```e
def makeHornerPolynomial(coefficients :List) {
    def indexing := (0..!coefficients.size()).descending()
    return def hornerPolynomial(x) {
        var acc := 0
        for i in indexing {
            acc := acc * x + coefficients[i]
        }
        return acc
    }
}
```



```e
? makeHornerPolynomial([-19, 7, -4, 6])(3)
# value: 128
```



## EchoLisp


###  Functional version


```lisp

(define (horner x poly)
(foldr (lambda (coeff acc) (+ coeff (* acc x))) 0 poly))

(horner 3 '(-19 7 -4 6)) → 128

```


###  Library


```lisp

(lib 'math)
Lib: math.lib loaded.

(define P '(-19 7 -4 6))
(poly->string 'x P) → 6x^3 -4x^2 +7x -19
(poly 3 P) → 128

```


## Elena

{{trans|C#}}
ELENA 4.1 :

```elena
import extensions;
import system'routines;

horner(coefficients,variable)
{
    ^ coefficients.clone().sequenceReverse().accumulate(new Real(),(accumulator,coefficient => accumulator * variable + coefficient))
}

public program()
{
    console.printLine(horner(new real[]::(-19.0r, 7.0r, -4.0r, 6.0r), 3.0r))
}
```

{{out}}

```txt

128.0

```



## Elixir


```elixir
horner = fn(list, x)-> List.foldr(list, 0, fn(c,acc)-> x*acc+c end) end

IO.puts horner.([-19,7,-4,6], 3)
```


{{out}}

```txt

128

```



## Emacs Lisp

{{trans|Common Lisp}}

```Emacs Lisp

(defun horner (coeffs x)
  (reduce #'(lambda (coef acc) (+ (* acc x) coef) )
	  coeffs :from-end t :initial-value 0) )

(horner '(-19 7 -4 6) 3)

```

<b>Output:</b>

```txt

128

```



## Erlang


```erlang

horner(L,X) ->
  lists:foldl(fun(C, Acc) -> X*Acc+C end,0, lists:reverse(L)).
t() ->
  horner([-19,7,-4,6], 3).

```



## ERRE


```ERRE

PROGRAM HORNER

!                        2   3
! polynomial is -19+7x-4x +6x
!

DIM C[3]

PROCEDURE HORNER(C[],X->RES)
  LOCAL I%,V
  FOR I%=UBOUND(C,1) TO 0 STEP -1 DO
     V=V*X+C[I%]
  END FOR
  RES=V
END PROCEDURE

BEGIN
  C[]=(-19,7,-4,6)
  HORNER(C[],3->RES)
  PRINT(RES)
END PROGRAM

```



## Euler Math Toolbox



```Euler Math Toolbox

>function horner (x,v) ...
$  n=cols(v); res=v{n};
$  loop 1 to n-1; res=res*x+v{n-#}; end;
$  return res
$endfunction
>v=[-19,7,-4,6]
 [ -19  7  -4  6 ]
>horner(2,v) // test Horner
 27
>evalpoly(2,v) // built-in Horner
 27
>horner(I,v) // complex values
 -15+1i
>horner(1±0.05,v) // interval values
 ~-10.9,-9.11~
>function p(x) &= sum(@v[k]*x^(k-1),k,1,4) // Symbolic Polynomial
                            3      2
                         6 x  - 4 x  + 7 x - 19

```


=={{header|F Sharp|F#}}==

```fsharp

let horner l x =
    List.rev l |> List.fold ( fun acc c -> x*acc+c) 0

horner [-19;7;-4;6] 3

```



## Factor


```factor
: horner ( coeff x -- res )
    [ <reversed> 0 ] dip '[ [ _ * ] dip + ] reduce ;
```


 ( scratchpad ) { -19 7 -4 6 } 3 horner .
 128


## Forth


```forth
: fhorner ( coeffs len F: x -- F: val )
  0e
  floats bounds ?do
    fover f*  i f@ f+
  1 floats +loop
  fswap fdrop ;

create coeffs 6e f, -4e f, 7e f, -19e f,

coeffs 4 3e fhorner f.    \ 128.
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program test_horner

  implicit none

  write (*, '(f5.1)') horner ((/-19.0, 7.0, -4.0, 6.0/), 3.0)

contains

  function horner (coeffs, x) result (res)

    implicit none
    real, dimension (:), intent (in) :: coeffs
    real, intent (in) :: x
    real :: res
    integer :: i

    res = 0.0
    do i = size (coeffs), 1, -1
      res = res * x + coeffs (i)
    end do

  end function horner

end program test_horner
```

Output:

```txt
128.0
```



###  Fortran 77


```fortran
      FUNCTION HORNER(N,A,X)
      IMPLICIT NONE
      INTEGER I,N
      DOUBLE PRECISION A(N),X,Y,HORNER
      Y = A(N)
      DO I = N - 1,1,-1
        Y = Y*X + A(I)
      END DO
      HORNER=Y
      END
```


As a matter of fact, computing the derivative is not much more difficult (see [http://www.cs.berkeley.edu/~wkahan/Math128/Poly.pdf Roundoff in Polynomial Evaluation], W. Kahan, 1986). The following subroutine computes both polynomial value and derivative for argument x.


```fortran
      SUBROUTINE HORNER2(N,A,X,Y,Z)
C COMPUTE POLYNOMIAL VALUE AND DERIVATIVE
C SEE "ROUNDOFF IN POLYNOMIAL EVALUATION", W. KAHAN, 1986
C POLY: A(1) + A(2)*X + ... + A(N)*X**(N-1)
C Y: VALUE, Z: DERIVATIVE
      IMPLICIT NONE
      INTEGER I,N
      DOUBLE PRECISION A(N),X,Y,Z
      Z = 0.0D0
      Y = A(N)
      DO 10 I = N - 1,1,-1
        Z = Z*X + Y
   10 Y = Y*X + A(I)
      END

```




## FreeBASIC


```freebasic

Function AlgoritmoHorner(coeffs() As Integer, x As Integer) As Integer
    Dim As Integer  i, acumulador = 0
    For i = Ubound(coeffs, 1) To 0 Step -1
        acumulador = (acumulador * x) + coeffs(i)
    Next i
    Return acumulador
End Function

Dim As Integer x = 3
Dim As Integer coeficientes(3) = {-19, 7, -4, 6}
Print "Algoritmo de Horner para el polinomio 6*x^3 - 4*x^2 + 7*x - 19 para x = 3: ";
Print AlgoritmoHorner(coeficientes(), x)
End

```

{{out}}

```txt

Algoritmo de Horner para el polinomio 6*x^3 - 4*x^2 + 7*x - 19 para x = 3:  128

```




## FunL

{{trans|Haskell}}

```funl
import lists.foldr

def horner( poly, x ) = foldr( \a, b -> a + b*x, 0, poly )

println( horner([-19, 7, -4, 6], 3) )
```


{{out}}


```txt

128

```



## GAP


```gap
# The idiomatic way to compute with polynomials

x := Indeterminate(Rationals, "x");

# This is a value in a polynomial ring, not a function
p := 6*x^3 - 4*x^2 + 7*x - 19;

Value(p, 3);
# 128

u := CoefficientsOfUnivariatePolynomial(p);
# [ -19, 7, -4, 6 ]

# One may also create the polynomial from coefficients
q := UnivariatePolynomial(Rationals, [-19, 7, -4, 6], x);
# 6*x^3-4*x^2+7*x-19

p = q;
# true

# Now a Horner implementation
Horner := function(coef, x)
	local v, c;
	v := 0;
	for c in Reversed(coef) do
		v := x*v + c;
	od;
	return v;
end;

Horner(u, 3);
# 128
```



## Go


```go
package main

import "fmt"

func horner(x int64, c []int64) (acc int64) {
    for i := len(c) - 1; i >= 0; i-- {
        acc = acc*x + c[i]
    }
    return
}

func main() {
    fmt.Println(horner(3, []int64{-19, 7, -4, 6}))
}
```

Output:

```txt

128

```



## Groovy

Solution:

```groovy
def hornersRule = { coeff, x -> coeff.reverse().inject(0) { accum, c -> (accum * x) + c } }
```


Test includes demonstration of [[currying]] to create polynomial functions of one variable from generic Horner's rule calculation. Also demonstrates constructing the derivative function for the given polynomial. And finally demonstrates in the Newton-Raphson method to find one of the polynomial's roots using the polynomial and derivative functions constructed earlier.

```groovy
def coefficients = [-19g, 7g, -4g, 6g]
println (["p coefficients":coefficients])

def testPoly = hornersRule.curry(coefficients)
println (["p(3)":testPoly(3g)])
println (["p(0)":testPoly(0g)])

def derivativeCoefficients = { coeff -> (1..<(coeff.size())).collect { coeff[it] * it } }
println (["p' coefficients":derivativeCoefficients(coefficients)])

def testDeriv = hornersRule.curry(derivativeCoefficients(coefficients))
println (["p'(3)":testDeriv(3g)])
println (["p'(0)":testDeriv(0g)])

def newtonRaphson = { x, f, fPrime ->
    while (f(x).abs() > 0.0001) {
        x -= f(x)/fPrime(x)
    }
    x
}

def root = newtonRaphson(3g, testPoly, testDeriv)
println ([root:root.toString()[0..5], "p(root)":testPoly(root).toString()[0..5], "p'(root)":testDeriv(root).toString()[0..5]])
```


Output:

```txt
[p coefficients:[-19, 7, -4, 6]]
[p(3):128]
[p(0):-19]
[p' coefficients:[7, -8, 18]]
[p'(3):145]
[p'(0):7]
[root:1.4183, p(root):0.0000, p'(root):31.862]
```



## Haskell


```haskell
horner :: (Num a) => a -> [a] -> a
horner x = foldr (\a b -> a + b*x) 0

main = print $ horner 3 [-19, 7, -4, 6]
```



## HicEst


```HicEst
REAL :: x=3, coeffs(4)
DATA    coeffs/-19.0, 7.0, -4.0, 6.0/

WRITE(Messagebox) Horner(coeffs, x) ! shows 128

FUNCTION Horner(c, x)
   DIMENSION c(1)
   Horner = 0
   DO i = LEN(c), 1, -1
      Horner = x*Horner + c(i)
   ENDDO
END
```


=={{header|Icon}} and {{header|Unicon}}==


```Icon

procedure poly_eval (x, coeffs)
  accumulator := 0
  every index := *coeffs to 1 by -1 do
    accumulator := accumulator * x + coeffs[index]
  return accumulator
end

procedure main ()
  write (poly_eval (3, [-19, 7, -4, 6]))
end

```



## J

'''Solution''':
```j


   horner =: 4 :  '  (+ *&y)/x'

   horner1 =: (#."0 _ |.)~

   horner2=: [: +`*/ [: }: ,@,.    NB. Alternate

```

'''Example''':
```j
   _19 7 _4 6 horner 3
128
```

'''Note:'''

The primitive verb <code>p.</code> would normally be used to evaluate polynomials.

```j
   _19 7 _4 6 p. 3
128
```



## Java

{{works with|Java|1.5+}}

```java5
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Horner {
    public static void main(String[] args){
        List<Double> coeffs = new ArrayList<Double>();
        coeffs.add(-19.0);
        coeffs.add(7.0);
        coeffs.add(-4.0);
        coeffs.add(6.0);
        System.out.println(polyEval(coeffs, 3));
    }

    public static double polyEval(List<Double> coefficients, double x) {
        Collections.reverse(coefficients);
        Double accumulator = coefficients.get(0);
        for (int i = 1; i < coefficients.size(); i++) {
            accumulator = (accumulator * x) + (Double) coefficients.get(i);
        }
        return accumulator;
    }
}
```

Output:

```txt
128.0
```



## JavaScript

{{works with|JavaScript|1.8}} which includes {{works with|Firefox|3}}

{{trans|Haskell}}

```javascript
function horner(coeffs, x) {
    return coeffs.reduceRight( function(acc, coeff) { return(acc * x + coeff) }, 0);
}
console.log(horner([-19,7,-4,6],3));  // ==> 128

```



## Julia

{{works with|Julia|0.6}}

'''Imperative''':

```julia
function horner(coefs, x)
    s = coefs[end]
    for k in length(coefs)-1:-1:1
        s = coefs[k] + x * s
    end
    return s
end

@show horner([-19, 7, -4, 6], 3)
```


{{out}}

```txt
horner([-19, 7, -4, 6], 3) = 128
```


'''Functional''':

```julia
horner2(coefs, x) = foldr((u, v) -> u + x * v, 0, coefs)

@show horner2([-19, 7, -4, 6], 3)
```


{{out}}

```txt
horner2([-19, 7, -4, 6], 3) = 128
```



## K


```K

  horner:{y _sv|x}
  horner[-19 7 -4 6;3]
128

```



## Kotlin


```scala
// version 1.1.2

fun horner(coeffs: DoubleArray, x: Double): Double {
    var sum = 0.0
    for (i in coeffs.size - 1 downTo 0) sum = sum * x + coeffs[i]
    return sum
}

fun main(args: Array<String>) {
    val coeffs = doubleArrayOf(-19.0, 7.0, -4.0, 6.0)
    println(horner(coeffs, 3.0))
}
```


{{out}}

```txt

128.0

```



## Liberty BASIC


```lb
src$ = "Hello"
coefficients$ = "-19 7 -4 6" ' list coefficients of all x^0..x^n in order
x = 3
print horner(coefficients$, x)      '128

print horner("4  3  2  1", 10)      '1234
print horner("1  1  0  0  1", 2)    '19
end

function horner(coefficients$, x)
    accumulator = 0
    'getting length of a list requires extra pass with WORD$.
    'So we just started from high above
    for index = 100 to 1 step -1
        cft$ = word$(coefficients$, index)
        if cft$<>"" then accumulator = ( accumulator * x ) + val(cft$)
    next
    horner = accumulator
end function

```



## Logo


```logo
to horner :x :coeffs
  if empty? :coeffs [output 0]
  output (first :coeffs) + (:x * horner :x bf :coeffs)
end

show horner 3 [-19 7 -4 6]   ; 128
```



## Lua


```lua
function horners_rule( coeff, x )
    local res = 0
    for i = #coeff, 1, -1 do
        res = res * x + coeff[i]
    end
    return res
end

x = 3
coefficients = { -19, 7, -4, 6 }
print( horners_rule( coefficients, x ) )
```




## Maple


```Maple

applyhorner:=(L::list,x)->foldl((s,t)->s*x+t,op(ListTools:-Reverse(L))):

applyhorner([-19,7,-4,6],x);

applyhorner([-19,7,-4,6],3);

```

Output:

```txt

                    ((6 x - 4) x + 7) x - 19

                              128

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Horner[l_List, x_] := Fold[x #1 + #2 &, 0, l]
Horner[{6, -4, 7, -19}, x]
-> -19 + x (7 + x (-4 + 6 x))

-19 + x (7 + x (-4 + 6 x)) /. x -> 3
-> 128
```



## MATLAB


```MATLAB
function accumulator = hornersRule(x,coefficients)

    accumulator = 0;

    for i = (numel(coefficients):-1:1)
        accumulator = (accumulator * x) + coefficients(i);
    end

end
```

Output:

```MATLAB>>
 hornersRule(3,[-19, 7, -4, 6])

ans =

   128
```

Matlab also has a built-in function "polyval" which uses Horner's Method to evaluate polynomials. The list of coefficients is in descending order of power, where as to task spec specifies ascending order.

```MATLAB>>
 polyval(fliplr([-19, 7, -4, 6]),3)

ans =

   128
```



## Maxima


```maxima
/* Function horner already exists in Maxima, though it operates on expressions, not lists of coefficients */
horner(5*x^3+2*x+1);
x*(5*x^2+2)+1

/* Here is an implementation */
horner2(p, x) := block([n, y, i],
   n: length(p),
   y: p[n],
   for i: n - 1 step -1 thru 1 do y: y*x + p[i],
   y
)$

horner2([-19, 7, -4, 6], 3);
128

/* Another with rreduce */
horner3(p,x):=rreduce(lambda([a,y],x*y+a),p);
horner3([a,b,c,d,e,f],x);
x*(x*(x*(x*(f*x+e)+d)+c)+b)+a

/* Extension to compute also derivatives up to a specified order.
   See William Kahan, Roundoff in Polynomial Evaluation, 1986
   http://www.cs.berkeley.edu/~wkahan/Math128/Poly.pdf */

poleval(a, x, [m]) := block(
   [n: length(a), v, k: 1],
   if emptyp(m) then m: 1 else m: 1 + first(m),
   v: makelist(0, m),
   v[1]: a[n],
   for i from n - 1 thru 1 step -1 do (
      for j from m thru 2 step -1 do v[j]: v[j] * x + v[j - 1],
      v[1]: v[1] * x + a[i]
   ),
   for i from 2 thru m do (
      v[i]: v[i] * k,
      k: k * i
   ),
   if m = 1 then first(v) else v
)$

poleval([0, 0, 0, 0, 1], x, 4);
[x^4, 4 * x^3, 12 * x^2, 24 * x, 24]

poleval([0, 0, 0, 0, 1], x);
x^4
```



## Mercury


```mercury

:- module horner.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, list, string.

main(!IO) :-
    io.format("%i\n", [i(horner(3, [-19, 7, -4, 6]))], !IO).

:- func horner(int, list(int)) = int.

horner(X, Cs) = list.foldr((func(C, Acc) = Acc * X + C), Cs, 0).

```


=={{header|МК-61/52}}==
<lang>ИП0	1	+	П0
ИПE	ИПD	*	КИП0	+	ПE
ИП0	1	-	x=0	04
ИПE	С/П
```


''Input:'' Р1:РС - coefficients, Р0 - number of the coefficients, РD - ''x''.

=={{header|Modula-2}}==

```modula2
MODULE Horner;
FROM RealStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE Horner(coeff : ARRAY OF REAL; x : REAL) : REAL;
VAR
    ans : REAL;
    i : CARDINAL;
BEGIN
    ans := 0.0;
    FOR i:=HIGH(coeff) TO 0 BY -1 DO
        ans := (ans * x) + coeff[i];
    END;
    RETURN ans
END Horner;

TYPE A = ARRAY[0..3] OF REAL;
VAR
    buf : ARRAY[0..63] OF CHAR;
    coeff : A;
    ans : REAL;
BEGIN
    coeff := A{-19.0, 7.0, -4.0, 6.0};
    ans := Horner(coeff, 3.0);
    RealToStr(ans, buf);
    WriteString(buf);
    WriteLn;
    ReadChar
END Horner.
```



## NetRexx


```netrexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

c = [-19, 7, -4, 6] -- # list coefficients of all x^0..x^n in order
n=3
x=3
r=0
loop i=n to 0 by -1
  r=r*x+c[i]
  End
Say r
Say 6*x**3-4*x**2+7*x-19
```

'''Output:'''

```txt
128
128
```



## Nim


```nim
# You can also just use `reversed` proc from stdlib `algorithm` module
iterator reversed[T](x: openArray[T]): T =
  for i in countdown(x.high, x.low):
    yield x[i]

proc horner[T](coeffs: openArray[T], x: T): int =
  for c in reversed(coeffs):
    result = result * x + c

echo horner([-19, 7, -4, 6], 3)
```


=={{header|Oberon-2}}==
{{works with|oo2c}}

```oberon2

MODULE HornerRule;
IMPORT
  Out;

TYPE
  Coefs = POINTER TO ARRAY OF LONGINT;
VAR
  coefs: Coefs;

PROCEDURE Eval(coefs: ARRAY OF LONGINT;size,x: LONGINT): LONGINT;
VAR
  i,acc: LONGINT;
BEGIN
  acc := 0;
  FOR i := LEN(coefs) - 1 TO 0 BY -1 DO
	acc := acc * x + coefs[i]
  END;
  RETURN acc
END Eval;

BEGIN
  NEW(coefs,4);
  coefs[0] := -19;
  coefs[1] := 7;
  coefs[2] := -4;
  coefs[3] := 6;
  Out.Int(Eval(coefs^,4,3),0);Out.Ln
END HornerRule.

```

{{out}}

```txt

128

```


=={{header|Objective-C}}==
{{works with|Mac OS X|10.6+}} Using blocks

```objc>#import <Foundation/Foundation.h


typedef double (^mfunc)(double, double);

@interface NSArray (HornerRule)
- (double)horner: (double)x;
- (NSArray *)reversedArray;
- (double)injectDouble: (double)s with: (mfunc)op;
@end

@implementation NSArray (HornerRule)
- (NSArray *)reversedArray
{
  return [[self reverseObjectEnumerator] allObjects];
}


- (double)injectDouble: (double)s with: (mfunc)op
{
  double sum = s;
  for(NSNumber* el in self) {
    sum = op(sum, [el doubleValue]);
  }
  return sum;
}

- (double)horner: (double)x
{
  return [[self reversedArray] injectDouble: 0.0 with: ^(double s, double a) { return s * x + a; } ];
}
@end

int main()
{
  @autoreleasepool {

    NSArray *coeff = @[@-19.0, @7.0, @-4.0, @6.0];
    printf("%f\n", [coeff horner: 3.0]);

  }
  return 0;
}
```



## Objeck


```objeck

class Horner {
  function : Main(args : String[]) ~ Nil {
    coeffs := Collection.FloatVector->New();
    coeffs->AddBack(-19.0);
    coeffs->AddBack(7.0);
    coeffs->AddBack(-4.0);
    coeffs->AddBack(6.0);
    PolyEval(coeffs, 3)->PrintLine();
  }

  function : PolyEval(coefficients : Collection.FloatVector , x : Float) ~ Float {
    accumulator := coefficients->Get(coefficients->Size() - 1);
    for(i := coefficients->Size() - 2; i > -1; i -= 1;) {
       accumulator := (accumulator * x) + coefficients->Get(i);
    };

    return accumulator;
  }
}

```



## OCaml



```ocaml
# let horner coeffs x =
    List.fold_left (fun acc coef -> acc * x + coef) 0 (List.rev coeffs) ;;
val horner : int list -> int -> int = <fun>

# let coeffs = [-19; 7; -4; 6] in
  horner coeffs 3 ;;
- : int = 128
```

It's also possible to do fold_right instead of reversing and doing fold_left; but fold_right is not tail-recursive.


## Octave


```octave
function r = horner(a, x)
  r = 0.0;
  for i = length(a):-1:1
    r = r*x + a(i);
  endfor
endfunction

horner([-19, 7, -4, 6], 3)
```



## ooRexx


```oorexx
/* Rexx ---------------------------------------------------------------
* 04.03.2014 Walter Pachl
*--------------------------------------------------------------------*/
c = .array~of(-19,7,-4,6) -- coefficients of all x^0..x^n in order
n=3
x=3
r=0
loop i=n+1 to 1 by -1
  r=r*x+c[i]
  End
Say r
Say 6*x**3-4*x**2+7*x-19
```

'''Output:'''

```txt
128
128
```



## Oz


```oz
declare
  fun {Horner Coeffs X}
     {FoldL1 {Reverse Coeffs}
      fun {$ Acc Coeff}
         Acc*X + Coeff
      end}
  end

  fun {FoldL1 X|Xr Fun}
     {FoldL Xr Fun X}
  end
in
  {Show {Horner [~19 7 ~4 6] 3}}
```



## PARI/GP

Also note that Pari has a polynomial type.  Evaluating these is as simple as <code>subst(P,variable(P),x)</code>.

```parigp
horner(v,x)={
  my(s=0);
  forstep(i=#v,1,-1,s=s*x+v[i]);
  s
};
```



## Pascal


```pascal
Program HornerDemo(output);

function horner(a: array of double; x: double): double;
  var
    i: integer;
  begin
    horner := a[high(a)];
    for i := high(a) - 1 downto low(a) do
      horner := horner * x + a[i];
  end;

const
  poly: array [1..4] of double = (-19.0, 7.0, -4.0, 6.0);

begin
  write ('Horner calculated polynomial of 6*x^3 - 4*x^2 + 7*x - 19 for x = 3: ');
  writeln (horner (poly, 3.0):8:4);
end.
```

Output:

```txt
Horner calculated polynomial of 6*x^3 - 4*x^2 + 7*x - 19 for x = 3: 128.0000

```



## Perl


```Perl
use 5.10.0;
use strict;
use warnings;

sub horner(\@$){
	my ($coef, $x) = @_;
	my $result = 0;
	$result = $result * $x + $_ for reverse @$coef;
	return $result;
}

my @coeff = (-19.0, 7, -4, 6);
my $x = 3;
say horner @coeff, $x;
```


===<!-- Perl -->Functional version===

```perl
use strict;
use List::Util qw(reduce);

sub horner($$){
	my ($coeff_ref, $x) = @_;
	reduce { $a * $x + $b } reverse @$coeff_ref;
}

my @coeff = (-19.0, 7, -4, 6);
my $x = 3;
print horner(\@coeff, $x), "\n";
```


===<!-- Perl -->Recursive version===

```perl
sub horner {
    my ($coeff, $x) = @_;
    @$coeff and
    $$coeff[0] + $x * horner( [@$coeff[1 .. $#$coeff]], $x )
}

print horner( [ -19, 7, -4, 6 ], 3 );
```



## Perl 6


```perl6
sub horner ( @coeffs, $x ) {
    @coeffs.reverse.reduce: { $^a * $x + $^b };
}

say horner( [ -19, 7, -4, 6 ], 3 );
```


A recursive version would spare us the need for reversing the list of coefficients.  However, special care must be taken in order to write it, because the way Perl 6 implements lists is not optimized for this kind of treatment.  [[Lisp]]-style lists are, and fortunately it is possible to emulate them with [http://doc.perl6.org/type/Pair Pairs] and the reduction meta-operator:


```perl6
multi horner(Numeric $c, $) { $c }
multi horner(Pair $c, $x) {
    $c.key + $x * horner( $c.value, $x )
}

say horner( [=>](-19, 7, -4, 6 ), 3 );
```


We can also use the composition operator:

```perl6
sub horner ( @coeffs, $x ) {
    ([o] map { $_ + $x * * }, @coeffs)(0);
}

say horner( [ -19, 7, -4, 6 ], 3 );
```


{{out}}

```txt
128
```


One advantage of using the composition operator is that it allows for the use of an infinite list of coefficients.

```perl6
sub horner ( @coeffs, $x ) {
    map { .(0) }, [\o] map { $_ + $x * * }, @coeffs;
}

say horner( [ 1 X/ (1, |[\*] 1 .. *) ], i*pi )[20];

```

{{out}}

```txt
-0.999999999924349-5.28918515954219e-10i
```



## Phix


```Phix
function horner(atom x, sequence coeff)
atom res = 0
    for i=length(coeff) to 1 by -1 do
        res = res*x + coeff[i]
    end for
    return res
end function

?horner(3,{-19, 7, -4, 6})
```

{{out}}

```txt

128

```



## PHP


```php
<?php
function horner($coeff, $x) {
    $result = 0;
    foreach (array_reverse($coeff) as $c)
        $result = $result * $x + $c;
    return $result;
}

$coeff = array(-19.0, 7, -4, 6);
$x = 3;
echo horner($coeff, $x), "\n";
?>
```



### Functional version

{{works with|PHP|5.3+}}

```php
<?php
function horner($coeff, $x) {
    return array_reduce(array_reverse($coeff), function ($a, $b) use ($x) { return $a * $x + $b; }, 0);
}

$coeff = array(-19.0, 7, -4, 6);
$x = 3;
echo horner($coeff, $x), "\n";
?>
```



## PicoLisp


```PicoLisp
(de horner (Coeffs X)
   (let Res 0
      (for C (reverse Coeffs)
         (setq Res (+ C (* X Res))) ) ) )
```


```PicoLisp
: (horner (-19.0 7.0 -4.0 6.0) 3.0)
-> 128
```



## PL/I


```PL/I

declare (i, n) fixed binary, (x, value) float; /* 11 May 2010 */
get (x);
get (n);
begin;
   declare a(0:n) float;
   get list (a);
   value = a(n);
   do i = n to 1 by -1;
      value = value*x + a(i-1);
   end;
   put (value);
end;

```



## Potion


```potion
horner = (x, coef) :
   result = 0
   coef reverse each (a) :
      result = (result * x) + a
   .
   result
.

horner(3, (-19, 7, -4, 6)) print
```



## PowerShell

{{works with|PowerShell|4.0}}

```PowerShell

function horner($coefficients, $x) {
    $accumulator = 0
    foreach($i in ($coefficients.Count-1)..0){
        $accumulator = ( $accumulator * $x ) + $coefficients[$i]
    }
    $accumulator
}
$coefficients = @(-19, 7, -4, 6)
$x = 3
horner $coefficients $x

```

<b>Output:</b>

```txt

128

```



## Prolog

Tested with SWI-Prolog. Works with other dialects.

```Prolog
horner([], _X, 0).

horner([H|T], X, V) :-
	horner(T, X, V1),
	V is V1 * X + H.

```

Output :

```Prolog
 ?- horner([-19, 7, -4, 6], 3, V).
V = 128.
```



### Functional approach

Works with SWI-Prolog and module lambda, written by <b>Ulrich Neumerkel</b> found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl

```Prolog
:- use_module(library(lambda)).


% foldr(Pred, Init, List, R).
%
foldr(_Pred, Val, [], Val).
foldr(Pred, Val, [H | T], Res) :-
	foldr(Pred, Val, T, Res1),
	call(Pred, Res1, H, Res).

f_horner(L, V, R) :-
	foldr(\X^Y^Z^(Z is X * V + Y), 0, L, R).

```


===Functional syntax (Ciao)===
Works with Ciao (https://github.com/ciao-lang/ciao) and the fsyntax package:

```Prolog

:- module(_, [horner/3], [fsyntax, hiord]).
:- use_module(library(hiordlib)).
horner(L, X) := ~foldr((''(H,V0,V) :- V is V0*X + H), L, 0).

```



## PureBasic


```PureBasic
Procedure Horner(List Coefficients(), b)
  Define result
  ForEach Coefficients()
    result*b+Coefficients()
  Next
  ProcedureReturn result
EndProcedure
```


'''Implemented as

```PureBasic
NewList a()
AddElement(a()): a()=  6
AddElement(a()): a()= -4
AddElement(a()): a()=  7
AddElement(a()): a()=-19
Debug Horner(a(),3)
```

'''Outputs
 128


## Python


```python>>>
 def horner(coeffs, x):
	acc = 0
	for c in reversed(coeffs):
		acc = acc * x + c
	return acc

>>> horner( (-19, 7, -4, 6), 3)
128
```



### Functional version


```python>>>
 try: from functools import reduce
except: pass

>>> def horner(coeffs, x):
	return reduce(lambda acc, c: acc * x + c, reversed(coeffs), 0)

>>> horner( (-19, 7, -4, 6), 3)
128
```


==={{libheader|NumPy}}===

```python>>>
 import numpy
>>> numpy.polynomial.polynomial.polyval(3, (-19, 7, -4, 6))
128.0
```



## R

Procedural style:

```r
horner <- function(a, x) {
  y <- 0
  for(c in rev(a)) {
    y <- y * x + c
  }
  y
}

cat(horner(c(-19, 7, -4, 6), 3), "\n")
```

Functional style:

```r
horner <- function(x, v) {
  Reduce(v, right=T, f=function(a, b) {
    b * x + a
  })
}
```

{{out}}

```txt

> v <- c(-19, 7, -4, 6)
> horner(3, v)
[1] 128

```



## Racket

Translated from Haskell


```racket

#lang racket
(define (horner x l)
    (foldr (lambda (a b) (+ a (* b x))) 0 l))

(horner 3 '(-19 7 -4 6))


```



## Rascal


```rascal
import List;

public int horners_rule(list[int] coefficients, int x){
	acc = 0;
	for( i <- reverse(coefficients)){
		acc = acc * x + i;}
	return acc;
}
```

A neater and shorter solution using a reducer:

```rascal
public int horners_rule2(list[int] coefficients, int x) = (0 | it * x + c | c <- reverse(coefficients));
```

Output:

```rascal>rascal
horners_rule([-19, 7, -4, 6], 3)
int: 128

rascal>horners_rule2([-19, 7, -4, 6], 3)
int: 128
```



## REBOL



```rebol
REBOL []

horner: func [coeffs x] [
    result: 0
    foreach i reverse coeffs [
        result: (result * x) + i
        ]
    return result
    ]

print horner [-19 7 -4 6] 3
```



## REXX


### version 1


```rexx
/*REXX program  demonstrates using    Horner's rule    for   polynomial evaluation.     */
numeric digits 30                                /*use extra numeric precision.         */
parse  arg  x poly                               /*get value of X and the coefficients. */
$=                                               /*start with a clean slate equation.   */
       do deg=0  until  poly==''                 /*get the equation's coefficients.     */
       parse var poly c.deg poly;  c.deg=c.deg/1 /*get equation coefficient & normalize.*/
       if c.deg>=0  then c.deg= '+'c.deg         /*if ¬ negative, then prefix with a  + */
       $=$  c.deg                                /*concatenate it to the equation.      */
       if deg\==0 & c.deg\=0  then $=$'∙x^'deg   /*¬1st coefficient & ¬0?  Append X pow.*/
       $=$ '  '                                  /*insert some blanks, make it look nice*/
       end   /*deg*/
say '         x = '   x
say '    degree = '  deg
say '  equation = '   $
a=c.deg                                          /*A:  is the accumulator  (or answer). */
         do j=deg-1  by -1  for deg;   a=a*x+c.j /*apply Horner's rule to the equations.*/
         end   /*j*/
say                                              /*display a blank line for readability.*/
say '    answer = ' a                            /*stick a fork in it,  we're all done. */
```

'''output'''   when the following is used for input:   <tt> 3   -19   7   -4   6 </tt>

```txt

         x =  3
    degree =  3
  equation =   -19    +7∙x^1    -4∙x^2    +6∙x^3

    answer =  128

```



### version 2


```rexx
/* REXX ---------------------------------------------------------------
* 27.07.2012 Walter Pachl
*            coefficients reversed to descending order of power
*            I'm used to x**2+x-3
*            equation formatting prettified (coefficients 1 and 0)
*--------------------------------------------------------------------*/
  Numeric Digits 30                /* use extra numeric precision.   */
  Parse Arg x poly                 /* get value of x and coefficients*/
  rpoly=''
  Do p=0 To words(poly)-1
    rpoly=rpoly word(poly,words(poly)-p)
    End
  poly=rpoly
  equ=''                           /* start with equation clean slate*/
  deg=words(poly)-1
  pdeg=deg
  Do Until deg<0                   /* get the equation's coefficients*/
    Parse Var poly c.deg poly      /* in descending order of powers  */
    c.deg=c.deg+0                  /* normalize it                   */
    If c.deg>0 & deg<pdeg Then     /* positive and not first term    */
      prefix='+'                   /*  prefix a + sign.              */
    Else prefix=''
    Select
      When deg=0 Then term=c.deg
      When deg=1 Then
        If c.deg=1 Then term='x'
                   Else term=c.deg'*x'
      Otherwise
        If c.deg=1 Then term='x^'deg
                   Else term=c.deg'*x^'deg
      End
    If c.deg<>0 Then               /* build up the equation          */
      equ=equ||prefix||term
    deg=deg-1
    End
  a=c.pdeg
  Do p=pdeg To 1 By -1             /* apply Horner's rule.           */
    pm1=p-1
    a=a*x+c.pm1
    End
  Say '        x = ' x
  Say '   degree = ' pdeg
  Say ' equation = ' equ
  Say ' '
  Say '   result = ' a
```

{{out}}

```txt
        x =  3
   degree =  3
 equation =  6*x^3-4*x^2+7*x-19

   result =  128
```



## Ring


```ring

coefficients = [-19, 7, -4, 6]
see "x =  3" + nl +
"degree =  3" + nl +
"equation =  6*x^3-4*x^2+7*x-19" + nl +
"result = " + horner(coefficients, 3) + nl

func horner coeffs, x
w = 0
for n = len(coeffs) to 1 step -1
    w = w * x + coeffs[n]
next
return w

```

Output:

```txt

x =  3
degree =  3
equation =  6*x^3-4*x^2+7*x-19
result = 128

```



## RLaB


RLaB implements horner's scheme for polynomial evaluation in its built-in function ''polyval''.
What is important is that RLaB stores the polynomials as row vectors starting from the highest
power just as matlab and octave do.

This said, solution to the problem is

```RLaB

>> a = [6, -4, 7, -19]
           6            -4             7           -19
>> x=3
           3
>> polyval(x, a)
         128


```



## Ruby


```ruby
def horner(coeffs, x)
  coeffs.reverse.inject(0) {|acc, coeff| acc * x + coeff}
end
p horner([-19, 7, -4, 6], 3)  # ==> 128
```



## Rust


```rust
fn horner(v: &[f64], x: f64) -> f64 {
    v.iter().rev().fold(0.0, |acc, coeff| acc*x + coeff)
}

fn main() {
    let v = [-19., 7., -4., 6.];
    println!("result: {}", horner(&v, 3.0));
}
```


A generic version that works with any number type and much more. So much more, it's hard to imagine what that may be useful for.

```rust
extern crate num; // 0.2.0
use num::Zero;
use std::ops::{Add, Mul};

fn horner<Arr, Arg, Out>(v: &[Arr], x: Arg) -> Out
where
    Arr: Clone,
    Arg: Clone,
    Out: Zero + Mul<Arg, Output = Out> + Add<Arr, Output = Out>,
{
    v.iter()
        .rev()
        .fold(Zero::zero(), |acc, coeff| acc * x.clone() + coeff.clone())
}

fn main() {
    let v = [-19., 7., -4., 6.];
    let output: f64 = horner(&v, 3.0);
    println!("result: {}", output);
}
```



## Run BASIC


```runbasic
coef$ = "-19 7 -4 6" ' list coefficients of all x^0..x^n in order
x = 3
print horner(coef$,x)                     '128
print horner("1.2 2.3 3.4 4.5 5.6", 8)    '25478.8
print horner("5 4 3 2 1", 10)             '12345
print horner("1 0 1 1 1 0 0 1", 2)        '157
end

function horner(coef$,x)
  while word$(coef$, i + 1) <> ""
     i = i + 1                          ' count the num of values
  wend
  for j = i to 1 step -1
    accum = ( accum * x ) + val(word$(coef$, j))
  next
  horner = accum
end function
```



## Sather


```sather
class MAIN is

  action(s, e, x:FLT):FLT is
    return s*x + e;
  end;

  horner(v:ARRAY{FLT}, x:FLT):FLT is
    rv ::= v.reverse;
    return rv.reduce(bind(action(_, _, x)));
  end;

  main is
    #OUT + horner(|-19.0, 7.0, -4.0, 6.0|, 3.0) + "\n";
  end;
end;
```



## Scala


```scala
def horner(coeffs:List[Double], x:Double)=
   coeffs.reverse.foldLeft(0.0){(a,c)=> a*x+c}

```


```scala
val coeffs=List(-19.0, 7.0, -4.0, 6.0)
println(horner(coeffs, 3))
   -> 128.0

```



## Scheme

{{Works with|Scheme|R<math>^5</math>RS}}

```scheme
(define (horner lst x)
  (define (*horner lst x acc)
    (if (null? lst)
        acc
        (*horner (cdr lst) x (+ (* acc x) (car lst)))))
  (*horner (reverse lst) x 0))

(display (horner (list -19 7 -4 6) 3))
(newline)
```

Output:
<lang>128
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const type: coeffType is array float;

const func float: horner (in coeffType: coeffs, in float: x) is func
  result
    var float: res is 0.0;
  local
    var integer: i is 0;
  begin
    for i range length(coeffs) downto 1 do
      res := res * x + coeffs[i];
    end for;
  end func;

const proc: main is func
  local
    const coeffType: coeffs is [] (-19.0, 7.0, -4.0, 6.0);
  begin
    writeln(horner(coeffs, 3.0) digits 1);
  end func;
```


Output:

```txt

128.0

```



## Sidef

Functional:

```ruby
func horner(coeff, x) {
    coeff.reverse.reduce { |a,b| a*x + b };
}

say horner([-19, 7, -4, 6], 3);   # => 128
```


Recursive:

```ruby
func horner(coeff, x) {
    coeff.len > 0
        && (coeff[0] + x*horner(coeff.ft(1), x));
}

say horner([-19, 7, -4, 6], 3);   # => 128
```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
OrderedCollection extend [
  horner: aValue [
    ^ self reverse inject: 0 into: [:acc :c | acc * aValue + c].
  ]
].

(#(-19 7 -4 6) asOrderedCollection horner: 3) displayNl.
```



## Standard ML


```sml
(* Assuming real type for coefficients and x *)
fun horner coeffList x = foldr (fn (a, b) => a + b * x) (0.0) coeffList
```



## Swift


```swift
func horner(coefs: [Double], x: Double) -> Double {
  return reduce(lazy(coefs).reverse(), 0) { $0 * x + $1 }
}

println(horner([-19, 7, -4, 6], 3))
```

{{out}}

```txt
128.0
```



## Tcl


```tcl
package require Tcl 8.5
proc horner {coeffs x} {
    set y 0
    foreach c [lreverse $coeffs] {
        set y [expr { $y*$x+$c }]
    }
    return $y
}
```

Demonstrating:

```tcl
puts [horner {-19 7 -4 6} 3]
```

Output:

```txt
128
```



## VBA


Note: this function, "Horner", gets its coefficients as a ParamArray which has no specified length. This array collect all arguments after the first one(s). This means you must specify x first, then the coefficients.


```VBA

Public Function Horner(x, ParamArray coeff())
Dim result As Double
Dim ncoeff As Integer

result = 0
ncoeff = UBound(coeff())

For i = ncoeff To 0 Step -1
  result = (result * x) + coeff(i)
Next i
Horner = result
End Function

```


Output:

```txt

print Horner(3, -19, 7, -4, 6)
 128

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Function Horner(coefficients As Double(), variable As Double) As Double
        Return coefficients.Reverse().Aggregate(Function(acc, coeff) acc * variable + coeff)
    End Function

    Sub Main()
        Console.WriteLine(Horner({-19.0, 7.0, -4.0, 6.0}, 3.0))
    End Sub

End Module
```

{{out}}

```txt
128
```



## Visual FoxPro


### Coefficients in ascending order.


```vfp

LOCAL x As Double
LOCAL ARRAY aCoeffs[1]
CLEAR
CREATE CURSOR coeffs (c1 I, c2 I, c3 I, c4 I)
INSERT INTO coeffs VALUES (-19,7,-4,6)
SCATTER TO aCoeffs
x = VAL(INPUTBOX("Value of x:", "Value"))
? EvalPoly(@aCoeffs, x)
USE IN coeffs

FUNCTION EvalPoly(c, x As Double) As Double
LOCAL s As Double, k As Integer, n As Integer
n = ALEN(c)
s = 0
FOR k = n TO 1 STEP -1
	s = s*x + c[k]
ENDFOR
RETURN s
ENDFUNC

```



### Coefficients in descending order.


```vfp

LOCAL x As Double
LOCAL ARRAY aCoeffs[1]
CLEAR
CREATE CURSOR tmp (c1 I, c2 I, c3 I, c4 I)
INSERT INTO tmp VALUES (6,-4,7,-19)
SCATTER TO aCoeffs
x = VAL(INPUTBOX("Value of x:", "Value"))
? EvalPolyDesc(@aCoeffs, x)
USE IN tmp

FUNCTION EvalPolyDesc(c, x As Double) As Double
LOCAL s As Double, e
s = 0
FOR EACH e IN c FOXOBJECT
	s = s*x + e
ENDFOR
RETURN s
ENDFUNC

```



## VBScript


```vb

Function horners_rule(coefficients,x)
	accumulator = 0
	For i = UBound(coefficients) To 0 Step -1
		accumulator = (accumulator * x) + coefficients(i)
	Next
	horners_rule = accumulator
End Function

WScript.StdOut.WriteLine horners_rule(Array(-19,7,-4,6),3)

```


{{Out}}

```txt
128
```



## XPL0


```XPL0
code IntOut=11;

func Horner(X, N, C);   \Return value of polynomial in X
int X, N, C;            \variable, number of terms, coefficient list
int A, I;
[A:= 0;
for I:= N-1 downto 0 do
        A:= A*X + C(I);
return A;
];

IntOut(0, Horner(3, 4, [-19, 7, -4, 6]));
```


Output:

```txt

128

```



## zkl


```zkl
fcn horner(coeffs,x)
   { coeffs.reverse().reduce('wrap(a,coeff){ a*x + coeff },0.0) }
```

{{out}}

```txt

horner(T(-19,7,-4,6), 3).println();
128

```

