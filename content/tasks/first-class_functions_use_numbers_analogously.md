+++
title = "First-class functions/Use numbers analogously"
description = ""
date = 2019-10-18T10:27:50Z
aliases = []
[extra]
id = 4649
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "axiom",
  "bbc_basic",
  "clojure",
  "common_lisp",
  "csharp",
  "d",
  "e",
  "elena",
  "erlang",
  "factor",
  "fantom",
  "fsharp",
  "go",
  "groovy",
  "haskell",
  "j",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "nemerle",
  "never",
  "objeck",
  "ocaml",
  "oforth",
  "oz",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "sidef",
  "slate",
  "tcl",
  "ursala",
  "zkl",
]
+++

In [[First-class functions]], a language is showing how its manipulation of functions is similar to its manipulation of other types.

This tasks aim is to compare and contrast a language's implementation of first class functions, with its normal handling of numbers.


Write a program to create an ordered collection of a mixture of literally typed and expressions producing a real number, together with another ordered collection of their multiplicative inverses. Try and use the following pseudo-code to generate the numbers for the ordered collections:
   x  = 2.0
   xi = 0.5
   y  = 4.0
   yi = 0.25
   z  = x + y
   zi = 1.0 / ( x + y )

Create a function ''multiplier'', that given two numbers as arguments returns a function that when called with one argument, returns the result of multiplying the two arguments to the call to multiplier that created it and the argument in the call:
  new_function = multiplier(n1,n2)
  # where new_function(m) returns the result of n1 * n2 * m

Applying the multiplier of a number and its inverse from the two ordered collections of numbers in pairs, show that the result in each case is one.

'''Compare and contrast the resultant program with the corresponding entry in [[First-class functions]].''' They should be close.

<small>To paraphrase the task description: Do what was done before, but with numbers rather than functions</small>





## Ada


```Ada
with Ada.Text_IO;
procedure Firstclass is
   generic
      n1, n2 : Float;
   function Multiplier (m : Float) return Float;
   function Multiplier (m : Float) return Float is
   begin
      return n1 * n2 * m;
   end Multiplier;

   num, inv : array (1 .. 3) of Float;
begin
   num := (2.0, 4.0, 6.0);
   inv := (1.0/2.0, 1.0/4.0, 1.0/6.0);
   for i in num'Range loop
      declare
         function new_function is new Multiplier (num (i), inv (i));
      begin
         Ada.Text_IO.Put_Line (Float'Image (new_function (0.5)));
      end;
   end loop;
end Firstclass;
```

```txt
5.00000E-01
5.00000E-01
5.00000E-01
```



## ALGOL 68

Note: Standard ALGOL 68's scoping rules forbids exporting a '''proc'''[edure] (or '''format''') out of it's scope (closure).  Hence this specimen will run on [[ELLA ALGOL 68]], but is non-standard.  For a discussion of first-class functions in ALGOL 68 consult [http://www.cs.ru.nl/~kees/home/papers/psi96.pdf "The Making of Algol 68"] - [[wp:Cornelis_H.A._Koster|C.H.A. Koster]] (1993). <!-- Retrieved April 28, 2007 -->

```algol68
REAL
  x  := 2,
  xi := 0.5,
  y  := 4,
  yi := 0.25,
  z  := x + y,
  zi := 1 / ( x + y );

MODE F = PROC(REAL)REAL;

PROC multiplier = (REAL n1, n2)F: ((REAL m)REAL: n1 * n2 * m);

# Numbers as members of collections #
[]REAL num list = (x, y, z),
       inv num list = (xi, yi, zi);

# Apply numbers from list #
FOR key TO UPB num list DO
  REAL n = num list[key],
       inv n = inv num list[key];
  print ((multiplier(inv n, n)(.5), new line))
OD
```

Output:

```txt

+.500000000000000e +0
+.500000000000000e +0
+.500000000000000e +0

```


Comparing and contrasting with the First Class Functions example:

As noted above, in order to do what is required by this task and the First Class Functions task, extensions to Algol 68 must be used.

The First Class Functions example uses C. H. Lindsey's partial parameterization extension to Algol 68 which implemented in Algol 68G but not in algol68toc.
This example uses an alternative (technically, invalid Algol 68 as the author notes) accepted by algol68toc but not Algol 68G.


## Axiom


```Axiom
(x,xi,y,yi) := (2.0,0.5,4.0,0.25)
(z,zi) := (x+y,1/(x+y))
(numbers,invers) := ([x,y,z],[xi,yi,zi])
multiplier(a:Float,b:Float):(Float->Float) == (m +-> a*b*m)
[multiplier(number,inver) 0.5 for number in numbers for inver in invers]

```
Output:

```Axiom
  [0.5,0.5,0.5]
                          Type: List(Float)
```

We can also curry functions, possibly with function composition, with the same output as before:

```Axiom
mult(n:Float):(Float->Float) == curryLeft(*$Float,n)$MAPPKG3(Float,Float,Float)
[mult(number*inver) 0.5 for number in numbers for inver in invers]
[(mult(number)*mult(inver)) 0.5 for number in numbers for inver in invers]
```

Using the Spad code in [[First-class functions#Axiom]], this can be done more economically using:

```Axiom
(map(mult,numbers)*map(mult,invers)) 0.5
```

For comparison, [[First-class functions#Axiom]] gave:

```Axiom
fns := [sin$Float, cos$Float, (x:Float):Float +-> x^3]
inv := [asin$Float, acos$Float, (x:Float):Float +-> x^(1/3)]
[(f*g) 0.5 for f in fns for g in inv]

```

- which has the same output.


## BBC BASIC

```bbcbasic
      REM Create some numeric variables:
      x = 2 : xi = 1/2
      y = 4 : yi = 0.25
      z = x + y : zi = 1 / (x + y)

      REM Create the collections (here structures are used):
      DIM c{x, y, z}
      DIM ci{x, y, z}
      c.x = x : c.y = y : c.z = z
      ci.x = xi : ci.y = yi : ci.z = zi

      REM Create some multiplier functions:
      multx = FNmultiplier(c.x, ci.x)
      multy = FNmultiplier(c.y, ci.y)
      multz = FNmultiplier(c.z, ci.z)

      REM Test applying the compositions:
      x = 1.234567 : PRINT x " ", FN(multx)(x)
      x = 2.345678 : PRINT x " ", FN(multy)(x)
      x = 3.456789 : PRINT x " ", FN(multz)(x)
      END

      DEF FNmultiplier(n1,n2)
      LOCAL f$, p%
      f$ = "(m)=" + STR$n1 + "*" + STR$n2 + "*m"
      DIM p% LEN(f$) + 4
      $(p%+4) = f$ : !p% = p%+4
      = p%
```

'''Output:'''

```txt

  1.234567            1.234567
  2.345678            2.345678
  3.456789          3.45678901

```

Compare with the implementation of First-class functions:

```bbcbasic
      REM Create some functions and their inverses:
      DEF FNsin(a) = SIN(a)
      DEF FNasn(a) = ASN(a)
      DEF FNcos(a) = COS(a)
      DEF FNacs(a) = ACS(a)
      DEF FNcube(a) = a^3
      DEF FNroot(a) = a^(1/3)

      dummy = FNsin(1)

      REM Create the collections (here structures are used):
      DIM cA{Sin%, Cos%, Cube%}
      DIM cB{Asn%, Acs%, Root%}
      cA.Sin% = ^FNsin() : cA.Cos% = ^FNcos() : cA.Cube% = ^FNcube()
      cB.Asn% = ^FNasn() : cB.Acs% = ^FNacs() : cB.Root% = ^FNroot()

      REM Create some function compositions:
      AsnSin% = FNcompose(cB.Asn%, cA.Sin%)
      AcsCos% = FNcompose(cB.Acs%, cA.Cos%)
      RootCube% = FNcompose(cB.Root%, cA.Cube%)

      REM Test applying the compositions:
      x = 1.234567 : PRINT x, FN(AsnSin%)(x)
      x = 2.345678 : PRINT x, FN(AcsCos%)(x)
      x = 3.456789 : PRINT x, FN(RootCube%)(x)
      END

      DEF FNcompose(f%,g%)
      LOCAL f$, p%
      f$ = "(x)=" + CHR$&A4 + "(&" + STR$~f% + ")(" + \
      \             CHR$&A4 + "(&" + STR$~g% + ")(x))"
      DIM p% LEN(f$) + 4
      $(p%+4) = f$ : !p% = p%+4
      = p%
```


## C#
The structure here is exactly the same as the C# entry in [[First-class functions]]. The "var" keyword allows us to use the same initialization code for an array of doubles as an array of functions. Note that variable names have been changed to correspond with the new functionality.

```c#
using System;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        double x, xi, y, yi, z, zi;
        x = 2.0;
        xi = 0.5;
        y = 4.0;
        yi = 0.25;
        z = x + y;
        zi = 1.0 / (x + y);

        var numlist = new[] { x, y, z };
        var numlisti = new[] { xi, yi, zi };
        var multiplied = numlist.Zip(numlisti, (n1, n2) =>
                       {
                           Func<double, double> multiplier = m => n1 * n2 * m;
                           return multiplier;
                       });

        foreach (var multiplier in multiplied)
            Console.WriteLine(multiplier(0.5));
    }
}

```



## Clojure


```clojure
(def x  2.0)
(def xi 0.5)
(def y  4.0)
(def yi 0.25)
(def z  (+ x y))
(def zi (/ 1.0 (+ x y)))

(def numbers [x y z])
(def invers  [xi yi zi])

(defn multiplier [a b]
      (fn [m] (* a b m)))

> (for [[n i] (zipmap numbers invers)]
       ((multiplier n i) 0.5))
(0.5 0.5 0.5)
```

For comparison:

```clojure

(use 'clojure.contrib.math)
(let [fns [#(Math/sin %) #(Math/cos %) (fn [x] (* x x x))]
      inv [#(Math/asin %) #(Math/acos %) #(expt % 1/3)]]
  (map #(% 0.5) (map #(comp %1 %2) fns inv)))

```

Output:

```txt
(0.5 0.4999999999999999 0.5000000000000001)
```



## Common Lisp



```lisp
(defun multiplier (f g)
  #'(lambda (x) (* f g x)))

(let* ((x 2.0)
       (xi 0.5)
       (y 4.0)
       (yi 0.25)
       (z (+ x y))
       (zi (/ 1.0 (+ x y)))
       (numbers (list x y z))
       (inverses (list xi yi zi)))
  (loop with value = 0.5
        for number in numbers
        for inverse in inverses
        for multiplier = (multiplier number inverse)
        do (format t "~&(~A * ~A)(~A) = ~A~%"
                   number
                   inverse
                   value
                   (funcall multiplier value))))
```


Output:

 (2.0 * 0.5)(0.5) = 0.5
 (4.0 * 0.25)(0.5) = 0.5
 (6.0 * 0.16666667)(0.5) = 0.5

The code from [[First-class functions]], for comparison:


```lisp
(defun compose (f g) (lambda (x) (funcall f (funcall g x))))
(defun cube (x) (expt x 3))
(defun cube-root (x) (expt x (/ 3)))

(loop with value = 0.5
      for function in (list #'sin  #'cos  #'cube     )
      for inverse  in (list #'asin #'acos #'cube-root)
      for composed = (compose inverse function)
      do (format t "~&(~A ∘ ~A)(~A) = ~A~%"
                 inverse
                 function
                 value
                 (funcall composed value)))
```


Output:

 (#<FUNCTION ASIN> ∘ #<FUNCTION SIN>)(0.5) = 0.5
 (#<FUNCTION ACOS> ∘ #<FUNCTION COS>)(0.5) = 0.5
 (#<FUNCTION CUBE-ROOT> ∘ #<FUNCTION CUBE>)(0.5) = 0.5


## D


```d
import std.stdio;

auto multiplier(double a, double b)
{
    return (double c) => a * b * c;
}

void main()
{
    double x = 2.0;
    double xi = 0.5;
    double y = 4.0;
    double yi = 0.25;
    double z = x + y;
    double zi = 1.0 / (z);

    double[3] f = [x, y, z];
    double[3] r = [xi, yi, zi];

    foreach (i; 0..3)
    {
        auto mult = multiplier(f[i], r[i]);
        writefln("%f * %f * %f == %f", f[i], r[i], 1.0, mult(1));
    }
}
```

Output:

```txt
2.000000 * 0.500000 * 1.000000 == 1.000000
4.000000 * 0.250000 * 1.000000 == 1.000000
6.000000 * 0.166667 * 1.000000 == 1.000000
```



## E


This is written to have identical structure to [[First-class functions#E]], though the variable names are different.


```e
def x := 2.0
def xi := 0.5
def y := 4.0
def yi := 0.25
def z := x + y
def zi := 1.0 / (x + y)
def forward := [x,  y,  z ]
def reverse := [xi, yi, zi]

def multiplier(a, b) {
    return fn x { a * b * x }
}

def s := 0.5
for i => a in forward {
    def b := reverse[i]
    println(`s = $s, a = $a, b = $b, multiplier($a, $b)($s) = ${multiplier(a, b)(s)}`)
}
```


Output:

 s = 0.5, a = 2.0, b = 0.5, multiplier(2.0, 0.5)(0.5) = 0.5
 s = 0.5, a = 4.0, b = 0.25, multiplier(4.0, 0.25)(0.5) = 0.5
 s = 0.5, a = 6.0, b = 0.16666666666666666, multiplier(6.0, 0.16666666666666666)(0.5) = 0.5

Note: <code>def g := reverse[i]</code> is needed here because E as yet has no defined protocol for iterating over collections in parallel. [http://wiki.erights.org/wiki/Parallel_iteration Page for this issue.]

## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

public program()
{
    real x := 2.0r;
    real xi := 0.5r;
    real y := 4.0r;
    real yi := 0.25r;
    real z := x + y;
    real zi := 1.0r / (x + y);

    var numlist := new real[]::(  x, y, z );
    var numlisti := new real[]::( xi, yi, zi );

    var multiplied := numlist.zipBy(numlisti, (n1,n2 => (m => n1 * n2 * m) )).toArray();

    multiplied.forEach:(multiplier){ console.printLine(multiplier(0.5r)) }
}
```

```txt

0.5
0.5
0.5

```



## Erlang


```Erlang

-module( first_class_functions_use_numbers ).

-export( [task/0] ).

task() ->
	X = 2.0, Xi = 0.5, Y = 4.0, Yi = 0.25, Z = X + Y, Zi = 1.0 / (X + Y),
	As = [X, Y, Z],
	Bs = [Xi, Yi, Zi],
	[io:fwrite( "Value: 2.5 Result: ~p~n", [(multiplier(A, B))(2.5)]) || {A, B} <- lists:zip(As, Bs)].



multiplier( N1, N2 ) -> fun(M) -> N1 * N2 * M end.

```

```txt

20> first_class_functions_use_numbers:task().
Value: 2.5 Result: 2.5
Value: 2.5 Result: 2.5
Value: 2.5 Result: 2.5

```



## F#


```fsharp

let x = 2.0
let xi = 0.5
let y = 4.0
let yi = 0.25
let z = x + y
let zi = 1.0 / ( x + y )
let multiplier (n1,n2) = fun (m:float) -> n1 * n2 * m

[x; y; z]
|> List.zip [xi; yi; zi]
|> List.map multiplier
|> List.map ((|>) 0.5)
|> printfn "%A"

```

```txt

[0.5; 0.5; 0.5]

```



## Factor

Compared to http://rosettacode.org/wiki/First-class_functions, the call to "compose" is replaced with the call to "mutliplier"

```factor
USING: arrays kernel literals math prettyprint sequences ;
IN: q

CONSTANT: x  2.0
CONSTANT: xi 0.5
CONSTANT: y  4.0
CONSTANT: yi .25
CONSTANT: z $[ $ x $ y + ]
CONSTANT: zi $[ 1 $ x $ y + / ]

CONSTANT: A ${ x y z }
CONSTANT: B ${ xi yi zi }

: multiplier ( n1 n2 -- q ) [ * * ] 2curry ;
: create-all ( seq1 seq2 -- seq ) [ multiplier ] 2map ;
: example  ( -- )
  0.5 A B create-all
  [ call( x -- y ) ] with map . ;
```

```txt
{ 0.5 0.5 0.5 }
```



## Fantom



```fantom

class Main
{
  static |Float -> Float| combine (Float n1, Float n2)
  {
    return |Float m -> Float| { n1 * n2 * m }
  }

  public static Void main ()
  {
    Float x := 2f
    Float xi := 0.5f
    Float y := 4f
    Float yi := 0.25f
    Float z := x + y
    Float zi := 1 / (x + y)
    echo (combine(x, xi)(0.5f))
    echo (combine(y, yi)(0.5f))
    echo (combine(z, zi)(0.5f))
  }
}

```


The <code>combine</code> function is very similar to the <code>compose</code> function in 'First-class functions'.  In both cases a new function is returned:


```fantom

  static |Obj -> Obj| compose (|Obj -> Obj| fn1, |Obj -> Obj| fn2)
  {
    return |Obj x -> Obj| { fn2 (fn1 (x)) }
  }

```



## Go

==="Number means value"===
Task interpretation 1:  "Number" means a numeric value, not any sort of reference.  This is the most natural interpretation in Go.

At point A, the six variables have been assigned values, 64 bit floating point numbers, and not references to anything that is evaluated later.  Again, at point B, these values have been ''copied'' into the array elements.  (The arrays being "ordered collections.")  The original six variables could be changed at this point and the array values would stay the same.

Multiplier multiplies pairs of values and binds the result to the returned closure.  This might be considered a difference from the First-class functions task.  In that task, the functions were composed into a new function but not evaluated at the time of composition.  They ''could'' have been, but that is not the usual meaning of function composition.

This task however, works with numbers, which are not reference types.  Specifically, the closure here could could have closed on n1 an n2 individually and delayed multiplication until closure evaluation, but that might seem inconsistent with the task interpretation of working with numbers as values.  Also, one would expect that a function named "multiplier" does actually multiply.

Multiplier of this task and compose of the First-class function task are similar in that they both return first class function objects, which are closed on free variables.  The free variables in compose held the two (unevaluated) functions being composed.  The free variable in multiplier holds the ''result'' of multiplication.

At point C, numbers and their inverses have been multiplied and bound to first class functions.  The ordered collection arrays could be modified at this point and the function objects would be unaffected.


```go
package main

import "fmt"

func main() {
    x := 2.
    xi := .5
    y := 4.
    yi := .25
    z := x + y
    zi := 1 / (x + y)
    // point A

    numbers := []float64{x, y, z}
    inverses := []float64{xi, yi, zi}
    // point B

    mfs := make([]func(float64) float64, len(numbers))
    for i := range mfs {
        mfs[i] = multiplier(numbers[i], inverses[i])
    }
    // point C

    for _, mf := range mfs {
        fmt.Println(mf(1))
    }
}

func multiplier(n1, n2 float64) func(float64) float64 {
    // compute product of n's, store in a new variable
    n1n2 := n1 * n2
    // close on variable containing product
    return func(m float64) float64 {
        return n1n2 * m
    }
}
```

Output:

```txt

1
1
1

```

==="Number means reference"===
Task interpretation 2:  "Number" means something abstract, evaluation of which is delayed as long as possible.  This interpretation is suggested by the task wording that this program and and the corresponding First-class functions program "should be close" and "do what was done before...."

To implement this behavior, reference types are used for "numbers" and a polymorphic array is used for the ordered collection, allowing both static and computed objects to stored.

At point A, the variables z and zi are assigned function literals, first class function objects closed on x and y.  Changing the values of x or y at this point will cause z and zi to return different results.  The x and y in these function literals reference the same storage as the x and y assigned values 2 and 4.  This is more like the the First-class functions task in that we now have functions which we can compose.

At point B, we have filled the polymorphic arrays with all reference types.  References to numeric typed variables x, xi, y, and yi were created with the & operator.  z and zi can already be considered reference types in that they reference x and y.  Changes to any of x, xi, y, or yi at this point would still affect later results.

Multiplier, in this interpretation of the task, simply composes multiplication of three reference objects, which may be variables or functions.  It does not actually multiply and does not even evaluate the three objects.  This is very much like the compose function of the First-class functions task.

Pursuant to the task description, this version of multiplier "returns the result of n1 * n2 * m" in the sense that it sets up evaluation of n1, n2, and m to be done at the same time, even if that time is not quite yet.

At point C, changes to x, xi, y, and yi will still propagate through and affect the results returned by the mfs objects.  This can be seen as like the First-class functions task in that nothing (nothing numberic anyway) is evaluated until the final composed objects are evaluated.

[[/Go interface type|This can also be done with an interface type]] rather than the "empty interface" (<code>interface{}</code>) for better type safety and to avoid the <code>eval</code> function and type switch.

```go
package main

import "fmt"

func main() {
    x := 2.
    xi := .5
    y := 4.
    yi := .25
    z := func() float64 { return x + y }
    zi := func() float64 { return 1 / (x + y) }
    // point A

    numbers := []interface{}{&x, &y, z}
    inverses := []interface{}{&xi, &yi, zi}
    // point B

    mfs := make([]func(n interface{}) float64, len(numbers))
    for i := range mfs {
        mfs[i] = multiplier(numbers[i], inverses[i])
    }
    // pointC

    for _, mf := range mfs {
        fmt.Println(mf(1.))
    }
}

func multiplier(n1, n2 interface{}) func(interface{}) float64 {
    return func(m interface{}) float64 {
        // close on interface objects n1, n2, and m
        return eval(n1) * eval(n2) * eval(m)
    }
}

// utility function for evaluating multiplier interface objects
func eval(n interface{}) float64 {
    switch n.(type) {
    case float64:
        return n.(float64)
    case *float64:
        return *n.(*float64)
    case func() float64:
        return n.(func() float64)()
    }
    panic("unsupported multiplier type")
    return 0 // never reached
}
```



## Groovy



```groovy
def multiplier = { n1, n2 -> { m -> n1 * n2 * m } }

def ε = 0.00000001  // tolerance(epsilon): acceptable level of "wrongness" to account for rounding error
[(2.0):0.5, (4.0):0.25, (6.0):(1/6.0)].each { num, inv ->
    def new_function = multiplier(num, inv)
    (1.0..5.0).each { trial ->
        assert (new_function(trial) - trial).abs() < ε
        printf('%5.3f * %5.3f * %5.3f == %5.3f\n', num, inv, trial, trial)
    }
    println()
}
```

```txt
2.000 * 0.500 * 1.000 == 1.000
2.000 * 0.500 * 2.000 == 2.000
2.000 * 0.500 * 3.000 == 3.000
2.000 * 0.500 * 4.000 == 4.000
2.000 * 0.500 * 5.000 == 5.000

4.000 * 0.250 * 1.000 == 1.000
4.000 * 0.250 * 2.000 == 2.000
4.000 * 0.250 * 3.000 == 3.000
4.000 * 0.250 * 4.000 == 4.000
4.000 * 0.250 * 5.000 == 5.000

6.000 * 0.167 * 1.000 == 1.000
6.000 * 0.167 * 2.000 == 2.000
6.000 * 0.167 * 3.000 == 3.000
6.000 * 0.167 * 4.000 == 4.000
6.000 * 0.167 * 5.000 == 5.000
```



## Haskell



```haskell
module Main
  where

import Text.Printf

-- Pseudo code happens to be valid Haskell
x  = 2.0
xi = 0.5
y  = 4.0
yi = 0.25
z  = x + y
zi = 1.0 / ( x + y )

-- Multiplier function
multiplier :: Double -> Double -> Double -> Double
multiplier a b = \m -> a * b * m

main :: IO ()
main = do
  let
    numbers = [x, y, z]
    inverses = [xi, yi, zi]
    pairs = zip numbers inverses
    print_pair (number, inverse) =
      let new_function = multiplier number inverse
      in printf "%f * %f * 0.5 = %f\n" number inverse (new_function 0.5)
  mapM_ print_pair pairs

```


This is very close to the first-class functions example, but given as a full Haskell program rather than an interactive session.

=={{header|Icon}} and {{header|Unicon}}==

The following is a Unicon solution.  It can be recast in Icon, but only at the cost
of losing the "function-call" syntax on the created "procedure".  The solution uses
the same basic foundation that is buried in the "compose" procedure in the
[[First-class functions]] task solution.  The solution here
is simpler and more direct since it handles a specific function definition.

```unicon
import Utils

procedure main(A)
    mult := multiplier(get(A),get(A))   # first 2 args define function
    every write(mult(!A))     # remaining are passed to new function
end

procedure multiplier(n1,n2)
    return makeProc { repeat inVal := n1 * n2 * (inVal@&source)[1] }
end
```


A sample run:

```txt
->mu 2 3 4 5 6 7
24
30
36
42
->
```



## J



### Explicit version


This seems to satisfy the new problem statement:


```j
   x          =:  2.0
   xi         =:  0.5
   y          =:  4.0
   yi         =:  0.25
   z          =:  x + y
   zi         =:  1.0 % (x + y)  NB. / is spelled % in J

   fwd        =:  x ,y ,z
   rev        =:  xi,yi,zi

   multiplier =:  2 : 'm * n * ]'
```


Example use:

<lang>   fwd multiplier rev 0.5
0.5 0.5 0.5
```


For contrast, here are the final results from  [[First-class functions#J]]:
<lang>   BA unqcol 0.5
0.5 0.5 0.5 0.5
```


===Tacit (unorthodox) version===
Although the pseudo-code to generate the numbers can certainly be written (see  above [http://rosettacode.org/wiki/First-class_functions/Use_numbers_analogously#Explicit_version Explicit version] ) this is not done for this version because it would destroy part of the analogy (J encourages, from the programming perspective, to process all components at once as opposed to one component at a time).  In addition, this version is done in terms of boxed lists of numbers instead of plain list of numbers, again, to preserve the analogy.

<lang>   multiplier=. train@:((;:'&*') ;~ an@: *)

   ]A=. 2  ; 4  ; (2 + 4)   NB. Corresponds to  ]A=. box (1&o.)`(2&o.)`(^&3)
┌─┬─┬─┐
│2│4│6│
└─┴─┴─┘
   ]B=. %&.> A              NB. Corresponds to  ]B =. inverse&.> A
┌───┬────┬────────┐
│0.5│0.25│0.166667│
└───┴────┴────────┘
   ]BA=. B multiplier&.> A  NB. Corresponds to  B compose&.> A
┌───┬───┬───┐
│1&*│1&*│1&*│
└───┴───┴───┘
   BA of &> 0.5             NB. Corresponds to  BA of &> 0.5  (exactly)
0.5 0.5 0.5
```


Please refer to [http://rosettacode.org/wiki/First-class_functions#Tacit_.28unorthodox.29_version First-class functions tacit (unorthodox) version] for the definitions of the functions train, an and of.


## jq

It may be helpful to compare the following definition of "multiplier" with its Ruby counterpart [[#Ruby|below]]. Whereas the Ruby definition must name all its positional parameters, the jq equivalent is defined as a filter that obtains them implicitly from its input.

```jq
# Infrastructure:
# zip this and that
def zip(that):
  . as $this | reduce range(0;length) as $i ([]; . + [ [$this[$i], that[$i]] ]);

# The task:
def x:  2.0;
def xi: 0.5;
def y:  4.0;
def yi: 0.25;
def z:  x + y;
def zi: 1.0 / (x + y);

def numlist: [x,y,z];

def invlist: [xi, yi, zi];

# Input: [x,y]
def multiplier(j): .[0] * .[1] * j;

numlist|zip(invlist) | map( multiplier(0.5) )
```

 $ jq -n -c -f First_class_functions_Use_numbers_analogously.jq
 [0.5,0.5,0.5]

As of this writing, there is no entry for jq at [[First-class functions]].


## Julia

In Julia, like Python and R, functions can be treated as like as other Types.


```julia
x, xi  = 2.0, 0.5
y, yi  = 4.0, 0.25
z, zi  = x + y, 1.0 / ( x + y )

multiplier = (n1, n2) -> (m) -> n1 * n2 * m

numlist  = [x ,  y,  z]
numlisti = [xi, yi, zi]

@show collect(multiplier(n, invn)(0.5) for (n, invn) in zip(numlist, numlisti))
```


```txt
collect(((multiplier(n, invn))(0.5) for (n, invn) = zip(numlist, numlisti))) = [0.5, 0.5, 0.5]
```



## Kotlin


```scala
// version 1.1.2

fun multiplier(n1: Double, n2: Double) = { m: Double -> n1 * n2 * m}

fun main(args: Array<String>) {
    val x  = 2.0
    val xi = 0.5
    val y  = 4.0
    val yi = 0.25
    val z  = x + y
    val zi = 1.0 / ( x + y)
    val a  = doubleArrayOf(x, y, z)
    val ai = doubleArrayOf(xi, yi, zi)
    val m  = 0.5
    for (i in 0 until a.size) {
        println("${multiplier(a[i], ai[i])(m)} = multiplier(${a[i]}, ${ai[i]})($m)")
    }
}
```


```txt

0.5 = multiplier(2.0, 0.5)(0.5)
0.5 = multiplier(4.0, 0.25)(0.5)
0.5 = multiplier(6.0, 0.16666666666666666)(0.5)

```



## Lua


```Lua

-- This function returns another function that
-- keeps n1 and n2 in scope, ie. a closure.
function multiplier (n1, n2)
    return  function (m)
                return n1 * n2 * m
            end
end

-- Multiple assignment a-go-go
local x, xi, y, yi  = 2.0, 0.5, 4.0, 0.25
local z, zi = x + y, 1.0 / ( x + y )
local nums, invs = {x, y, z}, {xi, yi, zi}

-- 'new_function' stores the closure and then has the 0.5 applied to it
-- (this 0.5 isn't in the task description but everyone else used it)
for k, v in pairs(nums) do
    new_function = multiplier(v, invs[k])
    print(v .. " * " .. invs[k] .. " * 0.5 = " .. new_function(0.5))
end

```

```txt

2 * 0.5 * 0.5 = 0.5
4 * 0.25 * 0.5 = 0.5
6 * 0.16666666666667 * 0.5 = 0.5

```



## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      \\ by default numbers are double
      x  = 2
      xi = 0.5
      y  = 4
      yi = 0.25
      z  = x + y
      zi = 1 / ( x + y )
      Composed=lambda (a, b)-> {
            =lambda a,b (n)->{
                  =a*b*n
            }
       }
      numbers=(x,y,z)
      inverses=(xi,yi,zi)
      Dim Base 0, combo(3)
      combo(0)=Composed(x,xi), Composed(y,yi), Composed(z,zi)
      num=each(numbers)
      inv=each(inverses)
      fun=each(combo())
      While num, inv, fun {
         Print $("0.00"), Array(num);" * ";Array(inv);" * 0.50 = "; combo(fun^)(0.5),$("")
         Print
      }
}
Checkit
\\ for functions  we have this definition
Composed=lambda (f1, f2) -> {
      =lambda f1, f2 (x)->{
            =f1(f2(x))
      }
}


```

```txt

2.00 * 0.50 * 0.50 = 0.50
4.00 * 0.25 * 0.50 = 0.50
6.00 * 0.17 * 0.50 = 0.50

```



=={{header|Mathematica}} / {{header|Wolfram Language}}==

This code demonstrates the example using structure similar to function composition, however the composition function is replace with the multiplier function.


```Mathematica
multiplier[n1_,n2_]:=n1 n2 #&
num={2,4,2+4};
numi=1/num;
multiplierfuncs = multiplier @@@ Transpose[{num, numi}];

```


The resulting functions are unity multipliers:

```txt
Table[i[0.666], {i, multiplierfuncs}]

{0.666, 0.666, 0.666}
```


Note that unlike Composition, the above definition of multiplier only allows for exactly two arguments. The definition can be changed to allow any nonzero number of arguments:

```Mathematica
multiplier[arg__] := Times[arg, #] &

```



## Nemerle

```Nemerle
using System;
using System.Console;
using Nemerle.Collections.NCollectionsExtensions;

module FirstClassNums
{
    Main() : void
    {
        def x = 2.0;   def xi = 0.5;
        def y = 4.0;   def yi = 0.25;
        def z = x + y; def zi = 1.0 / (x + y);
        def multiplier = fun (a, b) {fun (c) {a * b * c}};
        def nums = [x, y, z];
        def inums = [xi, yi, zi];
        WriteLine($[multiplier(n, m) (0.5)|(n, m) in ZipLazy(nums, inums)]);
    }
}
```



## Never


```Never

func multiplier(a : float, b : float) -> (float) -> float {
    let func(m : float) -> float { a * b * m }
}

func main() -> int {
    var x = 2.0;
    var xi = 0.5;
    var y = 4.0;
    var yi = 0.25;
    var z = x + y;
    var zi = 1.0 / z;

    var f = [ x, y, z ] : float;
    var i = [ xi, yi, zi ] : float;
    var c = 0;
    var mult = let func(m : float) -> float { 0.0 };

    for (c = 0; c < 3; c = c + 1) {
        mult = multiplier(f[c], i[c]);
        prints(f[c] + " * " + i[c] + " * " + 1.0 + " = " + mult(1) + "\n")
    };

    0
}

```

```txt

2.00 * 0.50 * 1.00 = 1.00
4.00 * 0.25 * 1.00 = 1.00
6.00 * 0.17 * 1.00 = 1.00

```



## Objeck

Similar however this code does not generate a list of functions.

```objeck
use Collection.Generic;

class FirstClass {
  function : Main(args : String[]) ~ Nil {
    x := 2.0;
    xi := 0.5;
    y := 4.0;
    yi := 0.25;
    z := x + y;
    zi := 1.0 / (x + y);

    numlist := CompareVector->New()<FloatHolder>;
    numlist->AddBack(x); numlist->AddBack(y); numlist->AddBack(z);

    numlisti := Vector->New()<FloatHolder>;
    numlisti->AddBack(xi); numlisti->AddBack(yi); numlisti->AddBack(zi);

    each(i : numlist) {
      v := numlist->Get(i); vi := numlisti->Get(i);
      mult := Multiplier(v, vi);
      r := mult(0.5);
      "{$v} * {$vi} * 0.5 = {$r}"->PrintLine();
    };
  }

  function : Multiplier(a : FloatHolder, b : FloatHolder) ~ (FloatHolder) ~ FloatHolder {
    return \(FloatHolder) ~ FloatHolder : (c) =>  a * b * c;
  }
}

```


```txt

2 * 0.5 * 0.5 = 0.5
4 * 0.25 * 0.5 = 0.5
6 * 0.166667 * 0.5 = 0.5

```



## OCaml



```ocaml
# let x = 2.0;;

# let y = 4.0;;

# let z = x +. y;;

# let coll = [ x; y; z];;

# let inv_coll = List.map (fun x -> 1.0 /. x) coll;;

# let multiplier n1 n2 = (fun t -> n1 *. n2 *. t);;

(* create a list of new functions *)
# let func_list = List.map2 (fun n inv -> (multiplier n inv)) coll inv_coll;;

# List.map (fun f -> f 0.5) func_list;;
- : float list = [0.5; 0.5; 0.5]

(* or just apply the generated function immediately... *)
# List.map2 (fun n inv -> (multiplier n inv) 0.5) coll inv_coll;;
- : float list = [0.5; 0.5; 0.5]
```



## Oforth



```Oforth
: multiplier(n1, n2)  #[ n1 n2 * * ] ;

: firstClassNum
| x xi y yi z zi |
   2.0 ->x
   0.5 ->xi
   4.0 ->y
   0.25 ->yi
   x y + ->z
   x y + inv ->zi
   [ x, y, z ] [ xi, yi, zi ] zipWith(#multiplier) map(#[ 0.5 swap perform ] ) . ;
```

```txt

[0.5, 0.5, 0.5]

```



## Oz


```oz
declare

  [X Y Z] = [2.0  4.0  Z=X+Y]
  [XI YI ZI] = [0.5  0.25  1.0/(X+Y)]

  fun {Multiplier A B}
     fun {$ M}
        A * B * M
     end
  end

in

  for
     N in [X  Y  Z]
     I in [XI YI ZI]
  do
     {Show {{Multiplier N I} 0.5}}
  end
```


"Multiplier" is like "Compose", but with multiplication instead of function application. Otherwise the code is identical except for the argument types (numbers instead of functions).


## PARI/GP

```parigp
multiplier(n1,n2)={
  x -> n1 * n2 * x
};

test()={
  my(x = 2.0, xi = 0.5, y = 4.0, yi = 0.25, z = x + y, zi = 1.0 / ( x + y ));
  print(multiplier(x,xi)(0.5));
  print(multiplier(y,yi)(0.5));
  print(multiplier(z,zi)(0.5));
};
```

The two are very similar, though as requested the test numbers are in 6 variables instead of two vectors.


## Perl


```perl
sub multiplier {
    my ( $n1, $n2 ) = @_;
    sub {
        $n1 * $n2 * $_[0];
    };
}

my $x  = 2.0;
my $xi = 0.5;
my $y  = 4.0;
my $yi = 0.25;
my $z  = $x + $y;
my $zi = 1.0 / ( $x + $y );

my %zip;
@zip{ $x, $y, $z } = ( $xi, $yi, $zi );

while ( my ( $number, $inverse ) = each %zip ) {
    print multiplier( $number, $inverse )->(0.5), "\n";
}

```

Output:

```txt
0.5
0.5
0.5

```

The entry in first-class functions uses the same technique:

```perl
sub compose {
    my ($f, $g) = @_;

    sub {
        $f -> ($g -> (@_));
    };
}
...
compose($flist1[$_], $flist2[$_]) -> (0.5)

```



## Perl 6

```perl6
sub multiplied ($g, $f) { return { $g * $f * $^x } }

my $x  = 2.0;
my $xi = 0.5;
my $y  = 4.0;
my $yi = 0.25;
my $z  = $x + $y;
my $zi = 1.0 / ( $x + $y );

my @numbers = $x, $y, $z;
my @inverses = $xi, $yi, $zi;

for flat @numbers Z @inverses { say multiplied($^g, $^f)(.5) }
```

Output:

```txt
0.5
0.5
0.5
```

The structure of this is identical to first-class function task.


## Phix

Just as there is no real support for first class functions, not much that is pertinent to this task for numbers either, but the manual way is just as trivial:

```Phix
sequence mtable = {}

function multiplier(atom n1, atom n2)
    mtable = append(mtable,{n1,n2})
    return length(mtable)
end function

function call_multiplier(integer f, atom m)
atom {n1,n2} = mtable[f]
    return n1*n2*m
end function

constant x = 2,
         xi = 0.5,
         y  = 4,
         yi = 0.25,
         z  = x + y,
         zi = 1 / ( x + y )

?call_multiplier(multiplier(x,xi),0.5)
?call_multiplier(multiplier(y,yi),0.5)
?call_multiplier(multiplier(z,zi),0.5)
```

```txt

0.5
0.5
0.5

```

I should perhaps note that output in Phix automatically rounds to the specified precision (10 d.p. if none) so 4.9999 to two decimal places is shown as 5.00, and you can be pretty sure that sort of thing is happening on the last line.

Compared to first class functions, there are (as in my view there should be) significant differences in the treatment of numbers and functions, but as mentioned on that page tagging ctable entries should be quite sufficient.

```Phix
sequence ctable = {}

function compose(integer f, integer g)
    ctable = append(ctable,{f,g})
    return length(ctable)
end function

function call_composite(integer f, atom x)
integer g
    {f,g} = ctable[f]
    return call_func(f,{call_func(g,{x})})
end function

function plus1(atom x)
    return x+1
end function

function halve(atom x)
    return x/2
end function

constant m = compose(routine_id("halve"),routine_id("plus1"))

?call_composite(m,1)    -- displays 1
?call_composite(m,4)    -- displays 2.5
```



## PicoLisp


```PicoLisp
(load "@lib/math.l")

(de multiplier (N1 N2)
   (curry (N1 N2) (X)
      (*/ N1 N2 X `(* 1.0 1.0)) ) )

(let (X 2.0  Xi 0.5  Y 4.0  Yi 0.25  Z (+ X Y)  Zi (*/ 1.0 1.0 Z))
   (mapc
      '((Num Inv)
         (prinl (format ((multiplier Inv Num) 0.5) *Scl)) )
      (list X Y Z)
      (list Xi Yi Zi) ) )
```

Output:

```txt
0.500000
0.500000
0.500001
```

This follows the same structure as [[First-class functions#PicoLisp]], just
that the function 'multiplier' above accepts two numbers, while 'compose'
below accepts two functions:

```PicoLisp
(load "@lib/math.l")

(de compose (F G)
   (curry (F G) (X)
      (F (G X)) ) )

(de cube (X)
   (pow X 3.0) )

(de cubeRoot (X)
   (pow X 0.3333333) )

(mapc
   '((Fun Inv)
      (prinl (format ((compose Inv Fun) 0.5) *Scl)) )
   '(sin  cos  cube)
   '(asin acos cubeRoot) )
```

With a similar output:

```txt
0.500001
0.499999
0.500000
```



## Python

This new task:

```python
IDLE 2.6.1
>>> # Number literals
>>> x,xi, y,yi = 2.0,0.5, 4.0,0.25
>>> # Numbers from calculation
>>> z  = x + y
>>> zi = 1.0 / (x + y)
>>> # The multiplier function is similar to 'compose' but with numbers
>>> multiplier = lambda n1, n2: (lambda m: n1 * n2 * m)
>>> # Numbers as members of collections
>>> numlist = [x, y, z]
>>> numlisti = [xi, yi, zi]
>>> # Apply numbers from list
>>> [multiplier(inversen, n)(.5) for n, inversen in zip(numlist, numlisti)]
[0.5, 0.5, 0.5]
>>>
```


The Python solution to First-class functions for comparison:

```python
>>
 # Some built in functions and their inverses
>>> from math import sin, cos, acos, asin
>>> # Add a user defined function and its inverse
>>> cube = lambda x: x * x * x
>>> croot = lambda x: x ** (1/3.0)
>>> # First class functions allow run-time creation of functions from functions
>>> # return function compose(f,g)(x) == f(g(x))
>>> compose = lambda f1, f2: ( lambda x: f1(f2(x)) )
>>> # first class functions should be able to be members of collection types
>>> funclist = [sin, cos, cube]
>>> funclisti = [asin, acos, croot]
>>> # Apply functions from lists as easily as integers
>>> [compose(inversef, f)(.5) for f, inversef in zip(funclist, funclisti)]
[0.5, 0.4999999999999999, 0.5]
>>>
```

As can be see, the treatment of functions is very close to the treatment of numbers. there are no extra wrappers, or function pointer syntax added, for example.


## R


```R
multiplier <- function(n1,n2) { (function(m){n1*n2*m}) }
x  = 2.0
xi = 0.5
y  = 4.0
yi = 0.25
z  = x + y
zi = 1.0 / ( x + y )
num = c(x,y,z)
inv = c(xi,yi,zi)

multiplier(num,inv)(0.5)

Output
[1] 0.5 0.5 0.5

```


Compared to original first class functions

```R
sapply(mapply(compose,f1,f2),do.call,list(.5))
[1] 0.5 0.5 0.5
```



## Racket



```Racket

#lang racket

(define x  2.0)
(define xi 0.5)
(define y  4.0)
(define yi 0.25)
(define z  (+ x y))
(define zi (/ 1.0 (+ x y)))

(define ((multiplier x y) z) (* x y z))

(define numbers  (list x  y  z))
(define inverses (list xi yi zi))

(for/list ([n numbers] [i inverses])
  ((multiplier n i) 0.5))
;; -> '(0.5 0.5 0.5)

```



## REXX

The REXX language doesn't have an easy method to call functions by using a variable name,

but the '''interpret''' instruction can be used to provide that capability.

```rexx
/*REXX program to use a  first-class function  to  use numbers analogously.             */
nums=   2.0     4.0      6.0                     /*various numbers,  can have fractions.*/
invs= 1/2.0   1/4.0    1/6.0                     /*inverses of the above (real) numbers.*/
   m=   0.5                                      /*multiplier when invoking new function*/
             do j=1  for words(nums);   num= word(nums, j);  inv= word(invs, j)
             nf= multiplier(num, inv);  interpret call nf m       /*sets the var RESULT.*/
             say 'number=' @(num)    'inverse=' @(inv)    'm=' @(m)    'result=' @(result)
             end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
@:          return left( arg(1) / 1, 15)         /*format the number,  left justified.  */
multiplier: procedure expose n1n2; parse arg n1,n2;   n1n2= n1 * n2;   return 'a_new_func'
a_new_func: return n1n2 * arg(1)
```

```txt

number= 2               inverse= 0.5             m= 0.5             result= 0.5
number= 4               inverse= 0.25            m= 0.5             result= 0.5
number= 6               inverse= 0.166666667     m= 0.5             result= 0.5

```



## Ruby


```ruby
multiplier = proc {|n1, n2| proc {|m| n1 * n2 * m}}
numlist = [x=2, y=4, x+y]
invlist = [0.5, 0.25, 1.0/(x+y)]
p numlist.zip(invlist).map {|n, invn| multiplier[invn, n][0.5]}
# => [0.5, 0.5, 0.5]
```


This structure is identical to the treatment of Ruby's [[First-class functions#Ruby|first-class functions]] -- create a Proc object that returns a Proc object (a closure).  These examples show that 0.5 times number ''n'' (or passed to function ''f'') times inverse of ''n'' (or passed to inverse of ''f'') returns the original number, 0.5.


## Rust


```rust
#![feature(conservative_impl_trait)]
fn main() {
    let (x, xi) = (2.0, 0.5);
    let (y, yi) = (4.0, 0.25);
    let z = x + y;
    let zi = 1.0/z;

    let numlist = [x,y,z];
    let invlist = [xi,yi,zi];

    let result = numlist.iter()
                        .zip(&invlist)
                        .map(|(x,y)| multiplier(*x,*y)(0.5))
                        .collect::<Vec<_>>();
    println!("{:?}", result);
}

fn multiplier(x: f64, y: f64) -> impl Fn(f64) -> f64 {
    move |m| x*y*m
}

```


This is very similar to the [[First-class functions#Rust|first-class functions]] implementation save that the type inference works a little bit better here (e.g. when declaring <code>numlist</code> and <code>invlist</code>) and <code>multiplier</code>'s declaration is substantially simpler than <code>compose</code>'s. Both of these boil down to the fact that closures and regular functions are actually different types in Rust so we have to be generic over them but here we are only dealing with 64-bit floats.


## Scala


```scala
scala
 val x = 2.0
x: Double = 2.0

scala> val xi = 0.5
xi: Double = 0.5

scala> val y = 4.0
y: Double = 4.0

scala> val yi = 0.25
yi: Double = 0.25

scala> val z = x + y
z: Double = 6.0

scala> val zi = 1.0 / ( x + y )
zi: Double = 0.16666666666666666

scala> val numbers = List(x, y, z)
numbers: List[Double] = List(2.0, 4.0, 6.0)

scala> val inverses = List(xi, yi, zi)
inverses: List[Double] = List(0.5, 0.25, 0.16666666666666666)

scala> def multiplier = (n1: Double, n2: Double) => (m: Double) => n1 * n2 * m
multiplier: (Double, Double) => (Double) => Double

scala> def comp = numbers zip inverses map multiplier.tupled
comp: List[(Double) => Double]

scala> comp.foreach(f=>println(f(0.5)))
0.5
0.5
0.5
```



## Scheme

This implementation closely follows the Scheme implementation of the [[First-class functions]] problem.

```scheme
(define x  2.0)
(define xi 0.5)
(define y  4.0)
(define yi 0.25)
(define z  (+ x y))
(define zi (/ (+ x y)))

(define number (list x y z))
(define inverse (list xi yi zi))

(define (multiplier n1 n2) (lambda (m) (* n1 n2 m)))

(define m 0.5)
(define (go n1 n2)
  (for-each (lambda (n1 n2)
              (display ((multiplier n1 n2) m))
              (newline))
            n1 n2))
(go number inverse)
```

Output:
 0.5
 0.5
 0.5


## Sidef


```ruby
func multiplier(n1, n2) {
    func (n3) {
        n1 * n2 * n3
    }
}

var x  = 2.0
var xi = 0.5
var y  = 4.0
var yi = 0.25
var z  = (x + y)
var zi = (1 / (x + y))

var numbers = [x, y, z]
var inverses = [xi, yi, zi]

for f,g (numbers ~Z inverses) {
    say multiplier(f, g)(0.5)
}
```

```txt

0.5
0.5
0.5

```



## Slate

```slate
define: #multiplier -> [| :n1 :n2 | [| :m | n1 * n2 * m]].
define: #x -> 2.
define: #y -> 4.
define: #numlist -> {x. y. x + y}.
define: #numlisti -> (numlist collect: [| :x | 1.0 / x]).

numlist with: numlisti collect: [| :n1 :n2 | (multiplier applyTo: {n1. n2}) applyWith: 0.5].
```



## Tcl

```tcl
package require Tcl 8.5
proc multiplier {a b} {
    list apply {{ab m} {expr {$ab*$m}}} [expr {$a*$b}]
}
```

Note that, as with Tcl's solution for [[First-class functions#Tcl|First-class functions]], the resulting term must be expanded on application. For example, study this interactive session:

```tcl
% set mult23 [multiplier 2 3]
apply {{ab m} {expr {$ab*$m}}} 6
% {*}$mult23 5
30
```

Formally, for the task:

```tcl
set x  2.0
set xi 0.5
set y  4.0
set yi 0.25
set z  [expr {$x + $y}]
set zi [expr {1.0 / ( $x + $y )}]
set numlist [list $x $y $z]
set numlisti [list $xi $yi $zi]
foreach a $numlist b $numlisti {
    puts [format "%g * %g * 0.5 = %g" $a $b [{*}[multiplier $a $b] 0.5]]
}
```

Which produces this output:

```txt
2 * 0.5 * 0.5 = 0.5
4 * 0.25 * 0.5 = 0.5
6 * 0.166667 * 0.5 = 0.5
```



## Ursala

The form is very similar to the first class functions task solution
in Ursala, except that the multiplier function takes the place of the
composition operator (+), and is named in compliance
with the task specification.

```Ursala
#import std
#import flo

numbers  = <2.,4.,plus(2.,4.)>
inverses = <0.5,0.25,div(1.,plus(2.,4.))>

multiplier = //times+ times

#cast %eL

main = (gang multiplier*p\numbers inverses) 0.5
```

The multiplier could have been written in pattern
matching form like this.

```Ursala
multiplier("a","b") "c" = times(times("a","b"),"c")
```

The main program might also have been written with an
anonymous function like this.

```Ursala
main = (gang (//times+ times)*p\numbers inverses) 0.5
```

output:

```txt

<5.000000e-01,5.000000e-01,5.000000e-01>

```



## zkl


```zkl
var x=2.0, y=4.0, z=(x+y), c=T(x,y,z).apply(fcn(n){ T(n,1.0/n) });
  //-->L(L(2,0.5),L(4,0.25),L(6,0.166667))
```


```zkl
fcn multiplier(n1,n2){ return('*.fp(n1,n2)) }
```

This is actually partial evaluation, multiplier returns n1*n2*X where X isn't known yet. So multiplier(2,3)(4) --> (2*3*)4 --> 24. Even better, multiplier(2,3)(4,5) --> 120, multiplier(2,3)(4,5,6) --> 720, multiplier(2,3)() --> 6.

Alternatively,

```zkl
fcn multiplier(n1,n2){ fcn(n,X){ n*X }.fp(n1*n2) }
```


```zkl
var ms=c.apply(fcn([(n1,n2)]){ multiplier(n1,n2) });
   //-->L(Deferred,Deferred,Deferred) // lazy eval of n*(1/n)*X
ms.run(True,1.0)  //-->L(1,1,1)
ms.run(True,5.0)  //-->L(5,5,5)
ms.run(True,0.5)  //-->L(0.5,0.5,0.5)
```

List.run(True,X), for each item in the list, does i(X) and collects the results into another list. Sort of an inverted map or fold.

{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Cannot do function composition. Function definitions are dynamic, but functions cannot be passed as values. -->
