+++
title = "Exponentiation order"
description = ""
date = 2019-07-03T07:14:12Z
aliases = []
[extra]
id = 17399
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "algol_68",
  "algol_w",
  "apl",
  "awk",
  "basic",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "d",
  "echolisp",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "io",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "latitude",
  "lua",
  "maple",
  "ocaml",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "simula",
  "sinclair_zx81_basic",
  "tcl",
  "vba",
  "vbscript",
  "verbexx",
  "zkl",
]
+++

This task will demonstrate the order of [[wp:Exponentiation|exponentiation]]   <big>('''x<sup>y</sup>''') </big>   when there are multiple exponents.

(Many programming languages, especially those with extended-precision integer arithmetic, usually support one of <code>**</code>, <code>^</code>, <code>↑</code> or some such for exponentiation.)


;Task requirements
Show the result of a language's evaluation of multiple exponentiation (either as an integer or floating point).

If your language's exponentiation operator is not one of the usual ones, please comment on how to recognize it.


Using whatever operator or syntax your language supports (if any), show the results in three lines (with identification):
<big>
::::*   5**3**2
::::*   (5**3)**2
::::*   5**(3**2)
</big>



If there are other methods (or formats) of multiple exponentiations, show them as well.


## See also

* MathWorld entry:   [http://mathworld.wolfram.com/Exponentiation.html exponentiation]


## Related tasks

* Rosetta Code task:   [[Arbitrary-precision integers (included)]]





## 11l


```11l
print(5 ^ 3 ^ 2)
print((5 ^ 3) ^ 2)
print(5 ^ (3 ^ 2))
```

```txt

1.95313e+06
15625
1.95313e+06

```



## ALGOL 68

Algol 68 provides various alternative symbols for the exponentiation operator generally, "**", "^" and "UP" can be used.

```algol68
print( ( "5**3**2:   ", 5**3**2, newline ) );
print( ( "(5**3)**2: ", (5**3)**2, newline ) );
print( ( "5**(3**2): ", 5**(3**2), newline ) )
```

```txt

5**3**2:        +15625
(5**3)**2:      +15625
5**(3**2):    +1953125

```



## ALGOL W

The Algol W exponentiation operator always produces a real result and requires an integer right operand, hence the round functions in the following.

```algolw
begin
    write( "5**3**2:   ", round( 5 ** 3 ** 2 ) );
    write( "(5**3)**2: ", round( ( 5 ** 3 ) ** 2 ) );
    write( "5**(3**2): ", round( 5 ** round( 3 ** 2 ) ) )
end.
```

```txt

5**3**2:            15625
(5**3)**2:          15625
5**(3**2):        1953125

```



## APL

APL has no order of precedence other than right-to-left operation. * is the APL exponentiation operator.

```txt

      5*3*2
1953125
      (5*3)*2
15625
      5*(3*2)
1953125

```



## AWK


```AWK

# syntax: GAWK -f EXPONENTIATION_ORDER.AWK
BEGIN {
    printf("5^3^2   = %d\n",5^3^2)
    printf("(5^3)^2 = %d\n",(5^3)^2)
    printf("5^(3^2) = %d\n",5^(3^2))
    exit(0)
}

```

<p>output:</p>

```txt

5^3^2   = 1953125
(5^3)^2 = 15625
5^(3^2) = 1953125

```



## BASIC

=
## Sinclair ZX81 BASIC
=

```basic
10 PRINT "5**3**2   = ";5**3**2
20 PRINT "(5**3)**2 = ";(5**3)**2
30 PRINT "5**(3**2) = ";5**(3**2)
```

```txt
5**3**2   = 15625
(5**3)**2 = 15625
5**(3**2) = 1953125
```


=
## BBC BASIC
=

```bbcbasic
PRINT "5^3^2   = "; 5^3^2
PRINT "(5^3)^2 = "; (5^3)^2
PRINT "5^(3^2) = "; 5^(3^2)
```

```txt
5^3^2   = 15625
(5^3)^2 = 15625
5^(3^2) = 1953125
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PRINT "5^3^2   =";5^3^2
110 PRINT "(5^3)^2 =";(5^3)^2
120 PRINT "5^(3^2) =";5^(3^2)
```

```txt
5^3^2   = 15625
(5^3)^2 = 15625
5^(3^2) = 1953125
```



## C

C does not have an exponentiation operator. The caret operator '^' performs xor bitwise operation in C. The function pow in the standard C Math library takes 2 arguments.

Expressions in C are evaluated by RPN. The RPNs of 5^3^2 and 5^(3^2) are the same and thus also their pow expressions.

```C

include<stdio.h>
#include<math.h>

int main()
{
	printf("\n5 ^ 3 ^ 2   = %.0f",pow(5,pow(3,2))); /*.0f suppresses decimal output*/
	printf("\n(5 ^ 3) ^ 2 = %.0f",pow(pow(5,3),2));
	printf("\n5 ^ (3 ^ 2) = %.0f",pow(5,pow(3,2)));

	return 0;
}

```

```txt


5 ^ 3 ^ 2   = 1953125
(5 ^ 3) ^ 2 = 15625
5 ^ (3 ^ 2) = 1953125

```



## Clojure

Clojure uses prefix notation and expt only takes 2 arguments for exponentiation, so "5**3**2" isn't represented.


```clojure
(use 'clojure.math.numeric-tower)
;; (5**3)**2
(expt (expt 5 3) 2)      ; => 15625

;; 5**(3**2)
(expt 5 (expt 3 2))      ; => 1953125

;; (5**3)**2 alternative: use reduce
(reduce expt [5 3 2])  ; => 15625

;; 5**(3**2) alternative: evaluating right-to-left with reduce requires a small modification
(defn rreduce [f coll] (reduce #(f %2 %) (reverse coll)))
(rreduce expt [5 3 2]) ; => 1953125
```



## Common Lisp

Because Common Lisp uses prefix notation and <code>expt</code> accepts only two arguments, it doesn't have an expression for <code>5**3**2</code>.  Just showing expressions for the latter two.

```lisp
(expt (expt 5 3) 2)
(expt 5 (expt 3 2))
```

```txt
15625
1953125
```



## D


```d
void main() {
    import std.stdio, std.math, std.algorithm;

    writefln("5 ^^ 3 ^^ 2          = %7d", 5 ^^ 3 ^^ 2);
    writefln("(5 ^^ 3) ^^ 2        = %7d", (5 ^^ 3) ^^ 2);
    writefln("5 ^^ (3 ^^ 2)        = %7d", 5 ^^ (3 ^^ 2));
    writefln("[5, 3, 2].reduce!pow = %7d", [5, 3, 2].reduce!pow);
}
```

```txt
5 ^^ 3 ^^ 2          = 1953125
(5 ^^ 3) ^^ 2        =   15625
5 ^^ (3 ^^ 2)        = 1953125
[5, 3, 2].reduce!pow =   15625
```



## EchoLisp


```scheme

;; the standard and secure way is to use the (expt a b) function
(expt 5 (expt 3 2))  ;; 5 ** ( 3 ** 2)
    → 1953125
(expt (expt 5 3) 2) ;; (5 ** 3) ** 2
    → 15625

;; infix EchoLisp may use the ** operator, which right associates
(lib 'match)
(load 'infix.glisp)

(5 ** 3 ** 2)
    → 1953125
((5 ** 3) ** 2)
    → 15625
(5 ** (3 ** 2))
    → 1953125

```



## Factor

Factor is a concatenative stack language, so expressions take the form of reverse Polish notation. <tt>5^3^2</tt> and <tt>5^(3^2)</tt> have identical postfix notation, so they are combined.

```factor
USING: formatting math.functions ;
5 3 2 ^ ^ 5 3 ^ 2 ^ "5 3 2 ^ ^: %-7d\n5 3 ^ 2 ^: %-7d\n" printf
```

```txt

5 3 2 ^ ^: 1953125
5 3 ^ 2 ^: 15625

```



## Fortran


```Fortran
write(*, "(a, i0)") "5**3**2   = ", 5**3**2
write(*, "(a, i0)") "(5**3)**2 = ", (5**3)**2
write(*, "(a, i0)") "5**(3**2) = ", 5**(3**2)
```

```txt
5**3**2   = 1953125
(5**3)**2 = 15625
5**(3**2) = 1953125
```



## FreeBASIC


```freebasic
' FB 1.05.0

' The exponentation operator in FB is ^ rather than **.
' In the absence of parenthesis this operator is
' left-associative. So the first example
' will have the same value as the second example.

Print "5^3^2   =>"; 5^3^2
Print "(5^3)^2 =>"; (5^3)^2
Print "5^(3^2) =>"; 5^(3^2)
Sleep
```


```txt

5^3^2   => 15625
(5^3)^2 => 15625
5^(3^2) => 1953125

```



## Go


```Go
package main

import "fmt"
import "math"

func main() {
    var a, b, c float64
    a = math.Pow(5, math.Pow(3, 2))
    b = math.Pow(math.Pow(5, 3), 2)
    c = math.Pow(5, math.Pow(3, 2))
    fmt.Printf("5^3^2   = %.0f\n", a)
    fmt.Printf("(5^3)^2 = %.0f\n", b)
    fmt.Printf("5^(3^2) = %.0f\n", c)
}
```


```txt

5^3^2   = 1953125
(5^3)^2 = 15625
5^(3^2) = 1953125

```



## Groovy


Solution:

```groovy
println(" 5 ** 3 ** 2 == " + 5**3**2)
println("(5 ** 3)** 2 == " + (5**3)**2)
println(" 5 **(3 ** 2)== " + 5**(3**2))
```


Output:

```txt
 5 ** 3 ** 2 == 15625
(5 ** 3)** 2 == 15625
 5 **(3 ** 2)== 1953125
```



## Haskell


Haskell has three infix exponentiation operators dealing with different domains:

```txt
λ> :i (^)
(^) :: (Num a, Integral b) => a -> b -> a 	-- Defined in ‘GHC.Real’
infixr 8 ^
λ> :i (**)
class Fractional a => Floating a where
  ...
  (**) :: a -> a -> a
  ...
  	-- Defined in ‘GHC.Float’
infixr 8 **
λ> :i (^^)
(^^) :: (Fractional a, Integral b) => a -> b -> a  -- Defined in ‘GHC.Real’
infixr 8 ^^
```

All of them are right-associative.

```txt
λ> 5^3^2
1953125
λ> (5^3)^2
15625
λ> 5^(3^2)
1953125
λ> 5**3**2 == 5**(3**2)
True
```


However natural chaining of (^^) operator is impossible:
  5^^3^^2 = 5^^(3^^2)
but (3^^2) is not Integral any longer, so evaluation leads to the type error. Left-assiciative chain is Ok:

```txt
λ> (5^^3)^^2
15625.0
λ> ((5^^3)^^2)^^4
5.9604644775390624e16
```



## Io


```txt
Io> 5**3**2
==> 15625
Io> (5**3)**2
==> 15625
Io> 5**(3**2)
==> 1953125
Io> 5 pow(3) pow(2)
==> 15625
Io> 5 **(3) **(2)
==> 15625
Io> Number getSlot("**") == Number getSlot("pow")
==> true
Io>
```

Operators in Io are implemented as methods.  Here the <code>**</code> method is the same as the <code>pow</code> method.  Syntax sugar converts "normal" mathematical expressions to messages.


## J


J uses the same evaluation order for exponentiation as it does for assignment. That is to say: the bottom up view is right-to-left and the top-down view is left-to-right.


```J
   5^3^2
1.95312e6
   (5^3)^2
15625
   5^(3^2)
1.95312e6
```


----


## Java

Java has no exponentiation operator, but uses the static method java.lang.Math.pow(double a, double b). There are no associativity issues.


## jq

Requires: jq 1.5 or higher

jq's built-in for exponentiation is an arity-two function and thus no ambiguity arising from infix-notation is possible. Here's an example:


```jq
jq -n 'pow(pow(5;3);2)'
15625
```


For chaining, one could use `reduce`:


```jq
    def pow: reduce .[1:] as $i (.[0]; pow(.;$i))

    [5,3,2] | pow
```


Result: 15625


## Julia

```julia
@show 5 ^ 3 ^ 2 # default: power operator is read right-to-left
@show (5 ^ 3) ^ 2
@show 5 ^ (3 ^ 2)
@show reduce(^, [5, 3, 2])
@show foldl(^, [5, 3, 2]) # guarantees left associativity
@show foldr(^, [5, 3, 2]) # guarantees right associativity
```


```txt
5 ^ (3 ^ 2) = 1953125
(5 ^ 3) ^ 2 = 15625
5 ^ (3 ^ 2) = 1953125
reduce(^, [5, 3, 2]) = 15625
foldl(^, [5, 3, 2]) = 15625
foldr(^, [5, 3, 2]) = 1953125
```



## Kotlin

Kotlin does not have a dedicated exponentiation operator and we would normally use Java's Math.pow function instead. However, it's possible to define an infix function which would look like an operator and here we do so for integer base and exponent. For simplicity we disallow negative exponents altogether and consider 0 ** 0 == 1. Associativity would, of course, be the same as for a normal function call.

```scala
// version 1.0.5-2

infix fun Int.ipow(exp: Int): Int = when {
    exp < 0   -> throw IllegalArgumentException("negative exponents not allowed")
    exp == 0  -> 1
    else      -> {
        var ans = 1
        var base = this
        var e = exp
        while(e != 0) {
            if (e and 1 == 1) ans *= base
            e = e shr 1
            base *= base
        }
        ans
    }
}

fun main(args: Array<String>) {
    println("5**3**2   = ${5 ipow 3 ipow 2}")
    println("(5**3)**2 = ${(5 ipow 3) ipow 2}")
    println("5**(3**2) = ${5 ipow (3 ipow 2)}")
}
```


```txt

5**3**2   = 15625
(5**3)**2 = 15625
5**(3**2) = 1953125

```



## Latitude


```latitude
5 ^ 3 ^ 2.   ;; 1953125
(5 ^ 3) ^ 2. ;; 15625
5 ^ (3 ^ 2). ;; 1953125
```



## Lua


```Lua
print("5^3^2 = " .. 5^3^2)
print("(5^3)^2 = " .. (5^3)^2)
print("5^(3^2) = " .. 5^(3^2))
```

```txt
5^3^2 = 1953125
(5^3)^2 = 15625
5^(3^2) = 1953125
```

Lua also has math.pow(a, b), which is identical to pow(a, b) in C.  Since function arguments are contained in brackets anyway, the associativity of nested uses of math.pow will be obvious.


## Maple


```Maple
5^3^2;
(5^3)^2;
5^(3^2);
```

```txt
Error, ambiguous use of `^`, please use parentheses
15625
1953125
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
a = "5^3^2";
Print[a <> " = " <> ToString[ToExpression[a]]]
b = "(5^3)^2";
Print[b <> " = " <> ToString[ToExpression[b]]]
c = "5^(3^2)";
Print[c <> " = " <> ToString[ToExpression[c]]]
```

```txt
5^3^2 = 1953125
(5^3)^2 = 15625
5^(3^2) = 1953125
```



## OCaml

OCaml language has '**' as an exponentiation symbol for floating point integers
<OCaml code>
# 5. ** 3. ** 2. ;;
# 5. **( 3. ** 2.) ;;
#(5. ** 3. ) **2. ;;

```txt

- :   float = 1953125.
- :     float = 1953125.
- :   float = 15625.


```



## PARI/GP

Exponentiation is right-associative in GP.

```parigp
f(s)=print(s" = "eval(s));
apply(f, ["5^3^2", "(5^3)^2", "5^(3^2)"]);
```

```txt
5^3^2 = 1953125
(5^3)^2 = 15625
5^(3^2) = 1953125
```



## Perl


```perl
say "$_ = " . eval($_)  for  qw/5**3**2  (5**3)**2  5**(3**2)/;
```

```txt

5**3**2 = 1953125
(5**3)**2 = 15625
5**(3**2) = 1953125

```



## Perl 6

Note that the reduction forms automatically go right-to-left because the base operator is right-associative.  Most other operators are left-associative and would automatically reduce left-to-right instead.


```perl6
use MONKEY-SEE-NO-EVAL;
sub demo($x) { say "  $x\t───► ", EVAL $x }

demo '5**3**2';      # show ** is right associative
demo '(5**3)**2';
demo '5**(3**2)';

demo '[**] 5,3,2';   # reduction form, show only final result
demo '[\**] 5,3,2';  # triangle reduction, show growing results

# Unicode postfix exponents are supported as well:

demo '(5³)²';
demo '5³²';

```


```txt

  5**3**2	───► 1953125
  (5**3)**2	───► 15625
  5**(3**2)	───► 1953125
  [**] 5,3,2	───► 1953125
  [\**] 5,3,2	───► 2 9 1953125
  (5³)²	───► 15625
  5³²	───► 23283064365386962890625

```


The Unicode exponent form without parentheses ends up raising to the 32nd power.  Nor are you even allowed to parenthesize it the other way: <tt>5(³²)</tt> would be a syntax error.  Despite all that, for programs that do a lot of squaring or cubing, the postfix forms can enhance both readability and concision.


## Phix

Phix has a power function rather than an infix power operator, hence there is no possible confusion.

```Phix
?power(power(5,3),2)
?power(5,power(3,2))
```

```txt

15625
1953125

```



## PicoLisp

The PicoLisp '**' exponentiation function takes 2 arguments

```PicoLisp
: (** (** 5 3) 2)
-> 15625

: (** 5 (** 3 2))
-> 1953125
```



## Python


```python
>>
 5**3**2
1953125
>>> (5**3)**2
15625
>>> 5**(3**2)
1953125
>>> # The following is not normally done
>>> try: from functools import reduce # Py3K
except: pass

>>> reduce(pow, (5, 3, 2))
15625
>>>
```



## Racket


```racket
#lang racket
;; 5**3**2 depends on associativity of ** : Racket's (scheme's) prefix function
;; calling syntax only allows for pairs of arguments for expt.

;; So no can do for 5**3**2
;; (5**3)**2
(displayln "prefix")
(expt (expt 5 3) 2)
;; (5**3)**2
(expt 5 (expt 3 2))

;; There is also a less-used infix operation (for all functions, not just expt)... which I suppose
;; might do with an airing. But fundamentally nothing changes.
(displayln "\"in\"fix")
((5 . expt . 3) . expt .  2)
(5  . expt . (3 . expt . 2))

;; everyone's doing a reduction, it seems
(displayln "reduction")
(require (only-in srfi/1 reduce reduce-right))
(reduce expt 1 '(5 3 2))
(reduce-right expt 1 '(5 3 2))
```

```txt
prefix
15625
1953125
"in"fix
15625
1953125
reduction
14134776518227074636666380005943348126619871175004951664972849610340958208
1953125
```



## REXX


```rexx
/*REXX program demonstrates various ways of multiple exponentiations.   */
/*┌────────────────────────────────────────────────────────────────────┐
  │ The REXX language uses      **      for exponentiation.            │
  │                   Also,    *  *     can be used.                   │
  |    and even                */*power of*/*                          |
  └────────────────────────────────────────────────────────────────────┘*/

say '   5**3**2   ───► '    5**3**2
say '   (5**3)**2 ───► '    (5**3)**2
say '   5**(3**2) ───► '    5**(3**2)
                                       /*stick a fork in it, we're done.*/
```

'''output'''

```txt

   5**3**2   ───►  15625
   (5**3)**2 ───►  15625
   5**(3**2) ───►  1953125

```



## Ring


In the Ring it is impossible to show the result of: 5^3^2


```ring

see "(5^3)^2 =>" + pow(pow(5,3),2) + nl
see "5^(3^2) =>" + pow(5,pow(3,2)) + nl

```

Output:

```txt

(5^3)^2 =>15625
5^(3^2) =>1953125

```



## Ruby


```ruby
ar = ["5**3**2", "(5**3)**2", "5**(3**2)", "[5,3,2].inject(:**)"]
ar.each{|exp| puts "#{exp}:\t#{eval exp}"}

```

```txt

5**3**2:	1953125
(5**3)**2:	15625
5**(3**2):	1953125
[5,3,2].inject(:**):	15625

```



## Rust



```rust
fn main() {
    println!("5**3**2   = {:7}", 5u32.pow(3).pow(2));
    println!("(5**3)**2 = {:7}", (5u32.pow(3)).pow(2));
    println!("5**(3**2) = {:7}", 5u32.pow(3u32.pow(2)));
}
```

```txt

5**3**2   =   15625
(5**3)**2 =   15625
5**(3**2) = 1953125

```



## Scala

Scal has no exponentiation operator, but uses the function (scala.)math.pow(x: Double, y: Double): Double function in the Scala runtime library.
Integer exponentiation can be done with e.g. BigInt or BigInteger.pow(n: Int) method.
There are no associativity issues.

## Sidef

In Sidef, the whitespace between the operands and the operator controls the precedence of the operation.

```ruby
var a = [
    '5**3**2',
    '(5**3)**2',
    '5**(3**2)',
    '5 ** 3 ** 2',
    '5 ** 3**2',
    '5**3 ** 2',
    '[5,3,2]«**»',
]

a.each {|e|
    "%-12s == %s\n".printf(e, eval(e))
}
```

```txt

5**3**2      == 1953125
(5**3)**2    == 15625
5**(3**2)    == 1953125
5 ** 3 ** 2  == 15625
5 ** 3**2    == 1953125
5**3 ** 2    == 15625
[5,3,2]«**»  == 15625

```



## Simula


```Simula
OutText("5** 3 **2: "); OutInt(5** 3 **2, 0); Outimage;
OutText("(5**3)**2: "); OutInt((5**3)**2, 0); Outimage;
OutText("5**(3**2): "); OutInt(5**(3**2), 0); Outimage
```

```txt
5** 3 **2: 15625
(5**3)**2: 15625
5**(3**2): 1953125
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln("5**3**2   = " <& 5**3**2);
    writeln("(5**3)**2 = " <& (5**3)**2);
    writeln("5**(3**2) = " <& 5**(3**2));
  end func;
```


```txt

5**3**2   = 1953125
(5**3)**2 = 15625
5**(3**2) = 1953125

```



## Tcl


```tcl
foreach expression {5**3**2 (5**3)**2 5**(3**2)} {
    puts "${expression}:\t[expr $expression]"
}
```

```txt

5**3**2:	1953125
(5**3)**2:	15625
5**(3**2):	1953125

```

There's also a binary <code>pow()</code> expression function that always converts its arguments to floating point numbers and then applies the exponentiation operation; it's now largely obsolete because of the <code>**</code> operator, but is retained for backward compatibility with older programs.


## VBA


```vb
Public Sub exp()
    Debug.Print "5^3^2", 5 ^ 3 ^ 2
    Debug.Print "(5^3)^2", (5 ^ 3) ^ 2
    Debug.Print "5^(3^2)", 5 ^ (3 ^ 2)
End Sub
```
```txt
5^3^2          15625
(5^3)^2        15625
5^(3^2)        1953125

```


## VBScript


```vb

WScript.StdOut.WriteLine "5^3^2 => " & 5^3^2
WScript.StdOut.WriteLine "(5^3)^2 => " & (5^3)^2
WScript.StdOut.WriteLine "5^(3^2) => " & 5^(3^2)

```


```txt

5^3^2 => 15625
(5^3)^2 => 15625
5^(3^2) => 1953125

```



## Verbexx


```verbexx
// Exponentiation order example:

@SAY "5**3**2   = " ( 5**3**2   );
@SAY "(5**3)**2 = " ( (5**3)**2 );
@SAY "5**(3**2) = " ( 5**(3**2) );

/] Output:

    5**3**2   =  1953125
    (5**3)**2 =  15625
    5**(3**2) =  1953125
```



## zkl

zkl does not have an exponentiation operator but floats have a pow method.

```zkl
println("5 ^ 3 ^ 2   = %,d".fmt((5.0).pow((3.0).pow(2))));
println("(5 ^ 3) ^ 2 = %,d".fmt((5.0).pow(3).pow(2)));
println("5 ^ (3 ^ 2) = %,d".fmt((5.0).pow((3.0).pow(2))));
```

```txt

5 ^ 3 ^ 2   = 1,953,125
(5 ^ 3) ^ 2 = 15,625
5 ^ (3 ^ 2) = 1,953,125

```

