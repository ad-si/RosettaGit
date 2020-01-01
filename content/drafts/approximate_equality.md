+++
title = "Approximate Equality"
description = ""
date = 2019-10-15T09:57:07Z
aliases = []
[extra]
id = 22502
[taxonomies]
categories = []
tags = []
+++

{{task}}

Sometimes, when testing whether the solution to a task (for example, here on Rosetta Code) is correct, the
difference in floating point calculations between different language implementations becomes significant.

For example, a difference between '''32''' bit and '''64''' bit floating point calculations may appear by
about the 8th significant digit in base 10 arithmetic.


;Task:
Create a function which returns true if two floating point numbers are approximately equal.


The function should allow for differences in the magnitude of numbers, so that, for example,

'''100000000000000.01'''   may be approximately equal to   '''100000000000000.011''',

even though   '''100.01'''   is not approximately equal to   '''100.011'''.

If the language has such a feature in its standard library, this may be used instead of a custom function.

Show the function results with comparisons on the following pairs of values:
:#     100000000000000.01,                        100000000000000.011       (note: should return ''true'')
:#     100.01,                                    100.011                                                     (note: should return ''false'')
:#     10000000000000.001 <big>/</big> 10000.0,   1000000000.0000001000
:#     0.001,                                     0.0010000001
:#     0.000000000000000000000101,                0.0
:#      sqrt(2) * sqrt(2),                    2.0
:#     -sqrt(2) * sqrt(2),                        -2.0
:#     3.14159265358979323846,                    3.14159265358979324
<br/>
Answers should be true for the first example and false in the second, so that just rounding the numbers to a fixed number of decimals should not be enough. Otherwise answers may vary and still be correct. See the Python code for one type of solution.




__TOC__


## C#


```c#
using System;

public static class Program
{
    public static void Main() {
        Test(100000000000000.01, 100000000000000.011);
        Test(100.01, 100.011);
        Test(10000000000000.001 / 10000.0, 1000000000.0000001000);
        Test(0.001, 0.0010000001);
        Test(0.000000000000000000000101, 0.0);
        Test(Math.Sqrt(2) * Math.Sqrt(2), 2.0);
        Test(-Math.Sqrt(2) * Math.Sqrt(2), -2.0);
        Test(3.14159265358979323846, 3.14159265358979324);

        void Test(double a, double b) {
            const double epsilon = 1e-18;
            WriteLine($"{a}, {b} => {a.ApproxEquals(b, epsilon)}");
        }
    }

    public static bool ApproxEquals(this double value, double other, double epsilon) => Math.Abs(value - other) < epsilon;
}
```

{{out}}

```txt

100000000000000.02, 100000000000000.02 => True
100.01, 100.011 => False
1000000000.0000002, 1000000000.0000001 => False
0.001, 0.0010000001 => False
1.01E-22, 0 => True
2.0000000000000004, 2 => False
-2.0000000000000004, -2 => False
3.141592653589793, 3.141592653589793 => True
```



## Factor

The <code>~</code> word takes three arguments: the two values to be compared, and an epsilon value representing the allowed distance between the two values. A positive epsilon performs an absolute distance test, an epsilon of zero performs an exact comparison, and a negative epsilon performs a relative distance test (as required by this task).
{{works with|Factor|0.99 development version 2019-07-10}}

```factor
USING: formatting generalizations kernel math math.functions ;

100000000000000.01             100000000000000.011
100.01                         100.011
10000000000000.001 10000.0 /f  1000000000.0000001000
0.001                          0.0010000001
0.000000000000000000000101     0.0
2 sqrt dup *                   2.0
2 sqrt dup neg *              -2.0
3.14159265358979323846         3.14159265358979324

[ 2dup -1e-15 ~ "%+47.30f %+47.30f -1e-15 ~ : %u\n" printf ]
2 8 mnapply
```

{{out}}

```txt

+100000000000000.015625000000000000000000000000 +100000000000000.015625000000000000000000000000 -1e-15 ~ : t
            +100.010000000000005115907697472721             +100.010999999999995679900166578591 -1e-15 ~ : f
     +1000000000.000000238418579101562500000000      +1000000000.000000119209289550781250000000 -1e-15 ~ : t
              +0.001000000000000000020816681712               +0.001000000100000000054917270731 -1e-15 ~ : f
              +0.000000000000000000000101000000               +0.000000000000000000000000000000 -1e-15 ~ : f
              +2.000000000000000444089209850063               +2.000000000000000000000000000000 -1e-15 ~ : t
              -2.000000000000000444089209850063               -2.000000000000000000000000000000 -1e-15 ~ : t
              +3.141592653589793115997963468544               +3.141592653589793115997963468544 -1e-15 ~ : t

```



## Go

Go's float64 type is limited to 15 or 16 digits of precision. As there are some numbers in this task which have more digits than this I've used big.Float instead.

```go
package main

import (
    "fmt"
    "log"
    "math/big"
)

func max(a, b *big.Float) *big.Float {
    if a.Cmp(b) > 0 {
        return a
    }
    return b
}

func isClose(a, b *big.Float) bool {
    relTol := big.NewFloat(1e-9) // same as default for Python's math.isclose() function
    t := new(big.Float)
    t.Sub(a, b)
    t.Abs(t)
    u, v, w := new(big.Float), new(big.Float), new(big.Float)
    u.Mul(relTol, max(v.Abs(a), w.Abs(b)))
    return t.Cmp(u) <= 0
}

func nbf(s string) *big.Float {
    n, ok := new(big.Float).SetString(s)
    if !ok {
        log.Fatal("invalid floating point number")
    }
    return n
}

func main() {
    root2 := big.NewFloat(2.0)
    root2.Sqrt(root2)
    pairs := [][2]*big.Float{
        {nbf("100000000000000.01"), nbf("100000000000000.011")},
        {nbf("100.01"), nbf("100.011")},
        {nbf("0").Quo(nbf("10000000000000.001"), nbf("10000.0")), nbf("1000000000.0000001000")},
        {nbf("0.001"), nbf("0.0010000001")},
        {nbf("0.000000000000000000000101"), nbf("0.0")},
        {nbf("0").Mul(root2, root2), nbf("2.0")},
        {nbf("0").Mul(nbf("0").Neg(root2), root2), nbf("-2.0")},
        {nbf("100000000000000003.0"), nbf("100000000000000004.0")},
        {nbf("3.14159265358979323846"), nbf("3.14159265358979324")},
    }
    for _, pair := range pairs {
        s := "≉"
        if isClose(pair[0], pair[1]) {
            s = "≈"
        }
        fmt.Printf("% 21.19g %s %- 21.19g\n", pair[0], s, pair[1])
    }
}
```


{{out}}

```txt

   100000000000000.01 ≈  100000000000000.011
               100.01 ≉  100.011
   1000000000.0000001 ≈  1000000000.0000001
                0.001 ≉  0.0010000001
             1.01e-22 ≉  0
 2.000000000000000273 ≈  2
-2.000000000000000273 ≈ -2
   100000000000000003 ≈  100000000000000004
 3.141592653589793239 ≈  3.14159265358979324

```



## Julia

Julia has an infix operator, ≈, which corresponds to Julia's buitin isapprox() function.
{{trans|Python}}

```julia
testvalues = [[100000000000000.01,           100000000000000.011],
              [100.01,                       100.011],
              [10000000000000.001 / 10000.0, 1000000000.0000001000],
              [0.001,                        0.0010000001],
              [0.000000000000000000000101,   0.0],
              [sqrt(2) * sqrt(2),            2.0],
              [-sqrt(2) * sqrt(2),          -2.0],
              [3.14159265358979323846,       3.14159265358979324]]

for (x, y) in testvalues
    println(rpad(x, 21), " ≈ ", lpad(y, 22), ": ", x ≈ y)
end

```
{{out}}

```txt

1.0000000000000002e14 ≈  1.0000000000000002e14: true
100.01                ≈                100.011: false
1.0000000000000002e9  ≈   1.0000000000000001e9: true
0.001                 ≈           0.0010000001: false
1.01e-22              ≈                    0.0: false
2.0000000000000004    ≈                    2.0: true
-2.0000000000000004   ≈                   -2.0: true
3.141592653589793     ≈      3.141592653589793: true

```



## Perl

Passes task tests, but use the module <code>Test::Number::Delta</code> for anything of real importance.

```perl
use strict;
use warnings;

sub is_close {
    my($a,$b,$eps) = @_;
    $eps //= 15;
    my $epse = $eps;
    $epse++ if sprintf("%.${eps}f",$a) =~ /\./;
    $epse++ if sprintf("%.${eps}f",$a) =~ /\-/;
    my $afmt = substr((sprintf "%.${eps}f", $a), 0, $epse);
    my $bfmt = substr((sprintf "%.${eps}f", $b), 0, $epse);
    printf "%-5s %s ≅ %s\n", ($afmt eq $bfmt ? 'True' : 'False'), $afmt, $bfmt;
}

for (
    [100000000000000.01, 100000000000000.011],
    [100.01, 100.011],
    [10000000000000.001 / 10000.0, 1000000000.0000001000],
    [0.001, 0.0010000001],
    [0.000000000000000000000101, 0.0],
    [sqrt(2) * sqrt(2), 2.0],
    [-sqrt(2) * sqrt(2), -2.0],
    [100000000000000003.0, 100000000000000004.0],
    [3.14159265358979323846, 3.14159265358979324]
    ) {
        my($a,$b) = @$_;
        is_close($a,$b);
}

print "\nTolerance may be adjusted.\n";
my $real_pi  = 2 * atan2(1, 0);
my $roman_pi = 22/7;
is_close($real_pi,$roman_pi,$_) for <10 3>;
```

{{out}}

```txt
True  100000000000000.0 ≅ 100000000000000.0
False 100.0100000000000 ≅ 100.0109999999999
True  1000000000.000000 ≅ 1000000000.000000
False 0.001000000000000 ≅ 0.001000000100000
True  0.000000000000000 ≅ 0.000000000000000
True  2.000000000000000 ≅ 2.000000000000000
True  -2.000000000000000 ≅ -2.000000000000000
True  10000000000000000 ≅ 10000000000000000
True  3.141592653589793 ≅ 3.141592653589793

Tolerance may be adjusted.
False 3.141592653 ≅ 3.142857142
True  3.14 ≅ 3.14
```



## Perl 6

{{works with|Rakudo|2019.07.1}}
Is approximately equal to is a built-in operator in Perl 6. Unicode ≅, or the ASCII equivalent: =~=. By default it uses a tolerance of 1e-15 times the order of magnitude of the larger comparand, though that is adjustable by setting the dynamic variable $*TOLERANCE to the desired value. Probably a good idea to localize the changed $*TOLERANCE as it will affect all comparisons within its scope.

Most of the following tests are somewhat pointless in Perl 6. To a large extent, when dealing with Rational values, you don't really need to worry about "approximately equal to", and all of the test values below, with the exception of <code>sqrt(2)</code>, are Rats by default, and exact. You would have specifically coerce them to Nums (floating point) to lose the precision.

For example, in Perl 6, the sum of .1, .2, .3, & .4 is ''identically'' equal to 1.


```perl6>say 0.1 + 0.2 + 0.3 + 0.4 === 1.0000000000000000000000000000000000000000000000000000000000000000000000000; # True</lang


It's also ''approximately'' equal to 1 but... ¯\_(ツ)_/¯


```perl6
for
    100000000000000.01, 100000000000000.011,
    100.01, 100.011,
    10000000000000.001 / 10000.0, 1000000000.0000001000,
    0.001, 0.0010000001,
    0.000000000000000000000101, 0.0,
    sqrt(2) * sqrt(2), 2.0,
    -sqrt(2) * sqrt(2), -2.0,
    100000000000000003.0, 100000000000000004.0,
    3.14159265358979323846, 3.14159265358979324

  -> $a, $b {
    say "$a ≅ $b: ", $a ≅ $b;
}

say "\nTolerance may be adjusted.";

say 22/7, " ≅ ", π, ": ", 22/7 ≅ π;
{ # Localize the tolerance to only this block
  my $*TOLERANCE = .001;
  say 22/7, " ≅ ", π, ": ", 22/7 ≅ π;
}
```

{{out}}

```txt
100000000000000.01 ≅ 100000000000000.011: True
100.01 ≅ 100.011: False
1000000000.0000001 ≅ 1000000000.0000001: True
0.001 ≅ 0.0010000001: False
0.000000000000000000000101 ≅ 0: True
2.0000000000000004 ≅ 2: True
-2.0000000000000004 ≅ -2: True
100000000000000003 ≅ 100000000000000004: True
3.141592653589793226752 ≅ 3.14159265358979324: True

Tolerance may be adjusted.
3.142857 ≅ 3.141592653589793: False
3.142857 ≅ 3.141592653589793: True
```



## Phix

Traditionally I have always just used sprintf() to compare floating point atoms in phix.

For this task, it proved much harder to get decent-looking output, than it did to perform the tests, and to that end I
decided to allow the display format (dfmt) to be overidden, when needed, and for the tricker/ambiguous test 5, I also allow the compare format (cfmt) to be overidden, getting ''both'' a true and false result. Likewise I have a different result for test 4 to everyone else, but simply setting the cfmt to "%.8f" would get it the NOT.

```Phix
procedure test(atom a,b, string dfmt="%g", cfmt="%g")
    bool eq = sprintf(cfmt,a)==sprintf(cfmt,b)
    string eqs = iff(eq?"":"NOT "),
           sa = sprintf(dfmt,a),
           sb = sprintf(dfmt,b)
    printf(1,"%30s is %sapproximately equal to %s\n",{sa,eqs,sb})
end procedure

test(100000000000000.01,100000000000000.011,"%.3f")
test(100.01,100.011)
test(10000000000000.001/10000.0,1000000000.0000001000,"%.10f")
test(0.001,0.0010000001,"%.8f")
test(0.000000000000000000000101,0.0,"%f")
test(0.000000000000000000000101,0.0,"%f","%6f")
test(sqrt(2)*sqrt(2),2.0)
test(-sqrt(2)*sqrt(2),-2.0)
test(3.14159265358979323846,3.14159265358979324,"%.20f")
```

{{out}}
64 bit (implied by some of the accuracies specified for this task):

```txt

           100000000000000.010 is approximately equal to 100000000000000.011
                        100.01 is NOT approximately equal to 100.011
         1000000000.0000001001 is approximately equal to 1000000000.0000001000
                 0.00100000000 is approximately equal to 0.0010000001
 0.000000000000000000000101000 is NOT approximately equal to 0.000000
 0.000000000000000000000101000 is approximately equal to 0.000000
                             2 is approximately equal to 2
                            -2 is approximately equal to -2
        3.14159265358979323851 is approximately equal to 3.14159265358979324003

```

32 bit (in fact a couple of them, the first and last pairs, are actually genuinely identical):

```txt

           100000000000000.016 is approximately equal to 100000000000000.016
                        100.01 is NOT approximately equal to 100.011
         1000000000.0000002384 is approximately equal to 1000000000.0000001192
                  0.0010000000 is approximately equal to 0.0010000001
 0.000000000000000000000101000 is NOT approximately equal to 0.000000
 0.000000000000000000000101000 is approximately equal to 0.000000
                             2 is approximately equal to 2
                            -2 is approximately equal to -2
            3.1415926535897931 is approximately equal to 3.1415926535897931

```



## Python

The Python source documentation states:

```txt

math.isclose -> bool
    a: double
    b: double
    *
    rel_tol: double = 1e-09
        maximum difference for being considered "close", relative to the
        magnitude of the input values
    abs_tol: double = 0.0
        maximum difference for being considered "close", regardless of the
        magnitude of the input values
Determine whether two floating point numbers are close in value.
Return True if a is close in value to b, and False otherwise.
For the values to be considered close, the difference between them
must be smaller than at least one of the tolerances.
-inf, inf and NaN behave similarly to the IEEE 754 Standard.  That
is, NaN is not close to anything, even itself.  inf and -inf are
only close to themselves.

```


```python
from numpy import sqrt
from math import isclose

testvalues = [[100000000000000.01,           100000000000000.011],
              [100.01,                       100.011],
              [10000000000000.001 / 10000.0, 1000000000.0000001000],
              [0.001,                        0.0010000001],
              [0.000000000000000000000101,   0.0],
              [sqrt(2) * sqrt(2),            2.0],
              [-sqrt(2) * sqrt(2),          -2.0],
              [3.14159265358979323846,       3.14159265358979324]]

for (x, y) in testvalues:
    maybenot = "is" if isclose(x, y) else "is NOT"
    print(x, maybenot, "approximately equal to ", y)


```
{{out}}

```txt

100000000000000.02 is approximately equal to  100000000000000.02
100.01 is NOT approximately equal to  100.011
1000000000.0000002 is approximately equal to  1000000000.0000001
0.001 is NOT approximately equal to  0.0010000001
1.01e-22 is NOT approximately equal to  0.0
2.0 is approximately equal to  2.0
-2.0 is approximately equal to  -2.0
3.141592653589793 is approximately equal to  3.141592653589793

```



## Racket


In Racket, a number literal with decimal point is considered a flonum, an inexact number which could be either 30 or 62 bits depending on machines. By prefixing the literal with <code>#e</code>, it is now considered an exact, rational number. In this task, we test the approximate equality on both variants:


```racket
#lang racket

(define (≈ a b [tolerance 1e-9])
  (<= (abs (/ (- a b) (max a b))) tolerance))

(define all-tests
  `(([100000000000000.01 100000000000000.011]
     [100.01 100.011]
     [,(/ 10000000000000.001 10000.0) 1000000000.0000001000]
     [0.001 0.0010000001]
     [0.000000000000000000000101 0.0]
     [,(* (sqrt 2) (sqrt 2)) 2.0]
     [,(* (- (sqrt 2)) (sqrt 2)) -2.0]
     [100000000000000003.0 100000000000000004.0]
     [3.14159265358979323846 3.14159265358979324])
    ([#e100000000000000.01 #e100000000000000.011]
     [#e100.01 #e100.011]
     [,(/ #e10000000000000.001 #e10000.0) #e1000000000.0000001000]
     [#e0.001 #e0.0010000001]
     [#e0.000000000000000000000101 #e0.0]
     [,(* (sqrt 2) (sqrt 2)) #e2.0]
     [,(* (- (sqrt 2)) (sqrt 2)) #e-2.0]
     [100000000000000003 100000000000000004]
     [#e3.14159265358979323846 #e3.14159265358979324])))

(define (format-num x)
  (~a (~r x #:precision 30) #:min-width 50 #:align 'right))

(for ([tests (in-list all-tests)] [name '("inexact" "exact")])
  (printf "~a:\n" name)
  (for ([test (in-list tests)])
    (match-define (list a b) test)
    (printf "~a ~a: ~a\n" (format-num a) (format-num b) (≈ a b)))
  (newline))
```


{{out}}

```txt

inexact:
    100000000000000.015625000000000000310697263104     100000000000000.015625000000000000310697263104: #t
                100.010000000000005116710235406336                 100.010999999999995680439855480832: #f
         1000000000.000000238418579101562504740864          1000000000.000000119209289550781252370432: #t
                  0.001000000000000000013287555072                   0.001000000100000000093229940736: #f
                        0.000000000000000000000101                                                  0: #f
                  2.000000000000000444089209850063                                                  2: #t
                 -2.000000000000000444089209850063                                                 -2: #t
                                100000000000000000                                 100000000000000000: #t
                  3.141592653589793121575456735232                   3.141592653589793121575456735232: #t

exact:
                                100000000000000.01                                100000000000000.011: #t
                                            100.01                                            100.011: #f
                                1000000000.0000001                                 1000000000.0000001: #t
                                             0.001                                       0.0010000001: #f
                        0.000000000000000000000101                                                  0: #f
                  2.000000000000000444089209850063                                                  2: #t
                 -2.000000000000000444089209850063                                                 -2: #t
                                100000000000000003                                 100000000000000004: #t
                            3.14159265358979323846                                3.14159265358979324: #t

```



## REXX

Since the REXX language uses decimal digits (characters) for floating point numbers (and integers),   it's just a matter of

choosing the   ''number''   of decimal digits for the precision to be used for arithmetic   (in this case, fifteen decimal digits).

```rexx
/*REXX program mimics an  "approximately equal to"  for comparing floating point numbers*/
numeric digits 15                                /*what other FP hardware normally uses.*/
@.=                                              /*assign default for the   @   array.  */
parse arg @.1                                    /*obtain optional argument from the CL.*/
if @.1=''  |  @.1==","  then do;   @.1= 100000000000000.01           100000000000000.011
                                   @.2= 100.01                       100.011
                                   @.3= 10000000000000.001 / 10000   1000000000.0000001000
                                   @.4= 0.001                        0.0010000001
                                   @.5= 0.00000000000000000000101    0.0
                                   @.6=  sqrt(2) * sqrt(2)           2.0
                                   @.7= -sqrt(2) * sqrt(2)           '-2.0'
                                   @.8= 3.14159265358979323846       3.14159265358979324
          /* added ───► */         @.9= 100000000000000003.0         100000000000000004.0
                             end
     do j=1  while @.j\==''                      /*process CL argument or the array #s. */
     say
     say center(' processing pair ' j" ",71,'═') /*display a title for the pair of #s.  */
     parse value  @.j  with  a  b                /*extract two values from a pair of #s.*/
     say 'A='   a                                /*display the value of  A  to the term.*/
     say 'B='   b                                /*   "     "    "    "  B   "  "    "  */
     say right('A approximately equal to B?', 65)   word("false true", 1 + approxEQ(a,b) )
     end   /*j*/                                 /* [↑]  right─justify text & true/false*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
approxEQ: procedure; parse arg x,y;   return x=y /*floating point compare with 15 digits*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x;  if x=0  then return 0;  d=digits();  numeric digits;  h=d+6
      numeric form; m.=9; parse value format(x,2,1,,0) 'E0' with g "E" _ .; g=g *.5'e'_ %2
        do j=0  while h>9;      m.j=h;               h=h%2+1;       end  /*j*/
        do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;  end  /*k*/; return g/1
```

{{out|output|text=  when using the internal default inputs:}}

```txt

═════════════════════════ processing pair  1 ══════════════════════════
A= 100000000000000.01
B= 100000000000000.011
                                      A approximately equal to B? true

═════════════════════════ processing pair  2 ══════════════════════════
A= 100.01
B= 100.011
                                      A approximately equal to B? false

═════════════════════════ processing pair  3 ══════════════════════════
A= 1000000000
B= 1000000000.0000001000
                                      A approximately equal to B? true

═════════════════════════ processing pair  4 ══════════════════════════
A= 0.001
B= 0.0010000001
                                      A approximately equal to B? false

═════════════════════════ processing pair  5 ══════════════════════════
A= 0.00000000000000000000101
B= 0.0
                                      A approximately equal to B? false

═════════════════════════ processing pair  6 ══════════════════════════
A= 2.00000000000000
B= 2.0
                                      A approximately equal to B? true

═════════════════════════ processing pair  7 ══════════════════════════
A= -2.00000000000000
B= -2.0
                                      A approximately equal to B? true

═════════════════════════ processing pair  8 ══════════════════════════
A= 3.14159265358979323846
B= 3.14159265358979324
                                      A approximately equal to B? true

═════════════════════════ processing pair  9 ══════════════════════════
A= 100000000000000003.0
B= 100000000000000004.0
                                      A approximately equal to B? true

```



## Sidef

Two values can be compared for approximate equality by using the built-in operator '''≅''', available in ASCII as '''=~=''', which does approximate comparison by rounding both operands at '''(PREC>>2)-1''' decimals. However, by default, Sidef uses a floating-point precision of 192 bits.

```ruby
[
    100000000000000.01, 100000000000000.011,
    100.01, 100.011,
    10000000000000.001 / 10000.0, 1000000000.0000001000,
    0.001, 0.0010000001,
    0.000000000000000000000101, 0.0,
    sqrt(2) * sqrt(2), 2.0,
    -sqrt(2) * sqrt(2), -2.0,
    sqrt(-2) * sqrt(-2), -2.0,
    cbrt(3)**3, 3,
    cbrt(-3)**3, -3,
    100000000000000003.0, 100000000000000004.0,
    3.14159265358979323846, 3.14159265358979324
].each_slice(2, {|a,b|
    say ("#{a} ≅ #{b}: ", a ≅ b)
})
```

{{out}}

```txt

100000000000000.01 ≅ 100000000000000.011: false
100.01 ≅ 100.011: false
1000000000.0000001 ≅ 1000000000.0000001: true
0.001 ≅ 0.0010000001: false
0.000000000000000000000101 ≅ 0: false
2 ≅ 2: true
-2 ≅ -2: true
-2 ≅ -2: true
3 ≅ 3: true
-3-7.82914889268316957969274243345625157631318402415e-58i ≅ -3: true
100000000000000003 ≅ 100000000000000004: false
3.14159265358979323846 ≅ 3.14159265358979324: false

```


The Number '''n.round(-k)''' can be used for rounding the number ''n'' to ''k'' decimal places. A positive argument can be used for rounding before the decimal point.


```ruby
var a = 100000000000000.01
var b = 100000000000000.011

# Rounding at 2 and 3 decimal places, respectively
say (round(a, -2) == round(b, -2))      # true
say (round(a, -3) == round(b, -3))      # false
```


There is also the built-in '''approx_cmp(a, b, k)''' method, which is equivalent with '''a.round(k) <=> b.round(k)'''.


```ruby
var a = 22/7
var b = Num.pi

say ("22/7 ≅ π at 2 decimals: ", approx_cmp(a, b, -2) == 0)
say ("22/7 ≅ π at 3 decimals: ", approx_cmp(a, b, -3) == 0)
```


{{out}}

```txt

22/7 ≅ π at 2 decimals: true
22/7 ≅ π at 3 decimals: false

```


Additionally, the '''rat_approx''' method can be used for computing a very good rational approximation to a given real value:


```ruby
say (1.33333333.rat_approx == 4/3)   # true
say (zeta(-5).rat_approx == -1/252)  # true
```


Rational approximations illustrated for substrings of PI:

```ruby
for k in (3..19) {
    var r = Str(Num.pi).first(k)
    say ("rat_approx(#{r}) = ", Num(r).rat_approx.as_frac)
}
```

{{out}}

```txt

rat_approx(3.1) = 31/10
rat_approx(3.14) = 22/7
rat_approx(3.141) = 245/78
rat_approx(3.1415) = 333/106
rat_approx(3.14159) = 355/113
rat_approx(3.141592) = 355/113
rat_approx(3.1415926) = 86953/27678
rat_approx(3.14159265) = 102928/32763
rat_approx(3.141592653) = 103993/33102
rat_approx(3.1415926535) = 1354394/431117
rat_approx(3.14159265358) = 833719/265381
rat_approx(3.141592653589) = 17925491/5705861
rat_approx(3.1415926535897) = 126312511/40206521
rat_approx(3.14159265358979) = 144029661/45846065
rat_approx(3.141592653589793) = 325994779/103767361
rat_approx(3.1415926535897932) = 903259831/287516534
rat_approx(3.14159265358979323) = 1726375805/549522486

```



## zkl

Floats are 64 bit and have the closeTo method, which takes a comparison value
and tolerance. If the tolerance is >=0, comparison is absolute.
If tolerance is <0 (and x!=0 and y!=0), the comparison is relative.

```zkl
testValues:=T(
   T(100000000000000.01,100000000000000.011),
   T(100.01, 100.011),
   T(10000000000000.001 / 10000.0, 1000000000.0000001),
   T(0.001, 0.0010000001),
   T(0.00000000000000000101, 0.0),
   T(  (2.0).sqrt()*(2.0).sqrt(),  2.0),
   T( -(2.0).sqrt()*(2.0).sqrt(), -2.0),
   T(100000000000000003.0, 100000000000000004.0),
   T(3.14159265358979323846, 3.14159265358979324)
);

tolerance:=-1e-9;	// <0 for relative comparison
foreach x,y in (testValues){
   maybeNot:=( if(x.closeTo(y,tolerance)) " \u2248" else "!\u2248" );
   println("% 25.19g %s %- 25.19g  %g".fmt(x,maybeNot,y, (x-y).abs()));
}
```


{{out}}

```txt

     100000000000000.0156  ≈  100000000000000.0156      0
     100.0100000000000051 !≈  100.0109999999999957      0.001
     1000000000.000000238  ≈  1000000000.000000119      1.19209e-07
  0.001000000000000000021 !≈  0.001000000100000000055   1e-10
 1.010000000000000018e-18 !≈  0                         1.01e-18
     2.000000000000000444  ≈  2                         4.44089e-16
    -2.000000000000000444  ≈ -2                         4.44089e-16
       100000000000000000  ≈  100000000000000000        0
     3.141592653589793116  ≈  3.141592653589793116      0

```

