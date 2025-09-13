+++
title = "Polynomial synthetic division"
description = ""
date = 2019-09-09T18:24:24Z
aliases = []
[extra]
id = 19225
[taxonomies]
categories = ["task"]
tags = []
+++

## Task


:<cite>In algebra, [[wp:Synthetic division|polynomial synthetic division]] is an algorithm for dividing a polynomial by another polynomial of the same or lower degree in an efficient way using a trick involving clever manipulations of coefficients, which results in a lower time complexity than [[polynomial long division]].</cite>




## C#
{{trans|Java}}

```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace SyntheticDivision
{
    class Program
    {
        static (List<int>,List<int>) extendedSyntheticDivision(List<int> dividend, List<int> divisor)
        {
            List<int> output = dividend.ToList();
            int normalizer = divisor[0];

            for (int i = 0; i < dividend.Count() - (divisor.Count() - 1); i++)
            {
                output[i] /= normalizer;

                int coef = output[i];
                if (coef != 0)
                {
                    for (int j = 1; j < divisor.Count(); j++)
                        output[i + j] += -divisor[j] * coef;
                }
            }

            int separator = output.Count() - (divisor.Count() - 1);

            return (
                output.GetRange(0, separator),
                output.GetRange(separator, output.Count() - separator)
            );
        }

        static void Main(string[] args)
        {
            List<int> N = new List<int>{ 1, -12, 0, -42 };
            List<int> D = new List<int> { 1, -3 };

            var (quotient, remainder) = extendedSyntheticDivision(N, D);
            Console.WriteLine("[ {0} ] / [ {1} ] = [ {2} ], remainder [ {3} ]" ,
                string.Join(",", N),
                string.Join(",", D),
                string.Join(",", quotient),
                string.Join(",", remainder)
            );
        }
    }
}

```


## Go

{{trans|Python}}

```go
package main

import (
    "fmt"
    "math/big"
)

func div(dividend, divisor []*big.Rat) (quotient, remainder []*big.Rat) {
    out := make([]*big.Rat, len(dividend))
    for i, c := range dividend {
        out[i] = new(big.Rat).Set(c)
    }
    for i := 0; i < len(dividend)-(len(divisor)-1); i++ {
        out[i].Quo(out[i], divisor[0])
        if coef := out[i]; coef.Sign() != 0 {
            var a big.Rat
            for j := 1; j < len(divisor); j++ {
                out[i+j].Add(out[i+j], a.Mul(a.Neg(divisor[j]), coef))
            }
        }
    }
    separator := len(out) - (len(divisor) - 1)
    return out[:separator], out[separator:]
}

func main() {
    N := []*big.Rat{
        big.NewRat(1, 1),
        big.NewRat(-12, 1),
        big.NewRat(0, 1),
        big.NewRat(-42, 1)}
    D := []*big.Rat{big.NewRat(1, 1), big.NewRat(-3, 1)}
    Q, R := div(N, D)
    fmt.Printf("%v / %v = %v remainder %v\n", N, D, Q, R)
}
```

{{out}}

```txt

[1/1 -12/1 0/1 -42/1] / [1/1 -3/1] = [1/1 -9/1 -27/1] remainder [-123/1]

```



## J


Solving this the easy way:


```J
   psd=: [:(}. ;{.) ([ (] -/@,:&}. (* {:)) ] , %&{.~)^:(>:@-~&#)~
```


Task example:


```J
   (1, (-12), 0, -42) psd (1, -3)
┌────────┬────┐
│1 _9 _27│_123│
└────────┴────┘

```



## Java

{{trans|Python}}

```java
import java.util.Arrays;

public class Test {

    public static void main(String[] args) {
        int[] N = {1, -12, 0, -42};
        int[] D = {1, -3};

        System.out.printf("%s / %s = %s",
                Arrays.toString(N),
                Arrays.toString(D),
                Arrays.deepToString(extendedSyntheticDivision(N, D)));
    }

    static int[][] extendedSyntheticDivision(int[] dividend, int[] divisor) {
        int[] out = dividend.clone();
        int normalizer = divisor[0];

        for (int i = 0; i < dividend.length - (divisor.length - 1); i++) {
            out[i] /= normalizer;

            int coef = out[i];
            if (coef != 0) {
                for (int j = 1; j < divisor.length; j++)
                    out[i + j] += -divisor[j] * coef;
            }
        }

        int separator = out.length - (divisor.length - 1);

        return new int[][]{
            Arrays.copyOfRange(out, 0, separator),
            Arrays.copyOfRange(out, separator, out.length)
        };
    }
}
```



```txt
[1, -12, 0, -42] / [1, -3] = [[1, -9, -27], [-123]]
```



## Kotlin

{{trans|Python}}

```scala
// version 1.1.2

fun extendedSyntheticDivision(dividend: IntArray, divisor: IntArray): Pair<IntArray, IntArray> {
    val out = dividend.copyOf()
    val normalizer = divisor[0]
    val separator = dividend.size - divisor.size + 1
    for (i in 0 until separator) {
        out[i] /= normalizer
        val coef = out[i]
        if (coef != 0) {
            for (j in 1 until divisor.size) out[i + j] += -divisor[j] * coef
        }
    }
    return out.copyOfRange(0, separator) to out.copyOfRange(separator, out.size)
}

fun main(args: Array<String>) {
    println("POLYNOMIAL SYNTHETIC DIVISION")
    val n = intArrayOf(1, -12, 0, -42)
    val d = intArrayOf(1, -3)
    val (q, r) = extendedSyntheticDivision(n, d)
    print("${n.contentToString()} / ${d.contentToString()}  =  ")
    println("${q.contentToString()}, remainder ${r.contentToString()}")
    println()
    val n2 = intArrayOf(1, 0, 0, 0, -2)
    val d2 = intArrayOf(1, 1, 1, 1)
    val (q2, r2) = extendedSyntheticDivision(n2, d2)
    print("${n2.contentToString()} / ${d2.contentToString()}  =  ")
    println("${q2.contentToString()}, remainder ${r2.contentToString()}")
}
```


{{out}}

```txt

POLYNOMIAL SYNTHETIC DIVISION
[1, -12, 0, -42] / [1, -3]  =  [1, -9, -27], remainder [-123]

[1, 0, 0, 0, -2] / [1, 1, 1, 1]  =  [1, -1], remainder [0, 0, -1]

```



## Perl

{{trans|Perl 6}}

```perl
sub synthetic_division {
    my($numerator,$denominator) = @_;
    my @result = @$numerator;
    my $end    = @$denominator-1;

    for my $i (0 .. @$numerator-($end+1)) {
        next unless $result[$i];
        $result[$i]    /= @$denominator[0];
        $result[$i+$_] -= @$denominator[$_] * $result[$i] for 1 .. $end;
    }

    return join(' ', @result[0 .. @result-($end+1)]), join(' ', @result[-$end .. -1]);
}

sub poly_divide {
    *n = shift; *d = shift;
    my($quotient,$remainder)= synthetic_division( \@n, \@d );
    "[@n] / [@d] = [$quotient], remainder [$remainder]\n";
}

print poly_divide([1, -12, 0, -42], [1, -3]);
print poly_divide([1, 0, 0, 0, -2], [1, 1, 1, 1]);
```

{{out}}

```txt
[1 -12 0 -42] / [1 -3] = [1 -9 -27], remainder [-123]
[1 0 0 0 -2] / [1 1 1 1] = [1 -1], remainder [0 0 -1]
```



## Perl 6

{{trans|Python}}
{{works with|Rakudo|2018.09}}


```perl6
sub synthetic-division ( @numerator, @denominator ) {
    my @result = @numerator;
    my $end    = @denominator.end;

    for ^(@numerator-$end) -> $i {
        @result[$i]    /= @denominator[0];
        @result[$i+$_] -= @denominator[$_] * @result[$i] for 1..$end;
    }

    'quotient' => @result[0 ..^ *-$end],
    'remainder' => @result[*-$end .. *];
}

my @tests =
[1, -12, 0, -42], [1, -3],
[1, 0, 0, 0, -2], [1, 1, 1, 1];

for @tests -> @n, @d {
    my %result = synthetic-division( @n, @d );
    say "[{@n}] / [{@d}] = [%result<quotient>], remainder [%result<remainder>]";
}
```

{{out}}

```txt
[1 -12 0 -42] / [1 -3] = [1 -9 -27], remainder [-123]
[1 0 0 0 -2] / [1 1 1 1] = [1 -1], remainder [0 0 -1]

```



## Phix

{{trans|Kotlin}}

```Phix
function extendedSyntheticDivision(sequence dividend, divisor)
    sequence out = dividend
    integer normalizer = divisor[1]
    integer separator = length(dividend) - length(divisor) + 1
    for i=1 to separator do
        out[i] /= normalizer
        integer coef = out[i]
        if (coef != 0) then
            for j=2 to length(divisor) do out[i+j-1] += -divisor[j] * coef end for
        end if
    end for
    return {out[1..separator],out[separator+1..$]}
end function

constant tests = {{{1, -12, 0, -42},{1, -3}},
                  {{1, 0, 0, 0, -2},{1, 1, 1, 1}}}

printf(1,"Polynomial synthetic division\n")
for t=1 to length(tests) do
    sequence {n,d} = tests[t],
             {q,r} = extendedSyntheticDivision(n, d)
    printf(1,"%v / %v  =  %v, remainder %v\n",{n,d,q,r})
end for
```

{{out}}

```txt

Polynomial synthetic division
{1,-12,0,-42} / {1,-3}  =  {1,-9,-27}, remainder {-123}
{1,0,0,0,-2} / {1,1,1,1}  =  {1,-1}, remainder {0,0,-1}

```



## Python

Here is an extended synthetic division algorithm, which means that it supports a divisor polynomial (instead of just a monomial/binomial). It also supports non-monic polynomials (polynomials which first coefficient is different than 1). Polynomials are represented by lists of coefficients with decreasing degree (left-most is the major degree , right-most is the constant).
{{works with|Python 2.x}}

```python
# -*- coding: utf-8 -*-

def extended_synthetic_division(dividend, divisor):
    '''Fast polynomial division by using Extended Synthetic Division. Also works with non-monic polynomials.'''
    # dividend and divisor are both polynomials, which are here simply lists of coefficients. Eg: x^2 + 3x + 5 will be represented as [1, 3, 5]

    out = list(dividend) # Copy the dividend
    normalizer = divisor[0]
    for i in xrange(len(dividend)-(len(divisor)-1)):
        out[i] /= normalizer # for general polynomial division (when polynomials are non-monic),
                                 # we need to normalize by dividing the coefficient with the divisor's first coefficient
        coef = out[i]
        if coef != 0: # useless to multiply if coef is 0
            for j in xrange(1, len(divisor)): # in synthetic division, we always skip the first coefficient of the divisor,
                                              # because it's only used to normalize the dividend coefficients
                out[i + j] += -divisor[j] * coef

    # The resulting out contains both the quotient and the remainder, the remainder being the size of the divisor (the remainder
    # has necessarily the same degree as the divisor since it's what we couldn't divide from the dividend), so we compute the index
    # where this separation is, and return the quotient and remainder.
    separator = -(len(divisor)-1)
    return out[:separator], out[separator:] # return quotient, remainder.

if __name__ == '__main__':
    print "POLYNOMIAL SYNTHETIC DIVISION"
    N = [1, -12, 0, -42]
    D = [1, -3]
    print "  %s / %s =" % (N,D),
    print " %s remainder %s" % extended_synthetic_division(N, D)

```


Sample output:

```txt

POLYNOMIAL SYNTHETIC DIVISION
  [1, -12, 0, -42] / [1, -3] =  [1, -9, -27] remainder [-123]

```



## Racket

{{trans|Python}}


```racket
#lang racket/base
(require racket/list)
;; dividend and divisor are both polynomials, which are here simply lists of coefficients.
;; Eg: x^2 + 3x + 5 will be represented as (list 1 3 5)
(define (extended-synthetic-division dividend divisor)
  (define out (list->vector dividend)) ; Copy the dividend
  ;; for general polynomial division (when polynomials are non-monic), we need to normalize by
  ;; dividing the coefficient with the divisor's first coefficient
  (define normaliser (car divisor))
  (define divisor-length (length divisor)) ; } we use these often enough
  (define out-length (vector-length out))  ; }

  (for ((i (in-range 0 (- out-length divisor-length -1))))
    (vector-set! out i (quotient (vector-ref out i) normaliser))
    (define coef (vector-ref out i))
    (unless (zero? coef) ; useless to multiply if coef is 0
      (for ((i+j (in-range (+ i 1)                ; in synthetic division, we always skip the first
                           (+ i divisor-length))) ; coefficient of the divisior, because it's
            (divisor_j (in-list (cdr divisor))))  ;  only used to normalize the dividend coefficients
        (vector-set! out i+j (+ (vector-ref out i+j) (* coef divisor_j -1))))))
  ;; The resulting out contains both the quotient and the remainder, the remainder being the size of
  ;; the divisor (the remainder has necessarily the same degree as the divisor since it's what we
  ;; couldn't divide from the dividend), so we compute the index where this separation is, and return
  ;; the quotient and remainder.

  ;; return quotient, remainder (conveniently like quotient/remainder)
  (split-at (vector->list out) (- out-length (sub1 divisor-length))))

(module+ main
  (displayln "POLYNOMIAL SYNTHETIC DIVISION")
  (define N '(1 -12 0 -42))
  (define D '(1 -3))
  (define-values (Q R) (extended-synthetic-division N D))
  (printf "~a / ~a = ~a remainder ~a~%" N D Q R))
```


{{out}}


```txt
POLYNOMIAL SYNTHETIC DIVISION
(1 -12 0 -42) / (1 -3) = (1 -9 -27) remainder (-123)
```



## REXX


```rexx
/* REXX Polynomial Division                */
/* extended to support order of divisor >1 */
call set_dd '1 0 0 0 -1'
Call set_dr '1 1 1 1'
Call set_dd '1 -12 0 -42'
Call set_dr '1 -3'
q.0=0
Say list_dd '/' list_dr
do While dd.0>=dr.0
  q=dd.1/dr.1
  Do j=1 To dr.0
    dd.j=dd.j-q*dr.j
    End
  Call set_q q
  Call shift_dd
  End
say 'Quotient:' mk_list_q() 'Remainder:' mk_list_dd()
Exit

set_dd:
Parse Arg list
list_dd='['
Do i=1 To words(list)
  dd.i=word(list,i)
  list_dd=list_dd||dd.i','
  End
dd.0=i-1
list_dd=left(list_dd,length(list_dd)-1)']'
Return

set_dr:
Parse Arg list
list_dr='['
Do i=1 To words(list)
  dr.i=word(list,i)
  list_dr=list_dr||dr.i','
  End
dr.0=i-1
list_dr=left(list_dr,length(list_dr)-1)']'
Return

set_q:
z=q.0+1
q.z=arg(1)
q.0=z
Return

shift_dd:
Do i=2 To dd.0
  ia=i-1
  dd.ia=dd.i
  End
dd.0=dd.0-1
Return

mk_list_q:
list='['q.1''
Do i=2 To q.0
  list=list','q.i
  End
Return list']'

mk_list_dd:
list='['dd.1''
Do i=2 To dd.0
  list=list','dd.i
  End
Return list']'


```

{{out}}

```txt
[1,-12,0,-42] / [1,-3]
Quotient: [1,-9,-27] Remainder: -123

[1,0,0,0,-2] / [1,1,1,1]
Quotient: [1,-1] Remainder: [0,0,-1]
```



## Scala


### Java Interoperability

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/59vpjcQ/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/uUk8yRPnQdGdS1aAUFjhmA Scastie (remote JVM)].

```Scala
import java.util

object PolynomialSyntheticDivision extends App {

  val N: Array[Int] = Array(1, -12, 0, -42)
  val D: Array[Int] = Array(1, -3)

  def extendedSyntheticDivision(dividend: Array[Int],
                                divisor: Array[Int]): Array[Array[Int]] = {
    val out = dividend.clone
    val normalizer = divisor(0)

    for (i <- 0 until dividend.length - (divisor.length - 1)) {
      out(i) /= normalizer
      val coef = out(i)
      if (coef != 0)
        for (j <- 1 until divisor.length) out(i + j) += -divisor(j) * coef
    }
    val separator = out.length - (divisor.length - 1)
    Array[Array[Int]](util.Arrays.copyOfRange(out, 0, separator),
      util.Arrays.copyOfRange(out, separator, out.length))
  }

  println(f"${util.Arrays.toString(N)}%s / ${util.Arrays.toString(D)}%s = ${
    util.Arrays
      .deepToString(extendedSyntheticDivision(N, D).asInstanceOf[Array[AnyRef]])
  }%s")

}
```


## Sidef

{{trans|Python}}

```ruby
func extended_synthetic_division(dividend, divisor) {
    var end = divisor.end
    var out = dividend.clone
    var normalizer = divisor[0]

    for i in ^(dividend.len - end) {
        out[i] /= normalizer
        var coef = out[i]
        if (coef != 0) {
            for j in (1 .. end) {
                out[i+j] += -(divisor[j] * coef)
            }
        }
    }

    var remainder = out.splice(-end)
    var quotient = out

    return(quotient, remainder)
}

var (n, d) = ([1, -12, 0, -42], [1, -3])
print("  %s / %s =" % (n, d))
print(" %s remainder %s\n" % extended_synthetic_division(n, d))
```

{{out}}
  [1, -12, 0, -42] / [1, -3] = [1, -9, -27] remainder [-123]


## Tcl

{{trans|Python}}

This uses a common utility proc <tt>range</tt>, and a less common one called <tt>lincr</tt>, which increments elements of lists.  The routine for polynomial division is placed in a namespace ensemble, such that it can be conveniently shared with other commands for polynomial arithmetic (eg <tt>polynomial multiply</tt>).


```Tcl
#   range ?start? end+1
# start defaults to 0:  [range 5] = {0 1 2 3 4}
proc range {a {b ""}} {
    if {$b eq ""} {
        set b $a
        set a 0
    }
    for {set r {}} {$a<$b} {incr a} {
        lappend r $a
    }
    return $r
}

#   lincr list idx ?...? increment
# By analogy with [lset] and [incr]:
# Adds incr to the item at [lindex list idx ?...?].  incr may be a float.
proc lincr {_ls args} {
    upvar 1 $_ls ls
    set incr [lindex $args end]
    set idxs [lrange $args 0 end-1]
    lset ls {*}$idxs [expr {$incr + [lindex $ls {*}$idxs]}]
}

namespace eval polynomial {
    # polynomial division, returns [list $dividend $remainder]
    proc divide {top btm} {
        set out $top
        set norm [lindex $btm 0]
        foreach i [range [expr {[llength $top] - [llength $btm] + 1}]] {
            lset out $i [set coef [expr {[lindex $out $i] * 1.0 / $norm}]]
            if {$coef != 0} {
                foreach j [range 1 [llength $btm]] {
                    lincr out [expr {$i+$j}] [expr {-[lindex $btm $j] * $coef}]
                }
            }
        }
        set terms [expr {[llength $btm]-1}]
        list [lrange $out 0 end-$terms] [lrange $out end-[incr terms -1] end]
    }
    namespace export *
    namespace ensemble create
}

proc test {} {
    set top {1 -12 0 -42}
    set btm {1 -3}
    set div [polynomial divide $top $btm]
    puts "$top / $btm = $div"
}
test
```


{{out}}

```txt
1 -12 0 -42 / 1 -3 = {1.0 -9.0 -27.0} -123.0
```



## zkl

{{trans|Python}}

```zkl
fcn extended_synthetic_division(dividend, divisor){
# Fast polynomial division by using Extended Synthetic Division. Also works with non-monic polynomials.
# dividend and divisor are both polynomials, which are here simply lists of coefficients. Eg: x^2 + 3x + 5 will be represented as [1, 3, 5]
   out,normalizer:=dividend.copy(), divisor[0];
   foreach i in (dividend.len() - (divisor.len() - 1)){
      out[i] /= normalizer; # for general polynomial division (when polynomials are non-monic),
                            # we need to normalize by dividing the coefficient with the divisor's first coefficient
      coef := out[i];
      if(coef != 0){  # useless to multiply if coef is 0
	 foreach j in ([1..divisor.len() - 1]){ # in synthetic division, we always skip the first coefficient of the divisior,
	    out[i + j] += -divisor[j] * coef;   # because it's only used to normalize the dividend coefficients
	 }
      }
   }

    # out contains the quotient and remainder, the remainder being the size of the divisor (the remainder
    # has necessarily the same degree as the divisor since it's what we couldn't divide from the dividend), so we compute the index
    # where this separation is, and return the quotient and remainder.
   separator := -(divisor.len() - 1);
   return(out[0,separator], out[separator,*]) # return quotient, remainder.
}
```


```zkl
println("POLYNOMIAL SYNTHETIC DIVISION");
N,D := T(1, -12, 0, -42), T(1, -3);
print("  %s / %s =".fmt(N,D));
println(" %s remainder %s".fmt(extended_synthetic_division(N,D).xplode()));
```

{{out}}

```txt

POLYNOMIAL SYNTHETIC DIVISION
  L(1,-12,0,-42) / L(1,-3) = L(1,-9,-27) remainder L(-123)

```
