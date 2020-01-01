+++
title = "Numerical integration/Adaptive Simpson's method"
description = ""
date = 2019-06-25T17:01:18Z
aliases = []
[extra]
id = 22011
[taxonomies]
categories = []
tags = []
+++

{{draft task|Arithmetic operations}}
Lychee (1969)'s Modified [[wp:Adaptive_Simpson's_method|Adaptive Simpson's method]] (doi:10.1145/321526.321537) is a numerical quadrature method that recursively bisects the interval until the precision is high enough.

{| class="mw-collapsible"
|+ Pseudocode: Simpson's method, adaptive
|-
|
 ''; Lychee's ASR, Modifications 1, 2, 3''
 '''procedure''' _quad_asr_simpsons(f, a, fa, b, fb)
     m := (a + b) / 2
     fm := f(m)
     h := b - a

     '''return multiple''' [m, fm, (h / 6) * (f(a) + f(b) + 4*sum1 + 2*sum2)]

 '''procedure''' _quad_asr(f, a, fa, b, fb, tol, whole, m, fm, depth)
     lm, flm, left  := _quad_asr_simpsons(f, a, fa, m, fm)
     rm, frm, right := _quad_asr_simpsons(f, m, fm, b, fb)
     delta := left + right - whole

     tol' := tol / 2
     '''if''' depth <= 0 ''or'' tol' == tol ''or'' abs(delta) <= 15 * tol:
         '''return''' left + right + delta / 15
     '''else''':
         '''return''' _quad_asr(f, a, fa, m, fm, tol', left , lm, flm, depth - 1) +
                _quad_asr(f, m, fm, b, fb, tol', right, rm, frm, depth - 1)

 '''procedure''' quad_asr(f, a, b, tol, depth)
    fa := f(a)
    fb := f(b)
    m, fm, whole := _quad_asr_simpsons(f, a, fa, b, fb)
    '''return''' _quad_asr(f, a, fa, b, fb, tol, whole, m, fm, depth)
|}


## C

{{trans|zkl}}

```c
#include <stdio.h>
#include <math.h>

typedef struct { double m; double fm; double simp; } triple;

/* "structured" adaptive version, translated from Racket */
triple _quad_simpsons_mem(double (*f)(double), double a, double fa, double b, double fb) {
    // Evaluates Simpson's Rule, also returning m and f(m) to reuse.
    double m = (a + b) / 2;
    double fm = f(m);
    double simp = fabs(b - a) / 6 * (fa + 4*fm + fb);
    triple t = {m, fm, simp};
    return t;
}

double _quad_asr(double (*f)(double), double a, double fa, double b, double fb, double eps, double whole, double m, double fm) {
    // Efficient recursive implementation of adaptive Simpson's rule.
    // Function values at the start, middle, end of the intervals are retained.
    triple lt = _quad_simpsons_mem(f, a, fa, m, fm);
    triple rt = _quad_simpsons_mem(f, m, fm, b, fb);
    double delta = lt.simp + rt.simp - whole;
    if (fabs(delta) <= eps * 15) return lt.simp + rt.simp + delta/15;
    return _quad_asr(f, a, fa, m, fm, eps/2, lt.simp, lt.m, lt.fm) +
           _quad_asr(f, m, fm, b, fb, eps/2, rt.simp, rt.m, rt.fm);
}

double quad_asr(double (*f)(double), double a, double b, double eps) {
    // Integrate f from a to b using ASR with max error of eps.
    double fa = f(a);
    double fb = f(b);
    triple t = _quad_simpsons_mem(f, a, fa, b, fb);
    return _quad_asr(f, a, fa, b, fb, eps, t.simp, t.m, t.fm);
}

int main(){
    double a = 0.0, b = 1.0;
    double sinx = quad_asr(sin, a, b, 1e-09);
    printf("Simpson's integration of sine from %g to %g = %f\n", a, b, sinx);
    return 0;
}
```


{{out}}

```txt

Simpson's integration of sine from 0 to 1 = 0.459698

```



## Factor

{{trans|Julia}}

```factor
USING: formatting kernel locals math math.functions math.ranges
sequences ;
IN: rosetta-code.simpsons

:: simps ( f a b n -- x )
    n even?
    [ n "n must be even; %d was given" sprintf throw ] unless
    b a - n / :> h
    1 n 2 <range> 2 n 1 - 2 <range>
    [ [ a + h * f call ] map-sum ] bi@ [ 4 ] [ 2 ] bi*
    [ * ] 2bi@ a b [ f call ] bi@ + + + h 3 / * ; inline

[ sin ] 0 1 100 simps
"Simpson's rule integration of sin from 0 to 1 is: %u\n" printf
```

{{out}}

```txt

Simpson's rule integration of sin from 0 to 1 is: 0.4596976941573994

```



## Go

Like the zkl entry, this is also a translation of the Python code in the Wikipedia article.

```go
package main

import (
    "fmt"
    "math"
)

type F = func(float64) float64

/* "structured" adaptive version, translated from Racket */
func quadSimpsonsMem(f F, a, fa, b, fb float64) (m, fm, simp float64) {
    // Evaluates Simpson's Rule, also returning m and f(m) to reuse.
    m = (a + b) / 2
    fm = f(m)
    simp = math.Abs(b-a) / 6 * (fa + 4*fm + fb)
    return
}

func quadAsrRec(f F, a, fa, b, fb, eps, whole, m, fm float64) float64 {
    // Efficient recursive implementation of adaptive Simpson's rule.
    // Function values at the start, middle, end of the intervals are retained.
    lm, flm, left := quadSimpsonsMem(f, a, fa, m, fm)
    rm, frm, right := quadSimpsonsMem(f, m, fm, b, fb)
    delta := left + right - whole
    if math.Abs(delta) <= eps*15 {
        return left + right + delta/15
    }
    return quadAsrRec(f, a, fa, m, fm, eps/2, left, lm, flm) +
        quadAsrRec(f, m, fm, b, fb, eps/2, right, rm, frm)
}

func quadAsr(f F, a, b, eps float64) float64 {
    // Integrate f from a to b using ASR with max error of eps.
    fa, fb := f(a), f(b)
    m, fm, whole := quadSimpsonsMem(f, a, fa, b, fb)
    return quadAsrRec(f, a, fa, b, fb, eps, whole, m, fm)
}

func main() {
    a, b := 0.0, 1.0
    sinx := quadAsr(math.Sin, a, b, 1e-09)
    fmt.Printf("Simpson's integration of sine from %g to %g = %f\n", a, b, sinx)
}
```


{{out}}

```txt

Simpson's integration of sine from 0 to 1 = 0.459698

```



## J

Typically one would choose the library implementation:

```txt

   load'~addons/math/misc/integrat.ijs'

   NB. integrate returns definite integral and estimated digits of accuracy
   1&o. integrate 0 1
0.459698 9

   NB. adapt implements adaptive Simpson's rule, however recomputes some integrands
   1&o. adapt 0 1 1e_9
0.459698


```




```J

Note'expected answer computed by j www.jsoftware.com'

       1-&:(1&o.d._1)0
    0.459698

    translated from c
)

mp=: +/ .*  NB. matrix product

NB. Evaluates Simpson's Rule, also returning m and f(m) to reuse.
uquad_simpsons_mem=: adverb define
 'a fa b fb'=. y
 em=. a ([ + [: -: -~) b
 fm=. u em
 simp=. ((| b - a) % 6) * 1 4 1 mp fa , fm , fb
 em, fm, simp
)

Simp=: 1 :'2{m'
Fm=: 1 :'1{m'
M=: 1 :'0{m'

NB. Efficient recursive implementation of adaptive Simpson's rule.
NB. Function values at the start, middle, end of the intervals are retained.
uquad_asr=: adverb define
 'a fa b fb eps whole em fm'=. y
 lt=. u uquad_simpsons_mem(a, fa, em, fm)
 rt=. u uquad_simpsons_mem(em, fm, b, fb)
 delta=. lt Simp + rt Simp - whole
 if. (| delta) <: eps * 15 do.
  lt Simp + rt Simp + delta % 15
 else.
  (a, fa, em, fm, (-: eps), lt Simp, lt M, lt Fm) +&(u uquad_asr) (em, fm, b, fb, (-: eps), rt Simp, rt M, rt Fm)
 end.
)

NB. Integrate u from a to b using ASR with max error of eps.
quad_asr=: adverb define
 'a b eps'=. y
 fa=. u a
 fb=. u b
 t=. u uquad_simpsons_mem a, fa, b, fb
 u uquad_asr a, fa, b, fb, eps, t Simp, t M, t Fm
)

```



```txt

   echo 'Simpson''s integration of sine from 0 to 1 = ' , ": 1&o. quad_asr 0 1 1e_9
Simpson's integration of sine from 0 to 1 = 0.459698

```



## Julia

Originally from Modesto Mas, https://mmas.github.io/simpson-integration-julia

```julia
function simps(f::Function, a::Number, b::Number, n::Number)
    iseven(n) || throw("n must be even, and $n was given")
    h = (b-a)/n
    s = f(a) + f(b)
    s += 4 * sum(f.(a .+ collect(1:2:n) .* h))
    s += 2 * sum(f.(a .+ collect(2:2:n-1) .* h))
    h/3 * s
end

println("Simpson's rule integration of sin from 0 to 1 is: ",  simps(sin, 0.0, 1.0, 100))

```
{{out}}

```txt

Simpson's rule integration of sin from 0 to 1 is: 0.45969769415739936

```



## Kotlin

{{trans|Go}}

```scala
// Version 1.2.71

import kotlin.math.abs
import kotlin.math.sin

typealias F = (Double) -> Double
typealias T = Triple<Double, Double, Double>

/* "structured" adaptive version, translated from Racket */
fun quadSimpsonsMem(f: F, a: Double, fa: Double, b: Double, fb: Double): T {
    // Evaluates Simpson's Rule, also returning m and f(m) to reuse
    val m = (a + b) / 2
    val fm = f(m)
    val simp = abs(b - a) / 6 * (fa + 4 * fm + fb)
    return T(m, fm, simp)
}

fun quadAsrRec(f: F, a: Double, fa: Double, b: Double, fb: Double,
    eps: Double, whole: Double, m: Double, fm: Double): Double {
    // Efficient recursive implementation of adaptive Simpson's rule.
    // Function values at the start, middle, end of the intervals are retained.
    val (lm, flm, left) = quadSimpsonsMem(f, a, fa, m, fm)
    val (rm, frm, right) = quadSimpsonsMem(f, m, fm, b, fb)
    val delta = left + right - whole
    if (abs(delta) <= eps * 15)  return left + right + delta / 15
    return quadAsrRec(f, a, fa, m, fm, eps / 2, left, lm, flm) +
        quadAsrRec(f, m, fm, b, fb, eps / 2, right, rm, frm)
}

fun quadAsr(f: F, a: Double, b: Double, eps: Double): Double {
    // Integrate f from a to b using ASR with max error of eps.
    val fa = f(a)
    val fb = f(b)
    val (m, fm, whole) = quadSimpsonsMem(f, a, fa, b, fb)
    return quadAsrRec(f, a, fa, b, fb, eps, whole, m, fm)
}

fun main(args: Array<String>) {
    val a = 0.0
    val b = 1.0
    val sinx = quadAsr(::sin, a, b, 1.0e-09)
    println("Simpson's integration of sine from $a to $b = ${"%6f".format(sinx)}")
}
```


{{output}}

```txt

Simpson's integration of sine from 0.0 to 1.0 = 0.459698

```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;

sub adaptive_Simpson_quadrature {
    my($f, $left, $right, $eps) = @_;
    my $lf = eval "$f($left)";
    my $rf = eval "$f($right)";
    my ($mid, $midf, $whole) = Simpson_quadrature_mid($f, $left, $lf, $right, $rf);
    return recursive_Simpsons_asr($f, $left, $lf, $right, $rf, $eps, $whole, $mid, $midf);

    sub Simpson_quadrature_mid {
        my($g, $l, $lf, $r, $rf) = @_;
        my $mid = ($l + $r) / 2;
        my $midf = eval "$g($mid)";
        ($mid, $midf, abs($r - $l) / 6 * ($lf + 4 * $midf + $rf))
    }

    sub recursive_Simpsons_asr {
        my($h, $a, $fa, $b, $fb, $eps, $whole, $m, $fm) = @_;
        my ($lm, $flm, $left)  = Simpson_quadrature_mid($h, $a, $fa, $m, $fm);
        my ($rm, $frm, $right) = Simpson_quadrature_mid($h, $m, $fm, $b, $fb);
        my $delta = $left + $right - $whole;
        abs($delta) <= 15 * $eps
            ? $left + $right + $delta / 15
            : recursive_Simpsons_asr($h, $a, $fa, $m, $fm, $eps/2, $left,  $lm, $flm) +
              recursive_Simpsons_asr($h, $m, $fm, $b, $fb, $eps/2, $right, $rm, $frm)
    }
}

my ($a, $b) = (0, 1);
my $sin = adaptive_Simpson_quadrature('sin', $a, $b, 1e-9);
printf "Simpson's integration of sine from $a to $b = %.9f", $sin
```

{{out}}

```txt
Simpson's integration of sine from 0 to 1 = 0.459697694
```



## Perl 6

{{works with|Rakudo|2018.10}}
Fairly direct translation of the Python code.


```perl6
sub adaptive-Simpson-quadrature(&f, $left, $right, \ε = 1e-9) {
    my $lf = f($left);
    my $rf = f($right);
    my ($mid, $midf, $whole) = Simpson-quadrature-mid(&f, $left, $lf, $right, $rf);
    return recursive-Simpsons-asr(&f, $left, $lf, $right, $rf, ε, $whole, $mid, $midf);

    sub Simpson-quadrature-mid(&g, $l, $lf, $r, $rf){
        my $mid = ($l + $r) / 2;
        my $midf = g($mid);
        ($mid, $midf, ($r - $l).abs / 6 * ($lf + 4 * $midf + $rf))
    }

    sub recursive-Simpsons-asr(&h, $a, $fa, $b, $fb, $eps, $whole, $m, $fm){
        my ($lm, $flm, $left)  = Simpson-quadrature-mid(&h, $a, $fa, $m, $fm);
        my ($rm, $frm, $right) = Simpson-quadrature-mid(&h, $m, $fm, $b, $fb);
        my $delta = $left + $right - $whole;
        $delta.abs <= 15 * $eps
            ?? $left + $right + $delta / 15
            !! recursive-Simpsons-asr(&h, $a, $fa, $m, $fm, $eps/2, $left,  $lm, $flm) +
               recursive-Simpsons-asr(&h, $m, $fm, $b, $fb, $eps/2, $right, $rm, $frm)
    }
}

my ($a, $b) = 0e0, 1e0;
my $sin = adaptive-Simpson-quadrature(&sin, $a, $b, 1e-9).round(10**-9);;
say "Simpson's integration of sine from $a to $b = $sin";
```

{{out}}

```txt
Simpson's integration of sine from 0 to 1 = 0.459697694
```



## Phix

{{trans|Go}}

```Phix
function quadSimpsonsMem(integer f, atom a, fa, b, fb)
    -- Evaluates Simpson's Rule, also returning m and f(m) to reuse.
    atom m = (a + b) / 2,
         fm = call_func(f,{m}),
         simp = abs(b-a) / 6 * (fa + 4*fm + fb)
    return {m, fm, simp}
end function

function quadAsrRec(integer f, atom a, fa, b, fb, eps, whole, m, fm)
    -- Efficient recursive implementation of adaptive Simpson's rule.
    -- Function values at the start, middle, end of the intervals are retained.
    atom {lm, flm, left} := quadSimpsonsMem(f, a, fa, m, fm),
         {rm, frm, rght} := quadSimpsonsMem(f, m, fm, b, fb),
         delta := left + rght - whole
    if abs(delta) <= eps*15 then
        return left + rght + delta/15
    end if
    return quadAsrRec(f, a, fa, m, fm, eps/2, left, lm, flm) +
           quadAsrRec(f, m, fm, b, fb, eps/2, rght, rm, frm)
end function

function quadAsr(integer f, atom a, b, eps)
    -- Integrate f from a to b using ASR with max error of eps.
    atom fa := call_func(f,{a}),
         fb := call_func(f,{b}),
         {m, fm, whole} := quadSimpsonsMem(f, a, fa, b, fb)
    return quadAsrRec(f, a, fa, b, fb, eps, whole, m, fm)
end function

-- we need a mini wrapper to get a routine_id for sin()
-- (because sin() is implemented in low-level assembly)
function _sin(atom a)
    return sin(a)
end function

atom a := 0.0, b := 1.0,
     sinx := quadAsr(routine_id("_sin"), a, b, 1e-09)
printf(1,"Simpson's integration of sine from %g to %g = %f\n", {a, b, sinx})
```

{{out}}

```txt

Simpson's integration of sine from 0 to 1 = 0.459698

```



## Python


```Python

#! python3

'''
    example

    $ python3 /tmp/integrate.py
    Simpson's integration of sine from 0.0 to 1.0 = 0.4596976941317858

    expected answer computed by j www.jsoftware.com

       1-&:(1&o.d._1)0
    0.459698


    translated from c
'''

import math

import collections
triple = collections.namedtuple('triple', 'm fm simp')

def _quad_simpsons_mem(f: callable, a: float , fa: float, b: float, fb: float)->tuple:
    '''
        Evaluates Simpson's Rule, also returning m and f(m) to reuse.
    '''
    m = a + (b - a) / 2
    fm = f(m)
    simp = abs(b - a) / 6 * (fa + 4*fm + fb)
    return triple(m, fm, simp,)

def _quad_asr(f: callable, a: float, fa: float, b: float, fb: float, eps: float, whole: float, m: float, fm: float)->float:
    '''
    	Efficient recursive implementation of adaptive Simpson's rule.
    	Function values at the start, middle, end of the intervals are retained.
    '''
    lt = _quad_simpsons_mem(f, a, fa, m, fm)
    rt = _quad_simpsons_mem(f, m, fm, b, fb)
    delta = lt.simp + rt.simp - whole
    return (lt.simp + rt.simp + delta/15
        if (abs(delta) <= eps * 15) else
            _quad_asr(f, a, fa, m, fm, eps/2, lt.simp, lt.m, lt.fm) +
            _quad_asr(f, m, fm, b, fb, eps/2, rt.simp, rt.m, rt.fm)
    )

def quad_asr(f: callable, a: float, b: float, eps: float)->float:
    '''
        Integrate f from a to b using ASR with max error of eps.
    '''
    fa = f(a)
    fb = f(b)
    t = _quad_simpsons_mem(f, a, fa, b, fb)
    return _quad_asr(f, a, fa, b, fb, eps, t.simp, t.m, t.fm)

def main():
    (a, b,) = (0.0, 1.0,)
    sinx = quad_asr(math.sin, a, b, 1e-09);
    print("Simpson's integration of sine from {} to {} = {}\n".format(a, b, sinx))

main()

```



## REXX

{{trans|Go}}

```rexx
/*REXX program performs numerical integration using adaptive Simpson's method.          */
numeric digits length( pi() )  -  length(.)      /*use # of digits in pi for precision. */
a= 0;     b= 1;       f= 'SIN'                   /*define values for  A,  B,  and  F.   */
sinx= quadAsr('SIN',a,b,"1e" || (-digits() + 1) )
say "Simpson's integration of sine from "      a     ' to '      b      ' = '       sinx
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
pi:       pi= 3.14159265358979323846;  return pi /*pi  has  twenty-one  decimal digits. */
r2r:      return arg(1)  //  (pi() *2)           /*normalize radians ──► a unit circle, */
/*──────────────────────────────────────────────────────────────────────────────────────*/
quadSimp: procedure; parse arg f,a,fa,b,fb;   m= (a+b) / 2;     interpret  'fm='   f"(m)"
          simp= abs(b-a) / 6 * (fa + 4*fm + fb);    return  m  fm  simp
/*──────────────────────────────────────────────────────────────────────────────────────*/
quadAsr:  procedure; parse arg f,a,b,eps;                       interpret  'fa='   f"(a)"
                                                                interpret  'fb='   f"(b)"
          parse value  quadSimp(f,a,fa,b,fb)   with   m  fm  whole
          return quadAsrR(f,a,fa,b,fb,eps,whole,m,fm)
/*──────────────────────────────────────────────────────────────────────────────────────*/
quadAsrR: procedure; parse arg f,a,fa,b,fb,eps,whole,m,fm;      frac= digits() * 3 / 4
          parse value   quadSimp(f,a,fa,m,fm)   with  lm  flm  left
          parse value   quadSimp(f,m,fm,b,fb)   with  rm  frm  right
          $= left + right - whole                                     /*calculate delta.*/
          if abs($)<=eps*frac  then return left + right + $/frac
          return quadAsrR(f,a,fa,m,fm,eps/2, left,lm,flm) + ,
                 quadAsrR(f,m,fm,b,fb,eps/2,right,rm,frm)
/*──────────────────────────────────────────────────────────────────────────────────────*/
sin:      procedure; parse arg x;   x= r2r(x);   numeric fuzz min(5, max(1, digits() -3) )
          if x=pi*.5           then return 1;       if x==pi * 1.5  then return -1
          if abs(x)=pi | x=0   then return 0
                                           #= x;    _= x;    q= x*x
            do k=2  by 2  until p=#;    p= #;       _= - _ * q / (k * (k+1) );    #= # + _
            end   /*k*/
          return #
```

{{out|output|text=  when using the default inputs:}}

```txt

Simpson's integration of sine from  0  to  1  =  0.459697694131860282602

```



## Sidef

{{trans|Perl 6}}

```ruby
func adaptive_Simpson_quadrature(f, left, right, ε = 1e-9) {

    func quadrature_mid(l, lf, r, rf) {
        var mid = (l+r)/2
        var midf = f(mid)
        (mid, midf, abs(r-l)/6 * (lf + 4*midf + rf))
    }

    func recursive_asr(a, fa, b, fb, ε, whole, m, fm) {
        var (lm, flm, left)  = quadrature_mid(a, fa, m, fm)
        var (rm, frm, right) = quadrature_mid(m, fm, b, fb)
        var Δ = (left + right - whole)
        abs(Δ) <= 15*ε
            ? (left + right + Δ/15)
            : (__FUNC__(a, fa, m, fm, ε/2, left,  lm, flm) +
               __FUNC__(m, fm, b, fb, ε/2, right, rm, frm))
    }

    var (lf = f(left), rf = f(right))
    var (mid, midf, whole) = quadrature_mid(left, lf, right, rf)
    recursive_asr(left, lf, right, rf, ε, whole, mid, midf)
}

var (a = 0, b = 1)
var area = adaptive_Simpson_quadrature({ .sin }, a, b, 1e-15).round(-15)
say "Simpson's integration of sine from #{a} to #{b} ≈ #{area}"
```

{{out}}

```txt

Simpson's integration of sine from 0 to 1 ≈ 0.45969769413186

```



## zkl

{{trans|Python}}

```zkl
# "structured" adaptive version, translated from Racket
fcn _quad_simpsons_mem(f, a,fa, b,fb){
   #Evaluates the Simpson's Rule, also returning m and f(m) to reuse"""
   m,fm := (a + b)/2, f(m);
   return(m,fm, (b - a).abs()/6*(fa + fm*4 + fb));
}
fcn _quad_asr(f, a,fa, b,fb, eps, whole, m,fm){
  # Efficient recursive implementation of adaptive Simpson's rule.
  # Function values at the start, middle, end of the intervals are retained.

   lm,flm,left  := _quad_simpsons_mem(f, a,fa, m,fm);
   rm,frm,right := _quad_simpsons_mem(f, m,fm, b,fb);
   delta:=left + right - whole;
   if(delta.abs() <= eps*15) return(left + right + delta/15);
   _quad_asr(f, a,fa, m,fm, eps/2, left , lm,flm) +
   _quad_asr(f, m,fm, b,fb, eps/2, right, rm,frm)
}
fcn quad_asr(f,a,b,eps){
   #Integrate f from a to b using Adaptive Simpson's Rule with max error of eps
   fa,fb      := f(a),f(b);
   m,fm,whole := _quad_simpsons_mem(f, a,fa, b,fb);
   _quad_asr(f, a,fa, b,fb, eps,whole,m,fm);
}
```


```zkl
sinx:=quad_asr((1.0).sin.unbind(), 0.0, 1.0, 1e-09);
println("Simpson's integration of sine from 1 to 2 = ",sinx);
```

{{out}}

```txt

Simpson's integration of sine from 1 to 2 = 0.459698

```

