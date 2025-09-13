+++
title = "Pell's equation"
description = ""
date = 2019-10-08T00:34:21Z
aliases = []
[extra]
id = 22161
[taxonomies]
categories = ["task", "Mathematics"]
tags = []
+++

## Task

'''Pell's equation'''   (also called the '''Pell–Fermat''' equation)   is a   [https://en.wikipedia.org/wiki/Diophantine_equation <u>Diophantine equation</u>]   of the form:

:::::: <big> <b>  x<sup>2</sup> - ny<sup>2</sup>   =   1  </b> </big>

with integer solutions for   '''x'''   and   '''y''',   where   '''n'''   is a given non-square positive integer.


;Task requirements:
:*   find the smallest solution in positive integers to Pell's equation for   '''n''' = {61, 109, 181, 277}.


## See also

:*   Wikipedia entry: [https://en.wikipedia.org/wiki/Pell%27s_equation <u>Pell's equation</u>].





## ALGOL 68

{{Trans|Sidef}} Also tests for a trival solution only (if n is a perfect square only 1, 0 is solution).
```algol68
BEGIN
    # find solutions to Pell's eqauation: x^2 - ny^2 = 1 for integer x, y, n #
    MODE BIGINT     = LONG LONG INT;
    MODE BIGPAIR    = STRUCT( BIGINT v1, v2 );
    PROC solve pell = ( INT n )BIGPAIR:
         IF INT x = ENTIER( sqrt( n ) );
            x * x = n
         THEN
            # n is a erfect square - no solution otheg than 1,0              #
            BIGPAIR( 1, 0 )
         ELSE
            # there are non-trivial solutions                                #
            INT     y := x;
            INT     z := 1;
            INT     r := 2*x;
            BIGPAIR e := BIGPAIR( 1, 0 );
            BIGPAIR f := BIGPAIR( 0, 1 );
            BIGINT  a := 0;
            BIGINT  b := 0;
            WHILE
                y := (r*z - y);
                z := ENTIER ((n - y*y) / z);
                r := ENTIER ((x + y) / z);
                e := BIGPAIR( v2 OF e, r * v2 OF e + v1 OF e );
                f := BIGPAIR( v2 OF f, r * v2 OF f + v1 OF f );
                a := (v2 OF e + x*v2 OF f);
                b := v2 OF f;
                a*a - n*b*b /= 1
            DO SKIP OD;
            BIGPAIR( a, b )
         FI # solve pell # ;
    # task test cases                                                        #
    []INT nv = (61, 109, 181, 277);
    FOR i FROM LWB nv TO UPB nv DO
        INT  n = nv[ i ];
        BIGPAIR r = solve pell(n);
        print( ("x^2 - ", whole( n, -3 ), " * y^2 = 1 for x = ", whole( v1 OF r, -21), " and y = ", whole( v2 OF r, -21 ), newline ) )
    OD
END
```

```txt

x^2 -  61 * y^2 = 1 for x =            1766319049 and y =             226153980
x^2 - 109 * y^2 = 1 for x =       158070671986249 and y =        15140424455100
x^2 - 181 * y^2 = 1 for x =   2469645423824185801 and y =    183567298683461940
x^2 - 277 * y^2 = 1 for x = 159150073798980475849 and y =   9562401173878027020

```



## C#

```c#
using System;
using System.Numerics;

static class Program
{
    static void Fun(ref BigInteger a, ref BigInteger b, int c)
    {
        BigInteger t = a; a = b; b = b * c + t;
    }

    static void SolvePell(int n, ref BigInteger a, ref BigInteger b)
    {
        int x = (int)Math.Sqrt(n), y = x, z = 1, r = x << 1;
        BigInteger e1 = 1, e2 = 0, f1 = 0, f2 = 1;
        while (true)
        {
            y = r * z - y; z = (n - y * y) / z; r = (x + y) / z;
            Fun(ref e1, ref e2, r); Fun(ref f1, ref f2, r); a = f2; b = e2; Fun(ref b, ref a, x);
            if (a * a - n * b * b == 1) return;
        }
    }

    static void Main()
    {
        BigInteger x, y; foreach (int n in new[] { 61, 109, 181, 277 })
        {
            SolvePell(n, ref x, ref y);
            Console.WriteLine("x^2 - {0,3} * y^2 = 1 for x = {1,27:n0} and y = {2,25:n0}", n, x, y);
        }
    }
}
```

```txt
x^2 -  61 * y^2 = 1 for x =               1,766,319,049 and y =               226,153,980
x^2 - 109 * y^2 = 1 for x =         158,070,671,986,249 and y =        15,140,424,455,100
x^2 - 181 * y^2 = 1 for x =   2,469,645,423,824,185,801 and y =   183,567,298,683,461,940
x^2 - 277 * y^2 = 1 for x = 159,150,073,798,980,475,849 and y = 9,562,401,173,878,027,020
```



## D

```d
import std.bigint;
import std.math;
import std.stdio;

void fun(ref BigInt a, ref BigInt b, int c) {
    auto t = a;
    a = b;
    b = b * c + t;
}

void solvePell(int n, ref BigInt a, ref BigInt b) {
    int x = cast(int) sqrt(cast(real) n);
    int y = x;
    int z = 1;
    int r = x << 1;
    BigInt e1 = 1;
    BigInt e2 = 0;
    BigInt f1 = 0;
    BigInt f2 = 1;
    while (true) {
        y = r * z - y;
        z = (n - y * y) / z;
        r = (x + y) / z;
        fun(e1, e2, r);
        fun(f1, f2, r);
        a = f2;
        b = e2;
        fun(b, a, x);
        if (a * a - n * b * b == 1) {
            return;
        }
    }
}

void main() {
    BigInt x, y;
    foreach(n; [61, 109, 181, 277]) {
        solvePell(n, x, y);
        writefln("x^2 - %3d * y^2 = 1 for x = %27d and y = %25d", n, x, y);
    }
}
```

```txt
x^2 -  61 * y^2 = 1 for x =                  1766319049 and y =                 226153980
x^2 - 109 * y^2 = 1 for x =             158070671986249 and y =            15140424455100
x^2 - 181 * y^2 = 1 for x =         2469645423824185801 and y =        183567298683461940
x^2 - 277 * y^2 = 1 for x =       159150073798980475849 and y =       9562401173878027020
```



## Factor

```factor
USING: formatting kernel locals math math.functions sequences ;

:: solve-pell ( n -- a b )

    n sqrt >integer :> x!
    x :> y!
    1 :> z!
    2 x * :> r!

    1 0 :> ( e1! e2! )
    0 1 :> ( f1! f2! )
    0 0 :> ( a! b! )

    [ a sq b sq n * - 1 = ] [

        r z * y - y!
        n y sq - z / floor z!
        x y + z / floor r!

        e2 r e2 * e1 + e2! e1!
        f2 r f2 * f1 + f2! f1!

        e2 x f2 * + a!
        f2 b!

    ] until
    a b ;

{ 61 109 181 277 } [
    dup solve-pell
    "x^2 - %3d*y^2 = 1 for x = %-21d and y = %d\n" printf
] each
```

```txt

x^2 -  61*y^2 = 1 for x = 1766319049            and y = 226153980
x^2 - 109*y^2 = 1 for x = 158070671986249       and y = 15140424455100
x^2 - 181*y^2 = 1 for x = 2469645423824185801   and y = 183567298683461940
x^2 - 277*y^2 = 1 for x = 159150073798980475849 and y = 9562401173878027020

```



## FreeBASIC

'''for n = 277 the result is wrong, I do not know if you can represent such large numbers in FreeBasic!'''

```freebasic

Sub Fun(Byref a As LongInt, Byref b As LongInt, c As Integer)
    Dim As LongInt t
    t = a : a = b : b = b * c + t
End Sub

Sub SolvePell(n As Integer, Byref a As LongInt, Byref b As LongInt)
    Dim As Integer z, r
    Dim As LongInt x, y, e1, e2, f1, f2
    x = Sqr(n) : y = x : z  = 1 : r  = 2 * x
    e1 = 1 : e2 = 0 : f1 = 0 : f2 = 1
    While True
        y = r * z - y : z = (n - y * y) / z : r = (x + y) / z
        Fun(e1, e2, r) : Fun(f1, f2, r) : a = f2 : b = e2 : Fun(b, a, x)
        If a * a - n * b * b = 1 Then Exit Sub
    Wend
End Sub

Dim As Integer i
Dim As LongInt x, y
Dim As Integer n(0 To 3) = {61, 109, 181, 277}
For i = 0 To 3 ''n In {61, 109, 181, 277}
    SolvePell(n(i), x, y)
    Print Using "x^2 - ### * y^2 = 1 for x = ##################### and y = #####################"; n(i); x; y
Next i

```

```txt

x^2 -  61 * y^2 = 1 for x =            1766319049 and y =             226153980
x^2 - 109 * y^2 = 1 for x =       158070671986249 and y =        15140424455100
x^2 - 181 * y^2 = 1 for x =   2469645423824185801 and y =    183567298683461940
x^2 - 277 * y^2 = 1 for x =  -6870622864405488695 and y =  -8884342899831524596

```

<math>Insert formula here</math>


## Go

```go
package main

import (
    "fmt"
    "math/big"
)

var big1 = new(big.Int).SetUint64(1)

func solvePell(nn uint64) (*big.Int, *big.Int) {
    n := new(big.Int).SetUint64(nn)
    x := new(big.Int).Set(n)
    x.Sqrt(x)
    y := new(big.Int).Set(x)
    z := new(big.Int).SetUint64(1)
    r := new(big.Int).Lsh(x, 1)

    e1 := new(big.Int).SetUint64(1)
    e2 := new(big.Int)
    f1 := new(big.Int)
    f2 := new(big.Int).SetUint64(1)

    t := new(big.Int)
    u := new(big.Int)
    a := new(big.Int)
    b := new(big.Int)
    for {
        t.Mul(r, z)
        y.Sub(t, y)
        t.Mul(y, y)
        t.Sub(n, t)
        z.Quo(t, z)
        t.Add(x, y)
        r.Quo(t, z)
        u.Set(e1)
        e1.Set(e2)
        t.Mul(r, e2)
        e2.Add(t, u)
        u.Set(f1)
        f1.Set(f2)
        t.Mul(r, f2)
        f2.Add(t, u)
        t.Mul(x, f2)
        a.Add(e2, t)
        b.Set(f2)
        t.Mul(a, a)
        u.Mul(n, b)
        u.Mul(u, b)
        t.Sub(t, u)
        if t.Cmp(big1) == 0 {
            return a, b
        }
    }
}

func main() {
    ns := []uint64{61, 109, 181, 277}
    for _, n := range ns {
        x, y := solvePell(n)
        fmt.Printf("x^2 - %3d*y^2 = 1 for x = %-21s and y = %s\n", n, x, y)
    }
}
```


```txt

x^2 -  61*y^2 = 1 for x = 1766319049            and y = 226153980
x^2 - 109*y^2 = 1 for x = 158070671986249       and y = 15140424455100
x^2 - 181*y^2 = 1 for x = 2469645423824185801   and y = 183567298683461940
x^2 - 277*y^2 = 1 for x = 159150073798980475849 and y = 9562401173878027020

```




## Julia

```julia
function pell(n)
    x = BigInt(floor(sqrt(n)))
    y, z, r = x, BigInt(1), x << 1
    e1, e2, f1, f2 = BigInt(1), BigInt(0), BigInt(0), BigInt(1)
    while true
        y = r * z - y
        z = div(n - y * y, z)
        r = div(x + y, z)
        e1, e2 = e2, e2 * r + e1
        f1, f2 = f2, f2 * r + f1
        a, b = f2, e2
        b, a = a, a * x + b
        if a * a - n * b * b == 1
            return a, b
        end
    end
end

for target in BigInt[61, 109, 181, 277]
    x, y = pell(target)
    println("x\u00b2 - $target", "y\u00b2 = 1 for x = $x and y = $y")
end

```
```txt

x² - 61y² = 1 for x = 1766319049 and y = 226153980
x² - 109y² = 1 for x = 158070671986249 and y = 15140424455100
x² - 181y² = 1 for x = 2469645423824185801 and y = 183567298683461940
x² - 277y² = 1 for x = 159150073798980475849 and y = 9562401173878027020

```



## Kotlin

```scala
import java.math.BigInteger
import kotlin.math.sqrt

class BIRef(var value: BigInteger) {
    operator fun minus(b: BIRef): BIRef {
        return BIRef(value - b.value)
    }

    operator fun times(b: BIRef): BIRef {
        return BIRef(value * b.value)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as BIRef

        if (value != other.value) return false

        return true
    }

    override fun hashCode(): Int {
        return value.hashCode()
    }

    override fun toString(): String {
        return value.toString()
    }
}

fun f(a: BIRef, b: BIRef, c: Int) {
    val t = a.value
    a.value = b.value
    b.value = b.value * BigInteger.valueOf(c.toLong()) + t
}

fun solvePell(n: Int, a: BIRef, b: BIRef) {
    val x = sqrt(n.toDouble()).toInt()
    var y = x
    var z = 1
    var r = x shl 1
    val e1 = BIRef(BigInteger.ONE)
    val e2 = BIRef(BigInteger.ZERO)
    val f1 = BIRef(BigInteger.ZERO)
    val f2 = BIRef(BigInteger.ONE)
    while (true) {
        y = r * z - y
        z = (n - y * y) / z
        r = (x + y) / z
        f(e1, e2, r)
        f(f1, f2, r)
        a.value = f2.value
        b.value = e2.value
        f(b, a, x)
        if (a * a - BIRef(n.toBigInteger()) * b * b == BIRef(BigInteger.ONE)) {
            return
        }
    }
}

fun main() {
    val x = BIRef(BigInteger.ZERO)
    val y = BIRef(BigInteger.ZERO)
    intArrayOf(61, 109, 181, 277).forEach {
        solvePell(it, x, y)
        println("x^2 - %3d * y^2 = 1 for x = %,27d and y = %,25d".format(it, x.value, y.value))
    }
}
```

```txt
x^2 -  61 * y^2 = 1 for x =               1,766,319,049 and y =               226,153,980
x^2 - 109 * y^2 = 1 for x =         158,070,671,986,249 and y =        15,140,424,455,100
x^2 - 181 * y^2 = 1 for x =   2,469,645,423,824,185,801 and y =   183,567,298,683,461,940
x^2 - 277 * y^2 = 1 for x = 159,150,073,798,980,475,849 and y = 9,562,401,173,878,027,020
```



## Perl


```perl
sub solve_pell {
    my ($n) = @_;

    use bigint try => 'GMP';

    my $x = int(sqrt($n));
    my $y = $x;
    my $z = 1;
    my $r = 2 * $x;

    my ($e1, $e2) = (1, 0);
    my ($f1, $f2) = (0, 1);

    for (; ;) {

        $y = $r * $z - $y;
        $z = int(($n - $y * $y) / $z);
        $r = int(($x + $y) / $z);

        ($e1, $e2) = ($e2, $r * $e2 + $e1);
        ($f1, $f2) = ($f2, $r * $f2 + $f1);

        my $A = $e2 + $x * $f2;
        my $B = $f2;

        if ($A**2 - $n * $B**2 == 1) {
            return ($A, $B);
        }
    }
}

foreach my $n (61, 109, 181, 277) {
    my ($x, $y) = solve_pell($n);
    printf("x^2 - %3d*y^2 = 1 for x = %-21s and y = %s\n", $n, $x, $y);
}
```

```txt

x^2 -  61*y^2 = 1 for x = 1766319049            and y = 226153980
x^2 - 109*y^2 = 1 for x = 158070671986249       and y = 15140424455100
x^2 - 181*y^2 = 1 for x = 2469645423824185801   and y = 183567298683461940
x^2 - 277*y^2 = 1 for x = 159150073798980475849 and y = 9562401173878027020

```



## Perl 6

```perl6
use Lingua::EN::Numbers;

sub pell (Int $n) {

    my $y = my $x = Int(sqrt $n);
    my $z = 1;
    my $r = 2 * $x;

    my ($e1, $e2) = (1, 0);
    my ($f1, $f2) = (0, 1);

    loop {
        $y = $r * $z - $y;
        $z = Int(($n - $y²) / $z);
        $r = Int(($x + $y) / $z);

        ($e1, $e2) = ($e2, $r * $e2 + $e1);
        ($f1, $f2) = ($f2, $r * $f2 + $f1);

        my $A = $e2 + $x * $f2;
        my $B = $f2;

        if ($A² - $n * $B² == 1) {
            return ($A, $B);
        }
    }
}

for 61, 109, 181, 277, 8941 -> $n {
    next if $n.sqrt.narrow ~~ Int;
    my ($x, $y) = pell($n);
    printf "x² - %sy² = 1 for:\n\tx = %s\n\ty = %s\n\n", $n, |($x, $y)».&comma;
}
```

```txt
x² - 61y² = 1 for:
	x = 1,766,319,049
	y = 226,153,980

x² - 109y² = 1 for:
	x = 158,070,671,986,249
	y = 15,140,424,455,100

x² - 181y² = 1 for:
	x = 2,469,645,423,824,185,801
	y = 183,567,298,683,461,940

x² - 277y² = 1 for:
	x = 159,150,073,798,980,475,849
	y = 9,562,401,173,878,027,020

x² - 8941y² = 1 for:
	x = 2,565,007,112,872,132,129,669,406,439,503,954,211,359,492,684,749,762,901,360,167,370,740,763,715,001,557,789,090,674,216,330,243,703,833,040,774,221,628,256,858,633,287,876,949,448,689,668,281,446,637,464,359,482,677,366,420,261,407,112,316,649,010,675,881,349,744,201
	y = 27,126,610,172,119,035,540,864,542,981,075,550,089,190,381,938,849,116,323,732,855,930,990,771,728,447,597,698,969,628,164,719,475,714,805,646,913,222,890,277,024,408,337,458,564,351,161,990,641,948,210,581,361,708,373,955,113,191,451,102,494,265,278,824,127,994,180
```



## Phix

This now ignores the nonsquare part of the task spec, returning {1,0}.

```Phix
include mpfr.e

procedure fun(mpz a,b,t, integer c)
-- {a,b} = {b,c*b+a}  (and t gets trashed)
    mpz_set(t,a)
    mpz_set(a,b)
    mpz_mul_si(b,b,c)
    mpz_add(b,b,t)
end procedure

function SolvePell(integer n)
integer x = floor(sqrt(n)), y = x, z = 1, r = x*2
mpz e1 = mpz_init(1), e2 = mpz_init(),
    f1 = mpz_init(),  f2 = mpz_init(1),
    t = mpz_init(0),   u = mpz_init(),
    a = mpz_init(1),   b = mpz_init(0)
    if x*x!=n then
        while mpz_cmp_si(t,1)!=0 do
            y = r*z - y
            z = floor((n-y*y)/z)
            r = floor((x+y)/z)
            fun(e1,e2,t,r)          -- {e1,e2} = {e2,r*e2+e1}
            fun(f1,f2,t,r)          -- {f1,f2} = {f2,r*r2+f1}
            mpz_set(a,f2)
            mpz_set(b,e2)
            fun(b,a,t,x)            -- {b,a} = {f2,x*f2+e2}
            mpz_mul(t,a,a)
            mpz_mul_si(u,b,n)
            mpz_mul(u,u,b)
            mpz_sub(t,t,u)          -- t = a^2-n*b^2
        end while
    end if
    return {a, b}
end function

sequence ns = {4, 61, 109, 181, 277, 8941}
for i=1 to length(ns) do
    integer n = ns[i]
    mpz {x, y} = SolvePell(n)
    string xs = mpz_get_str(x,comma_fill:=true),
           ys = mpz_get_str(y,comma_fill:=true)
    printf(1,"x^2 - %3d*y^2 = 1 for x = %27s and y = %25s\n", {n, xs, ys})
end for
```

```txt

x^2 -   4*y^2 = 1 for x =                           1 and y =                         0
x^2 -  61*y^2 = 1 for x =               1,766,319,049 and y =               226,153,980
x^2 - 109*y^2 = 1 for x =         158,070,671,986,249 and y =        15,140,424,455,100
x^2 - 181*y^2 = 1 for x =   2,469,645,423,824,185,801 and y =   183,567,298,683,461,940
x^2 - 277*y^2 = 1 for x = 159,150,073,798,980,475,849 and y = 9,562,401,173,878,027,020
x^2 - 8941*y^2 = 1 for x = 2,565,007,112,872,132,129,669,406,439,503,954,211,359,492,684,749,762,
                             901,360,167,370,740,763,715,001,557,789,090,674,216,330,243,703,833,
                             040,774,221,628,256,858,633,287,876,949,448,689,668,281,446,637,464,
                             359,482,677,366,420,261,407,112,316,649,010,675,881,349,744,201
                  and y = 27,126,610,172,119,035,540,864,542,981,075,550,089,190,381,938,849,116,
                             323,732,855,930,990,771,728,447,597,698,969,628,164,719,475,714,805,
                             646,913,222,890,277,024,408,337,458,564,351,161,990,641,948,210,581,
                             361,708,373,955,113,191,451,102,494,265,278,824,127,994,180

```



## Python

```python
import math

def fun(a, b, c):
    t = a[0]
    a[0] = b[0]
    b[0] = b[0] * c + t

def solvePell(n, a, b):
    x = int(math.sqrt(n))
    y = x
    z = 1
    r = x << 1
    e1 = [1]
    e2 = [0]
    f1 = [0]
    f2 = [1]
    while True:
        y = r * z - y
        z = ((n - y * y) // z)
        r = (x + y) // z
        fun(e1, e2, r)
        fun(f1, f2, r)
        a[0] = f2[0]
        b[0] = e2[0]
        fun(b, a, x)
        if a[0] * a[0] - n * b[0] * b[0] == 1:
            return

x = [0]
y = [0]
for n in [61, 109, 181, 277]:
    solvePell(n, x, y)
    print("x^2 - %3d * y^2 = 1 for x = %27d and y = %25d" % (n, x[0], y[0]))
```

```txt
x^2 -  61 * y^2 = 1 for x =                  1766319049 and y =                 226153980
x^2 - 109 * y^2 = 1 for x =             158070671986249 and y =            15140424455100
x^2 - 181 * y^2 = 1 for x =         2469645423824185801 and y =        183567298683461940
x^2 - 277 * y^2 = 1 for x =       159150073798980475849 and y =       9562401173878027020
```



## REXX


```rexx
/*REXX program to solve Pell's equation for the smallest solution of positive integers. */
numeric digits 2200                              /*ensure enough decimal digs for answer*/
parse arg $                                      /*obtain optional arguments from the CL*/
if $=='' | $==","  then $= 61 109 181 277        /*Not specified?  Then use the defaults*/
d= 22                                            /*used for aligning the output numbers.*/
     do j=1  for words($);    #= word($, j)      /*process all the numbers in the list. */
     parse value   pells(#)   with   x  y        /*extract the two values of  X  and  Y.*/
     say 'x^2 -'right(#,max(4,length(#))) "* y^2 == 1  when x="right(x, max(d,length(x))),
                                                      ' and y='right(y, max(d,length(y)))
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
floor: procedure; parse arg x;  _= x % 1;          return  _   -    (x < 0)   *   (x \= _)
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt: procedure; parse arg x;  r= 0;     q= 1;           do  while q<=x;  q= q * 4;   end
         do  while q>1; q= q%4; _= x-r-q; r= r%2; if _>=0  then do; x= _; r= r+q; end; end
       return r                                  /*R:  is the integer square root of X. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
pells: procedure; parse arg n; x= iSqrt(n);  y=x /*obtain arg;  obtain integer sqrt of N*/
       parse value  1 0   with   e1 e2  1  f2 f1 /*assign values for: E1, E2, and F2, F1*/
       z= 1;        r= x + x
                                         do  until ( (e2 + x*f2)**2  -  n*f2*f2)  ==  1
                                         y= r*z   -   y
                                         z= floor( (n - y*y) / z)
                                         r= floor( (x + y  ) / z)
                                         parse value  e2   r*e2  +  e1     with    e1  e2
                                         parse value  f2   r*f2  +  f1     with    f1  f2
                                         end   /*until*/
       return e2  +  x * f2     f2
```

```txt

x^2 -  61 * y^2 == 1  when x=            1766319049  and y=             226153980
x^2 - 109 * y^2 == 1  when x=       158070671986249  and y=        15140424455100
x^2 - 181 * y^2 == 1  when x=   2469645423824185801  and y=    183567298683461940
x^2 - 277 * y^2 == 1  when x= 159150073798980475849  and y=   9562401173878027020

```



## Ruby

```ruby
def solve_pell(n)
  x = Integer.sqrt(n)
  y = x
  z = 1
  r = 2*x
  e1, e2 = 1, 0
  f1, f2 = 0, 1

  loop do
    y = r*z - y
    z = (n - y*y) / z
    r = (x + y) / z
    e1, e2 = e2, r*e2 + e1
    f1, f2 = f2, r*f2 + f1
    a,  b  = e2 + x*f2, f2
    break a, b if a*a - n*b*b == 1
  end
end

[61, 109, 181, 277].each {|n| puts "x*x - %3s*y*y = 1 for x = %-21s and y = %s" % [n, *solve_pell(n)]}

```

```txt

x*x -  61*y*y = 1 for x = 1766319049            and y = 226153980
x*x - 109*y*y = 1 for x = 158070671986249       and y = 15140424455100
x*x - 181*y*y = 1 for x = 2469645423824185801   and y = 183567298683461940
x*x - 277*y*y = 1 for x = 159150073798980475849 and y = 9562401173878027020


```



## Sidef


```ruby
func solve_pell(n) {

    var x = n.isqrt
    var y = x
    var z = 1
    var r = 2*x

    var (e1, e2) = (1, 0)
    var (f1, f2) = (0, 1)

    loop {

        y = (r*z - y)
        z = floor((n - y*y) / z)
        r = floor((x + y) / z)

        (e1, e2) = (e2, r*e2 + e1)
        (f1, f2) = (f2, r*f2 + f1)

        var A = (e2 + x*f2)
        var B = f2

        if (A**2 - n*B**2 == 1) {
            return (A, B)
        }
    }
}

for n in [61, 109, 181, 277] {
    var (x, y) = solve_pell(n)
    printf("x^2 - %3d*y^2 = 1 for x = %-21s and y = %s\n", n, x, y)
}
```

```txt

x^2 -  61*y^2 = 1 for x = 1766319049            and y = 226153980
x^2 - 109*y^2 = 1 for x = 158070671986249       and y = 15140424455100
x^2 - 181*y^2 = 1 for x = 2469645423824185801   and y = 183567298683461940
x^2 - 277*y^2 = 1 for x = 159150073798980475849 and y = 9562401173878027020

```



## Visual Basic .NET

```vbnet
Imports System.Numerics

Module Module1
    Sub Fun(ByRef a As BigInteger, ByRef b As BigInteger, c As Integer)
        Dim t As BigInteger = a : a = b : b = b * c + t
    End Sub

    Sub SolvePell(n As Integer, ByRef a As BigInteger, ByRef b As BigInteger)
        Dim x As Integer = Math.Sqrt(n), y As Integer = x, z As Integer = 1, r As Integer = x << 1,
            e1 As BigInteger = 1, e2 As BigInteger = 0, f1 As BigInteger = 0, f2 As BigInteger = 1
        While True
            y = r * z - y : z = (n - y * y) / z : r = (x + y) / z
            Fun(e1, e2, r) : Fun(f1, f2, r) : a = f2 : b = e2 : Fun(b, a, x)
            If a * a - n * b * b = 1 Then Exit Sub
        End While
    End Sub

    Sub Main()
        Dim x As BigInteger, y As BigInteger
        For Each n As Integer In {61, 109, 181, 277}
            SolvePell(n, x, y)
            Console.WriteLine("x^2 - {0,3} * y^2 = 1 for x = {1,27:n0} and y = {2,25:n0}", n, x, y)
        Next
    End Sub
End Module
```

```txt
x^2 -  61 * y^2 = 1 for x =               1,766,319,049 and y =               226,153,980
x^2 - 109 * y^2 = 1 for x =         158,070,671,986,249 and y =        15,140,424,455,100
x^2 - 181 * y^2 = 1 for x =   2,469,645,423,824,185,801 and y =   183,567,298,683,461,940
x^2 - 277 * y^2 = 1 for x = 159,150,073,798,980,475,849 and y = 9,562,401,173,878,027,020
```



## zkl

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library
```zkl
var [const] BI=Import("zklBigNum");  // libGMP

fcn solve_pell(n){
   x,y,z,r := BI(n).root(2),  x.copy(),  BI(1),  x*2;
   e1,e2, f1,f2 := BI(1), BI(0),  BI(0), BI(1);
   reg t;	// a,b = c,d is a=c; b=d
   do(30_000){  // throttle this in case of screw up
      y,z,r = (r*z - y),  (n - y*y)/z,  (x + y)/z;

      t,e2,e1 = e2,  r*e2 + e1,  t;
      t,f2,f1 = f2,  r*f2 + f1,  t;

      A,B := e2 + x*f2, f2;

      if (A*A - B*B*n == 1) return(A,B);
   }
}
```


```zkl
foreach n in (T(61, 109, 181, 277)){
   x,y:=solve_pell(n);
   println("x^2 - %3d*y^2 = 1 for x = %-21d and y = %d".fmt(n,x,y));
}
```


```txt

x^2 -  61*y^2 = 1 for x = 1766319049            and y = 226153980
x^2 - 109*y^2 = 1 for x = 158070671986249       and y = 15140424455100
x^2 - 181*y^2 = 1 for x = 2469645423824185801   and y = 183567298683461940
x^2 - 277*y^2 = 1 for x = 159150073798980475849 and y = 9562401173878027020

```

