+++
title = "Convert decimal number to rational"
description = ""
date = 2019-08-24T20:20:15Z
aliases = []
[extra]
id = 9912
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "applescript",
  "autohotkey",
  "bracmat",
  "c",
  "clojure",
  "common_lisp",
  "csharp",
  "d",
  "echolisp",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "liberty_basic",
  "maple",
  "netrexx",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "pl_i",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "tcl",
  "zkl",
]
+++

The task is to write a program to transform a decimal number into a fraction in lowest terms.

It is not always possible to do this exactly. For instance, while rational numbers can be converted to decimal representation, some of them need an infinite number of digits to be represented exactly in decimal form. Namely, [[wp:Repeating decimal|repeating decimals]] such as 1/3 = 0.333...

Because of this, the following fractions cannot be obtained (reliably) unless the language has some way of representing repeating decimals:
* 67 / 74 = 0.9(054) = 0.9054054...
* 14 / 27 = 0.(518)  = 0.518518...


Acceptable output:

* 0.9054054 → 4527027 / 5000000
* 0.518518  → 259259 / 500000


Finite decimals are of course no problem:

* 0.75      → 3 / 4





## Ada

Specification of a procedure Real_To_Rational, which is searching for the best approximation of a real number. The procedure is generic. I.e., you can instantiate it by your favorite "Real" type (Float, Long_Float, ...).


```Ada
generic
   type Real is digits <>;
procedure Real_To_Rational(R: Real;
                           Bound: Positive;
                           Nominator: out Integer;
                           Denominator: out Positive);
```


The implementation (just brute-force search for the best approximation with Denominator less or equal Bound):


```Ada
procedure Real_To_Rational (R: Real;
                            Bound: Positive;
                            Nominator: out Integer;
                            Denominator: out  Positive) is
   Error: Real;
   Best: Positive := 1;
   Best_Error: Real := Real'Last;
begin
   if R = 0.0 then
      Nominator := 0;
      Denominator := 1;
      return;
   elsif R < 0.0 then
      Real_To_Rational(-R, Bound, Nominator, Denominator);
      Nominator := - Nominator;
      return;
   else
      for I in 1 .. Bound loop
         Error := abs(Real(I) * R - Real'Rounding(Real(I) * R));
         if Error < Best_Error then
            Best := I;
            Best_Error := Error;
         end if;
      end loop;
   end if;
   Denominator := Best;
   Nominator   := Integer(Real'Rounding(Real(Denominator) * R));

end Real_To_Rational;
```


The main program, called "Convert_Decimal_To_Rational", reads reals from the standard input until 0.0. It outputs progressively better rational approximations of the reals, where "progressively better" means a larger Bound for the Denominator:


```Ada
with Ada.Text_IO; With Real_To_Rational;

procedure Convert_Decimal_To_Rational is

   type My_Real is new Long_Float; -- change this for another "Real" type

   package FIO is new Ada.Text_IO.Float_IO(My_Real);
   procedure R2R is new Real_To_Rational(My_Real);

   Nom, Denom: Integer;
   R: My_Real;

begin
   loop
      Ada.Text_IO.New_Line;
      FIO.Get(R);
      FIO.Put(R, Fore => 2, Aft => 9, Exp => 0);
      exit when R = 0.0;
      for I in 0 .. 4 loop
         R2R(R, 10**I, Nom, Denom);
         Ada.Text_IO.Put("  " & Integer'Image(Nom) &
                         " /" & Integer'Image(Denom));
      end loop;
   end loop;
end Convert_Decimal_To_Rational;

```


Finally, the output (reading the input from a file):

```txt
> ./convert_decimal_to_rational < input.txt

 0.750000000   1 / 1   3 / 4   3 / 4   3 / 4   3 / 4
 0.518518000   1 / 1   1 / 2   14 / 27   14 / 27   14 / 27
 0.905405400   1 / 1   9 / 10   67 / 74   67 / 74   67 / 74
 0.142857143   0 / 1   1 / 7   1 / 7   1 / 7   1 / 7
 3.141592654   3 / 1   22 / 7   22 / 7   355 / 113   355 / 113
 2.718281828   3 / 1   19 / 7   193 / 71   1457 / 536   25946 / 9545
-0.423310825   0 / 1  -3 / 7  -11 / 26  -69 / 163  -1253 / 2960
31.415926536   31 / 1   157 / 5   377 / 12   3550 / 113   208696 / 6643
 0.000000000

```


## AppleScript


```applescript
on run

    script ratioString
        -- Using a tolerance epsilon of 1/10000
        on |λ|(x)
            showRatio(approxRatio(1.0E-4, x))
        end |λ|
    end script

    map(ratioString, ¬
        {0.9054054, 0.518518, 0.75})

    --> {"67/74", "14/27", "3/4"}
end run


-- approxRatio :: Real -> Real -> Ratio
on approxRatio(epsilon, n)
    if {real, integer} contains (class of epsilon) and 0 < epsilon then
        set e to epsilon
    else
        set e to 1 / 10000
    end if

    script gcde
        on |λ|(e, x, y)
            script _gcd
                on |λ|(a, b)
                    if b < e then
                        a
                    else
                        |λ|(b, a mod b)
                    end if
                end |λ|
            end script
            |λ|(abs(x), abs(y)) of _gcd
        end |λ|
    end script

    set c to |λ|(e, 1, n) of gcde
    Ratio((n div c), (1 div c))
end approxRatio


-- Ratio :: Int -> Int -> Ratio
on Ratio(n, d)
    {type:"Ratio", n:n, d:d}
end Ratio


-- showRatio :: Ratio -> String
on showRatio(r)
    (n of r as string) & "/" & (d of r as string)
end showRatio


-- GENERIC FUNCTIONS ---------------------------------------------

-- abs :: Num -> Num
on abs(x)
    if 0 > x then
        -x
    else
        x
    end if
end abs


-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map
```

```txt
{"67/74", "14/27", "3/4"}
```



## AutoHotkey



```AutoHotkey
    Array := []
    inputbox, string, Enter Number
    stringsplit, string, string, .
    if ( string1 = 0 )
            string1 =
    loop, parse, string, .
            if A_index = 2
                    loop, parse, A_loopfield
                                                    Array[A_index] := A_loopfield,          k := A_index
    if (k = 1)
     {
    numerator := Array[1]
    Denominator := 10
    goto label
    }
    Original1 := K
    To_rn := floor(k/2)
    M_M := k - To_rn
    Original2 := k - To_rn
    loop
    {
    loop, % To_rn

    {
    Check1 .= Array[k]
    Check2 .= Array[M_M]
    k--
    m_M--
    }
    if ( check1 = check2 )
    {
            ;~ process beginsTO check;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      loop, % To_rn
        nines .= 9
    loop, % k - TO_rn
        Zeroes .= 0
    loop % k - TO_rn
            Minus .= Array[A_index]
    loop % k
            Plus .= Array[A_index]
    if ( minus = "" )
            minus := 0
    Numerator := Plus - minus
    Denominator := Nines . Zeroes
    ;;;;;;;;;;;;;HCF
    goto, label
    }
    Check1 =
    check2 =
    k := Original1
    m_M := original2 + A_index
    TO_rn--
    if ( to_rn = 0 )
    {
            zeroes =
            loop % original1
                    zeroes .= 0
    Denominator := 1 . zeroes
    numerator := string2
    goto, label
    }
    }
    esc::Exitapp
    label:
    Index := 2
    loop
    {

            if (mod(denominator, numerator) = 0 )
                    HCF := numerator
            if ( index = floor(numerator/2) )
            break
    if  ( mod(numerator, index) = 0 ) && ( mod(denominator, index) = 0 )
    {
            HCF = %index%
            index++
    }
    else
            index++
    }
    if ( HCF = "" )
            Ans := numerator "/" Denominator
    else
    Ans := floor(numerator/HCF) "/" floor(Denominator/HCF)
    MsgBox % String . "  ->  " . String1 . " " . Ans
    reload

```


```txt

0.9054054 -> 67/74
0.518518 -> 14/27
0.75 -> 3/4

```



## Bracmat


```bracmat
( ( exact
  =   integerPart decimalPart z
    .     @(!arg:?integerPart "." ?decimalPart)
        &   !integerPart
          + (   @( !decimalPart
                 : (? ((%@:~0) ?:?decimalPart)) [?z
                 )
              & !decimalPart*10^(-1*!z)
            | 0
            )
      | !arg
  )
& ( approximation
  =     integerPart firstDecimals repeatingDecimals
      , x y z z-y x-y numerator denominator
    .   @( !arg
         :   ?integerPart
             "."
             [?x
             ?firstDecimals
             ?repeatingDecimals
             [?y
             !repeatingDecimals
             [?z
         )
      & !z+-1*!y:?z-y
      & !x+-1*!y:?x-y
      & 10:?numerator:?denominator
      & ( !z-y:0&0:?repeatingDecimals
        |   9:?denominator
          &   whl
            ' ( !z+-1:>!y:?z
              & !numerator*10:?numerator
              & !denominator*10+9:?denominator
              )
          & @(!repeatingDecimals:? #?repeatingDecimals)
        )
      & ( @(!firstDecimals:? #?firstDecimals)
        | 0:?firstDecimals
        )
      &   !integerPart
        + !firstDecimals*10^(!x-y+!z-y)
        + !numerator*!denominator^-1*!repeatingDecimals*10^!x-y
  )
&     "0.9054054054"
      "0.5185185185"
      "0.75"
      "0.905405400"
      "0.1428571428"
      "35.000"
      "35.001"
      "0.00000000001"
      "0.000001000001"
      "0.9"
      "0.99"
      "0.909"
      "0.9090"
      "0.90909"
  : ?decs
&   whl
  ' ( !decs:%?dec ?decs
    & approximation$!dec:?approx
    &   out
      $ ( !dec
          "="
          (exact$!dec:?precise)
          ( !approx:!precise&
          | str$("(approx. " !approx ")")
          )
        )
    )
);
```

Output:

```txt
0.9054054054 = 4527027027/5000000000 (approx. 67/74)
0.5185185185 = 1037037037/2000000000 (approx. 14/27)
0.75 = 3/4
0.905405400 = 4527027/5000000
0.1428571428 = 357142857/2500000000
35.000 = 35
35.001 = 35001/1000
0.00000000001 = 1/100000000000
0.000001000001 = 1000001/1000000000000 (approx. 1/999999)
0.9 = 9/10
0.99 = 99/100 (approx. 1)
0.909 = 909/1000
0.9090 = 909/1000 (approx. 10/11)
0.90909 = 90909/100000 (approx. 10/11)
```



## C

Since the intention of the task giver is entirely unclear, here's another version of best rational approximation of a floating point number.  It's somewhat more rigorous than the Perl version below, but is still not quite complete.
```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>

/* f : number to convert.
 * num, denom: returned parts of the rational.
 * md: max denominator value.  Note that machine floating point number
 *     has a finite resolution (10e-16 ish for 64 bit double), so specifying
 *     a "best match with minimal error" is often wrong, because one can
 *     always just retrieve the significand and return that divided by
 *     2**52, which is in a sense accurate, but generally not very useful:
 *     1.0/7.0 would be "2573485501354569/18014398509481984", for example.
 */
void rat_approx(double f, int64_t md, int64_t *num, int64_t *denom)
{
	/*  a: continued fraction coefficients. */
	int64_t a, h[3] = { 0, 1, 0 }, k[3] = { 1, 0, 0 };
	int64_t x, d, n = 1;
	int i, neg = 0;

	if (md <= 1) { *denom = 1; *num = (int64_t) f; return; }

	if (f < 0) { neg = 1; f = -f; }

	while (f != floor(f)) { n <<= 1; f *= 2; }
	d = f;

	/* continued fraction and check denominator each step */
	for (i = 0; i < 64; i++) {
		a = n ? d / n : 0;
		if (i && !a) break;

		x = d; d = n; n = x % n;

		x = a;
		if (k[1] * a + k[0] >= md) {
			x = (md - k[0]) / k[1];
			if (x * 2 >= a || k[1] >= md)
				i = 65;
			else
				break;
		}

		h[2] = x * h[1] + h[0]; h[0] = h[1]; h[1] = h[2];
		k[2] = x * k[1] + k[0]; k[0] = k[1]; k[1] = k[2];
	}
	*denom = k[1];
	*num = neg ? -h[1] : h[1];
}

int main()
{
	int i;
	int64_t d, n;
	double f;

	printf("f = %16.14f\n", f = 1.0/7);
	for (i = 1; i <= 20000000; i *= 16) {
		printf("denom <= %d: ", i);
		rat_approx(f, i, &n, &d);
		printf("%lld/%lld\n", n, d);
	}

	printf("\nf = %16.14f\n", f = atan2(1,1) * 4);
	for (i = 1; i <= 20000000; i *= 16) {
		printf("denom <= %d: ", i);
		rat_approx(f, i, &n, &d);
		printf("%lld/%lld\n", n, d);
	}

	return 0;
}
```
Output:<lang>f = 0.14285714285714
denom <= 1: 0/1
denom <= 16: 1/7
denom <= 256: 1/7
denom <= 4096: 1/7
denom <= 65536: 1/7
denom <= 1048576: 1/7
denom <= 16777216: 1/7

f = 3.14159265358979
denom <= 1: 3/1
denom <= 16: 22/7
denom <= 256: 355/113
denom <= 4096: 355/113
denom <= 65536: 104348/33215
denom <= 1048576: 3126535/995207
denom <= 16777216: 47627751/15160384
```


## C#

```c#
using System;
using System.Text;

namespace RosettaDecimalToFraction
{
    public class Fraction
    {
        public Int64 Numerator;
        public Int64 Denominator;
        public Fraction(double f, Int64 MaximumDenominator = 4096)
        {
            /* Translated from the C version. */
            /*  a: continued fraction coefficients. */
            Int64 a;
            var h = new Int64[3] { 0, 1, 0 };
            var k = new Int64[3] { 1, 0, 0 };
            Int64 x, d, n = 1;
            int i, neg = 0;

            if (MaximumDenominator <= 1)
            {
                Denominator = 1;
                Numerator = (Int64)f;
                return;
            }

            if (f < 0) { neg = 1; f = -f; }

            while (f != Math.Floor(f)) { n <<= 1; f *= 2; }
            d = (Int64)f;

            /* continued fraction and check denominator each step */
            for (i = 0; i < 64; i++)
            {
                a = (n != 0) ? d / n : 0;
                if ((i != 0) && (a == 0)) break;

                x = d; d = n; n = x % n;

                x = a;
                if (k[1] * a + k[0] >= MaximumDenominator)
                {
                    x = (MaximumDenominator - k[0]) / k[1];
                    if (x * 2 >= a || k[1] >= MaximumDenominator)
                        i = 65;
                    else
                        break;
                }

                h[2] = x * h[1] + h[0]; h[0] = h[1]; h[1] = h[2];
                k[2] = x * k[1] + k[0]; k[0] = k[1]; k[1] = k[2];
            }
            Denominator = k[1];
            Numerator = neg != 0 ? -h[1] : h[1];
        }
        public override string ToString()
        {
            return string.Format("{0} / {1}", Numerator, Denominator);
        }
    }
    class Program
    {
        static void Main(string[] args)
        {
            Console.OutputEncoding = UTF8Encoding.UTF8;
            foreach (double d in new double[] { 0.9054054, 0.518518, 0.75, 0.4285714, 0.833333,
                0.90909, 3.14159265358979, 2.7182818284590451 })
            {
                var f = new Fraction(d, d >= 2 ? 65536 : 4096);
                Console.WriteLine("{0,20} → {1}", d, f);

            }
        }
    }
}

```


```txt
           0.9054054 → 67 / 74
            0.518518 → 14 / 27
                0.75 → 3 / 4
           0.4285714 → 3 / 7
            0.833333 → 5 / 6
             0.90909 → 10 / 11
    3.14159265358979 → 104348 / 33215
    2.71828182845905 → 49171 / 18089
```





## Clojure


```txt

user=> (rationalize 0.1)
1/10
user=> (rationalize 0.9054054)
4527027/5000000
user=> (rationalize 0.518518)
259259/500000
user=> (rationalize Math/PI)
3141592653589793/1000000000000000

```



## Common Lisp

There are two functions for converting decimals to rationals: '''rational''' returns a rational that is mathematically equal in value to the decimal and '''rationalize''' returns a rational that approximates the decimal to the accuracy of the underlying floating-point representation.

```txt
> (rational 0.9054054)
7595091/8388608
> (rationalize 0.9054054)
67/74
> (= (rational 0.9054054) 0.9054054)
T
> (= (rationalize 0.9054054) 0.9054054)
NIL
> (rational .518518)
1087411/2097152
> (rationalize .518518)
33279/64181
> (rational .5185185)
8699297/16777216
> (rationalize .5185185)
14/27
> (rational .75)
3/4
> (rationalize .75)
3/4
```



## D

```d
import std.stdio, std.math, std.string, std.typecons;

alias Fraction = Tuple!(int,"nominator", uint,"denominator");

Fraction real2Rational(in real r, in uint bound) /*pure*/ nothrow {
    if (r == 0.0) {
        return Fraction(0, 1);
    } else if (r < 0.0) {
        auto result = real2Rational(-r, bound);
        result.nominator = -result.nominator;
        return result;
    } else {
        uint best = 1;
        real bestError = real.max;

        foreach (i; 1 .. bound + 1) {
            // round is not pure.
            immutable real error = abs(i * r - round(i * r));
            if (error < bestError) {
                best = i;
                bestError = error;
            }
        }

        return Fraction(cast(int)round(best * r), best);
    }
}

void main() {
    immutable tests = [ 0.750000000,  0.518518000, 0.905405400,
                        0.142857143,  3.141592654, 2.718281828,
                       -0.423310825, 31.415926536];

    foreach (r; tests) {
        writef("%8.9f  ", r);
        foreach (i; 0 .. 5)
            writef("  %d/%d", real2Rational(r, 10 ^^ i).tupleof);
        writeln();
    }
}
```

```txt
0.750000000    1/1  3/4  3/4  3/4  3/4  3/4
0.518518000    1/1  1/2  14/27  14/27  14/27  37031/71417
0.905405400    1/1  9/10  67/74  67/74  67/74  67/74
0.142857143    0/1  1/7  1/7  1/7  1/7  1/7
3.141592654    3/1  22/7  22/7  355/113  355/113  104348/33215
2.718281828    3/1  19/7  193/71  1457/536  25946/9545  222630/81901
-0.423310825    0/1  -3/7  -11/26  -69/163  -1253/2960  -10093/23843
31.415926536    31/1  157/5  377/12  3550/113  208696/6643  2918194/92889
```



## EchoLisp

The '''rationalize''' function uses a Stern-Brocot tree [http://en.wikipedia.org/wiki/Stern%E2%80%93Brocot_tree] to find the best rational approximation of an inexact (floating point) number, for a given precision. The '''inexact->exact''' function returns a rational approximation for the default precision 0.0001 .

```scheme

(exact->inexact 67/74)
   → 0.9054054054054054
(inexact->exact 0.9054054054054054)
   → 67/74

(rationalize 0.7978723404255319)
  → 75/94

;; finding rational approximations of PI
(for ((ε (in-range -1 -15 -1)))
	(writeln ( format "precision:10^%d %t PI = %d" ε
        (rationalize PI (expt 10 e)))))

"precision:10^-1                   PI = 16/5"
"precision:10^-2                   PI = 22/7"    ;;🎩
"precision:10^-3                   PI = 201/64"
"precision:10^-4                   PI = 333/106"
"precision:10^-5                   PI = 355/113"  ;; 🎩 🎩
"precision:10^-6                   PI = 355/113"
"precision:10^-7                   PI = 75948/24175"
"precision:10^-8                   PI = 100798/32085"
"precision:10^-9                   PI = 103993/33102"
"precision:10^-10                  PI = 312689/99532"
"precision:10^-11                  PI = 833719/265381"
"precision:10^-12                  PI = 4272943/1360120"
"precision:10^-13                  PI = 5419351/1725033"
"precision:10^-14                  PI = 58466453/18610450"

```



## Factor


```factor

USE: math.floating-point
{ 0.9054054 0.518518 0.75 } [ double>ratio ] map [ . ] each

```

```txt

4077583422059235/4503599627370496
2335197471584895/4503599627370496
3/4

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Convert_decimal_number_to_rational this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```Forth

\ Brute force search, optimized to search only within integer bounds surrounding target
\ Forth 200x compliant

: RealToRational  ( float_target int_denominator_limit -- numerator denominator )
    {:  f: thereal denlimit | realscale  numtor denom neg? f: besterror f: temperror :}
    0 to numtor
    0 to denom
    9999999e to besterror                 \ very large error that will surely be improved upon
    thereal F0< to neg?                   \ save sign for later
    thereal FABS to thereal

    thereal FTRUNC f>s 1+ to realscale      \ realscale helps set integer bounds around target

    denlimit 1+ 1 ?DO                    \ search through possible denominators ( 1 to denlimit)

        I realscale *  I realscale 1- *  ?DO    \ search within integer limits bounding the real
            I s>f  J s>f  F/                    \ e.g. for 3.1419e search only between 3 and 4
            thereal F- FABS to temperror

            temperror besterror F< IF
                temperror to besterror I to numtor J to denom
            THEN
        LOOP

    LOOP

    neg? IF numtor NEGATE to numtor THEN

    numtor denom
;
(run)
1.618033988e 100 RealToRational  swap . . 144 89
3.14159e 1000 RealToRational swap . . 355 113
2.71828e 1000 RealToRational swap . . 1264 465
0.9054054e 100 RealToRational swap . . 67  74

```




## Fortran

This sort of calculation works best in base ten and with an input scheme that recognises a protocol for specifying that the decimal value has a recurring sequence, whereupon the methods taught in school could be employed. Such a protocol often conflicts with presenting a digit sequence with the last digit correctly rounded. Although many early computers worked in base ten, these days a binary base is usual, be it 2, 4, 8, or 16. Alas, five is not a factor of two nor any of its powers, and many decimal fractions, even a brief one such as 0·1, convert to a recurring sequence in binary so for example, 10·15 is in binary 1010·0010011001100110011... However, 203/20 when evaluated in binary floating-point will generate the same binary sequence as 10·15, granted that the compiler does its arithmetic correctly. Nevertheless, such a decimal value is ''not'' the value that a binary computer will work with, except for restricted fractions such as 0·5, 0·25, 0·75, and so forth up to the precision of the arithmetic.

Rather than engage in fancy schemes, here are two "brute force" methods. The first simply multiplies the value by a large power of ten, then casts out common factors in P/Q = x*1000000000/100000000. But if the best value for Q involves factors other than two and five, this won't work well. The second method is to jiggle either P or Q upwards depending on whether P/Q is smaller or larger than X, reporting improvements as it goes. Once beyond small numbers there are many small improvements to be found, so only those much better than the previous best are reported. Loosely speaking, the number of digits correct in good values of P/Q should be the sum of the number of digits in P and Q, and more still for happy fits, but a factor of eight suffices to suppress the rabble. Thus for Pi, the famous 22/7 and 355/113 appear as desired. Later pairs use lots more digits without a surprise hit, except for the short decimal sequence which comes out as 314159/100000 that reconstitutes the given decimal fraction exactly. Which is not a good approximation for Pi, and its pairs diverge from those of the more accurate value. In other words, one must assess the precision of the given value and not be distracted by the spurious precision offered by the larger P/Q pairs, so for 3·14159 with six digits, there is little point in going further than 355/113 - with their six digits. Contrariwise, if a P/Q with few digits matches many more digits of the given number, then a source rational number can be suspected. But if given just a few digits, such as 0·518 (or 0·519, when rounded), 13/25 could be just as likely a source number as 14/27 which is further away.

The source uses the MODULE facility of F90 merely to avoid the annoyance of having to declare the type of integer function GCD. The T ("tab") format code is employed to facilitate the alignment of output, given that P/Q is presented with I0 format so that there are no spaces (as in "   22/    7" for example), the latter being standard in F90 but an extension in earlier Fortrans.
```Fortran
      MODULE PQ	!Plays with some integer arithmetic.
       INTEGER MSG	!Output unit number.
       CONTAINS		!One good routine.
        INTEGER FUNCTION GCD(I,J)	!Greatest common divisor.
         INTEGER I,J	!Of these two integers.
         INTEGER N,M,R	!Workers.
          N = MAX(I,J)	!Since I don't want to damage I or J,
          M = MIN(I,J)	!These copies might as well be the right way around.
    1     R = MOD(N,M)		!Divide N by M to get the remainder R.
          IF (R.GT.0) THEN	!Remainder zero?
            N = M			!No. Descend a level.
            M = R			!M-multiplicity has been removed from N.
            IF (R .GT. 1) GO TO 1	!No point dividing by one.
          END IF			!If R = 0, M divides N.
          GCD = M			!There we are.
        END FUNCTION GCD	!Euclid lives on!

        SUBROUTINE RATIONAL10(X)!By contrast, this is rather crude.
         DOUBLE PRECISION X	!The number.
         DOUBLE PRECISION R	!Its latest rational approach.
         INTEGER P,Q		!For R = P/Q.
         INTEGER F,WHACK	!Assistants.
         PARAMETER (WHACK = 10**8)	!The rescale...
          P = X*WHACK + 0.5	!Multiply by WHACK/WHACK = 1 and round to integer.
          Q = WHACK		!Thus compute X/1, sortof.
          F = GCD(P,Q)		!Perhaps there is a common factor.
          P = P/F		!Divide it out.
          Q = Q/F		!For a proper rational number.
          R = DBLE(P)/DBLE(Q)	!So, where did we end up?
          WRITE (MSG,1) P,Q,X - R,WHACK	!Details.
    1     FORMAT ("x - ",I0,"/",I0,T28," = ",F18.14,
     1     " via multiplication by ",I0)
        END SUBROUTINE RATIONAL10	!Enough of this.

        SUBROUTINE RATIONAL(X)	!Use brute force in a different way.
         DOUBLE PRECISION X	!The number.
         DOUBLE PRECISION R,E,BEST	!Assistants.
         INTEGER P,Q		!For R = P/Q.
         INTEGER TRY,F		!Floundering.
          P = 1 + X	!Prevent P = 0.
          Q = 1		!So, X/1, sortof.
          BEST = X*6	!A largeish value for the first try.
          DO TRY = 1,10000000	!Pound away.
            R = DBLE(P)/DBLE(Q)		!The current approximation.
            E = X - R			!Deviation.
            IF (ABS(E) .LE. BEST) THEN	!Significantly better than before?
              BEST = ABS(E)*0.125		!Yes. Demand eightfold improvement to notice.
              F = GCD(P,Q)			!We may land on a multiple.
              IF (BEST.LT.0.1D0) WRITE (MSG,1) P/F,Q/F,E	!Skip early floundering.
    1         FORMAT ("x - ",I0,"/",I0,T28," = ",F18.14)	!Try to align columns.
              IF (F.NE.1) WRITE (MSG,*) "Common factor!",F	!A surprise!
              IF (E.EQ.0) EXIT			!Perhaps we landed a direct hit?
            END IF			!So much for possible announcements.
            IF (E.GT.0) THEN	!Is R too small?
              P = P + CEILING(E*Q)	!Yes. Make P bigger by the shortfall.
            ELSE IF (E .LT. 0) THEN	!But perhaps R is too big?
              Q = Q + 1			!If so, use a smaller interval.
            END IF		!So much for adjustments.
          END DO		!Try again.
        END SUBROUTINE RATIONAL	!Limited integers, limited sense.

        SUBROUTINE RATIONALISE(X,WOT)	!Run the tests.
         DOUBLE PRECISION X	!The value.
         CHARACTER*(*) WOT	!Some blather.
          WRITE (MSG,*) X,WOT	!Explanations can help.
          CALL RATIONAL10(X)	!Try a crude method.
          CALL RATIONAL(X)	!Try a laborious method.
          WRITE (MSG,*)		!Space off.
        END SUBROUTINE RATIONALISE	!That wasn't much fun.
      END MODULE PQ	!But computer time is cheap.

      PROGRAM APPROX
      USE PQ
      DOUBLE PRECISION PI,E
      MSG = 6
      WRITE (MSG,*) "Rational numbers near to decimal values."
      WRITE (MSG,*)
      PI = 1		!Thus get a double precision conatant.
      PI = 4*ATAN(PI)	!That will determine the precision of ATAN.
      E = DEXP(1.0D0)	!Rather than blabber on about 1 in double precision.
      CALL RATIONALISE(0.1D0,"1/10 Repeating in binary..")
      CALL RATIONALISE(3.14159D0,"Pi approx.")
      CALL RATIONALISE(PI,"Pi approximated better.")
      CALL RATIONALISE(E,"e: rational approximations aren't much use.")
      CALL RATIONALISE(10.15D0,"Exact in decimal, recurring in binary.")
      WRITE (MSG,*)
      WRITE (MSG,*) "Variations on 67/74"
      CALL RATIONALISE(0.9054D0,"67/74 = 0·9(054) repeating in base 10")
      CALL RATIONALISE(0.9054054D0,"Two repeats.")
      CALL RATIONALISE(0.9054054054D0,"Three repeats.")
      WRITE (MSG,*)
      WRITE (MSG,*) "Variations on 14/27"
      CALL RATIONALISE(0.518D0,"14/27 = 0·(518) repeating in decimal.")
      CALL RATIONALISE(0.519D0,"Rounded.")
      CALL RATIONALISE(0.518518D0,"Two repeats, truncated.")
      CALL RATIONALISE(0.518519D0,"Two repeats, rounded.")
      END
```


Some examples. Each rational value is followed by X - P/Q. Notice that 0·(518) repeating, presented as 0·518518, is ''not'' correctly rounded.

```txt

 Rational numbers near to decimal values.

  0.100000000000000      1/10 Repeating in binary..
x - 1/10                    =   0.00000000000000 via multiplication by 100000000
x - 1/2                     =  -0.40000000000000
x - 1/7                     =  -0.04285714285714
x - 1/10                    =   0.00000000000000

   3.14159000000000      Pi approx.
x - 314159/100000           =   0.00000000000000 via multiplication by 100000000
x - 16/5                    =  -0.05841000000000
x - 22/7                    =  -0.00126714285714
x - 355/113                 =  -0.00000292035398
x - 9563/3044               =  -0.00000001314060
x - 85712/27283             =  -0.00000000109959
x - 238010/75761            =  -0.00000000013199
x - 314159/100000           =   0.00000000000000

   3.14159265358979      Pi approximated better.
x - 62831853/20000000       =   0.00000000358979 via multiplication by 100000000
x - 16/5                    =  -0.05840734641021
x - 22/7                    =  -0.00126448926735
x - 355/113                 =  -0.00000026676419
x - 104348/33215            =  -0.00000000033163
x - 312689/99532            =  -0.00000000002914
x - 1146408/364913          =  -0.00000000000161
x - 5419351/1725033         =  -0.00000000000002

   2.71828182845905      e: rational approximations aren't much use.
x - 271828183/100000000     =  -0.00000000154095 via multiplication by 100000000
x - 3/1                     =  -0.28171817154095
x - 11/4                    =  -0.03171817154095
x - 49/18                   =  -0.00394039376318
x - 87/32                   =  -0.00046817154095
x - 193/71                  =  -0.00002803069588
x - 1457/536                =  -0.00000175363051
x - 9620/3539               =  -0.00000017210609
x - 23225/8544              =  -0.00000000674695
x - 49171/18089             =  -0.00000000027665
x - 566827/208524           =  -0.00000000001154
x - 3820276/1405401         =  -0.00000000000130
x - 11411657/4198114        =  -0.00000000000012

   10.1500000000000      Exact in decimal, recurring in binary.
x - 203/20                  =   0.00000000000000 via multiplication by 100000000
x - 41/4                    =  -0.10000000000000
x - 132/13                  =  -0.00384615384615
x - 203/20                  =   0.00000000000000


 Variations on 67/74
  0.905400000000000      67/74 = 0·9(054) repeating in base 10
x - 4527/5000               =   0.00000000000000 via multiplication by 100000000
x - 1/1                     =  -0.09460000000000
x - 9/10                    =   0.00540000000000
x - 19/21                   =   0.00063809523810
x - 67/74                   =  -0.00000540540541
x - 2029/2241               =   0.00000062472111
x - 4527/5000               =   0.00000000000000

  0.905405400000000      Two repeats.
x - 4527027/5000000         =   0.00000000000000 via multiplication by 100000000
x - 1/1                     =  -0.09459460000000
x - 9/10                    =   0.00540540000000
x - 19/21                   =   0.00064349523810
x - 67/74                   =  -0.00000000540541
x - 2012029/2222241         =   0.00000000067562
x - 2228707/2461557         =   0.00000000008442
x - 2259125/2495153         =   0.00000000001050
x - 2263011/2499445         =   0.00000000000120
x - 2263480/2499963         =   0.00000000000008
x - 4527027/5000000         =   0.00000000000000

  0.905405405400000      Three repeats.
x - 90540541/100000000      =  -0.00000000460000 via multiplication by 100000000
x - 1/1                     =  -0.09459459460000
x - 9/10                    =   0.00540540540000
x - 19/21                   =   0.00064350063810
x - 67/74                   =  -0.00000000000541


 Variations on 14/27
  0.518000000000000      14/27 = 0·(518) repeating in decimal.
x - 259/500                 =   0.00000000000000 via multiplication by 100000000
x - 1/1                     =  -0.48200000000000
x - 1/2                     =   0.01800000000000
x - 13/25                   =  -0.00200000000000
x - 29/56                   =   0.00014285714286
x - 72/139                  =   0.00001438848921
x - 259/500                 =   0.00000000000000

  0.519000000000000      Rounded.
x - 519/1000                =   0.00000000000000 via multiplication by 100000000
x - 1/1                     =  -0.48100000000000
x - 1/2                     =   0.01900000000000
x - 13/25                   =  -0.00100000000000
x - 41/79                   =   0.00001265822785
x - 478/921                 =  -0.00000108577633
x - 519/1000                =   0.00000000000000

  0.518518000000000      Two repeats, truncated.
x - 259259/500000           =   0.00000000000000 via multiplication by 100000000
x - 1/1                     =  -0.48148200000000
x - 1/2                     =   0.01851800000000
x - 13/25                   =  -0.00148200000000
x - 14/27                   =  -0.00000051851852
x - 32929/63506             =   0.00000006468680
x - 36471/70337             =   0.00000000804697
x - 36975/71309             =   0.00000000086946
x - 37031/71417             =   0.00000000008401
x - 185183/357139           =   0.00000000000560
x - 259259/500000           =   0.00000000000000

  0.518519000000000      Two repeats, rounded.
x - 518519/1000000          =   0.00000000000000 via multiplication by 100000000
x - 1/1                     =  -0.48148100000000
x - 1/2                     =   0.01851900000000
x - 13/25                   =  -0.00148100000000
x - 14/27                   =   0.00000048148148
x - 35461/68389             =  -0.00000006008276
x - 39283/75760             =  -0.00000000739176
x - 39815/76786             =  -0.00000000085953
x - 39885/76921             =  -0.00000000001300
x - 478634/923079           =   0.00000000000108
x - 518519/1000000          =   0.00000000000000

```



## FreeBASIC


```FreeBasic
'' Written in FreeBASIC
'' (no error checking, limited to 64-bit signed math)
type number as longint
#define str2num vallng
#define pow10(n) clngint(10 ^ (n))

function gcd(a as number, b as number) as number
    if a = 0 then return b
    return gcd(b mod a, a)
end function


function parserational(n as const string) as string
    dim as string whole, dec, num, denom
    dim as number iwhole, idec, inum, idenom, igcd

    '' find positions of '.', '(' and ')' in code
    dim as integer dpos, r1pos, r2pos
    dpos = instr(n & ".", ".")
    r1pos = instr(n & "(", "(")
    r2pos = instr(n & ")", ")")

    '' extract sections of number (whole, decimal, repeated numerator), generate '999' denominator
    whole = left(n, dpos - 1)
    dec = mid(n, dpos + 1, r1pos - dpos - 1)
    num = mid(n, r1pos + 1, r2pos - r1pos - 1)
    denom = string(len(num), "9"): if denom = "" then denom = "1"

    '' parse sections to integer
    iwhole = str2num(whole)
    idec = str2num(dec)
    inum = str2num(num)
    idenom = str2num(denom)

    '' if whole was negative, decimal and repeated sections need to be negative too
    if left(ltrim(whole), 1) = "-" then idec = -idec: inum = -inum

    '' add decimal part to repeated fraction, and scale down
    inum += idec * idenom
    idenom *= pow10(len(dec))

    '' add integer part to fraction
    inum += iwhole * idenom

    '' simplify fraction
    igcd = abs(gcd(inum, idenom))
    if igcd <> 0 then
        inum \= igcd
        idenom \= igcd
    end if

    return inum & " / " & idenom & " = " & (inum / idenom)

end function

data "0.9(054)", "0.(518)", "-.12(345)", ""
do
    dim as string n
    read n
    if n = "" then exit do
    print n & ":", parserational(n)
loop
```



## Go

Go has no native decimal representation so strings are used as input here.  The program parses it into a Go rational number, which automatically reduces.

```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    for _, d := range []string{"0.9054054", "0.518518", "0.75"} {
        if r, ok := new(big.Rat).SetString(d); ok {
            fmt.Println(d, "=", r)
        } else {
            fmt.Println(d, "invalid decimal number")
        }
    }
}
```

Output:

```txt

0.9054054 = 4527027/5000000
0.518518 = 259259/500000
0.75 = 3/4

```



## Groovy

'''Solution:'''

Uses the ''Rational'' number class and ''RationalCategory'' helper class created in the solution to the [[Arithmetic/Rational#Groovy|Arithmetic/Rational]] task.

'''Test:'''

```groovy
Number.metaClass.mixin RationalCategory

[
    0.9054054, 0.518518, 0.75, Math.E, -0.423310825, Math.PI, 0.111111111111111111111111
].each{
    printf "%30.27f %s\n", it, it as Rational
}
```


'''Output:'''

```txt
 0.905405400000000000000000000 4527027//5000000
 0.518518000000000000000000000 259259//500000
 0.750000000000000000000000000 3//4
 2.718281828459045000000000000 6121026514868073//2251799813685248
-0.423310825000000000000000000 -16932433//40000000
 3.141592653589793000000000000 884279719003555//281474976710656
 0.111111111111111111111111000 111111111111111111111111//1000000000000000000000000
```

Clearly this solution does not make any assumptions or approximations regarding repeating decimals.


## Haskell

Note that the decimal values of the task description are truncated in some cases.

The first map finds the simplest fractions within a given radius, because the floating-point representation is not exact. The second line shows that the numbers could be parsed into fractions at compile time if they are given the right type. The last converts the string representation of the given values directly to fractions.

```haskell
Prelude
 map (\d -> Ratio.approxRational d 0.0001) [0.9054054, 0.518518, 0.75]
[67 % 74,14 % 27,3 % 4]
Prelude> [0.9054054, 0.518518, 0.75] :: [Rational]
[4527027 % 5000000,259259 % 500000,3 % 4]
Prelude> map (fst . head . Numeric.readFloat) ["0.9054054", "0.518518", "0.75"] :: [Rational]
[4527027 % 5000000,259259 % 500000,3 % 4]
```



## J

J's <code>x:</code> built-in will find a rational number which "best matches" a floating point number.


```j
   x: 0.9054054 0.518518 0.75               NB. find "exact" rational representation
127424481939351r140737488355328 866492568306r1671094481399 3r4
```


These numbers are ratios where the integer on the left of the <code>r</code> is the numerator and the integer on the right of the <code>r</code> is the denominator. (Note that this use is in analogy with floating point notion, though it is true that hexadecimal notation and some languages' typed numeric notations use letters within numbers. Using letters rather than other characters makes lexical analysis simpler to remember - both letters and numbers are almost always "word forming characters".)

Note that the concept of "best" has to do with the expected precision of the argument:

```j
   x: 0.9 0.5
9r10 1r2
   x: 0.9054 0.5185
4527r5000 1037r2000
   x: 0.9054054 0.5185185
127424481939351r140737488355328 1037037r2000000
   x: 0.9054054054 0.5185185185
5358191125333r5918002138463 6073341499873r11712872893031
   x: 0.9054054054054 0.5185185185185
67r74 14r27
   x: 0.9054054054054054 0.5185185185185185
67r74 14r27
```


Note that J allows us to specify an epsilon, for the purpose of recognizing a best fit, but the allowed values must be rather small.  In J version 6, the value 5e_11 was nearly the largest epsilon allowed:


```j
   x:(!. 5e_11) 0.9054054054 0.5185185185
67r74 14r27
```


(Note that epsilon will be scaled by magnitude of the largest number involved in a comparison when testing floating point representations of numbers for "equality".  Note also that this J implementation uses 64 bit ieee floating point numbers.)

Here are some other alternatives for dealing with decimals and fractions:


```j
   0j10": x:inv x: 0.9054054 0.518518 0.75  NB. invertible (shown to 10 decimal places)
0.9054054000 0.5185180000 0.7500000000
   0j10": x:inv 67r74 42r81 3r4             NB. decimal representation (shown to 10 decimal places)
0.9054054054 0.5185185185 0.7500000000
   x: x:inv 67r74 42r81 3r4                 NB. invertible
67r74 14r27 3r4
```



## Java


```java
import org.apache.commons.math3.fraction.BigFraction;

public class Test {

    public static void main(String[] args) {
        double[] n = {0.750000000, 0.518518000, 0.905405400, 0.142857143,
            3.141592654, 2.718281828, -0.423310825, 31.415926536};

        for (double d : n)
            System.out.printf("%-12s : %s%n", d, new BigFraction(d, 0.00000002D, 10000));
    }
}
```


```txt
0.75         : 3 / 4
0.518518     : 37031 / 71417
0.9054054    : 67 / 74
0.142857143  : 1 / 7
3.141592654  : 104348 / 33215
2.718281828  : 23225 / 8544
-0.423310825 : -1253 / 2960
31.415926536 : 208696 / 6643
```



## JavaScript

Deriving an approximation within a specified tolerance:

```JavaScript
(() => {
    'use strict';

    const main = () =>
        showJSON(
            map( // Using a tolerance epsilon of 1/10000
                n => showRatio(approxRatio(0.0001)(n)),
                [0.9054054, 0.518518, 0.75]
            )
        );

    // Epsilon -> Real -> Ratio

    // approxRatio :: Real -> Real -> Ratio
    const approxRatio = eps => n => {
        const
            gcde = (e, x, y) => {
                const _gcd = (a, b) => (b < e ? a : _gcd(b, a % b));
                return _gcd(Math.abs(x), Math.abs(y));
            },
            c = gcde(Boolean(eps) ? eps : (1 / 10000), 1, n);
        return Ratio(
            Math.floor(n / c), // numerator
            Math.floor(1 / c) // denominator
        );
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Ratio :: Int -> Int -> Ratio
    const Ratio = (n, d) => ({
        type: 'Ratio',
        'n': n, // numerator
        'd': d // denominator
    });

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // showJSON :: a -> String
    const showJSON = x => JSON.stringify(x, null, 2);

    // showRatio :: Ratio -> String
    const showRatio = nd =>
        nd.n.toString() + '/' + nd.d.toString();

    // MAIN ---
    return main();
})();
```

```txt
[
  "67/74",
  "14/27",
  "3/4"
]
```



## Julia

Julia has a native Rational type, and provides [http://docs.julialang.org/en/latest/manual/conversion-and-promotion/#case-study-rational-conversions a convenience conversion function] that implements a standard algorithm for approximating a floating-point number by a ratio of integers to within a given tolerance, which defaults to machine epsilon.


```Julia
rationalize(0.9054054)
rationalize(0.518518)
rationalize(0.75)
```


 4527027//5000000
 259259//500000
 3//4

Since Julia by default uses its [[wp:Double-precision floating-point format#IEEE 754 double-precision binary floating-point format: binary64|Float64]] type to represent floating-point numbers, if enough decimal digits are provided (so that the difference between the floating-point representation of the resulting fraction and the original number is smaller than the machine epsilon) the smaller fraction is returned, which in this case is the exact result:

 julia> rationalize(0.5185185185185185)
 14//27
 julia> rationalize(0.9054054054054054)
 67//74

Here is the core algorithm written in Julia 1.0, without handling various data types and corner cases:


```Julia
function rat(x::AbstractFloat, tol::Real=eps(x))::Rational
    p, q, pp, qq  = copysign(1,x), 0, 0, 1
    x, y = abs(x), 1.0
    r, a = modf(x)
    nt, t, tt = tol, 0.0, tol

    while r > nt        # convergents of the continued fraction: np//nq = (p*a + pp) // (q*a + qq)
        np, nq = Int(a).*(p,q) .+ (pp,qq)
        p, pp, q, qq = np, p, nq, q

        x, y = y, r     # instead of the inexact 1/r...
        a, r = divrem(x,y)

        t, tt = nt, t   # maintain x = (p + (-1)^i * r) / q
        nt = a*t+tt
    end

    i = Int(cld(x-tt,y+t))   # find optimal semiconvergent: smallest i such that x-i*y < i*t+tt
    return (i*p+pp) // (i*q+qq)
end
```



## Kotlin


```scala
// version 1.1.2

class Rational(val num: Long, val den: Long) {
    override fun toString() = "$num/$den"
}

fun decimalToRational(d: Double): Rational {
    val ds = d.toString().trimEnd('0').trimEnd('.')
    val index = ds.indexOf('.')
    if (index == -1) return Rational(ds.toLong(), 1L)
    var num = ds.replace(".", "").toLong()
    var den = 1L
    for (n in 1..(ds.length - index - 1)) den *= 10L
    while (num % 2L == 0L && den % 2L == 0L) {
        num /= 2L
        den /= 2L
    }
    while (num % 5L == 0L && den % 5L == 0L) {
        num /= 5L
        den /= 5L
    }
    return Rational(num, den)
}

fun main(args: Array<String>) {
    val decimals = doubleArrayOf(0.9054054, 0.518518, 2.405308, .75, 0.0, -0.64, 123.0, -14.6)
    for (decimal in decimals)
        println("${decimal.toString().padEnd(9)} = ${decimalToRational(decimal)}")
}
```


```txt

0.9054054 = 4527027/5000000
0.518518  = 259259/500000
2.405308  = 601327/250000
0.75      = 3/4
0.0       = 0/1
-0.64     = -16/25
123.0     = 123/1
-14.6     = -73/5

```



## Liberty BASIC


```lb

'   Uses convention that one repeating sequence implies infinitely repeating sequence..
'   Non-recurring fractions are limited to nd number of digits in nuerator & denominator

nd =3   '   suggest 3. 4 is slow. >4 is .......
do
    read x$
    data "0.5", "0.1", "0.333", "1 /3", "0.33", "0.14159265", "2^-0.5", "0.1 +0.9*rnd(1)"
    data "0.142857142857", "int( 1000*rnd(1))/int( 1000*rnd(1))","end"   '   always between 0 and 0.999999...
    if x$ ="end" then exit do
    print x$; " is ";
    type$ =check$( x$)
    print type$;

    if type$ ="recurring" then
        x   =val( mid$( x$, 3, ( len( x$) -2) /2))
        rep =( len( x$) -2) /2
        num =x
        den =10^rep -1
        gcd =gcd( num, den)
        print
        print " Calculating exact fraction for ", recurring$( x); " recurring & found";
        print num /gcd; " /"; den /gcd
        print
    else    '   non-recurring. Check numerators & denominators <1000
        x =eval( x$)
        print
        print " Looking for fractions that are close to "; using( "#.############", x); " & found ";
        eps =10^nd
        for n = 1 to nd
            for i =1 to 10^n -1
                for j =i to 10^n -1
                    fr =i /j
                    if abs( x -fr) <eps then
                        eps =abs( x -fr)
                        'print i; " /"; j; " = ", using( "##.############", fr), "with error +/-"; using( "###.#########", eps /x *100); " %"
                        ii =i: jj =j
                        if eps =0 then exit for
                    end if
                next j
                scan
                if eps =0 then exit for
            next i
            if eps =0 then exit for
        next n
        print ii; " /"; jj
        print
    end if
loop until 0

print
print "END."

end

function recurring$( x)
    recurring$ ="0."
    do
        recurring$ =recurring$ +str$( x)
    loop until len( recurring$) >=14
end function

function gcd( a, b)  '   thanks Uncle Ben..
    while b <>0
       t =b
       b =a mod b
       a =t
    wend
    gcd =a
end function

function check$( i$)
    check$ ="non-recurring"
    length =len( i$) -2     '   allow for the '0.'.
    if length /2 =int( length /2) then if mid$( i$, 3, length /2) =mid$( i$, 3 +length /2, length /2) then check$ ="recurring"
end function

```


```txt

 0.5 is non-recurring
 Looking for fractions that are close to 0.500000000000 & found 1 /2

0.1 is non-recurring
 Looking for fractions that are close to 0.100000000000 & found 1 /10

0.333 is non-recurring
 Looking for fractions that are close to 0.333000000000 & found 332 /997

1 /3 is non-recurring
 Looking for fractions that are close to 0.333333333333 & found 1 /3

0.33 is recurring
 Calculating exact fraction for           0.333333333333 recurring & found1 /3

0.14159265 is non-recurring
 Looking for fractions that are close to 0.141592650000 & found 16 /113

2^-0.5 is non-recurring
 Looking for fractions that are close to 0.707106781187 & found 408 /577

0.1 +0.9*rnd(1) is non-recurring
 Looking for fractions that are close to 0.489587274383 & found 47 /96

0.142857142857 is recurring
 Calculating exact fraction for           0.142857142857 recurring & found1 /7

int( 1000*rnd(1))/int( 1000*rnd(1)) is non-recurring
 Looking for fractions that are close to 0.032786885246 & found 2 /61
END.

```



## Maple


```Maple

> map( convert, [ 0.9054054, 0.518518, 0.75 ], 'rational', 'exact' );
                          4527027  259259
                         [-------, ------, 3/4]
                          5000000  500000

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Map[Rationalize[#,0]&,{0.9054054,0.518518, 0.75} ]
-> {4527027/5000000,259259/500000,3/4}
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab

  [a,b]=rat(.75)
  [a,b]=rat(.518518)
  [a,b]=rat(.9054054)

```


Output:

```txt

  >>   [a,b]=rat(.75)
  a =  3
  b =  4
  >>   [a,b]=rat(.518518)
  a =  37031
  b =  71417
  >>   [a,b]=rat(.9054054)
  a =  67
  b =  74

```


=={{header|MK-61/52}}==

<lang>П0	П1	ИП1	{x}	x#0	14	КИП4	ИП1	1	0
*	П1	БП	02	ИП4	10^x	П0	ПA	ИП1	ПB
ИПA	ИПB	/	П9	КИП9	ИПA	ИПB	ПA	ИП9	*
-	ПB	x=0	20	ИПA	ИП0	ИПA	/	П0	ИП1
ИПA	/	П1	ИП0	ИП1	С/П
```



## NetRexx

Now the nearly equivalent program.

```netrexx

/*NetRexx program to convert decimal numbers to fractions *************
* 16.08.2012 Walter Pachl derived from Rexx Version 2
**********************************************************************/
options replace format comments java crossref savelog symbols
  Numeric Digits 10               /* use "only" 10 digs of precision */
  ratt('0.9054054054','67/74')
  ratt('0.5185185185','14/27')
  ratt('0.75'        ,'3/4')
  ratt('0.905405400',' 693627417/766095958')
  ratt('0.9054054054','67/74')
  ratt('0.1428571428','1/7')
  ratt('35.000','35')
  ratt('35.001','35001/1000')
  ratt('0.00000000001','?')
  ratt('0.000001000001','1/999999')

ratt(0.9054054054,'1/3')


method ratt(d = Rexx,fs = Rexx) public static
 fract=rat(d)
 Say '  'd '->' fract
 Parse fract no '/' de
 If de='' Then x=no
          Else x=no/de
 If x<>d Then
   Say '> '||x 'is different'

method rat(in, high='') public static
/**********************************************************************
* rat(number<,high) returns a fraction or an integer that is equal to
* or approximately equal to number.
* Nominator and denominator must not have more than high digits
* 16.08.2012 Walter Pachl derived from Rexx Version 2
**********************************************************************/
  if high=='' then
    high=10**(digits - 1)           /* maximum nominator/denominator */
  x=in                                 /* working copy               */
  nom=0                                /* start values nominator     */
  den=1                                /*              denominator   */
  tnom=1                               /*         temp nominator     */
  tden=0                               /*         temp denominator   */
  loop While tnom<=high & tden<=high   /* nominator... not too large */
    n=x.trunc()                        /* take integer part of x     */
    z=tnom;                            /* save temp nominator        */
    tnom=n*tnom+nom;                   /* compute new temp nominator */
    nom=z                              /* assign nominator           */
    z=tden;                            /* save temp denominator      */
    tden=n*tden+den                    /* compute new temp denominato*/
    den=z                              /* assign denominator         */
    if n=x | tnom/tden=in then do
      if tnom>high | tden>high then    /* temp value(s) too large    */
        Leave                          /* don't use them             */
      nom=tnom                         /* otherwise take them as     */
      den=tden                         /* final values               */
      leave                            /* and end the loop           */
      end
    x=1/(x-n)                          /* compute x for next round   */
    end
  If den=1 Then Return nom             /* an integer                 */
           Else Return nom'/'den       /* otherwise a fraction       */
```

Output is the same as for Rexx Version 2.


## PARI/GP

Quick and dirty.

```parigp
convert(x)={
  my(n=0);
  while(x-floor(x*10^n)/10^n!=0.,n++);
  floor(x*10^n)/10^n
};
```


To convert a number into a rational with a denominator not dividing a power of 10, use <code>contfrac</code> and the Gauss-Kuzmin distribution to distinguish (hopefully!) where to truncate.


## Perl

Note: the following is considerably more complicated than what was specified, because the specification is not, well, specific.  Three methods are provided with different interpretation of what "conversion" means: keeping the string representation the same, keeping machine representation the same, or find best approximation with denominator in a reasonable range.  None of them takes integer overflow seriously (though the best_approx is not as badly subject to it), so not ready for real use.

```perl
sub gcd {
        my ($m, $n) = @_;
        ($m, $n) = ($n, $m % $n) while $n;
        return $m
}

sub rat_machine {
        my $n = shift;
        my $denom = 1;
        while ($n != int $n) {
                # assuming the machine format is base 2, and multiplying
                # by 2 doesn't change the mantissa
                $n *= 2;

                # multiply denom by 2, ignoring (very) possible overflow
                $denom <<= 1;
        }
        if ($n) {
                my $g = gcd($n, $denom);
                $n /= $g;
                $denom /= $g;
        }
        return $n, $denom;
}

# helper, make continued fraction back into normal fraction
sub get_denom {
        my ($num, $denom) = (1, pop @_);
        for (reverse @_) {
                ($num, $denom) = ($denom, $_ * $denom + $num);
        }
        wantarray ? ($num, $denom) : $denom
}

sub best_approx {
        my ($n, $limit) = @_;
        my ($denom, $neg);
        if ($n < 0) {
                $neg = 1;
                $n = -$n;
        }

        my $int = int($n);
        my ($num, $denom, @coef) = (1, $n - $int);

        # continued fraction, sort of
        while (1) {
                # make sure it terminates
                last if $limit * $denom < 1;
                my $i = int($num / $denom);

                # not the right way to get limit, but it works
                push @coef, $i;

                if (get_denom(@coef) > $limit) {
                        pop @coef;
                        last;
                }

                # we lose precision here, but c'est la vie
                ($num, $denom) = ($denom, $num - $i * $denom);
        }

        ($num, $denom) = get_denom @coef;
        $num += $denom * $int;

        return $neg ? -$num : $num, $denom;
}

sub rat_string {
        my $n = shift;
        my $denom = 1;
        my $neg;

        # trival xyz.0000 ... case
        $n =~ s/\.0+$//;
        return $n, 1 unless $n =~ /\./;

        if ($n =~ /^-/) {
                $neg = 1;
                $n =~ s/^-//;
        }

        # shift decimal point to the right till it's gone
        $denom *= 10    while $n =~ s/\.(\d)/$1\./;
        $n =~ s/\.$//;

        # removing leading zeros lest it looks like octal
        $n =~ s/^0*//;
        if ($n) {
                my $g = gcd($n, $denom);
                $n /= $g;
                $denom /= $g;
        }
        return $neg ? -$n : $n, $denom;
}

my $limit = 1e8;
my $x = 3/8;
print "3/8 = $x:\n";
printf "machine: %d/%d\n", rat_machine $x;
printf "string:  %d/%d\n", rat_string  $x;
printf "approx below $limit:  %d/%d\n", best_approx $x, $limit;

$x = 137/4291;
print "\n137/4291 = $x:\n";
printf "machine: %d/%d\n", rat_machine $x;
printf "string:  %d/%d\n", rat_string  $x;
printf "approx below $limit:  %d/%d\n", best_approx $x, $limit;

$x = sqrt(1/2);
print "\n1/sqrt(2) = $x\n";
printf "machine: %d/%d\n", rat_machine $x;
printf "string:  %d/%d\n", rat_string  $x;
printf "approx below 10:  %d/%d\n", best_approx $x, 10;
printf "approx below 100:  %d/%d\n", best_approx $x, 100;
printf "approx below 1000:  %d/%d\n", best_approx $x, 1000;
printf "approx below 10000:  %d/%d\n", best_approx $x, 10000;
printf "approx below 100000:  %d/%d\n", best_approx $x, 100000;
printf "approx below $limit:  %d/%d\n", best_approx $x, $limit;

$x = -4 * atan2(1,1);
print "\n-Pi = $x\n";
printf "machine: %d/%d\n", rat_machine $x;
printf "string:  %d/%d\n", rat_string  $x;

for (map { 10 ** $_ } 1 .. 10) {
        printf "approx below %g: %d / %d\n", $_, best_approx($x, $_)
}
```

Output:
```txt
3/8 = 0.375:
machine: 3/8
string:  3/8
approx below 100000000:  3/8

137/4291 = 0.0319272896760662:
machine: 2300603678209305/72057594037927936
string:  159636448380331/5000000000000000
approx below 100000000:  137/4291

1/sqrt(2) = 0.707106781186548
machine: 6369051672525773/9007199254740992
string:  176776695296637/250000000000000
approx below 10:  5/7
approx below 100:  29/41
approx below 1000:  408/577
approx below 10000:  5741/8119
approx below 100000:  33461/47321
approx below 100000000:  38613965/54608393

-Pi = -3.14159265358979
machine: -884279719003555/281474976710656
string:  -314159265358979/100000000000000
approx below 10: -22 / 7
approx below 100: -22 / 7
approx below 1000: -355 / 113
approx below 10000: -355 / 113
approx below 100000: -208341 / 66317
approx below 1e+06: -1146408 / 364913
approx below 1e+07: -5419351 / 1725033
approx below 1e+08: -245850922 / 78256779
approx below 1e+09: -1881244168 / 598818617
approx below 1e+10: -9978066541 / 3176117225
```



## Perl 6

Decimals are natively represented as rationals in Perl 6, so if the task does not need to handle repeating decimals, it is trivially handled by the <tt>.nude</tt> method, which returns the numerator and denominator:

```perl6
say .nude.join('/') for 0.9054054, 0.518518, 0.75;
```

```txt
4527027/5000000
259259/500000
3/4
```

However, if we want to take repeating decimals into account, then we can get a bit fancier.

```perl6
sub decimal_to_fraction ( Str $n, Int $rep_digits = 0 ) returns Str {
    my ( $int, $dec ) = ( $n ~~ /^ (\d+) \. (\d+) $/ )».Str or die;

    my ( $numer, $denom ) = ( $dec, 10 ** $dec.chars );
    if $rep_digits {
        my $to_move = $dec.chars - $rep_digits;
        $numer -= $dec.substr(0, $to_move);
        $denom -= 10 ** $to_move;
    }

    my $rat = Rat.new( $numer.Int, $denom.Int ).nude.join('/');
    return $int > 0 ?? "$int $rat" !! $rat;
}

my @a = ['0.9054', 3], ['0.518', 3], ['0.75', 0], | (^4).map({['12.34567', $_]});
for @a -> [ $n, $d ] {
    say "$n with $d repeating digits = ", decimal_to_fraction( $n, $d );
}
```

```txt
0.9054 with 3 repeating digits = 67/74
0.518 with 3 repeating digits = 14/27
0.75 with 0 repeating digits = 3/4
12.34567 with 0 repeating digits = 12 34567/100000
12.34567 with 1 repeating digits = 12 31111/90000
12.34567 with 2 repeating digits = 12 17111/49500
12.34567 with 3 repeating digits = 12 1279/3700
```



## Phix


```Phix
function decrat(string s)
integer nom = 0
integer denom = 1
    if s[1..2]!="0." then ?9/0 end if
    for i=3 to length(s) do
        integer ch = s[i]
        if ch<'0' or ch>'9' then ?9/0 end if
        nom = nom*10 + ch-'0'
        denom *= 10
    end for
    return sq_div({nom,denom},gcd(nom,denom))
end function

?decrat("0.9054054")
?decrat("0.518518")
?decrat("0.75")
```

```txt

{4527027,5000000}
{259259,500000}
{3,4}

```



## PHP

```php
function asRational($val, $tolerance = 1.e-6)
{
    if ($val == (int) $val) {
        // integer
        return $val;
    }

    $h1=1;
    $h2=0;
    $k1=0;
    $k2=1;
    $b = 1 / $val;

    do {
        $b = 1 / $b;
        $a = floor($b);
        $aux = $h1;
        $h1 = $a * $h1 + $h2;
        $h2 = $aux;
        $aux = $k1;
        $k1 = $a * $k1 + $k2;
        $k2 = $aux;
        $b = $b - $a;
    } while (abs($val-$h1/$k1) > $val * $tolerance);

    return $h1.'/'.$k1;
}

echo asRational(1/5)."\n"; // "1/5"
echo asRational(1/4)."\n"; // "1/4"
echo asRational(1/3)."\n"; // "1/3"
echo asRational(5)."\n"; // "5"
```



## PL/I

<lang>(size, fofl):
Convert_Decimal_To_Rational: procedure options (main); /* 14 January 2014, from Ada */

Real_To_Rational: procedure (R, Bound, Numerator, Denominator) recursive
         options (reorder);
   declare R float (18), Bound float,
          (Numerator, Denominator) fixed binary (31);
   declare Error float;
   declare Best fixed binary initial (1);
   declare Best_Error float initial (huge(error));
   declare I fixed binary (31);

   if R = 0 then
      do;
         Numerator   = 0;
         Denominator = 1;
         return;
      end;
   else if R < 0 then
      do;
         call Real_To_Rational(-R, Bound, Numerator, Denominator);
         Numerator = -Numerator;
         return;
      end;
   else
      do I = 1 to Bound;
         Error = abs(I * R - trunc(I * R + sign(R)*0.5));
         if Error < Best_Error then
            do;
               Best = I;
               Best_Error = Error;
            end;
      end;

   Denominator = Best;
   Numerator   = Denominator * R + sign(R) * 0.5;

end Real_To_Rational;


   declare (Num, Denom) fixed binary (31);
   declare R float (18);
   declare I fixed BINARY;

   do R = 0.75, 0.25,   0.3333333,   0.518518000,  0.905405400,
          0.142857143,  3.141592654, 2.718281828, -0.423310825,
          31.415926536, 0;
      put skip edit(R) (f(13,9));
      do I = 0 to 4;
         call Real_to_Rational(R, 10**I, Num, Denom);
         put edit('   ' || trim(Num) || ' / ' || trim(Denom)) (a);
      end;
   end;
end Convert_Decimal_To_Rational;
```

Output:

```txt

  0.750000000   1 / 1   3 / 4   3 / 4   3 / 4   3 / 4
  0.250000000   0 / 1   1 / 4   1 / 4   1 / 4   1 / 4
  0.333333300   0 / 1   1 / 3   1 / 3   1 / 3   1 / 3
  0.518518000   1 / 1   1 / 2   14 / 27   14 / 27   14 / 27
  0.905405400   1 / 1   9 / 10   67 / 74   67 / 74   67 / 74
  0.142857143   0 / 1   1 / 7   1 / 7   1 / 7   1 / 7
  3.141592654   3 / 1   22 / 7   22 / 7   355 / 113   355 / 113
  2.718281828   3 / 1   19 / 7   193 / 71   1457 / 536   25946 / 9545
 -0.423310825   0 / 1   -3 / 7   -11 / 26   -69 / 163   -1253 / 2960
 31.415926536   31 / 1   157 / 5   377 / 12   3550 / 113   208696 / 6643
  0.000000000   0 / 1   0 / 1   0 / 1   0 / 1   0 / 1

```



## PureBasic


```purebasic
Procedure.i ggT(a.i, b.i)
  Define t.i : If a < b : Swap a, b : EndIf
  While a%b : t=a : a=b : b=t%a : Wend : ProcedureReturn b
EndProcedure

Procedure.s Dec2Rat(dn.d)
  Define nk$, gt.i, res$
  nk$=Trim(StringField(StrD(dn),2,"."),"0")
  gt=ggT(Val(nk$),Int(Pow(10.0,Len(nk$))))
  res$=Str(Val(nk$)/gt)+"/"+Str(Int(Pow(10.0,Len(nk$)))/gt)
  ProcedureReturn res$
EndProcedure

OpenConsole()
Define d.d
Repeat
  Read.d d : If Not (d>0.0 And d<1.0) : Break : EndIf
  Print(LSet(StrD(d),15," ")+" -> "+#TAB$+Dec2Rat(d)+#CRLF$)
ForEver
Input() : End

DataSection
  Data.d 0.9054054,0.518518,0.75,0.0
EndDataSection
```


```txt

0.9054054       ->      4527027/5000000
0.518518        ->      259259/500000
0.75            ->      3/4

```



## Python

Note that the decimal values of the task description are truncated in some cases.

The first loop limits the size of the denominator, because the floating-point representation is not exact. The second converts the string representation of the given values directly to fractions.

```python
>>
 from fractions import Fraction
>>> for d in (0.9054054, 0.518518, 0.75): print(d, Fraction.from_float(d).limit_denominator(100))

0.9054054 67/74
0.518518 14/27
0.75 3/4
>>> for d in '0.9054054 0.518518 0.75'.split(): print(d, Fraction(d))

0.9054054 4527027/5000000
0.518518 259259/500000
0.75 3/4
>>>
```



Or, writing our own '''approxRatio''' function:
```python
'''Approximate rationals from decimals'''

from math import (floor, gcd)
import sys


# approxRatio :: Float -> Float -> Ratio
def approxRatio(epsilon):
    '''The simplest rational approximation to
       n within the margin given by epsilon.
    '''
    def gcde(e, x, y):
        def _gcd(a, b):
            return a if b < e else _gcd(b, a % b)
        return _gcd(abs(x), abs(y))
    return lambda n: (lambda c=(
        gcde(epsilon if 0 < epsilon else (0.0001), 1, n)
    ): ratio(floor(n / c))(floor(1 / c)))()


# main :: IO ()
def main():
    '''Conversions at different levels of precision.'''

    xs = [0.9054054, 0.518518, 0.75]
    print(
        fTable(__doc__ + ' (epsilon of 1/10000):\n')(str)(
            lambda r: showRatio(r) + ' -> ' + repr(fromRatio(r))
        )(
            approxRatio(1 / 10000)
        )(xs)
    )
    print('\n')

    e = minBound(float)
    print(
        fTable(__doc__ + ' (epsilon of ' + repr(e) + '):\n')(str)(
            lambda r: showRatio(r) + ' -> ' + repr(fromRatio(r))
        )(
            approxRatio(e)
        )(xs)
    )


# GENERIC -------------------------------------------------

# fromRatio :: Ratio Int -> Float
def fromRatio(r):
    '''A floating point value derived from a
       a rational value.
    '''
    return r.get('numerator') / r.get('denominator')


# minBound :: Bounded Type -> a
def minBound(t):
    '''Minimum value for a bounded type.'''
    maxsize = sys.maxsize
    float_infomin = sys.float_info.min
    return {
        int: (-maxsize - 1),
        float: float_infomin,
        bool: False,
        str: chr(0)
    }[t]


# ratio :: Int -> Int -> Ratio Int
def ratio(n):
    '''Rational value constructed
       from a numerator and a denominator.
    '''
    def go(n, d):
        g = gcd(n, d)
        return {
            'type': 'Ratio',
            'numerator': n // g, 'denominator': d // g
        }
    return lambda d: go(n * signum(d), abs(d))


# showRatio :: Ratio -> String
def showRatio(r):
    '''String representation of the ratio r.'''
    d = r.get('denominator')
    return str(r.get('numerator')) + (
        ' / ' + str(d) if 1 != d else ''
    )


# signum :: Num -> Num
def signum(n):
    '''The sign of n.'''
    return -1 if 0 > n else (1 if 0 < n else 0)


# DISPLAY -------------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Approximate rationals from decimals (epsilon of 1/10000):

0.9054054 -> 67 / 74 -> 0.9054054054054054
 0.518518 -> 14 / 27 -> 0.5185185185185185
     0.75 -> 3 / 4 -> 0.75


Approximate rationals from decimals (epsilon of 2.2250738585072014e-308):

0.9054054 -> 4077583422059235 / 4503599627370496 -> 0.9054054
 0.518518 -> 2335197471584895 / 4503599627370496 -> 0.518518
     0.75 -> 3 / 4 -> 0.75
```



## R


```R

ratio<-function(decimal){
  denominator=1
  while(nchar(decimal*denominator)!=nchar(round(decimal*denominator))){
    denominator=denominator+1
  }
  str=paste(decimal*denominator,"/",sep="")
  str=paste(str,denominator,sep="")
  return(str)
}

```

```txt

> ratio(.75)
[1] "3/4"
> ratio(0.9054054)
[1] "4527027/5e+06"
> ratio(3.14)
[1] "157/50"

```



## Racket

Racket has builtin exact and inexact representantions of numbers, 3/4 is a valid number syntactically, and one can change between the exact and inexact values with the functions showed in the example.
They have some amount of inaccuracy, but i guess it can be tolerated.

```Racket
#lang racket

(inexact->exact 0.75)  ; -> 3/4
(exact->inexact 3/4)   ; -> 0.75

(exact->inexact 67/74) ; -> 0.9054054054054054
(inexact->exact 0.9054054054054054) ;-> 8155166892806033/9007199254740992
```



## REXX


### version 1

This REXX example supports almost any form of numeric input,   some examples are:
::*   ±nnn
::*   ±nnn<b>.</b>
::*   ±nnn<b>.</b>fff
::*   ±<b>.</b>fff
::*   ±nnnE±ppp
::*   <b>.</b>fffE±ppp
::*   ±nnn<b>.</b>fffE±ppp       (with an uppercase exponent signifier)
::*   ±nnn<b>.</b>fffe±ppp       (with an lowercase exponent signifier)
::*   numeratorNumber/denominatorNumber
::*   denominator is optional   (but if a   <big>'''/'''</big>   is used, it must be present)
::*   superfluous blanks are permitted (for whitespace)
::*   leading zeroes are permitted
::*   leading signs are permitted
::*   improper fractions are permitted

─── where:
:* '''nnn'''                   represent decimal digits       before        the decimal point (if there is one)
:* '''fff'''              represent decimal digits   after   the decimal point (if there is one)
:* '''ppp'''                   represent decimal digits of the (base ten) exponent
:*   '''<big>±</big>'''     represent an optional (leading) sign, either   '''+'''   or   '''-'''
:*   '''<b>.</b>'''           if it's trailing, the decimal point is optional


REXX can support almost any number of decimal digits, but   '''10'''   was chosen for practicality for this task.

```rexx
/*REXX program converts a rational fraction  [n/m]  (or  nnn.ddd)  to it's lowest terms.*/
numeric digits 10                                /*use ten decimal digits of precision. */
parse arg  orig 1 n.1  "/"  n.2;       if n.2=''  then n.2=1         /*get the fraction.*/
if n.1=''  then call er 'no argument specified.'

  do j=1  for 2;     if \datatype(n.j, 'N')  then call er  "argument isn't numeric:"   n.j
  end   /*j*/                                    /* [↑]  validate arguments:  n.1  n.2  */

if n.2=0  then call er "divisor can't be zero."  /*Whoa!   We're dividing by zero !     */
say 'old ='    space(orig)                       /*display the original fraction.       */
say 'new ='    rat(n.1/n.2)                      /*display the result ──► terminal.     */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
er:  say;      say '***error***';     say;    say arg(1);    say;       exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
rat: procedure;  parse arg x 1 _x,y;          if y==''  then y = 10**(digits()-1)
     b=0;  g=0;  a=1;  h=1                               /* [↑]    Y   is the tolerance.*/
                            do  while  a<=y & g<=y;   n=trunc(_x)
                            _=a;   a=n*a+b;   b=_
                            _=g;   g=n*g+h;   h=_
                            if n=_x | a/g=x then do;  if a>y | g>y  then iterate
                                                      b=a;     h=g;      leave
                                                 end
                            _x=1/(_x-n)
                            end   /*while*/
     if h==1  then return b                              /*don't return number  ÷  by 1.*/
                   return b'/'h                          /*proper or improper fraction. */
```

'''output'''   when using various inputs (which are displayed as part of the output):

(Multiple runs are shown, outputs are separated by a blank line.)

```txt

old = 0.9054054054
new = 67/74

old = 0.5185185185
new = 14/27

old = 0.75
new = 3/4

old = 0.905405400
new = 693627417/766095958

old = 0.9054054054
new = 67/74

old = 0.1428571428
new = 1/7

```



### version 2


```rexx
/*REXX program to convert decimal numbers to fractions ****************
* 15.08.2012 Walter Pachl derived from above for readability
* It took me time to understand :-) I need descriptive variable names
* Output shows where the fraction only approximates the number
* due to the limit (high) imposed on nominator and denominator
**********************************************************************/
  Numeric Digits 10               /* use "only" 10 digs of precision */
  Call test '0.9054054054','67/74'
  Call test '0.5185185185','14/27'
  Call test '0.75'        ,'3/4'
  Call test '0.905405400',' 693627417/766095958'
  Call test '0.9054054054','67/74'
  Call test '0.1428571428','1/7'
  Call test '35.000','35'
  Call test '35.001','35001/1000'
  Call test '0.00000000001','?'
  Call test '0.000001000001','1/999999'
  Exit

test:
/**********************************************************************
* Test driver for rat
**********************************************************************/
  Parse Arg d,fs                     /* number and expected fraction */
  fh=rat(d)                          /* convert number to fracrion   */
  Call o '  'd fh
  If fh<>fs Then Call o '                   not='fs
  interpret 'x='fh                   /* compute value of fraction    */
  If x<>d Then                       /* not exactly equal to number  */
    Call o '> '||x 'is different'
  Call o ' '
  Return

o: Say arg(1); Return

rat: procedure
/**********************************************************************
* rat(number<,high) returns a fraction or an integer that is equal to
* or approximately equal to number.
* Nominator and denominator must not have more than high digits
* 15.08.2012 Walter Pachl derived from Version 1
**********************************************************************/
parse arg in,high
  x=in                                 /* working copy               */
  if high=='' then
    high=10**(digits()-1)           /* maximum nominator/denominator */
  nom=0                                /* start values nominator     */
  den=1                                /*              denominator   */
  tnom=1                               /*         temp nominator     */
  tden=0                               /*         temp denominator   */
  do While tnom<=high & tden<=high     /* nominator... not too large */
    n=trunc(x)                         /* take integer part of x     */
    z=tnom;                            /* save temp nominator        */
    tnom=n*tnom+nom;                   /* compute new temp nominator */
    nom=z                              /* assign nominator           */
    z=tden;                            /* save temp denominator      */
    tden=n*tden+den                    /* compute new temp denominato*/
    den=z                              /* assign denominator         */
    if n=x | tnom/tden=in then do
      if tnom>high | tden>high then    /* temp value(s) too large    */
        Leave                          /* don't use them             */
      nom=tnom                         /* otherwise take them as     */
      den=tden                         /* final values               */
      leave                            /* and end the loop           */
      end
    x=1/(x-n)                          /* compute x for next round   */
    end
  if den=1 then return nom             /* denominator 1: integer     */
                return nom'/'den       /* otherwise a fraction       */

```

Output:

```txt

  0.9054054054 67/74

  0.5185185185 14/27

  0.75 3/4

  0.905405400 693627417/766095958
> 0.9054053996 is different

  0.9054054054 67/74

  0.1428571428 1/7
> 0.1428571429 is different

  35.000 35

  35.001 35001/1000

  0.00000000001 0
                   not=?
> 0 is different

  0.000001000001 1/999999

```



### version 3


```rexx
/* REXX ---------------------------------------------------------------
* 13.02.2014 Walter Pachl
* specify the number as xxx.yyy(pqr) pqr is the period
*        for the number xxx.yyypqrpqrpqrpqrpqr...
*--------------------------------------------------------------------*/
Numeric Digits 100
Call test '5.55555','111111/20000'
Call test '3','3'
Call test '0.03','3/100'
Call test '0.9(054)','67/74'
Call test '0.(3)','1/3'
Call test '5.28(571428)','37/7'
Call test '5.28(571428)','38/7 (demonstrate error case)'
Call test '0.(518)','14/27'
Call test '0.75'        ,'3/4'
Call test '0.(142857)','1/7'
Call test '0.1(428571)','1/7'
Call test '35.000','35'
Call test '35.001','35001/1000'
Call test '0.00000000001','1/100000000000'
Call test '0.000001000001','1000001/1000000000000'
Exit
test:
  Parse Arg z, soll
  zin=z
  If pos('(',z)=0 Then Do
    Parse Var z i '.' f
    z=i||f
    n=10**length(f)
    End
  Else Do
    lp=pos('(',z)-3
    rp=pos(')',z)-4
    x=space(translate(z,'  ','()'),0)
    z1=x*10**lp
    Parse Var z1 z1 '.'
    z2=x*10**rp
    z=z2-z1
    n=10**rp-10**lp
    End
  dd=gcd(z,n)
  zz=z/dd
  nn=n/dd
  If nn=1 Then
    fract=zz
  Else
    fract=zz'/'nn
  If fract==soll Then
    tag='ok'
  Else
    tag='should be' soll
  say zin '=' fract tag
   Return

GCD: procedure
/**********************************************************************
* Recursive procedure
**********************************************************************/
Parse Arg a,b
if b = 0 then return abs(a)
return GCD(b,a//b)
```

'''Output:'''

```txt
5.55555 = 111111/20000 ok
3 = 3 ok
0.03 = 3/100 ok
0.9(054) = 67/74 ok
0.(3) = 1/3 ok
5.28(571428) = 37/7 ok
5.28(571428) = 37/7 should be 38/7 (demonstrate error case)
0.(518) = 14/27 ok
0.75 = 3/4 ok
0.(142857) = 1/7 ok
0.1(428571) = 1/7 ok
35.000 = 35 ok
35.001 = 35001/1000 ok
0.00000000001 = 1/100000000000 ok
0.000001000001 = 1000001/1000000000000 ok
```



## Ruby

Note that the decimal values of the task description are truncated in some cases.

This converts the string representation of the given values directly to fractions.

```ruby

 '0.9054054 0.518518 0.75'.split.each { |d| puts "%s %s" % [d, Rational(d)] }
0.9054054 4527027/5000000
0.518518 259259/500000
0.75 3/4
=> ["0.9054054", "0.518518", "0.75"]
```


This loop finds the simplest fractions within a given radius, because the floating-point representation is not exact.

```ruby
[0.9054054, 0.518518, 0.75].each { |f| puts "#{f} #{f.rationalize(0.0001)}" }
# =>0.9054054 67/74
# =>0.518518 14/27
# =>0.75 3/4

```

A suffix for integer and float literals was introduced:


```txt

2.1.0p0 :001 > 0.9054054r
 => (4527027/5000000)
2.1.0p0 :002 > 0.518518r
 => (259259/500000)
2.1.0p0 :003 > 0.75r
 => (3/4)

```



## Rust

<lang>
extern crate rand;
extern crate num;

use num::Integer;
use rand::Rng;

fn decimal_to_rational (mut n : f64) -> [isize;2] {
    //Based on Farey sequences
    assert!(n.is_finite());
    let flag_neg  = n < 0.0;
    if flag_neg { n = n*(-1.0) }
    if n < std::f64::MIN_POSITIVE { return [0,1] }
    if (n - n.round()).abs() < std::f64::EPSILON { return [n.round() as isize, 1] }
    let mut a : isize = 0;
    let mut b : isize = 1;
    let mut c : isize = n.ceil() as isize;
    let mut d : isize = 1;
    let aux1 = isize::max_value()/2;
    while c < aux1  && d < aux1 {
        let aux2 : f64 = (a as f64 + c as f64)/(b as f64 + d as f64);
        if (n - aux2).abs() < std::f64::EPSILON { break }
        if n > aux2 {
            a = a + c;
            b = b + d;
        } else {
            c = a + c;
            d = b + d;
        }
    }
    // Make sure that the fraction is irreducible
    let gcd = (a+c).gcd(&(b+d));
    if flag_neg { [-(a + c)/gcd, (b + d)/gcd] } else { [(a + c)/gcd, (b + d)/gcd] }
}

#[test]
fn test1 () {
    // Test the function with 1_000_000 random decimal numbers
    let mut rng = rand::thread_rng();
    for _i in 1..1_000_000 {
        let number = rng.gen::<f64>();
        let result = decimal_to_rational(number);
        assert!((number - (result[0] as f64)/(result[1] as f64)).abs() < std::f64::EPSILON);
        assert!(result[0].gcd(&result[1]) == 1);
    }
}

fn main () {
    let mut rng = rand::thread_rng();
    for _i in 1..10 {
        let number = rng.gen::<f64>();
        let result = decimal_to_rational(number);
        if result[1] == 1 { println!("{} -> {}", number, result[0]) } else { println!("{} ->  {}/{}", number, result[0], result[1]) }
    }
    for i in [-0.9054054, 0.518518, -0.75, 0.5185185185185185, -0.9054054054054054, 0.0, 1.0, 2.0].iter() {
        let result = decimal_to_rational(*i as f64);
        if result[1] == 1 { println!("{} = {}",*i, result[0]) } else { println!("{} =  {}/{}", *i, result[0], result[1]) }
    }
}

```

First test the function with 1_000_000 random double floats :

```txt

running 1 test
test test1 ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out

```

Now run it with 10 random double floats and some selected ones printing the results

```txt

0.47783621261626297 -> 36036136/75415247
0.29135687639237284 -> 164756020/565478399
0.04962905490905656 -> 4105111/82715881
0.33418703921783965 -> 16612678/49710719
0.07284921759788943 -> 6252865/85832974
0.783712619202954 -> 62096920/79234299
0.3788902482801324 -> 30401287/80237713
0.03780115715370047 -> 1522201/40268635
0.9975233883406127 -> 79858287/80056556
-0.9054054 ->  -4527027/5000000
0.518518 ->  259259/500000
-0.75 ->  -3/4
0.5185185185185185 ->  14/27
-0.9054054054054054 ->  -67/74
0 -> 0
1 -> 1
2 -> 2

```



## Scala

{{Out}}Best seen running in your browser [https://scastie.scala-lang.org/rrlFnuTURgirBiTsH3Kqrg Scastie (remote JVM)].

```Scala
import org.apache.commons.math3.fraction.BigFraction

object Number2Fraction extends App {
  val n = Array(0.750000000, 0.518518000, 0.905405400,
    0.142857143, 3.141592654, 2.718281828, -0.423310825, 31.415926536)
  for (d <- n)
    println(f"$d%-12s : ${new BigFraction(d, 0.00000002D, 10000)}%s")
}
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/bigrat.htm bigrat.s7i]
defines the operator [http://seed7.sourceforge.net/libraries/bigrat.htm#%28attr_bigRational%29parse%28in_var_string%29 parse],
which accepts, besides fractions, also a decimal number with repeating decimals.

```seed7
$ include "seed7_05.s7i";
  include "bigrat.s7i";

const proc: main is func
  begin
    writeln(bigRational parse "0.9(054)");
    writeln(bigRational parse "0.(518)");
    writeln(bigRational parse "0.75");
    writeln(bigRational parse "3.(142857)");
    writeln(bigRational parse "0.(8867924528301)");
    writeln(bigRational parse "0.(846153)");
    writeln(bigRational parse "0.9054054");
    writeln(bigRational parse "0.518518");
    writeln(bigRational parse "0.14285714285714");
    writeln(bigRational parse "3.14159265358979");
    writeln(bigRational parse "2.718281828");
    writeln(bigRational parse "31.415926536");
    writeln(bigRational parse "0.000000000");
  end func;
```

```txt
67/74
14/27
3/4
22/7
47/53
11/13
4527027/5000000
259259/500000
7142857142857/50000000000000
314159265358979/100000000000000
679570457/250000000
3926990817/125000000
0/1
```



## Sidef

This can be done by using the ''to_r'' method, which converts a scalar-object into a rational number:

```ruby
'0.9054054 0.518518 0.75'.split.each { |d|
    say d.num.as_rat;
}
```


Another way is by calling the ''rat'' method on Number objects:

```ruby
say 0.9054054.as_rat;
say 0.518518.as_rat;
say 0.75.as_rat;
```


```txt

4527027/5000000
259259/500000
3/4

```



## Tcl

Here is a complete script with the implemented function and a small test suite (which is executed when this script is called directly from a shell) - originally on http://wiki.tcl.tk/752:

```Tcl
#!/usr/bin/env tclsh

 proc dbl2frac {dbl {eps 0.000001}} {
   for {set den 1} {$den<1024} {incr den} {
      set num [expr {round($dbl*$den)}]
      if {abs(double($num)/$den - $dbl) < $eps} break
   }
   list $num $den
 }
#-------------------- That's all... the rest is the test suite
if {[file tail $argv0] eq [file tail [info script]]} {
    foreach {test            -> expected} {
	{dbl2frac 0.518518}  -> {42 81}
	{dbl2frac 0.75}      -> {3 4}
	{dbl2frac 0.9054054} -> {67 74}
    } {
	catch $test res
	if {$res ne $expected} {
	    puts "$test -> $res, expected $expected"
	}
    }
}
```

Running it shows one unexpected result, but on closer examination it is clear that 14/27 equals 42/81, so it should indeed be the right solution:
 ~ $ fractional.tcl
 dbl2frac 0.518518 -> 14 27, expected 42 81
 ~ $


## zkl

```zkl
fcn real2Rational(r,bound){
   if (r == 0.0) return(0,1);
   if (r < 0.0){
      result := real2Rational(-r, bound);
      return(-result[0],result[1]);
   } else {
      best,bestError := 1,(1.0).MAX;
      foreach i in ([1 .. bound + 1]){
         error := (r*i - (r*i).round()).abs();
	 if (error < bestError) best,bestError = i,error;
      }
      return((r*best).round().toInt(),best);
   }
}
```


```zkl
tests := T(0.750000000,  0.518518000, 0.905405400,
	   0.142857143,  3.141592654, 2.718281828,
	  -0.423310825, 31.415926536);
foreach r in (tests) {
   print("%8.9f  ".fmt(r));
   foreach i in (6)
      { print("  %d/%d".fmt(real2Rational(r,(10).pow(i)).xplode())) }
   println();
}
```

```txt

0.750000000    1/1  3/4  3/4  3/4  3/4  3/4
0.518518000    1/2  1/2  14/27  14/27  14/27  37031/71417
0.905405400    1/1  10/11  67/74  67/74  67/74  67/74
0.142857143    0/1  1/7  1/7  1/7  1/7  1/7
3.141592654    3/1  22/7  22/7  355/113  355/113  104348/33215
2.718281828    3/1  19/7  193/71  2721/1001  25946/9545  222630/81901
-0.423310825    -1/2  -3/7  -11/26  -69/163  -1253/2960  -10093/23843
31.415926536    63/2  157/5  3173/101  3550/113  208696/6643  2918194/92889

```

