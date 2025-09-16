+++
title = "Van der Corput sequence"
description = ""
date = 2019-03-21T19:18:03Z
aliases = []
[extra]
id = 9349
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "360_assembly",
  "actionscript",
  "ada",
  "autohotkey",
  "awk",
  "bbc_basic",
  "bc",
  "c",
  "c#",
  "c++",
  "clojure",
  "common_lisp",
  "d",
  "ela",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "maxima",
  "pari_gp",
  "pascal",
  "perl_6",
  "perl",
  "phix",
  "picolisp",
  "pl_i",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "seed7",
  "sidef",
  "stata",
  "swift",
  "tcl",
  "vba",
  "vbscript",
  "visual_basic_.net",
  "xpl0",
  "zkl",
]
+++

## Description
When counting integers in binary, if you put a (binary) point to the right of the count then the column immediately to the left denotes a digit with a multiplier of <math>2^0</math>; the digit in the next column to the left has a multiplier of <math>2^1</math>; and so on.

So in the following table:

```txt
  0.
  1.
 10.
 11.
 ...
```

the binary number "<code>10</code>" is <math>1 \times 2^1 + 0 \times 2^0</math>.

You can also have binary digits to the right of the “point”, just as in the decimal number system. In that case, the digit in the place immediately to the right of the point has a weight of <math>2^{-1}</math>, or <math>1/2</math>.
The weight for the second column to the right of the point is <math>2^{-2}</math> or <math>1/4</math>. And so on.

If you take the integer binary count of the first table, and ''reflect'' the digits about the binary point, you end up with '''the van der Corput sequence of numbers in base 2'''.


```txt
  .0
  .1
  .01
  .11
  ...
```


The third member of the sequence, binary <code>0.01</code>, is therefore <math>0 \times 2^{-1} + 1 \times 2^{-2}</math> or <math>1/4</math>.


 [[File:Van der corput distribution.png|400|thumb|right|Distribution of 2500 points each: Van der Corput (top) vs pseudorandom]] Members of the sequence lie within the interval <math>0 \leq x < 1</math>. Points within the sequence tend to be evenly distributed which is a useful trait to have for [[wp:Monte Carlo method|Monte Carlo simulations]].
This sequence is also a superset of the numbers representable by the "fraction" field of [[wp:IEEE 754-1985|an old IEEE floating point standard]]. In that standard, the "fraction" field represented the fractional part of a binary number beginning with "1." e.g. 1.101001101.

'''Hint'''

A ''hint'' at a way to generate members of the sequence is to modify a routine used to change the base of an integer:

```python
>>>
 def base10change(n, base):
	digits = []
	while n:
		n,remainder = divmod(n, base)
		digits.insert(0, remainder)
	return digits

>>> base10change(11, 2)
[1, 0, 1, 1]
```

the above showing that <code>11</code> in decimal is <math>1\times 2^3 + 0\times 2^2 + 1\times 2^1 + 1\times 2^0</math>.

Reflected this would become <code>.1101</code> or <math>1\times 2^{-1} + 1\times 2^{-2} + 0\times 2^{-3} + 1\times 2^{-4}</math>


## Task
* Create a function/method/routine that given ''n'', generates the ''n'''th term of the van der Corput sequence in base 2.
* Use the function to compute ''and display'' the first ten members of the sequence. (The first member of the sequence is for ''n''=0).

* As a stretch goal/extra credit, compute and show members of the sequence for bases other than 2.



## See also
* [http://www.puc-rio.br/marco.ind/quasi_mc.html#low_discrep The Basic Low Discrepancy Sequences]
* [[Non-decimal radices/Convert]]
* [[wp:Van der Corput sequence|Van der Corput sequence]]





## 360 Assembly

{{trans|BBC BASIC}}
The program uses two ASSIST macros (XDECO,XPRNT) to keep the code as short as possible.

```360asm
*        Van der Corput sequence   31/01/2017
VDCS     CSECT
         USING  VDCS,R13           base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
         ZAP    B,=P'2'            b=2  (base)
         ZAP    M,=P'-1'           m=-1
         SR     R6,R6              i=0
LOOPI    CH     R6,=H'10'          do i=0 to 10
         BH     ELOOPI
         AP     M,=P'1'            w=m+1
         ZAP    V,=P'0'            v=0
         ZAP    S,=P'1'            s=1
         ZAP    N,M                n=m
WHILE    CP     N,=P'0'            do while n<>0
         BE     EWHILE
         MP     S,B                s=s*b
         ZAP    PL16,N             n
         DP     PL16,B             n/b
         ZAP    W,PL16+8(8)        w=n mod b
         MP     W,=P'100000'       *100000
         ZAP    PL16,W             w
         DP     PL16,S             w/s
         ZAP    W,PL16(8)          w=w/s
         AP     V,W                v=v+(n mod b)*100000/s
         ZAP    PL16,N             n
         DP     PL16,B             n/b
         ZAP    N,PL16(8)          n=n/b
         B      WHILE
EWHILE   XDECO  R6,XDEC            edit i
         MVC    PG+0(3),XDEC+9     output i
         MVC    PG+3(3),=C' 0.'
         UNPK   Z,V                unpack v
         OI     Z+L'Z-1,X'F0'      edit v
         MVC    PG+6(5),Z+11       output v  (v/100000)
         XPRNT  PG,L'PG            print buffer
         LA     R6,1(R6)           i=i+1
         B      LOOPI
ELOOPI   L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
B        DS     PL8
M        DS     PL8
V        DS     PL8
S        DS     PL8
N        DS     PL8
W        DS     PL8                packed
Z        DS     ZL16               zoned
PL16     DS     PL16               packed max
PG       DC     CL80' '            buffer
XDEC     DS     CL12               work area for xdeco
         YREGS
         END    VDCS
```

{{out}}

```txt

  0 0.00000
  1 0.50000
  2 0.25000
  3 0.75000
  4 0.12500
  5 0.62500
  6 0.37500
  7 0.87500
  8 0.06250
  9 0.56250
 10 0.31250

```



## ActionScript

This implementation uses logarithms to computes the nth term of the sequence at any base. Numbers in the output are rounded to 6 decimal places to hide any floating point inaccuracies.

```ActionScript3

package {

    import flash.display.Sprite;
    import flash.events.Event;

    public class VanDerCorput extends Sprite {

        public function VanDerCorput():void {
            if (stage) init();
            else addEventListener(Event.ADDED_TO_STAGE, init);
        }

        private function init(e:Event = null):void {

            removeEventListener(Event.ADDED_TO_STAGE, init);

            var base2:Vector.<Number> = new Vector.<Number>(10, true);
            var base3:Vector.<Number> = new Vector.<Number>(10, true);
            var base4:Vector.<Number> = new Vector.<Number>(10, true);
            var base5:Vector.<Number> = new Vector.<Number>(10, true);
            var base6:Vector.<Number> = new Vector.<Number>(10, true);
            var base7:Vector.<Number> = new Vector.<Number>(10, true);
            var base8:Vector.<Number> = new Vector.<Number>(10, true);

            var i:uint;

            for ( i = 0; i < 10; i++ ) {
                base2[i] = Math.round( _getTerm(i, 2) * 1000000 ) / 1000000;
                base3[i] = Math.round( _getTerm(i, 3) * 1000000 ) / 1000000;
                base4[i] = Math.round( _getTerm(i, 4) * 1000000 ) / 1000000;
                base5[i] = Math.round( _getTerm(i, 5) * 1000000 ) / 1000000;
                base6[i] = Math.round( _getTerm(i, 6) * 1000000 ) / 1000000;
                base7[i] = Math.round( _getTerm(i, 7) * 1000000 ) / 1000000;
                base8[i] = Math.round( _getTerm(i, 8) * 1000000 ) / 1000000;
            }

            trace("Base 2: " + base2.join(', '));
            trace("Base 3: " + base3.join(', '));
            trace("Base 4: " + base4.join(', '));
            trace("Base 5: " + base5.join(', '));
            trace("Base 6: " + base6.join(', '));
            trace("Base 7: " + base7.join(', '));
            trace("Base 8: " + base8.join(', '));

        }

        private function _getTerm(n:uint, base:uint = 2):Number {

            var r:Number = 0, p:uint, digit:uint;
            var baseLog:Number = Math.log(base);

            while ( n > 0 ) {
                p = Math.pow( base, uint(Math.log(n) / baseLog) );

                digit = n / p;
                n %= p;
                r += digit / (p * base);
            }

            return r;

        }

    }

}

```

{{out}}

```txt

Base 2: 0, 0.5, 0.25, 0.75, 0.125, 0.625, 0.375, 0.875, 0.0625, 0.5625
Base 3: 0, 0.333333, 0.666667, 0.111111, 0.444444, 0.777778, 0.222222, 0.555556, 0.888889, 0.037037
Base 4: 0, 0.25, 0.5, 0.75, 0.0625, 0.3125, 0.5625, 0.8125, 0.125, 0.375
Base 5: 0, 0.2, 0.4, 0.6, 0.8, 0.04, 0.24, 0.44, 0.64, 0.84
Base 6: 0, 0.166667, 0.333333, 0.5, 0.666667, 0.833333, 0.027778, 0.194444, 0.361111, 0.527778
Base 7: 0, 0.142857, 0.285714, 0.428571, 0.571429, 0.714286, 0.857143, 0.020408, 0.163265, 0.306122
Base 8: 0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.015625, 0.140625

```



## Ada


```Ada
with Ada.Text_IO;

procedure Main is
   package Float_IO is new Ada.Text_IO.Float_IO (Float);
   function Van_Der_Corput (N : Natural; Base : Positive := 2) return Float is
      Value    : Natural  := N;
      Result   : Float    := 0.0;
      Exponent : Positive := 1;
   begin
      while Value > 0 loop
         Result   := Result +
                     Float (Value mod Base) / Float (Base ** Exponent);
         Value    := Value / Base;
         Exponent := Exponent + 1;
      end loop;
      return Result;
   end Van_Der_Corput;
begin
   for Base in 2 .. 5 loop
      Ada.Text_IO.Put ("Base" & Integer'Image (Base) & ":");
      for N in 1 .. 10 loop
         Ada.Text_IO.Put (' ');
         Float_IO.Put (Item => Van_Der_Corput (N, Base), Exp => 0);
      end loop;
      Ada.Text_IO.New_Line;
   end loop;
end Main;
```


{{out}}

```txt
Base 2:  0.50000  0.25000  0.75000  0.12500  0.62500  0.37500  0.87500  0.06250  0.56250  0.31250
Base 3:  0.33333  0.66667  0.11111  0.44444  0.77778  0.22222  0.55556  0.88889  0.03704  0.37037
Base 4:  0.25000  0.50000  0.75000  0.06250  0.31250  0.56250  0.81250  0.12500  0.37500  0.62500
Base 5:  0.20000  0.40000  0.60000  0.80000  0.04000  0.24000  0.44000  0.64000  0.84000  0.08000
```



## AutoHotkey

{{works with|AutoHotkey_L}}

```AutoHotkey
SetFormat, FloatFast, 0.5
for i, v in [2, 3, 4, 5, 6] {
    seq .= "Base " v ": "
    Loop, 10
        seq .= VanDerCorput(A_Index - 1, v) (A_Index = 10 ? "`n" : ", ")
}
MsgBox, % seq

VanDerCorput(n, b, r=0) {
    while n
        r += Mod(n, b) * b ** -A_Index, n := n // b
    return, r
}
```

{{out}}

```txt
Base 2: 0, 0.50000, 0.25000, 0.75000, 0.12500, 0.62500, 0.37500, 0.87500, 0.06250, 0.56250
Base 3: 0, 0.33333, 0.66667, 0.11111, 0.44444, 0.77778, 0.22222, 0.55555, 0.88889, 0.03704
Base 4: 0, 0.25000, 0.50000, 0.75000, 0.06250, 0.31250, 0.56250, 0.81250, 0.12500, 0.37500
Base 5: 0, 0.20000, 0.40000, 0.60000, 0.80000, 0.04000, 0.24000, 0.44000, 0.64000, 0.84000
Base 6: 0, 0.16667, 0.33333, 0.50000, 0.66667, 0.83333, 0.02778, 0.19445, 0.36111, 0.52778
```


## AWK


```AWK

# syntax: GAWK -f VAN_DER_CORPUT_SEQUENCE.AWK
# converted from BBC BASIC
BEGIN {
    printf("base")
    for (i=0; i<=9; i++) {
      printf(" %7d",i)
    }
    printf("\n")
    for (base=2; base<=5; base++) {
      printf("%-4s",base)
      for (i=0; i<=9; i++) {
        printf(" %7.5f",vdc(i,base))
      }
      printf("\n")
    }
    exit(0)
}
function vdc(n,b,  s,v) {
    s = 1
    while (n) {
      s *= b
      v += (n % b) / s
      n /= b
      n = int(n)
    }
    return(v)
}

```

<p>Output:</p>

```txt

base       0       1       2       3       4       5       6       7       8       9
2    0.00000 0.50000 0.25000 0.75000 0.12500 0.62500 0.37500 0.87500 0.06250 0.56250
3    0.00000 0.33333 0.66667 0.11111 0.44444 0.77778 0.22222 0.55556 0.88889 0.03704
4    0.00000 0.25000 0.50000 0.75000 0.06250 0.31250 0.56250 0.81250 0.12500 0.37500
5    0.00000 0.20000 0.40000 0.60000 0.80000 0.04000 0.24000 0.44000 0.64000 0.84000

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      @% = &20509
      FOR base% = 2 TO 5
        PRINT "Base " ; STR$(base%) ":"
        FOR number% = 0 TO 9
          PRINT FNvdc(number%, base%);
        NEXT
        PRINT
      NEXT
      END

      DEF FNvdc(n%, b%)
      LOCAL v, s%
      s% = 1
      WHILE n%
        s% *= b%
        v += (n% MOD b%) / s%
        n% DIV= b%
      ENDWHILE
      = v
```

{{out}}

```txt

Base 2:
  0.00000  0.50000  0.25000  0.75000  0.12500  0.62500  0.37500  0.87500  0.06250  0.56250
Base 3:
  0.00000  0.33333  0.66667  0.11111  0.44444  0.77778  0.22222  0.55556  0.88889  0.03704
Base 4:
  0.00000  0.25000  0.50000  0.75000  0.06250  0.31250  0.56250  0.81250  0.12500  0.37500
Base 5:
  0.00000  0.20000  0.40000  0.60000  0.80000  0.04000  0.24000  0.44000  0.64000  0.84000

```



## bc

This solution hardcodes the literal <tt>10</tt> because [[Literals/Integer#bc|numeric literals in bc]] can use any base from 2 to 16. This solution only works with integer bases from 2 to 16.


```bc
/*
 * Return the _n_th term of the van der Corput sequence.
 * Uses the current _ibase_.
 */
define v(n) {
	auto c, r, s

	s = scale
	scale = 0  /* to use integer division */

	/*
	 * c = count digits of n
	 * r = reverse the digits of n
	 */
	for (0; n != 0; n /= 10) {
		c += 1
		r = (10 * r) + (n % 10)
	}

	/* move radix point to left of digits */
	scale = length(r) + 6
	r /= 10 ^ c

	scale = s
	return r
}

t = 10
for (b = 2; b <= 4; b++) {
	"base "; b
	obase = b
	for (i = 0; i < 10; i++) {
		ibase = b
		"  "; v(i)
		ibase = t
	}
	obase = t
}
quit
```


Some of the calculations are not exact, because bc performs calculations using base 10. So the program prints a result like <tt>.202222221</tt> (base 3) when the exact result would be <tt>.21</tt> (base 3).

{{out}}

```txt
base 2
  0.00000000000000
  .10000000000000
  .01000000000000
  .11000000000000
  .00100000000000
  .10100000000000
  .01100000000000
  .11100000000000
  .00010000000000
  .10010000000000
base 3
  0.000000000
  .022222222
  .122222221
  .002222222
  .102222222
  .202222221
  .012222222
  .112222221
  .212222221
  .000222222
base 4
  0.0000000
  .1000000
  .2000000
  .3000000
  .0100000
  .1100000
  .2100000
  .310000000
  .0200000
  .1200000
```



## C


```c
#include <stdio.h>

void vc(int n, int base, int *num, int *denom)
{
        int p = 0, q = 1;

        while (n) {
                p = p * base + (n % base);
                q *= base;
                n /= base;
        }

        *num = p;
        *denom = q;

        while (p) { n = p; p = q % p; q = n; }
        *num /= q;
        *denom /= q;
}

int main()
{
        int d, n, i, b;
        for (b = 2; b < 6; b++) {
                printf("base %d:", b);
                for (i = 0; i < 10; i++) {
                        vc(i, b, &n, &d);
                        if (n) printf("  %d/%d", n, d);
                        else   printf("  0");
                }
                printf("\n");
        }

        return 0;
}
```

{{out}}

```txt
base 2:  0  1/2  1/4  3/4  1/8  5/8  3/8  7/8  1/16  9/16
base 3:  0  1/3  2/3  1/9  4/9  7/9  2/9  5/9  8/9  1/27
base 4:  0  1/4  1/2  3/4  1/16  5/16  9/16  13/16  1/8  3/8
base 5:  0  1/5  2/5  3/5  4/5  1/25  6/25  11/25  16/25  21/25
```


## C++

{{trans|Perl 6}}

```cpp
#include <cmath>
#include <iostream>

double vdc(int n, double base = 2)
{
    double vdc = 0, denom = 1;
    while (n)
    {
        vdc += fmod(n, base) / (denom *= base);
        n /= base; // note: conversion from 'double' to 'int'
    }
    return vdc;
}

int main()
{
    for (double base = 2; base < 6; ++base)
    {
        std::cout << "Base " << base << "\n";
        for (int n = 0; n < 10; ++n)
        {
            std::cout << vdc(n, base) << " ";
        }
        std::cout << "\n\n";
    }
}
```

{{out}}

```txt

Base 2
0 0.5 0.25 0.75 0.125 0.625 0.375 0.875 0.0625 0.5625

Base 3
0 0.333333 0.666667 0.111111 0.444444 0.777778 0.222222 0.555556 0.888889 0.037037

Base 4
0 0.25 0.5 0.75 0.0625 0.3125 0.5625 0.8125 0.125 0.375

Base 5
0 0.2 0.4 0.6 0.8 0.04 0.24 0.44 0.64 0.84

```


## C#

This is based on the C version.<br/>
It uses LINQ and enumeration over a collection
to package the sequence and make it easy to use.
Note that the iterator returns a generic Tuple
whose items are the numerator and denominator for the item.


```c#

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace VanDerCorput
{
    /// <summary>
    /// Computes the Van der Corput sequence for any number base.
    /// The numbers in the sequence vary from zero to one, including zero but excluding one.
    /// The sequence possesses low discrepancy.
    /// Here are the first ten terms for bases 2 to 5:
    ///
    /// base 2:  0  1/2  1/4  3/4  1/8  5/8  3/8  7/8  1/16  9/16
    /// base 3:  0  1/3  2/3  1/9  4/9  7/9  2/9  5/9  8/9  1/27
    /// base 4:  0  1/4  1/2  3/4  1/16  5/16  9/16  13/16  1/8  3/8
    /// base 5:  0  1/5  2/5  3/5  4/5  1/25  6/25  11/25  16/25  21/25
    /// </summary>
    /// <see cref="http://rosettacode.org/wiki/Van_der_Corput_sequence"/>
    public class VanDerCorputSequence: IEnumerable<Tuple<long,long>>
    {
        /// <summary>
        /// Number base for the sequence, which must bwe two or more.
        /// </summary>
        public int Base { get; private set; }

        /// <summary>
        /// Maximum number of terms to be returned by iterator.
        /// </summary>
        public long Count { get; private set; }

        /// <summary>
        /// Construct a sequence for the given base.
        /// </summary>
        /// <param name="iBase">Number base for the sequence.</param>
        /// <param name="count">Maximum number of items to be returned by the iterator.</param>
        public VanDerCorputSequence(int iBase, long count = long.MaxValue) {
            if (iBase < 2)
                throw new ArgumentOutOfRangeException("iBase", "must be two or greater, not the given value of " + iBase);
            Base = iBase;
            Count = count;
        }

        /// <summary>
        /// Compute nth term in the Van der Corput sequence for the base specified in the constructor.
        /// </summary>
        /// <param name="n">The position in the sequence, which may be zero or any positive number.</param>
        /// This number is always an integral power of the base.</param>
        /// <returns>The Van der Corput sequence value expressed as a Tuple containing a numerator and a denominator.</returns>
        public Tuple<long,long> Compute(long n)
        {
            long p = 0, q = 1;
            long numerator, denominator;
            while (n != 0)
            {
                p = p * Base + (n % Base);
                q *= Base;
                n /= Base;
            }
            numerator = p;
            denominator = q;
            while (p != 0)
            {
                n = p;
                p = q % p;
                q = n;
            }
            numerator /= q;
            denominator /= q;
            return new Tuple<long,long>(numerator, denominator);
        }

        /// <summary>
        /// Compute nth term in the Van der Corput sequence for the given base.
        /// </summary>
        /// <param name="iBase">Base to use for the sequence.</param>
        /// <param name="n">The position in the sequence, which may be zero or any positive number.</param>
        /// <returns>The Van der Corput sequence value expressed as a Tuple containing a numerator and a denominator.</returns>
        public static Tuple<long, long> Compute(int iBase, long n)
        {
            var seq = new VanDerCorputSequence(iBase);
            return seq.Compute(n);
        }

        /// <summary>
        /// Iterate over the Van Der Corput sequence.
        /// The first value in the sequence is always zero, regardless of the base.
        /// </summary>
        /// <returns>A tuple whose items are the Van der Corput value given as a numerator and denominator.</returns>
        public IEnumerator<Tuple<long, long>> GetEnumerator()
        {
            long iSequenceIndex = 0L;
            while (iSequenceIndex < Count)
            {
                yield return Compute(iSequenceIndex);
                iSequenceIndex++;
            }
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            TestBasesTwoThroughFive();

            Console.WriteLine("Type return to continue...");
            Console.ReadLine();
        }

        static void TestBasesTwoThroughFive()
        {
            foreach (var seq in Enumerable.Range(2, 5).Select(x => new VanDerCorputSequence(x, 10))) // Just the first 10 elements of the each sequence
            {
                Console.Write("base " + seq.Base + ":");
                foreach(var vc in seq)
                    Console.Write(" " + vc.Item1 + "/" + vc.Item2);
                Console.WriteLine();
            }
        }
    }
}
```

{{out}}

```txt
base 2: 0/1 1/2 1/4 3/4 1/8 5/8 3/8 7/8 1/16 9/16
base 3: 0/1 1/3 2/3 1/9 4/9 7/9 2/9 5/9 8/9 1/27
base 4: 0/1 1/4 1/2 3/4 1/16 5/16 9/16 13/16 1/8 3/8
base 5: 0/1 1/5 2/5 3/5 4/5 1/25 6/25 11/25 16/25 21/25
base 6: 0/1 1/6 1/3 1/2 2/3 5/6 1/36 7/36 13/36 19/36
Type return to continue...
```


## Clojure

```clojure
(defn van-der-corput
  "Get the nth element of the van der Corput sequence."
  ([n]
   ;; Default base = 2
   (van-der-corput n 2))
  ([n base]
   (let [s (/ 1 base)]  ;; A multiplicand to shift to the right of the decimal.
     ;; We essentially want to reverse the digits of n and put them after the
     ;; decimal point. So, we repeatedly pull off the lowest digit of n, scale
     ;; it to the right of the decimal point, and accumulate that.
     (loop [sum 0
            n n
            scale s]
       (if (zero? n)
         sum  ;; Base case: no digits left, so we're done.
         (recur (+ sum (* (rem n base) scale))  ;; Accumulate the least digit
                (quot n base)                   ;; Drop a digit of n
                (* scale s)))))))               ;; Move farther past the decimal

(clojure.pprint/print-table
  (cons :base (range 10))  ;; column headings
  (for [base (range 2 6)]  ;; rows
    (into {:base base}
          (for [n (range 10)]  ;; table entries
            [n (van-der-corput n base)]))))
```


{{out}}

```txt
| :base | 0 |   1 |   2 |   3 |    4 |    5 |    6 |     7 |     8 |     9 |
|-------+---+-----+-----+-----+------+------+------+-------+-------+-------|
|     2 | 0 | 1/2 | 1/4 | 3/4 |  1/8 |  5/8 |  3/8 |   7/8 |  1/16 |  9/16 |
|     3 | 0 | 1/3 | 2/3 | 1/9 |  4/9 |  7/9 |  2/9 |   5/9 |   8/9 |  1/27 |
|     4 | 0 | 1/4 | 1/2 | 3/4 | 1/16 | 5/16 | 9/16 | 13/16 |   1/8 |   3/8 |
|     5 | 0 | 1/5 | 2/5 | 3/5 |  4/5 | 1/25 | 6/25 | 11/25 | 16/25 | 21/25 |
```


## Common Lisp

```lisp
(defun van-der-Corput (n base)
  (loop for d = 1 then (* d base) while (<= d n)
	finally
	(return (/ (parse-integer
		     (reverse (write-to-string n :base base))
		     :radix base)
		   d))))

(loop for base from 2 to 5 do
      (format t "Base ~a: ~{~6a~^~}~%" base
	      (loop for i to 10 collect (van-der-Corput i base))))
```

{{out}}

```txt
Base 2: 0     1/2   1/4   3/4   1/8   5/8   3/8   7/8   1/16  9/16  5/16
Base 3: 0     1/3   2/3   1/9   4/9   7/9   2/9   5/9   8/9   1/27  10/27
Base 4: 0     1/4   1/2   3/4   1/16  5/16  9/16  13/16 1/8   3/8   5/8
Base 5: 0     1/5   2/5   3/5   4/5   1/25  6/25  11/25 16/25 21/25 2/25
```


## D

```d
double vdc(int n, in double base=2.0) pure nothrow @safe @nogc {
    double vdc = 0.0, denom = 1.0;
    while (n) {
        denom *= base;
        vdc += (n % base) / denom;
        n /= base;
    }
    return vdc;
}

void main() {
    import std.stdio, std.algorithm, std.range;

    foreach (immutable b; 2 .. 6)
        writeln("\nBase ", b, ": ", 10.iota.map!(n => vdc(n, b)));
}
```

{{out}}

```txt
Base 2: [0, 0.5, 0.25, 0.75, 0.125, 0.625, 0.375, 0.875, 0.0625, 0.5625]

Base 3: [0, 0.333333, 0.666667, 0.111111, 0.444444, 0.777778, 0.222222, 0.555556, 0.888889, 0.037037]

Base 4: [0, 0.25, 0.5, 0.75, 0.0625, 0.3125, 0.5625, 0.8125, 0.125, 0.375]

Base 5: [0, 0.2, 0.4, 0.6, 0.8, 0.04, 0.24, 0.44, 0.64, 0.84]
```


## Ela

```ela
open random number list

vdc bs n = vdc' 0.0 1.0 n
  where vdc' v d n
          | n > 0 = vdc' v' d' n'
          | else  = v
          where
            d' = d * bs
            rem = n % bs
            n' = truncate (n / bs)
            v' = v + rem / d'
```


Test (with base 2.0, using non-strict map function on infinite list):


```ela
take 10 <| map' (vdc 2.0) [1..]
```

{{out}}

```txt
[0.5,0.25,0.75,0.125,0.625,0.375,0.875,0.0625,0.5625,0.3125]
```


## Elixir

{{works with|Elixir|1.1}}

```elixir
defmodule Van_der_corput do
  def sequence( n, base \\ 2 ) do
    "0." <> (Integer.to_string(n, base) |> String.reverse )
  end

  def float( n, base \\ 2 ) do
    Integer.digits(n, base) |> Enum.reduce(0, fn i,acc -> (i + acc) / base end)
  end

  def fraction( n, base \\ 2 ) do
    str = Integer.to_string(n, base) |> String.reverse
    denominator = Enum.reduce(1..String.length(str), 1, fn _,acc -> acc*base end)
    reduction( String.to_integer(str, base), denominator )
  end

  defp reduction( 0, _ ), do: "0"
  defp reduction( numerator, denominator ) do
    gcd = gcd( numerator, denominator )
    "#{ div(numerator, gcd) }/#{ div(denominator, gcd) }"
  end

  defp gcd( a, 0 ), do: a
  defp gcd( a, b ), do: gcd( b, rem(a, b) )
end

funs = [ {"Float(Base):",     &Van_der_corput.sequence/2},
         {"Float(Decimal):",  &Van_der_corput.float/2   },
         {"Fraction:",        &Van_der_corput.fraction/2} ]
Enum.each(funs, fn {title, fun} ->
  IO.puts title
  Enum.each(2..5, fn base ->
    IO.puts "  Base #{ base }: #{ Enum.map_join(0..9, ", ", &fun.(&1, base)) }"
  end)
end)
```


{{out}}

```txt

Float(Base):
  Base 2: 0.0, 0.1, 0.01, 0.11, 0.001, 0.101, 0.011, 0.111, 0.0001, 0.1001
  Base 3: 0.0, 0.1, 0.2, 0.01, 0.11, 0.21, 0.02, 0.12, 0.22, 0.001
  Base 4: 0.0, 0.1, 0.2, 0.3, 0.01, 0.11, 0.21, 0.31, 0.02, 0.12
  Base 5: 0.0, 0.1, 0.2, 0.3, 0.4, 0.01, 0.11, 0.21, 0.31, 0.41
Float(Decimal):
  Base 2: 0.0, 0.5, 0.25, 0.75, 0.125, 0.625, 0.375, 0.875, 0.0625, 0.5625
  Base 3: 0.0, 0.3333333333333333, 0.6666666666666666, 0.1111111111111111, 0.4444444444444444, 0.7777777777777778, 0.2222222222222222, 0.5555555555555555, 0.8888888888888888, 0.037037037037037035
  Base 4: 0.0, 0.25, 0.5, 0.75, 0.0625, 0.3125, 0.5625, 0.8125, 0.125, 0.375
  Base 5: 0.0, 0.2, 0.4, 0.6, 0.8, 0.04, 0.24, 0.44000000000000006, 0.64, 0.8400000000000001
Fraction:
  Base 2: 0, 1/2, 1/4, 3/4, 1/8, 5/8, 3/8, 7/8, 1/16, 9/16
  Base 3: 0, 1/3, 2/3, 1/9, 4/9, 7/9, 2/9, 5/9, 8/9, 1/27
  Base 4: 0, 1/4, 1/2, 3/4, 1/16, 5/16, 9/16, 13/16, 1/8, 3/8
  Base 5: 0, 1/5, 2/5, 3/5, 4/5, 1/25, 6/25, 11/25, 16/25, 21/25

```



## Erlang

I liked the bc output-in-same-base, but think this is the way it should look.

```Erlang

-module( van_der_corput ).

-export( [sequence/1, sequence/2, task/0] ).

sequence( N ) -> sequence( N, 2 ).

sequence( 0, _Base ) -> 0.0;
sequence( N, Base ) -> erlang:list_to_float( "0." ++ lists:flatten([erlang:integer_to_list(X) || X <- sequence_loop(N, Base)]) ).

task() -> [task(X) || X <- lists:seq(2, 5)].



sequence_loop( 0, _Base ) -> [];
sequence_loop( N, Base ) ->
	New_n = N div Base,
	Digit = N rem Base,
	[Digit | sequence_loop( New_n, Base )].

task( Base ) ->
	io:fwrite( "Base ~p:", [Base] ),
	[io:fwrite( " ~p", [sequence(X, Base)] ) || X <- lists:seq(0, 9)],
	io:fwrite( "~n" ).

```

{{out}}

```txt

34> van_der_corput:task().
Base 2: 0.0 0.1 0.01 0.11 0.001 0.101 0.011 0.111 0.0001 0.1001
Base 3: 0.0 0.1 0.2 0.01 0.11 0.21 0.02 0.12 0.22 0.001
Base 4: 0.0 0.1 0.2 0.3 0.01 0.11 0.21 0.31 0.02 0.12
Base 5: 0.0 0.1 0.2 0.3 0.4 0.01 0.11 0.21 0.31 0.41

```



## ERRE


```ERRE
PROGRAM VAN_DER_CORPUT

!
! for rosettacode.org
!

PROCEDURE VDC(N%,B%->RES)
      LOCAL V,S%
      S%=1
      WHILE N%>0 DO
        S%*=B%
        V+=(N% MOD B%)/S%
        N%=N% DIV B%
      END WHILE
      RES=V
END PROCEDURE

BEGIN
      FOR BASE%=2 TO 5 DO
        PRINT("Base";STR$(BASE%);":")
        FOR NUMBER%=0 TO 9 DO
          VDC(NUMBER%,BASE%->RES)
          WRITE("#.##### ";RES;)
        END FOR
        PRINT
      END FOR
END PROGRAM
```

{{out}}

```txt

Base 2:
 0.00000 0.50000 0.25000 0.75000 0.12500 0.62500 0.37500 0.87500 0.06250 0.56250
Base 3:
 0.00000 0.33333 0.66667 0.11111 0.44444 0.77778 0.22222 0.55556 0.88889 0.03704
Base 4:
 0.00000 0.25000 0.50000 0.75000 0.06250 0.31250 0.56250 0.81250 0.12500 0.37500
Base 5:
 0.00000 0.20000 0.40000 0.60000 0.80000 0.04000 0.24000 0.44000 0.64000 0.84000

```



## Euphoria

{{trans|D}}

```euphoria
function vdc(integer n, atom base)
    atom vdc, denom, rem
    vdc = 0
    denom = 1
    while n do
        denom *= base
        rem = remainder(n,base)
        n = floor(n/base)
        vdc += rem / denom
    end while
    return vdc
end function

for i = 2 to 5 do
    printf(1,"Base %d\n",i)
    for j = 0 to 9 do
        printf(1,"%g ",vdc(j,i))
    end for
    puts(1,"\n\n")
end for
```


{{out}}

```txt
Base 2
0 0.5 0.25 0.75 0.125 0.625 0.375 0.875 0.0625 0.5625

Base 3
0 0.333333 0.666667 0.111111 0.444444 0.777778 0.222222 0.555556 0.888889 0.037037

Base 4
0 0.25 0.5 0.75 0.0625 0.3125 0.5625 0.8125 0.125 0.375

Base 5
0 0.2 0.4 0.6 0.8 0.04 0.24 0.44 0.64 0.84


```


=={{header|F_Sharp|F#}}==

```fsharp
open System

let vdc n b =
    let rec loop n denom acc =
        if n > 0l then
            let m, remainder = Math.DivRem(n, b)
            loop m (denom * b) (acc + (float remainder) / (float (denom * b)))
        else acc
    loop n 1 0.0


[<EntryPoint>]
let main argv =
    printfn "%A" [ for n in 0 .. 9 -> (vdc n 2) ]
    printfn "%A" [ for n in 0 .. 9 -> (vdc n 5) ]
    0
```

{{out}}

```txt
[0.0; 0.5; 0.25; 0.75; 0.125; 0.625; 0.375; 0.875; 0.0625; 0.5625]
[0.0; 0.2; 0.4; 0.6; 0.8; 0.04; 0.24; 0.44; 0.64; 0.84]
```



## Factor

{{works with|Factor|0.98}}

```factor
USING: formatting fry io kernel math math.functions math.parser
math.ranges sequences ;
IN: rosetta-code.van-der-corput

: vdc ( n base -- x )
    [ >base string>digits <reversed> ]
    [ nip '[ 1 + neg _ swap ^ * ] ] 2bi map-index sum ;

: vdc-demo ( -- )
    2 5 [a,b] [
        dup "Base %d: " printf 10 <iota>
        [ swap vdc "%-5u " printf ] with each nl
    ] each ;

MAIN: vdc-demo
```

{{out}}

```txt

Base 2: 0     1/2   1/4   3/4   1/8   5/8   3/8   7/8   1/16  9/16
Base 3: 0     1/3   2/3   1/9   4/9   7/9   2/9   5/9   8/9   1/27
Base 4: 0     1/4   1/2   3/4   1/16  5/16  9/16  13/16 1/8   3/8
Base 5: 0     1/5   2/5   3/5   4/5   1/25  6/25  11/25 16/25 21/25

```



## Forth


```forth
: fvdc ( base n -- f )
  0e 1e ( F: vdc denominator )
  begin dup while
    over s>d d>f f*
    over /mod  ( base rem n )
    swap s>d d>f fover f/
    frot f+ fswap
  repeat 2drop fdrop ;

: test  10 0 do 2 i fvdc cr f. loop ;
```


{{out}}

```txt
test
0.
0.5
0.25
0.75
0.125
0.625
0.375
0.875
0.0625
0.5625  ok
```



## Fortran

This is straightforward once one remembers that the obvious scheme for extracting digits from a number produces them from the low-order end to the high-order end. This reversal is normally annoying, but here a "reflection" ''is'' desired. The source is old-style, except for using F90's ability to have a function (or subroutine) name appear on its END statement with this checked by the compiler. Because the MODULE protocol introduced by F90 is not bothered with, the type of the function has to be declared in all routines invoking it if the default type based on the form of the name does not suffice. Single precision suffices, but the F90 compiler moans that the type of the function itself has not been explicitly declared. Ah well.
```Fortran
      FUNCTION VDC(N,BASE)	!Calculates a Van der Corput number...
Converts 1234 in decimal to 4321 in V, and P = 10000.
       INTEGER N	!For this integer,
       INTEGER BASE	!In this base.
       INTEGER I	!A copy of N that can be damaged.
       INTEGER P	!Successive powers of BASE.
       INTEGER V	!Accumulates digits.
        P = 1		! = BASE**0
        V = 0		!Start with no digits, as if N = 0.
        I = N		!Here we go.
        DO WHILE (I .NE. 0)	!While something remains,
          V = V*BASE + MOD(I,BASE)	!Extract its low-order digit.
          I = I/BASE			!Reduce it by a power.
          P = P*BASE			!And track the power.
        END DO			!Thus extract the digits in reverse order: right-to-left.
        VDC = V/FLOAT(P)	!The power is one above the highest digit.
      END FUNCTION VDC	!Numerology is weird.

      PROGRAM POKE
      INTEGER FIRST,LAST	!Might as well document some constants.
      PARAMETER (FIRST = 0,LAST = 9)	!Thus, the first ten values.
      INTEGER I,BASE		!Steppers.
      REAL VDC			!Stop the compiler moaning about undeclared items.

      WRITE (6,1) FIRST,LAST,(I, I = FIRST,LAST)	!Announce.
    1 FORMAT ("Calculates values ",I0," to ",I0," of the ",
     1 "Van der Corput sequence, in various bases."/
     2 "Base",666I9)

      DO BASE = 2,13	!A selection of bases.
        WRITE (6,2) BASE,(VDC(I,BASE), I = FIRST,LAST)	!Show the specified span.
    2   FORMAT (I4,666F9.6)	!Aligns with FORMAT 1.
      END DO		!On to the next base.

      END
```

Output: six-digit precision is about the most that single precision offers.

```txt

Calculates values 0 to 9 of the Van der Corput sequence, in various bases.
Base        0        1        2        3        4        5        6        7        8        9
   2 0.000000 0.500000 0.250000 0.750000 0.125000 0.625000 0.375000 0.875000 0.062500 0.562500
   3 0.000000 0.333333 0.666667 0.111111 0.444444 0.777778 0.222222 0.555556 0.888889 0.037037
   4 0.000000 0.250000 0.500000 0.750000 0.062500 0.312500 0.562500 0.812500 0.125000 0.375000
   5 0.000000 0.200000 0.400000 0.600000 0.800000 0.040000 0.240000 0.440000 0.640000 0.840000
   6 0.000000 0.166667 0.333333 0.500000 0.666667 0.833333 0.027778 0.194444 0.361111 0.527778
   7 0.000000 0.142857 0.285714 0.428571 0.571429 0.714286 0.857143 0.020408 0.163265 0.306122
   8 0.000000 0.125000 0.250000 0.375000 0.500000 0.625000 0.750000 0.875000 0.015625 0.140625
   9 0.000000 0.111111 0.222222 0.333333 0.444444 0.555556 0.666667 0.777778 0.888889 0.012346
  10 0.000000 0.100000 0.200000 0.300000 0.400000 0.500000 0.600000 0.700000 0.800000 0.900000
  11 0.000000 0.090909 0.181818 0.272727 0.363636 0.454545 0.545455 0.636364 0.727273 0.818182
  12 0.000000 0.083333 0.166667 0.250000 0.333333 0.416667 0.500000 0.583333 0.666667 0.750000
  13 0.000000 0.076923 0.153846 0.230769 0.307692 0.384615 0.461538 0.538462 0.615385 0.692308

```



## FreeBASIC


```freebasic
' version 03-12-2016
' compile with: fbc -s console

Function num_base(number As ULongInt, _base_ As UInteger) As String

    If _base_ > 9 Then
        Print "base not handled by function"
        Sleep 5000
        Return ""
    End If

    Dim As ULongInt n
    Dim As String ans

    While number <> 0
        n = number Mod _base_
        ans = Str(n) + ans
        number = number \ _base_
    Wend

    If ans = "" Then ans = "0"

    Return "." + ans

End Function

' ------=< MAIN >=------

Dim As ULong k, l
For k = 2 To 5
    Print "Base = "; k
    For l = 0 To 12
        Print left(num_base(l, k) + "      ",6);
    Next
    Print : print
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Base = 2
.0    .1    .10   .11   .100  .101  .110  .111  .1000 .1001 .1010 .1011 .1100

Base = 3
.0    .1    .2    .10   .11   .12   .20   .21   .22   .100  .101  .102  .110

Base = 4
.0    .1    .2    .3    .10   .11   .12   .13   .20   .21   .22   .23   .30

Base = 5
.0    .1    .2    .3    .4    .10   .11   .12   .13   .14   .20   .21   .22
```



## Go


```go
package main

import "fmt"

func v2(n uint) (r float64) {
    p := .5
    for n > 0 {
        if n&1 == 1 {
            r += p
        }
        p *= .5
        n >>= 1
    }
    return
}

func newV(base uint) func(uint) float64 {
    invb := 1 / float64(base)
    return func(n uint) (r float64) {
        p := invb
        for n > 0 {
            r += p * float64(n%base)
            p *= invb
            n /= base
        }
        return
    }
}

func main() {
    fmt.Println("Base 2:")
    for i := uint(0); i < 10; i++ {
        fmt.Println(i, v2(i))
    }
    fmt.Println("Base 3:")
    v3 := newV(3)
    for i := uint(0); i < 10; i++ {
        fmt.Println(i, v3(i))
    }
}
```

{{out}}

```txt

Base 2:
0 0
1 0.5
2 0.25
3 0.75
4 0.125
5 0.625
6 0.375
7 0.875
8 0.0625
9 0.5625
Base 3:
0 0
1 0.3333333333333333
2 0.6666666666666666
3 0.1111111111111111
4 0.4444444444444444
5 0.7777777777777777
6 0.2222222222222222
7 0.5555555555555556
8 0.8888888888888888
9 0.037037037037037035

```



## Haskell

The function <tt>vdc</tt> returns the n<sup>th</sup> exact, arbitrary precision van der Corput number for any base &ge; 2 and any n.  (A reasonable value is returned for negative values of n.)

```haskell
import Data.List
import Data.Ratio
import System.Environment
import Text.Printf

-- A wrapper type for Rationals to make them look nicer when we print them.
newtype Rat = Rat Rational
instance Show Rat where
  show (Rat n) = show (numerator n) ++ "/" ++ show (denominator n)

-- Convert a list of base b digits to its corresponding number.  We assume the
-- digits are valid base b numbers and that their order is from least to most
-- significant.
digitsToNum :: Integer -> [Integer] -> Integer
digitsToNum b = foldr1 (\d acc -> b * acc + d)

-- Convert a number to the list of its base b digits.  The order will be from
-- least to most significant.
numToDigits :: Integer -> Integer -> [Integer]
numToDigits _ 0 = [0]
numToDigits b n = unfoldr step n
  where step 0 = Nothing
        step m = let (q,r) = m `quotRem` b in Just (r,q)

-- Return the n'th element in the base b van der Corput sequence.  The base
-- must be ≥ 2.
vdc :: Integer -> Integer -> Rat
vdc b n | b < 2 = error "vdc: base must be ≥ 2"
        | otherwise = let ds = reverse $ numToDigits b n
                      in Rat (digitsToNum b ds % b ^ length ds)

-- Print the base followed by a sequence of van der Corput numbers.
printVdc :: (Integer,[Rat]) -> IO ()
printVdc (b,ns) = putStrLn $ printf "Base %d:" b
                  ++ concatMap (printf " %5s" . show) ns

-- To print the n'th van der Corput numbers for n in [2,3,4,5] call the program
-- with no arguments.  Otherwise, passing the base b, first n, next n and
-- maximum n will print the base b numbers for n in [firstN, nextN, ..., maxN].
main :: IO ()
main = do
  args <- getArgs
  let (bases, nums) = case args of
        [b, f, s, m] -> ([read b], [read f, read s..read m])
        _ -> ([2,3,4,5], [0..9])
  mapM_ printVdc [(b,rs) | b <- bases, let rs = map (vdc b) nums]
```

{{out}} for small bases:

```txt

$ ./vandercorput
Base 2:   0/1   1/2   1/4   3/4   1/8   5/8   3/8   7/8  1/16  9/16
Base 3:   0/1   1/3   2/3   1/9   4/9   7/9   2/9   5/9   8/9  1/27
Base 4:   0/1   1/4   1/2   3/4  1/16  5/16  9/16 13/16   1/8   3/8
Base 5:   0/1   1/5   2/5   3/5   4/5  1/25  6/25 11/25 16/25 21/25

```

{{out}} for a larger base.  (Base 123 for n &isin; [50, 100, &hellip;, 300].)

```txt

$ ./vandercorput 123 50 100 300
Base 123: 50/123 100/123 3322/15129 9472/15129 494/15129 6644/15129

```


=={{header|Icon}} and {{header|Unicon}}==
The following solution works in both Icon and Unicon:

```Unicon
procedure main(A)
    base := integer(get(A)) | 2
    every writes(round(vdc(0 to 9,base),10)," ")
    write()
end

procedure vdc(n, base)
    e := 1.0
    x := 0.0
    while x +:= 1(((0 < n) % base) / (e *:= base), n /:= base)
    return x
end

procedure round(n,d)
    places := 10 ^ d
    return real(integer(n*places + 0.5)) / places
end
```


and a sample run is:


```txt
->vdc
0.0 0.5 0.25 0.75 0.125 0.625 0.375 0.875 0.0625 0.5625
->vdc 3
0.0 0.3333333333 0.6666666667 0.1111111111 0.4444444444 0.7777777778 0.2222222222 0.5555555556 0.8888888889 0.037037037
->vdc 5
0.0 0.2 0.4 0.6 0.8 0.04 0.24 0.44 0.64 0.84
->vdc 123
0.0 0.0081300813 0.0162601626 0.0243902439 0.0325203252 0.0406504065 0.0487804878 0.0569105691 0.0650406504 0.07317073170000001
->
```


An alternate, Unicon-specific implementation of <tt>vdc</tt> patterned after the functional Perl 6
solution is:

```Unicon
procedure vdc(n, base)
    s1 := create |((0 < 1(.n, n /:= base)) % base)
    s2 := create 2(e := 1.0, |(e *:= base))
    every (result := 0) +:= |s1() / s2()
    return result
end
```

It produces the same output as shown above.


## J

'''Solution:'''

```j
vdc=: ([ %~ %@[ #. #.inv)"0 _
```

'''Examples:'''

```j
   2 vdc i.10                NB. 1st 10 nums of Van der Corput sequence in base 2
0 0.5 0.25 0.75 0.125 0.625 0.375 0.875 0.0625 0.5625
   2x vdc i.10               NB. as above but using rational nums
0 1r2 1r4 3r4 1r8 5r8 3r8 7r8 1r16 9r16
   2 3 4 5x vdc i.10         NB. 1st 10 nums of Van der Corput sequence in bases 2 3 4 5
0 1r2 1r4 3r4  1r8  5r8  3r8   7r8  1r16  9r16
0 1r3 2r3 1r9  4r9  7r9  2r9   5r9   8r9  1r27
0 1r4 1r2 3r4 1r16 5r16 9r16 13r16   1r8   3r8
0 1r5 2r5 3r5  4r5 1r25 6r25 11r25 16r25 21r25
```


In other words: use the left argument as the "base" to structure the sequence numbers into digits ("base 2", etc.).  Then use the reciprocal of the left argument as the "base" to re-represent this sequence and divide that result by the left argument to get the Van der Corput sequence number.


## Java

{{trans|Perl 6}}
Using <code>(denom *= 2)</code> as the denominator is not a recommended way of doing things since it is not clear when the multiplication and assignment happen.
Comparing this to the "++" operator, it looks like it should do the doubling and assignment second. Comparing it to the "++" operator used as a preincrement operator, it looks like it should do the doubling and assignment first.
Comparing it to the behavior of parentheses, it looks like it should do the doubling and assignment first. Luckily for us, it works the same in Java as in Perl 6 (doubling and assignment first). It was kept the Perl 6 way to help with the comparison.
Normally, we would initialize denom to 2 (since that is the denominator of the leftmost digit), use it alone in the vdc sum, and then double it after.

```java
public class VanDerCorput{
	public static double vdc(int n){
		double vdc = 0;
		int denom = 1;
		while(n != 0){
			vdc += n % 2.0 / (denom *= 2);
			n /= 2;
		}
		return vdc;
	}

	public static void main(String[] args){
		for(int i = 0; i <= 10; i++){
			System.out.println(vdc(i));
		}
	}
}
```

{{out}}

```txt
0.0
0.5
0.25
0.75
0.125
0.625
0.375
0.875
0.0625
0.5625
0.3125
```



## jq

{{ works with|jq|1.4}}

The neat thing about the following implementation of vdc(base) is that it shows how the task can be accomplished in two separate steps without the need to construct an intermediate array.

```jq
# vdc(base) converts an input decimal integer to a decimal number based on the van der
# Corput sequence using base 'base', e.g. (4 | vdc(2)) is 0.125.
#
def vdc(base):

  # The helper function converts a stream of residuals to a decimal,
  # e.g. if base is 2, then decimalize( (0,0,1) ) yields 0.125
  def decimalize(stream):
    reduce stream as $d   # state: [accumulator, power]
      ( [0, 1/base];
       .[1] as $power | [ .[0] + ($d * $power), $power / base] )
    | .[0];

  if . == 0 then 0
  else decimalize(recurse( if . == 0 then empty else ./base | floor end ) % base)
  end ;
```

'''Example:'''

```jq
def round(n):
  (if . < 0 then -1 else 1 end) as $s
  | $s*10*.*n | if (floor%10)>4 then (.+5) else . end | ./10 | floor/n | .*$s;

range(2;6) | . as $base | "Base \(.): \( [ range(0;11) | vdc($base)|round(1000) ] )"
```

{{out}}

```sh

$ jq -n -f -c -r van_der_corput_sequence.jq
Base 2: [0,0.5,0.25,0.75,0.125,0.625,0.375,0.875,0.063,0.563,0.313]
Base 3: [0,0.333,0.667,0.111,0.444,0.778,0.222,0.556,0.889,0.037,0.37]
Base 4: [0,0.25,0.5,0.75,0.063,0.313,0.563,0.813,0.125,0.375,0.625]
Base 5: [0,0.2,0.4,0.6,0.8,0.04,0.24,0.44,0.64,0.84,0.08]
```



## Julia


```julia
vandercorput(num::Integer, base::Integer) = sum(d * Float64(base) ^ -ex for (ex, d) in enumerate(digits(num, base)))

for base in 2:9
    @printf("%10s %i:", "Base", base)
    for num in 0:9 @printf("%7.3f", vandercorput(num, base)) end
    println(" [...]")
end
```


{{out}}

```txt
      Base 2:  0.000  0.500  0.250  0.750  0.125  0.625  0.375  0.875  0.063  0.563...
      Base 3:  0.000  0.333  0.667  0.111  0.444  0.778  0.222  0.556  0.889  0.037...
      Base 4:  0.000  0.250  0.500  0.750  0.063  0.313  0.563  0.813  0.125  0.375...
      Base 5:  0.000  0.200  0.400  0.600  0.800  0.040  0.240  0.440  0.640  0.840...
      Base 6:  0.000  0.167  0.333  0.500  0.667  0.833  0.028  0.194  0.361  0.528...
      Base 7:  0.000  0.143  0.286  0.429  0.571  0.714  0.857  0.020  0.163  0.306...
      Base 8:  0.000  0.125  0.250  0.375  0.500  0.625  0.750  0.875  0.016  0.141...
      Base 9:  0.000  0.111  0.222  0.333  0.444  0.556  0.667  0.778  0.889  0.012...

```



## Kotlin

{{trans|C}}

```scala
// version 1.1.2

data class Rational(val num: Int, val denom: Int)

fun vdc(n: Int, base: Int): Rational {
    var p = 0
    var q = 1
    var nn = n
    while (nn != 0) {
        p = p * base + nn % base
        q *= base
        nn /= base
    }
    val num = p
    val denom = q
    while (p != 0) {
        nn = p
        p = q % p
        q = nn
    }
    return Rational(num / q, denom / q)
}

fun main(args: Array<String>) {
    for (b in 2..5) {
        print("base $b:")
        for (i in 0..9) {
            val(num, denom) = vdc(i, b)
            if (num != 0) print("  $num/$denom")
            else print("  0")
        }
        println()
    }
}
```


{{out}}

```txt

base 2:  0  1/2  1/4  3/4  1/8  5/8  3/8  7/8  1/16  9/16
base 3:  0  1/3  2/3  1/9  4/9  7/9  2/9  5/9  8/9  1/27
base 4:  0  1/4  1/2  3/4  1/16  5/16  9/16  13/16  1/8  3/8
base 5:  0  1/5  2/5  3/5  4/5  1/25  6/25  11/25  16/25  21/25

```



## Lua


```lua
function vdc(n, base)
    local digits = {}
    while n ~= 0 do
        local m = math.floor(n / base)
        table.insert(digits, n - m * base)
        n = m
    end
    m = 0
    for p, d in pairs(digits) do
        m = m + math.pow(base, -p) * d
    end
    return m
end
```



## Mathematica


```Mathematica
VanDerCorput[n_,base_:2]:=Table[
  FromDigits[{Reverse[IntegerDigits[k,base]],0},base],
{k,n}]
```




```txt
VanDerCorput[10,2]
->{1/2,1/4,3/4,1/8,5/8,3/8,7/8,1/16,9/16,5/16}

VanDerCorput[10,3]
->{1/3, 2/3, 1/9, 4/9, 7/9, 2/9, 5/9, 8/9, 1/27, 10/27}

VanDerCorput[10,4]
->{1/4, 1/2, 3/4, 1/16, 5/16, 9/16, 13/16, 1/8, 3/8, 5/8}

VanDerCorput[10,5]
->{1/5, 2/5, 3/5, 4/5, 1/25, 6/25, 11/25, 16/25, 21/25, 2/25}
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
    function x = corput (n)
    b = dec2bin(1:n)-'0';   % generate sequence of binary numbers from 1 to n
    l = size(b,2);          % get number of binary digits
    w = (1:l)-l-1;          % 2.^w are the weights
    x = b * ( 2.^w');       % matrix times vector multiplication for
    end;
```


{{out}}

```txt
 corput(10)
 ans =

   0.500000
   0.250000
   0.750000
   0.125000
   0.625000
   0.375000
   0.875000
   0.062500
   0.562500
   0.312500
```



## Maxima


Define two helper functions

```Maxima
/* convert a decimal integer to a list of digits in base `base' */
dec2digits(d, base):= block([digits: []],
  while (d>0) do block([newdi: mod(d, base)],
    digits: cons(newdi, digits),
    d: round( (d - newdi) / base)),
  digits)$

dec2digits(123, 10);
/* [1, 2, 3] */
dec2digits(  8,  2);
/* [1, 0, 0, 0] */
```



```Maxima
/* convert a list of digits in base `base' to a decimal integer */
digits2dec(l, base):= block([s: 0, po: 1],
  for di in reverse(l) do (s: di*po + s, po: po*base),
  s)$

digits2dec([1, 2, 3], 10);
/* 123 */
digits2dec([1, 0, 0, 0], 2);
/* 8 */
```


The main function

```Maxima
vdc(n, base):= makelist(
  digits2dec(
    dec2digits(k, base),
    1/base) / base,
  k, n);

vdc(10, 2);
/*
                        1  1  3  1  5  3  7  1   9   5
(%o123)                [-, -, -, -, -, -, -, --, --, --]
                        2  4  4  8  8  8  8  16  16  16
*/

vdc(10, 5);
/*
                      1  2  3  4  1   6   11  16  21  2
(%o124)              [-, -, -, -, --, --, --, --, --, --]
                      5  5  5  5  25  25  25  25  25  25
*/
```


<tt>digits2dec</tt> can by used with symbols to produce the same example as in
the task description

```Maxima

/* 11 in decimal is */
digits: digits2dec([box(1), box(0), box(1), box(1)], box(2));
aux: expand(digits2dec(digits, 1/base) / base)$
simp: false$
/* reflected this would become ... */
subst(box(2), base, aux);
simp: true$

/*

                         3          2
                      """  """   """  """   """ """   """
(%o126)               "2"  "1" + "2"  "0" + "2" "1" + "1"
                      """  """   """  """   """ """   """

                      - 4          - 3          - 2          - 1
               """ """      """ """      """ """      """ """
(%o129)        "1" "2"    + "0" "2"    + "1" "2"    + "1" "2"
               """ """      """ """      """ """      """ """

*/
```


=={{header|Modula-2}}==

```modula2
MODULE Sequence;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE vc(n,base : INTEGER; VAR num,denom : INTEGER);
VAR p,q : INTEGER;
BEGIN
    p := 0;
    q := 1;

    WHILE n#0 DO
        p := p * base + (n MOD base);
        q := q * base;
        n := n DIV base
    END;

    num := p;
    denom := q;

    WHILE p#0 DO
        n := p;
        p := q MOD p;
        q := n
    END;

    num := num DIV q;
    denom := denom DIV q
END vc;

VAR
    buf : ARRAY[0..31] OF CHAR;
    d,n,i,b : INTEGER;
BEGIN
    FOR b:=2 TO 5 DO
        FormatString("base %i:", buf, b);
        WriteString(buf);
        FOR i:=0 TO 9 DO
            vc(i,b,n,d);
            IF n#0 THEN
                FormatString("  %i/%i", buf, n, d);
                WriteString(buf)
            ELSE
                WriteString("  0")
            END
        END;
        WriteLn
    END;

    ReadChar
END Sequence.
```



## PARI/GP


```parigp
VdC(n)=n=binary(n);sum(i=1,#n,if(n[i],1.>>(#n+1-i)));
VdC(n)=sum(i=1,#binary(n),if(bittest(n,i-1),1.>>i)); \\ Alternate approach
vector(10,n,VdC(n))
```

{{out}}

```txt
[0.500000000, 0.250000000, 0.750000000, 0.125000000, 0.625000000, 0.375000000, 0.875000000, 0.0625000000, 0.562500000, 0.312500000]
```



## Pascal

Tested with Free Pascal

```pascal
Program VanDerCorput;
{$IFDEF FPC}
  {$MODE DELPHI}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

type
  tvdrCallback = procedure (nom,denom: NativeInt);

{ Base=2
function rev2(n,Pot:NativeUint):NativeUint;
var
  r : Nativeint;
begin
  r := 0;
  while Pot > 0 do
  Begin
    r := r shl 1 OR (n AND 1);
    n := n shr 1;
    dec(Pot);
  end;
  rev2 := r;
end;
}

function reverse(n,base,Pot:NativeUint):NativeUint;
var
  r,c : Nativeint;
begin
  r := 0;
//No need to test n> 0 in this special case, n starting in upper half
  while Pot > 0 do
  Begin
    c := n div base;
    r := n+(r-c)*base;
    n := c;
    dec(Pot);
  end;
  reverse := r;
end;

procedure VanDerCorput(base,count:NativeUint;f:tvdrCallback);
//calculates count nominater and denominater of Van der Corput sequence
// to base
var
 Pot,
 denom,nom,
 i : NativeUint;
Begin
  denom := 1;
  Pot := 0;
  while count > 0 do
  Begin
    IF Pot = 0 then
      f(0,1);
    //start in upper half
    i := denom;
    inc(Pot);
    denom := denom *base;

    repeat
      nom := reverse(i,base,Pot);
      IF count > 0 then
        f(nom,denom)
      else
        break;
      inc(i);
      dec(count);
    until i >= denom;
  end;
end;

procedure vdrOutPut(nom,denom: NativeInt);
Begin
  write(nom,'/',denom,'  ');
end;

var
 i : NativeUint;
Begin
  For i := 2 to 5 do
  Begin
    write(' Base ',i:2,' :');
    VanDerCorput(i,9,@vdrOutPut);
    writeln;
  end;
end.

```

;output:

```txt
 Base  2 :0/1  1/2  1/4  3/4  1/8  5/8  3/8  7/8  1/16  9/16
 Base  3 :0/1  1/3  2/3  1/9  4/9  7/9  2/9  5/9  8/9  1/27
 Base  4 :0/1  1/4  2/4  3/4  1/16  5/16  9/16  13/16  2/16  6/16
 Base  5 :0/1  1/5  2/5  3/5  4/5  1/25  6/25  11/25  16/25  21/25
```



## Perl

{{trans|Perl6}}

```perl
sub vdc {
    my @value = shift;
    my $base = shift // 2;
    use integer;
    push @value, $value[-1] / $base while $value[-1] > 0;
    my ($x, $sum) = (1, 0);
    no integer;
    $sum += ($_ % $base) / ($x *= $base) for @value;
    return $sum;
}

for my $base ( 2 .. 5 ) {
    print "base $base: ", join ' ', map { vdc($_, $base) } 0 .. 10;
    print "\n";
}
```



## Perl 6

{{Works with|rakudo|2016.08}}
First a cheap implementation in base 2, using string operations.


```perl6
constant VdC = map { :2("0." ~ .base(2).flip) }, ^Inf;
.say for VdC[^16];
```


Here is a more elaborate version using the polymod built-in integer method:

```perl6
sub VdC($base = 2) {
    map {
        [+] $_ && .polymod($base xx *) Z/ [\*] $base xx *
    }, ^Inf
}

.say for VdC[^10];
```

{{out}}

```txt
0
0.5
0.25
0.75
0.125
0.625
0.375
0.875
0.0625
0.5625
```


Here is a fairly standard imperative version in which we mutate three variables in parallel:

```perl6
sub vdc($num, $base = 2) {
    my $n = $num;
    my $vdc = 0;
    my $denom = 1;
    while $n {
        $vdc += $n mod $base / ($denom *= $base);
        $n div= $base;
    }
    $vdc;
}

for 2..5 -> $b {
    say "Base $b";
    say (vdc($_,$b) for ^10).perl;
    say '';
}
```

{{out}}

```txt
Base 2
(0, 1/2, 1/4, 3/4, 1/8, 5/8, 3/8, 7/8, 1/16, 9/16)

Base 3
(0, 1/3, 2/3, 1/9, 4/9, 7/9, 2/9, 5/9, 8/9, 1/27)

Base 4
(0, 1/4, 1/2, 3/4, 1/16, 5/16, 9/16, 13/16, 1/8, 3/8)

Base 5
(0, 1/5, 2/5, 3/5, 4/5, 1/25, 6/25, 11/25, 16/25, 21/25)

```

Here is a functional version that produces the same output:

```perl6
sub vdc($value, $base = 2) {
    my @values = $value, { $_ div $base } ... 0;
    my @denoms = $base,  { $_  *  $base } ... *;
    [+] do for (flat @values Z @denoms) -> $v, $d {
        $v mod $base / $d;
    }
}
```

We first define two sequences, one finite, one infinite.
When we zip those sequences together, the finite sequence terminates the loop (which, since a Perl 6 loop returns all its values, is merely another way of writing a <tt>map</tt>).
We then sum with <tt>[+]</tt>, a reduction of the <tt>+</tt> operator.
(We could have in-lined the sequences or used a traditional <tt>map</tt> operator, but this way seems more readable than the typical FP solution.)
The <tt>do</tt> is necessary to introduce a statement where a term is expected, since Perl 6 distinguishes "sentences" from "noun phrases" as a natural language might.


## Phix

Not entirely sure what to print, so decided to print in three different ways.

It struck me straightaway that the VdC of say 123 is 321/1000, which seems trivial in any base or desired format.

```Phix
enum BASE, FRAC, DECIMAL
constant DESC = {"Base","Fraction","Decimal"}

function vdc(integer n, atom base, integer flag)
object res = ""
atom num = 0, denom = 1, digit, g
    while n do
        denom *= base
        digit = remainder(n,base)
        n = floor(n/base)
        if flag=BASE then
            res &= digit+'0'
        else
            num = num*base+digit
        end if
    end while
    if flag=FRAC then
        g = gcd(num,denom)
        return {num/g,denom/g}
    elsif flag=DECIMAL then
        return num/denom
    end if
    return {iff(length(res)=0?"0":"0."&res)}
end function

procedure show_vdc(integer flag, string fmt)
object v
    for i=2 to 5 do
        printf(1,"%s %d: ",{DESC[flag],i})
        for j=0 to 9 do
            v = vdc(j,i,flag)
            if flag=FRAC and v[1]=0 then
                printf(1,"0 ")
            else
                printf(1,fmt,v)
            end if
        end for
        puts(1,"\n")
    end for
end procedure

show_vdc(BASE,"%s ")
show_vdc(FRAC,"%d/%d ")
show_vdc(DECIMAL,"%g ")
```

{{out}}

```txt

Base 2: 0 0.1 0.01 0.11 0.001 0.101 0.011 0.111 0.0001 0.1001
Base 3: 0 0.1 0.2 0.01 0.11 0.21 0.02 0.12 0.22 0.001
Base 4: 0 0.1 0.2 0.3 0.01 0.11 0.21 0.31 0.02 0.12
Base 5: 0 0.1 0.2 0.3 0.4 0.01 0.11 0.21 0.31 0.41
Fraction 2: 0 1/2 1/4 3/4 1/8 5/8 3/8 7/8 1/16 9/16
Fraction 3: 0 1/3 2/3 1/9 4/9 7/9 2/9 5/9 8/9 1/27
Fraction 4: 0 1/4 1/2 3/4 1/16 5/16 9/16 13/16 1/8 3/8
Fraction 5: 0 1/5 2/5 3/5 4/5 1/25 6/25 11/25 16/25 21/25
Decimal 2: 0 0.5 0.25 0.75 0.125 0.625 0.375 0.875 0.0625 0.5625
Decimal 3: 0 0.333333 0.666667 0.111111 0.444444 0.777778 0.222222 0.555556 0.888889 0.037037
Decimal 4: 0 0.25 0.5 0.75 0.0625 0.3125 0.5625 0.8125 0.125 0.375
Decimal 5: 0 0.2 0.4 0.6 0.8 0.04 0.24 0.44 0.64 0.84

```



## PicoLisp


```PicoLisp
(scl 6)

(de vdc (N B)
   (default B 2)
   (let (R 0  A 1.0)
      (until (=0 N)
         (inc 'R (* (setq A (/ A B)) (% N B)))
         (setq N (/ N B)) )
      R ) )

(for B (2 3 4)
   (prinl "Base: " B)
   (for N (range 0 9)
      (prinl N ": " (round (vdc N B) 4)) ) )
```

{{out}}

```txt
Base: 2
0: 0.0000
1: 0.5000
2: 0.2500
3: 0.7500
4: 0.1250
5: 0.6250
6: 0.3750
7: 0.8750
8: 0.0625
9: 0.5625
Base: 3
0: 0.0000
1: 0.3333
2: 0.6667
3: 0.1111
4: 0.4444
5: 0.7778
6: 0.2222
7: 0.5556
8: 0.8889
9: 0.0370
Base: 4
0: 0.0000
1: 0.2500
2: 0.5000
3: 0.7500
4: 0.0625
5: 0.3125
6: 0.5625
7: 0.8125
8: 0.1250
9: 0.3750
```



## PL/I

<lang>
vdcb: procedure (an) returns (bit (31)); /* 6 July 2012 */
   declare an fixed binary (31);
   declare (n, i) fixed binary (31);
   declare v bit (31) varying;

   n = an; v = ''b;
   do i = 1 by 1 while (n > 0);
      if iand(n, 1) = 1 then v = v || '1'b; else v = v || '0'b;
      n = isrl(n, 1);
   end;
   return (v);
end vdcb;

   declare i fixed binary (31);

   do i = 0 to 10;
      put skip list ('0.' || vdcb(i));
   end;

```

{{out}}

```txt

0.0000000000000000000000000000000
0.1000000000000000000000000000000
0.0100000000000000000000000000000
0.1100000000000000000000000000000
0.0010000000000000000000000000000
0.1010000000000000000000000000000
0.0110000000000000000000000000000
0.1110000000000000000000000000000
0.0001000000000000000000000000000
0.1001000000000000000000000000000
0.0101000000000000000000000000000

```



## Prolog


```prolog
% vdc( N, Base, Out )
% Out = the Van der Corput representation of N in given Base
vdc( 0, _, [] ).
vdc( N, Base, Out ) :-
    Nr is mod(N, Base),
    Nq is N // Base,
    vdc( Nq, Base, Tmp ),
    Out = [Nr|Tmp].

% Writes every element of a list to stdout; no newlines
write_list( [] ).
write_list( [H|T] ) :-
    write( H ),
    write_list( T ).

% Writes the Nth Van der Corput item.
print_vdc( N, Base ) :-
    vdc( N, Base, Lst ),
    write('0.'),
    write_list( Lst ).
print_vdc( N ) :-
    print_vdc( N, 2 ).

% Prints the first N+1 elements of the Van der Corput
% sequence, each to its own line
print_some( 0, _ ) :-
    write( '0.0' ).
print_some( N, Base ) :-
    M is N - 1,
    print_some( M, Base ),
    nl,
    print_vdc( N, Base ).
print_some( N ) :-
    print_some( N, 2 ).

test :-
   writeln('First 10 members in base 2:'),
   print_some( 9 ),
   nl,
   write('7th member in base 4 (stretch goal) => '),
   print_vdc( 7, 4 ).

```


{{out}} (result of test):

```txt

First 10 members in base 2:
0.0
0.1
0.01
0.11
0.001
0.101
0.011
0.111
0.0001
0.1001
7th member in base 4 (stretch goal) => 0.31
true .

```



## PureBasic


```PureBasic
Procedure.d nBase(n.i,b.i)
  Define r.d,s.i=1
  While n
    s*b
    r+(Mod(n,b)/s)
    n=Int(n/b)
  Wend
  ProcedureReturn r
EndProcedure

Define.i b,c
OpenConsole("van der Corput - Sequence")
For b=2 To 5
  Print("Base "+Str(b)+": ")
  For c=0 To 9
    Print(StrD(nBase(c,b),5)+~"\t")
  Next
  PrintN("")
Next
Input()
```

{{out}}

```txt
Base 2: 0.00000 0.50000 0.25000 0.75000 0.12500 0.62500 0.37500 0.87500 0.06250 0.56250
Base 3: 0.00000 0.33333 0.66667 0.11111 0.44444 0.77778 0.22222 0.55556 0.88889 0.03704
Base 4: 0.00000 0.25000 0.50000 0.75000 0.06250 0.31250 0.56250 0.81250 0.12500 0.37500
Base 5: 0.00000 0.20000 0.40000 0.60000 0.80000 0.04000 0.24000 0.44000 0.64000 0.84000
```



## Python

(Python3.x)

The multi-base sequence generator

```python
def vdc(n, base=2):
    vdc, denom = 0,1
    while n:
        denom *= base
        n, remainder = divmod(n, base)
        vdc += remainder / denom
    return vdc
```


'''Sample output'''

Base 2 and then 3:

```python
>>>
 [vdc(i) for i in range(10)]
[0, 0.5, 0.25, 0.75, 0.125, 0.625, 0.375, 0.875, 0.0625, 0.5625]
>>> [vdc(i, 3) for i in range(10)]
[0, 0.3333333333333333, 0.6666666666666666, 0.1111111111111111, 0.4444444444444444, 0.7777777777777777, 0.2222222222222222, 0.5555555555555556, 0.8888888888888888, 0.037037037037037035]
>>>
```



### As fractions

We can get the output as rational numbers if we use the fraction module
(and change its string representation to look like a fraction):

```python
>>>
 from fractions import Fraction
>>> Fraction.__repr__ = lambda x: '%i/%i' % (x.numerator, x.denominator)
>>> [vdc(i, base=Fraction(2)) for i in range(10)]
[0, 1/2, 1/4, 3/4, 1/8, 5/8, 3/8, 7/8, 1/16, 9/16]
```



### Stretch goal

Sequences for different bases:

```python
>>>
 for b in range(3,6):
	print('\nBase', b)
	print([vdc(i, base=Fraction(b)) for i in range(10)])

Base 3
[0, 1/3, 2/3, 1/9, 4/9, 7/9, 2/9, 5/9, 8/9, 1/27]

Base 4
[0, 1/4, 1/2, 3/4, 1/16, 5/16, 9/16, 13/16, 1/8, 3/8]

Base 5
[0, 1/5, 2/5, 3/5, 4/5, 1/25, 6/25, 11/25, 16/25, 21/25]
```



## Racket

Following the suggestion.

```racket
#lang racket
(define (van-der-Corput n base)
  (if (zero? n)
      0
      (let-values ([(q r) (quotient/remainder n base)])
        (/ (+ r (van-der-Corput q base))
           base))))
```

By digits, extracted arithmetically.

```racket
#lang racket
(define (digit-length n base)
  (if (< n base) 1 (add1 (digit-length (quotient n base) base))))
(define (digit n i base)
  (remainder (quotient n (expt base i)) base))
(define (van-der-Corput n base)
  (for/sum ([i (digit-length n base)]) (/ (digit n i base) (expt base (+ i 1)))))
```

Output.

```racket
(for ([base (in-range 2 (add1 5))])
  (printf "Base ~a: " base)
  (for ([n (in-range 0 10)])
    (printf "~a " (van-der-Corput n base)))
  (newline))

#| Base 2: 0 1/2 1/4 3/4 1/8 5/8 3/8 7/8 1/16 9/16
   Base 3: 0 1/3 2/3 1/9 4/9 7/9 2/9 5/9 8/9 1/27
   Base 4: 0 1/4 1/2 3/4 1/16 5/16 9/16 13/16 1/8 3/8
   Base 5: 0 1/5 2/5 3/5 4/5 1/25 6/25 11/25 16/25 21/25 |#
```



## REXX


### binary version

This REXX version only handles binary (base 2).

Virtually any integer (including negative) is allowed and is accurate (no rounding).

A range of integers (for output) is also supported.

```rexx
/*REXX program converts an integer (or a range)  ──►  a Van der Corput number in base 2.*/
numeric digits 1000                              /*handle almost anything the user wants*/
parse arg a b .                                  /*obtain the optional arguments from CL*/
if a==''  then parse value  0  10   with   a  b  /*Not specified?  Then use the defaults*/
if b==''  then b=a                               /*assume a  range  for a single number.*/

      do j=a  to b                               /*traipse through the range of numbers.*/
      _=VdC( abs(j) )                            /*convert absolute value of an integer.*/
      leading=substr('-',   2 + sign(j) )        /*if needed,  elide the leading sign.  */
      say leading || _                           /*show number, with leading minus sign?*/
      end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
VdC: procedure;    y=x2b( d2x( arg(1) ) )  + 0   /*convert to  hexadecimal, then binary.*/
     if y==0  then return 0                      /*handle the special case of zero.     */
              else return '.'reverse(y)          /*heavy lifting is performed by REXX.  */
```

'''output''' when using the default input of:   <tt> 0   10 </tt>

```txt

0
.1
.01
.11
.001
.101
.011
.111
.0001
.1001
.0101

```



### any radix up to 90

This version handles what the first version does,   plus any radix up to (and including) base '''90'''.

It can also support a list (enabled when the base is negative).

```rexx
/*REXX program converts an  integer  (or a range)  ──►  a Van der Corput number,        */
/*─────────────── in base 2,  or optionally, any other base up to and including base 90.*/
numeric digits 1000                              /*handle almost anything the user wants*/
parse arg a b r .                                /*obtain optional arguments from the CL*/
if a=='' | a=="," then parse value 0 10 with a b /*Not specified?  Then use the defaults*/
if b=='' | b=="," then b=a                       /* "      "         "   "   "      "   */
if r=='' | r=="," then r=2                       /* "      "         "   "   "      "   */
z=                                               /*a placeholder for a list of numbers. */
                do j=a  to b                     /*traipse through the range of integers*/
                _=VdC( abs(j), abs(r) )          /*convert the ABSolute value of integer*/
                _=substr('-',  2 + sign(j) )_    /*if needed, keep the leading  -  sign.*/
                if r>0  then say _               /*if positive base, then just show it. */
                        else z=z _               /*     ··· else append (build) a list. */
                end   /*j*/

if z\==''  then say strip(z)                     /*if a list is wanted, then display it.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
base: procedure; parse arg x, toB, inB           /*get a number,  toBase,  and  inBase. */
  /*╔══════════════════════════════════════════════════════════════════════════════════╗
    ║ Input to this function:    x       (X   is required  and it must be an integer). ║
    ║                          toBase    the base to convert   X   to    (default=10). ║
    ║                          inBase    the base  X  is expressed in    (default=10). ║
    ║                                                                                  ║
    ║                                    toBase & inBase  have a limit of:   2 ──► 90  ║
    ╚══════════════════════════════════════════════════════════════════════════════════╝*/
      @abc= 'abcdefghijklmnopqrstuvwxyz'         /*the lowercase Latin alphabet letters.*/
      @abcU=@abc;          upper @abcU           /*go whole hog & extend with uppercase.*/
      @@@= 0123456789 || @abc || @abcU           /*prefix them with the decimal digits. */
      @@@= @@@'<>[]{}()?~!@#$%^&*_+-=|\/;:`'     /*add some special characters as well, */
                                                 /*──those chars should all be viewable.*/
      numeric digits 1000                        /*what the hey, support bigun' numbers.*/
      maxB=length(@@@)                           /*maximum base (radix) supported here. */
      if toB==''  then toB=10                    /*if omitted,  then assume default (10)*/
      if inB==''  then inB=10                    /* "    "        "     "      "      " */
      #=0                                        /* [↓] convert base inB  X  ──► base 10*/
             do j=1  for length(x)               /*process each "numeral" in the string.*/
             _=substr(x, j, 1)                   /*pick off a "digit" (numeral) from  X.*/
             v=pos(_, @@@)                       /*get the value of this "digit"/numeral*/
             if v==0 | v>inB  then call erd      /*is it an illegal "digit" (numeral) ? */
             #=# * inB    + v  - 1               /*construct new number, digit by digit.*/
             end   /*j*/
      y=                                         /* [↓] convert base 10  # ──► base toB.*/
             do  while  #>=toB                   /*deconstruct the  new   number (#).   */
             y=substr(@@@, # // toB  + 1,  1)y   /*  construct the output number,  ···  */
             #=# % toB                           /*  ···  and also whittle down  #.     */
             end   /*while*/

      return substr(@@@,  # + 1,  1)y            /*return a constructed "numeric" string*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
erd:  say 'the character '   v    " isn't a legal numeral for base "    inB'.';    exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
VdC:  return '.'reverse(base(arg(1), arg(2)))    /*convert the #, reverse the #, append.*/
```

(A   ''negative''   base indicates to show numbers as a list.)

{{out|output|text=  when using the input of:     <tt> 0   30   -2 </tt>}}

```txt

.0 .1 .01 .11 .001 .101 .011 .111 .0001 .1001 .0101 .1101 .0011 .1011 .0111 .1111 .00001 .10001 .01001 .11001 .00101 .10101 .01101 .11101 .00011 .10011 .01011 .11011 .00111 .10111 .01111

```

{{out|output|text=  when using the input of:     <tt> 1   30   -3 </tt>}}

```txt

.1 .2 .01 .11 .21 .02 .12 .22 .001 .101 .201 .011 .111 .211 .021 .121 .221 .002 .102 .202 .012 .112 .212 .022 .122 .222 .0001 .1001 .2001 .0101

```

{{out|output|text=  when using the input of:     <tt> 1   30   -4 </tt>}}

```txt

.1 .2 .3 .01 .11 .21 .31 .02 .12 .22 .32 .03 .13 .23 .33 .001 .101 .201 .301 .011 .111 .211 .311 .021 .121 .221 .321 .031 .131 .231

```

{{out|output|text=  when using the input of:     <tt> 1   30   -5 </tt>}}

```txt

.1 .2 .3 .4 .01 .11 .21 .31 .41 .02 .12 .22 .32 .42 .03 .13 .23 .33 .43 .04 .14 .24 .34 .44 .001 .101 .201 .301 .401 .011

```

{{out|output|text=  when using the input of:     <tt> 55582777   55582804   -80 </tt>}}

```txt

.V[Is1 .W[Is1 .X[Is1 .Y[Is1 .Z[Is1 .<[Is1 .>[Is1 .[[Is1 .][Is1 .{[Is1 .}[Is1 .([Is1 .)[Is1 .?[Is1 .~[Is1 .![Is1 .@[Is1 .#[Is1 .$[Is1 .%[Is1 .^[Is1 .&[Is1 .*[Is1 .0]Is1 .1]Is1 .2]Is1 .3]Is1 .4]Is1

```



## Ring


```ring

decimals(4)
for base = 2 to 5
    see "base " + string(base) + " : "
    for number = 0 to 9
        see "" + corput(number, base) + " "
    next
    see nl
next

func corput n, b
     vdc = 0
     denom = 1
     while n
           denom *= b
           rem = n % b
           n = floor(n/b)
           vdc += rem / denom
     end
     return vdc

```

Output:

```txt

base 2 : 0 0.5000 0.2500 0.7500 0.1250 0.6250 0.3750 0.8750 0.0625 0.5625
base 3 : 0 0.3333 0.6667 0.1111 0.4444 0.7778 0.2222 0.5556 0.8889 0.0370
base 4 : 0 0.2500 0.5000 0.7500 0.0625 0.3125 0.5625 0.8125 0.1250 0.3750
base 5 : 0 0.2000 0.4000 0.6000 0.8000 0.0400 0.2400 0.4400 0.6400 0.8400

```



## Ruby

The multi-base sequence generator

```ruby
def vdc(n, base=2)
  str = n.to_s(base).reverse
  str.to_i(base).quo(base ** str.length)
end

(2..5).each do |base|
  puts "Base #{base}: " + Array.new(10){|i| vdc(i,base)}.join(", ")
end
```


'''Sample output'''

```txt

Base 2: 0/1, 1/2, 1/4, 3/4, 1/8, 5/8, 3/8, 7/8, 1/16, 9/16
Base 3: 0/1, 1/3, 2/3, 1/9, 4/9, 7/9, 2/9, 5/9, 8/9, 1/27
Base 4: 0/1, 1/4, 1/2, 3/4, 1/16, 5/16, 9/16, 13/16, 1/8, 3/8
Base 5: 0/1, 1/5, 2/5, 3/5, 4/5, 1/25, 6/25, 11/25, 16/25, 21/25

```



## Scala


```scala
object VanDerCorput extends App {
    def compute(n: Int, base: Int = 2) =
        Iterator.from(0).
            scanLeft(1)((a, _) => a * base).
            map(b => (n - 1) / b -> b).
            takeWhile(_._1 != 0).
            foldLeft(0d)((a, b) => a + (b._1 % base).toDouble / b._2 / base)

    val n = scala.io.StdIn.readInt
    val b = scala.io.StdIn.readInt
    (1 to n).foreach(x => println(compute(x, b)))
}
```


{{out}}

```txt

n: 30
base: 2
0.0
0.5
0.25
0.75
0.125
0.625
0.375
0.875
0.0625
0.5625
0.3125
0.8125
0.1875
0.6875
0.4375
0.9375
0.03125
0.53125
0.28125
0.78125
0.15625
0.65625
0.40625
0.90625
0.09375
0.59375
0.34375
0.84375
0.21875
0.71875
```



## Seed7

{{trans|D}}

```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const func float: vdc (in var integer: number, in integer: base) is func
  result
    var float: vdc is 0.0;
  local
    var integer: denom is 1;
    var integer: remainder is 0;
  begin
    while number <> 0 do
      denom *:= base;
      remainder := number rem base;
      number := number div base;
      vdc +:= flt(remainder) / flt(denom);
    end while;
  end func;

const proc: main is func
  local
    var integer: base is 0;
    var integer: number is 0;
  begin
    for base range 2 to 5 do
      writeln;
      writeln("Base " <& base);
      for number range 0 to 9 do
        write(vdc(number, base) digits 6 <& " ");
      end for;
      writeln;
    end for;
  end func;
```


{{out}}

```txt


Base 2
0.000000 0.500000 0.250000 0.750000 0.125000 0.625000 0.375000 0.875000 0.062500 0.562500

Base 3
0.000000 0.333333 0.666667 0.111111 0.444444 0.777778 0.222222 0.555556 0.888889 0.037037

Base 4
0.000000 0.250000 0.500000 0.750000 0.062500 0.312500 0.562500 0.812500 0.125000 0.375000

Base 5
0.000000 0.200000 0.400000 0.600000 0.800000 0.040000 0.240000 0.440000 0.640000 0.840000

```



## Sidef

{{trans|Perl}}

```ruby
func vdc(value, base=2) {
    while (value[-1] > 0) {
        value.append(value[-1] / base -> int)
    }
    var (x, sum) = (1, 0)
    value.each { |i|
        sum += ((i % base) / (x *= base))
    }
    return sum
}
 
for base in (2..5) {
    var seq = 10.of {|i| vdc([i], base) }
    "base %d: %s\n".printf(base, seq.map{|n| "%.4f" % n}.join(', '))
}
```

{{out}}

```txt
base 2: 0.0000, 0.5000, 0.2500, 0.7500, 0.1250, 0.6250, 0.3750, 0.8750, 0.0625, 0.5625
base 3: 0.0000, 0.3333, 0.6667, 0.1111, 0.4444, 0.7778, 0.2222, 0.5556, 0.8889, 0.0370
base 4: 0.0000, 0.2500, 0.5000, 0.7500, 0.0625, 0.3125, 0.5625, 0.8125, 0.1250, 0.3750
base 5: 0.0000, 0.2000, 0.4000, 0.6000, 0.8000, 0.0400, 0.2400, 0.4400, 0.6400, 0.8400
```



## Swift

{{trans|C}}

```swift
func vanDerCorput(n: Int, base: Int, num: inout Int, denom: inout Int) {
  var n = n, p = 0, q = 1

  while n != 0 {
    p = p * base + (n % base)
    q *= base
    n /= base
  }

  num = p
  denom = q

  while p != 0 {
    n = p
    p = q % p
    q = n
  }

  num /= q
  denom /= q
}

var num = 0
var denom = 0

for base in 2...5 {
  print("base \(base): 0 ", terminator: "")

  for n in 1..<10 {
    vanDerCorput(n: n, base: base, num: &num, denom: &denom)

    print("\(num)/\(denom) ", terminator: "")
  }

  print()
}
```


{{out}}

```txt
base 2: 0 1/2 1/4 3/4 1/8 5/8 3/8 7/8 1/16 9/16
base 3: 0 1/3 2/3 1/9 4/9 7/9 2/9 5/9 8/9 1/27
base 4: 0 1/4 1/2 3/4 1/16 5/16 9/16 13/16 1/8 3/8
base 5: 0 1/5 2/5 3/5 4/5 1/25 6/25 11/25 16/25 21/25
```



## Stata

Stata has builtin functions in Mata to compute '''[https://en.wikipedia.org/wiki/Halton_sequence Halton sequences]''', which are generalizations of the Van der Corput sequence. See '''[https://www.stata.com/help.cgi?mf_halton halton]''' in Stata help, and two articles in the Stata Journal: '''[http://www.stata-journal.com/article.html?article=st0244 Scrambled Halton sequences in Mata]''' by Stanislav Kolenikov and '''[http://www.stata-journal.com/article.html?article=st0103 Generating Halton sequences using Mata]''' by David M. Drukker and Richard Gates.


```stata
mata
// 5th term of Van der Corput sequence
halton(1,1,5)
  .625

// the first 10 terms of Van der Corput sequence
halton(10,1)
            1
     +---------+
   1 |     .5  |
   2 |    .25  |
   3 |    .75  |
   4 |   .125  |
   5 |   .625  |
   6 |   .375  |
   7 |   .875  |
   8 |  .0625  |
   9 |  .5625  |
  10 |  .3125  |
     +---------+

// the first 10 terms of Van der Corput sequence in base 3
ghalton(10,3,0)
                  1
     +---------------+
   1 |  .3333333333  |
   2 |  .6666666667  |
   3 |  .1111111111  |
   4 |  .4444444444  |
   5 |  .7777777778  |
   6 |  .2222222222  |
   7 |  .5555555556  |
   8 |  .8888888889  |
   9 |   .037037037  |
  10 |  .3703703704  |
     +---------------+

end
```


Reproduce the plot in the task description:


```stata
clear
mata
st_addobs(2500)
st_addvar("double","x")
st_addvar("double","y")
st_addvar("double","z")
k=1::2500
st_store(k,1,k)
st_store(k,2,0.5*runiform(2500,1))
st_store(k,3,0.5:+0.5*halton(2500,1))
end
twoway scatter y x, msize(tiny) color(blue) ///
    || scatter z x, msize(tiny) color(green) legend(off) xtitle("") ///
	title(Distribution: Van der Corput (top) vs pseudorandom) ///
	ylabel(, angle(0) format(%3.1f))
```



## Tcl

The core of this is code to handle digit reversing. Note that this also tackles negative numbers (by preserving the sign independently).

```tcl
proc digitReverse {n {base 2}} {
    set n [expr {[set neg [expr {$n < 0}]] ? -$n : $n}]
    set result 0.0
    set bit [expr {1.0 / $base}]
    for {} {$n > 0} {set n [expr {$n / $base}]} {
	set result [expr {$result + $bit * ($n % $base)}]
	set bit [expr {$bit / $base}]
    }
    return [expr {$neg ? -$result : $result}]
}
```

Note that the above procedure will produce terms of the Van der Corput sequence by default.

```tcl
# Print the first 10 terms of the Van der Corput sequence
for {set i 1} {$i <= 10} {incr i} {
    puts "vanDerCorput($i) = [digitReverse $i]"
}

# In other bases
foreach base {3 4 5} {
    set seq {}
    for {set i 1} {$i <= 10} {incr i} {
	lappend seq [format %.5f [digitReverse $i $base]]
    }
    puts "${base}: [join $seq {, }]"
}
```

{{out}}

```txt

vanDerCorput(1) = 0.5
vanDerCorput(2) = 0.25
vanDerCorput(3) = 0.75
vanDerCorput(4) = 0.125
vanDerCorput(5) = 0.625
vanDerCorput(6) = 0.375
vanDerCorput(7) = 0.875
vanDerCorput(8) = 0.0625
vanDerCorput(9) = 0.5625
vanDerCorput(10) = 0.3125
3: 0.33333, 0.66667, 0.11111, 0.44444, 0.77778, 0.22222, 0.55556, 0.88889, 0.03704, 0.37037
4: 0.25000, 0.50000, 0.75000, 0.06250, 0.31250, 0.56250, 0.81250, 0.12500, 0.37500, 0.62500
5: 0.20000, 0.40000, 0.60000, 0.80000, 0.04000, 0.24000, 0.44000, 0.64000, 0.84000, 0.08000

```



## VBA

{{trans|Phix}}Base only.
```vb
Private Function vdc(ByVal n As Integer, BASE As Variant) As Variant
    Dim res As String
    Dim digit As Integer, g As Integer, denom As Integer
    denom = 1
    Do While n
        denom = denom * BASE
        digit = n Mod BASE
        n = n \ BASE
        res = res & CStr(digit) '+ "0"
    Loop
    vdc = IIf(Len(res) = 0, "0", "0." & res)
End Function

Public Sub show_vdc()
    Dim v As Variant, j As Integer
    For i = 2 To 5
        Debug.Print "Base "; i; ": ";
        For j = 0 To 9
            v = vdc(j, i)
            Debug.Print v; " ";
        Next j
        Debug.Print
    Next i
End Sub
```
{{out}}

```txt
Base  2 : 0 0.1 0.01 0.11 0.001 0.101 0.011 0.111 0.0001 0.1001
Base  3 : 0 0.1 0.2 0.01 0.11 0.21 0.02 0.12 0.22 0.001
Base  4 : 0 0.1 0.2 0.3 0.01 0.11 0.21 0.31 0.02 0.12
Base  5 : 0 0.1 0.2 0.3 0.4 0.01 0.11 0.21 0.31 0.41
```


## VBScript


```VBScript
'http://rosettacode.org/wiki/Van_der_Corput_sequence
'Van der Corput Sequence fucntion call = VanVanDerCorput(number,base)

Base2 = "0" : Base3 = "0" : Base4 = "0" : Base5 = "0"
Base6 = "0" : Base7 = "0" : Base8 = "0" : Base9 = "0"

l = 1
h = 1
Do Until l = 9
	'Set h to the value of l after each function call
	'as it sets it to 0 - see lines 37 to 40.
	Base2 = Base2 & ", " & VanDerCorput(h,2) : h = l
	Base3 = Base3 & ", " & VanDerCorput(h,3) : h = l
	Base4 = Base4 & ", " & VanDerCorput(h,4) : h = l
	Base5 = Base5 & ", " & VanDerCorput(h,5) : h = l
	Base6 = Base6 & ", " & VanDerCorput(h,6) : h = l
	l = l + 1
Loop

WScript.Echo "Base 2: " & Base2
WScript.Echo "Base 3: " & Base3
WScript.Echo "Base 4: " & Base4
WScript.Echo "Base 5: " & Base5
WScript.Echo "Base 6: " & Base6

'Van der Corput Sequence
Function VanDerCorput(n,b)
	k = RevString(Dec2BaseN(n,b))
	For i = 1 To Len(k)
		VanDerCorput = VanDerCorput + (CLng(Mid(k,i,1)) * b^-i)
	Next
End Function

'Decimal to Base N Conversion
Function Dec2BaseN(q,c)
	Dec2BaseN = ""
	Do Until q = 0
		Dec2BaseN = CStr(q Mod c) & Dec2BaseN
		q = Int(q / c)
	Loop
End Function

'Reverse String
Function RevString(s)
	For j = Len(s) To 1 Step -1
		RevString = RevString & Mid(s,j,1)
	Next
End Function
```

{{out}}

```txt

Base 2: 0, 0.5, 0.5, 0.25, 0.75, 0.125, 0.625, 0.375, 0.875
Base 3: 0, 0.333333333333333, 0.666666666666667, 0.111111111111111, 0.444444444444444, 0.777777777777778, 0.222222222222222, 0.555555555555556, 0.888888888888889
Base 4: 0, 0.25, 0.5, 0.75, 0.0625, 0.3125, 0.5625, 0.8125, 0.125
Base 5: 0, 0.2, 0.4, 0.6, 0.8, 0.04, 0.24, 0.44, 0.64
Base 6: 0, 0.166666666666667, 0.333333333333333, 0.5, 0.666666666666667, 0.833333333333333, 2.77777777777778E-02, 0.194444444444444, 0.361111111111111

```



## Visual Basic .NET

{{trans|C}}

```vbnet
Module Module1

    Function ToBase(n As Integer, b As Integer) As String
        Dim result = ""
        If b < 2 Or b > 16 Then
            Throw New ArgumentException("The base is out of range")
        End If

        Do
            Dim remainder = n Mod b
            result = "0123456789ABCDEF"(remainder) + result
            n = n \ b
        Loop While n > 0

        Return result
    End Function

    Sub Main()
        For b = 2 To 5
            Console.WriteLine("Base = {0}", b)
            For i = 0 To 12
                Dim s = "." + ToBase(i, b)
                Console.Write("{0,6} ", s)
            Next
            Console.WriteLine()
            Console.WriteLine()
        Next
    End Sub

End Module
```

{{out}}

```txt
Base = 2
    .0     .1    .10    .11   .100   .101   .110   .111  .1000  .1001  .1010  .1011  .1100

Base = 3
    .0     .1     .2    .10    .11    .12    .20    .21    .22   .100   .101   .102   .110

Base = 4
    .0     .1     .2     .3    .10    .11    .12    .13    .20    .21    .22    .23    .30

Base = 5
    .0     .1     .2     .3     .4    .10    .11    .12    .13    .14    .20    .21    .22
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

func real VdC(N);       \Return Nth term of van der Corput sequence in base 2
int  N;
real V, U;
[V:= 0.0;  U:= 0.5;
repeat  N:= N/2;
        if rem(0) then V:= V+U;
        U:= U/2.0;
until   N=0;
return V;
];

int N;
for N:= 0 to 10-1 do
        [IntOut(0, N);  RlOut(0, VdC(N));  CrLf(0)]
```

{{out}}

```txt

0    0.00000
1    0.50000
2    0.25000
3    0.75000
4    0.12500
5    0.62500
6    0.37500
7    0.87500
8    0.06250
9    0.56250

```



## zkl

{{trans|Python}}

```zkl
fcn vdc(n,base=2){
   vdc:=0.0; denom:=1;
   while(n){ reg remainder;
      denom *= base;
      n, remainder = n.divr(base);
      vdc += (remainder.toFloat() / denom);
   }
   vdc
}
```

{{trans|Ruby}}

```zkl
fcn vdc(n,base=2){
   str:=n.toString(base).reverse();
   str.toInt(base).toFloat()/(base.toFloat().pow(str.len()))
}
```

{{out}}

```txt

[0..10].apply(vdcR).println("base 2");
L(0,0.5,0.25,0.75,0.125,0.625,0.375,0.875,0.0625,0.5625,0.3125)base 2

[0..10].apply(vdc.fp1(3)).println("base 3");
L(0,0.333333,0.666667,0.111111,0.444444,0.777778,0.222222,0.555556,0.888889,0.037037,0.37037)base 3

```
