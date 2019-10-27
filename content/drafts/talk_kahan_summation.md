+++
title = "Talk:Kahan summation"
description = ""
date = 2019-02-17T18:47:50Z
aliases = []
[extra]
id = 18367
[taxonomies]
categories = []
tags = []
+++

==Task - The problem is badly formulated==

Rounding errors in sums do not occur because we use a certain number of digits after the decimal point. The source of the such errors is the '''floating point''' representation of decimal fractions. As you can see, when using '''fixed point''' representation, there are no rounding errors. Of course, the calculated sum of a / 3 + a / 3 + a / 3 is less than a, but this error is caused by the rounding at division.

```C

/*
 * RosettaCode: Kahan summation, C89 (MS Visual Studio 2010)
 *
 * C language has no fixed decimal fraction type. Nevertheless to obtain 
 * "six-digits precision" we can use ordinary fractions with fixed denominator.
 */
#include <stdio.h>

#define DECIMAL_FRACTION long int
#define DENOMINATOR 1000000L
#define DECIMAL_TO_FIXED(WHOLE,FRACTIONAL) (WHOLE*DENOMINATOR+FRACTIONAL)
#define FIXED_TO_WHOLE(VALUE) (VALUE / DENOMINATOR)
#define FIXED_TO_FRACT(VALUE) (VALUE % DENOMINATOR)

int main(void)
{
    DECIMAL_FRACTION a = DECIMAL_TO_FIXED(10000,0);
    DECIMAL_FRACTION b = DECIMAL_TO_FIXED(3,14159);
    DECIMAL_FRACTION c = DECIMAL_TO_FIXED(2,71828);

    DECIMAL_FRACTION leftSum;
    DECIMAL_FRACTION rightSum;
    DECIMAL_FRACTION kahanSum;

    leftSum  = a;
    leftSum += b;
    leftSum += c;

    rightSum  = c;
    rightSum += b;
    rightSum += a;

    {
        /*
         * Actually we sum only a+b+c with an un-rolled implementation
         * of Kahan algorithm. KISS
         */

        DECIMAL_FRACTION correction           = 0;
        DECIMAL_FRACTION inputMinusCorrection = 0;
        DECIMAL_FRACTION updatedSum           = 0;

        kahanSum = a;

        inputMinusCorrection = b - correction;
        updatedSum  = kahanSum + inputMinusCorrection;
        correction  = updatedSum - kahanSum;
        correction -= inputMinusCorrection;
        kahanSum = updatedSum;

        inputMinusCorrection = c - correction;
        updatedSum  = kahanSum + inputMinusCorrection;
        correction  = updatedSum - kahanSum;
        correction -= inputMinusCorrection;
        kahanSum = updatedSum;
    }

#define PRINT(S,V) printf(S##" = %d.%d\n", FIXED_TO_WHOLE(V), FIXED_TO_FRACT(V))

    PRINT("a", a);
    PRINT("b", b);
    PRINT("c", c);
    putchar('\n');

    PRINT("  (a+b)+c", leftSum);
    PRINT("  a+(b+c)", rightSum);
    PRINT("Kahan sum", kahanSum);

    if ( leftSum == kahanSum && rightSum == kahanSum )
        puts("\nC can compute on fixed point numbers without round-off errors");

    getchar();
    return 0;
}

```

{{Out}}

```txt

a = 1410.65408
b = 3.14159
c = 2.71828

  (a+b)+c = 1415.151395
  a+(b+c) = 1415.151395
Kahan sum = 1415.151395

C can compute on fixed point numbers without round-off errors

```


==Task==
The idea of showing the Kahan summation on Rosettacode is good, but the Task is not good yet. I suggest to remove the requirements of using a Decimal module and make it optional. So many languages can use normal floating point values. I also suggest to increase the number of input values and the number of their digits, to better show why the Kahan summation is useful.

This is a D version of the Task, without a Decimal:


```d
real kahanSum(T)(in T[] data) pure nothrow @safe @nogc {
    real tot = 0, c = 0;
    foreach (immutable x; data) {
        immutable y = x - c;
        immutable t = tot + y;
        c = (t - tot) - y;
        tot = t;
    }
    return tot;
}

void main() {
    import std.stdio, std.algorithm;

    enum a = 10000.0L, b = 3.14159L, c = 2.71828L;
    writefln("%5.10f", (a + b) + c);
    writefln("%5.10f", [a, b, c].kahanSum);
    writefln("%5.10f", [a, b, c].sum);
}
```


It prints (the D+Phobos sum performs a Kahan summation):


```txt
10005.8598700000
10005.8598700000
10005.8598700000
```
 -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]]) 10:28, 11 December 2014 (UTC)

:Would you need different numbers for different precision numbers? What numbers to choose as I would prefer some standardisation.
:Or should it be written so that the example is forced to show the effect with the constants they use? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:49, 11 December 2014 (UTC)

:: I also agree Kahan summation is a good task but have some issues.  It seems Kahan summation is meaningful for fixed-precision floating-point numbers.  That is, floating point numbers where the number of significant digits (or bits) is limited.  This needs to be clear in the task description.  If a language has no convenient fixed-precision floating-point representation, people should feel free to omit it.

::I think the fixed-precision floating-point type most languages will support is IEEE 754 64-bit float.  I suggest we provide task data crafted for this type.  The Python example data works for floating point math that can be limited to 6 significant decimal places.  It would be fine to retain this as alternative data for languages that can conveniently limit the significant decimal digits as Python can.  If other languages come along that can do neither IEEE 754 64-bit floats nor 6-significant decimal digit floats but have some other limited precision floating point representation, we should allow them appropriate test data.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 20:29, 11 December 2014 (UTC)

::Example added.  Bearophile's suggestion of adding more numbers turned out to be good.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 20:49, 11 December 2014 (UTC)

::I also suggest we explicitly disallow solutions showing the same answer from both a naive method and Kahan and concluding, "my language already does it."  A valid solution should show that Kahan provides some advantage over some other method.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 20:57, 11 December 2014 (UTC)

All good suggestions.Please join in in improving this. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 23:41, 11 December 2014 (UTC)

:OK made my task revisions for the night, taking your comments into consideration... --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:40, 12 December 2014 (UTC)
:: Modified the R example to comply with the new task, still in R there is no difference because you start losing precision in R after the 15 or 16 decimal place. Perhaps the tasks needs a different set of numbers to illustrate the case? --[[User:Jmcastagnetto|Jmcastagnetto]] ([[User talk:Jmcastagnetto|talk]]) 16:45, 12 December 2014 (UTC)
::: Further modification to include the subtask for R. Now, using the precision that the main tasks defines, apparently there is no difference, but if you compare the results of both operations, there is a difference. Added code to show that in the R example. --[[User:Jmcastagnetto|jmcastagnetto]] ([[User talk:Jmcastagnetto|talk]]) 17:27, 12 December 2014 (UTC)

::::Hi I  wonder if you might bring this up in your R language community and ask about the perplexing result? Maybe the setting of precision only affects output precision and internally a higher precision is used for values? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:41, 12 December 2014 (UTC)

::::Yep. I found this: https://stat.ethz.ch/pipermail/r-help/2010-January/226016.html, which leads me to believe that R is using its full, floating point, for arithmetic and you are only limiting the digits in the output - not even calculating in decimal arithmatic. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:48, 12 December 2014 (UTC)

::::Just found this R info: https://www.stat.auckland.ac.nz/~stat782/downloads/04-Numerics.pdf. according to that, R is floating point and the FP task section should be followed. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:56, 12 December 2014 (UTC)

::::OK, indeed the options() call just restricts the number of digits being displayed, not the actual number used in calculations. In that respect it is in the same boat as PHP. AFAIK there is a package (not a base component) that can deal with arbitrary precision (http://cran.r-project.org/web/packages/Rmpfr/), but that is not what this particular task is aiming to show. I have redone the operations and results on a Windows 7 64-bit machine from a friend, will check again on my Ubuntu 64-bit box to corroborate the results. --[[User:Jmcastagnetto|jmcastagnetto]] ([[User talk:Jmcastagnetto|talk]]) 02:11, 17 December 2014 (UTC)

==So far, in J and R==
We have decimal arithmetic not being output digits, and built-in equality fuzz. This, I think, is all useful info to know when comparing languages,
. The j example manages to follow the spirit of the task and leave a readable and informative entry. The R example could become just as good if it broke down the task in a similar way, but that would need more detailed knowledge of how the language treats numbers.

:Indeed the J example is much better that my plain R one, will rework it to make it more understandable --[[User:Jmcastagnetto|jmcastagnetto]] ([[User talk:Jmcastagnetto|talk]]) 02:15, 17 December 2014 (UTC)

The task description could always be improved, I personally like the direction it is changed to, but I am starting to wonder if it will end in a morass of floating point fine-point'erry! 

Any ideas for a save? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:48, 13 December 2014 (UTC)

:I have the same concerns.  How about something like this,

```python
def kahan(vals, start = 0):
    tot = start
    c = 0
    for x in vals:
        y = x - c
        t = tot + y
        c = (t - tot) - y
        tot = t
    return tot

def triangle(n):
    return n * (n - 1) / 2

bigVal = 1e20
numSmall = 54321

print("Sequential: ", sum(range(numSmall), bigVal))
print("Kahan       ", kahan(range(numSmall), bigVal))
print("Triangle:               ", triangle(numSmall))
```

{{out}}

```txt

Sequential:  1.0000000000146198e+20
Kahan        1.0000000000147536e+20
Triangle:                1475358360.0

```

:The idea is to sum enough numbers to skip over the fine-point-erry, while still picking numbers where it's easy to see the correct answer.  Adding just three numers is always going rely on doing stuff to the last bit.  If you add lots of numbers you can get the result we're interested in to span multiple decimal places.  Most people will accept that the triangle fomula should give a good anwer, triangle(54321) fits in a 32 bit int.  For types other than 754 binary64, it should be easy to pick other parameters bigVal and numSmall that will still show the loss of precision in the sequential sum.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 03:26, 14 December 2014 (UTC)

Thanks Sonia. I am still digesting your suggestion, but just on first reading, (and little cogitation I admit), the solution is not "self scaling" in any way, for different precisions. The current task floating point examples might self scale in say Python, but do not in J because of their "fuzzy comparison" (a problem with the task description and not a slur of J by the way). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:18, 14 December 2014 (UTC)

==Task 2==
I suggest to rewrite this task from scratch, giving in the task description 7 or 10 64 bit floating point numbers (or more if generated with a portable random generator), and show the results with a basic summation algorithm versus doing it with Kahan. And show in some ways that the second loses less precision. Nothing more. -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]]) 10:34, 20 December 2014 (UTC)

:But what about the issues we have already uncovered in several languages? What about languages that have much better control of number representation and calculations? There is a chance for languages that have these extra capabilities to shine and I would not want to lose that by fixing on one representation. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:03, 20 December 2014 (UTC)

::There is no chance of solving this task unless the representation is known.  Kahan summation is only meaningful for fixed-precision floating-point formats. (I know this issue hasn't come up but I expect some people to hear "decimal" and try to use a fixed-point decimal type, which I think may be more common than floating-point decimal.)  For an appropriate type then, observing a difference from simple summation is only possible when results are displayed with sufficient precision.  For the example with epsilon in R for example, you must know that you have nearly 16 significant decimal digits, then since epsilon is a difference in the 16th decimal place, you must display 16 significant figures.  R can do this with sprintf.  I suspect most languages will have a way of displaying numbers at either full or specified precision.  Even in the case where full resolution was not displayed, a difference between Kahan and simple summation could still be shown if you at least knew how much precision is displayed (or equivalently, how much precision is suppressed.)  Then you could contrive data so the the difference would ultimately be apparent at whatever (known) precision is displayed.  Not knowing if you are doing fixed-precision floating point math, not knowing the precision you are working with, or not knowing what precision is displayed are showstoppers to illustrating Kahan summation.  Some task requirements to demonstrate these capabilities would help avoid much floundering.  For example,
::*State the precision and base used, ex, 6 digit decimal, or 53 bit binary.
::*Determine the unit of least precision, or unit of last place, or ULP.  In general this is base**(-precision).
::*Compute and show 1, ULP, and 1-ULP in sufficient precision to show all three different.
::*Now compute and show 1+ULP at full precision.  It must show 1.000... with zeros covering the full precision.  Examples,

```txt

6 digit decimal
1:     1.00000
ULP:   .000001
1-ULP: .999999
1+ULP: 1.00000

```


```txt

IEEE 754 binary64, base 2, precision 53.
1:     1.000000000000000e+00
ULP:   1.110223024625157e-16
1-ULP: 9.999999999999999e-01
1+ULP: 1.000000000000000e+00

```

:If you can't reproduce 1-ULP showing something less than one and 1+ULP showing exactly one, you can't do the task.  It means you don't have a suitable representation, you don't have the ULP, you can't display full precision, or something.

:The epsilon technique should find ULP if the divisor is the base, and if equality can be tested without fuzzing.  If you count iterations, it will tell you the precision as well.  Really though I suggest the epsilon technique not be required.  You should know your precision and base without computing them.

```go
package main

import (
    "fmt"
    "math"
)

// defining constants for IEEE 754 binary64
const (
    base = 2
    prec = 53
)

// "epsilon"
func ulp() (u float64, p int) {
    u = 1.
    for 1+u > 1 {
        p++
        u /= 2
    }
    return
}

func main() {
    u, p := ulp()
    fmt.Println("ulp by definintion:", math.Pow(base, -prec))
    fmt.Println("ulp computed:      ", u)
    fmt.Println("computed precision:", p)
}
```

{{out}}

```txt

ulp by definintion: 1.1102230246251565e-16
ulp computed:       1.1102230246251565e-16
computed precision: 53

```

::Kahan summation of 1, +ULP, -ULP does technically show that it works, but I think the example is a little abstract and doesn't illustrate the practical value of Kahan summation well.  My examples adding 10 copies of pi or adding a triangle of numbers were attempts at doing this and showing accumulated discrepancies greater than just 1 ULP.  Here's one more attempt, this time summing a bunch of random numbers and accumulating a reference result.

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

func kahan(s []float64) float64 {
    var tot, c float64
    for _, x := range s {
        y := x - c
        t := tot + y
        c = (t - tot) - y
        tot = t
    }
    return tot
}

func seq(s []float64) float64 {
    tot := 0.
    for _, x := range s {
        tot += x
    }
    return tot
}

// defining constants for IEEE 754 binary64
const (
    base = 2
    prec = 53
)

var ulp = math.Pow(base, -prec)

func main() {
    rand.Seed(time.Now().UnixNano())
    n := make([]float64, 10001)
    n[0] = 1
    refSum := 0.
    for i := 1; i < len(n); i++ {
        r := rand.Float64() * 1.1 * ulp
        n[i] = r
        refSum += r
    }

    fmt.Printf("Sequential: %.15f\n", seq(n))
    fmt.Printf("Kahan:      %.15f\n", kahan(n))
    fmt.Printf("Reference:  %.18f\n", refSum)
}
```

{{out}}

```txt

Sequential: 1.000000000000215
Kahan:      1.000000000000614
Reference:  0.000000000000613562

```

:It's a little strange because that 1.1 is a fudge factor needed for satisfying results.  A multiple of a little more than half the base works well...because rounding...  Anyway, it's nice to see the Kahan sum contain a rouding of the reference sum and see the sequential sum lose a few decimal places.  Here's similar code for six decimal places,

```python
from decimal import *
from random import random

getcontext().prec = 6

def kahan(vals, start = 0):
    tot = start
    c = 0
    for x in vals:
        y = x - c
        t = tot + y
        c = (t - tot) - y
        tot = t
    return tot

ulp = Decimal('1')
prec = 0
while 1+ulp > 1:
    prec += 1
    ulp /= 10
print(ulp, prec)

n = []
refSum = Decimal('0')
for i in range(10000):
    r = Decimal(random() * 6) * ulp
    n.append(Decimal(r))
    refSum += r

print("Sequential: ", sum(n, Decimal('1')))
print("Kahan       ", kahan(n, Decimal('1')))
print("Reference:  ", refSum)
```

{{out}}

```txt

0.000001 6
Sequential:  1.01615
Kahan        1.02979
Reference:   0.0297927

```

::&mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 01:53, 21 December 2014 (UTC)


==Epsilon computation==
The "Epsilon computation around 1" sub task should be a task by itself.

```python
epsilon = 1.0
while 1.0 + epsilon != 1.0:
    epsilon = epsilon / 2.0
```

The "Epsilon computation around 1" sub task is a nice indicator about IEEE 754 floating point, used (or not) in the language implementation (specific compilers). Most of the time languages are not explicit about the precision required in floating point data type.

### IEEE 754

IEEE 754 (1985 & 2008) floating point has 4 major data types:

```txt

Precision Name      Bits  Mantissa   Decimal precision 
------------------  ----  ---------  -----------------
simple   precision    32  24 bits    5.9604E-9
double   precision    64  53 bits    1.1102E-16
extended precision    80  64 bits    5.4210E-20
decimal128           120  34 digits  1.0000E-34

```

C 99, Fortran 77, have IEEE-754 corresponding data types:

```txt

IEEE 754 name       GNU C        Intel C      Visual C  Fortran 77  Fortran 95              VB .Net           
------------------  -----------  -----------  --------  ----------  ----------------------  ----------  
simple   precision  float        float        float     real*4      SELECTED_REAL_KIND(8)   Single
double   precision  double       double       double    real*8      SELECTED_REAL_KIND(16)  Double
extended precision  long double  long double  n/a       real*10     SELECTED_REAL_KIND(20)  n/a
decimal128          __float128   __Quad       n/a       real*16     SELECTED_REAL_KIND(34)  n/a

```

In Microsoft Visual C long double is treated as double.

### Compilers

The epsilon computation using different implementation of languages and different data types gives the
following results:

```txt

Language   Compiler   Declaration  N  Epsilon                    IEEE-754
--------   --------   ------------ -- -------------------------- --------
C++        VC++ 6.0   float        53 1.110223E-16               Double
C++        VC++ 6.0   double       53 1.110223E-16               Double
C++        VC++ 6.0   long double  53 1.110223E-16               Double
Fortran    Plato      real*4       64 5.421011E-20               Extended     
Fortran    Plato      real*8       64 5.421010862428E-20         Extended
Fortran    Plato      real*10      64 5.42101086242752217E-20    Extended
Fortran    Plato      real*16      64 5.42101086242752217E-20    Extended
Pascal     Free       real         64 5.42101086242752E-020      Extended
Pascal     Free       double       64 5.42101086242752E-020      Extended
Pascal     Free       extended     64 5.4210108624275222E-0020   Extended
Perl       Strawberry              53 1.11022302462516e-016      Double
Python     v335                    53 1.1102230246251565e-16     Double
SmallBasic 1.2                     94 1.0e-28                    Fixed128*
VB6        VB 6.0     Single       24 5.960464E-08               Single
VB6        VB 6.0     Double       53 1.11022302462516E-16       Double
VBA        VBA 7.1    Single       24 5.960464E-08               Single
VBA        VBA 7.1    Double       53 1.11022302462516E-16       Double
VBScript   Win 10                  53 1.110223E-16               Double
VB.Net     VS 2013    Single       53 1.110223E-16               Double 
VB.Net     VS 2013    Double       53 1.11022302462516E-16       Double
VB.Net     VS 2013    Decimal      94 1.0e-28                    Fixed128*

```

N is the loop count.

Fixed128 : Use of IEEE Decimal128 floating point to emulate fixed point arithmetic.


It is interesting to see that several compilers do not use the different IEEE-754 precisions to implement the different data types.
The trade-off between compiler simplicity a runtime efficiency is: why to bother with different floating point precisions and all the implied cross conversion routines, why not use only the higher precision.


### Kahan summation

Kahan summation algorithm task is a good idea but, the example numbers : 10000.0, 3.14159, 2.71828 
are a bad choice, because no rounding errors when IEEE 754 floating point double precision (64 bits) are used by the language, and unfortunatly is now the standard. Let's note that William Kahan is a father of the original IEEE 754 and its revisions.



--[[User:PatGarrett|PatGarrett]] ([[User talk:PatGarrett|talk]]) 16:43, 16 February 2019 (UTC)
