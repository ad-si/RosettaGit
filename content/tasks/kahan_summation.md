+++
title = "Kahan summation"
description = ""
date = 2019-10-04T12:35:35Z
aliases = []
[extra]
id = 18366
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "awk",
  "c",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "fortran",
  "go",
  "j",
  "java",
  "julia",
  "kotlin",
  "lua",
  "objeck",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "tcl",
  "zkl",
]
+++

<small>
:;Note to language implementers:
:This task touches on the details of number representation that is often hidden for convenience in many programming languages. You may need to understand the difference between precision in calculations and precision in presenting answers for example; or understand decimal arithmetic as distinct from the usual binary based floating point schemes used in many languages.
</small>

The [[wp:Kahan summation algorithm|Kahan summation algorithm]] is a method of summing a series of numbers represented in a limited precision in a way that minimises the loss of precision in the result.

'''The task''' is to follow the previously linked Wikipedia articles algorithm and its [[wp:Kahan_summation_algorithm#Worked_example|worked example]] by:
# Do all arithmetic in decimal to a precision of six digits.
# Write a function/method/procedure/subroutine/... to perform Kahan summation on an ordered collection of numbers, (such as a list of numbers).
# Create the three numbers <code>a, b, c</code> equal to <code>10000.0, 3.14159, 2.71828</code> respectively.
# Show that the simple left-to-right summation, equivalent to <code>(a + b) + c</code> gives an answer of 10005.8
# Show that the Kahan function applied to the sequence of values <code>a, b, c</code> results in the more precise answer of 10005.9


'''If your language does not have six digit decimal point''', but does have some fixed precision floating point number system then '''state this''' and try this alternative task using floating point numbers:
* Follow the same steps as for the main task description above but for the numbers <code>a, b, c</code> use the floating-point values <code>1.0, epsilon, -epsilon</code> respectively where epsilon is determined by its final value after the following:

```python
epsilon = 1.0
while 1.0 + epsilon != 1.0:
    epsilon = epsilon / 2.0
```

The above should ensure that <code>(a + b) + c != 1.0</code> whilst their Kahan sum does equal 1.0 . ''Only'' if this is not the case are you then free to find three values with this property of your own.

'''If your language has decimals, but not six digits of precision''' then you may follow the outline given in [[Kahan_summation#Python:_Arbitrary_precision_Decimal]] which uses ideas from the floating point case above.


;In general:
* Show all output on this page.
* If the floating point calculations are used then answers may depend on the hardware platform or compiler/interpreter tool-chain used for a language.
* ''Slight'' deviations from the task description should be explained and the the subsequent difference in output explained.
* All examples should have constants chosen to clearly show the benefit of Kahan summing!





## ALGOL 68

Defines and uses a 6 digit float decimal type to solve the main "1000.0 + pi + e" task.

```algol68
# Kahan summation - Algol 68 doesn't have a float decimal mode, however as    #
# only 6 digits of precision are required and assuming INT is 32 bits, we can #
# emulate this fairly easily                                                  #

# MODE DEC is a float decimal with 6 digits                                   #
# We normalise the mantissa so that the first digit is non-zero if            #
# the number is not 0                                                         #
# so e.g.  1 is represented as 100000 E0                                      #
#         10 is represented as 100000 E1                                      #
# etc.                                                                        #
# we only provide enough operations for the task                              #

MODE DEC = STRUCT( BOOL negative, INT mantissa, INT exponent );

# constructs a string representation of a DEC number                          #
OP TOSTRING = ( DEC number )STRING:
    IF mantissa OF number = 0
    THEN
        # number is zero #
        " 0.00000E +0"
    ELSE
        # non-zero number #
        STRING result = whole( mantissa OF number, 0 );
        ( IF negative OF number
          THEN
              # number is negative - show leading "-" sign #
              "-"
          ELSE
              # positive number - show leading space #
              " "
          FI
        + result[ 1 : 1 ]
        + "."
        + result[ 2 :   ]
        + "E"
        + whole( exponent OF number, 3 )
        )
    FI
;

# negates a DEC number                                                        #
OP - = ( DEC a )DEC: ( NOT negative OF a, mantissa OF a, exponent OF a );

# adds two DEC numbers                                                        #
OP + = ( DEC a, DEC b )DEC:
    IF exponent OF a < exponent OF b
    THEN
        # the left operand has a smaller exponent than the right,             #
        # reverse the operands                                                #
        b + a

    ELSE
        # the left operand's exponent is at least as big as the right's       #

        # removes a digit from a DEC number with rounding, the exponent is    #
        # unchanged                                                           #
        OP REDUCE = ( DEC a )DEC: ( negative OF a
                                  , ( ( mantissa OF a OVER 10 )
                                      + IF mantissa OF a MOD 10 > 4 THEN 1 ELSE 0 FI
                                    )
                                  , exponent OF a
                                  );
        # adds an extra digit to a DEC number, exponent is unchanged          #
        OP EXTEND = ( DEC a )DEC: ( negative OF a, mantissa OF a * 10, exponent OF a );
        # signs the mantissa                                                  #
        OP APPLYSIGN = ( DEC a )DEC:( FALSE, IF negative OF a THEN - mantissa OF a ELSE mantissa OF a FI, exponent OF a );
        # unsigns the mantissa and sets negative accordingly                  #
        OP EXTRACTSIGN = ( DEC a )DEC: ( mantissa OF a < 0, ABS ( mantissa OF a ), exponent OF a );

        # give each number an extra digit                                     #
        DEC result  := EXTEND a;
        DEC operand := EXTEND b;

        # scale the values so that they have the same exponent                #

        WHILE exponent OF operand < exponent OF result
        DO
            operand := REDUCE operand;
            exponent OF operand PLUSAB 1
        OD;

        # sign the mantissas                                                  #
        result  := APPLYSIGN result;
        operand := APPLYSIGN operand;

        # add                                                                 #
        mantissa OF result PLUSAB mantissa OF operand;

        # remove the extra digit, round and unsign the mantissa               #
        result := REDUCE EXTRACTSIGN result;

        # if the mantissa is < 100000 and not 0, increase the mantissa and    #
        # reduce the exponent so that the first digit is not 0                #
        IF mantissa OF result /= 0
        THEN
            # non-zero mantissa - scale if necessary                          #
            WHILE mantissa OF result < 100000
            DO
                mantissa OF result TIMESAB 10;
                exponent OF result MINUSAB  1
            OD
        FI;

        result

    FI # + #
;

# subtracts two DEC numbers                                                   #
OP - = ( DEC a, DEC b )DEC: a + ( - b );

# performs Kahan summation on an array of DEC numbers                         #
# The numbers must be ordered highest to lowest                               #
# Algorithm as per the Wikipaedia page, except we start with sum              #
# set to the first element of the list, not 0                                 #
OP KAHANSUM = ( []DEC a )DEC:
    BEGIN

        DEC sum := a[LWB a];
        DEC c   := ( FALSE, 0, 0 );

        FOR i FROM LWB a + 1 TO UPB a
        DO
            DEC y := a[i] - c;
            DEC t := sum + y;
            c     := ( t - sum ) - y;
            sum   := t
        OD;

        sum
    END # KAHANSUM #
;

# test the Kahan summation and 6 digit decimal floating point                 #
main:
(

    # construct DEC values a = 10000.0, b = 3.14159, c = 2.71828              #

    DEC a := ( FALSE, 100000, 4 );
    DEC b := ( FALSE, 314159, 0 );
    DEC c := ( FALSE, 271828, 0 );

    # simple sum #
    print( ( ( "Simple: " + TOSTRING a + " + " + TOSTRING b + " + " + TOSTRING c + " = " + TOSTRING ( ( a + b ) + c ) )
           , newline
           )
         );
    # Kahan #
    []DEC list = ( a, b, c );
    print( ( ( "Kahan : " + TOSTRING a + " + " + TOSTRING b + " + " + TOSTRING c + " = " + TOSTRING KAHANSUM list )
           , newline
           )
         );

    SKIP
)

```

```txt
Simple:  1.00000E +4 +  3.14159E +0 +  2.71828E +0 =  1.00058E +4
Kahan :  1.00000E +4 +  3.14159E +0 +  2.71828E +0 =  1.00059E +4
```


## AWK


```AWK

# syntax: GAWK -f KAHAN_SUMMATION.AWK
# converted from C
BEGIN {
    epsilon = 1
    while (1 + epsilon != 1) {
      epsilon /= 2
    }
    arr[1] = a = 1.0
    arr[2] = b = epsilon
    arr[3] = c = -b
    printf("Epsilon   = %18.16g\n",b)
    printf("(a+b)+c   = %18.16f\n",(a+b)+c)
    printf("Kahan sum = %18.16f\n",kahan_sum(arr))
    exit(0)
}
function kahan_sum(nums,  c,i,sum,t,y) {
    for (i=1; i<=length(nums); i++) {
      y = nums[i] - c
      t = sum + y
      c = (t - sum) - y
      sum = t
    }
    return(sum)
}

```

```txt

Epsilon   = 1.110223024625157e-016
(a+b)+c   = 0.9999999999999999
Kahan sum = 1.0000000000000000

```



## C

```c
#include <stdio.h>
#include <stdlib.h>

float epsilon() {
    float eps = 1.0f;
    while (1.0f + eps != 1.0f) eps /= 2.0f;
    return eps;
}

float kahanSum(float *nums, int count) {
    float sum = 0.0f;
    float c = 0.0f;
    float t, y;
    int i;
    for (i = 0; i < count; ++i) {
        y = nums[i] - c;
        t = sum + y;
        c = (t - sum) - y;
        sum = t;
    }
    return sum;
}

int main() {
    float a = 1.0f;
    float b = epsilon();
    float c = -b;
    float fa[3];

    fa[0] = a;
    fa[1] = b;
    fa[2] = c;

    printf("Epsilon     = %0.12f\n", b);
    printf("(a + b) + c = %0.12f\n", (a + b) + c);
    printf("Kahan sum   = %0.12f\n", kahanSum(fa, 3));

    return 0;
}
```

```txt
Epsilon     = 0.000000059605
(a + b) + c = 0.999999940395
Kahan sum   = 1.000000000000
```



## C++


```cpp
#include <iostream>

float epsilon() {
    float eps = 1.0f;
    while (1.0f + eps != 1.0f) eps /= 2.0f;
    return eps;
}

float kahanSum(std::initializer_list<float> nums) {
    float sum = 0.0f;
    float c = 0.0f;
    for (auto num : nums) {
        float y = num - c;
        float t = sum + y;
        c = (t - sum) - y;
        sum = t;
    }
    return sum;
}

int main() {
    using namespace std;

    float a = 1.f;
    float b = epsilon();
    float c = -b;

    cout << "Epsilon      = " << b << endl;
    cout << "(a + b) + c  = " << (a + b) + c << endl;
    cout << "Kahan sum    = " << kahanSum({ a, b, c }) << endl;

    return 0;
}
```

```txt
Epsilon      = 5.96046e-08
(a + b) + c  = 1
Kahan sum    = 1
```


## C#

```c#
using System;

namespace KahanSummation {
    class Program {
        static float KahanSum(params float[] fa) {
            float sum = 0.0f;
            float c = 0.0f;
            foreach (float f in fa) {
                float y = f - c;
                float t = sum + y;
                c = (t - sum) - y;
                sum = t;
            }

            return sum;
        }

        static float Epsilon() {
            float eps = 1.0f;
            while (1.0f + eps != 1.0f) eps /= 2.0f;
            return eps;
        }

        static void Main(string[] args) {
            float a = 1.0f;
            float b = Epsilon();
            float c = -b;
            Console.WriteLine("Epsilon      = {0}", b);
            Console.WriteLine("(a + b) + c  = {0}", (a + b) + c);
            Console.WriteLine("Kahan sum    = {0}", KahanSum(a, b, c));
        }
    }
}
```

```txt
Epsilon      = 1.110223E-16
(a + b) + c  = 1
Kahan sum    = 1
```



## D


```d
import std.stdio;

float kahanSum(float[] fa...) {
    float sum = 0.0;
    float c = 0.0;
    foreach (f; fa) {
        float y = f - c;
        float t = sum + y;
        c = (t - sum) - y;
        sum = t;
    }
    return sum;
}

void main() {
    float a = 1.0;
    float b = float.epsilon;
    float c = -b;
    writefln("Epsilon      = %0.8e", b);
    writefln("(a + b) + c  = %0.8f", ((a + b) + c));
    writefln("Kahan sum    = %0.8f", kahanSum(a, b, c));
}
```

```txt
Epsilon      = 1.19209290e-07
(a + b) + c  = 1.00000000
Kahan sum    = 1.00000000
```



## EchoLisp

No fixed point addition. EchoLisp  floating point numbers are always stored as double precision floating point numbers, following the international IEEE 754 standard.

```scheme
;; using floating point arithmetic
;; the maximum number of decimals is 17. Floating point arithmetic is not always 100% accurate
;; even with simple data :

(+ 0.2 0.1 0.3)  → 0.6000000000000001 ;; (1)

;; 👀 Will Kahan method do better ??
;; Kahan procedure :

(define  ( K+ nums ( sum 0.0) (c 0.0) (y) (t))
	(while (!null? nums)
		(set! y (- (first nums) c))
		(set! t (+ sum y))
		(set! c (- (- t sum) y))
		(set! sum t)
		(set! nums (rest nums)))
		sum)

;; 👏 👏 Kahan improves on the above (1)
(K+ '( 0.2 0.1 0.3))  → 0.6

;; using epsilon such as (1.0 + epsilon) = 1.0
;; compute 1 + epsilon - epsilon :

(define eps 1.0)
     (while (!= (+ 1.0 eps) 1.0) (set! eps (// eps 2.0)))
eps → 1.1102230246251565e-16

(define a  1.0 )
(define b eps)
(define c (- b))

(writeln 'standard-add= (+ a b c))
(writeln 'Kahan-add= (K+ (list a b c)))

    → standard-add= 0.9999999999999999
    → Kahan-add= 1

;;NB. The 17-nth decimal we gain using Kahan method will probably be lost in subsequent calculations.
;;Kahan algorithm will be useful for fixed-point decimal calculations, which EchoLisp does not have.
```


=={{header|F sharp|F#}}==
Solving the alternative task with an recursive algorithm and IEEE 754 double precision datatype.

```fsharp
let ε =
    let rec δ λ =
        match λ+1.0 with
        | 1.0 -> λ
        | _ -> δ(λ/2.0)
    δ(1.0)

let Σ numbers =
    let rec Σ numbers σ c =
        match numbers with
        | [] -> σ
        | number :: rest ->
            let y = number + c
            let t = σ + y
            let z = ((t - σ) - y)
            Σ rest t z
    Σ numbers 0.0 0.0

[<EntryPoint>]
let main argv =
    let (a, b, c) = ( 1.0, ε, -ε )
    let input = [ a; b; c ]
    printfn "ε:     %e" ε
    printfn "Sum:   %e" (input |> List.sum)
    printfn "Kahan: %e" (input |> Σ)
    0
```

```txt
ε:     1.110223e-016
Sum:   1.000000e+000
Kahan: 1.000000e+000
```



## Fortran


### When the computer works in decimal

As the person who provided the worked example for the Wikipædia article, I am hoist by my own petard! So, consider the IBM1620, a decimal computer, where the user could specify the number of digits to be used in arithmetic: it shall be six decimal digits instead of the default eight. The following source file is Second Fortran (of 1958). Variables were not usually declared (there are no INTEGER or REAL statements), so those whose name starts with I,...,N were integers (known as "fixed-point"), otherwise floating-point. Array bounds were not checked, and a subprogram receiving an array as a parameter needed to know merely that it was an array with the bound specified for one-dimensional arrays unimportant. Some compilers produce code that always executes a DO-loop once (because their check is at the end of the loop) so that zero-loops could not be relied on until after F77, thus S is ''not'' initialised to the first array element and the loop started with the second element, just in case N = 1.

Standard output was via the console typewriter (a sturdy device), via a TYPE statement. Text literals were available only via the "H"-format code, which consisted of a count before the H, followed by ''exactly'' that number of characters, or else... A lot of time and patience attended miscounts. All text is of course in capitals only. Indentation is a latter-day affectation: many decks of cards were prepared with minimal spacing.

```Fortran
      FUNCTION SUMC(A,N)
COMPENSATED SUMMATION. C WILL NOT STAY ZERO, DESPITE MATHEMATICS.
       DIMENSION A(12345)
        S = 0.0
        C = 0.0
        DO 1 I = 1,N
          Y = A(I) - C
          T = S + Y
          C = (T - S) - Y
          S = T
    1   CONTINUE
        SUMC = S
      END
      DIMENSION A(3)
      A(1) = 10000.0
      A(2) = 3.14159
      A(3) = 2.71828
      TYPE 1, A(1) + A(2) + A(3)
      TYPE 1, SUMC(A,3)
    1 FORMAT (6HSUM = ,F12.1)
      END

```

Alas, I no longer have access to an IBM1620 (or an emulator) whereby to elicit output. Despite being a language style over half a century old, exactly this source is acceptable to a F90/95 compiler, but it on current computers will use binary arithmetic and a precision not meeting the specification. Fortran as a language does not have a "decimal" type (as say in Cobol or Pl/i) but instead the compiler uses the arithmetic convenient for the cpu it is producing code for. This may be in base ten as with the IBM1620 and some others, or base two, or base eight, or base sixteen - or even base three on some Russian computers. Similarly, the system's standard word size may be 16, 18 (PDP 15), 32, 36, 48 (B6700), 64, or even 128 bits. Nor is that the end of the variations. The B6700 worked in base eight which meant that a floating-point number occupied 4'''7''' bits. The 48'th bit was unused in arithmetic, including that of integers. Since a CHARACTER type was unavailable, text was placed up to six eight-bit (EBCDIC) codes to a word, and characters differing in the value of the topmost bit were indistinguishable. A special operator .IS. was introduced to supplement .EQ.

All of this means that number size specifications such as REAL*8 are not as straightforward as one might imagine since a word size is not necessarily a multiple of eight bits, which is why REAL and DOUBLE are preferred over REAL*4 and REAL*8, but even so, a computation that worked on one computer may still behave oddly on another. And, even on binary computers where only simple sizes such as 32 or 64 bit numbers are offered there are still problems because with floating-point there is the question of assumed-one leading-bit for normalised numbers. The IBM pc and its followers uses this for 32- and 64-bit floating-point, but ''not'' for 80-bit floating point. And is it truncation or rounding, and what style of rounding at that?


### Second phase: decimal precision via binary floating point

Decimal fractions are nearly always recurring sequences in binary (i.e. except for powers of two such as three eights, etc) so at once there are approximations and these may combine unhelpfully. A value such as 3.14159 uses up nearly all of the precision of a 32-bit binary floating-point number, and is not represented exactly. This applies in turn to the various intermediate results as the summation proceeds, and one might be unlucky with the demonstration. The TRUNC6 approach alas doesn't work, as both summations come out as 10005·8, but with ROUND6, the desired results are presented...

```Fortran
      REAL FUNCTION TRUNC6(X) 	!Truncate X to six-digit decimal precision.
       REAL X	 !The number.
       REAL A	 !Its base ten log.
       REAL P	 !Its fractional part.
       REAL T	 !A scratchpad.
       INTEGER I,IP 	!For playing with powers of ten.
        IF (ISNAN(X)) THEN	!Avoid complaints from LOG10.
          TRUNC6 = X		!I'm not bothering with infinity.
        ELSE IF (X.EQ.0) THEN	!But a zero is quite possible.
          TRUNC6 = 0		!And provokes LOG10.
        ELSE		!But, this is the expectation.
          A = ABS(X)		!Simplify.
          A = LOG10(A)		!Convert to base ten, whatever X is in.
          IP = A		!Get the integer part of the log.
          P = A - IP		!Remove some power of ten.
          IF (P.LT.0) THEN	!Values of X less than one have a negative log.
            IP = IP - 1		!But I want the log table style.
            P = P + 1		!With positive fractional part.
          END IF		!X = 10**(IP + P), where P is in [0,1).
          I = 10**(P + 5)	!10**(5.30103) = 200000.00stuff. Six significant digits.
          IP = IP - 5  		!The countervailing power of ten.
          IF (IP.LT.0) THEN	!If less than zero, divide by a fp number to avoid integer division.
            T = I/10.0D0**(-IP)	!Remember that 0·1 and associates are approximations in binary.
          ELSE IF (IP.GT.0) THEN!But if greater than zero,
            T = I*10**IP	!Positive powers of ten are integers and exact.
          ELSE			!And if the power were zero, no worries.
            T = I		!Just take the integer.
          END IF		!So much for shifting and counter-shifting.
          TRUNC6 = SIGN(T,X)	!Return the original sign.
        END IF		 !That was a struggle.
      END	 !The result is still a binary floating-point number.

      REAL FUNCTION ROUND6(X)	!Rely on the formatting system.
       REAL X			!The number.
       CHARACTER*16 TEXT	 !Sufficient for "-0.123456E+666".
        WRITE (TEXT,1) X	 !Out it goes.
    1   FORMAT (E16.6)		 !With rounding to six significant digits.
        READ (TEXT,*) ROUND6	 !Get it back.
      END			 !All the hard work is done by the format routines.

      REAL FUNCTION SUMC6(A,N)	!Add elements of the array, using limited precision.
Compensated summation. C will not stay zero, despite mathematics.
       REAL A(N)	 !The array. Presumably with at least one element.
       INTEGER N	 !The number of elements.
       REAL S,C,Y,T	 !Assistants.
       INTEGER I	 !A stepper.
        S = A(1)	 !Start with the first element.
        C = 0.0		 !No previous omissions to carry forward.
        DO I = 2,N	!Step through the remainder of the array.
          Y = ROUND6(A(I) - C)		!Combine the next value with the compensation.
          T = ROUND6(S + Y)		!Augment the sum in T.
          C = ROUND6(ROUND6(T - S) - Y)	!Catch what part of Y didn't get added to T.
          S = T				!Place the sum.
        END DO		!On to the next element.
        SUMC6 = S	!C will no longer be zero.
      END		!Using a working mean might help.

      REAL A(3)
      A(1) = 10000.0
      A(2) = 3.14159
      A(3) = 2.71828
      WRITE (6,1) "S",ROUND6(ROUND6(A(1) + A(2)) + A(3))
      WRITE (6,1) "C",SUMC6(A,3)
    1 FORMAT (A1,"Sum = ",F12.1)
      END

```

Output is

```txt
SSum =      10005.8
CSum =      10005.9
```

Which may be more by good luck than by good management. If the ROUND6 attempt to restrict the precision to something like six decimal digits worth is abandoned and the calculations are preformed with ordinary binary floating-point arithmetic (as per the first source file), then both sums come out as 10005·9. This too may be more by good luck.

The code is not special, and should be acceptable to F77 onwards. DO-statements now may execute zero times, variables can be declared with types, generic function names are recognised, and in-line comments can be strewn about.

The conversion to a six-digit decimal style of floating-point arithmetic shows a side-effect of the base on precision. A normalised mantissa has the range [100000,999999] (in the above scheme, the fractional point is after the mantissa: more usual is to have it at the start, either as 0.100000 or 1.00000) and evidently, the resolution is one step in the lowest place. Thus the relative precision differs by a factor of ten over the possible range of values. With binary, it would differ only by a factor of two, but in base sixteen, by a factor of sixteen. In this regard, small bases are better. The ad-hoc calculation also suffers when trimming the precision of values such as 999999, returning 999998 because the logarithim is very nearly six. To obtain the required imprecision precisely, more precise arithmetic would be helpful! If the truncation to an integer is made into a rounding (as in <code>I = 10**(P + 5) + 0.5</code>) there would still be difficulty, far more easily avoided by the method of ROUND6, which doesn't have to worry about strange values such as zero either.

Alternatively, devise a proper scheme for decimal arithmetic.


### Third phase: what is being computed?

This involves inspecting some machine code, as produced on an AMD FX 6300 six-core cpu by the Compaq "Visual" Fortran compiler, version 6.6 for F90/95 on a Windows XP system. No optimisation activities were requested from the compiler.

The expression <code>S = A(1) + A(2) + A(3)</code> was rendered as four operation codes in a row: FLD, FADD, FADD, FSTP - omitting the addressing details. This means that the arithmetic was conducted in the cpu's floating-point unit using eighty-bit floating-point arithmetic (or, REAL*10), however the FSTP that stored the result to S, stored to a 32-bit recipient so that the result was in single precision and would have been rounded to fit.

If instead a simple DO-loop is used to calculate the sum, each iteration involved a FLD, FADD, FSTP. This means that at each step, the value of S (single precision) is loaded into the 80-bit register, a (single precision) value is added using 80-bit arithmetic, then the result is stored back to the single-precision variable, S. The third option is to use the supplied facility, <code>S = SUM(A)</code>, but alas, this accumulates the result in the same stepwise manner, using a temporary storage area of the same type as the array: single precision. It makes no attempt to recognise that the summation could be accumulated in a register, still less to do so with 80-bit precision. Similarly, the compiler made no attempt to recognise that on completion of one expression, the next expression used the result of the previous expression. Thus, in the sequence <code>T:=S + Y; C:=(T - S) - Y;</code> the value of T was reloaded for the second statement, even though the previous statement's last step had been to save a result to T. Some compilers and some languages do allow for this to be recognised (possibly as an option) but on computers where the working register has a different precision from that of a variable in storage, it will not be just the speed of the calculation that changes. Such changes will almost surely wreck the workings of compensated summation! Fortran has strict rules on the order of evaluation, and there is a long history of needing care. For instance, to compute m/h² where m is the mass of an electron and h is Planck's constant, h² risks exponent overflow so (m/h)/h would be safer.

For the test data, all methods, including compensated summation, returned exactly the same result, even to the last bit, so one wonders just what is going on. Using decimal numbers when concerned with the exact details of binary arithmetic is troublesome because the conversions are generally inexact, so the plan is to reveal the numbers in base two. Besides F format, later Fortran offers further options such as B O, and Z formats for binary, octal and hexadecimal (the code H being spoken for another use) however these present the bit-patterns as packed into the storage used for the variable, they do not reveal its ''numerical'' value in binary, etc. So, for 3.14159 in decimal,
 100000001 0010010000111111010000 is what is shown with B-format (though not the space inserted here)
        11.001001000011111101           is that value in binary (three, plus an eighth, plus a sixty-forth, plus ...)
        11.0010010000111111011011       is 4*ATAN(1) in single precision.
        11.001001000011111101101010100010001000010110100011  is 4*ATAN(1) in double precision.
        11.001001000011111101101010100010001000010110100011000010001101001100010... is more accurate.

It is a small relief to see that 4*ATAN(1) gives a correctly-rounded value - at least with this system. Specifying a decimal value, even if with additional digits, may not yield the best result in binary. To find out exactly what binary values are being used, one needs a scheme as follows:

```Fortran
      MODULE PROBE
      CONTAINS
      CHARACTER*76 FUNCTION FP8DIGITS(X,BASE,W)	!Full expansion of the value of X in BASE.
Converts a number X to a specified BASE. For integers, successive division by BASE, for fractions, successive multiplication.
Can't use the FORMAT system in case this function is invoked in a formatted WRITE: some furrytrans don't handle reentrancy.
       REAL*8 X,T		!The value, and an associate.
       INTEGER BASE		!As desired.
       INTEGER W		!Allowance for the integer part, to promote alignment.
       CHARACTER*(76) TEXT	!Scratchpad for results.
       INTEGER L		!The length of the result.
       INTEGER N,ND		!Counters.
       INTEGER D		!The digit of the moment.
       LOGICAL NEG		!Annoyance with signs.
       CHARACTER*1 DIGIT(0:15)	!Want more than just decimal.
       PARAMETER (DIGIT = (/"0","1","2","3","4","5","6","7","8","9",	!So no CHAR(ICHAR("0") + d) tricks.
     1  "A","B","C","D","E","F"/))	!Because these are not adjacent character codes.
        IF (BASE.LE.1 .OR. BASE.GT.16) BASE = 10	!Preclude oddities.
        TEXT = "Base"		!Scrub the TEXT with an announcement.
        IF (BASE.GE.10) TEXT (6:6) = "1"	!I'm playing with a restricted range only.
        TEXT (7:7) = DIGIT(MOD(BASE,10))	!So in-line code will do.
        TEXT(8:8) = ":"		!So much for the preamble.
        T = X			!Grab the value.
        N = T			!Its integer part, with truncation.
        T = ABS(T - N)		!Thus obtain the fractional part.
        NEG = X .LT. 0		!Negative numbers are a nuisance.
        IF (NEG) N = -N		!So simplify for what follows.
        L = 10 + W		!Finger the units position..
        ND = 0			!No digits have been rolled.
Crunch the integer part.
   10   D = MOD(N,BASE)		!Extract the low-order digit in BASE.
        TEXT(L:L) = DIGIT(D)	!Place it as text.
        ND = ND + 1		!Count another digit rolled.
        N = N/BASE		!Drop down a power.
        L = L - 1		!Move back correspondingly.
        IF (L.LT.10) THEN	!Run out of space?
          TEXT(9:9) = "!"	!"Overflow!"
          GO TO 900		!TEXT might be far too short.
        END IF			!But, space is expected.
        IF (N.GT.0) GO TO 10	!Are we there yet?
        IF (NEG) TEXT(L:L) = "-"!Yes! Is a negative sign needed?
        L = 10 + W + 1		!Finger what follows the units position.
        TEXT(L:L) = "."		!Laziness leads to a full stop for a decimal point.
Crunch through the fractional part until nothing remains.
        DO WHILE(T.GT.0)	!Eventually, this will be zero.
          IF (L.GE.LEN(TEXT)) THEN	!Provided I have enough space!
            L = LEN(TEXT)		!If not, use the whole supply.
            TEXT(L:L) = "~"		!Place a marker suggesting that more should follow.
            GO TO 900			!And give up.
          END IF		!Otherwise, a digit is to be found.
          T = T*BASE		!Shift up a power.
          N = T			!The integer part is the digit.
          T = T - N		!Remove that integer part from T.
          L = L + 1		!Advance the finger.
          TEXT(L:L) = DIGIT(N)	!Place the digit.
          ND = ND + 1		!Count it also.
        END DO		!And see if anything remains.
Cast forth an addendum, to save the reader from mumbling while counting long strings of digits.
        IF (LEN(TEXT) - L .GT. 11) THEN	!Err, is there space for an addendum?
          TEXT(L + 2:L + 8) = "Digits:"	!Yes! Reveal the number of digits.
          L = L + 10			!Advance to the tens-to-be location.
          IF (ND.GT.9) TEXT(L:L) = DIGIT(ND/10)	!I expect no more than two-digit digit counts.
          L = L + 1			!Finger the units position.
          TEXT(L:L) = DIGIT(MOD(ND,10))	!Thus, no use of the FORMAT system.
        END IF				!So m uch for the addendum.
  900   FP8DIGITS = TEXT	!Anyway, here it all is.
      END FUNCTION FP8DIGITS	!Bases play best with related bases, such as 4 and 8. Less so with (say) 3 and 7...

      REAL FUNCTION SUMC(A,N)	!Add elements of the array, using limited precision.
Compensated summation. C will not stay zero, despite mathematics.
       REAL A(N)	!The array. Presumably with at least one element. Horror if N is wrongly supplied!
       INTEGER N	!The number of elements.
       REAL S,C,Y,T	!Assistants.
       INTEGER I	!A stepper.
        IF (N.LT.1) STOP "Yes, we have no bananas."	!This shouldn't happen.
        S = A(1)	!Start with the first element.
        C = 0.0		!No previous omissions to carry forward.
        DO I = 2,N	!Step through the remainder of the array.
          WRITE (6,*) "i=",I
          WRITE (6,666) "A(i)", FP8DIGITS(DBLE(A(I)),2,14)
  666     FORMAT (A8,":",A)
          WRITE (6,666) "C",    FP8DIGITS(DBLE(C),2,14)
          Y = A(I) - C		!Combine the next value with the compensation.
          WRITE (6,666) "Y=A-C",FP8DIGITS(DBLE(Y),2,14)
          WRITE (6,666) "S",    FP8DIGITS(DBLE(S),2,14)
          T = S + Y		!Augment the sum, temporarily in T.
          WRITE (6,666) "T=S+Y",FP8DIGITS(DBLE(T),2,14)
          WRITE (6,666) "T-S",  FP8DIGITS(DBLE(T - S),2,14)
          C = (T - S) - Y	!Catch what part of Y didn't get added to T.
          WRITE (6,666) "C=T-S-Y",FP8DIGITS(DBLE(C),2,14)
          S = T			!Place the sum.
          WRITE (6,666) "S",FP8DIGITS(DBLE(S),2,14)
        END DO		!On to the next element.
        SUMC = S	!C will no longer be zero.
      END FUNCTION SUMC	!Using a working mean might help.
      END MODULE PROBE

      USE PROBE
      REAL A(3)
      REAL*4 X4E,X4L,X4S,X4C
      REAL*8 X8E
      INTEGER I
      INTEGER BASE,W
      BASE = 2
      W = 14
      A(1) = 10000.0
      A(2) = 3.14159
      A(3) = 2.71828

      WRITE (6,*) "Sum via the additions in one expression."
      X4E = A(1) + A(2) + A(3)	!Calc. in R10, saved to R4.
      WRITE (6,1) "4",X4E
      WRITE (6,666) "1exprn",FP8DIGITS(DBLE(X4E),BASE,W)

      WRITE (6,*) "Sum via a loop."
      X4L = 0
      DO I = 1,3
        X4L = X4L + A(I)	!Terms in R10, saved to R4.
        WRITE (6,666) "A(i)",FP8DIGITS(DBLE(A(I)),BASE,W)
        WRITE (6,666) "X4L",FP8DIGITS(DBLE(X4L),BASE,W)
      END DO
      WRITE (6,1) "L",X4L
      WRITE (6,666) "Loop",FP8DIGITS(DBLE(X4L),BASE,W)

      WRITE (6,*) "Sum via SUM(A)"
      X4S = SUM(A)
      WRITE (6,1) "s",X4S
      WRITE (6,666) "SUM(A)",FP8DIGITS(DBLE(X4S),BASE,W)
      X8E = A(1) + A(2) + A(3)	!Calc in R10, saved to R8.
      WRITE (6,*) "X4S - X4L",X4S - X4L
      WRITE (6,*) "X4E - X8E=",X4E - X8E
      WRITE (6,1) "8",X8E
      WRITE (6,666) "1exprn*8",FP8DIGITS(X8E,BASE,W)

      WRITE (6,*) "Sum via SUMC"
      X4C = SUMC(A,3)
      WRITE (6,1) "C",X4C
      WRITE (6,666) "SUMC",FP8DIGITS(DBLE(X4C),BASE,W)

      WRITE (6,666) "Dec. calc",FP8DIGITS(10005.85987D0,BASE,W)
      WRITE (6,666) "Pie",FP8DIGITS(10000D0+4*ATAN(1D0)+EXP(1D0),BASE,W)
      WRITE (6,*) "The array..."
      DO I = 1,3
        WRITE (6,*) FP8DIGITS(DBLE(A(I)),BASE,W)
      END DO
      WRITE (6,"('3.14159 bits, packed',B32)") A(2)
    1 FORMAT (A1,"Sum = ",F12.1)
  666 FORMAT (A8,":",A)
      END
```

Because of the use of a function returning a CHARACTER value, and an array with a lower bound of zero, F90 features are needed, and to reduce the complaints over non-declaration of types of invoked functions, the MODULE protocol was convenient. Startlingly, if function SUMC was invoked with the wrong value of N (after removing a fourth element, Euler's constant, 0·5772156649015..., but forgetting to change the 4 back to 3) there was no complaint from the array bound-checking, and a fourth element was accessed! (It was zero) Changing the declaration from the old-style of A(N) to A(:) revived the bound checking, but at the loss of a documentation hint.

The output is rather messy, so after some slight editing,

```txt
 Sum via the additions in one expression.
4Sum =      10005.9
  1exprn:  10011100010101.1101110001
 Sum via a loop.
    A(i):  10011100010000.
     X4L:  10011100010000.
    A(i):              11.001001000011111101
     X4L:  10011100010011.0010010001
    A(i):              10.1011011111100001001101
     X4L:  10011100010101.1101110001
LSum =      10005.9
    Loop:  10011100010101.1101110001
 Sum via SUM(A)
sSum =      10005.9
  SUM(A):  10011100010101.1101110001
 X4S - X4L  0.0000000E+00
 X4E - X8E=  4.813671112060547E-004
8Sum =      10005.9
1exprn*8:  10011100010101.1101110000100000011101
 Sum via SUMC
 i=           2
    A(i):              11.001001000011111101
       C:               0.
   Y=A-C:              11.001001000011111101
       S:  10011100010000.
   T=S+Y:  10011100010011.0010010001
     T-S:              11.0010010001
 C=T-S-Y:               0.000000000000000011
       S:  10011100010011.0010010001
 i=           3
    A(i):              10.1011011111100001001101
       C:               0.000000000000000011
   Y=A-C:              10.1011011111100000011101
       S:  10011100010011.0010010001
   T=S+Y:  10011100010101.1101110001
     T-S:              10.10111
 C=T-S-Y:               0.0000000000011111100011
       S:  10011100010101.1101110001
CSum =      10005.9
    SUMC:  10011100010101.1101110001
Dec. cal:  10011100010101.110111000010000001110000101110001101
     Pie:  10011100010101.110111000010000010111011111010110001
 The array...
   10011100010000.
               11.001001000011111101
               10.1011011111100001001101
3.14159 bits, packed 1000000010010010000111111010000
```

The key point is that twenty-four bits are employed for the mantissa of a single-precision floating-point number, and this is using the implicit leading-one bit of normalised numbers - that bit is not stored so that 23 bits of storage deliver 24 bits... When 10,000 is represented in binary that requires fourteen bits so a further ten bits below the fractional point are yet available. So, placing a | after the tenth fractional bit,
 3.14159:  11.0010010000 | 11111101
 2.71828: +10.1011011111 | 100001001101
        = 101.1101101111 |1100000011101  There should be a carry from the unseen lower bits...
        = 101.1101110000 | 100000011101  This should be the result in the lower bits, the carry made.
          101.1101110001   ^             This is the calculated result in the lower bits.
Because ''both'' values round up one (because the eleventh bit is a 1) in the actual calculation, the result is one bit high. But this is entirely proper, because the eleventh bit of the sum, by sheer chance, is a 1 (as seen above: below it is a ^), and so the plain summation is as accurate as it can be and the compensated addition can't do any better despite all its juggling. The double-precision result (from the 80-bit calculation) confirms this as the difference X4E - X8E of 4.813671112060547D-004 is 0.0000000000011111100011 in binary or | 011111100011, exactly the last value of C. It is calculated back-to-front compared to the hand calculation (above the ^) where the discrepancy is | 100000011101: subtract one from the other and zero results, except for the last bit.

===Fourth phase - try some alternative test data===
The values that demonstrated a benefit for six-digit decimal arithmetic show no gain in binary, but perhaps other values will do so.

F90 offers interesting functions related to the floating-point number scheme, such as FRACTION(x) which returns the mantissa that for binary floating point will be in the range [0·5,1) so FRACTION(3·0) = 0·75, and EXPONENT(x), which will be for powers of two in a binary scheme. RADIX(x) will reveal the base and DIGITS(x) the number of digits of precision, while EPSILON(x) give the smallest value that can be distinguished from one. In these functions the parameter's value is irrelevant, it is present only to select between single and double precision, or perhaps quadruple precision. However, revealing their values in base ten is not entirely helpful, so using FP8DIGITS as before, proceed as follows:

```Fortran
      USE PROBE
      REAL A(3)
      REAL*4 X4E,X4L,X4S,X4C
      REAL*4 ONE,EPS
      REAL*8 X8E
      INTEGER I,N
      INTEGER BASE,W
Cast forth some pearls.
      WRITE (6,1) RADIX(X4E),DIGITS(X4E),EPSILON(X4E),
     1 RADIX(X8E),DIGITS(X8E),EPSILON(X8E)
    1 FORMAT ("Special functions report on the floating-point scheme.",
     1 /," Single precision: Radix=",I0,", Digits=",I0,", eps=",E14.7,
     2 /," Double precision: Radix=",I0,", Digits=",I0,", eps=",E14.7)
Concoct a heading for the table that will follow.
      WRITE (6,2) "eps","eps as stored"," as a number",
     1 " 1 + eps as stored","as a number"
    2 FORMAT ("Now to experiment with the computation scheme.",/,
     1 "Bit ",A14,2(A32,1X,A50))	!These sizes match those of FORMAT 11.
      N = 1		!In the beginning,
      EPS = 1		!There is one bit.
Consider the current precision.
   10 ONE = 1 + EPS	!Some numbers are more different than others.
      WRITE (6,11) N,EPS,EPS,FP8DIGITS(DBLE(EPS),2,2),
     1                   ONE,FP8DIGITS(DBLE(ONE),2,2)
   11 FORMAT (I3,":",1PE14.7,2(B32,1X,A50))
      IF (ONE .NE. 1) THEN	!Still see the difference?
        N = N + 1		!Yes. Count up another.
        EPS = EPS/2		!Go one smaller.
        GO TO 10		!And try again.
      END IF		!Mathematically, this will never end. But, with finite precision...
Compare with the results from the special function EPSILON.
      WRITE (6,*) "  ",EPS,"is the first eps indistinguishable from 1."
      WRITE (6,*) "  ",EPSILON(X4E),"reported smallest distinguishable."
      WRITE (6,*) "  ",2*EPS - EPSILON(EPS),"the difference."

Concoct some test values.
      A(1) = 1
      A(2) = +EPS
      A(3) = -EPS
Choose a revelation format.
      BASE = 2
      W = 2
Commence the tests.
      WRITE (6,*) "Sum via the additions in one expression."
      X4E = A(1) + A(2) + A(3)	!Calc. in R10, saved to R4.
      WRITE (6,665) "4",X4E
      WRITE (6,666) "1exprn",FP8DIGITS(DBLE(X4E),BASE,W)

      WRITE (6,*) "Sum via a loop."
      X4L = 0
      DO I = 1,3
        X4L = X4L + A(I)	!Terms in R10, saved to R4.
        WRITE (6,666) "A(i)",FP8DIGITS(DBLE(A(I)),BASE,W)
        WRITE (6,666) "X4L",FP8DIGITS(DBLE(X4L),BASE,W)
      END DO
      WRITE (6,665) "L",X4L
      WRITE (6,666) "Loop",FP8DIGITS(DBLE(X4L),BASE,W)

      WRITE (6,*) "Sum via SUM(A)"
      X4S = SUM(A)
      WRITE (6,665) "s",X4S
      WRITE (6,666) "SUM(A)",FP8DIGITS(DBLE(X4S),BASE,W)
      X8E = A(1) + A(2) + A(3)	!Calc in R10, saved to R8.
      WRITE (6,*) "X4E",X4E
      WRITE (6,*) "X4L",X4L
      WRITE (6,*) "X4S",X4S
      WRITE (6,*) "X4S - X4L",X4S - X4L
      WRITE (6,*) "X4E - X8E=",X4E - X8E
      WRITE (6,665) "8",X8E
      WRITE (6,666) "1exprn*8",FP8DIGITS(X8E,BASE,W)

      WRITE (6,*) "Sum via SUMC"
      X4C = SUMC(A,3)
      WRITE (6,665) "C",X4C
      WRITE (6,666) "SUMC",FP8DIGITS(DBLE(X4C),BASE,W)

      WRITE (6,*) "The array..."
      DO I = 1,3
        WRITE (6,*) FP8DIGITS(DBLE(A(I)),BASE,W)
      END DO
  665 FORMAT (A1,"Sum = ",F12.1)
  666 FORMAT (A8,":",A)
      END
```

Output, again with some minor editing. Because free-format output presents numbers in "scientific format" with a value in the units digit while "E"-type format does so with a zero units digit (or no units digit at all if cramped), the 1P prefix is used to cause a shift of one place and the exponent is adjusted accordingly. The same shift can be applied to "F"-type format but in that case there is no exponent to correct.

```txt

Special functions report on the floating-point scheme.
 Single precision: Radix=2, Digits=24, eps= 0.1192093E-06
 Double precision: Radix=2, Digits=53, eps= 0.2220446E-15
Now to experiment with the computation scheme.
Bit            eps                   eps as stored                                as a number               1 + eps as stored                                as a number
  1: 1.0000000E+00  111111100000000000000000000000    1. Digits:  1                           1000000000000000000000000000000   10. Digits:  2
  2: 5.0000000E-01  111111000000000000000000000000    0.1 Digits:  2                           111111110000000000000000000000    1.1 Digits:  2
  3: 2.5000000E-01  111110100000000000000000000000    0.01 Digits:  3                          111111101000000000000000000000    1.01 Digits:  3
  4: 1.2500000E-01  111110000000000000000000000000    0.001 Digits:  4                         111111100100000000000000000000    1.001 Digits:  4
  5: 6.2500000E-02  111101100000000000000000000000    0.0001 Digits:  5                        111111100010000000000000000000    1.0001 Digits:  5
  6: 3.1250000E-02  111101000000000000000000000000    0.00001 Digits:  6                       111111100001000000000000000000    1.00001 Digits:  6
  7: 1.5625000E-02  111100100000000000000000000000    0.000001 Digits:  7                      111111100000100000000000000000    1.000001 Digits:  7
  8: 7.8125000E-03  111100000000000000000000000000    0.0000001 Digits:  8                     111111100000010000000000000000    1.0000001 Digits:  8
  9: 3.9062500E-03  111011100000000000000000000000    0.00000001 Digits:  9                    111111100000001000000000000000    1.00000001 Digits:  9
 10: 1.9531250E-03  111011000000000000000000000000    0.000000001 Digits: 10                   111111100000000100000000000000    1.000000001 Digits: 10
 11: 9.7656250E-04  111010100000000000000000000000    0.0000000001 Digits: 11                  111111100000000010000000000000    1.0000000001 Digits: 11
 12: 4.8828125E-04  111010000000000000000000000000    0.00000000001 Digits: 12                 111111100000000001000000000000    1.00000000001 Digits: 12
 13: 2.4414063E-04  111001100000000000000000000000    0.000000000001 Digits: 13                111111100000000000100000000000    1.000000000001 Digits: 13
 14: 1.2207031E-04  111001000000000000000000000000    0.0000000000001 Digits: 14               111111100000000000010000000000    1.0000000000001 Digits: 14
 15: 6.1035156E-05  111000100000000000000000000000    0.00000000000001 Digits: 15              111111100000000000001000000000    1.00000000000001 Digits: 15
 16: 3.0517578E-05  111000000000000000000000000000    0.000000000000001 Digits: 16             111111100000000000000100000000    1.000000000000001 Digits: 16
 17: 1.5258789E-05  110111100000000000000000000000    0.0000000000000001 Digits: 17            111111100000000000000010000000    1.0000000000000001 Digits: 17
 18: 7.6293945E-06  110111000000000000000000000000    0.00000000000000001 Digits: 18           111111100000000000000001000000    1.00000000000000001 Digits: 18
 19: 3.8146973E-06  110110100000000000000000000000    0.000000000000000001 Digits: 19          111111100000000000000000100000    1.000000000000000001 Digits: 19
 20: 1.9073486E-06  110110000000000000000000000000    0.0000000000000000001 Digits: 20         111111100000000000000000010000    1.0000000000000000001 Digits: 20
 21: 9.5367432E-07  110101100000000000000000000000    0.00000000000000000001 Digits: 21        111111100000000000000000001000    1.00000000000000000001 Digits: 21
 22: 4.7683716E-07  110101000000000000000000000000    0.000000000000000000001 Digits: 22       111111100000000000000000000100    1.000000000000000000001 Digits: 22
 23: 2.3841858E-07  110100100000000000000000000000    0.0000000000000000000001 Digits: 23      111111100000000000000000000010    1.0000000000000000000001 Digits: 23
 24: 1.1920929E-07  110100000000000000000000000000    0.00000000000000000000001 Digits: 24     111111100000000000000000000001    1.00000000000000000000001 Digits: 24
 25: 5.9604645E-08  110011100000000000000000000000    0.000000000000000000000001 Digits: 25    111111100000000000000000000000    1. Digits:  1
     5.9604645E-08 is the first eps indistinguishable from 1.
     1.1920929E-07 reported smallest distinguishable.
     0.0000000E+00 the difference.
 Sum via the additions in one expression.
4Sum =          1.0
  1exprn:   1. Digits:  1
 Sum via a loop.
    A(i):   1. Digits:  1
     X4L:   1. Digits:  1
    A(i):   0.000000000000000000000001 Digits: 25
     X4L:   1. Digits:  1
    A(i):  -0.000000000000000000000001 Digits: 25
     X4L:   0.111111111111111111111111 Digits: 25
LSum =          1.0
    Loop:   0.111111111111111111111111 Digits: 25
 Sum via SUM(A)
sSum =          1.0
  SUM(A):   0.111111111111111111111111 Digits: 25
 X4E   1.000000
 X4L  0.9999999
 X4S  0.9999999
 X4S - X4L  0.0000000E+00
 X4E - X8E=  0.000000000000000E+000
8Sum =          1.0
1exprn*8:   1. Digits:  1
 Sum via SUMC
 i=           2
    A(i):               0.000000000000000000000001 Digits: 25
       C:               0. Digits:  1
   Y=A-C:               0.000000000000000000000001 Digits: 25
       S:               1. Digits:  1
   T=S+Y:               1. Digits:  1
     T-S:               0. Digits:  1
 C=T-S-Y:              -0.000000000000000000000001 Digits: 25
       S:               1. Digits:  1
 i=           3
    A(i):              -0.000000000000000000000001 Digits: 25
       C:              -0.000000000000000000000001 Digits: 25
   Y=A-C:               0. Digits:  1
       S:               1. Digits:  1
   T=S+Y:               1. Digits:  1
     T-S:               0. Digits:  1
 C=T-S-Y:               0. Digits:  1
       S:               1. Digits:  1
CSum =          1.0
    SUMC:   1. Digits:  1
 The array...
    1. Digits:  1
    0.000000000000000000000001 Digits: 25
   -0.000000000000000000000001 Digits: 25

```


Thus, the single-expression summation in performing the whole calculation in REAL*10 obtains the exact result, but the loopwise calculation does not, be it via an explicit loop or via the SUM function that uses a loop and no extra precision. Output formats with just one decimal digit of course round to one as the result, but the free-format outputs show the non-one result. Basically, adding eps to one has no effect, but, subtracting eps from one does - remember that the arithmetic is done in an eighty-bit floating-point register and then the result is converted to single precision. If the arithmetic were to be done in single precision, the result may well differ. Similarly, if the mantissa had a different precision (such as 23 bits as with no implicit-one) the results could well differ. On the other hand, substituting the best possible values for π and e will not make a difference because the action is around fractional bits ten and eleven, well within the correctness range of the decimal constants.

And this time, the compensated summation calculation comes out with one while the successive additions via a loop do not, as required.


## Go

Go has no floating point decimal type.  Its floating point types are float64 and float32, implementations of IEEE 754 binary64 and binary32 formats.  Alternative task:

```go
package main

import "fmt"

type float float64

func kahan(s ...float) float {
    var tot, c float
    for _, x := range s {
        y := x - c
        t := tot + y
        c = (t - tot) - y
        tot = t
    }
    return tot
}

func epsilon() float {
    ε := float(1)
    for 1+ε != 1 {
        ε /= 2
    }
    return ε
}

func main() {
    a := float(1)
    b := epsilon()
    c := -b
    fmt.Println("Left associative:", a+b+c)
    fmt.Println("Kahan summation: ", kahan(a, b, c))
    fmt.Println("Epsilon:         ", b)
}
```

```txt
Left associative: 0.9999999999999999
Kahan summation:  1
Epsilon:          1.1102230246251565e-16

```

With float defined as float32,

```txt

Left associative: 0.99999994
Kahan summation:  1
Epsilon:          5.9604645e-08
```



## J

J does not implement 6 digit floating point addition (though the programmer could of course emulate such a thing - if it were specified precisely enough). But J does have IEEE-754 floating point numbers. So...

Required code (for the current draft of this task):

```J
epsilon=:3 :0''
  epsilon=. 1.0
  while. 1.0 ~: 1.0 + epsilon do.
    epsilon=. epsilon % 2.0
  end.
)

a=: 1.0
b=: epsilon
c=: -epsilon

KahanSum=:3 :0
  input=. y
  c=. 0.0
  sum=. 0.0
  for_i. i.#input do.
    y=. (i{input) - c
    t=. sum + y
    c=. (t - sum) - y
    sum=. t
  end.
)
```


Required results:


```J
   (a+b)+c
1
   KahanSum a,b,c
1
```

There are several issues here, but the bottom line is that several assumptions behind the design of this task conflict with the design of J.

But let's take them one by one.

First, the value of epsilon:

```J
   epsilon
5.68434e_14
```

This is because J's concept of equality, in the context of floating point numbers, already includes an epsilon. Floating point numbers are inherently imprecise, and this task has assumed that the language has assumed that floating point numbers are precise. So let's implement something closer to this previously unstated assumption:

```J
epsilon=:3 :0''
  epsilon=. 1.0
  while. 1.0 (~:!.0) 1.0 + epsilon do.
    epsilon=. epsilon % 2.0
  end.
)
```

Now we have a smaller value for this explicit value of epsilon:

```J
   epsilon
1.11022e_16
```

With this new value for epsilon, let's try the problem again:


```J
   (a+b)+c
1
   KahanSum a,b,c
1
```

Oh dear, we still are not failing in the manner clearly specified as a task requirement.

Well, again, the problem is that the task has assumed that the language is treating floating point values as precise when their actual accuracy is less than their precision. This time, the problem is in the display of the result. So let's also override that mechanism and ask J to display results to 16 places of precision:


```J
   ":!.16 (a+b)+c
0.9999999999999999
   ":!.16 KahanSum a,b,c
1
```

Voila!
See also http://keiapl.info/anec/#fuzz

## Java

As is noted in the Kotlin implementation, the JVM does not provide the desired decimal type, so the alternate implementation is provided instead.

```Java
public class KahanSummation {
    private static float kahanSum(float... fa) {
        float sum = 0.0f;
        float c = 0.0f;
        for (float f : fa) {
            float y = f - c;
            float t = sum + y;
            c = (t - sum) - y;
            sum = t;
        }
        return sum;
    }

    private static float epsilon() {
        float eps = 1.0f;
        while (1.0f + eps != 1.0f) eps /= 2.0f;
        return eps;
    }

    public static void main(String[] args) {
        float a = 1.0f;
        float b = epsilon();
        float c = -b;
        System.out.println("Epsilon      = " + b);
        System.out.println("(a + b) + c  = " + ((a + b) + c));
        System.out.println("Kahan sum    = " + kahanSum(a, b, c));
    }
}
```

```txt
Epsilon      = 5.9604645E-8
(a + b) + c  = 0.99999994
Kahan sum    = 1.0
```



## Julia

Julia can use its BigFloat data type to avoid floating point epsilon errors, as shown below.

```julia
epsilon() = begin eps = 1.0; while 1.0 + eps != 1.0 eps /= 2.0 end; eps end

function kahansum(arr)
    tot = temp = 0.0
    for x in arr
        y = x - temp
        t = tot + y
        temp = (t - tot) - y
        tot = t
    end
    return tot
end

const a = 1.0
const ep = epsilon()
const b = -ep
const v = [a, ep, b]

println("Epsilon is $ep")
println("(a + ep) + -ep = ", (a + ep) + b)
println("Kahan sum is ", kahansum(v))
println("BigFloat sum is ", (BigFloat(a) + ep) + b)

```
```txt

Epsilon is 1.1102230246251565e-16
(a + ep) + -ep = 0.9999999999999999
Kahan sum is 1.0
BigFloat sum is 1.0

```



## Kotlin

Kotlin does not have a 6 digit decimal type. The closest we have to it is the 4-byte floating point type, Float, which has a precision of 6 to 9 significant decimal digits - about 7 on average. So performing the alternative task:

```scala
// version 1.1.2

fun kahanSum(vararg fa: Float): Float {
    var sum = 0.0f
    var c = 0.0f
    for (f in fa) {
        val y = f - c
        val t = sum + y
        c = (t - sum) - y
        sum = t
    }
    return sum
}

fun epsilon(): Float {
    var eps = 1.0f
    while (1.0f + eps != 1.0f) eps /= 2.0f
    return eps
}

fun main(args: Array<String>) {
    val a = 1.0f
    val b = epsilon()
    val c = -b
    println("Epsilon      = $b")
    println("(a + b) + c  = ${(a + b) + c}")
    println("Kahan sum    = ${kahanSum(a, b, c)}")
}
```

```txt
Epsilon      = 5.9604645E-8
(a + b) + c  = 0.99999994
Kahan sum    = 1.0
```



## Lua

```lua
function epsilon()
    local eps = 1.0
    while 1.0 + eps ~= 1.0 do
        eps = eps / 2.0
    end
    return eps
end

function kahanSum(nums)
    local sum = 0.0
    local c = 0.0
    for _,num in ipairs(nums) do
        local y = num - c
        local t = sum + y
        c = (t - sum) - y
        sum = t
    end
    return sum
end

-- main
local a = 1.0
local b = epsilon()
local c = -b
local fa = {a,b,c}

print(string.format("Epsilon     = %0.24f", b))
print(string.format("(a + b) + c = %0.24f", (a+b)+c))
print(string.format("Kahan sum   = %0.24f", kahanSum(fa)))
```

```txt
Epsilon     = 0.000000000000000111022302
(a + b) + c = 0.999999999999999890000000
Kahan sum   = 1.000000000000000000000000
```


=={{header|Modula-2}}==

```modula2
MODULE KahanSummation;
FROM RealStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteReal(num : REAL);
VAR
    buf : ARRAY[0..64] OF CHAR;
BEGIN
    RealToStr(num,buf);
    WriteString(buf);
END WriteReal;

PROCEDURE KahanSum(fa : ARRAY OF REAL) : REAL;
VAR
    sum,c,y,t : REAL;
    i : CARDINAL;
    buf : ARRAY[0..64] OF CHAR;
BEGIN
    sum := 0.0;
    c := 0.0;
    FOR i:=0 TO HIGH(fa) DO
        y := fa[i] - c;
        t := sum + y;
        c := (t - sum) - y;
        sum := t;
    END;
    RETURN sum;
END KahanSum;

PROCEDURE Epsilon() : REAL;
VAR
    eps : REAL;
BEGIN
    eps := 1.0;
    WHILE 1.0 + eps # 1.0 DO
        eps := eps / 2.0;
    END;
    RETURN eps;
END Epsilon;

VAR
    fa : ARRAY[0..2] OF REAL;
    a,b,c : REAL;
BEGIN
    a := 1.0;
    b := Epsilon();
    c := -b;
    WriteString("Epsilon      = ");
    WriteReal(b);
    WriteLn;
    WriteString("(a + b) + c  = ");
    WriteReal((a + b) + c);
    WriteLn;

    fa[0] := a;
    fa[1] := b;
    fa[2] := c;
    WriteString("Kahan sum    = ");
    WriteReal(KahanSum(fa));
    WriteLn;

    ReadChar;
END KahanSummation.
```

```txt
Epsilon      = 1.1102230E-16
(a + b) + c  = 1.00000000
Kahan sum    = 1.00000000
```



## PARI/GP

No decimals here; the example below uses 64-bit binary arithmetic.

This is a ''partial solution'' only; I haven't found appropriate values that fail for normal addition. (Experimentation should produce these, in which case the existing code should work with the inputs changed.)

```parigp
Kahan(v)=my(s=0.,c=0.,y,t);for(i=1,#v,y=v[i]-c;t=s+y;c=t-s-y;s=t);s;
epsilon()=my(e=1.); while(e+1. != 1., e>>=1); e;
\p 19
e=epsilon();
Kahan([1.,e,-e])
```

```txt
%1 = 1.000000000000000000
```



## Objeck

```objeck
class KahanSum {
   function : Main(args : String[]) ~ Nil {
      a := 1.0f;
      b := Epsilon();
      c := -b;

      abc := (a + b) + c;
      fa := Float->New[3]; fa[0] := a; fa[1] := b; fa[2] := c;
      sum := KahanSum(fa);

      "Epsilon      = {$b}"->PrintLine();
      "(a + b) + c  = {$abc}"->PrintLine();
      "Kahan sum    = {$sum}"->PrintLine();
   }

   function : KahanSum(fa : Float[]) ~ Float {
      sum := 0.0f;
      c := 0.0f;
      each(i : fa) {
         f := fa[i];
         y := f - c;
         t := sum + y;
         c := (t - sum) - y;
         sum := t;
      };

      return sum;
   }

   function : Epsilon() ~ Float {
      eps := 1.0f;
      while(1.0f + eps <> 1.0f) { eps /= 2.0f; };
      return eps;
   }
}
```


```txt

Epsilon      = 1.11022e-16
(a + b) + c  = 1
Kahan sum    = 1

```



## Perl

```perl
use strict;
use warnings;
use feature 'say';

sub kahan {
    my(@nums) = @_;
    my $summ = my $c = 0e0;
    for my $num (@nums) {
        my $y = $num - $c;
        my $t = $summ + $y;
        $c = ($t - $summ) - $y;
        $summ = $t;
    }
    $summ
}

my $eps = 1;
do { $eps /= 2 } until 1e0 == 1e0 + $eps;

say 'Epsilon:    ' . $eps;
say 'Simple sum: ' . sprintf "%.16f", ((1e0 + $eps) - $eps);
say 'Kahan sum:  ' . sprintf "%.16f", kahan(1e0, $eps, -$eps);
```

```txt
Epsilon:    1.11022302462516e-16
Simple sum: 0.9999999999999999
Kahan sum:  1.0000000000000000
```



## Perl 6

Perl 6 does not offer a fixed precision decimal. It ''does'' have IEEE 754 floating point numbers so let's try implementing the floating point option as shown in Python. Need to explicitly specify scientific notation numbers to force floating point Nums.


```perl6
constant ε = (1e0, */2e0 … *+1e0==1e0)[*-1];

sub kahan (*@nums) {
    my $summ = my $c = 0e0;
    for @nums -> $num {
        my $y = $num - $c;
        my $t = $summ + $y;
        $c = ($t - $summ) - $y;
        $summ = $t;
    }
    $summ
}

say 'Epsilon:    ', ε;

say 'Simple sum: ', ((1e0 + ε) - ε).fmt: "%.16f";

say 'Kahan sum:  ', kahan(1e0, ε, -ε).fmt: "%.16f";
```

```txt
Epsilon:    1.1102230246251565e-16
Simple sum: 0.9999999999999999
Kahan sum:  1.0000000000000000

```



## Phix

Phix does not have fixed precision decimals, just the usual IEEE 754 floating point numbers.

Using enforced rounding. From the manual: "For the %f format, precision specifies how many digits follow

the decimal point character, whereas for %g, the precision specifies how many significant digits to print",

hence we can use printf(%g), followed immediately by scanf(), to emulate limited precision decimals.

```Phix
function round6(atom a)
-- NB: only suitable for this very specific example
    string s = trim(sprintf("%6g",a))
    {{a}} = scanf(s,"%f")
    return a
end function

function roundsum(sequence s)
atom res = 0
    for i=1 to length(s) do
        res = round6(res+s[i])
    end for
    return res
end function

function kahanroundsum(sequence s)
    atom tot = 0, c = 0
    for i=1 to length(s) do
        atom x = s[i],
             y = round6(x-c),
             t = round6(tot+y)
        c = round6(round6(t-tot)-y)
        tot = t
    end for
    return tot
end function

procedure main()
    sequence s = {10000.0, 3.14159, 2.71828}
    string fmt = "%5.1f\n"
    printf(1,fmt,roundsum(s))
    printf(1,fmt,kahanroundsum(s))
end procedure
main()
```

```txt

10005.8
10005.9

```

Alternative task using floats and the explicitly calculated epsilon:

```Phix
function kahansum(sequence s)
    atom tot = 0, c = 0
    for i=1 to length(s) do
        atom x = s[i],
             y = x - c,
             t = tot + y
        c = (t - tot) - y
        tot = t
    end for
    return tot
end function

function epsilon()
    atom e=1
    while 1+e!=1 do
        e/=2
    end while
    return e
end function

procedure main2()
    atom a = 1.0,
         b := epsilon(),
         c := -b
    sequence s = {a,b,c}
    string fmt = iff(machine_bits()=32?"%.16f\n":"%.19f\n")
    ?{"Epsilon:",b}
    printf(1,"Simple sum: "&fmt,sum(s))
    printf(1,"Kahan sum : "&fmt,kahansum(s))
end procedure
main2()
```

32-bit

```txt

{"Epsilon:",1.110223024e-16}
Simple sum: 0.9999999999999998
Kahan sum : 1.0000000000000000

```

64-bit

```txt

{"Epsilon:",5.421010862e-20}
Simple sum: 0.9999999999999999999
Kahan sum : 1.0000000000000000000

```

Above and beyond:

```Phix
function NeumaierSum(sequence s)
    atom res = s[1],
         c = 0.0                -- A running compensation for lost low-order bits.
    for i = 2 to length(s) do
        atom t = res + s[i]
        if abs(res) >= abs(s[i]) then
            c += (res - t) + s[i] -- If res is bigger, low-order digits of s[i] are lost.
        else
            c += (s[i] - t) + res -- Else low-order digits of res are lost
        end if
        res = t
    end for
    return res + c              -- Correction only applied once at the very end
end function

procedure main3()
sequence s = {1e300,1,-1e300}
printf(1,"Simple sum: %d\n",sum(s))
printf(1,"Kahan sum : %d\n",kahansum(s))
printf(1,"Neumaier sum : %d\n",NeumaierSum(s))
end procedure
main3()
```

```txt

Simple sum: 0
Kahan sum : 0
Neumaier sum : 1

```



## PHP

'''Script'''

```php
<?php
// Main task

// 1. Do all arithmetic in decimal to a precision of six digits.
// PHP does not have a way of restricting overall precision

// 2.  Write a function/method/procedure/subroutine/... to perform Kahan
// summation on an ordered collection of numbers, (such as a list of numbers). )
function kahansum($input) {
    $sum = $c = 0;
    foreach($input as $item) {
        $y = $item + $c;
        $t = $sum + $y;
        $c = ($t - $sum) - $y;
        $sum = $t;
    }
    return $sum;
}

// 3. Create the three numbers a, b, c equal to 10000.0, 3.14159, 2.71828
// respectively.
$input = array(10000.0, 3.14159, 2.71828);
list($a, $b, $c) = $input;

// 4. show that the simple left-to-right summation, equivalent to (a + b) + c
// gives an answer of 10005.8 )
$sumabc = ($a + $b) + $c;
echo "Main task - Left to right summation: ";
echo sprintf("%6.1f", $sumabc).PHP_EOL;
// 10005.9

// 5. Show that the Kahan function applied to the sequence of values a, b, c
// results in the more precise answer of 10005.9
echo "Main task - Kahan summation: ";
echo sprintf("%6.1f", kahansum($input)).PHP_EOL;
// 10005.9

// Let's use the substask
$epsilon = 1.0;
while ((1.0 + $epsilon) != 1.0) {
    $epsilon /= 2.0;
}
echo "Trying the subtask".PHP_EOL."epsilon: ";
echo $epsilon.PHP_EOL;
// 1.1102230246252E-16

$a = 1.0;
$b = $epsilon;
$c = -$epsilon;

$input = array($a, $b, $c);

// left-to-right summation
$sumabc = ($a + $b) + $c;
echo "Sub task - Left to right summation: ";
echo sprintf("%.1f", $sumabc).PHP_EOL;

// kahan summation
echo "Sub task - Kahan summation: ";
echo sprintf("%.1f", kahansum($input)).PHP_EOL;

// but, are they really the same or is an artifact?
echo "Are the results the same?".PHP_EOL;
var_dump((($a + $b) + $c) === kahansum($input));

// ok, then what is the difference?
echo "Difference between the operations: ";
$diff = (($a + $b) + $c) - kahansum($input);
echo $diff.PHP_EOL;

```

'''Script output'''

```bash
$ php kahansum.php
Main task - Left to right summation: 10005.9
Main task - Kahan summation: 10005.9
Trying the subtask
epsilon: 1.1102230246252E-16
Sub task - Left to right summation: 1.0
Sub task - Kahan summation: 1.0
Are the results the same?
bool(false)
Difference between the operations: 1.1102230246252E-16

```



## Python


### Python: Decimal


```python>>>
 from decimal import *
>>>
>>> getcontext().prec = 6
>>>
>>> def kahansum(input):
    summ = c = 0
    for num in input:
        y = num - c
        t = summ + y
        c = (t - summ) - y
        summ = t
    return summ

>>> a, b, c = [Decimal(n) for n in '10000.0 3.14159 2.71828'.split()]
>>> a, b, c
(Decimal('10000.0'), Decimal('3.14159'), Decimal('2.71828'))
>>>
>>> (a + b) + c
Decimal('10005.8')
>>> kahansum([a, b, c])
Decimal('10005.9')
>>>
>>>
>>> sum([a, b, c])
Decimal('10005.8')
>>> # it seems Python's sum() doesn't make use of this technique.
>>>
>>> # More info on the current Decimal context:
>>> getcontext()
Context(prec=6, rounding=ROUND_HALF_EVEN, Emin=-999999, Emax=999999, capitals=1, clamp=0, flags=[Inexact, Rounded], traps=[InvalidOperation, DivisionByZero, Overflow])
>>>
>>>
>>> ## Lets try the simple summation with more precision for comparison
>>> getcontext().prec = 20
>>> (a + b) + c
Decimal('10005.85987')
>>>
```



### Python: Floats

This was written as proof that the floating-point sub-task could work and is better off displayed, so...

```python>>>
 eps = 1.0
>>> while 1.0 + eps != 1.0:
	eps = eps / 2.0


>>> eps
1.1102230246251565e-16
>>> (1.0 + eps) - eps
0.9999999999999999
>>> kahansum([1, eps, -eps])
1.0
>>>
>>>
>>> # Info on this implementation of floats
>>> import sys
>>> sys.float_info
sys.float_info(max=1.7976931348623157e+308, max_exp=1024, max_10_exp=308, min=2.2250738585072014e-308, min_exp=-1021, min_10_exp=-307, dig=15, mant_dig=53, epsilon=2.220446049250313e-16, radix=2, rounds=1)
>>>
```

Note that the kahansum function from the decimal example is [[wp:Duck typing|duck typed]] and adjusts to work with the number type used in its argument list.

Note also that the '''math.fsum''' function employs an even more precise algorithm for the summation of floating point numbers.


### Python: Arbitrary precision Decimal

Some languages have a decimal type, but cannot alter its precision to six digits. This example mimmicks this case byusing the default Python decimal precision and a variant of epsilon finding that divides by ten instead of two.

```python>>>
 from decimal import localcontext, Decimal
>>>
>>> with localcontext() as ctx:
	one, ten = Decimal('1.0'), Decimal('10')
	eps = one
	while one + eps != one:
		eps = eps / ten
	print('eps is:', eps)
	print('Simple sum is:', (one + eps) - eps)
	print('Kahan sum is:', kahansum([one, eps, -eps]))


eps is: 1E-28
Simple sum is: 0.9999999999999999999999999999
Kahan sum is: 1.000000000000000000000000000
>>>
```



## R

R can only limit the number of digits being displayed, not the ones used in calculation. In fact one of the base types is '''numeric''', implemented as standard floating point number.

Therefore, trying to use the main rules will not work as we can see in the code below


```r
# an implementation of the Kahan summation algorithm
kahansum <- function(x) {
    ks <- 0
    c <- 0
    for(i in 1:length(x)) {
        y <- x[i] - c
        kt <- ks + y
        c = (kt - ks) - y
        ks = kt
    }
    ks
}

# The three numbers a, b, c equal to 10000.0, 3.14159, 2.71828 respectively.
a <- 10000.0
b <- 3.14159
c <- 2.71828

# just to make sure, let's look at the classes of these three numbers
sapply(c(a, b, c), FUN=Class)
# [1] "numeric" "numeric" "numeric"

# The simple left-to-right summation: (a + b) + c
(a + b) + c
# [1] 10005.86

# The Kahan summation applied to the values a, b, c
input <- c(a, b, c)
kahansum(input)
# [1] 10005.86

```

Apparently there is no difference between the two approaches. So let's try the alternative steps given in the task.

We first calculate '''epsilon'''

```r
epsilon = 1.0
while ((1.0 + epsilon) != 1.0) {
    epsilon = epsilon / 2.0
}
epsilon
# [1] 1.11022e-16

```

and use it to test the left-to-right summation and the Kahan function

```r
a <- 1.0
b <- epsilon
c <- -epsilon

# left-to-right summation
(a + b) + c
# [1] 1

# kahan summation
kahansum(c(a, b, c))
# [1] 1

```

It might seem that again there is no difference, but let's explore a bit more and see if both results are really the same

```r
(a + b) + c == kahansum(c(a, b, c))
# FALSE

# ok, then what is the difference?
((a + b) + c) - kahansum(c(a, b, c))
# [1] -1.110223e-16

```

The absolute value of the difference, is very close to the value we obtained for '''epsilon'''

Just to make sure we are not fooling ourselves, let's see if the value of epsilon is stable using different divisors for the generating function

```r
# machine epsilon
mepsilon <- function(divisor) {
  guess <- 1.0
  while ((1.0 + guess) != 1.0) {
    guess <- guess / divisor
  }
  guess
}

# let's try from 2 to 1000
epsilons <- sapply(2:1000, FUN=mepsilon)
summary(epsilons)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 2.439e-19 1.905e-18 5.939e-18 1.774e-17 2.238e-17 1.110e-16

```

It would seem that it makes a differences what divisor we are using, let's look at the distribution using a text plotting library

```r
library(txtplot)

txtboxplot(epsilons)
#     0         1e-17      2e-17       3e-17       4e-17       5e-17
# |---+-----------+----------+-----------+-----------+-----------+--------|
#       +----+------------------+
#     --|    |                  |-------------------------------------
#       +----+------------------+

txtdensity(epsilons)
# 6e+16 +--+---------+----------+----------+---------+----------+----------+
#       |   **                                                             |
#       |  ***                                                             |
# 5e+16 +  * **                                                            +
#       |     *                                                            |
# 4e+16 +     *                                                            +
#       |      *                                                           |
#       |      *                                                           |
# 3e+16 +      **                                                          +
#       |       *                                                          |
# 2e+16 +       **                                                         +
#       |        **                                                        |
#       |         ***                                                      |
# 1e+16 +           ****                                                   +
#       |              *************                                       |
#     0 +                          ************************************    +
#       +--+---------+----------+----------+---------+----------+----------+
#          0       2e-17      4e-17      6e-17     8e-17      1e-16

```

Definitely the epsilon generating function is not stable and gives a positively skewed (right-tailed) distribution.
We could consider using the median value of the series of epsilons we have estimated, but because the precision for the base numeric type (class) in R is ~16 decimals, using that value will be in practice indistinguishable from using zero.

```r
epsilon <- median(epsilons)
epsilon
# [1] 5.939024e-18

a <- 1.0
b <- epsilon
c <- -epsilon

# left-to-right summation
(a + b) + c
# [1] 1

# kahan summation
kahansum(c(a, b, c))
# [1] 1

# are they the same?
(a + b) + c == kahansum(c(a, b, c))
# TRUE

```



## Racket

Racket doesn't have arbitrary fixed precision numbers, but we can use single precision float point numbers that have approximately 8 decimal places. To reproduce the original task we have to replace the <code>10000.0</code> with <code>1000000.0</code>.

We then compare this result to the double precision result of the same numbers. The double precision numbers have almost 16 decimal places, so with <code>1000000.0</code> the usual summation nd the Kahan summation give the same result.

Finally we try the alternative task version for single precision float point numbers.

```Racket
#lang racket

(define (sum/kahan . args)
  (define-values (sum c)
    (for/fold ([sum 0] [c 0]) ([num args])
      (define y (- num c))
      (define t (+ sum y))
      (values t (- (- t sum) y))))
  sum)

(displayln "Single presition flonum")
(+ 1000000.0f0 3.14159f0 2.71828f0)
(sum/kahan 1000000.0f0 3.14159f0 2.71828f0)

(displayln "Double presition flonum")
(+ 1000000.0 3.14159 2.71828)
(sum/kahan 1000000.0 3.14159 2.71828)
```

```txt
Single presition flonum
1000005.8f0
1000005.9f0
Double presition flonum
1000005.85987
1000005.85987
```


Alternative task version

```Racket
(define epsilon.f0
  (let loop ([epsilon 1.f0])
    (if (= (+ 1.f0 epsilon) 1.f0)
        epsilon
        (loop (/ epsilon 2.f0)))))

(displayln "Alternative task, single precision flonum")
epsilon.f0

(+ 1.f0 epsilon.f0 (- epsilon.f0))
(sum/kahan 1.f0 epsilon.f0 (- epsilon.f0))
```

```txt
Alternative task, single precision flonum
5.9604645f-008
0.99999994f0
1.0f0
```



## REXX

Programming note:   It wasn't clear what precision to use for the 2<sup>nd</sup> part of this task, so an arbitrary precision

of   '''30'''   (decimal digits) was chosen.   The default precision for REXX is   '''9'''   decimal digits.

### vanilla version


```rexx
/*REXX program demonstrates  simple addition  versus using  Kahan  summation algorithm. */
numeric digits 6                                 /*use six decimal digits for precision.*/
call show  10000.0,  3.14169,  2.71828           /*invoke SHOW to sum & display numbers.*/
numeric digits 30                                /*from now on, use  30  decimal digits.*/
epsilon=1
                   do  while  1+epsilon \= 1     /*keep looping 'til we can't add unity.*/
                   epsilon=epsilon / 2           /*halve the value of  epsilon variable.*/
                   end   /*while*/
say                                              /*display a blank line before the fence*/
say copies('▒', 70);    say                      /*display a fence, then a blank line.  */
call show   1.0,   epsilon,  -epsilon            /*invoke SHOW to sum & display numbers.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
kahan: procedure;  $=0;  c=0;    do j=1  for arg()         /*perform for each argument. */
                                 y=arg(j) - c              /*subtract  C  from argument.*/
                                 t=$ + y                   /*use a temporary sum  (T).  */
                                 c=t - $ - y               /*compute the value of  C.   */
                                 $=t                       /*redefine the sum  ($).     */
                                 end   /*j*/
       return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:  procedure;  parse arg a,b,c                         /*obtain the arguments.      */
       say 'decimal digits ='  digits()                    /*show number of decimal digs*/
       say '   a = '     left('', a>=0)     a              /*display   A   justified.   */
       say '   b = '     left('', b>=0)     b              /*   "      B       "        */
       say '   c = '     left('', c>=0)     c              /*   "      C       "        */
       say 'simple summation of a,b,c = '         a+b+c    /*compute simple summation.  */
       say 'Kahan  summation of a,b,c = '   kahan(a,b,c)   /*sum via Kahan  summation.  */
       return
```

```txt
decimal digits = 6
   a =  10000.0
   b =  3.14169
   c =  2.71828
simple summation of a,b,c =  10005.8
Kahan  summation of a,b,c =  10005.9

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒

decimal digits = 30
   a =   1.0
   b =   0.00000000000000000000000000000315544362088404722164691426133
   c =  -0.00000000000000000000000000000315544362088404722164691426133
simple summation of a,b,c =  1.00000000000000000000000000000
Kahan  summation of a,b,c =  1.00000000000000000000000000000
```

```txt
decimal digits = 6
   a =   10000.0
   b =   3.14169
   c =   2.71828
simple summation of a,b,c =  10005.8
Kahan  summation of a,b,c =  10005.9

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒

decimal digits = 30
   a =   1.0
   b =   0.00000000000000000000000000000315544362088404722164691426133
   c =  -0.00000000000000000000000000000315544362088404722164691426133
simple summation of a,b,c =  0.999999999999999999999999999997
Kahan  summation of a,b,c =  1.00000000000000000000000000000

```

```txt

decimal digits = 6
   a =   10000.0
   b =   3.14169
   c =   2.71828
simple summation of a,b,c =  10005.8
Kahan  summation of a,b,c =  10005.9

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒

decimal digits = 30
   a =   1.0
   b =   3.15544362088404722164691426133E-30
   c =  -3.15544362088404722164691426133E-30
simple summation of a,b,c =  0.999999999999999999999999999997
Kahan  summation of a,b,c =  1.00000000000000000000000000000
```

```txt
decimal digits = 6
   a =   10000.0
   b =   3.14169
   c =   2.71828
simple summation of a,b,c =  10005.8
Kahan  summation of a,b,c =  10005.9

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒

decimal digits = 30
   a =   1.0
   b =   3.15544362088404722164691426133E-30
   c =  -3.15544362088404722164691426133E-30
simple summation of a,b,c =  1.00000000000000000000000000000
Kahan  summation of a,b,c =  1.00000000000000000000000000000
```



### tweaked version

The following tweaked REXX version causes Regina (version 3.4 and later) to work properly.

```rexx
/*REXX program demonstrates  simple addition  versus using  Kahan  summation algorithm. */
numeric digits 6                                 /*use six decimal digits for precision.*/
call show  10000.0,  3.14169,  2.71828           /*invoke SHOW to sum & display numbers.*/
numeric digits 30                                /*from now on, use  30  decimal digits.*/
epsilon=1
                   do  while  1+epsilon \= 1     /*keep looping 'til we can't add unity.*/
                   epsilon=epsilon / 2           /*halve the value of  epsilon variable.*/
                   end   /*while*/
say                                              /*display a blank line before the fence*/
say copies('▒', 70);    say                      /*display a fence, then a blank line.  */
                                                 /* [↓]  for Regina REXX 3.4 and later. */
numeric digits digits()+2                        /*bump the precision by two dec digits.*/
call show   1.0,   epsilon,  -epsilon            /*invoke SHOW to sum & display numbers.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
kahan: procedure;  $=0;  c=0;    do j=1  for arg()         /*perform for each argument. */
                                 y=arg(j) - c              /*subtract  C  from argument.*/
                                 t=$ + y                   /*use a temporary sum  (T).  */
                                 c=t - $ - y               /*compute the value of  C.   */
                                 $=t                       /*redefine the sum  ($).     */
                                 end   /*j*/
       return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:  procedure;  parse arg a,b,c                         /*obtain the arguments.      */
       say 'decimal digits ='  digits()                    /*show number of decimal digs*/
       say '   a = '     left('', a>=0)     a              /*display   A   justified.   */
       say '   b = '     left('', b>=0)     b              /*   "      B       "        */
       say '   c = '     left('', c>=0)     c              /*   "      C       "        */
       say 'simple summation of a,b,c = '         a+b+c    /*compute simple summation.  */
       say 'Kahan  summation of a,b,c = '   kahan(a,b,c)   /*sum via Kahan  summation.  */
       return
```

```txt
decimal digits = 6
   a =    10000.0
   b =    3.14169
   c =    2.71828
simple summation of a,b,c =  10005.8
Kahan  summation of a,b,c =  10005.9

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒

decimal digits = 32
   a =    1.0
   b =    3.15544362088404722164691426133E-30
   c =   -3.15544362088404722164691426133E-30
simple summation of a,b,c =  1.0000000000000000000000000000001
Kahan  summation of a,b,c =  1.0000000000000000000000000000000
```


'''output''' for ooRexx:

```txt
decimal digits = 6
   a =    10000.0
   b =    3.14169
   c =    2.71828
simple summation of a,b,c =  10005.8
Kahan  summation of a,b,c =  10005.9

ªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªª

decimal digits = 32
   a =    1.0
   b =    0.00000000000000000000000000000315544362088404722164691426133
   c =   -0.00000000000000000000000000000315544362088404722164691426133
simple summation of a,b,c =  1.0000000000000000000000000000001
Kahan  summation of a,b,c =  1.0000000000000000000000000000000

ªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªª
```



## Ruby

Arrays provide a "sum" method, which uses a Kahan-Babuska balancing compensated summation algorithm, according to the C-source.

```ruby
epsilon = 1.0
epsilon /= 2 until 1.0 + epsilon == 1.0

a = 1.0
b = epsilon
c = -b

puts "epsilon    : #{epsilon}"
puts "(a+b)+c    : #{(a+b)+c}"
puts "[a,b,c].sum: #{[a,b,c].sum}"

```

```txt
epsilon    : 1.1102230246251565e-16
(a+b)+c    : 0.9999999999999999
[a,b,c].sum: 1.0

```


## Scala

===IEEE 754 Single precision 32-bit (JavaScript defaults to Double precision.)===

```Scala
import scala.annotation.tailrec

object KahanSummation extends App {
  {
    def epsilon: Float = {
      @tailrec
      def eps(x: Float): Float = if (1 + x == 1) x else eps(x / 2)

      eps(1)
    }

    def kahanSum(fa: Float*): Float = {
      @tailrec
      def iter(fa: Seq[Float], c: Float, sum: Float): Float =
        if (fa.nonEmpty) {
          val y = fa.head - c
          val t = sum + y
          iter(fa.tail, (t - sum) - y, t)
        } else sum

      iter(fa, 0, 0)
    }

    val (a, ε) = (1f, epsilon)
    println(f"${"ε"}%-12s= ${ε}")
    println(f"(a + b) - b = ${(a + ε) - ε}")
    println("Kahan sum   = " + kahanSum(a, ε, -ε))
  }
}
```

{{Out}}See it running in your browser by [https://scalafiddle.io/sf/lWI0SJC/0 ScalaFiddle (JavaScript, non JVM)] or by [https://scastie.scala-lang.org/CI7dNAKQSlupivKAnYcGJg Scastie (remote JVM)].
Note: JVM float is  IEEE-754 32 bit floating point while JavaScript always default to the IEEE 754 standard 64-bit.


## Tcl


### Tcl: Floats

First, using native floating point we see the same epsilon value as other languages using float64:


```Tcl
# make {+ - * /} etc available as commands, for easier expressions
namespace path ::tcl::mathop

# find epsilon with native floating point:
proc epsilon {} {
    set e 1.0
    while {1 + $e != 1} {
        set e   [/ $e 2]
    }
    return $e
}

# kahan sum with native floats:
proc kahansum {args} {
    set sum 0.0
    set c   0.0
    foreach i $args {
        set y   [- $i $c]
        set t   [+ $sum $y]
        set c   [- [- $t $sum] $y]
        set sum $t
    }
    return $sum
}

puts "Native floating point:"
puts "\tEpsilon is: [set e [epsilon]]"
puts "\tAssociative sum: [expr {1.0 + $e - $e}]"
puts "\tKahan sum: [kahansum 1.0 $e -$e]"
```


```txt
Epsilon is: 1.1102230246251565e-16
Associative sum: 0.9999999999999999
Kahan sum: 1.0
```



### Tcl: Decimals

For the decimal part of the exercise we can use a the Tcllib library <tt>math::decimal</tt>.  Note how similar the implementation of Kahan sum is:  the only changes are <tt>fromstr</tt> and <tt>tostr</tt>.

The last stanza exercises the decimal package's different rounding modes, to see what happens there:

```Tcl
package require math::decimal
namespace path ::math::decimal

proc kahansum {args} {
    set sum [fromstr 0.0]
    set c   [fromstr 0.0]
    foreach i $args {
        set i [fromstr $i]
        set y   [- $i $c]
        set t   [+ $sum $y]
        set c   [- [- $t $sum] $y]
        set sum $t
    }
    return [tostr $sum]
}

proc asum {args} {
    set sum [fromstr 0.0]
    foreach a $args {
        set sum [+ $sum [fromstr $a]]
    }
    return [tostr $sum]
}

setVariable precision 6
set a 10000.0
set b 3.14159
set c 2.71828

foreach rounding {half_even half_up half_down down up floor ceiling} {
    setVariable rounding $rounding
    puts "Rounding mode: $rounding"
    puts "\tAssociative sum $a + $b + $c: [asum $a $b $c]"
    puts "\tKahan       sum $a + $b + $c: [kahansum $a $b $c]"
}
```

The results are a little surprising:
```txt
Rounding mode: half_even
        Associative sum 10000.0 + 3.14159 + 2.71828: 10005.8
        Kahan       sum 10000.0 + 3.14159 + 2.71828: 10005.9
Rounding mode: half_up
        Associative sum 10000.0 + 3.14159 + 2.71828: 10005.8
        Kahan       sum 10000.0 + 3.14159 + 2.71828: 10005.9
Rounding mode: half_down
        Associative sum 10000.0 + 3.14159 + 2.71828: 10005.8
        Kahan       sum 10000.0 + 3.14159 + 2.71828: 10005.9
Rounding mode: down
        Associative sum 10000.0 + 3.14159 + 2.71828: 10005.8
        Kahan       sum 10000.0 + 3.14159 + 2.71828: 10005.8
Rounding mode: up
        Associative sum 10000.0 + 3.14159 + 2.71828: 10006.0
        Kahan       sum 10000.0 + 3.14159 + 2.71828: 10005.9
Rounding mode: floor
        Associative sum 10000.0 + 3.14159 + 2.71828: 10005.8
        Kahan       sum 10000.0 + 3.14159 + 2.71828: 10005.8
Rounding mode: ceiling
        Associative sum 10000.0 + 3.14159 + 2.71828: 10006.0
        Kahan       sum 10000.0 + 3.14159 + 2.71828: 10005.9
```

In no rounding mode are both answers correct.
With "down" and "floor" rounding, the Kahan sum is too low (10005.8), but any other rounding makes it correct (10005.9).
The Associative largest-to-smallest sum is never correct:  "up" and "ceiling" rounding make it too high, while the rest make it low.


## zkl

zkl floats are C doubles.
```zkl
fcn kahanSum(numbers){
   sum:=c:=0.0;
   foreach x in (vm.arglist){
      y,t:=x - c, sum + y;
      c=(t - sum) - y;
      sum=t;
   }
   sum
}
fcn epsilon{
    e:=1.0;
    while(1.0 + e!=1.0){ e/=2 }
    e
}
```


```zkl
a,b,c,sum:=1.0,epsilon(),-b,a + b + c;
sum             :"%.20f".fmt(_).println("\tLeft associative. Delta from 1: ",1.0 - sum);
kahanSum(a,b,c) :"%.20f".fmt(_).println("\tKahan summation");
b.println("\t\tEpsilon");
```

```txt
0.99999999999999988898	Left associative. Delta from 1: 1.11022e-16
1.00000000000000000000	Kahan summation
1.11022e-16		Epsilon
```

