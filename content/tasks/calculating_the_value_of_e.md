+++
title = "Calculating the value of e"
description = ""
date = 2019-10-18T19:51:02Z
aliases = []
[extra]
id = 21788
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "360_assembly",
  "ada",
  "algol_68",
  "applescript",
  "awk",
  "burlesque",
  "c",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dyalect",
  "easylang",
  "edsac_order_code",
  "factor",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "k",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "min",
  "myrddin",
  "nim",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "standard_ml",
  "swift",
  "tcl",
  "vbscript",
  "visual_basic_dotnet",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

### Task:
Calculate the value of   <big>''e''</big>.


(<big>''e''</big>   is also known as   ''Euler's number''   and   ''Napier's constant''.)


See details: [https://en.wikipedia.org/wiki/E_(mathematical_constant) Calculating the value of e]





## 11l

{{trans|Python}}

```11l
V e0 = 0.0
V e = 2.0
V n = 0
V fact = 1
L (e - e0 > 1e-15)
   e0 = e
   n++
   fact *= 2 * n * (2 * n + 1)
   e += (2.0 * n + 2) / fact

print(‘Computed e = ’e)
print(‘Real e = ’math:e)
print(‘Error = ’(math:e - e))
print(‘Number of iterations = ’n)
```

{{out}}

```txt

Computed e = 2.718281779
Real e = 2.718281828
Error = 4.941845111e-8
Number of iterations = 8

```



## 360 Assembly

The 'include' file FORMAT, to format a floating point number,  can be found in:
[[360_Assembly_include|Include files 360 Assembly]].

```360asm
*        Calculating the value of e - 21/07/2018
CALCE    PROLOG
         LE     F0,=E'0'
         STE    F0,EOLD            eold=0
         LE     F2,=E'1'           e=1
         LER    F4,F2              xi=1
         LER    F6,F2              facti=1
BWHILE   CE     F2,EOLD            while e<>eold
         BE     EWHILE             ~
         STE    F2,EOLD              eold=e
         LE     F0,=E'1'             1
         DER    F0,F6                1/facti
         AER    F2,F0                e=e+1/facti
         AE     F4,=E'1'             xi=xi+1
         MER    F6,F4                facti=facti*xi
         LER    F0,F4                xi
         B      BWHILE             end while
EWHILE   LER    F0,F2              e
         LA     R0,5               number of decimals
         BAL    R14,FORMATF        format a float number
         MVC    PG(13),0(R1)       output e
         XPRNT  PG,L'PG            print e
         EPILOG
         COPY   FORMATF            format a float number
EOLD     DS     E                  eold
PG       DC     CL80' '            buffer
         REGEQU
         END    CALCE
```

{{out}}

```txt

      2.71828

```


## Ada

{{trans|Kotlin}}

```ada
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;

procedure Euler is
   Epsilon : constant     := 1.0E-15;
   Fact    : Long_Integer := 1;
   E       : Long_Float   := 2.0;
   E0      : Long_Float   := 0.0;
   N       : Long_Integer := 2;

begin

   loop
      E0   := E;
      Fact := Fact * N;
      N    := N + 1;
      E    := E + (1.0 / Long_Float (Fact));
      exit when abs (E - E0) < Epsilon;
   end loop;

   Put ("e = ");
   Put (E, 0, 15, 0);
   New_Line;

end Euler;
```

{{out}}

```txt

e = 2.718281828459046

```



## ALGOL 68

{{trans|Kotlin}}

```algol68
BEGIN
    # calculate an approximation to e #
    LONG REAL epsilon = 1.0e-15;
    LONG INT  fact   := 1;
    LONG REAL e      := 2;
    LONG INT  n      := 2;
    WHILE
        LONG REAL e0 = e;
        fact *:= n;
        n    +:= 1;
        e    +:= 1.0 / fact;
        ABS ( e - e0 ) >= epsilon
    DO SKIP OD;
    print( ( "e = ", fixed( e, -17, 15 ), newline ) )
END
```

{{out}}

```txt

e = 2.718281828459045

```



## AppleScript

For the purposes of 32 bit floating point, the value seems to stabilise after summing c. 16 terms.


```applescript
on run

    sum(map(inverse, ¬
        scanl(product, 1, enumFromToInt(1, 16))))

    --> 2.718281828459

end run

-- inverse :: Float -> Float
on inverse(x)
    1 / x
end inverse

-- product :: Float -> Float -> Float
on product(a, b)
    a * b
end product


-- GENERIC FUNCTIONS ----------------------------------------

-- enumFromToInt :: Int -> Int -> [Int]
on enumFromToInt(m, n)
    if m ≤ n then
        set lst to {}
        repeat with i from m to n
            set end of lst to i
        end repeat
        return lst
    else
        return {}
    end if
end enumFromToInt

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
on iterateUntil(p, f, x)
    script
        property mp : mReturn(p)'s |λ|
        property mf : mReturn(f)'s |λ|
        property lst : {x}
        on |λ|(v)
            repeat until mp(v)
                set v to mf(v)
                set end of lst to v
            end repeat
            return lst
        end |λ|
    end script
    |λ|(x) of result
end iterateUntil

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

-- scanl :: (b -> a -> b) -> b -> [a] -> [b]
on scanl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        set lst to {startValue}
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
            set end of lst to v
        end repeat
        return lst
    end tell
end scanl

-- sum :: [Num] -> Num
on sum(xs)
    script add
        on |λ|(a, b)
            a + b
        end |λ|
    end script

    foldl(add, 0, xs)
end sum
```

{{Out}}

```txt
2.718281828459
```



## AWK


```AWK

# syntax: GAWK -f CALCULATING_THE_VALUE_OF_E.AWK
BEGIN {
    epsilon = 1.0e-15
    fact = 1
    e = 2.0
    n = 2
    do {
      e0 = e
      fact *= n++
      e += 1.0 / fact
    } while (abs(e-e0) >= epsilon)
    printf("e=%.15f\n",e)
    exit(0)
}
function abs(x) { if (x >= 0) { return x } else { return -x } }

```

{{out}}

```txt

e=2.718281828459046

```



## Burlesque


```burlesque

blsq ) 70rz?!{10 100**\/./}ms36.+Sh'.1iash
2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274

```



## C

{{trans|Kotlin}}

```c
#include <stdio.h>
#include <math.h>

#define EPSILON 1.0e-15

int main() {
    unsigned long long fact = 1;
    double e = 2.0, e0;
    int n = 2;
    do {
        e0 = e;
        fact *= n++;
        e += 1.0 / fact;
    }
    while (fabs(e - e0) >= EPSILON);
    printf("e = %.15f\n", e);
    return 0;
}
```


{{output}}

```txt

e = 2.718281828459046

```




## C++

{{trans|C}}

```cpp
#include <iostream>
#include <iomanip>
#include <cmath>

using namespace std;

int main() {
    const double EPSILON = 1.0e-15;
    unsigned long long fact = 1;
    double e = 2.0, e0;
    int n = 2;
    do {
        e0 = e;
        fact *= n++;
        e += 1.0 / fact;
    }
    while (fabs(e - e0) >= EPSILON);
    cout << "e = " << setprecision(16) << e << endl;
    return 0;
}
```


{{output}}

```txt

e = 2.718281828459046

```


## C#

```c#
using System;

namespace CalculateE {
    class Program {
        public const double EPSILON = 1.0e-15;

        static void Main(string[] args) {
            ulong fact = 1;
            double e = 2.0;
            double e0;
            uint n = 2;
            do {
                e0 = e;
                fact *= n++;
                e += 1.0 / fact;
            } while (Math.Abs(e - e0) >= EPSILON);
            Console.WriteLine("e = {0:F15}", e);
        }
    }
}
```

{{out}}

```txt
e = 2.718281828459050
```




### Using Decimal type


```c#
using System;

class Calc_E
{

    static Decimal CalcE()
    {
        Decimal f = 1, e = 2; int n = 1;
        do e += (f = f / ++n); while (f > 1e-27M);
        return e;
    }

    static void Main()
    {
        Console.WriteLine(Math.Exp(1)); // double precision built-in result
        Console.WriteLine(CalcE());  // Decimal precision result
    }
}
```

{{out}}

```txt
2.71828182845905
2.7182818284590452353602874713
```


### Arbitrary Precision

{{libheader|System.Numerics}}
Automatically determines number of padding digits required for the arbitrary precision output. Can calculate a quarter million digits of '''e''' in under half a minute.

```c#
using System; using System.Numerics;
using static System.Math; using static System.Console;

static class Program
{
    static string CalcE(int nDigs)
    {
        int pad = (int)Round(Log10(nDigs)), n = 1;
        BigInteger f = BigInteger.Pow(10, nDigs + pad), e = f + f;
        do e += (f /= ++n); while (f > n);
        return (e / BigInteger.Pow(10, pad + 1)).ToString().Insert(1, ".");
    }

    static void Main()
    {
        WriteLine(Exp(1));  //  double precision built-in function
        WriteLine(CalcE(100));   //  arbitrary precision result
        DateTime st = DateTime.Now; int qmil = 250_000;
        string es = CalcE(qmil);  //  large arbitrary precision result string
        WriteLine("{0:n0} digits in {1:n3} seconds.", qmil, (DateTime.Now - st).TotalSeconds);
        WriteLine("partial: {0}...{1}", es.Substring(0, 46), es.Substring(es.Length - 45));
    }
}
```

{{out}}

```txt
2.71828182845905
2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427
250,000 digits in 22.833 seconds.
partial: 2.71828182845904523536028747135266249775724709...026587951482508371108187783411598287506586313

```



## COBOL

{{trans|C}}

```COBOL>       >
SOURCE FORMAT IS FIXED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EULER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 EPSILON USAGE COMPUTATIONAL-2 VALUE 1.0E-15.
           01 FACT USAGE BINARY-DOUBLE UNSIGNED VALUE 1.
           01 N USAGE BINARY-INT UNSIGNED.
           01 E USAGE COMPUTATIONAL-2 VALUE 2.0.
           01 E0 USAGE COMPUTATIONAL-2 value 0.0.
           01 RESULT-MESSAGE.
              03 FILLER PIC X(4) VALUE 'e = '.
              03 RESULT-VALUE PIC 9.9(18) USAGE DISPLAY.
       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM
              VARYING N FROM 2 BY 1
              UNTIL FUNCTION ABS(E - E0) < EPSILON
              MOVE E TO E0
              COMPUTE FACT = FACT * N
              COMPUTE E = E + 1.0 / FACT
           END-PERFORM.
           MOVE E TO RESULT-VALUE.
           DISPLAY RESULT-MESSAGE.
           STOP RUN.

```

{{out}}

```txt

e = 2.718281828459041093

```



## Common Lisp


```lisp
;;Change this to change how many iterations
(setq iters 1000)

;;Tail Recursive Factorial function
(defun fact (x &optional (y 1)) "calculates x!"
	(if (<= x 0) y (fact (- x 1) (* y x))))

;;Recursive calculate e function
(defun calc (iterations) "Calculates e for however many iterations"
	(if (< iterations 0) 0 (+ (/ 1 (fact iterations)) (calc (- iterations 1)))))

(print (float (calc iters)))
```

Output:

```txt

2.7182817

```




## Clojure


```clojure

;; Calculating the number e, euler-napier number.
;; We will use two methods
;; First method: the forumula (1 + 1/n)^n
;; Second method: the series partial sum 1/(p!)


;;first method

(defn inverse-plus-1 [n]
  (+ 1 (/ 1 n)))

(defn e-return [n]
  (Math/pow (inverse-plus-1 n) n))

(time (e-return 100000.))

;;"Elapsed time: 0.165629 msecs"
;;2.7182682371922975

;;SECOND METHOD

(defn method-e [n]
  (loop [e-aprx 0M
        value-add 1M
        p  1M]
    (if (> p n)
    e-aprx
    (recur (+ e-aprx value-add) (/ value-add p) (inc p)))))

(time (with-precision 110 (method-e 200M)))


```



```txt

"Elapsed time: 11.310568 msecs"
2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274274663923M

```



## D


```d
import std.math;
import std.stdio;

enum EPSILON = 1.0e-15;

void main() {
    ulong fact = 1;
    double e = 2.0;
    double e0;
    int n = 2;
    do {
        e0 = e;
        fact *= n++;
        e += 1.0 / fact;
    } while (abs(e - e0) >= EPSILON);
    writefln("e = %.15f", e);
}
```

{{out}}

```txt
e = 2.718281828459046
```



## Delphi

''See [[#Pascal|Pascal]]''


## Dyalect


{{trans|Swift}}


```dyalect
func calculateE(epsilon = 1.0e-15) {
    func abs(n) {
        if n < 0 {
            -n
        } else {
            n
        }
    }

    var fact = 1
    var e = 2.0
    var e0 = 0.0
    var n = 2

    while true {
        e0 = e
        fact *= n
        n += 1
        e += 1.0 / Float(fact)

        if abs(e - e0) < epsilon {
            break
        }
    }

    return e
}

print(calculateE())
```


{{out}}


```txt
2.71828182845905
```



## EasyLang

<lang>fact = 1
n = 2
e# = 2
while absf (e# - e0#) > 0.0001
  e0# = e#
  fact = fact * n
  n += 1
  e# += 1 / fact
.
print e#
```



## EDSAC order code

The difficulty here is that the EDSAC was designed to hold real numbers
in the range -1 <= x < 1 only.
Subroutines were devised for floating-point arithmetic,
but rather than get involved with those we'll cheat slightly
by calculating e - 2 and printing the result with '2' in front.
It will be seen that the answer is 3 out in the 10th decimal place.

```edsac

  [Calculate e]
  [EDSAC program, Initial Orders 2]

  [Library subroutine M3. Prints header and is then overwritten]
  [Here, last character sets teleprinter to figures]
   PFGKIFAFRDLFUFOFE@A6FG@E8FEZPF
   @&*CALCULATION!OF!E@&#
     ..PZ  [blank tape, needed to mark end of header text]

  [Library subroutine D6. Division, accurate, fast.
  Closed, 36 locations, working positions 6D and 8D.
  C(0D) := C(0D)/C(4D), where C(4D) <> 0, -1.]
     T56K  [define load address for subroutine]
     GKA3FT34@S4DE13@T4DSDTDE2@T4DADLDTDA4DLDE8@RDU4DLDA35@
     T6DE25@U8DN8DA6DT6DH6DS6DN4DA4DYFG21@SDVDTDEFW1526D

  [Library subroutine P1.
  Prints a single positive number (without layout or round-off).
  Prints number in 0D to n places of decimals, where
  n is specified by 'P n F' pseudo-order after subroutine call.
  Closed, 21 locations.]
    T92K  [define load address for subroutine]
   GKA18@U17@S20@T5@H19@PFT5@VDUFOFFFSFL4FTDA5@A2FG6@EFU3FJFM1F
    ..PZ

               [Main routine]
        T120K  [Define load address for main program.
                Must be even, because of double values at start.]
        GK     [set @ (theta) for relative addresses]
    [0] PF PF  [build sum 4*(1/3! + 1/4! + 1/5! + ...)]
    [2] PF PF  [term in sum]
    [4] PD PF  [2^-34, stop when term < this]
    [6] PF     [divisor]
    [7] IF     [1/2]
    [8] QF     [1/16]
    [9] @F     [carriage return]
   [10] &F     [line feed]
   [11] WF     [digit '2']
   [12] MF     [full stop / decimal point]
   [13] K4096F [teleprinter null]

   [14] A8@    [load 1/16]
        LD     [shift, makes 1/8]
        UD     [to 0D for subroutine D6]
        T6@    [divisor := 1/8]
        T#@    [sum := 0]

               [loop, acc assumed to be 0 here]
   [19] A6@    [load divisor]
        A8@    [add 1/16]
        U6@    [update divisor]
        T4D    [to 4D for subroutine D6]
   [23] A23@   [for subroutine return]
        G56F   [call D6]
        AD     [load quotient]
        U2#@   [store as term]
        A#@    [add term into sum]
        T#@    [update sum]
        A2#@   [load term]
        S4#@   [test for convergence]
        G36@   [jump out if so]
        A4#@   [restore term after test]
        R4F    [divide by 16]
        TD     [to 0D for subroutine D6]
        E19@   [loop back]

               [here when converged]
   [36] TF     [clear acc]
        A#@    [load sum]
        R1F    [shift to divide by 4]
        A7@    [add 1/2, now have (e - 2)]
        YF     [round]
        TD     [to 0D for subroutine P1]
        O11@   [print '2.']
        O12@
   [44] A44@   [for subroutine return]
        G92F   [call P1 to print (e - 2)]
        P10F   [10 decimals]
        O9@    [print CR]
        O10@   [print LF]
        O13@   [null to flush print buffer]
        ZF     [stop]
        E14Z   [relative address of entry]
        PF     [enter with accumulator = 0]

```

{{out}}

```txt

CALCULATION OF E
2.7182818282

```


=={{header|F_Sharp|F#}}==

```fsharp

// A function to generate the sequence 1/n!). Nigel Galloway: May 9th., 2018
let e = Seq.unfold(fun (n,g)->Some(n,(n/g,g+1N))) (1N,1N)

```

Which may be used:

```fsharp

printfn "%.14f" (float (e |> Seq.take 20 |> Seq.sum))

```

{{out}}

```txt

2.71828182845905

```



## Factor

{{works with|Factor|0.98}}

```factor
USING: math math.factorials prettyprint sequences ;
IN: rosetta-code.calculate-e

CONSTANT: terms 20

terms <iota> [ n! recip ] map-sum >float .
```

{{out}}

```txt

2.718281828459045

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Calculating_the_value_of_e this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

Algorithm e-spigot: compute the first n decimal digits of e
(due to Stanley Rabinowitz and Stan Wagon):

1. Initialize: Let the first digit be 2 and initialize an array A of length n + 1 to (1, 1, 1, . . . , 1).

2. Repeat n − 1 times:

* Multiply by 10: Multiply each entry of A by 10.

* Take the fractional part: Starting from the right, reduce the ith entry of A modulo i + 1, carrying the quotient one place left.

* Output the next digit: The final quotient is the next digit of e.


```forth
100 constant #digits
: int-array  create cells allot  does> swap cells + ;

#digits 1+ int-array e-digits[]

: init-e ( -- )
   [ #digits 1+ ] literal 0 DO
      1  i e-digits[]  !
   LOOP
   ." = 2." ;

: .e  ( -- )
   init-e
   [ #digits 1- ] literal 0 DO
      0  \ carry
      0 #digits DO
         i e-digits[] dup @  10 *  rot +  i 2 + /mod -rot  swap !
      -1 +LOOP
      0 .r
   LOOP ;

```

{{out}}

```txt

Gforth 0.7.3, Copyright (C) 1995-2008 Free Software Foundation, Inc.
Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
Type `bye' to exit
.e = 2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427 ok

```


## Fortran


```fortran

Program eee
implicit none
integer, parameter  :: QP = selected_real_kind(16)
real(QP), parameter :: one = 1.0
real(QP)            :: ee

write(*,*) '    exp(1.) ', exp(1._QP)

ee = 1. +(one +(one +(one +(one +(one+ (one +(one +(one +(one +(one +(one &
        +(one +(one +(one +(one +(one +(one +(one +(one +(one +(one)      &
        /21.)/20.)/19.)/18.)/17.)/16.)/15.)/14.)/13.)/12.)/11.)/10.)/9.)  &
        /8.)/7.)/6.)/5.)/4.)/3.)/2.)

write(*,*) ' polynomial ', ee

end Program eee
```

{{out}}

```txt

     exp(1.)    2.71828182845904523543
  polynomial    2.71828182845904523543

```



## FreeBASIC


### Normal basic


```freebasic
' version 02-07-2018
' compile with: fbc -s console

Dim As Double e , e1
Dim As ULongInt n = 1, n1 = 1

e = 1 / 1

While e <> e1
    e1 = e
    e += 1 / n
    n1 += 1
    n *= n1
Wend

Print "The value of e ="; e

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
The value of e = 2.718281828459046
```


### GMP version

{{libheader|GMP}}

```freebasic
' version 02-07-2018
' compile with: fbc -s console

#Include "gmp.bi"

Sub value_of_e(e As Mpf_ptr)

    Dim As ULong n = 1
    Dim As Mpf_ptr e1, temp
    e1   = Allocate(Len(__mpf_struct)) : Mpf_init(e1)
    temp = Allocate(Len(__mpf_struct)) : Mpf_init(temp)

    Dim As Mpz_ptr fac
    fac = Allocate(Len(__mpz_struct)) : Mpz_init_set_ui(fac, 1)

    Mpf_set_ui(e, 1)     ' 1 / 0! = 1 / 1

    While Mpf_cmp(e1, e) <> 0
        Mpf_set(e1, e)
        Mpf_set_z(temp, fac)
        n+= 1
        Mpz_mul_ui(fac, fac, n)
        Mpf_ui_div(temp, 1, temp)
        Mpf_add(e, e, temp)
    Wend

End Sub

' ------=< MAIN >=------

Dim As UInteger prec = 50  ' precision = 50 digits
Dim As ZString Ptr outtext = Callocate (prec + 10)
Mpf_set_default_prec(prec * 3.5)
Dim As Mpf_ptr e
e = Allocate(Len(__mpf_struct)) : Mpf_init(e)
value_of_e(e)

Gmp_sprintf(outtext,"%.*Ff", prec, e)

Print "The value of e = "; *outtext

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
The value of e = 2.71828182845904523536028747135266249775724709369996
```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "math"
)

const epsilon = 1.0e-15

func main() {
    fact := uint64(1)
    e := 2.0
    n := uint64(2)
    for {
        e0 := e
        fact *= n
        n++
        e += 1.0 / float64(fact)
        if math.Abs(e - e0) < epsilon {
            break
        }
    }
    fmt.Printf("e = %.15f\n", e)
}
```


{{out}}

```txt

e = 2.718281828459046

```



## Free Pascal

''See [[#Pascal|Pascal]]''


## Groovy

'''Solution: '''

If the difference between previous and next iteration is less than the tolerance (ε) we judge that the sequence of partial sums has converged "enough".

Since the difference between partial sums is always the "last" term, it suffices to ensure that the "last" term is less than the tolerance.

```groovy
def ε = 1.0e-15
def φ = 1/ε

def generateAddends = {
    def addends = []
    def n = 0.0
    def fact = 1.0
    while (true) {
        fact *= (n < 2 ? 1.0 : n) as double
        addends << 1.0/fact
        if (fact > φ) break // any further addends would not pass the tolerance test
        n++
    }
    addends.sort(false) // smallest addends first for better response to rounding error
}

def e = generateAddends().sum()
```

'''Test: '''

```groovy
printf "%17.15f\n%17.15f\n", e, Math.E
```

'''Output: '''

```txt
2.718281828459045
2.718281828459045
```



## Haskell

For the purposes of 64 bit floating point precision, the value seems to stabilise after summing c. 17-20 terms.


```haskell
eApprox :: Double
eApprox = foldr ((+) . (1 /)) 0 (scanl (*) 1 [1 .. 20])

main :: IO ()
main = print eApprox
```

{{Out}}

```txt
2.7182818284590455
```


Or equivalently, in a single fold:


```haskell
import Data.List

eApprox2 :: Double
eApprox2 =
  fst $
  foldl' --' strict variant of foldl
    (\(e, fl) x ->
        let flx = fl * x
        in (e + (1 / flx), flx))
    (1, 1)
    [1 .. 20]

main :: IO ()
main = print eApprox2
```

{{Out}}

```txt
2.7182818284590455
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "e.bas"
110 LET E1=0:LET E,N,N1=1
120 DO WHILE E<>E1
130   LET E1=E:LET E=E+1/N
140   LET N1=N1+1:LET N=N*N1
150 LOOP
160 PRINT "The value of e =";E
```

{{Out}}

```txt
The value of e = 2.71828183
```



## J

Ken Iverson recognized that numbers are fairly useful and common, even in programming.  The j language has expressive notations for numbers.  Examples:

```txt

   NB. rational one half times pi to the first power
            NB. pi to the power of negative two
                      NB. two oh in base 111
                                   NB. complex number length 1, angle in degrees 180
   1r2p1    1p_2      111b20       1ad270
1.5708 0.101321 222 0j_1

```


It won't surprise you that in j we can write

```txt

   1x1  NB. 1 times e^1
2.71828

```


The unary power verb ^ uses Euler's number as the base, hence

```txt

   ^ 1
2.71828

```


Finally, to compute e find the sum as insert plus +/ of the reciprocals % of factorials ! of integers i. .  Using x to denote extended precision integers j will give long precision decimal expansions of rational numbers.  Format ": several expansions to verify the number of valid digits to the expansion.  Let's try for arbitrary digits.

```txt

   NB. approximation to e as a rational number
   NB. note the "r" separating numerator from denominator
   +/ % ! i. x: 20
82666416490601r30411275102208

   NB. 31 places shown with 20 terms
   32j30 ": +/ % ! i. x: 20
2.718281828459045234928752728335

   NB. 40 terms
   32j30 ": +/ % ! i. x: 40
2.718281828459045235360287471353

   NB. 50 terms,
   32j30 ": +/ % ! i. x: 50
2.718281828459045235360287471353


   NB. verb to compute e as a rational number
   e =: [: +/ [: % [: ! [: i. x:

   NB. format for e to so many places
   places =: >: j. <:

   NB. verb f computes e for y terms  and formats it in x decimal places
   f =: (":~ places)~ e

   While =: conjunction def 'u^:(0~:v)^:_'


   NB. return number of terms and the corresponding decimal representation
   e_places =: ({: , {.)@:(((f n) ; {.@:] , <@:(>:@:n@:]))While([: ~:/ 2 {. ]) '0' ; '1'&;)&1

   e_places 1
┌─┬──┐
│5│ 3│
└─┴──┘

   e_places 4
┌─┬─────┐
│9│2.718│
└─┴─────┘

   e_places 40
┌──┬─────────────────────────────────────────┐
│37│2.718281828459045235360287471352662497757│
└──┴─────────────────────────────────────────┘


```



## Java

{{trans|Kotlin}}

```java
public class CalculateE {
    public static final double EPSILON = 1.0e-15;

    public static void main(String[] args) {
        long fact = 1;
        double e = 2.0;
        int n = 2;
        double e0;
        do {
            e0 = e;
            fact *= n++;
            e += 1.0 / fact;
        } while (Math.abs(e - e0) >= EPSILON);
        System.out.printf("e = %.15f\n", e);
    }
}
```

{{out}}

```txt
e = 2.718281828459046
```



## Javascript


```javascript
(() => {
    'use strict';

    const e = () =>
        sum(map(x => 1 / x,
            scanl(
                (a, x) => a * x,
                1,
                enumFromToInt(1, 20)
            )
        ));

    // GENERIC FUNCTIONS ----------------------------------

    // enumFromToInt :: Int -> Int -> [Int]
    const enumFromToInt = (m, n) =>
        n >= m ? (
            iterateUntil(x => x >= n, x => 1 + x, m)
        ) : [];

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        let vs = [x],
            h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // scanl :: (b -> a -> b) -> b -> [a] -> [b]
    const scanl = (f, startValue, xs) =>
        xs.reduce((a, x) => {
            const v = f(a.acc, x);
            return {
                acc: v,
                scan: a.scan.concat(v)
            };
        }, {
            acc: startValue,
            scan: [startValue]
        })
        .scan;

    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // MAIN -----------------------------------------------
    return e();
})();
```


```txt
2.7182818284590455
```



## Julia

{{works with|Julia|0.6}}

'''Module''':

```julia
module NeperConstant

export NeperConst

struct NeperConst{T}
    val::T
end

Base.show(io::IO, nc::NeperConst{T}) where T = print(io, "ℯ (", T, ") = ", nc.val)

function NeperConst{T}() where T
    local e::T  = 2.0
    local e2::T = 1.0
    local den::(T ≡ BigFloat ? BigInt : Int128) = 1
    local n::typeof(den) = 2
    while e ≠ e2
        e2 = e
        den *= n
        n += one(n)
        e += 1.0 / den
    end
    return NeperConst{T}(e)
end

end  # module NeperConstant
```


'''Main''':

```julia
for F in (Float16, Float32, Float64, BigFloat)
    println(NeperConst{F}())
end
```


{{out}}

```txt
(Float16) 2.717
(Float32) 2.718282
(Float64) 2.7182818284590455
(BigFloat) 2.718281828459045235360287471352662497757247093699959574966967627724076630353416
```



## K


```K

/ Computing value of e
/ ecomp.k
\p 17
fact: {*/1+!:x}
evalue:{1 +/(1.0%)'fact' 1+!20}
evalue[]

```


{{out}}

```txt

  \l ecomp
2.7182818284590455

```



## Kotlin


```scala
// Version 1.2.40

import kotlin.math.abs

const val EPSILON = 1.0e-15

fun main(args: Array<String>) {
    var fact = 1L
    var e = 2.0
    var n = 2
    do {
        val e0 = e
        fact *= n++
        e += 1.0 / fact
    }
    while (abs(e - e0) >= EPSILON)
    println("e = %.15f".format(e))
}
```


{{output}}

```txt

e = 2.718281828459046

```



## Lua


```lua
EPSILON = 1.0e-15;

fact = 1
e = 2.0
e0 = 0.0
n = 2

repeat
    e0 = e
    fact = fact * n
    n = n + 1
    e = e + 1.0 / fact
until (math.abs(e - e0) < EPSILON)

io.write(string.format("e = %.15f\n", e))
```

{{out}}

```txt
e = 2.718281828459046
```



## M2000 Interpreter

Using @ for Decimal, and ~ for Float, # for Currency (Double is the default type for M2000)


```M2000 Interpreter

Module FindE {
      Function comp_e (n){
           \\ max 28 for decimal (in one line with less spaces)
           n/=28:For i=27to 1:n=1+n/i:Next i:=n
      }
      Clipboard Str$(comp_e(1@),"")+" Decimal"+{
      }+Str$(comp_e(1),"")+" Double"+{
      }+Str$(comp_e(1~),"")+" Float"+{
      }+Str$(comp_e(1#),"")+" Currency"+{
      }
      Report Str$(comp_e(1@),"")+" Decimal"+{
      }+Str$(comp_e(1),"")+" Double"+{
      }+Str$(comp_e(1~),"")+" Float"+{
      }+Str$(comp_e(1#),"")+" Currency"+{
      }
}
FindE

```


{{out}}

```txt

2.7182818284590452353602874712 Decimal
2.71828182845905 Double
2.718282 Float
2.7183 Currency
</pre >

As a lambda function (also we use a faster For, using block {})

```M2000 Interpreter

      comp_e=lambda (n)->{n/=28:For i=27to 1 {n=1+n/i}:=n}

```



## Mathematica



```Mathematica
1+Fold[1.+#1/#2&,1,Range[10,2,-1]]
```


{{output}}

```txt

2.7182818261984928652

```



```Mathematica
Sum[1/x!, {x, 0, ∞}]
```


```Mathematica
Limit[(1+1/x)^x,x->∞]
```


```Mathematica
Exp[1]
```

or even just

```Mathematica
𝕖
```

input as
```Mathematica
≡ee≡
```


{{output}}

```txt

𝕖

```



## min

{{works with|min|0.19.3}}

```min
(:n (n 0 ==) ((0)) (-1 () ((succ dup) dip append) n times) if) :iota
(iota 'succ '* map-reduce) :factorial

20 iota (factorial 1 swap /) '+ map-reduce print
```

{{out}}

```txt

2.718281828459046

```


=={{header|Modula-2}}==

```modula2
MODULE CalculateE;
FROM RealStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

CONST EPSILON = 1.0E-15;

PROCEDURE abs(n : REAL) : REAL;
BEGIN
    IF n < 0.0 THEN
        RETURN -n
    END;
    RETURN n
END abs;

VAR
    buf : ARRAY[0..31] OF CHAR;
    fact,n : LONGCARD;
    e,e0 : LONGREAL;
BEGIN
    fact := 1;
    e := 2.0;
    n := 2;

    REPEAT
        e0 := e;
        fact := fact * n;
        INC(n);
        e := e + 1.0 / LFLOAT(fact)
    UNTIL abs(e - e0) < EPSILON;

    WriteString("e = ");
    RealToStr(e, buf);
    WriteString(buf);
    WriteLn;

    ReadChar
END CalculateE.
```



## Myrddin


```Myrrdin
use std

const main = {
   var f: uint64 = 1
   var e: flt64 = 2.0
   var e0: flt64 = 0.0
   var n = 2

   while e > e0
      e0 = e
      f *= n
      e += 1.0 / (f : flt64)
      n++
   ;;
   std.put("e: {}\n", e)
}
```



## Nim


```nim
const epsilon : float64 = 1.0e-15
var fact : int64 = 1
var e : float64 = 2.0
var e0 : float64 = 0.0
var n : int64 = 2

while abs(e - e0) >= epsilon:
  e0 = e
  fact = fact * n
  inc(n)
  e = e + 1.0 / fact.float64

echo e
```



## Pascal

The <tt>exp</tt> function is part of the language specification, thus ''has'' to exist.

```pascal
program euler(input, output, stdErr);
var
	e: real;
begin
	e := exp(1);
end.
```



## Perl

With the <code>bignum</code> core module in force, Brother's algorithm requires only 18 iterations to match the precision of the built-in value, <code>e</code>.

```perl
use bignum qw(e);

$e = 2;
$f = 1;
do {
    $e0 = $e;
    $n++;
    $f *= 2*$n * (1 + 2*$n);
    $e += (2*$n + 2) / $f;
} until ($e-$e0) < 1.0e-39;

print "Computed " . substr($e, 0, 41), "\n";
print "Built-in " . e, "\n";
```

{{out}}

```txt
Computed 2.718281828459045235360287471352662497757
Built-in 2.718281828459045235360287471352662497757
```


To calculate 𝑒 to an arbitrary precision, enable the <code>bigrat</code> core module evaluate the Taylor series as a rational number,
then use <code>Math::Decimal</code> do to the 'long division' with the large integers.
Here, 71 terms of the Taylor series yield 𝑒 to 101 digits.


```perl
use bigrat;
use Math::Decimal qw(dec_canonise dec_mul dec_rndiv_and_rem);

sub factorial { my $n = 1; $n *= $_ for 1..shift; $n }

for $n (0..70) {
   $sum += 1/factorial($n);
}

($num,$den) = $sum =~ m#(\d+)/(\d+)#;
print "numerator:   $num\n";
print "denominator: $den\n";

$num_dec = dec_canonise($num);
$den_dec = dec_canonise($den);
$ten     = dec_canonise("10");

($q, $r) = dec_rndiv_and_rem("FLR", $num_dec, $den_dec);
$e = "$q.";
for (1..100) {
    $num_dec = dec_mul($r, $ten);
    ($q, $r) = dec_rndiv_and_rem("FLR", $num_dec, $den_dec);
    $e .= $q;
}

printf "\n%s\n", subset $e, 0,102;
```

{{out}}

```txt
numerator:   32561133701373476427912330475884581607687531065877567210421813247164172713574202714721554378508046501
denominator: 11978571669969891796072783721689098736458938142546425857555362864628009582789845319680000000000000000

2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274
```



## Perl 6

{{works with|Rakudo|2018.03}}

```perl6
# If you need high precision: Sum of a Taylor series method.
# Adjust the terms parameter to suit. Theoretically the
# terms could be ∞. Practically, calculating an infinite
# series takes an awfully long time so limit to 500.

sub postfix:<!> (Int $n) { (constant f = 1, |[\*] 1..*)[$n] }
sub 𝑒 (Int $terms) { sum map { FatRat.new(1,.!) }, ^$terms }

say 𝑒(500).comb(80).join: "\n";

say '';

# Or, if you don't need high precision, it's a built-in.
say e;
```

{{out}}

```txt
2.718281828459045235360287471352662497757247093699959574966967627724076630353547
59457138217852516642742746639193200305992181741359662904357290033429526059563073
81323286279434907632338298807531952510190115738341879307021540891499348841675092
44761460668082264800168477411853742345442437107539077744992069551702761838606261
33138458300075204493382656029760673711320070932870912744374704723069697720931014
16928368190255151086574637721112523897844250569536967707854499699679468644549059
87931636889230098793127736178215424999229576351482208269895193668033182528869398
49646510582093923982948879332036250944311730123819706841614039701983767932068328
23764648042953118023287825098194558153017567173613320698112509961818815930416903
51598888519345807273866738589422879228499892086805825749279610484198444363463244
96848756023362482704197862320900216099023530436994184914631409343173814364054625
31520961836908887070167683964243781405927145635490613031072085103837505101157477
04171898610687396965521267154688957035035402123407849819334321068170121005627880
23519303322474501585390473041995777709350366041699732972508868769664035557071622
684471625608

2.71828182845905
```



## Phix

{{trans|Python}}

```Phix
atom e0 = 0, e = 2, n = 0, fact = 1
while abs(e-e0)>=1e-15 do
    e0 = e
    n += 1
    fact *= 2*n*(2*n+1)
    e += (2*n+2)/fact
end while
printf(1,"Computed e = %.15f\n",e)
printf(1,"    Real e = %.15f\n",E)
printf(1,"     Error = %g\n",E-e)
printf(1,"Number of iterations = %d\n",n)
```

{{out}}

```txt

Computed e = 2.718281828459045
    Real e = 2.718281828459045
     Error = 4.4409e-16
Number of iterations = 9

```



## PicoLisp


```PicoLisp
(scl 15)
(let (F 1  E 2.0  E0 0  N 2)
   (while (> E E0)
      (setq E0 E  F (* F N))
      (inc 'E (*/ 1.0 F))
      (inc 'N) )
   (prinl "e = " (format E *Scl)) )
```

{{out}}

```txt
e = 2.718281828459046
```



## PowerShell

{{trans|Python}}

```powershell
$e0 = 0
$e = 2
$n = 0
$fact = 1
while([Math]::abs($e-$e0) -gt 1E-15){
   $e0 = $e
   $n += 1
   $fact *= 2*$n*(2*$n+1)
   $e += (2*$n+2)/$fact
}

Write-Host "Computed e = $e"
Write-Host "    Real e = $([Math]::Exp(1))"
Write-Host "     Error = $([Math]::Exp(1) - $e)"
Write-Host "Number of iterations = $n"
```

{{out}}

```txt
Computed e = 2.71828182845904
    Real e = 2.71828182845905
     Error = 4.44089209850063E-16
Number of iterations = 9
```



## Python


### Imperative


```python
import math
#Implementation of Brother's formula
e0 = 0
e = 2
n = 0
fact = 1
while(e-e0 > 1e-15):
	e0 = e
	n += 1
	fact *= 2*n*(2*n+1)
	e += (2.*n+2)/fact

print "Computed e = "+str(e)
print "Real e = "+str(math.e)
print "Error = "+str(math.e-e)
print "Number of iterations = "+str(n)
```

{{out}}

```txt

Computed e = 2.71828182846
Real e = 2.71828182846
Error = 4.4408920985e-16
Number of iterations = 9

```



### Functional

This approximation stabilises (within the constraints of available floating point precision) after about the 17th term of the series.

{{Works with|Python|3.7}}

```python
'''Calculating an approximate value for e'''

from itertools import (accumulate, chain)
from functools import (reduce)
from operator import (mul)


# eApprox :: () -> Float
def eApprox():
    '''Approximation to the value of e.'''
    return reduce(
        lambda a, x: a + 1 / x,
        scanl(mul)(1)(
            range(1, 18)
        ),
        0
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test'''

    print(
        eApprox()
    )


# GENERIC ABSTRACTIONS ------------------------------------

# scanl is like reduce, but returns a succession of
# intermediate values, building from the left.
# See, for example, under `scan` in the Lists chapter of
# the language-independent Bird & Wadler 1988.

# scanl :: (b -> a -> b) -> b -> [a] -> [b]
def scanl(f):
    '''scanl is like reduce, but returns a succession of
       intermediate values, building from the left.'''
    return lambda a: lambda xs: (
        accumulate(chain([a], xs), f)
    )


# MAIN ---
if __name__ == '__main__':
    main()
```


```txt
2.7182818284590455
```

{{Out}}


## R


```R

options(digits=22)
cat("e =",sum(rep(1,20)/factorial(0:19)))

```

{{Out}}

```txt

e = 2.718281828459046

```



## Racket


```racket
#lang racket
(require math/number-theory)

(define (calculate-e (terms 20))
  (apply + (map (compose / factorial) (range terms))))

(module+ main
  (let ((e (calculate-e)))
    (displayln e)
    (displayln (real->decimal-string e 20))
    (displayln (real->decimal-string (- (exp 1) e) 20))))
```

{{out}}

```txt
82666416490601/30411275102208
2.71828182845904523493
0.00000000000000000000
```



## REXX


### version 1

This REXX version uses the following formula to calculate Napier's constant   <big>''e''</big>:

  ╔═══════════════════════════════════════════════════════════════════════════════════════╗
  ║                                                                                       ║
  ║           1         1         1         1         1         1         1               ║
  ║   e  =   ───   +   ───   +   ───   +   ───   +   ───   +   ───   +   ───   +    ∙∙∙   ║
  ║           0!        1!        2!        3!        4!        5!        6!              ║
  ║                                                                                       ║
  ╚═══════════════════════════════════════════════════════════════════════════════════════╝

If the argument (digs) is negative, a running number of decimal digits of   <big>''e''</big>   is
shown.

```rexx
/*REXX pgm calculates  e  to a # of decimal digits. If digs<0, a running value is shown.*/
parse arg digs .                                 /*get optional number of decimal digits*/
if digs=='' | digs==","  then digs= 101          /*Not specified?  Then use the default.*/
numeric digits abs(digs);     w=length(digits()) /*use the absolute value of  digs.     */
                    e= 1;     q= 1               /*1st value of  e    and     q.        */
      do #=1  until e==old;   old= e             /*start calculations at the second term*/
      q= q / #                                   /*calculate the divisor for this term. */
      e= e + q                                   /*add quotient to running   e   value. */
      if digs>0  then iterate                    /*DIGS>0?  Then don't show running digs*/
      $= compare(e, old)                         /*$  is first digit not compared equal.*/
      if $>0  then say right('with', 10)    right(#+1, w)     "terms,"      right($-1, w),
            "decimal digits were calculated for   e   (Napier's constant)"     /*   ↑   */
      end   /*#*/                                /* -1  is for the decimal point────┘   */
say                                              /*stick a fork in it,  we're all done. */
say '(with'    abs(digs)      "decimal digits)   the value of   e   is:";         say e
```

Programming note:   the factorial of the   '''do'''   loop index is calculated by   ''division'',   not by the usual   ''multiplication''   (for optimization).


{{out|output|text=  when using the default input:}}

```txt

(with 101 decimal digits)   the value of   e   is:
2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274

```

{{out|output|text=  when using the input of:   <tt> -101 </tt>}}


(Shown at three-quarter size.)
<pre style="font-size:75%;height:55ex">
      with   2 terms,   0 decimal digits were calculated for   e   (Napier's constant)
      with   3 terms,   1 decimal digits were calculated for   e   (Napier's constant)
      with   4 terms,   2 decimal digits were calculated for   e   (Napier's constant)
      with   5 terms,   2 decimal digits were calculated for   e   (Napier's constant)
      with   6 terms,   3 decimal digits were calculated for   e   (Napier's constant)
      with   7 terms,   4 decimal digits were calculated for   e   (Napier's constant)
      with   8 terms,   5 decimal digits were calculated for   e   (Napier's constant)
      with   9 terms,   6 decimal digits were calculated for   e   (Napier's constant)
      with  10 terms,   6 decimal digits were calculated for   e   (Napier's constant)
      with  11 terms,   8 decimal digits were calculated for   e   (Napier's constant)
      with  12 terms,   9 decimal digits were calculated for   e   (Napier's constant)
      with  13 terms,  10 decimal digits were calculated for   e   (Napier's constant)
      with  14 terms,  11 decimal digits were calculated for   e   (Napier's constant)
      with  15 terms,  12 decimal digits were calculated for   e   (Napier's constant)
      with  16 terms,  14 decimal digits were calculated for   e   (Napier's constant)
      with  17 terms,  13 decimal digits were calculated for   e   (Napier's constant)
      with  18 terms,  16 decimal digits were calculated for   e   (Napier's constant)
      with  19 terms,  17 decimal digits were calculated for   e   (Napier's constant)
      with  20 terms,  18 decimal digits were calculated for   e   (Napier's constant)
      with  21 terms,  19 decimal digits were calculated for   e   (Napier's constant)
      with  22 terms,  21 decimal digits were calculated for   e   (Napier's constant)
      with  23 terms,  21 decimal digits were calculated for   e   (Napier's constant)
      with  24 terms,  24 decimal digits were calculated for   e   (Napier's constant)
      with  25 terms,  25 decimal digits were calculated for   e   (Napier's constant)
      with  26 terms,  27 decimal digits were calculated for   e   (Napier's constant)
      with  27 terms,  27 decimal digits were calculated for   e   (Napier's constant)
      with  28 terms,  29 decimal digits were calculated for   e   (Napier's constant)
      with  29 terms,  30 decimal digits were calculated for   e   (Napier's constant)
      with  30 terms,  32 decimal digits were calculated for   e   (Napier's constant)
      with  31 terms,  33 decimal digits were calculated for   e   (Napier's constant)
      with  32 terms,  35 decimal digits were calculated for   e   (Napier's constant)
      with  33 terms,  37 decimal digits were calculated for   e   (Napier's constant)
      with  34 terms,  38 decimal digits were calculated for   e   (Napier's constant)
      with  35 terms,  40 decimal digits were calculated for   e   (Napier's constant)
      with  36 terms,  41 decimal digits were calculated for   e   (Napier's constant)
      with  37 terms,  43 decimal digits were calculated for   e   (Napier's constant)
      with  38 terms,  45 decimal digits were calculated for   e   (Napier's constant)
      with  39 terms,  46 decimal digits were calculated for   e   (Napier's constant)
      with  40 terms,  48 decimal digits were calculated for   e   (Napier's constant)
      with  41 terms,  49 decimal digits were calculated for   e   (Napier's constant)
      with  42 terms,  51 decimal digits were calculated for   e   (Napier's constant)
      with  43 terms,  52 decimal digits were calculated for   e   (Napier's constant)
      with  44 terms,  54 decimal digits were calculated for   e   (Napier's constant)
      with  45 terms,  56 decimal digits were calculated for   e   (Napier's constant)
      with  46 terms,  57 decimal digits were calculated for   e   (Napier's constant)
      with  47 terms,  59 decimal digits were calculated for   e   (Napier's constant)
      with  48 terms,  61 decimal digits were calculated for   e   (Napier's constant)
      with  49 terms,  62 decimal digits were calculated for   e   (Napier's constant)
      with  50 terms,  64 decimal digits were calculated for   e   (Napier's constant)
      with  51 terms,  65 decimal digits were calculated for   e   (Napier's constant)
      with  52 terms,  67 decimal digits were calculated for   e   (Napier's constant)
      with  53 terms,  69 decimal digits were calculated for   e   (Napier's constant)
      with  54 terms,  71 decimal digits were calculated for   e   (Napier's constant)
      with  55 terms,  72 decimal digits were calculated for   e   (Napier's constant)
      with  56 terms,  74 decimal digits were calculated for   e   (Napier's constant)
      with  57 terms,  76 decimal digits were calculated for   e   (Napier's constant)
      with  58 terms,  78 decimal digits were calculated for   e   (Napier's constant)
      with  59 terms,  80 decimal digits were calculated for   e   (Napier's constant)
      with  60 terms,  81 decimal digits were calculated for   e   (Napier's constant)
      with  61 terms,  83 decimal digits were calculated for   e   (Napier's constant)
      with  62 terms,  84 decimal digits were calculated for   e   (Napier's constant)
      with  63 terms,  87 decimal digits were calculated for   e   (Napier's constant)
      with  64 terms,  88 decimal digits were calculated for   e   (Napier's constant)
      with  65 terms,  91 decimal digits were calculated for   e   (Napier's constant)
      with  66 terms,  92 decimal digits were calculated for   e   (Napier's constant)
      with  67 terms,  94 decimal digits were calculated for   e   (Napier's constant)
      with  68 terms,  96 decimal digits were calculated for   e   (Napier's constant)
      with  69 terms,  98 decimal digits were calculated for   e   (Napier's constant)
      with  70 terms, 100 decimal digits were calculated for   e   (Napier's constant)
      with  71 terms, 101 decimal digits were calculated for   e   (Napier's constant)

(with 101 decimal digits)   the value of   e   is:
2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274

```



### version 2

Using the series shown in version 1 compute e to the specified precision.

```rexx
/*REXX pgm calculates e to nn of decimal digits             */
Parse Arg dig             /* the desired precision          */
Numeric Digits (dig+3)    /* increase precision             */
dig2=dig+2                /* limit the loop                 */
e=1                       /* first element of the series    */
q=1                       /* next element of the series     */
Do n=1 By 1               /* start adding the elements      */
  old=e                   /* current sum                    */
  q=q/n                   /* new element                    */
  e=e+q                   /* add the new element to the sum */
  If left(e,dig2)=left(old,dig2) Then /* no change          */
    Leave                 /* we are done                    */
  End
Numeric Digits dig        /* the desired precision          */
e=e/1                     /* the desired approximation      */
Return left(e,dig+1) '('n 'iterations required)'
```

{{out}}

```txt
J:\>rexx eval compey(66)
compey(66)=2.71828182845904523536028747135266249775724709369995957496696762772 (52 iterations required)
```

Check the function's correctness

```rexx
 /*REXX check the correctness of compey */
e_='2.7182818284590452353602874713526624977572470936999595749669676277240'||,
   '766303535475945713821785251664274274663919320030599218174135966290435'||,
   '729003342952605956307380251882050351967424723324653614466387706813388353430034'
ok=0
Do d=3 To 100
  Parse Value compey(d) with e .
  Numeric digits d
  If e<>e_/1 Then Do
    say d e
    Say e
    Say e_/1
    End
  Else ok=ok+1
  End
Say ok 'comparisons are ok'
```

{{out}}

```txt
J:\>rexx compez
98 comparisons are ok
```



## Ring


```ring

# Project : Calculating the value of e

decimals(14)

for n = 1 to 100000
     e = pow((1 + 1/n),n)
next
see "Calculating the value of e with method #1:" + nl
see "e = " + e + nl

e = 0
for n = 0 to 12
     e = e + (1 / factorial(n))
next
see "Calculating the value of e with method #2:" + nl
see "e = " + e + nl

func factorial(n)
       if n = 0 or n = 1
          return 1
       else
          return n * factorial(n-1)
       ok

```

Output:

```txt

Calculating the value of e with method #1:
e = 2.71826823719230
Calculating the value of e with method #2:
e = 2.71828182828617

```



## Ruby

{{trans|C}}

```ruby

fact = 1
e = 2
e0 = 0
n = 2

until (e - e0).abs < Float::EPSILON do
  e0 = e
  fact *= n
  n += 1
  e += 1.0 / fact
end

puts e

```

Built in:

```ruby
require "bigdecimal/math"

puts BigMath.E(50).to_s # 50 decimals

```

{{out}}

```txt

0.27182818284590452353602874713526624977572470937e1

```



## Rust


```Rust
const EPSILON: f64 = 1e-15;

fn main() {
    let mut fact: u64 = 1;
    let mut e: f64 = 2.0;
    let mut n: u64 = 2;
    loop {
        let e0 = e;
        fact *= n;
        n += 1;
        e += 1.0 / fact as f64;
        if (e - e0).abs() < EPSILON {
            break;
        }
    }
    println!("e = {:.15}", e);
}
```

{{out}}

```txt
e = 2.718281828459046
```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/gLmNcH2/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/WSvNG9xMT5GcugVTvqogVg Scastie (remote JVM)].

```Scala
import scala.annotation.tailrec

object CalculateE extends App {
  private val ε = 1.0e-15

  @tailrec
  def iter(fact: Long, ℯ: Double, n: Int, e0: Double): Double = {
    val newFact = fact * n
    val newE = ℯ + 1.0 / newFact
    if (math.abs(newE - ℯ) < ε) ℯ
    else iter(newFact, newE, n + 1, ℯ)
  }

  println(f"ℯ = ${iter(1L, 2.0, 2, 0)}%.15f")
}
```



## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/math.htm math.s7i] defines
the constant [http://seed7.sourceforge.net/libraries/math.htm#E E].
The program below computes e:


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const float: EPSILON is 1.0e-15;

const proc: main is func
  local
    var integer: fact is 1;
    var float: e is 2.0;
    var float: e0 is 0.0;
    var integer: n is 2;
  begin
    repeat
      e0 := e;
      fact *:= n;
      incr(n);
      e +:= 1.0 / flt(fact);
    until abs(e - e0) < EPSILON;
    writeln("e = " <& e digits 15);
  end func;
```


{{out}}

```txt

e = 2.718281828459046

```



## Sidef



```ruby
func calculate_e(n=50) {
    sum(0..n, {|k| 1/k! })
}

say calculate_e()
say calculate_e(69).as_dec(100)
```

{{out}}

```txt

2.7182818284590452353602874713526624977572470937
2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427

```


For finding the number of required terms for calculating ''e'' to a given number of decimal places, using the formula '''Sum_{k=0..n} 1/k!''', we have:


```ruby
func f(n) {
    var t = n*log(10)
    (n + 10).bsearch_le { |k|
        lngamma(k+1) <=> t
    }
}

for k in (1..10) {
    var n = f(10**k)
    say "Sum_{k=0..#{n}} 1/k! = e correct to #{10**k->commify} decimal places"
}
```

{{out}}

```txt

Sum_{k=0..13} 1/k! = e correct to 10 decimal places
Sum_{k=0..69} 1/k! = e correct to 100 decimal places
Sum_{k=0..449} 1/k! = e correct to 1,000 decimal places
Sum_{k=0..3248} 1/k! = e correct to 10,000 decimal places
Sum_{k=0..25205} 1/k! = e correct to 100,000 decimal places
Sum_{k=0..205022} 1/k! = e correct to 1,000,000 decimal places
Sum_{k=0..1723507} 1/k! = e correct to 10,000,000 decimal places
Sum_{k=0..14842906} 1/k! = e correct to 100,000,000 decimal places
Sum_{k=0..130202808} 1/k! = e correct to 1,000,000,000 decimal places
Sum_{k=0..1158787577} 1/k! = e correct to 10,000,000,000 decimal places

```


## Standard ML


```sml
fun calcEToEps() =
  let
    val eps = 1.0e~15
    fun calcToEps'(eest: real, prev: real, denom, i) =
      if Real.abs(eest - prev) < eps then
        eest
      else
        let
          val denom' = denom * i;
          val prev' = eest
        in
          calcToEps'(eest + 1.0/denom', prev', denom', i + 1.0)
        end
  in
    calcToEps'(2.0, 1.0, 1.0, 2.0)
  end;
```


{{out}}


```txt
- val eEst = calcEToEps();
val eEst = 2.71828182846 : real
- Math.e - eEst;
val it = ~4.4408920985E~16 : real

```



## Swift


{{trans|C}}


```swift
import Foundation


func calculateE(epsilon: Double = 1.0e-15) -> Double {
  var fact: UInt64 = 1
  var e = 2.0, e0 = 0.0
  var n = 2

  repeat {
    e0 = e
    fact *= UInt64(n)
    n += 1
    e += 1.0 / Double(fact)
  } while fabs(e - e0) >= epsilon

  return e
}

print(String(format: "e = %.15f\n", arguments: [calculateE()]))
```


{{out}}


```txt
e = 2.718281828459046
```



## Tcl


```tcl

set ε 1.0e-15
set fact 1
set e 2.0
set e0 0.0
set n 2

while {[expr abs($e - $e0)] > ${ε}} {
  set e0 $e
  set fact [expr $fact * $n]
  incr n
  set e [expr $e + 1.0/$fact]
}
puts "e = $e"
```

{{Out}}

```txt

e = 2.7182818284590455

```



## VBScript

{{Trans|Python}}

```vb
e0 = 0 : e = 2 : n = 0 : fact = 1
While (e - e0) > 1E-15
	e0 = e
	n = n + 1
	fact = fact * 2*n * (2*n + 1)
	e = e + (2*n + 2)/fact
Wend

WScript.Echo "Computed e = " & e
WScript.Echo "Real e = " & Exp(1)
WScript.Echo "Error = " & (Exp(1) - e)
WScript.Echo "Number of iterations = " & n
```

{{Out}}

```txt
Computed e = 2.71828182845904
Real e = 2.71828182845905
Error = 4.44089209850063E-16
Number of iterations = 9
```



## Visual Basic .NET

{{trans|C#}}
{{libheader|System.Numerics}}Automatically determines number of padding digits required for the arbitrary precision output.  Can calculate a quarter million digits of '''''e''''' in under a half a minute.

```vbnet
Imports System, System.Numerics, System.Math, System.Console

Module Program
    Function CalcE(ByVal nDigs As Integer) As String
        Dim pad As Integer = Round(Log10(nDigs)), n = 1,
            f As BigInteger = BigInteger.Pow(10, nDigs + pad), e = f + f
        Do : n+= 1 : f /= n : e += f : Loop While f > n
        Return (e / BigInteger.Pow(10, pad + 1)).ToString().Insert(1, ".")
    End Function

    Sub Main()
        WriteLine(Exp(1))  '  double precision built-in function
        WriteLine(CalcE(100))   '  arbitrary precision result
        Dim st As DateTime = DateTime.Now, qmil As Integer = 250_000,
            es As String = CalcE(qmil)  '  large arbitrary precision result string
        WriteLine("{0:n0} digits in {1:n3} seconds.", qmil, (DateTime.Now - st).TotalSeconds)
        WriteLine("partial: {0}...{1}", es.Substring(0, 46), es.Substring(es.Length - 45))
    End Sub
End Module
```

{{out}}

```txt
2.71828182845905
2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427
250,000 digits in 22.559 seconds.
partial: 2.71828182845904523536028747135266249775724709...026587951482508371108187783411598287506586313
```



## zkl

{{trans|C}}

```zkl
const EPSILON=1.0e-15;
fact,e,n := 1, 2.0, 2;
do{
   e0:=e;
   fact*=n; n+=1;
   e+=1.0/fact;
}while((e - e0).abs() >= EPSILON);
println("e = %.15f".fmt(e));
```

{{out}}

```txt

e = 2.718281828459046

```



## ZX Spectrum Basic



```zxbasic
10 LET p=13: REM precision, or the number of terms in the Taylor expansion, from 0 to 33...
20 LET k=1: REM ...the Spectrum's maximum expressible precision is reached at p=13, while...
30 LET e=0: REM ...the factorial can't go any higher than 33
40 FOR x=1 TO p
50 LET e=e+1/k
60 LET k=k*x
70 NEXT x
80 PRINT e
90 PRINT e-EXP 1: REM the Spectrum ROM uses Chebyshev polynomials to evaluate EXP x = e^x
```


{{Out}}

```txt

2.7182818
9.3132257E-10

```

