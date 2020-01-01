+++
title = "Continued fraction/Arithmetic/Construct from rational number"
description = ""
date = 2019-10-20T06:45:32Z
aliases = []
[extra]
id = 12894
[taxonomies]
categories = []
tags = []
+++

{{task}}To understand this task in context please see [[Continued fraction arithmetic]]

The purpose of this task is to write a function <math>\mathit{r2cf}(\mathrm{int}</math> <math>N_1, \mathrm{int}</math> <math>N_2)</math>, or <math>\mathit{r2cf}(\mathrm{Fraction}</math> <math>N)</math>, which will output a continued fraction assuming:
:<math>N_1</math> is the numerator
:<math>N_2</math> is the denominator

The function should output its results one digit at a time each time it is called, in a manner sometimes described as lazy evaluation.

To achieve this it must determine: the integer part; and remainder part, of <math>N_1</math> divided by <math>N_2</math>. It then sets <math>N_1</math> to <math>N_2</math> and <math>N_2</math> to the determined remainder part. It then outputs the determined integer part. It does this until <math>\mathrm{abs}(N_2)</math> is zero.

Demonstrate the function by outputing the continued fraction for:
: 1/2
: 3
: 23/8
: 13/11
: 22/7
: -151/77
<math>\sqrt 2</math> should approach <math>[1; 2, 2, 2, 2, \ldots]</math> try ever closer rational approximations until boredom gets the better of you:
: 14142,10000
: 141421,100000
: 1414214,1000000
: 14142136,10000000

Try :
: 31,10
: 314,100
: 3142,1000
: 31428,10000
: 314285,100000
: 3142857,1000000
: 31428571,10000000
: 314285714,100000000

Observe how this rational number behaves differently to <math>\sqrt 2</math> and convince yourself that, in the same way as <math>3.7</math> may be represented as <math>3.70</math> when an extra decimal place is required, <math>[3;7]</math> may be represented as <math>[3;7,\infty]</math> when an extra term is required.


## C

C does not implement Lazy evaluation and it is this particular feature which is the real challenge of this particular example. It can however be simulated. The following example uses pointers. It seems that the same data is being passed but since the function accepts pointers, the variables are being changed. One other way to simulate laziness would be to use global variables. Then although it would seem that the same values are being passed even as constants, the job is actually getting done. In my view, that would be plain cheating.


```C

#include<stdio.h>

typedef struct{
	int num,den;
	}fraction;

fraction examples[] = {{1,2}, {3,1}, {23,8}, {13,11}, {22,7}, {-151,77}};
fraction sqrt2[] = {{14142,10000}, {141421,100000}, {1414214,1000000}, {14142136,10000000}};
fraction pi[] = {{31,10}, {314,100}, {3142,1000}, {31428,10000}, {314285,100000}, {3142857,1000000}, {31428571,10000000}, {314285714,100000000}};

int r2cf(int *numerator,int *denominator)
{
	int quotient=0,temp;

	if(denominator != 0)
	{
		quotient = *numerator / *denominator;

		temp = *numerator;

		*numerator = *denominator;

		*denominator = temp % *denominator;
	}

	return quotient;
}

int main()
{
	int i;

	printf("Running the examples :");

	for(i=0;i<sizeof(examples)/sizeof(fraction);i++)
	{
		printf("\nFor N = %d, D = %d :",examples[i].num,examples[i].den);

		while(examples[i].den != 0){
			printf(" %d ",r2cf(&examples[i].num,&examples[i].den));
			}
	}

	printf("\n\nRunning for %c2 :",251); /* 251 is the ASCII code for the square root symbol */

	for(i=0;i<sizeof(sqrt2)/sizeof(fraction);i++)
	{
		printf("\nFor N = %d, D = %d :",sqrt2[i].num,sqrt2[i].den);

		while(sqrt2[i].den != 0){
			printf(" %d ",r2cf(&sqrt2[i].num,&sqrt2[i].den));
			}
	}

	printf("\n\nRunning for %c :",227); /* 227 is the ASCII code for Pi's symbol */

	for(i=0;i<sizeof(pi)/sizeof(fraction);i++)
	{
		printf("\nFor N = %d, D = %d :",pi[i].num,pi[i].den);

		while(pi[i].den != 0){
			printf(" %d ",r2cf(&pi[i].num,&pi[i].den));
			}
	}



	return 0;
}


```

And the run gives :

```txt

Running the examples :
For N = 1, D = 2 : 0  2
For N = 3, D = 1 : 3
For N = 23, D = 8 : 2  1  7
For N = 13, D = 11 : 1  5  2
For N = 22, D = 7 : 3  7
For N = -151, D = 77 : -1  -1  -24  -1  -2

Running for √2 :
For N = 14142, D = 10000 : 1  2  2  2  2  2  1  1  29
For N = 141421, D = 100000 : 1  2  2  2  2  2  2  3  1  1  3  1  7  2
For N = 1414214, D = 1000000 : 1  2  2  2  2  2  2  2  3  6  1  2  1  12
For N = 14142136, D = 10000000 : 1  2  2  2  2  2  2  2  2  2  6  1  2  4  1  1  2

Running for π :
For N = 31, D = 10 : 3  10
For N = 314, D = 100 : 3  7  7
For N = 3142, D = 1000 : 3  7  23  1  2
For N = 31428, D = 10000 : 3  7  357
For N = 314285, D = 100000 : 3  7  2857
For N = 3142857, D = 1000000 : 3  7  142857
For N = 31428571, D = 10000000 : 3  7  476190  3
For N = 314285714, D = 100000000 : 3  7  7142857

```



## C++


```cpp
#include <iostream>
/* Interface for all Continued Fractions
   Nigel Galloway, February 9th., 2013.
*/
class ContinuedFraction {
	public:
	virtual const int nextTerm(){};
	virtual const bool moreTerms(){};
};
/* Create a continued fraction from a rational number
   Nigel Galloway, February 9th., 2013.
*/
class r2cf : public ContinuedFraction {
	private: int n1, n2;
	public:
	r2cf(const int numerator, const int denominator): n1(numerator), n2(denominator){}
	const int nextTerm() {
		const int thisTerm = n1/n2;
		const int t2 = n2; n2 = n1 - thisTerm * n2; n1 = t2;
		return thisTerm;
	}
	const bool moreTerms() {return fabs(n2) > 0;}
};
/* Generate a continued fraction for sqrt of 2
   Nigel Galloway, February 9th., 2013.
*/
class SQRT2 : public ContinuedFraction {
	private: bool first=true;
	public:
	const int nextTerm() {if (first) {first = false; return 1;} else return 2;}
	const bool moreTerms() {return true;}
};
```


### Testing

====1/2 3 23/8 13/11 22/7 -151/77====

```cpp
int main() {
	for(r2cf n(1,2); n.moreTerms(); std::cout << n.nextTerm() << " ");
	std::cout << std::endl;
	for(r2cf n(3,1); n.moreTerms(); std::cout << n.nextTerm() << " ");
	std::cout << std::endl;
	for(r2cf n(23,8); n.moreTerms(); std::cout << n.nextTerm() << " ");
	std::cout << std::endl;
	for(r2cf n(13,11); n.moreTerms(); std::cout << n.nextTerm() << " ");
	std::cout << std::endl;
	for(r2cf n(22,7); n.moreTerms(); std::cout << n.nextTerm() << " ");
	std::cout << std::endl;
	for(r2cf cf(-151,77); cf.moreTerms(); std::cout << cf.nextTerm() << " ");
	std::cout << std::endl;
	return 0;
}
```

{{out}}

```txt

0 2
3
2 1 7
1 5 2
3 7
-1 -1 -24 -1 -2

```


### =<math>\sqrt 2</math>=


```cpp
int main() {
	int i = 0;
	for(SQRT2 n; i++ < 20; std::cout << n.nextTerm() << " ");
	std::cout << std::endl;
	for(r2cf n(14142,10000); n.moreTerms(); std::cout << n.nextTerm() << " ");
	std::cout << std::endl;
	for(r2cf n(14142136,10000000); n.moreTerms(); std::cout << n.nextTerm() << " ");
	std::cout << std::endl;
	return 0;
}
```

{{out}}

```txt

1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
1 2 2 2 2 2 1 1 29
1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2

```


### =Real approximations of a rational number=


```cpp
int main() {
  for(r2cf n(31,10); n.moreTerms(); std::cout << n.nextTerm() << " ");
  std::cout << std::endl;
  for(r2cf n(314,100); n.moreTerms(); std::cout << n.nextTerm() << " ");
  std::cout << std::endl;
  for(r2cf n(3142,1000); n.moreTerms(); std::cout << n.nextTerm() << " ");
  std::cout << std::endl;
  for(r2cf n(31428,10000); n.moreTerms(); std::cout << n.nextTerm() << " ");
  std::cout << std::endl;
  for(r2cf n(314285,100000); n.moreTerms(); std::cout << n.nextTerm() << " ");
  std::cout << std::endl;
  for(r2cf n(3142857,1000000); n.moreTerms(); std::cout << n.nextTerm() << " ");
  std::cout << std::endl;
  for(r2cf n(31428571,10000000); n.moreTerms(); std::cout << n.nextTerm() << " ");
  std::cout << std::endl;
  for(r2cf n(314285714,100000000); n.moreTerms(); std::cout << n.nextTerm() << " ");
  std::cout << std::endl;
  return 0;
}
```

{{out}}

```txt

3 10
3 7 7
3 7 23 1 2
3 7 357
3 7 2857
3 7 142857
3 7 476190 3
3 7 7142857

```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Collections.Generic;

class Program
{
    static IEnumerable<int> r2cf(int n1, int n2)
    {
        while (Math.Abs(n2) > 0)
        {
            int t1 = n1 / n2;
            int t2 = n2;
            n2 = n1 - t1 * n2;
            n1 = t2;
            yield return t1;
        }
    }

    static void spit(IEnumerable<int> f)
    {
        foreach (int n in f) Console.Write(" {0}", n);
        Console.WriteLine();
    }

    static void Main(string[] args)
    {
        spit(r2cf(1, 2));
        spit(r2cf(3, 1));
        spit(r2cf(23, 8));
        spit(r2cf(13, 11));
        spit(r2cf(22, 7));
        spit(r2cf(-151, 77));
        for (int scale = 10; scale <= 10000000; scale *= 10)
        {
            spit(r2cf((int)(Math.Sqrt(2) * scale), scale));
        }
        spit(r2cf(31, 10));
        spit(r2cf(314, 100));
        spit(r2cf(3142, 1000));
        spit(r2cf(31428, 10000));
        spit(r2cf(314285, 100000));
        spit(r2cf(3142857, 1000000));
        spit(r2cf(31428571, 10000000));
        spit(r2cf(314285714, 100000000));
    }
}

```

Output

```txt

 0 2
 3
 2 1 7
 1 5 2
 3 7
 -1 -1 -24 -1 -2
 1 2 2
 1 2 2 3 1 1 2
 1 2 2 2 2 5 3
 1 2 2 2 2 2 1 1 29
 1 2 2 2 2 2 2 3 1 1 3 1 7 2
 1 2 2 2 2 2 2 2 1 1 4 1 1 1 1 1 2 1 6
 1 2 2 2 2 2 2 2 2 2 1 594
 3 10
 3 7 7
 3 7 23 1 2
 3 7 357
 3 7 2857
 3 7 142857
 3 7 476190 3
 3 7 7142857
```



## Clojure


```clojure
(defn r2cf [n d]
  (if-not (= d 0) (cons (quot n d) (lazy-seq (r2cf d (rem n d))))))

; Example usage
(def demo '((1 2)
            (3 1)
            (23 8)
            (13 11)
            (22 7)
            (-151 77)
            (14142 10000)
            (141421 100000)
            (1414214 1000000)
            (14142136 10000000)
            (31 10)
            (314 100)
            (3142 1000)
            (31428 10000)
            (314285 100000)
            (3142857 1000000)
            (31428571 10000000)
            (314285714 100000000)
            (3141592653589793 1000000000000000)))

(doseq [inputs demo
        :let [outputs (r2cf (first inputs) (last inputs))]]
  (println inputs ";" outputs))
```


{{out}}

```txt

(1 2) ; (0 2)
(3 1) ; (3)
(23 8) ; (2 1 7)
(13 11) ; (1 5 2)
(22 7) ; (3 7)
(-151 77) ; (-1 -1 -24 -1 -2)
(14142 10000) ; (1 2 2 2 2 2 1 1 29)
(141421 100000) ; (1 2 2 2 2 2 2 3 1 1 3 1 7 2)
(1414214 1000000) ; (1 2 2 2 2 2 2 2 3 6 1 2 1 12)
(14142136 10000000) ; (1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2)
(31 10) ; (3 10)
(314 100) ; (3 7 7)
(3142 1000) ; (3 7 23 1 2)
(31428 10000) ; (3 7 357)
(314285 100000) ; (3 7 2857)
(3142857 1000000) ; (3 7 142857)
(31428571 10000000) ; (3 7 476190 3)
(314285714 100000000) ; (3 7 7142857)
(3141592653589793 1000000000000000) ; (3 7 15 1 292 1 1 1 2 1 3 1 14 4 2 3 1 12 5 1 5 20 1 11 1 1 1 2)

```



## Common Lisp


```lisp
(defun r2cf (n1 n2)
  (lambda ()
    (unless (zerop n2)
      (multiple-value-bind (t1 r)
          (floor n1 n2)
        (setf n1 n2 n2 r)
        t1))))

;; Example usage

(defun demo-generator (numbers)
  (let* ((n1 (car numbers))
         (n2 (cadr numbers))
         (gen (r2cf n1 n2)))
    (format t "~S  ; ~S~%"
            `(r2cf ,n1 ,n2)
            (loop
              :for r = (funcall gen)
              :until (null r)
              :collect r))))

(mapcar #'demo-generator
        '((1 2)
          (3 1)
          (23 8)
          (13 11)
          (22 7)
          (-151 77)
          (14142 10000)
          (141421 100000)
          (1414214 1000000)
          (14142136 10000000)
          (31 10)
          (314 100)
          (3142 1000)
          (31428 10000)
          (314285 100000)
          (3142857 1000000)
          (31428571 10000000)
          (314285714 100000000)
          (3141592653589793 1000000000000000)))
```


Output:

```txt

(R2CF 3 1)  ; (3)
(R2CF 23 8)  ; (2 1 7)
(R2CF 13 11)  ; (1 5 2)
(R2CF 22 7)  ; (3 7)
(R2CF -151 77)  ; (-2 25 1 2)
(R2CF 14142 10000)  ; (1 2 2 2 2 2 1 1 29)
(R2CF 141421 100000)  ; (1 2 2 2 2 2 2 3 1 1 3 1 7 2)
(R2CF 1414214 1000000)  ; (1 2 2 2 2 2 2 2 3 6 1 2 1 12)
(R2CF 14142136 10000000)  ; (1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2)
(R2CF 31 10)  ; (3 10)
(R2CF 314 100)  ; (3 7 7)
(R2CF 3142 1000)  ; (3 7 23 1 2)
(R2CF 31428 10000)  ; (3 7 357)
(R2CF 314285 100000)  ; (3 7 2857)
(R2CF 3142857 1000000)  ; (3 7 142857)
(R2CF 31428571 10000000)  ; (3 7 476190 3)
(R2CF 314285714 100000000)  ; (3 7 7142857)
(R2CF 3141592653589793 1000000000000000)  ; (3 7 15 1 292 1 1 1 2 1 3 1 14 4 2 3 1 12 5 1 5 20 1 11 1 1 1 2)

```



## D

{{trans|Kotlin}}

```D
import std.concurrency;
import std.stdio;

struct Pair {
    int first, second;
}

auto r2cf(Pair frac) {
    return new Generator!int({
        auto num = frac.first;
        auto den = frac.second;
        while (den != 0) {
            auto div = num / den;
            auto rem = num % den;
            num = den;
            den = rem;
            div.yield();
        }
    });
}

void iterate(Generator!int seq) {
    foreach(i; seq) {
        write(i, " ");
    }
    writeln();
}

void main() {
    auto fracs = [
        Pair(   1,  2),
        Pair(   3,  1),
        Pair(  23,  8),
        Pair(  13, 11),
        Pair(  22,  7),
        Pair(-151, 77),
    ];
    foreach(frac; fracs) {
        writef("%4d / %-2d = ", frac.first, frac.second);
        frac.r2cf.iterate;
    }
    writeln;

    auto root2 = [
        Pair(    14_142,     10_000),
        Pair(   141_421,    100_000),
        Pair( 1_414_214,  1_000_000),
        Pair(14_142_136, 10_000_000),
    ];
    writeln("Sqrt(2) ->");
    foreach(frac; root2) {
        writef("%8d / %-8d = ", frac.first, frac.second);
        frac.r2cf.iterate;
    }
    writeln;

    auto pi = [
        Pair(         31,          10),
        Pair(        314,         100),
        Pair(      3_142,       1_000),
        Pair(     31_428,      10_000),
        Pair(    314_285,     100_000),
        Pair(  3_142_857,   1_000_000),
        Pair( 31_428_571,  10_000_000),
        Pair(314_285_714, 100_000_000),
    ];
    writeln("Pi ->");
    foreach(frac; pi) {
        writef("%9d / %-9d = ", frac.first, frac.second);
        frac.r2cf.iterate;
    }
}
```


{{out}}

```txt
   1 / 2  = 0 2
   3 / 1  = 3
  23 / 8  = 2 1 7
  13 / 11 = 1 5 2
  22 / 7  = 3 7
-151 / 77 = -1 -1 -24 -1 -2

Sqrt(2) ->
   14142 / 10000    = 1 2 2 2 2 2 1 1 29
  141421 / 100000   = 1 2 2 2 2 2 2 3 1 1 3 1 7 2
 1414214 / 1000000  = 1 2 2 2 2 2 2 2 3 6 1 2 1 12
14142136 / 10000000 = 1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2

Pi ->
       31 / 10        = 3 10
      314 / 100       = 3 7 7
     3142 / 1000      = 3 7 23 1 2
    31428 / 10000     = 3 7 357
   314285 / 100000    = 3 7 2857
  3142857 / 1000000   = 3 7 142857
 31428571 / 10000000  = 3 7 476190 3
314285714 / 100000000 = 3 7 7142857
```



## EDSAC order code

Besides the assigned task, this program demonstrates a division subroutine
for 35-bit positive integers, returning quotient and remainder.

```edsac

 [Continued fractions from rationals.
  EDSAC program, Initial Orders 2.]

 [Memory usage:
   56..109  Print subroutine, modified from the EDSAC library
  110..146  Division subroutine for long positive integers
  148..196  Continued fraction subroutine, as specified by Rosetta Code
  200..260  Main routine
  262..     List of rationals, variable number of items]

 [Define where to store the list of rationals.]
          T  45 K [store address in location 45;
                   values are then accessed by code letter H (*)]
          P 262 F [<------ address here]
 [(*) Arbitrary choice. We could equally well use 46 and N, 47 and M, etc.]

 [Library subroutine R2. Reads positive integers during input of orders,
    and is then overwritten (so doesn't take up any memory).
  Negative numbers can be input by adding 2^35.
  Each integer is followed by 'F', except the last is followed by '#TZ'.]
  GKT20FVDL8FA40DUDTFI40FA40FS39FG@S2FG23FA5@T5@E4@E13Z
          T    #H  [Tell R2 the storage location defined above]

 [Rationals to be read by R2. First item is count, then num/den pairs.]
  8F 1F2F 3F1F 33F8F 13F11F 22F7F 34359738217F77F
  141421356F100000000F 314285714F100000000#TZ

 [----------------------------------------------------------------------
  Modification of library subroutine P7.
  Prints signed integer up to 10 digits, left-justified.
  54 storage locations; working position 4D.
  Must be loaded at an even address.
  Input: Number is at 0D.]
          T  56 K
  GKA3FT42@A49@T31@ADE10@T31@A48@T31@SDTDH44#@NDYFLDT4DS43@TF
  H17@S17@A43@G23@UFS43@T1FV4DAFG50@SFLDUFXFOFFFSFL4FT4DA49@
  T31@A1FA43@G20@XFP1024FP610D@524D!FO46@O26@XFSFL8FT4DE39@

 [----------------------------------------------------------------------
  Division subroutine for long positive integers.
  35-bit dividend and divisor (max 2^34 - 1)
    returning quotient and remainder.
  Input:  dividend at 4D, divisor at 6D
  Output: remainder at 4D, quotient at 6D.
  Working locations 0D, 8D.]
          T 110 K
          G     K
          A   3 F [plant link]
          T  35 @
          A   6 D [load divisor]
          U   8 D [save at 8D (6D is required for quotient)]
    [4]   T     D [initialize shifted divisor]
          A   4 D [load dividend]
          R     D [shift 1 right]
          S     D [shifted divisor > dividend/2 yet?]
          G  13 @ [yes, start subtraction]
          T  36 @ [no, clear acc]
          A     D [shift divisor 1 more]
          L     D
          E   4 @ [loop back (always, since acc >= 0)]
   [13]   T  36 @ [clear acc]
          T   6 D [initialize quotient to 0]
   [15]   A   4 D [load remainder (initially = dividend)]
          S     D [trial subtraction]
          G  23 @ [skip if can't subtract]
          T   4 D [update remainder]
          A   6 D [load quotient]
          Y     F [add 1 by rounding twice (*)]
          Y     F
          T   6 D
   [23]   T  36 @ [clear acc]
          A   8 D [load original divisor]
          S     D [is shifted divisor back to original?]
          E  35 @ [yes, exit (with accumulator = 0,
                   in accordance with EDSAC convention)]
          T  36 @ [no, clear acc]
          A     D [shift divisor 1 right]
          R     D
          T     D
          A   6 D [shift quotient 1 left]
          L     D
          T   6 D
          E  15 @ [loop back (always, since acc = 0)]
   [35]   E     F [return; order set up at runtime]
   [36]   P     F [junk word, to clear accumulator]

 [(*) This saves the bother of defining a double-word constant 1
      and making sure that it's at an even address.]

 [----------------------------------------------------------------------
  Subroutine for lazy evaluation of continued fraction.
  Must be loaded at an even address.
  Locations relative to start of subroutine:
  0:    Entry point
  1:    Flag, < 0 if c.f. is finished, >= 0 if there's another term
  2, 3: Next term of c.f., provided the flag (location 1) is >= 0
  4, 5: Caller places numerator here before first call
  6, 7: Caller places denominator here before first call; must be > 0

  After setting up the numerator and denominator of the rational number,
    the caller should repeatedly call location 0, reading the result
    from location 1 and double location 2.
  Locations 4..7 are maintained by the subroutine and should not be changed
    by the caller until a new continued fraction is required.]

          T  46 K [place address of subroutine in location 46]
          P 148 F
          E  25 K [load the code below to that address (WWG page 18)]
          T     N
          G     K
    [0]   G   8 @ [entry point]
    [1]   P     F [flag returned here]
    [2]   P F P F [term returned here, if flag >= 0;
                   also used as temporary store]
    [4]   P F P F [caller puts numerator here]
    [6]   P F P F [caller puts denominator here]
    [8]   A   3 F [plant link]
          T  28 @
          S   6#@ [load negative of denominator]
          E  44 @ [if denom <= 0, no more terms]
          T     F [clear acc]
          A   4#@ [load numerator]
          T   2#@ [save before overwriting]
          A   6#@ [load denominator]
          U   4#@ [make it numerator for next call]
          T   6 D [also to 6D for division]
          A   2#@ [load numerator]
          G  29 @ [special action if negative]
          T   4 D [to 4D for division]
   [21]   A  21 @ [for return from next]
          G 110 F [call the above division subroutine]
          A   4 D [load remainder]
          T   6#@ [make it denominator for next call]
          A   6 D [load quotient]
   [26]   T   2#@ [return it as next term]
   [27]   T   1 @ [flag >= 0 means term is valid]
   [28]   E     F [exit with acc = 0]

         [Here if rational = -n/d where n, d > 0. Principle is:
          if  n + d - 1 = qd + r  then  -n = -qd + (d - 1 - r)]
   [29]   T   4 D [save numerator in 4D]
          S   6 D [acc := -den]
          Y     F [add 1 by rounding twice]
          Y     F
          T   2#@ [save (1 - den) for later]
          S   4 D [load abs(num)]
          S   2#@ [add (den - 1)]
          T   4 D [to 4D for division]
   [37]   A  37 @ [for return from next]
          G 110 F [call the above division subroutine]
          S   2#@ [load (den - 1)]
          S   4 D [subtract remainder]
          T   6#@ [result is new denominator]
          S   6 D [load negated quotient]
          G  26 @ [join common code]

         [Here if there are no more terms of the c.f.]
   [44]   T     F [clear acc]
          A   8 @ [this is negative since 'A' = -4]
          G  27 @ [exit with negative flag]

 [----------------------------------------------------------------------
  Main routine]
          T 200 K
          G     K
    [Variables]
    [0]   P     F [negative counter of continued fractions]
    [1]   P     F [character before term, first '=' then ',']
    [Constants]
    [2]   P     D [single-word 1]
    [3]   A   2#H [order to load first numerator]
    [4]   P   2 F [to inc addresses by 2]
    [5]   #     F [teleprinter figures shift]
    [6]   X     F [slash (in figures mode)]
    [7]   V     F [equals sign (in figures mode)]
    [8]   N     F [comma (in figures mode)]
    [9]   !     F [space]
   [10]   @     F [carriage return]
   [11]   &     F [line feed]
   [12]   K4096 F [teleprinter null]

          [Enter with acc = 0]
   [13]   O   5 @ [set teleprinter to figures]
          S     H [negative of number of c.f.s]
          T     @ [initialize counter]
          A   3 @ [initial load order]
   [17]   U  22 @ [plant order to load numerator]
          A   4 @ [inc address by 2]
          T  28 @ [plant order to load denominator]
          A   7 @ [set to print '=' before first term]
          T   1 @

          [Demonstrate the subroutine above.
          Since its address was placed in location 46,
          we can use code letter N to refer to it.]
   [22]   A    #H [load numerator (order set up at runtime)]
          U   4#N [pass to subroutine]
          T     D [also to 0D for printing]
   [25]   A  25 @ [for return from print subroutine]
          G  56 F [print numerator]
          O   6 @ [followed by slash]
   [28]   A    #H [load denominator (order set up at runtime)]
          U   6#N [pass to subroutine]
          T     D [also to 0D for printing]
   [31]   A  31 @ [for return from print subroutine]
          G  56 F [print denominator]
          O   9 @ [followed by space]
   [34]   A  34 @ [for return from subroutine]
          G     N [call subroutine for next term]
          A   1 N [load flag]
          G  48 @ [if < 0, c.f. is finished, jump out]
          O   1 @ [print equals or comma]
          O   9 @ [print space]
          T     F [clear acc]
          A   2#N [load term]
          T     D [to 0D for printing]
   [43]   A  43 @ [for return from print subroutine]
          G  56 F [print term; clears acc]
          A   8 @ [set to print ',' before subsequent terms]
          T   1 @
          E  34 @ [loop back for next term]

         [On to next continued fraction]
   [48]   O  10 @ [print new line]
          O  11 @
          T     F [clear acc]
          A     @ [load negative count of c.f.s]
          A   2 @ [add 1]
          E  59 @ [exit if count = 0]
          T     @ [store back]
          A  22 @ [order to load numerator]
          A   4 @ [inc address by 4 for next c.f.]
          A   4 @
          G  17 @ [loop back (always, since 'A' < 0)]

   [59]   O  12 @ [print null to flush teleprinter buffer]
          Z     F [stop]

          E  13 Z [define entry point]
          P     F [acc = 0 on entry]

```

{{out}}

```txt

1/2 = 0, 2
3/1 = 3
33/8 = 4, 8
13/11 = 1, 5, 2
22/7 = 3, 7
-151/77 = -2, 25, 1, 2
141421356/100000000 = 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 4, 1, 1, 2, 6, 8
314285714/100000000 = 3, 7, 7142857

```


=={{header|F_Sharp|F#}}==

```fsharp
let rec r2cf n d =
    if d = LanguagePrimitives.GenericZero then []
    else let q = n / d in q :: (r2cf d (n - q * d))

[<EntryPoint>]
let main argv =
    printfn "%A" (r2cf 1 2)
    printfn "%A" (r2cf 3 1)
    printfn "%A" (r2cf 23 8)
    printfn "%A" (r2cf 13 11)
    printfn "%A" (r2cf 22 7)
    printfn "%A" (r2cf -151 77)
    printfn "%A" (r2cf 141 100)
    printfn "%A" (r2cf 1414 1000)
    printfn "%A" (r2cf 14142 10000)
    printfn "%A" (r2cf 141421 100000)
    printfn "%A" (r2cf 1414214 1000000)
    printfn "%A" (r2cf 14142136 10000000)
    0
```

Output

```txt
[0; 2]
[3]
[2; 1; 7]
[1; 5; 2]
[3; 7]
[-1; -1; -24; -1; -2]
[1; 2; 2; 3; 1; 1; 2]
[1; 2; 2; 2; 2; 5; 3]
[1; 2; 2; 2; 2; 2; 1; 1; 29]
[1; 2; 2; 2; 2; 2; 2; 3; 1; 1; 3; 1; 7; 2]
[1; 2; 2; 2; 2; 2; 2; 2; 3; 6; 1; 2; 1; 12]
[1; 2; 2; 2; 2; 2; 2; 2; 2; 2; 6; 1; 2; 4; 1; 1; 2]
```

;A version for larger numerators and denominators.

```fsharp

let rec rI2cf n d =
    if d = 0I then []
    else let q = n / d in (decimal)q :: (rI2cf d (n - q * d))

```



## Factor

Note that the input values are stored as strings and converted to numbers before being fed to <code>r2cf</code>. This is because ratios automatically reduce themselves to the lowest-terms mixed number, which would make for confusing output in this instance.

```factor
USING: formatting kernel lists lists.lazy math math.parser qw
sequences ;
IN: rosetta-code.cf-arithmetic

: r2cf ( x -- lazy )
    [ >fraction [ /mod ] keep swap [ ] [ / ] if-zero nip ]
    lfrom-by [ integer? ] luntil [ >fraction /i ] lmap-lazy ;

: main ( -- )
    qw{
        1/2
        3
        23/8
        13/11
        22/7
        -151/77
        14142/10000
        141421/100000
        1414214/1000000
        14142136/10000000
        31/10
        314/100
        3142/1000
        31428/10000
        314285/100000
        3142857/1000000
        31428571/10000000
        314285714/100000000
    }
    [ dup string>number r2cf list>array "%19s -> %u\n" printf ]
    each ;

MAIN: main
```

{{out}}

```txt

                1/2 -> { 0 2 }
                  3 -> { 3 }
               23/8 -> { 2 1 7 }
              13/11 -> { 1 5 2 }
               22/7 -> { 3 7 }
            -151/77 -> { -1 -1 -24 -1 -2 }
        14142/10000 -> { 1 2 2 2 2 2 1 1 29 }
      141421/100000 -> { 1 2 2 2 2 2 2 3 1 1 3 1 7 2 }
    1414214/1000000 -> { 1 2 2 2 2 2 2 2 3 6 1 2 1 12 }
  14142136/10000000 -> { 1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2 }
              31/10 -> { 3 10 }
            314/100 -> { 3 7 7 }
          3142/1000 -> { 3 7 23 1 2 }
        31428/10000 -> { 3 7 357 }
      314285/100000 -> { 3 7 2857 }
    3142857/1000000 -> { 3 7 142857 }
  31428571/10000000 -> { 3 7 476190 3 }
314285714/100000000 -> { 3 7 7142857 }

```



## Go

(Note, the files making up this package are re-used as presented here for for the
[[Continued_fraction/Arithmetic/G(matrix_NG,_Contined_Fraction_N)#Go]]
and
[[Continued_fraction/Arithmetic/G(matrix_NG,_Contined_Fraction_N1,_Contined_Fraction_N2)#Go]]
tasks.)

File <code>cf.go</code>:

```Go
package cf

import (
	"fmt"
	"strings"
)

// ContinuedFraction is a regular continued fraction.
type ContinuedFraction func() NextFn

// NextFn is a function/closure that can return
// a posibly infinite sequence of values.
type NextFn func() (term int64, ok bool)

// String implements fmt.Stringer.
// It formats a maximum of 20 values, ending the
// sequence with ", ..." if the sequence is longer.
func (cf ContinuedFraction) String() string {
	var buf strings.Builder
	buf.WriteByte('[')
	sep := "; "
	const maxTerms = 20
	next := cf()
	for n := 0; ; n++ {
		t, ok := next()
		if !ok {
			break
		}
		if n > 0 {
			buf.WriteString(sep)
			sep = ", "
		}
		if n >= maxTerms {
			buf.WriteString("...")
			break
		}
		fmt.Fprint(&buf, t)
	}
	buf.WriteByte(']')
	return buf.String()
}

// Sqrt2 is the continued fraction for √2, [1; 2, 2, 2, ...].
func Sqrt2() NextFn {
	first := true
	return func() (int64, bool) {
		if first {
			first = false
			return 1, true
		}
		return 2, true
	}
}

// Phi is the continued fraction for ϕ, [1; 1, 1, 1, ...].
func Phi() NextFn {
	return func() (int64, bool) { return 1, true }
}

// E is the continued fraction for e,
// [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, 1, 10, 1, 1, 12, ...].
func E() NextFn {
	var i int
	return func() (int64, bool) {
		i++
		switch {
		case i == 1:
			return 2, true
		case i%3 == 0:
			return int64(i/3) * 2, true
		default:
			return 1, true
		}
	}
}
```

File <code>rat.go</code>:

```Go
package cf

import "fmt"

// A Rat represents a quotient N/D.
type Rat struct {
	N, D int64
}

// String implements fmt.Stringer and returns a string
// representation of `r` in the form "N/D" (even if D == 1).
func (r Rat) String() string {
	return fmt.Sprintf("%d/%d", r.N, r.D)
}

// As ContinuedFraction returns a contined fraction representation of `r`.
func (r Rat) AsContinuedFraction() ContinuedFraction { return r.CFTerms }
func (r Rat) CFTerms() NextFn {
	return func() (int64, bool) {
		if r.D == 0 {
			return 0, false
		}
		q := r.N / r.D
		r.N, r.D = r.D, r.N-q*r.D
		return q, true
	}
}

// Rosetta Code task explicitly asked for this function,
// so here it is. We'll just use the types above instead.
func r2cf(n1, n2 int64) ContinuedFraction { return Rat{n1, n2}.CFTerms }
```

File <code>rat_test.go</code>:

```Go
package cf

import (
	"fmt"
	"math"
)

func Example_ConstructFromRational() {
	cases := [...]Rat{
		{1, 2},
		{3, 1},
		{23, 8},
		{13, 11},
		{22, 7},
		{-151, 77},
	}
	for _, r := range cases {
		fmt.Printf("%7s = %s\n", r, r.AsContinuedFraction())
	}

	for _, tc := range [...]struct {
		name   string
		approx float64
		cf     ContinuedFraction
		d1, d2 int64
	}{
		{"√2", math.Sqrt2, Sqrt2, 1e4, 1e8},
		{"π", math.Pi, nil, 10, 1e10},
		{"ϕ", math.Phi, Phi, 10, 1e5},
		{"e", math.E, E, 1e5, 1e9},
	} {
		fmt.Printf("\nApproximating %s ≅ %v:\n", tc.name, tc.approx)
		for d := tc.d1; d < tc.d2; d *= 10 {
			n := int64(math.Round(tc.approx * float64(d)))
			r := Rat{n, d}
			fmt.Println(r, "=", r.AsContinuedFraction())
		}
		if tc.cf != nil {
			wid := int(math.Log10(float64(tc.d2)))*2 + 2 // ick
			fmt.Printf("%*s: %v\n", wid, "Actual", tc.cf)
		}
	}

	// Output:
	// [… commented output used by go test omitted for
	//    Rosetta Code listing; it is the same as below …]
}
```

{{out}}

```txt

    1/2 = [0; 2]
    3/1 = [3]
   23/8 = [2; 1, 7]
  13/11 = [1; 5, 2]
   22/7 = [3; 7]
-151/77 = [-1; -1, -24, -1, -2]

Approximating √2 ≅ 1.4142135623730951:
14142/10000 = [1; 2, 2, 2, 2, 2, 1, 1, 29]
141421/100000 = [1; 2, 2, 2, 2, 2, 2, 3, 1, 1, 3, 1, 7, 2]
1414214/1000000 = [1; 2, 2, 2, 2, 2, 2, 2, 3, 6, 1, 2, 1, 12]
14142136/10000000 = [1; 2, 2, 2, 2, 2, 2, 2, 2, 2, 6, 1, 2, 4, 1, 1, 2]
            Actual: [1; 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, ...]

Approximating π ≅ 3.141592653589793:
31/10 = [3; 10]
314/100 = [3; 7, 7]
3142/1000 = [3; 7, 23, 1, 2]
31416/10000 = [3; 7, 16, 11]
314159/100000 = [3; 7, 15, 1, 25, 1, 7, 4]
3141593/1000000 = [3; 7, 16, 983, 4, 2]
31415927/10000000 = [3; 7, 15, 1, 354, 2, 6, 1, 4, 1, 2]
314159265/100000000 = [3; 7, 15, 1, 288, 1, 2, 1, 3, 1, 7, 4]
3141592654/1000000000 = [3; 7, 15, 1, 293, 11, 1, 1, 7, 2, 1, 3, 3, 2]

Approximating ϕ ≅ 1.618033988749895:
16/10 = [1; 1, 1, 2]
162/100 = [1; 1, 1, 1, 1, 1, 2, 2]
1618/1000 = [1; 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5]
16180/10000 = [1; 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5]
      Actual: [1; 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...]

Approximating e ≅ 2.718281828459045:
271828/100000 = [2; 1, 2, 1, 1, 4, 1, 1, 6, 10, 1, 1, 2]
2718282/1000000 = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 3, 141]
27182818/10000000 = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 11, 1, 2, 10, 6, 2]
271828183/100000000 = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 2, 1, 1, 17, 6, 1, 1, 1, ...]
              Actual: [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, 1, 10, 1, 1, 12, 1, 1, ...]

```



## Haskell

{{trans|Python}}
This more general version generates a continued fraction from any real number (with rationals as a special case):

```haskell
import Data.Ratio ((%))

real2cf :: (RealFrac a, Integral b) => a -> [b]
real2cf x =
  let (i, f) = properFraction x
  in i :
     if f == 0
       then []
       else real2cf (1 / f)

main :: IO ()
main =
  mapM_
    print
    [ real2cf (13 % 11)
    , take 20 $ real2cf (sqrt 2)
    ]
```

{{Out}}

```txt
[1,5,2]
[1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]
```



## J

Note that the continued fractions shown in this task differ from those in the [[Continued_fraction#j|Continued fraction]] task as '''b''' here is implicitly always <code>1</code>.


### Tacit version 1


This version is a modification of an explicit version shown in http://www.jsoftware.com/jwiki/Essays/Continued%20Fractions to comply with the task specifications.

```j
cf=: _1 1 ,@}. (, <.)@%@-/ ::]^:a:@(, <.)@(%&x:/)
```


### = Examples =


```j
   cf each 1 2;3 1;23 8;13 11;22 7;14142136 10000000;_151 77
┌───┬─┬─────┬─────┬───┬─────────────────────────────────┬─────────┐
│0 2│3│2 1 7│1 5 2│3 7│1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2│_2 25 1 2│
└───┴─┴─────┴─────┴───┴─────────────────────────────────┴─────────┘
   cf each 14142 10000;141421 100000;1414214 1000000;14142136 10000000
┌──────────────────┬───────────────────────────┬────────────────────────────┬─────────────────────────────────┐
│1 2 2 2 2 2 1 1 29│1 2 2 2 2 2 2 3 1 1 3 1 7 2│1 2 2 2 2 2 2 2 3 6 1 2 1 12│1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2│
└──────────────────┴───────────────────────────┴────────────────────────────┴─────────────────────────────────┘
   cf each 31 10;314 100;3142 1000;31428 10000;314285 100000;3142857 1000000;31428571 10000000;314285714 100000000
┌────┬─────┬──────────┬───────┬────────┬──────────┬────────────┬───────────┐
│3 10│3 7 7│3 7 23 1 2│3 7 357│3 7 2857│3 7 142857│3 7 476190 3│3 7 7142857│
└────┴─────┴──────────┴───────┴────────┴──────────┴────────────┴───────────┘
```

This tacit version first produces the answer with a trailing ∞ (represented by _ in J) which is then removed by the last operation (_1 1 ,@}. ...). A continued fraction can be evaluated using the verb ((+%)/) and both representations produce equal results,

```j
   3 7 =&((+ %)/) 3 7 _
1
```

Incidentally, J and Tcl report a different representation for -151/77 versus the representation of some other implementations; however, both representations produce equal results.

```j
   _2 25 1 2 =&((+ %)/) _1 _1 _24 _1 _2
1
```



### Tacit version 2


Translation of python


```J
r2cf=:1 1{."1@}.({:,(0,{:)#:{.)^:(*@{:)^:a:
```


Example use:


```J
   ((":@{.,'/',":@{:),':  ',":@r2cf)@>1 2;3 1;23 8;13 11;22 7;14142136 10000000;_151 77;14142 10000;141421 100000;1414214 1000000;14142136 10000000;31 10;314 100;3142 1000;31428 10000;314285 100000;3142857 1000000;31428571 10000000;314285714 100000000
1/2:  0 2
3/1:  3
23/8:  2 1 7
13/11:  1 5 2
22/7:  3 7
14142136/10000000:  1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2
_151/77:  _2 25 1 2
14142/10000:  1 2 2 2 2 2 1 1 29
141421/100000:  1 2 2 2 2 2 2 3 1 1 3 1 7 2
1414214/1000000:  1 2 2 2 2 2 2 2 3 6 1 2 1 12
14142136/10000000:  1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2
31/10:  3 10
314/100:  3 7 7
3142/1000:  3 7 23 1 2
31428/10000:  3 7 357
314285/100000:  3 7 2857
3142857/1000000:  3 7 142857
31428571/10000000:  3 7 476190 3
314285714/100000000:  3 7 7142857
```



### Explicit versions


### = version 1 =

Implemented as a class, r2cf preserves state in a separate locale.  I've used some contrivances to jam the examples onto one line.

```J

coclass'cf'
create =: dyad def 'EMPTY [ N =: x , y'
destroy =: codestroy
r2cf =: monad define
 if. 0 (= {:) N do. _ return. end.
 RV =. <.@:(%/) N
 N =: ({. , |/)@:|. N
 RV
)

cocurrent'base'
CF =: conew'cf'

Until =: conjunction def 'u^:(-.@:v)^:_'

(,. }.@:}:@:((,r2cf__CF)Until(_-:{:))@:(8[create__CF/)&.>)1 2;3 1;23 8;13 11;22 7;14142136 10000000;_151 77
Note 'Output'
┌─────────────────┬─────────────────────────────────┐
│1 2              │0 2                              │
├─────────────────┼─────────────────────────────────┤
│3 1              │3                                │
├─────────────────┼─────────────────────────────────┤
│23 8             │2 1 7                            │
├─────────────────┼─────────────────────────────────┤
│13 11            │1 5 2                            │
├─────────────────┼─────────────────────────────────┤
│22 7             │3 7                              │
├─────────────────┼─────────────────────────────────┤
│14142136 10000000│1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2│
├─────────────────┼─────────────────────────────────┤
│_151 77          │_2 25 1 2                        │
└─────────────────┴─────────────────────────────────┘
)
```


### = version 2 =


```J

f =: 3 : 0
  a =. {.y
  b =. {:y
  out=. <. a%b
  while. b > 1 do.
    'a b' =. b; b|a
    out=. out , <. a%b
  end.
)
   f each 1 2;3 1;23 8;13 11;22 7;14142136 10000000;_151 77
┌───┬─┬─────┬─────┬───┬───────────────────────────────────┬─────────┐
│0 2│3│2 1 7│1 5 2│3 7│1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2 _│_2 25 1 2│
└───┴─┴─────┴─────┴───┴───────────────────────────────────┴─────────┘
```



### = version 3 =


translation of python:


```J
r2cf=:3 :0
  'n1 n2'=. y
  r=.''
  while.n2 do.
    'n1 t1 n2'=. n2,(0,n2)#:n1
    r=.r,t1
  end.
)
```


Example:


```J
   r2cf each 1 2;3 1;23 8;13 11;22 7;14142136 10000000;_151 77
┌───┬─┬─────┬─────┬───┬─────────────────────────────────┬─────────┐
│0 2│3│2 1 7│1 5 2│3 7│1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2│_2 25 1 2│
└───┴─┴─────┴─────┴───┴─────────────────────────────────┴─────────┘
```



## Java

{{trans|Kotlin}}
{{works with|Java|9}}

```Java
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class ConstructFromRationalNumber {
    private static class R2cf implements Iterator<Integer> {
        private int num;
        private int den;

        R2cf(int num, int den) {
            this.num = num;
            this.den = den;
        }

        @Override
        public boolean hasNext() {
            return den != 0;
        }

        @Override
        public Integer next() {
            int div = num / den;
            int rem = num % den;
            num = den;
            den = rem;
            return div;
        }
    }

    private static void iterate(R2cf generator) {
        generator.forEachRemaining(n -> System.out.printf("%d ", n));
        System.out.println();
    }

    public static void main(String[] args) {
        List<Map.Entry<Integer, Integer>> fracs = List.of(
                Map.entry(1, 2),
                Map.entry(3, 1),
                Map.entry(23, 8),
                Map.entry(13, 11),
                Map.entry(22, 7),
                Map.entry(-151, 77)
        );
        for (Map.Entry<Integer, Integer> frac : fracs) {
            System.out.printf("%4d / %-2d = ", frac.getKey(), frac.getValue());
            iterate(new R2cf(frac.getKey(), frac.getValue()));
        }

        System.out.println("\nSqrt(2) ->");
        List<Map.Entry<Integer, Integer>> root2 = List.of(
                Map.entry(    14_142,     10_000),
                Map.entry(   141_421,    100_000),
                Map.entry( 1_414_214,  1_000_000),
                Map.entry(14_142_136, 10_000_000)
        );
        for (Map.Entry<Integer, Integer> frac : root2) {
            System.out.printf("%8d / %-8d = ", frac.getKey(), frac.getValue());
            iterate(new R2cf(frac.getKey(), frac.getValue()));
        }

        System.out.println("\nPi ->");
        List<Map.Entry<Integer, Integer>> pi = List.of(
                Map.entry(         31,        10),
                Map.entry(        314,       100),
                Map.entry(      3_142,      1_000),
                Map.entry(     31_428,     10_000),
                Map.entry(    314_285,    100_000),
                Map.entry(  3_142_857,   1_000_000),
                Map.entry( 31_428_571,  10_000_000),
                Map.entry(314_285_714, 100_000_000)
        );
        for (Map.Entry<Integer, Integer> frac : pi) {
            System.out.printf("%9d / %-9d = ", frac.getKey(), frac.getValue());
            iterate(new R2cf(frac.getKey(), frac.getValue()));
        }
    }
}
```

{{out}}

```txt
   1 / 2  = 0 2
   3 / 1  = 3
  23 / 8  = 2 1 7
  13 / 11 = 1 5 2
  22 / 7  = 3 7
-151 / 77 = -1 -1 -24 -1 -2

Sqrt(2) ->
   14142 / 10000    = 1 2 2 2 2 2 1 1 29
  141421 / 100000   = 1 2 2 2 2 2 2 3 1 1 3 1 7 2
 1414214 / 1000000  = 1 2 2 2 2 2 2 2 3 6 1 2 1 12
14142136 / 10000000 = 1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2

Pi ->
       31 / 10        = 3 10
      314 / 100       = 3 7 7
     3142 / 1000      = 3 7 23 1 2
    31428 / 10000     = 3 7 357
   314285 / 100000    = 3 7 2857
  3142857 / 1000000   = 3 7 142857
 31428571 / 10000000  = 3 7 476190 3
314285714 / 100000000 = 3 7 7142857

```



## Julia

{{works with|Julia|0.6}}


```julia
# It'st most appropriate to define a Julia iterable object for this task
# Julia doesn't have Python'st yield, the closest to it is produce/consume calls with Julia tasks
# but for various reasons they don't work out for this task
# This solution works with two integers, a Julia rational or a real

mutable struct ContinuedFraction{T<:Integer}
    n1::T # numerator or real
    n2::T # denominator or 1 if real
    t1::T # generated coefficient
end

# Constructors for all possible input types
ContinuedFraction{T<:Integer}(n1::T, n2::T) = ContinuedFraction(n1, n2, 0)
ContinuedFraction(n::Rational) = ContinuedFraction(numerator(n), denominator(n))
ContinuedFraction(n::AbstractFloat) = ContinuedFraction(Rational(n))

# Methods to make our object iterable
Base.start(::ContinuedFraction) = nothing
# Returns true if we've prepared the continued fraction
Base.done(cf::ContinuedFraction, st) = cf.n2 == 0
# Generates the next coefficient
function Base.next(cf::ContinuedFraction, st)
    cf.n1, (cf.t1, cf.n2) = cf.n2, divrem(cf.n1, cf.n2)
    return cf.t1, nothing
end

# Tell Julia that this object always returns ints (all coeffs are integers)
Base.eltype{T}(::Type{ContinuedFraction{T}}) = T

# Overload the default collect function so that we can collect the first maxiter coeffs of infinite continued fractions
# array slicing doesn't work as Julia crashes before the slicing due to our infinitely long array
function Base.collect(itr::ContinuedFraction, maxiter::Integer = 100)
    r = Array{eltype(itr)}(maxiter)
    i = 1
    for v in itr
        r[i] = v
        i += 1
        if i > maxiter break end
    end
    return r[1:i-1]
end

# Test cases according to task description with outputs in comments
println(collect(ContinuedFraction(1, 2)))       # => [0, 2]
println(collect(ContinuedFraction(3, 1)))       # => [3]
println(collect(ContinuedFraction(23, 8)))      # => [2, 1, 7]
println(collect(ContinuedFraction(13, 11)))     # => [1, 5, 2]
println(collect(ContinuedFraction(22, 7)))      # => [3, 7]
println(collect(ContinuedFraction(14142, 10000)))       # => [1, 2, 2, 2, 2, 2, 1, 1, 29]
println(collect(ContinuedFraction(141421, 100000)))     # => [1, 2, 2, 2, 2, 2, 2, 3, 1, 1, 3, 1, 7, 2]
println(collect(ContinuedFraction(1414214, 1000000)))   # => [1, 2, 2, 2, 2, 2, 2, 2, 3, 6, 1, 2, 1, 12]
println(collect(ContinuedFraction(14142136, 10000000))) # => [1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 6, 1, 2, 4, 1, 1, 2]

println(collect(ContinuedFraction(13 // 11)))   # => [1, 5, 2]
println(collect(ContinuedFraction(√2), 20))     # => [1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
```



## Kotlin


```scala
// version 1.1.2
// compile with -Xcoroutines=enable flag from command line

import kotlin.coroutines.experimental.buildSequence

fun r2cf(frac: Pair<Int, Int>) =
    buildSequence {
        var num = frac.first
        var den = frac.second
        while (Math.abs(den) != 0) {
            val div = num / den
            val rem = num % den
            num = den
            den = rem
            yield(div)
        }
    }

fun iterate(seq: Sequence<Int>) {
    for (i in seq) print("$i ")
    println()
}

fun main(args: Array<String>) {
    val fracs = arrayOf(1 to 2, 3 to 1, 23 to 8, 13 to 11, 22 to 7, -151 to 77)
    for (frac in fracs) {
        print("${"%4d".format(frac.first)} / ${"%-2d".format(frac.second)} = ")
        iterate(r2cf(frac))
    }
    val root2 = arrayOf(14142 to 10000, 141421 to 100000,
                        1414214 to 1000000, 14142136 to 10000000)
    println("\nSqrt(2) ->")
    for (frac in root2) {
        print("${"%8d".format(frac.first)} / ${"%-8d".format(frac.second)} = ")
        iterate(r2cf(frac))
    }
    val pi = arrayOf(31 to 10, 314 to 100, 3142 to 1000, 31428 to 10000,
                     314285 to 100000, 3142857 to 1000000,
                     31428571 to 10000000, 314285714 to 100000000)
    println("\nPi ->")
    for (frac in pi) {
        print("${"%9d".format(frac.first)} / ${"%-9d".format(frac.second)} = ")
        iterate(r2cf(frac))
    }
}
```


{{out}}

```txt

   1 / 2  = 0 2
   3 / 1  = 3
  23 / 8  = 2 1 7
  13 / 11 = 1 5 2
  22 / 7  = 3 7
-151 / 77 = -1 -1 -24 -1 -2

Sqrt(2) ->
   14142 / 10000    = 1 2 2 2 2 2 1 1 29
  141421 / 100000   = 1 2 2 2 2 2 2 3 1 1 3 1 7 2
 1414214 / 1000000  = 1 2 2 2 2 2 2 2 3 6 1 2 1 12
14142136 / 10000000 = 1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2

Pi ->
       31 / 10        = 3 10
      314 / 100       = 3 7 7
     3142 / 1000      = 3 7 23 1 2
    31428 / 10000     = 3 7 357
   314285 / 100000    = 3 7 2857
  3142857 / 1000000   = 3 7 142857
 31428571 / 10000000  = 3 7 476190 3
314285714 / 100000000 = 3 7 7142857

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica has a build-in function ContinuedFraction.

```mathematica
ContinuedFraction[1/2]
ContinuedFraction[3]
ContinuedFraction[23/8]
ContinuedFraction[13/11]
ContinuedFraction[22/7]
ContinuedFraction[-151/77]
ContinuedFraction[14142/10000]
ContinuedFraction[141421/100000]
ContinuedFraction[1414214/1000000]
ContinuedFraction[14142136/10000000]
```

{{Out}}

```txt
{0, 2}
{3}
{2, 1, 7}
{1, 5, 2}
{3, 7}
{-1, -1, -24, -1, -2}
{1, 2, 2, 2, 2, 2, 1, 1, 29}
{1, 2, 2, 2, 2, 2, 2, 3, 1, 1, 3, 1, 7, 2}
{1, 2, 2, 2, 2, 2, 2, 2, 3, 6, 1, 2, 1, 12}
{1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 6, 1, 2, 4, 1, 1, 2}
```


=={{header|Modula-2}}==

```modula2
MODULE ConstructFromrationalNumber;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE R2cf = RECORD
    num,den : INTEGER;
END;

PROCEDURE HasNext(self : R2cf) : BOOLEAN;
BEGIN
    RETURN self.den # 0;
END HasNext;

PROCEDURE Next(VAR self : R2cf) : INTEGER;
VAR div,rem : INTEGER;
BEGIN
    div := self.num / self.den;
    rem := self.num REM self.den;
    self.num := self.den;
    self.den := rem;
    RETURN div;
END Next;

PROCEDURE Iterate(self : R2cf);
VAR buf : ARRAY[0..64] OF CHAR;
BEGIN
    WHILE HasNext(self) DO
        FormatString("%i ", buf, Next(self));
        WriteString(buf);
    END;
    WriteLn;
END Iterate;

PROCEDURE Print(num,den : INTEGER);
VAR frac : R2cf;
VAR buf : ARRAY[0..64] OF CHAR;
BEGIN
    FormatString("%9i / %-9i = ", buf, num, den);
    WriteString(buf);

    frac.num := num;
    frac.den := den;
    Iterate(frac);
END Print;

VAR frac : R2cf;
BEGIN
    Print(1,2);
    Print(3,1);
    Print(23,8);
    Print(13,11);
    Print(22,7);
    Print(-151,77);

    WriteLn;
    WriteString("Sqrt(2) ->");
    WriteLn;
    Print(14142,10000);
    Print(141421,100000);
    Print(1414214,1000000);
    Print(14142136,10000000);

    WriteLn;
    WriteString("Pi ->");
    WriteLn;
    Print(31,10);
    Print(314,100);
    Print(3142,1000);
    Print(31428,10000);
    Print(314285,100000);
    Print(3142857,1000000);
    Print(31428571,10000000);
    Print(314285714,100000000);

    ReadChar;
END ConstructFromrationalNumber.
```



## PARI/GP


```parigp
apply(contfrac,[1/2,3,23/8,13/11,22/7,-151/77])
```

{{out}}

```txt
[[0, 2], [3], [2, 1, 7], [1, 5, 2], [3, 7], [-2, 25, 1, 2]]
```



## Perl

To do output one digit at a time, we first turn off buffering to be pedantic, then use a closure that yields one term per call.

```perl
$|=1;

sub rc2f {
  my($num, $den) = @_;
  return sub { return unless $den;
               my $q = int($num/$den);
               ($num, $den) = ($den, $num - $q*$den);
               $q; };
}

sub rcshow {
  my $sub = shift;
  print "[";
  my $n = $sub->();
  print "$n" if defined $n;
  print "; $n" while defined($n = $sub->());
  print "]\n";
}

rcshow(rc2f(@$_))
   for ([1,2],[3,1],[23,8],[13,11],[22,7],[-151,77]);
print "\n";
rcshow(rc2f(@$_))
   for ([14142,10000],[141421,100000],[1414214,1000000],[14142136,10000000]);
print "\n";
rcshow(rc2f(314285714,100000000));
```

{{out}}

```txt

[0; 2]
[3]
[2; 1; 7]
[1; 5; 2]
[3; 7]
[-1; -1; -24; -1; -2]

[1; 2; 2; 2; 2; 2; 1; 1; 29]
[1; 2; 2; 2; 2; 2; 2; 3; 1; 1; 3; 1; 7; 2]
[1; 2; 2; 2; 2; 2; 2; 2; 3; 6; 1; 2; 1; 12]
[1; 2; 2; 2; 2; 2; 2; 2; 2; 2; 6; 1; 2; 4; 1; 1; 2]

[3; 7; 7142857]

```



## Perl 6

Straightforward implementation:

```perl6
sub r2cf(Rat $x is copy) {
    gather loop {
	$x -= take $x.floor;
	last unless $x > 0;
	$x = 1 / $x;
    }
}

say r2cf(.Rat) for <1/2 3 23/8 13/11 22/7 1.41 1.4142136>;
```

{{out}}

```txt
(0 2)
(3)
(2 1 7)
(1 5 2)
(3 7)
(1 2 2 3 1 1 2)
(1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2)
```

As a silly one-liner:

```perl6
sub r2cf(Rat $x is copy) { gather $x [R/]= 1 while ($x -= take $x.floor) > 0 }
```



## Phix

{{trans|C}}

```Phix
function r2cf(sequence fraction)
    integer {numerator, denominator} = fraction
    integer quotient = 0
    if denominator!=0 then
        quotient = floor(numerator/denominator)
        {numerator,denominator} = {denominator,mod(numerator,denominator)}
    end if
    return {quotient,{numerator,denominator}}
end function

constant DENOMINATOR = 2
procedure test(string txt, sequence tests)
sequence fraction
integer quotient
    printf(1,"Running %s :",{txt})
    for i=1 to length(tests) do
        fraction = tests[i]
        printf(1,"\nFor N = %d, D = %d :",fraction)
        while fraction[DENOMINATOR]!=0 do
            {quotient,fraction} = r2cf(fraction)
            printf(1," %d ",quotient)
        end while
    end for
    printf(1,"\n\n")
end procedure

constant examples = {{1,2}, {3,1}, {23,8}, {13,11}, {22,7}, {-151,77}},
         sqrt2 = {{14142,10000}, {141421,100000}, {1414214,1000000}, {14142136,10000000}},
         pi = {{31,10}, {314,100}, {3142,1000}, {31428,10000}, {314285,100000}, {3142857,1000000}, {31428571,10000000}, {314285714,100000000}}

test("the examples",examples)
test("for sqrt(2)",sqrt2)
test("for pi",pi)
```

{{out}}

```txt

Running the examples :
For N = 1, D = 2 : 0  2
For N = 3, D = 1 : 3
For N = 23, D = 8 : 2  1  7
For N = 13, D = 11 : 1  5  2
For N = 22, D = 7 : 3  7
For N = -151, D = 77 : -2  25  1  2

Running for sqrt(2) :
For N = 14142, D = 10000 : 1  2  2  2  2  2  1  1  29
For N = 141421, D = 100000 : 1  2  2  2  2  2  2  3  1  1  3  1  7  2
For N = 1414214, D = 1000000 : 1  2  2  2  2  2  2  2  3  6  1  2  1  12
For N = 14142136, D = 10000000 : 1  2  2  2  2  2  2  2  2  2  6  1  2  4  1  1  2

Running for pi :
For N = 31, D = 10 : 3  10
For N = 314, D = 100 : 3  7  7
For N = 3142, D = 1000 : 3  7  23  1  2
For N = 31428, D = 10000 : 3  7  357
For N = 314285, D = 100000 : 3  7  2857
For N = 3142857, D = 1000000 : 3  7  142857
For N = 31428571, D = 10000000 : 3  7  476190  3
For N = 314285714, D = 100000000 : 3  7  7142857

```



## Python

{{trans|Ruby}}

```python
def r2cf(n1,n2):
  while n2:
    n1, (t1, n2) = n2, divmod(n1, n2)
    yield t1

print(list(r2cf(1,2)))    # => [0, 2]
print(list(r2cf(3,1)))    # => [3]
print(list(r2cf(23,8)))    # => [2, 1, 7]
print(list(r2cf(13,11)))    # => [1, 5, 2]
print(list(r2cf(22,7)))    # => [3, 7]
print(list(r2cf(14142,10000)))    # => [1, 2, 2, 2, 2, 2, 1, 1, 29]
print(list(r2cf(141421,100000)))    # => [1, 2, 2, 2, 2, 2, 2, 3, 1, 1, 3, 1, 7, 2]
print(list(r2cf(1414214,1000000)))    # => [1, 2, 2, 2, 2, 2, 2, 2, 3, 6, 1, 2, 1, 12]
print(list(r2cf(14142136,10000000)))    # => [1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 6, 1, 2, 4, 1, 1, 2]
```

This version generates it from any real number (with rationals as a special case):

```python
def real2cf(x):
    while True:
        t1, f = divmod(x, 1)
        yield int(t1)
        if not f:
            break
        x = 1/f

from fractions import Fraction
from itertools import islice

print(list(real2cf(Fraction(13, 11))))    # => [1, 5, 2]
print(list(islice(real2cf(2 ** 0.5), 20)))    # => [1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
```



## Racket


```racket

#lang racket

(define ((r2cf n d))
  (or (zero? d)
      (let-values ([(q r) (quotient/remainder n d)])
        (set! n d)
        (set! d r)
        q)))

(define (r->cf n d)
  (for/list ([i (in-producer (r2cf n d) #t)]) i))

(define (real->cf x places)
  (define d (expt 10 places))
  (define n (exact-floor (* x d)))
  (r->cf n d))

(map r->cf
     '(1 3 23 13 22 -151)
     '(2 1  8 11  7   77))
(real->cf (sqrt 2) 10)
(real->cf pi 10)

```


{{out}}


```txt

'((0 2) (3) (2 1 7) (1 5 2) (3 7) (-1 -1 -24 -1 -2))
'(1 2 2 2 2 2 2 2 2 2 2 2 2 3 3 1 3 8 9 1 20 1 2)
'(3 7 15 1 292 1 1 6 2 13 3 1 12 3)

```



## REXX

Programming notes:
*   Increasing   '''numeric digits'''   to a higher value will generate more terms.
*   Two subroutines,   '''sqrt'''   and   '''pi''',   were included here to demonstrate terms for   √<span style="text-decoration: overline"> 2 </span>   and   pi.
*   The subroutine   '''$maxfact'''   was included and is only needed if the number used for   '''r2cf'''   is a decimal fraction.
*   Checks were included to verify that the arguments being passed to   '''r2cf'''   are indeed numeric and also not zero.
*   This REXX version also handles negative numbers.

```rexx
/*REXX program converts a  decimal  or  rational fraction  to a  continued fraction.    */
numeric digits 230                               /*determines how many terms to be gened*/
say '              1/2  ──► CF: '   r2cf( '1/2'      )
say '               3   ──► CF: '   r2cf(   3        )
say '             23/8  ──► CF: '   r2cf( '23/8'     )
say '             13/11 ──► CF: '   r2cf( '13/11'    )
say '             22/7  ──► CF: '   r2cf( '22/7 '    )
say '                       ___'
say '───────── attempts at √ 2.'
say '14142/1e4          ──► CF: '   r2cf( '14142/1e4 '          )
say '141421/1e5         ──► CF: '   r2cf( '141421/1e5 '         )
say '1414214/1e6        ──► CF: '   r2cf( '1414214/1e6 '        )
say '14142136/1e7       ──► CF: '   r2cf( '14142136/1e7 '       )
say '141421356/1e8      ──► CF: '   r2cf( '141421356/1e8 '      )
say '1414213562/1e9     ──► CF: '   r2cf( '1414213562/1e9 '     )
say '14142135624/1e10   ──► CF: '   r2cf( '14142135624/1e10 '   )
say '141421356237/1e11  ──► CF: '   r2cf( '141421356237/1e11 '  )
say '1414213562373/1e12 ──► CF: '   r2cf( '1414213562373/1e12 ' )
say '√2                 ──► CF: '   r2cf(  sqrt(2)              )
say
say '───────── an attempt at pi'
say 'pi                 ──► CF: '   r2cf(  pi() )
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
$maxFact: procedure;  parse arg x 1 _x,y;   y=10**(digits()-1);   b=0;  h=1;  a=1;     g=0
            do while a<=y & g<=y;  n=trunc(_x);  _=a;  a=n*a+b;   b=_;  _=g;  g=n*g+h; h=_
            if n=_x | a/g=x  then do; if a>y | g>y  then iterate; b=a;  h=g;  leave;   end
            _x=1/(_x-n);  end;                           return  b'/'h
/*──────────────────────────────────────────────────────────────────────────────────────*/
pi: return 3.1415926535897932384626433832795028841971693993751058209749445923078164062862,
           || 089986280348253421170679821480865132823066470938446095505822317253594081284,
           || 811174502841027019385211055596446229489549303819644288109756659334461284756,
           || 48233786783165271                        /* ··· should  ≥  NUMERIC DIGITS */
/*──────────────────────────────────────────────────────────────────────────────────────*/
r2cf: procedure; parse arg g 1 s 2;  $=;     if s=='-'  then g=substr(g, 2)
                                                        else s=
      if pos(., g)\==0  then do;  if \datatype(g, 'N')  then call serr 'not numeric:'   g
                                  g=$maxfact(g)
                             end
      if pos('/', g)==0      then g=g"/"1
      parse var  g   n  '/'  d
      if \datatype(n, 'W')   then call serr    "a numerator isn't an integer:"    n
      if \datatype(d, 'W')   then call serr  "a denominator isn't an integer:"    d
      if d=0                 then call serr  'a denominator is zero'
      n=abs(n)                                         /*ensure numerator is positive.  */
                         do  while  d\==0;      _=d    /*where the rubber meets the road*/
                         $=$  s || (n%d)               /*append another number to list. */
                         d=n // d;              n=_    /* %  is int div,  // is modulus.*/
                         end   /*while*/
      return strip($)
/*──────────────────────────────────────────────────────────────────────────────────────*/
serr: say;    say '***error***';    say;    say arg(1);     say;    exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x;  if x=0  then return 0;  d=digits();   h=d+6;   numeric form
      m.=9; numeric digits; parse value format(x,2,1,,0) 'E0' with g 'E' _ .; g=g*.5'e'_%2
                     do j=0  while h>9;      m.j=h;               h=h%2+1;       end /*j*/
                     do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;  end /*k*/
      numeric digits d;                      return g/1
```

{{out|output|text=  when using the default (internal) inputs:}}

```txt

              1/2  ──► CF:  0 2
               3   ──► CF:  3
             23/8  ──► CF:  2 1 7
             13/11 ──► CF:  1 5 2
             22/7  ──► CF:  3 7
                       ___
───────── attempts at √ 2.
14142/1e4          ──► CF:  1 2 2 2 2 2 1 1 29
141421/1e5         ──► CF:  1 2 2 2 2 2 2 3 1 1 3 1 7 2
1414214/1e6        ──► CF:  1 2 2 2 2 2 2 2 3 6 1 2 1 12
14142136/1e7       ──► CF:  1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2
141421356/1e8      ──► CF:  1 2 2 2 2 2 2 2 2 2 2 3 4 1 1 2 6 8
1414213562/1e9     ──► CF:  1 2 2 2 2 2 2 2 2 2 2 2 1 1 14 1 238 1 3
14142135624/1e10   ──► CF:  1 2 2 2 2 2 2 2 2 2 2 2 2 2 5 4 1 8 4 2 1 4
141421356237/1e11  ──► CF:  1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 1 4 1 2 1 63 2 1 1 1 4 2
1414213562373/1e12 ──► CF:  1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 1 11 2 3 2 1 1 1 25 1 2 3
√2                 ──► CF:  1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3

───────── an attempt at pi
pi                 ──► CF:  3 7 15 1 292 1 1 1 2 1 3 1 14 2 1 1 2 2 2 2 1 84 2 1 1 15 3 13 1 4 2 6 6 99 1 2 2 6 3 5 1 1 6 8 1 7 1 2 3 7 1 2 1 1 12 1 1 1 3 1 1 8 1 1 2 1 6 1 1 5 2 2 3 1 2 4 4 16 1 161 45 1 22 1 2 2 1 4 1 2 24 1 2 1 3 1 2 1 1 10 2 5 4 1 2 2 8 1 5 2 2 26 1 4 1 1 8 2 42 2 1 7 3 3 1 1 7 2 4 9 7 2 3 1 57 1 18 1 9 19 1 2 18 1 3 7 30 1 1 1 3 3 3 1 2 8 1 1 2 1 15 1 2 13 1 2 1 4 1 12 1 1 3 3 28 1 10 3 2 20 1 1 1 1 4 1 1 1 5 3 2 1 6 1 4 1 120 2 1 1 3 1 23 1 15 1 3 7 1 16 1 2 1 21 2 1 1 2 9 1 6 4

```



## Ruby


```ruby
# Generate a continued fraction from a rational number

def r2cf(n1,n2)
  while n2 > 0
    n1, (t1, n2) = n2, n1.divmod(n2)
    yield t1
  end
end
```


### Testing

'''Test 1:'''

```ruby
[[1,2], [3,1], [23,8], [13,11], [22,7], [-151,77]].each do |n1,n2|
  print "%10s : " % "#{n1} / #{n2}"
  r2cf(n1,n2) {|n| print "#{n} "}
  puts
end
```

{{out}}

```txt

     1 / 2 : 0 2
     3 / 1 : 3
    23 / 8 : 2 1 7
   13 / 11 : 1 5 2
    22 / 7 : 3 7
 -151 / 77 : -2 25 1 2

```

'''Test 2:'''
<math>\sqrt 2</math>

```ruby
(5..8).each do |digit|
  n2 = 10 ** (digit-1)
  n1 = (Math.sqrt(2) * n2).round
  print "%-8s / %-8s : " % [n1, n2]
  r2cf(n1,n2) {|n| print "#{n} "}
  puts
end
```

{{out}}

```txt

14142    / 10000    : 1 2 2 2 2 2 1 1 29
141421   / 100000   : 1 2 2 2 2 2 2 3 1 1 3 1 7 2
1414214  / 1000000  : 1 2 2 2 2 2 2 2 3 6 1 2 1 12
14142136 / 10000000 : 1 2 2 2 2 2 2 2 2 2 6 1 2 4 1 1 2

```

'''Test 3:'''

```ruby
a =[ [31,10],
     [314,100],
     [3142,1000],
     [31428,10000],
     [314285,100000],
     [3142857,1000000],
     [31428571,10000000],
     [314285714,100000000]
   ]
a.each do |n1,n2|
  print "%-9s / %-9s : " % [n1, n2]
  r2cf(n1,n2) {|n| print "#{n} "}
  puts
end
```

{{out}}

```txt

31        / 10        : 3 10
314       / 100       : 3 7 7
3142      / 1000      : 3 7 23 1 2
31428     / 10000     : 3 7 357
314285    / 100000    : 3 7 2857
3142857   / 1000000   : 3 7 142857
31428571  / 10000000  : 3 7 476190 3
314285714 / 100000000 : 3 7 7142857

```



## Rust


```rust

struct R2cf {
    n1: i64,
    n2: i64
}

// This iterator generates the continued fraction representation from the
// specified rational number.
impl Iterator for R2cf {
    type Item = i64;

    fn next(&mut self) -> Option<i64> {
        if self.n2 == 0 {
            None
        }
        else {
            let t1 = self.n1 / self.n2;
            let t2 = self.n2;
            self.n2 = self.n1 - t1 * t2;
            self.n1 = t2;
            Some(t1)
        }
    }
}

fn r2cf(n1: i64, n2: i64) -> R2cf {
    R2cf { n1: n1, n2: n2 }
}

macro_rules! printcf {
    ($x:expr, $y:expr) => (println!("{:?}", r2cf($x, $y).collect::<Vec<_>>()));
}

fn main() {
    printcf!(1, 2);
    printcf!(3, 1);
    printcf!(23, 8);
    printcf!(13, 11);
    printcf!(22, 7);
    printcf!(-152, 77);

    printcf!(14_142, 10_000);
    printcf!(141_421, 100_000);
    printcf!(1_414_214, 1_000_000);
    printcf!(14_142_136, 10_000_000);

    printcf!(31, 10);
    printcf!(314, 100);
    printcf!(3142, 1000);
    printcf!(31_428, 10_000);
    printcf!(314_285, 100_000);
    printcf!(3_142_857, 1_000_000);
    printcf!(31_428_571, 10_000_000);
    printcf!(314_285_714, 100_000_000);
}

```


{{out}}

```txt

[0, 2]
[3]
[2, 1, 7]
[1, 5, 2]
[3, 7]
[-1, -1, -37, -2]
[1, 2, 2, 2, 2, 2, 1, 1, 29]
[1, 2, 2, 2, 2, 2, 2, 3, 1, 1, 3, 1, 7, 2]
[1, 2, 2, 2, 2, 2, 2, 2, 3, 6, 1, 2, 1, 12]
[1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 6, 1, 2, 4, 1, 1, 2]
[3, 10]
[3, 7, 7]
[3, 7, 23, 1, 2]
[3, 7, 357]
[3, 7, 2857]
[3, 7, 142857]
[3, 7, 476190, 3]
[3, 7, 7142857]

```



## Sidef

{{trans|Perl}}

```ruby
func r2cf(num, den) {
    func() {
        den || return nil
        var q = num//den
        (num, den) = (den, num - q*den)
        return q
    }
}

func showcf(f) {
    print "["
    var n = f()
    print "#{n}" if defined(n)
    print "; #{n}" while defined(n = f())
    print "]\n"
}

[
    [1/2, 3/1, 23/8, 13/11, 22/7, -151/77],
    [14142/10000, 141421/100000, 1414214/1000000, 14142136/10000000],
    [314285714/100000000],
].each { |seq|
    seq.each { |r| showcf(r2cf(r.nude)) }
    print "\n"
}
```

{{out}}

```txt

[0; 2]
[3]
[2; 1; 7]
[1; 5; 2]
[3; 7]
[-1; -1; -24; -1; -2]

[1; 2; 2; 2; 2; 2; 1; 1; 29]
[1; 2; 2; 2; 2; 2; 2; 3; 1; 1; 3; 1; 7; 2]
[1; 2; 2; 2; 2; 2; 2; 2; 3; 6; 1; 2; 1; 12]
[1; 2; 2; 2; 2; 2; 2; 2; 2; 2; 6; 1; 2; 4; 1; 1; 2]

[3; 7; 7142857]

```



## Tcl

{{works with|Tcl|8.6}}
{{trans|Ruby}}

### Direct translation


```tcl
package require Tcl 8.6

proc r2cf {n1 {n2 1}} {
    # Convert a decimal fraction (e.g., 1.23) into a form we can handle
    if {$n1 != int($n1) && [regexp {\.(\d+)} $n1 -> suffix]} {
	set pow [string length $suffix]
	set n1 [expr {int($n1 * 10**$pow)}]
	set n2 [expr {$n2 * 10**$pow}]
    }
    # Construct the continued fraction as a coroutine that yields the digits in sequence
    coroutine cf\#[incr ::cfcounter] apply {{n1 n2} {
	yield [info coroutine]
	while {$n2 > 0} {
	    yield [expr {$n1 / $n2}]
	    set n2 [expr {$n1 % [set n1 $n2]}]
	}
	return -code break
    }} $n1 $n2
}
```

Demonstrating:

```tcl
proc printcf {name cf} {
    puts -nonewline "$name -> "
    while 1 {
	puts -nonewline "[$cf],"
    }
    puts "\b "
}

foreach {n1 n2} {
    1 2
    3 1
    23 8
    13 11
    22 7
    -151 77
    14142 10000
    141421 100000
    1414214 1000000
    14142136 10000000
    31 10
    314 100
    3142 1000
    31428 10000
    314285 100000
    3142857 1000000
    31428571 10000000
    314285714 100000000
    3141592653589793 1000000000000000
} {
    printcf "\[$n1;$n2\]" [r2cf $n1 $n2]
}
```

{{out}}

```txt

[1;2] -> 0,2
[3;1] -> 3
[23;8] -> 2,1,7
[13;11] -> 1,5,2
[22;7] -> 3,7
[-151;77] -> -2,25,1,2
[14142;10000] -> 1,2,2,2,2,2,1,1,29
[141421;100000] -> 1,2,2,2,2,2,2,3,1,1,3,1,7,2
[1414214;1000000] -> 1,2,2,2,2,2,2,2,3,6,1,2,1,12
[14142136;10000000] -> 1,2,2,2,2,2,2,2,2,2,6,1,2,4,1,1,2
[31;10] -> 3,10
[314;100] -> 3,7,7
[3142;1000] -> 3,7,23,1,2
[31428;10000] -> 3,7,357
[314285;100000] -> 3,7,2857
[3142857;1000000] -> 3,7,142857
[31428571;10000000] -> 3,7,476190,3
[314285714;100000000] -> 3,7,7142857
[3141592653589793;1000000000000000] -> 3,7,15,1,292,1,1,1,2,1,3,1,14,4,2,3,1,12,5,1,5,20,1,11,1,1,1,2

```


### Objectified version


```tcl
package require Tcl 8.6

# General generator class based on coroutines
oo::class create Generator {
    constructor {} {
	coroutine [namespace current]::coro my Apply
    }
    destructor {
	catch {rename [namespace current]::coro {}}
    }
    method Apply {} {
	yield
        # Call the method (defined in subclasses) that actually produces values
	my Produce
	my destroy
	return -code break
    }
    forward generate coro
    method unknown args {
	if {![llength $args]} {
	    tailcall coro
	}
	next {*}$args
    }

    # Various ways to get the sequence from the generator
    method collect {} {
	set result {}
	while 1 {
	    lappend result [my generate]
	}
	return $result
    }
    method take {n {suffix ""}} {
	set result {}
	for {set i 0} {$i < $n} {incr i} {
	    lappend result [my generate]
	}
	while {$suffix ne ""} {
	    my generate
	    lappend result $suffix
	    break
	}
	return $result
    }
}

oo::class create R2CF {
    superclass Generator
    variable a b
    # The constructor converts other kinds of fraction (e.g., 1.23, 22/7) into a
    # form we can handle.
    constructor {n1 {n2 1}} {
	next;  # Delegate to superclass for coroutine management
	if {[regexp {(.*)/(.*)} $n1 -> a b]} {
	    # Nothing more to do; assume we can ignore second argument here
	} elseif {$n1 != int($n1) && [regexp {\.(\d+)} $n1 -> suffix]} {
	    set pow [string length $suffix]
	    set a [expr {int($n1 * 10**$pow)}]
	    set b [expr {$n2 * 10**$pow}]
	} else {
	    set a $n1
	    set b $n2
	}
    }
    # How to actually produce the values of the sequence
    method Produce {} {
	while {$b > 0} {
	    yield [expr {$a / $b}]
	    set b [expr {$a % [set a $b]}]
	}
    }
}

proc printcf {name cf {take ""}} {
    if {$take ne ""} {
	set terms [$cf take $take \u2026]
    } else {
	set terms [$cf collect]
    }
    puts [format "%-15s-> %s" $name [join $terms ,]]
}

foreach {n1 n2} {
    1 2
    3 1
    23 8
    13 11
    22 7
    -151 77
    14142 10000
    141421 100000
    1414214 1000000
    14142136 10000000
    31 10
    314 100
    3142 1000
    31428 10000
    314285 100000
    3142857 1000000
    31428571 10000000
    314285714 100000000
    3141592653589793 1000000000000000
} {
    printcf "\[$n1;$n2\]" [R2CF new $n1 $n2]
}
# Demonstrate parsing of input in forms other than a direct pair of decimals
printcf "1.5" [R2CF new 1.5]
printcf "23/7" [R2CF new 23/7]
```

{{out}}

```txt

[1;2]          -> 0,2
[3;1]          -> 3
[23;8]         -> 2,1,7
[13;11]        -> 1,5,2
[22;7]         -> 3,7
[-151;77]      -> -2,25,1,2
[14142;10000]  -> 1,2,2,2,2,2,1,1,29
[141421;100000]-> 1,2,2,2,2,2,2,3,1,1,3,1,7,2
[1414214;1000000]-> 1,2,2,2,2,2,2,2,3,6,1,2,1,12
[14142136;10000000]-> 1,2,2,2,2,2,2,2,2,2,6,1,2,4,1,1,2
[31;10]        -> 3,10
[314;100]      -> 3,7,7
[3142;1000]    -> 3,7,23,1,2
[31428;10000]  -> 3,7,357
[314285;100000]-> 3,7,2857
[3142857;1000000]-> 3,7,142857
[31428571;10000000]-> 3,7,476190,3
[314285714;100000000]-> 3,7,7142857
[3141592653589793;1000000000000000]-> 3,7,15,1,292,1,1,1,2,1,3,1,14,4,2,3,1,12,5,1,5,20,1,11,1,1,1,2
1.5            -> 1,2
23/7           -> 3,3,2

```



## XPL0


```XPL0
include c:\cxpl\codes;
real Val;

proc R2CF(N1, N2, Lev);         \Output continued fraction for N1/N2
int  N1, N2, Lev;
int  Quot, Rem;
[if Lev=0 then Val:= 0.0;
Quot:= N1/N2;
Rem:= rem(0);
IntOut(0, Quot);
if Rem then [ChOut(0, if Lev then ^, else ^;);  R2CF(N2, Rem, Lev+1)];
Val:= Val + float(Quot);        \generate value from continued fraction
if Lev then Val:= 1.0/Val;
];

int I, Data;
[Data:= [1,2, 3,1, 23,8, 13,11, 22,7, 0];
Format(0, 15);
I:= 0;
while Data(I) do
   [IntOut(0, Data(I));  ChOut(0, ^/);  IntOut(0, Data(I+1));  ChOut(0, 9\tab\);
   ChOut(0, ^[);  R2CF(Data(I), Data(I+1), 0);  ChOut(0, ^]);  ChOut(0, 9\tab\);
   RlOut(0, Val);  CrLf(0);
   I:= I+2];
]
```

{{out}}

```txt

1/2     [0;2]    5.000000000000000E-001
3/1     [3]      3.000000000000000E+000
23/8    [2;1,7]  2.875000000000000E+000
13/11   [1;5,2]  1.181818181818180E+000
22/7    [3;7]    3.142857142857140E+000

```



## zkl

Two iterators; one light weight, one heavy weight.

Light weight, explicit state:

```zkl
fcn r2cf(nom,dnom){ // -->Walker (iterator)
   Walker.tweak(fcn(state){
      nom,dnom:=state;
      if(dnom==0) return(Void.Stop);
      n,d:=nom.divr(dnom);
      state.clear(dnom,d);
      n
   }.fp(List(nom,dnom)))  // partial application (light weight closure)
}
```

Heavy weight, implicit state:

```zkl
fcn r2cf2(nom,dnom){ // -->Generator (heavy weight Walker)
   Utils.Generator(fcn(nom,dnom){
      while(dnom){
	 n,d:=nom.divr(dnom); nom,dnom=dnom,d;
	 vm.yield(n);
      }
      Void.Stop;
   },nom,dnom)
}
```

Both of the above return an iterator so they function the same:

```zkl
foreach nom,dnom in (T(T(1,2), T(3,1), T(23,8), T(13,11), T(22,7),
	T(14142,10000), T(141421,100000), T(1414214,1000000),
	T(14142136,10000000))){
   r2cf(nom,dnom).walk(25).println();  // print up to 25 numbers
}
```

{{out}}

```txt

L(0,2)
L(3)
L(2,1,7)
L(1,5,2)
L(3,7)
L(1,2,2,2,2,2,1,1,29)
L(1,2,2,2,2,2,2,3,1,1,3,1,7,2)
L(1,2,2,2,2,2,2,2,3,6,1,2,1,12)
L(1,2,2,2,2,2,2,2,2,2,6,1,2,4,1,1,2)

```

