+++
title = "AKS test for primes"
description = ""
date = 2019-10-06T00:13:42Z
aliases = []
[extra]
id = 17175
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}

The [http://www.cse.iitk.ac.in/users/manindra/algebra/primality_v6.pdf AKS algorithm] for testing whether a number is prime is a polynomial-time algorithm based on an elementary theorem about Pascal triangles.

The theorem on which the test is based can be stated as follows:

*   a number   <big><big><math>p</math></big></big>   is prime   if and only if   all the coefficients of the polynomial expansion of
::: <big><big><math>(x-1)^p - (x^p - 1)</math></big></big>
are divisible by   <big><big><math>p</math>.</big></big>


;Example:
Using   <big><big><math>p=3</math>:</big></big>

          <big><big>(x-1)^3 - (x^3 - 1)
             = (x^3 - 3x^2 + 3x - 1) - (x^3 - 1)
             = -3x^2 + 3x</big></big>


And all the coefficients are divisible by '''3''',   so '''3''' is prime.


{{alertbox|#ffe4e4|'''Note:'''<br/>This task is '''not''' the AKS primality test.   It is an inefficient exponential time algorithm discovered in the late 1600s and used as an introductory lemma in the AKS derivation.}}


;Task:


# Create a function/subroutine/method that given   <big><big><math>p</math></big></big>   generates the coefficients of the expanded polynomial representation of   <big><big><math>(x-1)^p</math>.</big></big>
# Use the function to show here the polynomial expansions of   <big><big><math>(x-1)^p</math></big></big>   for   <big><big><math>p</math></big></big>   in the range   '''0'''   to at least   '''7''',   inclusive.
# Use the previous function in creating another function that when given   <big><big><math>p</math></big></big>   returns whether   <big><big><math>p</math></big></big>   is prime using the theorem.
# Use your test to generate a list of all primes ''under''   '''35'''.
# '''As a stretch goal''',   generate all primes under   '''50'''   (needs integers larger than 31-bit).


;References:
* [https://en.wikipedia.org/wiki/AKS_primality_test Agrawal-Kayal-Saxena (AKS) primality test] (Wikipedia)
* [http://www.youtube.com/watch?v=HvMSRWTE2mI Fool-Proof Test for Primes] - Numberphile (Video).  The accuracy of this video is disputed -- at best it is an oversimplification.





## 8th


```8th

with: a

: nextrow  \ a -- a
    len
    [ ( drop [1] ),
      ( drop [1,1] ),
      ( ' n:+ y 1 slide 1 push ) ]
    swap 2 min caseof ;

;with

with: n

: .x   \ n --
    dup
    [ ( drop ),
      ( drop "x" . ),
      ( "x^" . . ) ]
    swap 2 min caseof space ;

: .term  \ coef exp -- ; omit coef for 1x^n when n > 0
    over 1 = over 0 > and if  nip .x  else  swap . .x  then ;

: .sgn  \ +/-1 --
    [ "-", null, "+" ]
    swap 1+ caseof . space ;

: .lhs  \ n --
    "(x-1)^" . . ;

: .rhs  \ a -- a
    a:len 1- >r
    1 swap ( third .sgn r@ rot - .term -1 * ) a:each
    nip rdrop ;

: .eqn  \ a -- a
    a:len 1- .lhs " = " . .rhs ;

: .binomials  \ --
    [] ( nextrow .eqn cr ) 8 times drop ;

: primerow? \ a -- a ?
    a:len 3 < if false ;then
    1 a:@ >r   \ 2nd position is the number to check for primality
    true swap ( nip dup 1 = swap r@ mod 0 = or and ) a:each swap
    rdrop ;

: .primes-via-aks \ --
    [] ( nextrow primerow? if 1 a:@ . space then ) 50 times drop ;

;with

.binomials cr
"The primes upto 50 are (via AKS): " . .primes-via-aks cr

bye
```

{{out}}

```txt

(x-1)^0 = + 1
(x-1)^1 = + x - 1
(x-1)^2 = + x^2 - 2x + 1
(x-1)^3 = + x^3 - 3x^2 + 3x - 1
(x-1)^4 = + x^4 - 4x^3 + 6x^2 - 4x + 1
(x-1)^5 = + x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
(x-1)^6 = + x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
(x-1)^7 = + x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1

The primes upto 50 are (via AKS): 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47

```


## ALGOL 68

The code below uses Algol 68 Genie which provides arbitrary precision arithmetic for LONG LONG modes.


```algol68

BEGIN
COMMENT
   Mathematical preliminaries.

   First note that the homogeneous polynomial (a+b)^n is symmetrical
   (to see this just swap the variables a and b).  Therefore its
   coefficients need be calculated only to that of (ab)^{n/2} for even
   n or (ab)^{(n-1)/2} for odd n.

   Second, the coefficients are the binomial coefficients C(n,k) where
   the coefficient of a^k b^(n-k) is C(n,k) = n! / k!  (k-1)!.  This
   leads to an immediate and relatively efficient implementation for
   which we do not need to compute n! before dividing by k! and (k-1)!
   but, rather cancel common factors as we go along.  Further, the
   well-known symmetry identity C(n,k) = C(n, n-k) allows a
   significant reduction in computational effort.

   Third, (x-1)^n is the value of (a + b)^n when a=x and b = -1.  The
   powers of -1 alternate between +1 and -1 so we may as well compute
   (x+1)^n and negate every other coefficient when printing.
COMMENT
   PR precision=300 PR
   MODE LLI = LONG LONG INT;	CO For brevity CO
   PROC choose = (INT n, k) LLI :
   BEGIN
      LLI result := 1;
      INT sym k := (k >= n%2 | n-k | k);	CO Use symmetry CO
      IF sym k > 0 THEN
	 FOR i FROM 0 TO sym k-1
	 DO
	    result TIMESAB (n-i);
	    result OVERAB (i+1)
	 OD
      FI;
      result
   END;
   PROC coefficients = (INT n) [] LLI :
   BEGIN
      [0:n] LLI a;
      FOR i FROM 0 TO n%2
      DO
	 a[i] := a[n-i] := choose (n, i)		CO Use symmetry CO
      OD;
      a
   END;
COMMENT
   First print the polynomials (x-1)^n, remembering to alternate signs
   and to tidy up the constant term, the x^1 term and the x^n term.
   This means we must treat (x-1)^0 and (x-1)^1 specially
COMMENT
   FOR n FROM 0 TO 7
   DO
      [0:n] LLI a := coefficients (n);
      printf (($"(x-1)^", g(0), " = "$, n));
      CASE n+1 IN
         printf (($g(0)l$, a[0])),
         printf (($"x - ", g(0)l$, a[1]))
      OUT
         printf (($"x^", g(0)$, n));
         FOR i TO n-2
         DO
	    printf (($xax, g(0), "x^", g(0)$, (ODD i | "-" | "+"), a[i], n-i))
         OD;
         printf (($xax, g(0), "x"$, (ODD (n-1) | "-" | "+"), a[n-1]));
         printf (($xaxg(0)l$, (ODD n | "-" | "+"), a[n]))
      ESAC
OD;
COMMENT
   Finally, for the "AKS" portion of the task, the sign of the
   coefficient has no effect on its divisibility by p so, once again,
   we may as well use the positive coefficients.  Symmetry clearly
   reduces the necessary number of tests by a factor of two.
COMMENT
   PROC is prime = (INT n) BOOL :
   BEGIN
      BOOL prime := TRUE;
      FOR i FROM 1 TO n%2 WHILE prime DO prime := choose (n, i) MOD n = 0 OD;
      prime
   END;
   print ("Primes < 50 are ");
   FOR n FROM 2 TO 50 DO (is prime (n) | printf (($g(0)x$, n)) ) OD;
   print (newline);
   print ("And just to show off, the primes between 900 and 1000 are ");
   FOR n FROM 900 TO 1000 DO IF is prime (n) THEN printf (($g(0)x$, n)) FI OD;
   print (newline)
END

```

{{out}}

```txt

(x-1)^0 = 1
(x-1)^1 = x - 1
(x-1)^2 = x^2 - 2x + 1
(x-1)^3 = x^3 - 3x^2 + 3x - 1
(x-1)^4 = x^4 - 4x^3 + 6x^2 - 4x + 1
(x-1)^5 = x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
(x-1)^6 = x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
(x-1)^7 = x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1
Primes < 50 are 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
And just to show off, the primes between 900 and 1000 are 907 911 919 929 937 941 947 953 967 971 977 983 991 997

```



## AutoHotkey

{{works with|AutoHotkey L}}

```autohotkey
; 1. Create a function/subroutine/method that given p generates the coefficients of the expanded polynomial representation of (x-1)^p.
; Function modified from http://rosettacode.org/wiki/Pascal%27s_triangle#AutoHotkey
pascalstriangle(n=8) ; n rows of Pascal's triangle
{
	p := Object(), z:=Object()
	Loop, % n
		Loop, % row := A_Index
			col := A_Index
			, p[row, col] := row = 1 and col = 1
				? 1
				: (p[row-1, col-1] = "" ; math operations on blanks return blanks; I want to assume zero
					? 0
					: p[row-1, col-1])
				- (p[row-1, col] = ""
					? 0
					: p[row-1, col])
	Return p
}

; 2. Use the function to show here the polynomial expansions of p for p in the range 0 to at least 7, inclusive.
For k, v in pascalstriangle()
{
	s .= "`n(x-1)^" k-1 . "="
	For k, w in v
		s .= "+" w "x^" k-1
}
s := RegExReplace(s, "\+-", "-")
s := RegExReplace(s, "x\^0", "")
s := RegExReplace(s, "x\^1", "x")
Msgbox % clipboard := s

; 3. Use the previous function in creating another function that when given p returns whether p is prime using the AKS test.
aks(n)
{
	isnotprime := False
	For k, v in pascalstriangle(n+1)[n+1]
		(k != 1 and k != n+1) ? isnotprime |= !(v // n = v / n) ; if any is not divisible, returns true
	Return !isnotprime
}

; 4. Use your AKS test to generate a list of all primes under 35.
i := 49
p := pascalstriangle(i+1)
Loop, % i
{
	n := A_Index
	isnotprime := False
	For k, v in p[n+1]
		(k != 1 and k != n+1) ? isnotprime |= !(v // n = v / n) ; if any is not divisible, returns true
	t .= isnotprime ? "" : A_Index " "
}
Msgbox % t
Return
```

{{out}}

```txt
(x-1)^0=+1
(x-1)^1=-1+1x
(x-1)^2=+1-2x+1x^2
(x-1)^3=-1+3x-3x^2+1x^3
(x-1)^4=+1-4x+6x^2-4x^3+1x^4
(x-1)^5=-1+5x-10x^2+10x^3-5x^4+1x^5
(x-1)^6=+1-6x+15x^2-20x^3+15x^4-6x^5+1x^6
(x-1)^7=-1+7x-21x^2+35x^3-35x^4+21x^5-7x^6+1x^7

1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
```


Function maxes out at i = 61 as AutoHotkey supports up to 64-bit signed integers.


## Bracmat

Bracmat automatically normalizes symbolic expressions with the algebraic binary operators <code>+</code>, <code>*</code>, <code>^</code> and <code>\L</code> (logartithm). It can differentiate such expressions using the <code>\D</code> binary operator. (These operators were implemented in Bracmat before all other operators!). Some algebraic values can exist in two evaluated forms. The equivalent <code>x*(a+b)</code> and <code>x*a+x*b</code> are both considered "normal", but <code>x*(a+b)+-1</code> is not, and therefore expanded to <code>-1+a*x+b*x</code>. This is used in the <code>forceExpansion</code> function to convert e.g. <code>x*(a+b)</code> to <code>x*a+x*b</code>.

The primality test uses a pattern that looks for a fractional factor. If such a factor is found, the test fails. Otherwise it succeeds.

```bracmat
( (forceExpansion=.1+!arg+-1)
& (expandx-1P=.forceExpansion$((x+-1)^!arg))
& ( isPrime
  =
    .         forceExpansion
            $ (!arg^-1*(expandx-1P$!arg+-1*(x^!arg+-1)))
          : ?+/*?+?
        & ~`
      |
  )
& out$"Polynomial representations of (x-1)^p for p <= 7 :"
& -1:?n
&   whl
  ' ( 1+!n:~>7:?n
    & out$(str$("n=" !n ":") expandx-1P$!n)
    )
& 1:?n
& :?primes
&   whl
  ' ( 1+!n:~>50:?n
    & ( isPrime$!n&!primes !n:?primes
      |
      )
    )
& out$"2 <= Primes <= 50:"
& out$!primes
);
```

Output:

```txt
Polynomial representations of (x-1)^p for p <= 7 :
n=0: 1
n=1: -1+x
n=2: 1+-2*x+x^2
n=3: -1+3*x+-3*x^2+x^3
n=4: 1+-4*x+6*x^2+-4*x^3+x^4
n=5: -1+5*x+-10*x^2+10*x^3+-5*x^4+x^5
n=6: 1+-6*x+15*x^2+-20*x^3+15*x^4+-6*x^5+x^6
  n=7:
    -1
  + 7*x
  + -21*x^2
  + 35*x^3
  + -35*x^4
  + 21*x^5
  + -7*x^6
  + x^7
2 <= Primes <= 50:
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
```

The AKS test kan be written more concisely than the task describes. This prints the primes between 980 and 1000:

```bracmat
( out$"Primes between 980 and 1000, short version:"
& 980:?n
&   whl
  ' ( !n+1:<1000:?n
    & ( 1+!n^-1*((x+-1)^!n+-1*(x^!n+-1))+-1:?+/*?+?
      | out$!n
      )
    )
);
```

Output:

```txt
Primes between 980 and 1000, short version:
983
991
997
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

long long c[100];

void coef(int n)
{
	int i, j;

	if (n < 0 || n > 63) abort(); // gracefully deal with range issue

	for (c[i=0] = 1; i < n; c[0] = -c[0], i++)
		for (c[1 + (j=i)] = 1; j > 0; j--)
			c[j] = c[j-1] - c[j];
}

int is_prime(int n)
{
	int i;

	coef(n);
	c[0] += 1, c[i=n] -= 1;
	while (i-- && !(c[i] % n));

	return i < 0;
}

void show(int n)
{
	do printf("%+lldx^%d", c[n], n); while (n--);
}

int main(void)
{
	int n;

	for (n = 0; n < 10; n++) {
		coef(n);
		printf("(x-1)^%d = ", n);
		show(n);
		putchar('\n');
	}

	printf("\nprimes (never mind the 1):");
	for (n = 1; n <= 63; n++)
		if (is_prime(n))
			printf(" %d", n);

	putchar('\n');
	return 0;
}
```


The ugly output:

```txt

(x-1)^0 = +1x^0
(x-1)^1 = +1x^1-1x^0
(x-1)^2 = +1x^2-2x^1+1x^0
(x-1)^3 = +1x^3-3x^2+3x^1-1x^0
(x-1)^4 = +1x^4-4x^3+6x^2-4x^1+1x^0
(x-1)^5 = +1x^5-5x^4+10x^3-10x^2+5x^1-1x^0
(x-1)^6 = +1x^6-6x^5+15x^4-20x^3+15x^2-6x^1+1x^0
(x-1)^7 = +1x^7-7x^6+21x^5-35x^4+35x^3-21x^2+7x^1-1x^0
(x-1)^8 = +1x^8-8x^7+28x^6-56x^5+70x^4-56x^3+28x^2-8x^1+1x^0
(x-1)^9 = +1x^9-9x^8+36x^7-84x^6+126x^5-126x^4+84x^3-36x^2+9x^1-1x^0

primes (never mind the 1): 1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61

```



## C++

{{trans|Pascal}}

```cpp

#include <iomanip>
#include <iostream>
using namespace std;

const int pasTriMax = 61;

uint64_t pasTri[pasTriMax + 1];

void pascalTriangle(unsigned long n)
// Calculate the n'th line 0.. middle
{
    unsigned long j, k;

    pasTri[0] = 1;
    j = 1;
    while (j <= n)
    {
        j++;
        k = j / 2;
        pasTri[k] = pasTri[k - 1];
        for ( ;k >= 1; k--)
            pasTri[k] += pasTri[k - 1];
    }
}

bool isPrime(unsigned long n)
{
    if (n > pasTriMax)
    {
        cout << n << " is out of range" << endl;
        exit(1);
    }

    pascalTriangle(n);
    bool res = true;
    int i = n / 2;
    while (res && (i > 1))
    {
        res = res && (pasTri[i] % n == 0);
        i--;
    }
    return res;
}

void expandPoly(unsigned long n)
{
    const char vz[] = {'+', '-'};

    if (n > pasTriMax)
    {
        cout << n << " is out of range" << endl;
        exit(1);
    }

    switch (n)
    {
        case 0:
            cout << "(x-1)^0 = 1" << endl;
            break;
        case 1:
            cout << "(x-1)^1 = x-1" << endl;
            break;
        default:
            pascalTriangle(n);
            cout << "(x-1)^" << n << " = ";
            cout << "x^" << n;
            bool bVz = true;
            int nDiv2 = n / 2;
            for (unsigned long j = n - 1; j > nDiv2; j--, bVz = !bVz)
                cout << vz[bVz] << pasTri[n - j] << "*x^" << j;
            for (unsigned long j = nDiv2; j > 1; j--, bVz = !bVz)
                cout << vz[bVz] << pasTri[j] << "*x^" << j;
            cout << vz[bVz] << pasTri[1] << "*x";
            bVz = !bVz;
            cout << vz[bVz] << pasTri[0] << endl;
            break;
    }
}

int main()
{
    for (unsigned long n = 0; n <= 9; n++)
        expandPoly(n);
    for (unsigned long n = 2; n <= pasTriMax; n++)
        if (isPrime(n))
            cout << setw(3) << n;
    cout << endl;
}

```

{{out}}

```txt

(x-1)^0 = 1
(x-1)^1 = x-1
(x-1)^2 = x^2-2*x+1
(x-1)^3 = x^3-3*x^2+3*x-1
(x-1)^4 = x^4-4*x^3+6*x^2-4*x+1
(x-1)^5 = x^5-5*x^4+10*x^3-10*x^2+5*x-1
(x-1)^6 = x^6-6*x^5+15*x^4-20*x^3+15*x^2-6*x+1
(x-1)^7 = x^7-7*x^6+21*x^5-35*x^4+35*x^3-21*x^2+7*x-1
(x-1)^8 = x^8-8*x^7+28*x^6-56*x^5+70*x^4-56*x^3+28*x^2-8*x+1
(x-1)^9 = x^9-9*x^8+36*x^7-84*x^6+126*x^5-126*x^4+84*x^3-36*x^2+9*x-1
  2  3  5  7 11 13 17 19 23 29 31 37 41 43 47 53 59 61

```


=={{header|C sharp|C#}}==
{{trans|C}}

```csharp

using System;
    public class AksTest
    {
        static long[] c = new long[100];

        static void Main(string[] args)
        {
        for (int n = 0; n < 10; n++) {
		coef(n);
		Console.Write("(x-1)^" + n + " = ");
		show(n);
		Console.WriteLine("");
	}
	   Console.Write("Primes:");
	  for (int n = 1; n <= 63; n++)
	     if (is_prime(n))
	       Console.Write(n + " ");

	    Console.WriteLine('\n');
            Console.ReadLine();
        }

        static void coef(int n)
        {
            int i, j;

            if (n < 0 || n > 63) System.Environment.Exit(0);// gracefully deal with range issue

            for (c[i = 0] = 1L; i < n; c[0] = -c[0], i++)
                for (c[1 + (j = i)] = 1L; j > 0; j--)
                    c[j] = c[j - 1] - c[j];
        }

        static bool is_prime(int n)
        {
            int i;

            coef(n);
            c[0] += 1;
            c[i = n] -= 1;

            while (i-- != 0 && (c[i] % n) == 0) ;

            return i < 0;
        }

        static void show(int n)
	    {
		    do {
                Console.Write("+" + c[n] + "x^" + n);
		    }while (n-- != 0);
	    }
    }

```



## Clojure

The *' function is an arbitrary precision multiplication.

```clojure
(defn c
  "kth coefficient of (x - 1)^n"
  [n k]
  (/ (apply *' (range n (- n k) -1))
     (apply *' (range k 0 -1))
     (if (and (even? k) (< k n)) -1 1)))

(defn cs
  "coefficient series for (x - 1)^n, k=[0..n]"
  [n]
  (map #(c n %) (range (inc n))))

(defn aks? [p] (->> (cs p) rest butlast (every? #(-> % (mod p) zero?))))

(println "coefficient series n (k[0] .. k[n])")
(doseq [n (range 10)] (println n (cs n)))
(println)
(println "primes < 50 per AKS:" (filter aks? (range 2 50)))
```

{{output}}

```txt
coefficient series n (k[0] .. k[n])
0 (1)
1 (-1 1)
2 (-1 2 1)
3 (-1 3 -3 1)
4 (-1 4 -6 4 1)
5 (-1 5 -10 10 -5 1)
6 (-1 6 -15 20 -15 6 1)
7 (-1 7 -21 35 -35 21 -7 1)
8 (-1 8 -28 56 -70 56 -28 8 1)
9 (-1 9 -36 84 -126 126 -84 36 -9 1)

primes < 50 per AKS: (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
```



## CoffeeScript


```coffeescript
pascal = () ->
    a = []
    return () ->
        if a.length is 0 then a = [1]
        else
            b = (a[i] + a[i+1] for i in [0 ... a.length - 1])
            a = [1].concat(b).concat [1]

show = (a) ->
    show_x = (e) ->
        switch e
            when 0 then ""
            when 1 then "x"
            else "x^#{e}"

    degree = a.length - 1
    str = "(x - 1)^#{degree} ="
    sgn = 1

    for i in [0...a.length]
        str += ' ' + (if sgn > 0 then "+" else "-") + ' ' + a[i] + show_x(degree - i)
        sgn = -sgn

    return str

primerow = (row) ->
    degree = row.length - 1
    row[1 ... degree].every (x) -> x % degree is 0

p = pascal()
console.log show p() for i in [0..7]

p = pascal()
p(); p()  # skip 0 and 1

primes = (i+1 for i in [1..49] when primerow p())

console.log ""
console.log "The primes upto 50 are: #{primes}"
```

{{out}}

```txt
(x - 1)^0 = + 1
(x - 1)^1 = + 1x - 1
(x - 1)^2 = + 1x^2 - 2x + 1
(x - 1)^3 = + 1x^3 - 3x^2 + 3x - 1
(x - 1)^4 = + 1x^4 - 4x^3 + 6x^2 - 4x + 1
(x - 1)^5 = + 1x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
(x - 1)^6 = + 1x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
(x - 1)^7 = + 1x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1

The primes upto 50 are: 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47
```



## Common Lisp


```lisp
(defun coefficients (p)
  (cond
    ((= p 0) #(1))

    (t (loop for i from 1 upto p
             for result = #(1 -1) then (map 'vector
                                            #'-
                                            (concatenate 'vector result #(0))
                                            (concatenate 'vector #(0) result))
             finally (return result)))))

(defun primep (p)
  (cond
    ((< p 2) nil)

    (t (let ((c (coefficients p)))
         (decf (elt c 0))
         (loop for i from 0 upto (/ (length c) 2)
               for x across c
               never (/= (mod x p) 0))))))

(defun main ()
  (format t "# p: (x-1)^p for small p:~%")
  (loop for p from 0 upto 7
        do (format t "~D: " p)
           (loop for i from 0
                 for x across (reverse (coefficients p))
                 do (when (>= x 0) (format t "+"))
                    (format t "~D" x)
                    (if (> i 0)
                        (format t "X^~D " i)
                        (format t " ")))
           (format t "~%"))
  (loop for i from 0 to 50
        do (when (primep i) (format t "~D " i)))
  (format t "~%"))
```

{{out}}

```txt
# p: (x-1)^p for small p:
0: +1
1: -1 +1X^1
2: +1 -2X^1 +1X^2
3: -1 +3X^1 -3X^2 +1X^3
4: +1 -4X^1 +6X^2 -4X^3 +1X^4
5: -1 +5X^1 -10X^2 +10X^3 -5X^4 +1X^5
6: +1 -6X^1 +15X^2 -20X^3 +15X^4 -6X^5 +1X^6
7: -1 +7X^1 -21X^2 +35X^3 -35X^4 +21X^5 -7X^6 +1X^7
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
```



## Crystal

{{trans|Ruby}}

```ruby
def x_minus_1_to_the(p)
  p.times.reduce([1]) do |ex, _|
    ([0_i64] + ex).zip(ex + [0]).map { |x,y| x - y }
  end
end

def prime?(p)
  return false if p < 2
  coeff = x_minus_1_to_the(p)[1..p/2] # only need half of coeff terms
  coeff.all?{ |n| n%p == 0 }
end

8.times do |n|
  puts "(x-1)^#{n} = " +
  x_minus_1_to_the(n).map_with_index{ |c, p|
    p.zero? ? c.to_s : (c < 0 ? " - " : " + ") + (c.abs == 1 ? "x" : "#{c.abs}x") + (p == 1 ? "" : "^#{p}")
  }.join
end

puts "\nPrimes below 50:", 50.times.select {|n| prime? n}.join(',')

```


{{out}}

```txt

(x-1)^0 = 1
(x-1)^1 = -1 + x
(x-1)^2 = 1 - 2x + x^2
(x-1)^3 = -1 + 3x - 3x^2 + x^3
(x-1)^4 = 1 - 4x + 6x^2 - 4x^3 + x^4
(x-1)^5 = -1 + 5x - 10x^2 + 10x^3 - 5x^4 + x^5
(x-1)^6 = 1 - 6x + 15x^2 - 20x^3 + 15x^4 - 6x^5 + x^6
(x-1)^7 = -1 + 7x - 21x^2 + 35x^3 - 35x^4 + 21x^5 - 7x^6 + x^7

Primes below 50:
2,3,5,7,11,13,17,19,23,29,31,37,41,43,47

```



## D

{{trans|Python}}

```d
import std.stdio, std.range, std.algorithm, std.string, std.bigint;

BigInt[] expandX1(in uint p) pure /*nothrow*/ {
    if (p == 0) return [1.BigInt];
    typeof(return) r = [1.BigInt, BigInt(-1)];
    foreach (immutable _; 1 .. p)
        r = zip(r~0.BigInt, 0.BigInt~r).map!(xy => xy[0]-xy[1]).array;
    r.reverse();
    return r;
}

bool aksTest(in uint p) pure /*nothrow*/ {
    if (p < 2) return false;
    auto ex = p.expandX1;
    ex[0]++;
    return !ex[0 .. $ - 1].any!(mult => mult % p);
}

void main() {
    "# p: (x-1)^p for small p:".writeln;
    foreach (immutable p; 0 .. 12)
        writefln("%3d: %s", p, p.expandX1.zip(iota(p + 1)).retro
                 .map!q{"%+dx^%d ".format(a[])}.join.replace("x^0", "")
                 .replace("^1 ", " ").replace("+", "+ ")
                 .replace("-", "- ").replace(" 1x", " x")[2 .. $]);

    "\nSmall primes using the AKS test:".writeln;
    101.iota.filter!aksTest.writeln;
}
```

{{out}}

```txt
# p: (x-1)^p for small p:
  0: 1
  1: x - 1
  2: x^2 - 2x + 1
  3: x^3 - 3x^2 + 3x - 1
  4: x^4 - 4x^3 + 6x^2 - 4x + 1
  5: x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
  6: x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
  7: x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1
  8: x^8 - 8x^7 + 28x^6 - 56x^5 + 70x^4 - 56x^3 + 28x^2 - 8x + 1
  9: x^9 - 9x^8 + 36x^7 - 84x^6 + 126x^5 - 126x^4 + 84x^3 - 36x^2 + 9x - 1
 10: x^10 - 10x^9 + 45x^8 - 120x^7 + 210x^6 - 252x^5 + 210x^4 - 120x^3 + 45x^2 - 10x + 1
 11: x^11 - 11x^10 + 55x^9 - 165x^8 + 330x^7 - 462x^6 + 462x^5 - 330x^4 + 165x^3 - 55x^2 + 11x - 1

Small primes using the AKS test:
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
```



## EchoLisp

We use the math.lib library and the poly functions to compute and display the required polynomials. A polynomial P(x) = a0 +a1*x + .. an*x^n is a list of coefficients (a0 a1 .... an).

```lisp

(lib 'math.lib)
;; 1 - x^p  :  P = (1 0 0 0 ... 0 -1)
(define (mono p) (append (list 1) (make-list (1- p) 0) (list -1)))

;; compute (x-1)^p ,  p >= 1
(define (aks-poly p)
	(poly-pow (list -1 1) p))

;;
(define (show-them n)
	(for ((p (in-range 1 n)))
		(writeln 'p p (poly->string 'x (aks-poly p)))))

;; aks-test
;; P = (x-1)^p + 1 - x^p
(define (aks-test p)
	(let ((P (poly-add (mono p) (aks-poly p)))
	    (test (lambda(a) (zero? (modulo a p))))) ;; p divides a[i] ?
	    (apply and (map test P)))) ;; returns #t if true for all a[i]

```

{{Output}}

```lisp

(show-them 13) →
p     1     x -1
p     2     x^2 -2x +1
p     3     x^3 -3x^2 +3x -1
p     4     x^4 -4x^3 +6x^2 -4x +1
p     5     x^5 -5x^4 +10x^3 -10x^2 +5x -1
p     6     x^6 -6x^5 +15x^4 -20x^3 +15x^2 -6x +1
p     7     x^7 -7x^6 +21x^5 -35x^4 +35x^3 -21x^2 +7x -1
p     8     x^8 -8x^7 +28x^6 -56x^5 +70x^4 -56x^3 +28x^2 -8x +1
p     9     x^9 -9x^8 +36x^7 -84x^6 +126x^5 -126x^4 +84x^3 -36x^2 +9x -1
p     10     x^10 -10x^9 +45x^8 -120x^7 +210x^6 -252x^5 +210x^4 -120x^3 +45x^2 -10x +1
p     11     x^11 -11x^10 +55x^9 -165x^8 +330x^7 -462x^6 +462x^5 -330x^4 +165x^3 -55x^2 +11x -1
p     12     x^12 -12x^11 +66x^10 -220x^9 +495x^8 -792x^7 +924x^6 -792x^5 +495x^4 -220x^3 +66x^2 -12x +1

(lib 'bigint)
Lib: bigint.lib loaded.

(for ((p (in-range 2 100)))
    (when (aks-test p) (write p)))  →

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```


## Elena

{{trans|C#}}
ELENA 4.x :

```elena
import extensions;

singleton AksTest
{
    static long[] c := new long[](100);

    coef(int n)
    {
        int i := 0;
        int j := 0;

        if ((n < 0) || (n > 63)) { AbortException.raise() }; // gracefully deal with range issue

        c[i] := 1l;
        for (int i := 0, i < n, i += 1) {
            c[1 + i] := 1l;
            for (int j := i, j > 0, j -= 1) {
                c[j] := c[j - 1] - c[j]
            };
            c[0] := c[0].Negative
        }
    }

    bool is_prime(int n)
    {
        int i := n;

        self.coef(n);
        c[0] := c[0] + 1;
        c[i] := c[i] - 1;

        i -= 1;
        while (i + 1 != 0 && c[i+1].mod(n) == 0)
        {
            i -= 1
        };

        ^ i < 0
    }

    show(int n)
    {
        int i := n;
        i += 1;
        while(i != 0)
        {
            i -= 1;
            console.print("+",c[i],"x^",i)
        }
    }
}

public program()
{
    for (int n := 0, n < 10, n += 1) {
        AksTest.coef(n);

		console.print("(x-1)^",n," = ");
		AksTest.show(n);
        console.printLine()
    };

    console.print("Primes:");
    for (int n := 1, n <= 63, n += 1) {
        if (AksTest.is_prime(n))
        {
            console.print(n," ")
        }
    };

    console.printLine().readChar()
}
```

{{out}}

```txt

(x-1)^0 = +1x^0
(x-1)^1 = +1x^1+-1x^0
(x-1)^2 = +1x^2+-2x^1+1x^0
(x-1)^3 = +1x^3+-3x^2+3x^1+-1x^0
(x-1)^4 = +1x^4+-4x^3+6x^2+-4x^1+1x^0
(x-1)^5 = +1x^5+-5x^4+10x^3+-10x^2+5x^1+-1x^0
(x-1)^6 = +1x^6+-6x^5+15x^4+-20x^3+15x^2+-6x^1+1x^0
(x-1)^7 = +1x^7+-7x^6+21x^5+-35x^4+35x^3+-21x^2+7x^1+-1x^0
(x-1)^8 = +1x^8+-8x^7+28x^6+-56x^5+70x^4+-56x^3+28x^2+-8x^1+1x^0
(x-1)^9 = +1x^9+-9x^8+36x^7+-84x^6+126x^5+-126x^4+84x^3+-36x^2+9x^1+-1x^0
Primes:1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule AKS do
  def iterate(f, x), do: fn -> [x | iterate(f, f.(x))] end

  def take(0, _lazy), do: []
  def take(n, lazy) do
    [value | next] = lazy.()
    [value | take(n-1, next)]
  end

  def pascal, do: iterate(fn row -> [1 | sum_adj(row)] end, [1])

  defp sum_adj([_] = l), do: l
  defp sum_adj([a, b | _] = row), do: [a+b | sum_adj(tl(row))]

  def show_binomial(row) do
    degree = length(row) - 1
    ["(x - 1)^",  to_char_list(degree), " =", binomial_rhs(row, 1, degree)]
  end

  defp show_x(0), do: ""
  defp show_x(1), do: "x"
  defp show_x(n), do: [?x, ?^ | to_char_list(n)]

  defp binomial_rhs([], _, _), do: []
  defp binomial_rhs([coef | coefs], sgn, exp) do
    signchar = if sgn > 0, do: ?+, else: ?-
    [0x20, signchar, 0x20, to_char_list(coef), show_x(exp) | binomial_rhs(coefs, -sgn, exp-1)]
  end

  def primerow(row, n), do: Enum.all?(row, fn coef -> (coef == 1) or (rem(coef, n) == 0) end)

  def main do
    for row <- take(8, pascal), do: IO.puts show_binomial(row)
    IO.write "\nThe primes upto 50: "
    IO.inspect for {row, n} <- Enum.zip(tl(tl(take(51, pascal))), 2..50), primerow(row, n), do: n
  end
end

AKS.main
```


{{out}}

```txt

(x - 1)^0 = + 1
(x - 1)^1 = + 1x - 1
(x - 1)^2 = + 1x^2 - 2x + 1
(x - 1)^3 = + 1x^3 - 3x^2 + 3x - 1
(x - 1)^4 = + 1x^4 - 4x^3 + 6x^2 - 4x + 1
(x - 1)^5 = + 1x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
(x - 1)^6 = + 1x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
(x - 1)^7 = + 1x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1

The primes upto 50: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]

```



## Erlang

{{trans|CoffeeScript}}

The Erlang io module can print out lists of characters with any level of nesting as a flat string.  (e.g. ["Er", ["la", ["n"]], "g"] prints as "Erlang") which is useful when constructing the strings to print out for the binomial expansions.  The program also shows how lazy lists can be implemented in Erlang.


```erlang
#! /usr/bin/escript

-import(lists, [all/2, seq/2, zip/2]).

iterate(F, X) -> fun() -> [X | iterate(F, F(X))] end.

take(0, _lazy) -> [];
take(N, Lazy) ->
    [Value | Next] = Lazy(),
    [Value | take(N-1, Next)].


pascal() -> iterate(fun (Row) -> [1 | sum_adj(Row)] end, [1]).

sum_adj([_] = L) -> L;
sum_adj([A, B | _] = Row) -> [A+B | sum_adj(tl(Row))].


show_binomial(Row) ->
    Degree = length(Row) - 1,
    ["(x - 1)^",  integer_to_list(Degree), " =", binomial_rhs(Row, 1, Degree)].

show_x(0) -> "";
show_x(1) -> "x";
show_x(N) -> [$x, $^ | integer_to_list(N)].

binomial_rhs([], _, _) -> [];
binomial_rhs([Coef | Coefs], Sgn, Exp) ->
    SignChar = if Sgn > 0 -> $+; true -> $- end,
    [$ , SignChar, $ , integer_to_list(Coef), show_x(Exp) | binomial_rhs(Coefs, -Sgn, Exp-1)].


primerow(Row, N) -> all(fun (Coef) -> (Coef =:= 1) or (Coef rem N =:= 0) end, Row).

main(_) ->
    [io:format("~s~n", [show_binomial(Row)]) || Row <- take(8, pascal())],
    io:format("~nThe primes upto 50: ~p~n",
               [[N || {Row, N} <- zip(tl(tl(take(51, pascal()))), seq(2, 50)),
                      primerow(Row, N)]]).

```

{{out}}

```txt
(x - 1)^0 = + 1
(x - 1)^1 = + 1x - 1
(x - 1)^2 = + 1x^2 - 2x + 1
(x - 1)^3 = + 1x^3 - 3x^2 + 3x - 1
(x - 1)^4 = + 1x^4 - 4x^3 + 6x^2 - 4x + 1
(x - 1)^5 = + 1x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
(x - 1)^6 = + 1x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
(x - 1)^7 = + 1x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1

The primes upto 50: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]

```



## Factor


```factor
USING: combinators formatting io kernel make math math.parser
math.polynomials prettyprint sequences ;
IN: rosetta-code.aks-test

! Polynomials are represented by the math.polynomials vocabulary
! as sequences with the highest exponent on the right. Hence
! { -1 1 } represents x - 1.
: (x-1)^ ( n -- seq ) { -1 1 } swap p^ ;

: choose-exp ( n -- str )
    { { 0 [ "" ] } { 1 [ "x" ] } [ "x^%d" sprintf ] } case ;

: choose-coeff ( n -- str )
    [ dup neg? [ neg "- " ] [ "+ " ] if % # ] "" make ;

: terms ( coeffs-seq -- terms-seq )
    [ [ choose-coeff ] [ choose-exp append ] bi* ] map-index ;

: (.p) ( n -- str ) (x-1)^ terms <reversed> " " join 3 tail ;

: .p ( n -- ) dup zero? [ drop "1" ] [ (.p) ] if print ;

: show-poly ( n -- ) [ "(x-1)^%d = " printf ] [ .p ] bi ;

: part1 ( -- ) 8 <iota> [ show-poly ] each ;

: (prime?) ( n -- ? )
    (x-1)^ rest but-last dup first [ mod 0 = not ] curry find
    nip not ;

: prime? ( n -- ? ) dup 2 < [ drop f ] [ (prime?) ] if ;

: part2 ( -- )
    "Primes up to 50 via AKS:" print
    50 <iota> [ prime? ] filter . ;

: aks-test ( -- ) part1 nl part2 ;

MAIN: aks-test
```

{{out}}

```txt

(x-1)^0 = 1
(x-1)^1 = x - 1
(x-1)^2 = x^2 - 2x + 1
(x-1)^3 = x^3 - 3x^2 + 3x - 1
(x-1)^4 = x^4 - 4x^3 + 6x^2 - 4x + 1
(x-1)^5 = x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
(x-1)^6 = x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
(x-1)^7 = x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1

Primes up to 50 via AKS:
V{ 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 }

```



## Forth


```forth
: coeffs ( u -- nu ... n0 ) \ coefficients of (x-1)^u
   1 swap 1+ dup 1 ?do over over i - i */ negate swap loop drop ;

: prime? ( u -- f )
   dup 2 < if drop false exit then
   dup >r coeffs 1+
   \ if not prime, this loop consumes at most half the coefficients, otherwise all
   begin dup 1 <> while
      r@ mod 0= while
   repeat then rdrop
   dup 1 = >r
   begin 1 = until
   r> ;

: .monom ( u1 u2 -- )
   dup 0> if [char] + emit then 0 .r ?dup if ." x^" . else space then ;
: .poly ( u -- )
   dup >r coeffs 0 r> 1+ 0 ?do
      tuck swap .monom 1+
   loop ;

: main
   11 0 ?do i . ." : " i .poly cr loop cr
   50 1 ?do i prime? if i . then loop
   cr ;
```

{{out}}

```txt

0 : +1
1 : -1 +1x^1
2 : +1 -2x^1 +1x^2
3 : -1 +3x^1 -3x^2 +1x^3
4 : +1 -4x^1 +6x^2 -4x^3 +1x^4
5 : -1 +5x^1 -10x^2 +10x^3 -5x^4 +1x^5
6 : +1 -6x^1 +15x^2 -20x^3 +15x^4 -6x^5 +1x^6
7 : -1 +7x^1 -21x^2 +35x^3 -35x^4 +21x^5 -7x^6 +1x^7
8 : +1 -8x^1 +28x^2 -56x^3 +70x^4 -56x^5 +28x^6 -8x^7 +1x^8
9 : -1 +9x^1 -36x^2 +84x^3 -126x^4 +126x^5 -84x^6 +36x^7 -9x^8 +1x^9
10 : +1 -10x^1 +45x^2 -120x^3 +210x^4 -252x^5 +210x^6 -120x^7 +45x^8 -10x^9 +1x^10

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
```


## Fortran


```fortran

program aks
  implicit none

  ! Coefficients of polynomial expansion
  integer(kind=16), dimension(:), allocatable :: coeffs
  integer(kind=16) :: n
  ! Character variable for I/O
  character(len=40) :: tmp

  ! Point #2
  do n = 0, 7
    write(tmp, *) n
    call polynomial_expansion(n, coeffs)
    write(*, fmt='(A)', advance='no') '(x - 1)^'//trim(adjustl(tmp))//' ='
    call print_polynom(coeffs)
  end do

  ! Point #4
  do n = 2, 35
    if (is_prime(n)) write(*, '(I4)', advance='no') n
  end do
  write(*, *)

  ! Point #5
  do n = 2, 124
    if (is_prime(n)) write(*, '(I4)', advance='no') n
  end do
  write(*, *)

  if (allocated(coeffs)) deallocate(coeffs)
contains
  ! Calculate coefficients of (x - 1)^n using binomial theorem
  subroutine polynomial_expansion(n, coeffs)
    integer(kind=16), intent(in) :: n
    integer(kind=16), dimension(:), allocatable, intent(out) :: coeffs
    integer(kind=16) :: i, j

    if (allocated(coeffs)) deallocate(coeffs)

    allocate(coeffs(n + 1))

    do i = 1, n + 1
      coeffs(i) = binomial(n, i - 1)*(-1)**(n - i - 1)
    end do
  end subroutine

  ! Calculate binomial coefficient using recurrent relation, as calculation
  ! using factorial overflows too quickly.
  function binomial(n, k) result (res)
    integer(kind=16), intent(in) :: n, k
    integer(kind=16) :: res
    integer(kind=16) :: i

    if (k == 0) then
      res = 1
      return
    end if

    res = 1
    do i = 0, k - 1
      res = res*(n - i)/(i + 1)
    end do
  end function

  ! Outputs polynomial with given coefficients
  subroutine print_polynom(coeffs)
    integer(kind=16), dimension(:), allocatable, intent(in) :: coeffs
    integer(kind=4) :: i, p
    character(len=40) :: cbuf, pbuf
    logical(kind=1) :: non_zero

    if (.not. allocated(coeffs)) return

    non_zero = .false.

    do i = 1, size(coeffs)
      if (coeffs(i) .eq. 0) cycle

      p = i - 1
      write(cbuf, '(I40)') abs(coeffs(i))
      write(pbuf, '(I40)') p

      if (non_zero) then
        if (coeffs(i) .gt. 0) then
          write(*, fmt='(A)', advance='no') ' + '
        else
          write(*, fmt='(A)', advance='no') ' - '
        endif
      else
        if (coeffs(i) .gt. 0) then
          write(*, fmt='(A)', advance='no') '   '
        else
          write(*, fmt='(A)', advance='no') ' - '
        endif
      endif

      if (p .eq. 0) then
        write(*, fmt='(A)', advance='no') trim(adjustl(cbuf))
      elseif (p .eq. 1) then
        if (coeffs(i) .eq. 1) then
          write(*, fmt='(A)', advance='no') 'x'
        else
          write(*, fmt='(A)', advance='no') trim(adjustl(cbuf))//'x'
        end if
      else
        if (coeffs(i) .eq. 1) then
          write(*, fmt='(A)', advance='no') 'x^'//trim(adjustl(pbuf))
        else
          write(*, fmt='(A)', advance='no') &
            trim(adjustl(cbuf))//'x^'//trim(adjustl(pbuf))
        end if
      end if
      non_zero = .true.
    end do

    write(*, *)
  end subroutine

  ! Test if n is prime using AKS test. Point #3.
  function is_prime(n) result (res)
    integer(kind=16), intent (in) :: n
    logical(kind=1) :: res
    integer(kind=16), dimension(:), allocatable :: coeffs
    integer(kind=16) :: i

    call polynomial_expansion(n, coeffs)
    coeffs(1) = coeffs(1) + 1
    coeffs(n + 1) = coeffs(n + 1) - 1

    res = .true.

    do i = 1, n + 1
      res = res .and. (mod(coeffs(i), n) == 0)
    end do

    if (allocated(coeffs)) deallocate(coeffs)
  end function
end program aks

```

{{out}}

```txt

(x - 1)^0 =   1
(x - 1)^1 = - 1 + x
(x - 1)^2 =   1 - 2x + x^2
(x - 1)^3 = - 1 + 3x - 3x^2 + x^3
(x - 1)^4 =   1 - 4x + 6x^2 - 4x^3 + x^4
(x - 1)^5 = - 1 + 5x - 10x^2 + 10x^3 - 5x^4 + x^5
(x - 1)^6 =   1 - 6x + 15x^2 - 20x^3 + 15x^4 - 6x^5 + x^6
(x - 1)^7 = - 1 + 7x - 21x^2 + 35x^3 - 35x^4 + 21x^5 - 7x^6 + x^7
   2   3   5   7  11  13  17  19  23  29  31
   2   3   5   7  11  13  17  19  23  29  31  37  41  43  47  53  59  61  67  71  73  79  83  89  97 101 103 107 109 113

```




## Go


```go
package main

import "fmt"

func bc(p int) []int64 {
    c := make([]int64, p+1)
    r := int64(1)
    for i, half := 0, p/2; i <= half; i++ {
        c[i] = r
        c[p-i] = r
        r = r * int64(p-i) / int64(i+1)
    }
    for i := p - 1; i >= 0; i -= 2 {
        c[i] = -c[i]
    }
    return c
}

func main() {
    for p := 0; p <= 7; p++ {
        fmt.Printf("%d:  %s\n", p, pp(bc(p)))
    }
    for p := 2; p < 50; p++ {
        if aks(p) {
            fmt.Print(p, " ")
        }
    }
    fmt.Println()
}

var e = []rune("²³⁴⁵⁶⁷")

func pp(c []int64) (s string) {
    if len(c) == 1 {
        return fmt.Sprint(c[0])
    }
    p := len(c) - 1
    if c[p] != 1 {
        s = fmt.Sprint(c[p])
    }
    for i := p; i > 0; i-- {
        s += "x"
        if i != 1 {
            s += string(e[i-2])
        }
        if d := c[i-1]; d < 0 {
            s += fmt.Sprintf(" - %d", -d)
        } else {
            s += fmt.Sprintf(" + %d", d)
        }
    }
    return
}

func aks(p int) bool {
    c := bc(p)
    c[p]--
    c[0]++
    for _, d := range c {
        if d%int64(p) != 0 {
            return false
        }
    }
    return true
}
```

{{out}}

```txt

0:  1
1:  x - 1
2:  x² - 2x + 1
3:  x³ - 3x² + 3x - 1
4:  x⁴ - 4x³ + 6x² - 4x + 1
5:  x⁵ - 5x⁴ + 10x³ - 10x² + 5x - 1
6:  x⁶ - 6x⁵ + 15x⁴ - 20x³ + 15x² - 6x + 1
7:  x⁷ - 7x⁶ + 21x⁵ - 35x⁴ + 35x³ - 21x² + 7x - 1
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47

```



## FreeBASIC


```FreeBASIC
'METHOD -- Use the Pascal triangle to retrieve the coefficients
'UPPER LIMIT OF FREEBASIC ULONGINT GETS PRIMES UP TO 70
Sub string_split(s_in As String,char As String,result() As String)
    Dim As String s=s_in,var1,var2
    Dim As Integer n,pst
    #macro split(stri,char,var1,var2)
    pst=Instr(stri,char)
    var1="":var2=""
    If pst<>0 Then
        var1=Mid(stri,1,pst-1)
        var2=Mid(stri,pst+1)
    Else
        var1=stri
    End If
    Redim Preserve result(1 To 1+n-((Len(var1)>0)+(Len(var2)>0)))
    result(n+1)=var1
    #endmacro
    Do
        split(s,char,var1,var2):n=n+1:s=var2
    Loop Until var2=""
    Redim Preserve result(1 To Ubound(result)-1)
End Sub

'Get Pascal triangle components
Function pasc(n As Integer,flag As Integer=0) As String
    n+=1
    Dim As Ulongint V(n):V(1)=1ul
    Dim As String s,sign
    For r  As Integer= 2 To n
        s=""
        For i As Integer = r To 1 Step -1
            V(i) +=  V(i-1)
            If i Mod 2=1 Then sign="" Else sign="-"
            s+=sign+Str(V(i))+","
        Next i
    Next r
    If flag Then 'formatted output
        Dim As String i,i2,i3,g
        Redim As String a(0)
        string_split(s,",",a())
        For n1 As Integer=1 To Ubound(a)
            If Left(a(n1),1)="-" Then sign="" Else sign="+"
            If n1=Ubound(a) Then i2="" Else i2=a(n1)
            If n1=2 Then i3="x" Else i3="x^"+Str(n1-1)
            If n1=1 Then i="":sign=" " Else i=i3
            g+=sign+i2+i+" "
        Next n1
        g="(x-1)^"+Str(n-1)+" = "+g
        Return g
    End If
    Return s
End Function

Function isprime(num As Integer) As Integer
    Redim As String a(0)
    string_split(pasc(num),",",a())
    For n As Integer=Lbound(a)+1 To Ubound(a)-1
        If (Valulng(Ltrim(a(n),"-"))) Mod num<>0 Then Return 0
    Next n
    Return -1
End Function
'
### ==============================

'Formatted output
For n As Integer=1 To 9
    Print pasc(n,1)
Next n

Print
'Limit of Freebasic Ulongint sets about 70 max
Print "Primes up to 70:"
For n As Integer=2 To 70
    If isprime(n) Then Print n;
Next n

Sleep
```

{{out}}

```txt
(x-1)^1 =  -1 +x
(x-1)^2 =  1 -2x +x^2
(x-1)^3 =  -1 +3x -3x^2 +x^3
(x-1)^4 =  1 -4x +6x^2 -4x^3 +x^4
(x-1)^5 =  -1 +5x -10x^2 +10x^3 -5x^4 +x^5
(x-1)^6 =  1 -6x +15x^2 -20x^3 +15x^4 -6x^5 +x^6
(x-1)^7 =  -1 +7x -21x^2 +35x^3 -35x^4 +21x^5 -7x^6 +x^7
(x-1)^8 =  1 -8x +28x^2 -56x^3 +70x^4 -56x^5 +28x^6 -8x^7 +x^8
(x-1)^9 =  -1 +9x -36x^2 +84x^3 -126x^4 +126x^5 -84x^6 +36x^7 -9x^8 +x^9

Primes up to 70:
 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67
```



## Haskell


```haskell
expand p = scanl (\z i -> z * (p-i+1) `div` i) 1 [1..p]


test p | p < 2     = False
       | otherwise = and [mod n p == 0 | n <- init . tail $ expand p]


printPoly [1] = "1"
printPoly p   = concat [ unwords [pow i, sgn (l-i), show (p!!(i-1))]
                       | i <- [l-1,l-2..1] ] where
    l = length p
    sgn i = if even i then "+" else "-"
    pow i = take i "x^" ++ if i > 1 then show i else ""


main = do
    putStrLn "-- p: (x-1)^p for small p"
    putStrLn $ unlines [show i ++ ": " ++ printPoly (expand i) | i <- [0..10]]
    putStrLn "-- Primes up to 100:"
    print (filter test [1..100])
```

{{out}}

```txt
-- p: (x-1)^p for small p
0: 1
1: x - 1
2: x^2 - 2x + 1
3: x^3 - 3x^2 + 3x - 1
4: x^4 - 4x^3 + 6x^2 - 4x + 1
5: x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
6: x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
7: x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1
8: x^8 - 8x^7 + 28x^6 - 56x^5 + 70x^4 - 56x^3 + 28x^2 - 8x + 1
9: x^9 - 9x^8 + 36x^7 - 84x^6 + 126x^5 - 126x^4 + 84x^3 - 36x^2 + 9x - 1
10: x^10 - 10x^9 + 45x^8 - 120x^7 + 210x^6 - 252x^5 + 210x^4 - 120x^3 + 45x^2 - 10x + 1

-- Primes up to 100:
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
```



## Idris


```idris
import Data.Vect

-- Computes Binomial Coefficients
binCoef : Nat -> Nat -> Nat
binCoef _ Z = (S Z)
binCoef (S n) (S k) =
    if n == k then (S Z) else ((S n) * (binCoef n k)) `div` (S k)

-- Binomial Expansion Of (x - 1)^p
expansion : (n : Nat) -> Vect (S n) Integer
expansion n = expansion' n 1
  where
    expansion' : (n : Nat) -> Integer -> Vect (S n) Integer
    expansion' (S m) s = s * (toIntegerNat $ binCoef n (n `minus` (S m))) ::
                            expansion' m (s * -1)
    expansion' Z s = [s]


showExpansion : Vect n Integer -> String
showExpansion [] = " "
showExpansion (x::xs) {n = S k} = (if x < 0 then "-" else "") ++
        term x k ++ showExpansion' xs
  where
        term : Integer -> Nat -> String
        term x n = if n == 0 then (show (abs x)) else
                      (if (abs x) == 1 then "" else
                          (show (abs x))) ++ "x" ++
                            (if n == 1 then "" else "^" ++ show n)

        sign : Integer -> String
        sign x = if x >= 0 then " + " else " - "

        showExpansion' : Vect m Integer -> String
        showExpansion' [] = ""
        showExpansion' (y::ys) {m = S k} = sign y ++ term y k ++
                                                showExpansion' ys


natToFin' : (m : Nat) -> Fin (S m)
natToFin' n with (natToFin n (S n))
    natToFin' n | Just y = y


isPrime : Nat -> Bool
isPrime Z = False
isPrime (S Z ) = False
isPrime n = foldl (\divs, term => divs && (term `mod` (toIntegerNat n)) == 0)
              True (fullExpansion $ expansion n)

    -- (x - 1)^p - ((x^p) - 1)
    where fullExpansion : Vect (S m) Integer -> Vect (S m) Integer
          fullExpansion (x::xs) {m} = updateAt (natToFin' m) (+1) $ (x-1)::xs


printExpansions : Nat -> IO ()
printExpansions n = do
      putStrLn "-- p: (x-1)^p for small p"
      sequence_ $ map printExpansion [0..n]
  where printExpansion : Nat -> IO ()
        printExpansion n = do
            print n
            putStr ": "
            putStrLn $ showExpansion $ expansion n


main : IO()
main = do
  printExpansions 10
  putStrLn "\n-- Primes Up To 100:"
  putStrLn $ show $ filter isPrime [0..100]
```

{{out}}

```txt
-- p: (x-1)^p for small p
0: 1
1: x - 1
2: x^2 - 2x + 1
3: x^3 - 3x^2 + 3x - 1
4: x^4 - 4x^3 + 6x^2 - 4x + 1
5: x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
6: x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
7: x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1
8: x^8 - 8x^7 + 28x^6 - 56x^5 + 70x^4 - 56x^3 + 28x^2 - 8x + 1
9: x^9 - 9x^8 + 36x^7 - 84x^6 + 126x^5 - 126x^4 + 84x^3 - 36x^2 + 9x - 1
10: x^10 - 10x^9 + 45x^8 - 120x^7 + 210x^6 - 252x^5 + 210x^4 - 120x^3 + 45x^2 - 10x + 1

-- Primes Up To 100:
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
```



## J


'''Solution''':
```j
   binomialExpansion =:  (!~ * _1 ^ 2 | ]) i.&.:<:         NB. 1) Create a function that gives the coefficients of (x-1)^p.
   testAKS           =:  0 *./ .= ] | binomialExpansion    NB. 3) Use that function to create another which determines whether p is prime using AKS.
```


'''Examples''':
```j
   binomialExpansion&.> i. 8   NB.  2) show the polynomial expansions p in the range 0 to at 7 inclusive.
+-++--+----+-------+-----------+---------------+------------------+
|0||_2|_3 3|_4 6 _4|_5 10 _10 5|_6 15 _20 15 _6|_7 21 _35 35 _21 7|
+-++--+----+-------+-----------+---------------+------------------+
   (#~ testAKS&> ) 2+i. 35     NB. 4) Generate a list of all primes under 35.
2 3 5 7 11 13 17 19 23 29 31
   (#~ testAKS&> ) 2+i. 50     NB. 5) [stretch] Generate all primes under 50
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
   i.&.:(_1&p:) 50             NB. Double-check our results using built-in prime filter.
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
```



## Java


{{trans|C}}
'''Solution''':
```java
public class AksTest {
    private static final long[] c = new long[64];

    public static void main(String[] args) {
        for (int n = 0; n < 10; n++) {
            coeff(n);
            show(n);
        }

        System.out.print("Primes:");
        for (int n = 1; n < c.length; n++)
            if (isPrime(n))
                System.out.printf(" %d", n);

        System.out.println();
    }

    static void coeff(int n) {
        c[0] = 1;
        for (int i = 0; i < n; c[0] = -c[0], i++) {
            c[1 + i] = 1;
            for (int j = i; j > 0; j--)
                c[j] = c[j - 1] - c[j];
        }
    }

    static boolean isPrime(int n) {
        coeff(n);
        c[0]++;
        c[n]--;

        int i = n;
        while (i-- != 0 && c[i] % n == 0)
            continue;
        return i < 0;
    }

    static void show(int n) {
        System.out.print("(x-1)^" + n + " =");
        for (int i = n; i >= 0; i--) {
            System.out.print(" + " + c[i] + "x^" + i);
        }
        System.out.println();
    }
}
```

Output:

```txt

(x-1)^0 = +1x^0
(x-1)^1 = +1x^1+-1x^0
(x-1)^2 = +1x^2+-2x^1+1x^0
(x-1)^3 = +1x^3+-3x^2+3x^1+-1x^0
(x-1)^4 = +1x^4+-4x^3+6x^2+-4x^1+1x^0
(x-1)^5 = +1x^5+-5x^4+10x^3+-10x^2+5x^1+-1x^0
(x-1)^6 = +1x^6+-6x^5+15x^4+-20x^3+15x^2+-6x^1+1x^0
(x-1)^7 = +1x^7+-7x^6+21x^5+-35x^4+35x^3+-21x^2+7x^1+-1x^0
(x-1)^8 = +1x^8+-8x^7+28x^6+-56x^5+70x^4+-56x^3+28x^2+-8x^1+1x^0
(x-1)^9 = +1x^9+-9x^8+36x^7+-84x^6+126x^5+-126x^4+84x^3+-36x^2+9x^1+-1x^0
Primes: 1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61

```



## JavaScript

{{trans|CoffeeScript}}

```javascript
var i, p, pascal, primerow, primes, show, _i;

pascal = function() {
  var a;
  a = [];
  return function() {
    var b, i;
    if (a.length === 0) {
      return a = [1];
    } else {
      b = (function() {
        var _i, _ref, _results;
        _results = [];
        for (i = _i = 0, _ref = a.length - 1; 0 <= _ref ? _i < _ref : _i > _ref; i = 0 <= _ref ? ++_i : --_i) {
          _results.push(a[i] + a[i + 1]);
        }
        return _results;
      })();
      return a = [1].concat(b).concat([1]);
    }
  };
};

show = function(a) {
  var degree, i, sgn, show_x, str, _i, _ref;
  show_x = function(e) {
    switch (e) {
      case 0:
        return "";
      case 1:
        return "x";
      default:
        return "x^" + e;
    }
  };
  degree = a.length - 1;
  str = "(x - 1)^" + degree + " =";
  sgn = 1;
  for (i = _i = 0, _ref = a.length; 0 <= _ref ? _i < _ref : _i > _ref; i = 0 <= _ref ? ++_i : --_i) {
    str += ' ' + (sgn > 0 ? "+" : "-") + ' ' + a[i] + show_x(degree - i);
    sgn = -sgn;
  }
  return str;
};

primerow = function(row) {
  var degree;
  degree = row.length - 1;
  return row.slice(1, degree).every(function(x) {
    return x % degree === 0;
  });
};

p = pascal();

for (i = _i = 0; _i <= 7; i = ++_i) {
  console.log(show(p()));
}

p = pascal();

p();

p();

primes = (function() {
  var _j, _results;
  _results = [];
  for (i = _j = 1; _j <= 49; i = ++_j) {
    if (primerow(p())) {
      _results.push(i + 1);
    }
  }
  return _results;
})();

console.log("");

console.log("The primes upto 50 are: " + primes);
```

{{out}}

```txt
(x - 1)^0 = + 1
(x - 1)^1 = + 1x - 1
(x - 1)^2 = + 1x^2 - 2x + 1
(x - 1)^3 = + 1x^3 - 3x^2 + 3x - 1
(x - 1)^4 = + 1x^4 - 4x^3 + 6x^2 - 4x + 1
(x - 1)^5 = + 1x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
(x - 1)^6 = + 1x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
(x - 1)^7 = + 1x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1

The primes upto 50 are: 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47
```

Reviewed (ES6):

```javascript
function pascal(n) {
	var cs = []; if (n) while (n--) coef(); return coef
	function coef() {
		if (cs.length === 0) return cs = [1];
		for (var t=[1,1], i=cs.length-1; i; i-=1) t.splice( 1, 0, cs[i-1]+cs[i] ); return cs = t
	}
}

function show(cs) {
	for (var s='', sgn=true, i=0, deg=cs.length-1; i<=deg; sgn=!sgn, i+=1) {
		s += ' ' + (sgn ? '+' : '-') + cs[i] + (e => e==0 ? '' : e==1 ? 'x' : 'x<sup>' + e + '</sup>')(deg-i)
	}
	return '(x-1)<sup>' + deg + '</sup> =' + s;
}

function isPrime(cs) {
	var deg=cs.length-1; return cs.slice(1, deg).every( function(c) { return c % deg === 0 } )
}

var coef=pascal(); for (var i=0; i<=7; i+=1) document.write(show(coef()), '
')

document.write('
Primes: ');
for (var coef=pascal(2), n=2; n<=50; n+=1) if (isPrime(coef())) document.write(' ', n)
```

{{output}}
 (x-1)<sup>0</sup> = +1
 (x-1)<sup>1</sup> = +1x -1
 (x-1)<sup>2</sup> = +1x<sup>2</sup> -2x +1
 (x-1)<sup>3</sup> = +1x<sup>3</sup> -3x<sup>2</sup> +3x -1
 (x-1)<sup>4</sup> = +1x<sup>4</sup> -4x<sup>3</sup> +6x<sup>2</sup> -4x +1
 (x-1)<sup>5</sup> = +1x<sup>5</sup> -5x<sup>4</sup> +10x<sup>3</sup> -10x<sup>2</sup> +5x -1
 (x-1)<sup>6</sup> = +1x<sup>6</sup> -6x<sup>5</sup> +15x<sup>4</sup> -20x<sup>3</sup> +15x<sup>2</sup> -6x +1
 (x-1)<sup>7</sup> = +1x<sup>7</sup> -7x<sup>6</sup> +21x<sup>5</sup> -35x<sup>4</sup> +35x<sup>3</sup> -21x<sup>2</sup> +7x -1

 Primes: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47
{{trans|C}}

```JavaScript
function coef(n) {
 	for (var c=[1], i=0; i<n; c[0]=-c[0], i+=1) {
		c[i+1]=1; for (var j=i; j; j-=1) c[j] = c[j-1]-c[j]
	}
	return c
}

function show(cs)	{
	var s='', n=cs.length-1
	do s += (cs[n]>0 ? ' +' : ' ') + cs[n] + (n==0 ? '' : n==1 ? 'x' :'x<sup>'+n+'</sup>'); while (n--)
	return s
}

function isPrime(n) {
	var cs=coef(n), i=n-1; while (i-- && cs[i]%n == 0);
	return i < 1
}

for (var n=0; n<=7; n++) document.write('(x-1)<sup>',n,'</sup> = ', show(coef(n)), '
')

document.write('
Primes: ');
for (var n=2; n<=50; n++) if (isPrime(n)) document.write(' ', n)
```

{{output}}
 (x-1)<sup>0</sup> = +1
 (x-1)<sup>1</sup> = +1x -1
 (x-1)<sup>2</sup> = +1x<sup>2</sup> -2x +1
 (x-1)<sup>3</sup> = +1x<sup>3</sup> -3x<sup>2</sup> +3x -1
 (x-1)<sup>4</sup> = +1x<sup>4</sup> -4x<sup>3</sup> +6x<sup>2</sup> -4x +1
 (x-1)<sup>5</sup> = +1x<sup>5</sup> -5x<sup>4</sup> +10x<sup>3</sup> -10x<sup>2</sup> +5x -1
 (x-1)<sup>6</sup> = +1x<sup>6</sup> -6x<sup>5</sup> +15x<sup>4</sup> -20x<sup>3</sup> +15x<sup>2</sup> -6x +1
 (x-1)<sup>7</sup> = +1x<sup>7</sup> -7x<sup>6</sup> +21x<sup>5</sup> -35x<sup>4</sup> +35x<sup>3</sup> -21x<sup>2</sup> +7x -1

 Primes: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47


## jq

{{works with|jq|1.5rc1}}

In the [[#Prolog]] section of this page, it is shown how the
symmetry of rows in a Pascal triangle can be used to yield a more
efficient test of primality than is apparently envisioned by the
problem statement.  The key concept is the "OptPascal row", which is
just the longest non-decreasing sequence of the corresponding
Pascal row.  In this article, the focus will therefore be on OptPascal rows.

NOTE: jq uses IEEE 754 64-bit numbers and thus if builtin
arithmetic is used, is_prime will only be accurate up to 96 by
this method because of loss of precision.  The program below, however, can
easily be adapted to use a BigInt library such as the one at
https://github.com/joelpurra/jq-bigint

```jq
# add_pairs is a helper function for optpascal/0
# Input: an OptPascal array
# Output: the next OptPascal array (obtained by adding adjacent items,
# but if the last two items are unequal, then their sum is repeated)
def add_pairs:
  if length <= 1 then .
  elif length == 2 then (.[0] + .[1]) as $S
  | if (.[0] == .[1]) then [$S]
    else [$S,$S]
    end
  else [.[0] + .[1]] + (.[1:]|add_pairs)
  end;

# Input: an OptPascal row
# Output: the next OptPascalRow
def next_optpascal: [1] + add_pairs;

# generate a stream of OptPascal arrays, beginning with []
def optpascals: [] | recurse(next_optpascal);

# generate a stream of Pascal arrays
def pascals:
  # pascalize takes as input an OptPascal array and produces
  # the corresponding Pascal array;
  # if the input ends in a pair, then peel it off before reversing it.
  def pascalize:
  . + ((if .[-2] == .[-1] then .[0:-2] else .[0:-1] end) | reverse);

  optpascals | pascalize;

# Input: integer n
# Output: the n-th Pascal row
def pascal: nth(.; pascals);

def optpascal: nth(.; optpascals);
```


 '''Task 1:''' "A method to generate the coefficients of (x-1)^p"

```jq
def coefficients:
  def alternate_signs: . as $in
  | reduce range(0; length) as $i ([]; . + [$in[$i] * (if $i % 2 == 0 then 1 else -1 end )]);
  (.+1) | pascal | alternate_signs;
```


'''Task 2:''' "Show here the polynomial expansions of (x − 1)^p for p in the range 0 to at least 7, inclusive."

```jq
range(0;8) | "Coefficient for (x - 1)^\(.): \(coefficients)"
```

{{out}}

```sh
Coefficients for (x - 1)^0: [1]
Coefficients for (x - 1)^1: [1,-1]
Coefficients for (x - 1)^2: [1,-2,1]
Coefficients for (x - 1)^3: [1,-3,3,-1]
Coefficients for (x - 1)^4: [1,-4,6,-4,1]
Coefficients for (x - 1)^5: [1,-5,10,-10,5,-1]
Coefficients for (x - 1)^6: [1,-6,15,-20,15,-6,1]
Coefficients for (x - 1)^7: [1,-7,21,-35,35,-21,7,-1]
```


'''Task 3:''' Prime Number Test

For brevity, we show here only the relatively efficient solution based on optpascal/0:

```jq
def is_prime:
  . as $N
  | if . < 2 then false
    else (1+.) | optpascal
    | all(  .[2:][]; . % $N == 0 )
    end;
```


'''Task 4:''' "Use your AKS test to generate a list of all primes under 35."

```jq
range(0;36) | select(is_prime)
```

{{out}}

```sh
2
3
5
7
11
13
17
19
23
29
31
```


'''Task 5:''' "As a stretch goal, generate all primes under 50."

```ja
[range(0;50) | select(is_prime)]
```

{{out}}

```sh
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]
```



## Julia

'''Task 1'''


```Julia

function polycoefs(n::Int64)
    pc = typeof(n)[]
    if n < 0
        return pc
    end
    sgn = one(n)
    for k in n:-1:0
        push!(pc, sgn*binomial(n, k))
        sgn = -sgn
    end
    return pc
end

```


Perhaps this should be done with a comprehension, but properly accounting for the sign is tricky in that case.

'''Task 2'''


```Julia
using Printf

function stringpoly(n::Int64)
    if n < 0
        return ""
    end
    st = @sprintf "(x - 1)^{%d} & = & " n
    for (i, c) in enumerate(polycoefs(n))
        if i == 1
            op = ""
            ac = c
        elseif c < 0
            op = "-"
            ac = abs(c)
        else
            op = "+"
            ac = abs(c)
        end
        p = n + 1 - i
        if p == 0
            st *= @sprintf " %s %d\\\\" op ac
        elseif ac == 1
            st *= @sprintf " %s x^{%d}" op p
        else
            st *= @sprintf " %s %dx^{%d}" op ac p
        end
    end
    return st
end

```


Of course this could be simpler, but this produces a nice payoff in typeset equations that do on include extraneous characters (leading pluses and coefficients of 1).

'''Task 3'''


```Julia

function isaksprime(n::Int64)
    if n < 2
        return false
    end
    for c in polycoefs(n)[2:(end-1)]
        if c%n != 0
            return false
        end
    end
    return true
end

```


'''Task 4'''


```Julia

println("<math>")
println("\\begin{array}{lcl}")
for i in 0:10
    println(stringpoly(i))
end
println("\\end{array}")
println("</math>\n")

L = 50
print("AKS primes less than ", L, ":  ")
sep = ""
for i in 1:L
    if isaksprime(i)
        print(sep, i)
        sep = ", "
    end
end
println()

```


{{out}}

<math>
\begin{array}{lcl}
(x - 1)^{0} & = &   1\\
(x - 1)^{1} & = &   x^{1} - 1\\
(x - 1)^{2} & = &   x^{2} - 2x^{1} + 1\\
(x - 1)^{3} & = &   x^{3} - 3x^{2} + 3x^{1} - 1\\
(x - 1)^{4} & = &   x^{4} - 4x^{3} + 6x^{2} - 4x^{1} + 1\\
(x - 1)^{5} & = &   x^{5} - 5x^{4} + 10x^{3} - 10x^{2} + 5x^{1} - 1\\
(x - 1)^{6} & = &   x^{6} - 6x^{5} + 15x^{4} - 20x^{3} + 15x^{2} - 6x^{1} + 1\\
(x - 1)^{7} & = &   x^{7} - 7x^{6} + 21x^{5} - 35x^{4} + 35x^{3} - 21x^{2} + 7x^{1} - 1\\
(x - 1)^{8} & = &   x^{8} - 8x^{7} + 28x^{6} - 56x^{5} + 70x^{4} - 56x^{3} + 28x^{2} - 8x^{1} + 1\\
(x - 1)^{9} & = &   x^{9} - 9x^{8} + 36x^{7} - 84x^{6} + 126x^{5} - 126x^{4} + 84x^{3} - 36x^{2} + 9x^{1} - 1\\
(x - 1)^{10} & = &   x^{10} - 10x^{9} + 45x^{8} - 120x^{7} + 210x^{6} - 252x^{5} + 210x^{4} - 120x^{3} + 45x^{2} - 10x^{1} + 1\\
\end{array}
</math>

AKS primes less than 50:  2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47


## Kotlin


```scala
// version 1.1

fun binomial(n: Int, k: Int): Long = when {
    n < 0 || k < 0 -> throw IllegalArgumentException("negative numbers not allowed")
    k == 0         -> 1L
    k == n         -> 1L
    else           -> {
        var prod = 1L
        var div  = 1L
        for (i in 1..k) {
            prod *= (n + 1 - i)
            div  *= i
            if (prod % div == 0L) {
                prod /= div
                div = 1L
            }
        }
        prod
    }
}

fun isPrime(n: Int): Boolean {
    if (n < 2) return false
    return (1 until n).none { binomial(n, it) % n.toLong() != 0L }
}

fun main(args: Array<String>) {
    var coeff: Long
    var sign: Int
    var op: String
    for (n in 0..9) {
        print("(x - 1)^$n = ")
        sign = 1
        for (k in n downTo 0) {
            coeff = binomial(n, k)
            op = if (sign == 1) " + " else " - "
            when (k) {
                n    -> print("x^$n")
                0    -> println("${op}1")
                else -> print("$op${coeff}x^$k")
            }
            if (n == 0) println()
            sign *= -1
        }
    }
    // generate primes under 62
    var p = 2
    val primes = mutableListOf<Int>()
    do {
        if (isPrime(p)) primes.add(p)
        if (p != 2) p += 2 else p = 3
    }
    while (p < 62)
    println("\nThe prime numbers under 62 are:")
    println(primes)
}
```


{{out}}

```txt

(x - 1)^0 = x^0
(x - 1)^1 = x^1 - 1
(x - 1)^2 = x^2 - 2x^1 + 1
(x - 1)^3 = x^3 - 3x^2 + 3x^1 - 1
(x - 1)^4 = x^4 - 4x^3 + 6x^2 - 4x^1 + 1
(x - 1)^5 = x^5 - 5x^4 + 10x^3 - 10x^2 + 5x^1 - 1
(x - 1)^6 = x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x^1 + 1
(x - 1)^7 = x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x^1 - 1
(x - 1)^8 = x^8 - 8x^7 + 28x^6 - 56x^5 + 70x^4 - 56x^3 + 28x^2 - 8x^1 + 1
(x - 1)^9 = x^9 - 9x^8 + 36x^7 - 84x^6 + 126x^5 - 126x^4 + 84x^3 - 36x^2 + 9x^1 - 1

The prime numbers under 62 are:
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61]

```



## Liberty BASIC

{{trans|Pascal}}
{{works with|Just BASIC|any}}

```lb

global pasTriMax
pasTriMax = 61
dim pasTri(pasTriMax + 1)

for n = 0 to 9
  call expandPoly n
next n
for n = 2 to pasTriMax
  if isPrime(n) <> 0 then
    print using("###", n);
  end if
next n
print
end

sub expandPoly n
  n = int(n)
  dim vz$(1)
  vz$(0) = "+"
  vz$(1) = "-"
  if n > pasTriMax then
    print n; " is out of range"
    end
  end if
  select case n
    case 0
      print "(x-1)^0 = 1"
    case 1
      print "(x-1)^1 = x-1"
    case else
      call pascalTriangle n
      print "(x-1)^"; n; " = ";
      print "x^"; n;
      bVz = 1
      nDiv2 = int(n / 2)
      for j = n - 1 to nDiv2 + 1 step -1
        print vz$(bVz); pasTri(n - j); "*x^"; j;
        bVz = abs(1 - bVz)
      next j
      for j = nDiv2 to 2 step -1
        print vz$(bVz); pasTri(j); "*x^"; j;
        bVz = abs(1 - bVz)
      next j
      print vz$(bVz); pasTri(1); "*x";
      bVz = abs(1 - bVz)
      print vz$(bVz); pasTri(0)
  end select
end sub

function isPrime(n)
  n = int(n)
  if n > pasTriMax then
    print n; " is out of range"
    end
  end if
  call pascalTriangle n
  res = 1
  i = int(n / 2)
  while res and (i > 1)
    res = res and (pasTri(i) mod n = 0)
    i = i - 1
  wend
  isPrime = res
end function

sub pascalTriangle n
  rem Calculate the n'th line 0.. middle
  n = int(n)
  pasTri(0) = 1
  j = 1
  while j <= n
    j = j + 1
    k = int(j / 2)
    pasTri(k) = pasTri(k - 1)
    for k = k to 1 step -1
      pasTri(k) = pasTri(k) + pasTri(k - 1)
    next k
  wend
end sub

```

{{out}}

```txt

(x-1)^0 = 1
(x-1)^1 = x-1
(x-1)^2 = x^2-2*x+1
(x-1)^3 = x^3-3*x^2+3*x-1
(x-1)^4 = x^4-4*x^3+6*x^2-4*x+1
(x-1)^5 = x^5-5*x^4+10*x^3-10*x^2+5*x-1
(x-1)^6 = x^6-6*x^5+15*x^4-20*x^3+15*x^2-6*x+1
(x-1)^7 = x^7-7*x^6+21*x^5-35*x^4+35*x^3-21*x^2+7*x-1
(x-1)^8 = x^8-8*x^7+28*x^6-56*x^5+70*x^4-56*x^3+28*x^2-8*x+1
(x-1)^9 = x^9-9*x^8+36*x^7-84*x^6+126*x^5-126*x^4+84*x^3-36*x^2+9*x-1
  2  3  5  7 11 13 17 19 23 29 31 37 41 43 47 53 59 61

```



## Maple

Maple handles algebraic manipulation of polynomials natively.

```Maple>
 for xpr in seq( expand( (x-1)^p ), p = 0 .. 7 ) do print( xpr ) end:
                                            1

                                          x - 1

                                       2
                                      x  - 2 x + 1

                                    3      2
                                   x  - 3 x  + 3 x - 1

                                4      3      2
                               x  - 4 x  + 6 x  - 4 x + 1

                            5      4       3       2
                           x  - 5 x  + 10 x  - 10 x  + 5 x - 1

                        6      5       4       3       2
                       x  - 6 x  + 15 x  - 20 x  + 15 x  - 6 x + 1

                    7      6       5       4       3       2
                   x  - 7 x  + 21 x  - 35 x  + 35 x  - 21 x  + 7 x - 1

```

To implement the primality test, we write the following procedure that uses the (built-in) polynomial expansion to generate a list of coefficients of the expanded polynomial.

```Maple
 polc := p -> [coeffs]( expand( (x-1)^p - (x^p-1) ) ):
```

Use <code>polc</code> to implement <code>prime?</code> which does the primality test.

```Maple
prime? := n -> n > 1 and {op}( map( modp, polc( n ), n ) ) = {0}
```

Of course, rather than calling <code>polc</code>, we can inline it, just for the sake of making the whole thing a one-liner (while adding argument type-checking for good measure):

```Maple
prime? := (n::posint) -> n > 1 and {op}( map( modp, [coeffs]( expand( (x-1)^n - (x^n-1) ) ), n ) ) = {0}
```

This agrees with the built-in primality test <code>isprime</code>:

```Maple>
 evalb( seq( prime?(i), i = 1 .. 1000 ) = seq( isprime( i ), i = 1 .. 1000 ) );
                                          true

```

Use <code>prime?</code> with the built-in Maple <code>select</code> procedure to pick off the primes up to 50:

```Maple>
 select( prime?, [seq](1..50) );
                [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]

```



=={{header|Mathematica}} / {{header|Wolfram Language}}==
Algebraic manipulation is built into Mathematica, so there's no need to create a function to do (x-1)^p

```Mathematica
Print["powers of (x-1)"]
(x - 1)^( Range[0, 7]) // Expand // TableForm
Print["primes under 50"]
poly[p_] := (x - 1)^p - (x^p - 1) // Expand;
coefflist[p_Integer] := Coefficient[poly[p], x, #] & /@ Range[0, p - 1];
AKSPrimeQ[p_Integer] := (Mod[coefflist[p] , p] // Union) == {0};
Select[Range[1, 50], AKSPrimeQ]
```

{{out}}

```txt
powers of (x-1)
1
-1+x
1-2 x+x^2
-1+3 x-3 x^2+x^3
1-4 x+6 x^2-4 x^3+x^4
-1+5 x-10 x^2+10 x^3-5 x^4+x^5
1-6 x+15 x^2-20 x^3+15 x^4-6 x^5+x^6
-1+7 x-21 x^2+35 x^3-35 x^4+21 x^5-7 x^6+x^7

primes under 50
{1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47}
```



## Objeck

{{trans|Java}}

```objeck
class AksTest {
  @c : static : Int[];

  function : Main(args : String[]) ~ Nil {
    @c := Int->New[100];

    for(n := 0; n < 10; n++;) {
      Coef(n);
      "(x-1)^ {$n} = "->Print();
      Show(n);
      '\n'->Print();
    };

    "\nPrimes:"->PrintLine();
    for(n := 2; n <= 63; n++;) {
      if(IsPrime(n)) {
        " {$n}"->Print();
      };
    };
    '\n'->Print();
  }

  function : native : Coef(n : Int) ~ Nil {
    i := 0; j := 0;

    if (n < 0 | n > 63) {
      Runtime->Exit(0);
    };

    for(@c[0] := 1; i < n; i++;) {
      j := i;
      for(@c[1 + j] := 1; j > 0; j--;) {
        @c[j] := @c[j-1] - @c[j];
      };
      @c[0] := @c[0] * -1;
    };
  }

  function : native : IsPrime(n : Int) ~ Bool {
    Coef(n);
    @c[0] += 1; @c[n] -= 1;

    i:=n;
    while (i <> 0 & (@c[i] % n) = 0) {
      i--;
    };

    return i = 0;
  }

  function : Show(n : Int) ~ Nil {
    do {
      value := @c[n];
      "+{$value}x^{$n}"->Print();
    } while (n-- <> 0);
  }
}
```


Output:

```txt

(x-1)^ 0 = +1x^0
(x-1)^ 1 = +1x^1+-1x^0
(x-1)^ 2 = +1x^2+-2x^1+1x^0
(x-1)^ 3 = +1x^3+-3x^2+3x^1+-1x^0
(x-1)^ 4 = +1x^4+-4x^3+6x^2+-4x^1+1x^0
(x-1)^ 5 = +1x^5+-5x^4+10x^3+-10x^2+5x^1+-1x^0
(x-1)^ 6 = +1x^6+-6x^5+15x^4+-20x^3+15x^2+-6x^1+1x^0
(x-1)^ 7 = +1x^7+-7x^6+21x^5+-35x^4+35x^3+-21x^2+7x^1+-1x^0
(x-1)^ 8 = +1x^8+-8x^7+28x^6+-56x^5+70x^4+-56x^3+28x^2+-8x^1+1x^0
(x-1)^ 9 = +1x^9+-9x^8+36x^7+-84x^6+126x^5+-126x^4+84x^3+-36x^2+9x^1+-1x^0

Primes:
 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61
```



## OCaml

{{trans|Clojure}}
Uses [http://github.com/c-cube/gen gen] library for lazy streams and [https://forge.ocamlcore.org/projects/zarith zarith] for arbitrarily sized integers. Runs as is through the [https://github.com/diml/utop utop] REPL.

```OCaml
#require "gen"
#require "zarith"
open Z
let range ?(step=one) i j = if i = j then Gen.empty else Gen.unfold (fun k ->
    if compare i j = compare k j then Some (k, (add step k)) else None) i

(* kth coefficient of (x - 1)^n *)
let coeff n k =
  let numer = Gen.fold mul one
    (range n (sub n k) ~step:minus_one) in
  let denom = Gen.fold mul one
    (range k zero ~step:minus_one) in
  div numer denom |> mul @@
    if
      compare k n < 0 && is_even k
    then
      minus_one
    else
      one

(* coefficient series for (x - 1)^n, k=[0..n] *)
let coeff_series n =
  Gen.map (coeff n) (range zero (succ n))

let middle g = Gen.drop 1 g |> Gen.peek |> Gen.filter_map
  (function (_, None) -> None | (e, _) -> Some e)

let is_mod_p ~p n = rem n p = zero

let aks p =
  coeff_series p |> middle |> Gen.for_all (is_mod_p ~p)

let _ =
  print_endline "coefficient series n (k[0] .. k[n])";
  Gen.iter
    (fun n -> Format.printf "%d (%s)\n" (to_int n)
      (Gen.map to_string (coeff_series n) |> Gen.to_list |> String.concat " "))
    (range zero (of_int 10));
  print_endline "";
  print_endline ("primes < 50 per AKS: " ^
    (Gen.filter aks (range (of_int 2) (of_int 50)) |>
    Gen.map to_string |> Gen.to_list |> String.concat " "))
```


{{output}}

```txt
coefficient series n (k[0] .. k[n])
0 (1)
1 (-1 1)
2 (-1 2 1)
3 (-1 3 -3 1)
4 (-1 4 -6 4 1)
5 (-1 5 -10 10 -5 1)
6 (-1 6 -15 20 -15 6 1)
7 (-1 7 -21 35 -35 21 -7 1)
8 (-1 8 -28 56 -70 56 -28 8 1)
9 (-1 9 -36 84 -126 126 -84 36 -9 1)

primes < 50 per AKS: (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
```


## Oforth


```Oforth
import: mapping

: nextCoef( prev -- [] )
| i |
   Array new 0 over dup
   prev size 1- loop: i [ prev at(i) prev at(i 1+) - over add ]
   0 over add
;

: coefs( n -- [] )
    [ 0, 1, 0 ] #nextCoef times(n) extract(2, n 2 + ) ;

: prime?( n -- b)
    coefs( n ) extract(2, n) conform?( #[n mod 0 == ] ) ;

: aks
| i |
   0 10 for: i [ System.Out "(x-1)^" << i << " = " << coefs( i ) << cr ]
   50 seq filter( #prime? ) apply(#.) printcr
;
```


{{out}}

```txt

(x-1)^0 = [1]
(x-1)^1 = [-1, 1]
(x-1)^2 = [1, -2, 1]
(x-1)^3 = [-1, 3, -3, 1]
(x-1)^4 = [1, -4, 6, -4, 1]
(x-1)^5 = [-1, 5, -10, 10, -5, 1]
(x-1)^6 = [1, -6, 15, -20, 15, -6, 1]
(x-1)^7 = [-1, 7, -21, 35, -35, 21, -7, 1]
(x-1)^8 = [1, -8, 28, -56, 70, -56, 28, -8, 1]
(x-1)^9 = [-1, 9, -36, 84, -126, 126, -84, 36, -9, 1]
(x-1)^10 = [1, -10, 45, -120, 210, -252, 210, -120, 45, -10, 1]
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47

```



## PARI/GP


```parigp
getPoly(n)=('x-1)^n;
vector(8,n,getPoly(n-1))
AKS_slow(n)=my(P=getPoly(n));for(i=1,n-1,if(polcoeff(P,i)%n,return(0))); 1;
AKS(n)=my(X=('x-1)*Mod(1,n));X^n=='x^n-1;
select(AKS, [1..50])
```

{{out}}

```txt
 [1, x - 1, x^2 - 2*x + 1, x^3 - 3*x^2 + 3*x - 1, x^4 - 4*x^3 + 6*x^2 - 4*x + 1, x^5 - 5*x^4 + 10*x^3 - 10*x^2 + 5*x - 1, x^6 - 6*x^5 + 15*x^4 - 20*x^3 + 15*x^2 - 6*x + 1, x^7 - 7*x^6 + 21*x^5 - 35*x^4 + 35*x^3 - 21*x^2 + 7*x - 1]
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
```



## Pascal

{{works with|Free Pascal}}

```pascal

const
  pasTriMax = 61;

type
  TPasTri = array[0 .. pasTriMax] of UInt64;

var
  pasTri: TPasTri;

procedure PascalTriangle(n: LongWord);
// Calculate the n'th line 0.. middle
var
  j, k: LongWord;
begin
  pasTri[0] := 1;
  j := 1;
  while j <= n do
  begin
    Inc(j);
    k := j div 2;
    pasTri[k] := pasTri[k - 1];
    for k := k downto 1 do
      Inc(pasTri[k], pasTri[k - 1]);
  end;
end;

function IsPrime(n: LongWord): Boolean;
var
  i: Integer;
begin
  if n > pasTriMax then
  begin
    WriteLn(n, ' is out of range');
    Halt;
  end;

  PascalTriangle(n);
  Result := true;
  i := n div 2;
  while Result and (i > 1) do
  begin
    Result := Result and (pasTri[i] mod n = 0);
    Dec(i);
  end;
end;

procedure ExpandPoly(n: LongWord);
const
  Vz: array[Boolean] of Char = ('+', '-');
var
  j: LongWord;
  bVz: Boolean;
begin
  if n > pasTriMax then
  begin
    WriteLn(n,' is out of range');
    Halt;
  end;

  case n of
    0: WriteLn('(x-1)^0 = 1');
    1: WriteLn('(x-1)^1 = x-1');
  else
    PascalTriangle(n);
    Write('(x-1)^', n, ' = ');
    Write('x^', n);
    bVz := true;
    for j := n - 1 downto n div 2 + 1 do
    begin
      Write(vz[bVz], pasTri[n - j], '*x^', j);
      bVz := not bVz;
    end;
    for j := n div 2 downto 2 do
    begin
      Write(vz[bVz], pasTri[j], '*x^', j);
      bVz := not bVz;
    end;
    Write(vz[bVz], pasTri[1], '*x');
    bVz := not bVz;
    WriteLn(vz[bVz], pasTri[0]);
  end;
end;

var
  n: LongWord;
begin
  for n := 0 to 9 do
    ExpandPoly(n);
  for n := 2 to pasTriMax do
    if IsPrime(n) then
      Write(n:3);
  WriteLn;
end.
```

;output:

```txt

(x-1)^0 = 1
(x-1)^1 = x-1
(x-1)^2 = x^2-2*x+1
(x-1)^3 = x^3-3*x^2+3*x-1
(x-1)^4 = x^4-4*x^3+6*x^2-4*x+1
(x-1)^5 = x^5-5*x^4+10*x^3-10*x^2+5*x-1
(x-1)^6 = x^6-6*x^5+15*x^4-20*x^3+15*x^2-6*x+1
(x-1)^7 = x^7-7*x^6+21*x^5-35*x^4+35*x^3-21*x^2+7*x-1
(x-1)^8 = x^8-8*x^7+28*x^6-56*x^5+70*x^4-56*x^3+28*x^2-8*x+1
(x-1)^9 = x^9-9*x^8+36*x^7-84*x^6+126*x^5-126*x^4+84*x^3-36*x^2+9*x-1
  2  3  5  7 11 13 17 19 23 29 31 37 41 43 47 53 59 61
```



## Perl


```perl
use strict;
use warnings;
# Select one of these lines.  Math::BigInt is in core, but quite slow.
use Math::BigInt;  sub binomial { Math::BigInt->new(shift)->bnok(shift) }
# use Math::Pari "binomial";
# use ntheory "binomial";

sub binprime {
  my $p = shift;
  return 0 unless $p >= 2;
  # binomial is symmetric, so only test half the terms
  for (1 .. ($p>>1)) { return 0 if binomial($p,$_) % $p }
  1;
}
sub coef {                   # For prettier printing
  my($n,$e) = @_;
  return $n unless $e;
  $n = "" if $n==1;
  $e==1 ? "${n}x" : "${n}x^$e";
}
sub binpoly {
  my $p = shift;
  join(" ", coef(1,$p),
            map { join("",("+","-")[($p-$_)&1]," ",coef(binomial($p,$_),$_)) }
            reverse 0..$p-1 );
}
print "expansions of (x-1)^p:\n";
print binpoly($_),"\n" for 0..9;
print "Primes to 80: [", join(",", grep { binprime($_) } 2..80), "]\n";
```

{{out}}

```txt
expansions of (x-1)^p:
1
x - 1
x^2 - 2x + 1
x^3 - 3x^2 + 3x - 1
x^4 - 4x^3 + 6x^2 - 4x + 1
x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1
x^8 - 8x^7 + 28x^6 - 56x^5 + 70x^4 - 56x^3 + 28x^2 - 8x + 1
x^9 - 9x^8 + 36x^7 - 84x^6 + 126x^5 - 126x^4 + 84x^3 - 36x^2 + 9x - 1
Primes to 80: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79]
```



### Real AKS

The ntheory module has implementations of the full AKS algorithm in Perl, C, and C+GMP.  This is vastly faster than the method used in this task and is polynomial time, but like all current AKS implementations is still ''much'' slower than other methods such as BPSW, APR-CL, and ECPP.

{{libheader|ntheory}}

```perl
use ntheory ":all";
# Uncomment next line to see the r and s values used.  Set to 2 for more detail.
# prime_set_config(verbose => 1);
say join(" ", grep { is_aks_prime($_) } 1_000_000_000 .. 1_000_000_100);
```

{{out}}

```txt
1000000007 1000000009 1000000021 1000000033 1000000087 1000000093 1000000097
```



## Perl 6

{{works with|Rakudo|2015-12}}

The expansions are generated similarly to how most FP languages generate sequences that resemble Pascal's triangle, using a zipwith meta-operator (Z) with subtraction, applied between two lists that add a 0 on either end to the prior list.  Here we define a constant infinite sequence using the <tt>...</tt> sequence operator with a "whatever" endpoint.  In fact, the second term <tt>[1,-1]</tt> could have been generated from the first term, but we put it in there for documentation so the reader can see what direction things are going.

The <tt>polyprime</tt> function pretty much reads like the original description.  Is it "so" that the p'th expansion's coefficients are all divisible by p?  The <tt>.[1 ..^ */2]</tt> slice is done simply to weed out divisions by 1 or by factors we've already tested (since the coefficients are symmetrical in terms of divisibility).  If we wanted to write <tt>polyprime</tt> even more idiomatically, we could have made it another infinite constant list that is just a mapping of the first list, but we decided that would just be showing off.  <tt>:-)</tt>


```perl6
constant expansions = [1], [1,-1], -> @prior { [|@prior,0 Z- 0,|@prior] } ... *;

sub polyprime($p where 2..*) { so expansions[$p].[1 ..^ */2].all %% $p }

# Showing the expansions:

say ' p: (x-1)ᵖ';
say '-----------';

sub super ($n) {
    $n.trans: '0123456789'
           => '⁰¹²³⁴⁵⁶⁷⁸⁹';
}

for ^13 -> $d {
    say $d.fmt('%2i: '), (
        expansions[$d].kv.map: -> $i, $n {
            my $p = $d - $i;
            [~] gather {
                take < + - >[$n < 0] ~ ' ' unless $p == $d;
                take $n.abs                unless $p == $d > 0;
                take 'x'                   if $p > 0;
                take super $p - $i         if $p > 1;
            }
        }
    )
}

#  And testing the function:

print "\nPrimes up to 100:\n  { grep &polyprime, 2..100 }\n";
```


{{out}}

```txt
 p: (x-1)ᵖ
-----------
 0: 1
 1: x - 1
 2: x² - 2x + 1
 3: x³ - 3x² + 3x - 1
 4: x⁴ - 4x³ + 6x² - 4x + 1
 5: x⁵ - 5x⁴ + 10x³ - 10x² + 5x - 1
 6: x⁶ - 6x⁵ + 15x⁴ - 20x³ + 15x² - 6x + 1
 7: x⁷ - 7x⁶ + 21x⁵ - 35x⁴ + 35x³ - 21x² + 7x - 1
 8: x⁸ - 8x⁷ + 28x⁶ - 56x⁵ + 70x⁴ - 56x³ + 28x² - 8x + 1
 9: x⁹ - 9x⁸ + 36x⁷ - 84x⁶ + 126x⁵ - 126x⁴ + 84x³ - 36x² + 9x - 1
10: x¹⁰ - 10x⁹ + 45x⁸ - 120x⁷ + 210x⁶ - 252x⁵ + 210x⁴ - 120x³ + 45x² - 10x + 1
11: x¹¹ - 11x¹⁰ + 55x⁹ - 165x⁸ + 330x⁷ - 462x⁶ + 462x⁵ - 330x⁴ + 165x³ - 55x² + 11x - 1
12: x¹² - 12x¹¹ + 66x¹⁰ - 220x⁹ + 495x⁸ - 792x⁷ + 924x⁶ - 792x⁵ + 495x⁴ - 220x³ + 66x² - 12x + 1

Primes up to 100:
  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```



## Phix


```Phix

-- Does not work for primes above 53, which is actually beyond the original task anyway.
-- Translated from the C version, just about everything is (working) out-by-1, what fun.

sequence c = repeat(0,100)

procedure coef(integer n)
-- out-by-1, ie coef(1)==^0, coef(2)==^1, coef(3)==^2 etc.
    c[n] = 1
    for i=n-1 to 2 by -1 do
        c[i] = c[i]+c[i-1]
    end for
end procedure

function is_prime(integer n)
    coef(n+1); -- (I said it was out-by-1)
    for i=2 to n-1 do   -- (technically "to n" is more correct)
        if remainder(c[i],n)!=0 then
            return 0
        end if
    end for
    return 1
end function

procedure show(integer n)
-- (As per coef, this is (working) out-by-1)
object ci
    for i=n to 1 by -1 do
        ci = c[i]
        if ci=1 then
            if remainder(n-i,2)=0 then
                if i=1 then
                    if n=1 then
                        ci = "1"
                    else
                        ci = "+1"
                    end if
                else
                    ci = ""
                end if
            else
                ci = "-1"
            end if
        else
            if remainder(n-i,2)=0 then
                ci = sprintf("+%d",ci)
            else
                ci = sprintf("-%d",ci)
            end if
        end if
        if i=1 then -- ie ^0
            printf(1,"%s",{ci})
        elsif i=2 then -- ie ^1
            printf(1,"%sx",{ci})
        else
            printf(1,"%sx^%d",{ci,i-1})
        end if
    end for
end procedure

procedure AKS_test_for_primes()
    for n=1 to 10 do -- (0 to 9 really)
        coef(n);
        printf(1,"(x-1)^%d = ", n-1);
        show(n);
        puts(1,'\n');
    end for

    puts(1,"\nprimes (<=53):");
--  coef(2); -- (needed to reset c, if we want to avoid saying 1 is prime...)
    c[2] = 1 -- (this manages "", which is all that call did anyway...)
    for n = 2 to 53 do
        if is_prime(n) then
            printf(1," %d", n);
        end if
    end for
    puts(1,'\n');
    if getc(0) then end if
end procedure

    AKS_test_for_primes()


```

{{out}}

```txt

(x-1)^0 = 1
(x-1)^1 = x-1
(x-1)^2 = x^2-2x+1
(x-1)^3 = x^3-3x^2+3x-1
(x-1)^4 = x^4-4x^3+6x^2-4x+1
(x-1)^5 = x^5-5x^4+10x^3-10x^2+5x-1
(x-1)^6 = x^6-6x^5+15x^4-20x^3+15x^2-6x+1
(x-1)^7 = x^7-7x^6+21x^5-35x^4+35x^3-21x^2+7x-1
(x-1)^8 = x^8-8x^7+28x^6-56x^5+70x^4-56x^3+28x^2-8x+1
(x-1)^9 = x^9-9x^8+36x^7-84x^6+126x^5-126x^4+84x^3-36x^2+9x-1

primes (<=53): 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53

```



## PicoLisp

<lang>(de pascal (N)
   (let D 1
      (make
         (for X (inc N)
            (link D)
            (setq D
               (*/ D (- (inc N) X) (- X)) ) ) ) ) )

(for (X 0 (> 10 X) (inc X))
   (println X '-> (pascal X) ) )

(println
   (filter
      '((X)
         (fully
            '((Y) (=0 (% Y X)))
            (cdr (head -1 (pascal X))) ) )
      (range 2 50) ) )

(bye)
```


{{out}}

```txt

0 -> (1)
1 -> (1 -1)
2 -> (1 -2 1)
3 -> (1 -3 3 -1)
4 -> (1 -4 6 -4 1)
5 -> (1 -5 10 -10 5 -1)
6 -> (1 -6 15 -20 15 -6 1)
7 -> (1 -7 21 -35 35 -21 7 -1)
8 -> (1 -8 28 -56 70 -56 28 -8 1)
9 -> (1 -9 36 -84 126 -126 84 -36 9 -1)
(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)

```



## PL/I


```PL/I

AKS: procedure options (main, reorder); /* 16 September 2015, derived from Fortran */

  /* Coefficients of polynomial expansion */
  declare coeffs(*) fixed (31) controlled;
  declare n fixed(3);


  /* Point #2 */
  do n = 0 to 7;
    call polynomial_expansion(n, coeffs);
    put edit ( '(x - 1)^', trim(n), ' =' ) (a);
    call print_polynomial (coeffs);
  end;

  /* Point #4 */
   put skip;
  do n = 2 to 35;
    if is_prime(n) then put edit ( trim (n) ) (x(1), a);
  end;

  /* Point #5 */
  put skip;
  do n = 2 to 97;
    if is_prime(n) then put edit ( trim (n) ) (x(1), a);
  end;
  put skip;



  /* Calculate coefficients of (x - 1)^n using binomial theorem */
polynomial_expansion: procedure (n, coeffs);
    declare n fixed binary;
    declare coeffs (*) fixed (31) controlled;
    declare i fixed binary;

    if allocation(coeffs) > 0 then free coeffs;
    allocate coeffs (n+1);

    do i = 1 to n + 1;
      coeffs(i) = binomial(n, i - 1);
      if iand(n - i - 1, 1) = 1 then coeffs(i) = -coeffs(i);
    end;
  end polynomial_expansion;

  /* Calculate binomial coefficient using recurrent relation, as calculation	*/
  /* using factorial overflows too quickly.					*/
binomial: procedure (n, k) returns (fixed(31));
    declare (n, k) fixed;
    declare i fixed;
    declare result fixed (31) initial (n);

    if k = 0 then return (1);

    do i = 1 to k - 1;
      result = (result*(n - i))/(i + 1);
    end;
    return (result);
  end binomial;

  /* Outputs polynomial with given coefficients */
print_polynomial: procedure (coeffs);
    declare coeffs (*) fixed (31) controlled;
    declare ( i, p ) fixed binary;
    declare non_zero bit (1) aligned;
    declare (true initial ('1'b), false initial ('0'b)) bit (1);

    if allocation(coeffs) = 0 then return;

    non_zero = false;

    do i = 1 to hbound(coeffs);
      if coeffs(i) = 0 then iterate;

      p = i - 1;

      if non_zero then
         do;
            if coeffs(i) > 0 then
               put edit ( ' + ' ) (a);
            else
               put edit ( ' - ' ) (a);
         end;
      else
        do;
           if coeffs(i) > 0 then
              put edit ( '   ' ) (a);
           else
              put edit ( ' - ' ) (a);
        end;

      if p = 0 then
        put edit ( trim(abs(coeffs(i))) ) (a);
      else if p = 1 then
         do;
            if coeffs(i) = 1 then
               put edit ( 'x' ) (a);
            else
               put edit ( trim(abs(coeffs(i))), 'x' ) (a);
        end;
      else
        do;
            if coeffs(i) = 1 then
               put edit ( 'x^', trim(p) ) (a);
            else
               put edit (  trim(abs(coeffs(i)) ), 'x^', trim(p)) (a);
        end;

      non_zero = true;
    end;

    put skip;
  end print_polynomial;

  /* Test if n is prime using AKS test. Point #3. */
is_prime: procedure (n) returns (bit (1));
    declare n fixed (15);
    declare result bit (1) aligned;
    declare coeffs (*) fixed (31) controlled;
    declare i fixed binary;

    call polynomial_expansion(n, coeffs);
    coeffs(1) = coeffs(1) + 1;
    coeffs(n + 1) = coeffs(n + 1) - 1;

    result = '1'b;

    do i = 1 to n + 1;
      result = result & (mod(coeffs(i), n) = 0);
    end;

    if allocation(coeffs) > 0 then free coeffs;
    return (result);
  end is_prime;

end AKS;

```

Results obtained:

```txt

(x - 1)^0 =   1
(x - 1)^1 = - 1 + x
(x - 1)^2 =   1 - 2x + x^2
(x - 1)^3 = - 1 + 3x - 3x^2 + x^3
(x - 1)^4 =   1 - 4x + 6x^2 - 4x^3 + x^4
(x - 1)^5 = - 1 + 5x - 10x^2 + 10x^3 - 5x^4 + x^5
(x - 1)^6 =   1 - 6x + 15x^2 - 20x^3 + 15x^4 - 6x^5 + x^6
(x - 1)^7 = - 1 + 7x - 21x^2 + 35x^3 - 35x^4 + 21x^5 - 7x^6 + x^7

 2 3 5 7 11 13 17 19 23 29 31
 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```



## Prolog

===Prolog(ue)===
The theorem as stated ties together two elementary concepts in mathematics:
prime numbers and the Pascal triangle.  The simplicity of the
connection can be expressed directly in Prolog by the following prime number
generator:

```prolog

  prime(P) :-
    pascal([1,P|Xs]),
    append(Xs, [1], Rest),
    forall( member(X,Xs), 0 is X mod P).

```

where pascal/1 is a generator of rows of the Pascal triangle, for
example as defined below; the other predicates used above are
standard.

This solution to the Rosetta Code problems will accordingly focus on
the Pascal triangle, but to illustrate a number of points, we shall
exploit its symmetry by representing each of its rows by the longest
initial non-decreasing segment of that row, as illustrated in the
third column of the following table:

```txt

Row   Pascal Row   optpascal
1       1           [1]
2      1 1          [1, 1]
3     1 2 1         [1, 2]
4    1 3 3 1        [1, 3, 3]

```


We shall refer to this condensed representation of a row as an
"optpascal list".  Using it, we can simplify and improve
the above prime number generator by defining it as follows:

  prime(N) :- optpascal([1,N|Xs]), forall( member(X,Xs), 0 is X mod N).

Using SWI-Prolog without modifying any of the memory management parameters, this
prime number generator was used to generate all primes up to and
including 75,659.

Since Pascal triangles are the foundation of our approach to
addressing the specific Rosetta Code problems, we begin by defining
the generator pascal/2 that is required by the first problem, but we
do so by defining it in terms of an efficient generator, optpascal/1.


### Pascal Triangle Generator


```prolog

% To generate the n-th row of a Pascal triangle
% pascal(+N, Row)
pascal(0, [1]).
pascal(N, Row) :-
  N > 0, optpascal( [1, N|Xs] ),
  !,
  pascalize( [1, N|Xs], Row ).

pascalize( Opt, Row ) :-
  % if Opt ends in a pair, then peel off the pair:
  ( append(X, [R,R], Opt) -> true ; append(X, [R], Opt) ),
  reverse(X, Rs),
  append( Opt, Rs, Row ).

% optpascal(-X) generates optpascal lines:
optpascal(X) :-
  optpascal_successor( [], X).

% optpascal_successor(+P, -Q) is true if Q is an optpascal list beneath the optpascal list P:
optpascal_successor(P, Q) :-
  optpascal(P, NextP),
  (Q = NextP ; optpascal_successor(NextP, Q)).

% optpascal(+Row, NextRow) is true if Row and NextRow are adjacent rows in the Pascal triangle.
% optpascal(+Row, NextRow) where the optpascal representation is used
optpascal(X, [1|Y]) :-
  add_pairs(X, Y).

% add_pairs(+OptPascal, NextOptPascal) is a helper function for optpascal/2.
% Given one OptPascal list, it generates the next by adding adjacent
% items, but if the last two items are unequal, then their sum is
% repeated.  This is intended to be a deterministic predicate, and to
% avoid a probable compiler limitation, we therefore use one cut.
add_pairs([], []).
add_pairs([X], [X]).
add_pairs([X,Y], Ans) :-
  S is X + Y,
  (X = Y -> Ans=[S] ; Ans=[S,S]),
  !.  % To overcome potential limitation of compiler

add_pairs( [X1, X2, X3|Xs], [S|Ys]) :-
  S is X1 + X2,
  add_pairs( [X2, X3|Xs], Ys).

```


### Solutions


Solutions with output from SWI-Prolog:


```prolog

%%% Task 1: "A method to generate the coefficients of (1-X)^p"

coefficients(N, Coefficients) :-
  pascal(N, X),
  alternate_signs(X, Coefficients).

alternate_signs( [], [] ).
alternate_signs( [A], [A] ).
alternate_signs( [A,B | X], [A, MB | Y] ) :-
  MB is -B,
  alternate_signs(X,Y).

%%% Task 2. "Show here the polynomial expansions of (x − 1)p for p in the range 0 to at least 7, inclusive."

coefficients(Coefficients) :-
  optpascal( Opt),
  pascalize( Opt, Row ),
  alternate_signs(Row, Coefficients).


%  As required by the problem statement, but necessarily very inefficient:
:- between(0, 7, N), coefficients(N, Coefficients), writeln(Coefficients), fail ; true.

```


```txt

[1]
[1,-1]
[1,-2,1]
[1,-3,3,-1]
[1,-4,6,-4,1]
[1,-5,10,-10,5,-1]
[1,-6,15,-20,15,-6,1]
[1,-7,21,-35,35,-21,7,-1]

```


The following would be more efficient because backtracking saves recomputation:

```txt

:- coefficients(Coefficients),
   writeln(Coefficients),
   Coefficients = [_,N|_], N = -7.

```



```prolog

%%% Task 3. Use the previous function in creating [sic]
%%% another function that when given p returns whether p is prime
%%% using the AKS test.

% Even for testing whether a given number, N, is prime,
% this approach is inefficient, but here is a Prolog implementation:

   prime_test_per_requirements(N) :-
     coefficients(N, [1|Coefficients]),
     append(Cs, [_], Coefficients),
     forall( member(C, Cs), 0 is C mod N).

```


The following is more efficient (because it relies on optpascal
lists rather than the full array of coefficients), and more
flexible (because it can be used to generate primes without requiring
recomputation):


```prolog

   prime(N) :- optpascal([1,N|Xs]), forall( member(X,Xs), 0 is X mod N).

```



```prolog

%%% Task 4. Use your AKS test to generate a list of all primes under 35.

:-   prime(N), (N < 35 -> write(N), write(' '), fail ; nl).

% Output: 1 2 3 5 7 11 13 17 19 23 29 31

%%% Task 5. As a stretch goal, generate all primes under 50.

:-  prime(N), (N < 50 -> write(N), write(' '), fail ; nl).

% Output: 1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47

```




## PureBasic


```purebasic
EnableExplicit
Define vzr.b = -1, vzc.b = ~vzr, nMAX.i = 10, n.i , k.i

Procedure coeff(nRow.i, Array pd.i(2))
  Define n.i, k.i
  For n=1 To nRow
    For k=0 To n
      If k=0 Or k=n : pd(n,k)=1 : Continue : EndIf
      pd(n,k)=pd(n-1,k-1)+pd(n-1,k)
    Next
  Next
EndProcedure

Procedure.b isPrime(n.i, Array pd.i(2))
  Define m.i
  For m=1 To n-1
    If Not pd(n,m) % n = 0 : ProcedureReturn #False : EndIf
  Next
  ProcedureReturn #True
EndProcedure

Dim pd.i(nMAX,nMAX)
pd(0,0)=1 : coeff(nMAX, pd())
OpenConsole()

For n=0 To nMAX
  Print(RSet(Str(n),3,Chr(32))+": ")
  If vzr : Print("+") : Else : Print("-") : EndIf
  For k=0 To n
    If k>0 : If vzc : Print("+") : Else : Print("-") : EndIf : vzc = ~vzc : EndIf
    Print(RSet(Str(pd(n,k)),3,Chr(32))+Space(3))
  Next
  PrintN("")
  vzr = ~vzr : vzc = ~vzr
Next
PrintN("")

nMAX=50 : Dim pd.i(nMAX,nMAX)
Print("Primes n<=50 : ") : coeff(nMAX, pd())
For n=2 To 50
  If isPrime(n,pd()) : Print(Str(n)+Space(2)) : EndIf
Next
Input()
```

{{out}}

```txt
  0: +  1
  1: -  1   +  1
  2: +  1   -  2   +  1
  3: -  1   +  3   -  3   +  1
  4: +  1   -  4   +  6   -  4   +  1
  5: -  1   +  5   - 10   + 10   -  5   +  1
  6: +  1   -  6   + 15   - 20   + 15   -  6   +  1
  7: -  1   +  7   - 21   + 35   - 35   + 21   -  7   +  1
  8: +  1   -  8   + 28   - 56   + 70   - 56   + 28   -  8   +  1
  9: -  1   +  9   - 36   + 84   -126   +126   - 84   + 36   -  9   +  1
 10: +  1   - 10   + 45   -120   +210   -252   +210   -120   + 45   - 10   +  1


Primes n<=50 : 2  3  5  7  11  13  17  19  23  29  31  37  41  43  47
```



## Python



```python
def expand_x_1(n):
# This version uses a generator and thus less computations
    c =1
    for i in range(n//2+1):
        c = c*(n-i)//(i+1)
        yield c

def aks(p):
    if p==2:
        return True

    for i in expand_x_1(p):
        if i % p:
# we stop without computing all possible solutions
            return False
    return True
```

or equivalently:

```python
def aks(p):
    if p==2:return True
    c=1
    for i in range(p//2+1):
        c=c*(p-i)//(i+1)
        if c%p:return False
    return True
```

alternatively:

```python
def expand_x_1(p):
    ex = [1]
    for i in range(p):
        ex.append(ex[-1] * -(p-i) / (i+1))
    return ex[::-1]

def aks_test(p):
    if p < 2: return False
    ex = expand_x_1(p)
    ex[0] += 1
    return not any(mult % p for mult in ex[0:-1])


print('# p: (x-1)^p for small p')
for p in range(12):
    print('%3i: %s' % (p, ' '.join('%+i%s' % (e, ('x^%i' % n) if n else '')
                                   for n,e in enumerate(expand_x_1(p)))))

print('\n# small primes using the aks test')
print([p for p in range(101) if aks_test(p)])
```


{{out}}

```txt
# p: (x-1)^p for small p
  0: +1
  1: -1 +1x^1
  2: +1 -2x^1 +1x^2
  3: -1 +3x^1 -3x^2 +1x^3
  4: +1 -4x^1 +6x^2 -4x^3 +1x^4
  5: -1 +5x^1 -10x^2 +10x^3 -5x^4 +1x^5
  6: +1 -6x^1 +15x^2 -20x^3 +15x^4 -6x^5 +1x^6
  7: -1 +7x^1 -21x^2 +35x^3 -35x^4 +21x^5 -7x^6 +1x^7
  8: +1 -8x^1 +28x^2 -56x^3 +70x^4 -56x^5 +28x^6 -8x^7 +1x^8
  9: -1 +9x^1 -36x^2 +84x^3 -126x^4 +126x^5 -84x^6 +36x^7 -9x^8 +1x^9
 10: +1 -10x^1 +45x^2 -120x^3 +210x^4 -252x^5 +210x^6 -120x^7 +45x^8 -10x^9 +1x^10
 11: -1 +11x^1 -55x^2 +165x^3 -330x^4 +462x^5 -462x^6 +330x^7 -165x^8 +55x^9 -11x^10 +1x^11

# small primes using the aks test
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
```



### Python: Output formatted for wiki

Using a wikitable and math features with the following additional code produces better formatted polynomial output:


```python
print('''
{| class="wikitable" style="text-align:left;"
|+ Polynomial Expansions and AKS prime test
|-
! <math>p</math>
! <math>(x-1)^p</math>
|-''')
for p in range(12):
    print('! <math>%i</math>\n| <math>%s</math>\n| %r\n|-'
          % (p,
             ' '.join('%s%s' % (('%+i' % e) if (e != 1 or not p or (p and not n) ) else '+',
                                (('x^{%i}' % n) if n > 1 else 'x') if n else '')
                      for n,e in enumerate(expand_x_1(p))),
             aks_test(p)))
print('|}')
```


{{out}}

{| class="wikitable" style="text-align:left;"
|+ Polynomial Expansions and AKS prime test
|-
! <math>p</math>
! <math>(x-1)^p</math>
!Prime(p)?
|-
! <math>0</math>
| <math>+1</math>
| False
|-
! <math>1</math>
| <math>-1 +x</math>
| False
|-
! <math>2</math>
| <math>+1 -2x +x^{2}</math>
| True
|-
! <math>3</math>
| <math>-1 +3x -3x^{2} +x^{3}</math>
| True
|-
! <math>4</math>
| <math>+1 -4x +6x^{2} -4x^{3} +x^{4}</math>
| False
|-
! <math>5</math>
| <math>-1 +5x -10x^{2} +10x^{3} -5x^{4} +x^{5}</math>
| True
|-
! <math>6</math>
| <math>+1 -6x +15x^{2} -20x^{3} +15x^{4} -6x^{5} +x^{6}</math>
| False
|-
! <math>7</math>
| <math>-1 +7x -21x^{2} +35x^{3} -35x^{4} +21x^{5} -7x^{6} +x^{7}</math>
| True
|-
! <math>8</math>
| <math>+1 -8x +28x^{2} -56x^{3} +70x^{4} -56x^{5} +28x^{6} -8x^{7} +x^{8}</math>
| False
|-
! <math>9</math>
| <math>-1 +9x -36x^{2} +84x^{3} -126x^{4} +126x^{5} -84x^{6} +36x^{7} -9x^{8} +x^{9}</math>
| False
|-
! <math>10</math>
| <math>+1 -10x +45x^{2} -120x^{3} +210x^{4} -252x^{5} +210x^{6} -120x^{7} +45x^{8} -10x^{9} +x^{10}</math>
| False
|-
! <math>11</math>
| <math>-1 +11x -55x^{2} +165x^{3} -330x^{4} +462x^{5} -462x^{6} +330x^{7} -165x^{8} +55x^{9} -11x^{10} +x^{11}</math>
| True
|-
|}


## R


Working on the coefficients of the following expression (x-1)^p - (x^p-1). Excluding the coefficient of X^(p-1).



```R
AKS<-function(p){
  i<-2:p-1
  l<-unique(factorial(p) / (factorial(p-i) * factorial(i)))
  if(all(l%%p==0)){
    print(noquote("It is prime."))
  }else{
   print(noquote("It isn't prime."))
  }
}
```



## Racket

With copious use of the math/number-theory library...


```racket
#lang racket
(require math/number-theory)

;; 1. coefficients of expanded polynomial (x-1)^p
;;    produces a vector because in-vector can provide a start
;;    and stop (of 1 and p) which allow us to drop the (-1)^p
;;    and the x^p terms, respectively.
;;
;;    (vector-ref (coefficients p) e) is the coefficient for p^e
(define (coefficients p)
  (for/vector ((e (in-range 0 (add1 p))))
    (define sign (expt -1 (- p e)))
    (* sign (binomial p e))))

;; 2. Show the polynomial expansions from p=0 .. 7 (inclusive)
;; (it's possible some of these can be merged...)
(define (format-coefficient c e leftmost?)
  (define (format-c.x^e c e)
    (define +c (abs c))
    (match* (+c e)
      [(_ 0) (format "~a" +c)]
      [(1 _) (format "x^~a" e)]
      [(_ _) (format "~ax^~a" +c e)]))
  (define +/- (if (negative? c) "-" "+"))
  (define +c.x^e (format-c.x^e c e))
  (match* (c e leftmost?)
    [(0 _ _) ""]
    [((? negative?) _ #t) (format "-~a" +c.x^e)]
    [(_ _ #t) +c.x^e]
    [(_ _ _) (format " ~a ~a" +/- +c.x^e)]))

(define (format-polynomial cs)
  (define cs-length (sequence-length cs))
  (apply
   string-append
   (reverse ; convention is to display highest exponent first
    (for/list ((c cs) (e (in-naturals)))
      (format-coefficient c e (= e (sub1 cs-length)))))))

(for ((p (in-range 0 (add1 11))))
  (printf "p=~a: ~a~%" p (format-polynomial (coefficients p))))

;; 3. AKS primeality test
(define (prime?/AKS p)
  (define cs (coefficients p))
  (and
   (or (= (vector-ref cs 0) -1) ; c_0 = -1 -> c_0 - (-1) = 0
       (divides? p 2))        ; c_0 = 1 -> c_0 - (-1) = 2 -> divides?
   (for/and ((c (in-vector cs 1 p))) (divides? p c))))

;; there is some discussion (see Discussion) about what to do with the perennial "1"
;; case. This is my way of saying that I'm ignoring it
(define lowest-tested-number 2)

;; 4. list of numbers < 35 that are prime (note that 1 is prime
;;    by the definition of the AKS test for primes):
(displayln (for/list ((i (in-range lowest-tested-number 35)) #:when (prime?/AKS i)) i))

;; 5. stretch goal: all prime numbers under 50
(displayln (for/list ((i (in-range lowest-tested-number 50)) #:when (prime?/AKS i)) i))
(displayln (for/list ((i (in-range lowest-tested-number 100)) #:when (prime?/AKS i)) i))

```


{{out}}

```txt
p=0: 1
p=1: x^1 - 1
p=2: x^2 - 2x^1 + 1
p=3: x^3 - 3x^2 + 3x^1 - 1
p=4: x^4 - 4x^3 + 6x^2 - 4x^1 + 1
p=5: x^5 - 5x^4 + 10x^3 - 10x^2 + 5x^1 - 1
p=6: x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x^1 + 1
p=7: x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x^1 - 1
p=8: x^8 - 8x^7 + 28x^6 - 56x^5 + 70x^4 - 56x^3 + 28x^2 - 8x^1 + 1
p=9: x^9 - 9x^8 + 36x^7 - 84x^6 + 126x^5 - 126x^4 + 84x^3 - 36x^2 + 9x^1 - 1
p=10: x^10 - 10x^9 + 45x^8 - 120x^7 + 210x^6 - 252x^5 + 210x^4 - 120x^3 + 45x^2 - 10x^1 + 1
p=11: x^11 - 11x^10 + 55x^9 - 165x^8 + 330x^7 - 462x^6 + 462x^5 - 330x^4 + 165x^3 - 55x^2 + 11x^1 - 1
(2 3 5 7 11 13 17 19 23 29 31)
(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```



## REXX


### version 1


```rexx
/* REXX ---------------------------------------------------------------
* 09.02.2014 Walter Pachl
* 22.02.2014 WP fix 'accounting' problem (courtesy GS)
*--------------------------------------------------------------------*/
c.=1
Numeric Digits 100
limit=200
pl=''
mmm=0
Do p=3 To limit
  pm1=p-1
  c.p.1=1
  c.p.p=1
  Do j=2 To p-1
    jm1=j-1
    c.p.j=c.pm1.jm1+c.pm1.j
    mmm=max(mmm,c.p.j)
    End
  End
Say '(x-1)**0 = 1'
do i=2 To limit
  im1=i-1
  sign='+'
  ol='(x-1)^'im1 '='
  Do j=i to 2 by -1
    If j=2 Then
      term='x  '
    Else
      term='x^'||(j-1)
    If j=i Then
      ol=ol term
    Else
      ol=ol sign c.i.j'*'term
    sign=translate(sign,'+-','-+')
    End
  If i<10 then
    Say ol sign 1
  Do j=2 To i-1
    If c.i.j//(i-1)>0 Then
      Leave
    End
  If j>i-1 Then
    pl=pl (i-1)
  End
Say ' '
Say 'Primes:' subword(pl,2,27)
Say '       ' subword(pl,29)
Say 'Largest coefficient:' mmm
Say 'This has' length(mmm) 'digits'
```

{{out}}

```txt
(x-1)**0 = 1
(x-1)^1 = x   - 1
(x-1)^2 = x^2 - 2*x   + 1
(x-1)^3 = x^3 - 3*x^2 + 3*x   - 1
(x-1)^4 = x^4 - 4*x^3 + 6*x^2 - 4*x   + 1
(x-1)^5 = x^5 - 5*x^4 + 10*x^3 - 10*x^2 + 5*x   - 1
(x-1)^6 = x^6 - 6*x^5 + 15*x^4 - 20*x^3 + 15*x^2 - 6*x   + 1
(x-1)^7 = x^7 - 7*x^6 + 21*x^5 - 35*x^4 + 35*x^3 - 21*x^2 + 7*x   - 1
(x-1)^8 = x^8 - 8*x^7 + 28*x^6 - 56*x^5 + 70*x^4 - 56*x^3 + 28*x^2 - 8*x   + 1

Primes: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103
        107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199
Largest coefficient: 45274257328051640582702088538742081937252294837706668420660
This has 59 digits
```



### version 2

This REXX version is an optimized version (of version 1)   and modified to address all of the task requirements.

The program determines programmatically the required number of decimal digits (precision) for the large coefficients.

```rexx
/*REXX program calculates  primes  via the  Agrawal─Kayal─Saxena  (AKS)  primality test.*/
parse arg Z .                                    /*obtain optional argument from the CL.*/
if Z=='' | Z=="," then Z= 200                    /*Not specified?  Then use the default.*/
OZ=Z;               tell= Z<0;       Z= abs(Z)   /*Is Z negative?  Then show expression.*/
numeric digits  max(9,  Z % 3)                   /*define a dynamic # of decimal digits.*/
call AKS                                         /*invoke the AKS funtion for coef. bld.*/
if left(OZ,1)=='+' then do; say Z isAksp(); exit /*display if  Z  is  or isn't  a prime.*/
                        end                      /* [↑]  call isAKSp if Z has leading +.*/
say;    say "primes found:"   #                  /*display the  prime number  list.     */
say;    if \datatype(#, 'W')  then exit          /* [↓]  the digit length of a big coef.*/
say 'Found '   words(#)   " primes and the largest coefficient has "   length(@.pm.h)  @dd
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
AKS: $.0= '-';      $.1= "+";           @. = 1   /*$.x: sign char; default coefficients.*/
     q.= 1;         q.1= 0;             q.4= 0   /*sparse array for faster comparisons. */
     #=;            L= length(Z)                 /*define list of prime numbers (so far)*/
        do p=3  for Z;      pm=p - 1;   pp=p + 1 /*PM & PP: used as a coding convenience*/
            do m=2  for pp % 2   - 1;   mm=m - 1 /*calculate coefficients for a power.  */
            @.p.m= @.pm.mm + @.pm.m;    h=pp - m /*calculate left  side of  coefficients*/
            @.p.h= @.p.m                         /*    "     right   "   "       "      */
            end   /*m*/                          /* [↑]  The  M   DO  loop creates both */
        end       /*p*/                          /*      sides in the same loop.        */
     if tell  then say '(x-1)^'right(0, L)":  1" /*possibly display the first expression*/
     @dd= 'decimal digits.'                      /* [↓]  test for primality by division.*/
        do n=2  for Z;     nh=n % 2;    d= n - 1 /*create expressions;  find the primes.*/
            do k=3  to nh  while @.n.k//d == 0   /*are coefficients divisible by  N-1 ? */
            end   /*k*/                          /* [↑]  skip the 1st & 2nd coefficients*/
        if k>nh   then if q.d  then #= # d       /*add a number to the prime list.      */
        if \tell  then iterate                   /*Don't tell?   Don't show expressions.*/
        y= '(x-1)^'right(d, L)":"                /*define the 1st part of the expression*/
        s=1                                      /*S:     is the sign indicator (-1│+1).*/
            do j=n  for n-1  by -1               /*create the higher powers first.      */
            if j==2  then xp= 'x'                /*if power=1, then don't show the power*/
                     else xp= 'x^' ||   j-1      /*        ··· else show power with  ^  */
            if j==n  then y=y  xp                /*no sign (+│-) for the 1st expression.*/
                     else y=y  $.s || @.n.j'∙'xp /*build the expression with sign (+|-).*/
            s= \s                                /*flip the sign for the next expression*/
            end   /*j*/                          /* [↑]  the sign (now) is either 0 │ 1,*/
        say  y  $.s'1'                           /*just show the first  N  expressions, */
        end       /*n*/                          /* [↑]  ··· but only for  negative  Z. */
     if #==''  then #= "none";        return #   /*if null, return "none"; else return #*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
isAKSp: if z==word(#,words(#))  then return ' is a prime.';  else return " isn't a prime."
```

{{out|output|text=   for task requirement #2, showing thirty-one expressions using as input:   <tt> -31 </tt>}}

(Shown at five-sixth size.)
<pre style="font-size:84%">
(x-1)^ 0:  1
(x-1)^ 1:  x -1
(x-1)^ 2:  x^2 -2∙x +1
(x-1)^ 3:  x^3 -3∙x^2 +3∙x -1
(x-1)^ 4:  x^4 -4∙x^3 +6∙x^2 -4∙x +1
(x-1)^ 5:  x^5 -5∙x^4 +10∙x^3 -10∙x^2 +5∙x -1
(x-1)^ 6:  x^6 -6∙x^5 +15∙x^4 -20∙x^3 +15∙x^2 -6∙x +1
(x-1)^ 7:  x^7 -7∙x^6 +21∙x^5 -35∙x^4 +35∙x^3 -21∙x^2 +7∙x -1
(x-1)^ 8:  x^8 -8∙x^7 +28∙x^6 -56∙x^5 +70∙x^4 -56∙x^3 +28∙x^2 -8∙x +1
(x-1)^ 9:  x^9 -9∙x^8 +36∙x^7 -84∙x^6 +126∙x^5 -126∙x^4 +84∙x^3 -36∙x^2 +9∙x -1
(x-1)^10:  x^10 -10∙x^9 +45∙x^8 -120∙x^7 +210∙x^6 -252∙x^5 +210∙x^4 -120∙x^3 +45∙x^2 -10∙x +1
(x-1)^11:  x^11 -11∙x^10 +55∙x^9 -165∙x^8 +330∙x^7 -462∙x^6 +462∙x^5 -330∙x^4 +165∙x^3 -55∙x^2 +11∙x -1
(x-1)^12:  x^12 -12∙x^11 +66∙x^10 -220∙x^9 +495∙x^8 -792∙x^7 +924∙x^6 -792∙x^5 +495∙x^4 -220∙x^3 +66∙x^2 -12∙x +1
(x-1)^13:  x^13 -13∙x^12 +78∙x^11 -286∙x^10 +715∙x^9 -1287∙x^8 +1716∙x^7 -1716∙x^6 +1287∙x^5 -715∙x^4 +286∙x^3 -78∙x^2 +13∙x -1
(x-1)^14:  x^14 -14∙x^13 +91∙x^12 -364∙x^11 +1001∙x^10 -2002∙x^9 +3003∙x^8 -3432∙x^7 +3003∙x^6 -2002∙x^5 +1001∙x^4 -364∙x^3 +91∙x^2 -14∙x +1
(x-1)^15:  x^15 -15∙x^14 +105∙x^13 -455∙x^12 +1365∙x^11 -3003∙x^10 +5005∙x^9 -6435∙x^8 +6435∙x^7 -5005∙x^6 +3003∙x^5 -1365∙x^4 +455∙x^3 -105∙x^2 +15∙x -1
(x-1)^16:  x^16 -16∙x^15 +120∙x^14 -560∙x^13 +1820∙x^12 -4368∙x^11 +8008∙x^10 -11440∙x^9 +12870∙x^8 -11440∙x^7 +8008∙x^6 -4368∙x^5 +1820∙x^4 -560∙x^3 +120∙x^2 -16∙x +1
(x-1)^17:  x^17 -17∙x^16 +136∙x^15 -680∙x^14 +2380∙x^13 -6188∙x^12 +12376∙x^11 -19448∙x^10 +24310∙x^9 -24310∙x^8 +19448∙x^7 -12376∙x^6 +6188∙x^5 -2380∙x^4 +680∙x^3 -136∙x^2 +17∙x -1
(x-1)^18:  x^18 -18∙x^17 +153∙x^16 -816∙x^15 +3060∙x^14 -8568∙x^13 +18564∙x^12 -31824∙x^11 +43758∙x^10 -48620∙x^9 +43758∙x^8 -31824∙x^7 +18564∙x^6 -8568∙x^5 +3060∙x^4 -816∙x^3 +153∙x^2 -18∙x +1
(x-1)^19:  x^19 -19∙x^18 +171∙x^17 -969∙x^16 +3876∙x^15 -11628∙x^14 +27132∙x^13 -50388∙x^12 +75582∙x^11 -92378∙x^10 +92378∙x^9 -75582∙x^8 +50388∙x^7 -27132∙x^6 +11628∙x^5 -3876∙x^4 +969∙x^3 -171∙x^2 +19∙x -1
(x-1)^20:  x^20 -20∙x^19 +190∙x^18 -1140∙x^17 +4845∙x^16 -15504∙x^15 +38760∙x^14 -77520∙x^13 +125970∙x^12 -167960∙x^11 +184756∙x^10 -167960∙x^9 +125970∙x^8 -77520∙x^7 +38760∙x^6 -15504∙x^5 +4845∙x^4 -1140∙x^3 +190∙x^2 -20∙x +1
(x-1)^21:  x^21 -21∙x^20 +210∙x^19 -1330∙x^18 +5985∙x^17 -20349∙x^16 +54264∙x^15 -116280∙x^14 +203490∙x^13 -293930∙x^12 +352716∙x^11 -352716∙x^10 +293930∙x^9 -203490∙x^8 +116280∙x^7 -54264∙x^6 +20349∙x^5 -5985∙x^4 +1330∙x^3 -210∙x^2 +21∙x -1
(x-1)^22:  x^22 -22∙x^21 +231∙x^20 -1540∙x^19 +7315∙x^18 -26334∙x^17 +74613∙x^16 -170544∙x^15 +319770∙x^14 -497420∙x^13 +646646∙x^12 -705432∙x^11 +646646∙x^10 -497420∙x^9 +319770∙x^8 -170544∙x^7 +74613∙x^6 -26334∙x^5 +7315∙x^4 -1540∙x^3 +231∙x^2 -22∙x +1
(x-1)^23:  x^23 -23∙x^22 +253∙x^21 -1771∙x^20 +8855∙x^19 -33649∙x^18 +100947∙x^17 -245157∙x^16 +490314∙x^15 -817190∙x^14 +1144066∙x^13 -1352078∙x^12 +1352078∙x^11 -1144066∙x^10 +817190∙x^9 -490314∙x^8 +245157∙x^7 -100947∙x^6 +33649∙x^5 -8855∙x^4 +1771∙x^3 -253∙x^2 +23∙x -1
(x-1)^24:  x^24 -24∙x^23 +276∙x^22 -2024∙x^21 +10626∙x^20 -42504∙x^19 +134596∙x^18 -346104∙x^17 +735471∙x^16 -1307504∙x^15 +1961256∙x^14 -2496144∙x^13 +2704156∙x^12 -2496144∙x^11 +1961256∙x^10 -1307504∙x^9 +735471∙x^8 -346104∙x^7 +134596∙x^6 -42504∙x^5 +10626∙x^4 -2024∙x^3 +276∙x^2 -24∙x +1
(x-1)^25:  x^25 -25∙x^24 +300∙x^23 -2300∙x^22 +12650∙x^21 -53130∙x^20 +177100∙x^19 -480700∙x^18 +1081575∙x^17 -2042975∙x^16 +3268760∙x^15 -4457400∙x^14 +5200300∙x^13 -5200300∙x^12 +4457400∙x^11 -3268760∙x^10 +2042975∙x^9 -1081575∙x^8 +480700∙x^7 -177100∙x^6 +53130∙x^5 -12650∙x^4 +2300∙x^3 -300∙x^2 +25∙x -1
(x-1)^26:  x^26 -26∙x^25 +325∙x^24 -2600∙x^23 +14950∙x^22 -65780∙x^21 +230230∙x^20 -657800∙x^19 +1562275∙x^18 -3124550∙x^17 +5311735∙x^16 -7726160∙x^15 +9657700∙x^14 -10400600∙x^13 +9657700∙x^12 -7726160∙x^11 +5311735∙x^10 -3124550∙x^9 +1562275∙x^8 -657800∙x^7 +230230∙x^6 -65780∙x^5 +14950∙x^4 -2600∙x^3 +325∙x^2 -26∙x +1
(x-1)^27:  x^27 -27∙x^26 +351∙x^25 -2925∙x^24 +17550∙x^23 -80730∙x^22 +296010∙x^21 -888030∙x^20 +2220075∙x^19 -4686825∙x^18 +8436285∙x^17 -13037895∙x^16 +17383860∙x^15 -20058300∙x^14 +20058300∙x^13 -17383860∙x^12 +13037895∙x^11 -8436285∙x^10 +4686825∙x^9 -2220075∙x^8 +888030∙x^7 -296010∙x^6 +80730∙x^5 -17550∙x^4 +2925∙x^3 -351∙x^2 +27∙x -1
(x-1)^28:  x^28 -28∙x^27 +378∙x^26 -3276∙x^25 +20475∙x^24 -98280∙x^23 +376740∙x^22 -1184040∙x^21 +3108105∙x^20 -6906900∙x^19 +13123110∙x^18 -21474180∙x^17 +30421755∙x^16 -37442160∙x^15 +40116600∙x^14 -37442160∙x^13 +30421755∙x^12 -21474180∙x^11 +13123110∙x^10 -6906900∙x^9 +3108105∙x^8 -1184040∙x^7 +376740∙x^6 -98280∙x^5 +20475∙x^4 -3276∙x^3 +378∙x^2 -28∙x +1
(x-1)^29:  x^29 -29∙x^28 +406∙x^27 -3654∙x^26 +23751∙x^25 -118755∙x^24 +475020∙x^23 -1560780∙x^22 +4292145∙x^21 -10015005∙x^20 +20030010∙x^19 -34597290∙x^18 +51895935∙x^17 -67863915∙x^16 +77558760∙x^15 -77558760∙x^14 +67863915∙x^13 -51895935∙x^12 +34597290∙x^11 -20030010∙x^10 +10015005∙x^9 -4292145∙x^8 +1560780∙x^7 -475020∙x^6 +118755∙x^5 -23751∙x^4 +3654∙x^3 -406∙x^2 +29∙x -1
(x-1)^30:  x^30 -30∙x^29 +435∙x^28 -4060∙x^27 +27405∙x^26 -142506∙x^25 +593775∙x^24 -2035800∙x^23 +5852925∙x^22 -14307150∙x^21 +30045015∙x^20 -54627300∙x^19 +86493225∙x^18 -119759850∙x^17 +145422675∙x^16 -155117520∙x^15 +145422675∙x^14 -119759850∙x^13 +86493225∙x^12 -54627300∙x^11 +30045015∙x^10 -14307150∙x^9 +5852925∙x^8 -2035800∙x^7 +593775∙x^6 -142506∙x^5 +27405∙x^4 -4060∙x^3 +435∙x^2 -30∙x +1
(x-1)^31:  x^31 -31∙x^30 +465∙x^29 -4495∙x^28 +31465∙x^27 -169911∙x^26 +736281∙x^25 -2629575∙x^24 +7888725∙x^23 -20160075∙x^22 +44352165∙x^21 -84672315∙x^20 +141120525∙x^19 -206253075∙x^18 +265182525∙x^17 -300540195∙x^16 +300540195∙x^15 -265182525∙x^14 +206253075∙x^13 -141120525∙x^12 +84672315∙x^11 -44352165∙x^10 +20160075∙x^9 -7888725∙x^8 +2629575∙x^7 -736281∙x^6 +169911∙x^5 -31465∙x^4 +4495∙x^3 -465∙x^2 +31∙x -1

primes:  2 3 5 7 11 13 17 19 23 29 31

Found  11  primes and the largest coefficient has  9  decimal digits.

```

{{out|output|text=   for task requirement #3, showing if   '''2221'''   is prime (or not) using for input:   <tt> +2221 </tt>}}


(Output note:   this number is really pushing at the limits of REXX's use of virtual memory to store the

coefficients;   the version of Regina REXX used herein has a limit of around 2 Gbytes.)

```txt

2221  is prime.

```

{{out|output|text=   for task requirement #4, showing all primes under   '''35'''   using the input:   <tt> 35 </tt>}}

```txt

primes:  2 3 5 7 11 13 17 19 23 29 31

Found  11  primes and the largest coefficient has  10  decimal digits.

```

{{out|output|text=   for requirement #5 (stretch goal), showing all primes under   '''50'''   using the input:   <tt> 50 </tt>}}

```txt

primes:  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47

Found  15  primes and the largest coefficient has  15  decimal digits.

```

{{out|output|text=   when using the input:   <tt> 500 </tt>}}

(Shown at five-sixth size.)
<pre style="font-size:84%">
primes:  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499

Found  95  primes and the largest coefficient has  150  decimal digits.

```



## Ruby

Using the `polynomial` Rubygem, this can be written directly from the definition in the description:


```ruby
require 'polynomial'

def x_minus_1_to_the(p)
  return Polynomial.new(-1,1)**p
end

def prime?(p)
  return false if p < 2
  (x_minus_1_to_the(p) - Polynomial.from_string("x**#{p}-1")).coefs.all?{|n| n%p==0}
end

8.times do |n|
  # the default Polynomial#to_s would be OK here; the substitutions just make the
  # output match the other version below.
  puts "(x-1)^#{n} = #{x_minus_1_to_the(n).to_s.gsub(/\*\*/,'^').gsub(/\*/,'')}"
end

puts "\nPrimes below 50:", 50.times.select {|n| prime? n}.join(',')
```


Or without the dependency:


```ruby
def x_minus_1_to_the(p)
  p.times.inject([1]) do |ex, _|
    ([0] + ex).zip(ex + [0]).map { |x,y| x - y }
  end
end

def prime?(p)
  return false if p < 2
  coeff = x_minus_1_to_the(p)[1..p/2] # only need half of coeff terms
  coeff.all?{ |n| n%p == 0 }
end

8.times do |n|
  puts "(x-1)^#{n} = " +
  x_minus_1_to_the(n).map.with_index { |c, p|
    p.zero? ? c.to_s :
      (c < 0 ? " - " : " + ") + (c.abs == 1 ? "x" : "#{c.abs}x") + (p == 1 ? "" : "^#{p}")
  }.join
end

puts "\nPrimes below 50:", 50.times.select {|n| prime? n}.join(',')
```


{{out}}

```txt

(x-1)^0 = 1
(x-1)^1 = -1 + x
(x-1)^2 = 1 - 2x + x^2
(x-1)^3 = -1 + 3x - 3x^2 + x^3
(x-1)^4 = 1 - 4x + 6x^2 - 4x^3 + x^4
(x-1)^5 = -1 + 5x - 10x^2 + 10x^3 - 5x^4 + x^5
(x-1)^6 = 1 - 6x + 15x^2 - 20x^3 + 15x^4 - 6x^5 + x^6
(x-1)^7 = -1 + 7x - 21x^2 + 35x^3 - 35x^4 + 21x^5 - 7x^6 + x^7

Primes below 50:
2,3,5,7,11,13,17,19,23,29,31,37,41,43,47
```



## Rust


```rust
use std::iter::repeat;

fn aks_coefficients(k: usize) -> Vec<i64> {
    let mut coefficients = repeat(0i64).take(k + 1).collect::<Vec<_>>();
    coefficients[0] = 1;
    for i in 1..(k + 1) {
        coefficients[i] = -(1..i).fold(coefficients[0], |prev, j|{
            let old = coefficients[j];
            coefficients[j] = old - prev;
            old
        });
    }
    coefficients
}

fn is_prime(p: usize) -> bool {
    if p < 2 {
        false
    } else {
        let c = aks_coefficients(p);
        (1 .. (c.len() - 1) / 2 + 1).all(|i| (c[i] % (p as i64)) == 0)
    }
}

fn main() {
    for i in 0..8 {
        println!("{}: {:?}", i, aks_coefficients(i));
    }
    for i in (1..51).filter(|&i| is_prime(i)) {
        print!("{} ", i);
    }
}
```

{{out}}

```txt
0: [1]
1: [1, -1]
2: [1, -2, 1]
3: [1, -3, 3, -1]
4: [1, -4, 6, -4, 1]
5: [1, -5, 10, -10, 5, -1]
6: [1, -6, 15, -20, 15, -6, 1]
7: [1, -7, 21, -35, 35, -21, 7, -1]
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
```


An alternative version which computes the coefficients in a more functional but less efficient way.


```rust

fn aks_coefficients(k: usize) -> Vec<i64> {
	if k == 0 {
		vec![1i64]
	} else {
		let zero = Some(0i64);
		range(1, k).fold(vec![1i64, -1], |r, _| {
			let a = r.iter().chain(zero.iter());
			let b = zero.iter().chain(r.iter());
			a.zip(b).map(|(x, &y)| x-y).collect()
		})
	}
}

```



## Scala


```Scala
def powerMin1(n: BigInt) = if (n % 2 == 0) BigInt(1) else BigInt(-1)

val pascal = (( Vector(Vector(BigInt(1))) /: (1 to 50)) { (rows, i) =>
    val v = rows.head
    val newVector = ((1 until v.length) map (j =>
        powerMin1(j+i) * (v(j-1).abs + v(j).abs))
    ).toVector
    (powerMin1(i) +: newVector :+ powerMin1(i+v.length)) +: rows
}).reverse

def poly2String(poly: Vector[BigInt]) = ((0 until poly.length) map { i =>
    (i, poly(i)) match {
        case (0, c) => c.toString
        case (_, c) =>
            (if (c >= 0) "+" else "-") +
            (if (c == 1) "x" else c.abs + "x") +
            (if (i == 1) "" else "^" + i)
    }
}) mkString ""

def isPrime(n: Int) = {
    val poly = pascal(n)
    poly.slice(1, poly.length - 1).forall(i => i % n == 0)
}

for(i <- 0 to 7) { println( f"(x-1)^$i = ${poly2String( pascal(i) )}" ) }

val primes = (2 to 50).filter(isPrime)
println
println(primes mkString " ")
```


{{out}}

```txt
(x-1)^0 = 1
(x-1)^1 = -1+x
(x-1)^2 = 1-2x+x^2
(x-1)^3 = -1+3x-3x^2+x^3
(x-1)^4 = 1-4x+6x^2-4x^3+x^4
(x-1)^5 = -1+5x-10x^2+10x^3-5x^4+x^5
(x-1)^6 = 1-6x+15x^2-20x^3+15x^4-6x^5+x^6
(x-1)^7 = -1+7x-21x^2+35x^3-35x^4+21x^5-7x^6+x^7

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
```



## Scheme



```Scheme

;; implement mod m arithmetic with polnomials in x
;; as lists of coefficients, x^0 first.
;;
;; so x^3 + 5 is represented as (5 0 0 1)

(define (+/m m a b)
  ;; add two polynomials
  (cond ((null? a) b)
        ((null? b) a)
        (else (cons (modulo (+ (car a) (car b)) m)
                    (+/m m (cdr a) (cdr b))))))

(define (*c/m m c a)
  ;; multiplication by a constant
  (map (lambda (v) (modulo (* c v) m)) a))

(define (*/m m a b)
  ;; multiply two polynomials
  (let loop ((a a))
    (if (null? a)
        '()
        (+/m m (*c/m m (car a) b)
             (cons 0 (*/m m (cdr a) b))))))

(define (x^n/m m n)
  (if (= n 0)
      '(1)
      (cons 0 (x^n/m m (- n 1)))))

(define (^n/m m a n)
  ;; calculate the n'th power of polynomial a
  (cond ((= n 0) '(1))
        ((= n 1) a)
        (else (*/m m a (^n/m m a (- n 1))))))

;; test case
;;
;; ? lift(Mod((x^3 + 5)*(4 + 3*x + x^2),6))
;; %13 = x^5 + 3*x^4 + 4*x^3 + 5*x^2 + 3*x + 2
;;
;; > (*/m 6 '(5 0 0 1) '(4 3 1))
;; '(2 3 5 4 3 1)
;;
;; working correctly

(define (rosetta-aks-test p)
  (if (or (= p 0) (= p 1))
      #f
      ;; u = (x - 1)^p
      ;; v = (x^p - 1)
      (let ((u (^n/m p (list -1 1) p))
            (v (+/m p (x^n/m p p) (list -1))))
        (every zero? (+/m p u (*c/m p -1 v))))))

;; > (filter rosetta-aks-test (iota 50))
;; '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)

```



## Scilab


<lang>
clear
xdel(winsid())

stacksize('max')
sz=stacksize();

n=7; //For the expansion up to power of n
g=50; //For test of primes up to g

function X = pascal(g) //Pascal´s triangle
    X(1,1)=1; //Zeroth power
    X(2,1)=1; //First power
    X(2,2)=1;
    for q=3:1:g+1 //From second power use this loop
       X(q,1)=1;
       X(q,q)=1;
        for p=2:1:q-1
            X(q,p)=X(q-1,p-1)+X(q-1,p);
        end
    end
endfunction

Z=pascal(g); //Generate Pascal's triangle up to g

Q(0+1)="(x-1)^0 = 1"; //For nicer display
Q(1+1)="(x-1)^1 = x^1-1"; //For nicer display

disp(Q(1))
disp(Q(2))

function cf=coef(Z,q,p) //Return coeffiecents for nicer display of expansion without "ones"
    if Z(q,p)==1 then
        cf="";
    else
        cf=string(Z(q,p));
    end
endfunction

for q=3:n+1  //Generate and display the expansions
    Q(q)=strcat(["(x-1)^",string(q-1)," = "]);
    sing=""; //Sign of coeff.
        for p=1:q-1 //Number of coefficients equals power minus 1
            Q(q)=strcat([Q(q),sing,coef(Z,q,p),"x^",string(q-p)]);
            if sing=="-" then sing="+"; else sing="-"; end
        end
        Q(q)=strcat([Q(q),sing,string(1)]);
    disp(Q(q))
    clear Q
end

function prime=prime(Z,g)
    prime="true";
    for p=2:g
        if abs(floor(Z(g+1,p)/g)-Z(g+1,p)/g)>0 then
            prime="false";
            break;
        end
    end
endfunction

R="2"; //For nicer display
for r=3:g
    if prime(Z,r)=="true" then
        R=strcat([R, ", ",string(r)]);
    end
end
disp(R)

```


{{out}}

```txt

(x-1)^0 = 1

 (x-1)^1 = x^1-1

 (x-1)^2 = x^2-2x^1+1

 (x-1)^3 = x^3-3x^2+3x^1-1

 (x-1)^4 = x^4-4x^3+6x^2-4x^1+1

 (x-1)^5 = x^5-5x^4+10x^3-10x^2+5x^1-1

 (x-1)^6 = x^6-6x^5+15x^4-20x^3+15x^2-6x^1+1

 (x-1)^7 = x^7-7x^6+21x^5-35x^4+35x^3-21x^2+7x^1-1

 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func array integer: expand_x_1 (in integer: p) is func
  result
    var array integer: ex is [] (1);
  local
    var integer: i is 0;
  begin
    for i range 0 to p - 1 do
      ex := [] (ex[1] * -(p - i) div (i + 1)) & ex;
    end for;
  end func;

const func boolean: aks_test (in integer: p) is func
  result
    var boolean: aks_test is FALSE;
  local
    var array integer: ex is 0 times 0;
    var integer: idx is 0;
  begin
    if p >= 2 then
      ex := expand_x_1(p);
      ex[1] +:= 1;
      for idx range 1 to pred(length(ex)) until ex[idx] rem p <> 0 do
        noop;
      end for;
      aks_test := idx = length(ex);
    end if;
  end func;

const proc: main is func
  local
    var integer: p is 0;
    var integer: n is 0;
    var integer: e is 0;
  begin
    writeln("# p: (x-1)^p for small p");
    for p range 0 to 11 do
      write(p lpad 3 <& ": ");
      for n key e range expand_x_1(p) do
        write(" ");
        if n >= 0 then
          write("+");
        end if;
        write(n);
        if e > 1 then
          write("x^" <& pred(e));
        end if;
      end for;
      writeln;
    end for;
    writeln;
    writeln("# small primes using the aks test");
    for p range 0 to 61 do
      if aks_test(p) then
        write(p <& " ");
      end if;
    end for;
    writeln;
  end func;
```


{{out}}

```txt

# p: (x-1)^p for small p
  0:  +1
  1:  -1 +1x^1
  2:  +1 -2x^1 +1x^2
  3:  -1 +3x^1 -3x^2 +1x^3
  4:  +1 -4x^1 +6x^2 -4x^3 +1x^4
  5:  -1 +5x^1 -10x^2 +10x^3 -5x^4 +1x^5
  6:  +1 -6x^1 +15x^2 -20x^3 +15x^4 -6x^5 +1x^6
  7:  -1 +7x^1 -21x^2 +35x^3 -35x^4 +21x^5 -7x^6 +1x^7
  8:  +1 -8x^1 +28x^2 -56x^3 +70x^4 -56x^5 +28x^6 -8x^7 +1x^8
  9:  -1 +9x^1 -36x^2 +84x^3 -126x^4 +126x^5 -84x^6 +36x^7 -9x^8 +1x^9
 10:  +1 -10x^1 +45x^2 -120x^3 +210x^4 -252x^5 +210x^6 -120x^7 +45x^8 -10x^9 +1x^10
 11:  -1 +11x^1 -55x^2 +165x^3 -330x^4 +462x^5 -462x^6 +330x^7 -165x^8 +55x^9 -11x^10 +1x^11

# small primes using the aks test
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61

```



## Sidef

{{trans|Perl}}

```ruby
func binprime(p) {
    p >= 2 || return false
    for i in (1 .. p>>1) {
        (binomial(p, i) % p) && return false
    }
    return true
}

func coef(n, e) {
    (e == 0) && return "#{n}"
    (n == 1) && (n = "")
    (e == 1) ? "#{n}x" : "#{n}x^#{e}"
}

func binpoly(p) {
    join(" ", coef(1, p), ^p -> map {|i|
        join(" ", %w(+ -)[(p-i)&1], coef(binomial(p, i), i))
    }.reverse...)
}

say "expansions of (x-1)^p:"
for i in ^10 { say binpoly(i) }
say "Primes to 80: [#{2..80 -> grep { binprime(_) }.join(' ')}]"
```

{{out}}

```txt

expansions of (x-1)^p:
1
x - 1
x^2 - 2x + 1
x^3 - 3x^2 + 3x - 1
x^4 - 4x^3 + 6x^2 - 4x + 1
x^5 - 5x^4 + 10x^3 - 10x^2 + 5x - 1
x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1
x^7 - 7x^6 + 21x^5 - 35x^4 + 35x^3 - 21x^2 + 7x - 1
x^8 - 8x^7 + 28x^6 - 56x^5 + 70x^4 - 56x^3 + 28x^2 - 8x + 1
x^9 - 9x^8 + 36x^7 - 84x^6 + 126x^5 - 126x^4 + 84x^3 - 36x^2 + 9x - 1
Primes to 80: [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79]

```



## Stata

Using the [https://ideas.repec.org/c/boc/bocode/s455001.html moremata] library to print the polynomial coefficients. They are in decreasing degree order. To install moremata, type '''ssc install moremata''' in Stata. Since Stata is using double precision floating-point instead of 32 bit integers, the polynomials are exact up to p=54.


```stata
mata
function pol(n) {
	a=J(1,n+1,1)
	r=1
	s=1
	for (k=0; k<n; k++) {
		s=-s
		r=(r*(n-k))/(k+1)
		a[k+2]=r*s
	}
	return(a)
}

for (n=0; n<=7; n++) mm_matlist(pol(n))

               1
    +-------------+
  1 |          1  |
    +-------------+
               1           2
    +-------------------------+
  1 |          1          -1  |
    +-------------------------+
               1           2           3
    +-------------------------------------+
  1 |          1          -2           1  |
    +-------------------------------------+
               1           2           3           4
    +-------------------------------------------------+
  1 |          1          -3           3          -1  |
    +-------------------------------------------------+
               1           2           3           4           5
    +-------------------------------------------------------------+
  1 |          1          -4           6          -4           1  |
    +-------------------------------------------------------------+
               1           2           3           4           5           6
    +-------------------------------------------------------------------------+
  1 |          1          -5          10         -10           5          -1  |
    +-------------------------------------------------------------------------+
               1           2           3           4           5           6           7
    +-------------------------------------------------------------------------------------+
  1 |          1          -6          15         -20          15          -6           1  |
    +-------------------------------------------------------------------------------------+
               1           2           3           4           5           6           7           8
    +-------------------------------------------------------------------------------------------------+
  1 |          1          -7          21         -35          35         -21           7          -1  |
    +-------------------------------------------------------------------------------------------------+


function isprime(n) {
	a=pol(n)
	for (k=2; k<=n; k++) {
		if (mod(a[k],n)) return(0)
	}
	return(1)
}

for (n=2; n<=50; n++) {
	if (isprime(n)) printf("%f ",n)
}

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
end
```



## Swift


```swift
func polynomialCoeffs(n: Int) -> [Int] {
    var result = [Int](count : n+1, repeatedValue : 0)

    result[0]=1
    for i in 1 ..< n/2+1 { //Progress up, until reaching the middle value
        result[i] = result[i-1] * (n-i+1)/i;
    }
    for i in n/2+1 ..< n+1 { //Copy the inverse of the first part
        result[i] = result[n-i];
    }
    // Take into account the sign
    for i in stride(from: 1, through: n, by: 2) {
        result[i] = -result[i]
    }

    return result
}

func isPrime(n: Int) -> Bool {

    var coeffs = polynomialCoeffs(n)

    coeffs[0]--
    coeffs[n]++

    for i in 1 ... n {
        if coeffs[i]%n != 0 {
            return false
        }
    }

    return true
}

for i in 0...10 {

    let coeffs = polynomialCoeffs(i)

    print("(x-1)^\(i) = ")
    if i == 0 {
        print("1")
    } else {
        if i == 1 {
            print("x")
        } else {
            print("x^\(i)")
            if i == 2 {
                print("\(coeffs[i-1])x")
            } else {
                for j in 1...(i - 2) {
                    if j%2 == 0 {
                        print("+\(coeffs[j])x^\(i-j)")
                    } else {
                        print("\(coeffs[j])x^\(i-j)")
                    }
                }
                if (i-1)%2 == 0 {
                    print("+\(coeffs[i-1])x")
                } else {
                    print("\(coeffs[i-1])x")
                }
            }
        }
        if i%2 == 0 {
            print("+\(coeffs[i])")
        } else {
            print("\(coeffs[i])")
        }
    }
    println()
}

println()
print("Primes under 50 : ")

for i in 1...50 {
    if isPrime(i) {
        print("\(i) ")
    }
}

```


{{out}}

```txt

(x-1)^0 = 1
(x-1)^1 = x-1
(x-1)^2 = x^2-2x+1
(x-1)^3 = x^3-3x^2+3x-1
(x-1)^4 = x^4-4x^3+6x^2-4x+1
(x-1)^5 = x^5-5x^4+10x^3-10x^2+5x-1
(x-1)^6 = x^6-6x^5+15x^4-20x^3+15x^2-6x+1
(x-1)^7 = x^7-7x^6+21x^5-35x^4+35x^3-21x^2+7x-1
(x-1)^8 = x^8-8x^7+28x^6-56x^5+70x^4-56x^3+28x^2-8x+1
(x-1)^9 = x^9-9x^8+36x^7-84x^6+126x^5-126x^4+84x^3-36x^2+9x-1
(x-1)^10 = x^10-10x^9+45x^8-120x^7+210x^6-252x^5+210x^4-120x^3+45x^2-10x+1

Primes under 50 : 1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47

```



## Tcl

A recursive method with memorization would be more efficient, but this is sufficient for small-scale work.

```tcl
proc coeffs {p {signs 1}} {
    set clist 1
    for {set i 0} {$i < $p} {incr i} {
	set clist [lmap x [list 0 {*}$clist] y [list {*}$clist 0] {
	    expr {$x + $y}
	}]
    }
    if {$signs} {
	set s -1
	set clist [lmap c $clist {expr {[set s [expr {-$s}]] * $c}}]
    }
    return $clist
}
proc aksprime {p} {
    if {$p < 2} {
	return false
    }
    foreach c [coeffs $p 0] {
	if {$c == 1} continue
	if {$c % $p} {
	    return false
	}
    }
    return true
}

for {set i 0} {$i <= 7} {incr i} {
    puts -nonewline "(x-1)^$i ="
    set j $i
    foreach c [coeffs $i] {
	puts -nonewline [format " %+dx^%d" $c $j]
	incr j -1
    }
    puts ""
}

set sub35primes {}
for {set i 1} {$i < 35} {incr i} {
    if {[aksprime $i]} {
	lappend sub35primes $i
    }
}
puts "primes under 35: [join $sub35primes ,]"

set sub50primes {}
for {set i 1} {$i < 50} {incr i} {
    if {[aksprime $i]} {
	lappend sub50primes $i
    }
}
puts "primes under 50: [join $sub50primes ,]"
```

{{out}}

```txt

(x-1)^0 = +1x^0
(x-1)^1 = +1x^1 -1x^0
(x-1)^2 = +1x^2 -2x^1 +1x^0
(x-1)^3 = +1x^3 -3x^2 +3x^1 -1x^0
(x-1)^4 = +1x^4 -4x^3 +6x^2 -4x^1 +1x^0
(x-1)^5 = +1x^5 -5x^4 +10x^3 -10x^2 +5x^1 -1x^0
(x-1)^6 = +1x^6 -6x^5 +15x^4 -20x^3 +15x^2 -6x^1 +1x^0
(x-1)^7 = +1x^7 -7x^6 +21x^5 -35x^4 +35x^3 -21x^2 +7x^1 -1x^0
primes under 35: 2,3,5,7,11,13,17,19,23,29,31
primes under 50: 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47

```



## uBasic/4tH

<lang>For n = 0 To 9
  Push n : Gosub _coef : Gosub _drop
  Print "(x-1)^";n;" = ";
  Push n : Gosub _show
  Print
Next

Print
Print "primes (never mind the 1):";

For n = 1 To 34
  Push n : Gosub _isprime
  If Pop() Then Print " ";n;
Next

Print
End

                                       ' show polynomial expansions
_show                                  ' ( n --)
  Do
    If @(Tos()) > -1 Then Print "+";
    Print @(Tos());"x^";Tos();
  While (Tos())
    Push Pop() - 1
  Loop

  Gosub _drop
Return

                                       ' test whether number is a prime
_isprime                               ' ( n --)
  Gosub _coef

  i = Tos()
  @(0) = @(0) + 1
  @(i) = @(i) - 1


  Do While (i) * ((@(i) % Tos()) = 0)
    i = i - 1
  Loop

  Gosub _drop
  Push (i = 0)
Return

                                       ' generate coefficients
_coef                                  ' ( n -- n)
  If (Tos() < 0) + (Tos() > 34) Then End
                                       ' gracefully deal with range issue
  i = 0
  @(i) = 1

  Do While i < Tos()
    j = i
    @(j+1) = 1

    Do While j > 0
      @(j) = @(j-1) - @(j)
      j = j - 1
    Loop

    @(0) = -@(0)
    i = i + 1
  Loop
Return

                                       ' drop a value from the stack
_drop                                  ' ( n --)
  If Pop() Endif
Return
```

{{out}}

```txt

(x-1)^0 = +1x^0
(x-1)^1 = +1x^1-1x^0
(x-1)^2 = +1x^2-2x^1+1x^0
(x-1)^3 = +1x^3-3x^2+3x^1-1x^0
(x-1)^4 = +1x^4-4x^3+6x^2-4x^1+1x^0
(x-1)^5 = +1x^5-5x^4+10x^3-10x^2+5x^1-1x^0
(x-1)^6 = +1x^6-6x^5+15x^4-20x^3+15x^2-6x^1+1x^0
(x-1)^7 = +1x^7-7x^6+21x^5-35x^4+35x^3-21x^2+7x^1-1x^0
(x-1)^8 = +1x^8-8x^7+28x^6-56x^5+70x^4-56x^3+28x^2-8x^1+1x^0
(x-1)^9 = +1x^9-9x^8+36x^7-84x^6+126x^5-126x^4+84x^3-36x^2+9x^1-1x^0

primes (never mind the 1): 1 2 3 5 7 11 13 17 19 23 29 31

```



## VBA

{{trans|Phix}}

```vb

'-- Does not work for primes above 97, which is actually beyond the original task anyway.
'-- Translated from the C version, just about everything is (working) out-by-1, what fun.
'-- This updated VBA version utilizes the Decimal datatype to handle numbers requiring
'-- more than 32 bits.
Const MAX = 99
Dim c(MAX + 1) As Variant
Private Sub coef(n As Integer)
'-- out-by-1, ie coef(1)==^0, coef(2)==^1, coef(3)==^2 etc.
    c(n) = CDec(1) 'converts c(n) from Variant to Decimal, a 12 byte data type
    For i = n - 1 To 2 Step -1
        c(i) = c(i) + c(i - 1)
    Next i
End Sub
Private Function is_prime(fn As Variant) As Boolean
    fn = CDec(fn)
    Call coef(fn + 1)   '-- (I said it was out-by-1)
    For i = 2 To fn - 1 '-- (technically "to n" is more correct)
        If c(i) - fn * Int(c(i) / fn) <> 0 Then 'c(i) Mod fn <> 0 Then --Mod works upto 32 bit numbers
            is_prime = False: Exit Function
        End If
    Next i
    is_prime = True
End Function
Private Sub show(n As Integer)
'-- (As per coef, this is (working) out-by-1)
    Dim ci As Variant
    For i = n To 1 Step -1
        ci = c(i)
        If ci = 1 Then
            If (n - i) Mod 2 = 0 Then
                If i = 1 Then
                    If n = 1 Then
                        ci = "1"
                    Else
                        ci = "+1"
                    End If
                Else
                    ci = ""
                End If
            Else
                ci = "-1"
            End If
        Else
            If (n - i) Mod 2 = 0 Then
                ci = "+" & ci
            Else
                ci = "-" & ci
            End If
        End If
        If i = 1 Then '-- ie ^0
            Debug.Print ci
        Else
            If i = 2 Then '-- ie ^1
                Debug.Print ci & "x";
            Else
                Debug.Print ci & "x^" & i - 1;
            End If
        End If
    Next i
End Sub
Public Sub AKS_test_for_primes()
    Dim n As Integer
    For n = 1 To 10 '-- (0 to 9 really)
        coef n
        Debug.Print "(x-1)^" & n - 1 & " = ";
        show n
    Next n
    Debug.Print "primes (<="; MAX; "):"
    coef 2 '-- (needed to reset c, if we want to avoid saying 1 is prime...)
    For n = 2 To MAX
        If is_prime(n) Then
            Debug.Print n;
        End If
    Next n
End Sub

```
{{out}}
```txt

(x-1)^0 = 1
(x-1)^1 = x-1
(x-1)^2 = x^2-2x+1
(x-1)^3 = x^3-3x^2+3x-1
(x-1)^4 = x^4-4x^3+6x^2-4x+1
(x-1)^5 = x^5-5x^4+10x^3-10x^2+5x-1
(x-1)^6 = x^6-6x^5+15x^4-20x^3+15x^2-6x+1
(x-1)^7 = x^7-7x^6+21x^5-35x^4+35x^3-21x^2+7x-1
(x-1)^8 = x^8-8x^7+28x^6-56x^5+70x^4-56x^3+28x^2-8x+1
(x-1)^9 = x^9-9x^8+36x^7-84x^6+126x^5-126x^4+84x^3-36x^2+9x-1
primes (<= 99 ):
 2  3  5  7  11  13  17  19  23  29  31  37  41  43  47  53  59  61  67  71  73  79  83  89  97

```



## Yabasic

{{trans|Phix}}

```Yabasic
// Does not work for primes above 53, which is actually beyond the original task anyway.
// Translated from the C version, just about everything is (working) out-by-1, what fun.

dim c(100)

sub coef(n)
	local i
// out-by-1, ie coef(1)==^0, coef(2)==^1, coef(3)==^2 etc.
    c(n) = 1
    for i = n-1 to 2 step -1
        c(i) = c(i) + c(i-1)
    next
end sub

sub is_prime(n)
	local i

    coef(n+1) // (I said it was out-by-1)
    for i = 2 to n-1   // (technically "to n" is more correct)
        if int(c(i)/n) <> c(i)/n then
            return 0
        end if
    next
    return 1
end sub

sub show(n)
// (As per coef, this is (working) out-by-1)
	local ci, ci$, i

    for i = n to 1 step -1
        ci = c(i)
        if ci = 1 then
            if mod(n-i, 2) = 0 then
                if i = 1 then
                    if n = 1 then
                        ci$ = "1"
                    else
                        ci$ = "+1"
                    end if
                else
                    ci$ = ""
                end if
            else
                ci$ = "-1"
            end if
        else
            if mod(n-i, 2) = 0 then
                ci$ = "+" + str$(ci)
            else
                ci$ = "-" + str$(ci)
            end if
        end if
        if i = 1 then // ie ^0
        	print ci$;
        elsif i=2 then // ie ^1
            print ci$, "x";
        else
            print ci$, "x^", i-1;
        end if
    next
end sub

sub AKS_test_for_primes()
	local n

    for n = 1 to 10 // (0 to 9 really)
        coef(n)
        print "(x-1)^", n-1, " = ";
        show(n)
        print
    next

    print "\nprimes (<=53): ";

    c(2) = 1 // (this manages "", which is all that call did anyway...)
    for n = 2 to 53
        if is_prime(n) then
            print " ", n;
        end if
    next
    print
end sub

AKS_test_for_primes()
```



## zkl

{{trans|Python}}

```zkl
var BN=Import("zklBigNum");
fcn expand_x_1(p){
    ex := L(BN(1));
    foreach i in (p){ ex.append(ex[-1] * -(p-i) / (i+1)) }
    return(ex.reverse())
 }
fcn aks_test(p){
    if (p < 2) return(False);
    ex := expand_x_1(p);
    ex[0] = ex[0] + 1;
    return(not ex[0,-1].filter('%.fp1(p)));
}
println("# p: (x-1)^p for small p");
foreach p in (12){
    println("%3d: ".fmt(p),expand_x_1(p).enumerate()
       .pump(String,fcn([(n,e)]){"%+d%s ".fmt(e,n and "x^%d".fmt(n) or "")}));
}

println("\n# small primes using the aks test");
println([0..110].filter(aks_test).toString(*));
```

{{out}}

```txt

# p: (x-1)^p for small p
  0: +1
  1: -1 +1x^1
  2: +1 -2x^1 +1x^2
  3: -1 +3x^1 -3x^2 +1x^3
  4: +1 -4x^1 +6x^2 -4x^3 +1x^4
  5: -1 +5x^1 -10x^2 +10x^3 -5x^4 +1x^5
  6: +1 -6x^1 +15x^2 -20x^3 +15x^4 -6x^5 +1x^6
  7: -1 +7x^1 -21x^2 +35x^3 -35x^4 +21x^5 -7x^6 +1x^7
  8: +1 -8x^1 +28x^2 -56x^3 +70x^4 -56x^5 +28x^6 -8x^7 +1x^8
  9: -1 +9x^1 -36x^2 +84x^3 -126x^4 +126x^5 -84x^6 +36x^7 -9x^8 +1x^9
 10: +1 -10x^1 +45x^2 -120x^3 +210x^4 -252x^5 +210x^6 -120x^7 +45x^8 -10x^9 +1x^10
 11: -1 +11x^1 -55x^2 +165x^3 -330x^4 +462x^5 -462x^6 +330x^7 -165x^8 +55x^9 -11x^10 +1x^11

# small primes using the aks test
L(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109)

```

