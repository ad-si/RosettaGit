+++
title = "Benford's law"
description = ""
date = 2019-04-26T20:55:13Z
aliases = []
[extra]
id = 13417
[taxonomies]
categories = []
tags = []
+++

{{task}}
{{Wikipedia|Benford's_law}}



'''Benford's law''', also called the '''first-digit law''', refers to the frequency distribution of digits in many (but not all) real-life sources of data.

In this distribution, the number 1 occurs as the first digit about 30% of the time, while larger numbers occur in that position less frequently: 9 as the first digit less than 5% of the time. This distribution of first digits is the same as the widths of gridlines on a logarithmic scale.

Benford's law also concerns the expected distribution for digits beyond the first, which approach a uniform distribution.

This result has been found to apply to a wide variety of data sets, including electricity bills, street addresses, stock prices, population numbers, death rates, lengths of rivers, physical and mathematical constants, and processes described by power laws (which are very common in nature). It tends to be most accurate when values are distributed across multiple orders of magnitude.

A set of numbers is said to satisfy Benford's law if the leading digit <big><math>d</math>  (<math>d \in \{1, \ldots, 9\}</math>)</big> occurs with probability

:::: <big><math>P(d) = \log_{10}(d+1)-\log_{10}(d) = \log_{10}\left(1+\frac{1}{d}\right)</math></big>

For this task, write (a) routine(s) to calculate the distribution of first significant (non-zero) digits in a collection of numbers, then display the actual vs. expected distribution in the way most convenient for your language (table / graph / histogram / whatever).

Use the first 1000 numbers from the Fibonacci sequence as your data set. No need to show how the Fibonacci numbers are obtained.

You can [[Fibonacci sequence|generate]] them or load them [http://www.fullbooks.com/The-first-1001-Fibonacci-Numbers.html from a file]; whichever is easiest.

Display your actual vs expected distribution.


''For extra credit:'' Show the distribution for one other set of numbers from a page on Wikipedia. State which Wikipedia page it can be obtained from and what the set enumerates. Again, no need to display the actual list of numbers or the code to load them.


;<nowiki>See also:</nowiki>
* [http://www.numberphile.com/videos/benfords_law.html numberphile.com].
* A starting page on Wolfram Mathworld is {{Wolfram|Benfords|Law}}.





## 11l

{{trans|D}}

```11l
F get_fibs()
   V a = 1.0
   V b = 1.0
   [Float] r
   L 1000
      r [+]= a
      (a, b) = (b, a + b)
   R r

F benford(seq)
   V freqs = [(0.0, 0.0)] * 9
   V seq_len = 0
   L(d) seq
      I d != 0
         freqs[String(d)[0].code - ‘1’.code][1]++
         seq_len++

   L(&f) freqs
      f = (log10(1.0 + 1.0 / (L.index + 1)), f[1] / seq_len)
   R freqs

print(‘#9 #9 #9’.format(‘Actual’, ‘Expected’, ‘Deviation’))

L(p) benford(get_fibs())
   print(‘#.: #2.2% | #2.2% | #0.4%’.format(L.index + 1, p[1] * 100, p[0] * 100, abs(p[1] - p[0]) * 100))
```

{{out}}

```txt

   Actual  Expected Deviation
1: 30.10% | 30.10% | 0.0030%
2: 17.70% | 17.61% | 0.0909%
3: 12.50% | 12.49% | 0.0061%
4:  9.60% |  9.69% | 0.0910%
5:  8.00% |  7.92% | 0.0819%
6:  6.70% |  6.69% | 0.0053%
7:  5.60% |  5.80% | 0.1992%
8:  5.30% |  5.12% | 0.1847%
9:  4.50% |  4.58% | 0.0757%

```



## 8th


```8th

: n:log10e ` 1 10 ln / ` ;

with: n

: n:log10  \ n -- n
    ln log10e * ;

: benford  \ x -- x
    1 swap / 1+ log10 ;

: fibs \ xt n
    swap >r
    0.0 1.0 rot
    ( dup r@ w:exec tuck + ) swap times
    2drop rdrop ;

var counts

: init
    a:new ( 0 a:push ) 9 times counts ! ;

: leading \ n -- n
    "%g" s:strfmt
    0 s:@ '0 - nip ;

: bump-digit \ n --
    1 swap
    counts @ swap 1- ' + a:op! drop ;

: count-fibs \ --
    ( leading bump-digit ) 1000 fibs ;

: adjust \ --
    counts @  ( 0.001 * ) a:map  counts ! ;

: spaces \ n --
    ' space swap times ;

: .fw \ s n --
    >r s:len r> rot . swap - spaces ;

: .header \ --
    "Digit" 8 .fw "Expected" 10 .fw "Actual" 10 .fw cr ;

: .digit \ n --
    >s 8 .fw ;

: .actual \ n --
    "%.3f" s:strfmt 10 .fw ;

: .expected \ n --
    "%.4f" s:strfmt 10 .fw ;

: report \ --
    .header
    counts @
    ( swap 1+ dup benford swap
      .digit .expected .actual cr )
    a:each drop ;

: benford-test
    init count-fibs adjust report ;

;with

benford-test
bye

```


{{out}}

```txt

Digit   Expected  Actual
1       0.3010    0.301
2       0.1761    0.177
3       0.1249    0.125
4       0.0969    0.096
5       0.0792    0.080
6       0.0669    0.067
7       0.0580    0.056
8       0.0512    0.053
9       0.0458    0.045

```



## Ada


The program reads the Fibonacci-Numbers from the standard input. Each input line is supposed to hold N, followed by Fib(N).


```Ada
with Ada.Text_IO, Ada.Numerics.Generic_Elementary_Functions;

procedure Benford is

   subtype Nonzero_Digit is Natural range 1 .. 9;
   function First_Digit(S: String) return Nonzero_Digit is
      (if S(S'First) in '1' .. '9'
         then Nonzero_Digit'Value(S(S'First .. S'First))
         else First_Digit(S(S'First+1 .. S'Last)));

   package N_IO is new Ada.Text_IO.Integer_IO(Natural);

   procedure Print(D: Nonzero_Digit; Counted, Sum: Natural) is
      package Math is new Ada.Numerics.Generic_Elementary_Functions(Float);
      package F_IO is new Ada.Text_IO.Float_IO(Float);
      Actual: constant Float := Float(Counted) / Float(Sum);
      Expected: constant Float := Math.Log(1.0 + 1.0 / Float(D), Base => 10.0);
      Deviation: constant Float := abs(Expected-Actual);
   begin
      N_IO.Put(D, 5);
      N_IO.Put(Counted, 14);
      F_IO.Put(Float(Sum)*Expected, Fore => 16, Aft => 1, Exp => 0);
      F_IO.Put(100.0*Actual, Fore => 9, Aft => 2, Exp => 0);
      F_IO.Put(100.0*Expected, Fore => 11, Aft => 2, Exp => 0);
      F_IO.Put(100.0*Deviation, Fore => 13, Aft => 2, Exp => 0);
   end Print;

   Cnt: array(Nonzero_Digit) of Natural := (1 .. 9 => 0);
   D: Nonzero_Digit;
   Sum: Natural := 0;
   Counter: Positive;

begin
   while not Ada.Text_IO.End_Of_File loop
      -- each line in the input file holds Counter, followed by Fib(Counter)
      N_IO.Get(Counter);
        -- Counter and skip it, we just don't need it
      D := First_Digit(Ada.Text_IO.Get_Line);
        -- read the rest of the line and extract the first digit
      Cnt(D) := Cnt(D)+1;
      Sum := Sum + 1;
   end loop;
   Ada.Text_IO.Put_Line(" Digit  Found[total]   Expected[total]    Found[%]"
                                          & "   Expected[%]   Difference[%]");
   for I in Nonzero_Digit loop
      Print(I, Cnt(I), Sum);
      Ada.Text_IO.New_Line;
   end loop;
end Benford;
```


{{out}}


```txt
>./benford < fibo.txt
 Digit  Found[total]   Expected[total]    Found[%]   Expected[%]   Difference[%]
    1           301             301.0       30.10         30.10            0.00
    2           177             176.1       17.70         17.61            0.09
    3           125             124.9       12.50         12.49            0.01
    4            96              96.9        9.60          9.69            0.09
    5            80              79.2        8.00          7.92            0.08
    6            67              66.9        6.70          6.69            0.01
    7            56              58.0        5.60          5.80            0.20
    8            53              51.2        5.30          5.12            0.18
    9            45              45.8        4.50          4.58            0.08
```



###  Extra Credit


Input is the list of primes below 100,000 from [http://www.mathsisfun.com/numbers/prime-number-lists.html]. Since each line in that file holds prime and only a prime, but no ongoing counter, we must slightly modify the program by commenting out a single line:


```Ada
      -- N_IO.Get(Counter);
```


We can also edit out the declaration of the variable "Counter" ...or live with a compiler warning about never reading or assigning that variable.

{{out}}

As it turns out, the distribution of the first digits of primes is almost flat and does '''not''' seem follow Benford's law:


```txt
>./benford < primes-to-100k.txt
 Digit  Found[total]   Expected[total]    Found[%]   Expected[%]   Difference[%]
    1          1193            2887.5       12.44         30.10           17.67
    2          1129            1689.1       11.77         17.61            5.84
    3          1097            1198.4       11.44         12.49            1.06
    4          1069             929.6       11.14          9.69            1.45
    5          1055             759.5       11.00          7.92            3.08
    6          1013             642.2       10.56          6.69            3.87
    7          1027             556.3       10.71          5.80            4.91
    8          1003             490.7       10.46          5.12            5.34
    9          1006             438.9       10.49          4.58            5.91
```



## Aime


```aime
text
sum(text a, text b)
{
    data d;
    integer e, f, n, r;

    e = ~a;
    f = ~b;

    r = 0;

    n = min(e, f);
    while (n) {
        n -= 1;
        e -= 1;
        f -= 1;
        r += a[e] - '0';
        r += b[f] - '0';
        b_insert(d, 0, r % 10 + '0');
        r /= 10;
    }

    if (f) {
        e = f;
        a = b;
    }

    while (e) {
        e -= 1;
        r += a[e] - '0';
        b_insert(d, 0, r % 10 + '0');
        r /= 10;
    }

    if (r) {
        b_insert(d, 0, r + '0');
    }

    d;
}

text
fibs(list l, integer n)
{
    integer c, i;
    text a, b, w;

    l[1] = 1;

    a = "0";
    b = "1";
    i = 1;
    while (i < n) {
        w = sum(a, b);
        a = b;
        b = w;
        c = w[0] - '0';
        l[c] = 1 + l[c];
        i += 1;
    }

    w;
}

integer
main(void)
{
    integer i, n;
    list f;
    real m;

    n = 1000;

    f.pn_integer(0, 10, 0);

    fibs(f, n);

    m = 100r / n;

    o_text("\t\texpected\t   found\n");
    i = 0;
    while (i < 9) {
        i += 1;
        o_form("%8d/p3d3w16//p3d3w16/\n", i, 100 * log10(1 + 1r / i), f[i] * m);
    }

    0;
}
```

{{out}}

```txt
		expected	   found
       1          30.102          30.1
       2          17.609          17.7
       3          12.493          12.5
       4           9.691           9.600
       5           7.918           8
       6           6.694           6.7
       7           5.799           5.6
       8           5.115           5.300
       9           4.575           4.5
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}
Uses Algol 68G's LONG LONG INT which has programmer specifiable precision.

```algol68
BEGIN
    # set the number of digits for LONG LONG INT values #
    PR precision 256 PR
    # returns the probability of the first digit of each non-zero number in s #
    PROC digit probability = ( []LONG LONG INT s )[]REAL:
         BEGIN
            [ 1 : 9 ]REAL result;
            # count the number of times each digit is the first #
            [ 1 : 9 ]INT  count := ( 0, 0, 0, 0, 0, 0, 0, 0, 0 );
            FOR i FROM LWB s TO UPB s DO
                LONG LONG INT v := ABS s[ i ];
                IF v /= 0 THEN
                    WHILE v > 9 DO v OVERAB 10 OD;
                    count[ SHORTEN SHORTEN v ] +:= 1
                FI
            OD;
            # calculate the probability of each digit #
            INT number of elements = ( UPB s + 1 ) - LWB s;
            FOR i TO 9 DO
                result[ i ] := IF number of elements = 0 THEN 0 ELSE count[ i ] / number of elements FI
            OD;
            result
         END # digit probability # ;
    # outputs the digit probabilities of some numbers and those expected by Benford's law #
    PROC compare to benford = ( []REAL actual )VOID:
         FOR i TO 9 DO
            print( ( "Benford: ", fixed( log( 1 + ( 1 / i ) ), -7, 3 ), " actual: ", fixed( actual[ i ], -7, 3 ), newline ) )
         OD # compare to benford # ;
    # generate 1000 fibonacci numbers #
    [ 0 : 1000 ]LONG LONG INT fn;
    fn[ 0 ] := 0;
    fn[ 1 ] := 1;
    FOR i FROM 2 TO UPB fn DO fn[ i ] := fn[ i - 1 ] + fn[ i - 2 ] OD;
    # get the probabilities of each first digit of the fibonacci numbers and #
    # compare to the probabilities expected by Benford's law #
    compare to benford( digit probability( fn ) )
END
```

{{out}}

```txt

Benford:   0.301 actual:   0.301
Benford:   0.176 actual:   0.177
Benford:   0.125 actual:   0.125
Benford:   0.097 actual:   0.096
Benford:   0.079 actual:   0.080
Benford:   0.067 actual:   0.067
Benford:   0.058 actual:   0.056
Benford:   0.051 actual:   0.053
Benford:   0.046 actual:   0.045

```



## AutoHotkey

{{works with|AutoHotkey_L}}(AutoHotkey1.1+)

```AutoHotkey
SetBatchLines, -1
fib := NStepSequence(1, 1, 2, 1000)
Out := "Digit`tExpected`tObserved`tDeviation`n"
n := []
for k, v in fib
	d := SubStr(v, 1, 1)
	, n[d] := n[d] ? n[d] + 1 : 1
for k, v in n
	Exppected := 100 * Log(1+ (1 / k))
	, Observed := (v / fib.MaxIndex()) * 100
	, Out .= k "`t" Exppected "`t" Observed "`t" Abs(Exppected - Observed) "`n"
MsgBox, % Out

NStepSequence(v1, v2, n, k) {
	a := [v1, v2]
	Loop, % k - 2 {
		a[j := A_Index + 2] := 0
		Loop, % j < n + 2 ? j - 1 : n
			a[j] := BigAdd(a[j - A_Index], a[j])
	}
	return, a
}

BigAdd(a, b) {
	if (StrLen(b) > StrLen(a))
		t := a, a := b, b := t
	LenA := StrLen(a) + 1, LenB := StrLen(B) + 1, Carry := 0
	Loop, % LenB - 1
		Sum := SubStr(a, LenA - A_Index, 1) + SubStr(B, LenB - A_Index, 1) + Carry
		, Carry := Sum // 10
		, Result := Mod(Sum, 10) . Result
	Loop, % I := LenA - LenB {
		if (!Carry) {
			Result := SubStr(a, 1, I) . Result
			break
		}
		Sum := SubStr(a, I, 1) + Carry
		, Carry := Sum // 10
		, Result := Mod(Sum, 10) . Result
		, I--
	}
	return, (Carry ? Carry : "") . Result
}
```
NStepSequence() is available [http://rosettacode.org/wiki/Fibonacci_n-step_number_sequences#AutoHotkey here].
'''Output:'''

```txt
Digit	Expected	Observed	Deviation
1	30.103000	30.100000	0.003000
2	17.609126	17.700000	0.090874
3	12.493874	12.500000	0.006126
4	9.691001	9.600000	0.091001
5	7.918125	8.000000	0.081875
6	6.694679	6.700000	0.005321
7	5.799195	5.600000	0.199195
8	5.115252	5.300000	0.184748
9	4.575749	4.500000	0.075749
```



## AWK


```AWK

# syntax: GAWK -f BENFORDS_LAW.AWK
BEGIN {
    n = 1000
    for (i=1; i<=n; i++) {
      arr[substr(fibonacci(i),1,1)]++
    }
    print("digit expected observed deviation")
    for (i=1; i<=9; i++) {
      expected  = log10(i+1) - log10(i)
      actual    = arr[i] / n
      deviation = expected - actual
      printf("%5d %8.4f %8.4f %9.4f\n",i,expected*100,actual*100,abs(deviation*100))
    }
    exit(0)
}
function fibonacci(n,  a,b,c,i) {
    a = 0
    b = 1
    for (i=1; i<=n; i++) {
      c = a + b
      a = b
      b = c
    }
    return(c)
}
function abs(x) { if (x >= 0) { return x } else { return -x } }
function log10(x) { return log(x)/log(10) }

```

{{out}}

```txt

digit expected observed deviation
    1  30.1030  30.0000    0.1030
    2  17.6091  17.7000    0.0909
    3  12.4939  12.5000    0.0061
    4   9.6910   9.6000    0.0910
    5   7.9181   8.0000    0.0819
    6   6.6947   6.7000    0.0053
    7   5.7992   5.7000    0.0992
    8   5.1153   5.3000    0.1847
    9   4.5757   4.5000    0.0757

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

float *benford_distribution(void)
{
    static float prob[9];
    for (int i = 1; i < 10; i++)
        prob[i - 1] = log10f(1 + 1.0 / i);

    return prob;
}

float *get_actual_distribution(char *fn)
{
    FILE *input = fopen(fn, "r");
    if (!input)
    {
        perror("Can't open file");
        exit(EXIT_FAILURE);
    }

    int tally[9] = { 0 };
    char c;
    int total = 0;
    while ((c = getc(input)) != EOF)
    {
        /* get the first nonzero digit on the current line */
        while (c < '1' || c > '9')
            c = getc(input);

        tally[c - '1']++;
        total++;

        /* discard rest of line */
        while ((c = getc(input)) != '\n' && c != EOF)
            ;
    }
    fclose(input);

    static float freq[9];
    for (int i = 0; i < 9; i++)
        freq[i] = tally[i] / (float) total;

    return freq;
}

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        printf("Usage: benford <file>\n");
        return EXIT_FAILURE;
    }

    float *actual = get_actual_distribution(argv[1]);
    float *expected = benford_distribution();

    puts("digit\tactual\texpected");
    for (int i = 0; i < 9; i++)
        printf("%d\t%.3f\t%.3f\n", i + 1, actual[i], expected[i]);

    return EXIT_SUCCESS;
}
```


{{Out}}
Use with a file which should contain a number on each line.

```txt
$ ./benford fib1000.txt
digit   actual  expected
1       0.301   0.301
2       0.177   0.176
3       0.125   0.125
4       0.096   0.097
5       0.080   0.079
6       0.067   0.067
7       0.056   0.058
8       0.053   0.051
9       0.045   0.046
```



## C++


```cpp
//to cope with the big numbers , I used the Class Library for Numbers( CLN )
//if used prepackaged you can compile writing "g++ -std=c++11 -lcln yourprogram.cpp -o yourprogram"
#include <cln/integer.h>
#include <cln/integer_io.h>
#include <iostream>
#include <algorithm>
#include <vector>
#include <iomanip>
#include <sstream>
#include <string>
#include <cstdlib>
#include <cmath>
#include <map>
using namespace cln ;

class NextNum {
public :
   NextNum ( cl_I & a , cl_I & b ) : first( a ) , second ( b ) { }
   cl_I operator( )( ) {
      cl_I result = first + second ;
      first = second ;
      second = result ;
      return result ;
   }
private :
   cl_I first ;
   cl_I second ;
} ;

void findFrequencies( const std::vector<cl_I> & fibos , std::map<int , int> &numberfrequencies  ) {
   for ( cl_I bignumber : fibos ) {
      std::ostringstream os ;
      fprintdecimal ( os , bignumber ) ;//from header file cln/integer_io.h
      int firstdigit = std::atoi( os.str( ).substr( 0 , 1 ).c_str( )) ;
      auto result = numberfrequencies.insert( std::make_pair( firstdigit , 1 ) ) ;
      if ( ! result.second )
	 numberfrequencies[ firstdigit ]++ ;
   }
}

int main( ) {
   std::vector<cl_I> fibonaccis( 1000 ) ;
   fibonaccis[ 0 ] = 0 ;
   fibonaccis[ 1 ] = 1 ;
   cl_I a = 0 ;
   cl_I b = 1 ;
   //since a and b are passed as references to the generator's constructor
   //they are constantly changed !
   std::generate_n( fibonaccis.begin( ) + 2 , 998 , NextNum( a , b ) ) ;
   std::cout << std::endl ;
   std::map<int , int> frequencies ;
   findFrequencies( fibonaccis , frequencies ) ;
   std::cout << "                found                    expected\n" ;
   for ( int i = 1 ; i < 10 ; i++ ) {
      double found = static_cast<double>( frequencies[ i ] ) / 1000 ;
      double expected = std::log10( 1 + 1 / static_cast<double>( i )) ;
      std::cout << i << " :" << std::setw( 16 ) << std::right << found * 100 << " %" ;
      std::cout.precision( 3 ) ;
      std::cout << std::setw( 26 ) << std::right << expected * 100 << " %\n" ;
   }
   return 0 ;
}

```

{{out}}

```txt
                found                    expected
1 :            30.1 %                      30.1 %
2 :            17.7 %                      17.6 %
3 :            12.5 %                      12.5 %
4 :             9.5 %                      9.69 %
5 :               8 %                      7.92 %
6 :             6.7 %                      6.69 %
7 :             5.6 %                       5.8 %
8 :             5.3 %                      5.12 %
9 :             4.5 %                      4.58 %

```



## Clojure


```lisp
(ns example
  (:gen-class))

(defn abs [x]
  (if (> x 0)
    x
    (- x)))

(defn calc-benford-stats [digits]
  " Frequencies of digits in data "
  (let [y (frequencies digits)
         tot (reduce + (vals y))]
    [y tot]))

(defn show-benford-stats [v]
  " Prints in percent the actual, Benford expected, and difference"
  (let [fd (map (comp first str) v)]        ; first digit of each record
    (doseq [q (range 1 10)
            :let [[y tot] (calc-benford-stats fd)
                  d (first (str q))         ; reference digit
                  f (/ (get y d 0) tot 0.01)  ; percent of occurence of digit
                  p (* (Math/log10 (/ (inc q) q)) 100)  ; Benford expected percent
                  e (abs (- f p))]]                     ; error (difference)
      (println (format "%3d %10.2f %10.2f %10.2f"
                       q
                       f
                       p
                       e)))))

; Generate fibonacci results
(def fib (lazy-cat [0N 1N] (map + fib (rest fib))))

;(def fib-digits (map (comp first str) (take 10000 fib)))
(def fib-digits (take 10000 fib))
(def header "         found-%    expected-%  diff")

(println "Fibonacci Results")
(println header)
(show-benford-stats fib-digits)
;
; Universal Constants from Physics (using first column of data)
(println "Universal Constants from Physics")
(println header)
(let [
      data-parser (fn [s]
                  (let [x (re-find #"\s{10}-?[0|/\.]*([1-9])" s)]
                    (if (not (nil? x))    ; Skips records without number
                      (second x)
                      x)))

      input (slurp "http://physics.nist.gov/cuu/Constants/Table/allascii.txt")

      y (for [line (line-seq (java.io.BufferedReader.
                               (java.io.StringReader. input)))]
          (data-parser line))
      z (filter identity y)]
  (show-benford-stats z))

; Sunspots
(println "Sunspots average count per month since 1749")
(println header)
(let [
      data-parser (fn [s]
                  (nth (re-find #"(.+?\s){3}([1-9])" s) 2))

      ; Sunspot data loaded from file (saved from ;https://solarscience.msfc.nasa.gov/greenwch/SN_m_tot_V2.0.txt")
      ; (note: attempting to load directly from url causes https Trust issues, so saved to file after loading to Browser)
      input (slurp "SN_m_tot_V2.0.txt")
      y (for [line (line-seq (java.io.BufferedReader.
                               (java.io.StringReader. input)))]
          (data-parser line))]

  (show-benford-stats y))


```

{{Output}}

```txt

Fibonacci Results
         found-%    expected-%  diff
  1      30.11      30.10       0.01
  2      17.62      17.61       0.01
  3      12.49      12.49       0.00
  4       9.68       9.69       0.01
  5       7.92       7.92       0.00
  6       6.68       6.69       0.01
  7       5.80       5.80       0.00
  8       5.13       5.12       0.01
  9       4.56       4.58       0.02
Universal Constants from Physics
         found-%    expected-%  diff
  1      34.34      30.10       4.23
  2      18.67      17.61       1.07
  3       9.04      12.49       3.46
  4       8.43       9.69       1.26
  5       8.43       7.92       0.52
  6       7.23       6.69       0.53
  7       3.31       5.80       2.49
  8       5.12       5.12       0.01
  9       5.42       4.58       0.85
Sunspots average count per month since 1749
         found-%    expected-%  diff
  1      37.44      30.10       7.34
  2      16.28      17.61       1.33
  3       7.16      12.49       5.34
  4       6.88       9.69       2.81
  5       6.35       7.92       1.57
  6       6.04       6.69       0.66
  7       7.25       5.80       1.45
  8       5.57       5.12       0.46
  9       5.76       4.58       1.18

```


## CoffeeScript


```coffeescript
fibgen = () ->
    a = 1; b = 0
    return () ->
        ([a, b] = [b, a+b])[1]

leading = (x) -> x.toString().charCodeAt(0) - 0x30

f = fibgen()

benford = (0 for i in [1..9])
benford[leading(f()) - 1] += 1 for i in [1..1000]

log10 = (x) -> Math.log(x) * Math.LOG10E

actual = benford.map (x) -> x * 0.001
expected = (log10(1 + 1/x) for x in [1..9])

console.log "Leading digital distribution of the first 1,000 Fibonacci numbers"
console.log "Digit\tActual\tExpected"
for i in [1..9]
    console.log i + "\t" + actual[i - 1].toFixed(3) + '\t' + expected[i - 1].toFixed(3)
```

{{out}}

```txt
Leading digital distribution of the first 1,000 Fibonacci numbers
Digit   Actual  Expected
1       0.301   0.301
2       0.177   0.176
3       0.125   0.125
4       0.096   0.097
5       0.080   0.079
6       0.067   0.067
7       0.056   0.058
8       0.053   0.051
9       0.045   0.046
```



## Common Lisp


```lisp
(defun calculate-distribution (numbers)
  "Return the frequency distribution of the most significant nonzero
   digits in the given list of numbers. The first element of the list
   is the frequency for digit 1, the second for digit 2, and so on."

  (defun nonzero-digit-p (c)
    "Check whether the character is a nonzero digit"
    (and (digit-char-p c) (char/= c #\0)))

  (defun first-digit (n)
    "Return the most significant nonzero digit of the number or NIL if
     there is none."
    (let* ((s (write-to-string n))
           (c (find-if #'nonzero-digit-p s)))
      (when c
        (digit-char-p c))))

  (let ((tally (make-array 9 :element-type 'integer :initial-element 0)))
    (loop for n in numbers
          for digit = (first-digit n)
          when digit
          do (incf (aref tally (1- digit))))
    (loop with total = (length numbers)
          for digit-count across tally
          collect (/ digit-count total))))

(defun calculate-benford-distribution ()
  "Return the frequency distribution according to Benford's law.
   The first element of the list is the probability for digit 1, the second
   element the probability for digit 2, and so on."
  (loop for i from 1 to 9
        collect (log (1+ (/ i)) 10)))

(defun benford (numbers)
  "Print a table of the actual and expected distributions for the given
   list of numbers."
  (let ((actual-distribution (calculate-distribution numbers))
        (expected-distribution (calculate-benford-distribution)))
    (write-line "digit actual expected")
    (format T "~:{~3D~9,3F~8,3F~%~}"
            (map 'list #'list '(1 2 3 4 5 6 7 8 9)
                              actual-distribution
                              expected-distribution))))
```



```txt
; *fib1000* is a list containing the first 1000 numbers in the Fibonnaci sequence
> (benford *fib1000*)
digit actual expected
  1    0.301   0.301
  2    0.177   0.176
  3    0.125   0.125
  4    0.096   0.097
  5    0.080   0.079
  6    0.067   0.067
  7    0.056   0.058
  8    0.053   0.051
  9    0.045   0.046
```



## D

{{trans|Scala}}

```d
import std.stdio, std.range, std.math, std.conv, std.bigint;

double[2][9] benford(R)(R seq) if (isForwardRange!R && !isInfinite!R) {
    typeof(return) freqs = 0;
    uint seqLen = 0;
    foreach (d; seq)
        if (d != 0) {
            freqs[d.text[0] - '1'][1]++;
            seqLen++;
        }

    foreach (immutable i, ref p; freqs)
        p = [log10(1.0 + 1.0 / (i + 1)), p[1] / seqLen];
    return freqs;
}

void main() {
    auto fibs = recurrence!q{a[n - 1] + a[n - 2]}(1.BigInt, 1.BigInt);

    writefln("%9s %9s %9s", "Actual", "Expected", "Deviation");
    foreach (immutable i, immutable p; fibs.take(1000).benford)
        writefln("%d: %5.2f%% | %5.2f%% | %5.4f%%",
                 i+1, p[1] * 100, p[0] * 100, abs(p[1] - p[0]) * 100);
}
```

{{out}}

```txt
   Actual  Expected Deviation
1: 30.10% | 30.10% | 0.0030%
2: 17.70% | 17.61% | 0.0908%
3: 12.50% | 12.49% | 0.0061%
4:  9.60% |  9.69% | 0.0910%
5:  8.00% |  7.92% | 0.0818%
6:  6.70% |  6.69% | 0.0053%
7:  5.60% |  5.80% | 0.1992%
8:  5.30% |  5.12% | 0.1847%
9:  4.50% |  4.58% | 0.0757%

```



### Alternative Version

The output is the same.

```d
import std.stdio, std.range, std.math, std.conv, std.bigint,
       std.algorithm, std.array;

auto benford(R)(R seq) if (isForwardRange!R && !isInfinite!R) {
    return seq.filter!q{a != 0}.map!q{a.text[0]-'1'}.array.sort().group;
}

void main() {
    auto fibs = recurrence!q{a[n - 1] + a[n - 2]}(1.BigInt, 1.BigInt);
    auto expected = iota(1, 10).map!(d => log10(1.0 + 1.0 / d));

    enum N = 1_000;
    writefln("%9s %9s %9s", "Actual", "Expected", "Deviation");
    foreach (immutable i, immutable f; fibs.take(N).benford)
        writefln("%d: %5.2f%% | %5.2f%% | %5.4f%%", i + 1,
                 f * 100.0 / N, expected[i] * 100,
                 abs((f / double(N)) - expected[i]) * 100);
}
```



## Elixir


```elixir
defmodule Benfords_law do
  def distribution(n), do: :math.log10( 1 + (1 / n) )

  def task(total \\ 1000) do
    IO.puts "Digit	Actual	Benfords expected"
    fib(total)
    |> Enum.group_by(fn i -> hd(to_char_list(i)) end)
    |> Enum.map(fn {key,list} -> {key - ?0, length(list)} end)
    |> Enum.sort
    |> Enum.each(fn {x,len} -> IO.puts "#{x}	#{len / total}	#{distribution(x)}" end)
  end

  defp fib(n) do                        # suppresses zero
    Stream.unfold({1,1}, fn {a,b} -> {a,{b,a+b}} end) |> Enum.take(n)
  end
end

Benfords_law.task
```


{{out}}

```txt

Digit   Actual  Benfords expected
1       0.301   0.3010299956639812
2       0.177   0.17609125905568124
3       0.125   0.12493873660829993
4       0.096   0.09691001300805642
5       0.08    0.07918124604762482
6       0.067   0.06694678963061322
7       0.056   0.05799194697768673
8       0.053   0.05115252244738129
9       0.045   0.04575749056067514

```



## Erlang


```Erlang

-module( benfords_law ).
-export( [actual_distribution/1, distribution/1, task/0] ).

actual_distribution( Ns ) -> lists:foldl( fun first_digit_count/2, dict:new(), Ns ).

distribution( N ) -> math:log10( 1 + (1 / N) ).

task() ->
	Total = 1000,
	Fibonaccis = fib( Total ),
	Actual_dict = actual_distribution( Fibonaccis ),
	Keys = lists:sort( dict:fetch_keys( Actual_dict) ),
	io:fwrite( "Digit	Actual	Benfords expected~n" ),
	[io:fwrite("~p	~p	~p~n", [X, dict:fetch(X, Actual_dict) / Total, distribution(X)]) || X <- Keys].



fib( N ) -> fib( N, 0, 1, [] ).
fib( 0, Current, _, Acc ) -> lists:reverse( [Current | Acc] );
fib( N, Current, Next, Acc ) -> fib( N-1, Next, Current+Next, [Current | Acc] ).

first_digit_count( 0, Dict ) -> Dict;
first_digit_count( N, Dict ) ->
	[Key | _] = erlang:integer_to_list( N ),
	dict:update_counter( Key - 48, 1, Dict ).

```

{{out}}

```txt

7> benfords_law:task().
Digit   Actual  Benfords expected
1       0.301   0.3010299956639812
2       0.177   0.17609125905568124
3       0.125   0.12493873660829993
4       0.096   0.09691001300805642
5       0.08    0.07918124604762482
6       0.067   0.06694678963061322
7       0.056   0.05799194697768673
8       0.053   0.05115252244738129
9       0.045   0.04575749056067514

```



## Factor


```factor
USING: assocs compiler.tree.propagation.call-effect formatting
kernel math math.functions math.statistics math.text.utils
sequences ;
IN: rosetta-code.benfords-law

: expected ( n -- x ) recip 1 + log10 ;

: next-fib ( vec -- vec' )
    [ last2 ] keep [ + ] dip [ push ] keep ;

: data ( -- seq ) V{ 1 1 } clone 998 [ next-fib ] times ;

: 1st-digit ( n -- m ) 1 digit-groups last ;

: leading ( -- seq ) data [ 1st-digit ] map ;

: .header ( -- )
    "Digit" "Expected" "Actual" "%-10s%-10s%-10s\n" printf ;

: digit-report ( digit digit-count -- digit expected actual )
    dupd [ expected ] dip 1000 /f ;

: .digit-report ( digit digit-count -- )
    digit-report "%-10d%-10.4f%-10.4f\n" printf ;

: main ( -- )
    .header leading histogram [ .digit-report ] assoc-each ;

MAIN: main
```

{{out}}

```txt

Digit     Expected  Actual
1         0.3010    0.3010
2         0.1761    0.1770
3         0.1249    0.1250
4         0.0969    0.0960
5         0.0792    0.0800
6         0.0669    0.0670
7         0.0580    0.0560
8         0.0512    0.0530
9         0.0458    0.0450

```



## Forth


```forth
: 3drop   drop 2drop ;
: f2drop  fdrop fdrop ;

: int-array  create cells allot  does> swap cells + ;

: 1st-fib   0e 1e ;
: next-fib  ftuck f+ ;

: 1st-digit ( fp -- n )
    pad 6 represent 3drop pad c@ [char] 0 - ;

10 int-array counts

: tally
    0 counts 10 cells erase
    1st-fib
    1000 0 DO
        1 fdup 1st-digit counts +!
        next-fib
    LOOP f2drop ;

: benford ( d -- fp )
    s>f 1/f 1e f+ flog ;

: tab  9 emit ;

: heading  ( -- )
    cr ." Leading digital distribution of the first 1,000 Fibonacci numbers:"
    cr ." Digit" tab ." Actual" tab ." Expected" ;

: .fixed ( n -- ) \ print count as decimal fraction
    s>d <# # # # [char] . hold #s #> type space ;

: report ( -- )
    precision  3 set-precision
    heading
    10 1 DO
        cr i 3 .r
        tab i counts @ .fixed
        tab i benford f.
    LOOP
    set-precision ;

: compute-benford  tally report ;
```

{{Out}}

```txt
Gforth 0.7.2, Copyright (C) 1995-2008 Free Software Foundation, Inc.
Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
Type `bye' to exit
compute-benford
Leading digital distribution of the first 1,000 Fibonacci numbers:
Digit	Actual	Expected
  1	0.301 	0.301
  2	0.177 	0.176
  3	0.125 	0.125
  4	0.096 	0.0969
  5	0.080 	0.0792
  6	0.067 	0.0669
  7	0.056 	0.058
  8	0.053 	0.0512
  9	0.045 	0.0458  ok
```



## Fortran

FORTRAN 90.  Compilation and output of this program using emacs compile command and a fairly obvious Makefile entry:

```fortran
-*- mode: compilation; default-directory: "/tmp/" -*-
Compilation started at Sat May 18 01:13:00

a=./f && make $a && $a
f95 -Wall -ffree-form f.F -o f
  0.301030010      0.176091254      0.124938756       9.69100147E-02   7.91812614E-02   6.69467747E-02   5.79919666E-02   5.11525236E-02   4.57575098E-02 THE LAW
  0.300999999      0.177000001      0.125000000       9.60000008E-02   7.99999982E-02   6.70000017E-02   5.70000000E-02   5.29999994E-02   4.50000018E-02 LEADING FIBONACCI DIGIT

Compilation finished at Sat May 18 01:13:00
```



```fortran
subroutine fibber(a,b,c,d)
  ! compute most significant digits, Fibonacci like.
  implicit none
  integer (kind=8), intent(in) :: a,b
  integer (kind=8), intent(out) :: c,d
  d = a + b
  if (15 .lt. log10(float(d))) then
    c = b/10
    d = d/10
  else
    c = b
  endif
end subroutine fibber

integer function leadingDigit(a)
  implicit none
  integer (kind=8), intent(in) :: a
  integer (kind=8) :: b
  b = a
  do while (9 .lt. b)
    b = b/10
  end do
  leadingDigit = transfer(b,leadingDigit)
end function leadingDigit

real function benfordsLaw(a)
  implicit none
  integer, intent(in) :: a
  benfordsLaw = log10(1.0 + 1.0 / a)
end function benfordsLaw

program benford

  implicit none

  interface

    subroutine fibber(a,b,c,d)
      implicit none
      integer (kind=8), intent(in) :: a,b
      integer (kind=8), intent(out) :: c,d
    end subroutine fibber

    integer function leadingDigit(a)
      implicit none
      integer (kind=8), intent(in) :: a
    end function leadingDigit

    real function benfordsLaw(a)
      implicit none
      integer, intent(in) :: a
    end function benfordsLaw

  end interface

  integer (kind=8) :: a, b, c, d
  integer :: i, count(10)
  data count/10*0/
  a = 1
  b = 1
  do i = 1, 1001
    count(leadingDigit(a)) = count(leadingDigit(a)) + 1
    call fibber(a,b,c,d)
    a = c
    b = d
  end do
  write(6,*) (benfordsLaw(i),i=1,9),'THE LAW'
  write(6,*) (count(i)/1000.0 ,i=1,9),'LEADING FIBONACCI DIGIT'
end program benford
```


## FreeBASIC

{{libheader|GMP}}

```freebasic
' version 27-10-2016
' compile with: fbc -s console

#Define max 1000    ' total number of Fibonacci numbers
#Define max_sieve  15485863  ' should give 1,000,000

#Include Once "gmp.bi"   ' uses the GMP libary

Dim As ZString Ptr z_str
Dim As ULong n, d
ReDim As ULong digit(1 To 9)
Dim As Double expect, found

Dim As mpz_ptr fib1, fib2
fib1 = Allocate(Len(__mpz_struct)) : Mpz_init_set_ui(fib1, 0)
fib2 = Allocate(Len(__mpz_struct)) : Mpz_init_set_ui(fib2, 1)

digit(1) = 1  ' fib2
For n = 2 To max
    Swap fib1, fib2                   ' fib1 = 1, fib2 = 0
    mpz_add(fib2, fib1, fib2)         ' fib1 = 1, fib2 = 1 (fib1 + fib2)
    z_str = mpz_get_str(0, 10, fib2)
    d = Val(Left(*z_str, 1))          ' strip the 1 digit on the left off
    digit(d) = digit(d) +1
Next

mpz_clear(fib1) : DeAllocate(fib1)
mpz_clear(fib2) : DeAllocate(fib2)

Print
Print "First 1000 Fibonacci numbers"
Print "nr:  total     found   expected  difference"

For d = 1 To 9
    n = digit(d)
    found = n / 10
    expect = (Log(1 + 1 / d) / Log(10)) * 100
    Print Using " ##  #####  ###.## %   ###.## %    ##.### %"; _
                            d; n ; found; expect; expect - found
Next


ReDim digit(1 To 9)
ReDim As UByte sieve(max_sieve)

'For d = 4 To max_sieve Step 2
'    sieve(d) = 1
'Next
Print : Print "start sieve"
For d = 3 To sqr(max_sieve)
    If sieve(d) = 0 Then
        For n = d * d To max_sieve Step d * 2
            sieve(n) = 1
        Next
    End If
Next

digit(2) = 1 ' 2

Print "start collecting first digits"
For n = 3 To max_sieve Step 2
    If sieve(n) = 0 Then
        d = Val(Left(Trim(Str(n)), 1))
        digit(d) = digit(d) +1
    End If
Next

Dim As ulong total
For n = 1 To 9
    total = total + digit(n)
Next

Print
Print "First";total; " primes"
Print "nr:     total     found   expected   difference"

For d = 1 To 9
    n = digit(d)
    found = n / total * 100
    expect = (Log(1 + 1 / d) / Log(10)) * 100
    Print Using " ##  ########  ###.## %   ###.## %    ###.### %"; _
                                d; n ; found; expect; expect - found
Next


' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
First 1000 Fibonacci numbers
nr:  total     found   expected  difference
  1    301   30.10 %    30.10 %     0.003 %
  2    177   17.70 %    17.61 %    -0.091 %
  3    125   12.50 %    12.49 %    -0.006 %
  4     96    9.60 %     9.69 %     0.091 %
  5     80    8.00 %     7.92 %    -0.082 %
  6     67    6.70 %     6.69 %    -0.005 %
  7     56    5.60 %     5.80 %     0.199 %
  8     53    5.30 %     5.12 %    -0.185 %
  9     45    4.50 %     4.58 %     0.076 %

start sieve
start collecting first digits

First1000000 primes
nr:     total     found   expected   difference
  1    415441   41.54 %    30.10 %    -11.441 %
  2     77025    7.70 %    17.61 %      9.907 %
  3     75290    7.53 %    12.49 %      4.965 %
  4     74114    7.41 %     9.69 %      2.280 %
  5     72951    7.30 %     7.92 %      0.623 %
  6     72257    7.23 %     6.69 %     -0.531 %
  7     71564    7.16 %     5.80 %     -1.357 %
  8     71038    7.10 %     5.12 %     -1.989 %
  9     70320    7.03 %     4.58 %     -2.456 %
```



## Go


```go
package main

import (
    "fmt"
    "math"
)

func Fib1000() []float64 {
    a, b, r := 0., 1., [1000]float64{}
    for i := range r {
        r[i], a, b = b, b, b+a
    }
    return r[:]
}

func main() {
    show(Fib1000(), "First 1000 Fibonacci numbers")
}

func show(c []float64, title string) {
    var f [9]int
    for _, v := range c {
        f[fmt.Sprintf("%g", v)[0]-'1']++
    }
    fmt.Println(title)
    fmt.Println("Digit  Observed  Predicted")
    for i, n := range f {
        fmt.Printf("  %d  %9.3f  %8.3f\n", i+1, float64(n)/float64(len(c)),
            math.Log10(1+1/float64(i+1)))
    }
}
```

{{Out}}

```txt

First 1000 Fibonacci numbers
Digit  Observed  Predicted
  1      0.301     0.301
  2      0.177     0.176
  3      0.125     0.125
  4      0.096     0.097
  5      0.080     0.079
  6      0.067     0.067
  7      0.056     0.058
  8      0.053     0.051
  9      0.045     0.046

```



## Groovy

'''Solution:'''

Uses [[Fibonacci_sequence#Analytic_8|Fibonacci sequence analytic formula]]
{{trans|Java}}

```groovy
def tallyFirstDigits = { size, generator ->
    def population = (0..<size).collect { generator(it) }
    def firstDigits = [0]*10
    population.each { number ->
        firstDigits[(number as String)[0] as int] ++
    }
    firstDigits
}
```


'''Test:'''

```groovy
def digitCounts = tallyFirstDigits(1000, aFib)
println "d    actual    predicted"
(1..<10).each {
    printf ("%d %10.6f %10.6f\n", it, digitCounts[it]/1000, Math.log10(1.0 + 1.0/it))
}
```


'''Output:'''

```txt
d    actual    predicted
1   0.301000   0.301030
2   0.177000   0.176091
3   0.125000   0.124939
4   0.095000   0.096910
5   0.080000   0.079181
6   0.067000   0.066947
7   0.056000   0.057992
8   0.053000   0.051153
9   0.045000   0.045757
```



## Haskell


```haskell
import qualified Data.Map as M
import Data.Char (digitToInt)

fstdigit :: Integer -> Int
fstdigit = digitToInt . head . show

n = 1000::Int
fibs = 1:1:zipWith (+) fibs (tail fibs)
fibdata = map fstdigit $ take n fibs
freqs = M.fromListWith (+) $ zip fibdata (repeat 1)

tab ::  [(Int, Double, Double)]
tab = [(d,
       (fromIntegral (M.findWithDefault 0 d freqs) /(fromIntegral n) ),
        logBase 10.0 $ 1 + 1/(fromIntegral d) ) | d<-[1..9]]

main = print tab
```

{{out}}

```txt
[(1,0.301,0.301029995663981),
(2,0.177,0.176091259055681),
(3,0.125,0.1249387366083),
(4,0.096,0.0969100130080564),
(5,0.08,0.0791812460476248),
(6,0.067,0.0669467896306132),
(7,0.056,0.0579919469776867),
(8,0.053,0.0511525224473813),
(9,0.045,0.0457574905606751)]

```


=={{header|Icon}} and {{header|Unicon}}==

The following solution works in both languages.


```unicon
global counts, total

procedure main()

   counts := table(0)
   total := 0.0
   every benlaw(fib(1 to 1000))

   every i := 1 to 9 do
      write(i,": ",right(100*counts[string(i)]/total,9)," ",100*P(i))

end

procedure benlaw(n)
   if counts[n ? (tab(upto('123456789')),move(1))] +:= 1 then total +:= 1
end

procedure P(d)
   return log(1+1.0/d, 10)
end

procedure fib(n)        # From Fibonacci Sequence task
    return fibMat(n)[1]
end

procedure fibMat(n)
    if n <= 0 then return [0,0]
    if n  = 1 then return [1,0]
    fp := fibMat(n/2)
    c := fp[1]*fp[1] + fp[2]*fp[2]
    d := fp[1]*(fp[1]+2*fp[2])
    if n%2 = 1 then return [c+d, d]
    else return [d, c]
end
```


Sample run:


```txt

->benlaw
1:      30.1 30.10299956639811
2:      17.7 17.60912590556812
3:      12.5 12.49387366082999
4:       9.6 9.69100130080564
5:       8.0 7.918124604762481
6:       6.7 6.694678963061322
7:       5.6 5.799194697768673
8:       5.3 5.115252244738128
9:       4.5 4.575749056067514
->

```



## J

We show the correlation coefficient of Benford's law with the leading digits of the first 1000 Fibonacci numbers is almost unity.

```J
log10 =: 10&^.
benford =: log10@:(1+%)
assert '0.30 0.18 0.12 0.10 0.08 0.07 0.06 0.05 0.05' -: 5j2 ": benford >: i. 9


append_next_fib =: , +/@:(_2&{.)
assert 5 8 13 -: append_next_fib 5 8

leading_digits =: {.@":&>
assert '581' -: leading_digits 5 8 13x

count =: #/.~ /: ~.
assert 2 1 3 4 -: count 'XCXBAXACXC'  NB. 2 A's, 1 B, 3 C's, and some X's.

normalize =: % +/
assert 1r3 2r3 -: normalize 1 2x

FIB =: append_next_fib ^: (1000-#) 1 1
LDF =: leading_digits FIB


TALLY_BY_KEY =: count LDF
assert 9 -: # TALLY_BY_KEY   NB. If all of [1-9] are present then we know what the digits are.

mean =: +/ % #
center=: - mean
mp =: $:~ :(+/ .*)
num =: mp&:center
den =: %:@:(*&:(+/@:(*:@:center)))
r =: num % den   NB. r is the LibreOffice correl function
assert '_0.982' -: 6j3 ": 1 2 3 r 6 5 3  NB. confirmed using LibreOffice correl function


assert '0.9999' -: 6j4 ": (normalize TALLY_BY_KEY) r benford >: i.9

assert '0.9999' -: 6j4 ": TALLY_BY_KEY r benford >: i.9  NB. Of course we don't need normalization
```



## Java


```Java
import java.math.BigInteger;
import java.util.Locale;

public class BenfordsLaw {

    private static BigInteger[] generateFibonacci(int n) {
        BigInteger[] fib = new BigInteger[n];
        fib[0] = BigInteger.ONE;
        fib[1] = BigInteger.ONE;
        for (int i = 2; i < fib.length; i++) {
            fib[i] = fib[i - 2].add(fib[i - 1]);
        }
        return fib;
    }

    public static void main(String[] args) {
        BigInteger[] numbers = generateFibonacci(1000);

        int[] firstDigits = new int[10];
        for (BigInteger number : numbers) {
            firstDigits[Integer.valueOf(number.toString().substring(0, 1))]++;
        }

        for (int i = 1; i < firstDigits.length; i++) {
            System.out.printf(Locale.ROOT, "%d %10.6f %10.6f%n",
                    i, (double) firstDigits[i] / numbers.length, Math.log10(1.0 + 1.0 / i));
        }
    }
}
```

The output is:

```txt
1   0.301000   0.301030
2   0.177000   0.176091
3   0.125000   0.124939
4   0.096000   0.096910
5   0.080000   0.079181
6   0.067000   0.066947
7   0.056000   0.057992
8   0.053000   0.051153
9   0.045000   0.045757
```

To use other number sequences, implement a suitable <tt>NumberGenerator</tt>, construct a <tt>Benford</tt> instance with it and print it.


## jq

{{works with|jq|1.4}}
This implementation shows the observed and expected number of occurrences together with the χ² statistic.

For the sake of being self-contained, the following includes a generator for Fibonacci numbers, and a prime number generator that is inefficient but brief and can generate numbers within an arbitrary range.
```jq
# Generate the first n Fibonacci numbers: 1, 1, ...
# Numerical accuracy is insufficient beyond about 1450.
def fibonacci(n):
  # input: [f(i-2), f(i-1), countdown]
  def fib: (.[0] + .[1]) as $sum
           | if .[2] <= 0 then empty
             elif .[2] == 1 then $sum
             else $sum, ([ .[1], $sum, .[2] - 1 ] | fib)
             end;
  [1, 0, n] | fib ;

# is_prime is tailored to work with jq 1.4
def is_prime:
  if . == 2 then true
  else 2 < . and . % 2 == 1 and
       . as $in
       | (($in + 1) | sqrt) as $m
       | (((($m - 1) / 2) | floor) + 1) as $max
       | reduce range(1; $max) as $i
           (true; if . then ($in % ((2 * $i) + 1)) > 0 else false end)
  end ;

# primes in [m,n)
def primes(m;n):
  range(m;n) | select(is_prime);

def runs:
  reduce .[] as $item
    ( [];
      if . == [] then [ [ $item, 1] ]
      else  .[length-1] as $last
            | if $last[0] == $item
              then (.[0:length-1] + [ [$item, $last[1] + 1] ] )
              else . + [[$item, 1]]
              end
      end ) ;

# Inefficient but brief:
def histogram: sort | runs;

def benford_probability:
  tonumber
  | if . > 0 then ((1 + (1 /.)) | log) / (10|log)
    else 0
    end ;

# benford takes a stream and produces an array of [ "d", observed, expected ]
def benford(stream):
  [stream | tostring | .[0:1] ] | histogram as $histogram
  | reduce ($histogram | .[] | .[0]) as $digit
      ([]; . + [$digit, ($digit|benford_probability)] )
  | map(select(type == "number")) as $probabilities
  | ([ $histogram | .[] | .[1] ] | add) as $total
  | reduce range(0; $histogram|length) as $i
      ([]; . + ([$histogram[$i] + [$total * $probabilities[$i]] ] ) ) ;

# given an array of [value, observed, expected] values,
# produce the χ² statistic
def chiSquared:
  reduce .[] as $triple
    (0;
     if $triple[2] == 0 then .
     else . + ($triple[1] as $o | $triple[2] as $e | ($o - $e) | (.*.)/$e)
     end) ;

# truncate n places after the decimal point;
# return a string since it can readily be converted back to a number
def precision(n):
  tostring as $s | $s | index(".")
  | if . then $s[0:.+n+1] else $s end ;

# Right-justify but do not truncate
def rjustify(n):
  length as $length | if n <= $length then . else " " * (n-$length) + . end;

# Attempt to align decimals so integer part is in a field of width n
def align(n):
  index(".") as $ix
  | if n < $ix then .
    elif $ix then (.[0:$ix]|rjustify(n)) +.[$ix:]
    else rjustify(n)
    end ;

# given an array of [value, observed, expected] values,
# produce rows of the form: value observed expected
def print_rows(prec):
  .[] | map( precision(prec)|align(5) + "  ") | add ;

def report(heading; stream):
    benford(stream) as $array
    | heading,
      " Digit Observed Expected",
      ( $array | print_rows(2) ),
      "",
      " χ² = \( $array | chiSquared | precision(4))",
      ""
;

def task:
  report("First 100 fibonacci numbers:"; fibonacci( 100) ),
  report("First 1000 fibonacci numbers:"; fibonacci(1000) ),
  report("Primes less than 1000:"; primes(2;1000)),
  report("Primes between 1000 and 10000:"; primes(1000;10000)),
  report("Primes less than 100000:"; primes(2;100000))
;

task
```

{{out}}

```txt
First 100 fibonacci numbers:
 Digit Observed Expected
    1     30     30.10
    2     18     17.60
    3     13     12.49
    4      9      9.69
    5      8      7.91
    6      6      6.69
    7      5      5.79
    8      7      5.11
    9      4      4.57

 χ² = 1.0287

First 1000 fibonacci numbers:
 Digit Observed Expected
    1    301    301.02
    2    177    176.09
    3    125    124.93
    4     96     96.91
    5     80     79.18
    6     67     66.94
    7     56     57.99
    8     53     51.15
    9     45     45.75

 χ² = 0.1694

Primes less than 1000:
 Digit Observed Expected
    1     25     50.57
    2     19     29.58
    3     19     20.98
    4     20     16.28
    5     17     13.30
    6     18     11.24
    7     18      9.74
    8     17      8.59
    9     15      7.68

 χ² = 45.0162

Primes between 1000 and 10000:
 Digit Observed Expected
    1    135    319.39
    2    127    186.83
    3    120    132.55
    4    119    102.82
    5    114     84.01
    6    117     71.03
    7    107     61.52
    8    110     54.27
    9    112     48.54

 χ² = 343.5583

Primes less than 100000:
 Digit Observed Expected
    1   1193   2887.47
    2   1129   1689.06
    3   1097   1198.41
    4   1069    929.56
    5   1055    759.50
    6   1013    642.15
    7   1027    556.25
    8   1003    490.65
    9   1006    438.90

 χ² = 3204.8072
```


## Julia



```Julia
fib(n) = ([one(n) one(n) ; one(n) zero(n)]^n)[1,2]

ben(l) = [count(x->x==i, map(n->string(n)[1],l)) for i='1':'9']./length(l)

benford(l) = [Number[1:9;] ben(l) log10(1.+1./[1:9;])]
```

{{Out}}

```txt
julia> benford([fib(big(n)) for n = 1:1000])
9x3 Array{Number,2}:
 1  0.301  0.30103
 2  0.177  0.176091
 3  0.125  0.124939
 4  0.096  0.09691
 5  0.08   0.0791812
 6  0.067  0.0669468
 7  0.056  0.0579919
 8  0.053  0.0511525
 9  0.045  0.0457575

```



## Kotlin


```scala
import java.math.BigInteger

interface NumberGenerator {
    val numbers: Array<BigInteger>
}

class Benford(ng: NumberGenerator) {
    override fun toString() = str

    private val firstDigits = IntArray(9)
    private val count = ng.numbers.size.toDouble()
    private val str: String

    init {
        for (n in ng.numbers) {
            firstDigits[n.toString().substring(0, 1).toInt() - 1]++
        }

        str = with(StringBuilder()) {
            for (i in firstDigits.indices) {
                append(i + 1).append('\t').append(firstDigits[i] / count)
                append('\t').append(Math.log10(1 + 1.0 / (i + 1))).append('\n')
            }

            toString()
        }
    }
}

object FibonacciGenerator : NumberGenerator {
    override val numbers: Array<BigInteger> by lazy {
        val fib = Array<BigInteger>(1000, { BigInteger.ONE })
        for (i in 2 until fib.size)
            fib[i] = fib[i - 2].add(fib[i - 1])
        fib
    }
}

fun main(a: Array<String>) = println(Benford(FibonacciGenerator))
```



## Liberty BASIC

Using function from
http://rosettacode.org/wiki/Fibonacci_sequence#Liberty_BASIC

```lb

dim bin(9)

N=1000
for i = 0 to N-1
    num$ = str$(fiboI(i))
    d=val(left$(num$,1))
    'print num$, d
    bin(d)=bin(d)+1
next
print

print "Digit", "Actual freq", "Expected freq"
for i = 1 to 9
    print i, bin(i)/N, using("#.###", P(i))
next


function P(d)
    P = log10(d+1)-log10(d)
end function

function log10(x)
    log10 = log(x)/log(10)
end function

function fiboI(n)
    a = 0
    b = 1
    for i = 1 to n
        temp = a + b
        a = b
        b = temp
    next i
    fiboI = a
end function

```


{{out}}

```txt

Digit         Actual freq   Expected freq
1             0.301         0.301
2             0.177         0.176
3             0.125         0.125
4             0.095         0.097
5             0.08          0.079
6             0.067         0.067
7             0.056         0.058
8             0.053         0.051
9             0.045         0.046

```



## Lua


```lua
actual = {}
expected = {}
for i = 1, 9 do
    actual[i] = 0
    expected[i] = math.log10(1 + 1 / i)
end

n = 0
file = io.open("fibs1000.txt", "r")
for line in file:lines() do
    digit = string.byte(line, 1) - 48
    actual[digit] = actual[digit] + 1
    n = n + 1
end
file:close()

print("digit   actual  expected")
for i = 1, 9 do
    print(i, actual[i] / n, expected[i])
end
```

{{out}}

```txt
digit   actual  expected
1       0.301   0.30102999566398
2       0.177   0.17609125905568
3       0.125   0.1249387366083
4       0.096   0.096910013008056
5       0.08    0.079181246047625
6       0.067   0.066946789630613
7       0.056   0.057991946977687
8       0.053   0.051152522447381
9       0.045   0.045757490560675
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
fibdata = Array[First@IntegerDigits@Fibonacci@# &, 1000];
Table[{d, N@Count[fibdata, d]/Length@fibdata, Log10[1. + 1/d]}, {d, 1,
    9}] // Grid
```

{{out}}

```txt
1	0.301	0.30103
2	0.177	0.176091
3	0.125	0.124939
4	0.096	0.09691
5	0.08	0.0791812
6	0.067	0.0669468
7	0.056	0.0579919
8	0.053	0.0511525
9	0.045	0.0457575
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method brenfordDeveation(nlist = Rexx[]) public static
  observed = 0
  loop n_ over nlist
    d1 = n_.left(1)
    if d1 = 0 then iterate n_
    observed[d1] = observed[d1] + 1
    end n_
  say ' '.right(4) 'Observed'.right(11) 'Expected'.right(11) 'Deviation'.right(11)
  loop n_ = 1 to 9
    actual = (observed[n_] / (nlist.length - 1))
    expect = Rexx(Math.log10(1 + 1 / n_))
    deviat = expect - actual
    say n_.right(3)':' (actual * 100).format(3, 6)'%' (expect * 100).format(3, 6)'%' (deviat * 100).abs().format(3, 6)'%'
    end n_
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method fibonacciList(size = 1000) public static returns Rexx[]
  fibs = Rexx[size + 1]
  fibs[0] = 0
  fibs[1] = 1
  loop n_ = 2 to size
    fibs[n_] = fibs[n_ - 1] + fibs[n_ - 2]
    end n_
  return fibs

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg n_ .
  if n_ = '' then n_ = 1000
  fibList = fibonacciList(n_)
  say 'Fibonacci sequence to' n_
  brenfordDeveation(fibList)
  return

```

{{out}}

```txt

Fibonacci sequence to 1000
        Observed    Expected   Deviation
  1:  30.100000%  30.103000%   0.003000%
  2:  17.700000%  17.609126%   0.090874%
  3:  12.500000%  12.493874%   0.006127%
  4:   9.600000%   9.691001%   0.091001%
  5:   8.000000%   7.918125%   0.081875%
  6:   6.700000%   6.694679%   0.005321%
  7:   5.600000%   5.799195%   0.199195%
  8:   5.300000%   5.115252%   0.184748%
  9:   4.500000%   4.575749%   0.075749%

```


=={{header|Oberon-2}}==
{{Works with|oo2c version 2}}

```oberon2

MODULE BenfordLaw;
IMPORT
  LRealStr,
  LRealMath,
  Out := NPCT:Console;

VAR
  r: ARRAY 1000 OF LONGREAL;
  d: ARRAY 10 OF LONGINT;
  a: LONGREAL;
  i: LONGINT;

PROCEDURE Fibb(VAR r: ARRAY OF LONGREAL);
VAR
  i: LONGINT;
BEGIN
  r[0] := 1.0;r[1] := 1.0;
  FOR i := 2 TO LEN(r) - 1 DO
    r[i] := r[i - 2] + r[i - 1]
  END
END Fibb;

PROCEDURE Dist(r [NO_COPY]: ARRAY OF LONGREAL; VAR d: ARRAY OF LONGINT);
VAR
  i: LONGINT;
  str: ARRAY 256 OF CHAR;
BEGIN
  FOR i := 0 TO LEN(r) - 1 DO
    LRealStr.RealToStr(r[i],str);
    INC(d[ORD(str[0]) - ORD('0')])
  END
END Dist;

BEGIN
  Fibb(r);
  Dist(r,d);
  Out.String("First 1000 fibonacci numbers: ");Out.Ln;
  Out.String(" digit ");Out.String(" observed ");Out.String(" predicted ");Out.Ln;
  FOR i := 1 TO LEN(d) - 1 DO
    a := LRealMath.ln(1.0 + 1.0 / i ) / LRealMath.ln(10);
    Out.Int(i,5);Out.LongRealFix(d[i] / 1000.0,9,3);Out.LongRealFix(a,10,3);Out.Ln
  END
END BenfordLaw.

```

{{out}}

```txt

First 1000 fibonacci numbers:
 digit  observed  predicted
    1    0.301     0.301
    2    0.177     0.176
    3    0.125     0.125
    4    0.096     0.097
    5    0.080     0.079
    6    0.067     0.067
    7    0.056     0.058
    8    0.053     0.051
    9    0.045     0.046

```



## OCaml

For the Fibonacci sequence, we use the function from
https://rosettacode.org/wiki/Fibonacci_sequence#Arbitrary_Precision

Note the remark about the compilation of the program there.

```ocaml

open Num

let fib =
  let rec fib_aux f0 f1 = function
    | 0 -> f0
    | 1 -> f1
    | n -> fib_aux f1 (f1 +/ f0) (n - 1)
  in
  fib_aux (num_of_int 0) (num_of_int 1) ;;

let create_fibo_string = function n -> string_of_num (fib n) ;;
let rec range i j = if i > j then [] else i :: (range (i + 1) j)

let n_max = 1000 ;;

let numbers = range 1 n_max in
  let get_first_digit = function s -> Char.escaped (String.get s 0) in
    let first_digits = List.map get_first_digit (List.map create_fibo_string numbers) in
  let data = Array.create 9 0 in
    let fill_data vec = function n -> vec.(n - 1) <- vec.(n - 1) + 1 in
    List.iter (fill_data data) (List.map int_of_string first_digits) ;
    Printf.printf "\nFrequency of the first digits in the Fibonacci sequence:\n" ;
    Array.iter (Printf.printf "%f ")
      (Array.map (fun x -> (float x) /. float (n_max)) data) ;

let xvalues = range 1 9 in
  let benfords_law = function x -> log10 (1.0 +. 1.0 /. float (x)) in
    Printf.printf "\nPrediction of Benford's law:\n " ;
    List.iter (Printf.printf "%f ") (List.map benfords_law xvalues) ;
    Printf.printf "\n" ;;

```

{{out}}

```txt

Frequency of the first digits in the Fibonacci sequence:
0.301000 0.177000 0.125000 0.096000 0.080000 0.067000 0.056000 0.053000 0.045000
Prediction of Benford's law:
 0.301030 0.176091 0.124939 0.096910 0.079181 0.066947 0.057992 0.051153 0.045757

```



## PARI/GP


```parigp
distribution(v)={
	my(t=vector(9,n,sum(i=1,#v,v[i]==n)));
	print("Digit\tActual\tExpected");
	for(i=1,9,print(i, "\t", t[i], "\t", round(#v*(log(i+1)-log(i))/log(10))))
};
dist(f)=distribution(vector(1000,n,digits(f(n))[1]));
lucas(n)=fibonacci(n-1)+fibonacci(n+1);
dist(fibonacci)
dist(lucas)
```

{{out}}

```txt
Digit   Actual  Expected
1       301     301
2       177     176
3       125     125
4       96      97
5       80      79
6       67      67
7       56      58
8       53      51
9       45      46

Digit   Actual  Expected
1       301     301
2       174     176
3       127     125
4       97      97
5       79      79
6       66      67
7       59      58
8       51      51
9       46      46
```


## Pascal


```pascal
program fibFirstdigit;
{$IFDEF FPC}{$MODE Delphi}{$ELSE}{$APPTYPE CONSOLE}{$ENDIF}
uses
  sysutils;
type
  tDigitCount = array[0..9] of LongInt;
var
  s: Ansistring;
  dgtCnt,
  expectedCnt : tDigitCount;

procedure GetFirstDigitFibonacci(var dgtCnt:tDigitCount;n:LongInt=1000);
//summing up only the first 9 digits
//n = 1000 -> difference to first 9 digits complete fib < 100 == 2 digits
var
  a,b,c : LongWord;//about 9.6 decimals
Begin
  for a in dgtCnt do dgtCnt[a] := 0;
  a := 0;b := 1;
  while n > 0 do
  Begin
    c := a+b;
    //overflow? round and divide by base 10
    IF c < a then
      Begin a := (a+5) div 10;b := (b+5) div 10;c := a+b;end;
    a := b;b := c;
    s := IntToStr(a);inc(dgtCnt[Ord(s[1])-Ord('0')]);
    dec(n);
  end;
end;

procedure InitExpected(var dgtCnt:tDigitCount;n:LongInt=1000);
var
  i: integer;
begin
  for i := 1 to 9  do
    dgtCnt[i] := trunc(n*ln(1 + 1 / i)/ln(10));
end;

var
  reldiff: double;
  i,cnt: integer;
begin
  cnt := 1000;
  InitExpected(expectedCnt,cnt);
  GetFirstDigitFibonacci(dgtCnt,cnt);
  writeln('Digit  count  expected  rel diff');
  For i := 1 to 9 do
  Begin
    reldiff := 100*(expectedCnt[i]-dgtCnt[i])/expectedCnt[i];
    writeln(i:5,dgtCnt[i]:7,expectedCnt[i]:10,reldiff:10:5,' %');
  end;
end.
```



```txt
Digit  Count  Expected  rel Diff
    1    301       301   0.00000 %
    2    177       176  -0.56818 %
    3    125       124  -0.80645 %
    4     96        96   0.00000 %
    5     80        79  -1.26582 %
    6     67        66  -1.51515 %
    7     56        57   1.75439 %
    8     53        51  -3.92157 %
    9     45        45   0.00000 %
```



## Perl


```Perl
#!/usr/bin/perl
use strict ;
use warnings ;
use POSIX qw( log10 ) ;

my @fibonacci = ( 0 , 1  ) ;
while ( @fibonacci != 1000 ) {
   push @fibonacci , $fibonacci[ -1 ] + $fibonacci[ -2 ] ;
}
my @actuals ;
my @expected ;
for my $i( 1..9 ) {
   my $sum = 0 ;
   map { $sum++ if $_ =~ /\A$i/ } @fibonacci ;
   push @actuals , $sum / 1000  ;
   push @expected , log10( 1 + 1/$i ) ;
}
print "         Observed         Expected\n" ;
for my $i( 1..9 ) {
   print "$i : " ;
   my $result = sprintf ( "%.2f" , 100 * $actuals[ $i - 1 ] ) ;
   printf "%11s %%" , $result ;
   $result = sprintf ( "%.2f" , 100 * $expected[ $i - 1 ] ) ;
   printf "%15s %%\n" , $result ;
}
```

{{Out}}

```txt

         Observed         Expected
1 :       30.10 %          30.10 %
2 :       17.70 %          17.61 %
3 :       12.50 %          12.49 %
4 :        9.50 %           9.69 %
5 :        8.00 %           7.92 %
6 :        6.70 %           6.69 %
7 :        5.60 %           5.80 %
8 :        5.30 %           5.12 %
9 :        4.50 %           4.58 %

```



## Perl 6

{{Works with|rakudo|2016-10-24}}

```perl6
sub benford(@a) { bag +« @a».substr(0,1) }

sub show(%distribution) {
    printf "%9s %9s  %s\n", <Actual Expected Deviation>;
    for 1 .. 9 -> $digit {
        my $actual = %distribution{$digit} * 100 / [+] %distribution.values;
        my $expected = (1 + 1 / $digit).log(10) * 100;
        printf "%d: %5.2f%% | %5.2f%% | %.2f%%\n",
          $digit, $actual, $expected, abs($expected - $actual);
    }
}

multi MAIN($file) { show benford $file.IO.lines }
multi MAIN() { show benford ( 1, 1, 2, *+* ... * )[^1000] }
```


'''Output:''' First 1000 Fibonaccis

```txt
   Actual  Expected  Deviation
1: 30.10% | 30.10% | 0.00%
2: 17.70% | 17.61% | 0.09%
3: 12.50% | 12.49% | 0.01%
4:  9.60% |  9.69% | 0.09%
5:  8.00% |  7.92% | 0.08%
6:  6.70% |  6.69% | 0.01%
7:  5.60% |  5.80% | 0.20%
8:  5.30% |  5.12% | 0.18%
9:  4.50% |  4.58% | 0.08%
```


'''Extra credit:''' Square Kilometers of land under cultivation, by country / territory. First column from Wikipedia: [[wp:Land_use_statistics_by_country|Land use statistics by country]].

```txt
   Actual  Expected  Deviation
1: 33.33% | 30.10% | 3.23%
2: 18.31% | 17.61% | 0.70%
3: 13.15% | 12.49% | 0.65%
4:  8.45% |  9.69% | 1.24%
5:  9.39% |  7.92% | 1.47%
6:  5.63% |  6.69% | 1.06%
7:  4.69% |  5.80% | 1.10%
8:  5.16% |  5.12% | 0.05%
9:  1.88% |  4.58% | 2.70%
```



## Phix

{{trans|Go}}

```Phix
procedure main(sequence s, string title)
sequence f = repeat(0,9)
    for i=1 to length(s) do
        f[sprint(s[i])[1]-'0'] += 1
    end for
    puts(1,title)
    puts(1,"Digit  Observed%  Predicted%\n")
    for i=1 to length(f) do
        printf(1,"  %d  %9.3f  %8.3f\n", {i, f[i]/length(s)*100, log10(1+1/i)*100})
    end for
end procedure
main(fib(1000),"First 1000 Fibonacci numbers\n")
main(primes(10000),"First 10000 Prime numbers\n")
main(threes(500),"First 500 powers of three\n")
```

Supporting staff:

```Phix
function fib(integer lim)
atom a=0, b=1
sequence res = repeat(0,lim)
    for i=1 to lim do
        {res[i], a, b} = {b, b, b+a}
    end for
    return res
end function

function primes(integer lim)
integer n = 1, k, p
sequence res = {2}
    while length(res)<lim do
        k = 3
        p = 1
        n += 2
        while k*k<=n and p do
            p = floor(n/k)*k!=n
            k += 2
        end while
        if p then
            res = append(res,n)
        end if
    end while
    return res
end function

function threes(integer lim)
sequence res = repeat(0,lim)
    for i=1 to lim do
        res[i] = power(3,i)
    end for
    return res
end function

constant INVLN10 = 0.43429_44819_03251_82765
function log10(object x1)
    return log(x1) * INVLN10
end function
```

{{out}} (put into columns by hand)

```txt

First 1000 Fibonacci numbers            First 10000 Prime numbers               First 500 powers of three
Digit  Observed%  Predicted%            Digit  Observed%  Predicted%            Digit  Observed%  Predicted%
  1     30.100    30.103                  1     16.010    30.103                  1     30.000    30.103
  2     17.700    17.609                  2     11.290    17.609                  2     17.600    17.609
  3     12.500    12.494                  3     10.970    12.494                  3     12.400    12.494
  4      9.600     9.691                  4     10.690     9.691                  4      9.800     9.691
  5      8.000     7.918                  5     10.550     7.918                  5      8.000     7.918
  6      6.700     6.695                  6     10.130     6.695                  6      6.600     6.695
  7      5.600     5.799                  7     10.270     5.799                  7      5.800     5.799
  8      5.300     5.115                  8     10.030     5.115                  8      5.200     5.115
  9      4.500     4.576                  9     10.060     4.576                  9      4.600     4.576

```



## PL/I


```PL/I

(fofl, size, subrg):
Benford: procedure options(main);                /* 20 October 2013 */
   declare sc(1000) char(1), f(1000) float (16);
   declare d fixed (1);

   call Fibonacci(f);
   call digits(sc, f);

   put skip list ('digit  expected     obtained');
   do d= 1 upthru 9;
      put skip edit (d, log10(1 + 1/d), tally(sc, trim(d))/1000)
         (f(3), 2 f(13,8) );
   end;

Fibonacci: procedure (f);
   declare f(*) float (16);
   declare i fixed binary;

   f(1), f(2) = 1;
   do i = 3 to 1000;
      f(i) = f(i-1) + f(i-2);
   end;
end Fibonacci;

digits: procedure (sc, f);
   declare sc(*) char(1), f(*) float (16);
   sc = substr(trim(f), 1, 1);
end digits;

tally: procedure (sc, d) returns (fixed binary);
   declare sc(*) char(1), d char(1);
   declare (i, t) fixed binary;
   t = 0;
   do i = 1 to 1000;
      if sc(i) = d then t = t + 1;
   end;
   return (t);
end tally;
end Benford;

```

Results:

```txt

digit  expected     obtained
  1   0.30103000   0.30099487
  2   0.17609126   0.17698669
  3   0.12493874   0.12500000
  4   0.09691001   0.09599304
  5   0.07918125   0.07998657
  6   0.06694679   0.06698608
  7   0.05799195   0.05599976
  8   0.05115252   0.05299377
  9   0.04575749   0.04499817

```



## PL/pgSQL


```SQL

WITH recursive
constant(val) AS
(
select 1000.
)
,
fib(a,b) AS
(
SELECT CAST(0 AS numeric), CAST(1 AS numeric)
UNION ALL
SELECT b,a+b
FROM fib
)
,
benford(first_digit, probability_real, probability_theoretical) AS
(
SELECT *,
	CAST(log(1. + 1./CAST(first_digit AS INT)) AS NUMERIC(5,4)) probability_theoretical
FROM (
	SELECT  first_digit, CAST(COUNT(1)/(select val from constant) AS NUMERIC(5,4)) probability_real FROM
	(
		SELECT SUBSTRING(CAST(a AS VARCHAR(100)),1,1) first_digit
		FROM fib
		WHERE SUBSTRING(CAST(a AS VARCHAR(100)),1,1) <> '0'
		LIMIT (select val from constant)
	) t
	GROUP BY first_digit
) f
ORDER BY first_digit ASC
)
select *
from benford cross join
     (select cast(corr(probability_theoretical,probability_real) as numeric(5,4)) correlation
      from benford) c

```



## PowerShell

The sample file was not found. I selected another that contained the first two-thousand in the Fibonacci sequence, so there is a small amount of extra filtering.

```PowerShell

$url  = "https://oeis.org/A000045/b000045.txt"
$file = "$env:TEMP\FibonacciNumbers.txt"
(New-Object System.Net.WebClient).DownloadFile($url, $file)

$benford = Get-Content -Path $file |
         Select-Object -Skip 1 -First 1000 |
         ForEach-Object {(($_ -split " ")[1].ToString().ToCharArray())[0]} |
         Group-Object |
         Select-Object -Property @{Name="Digit"   ; Expression={[int]($_.Name)}},
                                 Count,
                                 @{Name="Actual"  ; Expression={$_.Count/1000}},
                                 @{Name="Expected"; Expression={[double]("{0:f5}" -f [Math]::Log10(1 + 1 / $_.Name))}}

$benford | Sort-Object -Property Digit | Format-Table -AutoSize

Remove-Item -Path $file -Force -ErrorAction SilentlyContinue

```

{{Out}}

```txt

Digit Count Actual Expected
----- ----- ------ --------
    1   301  0.301  0.30103
    2   177  0.177  0.17609
    3   125  0.125  0.12494
    4    96  0.096  0.09691
    5    80   0.08  0.07918
    6    67  0.067  0.06695
    7    56  0.056  0.05799
    8    53  0.053  0.05115
    9    45  0.045  0.04576

```



## Prolog

{{works with|SWI Prolog|6.2.6 by Jan Wielemaker, University of Amsterdam}}
Note: SWI Prolog implements arbitrary precision integer arithmetic through use of the GNU MP library

```Prolog
%_________________________________________________________________
% Does the Fibonacci sequence follow Benford's law?
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Fibonacci sequence generator
fib(C, [P,S], C, N)  :- N is P + S.
fib(C, [P,S], Cv, V) :- succ(C, Cn), N is P + S, !, fib(Cn, [S,N], Cv, V).

fib(0, 0).
fib(1, 1).
fib(C, N) :- fib(2, [0,1], C, N). % Generate from 3rd sequence on

% The benford law calculated
benford(D, Val) :- Val is log10(1+1/D).

% Retrieves the first characters of the first 1000 fibonacci numbers
%        (excluding zero)
firstchar(V) :-
	fib(C,N), N =\= 0, atom_chars(N, [Ch|_]), number_chars(V, [Ch]),
	(C>999-> !; true).

% Increment the n'th list item (1 based), result -> third argument.
incNth(1, [Dh|Dt], [Ch|Dt]) :- !, succ(Dh, Ch).
incNth(H, [Dh|Dt], [Dh|Ct]) :- succ(Hn, H), !, incNth(Hn, Dt, Ct).

% Calculate the frequency of the all the list items
freq([], D, D).
freq([H|T], D, C) :- incNth(H, D, L), !, freq(T, L, C).

freq([H|T], Freq) :-
	length([H|T], Len), min_list([H|T], Min), max_list([H|T], Max),
	findall(0, between(Min,Max,_), In),
	freq([H|T], In, F),	  % Frequency stored in F
	findall(N, (member(V, F), N is V/Len), Freq). % Normalise F->Freq

% Output the results
writeHdr :-
	format('~t~w~15| - ~t~w\n', ['Benford', 'Measured']).
writeData(Benford, Freq) :-
	format('~t~2f%~15| - ~t~2f%\n', [Benford*100, Freq*100]).

go :- % main goal
	findall(B, (between(1,9,N), benford(N,B)), Benford),
	findall(C, firstchar(C), Fc), freq(Fc, Freq),
	writeHdr, maplist(writeData, Benford, Freq).
```

{{out}}

```txt
?- go.
        Benford - Measured
         30.10% - 30.10%
         17.61% - 17.70%
         12.49% - 12.50%
          9.69% - 9.60%
          7.92% - 8.00%
          6.69% - 6.70%
          5.80% - 5.60%
          5.12% - 5.30%
          4.58% - 4.50%
```



## PureBasic


```purebasic
#MAX_N=1000
NewMap d1.i()
Dim fi.s(#MAX_N)
fi(0)="0" : fi(1)="1"
Declare.s Sigma(sx.s,sy.s)

For I=2 To #MAX_N
  fi(I)=Sigma(fi(I-2),fi(I-1))
Next

For I=1 To #MAX_N
  d1(Left(fi(I),1))+1
Next

Procedure.s Sigma(sx.s, sy.s)
  Define i.i, v1.i, v2.i, r.i
  Define s.s, sa.s
  sy=ReverseString(sy) : s=ReverseString(sx)
  For i=1 To Len(s)*Bool(Len(s)>Len(sy))+Len(sy)*Bool(Len(sy)>=Len(s))
    v1=Val(Mid(s,i,1))
    v2=Val(Mid(sy,i,1))
    r+v1+v2
    sa+Str(r%10)
    r/10
  Next i
  If r : sa+Str(r%10) : EndIf
  ProcedureReturn ReverseString(sa)
EndProcedure

OpenConsole("Benford's law: Fibonacci sequence 1.."+Str(#MAX_N))

Print(~"Dig.\t\tCnt."+~"\t\tExp.\t\tDif.\n\n")
ForEach d1()
  Print(RSet(MapKey(d1()),4," ")+~"\t:\t"+RSet(Str(d1()),3," ")+~"\t\t")
  ex=Int(#MAX_N*Log(1+1/Val(MapKey(d1())))/Log(10))
  PrintN(RSet(Str(ex),3," ")+~"\t\t"+RSet(StrF((ex-d1())*100/ex,5),8," ")+" %")
Next

PrintN(~"\nPress Enter...")
Input()
```

{{out}}

```txt
Dig.            Cnt.            Exp.            Dif.

   1    :       301             301              0.00000 %
   2    :       177             176             -0.56818 %
   3    :       125             124             -0.80645 %
   4    :        96              96              0.00000 %
   5    :        80              79             -1.26582 %
   6    :        67              66             -1.51515 %
   7    :        56              57              1.75439 %
   8    :        53              51             -3.92157 %
   9    :        45              45              0.00000 %

Press Enter...
```



## Python

Works with Python 3.X & 2.7

```python
from __future__ import division
from itertools import islice, count
from collections import Counter
from math import log10
from random import randint

expected = [log10(1+1/d) for d in range(1,10)]

def fib():
    a,b = 1,1
    while True:
        yield a
        a,b = b,a+b

# powers of 3 as a test sequence
def power_of_threes():
    return (3**k for k in count(0))

def heads(s):
    for a in s: yield int(str(a)[0])

def show_dist(title, s):
    c = Counter(s)
    size = sum(c.values())
    res = [c[d]/size for d in range(1,10)]

    print("\n%s Benfords deviation" % title)
    for r, e in zip(res, expected):
        print("%5.1f%% %5.1f%%  %5.1f%%" % (r*100., e*100., abs(r - e)*100.))

def rand1000():
    while True: yield randint(1,9999)

if __name__ == '__main__':
    show_dist("fibbed", islice(heads(fib()), 1000))
    show_dist("threes", islice(heads(power_of_threes()), 1000))

    # just to show that not all kind-of-random sets behave like that
    show_dist("random", islice(heads(rand1000()), 10000))
```

{{out}}

```txt
fibbed Benfords deviation
 30.1%  30.1%    0.0%
 17.7%  17.6%    0.1%
 12.5%  12.5%    0.0%
  9.6%   9.7%    0.1%
  8.0%   7.9%    0.1%
  6.7%   6.7%    0.0%
  5.6%   5.8%    0.2%
  5.3%   5.1%    0.2%
  4.5%   4.6%    0.1%

threes Benfords deviation
 30.0%  30.1%    0.1%
 17.7%  17.6%    0.1%
 12.3%  12.5%    0.2%
  9.8%   9.7%    0.1%
  7.9%   7.9%    0.0%
  6.6%   6.7%    0.1%
  5.9%   5.8%    0.1%
  5.2%   5.1%    0.1%
  4.6%   4.6%    0.0%

random Benfords deviation
 11.2%  30.1%   18.9%
 10.9%  17.6%    6.7%
 11.6%  12.5%    0.9%
 11.1%   9.7%    1.4%
 11.6%   7.9%    3.7%
 11.4%   6.7%    4.7%
 10.3%   5.8%    4.5%
 11.0%   5.1%    5.9%
 10.9%   4.6%    6.3%
```



## R


```R

pbenford <- function(d){
  return(log10(1+(1/d)))
}

get_lead_digit <- function(number){
  return(as.numeric(substr(number,1,1)))
}

fib_iter <- function(n){
  first <- 1
  second <- 0
  for(i in 1:n){
    sum <- first + second
    first <- second
    second <- sum
  }
  return(sum)
}

fib_sequence <- mapply(fib_iter,c(1:1000))
lead_digits <- mapply(get_lead_digit,fib_sequence)

observed_frequencies <- table(lead_digits)/1000
expected_frequencies <- mapply(pbenford,c(1:9))

data <- data.frame(observed_frequencies,expected_frequencies)
colnames(data) <- c("digit","obs.frequency","exp.frequency")
dev_percentage <- abs((data$obs.frequency-data$exp.frequency)*100)
data <- data.frame(data,dev_percentage)

print(data)

```

{{out}}
 digit obs.frequency exp.frequency dev_percentage
     1         0.301       0.30103       0.003000
     2         0.177       0.17609       0.090874
     3         0.125       0.12494       0.006126
     4         0.096       0.09691       0.091001
     5         0.080       0.07918       0.081875
     6         0.067       0.06695       0.005321
     7         0.056       0.05799       0.199195
     8         0.053       0.05115       0.184748
     9         0.045       0.04576       0.075749


## Racket


```Racket
#lang racket

(define (log10 n) (/ (log n) (log 10)))

(define (first-digit n)
  (quotient n (expt 10 (inexact->exact (floor (log10 n))))))

(define N 10000)

(define fibs
  (let loop ([n N] [a 0] [b 1])
    (if (zero? n) '() (cons b (loop (sub1 n) b (+ a b))))))

(define v (make-vector 10 0))
(for ([n fibs])
  (define f (first-digit n))
  (vector-set! v f (add1 (vector-ref v f))))

(printf "N   OBS   EXP\n")
(define (pct n) (~r (* n 100.0) #:precision 1 #:min-width 4))
(for ([i (in-range 1 10)])
  (printf "~a: ~a% ~a%\n" i
          (pct (/ (vector-ref v i) N))
          (pct (log10 (+ 1 (/ i))))))

;; Output:
;; N   OBS   EXP
;; 1: 30.1% 30.1%
;; 2: 17.6% 17.6%
;; 3: 12.5% 12.5%
;; 4:  9.7%  9.7%
;; 5:  7.9%  7.9%
;; 6:  6.7%  6.7%
;; 7:  5.8%  5.8%
;; 8:  5.1%  5.1%
;; 9:  4.6%  4.6%
```



## REXX

The REXX language practically hasn't any high math functions, so the   '''e''',   '''ln''',   and '''log'''   functions were included herein.

For the extra credit stuff, I choose to generate the Fibonacci and factorials rather than find a
web-page with them listed,   as each list is very easy to generate.

```rexx
/*REXX program  demonstrates  some common functions  (thirty decimal digits are shown). */
numeric digits length( e() )  -  length(.)       /*use the width of  (e)  for LN & LOG. */
parse arg N .;  if N=='' | N==","  then N= 1000  /*allow sample size to be specified.   */
LN10= ln(10)                                     /*calculate the natural log of ten #'s.*/
w1= max(2 + length('observed'), length(N-2) )    /*for aligning output for a number.    */
w2= max(2 + length('expected'), length(N  ) )    /* "      "    frequency distributions.*/
pad= "   "                                       /*W1, W2: # digs past the decimal point*/
           do j=1  for 9;   #.j=pad  center( format( log( 1 + 1/j ), , length(N) + 2), w2)
           end   /*j*/                           /* [↑]  gen  9 frequencey coefficients.*/
@.= 1
           do j=3  for N-2;   a= j-1;    b= a-1;      @.j= @.a + @.b
           end   /*j*/                           /* [↑]  generate  N  Fibonacci numbers.*/
call show "Benford's law applied to"      N      'Fibonacci numbers'
@.= 1
           do j=2  for N-1;   a= j-1;                 @.j= @.a * j
           end   /*j*/                           /* [↑]  generate  N  factorials.       */
call show "Benford's law applied to"      N      'factorial products'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
e:    return 2.71828182845904523536028747135266249775724709369995957496696762772407663035
log:  return ln( arg(1) )    /   LN10
ln:   procedure; arg x; e=e(); _=e; ig= (x>1.5); is= 1 -2*(ig\=1); i=0; s=x;  return .ln()
/*──────────────────────────────────────────────────────────────────────────────────────*/
.ln:  do while ig&s>1.5  |  \ig&s<.5
        do k=-1; iz= s*_**-is; if k>=0 & (ig&iz<1 | \ig&iz>.5)  then leave; _= _*_; izz=iz
        end   /*k*/
      s=izz;  i= i + is* 2**k; end  /*while*/;    x= x * e** - i - 1;  z= 0;  _= -1;  p= z
        do k=1;  _= -_ * x;  z= z + _/k; if z=p  then leave; p=z; end /*k*/;    return z+i
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: say;  say pad   ' digit '    pad   center("observed",w1)  pad  center('expected',w2)
      say pad  '───────'   pad   center("", w1, '─')  pad  center("",w2,'─')   pad  arg(1)
      !.=0;  do j=1  for N;    _= left(@.j, 1);     !._= !._ + 1    /*get the 1st digit.*/
             end   /*j*/

             do f=1  for 9                                          /*show the results. */
             say pad  center(f,7)   pad   center( format( !.f/N, , length(N-2)), w1)   #.f
             end   /*k*/
      return
```

{{out|output|text=  when using the default (1000 numbers)   for the input:}}

```txt

     digit       observed       expected
    ───────     ──────────     ──────────     Benford's law applied to 1000 Fibonacci numbers
       1          0.301         0.301030
       2          0.177         0.176091
       3          0.125         0.124939
       4          0.096         0.096910
       5          0.080         0.079181
       6          0.067         0.066947
       7          0.056         0.057992
       8          0.053         0.051153
       9          0.045         0.045757

     digit       observed       expected
    ───────     ──────────     ──────────     Benford's law applied to 1000 factorial products
       1          0.293         0.301030
       2          0.176         0.176091
       3          0.124         0.124939
       4          0.102         0.096910
       5          0.069         0.079181
       6          0.087         0.066947
       7          0.051         0.057992
       8          0.051         0.051153
       9          0.047         0.045757

```



## Ring


```ring

# Project : Benford's law

decimals(3)
n= 1000
actual = list(n)
for x = 1 to len(actual)
     actual[x] = 0
next

for nr = 1 to n
     n1 = string(fibonacci(nr))
     j = number(left(n1,1))
     actual[j] = actual[j] + 1
next

see "Digit   " + "Actual   " + "Expected" + nl
for m = 1 to 9
     fr = frequency(m)*100
     see "" + m + "   " + (actual[m]/10) + "   " + fr + nl
next

func frequency(n)
      freq = log10(n+1) - log10(n)
      return freq

func log10(n)
      log1 = log(n) / log(10)
      return log1

func fibonacci(y)
       if y = 0 return 0 ok
       if y = 1 return 1 ok
       if y > 1 return fibonacci(y-1) + fibonacci(y-2) ok

```

Output:

```txt

Digit	Actual	Expected
1	30.100	30.103
2	17.700	17.609
3	12.500	12.494
4	9.500	9.691
5	8.000	7.918
6	6.700	6.695
7	5.600	5.799
8	5.300	5.115
9	4.500	4.576

```



## Ruby

{{trans|Python}}

```ruby
EXPECTED = (1..9).map{|d| Math.log10(1+1.0/d)}

def fib(n)
  a,b = 0,1
  n.times.map{ret, a, b = a, b, a+b; ret}
end

# powers of 3 as a test sequence
def power_of_threes(n)
  n.times.map{|k| 3**k}
end

def heads(s)
  s.map{|a| a.to_s[0].to_i}
end

def show_dist(title, s)
  s = heads(s)
  c = Array.new(10, 0)
  s.each{|x| c[x] += 1}
  size = s.size.to_f
  res = (1..9).map{|d| c[d]/size}
  puts "\n    %s Benfords deviation" % title
  res.zip(EXPECTED).each.with_index(1) do |(r, e), i|
    puts "%2d: %5.1f%%  %5.1f%%  %5.1f%%" % [i, r*100, e*100, (r - e).abs*100]
  end
end

def random(n)
  n.times.map{rand(1..n)}
end

show_dist("fibbed", fib(1000))
show_dist("threes", power_of_threes(1000))

# just to show that not all kind-of-random sets behave like that
show_dist("random", random(10000))
```


{{out}}

```txt

    fibbed Benfords deviation
 1:  30.1%   30.1%    0.0%
 2:  17.7%   17.6%    0.1%
 3:  12.5%   12.5%    0.0%
 4:   9.5%    9.7%    0.2%
 5:   8.0%    7.9%    0.1%
 6:   6.7%    6.7%    0.0%
 7:   5.6%    5.8%    0.2%
 8:   5.3%    5.1%    0.2%
 9:   4.5%    4.6%    0.1%

    threes Benfords deviation
 1:  30.0%   30.1%    0.1%
 2:  17.7%   17.6%    0.1%
 3:  12.3%   12.5%    0.2%
 4:   9.8%    9.7%    0.1%
 5:   7.9%    7.9%    0.0%
 6:   6.6%    6.7%    0.1%
 7:   5.9%    5.8%    0.1%
 8:   5.2%    5.1%    0.1%
 9:   4.6%    4.6%    0.0%

    random Benfords deviation
 1:  10.9%   30.1%   19.2%
 2:  10.9%   17.6%    6.7%
 3:  11.7%   12.5%    0.8%
 4:  10.8%    9.7%    1.1%
 5:  11.2%    7.9%    3.3%
 6:  11.9%    6.7%    5.2%
 7:  10.7%    5.8%    4.9%
 8:  11.1%    5.1%    6.0%
 9:  10.8%    4.6%    6.2%

```



## Run BASIC


```runbasic

N	= 1000
for i = 0 to N - 1
    n$	= str$(fibonacci(i))
    j	= val(left$(n$,1))
    actual(j) = actual(j) +1
next
print
html "<table border=1><TR bgcolor=wheat><TD>Digit<td>Actual<td>Expected</td><tr>"
for i = 1 to 9
   html "<tr align=right><td>";i;"</td><td>";using("##.###",actual(i)/10);"</td><td>";using("##.###", frequency(i)*100);"</td></tr>"
next
html "</table>"
end

function frequency(n)
    frequency = log10(n+1) - log10(n)
end function

function log10(n)
    log10 = log(n) / log(10)
end function

function fibonacci(n)
    b = 1
    for i = 1 to n
        temp		= fibonacci + b
        fibonacci	= b
        b		= temp
    next i
end function

```

<table border=1><TR bgcolor=wheat><TD>Digit<td>Actual<td>Expected</td><tr><tr align=right><td>1</td><td>30.100</td><td>30.103</td></tr><tr align=right><td>2</td><td>17.700</td><td>17.609</td></tr><tr align=right><td>3</td><td>12.500</td><td>12.494</td></tr><tr align=right><td>4</td><td> 9.500</td><td> 9.691</td></tr><tr align=right><td>5</td><td> 8.000</td><td> 7.918</td></tr><tr align=right><td>6</td><td> 6.700</td><td> 6.695</td></tr><tr align=right><td>7</td><td> 5.600</td><td> 5.799</td></tr><tr align=right><td>8</td><td> 5.300</td><td> 5.115</td></tr><tr align=right><td>9</td><td> 4.500</td><td> 4.576</td></tr></table>


## Rust

{{works with|rustc|1.12 stable}}

This solution uses the ''num'' create for arbitrary-precision integers and the ''num_traits'' create for the ''zero'' and ''one'' implementations. It computes the Fibonacci numbers from scratch via the ''fib'' function.


```rust

extern crate num_traits;
extern crate num;

use num::bigint::{BigInt, ToBigInt};
use num_traits::{Zero, One};
use std::collections::HashMap;

// Return a vector of all fibonacci results from fib(1) to fib(n)
fn fib(n: usize) -> Vec<BigInt> {
    let mut result = Vec::with_capacity(n);
    let mut a = BigInt::zero();
    let mut b = BigInt::one();

    result.push(b.clone());

    for i in 1..n {
        let t = b.clone();
        b = a+b;
        a = t;
        result.push(b.clone());
    }

    result
}

// Return the first digit of a `BigInt`
fn first_digit(x: &BigInt) -> u8 {
    let zero = BigInt::zero();
    assert!(x > &zero);

    let s = x.to_str_radix(10);

    // parse the first digit of the stringified integer
    *&s[..1].parse::<u8>().unwrap()
}

fn main() {
    const N: usize = 1000;
    let mut counter: HashMap<u8, u32> = HashMap::new();
    for x in fib(N) {
        let d = first_digit(&x);
        *counter.entry(d).or_insert(0) += 1;
    }

    println!("{:>13}    {:>10}", "real", "predicted");
    for y in 1..10 {
        println!("{}: {:10.3} v. {:10.3}", y, *counter.get(&y).unwrap_or(&0) as f32 / N as f32,
        (1.0 + 1.0 / (y as f32)).log10());
    }

}

```

{{out}}

```txt

         real     predicted
1:      0.301 v.      0.301
2:      0.177 v.      0.176
3:      0.125 v.      0.125
4:      0.096 v.      0.097
5:      0.080 v.      0.079
6:      0.067 v.      0.067
7:      0.056 v.      0.058
8:      0.053 v.      0.051
9:      0.045 v.      0.046

```



## Scala


```scala
// Fibonacci Sequence (begining with 1,1): 1 1 2 3 5 8 13 21 34 55 ...
val fibs : Stream[BigInt] = { def series(i:BigInt,j:BigInt):Stream[BigInt] = i #:: series(j, i+j); series(1,0).tail.tail }


/**
 * Given a numeric sequence, return the distribution of the most-signicant-digit
 * as expected by Benford's Law and then by actual distribution.
 */
def benford[N:Numeric]( data:Seq[N] ) : Map[Int,(Double,Double)] = {

  import scala.math._

  val maxSize = 10000000  // An arbitrary size to avoid problems with endless streams

  val size = (data.take(maxSize)).size.toDouble

  val distribution = data.take(maxSize).groupBy(_.toString.head.toString.toInt).map{ case (d,l) => (d -> l.size) }

  (for( i <- (1 to 9) ) yield { (i -> (log10(1D + 1D / i), (distribution(i) / size))) }).toMap
}

{
  println( "Fibonacci Sequence (size=1000): 1 1 2 3 5 8 13 21 34 55 ...\n" )
  println( "%9s %9s %9s".format( "Actual", "Expected", "Deviation" ) )

  benford( fibs.take(1000) ).toList.sorted foreach {
    case (k, v) => println( "%d: %5.2f%% | %5.2f%% | %5.4f%%".format(k,v._2*100,v._1*100,math.abs(v._2-v._1)*100) )
  }
}
```

{{out}}

```txt

Fibonacci Sequence (size=1000): 1 1 2 3 5 8 13 21 34 55 ...

   Actual  Expected Deviation
1: 30.10% | 30.10% | 0.0030%
2: 17.70% | 17.61% | 0.0909%
3: 12.50% | 12.49% | 0.0061%
4:  9.60% |  9.69% | 0.0910%
5:  8.00% |  7.92% | 0.0819%
6:  6.70% |  6.69% | 0.0053%
7:  5.60% |  5.80% | 0.1992%
8:  5.30% |  5.12% | 0.1847%
9:  4.50% |  4.58% | 0.0757%

```



## Sidef


```ruby
var (actuals, expected) = ([], [])
var fibonacci = 1000.of {|i| fib(i).digit(0) }

for i (1..9) {
    var num = fibonacci.count_by {|j| j == i }
    actuals.append(num / 1000)
    expected.append(1 + (1/i) -> log10)
}

"%17s%17s\n".printf("Observed","Expected")
for i (1..9) {
    "%d : %11s %%%15s %%\n".printf(
            i, "%.2f".sprintf(100 *  actuals[i - 1]),
               "%.2f".sprintf(100 * expected[i - 1]),
    )
}
```


{{out}}

```txt

         Observed         Expected
1 :       30.10 %          30.10 %
2 :       17.70 %          17.61 %
3 :       12.50 %          12.49 %
4 :        9.50 %           9.69 %
5 :        8.00 %           7.92 %
6 :        6.70 %           6.69 %
7 :        5.60 %           5.80 %
8 :        5.30 %           5.12 %
9 :        4.50 %           4.58 %

```



## SQL

If we load some numbers into a table, we can do the sums without too much difficulty. I tried to make this as database-neutral as possible, but I only had Oracle handy to test it on.

The query is the same for any number sequence you care to put in the <tt>benford</tt> table.


```SQL
-- Create table
create table benford (num integer);

-- Seed table
insert into benford (num) values (1);
insert into benford (num) values (1);
insert into benford (num) values (2);

-- Populate table
insert into benford (num)
  select
    ult + penult
  from
    (select max(num) as ult from benford),
    (select max(num) as penult from benford where num not in (select max(num) from benford))

-- Repeat as many times as desired
--    in Oracle SQL*Plus, press "Slash, Enter" a lot of times
--    or wrap this in a loop, but that will require something db-specific...

-- Do sums
select
  digit,
  count(digit) / numbers as actual,
  log(10, 1 + 1 / digit) as expected
from
  (
    select
      floor(num/power(10,length(num)-1)) as digit
    from
      benford
  ),
  (
    select
      count(*) as numbers
    from
      benford
  )
group by digit, numbers
order by digit;

-- Tidy up
drop table benford;
```


{{out}}
I only loaded the first 100 Fibonacci numbers before my fingers were sore from repeating the data load. 8~)

```txt
     DIGIT     ACTUAL   EXPECTED
---------- ---------- ----------
         1         .3 .301029996
         2        .18 .176091259
         3        .13 .124938737
         4        .09 .096910013
         5        .08 .079181246
         6        .06  .06694679
         7        .05 .057991947
         8        .07 .051152522
         9        .04 .045757491

9 rows selected.
```



## Stata



```stata
clear
set obs 1000
scalar phi=(1+sqrt(5))/2
gen fib=(phi^_n-(-1/phi)^_n)/sqrt(5)
gen k=real(substr(string(fib),1,1))
hist k, discrete                      // show a histogram
qui tabulate k, matcell(f)            // compute frequencies

mata
f=st_matrix("f")
p=log10(1:+1:/(1::9))*sum(f)
// print observed vs predicted probabilities
f,p
                 1             2
    +-----------------------------+
  1 |          297   301.0299957  |
  2 |          178   176.0912591  |
  3 |          127   124.9387366  |
  4 |           96   96.91001301  |
  5 |           80   79.18124605  |
  6 |           67   66.94678963  |
  7 |           57   57.99194698  |
  8 |           53   51.15252245  |
  9 |           45   45.75749056  |
    +-----------------------------+
```


Assuming the data are random, one can also do a goodness of fit [https://en.wikipedia.org/wiki/Pearson%27s_chi-squared_test chi-square test]:


```stata
// chi-square statistic
chisq=sum((f-p):^2:/p)
chisq
  .2219340262
// p-value
chi2tail(8,chisq)
  .9999942179
end
```


The p-value is very close to 1, showing that the observed distribution is very close to the Benford law.

The fit is not as good with the sequence (2+sqrt(2))^n:


```stata
clear
set obs 500
scalar s=2+sqrt(2)
gen a=s^_n
gen k=real(substr(string(a),1,1))
hist k, discrete
qui tabulate k, matcell(f)

mata
f=st_matrix("f")
p=log10(1:+1:/(1::9))*sum(f)
f,p
                 1             2
    +-----------------------------+
  1 |          134   150.5149978  |
  2 |           99   88.04562953  |
  3 |           68    62.4693683  |
  4 |           34    48.4550065  |
  5 |           33   39.59062302  |
  6 |           33   33.47339482  |
  7 |           33   28.99597349  |
  8 |           33   25.57626122  |
  9 |           33   22.87874528  |
    +-----------------------------+

chisq=sum((f-p):^2:/p)
chisq
  16.26588528

chi2tail(8,chisq)
  .0387287805
end
```


Now the p-value is less than the usual 5% risk, and one would reject the hypothesis that the data follow the Benford law.


## Swift



```Swift
import Foundation

/* Reads from a file and returns the content as a String */
func readFromFile(fileName file:String) -> String{

    var ret:String = ""

    let path = Foundation.URL(string: "file://"+file)

    do {
        ret = try String(contentsOf: path!, encoding: String.Encoding.utf8)
    }
    catch {
        print("Could not read from file!")
        exit(-1)
    }

    return ret
}

/* Calculates the probability following Benford's law */
func benford(digit z:Int) -> Double {

    if z<=0 || z>9 {
        perror("Argument must be between 1 and 9.")
        return 0
    }

    return log10(Double(1)+Double(1)/Double(z))
}

// get CLI input
if CommandLine.arguments.count < 2 {
    print("Usage: Benford [FILE]")
    exit(-1)
}

let pathToFile = CommandLine.arguments[1]

// Read from given file and parse into lines
let content = readFromFile(fileName: pathToFile)
let lines = content.components(separatedBy: "\n")

var digitCount:UInt64 = 0
var countDigit:[UInt64] = [0,0,0,0,0,0,0,0,0]

// check digits line by line
for line in lines {
    if line == "" {
        continue
    }
    let charLine = Array(line.characters)
        switch(charLine[0]){
            case "1":
                countDigit[0] += 1
                digitCount += 1
                break
            case "2":
                countDigit[1] += 1
                digitCount += 1
                break
            case "3":
                countDigit[2] += 1
                digitCount += 1
                break
            case "4":
                countDigit[3] += 1
                digitCount += 1
                break
            case "5":
                countDigit[4] += 1
                digitCount += 1
                break
            case "6":
                countDigit[5] += 1
                digitCount += 1
                break
            case "7":
                countDigit[6] += 1
                digitCount += 1
                break
            case "8":
                countDigit[7] += 1
                digitCount += 1
                break
            case "9":
                countDigit[8] += 1
                digitCount += 1
                break
            default:
                break
        }

}

// print result
print("Digit\tBenford [%]\tObserved [%]\tDeviation")
print("~~~~~\t~~~~~~~~~~~~\t~~~~~~~~~~~~\t~~~~~~~~~")
for i in 0..<9 {
    let temp:Double = Double(countDigit[i])/Double(digitCount)
    let ben = benford(digit: i+1)
    print(String(format: "%d\t%.2f\t\t%.2f\t\t%.4f", i+1,ben*100,temp*100,ben-temp))
}
```

{{out}}

```txt
$ ./Benford
Usage: Benford [FILE]
$ ./Benford Fibonacci.txt
Digit	Benford [%]	Observed [%]	Deviation
~~~~~	~~~~~~~~~~~~	~~~~~~~~~~~~	~~~~~~~~~
1	30.10		30.10		0.0000
2	17.61		17.70		-0.0009
3	12.49		12.50		-0.0001
4	9.69		9.60		0.0009
5	7.92		8.00		-0.0008
6	6.69		6.70		-0.0001
7	5.80		5.60		0.0020
8	5.12		5.30		-0.0018
9	4.58		4.50		0.0008
```



## Tcl


```tcl
proc benfordTest {numbers} {
    # Count the leading digits (RE matches first digit in each number,
    # even if negative)
    set accum {1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0}
    foreach n $numbers {
	if {[regexp {[1-9]} $n digit]} {
	    dict incr accum $digit
	}
    }

    # Print the report
    puts " digit | measured | theory"
    puts "-------+----------+--------"
    dict for {digit count} $accum {
	puts [format "%6d | %7.2f%% | %5.2f%%" $digit \
		  [expr {$count * 100.0 / [llength $numbers]}] \
		  [expr {log(1+1./$digit)/log(10)*100.0}]]
    }
}
```

Demonstrating with Fibonacci numbers:

```tcl
proc fibs n {
    for {set a 1;set b [set i 0]} {$i < $n} {incr i} {
	lappend result [set b [expr {$a + [set a $b]}]]
    }
    return $result
}
benfordTest [fibs 1000]
```

{{out}}

```txt

 digit | measured | theory
-------+----------+--------
     1 |   30.10% | 30.10%
     2 |   17.70% | 17.61%
     3 |   12.50% | 12.49%
     4 |    9.60% |  9.69%
     5 |    8.00% |  7.92%
     6 |    6.70% |  6.69%
     7 |    5.60% |  5.80%
     8 |    5.30% |  5.12%
     9 |    4.50% |  4.58%

```



## Visual FoxPro


```vfp

#DEFINE CTAB CHR(9)
#DEFINE COMMA ","
#DEFINE CRLF CHR(13) + CHR(10)
LOCAL i As Integer, n As Integer, n1 As Integer, rho As Double, c As String
n = 1000
LOCAL ARRAY a[n,2], res[1]
CLOSE DATABASES ALL
CREATE CURSOR fibo(dig C(1))
INDEX ON dig TAG dig COLLATE "Machine"
SET ORDER TO 0
*!* Populate the cursor with the leading digit of the first 1000 Fibonacci numbers
a[1,1] = "1"
a[1,2] = 1
a[2,1] = "1"
a[2,2] = 1
FOR i = 3 TO n
    a[i,2] = a[i-2,2] + a[i-1,2]
    a[i,1] = LEFT(TRANSFORM(a[i,2]), 1)
ENDFOR
APPEND FROM ARRAY a FIELDS dig
CREATE CURSOR results (digit I, count I, prob B(6), expected B(6))
INSERT INTO results ;
SELECT dig, COUNT(1), COUNT(1)/n, Pr(VAL(dig)) FROM fibo GROUP BY dig ORDER BY dig
n1 = RECCOUNT()
*!* Correlation coefficient
SELECT (n1*SUM(prob*expected) - SUM(prob)*SUM(expected))/;
(SQRT(n1*SUM(prob*prob) - SUM(prob)*SUM(prob))*SQRT(n1*SUM(expected*expected) - SUM(expected)*SUM(expected))) ;
FROM results INTO ARRAY res
rho = CAST(res[1] As B(6))
SET SAFETY OFF
COPY TO benford.txt TYPE CSV
c = FILETOSTR("benford.txt")
*!* Replace commas with tabs
c = STRTRAN(c, COMMA, CTAB) + CRLF + "Correlation Coefficient: " + TRANSFORM(rho)
STRTOFILE(c, "benford.txt", 0)
SET SAFETY ON

FUNCTION Pr(d As Integer) As Double
RETURN LOG10(1 + 1/d)
ENDFUNC

```

{{out}}

```txt

digit	count	prob	expected
1		301	0.301000	0.301000
2		177	0.177000	0.176100
3		125	0.125000	0.124900
4		 96	0.096000	0.096900
5		 80	0.080000	0.079200
6		 67	0.067000	0.066900
7		 56	0.056000	0.058000
8		 53	0.053000	0.051200
9		 45	0.045000	0.045800

Correlation Coefficient: 0.999908

```



## zkl

{{trans|Go}}

```zkl
show(  // use list (fib(1)...fib(1000)) --> (1..4.34666e+208)
   (0).pump(1000,List,fcn(ab){ab.append(ab.sum(0.0)).pop(0)}.fp(L(1,1))),
   "First 1000 Fibonacci numbers");

fcn show(data,title){
   f:=(0).pump(9,List,Ref.fp(0)); // (Ref(0),Ref(0)...
   foreach v in (data){ // eg  1.49707e+207 ("g" format) --> "1" (first digit)
      f[v.toString()[0].toInt()-1].inc(); }
   println(title);
   println("Digit  Observed  Predicted");
   foreach i,n in ([1..].zip(f)){ // -->(1,Ref)...(9,Ref)
      println("  %d  %9.3f  %8.3f".fmt(i,n.value.toFloat()/data.len(),
            (1.0+1.0/i).log10()))
   }
}
```

{{trans|CoffeeScript}}

```zkl
var BN=Import("zklBigNum");

fcn fibgen(a,b) { return(a,self.fcn.fp(b,a+b)) } //-->L(fib,fcn)

benford := [0..9].pump(List,Ref.fp(0)).copy(); //L(Ref(0),...)

const N=1000;

[1..N].reduce('wrap(fiber,_){
   n,f:=fiber;
   benford[n.toString()[0]].inc(); // first digit of fib
   f() // next (fib,fcn) pair
},fibgen(BN(1),BN(1)));

   // de-ref Refs ie convert to int to float, divide by N
actual   := benford.apply(T("value","toFloat",'/(N)));
expected := [1..9].apply(fcn(x){(1.0 + 1.0/x).log10()});

println("Leading digital distribution of the first 1,000 Fibonacci numbers");
println("Digit\tActual\tExpected");
foreach i in ([1..9]){ println("%d\t%.3f\t%.3f".fmt(i,actual[i], expected[i-1])); }
```

{{out}}

```txt

First 1000 Fibonacci numbers
Digit  Observed  Predicted
  1      0.301     0.301
  2      0.177     0.176
  3      0.125     0.125
  4      0.096     0.097
  5      0.080     0.079
  6      0.067     0.067
  7      0.056     0.058
  8      0.053     0.051
  9      0.045     0.046

```



## ZX Spectrum Basic

{{trans|Liberty BASIC}}

```zxbasic
10 RANDOMIZE
20 DIM b(9)
30 LET n=100
40 FOR i=1 TO n
50 GO SUB 1000
60 LET n$=STR$ fiboI
70 LET d=VAL n$(1)
80 LET b(d)=b(d)+1
90 NEXT i
100 PRINT "Digit";TAB 6;"Actual freq";TAB 18;"Expected freq"
110 FOR i=1 TO 9
120 LET pdi=(LN (i+1)/LN 10)-(LN i/LN 10)
130 PRINT i;TAB 6;b(i)/n;TAB 18;pdi
140 NEXT i
150 STOP
1000 REM Fibonacci
1010 LET fiboI=0: LET b=1
1020 FOR j=1 TO i
1030 LET temp=fiboI+b
1040 LET fiboI=b
1050 LET b=temp
1060 NEXT j
1070 RETURN

```

The results obtained are adjusted fairly well, except for the number 8. This occurs with Sinclair BASIC, Sam BASIC and SpecBAS fits.
