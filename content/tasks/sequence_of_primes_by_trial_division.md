+++
title = "Sequence of primes by trial division"
description = ""
date = 2019-10-20T02:00:13Z
aliases = []
[extra]
id = 17919
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
+++

## Task

Generate a sequence of primes by means of trial division.


Trial division is an algorithm where a candidate number is tested for being a prime by trying to divide it by other numbers.

You may use primes, or any numbers of your choosing, as long as the result is indeed a sequence of primes.

The sequence may be bounded (i.e. up to some limit), unbounded, starting from the start (i.e. 2) or above some given value.

Organize your function as you wish, in particular, it might resemble a filtering operation, or a sieving operation.

If you want to use a ready-made <code>is_prime</code> function, use one from the [[Primality by trial division]] page (i.e., add yours there if it isn't there already).


## Related tasks

*   [[count in factors]]
*   [[prime decomposition]]
*   [[factors of an integer]]
*   [[Sieve of Eratosthenes]]
*   [[primality by trial division]]
*   [[factors of a Mersenne number]]
*   [[trial factoring of a Mersenne number]]
*   [[partition an integer X into N primes]]





## Ada


Use the generic function Prime_Numbers.Is_Prime, as specified in [[Prime decomposition#Ada]]. The program reads two numbers A and B from the command line and prints all primes between A and B (inclusive).


```Ada
with Prime_Numbers, Ada.Text_IO, Ada.Command_Line;

procedure Sequence_Of_Primes is

   package Integer_Numbers is new
     Prime_Numbers (Natural, 0, 1, 2);
   use Integer_Numbers;

   Start: Natural := Natural'Value(Ada.Command_Line.Argument(1));
   Stop:  Natural := Natural'Value(Ada.Command_Line.Argument(2));

begin
   for I in Start .. Stop loop
      if Is_Prime(I) then
         Ada.Text_IO.Put(Natural'Image(I));
      end if;
   end loop;
end Sequence_Of_Primes;
```


```txt
>./sequence_of_primes 50 99
 53 59 61 67 71 73 79 83 89 97
```



## ALGOL 68

Simple bounded sequence using the "is prime" routine from [[Primality by trial division#ALGOL 68]]

```algol68
# is prime PROC from the primality by trial division task #
MODE ISPRIMEINT = INT;
PROC is prime = ( ISPRIMEINT p )BOOL:
  IF p <= 1 OR ( NOT ODD p AND p/= 2) THEN
    FALSE
  ELSE
    BOOL prime := TRUE;
    FOR i FROM 3 BY 2 TO ENTIER sqrt(p)
      WHILE prime := p MOD i /= 0 DO SKIP OD;
    prime
  FI;
# end of code from the primality by trial division task #

# returns an array of n primes >= start #
PROC prime sequence = ( INT start, INT n )[]INT:
     BEGIN
        [ n ]INT seq;
        INT      prime count := 0;
        FOR p FROM start WHILE prime count < n DO
            IF is prime( p ) THEN
                prime count +:= 1;
                seq[ prime count ] := p
            FI
        OD;
        seq
     END; # prime sequence #

# find 20 primes >= 30 #
[]INT primes = prime sequence( 30, 20 );
print( ( "20 primes starting at 30: " ) );
FOR p FROM LWB primes TO UPB primes DO
    print( ( " ", whole( primes[ p ], 0 ) ) )
OD;
print( ( newline ) )
```

```txt

20 primes starting at 30:  31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113

```


=={{header|ALGOL-M}}==

```algol

BEGIN
COMMENT
  PRIME NUMBER GENERATOR IN ALGOL-M.
  ONLY ODD NUMBERS ARE CHECKED, AND ONLY THE PRIME NUMBERS
  PREVIOUSLY FOUND (UP TO THE SQUARE ROOT OF THE NUMBER
  CURRENTLY UNDER EXAMINATION) ARE TESTED AS DIVISORS;

INTEGER I, K, M, N, S, NPRIMES, DIVISIBLE, FALSE, TRUE;

COMMENT COMPUTE P MOD Q;
INTEGER FUNCTION MOD (P, Q);
INTEGER P, Q;
BEGIN
    MOD := P - Q * (P / Q);
END;

COMMENT MAIN PROGRAM BEGINS HERE;

WRITE ("How many primes do you want to generate?");
READ (NPRIMES);
  BEGIN
    INTEGER ARRAY P[1:NPRIMES];
    FALSE := 0;
    TRUE := -1;

    COMMENT INITIALIZE P WITH FIRST PRIME NUMBER AND DISPLAY IT;
    P[1] := 2;
    WRITE (1, ":", P[1]);

    I := 1;  % COUNT OF PRIME NUMBERS FOUND SO FAR %
    K := 1;  % INDEX OF LARGEST PRIME <= SQRT OF N %
    N := 3;  % CURRENT NUMBER BEING CHECKED %
    WHILE I < NPRIMES DO
      BEGIN
        S := P[K] * P[k];
        IF S <= N THEN K := K + 1;
        DIVISIBLE := FALSE;
        M := 1;  % INDEX OF POSSIBLE DIVISORS %
        WHILE M <= K AND DIVISIBLE = FALSE DO
          BEGIN
            IF MOD(N, P[M]) = 0 THEN DIVISIBLE := TRUE;
            M := M + 1;
          END;
        IF DIVISIBLE = FALSE THEN
          BEGIN
            I := I + 1;
            P[I] := N;
            WRITE (I, ":", N);
          END;
        N := N + 2;
      END;
  END;
WRITE("All done. Goodbye");
END

```



## ALGOL W

Uses the ALGOL W isPrime procedure from the Primality by Trial Division task.

```algolw
begin
    % use the isPrime procedure from the Primality by Trial Division task     %
    logical procedure isPrime ( integer value n ) ; algol "isPrime" ;
    % sets the elements of p to the first n primes. p must have bounds 1 :: n %
    procedure getPrimes ( integer array p ( * )
                        ; integer value n
                        ) ;
    if n > 0 then begin
        % have room for at least oe prime %
        integer pPos, possiblePrime;
        p( 1 )        := 2;
        pPos          := 2;
        possiblePrime := 3;
        while pPos <= n do begin
            if isPrime( possiblePrime ) then begin
                p( pPos )     := possiblePrime;
                pPos          := pPos + 1;
            end;
            possiblePrime := possiblePrime + 1
        end
    end getPrimes ;

    begin % test getPrimes %
        integer array p( 1 :: 100 );
        getPrimes( p, 100 );
        for i := 1 until 100 do begin
            if i rem 20 = 1 then write(   i_w := 4, s_w := 1, p( i ) )
                            else writeon( i_w := 4, s_w := 1, p( i ) )
        end for_i
    end

end.
```

```txt

   2    3    5    7   11   13   17   19   23   29   31   37   41   43   47   53   59   61   67   71
  73   79   83   89   97  101  103  107  109  113  127  131  137  139  149  151  157  163  167  173
 179  181  191  193  197  199  211  223  227  229  233  239  241  251  257  263  269  271  277  281
 283  293  307  311  313  317  331  337  347  349  353  359  367  373  379  383  389  397  401  409
 419  421  431  433  439  443  449  457  461  463  467  479  487  491  499  503  509  521  523  541

```



## ATS


```ATS
(*
// Lazy-evaluation:
//   sieve for primes
*)
(* ****** ****** *)
//
// How to compile:
// with no GC:
// patscc -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -o sieve sieve.dats
// with Boehm-GC:
// patscc -D_GNU_SOURCE -DATS_MEMALLOC_GCBDW -o sieve sieve.dats -lgc
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

#define :: stream_cons
#define cons stream_cons
#define nil stream_nil

(* ****** ****** *)
//
fun
from{n:int} (n: int n)
  :<!laz> stream (intGte(n)) = $delay (cons{intGte(n)}(n, from (n+1)))
//
(* ****** ****** *)

typedef N2 = intGte(2)

(* ****** ****** *)

fun sieve
(
  ns: stream N2
) :<!laz>
  stream (N2) = $delay
(
let
  val-cons(n, ns) = !ns
in
  cons{N2}(n, sieve (stream_filter_cloref<N2> (ns, lam x => g1int_nmod(x, n) > 0)))
end : stream_con (N2)
) // end of [sieve]

//

val primes: stream (N2) = sieve (from(2))

//

macdef prime_get (n) = stream_nth_exn (primes, ,(n))

//

implement
main0 () = begin
//
println! ("prime 1000 = ", prime_get (1000)) ; // = 7927
(*
println! ("prime 5000 = ", prime_get (5000)) ; // = 48619
println! ("prime 10000 = ", prime_get (10000)) ; // = 104743
*)
//
end // end of [main0]

(* ****** ****** *)
```


## AWK


```AWK

# syntax: GAWK -f SEQUENCE_OF_PRIMES_BY_TRIAL_DIVISION.AWK
BEGIN {
    low = 1
    high = 100
    for (i=low; i<=high; i++) {
      if (is_prime(i) == 1) {
        printf("%d ",i)
        count++
      }
    }
    printf("\n%d prime numbers found in range %d-%d\n",count,low,high)
    exit(0)
}
function is_prime(x,  i) {
    if (x <= 1) {
      return(0)
    }
    for (i=2; i<=int(sqrt(x)); i++) {
      if (x % i == 0) {
        return(0)
      }
    }
    return(1)
}

```

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
25 prime numbers found in range 1-100

```



## Batch File


```Batch File

@echo off
::Prime list using trial division
:: Unbounded (well, up to 2^31-1, but you'll kill it before :)
:: skips factors of 2 and 3 in candidates and in divisors
:: uses integer square root to find max divisor to test
:: outputs numbers in rows of 10 right aligned primes
setlocal enabledelayedexpansion

cls
echo prime list
set lin=    0:
set /a num=1, inc1=4, cnt=0
call :line 2
call :line 3


:nxtcand
set /a num+=inc1, inc1=6-inc1,div=1, inc2=4
call :sqrt2 %num% & set maxdiv=!errorlevel!

:nxtdiv
set /a div+=inc2, inc2=6-inc2, res=(num%%div)
if %div% gtr !maxdiv! call :line %num% &  goto nxtcand
if %res%  equ 0 (goto :nxtcand ) else ( goto nxtdiv)

:sqrt2   [num] calculates integer square root
if %1 leq 0 exit /b 0
set /A "x=%1/(11*1024)+40, x=(%1/x+x)>>1, x=(%1/x+x)>>1, x=(%1/x+x)>>1, x=(%1/x+x)>>1, x=(%1/x+x)>>1, x+=(%1-x*x)>>31,sq=x*x
if sq gtr %1 set x-=1
exit /b !x!
goto:eof

:line    formats output in 10 right aligned columns
set num1=      %1
set lin=!lin!%num1:~-7%
set /a cnt+=1,res1=(cnt%%10)
if %res1% neq 0 goto:eof
echo %lin%
set cnt1=    !cnt!
set lin=!cnt1:~-5!:
goto:eof

```

```txt

prime list
    0:      2      3      5      7     11     13     17     19     23     29
   10:     31     37     41     43     47     53     59     61     67     71
   20:     73     79     83     89     97    101    103    107    109    113
   30:    127    131    137    139    149    151    157    163    167    173
   40:    179    181    191    193    197    199    211    223    227    229
   50:    233    239    241    251    257    263    269    271    277    281
   60:    283    293    307    311    313    317    331    337    347    349
   70:    353    359    367    373    379    383    389    397    401    409
   80:    419    421    431    433    439    443    449    457    461    463
   90:    467    479    487    491    499    503    509    521    523    541
  100:    547    557    563    569    571    577    587    593    599    601
  110:    607    613    617    619    631    641    643    647    653    659
  120:    661    673    677    683    691    701    709    719    727    733
  130:    739    743    751    757    761    769    773    787    797    809
  140:    811    821    823    827    829    839    853    857    859    863
  150:    877    881    883    887    907    911    919    929    937    941
  160:    947    953    967    971    977    983    991    997   1009   1013
  170:   1019   1021   1031   1033   1039   1049   1051   1061   1063   1069
  180:   1087   1091   1093   1097   1103   1109   1117   1123   1129   1151
  190:   1153   1163   1171   1181   1187   1193   1201   1213   1217   1223
  200:   1229   1231   1237   1249   1259   1277   1279   1283   1289   1291
  210:   1297   1301   1303   1307   1319   1321   1327   1361   1367   1373
  220:   1381   1399   1409   1423   1427   1429   1433   1439   1447   1451
  230:   1453   1459   1471   1481   1483   1487   1489   1493   1499   1511
  240:   1523   1531   1543   1549   1553   1559   1567   1571   1579   1583
  250:   1597   1601   1607   1609   1613   1619   1621   1627   1637   1657
  260:   1663   1667   1669   1693   1697   1699   1709   1721   1723   1733
  270:   1741   1747   1753   1759   1777   1783   1787   1789   1801   1811
  280:   1823   1831   1847   1861   1867   1871   1873   1877   1879   1889

```



## Befunge

Based on the test in the [[Primality_by_trial_division#Befunge|Primality by trial division]] task, this list all primes between 2 and 1,000,000.


```befunge>2
:::"}"8*:*>`#@_48*:**2v
v_v#`\*:%*:*84\/*:*84::+<
v#>::48*:*/\48*:*%%!#v_1^
<^+1$_.#<5#<5#<+#<,#<<0:\
```


```txt
2
3
5
7
11
13
.
.
.
999931
999953
999959
999961
999979
999983
```



## C


```C

#include<stdio.h>

int isPrime(unsigned int n)
{
	unsigned int num;

	if ( n < 2||!(n & 1))
		return n == 2;

	for (num = 3; num <= n/num; num += 2)
		if (!(n % num))
			return 0;
	return 1;
}

int main()
{
	unsigned int l,u,i,sum=0;

	printf("Enter lower and upper bounds: ");
	scanf("%ld%ld",&l,&u);

	for(i=l;i<=u;i++){
		if(isPrime(i)==1)
			{
				printf("\n%ld",i);
				sum++;
			}
	}

	printf("\n\nPrime numbers found in [%ld,%ld] : %ld",l,u,sum);

	return 0;
}

```

Output :

```txt

Enter lower and upper bounds: 1 100

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
37
41
43
47
53
59
61
67
71
73
79
83
89
97

Prime numbers found in [1,100] : 25

```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;

public class Program
{
    static void Main() {
        Console.WriteLine(string.Join(" ", Primes(100)));
    }

    static IEnumerable<int> Primes(int limit) => Enumerable.Range(2, limit-2).Where(IsPrime);
    static bool IsPrime(int n) => Enumerable.Range(2, (int)Math.Sqrt(n)-1).All(i => n % i != 0);
}
```

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```



## C++


```cpp

#include <math.h>
#include <iostream>
#include <iomanip>

bool isPrime( unsigned u ) {
    if( u < 4 ) return u > 1;
    if( /*!( u % 2 ) ||*/ !( u % 3 ) ) return false;

    unsigned q = static_cast<unsigned>( sqrt( static_cast<long double>( u ) ) ),
             c = 5;
    while( c <= q ) {
        if( !( u % c ) || !( u % ( c + 2 ) ) ) return false;
        c += 6;
    }
    return true;
}
int main( int argc, char* argv[] )
{
    unsigned mx = 100000000,
             wid = static_cast<unsigned>( log10( static_cast<long double>( mx ) ) ) + 1;

    std::cout << "[" << std::setw( wid ) << 2 << " ";
    unsigned u = 3, p = 1; // <- start computing from 3
    while( u < mx ) {
        if( isPrime( u ) ) { std::cout << std::setw( wid ) << u << " "; p++; }
        u += 2;
    }
    std::cout << "]\n\n Found " << p << " primes.\n\n";
    return 0;
}

```

```txt

[        2         3         5         7        11        13        17        19
        23        29        31        37        41        43        47        53
        59        61        67        71        73        79        83        89
        97 ...


```



## Clojure


```lisp
(ns test-p.core
  (:require [clojure.math.numeric-tower :as math]))

(defn prime? [a]
  " Uses trial division to determine if number is prime "
  (or (= a 2)
      (and (> a 2)
           (> (mod a 2) 0)
           (not (some #(= 0 (mod a %))
                  (range 3 (inc (int (Math/ceil (math/sqrt a)))) 2))))))
                       ; 3 to sqrt(a) stepping by 2

(defn primes-below [n]
  " Finds primes below number n "
  (for  [a (range 2 (inc n))
         :when (prime? a)]
        a))

(println (primes-below 100))
```


```txt
(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```



## Common Lisp


```lisp
(defun primes-up-to (max-number)
    "Compute all primes up to MAX-NUMBER using trial division"
    (loop for n from 2 upto max-number
          when (notany (evenly-divides n) primes)
          collect n into primes
          finally (return primes)))

(defun evenly-divides (n)
    "Create a function that checks whether its input divides N evenly"
    (lambda (x) (integerp (/ n x))))

(print (primes-up-to 100))
```


Output:
```txt
(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```



## D

This is a quite inefficient prime generator.

```d
import std.stdio, std.range, std.algorithm, std.traits,
       std.numeric, std.concurrency;

Generator!(ForeachType!R) nubBy(alias pred, R)(R items) {
    return new typeof(return)({
        ForeachType!R[] seen;

        OUTER: foreach (x; items) {
            foreach (y; seen)
                if (pred(x, y))
                    continue OUTER;
            yield(x);
            seen ~= x;
        }
    });
}

void main() /*@safe*/ {
    sequence!q{n + 2}
    .nubBy!((x, y) => gcd(x, y) > 1)
    .take(20)
    .writeln;
}
```

```txt
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]
```



## EchoLisp


### Trial division


```scheme
(lib 'sequences)
(define (is-prime? p)
	(cond
	[(< p 2) #f]
	[(zero? (modulo p 2)) (= p 2)]
	[else
		(for/and ((d [3 5 .. (1+ (sqrt p))] ))  (!zero? (modulo p d)))]))

(is-prime? 101) → #t
```


===Bounded - List filter ===

```scheme
(filter is-prime? (range 1 100))
    → (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```


=== Unbounded - Sequence filter ===

```scheme
(define f-primes (filter is-prime? [2 .. ]))
    → # 👓 filter: #sequence [2 3 .. Infinity[

(take f-primes 25)
    → (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```


=== Unbounded - Stream ===

```scheme
(define (s-next-prime n) ;; n odd
    (for ((p [n (+ n 2) .. ] ))
    #:break (is-prime? p) => (cons p (+ p 2))))

(define s-primes (stream-cons 2 (make-stream s-next-prime 3)))


(take s-primes 25)
    → (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```


=== Unbounded - Generator ===

```scheme
(define (g-next-prime n)
    (define next
        (for ((p [n .. ] )) #:break (is-prime? p) => p ))
    (yield next)
    (1+ next))

(define g-primes (make-generator g-next-prime 2))

(take g-primes 25)
    → (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```


=== Unbounded - Background task ===

```scheme
(lib 'tasks)
(lib 'bigint)

(define (t-next-prime n)
		(define next
		(for ((p [n .. ] )) #:break (is-prime? p) =>  p ))
		(writeln next) ;; or whatever action here
		(1+ next)) ;; unbounded : return #f to stop or CTRL-C



(define t-primes (make-task t-next-prime 1_000_000_000_000))

(task-run t-primes)
   → #task:id:95:running
1000000000039
1000000000061
1000000000063
1000000000091
1000000000121
1000000000163
*stopped*
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
		do
			sequence (1, 27)
		end

	sequence (lower, upper: INTEGER)
			-- Sequence of primes from 'lower' to 'upper'.
		require
			lower_positive: lower > 0
			upper_positive: upper > 0
			lower_smaller: lower < upper
		local
			i: INTEGER
		do
			io.put_string ("Sequence of primes from " + lower.out + " up to " + upper.out + ".%N")
			i := lower
			if i \\ 2 = 0 then
				i := i + 1
			end
			from
			until
				i > upper
			loop
				if is_prime (i) then
					io.put_integer (i)
					io.put_new_line
				end
				i := i + 2
			end
		end

feature {NONE}

	is_prime (n: INTEGER): BOOLEAN
			-- Is 'n' a prime number?
		require
			positiv_input: n > 0
		local
			i: INTEGER
			max: REAL_64
			math: DOUBLE_MATH
		do
			create math
			if n = 2 then
				Result := True
			elseif n <= 1 or n \\ 2 = 0 then
				Result := False
			else
				Result := True
				max := math.sqrt (n)
				from
					i := 3
				until
					i > max
				loop
					if n \\ i = 0 then
						Result := False
					end
					i := i + 2
				end
			end
		end

end

```

```txt

Sequence of primes from 1 to 27.)
3
5
7
11
13
17
19
23

```


## Elena

ELENA 4.x :

```elena
import extensions;
import system'routines;
import system'math;

isPrime =
    (n => new Range(2,(n.sqrt() - 1).RoundedInt).allMatchedBy:(i => n.mod:i != 0));

Primes =
    (n => new Range(2, n - 2).filterBy:isPrime);

public program()
{
    console.printLine(Primes(100))
}
```

```txt

2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97

```



## Elixir


```elixir
defmodule Prime do
  def sequence do
    Stream.iterate(2, &(&1+1)) |> Stream.filter(&is_prime/1)
  end

  def is_prime(2), do: true
  def is_prime(n) when n<2 or rem(n,2)==0, do: false
  def is_prime(n), do: is_prime(n,3)

  defp is_prime(n,k) when n<k*k, do: true
  defp is_prime(n,k) when rem(n,k)==0, do: false
  defp is_prime(n,k), do: is_prime(n,k+2)
end

IO.inspect Prime.sequence |> Enum.take(20)
```


```txt

[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]

```



## ERRE


```ERRE

PROGRAM PRIME_GENERATOR

!$DOUBLE

BEGIN
   PRINT(CHR$(12);) !CLS
   N=1
   LOOP
     N+=1
     FOR F=2 TO N DO
       IF F=N THEN PRINT(N;) EXIT END IF
       EXIT IF N=F*INT(N/F)
     END FOR
   END LOOP
END PROGRAM

```

You must press Ctrl+Break to stop the program.
```txt
 2  3  5  7  11  13  17  19  23  29  31  37  41  43  47  53  59  61  67  71  73
 79  83  89  97  101  103  107  109  113  127  131  137  139  149  151  157
 163  167  173  179  181  191  193  197  199  211  223  227  229  233  239  241
 251  257  263  269  271  277  281  283  293  307  311  313  317  331  337  347
 349  353  359  367  373  379  383  389  397  401  409  419  421  431  433  439
 443  449  457  461  463  467  479  487  491  499  503  509  521  523  541  547
 557  563  569  571  577  587  593  599  601  607  613  617  619  631  641  643
 647  653  659  661  673  677  683  691  701  709  719  727  733  739  743  751
 757  761  769  773  787  797  809  811  821  823  827  829  839  853  857  859
 863  877  881  883  887  907  911  919  929  937  941  947  953  967  971  977
 983  991  997  1009  1013  1019  1021  1031  1033  1039  1049  1051  1061
 1063  1069  1087  1091  1093  1097  1103  1109  1117  1123  1129  1151  1153
 1163  1171  1181  1187  1193  1201  1213  1217  1223  1229  1231  1237  1249
 1259  1277  1279  1283  1289  1291  1297  1301  1303  1307  1319  1321  1327
 1361  1367  1373  1381  1399  1409  1423  1427  1429  1433  1439  1447  1451
 1453  1459  1471  1481  1483  1487  1489  1493  1499  1511  1523  1531  1543
 1549  1553  1559  1567  1571  1579  1583  1597  1601 ^C

```



## F Sharp


```fsharp

(*
  Nigel Galloway April 7th., 2017.
*)
let SofE =
  let rec fg ng = seq{
    let n = Seq.item 0 ng
    yield n; yield! fg (Seq.cache(Seq.filter (fun g->g%n<>0) (Seq.skip 1 ng)))}
  fg (Seq.initInfinite(id)|>Seq.skip 2)

```

Let's print the sequence Prime[23] to Prime[42].
```fsharp

[23..42] |> Seq.iter(fun n->printf "%d " (Seq.item n SofE))

```


```txt

89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191

```



## Factor


```factor
USING: combinators kernel lists lists.lazy math math.functions
math.ranges prettyprint sequences ;

: prime? ( n -- ? )
    {
        { [ dup 2 < ] [ drop f ] }
        { [ dup even? ] [ 2 = ] }
        [ 3 over sqrt 2 <range> [ mod 0 > ] with all? ]
    } cond ;

! Create an infinite lazy list of primes.
: primes ( -- list ) 0 lfrom [ prime? ] lfilter ;

! Show the first fifteen primes.
15 primes ltake list>array .
```

```txt

{ 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 }

```



## FileMaker


```filemaker

(*
  Menno van Beek May 10th., 2018.
*)
Set Error Capture [On]
Allow User Abort [Off]
# Set default number of minutes
Set Variable [$maxduration; Value:1]
# Ask user for a desired duration of the test
Show Custom Dialog [	"Setup";
			"Enter the number of minutes (0,1-15, 6s increments) you would like this test to run.¶" &
			"Hit any of the modifier-keys until the result-dialog appears, when you wish to break off the test.";
			$maxduration ]
If [Get ( LastMessageChoice ) = 1]
	# Set all start-variables
	Set Variable 	[
			$result;
			Value:	Let ( [
					$start = Get ( CurrentTimeUTCMilliseconds ) ;
					x = Ceiling ( Abs ( 10 * $maxduration ) ) / 10 ;
					y = Case ( x < ,1 ; ,1 ; x > 15 ; 15 ; x ) ;
					$time = y * 60000 ; // 1 minute = 60000 milliseconds
					$number = 1 ;
					$primenumbers = 2 ;
					$duration = ""
				] ;
					""
				)]
	Loop
		# Increase each iteration by 2 (besides 2 there are no even prime numbers)
		# exit after duration is exceeded or when a modifier-key is actuated
		Exit Loop If [	Let ( [
					$number = $number + 2 ;
					$i = 1
				] ;
					$duration > $time or
					Get ( ActiveModifierKeys ) ≥ 1
				)]
		Loop
			# Loop until it is determined that a number is or isn't a prime number or the duration is exceeded
			# supplement $primenumbers each time one is found, update $duration each iteration
			Exit Loop If [	Let ( [
						$x = GetValue ( $primenumbers ; ValueCount ( $primenumbers ) - $i ) ;
						$d = If ( $x > 0 ; $number / $x ) ;
						$e = If ( Floor ( $d ) = $d ; $d ) ;
						$i = $i + 1 ;
						s = ( $x - 1 ) > $number/2 and $e = "" ;
						$primenumbers = If ( 	$x = "" or s ;
									List ( $primenumbers ; $number ) ;
									$primenumbers ) ;
						$duration = Get ( CurrentTimeUTCMilliseconds ) - $start
					] ;
						$x = "" or $e > 0 or s or $duration > $time
					)]
		End Loop
	End Loop
	# Count the number of primes found
	Set Variable 	[
			$result;
			Value:	Let ( [
					$n = ValueCount ( $primenumbers ) ;
					$$array = $primenumbers
				] ;
					""
				)]
	# Show results to user
	Show Custom Dialog [	"Result";
				List (
					"Prime numbers found: " & $n ;
					"Largest prime number: " & GetValue ( $primenumbers ; 1 ) ;
					"Duration of test (ms): " & $duration )]
End If
#

```



## Fortran

This version was written for an IBM1130, which offered 16-bit integers. Storage size was 32K words. It was written before the revised dogma that one is not a prime number became common. With punched-card input, using only capital letters was normal. This system offered Fortran IV which lacks the many later developments such as ''if ... then'' style statements, thus the profusion of statement labels and arithmetic if-statements. In ''if (expression) negative,zero,positive'' the sign of the expression is examined to choose the appropriate label to jump to. Labels can only be numbers, alas. There was also no MOD function for determining the remainder.

This routine does ''not'' attempt a calculation of sqrt(n), a time-consuming and also potentially inacurrate scheme. For instance, using Trunc(Log10(n)) + 1 to determine how many digits are needed to print ''n'' seems an obvious ad-hoc ploy, and gave 1 for ''n'' = 1 to 9, but it also gave 1 for ''n'' = 10, because Log10(10) was calculated as 0.9999964 or so (single precision on an IBM 390 system), which truncates to zero.

Instead, since many successive numbers are to be tested for primality, advantage can be taken of the fact that prime numbers only up to PRIME(LP) need be tried as factors all the way up to N = PRIME(LP + 1)**2 = XP2. This is similar to starting a pass through a Sieve of Eratoshenes at P*P rather than at 2*P. Thus, 5 is the largest factor to try, even beyond 5*5, all the way up to 49, because, if the number were divisible by 7, 7*2 would already have been checked because of 2, 7*3 because of 3, and so on. Only when 7*7 is needed will a further possible factor have to be tried. Likewise, although the last possible factor to try for N up to the integer limit of 32767 is 181 because the square of the next prime (191) exceeds 32767, in order for the method to be able to know this, the PRIME array must have space for this surplus prime. However, it does not know this properly because the square of 191 ''does'' exceed 32767 and so its value in XP2 will be incorrect, but this doesn't matter because only equality to XP2 is checked for and there will never be call to try 191 as a factor because 181 suffices up to the integer limit and the iteration will stop by then. Fortunately, 32767 is not divisible by three so that value will not be excluded as a possible candidate for N, and so the search can correctly end after inspecting the largest possible integer - finding it divisible by seven.

This method avoids considering multiples of two and three, leading to the need to pre-load array PRIME and print the first few values explicitly rather than flounder about with special startup tricks. Even so, in order not to pre-load with 7, and to correctly start the factor testing with 5, the first few primes are found with some wasted effort because 5 is not needed at the start. Storing the primes as found has the obvious advantage of enabling divisions only by prime numbers, but care with the startup is needed to ensure that primes have indeed been stored before they are called for.


```Fortran

CONCOCTED BY R.N.MCLEAN, APPLIED MATHS COURSE, AUCKLAND UNIVERSITY, MCMLXXI.
      INTEGER ENUFF,PRIME(44)
CALCULATION SHOWS PRIME(43) = 181, AND PRIME(44) = 191.
      INTEGER N,F,Q,XP2
      INTEGER INC,IP,LP,PP
      INTEGER ALINE(20),LL,I
      DATA ENUFF/44/
      DATA PP/4/
      DATA PRIME(1),PRIME(2),PRIME(3),PRIME(4)/1,2,3,5/
COPY THE KNOWN PRIMES TO THE OUTPUT LINE.
      DO 1 I = 1,PP
    1   ALINE(I) = PRIME(I)
      LL = PP
      LP = 3
      XP2 = PRIME(LP + 1)**2
      N = 5
      INC = 4
CONSIDER ANOTHER CANDIDATE. VIA INC, DODGE MULTIPLES OF 2 AND 3.
   10 INC = 6 - INC
      N = N + INC
      IF (N - XP2) 20,11,20
   11 LP = LP + 1
      XP2 = PRIME(LP + 1)**2
      GO TO 40
CHECK SUCCESSIVE PRIMES AS FACTORS, STARTING WITH PRIME(4) = 5.
   20 IP = 4
   21 F = PRIME(IP)
      Q = N/F
      IF (Q*F - N) 22,40,22
   22 IP = IP + 1
      IF (IP - LP) 21,21,30
CAUGHT ANOTHER PRIME.
   30 IF (PP - ENUFF) 31,32,32
   31 PP = PP + 1
      PRIME(PP) = N
   32 IF (LL - 20) 35,33,33
   33 WRITE (6,34) (ALINE(I), I = 1,LL)
   34 FORMAT (20I6)
      LL = 0
   35 LL = LL + 1
      ALINE(LL) = N
COMPLETED?
   40 IF (N - 32767) 10,41,41
   41 WRITE (6,34) (ALINE(I), I = 1,LL)
      END
```


Start of output:
     1     2     3     5     7    11    13    17    19    23    29    31    37    41    43    47    53    59    61    67
    71    73    79    83    89    97   101   103   107   109   113   127   131   137   139   149   151   157   163   167
   173   179   181   191   193   197   199   211   223   227   229   233   239   241   251   257   263   269   271   277
   281   283   293   307   311   313   317   331   337   347   349   353   359   367   373   379   383   389   397   401
   409   419   421   431   433   439   443   449   457   461   463   467   479   487   491   499   503   509   521   523
   541   547   557   563   569   571   577   587   593   599   601   607   613   617   619   631   641   643   647   653
   659   661   673   677   683   691   701   709   719   727   733   739   743   751   757   761   769   773   787   797
   809   811   821   823   827   829   839   853   857   859   863   877   881   883   887   907   911   919   929   937
   941   947   953   967   971   977   983   991   997  1009  1013  1019  1021  1031  1033  1039  1049  1051  1061  1063
  1069  1087  1091  1093  1097  1103  1109  1117  1123  1129  1151  1153  1163  1171  1181  1187  1193  1201  1213  1217
etc. and ends
 32423 32429 32441 32443 32467 32479 32491 32497 32503 32507 32531 32533 32537 32561 32563 32569 32573 32579 32587 32603
 32609 32611 32621 32633 32647 32653 32687 32693 32707 32713 32717 32719 32749


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function isPrime(n As Integer) As Boolean
  If n < 2 Then Return False
  If n = 2 Then Return True
  If n Mod 2  = 0 Then Return False
  Dim limit As Integer = Sqr(n)
  For i As Integer = 3 To limit Step 2
    If n Mod i = 0 Then Return False
  Next
  Return True
End Function

' Print all primes from 101 to 999
For i As Integer = 101 To 999
  If isPrime(i) Then
    Print Str(i); " ";
  End If
Next

Print : Print
Print "Press any key to quit"
Sleep
```


```txt

101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197
199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313
317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439
443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571
577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691
701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829
839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977
983 991 997

```



## Go

An unbounded cascading filtering method using channels, adapted from the classic concurrent prime sieve example in the "Try Go" window at http://golang.org/, improved by postponing the initiation of the filtering by a prime until the prime's square is seen in the input.

```go
package main

import "fmt"

func NumsFromBy(from int, by int, ch chan<- int) {
  for i := from; ; i+=by {
    ch <- i
  }
}

func Filter(in <-chan int, out chan<- int, prime int) {
  for {
    i := <-in
    if i%prime != 0 {            // here is the trial division
      out <- i
    }
  }
}

func Sieve(out chan<- int) {
  out <- 3
  q := 9
  ps := make(chan int)
  go Sieve(ps)                   // separate primes supply
  p := <-ps
  nums := make(chan int)
  go NumsFromBy(5,2,nums)        // end of setup
  for i := 0; ; i++ {
    n := <-nums
    if n < q {
    	out <- n                 // n is prime
    } else {
        ch1 := make(chan int)    // n == q == p*p
        go Filter(nums, ch1, p)  // creation of a filter by p, at p*p
        nums = ch1
    	p = <-ps                 // next prime
    	q = p*p                  //   and its square
    }
  }
}

func primes (c chan<- int) {
  c <- 2
  go Sieve(c)
}

func main() {
  ch := make(chan int)
  go primes(ch)
  fmt.Print("First twenty:")
  for i := 0; i < 20; i++ {
    fmt.Print(" ", <-ch)
  }
  fmt.Println()
}
```

```txt

First twenty: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71

```

A simple iterative method, also unbounded and starting with 2.

```go
package main

import "fmt"

func newP() func() int {
    n := 1
    return func() int {
        for {
            n++
            // Trial division as naïvely as possible.  For a candidate n,
            // numbers between 1 and n are checked to see if they divide n.
            // If no number divides n, n is prime.
            for f := 2; ; f++ {
                if f == n {
                    return n
                }
                if n%f == 0 { // here is the trial division
                    break
                }
            }
        }
    }
}

func main() {
    p := newP()
    fmt.Print("First twenty:")
    for i := 0; i < 20; i++ {
        fmt.Print(" ", p())
    }
    fmt.Println()
}
```

```txt

First twenty: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71

```



## Haskell

The most basic:

```haskell
[n | n <- [2..], []==[i | i <- [2..n-1], rem n i == 0]]
```


With trial division emulated by additions (the seeds of Sieve):

```haskell
[n | n <- [2..], []==[i | i <- [2..n-1], j <- [i,i+i..n], j==n]]
```


With recursive filtering (in wrong order, from bigger to smaller natural numbers):

```haskell
foldr (\x r -> x : filter ((> 0).(`rem` x)) r) [] [2..]
```


With iterated sieving (in right order, from smaller to bigger primes):

```haskell
Data.List.unfoldr (\(x:xs) -> Just (x, filter ((> 0).(`rem` x)) xs)) [2..]
```


A proper [[Primality by trial division#Haskell|primality testing by trial division]] can be used to produce short ranges of primes more efficiently:

```haskell
primesFromTo n m = filter isPrime [n..m]
```


The standard optimal trial division version has <code>isPrime</code> in the above inlined:


```haskell
-- primes = filter isPrime [2..]
primes = 2 : [n | n <- [3..], foldr (\p r-> p*p > n || rem n p > 0 && r)
                                    True primes]
```


It is easy to amend this to test only odd numbers by only odd primes, or automatically skip the multiples of ''3'' (also, ''5'', etc.) by construction as well (a ''wheel factorization'' technique):


```haskell
primes = 2 : 3 : [n | n <- [5,7..], foldr (\p r-> p*p > n || rem n p > 0 && r)
                                          True (drop 1 primes)]
       = [2,3,5] ++ [n | n <- scanl (+) 7 (cycle [4,2]),
                                     foldr (\p r-> p*p > n || rem n p > 0 && r)
                                           True (drop 2 primes)]
    -- = [2,3,5,7] ++ [n | n <- scanl (+) 11 (cycle [2,4,2,4,6,2,6,4]), ... (drop 3 primes)]
```



### Sieve by trial division

The classic David Turner's 1983 (1976? 1975?) SASL code repeatedly ''sieves'' a stream of candidate numbers from those divisible by a prime at a time, and works even for unbounded streams, thanks to lazy evaluation:

```haskell
primesT = sieve [2..]
          where
          sieve (p:xs) = p : sieve [x | x <- xs, rem x p /= 0]
-- map head
--  . iterate (\(p:xs) -> filter ((> 0).(`rem` p)) xs) $ [2..]
```


As shown in Melissa O'Neill's paper [http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf "The Genuine Sieve of Eratosthenes"], its complexity is quadratic in number of primes produced whereas that of optimal trial division is <math>O(n^{1.5}/(\log n)^{0.5})</math>, and of true SoE it is <math>O(n\log n\log\log n)</math>, in ''n'' primes produced.

Indeed as Eratosthenes sieve works by counting, ''its'' removal step could be prototyped as <code>(\(p:xs)-> minus xs [p,p+p..])</code>, where <code>minus xs ys == xs Data.List.\\ ys</code> for any finite and increasing ''xs'' and ''ys''.


### Bounded sieve by trial division

Bounded formulation has normal trial division complexity, because it can stop early via an explicit guard:

```haskell
primesTo m = sieve [2..m]
   where
   sieve (p:xs) | p*p > m   = p : xs
                | otherwise = p : sieve [x | x <- xs, rem x p /= 0]
-- (\(a,b:_) -> map head a ++ b) . span ((< m).(^2).head)
--   $ iterate (\(p:xs) -> filter ((>0).(`rem`p)) xs) [2..m]
```



### Postponed sieve by trial division

To make it unbounded, the guard cannot be simply discarded. The firing up of a filter by a prime should be ''postponed'' until its ''square'' is seen amongst the candidates (so a bigger chunk of input numbers are taken straight away as primes, between each opening up of a new filter, instead of just one head element in the non-postponed algorithm):

```haskell
primesPT = sieve primesPT [2..]
           where
           sieve ~(p:ps) (x:xs) = x : after (p*p) xs
                                        (sieve ps . filter ((> 0).(`rem` p)))
           after q (x:xs) f | x < q = x : after q xs f
                            | otherwise = f (x:xs)
-- fix $ concatMap (fst.fst) . iterate (\((_,t),p:ps) ->
--         (span (< head ps^2) [x | x <- t, rem x p > 0], ps)) . (,) ([2,3],[4..])
```


<code>~(p:ps)</code> is a lazy pattern: the matching will be delayed until any of its variables are actually needed. Here it means that on the very first iteration the head of <code>primesPT</code> will be safely accessed only after it is already defined (by <code>x : after (p*p) ...</code>).


### Segmented Generate and Test

Explicating the run-time list of ''filters'' (created implicitly by the sieves above) as a list of ''factors to test by'' on each segment between the consecutive squares of primes (so that no testing is done prematurely), and rearranging to avoid recalculations, leads to the following:

```haskell
import Data.List (inits)

primesST = 2 : 3 : sieve 5 9 (drop 2 primesST) (inits $ tail primesST)
   where
   sieve x q ps (fs:ft) = filter (\y-> all ((/=0).rem y) fs) [x,x+2..q-2]
                          ++ sieve (q+2) (head ps^2) (tail ps) ft
```

<code>inits</code> makes a stream of (progressively growing) prefixes of an input stream, starting with an empty prefix, here making the <code>fs</code> parameter to get a sequence of values <code>[], [3], [3,5], ...</code>.

Runs at empirical <math>O(n^{1.4...})</math> time complexity, in ''n'' primes produced. Can be used as a framework for unbounded segmented sieves, replacing divisibility testing with proper sieve of Eratosthenes on arrays, etc.

The filtering function is equivalent to <code>noDivsBy</code> [[Primality by trial division#Haskell|defined as part of <code>isPrime</code> function]], except that the comparisons testing for the square root are no longer needed and so are spared.


## J

Implementation:

```J
primTrial=:3 :0
  try=. i.&.(p:inv) %: >./ y
  candidate=. (y>1)*y=<.y
  y #~ candidate*(y e.try) = +/ 0= try|/ y
)
```


Example use:


```J
   primTrial 1e6+i.100
1000003 1000033 1000037 1000039 1000081 1000099
```


Note that this is a filter - it selects values from its argument which are prime. If no suitable values are found the resulting sequence of primes will be empty.

Note: if you instead want an iterator, 4&p: y returns the next prime after y.

See also: [[Sieve_of_Eratosthenes#J|Sieve of Eratosthenes]]


## Java

```java
import java.util.stream.IntStream;

public class Test {

    static IntStream getPrimes(int start, int end) {
        return IntStream.rangeClosed(start, end).filter(n -> isPrime(n));
    }

    public static boolean isPrime(long x) {
        if (x < 3 || x % 2 == 0)
            return x == 2;

        long max = (long) Math.sqrt(x);
        for (long n = 3; n <= max; n += 2) {
            if (x % n == 0) {
                return false;
            }
        }
        return true;
    }

    public static void main(String[] args) {
        getPrimes(0, 100).forEach(p -> System.out.printf("%d, ", p));
    }
}
```



```txt
2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97,
```



## Julia

I've chosen to solve this task by creating a new iterator type, <tt>TDPrimes</tt>.  <tt>TDPrimes</tt> contains the upper limit of the sequence.  The iteration state is the list of computed primes, and the item returned with each iteration is the current prime.  The core of the solution is the <tt>next</tt> method for <tt>TDPrimes</tt>, which computes the next prime by trial division of the previously determined primes contained in the iteration state.


```julia
struct TDPrimes{T<:Integer}
    uplim::T
end

Base.start{T<:Integer}(pl::TDPrimes{T}) = 2ones(T, 1)
Base.done{T<:Integer}(pl::TDPrimes{T}, p::Vector{T}) = p[end] > pl.uplim
function Base.next{T<:Integer}(pl::TDPrimes{T}, p::Vector{T})
    pr = npr = p[end]
    ispr = false
    while !ispr
        npr += 1
        ispr = all(npr % d != 0 for d in p)
    end
    push!(p, npr)
    return pr, p
end

println("Primes ≤ 100: ", join((p for p in TDPrimes(100)), ", "))
```


```txt
Primes ≤ 100: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
```



## jq

This entry uses is_prime/0 as defined at [[Primality_by_trial_division#jq]].

```jq
# Produce a (possibly empty) stream of primes in the range [m,n], i.e. m <= p <= n
def primes(m; n):
  ([m,2] | max) as $m
  | if $m > n then empty
    elif $m == 2 then 2, primes(3;n)
    else (1 + (2 * range($m/2 | floor; (n + 1) /2 | floor))) | select( is_prime )
    end;
```


'''Examples:'''

```jq
primes(0;10)
```


```sh
2
3
5
7
```

Produce an array of primes, p, satisfying 50 <= p <= 99:

```jq
[primes(50;99)]
```

 [53,59,61,67,71,73,79,83,89,97]


## Kotlin


```scala
// version 1.0.6

fun isPrime(n: Int): Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d : Int = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}

fun main(args: Array<String>) {
    // print all primes below 2000 say
    var count = 1
    print("    2")
    for (i in 3..1999 step 2)
        if (isPrime(i)) {
            count++
            print("%5d".format(i))
            if (count % 15 == 0) println()
        }
}
```


```txt

    2    3    5    7   11   13   17   19   23   29   31   37   41   43   47
   53   59   61   67   71   73   79   83   89   97  101  103  107  109  113
  127  131  137  139  149  151  157  163  167  173  179  181  191  193  197
  199  211  223  227  229  233  239  241  251  257  263  269  271  277  281
  283  293  307  311  313  317  331  337  347  349  353  359  367  373  379
  383  389  397  401  409  419  421  431  433  439  443  449  457  461  463
  467  479  487  491  499  503  509  521  523  541  547  557  563  569  571
  577  587  593  599  601  607  613  617  619  631  641  643  647  653  659
  661  673  677  683  691  701  709  719  727  733  739  743  751  757  761
  769  773  787  797  809  811  821  823  827  829  839  853  857  859  863
  877  881  883  887  907  911  919  929  937  941  947  953  967  971  977
  983  991  997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069
 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153 1163 1171 1181 1187
 1193 1201 1213 1217 1223 1229 1231 1237 1249 1259 1277 1279 1283 1289 1291
 1297 1301 1303 1307 1319 1321 1327 1361 1367 1373 1381 1399 1409 1423 1427
 1429 1433 1439 1447 1451 1453 1459 1471 1481 1483 1487 1489 1493 1499 1511
 1523 1531 1543 1549 1553 1559 1567 1571 1579 1583 1597 1601 1607 1609 1613
 1619 1621 1627 1637 1657 1663 1667 1669 1693 1697 1699 1709 1721 1723 1733
 1741 1747 1753 1759 1777 1783 1787 1789 1801 1811 1823 1831 1847 1861 1867
 1871 1873 1877 1879 1889 1901 1907 1913 1931 1933 1949 1951 1973 1979 1987
 1993 1997 1999

```



## Lambdatalk



```scheme

{def prime
 {def prime.rec
  {lambda {:m :n}
   {if {> {* :m :m} :n}
    then :n
    else {if {= {% :n :m} 0}
          then
          else {prime.rec {+ :m 1} :n} }}}}
 {lambda {:n}
  {prime.rec 2 :n} }}

{map prime {serie 3 100 2}}
-> 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

{map prime {serie 9901 10000 2}}
-> 9901 9907 9923 9929 9931 9941 9949 9967 9973

```

More to see in [http://epsilonwiki.free.fr/lambdaway/?view=primes2]


## Liberty BASIC


```lb

print "Rosetta Code - Sequence of primes by trial division"
print: print "Prime numbers between 1 and 50"
for x=1 to 50
    if isPrime(x) then print x
next x
[start]
input "Enter an integer: "; x
if x=0 then print "Program complete.": end
if isPrime(x) then print x; " is prime" else print x; " is not prime"
goto [start]

function isPrime(p)
    p=int(abs(p))
    if p=2 or then isPrime=1: exit function 'prime
    if p=0 or p=1 or (p mod 2)=0 then exit function 'not prime
    for i=3 to sqr(p) step 2
        if (p mod i)=0 then exit function 'not prime
    next i
    isPrime=1
end function

```

```txt

Rosetta Code - Primality by trial division

Prime numbers between 1 and 50
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
37
41
43
47
Enter an integer: 1
1 is not prime
Enter an integer: 2
2 is prime
Enter an integer:
Program complete.

```



## Lua


```Lua
-- Returns true if x is prime, and false otherwise
function isprime (x)
    if x < 2 then return false end
    if x < 4 then return true end
    if x % 2 == 0 then return false end
    for d = 3, math.sqrt(x), 2 do
        if x % d == 0 then return false end
    end
    return true
end

-- Returns table of prime numbers (from lo, if specified) up to hi
function primes (lo, hi)
    local t = {}
    if not hi then
        hi = lo
        lo = 2
    end
    for n = lo, hi do
        if isprime(n) then table.insert(t, n) end
    end
    return t
end

-- Show all the values of a table in one line
function show (x)
    for _, v in pairs(x) do io.write(v .. " ") end
    print()
end

-- Main procedure
show(primes(100))
show(primes(50, 150))
```

```txt
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149
```



## MATLAB


```MATLAB
function primeList = sieveOfEratosthenes(lastNumber)

    list = (2:lastNumber); %Construct list of numbers
    primeList = []; %Preallocate prime list

    while( list(1)^2 <lastNumber )

        primeList = [primeList list(1)]; %add prime to the prime list
        list( mod(list,list(1))==0 ) = []; %filter out all multiples of the current prime

    end

    primeList = [primeList list]; %The rest of the numbers in the list are primes

end
```
 sieveOfEratosthenes(30)

 ans =

     2     3     5     7    11    13    17    19    23    29


## Oforth


isPrime function is from Primality by trial division page


```Oforth
: primeSeq(n)  n seq filter(#isPrime) ;
```



## PARI/GP


```parigp
trial(n)={
  if(n < 4, return(n > 1)); /* Handle negatives */
  forprime(p=2,sqrt(n),
    if(n%p == 0, return(0))
  );
  1
};

select(trial, [1..100])
```



## Pascal

Hiding the work in a existing unit.

```Pascal

program PrimeRng;
uses
  primTrial;
var
  Range : ptPrimeList;
  i : integer;
Begin
  Range := PrimeRange(1000*1000*1000,1000*1000*1000+100);
  For i := Low(Range) to High(Range) do
    write(Range[i]:12);
  writeln;
end.
```

;output:

```txt
  1000000007  1000000009  1000000021  1000000033  1000000087  1000000093  1000000097
```



## Perl


```perl
sub isprime {
  my $n = shift;
  return ($n >= 2) if $n < 4;
  return unless $n % 2 && $n % 3;
  my $sqrtn = int(sqrt($n));
  for (my $i = 5; $i <= $sqrtn; $i += 6) {
    return unless $n % $i && $n % ($i+2);
  }
  1;
}

print join(" ", grep { isprime($_) } 0 .. 100 ), "\n";
print join(" ", grep { isprime($_) } 12345678 .. 12345678+100 ), "\n";
```

```txt
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
12345701 12345709 12345713 12345727 12345731 12345743 12345769
```



## Perl 6

Here is a straightforward implementation of the naive algorithm.

```perl6
constant @primes = 2, 3, { first * %% none(@_), (@_[* - 1], * + 2 ... *) } ... *;

say @primes[^100];
```

```txt
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541
```



## Phix

using is_prime from [[Primality_by_trial_division#Phix]]

```Phix
sequence s= {}
for i=0 to 100 do
    if is_prime(i) then s&=i end if
end for
?s
```

```txt

{2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97}

```



## PicoLisp


```PicoLisp
(de prime? (N)
   (or
      (= N 2)
      (and
         (> N 1)
         (bit? 1 N)
         (let S (sqrt N)
            (for (D 3  T  (+ D 2))
               (T (> D S) T)
               (T (=0 (% N D)) NIL) ) ) ) ) )

(de primeseq (A B)
   (filter prime? (range A B)) )

(println (primeseq 50 99))
```

```txt
(53 59 61 67 71 73 79 83 89 97)
```



## PowerShell


```PowerShell

function eratosthenes ($n) {
    if($n -ge 1){
        $prime = @(1..($n+1) | foreach{$true})
        $prime[1] = $false
        $m = [Math]::Floor([Math]::Sqrt($n))
        for($i = 2; $i -le $m; $i++) {
            if($prime[$i]) {
                for($j = $i*$i; $j -le $n; $j += $i) {
                    $prime[$j] = $false
                }
            }
        }
        1..$n | where{$prime[$_]}
    } else {
        "$n must be equal or greater than 1"
    }
}
function sieve-start-end ($start,$end) {
    (eratosthenes $end) | where{$_ -ge $start}

}
"$(sieve-start-end 100 200)"

```

<b>Output:</b>

```txt

101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199

```



## PureBasic


```PureBasic
EnableExplicit
#SPC=Chr(32)
#TB=~"\t"
#TBLF=~"\t\n"
Define.i a,b,l,n,count=0
Define *count.Integer=@count

Procedure.i AddCount(*c.Integer) ; *counter: by Ref
  *c\i+1
  ProcedureReturn *c\i
EndProcedure

Procedure.s FormatStr(tx$,l.i)
  Shared *count
  If AddCount(*count)%10=0
    ProcedureReturn RSet(tx$,l,#SPC)+#TBLF
  Else
    ProcedureReturn RSet(tx$,l,#SPC)+#TB
  EndIf
EndProcedure

Procedure.b Trial(n.i)
  Define.i i
  For i=3 To Int(Sqr(n)) Step 2
    If n%i=0 : ProcedureReturn #False : EndIf
  Next
  ProcedureReturn #True
EndProcedure

Procedure.b isPrime(n.i)
  If (n>1 And n%2<>0 And Trial(n)) Or n=2 : ProcedureReturn #True : EndIf
  ProcedureReturn #False
EndProcedure

OpenConsole("Sequence of primes by Trial Division")
PrintN("Input (n1<n2 & n1>0)")
Print("n1 : ") : a=Int(Val(Input()))
Print("n2 : ") : b=Int(Val(Input()))
l=Len(Str(b))
If a<b And a>0
  PrintN(~"\nPrime numbers between "+Str(a)+" and "+Str(b))
  For n=a To b
    If isPrime(n)
      Print(FormatStr(Str(n),l))
    EndIf
  Next
  Print(~"\nPrimes= "+Str(*count\i))
  Input()
EndIf
```

```txt
Input (n1<n2 & n1>0)
n1 : 10000
n2 : 11000

Prime numbers between 10000 and 11000
10007   10009   10037   10039   10061   10067   10069   10079   10091   10093
10099   10103   10111   10133   10139   10141   10151   10159   10163   10169
10177   10181   10193   10211   10223   10243   10247   10253   10259   10267
10271   10273   10289   10301   10303   10313   10321   10331   10333   10337
10343   10357   10369   10391   10399   10427   10429   10433   10453   10457
10459   10463   10477   10487   10499   10501   10513   10529   10531   10559
10567   10589   10597   10601   10607   10613   10627   10631   10639   10651
10657   10663   10667   10687   10691   10709   10711   10723   10729   10733
10739   10753   10771   10781   10789   10799   10831   10837   10847   10853
10859   10861   10867   10883   10889   10891   10903   10909   10937   10939
10949   10957   10973   10979   10987   10993
Primes= 106
```



## Python

Using the basic ''prime()'' function from: [http://rosettacode.org/wiki/Primality_by_trial_division#Python "Primality by trial division"]

```Python

def prime(a):
    return not (a < 2 or any(a % x == 0 for x in xrange(2, int(a**0.5) + 1)))

def primes_below(n):
    return [i for i in range(n) if prime(i)]

```

```txt
>>> primes_below(100)
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
```



## Racket


Infinite list of primes:


###  Using laziness


This example uses infinite lists (streams) to implement a sieve algorithm that produces all prime numbers.


```Racket
#lang lazy
(define nats (cons 1 (map add1 nats)))
(define (sift n l) (filter (λ(x) (not (zero? (modulo x n)))) l))
(define (sieve l) (cons (first l) (sieve (sift (first l) (rest l)))))
(define primes (sieve (rest nats)))
(!! (take 25 primes))
```



### = Optimized with postponed processing =


Since a prime's multiples that count start from its square, we should only add them when we reach that square.


```Racket
#lang lazy
(define nats (cons 1 (map add1 nats)))
(define (sift n l) (filter (λ(x) (not (zero? (modulo x n)))) l))
(define (when-bigger n l f)
  (if (< (car l) n) (cons (car l) (when-bigger n (cdr l) f)) (f l)))
(define (sieve l ps)
  (cons (car l) (when-bigger (* (car ps) (car ps)) (cdr l)
                             (λ(t) (sieve (sift (car ps) t) (cdr ps))))))
(define primes (sieve (cdr nats) primes))
(!! (take 25 primes))
```



###  Using threads and channels


Same algorithm as above, but now using threads and channels to produce a channel of all prime numbers (similar to newsqueak).  The macro at the top is a convenient wrapper around definitions of channels using a thread that feeds them.


```Racket
#lang racket
(define-syntax (define-thread-loop stx)
  (syntax-case stx ()
    [(_ (name . args) expr ...)
     (with-syntax ([out! (datum->syntax stx 'out!)])
       #'(define (name . args)
           (define out (make-channel))
           (define (out! x) (channel-put out x))
           (thread (λ() (let loop () expr ... (loop))))
           out))]))
(define-thread-loop (nats) (for ([i (in-naturals 1)]) (out! i)))
(define-thread-loop (filter pred? c)
  (let ([x (channel-get c)]) (when (pred? x) (out! x))))
(define (sift n c) (filter (λ(x) (not (zero? (modulo x n)))) c))
(define-thread-loop (sieve c)
  (let ([x (channel-get c)]) (out! x) (set! c (sift x c))))
(define primes (let ([ns (nats)]) (channel-get ns) (sieve ns)))
(for/list ([i 25] [x (in-producer (λ() (channel-get primes)))]) x)
```



###  Using generators


Yet another variation of the same algorithm as above, this time using generators.


```Racket
#lang racket
(require racket/generator)
(define nats (generator () (for ([i (in-naturals 1)]) (yield i))))
(define (filter pred g)
  (generator () (for ([i (in-producer g #f)] #:when (pred i)) (yield i))))
(define (sift n g) (filter (λ(x) (not (zero? (modulo x n)))) g))
(define (sieve g)
  (generator () (let loop ([g g]) (let ([x (g)]) (yield x) (loop (sift x g))))))
(define primes (begin (nats) (sieve nats)))
(for/list ([i 25] [x (in-producer primes)]) x)
```



## REXX


### somewhat optimized

This is an open-ended approach and it's a simple implementation and could be optimized more with some easy programming.

The method used is to divided all odd numbers by all previous odd primes up to and including the   <big>'''√{{overline|  }}'''</big>   of the odd number.

Usage note:   by using a negative number (for the program's argument), the list of primes is suppressed, but the prime count is still shown.

```rexx
/*REXX program lists a  sequence of primes  by  testing  primality  by  trial division. */
parse arg n .                                    /*get optional number of primes to find*/
if n=='' | n==","  then n= 26                    /*Not specified?  Then use the default.*/
tell= (n>0);            n= abs(n)                /*Is  N  negative?  Then don't display.*/
@.1=2;     if tell  then say right(@.1, 9)       /*display  2  as a special prime case. */
#=1                                              /*#  is number of primes found (so far)*/
                                                 /* [↑]  N:  default lists up to 101 #s.*/
   do j=3  by 2  while  #<n                      /*start with the first odd prime.      */
                                                 /* [↓]  divide by the primes.   ___    */
          do k=2  to #  while  !.k<=j            /*divide  J  with all primes ≤ √ J     */
          if j//@.k==0  then iterate j           /*÷ by prev. prime?  ¬prime     ___    */
          end   /*j*/                            /* [↑]   only divide up to     √ J     */
   #= #+1                                        /*bump the count of number of primes.  */
   @.#= j;           !.#= j*j                    /*define this prime; define its square.*/
   if tell  then say right(j, 9)                 /*maybe display this prime ──► terminal*/
   end   /*j*/                                   /* [↑]  only display N number of primes*/
                                                 /* [↓]  display number of primes found.*/
say  #       ' primes found.'                    /*stick a fork in it,  we're all done. */
```

```txt

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
       37
       41
       43
       47
       53
       59
       61
       67
       71
       73
       79
       83
       89
       97
      101
26  primes found.

```



### more optimized

This version shows how the REXX program may be optimized further by extending the list of low primes and

the special low prime divisions   (the   <big>'''//'''</big>   tests,   which is the   ''remainder''   when doing integer division).

```rexx
/*REXX program lists a  sequence of primes  by testing  primality  by  trial division.  */
parse arg N .                                    /*get optional number of primes to find*/
if N=='' | N==","  then N= 26                    /*Not specified?   Then assume default.*/
tell= (N>0);            N= abs(N)                /*N is negative?   Then don't display. */
@.1=2;   @.2=3;   @.3=5;   @.4=7;   @.5=11;   @.6=13;        #= 5;         s= @.# + 2
                                                 /*    [↑]  is the number of low primes.*/
      do p=1  for #   while  p<=N                /* [↓]  find primes, and maybe show 'em*/
      if tell  then say right(@.p, 9)            /*display some pre─defined low primes. */
      !.p= @.p**2                                /*also compute the squared value of P. */
      end   /*p*/                                /* [↑]  allows faster loop (below).    */
                                                 /* [↓]  N:  default lists up to 101 #s.*/
   do j=s  by 2  while  #<N                      /*continue on with the next odd prime. */
   if j // 3 ==0  then iterate                   /*is this integer a multiple of three? */
   parse var  j  ''  -1  _                       /*obtain the last digit of the  J  var.*/
   if _      ==5  then iterate                   /*is this integer a multiple of five?  */
   if j // 7 ==0  then iterate                   /* "   "     "    "     "     " seven? */
   if j //11 ==0  then iterate                   /* "   "     "    "     "     " eleven?*/
                                                 /* [↓]  divide by the primes.   ___    */
          do k=p  to #  while  !.k<=j            /*divide  J  by other primes ≤ √ J     */
          if j//@.k ==0   then iterate j         /*÷ by prev. prime?  ¬prime     ___    */
          end   /*k*/                            /* [↑]   only divide up to     √ J     */
   #= #+1                                        /*bump the count of number of primes.  */
   @.#= j;           !.#= j*j                    /*define this prime; define its square.*/
   if tell  then say right(j, 9)                 /*maybe display this prime ──► terminal*/
   end   /*j*/                                   /* [↑]  only display N number of primes*/
                                                 /* [↓]  display number of primes found.*/
say  #       ' primes found.'                    /*stick a fork in it,  we're all done. */
```

## Ring


```ring

for i = 1 to 100
    if isPrime(i) see "" + i + " " ok
next
see nl

func isPrime n
     if n < 2 return false ok
     if n < 4 return true ok
     if n % 2 = 0 return false ok
     for d = 3 to sqrt(n) step 2
         if n % d = 0 return false ok
     next
     return true

```



## Ruby

The Prime class in the standard library has several Prime generators. In some methods it can be specified which generator will be used. The generator can be used on it's own:

```ruby
require "prime"

pg = Prime::TrialDivisionGenerator.new
p pg.take(10) # => [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
p pg.next # => 31
```



## Scala

===Odds-Only "infinite" primes generator using Streams and Co-Inductive Streams===
Using Streams, [http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf the "unfaithful sieve"], i.e. '''sub-optimal trial division sieve'''.

```scala
def sieve(nums: Stream[Int]): Stream[Int] =
    Stream.cons(nums.head, sieve((nums.tail).filter(_ % nums.head != 0)))
  val primes = 2 #:: sieve(Stream.from(3, 2))

  println(primes take 10 toList) //         //List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
  println(primes takeWhile (_ < 30) toList) //List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
```

{{out}}Both println statements give the same results:

```txt
List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
```


The above code is extremely inefficient for larger ranges, both because it tests for primality using computationally expensive divide (modulo) operations and because it sets up deferred tests for division by all of the primes up to each prime candidate, meaning that it has approximately a square law computational complexity with range.


## Sidef

Using the ''is_prime()'' function from: [http://rosettacode.org/wiki/Primality_by_trial_division#Sidef "Primality by trial division"]

```ruby
func prime_seq(amount, callback) {
    var (counter, number) = (0, 0);
    while (counter < amount) {
        if (is_prime(number)) {
            callback(number);
            ++counter;
        }
        ++number;
    }
}

prime_seq(100, {|p| say p});     # prints the first 100 primes
```



## Spin

This example uses totally naive looping over test divisors <code>d</code> of <code>n</code> up to <code>n-1</code> until a divisor is found or the range is exhausted. This results in <code>d == n</code> after the loop if <code>n</code> is prime.
```spin
con
  _clkmode = xtal1+pll16x
  _clkfreq = 80_000_000

obj
  ser : "FullDuplexSerial"

pub main | d, n

  ser.start(31, 30, 0, 115200)

  repeat n from 2 to 100

    repeat d from 2 to n-1
      if n // d == 0
        quit

    if d == n
      ser.dec(n)
      ser.tx(32)

  waitcnt(_clkfreq + cnt)
  ser.stop
```

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```



## Swift


```swift
import Foundation

extension SequenceType {
  func takeWhile(include: Generator.Element -> Bool) -> AnyGenerator<Generator.Element> {
    var g = self.generate()
    return anyGenerator { g.next().flatMap{include($0) ? $0 : nil }}
  }
}

var pastPrimes = [2]

var primes = anyGenerator {
  _ -> Int? in
  defer {
    pastPrimes.append(pastPrimes.last!)
    let c = pastPrimes.count - 1
    for p in anyGenerator({++pastPrimes[c]}) {
      let lim = Int(sqrt(Double(p)))
      if (!pastPrimes.takeWhile{$0 <= lim}.contains{p % $0 == 0}) { break }
    }
  }
  return pastPrimes.last
}
```


### Simple version

```swift
var primes = [2]

func trialPrimes(_ max:Int){
// fill array 'primes' with primes <= max, 1s for small values like 400_000
    var cand = 3
    while cand <= max {
        for p in primes {
            if cand % p == 0 {
                break
            }
            if p*p > cand {
                primes.append(cand)
                break
            }
        }
        cand += 2
    }
}

trialPrimes(100)
print(primes)
```

```txt

[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

```



## Tcl

As we're generating a sequence of primes, we can use that sequence of primes to describe what we're filtering against.

```tcl
set primes {}
proc havePrime n {
    global primes
    foreach p $primes {
	# Do the test-by-trial-division
	if {$n/$p*$p == $n} {return false}
    }
    return true
}
for {set n 2} {$n < 100} {incr n} {
    if {[havePrime $n]} {
	lappend primes $n
	puts -nonewline "$n "
    }
}
puts ""
```

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```



## zkl

The code in [[Extensible prime generator#zkl]] is a much better solution to this problem.
```zkl
fcn isPrime(p){
   (p>=2) and (not [2 .. p.toFloat().sqrt()].filter1('wrap(n){ p%n==0 }))
}
fcn primesBelow(n){ [0..n].filter(isPrime) }
```

The Method filter1 stops at the first non False result, which, if there is one, is the first found diviser, thus short cutting the rest of the test.

```zkl
primesBelow(100).toString(*).println();
```

```txt

L(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97)

```

