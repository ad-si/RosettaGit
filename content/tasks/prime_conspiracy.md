+++
title = "Prime conspiracy"
description = ""
date = 2019-10-20T16:40:07Z
aliases = []
[extra]
id = 20623
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
languages = [
  "algol_68",
  "c",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "elixir",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "j",
  "java",
  "julia",
  "kotlin",
  "lua",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "seed7",
  "sidef",
  "vba",
  "zkl",
]
+++

A recent discovery, quoted from   [https://www.quantamagazine.org/20160313-mathematicians-discover-prime-conspiracy/ Quantamagazine]   (March 13, 2016):
     '' Two mathematicians have uncovered a simple, previously unnoticed property of ''
     '' prime numbers — those numbers that are divisible only by 1 and themselves.   ''
     '' Prime numbers, it seems, have decided preferences about the final digits of  ''
     '' the primes that immediately follow them.
and
     '' This conspiracy among prime numbers seems, at first glance, to violate a     ''
     '' longstanding assumption in number theory:  that prime numbers behave much    ''
     '' like random numbers.

     ''                        ─── (original authors from Stanford University):      ''
     ''                        ─── Kannan Soundararajan  and  Robert Lemke Oliver    ''


The task is to check this assertion, modulo 10.

Lets call   <big><code> i -> j </code></big>   a transition if   <big><code> i </code></big>   is the last decimal digit of a prime, and   <big><code> j </code></big>   the last decimal digit of the following prime.


## Task

Considering the first one million primes.   Count, for any pair of successive primes, the number of transitions   <big><code> i -> j </code></big>   and print them along with their relative frequency, sorted by   <big><code> i </code>.</big>

You can see that, for a given   <big><code> i </code>,</big>   frequencies are not evenly distributed.


;Observation:
(Modulo 10),    primes whose last digit is   '''9'''   "prefer"   the digit   '''1'''   to the digit   '''9''',   as its following prime.


;Extra credit:
Do the same for one hundred million primes.


;Example for 10,000 primes:

```txt

10000 first primes. Transitions prime % 10 → next-prime % 10.
1 → 1 count:        365 frequency: 3.65 %
1 → 3 count:        833 frequency: 8.33 %
1 → 7 count:        889 frequency: 8.89 %
1 → 9 count:        397 frequency: 3.97 %
2 → 3 count:          1 frequency: 0.01 %
3 → 1 count:        529 frequency: 5.29 %
3 → 3 count:        324 frequency: 3.24 %
3 → 5 count:          1 frequency: 0.01 %
3 → 7 count:        754 frequency: 7.54 %
3 → 9 count:        907 frequency: 9.07 %
5 → 7 count:          1 frequency: 0.01 %
7 → 1 count:        655 frequency: 6.55 %
7 → 3 count:        722 frequency: 7.22 %
7 → 7 count:        323 frequency: 3.23 %
7 → 9 count:        808 frequency: 8.08 %
9 → 1 count:        935 frequency: 9.35 %
9 → 3 count:        635 frequency: 6.35 %
9 → 7 count:        541 frequency: 5.41 %
9 → 9 count:        379 frequency: 3.79 %

```






## ALGOL 68

Solves the basic task (1 000 000 primes) using the standard sieve of Eratosthanes.
The sieve is represented by an array of BITS (32-bit items in Algol 68G).

```algol68
# extend SET, CLEAR and ELEM to operate on rows of BITS #
OP   SET   = ( INT n, REF[]BITS b )REF[]BITS:
     BEGIN
         INT w = n OVER bits width;
         b[ w ] := ( ( 1 + ( n MOD bits width ) ) SET b[ w ] );
         b
     END # SET # ;
OP   CLEAR = ( INT n, REF[]BITS b )REF[]BITS:
     BEGIN
         INT w = n OVER bits width;
         b[ w ] := ( ( 1 + ( n MOD bits width ) ) CLEAR b[ w ] );
         b
     END # SET # ;
OP   ELEM  = ( INT n, REF[]BITS b )BOOL: ( 1 + ( n MOD bits width ) ) ELEM b[ n OVER bits width ];

# constructs a bit array long enough to hold n values #
OP   BITARRAY = ( INT n )REF[]BITS: HEAP[ 0 : n OVER bits width ]BITS;

# construct a BITS value of all TRUE #
BITS all true = BEGIN
                    BITS v := 16r0;
                    FOR bit TO bits width DO v := bit SET v OD;
                    v
                END;

# initialises a bit array to all TRUE #
OP   SETALL   = ( REF[]BITS b )REF[]BITS:
     BEGIN
         FOR p FROM LWB b TO UPB b DO b[ p ] := all true OD;
         b
     END # SETALL # ;

# construct a sieve initialised to all TRUE apart from the first bit      #
INT sieve max = 15 500 000; # somewhat larger than the 1 000 000th prime  #
INT prime max =  1 000 000;
REF[]BITS sieve = 1 CLEAR SETALL BITARRAY sieve max;

# sieve the primes #
FOR s FROM 2 TO ENTIER sqrt( sieve max ) DO
    IF s ELEM sieve
    THEN
        FOR p FROM s * s BY s TO sieve max DO p CLEAR sieve OD
    FI
OD;

# count the number of times each combination of                           #
# ( last digit of previous prime, last digit of prime ) occurs            #
[ 0 : 9, 0 : 9 ]INT counts;
FOR p FROM 0 TO 9 DO FOR n FROM 0 TO 9 DO counts[ p, n ] := 0 OD OD;
INT previous prime := 2;
INT primes found   := 1;
FOR p FROM 3 TO sieve max WHILE primes found < prime max DO
    IF p ELEM sieve
    THEN
        primes found +:= 1;
        counts[ previous prime MOD 10, p MOD 10 ] +:= 1;
        previous prime := p
    FI
OD;

# print the counts #
# there are thus 4 possible final digits: 1, 3, 7, 9                      #
STRING labels = "123456789";                   # "labels" for the counts  #
INT total := 0;
FOR p TO 9 DO FOR n TO 9 DO total +:= counts[ p, n ] OD OD;
print( ( whole( primes found, 0 ), " primes, last prime considered: ", previous prime, newline ) );
FOR p TO 9 DO
    FOR n TO 9 DO
        IF counts[ p, n ] /= 0
        THEN
            print( ( labels[ p ], "->", labels[ n ]
                   , whole( counts[ p, n ], -8 )
                   , fixed( ( 100 * counts[ p, n ] ) / total, -8, 2 )
                   , newline
                   )
                 )
        FI
    OD
OD
```

```txt

1000000 primes, last prime considered:   +15485863
1->1   42853    4.29
1->3   77475    7.75
1->7   79453    7.95
1->9   50153    5.02
2->3       1    0.00
3->1   58255    5.83
3->3   39668    3.97
3->5       1    0.00
3->7   72827    7.28
3->9   79358    7.94
5->7       1    0.00
7->1   64230    6.42
7->3   68595    6.86
7->7   39603    3.96
7->9   77586    7.76
9->1   84596    8.46
9->3   64371    6.44
9->7   58130    5.81
9->9   42843    4.28

```



## C

```c
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

typedef unsigned char byte;

struct Transition {
    byte a, b;
    unsigned int c;
} transitions[100];

void init() {
    int i, j;
    for (i = 0; i < 10; i++) {
        for (j = 0; j < 10; j++) {
            int idx = i * 10 + j;
            transitions[idx].a = i;
            transitions[idx].b = j;
            transitions[idx].c = 0;
        }
    }
}

void record(int prev, int curr) {
    byte pd = prev % 10;
    byte cd = curr % 10;
    int i;

    for (i = 0; i < 100; i++) {
        int z = 0;
        if (transitions[i].a == pd) {
            int t = 0;
            if (transitions[i].b == cd) {
                transitions[i].c++;
                break;
            }
        }
    }
}

void printTransitions(int limit, int last_prime) {
    int i;

    printf("%d primes, last prime considered: %d\n", limit, last_prime);

    for (i = 0; i < 100; i++) {
        if (transitions[i].c > 0) {
            printf("%d->%d  count: %5d  frequency: %.2f\n", transitions[i].a, transitions[i].b, transitions[i].c, 100.0 * transitions[i].c / limit);
        }
    }
}

bool isPrime(int n) {
    int s, t, a1, a2;

    if (n % 2 == 0) return n == 2;
    if (n % 3 == 0) return n == 3;
    if (n % 5 == 0) return n == 5;
    if (n % 7 == 0) return n == 7;
    if (n % 11 == 0) return n == 11;
    if (n % 13 == 0) return n == 13;
    if (n % 17 == 0) return n == 17;
    if (n % 19 == 0) return n == 19;

    // assuming that addition is faster then multiplication
    t = 23;
    a1 = 96;
    a2 = 216;
    s = t * t;
    while (s <= n) {
        if (n % t == 0) return false;

        // first increment
        s += a1;
        t += 2;
        a1 += 24;
        assert(t * t == s);

        if (s <= n) {
            if (n % t == 0) return false;

            // second increment
            s += a2;
            t += 4;
            a2 += 48;
            assert(t * t == s);
        }
    }

    return true;
}

#define LIMIT 1000000
int main() {
    int last_prime = 3, n = 5, count = 2;

    init();
    record(2, 3);

    while (count < LIMIT) {
        if (isPrime(n)) {
            record(last_prime, n);
            last_prime = n;
            count++;
        }
        n += 2;

        if (count < LIMIT) {
            if (isPrime(n)) {
                record(last_prime, n);
                last_prime = n;
                count++;
            }
            n += 4;
        }
    }

    printTransitions(LIMIT, last_prime);

    return 0;
}
```

```txt
1000000 primes, last prime considered: 15485863
1->1  count: 42853  frequency: 4.29
1->3  count: 77475  frequency: 7.75
1->7  count: 79453  frequency: 7.95
1->9  count: 50153  frequency: 5.02
2->3  count:     1  frequency: 0.00
3->1  count: 58255  frequency: 5.83
3->3  count: 39668  frequency: 3.97
3->5  count:     1  frequency: 0.00
3->7  count: 72827  frequency: 7.28
3->9  count: 79358  frequency: 7.94
5->7  count:     1  frequency: 0.00
7->1  count: 64230  frequency: 6.42
7->3  count: 68595  frequency: 6.86
7->7  count: 39603  frequency: 3.96
7->9  count: 77586  frequency: 7.76
9->1  count: 84596  frequency: 8.46
9->3  count: 64371  frequency: 6.44
9->7  count: 58130  frequency: 5.81
9->9  count: 42843  frequency: 4.28
```



## C++


```cpp
#include <vector>
#include <iostream>
#include <cmath>
#include <utility>
#include <map>
#include <iomanip>

bool isPrime( int i ) {
   int stop = std::sqrt( static_cast<double>( i ) ) ;
   for ( int d = 2 ; d <= stop ; d++ )
      if ( i % d == 0 )
	 return false ;
   return true ;
}

class Compare {
public :
   Compare( ) {
   }

   bool operator( ) ( const std::pair<int , int> & a , const std::pair<int, int> & b ) {
      if ( a.first != b.first )
	 return a.first < b.first ;
      else
	 return a.second < b.second ;
   }
};

int main( ) {
   std::vector<int> primes {2} ;
   int current = 3 ;
   while ( primes.size( ) < 1000000 ) {
      if ( isPrime( current ) )
	 primes.push_back( current ) ;
      current += 2 ;
   }
   Compare myComp ;
   std::map<std::pair<int, int>, int , Compare> conspiracy (myComp) ;
   for ( int i = 0 ; i < primes.size( ) -1 ; i++ ) {
      int a = primes[i] % 10 ;
      int b = primes[ i + 1 ] % 10 ;
      std::pair<int , int> numbers { a , b} ;
      conspiracy[numbers]++ ;
   }
   std::cout << "1000000 first primes. Transitions prime % 10 → next-prime % 10.\n" ;
   for ( auto it = conspiracy.begin( ) ; it != conspiracy.end( ) ; it++ ) {
      std::cout << (it->first).first << " -> " << (it->first).second << " count:" ;
      int frequency = it->second ;
      std::cout << std::right << std::setw( 15 ) << frequency << " frequency: " ;
      std::cout.setf(std::ios::fixed, std::ios::floatfield ) ;
      std::cout.precision( 2 ) ;
      std::cout << (static_cast<double>(frequency) / 1000000.0) * 100 << " %\n" ;
   }
   return 0 ;
}
```

```txt
1 -> 1 count:          42853 frequency: 4.29 %
1 -> 3 count:          77475 frequency: 7.75 %
1 -> 7 count:          79453 frequency: 7.95 %
1 -> 9 count:          50153 frequency: 5.02 %
2 -> 3 count:              1 frequency: 0.00 %
3 -> 1 count:          58255 frequency: 5.83 %
3 -> 3 count:          39668 frequency: 3.97 %
3 -> 5 count:              1 frequency: 0.00 %
3 -> 7 count:          72827 frequency: 7.28 %
3 -> 9 count:          79358 frequency: 7.94 %
5 -> 7 count:              1 frequency: 0.00 %
7 -> 1 count:          64230 frequency: 6.42 %
7 -> 3 count:          68595 frequency: 6.86 %
7 -> 7 count:          39603 frequency: 3.96 %
7 -> 9 count:          77586 frequency: 7.76 %
9 -> 1 count:          84596 frequency: 8.46 %
9 -> 3 count:          64371 frequency: 6.44 %
9 -> 7 count:          58130 frequency: 5.81 %
9 -> 9 count:          42843 frequency: 4.28 %

```


## C#
```c#
using System;

namespace PrimeConspiracy {
    class Program {
        static void Main(string[] args) {
            const int limit = 1_000_000;
            const int sieveLimit = 15_500_000;

            int[,] buckets = new int[10, 10];
            int prevDigit = 2;
            bool[] notPrime = Sieve(sieveLimit);

            for (int n = 3, primeCount = 1; primeCount < limit; n++) {
                if (notPrime[n]) continue;

                int digit = n % 10;
                buckets[prevDigit, digit]++;
                prevDigit = digit;
                primeCount++;
            }

            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    if (buckets[i, j] != 0) {
                        Console.WriteLine("{0} -> {1}  count: {2,5:d}  frequency : {3,6:0.00%}", i, j, buckets[i, j], 1.0 * buckets[i, j] / limit);
                    }
                }
            }
        }

        public static bool[] Sieve(int limit) {
            bool[] composite = new bool[limit];
            composite[0] = composite[1] = true;

            int max = (int)Math.Sqrt(limit);
            for (int n = 2; n <= max; n++) {
                if (!composite[n]) {
                    for (int k = n * n; k < limit; k += n) {
                        composite[k] = true;
                    }
                }
            }

            return composite;
        }
    }
}
```

```txt
1 -> 1  count: 42853  frequency :  4.29%
1 -> 3  count: 77475  frequency :  7.75%
1 -> 7  count: 79453  frequency :  7.95%
1 -> 9  count: 50153  frequency :  5.02%
2 -> 3  count:     1  frequency :  0.00%
3 -> 1  count: 58255  frequency :  5.83%
3 -> 3  count: 39668  frequency :  3.97%
3 -> 5  count:     1  frequency :  0.00%
3 -> 7  count: 72827  frequency :  7.28%
3 -> 9  count: 79358  frequency :  7.94%
5 -> 7  count:     1  frequency :  0.00%
7 -> 1  count: 64230  frequency :  6.42%
7 -> 3  count: 68595  frequency :  6.86%
7 -> 7  count: 39603  frequency :  3.96%
7 -> 9  count: 77586  frequency :  7.76%
9 -> 1  count: 84596  frequency :  8.46%
9 -> 3  count: 64371  frequency :  6.44%
9 -> 7  count: 58130  frequency :  5.81%
9 -> 9  count: 42843  frequency :  4.28%
```



## D

```D
import std.algorithm;
import std.range;
import std.stdio;
import std.typecons;

alias Transition = Tuple!(int, int);

bool isPrime(int n) {
    if (n < 2) return false;
    if (n % 2 == 0) return n == 2;
    if (n % 3 == 0) return n == 3;
    int d = 5;
    while (d*d <= n) {
        if (n%d == 0) return false;
        d += 2;
        if (n%d == 0) return false;
        d += 4;
    }
    return true;
}

auto generatePrimes() {
    import std.concurrency;
    return new Generator!int({
        yield(2);
        int p = 3;
        while (p > 0) {
            if (isPrime(p)) {
                yield(p);
            }
            p += 2;
        }
    });
}

void main() {
    auto primes = generatePrimes().take(1_000_000).array;
    int[Transition] transMap;
    foreach (i; 0 .. primes.length - 1) {
        auto transition = Transition(primes[i] % 10, primes[i + 1] % 10);
        if (transition in transMap) {
            transMap[transition] += 1;
        } else {
            transMap[transition] = 1;
        }
    }
    auto sortedTransitions = transMap.keys.multiSort!(q{a[0] < b[0]}, q{a[1] < b[1]});
    writeln("First 1,000,000 primes. Transitions prime % 10 -> next-prime % 10.");
    foreach (trans; sortedTransitions) {
        writef("%s -> %s  count: %5d", trans[0], trans[1], transMap[trans]);
        writefln("  frequency: %4.2f%%", transMap[trans] / 10_000.0);
    }
}
```

```txt
First 1,000,000 primes. Transitions prime % 10 -> next-prime % 10.
1 -> 1  count: 42853  frequency: 4.29%
1 -> 3  count: 77475  frequency: 7.75%
1 -> 7  count: 79453  frequency: 7.95%
1 -> 9  count: 50153  frequency: 5.02%
2 -> 3  count:     1  frequency: 0.00%
3 -> 1  count: 58255  frequency: 5.83%
3 -> 3  count: 39668  frequency: 3.97%
3 -> 5  count:     1  frequency: 0.00%
3 -> 7  count: 72827  frequency: 7.28%
3 -> 9  count: 79358  frequency: 7.94%
5 -> 7  count:     1  frequency: 0.00%
7 -> 1  count: 64230  frequency: 6.42%
7 -> 3  count: 68595  frequency: 6.86%
7 -> 7  count: 39603  frequency: 3.96%
7 -> 9  count: 77586  frequency: 7.76%
9 -> 1  count: 84596  frequency: 8.46%
9 -> 3  count: 64371  frequency: 6.44%
9 -> 7  count: 58130  frequency: 5.81%
9 -> 9  count: 42843  frequency: 4.28%
```



## EchoLisp


```scheme

(lib 'math) ;; (in-primes n) stream
(decimals 4)

(define (print-trans trans   m N)
(printf "%d first primes. Transitions prime %% %d → next-prime %% %d." N m m)
	(define s (// (apply + (vector->list trans)) 100))
	(for ((i (* m m)) (t trans))
	#:continue (<= t 1) ;; get rid of 2,5 primes
	(printf  " %d → %d   count: %10d frequency:   %d %%  "
              (quotient i m)  (% i m) t  (// t s) )))

;; can apply to any modulo m
;; (in-primes n) returns a stream of primes

(define (task (m  10) (N 1000_000))
	(define trans (make-vector (* m m)))
	(for ((p1 (in-primes 2)) (p2 (in-primes 3)) (k N))
		 (vector+= trans (+ (* (% p1 m) m) (% p2 m)) 1))
	(print-trans trans   m N))

```

```txt

1000000 first primes. Transitions prime % 10 → next-prime % 10.
1 → 1 count:      42853 frequency: 4.2853 %
1 → 3 count:      77475 frequency: 7.7475 %
1 → 7 count:      79453 frequency: 7.9453 %
1 → 9 count:      50153 frequency: 5.0153 %
3 → 1 count:      58255 frequency: 5.8255 %
3 → 3 count:      39668 frequency: 3.9668 %
3 → 7 count:      72828 frequency: 7.2828 %
3 → 9 count:      79358 frequency: 7.9358 %
7 → 1 count:      64230 frequency: 6.423 %
7 → 3 count:      68595 frequency: 6.8595 %
7 → 7 count:      39603 frequency: 3.9603 %
7 → 9 count:      77586 frequency: 7.7586 %
9 → 1 count:      84596 frequency: 8.4596 %
9 → 3 count:      64371 frequency: 6.4371 %
9 → 7 count:      58130 frequency: 5.813 %
9 → 9 count:      42843 frequency: 4.2843 %

```



## Elixir


```elixir
defmodule Prime do
  def conspiracy(m) do
    IO.puts "#{m} first primes. Transitions prime % 10 → next-prime % 10."
    Enum.map(prime(m), &rem(&1, 10))
    |> Enum.chunk(2,1)
    |> Enum.reduce(Map.new, fn [a,b],acc -> Map.update(acc, {a,b}, 1, &(&1+1)) end)
    |> Enum.sort
    |> Enum.each(fn {{a,b},v} ->
         sv = to_string(v) |> String.rjust(10)
         sf = Float.to_string(100.0*v/m, [decimals: 4])
         IO.puts "#{a} → #{b} count:#{sv} frequency:#{sf} %"
       end)
  end

  def prime(n) do
    max = n * :math.log(n * :math.log(n)) |> trunc      # from Rosser's theorem
    Enum.to_list(2..max)
    |> prime(:math.sqrt(max), [])
    |> Enum.take(n)
  end
  defp prime([h|t], limit, result) when h>limit, do: Enum.reverse(result, [h|t])
  defp prime([h|t], limit, result) do
    prime((for x <- t, rem(x,h)>0, do: x), limit, [h|result])
  end
end

Prime.conspiracy(1000000)
```


```txt

1000000 first primes. Transitions prime % 10 → next-prime % 10.
1 → 1 count:     42853 frequency:4.2853 %
1 → 3 count:     77475 frequency:7.7475 %
1 → 7 count:     79453 frequency:7.9453 %
1 → 9 count:     50153 frequency:5.0153 %
2 → 3 count:         1 frequency:0.0001 %
3 → 1 count:     58255 frequency:5.8255 %
3 → 3 count:     39668 frequency:3.9668 %
3 → 5 count:         1 frequency:0.0001 %
3 → 7 count:     72827 frequency:7.2827 %
3 → 9 count:     79358 frequency:7.9358 %
5 → 7 count:         1 frequency:0.0001 %
7 → 1 count:     64230 frequency:6.4230 %
7 → 3 count:     68595 frequency:6.8595 %
7 → 7 count:     39603 frequency:3.9603 %
7 → 9 count:     77586 frequency:7.7586 %
9 → 1 count:     84596 frequency:8.4596 %
9 → 3 count:     64371 frequency:6.4371 %
9 → 7 count:     58130 frequency:5.8130 %
9 → 9 count:     42843 frequency:4.2843 %

```


=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Prime Conspiracy. Nigel Galloway: March 27th., 2018
primes|>Seq.take 10000|>Seq.map(fun n->n%10)|>Seq.pairwise|>Seq.countBy id|>Seq.groupBy(fun((n,_),_)->n)|>Seq.sortBy(fst)
  |>Seq.iter(fun(_,n)->Seq.sortBy(fun((_,n),_)->n) n|>Seq.iter(fun((n,g),z)->printfn "%d -> %d ocurred %3d times" n g z))

```

```txt

1 -> 1 ocurred 365 times
1 -> 3 ocurred 833 times
1 -> 7 ocurred 889 times
1 -> 9 ocurred 397 times
2 -> 3 ocurred   1 times
3 -> 1 ocurred 529 times
3 -> 3 ocurred 324 times
3 -> 5 ocurred   1 times
3 -> 7 ocurred 754 times
3 -> 9 ocurred 907 times
5 -> 7 ocurred   1 times
7 -> 1 ocurred 655 times
7 -> 3 ocurred 722 times
7 -> 7 ocurred 323 times
7 -> 9 ocurred 808 times
9 -> 1 ocurred 935 times
9 -> 3 ocurred 635 times
9 -> 7 ocurred 541 times
9 -> 9 ocurred 379 times

```



## Factor


```factor
USING: assocs formatting grouping kernel math math.primes math.statistics
sequences sorting ;
IN: rosetta-code.prime-conspiracy

: transitions ( n -- alist )
    nprimes [ 10 mod ] map 2 clump histogram >alist natural-sort ;

: t-values ( transition -- i j count freq )
    first2 [ first2 ] dip dup 10000. / ;

: print-trans ( transition -- )
    t-values "%d -> %d  count: %5d  frequency: %5.2f%%\n" printf ;

: header ( n -- )
    "First %d primes. Transitions prime %% 10 -> next-prime %% 10.\n" printf ;

: main ( -- )
    1,000,000 dup header transitions [ print-trans ] each ;

MAIN: main
```

```txt

First 1000000 primes. Transitions prime % 10 -> next-prime % 10.
1 -> 1  count: 42853  frequency:  4.29%
1 -> 3  count: 77475  frequency:  7.75%
1 -> 7  count: 79453  frequency:  7.95%
1 -> 9  count: 50153  frequency:  5.02%
2 -> 3  count:     1  frequency:  0.00%
3 -> 1  count: 58255  frequency:  5.83%
3 -> 3  count: 39668  frequency:  3.97%
3 -> 5  count:     1  frequency:  0.00%
3 -> 7  count: 72827  frequency:  7.28%
3 -> 9  count: 79358  frequency:  7.94%
5 -> 7  count:     1  frequency:  0.00%
7 -> 1  count: 64230  frequency:  6.42%
7 -> 3  count: 68595  frequency:  6.86%
7 -> 7  count: 39603  frequency:  3.96%
7 -> 9  count: 77586  frequency:  7.76%
9 -> 1  count: 84596  frequency:  8.46%
9 -> 3  count: 64371  frequency:  6.44%
9 -> 7  count: 58130  frequency:  5.81%
9 -> 9  count: 42843  frequency:  4.28%

```



## Fortran

Avoiding base ten chauvinism, here are results for bases two to thirteen. The source file relies on the [[Extensible_prime_generator]] project for its collection of primes. The bitbag file being in place, execution takes about two minutes for a hundred million primes, approaching the thirty-two bit limit.
```Fortran
      PROGRAM INHERIT	!Last digit persistence in successive prime numbers.
      USE PRIMEBAG	!Inherit this also.
      INTEGER MBASE,P0,NHIC	!Problem bounds.
      PARAMETER (MBASE = 13, P0 = 2, NHIC = 100000000)	!This should do.
      INTEGER N(0:MBASE - 1,0:MBASE - 1,2:MBASE)	!The counts. A triangular shape would be better.
      INTEGER I,B,D1,D2	!Assistants.
      INTEGER P,PP	!Prime, and Previous Prime.

      MSG = 6		!Standard output.
      WRITE (MSG,1) MBASE,P0,NHIC	!Announce intent.
    1 FORMAT ("Working in base 2 to ",I0," count the transitions "
     1 "from the low-order digit of one prime number ",/,
     2 "to the low-order digit of its successor. Starting with ",I0,
     3 " and making ",I0," advances.")
      IF (.NOT.GRASPPRIMEBAG(66)) STOP "Gan't grab my file!"	!Attempt in hope.

Chug through the primes.
   10 N = 0	!Clear all my counts!
      P = P0	!Start with the starting prime.
      DO I = 1,NHIC	!Make the specified number of advances.
        PP = P			!Thus, remember the previous prime.
        P = NEXTPRIME(P)	!And obtain the current prime.
        DO B = 2,MBASE		!For these, step through the relevant bases.
          D1 = MOD(PP,B)		!Last digit of the previous prime.
          D2 = MOD(P,B)			!In the base of the moment.
          N(D1,D2,B) = N(D1,D2,B) + 1	!Whee!
        END DO			!On to the next base.
      END DO		!And the next advance.
      WRITE (MSG,11) P	!Might as well announce where we got to.
   11 FORMAT ("Ending with ",I0)	!Hopefully, no overflow.

Cast forth the results.
   20 DO B = 2,MBASE	!Present results for each base.
        WRITE (MSG,21) B		!Announce it.
   21   FORMAT (/,"For base ",I0)	!Set off with a blank line.
        WRITE (MSG,22) (D1, D1 = 0,B - 1)	!The heading.
   22   FORMAT (" Last digit ending  ",I2,66I9)	!Alignment to match FORMAT 23.
        DO D2 = 0,B - 1		!For a given base, these are the possible ending digits of the successor.
          IF (ALL(N(0:B - 1,D2,B).EQ.0)) CYCLE	!No progenitor advanced to this successor digit?
          WRITE (MSG,23) D2,N(0:B - 1,D2,B)	!Otherwise, show the counts for the progenitor's digits.
   23     FORMAT (" next prime ends",I3,":",I2,66I9)	!Ah, layout.
        END DO			!On to the next successor digit.
      END DO		!On to the next base.
      END	!That was easy.
```


Results: just the counts - with the total number being a power of ten, percentages are deducible by eye. Though one could add row and column percentages as a further feature.

```txt

Working in base 2 to 13 count the transitions from the low-order digit of one prime number
to the low-order digit of its successor. Starting with 2 and making 1000000 advances.
Ending with 15485867

For base 2
 Last digit ending   0      1
 next prime ends  1: 1 999999

For base 3
 Last digit ending   0      1      2
 next prime ends  0: 0      0      1
 next prime ends  1: 0 215873 283956
 next prime ends  2: 1 283956 216213

For base 4
 Last digit ending   0      1      2      3
 next prime ends  1: 0 218510      0 281288
 next prime ends  3: 0 281288      1 218913

For base 5
 Last digit ending   0      1      2      3      4
 next prime ends  0: 0      0      0      1      0
 next prime ends  1: 0  42853  64230  58255  84596
 next prime ends  2: 1  79453  39603  72828  58130
 next prime ends  3: 0  77475  68596  39668  64371
 next prime ends  4: 0  50153  77586  79358  42843

For base 6
 Last digit ending   0      1      2      3      4      5
 next prime ends  1: 0 215873      0      0      0 283956
 next prime ends  3: 0      0      1      0      0      0
 next prime ends  5: 0 283956      0      1      0 216213

For base 7
 Last digit ending   0      1      2      3      4      5      6
 next prime ends  0: 0      0      0      0      0      1      0
 next prime ends  1: 0  15164  38288  24858  33111  24613  30637
 next prime ends  2: 0  24356  15039  42117  25881  34535  24722
 next prime ends  3: 0  34044  21375  14276  37720  26034  33259
 next prime ends  4: 1  29947  32827  22398  14129  42361  24973
 next prime ends  5: 0  34517  24541  33066  21444  14907  38209
 next prime ends  6: 0  28643  34581  29993  34351  24232  14850

For base 8
 Last digit ending   0      1      2      3      4      5      6      7
 next prime ends  1: 0  42702      0  70223      0  66419      0  70452
 next prime ends  3: 0  70625      1  42803      0  70049      0  66614
 next prime ends  5: 0  66295      0  70357      0  43094      0  70256
 next prime ends  7: 0  70174      0  66708      0  70440      0  42788

For base 9
 Last digit ending   0      1      2      3      4      5      6      7      8
 next prime ends  1: 0  14698  29263      0  33897  22320      0  23370  43020
 next prime ends  2: 0  35215  14554      0  17341  34005      0  42173  23462
 next prime ends  3: 0      0      1      0      0      0      0      0      0
 next prime ends  4: 0  23361  43191      0  14646  29104      0  33942  22393
 next prime ends  5: 0  42086  23284      1  35450  14711      0  17196  33979
 next prime ends  7: 0  34049  22454      0  23379  43149      0  14531  29062
 next prime ends  8: 0  17159  34004      0  41924  23418      0  35412  14796

For base 10
 Last digit ending   0      1      2      3      4      5      6      7      8      9
 next prime ends  1: 0  42853      0  58255      0      0      0  64230      0  84596
 next prime ends  3: 0  77475      1  39668      0      0      0  68595      0  64371
 next prime ends  5: 0      0      0      1      0      0      0      0      0      0
 next prime ends  7: 0  79453      0  72828      0      1      0  39603      0  58130
 next prime ends  9: 0  50153      0  79358      0      0      0  77586      0  42843

For base 11
 Last digit ending   0      1      2      3      4      5      6      7      8      9     10
 next prime ends  0: 0      0      0      0      0      0      0      1      0      0      0
 next prime ends  1: 0   3999   9998   5453  10821   9347  18789   5562  12567   8502  14888
 next prime ends  2: 1  13159   3258  10375   5433  12595   8182  20456   5114  12870   8556
 next prime ends  3: 0  14933  11238   3339  10110   5737  10495   8443  18307   4953  12496
 next prime ends  4: 0   7307  14591  12675   3771  12050   4399  10725   8466  20365   5645
 next prime ends  5: 0  11975   7154  14321  11385   3680   9682   4451  10440   8193  18726
 next prime ends  6: 0   5965  12664   9047  14950  13929   3722  12024   5689  12809   9283
 next prime ends  7: 0  19098   4879  12481   7231  15040  11258   3714  10078   5369  10835
 next prime ends  8: 0   8489  18361   5212  12580   8972  14431  12697   3224  10473   5466
 next prime ends  9: 0  10407   7339  18454   4847  12505   7177  14626  11299   3213  10157
 next prime ends 10: 0   4593  10518   8694  18866   6152  11947   7284  14721  13277   3976

For base 12
 Last digit ending   0      1      2      3      4      5      6      7      8      9     10     11
 next prime ends  1: 0  41742      0      0      0  56951      0  66245      0      0      0  84801
 next prime ends  3: 0      0      1      0      0      0      0      0      0      0      0      0
 next prime ends  5: 0  78104      0      1      0  41713      0  63702      0      0      0  66539
 next prime ends  7: 0  66284      0      0      0  85136      0  41602      0      0      0  57068
 next prime ends 11: 0  63609      0      0      0  66259      0  78541      0      0      0  41702

For base 13
 Last digit ending   0      1      2      3      4      5      6      7      8      9     10     11     12
 next prime ends  0: 0      0      0      0      0      0      0      0      0      0      0      1      0
 next prime ends  1: 0   1953   9078   4186   8454   2940   6619   3993  14451   6745  10926   4234   9735
 next prime ends  2: 0   6291   1835  10356   3955   8664   2911   8018   3614  15477   6545  11389   4285
 next prime ends  3: 0  10075   5366   1625   9007   3987   8401   3449   6747   3232  14138   6461  10867
 next prime ends  4: 1   5055  10180   5544   1645  10053   4101  10371   3530   7150   3124  15703   6810
 next prime ends  5: 0  11227   4131  10474   5407   1952   9144   4418   8564   3480   6626   3774  14199
 next prime ends  6: 0   7133  11205   5339  10153   6503   1999  10883   4331  10567   3440   7886   3930
 next prime ends  7: 0  14291   5974  10982   4158   9778   5023   1973   9236   4071   8416   2974   6492
 next prime ends  8: 0   3597  14154   6835  11009   4179   9815   6392   1899  10019   3924   8597   2919
 next prime ends  9: 0   6770   3123  14032   5767  10912   4100  10230   5368   1661   8938   3858   8550
 next prime ends 10: 0   3977   6789   3305  13949   6698  10979   5255  10223   5645   1591  10425   4377
 next prime ends 11: 0   8584   2908   6834   3068  14060   5901  11264   4231  10164   5385   1858   9155
 next prime ends 12: 0   4361   8598   3843   6695   3670  14376   7121  11145   5098  10160   6252   1998

```

Rows whose counts are all zero are omitted - this happens when a successor prime never has a last digit of that row's value, such as four in base ten. Although there is a prime whose last digit in base ten is two, it does not appear because it is the first prime and so has no progenitor. All-zero columns have not been omitted because this would spoil the regular layout.

And for a hundred million primes,

```txt

Working in base 2 to 13 count the transitions from the low-order digit of one prime number
to the low-order digit of its successor. Starting with 2 and making 100000000 advances.
Ending with 2038074751

For base 2
 Last digit ending   0        1
 next prime ends  1: 1 99999999

For base 3
 Last digit ending   0        1        2
 next prime ends  0: 0        0        1
 next prime ends  1: 0 22332857 27665881
 next prime ends  2: 1 27665880 22335380

For base 4
 Last digit ending   0        1        2        3
 next prime ends  1: 0 22518020        0 27480728
 next prime ends  3: 0 27480728        1 22520523

For base 5
 Last digit ending   0        1        2        3        4
 next prime ends  0: 0        0        0        1        0
 next prime ends  1: 0  4623041  6373982  6010982  7991431
 next prime ends  2: 1  7504612  4439355  7043695  6012739
 next prime ends  3: 0  7429438  6755196  4442561  6372940
 next prime ends  4: 0  5442344  7431870  7502896  4622916

For base 6
 Last digit ending   0        1        2        3        4        5
 next prime ends  1: 0 22332857        0        0        0 27665881
 next prime ends  3: 0        0        1        0        0        0
 next prime ends  5: 0 27665880        0        1        0 22335380

For base 7
 Last digit ending   0        1        2        3        4        5        6
 next prime ends  0: 0        0        0        0        0        1        0
 next prime ends  1: 0  1710317  3579611  2591646  3159868  2599446  3025827
 next prime ends  2: 0  2491147  1712083  3856492  2687056  3318876  2600510
 next prime ends  3: 0  3311720  2285770  1663558  3558788  2687777  3159971
 next prime ends  4: 1  2964083  3222283  2367009  1665335  3856805  2590402
 next prime ends  5: 0  3290904  2578468  3222924  2281997  1712637  3579829
 next prime ends  6: 0  2898544  3287950  2965955  3312874  2491217  1710319

For base 8
 Last digit ending   0        1        2        3        4        5        6        7
 next prime ends  1: 0  4675763        0  6884344        0  6579970        0  6857864
 next prime ends  3: 0  6857077        1  4674685        0  6884651        0  6583388
 next prime ends  5: 0  6582540        0  6856882        0  4679747        0  6881638
 next prime ends  7: 0  6882561        0  6583891        0  6856439        0  4678559

For base 9
 Last digit ending   0        1        2        3        4        5        6        7        8
 next prime ends  1: 0  1712429  2878457        0  3235655  2384613        0  2493235  3961973
 next prime ends  2: 0  3406194  1714996        0  1982716  3236297        0  3832993  2493585
 next prime ends  3: 0        0        1        0        0        0        0        0        0
 next prime ends  4: 0  2494580  3959235        0  1712610  2876272        0  3240428  2382606
 next prime ends  5: 0  3832709  2493277        1  3407612  1713603        0  1982172  3238184
 next prime ends  7: 0  3237873  2381303        0  2492438  3964156        0  1713609  2877266
 next prime ends  8: 0  1982576  3239513        0  3834700  2492617        0  3404208  1713308

For base 10
 Last digit ending   0        1        2        3        4        5        6        7        8        9
 next prime ends  1: 0  4623041        0  6010982        0        0        0  6373982        0  7991431
 next prime ends  3: 0  7429438        1  4442561        0        0        0  6755195        0  6372940
 next prime ends  5: 0        0        0        1        0        0        0        0        0        0
 next prime ends  7: 0  7504612        0  7043695        0        1        0  4439355        0  6012739
 next prime ends  9: 0  5442344        0  7502896        0        0        0  7431870        0  4622916

For base 11
 Last digit ending   0        1        2        3        4        5        6        7        8        9       10
 next prime ends  0: 0        0        0        0        0        0        0        1        0        0        0
 next prime ends  1: 0   472782   987152   690761  1128744   992963  1655521   643313  1147307   905220  1375649
 next prime ends  2: 1  1258827   426804  1020569   682385  1263618   899933  1755317   606871  1182187   904133
 next prime ends  3: 0  1363888  1125636   431931   982064   697538  1091297   919645  1634066   606358  1147618
 next prime ends  4: 0   820519  1348857  1227754   453516  1104903   596084  1130497   919442  1755304   642911
 next prime ends  5: 0  1096088   793472  1333255  1136957   451728   945965   595226  1089089   901084  1656397
 next prime ends  6: 0   671019  1163646   940857  1387241  1325341   452693  1104345   697468  1264482   993515
 next prime ends  7: 0  1673116   588655  1151664   815183  1385139  1137406   453348   983553   681577  1130124
 next prime ends  8: 0   939183  1634709   627942  1151436   943163  1332330  1228950   432234  1018575   691648
 next prime ends  9: 0  1094893   835575  1635970   590472  1162713   793939  1347687  1125195   426991   986431
 next prime ends 10: 0   609097  1096140   939338  1671789   672155  1095439   821436  1364945  1258087   472019

For base 12
 Last digit ending   0        1        2        3        4        5        6        7        8        9       10       11
 next prime ends  1: 0  4581666        0        0        0  5906767        0  6583464        0        0        0  7926375
 next prime ends  3: 0        0        1        0        0        0        0        0        0        0        0        0
 next prime ends  5: 0  7448579        0        1        0  4581008        0  6385500        0        0        0  6585388
 next prime ends  7: 0  6584610        0        0        0  7926739        0  4583117        0        0        0  5906000
 next prime ends 11: 0  6383417        0        0        0  6585962        0  7448384        0        0        0  4583022

For base 13
 Last digit ending   0        1        2        3        4        5        6        7        8        9       10       11       12
 next prime ends  0: 0        0        0        0        0        0        0        0        0        0        0        1        0
 next prime ends  1: 0   263116   888730   515969   825156   381828   644210   454557  1225834   718454  1026344   509296   879696
 next prime ends  2: 0   640913   253674   972641   487125   840035   378972   742069   437251  1308911   695121  1068820   508596
 next prime ends  3: 0   910433   574577   238678   873242   487011   825429   422028   667072   402445  1208422   695733  1027850
 next prime ends  4: 1   573518   915098   609277   238367   951577   501842   962981   431806   719222   402267  1307089   719673
 next prime ends  5: 0  1057748   501638   927155   575466   255695   888975   526983   836913   431200   668005   437501  1226499
 next prime ends  6: 0   749475  1056721   589276   912277   645204   263269  1009338   526684   963820   421819   740543   455343
 next prime ends  7: 0  1225645   654483  1030206   492677   873340   553007   263253   889784   501598   826388   378943   643500
 next prime ends  8: 0   434569  1220080   718107  1022332   501322   873378   645853   256146   952094   487955   841119   380763
 next prime ends  9: 0   668664   382770  1208272   644980  1023552   492857   912130   576547   238571   873109   487188   825341
 next prime ends 10: 0   446942   675205   402794  1210032   718260  1029952   588180   927404   608788   237924   971252   516580
 next prime ends 11: 0   837409   375182   673685   382919  1220536   654597  1057261   500450   914554   575080   254286   886648
 next prime ends 12: 0   524758   835971   446860   668145   435418  1227281   748191  1057827   574324   910879   640835   262564
```


## FreeBASIC


```freebasic
' version 13-04-2017
' updated 09-08-2018 Using bit-sieve of odd numbers
' compile with: fbc -s console
' compile with: fbc -s console -Wc -O2 ->more than 2x faster(20.2-> 8,7s)

const max = 2040*1000*1000  ' enough for 100,000,000 primes
const max2 = (max -1) \ 2
Dim As uByte _bit(7)
Dim shared As uByte sieve(max2 \ 8 + 1)
Dim shared As ULong end_digit(1 To 9, 1 To 9)
Dim As ULong i, j, x, i1, j1, x1, c, c1
Dim As String frmt_str = " # " + Chr(26) + " # count:######## frequency:##.##%"

' bit Mask
For i = 0 To 7
  _bit(i) = 1 shl i
Next
' sieving
For i = 1 To  (sqr(max) -1) / 2
  x = 2*i+1
  If (sieve(i Shr 3) And _bit(i And 7)) = 0 Then
    For j = (2*i+2)*i To max2 Step x
      sieve(j Shr 3) or= _bit(j And 7)
    Next
  End If
Next

' count
x = 2 : c = 1
For i = 1 To max2
    If (sieve(i Shr 3) And _bit(i And 7)) = 0 Then
        j = (2*i+1) Mod 10
        end_digit(x, j) += 1
        x = j
        c += 1
        If c = 1000000 Or c = 100000000 Then
            Print "first "; c; " primes"
            c1 = c \ 100
            For i1 = 1 To 9
                For j1 = 1 To 9
                    x1 = end_digit(i1, j1)
                    If x1 <> 0 Then
                         Print Using frmt_str; i1; j1; x1; (x1 / c1)
                    End If
                Next
            Next
            Print
            If c = 100000000 Then Exit for
        End If
    End If
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End

```

Output is shown side by side

```txt
first 1000000 primes                       first 100000000 primes
 1 → 1 count:   42853 frequency: 4.29%      1 → 1 count: 4623041 frequency: 4.62%
 1 → 3 count:   77475 frequency: 7.75%      1 → 3 count: 7429438 frequency: 7.43%
 1 → 7 count:   79453 frequency: 7.95%      1 → 7 count: 7504612 frequency: 7.50%
 1 → 9 count:   50153 frequency: 5.02%      1 → 9 count: 5442344 frequency: 5.44%
 2 → 3 count:       1 frequency: 0.00%      2 → 3 count:       1 frequency: 0.00%
 3 → 1 count:   58255 frequency: 5.83%      3 → 1 count: 6010981 frequency: 6.01%
 3 → 3 count:   39668 frequency: 3.97%      3 → 3 count: 4442561 frequency: 4.44%
 3 → 5 count:       1 frequency: 0.00%      3 → 5 count:       1 frequency: 0.00%
 3 → 7 count:   72827 frequency: 7.28%      3 → 7 count: 7043695 frequency: 7.04%
 3 → 9 count:   79358 frequency: 7.94%      3 → 9 count: 7502896 frequency: 7.50%
 5 → 7 count:       1 frequency: 0.00%      5 → 7 count:       1 frequency: 0.00%
 7 → 1 count:   64230 frequency: 6.42%      7 → 1 count: 6373982 frequency: 6.37%
 7 → 3 count:   68595 frequency: 6.86%      7 → 3 count: 6755195 frequency: 6.76%
 7 → 7 count:   39603 frequency: 3.96%      7 → 7 count: 4439355 frequency: 4.44%
 7 → 9 count:   77586 frequency: 7.76%      7 → 9 count: 7431870 frequency: 7.43%
 9 → 1 count:   84596 frequency: 8.46%      9 → 1 count: 7991431 frequency: 7.99%
 9 → 3 count:   64371 frequency: 6.44%      9 → 3 count: 6372940 frequency: 6.37%
 9 → 7 count:   58130 frequency: 5.81%      9 → 7 count: 6012739 frequency: 6.01%
 9 → 9 count:   42843 frequency: 4.28%      9 → 9 count: 4622916 frequency: 4.62%
```



## Go

This uses the Sieve of Eratosthenes, with a few simple optimizations, to generate the primes;
this takes about half the runtime.

Expect a run time of ~20−60 seconds to generate and process the full 100 million primes.

```Go
package main

import (
	"fmt"
	"sort"
)

func sieve(limit uint64) []bool {
	limit++
	// True denotes composite, false denotes prime.
	// We don't bother filling in the even composites.
	c := make([]bool, limit)
	c[0] = true
	c[1] = true
	p := uint64(3) // Start from 3.
	for {
		p2 := p * p
		if p2 >= limit {
			break
		}
		for i := p2; i < limit; i += 2 * p {
			c[i] = true
		}
		for {
			p += 2
			if !c[p] {
				break
			}
		}
	}
	return c
}

func main() {
	// sieve up to the 100 millionth prime
	sieved := sieve(2038074743)

	transMap := make(map[int]int, 19)
	i := 2            // last digit of first prime
	p := int64(3 - 2) // next prime, -2 since we +=2 first
	n := 1
	for _, num := range [...]int{1e4, 1e6, 1e8} {
		for ; n < num; n++ {
			// Set p to next prime by skipping composites.
			p += 2
			for sieved[p] {
				p += 2
			}
			// Count transition of i -> j.
			j := int(p % 10)
			transMap[i*10+j]++
			i = j
		}
		reportTransitions(transMap, n)
	}
}

func reportTransitions(transMap map[int]int, num int) {
	keys := make([]int, 0, len(transMap))
	for k := range transMap {
		keys = append(keys, k)
	}
	sort.Ints(keys)
	fmt.Println("First", num, "primes. Transitions prime % 10 -> next-prime % 10.")
	for _, key := range keys {
		count := transMap[key]
		freq := float64(count) / float64(num) * 100
		fmt.Printf("%d -> %d  count: %7d", key/10, key%10, count)
		fmt.Printf("  frequency: %4.2f%%\n", freq)
	}
	fmt.Println()
}
```


```txt

First 10000 primes. Transitions prime % 10 -> next-prime % 10.
1 -> 1  count:     365  frequency: 3.65%
1 -> 3  count:     833  frequency: 8.33%
1 -> 7  count:     889  frequency: 8.89%
1 -> 9  count:     397  frequency: 3.97%
2 -> 3  count:       1  frequency: 0.01%
3 -> 1  count:     529  frequency: 5.29%
3 -> 3  count:     324  frequency: 3.24%
3 -> 5  count:       1  frequency: 0.01%
3 -> 7  count:     754  frequency: 7.54%
3 -> 9  count:     907  frequency: 9.07%
5 -> 7  count:       1  frequency: 0.01%
7 -> 1  count:     655  frequency: 6.55%
7 -> 3  count:     722  frequency: 7.22%
7 -> 7  count:     323  frequency: 3.23%
7 -> 9  count:     808  frequency: 8.08%
9 -> 1  count:     935  frequency: 9.35%
9 -> 3  count:     635  frequency: 6.35%
9 -> 7  count:     541  frequency: 5.41%
9 -> 9  count:     379  frequency: 3.79%

First 1000000 primes. Transitions prime % 10 -> next-prime % 10.
1 -> 1  count:   42853  frequency: 4.29%
1 -> 3  count:   77475  frequency: 7.75%
1 -> 7  count:   79453  frequency: 7.95%
1 -> 9  count:   50153  frequency: 5.02%
2 -> 3  count:       1  frequency: 0.00%
3 -> 1  count:   58255  frequency: 5.83%
3 -> 3  count:   39668  frequency: 3.97%
3 -> 5  count:       1  frequency: 0.00%
3 -> 7  count:   72827  frequency: 7.28%
3 -> 9  count:   79358  frequency: 7.94%
5 -> 7  count:       1  frequency: 0.00%
7 -> 1  count:   64230  frequency: 6.42%
7 -> 3  count:   68595  frequency: 6.86%
7 -> 7  count:   39603  frequency: 3.96%
7 -> 9  count:   77586  frequency: 7.76%
9 -> 1  count:   84596  frequency: 8.46%
9 -> 3  count:   64371  frequency: 6.44%
9 -> 7  count:   58130  frequency: 5.81%
9 -> 9  count:   42843  frequency: 4.28%

First 100000000 primes. Transitions prime % 10 -> next-prime % 10.
1 -> 1  count: 4623041  frequency: 4.62%
1 -> 3  count: 7429438  frequency: 7.43%
1 -> 7  count: 7504612  frequency: 7.50%
1 -> 9  count: 5442344  frequency: 5.44%
2 -> 3  count:       1  frequency: 0.00%
3 -> 1  count: 6010981  frequency: 6.01%
3 -> 3  count: 4442561  frequency: 4.44%
3 -> 5  count:       1  frequency: 0.00%
3 -> 7  count: 7043695  frequency: 7.04%
3 -> 9  count: 7502896  frequency: 7.50%
5 -> 7  count:       1  frequency: 0.00%
7 -> 1  count: 6373982  frequency: 6.37%
7 -> 3  count: 6755195  frequency: 6.76%
7 -> 7  count: 4439355  frequency: 4.44%
7 -> 9  count: 7431870  frequency: 7.43%
9 -> 1  count: 7991431  frequency: 7.99%
9 -> 3  count: 6372940  frequency: 6.37%
9 -> 7  count: 6012739  frequency: 6.01%
9 -> 9  count: 4622916  frequency: 4.62%

```



## J


This gets the job done:


```J
   /:~ (~.,. ' ',. ":@(%/&1 999999)@(#/.~)) 2 (,'->',])&":/\ 10|p:i.1e6
1->1 42853  0.042853
1->3 77475 0.0774751
1->7 79453 0.0794531
1->9 50153 0.0501531
2->3     1      1e_6
3->1 58255 0.0582551
3->3 39668  0.039668
3->5     1      1e_6
3->7 72827 0.0728271
3->9 79358 0.0793581
5->7     1      1e_6
7->1 64230 0.0642301
7->3 68595 0.0685951
7->7 39603  0.039603
7->9 77586 0.0775861
9->1 84596 0.0845961
9->3 64371 0.0643711
9->7 58130 0.0581301
9->9 42843  0.042843
```


Note that the [[Sieve of Eratosthenes]] task has some important implications for how often we will see the various transitions here.

Anyways, here is how the code works:

'''p:i.1e6''' generates the first million primes.

'''10|''' ''...'' gets their last digits

'''2  ''...'' '''\''' pairs them up

'''(,'->',])&":/''' formats a pair of digits with a '->' between them

'''#/.~''' counts frequencies of unique values

'''~.''' gets the corresponding unique values

'''/:~''' sorts

And the rest is just more formatting...

Or, if you prefer the ratios formatted as percents, you could do this:


```J
   /:~ (~.,. ' ',. '%',.~ ":@(%/&1 9999.99)@(#/.~)) 2 (,'->',])&":/\ 10|p:i.1e6
1->1 42853  4.2853%
1->3 77475 7.74751%
1->7 79453 7.94531%
1->9 50153 5.01531%
2->3     1  0.0001%
3->1 58255 5.82551%
3->3 39668  3.9668%
3->5     1  0.0001%
3->7 72827 7.28271%
3->9 79358 7.93581%
5->7     1  0.0001%
7->1 64230 6.42301%
7->3 68595 6.85951%
7->7 39603  3.9603%
7->9 77586 7.75861%
9->1 84596 8.45961%
9->3 64371 6.43711%
9->7 58130 5.81301%
9->9 42843  4.2843%
```


'''Extra Credit:'''

Because of memory limitations on current machines, for the extra credit part, we need to divide and conqueror. Instead of simply tallying all the results <code>(#/.)</code> we will break the problem up into 100 groups of a million primes each, and then add the tallies <code>(+//.)</code>. Actually, the first 99 batches will be 1000001 primes each and only the final batch will be exactly 1000000 primes. (This gives us the overlap we need so that we can count the pair which spans each block of primes.)

More specifically, for each block, we will compute the unique list of labels (such as '9->1') and the unique list of tallies (such as 84596) and put each such result into a box. Then once we have all of our boxes of intermediate results, we will combine their contents.

In other words:


```J
   dgpairs=: 2 (,'->',])&":/\ 10 | p:
   combine=: ~.@[ ,. ' ',. ":@(%/&1 99999999)@(+//.)
   /:~ combine&;/|: (~.;#/.~)@dgpairs@((+ i.)/)"1 (1e6*i.100),.1e6+99>i.100
1->1 4.62304e6 0.0462304
1->3 7.42944e6 0.0742944
1->7 7.50461e6 0.0750461
1->9 5.44234e6 0.0544234
2->3         1      1e_8
3->1 6.01098e6 0.0601098
3->3 4.44256e6 0.0444256
3->5         1      1e_8
3->7  7.0437e6  0.070437
3->9  7.5029e6  0.075029
5->7         1      1e_8
7->1 6.37398e6 0.0637398
7->3  6.7552e6  0.067552
7->7 4.43936e6 0.0443936
7->9 7.43187e6 0.0743187
9->1 7.99143e6 0.0799143
9->3 6.37294e6 0.0637294
9->7 6.01274e6 0.0601274
9->9 4.62292e6 0.0462292
```



## Java


```java
public class PrimeConspiracy {

    public static void main(String[] args) {
        final int limit = 1000_000;
        final int sieveLimit = 15_500_000;

        int[][] buckets = new int[10][10];
        int prevDigit = 2;
        boolean[] notPrime = sieve(sieveLimit);

        for (int n = 3, primeCount = 1; primeCount < limit; n++) {
            if (notPrime[n])
                continue;

            int digit = n % 10;
            buckets[prevDigit][digit]++;
            prevDigit = digit;
            primeCount++;
        }

        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j++) {
                if (buckets[i][j] != 0) {
                    System.out.printf("%d -> %d : %2f%n", i,
                            j, buckets[i][j] / (limit / 100.0));
                }
            }
        }
    }

    public static boolean[] sieve(int limit) {
        boolean[] composite = new boolean[limit];
        composite[0] = composite[1] = true;

        int max = (int) Math.sqrt(limit);
        for (int n = 2; n <= max; n++) {
            if (!composite[n]) {
                for (int k = n * n; k < limit; k += n) {
                    composite[k] = true;
                }
            }
        }
        return composite;
    }
}
```



```txt
1 -> 1 : 4,285300
1 -> 3 : 7,747500
1 -> 7 : 7,945300
1 -> 9 : 5,015300
2 -> 3 : 0,000100
3 -> 1 : 5,825500
3 -> 3 : 3,966800
3 -> 5 : 0,000100
3 -> 7 : 7,282700
3 -> 9 : 7,935800
5 -> 7 : 0,000100
7 -> 1 : 6,423000
7 -> 3 : 6,859500
7 -> 7 : 3,960300
7 -> 9 : 7,758600
9 -> 1 : 8,459600
9 -> 3 : 6,437100
9 -> 7 : 5,813000
9 -> 9 : 4,284300
```



## Julia

```julia
using Primes
using DataStructures

function counttransitions(upto::Integer)
    cnt = counter(Pair{Int,Int})
    tot = 0
    prv, nxt = 2, 3
    while nxt ≤ upto
        push!(cnt, prv % 10 => nxt % 10)
        prv = nxt
        nxt = nextprime(nxt + 1)
        tot += 1
    end
    return sort(Dict(cnt)), tot - 1
end

trans, tot = counttransitions(100_000_000)

println("First 100_000_000 primes, last digit transitions:")
for ((i, j), fr) in trans
    @printf("%i → %i: freq. %3.4f%%\n", i, j, 100fr / tot)
end
```


```txt
First 100_000_000 primes, last digit transitions:
1 → 1: freq. 4.4247%
1 → 3: freq. 7.5979%
1 → 7: freq. 7.7580%
1 → 9: freq. 5.2183%
2 → 3: freq. 0.0000%
3 → 1: freq. 5.9101%
3 → 3: freq. 4.1671%
3 → 5: freq. 0.0000%
3 → 7: freq. 7.1718%
3 → 9: freq. 7.7529%
5 → 7: freq. 0.0000%
7 → 1: freq. 6.3962%
7 → 3: freq. 6.8317%
7 → 7: freq. 4.1692%
7 → 9: freq. 7.6052%
9 → 1: freq. 8.2678%
9 → 3: freq. 6.4053%
9 → 7: freq. 5.9033%
9 → 9: freq. 4.4205%
```



## Kotlin


```scala
// version 1.1.2
// compiled with flag -Xcoroutines=enable to suppress 'experimental' warning

import kotlin.coroutines.experimental.*

typealias Transition = Pair<Int, Int>

fun isPrime(n: Int) : Boolean {
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

fun generatePrimes() =
    buildSequence {
        yield(2)
        var p = 3
        while (p <= Int.MAX_VALUE) {
           if (isPrime(p)) yield(p)
           p += 2
        }
    }

fun main(args: Array<String>) {
    val primes = generatePrimes().take(1_000_000).toList()
    val transMap = mutableMapOf<Transition, Int>()
    for (i in 0 until primes.size - 1) {
        val transition = primes[i] % 10 to primes[i + 1] % 10
        if (transMap.containsKey(transition))
            transMap[transition] = transMap[transition]!! + 1
        else
            transMap.put(transition, 1)
    }
    val sortedTransitions = transMap.keys.sortedBy { it.second }.sortedBy { it.first }
    println("First 1,000,000 primes. Transitions prime % 10 -> next-prime % 10.")
    for (trans in sortedTransitions) {
        print("${trans.first} -> ${trans.second}  count: ${"%5d".format(transMap[trans])}")
        println("  frequency: ${"%4.2f".format(transMap[trans]!! / 10000.0)}%")
    }
}
```


```txt

First 1,000,000 primes. Transitions prime % 10 -> next-prime % 10.
1 -> 1  count: 42853  frequency: 4.29%
1 -> 3  count: 77475  frequency: 7.75%
1 -> 7  count: 79453  frequency: 7.95%
1 -> 9  count: 50153  frequency: 5.02%
2 -> 3  count:     1  frequency: 0.00%
3 -> 1  count: 58255  frequency: 5.83%
3 -> 3  count: 39668  frequency: 3.97%
3 -> 5  count:     1  frequency: 0.00%
3 -> 7  count: 72827  frequency: 7.28%
3 -> 9  count: 79358  frequency: 7.94%
5 -> 7  count:     1  frequency: 0.00%
7 -> 1  count: 64230  frequency: 6.42%
7 -> 3  count: 68595  frequency: 6.86%
7 -> 7  count: 39603  frequency: 3.96%
7 -> 9  count: 77586  frequency: 7.76%
9 -> 1  count: 84596  frequency: 8.46%
9 -> 3  count: 64371  frequency: 6.44%
9 -> 7  count: 58130  frequency: 5.81%
9 -> 9  count: 42843  frequency: 4.28%

```



## Lua

Takes about eight seconds with a limit of 10^6.  It could of course be changed to 10^8 for the extra credit but the execution time is longer than my patience lasted.

```Lua
-- Return boolean indicating whether or not n is prime
function isPrime (n)
    if n <= 1 then return false end
    if n <= 3 then return true end
    if n % 2 == 0 or n % 3 == 0 then return false end
    local i = 5
    while i * i <= n do
        if n % i == 0 or n % (i + 2) == 0 then return false end
        i = i + 6
    end
    return true
end

-- Return table of frequencies for final digits of consecutive primes
function primeCon (limit)
    local count, x, last, ending = 2, 3, 3
    local freqList = {
        [1] = {},
        [2] = {[3] = 1},
        [3] = {},
        [5] = {},
        [7] = {},
        [9] = {}
    }
    repeat
        x = x + 2
        if isPrime(x) then
            ending = x % 10
            if freqList[last][ending] then
                freqList[last][ending] = freqList[last][ending] + 1
            else
                freqList[last][ending] = 1
            end
            last = ending
            count = count + 1
        end
    until count == limit
    return freqList
end

-- Main procedure
local limit = 10^6
local t = primeCon(limit)
for a = 1, 9 do
    for b = 1, 9 do
        if t[a] and t[a][b] then
            io.write(a .. " -> " .. b .. "\tcount: " .. t[a][b])
            print("\tfrequency: " .. t[a][b] / limit * 100 .. " %")
        end
    end
end
```

```txt
1 -> 1  count: 42853    frequency: 4.2853 %
1 -> 3  count: 77475    frequency: 7.7475 %
1 -> 7  count: 79453    frequency: 7.9453 %
1 -> 9  count: 50153    frequency: 5.0153 %
2 -> 3  count: 1        frequency: 0.0001 %
3 -> 1  count: 58255    frequency: 5.8255 %
3 -> 3  count: 39668    frequency: 3.9668 %
3 -> 5  count: 1        frequency: 0.0001 %
3 -> 7  count: 72827    frequency: 7.2827 %
3 -> 9  count: 79358    frequency: 7.9358 %
5 -> 7  count: 1        frequency: 0.0001 %
7 -> 1  count: 64230    frequency: 6.423 %
7 -> 3  count: 68595    frequency: 6.8595 %
7 -> 7  count: 39603    frequency: 3.9603 %
7 -> 9  count: 77586    frequency: 7.7586 %
9 -> 1  count: 84596    frequency: 8.4596 %
9 -> 3  count: 64371    frequency: 6.4371 %
9 -> 7  count: 58130    frequency: 5.813 %
9 -> 9  count: 42843    frequency: 4.2843 %
```



## Pascal

Modified sieve of Eratothenes of odd numbers only.Tuned  output.Its easy to see the count of every transition in % value.

I just memorize the last digit and after finding the next prime in sieve increment the field of 2D array CTR_CntTrans[lastdigit,newDigit].

'''Extra credit:''' is included PrimeLimit = 2038074743-> 100'000'000 Primes

```pascal

program primCons;
{$IFNDEF FPC}
  {$APPTYPE CONSOLE}
{$ENDIF}
const
  PrimeLimit =  2038074748 DIV 2;
type
  tLimit = 0..PrimeLimit;
  tCntTransition = array[0..9,0..9] of NativeInt;
  tCntTransRec = record
                   CTR_CntTrans:tCntTransition;
                   CTR_primCnt,
                   CTR_Limit : NativeInt;
                 end;
  tCntTransRecField = array[0..19] of tCntTransRec;
var
  primes: array [tLimit] of boolean;
  CntTransitions : tCntTransRecField;

procedure SieveSmall;
//sieve of eratosthenes with only odd numbers
var
  i,j,p: NativeInt;
Begin
  FillChar(primes[1],SizeOF(primes),chr(ord(true)));
  i := 1;
  p := 3;
  j := i*(i+1)*2;
  repeat
    IF (primes[i]) then
    begin
      p := i+i+1;
      repeat
        primes[j] := false;
        inc(j,p);
      until j > PrimeLimit;
    end;
    inc(i);
    j := i*(i+1)*2;//position of i*i
    IF PrimeLimit < j then
      BREAK;
  until false;
end;

procedure OutputTransitions(const Trs:tCntTransRecField);
var
  i,j,k,res,cnt: NativeInt;
  ThereWasOutput: boolean;
Begin
  cnt := 0;
  while Trs[cnt].CTR_primCnt > 0 do
    inc(cnt);
  dec(cnt);
  IF cnt < 0 then
    EXIT;

  write('PrimCnt ');
  For i := 0 to cnt do
    write(Trs[i].CTR_primCnt:i+7);
  writeln;
  For i := 0 to 9 do
  Begin
    ThereWasOutput := false;
    For j := 0 to 9 do
    Begin
      res := Trs[0].CTR_CntTrans[i,j];
      IF res > 0 then
      Begin
        ThereWasOutput := true;
        write('''',i,'''->''',j,'''');
        For k := 0 to cnt do
        Begin
          res := Trs[k].CTR_CntTrans[i,j];
          write(res/Trs[k].CTR_primCnt*100:k+6:k+2,'%');
        end;
        writeln;
      end;
    end;
    IF ThereWasOutput then
      writeln;
  end;
end;

var
  pCntTransOld,
  pCntTransNew  : ^tCntTransRec;
  i,primCnt,lmt : NativeInt;
  prvChr,
  nxtChr : NativeInt;
Begin
  SieveSmall;
  pCntTransOld := @CntTransitions[0].CTR_CntTrans;


  pCntTransOld^.CTR_CntTrans[2,3]:= 1;
  lmt := 10*1000;

  //starting at 2 *2+1 => 5
  primCnt := 2; // the prime 2,3
  prvChr := 3;
  nxtChr  := prvChr;
  for i:= 2 to PrimeLimit do
  Begin
    inc(nxtChr,2);
    if nxtChr >= 10 then nxtChr := 1;
    IF primes[i] then
    Begin
      inc(pCntTransOld^.CTR_CntTrans[prvChr][nxtChr]);
      inc(primCnt);
      prvchr := nxtChr;
      IF primCnt >= lmt then
      Begin
        with pCntTransOld^ do Begin
          CTR_Limit := i;
          CTR_primCnt := primCnt;
        end;
        pCntTransNew := pCntTransOld;
        inc(pCntTransNew);
        pCntTransNew^:= pCntTransOld^;
        pCntTransOld := pCntTransNew;
        lmt := lmt*10;
      end;
    end;
  end;
  pCntTransOld^.CTR_primCnt := 0;
  OutputTransitions(CntTransitions);
end.

```

```txt
PrimCnt   10000  100000  1000000  10000000  100000000
'1'->'1'  3.65%  4.104%  4.2853%  4.46808%  4.623041%
'1'->'3'  8.33%  7.961%  7.7475%  7.56071%  7.429438%
'1'->'7'  8.89%  8.297%  7.9453%  7.69923%  7.504612%
'1'->'9'  3.97%  4.605%  5.0153%  5.26953%  5.442344%

'2'->'3'  0.01%  0.001%  0.0001%  0.00001%  0.000001%

'3'->'1'  5.29%  5.596%  5.8255%  5.93195%  6.010981%
'3'->'3'  3.24%  3.604%  3.9668%  4.22302%  4.442561%
'3'->'5'  0.01%  0.001%  0.0001%  0.00001%  0.000001%
'3'->'7'  7.54%  7.419%  7.2827%  7.14795%  7.043695%
'3'->'9'  9.07%  8.387%  7.9358%  7.69915%  7.502896%

'5'->'7'  0.01%  0.001%  0.0001%  0.00001%  0.000001%

'7'->'1'  6.55%  6.438%  6.4230%  6.39384%  6.373982%
'7'->'3'  7.22%  6.928%  6.8595%  6.81759%  6.755195%
'7'->'7'  3.23%  3.627%  3.9603%  4.22289%  4.439355%
'7'->'9'  8.08%  8.022%  7.7586%  7.56851%  7.431870%

'9'->'1'  9.35%  8.829%  8.4596%  8.20368%  7.991431%
'9'->'3'  6.35%  6.513%  6.4371%  6.40076%  6.372940%
'9'->'7'  5.41%  5.671%  5.8130%  5.93275%  6.012739%
'9'->'9'  3.79%  3.995%  4.2843%  4.46032%  4.622916%

real    0m11.400s

```



## Perl

```perl
use ntheory qw/forprimes nth_prime/;

my $upto = 1_000_000;
my %freq;
my($this_digit,$last_digit)=(2,0);

forprimes {
  ($last_digit,$this_digit) = ($this_digit, $_ % 10);
  $freq{$last_digit . $this_digit}++;
} 3,nth_prime($upto);

print "$upto first primes.  Transitions prime % 10 → next-prime % 10.\n";
printf "%s → %s count:\t%7d\tfrequency: %4.2f %%\n",
  substr($_,0,1), substr($_,1,1), $freq{$_}, 100*$freq{$_}/$upto
    for sort keys %freq;
```

```txt
1000000 first primes.  Transitions prime % 10 → next-prime % 10.
1 → 1 count:	  42853	frequency: 4.29 %
1 → 3 count:	  77475	frequency: 7.75 %
1 → 7 count:	  79453	frequency: 7.95 %
1 → 9 count:	  50153	frequency: 5.02 %
2 → 3 count:	      1	frequency: 0.00 %
3 → 1 count:	  58255	frequency: 5.83 %
3 → 3 count:	  39668	frequency: 3.97 %
3 → 5 count:	      1	frequency: 0.00 %
3 → 7 count:	  72827	frequency: 7.28 %
3 → 9 count:	  79358	frequency: 7.94 %
5 → 7 count:	      1	frequency: 0.00 %
7 → 1 count:	  64230	frequency: 6.42 %
7 → 3 count:	  68595	frequency: 6.86 %
7 → 7 count:	  39603	frequency: 3.96 %
7 → 9 count:	  77586	frequency: 7.76 %
9 → 1 count:	  84596	frequency: 8.46 %
9 → 3 count:	  64371	frequency: 6.44 %
9 → 7 count:	  58130	frequency: 5.81 %
9 → 9 count:	  42843	frequency: 4.28 %
```


The extra credit is done in less than 25 seconds on a Macbook.  Less than 2 seconds is spent generating primes.  We can get identical output in 10 seconds by using an array rather than a hash to store transitions.

```txt
100000000 first primes.  Transitions prime % 10 → next-prime % 10.
1 → 1 count:	4623041	frequency: 4.62 %
1 → 3 count:	7429438	frequency: 7.43 %
1 → 7 count:	7504612	frequency: 7.50 %
1 → 9 count:	5442344	frequency: 5.44 %
2 → 3 count:	      1	frequency: 0.00 %
3 → 1 count:	6010981	frequency: 6.01 %
3 → 3 count:	4442561	frequency: 4.44 %
3 → 5 count:	      1	frequency: 0.00 %
3 → 7 count:	7043695	frequency: 7.04 %
3 → 9 count:	7502896	frequency: 7.50 %
5 → 7 count:	      1	frequency: 0.00 %
7 → 1 count:	6373982	frequency: 6.37 %
7 → 3 count:	6755195	frequency: 6.76 %
7 → 7 count:	4439355	frequency: 4.44 %
7 → 9 count:	7431870	frequency: 7.43 %
9 → 1 count:	7991431	frequency: 7.99 %
9 → 3 count:	6372940	frequency: 6.37 %
9 → 7 count:	6012739	frequency: 6.01 %
9 → 9 count:	4622916	frequency: 4.62 %
```



## Perl 6

Using module <code>Math::Primesieve</code> to generate primes, as much faster than the built-in (but extra credit still very slow).

```perl6
use Math::Primesieve;

my %conspiracy;
my $upto = 1_000_000;
my $sieve = Math::Primesieve.new;
my @primes = $sieve.n-primes($upto+1);

@primes[^($upto+1)].reduce: -> $a, $b {
    my $d = $b % 10;
    %conspiracy{"$a → $d count:"}++;
    $d;
}

say "$_ \tfrequency: {($_.value/$upto*100).round(.01)} %" for %conspiracy.sort;
```

```txt
1 → 1 count:	42853 	frequency: 4.29 %
1 → 3 count:	77475 	frequency: 7.75 %
1 → 7 count:	79453 	frequency: 7.95 %
1 → 9 count:	50153 	frequency: 5.02 %
2 → 3 count:	1 	frequency: 0 %
3 → 1 count:	58255 	frequency: 5.83 %
3 → 3 count:	39668 	frequency: 3.97 %
3 → 5 count:	1 	frequency: 0 %
3 → 7 count:	72828 	frequency: 7.28 %
3 → 9 count:	79358 	frequency: 7.94 %
5 → 7 count:	1 	frequency: 0 %
7 → 1 count:	64230 	frequency: 6.42 %
7 → 3 count:	68595 	frequency: 6.86 %
7 → 7 count:	39603 	frequency: 3.96 %
7 → 9 count:	77586 	frequency: 7.76 %
9 → 1 count:	84596 	frequency: 8.46 %
9 → 3 count:	64371 	frequency: 6.44 %
9 → 7 count:	58130 	frequency: 5.81 %
9 → 9 count:	42843 	frequency: 4.28 %
```



## Phix

Using primes() from [[Almost_prime#Phix]]

```Phix
sequence p10k = primes(10000)
sequence transitions = repeat(repeat(0,9),9)
integer last = p10k[1], this
for i=2 to length(p10k) do
    this = remainder(p10k[i],10)
    transitions[last][this] += 1
    last = this
end for
for i=1 to 9 do
    for j=1 to 9 do
        if transitions[i][j]!=0 then
            printf(1,"%d->%d:%3.2f%%\n",{i,j,transitions[i][j]*100/length(p10k)})
        end if
    end for
end for
```

```txt

1->1:3.65%
1->3:8.33%
1->7:8.89%
1->9:3.97%
2->3:0.01%
3->1:5.29%
3->3:3.24%
3->5:0.01%
3->7:7.54%
3->9:9.07%
5->7:0.01%
7->1:6.55%
7->3:7.22%
7->7:3.23%
7->9:8.08%
9->1:9.35%
9->3:6.35%
9->7:5.41%
9->9:3.79%

```

Unfortunately, that prime number generator uses a table: while 1 million primes needs ~16MB*4|8, 10 million needs ~180MB*4|8, (both easily done on either 32 or 64 bit) 100 million would need ~2GB*8, (clearly 64 bit only) which is more than this 4GB box can allocate, it seems.
But it might work on a machine with lots more memory.


## Python

```python
def isPrime(n):
    if n < 2:
        return False
    if n % 2 == 0:
        return n == 2
    if n % 3 == 0:
        return n == 3

    d = 5
    while d * d <= n:
        if n % d == 0:
            return False
        d += 2

        if n % d == 0:
            return False
        d += 4
    return True

def generatePrimes():
    yield 2
    yield 3

    p = 5
    while p > 0:
        if isPrime(p):
            yield p
        p += 2
        if isPrime(p):
            yield p
        p += 4

g = generatePrimes()
transMap = {}
prev = None
limit = 1000000
for _ in xrange(limit):
    prime = next(g)
    if prev:
        transition = (prev, prime %10)
        if transition in transMap:
            transMap[transition] += 1
        else:
            transMap[transition] = 1
    prev = prime % 10

print "First {:,} primes. Transitions prime % 10 > next-prime % 10.".format(limit)
for trans in sorted(transMap):
    print "{0} -> {1} count {2:5} frequency: {3}%".format(trans[0], trans[1], transMap[trans], 100.0 * transMap[trans] / limit)
```

```txt
First 1,000,000 primes. Transitions prime % 10 > next-prime % 10.
1 -> 1 count 42853 frequency: 4.2853%
1 -> 3 count 77475 frequency: 7.7475%
1 -> 7 count 79453 frequency: 7.9453%
1 -> 9 count 50153 frequency: 5.0153%
2 -> 3 count     1 frequency: 0.0001%
3 -> 1 count 58255 frequency: 5.8255%
3 -> 3 count 39668 frequency: 3.9668%
3 -> 5 count     1 frequency: 0.0001%
3 -> 7 count 72827 frequency: 7.2827%
3 -> 9 count 79358 frequency: 7.9358%
5 -> 7 count     1 frequency: 0.0001%
7 -> 1 count 64230 frequency: 6.423%
7 -> 3 count 68595 frequency: 6.8595%
7 -> 7 count 39603 frequency: 3.9603%
7 -> 9 count 77586 frequency: 7.7586%
9 -> 1 count 84596 frequency: 8.4596%
9 -> 3 count 64371 frequency: 6.4371%
9 -> 7 count 58130 frequency: 5.813%
9 -> 9 count 42843 frequency: 4.2843%
```



## R


```rsplus

suppressMessages(library(gmp))

limit <- 1e6
result <- vector('numeric', 99)
prev_prime <- 2
count <- 0

getOutput <- function(transition) {
	if (result[transition] == 0) return()
	second <- transition %% 10
        first <-  (transition - second) / 10
	cat(first,"->",second,"count:", sprintf("%6d",result[transition]), "frequency:",
            sprintf("%5.2f%%\n",result[transition]*100/limit))
}

while (count <= limit) {
	count <- count + 1
	next_prime <- nextprime(prev_prime)
	transition <- 10*(asNumeric(prev_prime) %% 10) + (asNumeric(next_prime) %% 10)
	prev_prime <- next_prime
	result[transition] <- result[transition] + 1
}

cat(sprintf("%d",limit),"first primes. Transitions prime % 10 -> next-prime % 10\n")
invisible(sapply(1:99,getOutput))

```


```txt

1000000 first primes. Transitions prime % 10 -> next-prime % 10
1 -> 1 count:  42853 frequency:  4.29%
1 -> 3 count:  77475 frequency:  7.75%
1 -> 7 count:  79453 frequency:  7.95%
1 -> 9 count:  50153 frequency:  5.02%
2 -> 3 count:      1 frequency:  0.00%
3 -> 1 count:  58255 frequency:  5.83%
3 -> 3 count:  39668 frequency:  3.97%
3 -> 5 count:      1 frequency:  0.00%
3 -> 7 count:  72828 frequency:  7.28%
3 -> 9 count:  79358 frequency:  7.94%
5 -> 7 count:      1 frequency:  0.00%
7 -> 1 count:  64230 frequency:  6.42%
7 -> 3 count:  68595 frequency:  6.86%
7 -> 7 count:  39604 frequency:  3.96%
7 -> 9 count:  77586 frequency:  7.76%
9 -> 1 count:  84596 frequency:  8.46%
9 -> 3 count:  64371 frequency:  6.44%
9 -> 7 count:  58130 frequency:  5.81%
9 -> 9 count:  42843 frequency:  4.28%

```



## Racket



```racket
#lang racket

(require math/number-theory)

(define limit 1000000)

(define table
  (for/fold ([table (hash)] [prev 2] #:result table)
            ([p (in-list (next-primes 2 (sub1 limit)))])
    (define p-mod (modulo p 10))
    (values (hash-update table (cons prev p-mod) add1 0) p-mod)))

(define (pair<? p q) (or (< (car p) (car q)) (and (= (car p) (car q)) (< (cdr p) (cdr q)))))

(printf "~a first primes. Transitions prime % 10 → next-prime % 10.\n" limit)
(for ([item (sort (hash->list table) pair<? #:key car)])
  (match-define (cons (cons x y) freq) item)
  (printf "~a → ~a count: ~a frequency: ~a %\n"
          x y (~a freq #:min-width 8 #:align 'right) (~r (* 100 freq (/ 1 limit)) #:precision '(= 2))))
```


```txt

1000000 first primes. Transitions prime % 10 → next-prime % 10.
1 → 1 count:    42853 frequency: 4.29 %
1 → 3 count:    77475 frequency: 7.75 %
1 → 7 count:    79453 frequency: 7.95 %
1 → 9 count:    50153 frequency: 5.02 %
2 → 3 count:        1 frequency: 0.00 %
3 → 1 count:    58255 frequency: 5.83 %
3 → 3 count:    39668 frequency: 3.97 %
3 → 5 count:        1 frequency: 0.00 %
3 → 7 count:    72827 frequency: 7.28 %
3 → 9 count:    79358 frequency: 7.94 %
5 → 7 count:        1 frequency: 0.00 %
7 → 1 count:    64230 frequency: 6.42 %
7 → 3 count:    68595 frequency: 6.86 %
7 → 7 count:    39603 frequency: 3.96 %
7 → 9 count:    77586 frequency: 7.76 %
9 → 1 count:    84596 frequency: 8.46 %
9 → 3 count:    64371 frequency: 6.44 %
9 → 7 count:    58130 frequency: 5.81 %
9 → 9 count:    42843 frequency: 4.28 %

```



## REXX

The first   '''do'''   loop is a modified ''Sieve of Eratosthenes''     (just for odd numbers).

```rexx
/*REXX pgm shows a table of what last digit follows the previous last digit for N primes*/
parse arg N .                                    /*N:  the number of primes to be genned*/
if N=='' | N==","  then N=1000000                /*Not specified?  Then use the default.*/
Np=N+1;                 w=length(N-1)            /*W:  width used for formatting output.*/
H=N* (2**max(4, (w%2+1) ) )                      /*used as a rough limit for the sieve. */
@.=.                                             /*assume all numbers are prime (so far)*/
#=1                                              /*primes found so far {assume prime 2}.*/
     do j=3  by 2;   if @.j==''  then iterate    /*Is composite?  Then skip this number.*/
     #=#+1                                       /*bump the prime number counter.       */
             do m=j*j  to H  by j+j;  @.m=;  end /*strike odd multiples as composite.   */
     if #==Np  then leave                        /*Enough primes?   Then done with gen. */
     end   /*j*/                                 /* [↑]  gen using Eratosthenes' sieve. */
!.=0                                             /*initialize all the frequency counters*/
say 'For ' N " primes used in this study:"       /*show hdr information about this run. */
r=2                                              /*the last digit of the very 1st prime.*/
#=1                                              /*the number of primes looked at so far*/
     do i=3  by 2;   if @.i==''  then iterate    /*This number composite? Then ignore it*/
     #=#+1;          parse var  i  ''  -1  x     /*bump prime counter; get its last dig.*/
     !.r.x=!.r.x+1;  r=x                         /*bump the last digit counter for prev.*/
     if #==Np  then leave                        /*Done?   Then leave this  DO  loop.   */
     end   /*i*/                                 /* [↑]  examine almost all odd numbers.*/
say                                              /* [↓]  display the results to the term*/
     do d=1  for 9;  if d//2 | d==2  then say    /*display a blank line (if appropriate)*/
        do f=1  for 9; if !.d.f==0  then iterate /*Frequency count=0?   Then don't show.*/
        say 'digit '      d      "──►"      f      ' has a count of: ',
             right(!.d.f, w)",  frequency of:"     right(format(!.d.f/N*100, , 4)'%.', 10)
        end   /*v*/
     end      /*d*/                              /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default input:

```txt

For  1000000  primes used in this study:


digit  1 ──► 1  has a count of:   42853,  frequency of:   4.2853%.
digit  1 ──► 3  has a count of:   77475,  frequency of:   7.7475%.
digit  1 ──► 7  has a count of:   79453,  frequency of:   7.9453%.
digit  1 ──► 9  has a count of:   50153,  frequency of:   5.0153%.

digit  2 ──► 3  has a count of:       1,  frequency of:   0.0001%.

digit  3 ──► 1  has a count of:   58255,  frequency of:   5.8255%.
digit  3 ──► 3  has a count of:   39668,  frequency of:   3.9668%.
digit  3 ──► 5  has a count of:       1,  frequency of:   0.0001%.
digit  3 ──► 7  has a count of:   72828,  frequency of:   7.2828%.
digit  3 ──► 9  has a count of:   79358,  frequency of:   7.9358%.

digit  5 ──► 7  has a count of:       1,  frequency of:   0.0001%.

digit  7 ──► 1  has a count of:   64230,  frequency of:   6.4230%.
digit  7 ──► 3  has a count of:   68595,  frequency of:   6.8595%.
digit  7 ──► 7  has a count of:   39603,  frequency of:   3.9603%.
digit  7 ──► 9  has a count of:   77586,  frequency of:   7.7586%.

digit  9 ──► 1  has a count of:   84596,  frequency of:   8.4596%.
digit  9 ──► 3  has a count of:   64371,  frequency of:   6.4371%.
digit  9 ──► 7  has a count of:   58130,  frequency of:   5.8130%.
digit  9 ──► 9  has a count of:   42843,  frequency of:   4.2843%.

```



## Ruby


```ruby
require "prime"

def prime_conspiracy(m)
  conspiracy = Hash.new(0)
  Prime.take(m).map{|n| n%10}.each_cons(2){|a,b| conspiracy[[a,b]] += 1}
  puts "#{m} first primes. Transitions prime % 10 → next-prime % 10."
  conspiracy.sort.each do |(a,b),v|
    puts "%d → %d count:%10d frequency:%7.4f %" % [a, b, v, 100.0*v/m]
  end
end

prime_conspiracy(1_000_000)
```

```txt
1000000 first primes. Transitions prime % 10 → next-prime % 10.
1 → 1 count:     42853 frequency: 4.2853 %
1 → 3 count:     77475 frequency: 7.7475 %
1 → 7 count:     79453 frequency: 7.9453 %
1 → 9 count:     50153 frequency: 5.0153 %
2 → 3 count:         1 frequency: 0.0001 %
3 → 1 count:     58255 frequency: 5.8255 %
3 → 3 count:     39668 frequency: 3.9668 %
3 → 5 count:         1 frequency: 0.0001 %
3 → 7 count:     72827 frequency: 7.2827 %
3 → 9 count:     79358 frequency: 7.9358 %
5 → 7 count:         1 frequency: 0.0001 %
7 → 1 count:     64230 frequency: 6.4230 %
7 → 3 count:     68595 frequency: 6.8595 %
7 → 7 count:     39603 frequency: 3.9603 %
7 → 9 count:     77586 frequency: 7.7586 %
9 → 1 count:     84596 frequency: 8.4596 %
9 → 3 count:     64371 frequency: 6.4371 %
9 → 7 count:     58130 frequency: 5.8130 %
9 → 9 count:     42843 frequency: 4.2843 %
```


## Scala

===Imperative version (Ugly, side effects)===
Con: Has to unfair assume the one millionth prime.

```Scala
import scala.annotation.tailrec
import scala.collection.mutable

object PrimeConspiracy extends App {
  val limit = 1000000
  val sieveTop = 15485863/*one millionth prime*/ + 1
  val buckets = Array.ofDim[Int](10, 10)
  var prevPrime = 2

  def sieve(limit: Int) = {
    val composite = new mutable.BitSet(sieveTop)
    composite(0) = true
    composite(1) = true

    for (n <- 2 to math.sqrt(limit).toInt)
      if (!composite(n)) for (k <- n * n until limit by n) composite(k) = true
    composite
  }

  val notPrime = sieve(sieveTop)

  def isPrime(n: Long) = {
    @tailrec
    def inner(d: Int, end: Int): Boolean = {
      if (d > end) true
      else if (n % d != 0 && n % (d + 2) != 0) inner(d + 6, end) else false
    }

    n > 1 && ((n & 1) != 0 || n == 2) &&
      (n % 3 != 0 || n == 3) && inner(5, math.sqrt(n).toInt)
  }

  var primeCount = 1
  var n = 3

  while (primeCount < limit) {
    if (!notPrime(n)) {
      val prime = n
      buckets(prevPrime % 10)(prime % 10) += 1
      prevPrime = prime
      primeCount += 1
    }
    n += 1
  }

  for {i <- buckets.indices
       j <- buckets.head.indices} {
    val nPrime = buckets(i)(j)
    if (nPrime != 0) println(f"$i%d -> $j%d : $nPrime%5d  ${nPrime / (limit / 100.0)}%2f")
  }

  println(s"Successfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")
}
```

===Functional version, memoizatized===

```Scala
object PrimeConspiracy1 extends App {
  private val oddPrimes: Stream[Int] =
    3 #:: Stream.from(5, 2)
      .filter(n => oddPrimes.takeWhile(k => k * k <= n).forall(d => n % d != 0))
  val limit = 1000000

  println(s"Population: $limit primes,")
  println(s"Last considered prime ${oddPrimes(limit - 2)}")
  val lsd = oddPrimes.take(limit).par.map(_ % 10)

  val results: Seq[(((Int, Int), Int), Int)] =
    (2 +: lsd).zip(lsd)
      .groupBy(identity).map { case (k, v) => (k, v.size) }
      .toList.sortBy { case ((_, _), n) => -n }.zipWithIndex // Add ranking
      .sorted

  results.foreach { case (((i, j), nPrime), rank) =>
    println(f"$i%d -> $j%d : $nPrime%5d  ${nPrime / (limit / 100.0)}%2f rank:${rank + 1}%3d")
  }
  // println(results.map { case (((_, _), n), _) => n }.sum)

  println(s"Successfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")
}
```



## Seed7

The program below uses the [http://seed7.sourceforge.net/algorith/math.htm#sieve_of_eratosthenes Sieve of Eratosthenes],
to create a set of prime numbers.
The set of prime numbers is [http://seed7.sourceforge.net/faq.htm#initialize_data assigned to a constant].
This way the set of prime numbers is computed at compile-time.
Interesting fact: The Seed7 interpreter takes around 2 seconds to parse and execute the program.
Executing the compiled C++ solution of this task takes around 8 seconds.
Executing the [http://seed7.sourceforge.net/faq.htm#compile compiled] Seed7 program takes only 0.08 seconds.


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const func set of integer: eratosthenes (in integer: n) is func
  result
    var set of integer: sieve is EMPTY_SET;
  local
    var integer: i is 0;
    var integer: j is 0;
  begin
    sieve := {2 .. n};
    for i range 2 to sqrt(n) do
      if i in sieve then
        for j range i ** 2 to n step i do
          excl(sieve, j);
        end for;
      end if;
    end for;
  end func;

const type: countHashType is hash [string] integer;

const proc: main is func
  local
    const set of integer: primes is eratosthenes(15485863);
    var integer: lastPrime is 0;
    var integer: currentPrime is 0;
    var string: aKey is "";
    var countHashType: countHash is countHashType.value;
    var integer: count is 0;
    var integer: total is 0;
  begin
    for currentPrime range primes do
      if lastPrime <> 0 then
        incr(total);
        aKey := str(lastPrime rem 10) <& " -> " <& str(currentPrime rem 10);
	if aKey in countHash then
	  incr(countHash[aKey]);
	else
	  countHash @:= [aKey] 1;
	end if;
      end if;
      lastPrime := currentPrime;
    end for;
    for aKey range sort(keys(countHash)) do
      count := countHash[aKey];
      writeln(aKey <& " count: " <& count lpad 5 <& " frequency: " <&
              flt(count * 100)/flt(total) digits 2 lpad 4 <& " %");
    end for;
  end func;
```


```txt

1 -> 1 count: 42853 frequency: 4.29 %
1 -> 3 count: 77475 frequency: 7.75 %
1 -> 7 count: 79453 frequency: 7.95 %
1 -> 9 count: 50153 frequency: 5.02 %
2 -> 3 count:     1 frequency: 0.00 %
3 -> 1 count: 58255 frequency: 5.83 %
3 -> 3 count: 39668 frequency: 3.97 %
3 -> 5 count:     1 frequency: 0.00 %
3 -> 7 count: 72827 frequency: 7.28 %
3 -> 9 count: 79358 frequency: 7.94 %
5 -> 7 count:     1 frequency: 0.00 %
7 -> 1 count: 64230 frequency: 6.42 %
7 -> 3 count: 68595 frequency: 6.86 %
7 -> 7 count: 39603 frequency: 3.96 %
7 -> 9 count: 77586 frequency: 7.76 %
9 -> 1 count: 84596 frequency: 8.46 %
9 -> 3 count: 64371 frequency: 6.44 %
9 -> 7 count: 58130 frequency: 5.81 %
9 -> 9 count: 42843 frequency: 4.28 %

```



## Sidef

```ruby
var primes = (^Inf -> lazy.grep{.is_prime})

var upto = 1e6
var conspiracy = Hash()

primes.first(upto+1).reduce { |a,b|
    var d = b%10
    conspiracy{"#{a} → #{d}"} := 0 ++
    d
}

for k,v in (conspiracy.sort_by{|k,_v| k }) {
    printf("%s count: %6s\tfrequency: %2.2f %\n", k, v.commify, v / upto * 100)
}
```

```txt

1 → 1 count: 42,853	frequency: 4.29 %
1 → 3 count: 77,475	frequency: 7.75 %
1 → 7 count: 79,453	frequency: 7.95 %
1 → 9 count: 50,153	frequency: 5.02 %
2 → 3 count:      1	frequency: 0.00 %
3 → 1 count: 58,255	frequency: 5.83 %
3 → 3 count: 39,668	frequency: 3.97 %
3 → 5 count:      1	frequency: 0.00 %
3 → 7 count: 72,828	frequency: 7.28 %
3 → 9 count: 79,358	frequency: 7.94 %
5 → 7 count:      1	frequency: 0.00 %
7 → 1 count: 64,230	frequency: 6.42 %
7 → 3 count: 68,595	frequency: 6.86 %
7 → 7 count: 39,603	frequency: 3.96 %
7 → 9 count: 77,586	frequency: 7.76 %
9 → 1 count: 84,596	frequency: 8.46 %
9 → 3 count: 64,371	frequency: 6.44 %
9 → 7 count: 58,130	frequency: 5.81 %
9 → 9 count: 42,843	frequency: 4.28 %

```



## VBA


```vb

Option Explicit

Sub Main()
Dim Dict As Object, L() As Long
Dim t As Single

   Init Dict
   L = ListPrimes(100000000)
t = Timer
   PrimeConspiracy L, Dict, 1000000
Debug.Print "----------------------------"
Debug.Print "Execution time : " & Format(Timer - t, "0.000s.")
Debug.Print ""
   Init Dict
t = Timer
   PrimeConspiracy L, Dict, 5000000
Debug.Print "----------------------------"
Debug.Print "Execution time : " & Format(Timer - t, "0.000s.")
End Sub

Private Function ListPrimes(MAX As Long) As Long()
'http://rosettacode.org/wiki/Extensible_prime_generator#VBA
Dim t() As Boolean, L() As Long, c As Long, s As Long, i As Long, j As Long
    ReDim t(2 To MAX)
    ReDim L(MAX \ 2)
    s = Sqr(MAX)
    For i = 3 To s Step 2
        If t(i) = False Then
            For j = i * i To MAX Step i
                t(j) = True
            Next
        End If
    Next i
    L(0) = 2
    For i = 3 To MAX Step 2
        If t(i) = False Then
            c = c + 1
            L(c) = i
        End If
    Next i
    ReDim Preserve L(c)
    ListPrimes = L
End Function

Private Sub Init(d As Object)
   Set d = CreateObject("Scripting.Dictionary")
   d("1 to 1") = 0
   d("1 to 3") = 0
   d("1 to 7") = 0
   d("1 to 9") = 0
   d("2 to 3") = 0
   d("3 to 1") = 0
   d("3 to 3") = 0
   d("3 to 5") = 0
   d("3 to 7") = 0
   d("3 to 9") = 0
   d("5 to 7") = 0
   d("7 to 1") = 0
   d("7 to 3") = 0
   d("7 to 7") = 0
   d("7 to 9") = 0
   d("9 to 1") = 0
   d("9 to 3") = 0
   d("9 to 7") = 0
   d("9 to 9") = 0
End Sub

Private Sub PrimeConspiracy(Primes() As Long, Dict As Object, Nb)
Dim n As Long, temp As String, r, s, K
   For n = LBound(Primes) To Nb
      r = CStr((Primes(n)))
      s = CStr((Primes(n + 1)))
      temp = Right(r, 1) & " to " & Right(s, 1)
      If Dict.Exists(temp) Then Dict(temp) = Dict(temp) + 1
   Next
   Debug.Print Nb & " primes, last prime considered: " & Primes(Nb)
   Debug.Print "Transition  Count     Frequency"
   Debug.Print "
### =======  =======   ======
"
   For Each K In Dict.Keys
      Debug.Print K & "      " & Right("      " & Dict(K), 6) & "    " & Dict(K) / Nb * 100 & "%"
   Next
End Sub
```

```txt
1000000 primes, last prime considered: 15485867
Transition  Count     Frequency

### =======  =======   ======

1 to 1       42853    4,2853%
1 to 3       77475    7,7475%
1 to 7       79453    7,9453%
1 to 9       50153    5,0153%
2 to 3           1    0,0001%
3 to 1       58255    5,8255%
3 to 3       39668    3,9668%
3 to 5           1    0,0001%
3 to 7       72828    7,2828%
3 to 9       79358    7,9358%
5 to 7           1    0,0001%
7 to 1       64230    6,423%
7 to 3       68595    6,8595%
7 to 7       39604    3,9604%
7 to 9       77586    7,7586%
9 to 1       84596    8,4596%
9 to 3       64371    6,4371%
9 to 7       58130    5,813%
9 to 9       42843    4,2843%
----------------------------
Execution time : 6,969s.

5000000 primes, last prime considered: 86028157
Transition  Count     Frequency

### =======  =======   ======

1 to 1      220539    4,41078%
1 to 3      380424    7,60848%
1 to 7      388660    7,7732%
1 to 9      260209    5,20418%
2 to 3           1    0,00002%
3 to 1      295354    5,90708%
3 to 3      207458    4,14916%
3 to 5           1    0,00002%
3 to 7      358921    7,17842%
3 to 9      388461    7,76922%
5 to 7           1    0,00002%
7 to 1      319814    6,39628%
7 to 3      341870    6,8374%
7 to 7      207687    4,15374%
7 to 9      380708    7,61416%
9 to 1      414126    8,28252%
9 to 3      320442    6,40884%
9 to 7      294810    5,8962%
9 to 9      220515    4,4103%
----------------------------
Execution time : 35,816s.
```



## zkl

Using [[Extensible prime generator#zkl]].

```zkl
const CNT  =0d1_000_000;
sieve     :=Import("sieve.zkl",False,False,False).postponed_sieve;
conspiracy:=Dictionary();
Utils.Generator(sieve).reduce(CNT,'wrap(digit,p){
   d:=p%10;
   conspiracy.incV("%d → %d count:".fmt(digit,d));
   d
});
foreach key in (conspiracy.keys.sort()){ v:=conspiracy[key].toFloat();
   println("%s%,6d\tfrequency: %2.2F%".fmt(key,v,v/CNT *100))
}
```

```txt

1 → 1 count:42,853	frequency:  4.29%
1 → 3 count:77,475	frequency:  7.75%
1 → 7 count:79,453	frequency:  7.95%
1 → 9 count:50,153	frequency:  5.02%
2 → 3 count:     1	frequency:  0.00%
3 → 1 count:58,255	frequency:  5.83%
3 → 3 count:39,668	frequency:  3.97%
3 → 5 count:     1	frequency:  0.00%
3 → 7 count:72,827	frequency:  7.28%
3 → 9 count:79,358	frequency:  7.94%
5 → 7 count:     1	frequency:  0.00%
7 → 1 count:64,230	frequency:  6.42%
7 → 3 count:68,595	frequency:  6.86%
7 → 7 count:39,603	frequency:  3.96%
7 → 9 count:77,586	frequency:  7.76%
9 → 1 count:84,596	frequency:  8.46%
9 → 3 count:64,371	frequency:  6.44%
9 → 7 count:58,130	frequency:  5.81%
9 → 9 count:42,843	frequency:  4.28%

```

Extra credit:
```txt

1 → 1 count:4,623,041	frequency:  4.62%
1 → 3 count:7,429,438	frequency:  7.43%
1 → 7 count:7,504,612	frequency:  7.50%
1 → 9 count:5,442,344	frequency:  5.44%
2 → 3 count:     1	frequency:  0.00%
3 → 1 count:6,010,981	frequency:  6.01%
3 → 3 count:4,442,561	frequency:  4.44%
3 → 5 count:     1	frequency:  0.00%
3 → 7 count:7,043,695	frequency:  7.04%
3 → 9 count:7,502,896	frequency:  7.50%
5 → 7 count:     1	frequency:  0.00%
7 → 1 count:6,373,982	frequency:  6.37%
7 → 3 count:6,755,195	frequency:  6.76%
7 → 7 count:4,439,355	frequency:  4.44%
7 → 9 count:7,431,870	frequency:  7.43%
9 → 1 count:7,991,431	frequency:  7.99%
9 → 3 count:6,372,940	frequency:  6.37%
9 → 7 count:6,012,739	frequency:  6.01%
9 → 9 count:4,622,916	frequency:  4.62%

```

