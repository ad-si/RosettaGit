+++
title = "Primorial numbers"
description = ""
date = 2019-09-15T21:26:58Z
aliases = []
[extra]
id = 19245
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
languages = [
  "c",
  "clojure",
  "common_lisp",
  "d",
  "elixir",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "lingo",
  "mathematica",
  "nickle",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "zkl",
]
+++

## Task

Primorial numbers are those formed by multiplying successive prime numbers. 



The primorial number series is:

::*   primorial(0) =         1       (by definition)
::*   primorial(1) =         2       (2)
::*   primorial(2) =         6       (2*3)
::*   primorial(3) =             30       (2*3*5)
::*   primorial(4) =                 210       (2*3*5*7)
::*   primorial(5) =                     2310       (2*3*5*7*11)
::*   primorial(6) =                         30030       (2*3*5*7*11*13)
:;*         <big><b>∙ ∙ ∙</b></big>

To express this mathematically,   '''primorial<sub><big>''n''</big></sub>'''   is  
the product of the first   <big>''n''</big>   (successive) primes: 

<big><big><big>
:   <math>primorial_n = \prod_{k=1}^n prime_k</math>
</big></big>
:::::: ─── where   <big><big><math>prime_k</math></big></big>   is the   <big><big>''k''<sup>''th''</sup>''</big></big>   prime number.
</big>


In some sense, generating primorial numbers is similar to factorials. 

As with factorials, primorial numbers get large quickly.


'''task requirements:'''

*   Show the first ten primorial numbers   (0 ──► 9,   inclusive).
*   Show the length of primorial numbers whose index is:   10   100   1,000   10,000   and   100,000.
*   Show the length of the one millionth primorial number   (optional). 
*   Use exact integers, not approximations.  
 

By   ''length''   (above), it is meant the number of decimal digits in the numbers. 

 

'''links:'''

See the MathWorld webpage:   [http://mathworld.wolfram.com/Primorial.html primorial]

See the Wikipedia   webpage:   [http://en.wikipedia.org/wiki/Primorial primorial].

See the     OEIS     webpage:   [http://oeis.org/A002110 A2110].


'''Related tasks:'''
*[[Factorial]]
*[[Sequence of primorial primes]]






## C

Uses a custom bit-sieve to generate primes, and GMP to keep track of the value of the primorial. Output takes ~3m to generate on a typical laptop.

```c

#include <inttypes.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <gmp.h>

/* Eratosthenes bit-sieve */
int es_check(uint32_t *sieve, uint64_t n)
{
    if ((n != 2 && !(n & 1)) || (n < 2))
        return 0;
    else
        return !(sieve[n >> 6] & (1 << (n >> 1 & 31)));
}

uint32_t *es_sieve(const uint64_t nth, uint64_t *es_size)
{
    *es_size = nth * log(nth) + nth * (log(log(nth)) - 0.9385f) + 1;
    uint32_t *sieve = calloc((*es_size >> 6) + 1, sizeof(uint32_t));

    for (uint64_t i = 3; i < sqrt(*es_size) + 1; i += 2)
        if (!(sieve[i >> 6] & (1 << (i >> 1 & 31))))
            for (uint64_t j = i * i; j < *es_size; j += (i << 1))
                sieve[j >> 6] |= (1 << (j >> 1 & 31));

    return sieve;
}

size_t mpz_number_of_digits(const mpz_t op)
{
    char *opstr = mpz_get_str(NULL, 10, op);
    const size_t oplen = strlen(opstr);
    free(opstr);
    return oplen;
}

#define PRIMORIAL_LIMIT 1000000

int main(void)
{
    /* Construct a sieve of the first 1,000,000 primes */
    uint64_t sieve_size;
    uint32_t *sieve = es_sieve(PRIMORIAL_LIMIT, &sieve_size);

    mpz_t primorial;
    mpz_init_set_ui(primorial, 1);

    uint64_t prime_count = 0;
    int print = 1;
    double unused;

    for (uint64_t i = 2; i < sieve_size && prime_count <= PRIMORIAL_LIMIT; ++i) {
        if (print) {
            if (prime_count < 10)
                gmp_printf("Primorial(%" PRIu64 ") = %Zd\n", prime_count, primorial);
            /* Is the current number a power of 10? */
            else if (!modf(log10(prime_count), &unused))
                printf("Primorial(%" PRIu64 ") has %zu digits\n", prime_count, mpz_number_of_digits(primorial));
            print = 0;
        }

        if (es_check(sieve, i)) {
            mpz_mul_ui(primorial, primorial, i);
            prime_count++;
            print = 1;
        }

    }

    free(sieve);
    mpz_clear(primorial);
    return 0;
}

```

```txt

Primorial(0) = 1
Primorial(1) = 2
Primorial(2) = 6
Primorial(3) = 30
Primorial(4) = 210
Primorial(5) = 2310
Primorial(6) = 30030
Primorial(7) = 510510
Primorial(8) = 9699690
Primorial(9) = 223092870
Primorial(10) has 10 digits
Primorial(100) has 220 digits
Primorial(1000) has 3393 digits
Primorial(10000) has 45337 digits
Primorial(100000) has 563921 digits
Primorial(1000000) has 6722809 digits

```


## Clojure


### Single Process

Using HashMap prime number generation from https://rosettacode.org/wiki/Sieve_of_Eratosthenes#Unbounded_Versions

```lisp
(ns example
  (:gen-class))
  
; Generate Prime Numbers (Implementation from RosettaCode--link above)
(defn primes-hashmap
  "Infinite sequence of primes using an incremental Sieve or Eratosthenes with a Hashmap"
  []
  (letfn [(nxtoddprm [c q bsprms cmpsts]
            (if (>= c q) ;; only ever equal
              ; Update cmpsts with primes up to sqrt c
              (let [p2 (* (first bsprms) 2),
                    nbps (next bsprms),
                    nbp (first nbps)]
                (recur (+ c 2) (* nbp nbp) nbps (assoc cmpsts (+ q p2) p2)))

              (if (contains? cmpsts c)
                ; Not prime
                (recur (+ c 2) q bsprms
                       (let [adv (cmpsts c), ncmps (dissoc cmpsts c)]
                         (assoc ncmps
                           (loop [try (+ c adv)] ;; ensure map entry is unique
                             (if (contains? ncmps try)
                               (recur (+ try adv))
                               try))
                           adv)))
                ; prime
                (cons c (lazy-seq (nxtoddprm (+ c 2) q bsprms cmpsts))))))]
    (do (def baseoddprms (cons 3 (lazy-seq (nxtoddprm 5 9 baseoddprms {}))))
        (cons 2 (lazy-seq (nxtoddprm 3 9 baseoddprms {}))))))
		
;; Generate Primorial Numbers
(defn primorial [n]
  " Function produces the nth primorial number"
  (if (= n 0)
    1                                                           ; by definition
    (reduce *' (take n (primes-hashmap)))))                     ; multiply first n primes (retrieving primes from lazy-seq which generates primes as needed)
	
;; Show Results
(let [start (System/nanoTime)
      elapsed-secs (fn [] (/ (- (System/nanoTime) start) 1e9))]   ; System start time
  (doseq [i (concat (range 10) [1e2 1e3 1e4 1e5 1e6])
          :let [p (primorial i)]]                               ; Generate ith primorial number
    (if (< i 10)
      (println (format "primorial ( %7d ) = %10d" i (biginteger p)))         ; Output for first 10
      (println (format "primorial ( %7d ) has %8d digits\tafter %.3f secs"   ; Output with time since starting for remainder
                       (long i) (count (str p)) (elapsed-secs))))))

```

```txt

primorial (       0 ) =          1
primorial (       1 ) =          2
primorial (       2 ) =          6
primorial (       3 ) =         30
primorial (       4 ) =        210
primorial (       5 ) =       2310
primorial (       6 ) =      30030
primorial (       7 ) =     510510
primorial (       8 ) =    9699690
primorial (       9 ) =  223092870
primorial (     100 ) has      220 digits	after 0.012 secs
primorial (    1000 ) has     3393 digits	after 0.048 secs
primorial (   10000 ) has    45337 digits	after 0.284 secs
primorial (  100000 ) has   563921 digits	after 7.731 secs
primorial ( 1000000 ) has  6722809 digits	after 706.593 secs
Using: i7 920 @ 2.67 GHz CPU with Windows 10 /64 bit OS

```



### Parallel Process

Using HashMap prime number generation from https://rosettacode.org/wiki/Sieve_of_Eratosthenes#Unbounded_Versions

```lisp
(ns example
  (:gen-class))
(defn primes-hashmap
  "Infinite sequence of primes using an incremental Sieve or Eratosthenes with a Hashmap"
  []
  (letfn [(nxtoddprm [c q bsprms cmpsts]
            (if (>= c q) ;; only ever equal
              ; Update cmpsts with primes up to sqrt c
              (let [p2 (* (first bsprms) 2),
                    nbps (next bsprms),
                    nbp (first nbps)]
                (recur (+ c 2) (* nbp nbp) nbps (assoc cmpsts (+ q p2) p2)))

              (if (contains? cmpsts c)
                ; Not prime
                (recur (+ c 2) q bsprms
                       (let [adv (cmpsts c), ncmps (dissoc cmpsts c)]
                         (assoc ncmps
                           (loop [try (+ c adv)] ;; ensure map entry is unique
                             (if (contains? ncmps try)
                               (recur (+ try adv))
                               try))
                           adv)))
                ; prime
                (cons c (lazy-seq (nxtoddprm (+ c 2) q bsprms cmpsts))))))]
    (do (def baseoddprms (cons 3 (lazy-seq (nxtoddprm 5 9 baseoddprms {}))))
        (cons 2 (lazy-seq (nxtoddprm 3 9 baseoddprms {}))))))

;; Number of workers (threads) based upon number of available processors
(def workers
  (+ 2 (.. Runtime getRuntime availableProcessors)))

;; Generate of primorial numbers (using multiple processors)
(defn primorial [n]
  (if (= n 0)
    1
    ;(reduce mul+ (pmap #(reduce *' %) (partition-all (max workers (long (/ n workers))) (take n (primes-hashmap)))))));(*' allows for big integer arithmetic as needed)
    (->>                                                      ; Threads (i.e. pipes) sequence of expressions
        (take n (primes-hashmap) )                            ; generate primes
        (partition-all (max workers (long (/ n workers))))    ; partition primes amongst workers
        (pmap #(reduce *' %))                                 ; Multiply primes in each worker in parallel
        (reduce *'))))                                        ; multiply results of all workers together

;; Generate and Time Output
(let [start (System/nanoTime)
      elapsed-secs (fn [] (/ (- (System/nanoTime) start) 1e9))]                 ; System start time
  (doseq [i (concat (range 10) [1e2 1e3 1e4 1e5 1e6])
          :let [p (primorial i)]]                               ; Generate ith primorial number
    (if (< i 10)
      (println (format "primorial ( %7d ) = %10d" i (biginteger p)))                      ; Output for first 10
      (println (format "primorial ( %7d ) has %8d digits\tafter %.3f secs"   ; Output with time since starting for remainder
                       (long i) (count (str p)) (elapsed-secs))))))

```

```txt

primorial (       0 ) =          1
primorial (       1 ) =          2
primorial (       2 ) =          6
primorial (       3 ) =         30
primorial (       4 ) =        210
primorial (       5 ) =       2310
primorial (       6 ) =      30030
primorial (       7 ) =     510510
primorial (       8 ) =    9699690
primorial (       9 ) =  223092870
primorial (     100 ) has      220 digits	after 0.016 secs
primorial (    1000 ) has     3393 digits	after 0.050 secs
primorial (   10000 ) has    45337 digits	after 0.364 secs
primorial (  100000 ) has   563921 digits	after 2.619 secs
primorial ( 1000000 ) has  6722809 digits	after 69.812 secs
Using: i7 920 @ 2.67 GHz CPU with Windows 10 /64 bit OS

```



## Common Lisp


```lisp

(defun primorial-number-length (n w)
  (values (primorial-number n) (primorial-length w)))

(defun primorial-number (n)
  (loop for a below n collect (primorial a)))

(defun primorial-length (w)
  (loop for a in w collect (length (write-to-string (primorial a)))))

(defun primorial (n &optional (m 1) (k -1) (z 1) &aux (f (primep m)))
  (if (= k n) z (primorial n (1+ m) (+ k (if f 1 0)) (if f (* m z) z))))

(defun primep (n)
  (loop for a from 2 to (isqrt n) never (zerop (mod n a))))

```

```txt

> (primorial-number-length 10 '(100 1000 10000 100000))
(1 2 6 30 210 2310 30030 510510 9699690 223092870)
(220 3393 45337 563921)

```



## D

```d

import std.stdio;
import std.format;
import std.bigint;
import std.math;
import std.algorithm;


int sieveLimit = 1300_000;

bool[] notPrime;

void main()
{
  // initialize
  sieve(sieveLimit);

  // output 1	
  foreach (i; 0..10)
    writefln("primorial(%d): %d", i, primorial(i));

  // output 2
  foreach (i; 1..6)
    writefln("primorial(10^%d) has length %d", i, count(format("%d", primorial(pow(10, i)))));

}

BigInt primorial(int n)
{
  if (n == 0) return BigInt(1);

  BigInt result = BigInt(1);
  for (int i = 0; i < sieveLimit && n > 0; i++)
  {
    if (notPrime[i]) continue;
    result *= BigInt(i);
    n--;
  }
  return result;
}

void sieve(int limit)
{
  notPrime = new bool[limit];
  notPrime[0] = notPrime[1] = true;

  auto max = sqrt(cast (float) limit);
  for (int n = 2; n <= max; n++)
  {
    if (!notPrime[n])
    {
      for (int k = n * n; k < limit; k += n)
      {
        notPrime[k] = true;
      }
    }
  }
}


```


```txt

primorial(0): 1
primorial(1): 2
primorial(2): 6
primorial(3): 30
primorial(4): 210
primorial(5): 2310
primorial(6): 30030
primorial(7): 510510
primorial(8): 9699690
primorial(9): 223092870
primorial(10^1) has length 10
primorial(10^2) has length 220
primorial(10^3) has length 3393
primorial(10^4) has length 45337
primorial(10^5) has length 563921


```



## Elixir

Prime generator works too inefficiently to generate 1 million in a short time, but it will work eventually. This solution is efficient up to 100,000 primes.


```Elixir

defmodule SieveofEratosthenes do
  def init(lim) do
    find_primes(2,lim,(2..lim))
  end

  def find_primes(count,lim,nums) when (count * count) > lim do
    nums
  end

  def find_primes(count,lim,nums) when (count * count) <= lim do
    find_primes(count+1,lim,Enum.reject(nums,&(rem(&1,count) == 0 and &1 > count)))
  end
end

```



```Elixir

defmodule Primorial do
  def first(n,primes) do
    s = 0..9 |> Stream.map(fn n -> Enum.at(primes,n) end)
    (0..n-1)
      |> Enum.map(fn a -> s 
        |> Enum.take(a) 
        |> Enum.reduce(1, fn b,c -> b*c end) 
        |> format(a) end)
  end

  def numbers(lims,primes) do
    numbers(lims,primes,[])
  end

  def numbers([],_primes,vals) do
    vals
      |> Enum.reverse
      |> Enum.map(fn {m,n} -> str_fr(n,m) end)
  end

  def numbers([lim|lims],primes,vals) do
    numbers(lims,primes,[{lim,number_length(primes,lim)}] ++ vals)
  end

  defp number_length(primes,n) do
    primes 
      |> Enum.take(n) 
      |> Enum.reduce(fn a,b -> a * b end) 
      |> Integer.to_string 
      |> String.length
  end

  defp format(pri,i), do: IO.puts("Primorial #{i}: #{pri}")
  defp str_fr(pri,i), do: IO.puts("Primorial #{i} has length: #{pri}")
end

```



```Elixir

Primorial.first(10,SieveofEratosthenes.init(50))
Primorial.numbers([10,100,1_000,10_000,100_000],SieveofEratosthenes.init(1_300_000))

```


Primorial 0: 1

Primorial 1: 2

Primorial 2: 6

Primorial 3: 30

Primorial 4: 210

Primorial 5: 2310

Primorial 6: 30030

Primorial 7: 510510

Primorial 8: 9699690

Primorial 9: 223092870

Primorial 10 has length: 10

Primorial 100 has length: 220

Primorial 1000 has length: 3393

Primorial 10000 has length: 45337

Primorial 100000 has length: 563921

=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

### The function to generate a sequence of Primorial Numbers


```fsharp

// Primorial Numbers. Nigel Galloway: November 28th., 2017
let primorialNumbers = seq{
  let N = let N = ref 1I
          (fun (n:int) -> N := !N*(bigint n); !N)
  yield 1I; yield! Seq.map N primes}

```


### The Task


```fsharp

primorialNumbers |> Seq.take 10 |> Seq.iter(fun n->printfn "%A" n)

```

```txt

1
2
6
30
210
2310
30030
510510
9699690
223092870

```


```fsharp

[10;100;1000;10000;100000]|>List.iter(fun n->printfn "%d" ((int)(System.Numerics.BigInteger.Log10 (Seq.item n primorialNumbers))+1))

```

```txt

10
220
3393
45337
563921

```



## Factor

<lang>USING: formatting kernel literals math math.functions
math.primes sequences ;
IN: rosetta-code.primorial-numbers

CONSTANT: primes $[ 1,000,000 nprimes ]

: digit-count ( n -- count ) log10 floor >integer 1 + ;

: primorial ( n -- m ) primes swap head product ;

: .primorial ( n -- ) dup primorial "Primorial(%d) = %d\n"
    printf ;
    
: .digit-count ( n -- ) dup primorial digit-count
    "Primorial(%d) has %d digits\n" printf ;

: part1 ( -- ) 10 iota [ .primorial ] each ;

: part2 ( -- ) { 10 100 1000 10000 100000 1000000 }
    [ .digit-count ] each ;
    
: main ( -- ) part1 part2 ;

MAIN: main
```

```txt

Primorial(0) = 1
Primorial(1) = 2
Primorial(2) = 6
Primorial(3) = 30
Primorial(4) = 210
Primorial(5) = 2310
Primorial(6) = 30030
Primorial(7) = 510510
Primorial(8) = 9699690
Primorial(9) = 223092870
Primorial(10) has 10 digits
Primorial(100) has 220 digits
Primorial(1000) has 3393 digits
Primorial(10000) has 45337 digits
Primorial(100000) has 563921 digits
Primorial(1000000) has 6722809 digits

```



## Fortran


### The Plan

Since exact integer arithmetic is required, there can be no use of various approximations and the primorial numbers very soon exceed the integer limits, even faster than do factorials. As the one who provided the example of calculating factorials in the wikipædia article for multi-precision arithmetic, I am hoist by my own petard.

### =Prepare a list of prime numbers=
 
The first task is to prepare a collection of the first million prime numbers. I already have disc files for up to 4,294,984,663 but this would not be portable so instead a subroutine to prepare the values, which is possibly faster than reading from a disc file anyway. Since this is a largeish set, the division method is probably not as good as the sieve method. Both methods could be employed to generate consecutive prime numbers, interlaced with their use to calculate the next primorial number, but divide-and-conquer is more attractive, so, first, prepare the primes. The sieve is arranged to represent only odd numbers so that there is no tedious pass with every other element, and further, by arranging the length of the sieve to be a primorial number, the pattern of the knock-outs for the first few primes is the same for each usage. A span of 30030 (spanned by an array of 15015 elements) seems reasonable, as the next size up would be 510510 which is a bit big. It would be nice if the initial-value specification protocol could generate that pattern via the repetition of the components in an expression that would be evaluated by the compiler - something like (3-pattern) & (5-pattern) & (7-pattern) & (11-pattern) & (13-pattern), with each pattern being generated by a suitable repetition count - I am certainly not going to write out a sequence of 15015 constants, and there are limits on a statement length anyway so devising a programme to generate the required source statements is not promising. But alas, I can't see how to do this either via a DATA statement or a PARAMETER specification. So, array START is initialised by executable statements that sieve out multiples of 3, 5, 7, 11, and 13 - including their own appearances. Then, a proper sieve process is started, that augments the PRIME collection as it goes at the beginning since the first sieve pass needs prime numbers that are not initially in array PRIME. On the other hand, by abandoning the special initial value array one could double the size of the sieve using no more storage, which might be better. When the sieve process is working with large numbers, there will be fewer and fewer "hits" for each sieve number. With a sieve span of 30,030, a prime of the order of 300,000 will have only one chance in ten of falling within a particular surge (and if so delivers only one hit within it), and the effort for the misses will have been wasted. For this task, the largest prime required for sieving is less than 4,000 so each prime is still making a few hits.

Because the sieve does not represent even numbers, the determination of a starting point is a little tricky. For a given prime P, the requirement is to find the first ''odd'' multiple of P beyond N0 (the start of a span, an even number), and if this value is beyond the end of the span (marked by NN) then there is no need to step through the span with P. Conflating this test with the test for the start point of P*P being beyond the end of the span is a mistake that, with a span of 30, led to 361 being declared a prime, because 360/17 = 21+ so 22 would be the start multiple for 17 except that 22 is even so 23 is used, and 23*17 = 391 which is beyond the end of the span so, wrongly, the sieving was thereby deemed complete and 19 was not tried, a mistake because 19*19 = 361.

The sieve array is declared LOGICAL*1 as it is painful to use the default of a 32-bit storage word to represent a boolean variable. Alas, accessing a single byte of storage may take more time, especially when placing a value. Still worse would be the unpacking of say a 32-bit integer to get at individual bits in isolation. With large arrays, a lot of storage could be saved, but execution speed would be poor unless the hardware offered support for single-bit access.

And after all that fuss, the production of the first million primes is completed in a blink.

====Multi-precision multiplication====
The multiplication of the multi-precision number is done on the cheap by passing the prime as an ordinary integer, not as a big number. The millionth prime is 15,485,863 which fits easily into a 32-bit integer that can hold values up to 2,147,483,647 but this implies a limit on the base of the arithmetic of the big number scheme. If it works in base 1,000 then a digit multiplied by that large prime would yield up to 15,485,863,000 - which exceeds the 32-bit integer limit, while if it works in base 10, the largest number would be up to 154,858,630, well within range. But at the cost of many more digits in the number, and correspondingly longer computation time. Some experiments with calculating Primorial(100000): 
 Base:    10   100  1,000  10,000  100,000
 Secs:   554   278    185     117       52 - but wrong!
 64-bit                       300      241
The overflow happens in D*N + C, and converting D from INTEGER*4 to INTEGER*8 prevented the overflow, but introduces its own slowness. Activating "maximum optimisations" reduced 241 to 199, and reverting to base 100 with no INTEGER*8 converted 278 to 133. Then, abandoning the array bound checking reduced it further, to 121. The end result of the trick is to limit the base to 100. Given that, INTEGER*2 could be used for DIGIT, but a trial run showed almost the same time taken for Primorial(100000).

Hopefully, the compiler recognises the connection between the consecutive statements <code>D = B.DIGIT(I)</code> then <code>D = D*N + C</code> (rather than <code>D = B.DIGIT(I)*N + C</code>) which was done to facilitate trying INTEGER*8 D without needing to blabber on about conversion routines. But more importantly, one can dream that the compiler will recognise the classic sequence 
```Fortran
      B.DIGIT(I) = MOD(D,BIGBASE)
      C = D/BIGBASE
```
 and avoid performing two divisions. When an integer division is performed, the quotient appears in one register and the remainder in another and when writing in assembler with access to such registers there is no need for the two divisions as is forced by the use of high-level languages.

An alternative method, once consecutive primorials are not required, would be to compute the big number product of say ten consecutive primes then multiply the main big number by that, using multi-precision arithmetic for both. Abandoning the trick that restricts the base to 100 (for the current upper limit of the millionth prime) would allow the use of larger bases. Another possibility would be to calculate in a base more directly matching that of the computer (since the variable-length decimal arithmetic of the IBM1620 ''et al'' is no longer available), for instance base 65536 with sixteen-bit words, etc. Inspection of the result to calculate the number of decimal digits required would not be troublesome. In such a case, one might try something like
```Fortran
      INTEGER*4 D              !A 32-bit product.
      INTEGER*2 II(2),C,R      !Some 16-bit variables.
      EQUIVALENCE (D,II)       !Align.
      EQUIVALENCE (II(1),C),(II(2),R) !Carry in the high order half of D, Result in the low.
```

Supposing that unsigned 16-bit arithmetic was available then the product in D need not be split into a carry and a digit via the MOD and divide as above. But this is unlikely. Only in assembly would good code be possible.


### Source

The source style takes advantage of F90 for the MODULE protocol that saves the annoyance of otherwise having to prepare a collection of COMMON statements for those who would not write a single mainline for the job. With F90 the lower bound of array BIT need no longer be one, otherwise, with older Fortran the expressions would have to be recast and one added or subtracted as appropriate. This is a simple (but error-prone) clerical task: computers excel at clerical tasks, so let the computer do it.

Also helpful is the TYPE statement to define a big number with the use of somewhat explanatory nomenclature. Associated names (such as BIGORDER, BIGBASE, etc.) could also have been defined as a part of a TYPE definition, but the extra blabber didn't seem worthwhile when one set only would be used. So, for them the old-style usage of names with a structure in the name's form. More serious is the assistance offered by the abilities of the PARAMETER statement to define constants with interrelationships, here especially with the use of BIGORDER.

Various F90 array statements provide a convenience that could otherwise be achieved via explicit DO-loops, but rather more difficult to do without are the I0 format code (which should be just "I") that reveals an integer value with a size to fit the number, and the I<ORDER>.<ORDER> usage of two features: the parameterisation of what would otherwise be constants, and the F90 augmentation of Iw to Iw.d whereby leading zero digits are supplied. This enables the value of the big number to be revealed as a proper string of digits. Before F90, this would have to be done via writing to a text variable and messing about. Thus, the topmost digit of a big number, B.DIGIT(B.LAST), is sent out with I0 format so that there are neither leading spaces nor zeroes, then the subsequent digits are rolled with I<ORDER>.<ORDER> so that any necessary leading zeroes for each of them are provided - else there would be gaps within the digit string as presented should the base be greater than ten and a B.DIGIT value be less than ten. Then there's the usual trick that once a FORMAT sequence is exhausted by the elements of an I/O list, a new line is started and the scan of the codes (of FORMAT 101) resumes at the rightmost open bracket. This is not exercised for the smaller numbers that are shown in full, but was during testing.

On the other hand, the modifying prefix nP for E (and F) format codes has long been available. This shifts the position of the decimal point left or right by n places, and with the E format code modifies the exponent accordingly. Thus, 1.2345E+5 can become .12345E+6 via the prefix -1P as in FORMAT 113. Evidently, this is the same value. When used with a F code there is no exponent part to modify accordingly, but here, the exponent is calculated separately (and presented via the I0 format code), and, one is added in the WRITE statement's output expressions, thus I4 + 1 and I8 + 1. The point of all this is that the resulting exponent part gives the number of decimal digits in the number, as per the specification.

```Fortran

      MODULE BIGNUMBERS	!Limited services: decimal integers, no negative numbers.
       INTEGER BIGORDER		!A limit attempt at generality.
       PARAMETER (BIGORDER = 2)	!This is the order of the base of the big number arithmetic.
       INTEGER BIGBASE,BIGLIMIT	!Sized thusly.
       PARAMETER (BIGBASE = 10**BIGORDER, BIGLIMIT = 8888888/BIGORDER)	!Enough?
       TYPE BIGNUM	!So, a big number is simple.
        INTEGER LAST		!This many digits (of size BIGBASE) are in use.
        INTEGER DIGIT(BIGLIMIT)	!The digits, in ascending power order.
       END TYPE BIGNUM	!So much for that.
       CONTAINS		!Now for some assistants.
        SUBROUTINE BIGMULT(B,N)	!B:=B*N;	Multiply by an integer possibly bigger than the base.
         TYPE(BIGNUM) B	!The worker.
         INTEGER N	!A computer number, not a multi-digit number.
         INTEGER D	!Must be able to hold (BIGBASE - 1)*N + C
         INTEGER C	!The carry to the next digit.
         INTEGER I	!A stepper.
          C = 0		!No previous digit to carry from.
          DO I = 1,B.LAST	!Step through the digits, upwards powers.
            D = B.DIGIT(I)		!Grab a digit.
            D = D*N + C			!Apply the multiply.
            B.DIGIT(I) = MOD(D,BIGBASE)	!Place the resulting digit.
            C = D/BIGBASE		!Agony! TWO divisions per step!!
          END DO		!On to the next digit up.
          DO WHILE(C .GT. 0)	!Now spread the last carry to further digits.
            B.LAST = B.LAST + 1		!Up one more.
            IF (B.LAST .GT. BIGLIMIT) STOP "Overflow by multiply!"	!Perhaps not.
            B.DIGIT(B.LAST) = MOD(C,BIGBASE)	!The digit.
            C = C/BIGBASE		!The carry may be large, if N is large.
          END DO		!So slog on until it is gone.
        END SUBROUTINE BIGMULT	!Primary school stuff.
      END MODULE BIGNUMBERS	!No fancy tricks.

      MODULE ERATOSTHENES	!Prepare an array of prime numbers.
Considers odd numbers only as the pattern is very simple. Some trickery as a consequence.
       INTEGER NP,LASTP		!Counters.
       PARAMETER (LASTP = 1000000)	!The specified need.
       INTEGER PRIME(0:LASTP),PREZAP	!Initialisation is rather messy.
       PARAMETER (PREZAP = 6)		!Up to PRIME(6) = 13.
       DATA NP/PREZAP/, PRIME(0:PREZAP)/1,2,3,5,7,11,13/	!Not counting the "zeroth" prime, 1.
       CONTAINS	!Some tricky stuff/
        SUBROUTINE PREPARE PRIMES	!Fetch a limited copy of the Platonic ideal.
         INTEGER SURGE,LB		!A sieve has a certain rather special size.
         PARAMETER (SURGE = 30030, LB = SURGE/2 - 1)	!= 2*3*5*7*11*13.
         LOGICAL*1 BIT(0:LB),START(0:LB)!Two such arrays, thanks.
         INTEGER N0,NN		!Bounds for the current sieve span.
         INTEGER I,P,IP		!Assistants.
C  The scheme for a cycle of 2*3*5 = 30, remembering that even numbers are not involved so BIT(0:14).
C             |         surge 1             |         surge 2             |         surge 3             |
C        N =  |          1 1 1 1 1 2 2 2 2 2|3 3 3 3 3 4 4 4 4 4 5 5 5 5 5|6 6 6 6 6 7 7 7 7 7 8 8 8 8 8|9 9 9 9 9...
C             |1 3 5 7 9 1 3 5 7 9 1 3 5 7 9|1 3 5 7 9 1 3 5 7 9 1 3 5 7 9|1 3 5 7 9 1 3 5 7 9 1 3 5 7 9|1 3 5 7 9...
C  BIT(index) |                    1 1 1 1 1|                    1 1 1 1 1|                    1 1 1 1 1|
C             |0 1 2 3 4 5 6 7 8 9 0 1 2 3 4|0 1 2 3 4 5 6 7 8 9 0 1 2 3 4|0 1 2 3 4 5 6 7 8 9 0 1 2 3 4|0 1 2 3 4...
c     3 step  |  *     *     *     *     *  |  *     *     *     *     *  |  *     *     *     *     *  |  *     *
c     5 step  |    *         *         *    |    *         *         *    |    *         *         *    |    *
c     7 step  |      x             x        |    x             *          |  *             *            |*

Concoct the initial state, once only, that repeats every SURGE.
          START = .TRUE.	!Prepare the field.
          DO I = 2,PREZAP	!Only odd numbers are represented, so no PRIME(1) = 2..
            P = PRIME(I)	!Select a step.
            START(P/2:LB:P) = .FALSE.	!Knock out multiples of P.
          END DO		!This pattern is palindromic.
          NN = 0	!Syncopation. Where the previous surge ended.
Commence a pass through the BIT sieve.
   10     N0 = NN		!BIT(0) corresponds to N0 + 1, BIT(i) to N0 + 1 + 2i.
          NN = NN + SURGE	!BIT(LB) to NN - 1.
          BIT = START		!Pre-zapped for lesser primes.
          IP = PREZAP		!Syncopation. The last pre-zapped prime.
   11     IP = IP + 1		!The next prime to sieve with.
          IF (IP.GT.NP) CALL SCANFOR(.TRUE.)	!Whoops, not yet to hand!
          P = PRIME(IP)		!Now grab it.
          IF (P*P.GE.NN) GO TO 12	!If P*P exceeds the end, so will larger P.
          I = N0/P + 1			!First multiple of P past N0.
          IF (I.LT.P) I = P		!Less than P is superfluous: the position was zapped by earlier action.
          IF (MOD(I,2).EQ.0) I = I + 1	!If even, advance to the next odd multiple. Such as P.
          I = I*P			!The first number to zap. It will always be odd.
          IF (I.LT.NN) THEN		!Within the span?
            I = (I - N0 - 1)/2		!Yes. Its offset into the current span.
            BIT(I:LB:P) = .FALSE.	!Zap every P'th position.
          END IF			!So much for that P.
          GO TO 11		!On to the next.
Completed the passes. Scan for survivors.
   12     CALL SCANFOR(.FALSE.)		!All, not just the first new prime.
          IF (NP.LT.LASTP) GO TO 10	!Another batch?
          RETURN		!Done.
         CONTAINS	!Fold two usages into one routine.
          SUBROUTINE SCANFOR(ONE)	!Finds survivors.
           LOGICAL ONE		!Perhaps only the first is desired.
           INTEGER I,P		!Assistants.
            DO I = 0,LB		!Scan the current state.
              IF (BIT(I)) THEN		!Is this one unsullied?
                P = N0 + 1 + 2*I	!Yes! This is its value.
                IF (P.LE.PRIME(NP)) CYCLE	!But we may have it already.
                IF (NP.GE.LASTP) RETURN		!Whoops, perhaps too many!
                NP = NP + 1		!But if not, another new prime!
                PRIME(NP) = P		!So, stash it. Extract now just this one.
                IF (ONE) RETURN		!Later candidates may yet be unzapped.
              END IF			!So much for that value.
            END DO			!On to the next.
          END SUBROUTINE SCANFOR	!An odd IF allows for two usages in one routine.
        END SUBROUTINE PREPARE PRIMES	!Faster than reading from a disc file?
      END MODULE ERATOSTHENES	!Certainly, less storage is required this way.

      PROGRAM PRIMORIAL	!Simple enough, with some assistants.
      USE ERATOSTHENES	!Though probably not as he expected.
      USE BIGNUMBERS	!Just so.
      TYPE(BIGNUM) B	!I'll have one.
      INTEGER P,MARK	!Step stuff.
      INTEGER E,D	!Assistants for the floating-point analogue...
      INTEGER TASTE,IT	!Additional stuff for its rounding.
      PARAMETER (TASTE = 8/BIGORDER)	!Sufficient digits to show.
      INTEGER LEAD(TASTE)		!With a struggle.
      REAL T0,T1	!Some CPU time attempts.
      REAL*4 F4		!I'll also have a go via logs.
      REAL*8 F8		!In two precisions.
      INTEGER I4,I8	!Not much hope for single precision, though.

      WRITE (6,1) LASTP,BIGBASE	!Announce.
    1 FORMAT ("Calculates primorial numbers up to prime ",I0,
     1 ", working in base ",I0)
      CALL PREPARE PRIMES	!First, catch your rabbit.

Commence prime mashing.
  100 B.LAST = 1	!Begin at the beginning.
      B.DIGIT(1) = 1	!With one.
      DO P = 0,9	!Step up to the ninth prime, thus the first ten values as specified.
        CALL BIGMULT(B,PRIME(P))	!Multiply by a possibly large integer.
        WRITE (6,101) P,PRIME(P),P,B.DIGIT(B.LAST:1:-1)	!Digits in Arabic/Hindu order.
  101   FORMAT ("Prime(",I0,") = ",I0,", Primorial(",I0,") = ",
     1   I0,9I<BIGORDER>.<BIGORDER>,/,(10I<BIGORDER>.<BIGORDER>))
      END DO		!On to the next prime.

Convert to logarithmic striders.
      CALL CPU_TIME(T0)	!Start the clock.
      MARK = 10		!To be remarked upon in passing.
      DO P = 10,LASTP	!Step through additional primes.
        CALL BIGMULT(B,PRIME(P))	!Bigger, ever bigger the big number grows.
        IF (P.EQ.MARK) THEN		!A report point?
          MARK = MARK*10		!Yes. Prepare to note the next.
          CALL CPU_TIME(T1)		!Where are we at?
          E = (B.LAST - 1)*BIGORDER	!Convert from 10**BIGORDER to base 10.
          D = B.DIGIT(B.LAST)		!Grab the high-order digit.
          DO WHILE(D.GT.0)		!It is not zero..
            E = E + 1			!So it is at least one base ten digit.
            D = D/10			!Snip.
          END DO			!And perhaps there will be more.
Contemplate the rounding of the floating-point analogue.
          I4 = MIN(TASTE,B.LAST)	!I'm looking to taste the top digits.
          LEAD(1:I4) = B.DIGIT(B.LAST:B.LAST - I4 + 1:-1)	!Reverse, to have normal order.
          IF (B.LAST.GT.TASTE) THEN	!Are there even more digits?
            IT = I4			!Yes. This is now the low-order digit tasted.
            D = 0			!We should consider rounding up.
            IF (B.DIGIT(B.LAST - I4).GE.BIGBASE/2) D = 1	!If the next digit is big enough.
            DO WHILE (D.GT.0)		!Spread the carry.
              D = 0				!This one is used up.
              LEAD(IT) = LEAD(IT) + 1		!Thusly.
              IF (LEAD(IT).GT.BIGBASE) THEN	!But, maybe, overflow!
                IF (IT.GT.1) THEN		!Is there a higher-order to carry to?
                  LEAD(IT) = LEAD(IT) - BIGBASE		!Yes!
                  IT = IT - 1				!Step back to it,
                  D = 1					!Reassert a carry.
                END IF				!But only if there was a recipient available.
              END IF			!If not, the carry will still be zero.
            END DO			!And the loop won't continue.
          END IF			!So, no test for IT > 0 in a compound "while".
Cast forth the results.
          WRITE (6,102) P,PRIME(P),	!Name the step and its prime.
     1     P,LEAD(1:I4),E,		!The step and the leading few DIGIT of its primorial.
     2     T1 - T0			!CPU advance.
  102     FORMAT ("Prime(",I0,") = ",I0,", Primorial(",I0,") ~ 0."	!Approximately.
     1     I0,<I4 - 1>I<BIGORDER>.<BIGORDER>,"E+",I0,	!No lead zero digits, then with lead zero digits.
     2     T80,F12.3," seconds.")	!Append some CPU time information.
          T0 = T1			!Ready for the next popup.
        END IF				!So much for a report.
      END DO		!On to the next prime.

Chew some logarithms.
  110 WRITE (6,111)	!Some explanation.
  111 FORMAT (/,"Via summing logarithms: Single             Double")	!Ah, layout.
      MARK = 10		!Start somewhere interesting.
  112 F4 = SUM(LOG10( FLOAT(PRIME(1:MARK))))	!Whee!
      F8 = SUM(LOG10(DFLOAT(PRIME(1:MARK))))	!Generic function names too.
      I4 = F4		!Grab the integer part.
      I8 = F8		!The idea being to isolate the fractional part.
      WRITE (6,113) MARK,"10",10**(F4 - I4),I4 + 1,10**(F8 - I8),I8 + 1	!Reconstitute the number in extended E-format.
  113 FORMAT (I8,"#...in base ",A2,-1PF9.5,"E+",I0,T40,-1PF13.7,"E+",I0)	!As if via E-format.
      F4 = SUM(LOG( FLOAT(PRIME(1:MARK))))/LOG(10.0)	!Do it again in Naperian logs.
      F8 = SUM(LOG(DFLOAT(PRIME(1:MARK))))/LOG(10D0)	!Perhaps more accurately?
      I4 = F4
      I8 = F8
      WRITE (6,113) MARK,"e ",10**(F4 - I4),I4 + 1,10**(F8 - I8),I8 + 1	!We'll see.
      MARK = MARK*10	!The next reporting point.
      IF (MARK.LE.LASTP) GO TO 112	!Are we there yet?
      END	!So much for that.

```



### Results

For the smaller numbers, the number of digits in the results is apparent. For the larger values, it is given by the value of the exponent part.

```txt

Calculates primorial numbers up to prime 1000000, working in base 100
Prime(0) = 1, Primorial(0) = 1
Prime(1) = 2, Primorial(1) = 2
Prime(2) = 3, Primorial(2) = 6
Prime(3) = 5, Primorial(3) = 30
Prime(4) = 7, Primorial(4) = 210
Prime(5) = 11, Primorial(5) = 2310
Prime(6) = 13, Primorial(6) = 30030
Prime(7) = 17, Primorial(7) = 510510
Prime(8) = 19, Primorial(8) = 9699690
Prime(9) = 23, Primorial(9) = 223092870
Prime(10) = 29, Primorial(10) ~ 0.64696932E+10                                        0.000 seconds.
Prime(100) = 541, Primorial(100) ~ 0.47119308E+220                                    0.000 seconds.
Prime(1000) = 7919, Primorial(1000) ~ 0.6786296E+3393                                 0.000 seconds.
Prime(10000) = 104729, Primorial(10000) ~ 0.9063360E+45337                            0.875 seconds.
Prime(100000) = 1299709, Primorial(100000) ~ 0.1909604E+563921                      121.547 seconds.
Prime(1000000) = 15485863, Primorial(1000000) ~ 0.1147175E+6722809                17756.797 seconds.

Via summing logarithms: Single             Double
      10#...in base 10  0.64697E+10        0.6469693E+10
      10#...in base e   0.64697E+10        0.6469693E+10
     100#...in base 10  0.47123E+220       0.4711931E+220
     100#...in base e   0.47120E+220       0.4711931E+220
    1000#...in base 10  0.67887E+3393      0.6786296E+3393
    1000#...in base e   0.67849E+3393      0.6786296E+3393
   10000#...in base 10  0.10650E+45338     0.9063360E+45337
   10000#...in base e   0.82047E+45337     0.9063360E+45337
  100000#...in base 10  0.15399E+563940    0.1909604E+563921
  100000#...in base e   0.48697E+563884    0.1909604E+563921
 1000000#...in base 10  0.31623E+669099    0.1147174E+6722809
 1000000#...in base e   0.10000E+669683    0.1147175E+6722809
```

This is a rather severe demonstration of a faster-than-exponential growth rate in CPU time - the problem size increases exponentially as 10, 100, 1000, 10000, etc. (so that another 90, 900, 9000, etc. multiplies must be done) but the CPU time increases by more than a factor of ten. This is on an AMD FX6300 "six" core cpu at 3·76GHz that is steadily running the Great Internet Mersenne Prime computation on all six. Despite being at "idle" priority, it is presumably their contention for memory access that roughly triples the running time of any other individual computation.

Progress messages would have been helpful, but prior experience prompts caution: an attempt at having a collection of subprograms each accumulate their own CPU time usage for later assessment caused a worse than tenfold increase in CPU time for test runs and bizarre values as well. Another attempt at a "wait until a given time of day" that in the absence of access to any system routine for that became a loop on a call to the time-of-day routine promptly crashed the system - presumably due to too many (software-instituted) interrupts per second. Windows...

By contrast, there is only a slight pause for the even the largest summation of logarithms. Single precision demonstrates again its limited precision and 1000# suggests that base e logarithms are computed slightly more accurately. The double-precision results are the same as the first few decimal digits of the exact calculation, once some effort had been expended to have the values shown rounded correctly.


## FreeBASIC


```freebasic
' version 22-09-2015
' compile with: fbc -s console

Const As UInteger Base_ = 1000000000
ReDim Shared As UInteger primes()

Sub sieve(need As UInteger)

    ' estimate is to high, but ensures that we have enough primes
    Dim As UInteger max = need * (Log(need) + Log(Log(need)))
    Dim As UInteger t = 1 ,x , x2
    Dim As Byte p(max)

    ReDim primes (need + need \ 3) ' we trim the array later
    primes(0) = 1                  ' by definition
    primes(1) = 2                  ' first prime, the only even prime

    ' only consider the odd number
    For x = 3 To Sqr(max) Step 2
        If p(x) = 0 Then
            For x2 = x * x To max Step x * 2
                p(x2) = 1
            Next
        End If
    Next

    ' move found primes to array
    For x = 3 To max Step 2
        If p(x) = 0 Then
            t += 1
            primes(t) = x
        EndIf
    Next
    'ReDim Preserve primes(t)
    ReDim Preserve primes(need)

End Sub

' ------=< MAIN >=------

Dim As UInteger n, i, pow, primorial
Dim As String str_out, buffer = Space(10)

Dim As UInteger max = 100000 ' maximum number of primes we need

sieve(max)

primorial = 1
Print

For n = 0 To 9
    primorial = primorial * primes(n)
    Print Using " primorial(#) ="; n;
    RSet buffer, Str(primorial)
    str_out = buffer
    Print str_out
Next

' could use GMP, but why not make are own big integer routine
Dim As UInteger bigint(max), first = max, last = max
Dim As UInteger l, p, carry, low = 9, high = 10
Dim As ULongInt result
Dim As UInteger Ptr big_i

' start at the back, number grows to the left like normal number
bigint(last) = primorial
Print

For pow = 0 To Len(Str(max)) -2
    If pow > 0 Then
        low = high
        high = high * 10
    End If
    For n = low + 1 To high
        carry = 0
        big_i = @bigint(last)
        For i = last To first Step -1
            result = CULngInt(primes(n)) * *big_i + carry
            carry = result \ Base_
            *big_i = result - carry * Base_
            big_i = big_i -1
        Next i
        If carry <> 0 Then
            first = first -1
            *big_i = carry
        End If
    Next n
    l = Len(Str(bigint(first))) + (last - first) * 9
    Print " primorial("; high; ") has "; l ;" digits"
Next pow


' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
 primorial(0) =         1
 primorial(1) =         2
 primorial(2) =         6
 primorial(3) =        30
 primorial(4) =       210
 primorial(5) =      2310
 primorial(6) =     30030
 primorial(7) =    510510
 primorial(8) =   9699690
 primorial(9) = 223092870

 primorial(10) has 10 digits
 primorial(100) has 220 digits
 primorial(1000) has 3393 digits
 primorial(10000) has 45337 digits
 primorial(100000) has 563921 digits
```



## Go

Since this task isn't specifically about generating primes,
a fast external package is used to generate the primes.
The Go standard <tt>math/big</tt> package is used to multiply these as exact integers.

```go
package main

import (
	"fmt"
	"math/big"
	"time"

	"github.com/jbarham/primegen.go"
)

func main() {
	start := time.Now()
	pg := primegen.New()
	var i uint64
	p := big.NewInt(1)
	tmp := new(big.Int)
	for i <= 9 {
		fmt.Printf("primorial(%v) = %v\n", i, p)
		i++
		p = p.Mul(p, tmp.SetUint64(pg.Next()))
	}
	for _, j := range []uint64{1e1, 1e2, 1e3, 1e4, 1e5, 1e6} {
		for i < j {
			i++
			p = p.Mul(p, tmp.SetUint64(pg.Next()))
		}
		fmt.Printf("primorial(%v) has %v digits", i, len(p.String()))
		fmt.Printf("\t(after %v)\n", time.Since(start))
	}
}
```

```txt

primorial(0) = 1
primorial(1) = 2
primorial(2) = 6
primorial(3) = 30
primorial(4) = 210
primorial(5) = 2310
primorial(6) = 30030
primorial(7) = 510510
primorial(8) = 9699690
primorial(9) = 223092870
primorial(10) has 10 digits	(after 367.273µs)
primorial(100) has 220 digits	(after 8.460177ms)
primorial(1000) has 3393 digits	(after 8.760826ms)
primorial(10000) has 45337 digits	(after 26.838309ms)
primorial(100000) has 563921 digits	(after 2.049290216s)
primorial(1000000) has 6722809 digits	(after 4m19.931513819s)

```

Since this includes timing information this is also relevant:

```txt

% go version ; sysctl -n hw.model
go version go1.4.2 freebsd/amd64
Intel(R) Xeon(R) CPU E3-1245 v3 @ 3.40GHz

```



## Haskell



```Haskell

import Control.Arrow ((&&&))
import Data.List (scanl1, foldl1')

getNthPrimorial :: Int -> Integer
getNthPrimorial n = foldl1' (*) (take n primes)

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime = isPrime_ primes
  where isPrime_ :: [Integer] -> Integer -> Bool
        isPrime_ (p:ps) n
          | p * p > n      = True
          | n `mod` p == 0 = False
          | otherwise      = isPrime_ ps n

primorials :: [Integer]
primorials = 1 : scanl1 (*) primes

main :: IO ()
main = do
  -- Print the first 10 primorial numbers
  let firstTen = take 10 primorials
  putStrLn $ "The first 10 primorial numbers are: " ++ show firstTen

  -- Show the length of the primorials with index 10^[1..6]
  let powersOfTen = [1..6]
      primorialTens = map (id &&& (length . show . getNthPrimorial . (10^))) powersOfTen
      calculate = mapM_ (\(a,b) -> putStrLn $ "Primorial(10^"++show a++") has "++show b++" digits")
  calculate primorialTens


```


```txt

The first 10 primorial numbers are: [1,2,6,30,210,2310,30030,510510,9699690,223092870]
Primorial(10^1) has 10 digits
Primorial(10^2) has 220 digits
Primorial(10^3) has 3393 digits
Primorial(10^4) has 45337 digits
Primorial(10^5) has 563921 digits
Primorial(10^6) has 6722809 digits

```



## J


Implementation:


```J
primorial=:*/@:p:@i."0
```


Task examples:


```J
   primorial i. 10    NB. first 10 primorial numbers
1 2 6 30 210 2310 30030 510510 9699690 223092870
   #":primorial 10x  NB. lengths (of decimal representations)...
10
   #":primorial 100x
220
   #":primorial 1000x
3393
   #":primorial 10000x
45337
   #":primorial 100000x
563921
   #":primorial 1000000x
6722809
```


Note that the x suffix on a decimal number indicates that calculations should be exact (what the lisp community has dubbed "bignums"). This is a bit slow (internally, every number winds up being represented as a list of what might be thought of as "digits" in a very large base), but it gets the job done.


## Java


```java
import java.math.BigInteger;

public class PrimorialNumbers {
    final static int sieveLimit = 1300_000;
    static boolean[] notPrime = sieve(sieveLimit);

    public static void main(String[] args) {
        for (int i = 0; i < 10; i++)
            System.out.printf("primorial(%d): %d%n", i, primorial(i));

        for (int i = 1; i < 6; i++) {
            int len = primorial((int) Math.pow(10, i)).toString().length();
            System.out.printf("primorial(10^%d) has length %d%n", i, len);
        }
    }

    static BigInteger primorial(int n) {
        if (n == 0)
            return BigInteger.ONE;

        BigInteger result = BigInteger.ONE;
        for (int i = 0; i < sieveLimit && n > 0; i++) {
            if (notPrime[i])
                continue;
            result = result.multiply(BigInteger.valueOf(i));
            n--;
        }
        return result;
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
primorial(0): 1
primorial(1): 2
primorial(2): 6
primorial(3): 30
primorial(4): 210
primorial(5): 2310
primorial(6): 30030
primorial(7): 510510
primorial(8): 9699690
primorial(9): 223092870
primorial(10^1) has length 10
primorial(10^2) has length 220
primorial(10^3) has length 3393
primorial(10^4) has length 45337
primorial(10^5) has length 563921
```



## Julia

```julia

using Primes

primelist = primes(300000001) # primes to 30 million

primorial(n) = foldr(*, primelist[1:n], init=BigInt(1))

println("The first ten primorials are: $([primorial(n) for n in 1:10])")

for i in 1:6
    n = 10^i
    p = primorial(n)
    plen = Int(floor(log10(p))) + 1
    println("primorial($n) has length $plen digits in base 10.")
end

```

```txt
The first ten primorials are: BigInt[2, 6, 30, 210, 2310, 30030, 510510, 9699690, 223092870, 6469693230]
primorial(10) has length 10 digits in base 10.
primorial(100) has length 220 digits in base 10.
primorial(1000) has length 3393 digits in base 10.
primorial(10000) has length 45337 digits in base 10.
primorial(100000) has length 563921 digits in base 10.
primorial(1000000) has length 6722809 digits in base 10.

```



## Kotlin


```scala
// version 1.0.6

import java.math.BigInteger

const val LIMIT = 1000000  // expect a run time of about 20 minutes on a typical laptop

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

fun countDigits(bi: BigInteger): Int = bi.toString().length
      
fun main(args: Array<String>) {
    println("Primorial(0) = 1")
    println("Primorial(1) = 2")
    var count = 1
    var p = 3
    var prod = BigInteger.valueOf(2)
    var target = 10
    while(true) {
        if (isPrime(p)) {
            count++
            prod *= BigInteger.valueOf(p.toLong())
            if (count < 10) { 
                println("Primorial($count) = $prod")
                if (count == 9) println()
            }
            else if (count == target) { 
                println("Primorial($target) has ${countDigits(prod)} digits")              
                if (count == LIMIT) break
                target *= 10
            }
        }
        p += 2           
    }   
}
```


```txt

Primorial(0) = 1
Primorial(1) = 2
Primorial(2) = 6
Primorial(3) = 30
Primorial(4) = 210
Primorial(5) = 2310
Primorial(6) = 30030
Primorial(7) = 510510
Primorial(8) = 9699690
Primorial(9) = 223092870

Primorial(10) has 10 digits
Primorial(100) has 220 digits
Primorial(1000) has 3393 digits
Primorial(10000) has 45337 digits
Primorial(100000) has 563921 digits
Primorial(1000000) has 6722809 digits

```



## Lingo

For generating the list of primes an auto-extending Sieve of Eratosthenes lib is used (fast), and for the larger primorials a simple custom big-integer lib (very slow).

```Lingo
-- libs
sieve = script("math.primes").new()
bigint = script("bigint").new()

cnt = 1000 * 100
primes = sieve.getNPrimes(cnt)

pr = 1
put "Primorial 0: " & pr
repeat with i = 1 to 9
    pr = pr*primes[i]
    put "Primorial " & i & ": " & pr
end repeat

pow10 = 10
repeat with i = 10 to cnt
    pr = bigint.mul(pr, primes[i])
    if i mod pow10=0 then
        put "Primorial " & i & " has length: " & pr.length
        pow10 = pow10 * 10
    end if
end repeat
```

```txt

-- "Primorial 0: 1"
-- "Primorial 1: 2"
-- "Primorial 2: 6"
-- "Primorial 3: 30"
-- "Primorial 4: 210"
-- "Primorial 5: 2310"
-- "Primorial 6: 30030"
-- "Primorial 7: 510510"
-- "Primorial 8: 9699690"
-- "Primorial 9: 223092870"
-- "Primorial 10 has length: 10"
-- "Primorial 100 has length: 220"
-- "Primorial 1000 has length: 3393"
-- "Primorial 10000 has length: 45337"
-- "Primorial 100000 has length: 563921"

```



## Mathematica

The first 10 primorial numbers are:

```Mathematica
FoldList[Times, 1, NestList[NextPrime, 2, 8]]
```

```txt

{1,2,6,30,210,2310,30030,510510,9699690,223092870}
```


The lengths of selected primorial numbers are:

```Mathematica
primes = NestList[NextPrime, 2, 999999];
Grid@Table[{"primorial(10^" <> ToString[n] <> ") has ", 
   Length@IntegerDigits[Times @@ (primes[[;; 10^n]])], " digits"}, {n,
    6}]
```

```txt

primorial(10^1) has 10 digits
primorial(10^2) has 220	digits
primorial(10^3) has 3393 digits
primorial(10^4) has 45337 digits
primorial(10^5) has 563921 digits
primorial(10^6) has 6722809 digits

```



## Nickle


```c
library "prime_sieve.5c"

# For 1 million primes
# int val = 15485867;
int val = 1299743;

int start = millis();
int [*] primes = PrimeSieve::primes(val);
printf("%d primes (%d) in %dms\n", dim(primes), primes[dim(primes)-1], millis() - start);

int primorial(int n) {
   if (n == 0) return 1;
   if (n == 1) return 2;
   int v = 2;
   for (int i = 2; i <= n; i++) {
       v *= primes[i-2];
   }
   return v;
}

for (int i = 0; i < 10; i++) {
   printf("primorial(%d) = %d\n", i, primorial(i));
}

for (int i = 1; i < 6; i++) {
   start = millis();
   int p = 10**i;
   int pn = primorial(p);

   int digits = floor(Math::log10(pn)) + 1;
   printf("primorial(%d) has %d digits, in %dms\n", p, digits, millis() - start);
}
```


```txt
prompt$ nickle primorial.5c
100001 primes (1299743) in 1838ms
primorial(0) = 1
primorial(1) = 2
primorial(2) = 6
primorial(3) = 30
primorial(4) = 210
primorial(5) = 2310
primorial(6) = 30030
primorial(7) = 510510
primorial(8) = 9699690
primorial(9) = 223092870
primorial(10) has 10 digits, in 4ms
primorial(100) has 220 digits, in 2ms
primorial(1000) has 3393 digits, in 3ms
primorial(10000) has 45337 digits, in 128ms
primorial(100000) has 563921 digits, in 12304ms
```



## PARI/GP


```parigp
nthprimorial(n)=prod(i=1,n,prime(i));
vector(10,i,nthprimorial(i-1))
vector(5,n,#Str(nthprimorial(10^n)))
#Str(nthprimorial(10^6))
```

```txt
%1 = [1, 2, 6, 30, 210, 2310, 30030, 510510, 9699690, 223092870]
%2 = [10, 220, 3393, 45337, 563921]
%3 = 433637
```



### With vecprod

Pari/GP 2.11 added the <code>vecprod</code> command, which makes a significantly faster version possible.  We use <code>primes(n)</code> to get a vector of the first n primes, which <code>vecprod</code> multiplies using a product tree.

```parigp
nthprimorial(n)=vecprod(primes(n));
vector(6,n,#Str(nthprimorial(10^n)))
```

```txt
[10, 220, 3393, 45337, 563921, 6722809]
? ##
  ***   last result computed in 1,663 ms.
```



## Pascal

Using unit like GMP.  MPArith of wolfgang-ehrhardt.de
mp_primorial {-Primorial of n;  a = n# = product of primes <= n.}
so i sieve to get the right n 

```pascal
{$H+}
uses
  sysutils,mp_types,mp_base,mp_prime,mp_numth;
var
 x: mp_int;
 t0,t1: TDateTime;
 s: AnsiString;

var
  i,cnt : NativeInt;
  ctx :TPrimeContext;
begin
  mp_init(x);
  cnt := 1;
  i := 2;
  FindFirstPrime32(i,ctx);
  i := 10;
  t0 := time;
  repeat
    repeat
      FindNextPrime32(ctx);
      inc(cnt);
    until cnt = i;
    mp_primorial(ctx.prime,x);
    s:= mp_adecimal(x);
    writeln('MaxPrime ',ctx.prime:10,length(s):8,' digits');
    i := 10*i;
  until i > 1000*1000;
  t1 := time;
  Writeln((t1-t0)*86400.0:10:3,' s');
end.

```

```txt
MaxPrime         29      10 digits
MaxPrime        541     220 digits
MaxPrime       7919    3393 digits
MaxPrime     104729   45337 digits
MaxPrime    1299709  563921 digits
MaxPrime   15485863 6722809 digits
    111.290 s
using i4330 3.5 Ghz Win7 32-bit fpc 2.6.4 a little outdated
uups:  174.468 s Linux  32-bit fpc 3.0.1 
real  2m54.470s  user  1m59.133s sys 0m55.188s! 

```



### alternative

getPrimorialExact uses multiplication to Base 1e9.Heavily inspired by 
[[Primorial_numbers#FreeBASIC|FreeBASIC]]
It is extremly slow.PrimorialExact (1000000) should take about 2400 seconds on my computer.See GO which takes 2 secs instead of my 38 secs ( 3.4 Ghz 64Bit vs 3.5 Ghz 32 Bit ).
Tested x64 -> runtime 16m48 x64 div is substituted by mul and shift.Maybe first Mul then base convertion will be faster.
Obviously GMP will do it by far better.


```pascal
program Primorial;
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
uses
  sysutils;
var
  primes : array[0..1000000] of LongInt;

procedure InitSieve;
const
  HiSieve = 15485864;
var
  sieve: array of boolean;
  i, j: NativeInt;
Begin
  setlength(sieve,HiSieve);
  fillchar(sieve[0],HiSieve,chr(ord(True)));
  For i := 2 to Trunc(sqrt(HiSieve)) do
    IF sieve[i] then Begin
      j := i*i;repeat sieve[j]:= false;inc(j,i);until j>= HiSieve-1;end;
  i:= 2;j:= 1;
  repeat
    IF sieve[i] then begin primes[j]:= i;inc(j) end;
    inc(i);
  until i > HiSieve;
  primes[0] := 1;setlength(sieve,0);
end;

function getPrimorial(n:NativeInt):Uint64;
Begin
  result := ORD(n>=0);
  IF (n >= 0) AND (n < 16) then
    repeat result := result*primes[n]; dec(n); until n < 1;
end;

function getPrimorialDecDigits(n:NativeInt):NativeInt;
var
  res: extended;
Begin
  result := -1;
  IF (n > 0) AND (n <= 1000*1000) then
  Begin
    res := 0;
    repeat res := res+ln(primes[n]); dec(n); until n < 1;
    result := trunc(res/ln(10))+1;
  end;
end;

function getPrimorialExact(n:NativeInt):NativeInt;
const
  LongWordDec = 1000000000;
var
  MulArr : array of LongWord;
  pMul : ^LongWord;
  Mul1,prod,carry : Uint64;
  i,j,ul : NativeInt;
begin
  i := getPrimorialDecDigits(n) DIV 9 +10;
  Setlength(MulArr,i);
  Ul := 0;
  MulArr[Ul]:= 1;
  i := 1;
  repeat
    Mul1 := 1;
    //Make Mul1 as large as possible
    while (i<= n) AND ((LongWordDec DIV MUL1) >= primes[i])  do
      Begin Mul1 := Mul1*primes[i]; inc(i); end;
    carry := 0;
    pMul := @MulArr[0];
    For j := 0 to UL do
    Begin
      prod  := Mul1*pMul^+Carry;
      Carry := prod Div LongWordDec;
      pMul^ := Prod - Carry*LongWordDec;
      inc(pMul);
    end;
    IF Carry <> 0 then Begin inc(Ul);pMul^:= Carry; End;
  until i> n;
  //count digits
  i := Ul*9;
  Carry := MulArr[Ul];
  repeat
    Carry := Carry DIV 10;
    inc(i);
  until Carry = 0;
  result := i;
end;


var
  i: NativeInt;
Begin
  InitSieve;
  write('Primorial (0->9) ');
  For i := 0 to 9 do
    write(getPrimorial(i),',');
  writeln(#8#32#13#10);
  i:= 10;
  repeat
    writeln('Primorial (',i,') = digits ',
            getPrimorialDecDigits(i),' digits');
    i := i*10;
  until i> 1000000;
  writeln;
  i:= 10;
  repeat
    writeln('PrimorialExact (',i,') = digits ',
            getPrimorialExact(i),' digits');
    i := i*10;
  until i> 100000;
end.
```

```txt
Primorial (0->9) 1,2,6,30,210,2310,30030,510510,9699690,223092870

Primorial (10) = digits 10 digits
Primorial (100) = digits 220 digits
Primorial (1000) = digits 3393 digits
Primorial (10000) = digits 45337 digits
Primorial (100000) = digits 563921 digits
Primorial (1000000) = digits 6722809 digits
//real  0m0.143s without PrimorialExact

PrimorialExact (10) = digits 10 digits
PrimorialExact (100) = digits 220 digits
PrimorialExact (1000) = digits 3393 digits
PrimorialExact (10000) = digits 45337 digits
PrimorialExact (100000) = digits 563921 digits

real  0m38.311s
for x64 I tried it
PrimorialExact (1000000) = digits 6722809 digits
real    16m48.240s
```



## Perl


```perl
use ntheory qw(pn_primorial);

say "First ten primorials: ", join ", ", map { pn_primorial($_) } 0..9;

say "primorial(10^$_) has ".(length pn_primorial(10**$_))." digits" for 1..6;
```


The <code>pn_primorial</code> function is smart enough to return a <code>Math::BigInt</code> object if the result won't fit into a native integer, so it all works out.

```txt

First ten primorials: 1, 2, 6, 30, 210, 2310, 30030, 510510, 9699690, 223092870
primorial(10^1) has 10 digits
primorial(10^2) has 220 digits
primorial(10^3) has 3393 digits
primorial(10^4) has 45337 digits
primorial(10^5) has 563921 digits
primorial(10^6) has 6722809 digits

```


Still using the library for the core activities, we can do the same in two steps.  <code>primes($n)</code> returns a reference to a list of primes up to n, <code>nth_prime($n)</code> returns the nth prime, and <code>vecprod</code> does an efficient product tree.  So for 10^6 we can do:

```perl
use ntheory ":all";
say length( vecprod( @{primes( nth_prime(10**6) )} ) );
```


returning the same result in only slightly more time.  Both examples take under 4 seconds.


## Perl 6

With the module <code>Math::Primesieve</code>, this runs in about 1/3 the time vs the native prime generator.
```perl6
use Math::Primesieve;

my $sieve = Math::Primesieve.new;
my @primes = $sieve.primes(10_000_000);

sub primorial($n) { [*] @primes[^$n] }

say "First ten primorials: {(primorial $_ for ^10)}";
say "primorial(10^$_) has {primorial(10**$_).chars} digits" for 1..5;
```

```txt

First ten primorials: 1 2 6 30 210 2310 30030 510510 9699690 223092870
primorial(10^1) has 10 digits
primorial(10^2) has 220 digits
primorial(10^3) has 3393 digits
primorial(10^4) has 45337 digits
primorial(10^5) has 563921 digits
```



## Phix

Uses primes and add_block from [[Extensible_prime_generator#Phix]]

### slow bigatom or fast cheat

While straightforward, and exact, the bigatom version is extremely slow.

Cheating slightly, using base 10 logs with limited precision, is nice and fast
with exactly the same results as those from other languages on this page.


```Phix
atom t0 = time()
constant bool cheat = false
constant integer lim = iff(cheat?1000000    -- log10
                                :10000)     -- bigatom
while length(primes)<lim do add_block() end while

include bigatom.e

sequence tests = tagset(10,0)
for i=2 to iff(cheat?6:4) do
    tests &= power(10,i)
end for

object p = iff(cheat?0:BA_ONE) -- atom|bigatom
integer pi = 1
for i=1 to length(tests) do
    integer ti = tests[i]
    for pi=pi to ti do
        if cheat then
            p += log10(primes[pi])
        else
            p = ba_mul(p,primes[pi])
        end if
    end for
    if ti<=10 then
        string ps = iff(cheat?sprintf("%d",power(10,p)):ba_sprint(p))
        printf(1,"Primorial(%d) = %s\n",{ti,ps})
    else
        integer pd = iff(cheat?floor(p)+1:length(ba_sprint(p)))
        printf(1,"Primorial(%d) has %d digits (%s)\n",{ti,pd,elapsed(time()-t0)})
    end if
end for
?elapsed(time()-t0)
```

With cheat set to true:

```txt

Primorial(0) = 1
Primorial(1) = 2
Primorial(2) = 6
Primorial(3) = 30
Primorial(4) = 210
Primorial(5) = 2310
Primorial(6) = 30030
Primorial(7) = 510510
Primorial(8) = 9699690
Primorial(9) = 223092870
Primorial(10) = 6469693230
Primorial(100) has 220 digits (0.4s)
Primorial(1000) has 3393 digits (0.4s)
Primorial(10000) has 45337 digits (0.4s)
Primorial(100000) has 563921 digits (0.4s)
Primorial(1000000) has 6722809 digits (0.6s)
"0.6s"

```

Generating the primes takes about 2/3 of the total time.

With cheat set to false, as above except:

```txt

Primorial(100) has 220 digits (0.0s)
Primorial(1000) has 3393 digits (1.1s)
Primorial(10000) has 45337 digits (3 minutes and 02s)
"3 minutes and 02s"

```

Above that it is extremely slow - 10^5 on a slightly earlier version took over 2 hours, and
I estimate that 10^6 would probably take more than two weeks!


### bigint version

Already knowing that bigatom.e is not particularly fast, I tried copying the FreeBASIC approach.

```Phix
atom t0 = time()

constant lim = 10000-0
while length(primes)<lim do add_block() end while

integer primorial = 1
 
for n=0 to 9 do
    if n!=0 then
        primorial *= primes[n]
    end if
    printf(1," primorial(%d) = %d\n",{n,primorial})
end for

-- Roll our own big integer routine:
-- For best performance, base should be the largest power 
-- of ten such that base*primes[lim] is still an integer.
constant integer base = iff(machine_bits()=32?1000:1000000000)
constant integer digits_per = length(sprint(base-1))
-- Also check that the limits of precision will not be exceeded.
if base*primes[lim]>power(2,iff(machine_bits()=32?53:64)) then ?9/0 end if

-- start at the back, number grows to the right (like little endian)
sequence bigint = {primorial}
integer low = 9,
        high = 10
atom result

--Results of our homebrew method have been verified against bigatom:
--include bigatom.e
--bigatom check = ba_new(primorial)
--constant string dpfmt = sprintf("%%0%dd",digits_per)
 
atom tc = time()+1

printf(1,"\n")
for pow=0 to length(sprint(lim))-2 do
    if pow>0 then
        low = high
        high = high * 10
    end if
    for n=low+1 to high do
        integer carry = 0,
                pn = primes[n]
        for i=1 to length(bigint) do
            result = pn*bigint[i]+carry
            carry = floor(result/base)
            bigint[i] = result-carry*base
        end for
        while carry <> 0 do
            if time()>tc then
                printf(1,"%d\r",length(bigint)*digits_per)
                tc = time()+1
            end if
            result = carry
            carry = floor(carry/base)
            bigint &= result-carry*base
        end while
--check = ba_mul(check,pn)
--string bis = sprint(bigint[$])
--for i=length(bigint)-1 to 1 by -1 do
--  bis &= sprintf(dpfmt,bigint[i])
--end for
--string bc = ba_sprint(check)
--if bis!=bc then ?9/0 end if
    end for
    integer l = length(sprint(bigint[$])) + (length(bigint)-1)*digits_per
    printf(1," primorial(%d) has %d digits (%s)\n",{high,l,elapsed(time()-t0)})
end for
```

on 32 bit:

```txt

 primorial(0) = 1
 primorial(1) = 2
 primorial(2) = 6
 primorial(3) = 30
 primorial(4) = 210
 primorial(5) = 2310
 primorial(6) = 30030
 primorial(7) = 510510
 primorial(8) = 9699690
 primorial(9) = 223092870

 primorial(10) has 10 digits (0.4s)
 primorial(100) has 220 digits (0.4s)
 primorial(1000) has 3393 digits (0.4s)
 primorial(10000) has 45337 digits (4.0s)
 primorial(100000) has 563921 digits (7 minutes and 57s)

```

on 64 bit, with a higher lim, (I gave up on 10^6 on 32 bit) same except:

```txt

 primorial(10) has 10 digits (0.1s)
 primorial(100) has 220 digits (0.1s)
 primorial(1000) has 3393 digits (0.1s)
 primorial(10000) has 45337 digits (1.5s)
 primorial(100000) has 563921 digits (2 minutes and 52s)
 primorial(1000000) has 6722809 digits (5 hours 29 minutes and 4s)

```

Still not stunning, but maybe 50 to 100 times faster than bigatom (which is same speed on 32/64 bit).

### gmp

This will get 1,000,000 in about 7 mins, if you squidge an extra 0 into the get_primes argument.

```Phix
include primes.e
include mpfr.e

atom t0 = time()
constant primes = get_primes(-100001)
mpz primorial = mpz_init(1)
integer tens = 10
for i=1 to length(primes) do
    if i<=10 then
        printf(1,"Primorial(%d) = %s\n", {i-1, mpz_get_str(primorial)})
    elsif i-1=tens then
        printf(1,"Primorial(%d) has %d digits (%s)\n", 
                 {i-1, mpz_sizeinbase(primorial,10),elapsed(time()-t0)})
        tens *= 10
    end if
    mpz_mul_si(primorial, primorial, primes[i])
end for
primorial = mpz_free(primorial)
```

```txt

Primorial(0) = 1
Primorial(1) = 2
Primorial(2) = 6
Primorial(3) = 30
Primorial(4) = 210
Primorial(5) = 2310
Primorial(6) = 30030
Primorial(7) = 510510
Primorial(8) = 9699690
Primorial(9) = 223092870
Primorial(10) has 10 digits (0.2s)
Primorial(100) has 220 digits (0.2s)
Primorial(1000) has 3393 digits (0.2s)
Primorial(10000) has 45338 digits (0.3s)
Primorial(100000) has 563921 digits (4.2s)

```



## PicoLisp

This code uses '''prime?''' and '''take''' functions from [http://www.rosettacode.org/wiki/Extensible_prime_generator#PicoLisp Extensible Prime Generator(PicoLisp)]

```PicoLisp

(de prime? (N Lst)
   (let S (sqrt N)
      (for D Lst
         (T (> D S) T)
         (T (=0 (% N D)) NIL) ) ) )

(de take (N)
   (let I 1
      (make
         (link 2)
         (do (dec N)
            (until (prime? (inc 'I 2) (made)))
            (link I) ) ) ) )

# This is a simple approach to calculate primorial may not be the fastest one 
(de primorial (N)
   (apply * (take N)) )

#print 1st 10 primorial numbers
(for M 10 (prinl "primorial: "(primorial M)))

# print the length of primorial numbers.
[prinl (length (primorial (** 10 1)]
[prinl (length (primorial (** 10 2)]
[prinl (length (primorial (** 10 3)]
[prinl (length (primorial (** 10 4)]
#The last one takes a very long time to compute.
[prinl (length (primorial (** 10 5)]
```


```txt

primorial: 2
primorial: 6
primorial: 30
primorial: 210
primorial: 2310
primorial: 30030
primorial: 510510
primorial: 9699690
primorial: 223092870
primorial: 6469693230

10
220
3393
45337
563921

```



## Python

Uses the pure python library [https://pypi.python.org/pypi/pyprimes/0.1.1a pyprimes ].


```python
from pyprimes import nprimes
from functools import reduce


primelist = list(nprimes(1000001))    # [2, 3, 5, ...]

def primorial(n):
    return reduce(int.__mul__, primelist[:n], 1)

if __name__ == '__main__':
    print('First ten primorals:', [primorial(n) for n in range(10)])
    for e in range(7):
        n = 10**e
        print('primorial(%i) has %i digits' % (n, len(str(primorial(n)))))
```


```txt
First ten primorials: [1, 2, 6, 30, 210, 2310, 30030, 510510, 9699690, 223092870]
primorial(1) has 1 digits
primorial(10) has 10 digits
primorial(100) has 220 digits
primorial(1000) has 3393 digits
primorial(10000) has 45337 digits
primorial(100000) has 563921 digits
primorial(1000000) has 6722809 digits
```



## Racket

We have to reimplement <code>nth-prime</code> and replace it with a memorized version, to make it faster. But we can't memorize <code>primorial</code> because it would use too much memory.

```Racket
#lang racket

(require (except-in math/number-theory nth-prime))

(define-syntax-rule (define/cache (name arg) body ...)
  (begin
    (define cache (make-hash))
    (define (name arg)
      (hash-ref! cache arg (lambda () body ...)))))

(define (num-length n)
  ;warning: this defines (num-length 0) as 0
  (if (zero? n)
    0
    (add1 (num-length (quotient n 10)))))

(define/cache (nth-prime n)
  (if (zero? n)
      2
      (for/first ([p (in-naturals (add1 (nth-prime (sub1 n))))]
                  #:when (prime? p))
           p)))

(define (primorial n)
  (if (zero? n)
     1
     (* (primorial (sub1 n))
        (nth-prime (sub1 n)))))

(displayln
 (for/list ([i (in-range 10)])
   (primorial i)))

(for ([i (in-range 1 6)])
  (printf "Primorial(10^~a) has ~a digits.\n"
          i
          (num-length (primorial (expt 10 i)))))
```

```txt
(1 2 6 30 210 2310 30030 510510 9699690 223092870)
Primorial(10^1) has 10 digits.
Primorial(10^2) has 220 digits.
Primorial(10^3) has 3393 digits.
Primorial(10^4) has 45337 digits.
Primorial(10^5) has 563921 digits.
```



## REXX


```rexx
/*REXX program computes some primorial numbers for low #s, and for  10^n.     */
parse arg N H .                        /*get optional arguments:  N,  L,  H   */
if N=='' | N==','  then N=10           /*was  N  given?  Then use the default.*/
if H=='' | H==','  then H=100000       /*was  H  given?  Then use the default.*/
numeric digits 600000; w=length(digits())  /* be able to handle large numbers.*/
@.=.; @.0=1; @.1=2; @.2=3; @.3=5;  @.4=7;  @.5=11;  @.6=13  /*some low primes.*/
             s.1=4; s.2=9; s.3=25; s.4=49; s.5=121; s.6=169 /*squared primes. */
#=6                                                         /*number of primes*/
     do j=0  for N                     /*calculate the first  N  primorial #s.*/
     say right(j,length(N))    ' primorial is: '    primorial(j)
     end   /*j*/
p=1;                    say
     do k=1  for H                     /*process a large range of numbers.    */
     p=p*prime(k)                      /*calculate the next primorial number. */
     parse var k L 2 '' -1 R           /*get the left and rightmost dec digits*/
     if R\==0           then iterate   /*if right─most decimal digit\==0, skip*/
     if L\==1           then iterate   /* "  left─most    "      "  \==1,   " */
     if strip(k,,0)\==1 then iterate   /*Not a power of 10?  Then skip this K.*/
     say right(k,w) ' primorial length in decimal digits is:' right(length(p),w)
     end   /*k*/
exit                                   /*stick a fork in it,  we're all done. */
/*──────────────────────────────────PRIMORIAL subroutine──────────────────────*/
primorial: procedure expose @. s. #; parse arg y;  !=1    /*obtain the arg  Y.*/
               do p=0  to y;  !=!*prime(p);  end   /*p*/  /*calculate product.*/
return !                                                  /*return with the #.*/
/*──────────────────────────────────PRIME subroutine──────────────────────────*/
prime: procedure expose @. s. #;  parse arg n;  if @.n\==.  then return @.n
numeric digits 9                                          /*limit digs to min.*/
  do j=@.#+2  by 2                                        /*start looking at #*/
  if j//2==0  then iterate; if j//3==0    then iterate    /*divisible by 2│3 ?*/
  parse var j '' -1 _;      if _==5       then iterate    /*right-most dig≡5? */
  if j//7==0  then iterate; if j//11==0   then iterate    /*divisible by 7│11?*/
    do k=6  while s.k<=j;   if j//@.k==0  then iterate j  /*divide by primes. */
    end   /*k*/
  #=#+1;  @.#=j;  s.#=j*j;  return j                      /*next prime; return*/
  end     /*j*/
```

'''output''' when using the default inputs:

```txt

 0  primorial is:  1
 1  primorial is:  2
 2  primorial is:  6
 3  primorial is:  30
 4  primorial is:  210
 5  primorial is:  2310
 6  primorial is:  30030
 7  primorial is:  510510
 8  primorial is:  9699690
 9  primorial is:  223092870

    10  primorial length in decimal digits is:     10
   100  primorial length in decimal digits is:    220
  1000  primorial length in decimal digits is:   3393
 10000  primorial length in decimal digits is:  45337
100000  primorial length in decimal digits is: 563921

```



## Ring


```ring

# Project: Primorial numbers
load "bignumber.ring"
decimals(0)
num = 0
prim = 0
limit = 10000000
see "working..." + nl
see "wait for done..." + nl
while num < 100001
    prim = prim + 1
    prime = [] 
    primorial(prim)
end
see "done..." + nl
 
func primorial(pr)     
     n = 1
     n2 = 0
     flag = 1
     while flag = 1 and n < limit  
           nr = isPrime(n)
           if n=1
              nr=1 
           ok
           if nr=1
              n2 = n2 + 1
              add(prime,n)
           ok
           if n2=pr 
              flag=0
              num = num + 1
           ok
           n = n + 1
     end
     pro = 1
     str = ""
     for n=1 to len(prime)
         pro = FuncMultiply("" + pro,"" + prime[n])
         str = str + prime[n] + "*"
     next
     str = left(str,len(str)-1)
     if pr < 11
        see "primorial(" + string(pr-1) + ") : " + pro + nl
     ok
     if pr = 11
        see "primorial(" + string(pr-1) + ") " + "has " + len(pro) + " digits"+ nl
     but pr = 101
        see "primorial(" + string(pr-1) + ") " + "has " + len(pro) + " digits"+ nl
     but pr = 1001
         see "primorial(" + string(pr-1) + ") " + "has " + len(pro) + " digits"+ nl
     but pr = 10001
         see "primorial(" + string(pr-1) + ") " + "has " + len(pro) + " digits"+ nl
     but pr = 100001
         see "primorial(" + string(pr-1) + ") " + "has " + len(pro) + " digits"+ nl
     ok

```


```txt

working...
wait for done...
primorial(0) = 1
primorial(1) = 2
primorial(2) = 6
primorial(3) = 30
primorial(4) = 210
primorial(5) = 2310
primorial(6) = 30030
primorial(7) = 510510
primorial(8) = 9699690
primorial(9) = 223092870
primorial(10) has 10 digits
primorial(100) has 220 digits
primorial(1000) has 3393 digits
primorial(10000) has 45337 digits
primorial(100000) has 563921 digits
done...

```



## Ruby



```ruby
require 'prime'
 
def primorial_number(n)
  pgen = Prime.each
  (1..n).inject(1){|p,_| p*pgen.next}
end
 
puts "First ten primorials: #{(0..9).map{|n| primorial_number(n)}}"
 
(1..5).each do |n|
  puts "primorial(10**#{n}) has #{primorial_number(10**n).to_s.size} digits"
end
```


```txt
First ten primorials: [1, 2, 6, 30, 210, 2310, 30030, 510510, 9699690, 223092870]
primorial(10**1) has 10 digits
primorial(10**2) has 220 digits
primorial(10**3) has 3393 digits
primorial(10**4) has 45337 digits
primorial(10**5) has 563921 digits
```



## Scala

This example uses Spire's SafeLong for arbitrarily large integers and parallel vectors to accelerate bulk operations on large collections. The primorial is calculated by simply building a list of primes and taking the product. The prime generator here is relatively inefficient, but as it isn't the focus of this task the priority is brevity while retaining acceptable performance.


```scala
import spire.math.SafeLong
import spire.implicits._

import scala.collection.parallel.immutable.ParVector

object Primorial {
  def main(args: Array[String]): Unit = {
    println(
      s"""|First 10 Primorials:
          |${LazyList.range(0, 10).map(n => f"$n: ${primorial(n).toBigInt}%,d").mkString("\n")}
          |
          |Lengths of Primorials:
          |${LazyList.range(1, 7).map(math.pow(10, _).toInt).map(i => f"$i%,d: ${primorial(i).toString.length}%,d").mkString("\n")}
          |""".stripMargin)
  }
  
  def primorial(num: Int): SafeLong = if(num == 0) 1 else primesSL.take(num).to(ParVector).reduce(_*_)
  lazy val primesSL: Vector[SafeLong] = 2 +: ParVector.range(3, 20000000, 2).filter(n => !Iterator.range(3, math.sqrt(n).toInt + 1, 2).exists(n%_ == 0)).toVector.sorted.map(SafeLong(_))
}
```


```txt
First 10 Primorials:
0: 1
1: 2
2: 6
3: 30
4: 210
5: 2,310
6: 30,030
7: 510,510
8: 9,699,690
9: 223,092,870

Lengths of Primorials:
10: 10
100: 220
1,000: 3,393
10,000: 45,337
100,000: 563,921
1,000,000: 6,722,809
```



## Sidef

```ruby
say (
    'First ten primorials: ',
    {|i| pn_primorial(i) }.map(^10).join(', ')
)

{ |i|
    say ("primorial(10^#{i}) has " + pn_primorial(10**i).len + ' digits')
} << 1..6
```

```txt

First ten primorials: 1, 2, 6, 30, 210, 2310, 30030, 510510, 9699690, 223092870
primorial(10^1) has 10 digits
primorial(10^2) has 220 digits
primorial(10^3) has 3393 digits
primorial(10^4) has 45337 digits
primorial(10^5) has 563921 digits
primorial(10^6) has 6722809 digits

```


## Rust


```Rust

extern crate primal;
extern crate rayon;
extern crate rug;

use rayon::prelude::*;
use rug::Integer;

fn partial(p1 : usize, p2 : usize) -> String {
    let mut aux = Integer::from(1);
    let (_, hi) = primal::estimate_nth_prime(p2 as u64);
    let sieve = primal::Sieve::new(hi as usize);
    let prime1 = sieve.nth_prime(p1);
    let prime2 = sieve.nth_prime(p2);

    for i in sieve.primes_from(prime1).take_while(|i| *i <= prime2) {
        aux = Integer::from(aux * i as u32);
    }
    aux.to_string_radix(10)
}

fn main() {
    let mut j1 = Integer::new();
    for k in [2,3,5,7,11,13,17,19,23,29].iter() { 
        j1.assign_primorial(*k);
        println!("Primorial : {}", j1);
    }
    println!("Digits of primorial 10 : {}", partial(1, 10).chars().fold(0, |n, _| n + 1));
    println!("Digits of primorial 100 : {}", partial(1, 100).chars().fold(0, |n, _| n + 1));
    println!("Digits of primorial 1_000 : {}", partial(1, 1_000).chars().fold(0, |n, _| n + 1));
    println!("Digits of primorial 10_000 : {}", partial(1, 10_000).chars().fold(0, |n, _| n + 1));
    println!("Digits of primorial 100_000 : {}", partial(1, 100_000).chars().fold(0, |n, _| n + 1));
    
    let mut auxi = Integer::from(1);
    let ranges = vec![[1, 300_000], [300_001, 550_000], [550_001, 800_000], [800_001, 1_000_000]];
    let v = ranges.par_iter().map(|value| partial(value[0], value[1])).collect::<Vec<_>>();
    for i in v.iter() {
        auxi =Integer::from(&auxi * i.parse::<Integer>().unwrap());
    }
    let result = auxi.to_string_radix(10).chars().fold(0, |n, _| n+1);
    println!("Digits of primorial 1_000_000 : {}",result);
}

```

using Intel(R) Core(TM) i7-5500U CPU @ 2.40GHz
```txt

Primorial : 2
Primorial : 6
Primorial : 30
Primorial : 210
Primorial : 2310
Primorial : 30030
Primorial : 510510
Primorial : 9699690
Primorial : 223092870
Primorial : 6469693230
Digits of primorial 10 : 10
Digits of primorial 100 : 220
Digits of primorial 1_000 : 3393
Digits of primorial 10_000 : 45337
Digits of primorial 100_000 : 563921
Digits of primorial 1_000_000 : 6722809

real	0m19.448s
user	1m2.306s
sys	0m0.054s

```


## zkl

Using [[Extensible prime generator#zkl]] and the GMP big int library.

```zkl
sieve:=Import("sieve.zkl",False,False,False).postponed_sieve;
primes:=Utils.Generator(sieve).walk(0d10);  // first 10 primes
foreach n in (10)
   { primes[0,n].reduce('*,1):println("primorial(%d)=%d".fmt(n,_)); }

var [const] BN=Import("zklBigNum");
primes:=Utils.Generator(sieve).walk(0d1_000_000);
foreach n in ([1..6]){ n=(10).pow(n);
   primes[0,n].pump(BN(1).mul)
   :println("primorial(%,d)=%,d digits".fmt(n,_.numDigits));
}
```

Big int multiplication is done in place to minimize garbage. Also, subsets of read only lists (which the list of primes is) are not copies (ie they are a small header that points into the original list).

Strangely, it takes much longer to do one million big int multiplications than to generate one million primes, so it would be a good idea to "generate a prime, multiply, print at intervals" but I'm too lazy. 
```txt

primorial(0)=1
primorial(1)=2
primorial(2)=6
primorial(3)=30
primorial(4)=210
primorial(5)=2310
primorial(6)=30030
primorial(7)=510510
primorial(8)=9699690
primorial(9)=223092870
primorial(10)=10 digits
primorial(100)=220 digits
primorial(1,000)=3,393 digits
primorial(10,000)=45,338 digits
primorial(100,000)=563,921 digits
primorial(1,000,000)=6,722,809 digits

```

