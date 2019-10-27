+++
title = "Extensible prime generator"
description = ""
date = 2019-09-27T00:44:23Z
aliases = []
[extra]
id = 17458
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}

;Task:
Write a generator of prime numbers, in order, that will automatically adjust to accommodate the generation of any reasonably high prime.

The routine should demonstrably rely on either:
# Being based on an open-ended counter set to count without upper limit other than system or programming language limits. In this case, explain where this counter is in the code.
# Being based on a limit that is extended automatically. In this case, choose a small limit that ensures the limit will be passed when generating some of the values to be asked for below.
# If other methods of creating an extensible prime generator are used, the algorithm's means of extensibility/lack of limits should be stated.


The routine should be used to:
* Show the first twenty primes.
* Show the primes between 100 and 150.
* Show the ''number'' of primes between 7,700 and 8,000.
* Show the 10,000th prime.



Show output on this page.

'''Note:''' You may reference code already on this site if it is written to be imported/included, then only the code necessary for import and the performance of this task need be shown. (It is also important to leave a forward link on the referenced tasks entry so that later editors know that the code is used for multiple tasks). 

'''Note 2:''' If a languages in-built prime generator is extensible or is guaranteed to generate primes up to a system limit, (2<sup>31</sup> or memory overflow for example), then this may be used as long as an explanation of the limits of the prime generator is also given. (Which may include a link to/excerpt from, language documentation). 

;nice site to check results:
Website with vast count of primes. Small ones for the first 10000 and up to 1,000,000,000,000 aka 1E12, divided in subranges : "Each compressed file contains 10 million primes" 

http://www.primos.mat.br/indexen.html



* The task is written so it may be useful in solving the task   [[Emirp primes]]   as well as others (depending on its efficiency).






## Ada


The solution is based on an open-ended counter, named "Current" counting up to the limit from the Compiler, namely 2**63-1.

The solution uses the package Miller_Rabin from the [[Miller-Rabin primality test]]. When using the gnat Ada compiler, the largest integer we can deal with is 2**63-1. For anything larger, we could use a big-num package. 


```Ada
with Ada.Text_IO, Miller_Rabin;

procedure Prime_Gen is
   
   type Num is range 0 .. 2**63-1; -- maximum for the gnat Ada compiler
   
   MR_Iterations: constant Positive := 25; 
     -- the probability Pr[Is_Prime(N, MR_Iterations) = Probably_Prime] 
     -- is 1 for prime N and < 4**(-MR_Iterations) for composed N
   
   function Next(P: Num) return Num is
      N: Num := P+1;
      package MR is new Miller_Rabin(Num); use MR;
   begin
      while not (Is_Prime(N, MR_Iterations) = Probably_Prime) loop
	 N := N + 1;
      end loop;
      return N;
   end Next;
   
   Current: Num;
   Count: Num := 0;
   
begin
   -- show the first twenty primes
   Ada.Text_IO.Put("First 20 primes:");
   Current := 1;
   for I in 1 .. 20 loop
      Current := Next(Current);
      Ada.Text_IO.Put(Num'Image(Current));
   end loop;
   Ada.Text_IO.New_Line;
   
   -- show the primes between 100 and 150
   Ada.Text_IO.Put("Primes between 100 and 150:");
   Current := 99;
   loop
      Current := Next(Current);
      exit when Current > 150;
      Ada.Text_IO.Put(Num'Image(Current));
   end loop;
   Ada.Text_IO.New_Line;
   
   -- count primes between 7700 and 8000
   Ada.Text_IO.Put("Number of primes between 7700 and 8000:");
   Current := 7699;
   loop
      Current := Next(Current);
      exit when Current > 8000;
      Count := Count + 1;
   end loop;
   Ada.Text_IO.Put_Line(Num'Image(Count));
   
   Count := 10;
   Ada.Text_IO.Put_Line("Print the K_i'th prime, for $K=10**i:");
   begin
      loop
	 Current := 1;
	 for I in 1 .. Count loop
	    Current := Next(Current);
	 end loop;
	 Ada.Text_IO.Put(Num'Image(Count) & "th prime:" & 
			Num'Image(Current));
	 Count := Count * 10;
      end loop;
   exception
      when Constraint_Error => 
	 Ada.Text_IO.Put_Line(" can't compute the" & Num'Image(Count) &
				"th prime:");
   end;
end;
```


{{out}}


```txt
First 20 primes: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
Primes between 100 and 150: 101 103 107 109 113 127 131 137 139 149
Number of primes between 7700 and 8000: 30
Print the K_i'th prime, for $K=10**i:
 10th prime: 29 100th prime: 541 1000th prime: 7919 10000th prime: 104729 100000th prime: 1299709 1000000th prime: 15485863
```

(The program has been stopped after running several days.)


## AutoHotkey


```AutoHotkey
SetBatchLines, -1
p := 1	;p functions as the counter
Loop, 10000 {
	p := NextPrime(p)
	if (A_Index < 21)
		a .= p ", "
	if (p < 151 && p > 99)
		b .= p ", "
	if (p < 8001 && p > 7699)
		c++
}
MsgBox, % "First twenty primes: " RTrim(a, ", ")
	. "`nPrimes between 100 and 150: " RTrim(b, ", ")
	. "`nNumber of primes between 7,700 and 8,000: " RTrim(c, ", ")
	. "`nThe 10,000th prime: " p

NextPrime(n) {
	Loop
		if (IsPrime(++n))
			return n
}

IsPrime(n) {
	if (n < 2)
		return, 0
	else if (n < 4)
		return, 1
	else if (!Mod(n, 2))
		return, 0
	else if (n < 9)
		return 1
	else if (!Mod(n, 3))
		return, 0
	else {
		r := Floor(Sqrt(n))
		f := 5
		while (f <= r) {
			if (!Mod(n, f))
				return, 0
			if (!Mod(n, (f + 2)))
				return, 0
			f += 6
		}
		return, 1
	}
}
```

{{Output}}

```txt
First twenty primes: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71
Primes between 100 and 150: 101, 103, 107, 109, 113, 127, 131, 137, 139, 149
Number of primes between 7,700 and 8,000: 30
The 10,000th prime: 104729
```



## C

Extends the list of primes by sieving more chunks of integers.  There's no serious optimizations. The code can calculate all 32-bit primes in some seconds, and will overflow beyond that.

```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <math.h>

#define CHUNK_BYTES (32 << 8)
#define CHUNK_SIZE (CHUNK_BYTES << 6)

int field[CHUNK_BYTES];
#define GET(x) (field[(x)>>6] &  1<<((x)>>1&31))
#define SET(x) (field[(x)>>6] |= 1<<((x)>>1&31))

typedef unsigned uint;
typedef struct {
        uint *e;
        uint cap, len;
} uarray;
uarray primes, offset;

void push(uarray *a, uint n)
{
        if (a->len >= a->cap) {
                if (!(a->cap *= 2)) a->cap = 16;
                a->e = realloc(a->e, sizeof(uint) * a->cap);
        }
        a->e[a->len++] = n;
}

uint low;
void init(void)
{
        uint p, q;

        unsigned char f[1<<16];
        memset(f, 0, sizeof(f));
        push(&primes, 2);
        push(&offset, 0);
        for (p = 3; p < 1<<16; p += 2) {
                if (f[p]) continue;
                for (q = p*p; q < 1<<16; q += 2*p) f[q] = 1;
                push(&primes, p);
                push(&offset, q);
        }
        low = 1<<16;
}

void sieve(void)
{
        uint i, p, q, hi, ptop;
        if (!low) init();

        memset(field, 0, sizeof(field));

        hi = low + CHUNK_SIZE;
        ptop = sqrt(hi) * 2 + 1;

        for (i = 1; (p = primes.e[i]*2) < ptop; i++) {
                for (q = offset.e[i] - low; q < CHUNK_SIZE; q += p)
                        SET(q);
                offset.e[i] = q + low;
        }

        for (p = 1; p < CHUNK_SIZE; p += 2)
                if (!GET(p)) push(&primes, low + p);

        low = hi;
}

int main(void)
{
        uint i, p, c;

        while (primes.len < 20) sieve();
        printf("First 20:");
        for (i = 0; i < 20; i++)
                printf(" %u", primes.e[i]);
        putchar('\n');

        while (primes.e[primes.len-1] < 150) sieve();
        printf("Between 100 and 150:");
        for (i = 0; i < primes.len; i++) {
                if ((p = primes.e[i]) >= 100 && p < 150)
                        printf(" %u", primes.e[i]);
        }
        putchar('\n');

        while (primes.e[primes.len-1] < 8000) sieve();
        for (i = c = 0; i < primes.len; i++)
                if ((p = primes.e[i]) >= 7700 && p < 8000) c++;
        printf("%u primes between 7700 and 8000\n", c);

        for (c = 10; c <= 100000000; c *= 10) {
                while (primes.len < c) sieve();
                printf("%uth prime: %u\n", c, primes.e[c-1]);
        }

        return 0;
}
```

{{out}}

```txt

First 20: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
Between 100 and 150: 101 103 107 109 113 127 131 137 139 149
30 primes between 7700 and 8000
10th prime: 29
100th prime: 541
1000th prime: 7919
10000th prime: 104729
100000th prime: 1299709
1000000th prime: 15485863
10000000th prime: 179424673
100000000th prime: 2038074743

```



## Clojure



```Clojure
ns test-project-intellij.core
  (:gen-class)
  (:require [clojure.string :as string]))

(def primes
" The following routine produces a infinite sequence of primes 
  (i.e. can be infinite since the evaluation is lazy in that it 
  only produces values as needed).  The method is from clojure primes.clj library
  which produces primes based upon O'Neill's paper:
  'The Genuine Sieve of Eratosthenes'.  

   Produces primes based upon trial division on previously found primes up to
   (sqrt number), and uses 'wheel' to avoid
   testing numbers which are divisors of 2, 3, 5, or 7.
   A full explanation of the method is available at:
   [https://github.com/stuarthalloway/programming-clojure/pull/12] "

  (concat
    [2 3 5 7]
    (lazy-seq
      (let [primes-from   ; generates primes by only checking if primes 
                          ; numbers which are not divisible by 2, 3, 5, or 7
            (fn primes-from [n [f & r]]
              (if (some #(zero? (rem n %))
                        (take-while #(<= (* % %) n) primes))
                (recur (+ n f) r)
                (lazy-seq (cons n (primes-from (+ n f) r)))))

            ; wheel provides offsets from previous number to insure we are not landing on a divisor of 2, 3, 5, 7
            wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                          6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                          2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
        (primes-from 11 wheel)))))

(defn between [lo hi]
  "Primes between lo and hi value "
  (->> (take-while #(<= % hi) primes)
       (filter #(>= % lo))
       ))
	   
(println "First twenty:" (take 20 primes))
   
(println "Between 100 and 150:" (between 100 150))

(println "Number between 7,7700 and 8,000:" (count (between 7700 8000)))

(println "10,000th prime:" (nth primes (dec 10000)))    ; decrement by one since nth starts counting from 0


}
```

{{out}}

```txt

First 20: (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71)
Between 100 and 150: (101 103 107 109 113 127 131 137 139 149)
Number between 7,700 and 8,000: 30
10000th prime: 104729

```


'''Alternate version using deferred execution Co-Inductive Streams and a Wheel'''

The above version is adequate for ranges up to the low millions, so covers the task requirements of primes up to just over a hundred thousands easily.  However, it has a O(n^(3/2)) performance which means that it gets slow quite quickly with range as compared to a true incremental Sieve of Eratosthenes, which has O(n (log n)) performance.  The following code is about the same speed for ranges in the low millions but quickly passes the above code in speed for large ranges to where it only takes 10's of seconds for a range of a hundred million where the above code takes thousands of seconds.  The code is based on the Richard Bird list based Sieve of Eratosthenes mentioned in the O'Neil article but has infinite tree folding added as well as wheel factorization so that it is about the same speed and performance as a Sieve of Eratosthenes based on a priority queue; the code is written here in purely functional form with no mutation, as follows:

```clojure
(deftype CIS [v cont]
  clojure.lang.ISeq
    (first [_] v)
    (next [_] (if (nil? cont) nil (cont)))
    (more [this] (let [nv (.next this)] (if (nil? nv) (CIS. nil nil) nv)))
    (cons [this o] (clojure.core/cons o this))
    (empty [_] (if (and (nil? v) (nil? cont)) nil (CIS. nil nil)))
    (equiv [this o] (loop [cis1 this, cis2 o] (if (nil? cis1) (if (nil? cis2) true false)
                                                (if (or (not= (type cis1) (type cis2))
                                                        (not= (.v cis1) (.v ^CIS cis2))
                                                        (and (nil? (.cont cis1))
                                                              (not (nil? (.cont ^CIS cis2))))
                                                        (and (nil? (.cont ^CIS cis2))
                                                              (not (nil? (.cont cis1))))) false
                                                  (if (nil? (.cont cis1)) true
                                                    (recur ((.cont cis1)) ((.cont ^CIS cis2))))))))
    (count [this] (loop [cis this, cnt 0] (if (or (nil? cis) (nil? (.cont cis))) cnt
                                            (recur ((.cont cis)) (inc cnt)))))
  clojure.lang.Seqable
    (seq [this] (if (and (nil? v) (nil? cont)) nil this))
  clojure.lang.Sequential
  Object
    (toString [this] (if (and (nil? v) (nil? cont)) "()" (.toString (seq (map identity this))))))

(comment " the wheel could also be a pre-determined vector as for the 2/3/5/7 wheel below...
(def wheel
  [ 2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
    6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
    2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10 ])
")

(def wheel-primes [2 3 5 7 11 13 17])

(def next-prime 19)

(def nextnext-prime 23)

;; calculates the vector for very large wheels such as the 92160 element version here
;; the disadvantage is that it takes some time to calculate before the work can start...
(def wheel
  (loop [p 2, len 1, ^bytes ptrn [1]]
    (if (>= p next-prime)
      ptrn
      (let [cptrn (cycle ptrn), [f & rcyc] cptrn,
            np (+ p f), nlen (* len (- p 1)),
            culls
              (map (fn [[f _]] f)
                (iterate (fn [[c [g & r]]] [(+ c (* p g)) r]) [(* p p) cptrn])),
            gaps (drop 1
                    (for [[gp _ _ _ cnt]
                            (iterate (fn [[_ v cls [g & rgs] c]]
                                  (let [[cl & rcls] cls, tv (+ v g),
                                        [sg & srgs] rgs, nc (+ c 1)]
                                    (if (= cl tv)
                                      [(+ g sg) (+ tv sg) rcls srgs nc]
                                      [g tv cls rgs nc])))
                                [f np culls rcyc 0]) :while (<= cnt nlen)] gp))]
        (recur np nlen (vec gaps))))))

(def wheellmt (- (count wheel) 1))

(defn primes-treeFolding
  "Computes the unbounded sequence of primes using a Sieve of Eratosthenes algorithm modified from Bird."
  []
  (letfn [(mltpls [[p pi]]
            (letfn [(nxtmltpl [c ci]
                      (let [nci (if (< ci wheellmt) (+ ci 1) 0)]  
                        (->CIS c #(-> (nxtmltpl (+ c (* p (get wheel ci))) nci)))))]
              (nxtmltpl (* p p) pi))),
          (allmtpls [^CIS pxs]
            (->CIS (mltpls (.v pxs)) #(-> (allmtpls ((.cont pxs)))))),
          (union [^CIS xs ^CIS ys]
            (let [xv (.v xs), yv (.v ys)]
              (if (< xv yv) (->CIS xv #(-> (union ((.cont xs)) ys)))
                (if (< yv xv)
                  (->CIS yv #(-> (union xs ((.cont ys)))))
                  (->CIS xv #(-> (union (next xs) ((.cont ys))))))))),
          (pairs [^CIS mltplss] (let [^CIS tl ((.cont mltplss))]
                                  (->CIS (union (.v mltplss) (.v tl))
                                          #(-> (pairs ((.cont tl))))))),
          (mrgmltpls [^CIS mltplss]
            (->CIS (.v ^CIS (.v mltplss))
                    #(-> (union ((.cont ^CIS (.v mltplss)))
                                (mrgmltpls (pairs ((.cont mltplss)))))))),
          (minusStrtAt [n ni ^CIS cmpsts]
            (let [nn (+ n (get wheel ni)), nni (if (< ni wheellmt) (+ ni 1) 0)]
              (if (< n (.v cmpsts))
                (->CIS [n ni] #(-> (minusStrtAt nn nni cmpsts)))
                (recur nn nni ((.cont cmpsts)))))),
          (xtraprmsndxd []
            (->CIS [next-prime 0] #(-> (minusStrtAt nextnext-prime 1
                                (mrgmltpls (allmtpls (xtraprmsndxd))))))),
          (stripndxs [^CIS ndxd]
            (->CIS (get (.v ndxd) 0) #(-> (stripndxs ((.cont ndxd))))))]
    (loop [i (- (count wheel-primes) 1), ff (fn [] (stripndxs (xtraprmsndxd)))]
        (if (<= i 0)
          (->CIS (get wheel-primes 0) ff)
          (recur (- i 1) (fn [] (->CIS (get wheel-primes i) ff)))))))
```


Now these functional incremental sieves are of limited use if one requires ranges of billions as they are hundreds of times slower than a version of a bit-packed page-segmented mutable array Sieve of Eratosthenes, which for Clojure there is a version at the end of Clojure [[Sieve_of_Eratosthenes#Unbounded_Versions]] section on the Sieve of Eratosthenes task page; this version will handle ranges of a billion in seconds rather than hundreds of seconds.

=={{Header|D}}==
This uses a Prime struct defined in the [[Sieve of Eratosthenes#Extensible Version|third entry of the Sieve of Eratosthenes]] task. Prime keeps and extends a dynamic array instance member of uints. The Prime struct has a opCall that returns the n-th prime number. The opCall calls a grow() private method until the dynamic array of primes is long enough to contain the required answer. The function grow() just grows the dynamic array geometrically and performs a normal sieving. On a 64 bit system this program works up to the maximum prime number that can be represented in the 32 bits of an uint. This program is less efficient than the C entry, so it's better to not use it past some tens of millions of primes, but it's enough for more limited usages.

```d
void main() {
    import std.stdio, std.range, std.algorithm, sieve_of_eratosthenes3;

    Prime prime;
    writeln("First twenty primes:\n", 20.iota.map!prime);
    writeln("Primes primes between 100 and 150:\n",
            uint.max.iota.map!prime.until!q{a > 150}.filter!q{a > 99});
    writeln("Number of primes between 7,700 and 8,000: ",
            uint.max.iota.map!prime.until!q{a > 8_000}
            .filter!q{a > 7_699}.walkLength);
    writeln("10,000th prime: ", prime(9_999));
}
```

{{out}}

```txt
First twenty primes:
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]
Primes primes between 100 and 150:
[101, 103, 107, 109, 113, 127, 131, 137, 139, 149]
Number of primes between 7,700 and 8,000: 30
10,000th prime: 104729
```



### Faster Alternative Version


```d
/// Prime sieve based on: http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

import std.container: Array, BinaryHeap, RedBlackTree;

struct LazyPrimeSieve {
    @property bool empty() const pure nothrow @safe @nogc {
        return i > 203_280_221; // Pi(2 ^^ 32).
    }

    @property auto front() const pure nothrow @safe @nogc {
        return prime;
    }

    @property void popFront() pure nothrow /*@safe*/ {
        prime = sieveOne();
    }

private:
    static struct Wheel2357 {
        static immutable ubyte[48] holes = [2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6,
            2, 6, 4, 2, 6, 4, 6, 8, 4, 2, 4, 2, 4, 8, 6, 4, 6, 2, 4, 6, 2, 6, 6,
            4, 2, 4, 6, 2, 6, 4, 2, 4, 2, 10, 2, 10];
        static immutable ubyte[4] spokes = [2, 3, 5, 7];
        static immutable ubyte first = 11;
        uint i;

        auto spin() pure nothrow @safe @nogc {
            return holes[i++ % $];
        }
    }

    static struct CompositeIterator {
        uint prime;
        Wheel2357 wheel;
        ulong composite;

        this(uint p) pure nothrow @safe @nogc {
            prime = p;
            composite = p * wheel.first;
        }

        void next() pure nothrow @safe @nogc {
            composite += prime * wheel.spin;
        }
    }

    version (heap)  // Less memory but slower.
        BinaryHeap!(Array!CompositeIterator, "a.composite > b.composite") iterators;
    else            // Faster but is more GC intensive.
        RedBlackTree!(CompositeIterator, "a.composite < b.composite", true) iterators;

    uint prime = 2;
    uint i = 1;
    Wheel2357 wheel;
    uint candidate = wheel.first;

    uint sieveOne() pure nothrow /*@safe*/ {
        switch (i) {
            case 0: .. case wheel.spokes.length - 1:
                return wheel.spokes[i++];

            case wheel.spokes.length:
                i++;
                return candidate;

            case wheel.spokes.length + 1:
                version (heap) {}
                else
                    iterators = new typeof(iterators);
                goto default;

            default:
                goto POST_RETURN;

                while (true) {
                    candidate += wheel.spin;

                    while (iterators.front.composite < candidate) {
                        auto it = iterators.front;
                        iterators.removeFront;
                        it.next;
                        iterators.insert(it);
                    }

                    if (iterators.front.composite != candidate) {
                        i++;
                        return candidate;
        POST_RETURN:
                        // Only insert primes that are multiply
                        // occuring in [0, 2 ^^ 32).
                        if (candidate < 2 ^^ 16)
                            iterators.insert(CompositeIterator(candidate));
                    }
                }
        }
    }
}


void main() /*@safe*/ {
    import std.stdio, std.algorithm, std.range;

    writeln("Sum of first 100,000 primes: ", LazyPrimeSieve().take(100_000).sum(0uL));

    writeln("First twenty primes:\n", LazyPrimeSieve().take(20));
    writeln("Primes primes between 100 and 150:\n",
            LazyPrimeSieve().until!q{a > 150}.filter!q{a > 99});
    writeln("Number of primes between 7,700 and 8,000: ",
            LazyPrimeSieve().until!q{a > 8_000}.filter!q{a > 7_699}.walkLength);
    writeln("10,000th prime: ", LazyPrimeSieve().dropExactly(9999).front);
}
```

{{out}}

```txt
Sum of first 100,000 primes: 62260698721
First twenty primes:
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]
Primes primes between 100 and 150:
[101, 103, 107, 109, 113, 127, 131, 137, 139, 149]
Number of primes between 7,700 and 8,000: 30
10,000th prime: 104729
```


=={{Header|Dart}}==

A version based on a (hashed) Map:


```dart>Iterable<int
 primesMap() {
    Iterable<int> oddprms() sync* {
      yield(3); yield(5); // need at least 2 for initialization
      final Map<int, int> bpmap = {9: 6};
      final Iterator<int> bps = oddprms().iterator;
      bps.moveNext(); bps.moveNext(); // skip past 3 to 5
      int bp = bps.current;
      int n = bp;
      int q = bp * bp;
      while (true) {
        n += 2;
        while (n >= q || bpmap.containsKey(n)) {
          if (n >= q) {
            final int inc = bp << 1;
            bpmap[bp * bp + inc] = inc;
            bps.moveNext(); bp = bps.current; q = bp * bp;
          } else {
            final int inc = bpmap.remove(n);
            int next = n + inc;
            while (bpmap.containsKey(next)) {
              next += inc;
            }
            bpmap[next] = inc;
          }
          n += 2;
        }
        yield(n);
      }
    }
    return [2].followedBy(oddprms());
}

void main() {
  print("The first 20 primes:");
  String str = "( ";
  primesMap().take(20).forEach((p)=>str += "$p "); print(str + ")");
  print("Primes between 100 and 150:");
  str = "( ";
  primesMap().skipWhile((p)=>p<100).takeWhile((p)=>p<150)
    .forEach((p)=>str += "$p "); print(str + ")");
  print("Number of primes between 7700 and 8000: ${
    primesMap().skipWhile((p)=>p<7700).takeWhile((p)=>p<8000).length
  }");
  print("The 10,000th prime:  ${
    primesMap().skip(9999).first
  }");
  final start = DateTime.now().millisecondsSinceEpoch;
  final answer = primesMap().takeWhile((p)=>p<2000000).reduce((a,p)=>a+p);
  final elapsed = DateTime.now().millisecondsSinceEpoch - start;
}
```

{{output}}

```txt
The first 20 primes:
( 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 )
Primes between 100 and 150:
( 101 103 107 109 113 127 131 137 139 149 )
Number of primes between 7700 and 8000: 30
The 10,000th prime:  104729
The sum of the primes to two million:  142913828922
This test bench took 356 milliseconds.
```


This version has a O(n log (log n)) computational complexity due to hash map access being O(1) on average but is somewhat slow due to the constant execution overhead and therefore only somewhat useful for ranges of up to about the tens of millions.

As a bonus, it solves Euler Problem 10 of summing all the primes up to two million quite quickly.

'''A faster alternative version based on the infinite page segmented sieve'''

The unbounded page segmented bit-packed version from the [[Sieve_of_Eratosthenes#Unbounded_infinite_iterators.2Fgenerators_of_primes]] at the bottom of the section can do the same job tens of times faster just by substituting 'primesPaged()' for 'primesMap()' in the 'main' function above in all places used.  As noted for the listing on the task page, the code is only limited in range by the integer size limit on 32-bit execution environments, and will be tens of times faster than the above version.  It will otherwise have the same output as above.

It solves the Euler Problem 10 in almost too short a time to be measured, and it becomes useful for ranges of hundreds of thousands.  It can count all the primes to a billion on the low end tablet CPU of an Intel x5-Z8350 at 1.92 Gigahertz used to develop this in 40 seconds but using a generator slows the performance and it can use the provided `countPrimesTo` function to do the job four times as fast by directly manipulating the provided iteration of sieved bit-packed arrays.


## EchoLisp

Standard prime functions handle numbers < 2e+9. See [http://www.echolalie.org/echolisp/help.html#prime?] . The '''bigint''' library handles large numbers. See [http://www.echolalie.org/echolisp/help.html#bigint.lib]. The only limitations are time, memory, and browser performances ..

```lisp

; the first twenty primes
(primes 20)
    → { 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 }

; a stream to generate primes from a
(define (primes-from a)
    (let ((p (next-prime a)))
     (stream-cons p (primes-from p))))

; primes between 100,150
(for/list  ((p (primes-from 100)))  #:break  (> p 150) p)
    → (101 103 107 109 113 127 131 137 139 149)

; the built-in function (primes-pi )counts the number of primes < a
; count in [7700 ...  8000]
(- (primes-pi 8000) (primes-pi 7700) → 30

; nth-prime 
(nth-prime 10000) → 104729

;; big ones
(lib 'bigint)
(define (p-digits n) 
  (printf "(next-prime  %d ! ) has %d digits" n 
  (number-length (next-prime (factorial n )))))

(next-prime 0! ) has 1 digits
(next-prime 10! ) has 7 digits
(next-prime 100! ) has 158 digits
(next-prime 200! ) has 375 digits
(next-prime 300! ) has 615 digits
(next-prime 400! ) has 869 digits ;; 9400 msec (FireFox)

; is prime (1 + 116!) ?
(prime? (1+ (factorial 116)))  → #t


```


=={{Header|Elixir}}==

The [[Sieve_of_Eratosthenes#Elixir]] Task page lists two "infinite" extensible generators at the bottom.  The first of those, using a (hash) Map is reproduced here along with the code to fulfill the required tasks:

```elixir
defmodule PrimesSoEMap do
  @typep stt :: {integer, integer, integer, Enumerable.integer, %{integer => integer}}

  @spec advance(stt) :: stt
  defp advance {n, bp, q, bps?, map} do
    bps = if bps? === nil do Stream.drop(oddprms(), 1) else bps? end
    nn = n + 2
    if nn >= q do
      inc = bp + bp
      nbps = bps |> Stream.drop(1)
      [nbp] = nbps |> Enum.take(1)
      advance {nn, nbp, nbp * nbp, nbps, map |> Map.put(nn + inc, inc)}
    else if Map.has_key?(map, nn) do
      {inc, rmap} = Map.pop(map, nn)
      [next] =
        Stream.iterate(nn + inc, &(&1 + inc))
          |> Stream.drop_while(&(Map.has_key?(rmap, &1))) |> Enum.take(1)
      advance {nn, bp, q, bps, Map.put(rmap, next, inc)}
    else
      {nn, bp, q, bps, map}
    end end
  end

  @spec oddprms() :: Enumerable.integer
  defp oddprms do # put first base prime cull seq in Map so never empty
    # advance base odd primes to 5 when initialized
    init = {7, 5, 25, nil, %{9 => 6}}
    [3, 5] # to avoid race, preseed with the first 2 elements...
      |> Stream.concat(
            Stream.iterate(init, &(advance &1))
              |> Stream.map(fn {p,_,_,_,_} -> p end))
  end

  @spec primes() :: Enumerable.integer
  def primes do
    Stream.concat([2], oddprms())
  end

end

IO.write "The first 20 primes are:\n( "
PrimesSoEMap.primes() |> Stream.take(20) |> Enum.each(&(IO.write "#{&1} "))
IO.puts ")"
IO.write "The primes between 100 to 150 are:\n( "
PrimesSoEMap.primes() |> Stream.drop_while(&(&1<100))
  |> Stream.take_while(&(&1<150)) |> Enum.each(&(IO.write "#{&1} "))
IO.puts ")"
IO.write "The number of primes between 7700 and 8000 is:  "
PrimesSoEMap.primes() |> Stream.drop_while(&(&1<7700))
  |> Stream.take_while(&(&1<8000)) |> Enum.count |> IO.puts
IO.write "The 10,000th prime is:  "
PrimesSoEMap.primes() |> Stream.drop(9999)
  |> Enum.take(1) |> List.first |>IO.puts
IO.write "The sum of all the priems to two million is:  "
testfunc =
  fn () ->
    ans =
      PrimesSoEMap.primes() |> Stream.take_while(&(&1<=2000000))
        |> Enum.sum() |> IO.puts
    ans end
:timer.tc(testfunc)
  |> (fn {t,_} ->
    IO.puts "This test bench took #{t} microseconds." end).()
```

{{output}}

```txt
The first 20 primes are:
( 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 )
The primes between 100 to 150 are:
( 101 103 107 109 113 127 131 137 139 149 )
The number of primes between 7700 and 8000 is:  30
The 10,000th prime is:  104729
The sum of all the primes to two million is:  142913828922
This test bench took 7827912 microseconds.
```


The code solves the (trivial) task requirements quickly, but being purely functional, running on a Virtual Machine, and using the multi-precision ("Big Integer") `integer` type is somewhat slower than as implemented in some other languages.

The code is an "infinite" generator as the `integer` type in Elixir is of multi-precision and thus will never run out of range.

As a bonus, the above code solves the Euler Problem 10 of summing the primes to two million in about 7.83 seconds, or about a hundred thousand CPU cycles per prime on a !.92 Gigahertz CPU, which at least is within the 30 second time limit for that problem.

'''Alternate somewhat faster version (over two times)'''

The last code on the Sieve of Eratosthenes Task page uses deferred execution Co-Inductive Streams (CIS's) to implement the incremental functional Sieve of Eratosthenes using an infinite tree folding structure with a final output as a lazy Stream just as for the above.  It is about twice as fast as the above for reasonable ranges of a few millions.  It con be used just by substituting calls to the different named module as in `PrimesSoETreeFolding.primes` rather than `PrimesSoEMap.primes()`.  It has the same limitations as to being "infinite" as the above.

=={{header|F_Sharp|F#}}==

### The function


```fsharp

// Extensible sequence of prime numbers. Nigel Galloway: November 19th., 2017
let primes=seq{
  let p=System.Collections.Generic.SortedSet<int*int>()
  let s=set[1;7;11;13;17;19;23;29]
  let rec fN=function|(n,g) when Set.contains(n%30)s->p.Add(n,g)|>ignore|(n,g)->fN (n+g+g,g)
  let rec fG n (g:int[]) i=seq{match (n, p.Min) with
                               |(n,(pn,pg)) when n>pn->fN(pn+pg+pg,pg); p.Remove(pn,pg)|>ignore; yield! fG n g i
                               |(n,(pn,pg)) when n=pn->fN(pn+pg+pg,pg); p.Remove(pn,pg)|>ignore; yield! fG(n+g.[i]) g ((i+1)%8)
                               |(n,_)                ->yield n; fN(n+n+n,n); yield! fG (n+g.[i]) g ((i+1)%8)
                              }
  p.Add(49,7)|>ignore; yield! [2;3;5;7]; yield! fG 11 [|2;4;2;4;6;2;6;4|] 0
}
let pCache = Seq.cache primes
let isPrime   g=if g<2 then false else let mx=int(sqrt(float g)) in pCache|>Seq.takeWhile(fun n->n<=mx)|>Seq.forall(fun n->g%n>0)
let isPrime64 g=if g<2L then false else let mx=int(sqrt(float g)) in pCache|>Seq.takeWhile(fun n->n<=mx)|>Seq.forall(fun n->g%(int64 n)>0L)

```



### The Task


```fsharp

Seq.take 20 primes|> Seq.iter (fun n-> printf "%d " n)

```

{{out}}

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71

```


```fsharp

primes |> Seq.skipWhile (fun n->n<100) |> Seq.takeWhile (fun n->n<=150) |> Seq.iter (fun n -> printf "%d " n)

```

{{out}}

```txt

101 103 107 109 113 127 131 137 139 149

```


```fsharp

printfn "%d" (primes |> Seq.skipWhile (fun n->n<7700) |> Seq.takeWhile (fun n->n<=8000) |> Seq.length)

```

{{out}}

```txt

30

```

To demonstrate extensibility I find the 10000th prime.

```fsharp

Seq.item 9999 pCache

```

{{out}}

```txt

Real: 00:00:00.185, CPU: 00:00:00.190, GC gen0: 21, gen1: 0
val it : int = 104729

```

I then find the 10001st prime which takes less time.

```fsharp

Seq.item 10000 pCache

```

{{out}}

```txt

Real: 00:00:00.004, CPU: 00:00:00.010, GC gen0: 1, gen1: 0
val it : int = 104743

```



### Unbounded Mutable Array Generator


While the above code does the job for small ranges, it is very slow for more than a few thousands of primes.  Use of the last unbounded mutable array generator from the [[Sieve_of_Eratosthenes#Almost_functional_Unbounded]] F# section (last version) on the Sieve of Eratosthenes page is much, much faster as demonstrated below:

```fsharp
printfn "The first 20 primes are:  %s"
  ( primesSeq() |> Seq.take 20
      |> Seq.fold (fun s p -> s + string p + " ") "" )
printfn "The primes from 150 to 150 are:  %s"
  ( primesSeq() |> Seq.skipWhile ((>) (prime 100))
      |> Seq.takeWhile ((>=) (prime 150))
      |> Seq.fold (fun s p -> s + string p + " ") "" )
printfn "The number of primes from 7700 to 8000 are:  %d"
  ( primesSeq() |> Seq.skipWhile ((>) (prime 7700))
      |> Seq.takeWhile ((>=) (prime 8000)) |> Seq.length )
let strt = System.DateTime.Now.Ticks
for i = 1 to 8 do
  let n = pown 10 i // the item index below is zero based!
  printfn "The %dth prime is:  %A" n (primesSeq() |> Seq.item (n - 1))
let timed = (System.DateTime.Now.Ticks - strt) / 10000L
printfn "All of the last took %d milliseconds." timed
```


{{out}}

```txt
The first 20 primes are:  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 
The primes from 150 to 150 are:  101 103 107 109 113 127 131 137 139 149 
The number of primes from 7700 to 8000 are:  30
The 10th prime is:  29
The 100th prime is:  541
The 1000th prime is:  7919
The 10000th prime is:  104729
The 100000th prime is:  1299709
The 1000000th prime is:  15485863
The 10000000th prime is:  179424673
The 100000000th prime is:  2038074743
All of the last took 16634 milliseconds.
```


Note that the last only takes a few seconds to calculate the first hundred million primes where other algorithms can take days!

Even at that, this is slow due to the time to enumerate and there are much faster ways to do this without using enumeration as in manipulating the found prime bit representations directly...


## Fortran


### The Plan

Over the years, the storage of boolean variables has been a steady source of vexation. Few systems offer operation codes that can work on individual bits, so the usual approach is to allocate a convenient storage unit to hold the value. In Fortran, the default size of a LOGICAL variable is the same as that of an INTEGER variable, and these days, that means thirty-two bits to store the state of one. However, with the increasing use of character manipulation rather than just numbers, there is often support in the cpu for single-character access and some later Fortran compilers will recognise LOGICAL*1 and so reserve only eight bits per boolean variable. Even so, any attempt to store a boolean variable in a single bit will for every access require code to isolate that bit from the rest of the storage unit where it resides, and these operation codes will require more storage than would be saved. Similarly with collections of variables: some might best be aligned to word boundaries (and for double-sized variables, perhaps to even word boundaries) so the storage plan may well involve adding "padding" to preserve such alignment. Some languages (such as pl/i) offer a word ALIGNED to ensure this, and others (such as Pascal) offer PACKED for cramming, of use when dealing with records for a disc file and intending to save space. So, ... if there is a large array of boolean variables, and, there are not so many references to those variables, there is still an opportunity.

And indeed, the array shall be large. Some simple investigations show that storing a collection of prime numbers in an array of integers occupies rather more storage than storing a simple bit array spanning the same range of numbers, and given the obvious scheme of storing bits only for odd integers, this advantage is still greater - see the schedule in the source file. One could argue that the array of successive prime numbers could be stored in various space-saving ways, but, so also can the bit array be compressed. For instance, have a span of a "primorial" size such as 2*3*5 and a reference span with those factors marked "off": that removes seven odd numbers from consideration, leaving eight candidates for each surge and so only eight bits are required to state "prime" or not instead of fifteen. With non-binary computers, "bit fiddling" is less convenient but still possible. One must use techniques similar to those needed to work with the year, month, and day parts of an integer such as 20161015 in binary.

So, the plan is to have a long array of bits, and, rather than commit a lot of memory to this, do so in a disc file with random access. To follow the "extensible" aspect, this disc file will ''not'' be initialised to its maximum extent on the first invocation of the routine, instead, it will be extended as provoked by requests for NEXTPRIME and so forth. 


### =Initialisation=

When arranging a sieve of Eratosthenes, one of the problems is that one wishes to step along only with steps of prime number size to avoid wasted effort, but, before the sieve process is completed, there is no ready source of known prime numbers. This is especially difficult when instead of one long sieve covering the whole span of interest, the process is to proceed in surges, repeatedly using some limited size span. For this reason, it is often convenient to prepare an initial array of prime numbers knowing for example that Prime(4792) = 46337, and that the square of the next prime exceeds the range of signed 32-bit integers. But such pre-emptive preparation conflicts with the "extensible" notion, and requires special code and storage for the array. 

Because Fortran passes parameters by reference (i.e. by address of the original) a trick is possible. The array SCHARS is shared storage to hold a record from the disc file (as a "buffer") and when subroutine GRASPPRIMEBAG is invoked to gain access to its disc file it notes whether it must create the file. If so, the first record is to be written, and the call is PSURGE(SCHARS) to do so within the shared bitpad. PSURGE knows that its first stepper is with <code>F = 3</code> (because even numbers are not being represented) and proceeds with that, adjusting array SCHARS. When it is ready for the next sieve pass, it invokes <code>F = NEXTPRIME(F)</code> to find the next stepper, which will be five, and NEXTPRIME scans the bit array in SCHARS to find it. ''This is the same bitpad that PSURGE is in the process of adjusting.'' To support this startup ploy, GETSREC (invoked by NEXTPRIME) returns at once when SLAST = 0, signifying that there are no records in the work file as yet. Later, if  GETSREC determines that the bit array is to be extended, it invokes PSURGE with its local array BIT8 as the bitpad to be developed then written to disc, leaving the shared SCHAR array as a record buffer for the use of NEXTPRIME when invoked by PSURGE.

====Supporting NextPrime(n)====
Since the bit array has a simple linear relationship to the numbers it is associated with, function <code>NEXTPRIME(n)</code> (and <code>PREVIOUSPRIME(n)</code>) can easily calculate the index to access the appropriate bits; similarly, function <code>ISPRIME(n)</code> need merely check <code>n = NEXTPRIME(n - 1)</code> rather than slog through possibly all potential prime factors up to SQRT(n) - though this does mean that prime numbers up to ''n'' must be available rather than merely up to SQRT(n). [A later adjustment has ISPRIME(n) repeat the code to locate the bit for ''n'' rather than use NEXTPRIME, which has to scan the bit array to the next prime] But there would be no escape from such a slog for function FIRSTFACTOR(n), unless one abandoned the bit array for a FF array and modified the sieve process to record the first factor. Something like <code>Bit(i) = .false.</code> would be replaced by <code>if (FF(i) <= 1) then FF(i) = F</code>  Were the assignment to be made unconditionally (for faster running, perhaps), the array should be renamed to MaximumPrimeFactor. Either way, ISPRIME(n) remains easy, but much more storage than one bit per entry would be required.

If instead of the next prime one desires to find the ''n'' 'th prime number, then there is a difficulty that would not exist if the prime numbers only were stored in an array - but if they were then <code>NEXTPRIME(n)</code> would have difficulty. Of course, if storage is abundant, both forms of storage could be used and each request could be handled via a simple linear index calculation into the appropriate array.

====Supporting Prime(n)====
To find the ''n'''th prime number, obviously one could scan along the bit array from the start, keeping count. This will soon become tedious, so in order to support function <code>PRIME(n)</code>, each record starts with a count of all the primes that have preceded that record's span and so to find the count for a prime fingered in that record, the scan need work only from the start of that record. So the problem reduces to determining which record is the one containing the ''n'' 'th prime. Since the counts are obviously strictly increasing, a binary search would be a possibility as would be an interpolating search and one could even prepare an array containing the counts so that the search could proceed without needing disc accesses - at the cost of additional storage and organisational complexity, perhaps involving Aitken's interpolation formula, except that polynomials do not provide a good fit to the required shape. Fortunately, mathematicians have considered this aspect of prime numbers also, and a rather intimidating formula is available to give an estimate of the value of the ''n'' 'th prime number. Equipped with this, the appropriate record can be read, the count inspected, and a scan started to find the actual ''n'' 'th prime. Function <code>PRIME(n)</code> can be invoked just as array <code>PRIME(n)</code> might be, but with something of a roil of activity in the background.

Preparing the count field involves another trick, because the first record's count field instead holds the count of records in the bit file. Now note that the sieve starts with <code>SORG = 3</code> which means that before the first block there is a count of one prime number. Thus, if function PRIME is accessing the first block, it must know that the count of previous primes is one and not refer to the count field which instead holds the record count. When PSURGE is preparing the next batch of bits (for the second and subsequent records) it accesses the previous record to find the previous count and scans that record to count its primes so as to prepare the previous count for the new record. When the second record is being prepared by PSURGE, the previous record (the first record) has a record count of one, and, this is exactly the desired count of previous primes for the first record. But only at this point, because in moments the first record will be rewritten with a record count of two. For all this to work, <code>SORG = 3</code> must be the case: it is not a parameter but a constant.


### =Integer Overflow=

The variables are all the default thirty-two bit two's complement integers and integer overflow is a possibility, especially because someone is sure to wonder what is the largest prime that can be represented in thirty-two bits - see the output. The code would be extensible in another way, if all appearances of <code>INTEGER</code> were to be replaced by <code>INTEGER*8</code> though not all variables need be changed - such as <code>C</code> and <code>B</code> because they need only index a character in array SCHARS or a bit in a character. Using sixty-four bits for such variables is excessive even if the cpu uses a 64-bit data bus to memory. If such a change were to be made, then all would go well as cpu time and disc space were consumed up to the point when the count of prime numbers can no longer be fitted into the four character storage allowance in the record format. This will be when more than 4,294,967,295 primes have been counted (with 64-bit arithmetic its four bytes will manifest as an unsigned integer) in previous records, and Prime(4,294,967,295) = 104,484,802,043, so that the bit file would require a mere 6,530MB or so - which some may think is not too much. If so, expanding the allowance from four to five characters would be easy enough, and then 256 times as many primes could be counted. That would also expand the reach of the record counter, which otherwise would be limited to 4,294,967,295 records of 4096 bytes each, or a bit bag of 17,592,186,040,320 bytes - only seventeen terabytes...

Overflow is also a problem in many of the calculations. For instance, for a given (prime) number F, the marking of multiples of F via the sieve process starts with the bit corresponding to F² and if this exceeds the number corresponding to the last bit of the current sieve span, then the sieve process is complete for this span because all later values for F will be still further past the end. So, if <code>LST</code> is the number corresponding to the last bit of the current span, 
```Fortran
      DO WHILE(F*F <= LST)         !But, F*F might overflow the integer limit so instead,
      DO WHILE(F <= LST/F)                      !Except, LST might also overflow the integer limit, so 
      DO WHILE(F <= (IST + 2*(SBITS - 1))/F)    !Which becomes...
      DO WHILE(F <= IST/F + (MOD(IST,F) + 2*(SBITS - 1))/F) !Preserving the remainder from IST/F.
```

Except, <code>IST</code> might overflow the integer limit, in which case function PSURGE declares itself unable to proceed and returns ''false''.

Overflow is detected by the sudden appearance of negative numbers, as is characteristic of two's complement integer arithmetic. This is ''not'' guaranteed to be used on all computers (notably, on a decimal computer such as the IBM1620 and others), and in its absence, the procedure will malfunction. Some systems detect integer overflow via hardware (a special "flag" register, or an interrupt) and there may be facilities for noticing such events. First Fortran (1957) offered special statements such as <code>IF ACCUMULATOR OVERFLOW ''labelon'',''labeloff''</code> (yes, without brackets) and similarly for QUOTIENT OVERFLOW and DIVIDE CHECK but they were abandoned by the modernisers. The only general solution to this problem would be to convert to using multiple-precision (or "bignum") arithmetic, whereupon the code becomes extensible in another way simply by extending as needed the amount of storage allowed for variables.

These methods have been tested by converting <code>INTEGER</code> to <code>INTEGER*2</code> and also by using a record size of sixteen bytes (because UltraEdit, when displaying in binary, shows that many bytes to a line), and it was a real pleasure for once to be able to read the 32-bit count field at the start of each record left-to-right in hexadecimal rather than in the crazed little-endian order that would otherwise have been used.


### The Code

The source code employs the MODULE facility of F90 simply to avoid the tedium of setting up a COMMON storage area and having to declare the type of the functions in every routine that uses them. Otherwise, older style compilers will accept this, except for an occasional array facility (such as <code>BIT8 = CHAR(255)</code>) and the use of the $ format code to allow the next output to tag on to the same line. The rather more fearsome declaration <code>RECURSIVE FUNCTION NEXTPRIME</code> could be avoided if in subroutine PSURGE, the invocation of NEXTPRIME was replaced by in-line code. Similarly with subroutine GETSREC, though forgetting this didn't seem to make any difference. This is just convenience recursion, not structural recursion to some arbitrary depth.

Although recursion is now permissible if one utters the magic word <code>RECURSIVE</code>, this ability usually is not extended to the workings for formatted I/O so that if say a function is invoked in a WRITE statement's list, should that function attempt to use a WRITE statement, the run will be stopped. There can be slight dispensations if different types of WRITE statement are involved (say, formatted for one, and "free"-format for the other) but an ugly message is the likely result. The various functions are thus best invoked via an assignment statement to a scratch variable, which can then be printed. The functions are definitely not "pure" because although they are indeed functions only of their arguments, they all mess with shared storage, can produce error messages (and even a STOP), and can provoke I/O with a disc file, even creating such a file. For this reason, it would be unwise to attempt to invoke them via any sort of parallel processing. Similarly, the disc file is opened with exclusive use because of the possibility of writing to it. There are no facilities in standard Fortran to control the locking and unlocking of records of a disc file as would be needed when adding a new record and updating the record count. This would be needed if separate tasks could be accessing the bit file at the same time, and is prevented by exclusive use. If an interactive system were prepared to respond to requests for ISPRIME(n), ''etc.'' it should open the bit file only for its query then close it before waiting for the next request - which might be many milliseconds away.

The bit array is stored in an array of type CHARACTER*1 since this has been available longer and more widely than INTEGER*1. One hopes that the consequent genuflections to type checking via functions CHAR(i) and ICHAR(c) will not involve an overhead. 
```Fortran
      MODULE PRIMEBAG	!Need prime numbers? Plenty are available.
C   Creates and expands a disc file for a sieve of Eratoshenes, representing odd numbers only and starting with three.
C   Storage requirements: an array of N prime numbers in 16/32/64 bits vs. a bit array up to the 16/32/64 bit limit.
C Word size               N            Prime   N words in bits           Bit array in bits.
C     8 bit            P(31) =           127               248                         128
C                      P(54) =           251               432                         256
C    16 bit         P(3,512) =        32,749            56,192                      32,768
C                   P(6,542) =        65,521           104,672                      65,536
C    32 bit   P(105,097,565) = 2,147,483,647     3,363,122,080               2,147,483,648
C             P(203,280,221) = 4,294,967,291     6,504,967,072               4,294,967,296
C    64 bit        2.112E17                ?          1.352E19   9,223,372,036,854,775,808 ~ 9.22E18
C from n/Ln(n)     4.158E17                ?          2.661E19  18,446,744,073,709,551,616 ~ 1.84E19
       INTEGER MSG	!I/O unit number.
       INTEGER SSTASH	!For attachment to my stash file.
       INTEGER SRECLEN,SCHARS,SBITS	!Sizes.
       INTEGER SORG	!Where the sieve starts. This must be three.
       INTEGER SLAST	!Last record in my stash file.
       DATA SSTASH,SREC,SLAST/0,0,0/	!Prepared by PRIMEBAG.
       PARAMETER (SRECLEN = 1024)	!4K disc bloc size, but RECL (in OPEN) is in terms of four-byte integers.
       PARAMETER (SCHARS = (SRECLEN - 1)*4)	!Reserving space for one number at the start.
       PARAMETER (SBITS = SCHARS*8)	!Known size of a character.
       PARAMETER (SORG = 3)		!First odd number past two, which is not odd.
       CHARACTER*(*) SFILE		!A name is needed.
       PARAMETER (SFILE = "C:/Nicky/RosettaCode/Primes/PrimeSieve.bit")	!I don't have to count the characters.
Components of a buffered record for the stash.
       INTEGER SREC	!The record number.
       CHARACTER*1 C4(4)	!The start of the record - a counter.
       CHARACTER*1 SCHAR(0:SCHARS - 1)	!The majority of the record - a bit array, packed in 8-bit blobs...
Collect some bit twiddling assistants for AND and OR, rather than bit shifting.
       CHARACTER*1 BITON(0:7),BITOFF(0:7)	!Functions IBSET and IBCLR may not be available, and are little-endian anyway.
       PARAMETER (BITON =(/CHAR(2#10000000),CHAR(2#01000000),	!128,  64,	Reading strictly left-to-right.
     1                     CHAR(2#00100000),CHAR(2#00010000),	! 32,  16,	Uncompromising bigendery.
     1                     CHAR(2#00001000),CHAR(2#00000100),	!  8,   4,	Not just for bytes in words,
     3                     CHAR(2#00000010),CHAR(2#00000001)/))	!  2,   1.	But also bits in bytes.
       PARAMETER (BITOFF=(/CHAR(2#01111111),CHAR(2#10111111),	!127, 191,	BITON + BITOFF = 255.
     2                     CHAR(2#11011111),CHAR(2#11101111),	!223, 239,
     1                     CHAR(2#11110111),CHAR(2#11111011),	!247, 251,
     3                     CHAR(2#11111101),CHAR(2#11111110)/))	!253, 254.
       CONTAINS
        INTEGER FUNCTION I4UNPACK(C4)	!Convert four successive characters into an integer.
         CHARACTER*1 C4(4)	!The characters.
          I4UNPACK = ((ICHAR(C4(1))*256 + ICHAR(C4(2)))*256	!Convert the first four bytes
     1               + ICHAR(C4(3)))*256 + ICHAR(C4(4))		!To a four-byte integer.
        END FUNCTION I4UNPACK	!Big-endian style, irrespective of cpu endianness.
        SUBROUTINE C4PACK(I4)	!Convert an integer into successive bytes.
Could return the result via a fancy function, but for now a global variable will do.
         INTEGER I4,N	!The integer, and a copy to damage.
         INTEGER I	!A stepper.
          N = I4	!Keep the original safe.
          DO I = 4,1,-1	!Know that four characters will do. Fixed format makes this easy.
            C4(I) = CHAR(MOD(N,256))	!Grab the low-order eight bits.
            N = N/256			!And shift right eight.
          END DO			!Do it again.
        END SUBROUTINE C4PACK	!Stored big-endianly, irrespective of cpu endianness.

        LOGICAL FUNCTION GRASPPRIMEBAG(F)
         INTEGER F		!The I/O unit number to use.
         LOGICAL EXIST		!Use the keyword as a name
         INTEGER IOSTAT		!And don't worry over assignment direction.
         CHARACTER*3 STYLE	!One way or another.
          SSTASH = F		!I shall use it.
          INQUIRE (FILE = SFILE,EXIST = EXIST)	!Trouble with a missing "path" may arise.
          IF (EXIST) THEN	!If the file exists,
            STYLE = "OLD"	!I shall read it.
           ELSE			!But if it doesn't,
            STYLE = "NEW"	!I shall create it.
          END IF		!Enough prevarication.
          OPEN(SSTASH,FILE = SFILE, STATUS = STYLE,	!Go for the file.
     &     ACCESS = "DIRECT", RECL = SRECLEN, FORM = "UNFORMATTED",	!I have plans.
     &     ERR = 666, IOSTAT = IOSTAT)					!Which may be thwarted.
          IF (EXIST) THEN	!If there is one...
            CALL READSCHAR(1)	!The first record is also a header.
            SLAST = I4UNPACK(C4)	!The number of records stored.
           ELSE			!Otherwise, start from scratch.
            SLAST = 0			!No saved records.
            CALL PSURGE(SCHAR)		!During preparation of the first batch of bits.
          END IF		!All should now be in readiness.
          GRASPPRIMEBAG = .TRUE.!So, feel confidence.
         RETURN			!And escape.
  666     WRITE (*,667) IOSTAT,SFILE	!But, something may have gone wrong.
  667     FORMAT ("Pox! Error code ",I0,	!A "hole" in the directory path?
     1     " when attempting to open file ",A)	!Read-only access allowed when I want "update"?
          GRASPPRIMEBAG = .FALSE.		!Whatever, it didn't work.
        END FUNCTION GRASPPRIMEBAG	!So much for that.

        SUBROUTINE READSCHAR(R)	!Get record R into SCHAR, which may already hold it.
         INTEGER R		!The record number desired.
          IF (R.EQ.SREC) RETURN	!Perhaps it is already to hand.
          SREC = R		!If not, move attention to it.
          READ (SSTASH,REC = SREC) C4,SCHAR	!And read the record.
        END SUBROUTINE READSCHAR!Thus, I have a buffer too.

        LOGICAL FUNCTION PSURGE(BIT8)	!Add another record to the stash.
C   Surges forward into the next batch of primes, to be stored via a bit array in the file.
C   Each record starts with a count of the number of primes that have gone before.
C   Except that for the first record, this is the record counter for the stash file.
C   Except that when starting the second record, one is also the number of primes before SORG.
         CHARACTER*1 BIT8(0:SCHARS - 1)	!Watch out! This may be SCHAR itself!
         INTEGER IST,LST	!The numbers spanned by the surge.
         INTEGER F		!A factor.
         INTEGER I		!Another factor and a stepper.
         INTEGER C		!Index for array BIT8.
         INTEGER NP		!Number of primes.
Carry forward the count of previous primes to start the following record..
   10     IF (SLAST.GT.0) THEN	!Is there a previous record?
            CALL READSCHAR(SLAST)	!Yes. Grab it. A good chance this is already in C4,SCHAR.
            NP = I4UNPACK(C4)		!Its count of the primes accumulated before it.
            DO I = 0,SCHARS - 1		!Find out how namy primes it fingered by scanning its bits.
              NP = NP + COUNT(IAND(ICHAR(SCHAR(I)),ICHAR(BITON)).NE.0)	!Whee! Eight at a go!
            END DO			!On to the next byte.
          END IF		!When creating a new record, its follower may not be sought in this run.
Concoct the next batch of bits. Contorted calculations avoid integer overflow.
   20     BIT8 = CHAR(255)		!All bits are aligned with numbers that might prove to be prime.
          IST = SORG + SLAST*(2*SBITS)	!Bit(0) of BIT8(0) corresponds to IST.
          LST = IST + 2*(SBITS - 1)	!Bit(last) to this number. Remember, only odd numbers have bits.
          IF (IST.LE.0) THEN		!Humm. I'd better check.
            WRITE (MSG,21) SLAST,IST,LST	!This works only with two's complement integers.
   21       FORMAT (/,"Integer overflow in the sieve of Eratosthenes!",	!Oh dear.
     1      /,"Advancing from surge ",I0," to span ",I0," to ",I0)	!These numbers will look odd.
            PSURGE = .FALSE.			!But it is better than no indication of what went wrong.
           RETURN			!Give in.
          END IF		!Enough worrying.
          F = 3			!The first possible factor. Zapping will start at F²
c         DO WHILE(F.LE.LST/F)	!If F² is past the end, so will be still larger F: enough.
          DO WHILE(F.LE.IST/F + (MOD(IST,F) + 2*(SBITS - 1))/F)	!"Synthetic division" avoiding overflow.
            I = (IST - 1)/F + 1		!I want the first multiple of F in IST:LST. F may be a factor of IST.
            IF (MOD(I,2).EQ.0) I = I + 1!If even, advance to the next odd multiple. Even numbers are omitted by design.
            IF (I.LT.F) I = F		!Less than F is superfluous: the position was zapped by earlier action.
c           I = (I*F - IST)/2			!Current bit positions are for IST, IST+2, IST+4, etc.
            I = ((I - IST/F)*F - MOD(IST,F))/2	!Avoids overflow when calculating the start value, I*F.
            DO I = I,SBITS - 1,F	!Zap every F'th bit along. This is the sieve of Eratosthenes.
              C = I/8				!Eight bits per character.
              BIT8(C) = CHAR(IAND(ICHAR(BIT8(C)),	!For F = 3 and 5, characters will be hit more than once.
     1                            ICHAR(BITOFF(MOD(I,8)))))	!Whack a bit. All the above just for this!
            END DO			!On to the next bit.
   22       F = NEXTPRIME(F)		!So much for F. Next, please.
          END DO		!Are we there yet?
Correct the count in the header, if this is an added record.
   30     IF (SLAST.GT.0) THEN	!So, was there a pre-existing header record?
            CALL READSCHAR(1)		!Yes. Get the header record into C4,SCHAR.
            CALL C4PACK(SLAST + 1)	!This is the new record count.
            WRITE (SSTASH,REC = 1) C4,SCHAR	!Write it all back.
            SCHAR = BIT8	!Ensure that SCHAR and SREC will be agreed.
          END IF		!So much for the header's count.
Cast the bits into the stash by writing record SLAST + 1..
   40     IF (SLAST.EQ.0) THEN	!If we're writing the first record,
            CALL C4PACK(1)		!Then this is the record count.
           ELSE			!Otherwise,
            CALL C4PACK(NP)		!Place the previous primes count.
          END IF		!All this to help PRIME(i).
          SLAST = SLAST + 1	!This is now the last stashed record.
          WRITE (SSTASH,REC = SLAST) C4,BIT8	!I/O directly from the work area?
          SREC = SLAST		!This is where BIT8 was written.
          PSURGE = .TRUE.	!That assumes BIT8 is not SCHAR for SLAST > 1.
        END FUNCTION PSURGE	!That was fun!

        RECURSIVE SUBROUTINE GETSREC(R)	!Make present the bit array belonging to record R.
         INTEGER R		!The record number..
         CHARACTER*1 BIT8(0:SCHARS - 1)	!A scratchpad. Others may be relying on SCHAR.
          IF (SLAST.LE.0) RETURN!DANGER! The first record is being initialised!
          DO WHILE(SLAST.LT.R)	!If we haven't reached so far,
            IF (.NOT.PSURGE(BIT8)) THEN	!Slog forwards one record's worth.
              WRITE (MSG,1) R			!Or maybe not.
    1         FORMAT ("Cannot prepare surge ",I0)	!Explain.
              STOP "No bits, no go."			!And quit.
            END IF			!And having prepared the next block of bits,
          END DO		!Check afresh.
          CALL READSCHAR(R)	!Read the desired record's bits.
        END SUBROUTINE GETSREC	!Done.

        INTEGER FUNCTION PRIME(N)	!P(1) = 2, P(2) = 3, etc.
C   Calculate P(n) ~ n.ln(n)
C                  ~ n{ln(n) + ln(ln(n)) - 1 + (ln(ln(n)) - 2)/ln(n) - [ln(ln(n))**2 - 6*log(log(n)) + 11]/[2*(ln(n))**2] + ....}
C   J.B.Rosser's 1938 Theorem: n[ln(n) + ln(ln(n)) - 1] < P(n) < n[ln(n) + ln(ln(n))]
C    or, with E = ln(n) + ln(ln(n)),           n[E - 1] < P(n) < n[E]
C   Experimentation shows that the undershoot of the first two terms involves many records worth of bits.
C   Including additional terms does much better, but can overshoot.
         INTEGER N		!The desired one.
         INTEGER R,NP		!Counts.
         INTEGER B,C		!Bit and character indices.
         DOUBLE PRECISION EST,LN,LLN	!Hope, if not actuality.
          IF (N.LE.0) STOP "Primes are counted positively!"	!Something must be wrong!
          IF (N.LE.1) THEN	!The start of the bit array being preempted.
            PRIME = 2			!So, no array access.
           ELSE			!Otherwise, the fun begins.
            LN = LOG(DFLOAT(N))	!Here we go.
            LLN = LOG(LN)	!A popular term.
            EST = N*(LN		!Estimate the value of the N'th prime.
     1               + LLN - 1		!Second term
     2               + (LLN - 2)/LN		!Third term.
     3               - (LLN**2 - 6*LLN + 11)/(2*LN**2))	!Fourth term.
            R = (EST - SORG)/(2*SBITS) + 1	!Thereby selecting a record to scan.
            IF (R.LE.0) R = 1			!And not making a mess with N < 6 or so.
    9       CALL GETSREC(R)	!Go for the record.
            IF (R.LE.1) THEN	!The first record starts with the record count.
              NP = 1		!And I know how many primes precede its start point
             ELSE		!While for all subsequent records,
              NP = I4UNPACK(C4)	!This counts the number of primes that precede record R's start number.
            END IF		!So now I'm ready to count onwards.
            IF (N.LE.NP) THEN	!Maybe not.
              R = R - 1			!The estimate took me too far ahead.
              GO TO 9			!Try again.
            END IF		!Could escalate to a binary search or even an interpolating search.
Commence scanning the bits.
            C = 0		!Start with the first character of SREC..
            B = -1		!Syncopation. The formula is known to always under-estimate.
   10       IF (NP.LT.N) THEN	!Are we there yet?
   11         B = B + 1			!No. Advance to the next bit.
              IF (B.GE.8) THEN		!Overflowed a character yet?
                B = 0			!Yes. Start afresh at the first bit.
                C = C + 1		!And advance one character.
                IF (C.GE.SCHARS) THEN	!Overflowed the record yet?
                  C = 0			!Yes. Start afresh at its first character.
                  R = R + 1		!And advance to the next record.
                  CALL GETSREC(R)	!Possibly, create it.
                END IF			!So much for records.
              END IF			!We're now ready to test bit B of character C of record R.
              IF (IAND(ICHAR(SCHAR(C)),ICHAR(BITON(B))).EQ.0) GO TO 11	!Not a prime. Search on.
              NP = NP + 1		!Count another prime.
              GO TO 10			!Pehaps this will be the one.
            END IF		!So much for the search.
            PRIME = SORG + (R - 1)*(2*SBITS) + (C*8 + B)*2	!The corresponding number.
            IF (PRIME.LE.0) WRITE (MSG,666) N,PRIME	!Or, possibly not.
  666       FORMAT ("Integer overflow! Prime(",I0,") gives ",I0,"!")	!Let us hope the caller notices.
          END IF		!So, all going well,
        END FUNCTION PRIME	!It is found.

        RECURSIVE INTEGER FUNCTION NEXTPRIME(N)	!Keep right on to the end of the road.
Can invoke GETSREC, which can invoke PSURGE, which ... invokes NEXTPRIME. Oh dear.
         INTEGER N	!Not necessarily itself a prime number.
         INTEGER NN	!A value to work with.
         INTEGER R	!A record number into the stash.
         INTEGER I,IST	!Number offsets.
         INTEGER C,B	!Character and bit index.
          IF (N.LE.1) THEN	!Suspicion prevails.
            NN = 2			!This is not represented in my bit array.
           ELSE			!Otherwise, the fun begins.
            NN = N + 1			!Advance, with a copy I can mess with.
            IF (MOD(NN,2).EQ.0) NN = NN + 1	!Thus, NN is now odd.
            IF (NN.LE.0) GO TO 666		!But perhaps not proper, due to overflow.
            R = (NN - SORG)/(2*SBITS)		!SORG is odd, so (NN - SORG) is even.
            CALL GETSREC(R + 1)		!The first record is numbered one, not zero.
            IST = SORG + R*(2*SBITS)	!The number for its first bit: even numbers are omitted..
            I = (NN - IST)/2		!Offset into the record. NN - IST is even.
            C = I/8			!Which character in SCHAR(0:SCHARS - 1)?
            B = MOD(I,8)		!Which bit in SCHAR(C)?
   10       IF (IAND(ICHAR(SCHAR(C)),ICHAR(BITON(B))).EQ.0) THEN	!On for a prime.
              NN = NN + 2	!Alas, it is off, so NN is not a prime. Perhaps this will be.
              B = B + 1		!Advance one bit. Each bit steps two.
              IF (B.GE.8) THEN	!Past the end of the character?
                B = 0			!Yes. Back to bit zero.
                C = C + 1		!And advance one chracter.
                IF (C.GE.SCHARS) THEN	!Past the end of the record?
                  IF (NN.LE.0) GO TO 666!Yes. If NN has overflowed, the end of the rope is reached.
                  C = 0			!Back to the start of a record.
                  R = R + 1		!Advance one record.
                  CALL GETSREC(R + 1)	!And read it. (Count is from 1, not 0).
                END IF		!So much for overflowing a record.
              END IF		!So much for overflowing a character.
              GO TO 10	!Try again.
            END IF		!So much for the bit array.
          END IF		!If there had been a scan.
          NEXTPRIME = NN	!The number for which the scan stopped.
          IF (NN.GT.0) RETURN	!All is well.
  666     WRITE (MSG,667) N,NN	!Or, maybe not. Careful: this won't appear if NEXTPRIME is invoked in a WRITE list.
  667     FORMAT ("Integer overflow! NextPrime(",I0,") gives ",I0,"!")	!The recipient could do a two's complement.
          NEXTPRIME = NN	!Prefer to return the bad value rather than fail to return anything.
        END FUNCTION NEXTPRIME	!No divisions, no sieving. Here, anyway

        INTEGER FUNCTION PREVIOUSPRIME(N)	!If N is good, this can't overflow.
         INTEGER N	!The number, not necessarily a prime.
         INTEGER NN	!A value to mess with.
         INTEGER R	!A record number.
         INTEGER I	!Offset.
         INTEGER C,B	!Character and bit fingers.
          IF (N.LE.3) THEN	!Suppress annoyances.
            NN = 2		!This is now called the first prime, not one.
           ELSE			!Otherwise, some work is to be done.
            NN = N - 1			!Step back one to ensure previousness.
            IF (MOD(NN,2).EQ.0) NN = NN - 1	!And here, oddness is a minimal requirement.
            R = (NN - SORG)/(2*SBITS)	!Finger the record containing the bit for NN.
            CALL GETSREC(R + 1)		!Record counting starts with one.
            I = (NN - (SORG + R*(2*SBITS)))/2	!Offset into that record.
            C = I/8			!Finger the character in SCHAR.
            B = MOD(I,8)		!And the bit within the character.
   10       IF (IAND(ICHAR(SCHAR(C)),ICHAR(BITON(B))).EQ.0) THEN	!On for a prime.
              NN = NN - 2	!Alas, it is off, so NN is not a prime. Perhaps this will be.
              B = B - 1		!Retreat one bit. Each bit steps two.
              IF (B.LT.0) THEN	!Past the start of the character?
                B = 7			!Yes. Back to the last bit.
                C = C - 1		!And retreat one chracter.
                IF (C.LT.0) THEN	!Past the start of the record?
                  C = SCHARS - 1	!Yes. Back to the end of a record.
                  R = R - 1		!Retreat one record.
                  CALL GETSREC(R + 1)	!And read it. (Count is from 1, not 0).
                END IF		!So much for overflowing a record.
              END IF		!So much for overflowing a character.
              GO TO 10		!Try again.
            END IF		!So much for the bit array.
          END IF		!Possibly, it was not needed.
          PREVIOUSPRIME = NN	!There.
        END FUNCTION PREVIOUSPRIME	!Doesn't overflow, either.

        LOGICAL FUNCTION ISPRIME(N)	!Could fool around explicity testing 2 and 3 and say 5,
         INTEGER N			!But that means also checking that N > 2, N > 3, and N > 5.
c        ISPRIME = N .EQ. NEXTPRIME(N - 1)	!This is so much easier, but involves scanning to reach the next prime.
         INTEGER R,IST,I,C,B		!Assistants for indexing the bit array.
          IF (N.LE.1) THEN	!First, preclude sillyness.
            ISPRIME = .FALSE.		!Not a prime.
          ELSE IF (N.EQ.2) THEN	!This is the only even number
            ISPRIME = .TRUE.		!That is a prime.
          ELSE IF (MOD(N,2).EQ.0) THEN	!Other even numbers
            ISPRIME = .FALSE.		!Are not prime numbers.
          ELSE			!Righto, now N is an odd number and there is a bit array for them.
            R = (N - SORG)/(2*SBITS)	!SORG is odd, so (N - SORG) is even.
            CALL GETSREC(R + 1)		!The first record is numbered one, not zero.
            IST = SORG + R*(2*SBITS)	!The number for its first bit: even numbers are omitted.
            I = (N - IST)/2		!Offset into the record. N - IST is even.
            C = I/8			!Which character in SCHAR(0:SCHARS - 1)?
            B = MOD(I,8)		!Which bit in SCHAR(C), indexing from zero?
            ISPRIME = IAND(ICHAR(SCHAR(C)),ICHAR(BITON(B))).GT.0	!The bit is on for a prime.
          END IF			!All that fuss to find a single bit.
        END FUNCTION ISPRIME		!But, no divisions up to SQRT(N) or the like.
      END MODULE PRIMEBAG	!Functions updating a disc file as a side effect...

      PROGRAM POKE
      USE PRIMEBAG
      INTEGER I,P,N,N1,N2	!Assorted assistants.
      INTEGER ORDER		!A collection of special values.
      PARAMETER (ORDER = 6)	!For one, two, and four byte integers.
      INTEGER EDGE(ORDER)	!Considered as two's complement and unsigned.
      PARAMETER (EDGE = (/31,54,3512,6542,105097565,203280221/))	!These primes are of interest.
      MSG = 6		!Standard output.

      IF (.NOT.GRASPPRIMEBAG(66)) STOP "Gan't grab my file!"	!Attempt in hope.

Case 1.
C     FORALL(I = 1:20) LIST(I) = PRIME(I) is rejected because function Prime(i) is rather impure.
   10 WRITE (MSG,11)
   11 FORMAT (19X,"First twenty primes: ", $)
      DO I = 1,20
        P = PRIME(I)
        WRITE (MSG,12) P
   12   FORMAT (I0,",",$)
      END DO

Case 2.
   20 WRITE (MSG,21)
   21 FORMAT (/,12X,"Primes between 100 and 150: ",$)
      P = 100
   22 P = NEXTPRIME(P)		!While (P:=NextPrime(P)) <= 150 do Print P;
      IF (P.LE.150) THEN	!But alas, no assignment within an expression.
        WRITE (MSG,23) P
   23   FORMAT (I0,",",$)
        GO TO 22
      END IF

Case 3.
   30 N1 = 7700	!Might as well parameterise this.
      N2 = 8000	!Rather than litter the source with explicit integers.
      N = 0
      P = N1
   31 P = NEXTPRIME(P)
      IF (P.LE.N2) THEN
        N = N + 1
        GO TO 31
      END IF
      WRITE (MSG,32) N1,N2,N
   32 FORMAT (/"Number of primes between ",I0," and ",I0,": ",I0)

Case 4.
   40 WRITE (MSG,41)
   41 FORMAT (/,"Tenfold steps...")
      N = 1
      DO I = 1,9	!This goes about as far as it can go.
        P = PRIME(N)
        WRITE (MSG,42) N,P
   42   FORMAT ("Prime(",I0,") = ",I0)
        N = N*10
      END DO

Cast forth some interesting values.
  100 WRITE (MSG,101)
  101 FORMAT (/,"Primes close to number sizes")
      DO N = 1,ORDER	!Step through the list.
        N1 = EDGE(N) - 1	!Syncopation for the special value.
        DO I = 1,2		!I want the prime on either side.
          N1 = N1 + 1			!So, there are two successive primes to finger.
          WRITE (MSG,102) N1			!Identify the index.
  102     FORMAT ("Prime(",I0,") = ",$)		!Piecemeal writing to the output,
          P = PRIME(N1)				!As this may fling forth a complaint.
          WRITE (MSG,103) P			!Show the value returned.
  103     FORMAT (I0,", ",$)			!Which may be unexpected.
        END DO			!On to the second.
        WRITE (MSG,*)		!End the line after the second result.
      END DO		!On to the next in the list.

      END	!Whee!
```

Although the structuralist talk up the merit of "structured" constructs in programming, there are often annoying obstacles. With a WHILE-loop one usually has to repeat the "next item" code to "prime" the loop, as in 
```Fortran
      P = NEXTPRIME(100)
      DO WHILE (P.LE.150)
        ...stuff...
        P = NEXTPRIME(P)
      END DO
```

While this is not a large imposition in this example, if Fortran were to allow assignment within expressions as in Algol, the tedium of code replication and its risks could be avoided. 
```algol68
 P:=100; WHILE (P:=NextPrime(P)) <= 150 DO stuff;
```
 If instead of NEXTPRIME the "next item" code was to read a record of a disc file, the repetition needed becomes tiresome. So, an IF-statement, and ... a GO TO...


### The Results

When creating the bit file, everything appears in a blink up to the millionth prime, then a pause until the ten millionth prime, then about a minute to attain the hundred millionth prime. The blinking lights show that there is not much disc I/O in progress (actually, a solid-state unit) while the cpu is running at full speed. There is a further pause from there until overflow is reached. For a subsequent run with the disc file already prepared, all is completed in a blink. Pausing the Great Internet Mersenne Prime crunch (sixfold) increases the speed and I/O rate by about a third. During the file expansion, the rate of I/O slowly decreases at a decreasing rate - this is due to each successive sieve surge requiring more primes to step with, but the primes themselves thin out and being larger, require fewer steps to traverse the span. The effort for each surge is related to the sum of the reciprocals of the primes that will sieved with and this forms an interesting sequence in its own right. For a given surge width, fewer and fewer hits are made within that width by the larger prime numbers. However, 4092 bytes (four are reserved for the count, and on this windows XP system disc space is allocated in blocks of 4k) gives 32736 bits which with odd numbers only, spans 65472. With 32-bit two's complement, sqrt(2,147,483,647) = 46340·95 so even the largest stepper will land at least once within every span, except that only odd multiples are involved so already the hit rate has drifted below one per span and with extension to still larger numbers, the hit rate will fall further. Thus, if instead each pass were to be for the full width of the bit array in the disc file, the I/O system would suffer a thrashing during the multiple passes unless the entire file could be fitted into random-access memory. But such a scheme would not have the "extensible" aspect.

Output:

```txt

                   First twenty primes: 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,
            Primes between 100 and 150: 101,103,107,109,113,127,131,137,139,149,
Number of primes between 7700 and 8000: 30

Tenfold steps...
Prime(1) = 2
Prime(10) = 29
Prime(100) = 541
Prime(1000) = 7919
Prime(10000) = 104729
Prime(100000) = 1299709
Prime(1000000) = 15485863
Prime(10000000) = 179424673
Prime(100000000) = 2038074743

Primes close to number sizes
Prime(31) = 127, Prime(32) = 131,
Prime(54) = 251, Prime(55) = 257,
Prime(3512) = 32749, Prime(3513) = 32771,
Prime(6542) = 65521, Prime(6543) = 65537,
Prime(105097565) = 2147483647, Prime(105097566) = Integer overflow! Prime(105097566) gives -2147483637!
-2147483637,
Prime(203280221) =
Integer overflow in the sieve of Eratosthenes!
Advancing from surge 32801 to span -2147420221 to -2147354751
Cannot prepare surge 65601
No bits, no go.

```

The disc file holding all primes up to the thirty-two bit limit occupies 134,352,896 bytes, or 128MB. Nothing much, these days. Activating 7-zip out of curiosity resulted in compressing the file by a factor of two. As the primes thin out there will be more and more characters with only one bit on (if not none) rather than a fuller selection. However, the existence of prime pairs shows that the bits will never be all lonely forever. For this run, the last record is number 32,801 and 105,097,477 (hex 643A905) primes have gone before.

```txt

 First value  Bit array...
           3: 11101101 10100110 01011010 01001100 10110010 10010001 01101101 00000010
              10011000 01100100 10100100 11000011 01100000 10000010 11010011 00001001
              00100110 01011000 01000000 10110100 00001001 00001101 00100010 01001010
              01000101 00010000 11000011 00101001 00010110 10000010 00101000 10100100 ...

  2147481603: 00000000 00000100 00000000 00000000 00010000 00000000 00000000 00000000
              00000000 00000000 00000000 00000001 01000000 10000000 10000000 00000000
              00100000 00001000 01001100 10000000 00000001 00000100 00000010 00000000
              00000100 00000000 01000000 00000000 00000010 00000001 00001100 00000000 ...

```

Would anyone prefer to see that bit array in the little-endian order within bytes?

The thinning out rather suggests an alternative encoding such as by counting the number of "off" bits until the next "on" bit, but this would produce a variable-length packing so that it would no longer be easy to find the bit associated with a given number by something so simple as <code>R = (NN - SORG)/(2*SBITS)</code> as in NEXTPRIME. A more accomplished data compression system might instead offer a reference to a library containing a table of prime numbers, or even store the code for a programme that will generate the data. File Pbag.for is 23,008 bytes long, and of course contains irrelevant commentary and flabby phrases such as "PARAMETER", "INTEGER", "GRASPPRIMEBAG", ''etc''. As is the modern style, the code file is much larger at 548,923 bytes (containing kilobyte sequences of hex CC and of 00), but both are much smaller than the 134,352,896 bytes of file PrimeSieve.bit.


## FreeBASIC

This program uses the Sieve Of Eratosthenes which is not very efficient for large primes but is quick enough for present purposes.

The size of the sieve array (of type Boolean) is calculated as 20 times the number of primes required which is big enough to compute 
up to 50 million primes (a sieve size of 1 billion bytes) which takes under 50 seconds on my i3 @ 2.13 GHz. I've limited the
procedure to this but it should certainly be possible to use a much higher figure without running out of memory. 

It would also be possible to use a more efficient algorithm to compute the optimal sieve size for smaller numbers of primes but this will suffice for now. 


```freebasic
' FB 1.05.0

Enum SieveLimitType
  number 
  between
  countBetween
End Enum

Sub printPrimes(low As Integer, high As Integer, slt As SieveLimitType)
  If high < low OrElse low < 1 Then Return               ' too small
  If slt <> number AndAlso slt <> between AndAlso slt <> countBetween Then Return
  If slt <> number AndAlso (low < 2 OrElse high < 2) Then Return  
  If slt <> number AndAlso high > 1000000000 Then Return ' too big 
  If slt = number  AndAlso high > 50000000 Then Return   ' too big
  Dim As Integer n
  If slt = number Then
    n = 20 * high '' big enough to accomodate 50 million primes to which this procedure is limited
  Else
    n = high
  End If  
  Dim a(2 To n) As Boolean '' only uses 1 byte per element
  For i As Integer = 2 To n : a(i) = True : Next '' set all elements to True to start with
  Dim As Integer p = 2, q
  ' mark non-prime numbers by setting the corresponding array element to False

  Do
    For j As Integer = p * p To n Step p
      a(j) = False
    Next j
    ' look for next True element in array after 'p'
    q = 0
    For j As Integer = p + 1 To Sqr(n)
      If a(j) Then
        q = j
        Exit For
      End If
    Next j    
    If q = 0 Then Exit Do
    p = q
  Loop

  Select Case As Const slt
     Case number
       Dim count As Integer = 0
       For i As Integer = 2 To n
         If a(i) Then
           count += 1
           If count >= low AndAlso count <= high Then
             Print i; " ";
           End If
           If count = high Then Exit Select
         End If
       Next

    Case between
       For i As Integer = low To high
         If a(i) Then
           Print i; " ";
         End if
       Next

    Case countBetween
       Dim count As Integer = 0
       For i As Integer = low To high
         If a(i) Then count += 1
       Next
       Print count;

  End Select
  Print
End Sub

Print "The first 20 primes are :"
Print
printPrimes(1, 20, number)
Print
Print "The primes between 100 and 150 are :"
Print
printPrimes(100, 150, between)
Print
Print "The number of primes between 7700 and 8000 is :";
printPrimes(7700, 8000, countBetween)
Print
Print "The 10000th prime is :";
Dim t As Double = timer 
printPrimes(10000, 10000, number)
Print "Computed in "; CInt((timer - t) * 1000 + 0.5); " ms"
Print
Print "The 1000000th prime is :"; 
t = timer
printPrimes(1000000, 1000000, number)
Print "Computed in ";CInt((timer - t) * 1000 + 0.5); " ms"
Print
Print "The 50000000th prime is :"; 
t = timer
printPrimes(50000000, 50000000, number)
Print "Computed in ";CInt((timer - t) * 1000 + 0.5); " ms"
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

The first 20 primes are :

 2  3  5  7  11  13  17  19  23  29  31  37  41  43  47  53  59  61  67  71

The primes between 100 and 150 are :

 101  103  107  109  113  127  131  137  139  149

The number of primes between 7700 and 8000 is : 30

The 10000th prime is : 104729
Computed in  8 ms

The 1000000th prime is : 15485863
Computed in  775 ms

The 50000000th prime is : 982451653
Computed in  46703 ms

```



## Go

An implementation of "The Genuine Sieve of Eratosthenese" by Melissa E. O'Niell.  This is the paper cited above in the "Faster Alternative Version" of D.  The Go example here though strips away optimizations such as a wheel to show the central idea of storing prime multiples in a queue data structure.

```go
package main

import (
    "container/heap"
    "fmt"
)

func main() {
    p := newP()
    fmt.Print("First twenty: ")
    for i := 0; i < 20; i++ {
        fmt.Print(p(), " ")
    }
    fmt.Print("\nBetween 100 and 150: ")
    n := p()
    for n <= 100 {
        n = p()
    }
    for ; n < 150; n = p() {
        fmt.Print(n, " ")
    }
    for n <= 7700 {
        n = p()
    }
    c := 0
    for ; n < 8000; n = p() {
        c++
    }
    fmt.Println("\nNumber beween 7,700 and 8,000:", c)
    p = newP()
    for i := 1; i < 10000; i++ {
        p()
    }
    fmt.Println("10,000th prime:", p())
}

func newP() func() int {
    n := 1
    var pq pQueue
    top := &pMult{2, 4, 0}
    return func() int {
        for {
            n++
            if n < top.pMult { // n is a new prime
                heap.Push(&pq, &pMult{prime: n, pMult: n * n})
                top = pq[0]
                return n
            }
            // n was next on the queue, it's a composite
            for top.pMult == n {
                top.pMult += top.prime
                heap.Fix(&pq, 0)
                top = pq[0]
            }
        }
    }
}

type pMult struct {
    prime int
    pMult int
    index int
}

type pQueue []*pMult

func (q pQueue) Len() int           { return len(q) }
func (q pQueue) Less(i, j int) bool { return q[i].pMult < q[j].pMult }
func (q pQueue) Swap(i, j int) {
    q[i], q[j] = q[j], q[i]
    q[i].index = i
    q[j].index = j
}
func (p *pQueue) Push(x interface{}) {
    q := *p
    e := x.(*pMult)
    e.index = len(q)
    *p = append(q, e)
}
func (p *pQueue) Pop() interface{} {
    q := *p
    last := len(q) - 1
    e := q[last]
    *p = q[:last]
    return e
}
```

{{out}}

```txt

First twenty: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 
Between 100 and 150: 101 103 107 109 113 127 131 137 139 149 
Number beween 7,700 and 8,000: 30
10,000th prime: 104729

```


An alternative showing how to use a good and ''very'' fast open source Sieve of Atkin implementation
via [https://godoc.org/github.com/jbarham/primegen.go github.com/jbarham/primegen.go].
Due to how Go's imports work, the bellow can be given directly to "<code>go run</code>" or "<code>go build</code>" and the latest version of the primegen package will be fetched and built if it's not already present on the system.
(This example may not be exactly within the scope of this task, but it's a trivial to use and extremely fast prime generator probably worth considering whenever primes are needed in Go.)

```go
package main

import (
	"fmt"
	"github.com/jbarham/primegen.go"
)

func main() {
	p := primegen.New()

	fmt.Print("First twenty: ")
	for i := 0; i < 20; i++ {
		fmt.Print(p.Next(), " ")
	}
	fmt.Print("\nBetween 100 and 150: ")
	p.SkipTo(100)
	for n := p.Next(); n < 150; n = p.Next() {
		fmt.Print(n, " ")
	}
	p.SkipTo(7700)
	fmt.Println("\nNumber beween 7,700 and 8,000:", p.Count(8000))
	p.Reset()
	for i := 1; i < 1e4; i++ {
		p.Next()
	}
	fmt.Println("10,000th prime:", p.Next())
}
```



## Haskell


{{libheader|primes}}
{{Works with|GHC|7.8.3}}
{{Works with|primes|0.2.1.0}}

This program uses the [http://hackage.haskell.org/package/primes primes] package, which uses a lazy wheel sieve to produce an infinite list of primes.


```haskell
#!/usr/bin/env runghc

import Data.List
import Data.Numbers.Primes
import System.IO

firstNPrimes :: Integer -> [Integer]
firstNPrimes n = genericTake n primes

primesBetweenInclusive :: Integer -> Integer -> [Integer]
primesBetweenInclusive lo hi =
  dropWhile (< lo) $ takeWhile (<= hi) primes

nthPrime :: Integer -> Integer
nthPrime n = genericIndex primes (n - 1) -- beware 0-based indexing

main = do
  hSetBuffering stdout NoBuffering
  putStr "First 20 primes: "
  print $ firstNPrimes 20
  putStr "Primes between 100 and 150: "
  print $ primesBetweenInclusive 100 150
  putStr "Number of primes between 7700 and 8000: "
  print $ genericLength $ primesBetweenInclusive 7700 8000
  putStr "The 10000th prime: "
  print $ nthPrime 10000
```


{{out}}

```txt

First 20 primes: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
Primes between 100 and 150: [101,103,107,109,113,127,131,137,139,149]
Number of primes between 7700 and 8000: 30
The 10000th prime: 104729

```



### List based

Using list based unbounded sieve from [[Sieve_of_Eratosthenes#With_Wheel|here]] (runs instantly):


```haskell
 λ> take 20 primesW
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]

 λ> takeWhile (< 150) . dropWhile (< 100) $ primesW
[101,103,107,109,113,127,131,137,139,149]

 λ> length . takeWhile (< 8000) . dropWhile (< 7700) $ primesW
30

 λ> (!! (10000-1)) primesW
104729
```


==Icon and {{header|Unicon}}==

Only works in Unicon (use of the Heap class).  Brute force.

The expression:

```unicon
![2,3,5,7] | (nc := 11) | (nc +:= |wheel2345)
```

is an open-ended sequence generating potential primes.


```unicon
import Collections      # to get the Heap class for use as a Priority Queue
record filter(composite, prime)  # next composite involving this prime

procedure main()
    every writes((primes()\20)||" " | "\n")
    every p := primes() do if 100 < p < 150 then writes(p," ") else if p >= 150 then break write()
    every (n := 0, p := primes()) do if 7700 < p < 8000 then n +:= 1 else if p >= 8000 then break write(n)
    every (i := 1, p := primes()) do if (i+:=1) >= 10000 then break write(p)
end

procedure primes()
    local wheel2357, nc
    wheel2357 := [2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4, 2,
                  6, 4, 6, 8, 4, 2, 4, 2, 4, 8, 6, 4, 6, 2, 4, 6,
                  2, 6, 6, 4, 2, 4, 6, 2, 6, 4, 2, 4, 2, 10, 2, 10]
    suspend sieve(Heap(,getCompositeField), ![2,3,5.7] | (nc := 11) | (nc +:= |!wheel2357))
end

procedure sieve(pQueue, candidate)
    local nc
    if 0 = pQueue.size() then {   # 2 is prime
        pQueue.add(filter(candidate*candidate, candidate))
        return candidate
        }
    while candidate > (nc := pQueue.get()).composite do {
        nc.composite +:= nc.prime
        pQueue.add(nc)
        }
    pQueue.add(filter(nc.composite+nc.prime, nc.prime))
    if candidate < nc.composite then {   # new prime found!
        pQueue.add(filter(candidate*candidate, candidate))
        return candidate
        }

end

# Provide a function for comparing filters in the priority queue...
procedure getCompositeField(x); return x.composite; end
```


{{out}}

```txt

->ePrimes
2 3 5.7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 
101 103 107 109 113 127 131 137 139 149 
30
104729
->

```



## J


Using the p: builtin, http://www.jsoftware.com/help/dictionary/dpco.htm reports "Currently, arguments larger than 2^31 are tested to be prime according to a probabilistic algorithm (Miller-Rabin)".


```J
   p:i.20
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
   (#~ >:&100)i.&.(p:inv) 150
101 103 107 109 113 127 131 137 139 149
   #(#~ >:&7700)i.&.(p:inv) 8000
30
   p:10000-1
104729
```


Note: p: gives the nth prime, where 0 is first, 1 is second, 2 (cardinal) is third (ordinal) and so on...

Note: 4&p: gives the next prime


```J
   4 p: 104729
104743
```




## JavaScript


'''primeGenerator(num, showPrimes)'''
This function takes two arguments:

'''num''' is either an integer as a limit, or an array of two integers to present a range;

'''showPrimes''' is a boolean to indicate whether the result should be a list (if ''true'') or a single number (if ''false'').

Sounds a bit weird, but I hope it will be intelligible by the testing examples below. First the code:


```JavaScript
function primeGenerator(num, showPrimes) {
  var i,
      arr = [];

  function isPrime(num) {
    // try primes <= 16
    if (num <= 16) return (
      num == 2 || num == 3 || num == 5 || num == 7 || num == 11 || num == 13
    );
    // cull multiples of 2, 3, 5 or 7
    if (num % 2 == 0 || num % 3 == 0 || num % 5 == 0 || num % 7 == 0)
      return false;
    // cull square numbers ending in 1, 3, 7 or 9
    for (var i = 10; i * i <= num; i += 10) {
      if (num % (i + 1) == 0) return false;
      if (num % (i + 3) == 0) return false;
      if (num % (i + 7) == 0) return false;
      if (num % (i + 9) == 0) return false;
    }
    return true;
  }

  if (typeof num == "number") {
    for (i = 0; arr.length < num; i++) if (isPrime(i)) arr.push(i);
    // first x primes
    if (showPrimes) return arr;
    // xth prime
    else return arr.pop();
  }

  if (Array.isArray(num)) {
    for (i = num[0]; i <= num[1]; i++) if (isPrime(i)) arr.push(i);
    // primes between x .. y
    if (showPrimes) return arr;
    // number of primes between x .. y
    else return arr.length;
  }
  // throw a default error if nothing returned yet
  // (surrogate for a quite long and detailed try-catch-block anywhere before)
  throw("Invalid arguments for primeGenerator()");
}
```


'''Test'''
<lang>// first 20 primes
console.log(primeGenerator(20, true));

// primes between 100 and 150
console.log(primeGenerator([100, 150], true));

// numbers of primes between 7700 and 8000
console.log(primeGenerator([7700, 8000], false));

// the 10,000th prime
console.log(primeGenerator(10000, false));
```


'''Output'''
<lang>Array [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 51, 59, 61, 67, 71 ]

Array [ 101, 103, 107, 109, 113, 127, 131, 137, 139, 149 ]

30

104729
```



## jq

{{works with|jq|1.4}}

Recent versions of jq include extensive support for unbounded stream
generators, but in this section, we present a solution to the tasks
that should work with any version of jq from 1.4 onwards.  That is,
instead of using an unbounded generator of a stream of primes, the core of the
approach adopted here is a function, "extend_primes", which can be
applied recursively to generate arbitrarily many, or indefinitely
many, primes, as illustrated by the function named "primes" below.

'''Preliminaries:'''

```jq
# Recent versions of jq include the following definition:
# until/2 loops until cond is satisfied,
# and emits the value satisfying the condition:
def until(cond; next):
  def _until:
    if cond then . else (next|_until) end;
  _until;

def count(cond): reduce .[] as $x (0; if $x|cond then .+1 else . end);
```


'''Prime numbers:'''

```jq
# Is the input integer a prime?
# "previous" must be the array of sorted primes greater than 1 up to (.|sqrt)
def is_prime(previous):
  . as $in
  | (previous|length) as $plength
  | [false, 0]   # state: [found, ix]
  | until( .[0] or .[1] >= $plength;
           [ ($in % previous[.[1]]) == 0, .[1] + 1] )
  | .[0] | not ;

# extend_primes expects its input to be an array consisting of
# previously found primes, in order, and extends that array:
def extend_primes:
  if . == null or length == 0 then [2]
  else . as $previous
  | if . == [2] then [2,3]
    else . + [(2 + .[length-1]) | until( is_prime($previous) ; . + 2)]
    end
  end;

# If . is an integer > 0 then produce an array of . primes;
# otherwise emit an unbounded stream of primes:
def primes:
  . as $n
  | if type == "number" and $n > 0 then
      null | until( length == $n; extend_primes )
    else [2] | recurse(extend_primes) | .[length - 1]
    end;

# Primes up to and possibly including n:
def primes_upto(n):
  until( .[length-1] > n; extend_primes )
  | if .[length-1] > n then .[0:length-1] else . end;
```

'''The tasks:'''
The tasks are completed separately here to highlight the fact that by using "extend_primes", each task can be readily completed without generating unnecessarily many primes.

```jq
"First 20 primes:", (20 | primes), "",

"Primes between 100 and 150:",
   (primes_upto(150) | map(select( 100 < .))), "",

"The 10,000th prime is \( 10000 | primes | .[length - 1] )", "",

(( primes_upto(8000) | count( . > 7700) | length) as $length
    | "There are \($length) primes twixt 7700 and 8000.")
```

{{out}}

```sh
$ jq -r -c -n -f Extensible_prime_generator.jq
First 20 primes:
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]

Primes between 100 and 150:
[101,103,107,109,113,127,131,137,139,149]

The 10,000th prime is 104729

There are 30 primes twixt 7700 and 8000.
```



## Julia

Julia's Primes package, included in the distribution, is exact up to 2^64 = 18446744073709551616. 
After that, Primes can use the BigInt data type, and then may use a probabalistic prime 
determination algorithm for such integers of arbitrarily large size. The probabilistic formula is tune-able, 
and by default determines primes with Knuth's recommended level for cryptography of an error less than 
(0.25)^25 = 8.881784197001252e-16, or 1 in 1125899906842624.

```julia
using Primes

sum = 2
currentprime = 2
for i in 2:100000
    currentprime = nextprime(currentprime + 1)
    sum += currentprime
end
println("The sum of the first 100,000 primes is $sum")

curprime = 1
arr = zeros(Int, 20)
for i in 1:20
    curprime = nextprime(curprime + 1)
    arr[i] = curprime
end
println("The first 20 primes are ", arr)

println("the primes between 100 and 150 are ", primes(100,150))
println("The number of primes between 7,700 and 8,000 is ", length(primes(7700, 8000)))
println("The 10,000th prime is ", prime(10000))
```

{{output}}

```txt

The sum of the first 100,000 primes is 62260698721
The first 20 primes are [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]
the primes between 100 and 150 are [101, 103, 107, 109, 113, 127, 131, 137, 139, 149]
The number of primes between 7,700 and 8,000 is 30
The 10,000th prime is 104729

```


'''Alternative True Generator'''

The above code just uses the "Primes" package as a set of tools to solve the tasks.  The following code creates a very simple generator using `isprime` inside a iterator and then uses that to solve the tasks:

```julia
using Primes: isprime

PrimesGen() = Iterators.filter(isprime, Iterators.countfrom(Int64(2)))

print("Sum of first 100,000 primes:  ")
println(Iterators.sum(Iterators.take(PrimesGen(), 100000)))
print("First 20 primes:  ( ")
foreach((p->print(p," ")), Iterators.take(PrimesGen(), 20))
println(")")
print("Primes between 100 and 150:  ( ")
for p in Iterators.filter((p->p>=100), PrimesGen()) p > 150 && break; print(p, " ") end
println(")")
let cnt = 0
    for p in PrimesGen()
        p > 8000 && break; if p > 7700 cnt += 1 end
    end; println("Number of primes between 7700 and 8000:  ", cnt)
end
println("The 10,000th prime:  ", Iterators.first(Iterators.drop(PrimesGen(), 9999)))
println()
```


This outputs:
{{output}}

```txt
Sum of first 100,000 primes:  62260698721
First 20 primes:  ( 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 )
Primes between 100 and 150:  ( 101 103 107 109 113 127 131 137 139 149 )
Number of primes between 7700 and 8000:  30
The 10,000th prime:  104729
```


To show it's speed, lets use it to solve the Euler Problem 10 of calculating the sum of the primes to two million as follows:

```julia
using Printf: @printf
@time let sm = 0
          for p in Iterators.filter(isprime, Iterators.countfrom(UInt64(2)))
              p > 2000000 && break
              sm += p
          end; @printf("%d\n", sm) end
```


which outputs:
{{output}}

```txt
142913828922
  0.783845 seconds (328.02 k allocations: 5.042 MiB)
```


As shown, this is of adequate speed for this smallish range; however it wouldn't be adequate to do the same for a range of two billion.

This is an "infinite" iterator whose range is limited by the size of `Int64`, but as it will take about 300 thousand years to get there, it isn't much of a concern.

'''An "infinite" iterator based on a bit-packed page-segmented Sieve of Eratosthenes'''

The above code is more than adequate to solve the trivial tasks as required here, but is really too slow for "industrial strength" tasks for ranges of billions.  The following code uses the Page Segmented Algorithm from [[Sieve_of_Eratosthenes#Julia]] to solve the task:

```julia
using Printf: @printf

print("Sum of first 100,000 primes:  ")
println(Iterators.sum(Iterators.take(PrimesPaged(), 100000)))
print("First 20 primes:  ( ")
foreach((p->@printf("%d ", p)), Iterators.take(PrimesPaged(), 20))
println(")")
print("Primes between 100 and 150:  ( ")
for p in Iterators.filter((p->p>=100), PrimesPaged()) p > 150 && break; @printf("%d ", p)) end
println(")")
let cnt = 0
    for p in PrimesPaged()
        p > 8000 && break; if p > 7700 cnt += 1 end
    end; println("Number of primes between 7700 and 8000:  ", cnt)
end
@printf("The 10,000th prime:  %d\n", Iterators.first(Iterators.drop(PrimesPaged(), 9999)))
```


to produce the same output much faster.

To show how much faster it is, doing the same Euler Problem 10 as follows:

```julia
using Printf: @printf
@time let sm = 0
          for p in PrimesPaged()
              p > 2000000 && break
              sm += p
          end; @printf("%d\n", sm) end
```


produces:
{{output}}

```txt
142913828922
  0.016826 seconds (60 allocations: 23.891 KiB)
```


showing it is as over 40 times faster, but it will definitely get even relatively faster with increasing range as this range is relatively trivial for page segmentation and there are more optimizations one can make.

This generator is also "infinite" to the `UInt64` range, but now will "only" take hundreds of years to get there.


## Kotlin

Although we could use the java.math.BigInteger type to generate arbitrarily large primes, there is no need to do so here as the primes to be generated are well within the limits of the 4-byte Int type.
((workwith|Kotlin|version 1.3}}

```scala
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

fun generatePrimes() = sequence {
        yield(2)
        var p = 3
        while (p <= Int.MAX_VALUE) {
           if (isPrime(p)) yield(p)
           p += 2
        }
    }

fun main(args: Array<String>) {
    val primes = generatePrimes().take(10000) // generate first 10,000 primes
    println("First 20 primes : ${primes.take(20).toList()}")
    println("Primes between 100 and 150 : ${primes.filter { it in 100..150 }.toList()}")
    println("Number of primes between 7700 and 8000 = ${primes.filter { it in 7700..8000 }.count()}")
    println("10,000th prime = ${primes.last()}")
}
```


{{out}}

```txt

First 20 primes : [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]
Primes between 100 and 150 : [101, 103, 107, 109, 113, 127, 131, 137, 139, 149]
Number of primes between 7700 and 8000 = 30
10,000th prime = 104729

```


'''Alternate with better performance'''

Of all the current submissions on the page, the above code has got to be the worst.  While it is adequate to solve the trivial tasks required by the page, it is a Trial Division Sieve and has O(n^3/2)) asymptotic performance over `n`, the range, where even a purely functional incremental sieve has O(n log n) performance.  There is no need to consider BigInteger at all for a prime generator starting from the lowest to the Long number range, as it will never get there in less than 100's of years.  Even though the above code implements a rudimentary wheel factorization, it will still be extremely slow as that only provides constant factor gains.

The following odds-only incremental Sieve of Eratosthenes generator has O(n log (log n)) performance due to using a (mutable) HashMap:

```scala
fun primesHM(): Sequence<Int> = sequence {
    yield(2)
    fun oddprms(): Sequence<Int> = sequence {
        yield(3); yield(5) // need at least 2 for initialization
        val hm = HashMap<Int,Int>()
        hm.put(9, 6)
        val bps = oddprms().iterator(); bps.next(); bps.next() // skip past 5
        yieldAll(generateSequence(SieveState(7, 5, 25)) {
            ss ->
                var n = ss.n; var q = ss.q
                n += 2
                while ( n >= q || hm.containsKey(n)) {
                    if (n >= q) {
                        val inc = ss.bp shl 1
                        hm.put(n + inc, inc)
                        val bp = bps.next(); ss.bp = bp; q = bp * bp
                    }
                    else {
                        val inc = hm.remove(n)!!
                        var next = n + inc
                        while (hm.containsKey(next)) {
                            next += inc
                        }
                        hm.put(next, inc)
                    }
                    n += 2
                }
                ss.n = n; ss.q = q
                ss
        }.map { it.n })
    }
    yieldAll(oddprms())
}</Lang>

it is faster than the first example even though not using wheel factorization (other than odds-only) and rapidly pulls far ahead of it with increasing range such that it is usable to a range of 100 million in the order of 10 seconds.

'''Alternate with "industrial strength" performance'''

For ranges of a billion and more, one needs a sieve based on Page Segmented mutable arrays.  The last code on the Sieve of Eratosthenes Task page at:  [[Sieve_of_Eratosthenes#Unbounded_Versions_2]] can do the job.  When called with the following same `main` as the first example with `primesPaged()` substituted for `generatePrimes()` or `primesHM()`, it produces the same output.

It can count the primes to one billion in about 15 seconds on a slow tablet CPU (Intel x5-Z8350 at 1.92 Gigahertz) with the following code:

```kotlin
primesPaged().takeWhile { it <= 1_000_000_000 }.count()
```


Further speed-ups can be achieved of about a factor of four with maximum wheel factorization and by multi-threading by the factor of the effective number of cores used, but there is little point when most of the execution time as a generator is spend iterating over the results.

In order to take advantage of those optimizations, one needs to write functions that work directly with the provided sequence of culled bit pages such as the provided `countPrimesTo` function does, which counts the primes without the iteration about three times as fast.


## Lingo

The following script implements a Sieve of Eratosthenes that is automatically extended when a method call needs a higher upper limit.


```Lingo
-- parent script "sieve"
property _sieve

----------------------------------------
-- @constructor
----------------------------------------
on new (me)
    me._sieve = []
    me._primeSieve(100) -- arbitrary initial size of sieve
    return me
end

----------------------------------------
-- Returns sorted list of first n primes p with p >= a (default: a=1)
----------------------------------------
on getNPrimes (me, n, a)
    if voidP(a) then a = 1
    i = a
    res = []
    repeat while TRUE
        if i>me._sieve.count then me._primeSieve(2*i)
        if me._sieve[i] then res.add(i)
        if res.count=n then return res
        i = i +1
    end repeat
end

----------------------------------------
-- Returns sorted list of primes p with a <= p <= b
----------------------------------------
on getPrimesInRange (me, a, b)
    if me._sieve.count<b then me._primeSieve(b)
    primes = []
    repeat with i = a to b
        if me._sieve[i] then primes.add(i)
    end repeat
    return primes
end

----------------------------------------
-- Returns nth prime
----------------------------------------
on getNthPrime (me, n)
    if me._sieve.count<2*n then me._primeSieve(2*n)
    i = 0
    found = 0
    repeat while TRUE
        i = i +1
        if i>me._sieve.count then me._primeSieve(2*i)
        if me._sieve[i] then found=found+1
        if found=n then return i
    end repeat
end
            
----------------------------------------
-- Sieve of Eratosthenes
----------------------------------------
on _primeSieve (me, limit)
    if me._sieve.count>=limit then
        return
    else if me._sieve.count>0 then
        return me._complementSieve(limit)
    end if
    me._sieve = [0]
    repeat with i = 2 to limit
        me._sieve[i] = 1
    end repeat
    c = sqrt(limit)
    repeat with i = 2 to c
        if (me._sieve[i]=0) then next repeat
        j = i*i
        repeat while (j<=limit)
            me._sieve[j] = 0
            j = j + i
        end repeat
    end repeat
end

----------------------------------------
-- Expands existing sieve to new limit
----------------------------------------
on _complementSieve (me, n)
    n1 = me._sieve.count
    repeat with i = n1+1 to n
        me._sieve[i] = 1
    end repeat
    c1 = sqrt(n1)
    repeat with i = 2 to c1
        if (me._sieve[i]=0) then next repeat
        j = n1 - (n1 mod i)
        repeat while (j<=n)
            me._sieve[j] = 0
            j = j + i
        end repeat
    end repeat
    c = sqrt(n)
    repeat with i = c1+1 to c
        if (me._sieve[i]=0) then next repeat
        j = i*i
        repeat while (j<=n)
            me._sieve[j] = 0
            j = j + i
        end repeat
    end repeat
end
```



```Lingo
sieve = script("sieve").new()
put "First twenty primes: " & sieve.getNPrimes(20)
put "Primes between 100 and 150: "& sieve.getPrimesInRange(100, 150)
put "Number of primes between 7,700 and 8,000: " & sieve.getPrimesInRange(7700, 8000).count
put "The 10,000th prime: " & sieve.getNthPrime(10000)
```


{{out}}

```txt

-- "First twenty primes: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]"
-- "Primes between 100 and 150: [101, 103, 107, 109, 113, 127, 131, 137, 139, 149]"
-- "Number of primes between 7,700 and 8,000: 30"
-- "The 10,000th prime: 104729"

```



## M2000 Interpreter

The fancy way, using two lambda which shares closures (they are pointers, so lanbdas get copies of pointers, which is by value passing, and by reference too). Also I use my loved GOTO. Change 200th to 10000th and wait...
A 5@ is a literal for Decimals (Variables are Variant types, but when they get a value they hold that value type. Array items or in other containers they, get whatever we want, anytime)

Version 2
Now we can make a bigger computation using Fast! mode which eliminate Gui/console refresh to gain speed. We can make a refresh each 50primes, so we have a tiny delay on refreshing if we move a window above M2000 console.

Also I change IsPrime to not add to Inventory Known1, because we want this inventory to have all primes without any missing until the last one.

I provide another IsPrime2 which use PrimeNth to add to Known1.

Inventories are reference type. Lambda functions are value type, but closures which are reference type copied the reference by value, so we get then by reference. Inventories start now with 3 known primes, and PrimeNth works for odd numbers only (see x+=2 before the loop statement)

Loop statement check a flag in a block of code ({ }), so when the block ends restart again (resetting the loop flag)


```M2000 Interpreter

Module CheckPrimes {
      \\ Inventories are lists, Known and Known1 are pointers to Inventories
      Inventory Known=1:=2@,2:=3@,3:=5@
      Inventory Known1=2@, 3@, 5@
      \\ In a lambda all closures are copies
      \\ but Known and Know1 are copies of pointers
      \\ so are closures like by reference
      PrimeNth=lambda  Known, Known1  (n as long) -> {
            if n<1 then Error "Only >=1"
            if exist(known, n) then =eval(known) : exit
            if n>5 then {
                 i=len(known1) 
                 x=eval(known1, i-1)+2
            } else  x=5 : i=2
            { 
                  if i=n then  =known(n) : exit
                  ok=false
                  if frac(x) then 1000
                  if frac(x/2) else 1000
                  if frac(x/3) else 1000
                  x1=sqrt(x) : d=5@
                  Repeat 
                        if frac(x/d ) else exit
                        d += 2: if d>x1 then ok=true : exit
                        if frac(x/d) else exit
                        d += 4: if d<= x1 else ok=true: exit
                   Always
      1000    If ok then i++:Append Known, i:=x  : if not exist(Known1, x) then Append Known1, x
                   x+=2 : Loop }
      }
      \\ IsPrime has same closure, Known1
      IsPrime=lambda  Known1 (x as decimal) -> {
            if exist(Known1, x) then =true : exit
            if Eval(Known1, len(Known1)-1)>x then exit
            if frac(x/2) else exit
            if frac(x/3) else exit
            x1=sqrt(x):d = 5@
            {if frac(x/d ) else exit
                  d += 2: if d>x1 then =true : exit
                  if frac(x/d) else exit
                  d += 4: if d<= x1 else =true: exit
                  loop
             }
      }
      \\ fill Known1, PrimeNth is a closure here
      IsPrime2=lambda  Known1, PrimeNth (x as decimal) -> {
            if exist(Known1, x) then =true : exit
            i=len(Known1)
            if Eval(Known1, i-1)>x then exit
            {
                  z=PrimeNth(i)
                  if z<x then loop else.if z=x then =true :exit
                  i++
            }
      }
      Print "First twenty primes"
      n=PrimeNth(20)
      For i=1 to 20  : Print Known(i),: Next i
      Print
      Print "Primes between 100 and 150:"
      c=0     
      For i=100 to 150
            If IsPrime2(i) Then print i, : c++
      Next i
      Print
      Print "Count:", c
      Print "Primes between 7700 and 8000:"
      c=0
      For i=7700  to 8000
            If IsPrime(i) Then print i, : c++
      Next i
      Print
      Print "Count:", c
      Print "200th Prime:"
      Print PrimeNth(200)
      Print "List from 190th to 199th Prime:"
      For i=190 to 199 : Print Known(i), : Next i
      Print
      Print "Wait"
      Refresh  ' because refresh happen on next Print, which take time
      ' using set fast! we get no respond from GUI/M2000 Console
      ' also Esc, Break and Ctrl+C not work
      ' we have to use Refresh each 500 primes to have one Refresh
      Set fast !
      for i=500 to 10000 step 50: m=PrimeNth(i): Print "."; :Refresh:Next i
      Print
      Print "10000th Prime:", PrimeNth(10000)
      ' reset speed to fast (there are three levels: slow/fast/fast!)
      set fast
      Print
      Rem 1 : Print Known
      Rem  2: Print Known1
}
CheckPrimes

```

{{out}}

```txt

First twenty primes
 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
Primes between 100 and 150:
 101 103 107 109 113 127 131 137 139 149
Count: 10
Primes between 7700 and 8000:
 7703 7717 7723 7727 7741 7753 7757 7759 7789 7793 7817 7823 7829 7841 7853 7867 7873 7877 7879 7883 7901 7907 7919 7927 7933 7937 7949 7951 7963 7993
Count: 30
200th Prime: 
 1223
List from 190th to 199th Prime:
 1151 1153 1163 1171 1181 1187 1193 1201 1213 1217
Wait
.... (truncate for output)
10000th Prime: 104729

```



### Code Optimization


We can drop ok variable from PrimeNth, using a second label. Statement Restart, restart the block. Labels are hashed when first time used.

I use same indentation so you can copy it at same position as in example above. A loop statement mark once the current block for restart after then last statement on block.


```M2000 Interpreter

PrimeNth=lambda  Known, Known1  (n as long) -> {
      if n<1 then Error "Only >=1"
      if exist(known, n) then =eval(known) : exit
      if n>5 then {
           i=len(known1) 
           x=eval(known1, i-1)+2
      } else  x=5 : i=2
      { 
            if i=n then  =known(n) : exit
            if  frac(x) then 999
            if frac(x/2) else 999
            if frac(x/3) else 999
            x1=sqrt(x) : d=5@
            {if frac(x/d ) else 999
                  d += 2: if d>x1 then  1000
                  if frac(x/d) else 999
                  d += 4: if d<= x1 else 1000
                  loop
             }
 999     x++ : Restart
1000     i++:Append Known, i:=x  : if not exist(Known1, x) then Append Known1, x
         x++ : Loop }
}

```


=={{header|Mathematica}} / {{header|Wolfram Language}}== 
Prime and PrimePi use sparse caching and sieving. For large values, the Lagarias–Miller–Odlyzko algorithm for PrimePi is used, based on asymptotic estimates of the density of primes, and is inverted to give Prime.
PrimeQ first tests for divisibility using small primes, then uses the Miller–Rabin strong pseudoprime test base 2 and base 3, and then uses a Lucas test.

```Mathematica
Prime[Range[20]]
{2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71}
Select[Range[100,150], PrimeQ]
{101, 103, 107, 109, 113, 127, 131, 137, 139, 149}
PrimePi[8000] - PrimePi[7700]
30
Prime[10000]
104729
```



## Nim


For such trivial ranges as the task requirements or solving the Euler Problem 10 of summing the primes to two million, a basic generator such as the hash table based version from the [[Sieve_of_Eratosthenes#Nim_Unbounded_Versions]] section of the Sieve of Eratosthenes task will suffice, as follows:

```nim
import tables

type PrimeType = int

proc primesHashTable(): iterator(): PrimeType {.closure.} =
  iterator output(): PrimeType {.closure.} =
    # some initial values to avoid race and reduce initializations...
    yield 2.PrimeType; yield 3.PrimeType; yield 5.PrimeType; yield 7.PrimeType
    var h = initTable[PrimeType,PrimeType]()
    var n = 9.PrimeType
    let bps = primesHashTable()
    var bp = bps() # advance past 2
    bp = bps(); var q = bp * bp # to initialize with 3
    while true:
      if n >= q:
        let inc = bp + bp
        h.add(n + inc, inc)
        bp = bps(); q = bp * bp
      elif h.hasKey(n):
        var inc: PrimeType
        discard h.take(n, inc)
        var nxt = n + inc
        while h.hasKey(nxt): nxt += inc # ensure no duplicates
        h.add(nxt, inc)
      else: yield n
      n += 2.PrimeType
  output

var num = 0
stdout.write "The first 20 primes are:  "
var iter = primesHashTable()
for p in iter():
  if num >= 20: break else: stdout.write(p, " "); num += 1
echo ""
stdout.write "The primes between 100 and 150 are:  "
iter = primesHashTable()
for p in iter():
  if p >= 150: break
  if p >= 100: stdout.write(p, " ")
echo ""
num = 0
iter = primesHashTable()
for p in iter():
  if p > 8000: break
  if p >= 7700: num += 1
echo "The number of primes between 7700 and 8000 is:  ", num
num = 1
iter = primesHashTable()
for p in iter():
  if num >= 10000:
    echo "The 10,000th prime is:  ", p
    break
  num += 1
var sum = 0
iter = primesHashTable()
for p in iter():
  if p >= 2_000_000:
    echo "The sum of the primes to two million is:  ", sum
    break
  sum += p
```

{{output}}

```txt
The first 20 primes are:  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
The primes between 100 and 150 are:  101 103 107 109 113 127 131 137 139 149
The number of primes between 7700 and 8000 is:  30
The 10,000th prime is:  104729
The sum of the primes to two million is:  142913828922
```


The code isn't particularly fast but more than adequate to run this trivial series of tasks and capable for ranges of a few tens of millions.  It is limited by the maximum size of the `int` numeric range, but is too slow to ever reach that in a reasonable time, as in many minutes to even reach the 32-bit integer number range of over two billion let alone to reach the integer limit for 64-bit machines.

'''Alternate much faster Page Segmented Mutable Bit-Packed Seq version'''

The some Sieve of Eratosthenes Nim Unbounded Versions section as above includes a Page Segmented version at the end that can be used the same way for ranges of a billion in only a second (or something approaching a hundred times faster); this can be used for "industrial strength ranges" as in many billions in a reasonable time.  Its limit is just the numeric range used, so for 64-bit prime times is about 2e10 in years.

To use that sieve, just substitute the following in the code:

```nim
for p in primesPaged():
```


wherever the following is in the code:

```nim
for p in iter():
```

and one doesn't need the lines including `iter` just above these lines at all.

As noted in that section, using an extensible generator isn't the best choice for huge ranges as much higher efficiency can be obtained by using functions that deal directly with the composite culled packed-bit array representations directly as the `countPrimesTo` function does.  This makes further improvements to the culling efficiency pointless, as even if one were to reduce the culling speed to zero, it still would take several seconds per range of a billion to enumerate the found primes as a generator.


## PARI/GP


PARI includes a nice prime generator which is quite extensible:

```c
void
showprimes(GEN lower, GEN upper)
{
  forprime_t T;
  if (!forprime_init(&T, a,b)) return;
  while(forprime_next(&T))
  {
    pari_printf("%Ps\n", T.pp);
  }
}
```


Most of these functions are already built into GP:

```parigp
primes(20)
primes([100,150])
#primes([7700,8000]) /* or */
s=0; forprime(p=7700,8000,s++); s
prime(10000)
```


=={{Header|Pascal}}==
{{works with|Free Pascal}}
<b>there is something wrong </b>
<pre >
http://www.primos.mat.br/Ate100G.html -> 
75. de 16639648367 a 16875026921
76. de 16875026963 a 17110593779
77. de 17110593791 a 17346308407
...
my unit: 
 750000000 16875026921
 760000000 17110593779
 770000000 17346251243 <----Wrong

```


Limited to 2.7e14. Much faster than the  other Version. 94s to 257 s for the primes 2..1E11
First the unit.Still a work in progress.

versus primesieve  no chance at all, even single threaded 5x faster :-)
./primesieve -v
primesieve 7.2, <https://primesieve.org>
Copyright (C) 2010 - 2018 Kim Walisch
./primesieve -t1 100000071680 Sieve size = 256 KiB Threads = 1 100%
Seconds: 19.891 Primes: 4118057696

```pascal

unit primsieve;
//{$O+,R+}
{$IFDEF FPC}
  {$MODE objFPC}
  {$CODEALIGN proc=32,loop=1}
{$IFEND}
{segmented sieve of Erathostenes using only odd numbers}
{using presieved sieve of small primes, to reduce the most time consuming}
interface
  procedure InitPrime;
  procedure NextSieve;
  function SieveStart:Uint64;
  function SieveSize :LongInt;
  function Nextprime: Uint64;
  function TotalCount :Uint64;
  function PosOfPrime: Uint64;

implementation
uses
  sysutils;
const
  smlPrimes :array [0..10] of Byte = (2,3,5,7,11,13,17,19,23,29,31);
  maxPreSievePrimeNum = 7;
  maxPreSievePrime = 17;//smlPrimes[maxPreSievePrimeNum];
  cSieveSize = 16384 * 4; //<= High(Word)+1 // Level I Data Cache
type
  tSievePrim = record
                 svdeltaPrime:word;//diff between actual and new prime
                 svSivOfs:word;    //Offset in sieve
                 svSivNum:LongWord;//1 shl (1+16+32) = 5.6e14
               end;
  tpSievePrim = ^tSievePrim;

var
//sieved with primes 3..maxPreSievePrime.here about 255255 Byte
{$ALIGN 32}
  preSieve :array[0..3*5*7*11*13*17-1] of Byte;//must be > cSieveSize
{$ALIGN 32}
  Sieve :array[0..cSieveSize-1] of Byte;
{$ALIGN 32}
//prime = FoundPrimesOffset + 2*FoundPrimes[0..FoundPrimesCnt]
  FoundPrimes : array[0..12252] of Word;
{$ALIGN 32}
  sievePrimes : array[0..78498] of tSievePrim;// 1e6^2 ->1e12
//sievePrimes : array[0..1077863] of tSievePrim;// maximum 1e14
  FoundPrimesOffset : Uint64;
  FoundPrimesCnt,
  FoundPrimesIdx,
  FoundPrimesTotal,
  SieveNum,
  SieveMaxIdx,
  preSieveOffset,
  LastInsertedSievePrime :NativeUInt;

procedure CopyPreSieveInSieve; forward;
procedure CollectPrimes; forward;
procedure sieveOneSieve; forward;
procedure Init0Sieve; forward;
procedure SieveOneBlock; forward;

//****************************************
procedure preSieveInit;
var
  i,pr,j,umf : NativeInt;
Begin
  fillchar(preSieve[0],SizeOf(preSieve),#1);
  i := 1;
  pr := 3;// starts with pr = 3
  umf := 1;
  repeat
    IF preSieve[i] =1 then
    Begin
      pr := 2*i+1;
      j := i;
      repeat
        preSieve[j] := 0;
        inc(j,pr);
      until j> High(preSieve);
      umf := umf*pr;
    end;
    inc(i);
  until (pr = maxPreSievePrime)OR(umf>High(preSieve)) ;
  preSieveOffset := 0;
end;

function InsertSievePrimes(PrimPos:NativeInt):NativeInt;
var
  j    :NativeUINt;
  i,pr : NativeUInt;
begin
  i := 0;
  //ignore first primes already sieved with
  if SieveNum = 0 then
    i := maxPreSievePrimeNum;
  pr :=0;
  j := Uint64(SieveNum)*cSieveSize*2-LastInsertedSievePrime;
  with sievePrimes[PrimPos] do
  Begin
    pr := FoundPrimes[i]*2+1;
    svdeltaPrime := pr+j;
    j := pr;
  end;
  inc(PrimPos);
  for i := i+1 to FoundPrimesCnt-1 do
  Begin
    IF PrimPos > High(sievePrimes) then
      BREAK;
    with sievePrimes[PrimPos] do
    Begin
      pr := FoundPrimes[i]*2+1;
      svdeltaPrime := (pr-j);
      j := pr;
    end;
    inc(PrimPos);
  end;
  LastInsertedSievePrime :=Uint64(SieveNum)*cSieveSize*2+pr;
  result := PrimPos;
end;

procedure CalcSievePrimOfs(lmt:NativeUint);
//lmt High(sievePrimes)
var
  i,pr : NativeUInt;
  sq : Uint64;
begin
  pr := 0;
  i := 0;
  repeat
    with sievePrimes[i] do
    Begin
      pr := pr+svdeltaPrime;
      IF sqr(pr)  < (cSieveSize*2) then
      Begin
        svSivNum := 0;
        svSivOfs := (pr*pr-1) DIV 2;
      end
      else
      Begin
        SieveMaxIdx := i;
        pr := pr-svdeltaPrime;
        BREAK;
      end;
    end;
    inc(i);
  until i > lmt;

  for i := i to lmt do
  begin
    with sievePrimes[i] do
    Begin
      pr := pr+svdeltaPrime;
      sq := sqr(pr);
      svSivNum := sq DIV (2*cSieveSize);
      svSivOfs := ( (sq - Uint64(svSivNum)*(2*cSieveSize))-1)DIV 2;
    end;
  end;
end;

procedure sievePrimesInit;
var
  i,j,pr,PrimPos:NativeInt;
Begin
  LastInsertedSievePrime := 0;
  preSieveOffset := 0;
  SieveNum :=0;
  CopyPreSieveInSieve;
  //normal sieving of first sieve
  i := 1; // start with 3
  repeat
    while Sieve[i] = 0 do
      inc(i);
    pr := 2*i+1;
    inc(i);
    j := ((pr*pr)-1) DIV 2;
    if j > High(Sieve) then
      BREAK;
    repeat
      Sieve[j] := 0;
      inc(j,pr);
    until j > High(Sieve);
  until false;

  CollectPrimes;
  PrimPos := InsertSievePrimes(0);
  LastInsertedSievePrime := FoundPrimes[PrimPos]*2+1;

  IF PrimPos < High(sievePrimes) then
  Begin
    Init0Sieve;
    //Erste Sieb nochmals, aber ohne Eintrag
    sieveOneBlock;
    repeat
      sieveOneBlock;
      dec(SieveNum);
      PrimPos := InsertSievePrimes(PrimPos);
      inc(SieveNum);
   until PrimPos > High(sievePrimes);
  end;
  Init0Sieve;
end;

procedure Init0Sieve;
begin
  FoundPrimesTotal :=0;
  preSieveOffset := 0;
  SieveNum :=0;
  CalcSievePrimOfs(High(sievePrimes));
end;

procedure CopyPreSieveInSieve;
var
  lmt : NativeInt;
Begin
  lmt := preSieveOffset+cSieveSize;
  lmt := lmt-(High(preSieve)+1);
  IF lmt<= 0 then
  begin
    Move(preSieve[preSieveOffset],Sieve[0],cSieveSize);
    if lmt <> 0 then
      inc(preSieveOffset,cSieveSize)
    else
      preSieveOffset := 0;
  end
  else
  begin
    Move(preSieve[preSieveOffset],Sieve[0],cSieveSize-lmt);
    Move(preSieve[0],Sieve[cSieveSize-lmt],lmt);
    preSieveOffset := lmt
  end;
end;

procedure sieveOneSieve;
var
  sp:tpSievePrim;
  pSieve :pByte;
  i,j,pr,sn,dSievNum :NativeUint;

Begin
  pr := 0;
  sn := sieveNum;
  sp := @sievePrimes[0];
  pSieve := @Sieve[0];
  For i := SieveMaxIdx downto 0 do
    with sp^ do
    begin
      pr := pr+svdeltaPrime;
      IF svSivNum = sn then
      Begin
        j := svSivOfs;
        repeat
          pSieve[j] := 0;
          inc(j,pr);
        until j > High(Sieve);
        dSievNum := j DIV cSieveSize;
        svSivOfs := j-dSievNum*cSieveSize;
        svSivNum := sn+dSievNum;
//        svSivNum := svSivNum+dSievNum;
      end;
      inc(sp);
    end;
  i := SieveMaxIdx+1;
  repeat
    if i > High(SievePrimes) then
      BREAK;
    with sp^ do
    begin
      if svSivNum > sn then
      Begin
        SieveMaxIdx := I-1;
        Break;
      end;
      pr := pr+svdeltaPrime;
      j := svSivOfs;
      repeat
        Sieve[j] := 0;
        inc(j,pr);
      until j > High(Sieve);
      dSievNum := j DIV cSieveSize;
      svSivOfs := j-dSievNum*cSieveSize;
      svSivNum := sn+dSievNum;
    end;
    inc(i);
    inc(sp);
  until false;
end;

procedure CollectPrimes;
//extract primes to FoundPrimes
//
var
   pSieve : pbyte;
   pFound : pWord;
   i,idx : NativeUint;
Begin
  FoundPrimesOffset := SieveNum*2*cSieveSize;
  FoundPrimesIdx := 0;
  pFound :=@FoundPrimes[0];
  i := 0;
  idx := 0;
  IF SieveNum = 0 then
  //include small primes used to pre-sieve
  Begin
    repeat
      pFound[idx]:= (smlPrimes[idx]-1) DIV 2;
      inc(idx);
    until smlPrimes[idx]>maxPreSievePrime;
    i := (smlPrimes[idx] -1) DIV 2;
  end;
  //grabbing the primes without if then -> reduces time extremly
  //primes are born to let branch-prediction fail.
  pSieve:= @Sieve[Low(Sieve)];
  repeat
    //store every value until a prime aka 1 is found
    pFound[idx]:= i;
    inc(idx,pSieve[i]);
    inc(i);
  until i>High(Sieve);
  FoundPrimesCnt:= idx;
  inc(FoundPrimesTotal,Idx);
end;

procedure SieveOneBlock;
begin
  CopyPreSieveInSieve;
  sieveOneSieve;
  CollectPrimes;
  inc(SieveNum);
end;

procedure NextSieve;
Begin
  SieveOneBlock;
end;

function Nextprime:Uint64;
Begin
  result := FoundPrimes[FoundPrimesIdx]*2+1+FoundPrimesOffset;
  if (FoundPrimesIdx=0) AND (sievenum = 1) then
    inc(result);
  inc(FoundPrimesIdx);
  If FoundPrimesIdx>= FoundPrimesCnt then
    SieveOneBlock;
end;

function PosOfPrime: Uint64;
Begin
  result := FoundPrimesTotal-FoundPrimesCnt+FoundPrimesIdx;
end;

function TotalCount :Uint64;
begin
  result := FoundPrimesTotal;
end;

function SieveSize :LongInt;
Begin
  result := 2*cSieveSize;
end;

function SieveStart:Uint64;
Begin
  result := (SieveNum-1)*2*cSieveSize;
end;

procedure InitPrime;
Begin
  Init0Sieve;
  SieveOneBlock;
end;

begin
  preSieveInit;
  sievePrimesInit;
  InitPrime;
end.
{compiler: fpc/3.2.0/ppcx64 -Xs -O4 "%f"
50851378 in 1000079360 dauerte 529 ms
455052800 in 10000007168 dauerte 6297 ms
4118057696 in 100000071680 dauerte 93783 ms
}
```


;the test program:

```pascal

program test;
uses
  primsieve;

var
  i : NativeInt;
  cnt : Uint64;
Begin
  writeln('First 25 primes');
  For i := 1 to 25 do
    write(Nextprime:3);
  writeln;
Writeln;
  writeln('Primes betwenn 100 and 150');
  repeat
    i := NextPrime
  until i > 100;
  repeat
    write(i:4);
    i := NextPrime;
  until i>150;
  writeln;
Writeln;
  repeat
    i := NextPrime
  until i > 7700;
  cnt := 0;
  repeat
    inc(cnt);
    i := NextPrime;
  until i> 8000;
  writeln('between 7700 and 8000 are ',cnt,' primes');
Writeln;
  writeln('      i.th       prime');
  cnt := 10000;
  repeat
    while TotalCount < cnt do
      NextSieve;
    repeat
      i := NextPrime;
    until PosOfPrime = cnt;
    writeln(cnt:10,i:12);
    cnt := cnt*10;
  until cnt >100*1000*1000;
end.
```

;output:

```txt

First 25 primes
  2  3  5  7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

Primes betwenn 100 and 150
 101 103 107 109 113 127 131 137 139 149

between 7700 and 8000 are 30 primes

      i.th       prime
     10000      104729
    100000     1299709
   1000000    15485863
  10000000   179424673
 100000000  2038074743

real	0m1,121s

```




### alternative

The main intention is the use in http://rosettacode.org/wiki/Emirp_primes.
The speed is about 3x times slower than sieve of Atkin.About 13 secs for 10 billion/146 secs for 100 billion in 64-Bit.
But i can hold all primes til 1e11 in 2.5 Gb memory.Test for isEmirp inserted.
32-bit is slow doing 64-Bit math.Using a dynamic array is slow too in NextPrime.

```pascal
program emirp;
{$IFDEF FPC}
  {$MODE DELPHI}
  {$OPTIMIZATION ON,REGVAR,PEEPHOLE,CSE,ASMCSE}
  {$CODEALIGN proc=8}
//  {$R+,V+,O+}
{$ELSE}
  {$APPLICATION CONSOLE}
{$ENDIF}
uses
  sysutils;
type
  tSievenum      = NativeUint;
const
  cBitSize       = SizeOf(tSievenum)*8;
  cAndMask       = cBitSize-1;
  InitPrim      :array [0..9] of byte = (2,3,5,7,11,13,17,19,23,29);
(*
  {MAXANZAHL     =  2*3*5*7*11*13*17*19;*PRIM}
  MAXANZAHL     :array [0..8] of Longint =(2,6,30,210,2310,30030,
                                         510510,9699690,223092870);
  {WIFEMAXLAENGE =  1*2*4*6*10*12*16*18; *(PRIM-1)}
  WIFEMAXLAENGE :array [0..8] of longint =(1,2,8,48,480,5760,
                                         92160,1658880,36495360);
*)
//Don't sieve with primes that are multiples of 2..InitPrim[BIS]
  BIS           =     5;
  MaxMulFac     =    22; {array [0..9] of byte= (2,4,6,10,14,22,26,34,40,50);}
  cMaxZahl      = 30030;
  cRepFldLen    =  5760;
 
  MaxUpperLimit =  100*1000*1000*1000-1;
 
  MAXIMUM       = ((MaxUpperLimit-1) DIV cMaxZahl+1)*cMaxZahl;
  MAXSUCHE      = (((MAXIMUM-1) div cMaxZahl+1)*cRepFldLen-1)
                    DIV cBitSize;
 
type
  tRpFldIdx  = 0..cRepFldLen-1;
  pNativeUint = ^ NativeUint;
  (* numberField as Bit array *)
  tsearchFld   = array of tSievenum;
 
  tSegment       = record
                     dOfs,
                     dSegment    :tSievenum;
                  end;
  tpSegment     = ^tSegment;
  tMulFeld    = array [0..MaxMulFac shr 1 -1] of tSegment;
  tnumberField= array [0..cMaxZahl-1] of word; //word->  0..cRepFldLen-1
  tRevIdx     = array [tRpFldIdx] of word;//word->  0..cMaxZahl-1
  tDiffFeld   = array [tRpFldIdx] of byte;
  tNewPosFeld = array [tRpFldIdx] of Uint64;
 
  tRecPrime   = record
                  rpPrime,
                  rpsvPos : Uint64;
                  rpOfs,
                  rpSeg   :LongWord;
                end;
 
var
  BitSet,
  BitClr : Array [0..cAndMask] Of NativeUint;
  deltaNewPos : tNewPosFeld;
  MulFeld   : tMulFeld;
  searchFld : tsearchFld;
  number    : tnumberField;
  DiffFld   : tDiffFeld;
  RevIdx    : tRevIdx;
  actSquare   : Uint64;
  NewStartPos,
  MaxPos    : Uint64;
 
const
//K1  = $0101010101010101;
  K55 = $5555555555555555;
  K33 = $3333333333333333;
  KF1 = $0F0F0F0F0F0F0F0F;
  KF2 = $00FF00FF00FF00FF;
  KF4 = $0000FFFF0000FFFF;
  KF8 = $00000000FFFFFFFF;
 
function popcnt(n:Uint64):integer;overload;inline;
var
  c,b,k : NativeUint;
begin
  b := n;
  k := NativeUint(K55);c :=  (b shr  1) AND k; b := (b AND k)+C;
  k := NativeUint(K33);c := ((b shr  2) AND k);b := (b AND k)+C;
  k := NativeUint(KF1);c := ((b shr  4) AND k);b := (b AND k)+c;
  k := NativeUint(KF2);c := ((b shr  8) AND k);b := (b AND k)+c;
  k := NativeUint(KF4);c := ((b shr 16) AND k);b := (b AND k)+c;
  k := NativeUint(KF8);c :=  (b shr 32)+(b AND k);
  result := c;
end;
 
function popcnt(n:LongWord):integer;overload;
var
  c,k : LongWord;
begin
  result  := n;
  IF result = 0 then
    EXIT;
  k := LongWord(K55);c :=  (result  shr  1) AND k; result  := (result  AND k)+C;
  k := LongWord(K33);c := ((result  shr  2) AND k);result  := (result  AND k)+C;
  k := LongWord(KF1);c := ((result  shr  4) AND k);result  := (result  AND k)+c;
  k := LongWord(KF2);c := ((result  shr  8) AND k);result  := (result  AND k)+c;
  k := LongWord(KF4);
  result :=  (result  shr 16) AND k +(result  AND k);
end;
 
procedure Init;
{simple sieve of erathosthenes only eliminating small primes}
var
  pr,i,j,Ofs : NativeUint;
Begin
  //Init Bitmasks
  j := 1;
  For i := 0 to cAndMask do
  Begin
    BitSet[i] := J;
    BitClr[i] := NativeUint(NOT(J));
    j:= j+j;
  end;
  //building number wheel excluding multiples of small primes
  Fillchar(number,SizeOf(number),#0);
  For i := 0 to BIS do
  Begin
    pr := InitPrim[i];
    j := (High(number) div pr)*pr;
    repeat
      number[j] := 1;
      dec(j,pr);
    until j <= 0;
  end;
 
  // build reverse Index and save distances
  i := 1;
  j := 0;
  RevIdx[0]:= 1;
  repeat
    Ofs :=0;
    repeat
      inc(i);
      inc(ofs);
    until number[i] = 0;
    DiffFld[j] := ofs;
    inc(j);
    RevIdx[j] := i;
  until i = High(number);
  DiffFld[j] := 2;
 
  //calculate a bitnumber-index into cRepFldLen
  Fillchar(number,SizeOf(number),#0);
  Ofs := 1;
  for i := 0 to cRepFldLen-2 do
  begin
    inc(Ofs,DiffFld[i]);
    number[ofs] := i+1;
  end;
 
  //direct index into Mulfeld 2->0 ,4-> 1 ...
  For i := 0 to cRepFldLen-1 do
  Begin
    j := (DiffFld[i] shr 1) -1;
    DiffFld[i] := j;
  end;
end;
 
function CalcPos(m: Uint64): Uint64;
{search right position of m}
var
  i,res : NativeUint;
Begin
  res := m div cMaxZahl;
  i   := m-res* Uint64(cMaxzahl);//m mod cMaxZahl
  while (number[i]= 0) and (i <>1) do
  begin
    iF i = 0 THEN
    begin
      Dec(res,cRepFldLen);
      i := cMaxzahl;
    end;
    dec(i);
  end; {while}
  CalcPos := res *Uint64(cRepFldLen) +number[i];
end;
 
procedure CalcSqrOfs(out Segment,Ofs :Uint64);
Begin
  Segment  := actSquare div cMaxZahl;
  Ofs      := actSquare-Segment*cMaxZahl; //ofs Mod cMaxZahl
  Segment  := Segment*cRepFldLen;
end;
 
procedure MulTab(sievePr:Nativeint);
var
 k,Segment,Segment0,Rest,Rest0: NativeUint;
Begin
  {multiplication-table of differences}
  {2* sievePr,4* ,6* ...MaxMulFac*sievePr }
  sievePr := sievePr+sievePr;
  Segment0 := sievePr div cMaxzahl;
 
  Rest0    := sievePr-Segment0*cMaxzahl;
  Segment0 := Segment0 * cRepFldLen;
 
  Segment := Segment0;
  Rest := Rest0;
 
  with MulFeld[0] do
  begin
    dOfs := Rest0;
    dSegment:= Segment0;
  end;
 
  for k := 1 to MaxMulFac shr 1-1 do
  begin
    Segment := Segment+Segment0;
    Rest    := Rest+Rest0;
    IF Rest >= cMaxzahl then
    Begin
      Rest:= Rest-cMaxzahl;
      Segment := Segment+cRepFldLen;
    end;
    with MulFeld[k] do
    begin
      dOfs := Rest;
      dSegment:= Segment;
    end;
  end;
end;
 
procedure CalcDeltaNewPos(sievePr,MulPos:NativeUint);
var
  Ofs,Segment,prevPos,actPos : Uint64;
  i: NativeInt;
Begin
  MulTab(sievePr);
  //start at sqr sievePrime
  CalcSqrOfs(Segment,Ofs);
  NewStartPos := Segment+number[Ofs];
  prevPos := NewStartPos;
  deltaNewPos[0]:= prevPos;
  For i := 0 to cRepFldLen-2 do
  begin
    inc(mulpos);
    IF mulpos >= cRepFldLen then
      mulpos := 0;
    With MulFeld[DiffFld[mulpos]] do
    begin
      Ofs:= Ofs+dOfs;
      Segment := Segment+dSegment;
    end;
    If Ofs >= cMaxZahl then
    begin
      Ofs := Ofs-cMaxZahl;
      Segment := Segment+cRepFldLen;
    end;
    actPos := Segment+number[Ofs];
    deltaNewPos[i]:= actPos - prevPos;
    IF actPos> maxPos then
      BREAK;
 
    prevPos := actPos;
  end;
  deltaNewPos[cRepFldLen-1] := NewStartPos+cRepFldLen*sievePr-prevPos;
end;
 
procedure SieveByOnePrime(var sf:tsearchFld;sievePr:NativeUint);
var
  pNewPos : ^Uint64;
  pSiev0,
  pSiev   : ^tSievenum;// dynamic arrays are slow
  Ofs      : Int64;
  Position : UINt64;
  i: NativeInt;
 
Begin
  pSiev0 := @sf[0];
  Ofs := MaxPos-sievePr *cRepFldLen;
  Position := NewStartPos;
  {unmark multiples of sieve prime}
  repeat
    IF Position < Ofs then
    Begin
      pNewPos:= @deltaNewPos[0];
      For i := Low(deltaNewPos) to High(deltaNewPos) do
      Begin
        pSiev := pSiev0;
        inc(pSiev,Position DIV cBitSize);
        //pSiev^ == @sf[Position DIV cBitSize]
        pSiev^ := pSiev^ AND BitCLR[Position AND cAndMask];
        inc(Position,pNewPos^);
        inc(pNewPos);
      end
    end
    else
    Begin
      pNewPos:= @deltaNewPos[0];
      For i := Low(deltaNewPos) to High(deltaNewPos) do
      Begin
        IF Position >= MaxPos then
          Break;
        pSiev := pSiev0;
        inc(pSiev,Position DIV cBitSize);
        pSiev^ := pSiev^ AND BitCLR[Position AND cAndMask];
        inc(Position,pNewPos^);
        inc(pNewPos);
      end
    end;
  until Position >= MaxPos;
end;
 
procedure SieveAll;
var
  i,
  sievePr,
  PrimPos,
  srPrPos  : NativeUint;

Begin
  Init;
  MaxPos := CalcPos(MaxUpperLimit);
  {start of prime sieving}
  i := (MaxPos-1) DIV cBitSize+1;
  setlength(searchFld,i);
  IF Length(searchFld) <> i then
  Begin
    writeln('Not enough memory');
    Halt(-227);
  end;
  For i := High(searchFld) downto 0 do
     searchFld[i] := NativeUint(-1);
  {the first prime}
  srPrPos := 0;
  PrimPos := 0;
  sievePr := 1;
  actSquare := sievePr;
  repeat
    {next prime}
    inc(srPrPos);
    i := 2*(DiffFld[PrimPos]+1);
    //binom (a+b)^2; a^2 already known
    actSquare := actSquare+(2*sievePr+i)*i;
    inc(sievePr,i);
 
    IF actSquare > MaxUpperLimit THEN
      BREAK;
    {if sievePr == prime then sieve with sievePr}
    if BitSet[srPrPos AND cAndMask] AND
      searchFld[srPrPos DIV cBitSize] <> 0then
    Begin
      write(sievePr:8,#8#8#8#8#8#8#8#8);
      CalcDeltaNewPos(sievePr,PrimPos);
      SieveByOnePrime(searchFld,sievePr);
    end;
    inc(PrimPos);
    if PrimPos = cRepFldLen then
      dec(PrimPos,PrimPos);// := 0;
  until false;
end;
 
function InitRecPrime(pr: UInt64):tRecPrime;
var
  svPos,sg : NativeUint;
Begin
  svPos := CalcPos(pr);
  sg := svPos DIV cRepFldLen;
  with result do
  Begin
    rpsvPos := svPos;
    rpSeg   := sg;
    rpOfs   := svPos - sg*cRepFldLen;
    rpPrime := RevIdx[rpOfs]+ sg*cMaxZahl;
  end;
end;
 
function InitPrimeSvPos(svPos: Uint64):tRecPrime;
var
  sg : LongWord;
Begin
  sg := svPos DIV cRepFldLen;
  with result do
  Begin
    rpsvPos := svPos;
    rpSeg   := sg;
    rpOfs   := svPos - sg*cRepFldLen;
    rpPrime := RevIdx[rpOfs]+ sg*cMaxZahl;
  end;
end;
 
function NextPrime(var pr:  tRecPrime):Boolean;
var
  ofs : LongWord;
  svPos : Uint64;
Begin
  with pr do
  Begin
    svPos := rpsvPos;
    Ofs := rpOfs;
    repeat
      inc(svPos);
      if svPos > MaxPos then
      Begin
        result := false;
        EXIT;
      end;
      inc(Ofs);
      IF Ofs >= cRepFldLen then
      Begin
        ofs := 0;
        inc(rpSeg);
      end;
    until BitSet[svPos AND cAndMask] AND
      searchFld[svPos DIV cBitSize] <> 0;
    rpPrime := rpSeg*Uint64(cMaxZahl)+RevIdx[Ofs];
    rpSvPos := svPos;
    rpOfs := Ofs;
  end;
  result := true;
end;
 
function GetNthPrime(n: Uint64):tRecPrime;
var
  i : longWord;
  cnt: Uint64;
Begin
  IF n > MaxPos then
    EXIT;
 
  i := 0;
  cnt := Bis;
  For i := 0 to n DIV cBitSize do
    inc(cnt,PopCnt(NativeUint(searchFld[i])));
  i := n DIV cBitSize+1;
 
  while cnt < n do
  Begin
    inc(cnt,PopCnt(NativeUint(searchFld[i])));
    inc(i);
  end;
  dec(i);
 
  dec(cnt,PopCnt(NativeUint(searchFld[i])));
  result := InitPrimeSvPos(i*Uint64(cBitSize)-1);
  while cnt < n do
    IF NextPrime(Result) then
      inc(cnt)
    else
      Break;
end;
 
procedure ShowPrimes(loLmt,HiLmt: NativeInt);
var
  p1 :tRecPrime;
Begin
  IF HiLmt < loLmt then
    exit;
  p1 := InitRecPrime(loLmt);
  while p1.rpPrime < LoLmt do
    IF Not(NextPrime(p1)) Then
      EXIT;
 
  repeat
    write(p1.rpPrime,' ');
    IF Not(NextPrime(p1)) Then
      Break;
  until p1.rpPrime > HiLmt;
  writeln;
end;
 
function CountPrimes(loLmt,HiLmt: NativeInt):LongWord;
var
  p1 :tRecPrime;
Begin
  result := 0;
  IF HiLmt < loLmt then
    exit;
  p1 := InitRecPrime(loLmt);
  while p1.rpPrime < LoLmt do
    IF Not(NextPrime(p1)) Then
      EXIT;
  repeat
    inc(result);
    IF Not(NextPrime(p1)) Then
      Break;
  until p1.rpPrime > HiLmt;
end;
 
procedure WriteCntSmallPrimes(n: NativeInt);
var
  i, p,prPos,svPos : nativeUint;
Begin
  dec(n);
  IF n < 0 then
    EXIT;
  write('First ',n+1,' primes ');
  IF n < Bis then
  Begin
    For i := 0 to n do
      write(InitPrim[i]:3);
  end
  else
  Begin
    For i := 0 to BIS do
      write(InitPrim[i],' ');
    dec(n,Bis);
 
    svPos := 0;
    PrPos := 0;
    p     := 1;
    while n> 0 do
    Begin
      {next prime}
      inc(svPos);
      inc(p,2*(DiffFld[prPos]+1));
      if BitSet[svPos AND cAndMask] AND searchFld[svPos DIV cBitSize] <>0 then
      Begin
        write(p,' ');
        dec(n);
      end;
      inc(prPos);
      if prPos = cRepFldLen then
        dec(prPos,prPos);// := 0;
    end;
  end;
  writeln;
end;
 
function RvsNumL(var n: Uint64):Uint64;
//reverse and last digit, most of the time n > base therefor repeat
const
  base = 10;
var
  q, c: Int64;
Begin
  result := n;
  q := 0;
  repeat
    c:= result div Base;
    q := result+ (q-c)*Base;
    result := c;
  until result < Base;
  n := q*Base+result;
end;
 
function IsEmirp(n:Uint64):boolean;
var
 lastDgt:NativeUint;
 ofs: NativeUint;
 seg : Uint64;
Begin
  seg := n;
  lastDgt:= RvsNumL(n);
  result:= false;
  IF (seg = n) OR (n> MaxUpperLimit) then
    EXIT;
 
  IF lastDgt in [1,3,7,9] then
  Begin
    seg := n div cMaxZahl;
    ofs := n-seg* cMaxzahl;//m mod cMaxZahl
    IF (Number[ofs] <> 0) OR (ofs=1) then
    begin
      seg := seg *cRepFldLen+number[ofs];
      result := BitSet[seg AND cAndMask]  AND searchFld[seg DIV cBitSize] <> 0;
    end
  end;
end;
 
function GetEmirps(loLmt,HiLmt: Uint64):NativeInt;
var
  p1 :tRecPrime;
Begin
  result := 0; 
  IF HiLmt < loLmt then
    exit;
  IF loLmt > MaxUpperLimit then
    Exit;
  IF HiLmt > MaxUpperLimit then
    HiLmt := MaxUpperLimit;

  p1 := InitRecPrime(loLmt);
  while p1.rpPrime < LoLmt do
    IF Not(NextPrime(p1)) Then
      EXIT;
 
  repeat
    if isEmirp(p1.rpPrime) then
      inc(result);
    iF not(NextPrime(p1)) then
      BREAK;
  until p1.rpPrime > HiLmt;
end;
 
var
  T1,T0: TDateTime;
  Anzahl :Uint64;
  i,j,dgtCnt,totalCnt : Uint64;
  n : LongInt;
Begin
  T0 := now;
  SieveAll;
  T1 := now;
  writeln('         ');
  Writeln('time for sieving ',FormatDateTime('NN:SS.ZZZ',T1-T0));
  Anzahl := BIS;
  For n := MaxPos DIV cBitSize-1 downto 0 do
    inc(Anzahl,PopCnt(NativeUint(searchFld[n])));
  n := MaxPos AND cAndMask;
  IF n >0 then
  Begin
    dec(n);
    repeat
      IF BitSet[n] AND searchFld[MaxPos DIV cBitSize] <> 0 then
        inc(Anzahl);
      dec(n);
    until n< 0;
  end;
 
  Writeln('there are ',Anzahl,' primes til ',MaxUpperLimit);
  WriteCntSmallPrimes(20);
  write('primes between 100 and 150: ');
  ShowPrimes(100,150);
  write('count of primes between 7700 and 8000 ');
  Writeln(CountPrimes(7700,8000));
  i := 100;
  repeat
    Writeln('the ',i, ' th prime ',GetNthPrime(i).rpPrime);
    i := i * 10;
  until i*25 > MaxUpperLimit;
 
  writeln;
  writeln('Count Emirps');
  writeln('             Emirp           Total');
  writeln('Decimals     Count           Count');
  totalCnt := 0;
  j := 10;
  i := 2;
  dgtCnt := 2;  // 13 is not present so 13<->31 isnt found
  repeat
    write(i:8);
    inc(dgtCnt,GetEmirps(  j,  j+j-1));//10..00->19..99
    inc(dgtCnt,GetEmirps(3*j,3*j+j-1));//30..00->39..99
    inc(dgtCnt,GetEmirps(7*j,7*j+j-1));//70..00->79..99
    inc(dgtCnt,GetEmirps(9*j,9*j+j-1));//90..00->99..99
    inc(TotalCnt,dgtCnt);
    writeln(dgtCnt:12,TotalCnt:14);
    j:=j*10;
    inc(i);
    dgtCnt := 0;    
  until j >= MaxUpperLimit;
end.
```

;output:

```txt
//64-Bit
time ./emirp
         
time for sieving 04:17.895
there are 4118054813 primes til 99999999999
First 20 primes 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 
primes between 100 and 150: 101 103 107 109 113 127 131 137 139 149 
count of primes between 7700 and 8000 30
the 100 th prime 541
the 1000 th prime 7919
the 10000 th prime 104729
the 100000 th prime 1299709
the 1000000 th prime 15485863
the 10000000 th prime 179424673
the 100000000 th prime 2038074743
the 1000000000 th prime 22801763489

Count Emirps
               Emirp         Total
Decimals       Count         Count
       2           8             8
       3          28            36
       4         204           240
       5        1406          1646
       6        9538         11184
       7       70474         81658
       8      535578        617236
       9     4192024       4809260
      10    33619380      38428640
      11   274890232     313318872

real	7m44.649s

```


=={{Header|Perl}}==
Two examples of pure Perl extensible generators are shown in the [[Sieve of Eratosthenes#Extensible_sieves]] section.

The [https://metacpan.org/pod/Math::Prime::Util Math::Prime::Util] module provides a highly performant, feature-rich library for generating, testing, and manipulating prime numbers in Perl. It offers full interoperability with Perl's bigint pragma.

Limits with a 64-bit Perl:
* nth_prime takes about 20 seconds to return the 10^14th prime and should be fast for all results up to ~4e17.  It will be impractically slow past that.
* prime_count uses the LMO algorithm and takes about 35 seconds to return the count for primes to 10^16, and should have state of the art speed to 2^64-1.  After that it will use a primality test in the interval so it still useful for large sizes with a small range.
* fast approximations and upper/lower limits are available, which should be fast for any input size including bigints.
* <tt>primes</tt>, <tt>next_prime</tt>, <tt>prev_prime</tt>, <tt>forprimes</tt>, <tt>prime_iterator</tt>, <tt>prime_iterator_object</tt>, and primality tests will work for practically any size input.  The [https://metacpan.org/pod/Math::Prime::Util::GMP Math::Prime::Uti::GMP] module is recommended for large inputs.  With that module, these functions will work quickly for multi-thousand digit numbers.


```perl
use Math::Prime::Util qw(nth_prime prime_count primes);
# Direct solutions.
# primes([start],end) returns an array reference with all primes in the range
# prime_count([start],end) uses sieving or LMO to return fast prime counts
# nth_prime(n) does just that.  It runs quite fast for native size inputs.
say "First 20: ", join(" ", @{primes(nth_prime(20))});
say "Between 100 and 150: ", join(" ", @{primes(100,150)});
say prime_count(7700,8000), " primes between 7700 and 8000";
say "${_}th prime: ", nth_prime($_) for map { 10**$_ } 1..8;
```

{{out}}

```txt
First 20: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
Between 100 and 150: 101 103 107 109 113 127 131 137 139 149
30 primes between 7700 and 8000
10th prime: 29
100th prime: 541
1000th prime: 7919
10000th prime: 104729
100000th prime: 1299709
1000000th prime: 15485863
10000000th prime: 179424673
100000000th prime: 2038074743
```


There are many other ways, including <tt>prev_prime</tt> / <tt>next_prime</tt>, a simple iterator, an OO style iterator, a tied array, and a Pari-like <tt>forprimes</tt> construct.  For example, the above example using the OO iterator:

```perl
use Math::Prime::Util "prime_iterator_object";
my $it = prime_iterator_object;
say "First 20: ", join(" ", map { $it->iterate() } 1..20);
$it->seek_to_value(100);
print "Between 100 and 150:";
print " ", $it->iterate() while $it->value() <= 150;
print "\n";
$it->seek_to_value(7700);
my $c = 0;
$c++ while $it->iterate() <= 8000;
say "$c primes between 7700 and 8000";
say "${_}th prime: ", $it->ith($_) for map { 10**$_ } 1..8;
```

Or using forprimes and a tied array:

```perl
use Math::Prime::Util qw/forprimes/;
use Math::Prime::Util::PrimeArray;
tie my @primes, 'Math::Prime::Util::PrimeArray';

say "First 20: @primes[0..19]";  # Slice from the tied array
print "Between 100 and 150: ";  forprimes { print " $_"; } 100,150;  print "\n";
# Count with forprimes
my $c = 0;
forprimes { $c++ } 7700,8000;
print "$c primes between 7700 and 8000\n";
# The tied array tries to do the right thing -- sieve a window if it sees
# forward or backward iteration, and nth_prime if it looks like random access.
say "${_}th prime: ", $primes[$_-1] for map { 10**$_ } 1..8;
```


Example showing bigints:

```perl
use bigint;
use Math::Prime::Util qw/forprimes prime_get_config/;
warn "No GMP, expect slow results\n" unless prime_get_config->{gmp};
my $n = 10**200;
forprimes { say $_-$n } $n,$n+1000;
```

{{out}}

```txt

357
627
799
```



## Perl 6

Build a lazy infinite list of primes using the Perl 6 builtin is-prime method. ( A lazy list will not bother to calculate the values until they are actually used. ) That is the first line. If you choose to calculate the entire list, it is fairly certain that you will run out of process / system memory before you finish... (patience too probably) but there aren't really any other constraints. The is-prime builtin uses a Miller-Rabin primality test with 100 iterations, so while it is not 100% absolutely guaranteed that every number it flags as prime actually is, the chances that it is wrong are about 4<sup>-100</sup>. Much less than the chance that a cosmic ray will cause an error in your computers CPU.  Everything after the first line is just display code for the various task requirements.


```perl6
my @primes = lazy gather for 1 .. * { .take if $_.is-prime }

say "The first twenty primes:\n   ", "[{@primes[^20].fmt("%d", ', ')}]";
say "The primes between 100 and 150:\n   ", "[{@primes.&between(100, 150).fmt("%d", ', ')}]";
say "The number of primes between 7,700 and 8,000:\n   ", +@primes.&between(7700, 8000);
say "The 10,000th prime:\n   ", @primes[9999];

sub between (@p, $l, $u) {
    gather for @p { .take if $l < $_ < $u; last if $_ >= $u }
}
```

{{out}}

```txt
The first twenty primes:
   [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]
The primes between 100 and 150:
   [101, 103, 107, 109, 113, 127, 131, 137, 139, 149]
The number of primes between 7,700 and 8,000:
   30
The 10,000th prime:
   104729
```



## Phix

Allows a new (and discard-able) sieve block to extend the list of already known primes. By starting with 1..10 done, we can
then filter properly up to 100, so the next(/first) sieve cannot be larger than 90, then it goes 9900, with
99990000 logically next, but that blew 32-bit limits, besides capping it at 400k gave the best performance.

Unfortunately we lose the p<sup><small>2</small></sup> check because we are not necessarily looking at that,
on the plus side this completely avoids all marking/checking of even numbers.

This could almost certainly be further improved by halving the size of the sieve block.

I investigated the use of so-called "wheels", beguiled by the claim that "a 2-3-5-7 wheel saves 77%", until 
I realised the breakdown was 2: 50%, 3: 16%, 5: 7%, 7: 4% - it is unthinkable not to exclude even numbers, 
the added complexity (and subscripting) of a 30- or 210- element wheel does not seem worthwhile. While it
would be trivial to unroll a 2-3 wheel, it seems far better just to avoid even numbers altogether (hence, 
I believe, this achieves 66 of those 77% savings). 
Some further discussion of this can be found on my talk page --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]])


```Phix
-- demo/rosetta/Extensible_prime_generator.exw
sequence primes = {2,3,5,7}
atom sieved = 10

procedure add_block()
integer N = min((sieved-1)*sieved,400000)
sequence sieve = repeat(1,N)    -- sieve[i] is really i+sieved
    for i=2 to length(primes) do -- (evens filtered on output)
        atom p = primes[i], p2 = p*p
        if p2>sieved+N then exit end if
        if p2<sieved+1 then
            p2 += ceil((sieved+1-p2)/p)*p
        end if
        p2 -= sieved
        if and_bits(p2,1)=0 then p2 += p end if
--      if sieve[p2] then           -- dang!
            for k=p2 to N by p*2 do
                sieve[k] = 0
            end for
--      end if
    end for
    for i=1 to N by 2 do
        if sieve[i] then
            primes &= i+sieved
        end if
    end for
    sieved += N
end procedure

function is_prime(integer n)
    while sieved<n do
        add_block()
    end while
    return binary_search(n,primes)>0
end function

atom t0 = time()
while length(primes)<20 do add_block() end while
printf(1,"The first 20 primes are: ")   ?primes[1..20]
while sieved<150 do add_block() end while
sequence s = {}
for k=abs(binary_search(100,primes)) to length(primes) do
    integer p = primes[k]
    if p>150 then exit end if
    s &= p
end for
printf(1,"The primes between 100 and 150 are: ")    ?s
s = {}
for i=7700 to 8000 do
    if is_prime(i) then s&=i end if
end for
printf(1,"There are %d primes between 7700 and 8000.\n",length(s))
for i=1 to 8 do
    integer k = power(10,i)
    while length(primes)<k do
        add_block()
    end while
    printf(1,"The %,dth prime is : %d\n",{k,primes[k]})
end for
?time()-t0
```

{{Out}}

```txt

The first 20 primes are: {2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71}
The primes between 100 and 150 are: {101,103,107,109,113,127,131,137,139,149}
There are 30 primes between 7700 and 8000.
The 10th prime is : 29
The 100th prime is : 541
The 1,000th prime is : 7919
The 10,000th prime is : 104729
The 100,000th prime is : 1299709
The 1,000,000th prime is : 15485863
The 10,000,000th prime is : 179424673
The 100,000,000th prime is : 2038074743
27.578

```



## PicoLisp


```PicoLisp
(de prime? (N Lst)
   (let S (sqrt N)
      (for D Lst
         (T (> D S) T)
         (T (=0 (% N D)) NIL) ) ) )
(de primeseq (A B)
   (let (I 1 R)
      (nth
         (make
            (link 2)
            (while (> A (inc 'I 2))
               (and (prime? I (made)) (link I)) )
            (setq R (length (made)))
            (while (> B I)
               (and (prime? I (made)) (link I))
               (inc 'I 2) ) )
         (inc R) ) ) )
(de take (N)
   (let I 1
      (make
         (link 2)
         (do (dec N)
            (until (prime? (inc 'I 2) (made)))
            (link I) ) ) ) )

(prin "First 20 primes: ")
(println (take 20))
(prin "Primes between 100 and 150: ")
(println (primeseq 100 150))
(prinl
   "Number of primes between 7700 and 8000: "
   (length (primeseq 7700 8000)) )
(for N (10 100 1000 10000 100000 1000000)
   (prinl
      N
      "th prime: "
      (last (take N)) ) )
```

{{out}}

```txt
First 20 primes: (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71)
Primes between 100 and 150: (101 103 107 109 113 127 131 137 139 149)
Number of primes between 7700 and 8000: 30
10th prime: 29
100th prime: 541
1000th prime: 7919
10000th prime: 104729
100000th prime: 1299709
1000000th prime: 15485863
```


=={{Header|PureBasic}}==

```PureBasic
EnableExplicit
DisableDebugger
Define StartTime.i=ElapsedMilliseconds()

Procedure.b IsPrime(n.i)
  Define i.i=5
  If n<2 : ProcedureReturn #False : EndIf
  If n%2=0 : ProcedureReturn Bool(n=2) : EndIf
  If n%3=0 : ProcedureReturn Bool(n=3) : EndIf
  While i*i<=n
    If n%i=0 : ProcedureReturn #False : EndIf
    i+2
    If n%i=0 : ProcedureReturn #False : EndIf
    i+4
  Wend  
  ProcedureReturn #True
EndProcedure

If OpenConsole("Extensible prime generator")
  Define c.i=0, n.i=2
  Print("First twenty: ")
  While c<20
    If IsPrime(n)
      Print(Str(n)+" ")
      c+1
    EndIf
    n+1
  Wend
  
  Print(~"\nBetween 100 and 150: ")
  For n=100 To 150
    If IsPrime(n)
      Print(Str(n)+" ")
    EndIf
  Next
  
  Print(~"\nNumber beween 7'700 and 8'000: ")
  c=0
  For n=7700 To 8000
    c+IsPrime(n)
  Next
  Print(Str(c))
  
  Print(~"\n10'000th prime: ")
  c=0 : n=1
  While c<10000
    n+1
    c+IsPrime(n)    
  Wend
  Print(Str(n))  
EndIf
Print(~"\nRuntime milliseconds: "+
      Str(ElapsedMilliseconds()-StartTime))
Input()
```

{{out}}

```txt
First twenty: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
Between 100 and 150: 101 103 107 109 113 127 131 137 139 149
Number beween 7'700 and 8'000: 30
10'000th prime: 104729
Runtime milliseconds: 75
```


=={{Header|Python}}==


### Python: Croft spiral

The Croft spiral sieve prime generator from the [[Prime_decomposition#Python:_Using_Croft_Spiral_sieve|Prime decomposition]] task is used which contains the line 
```python
islice(count(7), 0, None, 2)
```

The call to <code>count(7)</code> is to a generator of integers that counts from 7 upwards with no upper limit set.

The definition <code>croft</code> is a generator of primes and is used to generate as many primes as are asked for, in order.


```python
from __future__ import print_function
from prime_decomposition import primes
from itertools import islice


def p_range(lower_inclusive, upper_exclusive):
    'Primes in the range'
    for p in primes():
        if p >= upper_exclusive: break
        if p >= lower_inclusive: yield p

if __name__ == '__main__':
    print('The first twenty primes:\n  ', list(islice(primes(),20)))
    print('The primes between 100 and 150:\n  ', list(p_range(100, 150)))
    print('The ''number'' of primes between 7,700 and 8,000:\n  ', len(list(p_range(7700, 8000))))
    print('The 10,000th prime:\n  ', next(islice(primes(),10000-1, 10000)))
```


{{out}}

```txt
The first twenty primes:
   [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]
The primes between 100 and 150:
   [101, 103, 107, 109, 113, 127, 131, 137, 139, 149]
The number of primes between 7,700 and 8,000:
   30
The 10,000th prime:
   104729
```


===Python: 210-wheel postponed incremental sieve===
With more contemporary itertools use, 210-wheel incremental sieve with postponed primes processing and thus with radically reduced memory footprint which increases slower than  square root of the number of primes produced:


```python
def wsieve():       # ideone.com/mqO25A
    wh11 = [ 2,4,2,4,6,2,6,4,2,4,6,6, 2,6,4,2,6,4,6,8,4,2,4,2,
             4,8,6,4,6,2,4,6,2,6,6,4, 2,4,6,2,6,4,2,4,2,10,2,10]
    cs = accumulate( chain( [11], cycle( wh11)))
    yield( next( cs))  # cf. ideone.com/WFv4f
    ps = wsieve()      #     codereview.stackexchange.com/q/92365/9064
    p = next(ps)       # 11         stackoverflow.com/q/30553925/849891
    psq = p*p          # 121
    D = dict( zip( accumulate( chain( [0], wh11)), count(0)))   # start from
    mults = {}
    for c in cs:
        if c in mults:
            wheel = mults.pop(c)  
        elif c < psq:              
            yield c ; continue   
        else:          # c==psq:  map (p*) (roll wh from p) = roll (wh*p) from (p*p)
            x = [p*d for d in wh11]
            i = D[ (p-11) % 210]
            wheel = accumulate( chain( [psq+x[i]], cycle( x[i+1:] + x[:i+1])))
            p = next(ps) ; psq = p*p 
        for m in wheel: 
            if not m in mults: 
                break
        mults[m] = wheel

def primes(): 
	yield from (2, 3, 5, 7)
	yield from wsieve() 

print( list( islice( primes(), 0, 20)))
print( list( takewhile( lambda x: x<150, 
                   dropwhile( lambda x: x<100, primes()))))
print( len( list( takewhile( lambda x: x<8000, 
                   dropwhile( lambda x: x<7700, primes())))))
print( list( islice( primes(), 10000-1, 10000))[0])
```


{{out}}

```txt

[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]
[101, 103, 107, 109, 113, 127, 131, 137, 139, 149]
30
104729

```



### Python: Iterative sieve on unbounded count from 2

After a [[https://paddy3118.blogspot.com/2018/11/to-infinity-beyond.html?showComment=1541671062388#c8979782377032256564 blog entry]] on implementing a particular Haskell solution in Python.


```python
from itertools import count, takewhile, islice

def prime_sieve():
    sieved = count(2)
    prime = next(sieved)
    yield prime
    primes = [prime]
    for x in sieved:
        if any(x % prime == 0 for prime in primes):
            continue
        yield x
        primes.append(x)

if __name__ == '__main__':
    def leq_150(x): return x <= 150
    def leq_8000(x): return x <= 8000
    
    print("Show the first twenty primes.\n   =",
        list(islice(prime_sieve(), 20)))
    print("Show the primes between 100 and 150\n   =",
        [x for x in takewhile(leq_150, prime_sieve()) if x >= 100])
    print("Show the number of primes between 7,700 and 8,000.\n   =",
        sum(1 for x in takewhile(leq_8000, prime_sieve()) if x >= 7700))
    print("Show the 10,000th prime.\n   =",
        next(islice(prime_sieve(), 10000-1, 10000)))
```


{{out}}

```txt
Show the first twenty primes.
   = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]
Show the primes between 100 and 150
   = [101, 103, 107, 109, 113, 127, 131, 137, 139, 149]
Show the number of primes between 7,700 and 8,000.
   = 30
Show the 10,000th prime.
   = 104729
```



## Racket

Personal note: unless I need the raw power of an application-specific primes generator/filter,
I pretty well stick with the <code>math/number-theory</code> library. And even when I write an ASPG/F I question
how it performs against the <code>math/number-theory</code> version!

The link referenced in the source: [http://docs.racket-lang.org/math/number-theory.html?q=prime%3F#%28part._primes%29 <code>math/number-theory</code> module documentation]


```scheme
#lang racket
;; Using the prime functions from:
(require math/number-theory)

(displayln "Show the first twenty primes.")
(next-primes 1 20)

(displayln "Show the primes between 100 and 150.")
;; Note that in each of the in-range filters I "add1" to the stop value, so that (in this case) 150 is
;; considered. I'm pretty sure it's not prime... but technology moves so fast nowadays that things
;; might have changed!
(for/list ((i (sequence-filter prime? (in-range 100 (add1 150))))) i)

(displayln "Show the number of primes between 7,700 and 8,000.")
;; (for/sum (...) 1) counts the values in a sequence
(for/sum ((i (sequence-filter prime? (in-range 7700 (add1 8000))))) 1)

(displayln "Show the 10,000th prime.")
(nth-prime (sub1 10000)) ; (nth-prime 0) => 2

;; If a languages in-built prime generator is extensible or is guaranteed to generate primes up to a
;; system limit, (2^31 or memory overflow for example), then this may be used as long as an
;; explanation of the limits of the prime generator is also given. (Which may include a link
;; to/excerpt from, language documentation). 
;;
;; Full details in:
;; [[http://docs.racket-lang.org/math/number-theory.html?q=prime%3F#%28part._primes%29]]
;; When reading the manual, note that "Integer" and "Natural" are unlimited (or bounded by whatever
;; big number representation there is (and the computational complexity of the work being asked).
(define 2^256 (expt 2 256))
2^256
(next-prime 2^256)
;; (Oh, and this is a 64-bit laptop, I left my 256-bit PC in the office.)
```


{{out}}

```txt
Show the first twenty primes.
(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71)
Show the primes between 100 and 150.
(101 103 107 109 113 127 131 137 139 149)
Show the number of primes between 7,700 and 8,000.
30
Show the 10,000th prime.
104729
115792089237316195423570985008687907853269984665640564039457584007913129639936
115792089237316195423570985008687907853269984665640564039457584007913129640233
```



## REXX

Programming note:   Most REXXes (of the 32-bit variety) run with an upper limit of roughly 2 Gbytes (for virtual storage), and that is the limit of this program (in building the stemmed array of prime numbers and prime number indicators, and available virtual storage will be the limiting factor of how many primes can be generated. 

The method of extending primes (via the PRIMES subroutine) is of two kinds when invoking the PRIMES subroutine:
:* a positive number which will (possibly) generate primes up to that total amount, and 
:* a negative number which will (possibly) generate primes up to   |number|.

Two arrays are available to the caller after invoking the PRIMES subroutine.   @.Nth   where this is the Nth prime.   Also, one can check if 1331 is a prime: !.1331 has the value of 1 (is prime), 0 (isn't prime).

For faster speed, the PRIMES subroutine's logic could be optimized a bit, as well as extending the initial list of low primes,   and extending the fixed divisions to reduce the inner   DO K   loop (divisions of 3───►19).

```rexx
/*REXX program calculates and displays primes using an extendible prime number generator*/
parse arg f .;    if f==''  then f=20            /*allow specifying number for  1 ──► F.*/
call primes f;              do j=1  for f;   $=$ @.j;    end  /*j*/
        say 'first'    f    'primes are:'    $
        say
call primes -150;           do j=100  to 150;   if !.j==0  then iterate;  $=$ j; end /*j*/
        say 'the primes between 100 to 150 (inclusive) are:'    $
        say
call primes -8000;          do j=7700  to 8000; if !.j==0  then iterate;  $=$ j; end /*j*/
        say 'the number of primes between 7700 and 8000 (inclusive) is:'  words($)
        say
call primes 10000
        say 'the 10000th prime is:'   @.10000
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
primes: procedure expose !. s. @. $ #;   parse arg H,,$;           Hneg=H<0;      H=abs(H)
        if symbol('!.0')=="LIT"  then do         /*1st time here?  Then initialize stuff*/
                                      !.=0;  @.=0;  s.=0   /*!.x=a prime; @.n=Nth prime.*/
                                      L=2 3 5 7 11 13 17 19 23    /*gen some low primes.*/
                                         do #=1  for words(L);  p=word(L, #); @.#=p; !.p=1
                                         end   /*#*/
                                      #=#-1;  !.0=#;  s.#=@.#**2 /*set #≡numb. of primes*/
                                      end
        if Hneg  then  if  H<=@.#  then return   /*do we have a high enough  P  already?*/
                                   else nop      /*this is used to match the above THEN.*/
                 else  if  H<=#    then return   /*are there enough primes currently ?  */
                                                 /* [↓]  gen more primes within range.  */
           do j=@.# + 2   by 2                   /*find primes until have   H   Primes. */
           if j//3 ==0  then iterate             /*is  J  divisible by three?           */
           parse var j '' -1 _;  if _==5 then iterate     /*is the right─most digit a 5?*/
           if j//7 ==0  then iterate             /*is  J  divisible by  seven?          */
           if j//11==0  then iterate             /* "  "       "     "  eleven?         */
           if j//13==0  then iterate             /* "  "       "     "  thirteen?       */
           if j//17==0  then iterate             /* "  "       "     "  seventeen?      */
           if j//19==0  then iterate             /* "  "       "     "  nineteen?       */
                                                 /*[↑]  above divisors go up to  L  end.*/
                 do k=!.0  while  s.k<=j         /*divide by the known  odd  primes.    */
                 if j//@.k==0  then iterate j    /*Is  J  ÷ by a prime?  ¬prime.     ___*/
                 end   /*k*/                     /* [↑]  divide by odd primes up to √ J */
           #=#+1                                 /*bump the number of primes found.     */
           @.#=j;        s.#=j * j;      !.j=1   /*assign to sparse array;  prime²;  P#.*/
           if Hneg  then if H<=@.#  then leave   /*is this a high enough prime?         */
                                    else nop     /*used to match the above  THEN.       */
                    else if H<=#    then leave   /*have enough primes been generated?   */
           end   /*j*/                           /* [↑]  keep generating until enough.  */
        return                                   /*return to invoker with more primes.  */
```

{{out|output|text=  when using the default input:}}

```txt

first 20 primes are:  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71

the primes between 100 to 150 (inclusive) are:  101 103 107 109 113 127 131 137 139 149

the number of primes between 7700 and 8000 (inclusive) is: 30

the 10000th prime is: 104729

```



## Ring


```ring

see "first twenty primes : "
i = 1
nr = 0
while i <= 20
      nr += 1     
      if isPrime(nr) see " " + nr i += 1 ok
end

see "primes between 100 and 150 : "
for nr = 100 to 150
    if isPrime(nr) see " " + nr ok
next
see nl

see "primes between 7,700 and 8,000 : "
i = 0
for nr = 7700 to 8000
    if isPrime(nr) i += 1 ok
next
see i + nl

see "The 10,000th prime : "
i = 1
nr = 0
while i <= 10000
      nr += 1     
      if isPrime(nr) i += 1 ok
end
see nr + nl
  
func isPrime n
     if n <= 1 return false ok
     if n <= 3 return true ok
     if (n & 1) = 0 return false ok
     for t = 3 to sqrt(n) step 2
         if (n % t) = 0 return false ok
     next
     return true

```

Output:

```txt

first twenty primes :  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
primes between 100 and 150 :  101 103 107 109 113 127 131 137 139 149
primes between 7,700 and 8,000 : 30
The 10,000th prime : 104729

```



## Ruby

The prime library behaves like an enumerator. It has an "each" method, which takes an upper bound as argument. This argument is nil by default, which means no upper bound.

```ruby
require "prime"

puts Prime.take(20).join(", ")
puts Prime.each(150).drop_while{|pr| pr < 100}.join(", ")
puts Prime.each(8000).drop_while{|pr| pr < 7700}.count
puts Prime.take(10_000).last
```

{{out}}

```txt

2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71
101, 103, 107, 109, 113, 127, 131, 137, 139, 149
30
104729

```



## Rust


Uses the code from [[Sieve_of_Eratosthenes#Unbounded_Page-Segmented_bit-packed_odds-only_version_with_Iterator]]; copy that code into a file named <code>src/pagesieve.rs</code> and prepend <code>pub</code> to all function declarations used in this code (<code>count_primes_paged</code> and <code>primes_paged</code>).


```rust
mod pagesieve;

use pagesieve::{count_primes_paged, primes_paged};

fn main() {
    println!("First 20 primes:\n {:?}",
             primes_paged().take(20).collect::<Vec<_>>());
    println!("Primes between 100 and 150:\n {:?}",
             primes_paged().skip_while(|&x| x < 100)
                           .take_while(|&x| x < 150)
                           .collect::<Vec<_>>());
    let diff = count_primes_paged(8000) - count_primes_paged(7700);
    println!("There are {} primes between 7,700 and 8,000", diff);
    // rust enumerations are zero base, so need to subtract 1!!!
    println!("The 10,000th prime is {}", primes_paged().nth(10_000 - 1).unwrap());
}
```

{{out}}

```txt

First 20 primes:
 [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]
Primes between 100 and 150:
 [101, 103, 107, 109, 113, 127, 131, 137, 139, 149]
There are 30 primes between 7,700 and 8,000
The 10,000th prime is 104729

```



## Seed7

The [http://seed7.sourceforge.net/algorith/math.htm#sieve_of_eratosthenes sieve of eratosthenes]
cannot be used, because it needs a limit. Instead the function getPrime is used.
GetPrime generates all primes in sequence.

```seed7
$ include "seed7_05.s7i";

const func boolean: isPrime (in integer: number) is func
  result
    var boolean: prime is FALSE;
  local
    var integer: count is 2;
  begin
    if number = 2 then
      prime := TRUE;
    elsif number > 2 then
      while number rem count <> 0 and count * count <= number do
        incr(count);
      end while;
      prime := number rem count <> 0;
    end if;
  end func;

var integer: currentPrime is 1;
var integer: primeNum is 0;

const func integer: getPrime is func
  result
    var integer: nextPrime is 0;
  begin
    repeat
      incr(currentPrime);
    until isPrime(currentPrime);
    nextPrime := currentPrime;
    incr(primeNum);
  end func;

const proc: main is func
  local
    var integer: aPrime is 0;
    var integer: count is 0;
  begin
    write("First twenty primes:");
    while primeNum < 20 do
      write(" " <& getPrime);
    end while;
    writeln;
    repeat
      aPrime := getPrime;
    until aPrime >= 100;
    write("Primes between 100 and 150:");
    while aPrime <= 150 do
      write(" " <& aPrime);
      aPrime := getPrime;
    end while;
    writeln;
    repeat
      aPrime := getPrime;
    until aPrime >= 7700;
    while aPrime <= 8000 do
      incr(count);
      aPrime := getPrime;
    end while;
    writeln("Number of primes between 7,700 and 8,000: " <& count);
    repeat
      aPrime := getPrime;
    until primeNum = 9999; # discard up to and including the 9,999 prime!
    writeln("The 10,000th prime: " <& getPrime);
  end func;
```


{{out}}

```txt

First twenty primes: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
Primes between 100 and 150: 101 103 107 109 113 127 131 137 139 149
Number of primes between 7,700 and 8,000: 30
The 10,000th prime: 104729

```



## Sidef


```ruby
say ("First 20: ", 20.nth_prime.primes.join(' '))
say ("Between 100 and 150: ", primes(100,150).join(' '))
say (prime_count(7700,8000), " primes between 7700 and 8000")
say ("10,000th prime: ", nth_prime(10_000))
```

{{out}}

```txt

First 20: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
Between 100 and 150: 101 103 107 109 113 127 131 137 139 149
30 primes between 7700 and 8000
10,000th prime: 104729

```



## Swift


The [[Sieve_of_Eratosthenes#Unbounded_.28Odds-Only.29_Versions]] Swift examples contain several versions that are Extensible Prime Generators using the Sieve of Eratosthenese.  One of the simplest is the Hashed Dictionary, reproduced here as follows:

```swift
import Foundation

func soeDictOdds() -> UnfoldSequence<Int, Int> {
  var bp = 5; var q = 25
  var bps: UnfoldSequence<Int, Int>.Iterator? = nil
  var dict = [9: 6] // Dictionary<Int, Int>(9 => 6)
  return sequence(state: 2, next: { n in
    if n < 9 { if n < 3 { n = 3; return 2 }; defer {n += 2}; return n }
    while n >= q || dict[n] != nil {
      if n >= q {
        let inc = bp + bp
        dict[n + inc] = inc
        if bps == nil {
          bps = soeDictOdds().makeIterator()
          bp = (bps?.next())!; bp = (bps?.next())!; bp = (bps?.next())! // skip 2/3/5...
        }
        bp = (bps?.next())!; q = bp * bp // guaranteed never nil
      } else {
        let inc = dict[n] ?? 0
        dict[n] = nil
        var next = n + inc
        while dict[next] != nil { next += inc }
        dict[next] = inc
      }
      n += 2
    }
    defer { n += 2 }; return n
  })
}

print("The first 20 primes are:  ", terminator: "")
soeDictOdds().lazy.prefix(20).forEach { print($0, "", terminator: "") }
print()

print("The primes between 100 and 150 are:  ", terminator: "")
soeDictOdds().lazy.drop(while: { $0 < Prime(100) }).lazy.prefix(while: { $0 <= 150 })
    .forEach { print($0, "", terminator: "") }
print()

print("The number of primes from 7700 to 8000 is  :", terminator: "")
print(soeDictOdds().lazy.drop(while: { $0 < 7700 }).lazy.prefix(while: { $0 <= 8000 })
        .lazy.reduce(0, { a, _ in a + 1 }))

print("The 10,000th prime is:  ", terminator: "")
print((soeDictOdds().lazy.dropFirst(9999).first { $0 == $0 })!)

print("The sum of primes to 2 million is:  ", terminator: "")

let start = NSDate()
let answr = soeDictOdds().lazy.prefix(while: { $0 <= 2000000 })
              .reduce(0, { a, p in a + Int64(p) })
let elpsd = -start.timeIntervalSinceNow

print(answr)
print(String(format: "This test took %.3f milliseconds.", elpsd * 1000))
```

{{output}}

```txt
The first 20 primes are:  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
The primes between 100 and 150 are:  101 103 107 109 113 127 131 137 139 149
The number of primes from 7700 to 8000 is  :30
The 10,000th prime is:  104729
The sum of primes to 2 million is:  142913828922
This test took 862.497 milliseconds.
```


The above version is somewhat slow due to the overhead of computing the hash for the dictionary, but useful up to ranges of a few million as in solving the Euler Problem of summing all primes up to two million as shown above.

'''Alternate much faster version using Page Segmentation with bit-packed arrays'''

The Page Segmentation algorithm as per the the last version of the Sieve of Eratosthenes in the same Swift Unbounded section as the above code can use enumeration to do the same thing but is about 50 times faster, taking an almost imperceptible time to solve Euler Problem 10 and can count the primes to a billion (even by enumeration) in a few seconds.  It can be used just by substituting `primesPaged()` for `soeDictOdds()` in the above testing code.


## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

# An iterative version of the Sieve of Eratosthenes.
# Effective limit is the size of memory.
coroutine primes apply {{} {
    yield
    while 1 {yield [coroutine primes_[incr p] apply {{} {
	yield [info coroutine]
	set plist {}
	for {set n 2} true {incr n} {
	    set found 0
	    foreach p $plist {
		if {$n%$p==0} {
		    set found 1
		    break
		}
	    }
	    if {!$found} {
		lappend plist $n
		yield $n
	    }
	}
    }}]}
}}

set p [primes]
for {set primes {}} {[llength $primes] < 20} {} {
    lappend primes [$p]
}
puts 1st20=[join $primes ,]
rename $p {}

set p [primes]
for {set primes {}} {[set n [$p]] <= 150} {} {
    if {$n >= 100 && $n <= 150} {
	lappend primes $n
    }
}
puts 100-150=[join $primes ,]
rename $p {}

set p [primes]
for {set count 0} {[set n [$p]] <= 8000} {} {
    incr count [expr {$n>=7700 && $n<=8000}]
}
puts count7700-8000=$count
rename $p {}

set p [primes]
for {set count 0} {$count < 10000} {incr count} {
    set prime [$p]
}
puts prime10000=$prime
rename $p {}
```

{{out}}

```txt

1st20=2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71
100-150=101,103,107,109,113,127,131,137,139,149
count7700-8000=30
prime10000=104729

```



## VBA


```vb
Option Explicit

Sub Main()
Dim Primes() As Long, n As Long, temp$
Dim t As Single
    t = Timer
    
    n = 133218295 'limit for an Array of Longs with VBA on my computer
    Primes = ListPrimes(n)
    Debug.Print "For N = " & Format(n, "#,##0") & ", execution time : " & _
        Format(Timer - t, "0.000 s") & ", " & _
        Format(UBound(Primes) + 1, "#,##0") & " primes numbers."
    
    'First twenty primes
    For n = 0 To 19
        temp = temp & ", " & Primes(n)
    Next
    Debug.Print "First twenty primes : "; Mid(temp, 3)
    'Primes between 100 and 150
    n = 0: temp = vbNullString
    Do While Primes(n) < 100
        n = n + 1
    Loop
    Do While Primes(n) < 150
        temp = temp & ", " & Primes(n)
        n = n + 1
    Loop
    Debug.Print "Primes between 100 and 150 : " & Mid(temp, 3)
    'Number of primes between 7,700 and 8,000
    Dim ccount As Long
    n = 0
    Do While Primes(n) < 7700
        n = n + 1
    Loop
    Do While Primes(n) < 8000
        ccount = ccount + 1
        n = n + 1
    Loop
    Debug.Print "Number of primes between 7,700 and 8,000 : " & ccount
    'The 10 x Xth prime
    n = 1
    Do While n <= 100000
        n = n * 10
        Debug.Print "The " & n & "th prime: "; Format(Primes(n - 1), "#,##0")
    Loop
    Debug.Print "VBA has a limit in array's dim"
    Debug.Print "With my computer, the limit for an array of Long is : 133 218 295"
    Debug.Print "The last prime I could find is the : " & _
        Format(UBound(Primes), "#,##0") & "th, Value : " & _
        Format(Primes(UBound(Primes)), "#,##0")
End Sub

Function ListPrimes(MAX As Long) As Long()
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
```

{{out}}

```txt
For N = 133 218 295, execution time : 9,422 s, 7 550 284 primes numbers.
First twenty primes : 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71
Primes between 100 and 150 : 101, 103, 107, 109, 113, 127, 131, 137, 139, 149
Number of primes between 7,700 and 8,000 : 30
The 10th prime: 29
The 100th prime: 541
The 1000th prime: 7 919
The 10000th prime: 104 729
The 100000th prime: 1 299 709
The 1000000th prime: 15 485 863
VBA has a limit in array's dim
With my computer, the limit for an array of Long is : 133 218 295
The last prime I could find is the : 7 550 283th, Value : 133 218 289
```



## zkl


```zkl
// http://stackoverflow.com/revisions/10733621/4

fcn postponed_sieve(){            # postponed sieve, by Will Ness      
   vm.yield(2); vm.yield(3);	  # original code David Eppstein, 
   vm.yield(5); vm.yield(7);      #        ActiveState Recipe 2002
   D:=Dictionary();               
   ps:=Utils.Generator(postponed_sieve);  # a separate Primes Supply:
   p:=ps.pump(2,Void);            # (3) a Prime to add to dict
   q:=p*p;                        # (9) when its sQuare is 
   c:=9;                          # the next Candidate
   while(1){
      if (not D.holds(c)){        # not a multiple of any prime seen so far:
         if (c < q) vm.yield(c);  #   a prime, or
	 else{   # (c==q):        #   the next prime's square:
            add(D,c + 2*p,2*p);   #     (9+6,6 : 15,21,27,33,...)
	    p=ps.next();          #     (5)
	    q=p*p;                #     (25)
	 }
      }else{                      # 'c' is a composite:
	 s := D.pop(c);           #   step of increment
	 add(D,c + s,s);          #   next multiple, same step
      }
      c += 2;                     # next odd candidate
   }
}

fcn add(D,x,s){                   # make no multiple keys in Dict
   while(D.holds(x)){ x += s }    # increment by the given step
   D[x] = s;
}
```


```zkl
primes:=Utils.Generator(postponed_sieve);
primes.walk(20).println();   // first 20 primes
 
primes.pump(List,fcn(p){  // the primes between 100 & 150
   if (p<100) Void.Skip else if(p>150) Void.Stop else p
}).println();

primes.reduce(fcn(n,p){  // count of primes between 7700 & 8000
   if (p<=7700) n else if(p>8000) Void.Stop else n+1
},0).println();

primes=Utils.Generator(postponed_sieve);	// new Generator
primes.drop(0d9_999); primes.next().println();  // 10,000th prime

   // or to carry on until the 100,000th:
primes.pump(Void,'wrap(p){ primes.n<=0d100_000 and p or Void.Stop }).println();
```

{{out}}

```txt

L(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71)
L(101,103,107,109,113,127,131,137,139,149)
30
104729
1299709

```


Using GMP (GNU Multiple Precision Arithmetic Library, probabilistic primes), this is a direct drop in for the above:
{{libheader|GMP}}

```zkl
var [const] BN=Import.lib("zklBigNum");  // libGMP
bigPrimes:=Walker(fcn(p){ p.nextPrime().copy(); }.fp(BN(1)));
```

For example:

```zkl
bigPrimes.walk(20).println();   // first 20 primes
bigPrimes.pump(Void,'wrap(p){ bigPrimes.n<=0d10_000 and p or Void.Stop }).println();
```

{{out}}

```txt

L(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71)
104729

```

