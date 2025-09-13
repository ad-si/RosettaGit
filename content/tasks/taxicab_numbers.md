+++
title = "Taxicab numbers"
description = ""
date = 2019-05-20T13:45:36Z
aliases = []
[extra]
id = 17373
[taxonomies]
categories = ["task", "Mathematics"]
tags = []
+++

[[File:taxi1729.png|600px||right]]

A   [[wp:Hardy–Ramanujan number|taxicab number]]   (the definition that is being used here)   is a positive integer that can be expressed as the sum of two positive cubes in more than one way.


The first taxicab number is   '''1729''',   which is:
::: 1<sup>3</sup>   +   12<sup>3</sup>       and
::: 9<sup>3</sup>   +   10<sup>3</sup>.


Taxicab numbers are also known as:
::*   taxi numbers
::*   taxi-cab numbers
::*   taxi cab numbers
::*   Hardy-Ramanujan numbers


## Task

* Compute and display the lowest 25 taxicab numbers (in numeric order, and in a human-readable format).
* For each of the taxicab numbers, show the number as well as it's constituent cubes.


;Extra credit
* Show the 2,000<sup>th</sup> taxicab number, and a half dozen more


## See also

* [http://oeis.org/A001235 A001235 taxicab numbers] on The On-Line Encyclopedia of Integer Sequences.
* [http://mathworld.wolfram.com/Hardy-RamanujanNumber.html Hardy-Ramanujan Number] on MathWorld.
* [http://mathworld.wolfram.com/TaxicabNumber.html taxicab number] on MathWorld.
* [https://en.wikipedia.org/wiki/Taxicab_number taxicab number] on Wikipedia.





## Befunge


This is quite slow in most interpreters, although a decent compiler should allow it to complete in a matter of seconds. Regardless of the speed, though, the range in a standard Befunge-93 implementation is limited to the first 64 numbers in the series, after which the 8-bit memory cells will overflow. That range could be extended in Befunge-98, but realistically you're not likely to wait that long for the results.


```befunge
v+1$$<_v#!`**::+1g42$$_v#<!`**::+1g43\g43::<<v,,.g42,<
>004p:0>1+24p:24g\:24g>>1+:34p::**24g::**+-|p>9,,,14v,
,,,"^3 + ^3= ^3 + ^3".\,,,9"= ".:\_v#g40g43<^v,,,,.g<^
5+,$$$\1+:38*`#@_\::"~"1+:24p34p0\0>14p24g04^>,04g.,,5
```


```txt
1729    = 10 ^3 + 9 ^3  = 12 ^3 + 1 ^3
4104    = 15 ^3 + 9 ^3  = 16 ^3 + 2 ^3
13832   = 20 ^3 + 18 ^3 = 24 ^3 + 2 ^3
20683   = 24 ^3 + 19 ^3 = 27 ^3 + 10 ^3
32832   = 30 ^3 + 18 ^3 = 32 ^3 + 4 ^3
39312   = 33 ^3 + 15 ^3 = 34 ^3 + 2 ^3
40033   = 33 ^3 + 16 ^3 = 34 ^3 + 9 ^3
46683   = 30 ^3 + 27 ^3 = 36 ^3 + 3 ^3
64232   = 36 ^3 + 26 ^3 = 39 ^3 + 17 ^3
65728   = 33 ^3 + 31 ^3 = 40 ^3 + 12 ^3
110656  = 40 ^3 + 36 ^3 = 48 ^3 + 4 ^3
110808  = 45 ^3 + 27 ^3 = 48 ^3 + 6 ^3
134379  = 43 ^3 + 38 ^3 = 51 ^3 + 12 ^3
149389  = 50 ^3 + 29 ^3 = 53 ^3 + 8 ^3
165464  = 48 ^3 + 38 ^3 = 54 ^3 + 20 ^3
171288  = 54 ^3 + 24 ^3 = 55 ^3 + 17 ^3
195841  = 57 ^3 + 22 ^3 = 58 ^3 + 9 ^3
216027  = 59 ^3 + 22 ^3 = 60 ^3 + 3 ^3
216125  = 50 ^3 + 45 ^3 = 60 ^3 + 5 ^3
262656  = 60 ^3 + 36 ^3 = 64 ^3 + 8 ^3
314496  = 66 ^3 + 30 ^3 = 68 ^3 + 4 ^3
320264  = 66 ^3 + 32 ^3 = 68 ^3 + 18 ^3
327763  = 58 ^3 + 51 ^3 = 67 ^3 + 30 ^3
373464  = 60 ^3 + 54 ^3 = 72 ^3 + 6 ^3
402597  = 61 ^3 + 56 ^3 = 69 ^3 + 42 ^3
```



## C

Using a priority queue to emit sum of two cubs in order. It's reasonably fast and doesn't use excessive amount of memory (the heap is only at 245 length upon the 2006th taxi).

```c
#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long xint;
typedef unsigned uint;
typedef struct {
	uint x, y; // x > y always
	xint value;
} sum_t;

xint *cube;
uint n_cubes;

sum_t *pq;
uint pq_len, pq_cap;

void add_cube(void)
{
	uint x = n_cubes++;
	cube = realloc(cube, sizeof(xint) * (n_cubes + 1));
	cube[n_cubes] = (xint) n_cubes*n_cubes*n_cubes;
	if (x < 2) return; // x = 0 or 1 is useless

	if (++pq_len >= pq_cap) {
		if (!(pq_cap *= 2)) pq_cap = 2;
		pq = realloc(pq, sizeof(*pq) * pq_cap);
	}

	sum_t tmp = (sum_t) { x, 1, cube[x] + 1 };
	// upheap
	uint i, j;
	for (i = pq_len; i >= 1 && pq[j = i>>1].value > tmp.value; i = j)
		pq[i] = pq[j];

	pq[i] = tmp;
}

void next_sum(void)
{
redo:	while (!pq_len || pq[1].value >= cube[n_cubes]) add_cube();

	sum_t tmp = pq[0] = pq[1];	// pq[0] always stores last seen value
	if (++tmp.y >= tmp.x) {		// done with this x; throw it away
		tmp = pq[pq_len--];
		if (!pq_len) goto redo;	// refill empty heap
	} else
		tmp.value += cube[tmp.y] - cube[tmp.y-1];

	uint i, j;
	// downheap
	for (i = 1; (j = i<<1) <= pq_len; pq[i] = pq[j], i = j) {
		if (j < pq_len && pq[j+1].value < pq[j].value) ++j;
		if (pq[j].value >= tmp.value) break;
	}
	pq[i] = tmp;
}

uint next_taxi(sum_t *hist)
{
	do next_sum(); while (pq[0].value != pq[1].value);

	uint len = 1;
	hist[0] = pq[0];
	do {
		hist[len++] = pq[1];
		next_sum();
	} while (pq[0].value == pq[1].value);

	return len;
}

int main(void)
{
	uint i, l;
	sum_t x[10];
	for (i = 1; i <= 2006; i++) {
		l = next_taxi(x);
		if (25 < i && i < 2000) continue;
		printf("%4u:%10llu", i, x[0].value);
		while (l--) printf(" = %4u^3 + %4u^3", x[l].x, x[l].y);
		putchar('\n');
	}
	return 0;
}
```

```txt

   1:      1729 =   12^3 +    1^3 =   10^3 +    9^3
   2:      4104 =   15^3 +    9^3 =   16^3 +    2^3
   3:     13832 =   20^3 +   18^3 =   24^3 +    2^3
   4:     20683 =   27^3 +   10^3 =   24^3 +   19^3
   5:     32832 =   30^3 +   18^3 =   32^3 +    4^3
   6:     39312 =   33^3 +   15^3 =   34^3 +    2^3
   7:     40033 =   33^3 +   16^3 =   34^3 +    9^3
   8:     46683 =   30^3 +   27^3 =   36^3 +    3^3
   9:     64232 =   36^3 +   26^3 =   39^3 +   17^3
  10:     65728 =   33^3 +   31^3 =   40^3 +   12^3
  11:    110656 =   40^3 +   36^3 =   48^3 +    4^3
  12:    110808 =   45^3 +   27^3 =   48^3 +    6^3
  13:    134379 =   43^3 +   38^3 =   51^3 +   12^3
  14:    149389 =   50^3 +   29^3 =   53^3 +    8^3
  15:    165464 =   48^3 +   38^3 =   54^3 +   20^3
  16:    171288 =   54^3 +   24^3 =   55^3 +   17^3
  17:    195841 =   57^3 +   22^3 =   58^3 +    9^3
  18:    216027 =   59^3 +   22^3 =   60^3 +    3^3
  19:    216125 =   50^3 +   45^3 =   60^3 +    5^3
  20:    262656 =   60^3 +   36^3 =   64^3 +    8^3
  21:    314496 =   66^3 +   30^3 =   68^3 +    4^3
  22:    320264 =   66^3 +   32^3 =   68^3 +   18^3
  23:    327763 =   58^3 +   51^3 =   67^3 +   30^3
  24:    373464 =   60^3 +   54^3 =   72^3 +    6^3
  25:    402597 =   61^3 +   56^3 =   69^3 +   42^3
2000:1671816384 = 1168^3 +  428^3 =  944^3 +  940^3
2001:1672470592 = 1124^3 +  632^3 = 1187^3 +   29^3
2002:1673170856 = 1034^3 +  828^3 = 1164^3 +  458^3
2003:1675045225 = 1153^3 +  522^3 = 1081^3 +  744^3
2004:1675958167 = 1096^3 +  711^3 = 1159^3 +  492^3
2005:1676926719 = 1188^3 +   63^3 = 1095^3 +  714^3
2006:1677646971 =  990^3 +  891^3 = 1188^3 +   99^3

```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace TaxicabNumber
{
    class Program
    {
        static void Main(string[] args)
        {
            IDictionary<long, IList<Tuple<int, int>>> taxicabNumbers = GetTaxicabNumbers(2006);
            PrintTaxicabNumbers(taxicabNumbers);
            Console.ReadKey();
        }

        private static IDictionary<long, IList<Tuple<int, int>>> GetTaxicabNumbers(int length)
        {
            SortedList<long, IList<Tuple<int, int>>> sumsOfTwoCubes = new SortedList<long, IList<Tuple<int, int>>>();

            for (int i = 1; i < int.MaxValue; i++)
            {
                for (int j = 1; j < int.MaxValue; j++)
                {
                    long sum = (long)(Math.Pow((double)i, 3) + Math.Pow((double)j, 3));

                    if (!sumsOfTwoCubes.ContainsKey(sum))
                    {
                        sumsOfTwoCubes.Add(sum, new List<Tuple<int, int>>());
                    }

                    sumsOfTwoCubes[sum].Add(new Tuple<int, int>(i, j));

                    if (j >= i)
                    {
                        break;
                    }
                }

                // Found that you need to keep going for a while after the length, because higher i values fill in gaps
                if (sumsOfTwoCubes.Count(t => t.Value.Count >= 2) >= length * 1.1)
                {
                    break;
                }
            }

            IDictionary<long, IList<Tuple<int, int>>> values = (from t in sumsOfTwoCubes where t.Value.Count >= 2 select t)
                .Take(2006)
                .ToDictionary(u => u.Key, u => u.Value);

            return values;
        }

        private static void PrintTaxicabNumbers(IDictionary<long, IList<Tuple<int, int>>> values)
        {
            int i = 1;

            foreach (long taxicabNumber in values.Keys)
            {
                StringBuilder output = new StringBuilder().AppendFormat("{0,10}\t{1,4}", i, taxicabNumber);

                foreach (Tuple<int, int> numbers in values[taxicabNumber])
                {
                    output.AppendFormat("\t= {0}^3 + {1}^3", numbers.Item1, numbers.Item2);
                }

                if (i <= 25 || (i >= 2000 && i <= 2006))
                {
                    Console.WriteLine(output.ToString());
                }

                i++;
            }
        }
    }
}
```



## Clojure


```clojure
(ns test-project-intellij.core
  (:gen-class))

(defn cube [x]
  "Cube a number through triple multiplication"
  (* x x x))

(defn sum3 [[i j]]
   " [i j] -> i^3 + j^3"
  (+ (cube i) (cube j)))

(defn next-pair [[i j]]
  " Generate next [i j] pair of sequence  (producing lower triangle pairs) "
  (if (< j i)
    [i (inc j)]
    [(inc i) 1]))

;; Pair sequence generator [1 1] [2 1] [2 2] [3 1] [3 2] [3 3] ...
(def pairs-seq	(iterate next-pair [1 1]))

(defn dict-inc [m pair]
  " Add pair to pair map m, with the key of the map based upon the cubic sum (sum3) and the value appends the pair "
  (update-in m [(sum3 pair)] (fnil #(conj % pair) [])))

(defn enough? [m n-to-generate]
  " Checks if we have enough taxi numbers (i.e. if number in map >= count-needed "
  (->> m                                ; hash-map of sum of cube of numbers [key] and their pairs as value
       (filter #(if (> (count (second %)) 1) true false))   ; filter out ones which don't have more than 1 entry
       (count)                                              ; count the item remaining
       (<= n-to-generate)))                                ; true iff count-needed is less or equal to the nubmer filtered

(defn find-taxi-numbers [n-to-generate]
  " Generates 1st n-to-generate taxi numbers"
  (loop [m {}               ; Hash-map containing cube of pairs (key) and set of pairs that produce sum (value)
         p pairs-seq        ; select pairs from our pair sequence generator (i.e. [1 1] [2 1] [2 2] ...)
         num-tried 0        ; Since its expensve to count how many taxi numbers we have found
         check-after 1]     ; we only check if we have enough numbers every time (num-tried equals check-after)
                            ; num-tried increments by 1 each time we try the next pair and
                            ; check-after doubles if we don't have enough taxi numbers
    (if (and (= num-tried check-after) (enough? m n-to-generate)) ; check if we found enough taxi numbers
      (sort-by first (into [] (filter #(> (count (second %)) 1) m)))  ; sort the taxi numbers and this is the result
      (if (= num-tried check-after)                                   ; Check if we need to increase our count between checking
        (recur (dict-inc m (first p)) (rest p) (inc num-tried) (* 2 check-after))   ; increased count between checking
        (recur (dict-inc m (first p)) (rest p) (inc num-tried) check-after)))))     ; didn't increase the count

; Generate 1st 2006 taxi numbers
(def result (find-taxi-numbers 2006))

;; Show First 25
(defn show-result [n sample]
  " Prints one line of result "
  (print (format "%4d:%10d" n  (first sample)))
  (doseq [q  (second sample)
          :let [[i j] q]]
      (print (format " = %4d^3 + %4d^3" i j)))
  (println))

; 1st 25 taxi numbers
(doseq [n (range 1 26)
        :let [sample (nth result (dec n))]]
  (show-result n sample))

; taxi numbers from 2000th to 2006th
(doseq [n (range 2000 2007)
        :let [sample (nth result (dec n))]]
  (show-result n sample))

}
```


```txt

  1:      1729 =   10^3 +    9^3 =   12^3 +    1^3
   2:      4104 =   15^3 +    9^3 =   16^3 +    2^3
   3:     13832 =   20^3 +   18^3 =   24^3 +    2^3
   4:     20683 =   24^3 +   19^3 =   27^3 +   10^3
   5:     32832 =   30^3 +   18^3 =   32^3 +    4^3
   6:     39312 =   33^3 +   15^3 =   34^3 +    2^3
   7:     40033 =   33^3 +   16^3 =   34^3 +    9^3
   8:     46683 =   30^3 +   27^3 =   36^3 +    3^3
   9:     64232 =   36^3 +   26^3 =   39^3 +   17^3
  10:     65728 =   33^3 +   31^3 =   40^3 +   12^3
  11:    110656 =   40^3 +   36^3 =   48^3 +    4^3
  12:    110808 =   45^3 +   27^3 =   48^3 +    6^3
  13:    134379 =   43^3 +   38^3 =   51^3 +   12^3
  14:    149389 =   50^3 +   29^3 =   53^3 +    8^3
  15:    165464 =   48^3 +   38^3 =   54^3 +   20^3
  16:    171288 =   54^3 +   24^3 =   55^3 +   17^3
  17:    195841 =   57^3 +   22^3 =   58^3 +    9^3
  18:    216027 =   59^3 +   22^3 =   60^3 +    3^3
  19:    216125 =   50^3 +   45^3 =   60^3 +    5^3
  20:    262656 =   60^3 +   36^3 =   64^3 +    8^3
  21:    314496 =   66^3 +   30^3 =   68^3 +    4^3
  22:    320264 =   66^3 +   32^3 =   68^3 +   18^3
  23:    327763 =   58^3 +   51^3 =   67^3 +   30^3
  24:    373464 =   60^3 +   54^3 =   72^3 +    6^3
  25:    402597 =   61^3 +   56^3 =   69^3 +   42^3
2000:1671816384 =  944^3 +  940^3 = 1168^3 +  428^3
2001:1672470592 = 1124^3 +  632^3 = 1187^3 +   29^3
2002:1673170856 = 1034^3 +  828^3 = 1164^3 +  458^3
2003:1675045225 = 1081^3 +  744^3 = 1153^3 +  522^3
2004:1675958167 = 1096^3 +  711^3 = 1159^3 +  492^3
2005:1676926719 = 1095^3 +  714^3 = 1188^3 +   63^3
2006:1677646971 =  990^3 +  891^3 = 1188^3 +   99^3

```



## D


### High Level Version

```d
void main() /*@safe*/ {
    import std.stdio, std.range, std.algorithm, std.typecons, std.string;

    auto iCubes = iota(1u, 1201u).map!(x => tuple(x, x ^^ 3));
    bool[Tuple!(uint, uint)][uint] sum2cubes;
    foreach (i, immutable i3; iCubes)
        foreach (j, immutable j3; iCubes[i .. $])
            sum2cubes[i3 + j3][tuple(i, j)] = true;

    const taxis = sum2cubes.byKeyValue.filter!(p => p.value.length > 1)
                  .array.schwartzSort!(p => p.key).release;

    foreach (/*immutable*/ const r; [[0, 25], [2000 - 1, 2000 + 6]]) {
        foreach (immutable i, const t; taxis[r[0] .. r[1]])
            writefln("%4d: %10d =%-(%s =%)", i + r[0] + 1, t.key,
                     t.value.keys.sort().map!q{"%4d^3 + %4d^3".format(a[])});
        writeln;
    }
}
```

```txt
   1:       1729 =   1^3 +   12^3 =   9^3 +   10^3
   2:       4104 =   2^3 +   16^3 =   9^3 +   15^3
   3:      13832 =   2^3 +   24^3 =  18^3 +   20^3
   4:      20683 =  10^3 +   27^3 =  19^3 +   24^3
   5:      32832 =   4^3 +   32^3 =  18^3 +   30^3
   6:      39312 =   2^3 +   34^3 =  15^3 +   33^3
   7:      40033 =   9^3 +   34^3 =  16^3 +   33^3
   8:      46683 =   3^3 +   36^3 =  27^3 +   30^3
   9:      64232 =  17^3 +   39^3 =  26^3 +   36^3
  10:      65728 =  12^3 +   40^3 =  31^3 +   33^3
  11:     110656 =   4^3 +   48^3 =  36^3 +   40^3
  12:     110808 =   6^3 +   48^3 =  27^3 +   45^3
  13:     134379 =  12^3 +   51^3 =  38^3 +   43^3
  14:     149389 =   8^3 +   53^3 =  29^3 +   50^3
  15:     165464 =  20^3 +   54^3 =  38^3 +   48^3
  16:     171288 =  17^3 +   55^3 =  24^3 +   54^3
  17:     195841 =   9^3 +   58^3 =  22^3 +   57^3
  18:     216027 =   3^3 +   60^3 =  22^3 +   59^3
  19:     216125 =   5^3 +   60^3 =  45^3 +   50^3
  20:     262656 =   8^3 +   64^3 =  36^3 +   60^3
  21:     314496 =   4^3 +   68^3 =  30^3 +   66^3
  22:     320264 =  18^3 +   68^3 =  32^3 +   66^3
  23:     327763 =  30^3 +   67^3 =  51^3 +   58^3
  24:     373464 =   6^3 +   72^3 =  54^3 +   60^3
  25:     402597 =  42^3 +   69^3 =  56^3 +   61^3

2000: 1671816384 = 428^3 + 1168^3 = 940^3 +  944^3
2001: 1672470592 =  29^3 + 1187^3 = 632^3 + 1124^3
2002: 1673170856 = 458^3 + 1164^3 = 828^3 + 1034^3
2003: 1675045225 = 522^3 + 1153^3 = 744^3 + 1081^3
2004: 1675958167 = 492^3 + 1159^3 = 711^3 + 1096^3
2005: 1676926719 =  63^3 + 1188^3 = 714^3 + 1095^3
2006: 1677646971 =  99^3 + 1188^3 = 891^3 +  990^3
```

Run-time: about 2.9 seconds with dmd compiler.

===Heap-Based Version===
```d
import std.stdio, std.string, std.container;

struct CubeSum {
    ulong x, y, value;

    this(in ulong x_, in ulong y_) pure nothrow @safe @nogc {
        this.x = x_;
        this.y = y_;
        this.value = x_ ^^ 3 + y_ ^^ 3;
    }
}

final class Taxi {
    BinaryHeap!(Array!CubeSum, "a.value > b.value") pq;
    CubeSum last;
    ulong n = 0;

    this() {
        last = nextSum();
    }

    CubeSum nextSum() {
        while (pq.empty || pq.front.value >= n ^^ 3)
            pq.insert(CubeSum(++n, 1));

        auto s = pq.front;
        pq.removeFront;
        if (s.x > s.y + 1)
            pq.insert(CubeSum(s.x, s.y + 1));

        return s;
    }

    CubeSum[] nextTaxi() {
        CubeSum s;
        typeof(return) train;

        while ((s = nextSum).value != last.value)
            last = s;

        train ~= last;

        do {
            train ~= s;
        } while ((s = nextSum).value == last.value);
        last = s;

        return train;
    }
}

void main() {
    auto taxi = new Taxi;

    foreach (immutable i; 1 .. 2007) {
        const t = taxi.nextTaxi;
        if (i > 25 && i < 2000)
            continue;

        writef("%4d: %10d", i, t[0].value);
        foreach (const s; t)
            writef(" = %4d^3 + %4d^3", s.x, s.y);
        writeln;
    }
}
```

```txt
   1:       1729 =   10^3 +    9^3 =   12^3 +    1^3
   2:       4104 =   15^3 +    9^3 =   16^3 +    2^3
   3:      13832 =   20^3 +   18^3 =   24^3 +    2^3
   4:      20683 =   24^3 +   19^3 =   27^3 +   10^3
   5:      32832 =   30^3 +   18^3 =   32^3 +    4^3
   6:      39312 =   33^3 +   15^3 =   34^3 +    2^3
   7:      40033 =   33^3 +   16^3 =   34^3 +    9^3
   8:      46683 =   30^3 +   27^3 =   36^3 +    3^3
   9:      64232 =   39^3 +   17^3 =   36^3 +   26^3
  10:      65728 =   40^3 +   12^3 =   33^3 +   31^3
  11:     110656 =   40^3 +   36^3 =   48^3 +    4^3
  12:     110808 =   45^3 +   27^3 =   48^3 +    6^3
  13:     134379 =   51^3 +   12^3 =   43^3 +   38^3
  14:     149389 =   50^3 +   29^3 =   53^3 +    8^3
  15:     165464 =   48^3 +   38^3 =   54^3 +   20^3
  16:     171288 =   54^3 +   24^3 =   55^3 +   17^3
  17:     195841 =   57^3 +   22^3 =   58^3 +    9^3
  18:     216027 =   59^3 +   22^3 =   60^3 +    3^3
  19:     216125 =   50^3 +   45^3 =   60^3 +    5^3
  20:     262656 =   60^3 +   36^3 =   64^3 +    8^3
  21:     314496 =   66^3 +   30^3 =   68^3 +    4^3
  22:     320264 =   68^3 +   18^3 =   66^3 +   32^3
  23:     327763 =   67^3 +   30^3 =   58^3 +   51^3
  24:     373464 =   60^3 +   54^3 =   72^3 +    6^3
  25:     402597 =   69^3 +   42^3 =   61^3 +   56^3
2000: 1671816384 = 1168^3 +  428^3 =  944^3 +  940^3
2001: 1672470592 = 1124^3 +  632^3 = 1187^3 +   29^3
2002: 1673170856 = 1164^3 +  458^3 = 1034^3 +  828^3
2003: 1675045225 = 1153^3 +  522^3 = 1081^3 +  744^3
2004: 1675958167 = 1159^3 +  492^3 = 1096^3 +  711^3
2005: 1676926719 = 1095^3 +  714^3 = 1188^3 +   63^3
2006: 1677646971 =  990^3 +  891^3 = 1188^3 +   99^3
```

Run-time: about 0.31 seconds with ldc2 compiler. It's faster than the Java solution.

===Low Level Heap-Based Version===
```d
struct Taxicabs {
    alias CubesSumT = uint; // Or ulong.

    static struct Sum {
        CubesSumT value;
        uint x, y;
    }

    // The cubes can be pre-computed if CubesSumT is a BigInt.
    private uint nCubes;
    private Sum[] pq;
    private uint pq_len;

    private void addCube() pure nothrow @safe {
        nCubes = nCubes ? nCubes + 1 : 2;
        if (nCubes < 2)
            return; // 0 or 1 is useless.

        pq_len++;
        if (pq_len >= pq.length)
            pq.length = (pq.length == 0) ? 2 : (pq.length * 2);

        immutable tmp = Sum(CubesSumT(nCubes - 2) ^^ 3 + 1,
                            nCubes - 2, 1);

        // Upheap.
        uint i = pq_len;
        for (; i >= 1 && pq[i >> 1].value > tmp.value; i >>= 1)
            pq[i] = pq[i >> 1];

        pq[i] = tmp;
    }


    private void nextSum() pure nothrow @safe {
        while (!pq_len || pq[1].value >= (nCubes - 1) ^^ 3)
            addCube();

        Sum tmp = pq[0] = pq[1]; //pq[0] always stores last seen value.
        tmp.y++;
        if (tmp.y >= tmp.x) { // Done with this x; throw it away.
            tmp = pq[pq_len];
            pq_len--;
            if (!pq_len)
                return nextSum(); // Refill empty heap.
        } else
            tmp.value += tmp.y ^^ 3 - (tmp.y - 1) ^^ 3;

        // Downheap.
        uint i = 1;
        while (true) {
            uint j = i << 1;
            if (j > pq_len)
                break;
            if (j < pq_len && pq[j + 1].value < pq[j].value)
                j++;
            if (pq[j].value >= tmp.value)
                break;
            pq[i] = pq[j];
            i = j;
        }

        pq[i] = tmp;
    }


    Sum[] nextTaxi(size_t N)(ref Sum[N] hist)
    pure nothrow @safe {
        do {
            nextSum();
        } while (pq[0].value != pq[1].value);

        uint len = 1;
        hist[0] = pq[0];
        do {
            hist[len] = pq[1];
            len++;
            nextSum();
        } while (pq[0].value == pq[1].value);

        return hist[0 .. len];
    }
}


void main() nothrow {
    import core.stdc.stdio;

    Taxicabs t;
    Taxicabs.Sum[3] x;

    foreach (immutable uint i; 1 .. 2007) {
        const triples = t.nextTaxi(x);
        if (i > 25 && i < 2000)
            continue;
        printf("%4u: %10lu", i, triples[0].value);
        foreach_reverse (const s; triples)
            printf(" = %4u^3 + %4u^3", s.x, s.y);
        '\n'.putchar;
    }
}
```

```txt
   1:       1729 =   12^3 +    1^3 =   10^3 +    9^3
   2:       4104 =   15^3 +    9^3 =   16^3 +    2^3
   3:      13832 =   20^3 +   18^3 =   24^3 +    2^3
   4:      20683 =   27^3 +   10^3 =   24^3 +   19^3
   5:      32832 =   30^3 +   18^3 =   32^3 +    4^3
   6:      39312 =   33^3 +   15^3 =   34^3 +    2^3
   7:      40033 =   33^3 +   16^3 =   34^3 +    9^3
   8:      46683 =   30^3 +   27^3 =   36^3 +    3^3
   9:      64232 =   36^3 +   26^3 =   39^3 +   17^3
  10:      65728 =   33^3 +   31^3 =   40^3 +   12^3
  11:     110656 =   40^3 +   36^3 =   48^3 +    4^3
  12:     110808 =   45^3 +   27^3 =   48^3 +    6^3
  13:     134379 =   43^3 +   38^3 =   51^3 +   12^3
  14:     149389 =   50^3 +   29^3 =   53^3 +    8^3
  15:     165464 =   48^3 +   38^3 =   54^3 +   20^3
  16:     171288 =   54^3 +   24^3 =   55^3 +   17^3
  17:     195841 =   57^3 +   22^3 =   58^3 +    9^3
  18:     216027 =   59^3 +   22^3 =   60^3 +    3^3
  19:     216125 =   50^3 +   45^3 =   60^3 +    5^3
  20:     262656 =   60^3 +   36^3 =   64^3 +    8^3
  21:     314496 =   66^3 +   30^3 =   68^3 +    4^3
  22:     320264 =   66^3 +   32^3 =   68^3 +   18^3
  23:     327763 =   58^3 +   51^3 =   67^3 +   30^3
  24:     373464 =   60^3 +   54^3 =   72^3 +    6^3
  25:     402597 =   61^3 +   56^3 =   69^3 +   42^3
2000: 1671816384 = 1168^3 +  428^3 =  944^3 +  940^3
2001: 1672470592 = 1124^3 +  632^3 = 1187^3 +   29^3
2002: 1673170856 = 1034^3 +  828^3 = 1164^3 +  458^3
2003: 1675045225 = 1153^3 +  522^3 = 1081^3 +  744^3
2004: 1675958167 = 1096^3 +  711^3 = 1159^3 +  492^3
2005: 1676926719 = 1188^3 +   63^3 = 1095^3 +  714^3
2006: 1677646971 =  990^3 +  891^3 = 1188^3 +   99^3
```

Run-time: about 0.08 seconds with ldc2 compiler.


## DCL

We invoke external utility SORT which I suppose technically speaking is not a formal part of the language but is darn handy at times;

```DCL
$ close /nolog sums_of_cubes
$ on control_y then $ goto clean
$ open /write sums_of_cubes sums_of_cubes.txt
$ i = 1
$ loop1:
$  write sys$output i
$  j = 1
$  loop2:
$   sum = i * i * i + j * j * j
$   if sum .lt. 0
$   then
$    write sys$output "overflow at ", j
$    goto next_i
$   endif
$   write sums_of_cubes f$fao( "!10SL,!10SL,!10SL", sum, i, j )
$   j = j + 1
$   if j .le. i then $ goto loop2
$ next_i:
$  i = i + 1
$  if i .le. 1289 then $ goto loop1  ! cube_root of 2^31-1
$ close sums_of_cubes
$ sort sums_of_cubes.txt sorted_sums_of_cubes.txt
$ close /nolog sorted_sums_of_cubes
$ open sorted_sums_of_cubes sorted_sums_of_cubes.txt
$ count = 0
$ read sorted_sums_of_cubes prev_prev_line  ! need to detect when there are more than just 2 different sums, e.g. 456
$ prev_prev_sum = f$element( 0, ",", f$edit( prev_prev_line, "collapse" ))
$ read sorted_sums_of_cubes prev_line
$ prev_sum = f$element( 0,",", f$edit( prev_line, "collapse" ))
$ loop3:
$  read /end_of_file = done sorted_sums_of_cubes line
$  sum = f$element( 0, ",", f$edit( line, "collapse" ))
$  if sum .eqs. prev_sum
$  then
$   if sum .nes. prev_prev_sum then $ count = count + 1
$   int_sum = f$integer( sum )
$   i1 = f$integer( f$element( 1, ",", prev_line ))
$   j1 = f$integer( f$element( 2, ",", prev_line ))
$   i2 = f$integer( f$element( 1, ",", line ))
$   j2 = f$integer( f$element( 2, ",", line ))
$   if count .le. 25 .or. ( count .ge. 2000 .and. count .le. 2006 ) then -
$    write sys$output f$fao( "!4SL:!11SL =!5SL^3 +!5SL^3 =!5SL^3 +!5SL^3", count, int_sum, i1, j1, i2, j2 )
$  endif
$  prev_prev_line = prev_line
$  prev_prev_sum = prev_sum
$  prev_line = line
$  prev_sum = sum
$  goto loop3
$ done:
$ close sorted_sums_of_cubes
$ exit
$
$ clean:
$ close /nolog sorted_sums_of_cubes
$ close /nolog sums_of_cubes
```

```txt
$ @taxicab_numbers
   1:       1729 =   10^3 +    9^3 =   12^3 +    1^3
   2:       4104 =   15^3 +    9^3 =   16^3 +    2^3
   3:      13832 =   20^3 +   18^3 =   24^3 +    2^3
   4:      20683 =   24^3 +   19^3 =   27^3 +   10^3
   5:      32832 =   30^3 +   18^3 =   32^3 +    4^3
   6:      39312 =   33^3 +   15^3 =   34^3 +    2^3
   7:      40033 =   33^3 +   16^3 =   34^3 +    9^3
   8:      46683 =   30^3 +   27^3 =   36^3 +    3^3
   9:      64232 =   36^3 +   26^3 =   39^3 +   17^3
  10:      65728 =   33^3 +   31^3 =   40^3 +   12^3
  11:     110656 =   40^3 +   36^3 =   48^3 +    4^3
  12:     110808 =   45^3 +   27^3 =   48^3 +    6^3
  13:     134379 =   43^3 +   38^3 =   51^3 +   12^3
  14:     149389 =   50^3 +   29^3 =   53^3 +    8^3
  15:     165464 =   48^3 +   38^3 =   54^3 +   20^3
  16:     171288 =   54^3 +   24^3 =   55^3 +   17^3
  17:     195841 =   57^3 +   22^3 =   58^3 +    9^3
  18:     216027 =   59^3 +   22^3 =   60^3 +    3^3
  19:     216125 =   50^3 +   45^3 =   60^3 +    5^3
  20:     262656 =   60^3 +   36^3 =   64^3 +    8^3
  21:     314496 =   66^3 +   30^3 =   68^3 +    4^3
  22:     320264 =   66^3 +   32^3 =   68^3 +   18^3
  23:     327763 =   58^3 +   51^3 =   67^3 +   30^3
  24:     373464 =   60^3 +   54^3 =   72^3 +    6^3
  25:     402597 =   61^3 +   56^3 =   69^3 +   42^3
2000: 1671816384 =  944^3 +  940^3 = 1168^3 +  428^3
2001: 1672470592 = 1124^3 +  632^3 = 1187^3 +   29^3
2002: 1673170856 = 1034^3 +  828^3 = 1164^3 +  458^3
2003: 1675045225 = 1081^3 +  744^3 = 1153^3 +  522^3
2004: 1675958167 = 1096^3 +  711^3 = 1159^3 +  492^3
2005: 1676926719 = 1095^3 +  714^3 = 1188^3 +   63^3
2006: 1677646971 =  990^3 +  891^3 = 1188^3 +   99^3
```



## EchoLisp

Using the '''heap''' library, and a heap to store the taxicab numbers. For taxi tuples - decomposition in more than two sums - we use the '''group''' function which transforms a list ( 3 5 5 6 8 ...) into ((3) (5 5) (6) ...).

```scheme

(require '(heap compile))

(define (scube a b) (+ (* a a a) (* b b b)))
(compile 'scube "-f") ; "-f" means : no bigint, no rational used

;; is n - a^3 a cube  b^3?
;; if yes return b, else #f
(define (taxi? n a (b 0))
	(set! b (cbrt (- n (* a a a)))) ;; cbrt is ∛
	(when (and (< b a) (integer? b)) b))
(compile 'taxi? "-f")

#|-------------------
looking for taxis
--------------------|#
;; remove from heap until heap-top >= a
;; when twins are removed, it is a taxicab number : push it
;; at any time (top stack) = last removed

(define (clean-taxi H limit: a  min-of-heap: htop)
		(when (and htop (> a htop))
				(when (!= (stack-top S) htop) (pop S))
				(push S htop)
				(heap-pop H)
				(clean-taxi H  a (heap-top H))))
(compile 'clean-taxi "-f")

;; loop on a and b, b <=a , until n taxicabs found
(define (taxicab (n 2100))
	(for ((a (in-naturals)))
		(clean-taxi H (* a a a) (heap-top H))
		#:break (> (stack-length S) n)
		(for ((b a))
			(heap-push H (scube a b)))))

#|------------------
printing taxis
---------------------|#
;; string of all decompositions
(define (taxi->string i n)
	(string-append (format "%d. %d " (1+ i) n)
	(for/string ((a (cbrt n)))
		#:when (taxi? n a)
		(format " = %4d^3 + %4d^3" a (taxi? n a)))))

(define (taxi-print taxis (nfrom 0) (nto 26))
		(for ((i (in-naturals nfrom)) (taxi (sublist taxis nfrom nto)))
		(writeln (taxi->string i (first taxi)))))

```


```scheme

(define S (stack 'S)) ;; to push taxis
(define H (make-heap < )) ;; make min heap of all scubes

(taxicab 2100)
(define taxis (group (stack->list S)))
(taxi-print taxis )

1. 1729 =   10^3 +    9^3 =   12^3 +    1^3
2. 4104 =   15^3 +    9^3 =   16^3 +    2^3
3. 13832 =   20^3 +   18^3 =   24^3 +    2^3
4. 20683 =   24^3 +   19^3 =   27^3 +   10^3
#| ... |#
24. 373464 =   60^3 +   54^3 =   72^3 +    6^3
25. 402597 =   61^3 +   56^3 =   69^3 +   42^3
26. 439101 =   69^3 +   48^3 =   76^3 +    5^3

(taxi-print taxis 1999 2006)
2000. 1671816384 = 944^3 + 940^3 = 1168^3 + 428^3
2001. 1672470592 = 1124^3 + 632^3 = 1187^3 +   29^3
2002. 1673170856 = 1034^3 + 828^3 = 1164^3 + 458^3
2003. 1675045225 = 1081^3 + 744^3 = 1153^3 + 522^3
2004. 1675958167 = 1096^3 + 711^3 = 1159^3 + 492^3
2005. 1676926719 = 1095^3 + 714^3 = 1188^3 +   63^3
2006. 1677646971 = 990^3 + 891^3 = 1188^3 +   99^3

;; extra bonus : print all taxis which are triplets
(define (taxi-tuples taxis (nfrom 0) (nto 2000))
		(for ((i (in-naturals nfrom)) (taxi (sublist taxis nfrom nto)))
		#:when (> (length taxi) 1) ;; filter for tuples is here
		(writeln (taxi->string i (first taxi)))))

(taxi-tuples taxis)

455. 87539319 = 414^3 + 255^3 = 423^3 + 228^3 = 436^3 + 167^3
535. 119824488 = 428^3 + 346^3 = 492^3 +   90^3 = 493^3 +   11^3
588. 143604279 = 423^3 + 408^3 = 460^3 + 359^3 = 522^3 + 111^3
655. 175959000 = 525^3 + 315^3 = 552^3 + 198^3 = 560^3 +   70^3
888. 327763000 = 580^3 + 510^3 = 661^3 + 339^3 = 670^3 + 300^3
1299. 700314552 = 828^3 + 510^3 = 846^3 + 456^3 = 872^3 + 334^3
1398. 804360375 = 920^3 + 295^3 = 927^3 + 198^3 = 930^3 +   15^3
1515. 958595904 = 856^3 + 692^3 = 984^3 + 180^3 = 986^3 +   22^3
1660. 1148834232 = 846^3 + 816^3 = 920^3 + 718^3 = 1044^3 + 222^3
1837. 1407672000 = 1050^3 + 630^3 = 1104^3 + 396^3 = 1120^3 + 140^3

```



## Elixir


```elixir
defmodule Taxicab do
  def numbers(n \\ 1200) do
    (for i <- 1..n, j <- i..n, do: {i,j})
    |> Enum.group_by(fn {i,j} -> i*i*i + j*j*j end)
    |> Enum.filter(fn {_,v} -> length(v)>1 end)
    |> Enum.sort
  end
end

nums = Taxicab.numbers |> Enum.with_index
Enum.each(nums, fn {x,i} ->
  if i in 0..24 or i in 1999..2005 do
    IO.puts "#{i+1} : #{inspect x}"
  end
end)
```


```txt

1 : {1729, [{9, 10}, {1, 12}]}
2 : {4104, [{9, 15}, {2, 16}]}
3 : {13832, [{18, 20}, {2, 24}]}
4 : {20683, [{19, 24}, {10, 27}]}
5 : {32832, [{18, 30}, {4, 32}]}
6 : {39312, [{15, 33}, {2, 34}]}
7 : {40033, [{16, 33}, {9, 34}]}
8 : {46683, [{27, 30}, {3, 36}]}
9 : {64232, [{26, 36}, {17, 39}]}
10 : {65728, [{31, 33}, {12, 40}]}
11 : {110656, [{36, 40}, {4, 48}]}
12 : {110808, [{27, 45}, {6, 48}]}
13 : {134379, [{38, 43}, {12, 51}]}
14 : {149389, [{29, 50}, {8, 53}]}
15 : {165464, [{38, 48}, {20, 54}]}
16 : {171288, [{24, 54}, {17, 55}]}
17 : {195841, [{22, 57}, {9, 58}]}
18 : {216027, [{22, 59}, {3, 60}]}
19 : {216125, [{45, 50}, {5, 60}]}
20 : {262656, [{36, 60}, {8, 64}]}
21 : {314496, [{30, 66}, {4, 68}]}
22 : {320264, [{32, 66}, {18, 68}]}
23 : {327763, [{51, 58}, {30, 67}]}
24 : {373464, [{54, 60}, {6, 72}]}
25 : {402597, [{56, 61}, {42, 69}]}
2000 : {1671816384, [{940, 944}, {428, 1168}]}
2001 : {1672470592, [{632, 1124}, {29, 1187}]}
2002 : {1673170856, [{828, 1034}, {458, 1164}]}
2003 : {1675045225, [{744, 1081}, {522, 1153}]}
2004 : {1675958167, [{711, 1096}, {492, 1159}]}
2005 : {1676926719, [{714, 1095}, {63, 1188}]}
2006 : {1677646971, [{891, 990}, {99, 1188}]}

```


## FreeBASIC


```freebasic
' version 11-10-2016
' compile with: fbc -s console

' Brute force

' adopted from "Sorting algorithms/Shell" sort Task
Sub shellsort(s() As String)
  ' sort from lower bound to the highter bound
  Dim As UInteger lb = LBound(s)
  Dim As UInteger ub = UBound(s)
  Dim As Integer done, i, inc = ub - lb

  Do
    inc = inc / 2.2
    If inc < 1 Then inc = 1

    Do
      done = 0
      For i = lb To ub - inc
        If s(i) > s(i + inc) Then
          Swap s(i), s(i + inc)
          done = 1
        End If
      Next
    Loop Until done = 0

  Loop Until inc = 1

End Sub

' ------=< MAIN >=------

Dim As UInteger x, y, count, c, sum
Dim As UInteger cube(1290)
Dim As String result(), str1, str2, str3
Dim As String buf11 = Space(11), buf5 = Space(5)
ReDim result(900000)    ' ~1291*1291\2

' set up the cubes
Print : Print " Calculate cubes"
For x = 1 To 1290
  cube(x) = x*x*x
Next

' combine and store
Print : Print " Combine cubes"
For x = 1 To 1290
  For y = x To 1290
    sum = cube(x)+cube(y)
    RSet buf11, Str(sum) : str1 = buf11
    RSet buf5, Str(x) : str2 = buf5
    RSet buf5, Str(y) : Str3 = buf5
    result(count)=buf11 + " = " + str2 + " ^ 3 + " + str3 + " ^ 3"
    count = count +1
  Next
Next

count= count -1
ReDim Preserve result(count) ' trim the array

Print : Print " Sort (takes some time)"
shellsort(result())   ' sort

Print : Print " Find the Taxicab numbers"
c = 1 ' start at index 1
For x = 0 To count -1
  ' find sums that match
  If Left(result(x), 11) = Left(result(x + 1), 11) Then
    result(c) = result(x)
    y = x +1
    Do    ' merge the other solution(s)
      result(c) = result(c) + Mid(result(y), 12)
      y = y +1
    Loop Until Left(result(x), 11) <> Left(result(y), 11)
    x = y -1 ' let x point to last match result
    c = c +1
  End If
Next

c = c -1
Print : Print " "; c; " Taxicab numbers found"
ReDim Preserve result(c) ' trim the array again

cls
Print : Print " Print first 25 numbers" : Print
For x = 1 To 25
  Print result(x)
Next

Print : Print " The 2000th to the 2006th" : Print
For x = 2000 To 2006
  Print result(x)
Next


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
  Print first 25 numbers

       1729 =     1 ^ 3 +    12 ^ 3 =     9 ^ 3 +    10 ^ 3
       4104 =     2 ^ 3 +    16 ^ 3 =     9 ^ 3 +    15 ^ 3
      13832 =     2 ^ 3 +    24 ^ 3 =    18 ^ 3 +    20 ^ 3
      20683 =    10 ^ 3 +    27 ^ 3 =    19 ^ 3 +    24 ^ 3
      32832 =     4 ^ 3 +    32 ^ 3 =    18 ^ 3 +    30 ^ 3
      39312 =     2 ^ 3 +    34 ^ 3 =    15 ^ 3 +    33 ^ 3
      40033 =     9 ^ 3 +    34 ^ 3 =    16 ^ 3 +    33 ^ 3
      46683 =     3 ^ 3 +    36 ^ 3 =    27 ^ 3 +    30 ^ 3
      64232 =    17 ^ 3 +    39 ^ 3 =    26 ^ 3 +    36 ^ 3
      65728 =    12 ^ 3 +    40 ^ 3 =    31 ^ 3 +    33 ^ 3
     110656 =     4 ^ 3 +    48 ^ 3 =    36 ^ 3 +    40 ^ 3
     110808 =     6 ^ 3 +    48 ^ 3 =    27 ^ 3 +    45 ^ 3
     134379 =    12 ^ 3 +    51 ^ 3 =    38 ^ 3 +    43 ^ 3
     149389 =     8 ^ 3 +    53 ^ 3 =    29 ^ 3 +    50 ^ 3
     165464 =    20 ^ 3 +    54 ^ 3 =    38 ^ 3 +    48 ^ 3
     171288 =    17 ^ 3 +    55 ^ 3 =    24 ^ 3 +    54 ^ 3
     195841 =     9 ^ 3 +    58 ^ 3 =    22 ^ 3 +    57 ^ 3
     216027 =     3 ^ 3 +    60 ^ 3 =    22 ^ 3 +    59 ^ 3
     216125 =     5 ^ 3 +    60 ^ 3 =    45 ^ 3 +    50 ^ 3
     262656 =     8 ^ 3 +    64 ^ 3 =    36 ^ 3 +    60 ^ 3
     314496 =     4 ^ 3 +    68 ^ 3 =    30 ^ 3 +    66 ^ 3
     320264 =    18 ^ 3 +    68 ^ 3 =    32 ^ 3 +    66 ^ 3
     327763 =    30 ^ 3 +    67 ^ 3 =    51 ^ 3 +    58 ^ 3
     373464 =     6 ^ 3 +    72 ^ 3 =    54 ^ 3 +    60 ^ 3
     402597 =    42 ^ 3 +    69 ^ 3 =    56 ^ 3 +    61 ^ 3

 The 2000th to the 2006th

 1671816384 =   428 ^ 3 +  1168 ^ 3 =   940 ^ 3 +   944 ^ 3
 1672470592 =    29 ^ 3 +  1187 ^ 3 =   632 ^ 3 +  1124 ^ 3
 1673170856 =   458 ^ 3 +  1164 ^ 3 =   828 ^ 3 +  1034 ^ 3
 1675045225 =   522 ^ 3 +  1153 ^ 3 =   744 ^ 3 +  1081 ^ 3
 1675958167 =   492 ^ 3 +  1159 ^ 3 =   711 ^ 3 +  1096 ^ 3
 1676926719 =    63 ^ 3 +  1188 ^ 3 =   714 ^ 3 +  1095 ^ 3
 1677646971 =    99 ^ 3 +  1188 ^ 3 =   891 ^ 3 +   990 ^ 3
```



## Go


```go
package main

import (
	"container/heap"
	"fmt"
	"strings"
)

type CubeSum struct {
	x, y  uint16
	value uint64
}

func (c *CubeSum) fixvalue() { c.value = cubes[c.x] + cubes[c.y] }

type CubeSumHeap []*CubeSum

func (h CubeSumHeap) Len() int            { return len(h) }
func (h CubeSumHeap) Less(i, j int) bool  { return h[i].value < h[j].value }
func (h CubeSumHeap) Swap(i, j int)       { h[i], h[j] = h[j], h[i] }
func (h *CubeSumHeap) Push(x interface{}) { (*h) = append(*h, x.(*CubeSum)) }
func (h *CubeSumHeap) Pop() interface{} {
	x := (*h)[len(*h)-1]
	*h = (*h)[:len(*h)-1]
	return x
}

type TaxicabGen struct {
	n int
	h CubeSumHeap
}

var cubes []uint64 // cubes[i] == i*i*i
func cubesExtend(i int) {
	for n := uint64(len(cubes)); n <= uint64(i); n++ {
		cubes = append(cubes, n*n*n)
	}
}

func (g *TaxicabGen) min() CubeSum {
	for len(g.h) == 0 || g.h[0].value > cubes[g.n] {
		g.n++
		cubesExtend(g.n)
		heap.Push(&g.h, &CubeSum{uint16(g.n), 1, cubes[g.n] + 1})
	}
	// Note, we use g.h[0] to "peek" at the min heap entry.
	c := *(g.h[0])
	if c.y+1 <= c.x {
		// Instead of Pop and Push we modify in place and fix.
		g.h[0].y++
		g.h[0].fixvalue()
		heap.Fix(&g.h, 0)
	} else {
		heap.Pop(&g.h)
	}
	return c
}

// Originally this was just: type Taxicab [2]CubeSum
// and we always returned two sums. Now we return all the sums.
type Taxicab []CubeSum

func (t Taxicab) String() string {
	var b strings.Builder
	fmt.Fprintf(&b, "%12d", t[0].value)
	for _, p := range t {
		fmt.Fprintf(&b, " =%5d³ +%5d³", p.x, p.y)
	}
	return b.String()
}

func (g *TaxicabGen) Next() Taxicab {
	a, b := g.min(), g.min()
	for a.value != b.value {
		a, b = b, g.min()
	}
	//return Taxicab{a,b}

	// Originally this just returned Taxicab{a,b} and we didn't look
	// further into the heap. Since we start by looking at the next
	// pair, that is okay until the first Taxicab number with four
	// ways of expressing the cube, which doesn't happen until the
	// 97,235th Taxicab:
	//     6963472309248 = 16630³ + 13322³ = 18072³ + 10200³
	//                   = 18948³ +  5436³ = 19083³ +  2421³
	// Now we return all ways so we need to peek into the heap.
	t := Taxicab{a, b}
	for g.h[0].value == b.value {
		t = append(t, g.min())
	}
	return t
}

func main() {
	const (
		low  = 25
		mid  = 2e3
		high = 4e4
	)
	var tg TaxicabGen
	firstn := 3 // To show the first triple, quadruple, etc
	for i := 1; i <= high+6; i++ {
		t := tg.Next()
		switch {
		case len(t) >= firstn:
			firstn++
			fallthrough
		case i <= low || (mid <= i && i <= mid+6) || i >= high:
			//fmt.Printf("h:%-4d  ", len(tg.h))
			fmt.Printf("%5d: %v\n", i, t)
		}
	}
}
```

```txt

    1:         1729 =   12³ +    1³ =   10³ +    9³
    2:         4104 =   16³ +    2³ =   15³ +    9³
    3:        13832 =   24³ +    2³ =   20³ +   18³
    4:        20683 =   27³ +   10³ =   24³ +   19³
    5:        32832 =   32³ +    4³ =   30³ +   18³
    6:        39312 =   34³ +    2³ =   33³ +   15³
    7:        40033 =   34³ +    9³ =   33³ +   16³
    8:        46683 =   36³ +    3³ =   30³ +   27³
    9:        64232 =   36³ +   26³ =   39³ +   17³
   10:        65728 =   40³ +   12³ =   33³ +   31³
   11:       110656 =   48³ +    4³ =   40³ +   36³
   12:       110808 =   48³ +    6³ =   45³ +   27³
   13:       134379 =   51³ +   12³ =   43³ +   38³
   14:       149389 =   53³ +    8³ =   50³ +   29³
   15:       165464 =   54³ +   20³ =   48³ +   38³
   16:       171288 =   55³ +   17³ =   54³ +   24³
   17:       195841 =   58³ +    9³ =   57³ +   22³
   18:       216027 =   60³ +    3³ =   59³ +   22³
   19:       216125 =   60³ +    5³ =   50³ +   45³
   20:       262656 =   64³ +    8³ =   60³ +   36³
   21:       314496 =   68³ +    4³ =   66³ +   30³
   22:       320264 =   66³ +   32³ =   68³ +   18³
   23:       327763 =   58³ +   51³ =   67³ +   30³
   24:       373464 =   72³ +    6³ =   60³ +   54³
   25:       402597 =   69³ +   42³ =   61³ +   56³
  455:     87539319 =  436³ +  167³ =  423³ +  228³ =  414³ +  255³
 2000:   1671816384 = 1168³ +  428³ =  944³ +  940³
 2001:   1672470592 = 1187³ +   29³ = 1124³ +  632³
 2002:   1673170856 = 1164³ +  458³ = 1034³ +  828³
 2003:   1675045225 = 1081³ +  744³ = 1153³ +  522³
 2004:   1675958167 = 1096³ +  711³ = 1159³ +  492³
 2005:   1676926719 = 1188³ +   63³ = 1095³ +  714³
 2006:   1677646971 =  990³ +  891³ = 1188³ +   99³
40000: 976889700163 = 8659³ + 6894³ = 9891³ + 2098³
40001: 976942087381 = 7890³ + 7861³ = 8680³ + 6861³
40002: 976946344920 = 9476³ + 5014³ = 9798³ + 3312³
40003: 976962998375 = 9912³ + 1463³ = 8415³ + 7250³
40004: 976974757064 = 9365³ + 5379³ = 9131³ + 5997³
40005: 977025552984 = 9894³ + 2040³ = 9792³ + 3366³
40006: 977104161000 = 9465³ + 5055³ = 9920³ +  970³

```



## Haskell



```haskell
import Data.List (sortBy, groupBy, tails, transpose)
import Data.Ord (comparing)

-- TAXICAB NUMBERS ----------------------------------------------------
taxis :: Int -> [[(Int, ((Int, Int), (Int, Int)))]]
taxis nCubes =
  filter ((> 1) . length) $
  groupBy ((. fst) . (==) . fst) $
  sortBy
    (comparing fst)
    [ (fst x + fst y, (x, y))
    | (x:t) <- tails $ ((^ 3) >>= (,)) <$> [1 .. nCubes]
    , y <- t ]

-- Taxicab numbers composed from first 1200 cubes
xs :: [(Int, [(Int, ((Int, Int), (Int, Int)))])]
xs = zip [1 ..] (taxis 1200)

-- PRETTY PRINTING ----------------------------------------------------
taxiRow :: (Int, [(Int, ((Int, Int), (Int, Int)))]) -> [String]
taxiRow (n, [(a, ((axc, axr), (ayc, ayr))), (b, ((bxc, bxr), (byc, byr)))]) =
  concat
    [ [show n, ". ", show a, " = "]
    , term axr axc " + "
    , term ayr ayc "  or  "
    , term bxr bxc " + "
    , term byr byc []
    ]
  where
    term r c l = ["(", show r, "^3=", show c, ")", l]

-- OUTPUT -------------------------------------------------------------
main :: IO ()
main =
  mapM_ putStrLn $
  concat <$>
  transpose
    (((<$>) =<< flip justifyRight ' ' . maximum . (length <$>)) <$>
     transpose (taxiRow <$> (take 25 xs ++ take 7 (drop 1999 xs))))
  where
    justifyRight n c s = drop (length s) (replicate n c ++ s)
```

```txt
   1.       1729 = (  1^3=        1) + (  12^3=      1728)  or  (  9^3=      729) + (  10^3=      1000)
   2.       4104 = (  2^3=        8) + (  16^3=      4096)  or  (  9^3=      729) + (  15^3=      3375)
   3.      13832 = (  2^3=        8) + (  24^3=     13824)  or  ( 18^3=     5832) + (  20^3=      8000)
   4.      20683 = ( 10^3=     1000) + (  27^3=     19683)  or  ( 19^3=     6859) + (  24^3=     13824)
   5.      32832 = (  4^3=       64) + (  32^3=     32768)  or  ( 18^3=     5832) + (  30^3=     27000)
   6.      39312 = (  2^3=        8) + (  34^3=     39304)  or  ( 15^3=     3375) + (  33^3=     35937)
   7.      40033 = (  9^3=      729) + (  34^3=     39304)  or  ( 16^3=     4096) + (  33^3=     35937)
   8.      46683 = (  3^3=       27) + (  36^3=     46656)  or  ( 27^3=    19683) + (  30^3=     27000)
   9.      64232 = ( 17^3=     4913) + (  39^3=     59319)  or  ( 26^3=    17576) + (  36^3=     46656)
  10.      65728 = ( 12^3=     1728) + (  40^3=     64000)  or  ( 31^3=    29791) + (  33^3=     35937)
  11.     110656 = (  4^3=       64) + (  48^3=    110592)  or  ( 36^3=    46656) + (  40^3=     64000)
  12.     110808 = (  6^3=      216) + (  48^3=    110592)  or  ( 27^3=    19683) + (  45^3=     91125)
  13.     134379 = ( 12^3=     1728) + (  51^3=    132651)  or  ( 38^3=    54872) + (  43^3=     79507)
  14.     149389 = (  8^3=      512) + (  53^3=    148877)  or  ( 29^3=    24389) + (  50^3=    125000)
  15.     165464 = ( 20^3=     8000) + (  54^3=    157464)  or  ( 38^3=    54872) + (  48^3=    110592)
  16.     171288 = ( 17^3=     4913) + (  55^3=    166375)  or  ( 24^3=    13824) + (  54^3=    157464)
  17.     195841 = (  9^3=      729) + (  58^3=    195112)  or  ( 22^3=    10648) + (  57^3=    185193)
  18.     216027 = (  3^3=       27) + (  60^3=    216000)  or  ( 22^3=    10648) + (  59^3=    205379)
  19.     216125 = (  5^3=      125) + (  60^3=    216000)  or  ( 45^3=    91125) + (  50^3=    125000)
  20.     262656 = (  8^3=      512) + (  64^3=    262144)  or  ( 36^3=    46656) + (  60^3=    216000)
  21.     314496 = (  4^3=       64) + (  68^3=    314432)  or  ( 30^3=    27000) + (  66^3=    287496)
  22.     320264 = ( 18^3=     5832) + (  68^3=    314432)  or  ( 32^3=    32768) + (  66^3=    287496)
  23.     327763 = ( 30^3=    27000) + (  67^3=    300763)  or  ( 51^3=   132651) + (  58^3=    195112)
  24.     373464 = (  6^3=      216) + (  72^3=    373248)  or  ( 54^3=   157464) + (  60^3=    216000)
  25.     402597 = ( 42^3=    74088) + (  69^3=    328509)  or  ( 56^3=   175616) + (  61^3=    226981)
2000. 1671816384 = (428^3= 78402752) + (1168^3=1593413632)  or  (940^3=830584000) + ( 944^3= 841232384)
2001. 1672470592 = ( 29^3=    24389) + (1187^3=1672446203)  or  (632^3=252435968) + (1124^3=1420034624)
2002. 1673170856 = (458^3= 96071912) + (1164^3=1577098944)  or  (828^3=567663552) + (1034^3=1105507304)
2003. 1675045225 = (522^3=142236648) + (1153^3=1532808577)  or  (744^3=411830784) + (1081^3=1263214441)
2004. 1675958167 = (492^3=119095488) + (1159^3=1556862679)  or  (711^3=359425431) + (1096^3=1316532736)
2005. 1676926719 = ( 63^3=   250047) + (1188^3=1676676672)  or  (714^3=363994344) + (1095^3=1312932375)
2006. 1677646971 = ( 99^3=   970299) + (1188^3=1676676672)  or  (891^3=707347971) + ( 990^3= 970299000)
```



## J



```J
cubes=: 3^~1+i.100 NB. first 100 cubes
triples=: /:~ ~. ,/ (+ , /:~@,)"0/~cubes NB. ordered pairs of cubes (each with their sum)
candidates=: ;({."#. <@(0&#`({.@{.(;,)<@}."1)@.(1<#))/. ])triples

NB. we just want the first 25 taxicab numbers
25{.(,.~ <@>:@i.@#) candidates
┌──┬──────┬────────────┬─────────────┐
│1 │1729  │1 1728      │729 1000     │
├──┼──────┼────────────┼─────────────┤
│2 │4104  │8 4096      │729 3375     │
├──┼──────┼────────────┼─────────────┤
│3 │13832 │8 13824     │5832 8000    │
├──┼──────┼────────────┼─────────────┤
│4 │20683 │1000 19683  │6859 13824   │
├──┼──────┼────────────┼─────────────┤
│5 │32832 │64 32768    │5832 27000   │
├──┼──────┼────────────┼─────────────┤
│6 │39312 │8 39304     │3375 35937   │
├──┼──────┼────────────┼─────────────┤
│7 │40033 │729 39304   │4096 35937   │
├──┼──────┼────────────┼─────────────┤
│8 │46683 │27 46656    │19683 27000  │
├──┼──────┼────────────┼─────────────┤
│9 │64232 │4913 59319  │17576 46656  │
├──┼──────┼────────────┼─────────────┤
│10│65728 │1728 64000  │29791 35937  │
├──┼──────┼────────────┼─────────────┤
│11│110656│64 110592   │46656 64000  │
├──┼──────┼────────────┼─────────────┤
│12│110808│216 110592  │19683 91125  │
├──┼──────┼────────────┼─────────────┤
│13│134379│1728 132651 │54872 79507  │
├──┼──────┼────────────┼─────────────┤
│14│149389│512 148877  │24389 125000 │
├──┼──────┼────────────┼─────────────┤
│15│165464│8000 157464 │54872 110592 │
├──┼──────┼────────────┼─────────────┤
│16│171288│4913 166375 │13824 157464 │
├──┼──────┼────────────┼─────────────┤
│17│195841│729 195112  │10648 185193 │
├──┼──────┼────────────┼─────────────┤
│18│216027│27 216000   │10648 205379 │
├──┼──────┼────────────┼─────────────┤
│19│216125│125 216000  │91125 125000 │
├──┼──────┼────────────┼─────────────┤
│20│262656│512 262144  │46656 216000 │
├──┼──────┼────────────┼─────────────┤
│21│314496│64 314432   │27000 287496 │
├──┼──────┼────────────┼─────────────┤
│22│320264│5832 314432 │32768 287496 │
├──┼──────┼────────────┼─────────────┤
│23│327763│27000 300763│132651 195112│
├──┼──────┼────────────┼─────────────┤
│24│373464│216 373248  │157464 216000│
├──┼──────┼────────────┼─────────────┤
│25│402597│74088 328509│175616 226981│
└──┴──────┴────────────┴─────────────┘
```


Explanation:

First, generate 100 cubes.

Then, form a 3 column table of unique rows: sum, small cube, large cube

Then, gather rows where the first entry is the same. Keep the ones with at least two such entries (sorted by ascending order of sum).

Then, place an counting index (starting from 1) in front of each row, so the columns are now: '''counting index''', '''sum''', '''small cube''', '''large cube'''.

Note that the cube root of the 25th entry is slightly smaller than 74, so testing against the first 100 cubes is more than sufficient.

Note that here we have elected to show the constituent cubes as themselves rather than as expressions involving their cube roots.

Extra credit:


```J
   x:each 7 {. 1999 }. (,.~ <@>:@i.@#) ;({."#. <@(0&#`({.@{.(;,)<@}."1)@.(1<#))/. ])/:~~.,/(+,/:~@,)"0/~3^~1+i.10000
┌────┬──────────┬────────────────────┬────────────────────┬┐
│2000│1671816384│78402752 1593413632 │830584000 841232384 ││
├────┼──────────┼────────────────────┼────────────────────┼┤
│2001│1672470592│24389 1672446203    │252435968 1420034624││
├────┼──────────┼────────────────────┼────────────────────┼┤
│2002│1673170856│96071912 1577098944 │567663552 1105507304││
├────┼──────────┼────────────────────┼────────────────────┼┤
│2003│1675045225│142236648 1532808577│411830784 1263214441││
├────┼──────────┼────────────────────┼────────────────────┼┤
│2004│1675958167│119095488 1556862679│359425431 1316532736││
├────┼──────────┼────────────────────┼────────────────────┼┤
│2005│1676926719│250047 1676676672   │363994344 1312932375││
├────┼──────────┼────────────────────┼────────────────────┼┤
│2006│1677646971│970299 1676676672   │707347971 970299000 ││
└────┴──────────┴────────────────────┴────────────────────┴┘
```


The extra blank box at the end is because when tackling this large of a data set, some sums can be achieved by three different pairs of cubes.


## Java


```java
import java.util.PriorityQueue;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

class CubeSum implements Comparable<CubeSum> {
	public long x, y, value;

	public CubeSum(long x, long y) {
		this.x = x;
		this.y = y;
		this.value = x*x*x + y*y*y;
	}

	public String toString() {
		return String.format("%4d^3 + %4d^3", x, y);
	}

	public int compareTo(CubeSum that) {
		return value < that.value ? -1 : value > that.value ? 1 : 0;
	}
}

class SumIterator implements Iterator<CubeSum> {
	PriorityQueue<CubeSum> pq = new PriorityQueue<CubeSum>();
	long n = 0;

	public boolean hasNext() { return true; }
	public CubeSum next() {
		while (pq.size() == 0 || pq.peek().value >= n*n*n)
			pq.add(new CubeSum(++n, 1));

		CubeSum s = pq.remove();
		if (s.x > s.y + 1) pq.add(new CubeSum(s.x, s.y+1));

		return s;
	}
}

class TaxiIterator implements Iterator<List<CubeSum>> {
	Iterator<CubeSum> sumIterator = new SumIterator();
	CubeSum last = sumIterator.next();

	public boolean hasNext() { return true; }
	public List<CubeSum> next() {
		CubeSum s;
		List<CubeSum> train = new ArrayList<CubeSum>();

		while ((s = sumIterator.next()).value != last.value)
			last = s;

		train.add(last);

		do { train.add(s); } while ((s = sumIterator.next()).value == last.value);
		last = s;

		return train;
	}
}

public class Taxi {
	public static final void main(String[] args) {
		Iterator<List<CubeSum>> taxi = new TaxiIterator();

		for (int i = 1; i <= 2006; i++) {
			List<CubeSum> t = taxi.next();
			if (i > 25 && i < 2000) continue;

			System.out.printf("%4d: %10d", i, t.get(0).value);
			for (CubeSum s: t)
				System.out.print(" = " + s);
			System.out.println();
		}
	}
}
```

```txt

   1:       1729 =   10^3 +    9^3 =   12^3 +    1^3
   2:       4104 =   15^3 +    9^3 =   16^3 +    2^3
   3:      13832 =   20^3 +   18^3 =   24^3 +    2^3
   4:      20683 =   24^3 +   19^3 =   27^3 +   10^3
   5:      32832 =   30^3 +   18^3 =   32^3 +    4^3
   6:      39312 =   33^3 +   15^3 =   34^3 +    2^3
   7:      40033 =   34^3 +    9^3 =   33^3 +   16^3
   8:      46683 =   30^3 +   27^3 =   36^3 +    3^3
   9:      64232 =   36^3 +   26^3 =   39^3 +   17^3
  10:      65728 =   33^3 +   31^3 =   40^3 +   12^3
  11:     110656 =   40^3 +   36^3 =   48^3 +    4^3
  12:     110808 =   45^3 +   27^3 =   48^3 +    6^3
  13:     134379 =   43^3 +   38^3 =   51^3 +   12^3
  14:     149389 =   50^3 +   29^3 =   53^3 +    8^3
  15:     165464 =   48^3 +   38^3 =   54^3 +   20^3
  16:     171288 =   54^3 +   24^3 =   55^3 +   17^3
  17:     195841 =   57^3 +   22^3 =   58^3 +    9^3
  18:     216027 =   59^3 +   22^3 =   60^3 +    3^3
  19:     216125 =   50^3 +   45^3 =   60^3 +    5^3
  20:     262656 =   60^3 +   36^3 =   64^3 +    8^3
  21:     314496 =   66^3 +   30^3 =   68^3 +    4^3
  22:     320264 =   66^3 +   32^3 =   68^3 +   18^3
  23:     327763 =   58^3 +   51^3 =   67^3 +   30^3
  24:     373464 =   60^3 +   54^3 =   72^3 +    6^3
  25:     402597 =   61^3 +   56^3 =   69^3 +   42^3
2000: 1671816384 = 1168^3 +  428^3 =  944^3 +  940^3
2001: 1672470592 = 1124^3 +  632^3 = 1187^3 +   29^3
2002: 1673170856 = 1164^3 +  458^3 = 1034^3 +  828^3
2003: 1675045225 = 1153^3 +  522^3 = 1081^3 +  744^3
2004: 1675958167 = 1159^3 +  492^3 = 1096^3 +  711^3
2005: 1676926719 = 1095^3 +  714^3 = 1188^3 +   63^3
2006: 1677646971 =  990^3 +  891^3 = 1188^3 +   99^3

```


## JavaScript


```JavaScript
var n3s = [],
    s3s = {}
for (var n = 1, e = 1200; n < e; n += 1) n3s[n] = n * n * n
for (var a = 1; a < e - 1; a += 1) {
    var a3 = n3s[a]
    for (var b = a; b < e; b += 1) {
        var b3 = n3s[b]
        var s3 = a3 + b3,
            abs = s3s[s3]
        if (!abs) s3s[s3] = abs = []
        abs.push([a, b])
    }
}

var i = 0
for (var s3 in s3s) {
    var abs = s3s[s3]
    if (abs.length < 2) continue
    i += 1
    if (abs.length == 2 && i > 25 && i < 2000) continue
    if (i > 2006) break
    document.write(i, ': ', s3)
    for (var ab of abs) {
        document.write(' = ', ab[0], '<sup>3</sup>+', ab[1], '<sup>3</sup>')
    }
    document.write('
')
}
```

 1: 1729 = 1<sup>3</sup>+12<sup>3</sup> = 9<sup>3</sup>+10<sup>3</sup>
 2: 4104 = 2<sup>3</sup>+16<sup>3</sup> = 9<sup>3</sup>+15<sup>3</sup>
 3: 13832 = 2<sup>3</sup>+24<sup>3</sup> = 18<sup>3</sup>+20<sup>3</sup>
 4: 20683 = 10<sup>3</sup>+27<sup>3</sup> = 19<sup>3</sup>+24<sup>3</sup>
 5: 32832 = 4<sup>3</sup>+32<sup>3</sup> = 18<sup>3</sup>+30<sup>3</sup>
 6: 39312 = 2<sup>3</sup>+34<sup>3</sup> = 15<sup>3</sup>+33<sup>3</sup>
 7: 40033 = 9<sup>3</sup>+34<sup>3</sup> = 16<sup>3</sup>+33<sup>3</sup>
 8: 46683 = 3<sup>3</sup>+36<sup>3</sup> = 27<sup>3</sup>+30<sup>3</sup>
 9: 64232 = 17<sup>3</sup>+39<sup>3</sup> = 26<sup>3</sup>+36<sup>3</sup>
 10: 65728 = 12<sup>3</sup>+40<sup>3</sup> = 31<sup>3</sup>+33<sup>3</sup>
 11: 110656 = 4<sup>3</sup>+48<sup>3</sup> = 36<sup>3</sup>+40<sup>3</sup>
 12: 110808 = 6<sup>3</sup>+48<sup>3</sup> = 27<sup>3</sup>+45<sup>3</sup>
 13: 134379 = 12<sup>3</sup>+51<sup>3</sup> = 38<sup>3</sup>+43<sup>3</sup>
 14: 149389 = 8<sup>3</sup>+53<sup>3</sup> = 29<sup>3</sup>+50<sup>3</sup>
 15: 165464 = 20<sup>3</sup>+54<sup>3</sup> = 38<sup>3</sup>+48<sup>3</sup>
 16: 171288 = 17<sup>3</sup>+55<sup>3</sup> = 24<sup>3</sup>+54<sup>3</sup>
 17: 195841 = 9<sup>3</sup>+58<sup>3</sup> = 22<sup>3</sup>+57<sup>3</sup>
 18: 216027 = 3<sup>3</sup>+60<sup>3</sup> = 22<sup>3</sup>+59<sup>3</sup>
 19: 216125 = 5<sup>3</sup>+60<sup>3</sup> = 45<sup>3</sup>+50<sup>3</sup>
 20: 262656 = 8<sup>3</sup>+64<sup>3</sup> = 36<sup>3</sup>+60<sup>3</sup>
 21: 314496 = 4<sup>3</sup>+68<sup>3</sup> = 30<sup>3</sup>+66<sup>3</sup>
 22: 320264 = 18<sup>3</sup>+68<sup>3</sup> = 32<sup>3</sup>+66<sup>3</sup>
 23: 327763 = 30<sup>3</sup>+67<sup>3</sup> = 51<sup>3</sup>+58<sup>3</sup>
 24: 373464 = 6<sup>3</sup>+72<sup>3</sup> = 54<sup>3</sup>+60<sup>3</sup>
 25: 402597 = 42<sup>3</sup>+69<sup>3</sup> = 56<sup>3</sup>+61<sup>3</sup>
 455: 87539319 = 167<sup>3</sup>+436<sup>3</sup> = 228<sup>3</sup>+423<sup>3</sup> = 255<sup>3</sup>+414<sup>3</sup>
 535: 119824488 = 11<sup>3</sup>+493<sup>3</sup> = 90<sup>3</sup>+492<sup>3</sup> = 346<sup>3</sup>+428<sup>3</sup>
 588: 143604279 = 111<sup>3</sup>+522<sup>3</sup> = 359<sup>3</sup>+460<sup>3</sup> = 408<sup>3</sup>+423<sup>3</sup>
 655: 175959000 = 70<sup>3</sup>+560<sup>3</sup> = 198<sup>3</sup>+552<sup>3</sup> = 315<sup>3</sup>+525<sup>3</sup>
 888: 327763000 = 300<sup>3</sup>+670<sup>3</sup> = 339<sup>3</sup>+661<sup>3</sup> = 510<sup>3</sup>+580<sup>3</sup>
 1299: 700314552 = 334<sup>3</sup>+872<sup>3</sup> = 456<sup>3</sup>+846<sup>3</sup> = 510<sup>3</sup>+828<sup>3</sup>
 1398: 804360375 = 15<sup>3</sup>+930<sup>3</sup> = 198<sup>3</sup>+927<sup>3</sup> = 295<sup>3</sup>+920<sup>3</sup>
 1515: 958595904 = 22<sup>3</sup>+986<sup>3</sup> = 180<sup>3</sup>+984<sup>3</sup> = 692<sup>3</sup>+856<sup>3</sup>
 1660: 1148834232 = 222<sup>3</sup>+1044<sup>3</sup> = 718<sup>3</sup>+920<sup>3</sup> = 816<sup>3</sup>+846<sup>3</sup>
 1837: 1407672000 = 140<sup>3</sup>+1120<sup>3</sup> = 396<sup>3</sup>+1104<sup>3</sup> = 630<sup>3</sup>+1050<sup>3</sup>
 2000: 1671816384 = 428<sup>3</sup>+1168<sup>3</sup> = 940<sup>3</sup>+944<sup>3</sup>
 2001: 1672470592 = 29<sup>3</sup>+1187<sup>3</sup> = 632<sup>3</sup>+1124<sup>3</sup>
 2002: 1673170856 = 458<sup>3</sup>+1164<sup>3</sup> = 828<sup>3</sup>+1034<sup>3</sup>
 2003: 1675045225 = 522<sup>3</sup>+1153<sup>3</sup> = 744<sup>3</sup>+1081<sup>3</sup>
 2004: 1675958167 = 492<sup>3</sup>+1159<sup>3</sup> = 711<sup>3</sup>+1096<sup>3</sup>
 2005: 1676926719 = 63<sup>3</sup>+1188<sup>3</sup> = 714<sup>3</sup>+1095<sup>3</sup>
 2006: 1677646971 = 99<sup>3</sup>+1188<sup>3</sup> = 891<sup>3</sup>+990<sup>3</sup>


## jq

```jq
# Output: an array of the form [i^3 + j^3, [i, j]] sorted by the sum.
# Only cubes of 1 to ($in-1) are considered; the listing is therefore truncated
# as it might not capture taxicab numbers greater than $in ^ 3.
def sum_of_two_cubes:
  def cubed: .*.*.;
  . as $in
  | (cubed + 1) as $limit
  | [range(1;$in) as $i | range($i;$in) as $j

  | [ ($i|cubed) + ($j|cubed), [$i, $j] ] ] | sort
  | map( select( .[0] < $limit ) );

# Output a stream of triples [t, d1, d2], in order of t,
# where t is a taxicab number, and d1 and d2 are distinct
# decompositions [i,j] with i^3 + j^3 == t.
# The stream includes each taxicab number once only.
#
def taxicabs0:
  sum_of_two_cubes as $sums
  | range(1;$sums|length) as $i
  | if $sums[$i][0] == $sums[$i-1][0]
      and ($i==1 or $sums[$i][0] != $sums[$i-2][0])
    then [$sums[$i][0], $sums[$i-1][1], $sums[$i][1]]
    else empty
    end;

# Output a stream of $n taxicab triples: [t, d1, d2] as described above,
# without repeating t.
def taxicabs:
  # If your jq includes until/2 then the following definition
  # can be omitted:
  def until(cond; next):
    def _until: if cond then . else (next|_until) end;  _until;
  . as $n
  | [10, ($n / 10 | floor)] | max as $increment
  | [20, ($n / 2 | floor)] | max
  | [ ., [taxicabs0] ]
  | until( .[1] | length >= $m; (.[0] + $increment) | [., [taxicabs0]] )
  | .[1][0:$n] ;
```


'''The task'''

```jq
2006 | taxicabs as $t
| (range(0;25), range(1999;2006)) as $i
| "\($i+1): \($t[$i][0]) ~ \($t[$i][1]) and \($t[$i][2])"
```

```sh
$ jq -n -r -f Taxicab_numbers.jq
1: 1729 ~ [1,12] and [9,10]
2: 4104 ~ [2,16] and [9,15]
3: 13832 ~ [2,24] and [18,20]
4: 20683 ~ [10,27] and [19,24]
5: 32832 ~ [4,32] and [18,30]
6: 39312 ~ [2,34] and [15,33]
7: 40033 ~ [9,34] and [16,33]
8: 46683 ~ [3,36] and [27,30]
9: 64232 ~ [17,39] and [26,36]
10: 65728 ~ [12,40] and [31,33]
11: 110656 ~ [4,48] and [36,40]
12: 110808 ~ [6,48] and [27,45]
13: 134379 ~ [12,51] and [38,43]
14: 149389 ~ [8,53] and [29,50]
15: 165464 ~ [20,54] and [38,48]
16: 171288 ~ [17,55] and [24,54]
17: 195841 ~ [9,58] and [22,57]
18: 216027 ~ [3,60] and [22,59]
19: 216125 ~ [5,60] and [45,50]
20: 262656 ~ [8,64] and [36,60]
21: 314496 ~ [4,68] and [30,66]
22: 320264 ~ [18,68] and [32,66]
23: 327763 ~ [30,67] and [51,58]
24: 373464 ~ [6,72] and [54,60]
25: 402597 ~ [42,69] and [56,61]
2000: 1671816384 ~ [428,1168] and [940,944]
2001: 1672470592 ~ [29,1187] and [632,1124]
2002: 1673170856 ~ [458,1164] and [828,1034]
2003: 1675045225 ~ [522,1153] and [744,1081]
2004: 1675958167 ~ [492,1159] and [711,1096]
2005: 1676926719 ~ [63,1188] and [714,1095]
2006: 1677646971 ~ [99,1188] and [891,990]
```



## Julia

```julia
using DataStructures, IterTools

function findtaxinumbers(nmax::Integer)
    cube2n = Dict{Int,Int}(x ^ 3 => x for x in 0:nmax)
    sum2cubes = DefaultDict{Int,Set{NTuple{2,Int}}}(Set{NTuple{2,Int}})
    for ((c1, _), (c2, _)) in product(cube2n, cube2n)
        if c1 ≥ c2
            push!(sum2cubes[c1 + c2], (cube2n[c1], cube2n[c2]))
        end
    end

    taxied = collect((k, v) for (k, v) in sum2cubes if length(v) ≥ 2)
    return sort!(taxied, by = first)
end
taxied = findtaxinumbers(1200)

for (ith, (cube, set)) in zip(1:25, taxied[1:25])
    @printf "%2i: %7i = %s\n" ith cube join(set, ", ")
    # println(ith, ": ", cube, " = ", join(set, ", "))
end
println("...")
for (ith, (cube, set)) in zip(2000:2006, taxied[2000:2006])
    @printf "%-4i: %i = %s\n" ith cube join(set, ", ")
end

# version 2
function findtaxinumbers(nmax::Integer)
    cubes, crev = collect(x ^ 3 for x in 1:nmax), Dict{Int,Int}()
    for (x, x3) in enumerate(cubes)
        crev[x3] = x
    end
    sums = collect(x + y for x in cubes for y in cubes if y < x)
    sort!(sums)

    idx = 0
    for i in 2:(endof(sums) - 1)
        if sums[i-1] != sums[i] && sums[i] == sums[i+1]
            idx += 1
            if 25 < idx < 2000 || idx > 2006 continue end
            n, p = sums[i], NTuple{2,Int}[]
            for x in cubes
                n < 2x && break
                if haskey(crev, n - x)
                    push!(p, (crev[x], crev[n - x]))
                end
            end
            @printf "%4d: %10d" idx n
            for x in p @printf(" = %4d ^ 3 + %4d ^ 3", x...) end
            println()
        end
    end
end

findtaxinumbers(1200)
```


```txt
 1:    1729 = (12, 1), (10, 9)
 2:    4104 = (16, 2), (15, 9)
 3:   13832 = (24, 2), (20, 18)
 4:   20683 = (27, 10), (24, 19)
 5:   32832 = (32, 4), (30, 18)
 6:   39312 = (33, 15), (34, 2)
 7:   40033 = (34, 9), (33, 16)
 8:   46683 = (30, 27), (36, 3)
 9:   64232 = (36, 26), (39, 17)
10:   65728 = (33, 31), (40, 12)
11:  110656 = (48, 4), (40, 36)
12:  110808 = (48, 6), (45, 27)
13:  134379 = (43, 38), (51, 12)
14:  149389 = (50, 29), (53, 8)
15:  165464 = (54, 20), (48, 38)
16:  171288 = (54, 24), (55, 17)
17:  195841 = (57, 22), (58, 9)
18:  216027 = (59, 22), (60, 3)
19:  216125 = (60, 5), (50, 45)
20:  262656 = (64, 8), (60, 36)
21:  314496 = (66, 30), (68, 4)
22:  320264 = (66, 32), (68, 18)
23:  327763 = (67, 30), (58, 51)
24:  373464 = (60, 54), (72, 6)
25:  402597 = (69, 42), (61, 56)
...
2000: 1671816384 = (944, 940), (1168, 428)
2001: 1672470592 = (1124, 632), (1187, 29)
2002: 1673170856 = (1034, 828), (1164, 458)
2003: 1675045225 = (1081, 744), (1153, 522)
2004: 1675958167 = (1159, 492), (1096, 711)
2005: 1676926719 = (1188, 63), (1095, 714)
2006: 1677646971 = (1188, 99), (990, 891)
   1:       1729 =    1 ^ 3 +   12 ^ 3 =    9 ^ 3 +   10 ^ 3
   2:       4104 =    2 ^ 3 +   16 ^ 3 =    9 ^ 3 +   15 ^ 3
   3:      13832 =    2 ^ 3 +   24 ^ 3 =   18 ^ 3 +   20 ^ 3
   4:      20683 =   10 ^ 3 +   27 ^ 3 =   19 ^ 3 +   24 ^ 3
   5:      32832 =    4 ^ 3 +   32 ^ 3 =   18 ^ 3 +   30 ^ 3
   6:      39312 =    2 ^ 3 +   34 ^ 3 =   15 ^ 3 +   33 ^ 3
   7:      40033 =    9 ^ 3 +   34 ^ 3 =   16 ^ 3 +   33 ^ 3
   8:      46683 =    3 ^ 3 +   36 ^ 3 =   27 ^ 3 +   30 ^ 3
   9:      64232 =   17 ^ 3 +   39 ^ 3 =   26 ^ 3 +   36 ^ 3
  10:      65728 =   12 ^ 3 +   40 ^ 3 =   31 ^ 3 +   33 ^ 3
  11:     110656 =    4 ^ 3 +   48 ^ 3 =   36 ^ 3 +   40 ^ 3
  12:     110808 =    6 ^ 3 +   48 ^ 3 =   27 ^ 3 +   45 ^ 3
  13:     134379 =   12 ^ 3 +   51 ^ 3 =   38 ^ 3 +   43 ^ 3
  14:     149389 =    8 ^ 3 +   53 ^ 3 =   29 ^ 3 +   50 ^ 3
  15:     165464 =   20 ^ 3 +   54 ^ 3 =   38 ^ 3 +   48 ^ 3
  16:     171288 =   17 ^ 3 +   55 ^ 3 =   24 ^ 3 +   54 ^ 3
  17:     195841 =    9 ^ 3 +   58 ^ 3 =   22 ^ 3 +   57 ^ 3
  18:     216027 =    3 ^ 3 +   60 ^ 3 =   22 ^ 3 +   59 ^ 3
  19:     216125 =    5 ^ 3 +   60 ^ 3 =   45 ^ 3 +   50 ^ 3
  20:     262656 =    8 ^ 3 +   64 ^ 3 =   36 ^ 3 +   60 ^ 3
  21:     314496 =    4 ^ 3 +   68 ^ 3 =   30 ^ 3 +   66 ^ 3
  22:     320264 =   18 ^ 3 +   68 ^ 3 =   32 ^ 3 +   66 ^ 3
  23:     327763 =   30 ^ 3 +   67 ^ 3 =   51 ^ 3 +   58 ^ 3
  24:     373464 =    6 ^ 3 +   72 ^ 3 =   54 ^ 3 +   60 ^ 3
  25:     402597 =   42 ^ 3 +   69 ^ 3 =   56 ^ 3 +   61 ^ 3
2000: 1671816384 =  428 ^ 3 + 1168 ^ 3 =  940 ^ 3 +  944 ^ 3
2001: 1672470592 =   29 ^ 3 + 1187 ^ 3 =  632 ^ 3 + 1124 ^ 3
2002: 1673170856 =  458 ^ 3 + 1164 ^ 3 =  828 ^ 3 + 1034 ^ 3
2003: 1675045225 =  522 ^ 3 + 1153 ^ 3 =  744 ^ 3 + 1081 ^ 3
2004: 1675958167 =  492 ^ 3 + 1159 ^ 3 =  711 ^ 3 + 1096 ^ 3
2005: 1676926719 =   63 ^ 3 + 1188 ^ 3 =  714 ^ 3 + 1095 ^ 3
2006: 1677646971 =   99 ^ 3 + 1188 ^ 3 =  891 ^ 3 +  990 ^ 3
```



## Kotlin

```scala
// version 1.0.6

import java.util.PriorityQueue

class CubeSum(val x: Long, val y: Long) : Comparable<CubeSum> {
    val value: Long = x * x * x + y * y * y

    override fun toString() = String.format("%4d^3 + %3d^3", x, y)

    override fun compareTo(other: CubeSum) = value.compareTo(other.value)
}

class SumIterator : Iterator<CubeSum> {
    private val pq = PriorityQueue<CubeSum>()
    private var n = 0L

    override fun hasNext() = true

    override fun next(): CubeSum {
        while (pq.size == 0 || pq.peek().value >= n * n * n)
            pq.add(CubeSum(++n, 1))
        val s: CubeSum = pq.remove()
        if (s.x > s.y + 1) pq.add(CubeSum(s.x, s.y + 1))
        return s
    }
}

class TaxiIterator : Iterator<MutableList<CubeSum>> {
    private val sumIterator = SumIterator()
    private var last: CubeSum = sumIterator.next()

    override fun hasNext() = true

    override fun next(): MutableList<CubeSum> {
        var s: CubeSum = sumIterator.next()
        val train = mutableListOf<CubeSum>()
        while (s.value != last.value) {
            last = s
            s = sumIterator.next()
        }
        train.add(last)
        do {
            train.add(s)
            s = sumIterator.next()
        }
        while (s.value == last.value)
        last = s
        return train
    }
}

fun main(args: Array<String>) {
    val taxi = TaxiIterator()
    for (i in 1..2006) {
        val t = taxi.next()
        if (i in 26 until 2000) continue
        print(String.format("%4d: %10d", i, t[0].value))
        for (s in t) print("  = $s")
        println()
    }
}
```


```txt

   1:       1729  =   10^3 +   9^3  =   12^3 +   1^3
   2:       4104  =   15^3 +   9^3  =   16^3 +   2^3
   3:      13832  =   20^3 +  18^3  =   24^3 +   2^3
   4:      20683  =   24^3 +  19^3  =   27^3 +  10^3
   5:      32832  =   30^3 +  18^3  =   32^3 +   4^3
   6:      39312  =   33^3 +  15^3  =   34^3 +   2^3
   7:      40033  =   33^3 +  16^3  =   34^3 +   9^3
   8:      46683  =   30^3 +  27^3  =   36^3 +   3^3
   9:      64232  =   39^3 +  17^3  =   36^3 +  26^3
  10:      65728  =   40^3 +  12^3  =   33^3 +  31^3
  11:     110656  =   40^3 +  36^3  =   48^3 +   4^3
  12:     110808  =   45^3 +  27^3  =   48^3 +   6^3
  13:     134379  =   51^3 +  12^3  =   43^3 +  38^3
  14:     149389  =   50^3 +  29^3  =   53^3 +   8^3
  15:     165464  =   48^3 +  38^3  =   54^3 +  20^3
  16:     171288  =   54^3 +  24^3  =   55^3 +  17^3
  17:     195841  =   57^3 +  22^3  =   58^3 +   9^3
  18:     216027  =   59^3 +  22^3  =   60^3 +   3^3
  19:     216125  =   50^3 +  45^3  =   60^3 +   5^3
  20:     262656  =   60^3 +  36^3  =   64^3 +   8^3
  21:     314496  =   66^3 +  30^3  =   68^3 +   4^3
  22:     320264  =   68^3 +  18^3  =   66^3 +  32^3
  23:     327763  =   67^3 +  30^3  =   58^3 +  51^3
  24:     373464  =   60^3 +  54^3  =   72^3 +   6^3
  25:     402597  =   69^3 +  42^3  =   61^3 +  56^3
2000: 1671816384  = 1168^3 + 428^3  =  944^3 + 940^3
2001: 1672470592  = 1124^3 + 632^3  = 1187^3 +  29^3
2002: 1673170856  = 1164^3 + 458^3  = 1034^3 + 828^3
2003: 1675045225  = 1153^3 + 522^3  = 1081^3 + 744^3
2004: 1675958167  = 1159^3 + 492^3  = 1096^3 + 711^3
2005: 1676926719  = 1095^3 + 714^3  = 1188^3 +  63^3
2006: 1677646971  =  990^3 + 891^3  = 1188^3 +  99^3

```



## Mathematica


```Mathematica
findTaxi[n_] := Sort[Keys[Select[Counts[Flatten[Table[x^3 + y^3, {x, 1, n}, {y, x, n}]]], GreaterThan[1]]]];
Take[findTaxiNumbers[100], 25]
found=findTaxiNumbers[1200][[2000 ;; 2005]]
Map[Reduce[x^3 + y^3 == # && x >= y && x > 0 && y > 0, {x, y}, Integers] &, found]
```

```txt
{1729, 4104, 13832, 20683, 32832, 39312, 40033, 46683, 64232, 65728, 110656, 110808, 134379, 149389, 165464, 171288, 195841, 216027, 216125, 262656, 314496, 320264, 327763, 373464, 402597}

{1671816384, 1672470592, 1673170856, 1675045225, 1675958167, 1676926719}

{(x == 944 && y == 940) || (x == 1168 && y == 428),
(x == 1124 && y == 632) || (x == 1187 && y == 29),
(x == 1034 && y == 828) || (x == 1164 && y == 458),
(x == 1081 && y == 744) || (x == 1153 && y == 522),
(x == 1096 && y == 711) || (x == 1159 && y == 492),
(x == 1095 &&  y == 714) || (x == 1188 && y == 63)}
```



## PARI/GP


```parigp
taxicab(n)=my(t); for(k=sqrtnint((n-1)\2,3)+1, sqrtnint(n,3), if(ispower(n-k^3, 3), if(t, return(1), t=1))); 0;
cubes(n)=my(t); for(k=sqrtnint((n-1)\2,3)+1, sqrtnint(n,3), if(ispower(n-k^3, 3, &t), print(n" =  \t"k"^3\t+ "t"^3")))
select(taxicab, [1..402597])
apply(cubes, %);
```

```txt
%1 = [1729, 4104, 13832, 20683, 32832, 39312, 40033, 46683, 64232, 65728, 110656, 110808, 134379, 149389, 165464, 171288, 195841, 216027, 216125, 262656, 314496, 320264, 327763, 373464, 402597]
1729 =          10^3    + 9^3
1729 =          12^3    + 1^3
4104 =          15^3    + 9^3
4104 =          16^3    + 2^3
13832 =         20^3    + 18^3
13832 =         24^3    + 2^3
20683 =         24^3    + 19^3
20683 =         27^3    + 10^3
32832 =         30^3    + 18^3
32832 =         32^3    + 4^3
39312 =         33^3    + 15^3
39312 =         34^3    + 2^3
40033 =         33^3    + 16^3
40033 =         34^3    + 9^3
46683 =         30^3    + 27^3
46683 =         36^3    + 3^3
64232 =         36^3    + 26^3
64232 =         39^3    + 17^3
65728 =         33^3    + 31^3
65728 =         40^3    + 12^3
110656 =        40^3    + 36^3
110656 =        48^3    + 4^3
110808 =        45^3    + 27^3
110808 =        48^3    + 6^3
134379 =        43^3    + 38^3
134379 =        51^3    + 12^3
149389 =        50^3    + 29^3
149389 =        53^3    + 8^3
165464 =        48^3    + 38^3
165464 =        54^3    + 20^3
171288 =        54^3    + 24^3
171288 =        55^3    + 17^3
195841 =        57^3    + 22^3
195841 =        58^3    + 9^3
216027 =        59^3    + 22^3
216027 =        60^3    + 3^3
216125 =        50^3    + 45^3
216125 =        60^3    + 5^3
262656 =        60^3    + 36^3
262656 =        64^3    + 8^3
314496 =        66^3    + 30^3
314496 =        68^3    + 4^3
320264 =        66^3    + 32^3
320264 =        68^3    + 18^3
327763 =        58^3    + 51^3
327763 =        67^3    + 30^3
373464 =        60^3    + 54^3
373464 =        72^3    + 6^3
402597 =        61^3    + 56^3
402597 =        69^3    + 42^3
```



## Pascal

Brute force: Create all combinations x³+ y³ | y < x one by on and test if there is a combination v < x and v> w > y with the same cube-sum.
Combinations to check = n*(n-1)/2.The mean distance of one Combination m is m/2 from m³+1³ to m³+(m-1)³.
searchSameSum checks one half of this distance == m/4.So O(n) ~ n³ /8  checks are needed.
searchSameSum takes most of the time (>95% ), sorting is neglectable.
[[http://rosettacode.org/wiki/Taxicab_numbers#C]]C-Version is ~6 times faster aka 43 vs 247 ms for max = 1290^3.
Here limit set to 1190 to just reach the goal of element 2006 ;-) so 200ms are possible.
Its impressive, that over all one check takes ~3.5 cpu-cycles on i4330 3.5Ghz


```pascal
program taxiCabNo;
uses
  sysutils;
type
  tPot3    = Uint32;
  tPot3Sol = record
               p3Sum : tPot3;
               i1,j1,
               i2,j2 : Word;
             end;
 tpPot3    = ^tPot3;
 tpPot3Sol = ^tPot3Sol;

var
//1290^3 = 2'146'689'000 < 2^31-1
//1190 is the magic number of the task ;-)
  pot3 : array[0..1190{1290}] of tPot3;//
  AllSol : array[0..3000] of tpot3Sol;
  AllSolHigh : NativeInt;

procedure SolOut(const s:tpot3Sol;no: NativeInt);
begin
  with s do
    writeln(no:5,p3Sum:12,' = ',j1:5,'^3 +',i1:5,'^3 =',j2:5,'^3 +',i2:5,'^3');
end;

procedure InsertAllSol;

var
  tmp: tpot3Sol;
  p :tpPot3Sol;
  p3Sum: tPot3;
  i: NativeInt;
Begin

  i := AllSolHigh;
  IF i > 0 then
  Begin
    p := @AllSol[i];
    tmp := p^;
    p3Sum := p^.p3Sum;
    //search the right place for insertion
    repeat
      dec(i);
      dec(p);
      IF (p^.p3Sum <= p3Sum) then
        BREAK;
    until  (i<=0);
    IF p^.p3Sum = p3Sum then
      EXIT;
    //free the right place by moving one place up
    inc(i);
    inc(p);
    IF i<AllSolHigh then
    Begin
      move(p^,AllSol[i+1],SizeOf(AllSol[0])*(AllSolHigh-i));
      p^ := tmp;
    end;
  end;
  inc(AllSolHigh);
end;

function searchSameSum(var sol:tpot3Sol):boolean;
//try to find a new combination for the same sum
//within the limits given by lo and hi
var
  Sum,
  SumLo: tPot3;
  hi,lo: NativeInt;
Begin
  with Sol do
  Begin
    Sum := p3Sum;
    lo:= i1;
    hi:= j1;
  end;

  repeat
    //Move hi down
    dec(hi);
    SumLo := Sum-Pot3[hi];
    //Move lo up an check until new combination found or implicite lo> hi
    repeat
      inc(lo)
    until (SumLo<=Pot3[lo]);
    //found?
    IF SumLo = Pot3[lo] then
      BREAK;
  until lo>=hi;

  IF lo<hi then
  Begin
    sol.i2:= lo;
    sol.j2:= hi;
    searchSameSum := true;
  end
  else
    searchSameSum := false;
end;

procedure Search;
var
  i,j: LongInt;
Begin
  AllSolHigh := 0;
  For j := 2 to High(pot3)-1 do
  Begin
    For i := 1 to j-1 do
    Begin
      with AllSol[AllSolHigh] do
      Begin
        p3Sum:= pot3[i]+pot3[j];
        i1:= i;
        j1:= j;
      end;
      IF searchSameSum(AllSol[AllSolHigh]) then
      BEGIN
        InsertAllSol;
        IF AllSolHigh>High(AllSol) then EXIT;
      end;
    end;
  end;
end;

var
  i: LongInt;
Begin
  For i := Low(pot3) to High(pot3) do
    pot3[i] := i*i*i;
  AllSolHigh := 0;
  Search;
  For i :=    0 to   24 do SolOut(AllSol[i],i+1);
  For i := 1999 to 2005 do SolOut(AllSol[i],i+1);
  writeln('count of solutions         ',AllSolHigh);
end.

```


```txt
    1        1729 =    12^3 +    1^3 =   10^3 +    9^3
    2        4104 =    16^3 +    2^3 =   15^3 +    9^3
    3       13832 =    24^3 +    2^3 =   20^3 +   18^3
......
   24      373464 =    72^3 +    6^3 =   60^3 +   54^3
   25      402597 =    69^3 +   42^3 =   61^3 +   56^3
 2000  1671816384 =  1168^3 +  428^3 =  944^3 +  940^3
 2001  1672470592 =  1187^3 +   29^3 = 1124^3 +  632^3
...
 2005  1676926719 =  1188^3 +   63^3 = 1095^3 +  714^3
 2006  1677646971 =  1188^3 +   99^3 =  990^3 +  891^3
count of solutions         2050
//checks              196438017
real  0m0.196s
```



## Perl

Uses segmentation so memory use is constrained as high values are searched for.  Also has parameter to look for Ta(3) and Ta(4) numbers (which is when segmentation is really needed).  By default shows the first 25 numbers; with one argument shows that many; with two arguments shows results in the range.


```perl
my($beg, $end) = (@ARGV==0) ? (1,25) : (@ARGV==1) ? (1,shift) : (shift,shift);

my $lim = 1e14;  # Ought to be dynamic as should segment size
my @basis = map { $_*$_*$_ } (1 .. int($lim ** (1.0/3.0) + 1));
my $paira = 2;  # We're looking for Ta(2) and larger

my ($segsize, $low, $high, $i) = (500_000_000, 0, 0, 0);

while ($i < $end) {
  $low = $high+1;
  die "lim too low" if $low > $lim;
  $high = $low + $segsize - 1;
  $high = $lim if $high > $lim;
  foreach my $p (_find_pairs_segment(\@basis, $paira, $low, $high,
                 sub { sprintf("%4d^3 + %4d^3", $_[0], $_[1]) })    ) {
    $i++;
    next if $i < $beg;
    last if $i > $end;
    my $n = shift @$p;
    printf "%4d: %10d  = %s\n", $i, $n, join("  = ", @$p);
  }
}

sub _find_pairs_segment {
  my($p, $len, $start, $end, $formatsub) = @_;
  my $plen = $#$p;

  my %allpairs;
  foreach my $i (0 .. $plen) {
    my $pi = $p->[$i];
    next if ($pi+$p->[$plen]) < $start;
    last if (2*$pi) > $end;
    foreach my $j ($i .. $plen) {
      my $sum = $pi + $p->[$j];
      next if $sum < $start;
      last if $sum > $end;
      push @{ $allpairs{$sum} }, $i, $j;
    }
    # If we wanted to save more memory, we could filter and delete every entry
    # where $n < 2 * $p->[$i+1].  This can cut memory use in half, but is slow.
  }

  my @retlist;
  foreach my $list (grep { scalar @$_ >= $len*2 } values %allpairs) {
    my $n = $p->[$list->[0]] + $p->[$list->[1]];
    my @pairlist;
    while (@$list) {
      push @pairlist, $formatsub->(1 + shift @$list, 1 + shift @$list);
    }
    push @retlist, [$n, @pairlist];
  }
  @retlist = sort { $a->[0] <=> $b->[0] } @retlist;
  return @retlist;
}
```

```txt

   1:       1729  =    1^3 +   12^3  =    9^3 +   10^3
   2:       4104  =    2^3 +   16^3  =    9^3 +   15^3
   3:      13832  =    2^3 +   24^3  =   18^3 +   20^3
   4:      20683  =   10^3 +   27^3  =   19^3 +   24^3
   5:      32832  =    4^3 +   32^3  =   18^3 +   30^3
   6:      39312  =    2^3 +   34^3  =   15^3 +   33^3
   7:      40033  =    9^3 +   34^3  =   16^3 +   33^3
   8:      46683  =    3^3 +   36^3  =   27^3 +   30^3
   9:      64232  =   17^3 +   39^3  =   26^3 +   36^3
  10:      65728  =   12^3 +   40^3  =   31^3 +   33^3
  11:     110656  =    4^3 +   48^3  =   36^3 +   40^3
  12:     110808  =    6^3 +   48^3  =   27^3 +   45^3
  13:     134379  =   12^3 +   51^3  =   38^3 +   43^3
  14:     149389  =    8^3 +   53^3  =   29^3 +   50^3
  15:     165464  =   20^3 +   54^3  =   38^3 +   48^3
  16:     171288  =   17^3 +   55^3  =   24^3 +   54^3
  17:     195841  =    9^3 +   58^3  =   22^3 +   57^3
  18:     216027  =    3^3 +   60^3  =   22^3 +   59^3
  19:     216125  =    5^3 +   60^3  =   45^3 +   50^3
  20:     262656  =    8^3 +   64^3  =   36^3 +   60^3
  21:     314496  =    4^3 +   68^3  =   30^3 +   66^3
  22:     320264  =   18^3 +   68^3  =   32^3 +   66^3
  23:     327763  =   30^3 +   67^3  =   51^3 +   58^3
  24:     373464  =    6^3 +   72^3  =   54^3 +   60^3
  25:     402597  =   42^3 +   69^3  =   56^3 +   61^3

```

With arguments 2000 2006:

```txt

2000: 1671816384  =  428^3 + 1168^3  =  940^3 +  944^3
2001: 1672470592  =   29^3 + 1187^3  =  632^3 + 1124^3
2002: 1673170856  =  458^3 + 1164^3  =  828^3 + 1034^3
2003: 1675045225  =  522^3 + 1153^3  =  744^3 + 1081^3
2004: 1675958167  =  492^3 + 1159^3  =  711^3 + 1096^3
2005: 1676926719  =   63^3 + 1188^3  =  714^3 + 1095^3
2006: 1677646971  =   99^3 + 1188^3  =  891^3 +  990^3

```



## Perl 6

This uses a pretty simple search algorithm that doesn't necessarily return the Taxicab numbers in order. Assuming we want all the Taxicab numbers within some range S to N, we'll search until we find N values. When we find the Nth value, we continue to search up to the cube root of the largest Taxicab number found up to that point. That ensures we will find all of them inside the desired range without needing to search arbitrarily or use magic numbers. Defaults to returning the Taxicab numbers from 1 to 25. Pass in a different start and end value if you want some other range.

```perl6
constant @cu = (^Inf).map: { .³ }

sub MAIN ($start = 1, $end = 25) {
    my %taxi;
    my int $taxis = 0;
    my $terminate = 0;
    my int $max = 0;

    for 1 .. * -> $c1 {
        last if ?$terminate && ($terminate < $c1);
        for 1 .. $c1 -> $c2 {
            my $this = @cu[$c1] + @cu[$c2];
            %taxi{$this}.push: [$c2, $c1];
            if %taxi{$this}.elems == 2 {
                ++$taxis;
                $max max= $this;
            }
    	    $terminate = ceiling $max ** (1/3) if $taxis == $end and !$terminate;
        }
    }

    display( %taxi, $start, $end );

}

sub display (%this_stuff, $start, $end) {
    my $i = $start;
    printf "%4d %10d  =>\t%s\n", $i++, $_.key,
        (.value.map({ sprintf "%4d³ + %-s\³", |$_ })).join: ",\t"
        for %this_stuff.grep( { $_.value.elems > 1 } ).sort( +*.key )[$start-1..$end-1];
}
```

{{out}}With no passed parameters (default):

```txt
   1       1729  =>	   9³ + 10³,	   1³ + 12³
   2       4104  =>	   9³ + 15³,	   2³ + 16³
   3      13832  =>	  18³ + 20³,	   2³ + 24³
   4      20683  =>	  19³ + 24³,	  10³ + 27³
   5      32832  =>	  18³ + 30³,	   4³ + 32³
   6      39312  =>	  15³ + 33³,	   2³ + 34³
   7      40033  =>	  16³ + 33³,	   9³ + 34³
   8      46683  =>	  27³ + 30³,	   3³ + 36³
   9      64232  =>	  26³ + 36³,	  17³ + 39³
  10      65728  =>	  31³ + 33³,	  12³ + 40³
  11     110656  =>	  36³ + 40³,	   4³ + 48³
  12     110808  =>	  27³ + 45³,	   6³ + 48³
  13     134379  =>	  38³ + 43³,	  12³ + 51³
  14     149389  =>	  29³ + 50³,	   8³ + 53³
  15     165464  =>	  38³ + 48³,	  20³ + 54³
  16     171288  =>	  24³ + 54³,	  17³ + 55³
  17     195841  =>	  22³ + 57³,	   9³ + 58³
  18     216027  =>	  22³ + 59³,	   3³ + 60³
  19     216125  =>	  45³ + 50³,	   5³ + 60³
  20     262656  =>	  36³ + 60³,	   8³ + 64³
  21     314496  =>	  30³ + 66³,	   4³ + 68³
  22     320264  =>	  32³ + 66³,	  18³ + 68³
  23     327763  =>	  51³ + 58³,	  30³ + 67³
  24     373464  =>	  54³ + 60³,	   6³ + 72³
  25     402597  =>	  56³ + 61³,	  42³ + 69³
```

With passed parameters 2000 2006:

```txt
2000 1671816384  =>	 940³ + 944³,	 428³ + 1168³
2001 1672470592  =>	 632³ + 1124³,	  29³ + 1187³
2002 1673170856  =>	 828³ + 1034³,	 458³ + 1164³
2003 1675045225  =>	 744³ + 1081³,	 522³ + 1153³
2004 1675958167  =>	 711³ + 1096³,	 492³ + 1159³
2005 1676926719  =>	 714³ + 1095³,	  63³ + 1188³
2006 1677646971  =>	 891³ + 990³,	  99³ + 1188³
```



## Phix

Uses a dictionary to map sum of cubes to either the first/only pair or an integer index into the result set.
Turned out to be a fair bit slower (15s) than I first expected.

```Phix
function get_taxis(integer last)
    sequence taxis = {}
    integer c1 = 1, maxc1 = 0, c2
    atom c3, h3 = 0
    while maxc1=0 or c1<maxc1 do
        c3 = power(c1,3)
        for c2 = 1 to c1 do
            atom this = power(c2,3)+c3
            integer node = getd_index(this)
            if node=NULL then
                setd(this,{c2,c1})
            else
                if this>h3 then h3 = this end if
                object data = getd_by_index(node)
                if not integer(data) then
                    taxis = append(taxis,{this,{data}})
                    data = length(taxis)
                    setd(this,data)
                    if data=last then
                        maxc1 = ceil(power(h3,1/3))
                    end if
                end if
                taxis[data][2] &= {{c2,c1}}
            end if
        end for
        c1 += 1
    end while
    destroy_dict(1,justclear:=true)
    taxis = sort(taxis)
    return taxis
end function

sequence taxis = get_taxis(2006)
constant sets = {{1,25},{2000,2006}}
for s=1 to length(sets) do
    integer {first,last} = sets[s]
    for i=first to last do
        printf(1,"%d: %d: %s\n",{i,taxis[i][1],sprint(taxis[i][2])})
    end for
end for
```

```txt

1: 1729: {{9,10},{1,12}}
2: 4104: {{9,15},{2,16}}
3: 13832: {{18,20},{2,24}}
4: 20683: {{19,24},{10,27}}
5: 32832: {{18,30},{4,32}}
6: 39312: {{15,33},{2,34}}
7: 40033: {{16,33},{9,34}}
8: 46683: {{27,30},{3,36}}
9: 64232: {{26,36},{17,39}}
10: 65728: {{31,33},{12,40}}
11: 110656: {{36,40},{4,48}}
12: 110808: {{27,45},{6,48}}
13: 134379: {{38,43},{12,51}}
14: 149389: {{29,50},{8,53}}
15: 165464: {{38,48},{20,54}}
16: 171288: {{24,54},{17,55}}
17: 195841: {{22,57},{9,58}}
18: 216027: {{22,59},{3,60}}
19: 216125: {{45,50},{5,60}}
20: 262656: {{36,60},{8,64}}
21: 314496: {{30,66},{4,68}}
22: 320264: {{32,66},{18,68}}
23: 327763: {{51,58},{30,67}}
24: 373464: {{54,60},{6,72}}
25: 402597: {{56,61},{42,69}}
2000: 1671816384: {{940,944},{428,1168}}
2001: 1672470592: {{632,1124},{29,1187}}
2002: 1673170856: {{828,1034},{458,1164}}
2003: 1675045225: {{744,1081},{522,1153}}
2004: 1675958167: {{711,1096},{492,1159}}
2005: 1676926719: {{714,1095},{63,1188}}
2006: 1677646971: {{891,990},{99,1188}}

```


Using a [[Priority_queue#Phix|priority queue]], otherwise based on C, quite a bit (18.5x) faster.

Copes with 40000..6, same results as Go, though that increases the runtime from 0.8s to 1min 15s.

```Phix
sequence cubes = {}

procedure add_cube()
    integer n = length(cubes)+1
    cubes = append(cubes,n*n*n)
    pq_add({{n,1},cubes[n]+1})
end procedure

constant VALUE = PRIORITY

function next_sum()
    while length(pq)<=2 or pq[1][VALUE]>=cubes[$] do add_cube() end while
    sequence res = pq_pop()
    integer {x,y} = res[DATA]
    y += 1
    if y<x then
        pq_add({{x,y},cubes[x]+cubes[y]})
    end if
    return res
end function

function next_taxi()
    sequence top
    while 1 do
        top = next_sum()
        if pq[1][VALUE]=top[VALUE] then exit end if
    end while
    sequence res = {top}
    atom v = top[PRIORITY]
    while 1 do
        top = next_sum()
        res = append(res,top[DATA])
        if pq[1][VALUE]!=v then exit end if
    end while
    return res
end function

for i=1 to 2006 do
    sequence x = next_taxi()
    if i<=25 or i>=2000 then
        atom v = x[1][VALUE]
        x[1] = x[1][DATA]
        string y = sprintf("%11d+%-10d",sq_power(x[1],3))
        for j=2 to length(x) do
            y &= sprintf(",%11d+%-10d",sq_power(x[j],3))
        end for
        printf(1,"%4d: %10d: %-23s [%s]\n",{i,v,sprint(x),y})
    end if
end for
```

```txt

   1:       1729: {{10,9},{12,1}}         [       1000+729       ,       1728+1         ]
   2:       4104: {{15,9},{16,2}}         [       3375+729       ,       4096+8         ]
   3:      13832: {{20,18},{24,2}}        [       8000+5832      ,      13824+8         ]
   4:      20683: {{24,19},{27,10}}       [      13824+6859      ,      19683+1000      ]
   5:      32832: {{30,18},{32,4}}        [      27000+5832      ,      32768+64        ]
   6:      39312: {{33,15},{34,2}}        [      35937+3375      ,      39304+8         ]
   7:      40033: {{33,16},{34,9}}        [      35937+4096      ,      39304+729       ]
   8:      46683: {{30,27},{36,3}}        [      27000+19683     ,      46656+27        ]
   9:      64232: {{39,17},{36,26}}       [      59319+4913      ,      46656+17576     ]
  10:      65728: {{40,12},{33,31}}       [      64000+1728      ,      35937+29791     ]
  11:     110656: {{40,36},{48,4}}        [      64000+46656     ,     110592+64        ]
  12:     110808: {{45,27},{48,6}}        [      91125+19683     ,     110592+216       ]
  13:     134379: {{51,12},{43,38}}       [     132651+1728      ,      79507+54872     ]
  14:     149389: {{50,29},{53,8}}        [     125000+24389     ,     148877+512       ]
  15:     165464: {{48,38},{54,20}}       [     110592+54872     ,     157464+8000      ]
  16:     171288: {{54,24},{55,17}}       [     157464+13824     ,     166375+4913      ]
  17:     195841: {{57,22},{58,9}}        [     185193+10648     ,     195112+729       ]
  18:     216027: {{59,22},{60,3}}        [     205379+10648     ,     216000+27        ]
  19:     216125: {{50,45},{60,5}}        [     125000+91125     ,     216000+125       ]
  20:     262656: {{60,36},{64,8}}        [     216000+46656     ,     262144+512       ]
  21:     314496: {{66,30},{68,4}}        [     287496+27000     ,     314432+64        ]
  22:     320264: {{68,18},{66,32}}       [     314432+5832      ,     287496+32768     ]
  23:     327763: {{67,30},{58,51}}       [     300763+27000     ,     195112+132651    ]
  24:     373464: {{60,54},{72,6}}        [     216000+157464    ,     373248+216       ]
  25:     402597: {{69,42},{61,56}}       [     328509+74088     ,     226981+175616    ]
2000: 1671816384: {{1168,428},{944,940}}  [ 1593413632+78402752  ,  841232384+830584000 ]
2001: 1672470592: {{1124,632},{1187,29}}  [ 1420034624+252435968 , 1672446203+24389     ]
2002: 1673170856: {{1164,458},{1034,828}} [ 1577098944+96071912  , 1105507304+567663552 ]
2003: 1675045225: {{1153,522},{1081,744}} [ 1532808577+142236648 , 1263214441+411830784 ]
2004: 1675958167: {{1159,492},{1096,711}} [ 1556862679+119095488 , 1316532736+359425431 ]
2005: 1676926719: {{1095,714},{1188,63}}  [ 1312932375+363994344 , 1676676672+250047    ]
2006: 1677646971: {{990,891},{1188,99}}   [  970299000+707347971 , 1676676672+970299    ]

```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")

(off 'B)
(for L (subsets 2 (range 1 1200))
   (let K (sum '((N) (** N 3)) L)
      (ifn (lup B K)
         (idx 'B (list K 1 (list L)) T)
         (inc (cdr @))
         (push (cddr @) L) ) ) )
(setq R
   (filter
      '((L) (>= (cadr L) 2))
      (idx 'B)) )
(for L (head 25 R)
   (println (car L) (caddr L)) )
(for L (head 7 (nth R 2000))
   (println (car L) (caddr L)) )
```


```txt

1729 ((9 10) (1 12))
4104 ((9 15) (2 16))
13832 ((18 20) (2 24))
20683 ((19 24) (10 27))
32832 ((18 30) (4 32))
39312 ((15 33) (2 34))
40033 ((16 33) (9 34))
46683 ((27 30) (3 36))
64232 ((26 36) (17 39))
65728 ((31 33) (12 40))
110656 ((36 40) (4 48))
110808 ((27 45) (6 48))
134379 ((38 43) (12 51))
149389 ((29 50) (8 53))
165464 ((38 48) (20 54))
171288 ((24 54) (17 55))
195841 ((22 57) (9 58))
216027 ((22 59) (3 60))
216125 ((45 50) (5 60))
262656 ((36 60) (8 64))
314496 ((30 66) (4 68))
320264 ((32 66) (18 68))
327763 ((51 58) (30 67))
373464 ((54 60) (6 72))
402597 ((56 61) (42 69))
1671816384 ((940 944) (428 1168))
1672470592 ((632 1124) (29 1187))
1673170856 ((828 1034) (458 1164))
1675045225 ((744 1081) (522 1153))
1675958167 ((711 1096) (492 1159))
1676926719 ((714 1095) (63 1188))
1677646971 ((891 990) (99 1188))

```



## Python

(Magic number 1201 found by trial and error)

```python
from collections import defaultdict
from itertools import product
from pprint import pprint as pp

cube2n = {x**3:x for x in range(1, 1201)}
sum2cubes = defaultdict(set)
for c1, c2 in product(cube2n, cube2n):
	if c1 >= c2: sum2cubes[c1 + c2].add((cube2n[c1], cube2n[c2]))

taxied = sorted((k, v) for k,v in sum2cubes.items() if len(v) >= 2)

#pp(len(taxied))  # 2068
for t in enumerate(taxied[:25], 1):
    pp(t)
print('...')
for t in enumerate(taxied[2000-1:2000+6], 2000):
    pp(t)
```


```txt
(1, (1729, {(12, 1), (10, 9)}))
(2, (4104, {(16, 2), (15, 9)}))
(3, (13832, {(20, 18), (24, 2)}))
(4, (20683, {(27, 10), (24, 19)}))
(5, (32832, {(30, 18), (32, 4)}))
(6, (39312, {(33, 15), (34, 2)}))
(7, (40033, {(33, 16), (34, 9)}))
(8, (46683, {(30, 27), (36, 3)}))
(9, (64232, {(36, 26), (39, 17)}))
(10, (65728, {(33, 31), (40, 12)}))
(11, (110656, {(48, 4), (40, 36)}))
(12, (110808, {(48, 6), (45, 27)}))
(13, (134379, {(51, 12), (43, 38)}))
(14, (149389, {(50, 29), (53, 8)}))
(15, (165464, {(54, 20), (48, 38)}))
(16, (171288, {(54, 24), (55, 17)}))
(17, (195841, {(57, 22), (58, 9)}))
(18, (216027, {(60, 3), (59, 22)}))
(19, (216125, {(60, 5), (50, 45)}))
(20, (262656, {(64, 8), (60, 36)}))
(21, (314496, {(66, 30), (68, 4)}))
(22, (320264, {(66, 32), (68, 18)}))
(23, (327763, {(58, 51), (67, 30)}))
(24, (373464, {(72, 6), (60, 54)}))
(25, (402597, {(69, 42), (61, 56)}))
...
(2000, (1671816384, {(1168, 428), (944, 940)}))
(2001, (1672470592, {(1187, 29), (1124, 632)}))
(2002, (1673170856, {(1164, 458), (1034, 828)}))
(2003, (1675045225, {(1153, 522), (1081, 744)}))
(2004, (1675958167, {(1159, 492), (1096, 711)}))
(2005, (1676926719, {(1188, 63), (1095, 714)}))
(2006, (1677646971, {(990, 891), (1188, 99)}))
```


Although, for this task it's simply faster to look up the cubes in the sum when we need to print them, because we can now store and sort only the sums:

```python
cubes, crev = [x**3 for x in range(1,1200)], {}
# for cube root lookup
for x,x3 in enumerate(cubes): crev[x3] = x + 1

sums = sorted(x+y for x in cubes for y in cubes if y < x)

idx = 0
for i in range(1, len(sums)-1):
    if sums[i-1] != sums[i] and sums[i] == sums[i+1]:
        idx += 1
        if idx > 25 and idx < 2000 or idx > 2006: continue

        n,p = sums[i],[]
        for x in cubes:
            if n-x < x: break
            if n-x in crev:
                p.append((crev[x], crev[n-x]))
        print "%4d: %10d"%(idx,n),
        for x in p: print " = %4d^3 + %4d^3"%x,
        print
```

{{out}}Output trimmed to reduce clutter.

```txt

   1:       1729  =    1^3 +   12^3  =    9^3 +   10^3
   2:       4104  =    2^3 +   16^3  =    9^3 +   15^3
   3:      13832  =    2^3 +   24^3  =   18^3 +   20^3
   4:      20683  =   10^3 +   27^3  =   19^3 +   24^3
   5:      32832  =    4^3 +   32^3  =   18^3 +   30^3
...
2004: 1675958167  =  492^3 + 1159^3  =  711^3 + 1096^3
2005: 1676926719  =   63^3 + 1188^3  =  714^3 + 1095^3
2006: 1677646971  =   99^3 + 1188^3  =  891^3 +  990^3

```



### Using heapq module

A priority queue that holds cube sums.  When consecutive sums come out with the same value, they are taxis.

```python
from heapq import heappush, heappop

def cubesum():
    h,n = [],1
    while True:
        while not h or h[0][0] > n**3: # could also pre-calculate cubes
            heappush(h, (n**3 + 1, n, 1))
            n += 1

        (s, x, y) = heappop(h)
        yield((s, x, y))
        y += 1
        if y < x:    # should be y <= x?
            heappush(h, (x**3 + y**3, x, y))

def taxis():
    out = [(0,0,0)]
    for s in cubesum():
        if s[0] == out[-1][0]:
            out.append(s)
        else:
            if len(out) > 1: yield(out)
            out = [s]

n = 0
for x in taxis():
    n += 1
    if n >= 2006: break
    if n <= 25 or n >= 2000:
        print(n, x)
```

```txt

(1, [(1729, 10, 9), (1729, 12, 1)])
(2, [(4104, 15, 9), (4104, 16, 2)])
(3, [(13832, 20, 18), (13832, 24, 2)])
(4, [(20683, 24, 19), (20683, 27, 10)])
(5, [(32832, 30, 18), (32832, 32, 4)])
(6, [(39312, 33, 15), (39312, 34, 2)])
(7, [(40033, 33, 16), (40033, 34, 9)])
(8, [(46683, 30, 27), (46683, 36, 3)])
(9, [(64232, 36, 26), (64232, 39, 17)])
(10, [(65728, 33, 31), (65728, 40, 12)])
(11, [(110656, 40, 36), (110656, 48, 4)])
(12, [(110808, 45, 27), (110808, 48, 6)])
(13, [(134379, 43, 38), (134379, 51, 12)])
(14, [(149389, 50, 29), (149389, 53, 8)])
(15, [(165464, 48, 38), (165464, 54, 20)])
(16, [(171288, 54, 24), (171288, 55, 17)])
(17, [(195841, 57, 22), (195841, 58, 9)])
(18, [(216027, 59, 22), (216027, 60, 3)])
(19, [(216125, 50, 45), (216125, 60, 5)])
(20, [(262656, 60, 36), (262656, 64, 8)])
(21, [(314496, 66, 30), (314496, 68, 4)])
(22, [(320264, 66, 32), (320264, 68, 18)])
(23, [(327763, 58, 51), (327763, 67, 30)])
(24, [(373464, 60, 54), (373464, 72, 6)])
(25, [(402597, 61, 56), (402597, 69, 42)])
(2000, [(1671816384, 944, 940), (1671816384, 1168, 428)])
(2001, [(1672470592, 1124, 632), (1672470592, 1187, 29)])
(2002, [(1673170856, 1034, 828), (1673170856, 1164, 458)])
(2003, [(1675045225, 1081, 744), (1675045225, 1153, 522)])
(2004, [(1675958167, 1096, 711), (1675958167, 1159, 492)])
(2005, [(1676926719, 1095, 714), (1676926719, 1188, 63)])

```



## Racket

This is the straighforward implementation, so it finds
only the first 25 values in a sensible amount of time.

```Racket
#lang racket

(define (cube x) (* x x x))

;floor of cubic root
(define (cubic-root x)
  (let ([aprox (inexact->exact (round (expt x (/ 1 3))))])
    (if (> (cube aprox) x)
        (- aprox 1)
        aprox)))

(let loop ([p 1] [n 1])
  (let ()
    (define pairs
      (for*/list ([j (in-range 1 (add1 (cubic-root (quotient n 2))))]
                  [k (in-value (cubic-root (- n (cube j))))]
                  #:when (= n (+ (cube j) (cube k))))
        (cons j k)))
    (if (>= (length pairs) 2)
      (begin
        (printf "~a: ~a" p n)
        (for ([pair (in-list pairs)])
          (printf " = ~a^3 + ~a^3" (car pair) (cdr pair)))
          (newline)
        (when (< p 25)
          (loop (add1 p) (add1 n))))
      (loop p (add1 n)))))
```

```txt
1: 1729 = 1^3 + 12^3 = 9^3 + 10^3
2: 4104 = 2^3 + 16^3 = 9^3 + 15^3
3: 13832 = 2^3 + 24^3 = 18^3 + 20^3
4: 20683 = 10^3 + 27^3 = 19^3 + 24^3
5: 32832 = 4^3 + 32^3 = 18^3 + 30^3
6: 39312 = 2^3 + 34^3 = 15^3 + 33^3
7: 40033 = 9^3 + 34^3 = 16^3 + 33^3
8: 46683 = 3^3 + 36^3 = 27^3 + 30^3
9: 64232 = 17^3 + 39^3 = 26^3 + 36^3
10: 65728 = 12^3 + 40^3 = 31^3 + 33^3
11: 110656 = 4^3 + 48^3 = 36^3 + 40^3
12: 110808 = 6^3 + 48^3 = 27^3 + 45^3
13: 134379 = 12^3 + 51^3 = 38^3 + 43^3
14: 149389 = 8^3 + 53^3 = 29^3 + 50^3
15: 165464 = 20^3 + 54^3 = 38^3 + 48^3
16: 171288 = 17^3 + 55^3 = 24^3 + 54^3
17: 195841 = 9^3 + 58^3 = 22^3 + 57^3
18: 216027 = 3^3 + 60^3 = 22^3 + 59^3
19: 216125 = 5^3 + 60^3 = 45^3 + 50^3
20: 262656 = 8^3 + 64^3 = 36^3 + 60^3
21: 314496 = 4^3 + 68^3 = 30^3 + 66^3
22: 320264 = 18^3 + 68^3 = 32^3 + 66^3
23: 327763 = 30^3 + 67^3 = 51^3 + 58^3
24: 373464 = 6^3 + 72^3 = 54^3 + 60^3
25: 402597 = 42^3 + 69^3 = 56^3 + 61^3
```



## REXX

Programming note:   to ensure that the taxicab numbers are in order, an extra 10% are generated.

```rexx
/*REXX program displays the specified first (lowest) taxicab numbers (for three ranges).*/
parse arg  L.1  H.1      L.2 H.2      L.3 H.3 .  /*obtain optional arguments from the CL*/
   if L.1=='' | L.1==","  then L.1=   1          /*L1  is the low  part of 1st range.   */
   if H.1=='' | H.1==","  then H.1=  25          /*H1   "  "  high   "   "  "    "      */
   if L.2=='' | L.2==","  then L.2= 454          /*L2   "  "  low    "   " 2nd   "      */
   if H.2=='' | H.2==","  then H.2= 456          /*H2   "  "  high   "   "  "    "      */
   if L.3=='' | L.3==","  then L.3=2000          /*L3   "  "  low    "   " 3rd   "      */
   if H.3=='' | H.3==","  then H.3=2006          /*H3   "  "  high   "   "  "    "      */
mx= max(H.1,    H.2,    H.3)                     /*find how many taxicab numbers needed.*/
mx= mx   + mx % 10                               /*cushion;  compensate for the triples.*/
ww= length(mx) * 3;           w= ww % 2          /*widths used for formatting the output*/
numeric digits max(9, ww)                        /*prepare to use some larger numbers.  */
@.=.;    #= 0;     @@. =0;    @and= "  ──and── " /*set some REXX vars and handy literals*/
$.=                                              /* [↓]  generate extra taxicab numbers.*/
    do j=1  until #>=mx;             C= j**3     /*taxicab numbers may not be in order. */
    !.j= C                                       /*use memoization for cube calculation.*/
       do k=1  for j-1;              s= C + !.k  /*define a whole bunch of cube sums.   */
       if @.s==.  then do;  @.s= j;  b.s= k      /*Cube not defined?   Then process it. */
                            iterate              /*define  @.S  and  B.S≡sum  of 2 cubes*/
                       end                       /* [↑]  define one cube sum at a time. */
       has=@@.s                                  /*has this number been defined before? */
       if has  then $.s=$.s @and U(j,'   +')U(k) /* ◄─ build a display string. [↓]      */
               else $.s=right(s,ww)  '───►'   U(@.s,"   +")U(b.s)   @and   U(j,'   +')U(k)
       @@.s= 1                                   /*mark taxicab number as a sum of cubes*/
       if has   then iterate                     /*S  is a triple (or sometimes better).*/
       #= #+1;      #.#= s                       /*bump taxicab counter; define taxicab#*/
       end   /*k*/                               /* [↑]  build the cubes one─at─a─time. */
    end      /*j*/                               /* [↑]  complete with overage numbers. */
A.=
       do k=1  for mx;   _= #.k;    A.k= $._     /*re─assign disjoint $. elements to A. */
       end   /*k*/
call Esort  mx                                   /*sort taxicab #s with an exchange sort*/
       do grp=1  for 3;  call tell L.grp, H.grp  /*display the three grps of numbers. */
       end   /*grp*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell:  do t=arg(1)  to arg(2);    say right(t, 9)':'   A.t;     end;        say;    return
U:     return right(arg(1), w)'^3'arg(2)         /*right─justify a number,  append "^3" */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Esort: procedure expose A.; parse arg N;         h= N    /*Esort when items have blanks.*/
         do  while h>1;     h= h % 2
           do i=1  for N-h;           k=h + i;   j= i
              do forever;   parse var A.k xk .;  parse var A.j xj .;  if xk>=xj then leave
              _= A.j;       A.j= A.k; A.k= _             /*swap two elements of A. array*/
              if h>=j  then leave;    j= j - h;  k= k - h
              end   /*forever*/
           end      /*i*/
         end        /*while h>1*/;               return
```

```txt

        1:         1729 ───►     10^3   +     9^3   ──and──      12^3   +     1^3
        2:         4104 ───►     15^3   +     9^3   ──and──      16^3   +     2^3
        3:        13832 ───►     20^3   +    18^3   ──and──      24^3   +     2^3
        4:        20683 ───►     24^3   +    19^3   ──and──      27^3   +    10^3
        5:        32832 ───►     30^3   +    18^3   ──and──      32^3   +     4^3
        6:        39312 ───►     33^3   +    15^3   ──and──      34^3   +     2^3
        7:        40033 ───►     33^3   +    16^3   ──and──      34^3   +     9^3
        8:        46683 ───►     30^3   +    27^3   ──and──      36^3   +     3^3
        9:        64232 ───►     36^3   +    26^3   ──and──      39^3   +    17^3
       10:        65728 ───►     33^3   +    31^3   ──and──      40^3   +    12^3
       11:       110656 ───►     40^3   +    36^3   ──and──      48^3   +     4^3
       12:       110808 ───►     45^3   +    27^3   ──and──      48^3   +     6^3
       13:       134379 ───►     43^3   +    38^3   ──and──      51^3   +    12^3
       14:       149389 ───►     50^3   +    29^3   ──and──      53^3   +     8^3
       15:       165464 ───►     48^3   +    38^3   ──and──      54^3   +    20^3
       16:       171288 ───►     54^3   +    24^3   ──and──      55^3   +    17^3
       17:       195841 ───►     57^3   +    22^3   ──and──      58^3   +     9^3
       18:       216027 ───►     59^3   +    22^3   ──and──      60^3   +     3^3
       19:       216125 ───►     50^3   +    45^3   ──and──      60^3   +     5^3
       20:       262656 ───►     60^3   +    36^3   ──and──      64^3   +     8^3
       21:       314496 ───►     66^3   +    30^3   ──and──      68^3   +     4^3
       22:       320264 ───►     66^3   +    32^3   ──and──      68^3   +    18^3
       23:       327763 ───►     58^3   +    51^3   ──and──      67^3   +    30^3
       24:       373464 ───►     60^3   +    54^3   ──and──      72^3   +     6^3
       25:       402597 ───►     61^3   +    56^3   ──and──      69^3   +    42^3

      454:     87483968 ───►    363^3   +   341^3   ──and──     440^3   +   132^3
      455:     87539319 ───►    414^3   +   255^3   ──and──     423^3   +   228^3   ──and──     436^3   +   167^3
      456:     87579037 ───►    370^3   +   333^3   ──and──     444^3   +    37^3

     2000:   1671816384 ───►    944^3   +   940^3   ──and──    1168^3   +   428^3
     2001:   1672470592 ───►   1124^3   +   632^3   ──and──    1187^3   +    29^3
     2002:   1673170856 ───►   1034^3   +   828^3   ──and──    1164^3   +   458^3
     2003:   1675045225 ───►   1081^3   +   744^3   ──and──    1153^3   +   522^3
     2004:   1675958167 ───►   1096^3   +   711^3   ──and──    1159^3   +   492^3
     2005:   1676926719 ───►   1095^3   +   714^3   ──and──    1188^3   +    63^3
     2006:   1677646971 ───►    990^3   +   891^3   ──and──    1188^3   +    99^3

```



## Ring


```ring

# Project : Taxicab numbers

num = 0
for n = 1 to 500000
    nr = 0
    tax = []
    for m = 1 to 75
        for p = m + 1 to 75
            if n = pow(m, 3) + pow(p, 3)
               add(tax, m)
               add(tax, p)
               nr = nr + 1
            ok
        next
    next
    if nr > 1
       num = num + 1
       see "" + num + " " + n + " => " + tax[1] + "^3 + " + tax[2] + "^3" + ", "
       see "" + tax[3] + "^3 +" + tax[4] + "^3" + nl
       if num = 25
          exit
       ok
    ok
next
see "ok" + nl

```

Output:

```txt

   1       1729  =>	   9³ + 10³,	   1³ + 12³
   2       4104  =>	   9³ + 15³,	   2³ + 16³
   3      13832  =>	  18³ + 20³,	   2³ + 24³
   4      20683  =>	  19³ + 24³,	  10³ + 27³
   5      32832  =>	  18³ + 30³,	   4³ + 32³
   6      39312  =>	  15³ + 33³,	   2³ + 34³
   7      40033  =>	  16³ + 33³,	   9³ + 34³
   8      46683  =>	  27³ + 30³,	   3³ + 36³
   9      64232  =>	  26³ + 36³,	  17³ + 39³
  10     65728  =>	  31³ + 33³,	  12³ + 40³
  11     110656  =>	  36³ + 40³,	   4³ + 48³
  12     110808  =>	  27³ + 45³,	   6³ + 48³
  13     134379  =>	  38³ + 43³,	  12³ + 51³
  14     149389  =>	  29³ + 50³,	   8³ + 53³
  15     165464  =>	  38³ + 48³,	  20³ + 54³
  16     171288  =>	  24³ + 54³,	  17³ + 55³
  17     195841  =>	  22³ + 57³,	   9³ + 58³
  18     216027  =>	  22³ + 59³,	   3³ + 60³
  19     216125  =>	  45³ + 50³,	   5³ + 60³
  20     262656  =>	  36³ + 60³,	   8³ + 64³
  21     314496  =>	  30³ + 66³,	   4³ + 68³
  22     320264  =>	  32³ + 66³,	  18³ + 68³
  23     327763  =>	  51³ + 58³,	  30³ + 67³
  24     373464  =>	  54³ + 60³,	   6³ + 72³
  25     402597  =>	  56³ + 61³,	  42³ + 69³
  ok

```



## Ruby


```ruby
def taxicab_number(nmax=1200)
  [*1..nmax].repeated_combination(2).group_by{|x,y| x**3 + y**3}.select{|k,v| v.size>1}.sort
end

t = [0] + taxicab_number

[*1..25, *2000...2007].each do |i|
  puts "%4d: %10d" % [i, t[i][0]] + t[i][1].map{|a| " = %4d**3 + %4d**3" % a}.join
end
```

```txt

   1:       1729 =    1**3 +   12**3 =    9**3 +   10**3
   2:       4104 =    2**3 +   16**3 =    9**3 +   15**3
   3:      13832 =    2**3 +   24**3 =   18**3 +   20**3
   4:      20683 =   10**3 +   27**3 =   19**3 +   24**3
   5:      32832 =    4**3 +   32**3 =   18**3 +   30**3
   6:      39312 =    2**3 +   34**3 =   15**3 +   33**3
   7:      40033 =    9**3 +   34**3 =   16**3 +   33**3
   8:      46683 =    3**3 +   36**3 =   27**3 +   30**3
   9:      64232 =   17**3 +   39**3 =   26**3 +   36**3
  10:      65728 =   12**3 +   40**3 =   31**3 +   33**3
  11:     110656 =    4**3 +   48**3 =   36**3 +   40**3
  12:     110808 =    6**3 +   48**3 =   27**3 +   45**3
  13:     134379 =   12**3 +   51**3 =   38**3 +   43**3
  14:     149389 =    8**3 +   53**3 =   29**3 +   50**3
  15:     165464 =   20**3 +   54**3 =   38**3 +   48**3
  16:     171288 =   17**3 +   55**3 =   24**3 +   54**3
  17:     195841 =    9**3 +   58**3 =   22**3 +   57**3
  18:     216027 =    3**3 +   60**3 =   22**3 +   59**3
  19:     216125 =    5**3 +   60**3 =   45**3 +   50**3
  20:     262656 =    8**3 +   64**3 =   36**3 +   60**3
  21:     314496 =    4**3 +   68**3 =   30**3 +   66**3
  22:     320264 =   18**3 +   68**3 =   32**3 +   66**3
  23:     327763 =   30**3 +   67**3 =   51**3 +   58**3
  24:     373464 =    6**3 +   72**3 =   54**3 +   60**3
  25:     402597 =   42**3 +   69**3 =   56**3 +   61**3
2000: 1671816384 =  428**3 + 1168**3 =  940**3 +  944**3
2001: 1672470592 =   29**3 + 1187**3 =  632**3 + 1124**3
2002: 1673170856 =  458**3 + 1164**3 =  828**3 + 1034**3
2003: 1675045225 =  522**3 + 1153**3 =  744**3 + 1081**3
2004: 1675958167 =  492**3 + 1159**3 =  711**3 + 1096**3
2005: 1676926719 =   63**3 + 1188**3 =  714**3 + 1095**3
2006: 1677646971 =   99**3 + 1188**3 =  891**3 +  990**3

```



## Rust


```rust

use std::collections::HashMap;
use itertools::Itertools;

fn cubes(n: u64) -> Vec<u64> {
	let mut cube_vector = Vec::new();
	for i in 1..=n {
		cube_vector.push(i.pow(3));
	}
	cube_vector
}

fn main() {
	let c = cubes(1201);
	let it = c.iter().combinations(2);
	let mut m = HashMap::new();
	for x in it {
		let sum = x[0] + x[1];
		m.entry(sum).or_insert(Vec::new()).push(x)
	}

	let mut result = Vec::new();

	for (k,v) in m.iter() {
		if v.len() > 1 {
			result.push((k,v));
		}
	}

	result.sort();
	for f in result {
		println!("{:?}", f);
	}
}

```

```txt


(1729, [[1, 1728], [729, 1000]])
(4104, [[8, 4096], [729, 3375]])
(13832, [[8, 13824], [5832, 8000]])
(20683, [[1000, 19683], [6859, 13824]])
(32832, [[64, 32768], [5832, 27000]])
(39312, [[8, 39304], [3375, 35937]])
(40033, [[729, 39304], [4096, 35937]])
(46683, [[27, 46656], [19683, 27000]])
(64232, [[4913, 59319], [17576, 46656]])
(65728, [[1728, 64000], [29791, 35937]])
(110656, [[64, 110592], [46656, 64000]])
(110808, [[216, 110592], [19683, 91125]])
(134379, [[1728, 132651], [54872, 79507]])
(149389, [[512, 148877], [24389, 125000]])
(165464, [[8000, 157464], [54872, 110592]])
(171288, [[4913, 166375], [13824, 157464]])
(195841, [[729, 195112], [10648, 185193]])
(216027, [[27, 216000], [10648, 205379]])
(216125, [[125, 216000], [91125, 125000]])
(262656, [[512, 262144], [46656, 216000]])
(314496, [[64, 314432], [27000, 287496]])

```



## Scala


```scala
import scala.math.pow

implicit class Pairs[A, B]( p:List[(A, B)]) {
  def collectPairs: Map[A, List[B]] = p.groupBy(_._1).mapValues(_.map(_._2)).filterNot(_._2.size<2)
}

// Make a sorted List of Taxi Cab Numbers. Limit it to the cube of 1200 because we know it's high enough.
val taxiNums = {
  (1 to 1200).toList            // Start with a sequential list of integers
    .combinations(2).toList     // Find all two number combinations
    .map {
      case a :: b :: nil => ((pow(a, 3) + pow(b, 3)).toInt, (a, b))
      case _ => 0 ->(0, 0)
    }                           // Turn the list into the sum of two cubes and
                                //      remember what we started with, eg. 28->(1,3)
    .collectPairs               // Only keep taxi cab numbers with a duplicate
    .toList.sortBy(_._1)        // Sort the results
}

def output() : Unit = {
  println( "%20s".format( "Taxi Cab Numbers" ) )
  println( "%20s%15s%15s".format( "-"*20, "-"*15, "-"*15 ) )

  taxiNums.take(25) foreach {
    case (p, a::b::Nil) => println( "%20d\t(%d\u00b3 + %d\u00b3)\t\t(%d\u00b3 + %d\u00b3)".format(p,a._1,a._2,b._1,b._2) )
  }

  taxiNums.slice(1999,2007) foreach {
    case (p, a::b::Nil) => println( "%20d\t(%d\u00b3 + %d\u00b3)\t(%d\u00b3 + %d\u00b3)".format(p,a._1,a._2,b._1,b._2) )
  }
}

```

```txt

    Taxi Cab Numbers
--------------------------------------------------
                1729	(1³ + 12³)	(9³ + 10³)
                4104	(2³ + 16³)	(9³ + 15³)
               13832	(2³ + 24³)	(18³ + 20³)
               20683	(10³ + 27³)	(19³ + 24³)
               32832	(4³ + 32³)	(18³ + 30³)
               39312	(2³ + 34³)	(15³ + 33³)
               40033	(9³ + 34³)	(16³ + 33³)
               46683	(3³ + 36³)	(27³ + 30³)
               64232	(17³ + 39³)	(26³ + 36³)
               65728	(12³ + 40³)	(31³ + 33³)
              110656	(4³ + 48³)	(36³ + 40³)
              110808	(6³ + 48³)	(27³ + 45³)
              134379	(12³ + 51³)	(38³ + 43³)
              149389	(8³ + 53³)	(29³ + 50³)
              165464	(20³ + 54³)	(38³ + 48³)
              171288	(17³ + 55³)	(24³ + 54³)
              195841	(9³ + 58³)	(22³ + 57³)
              216027	(3³ + 60³)	(22³ + 59³)
              216125	(5³ + 60³)	(45³ + 50³)
              262656	(8³ + 64³)	(36³ + 60³)
              314496	(4³ + 68³)	(30³ + 66³)
              320264	(18³ + 68³)	(32³ + 66³)
              327763	(30³ + 67³)	(51³ + 58³)
              373464	(6³ + 72³)	(54³ + 60³)
              402597	(42³ + 69³)	(56³ + 61³)
          1671816384	(428³ + 1168³)	(940³ + 944³)
          1672470592	(29³ + 1187³)	(632³ + 1124³)
          1673170856	(458³ + 1164³)	(828³ + 1034³)
          1675045225	(522³ + 1153³)	(744³ + 1081³)
          1675958167	(492³ + 1159³)	(711³ + 1096³)
          1676926719	(63³ + 1188³)	(714³ + 1095³)
          1677646971	(99³ + 1188³)	(891³ + 990³)

```



## Scheme

```scheme

(import (scheme base)
        (scheme write)
        (srfi 1)        ; lists
        (srfi 69)       ; hash tables
        (srfi 132))     ; sorting

(define *max-n* 1500) ; let's go up to here, maximum for x and y
(define *numbers* (make-hash-table eqv?)) ; hash table for total -> list of list of pairs

(define (retrieve key) (hash-table-ref/default *numbers* key '()))

;; add all combinations to the hash table
(do ((i 1 (+ i 1)))
  ((= i *max-n*) )
  (do ((j (+ 1 i) (+ j 1)))
    ((= j *max-n*) )
    (let ((n (+ (* i i i) (* j j j))))
      (hash-table-set! *numbers* n
                       (cons (list i j) (retrieve n))))))

(define (display-number i key)
  (display (+ 1 i)) (display ": ")
  (display key) (display " -> ")
  (display (retrieve key)) (newline))

(let ((sorted-keys (list-sort <
                              (filter (lambda (key) (> (length (retrieve key)) 1))
                                      (hash-table-keys *numbers*)))))
  ;; first 25
  (for-each (lambda (i) (display-number i (list-ref sorted-keys i)))
            (iota 25))
  ;; 2000-2006
  (for-each (lambda (i) (display-number i (list-ref sorted-keys i)))
            (iota 7 1999))
  )

```


```txt

1: 1729 -> ((9 10) (1 12))
2: 4104 -> ((9 15) (2 16))
3: 13832 -> ((18 20) (2 24))
4: 20683 -> ((19 24) (10 27))
5: 32832 -> ((18 30) (4 32))
6: 39312 -> ((15 33) (2 34))
7: 40033 -> ((16 33) (9 34))
8: 46683 -> ((27 30) (3 36))
9: 64232 -> ((26 36) (17 39))
10: 65728 -> ((31 33) (12 40))
11: 110656 -> ((36 40) (4 48))
12: 110808 -> ((27 45) (6 48))
13: 134379 -> ((38 43) (12 51))
14: 149389 -> ((29 50) (8 53))
15: 165464 -> ((38 48) (20 54))
16: 171288 -> ((24 54) (17 55))
17: 195841 -> ((22 57) (9 58))
18: 216027 -> ((22 59) (3 60))
19: 216125 -> ((45 50) (5 60))
20: 262656 -> ((36 60) (8 64))
21: 314496 -> ((30 66) (4 68))
22: 320264 -> ((32 66) (18 68))
23: 327763 -> ((51 58) (30 67))
24: 373464 -> ((54 60) (6 72))
25: 402597 -> ((56 61) (42 69))
2000: 1671816384 -> ((940 944) (428 1168))
2001: 1672470592 -> ((632 1124) (29 1187))
2002: 1673170856 -> ((828 1034) (458 1164))
2003: 1675045225 -> ((744 1081) (522 1153))
2004: 1675958167 -> ((711 1096) (492 1159))
2005: 1676926719 -> ((714 1095) (63 1188))
2006: 1677646971 -> ((891 990) (99 1188))

```



## Sidef

```ruby
var (start=1, end=25) = ARGV.map{.to_i}...
 
func display (h, start, end) {
    var i = start
    for n in [h.grep {|_,v| v.len > 1 }.keys.sort_by{.to_i}[start-1 .. end-1]] {
        printf("%4d %10d  =>\t%s\n", i++, n,
            h{n}.map{ "%4d³ + %-s" % (.first, "#{.last}³") }.join(",\t"))
    }
}
 
var taxi = Hash()
var taxis = 0
var terminate = 0
 
for c1 (1..Inf) {
    if (0<terminate && terminate<c1) {
        display(taxi, start, end)
        break
    }
    var c = c1**3
    for c2 (1..c1) {
        var this = (c2**3 + c)
        taxi{this} := [] << [c2, c1]
        ++taxis if (taxi{this}.len == 2)
        if (taxis==end && !terminate) {
            terminate = taxi.grep{|_,v| v.len > 1 }.keys.map{.to_i}.max.root(3)
        }
    }
}
```

```txt

   1       1729  =>	   9³ + 10³,	   1³ + 12³
   2       4104  =>	   9³ + 15³,	   2³ + 16³
   3      13832  =>	  18³ + 20³,	   2³ + 24³
   4      20683  =>	  19³ + 24³,	  10³ + 27³
   5      32832  =>	  18³ + 30³,	   4³ + 32³
   6      39312  =>	  15³ + 33³,	   2³ + 34³
   7      40033  =>	  16³ + 33³,	   9³ + 34³
   8      46683  =>	  27³ + 30³,	   3³ + 36³
   9      64232  =>	  26³ + 36³,	  17³ + 39³
  10      65728  =>	  31³ + 33³,	  12³ + 40³
  11     110656  =>	  36³ + 40³,	   4³ + 48³
  12     110808  =>	  27³ + 45³,	   6³ + 48³
  13     134379  =>	  38³ + 43³,	  12³ + 51³
  14     149389  =>	  29³ + 50³,	   8³ + 53³
  15     165464  =>	  38³ + 48³,	  20³ + 54³
  16     171288  =>	  24³ + 54³,	  17³ + 55³
  17     195841  =>	  22³ + 57³,	   9³ + 58³
  18     216027  =>	  22³ + 59³,	   3³ + 60³
  19     216125  =>	  45³ + 50³,	   5³ + 60³
  20     262656  =>	  36³ + 60³,	   8³ + 64³
  21     314496  =>	  30³ + 66³,	   4³ + 68³
  22     320264  =>	  32³ + 66³,	  18³ + 68³
  23     327763  =>	  51³ + 58³,	  30³ + 67³
  24     373464  =>	  54³ + 60³,	   6³ + 72³
  25     402597  =>	  56³ + 61³,	  42³ + 69³

```


With passed parameters 2000 and 2006:

```txt

2000 1671816384  =>	 940³ + 944³,	 428³ + 1168³
2001 1672470592  =>	 632³ + 1124³,	  29³ + 1187³
2002 1673170856  =>	 828³ + 1034³,	 458³ + 1164³
2003 1675045225  =>	 744³ + 1081³,	 522³ + 1153³
2004 1675958167  =>	 711³ + 1096³,	 492³ + 1159³
2005 1676926719  =>	 714³ + 1095³,	  63³ + 1188³
2006 1677646971  =>	 891³ + 990³,	  99³ + 1188³

```



## Tcl

```tcl
package require Tcl 8.6

proc heappush {heapName item} {
    upvar 1 $heapName heap
    set idx [lsearch -bisect -index 0 -integer $heap [lindex $item 0]]
    set heap [linsert $heap [expr {$idx + 1}] $item]
}
coroutine cubesum apply {{} {
    yield
    set h {}
    set n 1
    while true {
	while {![llength $h] || [lindex $h 0 0] > $n**3} {
	    heappush h [list [expr {$n**3 + 1}] $n 1]
	    incr n
	}
	set h [lassign $h item]
	yield $item
	lassign $item s x y
	if {[incr y] < $x} {
	    heappush h [list [expr {$x**3 + $y**3}] $x $y]
	}
    }
}}
coroutine taxis apply {{} {
    yield
    set out {{0 0 0}}
    while true {
	set s [cubesum]
	if {[lindex $s 0] == [lindex $out end 0]} {
	    lappend out $s
	} else {
	    if {[llength $out] > 1} {yield $out}
	    set out [list $s]
	}
    }
}}

# Put a cache in front for convenience
variable taxis {}
proc taxi {n} {
    variable taxis
    while {$n > [llength $taxis]} {lappend taxis [taxis]}
    return [lindex $taxis [expr {$n-1}]]
}

set 3 "\u00b3"
for {set n 1} {$n <= 25} {incr n} {
    puts ${n}:[join [lmap t [taxi $n] {format " %d = %d$3 + %d$3" {*}$t}] ","]
}
for {set n 2000} {$n <= 2006} {incr n} {
    puts ${n}:[join [lmap t [taxi $n] {format " %d = %d$3 + %d$3" {*}$t}] ","]
}
```

```txt

1: 1729 = 10³ + 9³, 1729 = 12³ + 1³
2: 4104 = 15³ + 9³, 4104 = 16³ + 2³
3: 13832 = 20³ + 18³, 13832 = 24³ + 2³
4: 20683 = 24³ + 19³, 20683 = 27³ + 10³
5: 32832 = 30³ + 18³, 32832 = 32³ + 4³
6: 39312 = 33³ + 15³, 39312 = 34³ + 2³
7: 40033 = 33³ + 16³, 40033 = 34³ + 9³
8: 46683 = 30³ + 27³, 46683 = 36³ + 3³
9: 64232 = 36³ + 26³, 64232 = 39³ + 17³
10: 65728 = 33³ + 31³, 65728 = 40³ + 12³
11: 110656 = 40³ + 36³, 110656 = 48³ + 4³
12: 110808 = 45³ + 27³, 110808 = 48³ + 6³
13: 134379 = 43³ + 38³, 134379 = 51³ + 12³
14: 149389 = 50³ + 29³, 149389 = 53³ + 8³
15: 165464 = 48³ + 38³, 165464 = 54³ + 20³
16: 171288 = 54³ + 24³, 171288 = 55³ + 17³
17: 195841 = 57³ + 22³, 195841 = 58³ + 9³
18: 216027 = 59³ + 22³, 216027 = 60³ + 3³
19: 216125 = 50³ + 45³, 216125 = 60³ + 5³
20: 262656 = 60³ + 36³, 262656 = 64³ + 8³
21: 314496 = 66³ + 30³, 314496 = 68³ + 4³
22: 320264 = 66³ + 32³, 320264 = 68³ + 18³
23: 327763 = 58³ + 51³, 327763 = 67³ + 30³
24: 373464 = 60³ + 54³, 373464 = 72³ + 6³
25: 402597 = 61³ + 56³, 402597 = 69³ + 42³
2000: 1671816384 = 944³ + 940³, 1671816384 = 1168³ + 428³
2001: 1672470592 = 1124³ + 632³, 1672470592 = 1187³ + 29³
2002: 1673170856 = 1034³ + 828³, 1673170856 = 1164³ + 458³
2003: 1675045225 = 1081³ + 744³, 1675045225 = 1153³ + 522³
2004: 1675958167 = 1096³ + 711³, 1675958167 = 1159³ + 492³
2005: 1676926719 = 1095³ + 714³, 1676926719 = 1188³ + 63³
2006: 1677646971 = 990³ + 891³, 1677646971 = 1188³ + 99³

```



## VBA


```vb
Public Type tuple
    i As Variant
    j As Variant
    sum As Variant
End Type
Public Type tuple3
    i1 As Variant
    j1 As Variant
    i2 As Variant
    j2 As Variant
    i3 As Variant
    j3 As Variant
    sum As Variant
End Type
Sub taxicab_numbers()
    Dim i As Variant, j As Variant
    Dim k As Long
    Const MAX = 2019
    Dim p(MAX) As Variant
    Const bigMAX = (MAX + 1) * (MAX / 2)
    Dim big(1 To bigMAX) As tuple
    Const resMAX = 4400
    Dim res(1 To resMAX) As tuple3
    For i = 1 To MAX
        p(i) = CDec(i * i * i) 'convert Variant to Decimal
    Next i                     'wich hold numbers upto 10^28

    k = 1
    For i = 1 To MAX
        For j = i To MAX
            big(k).i = CDec(i)
            big(k).j = CDec(j)
            big(k).sum = CDec(p(i) + p(j))
            k = k + 1
        Next j
    Next i
    n = 1
    Quicksort big, LBound(big), UBound(big)
    For i = 1 To bigMAX - 1
        If big(i).sum = big(i + 1).sum Then
            res(n).i1 = CStr(big(i).i)
            res(n).j1 = CStr(big(i).j)
            res(n).i2 = CStr(big(i + 1).i)
            res(n).j2 = CStr(big(i + 1).j)
            If big(i + 1).sum = big(i + 2).sum Then
                res(n).i3 = CStr(big(i + 2).i)
                res(n).j3 = CStr(big(i + 2).j)
                i = i + 1
            End If
            res(n).sum = CStr(big(i).sum)
            n = n + 1
            i = i + 1
        End If
    Next i
    Debug.Print n - 1; " taxis"
    For i = 1 To 25
        With res(i)
            Debug.Print String$(4 - Len(CStr(i)), " "); i;
            Debug.Print String$(11 - Len(.sum), " "); .sum; " = ";
            Debug.Print String$(4 - Len(.i1), " "); .i1; "^3 +";
            Debug.Print String$(4 - Len(.j1), " "); .j1; "^3 = ";
            Debug.Print String$(4 - Len(.i2), " "); .i2; "^3 +";
            Debug.Print String$(4 - Len(.j2), " "); .j2; "^3"
        End With
    Next i
    Debug.Print
    For i = 2000 To 2006
        With res(i)
            Debug.Print String$(4 - Len(CStr(i)), " "); i;
            Debug.Print String$(11 - Len(.sum), " "); .sum; " = ";
            Debug.Print String$(4 - Len(.i1), " "); .i1; "^3 +";
            Debug.Print String$(4 - Len(.j1), " "); .j1; "^3 = ";
            Debug.Print String$(4 - Len(.i2), " "); .i2; "^3 +";
            Debug.Print String$(4 - Len(.j2), " "); .j2; "^3"
        End With

    Next i
    Debug.Print
    For i = 1 To resMAX
        If res(i).i3 <> "" Then
            With res(i)
                Debug.Print String$(4 - Len(CStr(i)), " "); i;
                Debug.Print String$(11 - Len(.sum), " "); .sum; " = ";
                Debug.Print String$(4 - Len(.i1), " "); .i1; "^3 +";
                Debug.Print String$(4 - Len(.j1), " "); .j1; "^3 = ";
                Debug.Print String$(4 - Len(.i2), " "); .i2; "^3 +";
                Debug.Print String$(4 - Len(.j2), " "); .j2; "^3";
                Debug.Print String$(4 - Len(.i3), " "); .i3; "^3 +";
                Debug.Print String$(4 - Len(.j3), " "); .j3; "^3"
            End With
        End If
    Next i
End Sub
Sub Quicksort(vArray() As tuple, arrLbound As Long, arrUbound As Long)
    'https://wellsr.com/vba/2018/excel/vba-quicksort-macro-to-sort-arrays-fast/
    'Sorts a one-dimensional VBA array from smallest to largest
    'using a very fast quicksort algorithm variant.
    'Adapted to multidimensions/typedef
    Dim pivotVal As Variant
    Dim vSwap    As tuple
    Dim tmpLow   As Long
    Dim tmpHi    As Long

    tmpLow = arrLbound
    tmpHi = arrUbound
    pivotVal = vArray((arrLbound + arrUbound) \ 2).sum

    While (tmpLow <= tmpHi) 'divide
        While (vArray(tmpLow).sum < pivotVal And tmpLow < arrUbound)
            tmpLow = tmpLow + 1
        Wend

        While (pivotVal < vArray(tmpHi).sum And tmpHi > arrLbound)
            tmpHi = tmpHi - 1
        Wend

        If (tmpLow <= tmpHi) Then
             vSwap.i = vArray(tmpLow).i
             vSwap.j = vArray(tmpLow).j
             vSwap.sum = vArray(tmpLow).sum
             vArray(tmpLow).i = vArray(tmpHi).i
             vArray(tmpLow).j = vArray(tmpHi).j
             vArray(tmpLow).sum = vArray(tmpHi).sum
             vArray(tmpHi).i = vSwap.i
             vArray(tmpHi).j = vSwap.j
             vArray(tmpHi).sum = vSwap.sum
             tmpLow = tmpLow + 1
             tmpHi = tmpHi - 1
        End If
    Wend

    If (arrLbound < tmpHi) Then Quicksort vArray, arrLbound, tmpHi 'conquer
    If (tmpLow < arrUbound) Then Quicksort vArray, tmpLow, arrUbound 'conquer
End Sub
```
```txt
 4399  taxis
    1        1729 =    9^3 +  10^3 =    1^3 +  12^3
    2        4104 =    2^3 +  16^3 =    9^3 +  15^3
    3       13832 =    2^3 +  24^3 =   18^3 +  20^3
    4       20683 =   19^3 +  24^3 =   10^3 +  27^3
    5       32832 =   18^3 +  30^3 =    4^3 +  32^3
    6       39312 =   15^3 +  33^3 =    2^3 +  34^3
    7       40033 =   16^3 +  33^3 =    9^3 +  34^3
    8       46683 =   27^3 +  30^3 =    3^3 +  36^3
    9       64232 =   26^3 +  36^3 =   17^3 +  39^3
   10       65728 =   31^3 +  33^3 =   12^3 +  40^3
   11      110656 =    4^3 +  48^3 =   36^3 +  40^3
   12      110808 =   27^3 +  45^3 =    6^3 +  48^3
   13      134379 =   12^3 +  51^3 =   38^3 +  43^3
   14      149389 =   29^3 +  50^3 =    8^3 +  53^3
   15      165464 =   38^3 +  48^3 =   20^3 +  54^3
   16      171288 =   24^3 +  54^3 =   17^3 +  55^3
   17      195841 =    9^3 +  58^3 =   22^3 +  57^3
   18      216027 =   22^3 +  59^3 =    3^3 +  60^3
   19      216125 =   45^3 +  50^3 =    5^3 +  60^3
   20      262656 =   36^3 +  60^3 =    8^3 +  64^3
   21      314496 =    4^3 +  68^3 =   30^3 +  66^3
   22      320264 =   32^3 +  66^3 =   18^3 +  68^3
   23      327763 =   51^3 +  58^3 =   30^3 +  67^3
   24      373464 =   54^3 +  60^3 =    6^3 +  72^3
   25      402597 =   56^3 +  61^3 =   42^3 +  69^3

 2000  1671816384 =  940^3 + 944^3 =  428^3 +1168^3
 2001  1672470592 =   29^3 +1187^3 =  632^3 +1124^3
 2002  1673170856 =  828^3 +1034^3 =  458^3 +1164^3
 2003  1675045225 =  744^3 +1081^3 =  522^3 +1153^3
 2004  1675958167 =  492^3 +1159^3 =  711^3 +1096^3
 2005  1676926719 =  714^3 +1095^3 =   63^3 +1188^3
 2006  1677646971 =   99^3 +1188^3 =  891^3 + 990^3

  455    87539319 =  167^3 + 436^3 =  228^3 + 423^3 255^3 + 414^3
  535   119824488 =   90^3 + 492^3 =  346^3 + 428^3  11^3 + 493^3
  588   143604279 =  408^3 + 423^3 =  359^3 + 460^3 111^3 + 522^3
  655   175959000 =   70^3 + 560^3 =  315^3 + 525^3 198^3 + 552^3
  888   327763000 =  300^3 + 670^3 =  339^3 + 661^3 510^3 + 580^3
 1299   700314552 =  334^3 + 872^3 =  456^3 + 846^3 510^3 + 828^3
 1398   804360375 =   15^3 + 930^3 =  295^3 + 920^3 198^3 + 927^3
 1515   958595904 =   22^3 + 986^3 =  180^3 + 984^3 692^3 + 856^3
 1660  1148834232 =  718^3 + 920^3 =  816^3 + 846^3 222^3 +1044^3
 1837  1407672000 =  140^3 +1120^3 =  396^3 +1104^3 630^3 +1050^3
 2100  1840667192 =  681^3 +1151^3 =  372^3 +1214^3 225^3 +1223^3
 2143  1915865217 =    9^3 +1242^3 =  484^3 +1217^3 969^3 +1002^3
 2365  2363561613 =  501^3 +1308^3 =  684^3 +1269^3 765^3 +1242^3
 2480  2622104000 = 1020^3 +1160^3 =  600^3 +1340^3 678^3 +1322^3
 2670  3080802816 =  904^3 +1328^3 =   81^3 +1455^3 456^3 +1440^3
 2732  3235261176 =   33^3 +1479^3 =  270^3 +1476^31038^3 +1284^3
 2845  3499524728 =  116^3 +1518^3 =  350^3 +1512^31169^3 +1239^3
 2895  3623721192 =  348^3 +1530^3 =  761^3 +1471^31098^3 +1320^3
 2979  3877315533 = 1224^3 +1269^3 = 1077^3 +1380^3 333^3 +1566^3
 3293  4750893000 =  210^3 +1680^3 =  945^3 +1575^3 594^3 +1656^3
 3562  5544709352 =  207^3 +1769^3 = 1076^3 +1626^3 842^3 +1704^3
 3589  5602516416 =  912^3 +1692^3 = 1020^3 +1656^3 668^3 +1744^3
 3826  6434883000 =  590^3 +1840^3 =   30^3 +1860^3 396^3 +1854^3
 4162  7668767232 =   44^3 +1972^3 = 1384^3 +1712^3 360^3 +1968^3
 4359  8849601000 = 1017^3 +1983^3 = 1530^3 +1740^3 900^3 +2010^3
```


## zkl

An array of bytes is used to hold n, where array[n³+m³]==n.

```zkl
fcn taxiCabNumbers{
   const HeapSZ=0d5_000_000;
   iCubes:=[1..120].apply("pow",3);
   sum2cubes:=Data(HeapSZ).fill(0);  // BFheap of 1 byte zeros
   taxiNums:=List();
   foreach i,i3 in ([1..].zip(iCubes)){
      foreach j,j3 in ([i+1..].zip(iCubes[i,*])){
         ij3:=i3+j3;
	 if(z:=sum2cubes[ij3]){
	    taxiNums.append(T(ij3,
		z,(ij3-z.pow(3)).toFloat().pow(1.0/3).round().toInt(),
		i,j));
	 }
	 else sum2cubes[ij3]=i;
      }
   }
   taxiNums.sort(fcn([(a,_)],[(b,_)]){ a<b })
}
```


```zkl
fcn print(n,taxiNums){
   [n..].zip(taxiNums).pump(Console.println,fcn([(n,t)]){
      "%4d: %10,d = %2d\u00b3 + %2d\u00b3 =  %2d\u00b3 + %2d\u00b3".fmt(n,t.xplode())
   })
}
taxiNums:=taxiCabNumbers();  // 63 pairs
taxiNums[0,25]:print(1,_);
```

```txt

   1:      1,729 =  1³ + 12³ =   9³ + 10³
   2:      4,104 =  2³ + 16³ =   9³ + 15³
   3:     13,832 =  2³ + 24³ =  18³ + 20³
   4:     20,683 = 10³ + 27³ =  19³ + 24³
   5:     32,832 =  4³ + 32³ =  18³ + 30³
   6:     39,312 =  2³ + 34³ =  15³ + 33³
   7:     40,033 =  9³ + 34³ =  16³ + 33³
   8:     46,683 =  3³ + 36³ =  27³ + 30³
   9:     64,232 = 17³ + 39³ =  26³ + 36³
  10:     65,728 = 12³ + 40³ =  31³ + 33³
  11:    110,656 =  4³ + 48³ =  36³ + 40³
  12:    110,808 =  6³ + 48³ =  27³ + 45³
  13:    134,379 = 12³ + 51³ =  38³ + 43³
  14:    149,389 =  8³ + 53³ =  29³ + 50³
  15:    165,464 = 20³ + 54³ =  38³ + 48³
  16:    171,288 = 17³ + 55³ =  24³ + 54³
  17:    195,841 =  9³ + 58³ =  22³ + 57³
  18:    216,027 =  3³ + 60³ =  22³ + 59³
  19:    216,125 =  5³ + 60³ =  45³ + 50³
  20:    262,656 =  8³ + 64³ =  36³ + 60³
  21:    314,496 =  4³ + 68³ =  30³ + 66³
  22:    320,264 = 18³ + 68³ =  32³ + 66³
  23:    327,763 = 30³ + 67³ =  51³ + 58³
  24:    373,464 =  6³ + 72³ =  54³ + 60³
  25:    402,597 = 42³ + 69³ =  56³ + 61³

```

Using a binary heap:

```zkl
fcn cubeSum{
   heap,n:=Heap(fcn([(a,_)],[(b,_)]){ a<=b }), 1;  // heap cnt maxes out @ 244
   while(1){
      while(heap.empty or heap.top[0]>n.pow(3)){ # could also pre-calculate cubes
	 heap.push(T(n.pow(3) + 1, n,1));
	 n+=1;
      }
      s,x,y:= sxy:=heap.pop();
      vm.yield(sxy);
      y+=1;
      if(y<x)    # should be y <= x?
	 heap.push(T(x.pow(3) + y.pow(3), x,y));
   }
}
fcn taxis{
   out:=List(T(0,0,0));
   foreach s in (Utils.Generator(cubeSum)){
      if(s[0]==out[-1][0]) out.append(s);
      else{
	 if(out.len()>1) vm.yield(out);
	 out.clear(s)
      }
   }
}
n:=0;
foreach x in (Utils.Generator(taxis)){
   n += 1;
   if(n >= 2006) break;
   if(n <= 25 or n >= 2000) println(n,": ",x);
}
```

And a quickie heap implementation:

```zkl
class Heap{  // binary heap
   fcn init(lteqFcn='<=){
      var [const, private] heap=List().pad(64,Void); // a power of 2
      var cnt=0, cmp=lteqFcn;
   }
   fcn push(v){
	// Resize the heap if it is too small to hold another item
      if (cnt==heap.len()) heap.pad(cnt*2,Void);

      index:=cnt; cnt+=1; while(index){	 // Find out where to put the element
	 parent:=(index - 1)/2;
	 if(cmp(heap[parent],v)) break;
	 heap[index] = heap[parent];
	 index = parent;
      }
      heap[index] = v;
   }
   fcn pop{  // Remove the biggest element and return it
      if(not cnt) return(Void);
      v,temp:=heap[0], heap[cnt-=1];

      // Reorder the elements
      index:=0; while(1){   // Find the child to swap with
	 swap:=index*2 + 1;
	 if (swap>=cnt) break; // If there are no children, the heap is reordered
	 other:=swap + 1;
	 if(other<cnt and cmp(heap[other],heap[swap])) swap = other;
	 if(cmp(temp,heap[swap])) break; // If the bigger child is less than or equal to its parent, the heap is reordered

	 heap[index]=heap[swap];
	 index = swap;
      }
      heap[index] = temp;
      v
   }
   var [proxy] top=fcn  { if(cnt==0) Void else heap[0] };
   var [proxy] empty=fcn{ (not cnt) };
}
```

```txt

1: L(L(1729,10,9),L(1729,12,1))
...
23: L(L(327763,67,30),L(327763,58,51))
24: L(L(373464,60,54),L(373464,72,6))
25: L(L(402597,61,56),L(402597,69,42))
2000: L(L(1671816384,944,940),L(1671816384,1168,428))
2001: L(L(1672470592,1124,632),L(1672470592,1187,29))
2002: L(L(1673170856,1034,828),L(1673170856,1164,458))
2003: L(L(1675045225,1153,522),L(1675045225,1081,744))
2004: L(L(1675958167,1096,711),L(1675958167,1159,492))
2005: L(L(1676926719,1188,63),L(1676926719,1095,714))

```



## ZX Spectrum Basic

This will, in the strictest sense, work. Don't hold your breath though; after six hours on an emulator at full speed it had generated the first 10 numbers. Getting to 2006 may take a while longer.

You cannot fit the whole 1625-entry table of cubes (and this program on top) into the 16K ZX Spectrum. Replace all 1625s with 1200s to resolve; numerically unjustified as an exhaustive search, but we know this will be sufficient to find the 2006th number. Eventually.


```zxbasic
10 DIM f(1625): REM populating a cube table at the start will be faster than computing the cubes on the fly
20 FOR x=1 TO 1625
30 LET f(x)=x*x*x: REM x*x*x rather than x^3 as the ZX Spectrum's exponentiation function is legendarily slow
40 NEXT x
50 LET c=0
60 FOR x=1 TO 4294967295: REM the highest number the ZX Spectrum Basic can accurately hold internally; floor (cuberoot max)=1625, hence the table limit
70 LET k=0
80 FOR m=1 TO 1625
90 FOR n=m+1 TO 1625
100 IF f(m)+f(n)=x THEN GOTO 160
110 IF f(n)>=x THEN LET n=1625: REM overshot, break out of the loop
120 IF f(m)>=x THEN LET m=1625
130 NEXT n
140 NEXT m
150 NEXT x
160 IF k=1 THEN LET q=m: LET r=n: GO TO 230: REM got one!
170 LET o=m
180 LET p=n
190 LET k=1
200 NEXT n
210 NEXT m
220 NEXT x
230 LET c=c+1
240 IF c>25 AND c<2000 THEN GO TO 330
250 LET t$="": REM convert number to string; while ZX Spectrum Basic can store all the digits of integers up to 2^32-1...
260 LET t=INT (x/100000): REM ...it will resort to scientific notation trying to display any more than eight digits
270 LET b=x-t*100000
280 IF t=0 THEN GO TO 300: REM omit leading zero
290 LET t$=STR$ t
300 LET t$=t$+STR$ b
310 PRINT c;":";t$;"=";q;"^3+";r;"^3=";o;"^3+";p;"^3"
320 POKE 23692,10: REM suppress "scroll?" prompt when screen fills up at c=22
330 IF c=2006 THEN LET x=4294967295: LET n=1625: LET m=1625
340 NEXT n
350 NEXT m
360 NEXT x
```


```txt
1:1729=9^3+10^3=1^3+12^3
2:4104=9^3+15^3=2^3+16^3
3:13832=18^3+20^3=2^3+24^3
4:20683=19^3+24^3=10^3+27^3
5:32832=18^3+30^3=4^3+32^3
6:39312=15^3+33^3=2^3+34^3
7:40033=16^3+33^3=9^3+34^3
8:46683=27^3+30^3=3^3+36^3
9:64232=26^3+36^3=17^3+39^3
10:65728=31^3+33^3=12^3+40^3

D BREAK into program, 100:1
```


This program produces the first 25 Taxicab numbers. It is written with speed in mind.
The runtime is about 45 minutes on a ZX Spectrum (3.5 Mhz).

```zxbasic
  10 LET T=0: DIM F(72): LET D=0: LET S=0: LET B=0: LET A=0: LET C=0
  20 DIM H(50): DIM Y(50,2): FOR D=1 TO 72: LET F(D)=D*D*D: NEXT D
  30 FOR A=1 TO 58: FOR B=A+1 TO 72: LET S=F(A)+F(B): FOR D=B-1 TO A STEP -1
  40 LET T=S-F(D): IF T>F(D) THEN NEXT B: NEXT A: GO TO 90
  45 IF s>405224 THEN GO TO 70
  50 IF F(INT (EXP (LN (T)/3)+.5))=T THEN GO TO 80
  60 NEXT D
  70 NEXT B: NEXT A: GO TO 90
  80 PRINT S,: LET C=C+1: LET H(C)=S: LET Y(C,1)=A*65536+B: LET Y(C,2)=INT (EXP (LN (T)/3)+.5)*65536+D: GO TO 70
  90 LET S=INT (C/2)
 100 LET T=0: FOR A=1 TO C-S: IF H(A)>H(A+S) THEN LET T=H(A): LET H(A)=H(A+S): LET H(A+S)=T: LET T=Y(A,1): LET Y(A,1)=Y(A+S,1): LET Y(A+S,1)=T: LET T=Y(A,2): LET Y(A,2)=Y(A+S,2): LET Y(A+S,2)=T
 110 NEXT A: IF T<>0 THEN GO TO 100
 120 IF S<>1 THEN LET S=INT (S/2): GO TO 100
 130 CLS : FOR A=1 TO 25: PRINT A;":";H(A);"=";
 131 LPRINT A;":";H(A);"=";:
 140 LET T=INT (Y(A,1)/65536): PRINT T;"^3+";Y(A,1)-T*65536;"^3=";
 141 LPRINT T;"^3+";Y(A,1)-T*65536;"^3=";
 150 LET T=INT (Y(A,2)/65536): PRINT T;"^3+";Y(A,2)-T*65536;"^3"
 151 LPRINT T;"^3+";Y(A,2)-T*65536;"^3"
 160 NEXT A: PRINT
 170 STOP
```

```txt
1:1729=1^3+12^3=9^3+10^3
2:4104=2^3+16^3=9^3+15^3
3:13832=2^3+24^3=18^3+20^3
4:20683=10^3+27^3=19^3+24^3
5:32832=4^3+32^3=18^3+30^3
6:39312=2^3+34^3=15^3+33^3
7:40033=9^3+34^3=16^3+33^3
8:46683=3^3+36^3=27^3+30^3
9:64232=17^3+39^3=26^3+36^3
10:65728=12^3+40^3=31^3+33^3
11:110656=4^3+48^3=36^3+40^3
12:110808=6^3+48^3=27^3+45^3
13:134379=12^3+51^3=38^3+43^3
14:149389=8^3+53^3=29^3+50^3
15:165464=20^3+54^3=38^3+48^3
16:171288=17^3+55^3=24^3+54^3
17:195841=9^3+58^3=22^3+57^3
18:216027=3^3+60^3=22^3+59^3
19:216125=5^3+60^3=45^3+50^3
20:262656=8^3+64^3=36^3+60^3
21:314496=4^3+68^3=30^3+66^3
22:320264=18^3+68^3=32^3+66^3
23:327763=30^3+67^3=51^3+58^3
24:373464=6^3+72^3=54^3+60^3
25:402597=42^3+69^3=56^3+61^3
```

