+++
title = "Average loop length"
description = ""
date = 2019-10-06T04:52:36Z
aliases = []
[extra]
id = 12743
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "bbc_basic",
  "c",
  "clojure",
  "cpp",
  "d",
  "echolisp",
  "elixir",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "liberty_basic",
  "nim",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "simula",
  "tcl",
  "unicon",
  "vba",
  "zkl",
]
+++

## Task
Let <code>f</code> be a uniformly-randomly chosen mapping from the numbers 1..N to the numbers 1..N (note: not necessarily a permutation of 1..N; the mapping could produce a number in more than one way or not at all). At some point, the sequence <code>1, f(1), f(f(1))...</code> will contain a <em>repetition</em>, a number that occurring for the second time in the sequence.


;Task:
Write a program or a script that estimates, for each <code>N</code>, the average length until the first such repetition.

Also calculate this expected length using an analytical formula, and optionally compare the simulated result with the theoretical one.


This problem comes from the end of Donald Knuth's [http://www.youtube.com/watch?v=cI6tt9QfRdo Christmas tree lecture 2011].

Example of expected output:


```txt
 N    average    analytical    (error)

###   =========  ============  ======

  1     1.0000        1.0000  (  0.00%)
  2     1.4992        1.5000  (  0.05%)
  3     1.8784        1.8889  (  0.56%)
  4     2.2316        2.2188  (  0.58%)
  5     2.4982        2.5104  (  0.49%)
  6     2.7897        2.7747  (  0.54%)
  7     3.0153        3.0181  (  0.09%)
  8     3.2429        3.2450  (  0.07%)
  9     3.4536        3.4583  (  0.14%)
 10     3.6649        3.6602  (  0.13%)
 11     3.8091        3.8524  (  1.12%)
 12     3.9986        4.0361  (  0.93%)
 13     4.2074        4.2123  (  0.12%)
 14     4.3711        4.3820  (  0.25%)
 15     4.5275        4.5458  (  0.40%)
 16     4.6755        4.7043  (  0.61%)
 17     4.8877        4.8579  (  0.61%)
 18     4.9951        5.0071  (  0.24%)
 19     5.1312        5.1522  (  0.41%)
 20     5.2699        5.2936  (  0.45%)
```





## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Discrete_Random;
procedure Avglen is
   package IIO is new Ada.Text_IO.Integer_IO (Positive); use IIO;
   package LFIO is new Ada.Text_IO.Float_IO (Long_Float); use LFIO;
   subtype FactN is Natural range 0..20;
   TESTS : constant Natural := 1_000_000;

   function Factorial (N : FactN) return Long_Float is
      Result : Long_Float := 1.0;
   begin
      for I in 2..N loop Result := Result * Long_Float(I); end loop;
      return Result;
   end Factorial;

   function Analytical (N : FactN) return Long_Float is
      Sum : Long_Float := 0.0;
   begin
      for I in 1..N loop
         Sum := Sum + Factorial(N) / Factorial(N - I) / Long_Float(N)**I;
      end loop;
      return Sum;
   end Analytical;

   function Experimental (N : FactN) return Long_Float is
      subtype RandInt is Natural range 1..N;
      package Random is new Ada.Numerics.Discrete_Random(RandInt);
      seed : Random.Generator;
      Num : RandInt;
      count : Natural := 0;
      bits : array(RandInt'Range) of Boolean;
   begin
      Random.Reset(seed);
      for run in 1..TESTS loop
         bits := (others  => false);
         for I in RandInt'Range loop
            Num := Random.Random(seed); exit when bits(Num);
            bits(Num) := True; count := count + 1;
         end loop;
      end loop;
      return Long_Float(count)/Long_Float(TESTS);
   end Experimental;

   A, E, err : Long_Float;
begin
   Put_Line(" N  avg    calc   %diff");
   for I in 1..20 loop
      A := Analytical(I);  E := Experimental(I); err := abs(E-A)/A*100.0;
      Put(I, Width=>2); Put(E ,Aft=>4, exp=>0); Put(A, Aft=>4, exp=>0);
      Put(err, Fore=>3, Aft=>3, exp=>0); New_line;
   end loop;
end Avglen;
```

{{out}}

```txt

 N  avg    calc   %diff
 1 1.0000 1.0000  0.000
 2 1.5000 1.5000  0.003
 3 1.8886 1.8889  0.015
 4 2.2180 2.2188  0.033
 5 2.5104 2.5104  0.000
 6 2.7745 2.7747  0.006
 7 3.0191 3.0181  0.033
 8 3.2433 3.2450  0.052
 9 3.4583 3.4583  0.001
10 3.6597 3.6602  0.015
11 3.8524 3.8524  0.001
12 4.0352 4.0361  0.022
13 4.2147 4.2123  0.055
14 4.3853 4.3820  0.075
15 4.5453 4.5458  0.011
16 4.7055 4.7043  0.027
17 4.8592 4.8579  0.028
18 5.0062 5.0071  0.017
19 5.1535 5.1522  0.025
20 5.2955 5.2936  0.035

```



## BBC BASIC


```bbcbasic
      @% = &2040A
      MAX_N = 20
      TIMES = 1000000

      FOR n = 1 TO MAX_N
        avg = FNtest(n, TIMES)
        theory = FNanalytical(n)
        diff = (avg / theory - 1) * 100
        PRINT STR$(n), avg, theory, diff "%"
      NEXT
      END

      DEF FNanalytical(n)
      LOCAL i, s
      FOR i = 1 TO n
        s += FNfactorial(n) / n^i / FNfactorial(n-i)
      NEXT
      = s

      DEF FNtest(n, times)
      LOCAL i, b, c, x
      FOR i = 1 TO times
        x = 1 : b = 0
        WHILE (b AND x) = 0
          c += 1
          b OR= x
          x = 1 << (RND(n) - 1)
        ENDWHILE
      NEXT
      = c / times

      DEF FNfactorial(n)
      IF n=1 OR n=0 THEN =1 ELSE = n * FNfactorial(n-1)
```

{{out}}

```txt

1             1.0000    1.0000    0.0000%
2             1.4995    1.5000   -0.0366%
3             1.8879    1.8889   -0.0509%
4             2.2193    2.2188    0.0240%
5             2.5105    2.5104    0.0057%
6             2.7755    2.7747    0.0293%
7             3.0199    3.0181    0.0573%
8             3.2396    3.2450   -0.1664%
9             3.4562    3.4583   -0.0609%
10            3.6578    3.6602   -0.0659%
11            3.8523    3.8524   -0.0025%
12            4.0336    4.0361   -0.0602%
13            4.2139    4.2123    0.0366%
14            4.3816    4.3820   -0.0105%
15            4.5432    4.5458   -0.0570%
16            4.7108    4.7043    0.1386%
17            4.8578    4.8579   -0.0018%
18            5.0063    5.0071   -0.0144%
19            5.1564    5.1522    0.0814%
20            5.2945    5.2936    0.0166%

```



## C



```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define MAX_N 20
#define TIMES 1000000

double factorial(int n) {
	double f = 1;
	int i;
	for (i = 1; i <= n; i++) f *= i;
	return f;
}

double expected(int n) {
	double sum = 0;
	int i;
	for (i = 1; i <= n; i++)
		sum += factorial(n) / pow(n, i) / factorial(n - i);
	return sum;
}

int randint(int n) {
	int r, rmax = RAND_MAX / n * n;
	while ((r = rand()) >= rmax);
	return r / (RAND_MAX / n);
}

int test(int n, int times) {
	int i, count = 0;
	for (i = 0; i < times; i++) {
		int x = 1, bits = 0;
		while (!(bits & x)) {
			count++;
			bits |= x;
			x = 1 << randint(n);
		}
	}
	return count;
}

int main(void) {
	srand(time(0));
	puts(" n\tavg\texp.\tdiff\n-------------------------------");

	int n;
	for (n = 1; n <= MAX_N; n++) {
		int cnt = test(n, TIMES);
		double avg = (double)cnt / TIMES;
		double theory = expected(n);
		double diff = (avg / theory - 1) * 100;
		printf("%2d %8.4f %8.4f %6.3f%%\n", n, avg, theory, diff);
	}
	return 0;
}
```

{{out}}

```txt

 n      avg     exp.    diff
-------------------------------
 1   1.0000   1.0000  0.000%
 2   1.4998   1.5000 -0.015%
 3   1.8879   1.8889 -0.051%
 4   2.2181   2.2188 -0.029%
 5   2.5107   2.5104  0.012%
 6   2.7741   2.7747 -0.021%
 7   3.0168   3.0181 -0.044%
 8   3.2455   3.2450  0.014%
 9   3.4591   3.4583  0.023%
10   3.6596   3.6602 -0.017%
11   3.8519   3.8524 -0.013%
12   4.0384   4.0361  0.059%
13   4.2106   4.2123 -0.042%
14   4.3840   4.3820  0.044%
15   4.5449   4.5458 -0.020%
16   4.7058   4.7043  0.033%
17   4.8549   4.8579 -0.060%
18   5.0084   5.0071  0.026%
19   5.1479   5.1522 -0.084%
20   5.2957   5.2936  0.040%

```



## C++

Partial translation of C using stl and std.

```cpp
#include <random>
#include <vector>
#include <iostream>

#define MAX_N 20
#define TIMES 1000000

/**
 * Used to generate a uniform random distribution
 */
static std::random_device rd;  //Will be used to obtain a seed for the random number engine
static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
static std::uniform_int_distribution<> dis;

int randint(int n) {
    int r, rmax = RAND_MAX / n * n;
    dis=std::uniform_int_distribution(0,rmax) ;
    r = dis(gen);
    return r / (RAND_MAX / n);
}

unsigned long factorial(size_t n) {
    //Factorial using dynamic programming to memoize the values.
    static std::vector<unsigned long>factorials{1,1,2};
	for (;factorials.size() <= n;)
	    factorials.push_back(factorials.back()*factorials.size());
	return factorials[n];
}

long double expected(size_t n) {
    long double sum = 0;
    for (size_t i = 1; i <= n; i++)
        sum += factorial(n) / pow(n, i) / factorial(n - i);
    return sum;
}

int test(int n, int times) {
    int i, count = 0;
    for (i = 0; i < times; i++) {
        unsigned int x = 1, bits = 0;
        while (!(bits & x)) {
            count++;
            bits |= x;
            x = static_cast<unsigned int>(1 << randint(n));
        }
    }
    return count;
}

int main() {
    puts(" n\tavg\texp.\tdiff\n-------------------------------");

    int n;
    for (n = 1; n <= MAX_N; n++) {
        int cnt = test(n, TIMES);
        long double avg = (double)cnt / TIMES;
        long double theory = expected(static_cast<size_t>(n));
        long double diff = (avg / theory - 1) * 100;
        printf("%2d %8.4f %8.4f %6.3f%%\n", n, static_cast<double>(avg), static_cast<double>(theory), static_cast<double>(diff));
    }
    return 0;
}

```

{{out}}

```txt

  n      avg     exp.    diff
-------------------------------
 1   1.0000   1.0000  0.000%
 2   1.4998   1.5000 -0.016%
 3   1.8883   1.8889 -0.032%
 4   2.2188   2.2188  0.000%
 5   2.5105   2.5104  0.004%
 6   2.7760   2.7747  0.047%
 7   3.0180   3.0181 -0.004%
 8   3.2448   3.2450 -0.007%
 9   3.4580   3.4583 -0.010%
10   3.6614   3.6602  0.032%
11   3.8532   3.8524  0.022%
12   4.0349   4.0361 -0.029%
13   4.2153   4.2123  0.070%
14   4.3819   4.3820 -0.003%
15   4.5494   4.5458  0.079%
16   4.7082   4.7043  0.084%
17   4.8576   4.8579 -0.005%
18   5.0028   5.0071 -0.084%
19   5.1484   5.1522 -0.073%
20   5.2939   5.2936  0.006%


```



## Clojure

{{trans|Python}}

```lisp
(ns cyclelengths
  (:gen-class))

(defn factorial [n]
  " n! "
  (apply *' (range 1 (inc n))))             ; Use *' (vs. *) to allow arbitrary length arithmetic

(defn pow [n i]
  " n^i"
  (apply *' (repeat i n)))

(defn analytical [n]
  " Analytical Computation "
  (->>(range 1 (inc n))
      (map #(/ (factorial n) (pow n %) (factorial (- n %)))) ;calc n %))
      (reduce + 0)))

;; Number of random times to test each n
(def TIMES 1000000)

(defn single-test-cycle-length [n]
  " Single random test of cycle length "
  (loop [count 0
         bits 0
         x 1]
    (if (zero? (bit-and x bits))
      (recur (inc count) (bit-or bits x) (bit-shift-left 1 (rand-int n)))
        count)))

(defn avg-cycle-length [n times]
  " Average results of single tests of cycle lengths "
  (/
   (reduce +
           (for [i (range times)]
             (single-test-cycle-length n)))
  times))

;; Show Results
(println "\tAvg\t\tExp\t\tDiff")
(doseq [q (range 1 21)
        :let [anal (double (analytical q))
              avg (double (avg-cycle-length q TIMES))
              diff (Math/abs (* 100 (- 1 (/ avg anal))))]]
  (println (format "%3d\t%.4f\t%.4f\t%.2f%%" q avg anal diff)))

```

{{Output}}

```txt

	Avg	Exp	Diff
  1	1.0000	1.0000	0.00%
  2	1.4995	1.5000	0.03%
  3	1.8899	1.8889	0.05%
  4	2.2178	2.2188	0.04%
  5	2.5118	2.5104	0.06%
  6	2.7773	2.7747	0.09%
  7	3.0177	3.0181	0.02%
  8	3.2448	3.2450	0.01%
  9	3.4587	3.4583	0.01%
 10	3.6594	3.6602	0.02%
 11	3.8553	3.8524	0.08%
 12	4.0335	4.0361	0.06%
 13	4.2113	4.2123	0.03%
 14	4.3823	4.3820	0.01%
 15	4.5491	4.5458	0.07%
 16	4.7035	4.7043	0.02%
 17	4.8580	4.8579	0.00%
 18	5.0050	5.0071	0.04%
 19	5.1543	5.1522	0.04%
 20	5.2956	5.2936	0.04%

```



## D

{{trans|Perl 6}}

```d
import std.stdio, std.random, std.math, std.algorithm, std.range, std.format;

real analytical(in int n) pure nothrow @safe /*@nogc*/ {
    enum aux = (int k) => reduce!q{a * b}(1.0L, iota(n - k + 1, n + 1));
    return iota(1, n + 1)
           .map!(k => (aux(k) * k ^^ 2) / (real(n) ^^ (k + 1)))
           .sum;
}

size_t loopLength(size_t maxN)(in int size, ref Xorshift rng) {
    __gshared static bool[maxN + 1] seen;
    seen[0 .. size + 1] = false;
    int current = 1;
    size_t steps = 0;
    while (!seen[current]) {
        seen[current] = true;
        current = uniform(1, size + 1, rng);
        steps++;
    }
    return steps;
}

void main() {
    enum maxN  = 40;
    enum nTrials = 300_000;
    auto rng = Xorshift(unpredictableSeed);
    writeln(" n    average    analytical     (error)");
    writeln("
###   =========  ============  =======
");

    foreach (immutable n; 1 .. maxN + 1) {
        long total = 0;
        foreach (immutable _; 0 .. nTrials)
            total += loopLength!maxN(n, rng);
        immutable average = total / real(nTrials);
        immutable an = n.analytical;
        immutable percentError = abs(an - average) / an * 100;
        immutable errorS = format("%2.4f", percentError);
        writefln("%3d  %9.5f  %12.5f  (%7s%%)",
                 n, average, an, errorS);
    }
}
```

{{out}}

```txt
 n    average    analytical     (error)

###   =========  ============  =======

  1    1.00000       1.00000  ( 0.0000%)
  2    1.50017       1.50000  ( 0.0111%)
  3    1.88932       1.88889  ( 0.0226%)
  4    2.21795       2.21875  ( 0.0362%)
  5    2.51159       2.51040  ( 0.0474%)
  6    2.77373       2.77469  ( 0.0345%)
  7    3.01894       3.01814  ( 0.0264%)
  8    3.24734       3.24502  ( 0.0716%)
  9    3.45876       3.45832  ( 0.0127%)
 10    3.66595       3.66022  ( 0.1567%)
 11    3.85000       3.85237  ( 0.0616%)
 12    4.03532       4.03607  ( 0.0187%)
 13    4.20879       4.21235  ( 0.0843%)
 14    4.37664       4.38203  ( 0.1230%)
 15    4.54986       4.54581  ( 0.0892%)
 16    4.70431       4.70426  ( 0.0010%)
 17    4.85640       4.85787  ( 0.0302%)
 18    5.01359       5.00706  ( 0.1303%)
 19    5.15487       5.15220  ( 0.0519%)
 20    5.29486       5.29358  ( 0.0241%)
 21    5.43276       5.43150  ( 0.0231%)
 22    5.56570       5.56620  ( 0.0088%)
 23    5.70611       5.69788  ( 0.1443%)
 24    5.82618       5.82675  ( 0.0098%)
 25    5.94846       5.95298  ( 0.0759%)
 26    6.07440       6.07672  ( 0.0381%)
 27    6.20717       6.19811  ( 0.1461%)
 28    6.31546       6.31729  ( 0.0290%)
 29    6.44201       6.43437  ( 0.1187%)
 30    6.54592       6.54946  ( 0.0540%)
 31    6.65818       6.66265  ( 0.0671%)
 32    6.77215       6.77405  ( 0.0279%)
 33    6.88381       6.88372  ( 0.0013%)
 34    6.99790       6.99175  ( 0.0880%)
 35    7.10990       7.09820  ( 0.1648%)
 36    7.20391       7.20316  ( 0.0104%)
 37    7.30085       7.30667  ( 0.0796%)
 38    7.40366       7.40880  ( 0.0693%)
 39    7.51864       7.50959  ( 0.1204%)
 40    7.60255       7.60911  ( 0.0863%)
```



## EchoLisp


```scheme

(lib 'math) ;; Σ aka (sigma f(n)  nfrom nto)

(define (f-count  N (times 100000))
    (define count 0)
    (for ((i times))

    ;; new  random f mapping from  0..N-1 to 0..N-1
    ;; (f n) is NOT (random N)
    ;; because each call (f n) must return the same value

    (define f (build-vector N  (lambda(i) (random N))))

    (define hits (make-vector N))
        (define n 0)
        (while (zero? [hits n])
            (++ count)
            (vector+= hits n 1)
            (set! n [f n])))
    (// count times))

(define (f-anal N)
    (Σ  (lambda(i) (// (! N) (! (- N i)) (^  N i))) 1 N))

(decimals 5)
(define (f-print (maxN 21))
	(for ((N (in-range 1 maxN)))
	(define fc (f-count N))
	(define fa (f-anal N))
	(printf  "%3d %10d %10d  %10.2d %%" N fc fa (// (abs (- fa fc)) fc 0.01))))

```

{{out}}

```txt

(f-print)
  1          1          1          0 %
  2    1.49908        1.5       0.06 %
  3    1.89059    1.88889       0.09 %
  4    2.21709    2.21875       0.07 %
  5    2.50629     2.5104       0.16 %
  6    2.77027    2.77469       0.16 %
  7    3.01739    3.01814       0.02 %
  8    3.23934    3.24502       0.18 %
  9    3.45862    3.45832       0.01 %
 10    3.65959    3.66022       0.02 %
 11    3.85897    3.85237       0.17 %
 12    4.04188    4.03607       0.14 %
 13    4.21226    4.21235          0 %
 14    4.38021    4.38203       0.04 %
 15    4.54158    4.54581       0.09 %
 16    4.70633    4.70426       0.04 %
 17    4.86109    4.85787       0.07 %
 18    4.99903    5.00706       0.16 %
 19    5.15873     5.1522       0.13 %
 20    5.30243    5.29358       0.17 %

```




## Elixir

{{trans|Ruby}}
{{works with|Elixir|1.1+}}

```elixir
defmodule RC do
  def factorial(0), do: 1
  def factorial(n), do: Enum.reduce(1..n, 1, &(&1 * &2))

  def loop_length(n), do: loop_length(n, MapSet.new)

  defp loop_length(n, set) do
    r = :rand.uniform(n)
    if r in set, do: MapSet.size(set), else: loop_length(n, MapSet.put(set, r))
  end

  def task(runs) do
    IO.puts " N    average   analytical   (error) "
    IO.puts "
###   =========  ==========  ======
"
    Enum.each(1..20, fn n ->
      avg = Enum.reduce(1..runs, 0, fn _,sum -> sum + loop_length(n) end) / runs
      analytical = Enum.reduce(1..n, 0, fn i,sum ->
        sum + (factorial(n) / :math.pow(n, i) / factorial(n-i))
      end)
      :io.format "~3w  ~9.4f   ~9.4f  (~6.2f%)~n", [n, avg, analytical, abs(avg/analytical - 1)*100]
    end)
  end
end

runs = 1_000_000
RC.task(runs)
```


{{out}}

```txt

 N    average   analytical   (error)

###   =========  ==========  ======

  1     1.0000      1.0000  (  0.00%)
  2     1.5001      1.5000  (  0.00%)
  3     1.8892      1.8889  (  0.02%)
  4     2.2189      2.2188  (  0.01%)
  5     2.5113      2.5104  (  0.04%)
  6     2.7749      2.7747  (  0.01%)
  7     3.0185      3.0181  (  0.01%)
  8     3.2456      3.2450  (  0.02%)
  9     3.4612      3.4583  (  0.08%)
 10     3.6573      3.6602  (  0.08%)
 11     3.8524      3.8524  (  0.00%)
 12     4.0357      4.0361  (  0.01%)
 13     4.2102      4.2123  (  0.05%)
 14     4.3813      4.3820  (  0.02%)
 15     4.5422      4.5458  (  0.08%)
 16     4.7057      4.7043  (  0.03%)
 17     4.8581      4.8579  (  0.01%)
 18     5.0045      5.0071  (  0.05%)
 19     5.1533      5.1522  (  0.02%)
 20     5.2951      5.2936  (  0.03%)

```


=={{header|F_Sharp|F#}}==
{{trans|Scala}}
<p>But uses the Gamma function instead of factorials.</p>

```fsharp
open System

let gamma z =
    let lanczosCoefficients = [76.18009172947146;-86.50532032941677;24.01409824083091;-1.231739572450155;0.1208650973866179e-2;-0.5395239384953e-5]
    let rec sumCoefficients acc i coefficients =
        match coefficients with
        | []   -> acc
        | h::t -> sumCoefficients (acc + (h/i)) (i+1.0) t
    let gamma = 5.0
    let x = z - 1.0
    Math.Pow(x + gamma + 0.5, x + 0.5) * Math.Exp( -(x + gamma + 0.5) ) * Math.Sqrt( 2.0 * Math.PI ) * sumCoefficients 1.000000000190015 (x + 1.0) lanczosCoefficients

let factorial n = gamma ((float n) + 1.)

let expected n =
    seq {for i in 1 .. n do yield (factorial n) / System.Math.Pow((float n), (float i)) / (factorial (n - i)) }
    |> Seq.sum

let r = System.Random()

let trial n =
    let count = ref 0
    let x = ref 1
    let bits = ref 0
    while (!bits &&& !x) = 0 do
        count := !count + 1
        bits := !bits ||| !x
        x := 1 <<< r.Next(n)
    !count


let tested n times = (float (Seq.sum (seq { for i in 1 .. times do yield (trial n) }))) / (float times)

let results = seq {
    for n in 1 .. 20 do
        let avg = tested n 1000000
        let theory = expected n
        yield n, avg, theory
    }


[<EntryPoint>]
let main argv =
    printfn " N     average   analytical   (error)"
    printfn "------------------------------------"
    results
    |> Seq.iter (fun (n, avg, theory) ->
        printfn "%2i    %2.6f    %2.6f    %+2.3f%%" n avg theory ((avg / theory - 1.) * 100.))
    0

```

{{out}}

```txt
 N     average   analytical   (error)
------------------------------------
 1    1.000000    1.000000    +0.000%
 2    1.498934    1.500000    -0.071%
 3    1.889318    1.888889    +0.023%
 4    2.219397    2.218750    +0.029%
 5    2.510618    2.510400    +0.009%
 6    2.771914    2.774691    -0.100%
 7    3.014726    3.018139    -0.113%
 8    3.245022    3.245018    +0.000%
 9    3.457096    3.458316    -0.035%
10    3.660337    3.660216    +0.003%
11    3.849770    3.852372    -0.068%
12    4.038977    4.036074    +0.072%
13    4.213248    4.212348    +0.021%
14    4.380451    4.382029    -0.036%
15    4.541868    4.545807    -0.087%
16    4.704117    4.704258    -0.003%
17    4.858934    4.857871    +0.022%
18    5.004236    5.007063    -0.056%
19    5.154166    5.152196    +0.038%
20    5.298119    5.293585    +0.086%
```



## Go


```go
package main

import (
    "fmt"
    "math"
    "math/rand"
)

const nmax = 20

func main() {
    fmt.Println(" N    average    analytical    (error)")
    fmt.Println("
###   =========  ============  ======
")
    for n := 1; n <= nmax; n++ {
        a := avg(n)
        b := ana(n)
        fmt.Printf("%3d  %9.4f  %12.4f  (%6.2f%%)\n",
            n, a, b, math.Abs(a-b)/b*100)
    }
}

func avg(n int) float64 {
    const tests = 1e4
    sum := 0
    for t := 0; t < tests; t++ {
        var v [nmax]bool
        for x := 0; !v[x]; x = rand.Intn(n) {
            v[x] = true
            sum++
        }
    }
    return float64(sum) / tests
}

func ana(n int) float64 {
    nn := float64(n)
    term := 1.
    sum := 1.
    for i := nn - 1; i >= 1; i-- {
        term *= i / nn
        sum += term
    }
    return sum
}
```

{{out}}

```txt

 N    average    analytical    (error)

###   =========  ============  ======

  1     1.0000        1.0000  (  0.00%)
  2     1.5007        1.5000  (  0.05%)
  3     1.8959        1.8889  (  0.37%)
  4     2.2138        2.2188  (  0.22%)
  5     2.5013        2.5104  (  0.36%)
  6     2.7940        2.7747  (  0.70%)
  7     3.0197        3.0181  (  0.05%)
  8     3.2715        3.2450  (  0.82%)
  9     3.4147        3.4583  (  1.26%)
 10     3.6758        3.6602  (  0.43%)
 11     3.8672        3.8524  (  0.38%)
 12     4.0309        4.0361  (  0.13%)
 13     4.2153        4.2123  (  0.07%)
 14     4.3380        4.3820  (  1.00%)
 15     4.5030        4.5458  (  0.94%)
 16     4.7563        4.7043  (  1.11%)
 17     4.8616        4.8579  (  0.08%)
 18     4.9933        5.0071  (  0.27%)
 19     5.1534        5.1522  (  0.02%)
 20     5.3031        5.2936  (  0.18%)

```



## Haskell


```Haskell
import System.Random
import qualified Data.Set as S
import Text.Printf

findRep :: (Random a, Integral a, RandomGen b) => a -> b -> (a, b)
findRep n gen = findRep' (S.singleton 1) 1 gen
    where
      findRep' seen len gen'
          | S.member fx seen = (len, gen'')
          | otherwise        = findRep' (S.insert fx seen) (len + 1) gen''
          where
            (fx, gen'') = randomR (1, n) gen'

statistical :: (Integral a, Random b, Integral b, RandomGen c, Fractional d) =>
               a -> b -> c -> (d, c)
statistical samples size gen =
    let (total, gen') = sar samples gen 0
    in ((fromIntegral total) / (fromIntegral samples), gen')
    where
      sar 0        gen' acc = (acc, gen')
      sar samples' gen' acc =
          let (len, gen'') = findRep size gen'
          in sar (samples' - 1) gen'' (acc + len)

factorial :: (Integral a) => a -> a
factorial n = foldl (*) 1 [1..n]

analytical :: (Integral a, Fractional b) => a -> b
analytical n = sum [fromIntegral num /
                    fromIntegral (factorial (n - i)) /
                    fromIntegral (n ^ i) |
                    i <- [1..n]]
    where num = factorial n

test :: (Integral a, Random b, Integral b, PrintfArg b, RandomGen c) =>
        a -> [b] -> c -> IO c
test _       []     gen = return gen
test samples (x:xs) gen = do
  let (st, gen') = statistical samples x gen
      an         = analytical x
      err        = abs (st - an) / st * 100.0
      str        = printf "%3d  %9.4f  %12.4f  (%6.2f%%)\n"
                   x (st :: Float) (an :: Float) (err :: Float)
  putStr str
  test samples xs gen'

main :: IO ()
main = do
  putStrLn " N    average    analytical    (error)"
  putStrLn "
###   =========  ============  ======
"
  let samples = 10000 :: Integer
      range   = [1..20] :: [Integer]
  _ <- test samples range $ mkStdGen 0
  return ()
```


```txt
 N    average    analytical    (error)

###   =========  ============  ======

  1     1.0000        1.0000  (  0.00%)
  2     1.4941        1.5000  (  0.39%)
  3     1.8895        1.8889  (  0.03%)
  4     2.2246        2.2188  (  0.26%)
  5     2.5158        2.5104  (  0.21%)
  6     2.7875        2.7747  (  0.46%)
  7     3.0425        3.0181  (  0.80%)
  8     3.2157        3.2450  (  0.91%)
  9     3.4534        3.4583  (  0.14%)
 10     3.6561        3.6602  (  0.11%)
 11     3.8357        3.8524  (  0.43%)
 12     4.0291        4.0361  (  0.17%)
 13     4.1819        4.2123  (  0.73%)
 14     4.3469        4.3820  (  0.81%)
 15     4.4942        4.5458  (  1.15%)
 16     4.7093        4.7043  (  0.11%)
 17     4.8288        4.8579  (  0.60%)
 18     5.0021        5.0071  (  0.10%)
 19     5.1980        5.1522  (  0.88%)
 20     5.2961        5.2936  (  0.05%)
```



## J

First, let's consider an exact, brute force approach.

Since J array indices start at 0, we'll work with 0..N-1 instead of 1..N, dealing with the difference at the boundaries.

We can implement f as {&LIST where LIST is an arbitrary list of N numbers, each picked independently from the range 0..(N-1).  We can incrementally build the described sequence using (, f@{:) - here we extend the sequence by applying f to the last element of the sequence.  Since we are only concerned with the sequence up to the point of the first repeat, we can select the unique values giving us (~.@, f@{:).  This routine stops changing when we reach the desired length, so we can repeatedly apply it forever.  For example:

```J
   (~.@, {&0 0@{:)^:_] 0
0
   (~.@, {&0 0@{:)^:_] 1
1 0
```

Once we have the sequence, we can count how many elements are in it.

```J
   0 0 ([: # (] ~.@, {:@] { [)^:_) 1
2
```

Meanwhile, we can also generate all possible values of 1..N by counting out N^N values and breaking out the result as a base N list of digits.

```J
   (#.inv i.@^~)2
0 0
0 1
1 0
1 1
```

All that's left is to count the lengths of all possible sequences for all possible distinct instances of f and average the results:

```J
   (+/ % #)@,@((#.inv i.@^~) ([: # (] ~.@, {:@] { [)^:_)"1 0/ i.)1
1
   (+/ % #)@,@((#.inv i.@^~) ([: # (] ~.@, {:@] { [)^:_)"1 0/ i.)2
1.5
   (+/ % #)@,@((#.inv i.@^~) ([: # (] ~.@, {:@] { [)^:_)"1 0/ i.)3
1.88889
   (+/ % #)@,@((#.inv i.@^~) ([: # (] ~.@, {:@] { [)^:_)"1 0/ i.)4
2.21875
   (+/ % #)@,@((#.inv i.@^~) ([: # (] ~.@, {:@] { [)^:_)"1 0/ i.)5
2.5104
   (+/ % #)@,@((#.inv i.@^~) ([: # (] ~.@, {:@] { [)^:_)"1 0/ i.)6
2.77469
```

Meanwhile the analytic solution (derived by reading the Ada implementation) looks like this:

```J
   ana=: +/@(!@[ % !@- * ^) 1+i.
   ana"0]1 2 3 4 5 6
1 1.5 1.88889 2.21875 2.5104 2.77469
```

To get our simulation, we can take the exact approach and replace the part that generates all possible values for f with a random mechanism.  Since the task does not specify how long to run the simulation, and to make this change easy, we'll use N*1e4 tests.

```J
   sim=: (+/ % #)@,@((]?@$~1e4,]) ([: # (] ~.@, {:@] { [)^:_)"1 0/ i.)
   sim"0]1 2 3 4 5 6
1 1.5034 1.8825 2.22447 2.51298 2.76898
```

The simulation approach is noticeably slower than the analytic approach, while being less accurate.

Finally, we can generate our desired results:

```J
   (;:'N average analytic error'),:,.each(;ana"0 ([;];-|@%[) sim"0)1+i.20
+--+-------+--------+-----------+
|N |average|analytic|error      |
+--+-------+--------+-----------+
| 1|      1|      1 |          0|
| 2|    1.5|1.49955 |     0.0003|
| 3|1.88889| 1.8928 | 0.00207059|
| 4|2.21875|2.23082 | 0.00544225|
| 5| 2.5104|2.52146 | 0.00440567|
| 6|2.77469|2.78147 | 0.00244182|
| 7|3.01814| 3.0101 | 0.00266346|
| 8|3.24502|3.25931 | 0.00440506|
| 9|3.45832|3.45314 | 0.00149532|
|10|3.66022| 3.6708 | 0.00289172|
|11|3.85237|3.84139 | 0.00285049|
|12|4.03607|4.03252 |0.000881304|
|13|4.21235|4.18358 | 0.00682833|
|14|4.38203|4.38791 | 0.00134132|
|15|4.54581|4.54443 |0.000302246|
|16|4.70426|4.71351 | 0.00196721|
|17|4.85787|4.85838 |0.000104089|
|18|5.00706|5.00889 |0.000365752|
|19| 5.1522|5.14785 |0.000843052|
|20|5.29358|5.28587 | 0.00145829|
+--+-------+--------+-----------+
```

Here, error is the difference between the two values divided by the analytic value.


## Java


This uses a 0-based index (0, 1, ..., n-1) as opposed to the 1-based index (1, 2, ..., n) specified in the question, because it fits better with the native structure of Java.


```java
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

public class AverageLoopLength {

    private static final int N = 100000;

    //analytical(n) = sum_(i=1)^n (n!/(n-i)!/n**i)
    private static double analytical(int n) {
        double[] factorial = new double[n + 1];
        double[] powers = new double[n + 1];
        powers[0] = 1.0;
        factorial[0] = 1.0;
        for (int i = 1; i <= n; i++) {
            factorial[i] = factorial[i - 1] * i;
            powers[i] = powers[i - 1] * n;
        }
        double sum = 0;
        //memoized factorial and powers
        for (int i = 1; i <= n; i++) {
            sum += factorial[n] / factorial[n - i] / powers[i];
        }
        return sum;
    }

    private static double average(int n) {
        Random rnd = new Random();
        double sum = 0.0;
        for (int a = 0; a < N; a++) {
            int[] random = new int[n];
            for (int i = 0; i < n; i++) {
                random[i] = rnd.nextInt(n);
            }
            Set<Integer> seen = new HashSet<>(n);
            int current = 0;
            int length = 0;
            while (seen.add(current)) {
                length++;
                current = random[current];
            }
            sum += length;
        }
        return sum / N;
    }

    public static void main(String[] args) {
        System.out.println(" N    average    analytical    (error)");
        System.out.println("
###   =========  ============  ======
");
        for (int i = 1; i <= 20; i++) {
            double avg = average(i);
            double ana = analytical(i);
            System.out.println(String.format("%3d  %9.4f  %12.4f  (%6.2f%%)", i, avg, ana, ((ana - avg) / ana * 100)));
        }
    }
}
```



## Julia

{{trans|Python}}

```julia
using Printf

analytical(n::Integer) = sum(factorial(n) / big(n) ^ i / factorial(n - i) for i = 1:n)

function test(n::Integer, times::Integer = 1000000)
    c = 0
    for i = range(0, times)
        x, bits = 1, 0
        while (bits & x) == 0
            c += 1
            bits |= x
            x = 1 << rand(0:(n - 1))
        end
    end
    return c / times
end

function main(n::Integer)
    println(" n\tavg\texp.\tdiff\n-------------------------------")
    for n in 1:n
        avg = test(n)
        theory = analytical(n)
        diff = (avg / theory - 1) * 100
        @printf(STDOUT, "%2d %8.4f %8.4f %6.3f%%\n", n, avg, theory, diff)
    end
end

main(20)

```


{{out}}

```txt

 n	avg	exp.	diff
-------------------------------
 1   1.0000   1.0000  0.000%
 2   1.4998   1.5000 -0.015%
 3   1.8895   1.8889  0.034%
 4   2.2171   2.2188 -0.075%
 5   2.5082   2.5104 -0.088%
 6   2.7729   2.7747 -0.063%
 7   3.0171   3.0181 -0.033%
 8   3.2439   3.2450 -0.034%
 9   3.4578   3.4583 -0.016%
10   3.6616   3.6602  0.038%
11   3.8525   3.8524  0.004%
12   4.0353   4.0361 -0.020%
13   4.2126   4.2123  0.006%
14   4.3835   4.3820  0.034%
15   4.5428   4.5458 -0.067%
16   4.7027   4.7043 -0.033%
17   4.8560   4.8579 -0.039%
18   5.0054   5.0071 -0.033%
19   5.1492   5.1522 -0.058%
20   5.2896   5.2936 -0.076%

```



## Kotlin

{{trans|Go}}

```scala
const val NMAX  = 20
const val TESTS = 1000000
val rand = java.util.Random()

fun avg(n: Int): Double {
    var sum = 0
    for (t in 0 until TESTS) {
        val v = BooleanArray(NMAX)
        var x = 0
        while (!v[x]) {
            v[x] = true
            sum++
            x = rand.nextInt(n)
        }
    }
    return sum.toDouble() / TESTS
}

fun ana(n: Int): Double {
    val nn = n.toDouble()
    var term = 1.0
    var sum = 1.0
    for (i in n - 1 downTo 1) {
        term *= i / nn
        sum += term
    }
    return sum
}

fun main(args: Array<String>) {
    println(" N    average    analytical    (error)")
    println("
###   =========  ============  ======
")
    for (n in 1..NMAX) {
        val a = avg(n)
        val b = ana(n)
        println(String.format("%3d   %6.4f   %10.4f      (%4.2f%%)", n, a, b, Math.abs(a - b) / b * 100.0))
    }
}
```

Sample output:
{{out}}

```txt

 N    average    analytical    (error)

###   =========  ============  ======

  1   1.0000       1.0000      (0.00%)
  2   1.5004       1.5000      (0.03%)
  3   1.8890       1.8889      (0.00%)
  4   2.2179       2.2188      (0.04%)
  5   2.5108       2.5104      (0.02%)
  6   2.7738       2.7747      (0.03%)
  7   3.0178       3.0181      (0.01%)
  8   3.2482       3.2450      (0.10%)
  9   3.4572       3.4583      (0.03%)
 10   3.6608       3.6602      (0.02%)
 11   3.8545       3.8524      (0.06%)
 12   4.0378       4.0361      (0.04%)
 13   4.2131       4.2123      (0.02%)
 14   4.3795       4.3820      (0.06%)
 15   4.5481       4.5458      (0.05%)
 16   4.7044       4.7043      (0.00%)
 17   4.8610       4.8579      (0.06%)
 18   5.0027       5.0071      (0.09%)
 19   5.1498       5.1522      (0.05%)
 20   5.2941       5.2936      (0.01%)

```



## Liberty BASIC

{{trans|BBC BASIC}}

```lb

MAXN = 20
TIMES = 10000'00

't0=time$("ms")
FOR n = 1 TO MAXN
    avg = FNtest(n, TIMES)
    theory = FNanalytical(n)
    diff = (avg / theory - 1) * 100
    PRINT n, avg, theory, using("##.####",diff); "%"
NEXT
't1=time$("ms")
'print t1-t0; " ms"
END

function FNanalytical(n)
    FOR i = 1 TO n
        s = s+ FNfactorial(n) / n^i / FNfactorial(n-i)
    NEXT
    FNanalytical = s
end function

function FNtest(n, times)
    FOR i = 1 TO times
        x = 1 : b = 0
        WHILE (b AND x) = 0
            c = c + 1
            b = b OR x
            x = 2^int(n*RND(1))
        WEND
    NEXT
    FNtest = c / times
end function

function FNfactorial(n)
    IF n=1 OR n=0 THEN FNfactorial=1 ELSE FNfactorial= n * FNfactorial(n-1)
end function

```


{{out}}

```txt

1             1             1              0.0000%
2             1.4759        1.5           -1.6067%
3             1.8868        1.88888889    -0.1106%
4             2.2139        2.21875       -0.2186%
5             2.4784        2.5104        -1.2747%
6             2.7888        2.77469136     0.5085%
7             2.9846        3.0181387     -1.1112%
8             3.2645        3.24501801     0.6004%
9             3.464         3.45831574     0.1644%
10            3.6602        3.66021568    -0.0004%
11            3.8255        3.85237205    -0.6975%
12            4.019         4.03607368    -0.4230%
13            4.2033        4.21234791    -0.2148%
14            4.3985        4.38202942     0.3759%
15            4.5868        4.54580729     0.9018%
16            4.6705        4.70425825    -0.7176%
17            4.8807        4.85787082     0.4699%
18            4.9759        5.0070631     -0.6224%
19            5.1755        5.1521962      0.4523%
20            5.2792        5.29358459    -0.2717%

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
Grid@Prepend[
  Table[{n, #[[1]], #[[2]],
      Row[{Round[10000 Abs[#[[1]] - #[[2]]]/#[[2]]]/100., "%"}]} &@
    N[{Mean[Array[
        Length@NestWhileList[#, 1, UnsameQ[##] &, All] - 1 &[# /.
            MapIndexed[#2[[1]] -> #1 &,
             RandomInteger[{1, n}, n]] &] &, 10000]],
      Sum[n! n^(n - k - 1)/(n - k)!, {k, n}]/n^(n - 1)}, 5], {n, 1,
    20}], {"N", "average", "analytical", "error"}]
```

{{Out}}

```txt
N	average	analytical	error
1	1.0000	1.0000		0.%
2	1.5017	1.5000		0.11%
3	1.8910	1.8889		0.11%
4	2.2334	2.2188		0.66%
5	2.5090	2.5104		0.06%
6	2.8092	2.7747		1.24%
7	3.0468	3.0181		0.95%
8	3.2253	3.2450		0.61%
9	3.4695	3.4583		0.32%
10	3.6661	3.6602		0.16%
11	3.8662	3.8524		0.36%
12	4.0393	4.0361		0.08%
13	4.2232	4.2123		0.26%
14	4.3496	4.3820		0.74%
15	4.5706	4.5458		0.55%
16	4.6963	4.7043		0.17%
17	4.8548	4.8579		0.06%
18	5.0671	5.0071		1.2%
19	5.1702	5.1522		0.35%
20	5.2264	5.2936		1.27%
```



## Nim

{{trans|C}}

```nim
import random, math, strfmt
randomize()

const
  maxN = 20
  times = 1_000_000

proc factorial(n: int): float =
  result = 1
  for i in 1 .. n:
    result *= i.float

proc expected(n: int): float =
  for i in 1 .. n:
    result += factorial(n) / pow(n.float, i.float) / factorial(n - i)

proc test(n, times: int): int =
  for i in 1 .. times:
    var
      x = 1
      bits = 0
    while (bits and x) == 0:
      inc result
      bits = bits or x
      x = 1 shl random(n)

echo " n\tavg\texp.\tdiff"
echo "-------------------------------"
for n in 1 .. maxN:
  let cnt = test(n, times)
  let avg = cnt.float / times
  let theory = expected(n)
  let diff = (avg / theory - 1) * 100
  printlnfmt "{:2} {:8.4f} {:8.4f} {:6.3f}%", n, avg, theory, diff
```

{{out}}

```txt
 n	avg	exp.	diff
-------------------------------
 1   1.0000   1.0000      0%
 2   1.5001   1.5000  0.008%
 3   1.8884   1.8889 -0.025%
 4   2.2187   2.2187 -0.000%
 5   2.5098   2.5104 -0.025%
 6   2.7752   2.7747  0.017%
 7   3.0175   3.0181 -0.020%
 8   3.2411   3.2450 -0.120%
 9   3.4565   3.4583 -0.054%
10   3.6599   3.6602 -0.010%
11   3.8555   3.8524  0.081%
12   4.0381   4.0361  0.051%
13   4.2124   4.2123  0.000%
14   4.3813   4.3820 -0.017%
15   4.5471   4.5458  0.027%
16   4.7009   4.7043 -0.072%
17   4.8589   4.8579  0.021%
18   5.0054   5.0071 -0.034%
19   5.1554   5.1522  0.061%
20   5.2915   5.2936 -0.040%
```


=={{header|Oberon-2}}==

```oberon2

MODULE AvgLoopLen;
(* Oxford Oberon-2 *)
IMPORT Random, Out;

PROCEDURE Fac(n: INTEGER; f: REAL): REAL;
BEGIN
	IF n = 0 THEN
		RETURN f
	ELSE
		RETURN Fac(n - 1,n*f)
	END
END Fac;

PROCEDURE Power(n,i: INTEGER): REAL;
VAR
	p: REAL;
BEGIN
	p := 1.0;
	WHILE i > 0 DO p := p * n; DEC(i) END;
	RETURN p
END Power;

PROCEDURE Abs(x: REAL): REAL;
BEGIN
	IF x < 0 THEN RETURN -x ELSE RETURN x END
END Abs;

PROCEDURE Analytical(n: INTEGER): REAL;
VAR
	i: INTEGER;
	res: REAL;
BEGIN
	res := 0.0;
	FOR i := 1 TO n DO
		res := res + (Fac(n,1.0) / Power(n,i) / Fac(n - i,1.0));
	END;
	RETURN res
END Analytical;

PROCEDURE Averages(n: INTEGER): REAL;
CONST
	times = 100000;
VAR
	rnds: SET;
	r,count,i: INTEGER;
BEGIN
	count := 0; i := 0;
	WHILE i < times DO
		rnds := {};
		LOOP
			r := Random.Roll(n);
			IF r IN rnds THEN EXIT ELSE INCL(rnds,r); INC(count) END
		END;
		INC(i)
	END;

	RETURN count / times
END Averages;

VAR
	i: INTEGER;
	av,an,df: REAL;
BEGIN
	Random.Randomize;
	Out.String("        Averages  Analytical  Diff%     ");Out.Ln;
	FOR i := 1 TO 20 DO
		Out.Int(i,3); Out.String(": ");
		av := Averages(i);an := Analytical(i);df := Abs(av - an) / an * 100.0;
		Out.Fixed(av,10,4);Out.Fixed(an,11,4);Out.Fixed(df,10,4);Out.Ln
	END
END AvgLoopLen.

```

{{Out}}

```txt

        Averages  Analytical  Diff%
  1:     1.0000     1.0000    0.0000
  2:     1.5015     1.5000    0.0993
  3:     1.8868     1.8889    0.1085
  4:     2.2187     2.2188    0.0005
  5:     2.5119     2.5104    0.0578
  6:     2.7785     2.7747    0.1366
  7:     3.0184     3.0181    0.0090
  8:     3.2435     3.2450    0.0471
  9:     3.4585     3.4583    0.0056
 10:     3.6549     3.6602    0.1463
 11:     3.8559     3.8524    0.0918
 12:     4.0452     4.0361    0.2264
 13:     4.2097     4.2123    0.0628
 14:     4.3740     4.3820    0.1830
 15:     4.5583     4.5458    0.2739
 16:     4.7001     4.7043    0.0882
 17:     4.8654     4.8579    0.1556
 18:     5.0157     5.0071    0.1731
 19:     5.1515     5.1522    0.0135
 20:     5.2930     5.2936    0.0105

```



## PARI/GP

{{trans|C}}

```parigp
expected(n)=sum(i=1,n,n!/(n-i)!/n^i,0.);
test(n, times)={
  my(ct);
  for(i=1,times,
    my(x=1,bits);
    while(!bitand(bits,x),ct++; bits=bitor(bits,x); x = 1<<random(n))
  );
  ct
};
TIMES=1000000;
{for(n=1,20,
 my(cnt=test(n, TIMES),avg=cnt/TIMES,ex=expected(n),diff=(avg/ex-1)*100.);
 print(n"\t"avg*1."\t"ex*1."\t"diff);
)}
```

{{out}}

```txt
1       1.0000  1.0000  0.E-7
2       1.4998  1.5000  -0.012933
3       1.8891  1.8889  0.013559
4       2.2198  2.2188  0.047369
5       2.5095  2.5104  -0.034616
6       2.7744  2.7747  -0.010248
7       3.0177  3.0181  -0.012945
8       3.2467  3.2450  0.050600
9       3.4611  3.4583  0.080278
10      3.6595  3.6602  -0.018651
11      3.8541  3.8524  0.044880
12      4.0428  4.0361  0.16690
13      4.2116  4.2123  -0.017921
14      4.3825  4.3820  0.011150
15      4.5467  4.5458  0.020562
16      4.7087  4.7043  0.095058
17      4.8573  4.8579  -0.011997
18      5.0080  5.0071  0.018312
19      5.1530  5.1522  0.015970
20      5.2970  5.2936  0.065143
```



## Perl


```perl
use List::Util qw(sum reduce);

sub find_loop {
    my($n) = @_;
    my($r,@seen);
    while () { $seen[$r] = $seen[($r = int(1+rand $n))] ? return sum @seen : 1 }
}

print " N    empiric      theoric      (error)\n";
print "
###   =========  ============  ======
\n";

my $MAX    = 20;
my $TRIALS = 1000;

for my $n (1 .. $MAX) {
    my $empiric = ( sum map { find_loop($n) } 1..$TRIALS ) / $TRIALS;
    my $theoric = sum map { (reduce { $a*$b } $_**2, ($n-$_+1)..$n ) / $n ** ($_+1) } 1..$n;

    printf "%3d  %9.4f  %12.4f   (%5.2f%%)\n",
            $n,  $empiric, $theoric, 100 * ($empiric - $theoric) / $theoric;
}
```

{{out}}

```txt
 N    empiric      theoric     (error)

###   =========  ============  ======

  1     1.0000        1.0000   ( 0.00%)
  2     1.4950        1.5000   (-0.33%)
  3     1.9190        1.8889   ( 1.59%)
  4     2.2400        2.2188   ( 0.96%)
  5     2.5120        2.5104   ( 0.06%)
  6     2.7500        2.7747   (-0.89%)
  7     3.0360        3.0181   ( 0.59%)
  8     3.2600        3.2450   ( 0.46%)
  9     3.4440        3.4583   (-0.41%)
 10     3.6670        3.6602   ( 0.19%)
 11     3.8340        3.8524   (-0.48%)
 12     4.0450        4.0361   ( 0.22%)
 13     4.2160        4.2123   ( 0.09%)
 14     4.4420        4.3820   ( 1.37%)
 15     4.5600        4.5458   ( 0.31%)
 16     4.7940        4.7043   ( 1.91%)
 17     4.7830        4.8579   (-1.54%)
 18     4.9140        5.0071   (-1.86%)
 19     5.2490        5.1522   ( 1.88%)
 20     5.2930        5.2936   (-0.01%)
```



## Perl 6

{{Works with|rakudo|2016.08}}

```perl6
constant MAX_N  = 20;
constant TRIALS = 100;

for 1 .. MAX_N -> $N {
    my $empiric = TRIALS R/ [+] find-loop(random-mapping($N)).elems xx TRIALS;
    my $theoric = [+]
        map -> $k { $N ** ($k + 1) R/ [*] flat $k**2, $N - $k + 1 .. $N }, 1 .. $N;

    FIRST say " N    empiric      theoric      (error)";
    FIRST say "
###   =========  ============  ======
";

    printf "%3d  %9.4f  %12.4f    (%4.2f%%)\n",
            $N,  $empiric,
                        $theoric, 100 * abs($theoric - $empiric) / $theoric;
}

sub random-mapping { hash .list Z=> .roll given ^$^size }
sub find-loop { 0, | %^mapping{*} ...^ { (%){$_}++ } }
```

{{out|Example}}

```txt
 N    empiric      theoric      (error)

###   =========  ============  ======

  1     1.0000        1.0000    (0.00%)
  2     1.5600        1.5000    (4.00%)
  3     1.7800        1.8889    (5.76%)
  4     2.1800        2.2188    (1.75%)
  5     2.6200        2.5104    (4.37%)
  6     2.8300        2.7747    (1.99%)
  7     3.1200        3.0181    (3.37%)
  8     3.1400        3.2450    (3.24%)
  9     3.4500        3.4583    (0.24%)
 10     3.6700        3.6602    (0.27%)
 11     3.8300        3.8524    (0.58%)
 12     4.3600        4.0361    (8.03%)
 13     3.9000        4.2123    (7.42%)
 14     4.4900        4.3820    (2.46%)
 15     4.9500        4.5458    (8.89%)
 16     4.9800        4.7043    (5.86%)
 17     4.9100        4.8579    (1.07%)
 18     4.9700        5.0071    (0.74%)
 19     5.1000        5.1522    (1.01%)
 20     5.2300        5.2936    (1.20%)
```



## Phix


```Phix
constant MAX = 20,
         ITER = 1000000

function expected(integer n)
atom sum = 0
    for i=1 to n do
        sum += factorial(n) / power(n,i) / factorial(n-i)
    end for
    return sum
end function

function test(integer n)
integer count = 0, x, bits
    for i=1 to ITER do
        x = 1
        bits = 0
        while not and_bits(bits,x) do
            count += 1
            bits = or_bits(bits,x)
            x = power(2,rand(n)-1)
        end while
    end for
    return count/ITER
end function

atom av, ex
    puts(1," n     avg.     exp.  (error%)\n");
    puts(1,"==
### ===   ======  =====
\n");
    for n=1 to MAX do
        av = test(n)
        ex = expected(n)
        printf(1,"%2d %8.4f %8.4f  (%5.3f%%)\n", {n,av,ex,abs(1-av/ex)*100})
    end for
```

{{out}}

```txt

 n     avg.     exp.  (error%)
==
### ===   ======  =====

 1   1.0000   1.0000  (0.000%)
 2   1.5003   1.5000  (0.018%)
 3   1.8880   1.8889  (0.046%)
 4   2.2176   2.2188  (0.052%)
 5   2.5104   2.5104  (0.001%)
 6   2.7734   2.7747  (0.046%)
 7   3.0198   3.0181  (0.055%)
 8   3.2464   3.2450  (0.042%)
 9   3.4562   3.4583  (0.062%)
10   3.6618   3.6602  (0.043%)
11   3.8511   3.8524  (0.033%)
12   4.0357   4.0361  (0.009%)
13   4.2158   4.2123  (0.083%)
14   4.3843   4.3820  (0.052%)
15   4.5410   4.5458  (0.105%)
16   4.7084   4.7043  (0.087%)
17   4.8603   4.8579  (0.049%)
18   5.0044   5.0071  (0.052%)
19   5.1516   5.1522  (0.011%)
20   5.2955   5.2936  (0.037%)

```



## PicoLisp

{{trans|Python}}

```PicoLisp
(scl 4)
(seed (in "/dev/urandom" (rd 8)))

(de fact (N)
   (if (=0 N) 1 (apply * (range 1 N))) )

(de analytical (N)
   (sum
      '((I)
         (/
            (* (fact N) 1.0)
            (** N I)
            (fact (- N I)) ) )
      (range 1 N) ) )

(de testing (N)
   (let (C 0  N (dec N)  X 0  B 0  I 1000000)
      (do I
         (zero B)
         (one X)
         (while (=0 (& B X))
            (inc 'C)
            (setq
               B (| B X)
               X (** 2 (rand 0 N)) ) ) )
      (*/ C 1.0 I) ) )

(let F (2 8 8 6)
   (tab F "N" "Avg" "Exp" "Diff")
   (for I 20
      (let (A (testing I)  B (analytical I))
         (tab F
            I
            (round A 4)
            (round B 4)
            (round
               (*
                  (abs (- (*/ A 1.0 B) 1.0))
                  100 )
               2 ) ) ) ) )

(bye)
```



## PowerShell

{{works with|PowerShell|2}}

```PowerShell

function Get-AnalyticalLoopAverage ( [int]$N )
    {
    #  Expected loop average = sum from i = 1 to N of N! / (N-i)! / N^(N-i+1)
    #  Equivalently, Expected loop average = sum from i = 1 to N of F(i)
    #    where F(N) = 1, and F(i) = F(i+1)*i/N

    $LoopAverage = $Fi = 1

    If ( $N -eq 1 ) { return $LoopAverage }

    ForEach ( $i in ($N-1)..1 )
        {
        $Fi *= $i / $N
        $LoopAverage  += $Fi
        }
    return $LoopAverage
    }

function Get-ExperimentalLoopAverage ( [int]$N, [int]$Tests = 100000 )
    {
    If ( $N -eq 1 ) { return 1 }

    #  Using 0 through N-1 instead of 1 through N for speed and simplicity
    $NMO = $N - 1

    #  Create array to hold mapping function
    $F = New-Object int[] ( $N )

    $Count = 0
    $Random = New-Object System.Random

    ForEach ( $Test in 1..$Tests )
        {
        #  Map each number to a random number
        ForEach ( $i in 0..$NMO )
            {
            $F[$i] = $Random.Next( $N )
            }

        #  For each number...
        ForEach ( $i in 0..$NMO )
            {
            #  Add the number to the list
            $List = @()
            $Count++
            $List += $X = $i

            #  If loop does not yet exist in list...
            While ( $F[$X] -notin $List )
                {
                #  Go to the next mapped number and add it to the list
                $Count++
                $List += $X = $F[$X]
                }
            }
        }
    $LoopAvereage = $Count / $N / $Tests
    return $LoopAvereage
    }

```

Note: The use of the [pscustomobject] type accelerator to simplify making the test result table look pretty requires PowerShell 3.0.

```PowerShell

#  Display results for N = 1 through 20
ForEach ( $N in 1..20 )
    {
    $AnalyticalAverage   = Get-AnalyticalLoopAverage   $N
    $ExperimentalAverage = Get-ExperimentalLoopAverage $N
    [pscustomobject] @{
        N            = $N.ToString().PadLeft( 2, ' ' )
        Analytical   = $AnalyticalAverage.ToString( '0.00000000' )
        Experimental = $ExperimentalAverage.ToString( '0.00000000' )
        'Error (%)'  = ( [math]::Abs( $AnalyticalAverage - $ExperimentalAverage ) / $AnalyticalAverage * 100 ).ToString( '0.00000000' )
        }
    }

```

{{out}}

```txt

N  Analytical Experimental Error (%)
-  ---------- ------------ ---------
 1 1.00000000 1.00000000   0.00000000
 2 1.50000000 1.49985500   0.00966667
 3 1.88888889 1.88713000   0.09311765
 4 2.21875000 2.22103500   0.10298592
 5 2.51040000 2.51069200   0.01163161
 6 2.77469136 2.77264833   0.07363070
 7 3.01813870 3.01547143   0.08837474
 8 3.24501801 3.25003875   0.15472163
 9 3.45831574 3.45067667   0.22089013
10 3.66021568 3.65659000   0.09905646
11 3.85237205 3.85669273   0.11215626
12 4.03607368 4.03813500   0.05107253
13 4.21234791 4.20946231   0.06850349
14 4.38202942 4.38458786   0.05838465
15 4.54580729 4.54466400   0.02515032
16 4.70425825 4.70146375   0.05940356
17 4.85787082 4.86807647   0.21008483
18 5.00706310 5.01939278   0.24624572
19 5.15219620 5.15179263   0.00783296
20 5.29358459 5.29214950   0.02710991

```



## Python

{{trans|C}}

```python
from __future__ import division # Only necessary for Python 2.X
from math import factorial
from random import randrange

MAX_N = 20
TIMES = 1000000

def analytical(n):
	return sum(factorial(n) / pow(n, i) / factorial(n -i) for i in range(1, n+1))

def test(n, times):
    count = 0
    for i in range(times):
        x, bits = 1, 0
        while not (bits & x):
            count += 1
            bits |= x
            x = 1 << randrange(n)
    return count / times

if __name__ == '__main__':
    print(" n\tavg\texp.\tdiff\n-------------------------------")
    for n in range(1, MAX_N+1):
        avg = test(n, TIMES)
        theory = analytical(n)
        diff = (avg / theory - 1) * 100
        print("%2d %8.4f %8.4f %6.3f%%" % (n, avg, theory, diff))
```

{{out}}

```txt
 n	avg	exp.	diff
-------------------------------
 1   1.0000   1.0000  0.000%
 2   1.5006   1.5000  0.037%
 3   1.8887   1.8889 -0.012%
 4   2.2190   2.2188  0.011%
 5   2.5101   2.5104 -0.012%
 6   2.7750   2.7747  0.012%
 7   3.0158   3.0181 -0.076%
 8   3.2447   3.2450 -0.009%
 9   3.4586   3.4583  0.009%
10   3.6598   3.6602 -0.010%
11   3.8510   3.8524 -0.036%
12   4.0368   4.0361  0.017%
13   4.2099   4.2123 -0.058%
14   4.3784   4.3820 -0.083%
15   4.5484   4.5458  0.058%
16   4.7045   4.7043  0.006%
17   4.8611   4.8579  0.067%
18   5.0074   5.0071  0.007%
19   5.1534   5.1522  0.024%
20   5.2927   5.2936 -0.017%
```


## R


```R

expected <- function(size) {
  result <- 0
  for (i in 1:size) {
    result <- result + factorial(size) / size^i / factorial(size -i)
  }
  result
}

knuth <- function(size) {
  v <- sample(1:size, size, replace = TRUE)

  visit <- vector('logical',size)
  place <- 1
  visit[[1]] <- TRUE
  steps <- 0

  repeat {
    place <- v[[place]]
    steps <- steps + 1
    if (visit[[place]]) break
    visit[[place]] <- TRUE
  }
  steps
}

cat(" N    average    analytical     (error)\n")
cat("
###   =========  ============  =======
\n")
for (num in 1:20) {
  average <- mean(replicate(1e6, knuth(num)))
  analytical <- expected(num)
  error <- abs(average/analytical-1)*100

  cat(sprintf("%3d%11.4f%14.4f  ( %4.4f%%)\n", num, round(average,4), round(analytical,4), round(error,2)))
}

```


{{out}}

```txt

 N    average    analytical     (error)

###   =========  ============  =======

  1     1.0000        1.0000  ( 0.0000%)
  2     1.5002        1.5000  ( 0.0100%)
  3     1.8892        1.8889  ( 0.0100%)
  4     2.2190        2.2188  ( 0.0100%)
  5     2.5108        2.5104  ( 0.0200%)
  6     2.7751        2.7747  ( 0.0200%)
  7     3.0177        3.0181  ( 0.0100%)
  8     3.2472        3.2450  ( 0.0700%)
  9     3.4582        3.4583  ( 0.0000%)
 10     3.6600        3.6602  ( 0.0100%)
 11     3.8530        3.8524  ( 0.0200%)
 12     4.0366        4.0361  ( 0.0100%)
 13     4.2085        4.2123  ( 0.0900%)
 14     4.3814        4.3820  ( 0.0100%)
 15     4.5446        4.5458  ( 0.0300%)
 16     4.7063        4.7043  ( 0.0400%)
 17     4.8555        4.8579  ( 0.0500%)
 18     5.0099        5.0071  ( 0.0600%)
 19     5.1567        5.1522  ( 0.0900%)
 20     5.2940        5.2936  ( 0.0100%)

```



## Racket


```racket

#lang racket
(require (only-in math factorial))

(define (analytical n)
  (for/sum ([i (in-range 1 (add1 n))])
    (/ (factorial n) (expt n i) (factorial (- n i)))))

(define (test n times)
  (define (count-times seen times)
    (define x (random n))
    (if (memq x seen) times (count-times (cons x seen) (add1 times))))
  (/ (for/fold ([count 0]) ([i times]) (count-times '() count))
     times))

(define (test-table max-n times)
  (displayln " n avg    theory error\n------------------------")
  (for ([i (in-range 1 (add1 max-n))])
    (define average    (test i times))
    (define theory     (analytical i))
    (define difference (* (abs (sub1 (/ average theory))) 100))
    (displayln (~a (~a i #:width 2 #:align 'right)
                   " " (real->decimal-string average 4)
                   " " (real->decimal-string theory 4)
                   " " (real->decimal-string difference 4)
                   "%"))))

(test-table 20 10000)

```


{{out}}

```txt

 n avg    theory error
------------------------
 1 1.0000 1.0000 0.0000%
 2 1.5082 1.5000 0.5467%
 3 1.8966 1.8889 0.4082%
 4 2.2251 2.2188 0.2862%
 5 2.5138 2.5104 0.1354%
 6 2.7582 2.7747 0.5943%
 7 3.0253 3.0181 0.2373%
 8 3.2293 3.2450 0.4844%
 9 3.4602 3.4583 0.0545%
10 3.6831 3.6602 0.6252%
11 3.8459 3.8524 0.1680%
12 4.0348 4.0361 0.0316%
13 4.1896 4.2123 0.5400%
14 4.3555 4.3820 0.6054%
15 4.5678 4.5458 0.4838%
16 4.6950 4.7043 0.1968%
17 4.8524 4.8579 0.1126%
18 5.0224 5.0071 0.3063%
19 5.1017 5.1522 0.9801%
20 5.3316 5.2936 0.7181%

```



## REXX

This REXX program automatically adjusts the precision (decimal digits) to be used based on the size of the

factorial (product) for   '''RUNS'''.

Also note that the   <big>'''!'''</big>   (factorial function)   uses memoization for optimization.

```rexx
/*REXX program computes the average loop length mapping a random field 1···N ───► 1···N */
parse arg runs tests seed .                      /*obtain optional arguments from the CL*/
if  runs =='' |  runs ==","  then runs =      40 /*Not specified?  Then use the default.*/
if tests =='' | tests ==","  then tests= 1000000 /* "      "         "   "   "     "    */
if datatype(seed, 'W')  then call random ,, seed /*Is integer?   For RAND repeatability.*/
!.=0;          !.0=1                             /*used for  factorial (!)  memoization.*/
numeric digits 100000                            /*be able to calculate 25k! if need be.*/
numeric digits max(9, length( !(runs) )   )      /*set the NUMERIC DIGITS for  !(runs). */
say right(     runs, 24)      'runs'             /*display number of runs   we're using.*/
say right(    tests, 24)      'tests'            /*   "       "    " tests    "     "   */
say right( digits(), 24)      'digits'           /*   "       "    " digits   "     "   */
say
say "        N    average     exact     % error "     /* ◄─── title, header ►────────┐  */
hdr="       ═══  ═════════  ═════════  ═════════";       pad=left('',3)  /* ◄────────┘  */
say hdr
         do #=1  for runs;   av=fmtD( exact(#) ) /*use four digits past decimal point.  */
                             xa=fmtD( exper(#) ) /* "    "    "      "     "      "     */
         say right(#,9)  pad xa pad av pad fmtD( abs(xa-av) * 100 / av)   /*show values.*/
         end   /*#*/
say hdr                                          /*display the final header (some bars).*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
!:     procedure expose !.;  parse arg z;                      if !.z\==0  then return !.z
       !=1;       do j=2  for z -1;  !=!*j;  !.j=!;  end; /*compute factorial*/   return !
/*──────────────────────────────────────────────────────────────────────────────────────*/
exact: parse arg x;  s=0;     do j=1  for x;  s=s + !(x) / !(x-j) / x**j;  end;   return s
/*──────────────────────────────────────────────────────────────────────────────────────*/
exper: parse arg n;  k=0;     do tests;   $.=0                      /*do it TESTS times.*/
                                 do n;    r=random(1, n);      if $.r  then leave
                                 $.r=1;   k=k + 1                   /*bump the counter. */
                                 end   /*n*/
                              end      /*tests*/
       return k/tests
/*──────────────────────────────────────────────────────────────────────────────────────*/
fmtD:  parse arg y,d;     d=word(d 4, 1);    y=format(y, , d);     parse var  y  w  '.'  f
       if f=0  then return  w || left('', d +1);                                  return y
```

{{out|output|text=  when using the default inputs:}}

```txt
                      40 runs
                 1000000 tests
                      48 digits

         N    average     exact     % error
        ═══  ═════════  ═════════  ═════════
         1     1          1          0
         2     1.4964     1.5000     0.2400
         3     1.8876     1.8889     0.0688
         4     2.2222     2.2188     0.1532
         5     2.5104     2.5104     0
         6     2.7758     2.7747     0.0396
         7     3.0194     3.0181     0.0431
         8     3.2608     3.2450     0.4869
         9     3.4565     3.4583     0.0520
        10     3.6583     3.6602     0.0519
        11     3.8513     3.8524     0.0286
        12     4.0401     4.0361     0.0991
        13     4.2133     4.2123     0.0237
        14     4.3835     4.3820     0.0342
        15     4.5445     4.5458     0.0286
        16     4.6672     4.7043     0.7886
        17     4.8575     4.8579     0.0082
        18     5.0105     5.0071     0.0679
        19     5.1517     5.1522     0.0097
        20     5.2903     5.2936     0.0623
        21     5.4328     5.4315     0.0239
        22     5.5674     5.5662     0.0216
        23     5.6990     5.6979     0.0193
        24     5.8353     5.8268     0.1459
        25     5.9536     5.9530     0.0101
        26     6.0801     6.0767     0.0560
        27     6.1997     6.1981     0.0258
        28     6.3197     6.3173     0.0380
        29     6.4328     6.4344     0.0249
        30     6.5485     6.5495     0.0153
        31     6.6615     6.6627     0.0180
        32     6.7102     6.7740     0.9418
        33     6.8826     6.8837     0.0160
        34     6.9878     6.9917     0.0558
        35     7.0996     7.0982     0.0197
        36     7.2054     7.2032     0.0305
        37     7.3073     7.3067     0.0082
        38     7.4089     7.4088     0.0013
        39     7.5052     7.5096     0.0586
        40     7.6151     7.6091     0.0789
        ═══  ═════════  ═════════  ═════════

```



## Ruby

Ruby does not have a factorial method, not even in it's math library.

```ruby
class Integer
  def factorial
    self == 0 ? 1 : (1..self).inject(:*)
  end
end

def rand_until_rep(n)
  rands = {}
  loop do
    r = rand(1..n)
    return rands.size if rands[r]
    rands[r] = true
  end
end

runs = 1_000_000

puts " N    average    exp.        diff   ",
     "
###   ========  ========  ========
"
(1..20).each do |n|
  sum_of_runs = runs.times.inject(0){|sum, _| sum += rand_until_rep(n)}
  avg = sum_of_runs / runs.to_f
  analytical = (1..n).inject(0){|sum, i| sum += (n.factorial / (n**i).to_f / (n-i).factorial)}
  puts "%3d  %8.4f  %8.4f  (%8.4f%%)" % [n, avg, analytical, (avg/analytical - 1)*100]
end
```

{{out}}

```txt

 N    average    exp.        diff

###   ========  ========  ========

  1    1.0000    1.0000  (  0.0000%)
  2    1.4999    1.5000  ( -0.0054%)
  3    1.8886    1.8889  ( -0.0158%)
  4    2.2181    2.2188  ( -0.0293%)
  5    2.5107    2.5104  (  0.0110%)
  6    2.7717    2.7747  ( -0.1074%)
  7    3.0167    3.0181  ( -0.0484%)
  8    3.2442    3.2450  ( -0.0257%)
  9    3.4597    3.4583  (  0.0394%)
 10    3.6572    3.6602  ( -0.0821%)
 11    3.8502    3.8524  ( -0.0562%)
 12    4.0357    4.0361  ( -0.0084%)
 13    4.2139    4.2123  (  0.0360%)
 14    4.3805    4.3820  ( -0.0360%)
 15    4.5481    4.5458  (  0.0505%)
 16    4.7030    4.7043  ( -0.0265%)
 17    4.8582    4.8579  (  0.0075%)
 18    5.0078    5.0071  (  0.0151%)
 19    5.1568    5.1522  (  0.0893%)
 20    5.2885    5.2936  ( -0.0961%)

```



## Rust

{{libheader|rand}}

```rust
extern crate rand;

use rand::{ThreadRng, thread_rng};
use rand::distributions::{IndependentSample, Range};
use std::collections::HashSet;
use std::env;
use std::process;

fn help() {
    println!("usage: average_loop_length <max_N> <trials>");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut max_n: u32 = 20;
    let mut trials: u32 = 1000;

    match args.len() {
        1 => {}
        3 => {
            max_n = args[1].parse::<u32>().unwrap();
            trials = args[2].parse::<u32>().unwrap();
        }
        _ => {
            help();
            process::exit(0);
        }
    }

    let mut rng = thread_rng();

    println!(" N    average    analytical    (error)");
    println!("
###   =========  ============  ======
");
    for n in 1..(max_n + 1) {
        let the_analytical = analytical(n);
        let the_empirical = empirical(n, trials, &mut rng);
        println!(" {:>2}     {:3.4}        {:3.4}  ( {:>+1.2}%)",
                 n,
                 the_empirical,
                 the_analytical,
                 100f64 * (the_empirical / the_analytical - 1f64));
    }
}

fn factorial(n: u32) -> f64 {
    (1..n + 1).fold(1f64, |p, n| p * n as f64)
}

fn analytical(n: u32) -> f64 {
    let sum: f64 = (1..(n + 1))
                       .map(|i| factorial(n) / (n as f64).powi(i as i32) / factorial(n - i))
                       .fold(0f64, |a, v| a + v);
    sum
}

fn empirical(n: u32, trials: u32, rng: &mut ThreadRng) -> f64 {
    let sum: f64 = (0..trials)
                       .map(|_t| {
                           let mut item = 1u32;
                           let mut seen = HashSet::new();
                           let range = Range::new(1u32, n + 1);

                           for step in 0..n {
                               if seen.contains(&item) {
                                   return step as f64;
                               }
                               seen.insert(item);
                               item = range.ind_sample(rng);
                           }
                           n as f64
                       })
                       .fold(0f64, |a, v| a + v);
    sum / trials as f64
}



```

{{out}}
Using default arguments:

```txt

N    average    analytical    (error)

###   =========  ============  ======

  1     1.0000        1.0000  ( +0.00%)
  2     1.4992        1.5000  ( -0.05%)
  3     1.8881        1.8889  ( -0.04%)
  4     2.2177        2.2188  ( -0.05%)
  5     2.5107        2.5104  ( +0.01%)
  6     2.7752        2.7747  ( +0.02%)
  7     3.0172        3.0181  ( -0.03%)
  8     3.2452        3.2450  ( +0.01%)
  9     3.4628        3.4583  ( +0.13%)
 10     3.6606        3.6602  ( +0.01%)
 11     3.8515        3.8524  ( -0.02%)
 12     4.0348        4.0361  ( -0.03%)
 13     4.2105        4.2123  ( -0.04%)
 14     4.3835        4.3820  ( +0.03%)
 15     4.5477        4.5458  ( +0.04%)
 16     4.7042        4.7043  ( -0.00%)
 17     4.8580        4.8579  ( +0.00%)
 18     5.0076        5.0071  ( +0.01%)
 19     5.1554        5.1522  ( +0.06%)
 20     5.2911        5.2936  ( -0.05%)

```



## Scala



```Scala

import scala.util.Random

object AverageLoopLength extends App {

  val factorial: Stream[Double] = 1 #:: factorial.zip(Stream.from(1)).map(n => n._2 * factorial(n._2 - 1))

  def expected(n: Int) = (for (i <- 1 to n) yield factorial(n) / Math.pow(n, i) / factorial(n - i)).sum

  def trial(n: Int):Double = {
    var count = 0
    var x = 1
    var bits = 0

    while ((bits & x) == 0) {
      count = count + 1
      bits = bits | x
      x = 1 << Random.nextInt(n)
    }
    count
  }

  def tested(n: Int, times: Int) = (for (i <- 1 to times) yield trial(n)).sum / times

  val results = for (n <- 1 to 20;
                     avg = tested(n, 1000000);
                     theory = expected(n)
  ) yield (n, avg, theory, (avg / theory - 1) * 100)


  println("n          avg         exp      diff")
  println("------------------------------------")
  results foreach { n => {
      println(f"${n._1}%2d    ${n._2}%2.6f    ${n._3}%2.6f    ${n._4}%2.3f%%")
    }
  }

}

```

{{out}}

```txt

n          avg         exp      diff
------------------------------------
 1    1.000000    1.000000    0.000%
 2    1.499894    1.500000    -0.007%
 3    1.887826    1.888889    -0.056%
 4    2.217514    2.218750    -0.056%
 5    2.510049    2.510400    -0.014%
 6    2.773658    2.774691    -0.037%
 7    3.016585    3.018139    -0.051%
 8    3.246865    3.245018    0.057%
 9    3.458683    3.458316    0.011%
10    3.660361    3.660216    0.004%
11    3.852663    3.852372    0.008%
12    4.036970    4.036074    0.022%
13    4.213653    4.212348    0.031%
14    4.385226    4.382029    0.073%
15    4.545667    4.545807    -0.003%
16    4.705559    4.704258    0.028%
17    4.854056    4.857871    -0.079%
18    5.007146    5.007063    0.002%
19    5.148767    5.152196    -0.067%
20    5.292875    5.293585    -0.013%

```



## Scheme



```scheme

(import (scheme base)
        (scheme write)
        (srfi 1 lists)
        (only (srfi 13 strings) string-pad-right)
        (srfi 27 random-bits))

(define (analytical-function n)
  (define (factorial n)
    (fold * 1 (iota n 1)))
  ;
  (fold (lambda (i sum)
          (+ sum
             (/ (factorial n) (expt n i) (factorial (- n i)))))
        0
        (iota n 1)))

(define (simulation n runs)
  (define (single-simulation)
    (random-source-randomize! default-random-source)
    (let ((vec (make-vector n #f)))
      (let loop ((count 0)
                 (num (random-integer n)))
        (if (vector-ref vec num)
          count
          (begin (vector-set! vec num #t)
                 (loop (+ 1 count)
                       (random-integer n)))))))
  ;;
  (let loop ((total 0)
             (run runs))
    (if (zero? run)
      (/ total runs)
      (loop (+ total (single-simulation))
            (- run 1)))))

(display " N   average   formula   (error) \n")
(display "
###  ========= ========= ======
\n")
(for-each
  (lambda (n)
    (let ((simulation (inexact (simulation n 10000)))
          (formula (inexact (analytical-function n))))
      (display
        (string-append
          " "
          (string-pad-right (number->string n) 3)
          "   "
          (string-pad-right (number->string simulation) 6)
          "   "
          (string-pad-right (number->string formula) 6)
          "   ("
          (string-pad-right
            (number->string (* 100 (/ (- simulation formula) formula)))
            5)
          "%)"))
      (newline)))
  (iota 20 1))

```


{{out}}

```txt

 N   average   formula   (error)

###  ========= ========= ======

 1     1.0      1.0      (0.0  %)
 2     1.5018   1.5      (0.120%)
 3     1.8863   1.8888   (-0.13%)
 4     2.2154   2.2187   (-0.15%)
 5     2.5082   2.5104   (-0.08%)
 6     2.7613   2.7746   (-0.48%)
 7     3.036    3.0181   (0.591%)
 8     3.2656   3.2450   (0.634%)
 9     3.455    3.4583   (-0.09%)
 10    3.682    3.6602   (0.595%)
 11    3.8233   3.8523   (-0.75%)
 12    4.0409   4.0360   (0.119%)
 13    4.2471   4.2123   (0.825%)
 14    4.3577   4.3820   (-0.55%)
 15    4.5351   4.5458   (-0.23%)
 16    4.7181   4.7042   (0.294%)
 17    4.8877   4.8578   (0.614%)
 18    5.0239   5.0070   (0.336%)
 19    5.1216   5.1521   (-0.59%)
 20    5.2717   5.2935   (-0.41%)

```


## Simula



```simula
BEGIN

   REAL PROCEDURE FACTORIAL(N); INTEGER N;
   BEGIN
      REAL RESULT;
      INTEGER I;
      RESULT := 1.0;
      FOR I := 2 STEP 1 UNTIL N DO
         RESULT := RESULT * I;
      FACTORIAL := RESULT;
   END FACTORIAL;

   REAL PROCEDURE ANALYTICAL (N); INTEGER N;
   BEGIN
      REAL SUM, RN;
      INTEGER I;
      RN := N;
      FOR I := 1 STEP 1 UNTIL N DO
      BEGIN
         SUM := SUM + FACTORIAL(N) / FACTORIAL(N - I) / RN ** I;
      END;
      ANALYTICAL := SUM;
   END ANALYTICAL;

   REAL PROCEDURE EXPERIMENTAL(N); INTEGER N;
   BEGIN
      INTEGER NUM;
      INTEGER COUNT;
      INTEGER RUN;
      FOR RUN := 1 STEP 1 UNTIL TESTS DO
      BEGIN
         BOOLEAN ARRAY BITS(1:N);
         INTEGER I;
         FOR I := 1 STEP 1 UNTIL N DO
         BEGIN
            NUM := RANDINT(1,N,SEED);
            IF BITS(NUM) THEN GOTO L;
            BITS(NUM) := TRUE;
            COUNT := COUNT + 1;
         END FOR I;
      L:
      END FOR RUN;
      EXPERIMENTAL := COUNT / TESTS;
   END EXPERIMENTAL;

   INTEGER SEED, TESTS;
   SEED := ININT;
   TESTS := 1000000;
   BEGIN
      REAL A, E, ERR;
      INTEGER I;
      OUTTEXT(" N  AVG    CALC   %DIFF"); OUTIMAGE;
      FOR I := 1 STEP 1 UNTIL 20 DO
      BEGIN
         A := ANALYTICAL(I);
         E := EXPERIMENTAL(I);
         ERR := (ABS(E-A)/A)*100.0;
         OUTINT(I, 2);
         OUTFIX(E, 4, 7);
         OUTFIX(A, 4, 10);
         OUTFIX(ERR, 4, 10);
         OUTIMAGE;
      END FOR I;
   END;
END
```

{{in}}

```txt
678
```

{{out}}

```txt
 N  AVG    CALC   %DIFF
 1 1.0000    1.0000    0.0000
 2 1.4999    1.5000    0.0075
 3 1.8890    1.8889    0.0072
 4 2.2182    2.2188    0.0243
 5 2.5105    2.5104    0.0027
 6 2.7746    2.7747    0.0025
 7 3.0164    3.0181    0.0590
 8 3.2447    3.2450    0.0110
 9 3.4567    3.4583    0.0453
10 3.6622    3.6602    0.0539
11 3.8503    3.8524    0.0546
12 4.0373    4.0361    0.0300
13 4.2105    4.2123    0.0445
14 4.3819    4.3820    0.0027
15 4.5475    4.5458    0.0376
16 4.7056    4.7043    0.0295
17 4.8559    4.8579    0.0396
18 5.0105    5.0071    0.0694
19 5.1541    5.1522    0.0376
20 5.2961    5.2936    0.0467
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const integer: TESTS is 1000000;

const func float: factorial (in integer: number) is func
  result
    var float: factorial is 1.0;
  local
    var integer: i is 0;
  begin
    for i range 2 to number do
      factorial *:= flt(i);
    end for;
  end func;

const func float: analytical (in integer: number) is func
  result
    var float: sum is 0.0;
  local
    var integer: i is 0;
  begin
    for i range 1 to number do
      sum +:= factorial(number) / factorial(number - i) / flt(number)**i;
    end for;
  end func;

const func float: experimental (in integer: number) is func
  result
    var float: experimental is 0.0;
  local
    var integer: run is 0;
    var set of integer: seen is EMPTY_SET;
    var integer: current is 1;
    var integer: count is 0;
  begin
    for run range 1 to TESTS do
      current := 1;
      seen := EMPTY_SET;
      while current not in seen do
        incr(count);
        incl(seen, current);
        current := rand(1, number);
      end while;
    end for;
    experimental := flt(count) / flt(TESTS);
  end func;

const proc: main is func
  local
    var integer: number is 0;
    var float: analytical is 0.0;
    var float: experimental is 0.0;
    var float: err is 0.0;
  begin
    writeln(" N  avg    calc   %diff");
    for number range 1 to 20 do
      analytical := analytical(number);
      experimental := experimental(number);
      err := abs(experimental - analytical) / analytical * 100.0;
      writeln(number lpad 2 <& experimental digits 4 lpad 7 <&
              analytical digits 4 lpad 7 <& err digits 3 lpad 7);
    end for;
  end func;
```


{{out}}

```txt

 N  avg    calc   %diff
 1 1.0000 1.0000  0.000
 2 1.4999 1.5000  0.005
 3 1.8891 1.8889  0.010
 4 2.2196 2.2188  0.040
 5 2.5073 2.5104  0.122
 6 2.7744 2.7747  0.010
 7 3.0186 3.0181  0.015
 8 3.2463 3.2450  0.040
 9 3.4592 3.4583  0.027
10 3.6597 3.6602  0.013
11 3.8549 3.8524  0.066
12 4.0374 4.0361  0.033
13 4.2115 4.2123  0.019
14 4.3835 4.3820  0.033
15 4.5474 4.5458  0.035
16 4.7017 4.7043  0.055
17 4.8558 4.8579  0.043
18 5.0096 5.0071  0.051
19 5.1522 5.1522  0.000
20 5.2907 5.2936  0.054

```



## Tcl


```tcl
# Generate a list of the numbers increasing from $a to $b
proc range {a b} {
    for {set result {}} {$a <= $b} {incr a} {lappend result $a}
    return $result
}

# Computing the expected value analytically
proc tcl::mathfunc::factorial n {
    ::tcl::mathop::* {*}[range 2 $n]
}
proc Analytical {n} {
    set sum 0.0
    foreach x [range 1 $n] {
	set sum [expr {$sum + factorial($n) / factorial($n-$x) / double($n)**$x}]
    }
    return $sum
}

# Determining an approximation to the value experimentally
proc Experimental {n numTests} {
    set count 0
    set u0 [lrepeat $n 1]
    foreach run [range 1 $numTests] {
	set unseen $u0
	for {set i 0} {[lindex $unseen $i]} {incr count} {
	    lset unseen $i 0
	    set i [expr {int(rand()*$n)}]
	}
    }
    return [expr {$count / double($numTests)}]
}

# Tabulate the results in exactly the original format
puts " N    average    analytical    (error)"
puts "
###   =========  ============  ======
"
foreach n [range 1 20] {
    set a [Analytical $n]
    set e [Experimental $n 100000]
    puts [format "%3d  %9.4f  %12.4f  (%6.2f%%)" $n $e $a [expr {abs($e-$a)/$a*100.0}]]
}
```

{{out}}

```txt

 N    average    analytical    (error)

###   =========  ============  ======

  1     1.0000        1.0000  (  0.00%)
  2     1.5003        1.5000  (  0.02%)
  3     1.8881        1.8889  (  0.04%)
  4     2.2228        2.2188  (  0.18%)
  5     2.5109        2.5104  (  0.02%)
  6     2.7804        2.7747  (  0.20%)
  7     3.0223        3.0181  (  0.14%)
  8     3.2456        3.2450  (  0.02%)
  9     3.4598        3.4583  (  0.04%)
 10     3.6590        3.6602  (  0.03%)
 11     3.8527        3.8524  (  0.01%)
 12     4.0390        4.0361  (  0.07%)
 13     4.2156        4.2123  (  0.08%)
 14     4.3821        4.3820  (  0.00%)
 15     4.5527        4.5458  (  0.15%)
 16     4.6952        4.7043  (  0.19%)
 17     4.8530        4.8579  (  0.10%)
 18     4.9912        5.0071  (  0.32%)
 19     5.1578        5.1522  (  0.11%)
 20     5.2992        5.2936  (  0.11%)

```



## Unicon

{{trans|C}}

```unicon
link printf, factors

$define MAX_N 20
$define TIMES 1000000
$define RAND_MAX 2147483647

procedure expected(n)
    local sum := 0
    every i := 1 to n do
        sum +:= factorial(n) / (n ^ i) / factorial(n - i)
    return sum
end

procedure test(n, times)
    local i, count := 0, x, bits
    every i := 0 to times-1 do {
	x := 1
	bits := 0
	while iand(bits, x)=0 do {
            count +:= 1
            bits := ior(bits, x)
            x := ishift(1 , ?n-1)
        }
    }
    return count
end

procedure main(void)
    local n, cnt, avg, theory, diff
    write(" n\tavg\texp.\tdiff\n", repl("-",29))
    every n := 1 to MAX_N do {
        cnt := test(n, TIMES)
        avg := real(cnt) / TIMES
        theory := expected(n)
        diff := (avg / theory - 1) * 100
        printf("%2d %8.4r %8.4r %6.3r%%\n", n, avg, theory, diff)
    }
    return 0
end
```

{{out}}

```txt
 n      avg     exp.    diff
-----------------------------
 1   1.0000   1.0000  0.000%
 2   1.5008   1.5000  0.056%
 3   1.8879   1.8889 -0.051%
 4   2.2208   2.2188  0.091%
 5   2.5127   2.5104  0.093%
 6   2.7759   2.7747  0.044%
 7   3.0175   3.0181 -0.023%
 8   3.2425   3.2450 -0.079%
 9   3.4571   3.4583 -0.034%
10   3.6613   3.6602  0.029%
11   3.8493   3.8524 -0.081%
12   4.0384   4.0361  0.058%
13   4.2133   4.2123  0.023%
14   4.3804   4.3820 -0.037%
15   4.5475   4.5458  0.038%
16   4.7049   4.7043  0.014%
17   4.8575   4.8579 -0.008%
18   5.0088   5.0071  0.035%
19   5.1533   5.1522  0.021%
20   5.2893   5.2936 -0.081%
```



## VBA

{{trans|Phix}}

```vb
Const MAX = 20
Const ITER = 1000000

Function expected(n As Long) As Double
    Dim sum As Double
    For i = 1 To n
        sum = sum + WorksheetFunction.Fact(n) / n ^ i / WorksheetFunction.Fact(n - i)
    Next i
    expected = sum
End Function

Function test(n As Long) As Double
    Dim count As Long
    Dim x As Long, bits As Long
    For i = 1 To ITER
        x = 1
        bits = 0
        Do While Not bits And x
            count = count + 1
            bits = bits Or x
            x = 2 ^ (Int(n * Rnd()))
        Loop
    Next i
    test = count / ITER
End Function

Public Sub main()
    Dim n As Long
    Debug.Print " n     avg.     exp.  (error%)"
    Debug.Print "==
### ===   ======  =====
"
    For n = 1 To MAX
        av = test(n)
        ex = expected(n)
        Debug.Print Format(n, "@@"); "  "; Format(av, "0.0000"); "   ";
        Debug.Print Format(ex, "0.0000"); "  ("; Format(Abs(1 - av / ex), "0.000%"); ")"
    Next n
End Sub
```
{{out}}

```txt
 n     avg.     exp.  (error%)
==
### ===   ======  =====

 1  1,0000   1,0000  (0,000%)
 2  1,4994   1,5000  (0,041%)
 3  1,8893   1,8889  (0,023%)
 4  2,2187   2,2188  (0,001%)
 5  2,5107   2,5104  (0,010%)
 6  2,7769   2,7747  (0,080%)
 7  3,0162   3,0181  (0,064%)
 8  3,2472   3,2450  (0,066%)
 9  3,4603   3,4583  (0,056%)
10  3,6577   3,6602  (0,070%)
11  3,8527   3,8524  (0,010%)
12  4,0361   4,0361  (0,001%)
13  4,2121   4,2123  (0,005%)
14  4,3825   4,3820  (0,010%)
15  4,5466   4,5458  (0,016%)
16  4,7023   4,7043  (0,041%)
17  4,8567   4,8579  (0,025%)
18  5,0031   5,0071  (0,079%)
19  5,1530   5,1522  (0,016%)
20  5,2958   5,2936  (0,041%)
```


## zkl


```zkl
const N=20;

(" N    average    analytical    (error)").println();
("
###   =========  ============  ======
").println();
foreach n in ([1..N]){
   a := avg(n);
   b := ana(n);
   "%3d  %9.4f  %12.4f  (%6.2f%%)".fmt(
            n, a, b, ((a-b)/b*100)).println();
}

fcn f(n){ (0).random(n) }

fcn avg(n){
   tests := 0d10_000;
   sum := 0;
   do(tests){
      v:=(0).pump(n,List,T(Void,False)).copy();
      while(1){
         z := f(n);
         if(v[z]) break;
	 v[z] = True;
	 sum += 1;
      }
   }
   return(sum.toFloat() / tests);
}

fcn fact(n) { (1).reduce(n,fcn(N,n){N*n},1.0) } //-->Float
fcn ana(n){
   n=n.toFloat();
   (1).reduce(n,'wrap(sum,i){ sum+fact(n)/n.pow(i)/fact(n-i) },0.0);
}
```

{{out}}

```txt

 N    average    analytical    (error)

###   =========  ============  ======

  1     1.0000        1.0000  (  0.00%)
  2     1.5053        1.5000  (  0.35%)
  3     1.8899        1.8889  (  0.05%)
  4     2.2384        2.2188  (  0.89%)
  5     2.5090        2.5104  ( -0.06%)
  6     2.7824        2.7747  (  0.28%)
  7     3.0449        3.0181  (  0.89%)
  8     3.2430        3.2450  ( -0.06%)
  9     3.4744        3.4583  (  0.47%)
 10     3.6693        3.6602  (  0.25%)
 11     3.8833        3.8524  (  0.80%)
 12     4.0225        4.0361  ( -0.34%)
 13     4.1899        4.2123  ( -0.53%)
 14     4.4135        4.3820  (  0.72%)
 15     4.5807        4.5458  (  0.77%)
 16     4.7304        4.7043  (  0.56%)
 17     4.8437        4.8579  ( -0.29%)
 18     4.9838        5.0071  ( -0.46%)
 19     5.1767        5.1522  (  0.48%)
 20     5.2723        5.2936  ( -0.40%)

```

