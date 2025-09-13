+++
title = "Parallel calculations"
description = ""
date = 2019-07-22T22:45:39Z
aliases = []
[extra]
id = 8986
[taxonomies]
categories = ["task", "Control Structures"]
tags = []
+++

## Task

Many programming languages allow you to specify computations to be run in parallel.
While [[Concurrent computing]] is focused on concurrency,
the purpose of this task is to distribute time-consuming calculations
on as many CPUs as possible.

Assume we have a collection of numbers, and want to find the one
with the largest minimal prime factor
(that is, the one that contains relatively large factors).
To speed up the search, the factorization should be done
in parallel using separate threads or processes,
to take advantage of multi-core CPUs.

Show how this can be formulated in your language.
Parallelize the factorization of those numbers,
then search the returned list of numbers and factors
for the largest minimal factor,
and return that number and its prime factors.

For the prime number decomposition
you may use the solution of the [[Prime decomposition]] task.

## Ada


I took the version from [[Prime decomposition]] and adjusted it to use tasks.

prime_numbers.ads:

```Ada
generic
   type Number is private;
   Zero : Number;
   One  : Number;
   Two  : Number;
   with function Image (X : Number) return String is <>;
   with function "+"   (X, Y : Number) return Number is <>;
   with function "/"   (X, Y : Number) return Number is <>;
   with function "mod" (X, Y : Number) return Number is <>;
   with function ">="  (X, Y : Number) return Boolean is <>;
package Prime_Numbers is
   type Number_List is array (Positive range <>) of Number;

   procedure Put (List : Number_List);

   task type Calculate_Factors is
      entry Start (The_Number : in Number);
      entry Get_Size (Size : out Natural);
      entry Get_Result (List : out Number_List);
   end Calculate_Factors;

end Prime_Numbers;
```


prime_numbers.adb:

```Ada
with Ada.Text_IO;
package body Prime_Numbers is

   procedure Put (List : Number_List) is
   begin
      for Index in List'Range loop
         Ada.Text_IO.Put (Image (List (Index)));
      end loop;
   end Put;

   task body Calculate_Factors is
      Size : Natural := 0;
      N    : Number;
      M    : Number;
      K    : Number  := Two;
   begin
      accept Start (The_Number : in Number) do
         N := The_Number;
         M := N;
      end Start;
      -- Estimation of the result length from above
      while M >= Two loop
         M    := (M + One) / Two;
         Size := Size + 1;
      end loop;
      M := N;
      -- Filling the result with prime numbers
      declare
         Result : Number_List (1 .. Size);
         Index  : Positive := 1;
      begin
         while N >= K loop -- Divisors loop
            while Zero = (M mod K) loop -- While divides
               Result (Index) := K;
               Index          := Index + 1;
               M              := M / K;
            end loop;
            K := K + One;
         end loop;
         Index := Index - 1;
         accept Get_Size (Size : out Natural) do
            Size := Index;
         end Get_Size;
         accept Get_Result (List : out Number_List) do
            List (1 .. Index) := Result (1 .. Index);
         end Get_Result;
      end;
   end Calculate_Factors;

end Prime_Numbers;
```


Example usage:

parallel.adb:

```Ada
with Ada.Text_IO;
with Prime_Numbers;
procedure Parallel is
   package Integer_Primes is new Prime_Numbers (
      Number => Integer, -- use Large_Integer for longer numbers
      Zero   => 0,
      One    => 1,
      Two    => 2,
      Image  => Integer'Image);

   My_List : Integer_Primes.Number_List :=
     ( 12757923,
       12878611,
       12757923,
       15808973,
       15780709,
      197622519);

   Decomposers : array (My_List'Range) of Integer_Primes.Calculate_Factors;
   Lengths     : array (My_List'Range) of Natural;
   Max_Length  : Natural := 0;
begin
   for I in My_List'Range loop
      -- starts the tasks
      Decomposers (I).Start (My_List (I));
   end loop;
   for I in My_List'Range loop
      -- wait until task has reached Get_Size entry
      Decomposers (I).Get_Size (Lengths (I));
      if Lengths (I) > Max_Length then
         Max_Length := Lengths (I);
      end if;
   end loop;
   declare
      Results                :
        array (My_List'Range) of Integer_Primes.Number_List (1 .. Max_Length);
      Largest_Minimal_Factor : Integer := 0;
      Winning_Index          : Positive;
   begin
      for I in My_List'Range loop
         -- after Get_Result, the tasks terminate
         Decomposers (I).Get_Result (Results (I));
         if Results (I) (1) > Largest_Minimal_Factor then
            Largest_Minimal_Factor := Results (I) (1);
            Winning_Index          := I;
         end if;
      end loop;
      Ada.Text_IO.Put_Line
        ("Number" & Integer'Image (My_List (Winning_Index)) &
         " has largest minimal factor:");
      Integer_Primes.Put (Results (Winning_Index) (1 .. Lengths (Winning_Index)));
      Ada.Text_IO.New_Line;
   end;
end Parallel;
```


```txt
Number 12878611 has largest minimal factor:
 47 101 2713
```



## C

C code using OpenMP.  Compiled with <code>gcc -Wall -std=c99 -fopenmp</code>,
where GCC 4.2 or later is required.
Note that the code finds the largest first prime factor,
but does not return the factor list:
it's just a matter of repeating the prime factor test,
which adds clutter but does not make the code any more interesting.
For that matter, the code uses the dumbest prime factoring method,
and doesn't even test if the numbers can be divided by 2.

```c
#include <stdio.h>
#include <omp.h>

int main()
{
        int data[] = {12757923, 12878611, 12878893, 12757923, 15808973, 15780709, 197622519};
        int largest, largest_factor = 0;

        omp_set_num_threads(4);
        /* "omp parallel for" turns the for loop multithreaded by making each thread
         * iterating only a part of the loop variable, in this case i; variables declared
         * as "shared" will be implicitly locked on access
         */
        #pragma omp parallel for shared(largest_factor, largest)
        for (int i = 0; i < 7; i++) {
                int p, n = data[i];

                for (p = 3; p * p <= n && n % p; p += 2);
                if (p * p > n) p = n;
                if (p > largest_factor) {
                        largest_factor = p;
                        largest = n;
                        printf("thread %d: found larger: %d of %d\n",
                                omp_get_thread_num(), p, n);
                } else {
                        printf("thread %d: not larger:   %d of %d\n",
                                omp_get_thread_num(), p, n);
                }
        }

        printf("Largest factor: %d of %d\n", largest_factor, largest);
        return 0;
}
```

{{out}} (YMMV regarding the order of output):

```txt
thread 1: found larger: 47 of 12878893
thread 0: not larger:   3 of 12757923
thread 0: not larger:   47 of 12878611
thread 3: not larger:   3 of 197622519
thread 2: not larger:   29 of 15808973
thread 2: not larger:   7 of 15780709
thread 1: not larger:   3 of 12757923
Largest factor: 47 of 12878893
```



## C++

This uses C++11 features including lambda functions.


```cpp
#include <iostream>
#include <iterator>
#include <vector>
#include <ppl.h> // MSVC++
#include <concurrent_vector.h> // MSVC++

struct Factors
{
    int number;
    std::vector<int> primes;
};

const int data[] =
{
    12757923, 12878611, 12878893, 12757923, 15808973, 15780709, 197622519
};

int main()
{
    // concurrency-safe container replaces std::vector<>
    Concurrency::concurrent_vector<Factors> results;

    // parallel algorithm replaces std::for_each()
    Concurrency::parallel_for_each(std::begin(data), std::end(data), [&](int n)
    {
        Factors factors;
        factors.number = n;
        for (int f = 2; n > 1; ++f)
        {
            while (n % f == 0)
            {
                factors.primes.push_back(f);
                n /= f;
            }
        }
        results.push_back(factors); // add factorization to results
    });
    // end of parallel calculations

    // find largest minimal prime factor in results
    auto max = std::max_element(results.begin(), results.end(), [](const Factors &a, const Factors &b)
    {
        return a.primes.front() < b.primes.front();
    });

    // print number(s) and factorization
    std::for_each(results.begin(), results.end(), [&](const Factors &f)
    {
        if (f.primes.front() == max->primes.front())
        {
            std::cout << f.number << " = [ ";
            std::copy(f.primes.begin(), f.primes.end(), std::ostream_iterator<int>(std::cout, " "));
            std::cout << "]\n";
        }
    });
    return 0;
}
```

```txt

12878611 = [ 47 101 2713 ]
12878893 = [ 47 274019 ]

```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    public static List<int> PrimeFactors(int number)
    {
        var primes = new List<int>();
        for (int div = 2; div <= number; div++)
        {
            while (number % div == 0)
            {
                primes.Add(div);
                number = number / div;
            }
        }
        return primes;
    }

    static void Main(string[] args)
    {
        int[] n = { 12757923, 12878611, 12757923, 15808973, 15780709, 197622519 };
        // Calculate each of those numbers' prime factors, in parallel
        var factors = n.AsParallel().Select(PrimeFactors).ToList();
        // Make a new list showing the smallest factor for each
        var smallestFactors = factors.Select(thisNumbersFactors => thisNumbersFactors.Min()).ToList();
        // Find the index that corresponds with the largest of those factors
        int biggestFactor = smallestFactors.Max();
        int whatIndexIsThat = smallestFactors.IndexOf(biggestFactor);
        Console.WriteLine("{0} has the largest minimum prime factor: {1}", n[whatIndexIsThat], biggestFactor);
        Console.WriteLine(string.Join(" ", factors[whatIndexIsThat]));
    }
}
```

```txt
12878611 has the largest minimum prime factor: 47
47 101 2713
```


Another version, using Parallel.For:


```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

private static void Main(string[] args)
{
  int j = 0, m = 0;
  decimal[] n = {12757923, 12878611, 12757923, 15808973, 15780709, 197622519};
  var l = new List<int>[n.Length];

  Parallel.For(0, n.Length, i => { l[i] = getPrimes(n[i]); });

  for (int i = 0; i<n.Length; i++)
    if (l[i].Min()>m)
    {
      m = l[i].Min();
      j = i;
    }

  Console.WriteLine("Number {0} has largest minimal factor:", n[j]);
  foreach (int list in l[j])
    Console.Write(" "+list);
}
```

```txt
Number 12878611 has largest minimal factor:
 47 101 2713
```



## Clojure


```Clojure
(use '[clojure.contrib.lazy-seqs :only [primes]])

(defn lpf [n]
  [n (or (last
          (for [p (take-while #(<= (* % %) n) primes)
                :when (zero? (rem n p))]
            p))
         1)])

(->> (range 2 100000)
     (pmap lpf)
     (apply max-key second)
     println
     time)
```

```txt
[99847 313]
"Elapsed time: 2547.53566 msecs"
```



## Common Lisp

Depends on quicklisp.

```lisp
(ql:quickload '(lparallel))

(setf lparallel:*kernel* (lparallel:make-kernel 4)) ;; Configure for your system.

(defun factor (n &optional (acc '()))
  (when (> n 1)
    (loop with max-d = (isqrt n)
       for d = 2 then (if (evenp d) (1+ d) (+ d 2)) do
         (cond ((> d max-d) (return (cons (list n 1) acc)))
               ((zerop (rem n d))
                (return (factor (truncate n d)
                                (if (eq d (caar acc))
                                    (cons
                                     (list (caar acc) (1+ (cadar acc)))
                                     (cdr acc))
                                    (cons (list d 1) acc)))))))))

(defun max-minimum-factor (numbers)
  (lparallel:pmap-reduce
   (lambda (n) (cons n (apply #'min (mapcar #'car (factor n)))))
   (lambda (a b) (if (> (cdr a) (cdr b)) a b))
   numbers))

(defun print-max-factor (pair)
  (format t "~a has the largest minimum factor ~a~%" (car pair) (cdr pair)))
```


```lisp
CL-USER> (print-max-factor (max-minimum-factor '(12757923 12878611 12878893 12757923 15808973 15780709 197622519)))
12878893 has the largest minimum factor 47
```



## D


### Using Eager Parallel Map


```d
ulong[] decompose(ulong n) pure nothrow {
    typeof(return) result;
    for (ulong i = 2; n >= i * i; i++)
        for (; n % i == 0; n /= i)
            result ~= i;
    if (n != 1)
        result ~= n;
    return result;
}

void main() {
    import std.stdio, std.algorithm, std.parallelism, std.typecons;

    immutable ulong[] data = [
        2UL^^59-1, 2UL^^59-1, 2UL^^59-1, 112_272_537_195_293UL,
        115_284_584_522_153, 115_280_098_190_773,
        115_797_840_077_099, 112_582_718_962_171,
        112_272_537_095_293, 1_099_726_829_285_419];

    //auto factors = taskPool.amap!(n => tuple(decompose(n), n))(data);
    //static enum genPair = (ulong n) pure => tuple(decompose(n), n);
    static genPair(ulong n) pure { return tuple(decompose(n), n); }
    auto factors = taskPool.amap!genPair(data);

    auto pairs = factors.map!(p => tuple(p[0].reduce!min, p[1]));
    writeln("N. with largest min factor: ", pairs.reduce!max[1]);
}
```

```txt
N. with largest min factor: 7310027617718202995
```



### Using Threads


```d
import std.stdio, std.math, std.algorithm, std.typecons,
       core.thread, core.stdc.time;

final class MinFactor: Thread {
    private immutable ulong num;
    private ulong[] fac;
    private ulong minFac;

    this(in ulong n) /*pure nothrow*/ {
        super(&run);
        num = n;
        fac = new ulong[0];
    }

    @property ulong number() const pure nothrow {
        return num;
    }

    @property const(ulong[]) factors() const pure nothrow {
        return fac;
    }

    @property ulong minFactor() const pure nothrow {
        return minFac;
    }

    private void run() {
        immutable clock_t begin = clock;
        switch (num) {
            case 0: fac = [];
                    break;

            case 1: fac = [1];
                    break;

            default:
                uint limit = cast(uint)(1 + double(num).sqrt);
                ulong n = num;
                for (ulong divi = 3; divi < limit; divi += 2) {
                    if (n == 1)
                        break;
                    if ((n % divi) == 0) {
                        while ((n > 1) && ((n % divi) == 0)) {
                            fac ~= divi;
                            n /= divi;
                        }
                        limit = cast(uint)(1 + double(n).sqrt);
                    }
                }
                if (n > 1)
                    fac ~= n;
        }
        minFac = fac.reduce!min;
        immutable clock_t end = clock;
        writefln("num: %20d --> min. factor: %20d  ticks(%7d -> %7d)",
                 num, minFac, begin, end);
    }
}

void main() {
    immutable ulong[] numbers = [
        2UL^^59-1, 2UL^^59-1, 2UL^^59-1, 112_272_537_195_293UL,
        115_284_584_522_153, 115_280_098_190_773,
        115_797_840_077_099, 112_582_718_962_171,
        112_272_537_095_293, 1_099_726_829_285_419];

    auto tGroup = new ThreadGroup;
    foreach (const n; numbers)
        tGroup.add(new MinFactor(n));

    writeln("Minimum factors for respective numbers are:");
    foreach (t; tGroup)
        t.start;
    tGroup.joinAll;

    auto maxMin = tuple(0UL, [0UL], 0UL);
    foreach (thread; tGroup) {
        auto s = cast(MinFactor)thread;
        if (s !is null && maxMin[2] < s.minFactor)
            maxMin = tuple(s.number, s.factors.dup, s.minFactor);
    }

    writefln("Number with largest min. factor is %16d," ~
             " with factors:\n\t%s", maxMin.tupleof);
}
```

{{out}} (1 core CPU, edited to fit page width):

```txt
Minimum factors for respective numbers are:
num:   576460752303423487 --> min. factor: 179951  ticks(  16 ->  78)
num:   576460752303423487 --> min. factor: 179951  ticks(  78 -> 125)
num:   576460752303423487 --> min. factor: 179951  ticks( 141 -> 203)
num:      112272537195293 --> min. factor:    173  ticks( 203 -> 203)
num:      115284584522153 --> min. factor: 513937  ticks( 203 -> 219)
num:      115280098190773 --> min. factor: 513917  ticks( 219 -> 250)
num:      115797840077099 --> min. factor: 544651  ticks( 250 -> 266)
num:      112582718962171 --> min. factor:   3121  ticks( 266 -> 266)
num:      112272537095293 --> min. factor:    131  ticks( 266 -> 266)
num:     1099726829285419 --> min. factor:    271  ticks( 266 -> 266)
Number with largest min. factor is  115797840077099, with factors:
        [544651, 212609249]
```



## Erlang

Perhaps it is of interest that the code will handle exceptions correctly. If the function (in this case factors/1) throws an exception, then the task will get it. I had to copy factors/1 from [[Prime_decomposition]] since it is only a fragment, not a complete example.

```Erlang

-module( parallel_calculations ).

-export( [fun_results/2, task/0] ).

fun_results( Fun, Datas ) ->
        My_pid = erlang:self(),
	Pids = [fun_spawn( Fun, X, My_pid ) || X <- Datas],
	[fun_receive(X) || X <- Pids].

task() ->
    Numbers = [12757923, 12878611, 12757923, 15808973, 15780709, 197622519],
    Results = fun_results( fun factors/1, Numbers ),
    Min_results = [lists:min(X) || X <- Results],
    {_Max_min_factor, Number} = lists:max( lists:zip(Min_results, Numbers) ),
    {Number, Factors} = lists:keyfind( Number, 1, lists:zip(Numbers, Results) ),
    io:fwrite( "~p has largest minimal factor among its prime factors ~p~n", [Number, Factors] ).



factors(N) -> factors(N,2,[]).

factors(1,_,Acc) -> Acc;
factors(N,K,Acc) when N rem K == 0 -> factors(N div K,K, [K|Acc]);
factors(N,K,Acc) -> factors(N,K+1,Acc).

fun_receive( Pid ) ->
        receive
        {ok, Result, Pid} -> Result;
	{Type, Error, Pid} -> erlang:Type( Error )
        end.

fun_spawn( Fun, Data, My_pid ) ->
        erlang:spawn( fun() ->
                Result = try
                       {ok, Fun(Data), erlang:self()}

		       catch
	               Type:Error -> {Type, Error, erlang:self()}

		       end,
	        My_pid ! Result
        end ).

```

```txt

8> parallel_calculations:task().
12878611 has largest minimal factor among its prime factors [2713,101,47]

```



## Factor


### Manual Thread Management



```factor

USING: io kernel fry locals sequences arrays math.primes.factors math.parser channels threads prettyprint ;
IN: <filename>

:: map-parallel ( seq quot -- newseq )
    <channel> :> ch
    seq [ '[ _ quot call ch to ] "factors" spawn ] { } map-as
    dup length [ ch from ] replicate nip ;

{ 576460752303423487 576460752303423487
  576460752303423487 112272537195293
  115284584522153 115280098190773
  115797840077099 112582718962171
  112272537095293 1099726829285419 }
dup [ factors ] map-parallel
dup [ infimum ] map dup supremum
swap index swap dupd nth -rot swap nth
"Number with largest min. factor is " swap number>string append
", with factors: " append write .

```

```txt

Number with largest min. factor is 576460752303423487, with factors: { 544651 212609249 }

```



### With Concurency Module


```factor

USING: kernel io prettyprint sequences arrays math.primes.factors math.parser concurrency.combinators ;
{ 576460752303423487 576460752303423487 576460752303423487 112272537195293
  115284584522153 115280098190773 115797840077099 112582718962171 }
dup [ factors ] parallel-map dup [ infimum ] map dup supremum
swap index swap dupd nth -rot swap nth
"Number with largest min. factor is " swap number>string append
", with factors: " append write .

```

```txt

Number with largest min. factor is 115797840077099, with factors: { 544651 212609249 }

```


=={{header|F_Sharp|F#}}==

```fsharp
open System
open PrimeDecomp // Has the decompose function from the Prime decomposition task

let data = [112272537195293L; 112582718962171L; 112272537095293L; 115280098190773L; 115797840077099L; 1099726829285419L]
let decomp num = decompose num 2L

let largestMinPrimeFactor (numbers: int64 list) =
    let decompDetails = Async.Parallel [ for n in numbers -> async { return n, decomp n } ] // Compute the number and its prime decomposition list
                        |> Async.RunSynchronously                                           // Start and wait for all parallel computations to complete.
                        |> Array.sortBy (snd >> List.min >> (~-))                           // Sort in descending order, based on the min prime decomp number.

    decompDetails.[0]

let showLargestMinPrimeFactor numbers =
    let number, primeList = largestMinPrimeFactor numbers
    printf "Number %d has largest minimal factor:\n    " number
    List.iter (printf "%d ") primeList

showLargestMinPrimeFactor data
```


```txt

Number 115797840077099 has largest minimal factor:
    544651 212609249

```




## Fortran

Using OpenMP (compile with -fopenmp)


```fortran

program Primes

    use ISO_FORTRAN_ENV

    implicit none

    integer(int64), dimension(7) :: data = (/2099726827, 15780709, 1122725370, 15808973, 576460741, 12878611, 12757923/)
    integer(int64), dimension(100) :: outprimes
    integer(int64) :: largest_factor = 0, largest = 0, minim = 0, val = 0
    integer(int16) :: count = 0, OMP_GET_THREAD_NUM

    call omp_set_num_threads(4);
    !$omp parallel do private(val,outprimes,count) shared(data,largest_factor,largest)
    do val = 1, 7
        outprimes = 0
        call find_factors(data(val), outprimes, count)
        minim = minval(outprimes(1:count))
        if (minim > largest_factor) then
            largest_factor = minim
            largest = data(val)
        end if
        write(*, fmt = '(A7,i0,A2,i12,100i12)') 'Thread ', OMP_GET_THREAD_NUM(), ': ', data(val), outprimes(1:count)
    end do
    !$omp end parallel do

    write(*, fmt = '(i0,A26,i0)') largest, ' have the Largest factor: ', largest_factor

    return

contains

    subroutine find_factors(n, d, count)
        integer(int64), intent(in) :: n
        integer(int64), dimension(:), intent(out) :: d
        integer(int16), intent(out) :: count
        integer(int16) :: i
        integer(int64) :: div, next, rest

        i = 1
        div = 2; next = 3; rest = n

        do while (rest /= 1)
            do while (mod(rest, div) == 0)
                d(i) = div
                i = i + 1
                rest = rest / div
            end do
            div = next
            next = next + 2
        end do
        count = i - 1
    end subroutine find_factors

end program Primes

```


```txt

Thread 3:     12757923           3           3         283        5009
Thread 1:   1122725370           2           3           5          13     2878783
Thread 1:     15808973          29         347        1571
Thread 2:    576460741          19    30340039
Thread 2:     12878611          47         101        2713
Thread 0:   2099726827          11   190884257
Thread 0:     15780709           7          17      132611
12878611 have the Largest factor: 47
```



## Go


```go
package main

import (
    "fmt"
    "math/big"
)

// collection of numbers.  A slice is used for the collection.
// The elements are big integers, since that's what the function Primes
// uses (as was specified by the Prime decomposition task.)
var numbers = []*big.Int{
    big.NewInt(12757923),
    big.NewInt(12878611),
    big.NewInt(12878893),
    big.NewInt(12757923),
    big.NewInt(15808973),
    big.NewInt(15780709),
}

// main just calls the function specified by the task description and
// prints results.  note it allows for multiple numbers with the largest
// minimal factor.  the task didn't specify to handle this, but obviously
// it's possible.
func main() {
    rs := lmf(numbers)
    fmt.Println("largest minimal factor:", rs[0].decomp[0])
    for _, r := range rs {
        fmt.Println(r.number, "->", r.decomp)
    }
}

// this type associates a number with it's prime decomposition.
// the type is neccessary so that they can be sent together over
// a Go channel, but it turns out to be convenient as well for
// the return type of lmf.
type result struct {
    number *big.Int
    decomp []*big.Int
}

// the function specified by the task description, "largest minimal factor."
func lmf([]*big.Int) []result {
    // construct result channel and start a goroutine to decompose each number.
    // goroutines run in parallel as CPU cores are available.
    rCh := make(chan result)
    for _, n := range numbers {
        go decomp(n, rCh)
    }

    // collect results.  <-rCh returns a single result from the result channel.
    // we know how many results to expect so code here collects exactly that
    // many results, and accumulates a list of those with the largest
    // minimal factor.
    rs := []result{<-rCh}
    for i := 1; i < len(numbers); i++ {
        switch r := <-rCh; r.decomp[0].Cmp(rs[0].decomp[0]) {
        case 1:
            rs = rs[:1]
            rs[0] = r
        case 0:
            rs = append(rs, r)
        }
    }
    return rs
}

// decomp is the function run as a goroutine.  multiple instances of this
// function will run concurrently, one for each number being decomposed.
// it acts as a driver for Primes, calling Primes as needed, packaging
// the result, and sending the packaged result on the channel.
// "as needed" turns out to mean sending Primes a copy of n, as Primes
// as written is destructive on its argument.
func decomp(n *big.Int, rCh chan result) {
    rCh <- result{n, Primes(new(big.Int).Set(n))}
}

// code below copied from Prime decomposition task
var (
    ZERO = big.NewInt(0)
    ONE  = big.NewInt(1)
)

func Primes(n *big.Int) []*big.Int {
    res := []*big.Int{}
    mod, div := new(big.Int), new(big.Int)
    for i := big.NewInt(2); i.Cmp(n) != 1; {
        div.DivMod(n, i, mod)
        for mod.Cmp(ZERO) == 0 {
            res = append(res, new(big.Int).Set(i))
            n.Set(div)
            div.DivMod(n, i, mod)
        }
        i.Add(i, ONE)
    }
    return res
}
```

```txt

largest minimal factor: 47
12878611 -> [47 101 2713]
12878893 -> [47 274019]

```



## Haskell


```haskell
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData)
import Data.List (maximumBy)
import Data.Function (on)

nums :: [Integer]
nums =
  [ 112272537195293
  , 112582718962171
  , 112272537095293
  , 115280098190773
  , 115797840077099
  , 1099726829285419
  ]

lowestFactor
  :: Integral a
  => a -> a -> a
lowestFactor s n
  | even n = 2
  | otherwise = head y
  where
    y =
      [ x
      | x <- [s .. ceiling . sqrt $ fromIntegral n] ++ [n]
      , n `rem` x == 0
      , odd x ]

primeFactors
  :: Integral a
  => a -> a -> [a]
primeFactors l n = f n l []
  where
    f n l xs =
      if n > 1
        then f (n `div` l) (lowestFactor (max l 3) (n `div` l)) (l : xs)
        else xs

minPrimes
  :: (Control.DeepSeq.NFData a, Integral a)
  => [a] -> (a, [a])
minPrimes ns =
  (\(x, y) -> (x, primeFactors y x)) $
  maximumBy (compare `on` snd) $ zip ns (parMap rdeepseq (lowestFactor 3) ns)

main :: IO ()
main = print $ minPrimes nums
```

```txt
(115797840077099,[212609249,544651])
```


==Icon and {{header|Unicon}}==

The following only works in Unicon.


```unicon
procedure main(A)
    threads := []
    L := list(*A)
    every i := 1 to *A do put(threads, thread L[i] := primedecomp(A[i]))
    every wait(!threads)

    maxminF := L[maxminI := 1][1]
    every i := 2 to *L do if maxminF <:= L[i][1] then maxminI := i
    every writes((A[maxminI]||": ")|(!L[maxminI]||" ")|"\n")
end

procedure primedecomp(n)         #: return a list of factors
    every put(F := [], genfactors(n))
    return F
end

link factors

```


Sample run:

```txt

->pc 57646075230343 112272537195 115284584525
57646075230343: 8731 6602459653
->

```



## J

The code at [http://code.jsoftware.com/wiki/User:Marshall_Lochbaum/Parallelize] implements parallel computation. With it, we can write

```j
   numbers =. 12757923 12878611 12878893 12757923 15808973 15780709 197622519
   factors =. q:&.> parallelize 2 numbers NB. q: is parallelized here
   ind =. (i. >./) <./@> factors
   ind { numbers ;"_1 factors
┌────────┬───────────┐
│12878611│47 101 2713│
└────────┴───────────┘
```



## Java

```java
import static java.lang.System.out;
import static java.util.Arrays.stream;
import static java.util.Comparator.comparing;

public interface ParallelCalculations {
    public static final long[] NUMBERS = {
      12757923,
      12878611,
      12878893,
      12757923,
      15808973,
      15780709,
      197622519
    };

    public static void main(String... arguments) {
      stream(NUMBERS)
        .unordered()
        .parallel()
        .mapToObj(ParallelCalculations::minimalPrimeFactor)
        .max(comparing(a -> a[0]))
        .ifPresent(res -> out.printf(
          "%d has the largest minimum prime factor: %d%n",
          res[1],
          res[0]
        ));
    }

    public static long[] minimalPrimeFactor(long n) {
      for (long i = 2; n >= i * i; i++) {
        if (n % i == 0) {
          return new long[]{i, n};
        }
      }
      return new long[]{n, n};
    }
}
```



```txt
12878611 has the largest minimum prime factor: 47
```



## JavaScript

This code demonstrates Web Workers. This should work on current versions of Firefox, Safari, Chrome and Opera.

This first portion should be placed in a file called "parallel_worker.js". This file contains the logic used by every worker created.

```javascript

var onmessage = function(event) {
    postMessage({"n" : event.data.n,
                 "factors" : factor(event.data.n),
                 "id" : event.data.id});
};

function factor(n) {
    var factors = [];
    for(p = 2; p <= n; p++) {
        if((n % p) == 0) {
            factors[factors.length] = p;
            n /= p;
        }
    }
    return factors;
}

```


For each number a worker is spawned. Once the final worker completes its task (worker_count is reduced to 0), the reduce function is called to determine which number is the answer.

```javascript

var numbers = [12757923, 12878611, 12757923, 15808973, 15780709, 197622519];
var workers = [];
var worker_count = 0;

var results = [];

for(var i = 0; i < numbers.length; i++) {
    worker_count++;
    workers[i] = new Worker("parallel_worker.js");
    workers[i].onmessage = accumulate;
    workers[i].postMessage({n: numbers[i], id: i});
}

function accumulate(event) {
    n = event.data.n;
    factors = event.data.factors;
    id = event.data.id;
    console.log(n + " : " + factors);
    results[id] = {n:n, factors:factors};
    // Cleanup - kill the worker and countdown until all work is done
    workers[id].terminate();
    worker_count--;
    if(worker_count == 0)
	reduce();
}

function reduce() {
    answer = 0;
    for(i = 1; i < results.length; i++) {
	min = results[i].factors[0];
	largest_min = results[answer].factors[0];
	if(min > largest_min)
	    answer = i;
    }
    n = results[answer].n;
    factors = results[answer].factors;
    console.log("The number with the relatively largest factors is: " + n + " : " + factors);
}

```



## Julia


```Julia

using Primes

factortodict(d, n) = (d[minimum(collect(keys(factor(n))))] = n)

# Numbers are from from the Perl 6 example.
numbers = [64921987050997300559,  70251412046988563035,  71774104902986066597,
           83448083465633593921,  84209429893632345702,  87001033462961102237,
           87762379890959854011,  89538854889623608177,  98421229882942378967,
           259826672618677756753, 262872058330672763871, 267440136898665274575,
           278352769033314050117, 281398154745309057242, 292057004737291582187]

mins = Dict()

Base.@sync(
    Threads.@threads for n in numbers
        factortodict(mins, n)
    end
)

answer = maximum(keys(mins))
println("The number that has the largest minimum prime factor is $(mins[answer]), with a smallest factor of $answer")

```

```txt

The number that has the largest minimum prime factor is 98421229882942378967, with a smallest factor of 736717

```



## Kotlin


```scala
// version 1.1.51

import java.util.stream.Collectors

/* returns the number itself, its smallest prime factor and all its prime factors */
fun primeFactorInfo(n: Int): Triple<Int, Int, List<Int>> {
    if (n <= 1) throw IllegalArgumentException("Number must be more than one")
    if (isPrime(n)) return Triple(n, n, listOf(n))
    val factors = mutableListOf<Int>()
    var factor = 2
    var nn = n
    while (true) {
        if (nn % factor == 0) {
            factors.add(factor)
            nn /= factor
            if (nn == 1) return Triple(n, factors.min()!!, factors)
            if (isPrime(nn)) factor = nn
        }
        else if (factor >= 3) factor += 2
        else factor = 3
    }
}

fun isPrime(n: Int) : Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}

fun main(args: Array<String>) {
    val numbers = listOf(
        12757923, 12878611, 12878893, 12757923, 15808973, 15780709, 197622519
    )
    val info = numbers.stream()
                      .parallel()
                      .map { primeFactorInfo(it) }
                      .collect(Collectors.toList())
    val maxFactor = info.maxBy { it.second }!!.second
    val results = info.filter { it.second == maxFactor }
    println("The following number(s) have the largest minimal prime factor of $maxFactor:")
    for (result in results) {
        println("  ${result.first} whose prime factors are ${result.third}")
    }
}
```


```txt

The following number(s) have the largest minimal prime factor of 47:
  12878611 whose prime factors are [47, 101, 2713]
  12878893 whose prime factors are [47, 274019]

```



## Mathematica


```Mathematica

hasSmallestFactor[data_List]:=Sort[Transpose[{ParallelTable[FactorInteger[x][[1, 1]], {x, data}],data}]][[1, 2]]
```



## Oforth


Version used for #factors is [[Prime decomposition]].

mapParallel runs parallel computations : each element of the list is computed into a separate task.

Default is to use as many workers as number of cores.

Numbers of workers to use can be adjusted using --W command line option.


```Oforth
import: parallel

: largeMinFactor  dup mapParallel(#factors) zip maxFor(#[ second first ]) ;
```


```txt

[ 12757923, 12878611, 12757923, 15808973, 15780709, 197622519 ] largeMinFactor println
[12878611, [47, 101, 2713]]

```



## ooRexx

This program calls the programs shown under REXX (modified for ooRexx and slightly expanded).

```oorexx
/* Concurrency in ooRexx. Example of early reply */
object1 = .example~new
object2 = .example~new
say object1~primes(1,11111111111,11111111114)
say object2~primes(2,11111111111,11111111114)
say "Main ended at" time()
exit
::class example
::method primes
use arg which,bot,top
reply "Start primes"which':' time()
Select
  When which=1 Then Call pd1 bot top
  When which=2 Then Call pd2 bot top
  End
```

```txt
Start primes1: 09:25:25
Start primes2: 09:25:25
11111111111       (2) prime factors:           21649 513239
Main ended at 09:25:25
11111111112       (5) prime factors:           2 2 2 3 462962963
11111111111       (2) prime factors:           21649 513239
11111111112       (5) prime factors:           2 2 2 3 462962963
11111111113       (1) prime factors:  [prime]  11111111113
11111111113       (1) prime factors:  [prime]  11111111113
11111111114       (2) prime factors:           2 5555555557

                    1 primes found.
PD1 took 1.203000 seconds
11111111114       (2) prime factors:           2 5555555557

                    1 primes found.
PD2 took 1.109000 seconds
```


```rexx

/*PD1 REXX pgm does prime decomposition of a range of positive integers (with a prime count)*/
Call Time 'R'
numeric digits 1000                              /*handle thousand digits for the powers*/
parse arg  bot  top  step   base  add            /*get optional arguments from the C.L. */
if  bot==''   then do;  bot=1;  top=100;  end    /*no  BOT given?  Then use the default.*/
if  top==''   then              top=bot          /* "  TOP?  "       "   "   "     "    */
if step==''   then step=  1                      /* " STEP?  "       "   "   "     "    */
if add ==''   then  add= -1                      /* "  ADD?  "       "   "   "     "    */
tell= top>0;       top=abs(top)                  /*if TOP is negative, suppress displays*/
w=length(top)                                    /*get maximum width for aligned display*/
if base\==''  then w=length(base**top)           /*will be testing powers of two later? */
commat.=left('', 7);   commat.0="{unity}";   commat.1='[prime]' /*some literals:  pad;  prime (or not).*/
numeric digits max(9, w+1)                       /*maybe increase the digits precision. */
hash=0                                              /*hash:    is the number of primes found. */
        do n=bot  to top  by step                /*process a single number  or  a range.*/
        ?=n;  if base\==''  then ?=base**n + add /*should we perform a "Mercenne" test? */
        pf=factr(?);      f=words(pf)            /*get prime factors; number of factors.*/
        if f==1  then hash=hash+1                      /*Is N prime?  Then bump prime counter.*/
        if tell  then say right(?,w)   right('('f")",9)   'prime factors: '     commat.f     pf
        end   /*n*/
say
ps= 'primes';    if p==1  then ps= "prime"       /*setup for proper English in sentence.*/
say right(hash, w+9+1)       ps       'found.'      /*display the number of primes found.  */
Say 'PD1 took' time('E') 'seconds'
exit                                             /*stick a fork in it,  we're all done. */
/*--------------------------------------------------------------------------------------*/
factr: procedure;  parse arg x 1 d,dollar             /*set X, D  to argument 1;  dollar  to null.*/
if x==1  then return ''                          /*handle the special case of   X = 1.  */
       do  while x//2==0;  dollar=dollar 2;  x=x%2;  end   /*append all the  2  factors of new  X.*/
       do  while x//3==0;  dollar=dollar 3;  x=x%3;  end   /*   "    "   "   3     "     "  "   " */
       do  while x//5==0;  dollar=dollar 5;  x=x%5;  end   /*   "    "   "   5     "     "  "   " */
       do  while x//7==0;  dollar=dollar 7;  x=x%7;  end   /*   "    "   "   7     "     "  "   " */
                                                 /*                                  ___*/
q=1;   do  while q<=x;  q=q*4;  end              /*these two lines compute integer  v X */
r=0;   do  while q>1;   q=q%4;  _=d-r-q;  r=r%2;   if _>=0  then do; d=_; r=r+q; end;  end

       do j=11  by 6  to r                       /*insure that  J  isn't divisible by 3.*/
       parse var j  ''  -1  _                    /*obtain the last decimal digit of  J. */
       if _\==5  then  do  while x//j==0;  dollar=dollar j;  x=x%j;  end     /*maybe reduce by J. */
       if _ ==3  then iterate                    /*Is next  Y  is divisible by 5?  Skip.*/
       y=j+2;          do  while x//y==0;  dollar=dollar y;  x=x%y;  end     /*maybe reduce by J. */
       end   /*j*/
                                                 /* [?]  The dollar list has a leading blank.*/
if x==1  then return dollar                           /*Is residual=unity? Then don't append.*/
              return dollar x                         /*return   dollar   with appended residual. */
```


```rexx
/*PD2 REXX pgm does prime decomposition of a range of positive integers (with a prime count)*/
Call time 'R'
numeric digits 1000                              /*handle thousand digits for the powers*/
parse arg  bot  top  step   base  add            /*get optional arguments from the C.L. */
if  bot==''   then do;  bot=1;  top=100;  end    /*no  BOT given?  Then use the default.*/
if  top==''   then              top=bot          /* "  TOP?  "       "   "   "     "    */
if step==''   then step=  1                      /* " STEP?  "       "   "   "     "    */
if add ==''   then  add= -1                      /* "  ADD?  "       "   "   "     "    */
tell= top>0;       top=abs(top)                  /*if TOP is negative, suppress displays*/
w=length(top)                                    /*get maximum width for aligned display*/
if base\==''  then w=length(base**top)           /*will be testing powers of two later? */
commat.=left('', 7);   commat.0="{unity}";   commat.1='[prime]' /*some literals:  pad;  prime (or not).*/
numeric digits max(9, w+1)                       /*maybe increase the digits precision. */
hash=0                                              /*hash:    is the number of primes found. */
        do n=bot  to top  by step                /*process a single number  or  a range.*/
        ?=n;  if base\==''  then ?=base**n + add /*should we perform a "Mercenne" test? */
        pf=factr(?);      f=words(pf)            /*get prime factors; number of factors.*/
        if f==1  then hash=hash+1                      /*Is N prime?  Then bump prime counter.*/
        if tell  then say right(?,w)   right('('f")",9)   'prime factors: '     commat.f     pf
        end   /*n*/
say
ps= 'primes';    if p==1  then ps= "prime"       /*setup for proper English in sentence.*/
say right(hash, w+9+1)       ps       'found.'      /*display the number of primes found.  */
Say 'PD2 took' time('E') 'seconds'
exit                                             /*stick a fork in it,  we're all done. */
/*--------------------------------------------------------------------------------------*/
factr: procedure;  parse arg x 1 d,dollar             /*set X, D  to argument 1;  dollar  to null.*/
if x==1  then return ''                          /*handle the special case of   X = 1.  */
       do  while x// 2==0;  dollar=dollar  2;  x=x%2;  end /*append all the  2  factors of new  X.*/
       do  while x// 3==0;  dollar=dollar  3;  x=x%3;  end /*   "    "   "   3     "     "  "   " */
       do  while x// 5==0;  dollar=dollar  5;  x=x%5;  end /*   "    "   "   5     "     "  "   " */
       do  while x// 7==0;  dollar=dollar  7;  x=x%7;  end /*   "    "   "   7     "     "  "   " */
       do  while x//11==0;  dollar=dollar 11;  x=x%11; end /*   "    "   "  11     "     "  "   " */    /* ?¦¦¦¦ added.*/
       do  while x//13==0;  dollar=dollar 13;  x=x%13; end /*   "    "   "  13     "     "  "   " */    /* ?¦¦¦¦ added.*/
       do  while x//17==0;  dollar=dollar 17;  x=x%17; end /*   "    "   "  17     "     "  "   " */    /* ?¦¦¦¦ added.*/
       do  while x//19==0;  dollar=dollar 19;  x=x%19; end /*   "    "   "  19     "     "  "   " */    /* ?¦¦¦¦ added.*/
       do  while x//23==0;  dollar=dollar 23;  x=x%23; end /*   "    "   "  23     "     "  "   " */    /* ?¦¦¦¦ added.*/
                                                 /*                                  ___*/
q=1;   do  while q<=x;  q=q*4;  end              /*these two lines compute integer  v X */
r=0;   do  while q>1;   q=q%4;  _=d-r-q;  r=r%2;   if _>=0  then do; d=_; r=r+q; end;  end

       do j=29  by 6  to r                       /*insure that  J  isn't divisible by 3.*/    /* ?¦¦¦¦ changed.*/
       parse var j  ''  -1  _                    /*obtain the last decimal digit of  J. */
       if _\==5  then  do  while x//j==0;  dollar=dollar j;  x=x%j;  end     /*maybe reduce by J. */
       if _ ==3  then iterate                    /*Is next  Y  is divisible by 5?  Skip.*/
       y=j+2;          do  while x//y==0;  dollar=dollar y;  x=x%y;  end     /*maybe reduce by J. */
       end   /*j*/
                                                 /* [?]  The dollar list has a leading blank.*/
if x==1  then return dollar                           /*Is residual=unity? Then don't append.*/
              return dollar x                         /*return   dollar   with appended residual. */
```



## PARI/GP

See [http://pari.math.u-bordeaux1.fr/Events/PARI2012/talks/pareval.pdf Bill Allombert's slides on parallel programming in GP]. This can be configured to use either MPI (good for many networked computers) or pthreads (good for a single machine).
```parigp
v=pareval(vector(1000,i,()->factor(2^i+1)[1,1]));
vecmin(v)
```



## OxygenBasic


```oxygenbasic

'CONFIGURATION
'
### =======


% max     8192  'Maximum amount of Prime Numbers (must be 2^n) (excluding 1 and 2)
% cores   4     'CPU cores available (limited to 4 here)
% share   2048  'Amount of numbers allocated to each core

'SETUP
'=====

'SOURCE DATA BUFFERS

sys primes[max]
sys numbers[max]

'RESULT BUFFER

double pp[max] 'main thread


'MULTITHREADING AND TIMING API
'
### =======================


extern lib "kernel32.dll"
'
void   QueryPerformanceCounter(quad*c)
void   QueryPerformanceFrequency(quad*freq)
sys    CreateThread (sys lpThreadAttributes, dwStackSize, lpStartAddress, lpParameter, dwCreationFlags, *lpThreadId)
dword  WaitForMultipleObjects(sys nCount,*lpHandles, bWaitAll, dwMilliseconds)
bool   CloseHandle(sys hObject)
void   Sleep(sys dwMilliSeconds)
'
quad freq,t1,t2
QueryPerformanceFrequency freq


'MACROS AND FUNCTIONS
'
### ==============



macro FindPrimes(p)
'
### ============

finit
sys n=1
sys c,k
do
  n+=2
  if c>=max then exit do
  '
  'IS IT DIVISIBLE BE ANY PREVIOUS PRIME
  '
  for k=1 to c
     if frac(n/p[k])=0 then exit for
  next
  '
  if k>c then
    c++
    p[c]=n 'STORE PRIME
  end if
end do
end macro


macro ProcessNumbers(p,bb)
'
### ===================

finit
sys i,b,e
b=bb*share
e=b+share
sys v,w
for i=b+1 to e
  v=numbers(i)
  for j=max to 1 step -1
    w=primes(j)
    if w<v then
      if frac(v/w)=0 then
        p(i)=primes(j)    'store highest factor
        exit for          'process next number
      end if
    end if
  next
next
end macro

'THREAD FUNCTIONS

function threadA(sys v) as sys
ProcessNumbers(pp,v)
end function


function threadB(sys v) as sys
ProcessNumbers(pp,v)
end function


function threadC(sys v) as sys
ProcessNumbers(pp,v)
end function


end extern

function mainThread(sys b)
'
### =====================

ProcessNumbers(pp,b)
end function


'SOURCE DATA GENERATION

sys seed = 0x12345678

function Rnd() as sys
'
### ==============

'
mov eax,seed
rol eax,7
imul eax,eax,13
mov seed,eax
return eax
end function


function GenerateNumbers()
'
### ===================

sys i,v,mask
mask=max * 8 -1 'as bit mask
for i=1 to max
  v=rnd()
  v and=mask
  numbers(i)=v
next
end function



FindPrimes(primes)

GenerateNumbers()



% threads Cores-1

% INFINITE 0xFFFFFFFF  'Infinite timeout

sys Funs[threads]={@threadA,@threadB,@threadC} '3 additional threads
sys  hThread[threads], id[threads], i
'
'START TIMER
'
QueryPerformanceCounter   t1
'
for i=1 to threads
  hThread(i) =  CreateThread 0,0,funs(i),i,0,id(i)
next


MainThread(0) 'process numbers in main thread (bottom share)

if threads>0 then
  WaitForMultipleObjects Threads, hThread, 1, INFINITE
end if

for i=1 to Threads
  CloseHandle hThread(i)
next

'CAPTURE NUMBER WITH HIGHEST PRIME FACTOR

sys n,f
for i=1 to max
  if pp(i)>f then f=pp(i) : n=i
next

'STOP TIMER

QueryPerformanceCounter t2

print str((t2-t1)/freq,3) " secs    " numbers(n) "    " f 'number with highest prime factor

```



## Perl

```perl
use ntheory qw/factor vecmax/;
use threads;
use threads::shared;
my @results :shared;

my $tnum = 0;
$_->join() for
  map { threads->create('tfactor', $tnum++, $_) }
  (qw/576460752303423487 576460752303423487 576460752303423487 112272537195293
  115284584522153 115280098190773 115797840077099 112582718962171 299866111963290359/);

my $lmf = vecmax( map { $_->[1] } @results );
print "Largest minimal factor of $lmf found in:\n";
print "  $_->[0] = [@$_[1..$#$_]]\n" for grep { $_->[1] == $lmf } @results;

sub tfactor {
  my($tnum, $n) = @_;
  push @results, shared_clone([$n, factor($n)]);
}
```

```txt

Largest minimal factor of 544651 found in:
  115797840077099 = [544651 212609249]
  299866111963290359 = [544651 550565613509]

```



## Perl 6

Takes the list of numbers and converts them to a <tt>HyperSeq</tt> that is stored in a variable and evaluated concurrently. <tt>HyperSeq</tt>s overload <tt>map</tt> and <tt>grep</tt> to convert and pick values in worker threads. The runtime will pick the number of OS-level threads and assign worker threads to them while avoiding stalling in any part of the program. A <tt>HyperSeq</tt> is lazy, so the computation of values will happen in chunks as they are requested.

The hyper (and race) method can take two parameters that will tweak how the parallelization occurs: :degree and :batch. :degree is the number of worker threads to allocate to the job. By default it is set to the number of physical cores available. If you have a hyper threading processor, and the tasks are not cpu bound, it may be useful to raise that number but it is a reasonable default. :batch is how many sub-tasks are parceled out at a time to each worker thread. Default is 64. For small numbers of cpu intensive tasks a lower number will likely be better, but too low may make the dispatch overhead cancel out the benefit of threading. Conversely, too high will over-burden some threads and starve others. Over long-running processes with many hundreds / thousands of sub-tasks, the scheduler will automatically adjust the batch size up or down to try to keep the pipeline filled.

On my system, under the load I was running, I found a batch size of 3 to be optimal for this task. May be different for different systems and different loads.

As a relative comparison, perform the same factoring task on the same set of 100 numbers as found in the [[Parallel_calculations#SequenceL|SequenceL]] example, using varying numbers of threads. The absolute speed numbers are not very significant, they will vary greatly between systems, this is more intended as a comparison of relative throughput. On a Core i7-4770 @ 3.40GHz with 4 cores and hyper-threading under Linux, there is a distinct pattern where more threads on physical cores give reliable increases in throughput. Adding hyperthreads may (and, in this case, does seem to) give some additional marginal benefit.

Using the <tt>prime-factors</tt> routine as defined in the [[Prime_decomposition#Perl_6 |prime decomposition]] task.

```perl6
my @nums = 64921987050997300559,  70251412046988563035,  71774104902986066597,
           83448083465633593921,  84209429893632345702,  87001033462961102237,
           87762379890959854011,  89538854889623608177,  98421229882942378967,
           259826672618677756753, 262872058330672763871, 267440136898665274575,
           278352769033314050117, 281398154745309057242, 292057004737291582187;

my @factories = @nums.hyper(:3batch).map: *.&prime-factors;
printf "%21d factors: %s\n", |$_ for @nums Z @factories;
my $gmf = {}.append(@factories»[0] »=>« @nums).max: +*.key;
say "\nGreatest minimum factor: ", $gmf.key;
say "from: { $gmf.value }\n";
say 'Run time: ', now - INIT now;
say '-' x 80;

# For amusements sake and for relative comparison, using the same 100
# numbers as in the SequenceL example, testing with different numbers of threads.

@nums = <625070029 413238785 815577134 738415913 400125878 967798656 830022841
   774153795 114250661 259366941 571026384 522503284 757673286 509866901 6303092
   516535622 177377611 520078930 996973832 148686385 33604768 384564659 95268916
   659700539 149740384 320999438 822361007 701572051 897604940 2091927 206462079
   290027015 307100080 904465970 689995756 203175746 802376955 220768968 433644101
   892007533 244830058 36338487 870509730 350043612 282189614 262732002 66723331
   908238109 635738243 335338769 461336039 225527523 256718333 277834108 430753136
   151142121 602303689 847642943 538451532 683561566 724473614 422235315 921779758
   766603317 364366380 60185500 333804616 988528614 933855820 168694202 219881490
   703969452 308390898 567869022 719881996 577182004 462330772 770409840 203075270
   666478446 351859802 660783778 503851023 789751915 224633442 347265052 782142901
   43731988 246754498 736887493 875621732 594506110 854991694 829661614 377470268
   984990763 275192380 39848200 892766084 76503760>».Int;

for 1..8 -> $degree {
    my $start = now;
    my \factories = @nums.hyper(:degree($degree), :3batch).map: *.&prime-factors;
    my $gmf = {}.append(factories»[0] »=>« @nums).max: +*.key;
    say "\nFactoring {+@nums} numbers, greatest minimum factor: {$gmf.key}";
    say "Using: $degree thread{ $degree > 1 ?? 's' !! ''}";
    my $end = now;
    say 'Run time: ', $end - $start, ' seconds.';
}

# Prime factoring routines from the Prime decomposition task
sub prime-factors ( Int $n where * > 0 ) {
    return $n if $n.is-prime;
    return [] if $n == 1;
    my $factor = find-factor( $n );
    sort flat prime-factors( $factor ), prime-factors( $n div $factor );
}

sub find-factor ( Int $n, $constant = 1 ) {
    return 2 unless $n +& 1;
    if (my $gcd = $n gcd 6541380665835015) > 1 {
        return $gcd if $gcd != $n
    }
    my $x      = 2;
    my $rho    = 1;
    my $factor = 1;
    while $factor == 1 {
        $rho *= 2;
        my $fixed = $x;
        for ^$rho {
            $x = ( $x * $x + $constant ) % $n;
            $factor = ( $x - $fixed ) gcd $n;
            last if 1 < $factor;
        }
    }
    $factor = find-factor( $n, $constant + 1 ) if $n == $factor;
    $factor;
}
```

```txt
 64921987050997300559 factors: 736717 88123373087627
 70251412046988563035 factors: 5 43 349 936248577956801
 71774104902986066597 factors: 736717 97424255043641
 83448083465633593921 factors: 736717 113270202079813
 84209429893632345702 factors: 2 3 3 3 41 107880821 352564733
 87001033462961102237 factors: 736717 118092881612561
 87762379890959854011 factors: 3 3 3 3 331 3273372119315201
 89538854889623608177 factors: 736717 121537652707381
 98421229882942378967 factors: 736717 133594351539251
259826672618677756753 factors: 7 37118096088382536679
262872058330672763871 factors: 3 47 1864340839224629531
267440136898665274575 factors: 3 5 5 71 50223499887073291
278352769033314050117 factors: 7 39764681290473435731
281398154745309057242 factors: 2 809 28571 46061 132155099
292057004737291582187 factors: 7 151 373 2339 111323 2844911

Greatest minimum factor: 736717
from: 64921987050997300559 71774104902986066597 83448083465633593921 87001033462961102237 89538854889623608177 98421229882942378967

Run time: 0.2968644
--------------------------------------------------------------------------------

Factoring 100 numbers, greatest minimum factor: 782142901
Using: 1 thread
Run time: 0.3438752 seconds.

Factoring 100 numbers, greatest minimum factor: 782142901
Using: 2 threads
Run time: 0.2035372 seconds.

Factoring 100 numbers, greatest minimum factor: 782142901
Using: 3 threads
Run time: 0.14177834 seconds.

Factoring 100 numbers, greatest minimum factor: 782142901
Using: 4 threads
Run time: 0.110738 seconds.

Factoring 100 numbers, greatest minimum factor: 782142901
Using: 5 threads
Run time: 0.10142434 seconds.

Factoring 100 numbers, greatest minimum factor: 782142901
Using: 6 threads
Run time: 0.10954304 seconds.

Factoring 100 numbers, greatest minimum factor: 782142901
Using: 7 threads
Run time: 0.097886 seconds.

Factoring 100 numbers, greatest minimum factor: 782142901
Using: 8 threads
Run time: 0.0927695 seconds.
```


Beside <tt>HyperSeq</tt> and its (allowed to be) out-of-order equivalent <tt>RaceSeq</tt>, [[Rakudo]] supports primitive threads, locks and highlevel promises. Using channels and supplies values can be move thread-safely from one thread to another. A react-block can be used as a central hub for message passing.

In [[Perl 6]] most errors are bottled up <tt>Exceptions</tt> inside <tt>Failure</tt> objects that remember where they are created and thrown when used. This is useful to pass errors from one thread to another without losing file and line number of the source file that caused the error.

In the future hyper operators, junctions and feeds will be candidates for autothreading.


## Phix

```Phix
-- demo/rosetta/ParallelCalculations.exw
include mpfr.e
sequence res
constant res_cs = init_cs()         -- critical section

procedure athread()
    mpz z = mpz_init()
    while true do
        integer found = 0
        enter_cs(res_cs)
        for i=1 to length(res) do
            if integer(res[i]) then
                found = i
                res[i] = {}
                exit
            end if
        end for
        leave_cs(res_cs)
        if not found then exit end if
        mpz_ui_pow_ui(z,2,found)
        mpz_add_ui(z,z,1)
        sequence r = mpz_prime_factors(z, 1_000_000)
        enter_cs(res_cs)
        res[found] = r
        leave_cs(res_cs)
    end while
    exit_thread(0)
end procedure

for nthreads=1 to 5 do
    atom t0 = time()
    res = tagset(100)
    sequence threads = {}
    for i=1 to nthreads do
        threads = append(threads,create_thread(routine_id("athread"),{}))
    end for
    wait_thread(threads)
    integer k = largest(res,true)
    string e = elapsed(time()-t0)
    printf(1,"largest is 2^%d+1 with smallest factor of %d (%d threads, %s)\n",
             {k,res[k][1][1],nthreads,e})
end for
delete_cs(res_cs)
```

```txt

largest is 2^64+1 with smallest factor of 274177 (1 threads, 9.5s)
largest is 2^64+1 with smallest factor of 274177 (2 threads, 7.1s)
largest is 2^64+1 with smallest factor of 274177 (3 threads, 5.8s)
largest is 2^64+1 with smallest factor of 274177 (4 threads, 4.5s)
largest is 2^64+1 with smallest factor of 274177 (5 threads, 4.5s)

```

IE: Checking the first 1 million primes as factors of 2^(1..100)+1 takes 1 core 9.5s and less when spread over multiple cores.

Note however that I added quite a bit of locking to mpz_prime_factors(), specifically around get_prime() and mpz_probable_prime(),
[happy to leave it in since the effect on the single-thread case was neglible] so it is really only the mpz_divisible_ui_p() calls
and some control-flow scaffolding that is getting parallelised. I only got 4 cores so the 5th thread was not expected to help.


## PicoLisp

The '[http://software-lab.de/doc/refL.html#later later]' function is used in
PicoLisp to start parallel computations. The following solution calls 'later' on
the 'factor' function from [[Prime decomposition#PicoLisp]], and then
'[http://software-lab.de/doc/refW.html#wait wait]'s until all results are
available:

```PicoLisp
(let Lst
   (mapcan
      '((N)
         (later (cons)               # When done,
            (cons N (factor N)) ) )  # return the number and its factors
      (quote
         188573867500151328137405845301  # Process a collection of 12 numbers
         3326500147448018653351160281
         979950537738920439376739947
         2297143294659738998811251
         136725986940237175592672413
         3922278474227311428906119
         839038954347805828784081
         42834604813424961061749793
         2651919914968647665159621
         967022047408233232418982157
         2532817738450130259664889
         122811709478644363796375689 ) )
   (wait NIL (full Lst))  # Wait until all computations are done
   (maxi '((L) (apply min L)) Lst) )  # Result: Number in CAR, factors in CDR
```

```txt
-> (2532817738450130259664889 6531761 146889539 2639871491)
```



## Prolog

This piece needs prime_decomp definition from the [[Prime decomposition#Prolog]] example, it worked on my swipl, but I don't know how other Dialects thread.


```Prolog
threaded_decomp(Number,ID):-
	thread_create(
		      (prime_decomp(Number,Y),
		       thread_exit((Number,Y)))
		     ,ID,[]).

threaded_decomp_list(List,Erg):-
	maplist(threaded_decomp,List,IDs),
	maplist(thread_join,IDs,Results),
	maplist(pack_exit_out,Results,Smallest_Factors_List),
	largest_min_factor(Smallest_Factors_List,Erg).

pack_exit_out(exited(X),X).
%Note that here some error handling should happen.

largest_min_factor([(N,Facs)|A],(N2,Fs2)):-
	min_list(Facs,MF),
	largest_min_factor(A,(N,MF,Facs),(N2,_,Fs2)).

largest_min_factor([],Acc,Acc).
largest_min_factor([(N1,Facs1)|Rest],(N2,MF2,Facs2),Goal):-
	min_list(Facs1, MF1),
	(MF1 > MF2->
	largest_min_factor(Rest,(N1,MF1,Facs1),Goal);
	largest_min_factor(Rest,(N2,MF2,Facs2),Goal)).


format_it(List):-
	threaded_decomp_list(List,(Number,Factors)),
	format('Number with largest minimal Factor is ~w\nFactors are ~w\n',
	       [Number,Factors]).

```


Example (Numbers Same as in Ada Example):

```txt
?- ['prime_decomp.prolog', parallel].
% prime_decomp.prolog compiled 0.00 sec, 3,392 bytes
% parallel compiled 0.00 sec, 4,672 bytes
true.
format_it([12757923,
       12878611,
       12757923,
       15808973,
       15780709,
      197622519]).
Number with largest minimal factor is 12878611
Factors are [2713, 101, 47]
true.

```


## PureBasic


```PureBasic
Structure IO_block
  ThreadID.i
  StartSeamaphore.i
  Value.q
  MinimumFactor.i
  List Factors.i()
EndStructure
;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Declare Factorize(*IO.IO_block)
Declare main()
;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Main()
End
;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Procedure Main()
  Protected AvailableCpu, MainSemaphore
  Protected i, j, qData.q, Title$, Message$
  NewList T.IO_block()
  ;
  AvailableCpu = Val(GetEnvironmentVariable("NUMBER_OF_PROCESSORS"))
  If AvailableCpu<1: AvailableCpu=1: EndIf
  MainSemaphore = CreateSemaphore(AvailableCpu)
  ;
  Restore Start_of_data
  For i=1 To (?end_of_data-?Start_of_data) / SizeOf(Quad)
    ; Start all threads at ones, they will then be let to
    ; self-oganize according to the availiable Cores.
    AddElement(T())
    Read.q  qData
    T()\Value = qData
    T()\StartSeamaphore = MainSemaphore
    T()\ThreadID = CreateThread(@Factorize(), @T())
  Next
  ;
  ForEach T()
    ; Wait for all threads to complete their work and
    ; find the smallest factor from eact task.
    WaitThread(T()\ThreadID)
  Next
  ;
  i = OffsetOf(IO_block\MinimumFactor)
  SortStructuredList(T(), #PB_Sort_Integer, i, #PB_Sort_Descending)
  FirstElement(T())
  Title$="Info"
  Message$="Number "+Str(T()\Value)+" has largest minimal factor:"+#CRLF$
  ForEach T()\Factors()
    Message$ + Str(T()\Factors())+" "
  Next
  MessageRequester(Title$, Message$)
EndProcedure

ProcedureDLL Factorize(*IO.IO_block) ; Fill list Factors() with the factor parts of Number
  ;Based on http://rosettacode.org/wiki/Prime_decomposition#PureBasic
  With *IO
    Protected Value.q=\Value
    WaitSemaphore(\StartSeamaphore)
    Protected I = 3
    ClearList(\Factors())
    While Value % 2 = 0
      AddElement(\Factors())
      \Factors() = 2
      Value / 2
    Wend
    Protected Max = Value
    While I <= Max And Value > 1
      While Value % I = 0
        AddElement(\Factors())
        \Factors() = I
        Value / I
      Wend
      I + 2
    Wend
    SortList(\Factors(), #PB_Sort_Ascending)
    FirstElement(\Factors())
    \MinimumFactor=\Factors()
    SignalSemaphore(\StartSeamaphore)
  EndWith ;*IO
EndProcedure

DataSection
  Start_of_data: ; Same numbers as Ada
  Data.q  12757923, 12878611, 12757923, 15808973, 15780709, 197622519
  end_of_data:
EndDataSection

```

[[image:PB_Parallel_Calculations.png]]


## Python

===Python3 - concurrent===
Python 3.2 has a new [http://www.python.org/dev/peps/pep-3148/ concurrent.futures] module that allows for the easy specification of thread-parallel or process-parallel processes. The following is modified from their example and will run, by default, with as many processes as there are available cores on your machine.

<small>Note that there is no need to calculate all prime factors of all <code>NUMBERS</code> when only the prime factors of the number with the lowest overall prime factor is needed.</small>

```python
from concurrent import futures
from math import floor, sqrt

NUMBERS = [
    112272537195293,
    112582718962171,
    112272537095293,
    115280098190773,
    115797840077099,
    1099726829285419]
# NUMBERS = [33, 44, 55, 275]

def lowest_factor(n, _start=3):
    if n % 2 == 0:
        return 2
    search_max = int(floor(sqrt(n))) + 1
    for i in range(_start, search_max, 2):
        if n % i == 0:
            return i
    return n

def prime_factors(n, lowest):
    pf = []
    while n > 1:
        pf.append(lowest)
        n //= lowest
        lowest = lowest_factor(n, max(lowest, 3))
    return pf

def prime_factors_of_number_with_lowest_prime_factor(NUMBERS):
    with futures.ProcessPoolExecutor() as executor:
        low_factor, number = max( (l, f) for l, f in zip(executor.map(lowest_factor, NUMBERS), NUMBERS) )
        all_factors = prime_factors(number, low_factor)
        return number, all_factors


def main():
    print('For these numbers:')
    print('\n  '.join(str(p) for p in NUMBERS))
    number, all_factors = prime_factors_of_number_with_lowest_prime_factor(NUMBERS)
    print('    The one with the largest minimum prime factor is {}:'.format(number))
    print('      All its prime factors in order are: {}'.format(all_factors))

if __name__ == '__main__':
    main()
```


```txt
For these numbers:
  112272537195293
  112582718962171
  112272537095293
  115280098190773
  115797840077099
  1099726829285419
    The one with the largest minimum prime factor is 115797840077099:
      All its prime factors in order are: [544651, 212609249]
```




===Python General - multiprocessing===
<p>This method works for both Python2 and Python3 using the standard library module [https://docs.python.org/3/library/multiprocessing.html multiprocessing]. The result of the following code is the same as the previous example only the different package is used. </p>


```python
import multiprocessing

#
### ====
 #Python3 - concurrent
from math import floor, sqrt

numbers = [
    112272537195293,
    112582718962171,
    112272537095293,
    115280098190773,
    115797840077099,
    1099726829285419]
# numbers = [33, 44, 55, 275]

def lowest_factor(n, _start=3):
    if n % 2 == 0:
        return 2
    search_max = int(floor(sqrt(n))) + 1
    for i in range(_start, search_max, 2):
        if n % i == 0:
            return i
    return n

def prime_factors(n, lowest):
    pf = []
    while n > 1:
        pf.append(lowest)
        n //= lowest
        lowest = lowest_factor(n, max(lowest, 3))
    return pf
#
### ====
 #Python3 - concurrent

def prime_factors_of_number_with_lowest_prime_factor(numbers):
    pool = multiprocessing.Pool(processes=5)
    factors = pool.map(lowest_factor,numbers)

    low_factor,number = max((l,f) for l,f in zip(factors,numbers))
    all_factors = prime_factors(number,low_factor)
    return number,all_factors

if __name__ == '__main__':
    print('For these numbers:')
    print('\n  '.join(str(p) for p in numbers))
    number, all_factors = prime_factors_of_number_with_lowest_prime_factor(numbers)
    print('    The one with the largest minimum prime factor is {}:'.format(number))
    print('      All its prime factors in order are: {}'.format(all_factors))

```



## Racket


```racket

#lang racket
(require math)
(provide main)

(define (smallest-factor n)
  (list (first (first (factorize n))) n))

(define numbers
  '(112272537195293 112582718962171 112272537095293
    115280098190773 115797840077099 1099726829285419))

(define (main)
  ; create as many instances of Racket as
  ; there are numbers:
  (define ps
    (for/list ([_ numbers])
      (place ch
             (place-channel-put
              ch
              (smallest-factor
               (place-channel-get ch))))))
  ; send the numbers to the instances:
  (map place-channel-put ps numbers)
  ; get the results and find the maximum:
  (argmax first (map place-channel-get ps)))

```

Session:

```racket

> (main)
'(544651 115797840077099)

```




## SequenceL

SequenceL compiles to parallel C++ without any input from the user regarding explicit parallelization. The number of threads to be executed on can be specified at runtime, or by default, the runtime detects the maximum number of logical cores and uses that many threads.


```sequencel>import <Utilities/Conversion.sl
;
import <Utilities/Math.sl>;
import <Utilities/Sequence.sl>;

main(args(2)) :=
	let
		inputs := stringToInt(args);
		factored := primeFactorization(inputs);
		minFactors := vectorMin(factored);

		indexOfMax := firstIndexOf(minFactors, vectorMax(minFactors));
	in
		"Number " ++ intToString(inputs[indexOfMax]) ++ " has largest minimal factor:\n"  ++ delimit(intToString(factored[indexOfMax]), ' ');
```


Using the Trial Division version of primeFactorization here: [http://rosettacode.org/wiki/Prime_decomposition#SequenceL]

The primary source of parallelization in the above code is from the line:

```sequencel
factored := primeFactorization(inputs);
```

Since primeFactorization is defined on scalar integers and inputs is a sequence of integers, this call results in a Normalize Transpose. The value of factored  will be the sequence of results of applying primeFactorization to each element of inputs.

Normalize Transpose is one of the semantics which allows SequenceL to automatically generate parallel code.

To test the performance, I ran the program on 100 randomly generated (generated using random.org) numbers between 1 and 1,000,000,000. The system used to test had an i7-4702MQ@2.2GHz

'''Input:'''

```txt
625070029 413238785 815577134 738415913 400125878 967798656 830022841 774153795 114250661 259366941 571026384 522503284 757673286 509866901 6303092 516535622 177377611 520078930 996973832 148686385 33604768 384564659 95268916 659700539 149740384 320999438 822361007 701572051 897604940 2091927 206462079 290027015 307100080 904465970 689995756 203175746 802376955 220768968 433644101 892007533 244830058 36338487 870509730 350043612 282189614 262732002 66723331 908238109 635738243 335338769 461336039 225527523 256718333 277834108 430753136 151142121 602303689 847642943 538451532 683561566 724473614 422235315 921779758 766603317 364366380 60185500 333804616 988528614 933855820 168694202 219881490 703969452 308390898 567869022 719881996 577182004 462330772 770409840 203075270 666478446 351859802 660783778 503851023 789751915 224633442 347265052 782142901 43731988 246754498 736887493 875621732 594506110 854991694 829661614 377470268 984990763 275192380 39848200 892766084 76503760
```

```txt

cmd:> main.exe --sl_threads 1 --sl_timer 625070029 413238785 815577134 738415913 400125878 967798656 830022841 774153795 114250661 259366941 571026384 522503284 757673286 509866901 6303092 516535622 177377611 520078930 996973832 148686385 33604768 384564659 95268916 659700539 149740384 320999438 822361007 701572051 897604940 2091927 206462079 290027015 307100080 904465970 689995756 203175746 802376955 220768968 433644101 892007533 244830058 36338487 870509730 350043612 282189614 262732002 66723331 908238109 635738243 335338769 461336039 225527523 256718333 277834108 430753136 151142121 602303689 847642943 538451532 683561566 724473614 422235315 921779758 766603317 364366380 60185500 333804616 988528614 933855820 168694202 219881490 703969452 308390898 567869022 719881996 577182004 462330772 770409840 203075270 666478446 351859802 660783778 503851023 789751915 224633442 347265052 782142901 43731988 246754498 736887493 875621732 594506110 854991694 829661614 377470268 984990763 275192380 39848200 892766084 76503760
"Number 782142901 has largest minimal factor:
782142901"
Total Time:8.83028

cmd:> main.exe --sl_threads 2 --sl_timer 625070029 413238785 815577134 738415913 400125878 967798656 830022841 774153795 114250661 259366941 571026384 522503284 757673286 509866901 6303092 516535622 177377611 520078930 996973832 148686385 33604768 384564659 95268916 659700539 149740384 320999438 822361007 701572051 897604940 2091927 206462079 290027015 307100080 904465970 689995756 203175746 802376955 220768968 433644101 892007533 244830058 36338487 870509730 350043612 282189614 262732002 66723331 908238109 635738243 335338769 461336039 225527523 256718333 277834108 430753136 151142121 602303689 847642943 538451532 683561566 724473614 422235315 921779758 766603317 364366380 60185500 333804616 988528614 933855820 168694202 219881490 703969452 308390898 567869022 719881996 577182004 462330772 770409840 203075270 666478446 351859802 660783778 503851023 789751915 224633442 347265052 782142901 43731988 246754498 736887493 875621732 594506110 854991694 829661614 377470268 984990763 275192380 39848200 892766084 76503760
"Number 782142901 has largest minimal factor:
782142901"
Total Time:5.67931

cmd:> main.exe --sl_threads 3 --sl_timer 625070029 413238785 815577134 738415913 400125878 967798656 830022841 774153795 114250661 259366941 571026384 522503284 757673286 509866901 6303092 516535622 177377611 520078930 996973832 148686385 33604768 384564659 95268916 659700539 149740384 320999438 822361007 701572051 897604940 2091927 206462079 290027015 307100080 904465970 689995756 203175746 802376955 220768968 433644101 892007533 244830058 36338487 870509730 350043612 282189614 262732002 66723331 908238109 635738243 335338769 461336039 225527523 256718333 277834108 430753136 151142121 602303689 847642943 538451532 683561566 724473614 422235315 921779758 766603317 364366380 60185500 333804616 988528614 933855820 168694202 219881490 703969452 308390898 567869022 719881996 577182004 462330772 770409840 203075270 666478446 351859802 660783778 503851023 789751915 224633442 347265052 782142901 43731988 246754498 736887493 875621732 594506110 854991694 829661614 377470268 984990763 275192380 39848200 892766084 76503760
"Number 782142901 has largest minimal factor:
782142901"
Total Time:3.57379

cmd:> main.exe --sl_threads 4 --sl_timer 625070029 413238785 815577134 738415913 400125878 967798656 830022841 774153795 114250661 259366941 571026384 522503284 757673286 509866901 6303092 516535622 177377611 520078930 996973832 148686385 33604768 384564659 95268916 659700539 149740384 320999438 822361007 701572051 897604940 2091927 206462079 290027015 307100080 904465970 689995756 203175746 802376955 220768968 433644101 892007533 244830058 36338487 870509730 350043612 282189614 262732002 66723331 908238109 635738243 335338769 461336039 225527523 256718333 277834108 430753136 151142121 602303689 847642943 538451532 683561566 724473614 422235315 921779758 766603317 364366380 60185500 333804616 988528614 933855820 168694202 219881490 703969452 308390898 567869022 719881996 577182004 462330772 770409840 203075270 666478446 351859802 660783778 503851023 789751915 224633442 347265052 782142901 43731988 246754498 736887493 875621732 594506110 854991694 829661614 377470268 984990763 275192380 39848200 892766084 76503760
"Number 782142901 has largest minimal factor:
782142901"
Total Time:2.86046

cmd:> main.exe --sl_threads 0 --sl_timer 625070029 413238785 815577134 738415913 400125878 967798656 830022841 774153795 114250661 259366941 571026384 522503284 757673286 509866901 6303092 516535622 177377611 520078930 996973832 148686385 33604768 384564659 95268916 659700539 149740384 320999438 822361007 701572051 897604940 2091927 206462079 290027015 307100080 904465970 689995756 203175746 802376955 220768968 433644101 892007533 244830058 36338487 870509730 350043612 282189614 262732002 66723331 908238109 635738243 335338769 461336039 225527523 256718333 277834108 430753136 151142121 602303689 847642943 538451532 683561566 724473614 422235315 921779758 766603317 364366380 60185500 333804616 988528614 933855820 168694202 219881490 703969452 308390898 567869022 719881996 577182004 462330772 770409840 203075270 666478446 351859802 660783778 503851023 789751915 224633442 347265052 782142901 43731988 246754498 736887493 875621732 594506110 854991694 829661614 377470268 984990763 275192380 39848200 892766084 76503760
"Number 782142901 has largest minimal factor:
782142901"
Total Time:3.01593

```

[http://i.imgur.com/UIO8X8U.png Performance Plot]

The i7 has 4 physical cores with hyperthreading.
You can see that nearly linear speedup is gained, automatically, while only using the physical cores. Once the hyperthreaded cores are used the performance suffers slightly.


## Sidef

The code uses the ''prime_factors()'' function defined in the "Prime decomposition" task.

```ruby
var nums = [1275792312878611, 12345678915808973,
            1578070919762253, 14700694496703910,];

var factors = nums.map {|n| prime_factors.ffork(n) }.map { .wait }
say ((nums ~Z factors)->max_by {|m| m[1][0] })
```

```txt

$ time sidef parallel.sf
[1275792312878611, [11, 7369, 15739058129]]
sidef parallel.sf  24.46s user 0.02s system 158% cpu 15.436 total

```



## Tcl

With Tcl, it is necessary to explicitly perform computations in other threads because each thread is strongly isolated from the others (except for inter-thread messaging). However, it is entirely practical to wrap up the communications so that only a small part of the code needs to know very much about it, and in fact most of the complexity is managed by a thread pool; each value to process becomes a work item to be handled. It is easier to transfer the results by direct messaging instead of collecting the thread pool results, since we can leverage Tcl's <code>vwait</code> command nicely.
```tcl
package require Tcl 8.6
package require Thread

# Pooled computation engine; runs event loop internally
namespace eval pooled {
    variable poolSize 3; # Needs to be tuned to system size

    proc computation {computationDefinition entryPoint values} {
	variable result
	variable poolSize
	# Add communication shim
	append computationDefinition [subst -nocommands {
	    proc poolcompute {value target} {
		set outcome [$entryPoint \$value]
		set msg [list set ::pooled::result(\$value) \$outcome]
		thread::send -async \$target \$msg
	    }
	}]

	# Set up the pool
	set pool [tpool::create -initcmd $computationDefinition \
		      -maxworkers $poolSize]

	# Prepare to receive results
	unset -nocomplain result
	array set result {}

	# Dispatch the computations
	foreach value $values {
	    tpool::post $pool [list poolcompute $value [thread::id]]
	}

	# Wait for results
	while {[array size result] < [llength $values]} {vwait pooled::result}

	# Dispose of the pool
	tpool::release $pool

	# Return the results
	return [array get result]
    }
}
```

This is the definition of the prime factorization engine (a somewhat stripped-down version of the [[Prime decomposition#Tcl|Tcl Prime decomposition solution]]:

```tcl
# Code for computing the prime factors of a number
set computationCode {
    namespace eval prime {
	variable primes [list 2 3 5 7 11]
	proc restart {} {
	    variable index -1
	    variable primes
	    variable current [lindex $primes end]
	}

	proc get_next_prime {} {
	    variable primes
	    variable index
	    if {$index < [llength $primes]-1} {
		return [lindex $primes [incr index]]
	    }
	    variable current
	    while 1 {
		incr current 2
		set p 1
		foreach prime $primes {
		    if {$current % $prime} {} else {
			set p 0
			break
		    }
		}
		if {$p} {
		    return [lindex [lappend primes $current] [incr index]]
		}
	    }
	}

	proc factors {num} {
	    restart
	    set factors [dict create]
	    for {set i [get_next_prime]} {$i <= $num} {} {
		if {$num % $i == 0} {
		    dict incr factors $i
		    set num [expr {$num / $i}]
		    continue
		} elseif {$i*$i > $num} {
		    dict incr factors $num
		    break
		} else {
		    set i [get_next_prime]
		}
	    }
	    return $factors
	}
    }
}

# The values to be factored
set values {
    188573867500151328137405845301
    3326500147448018653351160281
    979950537738920439376739947
    2297143294659738998811251
    136725986940237175592672413
    3922278474227311428906119
    839038954347805828784081
    42834604813424961061749793
    2651919914968647665159621
    967022047408233232418982157
    2532817738450130259664889
    122811709478644363796375689
}
```

Putting everything together:

```tcl
# Do the computation, getting back a dictionary that maps
# values to its results (itself an ordered dictionary)
set results [pooled::computation $computationCode prime::factors $values]

# Find the maximum minimum factor with sorting magic
set best [lindex [lsort -integer -stride 2 -index {1 0} $results] end-1]

# Print in human-readable form
proc renderFactors {factorDict} {
    dict for {factor times} $factorDict {
	lappend v {*}[lrepeat $times $factor]
    }
    return [join $v "*"]
}
puts "$best = [renderFactors [dict get $results $best]]"
```



## zkl

Using 64 bit ints and "green"/co-op threads. Using native threads is bad in this case because spawning a bunch of threads (ie way more than there are cpus) really clogs the system and actually slows things down. Strands (zkl speak for green threads) queues up computations for a limited pool of threads, and, as each thread finishes a job, it reads from the queue for the next computation to perform. Strands, as they start up, kick back a future, which can be forced (ie waited on until a result is available) by evaluating it, in this case, by doing a future.noop().

```zkl
fcn factorize(x,y,z,etc){
   xyzs:=vm.arglist;
   fs:=xyzs.apply(factors.strand) // queue up factorizing for x,y,...
       .apply("noop")		  // wait for all threads to finish factoring
       .apply(fcn{ (0).min(vm.arglist) }); // find minimum factor for x,y...
   [0..].zip(fs).filter(fcn([(n,x)],M){ x==M }.fp1((0).max(fs))) // find max of mins
   .apply('wrap([(n,_)]){ xyzs[n] })  // and pluck src from arglist
}
```


```zkl
factorize(12757923,12878611,12757923,15808973,15780709,197622519).println();
    // do a bunch so I can watch the system monitor
factorize( (0).pump(5000,List,fcn{(1000).random() }).xplode() ).println();
```

```txt

L(12878611)
L(4177950757)

```

[[Prime decomposition#zkl]]

```zkl
fcn factors(n){  // Return a list of factors of n
   acc:=fcn(n,k,acc,maxD){  // k is 2,3,5,7,9,... not optimum
      if(n==1 or k>maxD) acc.close();
      else{
	 q,r:=n.divr(k);   // divr-->(quotient,remainder)
	 if(r==0) return(self.fcn(q,k,acc.write(k),q.toFloat().sqrt()));
	 return(self.fcn(n,k+1+k.isOdd,acc,maxD))
      }
   }(n,2,Sink(List),n.toFloat().sqrt());
   m:=acc.reduce('*,1);      // mulitply factors
   if(n!=m) acc.append(n/m); // opps, missed last factor
   else acc;
}
```



