+++
title = "Verify distribution uniformity/Naive"
description = ""
date = 2019-09-15T17:33:14Z
aliases = []
[extra]
id = 4680
[taxonomies]
categories = []
tags = []
+++

{{task|Probability and statistics}}

<small>This task is an adjunct to [[Seven-sided dice from five-sided dice]].</small>


;Task:
Create a function to check that the random integers returned from a small-integer generator function have uniform distribution.


The function should take as arguments:
* The function (or object) producing random integers.
* The number of times to call the integer generator.
* A 'delta' value of some sort that indicates how close to a flat distribution is close enough.


The function should produce:
* Some indication of the distribution achieved.
* An 'error' if the distribution is not flat enough.


Show the distribution checker working when the produced distribution is flat enough and when it is not. (Use a generator from [[Seven-sided dice from five-sided dice]]).


See also:
*[[Verify distribution uniformity/Chi-squared test]]





## Ada



```Ada
with Ada.Numerics.Discrete_Random, Ada.Text_IO;

procedure Naive_Random is

   type M_1000 is mod 1000;
   package Rand is new Ada.Numerics.Discrete_Random(M_1000);
   Gen: Rand.Generator;

   procedure Perform(Modulus: Positive; Expected, Margin: Natural;
                    Passed: out boolean) is
      Low:  Natural  := (100-Margin) * Expected/100;
      High: Natural  := (100+Margin) * Expected/100;
      Buckets: array(0 .. Modulus-1) of Natural := (others => 0);
      Index: Natural;
   begin
      for I in 1 .. Expected * Modulus loop
         Index := Integer(Rand.Random(Gen)) mod Modulus;
         Buckets(Index) := Buckets(Index) + 1;
      end loop;
      Passed := True;
      for I in Buckets'Range loop
         Ada.Text_IO.Put("Bucket" & Integer'Image(I+1) & ":" &
                           Integer'Image(Buckets(I)));
         if Buckets(I) < Low or else Buckets(I) > High then
            Ada.Text_IO.Put_Line(" (failed)");
            Passed := False;
         else
            Ada.Text_IO.Put_Line(" (passed)");
         end if;
      end loop;
      Ada.Text_IO.New_Line;
   end Perform;

   Number_Of_Buckets: Positive := Natural'Value(Ada.Text_IO.Get_Line);
   Expected_Per_Bucket: Natural := Natural'Value(Ada.Text_IO.Get_Line);
   Margin_In_Percent: Natural := Natural'Value(Ada.Text_IO.Get_Line);
   OK: Boolean;

begin
   Ada.Text_IO.Put_Line(  "Buckets:"    & Integer'Image(Number_Of_Buckets) &
                          ", Expected:" & Integer'Image(Expected_Per_Bucket) &
                          ", Margin:"   & Integer'Image(Margin_In_Percent));
   Rand.Reset(Gen);

   Perform(Modulus  => Number_Of_Buckets,
           Expected => Expected_Per_Bucket,
           Margin   => Margin_In_Percent,
           Passed   => OK);

   Ada.Text_IO.Put_Line("Test Passed? (" & Boolean'Image(OK) & ")");
end Naive_Random;
```


Sample run 1 (all buckets good):
```txt
7
1000
3
Buckets: 7, Expected: 1000, Margin: 3
Bucket 1: 1006 (passed)
Bucket 2: 1030 (passed)
Bucket 3: 997 (passed)
Bucket 4: 985 (passed)
Bucket 5: 976 (passed)
Bucket 6: 1024 (passed)
Bucket 7: 982 (passed)

Test Passed? (TRUE)
```


Sample run 2 (some buckets too large / to small):
```txt
7
1000
3
Buckets: 7, Expected: 1000, Margin: 3
Bucket 1: 1034 (failed)
Bucket 2: 985 (passed)
Bucket 3: 1025 (passed)
Bucket 4: 933 (failed)
Bucket 5: 1000 (passed)
Bucket 6: 1016 (passed)
Bucket 7: 1007 (passed)

Test Passed? (FALSE)
```



## AutoHotkey


```AutoHotkey
MsgBox, % DistCheck("dice7",10000,3)

DistCheck(function, repetitions, delta)
{  Loop, % 7 ; initialize array
   {  bucket%A_Index% := 0
   }

   Loop, % repetitions ; populate buckets
   {  v := %function%()
      bucket%v% += 1
   }

   lbnd := round((repetitions/7)*(100-delta)/100)
   ubnd := round((repetitions/7)*(100+delta)/100)
   text := "Distribution check:`n`nTotal elements = " repetitions
         . "`n`nMargin = " delta "% --> Lbound = " lbnd ", Ubound = " ubnd "`n"
   Loop, % 7
   {  text := text "`nBucket " A_Index " contains " bucket%A_Index% " elements."
      If bucket%A_Index% not between %lbnd% and %ubnd%
         text := text " Skewed."
   }
   Return, text
}
```


```txt
Distribution check:

Total elements = 10000

Margin = 3% --> Lbound = 1386, Ubound = 1471

Bucket 1 contains 1450 elements.
Bucket 2 contains 1374 elements. Skewed.
Bucket 3 contains 1412 elements.
Bucket 4 contains 1465 elements.
Bucket 5 contains 1370 elements. Skewed.
Bucket 6 contains 1485 elements. Skewed.
Bucket 7 contains 1444 elements.
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      MAXRND = 7
      FOR r% = 2 TO 5
        check% = FNdistcheck(FNdice5, 10^r%, 0.05)
        PRINT "Over "; 10^r% " runs dice5 ";
        IF check% THEN
          PRINT "failed distribution check with "; check% " bin(s) out of range"
        ELSE
          PRINT "passed distribution check"
        ENDIF
      NEXT
      END

      DEF FNdistcheck(RETURN func%, repet%, delta)
      LOCAL i%, m%, r%, s%, bins%()
      DIM bins%(MAXRND)
      FOR i% = 1 TO repet%
        r% = FN(^func%)
        bins%(r%) += 1
        IF r%>m% m% = r%
      NEXT
      FOR i% = 1 TO m%
        IF bins%(i%)/(repet%/m%) > 1+delta s% += 1
        IF bins%(i%)/(repet%/m%) < 1-delta s% += 1
      NEXT
      = s%

      DEF FNdice5 = RND(5)
```

Output:

```txt

Over 100 runs dice5 failed distribution check with 3 bin(s) out of range
Over 1000 runs dice5 failed distribution check with 1 bin(s) out of range
Over 10000 runs dice5 passed distribution check
Over 100000 runs dice5 passed distribution check

```



## C


```cpp
#include <iostream>
#include <stdio.h>
#include <math.h>

inline int rand5()
{
	int r, rand_max = RAND_MAX - (RAND_MAX % 5);
	while ((r = rand()) >= rand_max);
	return r / (rand_max / 5) + 1;
}

inline int rand5_7()
{
	int r;
	while ((r = rand5() * 5 + rand5()) >= 27);
	return r / 3 - 1;
}

/* assumes gen() returns a value from 1 to n */
int check(int (*gen)(), int n, int cnt, double delta) /* delta is relative */
{
	int i = cnt, *bins = calloc(sizeof(int), n);
	double ratio;
	while (i--) bins[gen() - 1]++;
	for (i = 0; i < n; i++) {
		ratio = bins[i] * n / (double)cnt - 1;
		if (ratio > -delta && ratio < delta) continue;

		printf("bin %d out of range: %d (%g%% vs %g%%), ",
			i + 1, bins[i], ratio * 100, delta * 100);
		break;
	}
	free(bins);
	return i == n;
}

int main()
{
	int cnt = 1;
	while ((cnt *= 10) <= 1000000) {
		printf("Count = %d: ", cnt);
		printf(check(rand5_7, 7, cnt, 0.03) ? "flat\n" : "NOT flat\n");
	}

	return 0;
}
```
output
```txt

Count = 10: bin 1 out of range: 1 (-30% vs 3%), NOT flat
Count = 100: bin 1 out of range: 11 (-23% vs 3%), NOT flat
Count = 1000: bin 1 out of range: 150 (5% vs 3%), NOT flat
Count = 10000: bin 3 out of range: 1477 (3.39% vs 3%), NOT flat
Count = 100000: flat
Count = 1000000: flat

```



## C++


```cpp
#include <map>
#include <iostream>
#include <cmath>

template<typename F>
 bool test_distribution(F f, int calls, double delta)
{
  typedef std::map<int, int> distmap;
  distmap dist;

  for (int i = 0; i < calls; ++i)
    ++dist[f()];

  double mean = 1.0/dist.size();

  bool good = true;

  for (distmap::iterator i = dist.begin(); i != dist.end(); ++i)
  {
    if (std::abs((1.0 * i->second)/calls - mean) > delta)
    {
      std::cout << "Relative frequency " << i->second/(1.0*calls)
                << " of result " << i->first
                << " deviates by more than " << delta
                << " from the expected value " << mean << "\n";
      good = false;
    }
  }

  return good;
}
```



## Clojure

The code could be shortened if the verify function did the output itself, but the "Clojure way" is to have functions, whenever possible, avoid side effects (like printing) and just return a result. Then the "application-level" code uses doseq and println to display the output to the user. The built-in (rand-int) function is used for all three runs of the verify function, but first with small N to simulate experimental error in a small sample size, then with larger N to show it working properly on large N.

```clojure
(defn verify [rand n & [delta]]
  (let [rands (frequencies (repeatedly n rand))
        avg (/ (reduce + (map val rands)) (count rands))
        max-delta (* avg (or delta 1/10))
        acceptable? #(<= (- avg max-delta) % (+ avg max-delta))]
    (for [[num count] (sort rands)]
      [num count (acceptable? count)])))

(doseq [n [100 1000 10000]
        [num count okay?] (verify #(rand-int 7) n)]
  (println "Saw" num count "times:"
           (if okay? "that's" "   not") "acceptable"))
```



```txt
Saw 1 13 times: that's acceptable
Saw 2 15 times: that's acceptable
Saw 3 11 times:    not acceptable
Saw 4 10 times:    not acceptable
Saw 5 19 times:    not acceptable
Saw 6 17 times:    not acceptable
Saw 0 121 times:    not acceptable
Saw 1 128 times:    not acceptable
Saw 2 161 times:    not acceptable
Saw 3 146 times: that's acceptable
Saw 4 134 times: that's acceptable
Saw 5 170 times:    not acceptable
Saw 6 140 times: that's acceptable
Saw 0 1480 times: that's acceptable
Saw 1 1372 times: that's acceptable
Saw 2 1411 times: that's acceptable
Saw 3 1442 times: that's acceptable
Saw 4 1395 times: that's acceptable
Saw 5 1432 times: that's acceptable
Saw 6 1468 times: that's acceptable

```



## Common Lisp

{{trans|OCaml}}

```lisp
(defun check-distribution (function n &optional (delta 1.0))
  (let ((bins (make-hash-table)))
    (loop repeat n do (incf (gethash (funcall function) bins 0)))
    (loop with target = (/ n (hash-table-count bins))
          for key being each hash-key of bins using (hash-value value)
          when (> (abs (- value target)) (* 0.01 delta n))
          do (format t "~&Distribution potentially skewed for ~w:~
                         expected around ~w got ~w." key target value)
          finally (return bins))))
```



```txt
> (check-distribution 'd7 1000)
Distribution potentially skewed for 1: expected around 1000/7 got 153.
Distribution potentially skewed for 2: expected around 1000/7 got 119.
Distribution potentially skewed for 3: expected around 1000/7 got 125.
Distribution potentially skewed for 7: expected around 1000/7 got 156.
T
#<EQL Hash Table{7} 200B5A53>

> (check-distribution 'd7 10000)
NIL
#<EQL Hash Table{7} 200CB5BB>
```



## D


```d
import std.stdio, std.string, std.math, std.algorithm, std.traits;

/**
Bin the answers to fn() and check bin counts are within
+/- delta % of repeats/bincount.
*/
void distCheck(TF)(in TF func, in int nRepeats, in double delta) /*@safe*/
if (isCallable!TF) {
    int[int] counts;
    foreach (immutable i; 0 .. nRepeats)
        counts[func()]++;
    immutable double target = nRepeats / double(counts.length);
    immutable int deltaCount = cast(int)(delta / 100.0 * target);

    foreach (immutable k, const count; counts)
        if (abs(target - count) >= deltaCount)
            throw new Exception(format(
                "distribution potentially skewed for '%s': '%d'\n",
                k, count));

    foreach (immutable k; counts.keys.sort())
        writeln(k, " ", counts[k]);
    writeln;
}

version (verify_distribution_uniformity_naive_main) {
    void main() {
        import std.random;
        distCheck(() => uniform(1, 6), 1_000_000, 1);
    }
}
```

If compiled with -version=verify_distribution_uniformity_naive_main:
{{out}}

```txt
1 199389
2 2
4 200016
5 200424
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule VerifyDistribution do
  def naive( generator, times, delta_percent ) do
    dict = Enum.reduce( List.duplicate(generator, times), Map.new, &update_counter/2 )
    values = Map.values(dict)
    average = Enum.sum( values ) / map_size( dict )
    delta = average * (delta_percent / 100)
    fun = fn {_key, value} -> abs(value - average) > delta end
    too_large_dict = Enum.filter( dict, fun )
    return( length(too_large_dict), too_large_dict, average, delta_percent )
  end

  def return( 0, _too_large_dict, _average, _delta ), do: :ok
  def return( _n, too_large_dict, average, delta ) do
    {:error, {too_large_dict, :failed_expected_average, average, 'with_delta_%', delta}}
  end

  def update_counter( fun, dict ), do: Map.update( dict, fun.(), 1, &(&1+1) )
end

fun = fn -> Dice.dice7 end
IO.inspect VerifyDistribution.naive( fun, 100000, 3 )
IO.inspect VerifyDistribution.naive( fun, 100, 3 )
```


{{out}}

```txt

:ok
{:error,
 {[{1, 16}, {2, 10}, {4, 15}, {5, 8}, {6, 20}, {7, 17}],
  :failed_expected_average, 14.285714285714286, 'with_delta_%', 3}}

```



## Erlang


```Erlang

-module( verify_distribution_uniformity ).

-export( [naive/3] ).

naive( Generator, Times, Delta_percent ) ->
    Dict = lists:foldl( fun update_counter/2, dict:new(), lists:duplicate(Times, Generator) ),
    Values = [dict:fetch(X, Dict) || X <- dict:fetch_keys(Dict)],
    Average = lists:sum( Values ) / dict:size( Dict ),
    Delta = Average * (Delta_percent / 100),
    Fun = fun(_Key, Value) -> erlang:abs(Value - Average) > Delta end,
    Too_large_dict = dict:filter( Fun, Dict ),
    return( dict:size(Too_large_dict), Too_large_dict, Average, Delta_percent ).



return( 0, _Too_large_dict, _Average, _Delta ) -> ok;
return( _N, Too_large_dict, Average, Delta ) ->
	{error, {dict:to_list(Too_large_dict), failed_expected_average, Average, 'with_delta_%', Delta}}.

update_counter( Fun, Dict ) -> dict:update_counter( Fun(), 1, Dict ).

```


{{out}}
Calling dice:dice7/0 few times shows skewed distribution.

```txt

61> Fun = fun dice:dice7/0.
62> verify_distribution_uniformity:naive( Fun, 100000, 3).
ok
63> verify_distribution_uniformity:naive( Fun, 100, 3).
{error,{[{3,15},{6,15},{5,13},{1,20},{4,11},{7,12}],
        failed_expected_average,14.285714285714286,'with_delta_%',
        3}}

```



## Euler Math Toolbox


Following the task verbatim.

<lang>
>function checkrandom (frand$, n:index, delta:positive real) ...
$  v=zeros(1,n);
$  loop 1 to n; v{#}=frand$(); end;
$  K=max(v);
$  fr=getfrequencies(v,1:K);
$  return max(fr/n-1/K)<delta/sqrt(n);
$  endfunction
>function dice () := intrandom(1,1,6);
>checkrandom("dice",1000000,1)
 1
>wd = 0|((1:6)+[-0.01,0.01,0,0,0,0])/6
 [ 0  0.165  0.335  0.5  0.666666666667  0.833333333333  1 ]
>function wrongdice () := find(wd,random())
>checkrandom("wrongdice",1000000,1)
 0

```


Checking the dice7 from dice5 generator.

<lang>
>function dice5 () := intrandom(1,1,5);
>function dice7 () ...
$  repeat
$    k=(dice5()-1)*5+dice5();
$    if k<=21 then return ceil(k/3); endif;
$  end;
$  endfunction
>checkrandom("dice7",1000000,1)
 1

```


Faster implementation with the matrix language.

<lang>
>function dice5(n) := intrandom(1,n,5)-1;
>function dice7(n) ...
$  v=dice5(2*n)*5+dice5(2*n);
$  return v[nonzeros(v<=21)][1:n];
$  endfunction
>test=dice7(1000000);
>function checkrandom (v, delta=1) ...
$  K=max(v); n=cols(v);
$  fr=getfrequencies(v,1:K);
$  return max(fr/n-1/K)<delta/sqrt(n);
$  endfunction
>checkrandom(test)
 1
>wd = 0|((1:6)+[-0.01,0.01,0,0,0,0])/6
 [ 0  0.165  0.335  0.5  0.666666666667  0.833333333333  1 ]
>function wrongdice (n) := find(wd,random(1,n))
>checkrandom(wrongdice(1000000))
 0

```



## Factor


```factor
USING: kernel random sequences assocs locals sorting prettyprint
  math math.functions math.statistics math.vectors math.ranges ;
IN: rosetta-code.dice7

! Output a random integer 1..5.
: dice5 ( -- x )
   5 [1,b] random
;

! Output a random integer 1..7 using dice5 as randomness source.
: dice7 ( -- x )
   0 [ dup 21 < ] [ drop dice5 5 * dice5 + 6 - ] do until
   7 rem 1 +
;

! Roll the die by calling the quotation the given number of times and return
! an array with roll results.
! Sample call: 1000 [ dice7 ] roll
: roll ( times quot: ( -- x ) -- array )
   [ call( --  x ) ] curry replicate
;

! Input array contains outcomes of a number of die throws. Each die result is
! an integer in the range 1..X. Calculate and return the number of each
! of the results in the array so that in the first position of the result
! there is the number of ones in the input array, in the second position
! of the result there is the number of twos in the input array, etc.
: count-dice-outcomes ( X array -- array )
    histogram
    swap [1,b] [ over [ 0 or ] change-at ] each
    sort-keys values
;

! Verify distribution uniformity/Naive. Delta is the acceptable deviation
! from the ideal number of items in each bucket, expressed as a fraction of
! the total count. Sides is the number of die sides. Die-func is a word that
! produces a random number on stack in the range [1..sides], times is the
! number of times to call it.
! Sample call: 0.02 7 [ dice7 ] 100000 verify
:: verify ( delta sides die-func: ( -- random ) times -- )
   sides
   times die-func roll
   count-dice-outcomes
   dup .
   times sides / :> ideal-count
   ideal-count v-n vabs
   times v/n
   delta [ < ] curry all?
   [ "Random enough" . ] [ "Not random enough" . ] if
;


! Call verify with 1, 10, 100, ... 1000000 rolls of 7-sided die.
: verify-all ( -- )
   { 1 10 100 1000 10000 100000 1000000 }
   [| times | 0.02 7 [ dice7 ] times verify ] each
;
```


Output:

```txt
USE: rosetta-code.dice7 verify-all
{ 0 0 0 1 0 0 0 }
"Not random enough"
{ 0 2 3 1 1 1 2 }
"Not random enough"
{ 17 12 15 11 13 13 19 }
"Not random enough"
{ 140 130 141 148 143 155 143 }
"Random enough"
{ 1457 1373 1427 1433 1443 1382 1485 }
"Random enough"
{ 14225 14320 14216 14326 14415 14084 14414 }
"Random enough"
{ 142599 141910 142524 143029 143353 142696 143889 }
"Random enough"
```



## Forth

requires Forth200x locals

```forth
: .bounds ( u1 u2 -- )  ." lower bound = " .  ."  upper bound = " 1- .  cr ;
: init-bins ( n -- addr )
   cells dup allocate throw  tuck swap erase ;
: expected ( u1 cnt -- u2 )  over 2/ + swap / ;
: calc-limits ( n cnt pct -- low high )
   >r  expected  r>  over 100 */ 2dup  + 1+ >r  -  r> ;
: make-histogram ( bins xt cnt -- )
   0 ?do 2dup  execute 1- cells  +  1 swap +!  loop  2drop ;
: valid-bin? ( addr n low high -- f )
   2>r  cells + @ dup .  2r> within ;

: check-distribution {: xt cnt n pct -- f :}
\ assumes xt generates numbers from 1 to n
   n init-bins  {: bins :}
   n cnt pct calc-limits  {: low high :}
   high low .bounds
   bins xt cnt make-histogram
   true  \ result flag
   n 0 ?do
      i 1+ . ." : "  bins i low high valid-bin?
      dup 0= if ." not " then ." ok" cr
      and
   loop
   bins free throw ;
```

{{output}}

```txt
cr ' d7 1000000 7 1 check-distribution .
lower bound = 141429  upper bound = 144285
1 : 143241 ok
2 : 142397 ok
3 : 143522 ok
4 : 142909 ok
5 : 142001 ok
6 : 142844 ok
7 : 143086 ok
-1

cr ' d7 10000 7 1 check-distribution .
lower bound = 1415  upper bound = 1443
1 : 1431 ok
2 : 1426 ok
3 : 1413 not ok
4 : 1427 ok
5 : 1437 ok
6 : 1450 not ok
7 : 1416 ok
0
```


## Fortran

{{works with|Fortran|95 and later}}

```fortran
subroutine distcheck(randgen, n, delta)

  interface
    function randgen
      integer :: randgen
    end function randgen
  end interface

  real, intent(in) :: delta
  integer, intent(in) :: n
  integer :: i, mval, lolim, hilim
  integer, allocatable :: buckets(:)
  integer, allocatable :: rnums(:)
  logical :: skewed = .false.

  allocate(rnums(n))

  do i = 1, n
    rnums(i) = randgen()
  end do

  mval = maxval(rnums)
  allocate(buckets(mval))
  buckets = 0

  do i = 1, n
    buckets(rnums(i)) = buckets(rnums(i)) + 1
  end do

  lolim = n/mval - n/mval*delta
  hilim = n/mval + n/mval*delta

  do i = 1, mval
    if(buckets(i) < lolim .or. buckets(i) > hilim) then
      write(*,"(a,i0,a,i0,a,i0)") "Distribution potentially skewed for bucket ", i, "   Expected: ", &
                                   n/mval, "   Actual: ", buckets(i)
      skewed = .true.
    end if
  end do

  if (.not. skewed) write(*,"(a)") "Distribution uniform"

  deallocate(rnums)
  deallocate(buckets)

end subroutine
```



## Go


```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// "given"
func dice5() int {
    return rand.Intn(5) + 1
}

// function specified by task "Seven-sided dice from five-sided dice"
func dice7() (i int) {
    for {
        i = 5*dice5() + dice5()
        if i < 27 {
            break
        }
    }
    return (i / 3) - 1
}

// function specified by task "Verify distribution uniformity/Naive"
//
// Parameter "f" is expected to return a random integer in the range 1..n.
// (Values out of range will cause an unceremonious crash.)
// "Max" is returned as an "indication of distribution achieved."
// It is the maximum delta observed from the count representing a perfectly
// uniform distribution.
// Also returned is a boolean, true if "max" is less than threshold
// parameter "delta."
func distCheck(f func() int, n int,
    repeats int, delta float64) (max float64, flatEnough bool) {
    count := make([]int, n)
    for i := 0; i < repeats; i++ {
        count[f()-1]++
    }
    expected := float64(repeats) / float64(n)
    for _, c := range count {
        max = math.Max(max, math.Abs(float64(c)-expected))
    }
    return max, max < delta
}

// Driver, produces output satisfying both tasks.
func main() {
    rand.Seed(time.Now().UnixNano())
    const calls = 1000000
    max, flatEnough := distCheck(dice7, 7, calls, 500)
    fmt.Println("Max delta:", max, "Flat enough:", flatEnough)
    max, flatEnough = distCheck(dice7, 7, calls, 500)
    fmt.Println("Max delta:", max, "Flat enough:", flatEnough)
}
```

Output:

```txt

Max delta: 356.1428571428696 Flat enough: true
Max delta: 787.8571428571304 Flat enough: false

```



## Haskell


```haskell
import System.Random
import Data.List
import Control.Monad
import Control.Arrow

distribCheck :: IO Int -> Int -> Int -> IO [(Int,(Int,Bool))]
distribCheck f n d = do
  nrs <- replicateM n f
  let group  = takeWhile (not.null) $ unfoldr (Just. (partition =<< (==). head)) nrs
      avg    = (fromIntegral n) / fromIntegral (length group)
      ul     = round $ (100 + fromIntegral d)/100 * avg
      ll     = round $ (100 - fromIntegral d)/100 * avg
  return $ map (head &&& (id &&& liftM2 (&&) (>ll)(<ul)).length) group
```

Example:

```haskell
*Main> mapM_ print .sort =<< distribCheck (randomRIO(1,6)) 100000 3
(1,(16911,True))
(2,(16599,True))
(3,(16670,True))
(4,(16624,True))
(5,(16526,True))
(6,(16670,True))
```



## Hy



```lisp
(import [collections [Counter]])
(import [random [randint]])

(defn uniform? [f repeats delta]
; Call 'f' 'repeats' times, then check if the proportion of each
; value seen is within 'delta' of the reciprocal of the count
; of distinct values.
  (setv bins (Counter (list-comp (f) [i (range repeats)])))
  (setv target (/ 1 (len bins)))
  (all (list-comp
    (<= (- target delta) (/ n repeats) (+ target delta))
    [n (.values bins)])))
```


Examples of use:


```lisp
(for [f [
    (fn [] (randint 1 10))
    (fn [] (if (randint 0 1) (randint 1 9) (randint 1 10)))]]
  (print (uniform? f 5000 .02)))
```


=={{header|Icon}} and {{header|Unicon}}==
This example assumes the random number generator is passed to <code>verify_uniform</code> as a co-expression.  The co-expression <code>rnd</code> is prompted for its next value using <code>@rnd</code>.  The co-expression is created using <code>create (|?10)</code> where the vertical bar means generate an infinite sequence of what is to its right (in this case, <code>?10</code> generates a random integer in the range [1,10]).

```Icon
# rnd  : a co-expression, which will generate the random numbers
# n    : the number of numbers to test
# delta: tolerance in non-uniformity
# This procedure fails if after the sampling the difference
# in uniformity exceeds delta, a proportion of n / number-of-alternatives
procedure verify_uniform (rnd, n, delta)
  # generate a table counting the outcome of the generator
  results := table (0)
  every (1 to n) do results[@rnd] +:= 1
  # retrieve the statistics
  smallest := n
  largest := 0
  every num := key(results) do { # record result and limits
    write (num || " " || results[num])
    if results[num] < smallest then smallest := results[num]
    if results[num] > largest  then largest  := results[num]
  }
  # decide if difference is within bounds defined by delta
  return largest-smallest < delta * n / *results
end

procedure main ()
  gen_1 := create (|?10) # uniform integers, 1 to 10
  if verify_uniform (gen_1, 1000000, 0.01)
    then write ("uniform")
    else write ("skewed")
  gen_2 := create (|(if ?2 = 1 then 6 else ?10)) # skewed integers, 1 to 10
  if verify_uniform (gen_2, 1000000, 0.01)
    then write ("uniform")
    else write ("skewed")
end
```

Output:

```txt

5 99988
2 99998
10 99894
7 99948
4 100271
1 99917
9 99846
6 99943
3 99824
8 100371
uniform
5 49940
2 50324
10 50243
7 49982
4 50295
1 50162
9 49800
6 549190
3 50137
8 49927
skewed

```



## J

This solution defines the checker as an adverb. Adverbs combine with the verb immediately to their left to create a new verb. So for a verb <code>generateDistribution</code> and an adverb <code>checkUniform</code>, the new verb might be thought of as <code>checkGeneratedDistributionisUniform</code>.

The ''delta'' is given as an optional left argument (<code>x</code>), defaulting to 5%. The right argument (<code>y</code>) is any valid argument to the distribution generating verb.

```j
checkUniform=: adverb define
  0.05 u checkUniform y
  :
  n=. */y
  delta=. x
  sample=. u n              NB. the "u" refers to the verb to left of adverb
  freqtable=. /:~ (~. sample) ,. #/.~ sample
  expected=. n % # freqtable
  errmsg=. 'Distribution is potentially skewed'
  errmsg assert (delta * expected) > | expected - {:"1 freqtable
  freqtable
)
```

It is possible to use tacit expressions within an explicit definition enabling a more functional and concise style:

```j
checkUniformT=: adverb define
  0.05 u checkUniformT y
  :
  freqtable=. /:~ (~. ,. #/.~) u n=. */y
  errmsg=. 'Distribution is potentially skewed'
  errmsg assert ((n % #) (x&*@[ > |@:-) {:"1) freqtable
  freqtable
)
```

Show usage using <code>rollD7t</code> given in [[Seven-dice from Five-dice#J|Seven-dice from Five-dice]]:

```j
   0.05 rollD7t checkUniform 1e5
1 14082
2 14337
3 14242
4 14470
5 14067
6 14428
7 14374
   0.05 rollD7t checkUniform 1e2
|Distribution is potentially skewed: assert
|   errmsg     assert(delta*expected)>|expected-{:"1 freqtable
```




## Java

{{trans|D}}
{{works with|Java|8}}

```java
import static java.lang.Math.abs;
import java.util.*;
import java.util.function.IntSupplier;

public class Test {

    static void distCheck(IntSupplier f, int nRepeats, double delta) {
        Map<Integer, Integer> counts = new HashMap<>();

        for (int i = 0; i < nRepeats; i++)
            counts.compute(f.getAsInt(), (k, v) -> v == null ? 1 : v + 1);

        double target = nRepeats / (double) counts.size();
        int deltaCount = (int) (delta / 100.0 * target);

        counts.forEach((k, v) -> {
            if (abs(target - v) >= deltaCount)
                System.out.printf("distribution potentially skewed "
                        + "for '%s': '%d'%n", k, v);
        });

        counts.keySet().stream().sorted().forEach(k
                -> System.out.printf("%d %d%n", k, counts.get(k)));
    }

    public static void main(String[] a) {
        distCheck(() -> (int) (Math.random() * 5) + 1, 1_000_000, 1);
    }
}
```


```txt
1 200439
2 201016
3 199406
4 199869
5 199270
```



## JavaScript

{{trans|Tcl}}

```javascript
function distcheck(random_func, times, opts) {
    if (opts === undefined) opts = {}
    opts['delta'] = opts['delta'] || 2;

    var count = {}, vals = [];
    for (var i = 0; i < times; i++) {
        var val = random_func();
        if (! has_property(count, val)) {
            count[val] = 1;
            vals.push(val);
        }
        else
            count[val] ++;
    }
    vals.sort(function(a,b) {return a-b});

    var target = times / vals.length;
    var tolerance = target * opts['delta'] / 100;

    for (var i = 0; i < vals.length; i++) {
        var val = vals[i];
        if (Math.abs(count[val] - target) > tolerance)
            throw "distribution potentially skewed for " + val +
                  ": expected result around " + target + ", got " +count[val];
        else
            print(val + "\t" + count[val]);
    }
}

function has_property(obj, propname) {
    return typeof(obj[propname]) == "undefined" ? false : true;
}

try {
    distcheck(function() {return Math.floor(10 * Math.random())}, 100000);
    print();
    distcheck(function() {return (Math.random() > 0.95 ? 1 : 0)}, 100000);
} catch (e) {
    print(e);
}
```

Output:

```txt
0       9945
1       9862
2       9954
3       10104
4       9861
5       10140
6       10066
7       10001
8       10101
9       9966

distribution potentially skewed for 0: expected result around 50000, got 95040
```



## Julia

{{works with|Julia|0.6}}


```julia
function distcheck(f::Function, rep::Int=10000, Δ::Int=3)
    smpl = f(rep)
    counts = Dict(k => count(smpl .== k) for k in unique(smpl))
    expected = rep / length(counts)
    lbound = expected * (1 - 0.01Δ)
    ubound = expected * (1 + 0.01Δ)
    noobs = count(x -> !(lbound ≤ x ≤ ubound), values(counts))
    if noobs > 0 warn(@sprintf "%2.4f%% values out of bounds" noobs / rep) end
    return counts
end

# Dice5 check
distcheck(x -> rand(1:5, x))
# Dice7 check
distcheck(dice7)
```



## Kotlin


```scala
// version 1.1.3

import java.util.Random

val r = Random()

fun dice5() = 1 + r.nextInt(5)

fun checkDist(gen: () -> Int, nRepeats: Int, tolerance: Double = 0.5) {
    val occurs = mutableMapOf<Int, Int>()
    for (i in 1..nRepeats) {
        val d = gen()
        if (occurs.containsKey(d))
            occurs[d] = occurs[d]!! + 1
        else
            occurs.put(d, 1)
    }
    val expected = (nRepeats.toDouble()/ occurs.size).toInt()
    val maxError = (expected * tolerance / 100.0).toInt()
    println("Repetitions = $nRepeats, Expected = $expected")
    println("Tolerance = $tolerance%, Max Error = $maxError\n")
    println("Integer   Occurrences   Error  Acceptable")
    val f = "  %d        %5d      %5d     %s"
    var allAcceptable = true
    for ((k,v) in occurs.toSortedMap()) {
        val error = Math.abs(v - expected)
        val acceptable = if (error <= maxError) "Yes" else "No"
        if (acceptable == "No") allAcceptable = false
        println(f.format(k, v, error, acceptable))
    }
    println("\nAcceptable overall: ${if (allAcceptable) "Yes" else "No"}")
}

fun main(args: Array<String>) {
    checkDist(::dice5, 1_000_000)
    println()
    checkDist(::dice5, 100_000)
}
```


Sample output:

```txt

Repetitions = 1000000, Expected = 200000
Tolerance = 0.5%, Max Error = 1000

Integer   Occurrences   Error  Acceptable
  1        200074         74     Yes
  2        200497        497     Yes
  3        199295        705     Yes
  4        199822        178     Yes
  5        200312        312     Yes

Acceptable overall: Yes

Repetitions = 100000, Expected = 20000
Tolerance = 0.5%, Max Error = 100

Integer   Occurrences   Error  Acceptable
  1        20265        265     No
  2        20229        229     No
  3        19836        164     No
  4        19931         69     Yes
  5        19739        261     No

Acceptable overall: No

```



## Liberty BASIC

LB cannot pass user-defined function by name, so we use predefined function name - GENERATOR

```lb

n=1000
print "Testing ";n;" times"
if  not(check(n, 0.05)) then print "Test failed" else print "Test passed"
print

n=10000
print "Testing ";n;" times"
if  not(check(n, 0.05)) then print "Test failed" else print "Test passed"
print

n=50000
print "Testing ";n;" times"
if  not(check(n, 0.05)) then print "Test failed" else print "Test passed"
print

end

function check(n, delta)
    'fill randoms
    dim a(n)
    maxBucket=0
    minBucket=1e10
    for i = 1 to n
        a(i) = GENERATOR()
        if a(i)>maxBucket then maxBucket=a(i)
        if a(i)<minBucket then minBucket=a(i)
    next
    'fill buckets
    nBuckets = maxBucket+1 'from 0
    dim buckets(maxBucket)
    for i = 1 to n
        buckets(a(i)) =  buckets(a(i))+1
    next
    'check buckets
    expected=n/(maxBucket-minBucket+1)
    minVal=int(expected*(1-delta))
    maxVal=int(expected*(1+delta))
    expected=int(expected)
    print "minVal", "Expected", "maxVal"
    print minVal, expected, maxVal
    print "Bucket", "Counter", "pass/fail"
    check = 1
    for i = minBucket to maxBucket
        print i, buckets(i), _
            iif$((minVal > buckets(i)) OR (buckets(i) > maxVal) ,"fail","")
        if (minVal > buckets(i)) OR (buckets(i) > maxVal) then check = 0
    next
end function

function iif$(test, valYes$, valNo$)
    iif$ =  valNo$
    if test then iif$ =  valYes$
end function

function GENERATOR()
    'GENERATOR = int(rnd(0)*10) '0..9
    GENERATOR = 1+int(rnd(0)*5) '1..5: dice5
end function

```

{{Out}}

```txt

Testing 1000 times
minVal        Expected      maxVal
190           200           210
Bucket        Counter       pass/fail
1             213           fail
2             204
3             192
4             188           fail
5             203
Test failed

Testing 10000 times
minVal        Expected      maxVal
1900          2000          2100
Bucket        Counter       pass/fail
1             2041
2             1952
3             1975
4             2026
5             2006
Test passed

Testing 50000 times
minVal        Expected      maxVal
9500          10000         10500
Bucket        Counter       pass/fail
1             10012
2             10207
3             10009
4             9911
5             9861
Test passed

```



## Mathematica


```Mathematica
SetAttributes[CheckDistribution, HoldFirst]
CheckDistribution[function_,number_,delta_] :=(Print["Expected: ", N[number/7], ", Generated :",
Transpose[Tally[Table[function, {number}]]][[2]] // Sort];  If[(Max[#]-Min[#])&
 [Transpose[Tally[Table[function, {number}]]][[2]]] < delta*number/700, "Flat", "Skewed"])
```


Example usage:

```txt
CheckDistribution[RandomInteger[{1, 7}], 10000, 5]

->Expected: 1428.57, Generated :{1372,1420,1429,1431,1433,1450,1465}
->"Skewed"

CheckDistribution[RandomInteger[{1, 7}], 100000, 5]

->Expected: 14285.7, Generated :{14182,14186,14240,14242,14319,14407,14424}
->"Flat"
```



## OCaml


```ocaml
let distcheck fn n ?(delta=1.0) () =
  let h = Hashtbl.create 5 in
  for i = 1 to n do
    let v = fn() in
    let n =
      try Hashtbl.find h v
      with Not_found -> 0
    in
    Hashtbl.replace h v (n+1)
  done;
  Hashtbl.iter (fun v n -> Printf.printf "%d => %d\n%!" v n) h;
  let target = (float n) /. float (Hashtbl.length h) in
  Hashtbl.iter (fun key value ->
    if abs_float(float value -. target) > 0.01 *. delta *. (float n)
    then (Printf.eprintf
      "distribution potentially skewed for '%d': expected around %f, got %d\n%!"
       key target value)
  ) h;
;;
```



## PARI/GP

This tests the purportedly random 7-sided die with a slightly biased 1000-sided die.

```parigp
dice5()=random(5)+1;

dice7()={
  my(t);
  while((t=dice5()*5+dice5()) > 26,);
  t\3-1
};

cumChi2(chi2,dof)={
	my(g=gamma(dof/2));
	incgam(dof/2,chi2/2,g)/g
};

test(f,n,alpha=.05)={
	v=vector(n,i,f());
	my(s,ave,dof,chi2,p);
	s=sum(i=1,n,v[i],0.);
	ave=s/n;
	dof=n-1;
	chi2=sum(i=1,n,(v[i]-ave)^2)/ave;
	p=cumChi2(chi2,dof);
	if(p<alpha,
		error("Not flat enough, significance only "p)
	,
		print("Flat with significance "p);
	)
};

test(dice7, 10^5)
test(()->if(random(1000),random(1000),1), 10^5)
```

Output:

```txt
Flat with significance 0.2931867820813680387842134664085280183

  ###   user error: Not flat enough, significance only 5.391077606003910233 E-3500006
```



## Perl

Testing two 'types' of 7-sided dice.  Both appear to be fair.
{{trans|Perl 6}}

```perl
sub roll7 { 1+int rand(7) }
sub roll5 { 1+int rand(5) }
sub roll7_5 {
  while(1) {
    my $d7 = (5*&roll5 + &roll5 - 6) % 8;
    return $d7 if $d7;
  }
}

my $threshold = 5;

print dist( $_, $threshold,  \&roll7   ) for <1001 1000006>;
print dist( $_, $threshold,  \&roll7_5 ) for <1001 1000006>;

sub dist {
my($n, $threshold, $producer) = @_;
    my @dist;
    my $result;
    my $expect = $n / 7;
    $result .= sprintf "%10d expected\n", $expect;

    for (1..$n) { @dist[&$producer]++; }

    for my $i (1..7) {
        my $v = @dist[$i];
        my $pct = ($v - $expect)/$expect*100;
        $result .= sprintf "%d %8d %6.1f%%%s\n", $i, $v, $pct, (abs($pct) > $threshold ? ' - skewed' : '');
    }
    return $result . "\n";
}
```

{{out}}

```txt
       143 expected
1      144    0.7%
2      137   -4.2%
3      121  -15.4% - skewed
4      163   14.0% - skewed
5      150    4.9%
6      138   -3.5%
7      148    3.5%

    142858 expected
1   142332   -0.4%
2   142648   -0.1%
3   143615    0.5%
4   142305   -0.4%
5   142703   -0.1%
6   142821   -0.0%
7   143582    0.5%

       143 expected
1      149    4.2%
2      159   11.2% - skewed
3      154    7.7% - skewed
4      130   -9.1% - skewed
5      143    0.0%
6      138   -3.5%
7      128  -10.5% - skewed

    142858 expected
1   142574   -0.2%
2   143043    0.1%
3   142446   -0.3%
4   143325    0.3%
5   142949    0.1%
6   142990    0.1%
7   142679   -0.1%
```



## Perl 6

Since the tested function is rolls of a 7 sided die, the test numbers are magnitudes of 10<sup>x</sup> bumped up to the closest multiple of 7. This reduces spurious error from there not being an integer expected value.

```perl6
my $d7 = 1..7;
sub roll7 { $d7.roll };

my $threshold = 3;

for 14, 105, 1001, 10003, 100002, 1000006 ->  $n
  { dist( $n, $threshold,  &roll7 ) };


sub dist ( $n is copy, $threshold, &producer ) {
    my @dist;
    my $expect = $n / 7;
    say "Expect\t",$expect.fmt("%.3f");

    loop ($_ = $n; $n; --$n) { @dist[&producer()]++; }

    for @dist.kv -> $i, $v is copy {
        next unless $i;
        $v //= 0;
        my $pct = ($v - $expect)/$expect*100;
        printf "%d\t%d\t%+.2f%% %s\n", $i, $v, $pct,
          ($pct.abs > $threshold ?? '- skewed' !! '');
    }
    say '';
}
```

Sample output:

```txt

Expect  2.000
1       2       +0.00%
2       3       +50.00% - skewed
3       2       +0.00%
4       2       +0.00%
5       3       +50.00% - skewed
6       0       -100.00% - skewed
7       2       +0.00%

Expect  15.000
1       15      +0.00%
2       17      +13.33% - skewed
3       13      -13.33% - skewed
4       16      +6.67% - skewed
5       14      -6.67% - skewed
6       16      +6.67% - skewed
7       14      -6.67% - skewed

Expect  143.000
1       134     -6.29% - skewed
2       142     -0.70%
3       141     -1.40%
4       137     -4.20% - skewed
5       142     -0.70%
6       170     +18.88% - skewed
7       135     -5.59% - skewed

Expect  1429.000
1       1396    -2.31%
2       1468    +2.73%
3       1405    -1.68%
4       1442    +0.91%
5       1453    +1.68%
6       1417    -0.84%
7       1422    -0.49%

Expect  14286.000
1       14222   -0.45%
2       14320   +0.24%
3       14326   +0.28%
4       14425   +0.97%
5       14140   -1.02%
6       14275   -0.08%
7       14294   +0.06%

Expect  142858.000
1       142510  -0.24%
2       142436  -0.30%
3       142438  -0.29%
4       143152  +0.21%
5       142905  +0.03%
6       143232  +0.26%
7       143333  +0.33%

```



## Phix


```Phix
function check(integer fid, range, iterations, atom delta)
--
-- fid: routine_id of function that yields integer 1..range
-- range: the maximum value that is returned from fid
-- iterations: number of iterations to test
-- delta: variance, for example 0.005 means 0.5%
--
-- returns -1/0/1 for impossible/not flat/flat.
--
    atom av = iterations/range  -- average/expected value

    if floor(av)<av-delta*av
    or ceil(av)>av+delta*av then
        return -1   -- impossible
    end if
    sequence counts = repeat(0,range)
    for i=1 to iterations do
        counts[call_func(fid,{})] += 1
    end for
    atom max_delta = max(sq_abs(sq_sub(counts,av)))
    return max_delta<delta*av
end function

function rand7()
  return rand(7)
end function

constant flats = {"impossible","not flat","flat"}
for p=2 to 7 do
    integer n = power(10,p)
--  n = n+7-remainder(n,7)
    integer flat = check(routine_id("rand7"), 7, n, 0.005)
    printf(1,"%d iterations: %s\n",{n,flats[flat+2]})
end for
```

{{out}}

```txt

100 iterations: impossible
1000 iterations: impossible
10000 iterations: not flat
100000 iterations: not flat
1000000 iterations: flat
10000000 iterations: flat

```

At the specified 0.5%, 1000000 iterations is occasionally not flat, and 10000 is sometimes flat at 3%.

As shown above, it is not mathematically possible to distribute 1000 over 7 bins with <= 0.5% variance.

At 100 iterations, the permitted range is ~14.21..14.36, so you could not get even one bin right.

At 1000 iterations, 142 is too low (and 144 too high), they would all have to be 143, but 7*143=1001.

The commented-out adjustment to n (as Perl 6) changes the "1000 impossible" result to "1001 not flat",

except of course for the one-in-however-many-gazillion chance of getting exactly 143 of each.


## PicoLisp

The following function takes a count, and allowed deviation in per mill
(one-tenth of a percent), and a 'prg' code body (i.e. an arbitrary number of
executable expressions).

```PicoLisp
(de checkDistribution (Cnt Pm . Prg)
   (let Res NIL
      (do Cnt (accu 'Res (run Prg 1) 1))
      (let
         (N (/ Cnt (length Res))
            Min (*/ N (- 1000 Pm) 1000)
            Max (*/ N (+ 1000 Pm) 1000) )
         (for R Res
            (prinl (cdr R) " " (if (>= Max (cdr R) Min) "Good" "Bad")) ) ) ) )
```

Output:

```txt
: (checkDistribution 100000 5 (rand 1 7))
14299 Good
14394 Bad
14147 Bad
14418 Bad
14159 Bad
14271 Good
14312 Good
```



## PureBasic


```PureBasic
Prototype RandNum_prt()

Procedure.s distcheck(*function.RandNum_prt, repetitions, delta.d)
  Protected text.s, maxIndex = 0
  Dim bucket(maxIndex) ;array will be resized as needed

  For i = 1 To repetitions ;populate buckets
    v = *function()
    If v > maxIndex
      maxIndex = v
      Redim bucket(maxIndex)
    EndIf
    bucket(v) + 1
  Next


  lbnd = Round((repetitions / maxIndex) * (100 - delta) / 100, #PB_Round_Up)
  ubnd = Round((repetitions / maxIndex) * (100 + delta) / 100, #PB_Round_Down)
  text = "Distribution check:" + #crlf$ + #crlf$
  text + "Total elements = " + Str(repetitions) + #crlf$ + #crlf$
  text + "Margin = " + StrF(delta, 2) + "% --> Lbound = " + Str(lbnd) + ", Ubound = " + Str(ubnd) + #crlf$

  For i = 1 To maxIndex
    If bucket(i) < lbnd Or bucket(i) > ubnd
      text + #crlf$ + "Bucket " + Str(i) + " contains " + Str(bucket(i)) + " elements.  Skewed."
    EndIf
  Next
  ProcedureReturn text
EndProcedure

MessageRequester("Results", distcheck(@dice7(), 1000000, 0.20))
```

A small delta was chosen to increase the chance of a skewed result in the sample output:

```txt
Distribution check:

Total elements = 1000000

Margin = 0.20% --> Lbound = 142572, Ubound = 143142

Bucket 1 contains 141977 elements.  Skewed.
Bucket 6 contains 143860 elements.  Skewed.
```



## Python

{{works with|Python|3.1}}

```python
from collections import Counter
from pprint import pprint as pp

def distcheck(fn, repeats, delta):
    '''\
    Bin the answers to fn() and check bin counts are within +/- delta %
    of repeats/bincount'''
    bin = Counter(fn() for i in range(repeats))
    target = repeats // len(bin)
    deltacount = int(delta / 100. * target)
    assert all( abs(target - count) < deltacount
                for count in bin.values() ), "Bin distribution skewed from %i +/- %i: %s" % (
                    target, deltacount, [ (key, target - count)
                                          for key, count in sorted(bin.items()) ]
                    )
    pp(dict(bin))
```

Sample output:

```txt
>>> distcheck(dice5, 1000000, 1)
{1: 200244, 2: 199831, 3: 199548, 4: 199853, 5: 200524}
>>> distcheck(dice5, 1000, 1)
Traceback (most recent call last):
  File "<pyshell#30>", line 1, in <module>
    distcheck(dice5, 1000, 1)
  File "C://Paddys/rand7fromrand5.py", line 54, in distcheck
    for key, count in sorted(bin.items()) ]
AssertionError: Bin distribution skewed from 200 +/- 2: [(1, 4), (2, -33), (3, 6), (4, 11), (5, 12)]
```



## R


```r
distcheck <- function(fn, repetitions=1e4, delta=3)
{
   if(is.character(fn))
   {
      fn <- get(fn)
   }
   if(!is.function(fn))
   {
      stop("fn is not a function")
   }
   samp <- fn(n=repetitions)
   counts <- table(samp)
   expected <- repetitions/length(counts)
   lbound <- expected * (1 - 0.01*delta)
   ubound <- expected * (1 + 0.01*delta)
   status <- ifelse(counts < lbound, "under",
      ifelse(counts > ubound, "over", "okay"))
   data.frame(value=names(counts), counts=as.vector(counts), status=status)
}
distcheck(dice7.vec)
```



## Racket

{{trans|Tcl}}

Returns a pair of a boolean stating uniformity and either the "uniform" distribution or a report of the first skew number found.


```racket
#lang racket
(define (pretty-fraction f)
  (if (integer? f) f
      (let* ((d (denominator f)) (n (numerator f)) (q (quotient n d)) (r (remainder n d)))
        (format "~a ~a" q (/ r d)))))

(define (test-uniformity/naive r n δ)
  (define observation (make-hash))
  (for ((_ (in-range n))) (hash-update! observation (r) add1 0))
  (define target (/ n (hash-count observation)))
  (define max-skew (* n δ 1/100))
  (define (skewed? v)
    (> (abs (- v target)) max-skew))
  (let/ec ek
    (cons
     #t
     (for/list ((k (sort (hash-keys observation) <)))
       (define v (hash-ref observation k))
       (when (skewed? v)
         (ek (cons
              #f
              (format "~a distribution of ~s potentially skewed for ~a. expected ~a got ~a"
                      'test-uniformity/naive r k (pretty-fraction target) v))))
       (cons k v)))))

(define (straight-die)
  (min 6 (add1 (random 6))))

(define (crooked-die)
  (min 6 (add1 (random 7))))

; Test whether the builtin generator is uniform:
(test-uniformity/naive (curry random 10) 1000 5)
; Test whether a straight die is uniform:
(test-uniformity/naive straight-die 1000 5)
; Test whether a biased die fails:
(test-uniformity/naive crooked-die 1000 5)
```


{{out}}

```txt
'(#t (0 . 96) (1 . 100) (2 . 103) (3 . 86) (4 . 94) (5 . 111) (6 . 106) (7 . 99) (8 . 108) (9 . 97))
'(#t (1 . 169) (2 . 185) (3 . 184) (4 . 163) (5 . 144) (6 . 155))
'(#f . "test-uniformity/naive distribution of #<procedure:crooked-die> potentially skewed for 6. expected 166 2/3 got 262")
```



## REXX


```rexx
/*REXX program  simulates a number of trials  of a  random digit  and show it's skew %. */
parse arg func times delta seed .                /*obtain arguments (options) from C.L. */
if  func=='' |  func==","   then  func= 'RANDOM' /*function not specified?  Use default.*/
if times=='' | times==","   then times= 1000000  /*times     "      "        "     "    */
if delta=='' | delta==","   then  delta= 1/2     /*delta%    "      "        "     "    */
if datatype(seed, 'W')  then call random ,,seed  /*use some RAND seed for repeatability.*/
highDig=9                                        /*use this var for the highest digit.  */
!.=0                                             /*initialize all possible random trials*/
      do times                                   /* [↓]  perform a bunch of trials.     */
      if func=='RANDOM'  then ?= random(highDig)                  /*use RANDOM function.*/
                         else interpret '?=' func "(0,"highDig')' /* " specified   "    */
      !.?= !.? + 1                                        /*bump the invocation counter.*/
      end   /*t*/                                /* [↑]  store trials ───► pigeonholes. */
                                                 /* [↓]  compute the digit's skewness.  */
g= times /  (1 + highDig)                        /*calculate number of each digit throw.*/
w= max(9, length( commas(times) ) )              /*maximum length of  number  of trials.*/
pad= left('', 9)                                 /*this is used for output indentation. */
say pad  'digit'   center(" hits", w)    ' skew '    "skew %"    'result'     /*header. */
say pad  '─────'   center('', w, '─')    '──────'    "──────"    '──────'     /*hdr sep.*/
                                                 /** [↑]  show header and the separator.*/
      do k=0  to highDig                         /*process each of the possible digits. */
      skew= g - !.k                              /*calculate the  skew   for the digit. */
      skewPC= (1 - (g - abs(skew)) / g) * 100    /*    "      "    "  percentage for dig*/
      say pad center(k, 5)                 right( commas(!.k), w)        right(skew, 6) ,
          right( format(skewPC, , 3), 6)   center( word('ok skewed', 1+(skewPC>delta)), 6)
      end   /*k*/

say pad   '─────'    center('', w, '─')    '──────'   "─────"   '──────'  /*separator.  */
y= 5+1+w+1+6+1+6+1+6                                                      /*width + seps*/
say pad center(" (with  "  commas(times)   '  trials)'  , y)              /*# trials.   */
say pad center(" (skewed when exceeds "      delta'%)'  , y)              /*skewed note.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;  do jc=length(_)-3  to 1  by -3; _=insert(',', _, jc); end;  return _
```

{{out|output|text=  when using the default inputs:}}

```txt

          digit    hits    skew  skew % result
          ───── ───────── ────── ────── ──────
            0      99,739    261  0.261   ok
            1      99,819    181  0.181   ok
            2     100,463   -463  0.463   ok
            3      99,787    213  0.213   ok
            4      99,632    368  0.368   ok
            5     100,787   -787  0.787 skewed
            6      99,704    296  0.296   ok
            7      99,605    395  0.395   ok
            8     100,488   -488  0.488   ok
            9      99,976     24  0.024   ok
          ───── ───────── ────── ───── ──────
               (with   1,000,000   trials)
               (skewed when exceeds  0.5%)

```



## Ring


```ring

# Project : Verify distribution uniformity/Naive

maxrnd = 7
for r = 2 to 5
    check = distcheck(pow(10,r), 0.05)
    see "over " + pow(10,r) + " runs dice5 " + nl
    if check
       see "failed distribution check with " + check + " bin(s) out of range" + nl
    else
       see "passed distribution check" + nl
    ok
next

func distcheck(repet, delta)
m = 1
s = 0
bins = list(maxrnd)
for i = 1 to repet
    r = dice5() + 1
    bins[r] = bins[r] + 1
    if r>m m = r ok
next
for i = 1 to m
    if bins[i]/(repet/m) > 1+delta s = s + 1 ok
    if bins[i]/(repet/m) < 1-delta s = s + 1 ok
next
return s

func dice5
     return random(5)

```

Output:

```txt

Over 100 runs dice5 failed distribution check with 3 bin(s) out of range
Over 1000 runs dice5 failed distribution check with 1 bin(s) out of range
Over 10000 runs dice5 passed distribution check
Over 100000 runs dice5 passed distribution check

```



## Ruby

{{trans|Tcl}}

```ruby
def distcheck(n, delta=1)
  unless block_given?
    raise ArgumentError, "pass a block to this method"
  end

  h = Hash.new(0)
  n.times {h[ yield ] += 1}

  target = 1.0 * n / h.length
  h.each do |key, value|
    if (value - target).abs > 0.01 * delta * n
      raise StandardError,
        "distribution potentially skewed for '#{key}': expected around #{target}, got #{value}"
    end
  end

  puts h.sort.map{|k, v| "#{k} #{v}"}
end

if __FILE__ == $0
  begin
    distcheck(100_000) {rand(10)}
    distcheck(100_000) {rand > 0.95}
  rescue StandardError => e
    p e
  end
end
```


{{out}}

```txt
0 10048
1 9949
2 9920
3 9919
4 9957
5 10087
6 9835
7 10026
8 10069
9 10190
#<StandardError: distribution potentially skewed for 'false': expected around 50000.0, got 95040>

```



## Run BASIC


```runbasic
s$ = "#########################"
dim num(100)
for i = 1 to 1000
  n = (rnd(1) * 10) + 1
  num(n) = num(n) + 1
next i

for i = 1 to 10
  print using("###",i);" "; using("#####",num(i));" ";left$(s$,num(i) / 5)
next i
```

```txt

  1    90 ##################
  2   110 ######################
  3   105 #####################
  4   100 ####################
  5   107 #####################
  6   133 #########################
  7    85 #################
  8    96 ###################
  9    82 ################
 10    92 ##################*
```



## Scala

===Imperative, ugly, mutable data===

```Scala
object DistrubCheck1 extends App {

  private def distCheck(f: () => Int, nRepeats: Int, delta: Double): Unit = {
    val counts = scala.collection.mutable.Map[Int, Int]()

    for (_ <- 0 until nRepeats)
      counts.updateWith(f()) {
        case Some(count) => Some(count + 1)
        case None => Some(1)
      }

    val target: Double = nRepeats.toDouble / counts.size
    val deltaCount: Int = (delta / 100.0 * target).toInt
    counts.foreach {
      case (k, v) =>
        if (math.abs(target - v) >= deltaCount)
          println(f"distribution potentially skewed for $k%s: $v%d")
    }
    counts.toIndexedSeq.foreach(entry => println(f"${entry._1}%d ${entry._2}%d"))
  }

  distCheck(() => 1 + util.Random.nextInt(5), 1_000_000, 1)

}
```



### Functional Style

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/oYJWUvX/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/O513W3VoQ7ulspUMnGvTiQ Scastie (remote JVM)].

```Scala
object DistrubCheck2 extends App {
  private def distCheck(f: () => Int, nRepeats: Int, delta: Double): Unit = {
    val counts: Map[Int, Int] =
      (0 until nRepeats).map(_ => f()).groupBy(identity).map { case (k, v) => (k, v.size) }
    val target = nRepeats / counts.size.toDouble

    counts.withFilter { case (_, v) => math.abs(target - v) >= (delta / 100.0 * target) }
      .foreach { case (k, v) => println(f"distribution potentially skewed for $k%s: $v%d") }

    counts.toIndexedSeq.foreach(entry => println(f"${entry._1}%d ${entry._2}%d"))
  }

  distCheck(() => 1 + util.Random.nextInt(5), 1_000_000, 1)

}
```


## Tcl


```tcl
proc distcheck {random times {delta 1}} {
    for {set i 0} {$i<$times} {incr i} {incr vals([uplevel 1 $random])}
    set target [expr {$times / [array size vals]}]
    foreach {k v} [array get vals] {
        if {abs($v - $target) > $times  * $delta / 100.0} {
           error "distribution potentially skewed for $k: expected around $target, got $v"
        }
    }
    foreach k [lsort -integer [array names vals]] {lappend result $k $vals($k)}
    return $result
}
```

Demonstration:

```tcl
# First, a uniformly distributed random variable
puts [distcheck {expr {int(10*rand())}} 100000]

# Now, one that definitely isn't!
puts [distcheck {expr {rand()>0.95}} 100000]
```

Which produces this output (error in red):
 0 10003 1 9851 2 10058 3 10193 4 10126 5 10002 6 9852 7 9964 8 9957 9 9994
 <span style="color:red">distribution potentially skewed for 0: expected around 50000, got 94873</span>


## VBScript


```vb
Option Explicit

sub verifydistribution(calledfunction, samples, delta)
	Dim i, n, maxdiff
	'We could cheat via Dim d(7), but "7" wasn't mentioned in the Task. Heh.
	Dim d : Set d = CreateObject("Scripting.Dictionary")
	wscript.echo "Running """ & calledfunction & """ " & samples & " times..."
	for i = 1 to samples
		Execute "n = " & calledfunction
		d(n) = d(n) + 1
	next
	n = d.Count
	maxdiff = 0
	wscript.echo "Expected average count is " & Int(samples/n) & " across " & n & " buckets."
	for each i in d.Keys
		dim diff : diff = abs(1 - d(i) / (samples/n))
		if diff > maxdiff then maxdiff = diff
		wscript.echo "Bucket " & i & " had " & d(i) & " occurences" _
		& vbTab & " difference from expected=" & FormatPercent(diff, 2)
	next
	wscript.echo "Maximum found variation is " & FormatPercent(maxdiff, 2) _
		& ", desired limit is " & FormatPercent(delta, 2) & "."
	if maxdiff > delta then wscript.echo "Skewed!" else wscript.echo "Smooth!"
end sub
```

Demonstration with included [[Seven-sided dice from five-sided dice#VBScript]] code:

```vb
verifydistribution "dice7", 1000, 0.03
verifydistribution "dice7", 100000, 0.03
```

Which produces this output:
 Running "dice7" 1000 times...
 Expected average count is 142 across 7 buckets.
 Bucket 2 had 150 occurences      difference from expected=5.00%
 Bucket 7 had 147 occurences      difference from expected=2.90%
 Bucket 6 had 146 occurences      difference from expected=2.20%
 Bucket 5 had 141 occurences      difference from expected=1.30%
 Bucket 1 had 152 occurences      difference from expected=6.40%
 Bucket 4 had 115 occurences      difference from expected=19.50%
 Bucket 3 had 149 occurences      difference from expected=4.30%
 Maximum found variation is 19.50%, desired limit is 3.00%.
 Skewed!
 Running "dice7" 100000 times...
 Expected average count is 14285 across 7 buckets.
 Bucket 5 had 14420 occurences    difference from expected=0.94%
 Bucket 4 had 14298 occurences    difference from expected=0.09%
 Bucket 2 had 14202 occurences    difference from expected=0.59%
 Bucket 7 had 14201 occurences    difference from expected=0.59%
 Bucket 6 had 14237 occurences    difference from expected=0.34%
 Bucket 3 had 14263 occurences    difference from expected=0.16%
 Bucket 1 had 14379 occurences    difference from expected=0.65%
 Maximum found variation is 0.94%, desired limit is 3.00%.
 Smooth!


## zkl

This tests the random spread over 0..9. It starts at 10 samples and doubles the sample size until the spread is within 0.1% of 10% for each bucket.

```zkl
fcn rtest(N){
   dist:=L(0,0,0,0,0,0,0,0,0,0);
   do(N){n:=(0).random(10); dist[n]=dist[n]+1}
   sum:=dist.sum();
   dist=dist.apply('wrap(n){n.toFloat()/sum*100});
   if (dist.filter((10.0).closeTo.fp1(0.1)).len() == 10)
      { "Good enough at %,d: %s".fmt(N,dist).println(); return(True); }
   False
}

n:=10;
while(not rtest(n)) {n*=2}
```

{{out}}
Reported numbers is the percent that bucket has of all samples.

```txt

Good enough at 163,840:
L(10.0665,9.94019,10.0146,9.99939,10.0775,10.0201,9.93713,10.0775,9.9054,9.96155)

```



{{omit from|GUISS}}
