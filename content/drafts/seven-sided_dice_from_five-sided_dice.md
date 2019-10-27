+++
title = "Seven-sided dice from five-sided dice"
description = ""
date = 2019-08-24T20:06:10Z
aliases = []
[extra]
id = 4679
[taxonomies]
categories = []
tags = []
+++

{{task|Probability and statistics}}

;Task:
(Given an equal-probability generator of one of the integers 1 to 5
as <code>dice5</code>),   create <code>dice7</code> that generates a pseudo-random integer from
1 to 7 in equal probability using only <code>dice5</code> as a source of random
numbers,   and check the distribution for at least one million calls using the function created in   [[Verify distribution uniformity/Naive|Simple Random Distribution Checker]].


'''Implementation suggestion:'''
<code>dice7</code> might call <code>dice5</code> twice, re-call if four of the 25
combinations are given, otherwise split the other 21 combinations
into 7 groups of three, and return the group index from the rolls.

<small>(Task adapted from an answer [http://stackoverflow.com/questions/90715/what-are-the-best-programming-puzzles-you-came-across here])</small>





## Ada

The specification of a package Random_57:

```Ada
package Random_57 is

   type Mod_7 is mod 7;

   function Random7 return Mod_7;
     -- a "fast" implementation, minimazing the calls to the Random5 generator
   function Simple_Random7 return Mod_7;
     -- a simple implementation

end Random_57;
```

Implementation of Random_57:

```Ada
 with Ada.Numerics.Discrete_Random;

package body Random_57 is
   type M5 is mod 5;

   package Rand_5 is new  Ada.Numerics.Discrete_Random(M5);
   Gen: Rand_5.Generator;
      function Random7 return Mod_7 is
      N: Natural;

   begin
      loop
         N :=  Integer(Rand_5.Random(Gen))* 5 + Integer(Rand_5.Random(Gen));
         -- N is uniformly distributed in 0 .. 24
         if N < 21 then
            return Mod_7(N/3);
         else -- (N-21) is in 0 .. 3
            N := (N-21) * 5 +  Integer(Rand_5.Random(Gen)); -- N is in 0 .. 19
            if N < 14 then
               return Mod_7(N / 2);
            else -- (N-14) is in 0 .. 5
               N := (N-14) * 5 +  Integer(Rand_5.Random(Gen)); -- N is in 0 .. 29
               if N < 28 then
                  return Mod_7(N/4);
               else -- (N-28) is in 0 .. 1
                  N := (N-28) * 5 + Integer(Rand_5.Random(Gen)); -- 0 .. 9
                  if N < 7 then
                     return Mod_7(N);
                  else -- (N-7) is in 0, 1, 2
                     N := (N-7)* 5 + Integer(Rand_5.Random(Gen)); -- 0 .. 14
                     if N < 14 then
                        return Mod_7(N/2);
                     else -- (N-14) is 0. This is not useful for us!
                        null;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end loop;

   end Random7;

   function Simple_Random7 return Mod_7 is
      N: Natural :=
        Integer(Rand_5.Random(Gen))* 5 + Integer(Rand_5.Random(Gen));
      -- N is uniformly distributed in 0 .. 24
   begin
      while N > 20 loop
         N :=  Integer(Rand_5.Random(Gen))* 5 + Integer(Rand_5.Random(Gen));
      end loop; -- Now I <= 20
      return Mod_7(N / 3);
   end Simple_Random7;

begin
   Rand_5.Reset(Gen);
end Random_57;
```

A main program, using the Random_57 package:

```Ada
with Ada.Text_IO, Random_57;

procedure R57 is

   use Random_57;

   type Fun is access function return Mod_7;

   function Rand return Mod_7 renames Random_57.Random7;
   -- change this to "... renames Random_57.Simple_Random;" if you like

   procedure Test(Sample_Size: Positive; Rand: Fun; Precision: Float := 0.3) is

      Counter: array(Mod_7) of Natural := (others => 0);
      Expected: Natural := Sample_Size/7;
      Small: Mod_7 := Mod_7'First;
      Large: Mod_7 := Mod_7'First;

      Result: Mod_7;
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Sample Size: " & Integer'Image(Sample_Size));
      Ada.Text_IO.Put( " Bins:");
      for I in 1 .. Sample_Size loop
         Result := Rand.all;
         Counter(Result) := Counter(Result) + 1;
      end loop;
      for J in Mod_7 loop
         Ada.Text_IO.Put(Integer'Image(Counter(J)));
         if Counter(J) < Counter(Small) then Small := J; end if;
         if Counter(J) > Counter(Large)  then Large := J;  end if;
      end loop;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line(" Small Bin:" & Integer'Image(Counter(Small)));
      Ada.Text_IO.Put_Line(" Large Bin: " & Integer'Image(Counter(Large)));

      if Float(Counter(Small)*7) * (1.0+Precision) < Float(Sample_Size) then
         Ada.Text_IO.Put_Line("Failed! Small too small!");
      elsif Float(Counter(Large)*7) * (1.0-Precision) > Float(Sample_Size) then
         Ada.Text_IO.Put_Line("Failed! Large too large!");
      else
         Ada.Text_IO.Put_Line("Passed");
      end if;
   end Test;

begin
   Test(    10_000, Rand'Access, 0.08);
   Test(   100_000, Rand'Access, 0.04);
   Test( 1_000_000, Rand'Access, 0.02);
   Test(10_000_000, Rand'Access, 0.01);
end R57;
```

{{out}}

```txt

Sample Size:  10000
 Bins: 1368 1404 1435 1491 1483 1440 1379
 Small Bin: 1368
 Large Bin:  1491
Passed

Sample Size:  100000
 Bins: 14385 14110 14362 14404 14362 14206 14171
 Small Bin: 14110
 Large Bin:  14404
Passed

Sample Size:  1000000
 Bins: 143765 142384 142958 142684 142799 142956 142454
 Small Bin: 142384
 Large Bin:  143765
Passed

Sample Size:  10000000
 Bins: 1429266 1428214 1428753 1427032 1428418 1428699 1429618
 Small Bin: 1427032
 Large Bin:  1429618
Passed
```



## ALGOL 68

{{trans|C}} - note: This specimen retains the original [[Seven-sided dice from five-sided dice#C|C]] coding style. 
{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}
C's version using no multiplications, divisions, or mod operators:

```algol68
PROC dice5 = INT:
  1 + ENTIER (5*random);

PROC mulby5 = (INT n)INT:
   ABS (BIN n SHL 2) + n;
 
PROC dice7 = INT: (
  INT d55 := 0;
  INT m := 1;
  WHILE
    m := ABS ((2r1 AND BIN m) SHL 2) + ABS (BIN m SHR 1);  # repeats 4 - 2 - 1 #
    d55 := mulby5(mulby5(d55)) + mulby5(dice5) + dice5 - 6;
# WHILE # d55 < m DO SKIP OD;

  m := 1;
  WHILE d55>0 DO
    d55 +:= m;
    m := ABS (BIN d55 AND 2r111); # modulas by 8 #
    d55 := ABS (BIN d55 SHR 3)    # divide by 8 #
  OD;
  m
);
 
PROC distcheck = (PROC INT dice, INT count, upb)VOID: (
  [upb]INT sum; FOR i TO UPB sum DO sum[i] := 0 OD;
  FOR i TO count DO sum[dice]+:=1 OD;
  FOR i TO UPB sum WHILE print(whole(sum[i],0)); i /= UPB sum DO print(", ") OD;
  print(new line)
);

main:
(
  distcheck(dice5, 1000000, 5);
  distcheck(dice7, 1000000, 7)
)
```

{{out}}

```txt

200598, 199852, 199939, 200602, 199009
143529, 142688, 142816, 142747, 142958, 142802, 142460

```



## AutoHotkey


```AutoHotkey
dice5()
{  Random, v, 1, 5
   Return, v
}

dice7()
{  Loop
   {  v := 5 * dice5() + dice5() - 6
      IfLess v, 21, Return, (v // 3) + 1
   }
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
        check% = FNdistcheck(FNdice7, 10^r%, 0.1)
        PRINT "Over "; 10^r% " runs dice7 ";
        IF check% THEN
          PRINT "failed distribution check with "; check% " bin(s) out of range"
        ELSE
          PRINT "passed distribution check"
        ENDIF
      NEXT
      END
      
      DEF FNdice7
      LOCAL x% : x% = FNdice5 + 5*FNdice5
      IF x%>26 THEN = FNdice7 ELSE = (x%+1) MOD 7 + 1
      
      DEF FNdice5 = RND(5)
      
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
```

{{out}}

```txt

Over 100 runs dice7 failed distribution check with 4 bin(s) out of range
Over 1000 runs dice7 failed distribution check with 2 bin(s) out of range
Over 10000 runs dice7 passed distribution check
Over 100000 runs dice7 passed distribution check

```



## C


```c
int rand5()
{
	int r, rand_max = RAND_MAX - (RAND_MAX % 5);
	while ((r = rand()) >= rand_max);
	return r / (rand_max / 5) + 1;
}
 
int rand5_7()
{
	int r;
	while ((r = rand5() * 5 + rand5()) >= 27);
	return r / 3 - 1;
}

int main()
{
	printf(check(rand5, 5, 1000000, .05) ? "flat\n" : "not flat\n");
	printf(check(rand7, 7, 1000000, .05) ? "flat\n" : "not flat\n");
	return 0;
}
```

{{out}}

```txt

flat
flat

```



## C++

This solution tries to minimize calls to the underlying d5 by reusing information from earlier calls.

```cpp>template<typename F
 class fivetoseven
{
public:
  fivetoseven(F f): d5(f), rem(0), max(1) {}
  int operator()();
private:
  F d5;
  int rem, max;
};

template<typename F>
 int fivetoseven<F>::operator()()
{
  while (rem/7 == max/7)
  {
    while (max < 7)
    {
      int rand5 = d5()-1;
      max *= 5;
      rem = 5*rem + rand5;
    }

    int groups = max / 7;
    if (rem >= 7*groups)
    {
      rem -= 7*groups;
      max -= 7*groups;
    }
  }

  int result = rem % 7;
  rem /= 7;
  max /= 7;
  return result+1;
}

int d5()
{
  return 5.0*std::rand()/(RAND_MAX + 1.0) + 1;
}

fivetoseven<int(*)()> d7(d5);

int main()
{
  srand(time(0));
  test_distribution(d5, 1000000, 0.001);
  test_distribution(d7, 1000000, 0.001);
}
```



## C sharp

{{trans|Java}}

```csharp

using System;

public class SevenSidedDice
{
    Random random = new Random();
		
        static void Main(string[] args)
		{
			SevenSidedDice sevenDice = new SevenSidedDice();
			Console.WriteLine("Random number from 1 to 7: "+ sevenDice.seven());
            Console.Read();
		}
		
		int seven()
		{
			int v=21;
			while(v>20)
				v=five()+five()*5-6;
			return 1+v%7;
		}
		
		int five()
		{
        return 1 + random.Next(5);
		}
}
```



## Clojure

Uses the verify function defined in [[Verify distribution uniformity/Naive#Clojure]]

```Clojure
(def dice5 #(rand-int 5))

(defn dice7 []
  (quot (->> dice5                     ; do the following to dice5
             (repeatedly 2)            ; call it twice
             (apply #(+ %1 (* 5 %2)))  ; d1 + 5*d2 => 0..24
             #()                       ; wrap that up in a function
             repeatedly                ; make infinite sequence of the above
             (drop-while #(> % 20))    ; throw away anything > 20
             first)                    ; grab first acceptable element
        3))                            ; divide by three rounding down

(doseq [n [100 1000 10000] [num count okay?] (verify dice7 n)]
  (println "Saw" num count "times:"
           (if okay? "that's" "   not") "acceptable"))
```



```txt
Saw 0 10 times:    not acceptable
Saw 1 19 times:    not acceptable
Saw 2 12 times:    not acceptable
Saw 3 15 times: that's acceptable
Saw 4 11 times:    not acceptable
Saw 5 11 times:    not acceptable
Saw 6 22 times:    not acceptable
Saw 0 142 times: that's acceptable
Saw 1 158 times:    not acceptable
Saw 2 151 times: that's acceptable
Saw 3 153 times: that's acceptable
Saw 4 118 times:    not acceptable
Saw 5 139 times: that's acceptable
Saw 6 139 times: that's acceptable
Saw 0 1498 times: that's acceptable
Saw 1 1411 times: that's acceptable
Saw 2 1436 times: that's acceptable
Saw 3 1434 times: that's acceptable
Saw 4 1414 times: that's acceptable
Saw 5 1408 times: that's acceptable
Saw 6 1399 times: that's acceptable
```



## Common Lisp

{{trans|C}}

```lisp
(defun d5 ()
  (1+ (random 5)))

(defun d7 ()
  (loop for d55 = (+ (* 5 (d5)) (d5) -6)
        until (< d55 21)
        finally (return (1+ (mod d55 7)))))
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

{{trans|C++}}

```d
import std.random;
import verify_distribution_uniformity_naive: distCheck;

/// Generates a random number in [1, 5].
int dice5() /*pure nothrow*/ @safe {
    return uniform(1, 6);
}

/// Naive, generates a random number in [1, 7] using dice5.
int fiveToSevenNaive() /*pure nothrow*/ @safe {
    immutable int r = dice5() + dice5() * 5 - 6;
    return (r < 21) ? (r % 7) + 1 : fiveToSevenNaive();
}

/**
Generates a random number in [1, 7] using dice5,
minimizing calls to dice5.
*/
int fiveToSevenSmart() @safe {
    static int rem = 0, max = 1;

    while (rem / 7 == max / 7) {
        while (max < 7) {
            immutable int rand5 = dice5() - 1;
            max *= 5;
            rem = 5 * rem + rand5;
        }

        immutable int groups = max / 7;
        if (rem >= 7 * groups) {
            rem -= 7 * groups;
            max -= 7 * groups;
        }
    }

    immutable int result = rem % 7;
    rem /= 7;
    max /= 7;
    return result + 1;
}

void main() /*@safe*/ {
    enum int N = 400_000;
    distCheck(&dice5, N, 1);
    distCheck(&fiveToSevenNaive, N, 1);
    distCheck(&fiveToSevenSmart, N, 1);
}
```

{{out}}

```txt
1 80365
2 79941
3 80065
4 79784
5 79845

1 57186
2 57201
3 57180
4 57231
5 57124
6 56832
7 57246

1 57367
2 56869
3 57644
4 57111
5 57157
6 56809
7 57043
```



## E

{{trans|Common Lisp}}
{{improve|E|Write dice7 in a prettier fashion and use the distribution checker once it's been written.}}

```e
def dice5() {
  return entropy.nextInt(5) + 1
}

def dice7() {
  var d55 := null
  while ((d55 := 5 * dice5() + dice5() - 6) >= 21) {}
  return d55 %% 7 + 1
}
```


```e
def bins := ([0] * 7).diverge()
for x in 1..1000 {
  bins[dice7() - 1] += 1
}
println(bins.snapshot())
```



## Elixir


```elixir
defmodule Dice do
  def dice5, do: :rand.uniform( 5 )
  
  def dice7 do
    dice7_from_dice5
  end
  
  defp dice7_from_dice5 do
    d55 = 5*dice5 + dice5 - 6           # 0..24
    if d55 < 21, do: rem( d55, 7 ) + 1,
                 else: dice7_from_dice5
  end
end

fun5 = fn -> Dice.dice5 end
IO.inspect VerifyDistribution.naive( fun5, 1000000, 3 )
fun7 = fn -> Dice.dice7 end
IO.inspect VerifyDistribution.naive( fun7, 1000000, 3 )
```


{{out}}

```txt

:ok
:ok

```



## Erlang


```Erlang

-module( dice ).

-export( [dice5/0, dice7/0, task/0] ).

dice5() -> random:uniform( 5 ).

dice7() ->
	dice7_small_enough( dice5() * 5 + dice5() - 6 ). % 0 - 24

task() ->
       verify_distribution_uniformity:naive( fun dice7/0, 1000000, 1 ).



dice7_small_enough( N ) when N < 21 -> N div 3 + 1;
dice7_small_enough( _N ) -> dice7().

```


{{out}}

```txt

76> dice:task().
ok

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


{{out}}

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

{{works with|GNU Forth}}

```forth
require random.fs

: d5 5 random 1+ ;
: discard? 5 = swap 1 > and ;
: d7
   begin d5 d5 2dup discard? while 2drop repeat
   1- 5 * + 1- 7 mod 1+ ;
```

{{out}}

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
```



## Fortran

{{works with|Fortran|95 and later}}

```fortran
module rand_mod
  implicit none

contains

function rand5()
  integer :: rand5
  real :: r

  call random_number(r)
  rand5 = 5*r + 1
end function
  
function rand7()
  integer :: rand7
  
  do
    rand7 = 5*rand5() + rand5() - 6
    if (rand7 < 21) then
      rand7 = rand7 / 3 + 1
      return
    end if
  end do
end function
end module

program Randtest
  use rand_mod
  implicit none
  
  integer, parameter :: samples = 1000000
 
  call distcheck(rand7, samples, 0.005)
  write(*,*)
  call distcheck(rand7, samples, 0.001)

end program
```

{{out}}

```txt
Distribution Uniform

Distribution potentially skewed for bucket 1  Expected: 142857  Actual: 143142
Distribution potentially skewed for bucket 2  Expected: 142857  Actual: 143454
Distribution potentially skewed for bucket 3  Expected: 142857  Actual: 143540
Distribution potentially skewed for bucket 4  Expected: 142857  Actual: 142677
Distribution potentially skewed for bucket 5  Expected: 142857  Actual: 142511
Distribution potentially skewed for bucket 6  Expected: 142857  Actual: 142163
Distribution potentially skewed for bucket 7  Expected: 142857  Actual: 142513
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

{{out}}

```txt

Max delta: 356.1428571428696 Flat enough: true
Max delta: 787.8571428571304 Flat enough: false

```



## Groovy


```groovy
random = new Random()

int rand5() {
    random.nextInt(5) + 1
}

int rand7From5() {
    def raw = 25
    while (raw > 21) {
        raw = 5*(rand5() - 1) + rand5()
    }
    (raw % 7) + 1
}
```

Test:

```groovy
def test = {
    (1..6). each {
        def counts = [0g, 0g, 0g, 0g, 0g, 0g, 0g]
        def target = 10g**it
        def popSize = 7*target
        (0..<(popSize)).each {
            def i = rand7From5() - 1
            counts[i] = counts[i] + 1g
        }
        BigDecimal stdDev = (counts.collect { it - target}.collect { it * it }.sum() / popSize) ** 0.5g
        def countMap = (0..<counts.size()).inject([:]) { map, index -> map + [(index+1):counts[index]] }
        
        println """\
         counts: ${countMap}
population size: ${popSize}
        std dev: ${stdDev.round(new java.math.MathContext(3))}
"""
    }
}

4.times {
    println """
TRIAL #${it+1}

### ========
"""
    test(it)
}
```

{{out}}
<pre style="height:30ex;overflow:scroll;">TRIAL #1

### ========

         counts: [1:16, 2:10, 3:9, 4:7, 5:12, 6:8, 7:8]
population size: 70
        std dev: 0.910

         counts: [1:85, 2:97, 3:108, 4:110, 5:95, 6:105, 7:100]
population size: 700
        std dev: 0.800

         counts: [1:990, 2:1008, 3:992, 4:1060, 5:1008, 6:997, 7:945]
population size: 7000
        std dev: 0.995

         counts: [1:9976, 2:10007, 3:10009, 4:9858, 5:10109, 6:9988, 7:10053]
population size: 70000
        std dev: 0.714

         counts: [1:100310, 2:99783, 3:99843, 4:100353, 5:99804, 6:99553, 7:100354]
population size: 700000
        std dev: 0.968

         counts: [1:999320, 2:1000942, 3:1000201, 4:1000878, 5:999181, 6:999632, 7:999846]
population size: 7000000
        std dev: 0.654


TRIAL #2

### ========

         counts: [1:10, 2:8, 3:9, 4:9, 5:14, 6:7, 7:13]
population size: 70
        std dev: 0.756

         counts: [1:104, 2:101, 3:97, 4:108, 5:100, 6:87, 7:103]
population size: 700
        std dev: 0.619

         counts: [1:995, 2:970, 3:1001, 4:953, 5:1006, 6:1081, 7:994]
population size: 7000
        std dev: 1.18

         counts: [1:10013, 2:10063, 3:9843, 4:9984, 5:9986, 6:10059, 7:10052]
population size: 70000
        std dev: 0.711

         counts: [1:100048, 2:99647, 3:100240, 4:100683, 5:99813, 6:100320, 7:99249]
population size: 700000
        std dev: 1.39

         counts: [1:1000579, 2:1000541, 3:999497, 4:1000805, 5:999708, 6:999161, 7:999709]
population size: 7000000
        std dev: 0.586


TRIAL #3

### ========

         counts: [1:9, 2:8, 3:11, 4:14, 5:10, 6:11, 7:7]
population size: 70
        std dev: 0.676

         counts: [1:100, 2:92, 3:105, 4:107, 5:111, 6:91, 7:94]
population size: 700
        std dev: 0.733

         counts: [1:1010, 2:1053, 3:967, 4:981, 5:1027, 6:959, 7:1003]
population size: 7000
        std dev: 0.984

         counts: [1:9857, 2:10037, 3:9992, 4:10231, 5:9828, 6:10140, 7:9915]
population size: 70000
        std dev: 1.37

         counts: [1:99650, 2:99580, 3:99848, 4:100507, 5:99916, 6:100212, 7:100287]
population size: 700000
        std dev: 1.01

         counts: [1:1001710, 2:999667, 3:1000685, 4:1000411, 5:999369, 6:998469, 7:999689]
population size: 7000000
        std dev: 0.965


TRIAL #4

### ========

         counts: [1:12, 2:7, 3:11, 4:12, 5:7, 6:9, 7:12]
population size: 70
        std dev: 0.676

         counts: [1:97, 2:96, 3:101, 4:93, 5:96, 6:124, 7:93]
population size: 700
        std dev: 1.01

         counts: [1:985, 2:1023, 3:1018, 4:1023, 5:995, 6:973, 7:983]
population size: 7000
        std dev: 0.615

         counts: [1:9948, 2:9968, 3:10131, 4:10050, 5:9990, 6:10039, 7:9874]
population size: 70000
        std dev: 0.764

         counts: [1:100125, 2:99616, 3:99912, 4:100286, 5:99674, 6:100190, 7:100197]
population size: 700000
        std dev: 0.787

         counts: [1:1001267, 2:999911, 3:1000602, 4:999483, 5:1000549, 6:998725, 7:999463]
population size: 7000000
        std dev: 0.798
```



## Haskell


```haskell
import System.Random
import Data.List

sevenFrom5Dice = do
  d51 <- randomRIO(1,5) :: IO Int
  d52 <- randomRIO(1,5) :: IO Int
  let d7 = 5*d51+d52-6
  if d7 > 20 then sevenFrom5Dice
       else return $ 1 + d7 `mod` 7
```

{{out}}

```haskell
*Main> replicateM 10 sevenFrom5Dice
[2,3,1,1,6,2,5,6,5,3]
```

Test:

```haskell
*Main> mapM_ print .sort =<< distribCheck sevenFrom5Dice 1000000 3
(1,(142759,True))                                                 
(2,(143078,True))                                                 
(3,(142706,True))                                                 
(4,(142403,True))                                                 
(5,(142896,True))                                                 
(6,(143028,True))                                                 
(7,(143130,True))
```


=={{header|Icon}} and {{header|Unicon}}==
{{trans|Ruby}} 
Uses <code>verify_uniform</code> from [[Simple_Random_Distribution_Checker#Icon_and_Unicon|here]].

```Icon

$include "distribution-checker.icn"

# return a uniformly distributed number from 1 to 7,
# but only using a random number in range 1 to 5.
procedure die_7 ()
  while rnd := 5*?5 + ?5 - 6 do {
    if rnd < 21 then suspend rnd % 7 + 1
  }
end

procedure main ()
  if verify_uniform (create (|die_7()), 1000000, 0.01)
    then write ("uniform")
    else write ("skewed")
end

```


{{out}}

```txt

5 142870
2 142812
7 142901
4 142960
1 143113
6 142706
3 142638
uniform

```



## J

The first step is to create 7-sided dice rolls from 5-sided dice rolls (<code>rollD5</code>):

```j>rollD5=: [: 
: ] ?@$ 5:      NB. makes a y shape array of 5s, "rolls" the array and increments.
roll2xD5=: [: rollD5 2 ,~ */ NB. rolls D5 twice for each desired D7 roll (y rows, 2 cols)
toBase10=: 5 #. <:           NB. decrements and converts rows from base 5 to 10
keepGood=: #~ 21&>           NB. compress out values not less than 21
groupin3s=: [: >. >: % 3:    NB. increments, divides by 3 and takes ceiling

getD7=: groupin3s@keepGood@toBase10@roll2xD5
```

Here are a couple of variations on the theme that achieve the same result:

```j
getD7b=: 0 8 -.~ 3 >.@%~ 5 #. [: <:@rollD5 2 ,~ ]
getD7c=: [: (#~ 7&>:) 3 >.@%~ [: 5&#.&.:<:@rollD5 ] , 2:
```

The trouble is that we probably don't have enough D7 rolls yet because we compressed out any double D5 rolls that evaluated to 21 or more. So we need to accumulate some more D7 rolls until we have enough. J has two types of verb definition - tacit (arguments not referenced) and explicit (more conventional function definitions) illustrated below:

Here's an explicit definition that accumulates rolls from <code>getD7</code>:

```j
rollD7x=: monad define
  n=. */y                             NB. product of vector y is total number of D7 rolls required
  rolls=. ''                          NB. initialize empty noun rolls
  while. n > #rolls do.               NB. checks if if enough D7 rolls accumulated
    rolls=. rolls, getD7 >. 0.75 * n  NB. calcs 3/4 of required rolls and accumulates getD7 rolls
  end.
  y $ rolls                           NB. shape the result according to the vector y
)
```

Here's a tacit definition that does the same thing:

```j>getNumRolls=: [: 
. 0.75 * */@[       NB. calc approx 3/4 of the required rolls
accumD7Rolls=: ] , getD7@getNumRolls  NB. accumulates getD7 rolls
isNotEnough=: */@[ > #@]              NB. checks if enough D7 rolls accumulated

rollD7t=: ] $ (accumD7Rolls ^: isNotEnough ^:_)&''
```

The <code>verb1 ^: verb2 ^:_</code> construct repeats <code>x verb1 y</code> while <code>x verb2 y</code> is true. It is like saying "Repeat accumD7Rolls while isNotEnough".

Example usage:

```j
   rollD7t 10         NB. 10 rolls of D7
6 4 5 1 4 2 4 5 2 5
   rollD7t 2 5        NB. 2 by 5 array of D7 rolls
5 1 5 1 3
3 4 3 5 6
   rollD7t 2 3 5      NB. 2 by 3 by 5 array of D7 rolls
4 7 7 5 7
3 7 1 4 5
5 4 5 7 6

1 1 7 6 3
4 4 1 4 4
1 1 1 6 5

NB. check results from rollD7x and rollD7t have same shape
   ($@rollD7x -: $@rollD7t) 10     
1
   ($@rollD7x -: $@rollD7t) 2 3 5   
1
```



## Java

{{trans|Python}}

```Java
import java.util.Random;
public class SevenSidedDice 
{
	private static final Random rnd = new Random();
	public static void main(String[] args)
	{
		SevenSidedDice now=new SevenSidedDice();
		System.out.println("Random number from 1 to 7: "+now.seven());
	}
	int seven()
	{
		int v=21;
		while(v>20)
			v=five()+five()*5-6;
		return 1+v%7;
	}
	int five()
	{
		return 1+rnd.nextInt(5);
	}
}
```



## JavaScript

{{trans|Ruby}}

```javascript
function dice5()
{
 return 1 + Math.floor(5 * Math.random());
}

function dice7()
{
 while (true)
 {
  var dice55 = 5 * dice5() + dice5() - 6;
  if (dice55 < 21)
   return dice55 % 7 + 1;
 }
}

distcheck(dice5, 1000000);
print();
distcheck(dice7, 1000000);
```

{{out}}

```txt
1       199792
2       200425
3       199243
4       200407
5       200133

1       143617
2       142209
3       143023
4       142990
5       142894
6       142648
7       142619 
```



## Julia


```Julia
dice5() = rand(1:5)

function dice7()
    r = 5*dice5() + dice5() - 6
    r < 21 ? (r%7 + 1) : dice7()
end
```

Distribution check:

```txt
julia> hist([dice5() for i=1:10^6])
(0:1:5,[199932,200431,199969,199925,199743])

julia> hist([dice7() for i=1:10^6])
(0:1:7,[142390,143032,142837,142999,142800,142642,143300])
```



## Kotlin


```scala
// version 1.1.3

import java.util.Random

val r = Random()

fun dice5() = 1 + r.nextInt(5)

fun dice7(): Int {
    while (true) {
       val t = (dice5() - 1) * 5 + dice5() - 1
       if (t >= 21) continue
       return 1 + t / 3
    }
}

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
    checkDist(::dice7, 1_400_000)
}
```


Sample output:

```txt

Repetitions = 1400000, Expected = 200000
Tolerance = 0.5%, Max Error = 1000

Integer   Occurrences   Error  Acceptable
  1        199285        715     Yes
  2        200247        247     Yes
  3        199709        291     Yes
  4        199983         17     Yes
  5        199990         10     Yes
  6        200664        664     Yes
  7        200122        122     Yes

Acceptable overall: Yes

```



## Liberty BASIC


```lb

n=1000000    '1000000 would take several minutes
print "Testing ";n;" times"
if  not(check(n, 0.05)) then print "Test failed" else print "Test passed"
end

'function check(n, delta) is defined at
'http://rosettacode.org/wiki/Verify_distribution_uniformity/Naive#Liberty_BASIC

function GENERATOR()
    'GENERATOR = int(rnd(0)*10) '0..9
    'GENERATOR = 1+int(rnd(0)*5) '1..5: dice5

    'dice7()
    do
        temp =dice5() *5 +dice5() -6
    loop until temp <21
    GENERATOR =( temp mod 7) +1

end function

function dice5()
    dice5=1+int(rnd(0)*5) '1..5: dice5
end function

```

{{Out}}

```txt

Testing 1000000 times
minVal        Expected      maxVal
135714        142857        150000
Bucket        Counter       pass/fail
1             143310
2             143500
3             143040
4             145185
5             140998
6             142610
7             141357
Test passed

```



## Lua


```lua
dice5 = function() return math.random(5) end

function dice7()
  x = dice5() * 5 + dice5() - 6
  if x > 20 then return dice7() end
  return x%7 + 1
end
```


## M2000 Interpreter

We make a stack object (is reference type) and pass it as a closure to dice7 lambda function. For each dice7 we pop the top value of stack, and we add a fresh dice5 (random(1,5)) as last value of stack, so stack used as FIFO. Each time z has the sum of 7 random values.

We check for uniform numbers using +-5% from expected value. 

```M2000 Interpreter

Module CheckIt {
      Def long i, calls, max, min
      s=stack:=random(1,5),random(1,5), random(1,5), random(1,5), random(1,5), random(1,5), random(1,5)
      z=0: for i=1 to 7 { z+=stackitem(s, i)} 
      dice7=lambda z, s -> {
            =((z-1) mod 7)+1 : stack s {z-=Number : data random(1,5): z+=Stackitem(7)}
      }
      Dim count(1 to 7)=0&  ' long type
      calls=700000
      p=0.05
      IsUniform=lambda max=calls/7*(1+p), min=calls/7*(1-p) (a)->{
            if len(a)=0 then =false : exit
            =false
            m=each(a)
            while m 
                  if array(m)<min or array(m)>max then break
            end while
            =true
      }
      For i=1 to calls {count(dice7())++}
      max=count()#max()
      expected=calls div 7
      min=count()#min()
      for i=1 to 7
      document doc$=format$("{0}{1::-7}",i,count(i))+{
      }
      Next i
      doc$=format$("min={0} expected={1} max={2}", min, expected, max)+{
      }+format$("Verify Uniform:{0}", if$(IsUniform(count())->"uniform", "skewed"))+{
      }
      Print
      report doc$
      clipboard doc$
}
CheckIt

```


{{out}}
<pre style="height:30ex;overflow:scroll">
1   9865
2  10109
3   9868
4   9961
5   9936
6   9922
7  10339
min=9865 expected=10000 max=10339
Verify Uniform:uniform


1 100214
2 100336
3 100049
4  99505
5  99951
6  99729
7 100216
min=99505 expected=100000 max=100336
Verify Uniform:uniform
</pre >


## Mathematica


```Mathematica
sevenFrom5Dice := (tmp$ = 5*RandomInteger[{1, 5}] + RandomInteger[{1, 5}] - 6; 
  If [tmp$ < 21, 1 + Mod[tmp$ , 7], sevenFrom5Dice])
```


```txt
CheckDistribution[sevenFrom5Dice, 1000000, 5]
->Expected: 142857., Generated :{142206,142590,142650,142693,142730,143475,143656}
->"Flat"
```



## OCaml


```ocaml
let dice5() = 1 + Random.int 5 ;;

let dice7 =
  let rolls2answer = Hashtbl.create 25 in
  let n = ref 0 in
  for roll1 = 1 to 5 do
    for roll2 = 1 to 5 do
      Hashtbl.add rolls2answer (roll1,roll2) (!n / 3 +1);
      incr n
    done;
  done;
  let rec aux() =
    let trial = Hashtbl.find rolls2answer (dice5(),dice5()) in
    if trial <= 7 then trial else aux()
  in
  aux
;;
```



## PARI/GP


```parigp
dice5()=random(5)+1;

dice7()={
  my(t);
  while((t=dice5()*5+dice5()) > 21,);
  (t+2)\3
};
```


## Perl

Using dice5 twice to generate numbers in the range 0 to 24.  If we consider these modulo 8 and re-call if we get zero, we have eliminated 4 cases and created the necessary number in the range from 1 to 7.

```perl
sub dice5 { 1+int rand(5) }

sub dice7 {
  while(1) {
    my $d7 = (5*dice5()+dice5()-6) % 8;
    return $d7 if $d7;
  }
}

my %count7;
my $n = 1000000;
$count7{dice7()}++ for 1..$n;
printf "%s: %5.2f%%\n", $_, 100*($count7{$_}/$n*7-1) for sort keys %count7;

```

{{out}}

```txt

1:  0.05%
2:  0.16%
3: -0.43%
4:  0.11%
5:  0.01%
6: -0.15%
7:  0.24%

```



## Perl 6

{{works with|Rakudo|2018.03}}


```perl6
my $d5 = 1..5;
sub d5() { $d5.roll; }  # 1d5

sub d7() {
    my $flat = 21;
    $flat = 5 * d5() - d5() until $flat < 21;
    $flat % 7 + 1;
}

# Testing
my @dist;
my $n = 1_000_000;
my $expect = $n / 7;

loop ($_ = $n; $n; --$n) { @dist[d7()]++; }

say "Expect\t",$expect.fmt("%.3f");
for @dist.kv -> $i, $v {
    say "$i\t$v\t" ~ (($v - $expect)/$expect*100).fmt("%+.2f%%") if $v;
}
```

{{out}}

```txt
Expect	142857.143
1	143088	+0.16%
2	143598	+0.52%
3	141741	-0.78%
4	142832	-0.02%
5	143040	+0.13%
6	142988	+0.09%
7	142713	-0.10%

```



## Phix

replace rand7() in [[Verify_distribution_uniformity/Naive#Phix]] with:

```Phix
function dice5()
    return rand(5)
end function

function dice7()
    while true do
        integer r = dice5()*5+dice5()-3         --  ( ie 3..27, but )
        if r<24 then return floor(r/3) end if   -- (only 3..23 useful)
    end while
end function
```

{{out}}

```txt

1000000 iterations: flat

```



## PicoLisp


```PicoLisp
(de dice5 ()
   (rand 1 5) )

(de dice7 ()
   (use R
      (until (> 21 (setq R (+ (* 5 (dice5)) (dice5) -6))))
      (inc (% R 7)) ) )
```

{{out}}

```txt
: (let R NIL
   (do 1000000 (accu 'R (dice7) 1))
   (sort R) )
-> ((1 . 142295) (2 . 142491) (3 . 143448) (4 . 143129) (5 . 142701) (6 . 143142) (7 . 142794))
```



## PureBasic

{{trans|Lua}}

```PureBasic
Procedure dice5()
  ProcedureReturn Random(4) + 1
EndProcedure
 
Procedure dice7()
  Protected x
  
  x = dice5() * 5 + dice5() - 6
  If x > 20 
    ProcedureReturn dice7()
  EndIf 
  
  ProcedureReturn x % 7 + 1
EndProcedure
```



## Python


```python
from random import randint

def dice5():
    return randint(1, 5)

def dice7():
    r = dice5() + dice5() * 5 - 6
    return (r % 7) + 1 if r < 21 else dice7()
```

Distribution check using [[Simple Random Distribution Checker#Python|Simple Random Distribution Checker]]:

```txt
>>> distcheck(dice5, 1000000, 1)
{1: 200244, 2: 199831, 3: 199548, 4: 199853, 5: 200524}
>>> distcheck(dice7, 1000000, 1)
{1: 142853, 2: 142576, 3: 143067, 4: 142149, 5: 143189, 6: 143285, 7: 142881}

```



## Racket


```Racket

#lang racket
(define (dice5) (add1 (random 5)))

(define (dice7)
  (define res (+ (* 5 (dice5)) (dice5) -6))
  (if (< res 21) (+ 1 (modulo res 7)) (dice7)))

```


Checking the uniformity using math library:


```racket

-> (require math/statistics)
-> (samples->hash (for/list ([i 700000]) (dice7)))
'#hash((7 . 100392)
       (6 . 100285)
       (5 . 99774)
       (4 . 100000)
       (3 . 100000)
       (2 . 99927)
       (1 . 99622))

```



## R

5-sided die.

```r
dice5 <- function(n=1) sample(5, n, replace=TRUE)
```

Simple but slow 7-sided die, using a while loop.

```r
dice7.while <- function(n=1) 
{
   score <- numeric()
   while(length(score) < n)
   {
      total <- sum(c(5,1) * dice5(2)) - 3
      if(total < 24) score <- c(score, total %/% 3)
   } 
   score 
}
system.time(dice7.while(1e6)) # longer than 4 minutes
```

More complex, but much faster vectorised version.

```r
dice7.vec <- function(n=1, checkLength=TRUE) 
{
   morethan2n <- 3 * n + 10 + (n %% 2)       #need more than 2*n samples, because some are discarded
   twoDfive <- matrix(dice5(morethan2n), nrow=2)
   total <- colSums(c(5, 1) * twoDfive) - 3
   score <- ifelse(total < 24, total %/% 3, NA)
   score <- score[!is.na(score)]
   #If length is less than n (very unlikely), add some more samples
   if(checkLength) 
   {
      while(length(score) < n)
      {
         score <- c(score, dice7(n, FALSE)) 
      }
      score[1:n]
   } else score  
}
system.time(dice7.vec(1e6))   # ~1 sec
```



## REXX


```rexx
/*REXX program simulates a 7─sided die based on a 5─sided throw for a number of trials. */
parse arg trials sample seed .                   /*obtain optional arguments from the CL*/
if trials=='' | trials=","  then trials=       1 /*Not specified?  Then use the default.*/
if sample=='' | sample=","  then sample= 1000000 /* "      "         "   "   "     "    */
if datatype(seed,'W')  then call random ,,seed   /*Integer?  Then use it as a RAND seed.*/
L= length(trials)                                /* [↑]  one million samples to be used.*/

   do #=1  for trials;          die.= 0          /*performs the number of desired trials*/
   k= 0
               do  until k==sample;             r= 5 * random(1, 5)  +  random(1, 5)  -  6
               if r>20  then iterate
               k= k+1;                          r=r // 7  +  1;         die.r= die.r + 1
               end   /*until*/
   say
   expect= sample % 7
   say center('trial:' right(#, L)    "   "     sample  'samples, expect' expect, 80, "─")

               do j=1  for 7
               say '      side'      j       "had "       die.j           ' occurrences',
                   '      difference from expected:'right(die.j - expect, length(sample) )
               end   /*j*/
   end   /*#*/                                   /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the input of:     <tt> 11 </tt>}}


(Shown at five-sixth size.)

<pre style="font-size:84%;height:71ex">
──────────────────trial:  1     1000000 samples, expect 142857──────────────────
      side 1 had  142076  occurrences       difference from expected:   -781
      side 2 had  143053  occurrences       difference from expected:    196
      side 3 had  142342  occurrences       difference from expected:   -515
      side 4 had  142633  occurrences       difference from expected:   -224
      side 5 had  143024  occurrences       difference from expected:    167
      side 6 had  143827  occurrences       difference from expected:    970
      side 7 had  143045  occurrences       difference from expected:    188

──────────────────trial:  2     1000000 samples, expect 142857──────────────────
      side 1 had  143470  occurrences       difference from expected:    613
      side 2 had  142998  occurrences       difference from expected:    141
      side 3 had  142654  occurrences       difference from expected:   -203
      side 4 had  142545  occurrences       difference from expected:   -312
      side 5 had  142452  occurrences       difference from expected:   -405
      side 6 had  143144  occurrences       difference from expected:    287
      side 7 had  142737  occurrences       difference from expected:   -120

──────────────────trial:  3     1000000 samples, expect 142857──────────────────
      side 1 had  142773  occurrences       difference from expected:    -84
      side 2 had  143198  occurrences       difference from expected:    341
      side 3 had  142296  occurrences       difference from expected:   -561
      side 4 had  142804  occurrences       difference from expected:    -53
      side 5 had  142897  occurrences       difference from expected:     40
      side 6 had  142382  occurrences       difference from expected:   -475
      side 7 had  143650  occurrences       difference from expected:    793

──────────────────trial:  4     1000000 samples, expect 142857──────────────────
      side 1 had  143150  occurrences       difference from expected:    293
      side 2 had  142635  occurrences       difference from expected:   -222
      side 3 had  142763  occurrences       difference from expected:    -94
      side 4 had  142853  occurrences       difference from expected:     -4
      side 5 had  143132  occurrences       difference from expected:    275
      side 6 had  142403  occurrences       difference from expected:   -454
      side 7 had  143064  occurrences       difference from expected:    207

──────────────────trial:  5     1000000 samples, expect 142857──────────────────
      side 1 had  143041  occurrences       difference from expected:    184
      side 2 had  142701  occurrences       difference from expected:   -156
      side 3 had  143416  occurrences       difference from expected:    559
      side 4 had  142097  occurrences       difference from expected:   -760
      side 5 had  142451  occurrences       difference from expected:   -406
      side 6 had  143332  occurrences       difference from expected:    475
      side 7 had  142962  occurrences       difference from expected:    105

──────────────────trial:  6     1000000 samples, expect 142857──────────────────
      side 1 had  142502  occurrences       difference from expected:   -355
      side 2 had  142429  occurrences       difference from expected:   -428
      side 3 had  143146  occurrences       difference from expected:    289
      side 4 had  142791  occurrences       difference from expected:    -66
      side 5 had  143271  occurrences       difference from expected:    414
      side 6 had  143415  occurrences       difference from expected:    558
      side 7 had  142446  occurrences       difference from expected:   -411

──────────────────trial:  7     1000000 samples, expect 142857──────────────────
      side 1 had  142700  occurrences       difference from expected:   -157
      side 2 had  142691  occurrences       difference from expected:   -166
      side 3 had  143067  occurrences       difference from expected:    210
      side 4 had  141562  occurrences       difference from expected:  -1295
      side 5 had  143316  occurrences       difference from expected:    459
      side 6 had  143150  occurrences       difference from expected:    293
      side 7 had  143514  occurrences       difference from expected:    657

──────────────────trial:  8     1000000 samples, expect 142857──────────────────
      side 1 had  142362  occurrences       difference from expected:   -495
      side 2 had  143298  occurrences       difference from expected:    441
      side 3 had  142639  occurrences       difference from expected:   -218
      side 4 had  142811  occurrences       difference from expected:    -46
      side 5 had  143275  occurrences       difference from expected:    418
      side 6 had  142765  occurrences       difference from expected:    -92
      side 7 had  142850  occurrences       difference from expected:     -7

──────────────────trial:  9     1000000 samples, expect 142857──────────────────
      side 1 had  143508  occurrences       difference from expected:    651
      side 2 had  142650  occurrences       difference from expected:   -207
      side 3 had  142614  occurrences       difference from expected:   -243
      side 4 had  142916  occurrences       difference from expected:     59
      side 5 had  142944  occurrences       difference from expected:     87
      side 6 had  143129  occurrences       difference from expected:    272
      side 7 had  142239  occurrences       difference from expected:   -618

──────────────────trial: 10     1000000 samples, expect 142857──────────────────
      side 1 had  142455  occurrences       difference from expected:   -402
      side 2 had  143112  occurrences       difference from expected:    255
      side 3 had  143435  occurrences       difference from expected:    578
      side 4 had  142704  occurrences       difference from expected:   -153
      side 5 had  142376  occurrences       difference from expected:   -481
      side 6 had  142721  occurrences       difference from expected:   -136
      side 7 had  143197  occurrences       difference from expected:    340

──────────────────trial: 11     1000000 samples, expect 142857──────────────────
      side 1 had  142967  occurrences       difference from expected:    110
      side 2 had  142204  occurrences       difference from expected:   -653
      side 3 had  142993  occurrences       difference from expected:    136
      side 4 had  142797  occurrences       difference from expected:    -60
      side 5 had  143081  occurrences       difference from expected:    224
      side 6 had  142711  occurrences       difference from expected:   -146
      side 7 had  143247  occurrences       difference from expected:    390

```



## Ring


```ring

# Project : Seven-sided dice from five-sided dice

for n = 1 to 20
         d = dice7()
         see "" + d + " "
next
see nl

func dice7()
         x = dice5() * 5 + dice5() - 6
         if x > 20 
            return dice7()
         ok
         dc = x % 7 + 1
         return dc

func dice5()
        rnd = random(4) + 1
        return rnd

```

Output:

```txt

7 6 3 5 2 2 7 1 2 7 3 7 4 4 4 2 3 2 6 1

```



## Ruby

{{trans|Tcl}}
Uses <code>distcheck</code> from [[Simple_Random_Distribution_Checker#Ruby|here]].

```ruby
require './distcheck.rb'

def d5
  1 + rand(5)
end

def d7
  loop do
    d55 = 5*d5 + d5 - 6
    return (d55 % 7 + 1) if d55 < 21
  end
end

distcheck(1_000_000) {d5}
distcheck(1_000_000) {d7}
```


{{out}}

```txt
1 200227
2 200264
3 199777
4 199387
5 200345
1 143175
2 143031
3 142731
4 142716
5 142931
6 142605
7 142811
```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/3RNtNEC/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/Y5qSeW52QiC40l5vJCUMRA Scastie (remote JVM)].

```Scala
import scala.util.Random

object SevenSidedDice extends App {
  private val rnd = new Random

  private def seven = {
    var v = 21

    def five = 1 + rnd.nextInt(5)

    while (v > 20) v = five + five * 5 - 6
    1 + v % 7
  }

  println("Random number from 1 to 7: " + seven)

}
```


## Sidef

{{trans|Perl}}

```ruby
func dice5 { 1 + 5.rand.int }

func dice7 {
  loop {
    var d7 = ((5*dice5() + dice5() - 6) % 8);
    d7 && return d7;
  }
}

var count7 = Hash.new;

var n = 1e6;
n.times { count7{dice7()} := 0 ++ }
count7.keys.sort.each { |k|
    printf("%s: %5.2f%%\n", k, 100*(count7{k}/n * 7 - 1));
}
```

{{out}}

```txt
1: -0.00%
2:  0.02%
3:  0.23%
4:  0.42%
5: -0.23%
6: -0.54%
7:  0.10%
```



## Tcl

Any old D&D hand will know these as a D5 and a D7...

```tcl
proc D5 {} {expr {1 + int(5 * rand())}}

proc D7 {} {
    while 1 {
        set d55 [expr {5 * [D5] + [D5] - 6}]
        if {$d55 < 21} {
            return [expr {$d55 % 7 + 1}]
        }
    }
}
```

Checking:
 <span class="sy0">%</span> distcheck D5 <span class="nu0">1000000</span>
 1 199893 2 200162 3 200075 4 199630 5 200240
 <span class="sy0">%</span> distcheck D7 <span class="nu0">1000000</span>
 1 143121 2 142383 3 143353 4 142811 5 142172 6 143291 7 142869


## VBA

The original StackOverflow page doesn't exist any longer. Luckily [https://web.archive.org/web/20100730055051/http://stackoverflow.com:80/questions/137783/given-a-function-which-produces-a-random-integer-in-the-range-1-to-5-write-a-fun archive.org] exists.

```vb
Private Function Test4DiscreteUniformDistribution(ObservationFrequencies() As Variant, Significance As Single) As Boolean
    'Returns true if the observed frequencies pass the Pearson Chi-squared test at the required significance level.
    Dim Total As Long, Ei As Long, i As Integer
    Dim ChiSquared As Double, DegreesOfFreedom As Integer, p_value As Double
    Debug.Print "[1] ""Data set:"" ";
    For i = LBound(ObservationFrequencies) To UBound(ObservationFrequencies)
        Total = Total + ObservationFrequencies(i)
        Debug.Print ObservationFrequencies(i); " ";
    Next i
    DegreesOfFreedom = UBound(ObservationFrequencies) - LBound(ObservationFrequencies)
    'This is exactly the number of different categories minus 1
    Ei = Total / (DegreesOfFreedom + 1)
    For i = LBound(ObservationFrequencies) To UBound(ObservationFrequencies)
        ChiSquared = ChiSquared + (ObservationFrequencies(i) - Ei) ^ 2 / Ei
    Next i
    p_value = 1 - WorksheetFunction.ChiSq_Dist(ChiSquared, DegreesOfFreedom, True)
    Debug.Print
    Debug.Print "Chi-squared test for given frequencies"
    Debug.Print "X-squared ="; Format(ChiSquared, "0.0000"); ", ";
    Debug.Print "df ="; DegreesOfFreedom; ", ";
    Debug.Print "p-value = "; Format(p_value, "0.0000")
    Test4DiscreteUniformDistribution = p_value > Significance
End Function
Private Function Dice5() As Integer
    Dice5 = Int(5 * Rnd + 1)
End Function
Private Function Dice7() As Integer
    Dim i As Integer
    Do
        i = 5 * (Dice5 - 1) + Dice5
    Loop While i > 21
    Dice7 = i Mod 7 + 1
End Function
Sub TestDice7()
    Dim i As Long, roll As Integer
    Dim Bins(1 To 7) As Variant
    For i = 1 To 1000000
        roll = Dice7
        Bins(roll) = Bins(roll) + 1
    Next i
    Debug.Print "[1] ""Uniform? "; Test4DiscreteUniformDistribution(Bins, 0.05); """"
End Sub
```

{{out}}
```txt
[1] "Data set:"  142418   142898   142940   142573   143030   143139   143002  
Chi-squared test for given frequencies
X-squared =2.8870, df = 6 , p-value = 0.8229
[1] "Uniform? True"

```


## VBScript


```vb
Option Explicit

function dice5
	dice5 = int(rnd*5) + 1
end function

function dice7
	dim j
	do
		j = 5 * dice5 + dice5 - 6
	loop until j < 21
	dice7 = j mod 7 + 1
end function
```



## Verilog


```verilog


///////////////////////////////////////////////////////////////////////////////
/// seven_sided_dice_tb : (testbench)                                       ///
///      Check the distribution of the output of a seven sided dice circuit ///
///////////////////////////////////////////////////////////////////////////////
module seven_sided_dice_tb;
  reg [31:0] freq[0:6];
  reg        clk;
  wire [2:0] dice_face;
  reg        req;
  wire       valid_roll;
  integer    i;
  initial begin
    clk <= 0;
    forever begin
       #1;
       clk <= ~clk;
    end
  end
  initial begin
    req <= 1'b1;
    for(i = 0; i < 7; i = i + 1) begin
      freq[i] <= 32'b0;
    end
    repeat(10) @(posedge clk);
    repeat(7000000) begin
      @(posedge clk);
      while(~valid_roll) begin 
        @(posedge clk);
      end
      freq[dice_face] <= freq[dice_face] + 32'b1;
    end
    $display("********************************************");
    $display("*** Seven sided dice distribution:          ");
    $display("    Theoretical distribution is an uniform  ");
    $display("    distribution with (1/7)-probability     ");
    $display("    for each possible outcome,              ");
    $display("  The experimental distribution is:          ");
    for(i = 0; i < 7; i = i + 1) begin
      if(freq[i] < 32'd1_000_000) begin
        $display("%d with probability 1/7 - (%d ppm)", i, (32'd1_000_000 - freq[i])/7);
      end
      else begin
        $display("%d with probability 1/7 + (%d ppm)", i, (freq[i] - 32'd1_000_000)/7);
      end
    end
    $finish;
  end

  seven_sided_dice DUT(
    .clk(clk),
    .req(req),
    .valid_roll(valid_roll),
    .dice_face(dice_face)
  );
endmodule
///////////////////////////////////////////////////////////////////////////////
/// seven_sided_dice :                                                      ///
///      Synthsizeable module that using a 5 sided dice as a black box      ///
///      is able to reproduce the outcomes if a 7-sided dice                ///
///////////////////////////////////////////////////////////////////////////////
module seven_sided_dice(
  input wire       clk,
  input wire       req,
  output reg       valid_roll,
  output reg [2:0] dice_face
);
  wire [2:0] face1;
  wire [2:0] face2;
  reg [4:0] combination;
  reg req_p1;
  reg req_p2;
  reg req_p3;
  always @(posedge clk) begin
    req_p1 <= req;
    req_p2 <= req_p1;
  end
  always @(posedge clk) begin
    if(req_p1) begin
      combination <= face1 + face2 + {face2, 2'b00};
    end
    if(req_p2) begin
      case(combination)
           5'd0,  5'd1,  5'd2: {valid_roll, dice_face} <= {1'b1, 3'd0};
           5'd3,  5'd4,  5'd5: {valid_roll, dice_face} <= {1'b1, 3'd1};
           5'd6,  5'd7,  5'd8: {valid_roll, dice_face} <= {1'b1, 3'd2};
           5'd9, 5'd10, 5'd11: {valid_roll, dice_face} <= {1'b1, 3'd3};
          5'd12, 5'd13, 5'd14: {valid_roll, dice_face} <= {1'b1, 3'd4};
          5'd15, 5'd16, 5'd17: {valid_roll, dice_face} <= {1'b1, 3'd5};
          5'd18, 5'd19, 5'd20: {valid_roll, dice_face} <= {1'b1, 3'd6};
          default: valid_roll <= 1'b0;
      endcase
    end
  end

  five_sided_dice dice1(
    .clk(clk),
    .req(req),
    .dice_face(face1)
  );

  five_sided_dice dice2(
    .clk(clk),
    .req(req),
    .dice_face(face2)
  );
endmodule

///////////////////////////////////////////////////////////////////////////////
/// five_sided_dice :                                                       ///
///      A model of the five sided dice component                           ///
///////////////////////////////////////////////////////////////////////////////
module five_sided_dice(
  input wire clk,
  input wire req,
  output reg [2:0] dice_face
);
  always @(posedge clk) begin
    if(req) begin
      dice_face  <= $urandom % 5;
    end
  end
endmodule

```


Compiling with Icarus Verilog

```txt

> iverilog seven-sided-dice.v -o seven-sided-dice

```

Running the test

```txt

> vvp seven-sided-dice
********************************************
*** Seven sided dice distribution:          
    Theoretical distribution is an uniform  
    distribution with (1/7)-probability     
    for each possible outcome,              
  The experimental distribution is:          
          0 with probability 1/7 + (        67 ppm)
          1 with probability 1/7 - (        47 ppm)
          2 with probability 1/7 + (        92 ppm)
          3 with probability 1/7 - (        17 ppm)
          4 with probability 1/7 - (        36 ppm)
          5 with probability 1/7 + (        51 ppm)
          6 with probability 1/7 - (       109 ppm)

```



## zkl


```zkl
var die5=(1).random.fp(6); // [1..5]
fcn die7{ while((r:=5*die5() + die5())>=27){} r/3-1 }

fcn rtest(N){ //test spread over [0..9]
   dist:=L(0,0,0,0,0,0,0,0,0,0);
   do(N){ dist[die7()]+=1 }
   sum:=dist.sum();
   dist=dist.apply('wrap(n){ "%.2f%%".fmt(n.toFloat()/sum*100) }).println();
}

println("Looking for ",100.0/7,"%");
rtest(0d1_000_000);
```

{{out}}

```txt

Looking for 14.2857%
L("0.00%","14.28%","14.36%","14.22%","14.26%","14.34%","14.33%","14.21%","0.00%","0.00%")

```

