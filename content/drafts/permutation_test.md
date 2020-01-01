+++
title = "Permutation test"
description = ""
date = 2019-05-04T03:36:55Z
aliases = []
[extra]
id = 9200
[taxonomies]
categories = []
tags = []
+++

{{Clarified-review}}{{task|Probability and statistics}}
A new medical treatment was tested on a population of <math>n + m</math>
volunteers, with each volunteer randomly assigned either to a group of
<math>n</math> treatment subjects, or to a group of <math>m</math> control subjects.

Members of the treatment group were given the treatment,
and members of the control group were given a placebo.
The effect of the treatment or placebo on each volunteer
was measured and reported in this table.

{| style="text-align: left; width: 50%;" border="4" cellpadding="2" cellspacing="2"
|+ Table of experimental results
|- style="background-color: rgb(255, 204, 255);"
! Treatment group !! Control group
|-
| 85 || 68
|-
| 88 || 41
|-
| 75 || 10
|-
| 66 || 49
|-
| 25 || 16
|-
| 29 || 65
|-
| 83 || 32
|-
| 39 || 92
|-
| 97 || 28
|-
|      || 98
|}
Write a program that performs a
[[wp:Permutation_test#Permutation_tests|permutation test]] to judge
whether the treatment had a significantly stronger effect than the
placebo.

* Do this by considering every possible alternative assignment from the same pool of volunteers to a treatment group of size <math>n</math> and a control group of size <math>m</math> (i.e., the same group sizes used in the actual experiment but with the group members chosen differently), while assuming that each volunteer's effect remains constant regardless.
* Note that the number of alternatives will be the [[wp:Binomial_coefficient|binomial coefficient]] <math>\tbinom{n+m}{n}</math>.
* Compute the mean effect for each group and the difference in means between the groups in every case by subtracting the mean of the control group from the mean of the treatment group.
* Report the percentage of alternative groupings for which the difference in means is less or equal to the actual experimentally observed difference in means, and the percentage for which it is greater.
* Note that they should sum to 100%.

Extremely dissimilar values are evidence of an effect not entirely due
to chance, but your program need not draw any conclusions.

You may assume the experimental data are known at compile time if
that's easier than loading them at run time. Test your solution on the
data given above.


## Ada



```Ada
with Ada.Text_IO; with Iterate_Subsets;

procedure Permutation_Test is

   type Group_Type is array(Positive range <>) of Positive;

   Treat_Group: constant Group_Type := (85, 88, 75, 66, 25, 29, 83, 39, 97);
   Ctrl_Group:  constant Group_Type := (68, 41, 10, 49, 16, 65, 32, 92, 28, 98);

   package Iter is new Iterate_Subsets(Treat_Group'Length, Ctrl_Group'Length);

   Full_Group: constant Group_Type(1 .. Iter.All_Elements)
     := Treat_Group & Ctrl_Group;

   function Mean(S: Iter.Subset) return Float is
      Sum: Natural := 0;
   begin
      for I in S'Range loop
         Sum := Sum + Full_Group(S(I));
      end loop;
      return Float(Sum)/Float(S'Length);
   end Mean;

   package FIO is new Ada.Text_IO.Float_IO(Float);

   T_Avg: Float := Mean(Iter.First);
   S_Avg: Float;
   S:     Iter.Subset := Iter.First;
   Equal:  Positive := 1; -- Mean(Iter'First) = Mean(Iter'First)
   Higher: Natural  := 0;
   Lower:  Natural  := 0;

begin -- Permutation_Test;
   -- first, count the subsets with a higher, an equal or a lower mean
   loop
      Iter.Next(S);
      S_Avg := Mean(S);
      if S_Avg = T_Avg then
         Equal := Equal + 1;
      elsif S_Avg >= T_Avg then
         Higher := Higher + 1;
      else
         Lower := Lower + 1;
      end if;
      exit when Iter.Last(S);
   end loop;

   -- second, output the results
   declare
      use Ada.Text_IO;
      Sum: Float := Float(Higher + Equal + Lower);
   begin
      Put("Less or Equal: ");
      FIO.Put(100.0*Float(Lower+Equal) / Sum, Fore=>3, Aft=>1, Exp=>0);
      Put(Integer'Image(Lower+Equal));
      New_Line;
      Put("More:          ");
      FIO.Put(100.0*Float(Higher) / Sum,      Fore=>3, Aft=>1, Exp=>0);
      Put(Integer'Image(Higher));
      New_Line;
   end;
end Permutation_Test;
```


This solution uses an auxiliary package Iterate_Subsets. Here is the Spec:

```Ada
generic
   Subset_Size, More_Elements: Positive;
package Iterate_Subsets is

   All_Elements: Positive := Subset_Size + More_Elements;
   subtype Index is Integer range 1 .. All_Elements;
   type Subset is array (1..Subset_Size) of Index;

   -- iterate over all subsets of size Subset_Size
   -- from the set {1, 2, ..., All_Element}

   function First return Subset;
   procedure Next(S: in out Subset);
   function Last(S: Subset) return Boolean;

end Iterate_Subsets;

```


And here is the implementation:


```Ada
package body Iterate_Subsets is

   function First return Subset is
      S: Subset;
   begin
      for I in S'Range loop
         S(I) := I;
      end loop;
      return S;
   end First;

   procedure Next(S: in out Subset) is
      I: Natural := S'Last;
   begin
      if S(I) < Index'Last then
         S(I) := S(I) + 1;
      else
         while S(I-1)+1 = S(I) loop
            I := I - 1;
         end loop;
         S(I-1) := S(I-1) + 1;
         for J in I .. S'Last loop
            S(J) := S(J-1) + 1;
         end loop;
      end if;
      return;
   end Next;

   function Last(S: Subset) return Boolean is
   begin
      return S(S'First) = Index'Last-S'Length+1;
   end Last;

end Iterate_Subsets;
```


{{out}}

```txt
Less or Equal:  87.2 80551
More:           12.8 11827

```




## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      ntreated% = 9
      nplacebo% = 10
      DIM results%(ntreated% + nplacebo% - 1)
      results%() = 85, 88, 75, 66, 25, 29, 83, 39, 97, \    REM treated group
      \            68, 41, 10, 49, 16, 65, 32, 92, 28, 98 : REM placebo group

      greater% = 0
      FOR comb% = 0 TO 2^(ntreated%+nplacebo%)-1
        IF FNnbits(comb%) = ntreated% THEN
          tsum% = 0 : psum% = 0
          FOR b% = 0 TO ntreated%+nplacebo%-1
            IF comb% AND 2^b% THEN
              tsum% += results%(b%)
            ELSE
              psum% += results%(b%)
            ENDIF
          NEXT
          meandiff = tsum%/ntreated% - psum%/nplacebo%
          IF comb% = 2^ntreated% - 1 THEN
            actual = meandiff
          ELSE
            greater% -= meandiff > actual
            groups% += 1
          ENDIF
        ENDIF
      NEXT

      percent = 100 * greater%/groups%
      PRINT "Percentage groupings <= actual experiment: "; 100 - percent
      PRINT "Percentage groupings >  actual experiment: "; percent
      END

      DEF FNnbits(N%)
      N% -= N% >>> 1 AND &55555555
      N% = (N% AND &33333333) + (N% >>> 2 AND &33333333)
      N% = (N% + (N% >>> 4)) AND &0F0F0F0F
      N% += N% >>> 8 : N% += N% >>> 16
      = N% AND &7F
```

{{out}}

```txt

Percentage groupings <= actual experiment: 87.1970296
Percentage groupings >  actual experiment: 12.8029704

```



## C


```c
#include <stdio.h>

int data[] = {  85, 88, 75, 66, 25, 29, 83, 39, 97,
                68, 41, 10, 49, 16, 65, 32, 92, 28, 98 };

int pick(int at, int remain, int accu, int treat)
{
        if (!remain) return (accu > treat) ? 1 : 0;

        return  pick(at - 1, remain - 1, accu + data[at - 1], treat) +
                ( at > remain ? pick(at - 1, remain, accu, treat) : 0 );
}

int main()
{
        int treat = 0, i;
        int le, gt;
        double total = 1;
        for (i = 0; i < 9; i++) treat += data[i];
        for (i = 19; i > 10; i--) total *= i;
        for (i = 9; i > 0; i--) total /= i;

        gt = pick(19, 9, 0, treat);
        le = total - gt;

        printf("<= : %f%%  %d\n > : %f%%  %d\n",
               100 * le / total, le, 100 * gt / total, gt);
        return 0;
}
```

Output:<lang><= : 87.197168%  80551
 > : 12.802832%  11827
```



## C++

This is a translaion of C

```cpp
#include <iostream>
#include <vector>
#include <numeric>
#include <functional>

class
{
public:
    int64_t operator()(int n, int k){ return partial_factorial(n, k) / factorial(n - k);}
private:
    int64_t partial_factorial(int from, int to) { return from == to ? 1 : from * partial_factorial(from - 1, to); }
    int64_t factorial(int n) { return n == 0 ? 1 : n * factorial(n - 1);}
}combinations;

int main()
{
    static constexpr int treatment = 9;
    const std::vector<int> data{ 85, 88, 75, 66, 25, 29, 83, 39, 97,
                                 68, 41, 10, 49, 16, 65, 32, 92, 28, 98 };

    int treated = std::accumulate(data.begin(), data.begin() + treatment, 0);

    std::function<int (int, int, int)> pick;
    pick = [&](int n, int from, int accumulated)
            {
                if(n == 0)
                    return accumulated > treated ? 1 : 0;
                else
                    return pick(n - 1, from - 1, accumulated + data[from - 1]) +
                            (from > n ? pick(n, from - 1, accumulated) : 0);
            };

    int total   = combinations(data.size(), treatment);
    int greater = pick(treatment, data.size(), 0);
    int lesser  = total - greater;

    std::cout << "<= : " << 100.0 * lesser  / total << "%  " << lesser  << std::endl
              << " > : " << 100.0 * greater / total << "%  " << greater << std::endl;
}
```

Output:<lang><= : 87.197168%  80551
 > : 12.802832%  11827
```



## C#

{{trans|Java}}

```cs
using System;
using System.Collections.Generic;

namespace PermutationTest {
    class Program {
        static readonly List<int> DATA = new List<int>{
            85, 88, 75, 66, 25, 29, 83, 39, 97,
            68, 41, 10, 49, 16, 65, 32, 92, 28, 98
        };

        static int Pick(int at, int remain, int accu, int treat) {
            if (remain == 0) {
                return (accu > treat) ? 1 : 0;
            }
            return Pick(at - 1, remain - 1, accu + DATA[at - 1], treat)
                + ((at > remain) ? Pick(at - 1, remain, accu, treat) : 0);
        }

        static void Main() {
            int treat = 0;
            double total = 1.0;
            for (int i = 0; i <= 8; i++) {
                treat += DATA[i];
            }
            for (int i = 19; i >= 11; i--) {
                total *= i;
            }
            for (int i = 9; i >= 1; --i) {
                total /= i;
            }
            int gt = Pick(19, 9, 0, treat);
            int le = (int) (total - gt);
            Console.WriteLine("<= {0}%  {1}", 100.0 * le / total, le);
            Console.WriteLine(" > {0}%  {1}", 100.0 * gt / total, gt);
        }
    }
}
```

{{out}}

```txt
&lt;= 87.1971681569205%  80551
 &gt; 12.8028318430795%  11827
```



## Common Lisp


```lisp
(defun perm-test (s1 s2)
  (let ((more 0) (leq 0)
	(all-data (append s1 s2))
	(thresh (apply #'+ s1)))
    (labels
      ((recur (data sum need avail)
	      (cond ((zerop need)   (if (>= sum thresh)
				      (incf more)
				      (incf leq)))
		    ((>= avail need)
		       (recur (cdr data) sum need (1- avail))
		       (recur (cdr data) (+ sum (car data)) (1- need) (1- avail))))))

      (recur all-data 0 (length s1) (length all-data))
      (cons more leq))))

(let* ((a (perm-test '(68 41 10 49 16 65 32 92 28 98)
		     '(85 88 75 66 25 29 83 39 97)))
       (x (car a))
       (y (cdr a))
       (s (+ x y)))
  (format t "<=: ~a ~6f%~% >: ~a ~6f%~%"
	  x (* 100e0 (/ x s))
	  y (* 100e0 (/ y s))))
```
output<lang><=: 80551 87.197%
 >: 11827 12.803%
```



## D


```d
import std.stdio, std.algorithm, std.array, combinations3;

auto permutationTest(T)(in T[] a, in T[] b) pure nothrow @safe {
    immutable tObs = a.sum;
    auto combs = combinations!false(a ~ b, a.length);
    immutable under = combs.count!(perm => perm.sum <= tObs);
    return under * 100.0 / combs.length;
}

void main() {
    immutable treatmentGroup = [85, 88, 75, 66, 25, 29, 83, 39, 97];
    immutable controlGroup = [68, 41, 10, 49, 16, 65, 32, 92, 28, 98];
    immutable under = permutationTest(treatmentGroup, controlGroup);
    writefln("Under =%6.2f%%\nOver  =%6.2f%%", under, 100.0 - under);
}
```

{{out}}

```txt
Under = 87.20%
Over  = 12.80%
```


Alternative version:
{{trans|C}}

```d
void main() @safe {
    import std.stdio, std.algorithm, std.range;

    immutable treatment = [85, 88, 75, 66, 25, 29, 83, 39, 97];
    immutable control = [68, 41, 10, 49, 16, 65, 32, 92, 28, 98];
    immutable both = treatment ~ control;
    immutable sTreat = treatment.sum;

    T pick(T)(in size_t at, in size_t remain, in T accu) pure nothrow @safe @nogc {
        if (remain == 0)
            return accu > sTreat;

        return pick(at - 1, remain - 1, accu + both[at - 1]) +
               (at > remain ? pick(at - 1, remain, accu) : 0);
    }

    alias mul = reduce!q{a * b};
    immutable t = mul(1.0, iota(both.length, treatment.length + 1, -1))
                  .reduce!q{a / b}(iota(treatment.length, 0, -1));
    immutable gt = pick(both.length, treatment.length, 0);
    immutable le = cast(int)(t - gt);
    writefln(" > : %2.2f%%  %d", 100.0 * gt / t, gt);
    writefln("<= : %2.2f%%  %d", 100.0 * le / t, le);
}
```

{{out}}

```txt
 > : 12.80%  11827
<= : 87.20%  80551
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Permutation do
  def statistic(ab, a) do
    sumab = Enum.sum(ab)
    suma  = Enum.sum(a)
    suma / length(a) - (sumab - suma) / (length(ab) - length(a))
  end

  def test(a, b) do
    ab = a ++ b
    tobs = statistic(ab, a)
    {under, count} = Enum.reduce(comb(ab, length(a)), {0,0}, fn perm, {under, count} ->
      if statistic(ab, perm) <= tobs, do: {under+1, count+1},
                                    else: {under  , count+1}
    end)
    under * 100.0 / count
  end

  defp comb(_, 0), do: [[]]
  defp comb([], _), do: []
  defp comb([h|t], m) do
    (for l <- comb(t, m-1), do: [h|l]) ++ comb(t, m)
  end
end

treatmentGroup = [85, 88, 75, 66, 25, 29, 83, 39, 97]
controlGroup   = [68, 41, 10, 49, 16, 65, 32, 92, 28, 98]
under = Permutation.test(treatmentGroup, controlGroup)
:io.fwrite "under = ~.2f%, over = ~.2f%~n", [under, 100-under]
```


{{out}}

```txt

under = 87.20%, over = 12.80%

```



## GAP


```gap
a := [85, 88, 75, 66, 25, 29, 83, 39, 97];
b := [68, 41, 10, 49, 16, 65, 32, 92, 28, 98];

# Compute a decimal approximation of a rational
Approx := function(x, d)
	local neg, a, b, n, m, s;
	if x < 0 then
		x := -x;
		neg := true;
	else
		neg := false;
	fi;
	a := NumeratorRat(x);
	b := DenominatorRat(x);
	n := QuoInt(a, b);
	a := RemInt(a, b);
	m := 10^d;
	s := "";
	if neg then
		Append(s, "-");
	fi;
	Append(s, String(n));
	n := Size(s) + 1;
	Append(s, String(m + QuoInt(a*m, b)));
	s[n] := '.';
	return s;
end;

PermTest := function(a, b)
	local c, d, p, q, u, v, m, n, k, diff, all;
	p := Size(a);
	q := Size(b);
	v := Concatenation(a, b);
	n := p + q;
	m := Binomial(n, p);
	diff := Sum(a)/p - Sum(b)/q;
	all := [1 .. n];
	k := 0;
	for u in Combinations(all, p) do
		c := List(u, i -> v[i]);
		d := List(Difference(all, u), i -> v[i]);
		if Sum(c)/p - Sum(d)/q > diff then
			k := k + 1;
		fi;
	od;
	return [Approx((1 - k/m)*100, 3), Approx(k/m*100, 3)];
end;

# in order, % less or greater than original diff
PermTest(a, b);
[ "87.197", "12.802" ]
```



## Go

A version doing all math in integers until computing final percentages.

```go
package main

import "fmt"

var tr = []int{85, 88, 75, 66, 25, 29, 83, 39, 97}
var ct = []int{68, 41, 10, 49, 16, 65, 32, 92, 28, 98}

func main() {
    // collect all results in a single list
    all := make([]int, len(tr)+len(ct))
    copy(all, tr)
    copy(all[len(tr):], ct)

    // compute sum of all data, useful as intermediate result
    var sumAll int
    for _, r := range all {
        sumAll += r
    }

    // closure for computing scaled difference.
    // compute results scaled by len(tr)*len(ct).
    // this allows all math to be done in integers.
    sd := func(trc []int) int {
        var sumTr int
        for _, x := range trc {
            sumTr += all[x]
        }
        return sumTr*len(ct) - (sumAll-sumTr)*len(tr)
    }

    // compute observed difference, as an intermediate result
    a := make([]int, len(tr))
    for i, _ := range a {
        a[i] = i
    }
    sdObs := sd(a)

    // iterate over all combinations.  for each, compute (scaled)
    // difference and tally whether leq or gt observed difference.
    var nLe, nGt int
    comb(len(all), len(tr), func(c []int) {
        if sd(c) > sdObs {
            nGt++
        } else {
            nLe++
        }
    })

    // print results as percentage
    pc := 100 / float64(nLe+nGt)
    fmt.Printf("differences <= observed: %f%%\n", float64(nLe)*pc)
    fmt.Printf("differences  > observed: %f%%\n", float64(nGt)*pc)
}

// combination generator, copied from combination task
func comb(n, m int, emit func([]int)) {
    s := make([]int, m)
    last := m - 1
    var rc func(int, int)
    rc = func(i, next int) {
        for j := next; j < n; j++ {
            s[i] = j
            if i == last {
                emit(s)
            } else {
                rc(i+1, j+1)
            }
        }
        return
    }
    rc(0, 0)
}
```

{{out}}

```txt

differences <= observed: 87.197168%
differences  > observed: 12.802832%

```



## Haskell


```haskell
binomial n m = (f !! n) `div` (f !! m) `div` (f !! (n - m))
	where f = scanl (*) 1 [1..]

permtest treat ctrl = (fromIntegral less) / (fromIntegral total) * 100
	where
	total = binomial (length avail) (length treat)
	less  = combos (sum treat) (length treat) avail
	avail = ctrl ++ treat
	combos total n a@(x:xs)
		| total < 0	= binomial (length a) n
		| n == 0	= 0
		| n > length a	= 0
		| n == length a = fromEnum (total < sum a)
		| otherwise	= combos (total - x) (n - 1) xs
				+ combos total n xs

main =	let	r = permtest
			[85, 88, 75, 66, 25, 29, 83, 39, 97]
			[68, 41, 10, 49, 16, 65, 32, 92, 28, 98]
	in do	putStr "> : "; print r
		putStr "<=: "; print $ 100 - r
```

{{out}}

```txt

> : 12.80283184307952
<=: 87.19716815692048

```


Somewhat faster, this goes from top down:

```haskell
binomial n m = (f !! n) `div` (f !! m) `div` (f !! (n - m))
	where f = scanl (*) 1 [1..]

perms treat ctrl = (less,total) where
	total = binomial (length ctrl + length treat) (length treat)
	less = length	$ filter (<= sum treat)
			$ sums (treat ++ ctrl) (length treat)
	sums x n
		| l < n || n < 0 = []
		| n == 0 = [0]
		| l == n = [sum x]
		| otherwise = [a + b | i <- [0..n], a <- sums left i, b <- sums right (n - i)]
			where	(l, l1) = (length x, l `div` 2)
				(left, right) = splitAt l1 x

main = print $ (lt, 100 - lt) where
	(a, b) = perms	[85, 88, 75, 66, 25, 29, 83, 39, 97]
			[68, 41, 10, 49, 16, 65, 32, 92, 28, 98]
	lt = (fromIntegral a) / (fromIntegral b) * 100
```


In cases where the sample data are a large number of relatively small positive integers, counting number of partial sums is a lot faster:

```haskell
combs maxsum len x = foldl f [(0,0,1)] x where
	f a n = merge a (map (addNum n) $ filter (\(l,_,_) -> l < len) a)
	addNum n (a,s,c)
		-- anything larger than maxsum is as good as infinity
		| s + n > maxsum = (a+1, maxsum + 1, c)
		| otherwise = (a+1, s+n, c)

	merge a [] = a
	merge [] a = a
	merge a@((a1,a2,a3):as) b@((b1,b2,b3):bs)
		| a1 == b1 && a2 == b2 = (a1,a2,a3+b3):merge as bs
		| a1 < b1 || (a1 == b1 && a2 < b2) = (a1,a2,a3):merge as b
		| otherwise = (b1,b2,b3):merge a bs

permtest a b = (lt, ge) where
	lt = sum	$ map (\(a,b,c) -> if a == la && b < sa then c else 0)
			$ combs sa la (a++b)
	ge = (binomial (la + lb) la) - lt
	(sa, la, lb) = (sum a, length a, length b)

binomial n m = (f !! n) `div` (f !! m) `div` (f !! (n - m))
	where f = scanl (*) 1 [1..]

-- how many combinations are less than current sum
main =	print$ permtest	[85, 88, 75, 66, 25, 29, 83, 39, 97]
			[68, 41, 10, 49, 16, 65, 32, 92, 28, 98]
```



## J


```j
require'stats'
trmt=: 0.85 0.88 0.75 0.66 0.25 0.29 0.83 0.39 0.97
ctrl=: 0.68 0.41 0.1 0.49 0.16 0.65 0.32 0.92 0.28 0.98
difm=: -&mean
result=: trmt difm ctrl
all=: trmt(#@[ ({. difm }.) |:@([ (comb ~.@,"1 i.@])&# ,) { ,) ctrl
smoutput 'under: ','%',~":100*mean all <: result
smoutput 'over:  ','%',~":100*mean all > result
```


Result:
<lang>under: 87.1972%
over:  12.8028%
```



## Java

{{trans|Kotlin}}

```Java
public class PermutationTest {
    private static final int[] data = new int[]{
        85, 88, 75, 66, 25, 29, 83, 39, 97,
        68, 41, 10, 49, 16, 65, 32, 92, 28, 98
    };

    private static int pick(int at, int remain, int accu, int treat) {
        if (remain == 0) return (accu > treat) ? 1 : 0;
        return pick(at - 1, remain - 1, accu + data[at - 1], treat)
            + ((at > remain) ? pick(at - 1, remain, accu, treat) : 0);
    }

    public static void main(String[] args) {
        int treat = 0;
        double total = 1.0;
        for (int i = 0; i <= 8; ++i) {
            treat += data[i];
        }
        for (int i = 19; i >= 11; --i) {
            total *= i;
        }
        for (int i = 9; i >= 1; --i) {
            total /= i;
        }
        int gt = pick(19, 9, 0, treat);
        int le = (int) (total - gt);
        System.out.printf("<= : %f%%  %d\n", 100.0 * le / total, le);
        System.out.printf(" > : %f%%  %d\n", 100.0 * gt / total, gt);
    }
}
```

{{out}}

```txt
<= : 87.197168%  80551
 > : 12.802832%  11827
```



## jq

{{works with|jq|1.4}}
'''Part 1: Combinations'''

```jq
# combination(r) generates a stream of combinations of r items from the input array.
def combination(r):
  if r > length or r < 0 then empty
  elif r == length then .
  else  ( [.[0]] + (.[1:]|combination(r-1))),
        ( .[1:]|combination(r))
  end;

```

'''Part 2: Permutation Test'''

```jq
# a and b should be arrays:
def permutationTest(a; b):

  def normalize(a;b):  # mainly to avoid having to compute $sumab
    (a|add) as $sa
    | (b|add) as $sb
    | (($sa + $sb)/((a|length) + (b|length))) as $avg
    | [(a | map(.-$avg)), (b | map(.-$avg))];

  # avg(a) - avg(b) (assuming ab==a+b and avg(ab) is 0)
  def statistic(ab; a):
    (a | add) as $suma
    # (ab|add) should be 0, by normalization
    | ($suma / (a|length)) +
      ($suma / ((ab|length) - (a|length)));

  normalize(a;b)
  | (a + b) as $ab                               # pooled observations
  | .[0] as $a | .[1] as $b
  | statistic($ab; $a) as $t_observed            # observed difference in means
  | reduce ($ab|combination($a|length)) as $perm # for each combination...
      ([0,0];                                    # state: [under,count]
       if statistic($ab; $perm) <= $t_observed then .[0] += 1 else . end
       | .[1] += 1 )
  | .[0] * 100.0 / .[1]                         # under/count
;
```

'''Example:'''

```jq
def treatmentGroup: [85, 88, 75, 66, 25, 29, 83, 39, 97];
def controlGroup: [68, 41, 10, 49, 16, 65, 32, 92, 28, 98];

permutationTest(treatmentGroup; controlGroup) as $under
| "% under=\($under)", "% over=\(100 - $under)"
```

{{out}}
 $ jq -n -r -f permutation_test.jq
 % under=87.14304271579813
 % over=12.856957284201869


## Julia

{{works with|Julia|0.6}}
The primary function for this solution is <tt>permutation_test</tt>, which relies on Julia's <tt>combinations</tt> (from <tt>Combinatorics</tt> module) function to provide all of the possible study arm assignments.  <tt>bifurcate</tt> splits the pooled results into "treatment" and "control" groups according to the indices provided by <tt>combinations</tt>.

'''Functions'''

```julia
using Combinatorics

meandiff(a::Vector{T}, b::Vector{T}) where T <: Real = mean(a) - mean(b)

function bifurcate(a::AbstractVector, sel::Vector{T}) where T <: Integer
    x         = a[sel]
    asel      = trues(length(a))
    asel[sel] = false
    y         = a[asel]
    return x, y
end

function permutation_test(treated::Vector{T}, control::Vector{T}) where T <: Real
    effect0 = meandiff(treated, control)
    pool    = vcat(treated, control)
    tlen    = length(treated)
    plen    = length(pool)
    better = worse = 0
    for subset in combinations(1:plen, tlen)
        t, c = bifurcate(pool, subset)
        if effect0 < meandiff(t, c)
            better += 1
        else
            worse += 1
        end
    end
    return better, worse
end
```


'''Main'''

```julia
const treated = [85, 88, 75, 66, 25, 29, 83, 39, 97]
const control = [68, 41, 10, 49, 16, 65, 32, 92, 28, 98]

(better, worse) = permutation_test(treated, control)

tot = better + worse

println("Permutation test using the following data:")
println("Treated:  ", treated)
println("Control:  ", control)
println("\nThere are $tot different permuted groups of these data.")
@printf("%8d, %5.2f%% showed better than actual results.\n", better, 100 * better / tot)
print(@sprintf("%8d, %5.2f%% showed equalivalent or worse results.", worse, 100 * worse / tot))
```


{{out}}

```txt
Permutation test using the following data:
Treated:  [85, 88, 75, 66, 25, 29, 83, 39, 97]
Control:  [68, 41, 10, 49, 16, 65, 32, 92, 28, 98]

There are 92378 different permuted groups of these data.
   11827, 12.80% showed better than actual results.
   80551, 87.20% showed equalivalent or worse results.
```



## Kotlin

{{trans|C}}

```scala
// version 1.1.2

val data = intArrayOf(
    85, 88, 75, 66, 25, 29, 83, 39, 97,
    68, 41, 10, 49, 16, 65, 32, 92, 28, 98
)

fun pick(at: Int, remain: Int, accu: Int, treat: Int): Int {
    if (remain == 0) return if (accu > treat) 1 else 0
    return pick(at - 1, remain - 1, accu + data[at - 1], treat) +
           if (at > remain) pick(at - 1, remain, accu, treat) else 0
}

fun main(args: Array<String>) {
    var treat = 0
    var total = 1.0
    for (i in 0..8) treat += data[i]
    for (i in 19 downTo 11) total *= i
    for (i in 9 downTo 1) total /= i
    val gt = pick(19, 9, 0, treat)
    val le = (total - gt).toInt()
    System.out.printf("<= : %f%%  %d\n", 100.0 * le / total, le)
    System.out.printf(" > : %f%%  %d\n", 100.0 * gt / total, gt)
}
```


{{out}}

```txt

<= : 87.197168%  80551
 > : 12.802832%  11827

```


## M2000 Interpreter

{{trans|C}}

```M2000 Interpreter

Module Checkit {
      Global data(), treat=0
      data()=(85, 88, 75, 66, 25, 29, 83, 39, 97,68, 41, 10, 49, 16, 65, 32, 92, 28, 98)
      Function pick(at, remain, accu) {
            If remain Else =If(accu>treat->1,0):Exit
            =pick(at-1,remain-1,accu+data(at-1))+If(at>remain->pick(at-1, remain, accu),0)
      }
      total=1
      For i=0 to 8 {treat+=data(i)}
      For i=19 to 11 {total*=i}
      For i=9 to 1 {total/=i}
      gt=pick(19,9,0)
      le=total-gt
      Print Format$("<= : {0:1}% {1}", 100*le/total, le)
      Print Format$(" > : {0:1}% {1}", 100*gt/total, gt)
}
Checkit

```

{{out}}

```txt
<= : 87.2%  80551
 > : 12.8%  11827

```


Slower version, using a lambda function with a series of inner lambda functions to return each combination at a time.


```M2000 Interpreter

Module CheckThis {
      Function CombinationsStep (a, nn) {
            c1=lambda (&f, &a) ->{=car(a) : a=cdr(a) : f=len(a)=0}
            m=len(a)
            c=c1
            n=m-nn+1
            p=2
            while m>n {
            c1=lambda c2=c,n=p, z=(,) (&f, &m) ->{if len(z)=0 then z=cdr(m)
                  =cons(car(m),c2(&f, &z)):if f then z=(,) : m=cdr(m) : f=len(m)+len(z)<n
             }
            c=c1
            p++
            m--
            }
            =lambda c, a (&f) ->c(&f, &a)
      }
      treated=(85, 88, 75, 66, 25, 29, 83, 39, 97)
      placebo=(68, 41, 10, 49, 16, 65, 32, 92, 28, 98)
      treat=0
      m=each(treated): while m {treat+=array(m)}
      total=1
      for i=len(placebo)+1 to len(placebo) +len(treated):total*=i:next i
      for i=len(placebo)-1 to 1: total/=i:next i
      d=total div 10**int(log(total))
      k=false
      StepA=CombinationsStep(cons(treated, placebo),len(treated))
      counter=0
      gt=0
      While not k {
            comb=StepA(&k)
            accu=0
            m=each(comb)
            while m {accu+=array(m)}
            gt+=if(accu>treat->1,0)
            counter++
            if counter mod d=0 then Print over str$(counter/total," #0.0%"): Refresh 1000
      }
      print over str$(counter/total," #0.0%")
      print
      lt=total-gt
      print Format$("less or equal={0:1}%, greater={1:1}%, total={2}",lt/total*100, gt/total*100, total)
}
CheckThis

```



## Mathematica


```mathematica
"<=: " <> ToString[#1] <> " " <> ToString[100. #1/#2] <> "%\n >: " <>
     ToString[#2 - #1] <> " " <> ToString[100. (1 - #1/#2)] <> "%" &[
   Count[Total /@ Subsets[Join[#1, #2], {Length@#1}],
    n_ /; n <= Total@#1],
   Binomial[Length@#1 + Length@#2, Length@#1]] &[{85, 88, 75, 66, 25,
  29, 83, 39, 97}, {68, 41, 10, 49, 16, 65, 32, 92, 28, 98}]
```

{{out}}

```txt
<=: 80551 87.1972%
 >: 11827 12.8028%
```



## Perl


```perl
#!/usr/bin/perl
use warnings;
use strict;

use List::Util qw{ sum };


sub means {
    my @groups = @_;
    return map sum(@$_) / @$_, @groups;
}


sub following {
    my $pattern    = shift;
    my $orig_count = grep $_, @$pattern;
    my $count;
    do {
        my $i = $#{$pattern};
        until (0 > $i) {
            $pattern->[$i] = $pattern->[$i] ? 0 : 1;
            last if $pattern->[$i];
            --$i;
        }
        $count = grep $_, @$pattern;
    } until $count == $orig_count or not $count;
    undef @$pattern unless $count;
}


my @groups;
my $i = 0;
while (<DATA>) {
    chomp;
    $i++, next if /^$/;
    push @{ $groups[$i] }, $_;
}

my @orig_means = means(@groups);
my $orig_cmp   = $orig_means[0] - $orig_means[1];

my $pattern = [ (0) x @{ $groups[0] },
                (1) x @{ $groups[1] }
              ];

my @cmp = (0) x 3;
while (@$pattern) {
    my @perms = map { my $g = $_;
                      [ (@{ $groups[0] }, @{ $groups[1] } ) [ grep $pattern->[$_] == $g, 0 .. $#{$pattern} ] ];
                  } 0, 1;
    my @means = means(@perms);
    $cmp[ ($means[0] - $means[1]) <=> $orig_cmp ]++;
} continue {
    following($pattern);
}
my $all    = sum(@cmp);
my $length = length $all;
for (0, -1, 1) {
    printf "%-7s %${length}d %6.3f%%\n",
        (qw(equal greater less))[$_], $cmp[$_], 100 * $cmp[$_] / $all;
}


__DATA__
85
88
75
66
25
29
83
39
97

68
41
10
49
16
65
32
92
28
98
```

{{out}}
 equal     313  0.339%
 less    80238 86.858%
 greater 11827 12.803%


## Perl 6

The use of <code>.race</code> to allow concurrent calculations means that multiple 'workers' will be updating <code>@trials</code> simultaneously. To avoid race conditions, the <code>⚛++</code> operator is used, which guarantees safe updates without the use of locks. That is turn requires declaring that array as being composed of <code>atomicint</code>.
{{works with|Rakudo|2018.09}}


```perl6
sub stats ( @test, @all ) {
    ([+] @test / +@test) - ([+] flat @all, (@test X* -1)) / @all - @test
}

my int @treated = <85 88 75 66 25 29 83 39 97>;
my int @control = <68 41 10 49 16 65 32 92 28 98>;
my int @all = flat @treated, @control;

my $base = stats( @treated, @all );

my atomicint @trials[3] = 0, 0, 0;

@all.combinations(+@treated).race.map: { @trials[ 1 + ( stats( $_, @all ) <=> $base ) ]⚛++ }

say 'Counts: <, =, > ', @trials;
say 'Less than    : %', 100 * @trials[0] / [+] @trials;
say 'Equal to     : %', 100 * @trials[1] / [+] @trials;
say 'Greater than : %', 100 * @trials[2] / [+] @trials;
say 'Less or Equal: %', 100 * ( [+] @trials[0,1] ) / [+] @trials;
```


{{out}}

```txt

Counts: <, =, > 80238 313 11827
Less than    : %86.858343
Equal to     : %0.338825
Greater than : %12.802832
Less or Equal: %87.197168

```



## Phix

{{Trans|C}}

```Phix
constant data = {85, 88, 75, 66, 25, 29, 83, 39, 97,
                 68, 41, 10, 49, 16, 65, 32, 92, 28, 98 }

function pick(int at, int remain, int accu, int treat)
    if remain=0 then return iff(accu>treat?1:0) end if
    return pick(at-1, remain-1, accu+data[at], treat) +
           iff(at>remain?pick(at-1, remain, accu, treat):0)
end function

int treat = 0, le, gt
atom total = 1;
for i=1 to 9 do treat += data[i] end for
for i=19 to 11 by -1 do total *= i end for
for i=9 to 1 by -1 do total /= i end for

gt = pick(19, 9, 0, treat)
le = total - gt;

printf(1,"<= : %f%%  %d\n > : %f%%  %d\n",
       {100*le/total, le, 100*gt/total, gt})
```

{{out}}

```txt

<= : 87.197168%  80551
 > : 12.802832%  11827

```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")  # For 'subsets'

(scl 2)

(de _stat (A)
   (let (LenA (length A)  SumA (apply + A))
      (-
         (*/ SumA LenA)
         (*/ (- SumAB SumA) (- LenAB LenA)) ) ) )

(de permutationTest (A B)
   (let
      (AB (append A B)
         SumAB (apply + AB)
         LenAB (length AB)
         Tobs (_stat A)
         Count 0 )
      (*/
         (sum
            '((Perm)
               (inc 'Count)
               (and (>= Tobs (_stat Perm)) 1) )
            (subsets (length A) AB) )
         100.0
         Count ) ) )

(setq
   *TreatmentGroup (0.85 0.88 0.75 0.66 0.25 0.29 0.83 0.39 0.97)
   *ControlGroup   (0.68 0.41 0.10 0.49 0.16 0.65 0.32 0.92 0.28 0.98) )

(let N (permutationTest *TreatmentGroup *ControlGroup)
   (prinl "under = " (round N) "%, over = " (round (- 100.0 N)) "%") )
```

{{out}}

```txt
under = 87.85%, over = 12.15%
```



## PureBasic

Given a treatment group with [n=9] and a control group with [m=10].
The numbers [x] from [1] to [1<<(n+m)] exhaust the possible states.

Any bit-String of Length [n+m] containing [n=9] "1's" is a Valid bit String,
as tested by: IsValidBitString(x,n+m,n).

Then we can use these bits to Select whether a particular index
For our array should be assigned to:
the treatment group or the control group


```PureBasic


Define.f meanTreated,meanControl,diffInMeans
Define.f actualmeanTreated,actualmeanControl,actualdiffInMeans

Dim poolA(19)

poolA(1) =85 ; first 9 the treated
poolA(2) =88
poolA(3) =75
poolA(4) =66
poolA(5) =25
poolA(6) =29
poolA(7) =83
poolA(8) =39
poolA(9) =97

poolA(10) =68 ; last 10 the control
poolA(11) =41
poolA(12) =10
poolA(13) =49
poolA(14) =16
poolA(15) =65
poolA(16) =32
poolA(17) =92
poolA(18) =28
poolA(19) =98

Procedure.i IsValidBitString(x,pool,treated)
Protected c,i
For i=1 to pool
mask=1<<(i-1)
If mask&x:c+1:EndIf
Next
If c=treated :ProcedureReturn x
Else         :ProcedureReturn 0
EndIf
EndProcedure

treated=9
control=10

pool   =treated+control

; actual Experimentally observed difference in means

For i=1 to Treated
sumTreated+poolA(i)
Next
For i=Treated+1 to Treated+Control
sumControl+poolA(i)
Next

actualmeanTreated=sumTreated /Treated
actualmeanControl=sumControl /Control
actualdiffInMeans=actualmeanTreated-actualmeanControl

; exhaust the possibilites
For x=1 to 1<<pool

; Valid? i.e. are there 9 "1's" ?
If IsValidBitString(x,pool,treated)
TotalComBinations+1:sumTreated=0:sumControl=0

; separate the groups
For i=pool to 1 Step -1
mask=1<<(i-1):idx=pool-i+1
If mask&x
sumTreated+poolA(idx)
Else
sumControl+poolA(idx)
EndIf
Next

meanTreated=sumTreated /Treated
meanControl=sumControl /Control
diffInMeans=meanTreated-meanControl
; gather the statistics
If (diffInMeans)<=(actualdiffInMeans)
diffLessOrEqual+1
Else
diffGreater+1
EndIf

EndIf
Next
; show our results
; cw(StrF(100*diffLessOrEqual/TotalComBinations,2)+" "+Str(diffLessOrEqual))
; cw(StrF(100*diffGreater    /TotalComBinations,2)+" "+Str(diffGreater))

Debug StrF(100*diffLessOrEqual/TotalComBinations,2)+" "+Str(diffLessOrEqual)
Debug StrF(100*diffGreater    /TotalComBinations,2)+" "+Str(diffGreater)

```


{{out}}

```txt

87.20 80551
12.80 11827

```




## Python

{{trans|Tcl}}

```python
from itertools import combinations as comb

def statistic(ab, a):
    sumab, suma = sum(ab), sum(a)
    return ( suma / len(a) -
             (sumab -suma) / (len(ab) - len(a)) )

def permutationTest(a, b):
    ab = a + b
    Tobs = statistic(ab, a)
    under = 0
    for count, perm in enumerate(comb(ab, len(a)), 1):
        if statistic(ab, perm) <= Tobs:
            under += 1
    return under * 100. / count

treatmentGroup = [85, 88, 75, 66, 25, 29, 83, 39, 97]
controlGroup   = [68, 41, 10, 49, 16, 65, 32, 92, 28, 98]
under = permutationTest(treatmentGroup, controlGroup)
print("under=%.2f%%, over=%.2f%%" % (under, 100. - under))
```

{{out}}

```txt
under=89.11%, over=10.89%
```


The above solution does a different thing than the other solutions. I'm not really sure why. If you want to do the same thing as the other solutions, then this is the solution:

```python
from itertools import combinations as comb

def permutationTest(a, b):
    ab = a + b
    Tobs = sum(a)
    under = 0
    for count, perm in enumerate(comb(ab, len(a)), 1):
        if sum(perm) <= Tobs:
            under += 1
    return under * 100. / count

treatmentGroup = [85, 88, 75, 66, 25, 29, 83, 39, 97]
controlGroup   = [68, 41, 10, 49, 16, 65, 32, 92, 28, 98]
under = permutationTest(treatmentGroup, controlGroup)
print("under=%.2f%%, over=%.2f%%" % (under, 100. - under))
```

{{out}}

```txt
under=87.20%, over=12.80%
```



## R



```r
permutation.test <- function(treatment, control) {
  perms <- combinations(length(treatment)+length(control),
                        length(treatment),
                        c(treatment, control),
                        set=FALSE)
  p <- mean(rowMeans(perms) <= mean(treatment))
  c(under=p, over=(1-p))
}
```



```r>
 permutation.test(c(85, 88, 75, 66, 25, 29, 83, 39, 97),
+                  c(68, 41, 10, 49, 16, 65, 32, 92, 28, 98))
    under      over
0.8719717 0.1280283
```




## Racket

{{trans|Common Lisp}}


```Racket
#lang racket/base

(define-syntax-rule (inc! x)
  (set! x (add1 x)))

(define (permutation-test control-gr treatment-gr)
  (let ([both-gr (append control-gr treatment-gr)]
        [threshold (apply + control-gr)]
        [more 0]
        [leq 0])
    (let loop ([data both-gr] [sum 0] [needed (length control-gr)] [available (length both-gr)])
      (cond [(zero? needed) (if (>= sum threshold)
                                (inc! more)
                                (inc! leq))]
            [(>= available needed) (loop (cdr data) sum needed (sub1 available))
                                   (loop (cdr data) (+ sum (car data)) (sub1 needed) (sub1 available))]
            [else (void)]))
    (values more leq)))

(let-values ([(more leq) (permutation-test '(68 41 10 49 16 65 32 92 28 98)
                                           '(85 88 75 66 25 29 83 39 97))])
  (let ([sum (+ more leq)])
    (printf "<=: ~a ~a%~n>:  ~a ~a%~n"
            more (real->decimal-string (* 100. (/ more sum)) 2)
            leq (real->decimal-string (* 100. (/ leq sum)) 2))))

```


{{out}}

```txt

<=: 80551 87.20%
>:  11827 12.80%

```



## REXX

This REXX program is modeled after the   '''C'''   version, with some generalizations and optimization added.

```rexx
/*REXX program  performs a    permutation test   on     N + M   subjects  (volunteers): */
                                                 /*     ↑   ↑                           */
                                                 /*     │   │                           */
                                                 /*     │   └─────control  population.  */
                                                 /*     └────────treatment population.  */
n= 9                                             /*define the number of the control pop.*/
data= 85 88 75 66 25 29 83 39 97         68 41 10 49 16 65 32 92 28 98
w= words(data);      m= w - n                    /*w:  volunteers  +  control population*/
L= length(w)
say '# of volunteers & control population: '             w
say 'volunteer population given treatment: '             right(n, L)
say ' control  population given a placebo: '             right(m, L)
say
say 'treatment population efficacy % (percentages): '    subword(data, 1, n)
say ' control  population placebo  % (percentages): '    subword(data, n+1 )
say
                     do v=  0  for w         ;           #.v= word(data, v+1) ;       end
treat= 0;            do i=  0  to n-1        ;           treat= treat + #.i   ;       end
  tot= 1;            do j=  w  to m+1  by -1 ;           tot= tot * j         ;       end
                     do k=w%2  to  1   by -1 ;           tot= tot / k         ;       end

GT= picker(n+m, n, 0)                            /*compute the GT value from PICKER func*/
LE= tot - GT                                     /*   "     "  LE   "   via subtraction.*/
say "<= "  format(100 * LE / tot, ,3)'%'   LE    /*display number with 3 decimal places.*/
say " > "  format(100 * GT / tot, ,3)'%'   GT    /*   "       "     "  "    "       "   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
picker:  procedure expose #. treat;        parse arg it,rest,eff    /*get the arguments.*/
         if rest==0  then return   eff > treat                      /*is REST = to zero?*/
         if it>rest  then q= picker(it-1, rest, eff)                /*maybe recurse.    */
                     else q= 0
         itP= it - 1                                                /*set temporary var.*/
         return picker(itP,  rest - 1,  eff + #.itP)  +  q          /*recurse.          */
```

{{out|output|text=  when using the default input:}}

```txt

# of volunteers & control population:  19
volunteer population given treatment:   9
 control  population given a placebo:  10

treatment population efficacy % (percentages):  85 88 75 66 25 29 83 39 97
 control  population placebo  % (percentages):  68 41 10 49 16 65 32 92 28 98

<=  87.197% 80551
 >  12.803% 11827

```



## Ruby

{{trans|Python}}

```ruby
def statistic(ab, a)
  sumab, suma = ab.inject(:+).to_f, a.inject(:+).to_f
  suma / a.size - (sumab - suma) / (ab.size - a.size)
end

def permutationTest(a, b)
  ab = a + b
  tobs = statistic(ab, a)
  under = count = 0
  ab.combination(a.size) do |perm|
    under += 1 if statistic(ab, perm) <= tobs
    count += 1
  end
  under * 100.0 / count
end

treatmentGroup = [85, 88, 75, 66, 25, 29, 83, 39, 97]
controlGroup   = [68, 41, 10, 49, 16, 65, 32, 92, 28, 98]
under = permutationTest(treatmentGroup, controlGroup)
puts "under=%.2f%%, over=%.2f%%" % [under, 100 - under]
```


{{out}}

```txt

under=87.20%, over=12.80%

```



## Scala

===Imperative version (Ugly, side effects)===
{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/69o8tJX/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/jpUyNPetRXabZkEKc68FAA Scastie (remote JVM)].

```Scala
object PermutationTest extends App {
  private val data =
    Array(85, 88, 75, 66, 25, 29, 83, 39, 97, 68, 41, 10, 49, 16, 65, 32, 92, 28, 98)
  private var (total, treat) = (1.0, 0)

  private def pick(at: Int, remain: Int, accu: Int, treat: Int): Int = {
    if (remain == 0) return if (accu > treat) 1 else 0

    pick(at - 1, remain - 1, accu + data(at - 1), treat) +
      (if (at > remain) pick(at - 1, remain, accu, treat) else 0)
  }

  for (i <- 0 to 8) treat += data(i)
  for (j <- 19 to 11 by -1) total *= j
  for (g <- 9 to 1 by -1) total /= g

  private val gt = pick(19, 9, 0, treat)
  private val le = (total - gt).toInt

  println(f"<= : ${100.0 * le / total}%f%%  ${le}%d")
  println(f" > : ${100.0 * gt / total}%f%%  ${gt}%d")

}
```


## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const array integer: treatmentGroup is [] (85, 88, 75, 66, 25, 29, 83, 39, 97);
const array integer: controlGroup  is [] (68, 41, 10, 49, 16, 65, 32, 92, 28, 98);
const array integer: both is treatmentGroup & controlGroup;

const func integer: pick (in integer: at, in integer: remain, in integer: accu, in integer: treat) is func
  result
    var integer: picked is 0;
  begin
    if remain = 0 then
      picked := ord(accu > treat);
    else
      picked := pick(at - 1, remain - 1, accu + both[at], treat);
      if at > remain then
        picked +:= pick(at - 1, remain, accu, treat);
      end if;
    end if;
  end func;

const proc: main is func
  local
    var integer: experimentalResult is 0;
    var integer: treat is 0;
    var integer: total is 1;
    var integer: le is 0;
    var integer: gt is 0;
    var integer: i is 0;
  begin
    for experimentalResult range treatmentGroup do
      treat +:= experimentalResult;
    end for;
    total := 19 ! 10;  # Binomial coefficient
    gt := pick(19, 9, 0, treat);
    le := total - gt;
    writeln("<= : " <& 100.0 * flt(le) / flt(total) digits 6 <& "%  " <& le);
    writeln(" > : " <& 100.0 * flt(gt) / flt(total) digits 6 <& "%  " <& gt);
  end func;
```

{{out}}
 <= : 87.197168%  80551
  > : 12.802832%  11827


## Sidef

{{trans|Ruby}}

```ruby
func statistic(ab, a) {
    var(sumab, suma) = (ab.sum, a.sum)
    suma/a.size - ((sumab-suma) / (ab.size-a.size))
}

func permutationTest(a, b) {
    var ab = (a + b)
    var tobs = statistic(ab, a)
    var under = (var count = 0)
    ab.combinations(a.len, {|*perm|
        statistic(ab, perm) <= tobs && (under += 1)
        count += 1
    })
    under * 100 / count
}

var treatmentGroup = [85, 88, 75, 66, 25, 29, 83, 39, 97]
var controlGroup   = [68, 41, 10, 49, 16, 65, 32, 92, 28, 98]
var under = permutationTest(treatmentGroup, controlGroup)
say ("under=%.2f%%, over=%.2f%%" % (under, 100 - under))
```

{{out}}

```txt

under=87.20%, over=12.80%

```



## Tcl


```tcl
package require Tcl 8.5

# Difference of means; note that the first list must be the concatenation of
# the two lists (because this is cheaper to work with).
proc statistic {AB A} {
    set sumAB [tcl::mathop::+ {*}$AB]
    set sumA [tcl::mathop::+ {*}$A]
    expr {
	$sumA / double([llength $A]) -
	($sumAB - $sumA) / double([llength $AB] - [llength $A])
    }
}

# Selects all k-sized combinations from a list.
proc selectCombinationsFrom {k l} {
    if {$k == 0} {return {}} elseif {$k == [llength $l]} {return [list $l]}
    set all {}
    set n [expr {[llength $l] - [incr k -1]}]
    for {set i 0} {$i < $n} {} {
        set first [lindex $l $i]
	incr i
        if {$k == 0} {
            lappend all $first
	} else {
	    foreach s [selectCombinationsFrom $k [lrange $l $i end]] {
		lappend all [list $first {*}$s]
	    }
        }
    }
    return $all
}

# Compute the permutation test value and its complement.
proc permutationTest {A B} {
    set whole [concat $A $B]
    set Tobs [statistic $whole $A]
    set undercount 0
    set overcount 0
    set count 0
    foreach perm [selectCombinationsFrom [llength $A] $whole] {
	set t [statistic $whole $perm]
	incr count
	if {$t <= $Tobs} {incr undercount} else {incr overcount}
    }
    set count [tcl::mathfunc::double $count]
    list [expr {$overcount / $count}] [expr {$undercount / $count}]
}
```

Demonstration code:

```tcl
set treatmentGroup {0.85 0.88 0.75 0.66 0.25 0.29 0.83 0.39 0.97}
set controlGroup   {0.68 0.41 0.10 0.49 0.16 0.65 0.32 0.92 0.28 0.98}
lassign [permutationTest $treatmentGroup $controlGroup] over under
puts [format "under=%.2f%%, over=%.2f%%" [expr {$under*100}] [expr {$over*100}]]
```

{{out}}

```txt
under=86.90%, over=13.10%
```



## Ursala


```Ursala
#import std
#import nat
#import flo

treatment_group = <85,88,75,66,25,29,83,39,97>
control_group   = <68,41,10,49,16,65,32,92,28,98>

f = # returns the fractions of alternative mean differences above and below the actual

float~*; -+
   vid^~G(plus,~&)+ (not fleq@rlX)*|@htX; ~~ float+ length,
   minus*+ mean^~*C/~& ^DrlrjXS(~&l,choices)^/-- length@l+-

#show+

t = --* *-'%'@lrNCC printf/$'%0.2f' times/$100. f(treatment_group,control_group)
```

{{out}}

```txt

12.80%
87.20%
```



## zkl

A solution that is not going to scale gracefully at all.
{{trans|D}}

```zkl
fcn permutationTest(a,b){
   ab    := a.extend(b);
   tObs  := a.sum(0);
   combs := Utils.Helpers.pickNFrom(a.len(),ab);  // 92,378
   under := combs.reduce('wrap(sum,perm){ sum+(perm.sum(0) <= tObs) },0);
   100.0 * under / combs.len();
}

treatmentGroup := T(85, 88, 75, 66, 25, 29, 83, 39, 97);
controlGroup   := T(68, 41, 10, 49, 16, 65, 32, 92, 28, 98);
under          := permutationTest(treatmentGroup, controlGroup);
println("Under =%6.2f%%\nOver  =%6.2f%%".fmt(under, 100.0 - under));
```

{{out}}

```txt

Under = 87.20%
Over  = 12.80%

```

