+++
title = "Sum and Product Puzzle"
description = ""
date = 2019-08-28T15:31:17Z
aliases = []
[extra]
id = 19053
[taxonomies]
categories = ["task"]
tags = []
+++

Solve the "<i>Impossible Puzzle</i>":

{{quote|
X and Y are two different whole numbers greater than 1. Their sum is no greater than 100, and Y is greater than X. S and P are two mathematicians (and consequently perfect logicians); S knows the sum X+Y and P knows the product X*Y. Both S and P know all the information in this paragraph.

The following conversation occurs:

* S says "P does not know X and Y."
* P says "Now I know X and Y."
* S says "Now I also know X and Y!"

What are X and Y?
}}

It can be hard to wrap one's head around what the three lines of dialog between S (the "sum guy") and P (the "product guy") convey about the values of X and Y.

So for your convenience, here's a break-down:

{| class="wikitable"
|-
!
! Quote
! Implied fact
|-
! 1)
| S says "P does not know X and Y."
| For every possible sum decomposition of the number <tt>X+Y</tt>, the product has in turn ''more than one'' product decomposition.
|-
! 2)
| P says "Now I know X and Y."
| The number <tt>X*Y</tt> has ''only one'' product decomposition for which fact 1 is true.
|-
! 3)
| S says "Now I also know X and Y."
| The number <tt>X+Y</tt> has ''only one'' sum decomposition for which fact 2 is true.
|}

Terminology:
* ''"sum decomposition" of a number'' = Any pair of positive integers <tt>(A, B)</tt> so that <tt>A+B</tt> equals the number. Here, with the additional constraint <tt>2 ≤ A < B</tt>.
* ''"product decomposition" of a number'' = Any pair of positive integers <tt>(A, B)</tt> so that <tt>A*B</tt> equals the number. Here, with the additional constraint <tt>2 ≤ A < B</tt>.



Your program can solve the puzzle by considering all possible pairs <tt>(X, Y)</tt> in the range <tt>2 ≤ X < Y ≤ 98</tt>, and then successively eliminating candidates based on the three facts. It turns out only one solution remains!

See the [[#Python|Python example]] for an implementation that uses this approach with a few optimizations.

*   Wikipedia:   [[wp:Sum and Product Puzzle|Sum and Product Puzzle]]
<hr>


## AWK


```AWK

# syntax: GAWK -f SUM_AND_PRODUCT_PUZZLE.AWK
BEGIN {
    for (s=2; s<=100; s++) {
      if ((a=satisfies_statement3(s)) != 0) {
        printf("%d (%d+%d)\n",s,a,s-a)
      }
    }
    exit(0)
}
function satisfies_statement1(s,  a) { # S says: P does not know the two numbers.
# Given s, for all pairs (a,b), a+b=s, 2 <= a,b <= 99, true if at least one of a or b is composite
    for (a=2; a<=int(s/2); a++) {
      if (is_prime(a) && is_prime(s-a)) {
        return(0)
      }
    }
    return(1)
}
function satisfies_statement2(p,  i,j,winner) { # P says: Now I know the two numbers.
# Given p, for all pairs (a,b), a*b=p, 2 <= a,b <= 99, true if exactly one pair satisfies statement 1
    for (i=2; i<=int(sqrt(p)); i++) {
      if (p % i == 0) {
        j = int(p/i)
        if (!(2 <= j && j <= 99)) { # in range
          continue
        }
        if (satisfies_statement1(i+j)) {
          if (winner) {
            return(0)
          }
          winner = 1
        }
      }
    }
    return(winner)
}
function satisfies_statement3(s,  a,b,winner) { # S says: Now I know the two numbers.
# Given s, for all pairs (a,b), a+b=s, 2 <= a,b <= 99, true if exactly one pair satisfies statements 1 and 2
    if (!satisfies_statement1(s)) {
      return(0)
    }
    for (a=2; a<=int(s/2); a++) {
      b = s - a
      if (satisfies_statement2(a*b)) {
        if (winner) {
          return(0)
        }
        winner = a
      }
    }
    return(winner)
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

<p>Output:</p>

```txt

17 (4+13)

```


## C#


```c#
using System;
using System.Linq;
using System.Collections.Generic;

public class Program
{
    public static void Main()
    {
        const int maxSum = 100;
        var pairs = (
            from X in 2.To(maxSum / 2 - 1)
            from Y in (X + 1).To(maxSum - 2).TakeWhile(y => X + y <= maxSum)
            select new { X, Y, S = X + Y, P = X * Y }
            ).ToHashSet();

        Console.WriteLine(pairs.Count);

        var uniqueP = pairs.GroupBy(pair => pair.P).Where(g => g.Count() == 1).Select(g => g.Key).ToHashSet();

        pairs.ExceptWith(pairs.GroupBy(pair => pair.S).Where(g => g.Any(pair => uniqueP.Contains(pair.P))).SelectMany(g => g));
        Console.WriteLine(pairs.Count);

        pairs.ExceptWith(pairs.GroupBy(pair => pair.P).Where(g => g.Count() > 1).SelectMany(g => g));
        Console.WriteLine(pairs.Count);

        pairs.ExceptWith(pairs.GroupBy(pair => pair.S).Where(g => g.Count() > 1).SelectMany(g => g));
        Console.WriteLine(pairs.Count);

        foreach (var pair in pairs) Console.WriteLine(pair);
    }
}

public static class Extensions
{
    public static IEnumerable<int> To(this int start, int end) {
        for (int i = start; i <= end; i++) yield return i;
    }

    public static HashSet<T> ToHashSet<T>(this IEnumerable<T> source) => new HashSet<T>(source);
}
```

```txt

2352
145
86
1
{ X = 4, Y = 13, S = 17, P = 52 }

```



## Common Lisp


### Version 1


```lisp

;;; Calculate all x's and their possible y's.
(defparameter *x-possibleys*
  (loop for x from 2 to 49
     collect (cons x (loop for y from (- 100 x) downto (1+ x)
			collect y)))
  "For every x there are certain y's, with respect to the rules of the puzzle")

(defun xys-operation (op x-possibleys)
  "returns an alist of ((x possible-y) . (op x possible-y))"
  (let ((x (car x-possibleys))
	(ys (cdr x-possibleys)))
    (mapcar #'(lambda (y) (cons (list x y) (funcall op x y))) ys)))

(defun sp-numbers (op x-possibleys)
  "generates all possible sums or products of the puzzle"
  (loop for xys in x-possibleys
       append (xys-operation op xys)))

(defun group-sp (sp-numbers)
  "sp: Sum or Product"
  (loop for sp-number in (remove-duplicates sp-numbers :key #'cdr)
     collect (cons (cdr sp-number)
		   (mapcar #'car
			   (remove-if-not
			    #'(lambda (sp) (= sp (cdr sp-number)))
			    sp-numbers
			    :key #'cdr)))))

(defun statement-1a (sum-groups)
  "remove all sums with a single possible xy"
  (remove-if
   #'(lambda (xys) (= (list-length xys) 1))
   sum-groups
   :key #'cdr))

(defun statement-1b (x-possibleys)
  "S says: P does not know X and Y."
  (let ((multi-xy-sums (statement-1a (group-sp (sp-numbers #'+ x-possibleys))))
	(products (group-sp (sp-numbers #'* x-possibleys))))
    (flet ((sum-has-xy-which-leads-to-unique-prod (sum-xys)
	     ;; is there any product with a single possible xy?
	     (some #'(lambda (prod-xys) (= (list-length (cdr prod-xys)) 1))
		   ;; all possible xys of the sum's (* x ys)
		   (mapcar #'(lambda (xy) (assoc (apply #'* xy) products))
			   (cdr sum-xys)))))
      ;; remove sums with even one xy which leads to a unique product
      (remove-if #'sum-has-xy-which-leads-to-unique-prod multi-xy-sums))))

(defun remaining-products (remaining-sums-xys)
  "P's number is one of these"
  (loop for sum-xys in remaining-sums-xys
     append (loop for xy in (cdr sum-xys)
	       collect (apply #'* xy))))

(defun statement-2 (remaining-sums-xys)
  "P says: Now I know X and Y."
  (let ((remaining-products (remaining-products remaining-sums-xys)))
    (mapcar #'(lambda (a-sum-unit)
		(cons (car a-sum-unit)
		      (mapcar #'(lambda (xy)
				  (list (count (apply #'* xy) remaining-products)
					xy))
			      (cdr a-sum-unit))))
	    remaining-sums-xys)))

(defun statement-3 (remaining-sums-with-their-products-occurrences-info)
  "S says: Now I also know X and Y."
  (remove-if
   #'(lambda (sum-xys)
       ;; remove those sums which have more than 1 product, that
       ;; appear only once amongst all remaining products
       (> (count 1 sum-xys :key #'car) 1))
   remaining-sums-with-their-products-occurrences-info
   :key #'cdr))

(defun solution (survivor-sum-and-its-xys)
  "Now we know X and Y too :-D"
  (let* ((sum (caar survivor-sum-and-its-xys))
	 (xys (cdar survivor-sum-and-its-xys))
	 (xy (second (find 1 xys :key #'car))))
    (pairlis '(x y sum product)
	     (list (first xy) (second xy) sum (apply #'* xy)))))


(solution
 (statement-3
  (statement-2
   (statement-1b *x-possibleys*)))) ;; => ((PRODUCT . 52) (SUM . 17) (Y . 13) (X . 4))

```



### Version 2


```lisp

;;; Algorithm of Rosetta code:

;;; All possible pairs
(defparameter *all-possible-pairs*
  (loop for i from 2 upto 100
     append (loop for j from (1+ i) upto 100
	       if (<= (+ i j) 100)
	       collect (list i j))))

(defun oncep (item list)
  (eql 1 (count item list)))

;;; Terminology
(defun sum-decomp (n)
  (loop for x from 2 below (/ n 2)
     for y = (- n x)
     collect (list x y)))

(defun prod-decomp (n)
  (loop for x from 2 below (sqrt n)
     for y = (/ n x)
     if (and (>= 100 (+ x y)) (zerop (rem n x)))
     collect (list x y)))

;;; For every possible sum decomposition of the number X+Y, the product has in turn more than one product decomposition:
(defun fact-1 (n)
  "n = x + y"
  (flet ((premise (pair)
	       (> (list-length (prod-decomp (apply #'* pair))) 1)))
	(every #'premise (sum-decomp n))))

;;; The number X*Y has only one product decomposition for which fact 1 is true:
(defun fact-2 (n)
  "n = x * y"
  (oncep t (mapcar (lambda (pair) (fact-1 (apply #'+ pair))) (prod-decomp n))))

;;; The number X+Y has only one sum decomposition for which fact 2 is true:
(defun fact-3 (n)
  "n = x + y"
  (oncep t (mapcar (lambda (pair) (fact-2 (apply #'* pair))) (sum-decomp n))))

(defun find-xy (all-possible-pairs)
  (remove-if-not
   #'(lambda (p) (fact-3 (apply #'+ p)))
   (remove-if-not
    #'(lambda (p) (fact-2 (apply #'* p)))
    (remove-if-not
     #'(lambda (p) (fact-1 (apply #'+ p)))
     all-possible-pairs))))

(find-xy *all-possible-pairs*) ;; => ((4 13))

```



## D

```d
void main() {
    import std.stdio, std.algorithm, std.range, std.typecons;

    const s1 = cartesianProduct(iota(1, 101), iota(1, 101))
               .filter!(p => 1 < p[0] && p[0] < p[1] && p[0] + p[1] < 100)
               .array;

    alias P = const Tuple!(int, int);
    enum add   = (P p) => p[0] + p[1];
    enum mul   = (P p) => p[0] * p[1];
    enum sumEq = (P p) => s1.filter!(q => add(q) == add(p));
    enum mulEq = (P p) => s1.filter!(q => mul(q) == mul(p));

    const s2 = s1.filter!(p => sumEq(p).all!(q => mulEq(q).walkLength != 1)).array;
    const s3 = s2.filter!(p => mulEq(p).setIntersection(s2).walkLength == 1).array;
    s3.filter!(p => sumEq(p).setIntersection(s3).walkLength == 1).writeln;
}
```

```txt
[const(Tuple!(int, int))(4, 13)]
```


With an older version of the LDC2 compiler replace the <code>cartesianProduct</code> line with:

```d

    const s1 = iota(1, 101).map!(x => iota(1, 101).map!(y => tuple(x, y))).joiner

```

The <code>.array</code> turn the lazy ranges into arrays. This is a necessary optimization because D lazy Ranges aren't memoized as Haskell lazy lists.

Run-time: about 0.43 seconds with dmd, 0.08 seconds with ldc2.


## Elixir

```elixir
defmodule Puzzle do
  def sum_and_product do
    s1 = for x <- 2..49, y <- x+1..99, x+y<100, do: {x,y}
    s2 = Enum.filter(s1, fn p ->
      Enum.all?(sumEq(s1,p), fn q -> length(mulEq(s1,q)) != 1 end)
    end)
    s3 = Enum.filter(s2, fn p -> only1?(mulEq(s1,p), s2) end)
    Enum.filter(s3, fn p -> only1?(sumEq(s1,p), s3) end) |> IO.inspect
  end

  defp add({x,y}), do: x + y

  defp mul({x,y}), do: x * y

  defp sumEq(s, p), do: Enum.filter(s, fn q -> add(p) == add(q) end)

  defp mulEq(s, p), do: Enum.filter(s, fn q -> mul(p) == mul(q) end)

  defp only1?(a, b) do
    MapSet.size(MapSet.intersection(MapSet.new(a), MapSet.new(b))) == 1
  end
end

Puzzle.sum_and_product
```


```txt

[{4, 13}]

```



## Factor

A loose translation of D.

```factor
USING: combinators.short-circuit fry kernel literals math
math.ranges memoize prettyprint sequences sets tools.time ;
IN: rosetta-code.sum-and-product

CONSTANT: s1 $[
    2 100 [a,b] dup cartesian-product concat
    [ first2 { [ < ] [ + 100 < ] } 2&& ] filter
]

: quot-eq ( pair quot -- seq )
    [ s1 ] 2dip tuck '[ @ _ @ = ] filter ; inline

MEMO: sum-eq ( pair -- seq ) [ first2 + ] quot-eq ;
MEMO: mul-eq ( pair -- seq ) [ first2 * ] quot-eq ;

: s2 ( -- seq )
    s1 [ sum-eq [ mul-eq length 1 = not ] all? ] filter ;

: only-1 ( seq quot -- newseq )
    over '[ @ _ intersect length 1 = ] filter ; inline

: sum-and-product ( -- )
    [ s2 [ mul-eq ] [ sum-eq ] [ only-1 ] bi@ . ] time ;

MAIN: sum-and-product
```

```txt

{ { 4 13 } }
Running time: 0.241637693 seconds

```



## Go


```go
package main

import "fmt"

type pair struct{ x, y int }

func main() {
	//const max = 100
	// Use 1685 (the highest with a unique answer) instead
	// of 100 just to make it work a little harder :).
	const max = 1685
	var all []pair
	for a := 2; a < max; a++ {
		for b := a + 1; b < max-a; b++ {
			all = append(all, pair{a, b})
		}
	}
	fmt.Println("There are", len(all), "pairs where a+b <", max, "(and a<b)")
	products := countProducts(all)

	// Those for which no sum decomposition has unique product to are
	// S mathimatician's possible pairs.
	var sPairs []pair
pairs:
	for _, p := range all {
		s := p.x + p.y
		// foreach a+b=s (a<b)
		for a := 2; a < s/2+s&1; a++ {
			b := s - a
			if products[a*b] == 1 {
				// Excluded because P would have a unique product
				continue pairs
			}
		}
		sPairs = append(sPairs, p)
	}
	fmt.Println("S starts with", len(sPairs), "possible pairs.")
	//fmt.Println("S pairs:", sPairs)
	sProducts := countProducts(sPairs)

	// Look in sPairs for those with a unique product to get
	// P mathimatician's possible pairs.
	var pPairs []pair
	for _, p := range sPairs {
		if sProducts[p.x*p.y] == 1 {
			pPairs = append(pPairs, p)
		}
	}
	fmt.Println("P then has", len(pPairs), "possible pairs.")
	//fmt.Println("P pairs:", pPairs)
	pSums := countSums(pPairs)

	// Finally, look in pPairs for those with a unique sum
	var final []pair
	for _, p := range pPairs {
		if pSums[p.x+p.y] == 1 {
			final = append(final, p)
		}
	}

	// Nicely show any answers.
	switch len(final) {
	case 1:
		fmt.Println("Answer:", final[0].x, "and", final[0].y)
	case 0:
		fmt.Println("No possible answer.")
	default:
		fmt.Println(len(final), "possible answers:", final)
	}
}

func countProducts(list []pair) map[int]int {
	m := make(map[int]int)
	for _, p := range list {
		m[p.x*p.y]++
	}
	return m
}

func countSums(list []pair) map[int]int {
	m := make(map[int]int)
	for _, p := range list {
		m[p.x+p.y]++
	}
	return m
}

// not used, manually inlined above
func decomposeSum(s int) []pair {
	pairs := make([]pair, 0, s/2)
	for a := 2; a < s/2+s&1; a++ {
		pairs = append(pairs, pair{a, s - a})
	}
	return pairs
}
```

For x + y < 100 (<code>max = 100</code>):

```txt

There are 2304 pairs where a+b < 100 (and a<b)
S starts with 145 possible pairs.
P then has 86 possible pairs.
Answer: 4 and 13

```

For x + y < 1685 (<code>max = 1685</code>):

```txt

There are 706440 pairs where a+b < 1685 (and a<b)
S starts with 50485 possible pairs.
P then has 17485 possible pairs.
Answer: 4 and 13

```

Run-time ~1 msec and ~600 msec respectively.
Could be slightly faster if the slices and maps were given an estimated capacity to start
(e.g. (max/2)² for all pairs)
to avoid re-allocations (and resulting copies).


## Haskell

```haskell
import Data.List (intersect)

s1, s2, s3, s4 :: [(Int, Int)]
s1 = [(x, y) | x <- [1 .. 100], y <- [1 .. 100], 1 < x && x < y && x + y < 100]

add, mul :: (Int, Int) -> Int
add (x, y) = x + y
mul (x, y) = x * y

sumEq, mulEq :: (Int, Int) -> [(Int, Int)]
sumEq p = filter (\q -> add q == add p) s1
mulEq p = filter (\q -> mul q == mul p) s1

s2 = filter (\p -> all (\q -> (length $ mulEq q) /= 1) (sumEq p)) s1
s3 = filter (\p -> length (mulEq p `intersect` s2) == 1) s2
s4 = filter (\p -> length (sumEq p `intersect` s3) == 1) s3

main = print s4
```

```txt
[(4,13)]
```

Run-time: about 1.97 seconds.

Or, to illustrate some of the available variants, it turns out that we can double performance by slightly rearranging the filters in '''sumEq''' and '''mulEq'''. It also proves fractionally faster to shed some of the of outer list comprehension sugaring, using '''>>=''' or '''concatMap''' directly.

For a further doubling of performance, we can redefine '''add''' and '''mul''' as uncurried versions of '''(+)''' and '''(*)'''.

The '''y > x''' condition can usefully be moved upstream – dropping it from the test, and redefining the range of y as '''[x + 1 .. 100]''' from the start. (The '''1 < x''' test can also be moved out of the test and into the initial generator).

Finally, as we expect and need only one solution, Haskell's lazy evaluation strategy will avoid wasted tests if we request only the first item from the possible solution stream.

```Haskell
import Data.List (intersect)

s1, s2, s3, s4 :: [(Int, Int)]
s1 =
  [2 .. 100] >>=
  \x ->
     [x + 1 .. 100] >>=
     \y ->
        [ (x, y)
        | x + y < 100 ]

add, mul :: (Int, Int) -> Int
add = uncurry (+)

mul = uncurry (*)

sumEq, mulEq :: (Int, Int) -> [(Int, Int)]
sumEq p = filter ((add p ==) . add) s1

mulEq p = filter ((mul p ==) . mul) s1

s2 = filter (all ((1 /=) . length . mulEq) . sumEq) s1

s3 = filter ((1 ==) . length . (`intersect` s2) . mulEq) s2

s4 = filter ((1 ==) . length . (`intersect` s3) . sumEq) s3

-- TEST -----------------------------------------------------------------------
main :: IO ()
main = print $ take 1 s4
```

```txt
[(4,13)]
```



## Java



```Java
package org.rosettacode;

import java.util.ArrayList;
import java.util.List;


/**
 * This program applies the logic in the Sum and Product Puzzle for the value
 * provided by systematically applying each requirement to all number pairs in
 * range. Note that the requirements: (x, y different), (x < y), and
 * (x, y > MIN_VALUE) are baked into the loops in run(), sumAddends(), and
 * productFactors(), so do not need a separate test. Also note that to test a
 * solution to this logic puzzle, it is suggested to test the condition with
 * maxSum = 1685 to ensure that both the original solution (4, 13) and the
 * additional solution (4, 61), and only these solutions, are found. Note
 * also that at 1684 only the original solution should be found!
 */
public class SumAndProductPuzzle {
    private final long beginning;
    private final int maxSum;
    private static final int MIN_VALUE = 2;
    private List<int[]> firstConditionExcludes = new ArrayList<>();
    private List<int[]> secondConditionExcludes = new ArrayList<>();

    public static void main(String... args){

        if (args.length == 0){
            new SumAndProductPuzzle(100).run();
            new SumAndProductPuzzle(1684).run();
            new SumAndProductPuzzle(1685).run();
        } else {
            for (String arg : args){
                try{
                    new SumAndProductPuzzle(Integer.valueOf(arg)).run();
                } catch (NumberFormatException e){
                    System.out.println("Please provide only integer arguments. " +
                            "Provided argument " + arg + " was not an integer. " +
                            "Alternatively, calling the program with no arguments " +
                            "will run the puzzle where maximum sum equals 100, 1684, and 1865.");
                }
            }
        }
    }

    public SumAndProductPuzzle(int maxSum){
        this.beginning = System.currentTimeMillis();
        this.maxSum = maxSum;
        System.out.println("Run with maximum sum of " + String.valueOf(maxSum) +
                " started at " + String.valueOf(beginning) + ".");
    }

    public void run(){
        for (int x = MIN_VALUE; x < maxSum - MIN_VALUE; x++){
            for (int y = x + 1; y < maxSum - MIN_VALUE; y++){

                if (isSumNoGreaterThanMax(x,y) &&
                    isSKnowsPCannotKnow(x,y) &&
                    isPKnowsNow(x,y) &&
                    isSKnowsNow(x,y)
                    ){
                    System.out.println("Found solution x is " + String.valueOf(x) + " y is " + String.valueOf(y) +
                            " in " + String.valueOf(System.currentTimeMillis() - beginning) + "ms.");
                }
            }
        }
        System.out.println("Run with maximum sum of " + String.valueOf(maxSum) +
                " ended in " + String.valueOf(System.currentTimeMillis() - beginning) + "ms.");
    }

    public boolean isSumNoGreaterThanMax(int x, int y){
        return x + y <= maxSum;
    }

    public boolean isSKnowsPCannotKnow(int x, int y){

        if (firstConditionExcludes.contains(new int[] {x, y})){
            return false;
        }

        for (int[] addends : sumAddends(x, y)){
            if ( !(productFactors(addends[0], addends[1]).size() > 1) ) {
                firstConditionExcludes.add(new int[] {x, y});
                return false;
            }
        }
        return true;
    }

    public boolean isPKnowsNow(int x, int y){

        if (secondConditionExcludes.contains(new int[] {x, y})){
            return false;
        }

        int countSolutions = 0;
        for (int[] factors : productFactors(x, y)){
            if (isSKnowsPCannotKnow(factors[0], factors[1])){
                countSolutions++;
            }
        }

        if (countSolutions == 1){
            return true;
        } else {
            secondConditionExcludes.add(new int[] {x, y});
            return false;
        }
    }

    public boolean isSKnowsNow(int x, int y){

        int countSolutions = 0;
        for (int[] addends : sumAddends(x, y)){
            if (isPKnowsNow(addends[0], addends[1])){
                countSolutions++;
            }
        }
        return countSolutions == 1;
    }

    public List<int[]> sumAddends(int x, int y){

        List<int[]> list = new ArrayList<>();
        int sum = x + y;

        for (int addend = MIN_VALUE; addend < sum - addend; addend++){
            if (isSumNoGreaterThanMax(addend, sum - addend)){
                list.add(new int[]{addend, sum - addend});
            }
        }
        return list;
    }

    public List<int[]> productFactors(int x, int y){

        List<int[]> list = new ArrayList<>();
        int product = x * y;

        for (int factor = MIN_VALUE; factor < product / factor; factor++){
            if (product % factor == 0){
                if (isSumNoGreaterThanMax(factor, product / factor)){
                    list.add(new int[]{factor, product / factor});
                }
            }
        }
        return list;
    }
}
```


```txt
Run with maximum sum of 100 started at 1492436207694.
Found solution x is 4 y is 13 in 7ms.
Run with maximum sum of 100 ended in 54ms.
Run with maximum sum of 1684 started at 1492436207748.
Found solution x is 4 y is 13 in 9084ms.
Run with maximum sum of 1684 ended in 8234622ms.
Run with maximum sum of 1685 started at 1492444442371.
Found solution x is 4 y is 13 in 8922ms.
Found solution x is 4 y is 61 in 8939ms.
Run with maximum sum of 1685 ended in 8013991ms.
```



## JavaScript



### ES5


```JavaScript
(function () {
    'use strict';

    // GENERIC FUNCTIONS

    // concatMap :: (a -> [b]) -> [a] -> [b]
    var concatMap = function concatMap(f, xs) {
            return [].concat.apply([], xs.map(f));
        },

        // curry :: ((a, b) -> c) -> a -> b -> c
        curry = function curry(f) {
            return function (a) {
                return function (b) {
                    return f(a, b);
                };
            };
        },

        // intersectBy :: (a - > a - > Bool) - > [a] - > [a] - > [a]
        intersectBy = function intersectBy(eq, xs, ys) {
            return xs.length && ys.length ? xs.filter(function (x) {
                return ys.some(curry(eq)(x));
            }) : [];
        },

        // range :: Int -> Int -> Maybe Int -> [Int]
        range = function range(m, n, step) {
            var d = (step || 1) * (n >= m ? 1 : -1);
            return Array.from({
                length: Math.floor((n - m) / d) + 1
            }, function (_, i) {
                return m + i * d;
            });
        };

    // PROBLEM FUNCTIONS

    // add, mul :: (Int, Int) -> Int
    var add = function add(xy) {
            return xy[0] + xy[1];
        },
        mul = function mul(xy) {
            return xy[0] * xy[1];
        };

    // sumEq, mulEq :: (Int, Int) -> [(Int, Int)]
    var sumEq = function sumEq(p) {
            var addP = add(p);
            return s1.filter(function (q) {
                return add(q) === addP;
            });
        },
        mulEq = function mulEq(p) {
            var mulP = mul(p);
            return s1.filter(function (q) {
                return mul(q) === mulP;
            });
        };

    // pairEQ :: ((a, a) -> (a, a)) -> Bool
    var pairEQ = function pairEQ(a, b) {
        return a[0] === b[0] && a[1] === b[1];
    };

    // MAIN

    // xs :: [Int]
    var xs = range(1, 100);

    // s1 s2, s3, s4 :: [(Int, Int)]
    var s1 = concatMap(function (x) {
            return concatMap(function (y) {
                return 1 < x && x < y && x + y < 100 ? [
                    [x, y]
                ] : [];
            }, xs);
        }, xs),

        s2 = s1.filter(function (p) {
            return sumEq(p).every(function (q) {
                return mulEq(q).length > 1;
            });
        }),

        s3 = s2.filter(function (p) {
            return intersectBy(pairEQ, mulEq(p), s2).length === 1;
        }),

        s4 = s3.filter(function (p) {
            return intersectBy(pairEQ, sumEq(p), s3).length === 1;
        });

    return s4;
})();

```


```JavaScript
[[4, 13]]
```

(Finished in 0.69s)



### ES6

```JavaScript
(() => {
    'use strict';

    const main = () => {

        const
            // xs :: [Int]
            xs = enumFromTo(1, 100),

            // s1 s2, s3, s4 :: [(Int, Int)]
            s1 = concatMap(x => concatMap(y =>
                ((1 < x) && (x < y) && 100 > (x + y)) ? [
                    [x, y]
                ] : [],
                xs), xs),
            s2 = filter(
                p => all(q => 1 < length(mulEq(q, s1)), sumEq(p, s1)),
                s1
            ),
            s3 = filter(
                p => 1 === length(intersectBy(
                    pairEQ, mulEq(p, s1), s2
                )),
                s2
            );

        return s3.filter(
            p => 1 === length(intersectBy(
                pairEQ, sumEq(p, s1), s3
            ))
        );
    };

    // PROBLEM FUNCTIONS ----------------------------------

    // add, mul :: (Int, Int) -> Int
    const
        add = xy => xy[0] + xy[1],
        mul = xy => xy[0] * xy[1],

        // sumEq, mulEq :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
        sumEq = (p, s) => {
            const addP = add(p);
            return filter(q => add(q) === addP, s);
        },
        mulEq = (p, s) => {
            const mulP = mul(p)
            return filter(q => mul(q) === mulP, s);
        },

        // pairEQ :: ((a, a) -> (a, a)) -> Bool
        pairEQ = (a, b) => (a[0] === b[0]) && (a[1] === b[1]);


    // GENERIC FUNCTIONS ----------------------------------

    // all :: (a -> Bool) -> [a] -> Bool
    const all = (p, xs) => xs.every(p);

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.reduce((a, x) => a.concat(f(x)), []);

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    const intersectBy = (eq, xs, ys) => {
        const ceq = curry(eq);
        return (0 < xs.length && 0 < ys.length) ?
            xs.filter(x => ys.some(ceq(x))) : [];
    };

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // MAIN ---
    return main();
})();
```

```JavaScript
[[4, 13]]
```

(Finished in 0.307s)


## Julia

From the awk/sidef version. It is also possible to use filters as in the Scala solution, but although less verbose,
using filters would be much slower in Julia, which often favors fast for loops over lists for speed.


```julia

using Primes

function satisfy1(x::Integer)
    prmslt100 = primes(100)
    for i in 2:(x ÷ 2)
        if i ∈ prmslt100 && x - i ∈ prmslt100
            return false
        end
    end
    return true
end

function satisfy2(x::Integer)
    once = false
    for i in 2:isqrt(x)
        if x % i == 0
            j = x ÷ i
            if 2 < j < 100 && satisfy1(i + j)
                if once return false end
                once = true
            end
        end
    end
    return once
end

function satisfyboth(x::Integer)
    if !satisfy1(x) return 0 end
    found = 0
    for i in 2:(x ÷ 2)
        if satisfy2(i * (x - i))
            if found > 0 return 0 end
            found = i
        end
    end
    return found
end

for i in 2:99
    if (j = satisfyboth(i)) > 0
        println("Solution: ($j, $(i - j))")
    end
end
```


```txt
Solution: (4, 13)
```



## Kotlin


```scala
// version 1.1.4-3

data class P(val x: Int, val y: Int, val sum: Int, val prod: Int)

fun main(args: Array<String>) {
    val candidates = mutableListOf<P>()
    for (x in 2..49) {
        for (y in x + 1..100 - x) {
            candidates.add(P(x, y, x + y, x * y))
        }
    }

    val sums = candidates.groupBy { it.sum }
    val prods = candidates.groupBy { it.prod }

    val fact1 = candidates.filter { sums[it.sum]!!.all { prods[it.prod]!!.size > 1 } }
    val fact2 = fact1.filter { prods[it.prod]!!.intersect(fact1).size == 1 }
    val fact3 = fact2.filter { sums[it.sum]!!.intersect(fact2).size == 1 }
    print("The only solution is : ")
    for ((x, y, _, _) in fact3) println("x = $x, y = $y")
}
```


```txt

The only solution is : x = 4, y = 13

```



## ooRexx


### version 1

for comments see REXX version 4.

```oorexx
all      =.set~new
Call time 'R'
cnt.=0
do a=2 to 100
  do b=a+1 to 100-2
    p=a b
    if a+b>100 then leave b
    all~put(p)
    prd=a*b
    cnt.prd+=1
    End
  End
Say "There are" all~items "pairs where X+Y <=" max "(and X<Y)"

spairs=.set~new
Do Until all~items=0
  do p over all
    d=decompositions(p)
    If take Then
      spairs=spairs~union(d)
    dif=all~difference(d)
    Leave
    End
  all=dif
  end
Say "S starts with" spairs~items "possible pairs."

sProducts.=0
Do p over sPairs
  Parse Var p x y
  prod=x*y
  sProducts.prod+=1
  End

pPairs=.set~new
Do p over sPairs
  Parse Var p xb yb
  prod=xb*yb
  If sProducts.prod=1 Then
    pPairs~put(p)
  End
Say "P then has" pPairs~items "possible pairs."

Sums.=0
Do p over pPairs
  Parse Var p xc yc
  sum=xc+yc
  Sums.sum+=1
  End

final=.set~new
Do p over pPairs
  Parse Var p x y
  sum=x+y
  If Sums.sum=1 Then
    final~put(p)
  End

si=0
Do p Over final
  si+=1
  sol.si=p
  End
Select
  When final~items=1 Then Say "Answer:" sol.1
  When final~items=0 Then Say "No possible answer."
  Otherwise Do;            Say final~items "possible answers:"
                           Do p over final
                             Say p
                             End
    End
  End
Say "Elapsed time:" time('E') "seconds"
Exit

decompositions: Procedure Expose cnt. take spairs
  epairs=.set~new
  Use Arg p
  Parse Var p aa bb
  s=aa+bb
  take=1
  Do xa=2 To s/2
    ya=s-xa
    pp=xa ya
    epairs~put(pp)
    prod=xa*ya
    If cnt.prod=1 Then
      take=0
    End
  return epairs
```

```txt
There are 2352 pairs where X+Y <= MAX (and X<Y)
S starts with 145 possible pairs.
P then has 86 possible pairs.
Answer: 4 13
Elapsed time: 0.016000 seconds
```


### version 2

Uses objects for storing the number pairs. Note the computed hash value and the == mathod
(required to make the set difference work)

```oorexx
all      =.set~new
Call time 'R'
cnt.=0
do a=2 to 100
  do b=a+1 to 100-2
    p=.pairs~new(a,b)
    if p~sum>100 then leave b
    all~put(p)
    prd=p~prod
    cnt.prd+=1
    End
  End
Say "There are" all~items "pairs where X+Y <=" max "(and X<Y)"

spairs=.set~new
Do Until all~items=0
  do p over all
    d=decompositions(p)
    If take Then
      spairs=spairs~union(d)
    dif=all~difference(d)
    Leave
    End
  all=dif
  end
Say "S starts with" spairs~items "possible pairs."

sProducts.=0
Do p over sPairs
  prod=p~prod
  sProducts.prod+=1
  End

pPairs=.set~new
Do p over sPairs
  prod=p~prod
  If sProducts.prod=1 Then
    pPairs~put(p)
  End
Say "P then has" pPairs~items "possible pairs."

Sums.=0
Do p over pPairs
  sum=p~sum
  Sums.sum+=1
  End

final=.set~new
Do p over pPairs
  sum=p~sum
  If Sums.sum=1 Then
    final~put(p)
  End

si=0
Do p Over final
  si+=1
  sol.si=p
  End
Select
  When final~items=1 Then Say "Answer:" sol.1~string
  When final~items=0 Then Say "No possible answer."
  Otherwise Do;            Say final~items "possible answers:"
                           Do p over final
                             Say p~string
                             End
    End
  End
Say "Elapsed time:" time('E') "seconds"
Exit

decompositions: Procedure Expose cnt. take spairs
  epairs=.set~new
  Use Arg p
  s=p~sum
  take=1
  Do xa=2 To s/2
    ya=s-xa
    pp=.pairs~new(xa,ya)
    epairs~put(pp)
    prod=pp~prod
    If cnt.prod=1 Then
      take=0
    End
  return epairs

::class pairs
::attribute a        -- allow access to attribute
::attribute b        -- allow access to attribute
::attribute sum      -- allow access to attribute
::attribute prod     -- allow access to attribute

-- only the strict equality form is needed for the collection classes,
::method "=="
  expose a b
  use strict arg other
  return a == other~a & b == other~b

-- not needed to make the set difference work, but added for completeness
::method "\=="
  expose a b
  use strict arg other
  return a \== other~a | b \== other~b

::method hashCode
  expose hash
  return hash

::method init        -- create pair, calculate sum, product
                     -- and index (blank delimited values)
  expose hash a b sum prod oid
  use arg a, b
  hash = a~hashCode~bitxor(b~hashCode) -- create hash value
  sum =a+b           -- sum
  prod=a*b           -- product

::method string      -- this creates the string to be shown
  expose a b
  return "[x="||a",y="||b"]"
```

```txt
There are 2352 pairs where X+Y <= MAX (and X<Y)
S starts with 145 possible pairs.
P then has 86 possible pairs.
Answer: [x=4,y=13]
Elapsed time: 0.079000 seconds
```



## Perl

```perl
use List::Util qw(none);

sub grep_unique {
    my($by, @list) = @_;
    my @seen;
    for (@list) {
        my $x = &$by(@$_);
        $seen[$x]= defined $seen[$x] ? 0 : join ' ', @$_;
    }
    grep { $_ } @seen;
}

sub sums {
    my($n) = @_;
    my @sums;
    push @sums, [$_, $n - $_] for 2 .. int $n/2;
    @sums;
}

sub sum     { $_[0] + $_[1] }
sub product { $_[0] * $_[1] }

for $i (2..97) {
    push @all_pairs, map { [$i, $_] } $i + 1..98
}

# Fact 1:
%p_unique = map { $_ => 1 } grep_unique(\&product, @all_pairs);
for my $p (@all_pairs) {
    push @s_pairs, [@$p] if none { $p_unique{join ' ', @$_} } sums sum @$p;
}

# Fact 2:
@p_pairs = map { [split ' ', $_] } grep_unique(\&product, @s_pairs);

# Fact 3:
@final_pair = grep_unique(\&sum, @p_pairs);

printf "X = %d, Y = %d\n", split ' ', $final_pair[0];
```

```txt
X = 4, Y = 13
```



## Perl 6

```perl6
sub grep-unique (&by, @list) { @list.classify(&by).values.grep(* == 1).map(*[0]) }
sub sums        ($n)         { ($_, $n - $_ for 2 .. $n div 2) }
sub sum         ([$x, $y])   { $x + $y }
sub product     ([$x, $y])   { $x * $y }

my @all-pairs = (|($_ X $_+1 .. 98) for 2..97);

# Fact 1:
my %p-unique := Set.new: map ~*, grep-unique &product, @all-pairs;
my @s-pairs = @all-pairs.grep: { none (%p-unique{~$_} for sums sum $_) };

# Fact 2:
my @p-pairs = grep-unique &product, @s-pairs;

# Fact 3:
my @final-pairs = grep-unique &sum, @p-pairs;

printf "X = %d, Y = %d\n", |$_ for @final-pairs;
```


```txt
X = 4, Y = 13
```



## Phix

Runs in 0.03s

```Phix
function is_prime(integer x)
    if x>3 then
        for i=2 to floor(sqrt(x)) do
            if mod(x,i)=0 then
                return 0
            end if
        end for
    end if
    return 1
end function

function satisfies_statement1(integer s)
-- S says: P does not know the two numbers.
-- Given s, for /all/ pairs (a,b), a+b=s, 2<=a,b<=99, at least one of a or b is composite
    for a=2 to floor(s/2) do
        if is_prime(a) and is_prime(s-a) then
            return 0
        end if
    end for
    return 1
end function

function satisfies_statement2(integer p)
-- P says: Now I know the two numbers.
-- Given p, for /all/ pairs (a,b), a*b=p, 2<=a,b<=99, exactly one pair satisfies statement 1
integer winner = 0
    for i=2 to floor(sqrt(p)) do
        if mod(p,i)=0 then
            integer j = floor(p/i)
            if 2<=j and j<=99 then
                if satisfies_statement1(i+j) then
                    if winner then return 0 end if
                    winner = 1
                end if
            end if
        end if
    end for
    return winner
end function

function satisfies_statement3(integer s)
-- S says: Now I know the two numbers.
-- Given s, for /all/ pairs (a,b), a+b=s, 2<=a,b<=99, exactly one pair satisfies statements 1 and 2
integer winner = 0
    if satisfies_statement1(s) then
        for a=2 to floor(s/2) do
            if satisfies_statement2(a*(s-a)) then
                if winner then return 0 end if
                winner = a
            end if
        end for
    end if
    return winner
end function

for s=2 to 100 do
    integer a = satisfies_statement3(s)
    if a!=0 then
        printf(1,"%d (%d+%d)\n",{s,a,s-a})
    end if
end for
```

```txt

17 (4+13)

```



## Python


Based on the Python solution from [[wp:Sum_and_Product_Puzzle#Python_code|Wikipedia]]:

```python
#!/usr/bin/env python

from collections import Counter

def decompose_sum(s):
    return [(a,s-a) for a in range(2,int(s/2+1))]

# Generate all possible pairs
all_pairs = set((a,b) for a in range(2,100) for b in range(a+1,100) if a+b<100)

# Fact 1 --> Select pairs for which all sum decompositions have non-unique product
product_counts = Counter(c*d for c,d in all_pairs)
unique_products = set((a,b) for a,b in all_pairs if product_counts[a*b]==1)
s_pairs = [(a,b) for a,b in all_pairs if
    all((x,y) not in unique_products for (x,y) in decompose_sum(a+b))]

# Fact 2 --> Select pairs for which the product is unique
product_counts = Counter(c*d for c,d in s_pairs)
p_pairs = [(a,b) for a,b in s_pairs if product_counts[a*b]==1]

# Fact 3 --> Select pairs for which the sum is unique
sum_counts = Counter(c+d for c,d in p_pairs)
final_pairs = [(a,b) for a,b in p_pairs if sum_counts[a+b]==1]

print(final_pairs)
```


```txt
[(4, 13)]
```



## Racket

{{trans|D}}To calculate the results faster this program use memorization. So it has a modified version of <code>sum=</code> and <code>mul=</code> to increase the chances of reusing the results.

```Racket
#lang racket
(define-syntax-rule (define/mem (name args ...) body ...)
  (begin
    (define cache (make-hash))
    (define (name args ...)
      (hash-ref! cache (list args ...) (lambda () body ...)))))

(define (sum p) (+ (first p) (second p)))
(define (mul p) (* (first p) (second p)))

(define (sum= p s) (filter (lambda (q) (= p (sum q))) s))
(define (mul= p s) (filter (lambda (q) (= p (mul q))) s))

(define (puzzle tot)
  (printf "Max Sum: ~a\n" tot)
  (define s1 (for*/list ([x (in-range 2 (add1 tot))]
                         [y (in-range (add1 x) (- (add1 tot) x))])
               (list x y)))
  (printf "Possible pairs: ~a\n" (length s1))

  (define/mem (sumEq/all p) (sum= p s1))
  (define/mem (mulEq/all p) (mul= p s1))

  (define s2 (filter (lambda (p) (andmap (lambda (q)
                                           (not (= (length (mulEq/all (mul q))) 1)))
                                         (sumEq/all (sum p))))
                     s1))
  (printf "Initial pairs for S: ~a\n" (length s2))

  (define s3 (filter (lambda (p) (= (length (mul= (mul p) s2)) 1))
                   s2))
  (displayln (length s3))
  (printf "Pairs for P: ~a\n" (length s3))

  (define s4 (filter (lambda (p) (= (length (sum= (sum p) s3)) 1))
                     s3))
  (printf "Final pairs for S: ~a\n" (length s4))

  (displayln s4))

(puzzle 100)
```

```txt
Max Sum: 100
Possible pairs: 2352
Initial pairs for S: 145
Pairs for P: 86
Final pairs for S: 1
((4 13))
```



## REXX


### version 1

I tried hard to understand/translate the algorithms shown so far (16 Oct 2016)
Unfortunately to no avail (not knowing the semantics of the used languages).
Finally I was successful by implementing the rules referred to in Wikipedia
http://www.win.tue.nl/~gwoegi/papers/freudenthal1.pdf
which had a very clear description.

```rexx
debug=0
If debug Then Do
  oid='sppn.txt'; 'erase' oid
  End
Call time 'R'
all_pairs=''
cnt.=0
i=0
/* first take all possible pairs 2<=x<y with x+y<=100 */
/* and compute the respective sums and products       */
/* count the number of times a sum or product occurs  */
Do x=2 To 98
  Do y=x+1 To 100-x
    x=right(x,2,0)
    y=right(y,2,0)
    all_pairs=all_pairs x'/'y
    i=i+1
    x.i=x
    y.i=y
    sum=x+y
    prd=x*y
    cnt.0s.sum=cnt.0s.sum+1
    cnt.0p.prd=cnt.0p.prd+1
    End
  End
n=i
/* now compute the possible pairs for each sum sum_d.sum */
/*                                 and product prd_d.prd */
/* also the list of possible sums and products suml, prdl*/
sum_d.=''
prd_d.=''
suml=''
prdl=''
Do i=1 To n
  x=x.i
  y=y.i
  x=right(x,2,0)
  y=right(y,2,0)
  sum=x+y
  prd=x*y
  cnt.0s.x.y=cnt.0s.sum
  cnt.0p.x.y=cnt.0p.prd
  sum_d.sum=sum_d.sum x'/'y
  prd_d.prd=prd_d.prd x'/'y
  If wordpos(sum,suml)=0 Then suml=suml sum
  If wordpos(prd,prdl)=0 Then prdl=prdl prd
  End
Say n 'possible pairs'
Call o 'SUM'
suml=wordsort(suml)
prdl=wordsort(prdl)
sumlc=suml
si=0
pi=0
Do While sumlc>''
  Parse Var sumlc sum sumlc
  si=si+1
  sum.si=sum
  si.sum=si
  If sum=17 Then sx=si
  temp=prdl
  Do While temp>''
    Parse Var temp prd temp
    If si=1 Then Do
      pi=pi+1
      prd.pi=prd
      pi.prd=pi
      If prd=52 Then px=pi
      End
    A.prd.sum='+'
    End
  End
sin=si
pin=pi
Call o 'SUM'
Do si=1 To sin
  Call o f5(si) f3(sum.si)
  End
Call o 'PRD'
Do pi=1 To pin
  Call o f5(pi) f6(prd.pi)
  End
a.='-'
Do pi=1 To pin
  prd=prd.pi
  Do si=1 To sin
    sum=sum.si
    Do sj=1 To words(sum_d.sum)
      If wordpos(word(sum_d.sum,sj),prd_d.prd)>0 Then
        Parse Value word(sum_d.sum,sj) with x '/' y
        prde=x*y
        sume=x+y
        pa=pi.prde
        sa=si.sume
        a.pa.sa='+'
      End
    End
  End
Call show '1'

Do pi=1 To pin
  prow=''
  cnt=0
  Do si=1 To sin
    If a.pi.si='+' Then Do
      cnt=cnt+1
      pj=pi
      sj=si
      End
    End
  If cnt=1 Then
    a.pj.sj='1'
  End
Call show '2'

Do si=1 To sin
  Do pi=1 To pin
    If a.pi.si='1' Then Leave
    End
  If pi<=pin Then Do
    Do pi=1 To pin
      If a.pi.si='+' Then
        a.pi.si='2'
      End
    End
  End
Call show '3'

Do pi=1 To pin
  prow=''
  Do si=1 To sin
    prow=prow||a.pi.si
    End
  If count('+',prow)>1 Then Do
    Do si=1 To sin
      If a.pi.si='+' Then
        a.pi.si='3'
      End
    End
  End
Call show '4'

Do si=1 To sin
  scol=''
  Do pi=1 To pin
    scol=scol||a.pi.si
    End
  If count('+',scol)>1 Then Do
    Do pi=1 To pin
      If a.pi.si='+' Then
        a.pi.si='4'
      End
    End
  End
Call show '5'

sol=0
Do pi=1 To pin
  Do si=1 To sin
    If a.pi.si='+' Then Do
      Say sum.si prd.pi
      sum=sum.si
      prd=prd.pi
      sol=sol+1
      End
    End
  End
Say sol 'solution(s)'
Say '            possible pairs'
Say 'Product='prd prd_d.52
Say '    Sum='sum sum_d.17
Say 'The only pair in both lists is 04/13.'
Say 'Elapsed time:' time('E') 'seconds'
Exit
show:
If debug Then Do
  Call o 'show' arg(1)
  Do pi=1 To 60
    ol=''
    Do si=1 To 60
      ol=ol||a.pi.si
      End
    Call o ol
    End
  Say 'a.'px'.'sx'='a.px.sx
  End
Return

Exit
o: Return lineout(oid,arg(1))
f3: Return format(arg(1),3)
f4: Return format(arg(1),4)
f5: Return format(arg(1),5)
f6: Return format(arg(1),6)

count: Procedure
  Parse Arg c,s
  s=translate(s,c,c||xrange('00'x,'ff'x))
  s=space(s,0)
  Return length(s)
```

```txt
2352 possible pairs
17 52
1 solution(s)
            possible pairs
Product=52  02/26 04/13
    Sum=17  02/15 03/14 04/13 05/12 06/11 07/10 08/09
The only pair in both lists is 04/13.
Elapsed time: 4.891000 seconds
```


### version 2

```rexx
Call time 'R'
Do s=2 To 100
  a=satisfies_statement3(s)
  If a>0 Then Do
    p=a*(s-a)
    Say a'/'||(s-a) 's='s 'p='p
    End
  End
Say 'Elapsed time:' time('E') 'seconds'
Exit

satisfies_statement1: Procedure
  Parse Arg s
  Do a=2 To s/2
    If is_prime(a) & is_prime(s-a) Then
      Return 0
    End
  Return 1

satisfies_statement2: Procedure
  Parse Arg p
  winner=0
  Do i=2 By 1 While i**2<p
    If p//i=0 Then Do
      j=p%i
      If 2<=j & j<=99 Then Do
        if satisfies_statement1(i+j) Then Do
          if winner Then
            Return 0
          winner=1
          End
        End
      End
    End
  Return winner

satisfies_statement3: Procedure
  Parse Arg s
  winner=0
  If satisfies_statement1(s)=0 Then
    Return 0
  Do a=2 To s/2
    b=s-a
    If satisfies_statement2(a*b) Then Do
      If winner>0 Then
        Return 0
      winner=a
      End
    End
  Return winner

is_prime: Procedure
  call Trace 'O'
  Parse Arg x
  If x<=3 Then Return 1
  i=2
  Do i=2 By 1 While i**2<=x
    If datatype(x/i,'W') Then Return 0
    End
  Return 1
```

```txt
4/13 s=17 p=52
Elapsed time: 0.078000 seconds
```



### version 3

```rexx
/*---------------------------------------------------------------------
* X and Y are two different whole numbers greater than 1.
* Their sum is no greater than 100, and Y is greater than X.
* S and P are two mathematicians (and consequently perfect logicians);
* S knows the sum X+Y and P knows the product X*Y.
* Both S and P know all the information in this paragraph.
*
* The following conversation occurs:
*
* * S says "P does not know X and Y."
* * P says "Now I know X and Y."
* * S says "Now I also know X and Y!"
*
* What are X and Y?
*--------------------------------------------------------------------*/
Call time 'R'
max=100
Products.=0
all=''
Do x=2 To max
  Do y=x+1 To max-2
    If x+y<=100 Then Do
      all=all x'/'y
      prod=x*y; Products.prod=Products.prod+1
      End
    End
  End
Say "There are" words(all) "pairs where X+Y <=" max "(and X<Y)"
/*---------------------------------------------------------------------
* First eliminate all pairs where the product is unique:
* For each pair we look at the decompositions of the sum (x+y).
* If for any of these decompositions (xa/ya) the product is unique
* then x/y cannot be the solution of the puzzle and we eliminate it
* from the list of possible pairs
*--------------------------------------------------------------------*/
sPairs=''
Do i=1 To words(all)
  xy=word(all,i)
  Parse Var xy x '/' Y
  Parse Var xy xx '/' Yy
  s=x+y
  take=1
  Do xa=2 To s/2
    ya=s-xa
    prod=xa*ya
    If products.prod=1 Then Do
      take=0
      Iterate i
      End
    End
  If take Then
    sPairs=sPairs xy
  End
Say "S starts with" words(sPairs) "possible pairs."

/*---------------------------------------------------------------------
* From the REMAINING pairs take only these where the product is unique:
* For each pair we look at the decompositions of the known product.
* If for any of these decompositions (xb/yb) the product is unique
* then xb/yb can be the solution of the puzzle and we add it
* to the list of possible pairs.
*--------------------------------------------------------------------*/
sProducts.=0
Do i=1 To words(sPairs)
  xy=word(sPairs,i)
  Parse Var xy x '/' y
  prod=x*y
  sProducts.prod=sProducts.prod+1
  End
pPairs=''
Do i=1 To words(sPairs)
  xy=word(sPairs,i)
  Parse Var xy x '/' y
  prod=x*y
  If sProducts.prod=1 Then
    pPairs=pPairs xy
  End
Say "P then has" words(pPairs) "possible pairs."

/*---------------------------------------------------------------------
* From the now REMAINING pairs take only these where the sum is unique
* Now we look at all possible pairs and find the one (xc/yc)
* with a unique sum which must be the sum we knew from the beginning.
* The pair xc/yc is then the solution
*--------------------------------------------------------------------*/
Sums.=0
Do i=1 To words(pPairs)
  xy=word(pPairs,i)
  Parse Var xy x '/' y
  sum=x+y
  Sums.sum=Sums.sum+1
  End
final=''
Do i=1 To words(pPairs)
  xy=word(pPairs,i)
  Parse Var xy x '/' y
  sum=x+y
  If Sums.sum=1 Then
    final = final xy
  End
Select
  When words(final)=1 Then Say "Answer:" strip(final)
  When words(final)=0 Then Say "No possible answer."
  Otherwise Do;            Say words(final) "possible answers:"
                           Say strip(final)
    End
  End
Say "Elapsed time:" time('E') "seconds"
Exit
```

```txt
There are 2352 pairs where X+Y <= 100 (and X<Y)
S starts with 145 possible pairs.
P then has 86 possible pairs.
Answer: 4/13
Elapsed time: 0.045000 seconds
```



### version 4

Now that I have understood the logic (I am neither S nor P) I have created an alternative to version 3.

```rexx
/*---------------------------------------------------------------------
* X and Y are two different whole numbers greater than 1.
* Their sum is no greater than 100, and Y is greater than X.
* S and P are two mathematicians (and consequently perfect logicians);
* S knows the sum X+Y and P knows the product X*Y.
* Both S and P know all the information in this paragraph.
*
* The following conversation occurs:
*
* * S says "P does not know X and Y."
* * P says "Now I know X and Y."
* * S says "Now I also know X and Y!"
*
* What are X and Y?
*--------------------------------------------------------------------*/
Call time 'R'
max=100
Products.=0
all=''
Do x=2 To max
  Do y=x+1 To max-2
    If x+y<=100 Then Do
      all=all x'/'y
      prod=x*y; Products.prod=Products.prod+1
      End
    End
  End
Say "There are" words(all) "pairs where X+Y <=" max "(and X<Y)"
/*---------------------------------------------------------------------
* First eliminate all pairs where the product is unique:
* For each pair we look at the decompositions of the sum (x+y).
* If for any of these decompositions (xa/ya) the product is unique
* then the given sum cannot be the sum of the pair we are looking for
* Otherwise all pairs in the sum's decompositions are eligible.
*--------------------------------------------------------------------*/
sPairs=''
done.=0
Do i=1 To words(all)
  xy=word(all,i)
  If done.xy Then Iterate
  Parse Var xy x '/' y
  s=x+y
  take=1
  el=''
  Do xa=2 To s/2
    ya=s-xa
    m=xa'/'ya
    done.m=1
    el=el m
    prod=xa*ya
    If products.prod=1 Then
      take=0
    End
  If take Then
    sPairs=sPairs el
  End
Say "S starts with" words(sPairs) "possible pairs."

/*---------------------------------------------------------------------
* From the REMAINING pairs take only these where the product is unique:
* For each pair we look at the decompositions of the known product.
* If for any of these decompositions (xb/yb) the product is unique
* then xb/yb can be the solution of the puzzle and we add it
* to the list of possible pairs.
*--------------------------------------------------------------------*/
sProducts.=0
Do i=1 To words(sPairs)
  xy=word(sPairs,i)
  Parse Var xy x '/' y
  prod=x*y
  sProducts.prod=sProducts.prod+1
  End
pPairs=''
Do i=1 To words(sPairs)
  xy=word(sPairs,i)
  Parse Var xy xb '/' yb
  prod=xb*yb
  If sProducts.prod=1 Then
    pPairs=pPairs xy
  End
Say "P then has" words(pPairs) "possible pairs."

/*---------------------------------------------------------------------
* From the now REMAINING pairs take only these where the sum is unique
* Now we look at all possible pairs and find the one (xc/yc)
* with a unique sum which must be the sum we knew from the beginning.
* The pair xc/yc is then the solution
*--------------------------------------------------------------------*/
Sums.=0
Do i=1 To words(pPairs)
  xy=word(pPairs,i)
  Parse Var xy xc '/' yc
  sum=xc+yc
  Sums.sum=Sums.sum+1
  End
final=''
Do i=1 To words(pPairs)
  xy=word(pPairs,i)
  Parse Var xy x '/' y
  sum=x+y
  If Sums.sum=1 Then
    final = final xy
  End
Select
  When words(final)=1 Then Say "Answer:" strip(final)
  When words(final)=0 Then Say "No possible answer."
  Otherwise Do;            Say words(final) "possible answers:"
                           Say strip(final)
    End
  End
Say "Elapsed time:" time('E') "seconds"
Exit
```

```txt
There are 2352 pairs where X+Y <= 100 (and X<Y)
S starts with 145 possible pairs.
P then has 86 possible pairs.
Answer: 4/13
Elapsed time: 0.032000 seconds
```



### version 5


```rexx
/*REXX program solves the  Sum and Product Puzzle (also known as the Impossible Puzzle).*/
@.=0; H=100;  do j=3  by 2  to H                 /*find all odd primes  ≤  1st argument.*/
                do k=3  until k*k>j;  if @.k==0  then iterate;  if j//k==0  then iterate j
                end  /*k*/;              @.j= 1  /*found a prime number:  J             */
              end    /*j*/
@.2=1                                            /*assign the even prime, ex post facto.*/
     do s=2  for H-1;  if C1(s)==0  then iterate /*find and display the puzzle solution.*/
     $= 0;                do m=2  for  s%2 -1    /* [↓]  check for uniqueness of product*/
                          if C2(m * (s-m))  then do;  if $>0  then iterate s;  $= m;   end
                          end   /*m*/
     if $>0  then say  "The numbers are:  "         $            " and "           s-$
     end   /*s*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
C1: procedure expose @.;    parse arg s          /*validate the first puzzle condition. */
      do a=2 for s%2-1; if @.a  then do; _=s-a; if @._  then return 0; end; end;  return 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
C2: procedure expose @. H;  parse arg p;    $= 0 /*validate the second puzzle condition.*/
      do j=2  while j*j < p                      /*perform up to the square root of  P. */
      if p//j==0  then do;               q= p % j
                       if q>=2  then  if q<=H  then  if C1(j+q)  then  if $  then return 0
                                                                             else $= 1
                       end
      end   /*j*/;              return $
```

```txt

The numbers are:   4  and  13

```



## Ruby

```ruby
def add(x,y) x + y end
def mul(x,y) x * y end

def sumEq(s,p) s.select{|q| add(*p) == add(*q)} end
def mulEq(s,p) s.select{|q| mul(*p) == mul(*q)} end

s1 = (a = *2...100).product(a).select{|x,y| x<y && x+y<100}
s2 = s1.select{|p| sumEq(s1,p).all?{|q| mulEq(s1,q).size != 1} }
s3 = s2.select{|p| (mulEq(s1,p) & s2).size == 1}
p    s3.select{|p| (sumEq(s1,p) & s3).size == 1}
```


```txt

[[4, 13]]

```



## Scala


```scala
object ImpossiblePuzzle extends App {
  type XY = (Int, Int)
  val step0 = for {
    x <- 1 to 100
    y <- 1 to 100
    if 1 < x && x < y && x + y < 100
  } yield (x, y)

  def sum(xy: XY) = xy._1 + xy._2
  def prod(xy: XY) = xy._1 * xy._2
  def sumEq(xy: XY) = step0 filter { sum(_) == sum(xy) }
  def prodEq(xy: XY) = step0 filter { prod(_) == prod(xy) }

  val step2 = step0 filter { sumEq(_) forall { prodEq(_).size != 1 }}
  val step3 = step2 filter { prodEq(_).intersect(step2).size == 1 }
  val step4 = step3 filter { sumEq(_).intersect(step3).size == 1 }
  println(step4)
}
```

```txt
Vector((4,13))
```

Run-time: about 3.82 seconds.


## Scheme



```scheme

(import (scheme base)
        (scheme cxr)
        (scheme write)
        (srfi 1))

;; utility method to find unique sum/product in given list
(define (unique-items lst key)
  (let ((all-items (map key lst)))
    (filter (lambda (i) (= 1 (count (lambda (p) (= p (key i)))
                                    all-items)))
            lst)))

;; list of all (x y x+y x*y) combinations with y > x
(define *xy-pairs*
  (apply append
         (map (lambda (i)
                (map (lambda (j)
                       (list i j (+ i j) (* i j)))
                     (iota (- 98 i) (+ 1 i))))
              (iota 96 2))))

;; S says "P does not know X and Y"
(define *products* ; get products which have multiple decompositions
  (let ((all-products (map fourth *xy-pairs*)))
    (filter (lambda (p) (> (count (lambda (i) (= i p)) all-products) 1))
            all-products)))

(define *fact-1* ; every x+y has x*y in *products*
  (filter (lambda (i)
            (every (lambda (p) (memq (fourth p) *products*))
                   (filter (lambda (p) (= (third i) (third p))) *xy-pairs*)))
          *xy-pairs*))

;; P says "Now I know X and Y"
(define *fact-2* ; find the unique X*Y
  (unique-items *fact-1* fourth))

;; S says "Now I also know X and Y"
(define *fact-3* ; find the unique X+Y
  (unique-items *fact-2* third))

(display (string-append "Initial pairs: " (number->string (length *xy-pairs*)) "\n"))
(display (string-append "After S: " (number->string (length *fact-1*)) "\n"))
(display (string-append "After P: " (number->string (length *fact-2*)) "\n"))
(display (string-append "After S: " (number->string (length *fact-3*)) "\n"))
(display (string-append "X: "
                        (number->string (caar *fact-3*))
                        " Y: "
                        (number->string (cadar *fact-3*))
                        "\n"))

```


```txt

Initial pairs: 4656
After S: 145
After P: 86
After S: 1
X: 4 Y: 13

```



## Sidef

```ruby
func grep_uniq(a, by) { a.group_by{ .(by) }.values.grep{.len == 1}.map{_[0]} }
func sums     (n)     { 2 .. n//2 -> map {|i| [i, n-i] } }

var pairs = (2..97 -> map {|i| ([i] ~X (i+1 .. 98))... })

var p_uniq = Hash()
p_uniq{grep_uniq(pairs, :prod).map { .to_s }...} = ()

var s_pairs = pairs.grep {|p| sums(p.sum).all { !p_uniq.contains(.to_s) } }
var p_pairs = grep_uniq(s_pairs, :prod)
var f_pairs = grep_uniq(p_pairs, :sum)

f_pairs.each { |p| printf("X = %d, Y = %d\n", p...) }
```

```txt

X = 4, Y = 13

```



## zkl

Damn it Jim, I'm a programmer, not a logician. So I translated the python code found in https://qmaurmann.wordpress.com/2013/08/10/sam-and-polly-and-python/ but I don't understand it. It does seem quite a bit more efficient than the Scala code, on par with the Python code.

```zkl
mul:=Utils.Helpers.summer.fp1('*,1); //-->list.reduce('*,1), multiply list items
var allPairs=[[(a,b); [2..100]; { [a+1..100] },{ a+b<100 }; ROList]]; // 2,304 pairs

sxys,pxys:=Dictionary(),Dictionary();  // hashes of allPairs sums and products: 95,1155
foreach xy in (allPairs){ sxys.appendV(xy.sum(),xy); pxys.appendV(xy:mul(_),xy) }

sOK:= 'wrap(s){ (not sxys[s].filter1('wrap(xy){ pxys[xy:mul(_)].len()<2 })) };
pOK:= 'wrap(p){ 1==pxys[p].filter('wrap([(x,y)]){ sOK(x+y) }).len() };
sOK2:='wrap(s){ 1==sxys[s].filter('wrap(xy){ pOK(xy:mul(_)) }).len() };
allPairs.filter('wrap([(x,y)]){ sOK(x+y) and pOK(x*y) and sOK2(x+y) })
.println();
```

[[ ]] denotes list comprehension, filter1 returns (and stops at) the first thing that is "true", 'wrap creates a closure so the "wrapped" code/function can see local variables (read only). In a [function] prototype, the "[(x,y)]xy]" notation says xy is a list like thing, assign the parts to x & y (xy is optional), used here to just to do it both ways. The ":" says take the LHS and stuff it into the "_".
```txt
L(L(4,13))
```

