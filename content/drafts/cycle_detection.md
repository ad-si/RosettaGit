+++
title = "Cycle detection"
description = ""
date = 2019-08-22T14:35:34Z
aliases = []
[extra]
id = 20050
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

;Task:
Detect a cycle in an iterated function using Brent's algorithm.


Detecting cycles in iterated function sequences is a sub-problem in many computer algorithms, such as factoring prime numbers. Some such algorithms are highly space efficient, such as Floyd's cycle-finding algorithm, also called the "tortoise and the hare algorithm". A more time efficient algorithm than "tortoise and hare" is Brent's Cycle algorithm. This task will implement Brent's algorithm.

See https://en.wikipedia.org/wiki/Cycle_detection for a discussion of the theory and discussions of other algorithms that are used to solve the problem.

When testing the cycle detecting function, you need two things:

1) An iterated function

2) A starting value

The iterated function used in this example is: f(x) = (x*x + 1) modulo 255.

The starting value used is 3.

With these as inputs, a sample program output would be:

'''3,10,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5'''

'''Cycle length = 6'''

'''Start index = 2'''

The output prints the first several items in the number series produced by the iterated function, then identifies how long the cycle is (6) followed by the zero-based index of the start of the first cycle (2). From this you can see that the cycle is:

101,2,5,26,167,95


## 11l

{{trans|D}}

```11l
F brent(f, x0)
   Int cycle_length
   V hare = x0
   V power = 1
   L
      V tortoise = hare
      L(i) 1..power
         hare = f(hare)
         I tortoise == hare
            cycle_length = i
            ^L.break
      power *= 2

   hare = x0
   L 1..cycle_length
      hare = f(hare)

   V cycle_start = 0
   V tortoise = x0
   L tortoise != hare
      tortoise = f(tortoise)
      hare = f(hare)
      cycle_start++

   print_result(x0, f, cycle_length, cycle_start)

F print_result(x0, f, len, start)
   print(‘Cycle length = ’len)
   print(‘Start index = ’start)
   V i = x0
   L 1..start
      i = f(i)
   V cycle = [0] * len
   L 0.<len
      cycle[L.index] = i
      i = f(i)
   print(‘Cycle: ’, end' ‘’)
   print(cycle)

brent(i -> (i * i + 1) % 255, 3)
```

{{out}}

```txt

Cycle length = 6
Start index = 2
Cycle: [101, 2, 5, 26, 167, 95]

```



## C

{{trans|Modula-2}}

```c
#include <stdio.h>
#include <stdlib.h>

typedef int(*I2I)(int);
typedef struct {
    int a, b;
} Pair;

Pair brent(I2I f, int x0) {
    int power = 1, lam = 1, tortoise = x0, hare, mu, i;
    Pair result;

    hare = (*f)(x0);
    while (tortoise != hare) {
        if (power == lam) {
            tortoise = hare;
            power = power * 2;
            lam = 0;
        }
        hare = (*f)(hare);
        lam++;
    }

    hare = x0;
    i = 0;
    while (i < lam) {
        hare = (*f)(hare);
        i++;
    }

    tortoise = x0;
    mu = 0;
    while (tortoise != hare) {
        tortoise = (*f)(tortoise);
        hare = (*f)(hare);
        mu++;
    }

    result.a = lam;
    result.b = mu;
    return result;
}

int lambda(int x) {
    return (x*x + 1) % 255;
}

int main() {
    int x0 = 3, x = 3, i;
    Pair result;

    printf("[3");
    for (i = 1; i <= 40; ++i) {
        x = lambda(x);
        printf(", %d", x);
    }
    printf("]\n");

    result = brent(lambda, x0);
    printf("Cycle length = %d\nStart index = %d\nCycle = [", result.a, result.b);

    x0 = 3;
    x = x0;
    for (i = 1; i <= result.b; ++i) {
        x = lambda(x);
    }
    for (i = 1; i <= result.a; ++i) {
        if (i > 1) {
            printf(", ");
        }

        printf("%d", x);
        x = lambda(x);
    }

    printf("]\n");

    return 0;
}
```

{{out}}

```txt
[3, 10, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5]
Cycle length = 6
Start index = 2
Cycle = [101, 2, 5, 26, 167, 95]
```



## C++


```cpp
struct ListNode {
      int val;
      ListNode *next;
      ListNode(int x) : val(x), next(NULL) {}
 };

ListNode* Solution::detectCycle(ListNode* A) {
    ListNode* slow = A;
    ListNode* fast = A;
    ListNode* cycleNode = 0;
    while (slow && fast && fast->next)
    {
        slow = slow->next;
        fast = fast->next->next;
        if (slow == fast)
        {
            cycleNode = slow;
            break;
        }
    }
    if (cycleNode == 0)
    {
        return 0;
    }
    std::set<ListNode*> setPerimeter;
    setPerimeter.insert(cycleNode);
    for (ListNode* pNode = cycleNode->next; pNode != cycleNode; pNode = pNode->next)
    setPerimeter.insert(pNode);
    for (ListNode* pNode = A; true; pNode = pNode->next)
    {
        std::set<ListNode*>::iterator iter = setPerimeter.find(pNode);
        if (iter != setPerimeter.end())
        {
            return pNode;
        }
    }
}
```


=={{header|C#|C_sharp}}==
This solution uses generics, so may find cycles of any type of data, not just integers.


```c#


// First file: Cycles.cs
// Author: Paul Anton Chernoch

using System;

namespace DetectCycles
{
  /// <summary>
  /// Find the length and start of a cycle in a series of objects of any IEquatable type using Brent's cycle algorithm.
  /// </summary>
  public class Cycles<T> where T : IEquatable<T>
  {
    /// <summary>
    /// Find the cycle length and start position of a series using Brent's cycle algorithm.
    ///
    ///  Given a recurrence relation X[n+1] = f(X[n]) where f() has
    ///  a finite range, you will eventually repeat a value that you have seen before.
    ///  Once this happens, all subsequent values will form a cycle that begins
    ///  with the first repeated value. The period of that cycle may be of any length.
    /// </summary>
    /// <returns>A tuple where:
    ///    Item1 is lambda (the length of the cycle)
    ///    Item2 is mu, the zero-based index of the item that started the first cycle.</returns>
    /// <param name="x0">First item in the series.</param>
    /// <param name="yielder">Function delegate that generates the series by iterated execution.</param>
    public static Tuple<int,int> FindCycle(T x0, Func<T,T> yielder)
    {
      int power, lambda;
      T tortoise, hare;
      power = lambda = 1;
      tortoise = x0;
      hare = yielder(x0);

      // Find lambda, the cycle length
      while (!tortoise.Equals (hare)) {
        if (power == lambda) {
          tortoise = hare;
          power *= 2;
          lambda = 0;
        }
        hare = yielder (hare);
        lambda += 1;
      }

      // Find mu, the zero-based index of the start of the cycle
      var mu = 0;
      tortoise = hare = x0;
      for (var times = 0; times < lambda; times++)
        hare = yielder (hare);

      while (!tortoise.Equals (hare))
      {
        tortoise = yielder (tortoise);
        hare = yielder (hare);
        mu += 1;
      }

      return new Tuple<int,int> (lambda, mu);
    }
  }
}

// Second file: Program.cs

using System;

namespace DetectCycles
{
	class MainClass
	{
		public static void Main (string[] args)
		{
			// A recurrence relation to use in testing
			Func<int,int> sequence = (int _x) => (_x * _x + 1) % 255;

			// Display the first 41 numbers in the test series
			var x = 3;
			Console.Write(x);
			for (var times = 0; times < 40; times++)
			{
				x = sequence(x);
				Console.Write(String.Format(",{0}", x));
			}
			Console.WriteLine();

			// Test the FindCycle method
			var cycle = Cycles<int>.FindCycle(3, sequence);
			var clength = cycle.Item1;
			var cstart = cycle.Item2;
			Console.Write(String.Format("Cycle length = {0}\nStart index = {1}\n", clength, cstart));
		}
	}
}


```



## D

{{trans|Java}}

```D
import std.range;
import std.stdio;

void main() {
    brent(i => (i * i + 1) % 255, 3);
}

void brent(int function(int) f, int x0) {
    int cycleLength;
    int hare = x0;
    FOUND:
    for (int power = 1; ; power *= 2) {
        int tortoise = hare;
        for (int i = 1; i <= power; i++) {
            hare = f(hare);
             if (tortoise == hare) {
                cycleLength = i;
                break FOUND;
            }
        }
    }

    hare = x0;
    for (int i = 0; i < cycleLength; i++)
        hare = f(hare);

    int cycleStart = 0;
    for (int tortoise = x0; tortoise != hare; cycleStart++) {
        tortoise = f(tortoise);
        hare = f(hare);
    }

    printResult(x0, f, cycleLength, cycleStart);
}

void printResult(int x0, int function(int) f, int len, int start) {
    writeln("Cycle length: ", len);
    writefln("Cycle: %(%s %)", iterate(x0, f).drop(start).take(len));
}

auto iterate(int start, int function(int) f) {
    return only(start).chain(generate!(() => start=f(start)));
}
```

{{out}}

```txt
Cycle length: 6
Cycle: 101 2 5 26 167 95
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Cycle_detection do
  def find_cycle(x0, f) do
    lambda = find_lambda(f, x0, f.(x0), 1, 1)
    hare = Enum.reduce(1..lambda, x0, fn _,hare -> f.(hare) end)
    mu = find_mu(f, x0, hare, 0)
    {lambda, mu}
  end

  # Find lambda, the cycle length
  defp find_lambda(_, tortoise, hare, _, lambda) when tortoise==hare, do: lambda
  defp find_lambda(f, tortoise, hare, power, lambda) do
    if power == lambda, do: find_lambda(f, hare, f.(hare), power*2, 1),
                      else: find_lambda(f, tortoise, f.(hare), power, lambda+1)
  end

  # Find mu, the zero-based index of the start of the cycle
  defp find_mu(_, tortoise, hare, mu) when tortoise==hare, do: mu
  defp find_mu(f, tortoise, hare, mu) do
    find_mu(f, f.(tortoise), f.(hare), mu+1)
  end
end

# A recurrence relation to use in testing
f = fn(x) -> rem(x * x + 1, 255) end

# Display the first 41 numbers in the test series
Stream.iterate(3, &f.(&1)) |> Enum.take(41) |> Enum.join(",") |> IO.puts

# Test the find_cycle function
{clength, cstart} = Cycle_detection.find_cycle(3, f)
IO.puts "Cycle length = #{clength}\nStart index = #{cstart}"
```


{{out}}

```txt

3,10,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5
Cycle length = 6
Start index = 2

```



## Factor

This is a strict translation of the Python code from the Wikipedia article. Perhaps a more idiomatic version could be added in the future, although there is value in showing how Factor's lexical variables differ from most languages. A variable binding with <code>!</code> at the end is mutable, and subsequent uses of that name followed by <code>!</code> change the value of the variable to the value at the top of the data stack.

```factor
USING: formatting kernel locals make math prettyprint ;

: cyclical-function ( n -- m ) dup * 1 + 255 mod ;

:: brent ( x0 quot -- λ μ )
    1 dup :> ( power! λ! )
    x0 :> tortoise!
    x0 quot call :> hare!
    [ tortoise hare = not ] [
        power λ = [
            hare tortoise!
            power 2 * power!
            0 λ!
        ] when
        hare quot call hare!
        λ 1 + λ!
    ] while

    0 :> μ!
    x0 dup tortoise! hare!
    λ [ hare quot call hare! ] times
    [ tortoise hare = not ] [
        tortoise quot call tortoise!
        hare quot call hare!
        μ 1 + μ!
    ] while
    λ μ ; inline

3 [ 20 [ dup , cyclical-function ] times ] { } make nip .
3 [ cyclical-function ] brent
"Cycle length: %d\nCycle start: %d\n" printf
```

{{out}}

```txt

{ 3 10 101 2 5 26 167 95 101 2 5 26 167 95 101 2 5 26 167 95 }
Cycle length: 6
Cycle start: 2

```



## FreeBASIC

===Brent's algorithm===
{{trans|Python}}

```freebasic
' version 11-01-2017
' compile with: fbc -s console

' define the function f(x)=(x*x +1) mod 255
Function f(x As Integer) As Integer
    Return (x * x +1) Mod 255
End Function

Sub brent(x0 As Integer, ByRef lam As Integer, ByRef mu As Integer)

    Dim As Integer i, power = 1
    lam = 1

    ' main phase: search successive powers of two
    Dim As Integer tortoise = f(x0)   ' f(x0) is the element/node next to x0.
    Dim As Integer hare = f(f(x0))

    While tortoise <> hare
        If power = lam Then
            tortoise = hare
            power *= 2
            lam = 0
        End If
        hare = f(hare)
        lam += 1
    Wend

    ' Find the position of the first repetition of length ?
    mu = 0
    tortoise = x0
    hare = x0
    For i = 0 To lam -1
        ' range(lam) produces a list with the values 0, 1, ... , lam-1
        hare = f(hare)
    Next
    ' The distance between the hare and tortoise is now ?.

    ' Next, the hare and tortoise move at same speed until they agree
    While tortoise <> hare
        tortoise = f(tortoise)
        hare = f(hare)
        mu += 1
    Wend

End Sub

' ------=< MAIN >=------

Dim As Integer  i, j, lam, mu, x0 = 3

brent(x0, lam, mu)
Print " Brent's algorithm"
Print " Cycle starts at position: "; mu; " (starting position = 0)"
Print " The length of the Cycle = "; lam
Print

j = f(x0)
Print " Cycle: ";
For i = 1 To lam + mu -1
    If i >= mu Then
        Print j;
        If i <> (lam + mu -1) Then Print ", "; Else Print ""
    End If
    j = f(j)
Next
Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
 Brent's algorithm
 Cycle starts at position:  2 (starting position = 0)
 The length of the Cycle =  6

 Cycle:  101,  2,  5,  26,  167,  95
```

===Tortoise and hare. Floyd's algorithm===
{{trans|Wikipedia}}

```freebasic
' version 11-01-2017
' compile with: fbc -s console

' define the function f(x)=(x*x +1) mod 255
Function f(x As Integer) As Integer
    Return (x * x +1) Mod 255
End Function

Sub floyd(x0 As Integer, ByRef lam As Integer, ByRef mu As Integer)

    ' Main phase of algorithm: finding a repetition x_i = x_2i.
    ' The hare moves twice as quickly as the tortoise and
    ' the distance between them increases by 1 at each step.
    ' Eventually they will both be inside the cycle and then,
    ' at some point, the distance between them will be
    ' divisible by the period ?.
    Dim As Integer tortoise = f(x0)   ' f(x0) is the element/node next to x0.
    Dim As Integer hare = f(f(x0))

    While tortoise <> hare
        tortoise = f(tortoise)
        hare = f(f(hare))
    Wend

    ' At this point the tortoise position, ?, which is also equal
    ' to the distance between hare and tortoise, is divisible by
    ' the period ?. So hare moving in circle one step at a time,
    ' and tortoise (reset to x0) moving towards the circle, will
    ' intersect at the beginning of the circle. Because the
    ' distance between them is constant at 2?, a multiple of ?,
    ' they will agree as soon as the tortoise reaches index µ.

    ' Find the position µ of first repetition.
    mu = 0
    tortoise = x0
    While tortoise <> hare
        tortoise = f(tortoise)
        hare = f(hare)         ' Hare and tortoise move at same speed
        mu += 1
    Wend

    ' Find the length of the shortest cycle starting from x_µ
    ' The hare moves one step at a time while tortoise is still.
    ' lam is incremented until ? is found.
    lam = 1
    hare = f(tortoise)
    While tortoise <> hare
        hare = f(hare)
        lam += 1
    Wend

End Sub


' ------=< MAIN >=------

Dim As Integer  i, j, lam, mu, x0 = 3

floyd(x0, lam, mu)
Print " Tortoise and hare. Floyd's algorithm"
Print " Cycle starts at position: "; mu; " (starting position = 0)"
Print " The length of the Cycle = "; lam
Print

j = f(x0)
Print " Cycle: ";
For i = 1 To lam + mu -1
    If i >= mu Then
        Print j;
        If i <> (lam + mu -1) Then Print ", "; Else Print ""
    End If
    j = f(j)

Next
Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```



## Go

{{trans|Python}}
{{trans|Wikipedia}}

Run it on the [https://play.golang.org/p/unOtxuwZfg go playground], or on [https://goplay.space/#S1pQZSuJci go play space].

```Go

package main

import "fmt"

func brent(f func(i int) int, x0 int) (int, int) {
	var λ, µ, power, tortoise, hare int

	// Main phase: search successive powers of two.
	power = 1
	λ = 1
	tortoise = x0
	hare = f(x0) // f(x0) is the element/node next to x0.

	for tortoise != hare {
		if power == λ { // Time to start a new power of two.
			tortoise = hare
			power *= 2
			λ = 0
		}
		hare = f(hare)
		λ++
	}

	// Find the position of the first repetition of length λ.
	µ = 0
	tortoise, hare = x0, x0
	for i := 0; i < λ; i++ {
		// produces a list with the values 0,1,...,λ-1.
		hare = f(hare)
		// The distance between hare and tortoise is now λ.
	}

	// The tortoise and the hare move at the same speed until they agree.
	for tortoise != hare {
		tortoise = f(tortoise)
		hare = f(hare)
		µ++
	}

	return λ, µ
}

func f(i int) int {
	return (i*i + 1) % 255
}

func main() {
	x0 := 3
	λ, µ := brent(f, x0)
	fmt.Println("Cycle length:", λ)
	fmt.Println("Cycle start index:", µ)
	a := []int{}
	for i := 0; i <= λ+1; i++ {
		a = append(a, x0)
		x0 = f(x0)
	}
	fmt.Println("Cycle:", a[µ:µ+λ])
}
```


{{out}}

```txt
Cycle length: 6
Cycle start index: 2
Cycle: [101 2 5 26 167 95]
```



## Haskell


Most of solutions given in other languages are not total. For function which does not have any cycles under iteration (i.e. f(x) = 1 + x) they will never terminate.

Haskellers, being able to handle conceptually infinite structures, traditionally consider totality  as an important issue. The following solution is total for total inputs (modulo totality of iterated function) and allows nontermination only if input is explicitly infinite.


```haskell
import Data.List (findIndex)

findCycle :: Eq a => [a] -> Maybe ([a], Int, Int)
findCycle lst =
  do l <- findCycleLength lst
     mu <- findIndex (uncurry (==)) $ zip lst (drop l lst)
     let c = take l $ drop mu lst
     return (c, l, mu)

findCycleLength :: Eq a => [a] -> Maybe Int
findCycleLength [] = Nothing
findCycleLength (x:xs) =
  let loop _ _ _ [] = Nothing
      loop pow lam x (y:ys)
        | x == y     = Just lam
        | pow == lam = loop (2*pow) 1 y ys
        | otherwise  = loop pow (1+lam) x ys
  in loop 1 1 x xs
```


'''Examples'''


```txt
λ> findCycle (cycle [1,2,3])
Just ([1,2,3],3,0)

λ> findCycle ([1..100] ++ cycle [1..12])
Just ([1,2,3,4,5,6,7,8,9,10,11,12],12,100)

λ> findCycle [1..1000]
Nothing

```



```txt

λ> findCycle (iterate (\x -> (x^2 + 1) `mod` 255) 3)
Just ([101,2,5,26,167,95],6,2)

λ> findCycle (take 100 $ iterate (\x -> x+1) 3)
Nothing

λ> findCycle (take 100000 $ iterate (\x -> x+1) 3)
Nothing

```




## J


Brute force implementation:


```J
cdetect=:1 :0
  r=. ~.@(,u@{:)^:_ y
  n=. u{:r
  (,(#r)-])r i. n
)
```


In other words: iterate until we stop getting new values, find the repeated value, and see how it fits into the sequence.

Example use:


```J
   255&|@(1 0 1&p.) cdetect 3
2 6
```


(Note that 1 0 1 are the coefficients of the polynomial <code>1 + (0 * x) + (1 * x * x)</code> or, if you prefer <code>(1 * x ^ 0) + (0 * x ^ 1) + (1 * x ^ 2)</code> - it's easier and probably more efficient to just hand the coefficients to p. than it is to write out some other expression which produces the same result.)


## Java

{{works with|Java|8}}

```java
import java.util.function.*;
import static java.util.stream.IntStream.*;

public class CycleDetection {

    public static void main(String[] args) {
        brent(i -> (i * i + 1) % 255, 3);
    }

    static void brent(IntUnaryOperator f, int x0) {
        int cycleLength;
        int hare = x0;
        FOUND:
        for (int power = 1; ; power *= 2) {
            int tortoise = hare;
            for (int i = 1; i <= power; i++) {
                hare = f.applyAsInt(hare);
                 if (tortoise == hare) {
                    cycleLength = i;
                    break FOUND;
                }
            }
        }

        hare = x0;
        for (int i = 0; i < cycleLength; i++)
            hare = f.applyAsInt(hare);

        int cycleStart = 0;
        for (int tortoise = x0; tortoise != hare; cycleStart++) {
            tortoise = f.applyAsInt(tortoise);
            hare = f.applyAsInt(hare);
        }

        printResult(x0, f, cycleLength, cycleStart);
    }

    static void printResult(int x0, IntUnaryOperator f, int len, int start) {
        System.out.printf("Cycle length: %d%nCycle: ", len);
        iterate(x0, f).skip(start).limit(len)
                .forEach(n -> System.out.printf("%s ", n));
    }
}
```



```txt
Cycle length: 6
Cycle: 101 2 5 26 167 95
```



## Julia

{{works with|Julia|0.6}}

Following the Wikipedia article:


```julia
using IterTools

function floyd(f, x0)
    local tort = f(x0)
    local hare = f(tort)
    while tort != hare
        tort = f(tort)
        hare = f(f(hare))
    end

    local μ = 0
    tort = x0
    while tort != hare
        tort = f(tort)
        hare = f(hare)
        μ += 1
    end

    λ = 1
    hare = f(tort)
    while tort != hare
        hare = f(hare)
        λ += 1
    end

    return λ, μ
end

f(x) = (x * x + 1) % 255

λ, μ = floyd(f, 3)
cycle = iterate(f, 3) |>
    x -> Iterators.drop(x, μ) |>
    x -> Iterators.take(x, λ) |>
    collect
println("Cycle length: ", λ, "\nCycle start index: ", μ, "\nCycle: ", join(cycle, ", "))
```


{{out}}

```txt
Cycle length: 6
Cycle start index: 2
Cycle: 101, 2, 5, 26, 167, 95
```



## Kotlin


```scala
// version 1.1.2

typealias IntToInt = (Int) -> Int

fun brent(f: IntToInt, x0: Int): Pair<Int, Int> {
    // main phase: search successive powers of two
    var power = 1
    var lam = 1
    var tortoise = x0
    var hare = f(x0)  // f(x0) is the element/node next to x0.
    while (tortoise != hare) {
        if (power == lam) {  // time to start a new power of two?
            tortoise = hare
            power *= 2
            lam = 0
        }
        hare = f(hare)
        lam++
    }

    // Find the position of the first repetition of length 'lam'
    var mu = 0
    tortoise = x0
    hare = x0
    for (i in 0 until lam) hare = f(hare)

    // The distance between the hare and tortoise is now 'lam'.
    // Next, the hare and tortoise move at same speed until they agree
    while (tortoise != hare) {
        tortoise = f(tortoise)
        hare = f(hare)
        mu++
    }
    return Pair(lam, mu)
}

fun main(args: Array<String>) {
    val f = { x: Int -> (x * x + 1) % 255 }
    // generate first 41 terms of the sequence starting from 3
    val x0 = 3
    var x = x0
    val seq = List(41) { if (it > 0) x = f(x) ; x }
    println(seq)
    val (lam, mu) = brent(f, x0)
    val cycle = seq.slice(mu until mu + lam)
    println("Cycle length = $lam")
    println("Start index  = $mu")
    println("Cycle        = $cycle")
}
```


{{out}}

```txt

[3, 10, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5]
Cycle length = 6
Start index  = 2
Cycle        = [101, 2, 5, 26, 167, 95]

```



## Lua

Fairly direct translation of the Wikipedia code, except that the sequence is stored in a table and passed back as a third return value.

```Lua
function brent (f, x0)
    local tortoise, hare, mu = x0, f(x0), 0
    local cycleTab, power, lam = {tortoise, hare}, 1, 1
    while tortoise ~= hare do
        if power == lam then
            tortoise = hare
            power = power * 2
            lam = 0
        end
        hare = f(hare)
        table.insert(cycleTab, hare)
        lam = lam + 1
    end
    tortoise, hare = x0, x0
    for i = 1, lam do hare = f(hare) end
    while tortoise ~= hare do
        tortoise = f(tortoise)
        hare = f(hare)
        mu = mu + 1
    end
    return lam, mu, cycleTab
end

local f = function (x) return (x * x + 1) % 255 end
local x0 = 3
local cycleLength, startIndex, sequence = brent(f, x0)
print("Sequence:", table.concat(sequence, " "))
print("Cycle length:", cycleLength)
print("Start Index:", startIndex)
```

{{out}}

```txt
Sequence:       3 10 101 2 5 26 167 95 101 2 5 26 167 95
Cycle length:   6
Start Index:    2
```


=={{header|Modula-2}}==
{{trans|Kotlin}}

```modula2
MODULE CycleDetection;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE IntToInt = PROCEDURE(INTEGER) : INTEGER;
TYPE Pair =
    RECORD
        a,b : INTEGER;
    END;

PROCEDURE Brent(f : IntToInt; x0 : INTEGER) : Pair;
VAR power,lam,tortoise,hare,mu,i : INTEGER;
BEGIN
    (* main phase: search successive powers of two *)
    power := 1;
    lam := 1;
    tortoise := x0;
    hare := f(x0);  (* f(x0) is the element/node next to x0. *)
    WHILE tortoise # hare DO
        (* time to start a new power of two? *)
        IF power = lam THEN
            tortoise := hare;
            power := power * 2;
            lam := 0
        END;
        hare := f(hare);
        INC(lam)
    END;

    (* Find the position of the first repetition of length 'lam' *)
    mu := 0;
    tortoise := x0;
    hare := x0;
    i := 0;
    WHILE i < lam DO
        hare := f(hare);
        INC(i)
    END;

    (* The distance between the hare and tortoise is now 'lam'.
       Next, the hare and tortoise move at same speed until they agree *)
    WHILE tortoise # hare DO
        tortoise := f(tortoise);
        hare := f(hare);
        INC(mu)
    END;

    RETURN Pair{lam,mu}
END Brent;

PROCEDURE Lambda(x : INTEGER) : INTEGER;
BEGIN
    RETURN (x * x + 1) MOD 255
END Lambda;

VAR
    buf : ARRAY[0..63] OF CHAR;
    x0,x,i : INTEGER;
    result : Pair;
BEGIN
    x0 := 3;
    x := x0;

    WriteString("[3");
    FOR i:=1 TO 40 DO
        x := Lambda(x);
        FormatString(", %i", buf, x);
        WriteString(buf)
    END;
    WriteString("]");
    WriteLn;

    result := Brent(Lambda, x0);
    FormatString("Cycle length = %i\nStart index  = %i\nCycle        = [", buf, result.a, result.b);
    WriteString(buf);

    x0 := 3;
    x := x0;
    FOR i:=1 TO result.b DO
        x := Lambda(x)
    END;
    FOR i:=1 TO result.a DO
        IF i > 1 THEN
            WriteString(", ")
        END;

        FormatString("%i", buf, x);
        WriteString(buf);
        x := Lambda(x)
    END;

    WriteString("]");
    WriteLn;

    ReadChar
END CycleDetection.
```



## ooRexx


```ooRexx
/* REXX */
x=3
list=x
Do i=1 By 1
  x=f(x)
  p=wordpos(x,list)
  If p>0 Then Do
    list=list x
    Say 'list ='list '...'
    Say 'Start index = ' (wordpos(x,list)-1) '(zero based)'
    Say 'Cycle length =' (words(list)-p)
    Say 'Cycle        =' subword(list,p,(words(list)-p))
    Leave
    End
  list=list x
  End
Exit
f: Return (arg(1)**2+1)//255
```

{{out}}

```txt

list =3 10 101 2 5 26 167 95 101 ...
Start index =  2 (zero based)
Cycle length = 6
Cycle        = 101 2 5 26 167 95

```



## Perl

{{trans|Perl 6}}

```perl
use utf8;

sub cyclical_function { ($_[0] * $_[0] + 1) % 255 }

sub brent {
    my($f, $x0) = @_;
    my $power = 1;
    my $λ = 1;
    my $tortoise = $x0;
    my $hare = &$f($x0);
    while ($tortoise != $hare) {
        if ($power == $λ) {
            $tortoise = $hare;
            $power *= 2;
            $λ = 0;
        }
        $hare = &$f($hare);
        $λ += 1;
    }

    my $μ = 0;
    $tortoise = $hare = $x0;
    $hare = &$f($hare) for 0..$λ-1;

    while ($tortoise != $hare) {
        $tortoise = &$f($tortoise);
        $hare = &$f($hare);
        $μ += 1;
    }
    return $λ, $μ;
}

my ( $l, $s ) = brent( \&cyclical_function, 3 );

sub show_range {
    my($start,$stop) = @_;
    my $result;
    my $x = 3;
    for my $n (0..$stop) {
        $result .= "$x " if $n >= $start;
        $x = cyclical_function($x);
    }
    $result;
}

print show_range(0,19) . "\n";
print "Cycle length $l\n";
print "Cycle start index $s\n";
print show_range($s,$s+$l-1) . "\n";
```

{{out}}

```txt
3 10 101 2 5 26 167 95 101 2 5 26 167 95 101 2 5 26 167 95
Cycle length 6
Cycle start index 2
101 2 5 26 167 95
```



## Perl 6

{{works with|Rakudo|2016-01}}
Pretty much a line for line translation of the Python code on the Wikipedia page.


```perl6
sub cyclical-function (\x) { (x * x + 1) % 255 };

my ( $l, $s ) = brent( &cyclical-function, 3 );

put join ', ', (3, -> \x { cyclical-function(x) } ... *)[^20], '...';
say "Cycle length $l.";
say "Cycle start index $s.";
say (3, -> \x { cyclical-function(x) } ... *)[$s .. $s + $l - 1];

sub brent (&f, $x0) {
    my $power = my $λ = 1;
    my $tortoise = $x0;
    my $hare = f($x0);
    while ($tortoise != $hare) {
        if $power == $λ {
            $tortoise = $hare;
            $power *= 2;
            $λ = 0;
        }
        $hare = f($hare);
        $λ += 1;
    }

    my $μ = 0;
    $tortoise = $hare = $x0;
    $hare = f($hare) for ^$λ;

    while ($tortoise != $hare) {
        $tortoise = f($tortoise);
        $hare = f($hare);
        $μ += 1;
    }
    return $λ, $μ;
}
```

{{out}}

```txt
3, 10, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, ...
Cycle length 6.
Cycle start index 2.
(101 2 5 26 167 95)
```



## Phix

Translation of the Wikipedia code, but using the more descriptive len and pos, instead of lambda and mu, and adding a limit.

```Phix
function f(integer x)
    return mod(x*x+1,255)
end function

function brent(integer x0)
integer pow2 = 1, len = 1, pos = 1
integer tortoise = x0,
        hare = f(x0)
sequence s = {tortoise,hare} -- (kept for output only)

    -- main phase: search successive powers of two
    while tortoise!=hare do
        if pow2 = len then
            tortoise = hare
            pow2 *= 2
            if pow2>16 then
                return {s,0,0}
            end if
            len = 0
        end if
        hare = f(hare)
        s &= hare
        len += 1
    end while

    -- Find the position of the first repetition of length len
    tortoise = x0
    hare = x0
    for i=1 to len do
        hare = f(hare)
    end for
    -- The distance between the hare and tortoise is now len.

    -- Next, the hare and tortoise move at same speed until they agree
    while tortoise<>hare do
        tortoise = f(tortoise)
        hare = f(hare)
        pos += 1
    end while

    return {s,len,pos}
end function

sequence s
integer len, pos, x0 = 3
{s,len,pos} = brent(x0)
printf(1," Brent's algorithm\n")
?s
printf(1," Cycle starts at position: %d (1-based)\n",{pos})
printf(1," The length of the Cycle = %d\n",{len})
?s[pos..pos+len-1]
```

{{out}}

```txt

 Brent's algorithm
{3,10,101,2,5,26,167,95,101,2,5,26,167,95}
 Cycle starts at position: 3 (1-based)
 The length of the Cycle = 6
{101,2,5,26,167,95}

```



## Python


### Procedural

Function from the Wikipedia article:

```python
import itertools

def brent(f, x0):
    # main phase: search successive powers of two
    power = lam = 1
    tortoise = x0
    hare = f(x0)  # f(x0) is the element/node next to x0.
    while tortoise != hare:
        if power == lam:  # time to start a new power of two?
            tortoise = hare
            power *= 2
            lam = 0
        hare = f(hare)
        lam += 1

    # Find the position of the first repetition of length lam
    mu = 0
    tortoise = hare = x0
    for i in range(lam):
    # range(lam) produces a list with the values 0, 1, ... , lam-1
        hare = f(hare)
    # The distance between the hare and tortoise is now lam.

    # Next, the hare and tortoise move at same speed until they agree
    while tortoise != hare:
        tortoise = f(tortoise)
        hare = f(hare)
        mu += 1

    return lam, mu

def iterate(f, x0):
    while True:
        yield x0
        x0 = f(x0)

if __name__ == '__main__':
    f = lambda x: (x * x + 1) % 255
    x0 = 3
    lam, mu = brent(f, x0)
    print("Cycle length: %d" % lam)
    print("Cycle start index: %d" % mu)
    print("Cycle: %s" % list(itertools.islice(iterate(f, x0), mu, mu+lam)))
```

{{out}}

```txt

Cycle length: 6
Cycle start index: 2
Cycle: [101, 2, 5, 26, 167, 95]

```


A modified version of the above where the first stage is restructured for clarity:

```python
import itertools

def brent_length(f, x0):
    # main phase: search successive powers of two
    hare = x0
    power = 1
    while True:
        tortoise = hare
        for i in range(1, power+1):
            hare = f(hare)
            if tortoise == hare:
                return i
        power *= 2

def brent(f, x0):
    lam = brent_length(f, x0)

    # Find the position of the first repetition of length lam
    mu = 0
    hare = x0
    for i in range(lam):
    # range(lam) produces a list with the values 0, 1, ... , lam-1
        hare = f(hare)
    # The distance between the hare and tortoise is now lam.

    # Next, the hare and tortoise move at same speed until they agree
    tortoise = x0
    while tortoise != hare:
        tortoise = f(tortoise)
        hare = f(hare)
        mu += 1

    return lam, mu

def iterate(f, x0):
    while True:
        yield x0
        x0 = f(x0)

if __name__ == '__main__':
    f = lambda x: (x * x + 1) % 255
    x0 = 3
    lam, mu = brent(f, x0)
    print("Cycle length: %d" % lam)
    print("Cycle start index: %d" % mu)
    print("Cycle: %s" % list(itertools.islice(iterate(f, x0), mu, mu+lam)))
```

{{out}}

```txt
Cycle length: 6
Cycle start index: 2
Cycle: [101, 2, 5, 26, 167, 95]
```



### Functional

In functional terms, the problem lends itself at first sight (see the Haskell version), to a reasonably compact recursive definition of a ''cycleLength'' function:
{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Cycle detection by recursion.'''

from itertools import (chain, cycle, islice)
from operator import (eq)


# cycleFound :: Eq a => [a] -> Maybe ([a], Int, Int)
def cycleFound(xs):
    '''Just the first cycle found, with its length
       and start index, or Nothing if no cycle seen.
    '''
    return bind(cycleLength(xs))(
        lambda n: bind(
            findIndex(uncurry(eq))(zip(xs, xs[n:]))
        )(lambda iStart: Just(
            (xs[iStart:iStart + n], n, iStart)
        ))
    )


# cycleLength :: Eq a => [a] -> Maybe Int
def cycleLength(xs):
    '''Just the length of the first cycle found,
       or Nothing if no cycle seen.
    '''
    def go(pwr, lng, x, ys):
        if ys:
            y, *yt = ys
            return Just(lng) if x == y else (
                go(2 * pwr, 1, y, yt) if (
                    lng == pwr
                ) else go(pwr, 1 + lng, x, yt)
            )
        else:
            return Nothing()

    return go(1, 1, xs[0], xs[1:]) if xs else Nothing()


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Reports of any cycle detection.'''

    print(
        fTable(
            'First cycle detected, if any:\n'
        )(fst)(maybe('No cycle found')(
            showCycle
        ))(
            compose(cycleFound)(snd)
        )([
            (
                'cycle([1, 2, 3])',
                take(1000)(cycle([1, 2, 3]))
            ), (
                '[0..100] + cycle([1..8])',
                take(1000)(
                    chain(
                        enumFromTo(0)(100),
                        cycle(enumFromTo(1)(8))
                    )
                )
            ), (
                '[1..500]',
                enumFromTo(1)(500)
            ), (
                'f(x) = (x*x + 1) modulo 255',
                take(1000)(iterate(
                    lambda x: (1 + (x * x)) % 255
                )(3))
            )
        ])
    )


# DISPLAY -------------------------------------------------

# showList :: [a] -> String
def showList(xs):
    ''''Compact stringification of a list,
        (no spaces after commas).
    '''
    return ''.join(repr(xs).split())


# showCycle :: ([a], Int, Int) -> String
def showCycle(cli):
    '''Stringification of cycleFound tuple.'''
    c, lng, iStart = cli
    return showList(c) + ' (from:' + str(iStart) + (
        ', length:' + str(lng) + ')'
    )


# GENERIC -------------------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# bind (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
def bind(m):
    '''bindMay provides the mechanism for composing a
       sequence of (a -> Maybe b) functions.
       If m is Nothing, it is passed straight through.
       If m is Just(x), the result is an application
       of the (a -> Maybe b) function (mf) to x.'''
    return lambda mf: (
        m if m.get('Nothing') else mf(m.get('Just'))
    )


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# findIndex :: (a -> Bool) -> [a] -> Maybe Int
def findIndex(p):
    '''Just the first index at which an
       element in xs matches p,
       or Nothing if no elements match.'''
    def go(xs):
        try:
            return Just(next(
                i for i, x in enumerate(xs) if p(x)
            ))
        except StopIteration:
            return Nothing()
    return lambda xs: go(xs)


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# fTable :: String -> (a -> String) ->
#                     (b -> String) ->
#        (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        ])
    return lambda xShow: lambda fxShow: (
        lambda f: lambda xs: go(
            xShow, fxShow, f, xs
        )
    )


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated
       applications of f to x.'''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


# maybe :: b -> (a -> b) -> Maybe a -> b
def maybe(v):
    '''Either the default value v, if m is Nothing,
       or the application of f to x,
       where m is Just(x).'''
    return lambda f: lambda m: v if m.get('Nothing') else (
        f(m.get('Just'))
    )


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    '''A function over a tuple
       derived from a default or
       curried function.'''
    return lambda xy: f(xy[0], xy[1])


# concat :: [[a]] -> [a]
# concat :: [String] -> String
def concat(xxs):
    '''The concatenation of all the elements in a list.'''
    xs = list(chain.from_iterable(xxs))
    unit = '' if isinstance(xs, str) else []
    return unit if not xs else (
        ''.join(xs) if isinstance(xs[0], str) else xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
First cycle detected, if any:

           cycle([1, 2, 3]) -> [1,2,3] (from:0, length:3)
   [0..100] + cycle([1..8]) -> [1,2,3,4,5,6,7,8] (from:101, length:8)
                   [1..500] -> No cycle found
f(x) = (x*x + 1) modulo 255 -> [101,2,5,26,167,95] (from:2, length:6)
```


But recursion scales poorly in Python, and the version above, while good for lists of a few hundred elements, will need reworking for longer lists and better use of space.

If we start by refactoring the recursion into the form of a higher order (but still recursive) ''until p f x'' function, we can then reimplement the internals of ''until'' itself to avoid recursion, without losing the benefits of compositional structure:

Recursive ''until'':

```python
# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.'''
    def go(f, x):
        return x if p(x) else go(f, f(x))
    return lambda f: lambda x: go(f, x)
```


''cycleLength'' refactored in terms of ''until'':

```python># cycleLength :: Eq a =
 [a] -> Maybe Int
def cycleLength(xs):
    '''Just the length of the first cycle found,
       or Nothing if no cycle seen.'''

    # f :: (Int, Int, Int, [Int]) -> (Int, Int, Int, [Int])
    def f(tpl):
        pwr, lng, x, ys = tpl
        y, *yt = ys
        return (2 * pwr, 1, y, yt) if (
            lng == pwr
        ) else (pwr, 1 + lng, x, yt)

    # p :: (Int, Int, Int, [Int]) -> Bool
    def p(tpl):
        _, _, x, ys = tpl
        return (not ys) or x == ys[0]

    if xs:
        _, lng, x, ys = until(p)(f)(
            (1, 1, xs[0], xs[1:])
        )
        return (
            Just(lng) if (x == ys[0]) else Nothing()
        ) if ys else Nothing()
    else:
        return Nothing()
```


Iterative reimplementation of ''until'':

```python
# until_ :: (a -> Bool) -> (a -> a) -> a -> a
def until_(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.'''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)
```



and now it all works again, with the structure conserved but recursion removed.
The Python no longer falls out of the tree at the sight of an ouroboros, and we can happily search for cycles in lists of several thousand items:
{{Works with|Python|3.7}}

```python
'''Cycle detection without recursion.'''

from itertools import (chain, cycle, islice)
from operator import (eq)


# cycleFound :: Eq a => [a] -> Maybe ([a], Int, Int)
def cycleFound(xs):
    '''Just the first cycle found, with its length
       and start index, or Nothing if no cycle seen.
    '''
    return bind(cycleLength(xs))(
        lambda n: bind(
            findIndex(uncurry(eq))(zip(xs, xs[n:]))
        )(lambda iStart: Just(
            (xs[iStart:iStart + n], n, iStart)
        ))
    )


# cycleLength :: Eq a => [a] -> Maybe Int
def cycleLength(xs):
    '''Just the length of the first cycle found,
       or Nothing if no cycle seen.'''

    # f :: (Int, Int, Int, [Int]) -> (Int, Int, Int, [Int])
    def f(tpl):
        pwr, lng, x, ys = tpl
        y, *yt = ys
        return (2 * pwr, 1, y, yt) if (
            lng == pwr
        ) else (pwr, 1 + lng, x, yt)

    # p :: (Int, Int, Int, [Int]) -> Bool
    def p(tpl):
        _, _, x, ys = tpl
        return (not ys) or x == ys[0]

    if xs:
        _, lng, x, ys = until(p)(f)(
            (1, 1, xs[0], xs[1:])
        )
        return (
            Just(lng) if (x == ys[0]) else Nothing()
        ) if ys else Nothing()
    else:
        return Nothing()


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Reports of any cycle detection.'''

    print(
        fTable(
            'First cycle detected, if any:\n'
        )(fst)(maybe('No cycle found')(
            showCycle
        ))(
            compose(cycleFound)(snd)
        )([
            (
                'cycle([1, 2, 3])',
                take(10000)(cycle([1, 2, 3]))
            ), (
                '[0..10000] + cycle([1..8])',
                take(20000)(
                    chain(
                        enumFromTo(0)(10000),
                        cycle(enumFromTo(1)(8))
                    )
                )
            ), (
                '[1..10000]',
                enumFromTo(1)(10000)
            ), (
                'f(x) = (x*x + 1) modulo 255',
                take(10000)(iterate(
                    lambda x: (1 + (x * x)) % 255
                )(3))
            )
        ])
    )


# DISPLAY -------------------------------------------------

# showList :: [a] -> String
def showList(xs):
    ''''Compact stringification of a list,
        (no spaces after commas).
    '''
    return ''.join(repr(xs).split())


# showCycle :: ([a], Int, Int) -> String
def showCycle(cli):
    '''Stringification of cycleFound tuple.'''
    c, lng, iStart = cli
    return showList(c) + ' (from:' + str(iStart) + (
        ', length:' + str(lng) + ')'
    )

# GENERIC -------------------------------------------------


# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# bind (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
def bind(m):
    '''bindMay provides the mechanism for composing a
       sequence of (a -> Maybe b) functions.
       If m is Nothing, it is passed straight through.
       If m is Just(x), the result is an application
       of the (a -> Maybe b) function (mf) to x.'''
    return lambda mf: (
        m if m.get('Nothing') else mf(m.get('Just'))
    )


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# concat :: [[a]] -> [a]
# concat :: [String] -> String
def concat(xxs):
    '''The concatenation of all the elements in a list.'''
    xs = list(chain.from_iterable(xxs))
    unit = '' if isinstance(xs, str) else []
    return unit if not xs else (
        ''.join(xs) if isinstance(xs[0], str) else xs
    )


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# findIndex :: (a -> Bool) -> [a] -> Maybe Int
def findIndex(p):
    '''Just the first index at which an
       element in xs matches p,
       or Nothing if no elements match.'''
    def go(xs):
        try:
            return Just(next(
                i for i, x in enumerate(xs) if p(x)
            ))
        except StopIteration:
            return Nothing()
    return lambda xs: go(xs)


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# fTable :: String -> (a -> String) ->
#                     (b -> String) ->
#        (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        ])
    return lambda xShow: lambda fxShow: (
        lambda f: lambda xs: go(
            xShow, fxShow, f, xs
        )
    )


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated
       applications of f to x.'''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


# maybe :: b -> (a -> b) -> Maybe a -> b
def maybe(v):
    '''Either the default value v, if m is Nothing,
       or the application of f to x,
       where m is Just(x).'''
    return lambda f: lambda m: v if m.get('Nothing') else (
        f(m.get('Just'))
    )


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    '''A function over a tuple
       derived from a default or
       curried function.'''
    return lambda xy: f(xy[0], xy[1])


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.'''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
First cycle detected, if any:

           cycle([1, 2, 3]) -> [1,2,3] (from:0, length:3)
 [0..10000] + cycle([1..8]) -> [1,2,3,4,5,6,7,8] (from:10001, length:8)
                 [1..10000] -> No cycle found
f(x) = (x*x + 1) modulo 255 -> [101,2,5,26,167,95] (from:2, length:6)
```



## Racket


I feel a bit bad about overloading λ, but it&rsquo;s in the spirit of the algorithm.

```racket

#lang racket/base

;; returns (values lambda mu)
(define (brent f x0)
  ;; main phase: search successive powers of two
  (define λ
    (let main-phase ((power 1)
                     (λ 1)
                     (tortoise x0)
                     (hare (f x0))) ;; f(x0) is the element/node next to x0.
      (cond [(= hare tortoise) λ]
            ;; time to start a new power of two?
            [(= power λ) (main-phase (* power 2) 1 hare (f hare))]
            [else (main-phase power (add1 λ) tortoise (f hare))])))

  (values
   λ
   ;; Find the position of the first repetition of length λ
   (let race ((µ 0)
              (tortoise x0)
              ;; The distance between the hare and tortoise is now λ.
              (hare (for/fold ((hare x0)) ((_ (in-range λ))) (f hare))))
     ;; Next, the hare and tortoise move at same speed until they agree
     (if (= tortoise hare) µ (race (add1 µ) (f tortoise) (f hare))))))

(module+ test
  (require rackunit racket/generator)
  (define (f x) (modulo (+ (* x x) 1) 255))
  (define (make-generator f x0)
    (generator () (let loop ((x x0)) (yield x) (loop (f x)))))

  (define g (make-generator f 3))

  (define l (for/list ((_ 20)) (g)))
  (check-equal? l '(3 10 101 2 5 26 167 95 101 2 5 26 167 95 101 2 5 26 167 95))
  (displayln l)
  (let-values (([µ λ] (brent f 3)))
    (printf "Cycle length = ~a~%Start Index = ~a~%" µ λ)))

```


{{out}}

```txt

(3 10 101 2 5 26 167 95 101 2 5 26 167 95 101 2 5 26 167 95)
Cycle length = 6
Start Index = 2

```



## REXX

===Brent's algorithm===

```rexx
/*REXX program detects a cycle in an iterated function  [F]  using Brent's algorithm.   */
init= 3;   @= init
                                  do  until length(@) > 79;    @= @ f(word(@, words(@) ) )
                                  end   /*until*/
say '  original list='    @  ...                          /*display original number list*/
call Brent init                                           /*invoke Brent algorithm for F*/
parse var result cycle idx                                /*get 2 values returned from F*/
say 'numbers in list='    words(@)                        /*display number of numbers.  */
say '  cycle length ='    cycle                           /*display the cycle    to term*/
say '  start index  ='     idx     "  ◄─── zero index"    /*   "     "  index     "   " */
say 'cycle sequence ='   subword(@, idx+1, cycle)         /*   "     "  sequence  "   " */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Brent: procedure; parse arg x0 1 tort;   pow=1;     #=1   /*TORT  is set to value of X0.*/
       hare= f(x0)                                        /*get 1st value for the func. */
                   do  while tort\==hare
                   if pow==#  then do;  tort= hare        /*set value of TORT to HARE.  */
                                        pow = pow + pow   /*double the value of  POW.   */
                                        #   = 0           /*reset  #  to zero  (lambda).*/
                                   end
                   hare= f(hare)
                   #= #+1                                 /*bump the lambda count value.*/
                   end   /*while*/
       hare= x0
                   do #;          hare= f(hare)           /*generate number of F values.*/
                   end   /*j*/
       tort= x0                                           /*find position of the 1st rep*/
                   do mu=0  while tort \== hare           /*MU  is a  zero─based  index.*/
                                  tort= f(tort)
                                  hare= f(hare)
                   end   /*mu*/
       return # mu
/*──────────────────────────────────────────────────────────────────────────────────────*/
f:     return ( arg(1) **2  +  1)   //   255     /*this defines/executes the function F.*/
```

'''output'''   using the defaults:

```txt

  original list= 3 10 101 2 5 26 167 95 101 2 5 26 167 95 101 2 5 26 167 95 101 2 5 26 167 95 101 ...
numbers in list= 27
  cycle length = 6
  start index  = 2   ◄─── zero index
cycle sequence = 101 2 5 26 167 95

```



### sequential search algorithm


```rexx
/*REXX program detects a cycle in an iterated function  [F]  using a sequential search. */
x=3;  @=x                                                  /*initial couple of variables*/
                        do  until cycle\==0;   x= f(x)     /*calculate another number.  */
                        cycle= wordpos(x, @)               /*This could be a repeat.    */
                        @= @ x                             /*append number to   @  list.*/
                        end   /*until*/
#= words(@) - cycle                                        /*compute the cycle length.  */
say '  original list='  @ ...                              /*display the sequence.      */
say 'numbers in list='  words(@)                           /*display number of numbers. */
say '  cycle length ='  #                                  /*display the cycle   to term*/
say '  start index  ='  cycle - 1     "  ◄─── zero based"  /*   "     "  index    "   " */
say 'cycle sequence ='  subword(@, cycle, #)               /*   "     "  sequence "   " */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
f:   return ( arg(1) **2  +  1)   //   255       /*this defines/executes the function F.*/
```

{{out|output|:}}

```txt

  original list= 3 10 101 2 5 26 167 95 101 ...
numbers in list= 9
  cycle length = 6
  start index  = 2   ◄─── zero based
cycle sequence = 101 2 5 26 167 95

```



### hash table algorithm

This REXX version is a lot faster   (than the sequential search algorithm)   if the   ''cycle length''   and/or   ''start index''   is large.

```rexx
/*REXX program detects a cycle in an iterated function  [F]  using a hash table.        */
x=3;  @=x;  !.=.;  !.x=1
                         do n=1+words(@);  x= f(x);   @= @ x  /*add the number to list. */
                         if !.x\==.  then leave               /*A repeat?   Then leave. */
                         !.x= n                               /*N:  numbers in  @  list.*/
                         end   /*n*/
say '  original list=' @ ...                                  /*maybe display the list. */
say 'numbers in list=' n                                      /*display number of nums. */
say '  cycle length =' n - !.x                                /*   "      "    cycle.   */
say '  start index  =' !.x - 1      '  ◄───  zero based'      /*   "      "    index.   */
say 'cycle sequence =' subword(@, !.x, n - !.x)               /*   "      "   sequence. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
f:   return ( arg(1) **2  +  1)   //   255       /*this defines/executes the function F.*/
```

{{out|output|text=  is identical to the 2<sup>nd</sup> REXX version.}}




### robust hash table algorithm

This REXX version implements a   ''robust''   hash table algorithm, which means that when the divisor

(in the   '''F'''   function)   is fairly large, the cycle length can be very large, making the creation of the

iterated function sequence.   This becomes problematic when the mechanism to append a number to

the sequence becomes time consuming because the sequence contains many hundreds of thousands

of numbers.

This REXX version allows the divisor for the   '''F'''   function to be specified, which can be chosen to stress

test the hash table algorithm.   A divisor which is   <big> ''two raised to the 49<sup>th</sup>  power'' </big>   was chosen;   it

generates a cyclic sequence that contains over 1.5 million numbers.

```rexx
/*REXX program detects a  cycle  in an  iterated function  [F]  using a robust hashing. */
parse arg power .                                      /*obtain optional args from C.L. */
if power=='' | power=","  then power=8                 /*Not specified?  Use the default*/
numeric digits 500                                     /*be able to handle big numbers. */
divisor= 2**power - 1                                  /*compute the divisor, power of 2*/
numeric digits max(9, length(divisor) * 2 + 1)         /*allow for the  square  plus one*/
say '       power ='  power                            /*display the power   to the term*/
say '     divisor ='  "{2**"power'}-1 = '    divisor   /*   "     "  divisor. "  "    " */
say
x=3;    @=x;   @@=;      m=100;    !.=.;     !.x=1     /*M:  maximum numbers to display.*/

               do n=1+words(@);    x= f(x);  @@=@@ x
               if n//2000==0  then do;   @=@ @@;   @@=;  end   /*Is a 2000th N?  Rejoin.*/
               if !.x\==.     then leave               /*is this number a repeat?  Leave*/
               !.x= n
               end   /*n*/                             /*N:   is the size of   @   list.*/
@= space(@ @@)                                         /*append residual numbers to  @  */
if n<m  then say '  original list=' @ ...              /*maybe display the list to term.*/
             say 'numbers in list=' n                  /*display number of numbers.     */
             say '  cycle length =' n - !.x            /*display the cycle to the term. */
             say '  start index  =' !.x - 1   "  ◄─── zero based"      /*show the index.*/
if n<m  then say 'cycle sequence =' subword(@, !.x, n- !.x) /*maybe display the sequence*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
f:   return ( arg(1) **2  +  1)   //   255       /*this defines/executes the function F.*/
```

{{out|output|text=   when the input (power of two) used is:     <tt> 49 </tt>}}

```txt

       power = 49
     divisor = {2**49}-1 =  562949953421311

  original list= 3 10 101 2 5 26 167 95 101 ...
numbers in list= 9
  cycle length = 6
  start index  = 2   ◄─── zero based
cycle sequence = 101 2 5 26 167 95

```



### variant of the hash table algorithm

There is more information in the "hash table"

and f has no "side effect".

```rexx
/*REXX pgm detects a cycle in an iterated function [F]               */
x=3; list=x; p.=0; p.x=1
Do q=2 By 1
  x=f(x)                           /* the next value                 */
  list=list x                      /* append it to the list          */
  If p.x>0 Then Do                 /* x was in the list              */
    cLen=q-p.x                     /* cycle length                   */
    Leave
    End
  p.x=q                            /* note position of x in the list */
  End
Say 'original list='  list ...
Say 'cycle length ='  cLen                   /*display the cycle len */
Say 'start index  ='  p.x-1 "  (zero based)" /*   "     "  index.    */
Say 'the sequence ='  subword(list,p.x,cLen) /*   "     "  sequence. */
Exit
/*-------------------------------------------------------------------*/
f: Return (arg(1)**2+1)// 255;                /*define the function F*/
```



## Ruby

{{works with|ruby|2.0}}


```Ruby
# Author: Paul Anton Chernoch
# Purpose:
#   Find the cycle length and start position of a numerical seried using Brent's cycle algorithm.
#
# Given a recurrence relation X[n+1] = f(X[n]) where f() has
# a finite range, you will eventually repeat a value that you have seen before.
# Once this happens, all subsequent values will form a cycle that begins
# with the first repeated value. The period of that cycle may be of any length.
#
# Parameters:
#   x0 ...... First integer value in the sequence
#   block ... Block that takes a single integer as input
#             and returns a single integer as output.
#             This yields a sequence of numbers that eventually repeats.
# Returns:
#   Two values: lambda and mu
#   lambda .. length of cycle
#   mu ...... zero-based index of start of cycle
#
def findCycle(x0)
  power = lambda = 1
  tortoise = x0
  hare = yield(x0)

  # Find lambda, the cycle length
  while tortoise != hare
    if power == lambda
      tortoise = hare
      power *= 2
      lambda = 0
    end
    hare = yield(hare)
    lambda += 1
  end

  # Find mu, the zero-based index of the start of the cycle
  hare = x0
  lambda.times { hare = yield(hare) }

  tortoise, mu = x0, 0
  while tortoise != hare
    tortoise = yield(tortoise)
    hare = yield(hare)
    mu += 1
  end

  return lambda, mu
end

# A recurrence relation to use in testing
def f(x) (x * x + 1) % 255 end

# Display the first 41 numbers in the test series
puts (1..40).reduce([3]){|acc,_| acc << f(acc.last)}.join(",")

# Test the findCycle function
clength, cstart = findCycle(3) { |x| f(x) }
puts "Cycle length = #{clength}\nStart index = #{cstart}"
```


{{out}}

```txt

3,10,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5
Cycle length = 6
Start index = 2

```



## Scala

{{Out}}Best seen in running your browser either by [https://scalafiddle.io/sf/6O7WjnO/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/kPCg0fxOQQCZPkOnmMR0Kg Scastie (remote JVM)].

```Scala
object CycleDetection extends App {

  def brent(f: Int => Int, x0: Int): (Int, Int) = {
    // main phase: search successive powers of two
    // f(x0) is the element/node next to x0.
    var (power, λ, μ, tortoise, hare) = (1, 1, 0, x0, f(x0))

    while (tortoise != hare) {
      if (power == λ) { // time to start a new power of two?
        tortoise = hare
        power *= 2
        λ = 0
      }
      hare = f(hare)
      λ += 1
    }

    // Find the position of the first repetition of length 'λ'
    tortoise = x0
    hare = x0
    for (i <- 0 until λ) hare = f(hare)

    // The distance between the hare and tortoise is now 'λ'.
    // Next, the hare and tortoise move at same speed until they agree
    while (tortoise != hare) {
      tortoise = f(tortoise)
      hare = f(hare)
      μ += 1
    }
    (λ, μ)
  }

  def cycle = loop.slice(μ, μ + λ)

  def f = (x: Int) => (x * x + 1) % 255

  // Generator for the first terms of the sequence starting from 3
  def loop: LazyList[Int] = 3 #:: loop.map(f(_))

  val (λ, μ) = brent(f, 3)
  println(s"Cycle length = $λ")
  println(s"Start index  = $μ")
  println(s"Cycle        = ${cycle.force}")

}
```


## Sidef

{{trans|Perl 6}}

```ruby
func brent (f, x0) {
    var power = 1
    var λ = 1
    var tortoise = x0
    var hare = f(x0)

    while (tortoise != hare) {
        if (power == λ) {
            tortoise = hare
            power *= 2
            λ = 0
        }
        hare = f(hare)
        λ += 1
    }

    var μ = 0
    tortoise = x0
    hare = x0
    { hare = f(hare) } * λ

    while (tortoise != hare) {
        tortoise = f(tortoise)
        hare = f(hare)
        μ += 1
    }

    return (λ, μ)
}

func cyclical_function(x) { (x*x + 1) % 255 }

var (l, s) = brent(cyclical_function, 3)

var seq = gather {
    var x = 3
    { take(x); x = cyclical_function(x) } * 20
}

say seq.join(', ')+', ...'

say "Cycle length #{l}.";
say "Cycle start index #{s}."
say [seq[s .. (s + l - 1)]]
```

{{out}}

```txt
3, 10, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, ...
Cycle length 6.
Cycle start index 2.
[101, 2, 5, 26, 167, 95]
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Function FindCycle(Of T As IEquatable(Of T))(x0 As T, yielder As Func(Of T, T)) As Tuple(Of Integer, Integer)
        Dim power = 1
        Dim lambda = 1
        Dim tortoise As T
        Dim hare As T

        tortoise = x0
        hare = yielder(x0)

        ' Find lambda, the cycle length
        While Not tortoise.Equals(hare)
            If power = lambda Then
                tortoise = hare
                power *= 2
                lambda = 0
            End If
            hare = yielder(hare)
            lambda += 1
        End While

        ' Find mu, the zero-based index of the start of the cycle
        Dim mu = 0
        tortoise = x0
        hare = x0
        For times = 1 To lambda
            hare = yielder(hare)
        Next

        While Not tortoise.Equals(hare)
            tortoise = yielder(tortoise)
            hare = yielder(hare)
            mu += 1
        End While

        Return Tuple.Create(lambda, mu)
    End Function

    Sub Main()
        ' A recurrence relation to use in testing
        Dim sequence = Function(_x As Integer) (_x * _x + 1) Mod 255

        ' Display the first 41 numbers in the test series
        Dim x = 3
        Console.Write(x)
        For times = 0 To 39
            x = sequence(x)
            Console.Write(",{0}", x)
        Next
        Console.WriteLine()

        ' Test the FindCycle method
        Dim cycle = FindCycle(3, sequence)
        Console.WriteLine("Cycle length = {0}", cycle.Item1)
        Console.WriteLine("Start index = {0}", cycle.Item2)
    End Sub

End Module
```

{{out}}

```txt
3,10,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5,26,167,95,101,2,5
Cycle length = 6
Start index = 2
```



## zkl

Algorithm from the Wikipedia

```zkl
fcn cycleDetection(f,x0){ // f(int), x0 is the integer starting value of the sequence
  # main phase: search successive powers of two
  power:=lam:=1;
  tortoise,hare:=x0,f(x0);  # f(x0) is the element/node next to x0.
  while(tortoise!=hare){
     if(power==lam){  # time to start a new power of two?
	tortoise,lam=hare,0;
	power*=2;
     }
     hare=f(hare);
     lam+=1;
  }
  # Find the position of the first repetition of length λ
  mu:=0; tortoise=hare=x0;
  do(lam){ hare=f(hare) } # The distance between the hare and tortoise is now λ

  # Next, the hare and tortoise move at same speed till they agree
  while(tortoise!=hare){
     tortoise,hare=f(tortoise),f(hare);
     mu+=1;
  }
  return(lam,mu);
}
```


```zkl
cycleDetection(fcn(x){ (x*x + 1)%255 }, 3).println(" == cycle length, start index");
```

{{out}}

```txt

L(6,2) == cycle length, start index

```

