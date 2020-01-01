+++
title = "Dice game probabilities"
description = ""
date = 2019-07-08T07:30:34Z
aliases = []
[extra]
id = 18509
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
Two players have a set of dice each. The first player has nine dice with four faces each, with numbers one to four. The second player has six normal dice with six faces each, each face has the usual numbers from one to six.

They roll their dice and sum the totals of the faces. The player with the highest total wins (it's a draw if the totals are the same). What's the probability of the first player beating the second player?

Later the two players use a different set of dice each. Now the first player has five dice with ten faces each, and the second player has six dice with seven faces each. Now what's the probability of the first player beating the second player?

This task was adapted from the Project Euler Problem n.205:
https://projecteuler.net/problem=205


## 11l

{{trans|C}}

```11l
F throw_die(n_sides, n_dice, s, [Int] &counts)
   I n_dice == 0
      counts[s]++
      R
   L(i) 1..n_sides
      throw_die(n_sides, n_dice - 1, s + i, counts)

F beating_probability(n_sides1, n_dice1,
                      n_sides2, n_dice2)
   V len1 = (n_sides1 + 1) * n_dice1
   V C1 = [0] * len1
   throw_die(n_sides1, n_dice1, 0, &C1)

   V len2 = (n_sides2 + 1) * n_dice2
   V C2 = [0] * len2
   throw_die(n_sides2, n_dice2, 0, &C2)

   Float p12 = (n_sides1 ^ n_dice1) * (n_sides2 ^ n_dice2)

   V tot = 0.0
   L(i) 0 .< len1
      L(j) 0 .< min(i, len2)
         tot += Float(C1[i]) * C2[j] / p12
   R tot

print(‘#.16’.format(beating_probability(4, 9, 6, 6)))
print(‘#.16’.format(beating_probability(10, 5, 7, 6)))
```


{{out}}

```txt

0.5731440767829814
0.6427886287176272

```



## C


```c
#include <stdio.h>
#include <stdint.h>

typedef uint32_t uint;
typedef uint64_t ulong;

ulong ipow(const uint x, const uint y) {
    ulong result = 1;
    for (uint i = 1; i <= y; i++)
        result *= x;
    return result;
}

uint min(const uint x, const uint y) {
    return (x < y) ? x : y;
}

void throw_die(const uint n_sides, const uint n_dice, const uint s, uint counts[]) {
    if (n_dice == 0) {
        counts[s]++;
        return;
    }

    for (uint i = 1; i < n_sides + 1; i++)
        throw_die(n_sides, n_dice - 1, s + i, counts);
}

double beating_probability(const uint n_sides1, const uint n_dice1,
                           const uint n_sides2, const uint n_dice2) {
    const uint len1 = (n_sides1 + 1) * n_dice1;
    uint C1[len1];
    for (uint i = 0; i < len1; i++)
        C1[i] = 0;
    throw_die(n_sides1, n_dice1, 0, C1);

    const uint len2 = (n_sides2 + 1) * n_dice2;
    uint C2[len2];
    for (uint j = 0; j < len2; j++)
        C2[j] = 0;
    throw_die(n_sides2, n_dice2, 0, C2);

    const double p12 = (double)(ipow(n_sides1, n_dice1) * ipow(n_sides2, n_dice2));

    double tot = 0;
    for (uint i = 0; i < len1; i++)
        for (uint j = 0; j < min(i, len2); j++)
            tot += (double)C1[i] * C2[j] / p12;
    return tot;
}

int main() {
    printf("%1.16f\n", beating_probability(4, 9, 6, 6));
    printf("%1.16f\n", beating_probability(10, 5, 7, 6));
    return 0;
}
```

{{out}}

```txt
0.5731440767829801
0.6427886287176260
```



## D


### version 1


```d
import std.stdio, std.range, std.algorithm;

void throwDie(in uint nSides, in uint nDice, in uint s, uint[] counts)
pure nothrow @safe @nogc {
    if (nDice == 0) {
        counts[s]++;
        return;
    }

    foreach (immutable i; 1 .. nSides + 1)
        throwDie(nSides, nDice - 1, s + i, counts);
}

real beatingProbability(uint nSides1, uint nDice1,
                        uint nSides2, uint nDice2)()
pure nothrow @safe /*@nogc*/ {
    uint[(nSides1 + 1) * nDice1] C1;
    throwDie(nSides1, nDice1, 0, C1);

    uint[(nSides2 + 1) * nDice2] C2;
    throwDie(nSides2, nDice2, 0, C2);

    immutable p12 = real((ulong(nSides1) ^^ nDice1) *
                         (ulong(nSides2) ^^ nDice2));

    return cartesianProduct(C1[].enumerate, C2[].enumerate)
           .filter!(p => p[0][0] > p[1][0])
           .map!(p => real(p[0][1]) * p[1][1] / p12)
           .sum;
}

void main() @safe {
    writefln("%1.16f", beatingProbability!(4, 9, 6, 6));
    writefln("%1.16f", beatingProbability!(10, 5, 7, 6));
}
```

{{out}}

```txt
0.5731440767829801
0.6427886287176262
```


===version 2 (Faster Alternative Version)===
{{trans|Python}}

```d
import std.stdio, std.range, std.algorithm;

ulong[] combos(R)(R sides, in uint n) pure nothrow @safe
if (isForwardRange!R) {
    if (sides.empty)
        return null;
    if (!n)
        return [1];
    auto ret = new typeof(return)(reduce!max(sides[0], sides[1 .. $]) * n + 1);
    foreach (immutable i, immutable v; enumerate(combos(sides, n - 1))) {
        if (!v)
            continue;
        foreach (immutable s; sides)
            ret[i + s] += v;
    }
    return ret;
}

real winning(R)(R sides1, in uint n1, R sides2, in uint n2)
pure nothrow @safe if (isForwardRange!R) {
    static void accumulate(T)(T[] arr) pure nothrow @safe @nogc {
        foreach (immutable i; 1 .. arr.length)
            arr[i] += arr[i - 1];
    }

    immutable p1 = combos(sides1, n1);
    auto p2 = combos(sides2, n2);
    immutable s = p1.sum * p2.sum;
    accumulate(p2);
    ulong win = 0; // 'win' is 1 beating 2.
    foreach (immutable i, immutable x1; p1.dropOne.enumerate)
        win += x1 * p2[min(i, $ - 1)];
    return win / real(s);
}

void main() @safe {
    writefln("%1.16f", winning(iota(1u, 5u),  9, iota(1u, 7u), 6));
    writefln("%1.16f", winning(iota(1u, 11u), 5, iota(1u, 8u), 6));
}
```

{{out}}

```txt
0.5731440767829801
0.6427886287176262
```



## Factor


```factor
USING: dice generalizations kernel math prettyprint sequences ;
IN: rosetta-code.dice-probabilities

: winning-prob ( a b c d -- p )
    [ [ random-roll ] 2bi@ > ] 4 ncurry [ 100000 ] dip replicate
    [ [ t = ] count ] [ length ] bi /f ;

9 4 6 6 winning-prob
5 10 6 7 winning-prob [ . ] bi@
```

{{out}}

```txt

0.57199
0.64174

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=ef255e4239a713aba35598a4617af3c9 Click this link to run this code]'''

```gambas
' Gambas module file

Public Sub Main()
Dim iSides, iPlayer1, iPlayer2, iTotal1, iTotal2, iCount, iCount0 As Integer
Dim iDice1 As Integer = 9
Dim iDice2 As Integer = 6
Dim iSides1 As Integer = 4
Dim iSides2 As Integer = 6

Randomize

For iCount0 = 0 To 1
  For iCount = 1 To 100000
    iPlayer1 = Roll(iDice1, iSides1)
    iPlayer2 = Roll(iDice2, iSides2)
    If iPlayer1 > iPlayer2 Then
      iTotal1 += 1
    Else If iPlayer1 <> iPlayer2 Then
      iTotal2 += 1
    Endif
  Next

  Print "Tested " & Str(iCount - 1) & " times"
  Print "Player1 with " & Str(iDice1) & " dice of " & Str(iSides1) & " sides"
  Print "Player2 with " & Str(iDice2) & " dice of " & Str(iSides2) & " sides"
  Print "Total wins Player1 = " & Str(iTotal1) & " - " & Str(iTotal2 / iTotal1)
  Print "Total wins Player2 = " & Str(iTotal2)
  Print Str((iCount - 1) - (iTotal1 + iTotal2)) & " draws" & gb.NewLine

  iDice1 = 5
  iDice2 = 6
  iSides1 = 10
  iSides2 = 7
  iTotal1 = 0
  iTotal2 = 0
Next

End

Public Sub Roll(iDice As Integer, iSides As Integer) As Integer
Dim iCount, iTotal As Short

For iCount = 1 To iDice
  iTotal += Rand(1, iSides)
Next

Return iTotal

End
```

Output:

```txt

Tested 100000 times
Player1 with 9 dice of 4 sides
Player2 with 6 dice of 6 sides
Total wins Player1 = 56980 - 0.62823797823798
Total wins Player2 = 35797
7223 draws

Tested 100000 times
Player1 with 5 dice of 10 sides
Player2 with 6 dice of 7 sides
Total wins Player1 = 64276 - 0.48548447320928
Total wins Player2 = 31205
4519 draws

```


## Go

{{trans|C}}

```go
package main

import(
    "math"
    "fmt"
)

func minOf(x, y uint) uint {
    if x < y {
        return x
    }
    return y
}

func throwDie(nSides, nDice, s uint, counts []uint) {
    if nDice == 0 {
        counts[s]++
        return
    }
    for i := uint(1); i <= nSides; i++ {
        throwDie(nSides, nDice - 1, s + i, counts)
    }
}

func beatingProbability(nSides1, nDice1, nSides2, nDice2 uint) float64 {
    len1 := (nSides1 + 1) * nDice1
    c1 := make([]uint, len1)  // all elements zero by default
    throwDie(nSides1, nDice1, 0, c1)

    len2 := (nSides2 + 1) * nDice2
    c2 := make([]uint, len2)
    throwDie(nSides2, nDice2, 0, c2)
    p12 := math.Pow(float64(nSides1), float64(nDice1)) *
           math.Pow(float64(nSides2), float64(nDice2))

    tot := 0.0
    for i := uint(0); i < len1; i++ {
        for j := uint(0); j < minOf(i, len2); j++ {
            tot += float64(c1[i] * c2[j]) / p12
        }
    }
    return tot
}

func main() {
    fmt.Println(beatingProbability(4, 9, 6, 6))
    fmt.Println(beatingProbability(10, 5, 7, 6))
}
```


{{out}}

```txt

0.5731440767829815
0.6427886287176273

```


More idiomatic go:


```go
package main

import (
	"fmt"
	"math/rand"
)

type set struct {
	n, s int
}

func (s set) roll() (sum int) {
	for i := 0; i < s.n; i++ {
		sum += rand.Intn(s.s) + 1
	}
	return
}

func (s set) beats(o set, n int) (p float64) {
	for i := 0; i < n; i++ {
		if s.roll() > o.roll() {
			p = p + 1.0
		}
	}
	p = p / float64(n)
	return
}

func main() {
	fmt.Println(set{9, 4}.beats(set{6, 6}, 1000))
	fmt.Println(set{5, 10}.beats(set{6, 7}, 1000))
}
```


{{out}}

```txt

0.576
0.639

```



## J

'''Solution:'''

```J
gen_dict =: (({. , #)/.~@:,@:(+/&>)@:{@:(# <@:>:@:i.)~ ; ^)&x:

beating_probability =: dyad define
 'C0 P0' =. gen_dict/ x
 'C1 P1' =. gen_dict/ y
 (C0 +/@:,@:(>/&:({."1) * */&:({:"1)) C1) % (P0 * P1)
)
```

'''Example Usage:'''

```J
   10 5 (;x:inv)@:beating_probability 7 6
┌─────────────────────┬────────┐
│3781171969r5882450000│0.642789│
└─────────────────────┴────────┘
   4 9 (;x:inv)@:beating_probability 6 6
┌─────────────────┬────────┐
│48679795r84934656│0.573144│
└─────────────────┴────────┘
```

gen_dict explanation:

<code>gen_dict</code> is akin to <code>gen_dict</code> in the python solution and
returns a table and total number of combinations, order matters.
The table has 2 columns.  The first column is the pip count on all dice,
the second column is the number of ways this many pips can occur.

<code>({. , #)/.~</code> make a vector having items head and tally of each group of like items in this case pip count and occurrences operating on

<code>,</code> raveled data (a vector) made of

<code>+/&></code> the sum of each of the

<code>{</code> Cartesian products of the

<code>(# <@:>:@:i.)</code> equi-probable die values

<code>&x:</code> but first use extended integers

<code>; ^</code> links the total possibilities to the result.


The verb <code>beating_probability</code> is akin to the python solution function having same name.

<code>C0 >/&:({."1) C1</code>  is a binary table where the pips of first player exceed pips of second player.  "Make a greater than table but first take the head of each item."

<code>C0 */&:({:"1) C1</code>  is the corresponding table of occurrences

<code>*</code> naturally we multiply the two tables (atom by atom, not a matrix product)

<code>+/@:,@:</code> sum the raveled table

<code>% (P0 * P1)</code> after which divide by the all possible rolls.


## Java


```java
import java.util.Random;

public class Dice{
	private static int roll(int nDice, int nSides){
		int sum = 0;
		Random rand = new Random();
		for(int i = 0; i < nDice; i++){
			sum += rand.nextInt(nSides) + 1;
		}
		return sum;
	}

	private static int diceGame(int p1Dice, int p1Sides, int p2Dice, int p2Sides, int rolls){
		int p1Wins = 0;
		for(int i = 0; i < rolls; i++){
			int p1Roll = roll(p1Dice, p1Sides);
			int p2Roll = roll(p2Dice, p2Sides);
			if(p1Roll > p2Roll) p1Wins++;
		}
		return p1Wins;
	}

	public static void main(String[] args){
		int p1Dice = 9; int p1Sides = 4;
		int p2Dice = 6; int p2Sides = 6;
		int rolls = 10000;
		int p1Wins = diceGame(p1Dice, p1Sides, p2Dice, p2Sides, rolls);
		System.out.println(rolls + " rolls, p1 = " + p1Dice + "d" + p1Sides + ", p2 = " + p2Dice + "d" + p2Sides);
		System.out.println("p1 wins " + (100.0 * p1Wins / rolls) + "% of the time");

		System.out.println();

		p1Dice = 5; p1Sides = 10;
		p2Dice = 6; p2Sides = 7;
		rolls = 10000;
		p1Wins = diceGame(p1Dice, p1Sides, p2Dice, p2Sides, rolls);
		System.out.println(rolls + " rolls, p1 = " + p1Dice + "d" + p1Sides + ", p2 = " + p2Dice + "d" + p2Sides);
		System.out.println("p1 wins " + (100.0 * p1Wins / rolls) + "% of the time");

		System.out.println();

		p1Dice = 9; p1Sides = 4;
		p2Dice = 6; p2Sides = 6;
		rolls = 1000000;
		p1Wins = diceGame(p1Dice, p1Sides, p2Dice, p2Sides, rolls);
		System.out.println(rolls + " rolls, p1 = " + p1Dice + "d" + p1Sides + ", p2 = " + p2Dice + "d" + p2Sides);
		System.out.println("p1 wins " + (100.0 * p1Wins / rolls) + "% of the time");

		System.out.println();

		p1Dice = 5; p1Sides = 10;
		p2Dice = 6; p2Sides = 7;
		rolls = 1000000;
		p1Wins = diceGame(p1Dice, p1Sides, p2Dice, p2Sides, rolls);
		System.out.println(rolls + " rolls, p1 = " + p1Dice + "d" + p1Sides + ", p2 = " + p2Dice + "d" + p2Sides);
		System.out.println("p1 wins " + (100.0 * p1Wins / rolls) + "% of the time");
	}
}
```

{{out}}

```txt
10000 rolls, p1 = 9d4, p2 = 6d6
p1 wins 57.56% of the time

10000 rolls, p1 = 5d10, p2 = 6d7
p1 wins 64.28% of the time

1000000 rolls, p1 = 9d4, p2 = 6d6
p1 wins 57.3563% of the time

1000000 rolls, p1 = 5d10, p2 = 6d7
p1 wins 64.279% of the time
```



## Julia

{{works with|Julia|0.6}}


```julia
play(ndices::Integer, nfaces::Integer) = (nfaces, ndices) ∋ 0 ? 0 : sum(rand(1:nfaces) for i in 1:ndices)

simulate(d1::Integer, f1::Integer, d2::Integer, f2::Integer; nrep::Integer=1_000_000) =
    mean(play(d1, f1) > play(d2, f2) for _ in 1:nrep)

println("\nPlayer 1: 9 dices, 4 faces\nPlayer 2: 6 dices, 6 faces\nP(Player1 wins) = ", simulate(9, 4, 6, 6))
println("\nPlayer 1: 5 dices, 10 faces\nPlayer 2: 6 dices, 7 faces\nP(Player1 wins) = ", simulate(5, 10, 6, 7))
```


{{out}}

```txt
Player 1: 9 dices, 4 faces
Player 2: 6 dices, 6 faces
P(Player1 wins) = 0.572805

Player 1: 5 dices, 10 faces
Player 2: 6 dices, 7 faces
P(Player1 wins) = 0.642727
```



## Kotlin

{{trans|C}}

```scala
// version 1.1.2

fun throwDie(nSides: Int, nDice: Int, s: Int, counts: IntArray) {
    if (nDice == 0) {
        counts[s]++
        return
    }
    for (i in 1..nSides) throwDie(nSides, nDice - 1, s + i, counts)
}

fun beatingProbability(nSides1: Int, nDice1: Int, nSides2: Int, nDice2: Int): Double {
    val len1 = (nSides1 + 1) * nDice1
    val c1 = IntArray(len1)  // all elements zero by default
    throwDie(nSides1, nDice1, 0, c1)

    val len2 = (nSides2 + 1) * nDice2
    val c2 = IntArray(len2)
    throwDie(nSides2, nDice2, 0, c2)

    val p12 = Math.pow(nSides1.toDouble(), nDice1.toDouble()) *
              Math.pow(nSides2.toDouble(), nDice2.toDouble())

    var tot = 0.0
    for (i in 0 until len1) {
        for (j in 0 until minOf(i, len2)) {
            tot += c1[i] * c2[j] / p12
        }
    }
    return tot
}

fun main(args: Array<String>) {
    println(beatingProbability(4, 9, 6, 6))
    println(beatingProbability(10, 5, 7, 6))
}
```


{{out}}

```txt

0.5731440767829815
0.6427886287176273

```



## ooRexx


### Algorithm


```oorexx
Numeric Digits 30
Call test '9 4 6 6'
Call test '5 10 6 7'
Exit
test:
Parse Arg w1 s1 w2 s2
p1.=0
p2.=0
Call pp 1,w1,s1,p1.,p2.
Call pp 2,w2,s2,p1.,p2.
p2low.=0
Do x=w1 To w1*s1
  Do y=0 To x-1
    p2low.x+=p2.y
    End
  End
pwin1=0
Do x=w1 To w1*s1
  pwin1+=p1.x*p2low.x
  End
Say 'Player 1 has' w1 'dice with' s1 'sides each'
Say 'Player 2 has' w2 'dice with' s2 'sides each'
Say 'Probability for player 1 to win:' pwin1
Say ''
Return

pp: Procedure
/*---------------------------------------------------------------------
* Compute and assign probabilities to get a sum x
* when throwing w dice each having s sides (marked from 1 to s)
* k=1 sets p1.*, k=2 sets p2.*
*--------------------------------------------------------------------*/
Use Arg k,w,s,p1.,p2.
str=''
cnt.=0
Do wi=1 To w
  str=str||'Do v'wi'=1 To' s';'
  End
str=str||'sum='
Do wi=1 To w-1
  str=str||'v'wi'+'
  End
str=str||'v'w';'
str=str||'cnt.'sum'+=1;'
Do wi=1 To w
  str=str||'End;'
  End
Interpret str
psum=0
Do x=0 To w*s
  If k=1 Then
    p1.x=cnt.x/(s**w)
  Else
    p2.x=cnt.x/(s**w)
  psum+=p1.x
  End
Return
```

{{out}}

```txt
Player 1 has 9 dice with 4 sides each
Player 2 has 6 dice with 6 sides each
Probability for player 1 to win: 0.573144076782980082947530864198

Player 1 has 5 dice with 10 sides each
Player 2 has 6 dice with 7 sides each
Probability for player 1 to win: 0.642788628717626159168373721835
```



### Algorithm using rational arithmetic


```oorexx
Numeric Digits 30
Call test '9 4 6 6'
Call test '5 10 6 7'
Exit

test:
Parse Arg w1 s1 w2 s2
p1.='0/1'
p2.='0/1'
Call pp 1,w1,s1,p1.,p2.
Call pp 2,w2,s2,p1.,p2.
p2low.='0/1'
Do x=w1 To w1*s1
  Do y=0 To x-1
    p2low.x=fr_add(p2low.x,p2.y)
    End
  End
pwin1='0/1'
Do x=w1 To w1*s1
  pwin1=fr_add(pwin1,fr_Mult(p1.x,p2low.x))
  End
Say 'Player 1 has' w1 'dice with' s1 'sides each'
Say 'Player 2 has' w2 'dice with' s2 'sides each'
Say 'Probability for player 1 to win:' pwin1
Parse Var pwin1 nom '/' denom
Say '                              ->' (nom/denom)
Say ''
Return

pp: Procedure
/*---------------------------------------------------------------------
* Compute and assign probabilities to get a sum x
* when throwing w dice each having s sides (marked from 1 to s)
* k=1 sets p1.*, k=2 sets p2.*
*--------------------------------------------------------------------*/
Use Arg k,w,s,p1.,p2.
str=''
cnt.=0
Do wi=1 To w
  str=str||'Do v'wi'=1 To' s';'
  End
str=str||'sum='
Do wi=1 To w-1
  str=str||'v'wi'+'
  End
str=str||'v'w';'
str=str||'cnt.sum+=1;'
Do wi=1 To w
  str=str||'End;'
  End
Interpret str

psum='0/1'
Do x=0 To w*s
  If k=1 Then
    p1.x=cnt.x'/'||(s**w)
  Else
    p2.x=cnt.x'/'||(s**w)
  psum=fr_add(psum,p1.x)
  End
Return


fr_add: Procedure
Parse Arg a,b
parse Var a an '/' az
parse Var b bn '/' bz
rn=an*bz+bn*az
rz=az*bz
res=fr_cancel(rn','rz)
Return res

fr_div: Procedure
Parse Arg a,b
parse Var a an '/' az
parse Var b bn '/' bz
rn=an*bz
rz=az*bn
res=fr_cancel(rn','rz)
Return res

fr_mult: Procedure
Parse Arg a,b
parse Var a an '/' az
parse Var b bn '/' bz
rn=an*bn
rz=az*bz
res=fr_cancel(rn','rz)
Return res

fr_cancel: Procedure
Parse Arg n ',' z
k=ggt(n,z)
Return n%k'/'z%k

ggt: Procedure
/**********************************************************************
* ggt (gcd) Greatest common Divisor
* Recursive procedure as shown in PL/I
**********************************************************************/
Parse Arg a,b
if b = 0 then return abs(a)
return ggt(b,a//b)
```

{{out}}

```txt
Player 1 has 9 dice with 4 sides each
Player 2 has 6 dice with 6 sides each
Probability for player 1 to win: 48679795/84934656
                              -> 0.573144076782980082947530864198

Player 1 has 5 dice with 10 sides each
Player 2 has 6 dice with 7 sides each
Probability for player 1 to win: 3781171969/5882450000
                              -> 0.642788628717626159168373721834
```



### Algorithm using class fraction

Class definition adapted from Arithmetic/Raional.

```oorexx
Numeric Digits 50
Call test '9 4 6 6'
Call test '5 10 6 7'
Exit

test:
Parse Arg w1 s1 w2 s2
p1.=.fraction~new(0,1)
p2.=.fraction~new(0,1)
Call pp 1,w1,s1,p1.,p2.
Call pp 2,w2,s2,p1.,p2.
p2low.=.fraction~new(0,1)
Do x=w1 To w1*s1
  Do y=0 To x-1
    p2low.x=p2low.x+p2.y
    End
  End
pwin1=.fraction~new(0,1)
Do x=w1 To w1*s1
  pwin1=pwin1+(p1.x*p2low.x)
  End
Say 'Player 1 has' w1 'dice with' s1 'sides each'
Say 'Player 2 has' w2 'dice with' s2 'sides each'
Say 'Probability for player 1 to win:' pwin1~string
Say '                              ->' pwin1~tonumber
Say ''
Return

pp: Procedure
/*---------------------------------------------------------------------
* Compute and assign probabilities to get a sum x
* when throwing w dice each having s sides (marked from 1 to s)
* k=1 sets p1.*, k=2 sets p2.*
*--------------------------------------------------------------------*/
Use Arg k,w,s,p1.,p2.
str=''
cnt.=0
Do wi=1 To w
  str=str||'Do v'wi'=1 To' s';'
  End
str=str||'sum='
Do wi=1 To w-1
  str=str||'v'wi'+'
  End
str=str||'v'w';'
str=str||'cnt.sum+=1;'
Do wi=1 To w
  str=str||'End;'
  End
Interpret str

psum=.fraction~new(0,1)
Do x=0 To w*s
  If k=1 Then
    p1.x=.fraction~new(cnt.x,s**w)
  Else
    p2.x=.fraction~new(cnt.x,s**w)
  psum=psum+p1.x
  End
Return

::class fraction inherit orderable
::options Digits 50
::method init
  expose numerator denominator
  use strict arg numerator, denominator = 1
  --Trace ?R
  --if numerator == 0 then denominator = 0
  --else if denominator == 0 then raise syntax 98.900 array("Fraction denominator cannot be zero")

  -- if the denominator is negative, make the numerator carry the sign
  if denominator < 0 then do
      numerator = -numerator
      denominator = - denominator
  end


  -- find the greatest common denominator and reduce to
  -- the simplest form
  gcd = self~gcd(numerator~abs, denominator~abs)

  numerator /= gcd
  denominator /= gcd

-- fraction instances are immutable, so these are
-- read only attributes

-- calculate the greatest common denominator of a numerator/denominator pair
::method gcd private
  use arg x, y
  --Say 'gcd:' x y digits()
  loop while y \= 0
      -- check if they divide evenly
      temp = x // y
      x = y
      y = temp
  end
  return x

-- calculate the least common multiple of a numerator/denominator pair
::method lcm private
  use arg x, y
  return x / self~gcd(x, y) * y

::method abs
  expose numerator denominator
  -- the denominator is always forced to be positive
  return self~class~new(numerator~abs, denominator)

-- convert a fraction to regular Rexx number
::method toNumber
  expose numerator denominator
  if numerator == 0 then return 0
  return numerator/denominator

::method add
  expose numerator denominator
  use strict arg other
  -- convert to a fraction if a regular number
  if \other~isa(.fraction) then other = self~class~new(other, 1)

  multiple = self~lcm(denominator, other~denominator)
  newa = numerator * multiple / denominator
  newb = other~numerator * multiple / other~denominator
  return self~class~new(newa + newb, multiple)

::method times
  expose numerator denominator
  use strict arg other
  -- convert to a fraction if a regular number
  if \other~isa(.fraction) then other = self~class~new(other, 1)
  return self~class~new(numerator * other~numerator, denominator * other~denominator)

-- some operator overrides -- these only work if the left-hand-side of the
-- subexpression is a quaternion
::method "*"
  forward message("TIMES")

::method "+"
  -- need to check if this is a prefix plus or an addition
  if arg() == 0 then
      return self  -- we can return this copy since it is imutable
  else
      forward message("ADD")

::method string
  expose numerator denominator
  if denominator == 1 then return numerator
  return numerator"/"denominator

-- override hashcode for collection class hash uses
::method hashCode
  expose numerator denominator
  return numerator~hashcode~bitxor(numerator~hashcode)

::attribute numerator GET
::attribute denominator GET

::requires rxmath library
```

{{out}}

```txt
Player 1 has 9 dice with 4 sides each
Player 2 has 6 dice with 6 sides each
Probability for player 1 to win: 48679795/84934656
                              -> 0.57314407678298008294753086419753086419753086419753

Player 1 has 5 dice with 10 sides each
Player 2 has 6 dice with 7 sides each
Probability for player 1 to win: 3781171969/5882450000
                              -> 0.64278862871762615916837372183358974576919480828566
```



### Test

Result from 10 million tries.

```oorexx
oid='diet.xxx'; Call sysFileDelete oid
Call test '9  4 6 6'
Call test '5 10 6 7'
Exit
test:
Parse Arg n1 s1 n2 s2
Call o 'Player 1:' n1 'dice with' s1 'sides each'
Call o 'Player 2:' n2 'dice with' s2 'sides each'
cnt1.=0
cnt2.=0
win.=0
nn=10000000
Call time 'R'
Do i=1 To nn
  sum1=sum(n1 s1) ; cnt1.sum1+=1
  sum2=sum(n2 s2) ; cnt2.sum2+=1
  Select
    When sum1>sum2 Then win.1+=1
    When sum1<sum2 Then win.2+=1
    Otherwise           win.0+=1
    End
  End
Call o win.1/nn 'player 1 wins'
Call o win.2/nn 'player 2 wins'
Call o win.0/nn 'draws'
/*
Do i=min(n1,n2) To max(n1*s1,n2*s2)
  Call o right(i,2) format(cnt1.i,7) format(cnt2.i,7)
  End
*/
Call o time('E') 'seconds elapsed'
Return

sum: Parse Arg n s
sum=0
Do k=1 To n
  sum+=rand(s)
  End
Return sum

rand: Parse Arg n
 Return random(n-1)+1

o:
Say arg(1)
Return lineout(oid,arg(1))
```


{{out}}

```txt
Player 1: 9 dice with 4 sides each
Player 2: 6 dice with 6 sides each
0.5730344 player 1 wins
0.3562464 player 2 wins
0.0707192 draws
186.794000 seconds elapsed
Player 1: 5 dice with 10 sides each
Player 2: 6 dice with 7 sides each
0.6425906 player 1 wins
0.312976 player 2 wins
0.0444334 draws
149.784000 seconds elapsed
```



## Perl

{{trans|Python}}

```perl
use List::Util qw(sum0 max);

sub comb {
    my ($s, $n) = @_;
    $n || return (1);
    my @r = (0) x ($n - max(@$s) + 1);
    my @c = comb($s, $n - 1);
    foreach my $i (0 .. $#c) {
        $c[$i] || next;
        foreach my $k (@$s) {
            $r[$i + $k] += $c[$i];
        }
    }
    return @r;
}

sub winning {
    my ($s1, $n1, $s2, $n2) = @_;

    my @p1 = comb($s1, $n1);
    my @p2 = comb($s2, $n2);

    my ($win, $loss, $tie) = (0, 0, 0);

    foreach my $i (0 .. $#p1) {
        $win  += $p1[$i] * sum0(@p2[0    .. $i - 1]);
        $tie  += $p1[$i] * sum0(@p2[$i   .. $i    ]);
        $loss += $p1[$i] * sum0(@p2[$i+1 .. $#p2  ]);
    }
    my $total = sum0(@p1) * sum0(@p2);
    map { $_ / $total } ($win, $tie, $loss);
}

print '(', join(', ', winning([1 ..  4], 9, [1 .. 6], 6)), ")\n";
print '(', join(', ', winning([1 .. 10], 5, [1 .. 7], 6)), ")\n";
```

{{out}}

```txt

(0.57314407678298, 0.070766169838454, 0.356089753378566)
(0.642788628717626, 0.0444960303104999, 0.312715340971874)

```



## Perl 6

{{Works with|rakudo|2018.02}}

```perl6
sub likelihoods ($roll) {
    my ($dice, $faces) = $roll.comb(/\d+/);
    my @counts;
    @counts[$_]++ for [X+] |(1..$faces,) xx $dice;
    return [@counts[]:p], $faces ** $dice;
}

sub beating-probability ([$roll1, $roll2]) {
    my (@c1, $p1) := likelihoods $roll1;
    my (@c2, $p2) := likelihoods $roll2;
    my $p12 = $p1 * $p2;

    [+] gather for flat @c1 X @c2 -> $p, $q {
	take $p.value * $q.value / $p12 if $p.key > $q.key;
    }
}

# We're using standard DnD notation for dice rolls here.
say .gist, "\t", .perl given beating-probability < 9d4 6d6 >;
say .gist, "\t", .perl given beating-probability < 5d10 6d7 >;
```

{{out}}

```txt
0.573144077	<48679795/84934656>
0.64278862872	<3781171969/5882450000>
```

Note that all calculations are in integer and rational arithmetic, so the results in fractional notation are exact.


## Phix

{{trans|Go}}

```Phix
function throwDie(integer nSides, nDice, s, sequence counts)
    if nDice == 0 then
        counts[s] += 1
    else
        for i=1 to nSides do
            counts = throwDie(nSides, nDice-1, s+i, counts)
        end for
    end if
    return counts
end function

function beatingProbability(integer nSides1, nDice1, nSides2, nDice2)
    integer len1 := (nSides1 + 1) * nDice1,
            len2 := (nSides2 + 1) * nDice2
    sequence c1 = throwDie(nSides1, nDice1, 0, repeat(0,len1)),
             c2 = throwDie(nSides2, nDice2, 0, repeat(0,len2))
    atom p12 := power(nSides1, nDice1) * power(nSides2, nDice2),
         tot := 0.0
    for i=1 to len1 do
        for j=1 to min(i-1,len2) do
            tot += (c1[i] * c2[j]) / p12
        end for
    end for
    return tot
end function

printf(1,"%0.16f\n",beatingProbability(4, 9, 6, 6))
printf(1,"%0.16f\n",beatingProbability(10, 5, 7, 6))
```

{{out}}
<small>
(aside: the following tiny discrepancies are to be expected when using IEEE-754 64/80-bit floats; if you want to read anything into them, it should just be that we are all using the same hardware, and probably showing a couple too many digits of (in)accuracy on 32-bit.)
</small>

32 bit, same as Go, Kotlin

```txt

0.5731440767829815
0.6427886287176273

```

64 bit, same as D, Python[last], Ruby, Tcl

```txt

0.5731440767829801
0.6427886287176262

```



## PL/I


### version 1


```pli
*process source attributes xref;
 dicegame: Proc Options(main);
 Call test(9, 4,6,6);
 Call test(5,10,6,7);
 test: Proc(w1,s1,w2,s2);
 Dcl (w1,s1,w2,s2,x,y) Bin Fixed(31);
 Dcl p1(100)    Dec Float(18) Init((100)0);
 Dcl p2(100)    Dec Float(18) Init((100)0);
 Dcl p2low(100) Dec Float(18) Init((100)0);
 Call pp(w1,s1,p1);
 Call pp(w2,s2,p2);
 Do x=w1 To w1*s1;
   Do y=0 To x-1;
     p2low(x)+=p2(y);
     End;
   End;
 pwin1=0;
 Do x=w1 To w1*s1;
  pwin1+=p1(x)*p2low(x);
  End;
 Put Edit('Player 1 has ',w1,' dice with ',s1,' sides each')
         (Skip,3(a,f(2)));
 Put Edit('Player 2 has ',w2,' dice with ',s2,' sides each')
         (Skip,3(a,f(2)));
 Put Edit('Probability for player 1 to win: ',pwin1)(Skip,a,f(20,18));
 Put Edit('')(Skip,a);
 End;
 pp: Proc(w,s,p);
 /*--------------------------------------------------------------------
 * Compute and assign probabilities to get a sum x
 * when throwing w dice each having s sides (marked from 1 to s)
 *-------------------------------------------------------------------*/
 Dcl (w,s)    Bin Fixed(31);
 Dcl p(100)   Dec Float(18);
 Dcl cnt(100) Bin Fixed(31);
 Dcl (a(12),e(12),v(12),sum,i,n) Bin Fixed(31);
 a=0;
 e=0;
 Do i=1 To w;
   a(i)=1;
   e(i)=s;
   End;
 n=0;
 cnt=0;
 Do v(1)=a(1) To e(1);
   Do v(2)=a(2) To e(2);
     Do v(3)=a(3) To e(3);
       Do v(4)=a(4) To e(4);
         Do v(5)=a(5) To e(5);
           Do v(6)=a(6) To e(6);
             Do v(7)=a(7) To e(7);
               Do v(8)=a(8) To e(8);
                 Do v(9)=a(9) To e(9);
                   Do v(10)=a(10) To e(10);
                     sum=0;
                     Do i=1 To 10;
                       sum=sum+v(i);
                       End;
                     cnt(sum)+=1;
                     n+=1;
                     End;
                   End;
                 End;
               End;
             End;
           End;
         End;
       End;
     End;
   End;
 Do k=w To w*s;
   p(k)=divide(cnt(k),n,18,16);
   End;
 End;
 End;
```

{{out}}

```txt
Player 1 has  9 dice with  4 sides each
Player 2 has  6 dice with  6 sides each
Probability for player 1 to win: 0.573013663291931152

Player 1 has  5 dice with 10 sides each
Player 2 has  6 dice with  7 sides each
Probability for player 1 to win: 0.642703175544738770
```


### version 2 using rational arithmetic


```pli
*process source attributes xref;
 dgf: Proc Options(main);
 Call test(9, 4,6,6);
 Call test(5,10,6,7);
 test: Proc(w1,s1,w2,s2);
 Dcl (w1,s1,w2,s2,x,y) Dec Float(18);
 Dcl 1 p1(100),
      2 nom      Dec Float(18) Init((100)0),
      2 denom    Dec Float(18) Init((100)1);
 Dcl 1 p2(100),
      2 nom      Dec Float(18) Init((100)0),
      2 denom    Dec Float(18) Init((100)1);
 Dcl 1 p2low(100),
      2 nom      Dec Float(18) Init((100)0),
      2 denom    Dec Float(18) Init((100)1);
 Dcl 1 pwin1,
      2 nom      Dec Float(18) Init(0),
      2 denom    Dec Float(18) Init(1);
 Dcl 1 prod Like pwin1;
 Call pp(w1,s1,p1);
 Call pp(w2,s2,p2);
 Do x=w1 To w1*s1;
   Do y=0 To x-1;
     Call fr_add(p2low(x),p2(y),p2low(x));
     End;
   End;
 Do x=w1 To w1*s1;
  Call fr_mult(p1(x),p2low(x),prod);
  Call fr_add(pwin1,prod,pwin1);
  End;
 Put Edit('Player 1 has ',w1,' dice with ',s1,' sides each')
         (Skip,3(a,f(2)));
 Put Edit('Player 2 has ',w2,' dice with ',s2,' sides each')
         (Skip,3(a,f(2)));
 Put Edit('Probability for player 1 to win: ',
          str(pwin1.nom),'/',str(pwin1.denom))(Skip,4(a));
 Put Edit('                              -> ',
          pwin1.nom/pwin1.denom)(Skip,a,f(20,18));
 Put Edit('')(Skip,a);
 End;

 pp: Proc(w,s,p);
 /*--------------------------------------------------------------------
 * Compute and assign probabilities to get a sum x
 * when throwing w dice each having s sides (marked from 1 to s)
 *-------------------------------------------------------------------*/
 Dcl (w,s)    Dec Float(18);
 Dcl 1 p(100),
      2 nom   Dec Float(18),
      2 denom Dec Float(18);
 Dcl cnt(100) Dec Float(18);
 Dcl (a(12),e(12),v(12),sum,i,n) Dec Float(18);
 a=0;
 e=0;
 Do i=1 To w;
   a(i)=1;
   e(i)=s;
   End;
 n=0;
 cnt=0;
 Do v(1)=a(1) To e(1);
   Do v(2)=a(2) To e(2);
     Do v(3)=a(3) To e(3);
       Do v(4)=a(4) To e(4);
         Do v(5)=a(5) To e(5);
           Do v(6)=a(6) To e(6);
             Do v(7)=a(7) To e(7);
               Do v(8)=a(8) To e(8);
                 Do v(9)=a(9) To e(9);
                   Do v(10)=a(10) To e(10);
                     sum=0;
                     Do i=1 To 10;
                       sum=sum+v(i);
                       End;
                     cnt(sum)+=1;
                     n+=1;
                     End;
                   End;
                 End;
               End;
             End;
           End;
         End;
       End;
     End;
   End;
 Do k=w To w*s;
   p(k).nom=cnt(k);
   p(k).denom=n;
   End;
 End;

 fr_add: Proc(a,b,res);
 Dcl 1 a,
      2 nom   Dec Float(18),
      2 denom Dec Float(18);
 Dcl 1 b Like a;
 Dcl res like a;
 /* Put Edit('fr_add',a.nom,a.denom,b.nom,b.denom)(Skip,a,4(f(15))); */
 res.nom=a.nom*b.denom+b.nom*a.denom;
 res.denom=a.denom*b.denom;
 Call fr_cancel(res,res);
 End;

 fr_mult: Proc(a,b,res);
 Dcl 1 a,
      2 nom   Dec Float(18),
      2 denom Dec Float(18);
 Dcl 1 b Like a;
 Dcl res like a;
 /* Put Edit('fr_mult',a.nom,a.denom,b.nom,b.denom)(Skip,a,4(f(15)));*/
 res.nom=a.nom*b.nom;
 res.denom=a.denom*b.denom;
 Call fr_cancel(res,res);
 End;

 fr_cancel: Proc(a,res);
 Dcl 1 a,
      2 nom   Dec Float(18),
      2 denom Dec Float(18);
 Dcl k Dec Float(18);
 Dcl 1 res like a;
 /* Put Edit('fr_cancel',a.nom,a.denom)(Skip,a,4(f(15)));            */
 k=ggt(a.nom,a.denom);
 res=a/k;
 End;

 ggt: Proc(a,b) Recursive Returns(Dec Float(18));
 /**********************************************************************
 * ggt (gcd) Greatest common Divisor
 * Recursive Proc(a,b)) as shown in PL/I
 **********************************************************************/
 Dcl (a,b) Dec Float(18);
 if b = 0 then return (abs(a));
 return (ggt(b,mod(a,b)));
 End;

 str: Proc(x) Returns(Char(20) Var);
 Dcl x Dec Float(18);
 Dcl res Char(20) Var;
 Put String(res) Edit(x)(f(20));
 Return (trim(res));
 End;

 End;
```

{{out}}

```txt
Player 1 has  9 dice with  4 sides each
Player 2 has  6 dice with  6 sides each
Probability for player 1 to win: 48679795/84934656
                              -> 0.573144076782980083

Player 1 has  5 dice with 10 sides each
Player 2 has  6 dice with  7 sides each
Probability for player 1 to win: 3781171969/5882450000
                              -> 0.642788628717626159
```



## Python


```python
from itertools import product

def gen_dict(n_faces, n_dice):
    counts = [0] * ((n_faces + 1) * n_dice)
    for t in product(range(1, n_faces + 1), repeat=n_dice):
        counts[sum(t)] += 1
    return counts, n_faces ** n_dice

def beating_probability(n_sides1, n_dice1, n_sides2, n_dice2):
    c1, p1 = gen_dict(n_sides1, n_dice1)
    c2, p2 = gen_dict(n_sides2, n_dice2)
    p12 = float(p1 * p2)

    return sum(p[1] * q[1] / p12
               for p, q in product(enumerate(c1), enumerate(c2))
               if p[0] > q[0])

print beating_probability(4, 9, 6, 6)
print beating_probability(10, 5, 7, 6)
```

{{out}}

```txt
0.573144076783
0.642788628718
```


To handle larger number of dice (and faster in general):

```python
from __future__ import print_function, division

def combos(sides, n):
    if not n: return [1]
    ret = [0] * (max(sides)*n + 1)
    for i,v in enumerate(combos(sides, n - 1)):
        if not v: continue
        for s in sides: ret[i + s] += v
    return ret

def winning(sides1, n1, sides2, n2):
    p1, p2 = combos(sides1, n1), combos(sides2, n2)
    win,loss,tie = 0,0,0 # 'win' is 1 beating 2
    for i,x1 in enumerate(p1):
        # using accumulated sum on p2 could save some time
        win += x1*sum(p2[:i])
        tie += x1*sum(p2[i:i+1])
        loss+= x1*sum(p2[i+1:])
    s = sum(p1)*sum(p2)
    return win/s, tie/s, loss/s

print(winning(range(1,5), 9, range(1,7), 6))
print(winning(range(1,11), 5, range(1,8), 6)) # this seem hardly fair

# mountains of dice test case
# print(winning((1, 2, 3, 5, 9), 700, (1, 2, 3, 4, 5, 6), 800))
```

{{out}}

```txt

(0.5731440767829801, 0.070766169838454, 0.3560897533785659)
(0.6427886287176262, 0.044496030310499875, 0.312715340971874)

```


If we further restrict die faces to be 1 to n instead of arbitrary values, the combo generation can be made much faster:

```python
from __future__ import division, print_function
from itertools import accumulate # Python3 only

def combos(sides, n):
    ret = [1] + [0]*(n + 1)*sides # extra length for negative indices
    for p in range(1, n + 1):
        rolling_sum = 0
        for i in range(p*sides, p - 1, -1):
            rolling_sum += ret[i - sides] - ret[i]
            ret[i] = rolling_sum
        ret[p - 1] = 0
    return ret

def winning(d1, n1, d2, n2):
    c1, c2 = combos(d1, n1), combos(d2, n2)
    ac = list(accumulate(c2 + [0]*(len(c1) - len(c2))))

    return sum(v*a for  v,a in zip(c1[1:], ac)) / (ac[-1]*sum(c1))


print(winning(4, 9, 6, 6))
print(winning(5, 10, 6, 7))

#print(winning(6, 700, 8, 540))
```

{{out}}

```txt

0.5731440767829801
0.6427886287176262

```



## Racket



```racket
#lang racket

(define probs# (make-hash))

(define (NdD n d)
  (hash-ref!
   probs# (cons n d)
   (λ ()
     (cond
       [(= n 0) ; every chance of nothing!
        (hash 0 1)]
       [else
        (for*/fold ((hsh (hash))) (((i p) (in-hash (NdD (sub1 n) d))) (r (in-range 1 (+ d 1))))
          (hash-update hsh (+ r i) (λ (p+) (+ p+ (/ p d))) 0))]))))

(define (game-probs N1 D1 N2 D2)
  (define P1 (NdD N1 D1))
  (define P2 (NdD N2 D2))
  (define-values (W D L)
    (for*/fold ((win 0) (draw 0) (lose 0)) (((r1 p1) (in-hash P1)) ((r2 p2) (in-hash P2)))
      (define p (* p1 p2))
      (cond
        [(< r1 r2) (values win draw (+ lose p))]
        [(= r1 r2) (values win (+ draw p) lose)]
        [(> r1 r2) (values (+ win p) draw lose)])))

  (printf "P(P1 win): ~a~%" (real->decimal-string W 6))
  (printf "P(draw):   ~a~%" (real->decimal-string D 6))
  (printf "P(P2 win): ~a~%" (real->decimal-string L 6))
  (list W D L))

(printf "GAME 1 (9D4 vs 6D6)~%")
(game-probs 9 4 6 6)
(newline)

(printf "GAME 2 (5D10 vs 6D7) [what is a D7?]~%")
(game-probs 5 10 6 7)
```


{{out}}

```txt
GAME 1 (9D4 vs 6D6)
P(P1 win): 0.573144
P(draw):   0.070766
P(P2 win): 0.356090
(48679795/84934656 144252007/2038431744 725864657/2038431744)

GAME 2 (5D10 vs 6D7) [what is a D7?]
P(P1 win): 0.642789
P(draw):   0.044496
P(P2 win): 0.312715
(3781171969/5882450000 523491347/11764900000 735812943/2352980000)
```



## REXX


### version 1

{{trans|ooRexx}}
(adapted for Classic Rexx)

```rexx
/* REXX */
Numeric Digits 30
Call test '9 4 6 6'
Call test '5 10 6 7'
Exit
test:
Parse Arg w1 s1 w2 s2
plist1=pp(w1,s1)
p1.=0
Do x=w1 To w1*s1
  Parse Var plist1 p1.x plist1
  End
plist2=pp(w2,s2)
p2.=0
Do x=w2 To w2*s2
  Parse Var plist2 p2.x plist2
  End
p2low.=0
Do x=w1 To w1*s1
  Do y=0 To x-1
    p2low.x=p2low.x+p2.y
    End
  End
pwin1=0
Do x=w1 To w1*s1
  pwin1=pwin1+p1.x*p2low.x
  End
Say 'Player 1 has' w1 'dice with' s1 'sides each'
Say 'Player 2 has' w2 'dice with' s2 'sides each'
Say 'Probability for player 1 to win:' pwin1
Say ''
Return

pp: Procedure
/*---------------------------------------------------------------------
* Compute and return the probabilities to get a sum x
* when throwing w dice each having s sides (marked from 1 to s)
*--------------------------------------------------------------------*/
Parse Arg w,s
str=''
cnt.=0
Do wi=1 To w
  str=str||'Do v'wi'=1 To' s';'
  End
str=str||'sum='
Do wi=1 To w-1
  str=str||'v'wi'+'
  End
str=str||'v'w';'
str=str||'cnt.'sum'=cnt.'sum'+1;'
Do wi=1 To w
  str=str||'End;'
  End
Interpret str
psum=0
Do x=0 To w*s
  p.x=cnt.x/(s**w)
  psum=psum+p.x
  End
res=''
Do x=w To s*w
  res=res p.x
  End
Return res
```

{{out}}

```txt

Player 1 has 9 dice with 4 sides each
Player 2 has 6 dice with 6 sides each
Probability for player 1 to win: 0.573144076782980082947530864198

Player 1 has 5 dice with 10 sides each
Player 2 has 6 dice with 7 sides each
Probability for player 1 to win: 0.642788628717626159168373721835

```



### version 2


```rexx
/* REXX */
oid='diet.xxx'; 'erase' oid
Call test '9  4 6 6'
Call test '5 10 6 7'
Exit
test:
Parse Arg n1 s1 n2 s2
Call o 'Player 1:' n1 'dice with' s1 'sides each'
Call o 'Player 2:' n2 'dice with' s2 'sides each'
cnt1.=0
cnt2.=0
win.=0
nn=10000
Call time 'R'
Do i=1 To nn
  sum1=sum(n1 s1) ; cnt1.sum1=cnt1.sum1+1
  sum2=sum(n2 s2) ; cnt2.sum2=cnt2.sum2+1
  Select
    When sum1>sum2 Then win.1=win.1+1
    When sum1<sum2 Then win.2=win.2+1
    Otherwise           win.0=win.0+1
    End
  End
Call o win.1/nn 'player 1 wins'
Call o win.2/nn 'player 2 wins'
Call o win.0/nn 'draws'
/*
Do i=min(n1,n2) To max(n1*s1,n2*s2)
  Call o right(i,2) format(cnt1.i,7) format(cnt2.i,7)
  End
*/
Call o time('E') 'seconds elapsed'
Return

sum: Parse Arg n s
sum=0
Do k=1 To n
  sum=sum+rand(s)
  End
Return sum

rand: Parse Arg n
 Return random(n-1)+1

o:
Say arg(1)
Return lineout(oid,arg(1))
```

{{out}}

```txt
Player 1: 9 dice with 4 sides each
Player 2: 6 dice with 6 sides each
0.574 player 1 wins
0.3506 player 2 wins
0.0754 draws
0.109000 seconds elapsed
Player 1: 5 dice with 10 sides each
Player 2: 6 dice with 7 sides each
0.6411 player 1 wins
0.3115 player 2 wins
0.0474 draws
0.078000 seconds elapsed
```



### optimized

This REXX version is an optimized and reduced version of the first part of the first REXX example.

```rexx
/*REXX pgm computes and displays the probabilities of a two─player S─sided, N─dice game.*/
numeric digits 100                               /*increase│decrease for heart's desire.*/
call game  9  4, 6  6   /*1st player:  9 dice,  4 sides;   2nd player:  6 dice,  6 sides*/
call game  5 10, 6  7   /* "     "     5   "   10   "       "     "     6   "    7   "  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
game: parse arg  w.1  s.1,   w.2  s.2            /*1st player(dice sides), 2nd player···*/
      p.=0
                 do   j=1  for 2;         @@.j=prob(w.j, s.j)
                   do k=w.j  to w.j*s.j;  parse var  @@.j   p.j.k  @@.j;  end  /*k*/
                 end   /*j*/
      low.=0
                 do   j=w.1  to w.1*s.1
                   do k=0  for j;         low.j=low.j + p.2.k;            end  /*k*/
                 end   /*j*/
      say '   Player  1  has '       w.1       " dice with "       s.1      ' sides each.'
      say '   Player  2  has '       w.2       " dice with "       s.2      ' sides each.'
      winP=0
                 do   j=w.1  to w.1*s.1;  winP=winP   + p.1.j * low.j
                 end   /*j*/
      say 'The probability for first player to win is '     format(winP*100, , 30)    "%."
      say                                        /*                             ↑       */
      return                                     /*display 30 decimal digits────┘       */
/*──────────────────────────────────────────────────────────────────────────────────────*/
prob: procedure; parse arg n,s,,@ $;       #.=0;         pow=s**n
                                   do j=1  for n;    @=@'DO _'j"=1 for" s';';   end  /*j*/
      @=@'_=';                     do k=1  for n-1;  @=@"_"k'+'             ;   end  /*k*/
      interpret  @'_'n";   #."_'=#.'_"+1"   copies(';END', k)
      ns=n*s;                      do j=0  to ns;    p.j=#.j / pow;             end  /*j*/
                                   do k=n  to ns;    $=$ p.k;                   end  /*k*/
      return $   /* ◄──────────────── probability of 1st player to win, S─sided, N dice.*/
```

{{out|output|text=  when using the default inputs:}}

```txt

   Player  1  has  9  dice with  4  sides each.
   Player  2  has  6  dice with  6  sides each.
The probability for first player to win is  57.314407678298008294753086419753 %.

   Player  1  has  5  dice with  10  sides each.
   Player  2  has  6  dice with  7  sides each.
The probability for first player to win is  64.278862871762615916837372183359 %.

```



## Ruby


```ruby
def roll_dice(n_dice, n_faces)
  return [[0,1]] if n_dice.zero?
  one  = [1] * n_faces
  zero = [0] * (n_faces-1)
  (1...n_dice).inject(one){|ary,_|
    (zero + ary + zero).each_cons(n_faces).map{|a| a.inject(:+)}
  }.map.with_index(n_dice){|n,sum| [sum,n]}  # sum: total of the faces
end

def game(dice1, faces1, dice2, faces2)
  p1 = roll_dice(dice1, faces1)
  p2 = roll_dice(dice2, faces2)
  p1.product(p2).each_with_object([0,0,0]) do |((sum1, n1), (sum2, n2)), win|
    win[sum1 <=> sum2] += n1 * n2        # [0]:draw, [1]:win, [-1]:lose
  end
end

[[9, 4, 6, 6], [5, 10, 6, 7]].each do |d1, f1, d2, f2|
  puts "player 1 has #{d1} dice with #{f1} faces each"
  puts "player 2 has #{d2} dice with #{f2} faces each"
  win = game(d1, f1, d2, f2)
  sum = win.inject(:+)
  puts "Probability for player 1 to win: #{win[1]} / #{sum}",
       "                              -> #{win[1].fdiv(sum)}", ""
end
```


{{out}}

```txt

player 1 has 9 dice with 4 faces each
player 2 has 6 dice with 6 faces each
Probability for player 1 to win: 7009890480 / 12230590464
                              -> 0.5731440767829801

player 1 has 5 dice with 10 faces each
player 2 has 6 dice with 7 faces each
Probability for player 1 to win: 7562343938 / 11764900000
                              -> 0.6427886287176262

```



## Sidef

{{trans|Python}}

```ruby
func combos(sides, n) {
    n || return [1]
    var ret = ([0] * (n*sides.max + 1))
    combos(sides, n-1).each_kv { |i,v|
        v && for s in sides { ret[i + s] += v }
    }
    return ret
}

func winning(sides1, n1, sides2, n2) {
    var (p1, p2) = (combos(sides1, n1), combos(sides2, n2))
    var (win,loss,tie) = (0,0,0)
    p1.each_kv { |i, x|
        win  += x*p2.ft(0,i-1).sum
        tie  += x*p2.ft(i, i).sum
        loss += x*p2.ft(i+1).sum
    }
    [win, tie, loss] »/» p1.sum*p2.sum
}

func display_results(String title, Array res) {
    say "=> #{title}"
    for name, prob in (%w(p₁\ win tie p₂\ win) ~Z res) {
        say "P(#{'%6s' % name}) =~ #{prob.round(-11)} (#{prob.as_frac})"
    }
    print "\n"
}

display_results('9D4 vs 6D6',  winning(range(1, 4), 9, range(1,6), 6))
display_results('5D10 vs 6D7', winning(range(1,10), 5, range(1,7), 6))
```

{{out}}

```txt

=> 9D4 vs 6D6
P(p₁ win) =~ 0.57314407678 (48679795/84934656)
P(   tie) =~ 0.07076616984 (144252007/2038431744)
P(p₂ win) =~ 0.35608975338 (725864657/2038431744)

=> 5D10 vs 6D7
P(p₁ win) =~ 0.64278862872 (3781171969/5882450000)
P(   tie) =~ 0.04449603031 (523491347/11764900000)
P(p₂ win) =~ 0.31271534097 (735812943/2352980000)

```



## Tcl


To handle the nested loop in <tt>NdK</tt>, [[Tcl]]'s metaprogramming abilities are exercised.  The goal is to produce a script that looks like:

```Tcl
foreach d0 {1 2 3 4 5 6} {
  foreach d1 {1 2 3 4 5 6} {
    ...
      foreach dN {1 2 3 4 5 6} {
        dict incr sum [::tcl::mathop::+ $n $d0 $d1 ... $DN]
      }
    ...
  }
}
```


See the comments attached to that procedure for a more thorough understanding of how that is achieved (with the caveat that <tt>$d0..$dN</tt> are reversed).

Such metaprogramming is a very powerful technique in Tcl for building scripts where other approaches (in this case, recursion) might not be appealing, and should be in every programmer's toolbox!


```Tcl
proc range {b} {    ;# a common standard proc:  [range 5] -> {0 1 2 3 4}
    set a 0
    set res {}
    while {$a < $b} {
        lappend res $a
        incr a
    }
    return $res
}

# This proc builds up a nested foreach call, then evaluates it.
#
# The script is built up in $script, starting with the body using "%%" as
# a placeholder.
#
# For each die, a level is wrapped around it as follows:
#   set script {foreach d0 {1 2 3 4 5 6} $script}
#   set script {foreach d1 {1 2 3 4 5 6} $script}
#
# .. and {$d0 $d1 ..} are collected in the variable $vars, which is used
# to replace "%%" at the end.
#
# The script is evaluated with [try] - earlier Tcl's could use [catch] or [eval]
proc NdK {n {k 6}} {    ;# calculate a score histogram for $n dice of $k faces
    set sum {}
    set script {
        dict incr sum [::tcl::mathop::+ $n %%]   ;# add $n because ranges are 0-based
    }   ;# %% is a placeholder
    set vars ""
    for {set i 0} {$i < $n} {incr i} {
        set script [list foreach d$i [range $k] $script]
        append vars " \$d$i"
    }
    set script [string map [list %% $vars] $script]
    try $script
    return $sum
}

proc win_pr {p1 p2} {    ;# calculate the winning probability of player 1 given two score histograms
    set P 0
    set N 0
    dict for {d1 k1} $p1 {
        dict for {d2 k2} $p2 {
            set k [expr {$k1 * $k2}]
            incr N $k
            incr P [expr {$k * ($d1 > $d2)}]
        }
    }
    expr {$P * 1.0 / $N}
}

foreach {p1 p2} {
    {9 4}   {6 6}
    {5 10}  {6 7}
} {
    puts [format "p1 has %dd%d; p2 has %dd%d" {*}$p1 {*}$p2]
    puts [format " p1 wins with Pr(%s)" [win_pr [NdK {*}$p1] [NdK {*}$p2]]]
}
```


{{Out}}

```txt
p1 has 9d4; p2 has 6d6
 p1 wins with Pr(0.5731440767829801)
p1 has 5d10; p2 has 6d7
 p1 wins with Pr(0.6427886287176262)
```


===Factoring out <tt>foreach*</tt>===

The nested-loop generation illustrated above is useful to factor out as a routine by itself.  Here it is abstracted as <tt>foreach*</tt>, with <tt>NdK</tt> modified to suit.

I include this to <em>emphasise</em> the importance and power of metaprogramming in a Tcler's toolbox, as well as sharing a useful proc.


```Tcl
package require Tcl 8.6    ;# for [tailcall] - otherwise use [uplevel 1 $script]
# This proc builds up a nested foreach call, then evaluates it.
#
# this:
#   foreach* a {1 2 3} b {4 5 6} {
#     puts "$a + $b"
#   }
#
# becomes:
#   foreach a {1 2 3} {
#     foreach b {4 5 6} {
#       puts "$a + $b"
#     }
#   }
proc foreach* {args} {
    set script [lindex $args end]
    set args [lrange $args 0 end-1]
    foreach {b a} [lreverse $args] {
        set script [list foreach $a $b $script]
    }
    tailcall {*}$script
}

proc NdK {n {k 6}} {    ;# calculate a score histogram for $n dice of $k faces

    set args {}     ;# arguments to [foreach*]
    set vars {}     ;# variables used in [foreach*] arguments that need to be added to sum
    set sum {}      ;# this will be the result dictionary

    for {set i 0} {$i < $n} {incr i} {
        lappend args d$i [range $k]
        lappend vars "\$d$i"
    }

    set vars [join $vars +]

    # [string map] to avoid "Quoting Hell"
    set script [string map [list %% $vars] {
        dict incr sum [expr {$n + %%}]  ;# $n because [range] is 0-based
    }]

    foreach* {*}$args $script
    return $sum
}
```



## zkl

{{trans|Python}}

```zkl
fcn combos(sides, n){
   if(not n) return(T(1));
   ret:=((0).max(sides)*n + 1).pump(List(),0);
   foreach i,v in (combos(sides, n - 1).enumerate()){
      if(not v) continue;
      foreach s in (sides){ ret[i + s] += v }
   }
   ret
}

fcn winning(sides1,n1, sides2,n2){
   p1, p2 := combos(sides1, n1), combos(sides2, n2);
   win,loss,tie := 0,0,0; # 'win' is 1 beating 2
   foreach i,x1 in (p1.enumerate()){
      # using accumulated sum on p2 could save some time
      win += x1*p2[0,i].sum(0);
      tie += x1*p2[i,1].sum(0);  // i>p2.len() but p2[bigi,?]-->[]
      loss+= x1*p2[i+1,*].sum(0);
   }
   s := p1.sum(0)*p2.sum(0);
   return(win.toFloat()/s, tie.toFloat()/s, loss.toFloat()/s);
}
```


```zkl
println(winning([1..4].walk(), 9, [1..6].walk(),6));
println(winning([1..10].walk(),5, [1..7].walk(),6)); # this seem hardly fair
```

{{out}}

```txt

L(0.573144,0.0707662,0.35609)
L(0.642789,0.044496,0.312715)

```

