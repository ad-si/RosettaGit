+++
title = "Chinese remainder theorem"
description = ""
date = 2019-10-16T00:30:48Z
aliases = []
[extra]
id = 17124
[taxonomies]
categories = []
tags = []
+++

{{task}}

Suppose   <math>n_1</math>,   <math>n_2</math>,   <math>\ldots</math>,   <math>n_k</math>   are positive [[integer]]s that are pairwise co-prime.

Then, for any given sequence of integers   <math>a_1</math>,   <math>a_2</math>,   <math>\dots</math>,   <math>a_k</math>,   there exists an integer   <math>x</math>   solving the following system of simultaneous congruences:

::: <math>\begin{align}
  x &\equiv a_1 \pmod{n_1} \\
  x &\equiv a_2 \pmod{n_2} \\
    &{}\  \  \vdots \\
  x &\equiv a_k \pmod{n_k}
\end{align}</math>

Furthermore, all solutions   <math>x</math>   of this system are congruent modulo the product,    <math>N=n_1n_2\ldots n_k</math>.


;Task:
Write a program to solve a system of linear congruences by applying the   [[wp:Chinese Remainder Theorem|Chinese Remainder Theorem]].

If the system of equations cannot be solved, your program must somehow indicate this.

(It may throw an exception or return a special false value.)

Since there are infinitely many solutions, the program should return the unique solution   <math>s</math>   where   <math>0 \leq s \leq n_1n_2\ldots n_k</math>.


''Show the functionality of this program'' by printing the result such that the   <math>n</math>'s   are   <math>[3,5,7]</math>   and the   <math>a</math>'s   are   <math>[2,3,2]</math>.


'''Algorithm''':   The following algorithm only applies if the   <math>n_i</math>'s   are pairwise co-prime.

Suppose, as above, that a solution is required for the system of congruences:

::: <math>x \equiv a_i \pmod{n_i} \quad\mathrm{for}\; i = 1, \ldots, k</math>

Again, to begin, the product   <math>N = n_1n_2 \ldots n_k</math>   is defined.

Then a solution   <math>x</math>   can be found as follows:

For each   <math>i</math>,   the integers   <math>n_i</math>   and   <math>N/n_i</math>   are co-prime.

Using the   [[wp:Extended Euclidean algorithm|Extended Euclidean algorithm]],   we can find integers   <math>r_i</math>   and   <math>s_i</math>   such that   <math>r_i n_i + s_i N/n_i = 1</math>.

Then, one solution to the system of simultaneous congruences is:

::: <math>x = \sum_{i=1}^k a_i s_i N/n_i</math>

and the minimal solution,

::: <math>x \pmod{N}</math>.





## 11l

{{trans|Python}}

```11l
F mul_inv(=a, =b)
   V b0 = b
   V x0 = 0
   V x1 = 1
   I b == 1
      R 1
   L a > 1
      V q = a I/ b
      (a, b) = (b, a % b)
      (x0, x1) = (x1 - q * x0, x0)
   I x1 < 0
      x1 += b0
   R x1

F chinese_remainder(n, a)
   V sum = 0
   V prod = product(n)
   L(n_i, a_i) zip(n, a)
      V p = prod I/ n_i
      sum += a_i * mul_inv(p, n_i) * p
   R sum % prod

V n = [3, 5, 7]
V a = [2, 3, 2]
print(chinese_remainder(n, a))
```

{{out}}

```txt
23
```



## 360 Assembly

{{trans|REXX}}

```360asm
*        Chinese remainder theorem 06/09/2015
CHINESE  CSECT
         USING  CHINESE,R12        base addr
         LR     R12,R15
BEGIN    LA     R9,1               m=1
         LA     R6,1               j=1
LOOPJ    C      R6,NN              do j=1 to nn
         BH     ELOOPJ
         LR     R1,R6              j
         SLA    R1,2               j*4
         M      R8,N-4(R1)         m=m*n(j)
         LA     R6,1(R6)           j=j+1
         B      LOOPJ
ELOOPJ   LA     R6,1               x=1
LOOPX    CR     R6,R9              do x=1 to m
         BH     ELOOPX
         LA     R7,1               i=1
LOOPI    C      R7,NN              do i=1 to nn
         BH     ELOOPI
         LR     R1,R7              i
         SLA    R1,2               i*4
         LR     R5,R6              x
         LA     R4,0
         D      R4,N-4(R1)         x//n(i)
         C      R4,A-4(R1)         if x//n(i)^=a(i)
         BNE    ITERX              then iterate x
         LA     R7,1(R7)           i=i+1
         B      LOOPI
ELOOPI   MVC    PG(2),=C'x='
         XDECO  R6,PG+2            edit x
         XPRNT  PG,14              print buffer
         B      RETURN
ITERX    LA     R6,1(R6)           x=x+1
         B      LOOPX
ELOOPX   XPRNT  NOSOL,17           print
RETURN   XR     R15,R15            rc=0
         BR     R14
NN       DC     F'3'
N        DC     F'3',F'5',F'7'
A        DC     F'2',F'3',F'2'
PG       DS     CL80
NOSOL    DC     CL17'no solution found'
         YREGS
         END    CHINESE
```

{{out}}

```txt

x=          23

```



## Ada


Using the package Mod_Inv from [[http://rosettacode.org/wiki/Modular_inverse#Ada]].


```Ada
with Ada.Text_IO, Mod_Inv;

procedure Chin_Rema is
   N: array(Positive range <>) of Positive := (3, 5, 7);
   A: array(Positive range <>) of Positive := (2, 3, 2);
   Tmp: Positive;
   Prod: Positive := 1;
   Sum: Natural := 0;

begin
   for I in N'Range loop
      Prod := Prod * N(I);
   end loop;

   for I in A'Range loop
      Tmp := Prod / N(I);
      Sum := Sum + A(I) * Mod_Inv.Inverse(Tmp, N(I)) * Tmp;
   end loop;
   Ada.Text_IO.Put_Line(Integer'Image(Sum mod Prod));
end Chin_Rema;
```



## AWK

{{trans|C}}
We are using the split-function to create both arrays, thus the indices start at 1. This is the only difference to the C version.

```awk
# Usage: GAWK -f CHINESE_REMAINDER_THEOREM.AWK
BEGIN {
    len = split("3 5 7", n)
    len = split("2 3 2", a)
    printf("%d\n", chineseremainder(n, a, len))
}
function chineseremainder(n, a, len,    p, i, prod, sum) {
    prod = 1
    sum = 0
    for (i = 1; i <= len; i++)
        prod *= n[i]
    for (i = 1; i <= len; i++) {
        p = prod / n[i]
        sum += a[i] * mulinv(p, n[i]) * p
    }
    return sum % prod
}
function mulinv(a, b,    b0, t, q, x0, x1) {
    # returns x where (a * x) % b == 1
    b0 = b
    x0 = 0
    x1 = 1
    if (b == 1)
        return 1
    while (a > 1) {
        q = int(a / b)
        t = b
        b = a % b
        a = t
        t = x0
        x0 = x1 - q * x0
        x1 = t
    }
    if (x1 < 0)
        x1 += b0
    return x1
}
```

{{out}}

```txt
23
```



## Bracmat

{{trans|C}}

```bracmat
( ( mul-inv
  =   a b b0 q x0 x1
    .   !arg:(?a.?b:?b0)
      & ( !b:1
        |   0:?x0
          & 1:?x1
          &   whl
            ' ( !a:>1
              &   (!b.mod$(!a.!b):?q.!x1+-1*!q*!x0.!x0)
                : (?a.?b.?x0.?x1)
              )
          & ( !x1:<0&!b0+!x1
            | !x1
            )
        )
  )
& ( chinese-remainder
  =   n a as p ns ni prod sum
    .   !arg:(?n.?a)
      & 1:?prod
      & 0:?sum
      & !n:?ns
      & whl'(!ns:%?ni ?ns&!prod*!ni:?prod)
      & !n:?ns
      & !a:?as
      &   whl
        ' ( !ns:%?ni ?ns
          & !as:%?ai ?as
          & div$(!prod.!ni):?p
          & !sum+!ai*mul-inv$(!p.!ni)*!p:?sum
          )
      & mod$(!sum.!prod):?arg
      & !arg
  )
& 3 5 7:?n
& 2 3 2:?a
& put$(str$(chinese-remainder$(!n.!a) \n))
);
```

Output:

```txt
23
```



## C

When n are not pairwise coprime, the program crashes due to division by zero, which is one way to convey error.

```c
#include <stdio.h>

// returns x where (a * x) % b == 1
int mul_inv(int a, int b)
{
	int b0 = b, t, q;
	int x0 = 0, x1 = 1;
	if (b == 1) return 1;
	while (a > 1) {
		q = a / b;
		t = b, b = a % b, a = t;
		t = x0, x0 = x1 - q * x0, x1 = t;
	}
	if (x1 < 0) x1 += b0;
	return x1;
}

int chinese_remainder(int *n, int *a, int len)
{
	int p, i, prod = 1, sum = 0;

	for (i = 0; i < len; i++) prod *= n[i];

	for (i = 0; i < len; i++) {
		p = prod / n[i];
		sum += a[i] * mul_inv(p, n[i]) * p;
	}

	return sum % prod;
}

int main(void)
{
	int n[] = { 3, 5, 7 };
	int a[] = { 2, 3, 2 };

	printf("%d\n", chinese_remainder(n, a, sizeof(n)/sizeof(n[0])));
	return 0;
}
```



## C++


```cpp
#include <iostream>
#include <numeric>
#include <vector>
#include <execution>

using namespace std;

int mulInv(int a, int b) {
	int b0 = b;
	int x0 = 0;
	int x1 = 1;

	if (b == 1) {
		return 1;
	}

	while (a > 1) {
		int q = a / b;
		int amb = a % b;
		a = b;
		b = amb;

		int xqx = x1 - q * x0;
		x1 = x0;
		x0 = xqx;
	}

	if (x1 < 0) {
		x1 += b0;
	}

	return x1;
}

int chineseRemainder(vector<int> n, vector<int> a) {
	int prod = std::reduce(std::execution::seq, n.begin(), n.end(), 1, [](int a, int b) { return a * b; });

	int sm = 0;
	for (int i = 0; i < n.size(); i++) {
		int p = prod / n[i];
		sm += a[i] * mulInv(p, n[i])*p;
	}

	return sm % prod;
}

int main() {
	vector<int> n = { 3, 5, 7 };
	vector<int> a = { 2, 3, 2 };

	cout << chineseRemainder(n,a) << endl;

	return 0;
}
```

{{out}}

```txt
23
```


## C#

```c#
using System;
using System.Linq;

namespace ChineseRemainderTheorem
{
    class Program
    {
        static void Main(string[] args)
        {
            int[] n = { 3, 5, 7 };
            int[] a = { 2, 3, 2 };

            int result = ChineseRemainderTheorem.Solve(n, a);

            int counter = 0;
            int maxCount = n.Length - 1;
            while (counter <= maxCount)
            {
                Console.WriteLine($"{result} ≡ {a[counter]} (mod {n[counter]})");
                counter++;
            }
        }
    }

    public static class ChineseRemainderTheorem
    {
        public static int Solve(int[] n, int[] a)
        {
            int prod = n.Aggregate(1, (i, j) => i * j);
            int p;
            int sm = 0;
            for (int i = 0; i < n.Length; i++)
            {
                p = prod / n[i];
                sm += a[i] * ModularMultiplicativeInverse(p, n[i]) * p;
            }
            return sm % prod;
        }

        private static int ModularMultiplicativeInverse(int a, int mod)
        {
            int b = a % mod;
            for (int x = 1; x < mod; x++)
            {
                if ((b * x) % mod == 1)
                {
                    return x;
                }
            }
            return 1;
        }
    }
}
```



## Clojure

Modeled after the Python version http://rosettacode.org/wiki/Category:Python

```lisp
(ns test-p.core
  (:require [clojure.math.numeric-tower :as math]))

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese_remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)           ; p = prod / n_i
                        egcd (extended-gcd p n_i)   ; Extended gcd
                        inv_p (second egcd)]        ; Second item is the inverse
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
                                                      ; (map vector n a) is same as
        ;                                             ; Python's version Zip (n, a)
    (mod sum-prod prod)))                             ; Result line

(def n [3 5 7])
(def a [2 3 2])

(println (chinese_remainder n a))

```


'''Output:'''

```txt

23

```



## Coffeescript


```coffeescript
crt = (n,a) ->
	sum = 0
	prod = n.reduce (a,c) -> a*c
	for [ni,ai] in _.zip n,a
		p = prod // ni
		sum += ai * p * mulInv p,ni
	sum % prod

mulInv = (a,b) ->
	b0 = b
	[x0,x1] = [0,1]
	if b==1 then return 1
	while a > 1
		q = a // b
		[a,b] = [b, a % b]
		[x0,x1] = [x1-q*x0, x0]
	if x1 < 0 then x1 += b0
	x1

print crt [3,5,7], [2,3,2]
```

'''Output:'''

```txt

23

```



## Common Lisp

Using function ''invmod'' from [[http://rosettacode.org/wiki/Modular_inverse#Common_Lisp]].

```lisp

(defun chinese-remainder (am)
"Calculates the Chinese Remainder for the given set of integer modulo pairs.
 Note: All the ni and the N must be coprimes."
  (loop :for (a . m) :in am
        :with mtot = (reduce #'* (mapcar #'(lambda(X) (cdr X)) am))
        :with sum  = 0
        :finally (return (mod sum mtot))
        :do
   (incf sum (* a (invmod (/ mtot m) m) (/ mtot m)))))

```


'''Output:'''

```txt

* (chinese-remainder '((2 . 3) (3 . 5) (2 . 7)))

23
* (chinese-remainder '((10 . 11) (4 . 12) (12 . 13)))

1000
* (chinese-remainder '((19 . 100) (0 . 23)))

1219
* (chinese-remainder '((10 . 11) (4 . 22) (9 . 19)))

debugger invoked on a SIMPLE-ERROR in thread
#<THREAD "main thread" RUNNING {1002A8B1B3}>:
  invmod: Values 418 and 11 are not coprimes.

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Exit debugger, returning to top level.

(INVMOD 418 11)
0]

```



## D

{{trans|Python}}

```d
import std.stdio, std.algorithm;

T chineseRemainder(T)(in T[] n, in T[] a) pure nothrow @safe @nogc
in {
    assert(n.length == a.length);
} body {
    static T mulInv(T)(T a, T b) pure nothrow @safe @nogc {
        auto b0 = b;
        T x0 = 0, x1 = 1;
        if (b == 1)
            return T(1);
        while (a > 1) {
            immutable q = a / b;
            immutable amb = a % b;
            a = b;
            b = amb;
            immutable xqx = x1 - q * x0;
            x1 = x0;
            x0 = xqx;
        }
        if (x1 < 0)
            x1 += b0;
        return x1;
    }

    immutable prod = reduce!q{a * b}(T(1), n);

    T p = 1, sm = 0;
    foreach (immutable i, immutable ni; n) {
        p = prod / ni;
        sm += a[i] * mulInv(p, ni) * p;
    }
    return sm % prod;
}

void main() {
    immutable n = [3, 5, 7],
              a = [2, 3, 2];
    chineseRemainder(n, a).writeln;
}
```

{{out}}

```txt
23
```



## EasyLang

{{trans|C}}
<lang>func mul_inv a b . x1 .
  b0 = b
  x1 = 1
  if b <> 1
    while a > 1
      q = a / b
      t = b
      b = a mod b
      a = t
      t = x0
      x0 = x1 - q * x0
      x1 = t
    .
    if x1 < 0
      x1 += b0
    .
  .
.
func remainder . n[] a[] r .
  prod = 1
  sum = 0
  for i range len n[]
    prod *= n[i]
  .
  for i range len n[]
    p = prod / n[i]
    call mul_inv p n[i] h
    sum += a[i] * h * p
    r = sum mod prod
  .
.
n[] = [ 3 5 7 ]
a[] = [ 2 3 2 ]
call remainder n[] a[] h
print h
```

{{out}}

```txt
23
```



## EchoLisp

'''egcd''' - extended gcd - and '''crt-solve''' - chinese remainder theorem solve - are included in math.lib.

```scheme

(lib 'math)
math.lib v1.10 ® EchoLisp
Lib: math.lib loaded.

(crt-solve '(2 3 2) '(3 5 7))
   → 23
(crt-solve '(2 3 2) '(7 1005 15))
💥 error: mod[i] must be co-primes : assertion failed : 1005

```



## Elixir

{{trans|Ruby}}
{{works with|Elixir|1.2}}
Brute-force:

```elixir
defmodule Chinese do
  def remainder(mods, remainders) do
    max = Enum.reduce(mods, fn x,acc -> x*acc end)
    Enum.zip(mods, remainders)
    |> Enum.map(fn {m,r} -> Enum.take_every(r..max, m) |> MapSet.new end)
    |> Enum.reduce(fn set,acc -> MapSet.intersection(set, acc) end)
    |> MapSet.to_list
  end
end

IO.inspect Chinese.remainder([3,5,7], [2,3,2])
IO.inspect Chinese.remainder([10,4,9], [11,22,19])
IO.inspect Chinese.remainder([11,12,13], [10,4,12])
```


{{out}}

```txt

[23]
[]
[1000]

```



## Erlang

{{trans|OCaml}}

```erlang
-module(crt).
-import(lists, [zip/2, unzip/1, foldl/3, sum/1]).
-export([egcd/2, mod/2, mod_inv/2, chinese_remainder/1]).

egcd(_, 0) -> {1, 0};
egcd(A, B) ->
    {S, T} = egcd(B, A rem B),
    {T, S - (A div B)*T}.

mod_inv(A, B) ->
    {X, Y} = egcd(A, B),
    if
        A*X + B*Y =:= 1 -> X;
        true -> undefined
    end.

mod(A, M) ->
    X = A rem M,
    if
        X < 0 -> X + M;
        true -> X
    end.

calc_inverses([], []) -> [];
calc_inverses([N | Ns], [M | Ms]) ->
    case mod_inv(N, M) of
        undefined -> undefined;
        Inv -> [Inv | calc_inverses(Ns, Ms)]
    end.

chinese_remainder(Congruences) ->
    {Residues, Modulii} = unzip(Congruences),
    ModPI = foldl(fun(A, B) -> A*B end, 1, Modulii),
    CRT_Modulii = [ModPI div M || M <- Modulii],
    case calc_inverses(CRT_Modulii, Modulii) of
        undefined -> undefined;
        Inverses ->
            Solution = sum([A*B || {A,B} <- zip(CRT_Modulii,
                                    [A*B || {A,B} <- zip(Residues, Inverses)])]),
            mod(Solution, ModPI)
    end.
```

{{out}}

```txt

16> crt:chinese_remainder([{10,11}, {4,12}, {12,13}]).
1000
17> crt:chinese_remainder([{10,11}, {4,22}, {9,19}]).
undefined
18> crt:chinese_remainder([{2,3}, {3,5}, {2,7}]).
23

```


=={{header|F_Sharp|F#}}==
===[https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving sieving]===

```fsharp
let rec sieve cs x N =
    match cs with
    | [] -> Some(x)
    | (a,n)::rest ->
        let arrProgress = Seq.unfold (fun x -> Some(x, x+N)) x
        let firstXmodNequalA = Seq.tryFind (fun x -> a = x % n)
        match firstXmodNequalA (Seq.take n arrProgress) with
        | None -> None
        | Some(x) -> sieve rest x (N*n)

[ [(2,3);(3,5);(2,7)];
  [(10,11); (4,22); (9,19)];
  [(10,11); (4,12); (12,13)] ]
|> List.iter (fun congruences ->
    let cs =
        congruences
        |> List.map (fun (a,n) -> (a % n, n))
        |> List.sortBy (snd>>(~-))
    let an = List.head cs
    match sieve (List.tail cs) (fst an) (snd an) with
    | None    -> printfn "no solution"
    | Some(x) -> printfn "result = %i" x
)
```

{{out}}

```txt
result = 23
no solution
result = 1000
```


### Or for those who prefer unsieved

This uses [[Greatest_common_divisor#F.23]] to verify valid input, can be simplified if you know input has a solution.


This uses [[Modular_inverse#F.23]]

```fsharp

//Chinese Division Theorem: Nigel Galloway: April 3rd., 2017
let CD n g =
  match Seq.fold(fun n g->if (gcd n g)=1 then n*g else 0) 1 g with
  |0 -> None
  |fN-> Some ((Seq.fold2(fun n i g -> n+i*(fN/g)*(MI g ((fN/g)%g))) 0 n g)%fN)

```

{{out}}

```txt

CD [10;4;12] [11;12;13] -> Some 1000
CD [10;4;9] [11;22;19]  -> None
CD [2;3;2] [3;5;7]      -> Some 23

```



## Factor


```factor
USING: math.algebra prettyprint ;
{ 2 3 2 } { 3 5 7 } chinese-remainder .
```

{{out}}

```txt

23

```



## Forth

Tested with GNU FORTH

```forth
: egcd ( a b -- a b )
    dup 0= IF
        2drop 1 0
    ELSE
        dup -rot /mod               \ -- b r=a%b q=a/b
        -rot recurse                \ -- q (s,t) = egcd(b, r)
        >r swap r@ * - r> swap      \ -- t (s - q*t)
    THEN ;

: egcd>gcd  ( a b x y -- n )  \ calculate gcd from egcd
    rot * -rot * + ;

: mod-inv  ( a m -- a' )     \ modular inverse with coprime check
    2dup egcd over >r egcd>gcd r> swap 1 <> -24 and throw ;

: array-product ( adr count -- n )
    1 -rot  cells bounds ?DO  i @ *  cell +LOOP ;

: crt-from-array  ( adr1 adr2 count -- n )
    2dup array-product   locals| M count m[] a[] |
    0  \ result
    count 0 DO
        m[] i cells + @
        dup M swap /
        dup rot mod-inv *
        a[] i cells + @ * +
    LOOP  M mod ;

create crt-residues[]  10 cells allot
create crt-moduli[]    10 cells allot

: crt ( .... n -- n )  \ takes pairs of "n (mod m)" from stack.
    10 min  locals| n |
    n 0 DO
        crt-moduli[] i cells + !
        crt-residues[] i cells + !
    LOOP
    crt-residues[] crt-moduli[] n crt-from-array ;

```

{{out}}

```txt

Gforth 0.7.2, Copyright (C) 1995-2008 Free Software Foundation, Inc.
Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
Type `bye' to exit
10 11  4 12  12 13  3 crt . 1000  ok
10 11  4 22   9 19  3 crt .
:2: Invalid numeric argument
10 11  4 22   9 19  3 >>>crt<<< .

```



## FunL


```funl
import integers.modinv

def crt( congruences ) =
    N = product( n | (_, n) <- congruences )
    sum( a*modinv(N/n, n)*N/n | (a, n) <- congruences ) mod N

println( crt([(2, 3), (3, 5), (2, 7)]) )
```


{{out}}


```txt

23

```



## Go

Go has the Extended Euclidean algorithm in the GCD function for big integers in the standard library.  GCD will return 1 only if numbers are coprime, so a result != 1 indicates the error condition.

```go
package main

import (
    "fmt"
    "math/big"
)

var one = big.NewInt(1)

func crt(a, n []*big.Int) (*big.Int, error) {
    p := new(big.Int).Set(n[0])
    for _, n1 := range n[1:] {
        p.Mul(p, n1)
    }
    var x, q, s, z big.Int
    for i, n1 := range n {
        q.Div(p, n1)
        z.GCD(nil, &s, n1, &q)
        if z.Cmp(one) != 0 {
            return nil, fmt.Errorf("%d not coprime", n1)
        }
        x.Add(&x, s.Mul(a[i], s.Mul(&s, &q)))
    }
    return x.Mod(&x, p), nil
}

func main() {
    n := []*big.Int{
        big.NewInt(3),
        big.NewInt(5),
        big.NewInt(7),
    }
    a := []*big.Int{
        big.NewInt(2),
        big.NewInt(3),
        big.NewInt(2),
    }
    fmt.Println(crt(a, n))
}
```

{{out}}
Two values, the solution x and an error value.

```txt

23 <nil>

```



## Haskell

{{trans|Erlang}}

```haskell
import Control.Monad (zipWithM)

egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b

modInv :: Int -> Int -> Either String Int
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> Right x
      | otherwise ->
        Left $ "No modular inverse for " ++ show a ++ " and " ++ show b

chineseRemainder :: [Int] -> [Int] -> Either String Int
chineseRemainder residues modulii =
  zipWithM modInv crtModulii modulii >>=
  (Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii

main :: IO ()
main =
  mapM_ (putStrLn . either id show) $
  uncurry chineseRemainder <$>
  [ ([10, 4, 12], [11, 12, 13])
  , ([10, 4, 9], [11, 22, 19])
  , ([2, 3, 2], [3, 5, 7])
  ]
```

{{Out}}

```txt
1000
No modular inverse for 418 and 11
23
```


=={{header|Icon}} and {{header|Unicon}}==

{{trans|Python}} with error check added.
Works in both languages:

```unicon
link numbers   # for gcd()

procedure main()
    write(cr([3,5,7],[2,3,2]) | "No solution!")
    write(cr([10,4,9],[11,22,19]) | "No solution!")
end

procedure cr(n,a)
    if 1 ~= gcd(n[i := !*n],a[i]) then fail  # Not pairwise coprime
    (prod := 1, sm := 0)
    every prod *:= !n
    every p := prod/(ni := n[i := !*n]) do sm +:= a[i] * mul_inv(p,ni) * p
    return sm%prod
end

procedure mul_inv(a,b)
    if b = 1 then return 1
    (b0 := b, x0 := 0, x1 := 1)
    while q := (1 < a)/b do {
        (t := a, a := b, b := t%b)
        (t := x0, x0 := x1-q*t, x1 := t)
        }
    return if x1 < 0 then x1+b0 else x1
end
```


Output:

```txt

->crt
23
No solution!
->

```



## J


'''Solution''' (''brute force''):
```j
   crt =: (1 + ] - {:@:[ -: {.@:[ | ])^:_&0@:,:
```

'''Example''':
```j
   3 5 7 crt 2 3 2
23
   11 12 13 crt 10 4 12
1000
```

'''Notes''': This is a brute force approach and does not meet the requirement for explicit notification of an an unsolvable set of equations (it just spins forever).  A much more thorough and educational approach can be found on the [[j:Essays/Chinese%20Remainder%20Theorem|J wiki's Essay on the Chinese Remainder Thereom]].


## Java

Translation of [[Chinese_remainder_theorem#Python|Python]] via [[Chinese_remainder_theorem#D|D]]
{{works with|Java|8}}

```java
import static java.util.Arrays.stream;

public class ChineseRemainderTheorem {

    public static int chineseRemainder(int[] n, int[] a) {

        int prod = stream(n).reduce(1, (i, j) -> i * j);

        int p, sm = 0;
        for (int i = 0; i < n.length; i++) {
            p = prod / n[i];
            sm += a[i] * mulInv(p, n[i]) * p;
        }
        return sm % prod;
    }

    private static int mulInv(int a, int b) {
        int b0 = b;
        int x0 = 0;
        int x1 = 1;

        if (b == 1)
            return 1;

        while (a > 1) {
            int q = a / b;
            int amb = a % b;
            a = b;
            b = amb;
            int xqx = x1 - q * x0;
            x1 = x0;
            x0 = xqx;
        }

        if (x1 < 0)
            x1 += b0;

        return x1;
    }

    public static void main(String[] args) {
        int[] n = {3, 5, 7};
        int[] a = {2, 3, 2};
        System.out.println(chineseRemainder(n, a));
    }
}
```



```txt
23
```



## jq

This implementation is similar to the one in C, but raises an error if there is no solution, as illustrated in the last example.

```jq
# mul_inv(a;b) returns x where (a * x) % b == 1, or else null
def mul_inv(a; b):

  # state: [a, b, x0, x1]
  def iterate:
    .[0] as $a | .[1] as $b
    | if $a > 1 then
        if $b == 0 then null
        else ($a / $b | floor) as $q
           | [$b, ($a % $b), (.[3] - ($q * .[2])), .[2]] | iterate
        end
      else .
      end ;

  if (b == 1) then 1
  else [a,b,0,1] | iterate
       | if . == null then .
         else  .[3] | if . <  0 then . + b else . end
         end
  end;

def chinese_remainder(mods; remainders):
  (reduce mods[] as $i (1; . * $i)) as $prod
  | reduce range(0; mods|length) as $i
      (0;
       ($prod/mods[$i]) as $p
       | mul_inv($p; mods[$i]) as $mi
       | if $mi == null then error("nogo: p=\($p) mods[\($i)]=\(mods[$i])")
         else . + (remainders[$i] * $mi * $p)
         end )
  | . % $prod ;
```

'''Examples''':
 chinese_remainder([3,5,7]; [2,3,2])
 # => 23
 chinese_remainder([100,23]; [19,0])
 # => 1219
 chinese_remainder([10,4,9]; [11,22,19])
 # jq: error: nogo: p=36 mods[0]=10


## Julia

{{works with|Julia|1.2}}


```julia
function chineseremainder(n::Array, a::Array)
    Π = prod(n)
    mod(sum(ai * invmod(Π ÷ ni, ni) * Π ÷ ni for (ni, ai) in zip(n, a)), Π)
end

@show chineseremainder([3, 5, 7], [2, 3, 2])
```


{{out}}

```txt
chineseremainder([3, 5, 7], [2, 3, 2]) = 23
```



## Kotlin

{{trans|C}}

```scala
// version 1.1.2

/* returns x where (a * x) % b == 1 */
fun multInv(a: Int, b: Int): Int {
    if (b == 1) return 1
    var aa = a
    var bb = b
    var x0 = 0
    var x1 = 1
    while (aa > 1) {
        val q = aa / bb
        var t = bb
        bb = aa % bb
        aa = t
        t = x0
        x0 = x1 - q * x0
        x1 = t
    }
    if (x1 < 0) x1 += b
    return x1
}

fun chineseRemainder(n: IntArray, a: IntArray): Int {
    val prod = n.fold(1) { acc, i -> acc * i }
    var sum = 0
    for (i in 0 until n.size) {
        val p = prod / n[i]
        sum += a[i] * multInv(p, n[i]) * p
    }
    return sum % prod
}

fun main(args: Array<String>) {
    val n = intArrayOf(3, 5, 7)
    val a = intArrayOf(2, 3, 2)
    println(chineseRemainder(n, a))
}
```


{{out}}

```txt

23

```



## Lua


```lua
-- Taken from https://www.rosettacode.org/wiki/Sum_and_product_of_an_array#Lua
function prodf(a, ...) return a and a * prodf(...) or 1 end
function prodt(t) return prodf(unpack(t)) end

function mulInv(a, b)
    local b0 = b
    local x0 = 0
    local x1 = 1

    if b == 1 then
        return 1
    end

    while a > 1 do
        local q = math.floor(a / b)
        local amb = math.fmod(a, b)
        a = b
        b = amb
        local xqx = x1 - q * x0
        x1 = x0
        x0 = xqx
    end

    if x1 < 0 then
        x1 = x1 + b0
    end

    return x1
end

function chineseRemainder(n, a)
    local prod = prodt(n)

    local p
    local sm = 0
    for i=1,#n do
        p = prod / n[i]
        sm = sm + a[i] * mulInv(p, n[i]) * p
    end

    return math.fmod(sm, prod)
end

n = {3, 5, 7}
a = {2, 3, 2}
io.write(chineseRemainder(n, a))
```

{{out}}

```txt
23
```



## Maple

This is a Maple built-in procedure, so it is trivial:

```Maple>
 chrem( [2, 3, 2], [3, 5, 7] );
                                           23

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Very easy, because it is a built-in function:

```Mathematica
ChineseRemainder[{2, 3, 2}, {3, 5, 7}]
23
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function f = chineseRemainder(r, m)
  s = prod(m) ./ m;
  [~, t] = gcd(s, m);
  f = s .* t * r';
```

{{out}}

```MATLAB>>
 chineseRemainder([2 3 2], [3 5 7])
 ans = 23
```


=={{header|Modula-2}}==

```modula2
MODULE CRT;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteInt(n : INTEGER);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%i", buf, n);
    WriteString(buf)
END WriteInt;

PROCEDURE MulInv(a,b : INTEGER) : INTEGER;
VAR
    b0,x0,x1,q,amb,xqx : INTEGER;
BEGIN
    b0 := b;
    x0 := 0;
    x1 := 1;

    IF b=1 THEN
        RETURN 1
    END;

    WHILE a>1 DO
        q := a DIV b;
        amb := a MOD b;
        a := b;
        b := amb;
        xqx := x1 - q * x0;
        x1 := x0;
        x0 := xqx
    END;

    IF x1<0 THEN
        x1 := x1 + b0
    END;

    RETURN x1
END MulInv;

PROCEDURE ChineseRemainder(n,a : ARRAY OF INTEGER) : INTEGER;
VAR
    i : CARDINAL;
    prod,p,sm : INTEGER;
BEGIN
    prod := n[0];
    FOR i:=1 TO HIGH(n) DO
        prod := prod * n[i]
    END;

    sm := 0;
    FOR i:=0 TO HIGH(n) DO
        p := prod DIV n[i];
        sm := sm + a[i] * MulInv(p, n[i]) * p
    END;

    RETURN sm MOD prod
END ChineseRemainder;

TYPE TA = ARRAY[0..2] OF INTEGER;
VAR n,a : TA;
BEGIN
    n := TA{3, 5, 7};
    a := TA{2, 3, 2};
    WriteInt(ChineseRemainder(n, a));
    WriteLn;

    ReadChar
END CRT.
```

{{out}}

```txt
23
```



## Nim

{{trans|C}}

```nim
proc mulInv(a0, b0): int =
  var (a, b, x0) = (a0, b0, 0)
  result = 1
  if b == 1: return
  while a > 1:
    let q = a div b
    a = a mod b
    swap a, b
    result = result - q * x0
    swap x0, result
  if result < 0: result += b0

proc chineseRemainder[T](n, a: T): int =
  var prod = 1
  var sum = 0
  for x in n: prod *= x

  for i in 0 .. <n.len:
    let p = prod div n[i]
    sum += a[i] * mulInv(p, n[i]) * p

  sum mod prod

echo chineseRemainder([3,5,7], [2,3,2])
```

Output:

```txt
23
```



## OCaml

This is using the Jane Street Ocaml Core library.

```ocaml
open Core.Std
open Option.Monad_infix

let rec egcd a b =
   if b = 0 then (1, 0)
   else
      let q = a/b and r = a mod b in
      let (s, t) = egcd b r in
         (t, s - q*t)


let mod_inv a b =
   let (x, y) = egcd a b in
      if a*x + b*y = 1 then Some x else None


let calc_inverses ns ms =
   let rec list_inverses ns ms l =
      match (ns, ms) with
         | ([], []) -> Some l
         | ([], _)
         | (_, []) -> assert false
         | (n::ns, m::ms) ->
            let inv = mod_inv n m in
               match inv with
                  | None -> None
                  | Some v -> list_inverses ns ms (v::l)
   in
      list_inverses ns ms [] >>= fun l -> Some (List.rev l)


let chinese_remainder congruences =
   let (residues, modulii) = List.unzip congruences in
   let mod_pi = List.reduce_exn modulii ~f:( * ) in
   let crt_modulii = List.map modulii ~f:(fun m -> mod_pi / m) in
   calc_inverses crt_modulii modulii >>=
      fun inverses ->
         Some (List.map3_exn residues inverses crt_modulii ~f:(fun a b c -> a*b*c)
               |> List.reduce_exn ~f:(+)
               |> fun n -> let n' = n mod mod_pi in if n' < 0 then n' + mod_pi else n')

```

{{out}}

```txt

utop # chinese_remainder [(10, 11); (4, 12); (12, 13)];;
- : int option = Some 1000

utop # chinese_remainder [(10, 11); (4, 22); (9, 19)];;
- : int option = None

```



## PARI/GP


```parigp
chivec(residues, moduli)={
  my(m=Mod(0,1));
  for(i=1,#residues,
    m=chinese(Mod(residues[i],moduli[i]),m)
  );
  lift(m)
};
chivec([2,3,2], [3,5,7])
```

{{out}}

```txt
23
```


Pari's chinese function takes a vector in the form <tt>[Mod(a1,n1), Mod(a2, n2), ...]</tt>, so we can do this directly:

```parigp
lift( chinese([Mod(2,3),Mod(3,5),Mod(2,7)]) )
```

or to take the residue/moduli array as above:

```parigp
chivec(residues,moduli)={
  lift(chinese(vector(#residues,i,Mod(residues[i],moduli[i]))))
}
```



## Perl

There are at least three CPAN modules for this:  ntheory (Math::Prime::Util), Math::ModInt, and Math::Pari.  All three handle bigints.
{{libheader|ntheory}}

```perl
use ntheory qw/chinese/;
say chinese([2,3], [3,5], [2,7]);
```

{{out}}

```txt
23
```

The function returns undef if no common residue class exists.  The combined modulus can be obtained using the <code>lcm</code> function applied to the moduli (e.g. <code>lcm(3,5,7) = 105</code> in the example above).


```perl
use Math::ModInt qw(mod);
use Math::ModInt::ChineseRemainder qw(cr_combine);
say cr_combine(mod(2,3),mod(3,5),mod(2,7));
```

{{out}}

```txt
mod(23, 105)
```

This returns a Math::ModInt object, which if no common residue class exists will be a special undefined object.  The <code>modulus</code> and <code>residue</code> methods may be used to extract the integer components.

=== Non-pairwise-coprime ===
All three modules will also handle cases where the moduli are not pairwise co-prime but a solution exists, e.g.:

```perl
use ntheory qw/chinese lcm/;
say chinese( [2328,16256], [410,5418] ), " mod ", lcm(16256,5418);
```

{{out}}

```txt
28450328 mod 44037504
```



## Perl 6

{{trans|C}}
{{works with|Rakudo|2015.12}}

```perl6
# returns x where (a * x) % b == 1
sub mul-inv($a is copy, $b is copy) {
    return 1 if $b == 1;
    my ($b0, @x) = $b, 0, 1;
    ($a, $b, @x) = (
	$b,
	$a % $b,
	@x[1] - ($a div $b)*@x[0],
	@x[0]
    ) while $a > 1;
    @x[1] += $b0 if @x[1] < 0;
    return @x[1];
}

sub chinese-remainder(*@n) {
    my \N = [*] @n;
    -> *@a {
	N R% [+] map {
	    my \p = N div @n[$_];
	    @a[$_] * mul-inv(p, @n[$_]) * p
	}, ^@n
    }
}

say chinese-remainder(3, 5, 7)(2, 3, 2);
```

{{out}}

```txt
23
```



## Phix

{{trans|C}}
Uses the function mul_inv() from [[Modular_inverse#Phix]]

```Phix
function chinese_remainder(sequence n, a)
    integer p, prod = 1, tot = 0;
    for i=1 to length(n) do prod *= n[i] end for
    for i=1 to length(n) do
        p = prod / n[i];
        object m = mul_inv(p, n[i])
        if string(m) then return "fail" end if
        tot += a[i] * m * p;
    end for
    return mod(tot,prod)
end function

?chinese_remainder({3,5,7},{2,3,2})
?chinese_remainder({11,12,13},{10,4,12})
?chinese_remainder({11,22,19},{10,4,9})
?chinese_remainder({100,23},{19,0})
```

{{out}}

```txt

23
1000
"fail"
1219

```



## PicoLisp


```PicoLisp
(de modinv (A B)
   (let (B0 B  X0 0  X1 1  Q 0  T1 0)
      (while (< 1 A)
         (setq
            Q (/ A B)
            T1 B
            B (% A B)
            A T1
            T1 X0
            X0 (- X1 (* Q X0))
            X1 T1 ) )
      (if (lt0 X1) (+ X1 B0) X1) ) )

(de chinrem (N A)
   (let P (apply * N)
      (%
         (sum
            '((N A)
               (setq T1 (/ P N))
               (* A (modinv T1 N) T1) )
            N
            A )
         P ) ) )

(println
   (chinrem (3 5 7) (2 3 2))
   (chinrem (11 12 13) (10 4 12)) )

(bye)
```



## PureBasic


```PureBasic
EnableExplicit
DisableDebugger
DataSection
  LBL_n1:
  Data.i 3,5,7
  LBL_a1:
  Data.i 2,3,2
  LBL_n2:
  Data.i 11,12,13
  LBL_a2:
  Data.i 10,4,12
  LBL_n3:
  Data.i 10,4,9
  LBL_a3:
  Data.i 11,22,19
EndDataSection

Procedure ErrorHdl()
  Print(ErrorMessage())
  Input()
EndProcedure

Macro PrintData(n,a)
  Define Idx.i=0
  Print("[")
  While n+SizeOf(Integer)*Idx<a
    Print("( ")
    Print(Str(PeekI(a+SizeOf(Integer)*Idx)))
    Print(" . ")
    Print(Str(PeekI(n+SizeOf(Integer)*Idx)))
    Print(" )")
    Idx+1
  Wend
  Print(~"]\nx = ")
EndMacro

Procedure.i Produkt_n(n_Adr.i,a_Adr.i)
  Define p.i=1
  While n_Adr<a_Adr
    p*PeekI(n_Adr)
    n_Adr+SizeOf(Integer)
  Wend
  ProcedureReturn p
EndProcedure

Procedure.i Eval_x1(a.i,b.i)
  Define b0.i=b, x0.i=0, x1.i=1, q.i, t.i
  If b=1 : ProcedureReturn x1 : EndIf
  While a>1
    q=Int(a/b)
    t=b : b=a%b : a=t
    t=x0 : x0=x1-q*x0 : x1=t
  Wend
  If x1<0 : ProcedureReturn x1+b0 : EndIf
  ProcedureReturn x1
EndProcedure

Procedure.i ChineseRem(n_Adr.i,a_Adr.i)
  Define prod.i=Produkt_n(n_Adr,a_Adr), a.i, b.i, p.i, Idx.i=0, sum.i
  While n_Adr+SizeOf(Integer)*Idx<a_Adr
    b=PeekI(n_Adr+SizeOf(Integer)*Idx)
    p=Int(prod/b) : a=p
    sum+PeekI(a_Adr+SizeOf(Integer)*Idx)*Eval_x1(a,b)*p
    Idx+1
  Wend
  ProcedureReturn sum%prod
EndProcedure

OnErrorCall(@ErrorHdl())
OpenConsole("Chinese remainder theorem")
PrintData(?LBL_n1,?LBL_a1)
PrintN(Str(ChineseRem(?LBL_n1,?LBL_a1)))
PrintData(?LBL_n2,?LBL_a2)
PrintN(Str(ChineseRem(?LBL_n2,?LBL_a2)))
PrintData(?LBL_n3,?LBL_a3)
PrintN(Str(ChineseRem(?LBL_n3,?LBL_a3)))
Input()
```

{{out}}

```txt
[( 2 . 3 )( 3 . 5 )( 2 . 7 )]
x = 23
[( 10 . 11 )( 4 . 12 )( 12 . 13 )]
x = 1000
[( 11 . 10 )( 22 . 4 )( 19 . 9 )]
x = Division by zero
```



## Python


### Procedural


### =Python 2.7=


```python
# Python 2.7
def chinese_remainder(n, a):
    sum = 0
    prod = reduce(lambda a, b: a*b, n)

    for n_i, a_i in zip(n, a):
        p = prod / n_i
        sum += a_i * mul_inv(p, n_i) * p
    return sum % prod


def mul_inv(a, b):
    b0 = b
    x0, x1 = 0, 1
    if b == 1: return 1
    while a > 1:
        q = a / b
        a, b = b, a%b
        x0, x1 = x1 - q * x0, x0
    if x1 < 0: x1 += b0
    return x1

if __name__ == '__main__':
    n = [3, 5, 7]
    a = [2, 3, 2]
    print chinese_remainder(n, a)
```

{{out}}

```txt
23
```



### =Python 3.6=


```python
# Python 3.6
from functools import reduce
def chinese_remainder(n, a):
    sum = 0
    prod = reduce(lambda a, b: a*b, n)
    for n_i, a_i in zip(n, a):
        p = prod // n_i
        sum += a_i * mul_inv(p, n_i) * p
    return sum % prod



def mul_inv(a, b):
    b0 = b
    x0, x1 = 0, 1
    if b == 1: return 1
    while a > 1:
        q = a // b
        a, b = b, a%b
        x0, x1 = x1 - q * x0, x0
    if x1 < 0: x1 += b0
    return x1



if __name__ == '__main__':
    n = [3, 5, 7]
    a = [2, 3, 2]
    print(chinese_remainder(n, a))
```

{{out}}

```txt
23
```



### Functional

Using an option type to represent the possibility that there may or may not be a  solution for any given pair of input lists.

(Note that the procedural versions above both fail with a '''ZeroDivisionError''' on inputs for which no solution is found).

{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Chinese remainder theorem'''

from operator import (add, mul)
from functools import reduce


# cnRemainder :: [Int] -> [Int] -> Either String Int
def cnRemainder(ms):
    '''Chinese remainder theorem.
       (moduli, residues) -> Either explanation or solution
    '''
    def go(ms, rs):
        mp = numericProduct(ms)
        cms = [(mp // x) for x in ms]

        def possibleSoln(invs):
            return Right(
                sum(map(
                    mul,
                    cms, map(mul, rs, invs)
                )) % mp
            )
        return bindLR(
            zipWithEither(modMultInv)(cms)(ms)
        )(possibleSoln)

    return lambda rs: go(ms, rs)


# modMultInv :: Int -> Int -> Either String Int
def modMultInv(a, b):
    '''Modular multiplicative inverse.'''
    x, y = eGcd(a, b)
    return Right(x) if 1 == (a * x + b * y) else (
        Left('no modular inverse for ' + str(a) + ' and ' + str(b))
    )


# egcd :: Int -> Int -> (Int, Int)
def eGcd(a, b):
    '''Extended greatest common divisor.'''
    def go(a, b):
        if 0 == b:
            return (1, 0)
        else:
            q, r = divmod(a, b)
            (s, t) = go(b, r)
            return (t, s - q * t)
    return go(a, b)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tests of soluble and insoluble cases.'''

    print(
        fTable(
            __doc__ + ':\n\n         (moduli, residues) -> ' + (
                'Either solution or explanation\n'
            )
        )(repr)(
            either(compose(quoted("'"))(curry(add)('No solution: ')))(
                compose(quoted(' '))(repr)
            )
        )(uncurry(cnRemainder))([
            ([10, 4, 12], [11, 12, 13]),
            ([11, 12, 13], [10, 4, 12]),
            ([10, 4, 9], [11, 22, 19]),
            ([3, 5, 7], [2, 3, 2]),
            ([2, 3, 2], [3, 5, 7])
        ])
    )


# GENERIC -------------------------------------------------

# Left :: a -> Either a b
def Left(x):
    '''Constructor for an empty Either (option type) value
       with an associated string.'''
    return {'type': 'Either', 'Right': None, 'Left': x}


# Right :: b -> Either a b
def Right(x):
    '''Constructor for a populated Either (option type) value'''
    return {'type': 'Either', 'Left': None, 'Right': x}
# any :: (a -> Bool) -> [a] -> Bool


def any_(p):
    '''True if p(x) holds for at least
       one item in xs.'''
    def go(xs):
        for x in xs:
            if p(x):
                return True
        return False
    return lambda xs: go(xs)


# bindLR (>>=) :: Either a -> (a -> Either b) -> Either b
def bindLR(m):
    '''Either monad injection operator.
       Two computations sequentially composed,
       with any value produced by the first
       passed as an argument to the second.'''
    return lambda mf: (
        mf(m.get('Right')) if None is m.get('Left') else m
    )


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# curry :: ((a, b) -> c) -> a -> b -> c
def curry(f):
    '''A curried function derived
       from an uncurried function.'''
    return lambda a: lambda b: f(a, b)


# either :: (a -> c) -> (b -> c) -> Either a b -> c
def either(fl):
    '''The application of fl to e if e is a Left value,
       or the application of fr to e if e is a Right value.'''
    return lambda fr: lambda e: fl(e['Left']) if (
        None is e['Right']
    ) else fr(e['Right'])


# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function ->
                 fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + (' -> ') + fxShow(f(x))
            for x in xs
        ])
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# numericProduct :: [Num] -> Num
def numericProduct(xs):
    '''The arithmetic product of all numbers in xs.'''
    return reduce(mul, xs, 1)


# partitionEithers :: [Either a b] -> ([a],[b])
def partitionEithers(lrs):
    '''A list of Either values partitioned into a tuple
       of two lists, with all Left elements extracted
       into the first list, and Right elements
       extracted into the second list.
    '''
    def go(a, x):
        ls, rs = a
        r = x.get('Right')
        return (ls + [x.get('Left')], rs) if None is r else (
            ls, rs + [r]
        )
    return reduce(go, lrs, ([], []))


# quoted :: Char -> String -> String
def quoted(c):
    '''A string flanked on both sides
       by a specified quote character.
    '''
    return lambda s: c + s + c


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    '''A function over a tuple,
       derived from a curried function.'''
    return lambda xy: f(xy[0])(xy[1])


# zipWithEither :: (a -> b -> Either String  c)
#            -> [a] -> [b] -> Either String [c]
def zipWithEither(f):
    '''Either a list of results if f succeeds with every pair
       in the zip of xs and ys, or an explanatory string
       if any application of f returns no result.
    '''
    def go(xs, ys):
        ls, rs = partitionEithers(map(f, xs, ys))
        return Left(ls[0]) if ls else Right(rs)
    return lambda xs: lambda ys: go(xs, ys)


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Chinese remainder theorem:

         (moduli, residues) -> Either solution or explanation

([10, 4, 12], [11, 12, 13]) -> 'No solution: no modular inverse for 48 and 10'
([11, 12, 13], [10, 4, 12]) ->  1000
 ([10, 4, 9], [11, 22, 19]) -> 'No solution: no modular inverse for 36 and 10'
     ([3, 5, 7], [2, 3, 2]) ->  23
     ([2, 3, 2], [3, 5, 7]) -> 'No solution: no modular inverse for 6 and 2'
```



## R

{{trans|C}}

```rsplus
mul_inv <- function(a, b)
{
  b0 <- b
  x0 <- 0L
  x1 <- 1L

  if (b == 1) return(1L)
  while(a > 1){
    q <- as.integer(a/b)

    t <- b
    b <- a %% b
    a <- t

    t <- x0
    x0 <- x1 - q*x0
    x1 <- t
  }

  if (x1 < 0) x1 <- x1 + b0
  return(x1)
}

chinese_remainder <- function(n, a)
{
  len <- length(n)

  prod <- 1L
  sum <- 0L

  for (i in 1:len) prod <- prod * n[i]

  for (i in 1:len){
    p <- as.integer(prod / n[i])
    sum <- sum + a[i] * mul_inv(p, n[i]) * p
  }

  return(sum %% prod)
}

n <- c(3L, 5L, 7L)
a <- c(2L, 3L, 2L)

chinese_remainder(n, a)
```

{{out}}

```txt
23
```





## Racket

This is more of a demonstration of the built-in function "solve-chinese", than
anything. A bit cheeky, I know... but if you've got a dog, why bark yourself?

Take a look in the "math/number-theory" package it's full of goodies!
URL removed -- I can't be doing the Dutch recaptchas I'm getting.

```racket
#lang racket
(require (only-in math/number-theory solve-chinese))
(define as '(2 3 2))
(define ns '(3 5 7))
(solve-chinese as ns)
```

{{out}}

```txt
23
```



## REXX


### algebraic


```rexx
/*REXX program demonstrates  Sun Tzu's  (or Sunzi's)  Chinese Remainder  Theorem.       */
parse arg Ns As .                                /*get optional arguments from the C.L. */
if Ns=='' | Ns==","  then Ns = '3,5,7'           /*Ns not specified?   Then use default.*/
if As=='' | As==","  then As = '2,3,2'           /*As  "      "          "   "      "   */
       say 'Ns: ' Ns
       say 'As: ' As;                   say
Ns=space(translate(Ns, , ','));  #=words(Ns)     /*elide any superfluous blanks from N's*/
As=space(translate(As, , ','));  _=words(As)     /*  "    "       "        "      "  A's*/
if #\==_   then do;  say  "size of number sets don't match.";   exit 131;    end
if #==0    then do;  say  "size of the  N  set isn't valid.";   exit 132;    end
if _==0    then do;  say  "size of the  A  set isn't valid.";   exit 133;    end
N=1                                              /*the product─to─be  for  prod(n.j).   */
      do j=1  for #                              /*process each number for  As  and Ns. */
      n.j=word(Ns,j);  N=N*n.j                   /*get an  N.j  and calculate product.  */
      a.j=word(As,j)                             /* "   "  A.j  from the  As  list.     */
      end   /*j*/

      do    x=1  for N                           /*use a simple algebraic method.       */
         do i=1  for #                           /*process each   N.i  and  A.i  number.*/
         if x//n.i\==a.i  then iterate x         /*is modulus correct for the number X ?*/
         end   /*i*/                             /* [↑]  limit solution to the product. */
      say 'found a solution with X='   x         /*display one possible solution.       */
      exit                                       /*stick a fork in it,  we're all done. */
      end      /*x*/

say 'no solution found.'                         /*oops, announce that solution ¬ found.*/
```

{{out|output|text=  when using the default inputs:}}

```txt

Ns:  3,5,7
As:  2,3,2

found a solution with X= 23

```



### congruences sets


```rexx
/*REXX program demonstrates  Sun Tzu's  (or Sunzi's)  Chinese Remainder  Theorem.       */
parse arg Ns As .                                /*get optional arguments from the C.L. */
if Ns=='' | Ns==","  then Ns = '3,5,7'           /*Ns not specified?   Then use default.*/
if As=='' | As==","  then As = '2,3,2'           /*As  "      "          "   "      "   */
       say 'Ns: ' Ns
       say 'As: ' As;                   say
Ns=space(translate(Ns, , ','));  #=words(Ns)     /*elide any superfluous blanks from N's*/
As=space(translate(As, , ','));  _=words(As)     /*  "    "       "        "      "  A's*/
if #\==_   then do;  say  "size of number sets don't match.";   exit 131;    end
if #==0    then do;  say  "size of the  N  set isn't valid.";   exit 132;    end
if _==0    then do;  say  "size of the  A  set isn't valid.";   exit 133;    end
N=1                                              /*the product─to─be  for  prod(n.j).   */
      do j=1  for #                              /*process each number for  As  and Ns. */
      n.j=word(Ns,j);  N=N*n.j                   /*get an  N.j  and calculate product.  */
      a.j=word(As,j)                             /* "   "  A.j  from the  As  list.     */
      end   /*j*/
@.=                                              /* [↓]  converts congruences ───► sets.*/
      do i=1  for #;  _=a.i;  @.i._=a.i;  p=a.i
        do N; p=p+n.i;  @.i.p=p;  end            /*build a (array) list of modulo values*/
      end   /*i*/
                                                 /* [↓]  find common number in the sets.*/
  do   x=1  for N;  if @.1.x==''  then iterate                       /*locate a number. */
    do v=2  to #;   if @.v.x==''  then iterate x;  end               /*Is in all sets ? */
  say 'found a solution with X='    x            /*display one possible solution.       */
  exit                                           /*stick a fork in it,  we're all done. */
  end   /*x*/

say 'no solution found.'                         /*oops, announce that solution ¬ found.*/
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




## Ruby

Brute-force.

```ruby

def chinese_remainder(mods, remainders)
  max = mods.inject( :* )
  series = remainders.zip( mods ).map{|r,m| r.step( max, m ).to_a }
  series.inject( :& ).first #returns nil when empty
end

p chinese_remainder([3,5,7], [2,3,2])     #=> 23
p chinese_remainder([10,4,9], [11,22,19]) #=> nil

```


Similar to above, but working with large(r) numbers.

```ruby

def extended_gcd(a, b)
  last_remainder, remainder = a.abs, b.abs
  x, last_x, y, last_y = 0, 1, 1, 0
  while remainder != 0
    last_remainder, (quotient, remainder) = remainder, last_remainder.divmod(remainder)
    x, last_x = last_x - quotient*x, x
    y, last_y = last_y - quotient*y, y
  end
  return last_remainder, last_x * (a < 0 ? -1 : 1)
end

def invmod(e, et)
  g, x = extended_gcd(e, et)
  if g != 1
    raise 'Multiplicative inverse modulo does not exist!'
  end
  x % et
end

def chinese_remainder(mods, remainders)
  max = mods.inject( :* )  # product of all moduli
  series = remainders.zip(mods).map{ |r,m| (r * max * invmod(max/m, m) / m) }
  series.inject( :+ ) % max
end

p chinese_remainder([3,5,7], [2,3,2])     #=> 23
p chinese_remainder([17353461355013928499, 3882485124428619605195281, 13563122655762143587], [7631415079307304117, 1248561880341424820456626, 2756437267211517231]) #=> 937307771161836294247413550632295202816
p chinese_remainder([10,4,9], [11,22,19]) #=> nil

```



## Rust


```rust
fn egcd(a: i64, b: i64) -> (i64, i64, i64) {
    if a == 0 {
        (b, 0, 1)
    } else {
        let (g, x, y) = egcd(b % a, a);
        (g, y - (b / a) * x, x)
    }
}

fn mod_inv(x: i64, n: i64) -> Option<i64> {
    let (g, x, _) = egcd(x, n);
    if g == 1 {
        Some((x % n + n) % n)
    } else {
        None
    }
}

fn chinese_remainder(residues: &[i64], modulii: &[i64]) -> Option<i64> {
    let prod = modulii.iter().product::<i64>();

    let mut sum = 0;

    for (&residue, &modulus) in residues.iter().zip(modulii) {
        let p = prod / modulus;
        sum += residue * mod_inv(p, modulus)? * p
    }

    Some(sum % prod)
}

fn main() {
    let modulii = [3,5,7];
    let residues = [2,3,2];

    match chinese_remainder(&residues, &modulii) {
        Some(sol) => println!("{}", sol),
        None      => println!("modulii not pairwise coprime")
    }

}
```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/9QZvFht/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/njcaS3BFT6GtaWT2cHiwXg Scastie (remote JVM)].

```Scala
import scala.util.{Success, Try}

object ChineseRemainderTheorem extends App {

  def chineseRemainder(n: List[Int], a: List[Int]): Option[Int] = {
    require(n.size == a.size)
    val prod = n.product

    def iter(n: List[Int], a: List[Int], sm: Int): Int = {
      def mulInv(a: Int, b: Int): Int = {
        def loop(a: Int, b: Int, x0: Int, x1: Int): Int = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }

  println(chineseRemainder(List(3, 5, 7), List(2, 3, 2)))
  println(chineseRemainder(List(11, 12, 13), List(10, 4, 12)))
  println(chineseRemainder(List(11, 22, 19), List(10, 4, 9)))

}
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const func integer: modInverse (in integer: a, in integer: b) is
  return ord(modInverse(bigInteger conv a, bigInteger conv b));

const proc: main is func
  local
    const array integer: n is [] (3, 5, 7);
    const array integer: a is [] (2, 3, 2);
    var integer: num is 0;
    var integer: prod is 1;
    var integer: sum is 0;
    var integer: index is 0;
  begin
    for num range n do
      prod *:= num;
    end for;
    for key index range a do
      num := prod div n[index];
      sum +:= a[index] * modInverse(num, n[index]) * num;
    end for;
    writeln(sum mod prod);
  end func;
```


{{out}}

```txt

23

```



## Sidef

{{trans|Perl 6}}

```ruby
func chinese_remainder(*n) {
    var N = n.prod
    func (*a) {
        n.range.sum { |i|
            var p = (N / n[i])
            a[i] * p.invmod(n[i]) * p
        } % N
    }
}

say chinese_remainder(3, 5, 7)(2, 3, 2)
```

{{out}}

```txt

23

```



## Swift


```swift
import Darwin

/*
 * Function: euclid
 * Usage: (r,s) = euclid(m,n)
 * --------------------------
 * The extended Euclidean algorithm subsequently performs
 * Euclidean divisions till the remainder is zero and then
 * returns the Bézout coefficients r and s.
 */

func euclid(_ m:Int, _ n:Int) -> (Int,Int) {
    if m % n == 0 {
        return (0,1)
    } else {
        let rs = euclid(n % m, m)
        let r = rs.1 - rs.0 * (n / m)
        let s = rs.0

        return (r,s)
    }
}

/*
 * Function: gcd
 * Usage: x = gcd(m,n)
 * -------------------
 * The greatest common divisor of two numbers a and b
 * is expressed by ax + by = gcd(a,b) where x and y are
 * the Bézout coefficients as determined by the extended
 * euclidean algorithm.
 */

func gcd(_ m:Int, _ n:Int) -> Int {
    let rs = euclid(m, n)
    return m * rs.0 + n * rs.1
}

/*
 * Function: coprime
 * Usage: truth = coprime(m,n)
 * ---------------------------
 * If two values are coprime, their greatest common
 * divisor is 1.
 */

func coprime(_ m:Int, _ n:Int) -> Bool {
    return gcd(m,n) == 1 ? true : false
}

coprime(14,26)
//coprime(2,4)

/*
 * Function: crt
 * Usage: x = crt(a,n)
 * -------------------
 * The Chinese Remainder Theorem supposes that given the
 * integers n_1...n_k that are pairwise co-prime, then for
 * any sequence of integers a_1...a_k there exists an integer
 * x that solves the system of linear congruences:
 *
 *   x === a_1 (mod n_1)
 *   ...
 *   x === a_k (mod n_k)
 */

func crt(_ a_i:[Int], _ n_i:[Int]) -> Int {
    // There is no identity operator for elements of [Int].
    // The offset of the elements of an enumerated sequence
    // can be used instead, to determine if two elements of the same
    // array are the same.
    let divs = n_i.enumerated()

    // Check if elements of n_i are pairwise coprime divs.filter{ $0.0 < n.0 }
    divs.forEach{
        n in divs.filter{ $0.0 < n.0 }.forEach{
            assert(coprime(n.1, $0.1))
        }
    }

    // Calculate factor N
    let N = n_i.map{$0}.reduce(1, *)

    // Euclidean algorithm determines s_i (and r_i)
    var s:[Int] = []

    // Using euclidean algorithm to calculate r_i, s_i
    n_i.forEach{ s += [euclid($0, N / $0).1] }

    // Solve for x
    var x = 0
    a_i.enumerated().forEach{
        x += $0.1 * s[$0.0] * N / n_i[$0.0]
    }

    // Return minimal solution
    return x % N
}

let a = [2,3,2]
let n = [3,5,7]

let x = crt(a,n)

print(x)
```


{{out}}

```txt
23
```



## Tcl

{{trans|C}}

```tcl
proc ::tcl::mathfunc::mulinv {a b} {
    if {$b == 1} {return 1}
    set b0 $b; set x0 0; set x1 1
    while {$a > 1} {
	set x0 [expr {$x1 - ($a / $b) * [set x1 $x0]}]
	set b [expr {$a % [set a $b]}]
    }
    incr x1 [expr {($x1 < 0) * $b0}]
}
proc chineseRemainder {nList aList} {
    set sum 0; set prod [::tcl::mathop::* {*}$nList]
    foreach n $nList a $aList {
	set p [expr {$prod / $n}]
	incr sum [expr {$a * mulinv($p, $n) * $p}]
    }
    expr {$sum % $prod}
}
puts [chineseRemainder {3 5 7} {2 3 2}]
```

{{out}}

```txt
23
```



## uBasic/4tH

{{trans|C}}
<lang>@(000) = 3 : @(001) = 5 : @(002) = 7
@(100) = 2 : @(101) = 3 : @(102) = 2

Print Func (_Chinese_Remainder (3))

' -------------------------------------

@(000) = 11 : @(001) = 12 : @(002) = 13
@(100) = 10 : @(101) = 04 : @(102) = 12

Print Func (_Chinese_Remainder (3))

' -------------------------------------

End

                                       ' returns x where (a * x) % b == 1
_Mul_Inv Param (2)                     ' ( a b -- n)
  Local (4)

  c@ = b@
  d@ = 0
  e@ = 1

  If b@ = 1 Then Return (1)

  Do While a@ > 1
     f@ = a@ / b@
     Push b@ : b@ = a@ % b@ : a@ = Pop()
     Push d@ : d@ = e@ - f@ * d@ : e@ = Pop()
  Loop

  If e@ < 0 Then e@ = e@ + c@

Return (e@)


_Chinese_Remainder Param (1)           ' ( len -- n)
  Local (5)

  b@ = 1
  c@ = 0

  For d@ = 0 Step 1 While d@ < a@
    b@ = b@ * @(d@)
  Next

  For d@ = 0 Step 1 While d@ < a@
    e@ = b@ / @(d@)
    c@ = c@ + (@(100 + d@) * Func (_Mul_Inv (e@, @(d@))) * e@)
  Next

Return (c@ % b@)

```

{{out}}

```txt

23
1000

0 OK, 0:1034

```



## VBA

Uses the function mul_inv() from [[Modular_inverse#VBA]]
{{trans|Phix}}
```vb
Private Function chinese_remainder(n As Variant, a As Variant) As Variant
    Dim p As Long, prod As Long, tot As Long
    prod = 1: tot = 0
    For i = 1 To UBound(n)
        prod = prod * n(i)
    Next i
    Dim m As Variant
    For i = 1 To UBound(n)
        p = prod / n(i)
        m = mul_inv(p, n(i))
        If WorksheetFunction.IsText(m) Then
            chinese_remainder = "fail"
            Exit Function
        End If
        tot = tot + a(i) * m * p
    Next i
    chinese_remainder = tot Mod prod
End Function
Public Sub re()
    Debug.Print chinese_remainder([{3,5,7}], [{2,3,2}])
    Debug.Print chinese_remainder([{11,12,13}], [{10,4,12}])
    Debug.Print chinese_remainder([{11,22,19}], [{10,4,9}])
    Debug.Print chinese_remainder([{100,23}], [{19,0}])
End Sub
```
{{out}}

```txt
 23
 1000
fail
 1219
```


## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Function ModularMultiplicativeInverse(a As Integer, m As Integer) As Integer
        Dim b = a Mod m
        For x = 1 To m - 1
            If (b * x) Mod m = 1 Then
                Return x
            End If
        Next
        Return 1
    End Function

    Function Solve(n As Integer(), a As Integer()) As Integer
        Dim prod = n.Aggregate(1, Function(i, j) i * j)
        Dim sm = 0
        Dim p As Integer
        For i = 0 To n.Length - 1
            p = prod / n(i)
            sm = sm + a(i) * ModularMultiplicativeInverse(p, n(i)) * p
        Next
        Return sm Mod prod
    End Function

    Sub Main()
        Dim n = {3, 5, 7}
        Dim a = {2, 3, 2}

        Dim result = Solve(n, a)

        Dim counter = 0
        Dim maxCount = n.Length - 1
        While counter <= maxCount
            Console.WriteLine($"{result} = {a(counter)} (mod {n(counter)})")
            counter = counter + 1
        End While
    End Sub

End Module
```

{{out}}

```txt
23 = 2 (mod 3)
23 = 3 (mod 5)
23 = 2 (mod 7)
```



## zkl

{{trans|Go}}
Using the GMP library, gcdExt is the Extended Euclidean algorithm.

```zkl
var BN=Import("zklBigNum"), one=BN(1);

fcn crt(xs,ys){
   p:=xs.reduce('*,BN(1));
   X:=BN(0);
   foreach x,y in (xs.zip(ys)){
      q:=p/x;
      z,s,_:=q.gcdExt(x);
      if(z!=one) throw(Exception.ValueError("%d not coprime".fmt(x)));
      X+=y*s*q;
   }
   return(X % p);
}
```


```zkl
println(crt(T(3,5,7),  T(2,3,2)));    //-->23
println(crt(T(11,12,13),T(10,4,12))); //-->1000
println(crt(T(11,22,19), T(10,4,9))); //-->ValueError: 11 not coprime
```



## ZX Spectrum Basic

{{trans|C}}

```zxbasic
10 DIM n(3): DIM a(3)
20 FOR i=1 TO 3
30 READ n(i),a(i)
40 NEXT i
50 DATA 3,2,5,3,7,2
100 LET prod=1: LET sum=0
110 FOR i=1 TO 3: LET prod=prod*n(i): NEXT i
120 FOR i=1 TO 3
130 LET p=INT (prod/n(i)): LET a=p: LET b=n(i)
140 GO SUB 1000
150 LET sum=sum+a(i)*x1*p
160 NEXT i
170 PRINT FN m(sum,prod)
180 STOP
200 DEF FN m(a,b)=a-INT (a/b)*b: REM Modulus function
1000 LET b0=b: LET x0=0: LET x1=1
1010 IF b=1 THEN RETURN
1020 IF a<=1 THEN GO TO 1100
1030 LET q=INT (a/b)
1040 LET t=b: LET b=FN m(a,b): LET a=t
1050 LET t=x0: LET x0=x1-q*x0: LET x1=t
1060 GO TO 1020
1100 IF x1<0 THEN LET x1=x1+b0
1110 RETURN
```


```txt
23
```

