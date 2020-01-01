+++
title = "Topswops"
description = ""
date = 2019-10-18T03:36:20Z
aliases = []
[extra]
id = 12627
[taxonomies]
categories = []
tags = []
+++

{{task}}

Topswops is a card game created by John Conway in the 1970's.


Assume you have a particular permutation of a set of   n   cards numbered   1..n   on both of their faces, for example the arrangement of four cards given by   [2, 4, 1, 3]   where the leftmost card is on top.

A round is composed of reversing the first   m   cards where   m   is the value of the topmost card.

Rounds are repeated until the topmost card is the number   1   and the number of swaps is recorded.


For our example the swaps produce:

```txt

    [2, 4, 1, 3]    # Initial shuffle
    [4, 2, 1, 3]
    [3, 1, 2, 4]
    [2, 1, 3, 4]
    [1, 2, 3, 4]

```


For a total of four swaps from the initial ordering to produce the terminating case where   1   is on top.


For a particular number   <code> n </code>   of cards,   <code> topswops(n) </code>   is the maximum swaps needed for any starting permutation of the   <code>n</code>   cards.


;Task:
The task is to generate and show here a table of   <code> n </code>   vs   <code> topswops(n) </code>   for   <code> n </code>   in the range   1..10   inclusive.


;Note:
[[oeis:A000375|Topswops]] is also known as   [http://www.haskell.org/haskellwiki/Shootout/Fannkuch Fannkuch]   from the German Pfannkuchen meaning   [http://youtu.be/3biN6nQYqZY pancake].


;Related tasks:
* [[Number reversal game]]
* [[Sorting algorithms/Pancake sort]]





## 360 Assembly

The program uses two ASSIST macro (XDECO,XPRNT) to keep the code as short as possible.

```360asm
*        Topswops optimized        12/07/2016
TOPSWOPS CSECT
         USING  TOPSWOPS,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
         MVC    N,=F'1'            n=1
LOOPN    L      R4,N               n; do n=1 to 10  ===-------------==*
         C      R4,=F'10'          "                                  *
         BH     ELOOPN             .                                  *
         MVC    P(40),PINIT        p=pinit
         MVC    COUNTM,=F'0'       countm=0
REPEAT   MVC    CARDS(40),P        cards=p  -------------------------+
         SR     R11,R11            count=0                           |
WHILE    CLC    CARDS,=F'1'        do while cards(1)^=1  ---------+
         BE     EWHILE             .                              |
         MVC    M,CARDS            m=cards(1)
         L      R2,M               m
         SRA    R2,1               m/2
         ST     R2,MD2             md2=m/2
         L      R3,M               @card(mm)=m
         SLA    R3,2               *4
         LA     R3,CARDS-4(R3)     @card(mm)
         LA     R2,CARDS           @card(i)=0
         LA     R6,1               i=1
LOOPI    C      R6,MD2             do i=1 to m/2  -------------+
         BH     ELOOPI             .                           |
         L      R0,0(R2)           swap r0=cards(i)
         MVC    0(4,R2),0(R3)      swap cards(i)=cards(mm)
         ST     R0,0(R3)           swap cards(mm)=r0
         AH     R2,=H'4'           @card(i)=@card(i)+4
         SH     R3,=H'4'           @card(mm)=@card(mm)-4
         LA     R6,1(R6)           i=i+1                       |
         B      LOOPI              ----------------------------+
ELOOPI   LA     R11,1(R11)         count=count+1                  |
         B      WHILE              -------------------------------+
EWHILE   C      R11,COUNTM         if count>countm
         BNH    NOTGT              then
         ST     R11,COUNTM           countm=count
NOTGT    BAL    R14,NEXTPERM       call nextperm
         LTR    R0,R0              until nextperm=0                 |
         BNZ    REPEAT             ---------------------------------+
         L      R1,N               n
         XDECO  R1,XDEC            edit n
         MVC    PG(2),XDEC+10      output n
         MVI    PG+2,C':'          output ':'
         L      R1,COUNTM          countm
         XDECO  R1,XDEC            edit countm
         MVC    PG+3(4),XDEC+8     output countm
         XPRNT  PG,L'PG            print buffer
         L      R1,N               n                                  *
         LA     R1,1(R1)           +1                                 *
         ST     R1,N               n=n+1                              *
         B      LOOPN              ===------------------------------==*
ELOOPN   L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
PINIT    DC     F'1',F'2',F'3',F'4',F'5',F'6',F'7',F'8',F'9',F'10'
CARDS    DS     10F                cards
P        DS     10F                p
COUNTM   DS     F                  countm
M        DS     F                  m
N        DS     F                  n
MD2      DS     F                  m/2
PG       DC     CL20' '            buffer
XDEC     DS     CL12               temp
*------- ----   nextperm ----------{-----------------------------------
NEXTPERM L      R9,N               nn=n
         SR     R8,R8              jj=0
         LR     R7,R9              nn
         BCTR   R7,0               j=nn-1
         LTR    R7,R7              if j=0
         BZ     ELOOPJ1            then skip do loop
LOOPJ1   LR     R1,R7              do j=nn-1 to 1 by -1; j ----+
         SLA    R1,2               .                           |
         L      R2,P-4(R1)         p(j)
         C      R2,P(R1)           if p(j)<p(j+1)
         BNL    PJGEPJP            then
         LR     R8,R7                jj=j
         B      ELOOPJ1              leave j                   |
PJGEPJP  BCT    R7,LOOPJ1          j=j-1  ---------------------+
ELOOPJ1  LA     R7,1(R8)           j=jj+1
LOOPJ2   CR     R7,R9              do j=jj+1 while j<nn  ------+
         BNL    ELOOPJ2            .                           |
         LR     R2,R7              j
         SLA    R2,2               .
         LR     R3,R9              nn
         SLA    R3,2               .
         L      R0,P-4(R2)         swap p(j),p(nn)
         L      R1,P-4(R3)         "
         ST     R0,P-4(R3)         "
         ST     R1,P-4(R2)         "
         BCTR   R9,0               nn=nn-1
         LA     R7,1(R7)           j=j+1                       |
         B      LOOPJ2             ----------------------------+
ELOOPJ2  LTR    R8,R8              if jj=0
         BNZ    JJNE0              then
         LA     R0,0                 return(0)
         BR     R14                  "
JJNE0    LA     R7,1(R8)           j=jj+1
         LR     R2,R7              j
         SLA    R2,2               r@p(j)
         LR     R3,R8              jj
         SLA    R3,2               r@p(jj)
LOOPJ3   L      R0,P-4(R2)         p(j)  ----------------------+
         C      R0,P-4(R3)         do j=jj+1 while p(j)<p(jj)  |
         BNL    ELOOPJ3
         LA     R2,4(R2)           r@p(j)=r@p(j)+4
         LA     R7,1(R7)           j=j+1                       |
         B      LOOPJ3             ----------------------------+
ELOOPJ3  L      R1,P-4(R3)         swap p(j),p(jj)
         ST     R0,P-4(R3)         "
         ST     R1,P-4(R2)         "
         LA     R0,1               return(1)
         BR     R14 ---------------}-----------------------------------
         YREGS
         END    TOPSWOPS
```

{{out}}

```txt

 1:   0
 2:   1
 3:   2
 4:   4
 5:   7
 6:  10
 7:  16
 8:  22
 9:  30
10:  38

```



## Ada

This is a straightforward approach that counts the number of swaps for each permutation. To generate all permutations over 1 .. N, for each of N in 1 .. 10, the package Generic_Perm from the Permutations task is used [[http://rosettacode.org/wiki/Permutations#The_generic_package_Generic_Perm]].


```Ada
with Ada.Integer_Text_IO, Generic_Perm;

procedure Topswaps is

   function Topswaps(Size: Positive) return Natural is
      package Perms is new Generic_Perm(Size);
      P: Perms.Permutation;
      Done: Boolean;
      Max: Natural;

      function Swapper_Calls(P: Perms.Permutation) return Natural is
	 Q: Perms.Permutation := P;
	 I: Perms.Element := P(1);
      begin
	 if I = 1 then
	    return 0;
	 else
	    for Idx in 1 .. I loop
	       Q(Idx) := P(I-Idx+1);
	    end loop;
	    return 1 + Swapper_Calls(Q);
	 end if;
      end Swapper_Calls;

   begin
      Perms.Set_To_First(P, Done);
      Max:= Swapper_Calls(P);
      while not Done loop
	 Perms.Go_To_Next(P, Done);
	 Max := natural'Max(Max, Swapper_Calls(P));
      end loop;
      return Max;
   end Topswaps;

begin
   for I in 1 .. 10 loop
      Ada.Integer_Text_IO.Put(Item => Topswaps(I), Width => 3);
   end loop;
end Topswaps;
```


{{out}}
```txt
  0  1  2  4  7 10 16 22 30 38
```



## AutoHotkey


```AutoHotkey
Topswops(Obj, n){
	R := []
	for i, val in obj{
		if (i <=n)
			res := val (A_Index=1?"":",") res
		else
			res .= "," val
	}
	Loop, Parse, res, `,
		R[A_Index]:= A_LoopField
	return R
}
```

Examples:
```AutoHotkey
Cards := [2, 4, 1, 3]
Res := Print(Cards)
while (Cards[1]<>1)
{
	Cards := Topswops(Cards, Cards[1])
	Res .= "`n"Print(Cards)
}
MsgBox % Res

Print(M){
	for i, val in M
			Res .= (A_Index=1?"":"`t") val
	return Trim(Res,"`n")
}
```

Outputs:
```txt
2	4	1	3
4	2	1	3
3	1	2	4
2	1	3	4
1	2	3	4
```



## C

An algorithm that doesn't go through all permutations, per Knuth tAoCP 7.2.1.2 exercise 107 (possible bad implementation on my part notwithstanding):

```c
#include <stdio.h>
#include <string.h>

typedef struct { char v[16]; } deck;
typedef unsigned int uint;

uint n, d, best[16];

void tryswaps(deck *a, uint f, uint s) {
#	define A a->v
#	define B b.v
	if (d > best[n]) best[n] = d;
	while (1) {
		if ((A[s] == s || (A[s] == -1 && !(f & 1U << s)))
			&& (d + best[s] >= best[n] || A[s] == -1))
			break;

		if (d + best[s] <= best[n]) return;
		if (!--s) return;
	}

	d++;
	deck b = *a;
	for (uint i = 1, k = 2; i <= s; k <<= 1, i++) {
		if (A[i] != i && (A[i] != -1 || (f & k)))
			continue;

		for (uint j = B[0] = i; j--;) B[i - j] = A[j];
		tryswaps(&b, f | k, s);
	}
	d--;
}

int main(void) {
	deck x;
	memset(&x, -1, sizeof(x));
	x.v[0] = 0;

	for (n = 1; n < 13; n++) {
		tryswaps(&x, 1, n - 1);
		printf("%2d: %d\n", n, best[n]);
	}

	return 0;
}
```

The code contains critical small loops, which can be manually unrolled for those with OCD.  POSIX thread support is useful if you got more than one CPUs.

```c
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <sched.h>

#define MAX_CPUS 8 // increase this if you got more CPUs/cores

typedef struct { char v[16]; } deck;

int n, best[16];

// Update a shared variable by spinlock.  Since this program really only
// enters locks dozens of times, a pthread_mutex_lock() would work
// equally fine, but RC already has plenty of examples for that.
#define SWAP_OR_RETRY(var, old, new)					\
	if (!__sync_bool_compare_and_swap(&(var), old, new)) {		\
		volatile int spin = 64;					\
		while (spin--);						\
		continue; }

void tryswaps(deck *a, int f, int s, int d) {
#define A a->v
#define B b->v

	while (best[n] < d) {
		int t = best[n];
		SWAP_OR_RETRY(best[n], t, d);
	}

#define TEST(x)									\
	case x: if ((A[15-x] == 15-x || (A[15-x] == -1 && !(f & 1<<(15-x))))	\
			&& (A[15-x] == -1 || d + best[15-x] >= best[n]))	\
			break;							\
		if (d + best[15-x] <= best[n]) return;				\
		s = 14 - x

	switch (15 - s) {
		TEST(0);  TEST(1);  TEST(2);  TEST(3);  TEST(4);
		TEST(5);  TEST(6);  TEST(7);  TEST(8);  TEST(9);
		TEST(10); TEST(11); TEST(12); TEST(13); TEST(14);
		return;
	}
#undef TEST

	deck *b = a + 1;
	*b = *a;
	d++;

#define FLIP(x)							\
	if (A[x] == x || ((A[x] == -1) && !(f & (1<<x)))) {	\
		B[0] = x;					\
		for (int j = x; j--; ) B[x-j] = A[j];		\
		tryswaps(b, f|(1<<x), s, d); }			\
	if (s == x) return;

	FLIP(1);  FLIP(2);  FLIP(3);  FLIP(4);  FLIP(5);
	FLIP(6);  FLIP(7);  FLIP(8);  FLIP(9);  FLIP(10);
	FLIP(11); FLIP(12); FLIP(13); FLIP(14); FLIP(15);
#undef FLIP
}

int num_cpus(void) {
	cpu_set_t ct;
	sched_getaffinity(0, sizeof(ct), &ct);

	int cnt = 0;
	for (int i = 0; i < MAX_CPUS; i++)
		if (CPU_ISSET(i, &ct))
			cnt++;

	return cnt;
}

struct work { int id; deck x[256]; } jobs[MAX_CPUS];
int first_swap;

void *thread_start(void *arg) {
	struct work *job = arg;
	while (1) {
		int at = first_swap;
		if (at >= n) return 0;

		SWAP_OR_RETRY(first_swap, at, at + 1);

		memset(job->x, -1, sizeof(deck));
		job->x[0].v[at] = 0;
		job->x[0].v[0] = at;
		tryswaps(job->x, 1 | (1 << at), n - 1, 1);
	}
}

int main(void) {
	int n_cpus = num_cpus();

	for (int i = 0; i < MAX_CPUS; i++)
		jobs[i].id = i;

	pthread_t tid[MAX_CPUS];

	for (n = 2; n <= 14; n++) {
		int top = n_cpus;
		if (top > n) top = n;

		first_swap = 1;
		for (int i = 0; i < top; i++)
			pthread_create(tid + i, 0, thread_start, jobs + i);

		for (int i = 0; i < top; i++)
			pthread_join(tid[i], 0);

		printf("%2d: %2d\n", n, best[n]);
	}

	return 0;
}
```



## C++


```cpp

#include <iostream>
#include <vector>
#include <numeric>
#include <algorithm>

int topswops(int n) {
  std::vector<int> list(n);
  std::iota(std::begin(list), std::end(list), 1);
  int max_steps = 0;
  do {
    auto temp_list = list;
    for (int steps = 1; temp_list[0] != 1; ++steps) {
      std::reverse(std::begin(temp_list), std::begin(temp_list) + temp_list[0]);
      if (steps > max_steps) max_steps = steps;
    }
  } while (std::next_permutation(std::begin(list), std::end(list)));
  return max_steps;
}

int main() {
  for (int i = 1; i <= 10; ++i) {
    std::cout << i << ": " << topswops(i) << std::endl;
  }
  return 0;
}
```


{{out}}

```txt
1: 0
2: 1
3: 2
4: 4
5: 7
6: 10
7: 16
8: 22
9: 30
10: 38
```



## D

Permutations generator from:
http://rosettacode.org/wiki/Permutations#Faster_Lazy_Version
{{trans|Haskell}}

```d
import std.stdio, std.algorithm, std.range, permutations2;

int topswops(in int n) pure @safe {
    static int flip(int[] xa) pure nothrow @safe @nogc {
        if (!xa[0]) return 0;
        xa[0 .. xa[0] + 1].reverse();
        return 1 + flip(xa);
    }
    return n.iota.array.permutations.map!flip.reduce!max;
}

void main() {
    foreach (immutable i; 1 .. 11)
        writeln(i, ": ", i.topswops);
}
```

{{out}}

```txt
1: 0
2: 1
3: 2
4: 4
5: 7
6: 10
7: 16
8: 22
9: 30
10: 38
```



### D: Faster Version

{{trans|C}}

```d
import std.stdio, std.typecons;

__gshared uint[32] best;

uint topswops(size_t n)() nothrow @nogc {
    static assert(n > 0 && n < best.length);
    size_t d = 0;

    alias T = byte;
    alias Deck = T[n];

    void trySwaps(in ref Deck deck, in uint f) nothrow @nogc {
        if (d > best[n])
            best[n] = d;

        foreach_reverse (immutable i; staticIota!(0, n)) {
            if ((deck[i] == i || (deck[i] == -1 && !(f & (1U << i))))
                && (d + best[i] >= best[n] || deck[i] == -1))
            break;
            if (d + best[i] <= best[n])
                return;
        }

        Deck deck2 = void;
        foreach (immutable i; staticIota!(0, n)) // Copy.
            deck2[i] = deck[i];

        d++;
        foreach (immutable i; staticIota!(1, n)) {
            enum uint k = 1U << i;
            if (deck[i] != i && (deck[i] != -1 || (f & k)))
                continue;

            deck2[0] = T(i);
            foreach_reverse (immutable j; staticIota!(0, i))
                deck2[i - j] = deck[j]; // Reverse copy.
            trySwaps(deck2, f | k);
        }
        d--;
    }

    best[n] = 0;
    Deck deck0 = -1;
    deck0[0] = 0;
    trySwaps(deck0, 1);
    return best[n];
}

void main() {
    foreach (immutable i; staticIota!(1, 14))
        writefln("%2d: %d", i, topswops!i());
}
```

{{out}}

```txt
 1: 0
 2: 1
 3: 2
 4: 4
 5: 7
 6: 10
 7: 16
 8: 22
 9: 30
10: 38
11: 51
12: 65
13: 80
```

With templates to speed up the computation, using the DMD compiler it's almost as fast as the second C version.


## Eiffel


```Eiffel

class
	TOPSWOPS

create
	make

feature

	make (n: INTEGER)
			-- Topswop game.
		local
			perm, ar: ARRAY [INTEGER]
			tcount, count: INTEGER
		do
			create perm_sol.make_empty
			create solution.make_empty
			across
				1 |..| n as c
			loop
				create ar.make_filled (0, 1, c.item)
				across
					1 |..| c.item as d
				loop
					ar [d.item] := d.item
				end
				permute (ar, 1)
				across
					1 |..| perm_sol.count as e
				loop
					tcount := 0
					from
					until
						perm_sol.at (e.item).at (1) = 1
					loop
						perm_sol.at (e.item) := reverse_array (perm_sol.at (e.item))
						tcount := tcount + 1
					end
					if tcount > count then
						count := tcount
					end
				end
				solution.force (count, c.item)
			end
		end

	solution: ARRAY [INTEGER]

feature {NONE}

	perm_sol: ARRAY [ARRAY [INTEGER]]

	reverse_array (ar: ARRAY [INTEGER]): ARRAY [INTEGER]
			-- Array with 'ar[1]' elements reversed.
		require
			ar_not_void: ar /= Void
		local
			i, j: INTEGER
		do
			create Result.make_empty
			Result.deep_copy (ar)
			from
				i := 1
				j := ar [1]
			until
				i > j
			loop
				Result [i] := ar [j]
				Result [j] := ar [i]
				i := i + 1
				j := j - 1
			end
		ensure
			same_elements: across ar as a all Result.has (a.item) end
		end

	permute (a: ARRAY [INTEGER]; k: INTEGER)
			-- All permutations of array 'a' stored in perm_sol.
		require
			ar_not_void: a.count >= 1
			k_valid_index: k > 0
		local
			i, t: INTEGER
			temp: ARRAY [INTEGER]
		do
			create temp.make_empty
			if k = a.count then
				across
					a as ar
				loop
					temp.force (ar.item, temp.count + 1)
				end
				perm_sol.force (temp, perm_sol.count + 1)
			else
				from
					i := k
				until
					i > a.count
				loop
					t := a [k]
					a [k] := a [i]
					a [i] := t
					permute (a, k + 1)
					t := a [k]
					a [k] := a [i]
					a [i] := t
					i := i + 1
				end
			end
		end

end

```

Test:

```Eiffel

class
	APPLICATION

create
	make

feature

	make
		do
			create topswop.make (10)
			across
				topswop.solution as t
			loop
				io.put_string (t.item.out + "%N")
			end
		end

	topswop: TOPSWOPS

end

```

{{out}}

```txt

0
1
2
4
7
10
16
22
30
38

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Topswops do
  def get_1_first( [1 | _t] ), do: 0
  def get_1_first( list ), do: 1 + get_1_first( swap(list) )

  defp swap( [n | _t]=list ) do
    {swaps, remains} = Enum.split( list, n )
    Enum.reverse( swaps, remains )
  end

  def task do
    IO.puts "N\ttopswaps"
    Enum.map(1..10, fn n -> {n, permute(Enum.to_list(1..n))} end)
    |> Enum.map(fn {n, n_permutations} -> {n, get_1_first_many(n_permutations)} end)
    |> Enum.map(fn {n, n_swops} -> {n, Enum.max(n_swops)} end)
    |> Enum.each(fn {n, max} -> IO.puts "#{n}\t#{max}" end)
  end

  def get_1_first_many( n_permutations ), do: (for x <- n_permutations, do: get_1_first(x))

  defp permute([]), do: [[]]
  defp permute(list), do: for x <- list, y <- permute(list -- [x]), do: [x|y]
end

Topswops.task
```


{{out}}

```txt

N       topswaps
1       0
2       1
3       2
4       4
5       7
6       10
7       16
8       22
9       30
10      38

```



## Erlang

This code is using the [[Permutations | permutation]] code by someone else. Thank you.

```Erlang

-module( topswops ).

-export( [get_1_first/1, swap/1, task/0] ).

get_1_first( [1 | _T] ) -> 0;
get_1_first( List ) -> 1 + get_1_first( swap(List) ).

swap( [N | _T]=List ) ->
	{Swaps, Remains} = lists:split( N, List ),
	lists:reverse( Swaps ) ++ Remains.

task() ->
	Permutations = [{X, permute:permute(lists:seq(1, X))} || X <- lists:seq(1, 10)],
	Swops = [{N, get_1_first_many(N_permutations)} || {N, N_permutations} <- Permutations],
	Topswops = [{N, lists:max(N_swops)} || {N, N_swops} <- Swops],
	io:fwrite( "N	topswaps~n" ),
	[io:fwrite("~p	~p~n", [N, Max]) || {N, Max} <- Topswops].



get_1_first_many( N_permutations ) -> [get_1_first(X) ||  X <- N_permutations].

```

{{out}}

```txt

42> topswops:task().
N       topswaps
1       0
2       1
3       2
4       4
5       7
6       10
7       16
8       22
9       30
10      38

```



## Factor


```factor
USING: formatting kernel math math.combinatorics math.order
math.ranges sequences ;
FROM: sequences.private => exchange-unsafe ;
IN: rosetta-code.topswops

! Reverse a subsequence in-place from 0 to n.
: head-reverse! ( seq n -- seq' )
    dupd [ 2/ ] [ ] bi rot
    [ [ over - 1 - ] dip exchange-unsafe ] 2curry each-integer ;

! Reverse the elements in seq according to the first element.
: swop ( seq -- seq' ) dup first head-reverse! ;

! Determine the number of swops until 1 is the head.
: #swops ( seq -- n )
    0 swap [ dup first 1 = ] [ [ 1 + ] [ swop ] bi* ] until
    drop ;

! Determine the maximum number of swops for a given length.
: topswops ( n -- max )
    [1,b] <permutations> [ #swops ] [ max ] map-reduce ;

: main ( -- )
    10 [1,b] [ dup topswops "%2d: %2d\n" printf ] each ;

MAIN: main
```

{{out}}

```txt

 1:  0
 2:  1
 3:  2
 4:  4
 5:  7
 6: 10
 7: 16
 8: 22
 9: 30
10: 38

```



## Fortran


```Fortran
module top
implicit none
contains
recursive function f(x) result(m)
  integer :: n, m, x(:),y(size(x)), fst
  fst = x(1)
  if (fst == 1) then
    m = 0
  else
    y(1:fst) = x(fst:1:-1)
    y(fst+1:) = x(fst+1:)
    m = 1 + f(y)
  end if
end function

recursive function perms(x) result(p)
integer, pointer     :: p(:,:), q(:,:)
integer              :: x(:), n, k, i
n = size(x)
if (n == 1) then
  allocate(p(1,1))
  p(1,:) = x
else
  q => perms(x(2:n))
  k = ubound(q,1)
  allocate(p(k*n,n))
  p = 0
  do i = 1,n
    p(1+k*(i-1):k*i,1:i-1) = q(:,1:i-1)
    p(1+k*(i-1):k*i,i) = x(1)
    p(1+k*(i-1):k*i,i+1:) = q(:,i:)
  end do
end if
end function
end module

program topswort
use top
implicit none
integer :: x(10)
integer, pointer  :: p(:,:)
integer :: i, j, m

forall(i=1:10)
  x(i) = i
end forall

do i = 1,10
  p=>perms(x(1:i))
  m = 0
  do j = 1, ubound(p,1)
    m = max(m, f(p(j,:)))
  end do
  print "(i3,a,i3)", i,": ",m
end do
end program

```



## Go


```go
// Adapted from http://www-cs-faculty.stanford.edu/~uno/programs/topswops.w
// at Donald Knuth's web site.  Algorithm credited there to Pepperdine
// and referenced to Mathematical Gazette 73 (1989), 131-133.
package main

import "fmt"

const ( // array sizes
    maxn = 10 // max number of cards
    maxl = 50 // upper bound for number of steps
)

func main() {
    for i := 1; i <= maxn; i++ {
        fmt.Printf("%d: %d\n", i, steps(i))
    }
}

func steps(n int) int {
    var a, b [maxl][maxn + 1]int
    var x [maxl]int
    a[0][0] = 1
    var m int
    for l := 0; ; {
        x[l]++
        k := int(x[l])
        if k >= n {
            if l <= 0 {
                break
            }
            l--
            continue
        }
        if a[l][k] == 0 {
            if b[l][k+1] != 0 {
                continue
            }
        } else if a[l][k] != k+1 {
            continue
        }
        a[l+1] = a[l]
        for j := 1; j <= k; j++ {
            a[l+1][j] = a[l][k-j]
        }
        b[l+1] = b[l]
        a[l+1][0] = k + 1
        b[l+1][k+1] = 1
        if l > m-1 {
            m = l + 1
        }
        l++
        x[l] = 0
    }
    return m
}
```

{{out}}

```txt

1: 0
2: 1
3: 2
4: 4
5: 7
6: 10
7: 16
8: 22
9: 30
10: 38

```



## Haskell


### =Searching permutations=


```Haskell
import Data.List (permutations)

topswops :: Int -> Int
topswops n = maximum $ map tops $ permutations [1 .. n]
    where
        tops    (1 : _) = 0
        tops xa@(x : _) = 1 + tops reordered
            where
                reordered = reverse (take x xa) ++ drop x xa

main = mapM_
        (\x -> putStrLn $ show x ++ ":\t" ++ show (topswops x))
        [1 .. 10]
```

{{out}}

```txt
1:	0
2:	1
3:	2
4:	4
5:	7
6:	10
7:	16
8:	22
9:	30
10:	38
```



### =Searching derangements=

'''Alternate version'''

Uses only permutations with all elements out of place.

```Haskell
import Data.List (permutations, inits)

import Control.Arrow (first)

derangements :: [Int] -> [[Int]]
derangements = (filter . (and .) . zipWith (/=)) <*> permutations

topswop :: Int -> [a] -> [a]
topswop = ((uncurry (++) . first reverse) .) . splitAt

topswopIter :: [Int] -> [[Int]]
topswopIter = takeWhile ((/= 1) . head) . iterate (topswop =<< head)

swops :: [Int] -> [Int]
swops = fmap (length . topswopIter) . derangements

topSwops :: [Int] -> [(Int, Int)]
topSwops = zip [1 ..] . fmap (maximum . (0 :) . swops) . tail . inits

main :: IO ()
main = mapM_ print $ take 10 $ topSwops [1 ..]
```

'''Output'''

```txt
*Main> mapM_ print $ take 10 $ topSwops [1..]
(1,0)
(2,1)
(3,2)
(4,4)
(5,7)
(6,10)
(7,16)
(8,22)
(9,30)
(10,38)
```


==Icon and {{header|Unicon}}==

This doesn't compile in Icon only because of the use of list comprehension to
build the original list of 1..n values.


```unicon
procedure main()
    every n := 1 to 10 do {
        ts := 0
        every (ts := 0) <:= swop(permute([: 1 to n :]))
        write(right(n, 3),": ",right(ts,4))
        }
end

procedure swop(A)
    count := 0
    while A[1] ~= 1 do {
        A := reverse(A[1+:A[1]]) ||| A[(A[1]+1):0]
        count +:= 1
        }
    return count
end

procedure permute(A)
    if *A <= 1 then return A
    suspend [(A[1]<->A[i := 1 to *A])] ||| permute(A[2:0])
end
```


Sample run:

```txt

->topswop
  1:    0
  2:    1
  3:    2
  4:    4
  5:    7
  6:   10
  7:   16
  8:   22
  9:   30
 10:   38
->

```



## J

'''Solution''':
```j
   swops =:  ((|.@:{. , }.)~ {.)^:a:
```

'''Example''' (''from task introduction''):
```j
   swops 2 4 1 3
2 4 1 3
4 2 1 3
3 1 2 4
2 1 3 4
1 2 3 4
```

'''Example''' (''topswops of all permutations of the integers 1..10''):
```j
   (,. _1 + ! >./@:(#@swops@A. >:)&i. ])&> 1+i.10
 1  0
 2  1
 3  2
 4  4
 5  7
 6 10
 7 16
 8 22
 9 30
10 38
```

'''Notes''': Readers less familiar with array-oriented programming may find [[Talk:Topswops#Alternate_J_solution|an alternate solution]] written in the structured programming style more accessible.


## Java

{{trans|D}}

```java
public class Topswops {
    static final int maxBest = 32;
    static int[] best;

    static private void trySwaps(int[] deck, int f, int d, int n) {
        if (d > best[n])
            best[n] = d;

        for (int i = n - 1; i >= 0; i--) {
            if (deck[i] == -1 || deck[i] == i)
                break;
            if (d + best[i] <= best[n])
                return;
        }

        int[] deck2 = deck.clone();
        for (int i = 1; i < n; i++) {
            final int k = 1 << i;
            if (deck2[i] == -1) {
                if ((f & k) != 0)
                    continue;
            } else if (deck2[i] != i)
                continue;

            deck2[0] = i;
            for (int j = i - 1; j >= 0; j--)
                deck2[i - j] = deck[j]; // Reverse copy.
            trySwaps(deck2, f | k, d + 1, n);
        }
    }

    static int topswops(int n) {
        assert(n > 0 && n < maxBest);
        best[n] = 0;
        int[] deck0 = new int[n + 1];
        for (int i = 1; i < n; i++)
            deck0[i] = -1;
        trySwaps(deck0, 1, 0, n);
        return best[n];
    }

    public static void main(String[] args) {
        best = new int[maxBest];
        for (int i = 1; i < 11; i++)
            System.out.println(i + ": " + topswops(i));
    }
}
```

{{out}}

```txt
1: 0
2: 1
3: 2
4: 4
5: 7
6: 10
7: 16
8: 22
9: 30
10: 38
```



## jq

The following uses permutations and is therefore impractical for n>10 or so.

'''Infrastructure:'''

```jq
# "while" as defined here is included in recent versions (>1.4) of jq:
def until(cond; next):
  def _until:
    if cond then . else (next|_until) end;
  _until;

# Generate a stream of permutations of [1, ... n].
# This implementation uses arity-0 filters for speed.
def permutations:
  # Given a single array, insert generates a stream by inserting (length+1) at different positions
  def insert: # state: [m, array]
     .[0] as $m | (1+(.[1]|length)) as $n
     | .[1]
     | if $m >= 0 then (.[0:$m] + [$n] + .[$m:]), ([$m-1, .] | insert) else empty end;

  if .==0 then []
  elif . == 1 then [1]
  else
    . as $n | ($n-1) | permutations | [$n-1, .] | insert
  end;
```

'''Topswops:'''

```jq
# Input: a permutation; output: an integer
def flips:
  # state: [i, array]
  [0, .]
  | until( .[1][0] == 1;
           .[1] as $p | $p[0] as $p0
	   | [.[0] + 1,  ($p[:$p0] | reverse) + $p[$p0:] ] )
  | .[0];

# input: n, the number of items
def fannkuch:
  reduce permutations as $p
    (0; [., ($p|flips) ] | max);
```


'''Example:'''

```jq
range(1; 11) | [., fannkuch ]
```

{{out}}

```sh
$ jq -n -c -f topswops.jq
[1,0]
[2,1]
[3,2]
[4,4]
[5,7]
[6,10]
[7,16]
[8,22]
[9,30]
[10,38]
```



## Julia

Fast, efficient version

```julia
function fannkuch(n)
	n == 1 && return 0
	n == 2 && return 1
	p = [1:n]
	q = copy(p)
	s = copy(p)
	sign = 1; maxflips = sum = 0
	while true
		q0 = p[1]
		if q0 != 1
			for i = 2:n
				q[i] = p[i]
			end
			flips = 1
			while true
				qq = q[q0] #??
				if qq == 1
					sum += sign*flips
					flips > maxflips && (maxflips = flips)
					break
				end
				q[q0] = q0
				if q0 >= 4
					i = 2; j = q0-1
					while true
						t = q[i]
						q[i] = q[j]
						q[j] = t
						i += 1
						j -= 1
						i >= j && break
					end
				end
				q0 = qq
				flips += 1
			end
		end
		#permute
		if sign == 1
			t = p[2]
			p[2] = p[1]
			p[1] = t
			sign = -1
		else
			t = p[2]
			p[2] = p[3]
			p[3] = t
			sign = 1
			for i = 3:n
				sx = s[i]
				if sx != 1
					s[i] = sx-1
					break
				end
				i == n && return maxflips
				s[i] = i
				t = p[1]
				for j = 1:i
					p[j] = p[j+1]
				end
				p[i+1] = t
			end
		end
	end
end
```

{{out}}

```txt
julia> function main()
for i = 1:10
	println(fannkuch(i))
end
end
# methods for generic function main
main() at none:2

julia> @time main()
0
1
2
4
7
10
16
22
30
38
elapsed time: 0.299617582 seconds
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

val best = IntArray(32)

fun trySwaps(deck: IntArray, f: Int, d: Int, n: Int) {
    if (d > best[n]) best[n] = d
    for (i in n - 1 downTo 0) {
        if (deck[i] == -1 || deck[i] == i) break
        if (d + best[i] <= best[n]) return
    }
    val deck2 = deck.copyOf()
    for (i in 1 until n) {
        val k = 1 shl i
        if (deck2[i] == -1) {
            if ((f and k) != 0) continue
        }
        else if (deck2[i] != i) continue
        deck2[0] = i
        for (j in i - 1 downTo 0) deck2[i - j] = deck[j]
        trySwaps(deck2, f or k, d + 1, n)
    }
}

fun topswops(n: Int): Int {
    require(n > 0 && n < best.size)
    best[n] = 0
    val deck0 = IntArray(n + 1)
    for (i in 1 until n) deck0[i] = -1
    trySwaps(deck0, 1, 0, n)
    return best[n]
}

fun main(args: Array<String>) {
    for (i in 1..10) println("${"%2d".format(i)} : ${topswops(i)}")
}
```


{{out}}

```txt

 1 : 0
 2 : 1
 3 : 2
 4 : 4
 5 : 7
 6 : 10
 7 : 16
 8 : 22
 9 : 30
10 : 38

```



## Lua


```Lua
-- Return an iterator to produce every permutation of list
function permute (list)
    local function perm (list, n)
        if n == 0 then coroutine.yield(list) end
        for i = 1, n do
            list[i], list[n] = list[n], list[i]
            perm(list, n - 1)
            list[i], list[n] = list[n], list[i]
        end
    end
    return coroutine.wrap(function() perm(list, #list) end)
end

-- Perform one topswop round on table t
function swap (t)
    local new, limit = {}, t[1]
    for i = 1, #t do
        if i <= limit then
            new[i] = t[limit - i + 1]
        else
            new[i] = t[i]
        end
    end
    return new
end

-- Find the most swaps needed for any starting permutation of n cards
function topswops (n)
    local numTab, highest, count = {}, 0
    for i = 1, n do numTab[i] = i end
    for numList in permute(numTab) do
        count = 0
        while numList[1] ~= 1 do
            numList = swap(numList)
            count = count + 1
        end
        if count > highest then highest = count end
    end
    return highest
end

-- Main procedure
for i = 1, 10 do print(i, topswops(i)) end
```

{{out}}

```txt
1       0
2       1
3       2
4       4
5       7
6       10
7       16
8       22
9       30
10      38
```



## Mathematica

An exhaustive search of all possible permutations is done

```Mathematica
flip[a_] :=
 Block[{a1 = First@a},
  If[a1 == Length@a, Reverse[a],
   Join[Reverse[a[[;; a1]]], a[[a1 + 1 ;;]]]]]

swaps[a_] := Length@FixedPointList[flip, a] - 2

Print[#, ": ", Max[swaps /@ Permutations[Range@#]]] & /@ Range[10];
```

{{out}}

```txt

1: 0
2: 1
3: 2
4: 4
5: 7
6: 10
7: 16
8: 22
9: 30
10: 38
```



## PARI/GP

Naive solution:

```parigp
flip(v:vec)={
  my(t=v[1]+1);
  if (t==2, return(0));
  for(i=1,t\2, [v[t-i],v[i]]=[v[i],v[t-i]]);
  1+flip(v)
}
topswops(n)={
  my(mx);
  for(i=0,n!-1,
    mx=max(flip(Vecsmall(numtoperm(n,i))),mx)
  );
  mx;
}
vector(10,n,topswops(n))
```

{{out}}

```txt
%1 = [0, 1, 2, 4, 7, 10, 16, 22, 30, 38]
```


An efficient solution would use PARI, following the [[#C|C]] solution.


## Perl

Recursive backtracking solution, starting with the final state and going backwards.

```perl

sub next_swop {
  my( $max, $level, $p, $d ) = @_;
  my $swopped = 0;
  for( 2..@$p ){ # find possibilities
    my @now = @$p;
    if( $_ == $now[$_-1] ) {
      splice @now, 0, 0, reverse splice @now, 0, $_;
      $swopped = 1;
      next_swop( $max, $level+1, \@now, [ @$d ] );
    }
  }
  for( 1..@$d ) { # create possibilities
    my @now = @$p;
    my $next = shift @$d;
    if( not $now[$next-1] ) {
      $now[$next-1] = $next;
      splice @now, 0, 0, reverse splice @now, 0, $next;
      $swopped = 1;
      next_swop( $max, $level+1, \@now, [ @$d ] );
    }
    push @$d, $next;
  }
  $$max = $level if !$swopped and $level > $$max;
}

sub topswops {
  my $n = shift;
  my @d = 2..$n;
  my @p = ( 1, (0) x ($n-1) );
  my $max = 0;
  next_swop( \$max, 0, \@p, \@d );
  return $max;
}

printf "Maximum swops for %2d cards: %2d\n", $_, topswops $_ for 1..10;

```


{{out}}

```txt

Maximum swops for  1 cards:  0
Maximum swops for  2 cards:  1
Maximum swops for  3 cards:  2
Maximum swops for  4 cards:  4
Maximum swops for  5 cards:  7
Maximum swops for  6 cards: 10
Maximum swops for  7 cards: 16
Maximum swops for  8 cards: 22
Maximum swops for  9 cards: 30
Maximum swops for 10 cards: 38

```



## Perl 6


```perl6
sub swops(@a is copy) {
    my int $count = 0;
    until @a[0] == 1 {
        @a[ ^@a[0] ] .= reverse;
        ++$count;
    }
    $count
}

sub topswops($n) { max (1..$n).permutations.race.map: *.&swops }

say "$_ {topswops $_}" for 1 .. 10;
```

{{Out}}

```txt
1 0
2 1
3 2
4 4
5 7
6 10
7 16
8 22
9 30
10 38
```



## Phix

Originally contributed by Jason Gade as part of the Euphoria version of the Great Computer Language Shootout benchmarks.

```Phix
function fannkuch(integer n)
sequence start = tagset(n),
         perm,
         perm1 = start,
         count = start
integer maxFlipsCount = 0, r = n+1
integer perm0, flipsCount, k, k2, j, j2

    while 1 do
        while r!=1 do count[r-1] = r r -= 1 end while
        if not (perm1[1]=1 or perm1[n]=n) then
            perm = perm1
            flipsCount = 0
            k = perm[1]
            while k!=1 do
                k2 = floor((k+1)/2)
                perm = reverse(perm[1..k]) & perm[k+1..n]
                flipsCount += 1
                k = perm[1]
            end while
            if flipsCount>maxFlipsCount then
                maxFlipsCount = flipsCount
            end if
        end if
        -- Use incremental change to generate another permutation
        while 1 do
            if r>n then return maxFlipsCount end if
            perm0 = perm1[1]
            j2 = 1
            while j2<r do
                j = j2+1
                perm1[j2] = perm1[j]
                j2 = j
            end while
            perm1[r] = perm0
            count[r] = count[r]-1
            if count[r]>1 then exit else r += 1 end if
        end while
    end while
end function -- fannkuch

for i=1 to 10 do
    ? fannkuch(i)
end for
```

{{out}}

```txt

0
1
2
4
7
10
16
22
30
38

```



## PicoLisp


```PicoLisp
(de fannkuch (N)
   (let (Lst (range 1 N)  L Lst  Max)
      (recur (L)  # Permute
         (if (cdr L)
            (do (length L)
               (recurse (cdr L))
               (rot L) )
            (zero N)  # For each permutation
            (for (P (copy Lst)  (> (car P) 1)  (flip P (car P)))
               (inc 'N) )
            (setq Max (max N Max)) ) )
      Max ) )

(for I 10
   (println I (fannkuch I)) )
```

Output:

```txt
1 0
2 1
3 2
4 4
5 7
6 10
7 16
8 22
9 30
10 38
```



## PL/I

{{incorrect|PL/I|Shown output is incorrect at the very least.}}

```PL/I

(subscriptrange):
topswap: procedure options (main); /* 12 November 2013 */
   declare cards(*) fixed (2) controlled, t fixed (2);
   declare dealt(*) bit(1) controlled;
   declare (count, i, m, n, c1, c2) fixed binary;
   declare random builtin;

   do n = 1 to 10;
      allocate cards(n), dealt(n);
      /* Take the n cards, in order ... */
      do i = 1 to n; cards(i) = i; end;
      /* ... and shuffle them. */
      do i = 1 to n;
         c1 = random*n+1; c2 = random*n+1;
         t = cards(c1); cards(c1) = cards(c2); cards(c2) = t;
      end;
      /* If '1' is the first card, game is trivial; swap it with another. */
      if cards(1) = 1 & n > 1 then
         do; t = cards(1); cards(1) = cards(2); cards(2) = t; end;

      count = 0;
      do until (cards(1) = 1);
         /* take the value of the first card, M, and reverse the first M cards. */
         m = cards(1);
         do i = 1 to m/2;
            t = cards(i); cards(i) = cards(m-i+1); cards(m-i+1) = t;
         end;
         count = count + 1;
      end;
      put skip edit (n, ':', count) (f(2), a, f(4));
   end;
end topswap;

```


```txt

 1:   1
 2:   1
 3:   2
 4:   2
 5:   4
 6:   2
 7:   1
 8:   9
 9:  16
10:   1

```



## Potion


```potion
range = (a, b):
  i = 0, l = list(b-a+1)
  while (a + i <= b):
    l (i) = a + i++.
  l.

fannkuch = (n):
  flips = 0, maxf = 0, k = 0, m = n - 1, r = n
  perml = range(0, n), count = list(n), perm = list(n)

  loop:
    while (r != 1):
      count (r-1) = r
      r--.

    if (perml (0) != 0 and perml (m) != m):
      flips = 0, i = 1
      while (i < n):
        perm (i) = perml (i)
        i++.
      k = perml (0)
      loop:
        i = 1, j = k - 1
        while (i < j):
          t = perm (i), perm (i) = perm (j), perm (j) = t
          i++, j--.
        flips++
        j = perm (k), perm (k) = k, k = j
        if (k == 0): break.
      .
      if (flips > maxf): maxf = flips.
    .

    loop:
      if (r == n):
        (n, maxf) say
        return (maxf).

      i = 0, j = perml (0)
      while (i < r):
        k = i + 1
        perml (i) = perml (k)
        i = k.
      perml (r) = j

      j = count (r) - 1
      count (r) = j
      if (j > 0): break.
      r++
_ n

n = argv(1) number
if (n<1): n=10.
fannkuch(n)

```


Output follows that of Perl6 and Python, ~2.5x faster than perl5


## Python

This solution uses cards numbered from 0..n-1 and variable p0 is introduced as a speed optimisation

```python>>>
 from itertools import permutations
>>> def f1(p):
	i = 0
	while True:
		p0  = p[0]
		if p0 == 1: break
		p[:p0] = p[:p0][::-1]
		i  += 1
	return i

>>> def fannkuch(n):
	return max(f1(list(p)) for p in permutations(range(1, n+1)))

>>> for n in range(1, 11): print(n,fannkuch(n))

1 0
2 1
3 2
4 4
5 7
6 10
7 16
8 22
9 30
10 38
>>>
```



### Python: Faster Version

{{trans|C}}

```python
try:
    import psyco
    psyco.full()
except ImportError:
    pass

best = [0] * 16

def try_swaps(deck, f, s, d, n):
    if d > best[n]:
        best[n] = d

    i = 0
    k = 1 << s
    while s:
        k >>= 1
        s -= 1
        if deck[s] == -1 or deck[s] == s:
            break
        i |= k
        if (i & f) == i and d + best[s] <= best[n]:
            return d
    s += 1

    deck2 = list(deck)
    k = 1
    for i2 in xrange(1, s):
        k <<= 1
        if deck2[i2] == -1:
            if f & k: continue
        elif deck2[i2] != i2:
            continue

        deck[i2] = i2
        deck2[:i2 + 1] = reversed(deck[:i2 + 1])
        try_swaps(deck2, f | k, s, 1 + d, n)

def topswops(n):
    best[n] = 0
    deck0 = [-1] * 16
    deck0[0] = 0
    try_swaps(deck0, 1, n, 0, n)
    return best[n]

for i in xrange(1, 13):
    print "%2d: %d" % (i, topswops(i))
```

{{out}}

```txt
 1: 0
 2: 1
 3: 2
 4: 4
 5: 7
 6: 10
 7: 16
 8: 22
 9: 30
10: 38
11: 51
12: 65
```



## R

Using iterpc package for optimization

```R

topswops <- function(x){
  i <- 0
  while(x[1] != 1){
    first <- x[1]
    if(first == length(x)){
      x <- rev(x)
    } else{
      x <- c(x[first:1], x[(first+1):length(x)])
    }
    i <- i + 1
  }
  return(i)
}

library(iterpc)

result <- NULL

for(i in 1:10){
  I <- iterpc(i, labels = 1:i, ordered = T)
  A <- getall(I)
  A <- data.frame(A)
  A$flips <- apply(A, 1, topswops)
  result <- rbind(result, c(i, max(A$flips)))
}

```


Output:

```txt

      [,1] [,2]
 [1,]    1    0
 [2,]    2    1
 [3,]    3    2
 [4,]    4    4
 [5,]    5    7
 [6,]    6   10
 [7,]    7   16
 [8,]    8   22
 [9,]    9   30
[10,]   10   38

```



## Racket

Simple search, only "optimization" is to consider only all-misplaced permutations (as in the alternative Haskell solution), which shaves off around 2 seconds (from ~5).


```Racket

#lang racket

(define (all-misplaced? l)
  (for/and ([x (in-list l)] [n (in-naturals 1)]) (not (= x n))))

(define (topswops n)
  (for/fold ([m 0]) ([p (in-permutations (range 1 (add1 n)))]
                     #:when (all-misplaced? p))
    (let loop ([p p] [n 0])
      (if (= 1 (car p))
        (max n m)
        (loop (let loop ([l '()] [r p] [n (car p)])
                (if (zero? n) (append l r)
                    (loop (cons (car r) l) (cdr r) (sub1 n))))
              (add1 n))))))

(for ([i (in-range 1 11)]) (printf "~a\t~a\n" i (topswops i)))

```


Output:

```txt

1	0
2	1
3	2
4	4
5	7
6	10
7	16
8	22
9	30
10	38

```



## REXX

The   '''decks'''   function is a modified permSets (permutation sets) subroutine,

and is optimized somewhat to take advantage by eliminating one-swop "decks".

```rexx
/*REXX program generates  N  decks of  numbered cards  and  finds the maximum  "swops". */
parse arg things .;          if things=='' then things=10

      do n=1  for things;          #=decks(n, n) /*create a (things) number of "decks". */
      mx= (n\==1)                                /*handle the case of a  one-card  deck.*/
                  do i=1  for #;   p=swops(!.i)  /*compute the SWOPS for this iteration.*/
                  if p>mx  then mx=p             /*This a new maximum?   Use a new max. */
                  end   /*i*/
      say '──────── maximum swops for a deck of'   right(n,2)   ' cards is'    right(mx,4)
      end   /*n*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
decks:  procedure expose !.; parse arg x,y,,$ @. /*   X  things  taken   Y   at a time. */
        #=0;                 call .decks 1       /* [↑]  initialize  $  &   @.  to null.*/
        return #                                 /*return number of permutations (decks)*/
.decks: procedure expose !. @. x y $ #;          parse arg ?
        if ?>y  then do;  _=@.1;  do j=2  for y-1;  _=_ @.j;  end  /*j*/;    #=#+1;  !.#=_
                     end
                else do;           qm=? - 1
                     if ?==1  then qs=2          /*don't use 1-swops that start with  1 */
                              else if @.1==?  then qs=2  /*skip the 1-swops: 3 x 1 x ···*/
                                              else qs=1
                       do q=qs  to x             /*build the permutations recursively.  */
                             do k=1  for qm;  if @.k==q  then iterate q
                             end  /*k*/
                       @.?=q;                 call .decks ? + 1
                       end        /*q*/
                     end
        return
/*──────────────────────────────────────────────────────────────────────────────────────*/

swops:  parse arg z;   do u=1;    parse var z t .;     if \datatype(t, 'W')  then t=x2d(t)
                       if word(z, t)==1  then return u            /*found unity at  T. */
                               do h=10  to things;     if pos(h, z)==0  then iterate
                               z=changestr(h, z, d2x(h) )         /* [↑]  any H's in Z?*/
                               end   /*h*/
                       z=reverse( subword(z, 1, t) )      subword(z, t + 1)
                       end   /*u*/
```

Some older REXXes don't have a '''changestr''' bif, so one is included here   ───►   [[CHANGESTR.REX]].

{{out|output|text=  when using the default input:}}

```txt

──────── maximum swops for a deck of  1  cards is    0
──────── maximum swops for a deck of  2  cards is    1
──────── maximum swops for a deck of  3  cards is    2
──────── maximum swops for a deck of  4  cards is    4
──────── maximum swops for a deck of  5  cards is    7
──────── maximum swops for a deck of  6  cards is   10
──────── maximum swops for a deck of  7  cards is   16
──────── maximum swops for a deck of  8  cards is   22
──────── maximum swops for a deck of  9  cards is   30
──────── maximum swops for a deck of 10  cards is   38

```



## Ruby

{{trans|Python}}

```ruby
def f1(a)
  i = 0
  while (a0 = a[0]) > 1
    a[0...a0] = a[0...a0].reverse
    i += 1
  end
  i
end

def fannkuch(n)
  [*1..n].permutation.map{|a| f1(a)}.max
end

for n in 1..10
  puts "%2d : %d" % [n, fannkuch(n)]
end
```


{{out}}

```txt

 1 : 0
 2 : 1
 3 : 2
 4 : 4
 5 : 7
 6 : 10
 7 : 16
 8 : 22
 9 : 30
10 : 38

```


'''Faster Version'''
{{trans|Java}}

```ruby
def try_swaps(deck, f, d, n)
  @best[n] = d  if d > @best[n]
  (n-1).downto(0) do |i|
    break  if deck[i] == -1 || deck[i] == i
    return if d + @best[i] <= @best[n]
  end
  deck2 = deck.dup
  for i in 1...n
    k = 1 << i
    if deck2[i] == -1
      next  if f & k != 0
    elsif deck2[i] != i
      next
    end
    deck2[0] = i
    deck2[1..i] = deck[0...i].reverse
    try_swaps(deck2, f | k, d+1, n)
  end
end

def topswops(n)
  @best[n] = 0
  deck0 = [-1] * (n + 1)
  try_swaps(deck0, 1, 0, n)
  @best[n]
end

@best = [0] * 16
for i in 1..10
  puts "%2d : %d" % [i, topswops(i)]
end
```



## Scala

{{libheader|Scala}}
```Scala
object Fannkuch extends App {

  def fannkuchen(l: List[Int], n: Int, i: Int, acc: Int): Int = {
    def flips(l: List[Int]): Int = (l: @unchecked) match {
      case 1 :: ls => 0
      case (n :: ls) =>
        val splitted = l.splitAt(n)
        flips(splitted._2.reverse_:::(splitted._1)) + 1
    }

    def rotateLeft(l: List[Int]) =
      l match {
        case Nil => List()
        case x :: xs => xs ::: List(x)
      }

    if (i >= n) acc
    else {
      if (n == 1) acc.max(flips(l))
      else {
        val split = l.splitAt(n)
        fannkuchen(rotateLeft(split._1) ::: split._2, n, i + 1, fannkuchen(l, n - 1, 0, acc))
      }
    }
  } // def fannkuchen(

  val result = (1 to 10).map(i => (i, fannkuchen(List.range(1, i + 1), i, 0, 0)))
  println("Computing results...")
  result.foreach(x => println(s"Pfannkuchen(${x._1})\t= ${x._2}"))
  assert(result == Vector((1, 0), (2, 1), (3, 2), (4, 4), (5, 7), (6, 10), (7, 16), (8, 22), (9, 30), (10, 38)), "Bad results")
  println(s"Successfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")
}
```
{{out}}
 Computing results...
 Pfannkuchen(1)	= 0
 Pfannkuchen(2)	= 1
 Pfannkuchen(3)	= 2
 Pfannkuchen(4)	= 4
 Pfannkuchen(5)	= 7
 Pfannkuchen(6)	= 10
 Pfannkuchen(7)	= 16
 Pfannkuchen(8)	= 22
 Pfannkuchen(9)	= 30
 Pfannkuchen(10)	= 38
 Successfully completed without errors. [total 7401 ms]

 Process finished with exit code 0


## Tcl

{{tcllib|struct::list}}<!-- Note that struct::list 1.8.1 (and probably earlier too) has a bug which makes this code hang when computing topswops(10). -->Probably an integer overflow at n=10.

```tcl
package require struct::list

proc swap {listVar} {
    upvar 1 $listVar list
    set n [lindex $list 0]
    for {set i 0; set j [expr {$n-1}]} {$i<$j} {incr i;incr j -1} {
	set tmp [lindex $list $i]
	lset list $i [lindex $list $j]
	lset list $j $tmp
    }
}

proc swaps {list} {
    for {set i 0} {[lindex $list 0] > 1} {incr i} {
	swap list
    }
    return $i
}

proc topswops list {
    set n 0
    ::struct::list foreachperm p $list {
	set n [expr {max($n,[swaps $p])}]
    }
    return $n
}

proc topswopsTo n {
    puts "n\ttopswops(n)"
    for {set i 1} {$i <= $n} {incr i} {
	puts $i\t[topswops [lappend list $i]]
    }
}
topswopsTo 10
```
{{out}}
 n	topswops(n)
 1	0
 2	1
 3	2
 4	4
 5	7
 6	10
 7	16
 8	22
 9	30
 10	38


## XPL0


```XPL0
code ChOut=8, CrLf=9, IntOut=11;
int  N, Max, Card1(16), Card2(16);

proc Topswop(D);        \Conway's card swopping game
int  D;                 \depth of recursion
int  I, J, C, T;
[if D # N then                  \generate N! permutations of 1..N in Card1
     [for I:= 0 to N-1 do
        [for J:= 0 to D-1 do    \check if object (letter) already used
            if Card1(J) = I+1 then J:=100;
        if J < 100 then
            [Card1(D):= I+1;    \card number not used so append it
            Topswop(D+1);       \recurse next level deeper
            ];
        ];
     ]
else [\determine number of topswops to get card 1 at beginning
     for I:= 0 to N-1 do Card2(I):= Card1(I);   \make working copy of deck
        C:= 0;                  \initialize swop counter
        while Card2(0) # 1 do
            [I:= 0;  J:= Card2(0)-1;
            while I < J do
                [T:= Card2(I);  Card2(I):= Card2(J);  Card2(J):= T;
                I:= I+1;  J:= J-1;
                ];
            C:= C+1;
            ];
     if C>Max then Max:= C;
     ];
];

[for N:= 1 to 10 do
    [Max:= 0;
    Topswop(0);
    IntOut(0, N);  ChOut(0, ^ );  IntOut(0, Max);  CrLf(0);
    ];
]
```


{{out}}

```txt

1 0
2 1
3 2
4 4
5 7
6 10
7 16
8 22
9 30
10 38

```



### XPL0: Faster Version

{{trans|C}}


```XPL0
code CrLf=9, IntOut=11, Text=12;
int  N, D, Best(16);

proc TrySwaps(A, F, S);
int  A, F, S;
int  B(16), I, J, K;
[if D > Best(N) then Best(N):= D;
loop    [if A(S)=-1 ! A(S)=S then quit;
        if D+Best(S) <= Best(N) then return;
        if S = 0 then quit;
        S:= S-1;
        ];
D:= D+1;
for I:= 0 to S do B(I):= A(I);
K:= 1;
for I:= 1 to S do
        [K:= K<<1;
        if B(I)=-1 & (F&K)=0 ! B(I)=I then
                [J:= I;  B(0):= J;
                while J do [J:= J-1;  B(I-J):= A(J)];
                TrySwaps(B, F!K, S);
                ];
        ];
D:= D-1;
];

int  I, X(16);
[for I:= 0 to 16-1 do
        [X(I):= -1;  Best(I):= 0];
X(0):= 0;
for N:= 1 to 13 do
        [D:= 0;
        TrySwaps(X, 1, N-1);
        IntOut(0, N);  Text(0, ": ");  IntOut(0, Best(N));  CrLf(0);
        ];
]
```


{{out}}

```txt

1: 0
2: 1
3: 2
4: 4
5: 7
6: 10
7: 16
8: 22
9: 30
10: 38
11: 51
12: 65
13: 80

```



## zkl

{{trans|D}}
Slow version

```zkl
fcn topswops(n){
   flip:=fcn(xa){
      if (not xa[0]) return(0);
      xa.reverse(0,xa[0]+1);  // inplace, ~4x faster than making new lists
      return(1 + self.fcn(xa));
   };
   (0).pump(n,List):Utils.Helpers.permute(_).pump(List,"copy",flip).reduce("max");
}

foreach n in ([1 .. 10]){ println(n, ": ", topswops(n)) }
```

{{out}}

```txt

1: 0
2: 1
3: 2
4: 4
5: 7
6: 10
7: 16
8: 22
9: 30
10: 38

```

