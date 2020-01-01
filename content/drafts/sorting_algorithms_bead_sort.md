+++
title = "Sorting algorithms/Bead sort"
description = ""
date = 2019-06-14T10:00:24Z
aliases = []
[extra]
id = 8099
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}
{{Sorting Algorithm}}

;Task:
Sort an array of positive integers using the [[wp:Bead_sort|Bead Sort Algorithm]].

A   ''bead sort''   is also known as a   ''gravity sort''.


Algorithm has   O(S),   where   S   is the sum of the integers in the input set:   Each bead is moved individually.

This is the case when bead sort is implemented without a mechanism to assist in finding empty spaces below the beads, such as in software implementations.





## 360 Assembly

{{trans|ooRexx}}
For maximum compatibility, this program uses only the basic instruction set (S/360)
and two ASSIST macros (XDECO,XPRNT) to keep it as short as possible.

```360asm
*        Bead Sort                 11/05/2016
BEADSORT CSECT
         USING  BEADSORT,R13       base register
SAVEAR   B      STM-SAVEAR(R15)    skip savearea
         DC     17F'0'             savearea
STM      STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         LA     R6,1               i=1
LOOPI1   CH     R6,=AL2(N)         do i=1 to hbound(z)
         BH     ELOOPI1            leave i
         LR     R1,R6                i
         SLA    R1,1                 <<1
         LH     R2,Z-2(R1)           z(i)
         CH     R2,LO                if z(i)<lo
         BNL    EIHO                 then
         STH    R2,LO                  lo=z(i)
EIHLO    CH     R2,HI                if z(i)>hi
         BNH    EIHHI                then
         STH    R2,HI                  hi=z(i)
EIHHI    LA     R6,1(R6)           iterate i
         B      LOOPI1             next i
ELOOPI1  LA     R9,1               1
         SH     R9,LO              -lo+1
         LA     R6,1               i=1
LOOPI2   CH     R6,=AL2(N)         do i=1 to hbound(z)
         BH     ELOOPI2            leave i
         LR     R1,R6                i
         SLA    R1,1                 <<1
         LH     R3,Z-2(R1)           z(i)
         AR     R3,R9                z(i)+o
         IC     R2,BEADS-1(R3)       beads(l)
         LA     R2,1(R2)             beads(l)+1
         STC    R2,BEADS-1(R3)       beads(l)=beads(l)+1
         LA     R6,1(R6)           iterate i
         B      LOOPI2             next i
ELOOPI2  SR     R8,R8              k=0
         LH     R6,LO              i=lo
LOOPI3   CH     R6,HI              do i=lo to hi
         BH     ELOOPI3            leave i
         LA     R7,1                 j=1
         SR     R10,R10              clear r10
         LR     R1,R6                i
         AR     R1,R9                i+o
         IC     R10,BEADS-1(R1)      beads(i+o)
LOOPJ3   CR     R7,R10               do j=1 to beads(i+o)
         BH     ELOOPJ3              leave j
         LA     R8,1(R8)               k=k+1
         LR     R1,R8                  k
         SLA    R1,1                   <<1
         STH    R6,S-2(R1)             s(k)=i
         LA     R7,1(R7)             iterate j
         B      LOOPJ3               next j
ELOOPJ3  AH     R6,=H'1'           iterate i
         B      LOOPI3             next i
ELOOPI3  LA     R7,1               j=1
LOOPJ4   CH     R7,=H'2'           do j=1 to 2
         BH     ELOOPJ4            leave j
         CH     R7,=H'1'             if j<>1
         BE     ONE                  then
         MVC    PG(7),=C'sorted:'      zap
ONE      LA     R10,PG+7             pgi=@pg+7
         LA     R6,1                 i=1
LOOPI4   CH     R6,=AL2(N)           do i=1 to hbound(z)
         BH     ELOOPI4              leave i
         CH     R7,=H'1'               if j=1
         BNE    TWO                    then
         LR     R1,R6                    i
         SLA    R1,1                     <<1
         LH     R11,Z-2(R1)              zs=z(i)
         B      XDECO                  else
TWO      LR     R1,R6                    i
         SLA    R1,1                     <<1
         LH     R11,S-2(R1)              zs=s(i)
XDECO    XDECO  R11,XDEC               edit zs
         MVC    0(6,R10),XDEC+6        output zs
         LA     R10,6(R10)             pgi=pgi+6
         LA     R6,1(R6)             iterate i
         B      LOOPI4               next i
ELOOPI4  XPRNT  PG,80                print buffer
         LA     R7,1(R7)             iterate j
         B      LOOPJ4             next j
ELOOPJ4  L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                "
         LTORG                     literal table
N        EQU    (S-Z)/2            number of items
Z        DC     H'5',H'3',H'1',H'7',H'-1',H'4',H'9',H'-12'
         DC     H'2001',H'-2010',H'17',H'0'
S        DS     (N)H               s same size as z
LO       DC     H'32767'           2**31-1
HI       DC     H'-32768'          -2**31
PG       DC     CL80'   raw:'      buffer
XDEC     DS     CL12               temp
BEADS    DC     4096X'00'          beads
         YREGS
         END    BEADSORT
```

{{out}}

```txt

   raw:     5     3     1     7    -1     4     9   -12  2001 -2010    17     0
sorted: -2010   -12    -1     0     1     3     4     5     7     9    17  2001

```



## AutoHotkey


```AutoHotkey
BeadSort(data){
	Pole:=[]	, TempObj:=[], Result:=[]
	for, i, v in data {
		Row := i
		loop, % v
			MaxPole := MaxPole>A_Index?MaxPole:A_Index	, Pole[A_Index, row] := 1
	}

	for i , obj in Pole {
		TempVar:=0	,	c := A_Index
		for n, v in obj
			TempVar += v
		loop, % TempVar
			TempObj[c, A_Index] := 1
	}

	loop, % Row {
		TempVar:=0	,	c := A_Index
		Loop, % MaxPole
			TempVar += TempObj[A_Index,c]
		Result[c] := TempVar
	}
	return Result
}
```

Examples:
```AutoHotkey
for i, val in BeadSort([54,12,87,56,36])
	res := val (res?",":"") res
MsgBox % res
```

{{out}}

```txt
12,36,54,56,87
```



## C

A rather straightforward implementation; since we do not use dynamic matrix, we have to know the maximum value in the array in advance.
Requires (max * length) bytes for beads; if memory is of concern, bytes can be replaced by bits.


```c
#include <stdio.h>
#include <stdlib.h>

void bead_sort(int *a, int len)
{
	int i, j, max, sum;
	unsigned char *beads;
#	define BEAD(i, j) beads[i * max + j]

	for (i = 1, max = a[0]; i < len; i++)
		if (a[i] > max) max = a[i];

	beads = calloc(1, max * len);

	/* mark the beads */
	for (i = 0; i < len; i++)
		for (j = 0; j < a[i]; j++)
			BEAD(i, j) = 1;

	for (j = 0; j < max; j++) {
		/* count how many beads are on each post */
		for (sum = i = 0; i < len; i++) {
			sum += BEAD(i, j);
			BEAD(i, j) = 0;
		}
		/* mark bottom sum beads */
		for (i = len - sum; i < len; i++) BEAD(i, j) = 1;
	}

	for (i = 0; i < len; i++) {
		for (j = 0; j < max && BEAD(i, j); j++);
		a[i] = j;
	}
	free(beads);
}

int main()
{
	int i, x[] = {5, 3, 1, 7, 4, 1, 1, 20};
	int len = sizeof(x)/sizeof(x[0]);

	bead_sort(x, len);
	for (i = 0; i < len; i++)
		printf("%d\n", x[i]);

	return 0;
}
```



## C++


```cpp
//this algorithm only works with positive, whole numbers.
//O(2n) time complexity where n is the summation of the whole list to be sorted.
//O(3n) space complexity.

#include <iostream>
#include <vector>

using std::cout;
using std::vector;

void distribute(int dist, vector<int> &List) {
	//*beads* go down into different buckets using gravity (addition).
    if (dist > List.size() )
        List.resize(dist); //resize if too big for current vector

    for (int i=0; i < dist; i++)
        List[i]++;
}

vector<int> beadSort(int *myints, int n) {
    vector<int> list, list2, fifth (myints, myints + n);

    cout << "#1 Beads falling down: ";
    for (int i=0; i < fifth.size(); i++)
        distribute (fifth[i], list);
    cout << '\n';

    cout << "\nBeads on their sides: ";
    for (int i=0; i < list.size(); i++)
        cout << " " << list[i];
    cout << '\n';

    //second part

    cout << "#2 Beads right side up: ";
    for (int i=0; i < list.size(); i++)
        distribute (list[i], list2);
    cout << '\n';

    return list2;
}

int main() {
    int myints[] = {734,3,1,24,324,324,32,432,42,3,4,1,1};
	vector<int> sorted = beadSort(myints, sizeof(myints)/sizeof(int));
	cout << "Sorted list/array: ";
	for(unsigned int i=0; i<sorted.size(); i++)
		cout << sorted[i] << ' ';
}
```



## Clojure

{{trans|Haskell}}

```Clojure
(defn transpose [xs]
  (loop [ret [], remain xs]
    (if (empty? remain)
      ret
      (recur (conj ret (map first remain))
             (filter not-empty (map rest remain))))))

(defn bead-sort [xs]
  (->> xs
       (map #(repeat % 1))
       transpose
       transpose
       (map #(reduce + %))))

;; This algorithm does not work if collection has zero
(-> [5 2 4 1 3 3 9] bead-sort println)

```


{{out}}

```txt
(9 5 4 3 3 2 1)
```



## COBOL

{{works with|GnuCOBOL}}

```COBOL>        >
SOURCE FORMAT FREE
*> This code is dedicated to the public domain
*> This is GNUCOBOL 2.0
identification division.
program-id. beadsort.
environment division.
configuration section.
repository. function all intrinsic.
data division.
working-storage section.
01  filler.
    03  row occurs 9 pic x(9).
    03  r pic 99.
    03  r1 pic 99.
    03  r2 pic 99.
    03  pole pic 99.
    03  a-lim pic 99 value 9.
    03  a pic 99.
    03  array occurs 9 pic 9.
01  NL pic x value x'0A'.
procedure division.
start-beadsort.

    *> fill the array
    compute a = random(seconds-past-midnight)
    perform varying a from 1 by 1 until a > a-lim
        compute array(a) = random() * 10
    end-perform

    perform display-array
    display space 'initial array'

    *> distribute the beads
    perform varying r from 1 by 1 until r > a-lim
        move all '.' to row(r)
        perform varying pole from 1 by 1 until pole > array(r)
            move 'o' to row(r)(pole:1)
        end-perform
    end-perform
    display NL 'initial beads'
    perform display-beads

    *> drop the beads
    perform varying pole from 1 by 1 until pole > a-lim
        move a-lim to r2
        perform find-opening
        compute r1 = r2 - 1
        perform find-bead
        perform until r1 = 0 *> no bead or no opening
            *> drop the bead
            move '.' to row(r1)(pole:1)
            move 'o' to row(r2)(pole:1)
            *> continue up the pole
            compute r2 = r2 - 1
            perform find-opening
            compute r1 = r2 - 1
            perform find-bead
        end-perform
    end-perform
    display NL 'dropped beads'
    perform display-beads

    *> count the beads in each row
    perform varying r from 1 by 1 until r > a-lim
        move 0 to array(r)
        inspect row(r) tallying array(r)
            for all 'o' before initial '.'
    end-perform

    perform display-array
    display space 'sorted array'

    stop run
    .
find-opening.
    perform varying r2 from r2 by -1
    until r2 = 1 or row(r2)(pole:1) = '.'
        continue
    end-perform
    .
find-bead.
    perform varying r1 from r1 by -1
    until r1 = 0 or row(r1)(pole:1) = 'o'
        continue
    end-perform
    .
display-array.
    display space
    perform varying a from 1 by 1 until a > a-lim
        display space array(a) with no advancing
    end-perform
    .
display-beads.
    perform varying r from 1 by 1 until r > a-lim
        display row(r)
    end-perform
    .
end program beadsort.
```


{{out}}

```txt
prompt$ cobc -xj beadsort.cob

 3 2 1 6 1 6 4 9 7 initial array

initial beads
ooo......
oo.......
o........
oooooo...
o........
oooooo...
oooo.....
ooooooooo
ooooooo..

dropped beads
o........
o........
oo.......
ooo......
oooo.....
oooooo...
oooooo...
ooooooo..
ooooooooo

 1 1 2 3 4 6 6 7 9 sorted array
```



## Common Lisp

{{trans|Clojure}}

```lisp

(defun transpose (remain &optional (ret '()))
  (if (null remain)
    ret
    (transpose (remove-if #'null (mapcar #'cdr remain))
               (append ret (list (mapcar #'car remain))))))

(defun bead-sort (xs)
  (mapcar #'length (transpose (transpose (mapcar (lambda (x) (make-list x :initial-element 1)) xs)))))

(bead-sort '(5 2 4 1 3 3 9))

```

{{out}}

```txt
(9 5 4 3 3 2 1)
```



## Delphi

{{trans|C}}

```d
program BeadSortTest;

{$APPTYPE CONSOLE}

uses
  SysUtils;

procedure BeadSort(var a : array of integer);
var
  i, j, max, sum : integer;
  beads : array of array of integer;
begin
  max := a[Low(a)];
  for i := Low(a) + 1 to High(a) do
    if a[i] > max then
      max := a[i];

  SetLength(beads, High(a) - Low(a) + 1, max);

  // mark the beads

  for i := Low(a) to High(a) do
    for j := 0 to a[i] - 1 do
      beads[i, j] := 1;

  for j := 0 to max - 1 do
  begin
    // count how many beads are on each post
    sum := 0;
    for i := Low(a) to High(a) do
    begin
      sum := sum + beads[i, j];
      beads[i, j] := 0;
    end;
    //mark bottom sum beads
    for i := High(a) + 1 - sum to High(a) do
      beads[i, j] := 1;
  end;

  for i := Low(a) to High(a) do
  begin
    j := 0;
    while (j < max) and (beads[i, j] <> 0) do
      inc(j);
    a[i] := j;
  end;

  SetLength(beads, 0, 0);
end;

const
  N = 8;
var
  i : integer;
  x : array[1..N] of integer = (5, 3, 1, 7, 4, 1, 1, 20);
begin
  for i := 1 to N do
    writeln(Format('x[%d] = %d', [i, x[i]]));

  BeadSort(x);

  for i := 1 to N do
    writeln(Format('x[%d] = %d', [i, x[i]]));

  readln;
end.
```

--[[User:Davidizadar|DavidIzadaR]] 18:12, 7 August 2011 (UTC)


## D

A functional-style solution.

```d
import std.stdio, std.algorithm, std.range, std.array, std.functional;

alias repeat0 = curry!(repeat, 0);

// Currenty std.range.transposed doesn't work.
auto columns(R)(R m) pure /*nothrow*/ @safe /*@nogc*/ {
    return m
           .map!walkLength
           .reduce!max
           .iota
           .map!(i => m.filter!(s => s.length > i).walkLength.repeat0);
}

auto beadSort(in uint[] data) pure /*nothrow @nogc*/ {
    return data.map!repeat0.columns.columns.map!walkLength;
}

void main() {
    [5, 3, 1, 7, 4, 1, 1].beadSort.writeln;
}
```

{{out}}

```txt
[7, 5, 4, 3, 1, 1, 1]
```



## Eiffel


```Eiffel

class
	BEAD_SORT

feature

	bead_sort (ar: ARRAY [INTEGER]): ARRAY [INTEGER]
			-- Sorted array in descending order.
		require
			only_positive_integers: across ar as a all a.item > 0 end
		local
			max, count, i, j, k: INTEGER
		do
			max := max_item (ar)
			create Result.make_filled (0, 1, ar.count)
			from
				i := 1
			until
				i > max
			loop
				count := 0
				from
					k := 1
				until
					k > ar.count
				loop
					if ar.item (k) >= i then
						count := count + 1
					end
					k := k + 1
				end
				from
					j := 1
				until
					j > count
				loop
					Result [j] := i
					j := j + 1
				end
				i := i + 1
			end
		ensure
			array_is_sorted: is_sorted (Result)
		end

feature {NONE}

	max_item (ar: ARRAY [INTEGER]): INTEGER
			-- Max item of 'ar'.
		require
			ar_not_void: ar /= Void
		do
			across
				ar as a
			loop
				if a.item > Result then
					Result := a.item
				end
			end
		ensure
			Result_is_max: across ar as a all a.item <= Result end
		end

	is_sorted (ar: ARRAY [INTEGER]): BOOLEAN
			--- Is 'ar' sorted in descending order?
		require
			ar_not_empty: ar.is_empty = False
		local
			i: INTEGER
		do
			Result := True
			from
				i := ar.lower
			until
				i = ar.upper
			loop
				if ar [i] < ar [i + 1] then
					Result := False
				end
				i := i + 1
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
			test := <<1, 5, 99, 2, 95, 7, 7>>
			create beadsort
			io.put_string ("unsorted:" + "%N")
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
			io.put_string ("%N" + "sorted:" + "%N")
			test := beadsort.bead_sort (test)
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
		end

	beadsort: BEAD_SORT

	test: ARRAY [INTEGER]

end


```

{{out}}

```txt

unsorted:
1 5 99 2 95 7 7
sorted:
99 95 7 7 5 2 1

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Sort do
  def bead_sort(list) when is_list(list), do: dist(dist(list))

  defp dist(list), do: List.foldl(list, [], fn(n, acc) when n>0 -> dist(acc, n, []) end)

  defp dist([],    0, acc), do: Enum.reverse(acc)
  defp dist([h|t], 0, acc), do: dist(t,    0, [h  |acc])
  defp dist([],    n, acc), do: dist([], n-1, [1  |acc])
  defp dist([h|t], n, acc), do: dist(t,  n-1, [h+1|acc])
end
```


Example:

```txt

iex(20)> Sort.bead_sort([5,3,9,4,1,6,8,2,7])
[9, 8, 7, 6, 5, 4, 3, 2, 1]

```



## Erlang


```erlang
-module(beadsort).

-export([sort/1]).

sort(L) ->
	dist(dist(L)).

dist(L) when is_list(L) ->
	lists:foldl(fun (N, Acc) -> dist(Acc, N, []) end, [], L).

dist([H | T], N, Acc) when N > 0 ->
	dist(T, N - 1, [H + 1 | Acc]);
dist([], N, Acc) when N > 0 ->
	dist([], N - 1, [1 | Acc]);
dist([H | T], 0, Acc) ->
	dist(T, 0, [H | Acc]);
dist([], 0, Acc) ->
	lists:reverse(Acc).
```

Example;

```erlang>1
 beadsort:sort([1,734,24,3,324,324,32,432,42,3,4,1,1]).
[734,432,324,324,42,32,24,4,3,3,1,1,1]
```


=={{header|F_Sharp|F#}}==
{{trans|Haskell}}

```fsharp
open System

let removeEmptyLists lists = lists |> List.filter (not << List.isEmpty)
let flip f x y = f y x

let rec transpose = function
    | []    -> []
    | lists -> (List.map List.head lists) :: transpose(removeEmptyLists (List.map List.tail lists))

// Using the backward composition operator "<<" (equivalent to Haskells ".") ...
let beadSort = List.map List.sum << transpose << transpose << List.map (flip List.replicate 1)

// Using the forward composition operator ">>" ...
let beadSort2 = List.map (flip List.replicate 1) >> transpose >> transpose >> List.map List.sum
```

Usage: beadSort [2;4;1;3;3] or beadSort2 [2;4;1;3;3]

{{out}}

```txt

  val it : int list = [4; 3; 3; 2; 1]

```



## Factor


```factor
USING: kernel math math.order math.vectors sequences ;
: fill ( seq len -- newseq ) [ dup length ] dip swap - 0 <repetition> append ;

: bead ( seq -- newseq )
dup 0 [ max ] reduce
[ swap 1 <repetition> swap fill ] curry map
[ ] [ v+ ] map-reduce ;

: beadsort ( seq -- newseq ) bead bead ;
```


```factor
( scratchpad ) { 5 2 4 1 3 3 9 } beadsort .
{ 9 5 4 3 3 2 1 }
```



## Fortran

{{works with|Fortran|2003}}

{{works with|Fortran|95}} removing the <tt>iso_fortran_env</tt> as explained in code

This implementation suffers the same problems of the C implementation: if the maximum value
in the array to be sorted is very huge, likely there will be not enough free memory to
complete the task. Nonetheless, if the Fortran implementation would use "silently" sparse
arrays and a compact representation for "sequences" of equal values in an array, then this
very same code would run fine even with large integers.


```fortran
program BeadSortTest
  use iso_fortran_env
  ! for ERROR_UNIT; to make this a F95 code,
  ! remove prev. line and declare ERROR_UNIT as an
  ! integer parameter matching the unit associated with
  ! standard error

  integer, dimension(7) :: a = (/ 7, 3, 5, 1, 2, 1, 20 /)

  call beadsort(a)
  print *, a

contains

  subroutine beadsort(a)
    integer, dimension(:), intent(inout) :: a

    integer, dimension(maxval(a), maxval(a)) :: t
    integer, dimension(maxval(a)) :: s
    integer :: i, m

    m = maxval(a)

    if ( any(a < 0) ) then
       write(ERROR_UNIT,*) "can't sort"
       return
    end if

    t = 0
    forall(i=1:size(a)) t(i, 1:a(i)) = 1  ! set up abacus
    forall(i=1:m)             ! let beads "fall"; instead of
       s(i) = sum(t(:, i))    ! moving them one by one, we just
       t(:, i) = 0            ! count how many should be at bottom,
       t(1:s(i), i) = 1       ! and then "reset" and set only those
    end forall

    forall(i=1:size(a)) a(i) = sum(t(i,:))

  end subroutine beadsort

end program BeadSortTest
```



## Go

Sorts non-negative integers only.  The extension to negative values seemed a distraction from this fun task.

```go
package main

import (
    "fmt"
    "sync"
)

var a = []int{170, 45, 75, 90, 802, 24, 2, 66}
var aMax = 1000

const bead = 'o'

func main() {
    fmt.Println("before:", a)
    beadSort()
    fmt.Println("after: ", a)
}

func beadSort() {
    // All space in the abacus = aMax poles x len(a) rows.
    all := make([]byte, aMax*len(a))
    // Slice up space by pole.  (The space could be sliced by row instead,
    // but slicing by pole seemed a more intuitive model of a physical abacus.)
    abacus := make([][]byte, aMax)
    for pole, space := 0, all; pole < aMax; pole++ {
        abacus[pole] = space[:len(a)]
        space = space[len(a):]
    }
    // Use a sync.Waitgroup as the checkpoint mechanism.
    var wg sync.WaitGroup
    // Place beads for each number concurrently. (Presumably beads can be
    // "snapped on" to the middle of a pole without disturbing neighboring
    // beads.)  Also note 'row' here is a row of the abacus.
    wg.Add(len(a))
    for row, n := range a {
        go func(row, n int) {
            for pole := 0; pole < n; pole++ {
                abacus[pole][row] = bead
            }
            wg.Done()
        }(row, n)
    }
    wg.Wait()
    // Now tip the abacus, letting beads fall on each pole concurrently.
    wg.Add(aMax)
    for _, pole := range abacus {
        go func(pole []byte) {
            // Track the top of the stack of beads that have already fallen.
            top := 0
            for row, space := range pole {
                if space == bead {
                    // Move each bead individually, but move it from its
                    // starting row to the top of stack in a single operation.
                    // (More physical simulation such as discovering the top
                    // of stack by inspection, or modeling gravity, are
                    // possible, but didn't seem called for by the task.
                    pole[row] = 0
                    pole[top] = bead
                    top++
                }
            }
            wg.Done()
        }(pole)
    }
    wg.Wait()
    // Read out sorted numbers by row.
    for row := range a {
        x := 0
        for pole := 0; pole < aMax && abacus[pole][row] == bead; pole++ {
            x++
        }
        a[len(a)-1-row] = x
    }
}
```



## Groovy

Solution:

```groovy
def beadSort = { list ->
    final nPoles = list.max()
    list.collect {
        print "."
        ([true] * it) + ([false] * (nPoles - it))
    }.transpose().collect { pole ->
        print "."
        pole.findAll { ! it } + pole.findAll { it }
    }.transpose().collect{ beadTally ->
        beadTally.findAll{ it }.size()
    }
}
```


Annotated Solution (same solution really):

```groovy
def beadSortVerbose = { list ->
    final nPoles = list.max()
    // each row is a number tally-arrayed across the abacus
    def beadTallies = list.collect { number ->
        print "."
        // true == bead, false == no bead
        ([true] * number) + ([false] * (nPoles - number))
    }
    // each row is an abacus pole
    def abacusPoles = beadTallies.transpose()
    def abacusPolesDrop = abacusPoles.collect { pole ->
        print "."
        // beads drop to the BOTTOM of the pole
        pole.findAll { ! it } + pole.findAll { it }
    }
    // each row is a number again
    def beadTalliesDrop = abacusPolesDrop.transpose()
    beadTalliesDrop.collect{ beadTally -> beadTally.findAll{ it }.size() }
}
```


Test:

```groovy
println beadSort([23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4])
println beadSort([88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1])
```


{{out}}

```txt
........................................................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
...............................................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
```


Individual dots shown here are "retallying dots". They are not equivalent to the "swap dots" shown in other [[Groovy]] sorting examples. Like the swap dots the retallying dots represent atomic operations that visually indicate the overall sorting effort. However, they are not equivalent to swaps, or even equivalent in actual effort between bead sorts.

The cost of transposition is not accounted for here because with clever indexing it can easily be optimized away. In fact, one could write a list class for [[Groovy]] that performs the transpose operation merely by setting a single boolean value that controls indexing calculations.


## Haskell


```haskell
import Data.List

beadSort :: [Int] -> [Int]
beadSort = map sum. transpose. transpose. map (flip replicate 1)
```

Example;

```haskell
*Main> beadSort [2,4,1,3,3]
[4,3,3,2,1]
```


=={{header|Icon}} and {{header|Unicon}}==
The program below handles integers and not just whole numbers.  As are so many others, the solution is limited by the lack of sparse array or list compression.


```Icon
procedure main()                     #: demonstrate various ways to sort a list and string
   write("Sorting Demo using ",image(beadsort))
      writes("  on list : ")
      writex(UL := [3, 14, 1, 5, 9, 2, 6, 3])
      displaysort(beadsort,copy(UL))
end

procedure beadsort(X)                           #: return sorted list ascending(or descending)
local base,i,j,x                                # handles negatives and zeros, may also reduce storage

   poles := list(max!X-(base := min!X -1),0)    # set up poles, we will track sums not individual beads
   every x := !X do {                           # each item in the list
      if integer(x) ~= x then runerr(101,x)     # ... must be an integer
      every poles[1 to x - base] +:= 1          # ... beads "fall" into the sum for that pole
      }


   every (X[j := *X to 1 by -1] := base) &
        (i := 1 to *poles) do                   # read from the bottom of the poles
     if poles[i] > 0 then {                     # if there's a bead on the pole ...
        poles[i] -:= 1                          # ... remove it
	    X[j] +:= 1                          # ... and add it in place
     }
   return X
end
```


Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'writex' in Bubble Sort]].
Note: min and max are available in the Icon Programming Library (IPL).

{{out|Abbreviated sample output}}

```txt
Sorting Demo using procedure beadsort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
```



## J


{{eff note|J|\:~}}


```j
bead=: [: +/ #"0&1
```


Example use:

<lang>   bead bead 2 4 1 3 3
4 3 3 2 1
   bead bead 5 3 1 7 4 1 1
7 5 4 3 1 1 1
```


Extending to deal with sequences of arbitrary integers:


```j
bball=: ] (] + [: bead^:2 -) <./ - 1:
```


Example use:

<lang>   bball 2 0 _1 3 1 _2 _3 0
3 2 1 0 0 _1 _2 _3
```



## Java



```Java


public class BeadSort
{
	public static void main(String[] args)
	{
		BeadSort now=new BeadSort();
		int[] arr=new int[(int)(Math.random()*11)+5];
		for(int i=0;i<arr.length;i++)
			arr[i]=(int)(Math.random()*10);
		System.out.print("Unsorted: ");
		now.display1D(arr);

		int[] sort=now.beadSort(arr);
		System.out.print("Sorted: ");
		now.display1D(sort);
	}
	int[] beadSort(int[] arr)
	{
		int max=a[0];
		for(int i=1;i<arr.length;i++)
			if(arr[i]>max)
				max=arr[i];

		//Set up abacus
		char[][] grid=new char[arr.length][max];
		int[] levelcount=new int[max];
		for(int i=0;i<max;i++)
		{
			levelcount[i]=0;
			for(int j=0;j<arr.length;j++)
				grid[j][i]='_';
		}
		/*
		display1D(arr);
		display1D(levelcount);
		display2D(grid);
		*/

		//Drop the beads
		for(int i=0;i<arr.length;i++)
		{
			int num=arr[i];
			for(int j=0;num>0;j++)
			{
				grid[levelcount[j]++][j]='*';
				num--;
			}
		}
		System.out.println();
		display2D(grid);
		//Count the beads
		int[] sorted=new int[arr.length];
		for(int i=0;i<arr.length;i++)
		{
			int putt=0;
			for(int j=0;j<max&&grid[arr.length-1-i][j]=='*';j++)
				putt++;
			sorted[i]=putt;
		}

		return sorted;
	}
	void display1D(int[] arr)
	{
		for(int i=0;i<arr.length;i++)
			System.out.print(arr[i]+" ");
		System.out.println();
	}
	void display1D(char[] arr)
	{
		for(int i=0;i<arr.length;i++)
			System.out.print(arr[i]+" ");
		System.out.println();
	}
	void display2D(char[][] arr)
	{
		for(int i=0;i<arr.length;i++)
			display1D(arr[i]);
		System.out.println();
	}
}

```

{{out}}

```txt

Unsorted: 9 4 7 0 4 3 0 5 3 8 7 9 8 7 0

* * * * * * * * *
* * * * * * * * *
* * * * * * * * _
* * * * * * * * _
* * * * * * * _ _
* * * * * * * _ _
* * * * * * * _ _
* * * * * _ _ _ _
* * * * _ _ _ _ _
* * * * _ _ _ _ _
* * * _ _ _ _ _ _
* * * _ _ _ _ _ _
_ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _

Sorted: 0 0 0 3 3 4 4 5 7 7 7 8 8 9 9

```



## jq

'''Part 1: The abacus'''
This implementation uses an "abacus" as described in the Wikipedia article.
However, rather than representing each row as a set of n beads, it suffices
to use the integer n instead.  Thus the initial state of our abacus
is simply the array of numbers to be sorted. (A better approach would be to normalize the integers by subtracting their minimum value minus 1; that would also allow sorting arrays of integers without restriction.)

'''Part 2: Gravity'''

```jq
# ncols is the number of columns (i.e. vertical poles)
def column_sums(ncols):
  . as $abacus
  | reduce range(0; ncols) as $col
    ([];
     . + [reduce $abacus[] as $row
           (0; if $row > $col then .+1 else . end)]) ;
```

'''Part 3: read the answer in order of largest-to-smallest'''

```jq
# Generic function to count the number of items in a stream:
def count(stream): reduce stream as $i (0; .+1);

def readout:
  . as $sums
  | .[0] as $n
  | reduce range(0;$n) as $i
      ([]; . + [count( $sums[] | select( . > $i) )]);
```

'''"Bead Sort":'''

```jq
def bead_sort: column_sums(max) | readout;
```


'''Example:'''

```jq
[734,3,1,24,324,324,32,432,42,3,4,1,1] | bead_sort
```

{{out}}

```sh
$ jq -n -c -f bead_sort.jq
[734,432,324,324,42,32,24,4,3,3,1,1,1]
```



## Julia

{{works with|Julia|0.6}}
Implement <code>beadsort</code> on a <code>BitArray</code> ''abacus''.  The function should work for any integer type.  It throws a <code>DomainError</code> if the input array contains a non-positive integer.

```julia
function beadsort(a::Vector{<:Integer})
    lo, hi = extrema(a)
    if lo < 1 throw(DomainError()) end
    len = length(a)
    abacus = falses(len, hi)
    for (i, v) in enumerate(a)
       abacus[i, 1:v] = true
    end
    for i in 1:hi
        v = sum(abacus[:, i])
        if v < len
            abacus[1:end-v, i] = false
            abacus[end-v+1:end, i] = true
        end
    end
    return collect(eltype(a), sum(abacus[i,:]) for i in 1:len)
end

v = rand(UInt8, 20)
println("# unsorted bytes: $v\n -> sorted bytes: $(beadsort(v))")
v = rand(1:2 ^ 10, 20)
println("# unsorted integers: $v\n -> sorted integers: $(beadsort(v))")
```


{{out}}

```txt
# unsorted bytes: UInt8[0xff, 0x52, 0xdd, 0x72, 0xe2, 0x13, 0xb5, 0xd3, 0x7f, 0xea, 0x3b, 0x46, 0x4b, 0x78, 0xfb, 0xbe, 0xd8, 0x2e, 0xa9, 0x7a]
 -> sorted bytes: UInt8[0x13, 0x2e, 0x3b, 0x46, 0x4b, 0x52, 0x72, 0x78, 0x7a, 0x7f, 0xa9, 0xb5, 0xbe, 0xd3, 0xd8, 0xdd, 0xe2, 0xea, 0xfb, 0xff]
# unsorted integers: [1012, 861, 798, 949, 481, 889, 78, 699, 718, 195, 426, 922, 762, 360, 1017, 208, 304, 13, 910, 854]
 -> sorted integers: [13, 78, 195, 208, 304, 360, 426, 481, 699, 718, 762, 798, 854, 861, 889, 910, 922, 949, 1012, 1017]
```



## Kotlin

{{trans|C}}

```scala
// version 1.1.2

fun beadSort(a: IntArray) {
    val n = a.size
    if (n < 2) return
    var max = a.max()!!
    val beads = ByteArray(max * n)
    /* mark the beads */
    for (i in 0 until n)
        for (j in 0 until a[i])
            beads[i * max + j] = 1

    for (j in 0 until max) {
        /* count how many beads are on each post */
        var sum = 0
        for (i in 0 until n) {
            sum += beads[i * max + j]
            beads[i * max + j] = 0
        }
        /* mark bottom sum beads */
        for (i in n - sum until n) beads[i * max + j] = 1
    }

    for (i in 0 until n) {
        var j = 0
        while (j < max && beads[i * max + j] == 1.toByte()) j++
        a[i] = j
    }
}

fun main(args: Array<String>) {
    val a  = intArrayOf(5, 3, 1, 7, 4, 1, 1, 20)
    println("Before sorting : ${a.contentToString()}")
    beadSort(a)
    println("After sorting  : ${a.contentToString()}")
}
```


{{out}}

```txt

Before sorting : [5, 3, 1, 7, 4, 1, 1, 20]
After sorting  : [1, 1, 1, 3, 4, 5, 7, 20]

```



## Lua


```Lua
-- Display message followed by all values of a table in one line
function show (msg, t)
    io.write(msg .. ":\t")
    for _, v in pairs(t) do io.write(v .. " ") end
    print()
end

-- Return a table of random numbers
function randList (length, lo, hi)
    local t = {}
    for i = 1, length do table.insert(t, math.random(lo, hi)) end
    return t
end

-- Count instances of numbers that appear in counting to each list value
function tally (list)
    local tal = {}
    for k, v in pairs(list) do
        for i = 1, v do
            if tal[i] then tal[i] = tal[i] + 1 else tal[i] = 1 end
        end
    end
    return tal
end

-- Sort a table of positive integers into descending order
function beadSort (numList)
    show("Before sort", numList)
    local abacus = tally(numList)
    show("Tally list", abacus)
    local sorted = tally(abacus)
    show("After sort", sorted)
end

-- Main procedure
math.randomseed(os.time())
beadSort(randList(10, 1, 10))
```

{{out}}

```txt
Before sort:    9 5 3 9 4 1 3 8 1 2
Tally list:     10 8 7 5 4 3 3 3 2
After sort:     9 9 8 5 4 3 3 2 1 1
```



## Mathematica


```Mathematica
beadsort[ a ] := Module[ { m, sorted, s ,t },

sorted = a; m = Max[a]; t=ConstantArray[0, {m,m} ];
If[ Min[a] < 0, Print["can't sort"]];
For[ i = 1, i < Length[a], i++,  t[[i,1;;a[[i]]]]=1 ]

For[ i = 1 ,i <= m, i++, s = Total[t[[;;,i]]];
t[[ ;; , i]] = 0; t[[1 ;; s , i]] = 1; ]

For[ i=1,i<=Length[a],i++, sorted[[i]] = Total[t[[i,;;]]]; ]
Print[sorted];
]
```



```txt
beadsort[{2,1,5,3,6}]
->{6,3,2,1,0}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method bead_sort(harry = Rexx[]) public static binary returns Rexx[]
  MIN_ = 'MIN'
  MAX_ = 'MAX'
  beads = Rexx 0
  beads[MIN_] = 0
  beads[MAX_] = 0

  loop val over harry
    -- collect occurences of beads in indexed string indexed on value
    if val < beads[MIN_] then beads[MIN_] = val -- keep track of min value
    if val > beads[MAX_] then beads[MAX_] = val -- keep track of max value
    beads[val] = beads[val] + 1
    end val

  harry_sorted = Rexx[harry.length]
  bi = 0
  loop xx = beads[MIN_] to beads[MAX_]
    -- extract beads in value order and insert in result array
    if beads[xx] == 0 then iterate xx
    loop for beads[xx]
      harry_sorted[bi] = xx
      bi = bi + 1
      end
    end xx

  return harry_sorted

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  unsorted = [734, 3, 1, 24, 324, -1024, -666, -1, 0, 324, 32, 0, 432, 42, 3, 4, 1, 1]
  sorted = bead_sort(unsorted)
  say arrayToString(unsorted)
  say arrayToString(sorted)
  return
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method arrayToString(harry = Rexx[]) private static
  list = Rexx ''
  loop vv over harry
    list = list vv
    end vv
  return '['list.space(1, ',')']'

```

{{out}}

```txt

[734,3,1,24,324,-1024,-666,-1,0,324,32,0,432,42,3,4,1,1]
[-1024,-666,-1,0,0,1,1,1,3,3,4,24,32,42,324,324,432,734]

```



## Nim


```nim
proc beadSort[T](a: var openarray[T]) =
  var max = low(T)
  var sum = 0

  for x in a:
    if x > max: max = x

  var beads = newSeq[int](max * a.len)

  for i in 0 .. < a.len:
    for j in 0 .. < a[i]:
      beads[i * max + j] = 1

  for j in 0 .. < max:
    sum = 0
    for i in 0 .. < a.len:
      sum += beads[i * max + j]
      beads[i * max + j] = 0

    for i in a.len - sum .. < a.len:
      beads[i * max + j] = 1

  for i in 0 .. < a.len:
    var j = 0
    while j < max and beads[i * max + j] > 0: inc j
    a[i] = j

var a = @[5, 3, 1, 7, 4, 1, 1, 20]
beadSort a
echo a
```

{{out}}

```txt
@[1, 1, 1, 3, 4, 5, 7, 20]
```



## OCaml

{{trans|Haskell}}

```ocaml
let rec columns l =
  match List.filter ((<>) []) l with
    [] -> []
  | l -> List.map List.hd l :: columns (List.map List.tl l)

let replicate n x = Array.to_list (Array.make n x)

let bead_sort l =
  List.map List.length (columns (columns (List.map (fun e -> replicate e 1) l)))
```

usage

```txt

# bead_sort [5;3;1;7;4;1;1];;
- : int list = [7; 5; 4; 3; 1; 1; 1]

```



## Octave

{{trans|Fortran}}

```octave
function sorted = beadsort(a)
  sorted = a;
  m = max(a);
  if ( any(a < 0) )
    error("can't sort");
  endif
  t = zeros(m, m);
  for i = 1:numel(a)
    t(i, 1:a(i)) = 1;
  endfor
  for i = 1:m
    s = sum(t(:, i));
    t(:, i) = 0;
    t(1:s, i) = 1;
  endfor
  for i = 1:numel(a)
    sorted(i) = sum(t(i, :));
  endfor
endfunction

beadsort([5, 7, 1, 3, 1, 1, 20])
```



## ooRexx


### version 1


```oorexx
in='10 -12 1 0 999 8 2 2 4 4'
 Do i=1 To words(in)
   z.i=word(in,i)
   End
 n=i-1
 init=0
 Call minmax

 beads.=0;
 Do i=1 To words(in)
   z=z.i
   beads.z+=1
   End
 j=0
 Do i=lo To hi
   Do While beads.i>0
     j+=1
     s.j=i
     beads.i-=1
     End;
   End;
 Call show ' Input:',z.,n
 Call show 'Sorted:',s.,n
 Exit

 minmax:
 Do i=1 To n
   If init=0 Then Do
     init=1
     lo=z.i
     hi=z.i
     End
   Else Do
     lo=min(lo,z.i)
     hi=max(hi,z.i)
     End
   End
 Return

show: Procedure Expose n
 Use Arg txt,a.
 ol=txtg>
 Do i=1 To n
   ol=ol format(a.i,3)
   End
 Say ol
 Return
```

{{out}}

```txt
 Input:  10 -12   1   0 999   8   2   2   4   4
Sorted: -12   0   1   2   2   4   4   8  10 999
```



### version 2

{{trans|REXX}}
'''Note:''' The only changes needed were to substitute '''<tt>_</tt>''', '''<tt>!</tt>''' and '''<tt>?</tt>''' characters for the &quot;deprecated&quot; <tt>'''$'''</tt>, <tt>'''#'''</tt> and '''<tt>@</tt>''' characters within variable names; as per <cite>The REXX Language, Second Edition</cite> by M. F. Cowlishaw. (See a description [http://www.rexxla.org/rexxlang/mfc/trl.html here]).

```ooRexx
/*REXX program sorts a list of integers using a bead sort. */

             /*get some grassHopper numbers.                            */
grasshopper=,
1 4 10 12 22 26 30 46 54 62 66 78 94 110 126 134 138 158 162 186 190 222 254 270



             /*GreeenGrocer numbers are also called hexagonal pyramidal */
             /*             numbers.                                    */
greengrocer=,
0 4 16 40 80 140 224 336 480 660 880 1144 1456 1820 2240 2720 3264 3876 4560


             /*get some Bernoulli numerator numbers.                    */
bernN='1 -1 1 0 -1 0 1 0 -1 0 5 0 -691 0 7 0 -3617 0 43867 0 -174611 0 854513'


             /*Psi is also called the Reduced Totient function,  and    */
             /*    is also called Carmichale lambda, or LAMBDA function.*/
psi=,
1 1 2 2 4 2 6 2 6 4 10 2 12 6 4 4 16 6 18 4 6 10 22 2 20 12 18 6 28 4 30 8 10 16



list=grasshopper greengrocer bernN psi /*combine the four lists into one*/


call showL 'before sort',list          /*show list before sorting. */
!=beadSort(list)                       /*invoke the bead sort.     */
call showL ' after sort',!             /*show  after array elements*/
exit


/*─────────────────────────────────beadSort@ subroutine────────────*/
beadSort: procedure expose _.
  parse arg z
  !=''                                 /*this'll be the sorted list*/
  low=999999999; high=-low             /*define the low and high #s*/
  _.=0                                 /*define all beads to zero. */


  do j=1 until z==''                   /*pick the meat off the bone*/
    parse var z x z
    if \datatype(x,'Whole') then
      do
        say
        say '*** error! ***'
        say
        say 'element' j "in list isn't numeric:" x
        say
        exit 13
        end

    x=x/1                              /*normalize number, it could*/
                                       /*be:  +4  007  5.  2e3 etc.*/
    _.x=_.x+1                          /*indicate this bead has a #*/
    low=min(low,x)                     /*keep track of the lowest #*/
    high=max(high,x)                   /* "     "    "  "  highest#*/
    end j

                                       /*now, collect the beads and*/
  do m=low to high                     /*let them fall (to zero).  */
    if _.m==0 then iterate             /*No bead here? Keep looking*/
    do n=1 for _.m                     /*let the beads fall to  0. */
      !=! m                            /*add it to the sorted list.*/
      end n
    end m

  return !


/*─────────────────────────────────────SHOW@ subroutine────────────*/
showL:
  widthH=length(words(arg(2)))         /*maximum width of the index*/

  do j=1 for words(arg(2))
    say 'element' right(j,widthH) arg(1)":" right(word(arg(2),j),10)
    end j

  say copies('─',80)                   /*show a separator line.    */
  return

```


{{out}}
<pre style="height:30ex;overflow:scroll">
element   1 before sort:          1
element   2 before sort:          4
element   3 before sort:         10
element   4 before sort:         12
element   5 before sort:         22
element   6 before sort:         26
element   7 before sort:         30
element   8 before sort:         46
element   9 before sort:         54
element  10 before sort:         62
element  11 before sort:         66
element  12 before sort:         78
element  13 before sort:         94
element  14 before sort:        110
element  15 before sort:        126
element  16 before sort:        134
element  17 before sort:        138
element  18 before sort:        158
element  19 before sort:        162
element  20 before sort:        186
element  21 before sort:        190
element  22 before sort:        222
element  23 before sort:        254
element  24 before sort:        270
element  25 before sort:          0
element  26 before sort:          4
element  27 before sort:         16
element  28 before sort:         40
element  29 before sort:         80
element  30 before sort:        140
element  31 before sort:        224
element  32 before sort:        336
element  33 before sort:        480
element  34 before sort:        660
element  35 before sort:        880
element  36 before sort:       1144
element  37 before sort:       1456
element  38 before sort:       1820
element  39 before sort:       2240
element  40 before sort:       2720
element  41 before sort:       3264
element  42 before sort:       3876
element  43 before sort:       4560
element  44 before sort:          1
element  45 before sort:         -1
element  46 before sort:          1
element  47 before sort:          0
element  48 before sort:         -1
element  49 before sort:          0
element  50 before sort:          1
element  51 before sort:          0
element  52 before sort:         -1
element  53 before sort:          0
element  54 before sort:          5
element  55 before sort:          0
element  56 before sort:       -691
element  57 before sort:          0
element  58 before sort:          7
element  59 before sort:          0
element  60 before sort:      -3617
element  61 before sort:          0
element  62 before sort:      43867
element  63 before sort:          0
element  64 before sort:    -174611
element  65 before sort:          0
element  66 before sort:     854513
element  67 before sort:          1
element  68 before sort:          1
element  69 before sort:          2
element  70 before sort:          2
element  71 before sort:          4
element  72 before sort:          2
element  73 before sort:          6
element  74 before sort:          2
element  75 before sort:          6
element  76 before sort:          4
element  77 before sort:         10
element  78 before sort:          2
element  79 before sort:         12
element  80 before sort:          6
element  81 before sort:          4
element  82 before sort:          4
element  83 before sort:         16
element  84 before sort:          6
element  85 before sort:         18
element  86 before sort:          4
element  87 before sort:          6
element  88 before sort:         10
element  89 before sort:         22
element  90 before sort:          2
element  91 before sort:         20
element  92 before sort:         12
element  93 before sort:         18
element  94 before sort:          6
element  95 before sort:         28
element  96 before sort:          4
element  97 before sort:         30
element  98 before sort:          8
element  99 before sort:         10
element 100 before sort:         16
────────────────────────────────────────────────────────────────────────────────
element   1  after sort:    -174611
element   2  after sort:      -3617
element   3  after sort:       -691
element   4  after sort:         -1
element   5  after sort:         -1
element   6  after sort:         -1
element   7  after sort:          0
element   8  after sort:          0
element   9  after sort:          0
element  10  after sort:          0
element  11  after sort:          0
element  12  after sort:          0
element  13  after sort:          0
element  14  after sort:          0
element  15  after sort:          0
element  16  after sort:          0
element  17  after sort:          0
element  18  after sort:          1
element  19  after sort:          1
element  20  after sort:          1
element  21  after sort:          1
element  22  after sort:          1
element  23  after sort:          1
element  24  after sort:          2
element  25  after sort:          2
element  26  after sort:          2
element  27  after sort:          2
element  28  after sort:          2
element  29  after sort:          2
element  30  after sort:          4
element  31  after sort:          4
element  32  after sort:          4
element  33  after sort:          4
element  34  after sort:          4
element  35  after sort:          4
element  36  after sort:          4
element  37  after sort:          4
element  38  after sort:          5
element  39  after sort:          6
element  40  after sort:          6
element  41  after sort:          6
element  42  after sort:          6
element  43  after sort:          6
element  44  after sort:          6
element  45  after sort:          7
element  46  after sort:          8
element  47  after sort:         10
element  48  after sort:         10
element  49  after sort:         10
element  50  after sort:         10
element  51  after sort:         12
element  52  after sort:         12
element  53  after sort:         12
element  54  after sort:         16
element  55  after sort:         16
element  56  after sort:         16
element  57  after sort:         18
element  58  after sort:         18
element  59  after sort:         20
element  60  after sort:         22
element  61  after sort:         22
element  62  after sort:         26
element  63  after sort:         28
element  64  after sort:         30
element  65  after sort:         30
element  66  after sort:         40
element  67  after sort:         46
element  68  after sort:         54
element  69  after sort:         62
element  70  after sort:         66
element  71  after sort:         78
element  72  after sort:         80
element  73  after sort:         94
element  74  after sort:        110
element  75  after sort:        126
element  76  after sort:        134
element  77  after sort:        138
element  78  after sort:        140
element  79  after sort:        158
element  80  after sort:        162
element  81  after sort:        186
element  82  after sort:        190
element  83  after sort:        222
element  84  after sort:        224
element  85  after sort:        254
element  86  after sort:        270
element  87  after sort:        336
element  88  after sort:        480
element  89  after sort:        660
element  90  after sort:        880
element  91  after sort:       1144
element  92  after sort:       1456
element  93  after sort:       1820
element  94  after sort:       2240
element  95  after sort:       2720
element  96  after sort:       3264
element  97  after sort:       3876
element  98  after sort:       4560
element  99  after sort:      43867
element 100  after sort:     854513
────────────────────────────────────────────────────────────────────────────────

```



## OpenEdge/Progress

Sorting algorithms are not the kind of thing you need / want to do in OpenEdge. If you want to sort simply define a temp-table with one field, populate it and get sorted results with FOR EACH temp-table DESCENDING.

```OpenEdge/Progress
FUNCTION beadSort RETURNS CHAR (
   i_c AS CHAR
):

   DEF VAR cresult   AS CHAR.
   DEF VAR ii        AS INT.
   DEF VAR inumbers  AS INT.
   DEF VAR irod      AS INT.
   DEF VAR irods     AS INT.
   DEF VAR crod      AS CHAR.
   DEF VAR cbeads    AS CHAR EXTENT.

   inumbers = NUM-ENTRIES( i_c ).

   /* determine number of rods needed */
   DO ii = 1 TO inumbers:
      irods = MAXIMUM( irods, INTEGER( ENTRY( ii, i_c ) ) ).
   END.

   /* put beads on rods */
   EXTENT( cbeads ) = inumbers.
   DO ii = 1 TO inumbers:
      cbeads[ ii ] = FILL( "X", INTEGER( ENTRY( ii, i_c ) ) ).
   END.

   /* drop beads on each rod */
   DO irod = 1 TO irods:
      crod = "".
      DO ii = 1 TO inumbers:
         crod = crod + SUBSTRING( cbeads[ ii ], irod, 1 ).
      END.
      crod = REPLACE( crod, " ", "" ).
      DO ii = 1 TO inumbers.
         SUBSTRING( cbeads[ ii ], irod, 1 ) = STRING( ii <= LENGTH( crod ), "X/ " ).
      END.
   END.

   /* get beads from rods */
   DO ii = 1 TO inumbers:
      cresult = cresult + "," + STRING( LENGTH( REPLACE( cbeads[ ii ], " ", "" ) ) ).
   END.

   RETURN SUBSTRING( cresult, 2 ).

END FUNCTION. /* beadSort */

MESSAGE
   "5,2,4,1,3,3,9  -> " beadSort( "5,2,4,1,3,3,9" ) SKIP
   "5,3,1,7,4,1,1  -> " beadSort( "5,3,1,7,4,1,1" ) SKIP(1)
   beadSort( "88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1" )
VIEW-AS ALERT-BOX.
```

{{out}}

```txt
---------------------------
Message
---------------------------
5,2,4,1,3,3,9  ->  9,5,4,3,3,2,1
5,3,1,7,4,1,1  ->  7,5,4,3,1,1,1

88,84,82,81,78,76,75,73,70,62,44,33,31,20,18,14,12,8,7,5,4,1,0
---------------------------
OK
---------------------------
```



## PARI/GP

This implementation uses the counting sort to order the beads in a given row.

```parigp
beadsort(v)={
  my(sz=vecmax(v),M=matrix(#v,sz,i,j,v[i]>=j)); \\ Set up beads
  for(i=1,sz,M[,i]=countingSort(M[,i],0,1)~);   \\ Let them fall
  vector(#v,i,value(M[i,]))                     \\ Convert back to numbers
};

countingSort(v,mn,mx)={
  my(u=vector(#v),i=0);
  for(n=mn,mx,
    for(j=1,#v,if(v[j]==n,u[i++]=n))
  );
  u
};

value(v)={
  if(#v==0 || !v[1], return(0));
  if(v[#v], return(#v));
  my(left=1, right=#v, mid);
  while (right - left > 1,
    mid=(right+left)\2;
    if(v[mid], left=mid, right=mid)
  );
  left
};
```



## Pascal

See [[Sorting_algorithms/Bead_sort#Delphi | Delphi]]


## Perl

Instead of storing the bead matrix explicitly, I choose to store just the number of beads in each row and column, compacting on the fly. At all times, the sum of the row widths is equal to the sum column heights.


```perl
sub beadsort {
    my @data = @_;

    my @columns;
    my @rows;

    for my $datum (@data) {
        for my $column ( 0 .. $datum-1 ) {
            ++ $rows[ $columns[$column]++ ];
        }
    }

    return reverse @rows;
}

beadsort 5, 7, 1, 3, 1, 1, 20;

```



## Perl 6

{{Works with|rakudo|2016-05}}
{{trans|Haskell}}

```perl6
# routine cribbed from List::Utils;
sub transpose(@list is copy) {
    gather {
        while @list {
            my @heads;
            if @list[0] !~~ Positional { @heads = @list.shift; }
            else { @heads = @list.map({$_.shift unless $_ ~~ []}); }
            @list = @list.map({$_ unless $_ ~~ []});
            take [@heads];
        }
    }
}

sub beadsort(@l) {
    (transpose(transpose(map {[1 xx $_]}, @l))).map(*.elems);
}

my @list = 2,1,3,5;
say beadsort(@list).perl;
```


{{out}}

```txt
(5, 3, 2, 1)
```

Here we simulate the dropping beads by using the <tt>push</tt> method.

```perl6
sub beadsort(*@list) {
    my @rods;
    for words ^«@list -> $x { @rods[$x].push(1) }
    gather for ^@rods[0] -> $y {
        take [+] @rods.map: { .[$y] // last }
    }
}

say beadsort 2,1,3,5;
```

The <tt>^</tt> is the "upto" operator that gives a range of 0 up to (but not including) its endpoint.  We use it as a hyperoperator (<tt>^«</tt>) to generate all the ranges of rod numbers we should drop a bead on, with the result that <tt>$x</tt> tells us which rod to drop each bead on.  Then we use <tt>^</tt> again on the first rod to see how deep the beads are stacked, since they are guaranteed to be the deepest there.  The <tt>[+]</tt> adds up all the beads that are found at level <tt>$y</tt>.  The <tt>last</tt> short circuits the map so we don't have to look for all the missing beads at a given level, since the missing beads are all guaranteed to come after the existing beads at that level (because we always dropped left to right starting at rod 0).


## Phix


```Phix
function beadsort(sequence a)
    sequence poles = repeat(0,max(a))
    for i=1 to length(a) do
        poles[1..a[i]] = sq_add(poles[1..a[i]],1)
    end for
    a[1..$] = 0
    for i=1 to length(poles) do
        a[1..poles[i]] = sq_add(a[1..poles[i]],1)
    end for
    return a
end function

?beadsort({5, 3, 1, 7, 4, 1, 1, 20})
```

{{out}}

```txt

{20,7,5,4,3,1,1,1}

```



## PHP

{{trans|Haskell}}

```php
<?php
function columns($arr) {
    if (count($arr) == 0)
        return array();
    else if (count($arr) == 1)
        return array_chunk($arr[0], 1);

    array_unshift($arr, NULL);
    // array_map(NULL, $arr[0], $arr[1], ...)
    $transpose = call_user_func_array('array_map', $arr);
    return array_map('array_filter', $transpose);
}

function beadsort($arr) {
    foreach ($arr as $e)
        $poles []= array_fill(0, $e, 1);
    return array_map('count', columns(columns($poles)));
}

print_r(beadsort(array(5,3,1,7,4,1,1)));
?>
```


{{out}}

```txt
Array
(
    [0] => 7
    [1] => 5
    [2] => 4
    [3] => 3
    [4] => 1
    [5] => 1
    [6] => 1
)
```



## PicoLisp

The following implements a direct model of the bead sort algorithm.
Each pole is a list of 'T' symbols for the beads.

```PicoLisp
(de beadSort (Lst)
   (let Abacus (cons NIL)
      (for N Lst                                   # Thread beads on poles
         (for (L Abacus  (ge0 (dec 'N))  (cdr L))
            (or (cdr L) (queue 'L (cons)))
            (push (cadr L) T) ) )
      (make
         (while (gt0 (cnt pop (cdr Abacus)))       # Drop and count beads
            (link @) ) ) ) )
```

{{out}}

```txt
: (beadSort (5 3 1 7 4 1 1 20))
-> (20 7 5 4 3 1 1 1)
```



## PL/I


### version 1


```PL/I

/* Handles both negative and positive values. */

maxval: procedure (z) returns (fixed binary);
   declare z(*) fixed binary;
   declare (maxv initial (0), i) fixed binary;
   do i = lbound(z,1) to hbound(z,1);
      maxv = max(z(i), maxv);
   end;
   put skip data (maxv); put skip;
   return (maxv);
end maxval;
minval: procedure (z) returns (fixed binary);
   declare z(*) fixed binary;
   declare (minv initial (0), i) fixed binary;

   do i = lbound(z,1) to hbound(z,1);
      if z(i) < 0 then minv = min(z(i), minv);
   end;
   put skip data (minv); put skip;
   return (minv);
end minval;

/* To deal with negative values, array elements are incremented */
/* by the greatest (in magnitude) negative value, thus making   */
/* them positive. The resultant values are stored in an         */
/* unsigned array (PL/I provides both signed and unsigned data  */
/* types). At procedure end, the array values are restored to   */
/* original values.                                             */

(subrg, fofl, size, stringrange, stringsize):
beadsort: procedure (z);                        /* 8-1-2010 */
   declare (z(*)) fixed binary;
   declare b(maxval(z)-minval(z)+1) bit (maxval(z)-minval(z)+1) aligned;
   declare (i, j, k, m, n) fixed binary;
   declare a(hbound(z,1)) fixed binary unsigned;
   declare offset fixed binary initial (minval(z));

   PUT SKIP LIST('CHECKPOINT A'); PUT SKIP;
   n = hbound(z,1);
   m = hbound(b,1);

   if offset < 0 then
      a = z - offset;
   else
      a = z;

   b = '0'b;

   do i = 1 to n;
      substr(b(i), 1, a(i)) = copy('1'b, a(i));
   end;
   do j = 1 to m; put skip list (b(j)); end;

   do j = 1 to m;
      k = 0;
      do i =1 to n;
         if substr(b(i), j, 1) then k = k + 1;
      end;
      do i = 1 to n;
         substr(b(i), j, 1) = (i <= k);
      end;
   end;
   put skip;
   do j = 1 to m; put skip list (b(j)); end;

   do i = 1 to n;
      k = 0;
      do j = 1 to m; k = k + substr(b(i), j, 1); end;
      a(i) = k;
   end;
   if offset < 0 then z = a + offset; else z = a;

end beadsort;
```



### version 2

{{trans|ooRexx}}
PL/I supports negative array indices!

```pli
*process source attributes xref;
 /* Handles both negative and positive values. */
 Beadsort: Proc Options(main);
 Dcl sysprint Print;
 Dcl (hbound,max,min) Builtin;

 Dcl z(10) Bin Fixed(31) Init(10,-12,1,0,999,8,2,2,4,4);
 Dcl s(10) Bin Fixed(31);
 Dcl (init,lo,hi) Bin Fixed(31) Init(0);
 Dcl (i,j) Bin Fixed(31) Init(0);

 Call minmax(z,init,lo,hi);

 Begin;
 Dcl beads(lo:hi) Bin Fixed(31);
 beads=0;
 Do i=1 To hbound(z);
   beads(z(i))+=1;
   End;
 Do i=lo To hi;
   Do While(beads(i)>0);
     j+=1;
     s(j)=i;
     beads(i)-=1;
     End;
   End;
 Put Edit(' Input:',(z(i) Do i=1 To hbound(z)))(skip,a,99(f(4)));
 Put Edit('Sorted:',(s(i) Do i=1 To hbound(s)))(skip,a,99(f(4)));
 End;

 minmax: Proc(z,init,lo,hi);
 Dcl z(*) Bin Fixed(31);
 Dcl (init,lo,hi) Bin Fixed(31);
 Do i=1 To hbound(z);
   If init=0 Then Do;
     init=1;
     lo,hi=z(i);
     End;
   Else Do;
     lo=min(lo,z(i));
     hi=max(hi,z(i));
     End;
   End;
 End;

 End;
```

{{out}}

```txt
 Input:  10 -12   1   0 999   8   2   2   4   4
Sorted: -12   0   1   2   2   4   4   8  10 999
```



## PowerShell


```PowerShell
Function BeadSort ( [Int64[]] $indata )
{
	if( $indata.length -gt 1 )
	{
		$min = $indata[ 0 ]
		$max = $indata[ 0 ]
		for( $i = 1; $i -lt $indata.length; $i++ )
		{
			if( $indata[ $i ] -lt $min )
			{
				$min = $indata[ $i ]
			}
			if( $indata[ $i ] -gt $max ) {
				$max = $indata[ $i ]
			}
		} #Find the min & max
		$poles = New-Object 'UInt64[]' ( $max - $min + 1 )
		$indata | ForEach-Object {
			$min..$_ | ForEach-Object {
				$poles[ $_ - $min ] += 1
			}
		} #Add Beads to the poles, already moved to the bottom
		$min..( $max - 1 ) | ForEach-Object {
			$i = $_ - $min
			if( $poles[ $i ] -gt $poles[ $i + 1 ] )
			{ #No special case needed for min, since there will always be at least 1 = min
				( $poles[ $i ] )..( $poles[ $i + 1 ] + 1 ) | ForEach-Object {
					Write-Output ( $i + $min )
				}
			}
		} #Output the results in pipeline fashion
		1..( $poles[ $max - $min ] ) | ForEach-Object {
			Write-Output $max  #No special case needed for max, since there will always be at least 1 = max
		}
	} else {
		Write-Output $indata
	}
}

$l = 100; BeadSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( -( $l - 1 ), $l - 1 ) } )
```



## PureBasic


```PureBasic
#MAXNUM=100

Dim MyData(Random(15)+5)
Global Dim Abacus(0,0)

Declare BeadSort(Array InData(1))
Declare PresentData(Array InData(1))

If OpenConsole()
  Define i
  ;- Generate a random array
  For i=0 To ArraySize(MyData())
    MyData(i)=Random(#MAXNUM)
  Next i
  PresentData(MyData())
  ;
  ;- Sort the array
  BeadSort(MyData())
  PresentData(MyData())
  ;
  Print("Press ENTER to exit"): Input()
EndIf

Procedure LetFallDown(x)
  Protected y=ArraySize(Abacus(),2)-1
  Protected ylim=y
  While y>=0
    If Abacus(x,y) And Not Abacus(x,y+1)
      Swap Abacus(x,y), Abacus(x,y+1)
      If y<ylim: y+1: Continue: EndIf
    Else
      y-1
    EndIf
  Wend
EndProcedure

Procedure BeadSort(Array n(1))
  Protected i, j, k
  NewList T()
  Dim Abacus(#MAXNUM,ArraySize(N()))
  ;- Set up the abacus
  For i=0 To ArraySize(Abacus(),2)
    For j=1 To N(i)
      Abacus(j,i)=#True
    Next
  Next
  ;- sort it in threads to simulate free beads falling down
  For i=0 To #MAXNUM
    AddElement(T()): T()=CreateThread(@LetFallDown(),i)
  Next
  ForEach T()
    WaitThread(T())
  Next
  ;- send it back to a normal array
  For j=0 To ArraySize(Abacus(),2)
    k=0
    For i=0 To ArraySize(Abacus())
      k+Abacus(i,j)
    Next
    N(j)=k
  Next
EndProcedure

Procedure PresentData(Array InData(1))
  Protected n, m, sum
  PrintN(#CRLF$+"The array is;")
  For n=0 To ArraySize(InData())
    m=InData(n): sum+m
    Print(Str(m)+" ")
  Next
  PrintN(#CRLF$+"And its sum= "+Str(sum))
EndProcedure
```


```txt

The array is;
4 38 100 25 69 69 16 8 59 71 53 33
And its sum= 545

The array is;
4 8 16 25 33 38 53 59 69 69 71 100
And its sum= 545
```



## Python


```python

#!/bin/python3
from itertools import zip_longest


def beadsort(l):
    return list(map(sum, zip_longest(*[[1] * e for e in l], fillvalue=0)))


# Demonstration code:
print(beadsort([5,3,1,7,4,1,1]))

```


{{out}}

```txt
[7, 5, 4, 3, 1, 1, 1]
```



## QB64


```QB64

#lang QB64
'***************************************************
'* BeadSort is VERY fast for small CGSortLibArray(max)-CGSortLibArray(min). Typical performance is
'* O(NlogN) or better. However as the key values (array values and ranges) go up, the performance
'* drops steeply excellent for small-ranged arrays. Integer only at this point.  Throughput is
'* roughly 900k/GHzS for double-precision, with binary range (0,1). Related to CountingSort()
'***************************************************
SUB BeadSort (CGSortLibArray() AS DOUBLE, start AS LONG, finish AS LONG, order&)
    DIM MAX AS DOUBLE: MAX = CGSortLibArray(start)
    DIM BeadSort_Sum AS DOUBLE
    DIM BeadSort_I AS LONG
    DIM BeadSort_J AS LONG
    FOR BeadSort_I = start + 1 TO (finish - start)
        IF (CGSortLibArray(BeadSort_I) > MAX) THEN MAX = CGSortLibArray(BeadSort_I)
    NEXT
    REDIM beads((finish - start), MAX)
    FOR BeadSort_I = 0 TO (finish - start) - 1
        FOR BeadSort_J = 0 TO CGSortLibArray(BeadSort_I) - 1
            beads(BeadSort_I, BeadSort_J) = 1
        NEXT
    NEXT
    IF order& = 1 THEN
        FOR BeadSort_J = 0 TO MAX
            BeadSort_Sum = 0
            FOR BeadSort_I = 0 TO (finish - start)
                BeadSort_Sum = BeadSort_Sum + beads(BeadSort_I, BeadSort_J)
                beads(BeadSort_I, BeadSort_J) = 0
            NEXT
            FOR BeadSort_I = (finish - start) - BeadSort_Sum TO (finish - start)
                beads(BeadSort_I, BeadSort_J) = 1
            NEXT
        NEXT
        FOR BeadSort_I = 0 TO (finish - start)
            BeadSort_J = 0
            WHILE BeadSort_J < MAX AND beads(BeadSort_I, BeadSort_J)
                BeadSort_J = BeadSort_J + 1
            WEND
            CGSortLibArray(BeadSort_I) = BeadSort_J
        NEXT
    ELSE
        FOR BeadSort_J = MAX TO 0 STEP -1
            BeadSort_Sum = 0
            FOR I = 0 TO (finish - start)
                BeadSort_Sum = BeadSort_Sum + beads(I, BeadSort_J)
                beads(I, BeadSort_J) = 0
            NEXT
            FOR I = (finish - start) TO (finish - start) - BeadSort_Sum STEP -1
                beads(I, BeadSort_J) = 1
            NEXT
        NEXT
        FOR BeadSort_I = 0 TO (finish - start)
            BeadSort_J = 0
            WHILE BeadSort_J < MAX AND beads(BeadSort_I, BeadSort_J)
                BeadSort_J = BeadSort_J + 1
            WEND
            CGSortLibArray(finish - BeadSort_I) = BeadSort_J
        NEXT
    END IF
END SUB

```



## Racket

{{trans|Haskell}}

```racket

#lang racket
(require rackunit)

(define (columns lst)
  (match (filter (λ (l) (not (empty? l))) lst)
    ['() '()]
    [l (cons (map car l) (columns (map cdr l)))]))

(define (bead-sort lst)
  (map length (columns (columns (map (λ (n) (make-list n 1)) lst)))))

;; unit test
(check-equal?
 (bead-sort '(5 3 1 7 4 1 1))
 '(7 5 4 3 1 1 1))

```



## REXX

The REXX language has the advantage of supporting sparse arrays, so implementing a bead sort is trivial, the

major drawback is   ''if''   the spread   (difference between the lowest and highest values)   is quite large   (if it's

greater than a few million),   it'll slow up the display   (but not the sorting).

Zero, negative, and duplicate integers (values) can be handled.

```rexx
/*REXX program sorts a list (four groups)  of integers  using the  bead sort  algorithm.*/
                                 /* [↓]  define  two dozen  grasshopper  numbers.       */
gHopper= 1 4 10 12 22 26 30 46 54 62 66 78 94 110 126 134 138 158 162 186 190 222 254 270
                                 /* [↓]  these are also called hexagonal pyramidal #s.  */
greenGrocer=  0 4 16 40 80 140 224 336 480 660 880 1144 1456 1820 2240 2720 3264 3876 4560
                                 /* [↓]  define twenty-three Bernoulli numerator numbers*/
bernN= '1 -1 1 0 -1 0 1 0 -1 0 5 0 -691 0 7 0 -3617 0 43867 0 -174611 0'
                                 /* [↓] also called the Reduced Totient function, and is*/
                                 /*also called Carmichael lambda, or the LAMBDA function*/
psi=      1 1 2 2 4 2 6 2 6 4 10 2 12 6 4 4 16 6 18 4 6 10 22 2 20 12 18 6 28 4 30 8 10 16
y= gHopper greenGrocer bernN psi                 /*combine the four lists into one list.*/
call show  'before sort',  y                     /*display the  list  before sorting.   */
say copies('░', 75)                              /*show long separator line before sort.*/
call show  ' after sort',  beadSort(y)           /*display the  list   after sorting.   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
beadSort: procedure; parse arg low . 1 high . 1 z,$;  @.=0 /*$:  the list to be sorted. */
             do j=1  until z=='';   parse var  z   x  z    /*pick the meat off the bone.*/
             x= x / 1;              @.x= @.x + 1           /*normalize X;  bump counter.*/
             low=min(low, x);       high=max(high, x)      /*track lowest and highest #.*/
             end   /*j*/
                                                           /* [↓] now, collect beads and*/
             do m=low  to high;     do @.m;  $=$ m;  end   /*let them fall (to zero).   */
             end   /*m*/
          return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: parse arg txt,y;            z=words(y);           w=length(z)
                      do k=1  for z
                      say right('element',30)   right(k,w)   txt":"   right( word(y,k), 9)
                      end   /*k*/;              return
```

{{out|output|text=  when using the default input:}}

(Shown at three-quarter size.)
<pre style="font-size:75%;height:90ex">
                       element  1 before sort:         1
                       element  2 before sort:         4
                       element  3 before sort:        10
                       element  4 before sort:        12
                       element  5 before sort:        22
                       element  6 before sort:        26
                       element  7 before sort:        30
                       element  8 before sort:        46
                       element  9 before sort:        54
                       element 10 before sort:        62
                       element 11 before sort:        66
                       element 12 before sort:        78
                       element 13 before sort:        94
                       element 14 before sort:       110
                       element 15 before sort:       126
                       element 16 before sort:       134
                       element 17 before sort:       138
                       element 18 before sort:       158
                       element 19 before sort:       162
                       element 20 before sort:       186
                       element 21 before sort:       190
                       element 22 before sort:       222
                       element 23 before sort:       254
                       element 24 before sort:       270
                       element 25 before sort:         0
                       element 26 before sort:         4
                       element 27 before sort:        16
                       element 28 before sort:        40
                       element 29 before sort:        80
                       element 30 before sort:       140
                       element 31 before sort:       224
                       element 32 before sort:       336
                       element 33 before sort:       480
                       element 34 before sort:       660
                       element 35 before sort:       880
                       element 36 before sort:      1144
                       element 37 before sort:      1456
                       element 38 before sort:      1820
                       element 39 before sort:      2240
                       element 40 before sort:      2720
                       element 41 before sort:      3264
                       element 42 before sort:      3876
                       element 43 before sort:      4560
                       element 44 before sort:         1
                       element 45 before sort:        -1
                       element 46 before sort:         1
                       element 47 before sort:         0
                       element 48 before sort:        -1
                       element 49 before sort:         0
                       element 50 before sort:         1
                       element 51 before sort:         0
                       element 52 before sort:        -1
                       element 53 before sort:         0
                       element 54 before sort:         5
                       element 55 before sort:         0
                       element 56 before sort:      -691
                       element 57 before sort:         0
                       element 58 before sort:         7
                       element 59 before sort:         0
                       element 60 before sort:     -3617
                       element 61 before sort:         0
                       element 62 before sort:     43867
                       element 63 before sort:         0
                       element 64 before sort:   -174611
                       element 65 before sort:         0
                       element 66 before sort:         1
                       element 67 before sort:         1
                       element 68 before sort:         2
                       element 69 before sort:         2
                       element 70 before sort:         4
                       element 71 before sort:         2
                       element 72 before sort:         6
                       element 73 before sort:         2
                       element 74 before sort:         6
                       element 75 before sort:         4
                       element 76 before sort:        10
                       element 77 before sort:         2
                       element 78 before sort:        12
                       element 79 before sort:         6
                       element 80 before sort:         4
                       element 81 before sort:         4
                       element 82 before sort:        16
                       element 83 before sort:         6
                       element 84 before sort:        18
                       element 85 before sort:         4
                       element 86 before sort:         6
                       element 87 before sort:        10
                       element 88 before sort:        22
                       element 89 before sort:         2
                       element 90 before sort:        20
                       element 91 before sort:        12
                       element 92 before sort:        18
                       element 93 before sort:         6
                       element 94 before sort:        28
                       element 95 before sort:         4
                       element 96 before sort:        30
                       element 97 before sort:         8
                       element 98 before sort:        10
                       element 99 before sort:        16
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
                       element  1  after sort:   -174611
                       element  2  after sort:     -3617
                       element  3  after sort:      -691
                       element  4  after sort:        -1
                       element  5  after sort:        -1
                       element  6  after sort:        -1
                       element  7  after sort:         0
                       element  8  after sort:         0
                       element  9  after sort:         0
                       element 10  after sort:         0
                       element 11  after sort:         0
                       element 12  after sort:         0
                       element 13  after sort:         0
                       element 14  after sort:         0
                       element 15  after sort:         0
                       element 16  after sort:         0
                       element 17  after sort:         0
                       element 18  after sort:         1
                       element 19  after sort:         1
                       element 20  after sort:         1
                       element 21  after sort:         1
                       element 22  after sort:         1
                       element 23  after sort:         1
                       element 24  after sort:         2
                       element 25  after sort:         2
                       element 26  after sort:         2
                       element 27  after sort:         2
                       element 28  after sort:         2
                       element 29  after sort:         2
                       element 30  after sort:         4
                       element 31  after sort:         4
                       element 32  after sort:         4
                       element 33  after sort:         4
                       element 34  after sort:         4
                       element 35  after sort:         4
                       element 36  after sort:         4
                       element 37  after sort:         4
                       element 38  after sort:         5
                       element 39  after sort:         6
                       element 40  after sort:         6
                       element 41  after sort:         6
                       element 42  after sort:         6
                       element 43  after sort:         6
                       element 44  after sort:         6
                       element 45  after sort:         7
                       element 46  after sort:         8
                       element 47  after sort:        10
                       element 48  after sort:        10
                       element 49  after sort:        10
                       element 50  after sort:        10
                       element 51  after sort:        12
                       element 52  after sort:        12
                       element 53  after sort:        12
                       element 54  after sort:        16
                       element 55  after sort:        16
                       element 56  after sort:        16
                       element 57  after sort:        18
                       element 58  after sort:        18
                       element 59  after sort:        20
                       element 60  after sort:        22
                       element 61  after sort:        22
                       element 62  after sort:        26
                       element 63  after sort:        28
                       element 64  after sort:        30
                       element 65  after sort:        30
                       element 66  after sort:        40
                       element 67  after sort:        46
                       element 68  after sort:        54
                       element 69  after sort:        62
                       element 70  after sort:        66
                       element 71  after sort:        78
                       element 72  after sort:        80
                       element 73  after sort:        94
                       element 74  after sort:       110
                       element 75  after sort:       126
                       element 76  after sort:       134
                       element 77  after sort:       138
                       element 78  after sort:       140
                       element 79  after sort:       158
                       element 80  after sort:       162
                       element 81  after sort:       186
                       element 82  after sort:       190
                       element 83  after sort:       222
                       element 84  after sort:       224
                       element 85  after sort:       254
                       element 86  after sort:       270
                       element 87  after sort:       336
                       element 88  after sort:       480
                       element 89  after sort:       660
                       element 90  after sort:       880
                       element 91  after sort:      1144
                       element 92  after sort:      1456
                       element 93  after sort:      1820
                       element 94  after sort:      2240
                       element 95  after sort:      2720
                       element 96  after sort:      3264
                       element 97  after sort:      3876
                       element 98  after sort:      4560
                       element 99  after sort:     43867

```



## Ruby

{{trans|Haskell}}

```ruby
class Array
  def beadsort
    map {|e| [1] * e}.columns.columns.map(&:length)
  end

  def columns
    y = length
    x = map(&:length).max
    Array.new(x) do |row|
      Array.new(y) { |column| self[column][row] }.compact # Remove nils.
    end
  end
end

# Demonstration code:
p [5,3,1,7,4,1,1].beadsort
```


{{out}}

```txt
[7, 5, 4, 3, 1, 1, 1]
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: beadSort (inout array integer: a) is func
  local
    var integer: max is 0;
    var integer: sum is 0;
    var array bitset: beads is 0 times {};
    var integer: i is 0;
    var integer: j is 0;
  begin
    beads := length(a) times {};
    for i range 1 to length(a) do
      if a[i] > max then
        max := a[i];
      end if;
      beads[i] := {1 .. a[i]};
    end for;
    for j range 1 to max do
      sum := 0;
      for i range 1 to length(a) do
        sum +:= ord(j in beads[i]);
        excl(beads[i], j);
      end for;
      for i range length(a) - sum + 1 to length(a) do
        incl(beads[i], j);
      end for;
    end for;
    for i range 1 to length(a) do
      for j range 1 to max until j not in beads[i] do
        noop;
      end for;
      a[i] := pred(j);
    end for;
  end func;

const proc: main is func
  local
    var array integer: a is [] (5, 3, 1, 7, 4, 1, 1, 20);
    var integer: num is 0;
  begin
    beadSort(a);
    for num range a do
      write(num <& " ");
    end for;
    writeln;
  end func;
```


{{out}}

```txt

1 1 1 3 4 5 7 20

```



## Sidef

{{trans|Perl}}

```ruby
func beadsort(arr) {

    var rows = []
    var columns = []

    for datum in arr {
        for column in ^datum {
            ++(columns[column] := 0)
            ++(rows[columns[column] - 1] := 0)
        }
    }

    rows.reverse
}

say beadsort([5,3,1,7,4,1,1])
```


{{out}}

```txt

[1, 1, 1, 3, 4, 5, 7]

```



## Standard ML

{{trans|Haskell}}

```sml
fun columns l =
  case List.filter (not o null) l of
    [] => []
  | l => map hd l :: columns (map tl l)

fun replicate (n, x) = List.tabulate (n, fn _ => x)

fun bead_sort l =
  map length (columns (columns (map (fn e => replicate (e, 1)) l)))
```

usage

```txt

- bead_sort [5,3,1,7,4,1,1];
val it = [7,5,4,3,1,1,1] : int list

```



## Tcl


```tcl
package require Tcl 8.5

proc beadsort numList {
    # Special case: empty list is empty when sorted.
    if {![llength $numList]} return
    # Set up the abacus...
    foreach n $numList {
	for {set i 0} {$i<$n} {incr i} {
	    dict incr vals $i
	}
    }
    # Make the beads fall...
    foreach n [dict values $vals] {
	for {set i 0} {$i<$n} {incr i} {
	    dict incr result $i
	}
    }
    # And the result is...
    dict values $result
}

# Demonstration code
puts [beadsort {5 3 1 7 4 1 1}]
```

{{out}}

```txt
7 5 4 3 1 1 1
```



## VBA

{{trans|Phix}}
```vb
Option Base 1

Private Function sq_add(arr As Variant, x As Double) As Variant
    Dim res() As Variant
    ReDim res(UBound(arr))
    For i = 1 To UBound(arr)
        res(i) = arr(i) + x
    Next i
    sq_add = res
End Function

Private Function beadsort(ByVal a As Variant) As Variant
    Dim poles() As Variant
    ReDim poles(WorksheetFunction.Max(a))
    For i = 1 To UBound(a)
        For j = 1 To a(i)
            poles(j) = poles(j) + 1
        Next j
    Next i
    For j = 1 To UBound(a)
        a(j) = 0
    Next j
    For i = 1 To UBound(poles)
        For j = 1 To poles(i)
            a(j) = a(j) + 1
        Next j
    Next i
    beadsort = a
End Function

Public Sub main()
    Debug.Print Join(beadsort([{5, 3, 1, 7, 4, 1, 1, 20}]), ", ")
End Sub
```
{{out}}

```txt
20, 7, 5, 4, 3, 1, 1, 1
```


## XPL0


```XPL0
include c:\cxpl\codes;

proc BeadSort(Array, Length);   \Sort Array into increasing order
int  Array, Length;             \Array contents range 0..31; number of items
int  Row, I, J, T, C;
[Row:= Reserve(Length*4);       \each Row has room for 32 beads
for I:= 0 to Length-1 do        \each Row gets Array(I) number of beads
    Row(I):= ~-1<<Array(I);     \(beware for 80186..Pentium <<32 doesn't shift)
for J:= 1 to Length-1 do
    for I:= Length-1 downto J do
        [T:= Row(I-1) & ~Row(I); \up to 31 beads fall in a single pass
        Row(I-1):= Row(I-1) | T; \(|=xor, !=or)
        Row(I):= Row(I) | T;
        ];
for I:= 0 to Length-1 do        \count beads in each Row
    [C:= 0;  T:= Row(I);
    while T do
        [if T&1 then C:= C+1;  T:= T>>1];
    Array(I):= C;               \count provides sorted order
    ];
];

int A, I;
[A:= [3, 1, 4, 1, 25, 9, 2, 6, 5, 0];
BeadSort(A, 10);
for I:= 0 to 10-1 do [IntOut(0, A(I));  ChOut(0, ^ )];
]
```


{{out}}

```txt

0 1 1 2 3 4 5 6 9 25

```



## zkl

{{trans|D}}

```zkl
fcn columns(m){ // m is list of lists of zeros/beads, # beads is n, eg (0,0,0)==3
   m
   .apply("len")   // (0,0,0)-->3
   .reduce("max")  // largest bead stack
   .walker()       // [0..max]
   .apply('wrap(i){ m.filter('wrap(s){ s.len() > i }).len().pump(List,0) });
}

fcn beadSort(data){
   data.apply("pump",List,0):columns(_):columns(_).apply("len");
}
```


```zkl
T(5,3,1,7,4,1,1):beadSort(_).println();
T(4,3,3,2,1):beadSort(_).println();
```

{{out}}

```txt

L(7,5,4,3,1,1,1)
L(4,3,3,2,1)

```



{{omit from|GUISS}}
