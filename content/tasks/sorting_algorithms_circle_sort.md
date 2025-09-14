+++
title = "Sorting Algorithms/Circle Sort"
description = ""
date = 2019-01-05T17:55:43Z
aliases = []
[extra]
id = 18460
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "coffeescript",
  "cpp",
  "d",
  "elixir",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "nim",
  "objeck",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "ubasic_4th",
  "zkl",
  "zx_spectrum_basic",
]
+++

Sort an array of integers (of any convenient size) into ascending order using Circlesort.

In short, compare the first element to the last element, then the second element to the second last element, etc.

Then split the array in two and recurse until there is only one single element in the array, like this:
 Before:
 '''6''' ''7'' 8 9 2 5 3 ''4'' '''1'''
 After:
 '''1''' ''4'' 3 5 2 9 8 ''7'' '''6'''

Repeat this procedure until quiescence (i.e. until there are no swaps).

Show both the initial, unsorted list and the final sorted list. (Intermediate steps during sorting are optional.)

Optimizations (like doing ''0.5 log2(n)'' iterations and then continue with an [[Insertion sort]]) are optional.


Pseudo code:

  '''function''' circlesort (index lo, index hi, swaps)
  {
    '''if''' lo == hi '''return''' (swaps)
    high ''':=''' hi
    low ''':=''' lo
    mid ''':=''' int((hi-lo)/2)
    '''while''' lo < hi {
      '''if'''  (value at lo) > (value at hi) {
         '''swap.values''' (lo,hi)
         swaps++
      }
      lo++
      hi--
    }
    '''if''' lo == hi
      '''if''' (value at lo) > (value at hi+1) {
          '''swap.values''' (lo,hi+1)
          swaps++
      }
    swaps ''':=''' circlesort(low,low+mid,swaps)
    swaps ''':=''' circlesort(low+mid+1,high,swaps)
    '''return'''(swaps)
  }
  '''while''' circlesort (0, '''sizeof'''(array)-1, 0)


## See also

* For more information on Circle sorting, see [http://sourceforge.net/p/forth-4th/wiki/Circle%20sort/ Sourceforge].





## C


```c
#include <stdio.h>

int circle_sort_inner(int *start, int *end)
{
	int *p, *q, t, swapped;

	if (start == end) return 0;

	// funny "||" on next line is for the center element of odd-lengthed array
	for (swapped = 0, p = start, q = end; p<q || (p==q && ++q); p++, q--)
		if (*p > *q)
			t = *p, *p = *q, *q = t, swapped = 1;

	// q == p-1 at this point
	return swapped | circle_sort_inner(start, q) | circle_sort_inner(p, end);
}

//helper function to show arrays before each call
void circle_sort(int *x, int n)
{
	do {
		int i;
		for (i = 0; i < n; i++) printf("%d ", x[i]);
		putchar('\n');
	} while (circle_sort_inner(x, x + (n - 1)));
}

int main(void)
{
	int x[] = {5, -1, 101, -4, 0, 1, 8, 6, 2, 3};
	circle_sort(x, sizeof(x) / sizeof(*x));

	return 0;
}
```

```txt

5 -1 101 -4 0 1 8 6 2 3
-4 -1 0 3 6 1 2 8 5 101
-4 -1 0 1 2 3 5 6 8 101

```



## C++


```cpp
#include <iostream>

int circlesort(int* arr, int lo, int hi, int swaps) {
    if(lo == hi) {
        return swaps;
    }
    int high = hi;
    int low = lo;
    int mid = (high - low) / 2;
    while(lo < hi) {
        if(arr[lo] > arr[hi]) {
            int temp = arr[lo];
            arr[lo] = arr[hi];
            arr[hi] = temp;
            swaps++;
        }
        lo++;
        hi--;
    }

    if(lo == hi) {
        if(arr[lo] > arr[hi+1]) {
            int temp = arr[lo];
            arr[lo] = arr[hi+1];
            arr[hi+1] = temp;
            swaps++;
        }
    }
    swaps = circlesort(arr, low, low+mid, swaps);
    swaps = circlesort(arr, low+mid+1, high, swaps);
    return swaps;
}

void circlesortDriver(int* arr, int n) {
    do {
        for(int i = 0; i < n; i++) {
            std::cout << arr[i] << ' ';
        }
        std::cout << std::endl;
    } while(circlesort(arr, 0, n-1, 0));
}

int main() {
    int arr[] = { 6, 7, 8, 9, 2, 5, 3, 4, 1 };
    circlesortDriver(arr, sizeof(arr)/sizeof(int));
    return 0;
}
```

Output:

```txt
6 7 8 9 2 5 3 4 1
1 3 4 2 5 6 7 8 9
1 2 3 4 5 6 7 8 9
```



## CoffeeScript

<lang>circlesort = (arr, lo, hi, swaps) ->
  if lo == hi
     return (swaps)

  high = hi
  low  = lo
  mid = Math.floor((hi-lo)/2)

  while lo < hi
    if arr[lo] > arr[hi]
       t = arr[lo]
       arr[lo] = arr[hi]
       arr[hi] = t
       swaps++
    lo++
    hi--

  if lo == hi
     if arr[lo] > arr[hi+1]
        t = arr[lo]
        arr[lo] = arr[hi+1]
        arr[hi+1] = t
        swaps++

  swaps = circlesort(arr,low,low+mid,swaps)
  swaps = circlesort(arr,low+mid+1,high,swaps)

  return(swaps)

VA = [2,14,4,6,8,1,3,5,7,9,10,11,0,13,12,-1]

while circlesort(VA,0,VA.length-1,0)
   console.log VA
```

Output:

```txt
console: -1,1,0,3,4,5,8,12,2,9,6,10,7,13,11,14
console: -1,0,1,3,2,5,4,8,6,7,9,12,10,11,13,14
console: -1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14
```



## D


```d
import std.stdio, std.algorithm, std.array, std.traits;

void circlesort(T)(T[] items) if (isMutable!T) {
    uint inner(size_t lo, size_t hi, uint swaps) {
        if (lo == hi)
            return swaps;
        auto high = hi;
        auto low = lo;
        immutable mid = (hi - lo) / 2;

        while (lo < hi) {
            if (items[lo] > items[hi]) {
                swap(items[lo], items[hi]);
                swaps++;
            }
            lo++;
            hi--;
        }

        if (lo == hi && items[lo] > items[hi + 1]) {
            swap(items[lo], items[hi + 1]);
            swaps++;
        }
        swaps = inner(low, low + mid, swaps);
        swaps = inner(low + mid + 1, high, swaps);
        return swaps;
    }

    if (!items.empty)
        while (inner(0, items.length - 1, 0)) {}
}

void main() {
    import std.random, std.conv;

    auto a = [5, -1, 101, -4, 0, 1, 8, 6, 2, 3];
    a.circlesort;
    a.writeln;
    assert(a.isSorted);

    // Fuzzy test.
    int[30] items;
    foreach (immutable _; 0 .. 100_000) {
        auto data = items[0 .. uniform(0, items.length)];
        foreach (ref x; data)
            x = uniform(-items.length.signed * 3, items.length.signed * 3);
        data.circlesort;
        assert(data.isSorted);
    }
}
```

```txt
[-4, -1, 0, 1, 2, 3, 5, 6, 8, 101]
```



## Elixir


```elixir
defmodule Sort do
  def circle_sort(data) do
    List.to_tuple(data)
    |> circle_sort(0, length(data)-1)
    |> Tuple.to_list
  end

  defp circle_sort(data, lo, hi) do
    case circle_sort(data, lo, hi, 0) do
      {result, 0} -> result
      {result, _} -> circle_sort(result, lo, hi)
    end
  end

  defp circle_sort(data, lo, lo, swaps), do: {data, swaps}
  defp circle_sort(data, lo, hi, swaps) do
    mid = div(lo + hi, 2)
    {data, swaps} = do_circle_sort(data, lo, hi, swaps)
    {data, swaps} = circle_sort(data, lo, mid, swaps)
    circle_sort(data, mid+1, hi, swaps)
  end

  def do_circle_sort(data, lo, hi, swaps) when lo>=hi do
    if lo==hi and elem(data, lo) > elem(data, hi+1),
      do:   {swap(data, lo, hi+1), swaps+1},
      else: {data, swaps}
  end
  def do_circle_sort(data, lo, hi, swaps) do
    if elem(data, lo) > elem(data, hi),
      do:   do_circle_sort(swap(data, lo, hi), lo+1, hi-1, swaps+1),
      else: do_circle_sort(data, lo+1, hi-1, swaps)
  end

  defp swap(data, i, j) do
    vi = elem(data, i)
    vj = elem(data, j)
    data |> put_elem(i, vj) |> put_elem(j, vi)
  end
end

data = [6, 7, 8, 9, 2, 5, 3, 4, 1]
IO.puts "before sort: #{inspect data}"
IO.puts " after sort: #{inspect Sort.circle_sort(data)}"
```


```txt

before sort: [6, 7, 8, 9, 2, 5, 3, 4, 1]
 after sort: [1, 2, 3, 4, 5, 6, 7, 8, 9]

```



## Forth

This one features the newest version of the algorithm on [http://sourceforge.net/p/forth-4th/wiki/Circle%20sort/ Sourceforge].
<lang>[UNDEFINED] cell- [IF] : cell- 1 cells - ; [THEN]

defer precedes                         ( addr addr -- flag )
variable (sorted?)                     \ is the array sorted?

: (compare)                            ( a1 a2 -- a1 a2)
  over @ over @ precedes               \ flag if swapped
  if over over over @ over @ swap rot ! swap ! false (sorted?) ! then
;

: (circlesort)                         ( a1 a2 --)
  over over = if drop drop exit then   \ quit if indexes are equal
  over over swap                       \ swap indexes (end begin)
  begin
    over over >                        \ as long as middle isn't passed
  while
    (compare) swap cell- swap cell+    \ check and swap opposite elements
  repeat rot recurse recurse           \ split array and recurse
;

: sort                                 ( a n --)
  1- cells over +                      \ calculate addresses
  begin true (sorted?) ! over over (circlesort) (sorted?) @ until drop drop
;

:noname < ; is precedes

10 constant /sample
create sample 5 , -1 , 101 , -4 , 0 , 1 , 8 , 6 , 2 , 3 ,

: .sample sample /sample cells bounds do i ? 1 cells +loop ;

sample /sample sort .sample
```


## Fortran


```fortran

!
module circlesort
! I have commented the code that was here and also 'tightened up' various pieces such as how swap detection was done as well
! as fixing an error where the code would exceed array bounds for odd number sized arrays.
! Also, giving some some attribution to the author. - Pete
! This code is a Fortran adaptation of a Forth algorithm laid out by "thebeez" at this URL;
! https://sourceforge.net/p/forth-4th/wiki/Circle%20sort/
!
  implicit none
  logical, private :: csr
  public :: circle_sort

contains

  recursive logical function csr(a, left, right,n) result(swapped)
    implicit none
    integer, intent(in) :: left, right,n
    integer, intent(inout) :: a(n)
    integer :: lo, hi, mid
    integer :: temp
    logical :: lefthalf,righthalf
!
    swapped = .FALSE.
    if (right <= left) return
    lo = left   !Store the upper and lower bounds of list for
    hi = right  !Recursion later
!
    do while (lo < hi)
!   Swap the pair of elements if hi < lo
       if (a(hi) < a(lo)) then
          swapped = .TRUE.
          temp = a(lo)
          a(lo) = a(hi)
          a(hi) = temp
       endif
       lo = lo + 1
       hi = hi - 1
    end do
!   Special case if array is an odd size (not even)
    if (lo == hi)then
       if(a(hi+1) .lt. a(lo))then
           swapped = .TRUE.
           temp = a(hi+1)
           a(hi+1) = a(lo)
           a(lo) = temp
       endif
    endif
    mid = (left + right) / 2 ! Bisection point
    lefthalf = csr(a, left, mid,n)
    righthalf = csr(a, mid + 1, right,n)
    swapped = swapped .or. lefthalf .or. righthalf
  end function csr
!
  subroutine circle_sort(a, n)
    use iso_c_binding, only: c_ptr, c_loc
    implicit none
    integer, intent(in) :: n
    integer, target,intent(inout) :: a(n)

    do while ( csr(a, 1, n,n))
! This is the canonical algorithm. However, if you want to
! speed it up, count the iterations and when you have approached
! 0.5*ln(n) iterations, perform a binary insertion sort then exit the loop.
    end do
  end subroutine circle_sort

end module circlesort
program sort
  use circlesort
  implicit none
  integer :: a(9)
  data a/6,7,8,9,2,5,3,4,1/
  call circle_sort(a, size(a))
  print *, a
end program sort


```



## FreeBASIC


```freebasic
' version 21-10-2016
' compile with: fbc -s console
' for boundry checks on array's compile with: fbc -s console -exx
' converted pseudo code into FreeBASIC code

' shared variables need to be declared before first use
Dim Shared As Long cs(-7 To 7)

Function circlesort(lo As Long, hi As Long, swaps As ULong) As ULong

    ' array is declared shared
    ' sort from lower bound to the highter bound
    ' array's can have subscript range from -2147483648 to +2147483647

    If lo = hi Then Return swaps

    Dim As Long high = hi
    Dim As Long low = lo
    Dim As Long mid_ = (hi - lo) \ 2

    While lo < hi
        If cs(lo) > cs(hi) Then
            Swap cs(lo), cs(hi)
            swaps += 1
        End If
        lo += 1
        hi -= 1
    Wend
    If lo = hi Then
        If cs(lo) > cs(hi +1) Then
            Swap cs(lo), cs(hi +1)
            swaps += 1
        End If
    End If
    swaps = circlesort(low          , low + mid_, swaps)
    swaps = circlesort(low + mid_ +1,       high, swaps)

    Return swaps

End Function

' ------=< MAIN >=------

Dim As Long i, a = LBound(cs), b = UBound(cs)

Randomize Timer
For i = a To b : cs(i) = i  : Next
For i = a To b ' little shuffle
    Swap cs(i), cs(Int(Rnd * (b - a +1)) + a)
Next

Print "unsorted ";
For i = a To b : Print Using "####"; cs(i); : Next : Print

' sort the array, loop until sorted
While circlesort(a, b, 0) : Wend

Print "  sorted ";
For i = a To b : Print Using "####"; cs(i); : Next : Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
unsorted   -4  -1   1   0   5  -7  -2   4  -6  -3   2   6   3   7  -5
  sorted   -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
```



## Go


```go
package main

import "fmt"

func circleSort(a []int, lo, hi, swaps int) int {
    if lo == hi {
        return swaps
    }
    high, low := hi, lo
    mid := (hi - lo) / 2
    for lo < hi {
        if a[lo] > a[hi] {
            a[lo], a[hi] = a[hi], a[lo]
            swaps++
        }
        lo++
        hi--
    }
    if lo == hi {
        if a[lo] > a[hi+1] {
            a[lo], a[hi+1] = a[hi+1], a[lo]
            swaps++
        }
    }
    swaps = circleSort(a, low, low+mid, swaps)
    swaps = circleSort(a, low+mid+1, high, swaps)
    return swaps
}

func main() {
    aa := [][]int{
        {6, 7, 8, 9, 2, 5, 3, 4, 1},
        {2, 14, 4, 6, 8, 1, 3, 5, 7, 11, 0, 13, 12, -1},
    }
    for _, a := range aa {
        fmt.Printf("Original: %v\n", a)
        for circleSort(a, 0, len(a)-1, 0) != 0 {
            // empty block
        }
        fmt.Printf("Sorted  : %v\n\n", a)
    }
}
```


```txt

Original: [6 7 8 9 2 5 3 4 1]
Sorted  : [1 2 3 4 5 6 7 8 9]

Original: [2 14 4 6 8 1 3 5 7 11 0 13 12 -1]
Sorted  : [-1 0 1 2 3 4 5 6 7 8 11 12 13 14]

```



## J

Of more parsing and atomic data, or less parsing with large data groups the latter produces faster J programs.  Consequently each iteration laminates the original with its reverse.  It joins the recursive call to the pairwise minima of the left block to the recursive call of the pairwise maxima of the right block, repeating the operations while the output changes.  This is sufficient for power of 2 length data.  The pre verb adjusts the data length.  And post recovers the original data.  This implementation discards the "in place" property described at the sourceforge link.


```J

circle_sort =: post  power_of_2_length@pre    NB. the main sorting verb
power_of_2_length =: even_length_iteration^:_ NB. repeat while the answer changes
even_length_iteration =: (<./ (,&$: |.) >./)@(-:@# ({. ,: |.@}.) ])^:(1<#)
pre =: ,   (-~ >.&.(2&^.))@# # >./            NB. extend data to next power of 2 length
post =: ({.~ #)~                              NB. remove the extra data

```

Examples:

```J

   show =: [ smoutput

   8 ([: circle_sort&.>@show ;&(?~)) 13  NB. sort lists length 8 and 13
┌───────────────┬────────────────────────────┐
│0 6 7 3 4 5 2 1│3 10 1 4 7 8 5 6 2 0 9 11 12│
└───────────────┴────────────────────────────┘
┌───────────────┬────────────────────────────┐
│0 1 2 3 4 5 6 7│0 1 2 3 4 5 6 7 8 9 10 11 12│
└───────────────┴────────────────────────────┘

   8 ([: circle_sort&.>@show ;&(1 }. 2 # ?~)) 13  NB. data has repetition
┌─────────────────────────────┬──────────────────────────────────────────────────────┐
│2 3 3 5 5 1 1 7 7 6 6 4 4 0 0│12 11 11 4 4 3 3 9 9 7 7 10 10 6 6 2 2 1 1 5 5 8 8 0 0│
└─────────────────────────────┴──────────────────────────────────────────────────────┘
┌─────────────────────────────┬──────────────────────────────────────────────────────┐
│0 0 1 1 2 3 3 4 4 5 5 6 6 7 7│0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12│
└─────────────────────────────┴──────────────────────────────────────────────────────┘

```



## Java


```java
import java.util.Arrays;

public class CircleSort {

    public static void main(String[] args) {
        circleSort(new int[]{2, 14, 4, 6, 8, 1, 3, 5, 7, 11, 0, 13, 12, -1});
    }

    public static void circleSort(int[] arr) {
        if (arr.length > 0)
            do {
                System.out.println(Arrays.toString(arr));
            } while (circleSortR(arr, 0, arr.length - 1, 0) != 0);
    }

    private static int circleSortR(int[] arr, int lo, int hi, int numSwaps) {
        if (lo == hi)
            return numSwaps;

        int high = hi;
        int low = lo;
        int mid = (hi - lo) / 2;

        while (lo < hi) {
            if (arr[lo] > arr[hi]) {
                swap(arr, lo, hi);
                numSwaps++;
            }
            lo++;
            hi--;
        }

        if (lo == hi && arr[lo] > arr[hi + 1]) {
            swap(arr, lo, hi + 1);
            numSwaps++;
        }

        numSwaps = circleSortR(arr, low, low + mid, numSwaps);
        numSwaps = circleSortR(arr, low + mid + 1, high, numSwaps);

        return numSwaps;
    }

    private static void swap(int[] arr, int idx1, int idx2) {
        int tmp = arr[idx1];
        arr[idx1] = arr[idx2];
        arr[idx2] = tmp;
    }
}
```



```txt
[2, 14, 4, 6, 8, 1, 3, 5, 7, 11, 0, 13, 12, -1]
[-1, 1, 0, 4, 3, 8, 12, 2, 7, 6, 11, 5, 13, 14]
[-1, 0, 1, 3, 2, 4, 7, 5, 6, 8, 12, 11, 13, 14]
[-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 13, 14]
```



## jq

With kudos to [[#Perl 6]].

"circlesort" as defined in this section can be used to sort any JSON array.  In case your jq does not have "until" as a builtin, here is its definition:

```jq
def until(cond; next):
     def _until: if cond then . else (next|_until) end;
     _until;
```


```jq
def circlesort:

  def swap(i;j): .[i] as $t | .[i] = .[j] | .[j] = $t;

  # state: [lo, hi, swaps, array]
  def cs:

    # increment lo, decrement hi, and if they are equal, increment hi again
    # i.e. ++hi if --hi == $lo
    def next: # [lo, hi]
      .[0] += 1 | .[1] -= 1 | (if .[0] == .[1] then .[1] += 1 else . end) ;

    .[0] as $start | .[1] as $stop
    | if $start < $stop then
        until(.[0] >= .[1];
	      .[0] as $lo | .[1] as $hi | .[3] as $array
              | if $array[$lo] > $array[$hi] then
		      .[3] = ($array | swap($lo; $hi))
                    | .[2] += 1         # swaps++
                else .
                end
	      | next)
        | .[0] as $lo | .[1] as $hi
        | [$start, $hi, .[2], .[3]] | cs
	| [$lo, $stop,  .[2], .[3]] | cs
      else .
      end ;

   [0, length-1, 0, .] | cs
   | .[2] as $swaps
   | .[3]
   | if $swaps == 0 then .
      else circlesort
      end ;
```

'''Example:'''

```jq
"The quick brown fox jumps over the lazy dog" | split(" ") | circlesort
```


```sh
$ jq -n -c -f -M circleSort.jq
["The","brown","dog","fox","jumps","lazy","over","quick","the"]
```



## Julia

```julia
function _ciclesort!(arr::Vector, lo::Int, hi::Int, swaps::Int)
    lo == hi && return swaps
    high = hi
    low  = lo
    mid  = (hi - lo) ÷ 2
    while lo < hi
        if arr[lo] > arr[hi]
            arr[lo], arr[hi] = arr[hi], arr[lo]
            swaps += 1
        end
        lo += 1
        hi -= 1
    end
    if lo == hi
        if arr[lo] > arr[hi+1]
            arr[lo], arr[hi+1] = arr[hi+1], arr[lo]
            swaps += 1
        end
    end
    swaps = _ciclesort!(arr, low, low + mid, swaps)
    swaps = _ciclesort!(arr, low + mid + 1, high, swaps)
    return swaps
end

function ciclesort!(arr::Vector)
    while !iszero(_ciclesort!(arr, 1, endof(arr), 0)) end
    return arr
end

v = rand(10)
println("# $v\n -> ", ciclesort!(v))
```


```txt
# [0.603704, 0.293639, 0.51395, 0.74624, 0.245282, 0.930508, 0.550865, 0.62253, 0.00608894, 0.270426]
 -> [0.00608894, 0.245282, 0.270426, 0.293639, 0.51395, 0.550865, 0.603704, 0.62253, 0.74624, 0.930508]
```



## Kotlin


```scala
// version 1.1.0

fun<T: Comparable<T>> circleSort(array: Array<T>, lo: Int, hi: Int, nSwaps: Int): Int {
    if (lo == hi) return nSwaps

    fun swap(array: Array<T>, i: Int, j: Int) {
        val temp  = array[i]
        array[i]  = array[j]
        array[j]  = temp
    }

    var high  = hi
    var low   = lo
    val mid   = (hi - lo) / 2
    var swaps = nSwaps
    while (low < high) {
        if (array[low] > array[high]) {
            swap(array, low, high)
            swaps++
        }
        low++
        high--
    }
    if (low == high)
        if (array[low] > array[high + 1]) {
            swap(array, low, high + 1)
            swaps++
        }
    swaps = circleSort(array, lo, lo + mid, swaps)
    swaps = circleSort(array, lo + mid + 1, hi, swaps)
    return swaps
}

fun main(args: Array<String>) {
    val array = arrayOf(6, 7, 8, 9, 2, 5, 3, 4, 1)
    println("Original: ${array.asList()}")
    while (circleSort(array, 0, array.size - 1, 0) != 0) ; // empty statement
    println("Sorted  : ${array.asList()}")
    println()
    val array2 = arrayOf("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog")
    println("Original: ${array2.asList()}")
    while (circleSort(array2, 0, array2.size - 1, 0) != 0) ;
    println("Sorted  : ${array2.asList()}")
}
```


```txt

Original: [6, 7, 8, 9, 2, 5, 3, 4, 1]
Sorted  : [1, 2, 3, 4, 5, 6, 7, 8, 9]

Original: [the, quick, brown, fox, jumps, over, the, lazy, dog]
Sorted  : [brown, dog, fox, jumps, lazy, over, quick, the, the]

```



## Lua

The first argument to the 'inner' function needs to be a reference to the table as Lua cannot use a pointer to the first element's memory address.  Conversely the 'outer' function only needs one argument as the size of the table is innately knowable.

```Lua
-- Perform one iteration of a circle sort
function innerCircle (t, lo, hi, swaps)
  if lo == hi then return swaps end
  local high, low, mid = hi, lo, math.floor((hi - lo) / 2)
  while lo < hi do
    if t[lo] > t[hi] then
      t[lo], t[hi] = t[hi], t[lo]
      swaps = swaps + 1
    end
    lo = lo + 1
    hi = hi - 1
  end
  if lo == hi then
    if t[lo] > t[hi + 1] then
      t[lo], t[hi + 1] = t[hi + 1], t[lo]
      swaps = swaps + 1
    end
  end
  swaps = innerCircle(t, low, low + mid, swaps)
  swaps = innerCircle(t, low + mid + 1, high, swaps)
  return swaps
end

-- Keep sorting the table until an iteration makes no swaps
function circleSort (t)
  while innerCircle(t, 1, #t, 0) > 0 do end
end

-- Main procedure
local array = {6, 7, 8, 9, 2, 5, 3, 4, 1}
circleSort(array)
print(table.concat(array, " "))
```

```txt
1 2 3 4 5 6 7 8 9
```


## Nim


```nim
proc innerCircleSort[T](a: var openArray[T], lo, hi, swaps: int): int =
  var localSwaps: int = swaps
  var localHi: int = hi
  var localLo: int = lo
  if localLo == localHi:
    return swaps

  var `high` = localHi
  var `low` = localLo
  var mid = (localHi - localLo) div 2

  while localLo < localHi:
    if a[localLo] > a[localHi]:
      swap a[localLo], a[localHi]
      inc localSwaps
    inc localLo
    dec localHi
  if localLo == localHi:
    if a[localLo] > a[localHi + 1]:
      swap a[localLo], a[localHi + 1]
      inc localSwaps

  localswaps = a.innerCircleSort(`low`, `low` + mid, localSwaps)
  localSwaps = a.innerCircleSort(`low` + mid + 1, `high`, localSwaps)
  result = localSwaps

proc circleSort[T](a: var openArray[T]) =
  while a.innerCircleSort(0, a.high, 0) != 0:
    discard

var arr = @[@[6, 7, 8, 9, 2, 5, 3, 4, 1],
            @[2, 14, 4, 6, 8, 1, 3, 5, 7, 11, 0, 13, 12, -1]]

for i in 0..arr.high:
  echo "Original: ", $arr[i]
  arr[i].circleSort()
  echo "Sorted: ", $arr[i], if i != arr.high: "\n" else: ""
```


```txt
Original: @[6, 7, 8, 9, 2, 5, 3, 4, 1]
Sorted: @[1, 2, 3, 4, 5, 6, 7, 8, 9]

Original: @[2, 14, 4, 6, 8, 1, 3, 5, 7, 11, 0, 13, 12, -1]
Sorted: @[-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 13, 14]
```



## Objeck

```objeck
class CircleSort {
  function : Main(args : String[]) ~ Nil {
    circleSort([2, 14, 4, 6, 8, 1, 3, 5, 7, 11, 0, 13, 12, -1]);
  }

  function : circleSort(arr : Int[]) ~ Nil {
    if(arr->Size() > 0) {
      do {
        arr->ToString()->PrintLine();
      }
      while(CircleSort(arr, 0, arr->Size() - 1, 0) <> 0);
    };
  }

  function : CircleSort( arr : Int[], lo : Int, hi : Int, num_swaps : Int) ~ Int {
    if(lo = hi) {
      return num_swaps;
    };


    high := hi;
    low := lo;
    mid := (hi - lo) / 2;

    while (lo < hi) {
      if(arr[lo] > arr[hi]) {
        Swap(arr, lo, hi);
        num_swaps++;
      };
      lo++;
      hi--;
    };

    if(lo = hi & arr[lo] > arr[hi + 1]) {
      Swap(arr, lo, hi + 1);
      num_swaps++;
    };

    num_swaps := CircleSort(arr, low, low + mid, num_swaps);
    num_swaps := CircleSort(arr, low + mid + 1, high, num_swaps);

    return num_swaps;
  }

  function : Swap(arr : Int[], idx1 : Int, idx2 : Int) ~ Nil {
    tmp := arr[idx1];
    arr[idx1] := arr[idx2];
    arr[idx2] := tmp;
  }
}

```


Output:

```txt

[2,14,4,6,8,1,3,5,7,11,0,13,12,-1]
[-1,1,0,4,3,8,12,2,7,6,11,5,13,14]
[-1,0,1,3,2,4,7,5,6,8,12,11,13,14]
[-1,0,1,2,3,4,5,6,7,8,11,12,13,14]

```



## PARI/GP

This follows the pseudocode pretty closely.

```parigp
circlesort(v)=
{
	local(v=v); \\ share with cs
	while (cs(1, #v),);
	v;
}
cs(lo, hi)=
{
	if (lo == hi, return (0));
	my(high=hi,low=lo,mid=(hi-lo)\2,swaps);
	while (lo < hi,
		if (v[lo] > v[hi],
			[v[lo],v[hi]]=[v[hi],v[lo]];
			swaps++
		);
		lo++;
		hi--
	);
	if (lo==hi && v[lo] > v[hi+1],
		[v[lo],v[hi+1]]=[v[hi+1],v[lo]];
		swaps++
	);
	swaps + cs(low,low+mid) + cs(low+mid+1,high);
}
print(example=[6,7,8,9,2,5,3,4,1]);
print(circlesort(example));
```

```txt
[6, 7, 8, 9, 2, 5, 3, 4, 1]
[1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Pascal


```pascal

{
   source file name on linux is ./p.p

   -*- mode: compilation; default-directory: "/tmp/" -*-
   Compilation started at Sat Mar 11 23:55:25

   a=./p && pc $a.p && $a
   Free Pascal Compiler version 3.0.0+dfsg-8 [2016/09/03] for x86_64
   Copyright (c) 1993-2015 by Florian Klaempfl and others
   Target OS: Linux for x86-64
   Compiling ./p.p
   Linking p
   /usr/bin/ld.bfd: warning: link.res contains output sections; did you forget -T?
   56 lines compiled, 0.0 sec
   1 2 3 4 5 6 7 8 9

   Compilation finished at Sat Mar 11 23:55:25
}

program sort;

var
   a : array[0..999] of integer;
   i :  integer;

procedure circle_sort(var a : array of integer; left : integer; right : integer);
var swaps : integer;

   procedure csinternal(var a : array of integer; left : integer; right : integer; var swaps : integer);
   var
      lo, hi, mid : integer;
      t           : integer;
   begin
      if left < right then
      begin
	 lo := left;
	 hi := right;
	 while lo < hi do
	 begin
	    if a[hi] < a[lo] then
	    begin
	       t := a[lo]; a[lo] := a[hi]; a[hi] := t;
	       swaps := swaps + 1;
	    end;
	    lo := lo + 1;
	    hi := hi - 1;
	 end;
	 if (lo = hi) and (a[lo+1] < a[lo]) then
	 begin
	    t := a[lo]; a[lo] := a[lo+1]; a[lo+1] := t;
	    swaps := swaps + 1;
	 end;
	 mid := trunc((hi + lo) / 2);
	 csinternal(a, left, mid, swaps);
	 csinternal(a, mid + 1, right, swaps)
      end
   end;

begin;
   swaps := 1;
   while (0 < swaps) do
   begin
      swaps := 0;
      csinternal(a, left, right, swaps);
   end
end;

begin
   {
      generating polynomial coefficients computed in j:  6 7 8 9 2 5 3 4 1x %. ^/~i.9x
      are 6 29999r280 _292519r1120 70219r288 _73271r640 10697r360 _4153r960 667r2016 _139r13440
   }
   a[1]:=6;a[2]:=7;a[3]:=8;a[4]:=9;a[5]:=2;a[6]:=5;a[7]:=3;a[8]:=4;a[9]:=1;
   circle_sort(a,1,9);
   for i := 1 to 9 do write(a[i], ' ');
   writeln();
end.

```



## Perl

Less flexible than the Perl 6 version, as written does only numeric comparisons.
```perl
sub circlesort {
    our @x; local *x = shift;
    my($beg,$end) = @_;

    my $swaps = 0;
    if ($beg < $end) {
        my $lo = $beg;
        my $hi = $end;
        while ($lo < $hi) {
            if ($x[$lo] > $x[$hi]) { # 'gt' here for string comparison
                @x[$lo,$hi] = @x[$hi,$lo];
                ++$swaps;
            }
            ++$hi if --$hi == ++$lo
        }
        $swaps += circlesort(\@x, $beg, $hi);
        $swaps += circlesort(\@x, $lo, $end);
    }
    $swaps;
}

my @a = <16 35 -64 -29 46 36 -1 -99 20 100 59 26 76 -78 39 85 -7 -81 25 88>;
while (circlesort(\@a, 0, $#a)) { print join(' ', @a), "\n" }
```

```txt
-99 -78 16 20 36 -81 -29 46 25 59 -64 -7 39 26 88 -1 35 85 76 100
-99 -78 -29 -81 16 -64 -7 20 -1 39 25 26 36 46 59 35 76 88 85 100
-99 -81 -78 -64 -29 -7 -1 16 20 25 26 35 36 39 46 59 76 85 88 100
-99 -81 -78 -64 -29 -7 -1 16 20 25 26 35 36 39 46 59 76 85 88 100
```



## Perl 6


The given algorithm can be simplified in several ways.  There's no need to compute the midpoint, since the hi/lo will end up there.  The extra swap conditional can be eliminated by incrementing hi at the correct moment inside the loop.  There's no need to
pass accumulated swaps down the call stack.

This does generic comparisons, so it works on any ordered type, including numbers or strings.

```perl6
sub circlesort (@x, $beg, $end) {
    my $swaps = 0;
    if $beg < $end {
        my ($lo, $hi) = $beg, $end;
        repeat {
            if @x[$lo] after @x[$hi] {
                @x[$lo,$hi] .= reverse;
                ++$swaps;
            }
            ++$hi if --$hi == ++$lo
        } while $lo < $hi;
        $swaps += circlesort(@x, $beg, $hi);
        $swaps += circlesort(@x, $lo, $end);
    }
    $swaps;
}

say my @x = (-100..100).roll(20);
say @x while circlesort(@x, 0, @x.end);

say @x = <The quick brown fox jumps over the lazy dog.>;
say @x while circlesort(@x, 0, @x.end);
```

```txt
16 35 -64 -29 46 36 -1 -99 20 100 59 26 76 -78 39 85 -7 -81 25 88
-99 -78 16 20 36 -81 -29 46 25 59 -64 -7 39 26 88 -1 35 85 76 100
-99 -78 -29 -81 16 -64 -7 20 -1 39 25 26 36 46 59 35 76 88 85 100
-99 -81 -78 -64 -29 -7 -1 16 20 25 26 35 36 39 46 59 76 85 88 100
The quick brown fox jumps over the lazy dog.
The brown fox jumps lazy dog. quick over the
The brown dog. fox jumps lazy over quick the
```



## Phix


```Phix
sequence array

function circle_sort_inner(integer lo, hi, swaps, level=1)
    if lo!=hi then
        integer high := hi,
                low := lo,
                mid := floor((high-low)/2)
        while lo <= hi do
            hi += (lo=hi)
            if array[lo] > array[hi] then
                {array[lo],array[hi]} = {array[hi],array[lo]}
                ?{array,"level",level,{low,high}}
                swaps += 1
            end if
            lo += 1
            hi -= 1
        end while
        swaps = circle_sort_inner(low,low+mid,swaps,level+1)
        swaps = circle_sort_inner(low+mid+1,high,swaps,level+1)
    end if
    return swaps
end function

procedure circle_sort()
    ?{array,"<== (initial)"}
    while circle_sort_inner(1, length(array), 0) do ?"loop" end while
    ?{array,"<== (sorted)"}
end procedure

array = {5, -1, 101, -4, 0, 1, 8, 6, 2, 3}
--array = {-4,-1,1,0,5,-7,-2,4,-6,-3,2,6,3,7,-5}
--array = {6, 7, 8, 9, 2, 5, 3, 4, 1}
--array = {2,14,4,6,8,1,3,5,7,9,10,11,0,13,12,-1}
--array = {"the","quick","brown","fox","jumps","over","the","lazy","dog"}
--array = {0.603704, 0.293639, 0.513965, 0.746246, 0.245282, 0.930508, 0.550878, 0.622534, 0.006089, 0.270426}
--array = shuffle(array)
circle_sort()
```

Shows the full inner workings: call depth and range being considered, after each swap made.

```txt

{{5,-1,101,-4,0,1,8,6,2,3},"<== (initial)"}
"loop"
"loop"
{{-4,-1,0,1,2,3,5,6,8,101},"<== (sorted)"}

```



## Python

The doctest passes with odd and even length lists.  As do the random tests.  Please see circle_sort.__doc__ for example use and output.

```python

#python3
#tests: expect no output.
#doctest with  python3 -m doctest thisfile.py
#additional tests:  python3 thisfile.py

def circle_sort_backend(A:list, L:int, R:int)->'sort A in place, returning the number of swaps':
    '''
        >>> L = [3, 2, 8, 28, 2,]
        >>> circle_sort(L)
        3
        >>> print(L)
        [2, 2, 3, 8, 28]
        >>> L = [3, 2, 8, 28,]
        >>> circle_sort(L)
        1
        >>> print(L)
        [2, 3, 8, 28]
    '''
    n = R-L
    if n < 2:
        return 0
    swaps = 0
    m = n//2
    for i in range(m):
        if A[R-(i+1)] < A[L+i]:
            (A[R-(i+1)], A[L+i],) = (A[L+i], A[R-(i+1)],)
            swaps += 1
    if (n & 1) and (A[L+m] < A[L+m-1]):
        (A[L+m-1], A[L+m],) = (A[L+m], A[L+m-1],)
        swaps += 1
    return swaps + circle_sort_backend(A, L, L+m) + circle_sort_backend(A, L+m, R)

def circle_sort(L:list)->'sort A in place, returning the number of swaps':
    swaps = 0
    s = 1
    while s:
        s = circle_sort_backend(L, 0, len(L))
        swaps += s
    return swaps

# more tests!
if __name__ == '__main__':
    from random import shuffle
    for i in range(309):
        L = list(range(i))
        M = L[:]
        shuffle(L)
        N = L[:]
        circle_sort(L)
        if L != M:
            print(len(L))
            print(N)
            print(L)

```



## Racket


By default this sorts with the numeric <code>&lt;</code> but any other
(diadic) function can be used to compare... e.g. <code>string&lt;?</code>.


```racket
#lang racket
(define (circle-sort v0 [<? <])
  (define v (vector-copy v0))

  (define (swap-if l r)
    (define v.l (vector-ref v l))
    (define v.r (vector-ref v r))
    (and (<? v.r v.l)
         (begin (vector-set! v l v.r) (vector-set! v r v.l) #t)))

  (define (inr-cs! L R)
    (cond
      [(>= L (- R 1)) #f] ; covers 0 or 1 vectors
      [else
       (define M (quotient (+ L R) 2))
       (define I-moved?
         (for/or ([l (in-range L M)] [r (in-range (- R 1) L -1)])
           (swap-if l r)))
       (define M-moved? (and (odd? (- L R)) (> M 0) (swap-if (- M 1) M)))
       (define L-moved? (inr-cs! L M))
       (define R-moved? (inr-cs! M R))
       (or I-moved? L-moved? R-moved? M-moved?)]))

  (let loop () (when (inr-cs! 0 (vector-length v)) (loop)))
  v)

(define (sort-random-vector)
  (define v (build-vector (+ 2 (random 10)) (λ (i) (random 100))))
  (define v< (circle-sort v <))
  (define sorted? (apply <= (vector->list v<)))
  (printf "   ~.a\n-> ~.a [~a]\n\n" v v< sorted?))

(for ([_ 10]) (sort-random-vector))

(circle-sort '#("table" "chair" "cat" "sponge") string<?)
```


```txt

   #(36 94 63 51 33)
-> #(33 36 51 63 94) [#t]

   #(73 74 20 20 79)
-> #(20 20 73 74 79) [#t]

   #(83 42)
-> #(42 83) [#t]

   #(53 95 43 33 66 47 1 61 28 96)
-> #(1 28 33 43 47 53 61 66 95 96) [#t]

   #(71 85)
-> #(71 85) [#t]

   #(36 85 50 19 88 17 2 53 21)
-> #(2 17 19 21 36 50 53 85 88) [#t]

   #(5 97 62 21 99 73 17 16 37 28)
-> #(5 16 17 21 28 37 62 73 97 99) [#t]

   #(12 60 89 90 2 95 9 28)
-> #(2 9 12 28 60 89 90 95) [#t]

   #(50 32 30 47 63 74)
-> #(30 32 47 50 63 74) [#t]

   #(63 41)
-> #(41 63) [#t]

'#("cat" "chair" "sponge" "table")

```



## REXX

This REXX version will work with any numbers that REXX supports, including negative and/or floating point numbers.

```rexx
/*REXX program uses a  circle sort algorithm  to sort an array (or list) of numbers.    */
parse arg x                                      /*obtain optional arguments from the CL*/
if x='' | x=","  then x= 6 7 8 9 2 5 3 4 1       /*Not specified?  Then use the default.*/
call make_array  'before sort:'                  /*display the list and make an array.  */
call circleSort      #                           /*invoke the circle sort subroutine.   */
call make_list   ' after sort:'                  /*make a list and display it to console*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
circleSort:      do  while  .circleSrt(1, arg(1), 0)\==0;    end;                   return
make_array: #=words(x);     do i=1 for #;   @.i=word(x, i);  end;  say arg(1)  x;   return
make_list:  y=@.1;          do j=2 for #-1; y=y  @.j;        end;  say arg(1)  y;   return
.swap:      parse arg a,b;  parse  value  @.a @.b  with  @.b @.a;  swaps=swaps+1;   return
/*──────────────────────────────────────────────────────────────────────────────────────*/
.circleSrt: procedure expose @.;  parse arg LO,HI,swaps    /*obtain  LO & HI  arguments.*/
            if LO==HI  then return swaps                   /*1 element?  Done with sort.*/
            high=HI;   low=LO;     mid=(HI-LO) % 2         /*assign some indices.       */
                                                           /* [↓]  sort a section of #'s*/
                       do  while LO<HI                     /*sort within a section.     */
                       if @.LO>@.HI  then call .swap LO,HI /*are numbers out of order ? */
                       LO=LO+1;  HI=HI-1                   /*add to LO;  shrink the HI. */
                       end   /*while*/                     /*just process one section.  */
            _=hi+1                                         /*point to  HI  plus one.    */
            if LO==HI  &  @.LO>@._  then call .swap LO, _  /*numbers still out of order?*/
            swaps=.circleSrt(low,        low+mid,  swaps)  /*sort the   lower  section. */
            swaps=.circleSrt(low+mid+1,  high,     swaps)  /*  "   "   higher     "     */
            return swaps                                   /*the section sorting is done*/
```

```txt

before sort:  6 7 8 9 2 5 3 4 1
 after sort:  1 2 3 4 5 6 7 8 9

```

```txt

before sort: 2 3 3 5 5 1 1 7 7 6 6 4 4 0 0
 after sort: 0 0 1 1 2 3 3 4 4 5 5 6 6 7 7

```

```txt

before sort: 2 3 44 44 5.77 +1 -12345 -3 -3.9 1e7 0
 after sort: -12345 -3.9 -3 0 +1 2 3 5.77 44 44 1e7

```

```txt

before sort: assinine donkey bovine cattle canine dog corvine crow equine horse feline cat hircine goat leporine hare lupine wolf murine rodent piscine fish porcine pig ursine bear vulpine fox
 after sort: assinine bear bovine canine cat cattle corvine crow dog donkey equine feline fish fox goat hare hircine horse leporine lupine murine pig piscine porcine rodent ursine vulpine wolf

```



## Ring


```ring

# Project : Sorting Algorithms/Circle Sort

test = [-4, -1, 1, 0, 5, -7, -2, 4, -6, -3, 2, 6, 3, 7, -5]
while circlesort(1, len(test), 0) end
showarray(test)

func circlesort(lo, hi, swaps)
     if lo = hi
        return swaps
     ok
     high = hi
     low = lo
     mid = floor((hi-lo)/2)
     while lo < hi
           if test[lo] > test[hi]
               temp = test[lo]
               test[lo] = test[hi]
               test[hi] = temp
               swaps = swaps + 1
           ok
           lo = lo + 1
           hi = hi - 1
     end
     if lo = hi
        if test[lo] > test[hi+1]
           temp = test[lo]
           test[lo] = test[hi+1]
           test[hi + 1] = temp
           swaps = swaps + 1
        ok
     ok
     swaps = circlesort(low, low+mid, swaps)
     swaps = circlesort(low+mid+1 ,high, swaps)
     return swaps

func showarray(vect)
     see "["
     svect = ""
     for n = 1 to len(vect)
         svect = svect + vect[n] + ", "
     next
     svect = left(svect, len(svect) - 2)
     see svect
     see "]" + nl

```

Output:

```txt

[-7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7]

```



## Ruby


```ruby
class Array
  def circle_sort!
    while _circle_sort!(0, size-1) > 0
    end
    self
  end

  private
  def _circle_sort!(lo, hi, swaps=0)
    return swaps if lo == hi
    low, high = lo, hi
    mid = (lo + hi) / 2
    while lo < hi
      if self[lo] > self[hi]
        self[lo], self[hi] = self[hi], self[lo]
        swaps += 1
      end
      lo += 1
      hi -= 1
    end
    if lo == hi && self[lo] > self[hi+1]
      self[lo], self[hi+1] = self[hi+1], self[lo]
      swaps += 1
    end
    swaps + _circle_sort!(low, mid) + _circle_sort!(mid+1, high)
  end
end

ary = [6, 7, 8, 9, 2, 5, 3, 4, 1]
puts "before sort: #{ary}"
puts " after sort: #{ary.circle_sort!}"
```


 before sort: [6, 7, 8, 9, 2, 5, 3, 4, 1]
  after sort: [1, 2, 3, 4, 5, 6, 7, 8, 9]

## Scala


```Scala
object CircleSort extends App {

  def sort(arr: Array[Int]): Array[Int] = {
    def circleSortR(arr: Array[Int], _lo: Int, _hi: Int, _numSwaps: Int): Int = {
      var lo = _lo
      var hi = _hi
      var numSwaps = _numSwaps

      def swap(arr: Array[Int], idx1: Int, idx2: Int): Unit = {
        val tmp = arr(idx1)
        arr(idx1) = arr(idx2)
        arr(idx2) = tmp
      }

      if (lo == hi) return numSwaps
      val (high, low) = (hi, lo)
      val mid = (hi - lo) / 2
      while ( lo < hi) {
        if (arr(lo) > arr(hi)) {
          swap(arr, lo, hi)
          numSwaps += 1
        }
        lo += 1
        hi -= 1
      }
      if (lo == hi && arr(lo) > arr(hi + 1)) {
        swap(arr, lo, hi + 1)
        numSwaps += 1
      }

      circleSortR(arr, low + mid + 1, high, circleSortR(arr, low, low + mid, numSwaps))
    }

    while (circleSortR(arr, 0, arr.length - 1, 0) != 0)()
    arr
  }

  println(sort(Array[Int](2, 14, 4, 6, 8, 1, 3, 5, 7, 11, 0, 13, 12, -1)).mkString(", "))

}
```



## Sidef


```ruby
func circlesort(arr, beg=0, end=arr.end) {
    var swaps = 0
    if (beg < end) {
        var (lo, hi) = (beg, end)
        do {
            if (arr[lo] > arr[hi]) {
                arr.swap(lo, hi)
                ++swaps
            }
            ++hi if (--hi == ++lo)
        } while (lo < hi)
        swaps += circlesort(arr, beg, hi)
        swaps += circlesort(arr, lo, end)
    }
    return swaps
}

var numbers = %n(2 3 3 5 5 1 1 7 7 6 6 4 4 0 0)
do { say numbers } while circlesort(numbers)
 
var strs = ["John", "Kate", "Zerg", "Alice", "Joe", "Jane", "Alice"]
do { say strs } while circlesort(strs)
```

```txt

[2, 3, 3, 5, 5, 1, 1, 7, 7, 6, 6, 4, 4, 0, 0]
[0, 0, 1, 4, 1, 5, 3, 7, 2, 3, 4, 5, 6, 6, 7]
[0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 7, 6, 6, 7]
[0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7]
["John", "Kate", "Zerg", "Alice", "Joe", "Jane", "Alice"]
["Alice", "Jane", "Alice", "Joe", "John", "Kate", "Zerg"]
["Alice", "Alice", "Jane", "Joe", "John", "Kate", "Zerg"]

```



## uBasic/4tH

This one uses the optimized version featured at [http://sourceforge.net/p/forth-4th/wiki/Circle%20sort/ Sourceforge].
<lang>PRINT "Circle sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Circlesort (n)
  PROC _ShowArray (n)
PRINT

END

_InnerCircle PARAM (2)
  LOCAL (3)
  c@ = a@
  d@ = b@
  e@ = 0

  IF c@ = d@ THEN RETURN (0)

  DO WHILE c@ < d@
    IF @(c@) > @(d@) THEN PROC _Swap (c@, d@) : e@ = e@ + 1
    c@ = c@ + 1
    d@ = d@ - 1
  LOOP

  e@ = e@ + FUNC (_InnerCircle (a@, d@))
  e@ = e@ + FUNC (_InnerCircle (c@, b@))
RETURN (e@)


_Circlesort PARAM(1)                   ' Circle sort
  DO WHILE FUNC (_InnerCircle (0, a@-1))
  LOOP
RETURN


_Swap PARAM(2)                         ' Swap two array elements
  PUSH @(a@)
  @(a@) = @(b@)
  @(b@) = POP()
RETURN


_InitArray                             ' Init example array
  PUSH 4, 65, 2, -31, 0, 99, 2, 83, 782, 1

  FOR i = 0 TO 9
    @(i) = POP()
  NEXT

RETURN (i)


_ShowArray PARAM (1)                   ' Show array subroutine
  FOR i = 0 TO a@-1
    PRINT @(i),
  NEXT

  PRINT
RETURN
```



## zkl


```zkl
fcn circleSort(list){
   csort:=fcn(list,lo,hi,swaps){
      if(lo==hi) return(swaps);
      high,low,mid:=hi,lo,(hi-lo)/2;
      while(lo<hi){
	 if(list[lo]>list[hi]){
	    list.swap(lo,hi);
	    swaps+=1;
	 }
	 lo+=1; hi-=1;
      }
      if(lo==hi)
	 if (list[lo]>list[hi+1]){
	    list.swap(lo,hi+1);
	    swaps+=1;
	 }
      swaps=self.fcn(list,low,low + mid,swaps);
      swaps=self.fcn(list,low + mid + 1,high,swaps);
      return(swaps);
   };
   list.println();
   while(csort(list,0,list.len()-1,0)){ list.println() }
   list
}
```


```zkl
circleSort(L(6,7,8,9,2,5,3,4,1));
circleSort(L(5,-1,101,-4,0,1,8,6,2,3));
```

```txt

L(6,7,8,9,2,5,3,4,1)
L(1,3,4,2,5,6,7,8,9)
L(1,2,3,4,5,6,7,8,9)
L(5,-1,101,-4,0,1,8,6,2,3)
L(-4,-1,0,3,6,1,2,8,5,101)
L(-4,-1,0,1,2,3,5,6,8,101)

```



## ZX Spectrum Basic

A language like ZX BASIC is not the most obvious choice for a routine which depends on local variables and recursion. This program proves that it can be implemented quite efficiently using arrays and global variables. The '''b''' and '''e''' variables are set up in such a way that they can be used for the first recursive call. The variables for the next recursion are saved in array '''s()''' which serves as a stack together with stack pointer '''p'''.

The size of the stack is determined by the amount of memory on the ZX Spectrum, which is 64KB (or 2<sup>16</sup> bytes). Each call requires two array elements. Note the size of a ZX Spectrum floating point number is 5 bytes, so this stack is slightly oversized. The somewhat strange indexing between both recursions is due to an stack pointer adjustment which was optimized away.

This version of Circle sort was based on the optimized version on [http://sourceforge.net/p/forth-4th/wiki/Circle%20sort/ Sourceforge]. It will also show a few asterisks while running, because it will take some time to finish (about two minutes).


```zxbasic

   10 DIM a(100): DIM s(32): RANDOMIZE : LET p=1: GO SUB 3000: GO SUB 2000: GO SUB 4000
   20 STOP
 1000 IF b=e THEN RETURN
 1010 LET s(p)=b: LET s(p+1)=e
 1020 IF a(s(p))>a(e) THEN LET t=a(s(p)): LET a(s(p))=a(e): LET a(e)=t: LET c=1
 1030 LET s(p)=s(p)+1: LET e=e-1: IF s(p)<e THEN GO TO 1020
 1040 LET p=p+2: GO SUB 1000: LET b=s(p-2): LET e=s(p-1): GO SUB 1000: LET p=p-2: RETURN
 2000 PRINT "*";: LET b=1: LET e=100: LET c=0: GO SUB 1000: IF c>0 THEN GO TO 2000
 2010 CLS : RETURN
 3000 FOR x=1 TO 100: LET a(x)=RND: NEXT x: RETURN
 4000 FOR x=1 TO 100: PRINT x,a(x): NEXT x: RETURN

```

