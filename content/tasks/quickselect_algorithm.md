+++
title = "Quickselect algorithm"
description = ""
date = 2019-04-23T20:28:05Z
aliases = []
[extra]
id = 16391
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "autohotkey",
  "c",
  "cobol",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "factor",
  "fortran",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "maple",
  "netrexx",
  "nim",
  "ocaml",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "standard_ml",
  "swift",
  "tcl",
  "vba",
  "zkl",
]
+++

Use the [[wp:Quickselect|quickselect algorithm]] on the vector
: [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
To show the first, second, third, ... up to the tenth largest member of the vector, in order, here on this page.

* Note: Quick''sort'' has a separate [[Sorting algorithms/Quicksort|task]].




## ALGOL 68


```algol68
BEGIN
    # returns the kth lowest element of list using the quick select algorithm #
    PRIO QSELECT = 1;
    OP   QSELECT = ( INT k, REF[]INT list )INT:
         IF LWB list > UPB list THEN
             # empty list #
             0
         ELSE
             # non-empty list #
             # partitions the subset of list from left to right #
             PROC partition = ( REF[]INT list, INT left, right, pivot index )INT:
                  BEGIN
                      # swaps elements a and b in list #
                      PROC swap = ( REF[]INT list, INT a, b )VOID:
                           BEGIN
                               INT t = list[ a ];
                               list[ a ] := list[ b ];
                               list[ b ] := t
                           END # swap # ;
                      INT pivot value = list[ pivot index ];
                      swap( list, pivot index, right );
                      INT store index := left;
                      FOR i FROM left TO right - 1 DO
                          IF list[ i ] < pivot value THEN
                              swap( list, store index, i );
                              store index +:= 1
                          FI
                      OD;
                      swap( list, right, store index );
                      store index
                  END # partition # ;
             INT  left  := LWB list, right := UPB list, result := 0;
             BOOL found := FALSE;
             WHILE NOT found DO
                 IF left = right THEN
                     result := list[ left ];
                     found := TRUE
                 ELSE
                     INT pivot index = partition( list, left, right, left + ENTIER ( ( random * ( right - left ) + 1 ) ) );
                     IF k = pivot index THEN
                         result := list[ k ];
                         found := TRUE
                     ELIF k < pivot index THEN
                         right := pivot index - 1
                     ELSE
                         left  := pivot index + 1
                     FI
                 FI
             OD;
             result
         FI # QSELECT # ;
    # test cases #
    FOR i TO 10 DO
        [ 1 : 10 ]INT test := []INT( 9, 8, 7, 6, 5, 0, 1, 2, 3, 4 );
        print( ( whole( i, -2 ), ": ", whole( i QSELECT test, -3 ), newline ) )
    OD
END
```

```txt

 1:   0
 2:   1
 3:   2
 4:   3
 5:   4
 6:   5
 7:   6
 8:   7
 9:   8
10:   9

```



## AutoHotkey

{{works with|AutoHotkey_L}} (AutoHotkey1.1+)
A direct implementation of the Wikipedia pseudo-code.

```AutoHotkey
MyList := [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
Loop, 10
	Out .= Select(MyList, 1, MyList.MaxIndex(), A_Index) (A_Index = MyList.MaxIndex() ? "" : ", ")
MsgBox, % Out
return

Partition(List, Left, Right, PivotIndex) {
	PivotValue := List[PivotIndex]
	, Swap(List, pivotIndex, Right)
	, StoreIndex := Left
	, i := Left - 1
	Loop, % Right - Left
		if (List[j := i + A_Index] <= PivotValue)
			Swap(List, StoreIndex, j)
			, StoreIndex++
	Swap(List, Right, StoreIndex)
	return StoreIndex
}

Select(List, Left, Right, n) {
	if (Left = Right)
		return List[Left]
	Loop {
		PivotIndex := (Left + Right) // 2
		, PivotIndex := Partition(List, Left, Right, PivotIndex)
		if (n = PivotIndex)
			return List[n]
		else if (n < PivotIndex)
			Right := PivotIndex - 1
		else
			Left := PivotIndex + 1
	}
}

Swap(List, i1, i2) {
	t := List[i1]
	, List[i1] := List[i2]
	, List[i2] := t
}
```

'''Output:'''

```txt
0, 1, 2, 3, 4, 5, 6, 7, 8, 9
```



## C


```c
#include <stdio.h>
#include <string.h>

int qselect(int *v, int len, int k)
{
#	define SWAP(a, b) { tmp = v[a]; v[a] = v[b]; v[b] = tmp; }
	int i, st, tmp;

	for (st = i = 0; i < len - 1; i++) {
		if (v[i] > v[len-1]) continue;
		SWAP(i, st);
		st++;
	}

	SWAP(len-1, st);

	return k == st	?v[st]
			:st > k	? qselect(v, st, k)
				: qselect(v + st, len - st, k - st);
}

int main(void)
{
#	define N (sizeof(x)/sizeof(x[0]))
	int x[] = {9, 8, 7, 6, 5, 0, 1, 2, 3, 4};
	int y[N];

	int i;
	for (i = 0; i < 10; i++) {
		memcpy(y, x, sizeof(x)); // qselect modifies array
		printf("%d: %d\n", i, qselect(y, 10, i));
	}

	return 0;
}
```

```txt

0: 0
1: 1
2: 2
3: 3
4: 4
5: 5
6: 6
7: 7
8: 8
9: 9

```



## C++


;Library

It is already provided in the standard library as <code>std::nth_element()</code>. Although the standard does not explicitly mention what algorithm it must use, the algorithm partitions the sequence into those less than the nth element to the left, and those greater than the nth element to the right, like quickselect; the standard also guarantees that the complexity is "linear on average", which fits quickselect.

```cpp
#include <algorithm>
#include <iostream>

int main() {
  for (int i = 0; i < 10; i++) {
    int a[] = {9, 8, 7, 6, 5, 0, 1, 2, 3, 4};
    std::nth_element(a, a + i, a + sizeof(a)/sizeof(*a));
    std::cout << a[i];
    if (i < 9) std::cout << ", ";
  }
  std::cout << std::endl;

  return 0;
}
```


```txt
0, 1, 2, 3, 4, 5, 6, 7, 8, 9
```


;Implementation

A more explicit implementation:

```cpp
#include <iterator>
#include <algorithm>
#include <functional>
#include <cstdlib>
#include <ctime>
#include <iostream>

template <typename Iterator>
Iterator select(Iterator begin, Iterator end, int n) {
  typedef typename std::iterator_traits<Iterator>::value_type T;
  while (true) {
    Iterator pivotIt = begin + std::rand() % std::distance(begin, end);
    std::iter_swap(pivotIt, end-1);  // Move pivot to end
    pivotIt = std::partition(begin, end-1, std::bind2nd(std::less<T>(), *(end-1)));
    std::iter_swap(end-1, pivotIt);  // Move pivot to its final place
    if (n == pivotIt - begin) {
      return pivotIt;
    } else if (n < pivotIt - begin) {
      end = pivotIt;
    } else {
      n -= pivotIt+1 - begin;
      begin = pivotIt+1;
    }
  }
}

int main() {
  std::srand(std::time(NULL));
  for (int i = 0; i < 10; i++) {
    int a[] = {9, 8, 7, 6, 5, 0, 1, 2, 3, 4};
    std::cout << *select(a, a + sizeof(a)/sizeof(*a), i);
    if (i < 9) std::cout << ", ";
  }
  std::cout << std::endl;

  return 0;
}
```


```txt
0, 1, 2, 3, 4, 5, 6, 7, 8, 9
```


## C#
Two different implementations - one that returns only one element from the array (Nth smallest element) and
second implementation that returns IEnumnerable that enumerates through element until Nth smallest element.


```c#
// ----------------------------------------------------------------------------------------------
//
//  Program.cs - QuickSelect
//
// ----------------------------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Linq;

namespace QuickSelect
{
    internal static class Program
    {
        #region Static Members

        private static void Main()
        {
            var inputArray = new[] {9, 8, 7, 6, 5, 0, 1, 2, 3, 4};
            // Loop 10 times
            Console.WriteLine( "Loop quick select 10 times." );
            for( var i = 0 ; i < 10 ; i++ )
            {
                Console.Write( inputArray.NthSmallestElement( i ) );
                if( i < 9 )
                    Console.Write( ", " );
            }
            Console.WriteLine();

            // And here is then more effective way to get N smallest elements from vector in order by using quick select algorithm
            // Basically we are here just sorting array (taking 10 smallest from array which length is 10)
            Console.WriteLine( "Just sort 10 elements." );
            Console.WriteLine( string.Join( ", ", inputArray.TakeSmallest( 10 ).OrderBy( v => v ).Select( v => v.ToString() ).ToArray() ) );
            // Here we are actually doing quick select once by taking only 4 smallest from array.
            Console.WriteLine( "Get 4 smallest and sort them." );
            Console.WriteLine( string.Join( ", ", inputArray.TakeSmallest( 4 ).OrderBy( v => v ).Select( v => v.ToString() ).ToArray() ) );
            Console.WriteLine( "< Press any key >" );
            Console.ReadKey();
        }

        #endregion
    }

    internal static class ArrayExtension
    {
        #region Static Members

        /// <summary>
        ///  Return specified number of smallest elements from array.
        /// </summary>
        /// <typeparam name="T">The type of the elements of array. Type must implement IComparable(T) interface.</typeparam>
        /// <param name="array">The array to return elemnts from.</param>
        /// <param name="count">The number of smallest elements to return. </param>
        /// <returns>An IEnumerable(T) that contains the specified number of smallest elements of the input array. Returned elements are NOT sorted.</returns>
        public static IEnumerable<T> TakeSmallest<T>( this T[] array, int count ) where T : IComparable<T>
        {
            if( count < 0 )
                throw new ArgumentOutOfRangeException( "count", "Count is smaller than 0." );
            if( count == 0 )
                return new T[0];
            if( array.Length <= count )
                return array;

            return QuickSelectSmallest( array, count - 1 ).Take( count );
        }

        /// <summary>
        /// Returns N:th smallest element from the array.
        /// </summary>
        /// <typeparam name="T">The type of the elements of array. Type must implement IComparable(T) interface.</typeparam>
        /// <param name="array">The array to return elemnt from.</param>
        /// <param name="n">Nth element. 0 is smallest element, when array.Length - 1 is largest element.</param>
        /// <returns>N:th smalles element from the array.</returns>
        public static T NthSmallestElement<T>( this T[] array, int n ) where T : IComparable<T>
        {
            if( n < 0 || n > array.Length - 1 )
                throw new ArgumentOutOfRangeException( "n", n, string.Format( "n should be between 0 and {0} it was {1}.", array.Length - 1, n ) );
            if( array.Length == 0 )
                throw new ArgumentException( "Array is empty.", "array" );
            if( array.Length == 1 )
                return array[ 0 ];

            return QuickSelectSmallest( array, n )[ n ];
        }

        /// <summary>
        ///  Partially sort array such way that elements before index position n are smaller or equal than elemnt at position n. And elements after n are larger or equal.
        /// </summary>
        /// <typeparam name="T">The type of the elements of array. Type must implement IComparable(T) interface.</typeparam>
        /// <param name="input">The array which elements are being partially sorted. This array is not modified.</param>
        /// <param name="n">Nth smallest element.</param>
        /// <returns>Partially sorted array.</returns>
        private static T[] QuickSelectSmallest<T>( T[] input, int n ) where T : IComparable<T>
        {
            // Let's not mess up with our input array
            // For very large arrays - we should optimize this somehow - or just mess up with our input
            var partiallySortedArray = (T[]) input.Clone();

            // Initially we are going to execute quick select to entire array
            var startIndex = 0;
            var endIndex = input.Length - 1;

            // Selecting initial pivot
            // Maybe we are lucky and array is sorted initially?
            var pivotIndex = n;

            // Loop until there is nothing to loop (this actually shouldn't happen - we should find our value before we run out of values)
            var r = new Random();
            while( endIndex > startIndex )
            {
                pivotIndex = QuickSelectPartition( partiallySortedArray, startIndex, endIndex, pivotIndex );
                if( pivotIndex == n )
                    // We found our n:th smallest value - it is stored to pivot index
                    break;
                if( pivotIndex > n )
                    // Array before our pivot index have more elements that we are looking for
                    endIndex = pivotIndex - 1;
                else
                    // Array before our pivot index has less elements that we are looking for
                    startIndex = pivotIndex + 1;

                // Omnipotent beings don't need to roll dices - but we do...
                // Randomly select a new pivot index between end and start indexes (there are other methods, this is just most brutal and simplest)
                pivotIndex = r.Next( startIndex,  endIndex );
            }
            return partiallySortedArray;
        }

        /// <summary>
        /// Sort elements in sub array between startIndex and endIndex, such way that elements smaller than or equal with value initially stored to pivot index are before
        /// new returned pivot value index.
        /// </summary>
        /// <typeparam name="T">The type of the elements of array. Type must implement IComparable(T) interface.</typeparam>
        /// <param name="array">The array that is being sorted.</param>
        /// <param name="startIndex">Start index of sub array.</param>
        /// <param name="endIndex">End index of sub array.</param>
        /// <param name="pivotIndex">Pivot index.</param>
        /// <returns>New pivot index. Value that was initially stored to <paramref name="pivotIndex"/> is stored to this newly returned index. All elements before this index are
        /// either smaller or equal with pivot value. All elements after this index are larger than pivot value.</returns>
        /// <remarks>This method modifies paremater array.</remarks>
        private static int QuickSelectPartition<T>( this T[] array, int startIndex, int endIndex, int pivotIndex ) where T : IComparable<T>
        {
            var pivotValue = array[ pivotIndex ];
            // Initially we just assume that value in pivot index is largest - so we move it to end (makes also for loop more straight forward)
            array.Swap( pivotIndex, endIndex );
            for( var i = startIndex ; i < endIndex ; i++ )
            {
                if( array[ i ].CompareTo( pivotValue ) > 0 )
                    continue;

                // Value stored to i was smaller than or equal with pivot value - let's move it to start
                array.Swap( i, startIndex );
                // Move start one index forward
                startIndex++;
            }
            // Start index is now pointing to index where we should store our pivot value from end of array
            array.Swap( endIndex, startIndex );
            return startIndex;
        }

        private static void Swap<T>( this T[] array, int index1, int index2 )
        {
            if( index1 == index2 )
                return;

            var temp = array[ index1 ];
            array[ index1 ] = array[ index2 ];
            array[ index2 ] = temp;
        }

        #endregion
    }
}
```

```txt
Loop quick select 10 times.
0, 1, 2, 3, 4, 5, 6, 7, 8, 9
Just sort 10 elements.
0, 1, 2, 3, 4, 5, 6, 7, 8, 9
Get 4 smallest and sort them.
0, 1, 2, 3
< Press any key >
```



## COBOL

The following is in the Managed COBOL dialect:
```cobol
       CLASS-ID MainProgram.

       METHOD-ID Partition STATIC USING T.
       CONSTRAINTS.
           CONSTRAIN T IMPLEMENTS type IComparable.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  pivot-val              T.

       PROCEDURE DIVISION USING VALUE arr AS T OCCURS ANY,
               left-idx AS BINARY-LONG, right-idx AS BINARY-LONG,
               pivot-idx AS BINARY-LONG
               RETURNING ret AS BINARY-LONG.
           MOVE arr (pivot-idx) TO pivot-val
           INVOKE self::Swap(arr, pivot-idx, right-idx)
           DECLARE store-idx AS BINARY-LONG = left-idx
           PERFORM VARYING i AS BINARY-LONG FROM left-idx BY 1
                   UNTIL i > right-idx
               IF arr (i) < pivot-val
                   INVOKE self::Swap(arr, i, store-idx)
                   ADD 1 TO store-idx
               END-IF
           END-PERFORM
           INVOKE self::Swap(arr, right-idx, store-idx)

           MOVE store-idx TO ret
       END METHOD.

       METHOD-ID Quickselect STATIC USING T.
       CONSTRAINTS.
           CONSTRAIN T IMPLEMENTS type IComparable.

       PROCEDURE DIVISION USING VALUE arr AS T OCCURS ANY,
               left-idx AS BINARY-LONG, right-idx AS BINARY-LONG,
               n AS BINARY-LONG
               RETURNING ret AS T.
           IF left-idx = right-idx
               MOVE arr (left-idx) TO ret
               GOBACK
           END-IF

           DECLARE rand AS TYPE Random = NEW Random()
           DECLARE pivot-idx AS BINARY-LONG = rand::Next(left-idx, right-idx)
           DECLARE pivot-new-idx AS BINARY-LONG
               = self::Partition(arr, left-idx, right-idx, pivot-idx)
           DECLARE pivot-dist AS BINARY-LONG = pivot-new-idx - left-idx + 1

           EVALUATE TRUE
               WHEN pivot-dist = n
                   MOVE arr (pivot-new-idx) TO ret

               WHEN n < pivot-dist
                   INVOKE self::Quickselect(arr, left-idx, pivot-new-idx - 1, n)
                       RETURNING ret

               WHEN OTHER
                   INVOKE self::Quickselect(arr, pivot-new-idx + 1, right-idx,
                       n - pivot-dist) RETURNING ret
           END-EVALUATE
       END METHOD.

       METHOD-ID Swap STATIC USING T.
       CONSTRAINTS.
           CONSTRAIN T IMPLEMENTS type IComparable.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  temp                   T.

       PROCEDURE DIVISION USING arr AS T OCCURS ANY,
               VALUE idx-1 AS BINARY-LONG, idx-2 AS BINARY-LONG.
           IF idx-1 <> idx-2
               MOVE arr (idx-1) TO temp
               MOVE arr (idx-2) TO arr (idx-1)
               MOVE temp TO arr (idx-2)
           END-IF
       END METHOD.

       METHOD-ID Main STATIC.
       PROCEDURE DIVISION.
           DECLARE input-array AS BINARY-LONG OCCURS ANY
               = TABLE OF BINARY-LONG(9, 8, 7, 6, 5, 0, 1, 2, 3, 4)
           DISPLAY "Loop quick select 10 times."
           PERFORM VARYING i AS BINARY-LONG FROM 1 BY 1 UNTIL i > 10
               DISPLAY self::Quickselect(input-array, 1, input-array::Length, i)
                   NO ADVANCING

               IF i < 10
                   DISPLAY ", " NO ADVANCING
               END-IF
           END-PERFORM
           DISPLAY SPACE
       END METHOD.
       END CLASS.
```



## Common Lisp

```lisp

(defun quickselect (n _list)
  (let* ((ys (remove-if (lambda (x) (< (car _list) x)) (cdr _list)))
         (zs (remove-if-not (lambda (x) (< (car _list) x)) (cdr _list)))
         (l (length ys))
         )
    (cond ((< n l) (quickselect n ys))
          ((> n l) (quickselect (- n l 1) zs))
          (t (car _list)))
    )
  )

(defparameter a '(9 8 7 6 5 0 1 2 3 4))
(format t "~a~&" (mapcar (lambda (x) (quickselect x a)) (loop for i from 0 below (length a) collect i)))

```

```txt

(0 1 2 3 4 5 6 7 8 9)

```



## Crystal

```ruby
def quickselect(a, k)
  arr = a.dup # we will be modifying it
  loop do
    pivot = arr.delete_at(rand(arr.size))
    left, right = arr.partition { |x| x < pivot }
    if k == left.size
      return pivot
    elsif k < left.size
      arr = left
    else
      k = k - left.size - 1
      arr = right
    end
  end
end

v = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
p v.each_index.map { |i| quickselect(v, i) }.to_a

```

```txt

[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

```



## D


### Standard Version

This could use a different algorithm:

```d
void main() {
    import std.stdio, std.algorithm;

    auto a = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4];
    foreach (immutable i; 0 .. a.length) {
        a.topN(i);
        write(a[i], " ");
    }
}
```

```txt
0 1 2 3 4 5 6 7 8 9
```



### Array Version

```d
import std.stdio, std.random, std.algorithm, std.range;

T quickSelect(T)(T[] arr, size_t n)
in {
    assert(n < arr.length);
} body {
    static size_t partition(T[] sub, in size_t pivot) pure nothrow
    in {
        assert(!sub.empty);
        assert(pivot < sub.length);
    } body {
        auto pivotVal = sub[pivot];
        sub[pivot].swap(sub.back);
        size_t storeIndex = 0;
        foreach (ref si; sub[0 .. $ - 1]) {
            if (si < pivotVal) {
                si.swap(sub[storeIndex]);
                storeIndex++;
            }
        }
        sub.back.swap(sub[storeIndex]);
        return storeIndex;
    }

    size_t left = 0;
    size_t right = arr.length - 1;
    while (right > left) {
        assert(left < arr.length);
        assert(right < arr.length);
        immutable pivotIndex = left + partition(arr[left .. right + 1],
            uniform(0U, right - left + 1));
        if (pivotIndex - left == n) {
            right = left = pivotIndex;
        } else if (pivotIndex - left < n) {
            n -= pivotIndex - left + 1;
            left = pivotIndex + 1;
        } else {
            right = pivotIndex - 1;
        }
    }

    return arr[left];
}

void main() {
    auto a = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4];
    a.length.iota.map!(i => a.quickSelect(i)).writeln;
}
```

```txt
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Elixir

```elixir
defmodule Quick do
  def select(k, [x|xs]) do
    {ys, zs} = Enum.partition(xs, fn e -> e < x end)
    l = length(ys)
    cond do
      k < l -> select(k, ys)
      k > l -> select(k - l - 1, zs)
      true  -> x
    end
  end

  def test do
    v = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
    Enum.map(0..length(v)-1, fn i -> select(i,v) end)
    |> IO.inspect
  end
end

Quick.test
```


```txt

[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

```



## Erlang

```erlang

-module(quickselect).

-export([test/0]).


test() ->
    V = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4],
    lists:map(
        fun(I) -> quickselect(I,V) end,
        lists:seq(0, length(V) - 1)
    ).

quickselect(K, [X | Xs]) ->
    {Ys, Zs} =
        lists:partition(fun(E) -> E < X end, Xs),
    L = length(Ys),
    if
        K < L ->
            quickselect(K, Ys);
        K > L ->
            quickselect(K - L - 1, Zs);
        true ->
            X
    end.

```


Output:

```txt

[0,1,2,3,4,5,6,7,8,9]

```



## Fortran

Conveniently, a function was already to hand for floating-point numbers and changing the type was trivial - because the array and its associates were declared in the same statement to facilitate exactly that. The style is F77 (except for the usage of a PARAMETER statement in TEST to set up the specific test, and the A(1:N) usage in the DATA statement, and the END FUNCTION usage) and it did not seem worthwhile activating the MODULE protocol of F90 just to save the tedium of having to declare INTEGER FINDELEMENT in the calling routine - doing so would require four additional lines... On the other hand, a MODULE would enable the convenient development of a collection of near-clones, one for each type of array (INTEGER, REAL*4, REAL*8) which could then be collected via an INTERFACE statement into forming an apparently generic function so that one needn't have to remember FINDELEMENTI2, FINDELEMENTI4, FINDELEMENTF4, FINDELEMENTF8, and so on. With multiple parameters of various types, the combinations soon become tiresomely numerous.

Those of a delicate disposition may wish to avert their eyes from the three-way IF-statement...
```Fortran
      INTEGER FUNCTION FINDELEMENT(K,A,N)	!I know I can.
Chase an order statistic: FindElement(N/2,A,N) leads to the median, with some odd/even caution.
Careful! The array is shuffled: for i < K, A(i) <= A(K); for i > K, A(i) >= A(K).
Charles Anthony Richard Hoare devised this method, as related to his famous QuickSort.
       INTEGER K,N		!Find the K'th element in order of an array of N elements, not necessarily in order.
       INTEGER A(N),HOPE,PESTY	!The array, and like associates.
       INTEGER L,R,L2,R2	!Fingers.
        L = 1			!Here we go.
        R = N			!The bounds of the work area within which the K'th element lurks.
        DO WHILE (L .LT. R)	!So, keep going until it is clamped.
          HOPE = A(K)		!If array A is sorted, this will be rewarded.
          L2 = L		!But it probably isn't sorted.
          R2 = R		!So prepare a scan.
          DO WHILE (L2 .LE. R2)	!Keep squeezing until the inner teeth meet.
            DO WHILE (A(L2) .LT. HOPE)	!Pass elements less than HOPE.
              L2 = L2 + 1		!Note that at least element A(K) equals HOPE.
            END DO			!Raising the lower jaw.
            DO WHILE (HOPE .LT. A(R2))	!Elements higher than HOPE
              R2 = R2 - 1		!Are in the desired place.
            END DO			!And so we speed past them.
            IF (L2 - R2) 1,2,3	!How have the teeth paused?
    1       PESTY = A(L2)		!On grit. A(L2) > HOPE and A(R2) < HOPE.
            A(L2) = A(R2)		!So swap the two troublemakers.
            A(R2) = PESTY		!To be as if they had been in the desired order all along.
    2       L2 = L2 + 1		!Advance my teeth.
            R2 = R2 - 1		!As if they hadn't paused on this pest.
    3     END DO		!And resume the squeeze, hopefully closing in K.
          IF (R2 .LT. K) L = L2	!The end point gives the order position of value HOPE.
          IF (K .LT. L2) R = R2	!But we want the value of order position K.
        END DO			!Have my teeth met yet?
        FINDELEMENT = A(K)	!Yes. A(K) now has the K'th element in order.
      END FUNCTION FINDELEMENT	!Remember! Array A has likely had some elements moved!

      PROGRAM POKE
      INTEGER FINDELEMENT	!Not the default type for F.
      INTEGER N			!The number of elements.
      PARAMETER (N = 10)	!Fixed for the test problem.
      INTEGER A(66)		!An array of integers.
      DATA A(1:N)/9, 8, 7, 6, 5, 0, 1, 2, 3, 4/	!The specified values.

      WRITE (6,1) A(1:N)	!Announce, and add a heading.
    1 FORMAT ("Selection of the i'th element in order from an array.",/
     1 "The array need not be in order, and may be reordered.",/
     2 "  i Val:Array elements...",/,8X,666I2)

      DO I = 1,N	!One by one,
        WRITE (6,2) I,FINDELEMENT(I,A,N),A(1:N)	!Request the i'th element.
    2   FORMAT (I3,I4,":",666I2)	!Match FORMAT 1.
      END DO		!On to the next trial.

      END	!That was easy.
```


To demonstrate that the array, if unsorted, will likely have elements re-positioned, the array's state after each call is shown.
```txt
Selection of the i'th element in order from an array.
The array need not be in order, and may be reordered.
  i Val:Array elements...
         9 8 7 6 5 0 1 2 3 4
  1   0: 0 2 1 3 5 6 7 8 4 9
  2   1: 0 1 2 3 5 6 7 8 4 9
  3   2: 0 1 2 3 5 6 7 8 4 9
  4   3: 0 1 2 3 5 6 7 8 4 9
  5   4: 0 1 2 3 4 6 7 8 5 9
  6   5: 0 1 2 3 4 5 7 8 6 9
  7   6: 0 1 2 3 4 5 6 8 7 9
  8   7: 0 1 2 3 4 5 6 7 8 9
  9   8: 0 1 2 3 4 5 6 7 8 9
 10   9: 0 1 2 3 4 5 6 7 8 9

```


Given an intention to make many calls on FINDELEMENT for the same array, the array might as well be fully sorted first by a routine specialising in that. Otherwise, if say going for quartiles, it would be better to start with the median and work out so as to have a better chance of avoiding unfortunate "pivot" values.

=={{header|F Sharp|F#}}==
```fsharp

let rec quickselect k list =
    match list with
    | [] -> failwith "Cannot take largest element of empty list."
    | [a] -> a
    | x::xs ->
        let (ys, zs) = List.partition (fun arg -> arg < x) xs
        let l = List.length ys
        if k < l then quickselect k ys
        elif k > l then quickselect (k-l-1) zs
        else x
//end quickselect

[<EntryPoint>]
let main args =
    let v = [9; 8; 7; 6; 5; 0; 1; 2; 3; 4]
    printfn "%A" [for i in 0..(List.length v - 1) -> quickselect i v]
    0

```

```txt
[0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
```



## Factor

```factor
USING: combinators kernel make math locals prettyprint sequences ;
IN: rosetta-code.quickselect

:: quickselect ( k seq -- n )
    seq unclip :> ( xs x )
    xs [ x < ] partition :> ( ys zs )
    ys length :> l
    {
        { [ k l < ] [ k ys quickselect ] }
        { [ k l > ] [ k l - 1 - zs quickselect ] }
        [ x ]
    } cond ;

: quickselect-demo ( -- )
    { 9 8 7 6 5 0 1 2 3 4 } dup length <iota> swap
    [ [ quickselect , ] curry each ] { } make . ;

MAIN: quickselect-demo
```

```txt

{ 0 1 2 3 4 5 6 7 8 9 }

```



## Go


```go
package main

import "fmt"

func quickselect(list []int, k int) int {
    for {
        // partition
        px := len(list) / 2
        pv := list[px]
        last := len(list) - 1
        list[px], list[last] = list[last], list[px]
        i := 0
        for j := 0; j < last; j++ {
            if list[j] < pv {
                list[i], list[j] = list[j], list[i]
                i++
            }
        }
        // select
        if i == k {
            return pv
        }
        if k < i {
            list = list[:i]
        } else {
            list[i], list[last] = list[last], list[i]
            list = list[i+1:]
            k -= i + 1
        }
    }
}

func main() {
    for i := 0; ; i++ {
        v := []int{9, 8, 7, 6, 5, 0, 1, 2, 3, 4}
        if i == len(v) {
            return
        }
        fmt.Println(quickselect(v, i))
    }
}
```

```txt

0
1
2
3
4
5
6
7
8
9

```


A more generic version that works for any container that conforms to <code>sort.Interface</code>:


```go
package main

import (
    "fmt"
    "sort"
    "math/rand"
)

func partition(a sort.Interface, first int, last int, pivotIndex int) int {
    a.Swap(first, pivotIndex) // move it to beginning
    left := first+1
    right := last
    for left <= right {
        for left <= last && a.Less(left, first) {
            left++
        }
        for right >= first && a.Less(first, right) {
            right--
        }
        if left <= right {
            a.Swap(left, right)
            left++
            right--
        }
    }
    a.Swap(first, right) // swap into right place
    return right
}

func quickselect(a sort.Interface, n int) int {
    first := 0
    last := a.Len()-1
    for {
        pivotIndex := partition(a, first, last,
	                        rand.Intn(last - first + 1) + first)
        if n == pivotIndex {
            return pivotIndex
        } else if n < pivotIndex {
            last = pivotIndex-1
        } else {
            first = pivotIndex+1
        }
    }
    panic("bad index")
}

func main() {
    for i := 0; ; i++ {
        v := []int{9, 8, 7, 6, 5, 0, 1, 2, 3, 4}
        if i == len(v) {
            return
        }
        fmt.Println(v[quickselect(sort.IntSlice(v), i)])
    }
}
```

```txt

0
1
2
3
4
5
6
7
8
9

```



## Haskell


```haskell
import Data.List (partition)

quickselect :: Ord a => Int -> [a] -> a
quickselect k (x:xs) | k < l     = quickselect k ys
                     | k > l     = quickselect (k-l-1) zs
                     | otherwise = x
  where (ys, zs) = partition (< x) xs
        l = length ys

main :: IO ()
main = do
  let v = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
  print $ map (\i -> quickselect i v) [0 .. length v-1]
```

```txt
[0,1,2,3,4,5,6,7,8,9]
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.

```unicon
procedure main(A)
    every writes(" ",select(1 to *A, A, 1, *A)|"\n")
end

procedure select(k,A,min,max)
    repeat {
        pNI := partition(?(max-min)+min, A, min, max)
        pD := pNI - min + 1
        if pD = k then return A[pNI]
        if k < pD then max := pNI-1
        else (k -:= pD, min := pNI+1)
        }
end

procedure partition(pivot,A,min,max)
    pV := (A[max] :=: A[pivot])
    sI := min
    every A[i := min to max-1] <= pV do (A[sI] :=: A[i], sI +:= 1)
    A[max] :=: A[sI]
    return sI
end
```


Sample run:

```txt

->qs 9 8 7 6 5 0 1 2 3 4
 0 1 2 3 4 5 6 7 8 9
->

```



## J


Caution: as defined, we should expect performance on this task to be bad. Quickselect is optimized for selecting a single element from a list, with best-case performance of O(n) and worst case performance of O(n^2). If we use it to select most of the items from a list, the overall task performance will be O(n^2) best case and O(n^3) worst case. If we really wanted to perform this task efficiently, we would first sort the list and then extract the desired elements. But we do not really want to be efficient here, and maybe that is the point.

Further caution: this task asks us to select "the first, second, third, ... up to the tenth largest member of the vector". But we also cannot know, apriori, what value is the first, second, third, ... largest member. So to accomplish this task we are first going to have to sort the list. But We Will Use Quickselect - that is the specification, after all. Perhaps this task should be taken as an illustration of how silly specifications can sometimes be. We need to have a good sense of humor, after all.

Another caution: quick select simply selects a value that matches. So in the simple case it's an identity operation. When we select a 5 from a list, we get a 5 back out. We can imagine that there might be cases where the thing we get back out is a more complicated data structure. But whether that is really efficient, or not, depends on other factors.

Final caution: a brute-force linear scan of a list is O(n) best case and O(n) worst case. A binary search on an ordered list tends to be faster. So when you hear someone talking about efficiency, you might want to ask "efficient at what?" In this case, I think there might be room for further clarification of that issue (but that makes this a good object lesson - in the real world there are many examples of presentations of ideas which sound great but where other alternatives might be significantly better).

With that out of the way, here's a pedantic (and laughably inefficient) implementation of quickselect:


```J
quickselect=:4 :0
  if. 0=#y do. _ return. end.
  n=.?#y
  m=.n{y
  if. x < m do.
    x quickselect (m>y)#y
  else.
    if. x > m do.
      x quickselect (m<y)#y
    else.
      m
    end.
  end.
)
```


"Proof" that it works:


```J
   8 quickselect 9, 8, 7, 6, 5, 0, 1, 2, 3, 4
8
```


And, the required task example:


```J
   ((10 {./:~) quickselect"0 1 ]) 9, 8, 7, 6, 5, 0, 1, 2, 3, 4
0 1 2 3 4 5 6 7 8 9
```


(Insert here: puns involving greater transparency, the emperor's new clothes, burlesque and maybe the dance of the seven veils.)


## Java


```java
import java.util.Random;

public class QuickSelect {

	private static <E extends Comparable<? super E>> int partition(E[] arr, int left, int right, int pivot) {
		E pivotVal = arr[pivot];
		swap(arr, pivot, right);
		int storeIndex = left;
		for (int i = left; i < right; i++) {
			if (arr[i].compareTo(pivotVal) < 0) {
				swap(arr, i, storeIndex);
				storeIndex++;
			}
		}
		swap(arr, right, storeIndex);
		return storeIndex;
	}

	private static <E extends Comparable<? super E>> E select(E[] arr, int n) {
		int left = 0;
		int right = arr.length - 1;
		Random rand = new Random();
		while (right >= left) {
			int pivotIndex = partition(arr, left, right, rand.nextInt(right - left + 1) + left);
			if (pivotIndex == n) {
				return arr[pivotIndex];
			} else if (pivotIndex < n) {
				left = pivotIndex + 1;
			} else {
				right = pivotIndex - 1;
			}
		}
		return null;
	}

	private static void swap(Object[] arr, int i1, int i2) {
		if (i1 != i2) {
			Object temp = arr[i1];
			arr[i1] = arr[i2];
			arr[i2] = temp;
		}
	}

	public static void main(String[] args) {
		for (int i = 0; i < 10; i++) {
			Integer[] input = {9, 8, 7, 6, 5, 0, 1, 2, 3, 4};
			System.out.print(select(input, i));
			if (i < 9) System.out.print(", ");
		}
		System.out.println();
	}

}
```


```txt
0, 1, 2, 3, 4, 5, 6, 7, 8, 9
```



## Javascript


### ES5


```javascript
// this just helps make partition read better
function swap(items, firstIndex, secondIndex) {
  var temp = items[firstIndex];
  items[firstIndex] = items[secondIndex];
  items[secondIndex] = temp;
};

// many algorithms on this page violate
// the constraint that partition operates in place
function partition(array, from, to) {
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
  var pivotIndex = getRandomInt(from, to),
      pivot = array[pivotIndex];
  swap(array, pivotIndex, to);
  pivotIndex = from;

  for(var i = from; i <= to; i++) {
    if(array[i] < pivot) {
      swap(array, pivotIndex, i);
      pivotIndex++;
    }
  };
  swap(array, pivotIndex, to);

  return pivotIndex;
};

// later versions of JS have TCO so this is safe
function quickselectRecursive(array, from, to, statistic) {
  if(array.length === 0 || statistic > array.length - 1) {
    return undefined;
  };

  var pivotIndex = partition(array, from, to);
  if(pivotIndex === statistic) {
    return array[pivotIndex];
  } else if(pivotIndex < statistic) {
    return quickselectRecursive(array, pivotIndex, to, statistic);
  } else if(pivotIndex > statistic) {
    return quickselectRecursive(array, from, pivotIndex, statistic);
  }
};

function quickselectIterative(array, k) {
  if(array.length === 0 || k > array.length - 1) {
    return undefined;
  };

  var from = 0, to = array.length,
      pivotIndex = partition(array, from, to);

  while(pivotIndex !== k) {
    pivotIndex = partition(array, from, to);
    if(pivotIndex < k) {
      from = pivotIndex;
    } else if(pivotIndex > k) {
      to = pivotIndex;
    }
  };

  return array[pivotIndex];
};

KthElement = {
  find: function(array, element) {
    var k = element - 1;
    return quickselectRecursive(array, 0, array.length, k);
    // you can also try out the Iterative version
    // return quickselectIterative(array, k);
  }
}
```


'''Example''':

```Javascript

var array = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4],
    ks = Array.apply(null, {length: 10}).map(Number.call, Number);
ks.map(k => { KthElement.find(array, k) });
```

```JavaScript
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
```



### ES6

```JavaScript
(() => {
    'use strict';

    // QUICKSELECT ------------------------------------------------------------

    // quickselect :: Ord a => Int -> [a] -> a
    const quickSelect = (k, xxs) => {
        const
            [x, xs] = uncons(xxs),
            [ys, zs] = partition(v => v < x, xs),
            l = length(ys);

        return (k < l) ? (
            quickSelect(k, ys)
        ) : (k > l) ? (
            quickSelect(k - l - 1, zs)
        ) : x;
    };


    // GENERIC FUNCTIONS ------------------------------------------------------

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // length :: [a] -> Int
    const length = xs => xs.length;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // partition :: Predicate -> List -> (Matches, nonMatches)
    // partition :: (a -> Bool) -> [a] -> ([a], [a])
    const partition = (p, xs) =>
        xs.reduce((a, x) =>
            p(x) ? [a[0].concat(x), a[1]] : [a[0], a[1].concat(x)], [
                [],
                []
            ]);

    // uncons :: [a] -> Maybe (a, [a])
    const uncons = xs => xs.length ? [xs[0], xs.slice(1)] : undefined;


    // TEST -------------------------------------------------------------------
    const v = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4];

    return map(i => quickSelect(i, v), enumFromTo(0, length(v) - 1));
})();
```

```JavaScript
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## jq

```jq
# Emit the k-th smallest item in the input array,
# or nothing if k is too small or too large.
# The smallest corresponds to k==1.
# The input array may hold arbitrary JSON entities, including null.
def quickselect(k):

  def partition(pivot):
    reduce .[] as $x
      # state: [less, other]
      ( [ [], [] ];                       # two empty arrays:
        if    $x  < pivot
        then .[0] += [$x]                 # add x to less
        else .[1] += [$x]                 # add x to other
        end
      );

  # recursive inner function has arity 0 for efficiency
  def qs:  # state: [kn, array] where kn counts from 0
    .[0] as $kn
     | .[1] as $a
    | $a[0] as $pivot
    | ($a[1:] | partition($pivot)) as $p
    | $p[0] as $left
    | ($left|length) as $ll
    | if   $kn == $ll then $pivot
      elif $kn <  $ll then [$kn, $left] | qs
      else [$kn - $ll - 1, $p[1] ] | qs
      end;

  if length < k or k <= 0 then empty else [k-1, .] | qs end;
```


'''Example''':
Notice that values of k that are too large or too small generate nothing.

```jq
(0, 12, range(1;11)) as $k
 | [9, 8, 7, 6, 5, 0, 1, 2, 3, 4] | quickselect($k)
 | "k=\($k) => \(.)"
```

```sh
$ jq -n -r -f quickselect.jq
k=1 => 0
k=2 => 1
k=3 => 2
k=4 => 3
k=5 => 4
k=6 => 5
k=7 => 6
k=8 => 7
k=9 => 8
k=10 => 9
$
```



## Julia

Using builtin function <code>select</code>:

```julia
v = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
@show v select(v, 1:10)

```


```txt
v = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
select(v, 1:10) = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Kotlin


```scala
// version 1.1.2

const val MAX = Int.MAX_VALUE
val rand = java.util.Random()

fun partition(list:IntArray, left: Int, right:Int, pivotIndex: Int): Int {
    val pivotValue = list[pivotIndex]
    list[pivotIndex] = list[right]
    list[right] = pivotValue
    var storeIndex = left
    for (i in left until right) {
        if (list[i] < pivotValue) {
            val tmp = list[storeIndex]
            list[storeIndex] = list[i]
            list[i] = tmp
            storeIndex++
        }
    }
    val temp = list[right]
    list[right] = list[storeIndex]
    list[storeIndex] = temp
    return storeIndex
}

tailrec fun quickSelect(list: IntArray, left: Int, right: Int, k: Int): Int {
    if (left == right) return list[left]
    var pivotIndex = left + Math.floor((rand.nextInt(MAX) % (right - left + 1)).toDouble()).toInt()
    pivotIndex = partition(list, left, right, pivotIndex)
    if (k == pivotIndex)
        return list[k]
    else if (k < pivotIndex)
        return quickSelect(list, left, pivotIndex - 1, k)
    else
        return quickSelect(list, pivotIndex + 1, right, k)
}

fun main(args: Array<String>) {
    val list = intArrayOf(9, 8, 7, 6, 5, 0, 1, 2, 3, 4)
    val right = list.size - 1
    for (k in 0..9) {
        print(quickSelect(list, 0, right, k))
        if (k < 9) print(", ")
    }
    println()
}
```


```txt

0, 1, 2, 3, 4, 5, 6, 7, 8, 9

```



## Lua


```Lua
function partition (list, left, right, pivotIndex)
    local pivotValue = list[pivotIndex]
    list[pivotIndex], list[right] = list[right], list[pivotIndex]
    local storeIndex = left
    for i = left, right do
        if list[i] < pivotValue then
            list[storeIndex], list[i] = list[i], list[storeIndex]
            storeIndex = storeIndex + 1
        end
    end
    list[right], list[storeIndex] = list[storeIndex], list[right]
    return storeIndex
end

function quickSelect (list, left, right, n)
    local pivotIndex
    while 1 do
        if left == right then return list[left] end
        pivotIndex = math.random(left, right)
        pivotIndex = partition(list, left, right, pivotIndex)
        if n == pivotIndex then
            return list[n]
        elseif n < pivotIndex then
            right = pivotIndex - 1
        else
            left = pivotIndex + 1
        end
    end
end

math.randomseed(os.time())
local vec = {9, 8, 7, 6, 5, 0, 1, 2, 3, 4}
for i = 1, 10 do print(i, quickSelect(vec, 1, #vec, i) .. " ") end
```

```txt
1       0
2       1
3       2
4       3
5       4
6       5
7       6
8       7
9       8
10      9
```



## Maple


```Maple
part := proc(arr, left, right, pivot)
	local val,safe,i:
	val := arr[pivot]:
	arr[pivot], arr[right] := arr[right], arr[pivot]:
	safe := left:
	for i from left to right do
		if arr[i] < val then
			arr[safe], arr[i] := arr[i], arr[safe]:
			safe := safe + 1:
		end if:
	end do:
	arr[right], arr[safe] := arr[safe], arr[right]:
	return safe:
end proc:

quickselect := proc(arr,k)
	local pivot,left,right:
	left,right := 1,numelems(arr):
	while(true)do
		if left = right then return arr[left]: end if:
		pivot := trunc((left+right)/2);
		pivot := part(arr, left, right, pivot):
		if k = pivot then
			return arr[k]:
		elif k < pivot then
			right := pivot-1:
		else
			left := pivot+1:
		end if:
	end do:
end proc:
roll := rand(1..20):
demo := Array([seq(roll(), i=1..20)]);
map(x->printf("%d ", x), demo):
print(quickselect(demo,7)):
print(quickselect(demo,14)):
```

```txt
5 4 2 1 3 6 8 11 11 11 8 11 9 11 16 20 20 18 17 16
8
11
```




## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary
/** @see <a href="http://en.wikipedia.org/wiki/Quickselect">http://en.wikipedia.org/wiki/Quickselect</a> */

runSample(arg)
return

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
method qpartition(list, ileft, iright, pivotIndex) private static
  pivotValue = list[pivotIndex]
  list = swap(list, pivotIndex, iright) -- Move pivot to end
  storeIndex = ileft
  loop i_ = ileft to iright - 1
    if list[i_] <= pivotValue then do
      list = swap(list, storeIndex, i_)
      storeIndex = storeIndex + 1
      end
    end i_
  list = swap(list, iright, storeIndex) -- Move pivot to its final place
  return storeIndex

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
method qselectInPlace(list, k_, ileft = -1, iright = -1) public static
  if ileft  = -1 then ileft  = 1
  if iright = -1 then iright = list[0]

  loop label inplace forever
    pivotIndex = Random().nextInt(iright - ileft + 1) + ileft -- select pivotIndex between left and right
  pivotNewIndex = qpartition(list, ileft, iright, pivotIndex)
  pivotDist = pivotNewIndex - ileft + 1
  select
    when pivotDist = k_ then do
      returnVal = list[pivotNewIndex]
      leave inplace
      end
    when k_ < pivotDist then
      iright = pivotNewIndex - 1
    otherwise do
      k_ = k_ - pivotDist
      ileft = pivotNewIndex + 1
      end
    end
    end inplace
  return returnVal

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
method swap(list, i1, i2) private static
  if i1 \= i2 then do
    t1       = list[i1]
    list[i1] = list[i2]
    list[i2] = t1
    end
  return list

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg samplelist
  if samplelist = '' | samplelist = '.' then samplelist = 9 8 7 6 5 0 1 2 3 4
  items = samplelist.words
  say 'Input:'
  say '    'samplelist.space(1, ',').changestr(',', ', ')
  say

  say 'Using in-place version of the algorithm:'
  iv = ''
  loop k_ = 1 to items
    iv = iv qselectInPlace(buildIndexedString(samplelist), k_)
    end k_
  say '    'iv.space(1, ',').changestr(',', ', ')
  say

  say 'Find the 4 smallest:'
  iv = ''
  loop k_ = 1 to 4
    iv = iv qselectInPlace(buildIndexedString(samplelist), k_)
    end k_
  say '    'iv.space(1, ',').changestr(',', ', ')
  say

  say 'Find the 3 largest:'
  iv = ''
  loop k_ = items - 2 to items
    iv = iv qselectInPlace(buildIndexedString(samplelist), k_)
    end k_
  say '    'iv.space(1, ',').changestr(',', ', ')
  say

  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method buildIndexedString(samplelist) private static
  list = 0
  list[0] = samplelist.words()
  loop k_ = 1 to list[0]
    list[k_] = samplelist.word(k_)
    end k_
  return list

```

```txt

Input:
    9, 8, 7, 6, 5, 0, 1, 2, 3, 4

Using in-place version of the algorithm:
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9

Find the 4 smallest:
    0, 1, 2, 3

Find the 3 largest:
    7, 8, 9

```



## Nim


```nim
proc qselect[T](a: var openarray[T]; k: int, inl = 0, inr = -1): T =
  var r = if inr >= 0: inr else: a.high
  var st = 0
  for i in 0 ..< r:
    if a[i] > a[r]: continue
    swap a[i], a[st]
    inc st

  swap a[r], a[st]

  if k == st:  a[st]
  elif st > k: qselect(a, k, 0, st - 1)
  else:        qselect(a, k, st, inr)

let x = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]

for i in 0..9:
  var y = x
  echo i, ": ", qselect(y, i)
```

Output:

```txt
0: 0
1: 1
2: 2
3: 3
4: 4
5: 5
6: 6
7: 7
8: 8
9: 9
```



## OCaml


```ocaml
let rec quickselect k = function
   [] -> failwith "empty"
 | x :: xs -> let ys, zs = List.partition ((>) x) xs in
              let l = List.length ys in
              if k < l then
                quickselect k ys
              else if k > l then
                quickselect (k-l-1) zs
              else
                x
```

Usage:

```txt

# let v = [9; 8; 7; 6; 5; 0; 1; 2; 3; 4];;
val v : int list = [9; 8; 7; 6; 5; 0; 1; 2; 3; 4]
# Array.init 10 (fun i -> quickselect i v);;
- : int array = [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|]

```



## PARI/GP


```parigp
part(list, left, right, pivotIndex)={
  my(pivotValue=list[pivotIndex],storeIndex=left,t);
  t=list[pivotIndex];
  list[pivotIndex]=list[right];
  list[right]=t;
  for(i=left,right-1,
    if(list[i] <= pivotValue,
      t=list[storeIndex];
      list[storeIndex]=list[i];
      list[i]=t;
      storeIndex++
    )
  );
  t=list[right];
  list[right]=list[storeIndex];
  list[storeIndex]=t;
  storeIndex
};
quickselect(list, left, right, n)={
  if(left==right,return(list[left]));
  my(pivotIndex=part(list, left, right, random(right-left)+left));
  if(pivotIndex==n,return(list[n]));
  if(n < pivotIndex,
    quickselect(list, left, pivotIndex - 1, n)
  ,
    quickselect(list, pivotIndex + 1, right, n)
  )
};
```



## Perl


```Perl
my @list = qw(9 8 7 6 5 0 1 2 3 4);
print join ' ', map { qselect(\@list, $_) } 1 .. 10 and print "\n";

sub qselect
{
    my ($list, $k) = @_;
    my $pivot = @$list[int rand @{ $list } - 1];
    my @left  = grep { $_ < $pivot } @$list;
    my @right = grep { $_ > $pivot } @$list;
    if ($k <= @left)
    {
        return qselect(\@left, $k);
    }
    elsif ($k > @left + 1)
    {
        return qselect(\@right, $k - @left - 1);
    }
    else { $pivot }
}
```


```txt
0 1 2 3 4 5 6 7 8 9
```



## Perl 6

```perl6
my @v = <9 8 7 6 5 0 1 2 3 4>
;
say map { select(@v, $_) }, 1 .. 10;

sub partition(@vector, $left, $right, $pivot-index) {
    my $pivot-value = @vector[$pivot-index];
    @vector[$pivot-index, $right] = @vector[$right, $pivot-index];
    my $store-index = $left;
    for $left ..^ $right -> $i {
        if @vector[$i] < $pivot-value {
            @vector[$store-index, $i] = @vector[$i, $store-index];
            $store-index++;
        }
    }
    @vector[$right, $store-index] = @vector[$store-index, $right];
    return $store-index;
}

sub select( @vector,
            \k where 1 .. @vector,
            \l where 0 .. @vector = 0,
            \r where l .. @vector = @vector.end ) {

    my ($k, $left, $right) = k, l, r;

    loop {
        my $pivot-index = ($left..$right).pick;
        my $pivot-new-index = partition(@vector, $left, $right, $pivot-index);
        my $pivot-dist = $pivot-new-index - $left + 1;
        given $pivot-dist <=> $k {
            when Same {
                return @vector[$pivot-new-index];
            }
            when More {
                $right = $pivot-new-index - 1;
            }
            when Less {
                $k -= $pivot-dist;
                $left = $pivot-new-index + 1;
            }
        }
    }
}
```

```txt
0 1 2 3 4 5 6 7 8 9
```



## Phix

Note the (three) commented-out multiple assignments are nowhere near as performant as the long-hand equivalents;
perhaps there may be a way to narrow down the divide in some future release of the compiler...


```Phix
global function quick_select(sequence s, integer k)
integer left = 1, right = length(s), pos
object pivotv, tmp

    while left<right do
        pivotv = s[k];
--      {s[k], s[right]} = {s[right], s[k]}
        tmp = s[k]
        s[k] = s[right]
        s[right]=tmp
        pos = left
        for i=left to right do
            if s[i]<pivotv then
--              {s[i], s[pos]} = {s[pos], s[i]}
                tmp = s[i]
                s[i] = s[pos]
                s[pos]=tmp
                pos += 1
            end if
        end for
--      {s[right], s[pos]} = {s[pos], s[right]}
        tmp = s[right]
        s[right] = s[pos]
        s[pos]=tmp
        if pos==k then exit end if
        if pos<k then
            left = pos + 1
        else
            right = pos - 1
        end if
    end while
    return {s,s[k]}
end function

sequence s = {9, 8, 7, 6, 5, 0, 1, 2, 3, 4}
integer r
for i=1 to 10 do
    {s,r} = quick_select(s,i)
    printf(1," %d",r)
end for
{} = wait_key()
```

```txt

 0 1 2 3 4 5 6 7 8 9

```



## PicoLisp


```PicoLisp
(seed (in "/dev/urandom" (rd 8)))
(de swapL (Lst X Y)
   (let L (nth Lst Y)
      (swap
         L
         (swap (nth Lst X) (car L)) ) ) )
(de partition (Lst L R P)
   (let V (get Lst P)
      (swapL Lst R P)
      (for I (range L R)
         (and
            (> V (get Lst I))
            (swapL Lst L I)
            (inc 'L) ) )
      (swapL Lst L R)
      L ) )
(de quick (Lst N L R)
   (default L (inc N)  R (length Lst))
   (if (= L R)
      (get Lst L)
      (let P (partition Lst L R (rand L R))
         (cond
            ((= N P) (get Lst N))
            ((> P N) (quick Lst N L P))
            (T (quick Lst N P R)) ) ) ) )
(let Lst (9 8 7 6 5 0 1 2 3 4)
   (println
      (mapcar
         '((N) (quick Lst N))
         (range 0 9) ) ) )
```

```txt
(0 1 2 3 4 5 6 7 8 9)
```



## PL/I


```PL/I

quick: procedure options (main); /* 4 April 2014 */

partition: procedure (list, left, right, pivot_Index) returns (fixed binary);
   declare list (*) fixed binary;
   declare (left, right, pivot_index) fixed binary;
   declare (store_index, pivot_value) fixed binary;
   declare I fixed binary;

     pivot_Value = list(pivot_Index);
     call swap (pivot_Index, right);  /* Move pivot to end */
     store_Index = left;
     do i = left to right-1;
         if list(i) < pivot_Value then
            do;
               call swap (store_Index, i);
               store_Index = store_index + 1;
            end;
     end;
     call swap (right, store_Index);  /* Move pivot to its final place */
     return (store_Index);

swap: procedure (i, j);
   declare (i, j) fixed binary; declare t fixed binary;

   t = list(i); list(i) = list(j); list(j) = t;
end swap;
end partition;

/* Returns the n-th smallest element of list within left..right inclusive */
/* (i.e. left <= n <= right). */
quick_select: procedure (list, left, right, n) recursive returns (fixed binary);
   declare list(*)          fixed binary;
   declare (left, right, n) fixed binary;
   declare pivot_index      fixed binary;

     if left = right then       /* If the list contains only one element */
         return ( list(left) ); /* Return that element                   */
     pivot_Index  = (left+right)/2;
         /* select a pivot_Index between left and right, */
         /* e.g. left + Math.floor(Math.random() * (right - left + 1)) */
     pivot_Index  = partition(list, left, right, pivot_Index);
     /* The pivot is in its final sorted position. */
     if n = pivot_Index then
         return ( list(n) );
     else if n < pivot_Index then
         return ( quick_select(list, left, pivot_Index - 1, n) );
     else
         return ( quick_select(list, pivot_Index + 1, right, n) );

end quick_select;

   declare a(10) fixed binary static initial (9, 8, 7, 6, 5, 0, 1, 2, 3, 4);
   declare I fixed binary;

   do i = 1 to 10;
      put skip edit ('The ', trim(i), '-th element is ', quick_select((a), 1, 10, (i) )) (a);
   end;

end quick;
```

Output:

```txt

The 1-th element is         0
The 2-th element is         1
The 3-th element is         2
The 4-th element is         3
The 5-th element is         4
The 6-th element is         5
The 7-th element is         6
The 8-th element is         7
The 9-th element is         8
The 10-th element is         9

```



## PowerShell


```PowerShell

 function partition($list, $left, $right, $pivotIndex) {
     $pivotValue = $list[$pivotIndex]
     $list[$pivotIndex], $list[$right] = $list[$right], $list[$pivotIndex]
     $storeIndex = $left
     foreach ($i in $left..($right-1)) {
         if ($list[$i] -lt $pivotValue) {
             $list[$storeIndex],$list[$i] = $list[$i], $list[$storeIndex]
             $storeIndex += 1
         }
     }
     $list[$right],$list[$storeIndex] = $list[$storeIndex], $list[$right]
     $storeIndex
}

function rank($list, $left, $right, $n) {
    if ($left -eq $right) {$list[$left]}
    else {
        $pivotIndex = Get-Random -Minimum $left -Maximum $right
        $pivotIndex = partition $list $left $right $pivotIndex
        if ($n -eq $pivotIndex) {$list[$n]}
        elseif ($n -lt $pivotIndex) {(rank $list $left ($pivotIndex - 1) $n)}
        else {(rank $list ($pivotIndex+1) $right $n)}
    }
}

function quickselect($list) {
    $right = $list.count-1
    foreach($left in 0..$right) {rank $list $left $right $left}
}
$arr = @(9, 8, 7, 6, 5, 0, 1, 2, 3, 4)
"$(quickselect $arr)"

```

<b>Output:</b>

```txt

0 1 2 3 4 5 6 7 8 9

```



## PureBasic

A direct implementation of the Wikipedia pseudo-code.

```PureBasic

Procedure QuickPartition (Array L(1), left, right, pivotIndex)
     pivotValue = L(pivotIndex)
     Swap L(pivotIndex) , L(right); Move pivot To End
     storeIndex = left
     For i=left To right-1
         If L(i) < pivotValue
             Swap L(storeIndex),L(i)
             storeIndex+1
         EndIf
     Next i
     Swap L(right), L(storeIndex)  ; Move pivot To its final place
     ProcedureReturn storeIndex
 EndProcedure
Procedure QuickSelect(Array L(1), left, right, k)
    Repeat
         If left = right:ProcedureReturn L(left):EndIf
         pivotIndex.i= left; Select pivotIndex between left And right
         pivotIndex= QuickPartition(L(), left, right, pivotIndex)
         If k = pivotIndex
             ProcedureReturn L(k)
         ElseIf k < pivotIndex
             right= pivotIndex - 1
         Else
             left= pivotIndex + 1
         EndIf
    ForEver
EndProcedure
Dim L.i(9)
For i=0 To 9
    Read L(i)
Next i
DataSection
    Data.i 9, 8, 7, 6, 5, 0, 1, 2, 3, 4
EndDataSection
For i=0 To 9
    Debug QuickSelect(L(),0,9,i)
Next i
```

```txt
0 1 2 3 4 5 6 7 8 9
```



## Python


### Procedural

A direct implementation of the Wikipedia pseudo-code, using a random initial pivot. I added some input flexibility allowing sensible defaults for left and right function arguments.

```python
import random

def partition(vector, left, right, pivotIndex):
    pivotValue = vector[pivotIndex]
    vector[pivotIndex], vector[right] = vector[right], vector[pivotIndex]  # Move pivot to end
    storeIndex = left
    for i in range(left, right):
        if vector[i] < pivotValue:
            vector[storeIndex], vector[i] = vector[i], vector[storeIndex]
            storeIndex += 1
    vector[right], vector[storeIndex] = vector[storeIndex], vector[right]  # Move pivot to its final place
    return storeIndex

def _select(vector, left, right, k):
    "Returns the k-th smallest, (k >= 0), element of vector within vector[left:right+1] inclusive."
    while True:
        pivotIndex = random.randint(left, right)     # select pivotIndex between left and right
        pivotNewIndex = partition(vector, left, right, pivotIndex)
        pivotDist = pivotNewIndex - left
        if pivotDist == k:
            return vector[pivotNewIndex]
        elif k < pivotDist:
            right = pivotNewIndex - 1
        else:
            k -= pivotDist + 1
            left = pivotNewIndex + 1

def select(vector, k, left=None, right=None):
    """\
    Returns the k-th smallest, (k >= 0), element of vector within vector[left:right+1].
    left, right default to (0, len(vector) - 1) if omitted
    """
    if left is None:
        left = 0
    lv1 = len(vector) - 1
    if right is None:
        right = lv1
    assert vector and k >= 0, "Either null vector or k < 0 "
    assert 0 <= left <= lv1, "left is out of range"
    assert left <= right <= lv1, "right is out of range"
    return _select(vector, left, right, k)

if __name__ == '__main__':
    v = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
    print([select(v, i) for i in range(10)])
```


```txt
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



### Composition of pure functions

```python
'''Quick select'''

from functools import reduce


# quickselect :: Ord a => Int -> [a] -> a
def quickSelect(k):
    '''The kth smallest element
       in the unordered list xs.'''
    def go(k, xs):
        x = xs[0]

        def ltx(y):
            return y < x
        ys, zs = partition(ltx)(xs[1:])
        n = len(ys)
        return go(k, ys) if k < n else (
            go(k - n - 1, zs) if k > n else x
        )
    return lambda xs: go(k, xs) if xs else None


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test'''

    v = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
    print(list(map(
        flip(quickSelect)(v),
        range(0, len(v))
    )))


# GENERIC -------------------------------------------------


# flip :: (a -> b -> c) -> b -> a -> c
def flip(f):
    '''The (curried) function f with its
       arguments reversed.'''
    return lambda a: lambda b: f(b)(a)


# partition :: (a -> Bool) -> [a] -> ([a], [a])
def partition(p):
    '''The pair of lists of those elements in xs
       which respectively do, and don't
       satisfy the predicate p.'''
    def go(a, x):
        ts, fs = a
        return (ts + [x], fs) if p(x) else (ts, fs + [x])
    return lambda xs: reduce(go, xs, ([], []))


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Racket


```racket
(define (quickselect A k)
  (define pivot (list-ref A (random (length A))))
  (define A1 (filter (curry > pivot) A))
  (define A2 (filter (curry < pivot) A))
  (cond
    [(<= k (length A1)) (quickselect A1 k)]
    [(> k (- (length A) (length A2))) (quickselect A2 (- k (- (length A) (length A2))))]
    [else pivot]))

(define a '(9 8 7 6 5 0 1 2 3 4))
(display (string-join (map number->string (for/list ([k 10]) (quickselect a (+ 1 k)))) ", "))

```

```txt
0, 1, 2, 3, 4, 5, 6, 7, 8, 9
```



## REXX

===uses in-line swap===

```rexx
/*REXX program sorts a list (which may be numbers) by using the quick select algorithm. */
parse arg list;  if list=''  then list=9 8 7 6 5 0 1 2 3 4    /*Not given?  Use default.*/
say right('list: ', 22)           list
#=words(list)
              do i=1  for #;  @.i=word(list, i)  /*assign all the items ──► @. (array). */
              end   /*i*/                        /* [↑]  #: number of items in the list.*/
say
      do j=1  for #                              /*show  1 ──►  # items place and value.*/
      say right('item', 20)     right(j, length(#))",  value: "      qSel(1, #, j)
      end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
qPart: procedure expose @.;  parse arg L 1 ?,R,X;                xVal=@.X
       parse value  @.X @.R   with   @.R @.X     /*swap the two names items  (X and R). */
             do k=L  to R-1                      /*process the left side of the list.   */
             if @.k>xVal  then iterate           /*when an item > item #X, then skip it.*/
             parse value @.? @.k  with  @.k @.?  /*swap the two named items  (? and K). */
             ?=?+1                               /*bump the item number (point to next).*/
             end   /*k*/
       parse       value @.R @.?  with  @.? @.R  /*swap the two named items  (R and ?). */
       return ?                                  /*return the item number to invoker.   */
/*──────────────────────────────────────────────────────────────────────────────────────*/
qSel: procedure expose @.;  parse arg L,R,z;  if L==R  then return @.L  /*only one item?*/
         do forever                              /*keep searching until we're all done. */
         new=qPart(L, R, (L+R) % 2)              /*partition the list into roughly  ½.  */
         $=new-L+1                               /*calculate pivot distance less  L+1.  */
         if $==z  then return @.new              /*we're all done with this pivot part. */
                  else if  z<$  then     R=new-1 /*decrease the right half of the array.*/
                                else do; z=z-$   /*decrease the distance.               */
                                         L=new+1 /*increase the  left half *f the array.*/
                                     end
         end   /*forever*/
```

'''output'''   when using the default input:

```txt

                list:  9 8 7 6 5 0 1 2 3 4

                item  1,  value:  0
                item  2,  value:  1
                item  3,  value:  2
                item  4,  value:  3
                item  5,  value:  4
                item  6,  value:  5
                item  7,  value:  6
                item  8,  value:  7
                item  9,  value:  8
                item 10,  value:  9

```



### uses swap subroutine


```rexx
/*REXX program sorts a list (which may be numbers) by using the quick select algorithm. */
parse arg list;  if list=''  then list=9 8 7 6 5 0 1 2 3 4    /*Not given?  Use default.*/
say right('list: ', 22)           list
#=words(list)
              do i=1  for #;  @.i=word(list, i)  /*assign all the items ──► @. (array). */
              end   /*i*/                        /* [↑]  #: number of items in the list.*/
say
      do j=1  for #                              /*show  1 ──►  # items place and value.*/
      say right('item', 20)     right(j, length(#))",  value: "      qSel(1, #, j)
      end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
qPart: procedure expose @.;  parse arg L 1 ?,R,X;                xVal=@.X
       call swap X,R                             /*swap the two named items  (X and R). */
                      do k=L  to R-1             /*process the left side of the list.   */
                      if @.k>xVal  then iterate  /*when an item > item #X, then skip it.*/
                      call swap ?,k              /*swap the two named items  (? and K). */
                      ?=?+1                      /*bump the item number (point to next).*/
                      end   /*k*/
       call swap R,?                             /*swap the two named items  (R and ?). */
       return ?                                  /*return the item number to invoker.   */
/*──────────────────────────────────────────────────────────────────────────────────────*/
qSel: procedure expose @.;  parse arg L,R,z;  if L==R  then return @.L  /*only one item?*/
        do forever                               /*keep searching until we're all done. */
        new=qPart(L, R, (L+R)%2)                 /*partition the list into roughly  ½.  */
        $=new-L+1                                /*calculate the pivot distance less L+1*/
        if $==z  then return @.new               /*we're all done with this pivot part. */
                 else if  z<$  then     R=new-1  /*decrease the right half of the array.*/
                               else do; z=z-$    /*decrease the distance.               */
                                        L=new+1  /*increase the  left half of the array.*/
                               end
        end   /*forever*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
swap: parse arg _1,_2;  parse value @._1 @._2  with  @._2 @._1;  return  /*swap 2 items.*/
```

'''output'''   is the identical to the 1<sup>st</sup> REXX version.




## Ring


```ring

aList = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
see partition(aList, 9, 4, 2) + nl

func partition list, left, right, pivotIndex
       pivotValue = list[pivotIndex]
       temp = list[pivotIndex]
       list[pivotIndex] = list[right]
       list[right]  = temp
       storeIndex = left
       for i = left to right-1
            if list[i] < pivotValue
               temp = list[storeIndex]
               list[storeIndex] = list[i]
               list[i] = temp
               storeIndex++ ok
            temp = list[right]
            list[right] = list[storeIndex]
            list[storeIndex] = temp
       next
       return storeIndex

```



## Ruby


```ruby
def quickselect(a, k)
  arr = a.dup # we will be modifying it
  loop do
    pivot = arr.delete_at(rand(arr.length))
    left, right = arr.partition { |x| x < pivot }
    if k == left.length
      return pivot
    elsif k < left.length
      arr = left
    else
      k = k - left.length - 1
      arr = right
    end
  end
end

v = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
p v.each_index.map { |i| quickselect(v, i) }
```


```txt
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Scala


```scala
import scala.util.Random

object QuickSelect {
  def quickSelect[A <% Ordered[A]](seq: Seq[A], n: Int, rand: Random = new Random): A = {
    val pivot = rand.nextInt(seq.length);
    val (left, right) = seq.partition(_ < seq(pivot))
    if (left.length == n) {
      seq(pivot)
    } else if (left.length < n) {
      quickSelect(right, n - left.length, rand)
    } else {
      quickSelect(left, n, rand)
    }
  }

  def main(args: Array[String]): Unit = {
    val v = Array(9, 8, 7, 6, 5, 0, 1, 2, 3, 4)
    println((0 until v.length).map(quickSelect(v, _)).mkString(", "))
  }
}
```


```txt
0, 1, 2, 3, 4, 5, 6, 7, 8, 9
```



## Sidef


```ruby
func quickselect(a, k) {
    var pivot = a.pick;
    var left  = a.grep{|i| i < pivot};
    var right = a.grep{|i| i > pivot};

    given(var l = left.len) {
        when (k)     { pivot }
        case (k < l) { __FUNC__(left, k) }
        default      { __FUNC__(right, k - l - 1) }
    }
}

var v = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4];
say v.range.map{|i| quickselect(v, i)};
```

```txt
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Standard ML


```sml
fun quickselect (_, _, []) = raise Fail "empty"
  | quickselect (k, cmp, x :: xs) = let
        val (ys, zs) = List.partition (fn y => cmp (y, x) = LESS) xs
        val l = length ys
      in
        if k < l then
          quickselect (k, cmp, ys)
        else if k > l then
          quickselect (k-l-1, cmp, zs)
        else
          x
      end
```

Usage:

```txt

- val v = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4];
val v = [9,8,7,6,5,0,1,2,3,4] : int list
- List.tabulate (10, fn i => quickselect (i, Int.compare, v));
val it = [0,1,2,3,4,5,6,7,8,9] : int list

```



## Swift


```swift
func select<T where T : Comparable>
(var elements: [T], n: Int) -> T {
  var r = indices(elements)
  while true {
    let pivotIndex = partition(&elements, r)
    if n == pivotIndex {
      return elements[pivotIndex]
    } else if n < pivotIndex {
      r.endIndex = pivotIndex
    } else {
      r.startIndex = pivotIndex+1
    }
  }
}

for i in 0 ..< 10 {
  let a = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
  print(select(a, i))
  if i < 9 { print(", ") }
}
println()
```


```txt
0, 1, 2, 3, 4, 5, 6, 7, 8, 9
```



## Tcl

```tcl
# Swap the values at two indices of a list
proc swap {list i j} {
    upvar 1 $list l
    set tmp [lindex $l $i]
    lset l $i [lindex $l $j]
    lset l $j $tmp
}

proc quickselect {vector k {left 0} {right ""}} {
    set last [expr {[llength $vector] - 1}]
    if {$right eq ""} {
	set right $last
    }
    # Sanity assertions
    if {![llength $vector] || $k <= 0} {
	error "Either empty vector, or k <= 0"
    } elseif {![tcl::mathop::<= 0 $left $last]} {
	error "left is out of range"
    } elseif {![tcl::mathop::<= $left $right $last]} {
	error "right is out of range"
    }

    # the _select core, inlined
    while 1 {
	set pivotIndex [expr {int(rand()*($right-$left))+$left}]

	# the partition core, inlined
	set pivotValue [lindex $vector $pivotIndex]
	swap vector $pivotIndex $right
	set storeIndex $left
	for {set i $left} {$i <= $right} {incr i} {
	    if {[lindex $vector $i] < $pivotValue} {
		swap vector $storeIndex $i
		incr storeIndex
	    }
	}
	swap vector $right $storeIndex
	set pivotNewIndex $storeIndex

	set pivotDist [expr {$pivotNewIndex - $left + 1}]
	if {$pivotDist == $k} {
	    return [lindex $vector $pivotNewIndex]
	} elseif {$k < $pivotDist} {
	    set right [expr {$pivotNewIndex - 1}]
	} else {
	    set k [expr {$k - $pivotDist}]
	    set left [expr {$pivotNewIndex + 1}]
	}
    }
}
```

Demonstrating:

```tcl
set v {9 8 7 6 5 0 1 2 3 4}
foreach i {1 2 3 4 5 6 7 8 9 10} {
    puts "$i => [quickselect $v $i]"
}
```

```txt

1 => 0
2 => 1
3 => 2
4 => 3
5 => 4
6 => 5
7 => 6
8 => 7
9 => 8
10 => 9

```



## VBA

```vb
Dim s As Variant
Private Function quick_select(ByRef s As Variant, k As Integer) As Integer
    Dim left As Integer, right As Integer, pos As Integer
    Dim pivotValue As Integer, tmp As Integer
    left = 1: right = UBound(s)
    Do While left < right
        pivotValue = s(k)
        tmp = s(k)
        s(k) = s(right)
        s(right) = tmp
        pos = left
        For i = left To right
            If s(i) < pivotValue Then
                tmp = s(i)
                s(i) = s(pos)
                s(pos) = tmp
                pos = pos + 1
            End If
        Next i
        tmp = s(right)
        s(right) = s(pos)
        s(pos) = tmp
        If pos = k Then
            Exit Do
        End If
        If pos < k Then
            left = pos + 1
        Else
            right = pos - 1
        End If
    Loop
    quick_select = s(k)
End Function
Public Sub main()
    Dim r As Integer, i As Integer
    s = [{9, 8, 7, 6, 5, 0, 1, 2, 3, 4}]
    For i = 1 To 10
        r = quick_select(s, i) 's is ByRef parameter
        Debug.Print IIf(i < 10, r & ", ", "" & r);
    Next i
End Sub

```
```txt
0, 1, 2, 3, 4, 5, 6, 7, 8, 9
```


## zkl

This is the in place version rather than the much more concise copy-partition functional method. A copy of the input list is made to cover the case it is immutable (or the input shouldn't be changed)

```zkl
fcn qselect(list,nth){	// in place quick select
   fcn(list,left,right,nth){
      if (left==right) return(list[left]);
      pivotIndex:=(left+right)/2; // or median of first,middle,last

      	// partition
      pivot:=list[pivotIndex];
      list.swap(pivotIndex,right);	// move pivot to end
      pivotIndex := left;
      i:=left; do(right-left){	// foreach i in ([left..right-1])
	 if (list[i] < pivot){
	    list.swap(i,pivotIndex);
	    pivotIndex += 1;
	 }
	 i += 1;
      }
      list.swap(pivotIndex,right);	// move pivot to final place

      if (nth==pivotIndex) return(list[nth]);
      if (nth<pivotIndex)  return(self.fcn(list,left,pivotIndex-1,nth));
      return(self.fcn(list,pivotIndex+1,right,nth));
   }(list.copy(),0,list.len()-1,nth);
}
```


```zkl
list:=T(10, 9, 8, 7, 6, 1, 2, 3, 4, 5);
foreach nth in (list.len()){ println(nth,": ",qselect(list,nth)) }
```

```txt

0: 1
1: 2
2: 3
3: 4
4: 5
5: 6
6: 7
7: 8
8: 9
9: 10

```

