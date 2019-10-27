+++
title = "Sorting algorithms/Permutation sort"
description = ""
date = 2019-10-20T03:08:28Z
aliases = []
[extra]
id = 2853
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}
{{Sorting Algorithm}}
{{omit from|GUISS}}

;Task:
Implement a permutation sort, which proceeds by generating the possible permutations 
of the input array/list until discovering the sorted one.

Pseudocode:
 '''while not''' InOrder(list) '''do'''
     nextPermutation(list)
 '''done'''





## ActionScript


```ActionScript
//recursively builds the permutations of permutable, appended to front, and returns the first sorted permutation it encounters
function permutations(front:Array, permutable:Array):Array {
	//If permutable has length 1, there is only one possible permutation. Check whether it's sorted
	if (permutable.length==1)
		return isSorted(front.concat(permutable));
	else
		//There are multiple possible permutations. Generate them.
		var i:uint=0,tmp:Array=null;
		do
		{
			tmp=permutations(front.concat([permutable[i]]),remove(permutable,i));
			i++;
		}while (i< permutable.length && tmp == null);
		//If tmp != null, it contains the sorted permutation. If it does not contain the sorted permutation, return null. Either way, return tmp.
		return tmp;
}
//returns the array if it's sorted, or null otherwise
function isSorted(data:Array):Array {
	for (var i:uint = 1; i < data.length; i++) 
		if (data[i]<data[i-1]) 
			return null;
	return data;
}
//returns a copy of array with the i'th element removed
function remove(array:Array, i:uint):Array {
	return array.filter(function(item,index,array){return(index !=i)}) ;
}
//wrapper around the permutation function to provide a more logical interface
function permutationSort(array:Array):Array {
	return permutations([],array);
}
```



## AutoHotkey

ahk forum: [http://www.autohotkey.com/forum/post-276680.html#276680 discussion]

```AutoHotkey
MsgBox % PermSort("")
MsgBox % PermSort("xxx")
MsgBox % PermSort("3,2,1")
MsgBox % PermSort("dog,000000,xx,cat,pile,abcde,1,cat")

PermSort(var) {                          ; SORT COMMA SEPARATED LIST
   Local i, sorted
   StringSplit a, var, `,                ; make array, size = a0

   v0 := a0                              ; auxiliary array for permutations
   Loop %v0%
      v%A_Index% := A_Index

   While unSorted("a","v")               ; until sorted
      NextPerm("v")                      ; try new permutations

   Loop % a0                             ; construct string from sorted array
      i := v%A_Index%, sorted .= "," . a%i%
   Return SubStr(sorted,2)               ; drop leading comma
}

unSorted(a,v) {
   Loop % %a%0-1 {
      i := %v%%A_Index%, j := A_Index+1, j := %v%%j%
      If (%a%%i% > %a%%j%)
         Return 1
   }
}

NextPerm(v) { ; the lexicographically next LARGER permutation of v1..v%v0%
   Local i, i1, j, t
   i := %v%0, i1 := i-1
   While %v%%i1% >= %v%%i% {
      --i, --i1
      IfLess i1,1, Return 1 ; Signal the end
   }
   j := %v%0
   While %v%%j% <= %v%%i1%
      --j
   t := %v%%i1%, %v%%i1% := %v%%j%, %v%%j% := t,  j := %v%0
   While i < j
      t := %v%%i%, %v%%i% := %v%%j%, %v%%j% := t, ++i, --j
}
```
 


## BBC BASIC


```bbcbasic
      DIM test(9)
      test() = 4, 65, 2, 31, 0, 99, 2, 83, 782, 1
      
      perms% = 0
      WHILE NOT FNsorted(test())
        perms% += 1
        PROCnextperm(test())
      ENDWHILE
      PRINT ;perms% " permutations required to sort "; DIM(test(),1)+1 " items."
      END
      
      DEF PROCnextperm(a())
      LOCAL last%, maxindex%, p%
      maxindex% = DIM(a(),1)
      IF maxindex% < 1 THEN ENDPROC
      p% = maxindex%-1
      WHILE a(p%) >= a(p%+1)
        p% -= 1
        IF p% < 0 THEN
          PROCreverse(a(), 0, maxindex%)
          ENDPROC
        ENDIF
      ENDWHILE
      last% = maxindex%
      WHILE a(last%) <= a(p%)
        last% -= 1
      ENDWHILE
      SWAP a(p%), a(last%)
      PROCreverse(a(), p%+1, maxindex%)
      ENDPROC
      
      DEF PROCreverse(a(), first%, last%)
      WHILE first% < last%
        SWAP a(first%), a(last%)
        first% += 1
        last% -= 1
      ENDWHILE
      ENDPROC
      
      DEF FNsorted(d())
      LOCAL I%
      FOR I% = 1 TO DIM(d(),1)
        IF d(I%) < d(I%-1) THEN = FALSE
      NEXT
      = TRUE
```

{{out}}

```txt

980559 permutations required to sort 10 items.

```



## C

Just keep generating [[wp:Permutation#Systematic_generation_of_all_permutations|next lexicographic permutation]] until the last one; it's sorted by definition.

```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>

typedef int(*cmp_func)(const void*, const void*);

void perm_sort(void *a, int n, size_t msize, cmp_func _cmp)
{
	char *p, *q, *tmp = malloc(msize);
#	define A(i) ((char *)a + msize * (i))
#	define swap(a, b) {\
		memcpy(tmp, a, msize);\
		memcpy(a, b, msize);\
		memcpy(b, tmp, msize);	}
	while (1) {
		/* find largest k such that a[k - 1] < a[k] */
		for (p = A(n - 1); (void*)p > a; p = q)
			if (_cmp(q = p - msize, p) > 0)
				break;

		if ((void*)p <= a) break;

		/* find largest l such that a[l] > a[k - 1] */
		for (p = A(n - 1); p > q; p-= msize)
			if (_cmp(q, p) > 0) break;

		swap(p, q); /* swap a[k - 1], a[l] */
		/* flip a[k] through a[end] */
		for (q += msize, p = A(n - 1); q < p; q += msize, p -= msize)
			swap(p, q);
	}
	free(tmp);
}

int scmp(const void *a, const void *b) { return strcmp(*(const char *const *)a, *(const char *const *)b); }

int main()
{
	int i;
	const char *strs[] = { "spqr", "abc", "giant squid", "stuff", "def" };
	perm_sort(strs, 5, sizeof(*strs), scmp);

	for (i = 0; i < 5; i++)
		printf("%s\n", strs[i]);
	return 0;
}
```


=={{header|C sharp|C#}}==
<lang C sharp|C#>
public static class PermutationSorter
{
    public static void Sort<T>(List<T> list) where T : IComparable
    {
        PermutationSort(list, 0);
    }
    public static bool PermutationSort<T>(List<T> list, int i) where T : IComparable
    {
        int j;
        if (issorted(list, i))
        {
            return true;
        }
        for (j = i + 1; j < list.Count; j++)
        {
            T temp = list[i];
            list[i] = list[j];
            list[j] = temp;
            if (PermutationSort(list, i + 1))
            {
                return true;
            }
            temp = list[i];
            list[i] = list[j];
            list[j] = temp;
        }
        return false;
    }
    public static bool issorted<T>(List<T> list, int i) where T : IComparable
    {
	    for (int j = list.Count-1; j > 0; j--)
        {
	        if(list[j].CompareTo(list[j-1])<0)
            {
		        return false;
	        }
	    }
	    return true;
    }
}

```



## C++

Since <tt>next_permutation</tt> already returns whether the resulting sequence is sorted, the code is quite simple:


```cpp>#include <algorithm


template<typename ForwardIterator>
 void permutation_sort(ForwardIterator begin, ForwardIterator end)
{
  while (std::next_permutation(begin, end))
  {
    // -- this block intentionally left empty --
  }
}
```



## Clojure



```lisp

(use '[clojure.contrib.combinatorics :only (permutations)])

(defn permutation-sort [s]
  (first (filter (partial apply <=) (permutations s))))

(permutation-sort [2 3 5 3 5])

```



## CoffeeScript


```coffeescript
# This code takes a ridiculously inefficient algorithm and rather futilely
# optimizes one part of it.  Permutations are computed lazily.

sorted_copy = (a) ->
  # This returns a sorted copy of an array by lazily generating
  # permutations of indexes and stopping when the indexes yield
  # a sorted array.
  indexes = [0...a.length]
  ans = find_matching_permutation indexes, (permuted_indexes) ->
    new_array = (a[i] for i in permuted_indexes)
    console.log permuted_indexes, new_array
    in_order(new_array)
  (a[i] for i in ans)

in_order = (a) ->
  # return true iff array a is in increasing order.
  return true if a.length <= 1
  for i in [0...a.length-1]
    return false if a[i] > a[i+1]
  true

get_factorials = (n) ->
  # return an array of the first n+1 factorials, starting with 0!
  ans = [1]
  f = 1
  for i in [1..n]
    f *= i
    ans.push f
  ans

permutation = (a, i, factorials) ->
  # Return the i-th permutation of an array by
  # using remainders of factorials to determine
  # elements.
  while a.length > 0
    f = factorials[a.length-1]
    n = Math.floor(i / f)
    i = i % f
    elem = a[n]
    a = a[0...n].concat(a[n+1...])
    elem
  # The above loop gets treated like
  # an array expression, so it returns
  # all the elements.

find_matching_permutation = (a, f_match) ->
  factorials = get_factorials(a.length)
  for i in [0...factorials[a.length]]
    permuted_array = permutation(a, i, factorials)
    if f_match permuted_array
      return permuted_array
  null
  
  
do ->
  a = ['c', 'b', 'a', 'd']
  console.log 'input:', a
  ans = sorted_copy a
  console.log 'DONE!'
  console.log 'sorted copy:', ans

```

{{out}}
<lang>
> coffee permute_sort.coffee 
input: [ 'c', 'b', 'a', 'd' ]
[ 0, 1, 2, 3 ] [ 'c', 'b', 'a', 'd' ]
[ 0, 1, 3, 2 ] [ 'c', 'b', 'd', 'a' ]
[ 0, 2, 1, 3 ] [ 'c', 'a', 'b', 'd' ]
[ 0, 2, 3, 1 ] [ 'c', 'a', 'd', 'b' ]
[ 0, 3, 1, 2 ] [ 'c', 'd', 'b', 'a' ]
[ 0, 3, 2, 1 ] [ 'c', 'd', 'a', 'b' ]
[ 1, 0, 2, 3 ] [ 'b', 'c', 'a', 'd' ]
[ 1, 0, 3, 2 ] [ 'b', 'c', 'd', 'a' ]
[ 1, 2, 0, 3 ] [ 'b', 'a', 'c', 'd' ]
[ 1, 2, 3, 0 ] [ 'b', 'a', 'd', 'c' ]
[ 1, 3, 0, 2 ] [ 'b', 'd', 'c', 'a' ]
[ 1, 3, 2, 0 ] [ 'b', 'd', 'a', 'c' ]
[ 2, 0, 1, 3 ] [ 'a', 'c', 'b', 'd' ]
[ 2, 0, 3, 1 ] [ 'a', 'c', 'd', 'b' ]
[ 2, 1, 0, 3 ] [ 'a', 'b', 'c', 'd' ]
DONE!
sorted copy: [ 'a', 'b', 'c', 'd' ]

```



## Common Lisp


Too bad <code>sorted?</code> vector code has to be copypasta'd. Could use <tt>map nil</tt> but that would in turn make it into spaghetti code.

The <code>nth-permutation</code> function is some classic algorithm from Wikipedia.


```lisp
(defun factorial (n)
  (loop for result = 1 then (* i result)
        for i from 2 to n
        finally (return result)))

(defun nth-permutation (k sequence)
  (if (zerop (length sequence))
      (coerce () (type-of sequence))
      (let ((seq (etypecase sequence
                   (vector (copy-seq sequence))
                   (sequence (coerce sequence 'vector)))))
        (loop for j from 2 to (length seq)
              do (setq k (truncate (/ k (1- j))))
              do (rotatef (aref seq (mod k j))
                          (aref seq (1- j)))
              finally (return (coerce seq (type-of sequence)))))))

(defun sortedp (fn sequence)
  (etypecase sequence
    (list (loop for previous = #1='#:foo then i
                for i in sequence
                always (or (eq previous #1#)
                           (funcall fn i previous))))
    ;; copypasta
    (vector (loop for previous = #1# then i
                  for i across sequence
                  always (or (eq previous #1#)
                             (funcall fn i previous))))))

(defun permutation-sort (fn sequence)
  (loop for i below (factorial (length sequence))
        for permutation = (nth-permutation i sequence)
        when (sortedp fn permutation)
          do (return permutation)))
```



```lisp
CL-USER> (time (permutation-sort #'> '(8 3 10 6 1 9 7 2 5 4)))
Evaluation took:
  5.292 seconds of real time
  5.204325 seconds of total run time (5.176323 user, 0.028002 system)
  [ Run times consist of 0.160 seconds GC time, and 5.045 seconds non-GC time. ]
  98.34% CPU
  12,337,938,025 processor cycles
  611,094,240 bytes consed
  
(1 2 3 4 5 6 7 8 9 10)
```



## Crystal


```crystal
def sorted?(items : Array)
    prev = items[0]
    items.each do |item|
        if item < prev
            return false
        end
        prev = item
    end
    return true
end

def permutation_sort(items : Array)
    items.each_permutation do |permutation|
        if sorted?(permutation)
            return permutation
        end
    end
end
```



## D


### Basic Version

This uses the second (lazy) permutations from the Permutations Task.

```d
import std.stdio, std.algorithm, permutations2;

void permutationSort(T)(T[] items) pure nothrow @safe @nogc {
    foreach (const perm; items.permutations!false)
        if (perm.isSorted)
            break;
}

void main() {
    auto data = [2, 7, 4, 3, 5, 1, 0, 9, 8, 6, -1];
    data.permutationSort;
    data.writeln;
}
```

{{out}}

```txt
[-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

The run-time is about 0.52 seconds with ldc2.


### Alternative Version

{{trans|C++}}

```d
import std.stdio, std.algorithm;

void permutationSort(T)(T[] items) pure nothrow @safe @nogc {
    while (items.nextPermutation) {}
}

void main() {
    auto data = [2, 7, 4, 3, 5, 1, 0, 9, 8, 6, -1];
    data.permutationSort;
    data.writeln;
}
```

The output is the same. 
Run-time about 1.04 seconds with ldc2 (the C++ entry with G++ takes about 0.4 seconds).


## E

{{trans|C++}}


```e
def swap(container, ixA, ixB) {
    def temp := container[ixA]
    container[ixA] := container[ixB]
    container[ixB] := temp
}

/** Reverse order of elements of 'sequence' whose indexes are in the interval [ixLow, ixHigh] */
def reverseRange(sequence, var ixLow, var ixHigh) {
    while (ixLow < ixHigh) {
        swap(sequence, ixLow, ixHigh)
        ixLow += 1
        ixHigh -= 1
    }
}

/** Algorithm from <http://marknelson.us/2002/03/01/next-permutation>, allegedly from a version of the C++ STL */
def nextPermutation(sequence) {
    def last := sequence.size() - 1
    var i := last
    while (true) {
        var ii := i
        i -= 1
        if (sequence[i] < sequence[ii]) {
            var j := last + 1
            while (!(sequence[i] < sequence[j -= 1])) {} # buried side effect
            swap(sequence, i, j)
            reverseRange(sequence, ii, last)
            return true
        }
        if (i == 0) {
            reverseRange(sequence, 0, last)
            return false
        }
    }
}

/** Note: Worst case on sorted list */
def permutationSort(flexList) {
    while (nextPermutation(flexList)) {}
}
```



## EchoLisp


```scheme

;; This efficient sort method uses the list library for permutations

(lib 'list)
(define (in-order L)
(cond
    ((empty? L) #t)
    ((empty? (rest L)) #t)
    (else (and ( < (first L) (second  L)) (in-order (rest L))))))

(define L (shuffle (iota 6)))
    → (1 5 4 2 0 3)

(for ((p (in-permutations (length L )))) 
    #:when (in-order (list-permute L p)) 
       (writeln (list-permute L p)) #:break #t)

    → (0 1 2 3 4 5)  

```
 


## Elixir


```elixir
defmodule Sort do
  def permutation_sort([]), do: []
  def permutation_sort(list) do
    Enum.find(permutation(list), fn [h|t] -> in_order?(t, h) end)
  end
  
  defp permutation([]), do: [[]]
  defp permutation(list) do
    for x <- list, y <- permutation(list -- [x]), do: [x|y]
  end
  
  defp in_order?([], _), do: true
  defp in_order?([h|_], pre) when h<pre, do: false
  defp in_order?([h|t], _), do: in_order?(t, h)
end

IO.inspect list = for _ <- 1..9, do: :rand.uniform(20)
IO.inspect Sort.permutation_sort(list)
```


{{out}}

```txt

[18, 2, 19, 10, 17, 10, 14, 8, 3]
[2, 3, 8, 10, 10, 14, 17, 18, 19]

```



## Factor


```factor
USING: grouping io math.combinatorics math.order prettyprint ;
IN: rosetta-code.permutation-sort

: permutation-sort ( seq -- seq' )
    [ [ before=? ] monotonic? ] find-permutation ;
    
{ 10 2 6 8 1 4 3 } permutation-sort .
"apple" permutation-sort print
```

{{out}}

```txt

{ 1 2 3 4 6 8 10 }
aelpp

```



## FreeBASIC


```freebasic
' version 07-04-2017
' compile with: fbc -s console

' Heap's algorithm non-recursive
Function permutation_sort(a() As ULong) As ULong

    Dim As ULong i, j, count
    Dim As ULong lb = LBound(a), ub = UBound(a)
    Dim As ULong n = ub - lb +1
    Dim As ULong c(lb To ub)

    While i < n
        If c(i) < i Then
            If (i And 1) = 0 Then
                Swap a(0), a(i)
            Else
                Swap a(c(i)), a(i)
            End If
            count += 1
            For j = lb To ub -1
                If a(j) > a(j +1) Then j = 99
            Next
            If j < 99 Then Return count
            c(i) += 1
            i = 0
        Else
            c(i) = 0
            i += 1
        End If
    Wend

End Function

' ------=< MAIN >=------

Dim As ULong k, p, arr(0 To 9)
Randomize Timer

Print "unsorted array"
For k = LBound(arr) To UBound(arr)
    arr(k) = Rnd * 1000
    Print arr(k) & IIf(k = UBound(arr), "", ", ");
Next
Print : Print

p = permutation_sort(arr())

Print "sorted array"
For k = LBound(arr) To UBound(arr)
    Print arr(k) & IIf(k = UBound(arr), "", ", ");
Next
Print : Print
Print "sorted array in "; p; " permutations"

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
unsorted array
81, 476, 915, 357, 934, 683, 413, 450, 2, 407

sorted array
2, 81, 357, 407, 413, 450, 476, 683, 915, 934

sorted array in 1939104 permutations
```



## Go

Not following the pseudocode, it seemed simpler to just test sorted at the bottom of a recursive permutation generator.

```go
package main

import "fmt"

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}

// in place permutation sort of slice a
func main() {
    fmt.Println("before:", a)
    if len(a) > 1 && !recurse(len(a) - 1) {
        // recurse should never return false from the top level.
        // if it does, it means some code somewhere is busted,
        // either the the permutation generation code or the
        // sortedness testing code.
        panic("sorted permutation not found!")
    }
    fmt.Println("after: ", a)
}

// recursive permutation generator
func recurse(last int) bool {
    if last <= 0 {
        // bottom of recursion.  test if sorted.
        for i := len(a) - 1; a[i] >= a[i-1]; i-- {
            if i == 1 {
                return true
            }
        }
        return false
    }
    for i := 0; i <= last; i++ {
        a[i], a[last] = a[last], a[i]
        if recurse(last - 1) {
            return true
        }
        a[i], a[last] = a[last], a[i]
    }
    return false
}
```



## Groovy

Permutation sort is an astonishingly inefficient sort algorithm. To even begin to make it tractable, we need to be able to create enumerated permutations on the fly, rather than relying on [[Groovy]]'s ''List.permutations()'' method. For a list of length ''N'' there are ''N!'' permutations. In this solution, ''makePermutation'' creates the ''I<sup>th</sup>'' permutation to order based on a recursive construction of a unique indexed permutation. The sort method then checks to see if that permutation is sorted, and stops when it is.

I believe that this method of constructing permutations results in a stable sort, but I have not actually proven that assertion.

```groovy
def factorial = { (it > 1) ? (2..it).inject(1) { i, j -> i*j } : 1 }

def makePermutation;
makePermutation = { list, i ->
    def n = list.size()
    if (n < 2) return list
    def fact = factorial(n-1)
    assert i < fact*n
    
    def index = i.intdiv(fact)
    [list[index]] + makePermutation(list[0..<index] + list[(index+1)..<n], i % fact)
}

def sorted = { a -> (1..<(a.size())).every { a[it-1] <= a[it] } }

def permutationSort = { a ->
    def n = a.size()
    def fact = factorial(n)
    def permuteA = makePermutation.curry(a)
    def pIndex = (0..<fact).find { print "."; sorted(permuteA(it)) }
    permuteA(pIndex)
}
```


Test:

```groovy
println permutationSort([7,0,12,-45,-1])
println ()
println permutationSort([10, 10.0, 10.00, 1])
println permutationSort([10, 10.00, 10.0, 1])
println permutationSort([10.0, 10, 10.00, 1])
println permutationSort([10.0, 10.00, 10, 1])
println permutationSort([10.00, 10, 10.0, 1])
println permutationSort([10.00, 10.0, 10, 1])
```

The examples with distinct integer and decimal values that compare as equal are there to demonstrate, but not to prove, that the sort is stable.

{{out}}

```txt
.............................................................................................[-45, -1, 0, 7, 12]

...................[1, 10, 10.0, 10.00]
...................[1, 10, 10.00, 10.0]
...................[1, 10.0, 10, 10.00]
...................[1, 10.0, 10.00, 10]
...................[1, 10.00, 10, 10.0]
...................[1, 10.00, 10.0, 10]
```



## Haskell


```Haskell
import Control.Monad

permutationSort l = head [p | p <- permute l, sorted p]

sorted (e1 : e2 : r) = e1 <= e2 && sorted (e2 : r)
sorted _             = True

permute              = foldM (flip insert) []

insert e []          = return [e]
insert e l@(h : t)   = return (e : l) `mplus`
                       do { t' <- insert e t ; return (h : t') }
```

{{works with|GHC|6.10}}

```haskell
import Data.List (permutations)

permutationSort l = head [p | p <- permutations l, sorted p]

sorted (e1 : e2 : r) = e1 <= e2 && sorted (e2 : r)
sorted _             = True
```


=={{header|Icon}} and {{header|Unicon}}==
Partly from [http://infohost.nmt.edu/tcc/help/lang/icon/backtrack.html here]

```icon
procedure do_permute(l, i, n)
    if i >= n then
        return l
    else
        suspend l[i to n] <-> l[i] & do_permute(l, i+1, n)
 end
 
 procedure permute(l)
    suspend do_permute(l, 1, *l)
 end
 
 procedure sorted(l)
    local i
    if (i := 2 to *l & l[i] >= l[i-1]) then return &fail else return 1
 end
 
 procedure main()
    local l
    l := [6,3,4,5,1]
    |( l := permute(l) & sorted(l)) \1 & every writes(" ",!l)
 end
```



## J

{{eff note|J|/:~}}
A function to locate the permuation index, in the naive manner prescribed by the task:

```j
ps =:(1+])^:((-.@-:/:~)@A.~)^:_ 0:
```

Of course, this can be calculated much more directly (and efficiently):

```j>ps =: A.@:/:</lang

Either way:

```j
   list =: 2 7 4 3 5 1 0 9 8 6
   
   ps list 
2380483    
   
   2380483 A. list
0 1 2 3 4 5 6 7 8 9
   
   (A.~ps) list
0 1 2 3 4 5 6 7 8 9
```



## Java


```java5
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

public class PermutationSort 
{
	public static void main(String[] args)
	{
		int[] a={3,2,1,8,9,4,6};
		System.out.println("Unsorted: " + Arrays.toString(a));
		a=pSort(a);
		System.out.println("Sorted: " + Arrays.toString(a));
	}
	public static int[] pSort(int[] a)
	{
		List<int[]> list=new ArrayList<int[]>();
		permute(a,a.length,list);
		for(int[] x : list)
			if(isSorted(x))
				return x;
		return a;
	}
	private static void permute(int[] a, int n, List<int[]> list) 
	{
		if (n == 1) 
		{
			int[] b=new int[a.length];
			System.arraycopy(a, 0, b, 0, a.length);
			list.add(b);
		    return;
		}
		for (int i = 0; i < n; i++) 
		{
		        swap(a, i, n-1);
		        permute(a, n-1, list);
		        swap(a, i, n-1);
		 }
	}
	private static boolean isSorted(int[] a)
	{
		for(int i=1;i<a.length;i++)
			if(a[i-1]>a[i])
				return false;
		return true;
	}
	private static void swap(int[] arr,int i, int j)
	{
		int temp=arr[i];
		arr[i]=arr[j];
		arr[j]=temp;
	}
}
```


{{out}}

```txt

Unsorted: [3, 2, 1, 8, 9, 4, 6]
Sorted: [1, 2, 3, 4, 6, 8, 9]

```



## jq

'''Infrastructure''':
The following function generates a stream of permutations of an arbitrary JSON array:

```jq
def permutations:
  if length == 0 then []
  else
    . as $in 
    | range(0;length) as $i
    | ($in|del(.[$i])|permutations) 
    | [$in[$i]] + .
  end ;
```


Next is a generic function for checking whether the input array is non-decreasing.
If your jq has until/2 then its definition here can be removed.

```jq
def sorted:
  def until(cond; next):
     def _until: if cond then . else (next|_until) end;
     _until;

  length as $length
  | if $length <= 1 then true
    else . as $in
    | 1 | until( . == $length or $in[.-1] > $in[.] ; .+1) == $length
  end;
```


'''Permutation-sort''':

The first permutation-sort solution presented here works with jq 1.4 but is slower than the subsequent solution,
which uses the "foreach" construct introduced after the release of jq 1.4.
"foreach" allows a stream generator to be interrupted.

{{works with|jq|1.4}}

```jq
def permutation_sort_slow:
  reduce permutations as $p (null; if . then . elif ($p | sorted) then $p else . end);
```


{{works with|jq|with foreach}}

```jq
def permutation_sort:
  # emit the first item in stream that satisfies the condition
  def first(stream; cond):
     label $out
     | foreach stream as $item
         ( [false, null];
           if .[0] then break $out else [($item | cond), $item] end;
           if .[0] then .[1] else empty end );
  first(permutations; sorted);
```


'''Example''':

```jq
["too", true, 1, 0, {"a":1},  {"a":0} ] | permutation_sort
```

{{out}}

```sh
$ jq -c -n -f Permutation_sort.jq
[true,0,1,"too",{"a":0},{"a":1}]
```



## Julia


```julia
# v0.6

using Combinatorics

function permsort(x::Array)
    for perm in permutations(x)
        if issorted(perm)
            return perm
        end
    end
end

x = randn(10)
@show x permsort(x)
```


{{out}}

```txt
x = [-0.799206, -2.52542, 0.677947, -1.85139, 0.744764, 1.5327, 0.808935, -0.876105, -0.234308, 0.874579]
permsort(x) = [-2.52542, -1.85139, -0.876105, -0.799206, -0.234308, 0.677947, 0.744764, 0.808935, 0.874579, 1.5327]
```



## Kotlin


```scala
// version 1.1.2

fun <T : Comparable<T>> isSorted(list: List<T>): Boolean {
    val size = list.size
    if (size < 2) return true
    for (i in 1 until size) {
        if (list[i] < list[i - 1]) return false
    }
    return true
}

fun <T : Comparable<T>> permute(input: List<T>): List<List<T>> {
    if (input.size == 1) return listOf(input)
    val perms = mutableListOf<List<T>>()
    val toInsert = input[0]
    for (perm in permute(input.drop(1))) {
        for (i in 0..perm.size) {
            val newPerm = perm.toMutableList()
            newPerm.add(i, toInsert)
            perms.add(newPerm)
        }
    }
    return perms
}

fun <T : Comparable<T>> permutationSort(input: List<T>): List<T> {
    if (input.size == 1) return input
    val toInsert = input[0]
    for (perm in permute(input.drop(1))) {
        for (i in 0..perm.size) {
            val newPerm = perm.toMutableList()
            newPerm.add(i, toInsert)
            if (isSorted(newPerm)) return newPerm
        }
    }
    return input
}

fun main(args: Array<String>) {
    val input = listOf('d', 'b', 'e', 'a', 'f', 'c')
    println("Before sorting : $input")
    val output = permutationSort(input)
    println("After sorting  : $output")
    println()
    val input2 = listOf("first", "second", "third", "fourth", "fifth", "sixth")
    println("Before sorting : $input2")
    val output2 = permutationSort(input2)
    println("After sorting  : $output2")
}
```


{{out}}

```txt

Before sorting : [d, b, e, a, f, c]
After sorting  : [a, b, c, d, e, f]

Before sorting : [first, second, third, fourth, fifth, sixth]
After sorting  : [fifth, first, fourth, second, sixth, third]

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

-- Return true if table t is in ascending order or false if not
function inOrder (t)
  for pos = 2, #t do
    if t[pos] < t[pos - 1] then
      return false
    end
  end
  return true
end

-- Main procedure
local list = {2,3,1}                 --\   Written to match task pseudocode,
local nextPermutation = permute(list) --\  more idiomatic would be:
while not inOrder(list) do             --\ 
  list = nextPermutation()             --/   for p in permute(list) do
end                                   --/       stuffWith(p)
print(unpack(list))                  --/     end
```

{{out}}

```txt
1       2       3
```



## Maple


```Maple
arr := Array([17,0,-1,72,0]):
len := numelems(arr):
P := Iterator:-Permute(len):
for p in P do
	lst:= convert(arr[sort(convert(p,list),output=permutation)],list):
	if (ListTools:-Sorted(lst)) then
		print(lst):
		break:
	end if:
end do:
```

{{Out|Output}}

```txt
[-1,0,0,17,72]
```



## Mathematica

Here is  a one-line solution. 
A custom order relation can be defined for the OrderedQ[] function.


```Mathematica
PermutationSort[x_List] := NestWhile[RandomSample, x, Not[OrderedQ[#]] &]
```


=={{header|MATLAB}} / {{header|Octave}}==


```MATLAB
function list = permutationSort(list)

    permutations = perms(1:numel(list)); %Generate all permutations of the item indicies 
    
    %Test every permutation of the indicies of the original list
    for i = (1:size(permutations,1))
        if issorted( list(permutations(i,:)) )
            list = list(permutations(i,:));
            return %Once the correct permutation of the original list is found break out of the program
        end
    end

end
```


Sample Usage:

```MATLAB>>
 permutationSort([4 3 1 5 6 2])

ans =

     1     2     3     4     5     6
```



## MAXScript


```MAXScript
fn inOrder arr =
(
	if arr.count < 2 then return true
	else
	(
		local i = 1 
		while i < arr.count do
		(
			if arr[i+1] < arr[i] do return false
			i += 1
		)
		return true
	)
)

fn permutations arr =
(
	if arr.count <= 1 then return arr
	else
	(
		for i = 1 to arr.count do
			(
				local rest = for r in 1 to arr.count where r != i collect arr[r]
				local permRest = permutations rest
				local new = join #(arr[i]) permRest
				if inOrder new do return new
			)
		)
)
```

Output:

```MAXScript

a = for i in 1 to 9 collect random 1 20
#(10, 20, 17, 15, 17, 15, 3, 11, 15)
permutations a
#(3, 10, 11, 15, 15, 15, 17, 17, 20)

```

Warning: This algorithm is very inefficient and Max will crash very quickly with bigger arrays.


## NetRexx

Uses the permutation iterator '''<tt>RPermutationIterator</tt>''' at [[Permutations#NetRexx|Permutations]] to generate the permutations.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.util.List
import java.util.ArrayList

numeric digits 20

class RSortingPermutationsort public

  properties private static
    iterations
    maxIterations

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method permutationSort(vlist = List) public static returns List
    perm = RPermutationIterator(vlist)
    iterations = 0
    maxIterations = RPermutationIterator.factorial(vlist.size())
    loop while perm.hasNext()
      iterations = iterations + 1
      pl = List perm.next()
      if isSorted(pl) then leave
      else pl = null
      end
    return pl

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method isSorted(ss = List) private static returns boolean
    status = isTrue
    loop ix = 1 while ix < ss.size()
      vleft  = Rexx ss.get(ix - 1)
      vright = Rexx ss.get(ix)      
      if vleft.datatype('N') & vright.datatype('N')
      then vtest = vleft > vright  -- For numeric types we must use regular comparison.
      else vtest = vleft >> vright -- For non-numeric/mixed types we must do strict comparison.
      if vtest then do
        status = isFalse
        leave ix
        end
      end ix
    return status

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method runSample(arg) private static
    placesList = -
        "UK  London,     US  New York,   US  Boston,     US  Washington" -
        "UK  Washington, US  Birmingham, UK  Birmingham, UK  Boston"
    anotherList = 'Alpha, Beta, Gamma, Beta'
    reversed = '7, 6, 5, 4, 3, 2, 1'
    unsorted = '734, 3, 1, 24, 324, -1024, -666, -1, 0, 324, 99999999'
    lists = [makeList(placesList), makeList(anotherList), makeList(reversed), makeList(unsorted)]
    loop il = 0 while il < lists.length
      vlist = lists[il]
      say vlist
      runtime = System.nanoTime()
      rlist = permutationSort(vlist)
      runtime = System.nanoTime() - runtime
      if rlist \= null then say rlist
      else say 'sort failed'
      say 'This permutation sort of' vlist.size() 'elements took' iterations 'passes (of' maxIterations') to complete. \-'
      say 'Elapsed time:' (runtime / 10 ** 9)'s.'
      say
      end il
    return

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method makeList(in) public static returns List
    lst = ArrayList()
    loop while in > ''
      parse in val ',' in
      lst.add(val.strip())
      end
    return lst
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method main(args = String[]) public static
    runSample(Rexx(args))
    return
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method isTrue() public static returns boolean
    return (1 == 1)
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method isFalse() public static returns boolean
    return (1 == 0)

```

{{out}}

```txt

[UK  London, US  New York, US  Boston, US  Washington UK  Washington, US  Birmingham, UK  Birmingham, UK  Boston]
[UK  Birmingham, UK  Boston, UK  London, US  Birmingham, US  Boston, US  New York, US  Washington UK  Washington]
This permutation sort of 7 elements took 4221 passes (of 5040) to complete. Elapsed time: 0.361959s.

[Alpha, Beta, Gamma, Beta]
[Alpha, Beta, Beta, Gamma]
This permutation sort of 4 elements took 2 passes (of 24) to complete. Elapsed time: 0.000113s.

[7, 6, 5, 4, 3, 2, 1]
[1, 2, 3, 4, 5, 6, 7]
This permutation sort of 7 elements took 5040 passes (of 5040) to complete. Elapsed time: 0.267956s.

[734, 3, 1, 24, 324, -1024, -666, -1, 0, 324, 99999999]
[-1024, -666, -1, 0, 1, 3, 24, 324, 324, 734, 99999999]
This permutation sort of 11 elements took 20186793 passes (of 39916800) to complete. Elapsed time: 141.461863s.

```



## Nim


```nim
iterator permutations[T](ys: openarray[T]): seq[T] =
  var
    d = 1
    c = newSeq[int](ys.len)
    xs = newSeq[T](ys.len)

  for i, y in ys: xs[i] = y
  yield xs

  block outter:
    while true:
      while d > 1:
        dec d
        c[d] = 0
      while c[d] >= d:
        inc d
        if d >= ys.len: break outter

      let i = if (d and 1) == 1: c[d] else: 0
      swap xs[i], xs[d]
      yield xs
      inc c[d]

proc isSorted[T](s: openarray[T]): bool =
  var last = low(T)
  for c in s:
    if c < last:
      return false
    last = c
  return true

proc permSort[T](a: openarray[T]): seq[T] =
  for p in a.permutations:
    if p.isSorted:
      return p

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
echo a.permSort
```

{{out}}

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## OCaml

Like the Haskell version, except not evaluated lazily. So it always computes all the permutations, before searching through them for a sorted one; which is more expensive than necessary; unlike the Haskell version, which stops generating at the first sorted permutation.

```ocaml
let rec sorted = function
 | e1 :: e2 :: r -> e1 <= e2 && sorted (e2 :: r)
 | _             -> true

let rec insert e = function
 | []          -> [[e]]
 | h :: t as l -> (e :: l) :: List.map (fun t' -> h :: t') (insert e t)

let permute xs = List.fold_right (fun h z -> List.concat (List.map (insert h) z))
                                 xs [[]]

let permutation_sort l = List.find sorted (permute l)
```



## PARI/GP


```parigp
permutationSort(v)={
  my(u);
  for(k=1,(#v)!,
    u=vecextract(v, numtoperm(#v,k));
    for(i=2,#u,
      if(u[i]<u[i-1], next(2))
    );
    return(u)
  )
};
```



## Perl

Pass a list in by reference, and sort in situ.

```perl
sub psort {
        my ($x, $d) = @_;

        unless ($d //= $#$x) {
                $x->[$_] < $x->[$_ - 1] and return for 1 .. $#$x;
                return 1
        }
        
        for (0 .. $d) {
                unshift @$x, splice @$x, $d, 1;
                next if $x->[$d] < $x->[$d - 1];
                return 1 if psort($x, $d - 1);
        }
}

my @a = map+(int rand 100), 0 .. 10;
print "Before:\t@a\n";
psort(\@a);
print "After:\t@a\n"
```


{{out}}

```txt
Before: 94 15 42 35 55 24 96 14 61 94 43
After:  14 15 24 35 42 43 55 61 94 94 96
```



## Perl 6


```perl6
# Lexicographic permuter from "Permutations" task.
sub next_perm ( @a ) {
    my $j = @a.end - 1;
    $j-- while $j >= 1 and [>] @a[ $j, $j+1 ];

    my $aj = @a[$j];
    my $k  = @a.end;
    $k-- while [>] $aj, @a[$k];

    @a[ $j, $k ] .= reverse;

    my Int $r = @a.end;
    my Int $s = $j + 1;
    while $r > $s {
        @a[ $r, $s ] .= reverse;
        $r--;
        $s++;
    }
}

sub permutation_sort ( @a ) {
    my @n = @a.keys;
    my $perm_count = [*] 1 .. +@n; # Factorial
    for ^$perm_count {
        my @permuted_a = @a[ @n ];
        return @permuted_a if [le] @permuted_a;
        next_perm(@n);
    }
}

my @data  = < c b e d a >; # Halfway between abcde and edcba
say 'Input  = ' ~ @data;
say 'Output = ' ~ @data.&permutation_sort;

```


{{out}}

```txt
Input  = c b e d a
Output = a b c d e
```



## Phix


```Phix
function inOrder(sequence s)
    for i=2 to length(s) do
        if s[i]<s[i-1] then return 0 end if
    end for
    return 1
end function

function permutationSort(sequence s)
    for n=1 to factorial(length(s)) do
        sequence perm = permute(n,s)
        if inOrder(perm) then return perm end if
    end for
    ?9/0 -- should never happen
end function

?permutationSort({"dog",0,15.545,{"cat","pile","abcde",1},"cat"})
```

{{out}}

```txt

{0,15.545,"cat","dog",{"cat","pile","abcde",1}}

```



## PHP


```php
function inOrder($arr){
	for($i=0;$i<count($arr);$i++){
		if(isset($arr[$i+1])){
			if($arr[$i] > $arr[$i+1]){
				return false;
			}
		}
	}
	return true;
}

function permute($items, $perms = array( )) {
    if (empty($items)) {
		if(inOrder($perms)){
			return $perms;
		}
    }  else {
        for ($i = count($items) - 1; $i >= 0; --$i) {
             $newitems = $items;
             $newperms = $perms;
             list($foo) = array_splice($newitems, $i, 1);
             array_unshift($newperms, $foo);
             $res = permute($newitems, $newperms);
			 if($res){
				return $res;
			 }		 		 
         }
    }
}

$arr = array( 8, 3, 10, 6, 1, 9, 7, 2, 5, 4);
$arr = permute($arr);
echo implode(',',$arr);
```


```txt
1,2,3,4,5,6,7,8,9,10
```



## PicoLisp


```PicoLisp
(de permutationSort (Lst)
   (let L Lst
      (recur (L)  # Permute
         (if (cdr L)
            (do (length L)
               (T (recurse (cdr L)) Lst)
               (rot L)
               NIL )
            (apply <= Lst) ) ) ) )
```

{{out}}

```txt
: (permutationSort (make (do 9 (link (rand 1 999)))))
-> (82 120 160 168 205 226 408 708 719)

: (permutationSort (make (do 9 (link (rand 1 999)))))
-> (108 212 330 471 667 716 739 769 938)

: (permutationSort (make (do 9 (link (rand 1 999)))))
-> (118 253 355 395 429 548 890 900 983)
```



## PowerShell


```PowerShell
Function PermutationSort( [Object[]] $indata, $index = 0, $k = 0 )
{
	$data = $indata.Clone()
	$datal = $data.length - 1
	if( $datal -gt 0 ) {
		for( $j = $index; $j -lt $datal; $j++ )
		{
			$sorted = ( PermutationSort $data ( $index + 1 ) $j )[0]
			if( -not $sorted )
			{
				$temp = $data[ $index ]
				$data[ $index ] = $data[ $j + 1 ]
				$data[ $j + 1 ] = $temp
			}
		}
		if( $index -lt ( $datal - 1 ) )
		{
			PermutationSort $data ( $index + 1 ) $j
		} else {
			$sorted = $true
			for( $i = 0; ( $i -lt $datal ) -and $sorted; $i++ )
			{
				$sorted = ( $data[ $i ] -le $data[ $i + 1 ] )
			}
			$sorted
			$data
		}
	}
}

0..4 | ForEach-Object { $a = $_; 0..4 | Where-Object { -not ( $_ -match "$a" ) } |
	ForEach-Object { $b = $_; 0..4 | Where-Object { -not ( $_ -match "$a|$b" ) } |
		ForEach-Object { $c = $_; 0..4 | Where-Object { -not ( $_ -match "$a|$b|$c" ) } |
			ForEach-Object { $d = $_; 0..4 | Where-Object { -not ( $_ -match "$a|$b|$c|$d" ) } |
				ForEach-Object { $e=$_; "$( PermutationSort ( $a, $b, $c, $d, $e ) )" } 
			} 
		} 
	} 
}
$l = 8; PermutationSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } )
```



## Prolog


```prolog
permutation_sort(L,S) :- permutation(L,S), sorted(S).

sorted([]).
sorted([_]).
sorted([X,Y|ZS]) :- X =< Y, sorted([Y|ZS]).

permutation([],[]).
permutation([X|XS],YS) :- permutation(XS,ZS), select(X,YS,ZS).
```



## PureBasic


```PureBasic
Macro reverse(firstIndex, lastIndex)
  first = firstIndex
  last = lastIndex
  While first < last
    Swap cur(first), cur(last)
    first + 1
    last - 1
  Wend 
EndMacro

Procedure nextPermutation(Array cur(1))
  Protected first, last, elementCount = ArraySize(cur())
  If elementCount < 2
    ProcedureReturn #False ;nothing to permute
  EndIf 
  
  ;Find the lowest position pos such that [pos] < [pos+1]
  Protected pos = elementCount - 1
  While cur(pos) >= cur(pos + 1)
    pos - 1
    If pos < 0
      reverse(0, elementCount)
      ProcedureReturn #False ;no higher lexicographic permutations left, return lowest one instead
    EndIf 
  Wend

  ;Swap [pos] with the highest positional value that is larger than [pos]
  last = elementCount
  While cur(last) <= cur(pos)
    last - 1
  Wend
  Swap cur(pos), cur(last)

  ;Reverse the order of the elements in the higher positions
  reverse(pos + 1, elementCount)
  ProcedureReturn #True ;next lexicographic permutation found
EndProcedure

Procedure display(Array a(1))
  Protected i, fin = ArraySize(a())
  For i = 0 To fin
    Print(Str(a(i)))
    If i = fin: Continue: EndIf
    Print(", ")
  Next
  PrintN("")
EndProcedure

If OpenConsole()
  Dim a(9)
  a(0) = 8: a(1) = 3: a(2) =  10: a(3) =  6: a(4) =  1: a(5) =  9: a(6) =  7: a(7) =  -4: a(8) =  5: a(9) =  3
  display(a())
  While nextPermutation(a()): Wend
  display(a())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
8, 3, 10, 6, 1, 9, 7, -4, 5, 3
-4, 1, 3, 3, 5, 6, 7, 8, 9, 10
```



## Python

{{works with|Python|2.6}}

```python
from itertools import permutations

in_order = lambda s: all(x <= s[i+1] for i,x in enumerate(s[:-1]))
perm_sort = lambda s: (p for p in permutations(s) if in_order(p)).next()
```



## R
           
{{libheader|e1071}}
Warning: This function keeps all the possible permutations in memory at once, which becomes silly when x has 10 or more elements.

```r
permutationsort <- function(x)
{
   if(!require(e1071) stop("the package e1071 is required")
   is.sorted <- function(x) all(diff(x) >= 0)

   perms <- permutations(length(x))
   i <- 1
   while(!is.sorted(x)) 
   {
      x <- x[perms[i,]]
      i <- i + 1
   }
   x
}
permutationsort(c(1, 10, 9, 7, 3, 0))
```



## Racket



```Racket

#lang racket
(define (sort l)
  (for/first ([p (in-permutations l)] #:when (apply <= p)) p))
(sort '(6 1 5 2 4 3)) ; => '(1 2 3 4 5 6)

```



## REXX


```rexx
/*REXX program  sorts and displays  an array  using the  permutation-sort  method.      */
call gen                                         /*generate the array elements.         */
call show     'before sort'                      /*show the  before  array elements.    */
say  copies('░', 75)                             /*show separator line between displays.*/
call pSort    L                                  /*invoke the permutation sort.         */
call show     ' after sort'                      /*show the   after  array elements.    */
say; say 'Permutation sort took '      ?      " permutations to find the sorted list."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
.pAdd:  #=#+1; do j=1 for N;  #.#=#.#  !.j;  end;   return          /*add a permutation.*/
show:          do j=1 for L; say @e right(j,wL) arg(1)":" translate(@.j,,'_'); end; return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen:    @.=;                            @.1 = '---Four_horsemen_of_the_Apocalypse---'
                                        @.2 = '
### ===============================
'
                                        @.3 = 'Famine───black_horse'
                                        @.4 = 'Death───pale_horse'
                                        @.5 = 'Pestilence_[Slaughter]───red_horse'
                                        @.6 = 'Conquest_[War]───white_horse'
        @e=right('element', 15)                          /*literal used for the display.*/
          do L=1  while @.L\=='';  @@.L=@.L;   end;    L=L-1;      wL=length(L);    return
/*──────────────────────────────────────────────────────────────────────────────────────*/
isOrd:  parse arg q                                      /*see if  Q  list is in order. */
        _=word(q, 1);  do j=2  to words(q);  x=word(q, j);  if x<_  then return 0;     _=x
                       end   /*j*/                       /* [↑]  Out of order?   ¬sorted*/
          do k=1  for #;  _=word(#.?, k);  @.k=@@._;  end  /*k*/;  return 1  /*in order.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
.pNext: procedure expose !.;    parse arg n,i;   nm=n-1
                             do k=nm  by -1  for nm;   kp=k+1
                             if !.k<!.kp   then  do;   i=k;   leave;   end
                             end   /*k*/                 /* [↓]  swap two array elements*/
           do j=i+1  while j<n;  parse value  !.j !.n  with  !.n !.j;   n=n-1;  end  /*j*/
        if i==0  then return 0                           /*0:  indicates no more perms. */
           do j=i+1  while !.j<!.i;   end  /*j*/         /*search perm for a lower value*/
        parse  value    !.j  !.i    with    !.i  !.j     /*swap two values in !.  array.*/
        return 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
pSort:  parse arg n,#.;  #=0                     /*generate  L  items (!)  permutations.*/
                     do f=1  for n;               !.f=f;        end  /*f*/
        call .pAdd;  do while .pNext(n, 0);       call .pAdd;   end  /*while*/
                     do ?=1  until isOrd($);      $=                        /*find perm.*/
                       do m=1  for #; _=word(#.?, m); $=$ @._;  end  /*m*/  /*build list*/
                     end   /*?*/
        return
```

{{out|output|text=  when using the default (internal) inputs:}}

```txt

        element 1 before sort: ---Four horsemen of the Apocalypse---
        element 2 before sort: 
### ===============================

        element 3 before sort: Famine───black horse
        element 4 before sort: Death───pale horse
        element 5 before sort: Pestilence [Slaughter]───red horse
        element 6 before sort: Conquest [War]───white horse
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
        element 1  after sort: ---Four horsemen of the Apocalypse---
        element 2  after sort: 
### ===============================

        element 3  after sort: Conquest [War]───white horse
        element 4  after sort: Death───pale horse
        element 5  after sort: Famine───black horse
        element 6  after sort: Pestilence [Slaughter]───red horse

Permutation sort took  21  permutations to find the sorted list.

```



## Ring


```ring

# Project : Sorting algorithms/Permutation sort

a = [4, 65, 2, 31, 0, 99, 2, 83, 782]  
result = []
permute(a,1)

for n = 1 to len(result)
     num = 0
     for m = 1 to len(result[n]) - 1
          if result[n][m] <= result[n][m+1]  
             num = num + 1
          ok
     next
      if num = len(result[n]) - 1
         nr = n
         exit
      ok
next 
see "" + nr + " permutations required to sort " + len(a) + " items." + nl

func permute(a,k) 
       if k = len(a)
          add(result,a)
       else
          for i = k to len(a)
               temp=a[k]
               a[k]=a[i]
               a[i]=temp
               permute(a,k+1)
               temp=a[k]
               a[k]=a[i]
               a[i]=temp
          next
       ok
       return a

```

Output:

```txt

169329 permutations required to sort 9 items.

```



## Ruby

{{works with|Ruby|1.8.7+}}
The Array class has a permutation method that, with no arguments, returns an enumerable object.

```ruby
class Array
  def permutationsort
    permutation.each{|perm| return perm if perm.sorted?}
  end
  
  def sorted?
    each_cons(2).all? {|a, b| a <= b}
  end
end
```



## Scheme


```scheme
(define (insertions e list)
  (if (null? list)
      (cons (cons e list) list)
      (cons (cons e list)
            (map (lambda (tail) (cons (car list) tail))
                 (insertions e (cdr list))))))

(define (permutations list)
  (if (null? list)
      (cons list list)
      (apply append (map (lambda (permutation)
                           (insertions (car list) permutation))
                         (permutations (cdr list))))))

(define (sorted? list)
  (cond ((null? list) #t)
        ((null? (cdr list)) #t)
        ((<= (car list) (cadr list)) (sorted? (cdr list)))
        (else #f)))

(define (permutation-sort list)
  (let loop ((permutations (permutations list)))
    (if (sorted? (car permutations))
        (car permutations)
        (loop (cdr permutations)))))
```



## Sidef

{{trans|Perl}}

```ruby
func psort(x, d=x.end) {

    if (d.is_zero) {
        for i in (1 .. x.end) {
            (x[i] < x[i-1]) && return false;
        }
        return true;
    }

    (d+1).times {
        x.prepend(x.splice(d, 1)...);
        x[d] < x[d-1] && next;
        psort(x, d-1) && return true;
    }

    return false;
}

var a = 10.of { 100.irand };
say "Before:\t#{a}";
psort(a);
say "After:\t#{a}";
```

{{out}}

```txt

Before:	60 98 85 85 37 0 62 96 95 2
After:	0 2 37 60 62 85 85 95 96 98

```



## Tcl

{{tcllib|struct::list}}
The <code>firstperm</code> procedure actually returns the lexicographically first permutation of the input list.  However, to meet the letter of the problem, let's loop:

```tcl
package require Tcl 8.5
package require struct::list

proc inorder {list} {::tcl::mathop::<= {*}$list}

proc permutationsort {list} {
    while { ! [inorder $list]} {
        set list [struct::list nextperm $list]
    }
    return $list
}
```



## Ursala

Standard library functions to generate permutations and test for ordering by a
given predicate are used.

```Ursala
#import std

permsort "p" = ~&ihB+ ordered"p"*~+ permutations

#cast %sL

example = permsort(lleq) <'pmf','oao','ejw','hhp','oqh','ock','dwj'>
```


{{out}}

```txt
<'dwj','ejw','hhp','oao','ock','oqh','pmf'>
```



## zkl

Performance is horrid

```zkl
rns:=T(4, 65, 2, 31, 0, 99, 2, 83, 782, 1);
fcn psort(list){ len:=list.len(); cnt:=Ref(0);
   foreach ns in (Utils.Helpers.permuteW(list)){ // lasy permutations
      cnt.set(1);
      ns.reduce('wrap(p,n){ if(p>n)return(Void.Stop); cnt.inc(); n });
      if(cnt.value==len) return(ns);
   }
}(rns).println();
```

{{out}}

```txt
L(0,1,2,2,4,31,65,83,99,782)
```

