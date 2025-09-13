+++
title = "Sorting algorithms/Patience sort"
description = ""
date = 2019-09-16T05:05:48Z
aliases = []
[extra]
id = 15995
[taxonomies]
categories = ["task", "Sorting Algorithms"]
tags = []
+++

## Task

Sort an array of numbers (of any convenient size) into ascending order using   [[wp:Patience sorting|Patience sorting]].


## Related tasks

:*   [[Longest increasing subsequence]]





## C

Takes integers as input, prints out usage on incorrect invocation

```C

#include<stdlib.h>
#include<stdio.h>

int* patienceSort(int* arr,int size){
	int decks[size][size],i,j,min,pickedRow;

	int *count = (int*)calloc(sizeof(int),size),*sortedArr = (int*)malloc(size*sizeof(int));

	for(i=0;i<size;i++){
		for(j=0;j<size;j++){
			if(count[j]==0 || (count[j]>0 && decks[j][count[j]-1]>=arr[i])){
				decks[j][count[j]] = arr[i];
				count[j]++;
				break;
			}
		}
	}

	min = decks[0][count[0]-1];
	pickedRow = 0;

	for(i=0;i<size;i++){
		for(j=0;j<size;j++){
			if(count[j]>0 && decks[j][count[j]-1]<min){
				min = decks[j][count[j]-1];
				pickedRow = j;
			}
		}
		sortedArr[i] = min;
		count[pickedRow]--;

		for(j=0;j<size;j++)
			if(count[j]>0){
				min = decks[j][count[j]-1];
				pickedRow = j;
				break;
			}
	}

	free(count);
	free(decks);

	return sortedArr;
}

int main(int argC,char* argV[])
{
	int *arr, *sortedArr, i;

	if(argC==0)
		printf("Usage : %s <integers to be sorted separated by space>");
	else{
		arr = (int*)malloc((argC-1)*sizeof(int));

		for(i=1;i<=argC;i++)
			arr[i-1] = atoi(argV[i]);

		sortedArr = patienceSort(arr,argC-1);

		for(i=0;i<argC-1;i++)
			printf("%d ",sortedArr[i]);
	}

	return 0;
}

```

Invocation and output :

```txt

C:\rosettaCode>patienceSort.exe 4 65 2 -31 0 99 83 781 1
-31 0 1 2 4 65 83 99 781

```



## C++


```cpp
#include <iostream>
#include <vector>
#include <stack>
#include <iterator>
#include <algorithm>
#include <cassert>

template <class E>
struct pile_less {
  bool operator()(const std::stack<E> &pile1, const std::stack<E> &pile2) const {
    return pile1.top() < pile2.top();
  }
};

template <class E>
struct pile_greater {
  bool operator()(const std::stack<E> &pile1, const std::stack<E> &pile2) const {
    return pile1.top() > pile2.top();
  }
};


template <class Iterator>
void patience_sort(Iterator first, Iterator last) {
  typedef typename std::iterator_traits<Iterator>::value_type E;
  typedef std::stack<E> Pile;

  std::vector<Pile> piles;
  // sort into piles
  for (Iterator it = first; it != last; it++) {
    E& x = *it;
    Pile newPile;
    newPile.push(x);
    typename std::vector<Pile>::iterator i =
      std::lower_bound(piles.begin(), piles.end(), newPile, pile_less<E>());
    if (i != piles.end())
      i->push(x);
    else
      piles.push_back(newPile);
  }

  // priority queue allows us to merge piles efficiently
  // we use greater-than comparator for min-heap
  std::make_heap(piles.begin(), piles.end(), pile_greater<E>());
  for (Iterator it = first; it != last; it++) {
    std::pop_heap(piles.begin(), piles.end(), pile_greater<E>());
    Pile &smallPile = piles.back();
    *it = smallPile.top();
    smallPile.pop();
    if (smallPile.empty())
      piles.pop_back();
    else
      std::push_heap(piles.begin(), piles.end(), pile_greater<E>());
  }
  assert(piles.empty());
}

int main() {
  int a[] = {4, 65, 2, -31, 0, 99, 83, 782, 1};
  patience_sort(a, a+sizeof(a)/sizeof(*a));
  std::copy(a, a+sizeof(a)/sizeof(*a), std::ostream_iterator<int>(std::cout, ", "));
  std::cout << std::endl;
  return 0;
}
```

```txt
-31, 0, 1, 2, 4, 65, 83, 99, 782,
```



## Clojure


```clojure

(defn patience-insert
  "Inserts a value into the sequence where each element is a stack.
   Comparison replaces the definition of less than.
   Uses the greedy strategy."
  [comparison sequence value]
  (lazy-seq
   (if (empty? sequence) `((~value)) ;; If there are no places to put the "card", make a new stack
       (let [stack (first sequence)
             top       (peek stack)]
         (if (comparison value top)
           (cons (conj stack value)  ;; Either put the card in a stack or recurse to the next stack
                 (rest sequence))
           (cons stack
                 (patience-insert comparison
                                  (rest sequence)
                                  value)))))))

(defn patience-remove
  "Removes the value from the top of the first stack it shows up in.
   Leaves the stacks otherwise intact."
  [sequence value]
  (lazy-seq
   (if (empty? sequence) nil              ;; If there are no stacks, we have no work to do
       (let [stack (first sequence)
             top       (peek stack)]
         (if (= top value)                ;; Are we there yet?
           (let [left-overs (pop stack)]
             (if (empty? left-overs)      ;; Handle the case that the stack is empty and needs to be removed
               (rest sequence)
               (cons left-overs
                     (rest sequence))))
           (cons stack
                 (patience-remove (rest sequence)
                                  value)))))))

(defn patience-recover
  "Builds a sorted sequence from a list of patience stacks.
   The given comparison takes the place of 'less than'"
  [comparison sequence]
  (loop [sequence sequence
         sorted         []]
    (if (empty? sequence) sorted
        (let [smallest  (reduce #(if (comparison %1 %2) %1 %2)  ;; Gets the smallest element in the list
                                (map peek sequence))
              remaining    (patience-remove sequence smallest)]
          (recur remaining
                 (conj sorted smallest)))))) ;; Recurse over the remaining values and add the new smallest to the end of the sorted list

(defn patience-sort
  "Sorts the sequence by comparison.
   First builds the list of valid patience stacks.
   Then recovers the sorted list from those.
   If you don't supply a comparison, assumes less than."
  ([comparison sequence]
     (->> (reduce (comp doall ;; This is prevent a stack overflow by making sure all work is done when it needs to be
                        (partial patience-insert comparison)) ;; Insert all the values into the list of stacks
                  nil
                  sequence)
          (patience-recover comparison)))              ;; After we have the stacks, send it off to recover the sorted list
  ([sequence]
     ;; In the case we don't have an operator, defer to ourselves with less than
     (patience-sort < sequence)))

;; Sort the test sequence and print it
(println (patience-sort [4 65 2 -31 0 99 83 782 1]))

```

```txt
[-31 0 1 2 4 65 83 99 782]
```



## D

```d
import std.stdio, std.array, std.range, std.algorithm;

void patienceSort(T)(T[] items) /*pure nothrow @safe*/
if (__traits(compiles, T.init < T.init)) {
    //SortedRange!(int[][], q{ a.back < b.back }) piles;
    T[][] piles;

    foreach (x; items) {
        auto p = [x];
        immutable i = piles.length -
                      piles
                      .assumeSorted!q{ a.back < b.back }
                      .upperBound(p)
                      .length;
        if (i != piles.length)
            piles[i] ~= x;
        else
            piles ~= p;
    }

    piles.nWayUnion!q{ a > b }.copy(items.retro);
}

void main() {
    auto data = [4, 65, 2, -31, 0, 99, 83, 782, 1];
    data.patienceSort;
    assert(data.isSorted);
    data.writeln;
}
```

```txt
[-31, 0, 1, 2, 4, 65, 83, 99, 782]
```



## Elixir


```elixir
defmodule Sort do
  def patience_sort(list) do
    piles = deal_pile(list, [])
    merge_pile(piles, [])
  end

  defp deal_pile([], piles), do: piles
  defp deal_pile([h|t], piles) do
    index = Enum.find_index(piles, fn pile -> hd(pile) <= h end)
    new_piles = if index, do:   add_element(piles, index, h, []),
                          else: piles ++ [[h]]
    deal_pile(t, new_piles)
  end

  defp add_element([h|t], 0,     elm, work), do: Enum.reverse(work, [[elm | h] | t])
  defp add_element([h|t], index, elm, work), do: add_element(t, index-1, elm, [h | work])

  defp merge_pile([], list), do: list
  defp merge_pile(piles, list) do
    {max, index} = max_index(piles)
    merge_pile(delete_element(piles, index, []), [max | list])
  end

  defp max_index([h|t]), do: max_index(t, hd(h), 1, 0)

  defp max_index([], max, _, max_i), do: {max, max_i}
  defp max_index([h|t], max, index, _) when hd(h)>max, do: max_index(t, hd(h), index+1, index)
  defp max_index([_|t], max, index, max_i)           , do: max_index(t, max, index+1, max_i)

  defp delete_element([h|t], 0, work) when length(h)==1, do: Enum.reverse(work, t)
  defp delete_element([h|t], 0, work)                  , do: Enum.reverse(work, [tl(h) | t])
  defp delete_element([h|t], index, work), do: delete_element(t, index-1, [h | work])
end

IO.inspect Sort.patience_sort [4, 65, 2, -31, 0, 99, 83, 782, 1]
```


```txt

[-31, 0, 1, 2, 4, 65, 83, 99, 782]

```



## Go

This version is written for int slices, but can be easily modified to sort other types.

```go
package main

import (
  "fmt"
  "container/heap"
  "sort"
)

type IntPile []int
func (self IntPile) Top() int { return self[len(self)-1] }
func (self *IntPile) Pop() int {
    x := (*self)[len(*self)-1]
    *self = (*self)[:len(*self)-1]
    return x
}

type IntPilesHeap []IntPile
func (self IntPilesHeap) Len() int { return len(self) }
func (self IntPilesHeap) Less(i, j int) bool { return self[i].Top() < self[j].Top() }
func (self IntPilesHeap) Swap(i, j int) { self[i], self[j] = self[j], self[i] }
func (self *IntPilesHeap) Push(x interface{}) { *self = append(*self, x.(IntPile)) }
func (self *IntPilesHeap) Pop() interface{} {
    x := (*self)[len(*self)-1]
    *self = (*self)[:len(*self)-1]
    return x
}

func patience_sort (n []int) {
  var piles []IntPile
  // sort into piles
  for _, x := range n {
    j := sort.Search(len(piles), func (i int) bool { return piles[i].Top() >= x })
    if j != len(piles) {
      piles[j] = append(piles[j], x)
    } else {
      piles = append(piles, IntPile{ x })
    }
  }

  // priority queue allows us to merge piles efficiently
  hp := IntPilesHeap(piles)
  heap.Init(&hp)
  for i, _ := range n {
    smallPile := heap.Pop(&hp).(IntPile)
    n[i] = smallPile.Pop()
    if len(smallPile) != 0 {
      heap.Push(&hp, smallPile)
    }
  }
  if len(hp) != 0 {
    panic("something went wrong")
  }
}

func main() {
    a := []int{4, 65, 2, -31, 0, 99, 83, 782, 1}
    patience_sort(a)
    fmt.Println(a)
}
```

```txt
[-31 0 1 2 4 65 83 99 782]
```



## Haskell


```haskell
import Control.Monad.ST
import Control.Monad
import Data.Array.ST
import Data.List
import qualified Data.Set as S

newtype Pile a = Pile [a]

instance Eq a => Eq (Pile a) where
  Pile (x:_) == Pile (y:_) = x == y

instance Ord a => Ord (Pile a) where
  Pile (x:_) `compare` Pile (y:_) = x `compare` y

patienceSort :: Ord a => [a] -> [a]
patienceSort = mergePiles . sortIntoPiles where

  sortIntoPiles :: Ord a => [a] -> [[a]]
  sortIntoPiles lst = runST $ do
      piles <- newSTArray (1, length lst) []
      let bsearchPiles x len = aux 1 len where
            aux lo hi | lo > hi = return lo
                      | otherwise = do
              let mid = (lo + hi) `div` 2
              m <- readArray piles mid
              if head m < x then
                aux (mid+1) hi
              else
                aux lo (mid-1)
          f len x = do
            i <- bsearchPiles x len
            writeArray piles i . (x:) =<< readArray piles i
            return $ if i == len+1 then len+1 else len
      len <- foldM f 0 lst
      e <- getElems piles
      return $ take len e
      where newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
            newSTArray = newArray

  mergePiles :: Ord a => [[a]] -> [a]
  mergePiles = unfoldr f . S.fromList . map Pile where
    f pq = case S.minView pq of
             Nothing -> Nothing
             Just (Pile [x], pq') -> Just (x, pq')
             Just (Pile (x:xs), pq') -> Just (x, S.insert (Pile xs) pq')

main :: IO ()
main = print $ patienceSort [4, 65, 2, -31, 0, 99, 83, 782, 1]
```

```txt
[-31,0,1,2,4,65,83,99,782]
```



## J

The data structure for append and transfer are as x argument a list with [[wp:CAR_and_CDR|cdr]] as the stacks and [[wp:CAR_and_CDR|car]] as the data to sort or growing sorted list; and the y argument being the index of pile to operate on.  New piles are created by using the new value, accomplished by selecting the entire x argument as a result.  Filtering removes empty stacks during unpiling.

```J

Until =: 2 :'u^:(0=v)^:_'
Filter =: (#~`)(`:6)

locate_for_append =: 1 i.~ (<&> {:S:0)  NB. returns an index
append =: (<@:(({::~ >:) , 0 {:: [)`]`(}.@:[)}) :: [
pile =: (,  append locate_for_append)/@:(;/)  NB. pile DATA

smallest =: ((>:@:i. , ]) <./)@:({:S:0@:}.) NB. index of pile with smallest value , that value
transfer =: (}:&.>@:({~ {.) , <@:((0{::[),{:@:]))`(1 0 * ])`[}
unpile =: >@:{.@:((0<#S:0)Filter@:(transfer smallest)Until(1=#))@:(a:&,)

patience_sort =: unpile@:pile

assert (/:~ -: patience_sort) ?@$~30    NB. test with 30 randomly chosen integers

Show =: 1 : 0
 smoutput y
 u y
:
 smoutput A=:x ,&:< y
 x u y
)

pile_demo =: (,  append Show  locate_for_append)/@:(;/)  NB. pile DATA
unpile_demo =: >@:{.@:((0<#S:0)Filter@:(transfer Show  smallest)Until(1=#))@:(a:&,)
patience_sort_demo =: unpile_demo@:pile_demo

```



```txt

   JVERSION
Engine: j701/2011-01-10/11:25
Library: 8.02.12
Platform: Linux 64
Installer: unknown
InstallPath: /usr/share/j/8.0.2

   patience_sort_demo Show ?.@$~10
4 6 8 6 5 8 6 6 6 9
┌─────┬─┐
│┌─┬─┐│0│
││6│9││ │
│└─┴─┘│ │
└─────┴─┘
┌───────┬─┐
│┌─┬───┐│1│
││6│9 6││ │
│└─┴───┘│ │
└───────┴─┘
┌─────────┬─┐
│┌─┬─┬───┐│2│
││6│6│9 6││ │
│└─┴─┴───┘│ │
└─────────┴─┘
┌───────────┬─┐
│┌─┬─┬─┬───┐│3│
││8│6│6│9 6││ │
│└─┴─┴─┴───┘│ │
└───────────┴─┘
┌─────────────┬─┐
│┌─┬─┬─┬─┬───┐│0│
││5│8│6│6│9 6││ │
│└─┴─┴─┴─┴───┘│ │
└─────────────┴─┘
┌───────────────┬─┐
│┌─┬───┬─┬─┬───┐│4│
││6│8 5│6│6│9 6││ │
│└─┴───┴─┴─┴───┘│ │
└───────────────┴─┘
┌─────────────────┬─┐
│┌─┬─┬───┬─┬─┬───┐│5│
││8│6│8 5│6│6│9 6││ │
│└─┴─┴───┴─┴─┴───┘│ │
└─────────────────┴─┘
┌───────────────────┬─┐
│┌─┬─┬─┬───┬─┬─┬───┐│0│
││6│8│6│8 5│6│6│9 6││ │
│└─┴─┴─┴───┴─┴─┴───┘│ │
└───────────────────┴─┘
┌─────────────────────┬─┐
│┌─┬───┬─┬───┬─┬─┬───┐│0│
││4│8 6│6│8 5│6│6│9 6││ │
│└─┴───┴─┴───┴─┴─┴───┘│ │
└─────────────────────┴─┘
┌──────────────────────┬───┐
│┌┬─────┬─┬───┬─┬─┬───┐│1 4│
│││8 6 4│6│8 5│6│6│9 6││   │
│└┴─────┴─┴───┴─┴─┴───┘│   │
└──────────────────────┴───┘
┌─────────────────────┬───┐
│┌─┬───┬─┬───┬─┬─┬───┐│3 5│
││4│8 6│6│8 5│6│6│9 6││   │
│└─┴───┴─┴───┴─┴─┴───┘│   │
└─────────────────────┴───┘
┌─────────────────────┬───┐
│┌───┬───┬─┬─┬─┬─┬───┐│1 6│
││4 5│8 6│6│8│6│6│9 6││   │
│└───┴───┴─┴─┴─┴─┴───┘│   │
└─────────────────────┴───┘
┌─────────────────────┬───┐
│┌─────┬─┬─┬─┬─┬─┬───┐│2 6│
││4 5 6│8│6│8│6│6│9 6││   │
│└─────┴─┴─┴─┴─┴─┴───┘│   │
└─────────────────────┴───┘
┌─────────────────────┬───┐
│┌───────┬─┬─┬─┬─┬───┐│3 6│
││4 5 6 6│8│8│6│6│9 6││   │
│└───────┴─┴─┴─┴─┴───┘│   │
└─────────────────────┴───┘
┌─────────────────────┬───┐
│┌─────────┬─┬─┬─┬───┐│3 6│
││4 5 6 6 6│8│8│6│9 6││   │
│└─────────┴─┴─┴─┴───┘│   │
└─────────────────────┴───┘
┌─────────────────────┬───┐
│┌───────────┬─┬─┬───┐│3 6│
││4 5 6 6 6 6│8│8│9 6││   │
│└───────────┴─┴─┴───┘│   │
└─────────────────────┴───┘
┌─────────────────────┬───┐
│┌─────────────┬─┬─┬─┐│1 8│
││4 5 6 6 6 6 6│8│8│9││   │
│└─────────────┴─┴─┴─┘│   │
└─────────────────────┴───┘
┌─────────────────────┬───┐
│┌───────────────┬─┬─┐│1 8│
││4 5 6 6 6 6 6 8│8│9││   │
│└───────────────┴─┴─┘│   │
└─────────────────────┴───┘
┌─────────────────────┬───┐
│┌─────────────────┬─┐│1 9│
││4 5 6 6 6 6 6 8 8│9││   │
│└─────────────────┴─┘│   │
└─────────────────────┴───┘
4 5 6 6 6 6 6 8 8 9

```



## Java


```java
import java.util.*;

public class PatienceSort {
    public static <E extends Comparable<? super E>> void sort (E[] n) {
        List<Pile<E>> piles = new ArrayList<Pile<E>>();
        // sort into piles
        for (E x : n) {
            Pile<E> newPile = new Pile<E>();
            newPile.push(x);
            int i = Collections.binarySearch(piles, newPile);
            if (i < 0) i = ~i;
            if (i != piles.size())
                piles.get(i).push(x);
            else
                piles.add(newPile);
        }

        // priority queue allows us to retrieve least pile efficiently
        PriorityQueue<Pile<E>> heap = new PriorityQueue<Pile<E>>(piles);
        for (int c = 0; c < n.length; c++) {
            Pile<E> smallPile = heap.poll();
            n[c] = smallPile.pop();
            if (!smallPile.isEmpty())
                heap.offer(smallPile);
        }
        assert(heap.isEmpty());
    }

    private static class Pile<E extends Comparable<? super E>> extends Stack<E> implements Comparable<Pile<E>> {
        public int compareTo(Pile<E> y) { return peek().compareTo(y.peek()); }
    }

    public static void main(String[] args) {
	Integer[] a = {4, 65, 2, -31, 0, 99, 83, 782, 1};
	sort(a);
	System.out.println(Arrays.toString(a));
    }
}
```

```txt
[-31, 0, 1, 2, 4, 65, 83, 99, 782]
```



## Julia


```julia
function patiencesort(list::Vector{T}) where T
    piles = Vector{Vector{T}}()
    for n in list
        if isempty(piles) ||
            (i = findfirst(pile -> n <= pile[end], piles)) ==  nothing
            push!(piles, [n])
        else
            push!(piles[i], n)
        end
    end
    mergesorted(piles)
end

function mergesorted(vecvec)
    lengths = map(length, vecvec)
    allsum = sum(lengths)
    sorted = similar(vecvec[1], allsum)
    for i in 1:allsum
        (val, idx) = findmin(map(x -> x[end], vecvec))
        sorted[i] = pop!(vecvec[idx])
        if isempty(vecvec[idx])
            deleteat!(vecvec, idx)
        end
    end
    sorted
end

println(patiencesort(rand(collect(1:1000), 12)))

```
```txt

[186, 243, 255, 257, 427, 486, 513, 613, 657, 734, 866, 907]

```



## Kotlin


```scala
// version 1.1.2

fun <T : Comparable<T>> patienceSort(arr: Array<T>) {
    if (arr.size < 2) return
    val piles = mutableListOf<MutableList<T>>()
    outer@ for (el in arr) {
        for (pile in piles) {
            if (pile.last() > el) {
                pile.add(el)
                continue@outer
            }
        }
        piles.add(mutableListOf(el))
    }

    for (i in 0 until arr.size) {
        var min = piles[0].last()
        var minPileIndex = 0
        for (j in 1 until piles.size) {
            if (piles[j].last() < min) {
                min = piles[j].last()
                minPileIndex = j
            }
        }
        arr[i] = min
        val minPile = piles[minPileIndex]
        minPile.removeAt(minPile.lastIndex)
        if (minPile.size == 0) piles.removeAt(minPileIndex)
    }
}

fun main(args: Array<String>) {
    val iArr = arrayOf(4, 65, 2, -31, 0, 99, 83, 782, 1)
    patienceSort(iArr)
    println(iArr.contentToString())
    val cArr = arrayOf('n', 'o', 'n', 'z', 'e', 'r', 'o', 's', 'u','m')
    patienceSort(cArr)
    println(cArr.contentToString())
    val sArr = arrayOf("dog", "cow", "cat", "ape", "ant", "man", "pig", "ass", "gnu")
    patienceSort(sArr)
    println(sArr.contentToString())
}
```


```txt

[-31, 0, 1, 2, 4, 65, 83, 99, 782]
[e, m, n, n, o, o, r, s, u, z]
[ant, ape, ass, cat, cow, dog, gnu, man, pig]

```



## OCaml


```ocaml
module PatienceSortFn (Ord : Set.OrderedType) : sig
    val patience_sort : Ord.t list -> Ord.t list
  end = struct

  module PilesSet = Set.Make
    (struct
       type t = Ord.t list
       let compare x y = Ord.compare (List.hd x) (List.hd y)
     end);;

  let sort_into_piles list =
    let piles = Array.make (List.length list) [] in
    let bsearch_piles x len =
      let rec aux lo hi =
        if lo > hi then
          lo
        else
          let mid = (lo + hi) / 2 in
          if Ord.compare (List.hd piles.(mid)) x < 0 then
            aux (mid+1) hi
          else
            aux lo (mid-1)
      in
        aux 0 (len-1)
    in
    let f len x =
      let i = bsearch_piles x len in
      piles.(i) <- x :: piles.(i);
      if i = len then len+1 else len
    in
    let len = List.fold_left f 0 list in
    Array.sub piles 0 len

  let merge_piles piles =
    let pq = Array.fold_right PilesSet.add piles PilesSet.empty in
    let rec f pq acc =
      if PilesSet.is_empty pq then
        acc
      else
        let elt = PilesSet.min_elt pq in
        match elt with
          [] -> failwith "Impossible"
        | x::xs ->
          let pq' = PilesSet.remove elt pq in
          f (if xs = [] then pq' else PilesSet.add xs pq') (x::acc)
    in
    List.rev (f pq [])

  let patience_sort n =
    merge_piles (sort_into_piles n)
end
```

Usage:

```txt
# module IntPatienceSort = PatienceSortFn
  (struct
     type t = int
     let compare = compare
   end);;
module IntPatienceSort : sig val patience_sort : int list -> int list end
# IntPatienceSort.patience_sort [4; 65; 2; -31; 0; 99; 83; 782; 1];;
- : int list = [-31; 0; 1; 2; 4; 65; 83; 99; 782]
```



## Perl

```Perl
sub patience_sort {
    my @s = [shift];
    for my $card (@_) {
	my @t = grep { $_->[-1] > $card } @s;
	if (@t) { push @{shift(@t)}, $card }
	else { push @s, [$card] }
    }
    my @u;
    while (my @v = grep @$_, @s) {
	my $value = (my $min = shift @v)->[-1];
	for (@v) {
	    ($min, $value) =
	    ($_, $_->[-1]) if $_->[-1] < $value
	}
	push @u, pop @$min;
    }
    return @u
}

print join ' ', patience_sort qw(4 3 6 2 -1 13 12 9);

```

```txt
-1 2 3 4 6 9 12 13
```



## Perl 6

```perl6
multi patience(*@deck) {
    my @stacks;
    for @deck -> $card {
        with @stacks.first: $card before *[*-1] -> $stack {
            $stack.push: $card;
        }
        else {
            @stacks.push: [$card];
        }
    }
    gather while @stacks {
        take .pop given min :by(*[*-1]), @stacks;
        @stacks .= grep: +*;
    }
}

say ~patience ^10 . pick(*);
```

```txt
0 1 2 3 4 5 6 7 8 9
```



## Phix


```Phix
function patience_sort(sequence s)
    -- create list of sorted lists
    sequence piles = {}
    for i=1 to length(s) do
        object n = s[i]
        for p=1 to length(piles)+1 do
            if p>length(piles) then
                piles = append(piles,{n})
            elsif n>=piles[p][$] then
                piles[p] = append(piles[p],n)
                exit
            end if
        end for
    end for
    -- merge sort the piles
    sequence res = {}
    while length(piles) do
        integer idx = smallest(piles,return_index:=true)
        res = append(res,piles[idx][1])
        if length(piles[idx])=1 then
            piles[idx..idx] = {}
        else
            piles[idx] = piles[idx][2..$]
        end if
    end while
    return res
end function

constant tests = {{4,65,2,-31,0,99,83,782,1},
                  {0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15},
                  "nonzerosum",
                  {"dog", "cow", "cat", "ape", "ant", "man", "pig", "ass", "gnu"}}

for i=1 to length(tests) do
    pp(patience_sort(tests[i]),{pp_StrFmt,-2})
end for
```

```txt

{-31,0,1,2,4,65,83,99,782}
{0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
"emnnoorsuz"
{"ant", "ape", "ass", "cat", "cow", "dog", "gnu", "man", "pig"}

```



## PHP


```php
<?php
class PilesHeap extends SplMinHeap {
    public function compare($pile1, $pile2) {
        return parent::compare($pile1->top(), $pile2->top());
    }
}

function patience_sort(&$n) {
    $piles = array();
    // sort into piles
    foreach ($n as $x) {
        // binary search
        $low = 0; $high = count($piles)-1;
        while ($low <= $high) {
            $mid = (int)(($low + $high) / 2);
            if ($piles[$mid]->top() >= $x)
                $high = $mid - 1;
            else
                $low = $mid + 1;
        }
        $i = $low;
        if ($i == count($piles))
            $piles[] = new SplStack();
        $piles[$i]->push($x);
    }

    // priority queue allows us to merge piles efficiently
    $heap = new PilesHeap();
    foreach ($piles as $pile)
        $heap->insert($pile);
    for ($c = 0; $c < count($n); $c++) {
        $smallPile = $heap->extract();
        $n[$c] = $smallPile->pop();
        if (!$smallPile->isEmpty())
        $heap->insert($smallPile);
    }
    assert($heap->isEmpty());
}

$a = array(4, 65, 2, -31, 0, 99, 83, 782, 1);
patience_sort($a);
print_r($a);
?>
```

```txt
Array
(
    [0] => -31
    [1] => 0
    [2] => 1
    [3] => 2
    [4] => 4
    [5] => 65
    [6] => 83
    [7] => 99
    [8] => 782
)
```



## PicoLisp


```PicoLisp
(de leftmost (Lst N H)
   (let L 1
      (while (<= L H)
         (use (X)
            (setq X (/ (+ L H) 2))
         (if (>= (caar (nth Lst X)) N)
               (setq H (dec X))
               (setq L (inc X)) ) ) )
      L ) )

(de patience (Lst)
   (let (L (cons (cons (car Lst)))  C 1  M NIL)
      (for N (cdr Lst)
         (let I (leftmost L N C)
            (and
               (> I C)
               (conc L (cons NIL))
               (inc 'C) )
            (push (nth L I) N) ) )
      (make
         (loop
            (setq M (cons 0 T))
            (for (I . Y) L
               (let? S (car Y)
                  (and
                     (< S (cdr M))
                     (setq M (cons I S)) ) ) )
            (T (=T (cdr M)))
            (link (pop (nth L (car M)))) ) ) ) )

(println
   (patience (4 65 2 -31 0 99 83 782 1)) )

(bye)
```



## Python

{{works with|Python|2.7+ and 3.2+}} (for <tt>functools.total_ordering</tt>)

```python
from functools import total_ordering
from bisect import bisect_left
from heapq import merge

@total_ordering
class Pile(list):
    def __lt__(self, other): return self[-1] < other[-1]
    def __eq__(self, other): return self[-1] == other[-1]

def patience_sort(n):
    piles = []
    # sort into piles
    for x in n:
        new_pile = Pile([x])
        i = bisect_left(piles, new_pile)
        if i != len(piles):
            piles[i].append(x)
        else:
            piles.append(new_pile)

    # use a heap-based merge to merge piles efficiently
    n[:] = merge(*[reversed(pile) for pile in piles])

if __name__ == "__main__":
    a = [4, 65, 2, -31, 0, 99, 83, 782, 1]
    patience_sort(a)
    print a
```

```txt
[-31, 0, 1, 2, 4, 65, 83, 99, 782]
```



## Racket



```racket
#lang racket/base
(require racket/match racket/list)

;; the car of a pile is the "bottom", i.e. where we place a card
(define (place-greedily ps-in c <?)
  (let inr ((vr null) (ps ps-in))
    (match ps
      [(list) (reverse (cons (list c) vr))]
      [(list (and psh (list ph _ ...)) pst ...)
       #:when (<? c ph) (append (reverse (cons (cons c psh) vr)) pst)]
      [(list psh pst ...) (inr (cons psh vr) pst)])))

(define (patience-sort cs-in <?)
  ;; Scatter
  (define piles
    (let scatter ((cs cs-in) (ps null))
      (match cs [(list) ps] [(cons a d) (scatter d (place-greedily ps a <?))])))
  ;; Gather
  (let gather ((rv null) (ps piles))
    (match ps
      [(list) (reverse rv)]
      [(list psh pst ...)
       (let scan ((least psh) (seens null) (unseens pst))
         (define least-card (car least))
         (match* (unseens least)
           [((list) (list l)) (gather (cons l rv) seens)]
           [((list) (cons l lt)) (gather (cons l rv) (cons lt seens))]
           [((cons (and ush (cons u _)) ust) (cons l _))
            #:when (<? l u) (scan least (cons ush seens) ust)]
           [((cons ush ust) least) (scan ush (cons least seens) ust)]))])))

(patience-sort (shuffle (for/list ((_ 10)) (random 7))) <)
```

```txt
'(1 1 2 2 2 3 4 4 4 5)
```



## REXX

The items to be sorted can be any form of REXX number, not just integers;   the items may also be character strings.

Duplicates are also sorted correctly.

```rexx
/*REXX program sorts a list of things (or items) using the  patience sort  algorithm.   */
parse arg xxx;     say ' input: '      xxx       /*obtain a list of things from the C.L.*/
n=words(xxx);      #=0;      !.=1                /*N:  # of things;  #:  number of piles*/
@.=                                              /* [↓]  append or create a pile  (@.j) */
   do i=1  for n;            q=word(xxx, i)      /* [↓]  construct the piles of things. */
                 do j=1  for #                   /*add the   Q   thing (item) to a pile.*/
                 if q>word(@.j,1) then iterate   /*Is this item greater?   Then skip it.*/
                 @.j=q  @.j;           iterate i /*add this item to the top of the pile.*/
                 end   /*j*/                     /* [↑]  find a pile, or make a new pile*/
   #=#+1;  @.#=q                                 /*increase the pile count;  a new pile.*/
   end                 /*i*/                     /*we are done with creating the piles. */
$=                                               /* [↓]   build a thingy list from piles*/
   do k=1  until  words($)==n                    /*pick off the smallest from the piles.*/
   _=                                            /*this is the smallest thingy so far···*/
           do m=1  for  #;   z=word(@.m, !.m)    /*traipse through many piles of items. */
           if z==''  then iterate                /*Is this pile null?    Then skip pile.*/
           if _==''  then _=z                    /*assume this one is the low pile value*/
           if _>=z   then do;  _=z;  p=m;  end   /*found a low value in a pile of items.*/
           end   /*m*/                           /*the traipsing is done, we found a low*/
   $=$ _                                         /*add to the output thingy  ($)  list. */
   !.p=!.p + 1                                   /*bump the thingy pointer in pile  P.  */
   end           /*k*/                           /* [↑]  each iteration finds a low item*/
                                                 /* [↓]  string  $  has a leading blank.*/
say 'output: '       strip($)                    /*stick a fork in it,  we're all done. */
```

```txt

 input:  4 65 2 -31 0 99 83 782 7.88 1e1 1
output:  -31 0 1 2 4 7.88 1e1 65 83 99 782

```

```txt

 input:  dog cow cat ape ant man pterodactyl
output:  ant ape cat cow dog man pterodactyl

```



## Ruby


```ruby
class Array
  def patience_sort
    piles = []
    each do |i|
      if (idx = piles.index{|pile| pile.last <= i})
        piles[idx] << i
      else
        piles << [i]    #create a new pile
      end
    end
    # merge piles
    result = []
    until piles.empty?
      first = piles.map(&:first)
      idx = first.index(first.min)
      result << piles[idx].shift
      piles.delete_at(idx) if piles[idx].empty?
    end
    result
  end
end

a = [4, 65, 2, -31, 0, 99, 83, 782, 1]
p a.patience_sort
```


```txt
[-31, 0, 1, 2, 4, 65, 83, 99, 782]
```


## Scala

```Scala
import scala.collection.mutable

object PatienceSort extends App {
  def sort[A](source: Iterable[A])(implicit bound: A => Ordered[A]): Iterable[A] = {
    val  piles = mutable.ListBuffer[mutable.Stack[A]]()

    def PileOrdering: Ordering[mutable.Stack[A]] =
      (a: mutable.Stack[A], b: mutable.Stack[A]) => b.head.compare(a.head)

    // Use a priority queue, to simplify extracting minimum elements.
    val pq = new mutable.PriorityQueue[mutable.Stack[A]]()(PileOrdering)

    // Create ordered piles of elements
    for (elem <- source) {
      // Find leftmost "possible" pile
      // If there isn't a pile available, add a new one.
      piles.find(p => p.head >= elem) match {
        case Some(p) => p.push(elem)
        case _ => piles += mutable.Stack(elem)
      }
    }

    pq ++= piles

    // Return a new list, by taking the smallest stack head
    // until all stacks are empty.
    for (_ <- source) yield {
      val smallestList = pq.dequeue
      val smallestVal = smallestList.pop

      if (smallestList.nonEmpty) pq.enqueue(smallestList)
      smallestVal
    }
  }

  println(sort(List(4, 65, 2, -31, 0, 99, 83, 782, 1)))
}
```



## Sidef


```ruby
func patience(deck) {
  var stacks = [];
  deck.each { |card|
    given (stacks.first { card < .last }) { |stack|
      case (defined stack) {
        stack << card
      }
      default {
        stacks << [card]
      }
    }
  }

  gather {
    while (stacks) {
      take stacks.min_by { .last }.pop
      stacks.grep!{ !.is_empty }
    }
  }
}

var a = [4, 65, 2, -31, 0, 99, 83, 782, 1]
say patience(a)
```

```txt

[-31, 0, 1, 2, 4, 65, 83, 99, 782]

```



## Standard ML

```sml
structure PilePriority = struct
  type priority = int
  fun compare (x, y) = Int.compare (y, x) (* we want min-heap *)
  type item = int list
  val priority = hd
end

structure PQ = LeftPriorityQFn (PilePriority)

fun sort_into_piles n =
  let
    val piles = DynamicArray.array (length n, [])
    fun bsearch_piles x =
      let
        fun aux (lo, hi) =
          if lo > hi then
            lo
          else
            let
              val mid = (lo + hi) div 2
            in
              if hd (DynamicArray.sub (piles, mid)) < x then
                aux (mid+1, hi)
              else
                aux (lo, mid-1)
            end
      in
        aux (0, DynamicArray.bound piles)
      end
    fun f x =
      let
        val i = bsearch_piles x
      in
        DynamicArray.update (piles, i, x :: DynamicArray.sub (piles, i))
      end
  in
    app f n;
    piles
  end

fun merge_piles piles =
  let
    val heap = DynamicArray.foldl PQ.insert PQ.empty piles
    fun f (heap, acc) =
      case PQ.next heap of
        NONE => acc
      | SOME (x::xs, heap') =>
        f ((if null xs then heap' else PQ.insert (xs, heap')),
           x::acc)
  in
    rev (f (heap, []))
  end

fun patience_sort n =
  merge_piles (sort_into_piles n)
```

Usage:

```txt
- patience_sort [4, 65, 2, ~31, 0, 99, 83, 782, 1];
val it = [~31,0,1,2,4,65,83,99,782] : int list
```



## Tcl

This uses the <code>-bisect</code> option to <code>lsearch</code> in order to do an efficient binary search (in combination with <code>-index end</code>, which means that the search is indexed by the end of the sublist).

```tcl
package require Tcl 8.6

proc patienceSort {items} {
    # Make the piles
    set piles {}
    foreach item $items {
	set p [lsearch -bisect -index end $piles $item]
	if {$p == -1} {
	    lappend piles [list $item]
	} else {
	    lset piles $p end+1 $item
	}
    }
    # Merge the piles; no suitable builtin, alas
    set indices [lrepeat [llength $piles] 0]
    set result {}
    while 1 {
	set j 0
	foreach pile $piles i $indices {
	    set val [lindex $pile $i]
	    if {$i < [llength $pile] && (![info exist min] || $min > $val)} {
		set k $j
		set next [incr i]
		set min $val
	    }
	    incr j
	}
	if {![info exist min]} break
	lappend result $min
	unset min
	lset indices $k $next
    }
    return $result
}
```

Demonstrating:

```tcl
puts [patienceSort {4 65 2 -31 0 99 83 782 1}]
```

```txt
-31 0 1 2 4 65 83 99 782
```



## zkl


```zkl
fcn patienceSort(ns){
   piles:=L();
   foreach n in (ns){ newPile:=True;   // create list of sorted lists
      foreach p in (piles){
	 if(n>=p[-1]) { p.append(n); newPile=False; break; }
      }
      if(newPile)piles.append(L(n));
   }
   // merge sort the piles
   r:=Sink(List); while(piles){
      mins:=piles.apply("get",0).enumerate();
      min :=mins.reduce(fcn(a,b){ (a[1]<b[1]) and a or b },mins[0])[0];
      r.write(piles[min].pop(0));
      if(not piles[min]) piles.del(min);
   }
   r.close();
}
```


```zkl
T(T(3,2,6,4,3,5,1),
  T(4,65,2,-31,0,99,83,782,1),
  T(0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15),
  "foobar")
.pump(Console.println,patienceSort);
```

```txt

L(1,2,3,3,4,5,6)
L(-31,0,1,2,4,65,83,99,782)
L(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
L("a","b","f","o","o","r")

```

