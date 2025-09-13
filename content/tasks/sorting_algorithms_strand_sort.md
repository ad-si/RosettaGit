+++
title = "Sorting algorithms/Strand sort"
description = ""
date = 2019-01-21T08:05:31Z
aliases = []
[extra]
id = 9526
[taxonomies]
categories = ["task", "Sorting Algorithms"]
tags = []
+++

## Task

Implement the [[wp:Strand sort|Strand sort]].

This is a way of sorting numbers by extracting shorter sequences of already sorted numbers from an unsorted list.





## AutoHotkey

```AutoHotkey
string =
(
-2 0 -2 5 5 3 -1 -3 5 5 0 2 -4 4 2
)
string2 := string
Loop
{
	loop, parse, string, %A_space%
	{
		list := 1 = A_index ? A_loopfield : list
		StringSplit, k, list, %A_space%

		if ( k%k0% <= A_loopfield ) && ( l != "" ) && ( A_index != 1 )
			list := list . " " . A_loopfield

		if ( k%k0% > A_loopfield )
			list := A_loopfield . " " . list , index++
		l := A_loopfield
	}
		if ( index = 0 )
		{
			MsgBox % "unsorted:" string2 "`n    Sorted:" list
			exitapp
		}
		string := list, list = "", index := 0
	}
esc::ExitApp
```
outout<lang>
unsorted:-2 0 -2 5 5 3 -1 -3 5 5 0 2 -4 4 2
  Sorted:-4 -3 -2 -2 -1 0 0 2 2 3 4 5 5 5 5
```



## C

Strand sort using singly linked list.  C99, compiled with <code>gcc -std=c99</code>

```c
#include <stdio.h>

typedef struct node_t *node, node_t;
struct node_t { int v; node next; };
typedef struct { node head, tail; } slist;

void push(slist *l, node e) {
	if (!l->head) l->head = e;
	if (l->tail)  l->tail->next = e;
	l->tail = e;
}

node removehead(slist *l) {
	node e = l->head;
	if (e) {
		l->head = e->next;
		e->next = 0;
	}
	return e;
}

void join(slist *a, slist *b) {
	push(a, b->head);
	a->tail = b->tail;
}

void merge(slist *a, slist *b) {
	slist r = {0};
	while (a->head && b->head)
		push(&r, removehead(a->head->v <= b->head->v ? a : b));

	join(&r, a->head ? a : b);
	*a = r;
	b->head = b->tail = 0;
}

void sort(int *ar, int len)
{
	node_t all[len];

	// array to list
	for (int i = 0; i < len; i++)
		all[i].v = ar[i], all[i].next = i < len - 1 ? all + i + 1 : 0;

	slist list = {all, all + len - 1}, rem, strand = {0},  res = {0};

	for (node e = 0; list.head; list = rem) {
		rem.head = rem.tail = 0;
		while ((e = removehead(&list)))
			push((!strand.head || e->v >= strand.tail->v) ? &strand : &rem, e);

		merge(&res, &strand);
	}

	// list to array
	for (int i = 0; res.head; i++, res.head = res.head->next)
		ar[i] = res.head->v;
}

void show(const char *title, int *x, int len)
{
	printf("%s ", title);
	for (int i = 0; i < len; i++)
		printf("%3d ", x[i]);
	putchar('\n');
}

int main(void)
{
	int x[] = {-2,0,-2,5,5,3,-1,-3,5,5,0,2,-4,4,2};
#	define SIZE sizeof(x)/sizeof(int)

	show("before sort:", x, SIZE);
	sort(x, sizeof(x)/sizeof(int));
	show("after sort: ", x, SIZE);

	return 0;
}
```
outout<lang>before sort:  -2   0  -2   5   5   3  -1  -3   5   5   0   2  -4   4   2
after sort:   -4  -3  -2  -2  -1   0   0   2   2   3   4   5   5   5   5
```



## C++


```cpp
#include <list>

template <typename T>
std::list<T> strandSort(std::list<T> lst) {
  if (lst.size() <= 1)
    return lst;
  std::list<T> result;
  std::list<T> sorted;
  while (!lst.empty()) {
    sorted.push_back(lst.front());
    lst.pop_front();
    for (typename std::list<T>::iterator it = lst.begin(); it != lst.end(); ) {
      if (sorted.back() <= *it) {
        sorted.push_back(*it);
        it = lst.erase(it);
      } else
        it++;
    }
    result.merge(sorted);
  }
  return result;
}
```



## Clojure


```Clojure
(ns rosettacode.strand-sort)

(defn merge-join
  "Produces a globally sorted seq from two sorted seqables"
  [[a & la :as all] [b & lb :as bll]]
  (cond (nil? a) bll
        (nil? b) all
        (< a b) (cons a (lazy-seq (merge-join la bll)))
        true    (cons b (lazy-seq (merge-join all lb)))))

(defn unbraid
  "Separates a sorted list from a sequence"
  [u]
  (when (seq u)
    (loop [[x & xs] u
           u []
           s []
           e x]
      (if (nil? x)
        [s u]
        (if (>= x e)
          (recur xs u (conj s x) x)
          (recur xs (conj u x) s e))))))

(defn strand-sort
  "http://en.wikipedia.org/wiki/Strand_sort"
  [s]
  (loop [[s u] (unbraid s)
         m nil]
    (if s
      (recur (unbraid u) (merge-join m s))
      m)))

(strand-sort [1, 6, 3, 2, 1, 7, 5, 3])
;;=> (1 1 2 3 3 5 6 7)

```



## CMake

Only for lists of integers.

```cmake
# strand_sort(<output variable> [<value>...]) sorts a list of integers.
function(strand_sort var)
  # Strand sort moves elements from _ARGN_ to _answer_.
  set(answer)                   # answer: a sorted list
  while(DEFINED ARGN)
    # Split _ARGN_ into two lists, _accept_ and _reject_.
    set(accept)                 # accept: elements in sorted order
    set(reject)                 # reject: all other elements
    set(p)
    foreach(e ${ARGN})
      if(DEFINED p AND p GREATER ${e})
        list(APPEND reject ${e})
      else()
        list(APPEND accept ${e})
        set(p ${e})
      endif()
    endforeach(e)

    # Prepare to merge _accept_ into _answer_. First, convert both lists
    # into arrays, for better indexing: set(e ${answer${i}}) is faster
    # than list(GET answer ${i} e).
    set(la 0)
    foreach(e ${answer})
      math(EXPR la "${la} + 1")
      set(answer${la} ${e})
    endforeach(e)
    set(lb 0)
    foreach(e ${accept})
      math(EXPR lb "${lb} + 1")
      set(accept${lb} ${e})
    endforeach(e)

    # Merge _accept_ into _answer_.
    set(answer)
    set(ia 1)
    set(ib 1)
    while(NOT ia GREATER ${la})         # Iterate elements of _answer_.
      set(ea ${answer${ia}})
      while(NOT ib GREATER ${lb})       # Take elements from _accept_,
        set(eb ${accept${ib}})          #   while they are less than
        if(eb LESS ${ea})               #   next element of _answer_.
          list(APPEND answer ${eb})
          math(EXPR ib "${ib} + 1")
        else()
          break()
        endif()
      endwhile()
      list(APPEND answer ${ea})         # Take next from _answer_.
      math(EXPR ia "${ia} + 1")
    endwhile()
    while(NOT ib GREATER ${lb})         # Take rest of _accept_.
      list(APPEND answer ${accept${ib}})
      math(EXPR ib "${ib} + 1")
    endwhile()

    # This _reject_ becomes next _ARGN_. If _reject_ is empty, then
    # set(ARGN) undefines _ARGN_, breaking the loop.
    set(ARGN ${reject})
  endwhile(DEFINED ARGN)

  set("${var}" ${answer} PARENT_SCOPE)
endfunction(strand_sort)
```



```cmake
strand_sort(result 11 55 55 44 11 33 33 44 22 22)
message(STATUS "${result}")  # -- 11;11;22;22;33;33;44;44;55;55
```



## Common Lisp


```lisp
(defun strand-sort (l cmp)
  (if l
    (let* ((l (reverse l))
	   (o (list (car l))) n)
      (loop for i in (cdr l) do
	    (push i (if (funcall cmp (car o) i) n o)))
      (merge 'list o (strand-sort n cmp) #'<))))

(let ((r (loop repeat 15 collect (random 10))))
  (print r)
  (print (strand-sort r #'<)))
```
output<lang>(5 8 6 0 6 8 4 7 0 7 1 5 3 3 6)
(0 0 1 3 3 4 5 5 6 6 6 7 7 8 8)
```



## D



###  Using doubly linked lists


```d
import std.stdio, std.container;

DList!T strandSort(T)(DList!T list) {
    static DList!T merge(DList!T left, DList!T right) {
        DList!T result;
        while (!left.empty && !right.empty) {
            if (left.front <= right.front) {
                result.insertBack(left.front);
                left.removeFront();
            } else {
                result.insertBack(right.front);
                right.removeFront();
            }
        }
        result.insertBack(left[]);
        result.insertBack(right[]);
        return result;
    }

    DList!T result, sorted, leftover;

    while (!list.empty) {
        leftover.clear();
        sorted.clear();
        sorted.insertBack(list.front);
        list.removeFront();
        foreach (item; list) {
            if (sorted.back <= item)
                sorted.insertBack(item);
            else
                leftover.insertBack(item);
        }
        result = merge(sorted, result);
        list = leftover;
    }

    return result;
}

void main() {
    auto lst = DList!int([-2,0,-2,5,5,3,-1,-3,5,5,0,2,-4,4,2]);
    foreach (e; strandSort(lst))
        write(e, " ");
}
```

```txt
-4 -3 -2 -2 -1 0 0 2 2 3 4 5 5 5 5
```



###  Faster version using slices


```d
import std.stdio, std.array;

T[] strandSort(T)(const(T)[] list) pure nothrow {
    static T[] merge(const(T)[] left, const(T)[] right) pure nothrow {
        T[] res;
        while (!left.empty && !right.empty) {
            if (left.front <= right.front) {
                res ~= left.front;
                left.popFront;
            } else {
                res ~= right.front;
                right.popFront;
            }
        }
        return res ~ left ~ right;
    }

    T[] result;
    while (!list.empty) {
        auto sorted = list[0 .. 1];
        list.popFront;
        typeof(sorted) leftover;
        foreach (const item; list)
            (sorted.back <= item ? sorted : leftover) ~= item;
        result = merge(sorted, result);
        list = leftover;
    }

    return result;
}

void main() {
    const arr = [-2, 0, -2, 5, 5, 3, -1, -3, 5, 5, 0, 2, -4, 4, 2];
    arr.strandSort.writeln;
}
```

```txt
[-4, -3, -2, -2, -1, 0, 0, 2, 2, 3, 4, 5, 5, 5, 5]
```



## Elixir

```elixir
defmodule Sort do
  def strand_sort(args), do: strand_sort(args, [])

  defp strand_sort([], result), do: result
  defp strand_sort(a, result) do
    {_, sublist, b} = Enum.reduce(a, {hd(a),[],[]}, fn val,{v,l1,l2} ->
                        if v <= val, do: {val, [val | l1], l2},
                                   else: {v,   l1, [val | l2]}
                      end)
    strand_sort(b, :lists.merge(Enum.reverse(sublist), result))
  end
end

IO.inspect Sort.strand_sort [7, 17, 6, 20, 20, 12, 1, 1, 9]
```


```txt

[1, 1, 6, 7, 9, 12, 17, 20, 20]

```



## Euphoria


```euphoria
function merge(sequence left, sequence right)
    sequence result
    result = {}
    while length(left) > 0 and length(right) > 0 do
        if left[$] <= right[1] then
            exit
        elsif right[$] <= left[1] then
            return result & right & left
        elsif left[1] < right[1] then
            result = append(result,left[1])
            left = left[2..$]
        else
            result = append(result,right[1])
            right = right[2..$]
        end if
    end while
    return result & left & right
end function

function strand_sort(sequence s)
    integer j
    sequence result
    result = {}
    while length(s) > 0 do
        j = length(s)
        for i = 1 to length(s)-1 do
            if s[i] > s[i+1] then
                j = i
                exit
            end if
        end for

        result = merge(result,s[1..j])
        s = s[j+1..$]
    end while
    return result
end function

constant s = rand(repeat(1000,10))
puts(1,"Before: ")
? s
puts(1,"After:  ")
? strand_sort(s)
```


Output:

```txt
Before: {551,746,940,903,51,18,346,417,340,502}
After:  {18,51,340,346,417,502,551,746,903,940}
```



## Go


```go
package main

import "fmt"

type link struct {
    int
    next *link
}

func linkInts(s []int) *link {
    if len(s) == 0 {
        return nil
    }
    return &link{s[0], linkInts(s[1:])}
}

func (l *link) String() string {
    if l == nil {
        return "nil"
    }
    r := fmt.Sprintf("[%d", l.int)
    for l = l.next; l != nil; l = l.next {
        r = fmt.Sprintf("%s %d", r, l.int)
    }
    return r + "]"
}

func main() {
    a := linkInts([]int{170, 45, 75, -90, -802, 24, 2, 66})
    fmt.Println("before:", a)
    b := strandSort(a)
    fmt.Println("after: ", b)
}

func strandSort(a *link) (result *link) {
    for a != nil {
        // build sublist
        sublist := a
        a = a.next
        sTail := sublist
        for p, pPrev := a, a; p != nil; p = p.next {
            if p.int > sTail.int {
                // append to sublist
                sTail.next = p
                sTail = p
                // remove from a
                if p == a {
                    a = p.next
                } else {
                    pPrev.next = p.next
                }
            } else {
                pPrev = p
            }
        }
        sTail.next = nil // terminate sublist
        if result == nil {
            result = sublist
            continue
        }
        // merge
        var m, rr *link
        if sublist.int < result.int {
            m = sublist
            sublist = m.next
            rr = result
        } else {
            m = result
            rr = m.next
        }
        result = m
        for {
            if sublist == nil {
                m.next = rr
                break
            }
            if rr == nil {
                m.next = sublist
                break
            }
            if sublist.int < rr.int {
                m.next = sublist
                m = sublist
                sublist = m.next
            } else {
                m.next = rr
                m = rr
                rr = m.next
            }
        }
    }
    return
}
```

Output:

```txt

before: [170 45 75 -90 -802 24 2 66]
after:  [-802 -90 2 24 45 66 75 170]

```



## Haskell



```haskell
-- Same merge as in Merge Sort
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
	| x <= y = x : merge xs (y : ys)
	| otherwise = y : merge (x : xs) ys

strandSort :: (Ord a) => [a] -> [a]
strandSort [] = []
strandSort (x : xs) = merge strand (strandSort rest) where
	(strand, rest) = extractStrand x xs
	extractStrand x [] = ([x], [])
	extractStrand x (x1 : xs)
		| x <= x1 = let (strand, rest) = extractStrand x1 xs in (x : strand, rest)
		| otherwise = let (strand, rest) = extractStrand x xs in (strand, x1 : rest)
```



## J

Using <code>merge</code> defined at [[Sorting algorithms/Merge sort#J]]:


```j
strandSort=: (#~ merge $:^:(0<#)@(#~ -.)) (= >./\)
```


Example use:


```j
   strandSort 3 1 5 4 2
1 2 3 4 5
```


Note: the order in which this J implementation processes the strands differs from the pseudocode currently at the wikipedia page on strand sort and matches the haskell implementation currently at the wikipedia page.

Also note that the individual strands can be seen by using <code>;</code> instead of <code>merge</code>.


```j
   ((#~ ; $:^:(0<#)@(#~ -.)) (= >./\)) 3 1 5 4 2
┌───┬───┬─┬┐
│3 5│1 4│2││
└───┴───┴─┴┘
   ((#~ ; $:^:(0<#)@(#~ -.)) (= >./\)) 3 3 1 2 4 3 5 6
┌─────────┬─────┬┐
│3 3 4 5 6│1 2 3││
└─────────┴─────┴┘
```



## Java

```java5
import java.util.Arrays;
import java.util.LinkedList;

public class Strand{
	// note: the input list is destroyed
	public static <E extends Comparable<? super E>>
	LinkedList<E> strandSort(LinkedList<E> list){
		if(list.size() <= 1) return list;

		LinkedList<E> result = new LinkedList<E>();
		while(list.size() > 0){
			LinkedList<E> sorted = new LinkedList<E>();
			sorted.add(list.removeFirst()); //same as remove() or remove(0)
			for(Iterator<E> it = list.iterator(); it.hasNext(); ){
				E elem = it.next();
				if(sorted.peekLast().compareTo(elem) <= 0){
					sorted.addLast(elem); //same as add(elem) or add(0, elem)
					it.remove();
				}
			}
			result = merge(sorted, result);
		}
		return result;
	}

	private static <E extends Comparable<? super E>>
	LinkedList<E> merge(LinkedList<E> left, LinkedList<E> right){
		LinkedList<E> result = new LinkedList<E>();
		while(!left.isEmpty() && !right.isEmpty()){
			//change the direction of this comparison to change the direction of the sort
			if(left.peek().compareTo(right.peek()) <= 0)
				result.add(left.remove());
			else
				result.add(right.remove());
		}
		result.addAll(left);
		result.addAll(right);
		return result;
	}

	public static void main(String[] args){
		System.out.println(strandSort(new LinkedList<Integer>(Arrays.asList(3,1,2,4,5))));
		System.out.println(strandSort(new LinkedList<Integer>(Arrays.asList(3,3,1,2,4,5))));
		System.out.println(strandSort(new LinkedList<Integer>(Arrays.asList(3,3,1,2,4,3,5,6))));
	}
}
```

Output:

```txt
[1, 2, 3, 4, 5]
[1, 2, 3, 3, 4, 5]
[1, 2, 3, 3, 3, 4, 5, 6]
```



## jq

Most of the implementation is the "merge" function for merging two arrays.  Notice that the helper function, strand, is defined here as an inner function.
```jq
# merge input array with array x by comparing the heads of the arrays
# in turn; # if both arrays are sorted, the result will be sorted:
def merge(x):
  length as $length
  | (x|length) as $xl
  | if $length == 0 then x
    elif $xl == 0 then .
    else
      . as $in
      | reduce range(0; $xl + $length) as $z
         # state [ix, xix, ans]
         ( [0, 0, []];
           if .[0] < $length and
              ((.[1] < $xl and $in[.[0]] <= x[.[1]]) or .[1] == $xl)
           then [(.[0] + 1), .[1], (.[2] + [$in[.[0]]]) ]
           else [.[0], (.[1] + 1), (.[2] + [x[.[1]]]) ]
           end
         ) | .[2]
    end ;

def strand_sort:
  # The inner function emits [strand, remainder]
  def strand:
    if length <= 1 then .
    else
      reduce .[] as $x
      # state: [strand, remainder]
      ([ [], [] ];
       if ((.[0]|length) == 0) or .[0][-1] <= $x
       then [ (.[0] + [$x]), .[1] ]
       else [ .[0], (.[1] + [$x]) ]
       end )
    end ;

  if length <= 1 then .
  else strand as $s
    | ($s[0] | merge( $s[1] | strand_sort))
  end ;

```

Example:
 [1,3,5,2,4,6] | strand_sort


## Julia

```julia
function mergelist(a, b)
    out = Vector{Int}()
    while !isempty(a) && !isempty(b)
        if a[1] < b[1]
            push!(out, popfirst!(a))
        else
            push!(out, popfirst!(b))
        end
    end
    append!(out, a)
    append!(out, b)
    out
end

function strand(a)
    i, s = 1, [popfirst!(a)]
    while i < length(a) + 1
        if a[i] > s[end]
            append!(s, splice!(a, i))
        else
            i += 1
        end
    end
    s
end

strandsort(a) = (out = strand(a); while !isempty(a) out = mergelist(out, strand(a)) end; out)

println(strandsort([1, 6, 3, 2, 1, 7, 5, 3]))

```
```txt

 [1, 1, 2, 3, 3, 5, 6, 7]

```



## Kotlin

```scala
// version 1.1.2

fun <T : Comparable<T>> strandSort(l: List<T>): List<T> {
    fun merge(left: MutableList<T>, right: MutableList<T>): MutableList<T> {
        val res = mutableListOf<T>()
        while (!left.isEmpty() && !right.isEmpty()) {
            if (left[0] <= right[0]) {
                res.add(left[0])
                left.removeAt(0)
            }
            else {
                res.add(right[0])
                right.removeAt(0)
            }
        }
        res.addAll(left)
        res.addAll(right)
        return res
    }

    var list = l.toMutableList()
    var result = mutableListOf<T>()
    while (!list.isEmpty()) {
        val sorted = mutableListOf(list[0])
        list.removeAt(0)
        val leftover = mutableListOf<T>()
        for (item in list) {
            if (sorted.last() <= item)
                sorted.add(item)
            else
                leftover.add(item)
        }
        result = merge(sorted, result)
        list = leftover
    }
    return result
}

fun main(args: Array<String>) {
    val l = listOf(-2, 0, -2, 5, 5, 3, -1, -3, 5, 5, 0, 2, -4, 4, 2)
    println(strandSort(l))
}
```


```txt

[-4, -3, -2, -2, -1, 0, 0, 2, 2, 3, 4, 5, 5, 5, 5]

```



## Mathematica


```Mathematica
StrandSort[ input_ ] := Module[ {results = {}, A = input},
While[Length@A > 0,
 sublist = {A[[1]]}; A = A[[2;;All]];
  For[i = 1, i < Length@A, i++,
   If[ A[[i]] > Last@sublist, AppendTo[sublist, A[[i]]]; A = Delete[A, i];]
  ];
 results = #[[Ordering@#]]&@Join[sublist, results];];
results ]
```

Example usage :

```txt
StrandSort[{2, 3, 7, 5, 1, 4, 7}]
{1, 2, 3, 4, 5, 7, 7}
```



## MAXScript


```MAXScript
fn strandSort arr =
(
	arr = deepcopy arr
	local sub = #()
	local results = #()
	while arr.count > 0 do
	(
		sub = #()
		append sub (amax arr)
		deleteitem arr (for i in 1 to arr.count where arr[i] == amax arr collect i)[1]
		local i = 1
		while i <= arr.count do
		(
			if arr[i] > sub[sub.count] do
			(
				append sub arr[i]
				deleteitem arr i
			)
			i += 1
		)
		results = join sub results
	)
	return results

)
```

Output:

```MAXScript

a = for i in 1 to 20 collect random 1 40
#(19, 26, 14, 31, 11, 33, 2, 14, 32, 28, 12, 38, 2, 37, 27, 18, 31, 24, 39, 28)
strandSort a
#(2, 2, 11, 12, 14, 14, 18, 19, 24, 26, 27, 28, 28, 31, 31, 32, 33, 37, 38, 39)

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

import java.util.List

placesList = [String -
    "UK  London",     "US  New York",   "US  Boston",     "US  Washington" -
  , "UK  Washington", "US  Birmingham", "UK  Birmingham", "UK  Boston"     -
]

lists = [ -
    placesList -
  , strandSort(String[] Arrays.copyOf(placesList, placesList.length)) -
]

loop ln = 0 to lists.length - 1
  cl = lists[ln]
  loop ct = 0 to cl.length - 1
    say cl[ct]
    end ct
    say
  end ln

return

method strandSort(A = String[]) public constant binary returns String[]

  rl = String[A.length]
  al = List strandSort(Arrays.asList(A))
  al.toArray(rl)

  return rl

method strandSort(Alst = List) public constant binary returns ArrayList

  A = ArrayList(Alst)
  result = ArrayList()
  loop label A_ while A.size > 0
    sublist = ArrayList()
    sublist.add(A.get(0))
    A.remove(0)
    loop i_ = 0 while i_ < A.size - 1
      if (Comparable A.get(i_)).compareTo(Comparable sublist.get(sublist.size - 1)) > 0 then do
        sublist.add(A.get(i_))
        A.remove(i_)
        end
      end i_
      result = merge(result, sublist)
    end A_

  return result

method merge(left = List, right = List) public constant binary returns ArrayList

  result = ArrayList()
  loop label mx while left.size > 0 & right.size > 0
    if (Comparable left.get(0)).compareTo(Comparable right.get(0)) <= 0 then do
      result.add(left.get(0))
      left.remove(0)
      end
    else do
      result.add(right.get(0))
      right.remove(0)
      end
    end mx
    if left.size > 0 then do
      result.addAll(left)
      end
    if right.size > 0 then do
      result.addAll(right)
      end

  return result

```

;Output

```txt

UK  London
US  New York
US  Boston
US  Washington
UK  Washington
US  Birmingham
UK  Birmingham
UK  Boston

UK  Birmingham
UK  Boston
UK  London
UK  Washington
US  Birmingham
US  Boston
US  New York
US  Washington

```



## Nim


```nim
proc mergeList[T](a, b: var seq[T]): seq[T] =
  result = @[]
  while a.len > 0 and b.len > 0:
    if a[0] < b[0]:
      result.add a[0]
      a.delete 0
    else:
      result.add b[0]
      b.delete 0
  result.add a
  result.add b

proc strand[T](a: var seq[T]): seq[T] =
  var i = 0
  result = @[a[0]]
  a.delete 0
  while i < a.len:
    if a[i] > result[result.high]:
      result.add a[i]
      a.delete i
    else:
      inc i

proc strandSort[T](a: seq[T]): seq[T] =
  var a = a
  result = a.strand
  while a.len > 0:
    var s = a.strand
    result = mergeList(result, s)

var a = @[1, 6, 3, 2, 1, 7, 5, 3]
echo a.strandSort
```

Output:

```txt
@[1, 1, 2, 3, 3, 5, 6, 7]
```



## OCaml

```ocaml
let rec strand_sort (cmp : 'a -> 'a -> int) : 'a list -> 'a list = function
   []    -> []
 | x::xs ->
   let rec extract_strand x = function
      [] -> [x], []
    | x1::xs when cmp x x1 <= 0 ->
      let strand, rest = extract_strand x1 xs in x::strand, rest
    | x1::xs ->
      let strand, rest = extract_strand x xs in strand, x1::rest
   in
   let strand, rest = extract_strand x xs in
   List.merge cmp strand (strand_sort cmp rest)
```

usage

```txt

# strand_sort compare [170; 45; 75; -90; -802; 24; 2; 66];;
- : int list = [-802; -90; 2; 24; 45; 66; 75; 170]

```



## PARI/GP


```parigp
strandSort(v)={
	my(sorted=[],unsorted=v,remaining,working);
	while(#unsorted,
		remaining=working=List();
		listput(working, unsorted[1]);
		for(i=2,#unsorted,
			if(unsorted[i]<working[#working],
				listput(remaining, unsorted[i])
			,
				listput(working, unsorted[i])
			)
		);
		unsorted=Vec(remaining);
		sorted=merge(sorted, Vec(working))
	);
	sorted
};
merge(u,v)={
	my(ret=vector(#u+#v),i=1,j=1);
	for(k=1,#ret,
		if(i<=#u & (j>#v | u[i]<v[j]),
			ret[k]=u[i];
			i++
		,
			ret[k]=v[j];
			j++
		)
	);
	ret
};
```



## Pascal


```Pascal
program StrandSortDemo;

type
  TIntArray = array of integer;

function merge(left: TIntArray; right: TIntArray): TIntArray;
  var
    i, j, k: integer;
  begin
    setlength(merge, length(left) + length(right));
    i := low(merge);
    j := low(left);
    k := low(right);
    repeat
      if ((left[j] <= right[k]) and (j <= high(left))) or (k > high(right)) then
      begin
        merge[i] := left[j];
        inc(j);
      end
      else
      begin
        merge[i] := right[k];
        inc(k);
      end;
      inc(i);
    until i > high(merge);
  end;

function StrandSort(s: TIntArray): TIntArray;
  var
    strand: TIntArray;
    i, j: integer;
  begin
    setlength(StrandSort, length(s));
    setlength(strand, length(s));
    i := low(s);
    repeat
      StrandSort[i] := s[i];
      inc(i);
    until (s[i] < s[i-1]);
    setlength(StrandSort, i);
    repeat
      setlength(strand, 1);
      j := low(strand);
      strand[j] := s[i];
      while (s[i+1] > s[i]) and (i < high(s)) do
      begin
        inc(i);
        inc(j);
	setlength(strand, length(strand) + 1);
        Strand[j] := s[i];
      end;
      StrandSort := merge(StrandSort, strand);
      inc(i);
    until (i > high(s));
  end;

var
  data: TIntArray;
  i: integer;

begin
  setlength(data, 8);
  Randomize;
  writeln('The data before sorting:');
  for i := low(data) to high(data) do
  begin
    data[i] := Random(high(data));
    write(data[i]:4);
  end;
  writeln;
  data := StrandSort(data);
  writeln('The data after sorting:');
  for i := low(data) to high(data) do
  begin
    write(data[i]:4);
  end;
  writeln;
end.
```



## Perl


```Perl
use 5.10.0;    # for given/when
sub merge {
        my ($x, $y) = @_;
        my @out;
        while (@$x and @$y) {
                given ($x->[-1] <=> $y->[-1]) {
                        when( 1) { unshift @out, pop @$x }
                        when(-1) { unshift @out, pop @$y }
                        default  { splice @out, 0, 0, pop(@$x), pop(@$y) }
                }
        }
        return @$x, @$y, @out
}

sub strand {
        my $x = shift;
        my @out = shift @$x // return;
        if (@$x) {
                for (-@$x .. -1) {
                        if ($x->[$_] >= $out[-1]) {
                                push @out, splice @$x, $_, 1
                        }
                }
        }
        return @out
}

sub strand_sort {
        my @x = @_;
        my @out;
        while (my @strand = strand(\@x)) {
                @out = merge(\@out, \@strand)
        }
        @out
}

my @a = map (int rand(100), 1 .. 10);
say "Before @a";
@a = strand_sort(@a);
say "After  @a";
```



## Perl 6

```perl6>sub infix:<M
 (@x-in, @y-in) {
    my @x = | @x-in;
    my @y = | @y-in;
    flat @x, @y,
        reverse gather while @x and @y {
            take do given @x[*-1] cmp @y[*-1] {
                when More { pop @x }
                when Less { pop @y }
                when Same { pop(@x), pop(@y) }
            }
        }
}

sub strand (@x) {
    my $i = 0;
    my $prev = -Inf;
    gather while $i < @x {
        @x[$i] before $prev ?? $i++ !! take $prev = splice(@x, $i, 1)[0];
    }
}

sub strand_sort (@x is copy) {
    my @out;
    @out M= strand(@x) while @x;
    @out;
}

my @a = (^100).roll(10);
say "Before {@a}";
@a = strand_sort(@a);
say "After  {@a}";

@a = <The quick brown fox jumps over the lazy dog>;
say "Before {@a}";
@a = strand_sort(@a);
say "After  {@a}";
```

```txt
Before 1 20 64 72 48 75 96 55 42 74
After  1 20 42 48 55 64 72 74 75 96
Before The quick brown fox jumps over the lazy dog
After  The brown dog fox jumps lazy over quick the
```



## Phix

Copy of [[Sorting_algorithms/Strand_sort#Euphoria|Euphoria]]

```Phix
function merge(sequence left, sequence right)
sequence result = {}
    while length(left)>0 and length(right)>0 do
        if left[$]<=right[1] then
            exit
        elsif right[$]<=left[1] then
            return result & right & left
        elsif left[1]<right[1] then
            result = append(result,left[1])
            left = left[2..$]
        else
            result = append(result,right[1])
            right = right[2..$]
        end if
    end while
    return result & left & right
end function

function strand_sort(sequence s)
integer j
sequence result = {}
    while length(s)>0 do
        j = length(s)
        for i=1 to length(s)-1 do
            if s[i]>s[i+1] then
                j = i
                exit
            end if
        end for
        result = merge(result,s[1..j])
        s = s[j+1..$]
    end while
    return result
end function
```



## PHP

```php
$lst = new SplDoublyLinkedList();
foreach (array(1,20,64,72,48,75,96,55,42,74) as $v)
    $lst->push($v);
foreach (strandSort($lst) as $v)
    echo "$v ";

function strandSort(SplDoublyLinkedList $lst) {
    $result = new SplDoublyLinkedList();
    while (!$lst->isEmpty()) {
        $sorted = new SplDoublyLinkedList();
        $remain = new SplDoublyLinkedList();
        $sorted->push($lst->shift());
        foreach ($lst as $item) {
            if ($sorted->top() <= $item) {
                $sorted->push($item);
            } else {
                $remain->push($item);
            }
        }
        $result = _merge($sorted, $result);
        $lst = $remain;
    }
    return $result;
}

function _merge(SplDoublyLinkedList $left, SplDoublyLinkedList $right) {
    $res = new SplDoublyLinkedList();
    while (!$left->isEmpty() && !$right->isEmpty()) {
        if ($left->bottom() <= $right->bottom()) {
            $res->push($left->shift());
        } else {
            $res->push($right->shift());
        }
    }
    foreach ($left as $v)  $res->push($v);
    foreach ($right as $v) $res->push($v);
    return $res;
}
```


```txt
1 20 42 48 55 64 72 74 75 96
```



## PicoLisp


```PicoLisp
(de strandSort (Lst)
   (let Res NIL  # Result list
      (while Lst
         (let Sub (circ (car Lst))  # Build sublist as fifo
            (setq
               Lst (filter
                  '((X)
                     (or
                        (> (car Sub) X)
                        (nil (fifo 'Sub X)) ) )
                  (cdr Lst) )
               Res (make
                  (while (or Res Sub)  # Merge
                     (link
                        (if2 Res Sub
                           (if (>= (car Res) (cadr Sub))
                              (fifo 'Sub)
                              (pop 'Res) )
                           (pop 'Res)
                           (fifo 'Sub) ) ) ) ) ) ) )
      Res ) )
```

Test:

```txt
: (strandSort (3 1 5 4 2))
-> (1 2 3 4 5)

: (strandSort (3 abc 1 (d e f) 5 T 4 NIL 2))
-> (NIL 1 2 3 4 5 abc (d e f) T)
```



## PL/I


```PL/I
strand: procedure options (main); /* 27 Oct. 2012 */
   declare A(100) fixed, used(100) bit (1), sorted fixed controlled;
   declare (temp, work) fixed controlled;
   declare (i, j, k, n) fixed binary;

   n = hbound(A, 1);
   used = '1'b;
   A = random()*99;

   put edit (A) (f(3));

   do while (allocation(sorted) < n);
      call fetch (A, work);
      call move  (temp, work);

      call merge(sorted, temp);
         /* Merges elements in SORTED with elements in TEMP. */
   end;
   /* Transfer the sorted elements to A. */
   do i = 1 to allocation(sorted);
      A(i) = sorted; free sorted;
   end;
   /* Print the sorted values. */
   put skip list ('The sorted values are:');
   put skip edit (A) (f(3));

/* Merges elements of SORTED with elements of TEMP and places  */
/* the result in SORTED. */
/* Elements in SORTED and TEMP are in forward order. */
merge: procedure (sorted, temp);
   declare (sorted, temp) fixed controlled;
   declare work fixed controlled;
   declare (j_ok, k_ok) bit (1);

   do until ((k_ok | j_ok) = '0'b);
      k_ok = allocation(sorted) > 0;
      j_ok = allocation(temp)   > 0;
      if k_ok & j_ok then
         do;
            if sorted <= temp then
               do; allocate work; work = sorted; free sorted; end;
            else
               do; allocate work; work = temp; free temp; end;
         end;
      else
         if allocation(temp) = 0 then
             /* temp is empty; copy remainder of sorted into work */
            do while (allocation(sorted) > 0);
               allocate work; work = sorted; free sorted;
            end;
         else
            /* sorted is empty; copy remainder of temp onto work */
            do while (allocation(temp) > 0);
               allocate work; work = temp; free temp;
            end;
   end;

   call move (sorted, work); /* Move the values to SORTED. */

end merge;

/* Collect a thread of ascending values from aray A, and stack them in temp. */
/* Note: the values in temp are in reverse order. */
fetch: procedure (A, temp);
   declare A(*) fixed, temp controlled fixed;
   declare i fixed binary;

   do i = 1 to hbound(A,1);
      if used(i) then
         do; allocate temp; temp = A(i); used(i) = '0'b; go to found; end;
   end;
found:
   do i = i+1 to hbound(A,1);
      if (temp <= A(i)) & used(i) then
         do; allocate temp; temp = A(i); used(i) = '0'b; end;
   end;
end fetch;

/* Copy the stack at TEMP to the stack at SORTED. */
/* In TEMP, elements are in reverse order;   */
/* in SORTED, elements are in forward order. */
move: procedure (sorted, temp);
   declare (sorted, temp) fixed controlled;

   do while (allocation(sorted) > 0); free sorted; end;
   do while (allocation (temp) > 0);
      allocate sorted; sorted = temp; free temp;
   end;
end move;

end strand;
```

Generated data:

```txt

 43  5 79 16 90 48 29 73 29 19 77 59 49  2 54 35 39 71 25 76 34 48 31 91 28 13 23 70 27 59 96  7 63 82 59 81 28 96 34 43
 81 98 21 47 72 57 45 64 94 51 18 11 65 12 61 97 13 84 95 89 43  8 14 31 58 68 58 39 59 26 72 38 26 85 30 89 42 90 29 11
 14 63 97 60  1 17 45 42 62 29 45 15 69 11 29 25 11 48 92  3

```

Results:

```txt

The sorted values are:
  1  2  3  5  7  8 11 11 11 11 12 13 13 14 14 15 16 17 18 19 21 23 25 25 26 26 27 28 28 29 29 29 29 29 30 31 31 34 34 35
 38 39 39 42 42 43 43 43 45 45 45 47 48 48 48 49 51 54 57 58 58 59 59 59 59 60 61 62 63 63 64 65 68 69 70 71 72 72 73 76
 77 79 81 81 82 84 85 89 89 90 90 91 92 94 95 96 96 97 97 98

```



## PureBasic


```PureBasic
Procedure strandSort(List a())
  Protected NewList subList()
  Protected NewList results()

  While ListSize(a()) > 0
    ClearList(subList())
    AddElement(subList())
    FirstElement(a())
    subList() = a()
    DeleteElement(a())
    ForEach a()
      If a() >= subList()
        AddElement(subList())
        subList() = a()
        DeleteElement(a())
      EndIf
    Next

    ;merge lists
    FirstElement(subList())
    If Not FirstElement(results())
      ;copy all of sublist() to results()
      MergeLists(subList(), results(), #PB_List_Last)
    Else
      Repeat
        If subList() < results()
          InsertElement(results())
          results() = subList()
          DeleteElement(subList())
          If Not NextElement(subList())
            Break
          EndIf
        ElseIf Not NextElement(results())
          ;add remainder of sublist() to end of results()
          MergeLists(subList(), results(), #PB_List_Last)
          Break
        EndIf
      ForEver
    EndIf

  Wend
  CopyList(results(), a())
EndProcedure

Procedure.s listContents(List a())
  Protected output.s
  PushListPosition(a())
  ForEach a()
    output + Str(a()) + ","
  Next
  PopListPosition(a())
  ProcedureReturn Left(output, Len(output) - 1)
EndProcedure

Procedure setupList(List a())
  ClearList(a())
  Protected elementCount, i

  elementCount = Random(5) + 10
  For i = 1 To elementCount
    AddElement(a())
    a() = Random(10) - 5
  Next
EndProcedure


If OpenConsole()
  NewList sample()
  Define i

  For i = 1 To 3
    setupList(sample())
    PrintN("List " + Str(i) + ":")
    PrintN("  Before:  " + listContents(sample()))
    strandSort(sample())
    PrintN("  After :  " + listContents(sample()))
    PrintN("")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
List 1:
  Before:  3,-2,-4,4,-1,-3,-2,-2,2,2,0
  After :  -4,-3,-2,-2,-2,-1,0,2,2,3,4

List 2:
  Before:  -4,4,3,-2,3,-2,5,0,-1,0,5,1
  After :  -4,-2,-2,-1,0,0,1,3,3,4,5,5

List 3:
  Before:  -2,0,-2,5,5,3,-1,-3,5,5,0,2,-4,4,2
  After :  -4,-3,-2,-2,-1,0,0,2,2,3,4,5,5,5,5
```



## Python


```Python
def merge_list(a, b):
	out = []
	while len(a) and len(b):
		if a[0] < b[0]:
			out.append(a.pop(0))
		else:
			out.append(b.pop(0))
	out += a
	out += b
	return out

def strand(a):
	i, s = 0, [a.pop(0)]
	while i < len(a):
		if a[i] > s[-1]:
			s.append(a.pop(i))
		else:
			i += 1
	return s

def strand_sort(a):
	out = strand(a)
	while len(a):
		out = merge_list(out, strand(a))
	return out

print strand_sort([1, 6, 3, 2, 1, 7, 5, 3])
```

Output:<lang>[1, 1, 2, 3, 3, 5, 6, 7]
```



## Racket


```racket

#lang racket
(require mzlib/list)
(define (merge xs ys) (merge-sorted-lists xs ys <=))

(define (strand-sort xs)
  (let loop ([xs xs] [ys '[]])
    (cond [(empty? xs) ys]
          [else (define-values (sorted unsorted) (extract-strand xs))
                (loop unsorted (merge sorted ys))])))

(define (extract-strand xs)
  (for/fold ([strand '()] [unsorted '[]]) ([x xs])
    (if (or (empty? strand) (< x (first strand)))
        (values (cons x strand) unsorted)
        (values strand (cons x unsorted)))))

(strand-sort (build-list 10 (λ(_) (random 15))))

```



## REXX

This REXX program was written to generate a specified amount of random numbers as
well as allowing a pre-pended list of numbers).

It can handle integers, floating point numbers, exponentiated numbers, and/or character strings.

```rexx
/*REXX program sorts a random list of words (or numbers) using the strand sort algorithm*/
parse arg size minv maxv old                     /*obtain optional arguments from the CL*/
if size=='' | size==","  then size=20            /*Not specified?  Then use the default.*/
if minv=='' | minv==","  then minv= 0            /*Not specified?  Then use the default.*/
if maxv=='' | maxv==","  then maxv=size          /*Not specified?  Then use the default.*/
               do i=1  for size                  /*generate a list of random numbers.   */
               old=old  random(0,maxv-minv)+minv /*append a random number to a list.    */
               end  /*i*/
old=space(old)                                   /*elide extraneous blanks from the list*/
          say center('unsorted list', length(old), "─");         say old
new=strand_sort(old)                             /*sort the list of the random numbers. */
say;      say center('sorted list'  , length(new), "─");         say new
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
strand_sort: procedure; parse arg x;                       y=
                          do  while words(x)\==0;          w=words(x)
                                 do j=1  for w-1                /*anything out of order?*/
                                 if word(x,j)>word(x,j+1)  then do;  w=j;  leave;  end
                                 end   /*j*/
                          y=merge(y,subword(x,1,w));       x=subword(x,w+1)
                          end            /*while*/
             return y
/*──────────────────────────────────────────────────────────────────────────────────────*/
merge:       procedure; parse arg a.1,a.2;    p=
                   do forever;  w1=words(a.1);  w2=words(a.2)   /*do while 2 lists exist*/
                   if w1==0 | if w2==0              then leave  /*Any list empty?  Stop.*/
                   if word(a.1,w1)  <= word(a.2,1)  then leave  /*lists are now sorted? */
                   if word(a.2,w2)  <= word(a.1,1)  then return space(p a.2 a.1)
                   #=1+(word(a.1,1) >= word(a.2,1));  p=p word(a.#,1);  a.#=subword(a.#,2)
                   end   /*forever*/
             return space(p a.1 a.2)
```

'''output'''   when using the input of:   <tt> 25 -9 30 1000 2000 3000 </tt>

```txt

────────────────────────────────unsorted list────────────────────────────────
1000 2000 3000 9 0 3 -8 17 8 -2 4 0 -3 19 -1 3 1 8 27 14 20 2 -6 23 1 -8 -4 4

─────────────────────────────────sorted list─────────────────────────────────
-8 -8 -6 -4 -3 -2 -1 0 0 1 1 2 3 3 4 4 8 8 9 14 17 19 20 23 27 1000 2000 3000

```

The REXX program can also sort words as well as numbers.


'''output'''   when using the input of:   <tt> 24 -9 100 66 66 8.8 carp Carp </tt>

```txt

──────────────────────────────────────unsorted list───────────────────────────────────────
66 66 8.8 carp Carp 20 77 88 9 39 -5 10 12 80 87 26 61 87 94 73 27 49 35 95 81 76 40 13 72

───────────────────────────────────────sorted list────────────────────────────────────────
-5 8.8 9 10 12 13 20 26 27 35 39 40 49 61 66 66 72 73 76 77 80 81 87 87 88 94 95 Carp carp

```

Note that an   ASCII   computer will sort words differently than an   EBCDIC   machine.


The order of sorting on an   ASCII   machine is:   <tt> numbers, upperCase, lowerCase </tt>

The order of sorting on an EBCDIC machine is:   <tt> lowerCase, upperCase, numbers </tt>





## Ring


```ring

# Project : Sorting algorithms/Strand sort

test = [-2,0,-2,5,5,3,-1,-3,5,5,0,2,-4,4,2]
results = []
resultsend = []
see "before sort:" + nl
showarray(test)
test = strandsort(test)
see "after sort:" + nl
showarray(test)

func strandsort(a)
        while len(a) > 0
                 sublist = []
                 add(sublist,a[1])
                 del(a,1)
                 for i = 1 to len(a)
                     if a[i] > sublist[len(sublist)]
                       add(sublist,a[i])
                       del(a,i)
                     ok
                next
                for n = 1 to len(sublist)
                     add(results,sublist[n])
                next
                for n = 1 to len(results)
                     for m = n + 1 to len(results)
                          if results[m] < results[n]
                             temp = results[m]
                             results[m] = results[n]
                             results[n] = temp
                          ok
                     next
                next
        end
        return results

func showarray(vect)
        svect = ""
        for n = 1 to len(vect)
              svect = svect + vect[n] + " "
        next
        svect = left(svect, len(svect) - 1)
        see svect + nl

```

Output:

```txt

before sort:
-2 0 -2 5 5 3 -1 -3 5 5 0 2 -4 4 2
after sort:
-4 -3 -2 -2 -1 0 0 2 2 3 4 5 5 5 5

```



## Ruby


```ruby
class Array
  def strandsort
    a = dup
    result = []
    until a.empty?
      v = a.first
      sublist, a = a.partition{|val| v=val if v<=val}   # In case of v>val, it becomes nil.

      result.each_index do |idx|
        break if sublist.empty?
        result.insert(idx, sublist.shift) if sublist.first < result[idx]
      end
      result += sublist
    end
    result
  end

  def strandsort!
    replace(strandsort)
  end
end

p [1, 6, 3, 2, 1, 7, 5, 3].strandsort
```


```txt
[1, 1, 2, 3, 3, 5, 6, 7]
```



## Sidef

```ruby
func merge(x, y) {
    var out = [];
    while (x && y) {
        given (x[-1] <=> y[-1]) {
            when ( 1) { out.prepend(x.pop) }
            when (-1) { out.prepend(y.pop) }
            default   { out.prepend(x.pop, y.pop) }
        }
    }
    x + y + out;
}

func strand(x) {
    x || return [];
    var out = [x.shift];
    if (x.len) {
        for i in (-x.len .. -1) {
            if (x[i] >= out[-1]) {
                out.append(x.pop_at(i));
            }
        }
    }
    return out;
}

func strand_sort(x) {
    var out = [];
    while (var strd = strand(x)) {
        out = merge(out, strd);
    }
    return out;
}

var a = 10.of { 100.irand };
say "Before: #{a}";
say "After: #{strand_sort(a)}";
```


```txt

Before: 24 62 29 95 11 21 46 3 23 20
After: 3 11 20 21 23 24 29 46 62 95

```



## Tcl


```tcl
proc merge {listVar toMerge} {
    upvar 1 $listVar v
    set i [set j 0]
    set out {}
    while {$i<[llength $v] && $j<[llength $toMerge]} {
	if {[set a [lindex $v $i]] < [set b [lindex $toMerge $j]]} {
	    lappend out $a
	    incr i
	} else {
	    lappend out $b
	    incr j
	}
    }
    # Done the merge, but will be one source with something left
    # This will handle all that by doing a merge of the remnants onto the end
    set v [concat $out [lrange $v $i end] [lrange $toMerge $j end]]
    return
}

proc strandSort A {
    set results {}
    while {[llength $A]} {
	set sublist [lrange $A 0 0]
	# We build a list of items that weren't filtered rather than removing "in place"
	# because this fits better with the way Tcl values work (the underlying data
	# structure is an array, not a linked list).
	set newA {}
	foreach a [lrange $A 1 end] {
	    if {$a > [lindex $sublist end]} {
		lappend sublist $a
	    } else {
		lappend newA $a
	    }
	}
	set A $newA
	merge results $sublist
    }
    return $results
}

puts [strandSort {3 1 5 4 2}]
```



## Ursala



```Ursala
strand_sort "r" =  # parameterized by a relational predicate "r"

@NiX -+
   :-0 ~&B^?a\~&Y@a "r"?abh/~&alh2faltPrXPRC ~&arh2falrtPXPRC,
   ~&r->l ^|rlPlCrrPX/~& @hNCNXtX ~&r->lbx "r"?rllPXh/~&llPrhPlrPCXrtPX ~&rhPllPClrPXrtPX+-
```

demonstration code:
```Ursala
#cast %nL

x = (strand_sort nat-nleq) <3,1,5,4,2>
```
output:
```txt
<1,2,3,4,5>
```



## zkl


```zkl
fcn strandSort(A){ //--> new list, A is cleared, should add A=A.copy()
   sublist:=List.createLong(A.len()); results:=List.createLong(A.len());
   while(A){
      sublist.clear(A.pop(0));
      foreach i in (A.len() - 1){
	 if(A[i]>sublist[-1]) sublist.append(A.pop(i));
      }
      results.merge(sublist);
   }
   results
}
```

The createLong list method creates a new list with pre-allocated space

```zkl
strandSort(L(3,1,5,4,2)).println();
strandSort("azbfe".split("")).println();
```

```txt

L(1,2,3,4,5)
L("a","b","e","f","z")

```


