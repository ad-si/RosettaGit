+++
title = "Longest increasing subsequence"
description = ""
date = 2019-09-03T07:14:52Z
aliases = []
[extra]
id = 15922
[taxonomies]
categories = []
tags = []
+++

{{task}}
Calculate and show here a [[wp:Longest increasing subsequence|longest increasing subsequence]] of the list:
:<math>\{3, 2, 6, 4, 5, 1\}</math>
And of the list:
:<math>\{0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15\}</math>

Note that a list may have more than one subsequence that is of the maximum length.

;Ref:
# [http://www.youtube.com/watch?v=4fQJGoeW5VE Dynamic Programming #1: Longest Increasing Subsequence] on YouTube
# An efficient solution can be based on [[wp:Patience sorting|Patience sorting]].





## 360 Assembly

{{trans|VBScript}}

```360asm
*        Longest increasing subsequence    04/03/2017
LNGINSQ  CSECT
         USING  LNGINSQ,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,1             i=1
       DO WHILE=(CH,R6,LE,=H'2') do i=1 to 2
       IF CH,R6,EQ,=H'1' THEN      if i=1 then
         MVC    N,=AL2((A2-A1)/2)    n=hbound(a1)
         MVC    AA(64),A1            a=a1
       ELSE     ,                  else
         MVC    N,=AL2((AA-A2)/2)    n=hbound(a2)
         MVC    AA(64),A2            a=a2
       ENDIF    ,                  endif
         MVC    PG,=CL80': '       init buffer
         LA     R2,AA-2            @a
         LH     R3,N               n
         BAL    R14,PRINT          print a
         MVC    LL,=H'0'           l=0
         SR     R7,R7              j=0
       DO WHILE=(CH,R7,LE,N)       do j=0 to n
         MVC    LO,=H'1'             lo=1
         MVC    HI,LL                hi=l
         LH     R4,LO                lo
       DO WHILE=(CH,R4,LE,HI)        do while lo<=hi
         LH     R1,LO                  lo
         AH     R1,HI                  lo+hi
         SRA    R1,1                   /2
         STH    R1,MIDDLE              middle=(lo+hi)/2
         SLA    R1,1                   *2
         LH     R1,MM(R1)              m(middle+1)
         SLA    R1,1                   *2
         LH     R3,AA(R1)              r3=a(m(middle+1)+1)
         LR     R1,R7                  j
         SLA    R1,1                   *2
         LH     R4,AA(R1)              r4=a(j+1)
         LH     R2,MIDDLE              middle
       IF CR,R3,LT,R4 THEN             if a(m(middle+1)+1)<a(j+1) then
         LA     R2,1(R2)                 middle+1
         STH    R2,LO                    lo=middle+1
       ELSE     ,                      else
         BCTR   R2,0                     middle-1
         STH    R2,HI                    hi=middle-1
       ENDIF    ,                      endif
         LH     R4,LO                  lo
       ENDDO    ,                    end /*while*/
         LH     R10,LO               newl=lo
         LR     R1,R10               newl
         SLA    R1,1                 *2
         LH     R3,MM-2(R1)          m(newl)
         LR     R1,R7                j
         SLA    R1,1                 *2
         STH    R3,PP(R1)            p(j+1)=m(newl)
         LR     R1,R10               newl
         SLA    R1,1                 *2
         STH    R7,MM(R1)            m(newl+1)=j
       IF CH,R10,GT,LL               if newl>l then
         STH    R10,LL                 l=newl
       ENDIF    ,                    endif
         LA     R7,1(R7)             j++
       ENDDO    ,                  enddo j
         LH     R1,LL              l
         SLA    R1,1               *2
         LH     R10,MM(R1)         k=m(l+1)
         LH     R7,LL              j=l
       DO WHILE=(CH,R7,GE,=H'1')   do j=l to 1 by -1
         LR     R1,R10               k
         SLA    R1,1                 *2
         LH     R2,AA(R1)            a(k+1)
         LR     R1,R7                j
         SLA    R1,1                 *2
         STH    R2,SS-2(R1)          s(j)=a(k+1)
         LR     R1,R10               k
         SLA    R1,1                 *2
         LH     R10,PP(R1)           k=p(k+1)
         BCTR   R7,0                 j--
       ENDDO    ,                  enddo j
         MVC    PG,=CL80'> '       init buffer
         LA     R2,SS-2            @s
         LH     R3,LL              l
         BAL    R14,PRINT          print a
         LA     R6,1(R6)           i++
       ENDDO    ,                enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
PRINT    LA     R10,PG        ---- print subroutine
         LA     R10,2(R10)         pgi=2
         LA     R7,1               j=1
       DO WHILE=(CR,R7,LE,R3)      do j=1 to nx
         LR     R1,R7                j
         SLA    R1,1                 *2
         LH     R1,0(R2,R1)          x(j)
         XDECO  R1,XDEC              edit x(j)
         MVC    0(3,R10),XDEC+9      output x(j)
         LA     R10,3(R10)           pgi+=3
         LA     R7,1(R7)             j++
       ENDDO    ,                  enddo j
         XPRNT  PG,L'PG            print buffer
         BR     R14           ---- return
A1       DC     H'3',H'2',H'6',H'4',H'5',H'1'
A2       DC     H'0',H'8',H'4',H'12',H'2',H'10',H'6',H'14'
         DC     H'1',H'9',H'5',H'13',H'3',H'11',H'7',H'15'
AA       DS     32H                a(32)
PP       DS     32H                p(32)
MM       DS     32H                m(32)
SS       DS     32H                s(32)
N        DS     H                  n
LL       DS     H                  l
LO       DS     H                  lo
HI       DS     H                  hi
MIDDLE   DS     H                  middle
PG       DS     CL80               buffer
XDEC     DS     CL12               temp for xdeco
         YREGS
         END    LNGINSQ
```

{{out}}

```txt

:   3  2  6  4  5  1
>   2  4  5
:   0  8  4 12  2 10  6 14  1  9  5 13  3 11  7 15
>   0  2  6  9 11 15

```



## AutoHotkey


```AutoHotkey
Lists := [[3,2,6,4,5,1], [0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15]]

for k, v in Lists {
	D := LIS(v)
	MsgBox, % D[D.I].seq
}

LIS(L) {
	D := []
	for i, v in L {
		D[i, "Length"] := 1, D[i, "Seq"] := v, D[i, "Val"] := v
		Loop, % i - 1 {
			if(D[A_Index].Val < v && D[A_Index].Length + 1 > D[i].Length) {
				D[i].Length := D[A_Index].Length + 1
				D[i].Seq := D[A_Index].Seq ", " v
				if (D[i].Length > MaxLength)
					MaxLength := D[i].Length, D.I := i
			}
		}
	}
	return, D
}
```

'''Output:'''

```txt
3, 4, 5
0, 4, 6, 9, 13, 15
```



## C

Using an array that doubles as linked list (more like reversed trees really). O(n) memory and O(n<sup>2</sup>) runtime.

```c
#include <stdio.h>
#include <stdlib.h>

struct node {
	int val, len;
	struct node *next;
};

void lis(int *v, int len)
{
	int i;
	struct node *p, *n = calloc(len, sizeof *n);
	for (i = 0; i < len; i++)
		n[i].val = v[i];

	for (i = len; i--; ) {
		// find longest chain that can follow n[i]
		for (p = n + i; p++ < n + len; ) {
			if (p->val > n[i].val && p->len >= n[i].len) {
				n[i].next = p;
				n[i].len = p->len + 1;
			}
		}
	}

	// find longest chain
	for (i = 0, p = n; i < len; i++)
		if (n[i].len > p->len) p = n + i;

	do printf(" %d", p->val); while ((p = p->next));
	putchar('\n');

	free(n);
}

int main(void)
{
	int x[] = { 3, 2, 6, 4, 5, 1 };
	int y[] = { 0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15 };

	lis(x, sizeof(x) / sizeof(int));
	lis(y, sizeof(y) / sizeof(int));
	return 0;
}
```

{{out}}

```txt

 3 4 5
 0 4 6 9 13 15

```



## C++

Patience sorting

```cpp
#include <iostream>
#include <vector>
#include <tr1/memory>
#include <algorithm>
#include <iterator>

template <typename E>
struct Node {
  E value;
  std::tr1::shared_ptr<Node<E> > pointer;
};

template <class E>
struct node_ptr_less {
  bool operator()(const std::tr1::shared_ptr<Node<E> > &node1,
		  const std::tr1::shared_ptr<Node<E> > &node2) const {
    return node1->value < node2->value;
  }
};


template <typename E>
std::vector<E> lis(const std::vector<E> &n) {
  typedef std::tr1::shared_ptr<Node<E> > NodePtr;

  std::vector<NodePtr> pileTops;
  // sort into piles
  for (typename std::vector<E>::const_iterator it = n.begin(); it != n.end(); it++) {
    NodePtr node(new Node<E>());
    node->value = *it;
    typename std::vector<NodePtr>::iterator j =
      std::lower_bound(pileTops.begin(), pileTops.end(), node, node_ptr_less<E>());
    if (j != pileTops.begin())
      node->pointer = *(j-1);
    if (j != pileTops.end())
      *j = node;
    else
      pileTops.push_back(node);
  }
  // extract LIS from piles
  std::vector<E> result;
  for (NodePtr node = pileTops.back(); node != NULL; node = node->pointer)
    result.push_back(node->value);
  std::reverse(result.begin(), result.end());
  return result;
}

int main() {
  int arr1[] = {3,2,6,4,5,1};
  std::vector<int> vec1(arr1, arr1 + sizeof(arr1)/sizeof(*arr1));
  std::vector<int> result1 = lis(vec1);
  std::copy(result1.begin(), result1.end(), std::ostream_iterator<int>(std::cout, ", "));
  std::cout << std::endl;

  int arr2[] = {0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15};
  std::vector<int> vec2(arr2, arr2 + sizeof(arr2)/sizeof(*arr2));
  std::vector<int> result2 = lis(vec2);
  std::copy(result2.begin(), result2.end(), std::ostream_iterator<int>(std::cout, ", "));
  std::cout << std::endl;
  return 0;
}
```


{{out}}

```txt
2, 4, 5,
0, 2, 6, 9, 11, 15,
```



## C#


### Recursive

{{works with|C sharp|6}}

```c#
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

public static class LIS
{
    public static IEnumerable<T> FindRec<T>(IList<T> values, IComparer<T> comparer = null) =>
        values == null ? throw new ArgumentNullException() :
            FindRecImpl(values, Sequence<T>.Empty, 0, comparer ?? Comparer<T>.Default).Reverse();

    private static Sequence<T> FindRecImpl<T>(IList<T> values, Sequence<T> current, int index, IComparer<T> comparer) {
        if (index == values.Count) return current;
        if (current.Length > 0 && comparer.Compare(values[index], current.Value) <= 0)
            return FindRecImpl(values, current, index + 1, comparer);
        return Max(
            FindRecImpl(values, current, index + 1, comparer),
            FindRecImpl(values, current + values[index], index + 1, comparer)
        );
    }

    private static Sequence<T> Max<T>(Sequence<T> a, Sequence<T> b) => a.Length < b.Length ? b : a;

    class Sequence<T> : IEnumerable<T>
    {
        public static readonly Sequence<T> Empty = new Sequence<T>(default(T), null);

        public Sequence(T value, Sequence<T> tail)
        {
            Value = value;
            Tail = tail;
            Length = tail == null ? 0 : tail.Length + 1;
        }

        public T Value { get; }
        public Sequence<T> Tail { get; }
        public int Length { get; }

        public static Sequence<T> operator +(Sequence<T> s, T value) => new Sequence<T>(value, s);

        public IEnumerator<T> GetEnumerator()
        {
            for (var s = this; s.Length > 0; s = s.Tail) yield return s.Value;
        }

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    }
}
```


### Patience sorting

{{works with|C sharp|7}}

```c#
public static class LIS
{
    public static T[] Find<T>(IList<T> values, IComparer<T> comparer = null) {
        if (values == null) throw new ArgumentNullException();
        if (comparer == null) comparer = Comparer<T>.Default;
        var pileTops = new List<T>();
        var pileAssignments = new int[values.Count];
        for (int i = 0; i < values.Count; i++) {
            T element = values[i];
            int pile = pileTops.BinarySearch(element, comparer);
            if (pile < 0) pile = ~pile;
            if (pile == pileTops.Count) pileTops.Add(element);
            else pileTops[pile] = element;
            pileAssignments[i] = pile;
        }
        T[] result = new T[pileTops.Count];
        for (int i = pileAssignments.Length - 1, p = pileTops.Count - 1; p >= 0; i--) {
            if (pileAssignments[i] == p) result[p--] = values[i];
        }
        return result;
    }
}
```



## Clojure

Implementation using the Patience Sort approach.
The elements (''newelem'') put on a pile combine the "card" with a reference to the top of the previous stack, as per the algorithm.
The combination is done using ''cons'', so what gets put on a pile is a list -- a descending subsequence.


```Clojure
(defn place [piles card]
  (let [[les gts] (->> piles (split-with #(<= (ffirst %) card)))
        newelem (cons card (->> les last first))
        modpile (cons newelem (first gts))]
    (concat les (cons modpile (rest gts)))))

(defn a-longest [cards]
  (let [piles (reduce place '() cards)]
    (->> piles last first reverse)))

(println (a-longest [3 2 6 4 5 1]))
(println (a-longest [0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15]))
```

{{out}}
<lang>(2 4 5)
(0 2 6 9 11 15)
```



## Common Lisp


### Common Lisp: Using the method in the video

Slower and more memory usage compared to the patience sort method.

```lisp
(defun longest-increasing-subseq (list)
  (let ((subseqs nil))
    (dolist (item list)
      (let ((longest-so-far (longest-list-in-lists (remove-if-not #'(lambda (l) (> item (car l))) subseqs))))
	(push (cons item longest-so-far) subseqs)))
    (reverse (longest-list-in-lists subseqs))))

(defun longest-list-in-lists (lists)
  (let ((longest nil)
	(longest-len 0))
    (dolist (list lists)
      (let ((len (length list)))
	(when (> len longest-len)
	  (setf longest list
		longest-len len))))
    longest))

(dolist (l (list (list 3 2 6 4 5 1)
		 (list 0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15)))
  (format t "~A~%" (longest-increasing-subseq l))))
```

{{out}}

```txt
(2 4 5)
(0 2 6 9 11 15)
```


### Common Lisp: Using the Patience Sort approach

This is 5 times faster and and uses a third of the memory compared to the approach in the video.

```lisp
(defun lis-patience-sort (input-list)
  (let ((piles nil))
    (dolist (item input-list)
      (setf piles (insert-item item piles)))
    (reverse (caar (last piles)))))

(defun insert-item (item piles)
  (let ((not-found t))
    (loop
       while not-found
       for pile in piles
       and prev = nil then pile
       and i from 0
       do (when (<= item (caar pile))
	    (setf (elt piles i) (push (cons item (car prev)) (elt piles i))
		  not-found nil)))
    (if not-found
	(append piles (list (list (cons item (caar (last piles))))))
 	piles)))

(dolist (l (list (list 3 2 6 4 5 1)
		   (list 0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15)))
    (format t "~A~%" (lis-patience-sort l)))
```

{{out}}

```txt
(2 4 5)
(0 2 6 9 11 15)
```

===Common Lisp: Using the Patience Sort approach (alternative)===
This is a different version of the code above.

```lisp
(defun insert-item (item piles)
  (multiple-value-bind
	(i prev)
      (do* ((prev nil (car x))
	    (x piles (cdr x))
	    (i 0 (1+ i)))
	   ((or (null x) (<= item (caaar x))) (values i prev)))
    (if (= i (length piles))
	(append piles (list (list (cons item (caar (last piles))))))
	(progn (push (cons item (car prev)) (elt piles i))
	       piles))))

(defun longest-inc-seq (input)
  (do* ((piles nil (insert-item (car x) piles))
	(x input (cdr x)))
       ((null x) (reverse (caar (last piles))))))

(dolist (l (list (list 3 2 6 4 5 1)
		   (list 0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15)))
    (format t "~A~%" (longest-inc-seq l)))
```

{{out}}

```txt
(2 4 5)
(0 2 6 9 11 15)
```



## D


### Simple Version

{{trans|Haskell}}
Uses the second powerSet function from the Power Set Task.

```d
import std.stdio, std.algorithm, power_set2;

T[] lis(T)(T[] items) pure nothrow {
    //return items.powerSet.filter!isSorted.max!q{ a.length };
    return items
           .powerSet
           .filter!isSorted
           .minPos!q{ a.length > b.length }
           .front;
}

void main() {
    [3, 2, 6, 4, 5, 1].lis.writeln;
    [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15].lis.writeln;
}
```

{{out}}

```txt
[2, 4, 5]
[0, 2, 6, 9, 11, 15]
```



### Patience sorting

{{trans|Python}}
From the second Python entry, using the Patience sorting method.

```d
import std.stdio, std.algorithm, std.array;

/// Return one of the Longest Increasing Subsequence of
/// items using patience sorting.
T[] lis(T)(in T[] items) pure nothrow
if (__traits(compiles, T.init < T.init))
out(result) {
    assert(result.length <= items.length);
    assert(result.isSorted);
    assert(result.all!(x => items.canFind(x)));
} body {
    if (items.empty)
        return null;

    static struct Node { T val; Node* back; }
    auto pile = [[new Node(items[0])]];

    OUTER: foreach (immutable di; items[1 .. $]) {
        foreach (immutable j, ref pj; pile)
            if (pj[$ - 1].val > di) {
                pj ~= new Node(di, j ? pile[j - 1][$ - 1] : null);
                continue OUTER;
            }
        pile ~= [new Node(di, pile[$ - 1][$ - 1])];
    }

    T[] result;
    for (auto ptr = pile[$ - 1][$ - 1]; ptr != null; ptr = ptr.back)
        result ~= ptr.val;
    result.reverse();
    return result;
}

void main() {
    foreach (d; [[3,2,6,4,5,1],
                 [0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15]])
        d.lis.writeln;
}
```

The output is the same.


### Faster Version

{{trans|Java}}
With some more optimizations.

```d
import std.stdio, std.algorithm, std.range, std.array;

T[] lis(T)(in T[] items) pure nothrow
if (__traits(compiles, T.init < T.init))
out(result) {
    assert(result.length <= items.length);
    assert(result.isSorted);
    assert(result.all!(x => items.canFind(x)));
} body {
    if (items.empty)
        return null;

    static struct Node {
        T value;
        Node* pointer;
    }
    Node*[] pileTops;
    auto nodes = minimallyInitializedArray!(Node[])(items.length);

    // Sort into piles.
    foreach (idx, x; items) {
        auto node = &nodes[idx];
        node.value = x;
        immutable i = pileTops.length -
                      pileTops.assumeSorted!q{a.value < b.value}
                      .upperBound(node)
                      .length;
        if (i != 0)
            node.pointer = pileTops[i - 1];
        if (i != pileTops.length)
            pileTops[i] = node;
        else
            pileTops ~= node;
    }

    // Extract LIS from nodes.
    size_t count = 0;
    for (auto n = pileTops[$ - 1]; n != null; n = n.pointer)
        count++;
    auto result = minimallyInitializedArray!(T[])(count);
    for (auto n = pileTops[$ - 1]; n != null; n = n.pointer)
        result[--count] = n.value;
    return result;
}

void main() {
    foreach (d; [[3,2,6,4,5,1],
                 [0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15]])
        d.writeln;
}
```

The output is the same.

=={{header|Déjà Vu}}==
{{trans|Python}}

```dejavu
in-pair:
	if = :nil dup:
		false drop
	else:
		@in-pair &> swap &< dup

get-last lst:
	get-from lst -- len lst

lis-sub pile i di:
	for j range 0 -- len pile:
		local :pj get-from pile j
		if > &< get-last pj di:
			push-to pj & di if j get-last get-from pile -- j :nil
			return
	push-to pile [ & di get-last get-last pile ]

lis d:
	local :pile [ [ & get-from d 0 :nil ] ]
	for i range 1 -- len d:
		lis-sub pile i get-from d i
	[ for in-pair get-last get-last pile ]

!. lis [ 3 2 6 4 5 1 ]
!. lis [ 0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15 ]

```


{{out}}

```txt
[ 2 4 5 ]
[ 0 2 6 9 11 15 ]
```



## Elixir

{{trans|Erlang}}

### Naive version

very slow

```elixir
defmodule Longest_increasing_subsequence do
  # Naive implementation
  def lis(l) do
    (for ss <- combos(l), ss == Enum.sort(ss), do: ss)
    |> Enum.max_by(fn ss -> length(ss) end)
  end

  defp combos(l) do
    Enum.reduce(1..length(l), [[]], fn k, acc -> acc ++ (combos(k, l)) end)
  end
  defp combos(1, l), do: (for x <- l, do: [x])
  defp combos(k, l) when k == length(l), do: [l]
  defp combos(k, [h|t]) do
    (for subcombos <- combos(k-1, t), do: [h | subcombos]) ++ combos(k, t)
  end
end

IO.inspect Longest_increasing_subsequence.lis([3,2,6,4,5,1])
IO.inspect Longest_increasing_subsequence.lis([0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15])
```


{{out}}

```txt

[3, 4, 5]
[0, 4, 6, 9, 13, 15]

```



### Patience sort version


```elixir
defmodule Longest_increasing_subsequence do
  # Patience sort implementation
  def patience_lis(l), do: patience_lis(l, [])

  defp patience_lis([h | t], []), do: patience_lis(t, [[{h,[]}]])
  defp patience_lis([h | t], stacks), do: patience_lis(t, place_in_stack(h, stacks, []))
  defp patience_lis([], []), do: []
  defp patience_lis([], stacks), do: get_previous(stacks) |> recover_lis |> Enum.reverse

  defp place_in_stack(e, [stack = [{h,_} | _] | tstacks], prevstacks) when h > e do
    prevstacks ++ [[{e, get_previous(prevstacks)} | stack] | tstacks]
  end
  defp place_in_stack(e, [stack | tstacks], prevstacks) do
    place_in_stack(e, tstacks, prevstacks ++ [stack])
  end
  defp place_in_stack(e, [], prevstacks) do
    prevstacks ++ [[{e, get_previous(prevstacks)}]]
  end

  defp get_previous(stack = [_|_]), do: hd(List.last(stack))
  defp get_previous([]), do: []

  defp recover_lis({e, prev}), do: [e | recover_lis(prev)]
  defp recover_lis([]), do: []
end

IO.inspect Longest_increasing_subsequence.patience_lis([3,2,6,4,5,1])
IO.inspect Longest_increasing_subsequence.patience_lis([0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15])
```


{{out}}

```txt

[2, 4, 5]
[0, 2, 6, 9, 11, 15]

```



## Erlang

Both implementations:
- Naive version{{trans|Haskell}}
- Patience sort version.

Function ''combos'' is copied from  [http://panduwana.wordpress.com/2010/04/21/combination-in-erlang/ panduwana blog].

Function ''maxBy'' is copied from  [http://stackoverflow.com/a/4762387/4162959 Hynek -Pichi- Vychodil's answer].


```erlang

-module(longest_increasing_subsequence).

-export([test_naive/0, test_patience/0]).

% **************************************************
% Interface to test the implementation
% **************************************************

test_naive() ->
    test_gen(fun lis/1).

test_patience() ->
    test_gen(fun patience_lis/1).

test_gen(F) ->
    show_result(F([3,2,6,4,5,1])),
    show_result(F([0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15])).

show_result(Res) ->
    io:format("~w\n", [Res]).

% **************************************************

% **************************************************
% Naive implementation
% **************************************************

lis(L) ->
    maxBy(
        fun(SS) -> length(SS) end,
        [ lists:usort(SS)
            ||  SS <- combos(L),
                SS == lists:sort(SS)]
    ).

% **************************************************

% **************************************************
% Patience sort implementation
% **************************************************

patience_lis(L) ->
    patience_lis(L, []).

patience_lis([H | T], Stacks) ->
    NStacks =
        case Stacks of
            [] ->
                [[{H,[]}]];
            _ ->
                place_in_stack(H, Stacks, [])
        end,
    patience_lis(T, NStacks);
patience_lis([], Stacks) ->
    case Stacks of
        [] ->
            [];
        [_|_] ->
            lists:reverse( recover_lis( get_previous(Stacks) ) )
    end.

place_in_stack(E, [Stack = [{H,_} | _] | TStacks], PrevStacks) when H > E ->
    PrevStacks ++ [[{E, get_previous(PrevStacks)} | Stack] | TStacks];
place_in_stack(E, [Stack = [{H,_} | _] | TStacks], PrevStacks) when H =< E ->
    place_in_stack(E, TStacks, PrevStacks ++ [Stack]);
place_in_stack(E, [], PrevStacks)->
    PrevStacks ++ [[{E, get_previous(PrevStacks)}]].

get_previous(Stack = [_|_]) ->
    hd(lists:last(Stack));
get_previous([]) ->
    [].

recover_lis({E,Prev}) ->
    [E|recover_lis(Prev)];
recover_lis([]) ->
    [].

% **************************************************

% **************************************************
% Copied from http://stackoverflow.com/a/4762387/4162959
% **************************************************

maxBy(F, L) ->
    element(
        2,
        lists:max([ {F(X), X} || X <- L])
    ).

% **************************************************

% **************************************************
% Copied from https://panduwana.wordpress.com/2010/04/21/combination-in-erlang/
% **************************************************

combos(L) ->
    lists:foldl(
        fun(K, Acc) -> Acc++(combos(K, L)) end,
        [[]],
        lists:seq(1, length(L))
    ).

combos(1, L) ->
    [[X] || X <- L];
combos(K, L) when K == length(L) ->
    [L];
combos(K, [H|T]) ->
    [[H | Subcombos]
        || Subcombos <- combos(K-1, T)]
    ++ (combos(K, T)).

% **************************************************

```


Output naive:

```txt

[3,4,5]
[0,4,6,9,13,15]

```


Output patience:

```txt

[2,4,5]
[0,2,6,9,11,15]

```



## Go

Patience sorting

```go
package main

import (
  "fmt"
  "sort"
)

type Node struct {
    val int
    back *Node
}

func lis (n []int) (result []int) {
  var pileTops []*Node
  // sort into piles
  for _, x := range n {
    j := sort.Search(len(pileTops), func (i int) bool { return pileTops[i].val >= x })
    node := &Node{ x, nil }
    if j != 0 { node.back = pileTops[j-1] }
    if j != len(pileTops) {
      pileTops[j] = node
    } else {
      pileTops = append(pileTops, node)
    }
  }

  if len(pileTops) == 0 { return []int{} }
  for node := pileTops[len(pileTops)-1]; node != nil; node = node.back {
    result = append(result, node.val)
  }
  // reverse
  for i := 0; i < len(result)/2; i++ {
    result[i], result[len(result)-i-1] = result[len(result)-i-1], result[i]
  }
  return
}

func main() {
    for _, d := range [][]int{{3, 2, 6, 4, 5, 1},
            {0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15}} {
        fmt.Printf("an L.I.S. of %v is %v\n", d, lis(d))
    }
}
```


{{out}}

```txt
an L.I.S. of [3 2 6 4 5 1] is [2 4 5]
an L.I.S. of [0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15] is [0 2 6 9 11 15]
```



## Haskell


### Naive implementation


```Haskell
import Data.Ord          ( comparing )
import Data.List         ( maximumBy, subsequences )
import Data.List.Ordered ( isSorted, nub )

lis :: Ord a => [a] -> [a]
lis = maximumBy (comparing length) . map nub  . filter isSorted . subsequences
--    longest                    <-- unique <-- increasing    <-- all

main = do
  print $ lis [3,2,6,4,5,1]
  print $ lis [0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15]
  print $ lis [1,1,1,1]
```


{{out}}

```txt
[2,4,5]
[0,2,6,9,11,15]
[1]
```



### Patience sorting


```Haskell
{-# LANGUAGE FlexibleContexts, UnicodeSyntax #-}

module Main (main, lis) where

import Control.Monad.ST  ( ST, runST )
import Control.Monad     ( (>>=), (=<<), foldM )
import Data.Array.ST     ( Ix,  STArray, readArray, writeArray, newArray )
import Data.Array.MArray ( MArray )

infix 4 ≡

(≡) :: Eq α ⇒ α → α → Bool
(≡) = (==)

(∘) = (.)


lis ∷ Ord α ⇒ [α] → [α]
lis xs = runST $ do
  let lxs = length xs
  pileTops ← newSTArray (min 1 lxs , lxs) []
  i        ← foldM (stack pileTops) 0 xs
  readArray pileTops i >>= return ∘ reverse

stack ∷ (Integral ι, Ord ε, Ix ι, MArray α [ε] μ)
      ⇒ α ι [ε] → ι → ε → μ ι
stack piles i x = do
  j ← bsearch piles x i
  writeArray piles j ∘ (x:) =<< if j ≡ 1 then return []
                                         else readArray piles (j-1)
  return $ if j ≡ i+1 then i+1 else i

bsearch ∷ (Integral ι, Ord ε, Ix ι, MArray α [ε] μ)
        ⇒ α ι [ε] → ε → ι → μ ι
bsearch piles x = go 1
  where go lo hi | lo > hi   = return lo
                 | otherwise =
                    do (y:_) ← readArray piles mid
                       if y < x then go (succ mid) hi
                                else go lo (pred mid)

                         where mid = (lo + hi) `div` 2

newSTArray ∷ Ix ι ⇒ (ι,ι) → ε → ST σ (STArray σ ι ε)
newSTArray = newArray


main ∷ IO ()
main = do
  print $ lis [3, 2, 6, 4, 5, 1]
  print $ lis [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]
  print $ lis [1, 1, 1, 1]
```


{{out}}

```txt
[2,4,5]
[0,2,6,9,11,15]
[1]
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages:


```unicon
procedure main(A)
    every writes((!lis(A)||" ") | "\n")
end

procedure lis(A)
    r := [A[1]] | fail
    every (put(pt := [], [v := !A]), p := !pt) do
        if put(p, p[-1] < v) then r := (*p > *r, p)
        else p[-1] := (p[-2] < v)
    return r
end
```


Sample runs:

```txt

->lis 3 2 6 4 5 1
 3 4 5
->lis 0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15
 0 4 6 9 11 15
->

```



## J


These examples are simple enough for brute force to be reasonable:


```j
increasing=: (-: /:~)@#~"1 #:@i.@^~&2@#
longestinc=: ] #~ [: (#~ ([: (= >./) +/"1)) #:@I.@increasing
```


In other words: consider all 2^n bitmasks of length n, and select those which strictly select increasing sequences. Find the length of the longest of these and use the masks of that length to select from the original sequence.

Example use:


```j

   longestinc 3,2,6,4,5,1
2 4 5
3 4 5
   longestinc 0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15
0 2 6 9 11 15
0 2 6 9 13 15
0 4 6 9 11 15
0 4 6 9 13 15
```



## Java

A solution based on patience sorting, except that it is not necessary to keep the whole pile, only the top (in solitaire, bottom) of the pile, along with pointers from each "card" to the top of its "previous" pile.

```java
import java.util.*;

public class LIS {
    public static <E extends Comparable<? super E>> List<E> lis(List<E> n) {
        List<Node<E>> pileTops = new ArrayList<Node<E>>();
        // sort into piles
        for (E x : n) {
	    Node<E> node = new Node<E>();
	    node.value = x;
            int i = Collections.binarySearch(pileTops, node);
            if (i < 0) i = ~i;
	    if (i != 0)
		node.pointer = pileTops.get(i-1);
            if (i != pileTops.size())
                pileTops.set(i, node);
            else
                pileTops.add(node);
        }
	// extract LIS from nodes
	List<E> result = new ArrayList<E>();
	for (Node<E> node = pileTops.size() == 0 ? null : pileTops.get(pileTops.size()-1);
                node != null; node = node.pointer)
	    result.add(node.value);
	Collections.reverse(result);
	return result;
    }

    private static class Node<E extends Comparable<? super E>> implements Comparable<Node<E>> {
	public E value;
	public Node<E> pointer;
        public int compareTo(Node<E> y) { return value.compareTo(y.value); }
    }

    public static void main(String[] args) {
	List<Integer> d = Arrays.asList(3,2,6,4,5,1);
	System.out.printf("an L.I.S. of %s is %s\n", d, lis(d));
        d = Arrays.asList(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15);
	System.out.printf("an L.I.S. of %s is %s\n", d, lis(d));
    }
}
```


{{out}}

```txt
an L.I.S. of [3, 2, 6, 4, 5, 1] is [2, 4, 5]
an L.I.S. of [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15] is [0, 2, 6, 9, 11, 15]
```



## JavaScript


```javascript
function getLis(input) {
  if (input.length === 0) {
    return [];
  }

  var lisLenPerIndex = [];
  let max = { index: 0, length: 1 };

  for (var i = 0; i < input.length; i++) {
    lisLenPerIndex[i] = 1;
    for (var j = i - 1; j >= 0; j--) {
      if (input[i] > input[j] && lisLenPerIndex[j] >= lisLenPerIndex[i]) {
        var length = lisLenPerIndex[i] = lisLenPerIndex[j] + 1;
        if (length > max.length) {
          max = { index: i, length };
        }
      }
    }
  }

  var lis = [input[max.index]];
  for (var i = max.index; i >= 0 && max.length !== 0; i--) {
    if (input[max.index] > input[i] && lisLenPerIndex[i] === max.length - 1) {
      lis.unshift(input[i]);
      max.length--;
    }
  }

  return lis;
}

console.log(getLongestIncreasingSubsequence([0, 7, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]));
console.log(getLongestIncreasingSubsequence([3, 2, 6, 4, 5, 1]));

```


{{out}}

```txt

[ 0, 2, 6, 9, 11, 15 ]
[ 2, 4, 5 ]

```



## jq

{{works with|jq|1.4}}
Use the patience sorting method to find a longest (strictly) increasing subsequence.

'''Generic functions:'''

Recent versions of jq have functions that obviate the need for the two generic functions defined in this subsection.

```jq
def until(cond; update):
  def _until:
    if cond then . else (update | _until) end;
  try _until catch if .== "break" then empty else . end;

# binary search for insertion point
def bsearch(target):
  . as $in
  | [0, length-1] # [low, high]
  | until(.[0] > .[1];
          .[0] as $low | .[1] as $high
          | ($low + ($high - $low) / 2 | floor) as $mid
          | if $in[$mid] >= target
            then .[1] = $mid - 1
            else .[0] = $mid + 1
            end )
  | .[0];
```

'''lis:'''

```jq
def lis:

  # Helper function:
  # given a stream, produce an array of the items in reverse order:
  def reverse(stream): reduce stream as $i ([]; [$i] + .);

  # put the items into increasing piles using the structure:
  # NODE = {"val": value, "back": NODE}
  reduce .[] as $x
    ( []; # array of NODE
      # binary search for the appropriate pile
      (map(.val) | bsearch($x)) as $i
      | setpath([$i];
                {"val": $x,
                 "back": (if $i > 0 then .[$i-1] else null end) })
    )
  | .[length - 1]
  | reverse( recurse(.back) | .val ) ;
```


'''Examples:'''

```jq
( [3,2,6,4,5,1],
  [0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15]
) | lis
```

{{out}}

```sh
$ jq -c -n -f lis.jq
[2,4,5]
[0,2,6,9,11,15]

```



## Julia

{{works with|Julia|0.6}}


```julia

function lis(arr::Vector)
    if length(arr) == 0 return copy(arr) end
    L = Vector{typeof(arr)}(length(arr))
    L[1] = [arr[1]]

    for i in 2:length(arr)
        nextL = []
        for j in 1:i
            if arr[j] < arr[i] && length(L[j]) ≥ length(nextL)
                nextL = L[j]
            end
        end
        L[i] = vcat(nextL, arr[i])
    end

    return L[indmax(length.(L))]
end

@show lis([3, 2, 6, 4, 5, 1])
@show lis([0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15])
```


{{out}}

```txt
lis([3, 2, 6, 4, 5, 1]) = [2, 4, 5]
lis([0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]) = [0, 2, 6, 9, 11, 15]
```



## Kotlin

Uses the algorithm in the Wikipedia L.I.S. article:

```scala
// version 1.1.0

fun longestIncreasingSubsequence(x: IntArray): IntArray =
    when (x.size) {
        0    -> IntArray(0)
        1    -> x
        else -> {
            val n = x.size
            val p = IntArray(n)
            val m = IntArray(n + 1)
            var len = 0
            for (i in 0 until n) {
                var lo = 1
                var hi = len
                while (lo <= hi) {
                    val mid = Math.ceil((lo + hi) / 2.0).toInt()
                    if (x[m[mid]] < x[i]) lo = mid + 1
                    else hi = mid - 1
                }
                val newLen = lo
                p[i] = m[newLen - 1]
                m[newLen] = i
                if (newLen > len) len = newLen
            }
            val s = IntArray(len)
            var k = m[len]
            for (i in len - 1 downTo 0) {
                s[i] = x[k]
                k = p[k]
            }
            s
        }
    }

fun main(args: Array<String>) {
    val lists = listOf(
        intArrayOf(3, 2, 6, 4, 5, 1),
        intArrayOf(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)
    )
    lists.forEach { println(longestIncreasingSubsequence(it).asList()) }
}
```


{{out}}

```txt

[2, 4, 5]
[0, 2, 6, 9, 11, 15]

```



## Lua


```lua
function buildLIS(seq)
    local piles = { { {table.remove(seq, 1), nil} } }
    while #seq>0 do
        local x=table.remove(seq, 1)
        for j=1,#piles do
            if piles[j][#piles[j]][1]>x then
                table.insert(piles[j], {x, (piles[j-1] and #piles[j-1])})
                break
            elseif j==#piles then
                table.insert(piles, {{x, #piles[j]}})
            end
        end
    end
    local t={}
    table.insert(t, piles[#piles][1][1])
    local p=piles[#piles][1][2]
    for i=#piles-1,1,-1 do
        table.insert(t, piles[i][p][1])
        p=piles[i][p][2]
    end
    table.sort(t)
    print(unpack(t))
end

buildLIS({3,2,6,4,5,1})
buildLIS({0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15})

```


{{out}}

```txt
2   4   5
0   2   6   9   11  15

```



## M2000 Interpreter


### Using Stack objects in an array

stack:=stackitem(L(i)), ! stack(L(j))  returns a refence to a new stack object, with the first item on L(i) (which is a reference to stack object) and merge using ! the copy of L(j) stack.


```M2000 Interpreter

Module LIS_example {
	Function LIS {
		LD=Stack.Size-1
		dim L(0 to LD)
		For i=0 to LD : Read V: L(i):=Stack:=V:next
		M=1
		M1=LD
		for i=LD-1 to 0
			for j=LD to i+1
				if stackitem(L(i))<stackitem(L(j)) then
					if len(L(i))<=len(L(j)) then L(i) =stack:=stackitem(L(i)), ! stack(L(j))
				end if
			next
			if len(L(i))>=M then M1=i:M=Len(L(i))
		next
		=L(M1)
	}
	Const seq$="sequence", subseq$="Longest increasing subsequence"
	Document doc$
	Disp(seq$, Stack:=3,2,6,4,5,1)
	Disp(subseq$, Lis(3,2,6,4,5,1))
	Disp(seq$, Stack:=0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)
	Disp(subseq$, LIS(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15))
	Print #-2,Doc$
	Clipboard Doc$

	Sub Disp(title$, m)
		local n=each(m), s$
		while n
			s$+=", "+str$(stackitem(n),"")
		end while
		s$=trim$(mid$(s$, 2))
		Doc$=title$+": "+s$+{
		}
	End Sub
}
LIS_example

```



### Using arrays in an array


```M2000 Interpreter

Module LIS_example {
	Function LIS {
		LD=Stack.Size-1
		dim L(0 to LD)
		For i=0 to LD : Read V: L(i):=(V,):next
		M=1
		M1=LD
		for i=LD-1 to 0
			for j=LD to i+1
				if Array(L(i))<Array(L(j)) then
				' 	you can use either is the same
				'	if len(L(i))<=len(L(j)) then L(i)=Cons((Array(L(i)),), L(j))
				if len(L(i))<=len(L(j)) then L(i)=(Array(L(i)),): Append L(i), L(j)
				end if
			next
			if len(L(i))>=M then M1=i:M=Len(L(i))
		next
		=L(M1)
	}
	Const seq$="sequence", subseq$="Longest increasing subsequence"
	Document doc$
	Disp(seq$, (3,2,6,4,5,1))
	Disp(subseq$, LIS(3,2,6,4,5,1))
	Disp(seq$, (0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15))
	Disp(subseq$, LIS(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15))
	Print #-2,Doc$
	Clipboard Doc$

	Sub Disp(title$, m)
		local n=each(m), s$
		while n
			s$+=", "+str$(Array(n),"")
		end while
		s$=trim$(mid$(s$, 2))
		Doc$=title$+": "+s$+{
		}
	End Sub
}
LIS_example

```


{{out}}
<pre style="height:30ex;overflow:scroll">
sequence: 3, 2, 6, 4, 5, 1
Longest increasing subsequence: 3, 4, 5
sequence: 0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15
Longest increasing subsequence: 0, 2, 6, 9, 11, 15
</pre >


## Mathematica

Although undocumented, Mathematica has the function LongestAscendingSequence which exactly does what the Task asks for:

```Mathematica
LongestAscendingSequence/@{{3,2,6,4,5,1},{0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15}}
```

{{out}}

```txt
{{2,4,5},{0,2,6,9,11,15}}
```



## Nirod

{{trans|Python}}

```nimrod
proc longestIncreasingSubsequence[T](d: seq[T]): seq[T] =
  var l = newSeq[seq[T]]()
  for i in 0 .. <d.len:
    var x = newSeq[T]()
    for j in 0 .. <i:
      if l[j][l[j].high] < d[i] and l[j].len > x.len:
        x = l[j]
    l.add x & @[d[i]]
  result = @[]
  for x in l:
    if x.len > result.len:
      result = x

for d in [@[3,2,6,4,5,1], @[0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]]:
  echo "a L.I.S. of ", d, " is ", longestIncreasingSubsequence(d)
```

{{out}}

```txt
a L.I.S. of @[3, 2, 6, 4, 5, 1] is @[3, 4, 5]
a L.I.S. of @[0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15] is @[0, 4, 6, 9, 13, 15]
```


=={{header|Objective-C}}==
Patience sorting

```objc>#import <Foundation/Foundation.h


@interface Node : NSObject {
@public
  id val;
  Node *back;
}
@end

@implementation Node
@end

@interface NSArray (LIS)
- (NSArray *)longestIncreasingSubsequenceWithComparator:(NSComparator)comparator;
@end

@implementation NSArray (LIS)
- (NSArray *)longestIncreasingSubsequenceWithComparator:(NSComparator)comparator {
  NSMutableArray *pileTops = [[NSMutableArray alloc] init];
  // sort into piles
  for (id x in self) {
    Node *node = [[Node alloc] init];
    node->val = x;
    int i = [pileTops indexOfObject:node
                      inSortedRange:NSMakeRange(0, [pileTops count])
                            options:NSBinarySearchingInsertionIndex|NSBinarySearchingFirstEqual
                    usingComparator:^NSComparisonResult(Node *node1, Node *node2) {
                      return comparator(node1->val, node2->val);
                    }];
    if (i != 0)
      node->back = pileTops[i-1];
    pileTops[i] = node;
  }

  // follow pointers from last node
  NSMutableArray *result = [[NSMutableArray alloc] init];
  for (Node *node = [pileTops lastObject]; node; node = node->back)
    [result addObject:node->val];
  return [[result reverseObjectEnumerator] allObjects];
}
@end

int main(int argc, const char *argv[]) {
  @autoreleasepool {
    for (NSArray *d in @[@[@3, @2, @6, @4, @5, @1],
         @[@0, @8, @4, @12, @2, @10, @6, @14, @1, @9, @5, @13, @3, @11, @7, @15]])
      NSLog(@"an L.I.S. of %@ is %@", d,
            [d longestIncreasingSubsequenceWithComparator:^NSComparisonResult(id obj1, id obj2) {
        return [obj1 compare:obj2];
      }]);
  }
  return 0;
}
```

{{out}}

```txt
an L.I.S. of (
    3,
    2,
    6,
    4,
    5,
    1
) is (
    2,
    4,
    5
)
an L.I.S. of (
    0,
    8,
    4,
    12,
    2,
    10,
    6,
    14,
    1,
    9,
    5,
    13,
    3,
    11,
    7,
    15
) is (
    0,
    2,
    6,
    9,
    11,
    15
)
```



## OCaml

===Naïve implementation===

```OCaml
let longest l = List.fold_left (fun acc x -> if List.length acc < List.length x
                                  then x
                                  else acc) [] l

let subsequences d l =
  let rec check_subsequences acc = function
    | x::s -> check_subsequences (if (List.hd (List.rev x)) < d
                                  then x::acc
                                  else acc) s
    | [] -> acc
  in check_subsequences [] l

let lis d =
  let rec lis' l = function
    | x::s -> lis' ((longest (subsequences x l)@[x])::l) s
    | [] -> longest l
  in lis' [] d

let _ =
  let sequences = [[3; 2; 6; 4; 5; 1]; [0; 8; 4; 12; 2; 10; 6; 14; 1; 9; 5; 13; 3; 11; 7; 15]]
  in
  List.map (fun x -> print_endline (String.concat " " (List.map string_of_int
                                                         (lis x)))) sequences
```

{{out}}

```txt

3 4 5
0 4 6 9 13 15

```



### Patience sorting


```ocaml
let lis cmp list =
  let pile_tops = Array.make (List.length list) [] in
  let bsearch_piles x len =
    let rec aux lo hi =
      if lo > hi then
        lo
      else
        let mid = (lo + hi) / 2 in
        if cmp (List.hd pile_tops.(mid)) x < 0 then
          aux (mid+1) hi
        else
          aux lo (mid-1)
    in
      aux 0 (len-1)
  in
  let f len x =
    let i = bsearch_piles x len in
    pile_tops.(i) <- x :: if i = 0 then [] else pile_tops.(i-1);
    if i = len then len+1 else len
  in
  let len = List.fold_left f 0 list in
  List.rev pile_tops.(len-1)
```

Usage:

```txt
# lis compare [3; 2; 6; 4; 5; 1];;
- : int list = [2; 4; 5]
# lis compare [0; 8; 4; 12; 2; 10; 6; 14; 1; 9; 5; 13; 3; 11; 7; 15];;
- : int list = [0; 2; 6; 9; 11; 15]
```



## Perl


### Dynamic programming

{{trans|Perl 6}}

```Perl
use strict;

sub lis {
    my @l = map [], 1 .. @_;
    push @{$l[0]}, +$_[0];
    for my $i (1 .. @_-1) {
        for my $j (0 .. $i - 1) {
            if ($_[$j] < $_[$i] and @{$l[$i]} < @{$l[$j]} + 1) {
                $l[$i] = [ @{$l[$j]} ];
            }
        }
        push @{$l[$i]}, $_[$i];
    }
    my ($max, $l) = (0, []);
    for (@l) {
        ($max, $l) = (scalar(@$_), $_) if @$_ > $max;
    }
    return @$l;
}

print join ' ', lis 3, 2, 6, 4, 5, 1;
print join ' ', lis 0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15;
```

{{out}}

```txt
2 4 5
0 2 6 9 11 15
```



### Patience sorting


```perl
sub lis {
    my @pileTops;
    # sort into piles
    foreach my $x (@_) {
	# binary search
	my $low = 0, $high = $#pileTops;
	while ($low <= $high) {
	    my $mid = int(($low + $high) / 2);
	    if ($pileTops[$mid]{val} >= $x) {
	        $high = $mid - 1;
	    } else {
	        $low = $mid + 1;
	    }
	}
	my $i = $low;
	my $node = {val => $x};
        $node->{back} = $pileTops[$i-1] if $i != 0;
	$pileTops[$i] = $node;
    }
    my @result;
    for (my $node = $pileTops[-1]; $node; $node = $node->{back}) {
        push @result, $node->{val};
    }

    return reverse @result;
}

foreach my $r ([3, 2, 6, 4, 5, 1],
	       [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]) {
    my @d = @$r;
    my @lis = lis(@d);
    print "an L.I.S. of [@d] is [@lis]\n";

}
```

{{out}}

```txt
an L.I.S. of [3 2 6 4 5 1] is [2 4 5]
an L.I.S. of [0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15] is [0 2 6 9 11 15]
```



## Perl 6

{{works with|rakudo|2018.03}}
===<!--Perl 6-->Dynamic programming===
Straight-forward implementation of the algorithm described in the video.


```perl6
sub lis(@d) {
    my @l = [].item xx @d;
    @l[0].push: @d[0];
    for 1 ..^ @d -> $i {
        for ^$i -> $j {
            if @d[$j] < @d[$i] && @l[$i] < @l[$j] + 1 {
                @l[$i] = [ @l[$j][] ]
            }
        }
        @l[$i].push: @d[$i];
    }
    return max :by(*.elems), @l;
}

say lis([3,2,6,4,5,1]);
say lis([0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15]);
```

{{out}}

```txt
[2 4 5]
[0 2 6 9 11 15]
```


===<!--Perl 6-->Patience sorting===

```perl6
sub lis(@deck is copy) {
    my @S = [@deck.shift() => Nil].item;
    for @deck -> $card {
        with first { @S[$_][*-1].key > $card }, ^@S -> $i {
            @S[$i].push: $card => @S[$i-1][*-1] // Nil
        } else {
            @S.push: [ $card => @S[*-1][*-1] // Nil ].item
        }
    }
    reverse map *.key, (
        @S[*-1][*-1], *.value ...^ !*.defined
    )
}

say lis <3 2 6 4 5 1>;
say lis <0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15>;
```

{{out}}

```txt
[2 4 5]
[0 2 6 9 11 15]
```



## Phix

Using the Wikipedia algorithm (converted to 1-based indexing)

```Phix
function lis(sequence X, integer N = length(X))
    sequence P = repeat(0,N)
    sequence M = repeat(0,N)
    integer len = 0
    for i=1 to N do
        integer lo = 1
        integer hi = len
        while lo<=hi do
            integer mid = ceil((lo+hi)/2)
            if X[M[mid]]<X[i] then
                lo = mid + 1
            else
                hi = mid - 1
            end if
        end while
        if lo>1 then
            P[i] = M[lo-1]
        end if
        M[lo] = i
        if lo>len then len = lo end if
    end for
    sequence res = repeat(0,len)
    if len>0 then
        integer k = M[len]
        for i=len to 1 by -1 do
            res[i] = X[k]
            k = P[k]
        end for
    end if
    return res
end function

constant tests = {{3, 2, 6, 4, 5, 1},
                  {0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15}}
for i=1 to length(tests) do
    ?lis(tests[i])
end for
```

{{out}}

```txt

{2,4,5}
{0,2,6,9,11,15}

```



## PHP

Patience sorting

```php
<?php
class Node {
    public $val;
    public $back = NULL;
}

function lis($n) {
    $pileTops = array();
    // sort into piles
    foreach ($n as $x) {
        // binary search
        $low = 0; $high = count($pileTops)-1;
        while ($low <= $high) {
            $mid = (int)(($low + $high) / 2);
            if ($pileTops[$mid]->val >= $x)
                $high = $mid - 1;
            else
                $low = $mid + 1;
        }
        $i = $low;
        $node = new Node();
        $node->val = $x;
        if ($i != 0)
            $node->back = $pileTops[$i-1];
        $pileTops[$i] = $node;
    }
    $result = array();
    for ($node = count($pileTops) ? $pileTops[count($pileTops)-1] : NULL;
         $node != NULL; $node = $node->back)
        $result[] = $node->val;

    return array_reverse($result);
}

print_r(lis(array(3, 2, 6, 4, 5, 1)));
print_r(lis(array(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)));
?>
```

{{out}}

```txt
Array
(
    [0] => 2
    [1] => 4
    [2] => 5
)
Array
(
    [0] => 0
    [1] => 2
    [2] => 6
    [3] => 9
    [4] => 11
    [5] => 15
)
```



## PicoLisp

Adapted patience sorting approach:

```PicoLisp
(de longinc (Lst)
   (let (D NIL  R NIL)
      (for I Lst
         (cond
            ((< I (last D))
               (for (Y . X) D
                  (T (> X I) (set (nth D Y) I)) ) )
            ((< I (car R))
               (set R I)
               (when D (set (cdr R) (last D))) )
            (T (when R (queue 'D (car R)))
               (push 'R I) ) ) )
      (flip R) ) )
```


Original recursive glutton:

```PicoLisp
(de glutton (L)
   (let N (pop 'L)
      (maxi length
         (recur (N L)
            (ifn L
               (list (list N))
               (mapcan
                  '((R)
                     (if (> (car R) N)
                        (list (cons N R) R)
                        (list (list N) R) ) )
                  (recurse (car L) (cdr L)) ) ) ) ) ) )

(test (2 4 5)
   (glutton (3 2 6 4 5 1)))

(test (2 6 9 11 15)
   (glutton (8 4 12 2 10 6 14 1 9 5 13 3 11 7 15)))

(test (-31 0 83 782)
   (glutton (4 65 2 -31 0 99 83 782 1)) )
```



## PowerShell

{{works with|PowerShell|2}}

```PowerShell
function Get-LongestSubsequence ( [int[]]$A )
    {
    If ( $A.Count -lt 2 ) { return $A }

    #  Start with an "empty" pile
    #  (We will only store the top value in each "pile".)
    $Pile = @( [int]::MaxValue )
    $Last = 0

    #  Hashtable to hold the back pointers
    $BP = @{}

    #  For each number in the orginal sequence...
    ForEach ( $N in $A )
        {
        #  Find the first pile with a value greater than N
        $i = 0..$Last | Where { $N -lt $Pile[$_] } | Select -First 1

        #  Place N on the pile
        $Pile[$i] = $N

        #  Set the back pointer for this value to the value of the previous pile
        $BP["$N"] = $Pile[$i-1]

        #  If this is the previously empty pile, add a new empty pile
        If ( $i -eq $Last )
            {
            $Pile += @( [int]::MaxValue )
            $Last++
            }
        }

    #  Ignore the empty pile
    $Last--

    #  Start with the value of the last pile
    $N = $Pile[$Last]
    $S = @( $N )

    #  Add the remainder of the values by walking through the back pointers
    ForEach ( $i in $Last..1 )
        {
        $S += ( $N = $BP["$N"] )
        }

    #  Return the series (reversed into the correct order)
    return $S[$Last..0]
    }
```


```PowerShell
( Get-LongestSubsequence 3, 2, 6, 4, 5, 1 ) -join ', '
( Get-LongestSubsequence 0, 8, 4, 12, 2, 10, 6, 16, 14, 1, 9, 5, 13, 3, 11, 7, 15 ) -join ', '
```

{{out}}

```txt
2, 4, 5
0, 2, 6, 9, 11, 15
```



## Prolog

Works with SWI-Prolog version 6.4.1

Naïve implementation.



```prolog
lis(In, Out) :-
	% we ask Prolog to find the longest sequence
	aggregate(max(N,Is), (one_is(In, [], Is), length(Is, N)), max(_, Res)),
	reverse(Res, Out).


% we describe the way to find increasing subsequence
one_is([], Current, Current).


one_is([H | T], Current, Final) :-
	(   Current = [], one_is(T, [H], Final));
	(   Current = [H1 | _], H1 < H,   one_is(T, [H | Current], Final));
	one_is(T, Current, Final).

```

Prolog finds the first longest subsequence

```txt
 ?- lis([0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15], Out).
Out = [0,4,6,9,13,15].

 ?- lis([3,2,6,4,5,1], Out).
Out = [3,4,5].

```



## Python


===Python: O(nlogn) Method from Wikipedia's LIS Article[https://en.wikipedia.org/wiki/Longest_increasing_subsequence#Efficient_algorithms]===

```python
def longest_increasing_subsequence(X):
    """Returns the Longest Increasing Subsequence in the Given List/Array"""
    N = len(X)
    P = [0] * N
    M = [0] * (N+1)
    L = 0
    for i in range(N):
       lo = 1
       hi = L
       while lo <= hi:
           mid = (lo+hi)//2
           if (X[M[mid]] < X[i]):
               lo = mid+1
           else:
               hi = mid-1

       newL = lo
       P[i] = M[newL-1]
       M[newL] = i

       if (newL > L):
           L = newL

    S = []
    k = M[L]
    for i in range(L-1, -1, -1):
        S.append(X[k])
        k = P[k]
    return S[::-1]

if __name__ == '__main__':
    for d in [[3,2,6,4,5,1], [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]]:
        print('a L.I.S. of %s is %s' % (d, longest_increasing_subsequence(d)))
```


{{out}}

```txt
a L.I.S. of [3, 2, 6, 4, 5, 1] is [2, 4, 5]
a L.I.S. of [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15] is [0, 2, 6, 9, 11, 15]
```



### Python: Method from video


```python
def longest_increasing_subsequence(d):
    'Return one of the L.I.S. of list d'
    l = []
    for i in range(len(d)):
        l.append(max([l[j] for j in range(i) if l[j][-1] < d[i]] or [[]], key=len)
                  + [d[i]])
    return max(l, key=len)

if __name__ == '__main__':
    for d in [[3,2,6,4,5,1], [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]]:
        print('a L.I.S. of %s is %s' % (d, longest_increasing_subsequence(d)))
```


{{out}}

```txt
a L.I.S. of [3, 2, 6, 4, 5, 1] is [3, 4, 5]
a L.I.S. of [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15] is [0, 4, 6, 9, 13, 15]
```



### Python: Patience sorting method


```python
from collections import namedtuple
from functools import total_ordering
from bisect import bisect_left

@total_ordering
class Node(namedtuple('Node_', 'val back')):
    def __iter__(self):
        while self is not None:
            yield self.val
            self = self.back
    def __lt__(self, other):
        return self.val < other.val
    def __eq__(self, other):
        return self.val == other.val

def lis(d):
    """Return one of the L.I.S. of list d using patience sorting."""
    if not d:
        return []
    pileTops = []
    for di in d:
        j = bisect_left(pileTops, Node(di, None))
        new_node = Node(di, pileTops[j-1] if j > 0 else None)
        if j == len(pileTops):
            pileTops.append(new_node)
        else:
            pileTops[j] = new_node

    return list(pileTops[-1])[::-1]

if __name__ == '__main__':
    for d in [[3,2,6,4,5,1],
              [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]]:
        print('a L.I.S. of %s is %s' % (d, lis(d)))
```


{{out}}

```txt
a L.I.S. of [3, 2, 6, 4, 5, 1] is [2, 4, 5]
a L.I.S. of [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15] is [0, 2, 6, 9, 11, 15]
```



## Racket

Patience sorting. The program saves only the top card of each pile, with a link (cons) to the top of the previous pile at the time it was inserted. It uses binary search to find the correct pile.

```Racket
#lang racket/base
(require data/gvector)

(define (gvector-last gv)
  (gvector-ref gv (sub1 (gvector-count gv))))

(define (lis-patience-sort input-list)
  (let ([piles (gvector)])
    (for ([item (in-list input-list)])
      (insert-item! piles item))
    (reverse (gvector-last piles))))

(define (insert-item! piles item)
  (if (zero? (gvector-count piles))
      (gvector-add! piles (cons item '()))
      (cond
        [(not (<= item (car (gvector-last piles))))
         (gvector-add! piles (cons item (gvector-last piles)))]
        [(<= item (car (gvector-ref piles 0)))
         (gvector-set! piles 0 (cons item '()))]
        [else (let loop ([first 1] [last (sub1 (gvector-count piles))])
                (if (= first last)
                    (gvector-set! piles first (cons item (gvector-ref piles (sub1 first))))
                    (let ([middle (quotient (+ first last) 2)])
                      (if (<= item (car (gvector-ref piles middle)))
                          (loop first middle)
                          (loop (add1 middle) last)))))])))
```

{{out}}

```txt
'(2 4 5)
'(0 2 6 9 11 15)
```



## REXX

{{trans|VBScript}}

```rexx
/*REXX program finds & displays the  longest increasing subsequence  from a list of #'s.*/
$.=;  $.1= 3 2 6 4 5 1                           /*define the 1st list to be examined.  */
      $.2= 0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15 /*   "    "  2nd   "   "  "     "      */

        do j=1   while  $.j\=='';     say        /* [↓]  process all of the list for LIS*/
        say ' input: '  $.j                      /*display the (original) input list.   */
        call LIS        $.j                      /*invoke the  LIS  function.           */
        say 'output: '  result                   /*display the  output (result from LIS)*/
        end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
LIS: procedure; parse arg x;   n= words(x);   if n==0  then return ''
     p.=;                            m.= p.
           do #=1  to n;  _= # - 1;  @._= word(x, #)    /*build an array (@) from input.*/
           end   /*#*/
     L= 0
           do j=0  to n-1;  lo= 1
           HI= L
                     do  while LO<=HI;    middle= (LO+HI) % 2
                          _= m.middle            /*create a temporary value for @ index.*/
                     if @._<@.j  then LO= middle + 1
                                 else HI= middle - 1
                     end   /*while*/
           newLO= LO
                  _= newLO - 1                   /*create a temporary value for M index.*/
           p.j= m._
           m.newLO= j
           if newLO>L  then L= newLO
           end   /*i*/
     k= m.L;                $=                   /* [↓]  build a list for the result.   */
                     do L;  $= @.k $;  k= p.k    /*perform this  DO  loop   L   times.  */
                     end   /*i*/
     return strip($)                             /*the result has an extra leading blank*/
```

{{out|output|text=  when using the internal default input:}}

```txt

 input:  3 2 6 4 5 1
output:  2 4 5

 input:  0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15
output:  0 2 6 9 11 15

```



## Ring


```ring

# Project : Longest increasing subsequence

tests = [[3, 2, 6, 4, 5, 1], [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]]
res = []
for x=1 to len(tests)
    lis(tests[x])
    showarray(res)
end

func lis(X)
     N = len(X)
     P = list(N)
     M = list(N)
     for nr = 1 to len(P)
         P[nr] = 0
     next
     for nr = 1 to len(M)
         P[nr] = 0
     next
     len = 0
     for i=1 to N
         lo = 1
         hi = len
         while lo <= hi
               mid = floor((lo+hi)/2)
               if X[M[mid]]<X[i]
                  lo = mid + 1
               else
                  hi = mid - 1
               ok
         end
         if lo>1
            P[i] = M[lo-1]
         ok
         M[lo] = i
         if lo>len
            len = lo
         ok
     next
     res = list(len)
     if len>0
        k = M[len]
        for i=len to 1 step -1
            res[i] = X[k]
            k = P[k]
        next
     ok
     return res

func showarray(vect)
     see "{"
     svect = ""
     for n = 1 to len(vect)
         svect = svect + vect[n] + ", "
     next
     svect = left(svect, len(svect) - 2)
     see svect
     see "}" + nl

```

Output:

```txt

{2, 4, 5}
{0, 2, 6, 9, 11, 15}

```



## Ruby

Patience sorting

```ruby
Node = Struct.new(:val, :back)

def lis(n)
  pileTops = []
  # sort into piles
  for x in n
    # binary search
    low, high = 0, pileTops.size-1
    while low <= high
      mid = low + (high - low) / 2
      if pileTops[mid].val >= x
        high = mid - 1
      else
        low = mid + 1
      end
    end
    i = low
    node = Node.new(x)
    node.back = pileTops[i-1]  if i > 0
    pileTops[i] = node
  end

  result = []
  node = pileTops.last
  while node
    result.unshift(node.val)
    node = node.back
  end
  result
end

p lis([3, 2, 6, 4, 5, 1])
p lis([0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15])
```

{{out}}

```txt
[2, 4, 5]
[0, 2, 6, 9, 11, 15]
```


## Rust



```Rust
fn lower_bound<T: PartialOrd>(list: &Vec<T>, value: &T) -> usize {
    if list.is_empty() {
        return 0;
    }
    let mut lower = 0usize;
    let mut upper = list.len();
    while lower != upper {
        let middle = lower + upper >> 1;
        if list[middle] < *value {
            lower = middle + 1;
        } else {
            upper = middle;
        }
    }
    return lower;
}

fn lis<T: PartialOrd + Copy>(list: &Vec<T>) -> Vec<T> {
    if list.is_empty() {
        return Vec::new();
    }
    let mut subseq: Vec<T> = Vec::new();
    subseq.push(*list.first().unwrap());
    for i in list[1..].iter() {
        if *i <= *subseq.last().unwrap() {
            let index = lower_bound(&subseq, i);
            subseq[index] = *i;
        } else {
            subseq.push(*i);
        }
    }
    return subseq;
}

fn main() {
    let list = vec![3, 2, 6, 4, 5, 1];
    println!("{:?}", lis(&list));
    let list = vec![0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15];
    println!("{:?}", lis(&list));
}
```


{{out}}

```txt
[1, 4, 5]
[0, 1, 3, 7, 11, 15]
```



## Scala


### Patience sorting

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/Wx8DsUO/1 ScalaFiddle (JavaScript)] or by [https://scastie.scala-lang.org/FtLHeaAwSrO6VXVOTTZ7FQ Scastie (JVM)].

```Scala
object LongestIncreasingSubsequence extends App {
  val tests = Map(
    "3,2,6,4,5,1" -> Seq("2,4,5", "3,4,5"),
    "0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15" -> Seq("0,2,6,9,11,15", "0,2,6,9,13,15", "0,4,6,9,13,15", "0,4,6,9,11,15")
  )

  def lis(l: Array[Int]): Seq[Array[Int]] =
    if (l.length < 2) Seq(l)
    else {
      def increasing(done: Array[Int], remaining: Array[Int]): Seq[Array[Int]] =
        if (remaining.isEmpty) Seq(done)
        else
          (if (remaining.head > done.last)
            increasing(done :+ remaining.head, remaining.tail)
          else Nil) ++ increasing(done, remaining.tail) // all increasing combinations

      val all = (1 to l.length)
        .flatMap(i => increasing(l take i takeRight 1, l.drop(i + 1)))
        .sortBy(-_.length)
      all.takeWhile(_.length == all.head.length) // longest of all increasing combinations
    }

  def asInts(s: String): Array[Int] = s split "," map (_.toInt)

  assert(tests forall {
    case (given, expect) =>
      val allLongests: Seq[Array[Int]] = lis(asInts(given))
      println(
        s"$given has ${allLongests.length} longest increasing subsequences, e.g. ${
          allLongests.last.mkString(",")}")
      allLongests.forall(lis => expect.contains(lis.mkString(",")))
  })
}
```

{{Out}}

```txt
3,2,6,4,5,1 has 2 longest increasing subsequences, e.g. 2,4,5
0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15 has 4 longest increasing subsequences, e.g. 0,2,6,9,11,15
```



### Brute force solution


```Scala
def powerset[A](s: List[A]) = (0 to s.size).map(s.combinations(_)).reduce(_++_)
def isSorted(l:List[Int])(f: (Int, Int) => Boolean) = l.view.zip(l.tail).forall(x => f(x._1,x._2))
def sequence(set: List[Int])(f: (Int, Int) => Boolean) = powerset(set).filter(_.nonEmpty).filter(x => isSorted(x)(f)).toList.maxBy(_.length)

sequence(set)(_<_)
sequence(set)(_>_)
```


## Scheme

Patience sorting

```scheme
(define (lis less? lst)
  (define pile-tops (make-vector (length lst)))
  (define (bsearch-piles x len)
    (let aux ((lo 0)
	      (hi (- len 1)))
      (if (> lo hi)
	  lo
	  (let ((mid (quotient (+ lo hi) 2)))
	    (if (less? (car (vector-ref pile-tops mid)) x)
		(aux (+ mid 1) hi)
		(aux lo (- mid 1)))))))
  (let aux ((len 0)
	    (lst lst))
    (if (null? lst)
	(reverse (vector-ref pile-tops (- len 1)))
	(let* ((x (car lst))
	       (i (bsearch-piles x len)))
	  (vector-set! pile-tops i (cons x (if (= i 0)
					       '()
					       (vector-ref pile-tops (- i 1)))))
	  (aux (if (= i len) (+ len 1) len) (cdr lst))))))

(display (lis < '(3 2 6 4 5 1))) (newline)
(display (lis < '(0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15))) (newline)
```


{{out}}

```txt
(2 4 5)
(0 2 6 9 11 15)
```



## Sidef

Dynamic programming:

```ruby
func lis(a) {
    var l = a.len.of { [] }
    l[0] << a[0]
    for i in (1..a.end) {
        for j in ^i {
            if ((a[j] < a[i]) && (l[i].len < l[j].len+1)) {
                l[i] = [l[j]...]
            }
        }
        l[i] << a[i]
    }
    l.max_by { .len }
}

say lis(%i<3 2 6 4 5 1>)
say lis(%i<0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15>)
```


Patience sorting:

```ruby
func lis(deck) {
    var pileTops = []
    deck.each { |x|
        var low = 0;
        var high = pileTops.end
        while (low <= high) {
            var mid = ((low + high) // 2)
            if (pileTops[mid]{:val} >= x) {
                high = mid-1
            } else {
                low = mid+1
            }
        }
        var i = low
        var node = Hash(val => x)
        node{:back} = pileTops[i-1] if (i != 0)
        pileTops[i] = node
    }
    var result = []
    for (var node = pileTops[-1]; node; node = node{:back}) {
        result << node{:val}
    }
    result.reverse
}

say lis(%i<3 2 6 4 5 1>)
say lis(%i<0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15>)
```


{{out}}

```txt

[2, 4, 5]
[0, 2, 6, 9, 11, 15]

```



## Standard ML

Patience sorting
{{works with|SML/NJ}}

```sml
fun lis cmp n =
  let
    val pile_tops = DynamicArray.array (length n, [])
    fun bsearch_piles x =
      let
        fun aux (lo, hi) =
          if lo > hi then
            lo
          else
            let
              val mid = (lo + hi) div 2
            in
              if cmp (hd (DynamicArray.sub (pile_tops, mid)), x) = LESS then
                aux (mid+1, hi)
              else
                aux (lo, mid-1)
            end
      in
        aux (0, DynamicArray.bound pile_tops)
      end
    fun f x =
      let
        val i = bsearch_piles x
      in
        DynamicArray.update (pile_tops, i,
	  x :: (if i = 0 then [] else DynamicArray.sub (pile_tops, i-1)))
      end
  in
    app f n;
    rev (DynamicArray.sub (pile_tops, DynamicArray.bound pile_tops))
  end
```

Usage:

```txt
- lis Int.compare [3, 2, 6, 4, 5, 1];
val it = [2,4,5] : int list
- lis Int.compare [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15];
val it = [0,2,6,9,11,15] : int list
```



## Swift



```swift
import Foundation

extension Array where Element: Comparable {
  @inlinable
  public func longestIncreasingSubsequence() -> [Element] {
    var startI = [Int](repeating: 0, count: count)
    var endI = [Int](repeating: 0, count: count + 1)
    var len = 0

    for i in 0..<count {
      var lo = 1
      var hi = len

      while lo <= hi {
        let mid = Int(ceil((Double(lo + hi)) / 2))

        if self[endI[mid]] <= self[i] {
          lo = mid + 1
        } else {
          hi = mid - 1
        }
      }

      startI[i] = endI[lo-1]
      endI[lo] = i

      if lo > len {
        len = lo
      }
    }

    var s = [Element]()
    var k = endI[len]

    for _ in 0..<len {
      s.append(self[k])
      k = startI[k]
    }

    return s.reversed()
  }
}

let l1 = [3, 2, 6, 4, 5, 1]
let l2 = [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]

print("\(l1) = \(l1.longestIncreasingSubsequence())")
print("\(l2) = \(l2.longestIncreasingSubsequence())")
```


{{out}}


```txt
[3, 2, 6, 4, 5, 1] = [2, 4, 5]
[0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15] = [0, 2, 6, 9, 11, 15]
```



## Swym

{{trans|Python}}
Based on the Python video solution. Interpreter at [[http://cheersgames.com/swym/SwymInterpreter.html?Array.%27lis%27%0A%7B%0A%20%20%27stems%27%20%3D%20Number.Array.mutableArray%5B%20%5B%5D%20%5D%0A%20%0A%20%20forEach%28this%29%20%27value%27-%3E%0A%20%20%7B%0A%20%20%20%20%27bestStem%27%20%3D%20stems.where%7B%3D%3D%5B%5D%20%7C%7C%20.last%20%3C%20value%7D.max%7B.length%7D%0A%20%0A%20%20%20%20stems.push%28%20bestStem%20+%20%5Bvalue%5D%20%29%0A%20%20%7D%0A%20%0A%20%20return%20stems.max%7B.length%7D%0A%7D%0A%20%0A%5B3%2C2%2C6%2C4%2C5%2C1%5D.lis.trace%0A%5B0%2C8%2C4%2C12%2C2%2C10%2C6%2C14%2C1%2C9%2C5%2C13%2C3%2C11%2C7%2C15%5D.lis.trace]]

```swym
Array.'lis'
{
  'stems' = Number.Array.mutableArray[ [] ]

  forEach(this) 'value'->
  {
    'bestStem' = stems.where{==[] || .last < value}.max{.length}

    stems.push( bestStem + [value] )
  }

  return stems.max{.length}
}

[3,2,6,4,5,1].lis.trace
[0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15].lis.trace
```

{{out}}

```txt

[3,4,5]
[0,4,6,9,13,15]

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

proc longestIncreasingSubsequence {sequence} {
    # Get the increasing subsequences (and their lengths)
    set subseq [list 1 [lindex $sequence 0]]
    foreach value $sequence {
	set max {}
	foreach {len item} $subseq {
	    if {[lindex $item end] < $value} {
		if {[llength [lappend item $value]] > [llength $max]} {
		    set max $item
		}
	    } elseif {![llength $max]} {
		set max [list $value]
	    }
	}
	lappend subseq [llength $max] $max
    }
    # Pick the longest subsequence; -stride requires Tcl 8.6
    return [lindex [lsort -stride 2 -index 0 $subseq] end]
}
```

Demonstrating:

```tcl
puts [longestIncreasingSubsequence {3 2 6 4 5 1}]
puts [longestIncreasingSubsequence {0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15}]
```

{{out}}

```txt

3 4 5
0 4 6 9 13 15

```



## VBScript


```vb

Function LIS(arr)
	n = UBound(arr)
	Dim p()
	ReDim p(n)
	Dim m()
	ReDim m(n)
	l = 0
	For i = 0 To n
		lo = 1
		hi = l
		Do While lo <= hi
			middle = Int((lo+hi)/2)
			If arr(m(middle)) < arr(i) Then
				lo = middle + 1
			Else
				hi = middle - 1
			End If
		Loop
		newl = lo
		p(i) = m(newl-1)
		m(newl) = i
		If newL > l Then
			l = newl
		End If
	Next
	Dim s()
	ReDim s(l)
	k = m(l)
	For i = l-1 To 0 Step - 1
		s(i) = arr(k)
		k = p(k)
	Next
	LIS = Join(s,",")
End Function

WScript.StdOut.WriteLine LIS(Array(3,2,6,4,5,1))
WScript.StdOut.WriteLine LIS(Array(0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15))

```


{{Out}}

```txt

2,4,5,
0,2,6,9,11,15,

```



## zkl


```zkl
fcn longestSequence(ns){ // based on Patience sorting
   piles:=L();
   backPtr:='wrap(np){ return(np-1,if(np) piles[np-1].len()-1 else -1) }; // maybe (-1,-1)
   foreach n in (ns){ newPile:=True;   // create list of sorted lists
      foreach e,p in (piles.enumerate()){
	 if(n<p[-1][0]){
	    p.del(1,-1)  // only need the first and last elements
	    .append(T(n,backPtr(e))); newPile=False;
	    break;
	 }
      }
      if(newPile) piles.append(L(T(n,backPtr(piles.len()))));
   }
   reg r=L(),p=-1,n=0;
   do{ n,p=piles[p][n]; r.write(n); p,n=p; }while(p!=-1);
   r.reverse()
}
```


```zkl
foreach ns in (T(T(1),T(3,2,6,4,5,1),T(4,65,2,-31,0,99,83,782,1),
	       T(0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15),"foobar")){
   s:=longestSequence(ns);
   println(s.len(),": ",s," from ",ns);
}
```

{{out}}

```txt

1: L(1) from L(1)
3: L(2,4,5) from L(3,2,6,4,5,1)
4: L(-31,0,83,782) from L(4,65,2,-31,0,99,83,782,1)
6: L(0,1,3,9,11,15) from L(0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15)
4: L("f","o","o","r") from foobar

```

