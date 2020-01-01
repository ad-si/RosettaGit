+++
title = "Sort disjoint sublist"
description = ""
date = 2019-10-18T20:43:07Z
aliases = []
[extra]
id = 9247
[taxonomies]
categories = []
tags = []
+++

{{task}}
Given a list of values and a set of integer indices into that value list, the task is to sort the values at the given indices, but preserving the values at indices outside the set of those to be sorted.

Make your example work with the following list of values and set of indices:
<code>
    values: [7, <b>6</b>, 5, 4, 3, 2, <b>1</b>, <b>0</b>]
    indices: {6, 1, 7}</code>
Where the correct result would be:
    <code>[7, <b>0</b>, 5, 4, 3, 2, <b>1</b>, <b>6</b>]</code>.

Note that for one based, rather than the zero-based indexing above, use the <code>indices: {7, 2, 8}</code>. The indices are described as a set rather than a list but any collection-type of those indices without duplication may be used as long as the example is insensitive to the order of indices given.


;Cf.
*   [[Order disjoint list items]]





## Ada


```Ada
with Ada.Text_IO, GNAT.Bubble_Sort;
use  Ada.Text_IO;

procedure DisjointSort is

   package Int_Io is new Integer_IO (Integer);

   subtype Index_Range is Natural range 1 .. 8;
   Input_Array : array (Index_Range) of Integer := (7, 6, 5, 4, 3, 2, 1, 0);

   subtype Subindex_Range is Natural range 1 .. 3;
   type Sub_Arrays is array (Subindex_Range) of Integer;

   Sub_Index : Sub_Arrays := (7, 2, 8);
   Sub_Array : Sub_Arrays;

   -- reuse of the somehow generic GNAT.Bubble_Sort (for Ada05)

   procedure Sort (Work_Array : in out Sub_Arrays) is
      procedure Exchange (Op1, Op2 : Natural) is
         Temp : Integer;
      begin
         Temp             := Work_Array (Op1);
         Work_Array (Op1) := Work_Array (Op2);
         Work_Array (Op2) := Temp;
      end Exchange;

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return (Work_Array (Op1) < Work_Array (Op2));
      end Lt;
   begin
      GNAT.Bubble_Sort.Sort
        (N    => Subindex_Range'Last,
         Xchg => Exchange'Unrestricted_Access,
         Lt   => Lt'Unrestricted_Access);
   end Sort;

begin
   -- as the positions are not ordered, first sort the positions
   Sort (Sub_Index);
   -- extract the values to be sorted
   for I in Subindex_Range loop
      Sub_Array (I) := Input_Array (Sub_Index (I));
   end loop;
   Sort (Sub_Array);
   -- put the sorted values at the right place
   for I in Subindex_Range loop
      Input_Array (Sub_Index (I))  := Sub_Array (I);
   end loop;

   for I in Index_Range loop
      Int_Io.Put (Input_Array (I), Width => 2);
   end loop;
   New_Line;

end DisjointSort;
```



## APL


```apl

      ∇SDS[⎕]∇
    ∇
[0]   Z←I SDS L
[1]   L[I[⍋I]]←Z[⍋Z←L[I←∪I]]
[2]   Z←L
    ∇

```

{{out}}

```txt

      ⎕IO←0
      6 1 7 SDS ⎕←⌽⍳8
7 6 5 4 3 2 1 0
7 0 5 4 3 2 1 6

```



## AppleScript


Works with versions of AppleScript from OS X 10.10 onwards


```AppleScript
use AppleScript version "2.4"
use framework "Foundation"
use scripting additions


-- disjointSort :: [Int] -> [Int] -> [Int]
on disjointSort(ixs, xs)
    set ks to sort(ixs)
    script nth -- 1-based index
        on |λ|(k)
            item (succ(k)) of xs
        end |λ|
    end script
    set dct to mapFromList(zip(ks, sort(map(nth, ks))))

    script build
        on |λ|(x, i)
            set mb to lookupDict(pred(i) as string, dct)
            if Nothing of mb then
                x
            else
                |Just| of mb
            end if
        end |λ|
    end script
    map(build, xs)
end disjointSort


on run
    disjointSort({6, 1, 7}, {7, 6, 5, 4, 3, 2, 1, 0})
end run



-- GENERIC FUNCTIONS ----------------------------------------------------

-- Just :: a -> Maybe a
on Just(x)
    {type:"Maybe", Nothing:false, Just:x}
end Just

-- Nothing :: Maybe a
on Nothing()
    {type:"Maybe", Nothing:true}
end Nothing

-- length :: [a] -> Int
on |length|(xs)
    set c to class of xs
    if list is c or string is c then
        length of xs
    else
        (2 ^ 29 - 1) -- (maxInt - simple proxy for non-finite)
    end if
end |length|

-- lookupDict :: a -> Dict -> Maybe b
on lookupDict(k, dct)
    set ca to current application
    set v to (ca's NSDictionary's dictionaryWithDictionary:dct)'s objectForKey:k
    if v ≠ missing value then
        Just(item 1 of ((ca's NSArray's arrayWithObject:v) as list))
    else
        Nothing()
    end if
end lookupDict

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- mapFromList :: [(k, v)] -> Dict
on mapFromList(kvs)
    set tpl to unzip(kvs)
    script
        on |λ|(x)
            x as string
        end |λ|
    end script
    (current application's NSDictionary's ¬
        dictionaryWithObjects:(|2| of tpl) ¬
            forKeys:map(result, |1| of tpl)) as record
end mapFromList

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- pred :: Enum a => a -> a
on pred(x)
    x - 1
end pred

-- sort :: Ord a => [a] -> [a]
on sort(xs)
    ((current application's NSArray's arrayWithArray:xs)'s ¬
        sortedArrayUsingSelector:"compare:") as list
end sort

-- succ :: Enum a => a -> a
on succ(x)
    1 + x
end succ

-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    set c to class of xs
    if list is c then
        if 0 < n then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    else if string is c then
        if 0 < n then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else if script is c then
        set ys to {}
        repeat with i from 1 to n
            set v to xs's |λ|()
            if missing value is v then
                return ys
            else
                set end of ys to v
            end if
        end repeat
        return ys
    else
        missing value
    end if
end take

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    {type:"Tuple", |1|:a, |2|:b, length:2}
end Tuple

-- unzip :: [(a,b)] -> ([a],[b])
on unzip(xys)
    set xs to {}
    set ys to {}
    repeat with xy in xys
        set end of xs to |1| of xy
        set end of ys to |2| of xy
    end repeat
    return Tuple(xs, ys)
end unzip

-- zip :: [a] -> [b] -> [(a, b)]
on zip(xs, ys)
    zipWith(Tuple, xs, ys)
end zip

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(|length|(xs), |length|(ys))
    if 1 > lng then return {}
    set xs_ to take(lng, xs) -- Allow for non-finite
    set ys_ to take(lng, ys) -- generators like cycle etc
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs_, item i of ys_)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```AppleScript
{7, 0, 5, 4, 3, 2, 1, 6}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      Sort% = FN_sortinit(0,0) : REM Ascending

      DIM list%(7) : list%() = 7, 6, 5, 4, 3, 2, 1, 0
      DIM indices%(2) : indices%() = 6, 1, 7

      PROCsortdisjoint(list%(), indices%())
      PRINT FNshowlist(list%())
      END

      DEF PROCsortdisjoint(l%(), i%())
      LOCAL C%, i%, n%, t%()
      n% = DIM(i%(),1)
      DIM t%(n%)
      FOR i% = 0 TO n%
        t%(i%) = l%(i%(i%))
      NEXT
      C% = n% + 1
      CALL Sort%, i%(0)
      CALL Sort%, t%(0)
      FOR i% = 0 TO n%
        l%(i%(i%)) = t%(i%)
      NEXT
      ENDPROC

      DEF FNshowlist(l%())
      LOCAL i%, o$
      o$ = "["
      FOR i% = 0 TO DIM(l%(),1)
        o$ += STR$(l%(i%)) + ", "
      NEXT
      = LEFT$(LEFT$(o$)) + "]"
```

'''Output:'''

```txt

[7, 0, 5, 4, 3, 2, 1, 6]

```



## Bracmat


```bracmat
7 6 5 4 3 2 1 0:?values
& 6 1 7:?indices
& 0:?sortedValues:?sortedIndices
&   whl
  ' ( !indices:%?i ?indices
    & !values:? [!i %@?value ?
    & (!value.)+!sortedValues:?sortedValues
    & (!i.)+!sortedIndices:?sortedIndices
    )
&   whl
  ' ( !sortedIndices:(?i.)+?sortedIndices
    & !values:?A [!i %@? ?Z
    & !sortedValues:(?value.)+?sortedValues
    & !A !value !Z:?values
    )
& out$!values;
```

Output:

```txt
7 0 5 4 3 2 1 6
```



## C


```c
#include <stdio.h>

/* yes, bubble sort */
void bubble_sort(int *idx, int n_idx, int *buf)
{
        int i, j, tmp;
#define for_ij for (i = 0; i < n_idx; i++) for (j = i + 1; j < n_idx; j++)
#define sort(a, b) if (a < b) { tmp = a; a = b; b = tmp;}
        for_ij { sort(idx[j], idx[i]);          }
        for_ij { sort(buf[idx[j]], buf[idx[i]]);}
#undef for_ij
#undef sort
}

int main()
{
        int values[] = {7, 6, 5, 4, 3, 2, 1, 0};
        int idx[] = {6, 1, 7};
        int i;

        printf("before sort:\n");
        for (i = 0; i < 8; i++)
                printf("%d ", values[i]);

        printf("\n\nafter sort:\n");
        bubble_sort(idx, 3, values);

        for (i = 0; i < 8; i++)
                printf("%d ", values[i]);
        printf("\n");

        return 0;
}
```



## C sharp


```csharp
using System;
using System.Linq;
using System.Collections.Generic;

public class Test
{
    public static void Main()
    {
        var list = new List<int>{ 7, 6, 5, 4, 3, 2, 1, 0 };
        list.SortSublist(6, 1, 7);
        Console.WriteLine(string.Join(", ", list));
    }
}

public static class Extensions
{
    public static void SortSublist<T>(this List<T> list, params int[] indices)
        where T : IComparable<T>
    {
        var sublist = indices.OrderBy(i => i)
            .Zip(indices.Select(i => list[i]).OrderBy(v => v),
                (Index, Value) => new { Index, Value });

        foreach (var entry in sublist) {
            list[entry.Index] = entry.Value;
        }
    }

}
```



## C++


```cpp
#include <algorithm>
#include <iostream>
#include <iterator>
#include <vector>

template <typename ValueIterator, typename IndicesIterator>
void sortDisjoint(ValueIterator valsBegin, IndicesIterator indicesBegin,
		  IndicesIterator indicesEnd) {
    std::vector<int> temp;

    for (IndicesIterator i = indicesBegin; i != indicesEnd; ++i)
        temp.push_back(valsBegin[*i]); // extract

    std::sort(indicesBegin, indicesEnd); // sort
    std::sort(temp.begin(), temp.end()); // sort a C++ container

    std::vector<int>::const_iterator j = temp.begin();
    for (IndicesIterator i = indicesBegin; i != indicesEnd; ++i, ++j)
        valsBegin[*i] = *j; // replace
}


int main()
{
    int values[] = { 7, 6, 5, 4, 3, 2, 1, 0 };
    int indices[] = { 6, 1, 7 };

    sortDisjoint(values, indices, indices+3);

    std::copy(values, values + 8, std::ostream_iterator<int>(std::cout, " "));
    std::cout << "\n";

    return 0;
}
```

{{out}}

```txt

7 0 5 4 3 2 1 6

```


{{trans|Go}}
Solution that sorts using a custom iterator that iterates a disjoint sublist.

```cpp
#include <algorithm>
#include <iostream>
#include <iterator>
#include <vector>

template <typename ValueIterator, typename IndicesIterator>
struct DisjointSubsetIterator :
  public std::iterator<std::random_access_iterator_tag,
		       typename std::iterator_traits<ValueIterator>::value_type> {
  typedef typename std::iterator_traits<ValueIterator>::value_type V;
  ValueIterator valsBegin;
  IndicesIterator i;
  DisjointSubsetIterator() { }
  DisjointSubsetIterator(const ValueIterator &_v, IndicesIterator _i) :
    valsBegin(_v), i(_i) { }
  DisjointSubsetIterator& operator++() { ++i; return *this; }
  DisjointSubsetIterator operator++(int) {
    DisjointSubsetIterator tmp = *this; ++(*this); return tmp; }
  bool operator==(const DisjointSubsetIterator& y) { return i == y.i; }
  bool operator!=(const DisjointSubsetIterator& y) { return i != y.i; }
  V &operator*() { return valsBegin[*i]; }
  DisjointSubsetIterator& operator--() { --i; return *this; }
  DisjointSubsetIterator operator--(int) {
    DisjointSubsetIterator tmp = *this; --(*this); return tmp; }
  DisjointSubsetIterator& operator+=(int n) { i += n; return *this; }
  DisjointSubsetIterator& operator-=(int n) { i -= n; return *this; }
  DisjointSubsetIterator operator+(int n) {
    DisjointSubsetIterator tmp = *this; return tmp += n; }
  DisjointSubsetIterator operator-(int n) {
    DisjointSubsetIterator tmp = *this; return tmp -= n; }
  int operator-(const DisjointSubsetIterator &y) { return i - y.i; }
  V &operator[](int n) { return *(*this + n); }
  bool operator<(const DisjointSubsetIterator &y) { return i < y.i; }
  bool operator>(const DisjointSubsetIterator &y) { return i > y.i; }
  bool operator<=(const DisjointSubsetIterator &y) { return i <= y.i; }
  bool operator>=(const DisjointSubsetIterator &y) { return i >= y.i; }
};
template <typename ValueIterator, typename IndicesIterator>
DisjointSubsetIterator<ValueIterator, IndicesIterator>
operator+(int n, const DisjointSubsetIterator<ValueIterator, IndicesIterator> &i) {
  return i + n; }

template <typename ValueIterator, typename IndicesIterator>
void sortDisjoint(ValueIterator valsBegin, IndicesIterator indicesBegin,
		  IndicesIterator indicesEnd) {
  std::sort(DisjointSubsetIterator<ValueIterator, IndicesIterator>(valsBegin, indicesBegin),
            DisjointSubsetIterator<ValueIterator, IndicesIterator>(valsBegin, indicesEnd));
}


int main()
{
    int values[] = { 7, 6, 5, 4, 3, 2, 1, 0 };
    int indices[] = { 6, 1, 7 };

    sortDisjoint(values, indices, indices+3);

    std::copy(values, values + 8, std::ostream_iterator<int>(std::cout, " "));
    std::cout << "\n";

    return 0;
}
```

{{out}}

```txt

7 0 5 4 3 2 1 6

```



## Clojure


```clojure
(defn disjoint-sort [coll idxs]
  (let [val-subset (keep-indexed #(when ((set idxs) %) %2) coll)
        replacements (zipmap (set idxs) (sort val-subset))]
    (apply assoc coll (flatten (seq replacements)))))
```


{{out}}

```txt
user=> (disjoint-sort [7 6 5 4 3 2 1 0] #{6 1 7})
[7 0 5 4 3 2 1 6]
```



## Common Lisp


```lisp
(defun disjoint-sort (values indices)
  "Destructively perform a disjoin sublist sort on VALUES with INDICES."
  (loop :for element :in
     (sort (loop :for index :across indices
              :collect (svref values index))
           '<)
     :for index :across (sort indices '<)
     :do (setf (svref values index) element))
  values)
```

{{out}}

```txt
CL-USER> (disjoint-sort #(7 6 5 4 3 2 1 0) #(6 1 7))
#(7 0 5 4 3 2 1 6)
```



## D


```d
import std.algorithm, std.range, std.array;

void main() {
    auto data = [7, 6, 5, 4, 3, 2, 1, 0];
    auto indices = [6, 1, 7];

    data.indexed(indices.sort()).sort();

    assert(data == [7, 0, 5, 4, 3, 2, 1, 6]);
}
```


### Lower Level version


```d
import std.algorithm: swap;

void disjointSort(T, U)(T[] arr, U[] indexes)
in {
    if (arr.length == 0)
        assert(indexes.length == 0);
    else {
        foreach (idx; indexes)
            assert(idx >= 0 && idx < arr.length);
    }
} body {
    void quickSort(U* left, U* right) {
        if (right > left) {
            auto pivot = arr[left[(right - left) / 2]];
            auto r = right, l = left;
            do {
                while (arr[*l] < pivot) l++;
                while (arr[*r] > pivot) r--;
                if (l <= r) {
                    swap(arr[*l], arr[*r]);
                    swap(l, r);
                    l++;
                    r--;
                }
            } while (l <= r);
            quickSort(left, r);
            quickSort(l, right);
        }
    }

    if (arr.length == 0 || indexes.length == 0)
        return;
    quickSort(&indexes[0], &indexes[$-1]);
}

void main() {
    auto data = [7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0];
    auto indexes = [6, 1, 1, 7];
    disjointSort(data, indexes);
    assert(data == [7.0, 0.0, 5.0, 4.0, 3.0, 2.0, 1.0, 6.0]);
}
```



### Simple Alternative Version


```d
import std.stdio, std.algorithm;

void main() {
    auto data = [7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0];
    auto indexes = [6, 1, 1, 7]; // One duplicated added to test.

    // Remove duplicates, in place:
    indexes.length -= indexes.sort().uniq().copy(indexes).length;

    foreach (i, idx; indexes)
        swap(data[i], data[idx]);

    data[0 .. indexes.length].sort();

    foreach_reverse (i, idx; indexes)
        swap(data[idx], data[i]);

    assert(data == [7.0, 0.0, 5.0, 4.0, 3.0, 2.0, 1.0, 6.0]);
}
```



## EchoLisp


```scheme

(define (sort-disjoint values indices)
    (define sorted (list-sort <
    (for/list [(v values) (i (in-naturals))]
	#:when (member i indices)  v)))

    (for/list [(v values) (i (in-naturals))]
	(if (not (member i indices)) v
	(begin0
		(first sorted)
		(set! sorted (rest sorted))))))

(define (task)
	(sort-disjoint '[7 6 5 4 3 2 1 0] {6 1 7}))

(task)
    → (7 0 5 4 3 2 1 6)

```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'routines;
import system'culture;

extension op
{
    sortSublist(indices)
    {
        var subList := indices.orderBy:(x => x)
                            .zipBy(indices.selectBy:(i => self[i])
                                .orderBy:(x => x), (index,val => new::{ Index = index; Value = val; }))
                            .toArray();

        var list := self.clone();
        subList.forEach:(r)
        {
            list[r.Index] := r.Value
        };

        ^ list
    }
}

public program()
{
    var list := new int[]::( 7, 6, 5, 4, 3, 2, 1, 0 );

    console.printLine(list.sortSublist(new int[]::(6, 1, 7)).asEnumerable())
}
```

{{out}}

```txt

7,0,5,4,3,2,1,6

```



## Elixir


```elixir
defmodule Sort_disjoint do
  def sublist(values, indices) when is_list(values) and is_list(indices) do
    indices2 = Enum.sort(indices)
    selected = select(values, indices2, 0, []) |> Enum.sort
    replace(values, Enum.zip(indices2, selected), 0, [])
  end

  defp select(_, [], _, selected), do: selected
  defp select([val|t], [i|rest], i, selected), do: select(t, rest, i+1, [val|selected])
  defp select([_|t], indices, i, selected), do: select(t, indices, i+1, selected)

  defp replace(values, [], _, list), do: Enum.reverse(list, values)
  defp replace([_|t], [{i,v}|rest], i, list), do: replace(t, rest, i+1, [v|list])
  defp replace([val|t], indices, i, list), do: replace(t, indices, i+1, [val|list])
end

values = [7, 6, 5, 4, 3, 2, 1, 0]
indices = [6, 1, 7]
IO.inspect Sort_disjoint.sublist(values, indices)
```


{{out}}

```txt

[7, 0, 5, 4, 3, 2, 1, 6]

```



## Erlang


```Erlang

-module( sort_disjoint ).

-export( [sublist/2, task/0] ).

sublist( Values, Indices ) ->
	Sorted_indices = lists:sort( Indices ),
	Values_indexes = lists:seq( 1, erlang:length(Values) ),
	{[], [], Indices_values} = lists:foldl( fun indices_values/2, {Values, Sorted_indices, []}, Values_indexes ),
	Sorted_indices_values = lists:zip( Sorted_indices, lists:sort(Indices_values) ),
	{Sorted_values, {[], []}} = lists:mapfoldl( fun merge/2, {Values, Sorted_indices_values}, Values_indexes ),
	Sorted_values.

task() -> sublist( [7, 6, 5, 4, 3, 2, 1, 0], [7, 2, 8] ).



indices_values( Index, {[H | Values], [Index | Indices], Indices_values} ) -> {Values, Indices, [H | Indices_values]};
indices_values( _Index, {[_H | Values], Indices, Indices_values} ) -> {Values, Indices, Indices_values}.

merge( Index, {[_H | Values], [{Index, Value} | Sorted_indices_values]} ) -> {Value, {Values, Sorted_indices_values}};
merge( _Index, {[H | Values], Sorted_indices_values} ) -> {H, {Values, Sorted_indices_values}}.

```

{{out}}

```txt

20> sort_disjoint:task().
[7,0,5,4,3,2,1,6]

```



## ERRE


```ERRE
PROGRAM DISJOINT

DIM LST%[7],INDICES%[2]

DIM L%[7],I%[2],Z%[2]
PROCEDURE SHOWLIST(L%[]->O$)
      LOCAL I%
      O$="["
      FOR I%=0 TO UBOUND(L%,1) DO
        O$=O$+STR$(L%[I%])+", "
      END FOR
      O$=LEFT$(O$,LEN(O$)-2)+"]"
END PROCEDURE

PROCEDURE SORT(Z%[]->Z%[])
   LOCAL N%,P%,FLIPS%
   P%=UBOUND(Z%,1)
   FLIPS%=TRUE
   WHILE FLIPS% DO
      FLIPS%=FALSE
      FOR N%=0 TO P%-1 DO
        IF Z%[N%]>Z%[N%+1] THEN SWAP(Z%[N%],Z%[N%+1]) FLIPS%=TRUE
      END FOR
   END WHILE
END PROCEDURE

PROCEDURE SortDisJoint(L%[],I%[]->L%[])
      LOCAL J%,N%
      LOCAL DIM T%[2]

      N%=UBOUND(I%,1)
      FOR J%=0 TO N% DO
        T%[J%]=L%[I%[J%]]
      END FOR
      SORT(I%[]->I%[])
      SORT(T%[]->T%[])
      FOR J%=0 TO N% DO
        L%[I%[J%]]=T%[J%]
      END FOR
END PROCEDURE

BEGIN
  LST%[]=(7,6,5,4,3,2,1,0)
  INDICES%[]=(6,1,7)
  SortDisJoint(LST%[],INDICES%[]->LST%[])
  ShowList(LST%[]->O$)
  PRINT(O$)
END PROGRAM
```

{{out}}

```txt

[ 7, 0, 5, 4, 3, 2, 1, 6]

```



## Euphoria


```euphoria
include sort.e

function uniq(sequence s)
    sequence out
    out = s[1..1]
    for i = 2 to length(s) do
        if not find(s[i], out) then
            out = append(out, s[i])
        end if
    end for
    return out
end function

function disjointSort(sequence s, sequence idx)
    sequence values
    idx = uniq(sort(idx))
    values = repeat(0, length(idx))
    for i = 1 to length(idx) do
        values[i] = s[idx[i]]
    end for
    values = sort(values)
    for i = 1 to length(idx) do
        s[idx[i]] = values[i]
    end for
    return s
end function

constant data = {7, 6, 5, 4, 3, 2, 1, 0}
constant indexes = {7, 2, 8}
```


Output:

```txt
{7,0,5,4,3,2,1,6}

```


=={{header|F_Sharp|F#}}==
{{trans|Python}}
Works with arrays instead of lists because this algorithm is more efficient with a random access collection type. Returns a copy of the array, as is usually preferred in F#.

```fsharp
let sortDisjointSubarray data indices =
  let indices = Set.toArray indices // creates a sorted array
  let result = Array.copy data
  Array.map (Array.get data) indices
  |> Array.sort
  |> Array.iter2 (Array.set result) indices
  result


printfn "%A" (sortDisjointSubarray [|7;6;5;4;3;2;1;0|] (set [6;1;7]))
```



## Factor


```factor
: disjoint-sort ( values indices -- seq )
    over <enumerated> nths unzip swap [ natural-sort ] bi@
    pick [ set-nth ] curry 2each ;
```

{{out}}

```factor
IN: scratchpad { 7 6 5 4 3 2 1 0 } { 6 1 7 } disjoint-sort .
{ 7 0 5 4 3 2 1 6 }
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program Example
  implicit none

  integer :: array(8) = (/ 7, 6, 5, 4, 3, 2, 1, 0 /)
  integer :: indices(3) = (/ 7, 2, 8 /)

! In order to make the output insensitive to index order
! we need to sort the indices first
  call Isort(indices)

! Should work with any sort routine as long as the dummy
! argument array has been declared as an assumed shape array
! Standard insertion sort used in this example
  call Isort(array(indices))

  write(*,*) array

contains

subroutine Isort(a)
  integer, intent(in out) :: a(:)
  integer :: temp
  integer :: i, j

  do i = 2, size(a)
     j = i - 1
     temp = a(i)
     do while (j>=1 .and. a(j)>temp)
        a(j+1) = a(j)
        j = j - 1
     end do
     a(j+1) = temp
  end do

end subroutine Isort
end program Example
```

Output

```txt
           7           0           5           4           3           2           1           6
```



## Go


```go
package main

import (
    "fmt"
    "sort"
)

func main() {
    // givens
    values := []int{7, 6, 5, 4, 3, 2, 1, 0}
    indices := map[int]int{6: 0, 1: 0, 7: 0}

    orderedValues := make([]int, len(indices))
    orderedIndices := make([]int, len(indices))
    i := 0
    for j := range indices {
        // validate that indices are within list boundaries
        if j < 0 || j >= len(values) {
            fmt.Println("Invalid index: ", j)
            return
        }
        // extract elements to sort
        orderedValues[i] = values[j]
        orderedIndices[i] = j
        i++
    }
    // sort
    sort.Ints(orderedValues)
    sort.Ints(orderedIndices)

    fmt.Println("initial:", values)
    // replace sorted values
    for i, v := range orderedValues {
        values[orderedIndices[i]] = v
    }
    fmt.Println("sorted: ", values)
}
```

Output:

```txt

initial: [7 6 5 4 3 2 1 0]
sorted:  [7 0 5 4 3 2 1 6]

```

Alternative algorithm, sorting in place through the extra level of indirection.

Compared to the strategy of extract-sort-replace, this strategy avoids the space overhead of the work area and the time overhead of extracting and reinserting elements.  At some point however, the cost of indirection multiplied by O(log n) would dominate, and extract-sort-replace would become preferable.

```go
package main

import (
    "fmt"
    "sort"
)

// type and methods satisfying sort.Interface
type subListSortable struct {
    values  sort.Interface
    indices []int
}

func (s subListSortable) Len() int {
    return len(s.indices)
}

func (s subListSortable) Swap(i, j int) {
    s.values.Swap(s.indices[i], s.indices[j])
}

func (s subListSortable) Less(i, j int) bool {
    return s.values.Less(s.indices[i], s.indices[j])
}

func main() {
    // givens
    values := []int{7, 6, 5, 4, 3, 2, 1, 0}
    indices := map[int]int{6: 0, 1: 0, 7: 0}

    // make ordered list of indices for sort methods
    ordered := make([]int, len(indices))
    if len(indices) > 0 {
        i := 0
        for j := range indices {
            ordered[i] = j
            i++
        }
        sort.Ints(ordered)

        // validate that indices are within list boundaries
        if ordered[0] < 0 {
            fmt.Println("Invalid index: ", ordered[0])
            return
        }
        if ordered[len(ordered)-1] >= len(values) {
            fmt.Println("Invalid index: ", ordered[len(ordered)-1])
            return
        }
    }

    // instantiate sortable type and sort
    s := subListSortable{sort.IntSlice(values), ordered}
    fmt.Println("initial:", s.values)
    sort.Sort(s)
    fmt.Println("sorted: ", s.values)
}
```



## Groovy

Groovy allows List-valued indexing to "gather" and "scatter" arbitrary sublists, making the solution almost trivial.

```groovy
def sparseSort = { a, indices = ([] + (0..<(a.size()))) ->
    indices.sort().unique()
    a[indices] = a[indices].sort()
    a
}
```


Test:

```groovy
def a = [7, 6, 5, 4, 3, 2, 1, 0]

println a
println sparseSort(a, [])      // no indices to sort
println a
println sparseSort(a, [6,1,7]) // suggested sample indices
println a
println sparseSort(a)          // default == sort all
println a
```


Output:

```txt
[7, 6, 5, 4, 3, 2, 1, 0]
[7, 6, 5, 4, 3, 2, 1, 0]
[7, 6, 5, 4, 3, 2, 1, 0]
[7, 0, 5, 4, 3, 2, 1, 6]
[7, 0, 5, 4, 3, 2, 1, 6]
[0, 1, 2, 3, 4, 5, 6, 7]
[0, 1, 2, 3, 4, 5, 6, 7]
```



## Haskell

Here are three variations on the solution: using ordinary lists, immutable "boxed" arrays, and mutable "unboxed" arrays.


```haskell
import Control.Monad
import qualified Data.Array as A
import Data.Array.IArray
import Data.Array.ST
import Data.List
import Data.List.Utils

-- Partition 'xs' according to whether their element indices are in 'is'.  Sort
-- the sublist corresponding to 'is', merging the result with the remainder of
-- the list.
disSort1
  :: (Ord a, Num a, Enum a, Ord b)
  => [b] -> [a] -> [b]
disSort1 xs is =
  let is_ = sort is
      (sub, rest) = partition ((`elem` is_) . fst) $ zip [0 ..] xs
  in map snd . merge rest . zip is_ . sort $ map snd sub

-- Convert the list to an array.  Extract the sublist corresponding to the
-- indices 'is'.  Sort the sublist, replacing those elments in the array.
disSort2
  :: (Ord a)
  => [a] -> [Int] -> [a]
disSort2 xs is =
  let as = A.listArray (0, length xs - 1) xs
      sub = zip (sort is) . sort $ map (as !) is
  in elems $ as // sub

-- Similar to disSort2, but using mutable arrays.  The sublist is updated
-- "in place", rather than creating a new array.  However, this is not visible
-- to a caller.
disSort3 :: [Int] -> [Int] -> [Int]
disSort3 xs is =
  elems . runSTUArray $
  do as <- newListArray (0, length xs - 1) xs
     sub <- (zip (sort is) . sort) Control.Applicative.<$> mapM (readArray as) is
     mapM_ (uncurry (writeArray as)) sub
     return as

main :: IO ()
main = do
  let xs = [7, 6, 5, 4, 3, 2, 1, 0]
      is = [6, 1, 7]
  print $ disSort1 xs is
  print $ disSort2 xs is
  print $ disSort3 xs is
```



Or, in terms of Data.Map:

```haskell
import Data.Map as M (fromList, keys, lookup)
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Data.List (sort)

disjointSort :: [Int] -> [Int] -> [Int]
disjointSort ixs xs =
  let ks = sort ixs
      dctAll = fromList $ zip xs [0 ..]
      dctIx = fromList $ zip ks $ sort (mapMaybe (`M.lookup` dctAll) ks)
  in mapMaybe
       ((<|>) <$> (`M.lookup` dctIx) <*> (`M.lookup` dctAll))
       (keys dctAll)

main :: IO ()
main = print $ disjointSort [6, 1, 7] [7, 6, 5, 4, 3, 2, 1, 0]
```

{{Out}}

```txt
[7,0,5,4,3,2,1,6]
```


=={{header|Icon}} and {{header|Unicon}}==

Icon's lists are 1-based, so the example uses (7, 2, 8) as the indices, not (6, 1 7).


```icon

link sort # get the 'isort' procedure for sorting a list

procedure sortDisjoint (items, indices)
  indices := isort (indices) # sort indices into a list
  result := copy (items)
  values := []
  every put (values, result[!indices])
  values := isort (values)
  every result[!indices] := pop (values)
  return result
end

procedure main ()
  # set up and do the sort
  items := [7, 6, 5, 4, 3, 2, 1, 0]
  indices := set(7, 2, 8) # note, Icon lists 1-based
  result := sortDisjoint (items, indices)
  # display result
  every writes (!items || " ")
  write ()
  every writes (!indices || " ")
  write ()
  every writes (!result || " ")
  write ()
end

```


Output:

```txt

7 6 5 4 3 2 1 0
2 7 8
7 0 5 4 3 2 1 6

```


The expression <code>!indices</code> generates the value of each index in turn, so the line
```icon
every put (values, result[!indices])
```
 effectively loops through each index, putting <code>result[index]</code> into the list 'values'.


## Io

Io does not come with a set type.

```Io
List disjointSort := method(indices,
    sortedIndices := indices unique sortInPlace
    sortedValues := sortedIndices map(idx,at(idx)) sortInPlace
    sortedValues foreach(i,v,atPut(sortedIndices at(i),v))
    self
)

list(7,6,5,4,3,2,1,0) disjointSort(list(6,1,7)) println
```

{{output}}

```txt
list(7, 0, 5, 4, 3, 2, 1, 6)
```



## J

Note that the task requires us to ignore the order of the indices.


```j
   7 6 5 4 3 2 1 0 (/:~@:{`[`]}~ /:~@~.) 6 1 7
7 0 5 4 3 2 1 6
```


Compare this with:

```j
   6 1 7 /:~@:{`[`]} 7 6 5 4 3 2 1 0
7 1 5 4 3 2 0 6
```


Here, the order of the indices specifies the order we want the selected items to be sorted in: 7 '''''1''''' 5 4 3 2 '''''0 6'''''


## Java

{{works with|Java|1.5+}}
This function will modify the index array and the values list.

```java5
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class Disjoint {
    public static <T extends Comparable<? super T>> void sortDisjoint(
            List<T> array, int[] idxs) {
        Arrays.sort(idxs);
        List<T> disjoint = new ArrayList<T>();
        for (int idx : idxs) {
            disjoint.add(array.get(idx));
        }
        Collections.sort(disjoint);
        int i = 0;
        for (int idx : idxs) {
            array.set(idx, disjoint.get(i++));
        }
    }

    public static void main(String[] args) {
        List<Integer> list = Arrays.asList(7, 6, 5, 4, 3, 2, 1, 0);
        int[] indices = {6, 1, 7};
        System.out.println(list);
        sortDisjoint(list, indices);
        System.out.println(list);
    }
}
```

{{out}}

```txt
[7, 6, 5, 4, 3, 2, 1, 0]
[7, 0, 5, 4, 3, 2, 1, 6]
```


{{works with|Java|1.5+}}
{{trans|Go}}
Shorter solution that sorts a list "wrapper" which represents a "view" into the disjoint sublist of the list.

```java5
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.AbstractList;

public class Disjoint {
    public static <T extends Comparable<? super T>> void sortDisjoint(
            final List<T> array, final int[] idxs) {
        Arrays.sort(idxs);
        Collections.sort(new AbstractList<T>() {
		public int size() { return idxs.length; }
		public T get(int i) { return array.get(idxs[i]); }
		public T set(int i, T x) { return array.set(idxs[i], x); }
	    });
    }

    public static void main(String[] args) {
        List<Integer> list = Arrays.asList(7, 6, 5, 4, 3, 2, 1, 0);
        int[] indices = {6, 1, 7};
        System.out.println(list);
        sortDisjoint(list, indices);
        System.out.println(list);
    }
}
```

{{out}}

```txt
[7, 6, 5, 4, 3, 2, 1, 0]
[7, 0, 5, 4, 3, 2, 1, 6]
```



## JavaScript


### ES5


### =Iterative=


Does not check for duplicate indices.

```JavaScript
function sort_disjoint(values, indices) {
  var sublist = [];
  indices.sort(function(a, b) { return a > b; });

  for (var i = 0; i < indices.length; i += 1) {
    sublist.push(values[indices[i]]);
  }

  sublist.sort(function(a, b) { return a < b; });

  for (var i = 0; i < indices.length; i += 1) {
    values[indices[i]] = sublist.pop();
  }

  return values;
}
```



### =Functional=



```JavaScript
(function () {
    'use strict';

    // disjointSort :: [a] -> [Int] -> [a]
    function disjointSort(xs, indices) {

        // Sequence of indices discarded
        var indicesSorted = indices.sort(),
            subsetSorted = indicesSorted
            .map(function (i) {
                return xs[i];
            })
            .sort();

        return xs
            .map(function (x, i) {
                var iIndex = indicesSorted.indexOf(i);

                return iIndex !== -1 ? (
                    subsetSorted[iIndex]
                ) : x;
            });
    }

    return disjointSort([7, 6, 5, 4, 3, 2, 1, 0], [6, 1, 7])

})();
```


{{Out}}

```txt
[7, 0, 5, 4, 3, 2, 1, 6]
```



### ES6



```JavaScript
(() => {
  'use strict';

  // disjointSort :: [Int] -> [Int] -> [Int]
  const disjointSort = (indices, xs) => {
    const
      ks = sort(indices),
      dct = mapFromList(
        zip(ks, sort(map(k => xs[k], ks)))
      );
    return map(
      (x, i) => {
        const v = dct[i.toString()];
        return undefined !== v ? v : x;
      },
      xs
    );
  };

  // main :: IO ()
  const main = () =>
    showLog(
      disjointSort(
        [6, 1, 7],
        [7, 6, 5, 4, 3, 2, 1, 0]
      )
    );

  // GENERIC FUNCTIONS ----------------------------

  // length :: [a] -> Int
  const length = xs => xs.length || Infinity;

  // map :: (a -> b) -> [a] -> [b]
  const map = (f, xs) => xs.map(f);

  // mapFromList :: [(k, v)] -> Dict
  const mapFromList = kvs =>
    kvs.reduce(
      (a, kv) => {
        const k = kv[0];
        return Object.assign(a, {
          [(('string' === typeof k) && k) || showJSON(k)]: kv[1]
        });
      }, {}
    );

  // showJSON :: a -> String
  const showJSON = x => JSON.stringify(x);

  // showLog :: a -> IO ()
  const showLog = (...args) =>
    console.log(
      args
      .map(JSON.stringify)
      .join(' -> ')
    );

  // sort :: Ord a => [a] -> [a]
  const sort = xs => xs.slice()
    .sort((a, b) => a < b ? -1 : (a > b ? 1 : 0));

  // take :: Int -> [a] -> [a]
  // take :: Int -> String -> String
  const take = (n, xs) =>
    xs.constructor.constructor.name !== 'GeneratorFunction' ? (
      xs.slice(0, n)
    ) : [].concat.apply([], Array.from({
      length: n
    }, () => {
      const x = xs.next();
      return x.done ? [] : [x.value];
    }));

  // Tuple (,) :: a -> b -> (a, b)
  const Tuple = (a, b) => ({
    type: 'Tuple',
    '0': a,
    '1': b,
    length: 2
  });

  // Use of `take` and `length` here allows for zipping with non-finite
  // lists - i.e. generators like cycle, repeat, iterate.

  // zip :: [a] -> [b] -> [(a, b)]
  const zip = (xs, ys) => {
    const lng = Math.min(length(xs), length(ys));
    return Infinity !== lng ? (() => {
      const bs = take(lng, ys);
      return take(lng, xs).map((x, i) => Tuple(x, bs[i]));
    })() : zipGen(xs, ys);
  };


  // MAIN ---
  return main();
})();
```

{{Out}}

```txt
[7, 0, 5, 4, 3, 2, 1, 6]
```



## jq


We define a jq function, disjointSort, that accepts the array of values as input,
but for clarity we first define a utility function for updating an array at multiple places:

```jq
def setpaths(indices; values):
  reduce range(0; indices|length) as $i
    (.; .[indices[$i]] = values[$i]);

def disjointSort(indices):
  (indices|unique) as $ix   # "unique" sorts
  # Set $sorted to the sorted array of values at $ix:
  | ([ .[ $ix[] ] ] | sort) as $sorted
  | setpaths( $ix; $sorted) ;
```

Example:
```jq

[7, 6, 5, 4, 3, 2, 1, 0] | disjointSort( [6, 1, 7] )
```
produces:
[7,0,5,4,3,2,1,6]


## Julia

{{works with|Julia|0.6}}


```julia
function sortselected(a::AbstractVector{<:Real}, s::AbstractVector{<:Integer})
    sel = unique(sort(s))
    if sel[1] < 1 || length(a) < sel[end]
        throw(BoundsError())
    end
    b = collect(copy(a))
    b[sel] = sort(b[sel])
    return b
end

a = [7, 6, 5, 4, 3, 2, 1, 0]
sel = [7, 2, 8]
b = sortselected(a, sel)

println("Original: $a\n\tsorted on $sel\n -> sorted array: $b")
```


{{out}}

```txt
Original: [7, 6, 5, 4, 3, 2, 1, 0]
	sorted on [7, 2, 8]
 -> sorted array: [7, 0, 5, 4, 3, 2, 1, 6]
```



## K


```K

  {@[x;y@<y;:;a@<a:x@y]}[7 6 5 4 3 2 1 0;6 1 7]
7 0 5 4 3 2 1 6

```



### Another way


```K

sort : {x[<x]}
nums : 7 6 5 4 3 2 1 0
i : sort 6 1 7
nums[i] : sort nums[i]
nums
7 0 5 4 3 2 1 6

```



## Kotlin


```scala
// version 1.1.51

/* in place sort */
fun IntArray.sortDisjoint(indices: Set<Int>) {
    val sortedSubset = this.filterIndexed { index, _ -> index in indices }.sorted()
    if (sortedSubset.size < indices.size)
        throw IllegalArgumentException("Argument set contains out of range indices")
    indices.sorted().forEachIndexed { index, value -> this[value] = sortedSubset[index] }
}

fun main(args: Array<String>) {
    val values = intArrayOf(7, 6, 5, 4, 3, 2, 1, 0)
    val indices = setOf(6, 1, 7)
    println("Original array : ${values.asList()} sorted on indices $indices")
    values.sortDisjoint(indices)
    println("Sorted array   : ${values.asList()}")
}

```


{{out}}

```txt

Original array : [7, 6, 5, 4, 3, 2, 1, 0] sorted on indices [6, 1, 7]
Sorted array   : [7, 0, 5, 4, 3, 2, 1, 6]

```



## Lua


```lua
values  = { 7, 6, 5, 4, 3, 2, 1, 0 }
indices = { 6, 1, 7 }

i = 1						-- discard duplicates
while i < #indices do
    j = i + 1
    while j < #indices do
	if indices[i] == indices[j] then
  	    table.remove( indices[j] )
	end
	j = j + 1
    end
    i = i + 1
end

for i = 1, #indices do
    indices[i] = indices[i] + 1      -- the tables of lua are one-based
end

vals = {}
for i = 1, #indices do
    vals[i] = values[ indices[i] ]
end

table.sort( vals )
table.sort( indices )

for i = 1, #indices do
    values[ indices[i] ] = vals[i]
end

for i = 1, #values do
    io.write( values[i], "  " )
end
```


```txt
7  0  5  4  3  2  1  6
```



## Maple


```Maple
sortDisjoint := proc(values, indices::set)
	local vals,inds,i:
	vals := sort([seq(values[i], i in indices)]):
	inds := sort(convert(indices, Array)):
	for i to numelems(vals) do
		values(inds[i]) := vals[i]:
	od:
end proc:
tst := Array([7,6,5,4,3,2,1,0]):
sortDisjoint(tst,{7,2,8});
```

{{out}}

```txt
[7 0 5 4 3 2 1 6]
```




## Mathematica


```Mathematica
Values = { 7, 6, 5, 4, 3, 2, 1, 0} ; Indices = { 7, 2, 8 };
Values[[Sort[Indices]]] = Sort[Values[[Indices]]];

Values
-> { 7, 0, 5, 4, 3, 2, 1, 6 }
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method sortDisjoint(oldList, indices) public static
  newList = oldList.space()
  if indices.words() > 1 then do -- only do work if we need to
    subList = ArrayList()
    idxList = ArrayList()
    -- pick the input list apart
    loop ix = 1 to indices.words()
      iw = indices.word(ix)
      nw = oldList.word(iw)
      -- protect against bad outcomes...
      if iw > oldList.words() then signal ArrayIndexOutOfBoundsException()
      if iw < 1               then signal ArrayIndexOutOfBoundsException()
      subList.add(nw)
      idxList.add(iw)
      end ix
    Collections.sort(subList) -- sort sublist
    Collections.sort(idxList) -- sort indices
    -- put it all back together
    loop kx = 0 to subList.size() - 1
      kk = Rexx subList.get(kx)
      ii = Rexx idxList.get(kx)
      newList = newList.subword(1, ii - 1) kk newList.subword(ii + 1)
      end kx
    end
  return newList

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg vList ',' iList
  if vList = '' then vList = 7 6 5 4 3 2 1 0
  if iList = '' then iList = 7 2 8
  rList = sortDisjoint(vList, iList)
  say 'In: ' vList.space
  say 'Out:' rList.space
  say 'Idx:' iList.space
  return

```

{{out}}

```txt

In:  7 6 5 4 3 2 1 0
Out: 7 0 5 4 3 2 1 6
Idx: 7 2 8

```



## Nial


{{works with|Q'Nial Version 6.3}}


```Nial

  values := [7, 6, 5, 4, 3, 2, 1, 0]
  indices := sortup [6, 1, 7]
  values#indices := sortup values#indices
7 0 5 4 3 2 1 6

```



## Nim


```nim
import algorithm

proc sortDisjoinSublist[T](data: var seq[T], indices: seq[int]) =
  var indices = indices
  sort indices, cmp[T]

  var values: seq[T] = @[]
  for i in indices: values.add data[i]
  sort values, cmp[T]

  for j, i in indices: data[i] = values[j]

var d = @[7, 6, 5, 4, 3, 2, 1, 0]
sortDisjoinSublist(d, @[6, 1, 7])
echo d
```

Output:

```txt
@[7, 0, 5, 4, 3, 2, 1, 6]
```


=={{header|Objective-C}}==
{{trans|Go}}
Sorts an array "wrapper" which represents a "view" into the disjoint sublist of the array.

```objc>#import <Foundation/Foundation.h


@interface DisjointSublistView : NSMutableArray {
  NSMutableArray *array;
  int *indexes;
  int num_indexes;
}
- (instancetype)initWithArray:(NSMutableArray *)a andIndexes:(NSIndexSet *)ind;
@end

@implementation DisjointSublistView
- (instancetype)initWithArray:(NSMutableArray *)a andIndexes:(NSIndexSet *)ind {
  if ((self = [super init])) {
    array = a;
    num_indexes = [ind count];
    indexes = malloc(num_indexes * sizeof(int));
    for (NSUInteger i = [ind firstIndex], j = 0; i != NSNotFound; i = [ind indexGreaterThanIndex:i], j++)
      indexes[j] = i;
  }
  return self;
}
- (void)dealloc {
  free(indexes);
}
- (NSUInteger)count { return num_indexes; }
- (id)objectAtIndex:(NSUInteger)i { return array[indexes[i]]; }
- (void)replaceObjectAtIndex:(NSUInteger)i withObject:(id)x { array[indexes[i]] = x; }
@end

@interface NSMutableArray (SortDisjoint)
- (void)sortDisjointSublist:(NSIndexSet *)indexes usingSelector:(SEL)comparator;
@end
@implementation NSMutableArray (SortDisjoint)
- (void)sortDisjointSublist:(NSIndexSet *)indexes usingSelector:(SEL)comparator {
  DisjointSublistView *d = [[DisjointSublistView alloc] initWithArray:self andIndexes:indexes];
  [d sortUsingSelector:comparator];
}
@end

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSMutableArray *a = [@[@7, @6, @5, @4, @3, @2, @1, @0] mutableCopy];
    NSMutableIndexSet *ind = [NSMutableIndexSet indexSet];
    [ind addIndex:6]; [ind addIndex:1]; [ind addIndex:7];
    [a sortDisjointSublist:ind usingSelector:@selector(compare:)];
    NSLog(@"%@", a);

  }
  return 0;
}
```

{{out}}

```txt
(
    7,
    0,
    5,
    4,
    3,
    2,
    1,
    6
)
```



## OCaml


With arrays:


```ocaml
let disjoint_sort cmp values indices =
  let temp = Array.map (Array.get values) indices in
  Array.sort cmp temp;
  Array.sort compare indices;
  Array.iteri (fun i j -> values.(j) <- temp.(i)) indices

let () =
  let values = [| 7; 6; 5; 4; 3; 2; 1; 0 |]
  and indices = [| 6; 1; 7 |] in
  disjoint_sort compare values indices;
  Array.iter (Printf.printf " %d") values;
  print_newline()
```


With lists:


```ocaml
let disjoint_sort cmp values indices =
  let indices = List.sort compare indices in
  let rec aux acc j = function
    | (i::iq), (v::vq) when i = j ->
        aux (v::acc) (succ j) (iq, vq)
    | [], _ -> acc
    | il, (_::vq) ->
        aux acc (succ j) (il, vq)
    | _, [] ->
        invalid_arg "index out of bounds"
  in
  let temp = aux [] 0 (indices, values) in
  let temp = List.sort cmp temp in
  let rec aux acc j = function
    | (i::iq), (_::vq), (r::rq) when i = j ->
        aux (r::acc) (succ j) (iq, vq, rq)
    | [], vl, _ ->
        List.rev_append acc vl
    | il, (v::vq), rl ->
        aux (v::acc) (succ j) (il, vq, rl)
    | (_::_, [], _) ->
        assert false
  in
  aux [] 0 (indices, values, temp)

let () =
  let values = [ 7; 6; 5; 4; 3; 2; 1; 0 ]
  and indices = [ 6; 1; 7 ] in
  let res = disjoint_sort compare values indices in
  List.iter (Printf.printf " %d") res;
  print_newline()
```



## ooRexx


```ooRexx
data = .array~of(7, 6, 5, 4, 3, 2, 1, 0)
-- this could be a list, array, or queue as well because of polymorphism
-- also, ooRexx arrays are 1-based, so using the alternate index set for the
-- problem.
indexes = .set~of(7, 2, 8)
call disjointSorter data, indexes

say "Sorted data is: ["data~toString("l", ", ")"]"

::routine disjointSorter
  use arg data, indexes
  temp = .array~new(indexes~items)
  -- we want to process these in a predictable order, so make an array
  tempIndexes = indexes~makearray
  -- we can't just assign things back in the same order.  The expected
  -- result requires the items be inserted back in first-to-last index
  -- order, so we need to sort the index values too
  tempIndexes~sortWith(.numberComparator~new)
  do index over tempIndexes
     temp~append(data[index])
  end
  -- sort as numbers
  temp~sortwith(.numberComparator~new)

  do i = 1 to tempIndexes~items
     data[tempIndexes[i]] = temp[i]
  end

-- a custom comparator that sorts strings as numeric values rather than
-- strings
::class numberComparator subclass comparator
::method compare
  use strict arg left, right
  -- perform the comparison on the names.  By subtracting
  -- the two and returning the sign, we give the expected
  -- results for the compares
  return (left - right)~sign
```

{{out}}

```txt
Sorted data is: [7, 0, 5, 4, 3, 2, 1, 6]
```



## Order


```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8sort_disjoint_sublist ORDER_PP_FN(              \
8fn(8L, 8I,                                                           \
    8lets((8I, 8seq_sort(8less, 8tuple_to_seq(8I)))                   \
          (8J,                                                        \
           8seq_sort(8less, 8seq_map(8fn(8X, 8seq_at(8X, 8L)), 8I))), \
          8replace(8L, 8I, 8J))) )

#define ORDER_PP_DEF_8replace ORDER_PP_FN(                   \
8fn(8L, 8I, 8V,                                              \
    8if(8is_nil(8I),                                         \
        8L,                                                  \
        8replace(8seq_set(8seq_head(8I), 8L, 8seq_head(8V)), \
                 8seq_tail(8I), 8seq_tail(8V)))) )

ORDER_PP(
  8sort_disjoint_sublist(8seq(7, 6, 5, 4, 3, 2, 1, 0), 8tuple(6, 1, 7))
)
```



## PARI/GP


```parigp
sortsome(v,which)={
  my(x=sum(i=1,#which,1<<(which[i]-1)),u=vecextract(v,x));
  u=vecsort(u);
  which=vecsort(which);
  for(i=1,#which,v[which[i]]=u[i]);
  v
};
```



## Perl


```Perl
#!/usr/bin/perl -w
use strict ;

# this function sorts the array in place
sub disjointSort {
   my ( $values , @indices ) = @_ ;

   @{$values}[ sort @indices ] = sort @{$values}[ @indices ] ;
}

my @values =  ( 7 , 6 , 5 , 4 , 3 , 2 , 1 , 0 ) ;
my @indices = ( 6 , 1 , 7 ) ;
disjointSort( \@values , @indices ) ;
print "[@values]\n" ;
```

Output:

```txt
[7 0 5 4 3 2 1 6]
```



## Perl 6

{{works with|Rakudo|2018.03}}


### Inline

Using L-value slice of the array, and `sort` as a mutating method:

```perl6
my @values  = 7, 6, 5, 4, 3, 2, 1, 0;
my @indices = 6, 1, 7;

@values[ @indices.sort ] .= sort;

@values.perl.say;
```

Output:
```txt
[7, 0, 5, 4, 3, 2, 1, 6]
```



### Iterative


```perl6
sub disjointSort( @values, @indices --> List ) {
   my @sortedValues = @values[ @indices ].sort ;
   for @indices.sort -> $insert {
      @values[ $insert ] = @sortedValues.shift ;
   }
   return @values ;
}

my @values = ( 7 , 6 , 5 , 4 , 3 , 2 , 1 , 0 ) ;
my @indices = ( 6 , 1 , 7 ) ;
my @sortedValues = disjointSort( @values , @indices ) ;
@sortedValues.perl.say ;
```

Output:

```txt
[7, 0, 5, 4, 3, 2, 1, 6]
```



## Phix

Lightly modified copy of [[Sort_disjoint_sublist#Euphoria|Euphoria]]

```Phix
function uniq(sequence s)
integer last=s[1], this, ndx = 1
    for i=2 to length(s) do
        this = s[i]
        if this!=last then
            ndx += 1
            s[ndx] = this
            last = this
        end if
    end for
    return s[1..ndx]
end function

function disjoint_sort(sequence s, sequence idx)
sequence copies
    if length(idx)>1 then
        idx = uniq(sort(idx))
        copies = repeat(0, length(idx))
        for i=1 to length(idx) do
            copies[i] = s[idx[i]]
        end for
        copies = sort(copies)
        for i=1 to length(idx) do
            s[idx[i]] = copies[i]
        end for
    end if
    return s
end function

?disjoint_sort({7,6,5,4,3,2,1,0},{7,2,8})
```

{{out}}

```txt

{7,0,5,4,3,2,1,6}

```



## PicoLisp

The indices are incremented here, as PicoLisp is 1-based

```PicoLisp
(let (Values (7 6 5 4 3 2 1 0)  Indices (7 2 8))
   (mapc
      '((V I) (set (nth Values I) V))
      (sort (mapcar '((N) (get Values N)) Indices))
      (sort Indices) )
   Values )
```

Output:

```txt
-> (7 0 5 4 3 2 1 6)
```



## PowerShell


{{works with|PowerShell|4.0}}


```PowerShell

function sublistsort($values, $indices) {
   $indices = $indices | sort
   $sub, $i = ($values[$indices] | sort), 0
   $indices | foreach { $values[$_] = $sub[$i++] }
   $values
}
$values = 7, 6, 5, 4, 3, 2, 1, 0
$indices = 6, 1, 7
"$(sublistsort $values $indices)"

```

<b>Output:</b>

```txt

7 0 5 4 3 2 1 6

```



## PureBasic

Based on the C implementation

```PureBasic
Procedure Bubble_sort(Array idx(1), n, Array buf(1))
  Protected i, j
  SortArray(idx(),#PB_Sort_Ascending)
  For i=0 To n
    For j=i+1 To n
      If buf(idx(j)) < buf(idx(i))
        Swap buf(idx(j)), buf(idx(i))
      EndIf
    Next
  Next
EndProcedure

Procedure main()
  DataSection
    values: Data.i 7, 6, 5, 4, 3, 2, 1, 0
    indices:Data.i 6, 1, 7
  EndDataSection

  Dim values.i(7) :CopyMemory(?values, @values(), SizeOf(Integer)*8)
  Dim indices.i(2):CopyMemory(?indices,@indices(),SizeOf(Integer)*3)

  If OpenConsole()
    Protected i
    PrintN("Before sort:")
    For i=0 To ArraySize(values())
      Print(Str(values(i))+" ")
    Next

    PrintN(#CRLF$+#CRLF$+"After sort:")
    Bubble_sort(indices(), ArraySize(indices()), values())
    For i=0 To ArraySize(values())
      Print(Str(values(i))+" ")
    Next

    Print(#CRLF$+#CRLF$+"Press ENTER to exit")
    Input()
  EndIf
EndProcedure

main()
```


```txt
Before sort:
7 6 5 4 3 2 1 0

After sort:
7 0 5 4 3 2 1 6
```



## Python

The function modifies the input data list in-place and follows the Python convention of returning None in such cases.


```python>>>
 def sort_disjoint_sublist(data, indices):
	indices = sorted(indices)
	values  = sorted(data[i] for i in indices)
	for index, value in zip(indices, values):
		data[index] = value


>>> d = [7, 6, 5, 4, 3, 2, 1, 0]
>>> i = set([6, 1, 7])
>>> sort_disjoint_sublist(d, i)
>>> d
[7, 0, 5, 4, 3, 2, 1, 6]
>>> # Which could be more cryptically written as:
>>> def sort_disjoint_sublist(data, indices):
	for index, value in zip(sorted(indices), sorted(data[i] for i in indices)): data[index] = value


>>>
```



Or, checking a dictionary for sublist indices, and returning a new (rather than mutated) list:

```python
'''Disjoint sublist sorting'''


# disjointSort :: [Int] -> [Int] -> [Int]
def disjointSort(ixs):
    '''A copy of the list xs, in which the disjoint sublist
       of items at zero-based indexes ixs is sorted in a default
       numeric or lexical order.'''
    def go(xs):
        ks = sorted(ixs)
        dct = dict(zip(ks, sorted(xs[k] for k in ks)))
        return list(dct[i] if i in dct else x for i, x in enumerate(xs))
    return lambda xs: go(xs)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tests'''

    print(
        tabulated('Disjoint sublists at indices [6, 1, 7] sorted:\n')
        (str)(str)(
            disjointSort([6, 1, 7])
        )([
            [7, 6, 5, 4, 3, 2, 1, 0],
            ['h', 'g', 'f', 'e', 'd', 'c', 'b', 'a']
        ])
    )


# Generic functions for test and display ------------------


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Function composition.'''
    return lambda f: lambda x: g(f(x))


# tabulated :: String -> (a -> String) ->
#                        (b -> String) ->
#                        (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function -> fx display function ->
                f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join(
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        )
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Disjoint sublists at indices [6, 1, 7] sorted:

                [7, 6, 5, 4, 3, 2, 1, 0] -> [7, 0, 5, 4, 3, 2, 1, 6]
['h', 'g', 'f', 'e', 'd', 'c', 'b', 'a'] -> ['h', 'a', 'f', 'e', 'd', 'c', 'b', 'g']
```



## R

R lets you access elements of vectors with a vector of indices.


```R
 values=c(7,6,5,4,3,2,1,0)
 indices=c(7,2,8)
 values[sort(indices)]=sort(values[indices])
 print(values)
```

Output:

```txt
 7 0 5 4 3 2 1 6
```



## Racket


```Racket

#lang racket

(define (sort-disjoint l is)
  (define xs
    (sort (for/list ([x l] [i (in-naturals)] #:when (memq i is)) x) <))
  (let loop ([l l] [i 0] [xs xs])
    (cond [(null? l) l]
          [(memq i is) (cons (car xs) (loop (cdr l) (add1 i) (cdr xs)))]
          [else        (cons (car l)  (loop (cdr l) (add1 i) xs))])))

(sort-disjoint '(7 6 5 4 3 2 1 0) '(6 1 7))
;; --> '(7 0 5 4 3 2 1 6)

```



## REXX

Duplicate entries in the index list aren't destructive or illegal.


Note that the list may contain numbers in any form (integer, floating point, exponentationed),

as well as alphabetic/alphanumeric/non-displayable characters.

The REXX language normally uses a one-based index.

```rexx
/*REXX program uses a   disjointed sublist   to  sort  a  random list  of values.       */
parse arg old ',' idx                            /*obtain the optional lists from the CL*/
if old=''  then old= 7 6 5 4 3 2 1 0             /*Not specified:  Then use the default.*/
if idx=''  then idx= 7 2 8                       /* "      "         "   "   "     "    */
say '  list of indices:'  idx;   say             /*    [↑]  is for one─based lists.     */
say '    unsorted list:'  old                    /*display the  old  list of numbers.   */
say '      sorted list:'  disjoint_sort(old,idx) /*sort 1st list using 2nd list indices.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
disjoint_sort: procedure;    parse arg x,ix;   y=;    z=;    p= 0
               ix= sortL(ix)                           /*ensure the index list is sorted*/
                    do i=1  for  words(ix)             /*extract indexed values from  X.*/
                    z= z   word(x, word(ix, i) )       /*pick the correct value from  X.*/
                    end   /*j*/
               z= sortL(z)                             /*sort extracted (indexed) values*/
                    do m=1  for words(x)               /*re─build (re-populate)  X list.*/
                    if wordpos(m, ix)==0  then y=y  word(x,m)    /*is the same  or  new?*/
                                          else do;  p= p + 1;        y= y word(z, p)
                                               end
                    end   /*m*/
               return strip(y)
/*──────────────────────────────────────────────────────────────────────────────────────*/
sortL: procedure; parse arg L;    n= words(L);       do j=1  for n;        @.j= word(L,j)
                                                     end   /*j*/
         do k=1  for n-1                               /*sort a list using a slow method*/
           do m=k+1  to n;   if @.m<@.k  then parse value   @.k  @.m    with  @.m  @.k
           end   /*m*/
         end     /*k*/                                 /* [↑]  use  PARSE  for swapping.*/
       $= @.1;               do j=2  to n;   $= $ @.j
                             end   /*j*/
       return $
```

{{out|output|text=  when using the default inputs:}}

```txt

  list of indices: 7 2 8

    unsorted list: 7 6 5 4 3 2 1 0
      sorted list: 7 0 5 4 3 2 1 6

```



## Ruby

By convention, the exlamation mark in the method name indicates that something potentially dangerous can happen.  (In this case, the in place modification).

```ruby
def sort_disjoint_sublist!(ar, indices)
  values = ar.values_at(*indices).sort
  indices.sort.zip(values).each{ |i,v| ar[i] = v }
  ar
end

values = [7, 6, 5, 4, 3, 2, 1, 0]
indices = [6, 1, 7]
p sort_disjoint_sublist!(values, indices)
```

Output

```txt
[7, 0, 5, 4, 3, 2, 1, 6]
```




## Run BASIC

Normally we sort with SQLite in memory. Faster and less code

```runbasic
sortData$ = "7, 6, 5, 4, 3, 2, 1, 0"
sortIdx$  = "7, 2, 8"

numSort = 8
dim sortData(numSort)
for i = 1 to numSort
   sortData(i) = val(word$(sortData$,i,","))
next i

while word$(sortIdx$,s + 1) <> ""
  s   = s + 1
  idx = val(word$(sortIdx$,s))
  gosub [bubbleSort]
wend
end

[bubbleSort]
sortSw = 1
while sortSw = 1
 sortSw = 0
 for i = idx to numSort - 1              ' start sorting at idx
  if sortData(i) > sortData(i+1) then
     sortSw        = 1
     sortHold      = sortData(i)
     sortData(i)   = sortData(i+1)
     sortData(i+1) = sortHold
  end if
 next i
wend
RETURN
```



## Scala

{{libheader|Scala}}
```scala
import scala.compat.Platform

object SortedDisjointSubList extends App {
  val (list, subListIndex) = (List(7, 6, 5, 4, 3, 2, 1, 0), List(6, 1, 7))

  def sortSubList[T: Ordering](indexList: List[Int], list: List[T]) = {
    val subListIndex = indexList.sorted
    val sortedSubListMap = subListIndex.zip(subListIndex.map(list(_)).sorted).toMap

    list.zipWithIndex.map { case (value, index) =>
      if (sortedSubListMap.isDefinedAt(index)) sortedSubListMap(index) else value
    }
  }

  assert(sortSubList(subListIndex, list) == List(7, 0, 5, 4, 3, 2, 1, 6), "Incorrect sort")
  println(s"List in sorted order.\nSuccessfully completed without errors. [total ${Platform.currentTime - executionStart} ms]")
}
```



## Scheme

{{works with|Gauche Scheme}}

```Scheme
(use gauche.sequence)
(define num-list '(7 6 5 4 3 2 1 0))
(define indices '(6 1 7))
(define table
  (alist->hash-table
    (map cons
      (sort indices)
      (sort indices < (lambda (x) (~ num-list x))))))

(map last
  (sort
    (map-with-index
      (lambda (i x) (list (hash-table-get table i i) x))
      num-list)
    <
    car))
```

{{output}}

```txt

(7 0 5 4 3 2 1 6)

```



## Sidef


```ruby
func disjointSort(values, indices) {
    values[indices.sort] = [values[indices]].sort...
}

var values =  [7, 6, 5, 4, 3, 2, 1, 0];
var indices = [6, 1, 7];

disjointSort(values, indices);
say values;
```

{{out}}

```txt
[7, 0, 5, 4, 3, 2, 1, 6]
```



## Standard ML

{{works with|SML/NJ}}
{{trans|Go}}

```sml
functor SortDisjointFn (A : MONO_ARRAY) : sig
    val sort : (A.elem * A.elem -> order) -> (A.array * int array) -> unit
  end = struct

    structure DisjointView : MONO_ARRAY = struct
      type elem = A.elem
      type array = A.array * int array
      fun length (a, s) = Array.length s
      fun sub ((a, s), i) = A.sub (a, Array.sub (s, i))
      fun update ((a, s), i, x) = A.update (a, Array.sub (s, i), x)

      (* dummy implementations for not-needed functions *)
      type vector = unit
      val maxLen = Array.maxLen
      fun array _ = raise Domain
      fun fromList _ = raise Domain
      fun tabulate _ = raise Domain
      fun vector _ = raise Domain
      fun copy _ = raise Domain
      fun copyVec _ = raise Domain
      fun appi _ = raise Domain
      fun app _ = raise Domain
      fun modifyi _ = raise Domain
      fun modify _ = raise Domain
      fun foldli _ = raise Domain
      fun foldl _ = raise Domain
      fun foldri _ = raise Domain
      fun foldr _ = raise Domain
      fun findi _ = raise Domain
      fun find _ = raise Domain
      fun exists _ = raise Domain
      fun all _ = raise Domain
      fun collate _ = raise Domain
    end

    structure DisjointViewSort = ArrayQSortFn (DisjointView)

    fun sort cmp (arr, indices) = (
      ArrayQSort.sort Int.compare indices;
      DisjointViewSort.sort cmp (arr, indices)
    )
  end
```

Usage:

```txt
- structure IntArray = struct
=   open Array
=   type elem = int
=   type array = int Array.array
=   type vector = int Vector.vector
= end;
structure IntArray :
  sig
[ ... rest omitted ]
- structure IntSortDisjoint = SortDisjointFn (IntArray);
structure IntSortDisjoint :
  sig val sort : (A.elem * A.elem -> order) -> A.array * int array -> unit end
- val a = Array.fromList [7, 6, 5, 4, 3, 2, 1, 0];
val a = [|7,6,5,4,3,2,1,0|] : int array
- val indices = Array.fromList [6, 1, 7];
val indices = [|6,1,7|] : int array
- IntSortDisjoint.sort Int.compare (a, indices);
val it = () : unit
- a;
val it = [|7,0,5,4,3,2,1,6|] : int array
```



## Swift

{{trans|Go}}
Sorts an array "wrapper" which represents a "view" into the disjoint sublist of the array.

```swift>struct DisjointSublistView<T
 : MutableCollectionType {
  let array : UnsafeMutablePointer<T>
  let indexes : [Int]

  subscript (position: Int) -> T {
    get {
      return array[indexes[position]]
    }
    set {
      array[indexes[position]] = newValue
    }
  }
  var startIndex : Int { return 0 }
  var endIndex : Int { return indexes.count }
  func generate() -> IndexingGenerator<DisjointSublistView<T>> { return IndexingGenerator(self) }
}

func sortDisjointSublist<T : Comparable>(inout array: [T], indexes: [Int]) {
  var d = DisjointSublistView(array: &array, indexes: sorted(indexes))
  sort(&d)
}

var a = [7, 6, 5, 4, 3, 2, 1, 0]
let ind = [6, 1, 7]
sortDisjointSublist(&a, ind)
println(a)
```

{{out}}

```txt
[7, 0, 5, 4, 3, 2, 1, 6]
```



## Tcl

This returns the sorted copy of the list; this is idiomatic for Tcl programs where values are immutable.

```tcl
package require Tcl 8.5
proc disjointSort {values indices args} {
    # Ensure that we have a unique list of integers, in order
    # We assume there are no end-relative indices
    set indices [lsort -integer -unique $indices]
    # Map from those indices to the values to sort
    set selected {}
    foreach i $indices {lappend selected [lindex $values $i]}
    # Sort the values (using any extra options) and write back to the list
    foreach i $indices v [lsort {*}$args $selected] {
	lset values $i $v
    }
    # The updated list is the result
    return $values
}
```

Demonstration:

```tcl
set values {7 6 5 4 3 2 1 0}
set indices {6 1 7}
puts \[[join [disjointSort $values $indices] ", "]\]
```

Output:

```txt
[7, 0, 5, 4, 3, 2, 1, 6]
```


## TUSCRIPT

TUSCRIPT indexing is one based

```tuscript

$$ MODE TUSCRIPT
values="7'6'5'4'3'2'1'0"
indices="7'2'8"
v_unsorted=SELECT (values,#indices)
v_sort=DIGIT_SORT (v_unsorted)
i_sort=DIGIT_SORT (indices)
LOOP i=i_sort,v=v_sort
values=REPLACE (values,#i,v)
ENDLOOP
PRINT values

```

Output:

```txt

7'0'5'4'3'2'1'6

```



## Ursala


```Ursala
#import std
#import nat

disjoint_sort = ^|(~&,num); ("i","v"). (-:(-:)"v"@p nleq-<~~lSrSX ~&rlPlw~|/"i" "v")*lS "v"

#cast %nL

t = disjoint_sort({6,1,7},<7,6,5,4,3,2,1,0>)
```

output:

```txt
<7,0,5,4,3,2,1,6>
```



## zkl


```zkl
values :=T(7, 6, 5, 4, 3, 2, 1, 0);
indices:=T(6, 1, 7);

indices.apply(values.get).sort() // a.get(0) == a[0]
  .zip(indices.sort()) //-->(v,i) == L(L(0,1),L(1,6),L(6,7))
  .reduce(fcn(newList,[(v,i)]){ newList[i]=v; newList },values.copy())
  .println();  // new list
```

This is an create-new-object version. An in place version is almost identical:

```zkl
values :=L(7, 6, 5, 4, 3, 2, 1, 0);

indices.apply(values.get).sort() // a.get(0) == a[0]
  .zip(indices.sort()) //-->(v,i) == L(L(0,1),L(1,6),L(6,7))
  .apply2(fcn([(v,i)],list){ list[i]=v },values);

values.println();  // modified list
```

{{out}}
```txt
L(7,0,5,4,3,2,1,6)
```

