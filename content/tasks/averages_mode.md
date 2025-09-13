+++
title = "Averages/Mode"
description = ""
date = 2019-10-02T10:04:28Z
aliases = []
[extra]
id = 4377
[taxonomies]
categories = ["Probability and statistics", "task"]
tags = []
+++

## Task

Write a program to find the [[wp:Mode (statistics)|mode]] value of a collection.

The case where the collection is empty may be ignored. Care must be taken to handle the case where the mode is non-unique.

If it is not appropriate or possible to support a general collection, use a vector (array), if possible. If it is not appropriate or possible to support an unspecified value type, use integers.

{{task heading|See also}}

{{Related tasks/Statistical measures}}

<hr>


## 11l

{{trans|Python}}

```11l
F modes(values)
   DefaultDict[Int, Int] count
   L(v) values
      count[v]++
   V best = max(count.values())
   R count.filter(kv -> kv[1] == @best).map(kv -> kv[0])

print(modes([1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17]))
print(modes([1, 1, 2, 4, 4]))
```

{{out}}

```txt

[6]
[1, 4]

```


=={{Header|ActionScript}}==
This implementation does not work with arbitrary collections. However, it works with arrays containing mixed data, including strings and other arrays.

```ActionScript
function Mode(arr:Array):Array {
	//Create an associative array to count how many times each element occurs,
	//an array to contain the modes, and a variable to store how many times each mode appears.
	var count:Array = new Array();
	var modeList:Array;
	var maxCount:uint=0;
	for (var i:String in arr) {
		//Record how many times an element has occurred. Note that each element in the cuont array
		//has to be initialized explicitly, since it is an associative array.
		if (count[arr[i]]==undefined) {
			count[arr[i]]=1;
		} else {
			count[arr[i]]++;
		}
		//If this is now the most common element, clear the list of modes, and add this element.
		if(count[arr[i]] > maxCount)
		{
			maxCount=count[arr[i]];
			modeList = new Array();
			modeList.push(arr[i]);
		}
		//If this is a mode, add it to the list.
		else if(count[arr[i]] == maxCount){
			modeList.push(arr[i]);
		}
	}
	return modeList;
}
```


=={{Header|Ada}}==
{{works with|Ada 2005}}
mode.ads:

```Ada
generic
   type Element_Type is private;
   type Element_Array is array (Positive range <>) of Element_Type;
package Mode is

   function Get_Mode (Set : Element_Array) return Element_Array;

end Mode;
```

mode.adb:

```Ada
with Ada.Containers.Indefinite_Vectors;

package body Mode is

   -- map Count to Elements
   package Count_Vectors is new Ada.Containers.Indefinite_Vectors
     (Element_Type => Element_Array,
      Index_Type => Positive);

   procedure Add (To : in out Count_Vectors.Vector; Item : Element_Type) is
      use type Count_Vectors.Cursor;
      Position : Count_Vectors.Cursor := To.First;
      Found    : Boolean              := False;
   begin
      while not Found and then Position /= Count_Vectors.No_Element loop
         declare
            Elements : Element_Array := Count_Vectors.Element (Position);
         begin
            for I in Elements'Range loop
               if Elements (I) = Item then
                  Found := True;
               end if;
            end loop;
         end;
         if not Found then
            Position := Count_Vectors.Next (Position);
         end if;
      end loop;
      if Position /= Count_Vectors.No_Element then
         -- element found, remove it and insert to next count
         declare
            New_Position : Count_Vectors.Cursor :=
               Count_Vectors.Next (Position);
         begin
            -- remove from old position
            declare
               Old_Elements : Element_Array :=
                  Count_Vectors.Element (Position);
               New_Elements : Element_Array (1 .. Old_Elements'Length - 1);
               New_Index    : Positive      := New_Elements'First;
            begin
               for I in Old_Elements'Range loop
                  if Old_Elements (I) /= Item then
                     New_Elements (New_Index) := Old_Elements (I);
                     New_Index                := New_Index + 1;
                  end if;
               end loop;
               To.Replace_Element (Position, New_Elements);
            end;
            -- new position not already there?
            if New_Position = Count_Vectors.No_Element then
               declare
                  New_Array : Element_Array (1 .. 1) := (1 => Item);
               begin
                  To.Append (New_Array);
               end;
            else
               -- add to new position
               declare
                  Old_Elements : Element_Array :=
                     Count_Vectors.Element (New_Position);
                  New_Elements : Element_Array (1 .. Old_Elements'Length + 1);
               begin
                  New_Elements (1 .. Old_Elements'Length) := Old_Elements;
                  New_Elements (New_Elements'Last)        := Item;
                  To.Replace_Element (New_Position, New_Elements);
               end;
            end if;
         end;
      else
         -- element not found, add to count 1
         Position := To.First;
         if Position = Count_Vectors.No_Element then
            declare
               New_Array : Element_Array (1 .. 1) := (1 => Item);
            begin
               To.Append (New_Array);
            end;
         else
            declare
               Old_Elements : Element_Array :=
                  Count_Vectors.Element (Position);
               New_Elements : Element_Array (1 .. Old_Elements'Length + 1);
            begin
               New_Elements (1 .. Old_Elements'Length) := Old_Elements;
               New_Elements (New_Elements'Last)        := Item;
               To.Replace_Element (Position, New_Elements);
            end;
         end if;
      end if;
   end Add;

   function Get_Mode (Set : Element_Array) return Element_Array is
      Counts : Count_Vectors.Vector;
   begin
      for I in Set'Range loop
         Add (Counts, Set (I));
      end loop;
      return Counts.Last_Element;
   end Get_Mode;

end Mode;
```

example use:

```Ada
with Ada.Text_IO;
with Mode;
procedure Main is
   type Int_Array is array (Positive range <>) of Integer;
   package Int_Mode is new Mode (Integer, Int_Array);

   Test_1 : Int_Array := (1, 2, 3, 1, 2, 4, 2, 5, 2, 3, 3, 1, 3, 6);
   Result : Int_Array := Int_Mode.Get_Mode (Test_1);
begin
   Ada.Text_IO.Put ("Input: ");
   for I in Test_1'Range loop
      Ada.Text_IO.Put (Integer'Image (Test_1 (I)));
      if I /= Test_1'Last then
         Ada.Text_IO.Put (",");
      end if;
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("Result:");
   for I in Result'Range loop
      Ada.Text_IO.Put (Integer'Image (Result (I)));
      if I /= Result'Last then
         Ada.Text_IO.Put (",");
      end if;
   end loop;
   Ada.Text_IO.New_Line;
end Main;
```

{{out}}

```txt
Input:  1, 2, 3, 1, 2, 4, 2, 5, 2, 3, 3, 1, 3, 6
Result: 2, 3
```


=={{Header|APL}}==

```APL
mode←{{s←⌈/⍵[;2]⋄⊃¨(↓⍵)∩{⍵,s}¨⍵[;1]}{⍺,≢⍵}⌸⍵}
```


=={{Header|AutoHotkey}}==
{{AutoHotkey case}}
Source: [http://www.autohotkey.com/forum/post-276175.html#276175 AutoHotkey forum] by Laszlo

```autohotkey
MsgBox % Mode("1 2 3")
MsgBox % Mode("1 2 0 3 0.0")
MsgBox % Mode("0.1 2.2 -0.1 0.22e1 2.20 0.1")

Mode(a, d=" ") { ; the number that occurs most frequently in a list delimited by d (space)
   Sort a, ND%d%
   Loop Parse, a, %d%
      If (V != A_LoopField) {
         If (Ct > MxCt)
            MxV := V, MxCt := Ct
         V := A_LoopField, Ct := 1
      }
      Else Ct++
   Return Ct>MxCt ? V : MxV
}
```



## AWK



```AWK
#!/usr/bin/gawk -f
{
	# compute histogram
	histo[$1] += 1;
};

function mode(HIS) {
    # Computes the mode from Histogram A
    max = 0;
    n = 0;
    for (k in HIS) {
	val = HIS[k];
	if (HIS[k] > max) {
	    max = HIS[k];
            n = 1;
	    List[n] = k;
	} else if (HIS[k] == max)	{
		List[++n] = k;
	}
    }

    for (k=1; k<=n; k++) {
        o = o""OFS""List[k];
    }
    return o;
}

END {
    print mode(histo);
};
```



```txt
cat modedata.txt
0
3
6
aa
3
6
aa
3
aa
6
7
1
as@mini10:~/src/RosettaCode$ ./mode.awk modedata.txt
 6 aa 3

```



## BBC BASIC


```bbcbasic
      DIM a(10), b(4)
      a() = 1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17
      b() = 1, 2, 4, 4, 1

      DIM modes(10)
      PRINT "Mode(s) of a() = " ;
      FOR i% = 1 TO FNmodes(a(), modes())
        PRINT ; modes(i%) " " ;
      NEXT
      PRINT
      PRINT "Mode(s) of b() = " ;
      FOR i% = 1 TO FNmodes(b(), modes())
        PRINT ; modes(i%) " " ;
      NEXT
      PRINT
      END

      DEF FNmodes(a(), m())
      LOCAL I%, J%, N%, c%(), max%
      N% = DIM(a(),1)
      IF N% = 0 THEN m(1) = a(0) : = 1
      DIM c%(N%)
      FOR I% = 0 TO N%-1
        FOR J% = I%+1 TO N%
          IF a(I%) = a(J%) c%(I%) += 1
        NEXT
        IF c%(I%) > max% max% = c%(I%)
      NEXT I%
      J% = 0
      FOR I% = 0 TO N%
        IF c%(I%) = max% J% += 1 : m(J%) = a(I%)
      NEXT
      = J%

```

{{out}}

```txt
Mode(s) of a() = 6
Mode(s) of b() = 1 4
```



## C

Using an array of doubles.  If another data type is desired, the <code>cmp_dbl</code> and <code>vcount</code> definitions should be changed accordingly.

```c
#include <stdio.h>
#include <stdlib.h>

typedef struct { double v; int c; } vcount;

int cmp_dbl(const void *a, const void *b)
{
	double x = *(const double*)a - *(const double*)b;
	return x < 0 ? -1 : x > 0;
}

int vc_cmp(const void *a, const void *b)
{
	return ((const vcount*)b)->c - ((const vcount*)a)->c;
}

int get_mode(double* x, int len, vcount **list)
{
	int i, j;
	vcount *vc;

	/* sort values */
	qsort(x, len, sizeof(double), cmp_dbl);

	/* count occurence of each value */
	for (i = 0, j = 1; i < len - 1; i++, j += (x[i] != x[i + 1]));

	*list = vc = malloc(sizeof(vcount) * j);
	vc[0].v = x[0];
	vc[0].c = 1;

	/* generate list value-count pairs */
	for (i = j = 0; i < len - 1; i++, vc[j].c++)
		if (x[i] != x[i + 1]) vc[++j].v = x[i + 1];

	/* sort that by count in descending order */
	qsort(vc, j + 1, sizeof(vcount), vc_cmp);

	/* the number of entries with same count as the highest */
	for (i = 0; i <= j && vc[i].c == vc[0].c; i++);

	return i;
}

int main()
{
	double values[] = { 1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 12, 12, 17 };
#	define len sizeof(values)/sizeof(double)
	vcount *vc;

	int i, n_modes = get_mode(values, len, &vc);

	printf("got %d modes:\n", n_modes);
	for (i = 0; i < n_modes; i++)
		printf("\tvalue = %g, count = %d\n", vc[i].v, vc[i].c);

	free(vc);
	return 0;
}
```

{{out}}

```txt
got 2 modes:
        value = 6, count = 4
        value = 12, count = 4
```



## C++

{{works with|g++|4.3.2}}

```cpp
#include <iterator>
#include <utility>
#include <algorithm>
#include <list>
#include <iostream>

// helper struct
template<typename T> struct referring
{
  referring(T const& t): value(t) {}
  template<typename Iter>
   bool operator()(std::pair<Iter, int> const& p) const
  {
    return *p.first == value;
  }
  T const& value;
};

// requires:
// FwdIterator is a ForwardIterator
// The value_type of FwdIterator is EqualityComparable
// OutIterator is an output iterator
// the value_type of FwdIterator is convertible to the value_type of OutIterator
// [first, last) is a valid range
// provides:
// the mode is written to result
template<typename FwdIterator, typename OutIterator>
 void mode(FwdIterator first, FwdIterator last, OutIterator result)
{
  typedef typename std::iterator_traits<FwdIterator>::value_type value_type;
  typedef std::list<std::pair<FwdIterator, int> > count_type;
  typedef typename count_type::iterator count_iterator;

  // count elements
  count_type counts;

  while (first != last)
  {
    count_iterator element = std::find_if(counts.begin(), counts.end(),
                                          referring<value_type>(*first));
    if (element == counts.end())
      counts.push_back(std::make_pair(first, 1));
    else
      ++element->second;
    ++first;
  }

  // find maximum
  int max = 0;
  for (count_iterator i = counts.begin(); i != counts.end(); ++i)
    if (i->second > max)
      max = i->second;

  // copy corresponding elements to output sequence
  for (count_iterator i = counts.begin(); i != counts.end(); ++i)
    if (i->second == max)
      *result++ = *i->first;
}

// example usage
int main()
{
  int values[] = { 1, 2, 3, 1, 2, 4, 2, 5, 2, 3, 3, 1, 3, 6 };
  median(values, values + sizeof(values)/sizeof(int),
         std::ostream_iterator<int>(std::cout, " "));
  std::cout << std::endl;
  return 0;
}
```

{{out}}
 2 3

## C#

```c#
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Test
{
    class Program
    {

        static void Main(string[] args)
        {
            /*
             * We Use Linq To Determine The Mode
             */
            List<int> myList = new List<int>() { 1, 1, 2, 4, 4 };

            var query =     from numbers in myList //select the numbers
                            group numbers by numbers //group them together so we can get the count
                            into groupedNumbers
                            select new { Number = groupedNumbers.Key, Count = groupedNumbers.Count() }; //so we got a query
            //find the max of the occurence of the mode
            int max = query.Max(g => g.Count);
            IEnumerable<int> modes = query.Where(x => x.Count == max).Select(x => x.Number);//match the frequence and select the number
            foreach (var item in modes)
            {
                Console.WriteLine(item);
            }

            Console.ReadLine();
        }



    }


}

```



## Clojure


```clojure
(defn modes [coll]
  (let [distrib (frequencies coll)
        [value freq] [first second] ; name the key/value pairs in the distrib (map) entries
        sorted (sort-by (comp - freq) distrib)
        maxfq (freq (first sorted))]
    (map value (take-while #(= maxfq (freq %)) sorted))))
```

Or a one-liner solution

```clojure
(defn modes [coll]
  (->> coll frequencies (sort-by val >) (partition-by val) first (map key)))
```


== {{header|CoffeeScript}} ==

```coffeescript
mode = (arr) ->
  # returns an array with the modes of arr, i.e. the
  # elements that appear most often in arr
  counts = {}
  for elem in arr
    counts[elem] ||= 0
    counts[elem] += 1
  max = 0
  for key, cnt of counts
    max = cnt if cnt > max
  (key for key, cnt of counts when cnt == max)

console.log mode [1, 2, 2, 2, 3, 3, 3, 4, 4]
```


== {{header|Common Lisp}} ==
The following returns a list of the modes of a sequence as the primary value, and the frequency as the secondary value.  E.g., <code>(mode '(a b c d a b c a b))</code> produces <code>(A B)</code> and <code>3</code>.  hash-table-options can be used to customize the hash table, e.g., to specify the test by which elements are compared.

```lisp
(defun mode (sequence &rest hash-table-options)
  (let ((frequencies (apply #'make-hash-table hash-table-options)))
    (map nil (lambda (element)
               (incf (gethash element frequencies 0)))
         sequence)
    (let ((modes '())
          (hifreq 0 ))
      (maphash (lambda (element frequency)
                 (cond ((> frequency hifreq)
                        (setf hifreq frequency
                              modes  (list element)))
                       ((= frequency hifreq)
                        (push element modes))))
               frequencies)
      (values modes hifreq))))
```



## D

The mode function returns a range of all the mode items:

```d
import std.stdio, std.algorithm, std.array;

auto mode(T)(T[] items) pure /*nothrow @safe*/ {
    int[T] aa;
    foreach (item; items)
        aa[item]++;
    immutable m = aa.byValue.reduce!max;
    return aa.byKey.filter!(k => aa[k] == m);
}

void main() /*@safe*/ {
    auto data = [1, 2, 3, 1, 2, 4, 2, 5, 3, 3, 1, 3, 6];
    writeln("Mode: ", data.mode);

    data ~= 2;
    writeln("Mode: ", data.mode);
}
```

{{out}}

```txt
Mode: [3]
Mode: [2, 3]
```



## E


```e
pragma.enable("accumulator")
def mode(values) {
    def counts := [].asMap().diverge()
    var maxCount := 0
    for v in values {
        maxCount max= (counts[v] := counts.fetch(v, fn{0}) + 1)
    }
    return accum [].asSet() for v => ==maxCount in counts { _.with(v) }
}
```


```e
? mode([1,1,2,2,3,3,4,4,4,5,5,6,6,7,8,8,9,9,0,0,0])
# value: [4, 0].asSet()
```

In the line "<code>maxCount max= (counts[v] := counts.fetch(v, fn{0}) + 1)</code>", <code>max=</code> is an update-assignment operation like <code>+=</code>. (The parentheses are unnecessary.) A more verbose version would be:

```e
  def newCount := counts.fetch(v, fn { 0 }) + 1
  counts[v] := newCount
  maxCount := maxCount.max(newCount)
```

In for loops, each key and value from the collection are [[Pattern Matching|pattern matched]] against the specified <code><var>key pattern</var> => <var>value pattern</var></code>. In "<code>for v => ==maxCount in counts</code>", the <code>==</code> is a pattern-match operator which fails unless the value examined is equal to the specified value; so this selects only the input values (keys in <code>counts</code>) whose counts are equal to the maximum count.


## EchoLisp


```scheme

(define (modes L)
	(define G (group* L)) ;; sorts and group equal items
	(define cardmax (for/max [(g G)] (length g)))
	(map first (filter (lambda(g) (= cardmax (length g))) G)))

(modes '( a b c a d e f))
    → (a)
(modes (iota 6))
    → (0 1 2 3 4 5)
(modes '(x))
    → (x)
(modes '(🎾 🏉 ☕️ 🎾 🎲 🎯 🎺 ☕️ 🎲 🎸 🎻 🏆 ☕️ 🏁 🎾 🎲 🎻 🏉 ))
    → (🎾 ☕️ 🎲)

(modes '())
😖️ error: group : expected list : null 🔎 'modes'

```



## Elena

ELENA 4.1:

```elena
import system'routines;
import system'collections;
import extensions;

extension op
{
    get Mode()
    {
        var countMap := Dictionary.new(0);
        self.forEach:(item)
        {
            countMap[item] := countMap[item] + 1
        };

        countMap := countMap.Values.sort:(p,n => p > n);

        var max := countMap.FirstMember;

        ^ countMap
            .filterBy:(kv => max.equal(kv.Value))
            .selectBy:(kv => kv.Key)
            .toArray()
    }
}

public program()
{
    var array1 := new int[]::(1, 1, 2, 4, 4);
    var array2 := new int[]::(1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17);
    var array3 := new ::(1, "blue", 2, 7.5r, 5, "green", "red", 5, 2, "blue", "white");

    console
        .printLine("mode of (",array1.asEnumerable(),") is (",array1.Mode,")")
        .printLine("mode of (",array2.asEnumerable(),") is (",array2.Mode,")")
        .printLine("mode of (",array3.asEnumerable(),") is (",array3.Mode,")")
        .readChar()
}
```

{{out}}

```txt

mode of (1,1,2,4,4) is (4,1)
mode of (1,3,6,6,6,6,7,7,12,12,17) is (6)
mode of (1,blue,2,7.5,5,green,red,5,2,blue,white) is (5,2,blue)

```



## Elixir


```elixir
defmodule Average do
  def mode(list) do
    gb = Enum.group_by(list, &(&1))
    max = Enum.map(gb, fn {_,val} -> length(val) end) |> Enum.max
    for {key,val} <- gb, length(val)==max, do: key
  end
end

lists = [[3,1,4,1,5,9,2,6,5,3,5,8,9],
         [1, 2, "qwe", "asd", 1, 2, "qwe", "asd", 2, "qwe"]]
Enum.each(lists, fn list ->
  IO.puts "mode: #{inspect list}"
  IO.puts "   => #{inspect Average.mode(list)}"
end)
```


{{out}}

```txt

mode: [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9]
   => [5]
mode: [1, 2, "qwe", "asd", 1, 2, "qwe", "asd", 2, "qwe"]
   => [2, "qwe"]

```



## Erlang


```Erlang

-module( mode ).
-export( [example/0, values/1] ).

example() ->
	Set = [1, 2, "qwe", "asd", 1, 2, "qwe", "asd", 2, "qwe"],
	io:fwrite( "In ~p the mode(s) is(are): ~p~n", [Set, values(Set)] ).

values( Set ) ->
	Dict = lists:foldl( fun values_count/2, dict:new(), Set ),
	[X || {X, _Y} <- dict:fold( fun keep_maxs/3, [{0, 0}], Dict )].



keep_maxs( Key, Value, [{_Max_key, Max_value} | _] ) when Value > Max_value ->
	[{Key, Value}];
keep_maxs( Key, Value, [{_Max_key, Max_value} | _]=Maxs ) when Value =:= Max_value ->
	[{Key, Value} | Maxs];
keep_maxs( _Key, _Value, Maxs ) ->
	Maxs.

values_count( Value, Dict ) -> dict:update_counter( Value, 1, Dict ).

```

{{out}}

```txt

12> mode:example().
In [1, 2, "qwe", "asd", 1, 2, "qwe", "asd", 2, "qwe"] the mode(s) is(are): ["qwe", 2]

```



## ERRE


```ERRE
PROGRAM MODE_AVG

!$INTEGER

DIM A[10],B[10],Z[10]

PROCEDURE SORT(Z[],P->Z[])
   LOCAL N,FLIPS
   FLIPS=TRUE
   WHILE FLIPS DO
      FLIPS=FALSE
      FOR N=0 TO P-1 DO
        IF Z[N]>Z[N+1] THEN SWAP(Z[N],Z[N+1]) FLIPS=TRUE
      END FOR
   END WHILE
END PROCEDURE

PROCEDURE CALC_MODE(Z[],P->MODES$)
   LOCAL I,OCCURRENCE,MAXOCCURRENCE,OLDVAL
   SORT(Z[],P->Z[])
   OCCURENCE=1
   MAXOCCURENCE=0
   OLDVAL=Z[0]
   MODES$=""
   FOR I=1 TO P DO
       IF Z[I]=OLDVAL THEN
           OCCURENCE=OCCURENCE+1
         ELSE
           IF OCCURENCE>MAXOCCURENCE THEN
                MAXOCCURENCE=OCCURENCE
                MODES$=STR$(OLDVAL)
             ELSIF OCCURENCE=MAXOCCURENCE THEN
                MODES$=MODES$+STR$(OLDVAL)
             ELSE
                !$NULL
           END IF
           OCCURENCE=1
       END IF
       OLDVAL=Z[I]
   END FOR
   !check after loop
   IF OCCURENCE>MAXOCCURENCE THEN
       MAXOCCURENCE=OCCURENCE
       MODES$=STR$(OLDVAL)
   ELSIF OCCURENCE=MAXOCCURENCE THEN
        MODES$=MODES$+STR$(OLDVAL)
   ELSE
       !$NULL
   END IF
END PROCEDURE

BEGIN
   A[]=(1,3,6,6,6,6,7,7,12,12,17)
   B[]=(1,2,4,4,1)
   PRINT("Modes for array A (1,3,6,6,6,6,7,7,12,12,17)";)
   CALC_MODE(A[],10->MODES$)
   PRINT(MODES$)
   PRINT("Modes for array B (1,2,4,4,1)";)
   CALC_MODE(B[],4->MODES$)
   PRINT(MODES$)
END PROGRAM
```

{{out}}

```txt

Modes for array A (1,3,6,6,6,6,7,7,12,12,17) 6
Modes for array B (1,2,4,4,1) 1 4

```


## Euphoria


```euphoria
include misc.e

function mode(sequence s)
    sequence uniques, counts, modes
    integer j,max
    uniques = {}
    counts = {}
    for i = 1 to length(s) do
        j = find(s[i], uniques)
        if j then
            counts[j] += 1
        else
            uniques = append(uniques, s[i])
            counts  = append(counts, 1)
        end if
    end for

    max = counts[1]
    for i = 2 to length(counts) do
        if counts[i] > max then
            max = counts[i]
        end if
    end for

    j = 1
    modes = {}
    while j <= length(s) do
        j = find_from(max, counts, j)
        if j = 0 then
            exit
        end if
        modes = append(modes, uniques[j])
        j += 1
    end while
    return modes
end function

constant s = { 1, "blue", 2, 7.5, 5, "green", "red", 5, 2, "blue", "white" }
pretty_print(1,mode(s),{3})
```

{{out}}

```txt
{
  "blue",
  2,
  5
}
```


=={{header|F_Sharp|F#}}==
The Unchecked.defaultof became available in version 1.9.4 I think.

```fsharp
let mode (l:'a seq) =
    l
    |> Seq.countBy (fun item -> item)               // Count individual items
    |> Seq.fold                                     // Find max counts
        (fun (cp, lst) (item, c) ->                 // State is (count, list of items with that count)
            if c > cp then (c, [item])              // New max - keep count and a list of the single item
            elif c = cp then (c, item :: lst)       // New element with max count - prepend it to the list
            else (cp,lst))                          // else just keep old count/list
        (0, [Unchecked.defaultof<'a>])              // Start with a count of 0 and a dummy item
    |> snd                                          // From (count, list) we just want the second item (the list)
```

Example usage:

```fsharp>
 mode ["a"; "b"; "c"; "c"];;
val it : string list = ["c"]
> mode ["a"; "b"; "c"; "c";"a"];;
val it : string list = ["c"; "a"]
> mode [1;2;1;3;2;0;0];;
val it : int list = [0; 2; 1]
```



## Factor

Factor has the word <code>mode</code> in <code>math.statistics</code> vocabulary.

```factor
{ 11 9 4 9 4 9 } mode ! 9
```



## Fortran

{{works with|Fortran|90 and later}}
For the <tt>Qsort_Module</tt> see [[Sorting_algorithms/Quicksort#Fortran]]

```fortran
program mode_test
  use Qsort_Module only Qsort => sort
  implicit none

  integer, parameter    :: S = 10
  integer, dimension(S) :: a1 = (/ -1, 7, 7, 2, 2, 2, -1, 7, -3, -3 /)
  integer, dimension(S) :: a2 = (/  1, 1, 1, 1, 1, 0, 2, 2, 2, 2 /)
  integer, dimension(S) :: a3 = (/  0, 0, -1, -1, 9, 9, 3, 3, 7, 7 /)

  integer, dimension(S) :: o
  integer               :: l, trash

  print *, stat_mode(a1)

  trash = stat_mode(a1, o, l)
  print *, o(1:l)
  trash = stat_mode(a2, o, l)
  print *, o(1:l)
  trash = stat_mode(a3, o, l)
  print *, o(1:l)

contains

  ! stat_mode returns the lowest (if not unique) mode
  ! others can hold other modes, if the mode is not unique
  ! if others is provided, otherslen should be provided too, and
  ! it says how many other modes are there.
  ! ok can be used to know if the return value has a meaning
  ! or the mode can't be found (void arrays)
  integer function stat_mode(a, others, otherslen, ok)
    integer, dimension(:), intent(in) :: a
    logical, optional, intent(out)    :: ok
    integer, dimension(size(a,1)), optional, intent(out) :: others
    integer, optional, intent(out)    :: otherslen

    ! ta is a copy of a, we sort ta modifying it, freq
    ! holds the frequencies and idx the index (for ta) so that
    ! the value appearing freq(i)-time is ta(idx(i))
    integer, dimension(size(a, 1)) :: ta, freq, idx
    integer                        :: rs, i, tm, ml, tf

    if ( present(ok) ) ok = .false.

    select case ( size(a, 1) )
    case (0)  ! no mode... ok is false
       return
    case (1)
       if ( present(ok) ) ok = .true.
       stat_mode = a(1)
       return
    case default
       if ( present(ok) ) ok = .true.
       ta = a         ! copy the array
       call sort(ta)  ! sort it in place (cfr. sort algos on RC)
       freq = 1
       idx = 0
       rs = 1         ! rs will be the number of different values

       do i = 2, size(ta, 1)
          if ( ta(i-1) == ta(i) ) then
             freq(rs) = freq(rs) + 1
          else
             idx(rs) = i-1
             rs = rs + 1
          end if
       end do
       idx(rs) = i-1

       ml = maxloc(freq(1:rs), 1)  ! index of the max value of freq
       tf = freq(ml)               ! the max frequency
       tm = ta(idx(ml))            ! the value with that freq

       ! if we want all the possible modes, we provide others
       if ( present(others) ) then
          i = 1
          others(1) = tm
          do
             freq(ml) = 0
             ml = maxloc(freq(1:rs), 1)
             if ( tf == freq(ml) ) then ! the same freq
                i = i + 1               ! as the max one
                others(i) = ta(idx(ml))
             else
                exit
             end if
          end do

          if ( present(otherslen) ) then
             otherslen = i
          end if

       end if
       stat_mode = tm
    end select

  end function stat_mode

end program mode_test
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub quicksort(a() As Integer, first As Integer, last As Integer)
  Dim As Integer length = last - first + 1
  If length < 2 Then Return
  Dim pivot As Integer = a(first + length\ 2)
  Dim lft As Integer = first
  Dim rgt As Integer = last
  While lft <= rgt
    While a(lft) < pivot
      lft +=1
    Wend
    While a(rgt) > pivot
      rgt -= 1
    Wend
    If lft <= rgt Then
       Swap a(lft), a(rgt)
       lft += 1
       rgt -= 1
    End If
  Wend
  quicksort(a(), first, rgt)
  quicksort(a(), lft, last)
End Sub

' The modal value(s) is/are stored in 'm'.
' The function returns the modal count.
Function mode(a() As Integer, m() As Integer, sorted As Boolean = false) As Integer
  Dim lb As Integer = LBound(a)
  Dim ub As Integer = UBound(a)
  If ub = -1 Then Return 0 '' empty array
  If Not sorted Then quicksort(a(), lb, ub)
  Dim cValue As Integer = a(lb)
  Dim cCount As Integer = 1
  Dim cMax As Integer = 0
  '' We iterate to the end of the array plus 1 to ensure the
  '' final value is dealt with properly
  For i As Integer = lb + 1 To ub + 1
    If i <= ub AndAlso a(i) = cValue Then
      cCount += 1
    Else
      If cCount > cMax Then
        Erase m
        Redim m(1 To 1)
        m(1) = cValue
        cMax = cCount
      ElseIf cCount = cMax Then
        Redim Preserve m(1 To UBound(m) + 1)
        m(UBound(m)) = cValue
      End If
      If i = ub + 1 Then Exit For
      cValue = a(i)
      cCount = 1
    End If
  Next
  Return cMax
End Function

Dim a(1 To 14) As Integer  = {1, 2, 3, 1, 2, 4, 2, 5, 2, 3, 3, 1, 3, 6}
Dim m() As Integer '' to store the mode(s)
Dim mCount As Integer = mode(a(), m())
Print "The following are the modes which occur"; mCount; " times : "
For i As Integer = LBound(m) To UBound(m) : Print m(i); " "; : Next
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

c:\FreeBasic>mode
The following are the modes which occur 4 times :
 2  3

```



## Haskell


```haskell
import Prelude (foldr, maximum, (==), (+))
import Data.Map (insertWith', empty, filter, elems, keys)

mode :: (Ord a) => [a] -> [a]
mode xs = keys (filter (== maximum (elems counts)) counts)
  where counts = foldr (\x -> insertWith' (+) x 1) empty xs
```

''counts'' is a map from each value found in ''xs'' to the number of occurrences (foldr traverses the list, insertWith' increments the count). This map is then filtered to only those entries whose count is the maximum count, and their keys (the values from the input list) are returned.
 > mode [1,2,3,3,2,1,1]
 [1]
 > mode [1,2,3,3,2,1]
 [1,2,3]
Alternately:

```haskell
import Data.List (group, sort)

mode :: (Ord a) => [a] -> [a]
mode xs = map fst $ filter ((==best).snd) counts
    where counts = map (\l -> (head l, length l)) . group . sort $ xs
          best = maximum (map snd counts)
```

Another version that does not require an orderable type:

```haskell
import Data.List (partition)

mode :: (Eq a) => [a] -> [a]
mode = snd . modesWithCount
    where modesWithCount :: (Eq a) => [a] -> (Int, [a])
          modesWithCount [] = (0,[])
          modesWithCount l@(x:_) | length xs > best = (length xs, [x])
                                 | length xs < best = (best, modes)
                                 | otherwise        = (best, x:modes)
            where (xs, notxs) = partition (== x) l
                  (best, modes) = modesWithCount notxs
```



## GAP


```gap
mode := function(v)
  local c, m;
  c := Collected(SortedList(v));
  m := Maximum(List(c, x -> x[2]));
  return List(Filtered(c, x -> x[2] = m), y -> y[1]);
end;

mode([ 7, 5, 6, 1, 5, 5, 7, 12, 17, 6, 6, 5, 12, 3, 6 ]);
# [ 5, 6 ]
```



## Go

'''Fixed collection type, fixed value type.''' In Go it is appropriate to program directly with built in types when possible.

```go
package main

import "fmt"

func main() {
    fmt.Println(mode([]int{2, 7, 1, 8, 2}))
    fmt.Println(mode([]int{2, 7, 1, 8, 2, 8}))
}

func mode(a []int) []int {
    m := make(map[int]int)
    for _, v := range a {
        m[v]++
    }
    var mode []int
    var n int
    for k, v := range m {
        switch {
        case v < n:
        case v > n:
            n = v
            mode = append(mode[:0], k)
        default:
            mode = append(mode, k)
        }
    }
    return mode
}
```

{{out}}

```txt

[2]
[2 8]

```

'''Fixed collection type, unspecified value type.'''  An empty interface can hold any type.  A slice <tt>[]interface</tt> can hold a mix of types. It's not too much more source code, although there is some overhead to support this generality.

```go
package main

import "fmt"

func main() {
    fmt.Println(mode([]interface{}{.2, .7, .1, .8, .2}))
    fmt.Println(mode([]interface{}{"two", 7, 1, 8, "two", 8}))
}

func mode(a []interface{}) []interface{} {
    m := make(map[interface{}]int)
    for _, v := range a {
        m[v]++
    }
    var mode []interface{}
    var n int
    for k, v := range m {
        switch {
        case v < n:
        case v > n:
            n = v
            mode = append(mode[:0], k)
        default:
            mode = append(mode, k)
        }
    }
    return mode
}
```

{{out}}

```txt

[0.2]
[two 8]

```

'''General collection, fixed value type.'''  The other kind of generality mentioned in the task requires more code.  In Go this is done with an interface to define generalized collection methods.  Here, the only method we need to demonstrate is iteration over the collection, so the interface has only one method.  Any number of types then can implement the interface.  Note that the mode function now takes an object of this interface type.  In effect, it becomes a generic function, oblivious to the implementation of the collection, and accessing it only through its methods.

```go
package main

import "fmt"

// interface type
type intCollection interface {
    iterator() func() (int, bool)
}

// concrete type implements interface
type intSlice []int

// method on concrete type satisfies interface method
func (s intSlice) iterator() func() (int, bool) {
    i := 0
    return func() (int, bool) {
        if i >= len(s) {
            return 0, false
        }
        v := s[i]
        i++
        return v, true
    }
}

func main() {
    fmt.Println(mode(intSlice{2, 7, 1, 8, 2}))
    fmt.Println(mode(intSlice{2, 7, 1, 8, 2, 8}))
}

// mode is now a generic function, in a sense.
// It knows what to do with an intCollection,
// but does not know the underlying concrete type.
func mode(a intCollection) []int {
    m := make(map[int]int)
    i := a.iterator()
    for {
        v, ok := i()
        if !ok {
            break
        }
        m[v]++
    }
    var mode []int
    var n int
    for k, v := range m {
        switch {
        case v < n:
        case v > n:
            n = v
            mode = append(mode[:0], k)
        default:
            mode = append(mode, k)
        }
    }
    return mode
}
```

{{out}}

```txt

[2]
[8 2]

```

'''General collection, unspecified value type,''' Finally, the two kinds of generality can be combined.  The iterator returned by the interface method now returns an empty interface rather than an int.  The intSlice concrete type of the previous example is retained, but now it must satisfy this interface method that uses <tt>interface{}</tt> instead of int.  <tt>runeList</tt> is added to illustrate how multiple types can satisfy the same interface.

```go
package main

import "fmt"

type collection interface {
    iterator() func() (interface{}, bool)
}

type intSlice []int

func (s intSlice) iterator() func() (interface{}, bool) {
    i := 0
    return func() (interface{}, bool) {
        if i >= len(s) {
            return 0, false
        }
        v := s[i]
        i++
        return v, true
    }
}

type runeList string

func (s runeList) iterator() func() (interface{}, bool) {
    c := make(chan rune)
    go func() {
        for _, r := range s {
            c <- r
        }
        close(c)
    }()
    return func() (interface{}, bool) {
        r, ok := <-c
        return string(r), ok
    }
}

func main() {
    fmt.Println(mode(intSlice{2, 7, 1, 8, 2}))
    fmt.Println(mode(runeList("Enzyklopädie")))
}

func mode(a collection) []interface{} {
    m := make(map[interface{}]int)
    i := a.iterator()
    for {
        v, ok := i()
        if !ok {
            break
        }
        m[v]++
    }
    var mode []interface{}
    var n int
    for k, v := range m {
        switch {
        case v < n:
        case v > n:
            n = v
            mode = append(mode[:0], k)
        default:
            mode = append(mode, k)
        }
    }
    return mode
}
```

{{out}}
("Enzyklopädie" has no repeated letters.  All are modal.)

```txt

[2]
[e d i k l o n p y z E ä]

```



## Groovy

Solution, both "collection type" and "element type" agnostic:

```groovy
def mode(Iterable col) {
    assert col
    def m = [:]
    col.each {
        m[it] = m[it] == null ? 1 : m[it] + 1
    }
    def keys = m.keySet().sort { -m[it] }
    keys.findAll { m[it] == m[keys[0]] }
}
```

Test:

```groovy
def random = new Random()
def sourceList = [ 'Lamp', 42.0, java.awt.Color.RED, new Date(), ~/pattern/]
(0..10).each {
    def a = (0..10).collect { sourceList[random.nextInt(5)] }
    println "${mode(a)}    ==    mode(${a})"
}
```

{{out}}

```txt
[pattern]    ==    mode([pattern, pattern, pattern, Lamp, pattern, Fri Oct 28 23:43:20 CDT 2011, java.awt.Color[r=255,g=0,b=0], Lamp, Lamp, Lamp, pattern])
[Lamp]    ==    mode([Lamp, Fri Oct 28 23:43:20 CDT 2011, Lamp, java.awt.Color[r=255,g=0,b=0], 42.0, java.awt.Color[r=255,g=0,b=0], Fri Oct 28 23:43:20 CDT 2011, Lamp, pattern, pattern, 42.0])
[java.awt.Color[r=255,g=0,b=0]]    ==    mode([java.awt.Color[r=255,g=0,b=0], java.awt.Color[r=255,g=0,b=0], 42.0, 42.0, Fri Oct 28 23:43:20 CDT 2011, Fri Oct 28 23:43:20 CDT 2011, 42.0, java.awt.Color[r=255,g=0,b=0], pattern, pattern, java.awt.Color[r=255,g=0,b=0]])
[Fri Oct 28 23:43:20 CDT 2011]    ==    mode([Fri Oct 28 23:43:20 CDT 2011, pattern, 42.0, Fri Oct 28 23:43:20 CDT 2011, Lamp, pattern, Fri Oct 28 23:43:20 CDT 2011, java.awt.Color[r=255,g=0,b=0], 42.0, 42.0, Fri Oct 28 23:43:20 CDT 2011])
[Fri Oct 28 23:43:20 CDT 2011, Lamp]    ==    mode([42.0, Fri Oct 28 23:43:20 CDT 2011, Lamp, Lamp, Fri Oct 28 23:43:20 CDT 2011, Fri Oct 28 23:43:20 CDT 2011, 42.0, Lamp, java.awt.Color[r=255,g=0,b=0], Lamp, Fri Oct 28 23:43:20 CDT 2011])
[java.awt.Color[r=255,g=0,b=0]]    ==    mode([Fri Oct 28 23:43:20 CDT 2011, java.awt.Color[r=255,g=0,b=0], java.awt.Color[r=255,g=0,b=0], pattern, 42.0, java.awt.Color[r=255,g=0,b=0], java.awt.Color[r=255,g=0,b=0], 42.0, pattern, Fri Oct 28 23:43:20 CDT 2011, pattern])
[42.0, java.awt.Color[r=255,g=0,b=0]]    ==    mode([42.0, java.awt.Color[r=255,g=0,b=0], pattern, Fri Oct 28 23:43:20 CDT 2011, Lamp, java.awt.Color[r=255,g=0,b=0], Lamp, Fri Oct 28 23:43:20 CDT 2011, java.awt.Color[r=255,g=0,b=0], 42.0, 42.0])
[Fri Oct 28 23:43:20 CDT 2011]    ==    mode([java.awt.Color[r=255,g=0,b=0], pattern, Fri Oct 28 23:43:20 CDT 2011, Lamp, 42.0, Fri Oct 28 23:43:20 CDT 2011, Fri Oct 28 23:43:20 CDT 2011, pattern, java.awt.Color[r=255,g=0,b=0], Lamp, Fri Oct 28 23:43:20 CDT 2011])
[Fri Oct 28 23:43:20 CDT 2011]    ==    mode([Fri Oct 28 23:43:20 CDT 2011, pattern, Fri Oct 28 23:43:20 CDT 2011, java.awt.Color[r=255,g=0,b=0], pattern, Fri Oct 28 23:43:20 CDT 2011, 42.0, java.awt.Color[r=255,g=0,b=0], Lamp, Fri Oct 28 23:43:20 CDT 2011, 42.0])
[pattern]    ==    mode([42.0, pattern, pattern, Lamp, 42.0, Lamp, Fri Oct 28 23:43:20 CDT 2011, java.awt.Color[r=255,g=0,b=0], pattern, 42.0, pattern])
[Lamp, 42.0]    ==    mode([Fri Oct 28 23:43:20 CDT 2011, pattern, Lamp, Lamp, Lamp, java.awt.Color[r=255,g=0,b=0], Fri Oct 28 23:43:20 CDT 2011, 42.0, 42.0, pattern, 42.0])
```


=={{header|Icon}} and {{header|Unicon}}==
The <tt>mode</tt> procedure generates all <i>n</i> mode values if the collection is <i>n</i>-modal.

```icon
procedure main(args)
    every write(!mode(args))
end

procedure mode(A)
    hist := table(0)
    every hist[!A] +:= 1
    hist := sort(hist, 2)
    modeCnt := hist[*hist][2]
    every modeP := hist[*hist to 1 by -1] do {
        if modeCnt = modeP[2] then suspend modeP[1]
        else fail
        }
end
```

{{out|Sample outputs}}

```txt
->am 3 1 4 1 5 9 7 6
1
->am 3 1 4 1 5 9 7 6 3
3
1
->
```



## J


```j
mode=: ~. #~ ( = >./ )@( #/.~ )
```

Example:

```txt
   mode 1 1 2 2 3 3 4 4 4 5 5 6 6 7 8 8 9 9 0 0 0
4 0
```



## Java


```java
import java.util.*;

public class Mode {
    public static <T> List<T> mode(List<? extends T> coll) {
        Map<T, Integer> seen = new HashMap<T, Integer>();
        int max = 0;
        List<T> maxElems = new ArrayList<T>();
        for (T value : coll) {
            if (seen.containsKey(value))
                seen.put(value, seen.get(value) + 1);
            else
                seen.put(value, 1);
            if (seen.get(value) > max) {
                max = seen.get(value);
                maxElems.clear();
                maxElems.add(value);
            } else if (seen.get(value) == max) {
                maxElems.add(value);
            }
        }
        return maxElems;
    }

    public static void main(String[] args) {
        System.out.println(mode(Arrays.asList(1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17))); // prints [6]
        System.out.println(mode(Arrays.asList(1, 1, 2, 4, 4))); // prints [1, 4]
    }
}
```



## JavaScript


```javascript
function mode(ary) {
    var counter = {};
    var mode = [];
    var max = 0;
    for (var i in ary) {
        if (!(ary[i] in counter))
            counter[ary[i]] = 0;
        counter[ary[i]]++;

        if (counter[ary[i]] == max)
            mode.push(ary[i]);
        else if (counter[ary[i]] > max) {
            max = counter[ary[i]];
            mode = [ary[i]];
        }
    }
    return mode;
}

mode([1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17]);  // [6]
mode([1, 2, 4, 4, 1]);  // [1,4]
```



## jq

jq only supports hashing of strings, so to preserve generality -- that is, to avoid assuming anything about the input array -- we simply sort it.
jq's <tt>sort</tt> is very fast in any case.
```jq
# modes/0 produces an array of [value, count]
# in increasing order of count:
def modes:
  sort | reduce .[] as $i ([];
    # state variable is an array of [value, count]
    if length == 0 then [ [$i, 1] ]
    elif .[-1][0] == $i then setpath([-1,1]; .[-1][1] + 1)
    else . + [[$i,1]]
    end )
    | sort_by( .[1] );

# mode/0 outputs a stream of the modal values;
# if the input array is empty, the output stream is also empty.
def mode:
  if length == 0 then empty
  else modes as $modes
       | $modes[-1][1] as $count
       | $modes[] | select( .[1] == $count) | .[0]
   end;
```
Examples:
```jq

[1,2,3,1,2,1]   | mode  # => 1
[1,2,3,1,2,1,2] | mode  # => 1 2
[1.1, 1.2, 1.3, 1.1, 1.2, 1.1] | mode) # => 1.1
```



## Julia


```k
function modes(values)
    dict = Dict() # Values => Number of repetitions
    modesArray = typeof(values[1])[] # Array of the modes so far
    max = 0 # Max of repetitions so far

    for v in values
        # Add one to the dict[v] entry (create one if none)
        if v in keys(dict)
            dict[v] += 1
        else
            dict[v] = 1
        end

        # Update modesArray if the number of repetitions
        # of v reaches or surpasses the max value
        if dict[v] >= max
            if dict[v] > max
                empty!(modesArray)
                max += 1
            end
            append!(modesArray, [v])
        end
    end

    return modesArray
end

println(modes([1,3,6,6,6,6,7,7,12,12,17]))
println(modes((1,1,2,4,4)))
```



## K


```k
  mode: {(?x)@&n=|/n:#:'=x}
  mode 1 1 1 1 2 2 2 3 3 3 3 4 4 3 2 4 4 4
3 4
```



## Kotlin


```scala>fun <T
 modeOf(a: Array<T>) {
    val sortedByFreq = a.groupBy { it }.entries.sortedByDescending { it.value.size }
    val maxFreq = sortedByFreq.first().value.size
    val modes = sortedByFreq.takeWhile { it.value.size == maxFreq }
    if (modes.size == 1)
       println("The mode of the collection is ${modes.first().key} which has a frequency of $maxFreq")
    else {
       print("There are ${modes.size} modes with a frequency of $maxFreq, namely : ")
       println(modes.map { it.key }.joinToString(", "))
    }
}

fun main(args: Array<String>) {
    val a = arrayOf(7, 1, 1, 6, 2, 4, 2, 4, 2, 1, 5)
    println("[" + a.joinToString(", ") + "]")
    modeOf(a)
    println()
    val b = arrayOf(true, false, true, false, true, true)
    println("[" + b.joinToString(", ") + "]")
    modeOf(b)
}
```


{{out}}

```txt

[7, 1, 1, 6, 2, 4, 2, 4, 2, 1, 5]
There are 2 modes with a frequency of 3, namely : 1, 2

[true, false, true, false, true, true]
The mode of the collection is true which has a frequency of 4

```



## Lasso


```Lasso
define getmode(a::array)::array => {
	local(mmap = map, maxv = 0, modes = array)
	// store counts
	with e in #a do => { #mmap->keys >> #e ? #mmap->find(#e) += 1 | #mmap->insert(#e = 1) }
	// get max value
	with e in #mmap->keys do => { #mmap->find(#e) > #maxv ? #maxv = #mmap->find(#e) }
	// get modes with max value
	with e in #mmap->keys where #mmap->find(#e) == #maxv do => { #modes->insert(#e) }
	return #modes
}
getmode(array(1,3,6,6,6,6,7,7,12,12,17))
getmode(array(1,3,6,3,4,8,9,1,2,3,2,2))
```


{{out}}

```txt
array(6)
array(2, 3)
```



## Liberty BASIC

Using string of integers instead collection.

```lb

a$ = "1 3 6 6 6 6 7 7 12 12 17"
b$ = "1 2 4 4 1"

print "Modes for ";a$
print modes$(a$)
print "Modes for ";b$
print modes$(b$)

function modes$(a$)
'get array size
    n=0
    t$ = "*"
    while t$<>""
        n=n+1
        t$=word$(a$, n)
        'print n, t$
    wend
    n=n-1
    'print "n=", n
'dim array
'read in array
    redim a(n)
    for i = 1 to n
        a(i)=val(word$(a$, i))
        'print i, a(i)
    next
'sort
    sort a(), 1, n
'get the modes
    occurence = 1
    maxOccurence = 0
    oldVal = a(1)
    modes$ = ""
    for i = 2 to n
        'print i, a(i)
        if a(i) = oldVal then
            occurence = occurence + 1
        else
            select case
            case occurence >  maxOccurence
                 maxOccurence = occurence
                 modes$ = oldVal; " "
            case occurence = maxOccurence
                modes$ =  modes$; oldVal; " "
            end select
            occurence = 1
        end if
         oldVal = a(i)
    next
    'check after loop
    select case
    case occurence >  maxOccurence
        maxOccurence = occurence
        modes$ = oldVal; " "
    case occurence = maxOccurence
        modes$ =  modes$; oldVal; " "
    end select
end function
```


{{out}}

```txt

Modes for 1 3 6 6 6 6 7 7 12 12 17
6
Modes for 1 2 4 4 1
1 4

```



## Lua


```lua
function mode(tbl) -- returns table of modes and count
	assert(type(tbl) == 'table')
	local counts = { }
	for _, val in pairs(tbl) do
		-- see http://lua-users.org/wiki/TernaryOperator
		counts[val] = counts[val] and counts[val] + 1 or 1
	end
	local modes = { }
	local modeCount = 0
	for key, val in pairs(counts) do
		if val > modeCount then
			modeCount = val
			modes = {key}
		elseif val == modeCount then
			table.insert(modes, key)
		end
	end
	return modes, modeCount
end

modes, count = mode({1,3,6,6,6,6,7,7,12,12,17})
for _, val in pairs(modes) do io.write(val..' ') end
print("occur(s) ", count, " times")

modes, count = mode({'a', 'a', 'b', 'd', 'd'})
for _, val in pairs(modes) do io.write(val..' ') end
print("occur(s) ", count, " times")

```



## M2000 Interpreter

We use Inventory with keys as numbers (internal are strings). Inventories work with hash function. So searching is very fast.

Function return an inventory, with all "modes" with same max number. Now work with mix numbers and strings. Islet return true if top of stack is letter (string).


```M2000 Interpreter

Module Checkit {
      \\ find mode
      Function GetMode {
            Inventory  N
            Inventory ALLMODES
            m=1
            While not empty {
                 if islet then  {
                       Read A$
                        if Exist(N, A$) then  {
                             k=Eval(N)
                             k++
                             if m=k then  {
                                  Append ALLMODES, A$
                             }
                             if m<k then  m=k : Clear ALLMODES : Append ALLMODES, A$
                             return N, A$:=k
                        }  Else Append N, A$:=1 : if m=1 then Append ALLMODES, A$

                  } else {
                        Read A
                        if Exist(N, A) then  {
                             k=Eval(N)
                             k++
                             if m=k then  {
                                  Append ALLMODES, A
                             }
                             if m<k then  m=k : Clear ALLMODES : Append ALLMODES, A
                             return N, A:=k
                        }  Else Append N, A:=1 : if m=1 then Append ALLMODES, A
                  }
             }
             =ALLMODES
      }
      Print GetMode(1, 2, 3, 1, 2, 4, 2, 5, 2, 3, 3, 1, 3, 6) ' print 2 3
      Dim A()
      A()=(1, 2, 3, 1, 2, 4, 2, 5, 2, 3, 3, 1, 3, 6)
      \\ get a pointer from A
      m=A()
      Print GetMode(!m) ' print 2 3
      z=stack:=1, 2,"B", 3, 1, 2, "B", 4, 2, 5,"B", 2, 3, 3, 1, 3, 6, "B"
      Print GetMode(!z)   ' print 2 3 B
}
Checkit

```


Using idea from BBC BASIC. Function GetMode return array. As Array get bigger function run slower exponential. Previous example using inventory, has linear response (double data, double time to run). Eacb function has a mew stack of value. This one line function Def AllStack()=[] get all arguments and place them in a stack object, and a pointer to stack returned. This function return an array: Def AllArray=Array([]). We can use (,) for empty array, (1,) for one item array, (1,2,3) for three item. We can use ((1,2),(3,4)) for an array with two arrays as items. We can use Stack for empty stack, or Stack:=1,2,3 for a stack with 3 items. Stacks are linked lists. A=Stack : Stack A { push 1,2,3 } : Print A  ' print 3 2 1 where 3 is the top. Stack A (Data 0} : Print A ' print  3 2 1 0 (data push to bottom to stack). Functions and Modules always have a "current stack". So M2000 is like basic but is a stack oriented language, but for expressions use infix notation.




```M2000 Interpreter

Module Checkit {
      Function GetMode(&a()){
            Local max%
            if len(a())=0 then =(,)
            link a() to a$()
            n%=len(a())
            dim c%(n%)
            if type$(a(0))="String" then {
                  For i%=0 to n%-2
                        For j%=i%+1 to n%-1
                              if order(a$(i%), a$(j%))=0 then c%(i%)++
                        Next j%
                        If c%(i%)>max% Then max%=c%(i%)
                  Next i%
                  For i%=0 to n%-1
                        If c%(i%) = max% Then Data a$(i%)
                  Next i%
            } Else {
                  For i%=0 to n%-2
                        For j%=i%+1 to n%-1
                              if a(i%)==a(j%) then c%(i%)++
                        Next j%
                        If c%(i%)>max% Then max%=c%(i%)
                  Next i%
                  For i%=0 to n%-1
                        If c%(i%) = max% Then Data a(i%)
                  Next i%
            }
            =Array([])
      }
      Dim m()
      m()=(2,3,43,234,234,3,324)
      Print GetMode(&m())  ' 3 234
      k=(1,2,1,2,1,2,3)
      n=GetMode(&k)

      ' iterate backward
      i=each(n, -1, 1)
      While i {
            Print Array(i),
      }
      Print
      k=("A","B","A","B", "B","C","D","A")
      ? GetMode(&k)    ' A B
}
Checkit

```



## Maple

The built-in function Statistics:-Mode can be used to compute a mode.
When the mode is unique, it returns a numeric result and when there are multiple modes, it returns a set, as in the following example:

```Maple
Statistics:-Mode([1, 2.1, 2.1, 3]);
Statistics:-Mode([1, 2.1, 2.1, 3.2, 3.2, 5]);
```


{{out}}

```txt
 2.1
 {2.1, 3.2}

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Built-in function commonest returns a '''list''' of the most common element(s), even is there is only one 'commonest' number. Example for multiple 'commonest' numbers and a single 'commonest' number:

```Mathematica
 Commonest[{b, a, c, 2, a, b, 1, 2, 3}]
 Commonest[{1, 3, 2, 3}]
```

{{out}}

```txt
 {b,a,2}
 {3}
```



## MATLAB


```Matlab
function modeValue = findmode(setOfValues)
   modeValue = mode(setOfValues);
end
```



## MUMPS


```MUMPS
MODE(X)
 ;X is assumed to be a list of numbers separated by "^"
 ;I is a loop index
 ;L is the length of X
 ;Y is a new array
 ;ML is the list of modes
 ;LOC is a placeholder to shorten the statement
 Q:'$DATA(X) "No data"
 Q:X="" "Empty Set"
 NEW Y,I,L,LOC
 SET L=$LENGTH(X,"^"),ML=""
 FOR I=1:1:L SET LOC=+$P(X,"^",I),Y(LOC)=$S($DATA(Y(LOC)):Y(LOC)+1,1:1)
 SET I="",I=$O(Y(I)),ML=I ;Prime the pump, rather than test for no data
 FOR  S I=$O(Y(I)) Q:I=""  S ML=$SELECT(Y($P(ML,"^"))>Y(I):ML,Y($P(ML,"^"))<Y(I):I,Y($P(ML,"^"))=Y(I):ML_"^"_I)
 QUIT ML
```


```txt
USER>W $$MODE^ROSETTA("1^2^3^2")
2
USER>W $$MODE^ROSETTA("1^2^3^2^3")
2^3
USER>W $$MODE^ROSETTA("")
Empty Set
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

run_samples()
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method mode(lvector = java.util.List) public static returns java.util.List
  seen    = 0
  modes   = ''
  modeMax = 0
  loop v_ = 0 to lvector.size() - 1
    mv = Rexx lvector.get(v_)
    seen[mv] = seen[mv] + 1
    select
      when seen[mv] > modeMax then do
        modeMax = seen[mv]
        modes = ''
        nx = 1
        modes[0]  = nx
        modes[nx] = mv
        end
      when seen[mv] = modeMax then do
        nx = modes[0] + 1
        modes[0]  = nx
        modes[nx] = mv
        end
      otherwise do
        nop
        end
      end
    end v_

  modeList = ArrayList(modes[0])
  loop v_ = 1 to modes[0]
    val = modes[v_]
    modeList.add(val)
    end v_

  return modeList

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method mode(rvector = Rexx[]) public static returns java.util.List
  return mode(ArrayList(Arrays.asList(rvector)))

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method show_mode(lvector = java.util.List) public static returns java.util.List
  modes = mode(lvector)
  say 'Vector:' (Rexx lvector).space(0)', Mode(s):' (Rexx modes).space(0)
  return modes

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method show_mode(rvector = Rexx[]) public static returns java.util.List
  return show_mode(ArrayList(Arrays.asList(rvector)))

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method run_samples() public static
  show_mode([Rexx 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])                                   -- 10 9 8 7 6 5 4 3 2 1
  show_mode([Rexx 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0.11])                 -- 0
  show_mode([Rexx 30, 10, 20, 30, 40, 50, -100, 4.7, -11e+2])                       -- 30
  show_mode([Rexx 30, 10, 20, 30, 40, 50, -100, 4.7, -11e+2, -11e+2])               -- 30 -11e2
  show_mode([Rexx 1, 8, 6, 0, 1, 9, 4, 6, 1, 9, 9, 9])                              -- 9
  show_mode([Rexx 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11])                               -- 1 2 3 4 5 6 7 8 9 10 11
  show_mode([Rexx 8, 8, 8, 2, 2, 2])                                                -- 8 2
  show_mode([Rexx 'cat', 'kat', 'Cat', 'emu', 'emu', 'Kat'])                        -- emu
  show_mode([Rexx 0, 1, 2, 3, 3, 3, 4, 4, 4, 4, 1, 0])                              -- 4
  show_mode([Rexx 2, 7, 1, 8, 2])                                                   -- 2
  show_mode([Rexx 2, 7, 1, 8, 2, 8])                                                -- 8 2
  show_mode([Rexx 'E', 'n', 'z', 'y', 'k', 'l', 'o', 'p', 'ä', 'd', 'i', 'e'])      -- E n z y k l o p ä d i e
  show_mode([Rexx 'E', 'n', 'z', 'y', 'k', 'l', 'o', 'p', 'ä', 'd', 'i', 'e', 'ä']) -- ä
  show_mode([Rexx 3, 1, 4, 1, 5, 9, 7, 6])                                          -- 1
  show_mode([Rexx 3, 1, 4, 1, 5, 9, 7, 6, 3])                                       -- 3, 1
  show_mode([Rexx 1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17])                              -- 6
  show_mode([Rexx 1, 1, 2, 4, 4])                                                   -- 4 1
  return

```

'''Output:'''

```txt

Vector: [10,9,8,7,6,5,4,3,2,1], Mode(s): [10,9,8,7,6,5,4,3,2,1]
Vector: [10,9,8,7,6,5,4,3,2,1,0,0,0,0,0.11], Mode(s): [0]
Vector: [30,10,20,30,40,50,-100,4.7,-1100], Mode(s): [30]
Vector: [30,10,20,30,40,50,-100,4.7,-1100,-1100], Mode(s): [30,-1100]
Vector: [1,8,6,0,1,9,4,6,1,9,9,9], Mode(s): [9]
Vector: [1,2,3,4,5,6,7,8,9,10,11], Mode(s): [1,2,3,4,5,6,7,8,9,10,11]
Vector: [8,8,8,2,2,2], Mode(s): [8,2]
Vector: [cat,kat,Cat,emu,emu,Kat], Mode(s): [emu]
Vector: [0,1,2,3,3,3,4,4,4,4,1,0], Mode(s): [4]
Vector: [2,7,1,8,2], Mode(s): [2]
Vector: [2,7,1,8,2,8], Mode(s): [2,8]
Vector: [E,n,z,y,k,l,o,p,ä,d,i,e], Mode(s): [E,n,z,y,k,l,o,p,ä,d,i,e]
Vector: [E,n,z,y,k,l,o,p,ä,d,i,e,ä], Mode(s): [ä]
Vector: [3,1,4,1,5,9,7,6], Mode(s): [1]
Vector: [3,1,4,1,5,9,7,6,3], Mode(s): [1,3]
Vector: [1,3,6,6,6,6,7,7,12,12,17], Mode(s): [6]
Vector: [1,1,2,4,4], Mode(s): [1,4]

```



## Nim


```nim
import tables

proc modes[T](xs: openArray[T]): T =
  var count = initCountTable[T]()
  for x in xs:
    count.inc(x)
  largest(count).key

echo modes(@[1,3,6,6,6,6,7,7,12,12,17])
echo modes(@[1,1,2,4,4])
```

Output:

```txt
6
1
```

=={{header|Oberon-2}}==
{{Works with|oo2c version2}}

```oberon2

MODULE Mode;
IMPORT
  Object:Boxed,
  ADT:Dictionary,
  ADT:LinkedList,
  Out := NPCT:Console;

TYPE
  Key = Boxed.LongInt;
  Val = Boxed.LongInt;


VAR
  x: ARRAY 11 OF LONGINT;
  y: ARRAY 5 OF LONGINT;
  z: ARRAY 8 OF LONGINT;

  PROCEDURE Show(ll: LinkedList.LinkedList(Key));
  VAR
    iter: LinkedList.Iterator(Key);
    i: LONGINT;
    k: Key;
  BEGIN
    iter := ll.GetIterator(NIL);
    FOR i := 0 TO ll.Size() - 1 DO;
      k := iter.Next();
      Out.Int(k.value,0);Out.Ln;
    END;
  END Show;

  PROCEDURE Mode(x: ARRAY OF LONGINT): LinkedList.LinkedList(Key);
  VAR
    d: Dictionary.Dictionary(Key,Val);
    i: LONGINT;
    k: Key; v: Val;
    iter: Dictionary.IterKeys(Key,Val);
    resp: LinkedList.LinkedList(Key);
    max: Boxed.LongInt;
  BEGIN
    d := NEW(Dictionary.Dictionary(Key,Val));
    FOR i := 0 TO LEN(x) - 1 DO
      k := NEW(Key,x[i]);
      IF d.Lookup(k,v) THEN
        d.Set(k,NEW(Val,v.value + 1));
      ELSE
        d.Set(k,NEW(Val,1))
      END
    END;

    max := NEW(Boxed.LongInt,0);
    resp := NEW(LinkedList.LinkedList(Key));
    iter := d.IterKeys();
    WHILE (iter.Next(k)) DO
      v := d.Get(k);
      IF v.Cmp(max) > 0 THEN
        resp.Clear();
        resp.Append(k);max := v
      ELSIF v.Cmp(max) = 0 THEN
        resp.Append(k);max := v
      END
    END;

    RETURN resp

  END Mode;

BEGIN
  x[0] := 1; x[1] := 3; x[2] := 6; x[3] := 6;
  x[4] := 6; x[5] := 6; x[6] := 7; x[7] := 7;
  x[8] := 12; x[9] := 12; x[10] := 17;
  Show(Mode(x));Out.Ln;
  y[0] := 1; y[1] := 2; y[2] := 4; y[3] := 4; y[4] := 1;
  Show(Mode(y));Out.Ln;
  z[0] := 1; z[1] := 2; z[2] := 4; z[3] := 4; z[4] := 1; z[5] := 5; z[6] := 5; z[7] := 5;
  Show(Mode(z));Out.Ln;
END Mode.

```

{{out}

```txt

6

4
1

5

```


## Objeck


```objeck

﻿use Collection;

class Mode {
  function : Main(args : String[]) ~ Nil {
    Print(Mode([1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17]));
    Print(Mode([1, 2, 4, 4, 1]));
  }

  function : Mode(coll : Int[]) ~ IntVector {
    seen := IntMap->New();
    max := 0;
    maxElems := IntVector->New();

    each(i : coll) {
      value := coll[i];

      featched := seen->Find(value)->As(IntHolder);
      if(featched <> Nil) {
        seen->Remove(value);
        seen->Insert(value, IntHolder->New(featched->Get() + 1));
      }
      else {
        seen->Insert(value, IntHolder->New(1));
      };

      featched := seen->Find(value)->As(IntHolder);
      if(featched->Get() > max) {
        max := featched->Get();
        maxElems->Empty();
        maxElems->AddBack(value);
      }
      else if(featched->Get() = max) {
        maxElems->AddBack(value);
      };
    };

    return maxElems;
  }

  function : Print(out : IntVector) ~ Nil {
    '['->Print();
    each(i : out) {
      out->Get(i)->Print();
      if(i + 1 < out->Size()) {
        ", "->Print();
      };
    };
    ']'->PrintLine();
  }
}

```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h


@interface NSArray (Mode)
- (NSArray *)mode;
@end

@implementation NSArray (Mode)
- (NSArray *)mode {
    NSCountedSet *seen = [NSCountedSet setWithArray:self];
    int max = 0;
    NSMutableArray *maxElems = [NSMutableArray array];
    for ( obj in seen ) {
        int count = [seen countForObject:obj];
        if (count > max) {
            max = count;
            [maxElems removeAllObjects];
            [maxElems addObject:obj];
        } else if (count == max) {
            [maxElems addObject:obj];
        }
    }
    return maxElems;
}
@end
```



## OCaml


```ocaml
let mode lst =
  let seen = Hashtbl.create 42 in
    List.iter (fun x ->
                 let old = if Hashtbl.mem seen x then
                   Hashtbl.find seen x
                 else 0 in
                   Hashtbl.replace seen x (old + 1))
      lst;
    let best = Hashtbl.fold (fun _ -> max) seen 0 in
      Hashtbl.fold (fun k v acc ->
                      if v = best then k :: acc
                      else acc)
        seen []
```

 # mode [1;3;6;6;6;6;7;7;12;12;17];;
 - : int list = [6]
 # mode [1;1;2;4;4];;
 - : int list = [4; 1]


## Octave

Of course Octave has the <tt>mode</tt> function; but it returns only the "lowest" mode if multiple modes are available.

```octave
function m = mode2(v)
  sv = sort(v);
  % build two vectors, vals and c, so that
  % c(i) holds how many times vals(i) appears
  i = 1; c = []; vals = [];
  while (i <= numel(v) )
    tc = sum(sv==sv(i)); % it would be faster to count
                         % them "by hand", since sv is sorted...
    c = [c, tc];
    vals = [vals, sv(i)];
    i += tc;
  endwhile
  % stack vals and c building a 2-rows matrix x
  x = cat(1,vals,c);
  % sort the second row (frequencies) into t (most frequent
  % first) and take the "original indices" i ...
  [t, i] = sort(x(2,:), "descend");
  % ... so that we can use them to sort columns according
  % to frequencies
  nv = x(1,i);
  % at last, collect into m (the result) all the values
  % having the same bigger frequency
  r = t(1); i = 1;
  m = [];
  while ( t(i) == r )
    m = [m, nv(i)];
    i++;
  endwhile
endfunction
```


```octave
a = [1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17];
mode2(a)
mode(a)

a = [1, 1, 2, 4, 4];
mode2(a)    % returns 1 and 4
mode(a)     % returns 1 only
```



## ooRexx

See the example at [[#Version_2|REXX, Version 2]] for a version that returns multiple mode values.

```ooRexx

-- will work with just about any collection...
call testMode .array~of(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
call testMode .list~of(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, .11)
call testMode .queue~of(30, 10, 20, 30, 40, 50, -100, 4.7, -11e2)

::routine testMode
  use arg list
  say "list =" list~makearray~toString("l", ", ")
  say "mode =" mode(list)
  say

::routine mode
  use arg list

  -- this is a good application for a bag
  -- add all of the items to the bag
  collector = .bag~new
  collector~putAll(list)
  -- now get a list of unique items
  indexes = .set~new~~putall(collector)
  count = 0    -- this is used to keep track of the maximums
  -- now see how many of each element we ended up with
  loop index over indexes
      items = collector~allat(index)
      newCount = items~items
      if newCount > count then do
          mode = items[1]
          count = newCount
      end
  end

  return mode

```



## Oz


```oz
declare
  fun {Mode Xs}
     Freq = {Dictionary.new}
     for X in Xs do
        Freq.X := {CondSelect Freq X 0} + 1
     end
     MaxCount = {FoldL {Dictionary.items Freq} Max 0}
  in
     for Value#Count in {Dictionary.entries Freq} collect:C do
        if Count == MaxCount then
  	 {C Value}
        end
     end
  end
in
  {Show {Mode [1 2 3 3 2 1 1]}}
  {Show {Mode [1 2 3 3 2 1]}}
```



## PARI/GP


```parigp
mode(v)={
  my(count=1,r=1,b=v[1]);
  v=vecsort(v);
  for(i=2,#v,
    if(v[i]==v[i-1],
      count++
    ,
      if(count>r,
        r=count;
        b=v[i-1]
      );
      count=1
    )
  );
  if(count>r,v[#v],b)
};
```



## Perl


```perl
use strict;
use List::Util qw(max);

sub mode
{
    my %c;
    foreach my $e ( @_ ) {
	$c{$e}++;
    }
    my $best = max(values %c);
    return grep { $c{$_} == $best } keys %c;
}
```



```perl
print "$_ " foreach mode(1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17);
print "\n";
print "$_ " foreach mode(1, 1, 2, 4, 4);
print "\n";
```



## Perl 6


{{works with|Rakudo|2019.03.1}}

```perl6
sub mode (*@a) {
    my %counts := @a.Bag;
    my $max = %counts.values.max;
    %counts.grep(*.value == $max).map(*.key);
}

# Testing with arrays:
say mode [1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17];
say mode [1, 1, 2, 4, 4];
```


{{out}}

```txt

6
(4 1)

```


Alternatively, a version that uses a single method chain with no temporary variables: (Same output with same input)


```perl6
sub mode (*@a) {
    @a.Bag                # count elements
      .classify(*.value)  # group elements with the same count
      .max(*.key)         # get group with the highest count
      .value.map(*.key);  # get elements in the group
}

say mode [1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17];
say mode [1, 1, 2, 4, 4];
```



## Phix


```Phix
function mode(sequence s)
-- returns a list of the most common values, each of which occurs the same number of times
object this
integer nxt = 1, count = 1, maxc = 1
sequence res = {}
    if length(s)!=0 then
        s = sort(s)
        this = s[1]
        for i=2 to length(s) do
            if s[i]!=this then
                s[nxt] = {count,this}
                nxt += 1
                this = s[i]
                count = 1
            else
                count += 1
                if count>maxc then
                    maxc = count
                end if
            end if
        end for
        s[nxt] = {count,this}
        res = ""
        for i=1 to nxt do
            if s[i][1]=maxc then
                res = append(res,s[i][2])
            end if
        end for
    end if
    return res
end function

?mode({1, 2, 5, -5, -9.5, 3.14159})
?mode({ 1, "blue", 2, 7.5, 5, "green", "red", 5, 2, "blue", "white" })
?mode({1, 2, 3, 1, 2, 4, 2, 5, 2, 3, 3, 1, 3, 6})
?mode({.2, .7, .1, .8, .2})
?mode({"two", 7, 1, 8, "two", 8})
?mode("Hello there world")
?mode({})
{} = wait_key()
```

{{out}}

```txt

{-9.5,-5,1,2,3.14159,5}
{2,5,"blue"}
{2,3}
{0.2}
{8,"two"}
"el"
{}

```



## PHP

Note: this function only works with strings and integers, as those are the only things that can be used as keys of an (associative) array in PHP.

```php
<?php
function mode($arr) {
    $count = array_count_values($arr);
    $best = max($count);
    return array_keys($count, $best);
}

print_r(mode(array(1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17)));
print_r(mode(array(1, 1, 2, 4, 4)));
?>
```



## PicoLisp


```PicoLisp
(de modes (Lst)
   (let A NIL
      (for X Lst
         (accu 'A X 1) )
      (mapcar car
         (maxi cdar
            (by cdr group A) ) ) ) )
```

{{out}}

```txt
: (modes (1 3 6 6 6 6 7 7 12 12 17))
-> (6)

: (modes (1 1 2 4 4))
-> (4 1)

: (modes (chop "ABRAHAMASANTACLARA"))
-> ("A")

: (modes (1 4 A 3 2 7 1 B B 3 6 2 4 C C 5 2 5 B A 3 2 C 3 5 5 4 C 7 7))
-> (5 C 2 3)
```



## PL/I


```PL/I
av: procedure options (main);  /* 28 October 2013 */
   declare x(10) fixed binary static initial (1, 4, 2, 6, 2, 5, 6, 2, 4, 2);
   declare f(32767) fixed binary;
   declare (j, n, max, value) fixed binary;
   declare i fixed;

   n = hbound(x,1);

   do i = 1 to n;
      j = x(i);
      f(j) = f(j) + 1;
   end;
   max = 0;
   do i = 1 to hbound(f,1);
      if max < f(i) then do; max = f(i); value = i; end;
   end;
   put list ('The mode value is ' || value || ' occurred ' ||
             max || ' times.');

end av;
```

Results:

```txt

The mode value is         2 occurred         4 times.

```



## PowerShell


```PowerShell
$data = @(1,1,1,2,3,4,5,5,6,7,7,7)
$groups = $data | group-object | sort-object count -Descending
$groups | ? {$_.Count -eq $groups[0].Count}
```

{{out}}

```txt
Count Name                      Group
----- ----                      -----
    3 7                         {7, 7, 7}
    3 1                         {1, 1, 1}
```



## PureBasic


```PureBasic
Procedure mean(Array InArray(1))

  Structure MyMean
    Value.i
    Cnt.i
  EndStructure

  Protected i, max, found
  Protected NewList MyDatas.MyMean()

  Repeat
    found=#False
    ForEach MyDatas()
      If InArray(i)=MyDatas()\Value
        MyDatas()\Cnt+1
        found=#True
        Break
      EndIf
    Next
    If Not found
      AddElement(MyDatas())
      MyDatas()\Value=InArray(i)
      MyDatas()\cnt+1
    EndIf
    If MyDatas()\Cnt>max
      max=MyDatas()\Cnt
    EndIf
    i+1
  Until i>ArraySize(InArray())

  ForEach MyDatas()
    If MyDatas()\Cnt=max
      For i=1 To max
        Print(Str(MyDatas()\Value)+" ")
      Next
    EndIf
  Next
EndProcedure
```



## Python

The following solutions require that the elements be ''hashable''.
{{works with|Python|2.5+ and 3.x}}

```python>>>
 from collections import defaultdict
>>> def modes(values):
	count = defaultdict(int)
	for v in values:
		count[v] +=1
	best = max(count.values())
	return [k for k,v in count.items() if v == best]

>>> modes([1,3,6,6,6,6,7,7,12,12,17])
[6]
>>> modes((1,1,2,4,4))
[1, 4]
```

{{works with|Python|2.7+ and 3.1+}}

```python>>>
 from collections import Counter
>>> def modes(values):
	count = Counter(values)
	best = max(count.values())
	return [k for k,v in count.items() if v == best]

>>> modes([1,3,6,6,6,6,7,7,12,12,17])
[6]
>>> modes((1,1,2,4,4))
[1, 4]
```

If you just want one mode (instead of all of them), here's a one-liner for that:

```python
def onemode(values):
    return max(set(values), key=values.count)
```



## Q


```q
mode:{(key x) where value x=max x} count each group @
```



## R


```R
statmode <- function(v) {
  a <- sort(table(v), decreasing=TRUE)
  r <- c()
  for(i in 1:length(a)) {
    if ( a[[1]] == a[[i]] ) {
      r <- c(r, as.integer(names(a)[i]))
    } else break; # since it's sorted, once we find
                  # a different value, we can stop
  }
  r
}

print(statmode(c(1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17)))
print(statmode(c(1, 1, 2, 4, 4)))
```



## Racket

Returns values of list of modes and their frequencies of appearance.

```Racket
#lang racket

(define (mode seq)
  (define frequencies (make-hash))
  (for ([s seq])
    (hash-update! frequencies
                  s
                  (lambda (freq) (add1 freq))
                  0))
  (for/fold ([ms null]
             [freq 0])
            ([(k v) (in-hash frequencies)])
    (cond [(> v freq)
           (values (list k) v)]
          [(= v freq)
           (values (cons k ms) freq)]
          [else
           (values ms freq)])))
```



## REXX


### version 1

Returns one mode value.

```rexx
/*REXX program finds the   mode   (most occurring element)  of a  vector.               */
/*      ════════vector═══════════      ═══show vector═══    ═════show result═════       */
      v= 1 8 6 0 1 9 4 6 1 9 9 9    ;   say 'vector='v;      say 'mode='mode(v);       say
      v= 1 2 3 4 5 6 7 8 9 11 10    ;   say 'vector='v;      say 'mode='mode(v);       say
      v= 8 8 8 2 2 2                ;   say 'vector='v;      say 'mode='mode(v);       say
      v='cat kat Cat emu emu Kat'   ;   say 'vector='v;      say 'mode='mode(v);       say
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sort: procedure expose @.;  parse arg # 1 h      /* [↓]  this is an  exchange sort.     */
           do  while h>1;                h=h%2   /*In REXX,   %    is an integer divide.*/
             do i=1  for #-h;     j=i;   k=h+i   /* [↓]  perform exchange for elements. */
               do  while @.k<@.j & h<j;  _=@.j;   @.j=@.k;   @.k=_;   j=j-h;  k=k-h;   end
             end   /*i*/
           end     /*while h>1*/;        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
mode: procedure expose @.; parse arg x;  freq=1  /*function finds the  MODE  of a vector*/
      #=words(x)                                 /*#:  the number of elements in vector.*/
           do k=1  for #;  @.k=word(x,k);  end   /* ◄──── make an array from the vector.*/
      call Sort  #                               /*sort the elements in the array.      */
      ?=@.1                                      /*assume the first element is the mode.*/
               do j=1  for #;        _=j-freq    /*traipse through the elements in array*/
               if @.j==@._  then do; freq=freq+1 /*is this element the same as previous?*/
                                     ?=@.j       /*this element is the mode (···so far).*/
                                 end
               end   /*j*/
      return ?                                   /*return the mode of vector to invoker.*/
```

'''output'''

```txt

vector=1 8 6 0 1 9 4 6 1 9 9 9
mode=9

vector=1 2 3 4 5 6 7 8 9 11 10
mode=1

vector=8 8 8 2 2 2
mode=2

vector=cat kat Cat emu emu Kat
mode=emu

```



### version 2

{{trans|NetRexx}}
{{works with|ooRexx}}
{{works with|Regina}}
and should work for every REXX.

```REXX
/* Rexx */
/*-- ~~ main ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
call run_samples
return
exit

/*-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/*-- returns a comma separated string of mode values from a comma separated input vector string */
mode:
  procedure
  parse arg lvector
  drop vector.
  vector. = ''
  call makeStem lvector /*-- this call creates the "vector." stem from the input string */
  seen.   = 0
  modes.  = ''
  modeMax = 0
  do v_ = 1 to vector.0
    mv = vector.v_
    seen.mv = seen.mv + 1
    select
      when seen.mv > modeMax then do
        modeMax = seen.mv
        drop modes.
        modes. = ''
        nx = 1
        modes.0  = nx
        modes.nx = mv
        end
      when seen.mv = modeMax then do
        nx = modes.0 + 1
        modes.0  = nx
        modes.nx = mv
        end
      otherwise do
        nop
        end
      end
    end v_

  lmodes = ''
  do e_ = 1 to modes.0
    lmodes = lmodes modes.e_
    end e_
  lmodes = strip(space(lmodes, 1, ','))

  return lmodes

/*-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/*-- pretty-print */
show_mode:
  procedure
  parse arg lvector
  lmodes = mode(lvector)
  say 'Vector: ['space(lvector, 0)'], Mode(s): ['space(lmodes, 0)']'
  return modes

/*-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/*-- load the "vector." stem from the comma separated input vector string */
makeStem:
  procedure expose vector.
  vector.0 = 0
  parse arg lvector
  do v_ = 1 while lvector \= ''
    parse var lvector val ',' lvector
    vector.0 = v_
    vector.v_ = strip(val)
    vector = strip(lvector)
    end v_
  return vector.0

/*-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
run_samples:
  procedure
  call show_mode '10, 9, 8, 7, 6, 5, 4, 3, 2, 1'                     -- 10 9 8 7 6 5 4 3 2 1
  call show_mode '10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0.11'   -- 0
  call show_mode '30, 10, 20, 30, 40, 50, -100, 4.7, -11e+2'         -- 30
  call show_mode '30, 10, 20, 30, 40, 50, -100, 4.7, -11e+2, -11e+2' -- 30 -11e2
  call show_mode '1, 8, 6, 0, 1, 9, 4, 6, 1, 9, 9, 9'                -- 9
  call show_mode '1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11'                 -- 1 2 3 4 5 6 7 8 9 10 11
  call show_mode '8, 8, 8, 2, 2, 2'                                  -- 8 2
  call show_mode 'cat, kat, Cat, emu, emu, Kat'                      -- emu
  call show_mode '0, 1, 2, 3, 3, 3, 4, 4, 4, 4, 1, 0'                -- 4
  call show_mode '2, 7, 1, 8, 2'                                     -- 2
  call show_mode '2, 7, 1, 8, 2, 8'                                  -- 8 2
  call show_mode 'E, n, z, y, k, l, o, p, ä, d, i, e'                -- E n z y k l o p ä d i e
  call show_mode 'E, n, z, y, k, l, o, p, ä, d, i, e, ä'             -- ä
  call show_mode '3, 1, 4, 1, 5, 9, 7, 6'                            -- 1
  call show_mode '3, 1, 4, 1, 5, 9, 7, 6, 3'                         -- 3, 1
  call show_mode '1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17'                -- 6
  call show_mode '1, 1, 2, 4, 4'                                     -- 4 1
  return
```

{{out}}

```txt
Vector: [10,9,8,7,6,5,4,3,2,1], Mode(s): [10,9,8,7,6,5,4,3,2,1]
Vector: [10,9,8,7,6,5,4,3,2,1,0,0,0,0,0.11], Mode(s): [0]
Vector: [30,10,20,30,40,50,-100,4.7,-11e+2], Mode(s): [30]
Vector: [30,10,20,30,40,50,-100,4.7,-11e+2,-11e+2], Mode(s): [30,-11e+2]
Vector: [1,8,6,0,1,9,4,6,1,9,9,9], Mode(s): [9]
Vector: [1,2,3,4,5,6,7,8,9,10,11], Mode(s): [1,2,3,4,5,6,7,8,9,10,11]
Vector: [8,8,8,2,2,2], Mode(s): [8,2]
Vector: [cat,kat,Cat,emu,emu,Kat], Mode(s): [emu]
Vector: [0,1,2,3,3,3,4,4,4,4,1,0], Mode(s): [4]
Vector: [2,7,1,8,2], Mode(s): [2]
Vector: [2,7,1,8,2,8], Mode(s): [2,8]
Vector: [E,n,z,y,k,l,o,p,ä,d,i,e], Mode(s): [E,n,z,y,k,l,o,p,ä,d,i,e]
Vector: [E,n,z,y,k,l,o,p,ä,d,i,e,ä], Mode(s): [ä]
Vector: [3,1,4,1,5,9,7,6], Mode(s): [1]
Vector: [3,1,4,1,5,9,7,6,3], Mode(s): [1,3]
Vector: [1,3,6,6,6,6,7,7,12,12,17], Mode(s): [6]
Vector: [1,1,2,4,4], Mode(s): [1,4]
```



## Ring


```ring

# Project : Averages/Mode

a = [1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17]
b = [1, 2, 4, 4, 1]
amodes = list(12)
see "mode(s) of a() = " + nl
for i1 = 1 to modes(a,amodes)
     see "" + amodes[i1] + " "
next
see nl
see "mode(s) of b() = " + nl
for i1 = 1 to modes(b,amodes)
     see "" + amodes [i1]  + " "
next
see nl

func modes(a,amodes)
       max = 0
       n = len(a)
       if n = 0
          amodes[1] = a[1]
          return 1
       ok
       c = list(n)
       for i = 1 to n
            for j = i+1 to n
                 if a[i] = a[j]
                    c[i] = c[i] + 1
                 ok
            next
            if c[i] > max
               max = c[i]
            ok
        next
        j = 0
        for i = 1 to n
             if c[i] = max
                j = j + 1
                amodes[j] = a[i]
             ok
        next
        return j

```

Output:

```txt

mode(s) of a() =
6
mode(s) of b() =
1 4

```



## Ruby

Here's two methods, the first more Ruby-ish, the second perhaps a bit more efficient.

```ruby
def mode(ary)
  seen = Hash.new(0)
  ary.each {|value| seen[value] += 1}
  max = seen.values.max
  seen.find_all {|key,value| value == max}.map {|key,value| key}
end

def mode_one_pass(ary)
  seen = Hash.new(0)
  max = 0
  max_elems = []
  ary.each do |value|
    seen[value] += 1
    if seen[value] > max
      max = seen[value]
      max_elems = [value]
    elsif seen[value] == max
      max_elems << value
    end
  end
  max_elems
end

p mode([1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17])  # => [6]
p mode([1, 1, 2, 4, 4]) # => [1, 4]
p mode_one_pass([1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17])  # => [6]
p mode_one_pass([1, 1, 2, 4, 4]) # => [1, 4]
```

{{works with|Ruby|1.8.7}}
If you just want one mode (instead of all of them), here's a one-liner for that:

```ruby
def one_mode(ary)
  ary.max_by { |x| ary.count(x) }
end
```




## Rust


```rust
use std::collections::HashMap;

fn main() {
    let mode_vec1 = mode(vec![ 1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17]);
    let mode_vec2 = mode(vec![ 1, 1, 2, 4, 4]);

    println!("Mode of vec1 is: {:?}", mode_vec1);
    println!("Mode of vec2 is: {:?}", mode_vec2);

    assert!( mode_vec1 == [6], "Error in mode calculation");
    assert!( (mode_vec2 == [1, 4]) || (mode_vec2 == [4,1]), "Error in mode calculation" );
}

fn mode(vs: Vec<i32>) -> Vec<i32> {
    let mut vec_mode = Vec::new();
    let mut seen_map = HashMap::new();
    let mut max_val = 0;
    for i in vs{
        let ctr = seen_map.entry(i).or_insert(0);
        *ctr += 1;
        if *ctr > max_val{
            max_val = *ctr;
        }
    }
    for (key, val) in seen_map {
        if val == max_val{
            vec_mode.push(key);
        }
    }
    vec_mode
}

```

{{out}}

```txt

Mode of vec1 is: [6]
Mode of vec2 is: [1,4] // may also print [4, 1], vector has no order guarantee

```



=={{header|S-lang}}==
I'm accepting strings and numbers, although I'm converting numbers to strings,
as S-Lang Assoc_Type only accepts strings as keys.
<lang S-lang>private variable mx, mxkey, modedat;

define find_max(key) {
  if (modedat[key] > mx) {
    mx = modedat[key];
    mxkey = {key};
  }
  else if (modedat[key] == mx) {
    list_append(mxkey, key);
  }
}

define find_mode(indat)
{
  % reset [file/module-scope] globals:
  mx = 0, mxkey = {}, modedat = Assoc_Type[Int_Type, 0];

  foreach $1 (indat)
    modedat[string($1)]++;

  array_map(Void_Type, &find_max, assoc_get_keys(modedat));

  if (length(mxkey) > 1) {
    $2 = 0;
    () = printf("{");
    foreach $1 (mxkey) {
      () = printf("%s%s", $2 ? ", " : "", $1);
      $2 = 1;
    }
    () = printf("} each have ");
  }
  else
    () = printf("%s has ", mxkey[0], mx);
  () = printf("the most entries (%d).\n", mx);
}

find_mode({"Hungadunga", "Hungadunga", "Hungadunga", "Hungadunga", "McCormick"});

find_mode({"foo", "2.3", "bar", "foo", "foobar", "quality", 2.3, "strnen"});
```

{{out}}

```txt
Hungadunga has the most entries (4).
{foo, 2.3} each have the most entries (2).
```



## Scala

{{works with|Scala|2.8}}
Receiving any collection is easy. Returning the result in the same collection takes some doing.

```scala
import scala.collection.breakOut
import scala.collection.generic.CanBuildFrom
def mode
  [T, CC[X] <: Seq[X]](coll: CC[T])
  (implicit o: T => Ordered[T], cbf: CanBuildFrom[Nothing, T, CC[T]])
  : CC[T] = {
  val grouped = coll.groupBy(x => x).mapValues(_.size).toSeq
  val max = grouped.map(_._2).max
  grouped.filter(_._2 == max).map(_._1)(breakOut)
}
```



## Scheme

{{works with|Berkeley Scheme}}

```scheme
(define (mode collection)
    (define (helper collection counts)
        (if (null? collection)
            counts
            (helper (remove (car collection) collection)
                    (cons (cons (car collection)
                                (appearances (car collection) collection)) counts))))
    (map car
         (filter (lambda (x) (= (cdr x) (apply max (map cdr (helper collection '())))))
                 (helper collection '())))
```



## Seed7

The example below defines the template function <code>createModeFunction</code>, which defines the function <code>mode</code>.
The template <code>createModeFunction</code> is instantiated explicit with <code>createModeFunction(integer)</code>.
Additionally to <code>mode</code> the function <code>str</code> is defined.
The function <code>str</code> is used by the template function
[http://seed7.sourceforge.net/libraries/enable_output.htm#enable_output%28in_type%29 enable_output] to allow writing arrays.
This way the <code>main</code> function can just [http://seed7.sourceforge.net/libraries/enable_output.htm#write%28in_aType%29 write] the mode.


```seed7
$ include "seed7_05.s7i";

const proc: createModeFunction (in type: elemType) is func
  begin

    const func array elemType: mode (in array elemType: data) is func
      result
        var array elemType: maxElems is 0 times elemType.value;
      local
        var hash [elemType] integer: counts is (hash [elemType] integer).value;
        var elemType: aValue is elemType.value;
        var integer: maximum is 0;
      begin
        for aValue range data do
          if aValue in counts then
            incr(counts[aValue]);
          else
            counts @:= [aValue] 1;
          end if;
          if counts[aValue] > maximum then
            maximum := counts[aValue];
            maxElems := [] (aValue);
          elsif counts[aValue] = maximum then
            maxElems &:= aValue;
          end if;
        end for;
      end func;

    const func string: str (in array elemType: data) is func
      result
        var string: stri is "";
      local
        var elemType: anElement is elemType.value;
      begin
        for anElement range data do
          stri &:= " " & str(anElement);
        end for;
        stri := stri[2 ..];
      end func;

      enable_output(array elemType);

  end func;

createModeFunction(integer);

const proc: main is func
  begin
    writeln(mode([] (1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17)));
    writeln(mode([] (1, 1, 2, 4, 4)));
  end func;
```


{{out}}

```txt

6
1 4

```



## Sidef


```ruby
func mode(array) {
    var c = Hash.new;
    array.each{|i| c{i} := 0 ++};
    var max = c.values.max;
    c.keys.grep{|i| c{i} == max};
}
```


'''Calling the function'''

```ruby
say mode([1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17]).join(' ');
say mode([1, 1, 2, 4, 4]).join(' ');
```


{{out}}

```txt

6
1 4

```


If you just want one mode (instead of all of them), here's a one-liner for that:


```ruby
func one_mode(arr) {
    arr.max_by{|i| arr.count(i)};
}
```



## Slate


```Slate
s@(Sequence traits) mode
[| sortedCounts |
  sortedCounts: (s as: Bag) sortedCounts.
  (sortedCounts mapSelect: [| :count :elem | sortedCounts last count = count]) valueSet
].
```



## Smalltalk

{{works with|GNU Smalltalk}}
This code is able to find the mode of any collection of any kind of object.

```smalltalk
OrderedCollection extend [
  mode [ |s|
     s := self asBag sortedByCount.
     ^ (s select: [ :k | ((s at: 1) key) = (k key) ]) collect: [:k| k value]
  ]
].

#( 1 3 6 6 6 6 7 7 12 12 17 ) asOrderedCollection
    mode displayNl.
#( 1 1 2 4 4) asOrderedCollection
    mode displayNl.
```



## SQL

Some databases have a built-in function. In Oracle you can say <code>select stats_mode(val) from...</code> but that returns one value, so doesn't handle non-unique modes. Other databases don't have a built-in. So here's a way to do this in a query.
```sql
-- setup
create table averages (val integer);
insert into averages values (1);
insert into averages values (2);
insert into averages values (3);
insert into averages values (1);
insert into averages values (2);
insert into averages values (4);
insert into averages values (2);
insert into averages values (5);
insert into averages values (2);
insert into averages values (3);
insert into averages values (3);
insert into averages values (1);
insert into averages values (3);
insert into averages values (6);
-- find the mode
with
  counts as
  (
    select
      val,
      count(*) as num
    from
      averages
    group by
      val
  )
select
  val as mode_val
from
  counts
where
  num in (select max(num) from counts);
```

{{out}}

```txt
  MODE_VAL
----------
         2
         3
```


## Tcl

{{works with|Tcl|8.6}}

```tcl
# Can find the modal value of any vector of values
proc mode {n args} {
    foreach n [list $n {*}$args] {
        dict incr counter $n
    }
    set counts [lsort -stride 2 -index 1 -decreasing $counter]
    set best {}
    foreach {n count} $counts {
        if {[lindex $counts 1] == $count} {
            lappend best $n
        } else break
    }
    return $best
}

# Testing
puts [mode 1 3 6 6 6 6 7 7 12 12 17];  # --> 6
puts [mode 1 1 2 4 4];  # --> 1 4
```

Note that this works for any kind of value.


## Swift

{{works with|Swift|4}}

This solution uses an extension of the Collection type to add a mode method. The only additional requirement of the Collection is that its Element conforms to Hashable.


```Swift

// Extend the Collection protocol. Any type that conforms to extension where its Element type conforms to Hashable will automatically gain this method.
extension Collection where Element: Hashable {

    /// Return a Mode of the function, or nil if none exist.
    func mode() -> Element? {
        var frequencies = [Element: Int]()

        // Standard for loop. Can also use the forEach(_:) or reduce(into:) methods on self.
        for element in self {
            frequencies[element] = (frequencies[element] ?? 0) + 1
        }

        // The max(by:) method used here to find one of the elements with the highest associated count.
        if let ( mode, _ ) = frequencies.max(by: { $0.value < $1.value }) {
            return mode
        } else {
            return nil
        }
    }

}

["q", "a", "a", "a", "a", "b", "b", "z", "c", "c", "c"].mode() // returns "a"
[1, 1, 2, 3, 3, 3, 3, 4, 4, 4].mode() // returns 3

let emptyArray: [Int] = []
emptyArray.mode() // returns nil


```



## UNIX Shell

{{works with|bash|4.0}}

```bash
#!/bin/bash

function mode {
    declare -A map
    max=0
    for x in "$@"; do
	tmp=$((${map[$x]} + 1))
	map[$x]=$tmp
	((tmp > max)) && max=$tmp
    done
    for x in "${!map[@]}"; do
	[[ ${map[$x]} == $max ]] && echo -n "$x "
    done
    echo
}
```


```bash
mode 1 2 1 2 a b a b a 2
a 2
```



## Ursala

The mode function defined below works on lists of any type and returns a list of the modes. There is no concept of a general collection in Ursala.  The algorithm is to partition the list by equality, then partition the classes by their lengths, and then select a representative from each member of the set of classes with the maximum length.

```Ursala
#import std

mode = ~&hS+ leql$^&h+ eql|=@K2

#cast %nLW

examples = mode~~ (<1,3,6,6,6,7,7,12,12,17>,<1,1,2,4,4>)
```

The function is tested on a pair of lists, one with a unique mode and one with multiple modes.
{{out}}

```txt
(<6>,<4,1>)
```



## VBA

Using an array of integers to show the built-in Mode_Mult function, which find and displays the modes in an array. The function ignores text and only works for numbers.

```vb
Public Sub main()
    s = [{1,2,3,3,3,4,4,4,5,5,6}]
    t = WorksheetFunction.Mode_Mult(s)
    For Each x In t
        Debug.Print x;
    Next x
End Sub
```
{{out}}

```txt
 3  4
```


## Vedit macro language

Current edit buffer stores the collection. Each line is an item in the collection.
The items can be any type (ascii text, numeric values in ascii, binary values).
However, binary file with fixed record length would require some modifications to the code.

The "mode" item and it's count are displayed on status line.
If there are multiple items with the same count, the smallest one is displayed.

```vedit
BOF                               // Copy all data to a new buffer
Reg_Copy(10, ALL)
Buf_Switch(Buf_Free)
Reg_Ins(10)

Sort(0, File_Size)                // Sort the data

BOF
repeat(ALL) {                     // Count & delete duplicate lines
    #1 = 1
    while(Match("^{.*}\N\1$", REGEXP)==0) {
        Del_Line(1)
        #1++
    }
    Num_Ins(#1, NOCR)             // Insert item count at the beginning of line
    Ins_Char(9)                   // TAB
    Line(1, ERRBREAK)             // Next (different) line
}

Sort(0, File_Size, REVERSE)       // Sort according to the count

BOF                               // Display the results
Reg_Copy_Block(10, CP, EOL_pos)
Buf_Quit(OK)
Statline_Message(@10)
```



## Wren


```wren
class Arithmetic {
    static mode(arr) {
        var map = {}
        for (e in arr) {
            if (map[e] == null) map[e] = 0
            map[e] = map[e] + 1
        }
        var max = map.values.reduce {|x, y| x > y ? x : y}
        return map.keys.where {|x| map[x] == max}.toList
    }
}

System.print(Arithmetic.mode([1,2,3,4,5,5,51,2,3]))

```

{{out}}
[2, 3, 5]

Order may differ


## XEmacs Lisp

This returns a list of the modes. Any type(s) of data can be passed in, and any "equal" predicate function can be specified.

```xelisp
(defun mode ( predicate &rest values)
  "Finds the mode of all values passed in.
Uses `predicate' to compare items."
  (let ((modes nil)                                      ; Declare local variables
	(mode-count 0)
	(value-list nil)
	current-value)
    (loop for value in values do
      (if (setq current-value (assoc* value value-list :test predicate)) ; Construct a linked list of cons cells, (value . count)
	  (incf (cdr current-value))
	(push (cons value 1) value-list)))
    (loop for (value . count) in value-list do           ; Find modes in linked list
      (if (> count mode-count)
	  (setq modes (list value)
		mode-count count)
	(when (eq count mode-count)
	  (push value modes))))
    modes))
```



## Yabasic


```Yabasic
sub floor(x)
    return int(x + .05)
end sub

SUB ASort$(matriz$())
    local last, gap, first, tempi$, tempj$, i, j

    last = arraysize(matriz$(), 1)

    gap = floor(last / 10) + 1
    while(TRUE)
	first = gap + 1
	for i = first to last
	    tempi$ = matriz$(i)
	    j = i - gap
	    while(TRUE)
	        tempj$ = matriz$(j)
	 	if (tempi$ >= tempj$) then
	   	    j = j + gap
	   	    break
	 	end if
	 	matriz$(j+gap) = tempj$
	 	if j <= gap then
	   	    break
	 	end if
	 	j = j - gap
	    wend
	    matriz$(j) = tempi$
        next i
	if gap = 1 then
	    return
	else
	    gap = floor(gap / 3.5) + 1
	end if
    wend
END SUB


sub getMode$(list$) // returns mode and count
	local m$(1), n, i, mode$, count, maxM$, maxC

	n = token(list$, m$(), ", ")
	ASort$(m$())

	for i = 1 to n
		if m$(i) <> mode$ then
			if count > maxC then
				maxM$ = mode$
				maxC = count
			end if
			count = 1
			mode$ = m$(i)
		else
			count = count + 1
		end if
	next i

	return maxM$ + "," + str$(maxC)
end sub

result$ = getMode$("1,3,6,6,6,6,7,7,12,12,17")
n = instr(result$, ",")
print "mode ", left$(result$, n - 1), " occur(s) ", right$(result$, len(result$) - n), " times."

result$ = getMode$("a, a, b, d, d")
print "mode ", left$(result$, n - 1), " occur(s) ", right$(result$, len(result$) - n), " times."
```


{{out}}

```txt

mode 6 occur(s) 4 times.
mode a occur(s) 2 times.
```



## zkl

This is a bit funky in that modes are returned as strings.
{{trans|D}}

```zkl
fcn mode(items){
   d:=Dictionary(); foreach i in (items){ d.incV(i) }
   m:=d.reduce(fcn(m,[(_,v)]){ v.max(m) },0);
   d.filter('wrap([(_,v)]){ v==m }).apply("get",0);
}
```


```zkl
data:=T(1, 2, 3, 1, 2, 4, 2, 5, 3, 3, 1, 3, 6);
println("Mode: ", mode(data));
println("Mode: ", mode(data.append(2)));
println("Mode: ", mode("this is a test".split("")));
```

{{out}}

```txt

Mode: L("3")
Mode: L("2","3")
Mode: L("s","t"," ")

```

