+++
title = "Sort stability"
description = ""
date = 2018-08-30T15:51:56Z
aliases = []
[extra]
id = 4336
[taxonomies]
categories = ["task", "Sorting"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "awk",
  "bbc_basic",
  "c",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "factor",
  "fortran",
  "gap",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "matlab",
  "netrexx",
  "nim",
  "ocaml",
  "oorexx",
  "openedge_progress",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "purebasic",
  "python",
  "r",
  "racket",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "stata",
  "tcl",
  "txr",
  "zkl",
]
+++

## Task

When sorting records in a table by a particular column or field, a [[wp:Stable_sort#Stability|stable sort]] will always retain the  relative order of records that have the same key.

For example, in this table of countries and cities, a stable sort on the '''second''' column, the cities, would  keep the US Birmingham above the UK Birmingham. (Although an unstable sort ''might'', in this case, place the US Birmingham above the UK Birmingham, a stable sort routine would ''guarantee'' it).

```txt
UK  London
US  New York
US  Birmingham
UK  Birmingham
```

Similarly, stable sorting on just the first column would generate “UK London” as the first item and “US Birmingham” as the last item (since the order of the elements having the same first word – “UK” or “US” – would be maintained).

#Examine the documentation on any in-built sort routines supplied by a language.
#Indicate if an in-built routine is supplied
#If supplied, indicate whether or not the in-built routine is stable.



(This [[wp:Stable_sort#Comparison_of_algorithms|Wikipedia table]] shows the stability of some common sort routines).





## Ada

[[Ada 83]] and [[Ada 95]] do not provide a standard sort utility.

[[Ada 2005]] provides two generic procedures for sorting arrays. One (<code>Ada.Containers.Generic_Array_Sort</code>) is for unconstrained array types and one (<code>Ada.Containers.Generic_Constrained_Array_Sort</code>) is for constrained array types.
Both are not guaranteed stable and in all implementation they are not.

Also, <code>Vectors</code> and <code>Doubly_Linked_Lists</code> containers have their own internal generic sort. <code>Doubly_Linked_Lists</code> sort is stable.


## AutoHotkey

Autohotkey has got a build-in sorting method for tables, which is stable.

```AutoHotkey
Table =
(
UK, London
US, New York
US, Birmingham
UK, Birmingham
)

Gui, Margin, 6
Gui, -MinimizeBox
Gui, Add, ListView, r5 w260 Grid, Orig.Position|Country|City
Loop, Parse, Table, `n, `r
{
    StringSplit, out, A_LoopField, `,, %A_Space%
    LV_Add("", A_Index, out1, out2)
}
LV_ModifyCol(1, "77 Center")
LV_ModifyCol(2, "100 Center")
LV_ModifyCol(3, 79)
Gui, Add, Button, w80, Restore Order
Gui, Add, Button, x+10 wp, Sort Countries
Gui, Add, Button, x+10 wp, Sort Cities
Gui, Show,, Sort stability
Return

GuiClose:
GuiEscape:
ExitApp

ButtonRestoreOrder:
    LV_ModifyCol(1, "Sort")
Return

ButtonSortCountries:
    LV_ModifyCol(2, "Sort")
Return

ButtonSortCities:
    LV_ModifyCol(3, "Sort")
Return
```



## AWK


```AWK

# syntax: GAWK -f SORT_STABILITY.AWK [-v width=x] -v field=x SORT_STABILITY.TXT
#
# sort by country: GAWK -f SORT_STABILITY.AWK -v field=1 SORT_STABILITY.TXT
# sort by city:    GAWK -f SORT_STABILITY.AWK -v field=2 SORT_STABILITY.TXT
#
# awk sort is not stable. Stability may be achieved by appending the
# record number, I.E. NR, to each key.
#
BEGIN {
    FIELDWIDTHS = "4 20" # 2 fields: country city
    PROCINFO["sorted_in"] = "@ind_str_asc"
    if (width == "") {
      width = 6
    }
}
{ arr[$field sprintf("%0*d",width,NR)] = $0 }
END {
    if (length(NR) > width) {
      printf("error: sort may still be unstable; change width to %d\n",length(NR))
      exit(1)
    }
    printf("after sorting on field %d:\n",field)
    for (i in arr) {
      printf("%s\n",arr[i])
    }
    exit(0)
}

```

<p>input:</p>

```txt

UK  London
US  New York
US  Birmingham
UK  Birmingham

```

<p>output from: GAWK -f SORT_STABILITY.AWK -v field=1 SORT_STABILITY.TXT</p>

```txt

after sorting on field 1:
UK  London
UK  Birmingham
US  New York
US  Birmingham

```

<p>output from: GAWK -f SORT_STABILITY.AWK -v field=2 SORT_STABILITY.TXT</p>

```txt

after sorting on field 2:
US  Birmingham
UK  Birmingham
UK  London
US  New York

```



## BBC BASIC

The supplied SORTLIB library currently uses a Shell Sort, so it is not stable.


## C

There is no built-in function in C ''language''.  <code>stdlib</code> which comes with any C ''implementation'' is required to provide a [[wp:Qsort|<code>qsort()</code>]] routine that can sort arbitrary datatypes.  Although the sorting algorithm is not specified, most (all?) implementions use a combined quicksort/insertion sort method for efficiency.  Quicksort is by nature unstable.


## C++

C++ standard library's [http://www.sgi.com/tech/stl/sort.html std::sort()] function is not guaranteed stable. The stable analog of it is the [http://www.sgi.com/tech/stl/stable_sort.html std::stable_sort()] function. In addition, [http://www.sgi.com/tech/stl/List.html std::list]'s <tt>sort()</tt> method is guaranteed stable.

## C#
The .NET library documentation for <tt>Array.Sort()</tt> says that it uses quicksort and is unstable.[http://msdn.microsoft.com/en-us/library/kwx6zbd4.aspx#remarksToggle]


## Clojure

Clojure's ''sort'' and ''sort-by'' functions are implemented using Java's ''java.utils.Array.sort'' methods, which are guaranteed stable.


## COBOL

The SORT statement causes a set of records or table elements to be arranged in a user-specified sequence.

If the DUPLICATES phrase is specified and the contents of all the key data items associated with one record or
table element are equal to the contents of the corresponding key data items associated with one or more other
records or table elements, the order of return of these records or the relative order of the contents of these
table elements is:

a) The order of the associated input files as specified in the SORT statement. Within a given input file the
order is that in which the records are accessed from that file.

b) The order in which these records are released by an input procedure, when an input procedure is specified.

c) The relative order of the contents of these table elements before sorting takes place.

When the DUPLICATES phrase is used, the sort is stable.


## Common Lisp

Common Lisp provides the two functions [http://www.lispworks.com/documentation/HyperSpec/Body/f_sort_.htm <code>sort</code> and <code>stable-sort</code>].

Each of these functions can sort arbitrary objects using a given predicate function, the input to which can be altered by the optional <code>key</code> parameter.

(eg; sorting file names based upon file sizes, the predicate might be <code>&lt;</code> and the <code>key</code> could be a function that transforms the file's name into its size)


## D

In the std.algorithm Phobos v.2 module there is SwapStrategy that defines the swapping strategy for algorithms like sort and partition.

<em>Unstable</em>, <em>stable</em> and <em>semistable</em> (in algorithms that partition ranges in two, <em>semistable</em> preserves stability on just the left of the partition point) are supported.
=={{header|Déjà Vu}}==
The included sorting algorithm, <code>sort</code>, is stable.


## Elixir

Enum.sort and Enum.sort_by of Elixir are stable.
These functions use merge sort algorithm.
The sorting algorithm will be stable as long as the given function returns true for values considered equal:

```elixir
cities = [ {"UK", "London"},
           {"US", "New York"},
           {"US", "Birmingham"},
           {"UK", "Birmingham"} ]

IO.inspect Enum.sort(cities)
IO.inspect Enum.sort(cities, fn a,b -> elem(a,0) >= elem(b,0) end)
IO.inspect Enum.sort_by(cities, fn {country, _city} -> country end)
IO.inspect Enum.sort_by(cities, fn {_country, city} -> city end)
```

```txt

[{"UK", "Birmingham"}, {"UK", "London"}, {"US", "Birmingham"}, {"US", "New York"}]
[{"US", "New York"}, {"US", "Birmingham"}, {"UK", "London"}, {"UK", "Birmingham"}]
[{"UK", "London"}, {"UK", "Birmingham"}, {"US", "New York"}, {"US", "Birmingham"}]
[{"US", "Birmingham"}, {"UK", "Birmingham"}, {"UK", "London"}, {"US", "New York"}]

```


'''Note:''' If the function does not return true, the sorting is not stable and the order of equal terms may be shuffled:

```elixir
IO.inspect Enum.sort(cities, fn a,b -> elem(a,0) > elem(b,0) end)
```

```txt

[{"US", "Birmingham"}, {"US", "New York"}, {"UK", "Birmingham"}, {"UK", "London"}]

```



## Erlang

The function lists:sort/1 is not documented as stable. The function lists:keysort/2 is documented as stable.


## Factor

The <code>sorting</code> vocabulary implements a stable sort. [http://docs.factorcode.org/content/article-sequences-sorting.html <code>sorting</code> docs]


## Fortran

The language does not offer an in-built sort facility. Numerous libraries exist, which may or may not have documentation on their sort routine's stability.

=={{header|F_Sharp|F#}}==
[http://msdn.microsoft.com/en-us/library/ee370375.aspx <code>Array.sort</code>] is not stable.
[http://msdn.microsoft.com/en-us/library/ee370323.aspx <code>List.sort</code>] and [http://msdn.microsoft.com/en-us/library/ee353483.aspx <code>Seq.sort</code>] are stable.


## GAP


```gap
# According to section 21.18 of the reference manual, Sort is not stable (it's a Shell sort).
# However, SortingPerm is stable. We will see it on an example, showing indexes of elements after the sort.

n := 20;
L := List([1 .. n], i -> Random("AB"));
# "AABABBBABBABAABABBAB"


p := SortingPerm(L);
# (3,10,15,17,18,19,9,14,7,13,6,12,16,8,4)(5,11)

a := Permuted(L, p);;
b := Permuted([1 .. n], p);;

PrintArray(TransposedMat(List([1 .. n], i -> [a[i], b[i]])));
# [ [  'A',  'A',  'A',  'A',  'A',  'A',  'A',  'A',  'A',  'B',  'B',  'B',  'B',  'B',  'B',  'B',  'B',  'B',  'B',  'B' ],
#   [    1,    2,    4,    8,   11,   13,   14,   16,   19,    3,    5,    6,    7,    9,   10,   12,   15,   17,   18,   20 ] ]
```



## Go

New to Go 1.2 is the function Stable() in the sort package and is documented to be a stable sort.  Other sort functions are documented to have no guarantee of stability.


## Groovy

Groovy's [http://groovy.codehaus.org/groovy-jdk/java/util/Collection.html#sort() Collection.sort()], Object[].[http://groovy.codehaus.org/groovy-jdk/java/lang/Object%5b%5d.html#sort() sort()], [http://groovy.codehaus.org/groovy-jdk/java/util/Map.html#sort() Map.sort()], and their various and sundry overloads all use the same stable sort algorithm.

Example:
```groovy
def cityList = ['UK  London', 'US  New York', 'US  Birmingham', 'UK  Birmingham',].asImmutable()
[
    'Sort by city': { city -> city[4..-1] },
    'Sort by country': { city -> city[0..3] },
].each{ String label, Closure orderBy ->
    println "\n\nBefore ${label}"
    cityList.each { println it }
    println "\nAfter ${label}"
    cityList.sort(false, orderBy).each{ println it }
}
```


Output:

```txt
Before Sort by city
UK  London
US  New York
US  Birmingham
UK  Birmingham

After Sort by city
US  Birmingham
UK  Birmingham
UK  London
US  New York


Before Sort by country
UK  London
US  New York
US  Birmingham
UK  Birmingham

After Sort by country
UK  London
UK  Birmingham
US  New York
US  Birmingham
```



## Haskell

Haskell's <tt>sort</tt> and <tt>sortBy</tt> functions are guaranteed stable.[http://www.haskell.org/onlinereport/list.html#sect17.3]

=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon use Quick Sort internally. As described in [http://unicon.sourceforge.net/book/ib.pdf The Implementation of Icon and Unicon: a Compendium]] sorting is done by the standard C library routine qsort which is not guaranteed to be stable.

Note(1): The built-in sort handles lists of mixed types by sorting first by type and then value.  No coercion of types is performed.  The sort order of types is: &null, integer, real, string, cset, procedure, list, set, table, record.


## J

J's <i>grade</i> primitive <code>/:</code>, and therefore its sort (such as <code>/:~</code>), are guaranteed stable.

From the [http://www.jsoftware.com/help/dictionary/d422.htm dictionary page] for <code>/:</code> : "Elements of /:y that select equal elements of y are in ascending order."


## Java

Java's [http://java.sun.com/javase/6/docs/api/java/util/Collections.html#sort(java.util.List) Collections.sort()] and [http://java.sun.com/javase/6/docs/api/java/util/Arrays.html#sort(java.lang.Object%5B%5D) Arrays.sort()] methods are guaranteed stable.

The following sample demonstrates Java's sort stability:

```Java
import java.util.Arrays;
import java.util.Comparator;

public class RJSortStability {

  public static void main(String[] args) {
    String[] cityList = { "UK  London", "US  New York", "US  Birmingham", "UK  Birmingham", };

    String[] cn = cityList.clone();
    System.out.println("\nBefore sort:");
    for (String city : cn) {
      System.out.println(city);
    }

    // sort by city
    Arrays.sort(cn, new Comparator<String>() {
      public int compare(String lft, String rgt) {
        return lft.substring(4).compareTo(rgt.substring(4));
      }
    });

    System.out.println("\nAfter sort on city:");
    for (String city : cn) {
      System.out.println(city);
    }

    cn = cityList.clone();
    System.out.println("\nBefore sort:");
    for (String city : cn) {
      System.out.println(city);
    }

    // sort by country
    Arrays.sort(cn, new Comparator<String>() {
      public int compare(String lft, String rgt) {
        return lft.substring(0, 2).compareTo(rgt.substring(0, 2));
      }
    });

    System.out.println("\nAfter sort on country:");
    for (String city : cn) {
      System.out.println(city);
    }

    System.out.println();
  }
}
```

;Output

```txt

Before sort:
UK  London
US  New York
US  Birmingham
UK  Birmingham

After sort on city:
US  Birmingham
UK  Birmingham
UK  London
US  New York

Before sort:
UK  London
US  New York
US  Birmingham
UK  Birmingham

After sort on country:
UK  London
UK  Birmingham
US  New York
US  Birmingham

```



## JavaScript

The ECMA standard does not specify what sorting algorithm to use, so it depends upon the implementation.


```javascript
ary = [["UK", "London"], ["US", "New York"], ["US", "Birmingham"], ["UK", "Birmingham"]]
print(ary);

ary.sort(function(a,b){return (a[1]<b[1] ? -1 : (a[1]>b[1] ? 1 : 0))});
print(ary);

/* a stable sort will output ["US", "Birmingham"] before ["UK", "Birmingham"] */
```


Stable implementations:
```txt
UK,London,US,New York,US,Birmingham,UK,Birmingham
US,Birmingham,UK,Birmingham,UK,London,US,New York
```


Not stable:
```txt
UK,London,US,New York,US,Birmingham,UK,Birmingham
UK,Birmingham,US,Birmingham,UK,London,US,New York
```



## jq

As of January 18, 2016 (Commit SHA 7835a72), the builtin sorting filters (notably sort/0 and sort_by/1) are stable; prior to that, stability was platform-dependent. This means that stability is NOT guaranteed in jq 1.5 or earlier.  In the following, a version of jq with sorting stability has been used.


```jq
[["UK", "London"],
 ["US", "New York"],
 ["US", "Birmingham"],
 ["UK", "Birmingham"]]
| sort_by(.[1])
```


Invocation:

```txt
$ jq -c -n -f rc-sort-stability.jq
```


```txt
[["US","Birmingham"],["UK","Birmingham"],["UK","London"],["US","New York"]]
```



## Julia

Julia's built-in <code>sort</code> function is documented to be stable by default (although non-stable sort algorithms can optionally be selected).

```txt

julia> A = [("UK", "London"), ("US", "New York"), ("US", "Birmingham"), ("UK", "Birmingham")];
julia> sort(A, by=x -> x[2])
4-element Array{(ASCIIString,ASCIIString),1}:
 ("US","Birmingham")
 ("UK","Birmingham")
 ("UK","London")
 ("US","New York")

```



## Kotlin

The collections in Kotlin's standard library are thin wrappers around the corresponding JDK collections and, since the latter's sort methods are stable, so too are Kotlin's standard sort functions.

```scala
// version 1.1.51

fun main(args: Array<String>) {
    val cities = listOf("UK London", "US New York", "US Birmingham", "UK Birmingham")
    println("Original   : $cities")
    // sort by country
    println("By country : ${cities.sortedBy { it.take(2) } }")
    // sort by city
    println("By city    : ${cities.sortedBy { it.drop(3) } }")
}
```


```txt

Original   : [UK London, US New York, US Birmingham, UK Birmingham]
By country : [UK London, UK Birmingham, US New York, US Birmingham]
By city    : [US Birmingham, UK Birmingham, UK London, US New York]

```



## Lasso

Arrays can be sorted in two “built in" ways in Lasso:


```Lasso
//Single param array:
array->sort

//An array of pairs, order by the right hand element of the pair:
with i in array order by #i->second do => { … }

//The array can also be ordered by multiple values:
with i in array order by #i->second, #i->first do => { … }
```


Sorting of arrays by either method uses “Qucksort” and is therefore unstable. A simulation of increasing sort stability would be introduced with additional params such as the example of ordering by the second then the first pair values in the example above - but would not be guaranteed stable.

* Note this explanation of sorting does not cover ordering by properties of complex objects, which is also possible using query expressions.

Sort by second value only:

```Lasso
local(a = array('UK'='London','US'='New York','US'='Birmingham','UK'='Birmingham'))
with i in #a order by #i->second do => {^ #i->first+' - '+#i->second+'\r' ^}
```

```txt
US - Birmingham
UK - Birmingham
UK - London
US - New York
```


Sort by second then by first:

```Lasso
local(a = array('UK'='London','US'='New York','US'='Birmingham','UK'='Birmingham'))
with i in #a order by #i->second, #i->first do => {^ #i->first+' - '+#i->second+'\r' ^}
```

```txt
UK - Birmingham
US - Birmingham
UK - London
US - New York
```



## Liberty BASIC

LB has build-in SORT routine.
Documentation does not says if it's stable or not.
Example from RC keeps order.

Here's an example showing that SORT indeed unstable.

```lb

randomize 0.5
N=15
dim a(N,2)

for i = 0 to N-1
    a(i,1)= int(i/5)
    a(i,2)= int(rnd(1)*5)
next

print "Unsorted by column #2"
print "by construction sorted by column #1"
for i = 0 to N-1
    print a(i,1), a(i,2)
next

sort a(), 0, N-1, 2
print

print "After sorting by column #2"
print "Notice wrong order by column #1"
for i = 0 to N-1
    print a(i,1), a(i,2),
    if i=0 then
        print
    else
        if  a(i,2) = a(i-1,2) AND  a(i,1) < a(i-1,1) then print "bad order" else print
    end if
next

```

```txt

Unsorted by column #2
by construction sorted by column #1
0             4
0             1
0             1
0             0
0             4
1             1
1             1
1             2
1             1
1             0
2             4
2             3
2             2
2             0
2             4

After sorting by column #2
Notice wrong order by column #1
0             0
1             0
2             0
1             1
1             1
1             1
0             1             bad order
0             1
1             2
2             2
2             3
2             4
0             4             bad order
0             4
2             4

```



## Lua

The built-in function table.sort is not guaranteed stable.


## M2000 Interpreter

M2000 has two types of Inventories, objects using a hash algorithm, the normal with unique keys, so a sort statement on this object use quicksort, and a second type, the "queue" type, which can get same keys, and has stable sort.

Here is the stable sort

```M2000 Interpreter

Module Stable {
      Inventory queue alfa
      Stack New {
            Data "UK", "London","US", "New York","US", "Birmingham", "UK","Birmingham"
            While not empty {
                  Append alfa, Letter$:=letter$
            }
      }
      sort alfa
      k=Each(alfa)
      Document A$
      NL$={
      }
      While k {
            A$= Eval$(k, k^)+" "+eval$(k)+NL$
      }
      Clipboard A$ ' write to clipboard
      Report A$
}
Call Stable

Output:
UK London
UK Birmingham
US New York
US Birmingham


```




We can sort in on key only. Lets make keys with two fields (based on fields lengths, except for last one)


```M2000 Interpreter

Module Stable1 {
      Inventory queue alfa
      Stack New {
            Data "UK London","US New York","US Birmingham", "UK Birmingham"
            While not empty {
                  Append alfa, Letter$
            }
      }
      sort alfa
      k=Each(alfa)
      Document A$
      NL$={
      }
      While k {
            A$= Eval$(k, k^)+NL$
      }
      Clipboard A$ ' write to clipboard
      Report A$
}
Call Stable1

Output:
UK Birmingham
UK London
US Birmingham
US New York


```


Now second column is sorting (but it is one column all, no two columns). So lets see the unstable sort. Because all keys now are unique we just remove queue from Inventory definition.

```M2000 Interpreter

Module Stable2 {
      Inventory alfa
      Stack New {
            Data "UK London","US New York","US Birmingham", "UK Birmingham"
            While not empty {
                  Append alfa, Letter$
            }
      }
      sort alfa
      k=Each(alfa)
      Document A$
      NL$={
      }
      While k {
            A$= Eval$(k, k^)+NL$
      }
      Clipboard A$ ' write to clipboard
      Report A$
}
Call Stable2

Output:
UK Birmingham
UK London
US Birmingham
US New York


```


So now we see that using unique keys in either type of inventories we get same output.
By default in inventory queue numbers in keys (in any position) are sorted as numbers.
We can use '''sort alfa as number''' for normal inventory, or ''sort alfa as text''

For ascending/descending sort we can use sort descending alfa as number

If we delete a key in normal inventory we miss the sort order. We can't delete keys in queue inventories, but we can drop from the last append a number of keys. Also Exist() function in queue inventories always find the last entry (for same keys), until that dropped, so with next use of Exist(pointer_to_inventory, key_case_sensitive$) we find the previous one. We can use keys as numbers, but they stored as strings.


## Mathematica

Sort is not always stable. Ordering, which gives a list of indices such as to put the elements of the list in order, is stable. An example would be to sort the list (of lists) {{1, 2, 3}, {4, 5, 6}, {5, 4, 3}, {9, 5, 1}}, and doing so by looking at the 2nd value of each list:

```Mathematica
mylist = {{1, 2, 3}, {4, 5, 6}, {5, 4, 3}, {9, 5, 1}};
Sort[mylist, (#1[[2]] < #2[[2]]) &]
#[[Ordering[#[[All, 2]]]]] &[mylist]
```

gives:

```Mathematica
```

Showing that Sort is unstable, and that by using input[[Ordering[input]]] Ordering provides a way to make a stable sort.


## MATLAB

MathWorks' policy seems to be that their built-in sorting algorithm will always be a stable sort across all versions ([http://www.mathworks.com/company/newsletters/news_notes/dec04/adventure.html reference]). To check to see if your version of MATLAB provides a stable sort,check the output of command "help sort".


## NetRexx

Java's [http://java.sun.com/javase/6/docs/api/java/util/Collections.html#sort(java.util.List) Collections.sort()] and [http://java.sun.com/javase/6/docs/api/java/util/Arrays.html#sort(java.lang.Object%5B%5D) Arrays.sort()] methods are guaranteed stable.  The following sample takes advantage of this to demonstrate sort stability.


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

class RCSortStability

method main(args = String[]) public constant

  cityList = [String "UK  London", "US  New York", "US  Birmingham", "UK  Birmingham"]

  cn = String[cityList.length]

  say
  say "Before sort:"
  System.arraycopy(cityList, 0, cn, 0, cityList.length)
  loop city = 0 to cn.length - 1
    say cn[city]
    end city

  cCompNm = Comparator CityComparator()
  Arrays.sort(cn, cCompNm)

  say
  say "After sort on city:"
  loop city = 0 to cn.length - 1
    say cn[city]
    end city

  say
  say "Before sort:"
  System.arraycopy(cityList, 0, cn, 0, cityList.length)
  loop city = 0 to cn.length - 1
    say cn[city]
    end city

  cCompCtry = Comparator CountryComparator()
  Arrays.sort(cn, cCompCtry)

  say
  say "After sort on country:"
  loop city = 0 to cn.length - 1
    say cn[city]
    end city
  say

  return

class RCSortStability.CityComparator implements Comparator

method compare(lft = Object, rgt = Object) public binary returns int
  return (String lft).substring(4).compareTo((String rgt).substring(4))

class RCSortStability.CountryComparator implements Comparator

method compare(lft = Object, rgt = Object) public binary returns int
  return (String lft).substring(0, 2).compareTo((String rgt).substring(0, 2))

```

;Output

```txt

Before sort:
UK  London
US  New York
US  Birmingham
UK  Birmingham

After sort on city:
US  Birmingham
UK  Birmingham
UK  London
US  New York

Before sort:
UK  London
US  New York
US  Birmingham
UK  Birmingham

After sort on country:
UK  London
UK  Birmingham
US  New York
US  Birmingham

```



## Nim

Default Nim sort in the algorithm module is stable.


## OCaml

OCaml's [http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html#VALsort List.sort] and [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html#VALsort Array.sort] functions are not guaranteed to be stable. The stable versions are [http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html#VALstable_sort List.stable_sort] and [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html#VALstable_sort Array.stable_sort], respectively.


## ooRexx

Open Object Rexx provides sort methods (<code>sort</code> and <code>sortWith(comparator)</code>) for its collection classes.  By default these sort methods are implemented via an unstable <em>Quicksort</em> algorithm but the language does provide stable sorting methods (<code>stableSort</code> and <code>stableSortWith(comparator)</code>) implemented via a stable <em>Mergesort</em> algorithm.

```ooRexx
/* Rexx */
Do
  cities = .array~of('UK  London', 'US  New York', 'US  Birmingham', 'UK  Birmingham',)

  Say; Say 'Original table'
  Call display cities

  Say; Say 'Unstable sort on city'
  sorted = cities~copy
  sorted~sortWith(.ColumnComparator~new(4, 20))
  Call display sorted

  Say; Say 'Stable sort on city'
  sorted = cities~copy
  sorted~stableSortWith(.ColumnComparator~new(4, 20))
  Call display sorted

  Say; Say 'Unstable sort on country'
  sorted = cities~copy
  sorted~sortWith(.ColumnComparator~new(1, 2))
  Call display sorted

  Say; Say 'Stable sort on country'
  sorted = cities~copy
  sorted~stableSortWith(.ColumnComparator~new(1, 2))
  Call display sorted

  Return
End
Exit

display: Procedure
Do
  Use arg CT

  Say '-'~copies(80)
  Loop c_ over CT
    Say c_
    End c_

  Return
End
Exit

```

;Output

```txt

Original table
--------------------------------------------------------------------------------
UK  London
US  New York
US  Birmingham
UK  Birmingham

Unstable sort on city
--------------------------------------------------------------------------------
UK  Birmingham
US  Birmingham
UK  London
US  New York

Stable sort on city
--------------------------------------------------------------------------------
US  Birmingham
UK  Birmingham
UK  London
US  New York

Unstable sort on country
--------------------------------------------------------------------------------
UK  London
UK  Birmingham
US  Birmingham
US  New York

Stable sort on country
--------------------------------------------------------------------------------
UK  London
UK  Birmingham
US  New York
US  Birmingham

```



## OpenEdge/Progress

The results can be forced to stable by ''additionally'' sorting on the ROWID of the record. If you leave the additional sort out, the indexes on the temp-table can influence the result.

```progress
DEFINE TEMP-TABLE tt
   FIELD country  AS CHAR FORMAT 'x(2)'
   FIELD city     AS CHAR FORMAT 'x(16)'
   .

DEFINE VARIABLE cc AS CHARACTER EXTENT 2.

CREATE tt. ASSIGN tt.country = 'UK' tt.city = 'London'.
CREATE tt. ASSIGN tt.country = 'US' tt.city = 'New York'.
CREATE tt. ASSIGN tt.country = 'US' tt.city = 'Birmingham'.
CREATE tt. ASSIGN tt.country = 'UK' tt.city = 'Birmingham'.

cc[1] = 'by country~n~n'.
FOR EACH tt BY tt.country BY ROWID( tt ):
   cc[1] = cc[1] + tt.country + '~t' + tt.city + '~n'.
END.

cc[2] = 'by city~n~n'.
FOR EACH tt BY tt.city BY ROWID( tt ):
   cc[2] = cc[2] + tt.country + '~t' + tt.city + '~n'.
END.

MESSAGE
   cc[1] SKIP(1) cc[2]
VIEW-AS ALERT-BOX.
```


'''Output:'''

```txt
---------------------------
Message
---------------------------
by country

UK	London
UK	Birmingham
US	New York
US	Birmingham


by city

US	Birmingham
UK	Birmingham
UK	London
US	New York
---------------------------
OK
---------------------------
```



## Oz

Oz' [http://www.mozart-oz.org/home/doc/base/list.html#label295 Sort] function is not guaranteed to be stable in the documentation.

However, internally it uses [[Merge sort]] and in practice ''is'' stable '''if''' a reflexive ordering is used, e.g. <code>Value.'=<'</code> or <code>Value.'>='</code>.

Example:

```oz
declare
  Cities = ['UK'#'London'
            'US'#'New York'
            'US'#'Birmingham'
            'UK'#'Birmingham']
in
  %% sort by city; stable because '=<' is reflexiv
  {Show {Sort Cities fun {$ A B} A.2 =< B.2 end}}

  %% sort by country; NOT stable because '<' is not reflexiv
  {Show {Sort Cities fun {$ A B} A.1 < B.1 end}}
```



## PARI/GP

Pari's <code>vecsort</code> is stable, see 3.8.60 in the User's Guide.  In particular, it uses a merge sort.


## Pascal

Standard Pascal has no built-in routine for sorting. The RTL of FreePascal uses Quicksort for TList, TFPList and TStringList in the Classes unit. In many places in the standard libraries fgl and in generics.collections the sort is configurable provided the programmer implements a sort.


## Perl

The stability of Perl's in-built [http://perldoc.perl.org/functions/sort.html sort] function is version-dependent. If you want to guarantee a stable sort from it, you should use the following [http://perldoc.perl.org/sort.html sort pragma]:

```perl
use sort 'stable';
```



## Perl 6

The [http://perlcabal.org/syn/S32/Containers.html#sort sort] built-in (available as sub and method) is stable.

Short demonstration for sorting only on the second item of each array:

```perl6
use v6;
my @cities =
    ['UK', 'London'],
    ['US', 'New York'],
    ['US', 'Birmingham'],
    ['UK', 'Birmingham'],
    ;

.say for @cities.sort: { .[1] };
```



## Phix

The standard sort method is merge sort, which is apparently stable, though I would be reluctant to guarantee that, or rely on it, especially given
that a simple tag sort is guaranteed stable by dint of ordering by tag should the keys be equal.

```Phix
sequence test = {{"UK","London"},
                 {"US","New York"},
                 {"US","Birmingham"},
                 {"UK","Birmingham"}}

---------------------
-- probably stable --
---------------------
function cmp(object a, object b)
    return compare(a[1],b[1])
end function
test = custom_sort(routine_id("cmp"),test)
pp(test,{pp_Nest,1})

-----------------------
-- guaranteed stable --
-----------------------
function tag_cmp(integer i, integer j)
    return compare({test[i][1],i},{test[j][1],j})
--  return compare(test[i][1],test[j][1])
end function
sequence tags = custom_sort(routine_id("tag_cmp"),shuffle(tagset(4)))
for i=1 to length(tags) do
    ?test[tags[i]]
end for
```

```txt

{{"UK", "London"},
 {"UK", "Birmingham"},
 {"US", "New York"},
 {"US", "Birmingham"}}
{"UK","London"}
{"UK","Birmingham"}
{"US","New York"}
{"US","Birmingham"}

```

The commented-out line in tag_cmp is unstable, or rather probably stable wrt the shuffle, whereas the active line guarantees
original (pre-shuffle) ordering, even if an unstable underlying sort method were used.
Obviously, written the way it is above, the guaranteed part only guarantees not to upset what the probably part left behind, and of course test=sort(test) guarantees alphabetical on second column within matching first column. Lastly, preserving a primary tag sort ordering within a secondary tag sort is a bit more mind-bending, but even that is not particularly difficult.


## PHP

PHP uses QuickSort for most of its [http://us2.php.net/manual/en/array.sorting.php sort functions] so it is unstable. [http://www.php.net/manual/en/function.sort.php]


## PicoLisp

The [http://software-lab.de/doc/refS.html#sort sort] function is unstable


## PureBasic

PureBasic's includes two built-in sort functions for arrays, <tt>SortArray()</tt> and <tt>SortStructuredArray()</tt>, and two built-in sort functions for linked lists, <tt>SortList()</tt> and <tt>SortStructuredList()</tt>.  Sorting of linked lists is stable and uses a merge-sort, while sorting for arrays is unstable and uses a quicksort.


## Python

Python's in-built [http://docs.python.org/library/functions.html#sorted sorted] function as well as the [http://docs.python.org/library/stdtypes.html#mutable-sequence-types sort method of lists] are guaranteed stable (since version 2.3). (For even more information on the underlying routine, [[wp:timsort]], see [http://svn.python.org/projects/python/trunk/Objects/listsort.txt this]).


## R

R uses shell sort (stable) or quick sort (unstable). An easy way to show the difference is names to vector entries, then check if names are still ordered after sorting.


```R

# First, define a bernoulli sample, of length 26.
x <- sample(c(0, 1), 26, replace=T)

x
# [1] 1 1 1 1 0 1 1 0 1 0 1 1 1 0 1 1 0 1 0 1 0 1 1 0 1 0

# Give names to the entries. "letters" is a builtin value
names(x) <- letters

x
# a b c d e f g h i j k l m n o p q r s t u v w x y z
# 1 1 1 1 0 1 1 0 1 0 1 1 1 0 1 1 0 1 0 1 0 1 1 0 1 0

# The unstable one, see how "a" appears after "l" now
sort(x, method="quick")
# z h s u e q x n j r t v w y p o m l a i g f d c b k
# 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

# The stable sort, letters are ordered in each section
sort(x, method="shell")
# e h j n q s u x z a b c d f g i k l m o p r t v w y
# 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

```



## Racket


Racket comes with a standard <tt>sort</tt> function, which is documented [[http://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._sort%29%29 here]].  It is documented as stable.


```Racket

#lang racket

(sort '(("UK" "London")
        ("US" "New York")
        ("US" "Birmingham")
        ("UK" "Birmingham"))
      string<? #:key first)
;; -> (("UK" "London") ("UK" "Birmingham")
;;     ("US" "New York") ("US" "Birmingham"))

(sort '(("UK" "London")
        ("US" "New York")
        ("US" "Birmingham")
        ("UK" "Birmingham"))
      string<? #:key second)
;; -> '(("US" "Birmingham") ("UK" "Birmingham")
;;      ("UK" "London") ("US" "New York"))

```



## REBOL


```rebol
; REBOL's sort function is not stable by default. You need to use a custom comparator to make it so.

blk: [
    [UK London]
    [US New-York]
    [US Birmingham]
    [UK Birmingham]
]
sort/compare blk func [a b] [either a/2 < b/2 [-1] [either a/2 > b/2 [1] [0]]]

; Note that you can also do a stable sort without nested blocks.
blk: [
    UK London
    US New-York
    US Birmingham
    UK Birmingham
]
sort/skip/compare blk 2 func [a b] [either a < b [-1] [either a > b [1] [0]]]
```



## REXX

Classic REXX has no built-in routines for sorting, so this programming example uses a classic ''bubble sort''   (which is stable).

```rexx
/*REXX program  sorts  a (stemmed)  array  using a  (stable)   bubble─sort   algorithm. */
call gen@                                        /*generate the array elements (strings)*/
call show  'before sort'                         /*show the  before array  elements.    */
     say copies('▒', 50)                         /*show a separator line between shows. */
call bubbleSort   #                              /*invoke the bubble sort.              */
call show  ' after sort'                         /*show the   after array  elements.    */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bubbleSort: procedure expose @.; parse arg n;    m=n-1  /*N:  number of array elements. */
               do m=m  for m  by -1  until ok;   ok=1   /*keep sorting array until done.*/
                   do j=1  for m;  k=j+1;  if @.j<=@.k  then iterate /*Not out─of─order?*/
                   _=@.j;  @.j=@.k;  @.k=_;      ok=0   /*swap 2 elements; flag as ¬done*/
                   end   /*j*/
               end       /*m*/
            return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen@: @.=;                 @.1 = 'UK  London'
                           @.2 = 'US  New York'
                           @.3 = 'US  Birmingham'
                           @.4 = 'UK  Birmingham'
             do #=1  while @.#\==''; end;  #=#-1 /*determine how many entries in list.  */
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: do j=1  for #;  say '      element' right(j,length(#)) arg(1)":"  @.j;  end;  return
```

'''output'''   using the default list:

```txt

      element 1 before sort: UK  London
      element 2 before sort: US  New York
      element 3 before sort: US  Birmingham
      element 4 before sort: UK  Birmingham
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      element 1  after sort: UK  Birmingham
      element 2  after sort: UK  London
      element 3  after sort: US  Birmingham
      element 4  after sort: US  New York

```



## Ring


```ring

aList = [["UK", "London"],
        ["US", "New York"],
        ["US", "Birmingham"],
        ["UK", "Birmingham"]]
see sort(aList,2)

```



## Ruby

Ruby's built-in sort methods (Array#sort, Array#sort!, Array#sort_by!, Enumerable#sort and Enumerable#sort_by) are not stable. [[MRI]] uses [[Sorting algorithms/Quicksort|quicksort]], which is not stable [http://groups.google.com/group/comp.lang.ruby/msg/2a6c50d173a902da (1)]. It seems that stable sorting is not worth the performance trade-off; MRI rejected a proposal to switch to a stable sort [http://redmine.ruby-lang.org/issues/show/1089 (2)].

```ruby
ary = [["UK", "London"],
       ["US", "New York"],
       ["US", "Birmingham"],
       ["UK", "Birmingham"]]
p ary.sort {|a,b| a[1] <=> b[1]}
# MRI reverses the Birminghams:
# => [["UK", "Birmingham"], ["US", "Birmingham"], ["UK", "London"], ["US", "New York"]]
```


Other implementations of Ruby might differ. Old versions of [[JRuby]] used java.util.Arrays.sort, which was a stable sort, but was slower than MRI. To increase performance, JRuby switched to quicksort, which is not stable. [http://jira.codehaus.org/browse/JRUBY-2198 (3)]


### Stable sort in Ruby

To code a stable sort, without implementing another sorting algorithm (such as [[Sorting algorithms/Merge sort|merge sort]]), use a Schwartzian transform.


```ruby
class Array
  def stable_sort
    n = -1
    if block_given?
      collect {|x| n += 1; [x, n]
      }.sort! {|a, b|
        c = yield a[0], b[0]
        if c.nonzero? then c else a[1] <=> b[1] end
      }.collect! {|x| x[0]}
    else
      sort_by {|x| n += 1; [x, n]}
    end
  end

  def stable_sort_by
    block_given? or return enum_for(:stable_sort_by)
    n = -1
    sort_by {|x| n += 1; [(yield x), n]}
  end
end
```



```ruby
ary = [["UK", "London"],
       ["US", "New York"],
       ["US", "Birmingham"],
       ["UK", "Birmingham"]]
p ary.stable_sort {|a, b| a[1] <=> b[1]}
# => [["US", "Birmingham"], ["UK", "Birmingham"], ["UK", "London"], ["US", "New York"]]
p ary.stable_sort_by {|x| x[1]}
# => [["US", "Birmingham"], ["UK", "Birmingham"], ["UK", "London"], ["US", "New York"]]
```



## Rust

Rust's builtin sorts (.sort(), .sort_by(...), .sort_by_key(...)) are all stable


```rust
fn main() {
    let country_city = [("UK", "London"),
                        ("US", "New York"),
                        ("US", "Birmingham"),
                        ("UK", "Birmingham")];

    let mut city_sorted = country_city.clone();
    city_sorted.sort_by_key(|k| k.1);

    let mut country_sorted = country_city.clone();
    country_sorted.sort_by_key(|k| k.0);


    println!("Original:");
    for x in &country_city {
        println!("{} {}", x.0, x.1);
    }

    println!("\nWhen sorted by city:");
    for x in &city_sorted {
        println!("{} {}", x.0, x.1);
    }

    println!("\nWhen sorted by county:");
    for x in &country_sorted {
        println!("{} {}", x.0, x.1);
    }
}
```


Output:
```txt
Original:
UK London
US New York
US Birmingham
UK Birmingham

When sorted by city:
US Birmingham
UK Birmingham
UK London
US New York

When sorted by county:
UK London
UK Birmingham
US New York
US Birmingham
```



## Scala

There are two sort methods defined on <tt>Seq</tt>, which is the base collection trait for all sequences. The methods are <tt>sortWith</tt> and <tt>sortBy</tt>, and differ only on the argument used. The first expects a function that will implement the "less than" method for the type of the sequence. The second expects a function from the type of the sequence into any type for which there is an <tt>Ordering</tt>, plus an implicit Ordering of the proper type.

The sort is stable.

Examples:

```scala
scala>
 val list = List((1, 'c'), (1, 'b'), (2, 'a'))
list: List[(Int, Char)] = List((1,c), (1,b), (2,a))

scala> val srt1 = list.sortWith(_._2 < _._2)
srt1: List[(Int, Char)] = List((2,a), (1,b), (1,c))

scala> val srt2 = srt1.sortBy(_._1) // Ordering[Int] is implicitly defined
srt2: List[(Int, Char)] = List((1,b), (1,c), (2,a))

scala> val cities = """
     | |UK  London
     | |US  New York
     | |US  Birmingham
     | |UK  Birmingham
     | |""".stripMargin.lines.filterNot(_ isEmpty).toSeq
cities: Seq[String] = ArrayBuffer(UK  London, US  New York, US  Birmingham, UK  Birmingham)

scala> cities.sortBy(_ substring 4)
res47: Seq[String] = ArrayBuffer(US  Birmingham, UK  Birmingham, UK  London, US  New York)
```

Besides that, there is the object <tt>scala.util.Sorting</tt>, which provides <tt>quickSort<tt> and <tt>stableSort</tt>. The former is only provided on <tt>Array</tt>, but the latter is provided over both <tt>Array</tt> and <tt>Seq</tt>. These sorts operate in-place, with the one over <tt>Seq</tt> returning a sorted <tt>Array</tt>. Here is one example:

```scala
scala>
 val cityArray = cities.toArray
cityArray: Array[String] = Array(UK  London, US  New York, US  Birmingham, UK  Birmingham)

scala> scala.util.Sorting.stableSort(cityArray, (_: String).substring(4) < (_: String).substring(4))

scala> cityArray
res56: Array[String] = Array(US  Birmingham, UK  Birmingham, UK  London, US  New York)
```



## Sidef

Sidef uses the stable merge-sort algorithm for sorting an array.

```ruby
var table = [
  <UK  London>,
  <US  New\ York>,
  <US  Birmingham>,
  <UK  Birmingham>,
];

table.sort {|a,b| a[0] <=> b[0]}.each { |col|
    say "#{col[0]} #{col[1]}"
}
```

```txt
UK London
UK Birmingham
US New York
US Birmingham
```



## Stata

See '''[http://www.stata.com/help.cgi?sort sort]''' in Stata help. The command has a ''stable'' option to ensure sort stability.


## Tcl

Tcl's built-in [http://www.tcl.tk/man/tcl8.4/TclCmd/lsort.htm <code>lsort</code>] command implements a stable sort. It has been guaranteed to be stable since Tcl 8.0. Internally, it uses the mergesort algorithm.


## TXR


Straight from the TXR documentation about the <code>sort</code> function:

<i>The <code>sort</code> function is stable for sequences which are lists.  This means  that  the  original  order  of items which are considered identical is preserved.  For strings and vectors, <code>sort</code> is not stable.</i>


## zkl

zkl's sort methods don't mention stability or columns, they are comparison based.

```zkl
fcn sortByColumn(list,col)
   { list.sort('wrap(city1,city2){ city1[col]<city2[col] }) }
```


```zkl
cities:=List(
   T("UK",  "London"), T("US",  "New York"),
   T("US",  "Birmingham"),T("UK",  "Birmingham"), );
sortByColumn(cities,0).concat("\n").println("\n------");
sortByColumn(cities,1).concat("\n").println();
```

```txt

L("UK","London")
L("UK","Birmingham")
L("US","New York")
L("US","Birmingham")
------
L("UK","Birmingham")
L("US","Birmingham")
L("UK","London")
L("US","New York")

```

