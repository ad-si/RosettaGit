+++
title = "Set"
description = ""
date = 2019-04-20T15:57:47Z
aliases = []
[extra]
id = 10526
[taxonomies]
categories = ["task", "Discrete math"]
tags = []
languages = [
  "ada",
  "aime",
  "apex",
  "autohotkey",
  "bbc_basic",
  "c",
  "ceylon",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dart",
  "echolisp",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "frink",
  "funl",
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
  "lfe",
  "liberty_basic",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "maxima",
  "nemerle",
  "nim",
  "ocaml",
  "ol",
  "oorexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "setl",
  "sidef",
  "simula",
  "smalltalk",
  "sql",
  "swift",
  "tcl",
  "vba",
  "zkl",
]
+++

A   '''set'''  is a collection of elements, without duplicates and without order.


## Task

Show each of these set operations:

* Set creation
* Test m &isin; S -- "m is an element in set S"
* A &cup; B -- ''union''; a set of all elements either in set A or in set B.
* A &cap; B -- ''intersection''; a set of all elements in ''both'' set A and set B.
* A &#x2216; B -- ''difference''; a set of all elements in set A, except those in set B.
* A &sube; B -- ''subset''; true if every element in set A is also in set B.
* A = B -- ''equality''; true if every element of set A is in set B and vice versa.



As an option, show some other set operations.

(If A &sube; B, but A &ne; B, then A is called a true or proper subset of B, written A &sub; B or A &#x228a; B.)

As another option, show how to modify a mutable set.


One might implement a set using an [[associative array]] (with set elements as array keys and some dummy value as the values).

One might also implement a set with a binary search tree, or with a hash table, or with an ordered array of binary bits (operated on with bit-wise binary operators).

The basic test, m &isin; S, is [[O]](n) with a sequential list of elements, O(''log'' n) with a balanced binary search tree, or (O(1) average-case, O(n) worst case) with a hash table.


## Ada


This solution uses the generic Ordered_Sets package from the Ada.Containers standard library, which internally is based on red-black trees.
An alternative hash-based solution could use the Hashed_Maps package from Ada.Containers.


```Ada
with ada.containers.ordered_sets, ada.text_io;
use ada.text_io;

procedure set_demo is
	package cs is new ada.containers.ordered_sets (character); use cs;

	function "+" (s : string) return set is
	(if s = "" then empty_set else Union(+ s(s'first..s'last - 1), To_Set (s(s'last))));

	function "-" (s : Set) return string is
	(if s = empty_set then "" else - (s - To_Set (s.last_element)) & s.last_element);
	s1, s2 : set;
begin
	loop
		put ("s1= ");
		s1 := + get_line;
		exit when s1 = +"Quit!";
		put ("s2= ");
		s2 := + get_line;
		Put_Line("Sets [" & (-s1) & "], [" & (-s2) & "] of size"
                & S1.Length'img & " and" & s2.Length'img & ".");
  		Put_Line("Intersection:   [" & (-(Intersection(S1, S2))) & "],");
  		Put_Line("Union:          [" & (-(Union(s1, s2)))        & "],");
  		Put_Line("Difference:     [" & (-(Difference(s1, s2)))   & "],");
  		Put_Line("Symmetric Diff: [" & (-(s1 xor s2)) & "],");
  		Put_Line("Subset: "  & Boolean'Image(s1.Is_Subset(s2))
              & ", Equal: " & Boolean'Image(s1 = s2) & ".");
	end loop;
end set_demo;

```


```txt
set
demo
Sets [est], [demo] of size 3 and 4.
Intersection:   [e],
Union:          [demost],
Difference:     [st],
Symmetric Diff: [dmost],
Subset: FALSE, Equal: FALSE.
quit!

```



## Aime


```aime
record
union(record a, record b)
{
    record c;
    r_copy(c, a);
    r_wcall(b, r_add, 1, 2, c);
    return c;
}

record
intersection(record a, record b)
{
    record c;
    text s;
    for (s in a) {
        if (r_key(b, s)) {
            c[s] = 0;
        }
    }
    return c;
}

record
difference(record a, record b)
{
    record c;
    r_copy(c, a);
    r_vcall(b, r_resign, 1, c);
    return c;
}

integer
subset(record a, record b)
{
    integer e;
    text s;
    e = 1;
    for (s in a) {
        if (!r_key(b, s)) {
            e = 0;
            break;
        }
    }
    return e;
}

integer
equal(record a, record b)
{
    return subset(a, b) && subset(b, a);
}

integer
main(void)
{
    record a, b;
    text s;

    r_fit(a, "apple", 0, "cherry", 0, "grape", 0);
    r_fit(b, "banana", 0, "cherry", 0, "date", 0);

    s = "banana";

    o_(" ", s, " is ", r_key(a, s) ? "" : "not ", "an element of A\n");
    o_(" ", s, " is ", r_key(b, s) ? "" : "not ", "an element of B\n");

    r_vcall(union(a, b), o_, 1, " ");
    o_newline();

    r_vcall(intersection(a, b), o_, 1, " ");
    o_newline();

    r_vcall(difference(a, b), o_, 1, " ");
    o_newline();

    o_(" ", subset(a, b) ? "yes" : "no", "\n");

    o_(" ", equal(a, b) ? "yes" : "no", "\n");

    return 0;
}
```

```txt
 banana is not an element of A
 banana is an element of B
 apple banana cherry date grape
 cherry
 apple grape
 no
 no
```



## Apex

In Apex, Sets are unordered collections of elements.  Although elements can be anything including primitives, Ids, Apex classes, or sObjects, typically they are used with primitives and Ids.


```apex

public class MySetController{
    public Set<String> strSet {get; private set; }
    public Set<Id> idSet {get; private set; }

    public MySetController(){
        //Initialize to an already known collection.  Results in a set of abc,def.
        this.strSet = new Set<String>{'abc','abc','def'};

        //Initialize to empty set and add in entries.
        this.strSet = new Set<String>();
        this.strSet.add('abc');
        this.strSet.add('def');
        this.strSet.add('abc');
        //Results in {'abc','def'}

        //You can also get a set from a map in Apex. In this case, the account ids are fetched from a SOQL query.
        Map<Id,Account> accountMap = new Map<Id,Account>([Select Id,Name From Account Limit 10]);
        Set<Id> accountIds = accountMap.keySet();

        //If you have a set, you can also use it with the bind variable syntax in SOQL:
        List<Account> accounts = [Select Name From Account Where Id in :accountIds];

        //Like other collections in Apex, you can use a for loop to iterate over sets:
        for(Id accountId : accountIds){
            Account a = accountMap.get(accountId);
            //Do account stuffs here.
        }
    }
}

```


```



## AutoHotkey


```AutoHotkey
test(Set,element){
	for i, val in Set
		if (val=element)
			return true
	return false
}

Union(SetA,SetB){
	SetC:=[], Temp:=[]
	for i, val in SetA
		SetC.Insert(val), Temp[val] := true
	for i, val in SetB
		if !Temp[val]
			SetC.Insert(val)
	return SetC
}

intersection(SetA,SetB){
	SetC:=[], Temp:=[]
	for i, val in SetA
		Temp[val] := true
	for i, val in SetB
		if Temp[val]
			SetC.Insert(val)
	return SetC
}

difference(SetA,SetB){
	SetC:=[], Temp:=[]
	for i, val in SetB
		Temp[val] := true
	for i, val in SetA
		if !Temp[val]
			SetC.Insert(val)
	return SetC
}

subset(SetA,SetB){
	Temp:=[], A:=B:=0
	for i, val in SetA
		Temp[val] := true , A++
	for i, val in SetB
		if Temp[val]{
			B++
			IfEqual, A, %B%, return 1
		} return 0
}

equal(SetA,SetB){
	return (SetA.MaxIndex() = SetB.MaxIndex() && subset(SetA,SetB)) ? 1: 0
}
```

Examples:
```AutoHotkey
A:= ["apple", "cherry", "elderberry", "grape"]
B:= ["banana", "cherry", "date", "elderberry", "fig"]
C:= ["apple", "cherry", "elderberry", "grape", "orange"]
D:= ["apple", "cherry", "elderberry", "grape"]
E:= ["apple", "cherry", "elderberry"]
M:= "banana"

Res =
(
A:= ["apple", "cherry", "elderberry", "grape"]
B:= ["banana", "cherry", "date", "elderberry", "fig"]
C:= ["apple", "cherry", "elderberry", "grape", "orange"]
D:= ["apple", "cherry", "elderberry", "grape"]
E:= ["apple", "cherry", "elderberry"]
M:= "banana"

)

Res .= "`nM is " (test(A,M)?"":"not ") "an element of Set A"
Res .= "`nM is " (test(B,M)?"":"not ") "an element of Set B"

Res .= "`nUnion(A,B) = "
for i, val in Union(A,B)
	Res.= (A_Index=1?"`t":", ") val

Res .= "`nintersection(A,B) = "
for i, val in intersection(A,B)
	Res.= (A_Index=1?"`t":", ") val

Res .= "`ndifference(A,B) = "
for i, val in difference(A,B)
	Res.= (A_Index=1?"`t":", ") val

Res .= "`n`nA is " (subset(A,C)?"":"not ") "a subset of Set C"
Res .= "`nA is " (subset(A,D)?"":"not ") "a subset of Set D"
Res .= "`nA is " (subset(A,E)?"":"not ") "a subset of Set E"

Res .= "`n`nA is " (equal(A,C)?"":"not ") "a equal to Set C"
Res .= "`nA is " (equal(A,D)?"":"not ") "a equal to Set D"
Res .= "`nA is " (equal(A,E)?"":"not ") "a equal to Set E"

MsgBox % Res
```

```txt
A:= ["apple", "cherry", "elderberry", "grape"]
B:= ["banana", "cherry", "date", "elderberry", "fig"]
C:= ["apple", "cherry", "elderberry", "grape", "orange"]
D:= ["apple", "cherry", "elderberry", "grape"]
E:= ["apple", "cherry", "elderberry"]
M:= "banana"

M is not an element of Set A
M is an element of Set B
Union(A,B) = 	apple, cherry, elderberry, grape, banana, date, fig
intersection(A,B) = 	cherry, elderberry
difference(A,B) = 	apple, grape

A is a subset of Set C
A is a subset of Set D
A is not a subset of Set E

A is not a equal to Set C
A is a equal to Set D
A is not a equal to Set E
```



## BBC BASIC

The sets are represented as 32-bit integers, which means that the maximum number of elements is 32.

```bbcbasic
      DIM list$(6)
      list$() = "apple", "banana", "cherry", "date", "elderberry", "fig", "grape"

      setA% = %1010101
      PRINT "Set A: " FNlistset(list$(), setA%)
      setB% = %0111110
      PRINT "Set B: " FNlistset(list$(), setB%)
      elementM% = %0000010
      PRINT "Element M: " FNlistset(list$(), elementM%) '

      IF elementM% AND setA% THEN
        PRINT "M is an element of set A"
      ELSE
        PRINT "M is not an element of set A"
      ENDIF
      IF elementM% AND setB% THEN
        PRINT "M is an element of set B"
      ELSE
        PRINT "M is not an element of set B"
      ENDIF

      PRINT '"The union of A and B is " FNlistset(list$(), setA% OR setB%)
      PRINT "The intersection of A and B is " FNlistset(list$(), setA% AND setB%)
      PRINT "The difference of A and B is " FNlistset(list$(), setA% AND NOT setB%)

      IF (setA% AND setB%) = setA% THEN
        PRINT '"Set A is a subset of set B"
      ELSE
        PRINT '"Set A is not a subset of set B"
      ENDIF
      IF setA% = setB% THEN
        PRINT "Set A is equal to set B"
      ELSE
        PRINT "Set A is not equal to set B"
      ENDIF
      END

      DEF FNlistset(list$(), set%)
      LOCAL i%, o$
      FOR i% = 0 TO 31
        IF set% AND 1 << i% o$ += list$(i%) + ", "
      NEXT
      = LEFT$(LEFT$(o$))
```

```txt

Set A: apple, cherry, elderberry, grape
Set B: banana, cherry, date, elderberry, fig
Element M: banana

M is not an element of set A
M is an element of set B

The union of A and B is apple, banana, cherry, date, elderberry, fig, grape
The intersection of A and B is cherry, elderberry
The difference of A and B is apple, grape

Set A is not a subset of set B
Set A is not equal to set B

```



## C

Building a set highly depends on the datatype and use case.  For example, a set of string could be implemented by hash table, sort tree or trie, but if all sets are known to have very few number of elements, it might be best to use just flat arrays.  There isn't, and shouldn't be, an all-purpose set type for C.

A frequent use of set is that of small, non-negative integers, implemented as a bit field as shown below.

```c
#include <stdio.h>

typedef unsigned int set_t; /* probably 32 bits; change according to need */

void show_set(set_t x, const char *name)
{
	int i;
	printf("%s is:", name);
	for (i = 0; (1U << i) <= x; i++)
		if (x & (1U << i))
			printf(" %d", i);
	putchar('\n');
}

int main(void)
{
	int i;
	set_t a, b, c;

	a = 0; /* empty set */
	for (i = 0; i < 10; i += 3) /* add 0 3 6 9 to set a */
		a |= (1U << i);
	show_set(a, "a");

	for (i = 0; i < 5; i++)
		printf("\t%d%s in set a\n", i, (a & (1U << i)) ? "":" not");

	b = a;
	b |= (1U << 5); b |= (1U << 10); /* b is a plus 5, 10 */
	b &= ~(1U << 0);	/* sans 0 */
	show_set(b, "b");

	show_set(a | b, "union(a, b)");
	show_set(c = a & b, "c = common(a, b)");
	show_set(a & ~b, "a - b"); /* diff, not arithmetic minus */
	show_set(b & ~a, "b - a");
	printf("b is%s a subset of a\n", !(b & ~a) ? "" : " not");
	printf("c is%s a subset of a\n", !(c & ~a) ? "" : " not");

	printf("union(a, b) - common(a, b) %s union(a - b, b - a)\n",
		((a | b) & ~(a & b)) == ((a & ~b) | (b & ~a))
			? "equals" : "does not equal");

	return 0;
}
```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

class Program
{
    static void PrintCollection(IEnumerable<int> x)
    {
        Console.WriteLine(string.Join(" ", x));
    }
    static void Main(string[] args)
    {
        Console.OutputEncoding = Encoding.UTF8;
        Console.WriteLine("Set creation");
        var A = new HashSet<int> { 4, 12, 14, 17, 18, 19, 20 };
        var B = new HashSet<int> { 2, 5, 8, 11, 12, 13, 17, 18, 20 };

        PrintCollection(A);
        PrintCollection(B);

        Console.WriteLine("Test m ∈ S -- \"m is an element in set S\"");
        Console.WriteLine("14 is an element in set A: {0}", A.Contains(14));
        Console.WriteLine("15 is an element in set A: {0}", A.Contains(15));

        Console.WriteLine("A ∪ B -- union; a set of all elements either in set A or in set B.");
        var aUb = A.Union(B);
        PrintCollection(aUb);

        Console.WriteLine("A ∖ B -- difference; a set of all elements in set A, except those in set B.");
        var aDb = A.Except(B);
        PrintCollection(aDb);

        Console.WriteLine("A ⊆ B -- subset; true if every element in set A is also in set B.");
        Console.WriteLine(A.IsSubsetOf(B));
        var C = new HashSet<int> { 14, 17, 18 };
        Console.WriteLine(C.IsSubsetOf(A));

        Console.WriteLine("A = B -- equality; true if every element of set A is in set B and vice versa.");
        Console.WriteLine(A.SetEquals(B));
        var D = new HashSet<int> { 4, 12, 14, 17, 18, 19, 20 };
        Console.WriteLine(A.SetEquals(D));

        Console.WriteLine("If A ⊆ B, but A ≠ B, then A is called a true or proper subset of B, written A ⊂ B or A ⊊ B");
        Console.WriteLine(A.IsProperSubsetOf(B));
        Console.WriteLine(C.IsProperSubsetOf(A));

        Console.WriteLine("Modify a mutable set.  (Add 10 to A; remove 12 from B).");
        A.Add(10);
        B.Remove(12);
        PrintCollection(A);
        PrintCollection(B);

        Console.ReadKey();
    }
}
```

```txt
Set creation
4 12 14 17 18 19 20
2 5 8 11 12 13 17 18 20
Test m ∈ S -- "m is an element in set S"
14 is an element in set A: True
15 is an element in set A: False
A ∪ B -- union; a set of all elements either in set A or in set B.
4 12 14 17 18 19 20 2 5 8 11 13
A ∖ B -- difference; a set of all elements in set A, except those in set B.
4 14 19
A ⊆ B -- subset; true if every element in set A is also in set B.
False
True
A = B -- equality; true if every element of set A is in set B and vice versa.
False
True
If A ⊆ B, but A ≠ B, then A is called a true or proper subset of B, written A ⊂ B or A ⊊ B
False
True
Modify a mutable set.  (Add 10 to A; remove 12 from B).
4 12 14 17 18 19 20 10
2 5 8 11 13 17 18 20
```



## C++

C++ standard library contains a set class, which is a sorted container without duplicates and implemented as a binary tree. Additional set functionality can be implemented in terms of standard library algorithms.

C++11 standard library also contains unordered_set based on a hash table. However, algorithms like std::set_intersection etc take sorted ranges, so set-specific functions should be hand-rolled.


```cpp

#include <set>
#include <iostream>
#include <iterator>
#include <algorithm>

namespace set_display {
template <class T>
std::ostream& operator<<(std::ostream& os, const std::set<T>& set)
{
    os << '[';
    if (!set.empty()) {
        std::copy(set.begin(), --set.end(), std::ostream_iterator<T>(os, ", "));
        os << *--set.end();
    }
    return os << ']';
}
}

template <class T>
bool contains(const std::set<T>& set, const T& key)
{
    return set.count(key) != 0;
}

template <class T>
std::set<T> set_union(const std::set<T>& a, const std::set<T>& b)
{
    std::set<T> result;
    std::set_union(a.begin(), a.end(), b.begin(), b.end(), std::inserter(result, result.end()));
    return result;
}

template <class T>
std::set<T> set_intersection(const std::set<T>& a, const std::set<T>& b)
{
    std::set<T> result;
    std::set_intersection(a.begin(), a.end(), b.begin(), b.end(), std::inserter(result, result.end()));
    return result;
}

template <class T>
std::set<T> set_difference(const std::set<T>& a, const std::set<T>& b)
{
    std::set<T> result;
    std::set_difference(a.begin(), a.end(), b.begin(), b.end(), std::inserter(result, result.end()));
    return result;
}

template <class T>
bool is_subset(const std::set<T>& set, const std::set<T>& subset)
{
    return std::includes(set.begin(), set.end(), subset.begin(), subset.end());
}

int main()
{
    using namespace set_display;
    std::set<int> a{2, 5, 7, 5, 9, 2}; //C++11 initialization syntax
    std::set<int> b{1, 5, 9, 7, 4 };
    std::cout << "a = " << a << '\n';
    std::cout << "b = " << b << '\n';

    int value1 = 8, value2 = 5;
    std::cout << "Set a " << (contains(a, value1) ? "contains " : "does not contain ") << value1 << '\n';
    std::cout << "Set a " << (contains(a, value2) ? "contains " : "does not contain ") << value2 << '\n';

    std::cout << "Union of a and b: " << set_union(a, b) << '\n';
    std::cout << "Intersection of a and b: " << set_intersection(a, b) << '\n';
    std::cout << "Difference of a and b: " << set_difference(a, b) << '\n';

    std::set<int> sub{5, 9};
    std::cout << "Set b " << (is_subset(a, b) ? "is" : "is not") << " a subset of a\n";
    std::cout << "Set " << sub << ' ' << (is_subset(a, sub) ? "is" : "is not") << " a subset of a\n";

    std::set<int> copy = a;
    std::cout << "a " << (a == copy ? "equals " : "does not equal ") << copy << '\n';

    return 0;
}

```



## Ceylon


```ceylon
shared void run() {
    value a = set {1, 2, 3};
    value b = set {3, 4, 5};
    value union = a | b;
    value intersection = a & b;
    value difference = a ~ b;
    value subset = a.subset(b);
    value equality = a == b;

    print("set a:         ``a``
           set b:         ``b``
           1 in a?        ``1 in a``
           a | b:         ``union``
           a & b:         ``intersection``
           a ~ b:         ``difference``
           a subset of b? ``subset``
           a == b?        ``equality``");
}
```



## Clojure



```clojure
(require 'clojure.set)

; sets can be created using the set method or set literal syntax
(def a (set [1 2 3 4]))
(def b #{4 5 6 7})

(a 10) ; returns the element if it's contained in the set, otherwise nil

(clojure.set/union a b)

(clojure.set/intersection a b)

(clojure.set/difference a b)

(clojure.set/subset? a b)
```



## CoffeeScript

This implements functions from the task, along with an iteration helper called "each".

```coffeescript

# For ad-hoc set features, it sometimes makes sense to use hashes directly,
# rather than abstract to this level, but I'm showing a somewhat heavy
# solution to show off CoffeeScript class syntax.
class Set
  constructor: (elems...) ->
    @hash = {}
    for elem in elems
      @hash[elem] = true

  add: (elem) ->
    @hash[elem] = true

  remove: (elem) ->
    delete @hash[elem]

  has: (elem) ->
    @hash[elem]?

  union: (set2) ->
    set = new Set()
    for elem of @hash
      set.add elem
    for elem in set2.to_array()
      set.add elem
    set

  intersection: (set2) ->
    set = new Set()
    for elem of @hash
      set.add elem if set2.has elem
    set

  minus: (set2) ->
    set = new Set()
    for elem of @hash
      set.add elem if !set2.has elem
    set

  is_subset_of: (set2) ->
    for elem of @hash
      return false if !set2.has elem
    true

  equals: (set2) ->
    this.is_subset_of(set2) and set2.is_subset_of this

  to_array: ->
    (elem for elem of @hash)

  each: (f) ->
    for elem of @hash
      f(elem)

  to_string: ->
    @to_array()

run_tests = ->
  set1 = new Set("apple", "banana") # creation
  console.log set1.has "apple" # true (membership)
  console.log set1.has "worms" # false (membership)

  set2 = new Set("banana", "carrots")
  console.log set1.union(set2).to_string() # [ 'apple', 'banana', 'carrots' ] (union)
  console.log set1.intersection(set2).to_string() # [ 'banana' ] (intersection)
  console.log set1.minus(set2).to_string() # [ 'apple' ] (difference)

  set3 = new Set("apple")
  console.log set3.is_subset_of set1 # true
  console.log set3.is_subset_of set2 # false

  set4 = new Set("apple", "banana")
  console.log set4.equals set1 # true
  console.log set4.equals set2 # false

  set5 = new Set("foo")
  set5.add "bar" # add
  console.log set5.to_string() # [ 'foo', 'bar' ]
  set5.remove "bar" # remove
  console.log set5.to_string() # [ 'foo' ]

  # iteration, prints apple then banana (order not guaranteed)
  set1.each (elem) ->
    console.log elem

run_tests()

```



## Common Lisp

Common Lisp provides some set operations on lists.

```lisp
(setf a '(1 2 3 4))
(setf b '(2 3 4 5))

(format t "sets: ~a ~a~%" a b)

;;; element
(loop for x from 1 to 6 do
	(format t (if (member x a)
		    "~d ∈ A~%"
		    "~d ∉ A~%") x))

(format t "A ∪ B: ~a~%" (union a b))
(format t "A ∩ B: ~a~%" (intersection a b))
(format t "A \\ B: ~a~%" (set-difference a b))
(format t (if (subsetp a b)
	    "~a ⊆ ~a~%"
	    "~a ⊈ ~a~%") a b)

(format t (if (and (subsetp a b)
		   (subsetp b a))
	    "~a = ~a~%"
	    "~a ≠ ~a~%") a b)
```



## D


```d
void main() {
    import std.stdio, std.algorithm, std.range;

    // Not true sets, items can be repeated, but must be sorted.
    auto s1 = [1, 2, 3, 4, 5, 6].assumeSorted;
    auto s2 = [2, 5, 6, 3, 4, 8].sort(); // [2,3,4,5,6,8].
    auto s3 = [1, 2, 5].assumeSorted;

    assert(s1.canFind(4)); // Linear search.
    assert(s1.contains(4)); // Binary search.
    assert(s1.setUnion(s2).equal([1,2,2,3,3,4,4,5,5,6,6,8]));
    assert(s1.setIntersection(s2).equal([2, 3, 4, 5, 6]));
    assert(s1.setDifference(s2).equal([1]));
    assert(s1.setSymmetricDifference(s2).equal([1, 8]));
    assert(s3.setDifference(s1).empty); // It's a subset.
    assert(!s1.equal(s2));

    auto s4 = [[1, 4, 7, 8], [1, 7], [1, 7, 8], [4], [7]];
    const s5 = [1, 1, 1, 4, 4, 7, 7, 7, 7, 8, 8];
    assert(s4.nWayUnion.equal(s5));
}
```




## D


```d

module set;
import std.typecons : Tuple, tuple;
struct Set(V) { // Limited set of V-type elements                                        // here 'this' is named A, s is B, v V-type item

protected V[] array;

	this(const Set s) {                                                              // construct A by copy of B
		array = s.array.dup;
	}

	this(V[] arg...){                                                                // construct A with items
		foreach(v; arg) if (v.isNotIn(array)) array ~= v;
	}

	enum : Set { empty = Set() }                                                     // ∅

	ref Set opAssign()(const Set s) {                                                // A = B
		array = s.array.dup;
		return this;
	}

	bool opBinaryRight(string op : "in")(const V v) const {                          // v ∈ A
		return v.isIn(array);
	}

	ref Set opOpAssign(string op)(const V v) if (op == "+" || op == "|") {           // A += {v}          // + = ∪ = |
		if (v.isIn(array)) return this;
		array ~= v;
		return this;
	}

	ref Set opOpAssign(string op)(const Set s) if (op == "+" || op == "|") {         // A += B
		foreach(x; s.array) if (x.isNotIn(array)) array ~= x;
		return this;
	}

	Set opBinary(string op)(const V v) const if (op == "+" || op == "|"){            // A + {v}
		Set result = this;
		result += v;
		return result;
	}

	Set opBinaryRight(string op)(const V v) const if (op == "+" || op == "|") {      // {v} + A
		Set result = this;
		result += v;
		return result;
	}

	Set opBinary(string op)(const Set s) const if (op == "+" || op == "|") {         // A + B
		Set result = this;
		result += s;
		return result;
	}

	Set opBinary(string op : "&")(const Set s) const{                                // A ∩ B               // ∩ = &
		Set result;
		foreach(x; array) if(x.isIn(s.array)) result += x;
		return result;
	}

	ref Set opOpAssign(string op : "&")(const Set s) {                               // A ∩= B
		return this(this & s);
	}

	Set opBinary(string op : "^")(const Set s) const {                               // (A ∪ B) - (A ∩ B)    //  = A ^ B
		Set result;
		foreach(x; array) if (x.isNotIn(s.array)) result += x;
		foreach(x; s.array) if(x.isNotIn(array)) result += x;
		return result;
	}

	ref opOpAssign(string op : "^")(const Set s) {
		return this = this ^ s;
	}

	Set opBinary(string op : "-")(const Set s) const {                                // A - B
		Set r;
		foreach(x; array) if(x.isNot(s.array)) r += x;
		return r;
	}

	ref Set opOpAssign(string op : "-")(const Set s) {                                // A -= B
		return this = this - s;
	}

	Set!(Tuple!(V,U)) opBinary(U, string op : "*")(const Set!U s) const {             // A × B = { (x, y) | ∀x ∈ A ∧ ∀y ∈ B }
		Set!(Tuple!(V, U)) r;
		foreach(x; array) foreach(y; s.array) r += tuple(x, y);
		return r;
	}

	bool isEmpty() const { return !array.length;}                                     // A ≟ ∅

	bool opBinary(string op : "in")(const Set s) const {                              // A ⊂ s
		foreach(v; array) if(v.isNotIn(s.array)) return false;
		return true;
	}

	bool opEquals(const Set s) const {                                                // A ≟ B
		if (array.length != s.array.length) return false;
		return this in s;
	}

	T[] array() const @property { return array.dup;}

}

Set!(Tuple!(T, T)) sqr(T)(const Set!T s) { return s * s; }                                 // A²

auto pow(T, uint n : 0)(const Set!T s) {                                                   // A ^ 0
	return Set!T.empty;
}

auto pow(T, uint n : 1)(const Set!T s) {                                                   // A ^ 1 = A
	return s;
}

auto pow(T, uint n : 2)(const Set!T s) {                                                   // A ^ 2 (=A²)
	return sqr!T(s);
}

auto pow(T, uint n)(const Set!T s) if(n % 2) {                                             // if n Odd,  A^n = A * (A^(n/2))²
        return s * sqr!T(pow!(T, n/2)(s));
}

auto pow(T, uint n)(const Set!T s) if(!(n % 2)) {                                           // if n Even, A^n = (A^(n/2))²
	return sqr!T(pow!(T, n/2)(s));
}

size_t Card(T)(const Set!T s) {return s.length; }                                           // Card(A)

Set!(Set!T) power(T)(Set!T s) {                                                             // ∀B ∈ P(A) ⇒ B ⊂ A
	Set!(Set!T) ret;
	foreach(e; s.array) {
		Set!(Set!T) rs;
		foreach(x; ret.array) {
			x += e;
			rs += x;
		}
		ret += rs;
	}
	return ret;
}

bool isIn(T)(T x, T[] array){
	foreach(a; array) if(a == x) return true;
	return false;
}
bool isNotIn(T)(T x, T[] array){
	foreachj(a; array) if(a == x) return false;
	return true;
}

```



## Dart


```d
void main(){
  //Set Creation
  Set A = new Set.from([1,2,3]);
  Set B = new Set.from([1,2,3,4,5]);
  Set C = new Set.from([1,2,4,5]);

  print('Set A = $A');
  print('Set B = $B');
  print('Set C = $C');
  print('');
  //Test if element is in set
  int m = 3;
  print('m = 5');
  print('m in A = ${A.contains(m)}');
  print('m in B = ${B.contains(m)}');
  print('m in C = ${C.contains(m)}');
  print('');
  //Union of two sets
  Set AC = A.union(C);
  print('Set AC = Union of A and C = $AC');
  print('');
  //Intersection of two sets
  Set A_C = A.intersection(C);
  print('Set A_C = Intersection of A and C = $A_C');
  print('');
  //Difference of two sets
  Set A_diff_C = A.difference(C);
  print('Set A_diff_C = Difference between A and C = $A_diff_C');
  print('');
  //Test if set is subset of another set
  print('A is a subset of B = ${B.containsAll(A)}');
  print('C is a subset of B = ${B.containsAll(C)}');
  print('A is a subset of C = ${C.containsAll(A)}');
  print('');
  //Test if two sets are equal
  print('A is equal to B  = ${B.containsAll(A) && A.containsAll(B)}');
  print('B is equal to AC = ${B.containsAll(AC) && AC.containsAll(B)}');
}
```

```txt
Set A = {1, 2, 3}
Set B = {1, 2, 3, 4, 5}
Set C = {1, 2, 4, 5}

m = 5
m in A = true
m in B = true
m in C = false

Set AC = Union of A and C = {1, 2, 3, 4, 5}

Set A_C = Intersection of A and C = {1, 2}

Set A_diff_C = Difference between A and C = {3}

A is a subset of B = true
C is a subset of B = true
A is a subset of C = false

A is equal to B  = false
B is equal to AC = true
```



## EchoLisp

EchoLisp sets are lists, i.e the set of all sets is a proper subset of the set of all lists. Sets elements may be any object, including sets.

The set operations are: ∩ ∪ ⊆ / ∈ = ∆ ×

```lisp

; use { } to read a set
(define A { 1 2 3 4 3 5 5}) → { 1 2 3 4 5 } ; duplicates are removed from a set
; or use make-set to make a set from a list
(define B (make-set ' ( 3 4 5 6 7 8 8))) → { 3 4 5 6 7 8 }
(set-intersect A B) → { 3 4 5 }
(set-intersect? A B) → #t ; predicate
(set-union A B) → { 1 2 3 4 5 6 7 8 }
(set-substract A B) → { 1 2 }
(set-sym-diff A B) → { 1 2 6 7 8 } ; ∆ symmetric difference
(set-equal? A B) →  #f
(set-equal? { a b c} { c b a}) → #t ; order is unimportant
(set-subset? A B) → #f ; B in A or B = A
(set-subset? A { 3 4 }) → #t
(member 4 A) → (4 5) ; same as #t : true
(member 9 A) → #f

; check basic equalities
(set-equal? A (set-union (set-intersect A B) (set-substract A B))) → #t
(set-equal? (set-union A B) (set-union (set-sym-diff A B) (set-intersect A B))) → #t

; × : cartesian product of two sets : all pairs (a . b) , a in A, b in B
; returns a list (not a set)
(define A { albert simon})
(define B { antoinette ornella marylin})

(set-product A B)
→ ((albert . antoinette) (albert . marylin) (albert . ornella) (simon . antoinette) (simon . marylin) (simon . ornella))

; sets elements may be sets
{ { a b c} {c b a } { a b d}} → { { a b c } { a b d } } ; duplicate removed

; A few functions return sets :
(primes 10) → { 2 3 5 7 11 13 17 19 23 29 }

```



## Elixir

```elixir
iex(1)> s = MapSet.new
#MapSet<[]>
iex(2)> sa = MapSet.put(s, :a)
#MapSet<[:a]>
iex(3)> sab = MapSet.put(sa, :b)
#MapSet<[:a, :b]>
iex(4)> sbc = Enum.into([:b, :c], MapSet.new)
#MapSet<[:b, :c]>
iex(5)> MapSet.member?(sab, :a)
true
iex(6)> MapSet.member?(sab, :c)
false
iex(7)> :a in sab
true
iex(8)> MapSet.union(sab, sbc)
#MapSet<[:a, :b, :c]>
iex(9)> MapSet.intersection(sab, sbc)
#MapSet<[:b]>
iex(10)> MapSet.difference(sab, sbc)
#MapSet<[:a]>
iex(11)> MapSet.disjoint?(sab, sbc)
false
iex(12)> MapSet.subset?(sa, sab)
true
iex(13)> MapSet.subset?(sab, sa)
false
iex(14)> sa == sab
false
```



## Erlang

Built in.

```txt

2> S = sets:new().
3> Sa = sets:add_element(a, S).
4> Sab = sets:from_list([a, b]).
5> sets:is_element(a, Sa).
true
6> Union = sets:union(Sa, Sab).
7> sets:to_list(Union).
[a,b]
8> Intersection = sets:intersection(Sa, Sab).
9> sets:to_list(Intersection).
[a]
10> Subtract = sets:subtract(Sab, Sa).
11> sets:to_list(Subtract).
[b]
12> sets:is_subset(Sa, Sab).
true
13> Sa =:= Sab.
false

```


=={{header|F_Sharp|F#}}==
The Collections.Set<'T> class implements "Immutable sets based on binary trees, where comparison is the F# structural comparison function, potentially using implementations of the IComparable interface on key values." (http://msdn.microsoft.com/en-us/library/ee353619.aspx)

```fsharp
[<EntryPoint>
]
let main args =
    // Create some sets (of int):
    let s1 = Set.ofList [1;2;3;4;3]
    let s2 = Set.ofArray [|3;4;5;6|]

    printfn "Some sets (of int):"
    printfn "s1 = %A" s1
    printfn "s2 = %A" s2
    printfn "Set operations:"
    printfn "2 ∈ s1? %A" (s1.Contains 2)
    printfn "10 ∈ s1? %A" (s1.Contains 10)
    printfn "s1 ∪ s2 = %A" (Set.union s1 s2)
    printfn "s1 ∩ s2 = %A" (Set.intersect s1 s2)
    printfn "s1 ∖ s2 = %A" (Set.difference s1 s2)
    printfn "s1 ⊆ s2? %A" (Set.isSubset s1 s1)
    printfn "{3, 1} ⊆ s1? %A" (Set.isSubset (Set.ofList [3;1]) s1)
    printfn "{3, 2, 4, 1} = s1? %A" ((Set.ofList [3;2;4;1]) = s1)
    printfn "s1 = s2? %A" (s1 = s2)
    printfn "More set operations:"
    printfn "#s1 = %A" s1.Count
    printfn "s1 ∪ {99} = %A" (s1.Add 99)
    printfn "s1 ∖ {3} = %A" (s1.Remove 3)
    printfn "s1 ⊂ s1? %A" (Set.isProperSubset s1 s1)
    printfn "s1 ⊂ s2? %A" (Set.isProperSubset s1 s2)
    0
```

```txt
Some sets (of int):
s1 = set [1; 2; 3; 4]
s2 = set [3; 4; 5; 6]
Set operations:
2 ∈ s1? true
10 ∈ s1? false
s1 ∪ s2 = set [1; 2; 3; 4; 5; 6]
s1 ∩ s2 = set [3; 4]
s1 ∖ s2 = set [1; 2]
s1 ⊆ s2? true
{3, 1} ⊆ s1? true
{3, 2, 4, 1} = s1? true
s1 = s2? false
More set operations:
#s1 = 4
s1 ∪ {99} = set [1; 2; 3; 4; 99]
s1 ∖ {3} = set [1; 2; 4]
s1 ⊂ s1? false
s1 ⊂ s2? false
```



## Factor

We will use Factor's hash-sets for this task. A hash-set is created with <code>HS{ ... }</code>.

```factor
( scratchpad ) USE: sets
( scratchpad ) HS{ 2 5 4 3 } HS{ 5 6 7 } union .
HS{ 2 3 4 5 6 7 }
( scratchpad ) HS{ 2 5 4 3 } HS{ 5 6 7 } intersect .
HS{ 5 }
( scratchpad ) HS{ 2 5 4 3 } HS{ 5 6 7 } diff .
HS{ 2 3 4 }
( scratchpad ) HS{ 2 5 4 3 } HS{ 5 6 7 } subset? .
f
( scratchpad ) HS{ 5 6 } HS{ 5 6 7 } subset? .
t
( scratchpad ) HS{ 5 6 } HS{ 5 6 7 } set= .
f
( scratchpad ) HS{ 6 5 7 } HS{ 5 6 7 } set= .
t
```



## Forth

Works with any ANS Forth.
Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f
include FMS-SILib.f

: union {: a b -- c :}
  begin
    b each:
  while dup
    a indexOf: if 2drop else a add: then
  repeat b <free a dup sort: ; ok

i{ 2 5 4 3 } i{ 5 6 7 } union p: i{ 2 3 4 5 6 7 } ok


: free2 ( a b -- ) <free <free ;
: intersect {: a b | c -- c :}
  heap> 1-array2 to c
  begin
    b each:
  while dup
    a indexOf: if drop c add: else drop then
  repeat a b free2 c dup sort: ;

i{ 2 5 4 3 } i{ 5 6 7 } intersect p: i{ 5 } ok


: diff {: a b | c -- c :}
  heap> 1-array2 to c
  begin
    a each:
  while dup
    b indexOf: if 2drop else c add: then
  repeat a b free2 c dup sort: ;

i{ 2 5 4 3 } i{ 5 6 7 } diff p: i{ 2 3 4 } ok

: subset {: a b -- flag :}
  begin
    a each:
  while
    b indexOf: if drop else false exit then
  repeat a b free2 true ;

i{ 2 5 4 3 } i{ 5 6 7 } subset . 0 ok
i{ 5 6 } i{ 5 6 7 } subset .  -1 ok


: set= {: a b -- flag :}
  a size: b size: <> if a b free2 false exit then
  a sort: b sort:
  begin
    a each: drop b each:
  while
    <> if a b free2 false exit then
  repeat a b free2 true ;

i{ 5 6 } i{ 5 6 7 } set= .  0 ok
i{ 6 5 7 } i{ 5 6 7 } set= .  -1 ok

```



## Frink


```frink

a = new set[1, 2]
b = toSet[[2,3]]   // Construct a set from an array

a.contains[2]  // Element test (returns true)
union[a,b]
intersection[a,b]
setDifference[a,b]
isSubset[a,b]  // Returns true if a is a subset of b
a==b           // set equality test

```



## FunL


```funl
A = {1, 2, 3}
B = {3, 4, 5}
C = {1, 2, 3, 4, 5}
D = {2, 1, 3}

println( '2 is in A: ' + (2 in A) )
println( '4 is in A: ' + (4 in A) )
println( 'A union B: ' + A.union(B) )
println( 'A intersect B: ' + A.intersect(B) )
println( 'A difference B: ' + A.diff(B) )
println( 'A subset of B: ' + A.subsetOf(B) )
println( 'A subset of B: ' + A.subsetOf(C) )
println( 'A equal B: ' + (A == B) )
println( 'A equal D: ' + (A == D) )

S = set( A )

println( 'S (mutable version of A): ' + S )
S.add( 4 )
println( 'S with 4 added: ' + S )
println( 'S subset of C: ' + S.subsetOf(C) )
S.remove( 1 )
println( 'S after 1 removed: ' + S )
```


```txt

2 is in A: true
4 is in A: false
A union B: {4, 5, 1, 2, 3}
A intersect B: {3}
A difference B: {1, 2}
A subset of B: false
A subset of B: true
A equal B: false
A equal D: true
S (mutable version of A): {1, 2, 3}
S with 4 added: {1, 2, 3, 4}
S subset of C: true
S after 1 removed: {2, 3, 4}

```



## Go

A common complaint is that Go has no native set type and so there are a number of third-party libraries offering to fill this perceived gap.  Yet Go has good native support for most applications for sets.

### Maps

Go maps meet the task description in that they do not require orderable elements.  To demonstrate that, a set of complex numbers is shown here.  Complex numbers can be compared for equality but are not ordered.

```go
package main

import "fmt"

// Define set as a type to hold a set of complex numbers.  A type
// could be defined similarly to hold other types of elements.  A common
// variation is to make a map of interface{} to represent a set of
// mixed types.  Also here the map value is a bool.  By always storing
// true, the code is nicely readable.  A variation to use less memory
// is to make the map value an empty struct.  The relative advantages
// can be debated.
type set map[complex128]bool

func main() {
    // task: set creation
    s0 := make(set)             // create empty set
    s1 := set{3: true}          // create set with one element
    s2 := set{3: true, 1: true} // create set with two elements

    // option: another way to create a set
    s3 := newSet(3, 1, 4, 1, 5, 9)

    // option: output!
    fmt.Println("s0:", s0)
    fmt.Println("s1:", s1)
    fmt.Println("s2:", s2)
    fmt.Println("s3:", s3)

    // task: element predicate
    fmt.Printf("%v ∈ s0: %t\n", 3, s0.hasElement(3))
    fmt.Printf("%v ∈ s3: %t\n", 3, s3.hasElement(3))
    fmt.Printf("%v ∈ s3: %t\n", 2, s3.hasElement(2))

    // task: union
    b := set{4: true, 2: true}
    fmt.Printf("s3 ∪ %v: %v\n", b, union(s3, b))

    // task: intersection
    fmt.Printf("s3 ∩ %v: %v\n", b, intersection(s3, b))

    // task: difference
    fmt.Printf("s3 \\ %v: %v\n", b, difference(s3, b))

    // task: subset predicate
    fmt.Printf("%v ⊆ s3: %t\n", b, subset(b, s3))
    fmt.Printf("%v ⊆ s3: %t\n", s2, subset(s2, s3))
    fmt.Printf("%v ⊆ s3: %t\n", s0, subset(s0, s3))

    // task: equality
    s2Same := set{1: true, 3: true}
    fmt.Printf("%v = s2: %t\n", s2Same, equal(s2Same, s2))

    // option: proper subset
    fmt.Printf("%v ⊂ s2: %t\n", s2Same, properSubset(s2Same, s2))
    fmt.Printf("%v ⊂ s3: %t\n", s2Same, properSubset(s2Same, s3))

    // option: delete.  it's built in.
    delete(s3, 3)
    fmt.Println("s3, 3 deleted:", s3)
}

func newSet(ms ...complex128) set {
    s := make(set)
    for _, m := range ms {
        s[m] = true
    }
    return s
}

func (s set) String() string {
    if len(s) == 0 {
        return "∅"
    }
    r := "{"
    for e := range s {
        r = fmt.Sprintf("%s%v, ", r, e)
    }
    return r[:len(r)-2] + "}"
}

func (s set) hasElement(m complex128) bool {
    return s[m]
}

func union(a, b set) set {
    s := make(set)
    for e := range a {
        s[e] = true
    }
    for e := range b {
        s[e] = true
    }
    return s
}

func intersection(a, b set) set {
    s := make(set)
    for e := range a {
        if b[e] {
            s[e] = true
        }
    }
    return s
}

func difference(a, b set) set {
    s := make(set)
    for e := range a {
        if !b[e] {
            s[e] = true
        }
    }
    return s
}

func subset(a, b set) bool {
    for e := range a {
        if !b[e] {
            return false
        }
    }
    return true
}

func equal(a, b set) bool {
    return len(a) == len(b) && subset(a, b)
}

func properSubset(a, b set) bool {
    return len(a) < len(b) && subset(a, b)
}
```

```txt

s0: ∅
s1: {(3+0i)}
s2: {(3+0i), (1+0i)}
s3: {(3+0i), (1+0i), (4+0i), (5+0i), (9+0i)}
3 ∈ s0: false
3 ∈ s3: true
2 ∈ s3: false
s3 ∪ {(4+0i), (2+0i)}: {(5+0i), (9+0i), (2+0i), (3+0i), (1+0i), (4+0i)}
s3 ∩ {(2+0i), (4+0i)}: {(4+0i)}
s3 \ {(4+0i), (2+0i)}: {(5+0i), (9+0i), (3+0i), (1+0i)}
{(2+0i), (4+0i)} ⊆ s3: false
{(3+0i), (1+0i)} ⊆ s3: true
∅ ⊆ s3: true
{(1+0i), (3+0i)} = s2: true
{(1+0i), (3+0i)} ⊂ s2: false
{(1+0i), (3+0i)} ⊂ s3: true
s3, 3 deleted: {(5+0i), (9+0i), (1+0i), (4+0i)}

```


### Big.Int

If elements of your set are integers or can be indexed by integers, are zero based and relatively "dense", then the big.Int type in the standard library can serve efficiently as a set.  The solution here doesn't even bother to define a set type, it just defines functions that use big.Ints directly as sets.

Note that the elements here, being integers, are of course ordered and so might not meet a strict reading of the task requirements.

```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    // create an empty set
    var s0 big.Int

    // create sets with elements
    s1 := newSet(3)
    s2 := newSet(3, 1)
    s3 := newSet(3, 1, 4, 1, 5, 9)

    // output
    fmt.Println("s0:", format(s0))
    fmt.Println("s1:", format(s1))
    fmt.Println("s2:", format(s2))
    fmt.Println("s3:", format(s3))

    // element predicate
    fmt.Printf("%v ∈ s0: %t\n", 3, hasElement(s0, 3))
    fmt.Printf("%v ∈ s3: %t\n", 3, hasElement(s3, 3))
    fmt.Printf("%v ∈ s3: %t\n", 2, hasElement(s3, 2))

    // union
    b := newSet(4, 2)
    fmt.Printf("s3 ∪ %v: %v\n", format(b), format(union(s3, b)))

    // intersection
    fmt.Printf("s3 ∩ %v: %v\n", format(b), format(intersection(s3, b)))

    // difference
    fmt.Printf("s3 \\ %v: %v\n", format(b), format(difference(s3, b)))

    // subset predicate
    fmt.Printf("%v ⊆ s3: %t\n", format(b), subset(b, s3))
    fmt.Printf("%v ⊆ s3: %t\n", format(s2), subset(s2, s3))
    fmt.Printf("%v ⊆ s3: %t\n", format(s0), subset(s0, s3))

    // equality
    s2Same := newSet(1, 3)
    fmt.Printf("%v = s2: %t\n", format(s2Same), equal(s2Same, s2))

    // proper subset
    fmt.Printf("%v ⊂ s2: %t\n", format(s2Same), properSubset(s2Same, s2))
    fmt.Printf("%v ⊂ s3: %t\n", format(s2Same), properSubset(s2Same, s3))

    // delete
    remove(&s3, 3)
    fmt.Println("s3, 3 removed:", format(s3))
}

func newSet(ms ...int) (set big.Int) {
    for _, m := range ms {
        set.SetBit(&set, m, 1)
    }
    return
}

func remove(set *big.Int, m int) {
    set.SetBit(set, m, 0)
}

func format(set big.Int) string {
    if len(set.Bits()) == 0 {
        return "∅"
    }
    r := "{"
    for e, l := 0, set.BitLen(); e < l; e++ {
        if set.Bit(e) == 1 {
            r = fmt.Sprintf("%s%v, ", r, e)
        }
    }
    return r[:len(r)-2] + "}"
}

func hasElement(set big.Int, m int) bool {
    return set.Bit(m) == 1
}

func union(a, b big.Int) (set big.Int) {
    set.Or(&a, &b)
    return
}

func intersection(a, b big.Int) (set big.Int) {
    set.And(&a, &b)
    return
}

func difference(a, b big.Int) (set big.Int) {
    set.AndNot(&a, &b)
    return
}

func subset(a, b big.Int) bool {
    ab := a.Bits()
    bb := b.Bits()
    if len(ab) > len(bb) {
        return false
    }
    for i, aw := range ab {
        if aw&^bb[i] != 0 {
            return false
        }
    }
    return true
}

func equal(a, b big.Int) bool {
    return a.Cmp(&b) == 0
}

func properSubset(a, b big.Int) (p bool) {
    ab := a.Bits()
    bb := b.Bits()
    if len(ab) > len(bb) {
        return false
    }
    for i, aw := range ab {
        bw := bb[i]
        if aw&^bw != 0 {
            return false
        }
        if aw != bw {
            p = true
        }
    }
    return
}
```

```txt

s0: ∅
s1: {3}
s2: {1, 3}
s3: {1, 3, 4, 5, 9}
3 ∈ s0: false
3 ∈ s3: true
2 ∈ s3: false
s3 ∪ {2, 4}: {1, 2, 3, 4, 5, 9}
s3 ∩ {2, 4}: {4}
s3 \ {2, 4}: {1, 3, 5, 9}
{2, 4} ⊆ s3: false
{1, 3} ⊆ s3: true
∅ ⊆ s3: true
{1, 3} = s2: true
{1, 3} ⊂ s2: false
{1, 3} ⊂ s3: true
s3, 3 removed: {1, 4, 5, 9}

```


### Intsets

Not quite in the stanard library but still in the official "sub repository", intsets are a ''sparse'' bit set.  Like big.Int they use a single bit to represent a possible element, but the sparse representation efficiently allows for large "holes" in the element sequence.  Also the intsets API provides a more set-like terminology so the RC task can be coded more directly.

```go
package main

import (
    "fmt"

    "golang.org/x/tools/container/intsets"
)

func main() {
    var s0, s1 intsets.Sparse // create some empty sets
    s1.Insert(3)              // insert an element
    s2 := newSet(3, 1)        // create sets with elements
    s3 := newSet(3, 1, 4, 1, 5, 9)

    // output
    fmt.Println("s0:", &s0)
    fmt.Println("s1:", &s1)
    fmt.Println("s2:", s2)
    fmt.Println("s3:", s3)

    // element predicate
    fmt.Printf("%v ∈ s0: %t\n", 3, s0.Has(3))
    fmt.Printf("%v ∈ s3: %t\n", 3, s3.Has(3))
    fmt.Printf("%v ∈ s3: %t\n", 2, s3.Has(2))

    // union
    b := newSet(4, 2)
    var s intsets.Sparse
    s.Union(s3, b)
    fmt.Printf("s3 ∪ %v: %v\n", b, &s)

    // intersection
    s.Intersection(s3, b)
    fmt.Printf("s3 ∩ %v: %v\n", b, &s)

    // difference
    s.Difference(s3, b)
    fmt.Printf("s3 \\ %v: %v\n", b, &s)

    // subset predicate
    fmt.Printf("%v ⊆ s3: %t\n", b, b.SubsetOf(s3))
    fmt.Printf("%v ⊆ s3: %t\n", s2, s2.SubsetOf(s3))
    fmt.Printf("%v ⊆ s3: %t\n", &s0, s0.SubsetOf(s3))

    // equality
    s2Same := newSet(1, 3)
    fmt.Printf("%v = s2: %t\n", s2Same, s2Same.Equals(s2))

    // delete
    s3.Remove(3)
    fmt.Println("s3, 3 removed:", s3)
}

func newSet(ms ...int) *intsets.Sparse {
    var set intsets.Sparse
    for _, m := range ms {
        set.Insert(m)
    }
    return &set
}
```

```txt

s0: {}
s1: {3}
s2: {1 3}
s3: {1 3 4 5 9}
3 ∈ s0: false
3 ∈ s3: true
2 ∈ s3: false
s3 ∪ {2 4}: {1 2 3 4 5 9}
s3 ∩ {2 4}: {4}
s3 \ {2 4}: {1 3 5 9}
{2 4} ⊆ s3: false
{1 3} ⊆ s3: true
{} ⊆ s3: true
{1 3} = s2: true
s3, 3 removed: {1 4 5 9}

```



## Groovy


```groovy
def s1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] as Set
def m1 = 6
def m2 = 7
def s2 = [0, 2, 4, 6, 8] as Set
assert m1 in s1                                        : 'member'
assert ! (m2 in s2)                                    : 'not a member'
def su = s1 + s2
assert su == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] as Set : 'union'
def si = s1.intersect(s2)
assert si == [8, 6, 4, 2] as Set                       : 'intersection'
def sd = s1 - s2
assert sd == [1, 3, 5, 7, 9, 10] as Set                : 'difference'
assert s1.containsAll(si)                              : 'subset'
assert ! s1.containsAll(s2)                            : 'not a subset'
assert (si + sd) == s1                                 : 'equality'
assert (s2 + sd) != s1                                 : 'inequality'
assert s1 != su && su.containsAll(s1)                  : 'proper subset'
s1 << 0
assert s1 == su                                        : 'added element 0 to s1'
```



## Haskell

GHC offers a functional, persistent set data structure in its <code>Data.Set</code> module. It is implemented using a binary search tree. Elements must be of an orderable type (instance of <code>Ord</code>).

```haskell
Prelude>
 import Data.Set
Prelude Data.Set> empty :: Set Integer -- Empty set
fromList []
Prelude Data.Set> let s1 = fromList [1,2,3,4,3] -- Convert list into set
Prelude Data.Set> s1
fromList [1,2,3,4]
Prelude Data.Set> let s2 = fromList [3,4,5,6]
Prelude Data.Set> union s1 s2 -- Union
fromList [1,2,3,4,5,6]
Prelude Data.Set> intersection s1 s2 -- Intersection
fromList [3,4]
Prelude Data.Set> s1 \\ s2 -- Difference
fromList [1,2]
Prelude Data.Set> s1 `isSubsetOf` s1 -- Subset
True
Prelude Data.Set> fromList [3,1] `isSubsetOf` s1
True
Prelude Data.Set> s1 `isProperSubsetOf` s1 -- Proper subset
False
Prelude Data.Set> fromList [3,1] `isProperSubsetOf` s1
True
Prelude Data.Set> fromList [3,2,4,1] == s1 -- Equality
True
Prelude Data.Set> s1 == s2
False
Prelude Data.Set> 2 `member` s1 -- Membership
True
Prelude Data.Set> 10 `notMember` s1
True
Prelude Data.Set> size s1 -- Cardinality
4
Prelude Data.Set> insert 99 s1 -- Create a new set by inserting
fromList [1,2,3,4,99]
Prelude Data.Set> delete 3 s1 -- Create a new set by deleting
fromList [1,2,4]
```


Regular lists can also be used as sets. Haskell has some functions to help with using lists as sets. No requirement is made of element type. However, these are not very efficient because they require linear time to find an element.

```haskell
Prelude>
 import Data.List
Prelude Data.List> let s3 = nub [1,2,3,4,3] -- Remove duplicates from list
Prelude Data.List> s3
[1,2,3,4]
Prelude Data.List> let s4 = [3,4,5,6]
Prelude Data.List> union s3 s4 -- Union
[1,2,3,4,5,6]
Prelude Data.List> intersect s3 s4 -- Intersection
[3,4]
Prelude Data.List> s3 \\ s4 -- Difference
[1,2]
Prelude Data.List> 42 : s3 -- Return new list with element inserted at the beginning
[42,1,2,3,4]
Prelude Data.List> delete 3 s3 -- Return new list with first occurrence of element removed
[1,2,4]
```


=={{header|Icon}} and {{header|Unicon}}==

The set is a basic datatype (structure) in Icon and Unicon, which supports 'member', 'union', 'intersection' and 'difference' operations.  Subset and equality must be implemented separately, or use the routines in the 'sets' library.

Implemented directly:


```unicon

procedure display_set (s)
  writes ("[")
  every writes (!s || " ")
  write ("]")
end

# fail unless s1 and s2 contain the same elements
procedure set_equals (s1, s2)
  return subset(s1, s2) & subset(s2, s1)
end

# fail if every element in s2 is not contained in s1
procedure subset (s1, s2)
  every (a := !s2) do {
    if not(member(s1,a)) then fail
  }
  return s2
end

procedure main ()
  a := set(1, 1, 2, 3, 4)
  b := set(2, 3, 5)
  writes ("a: ")
  display_set (a)
  writes ("b: ")
  display_set (b)
  # basic set operations
  writes ("Intersection: ")
  display_set (a ** b)
  writes ("Union: ")
  display_set (a ++ b)
  writes ("Difference: ")
  display_set (a -- b)
  # membership
  if member(a, 2) then
    write ("2 is a member of a")
  else
    write ("2 is not a member of a")
  if member(a, 5) then
    write ("5 is a member of a")
  else
    write ("5 is not a member of a")
  # equality
  if set_equals(a, set(1,2,3,4,4)) then
    write ("a equals set(1,2,3,4,4)")
  else
    write ("a does not equal set(1,2,3,4,4)")
  if set_equals(a, b) then
    write ("a equals b")
  else
    write ("a does not equal b")
  # subset
  if subset(a, set(1,2)) then
    write ("(1,2) is included in a")
  else
    write ("(1,2) is not included in a")
  if subset(a, set(1,2,5)) then
    write ("(1,2,5) is included in a")
  else
    write ("(1,2,5) is not included in a")
end

```


```txt

a: [2 4 1 3 ]
b: [5 2 3 ]
Intersection: [2 3 ]
Union: [5 2 4 1 3 ]
Difference: [4 1 ]
2 is a member of a
5 is not a member of a
a equals set(1,2,3,4,4)
a does not equal b
(1,2) is included in a
(1,2,5) is not included in a

```


Using library:


```unicon

link sets

procedure main ()
  a := set(1, 1, 2, 3, 4)
  b := set(2, 3, 5)
  write ("a: ", simage(a))
  write ("b: ", simage(b))
  # basic set operations
  write ("Intersection: ", simage (a**b))
  write ("Union: ", simage        (a++b))
  write ("Difference: ", simage   (a--b))
  # membership
  if member(a, 2) then
    write ("2 is a member of a")
  else
    write ("2 is not a member of a")
  if member(a, 5) then
    write ("5 is a member of a")
  else
    write ("5 is not a member of a")
  # equality
  if seteq(a, set(1,2,3,4,4)) then
    write ("a equals set(1,2,3,4,4)")
  else
    write ("a does not equal set(1,2,3,4,4)")
  if seteq(a, b) then
    write ("a equals b")
  else
    write ("a does not equal b")
  # check subset
  if setlt(set(1,2), a) then
    write ("(1,2) is included in a")
  else
    write ("(1,2) is not included in a")
  if setlt(a, set(1,2,5), a) then
    write ("(1,2,5) is included in a")
  else
    write ("(1,2,5) is not included in a")
end

```


```txt

a: { 2, 4, 1, 3 }
b: { 5, 2, 3 }
Intersection: { 2, 3 }
Union: { 5, 2, 4, 1, 3 }
Difference: { 4, 1 }
2 is a member of a
5 is not a member of a
a equals set(1,2,3,4,4)
a does not equal b
(1,2) is included in a
(1,2,5) is not included in a

```




## J


In J, we use a sequence to represent a set.  This actually winds up being a family of set implementations.  In this example, we chose to ignore order and specify that duplicate elements are not allowed.

Here are definitions for the required operations:


```j
union=: ~.@,
intersection=: [ -. -.
difference=: -.
subset=: *./@e.
equality=: -:&(/:~)
```


Examples:


```j
  2 4 6 8 ~.@, 2 3 5 7
2 4 6 8 3 5 7
   2 4 6 8 ([ -. -.) 2 3 5 7
2
   2 4 6 8 -. 2 3 5 7
4 6 8
   2 4 6 8 *./@e. 2 3 5 7
0
   '' *./@e. 2 3 5 7
1
   2 4 6 8 3 5 7 -:&(/:~) 8 7 6 5 4 3 2
1
```


Examples again, using names rather than code:


```j
   2 4 6 8 union 2 3 5 7
2 4 6 8 3 5 7
   2 4 6 8 intersection 2 3 5 7
2
   2 4 6 8 difference 2 3 5 7
4 6 8
   2 4 6 8 subset 2 3 5 7
0
   '' subset 2 3 5 7
1
   2 4 6 8 3 5 7 equality 8 7 6 5 4 3 2
1
```


Note that J uses 1 for true and 0 for false.  Mathematical revisionists object to this, but this is consistent with the original (and revised) formulations of boolean algebra.  (And there are deep ties to Bayes' rule.)

Note that these operations can be combined in sentences with other operations.  For example we could define


```j
properSubset=: subset * 1 - equality
```



## Java

To use this in Java 5 replace all "<>" with "<Integer>".

```java5
import java.util.Arrays;
import java.util.Collections;
import java.util.Set;
import java.util.TreeSet;

public class Sets {
    public static void main(String[] args){
        Set<Integer> a = new TreeSet<>();
        //TreeSet sorts on natural ordering (or an optional comparator)
        //other options: HashSet (hashcode)
        //               LinkedHashSet (insertion order)
        //               EnumSet (optimized for enum values)
        //others at: http://download.oracle.com/javase/7/docs/api/java/util/Set.html
        Set<Integer> b = new TreeSet<>();
        Set<Integer> c = new TreeSet<>();
        Set<Integer> d = new TreeSet<>();

        a.addAll(Arrays.asList(1, 2, 3, 4, 5));
        b.addAll(Arrays.asList(2, 3, 4, 5, 6, 8));
        c.addAll(Arrays.asList(2, 3, 4));
        d.addAll(Arrays.asList(2, 3, 4));
        System.out.println("a: " + a);
        System.out.println("b: " + b);
        System.out.println("c: " + c);
        System.out.println("d: " + d);

        System.out.println("2 in a: " + a.contains(2));
        System.out.println("6 in a: " + a.contains(6));

        Set<Integer> ab = new TreeSet<>();
        ab.addAll(a);
        ab.addAll(b);
        System.out.println("a union b: " + ab);

        Set<Integer> a_b = new TreeSet<>();
        a_b.addAll(a);
        a_b.removeAll(b);
        System.out.println("a - b: " + a_b);

        System.out.println("c subset of a: " + a.containsAll(c));
        //use a.conatins() for single elements

        System.out.println("c = d: " + c.equals(d));
        System.out.println("d = c: " + d.equals(c));

        Set<Integer> aib = new TreeSet<>();
        aib.addAll(a);
        aib.retainAll(b);
        System.out.println("a intersect b: " + aib);

        System.out.println("add 7 to a: " + a.add(7));
        System.out.println("add 2 to a again: " + a.add(2));

        //other noteworthy things related to sets:
        Set<Integer> empty = Collections.EMPTY_SET; //immutable empty set
        //empty.add(2);  would fail
        empty.isEmpty(); //test if a set is empty
        empty.size();
        Collections.disjoint(a, b); //returns true if the sets have no common elems (based on their .equals() methods)
        Collections.unmodifiableSet(a); //returns an immutable copy of a
    }
}
```

```txt
a: [1, 2, 3, 4, 5]
b: [2, 3, 4, 5, 6, 8]
c: [2, 3, 4]
d: [2, 3, 4]
2 in a: true
6 in a: false
a union b: [1, 2, 3, 4, 5, 6, 8]
a - b: [1]
c subset of a: true
c = d: true
d = c: true
a intersect b: [2, 3, 4, 5]
add 7 to a: true
add 2 to a again: false
```



## JavaScript

JavaScript does not support native sets before ECMAScript 6.


```javascript

var set = new Set();

set.add(0);
set.add(1);
set.add('two');
set.add('three');

set.has(0); //=> true
set.has(3); //=> false
set.has('two'); // true
set.has(Math.sqrt(4)); //=> false
set.has('TWO'.toLowerCase()); //=> true

set.size; //=> 4

set.delete('two');
set.has('two'); //==> false
set.size; //=> 3

//iterating set using ES6 for..of
//Set order is preserved in order items are added.
for (var item of set) {
  console.log('item is ' + item);
}
```



## jq

Neither JSON nor jq has a "set" type, but as explained below in the first part of this entry, finite sets of Unicode strings can be directly represented
in JSON and thus jq.

The second part of this entry focuses on a jq library of set-theoretic functions that support finite sets of arbitrary JSON entities.


### Finite Sets of Unicode Strings


There is an obvious 1-1 mapping between the collection of finite sets of Unicode strings and
the collection of JSON objects with distinct keys the values of which all have the boolean value "true".
For example, the set of strings {"a", "b"} corresponds to the JSON object {"a": true, "b": true }.

When restricted to such JSON objects, jq's equality operator ("==") yields set-theoretic semantics,
and similarly, jq's + operator yields set-theoretic union.

For example:

```jq
{"a":true, "b":true } == {"b":true, "a":true}.
{"a":true} + {"b":true } == { "a":true, "b":true}
```


Thus, it can be seen that jq has built-in support for sets of finite-length Unicode strings.

For simplicity and to avoid confusion, we shall refer to JSON objects all of whose keys are distinct and all values of which have the boolean value "true" as "string sets".

'''String-set test'''

Here is a jq filter for determining whether a JSON object is a "string set":

```jq
def is_stringset:
  . as $in | type == "object" and reduce keys[] as $key (true; . and $in[$key] == true);
```


'''String-set membership''':

The test for set membership, m ∈ S, where m is a string and S is a
set of strings, corresponds exactly to the jq test:

```jq
T | has(m)
```

where T is the JSON object corresponding to S.  This test is also efficient.

'''String-set intersection'''

```jq
# Set-intersection: A ∩ B
def stringset_intersection(A;B):
  reduce (A|keys)[] as $k
    ({}; if (B|has($k)) then . + {($k):true} else . end);
```


'''String-set difference'''

```jq
# stringset_difference: A \ B
def stringset_difference(A;B):
  reduce (A|keys)[] as $k
    ({}; if (B|has($k)) then . else . + {($k):true} end);
```


'''Subset'''

```jq
# A ⊆ B iff string_subset(A;B)
def stringset_subset(A;B):
  reduce (A|keys)[] as $k
    (true; . and (B|has($k)));
```



###  Finite Sets of JSON Entities

Finite sets of arbitrary JSON entities can be represented by sets of
strings using an invertible serialization of JSON entities, but in
the remainder of this entry, we provide a more straightforward
and probably more efficient implementation of finite sets using JSON arrays.

Specifically, the empty set is represented by [] and a non-empty set
of JSON entities with distinct members m1, m2, ... mN is
represented by the JSON array [s1, s2, ... sN] where:

[s1, s2, ... sN] is the result of ([m1, m2, ... mN] | sort)

When confined to sorted arrays, jq's equality operator (==) yields set-theoretic
semantics, and therefore, for the remainder of this entry, we shall refer
to sorted arrays simply as sets.

To convert an arbitrary jq or JSON array to a set, we can simply use the built-in
jq operator "unique".  To test whether an arbitrary JSON entity is a set
without sorting:

```jq
def is_set:
  . as $in
  | type == "array" and
    reduce range(0;length-1) as $i
      (true; if . then $in[$i] < $in[$i+1] else false end);
```


The following library of set-theoretic functions is intended for use
with jq version 1.4 or later.  However, as noted below, if used with a
version of jq that does not have bsearch, then it is assumed that a definition of bsearch
equivalent to that given in [[Binary search]] is available.

'''Set creation'''

* [] is the empty set;
* if m1 <= m2 <= ... mN then [m1, m2, ... mN] is the set containing the listed elements;
* The set of elements in an array, a, can be constructed by writing: a | unique
* The set of strings in the string-set SS is: SS|keys

'''m ∈ S'''

If m is a JSON entity and S a set, then the jq expression S[m] can be used to test
whether m is an element of S, but for large sets, this is inefficient. A generally more efficient test membership of m in S would use
bsearch as defined at [[Binary search]] or as provided in recent versions of jq:

```jq
def is_member(m):  bsearch(m) > -1;
```


'''Intersection'''

```jq
# If A and B are sets, then intersection(A;B) emits their intersection:
def intersection($A;$B):
  def pop:
    .[0] as $i
    | .[1] as $j
    | if $i == ($A|length) or $j == ($B|length) then empty
      elif $A[$i] == $B[$j] then $A[$i], ([$i+1, $j+1] | pop)
      elif $A[$i] <  $B[$j] then [$i+1, $j] | pop
      else [$i, $j+1] | pop
      end;
  [[0,0] | pop];
```

'''Difference'''

```jq
# If A and B are sets, then A-B is emitted
def difference(A;B):
  (A|length) as $al
  | (B|length) as $bl
  | if $al == 0 then [] elif $bl == 0 then A
    else
      reduce range(0; $al + $bl) as $k
        ( [0, 0, []];
          .[0] as $i | .[1] as $j
          | if $i < $al and $j < $bl then
              if A[$i] == B[$j] then [ $i+1, $j+1,  .[2] ]
              elif  A[$i] < B[$j] then [ $i+1, $j, .[2] + [A[$i]] ]
              else [ $i , $j+1, .[2] ]
              end
            elif $i < $al then [ $i+1, $j,  .[2] + [A[$i]] ]
            else .
            end
         ) | .[2]
    end ;
```


'''Union'''

A simple but inefficient implementation would use: (A + B) | unique

To compute the union of two sets efficiently, it is helpful to define a function for merging sorted arrays.

```jq
# merge input array with array x by comparing the heads of the arrays in turn;
# if both arrays are sorted, the result will be sorted:
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
           if .[0] < $length and ((.[1] < $xl and $in[.[0]] <= x[.[1]]) or .[1] == $xl)
           then [(.[0] + 1), .[1], (.[2] + [$in[.[0]]]) ]
           else [.[0], (.[1] + 1), (.[2] + [x[.[1]]]) ]
           end
         ) | .[2]
    end ;

def union(A;B):
  A|merge(B)
  | reduce .[] as $m ([]; if length == 0 or .[length-1] != $m then . + [$m] else . end);
```


'''A ⊆ B'''

```jq
def subset(A;B):
  # TCO
  def _subset:
    if .[0]|length == 0 then true
    elif .[1]|length == 0 then false
    elif .[0][0] == .[1][0] then [.[0][1:], .[1][1:]] | _subset
    elif .[0][0] < .[1][0] then false
    else [ .[0], .[1][1:] ] | _subset
    end;
  [A,B] | _subset;
```


'''Test whether two sets intersect'''

The following implementation assumes a version of jq with ''bsearch/1''.

If A and B are sets (i.e. A == (A|unique) and B == (B|unique)),
then ''[A,B] | intersect'' emits true if A and B have at least one element in common:

```jq
def intersect:
 .[0] as $A  | .[1] as $B
 | ($A|length) as $al
  | ($B|length) as $bl
  | if $al == 0 or $bl == 0 then false
    else
      ($B | bsearch($A[0])) as $b
      | if $b >= 0 then true
        else [$A[1:], $B[- (1 + $b) :]] | intersect
        end
    end;

```



## Julia


```txt
julia> S1 = Set(1:4) ; S2 = Set(3:6) ; println(S1,"\n",S2)
Set{Int64}({4,2,3,1})
Set{Int64}({5,4,6,3})

julia> 5 in S1 , 5 in S2
(false,true)

julia> intersect(S1,S2)
Set{Int64}({4,3})

julia> union(S1,S2)
Set{Int64}({5,4,6,2,3,1})

julia> setdiff(S1,S2)
Set{Int64}({2,1})

julia> issubset(S1,S2)
false

julia> isequal(S1,S2)
false

julia> symdiff(S1,S2)
Set{Int64}({5,6,2,1})
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
   val fruits  = setOf("apple", "pear", "orange", "banana")
   println("fruits  : $fruits")
   val fruits2 = setOf("melon", "orange", "lemon", "gooseberry")
   println("fruits2 : $fruits2\n")

   println("fruits  contains 'banana'     : ${"banana" in fruits}")
   println("fruits2 contains 'elderberry' : ${"elderbury" in fruits2}\n")

   println("Union        : ${fruits.union(fruits2)}")
   println("Intersection : ${fruits.intersect(fruits2)}")
   println("Difference   : ${fruits.minus(fruits2)}\n")

   println("fruits2 is a subset of fruits : ${fruits.containsAll(fruits2)}\n")
   val fruits3 = fruits
   println("fruits3 : $fruits3\n")
   var areEqual = fruits.containsAll(fruits2) && fruits3.containsAll(fruits)
   println("fruits2 and fruits are equal  : $areEqual")
   areEqual = fruits.containsAll(fruits3) && fruits3.containsAll(fruits)
   println("fruits3 and fruits are equal  : $areEqual\n")

   val fruits4 = setOf("apple", "orange")
   println("fruits4 : $fruits4\n")
   var isProperSubset = fruits.containsAll(fruits3) && !fruits3.containsAll(fruits)
   println("fruits3 is a proper subset of fruits : $isProperSubset")
   isProperSubset = fruits.containsAll(fruits4) && !fruits4.containsAll(fruits)
   println("fruits4 is a proper subset of fruits : $isProperSubset\n")

   val fruits5 = mutableSetOf("cherry", "blueberry", "raspberry")
   println("fruits5 : $fruits5\n")
   fruits5 += "guava"
   println("fruits5 + 'guava'  : $fruits5")
   println("fruits5 - 'cherry' : ${fruits5 - "cherry"}")
}
```


```txt

fruits  : [apple, pear, orange, banana]
fruits2 : [melon, orange, lemon, gooseberry]

fruits  contains 'banana'     : true
fruits2 contains 'elderberry' : false

Union        : [apple, pear, orange, banana, melon, lemon, gooseberry]
Intersection : [orange]
Difference   : [apple, pear, banana]

fruits2 is a subset of fruits : false

fruits3 : [apple, pear, orange, banana]

fruits2 and fruits are equal  : false
fruits3 and fruits are equal  : true

fruits4 : [apple, orange]

fruits3 is a proper subset of fruits : false
fruits4 is a proper subset of fruits : true

fruits5 : [cherry, blueberry, raspberry]

fruits5 + 'guava'  : [cherry, blueberry, raspberry, guava]
fruits5 - 'cherry' : [blueberry, raspberry, guava]

```



## Lasso


```Lasso
// Extend set type
define set->issubsetof(p::set) => .intersection(#p)->size == .size
define set->oncompare(p::set) => .intersection(#p)->size - .size

//	Set creation
local(set1) = set('j','k','l','m','n')
local(set2) = set('m','n','o','p','q')

//Test m ∈ S -- "m is an element in set S"
#set1 >> 'm'

// A ∪ B -- union; a set of all elements either in set A or in set B.
#set1->union(#set2)

//A ∩ B -- intersection; a set of all elements in both set A and set B.
#set1->intersection(#set2)

//A ∖ B -- difference; a set of all elements in set A, except those in set B.
#set1->difference(#set2)

//A ⊆ B -- subset; true if every element in set A is also in set B.
#set1->issubsetof(#set2)

//A = B -- equality; true if every element of set A is in set B and vice-versa.
#set1 == #set2
```


```txt
true
set(j, k, l, m, n, o, p, q)
set(m, n)
set(j, k, l)
false
false
```



## LFE

```lisp

> (set set-1 (sets:new))
#(set 0 16 16 8 80 48 ...)
> (set set-2 (sets:add_element 'a set-1))
#(set 1 16 16 8 80 48 ...)
> (set set-3 (sets:from_list '(a b)))
#(set 2 16 16 8 80 48 ...)
> (sets:is_element 'a set-2)
true
> (set union (sets:union set-2 set-3))
#(set 2 16 16 8 80 48 ...)
> (sets:to_list union)
(a b)
> (set intersect (sets:intersection set-2 set-3))
#(set 1 16 16 8 80 48 ...)
> (sets:to_list intersect)
(a)
> (set subtr (sets:subtract set-3 set-2))
#(set 1 16 16 8 80 48 ...)
> (sets:to_list subtr)
(b)
> (sets:is_subset set-2 set-3)
true
> (=:= set-2 set-3)
false
> (set set-4 (sets:add_element 'b set-2))
#(set 2 16 16 8 80 48 ...)
> (=:= set-3 set-4)
true

```



## Liberty BASIC

Sets are not natively available- implemented here in string form so no need to dim/redim or pass number of elements.

```lb

A$ ="red hot chili peppers rule OK"
B$ ="lady in red"

print " New set, in space-separated form. Extra spaces and duplicates will be removed. "
input newSet$
 newSet$  =trim$(           newSet$)
 newSet$  =stripBigSpaces$( newSet$)
 newSet$  =removeDupes$(    newSet$)
print " Set stored as the string '"; newSet$; "'"

print
print " 'red'  is an element of '"; A$; "' is "; isAnElementOf$( "red",  A$)
print " 'blue' is an element of '"; A$; "' is "; isAnElementOf$( "blue",  A$)
print " 'red'  is an element of '"; B$; "' is "; isAnElementOf$( "red",  B$)
print
print " Union        of '"; A$; "' & '"; B$; "' is '"; unionOf$( A$, B$); "'."
print
print " Intersection of '"; A$; "' & '"; B$; "' is '"; intersectionOf$( A$, B$); "'."
print
print " Difference   of '"; A$; "' & '"; B$; "' is '"; differenceOf$( A$, B$); "'."
print
print " '"; A$; "' equals '";        A$; "' is "; equalSets$( A$, A$)
print " '"; A$; "' equals '";        B$; "' is "; equalSets$( A$, B$)
print
print  " '"; A$; "' is a subset of '"; B$; "' is "; isSubsetOf$( A$, B$)
print  " 'red peppers' is a subset of 'red hot chili peppers rule OK' is "; isSubsetOf$( "red peppers", "red hot chili peppers rule OK")

end

function removeDupes$( a$)
    numElements =countElements( a$)
    redim elArray$( numElements)         '   ie 4 elements are array entries 1 to 4 and 0 is spare =""
    for m =0 to numElements
        el$ =word$( a$, m, " ")
        elArray$( m) =el$
    next m
    sort elArray$(), 0, numElements
    b$           =""
    penultimate$ ="999"
    for jk =0 to numElements    '   do not use "" ( nuls) or elementsalready seen
        if elArray$( jk) ="" then [on]
        if elArray$( jk) <>penultimate$ then b$ =b$ +elArray$( jk) +" ": penultimate$ =elArray$( jk)
        [on]
    next jk
    b$ =trim$( b$)
    removeDupes$ =b$
end function

function stripBigSpaces$( a$)   '   copy byte by byte, but id=f a space had a preceding space, ignore it.
    lenA =len( a$)
    penul$ =""
    for i =1 to len( a$)
        c$ =mid$( a$, i, 1)
        if c$ <>" " then
            if penul$ <>" " then
                b$ =b$ +c$
            else
                b$ =b$ +" " +c$
            end if
        end if
        penul$ =c$
    next i
    stripBigSpaces$ =b$
end function

function countElements( a$) '   count elements repr'd by space-separated words in string rep'n.
    if isNul$( a$) ="True" then countElements =0: exit function
    i  =0
    do
        el$ =word$( a$, i +1, " ")
        i =i +1
    loop until el$ =""
    countElements =i -1
end function

function isNul$( a$)    '   a nul set implies its string rep'n is length zero.
    if a$ ="" then isNul$ ="True" else isNul$ ="False"
end function

function isAnElementOf$( a$, b$)    '   check element a$ exists in set b$.
    isAnElementOf$ ="False"
    i  =0
    do
        el$ =word$( b$, i +1, " ")
        if a$ =el$ then isAnElementOf$ ="True"
        i =i +1
    loop until el$ =""
end function

function unionOf$( a$, b$)
    i  =1
    o$ =a$
    do
        w$ =word$( b$, i, " ")
        if w$ ="" then exit do
        if isAnElementOf$( w$, a$) ="False" then o$ =o$ +" " +w$
        i =i +1
    loop until w$ =""
    unionOf$ =o$
end function

function intersectionOf$( a$, b$)
    i  =1
    o$ =""
    do
        el$ =word$( a$, i, " ")
        if el$ ="" then exit do
        if ( isAnElementOf$( el$, b$) ="True") and ( o$ ="")  then o$ =el$
        if ( isAnElementOf$( el$, b$) ="True") and ( o$ <>el$) then o$ =o$ +" " +el$
        i =i +1
    loop until el$ =""
    intersectionOf$ =o$
end function

function equalSets$( a$, b$)
    if len( a$) <>len( b$) then equalSets$ ="False": exit function
    i =1
    do
        el$ =word$( a$, i, " ")
        if isAnElementOf$( el$, b$) ="False" then equalSets$ ="False": exit function
        i =i +1
    loop until w$ =""
    equalSets$ ="True"
end function

function differenceOf$( a$, b$)
    i  =1
    o$ =""
    do
        el$ =word$( a$, i, " ")
        if el$ ="" then exit do
        if ( isAnElementOf$( el$, b$) ="False") and ( o$ ="")   then o$ =el$
        if ( isAnElementOf$( el$, b$) ="False") and ( o$ <>el$) then o$ =o$ +" " +el$
        i =i +1
    loop until el$ =""
    differenceOf$ =o$
end function

function isSubsetOf$( a$, b$)
    isSubsetOf$ ="True"
    i  =1
    do
        el$ =word$( a$, i, " ")
        if el$ ="" then exit do
        if ( isAnElementOf$( el$, b$) ="False") then isSubsetOf$ ="False": exit function
        i =i +1
    loop until el$ =""
end function


```



 New set, in space-separated form. Extra spaces and duplicates will be removed.
 ? now is the the time for all good all men
 Set stored as the string 'all for good is men now the time'
 'red' is an element of 'red hot chili peppers rule OK' is True
 'blue' is an element of 'red hot chili peppers rule OK' is False
 'red' is an element of 'lady in red' is True
 Union of 'red hot chili peppers rule OK' & 'lady in red' is 'red hot chili peppers rule OK lady in'.
 Intersection of 'red hot chili peppers rule OK' & 'lady in red' is 'red'.
 Difference of 'red hot chili peppers rule OK' & 'lady in red' is 'hot chili peppers rule OK'.
 'red hot chili peppers rule OK' equals 'red hot chili peppers rule OK' is True
 'red hot chili peppers rule OK' equals 'lady in red' is False
 'red hot chili peppers rule OK' is a subset of 'lady in red' is False
 'red peppers' is a subset of 'red hot chili peppers rule OK' is True


## Lua

```lua
function emptySet()         return { }  end
function insert(set, item)  set[item] = true  end
function remove(set, item)  set[item] = nil  end
function member(set, item)  return set[item]  end
function size(set)
	local result = 0
	for _ in pairs(set) do result = result + 1 end
	return result
end
function fromTable(tbl) -- ignore the keys of tbl
	local result = { }
	for _, val in pairs(tbl) do
		result[val] = true
	end
	return result
end
function toArray(set)
	local result = { }
	for key in pairs(set) do
		table.insert(result, key)
	end
	return result
end
function printSet(set)
	print(table.concat(toArray(set), ", "))
end
function union(setA, setB)
	local result = { }
	for key, _ in pairs(setA) do
		result[key] = true
	end
	for key, _ in pairs(setB) do
		result[key] = true
	end
	return result
end
function intersection(setA, setB)
	local result = { }
	for key, _ in pairs(setA) do
		if setB[key] then
			result[key] = true
		end
	end
	return result
end
function difference(setA, setB)
	local result = { }
	for key, _ in pairs(setA) do
		if not setB[key] then
			result[key] = true
		end
	end
	return result
end
function subset(setA, setB)
	for key, _ in pairs(setA) do
		if not setB[key] then
			return false
		end
	end
	return true
end
function properSubset(setA, setB)
	return subset(setA, setB) and (size(setA) ~= size(setB))
end
function equals(setA, setB)
	return subset(setA, setB) and (size(setA) == size(setB))
end

```


(May work with earlier versions but not tested on those.)

This implementation creates, in effect, a set type with operators for comparisons (subset, equality, true subset), and set operations like unions, differences, and intersections.  It is a mutable set type, so primitives exist for insertion and removal of elements.  Elements can be tested for presence O(1) with the has() method or can be iterated over as an array-flavoured table since this is what the type presents as.  (All of its functionality is buried in metatables.)

The code is intended to be placed into a file and accessed as a module.  E.g. if placed into the file "set.lua" it would be accessed with <code>set = require 'set'</code>.


```lua
local function new(_, ...)
  local r = {}
  local s = setmetatable({}, {
    -- API operations
    __index = {

      -- single value insertion
      insert = function(s, v)
        if not r[v] then
          table.insert(s, v)
          r[v] = #s
        end
        return s
      end,

      -- single value removal
      remove = function(s, v)
        local i = r[v]
        if i then
          r[v] = nil
          local t = table.remove(s)
          if t ~= v then
            r[t] = i
            s[i] = t
          end
        end
        return s
      end,

      -- multi-value insertion
      batch_insert = function(s, ...)
        for _,v in pairs {...} do
          s:insert(v)
        end
        return s
      end,

      -- multi-value removal
      batch_remove = function(s, ...)
        for _,v in pairs {...} do
          s:remove(v)
        end
        return s
      end,

      -- membership test
      has = function(s, e)
        return r[e] ~= nil
      end
    },

    -- set manipulation operators

    -- union
    __add = function(s1, s2)
      r = set()
      r:batch_insert(table.unpack(s1))
      r:batch_insert(table.unpack(s2))
      return r
    end,

    -- subtraction
    __sub = function(s1, s2)
      r = set()
      r:batch_insert(table.unpack(s1))
      r:batch_remove(table.unpack(s2))
      return r
    end,

    -- intersection
    __mul = function(s1, s2)
      r = set()
      for _,v in ipairs(s1) do
        if s2:has(v) then
          r:insert(v)
        end
      end
      return r
    end,

    -- equality
    __eq = function(s1, s2)
      if #s1 ~= #s2 then return false end
      for _,v in ipairs(s1) do
        if not s2:has(v) then return false end
      end
      return true
    end,

    -- proper subset
    __lt = function(s1, s2)
      if s1 == s2 then return false end
      for _,v in ipairs(s1) do
        if not s2:has(v) then return false end
      end
      return true
    end,

    -- subset
    __lte = function(s1, s2)
      return (s1 == s2) or (s1 < s2)
    end,

    -- metatable type tag
    __type__ = 'set'
  })
  s:batch_insert(...)
  return s
end

return setmetatable({}, { __call = new })
```


## M2000 Interpreter

A tuple is a referenced type of an array, with variant type for each item (may also be a reference to another tuple). The empty tuple is this (,), and the one item is this (1,), and two items (1,2)
For search in a tuple we have O(N).


```M2000 Interpreter

Module Sets {
	setA=("apple", "cherry", "grape")
	setB=("banana","cherry", "date")

	Print Len(setA)=3 'true
	Print setA#pos("apple")>=0=true   ' exist
	Print setA#pos("banana")>=0=False  ' not exist

	intersection=lambda  SetB (x$)-> SetB#pos(x$)>=0
	SetC=SetA#filter(intersection,(,))
	Print SetC

	Difference= lambda (aSet)->{
		=lambda  aSet (x$)-> aSet#pos(x$)<0
	}
	IsetC=SetB#filter(Difference(setA),(,))
	Print SetC
	SetC=SetA#filter(Difference(setB),(,))
	Print SetC

	k=each(setB)
	SetC=cons(setA)
	while k
		if setA#pos(SetB#val$(k^))<0 then Append SetC, (SetB#val$(k^),)
	end while
	Print SetC
	\\ subset if items exists in same order
	Print SetA#pos("cherry","grape")>=0 ' true ' is a subset of SetA
	Print SetA#pos(("apple", "cherry"))>=0 ' true ' is a subset of SetA
	Print SetA#pos(("apple","grape"))>=0 ' false ' is not a subset of SetA in that order
	\\ subset in any position
	fold1=lambda (aSet)-> {
		=lambda aSet (x$, cond) ->{
			push cond and aSet#pos(x$)>=0
		}
	}
	SetC=("banana", "date")
	print SetC#Fold(fold1(SetA), True)  ' False
	print SetC#Fold(fold1(SetB), True)  ' True
	SetC=("cherry",)
	print SetC#Fold(fold1(SetA), True)  ' True
	print SetC#Fold(fold1(SetB), True)  ' True
	\\ Mutation
	\\ change value at position 0
	return SetC, 0:="banana"
	print SetC#Fold(fold1(SetA), True)  ' False
	print SetC#Fold(fold1(SetB), True)  ' True

	\\ equality
	SetC=Cons(SetA)  ' we get a copy of one or more tuple
	\\ SetC is subset of SetA and SetA is subset of  SetC
	Print SetC#Fold(fold1(SetA), True)=SetA#Fold(fold1(SetC), True)  ' True
	\\ another way
	Print Len(SetC#filter(Difference(setA),(,)))=0   ' true   \\ difference is an empty tuple
	append SetC, SetB
	Print Len(SetC)=6 ' true
	print SetC#pos(0 ->"cherry")=1 ' true
	print SetC#pos(2 -> "cherry")=4 ' true
	print SetC#pos(5 -> "cherry")=-1 ' true
	print SetC#pos(0 -> "banana","cherry")=3 ' true
	print SetC#pos( "banana","cherry")=3 ' true
	mapU=lambda ->{
		push ucase$(letter$)
	}
	fold2=lambda (x$, k$)->{
		push replace$(")(", ", ",k$+"("+quote$(x$)+")")
	}
	Print SetC#map(mapU)#fold$(fold2, "") ' ("APPLE", "CHERRY", "GRAPE", "BANANA", "CHERRY", "DATE")
	Print SetC#map(mapU)  ' APPLE CHERRY GRAPE BANANA CHERRY DATE
	Print SetC#fold$(fold2, "")  ' ("apple", "cherry", "grape", "banana", "cherry", "date")

}
Sets

```




## Maple

Sets in Maple are built-in, native data structures, and are immutable.  Sets are formed by enclosing a sequence of objects between braces.  You can get something essentially equivalent to set comprehensions by using {seq}, which applies the set constructor ("{}") to the sequencing operation "seq".

```Maple

> S := { 2, 3, 5, 7, 11, Pi, "foo", { 2/3, 3/4, 4/5 } };
           S := {2, 3, 5, 7, 11, "foo", Pi, {2/3, 3/4, 4/5}}

> type( S, set );
                                  true

> Pi in S;
          Pi  in  {2, 3, 5, 7, 11, "foo", Pi, {2/3, 3/4, 4/5}}

> if Pi in S then print( yes ) else print( no ) end:
                                  yes

> member( Pi, S );
                                  true

> if 4 in S then print( yes ) else print( no ) end:
                                   no

> evalb( { 2/3, 3/4, 4/5 } in S );
                                  true

> { a, b, c } union { 1, 2, 3 };
                           {1, 2, 3, a, b, c}

> { a, b, c } intersect { b, c, d };
                                 {b, c}

> { a, b, c } minus { b, c, d };
                                  {a}

> { a, b } subset { a, b, c };
                                  true

> { a, d } subset { a, b, c };
                                 false

> evalb( { 1, 2, 3 } = { 1, 2, 3 } );
                                  true

> evalb( { 1, 2, 3 } = { 1, 2, 4 } );
                                 false

```




## Mathematica


```Mathematica
set1 = {"a", "b", "c", "d", "e"}; set2 = {"a", "b", "c", "d", "e", "f", "g"};
MemberQ[set1, "a"]
Union[set1 , set2]
Intersection[set1 , set2]
Complement[set2, set1](*Set Difference*)
MemberQ[Subsets[set2], set1](*Subset*)
set1 == set2(*Equality*)
set1 == set1(*Equality*)
```

```txt
True
{"a", "b", "c", "d", "e", "f", "g"}
{"a", "b", "c", "d", "e"}
{"f", "g"}
True
False
True

```


=={{header|MATLAB}} / {{header|Octave}}==

There are two types of sets supported, sets with numeric values are stored in a vector, sets with string elements are stored in a cell-array.

```Matlab

    % Set creation
	s = [1, 2, 4];     % numeric values
	t = {'a','bb','ccc'}; % cell array of strings
        u = unique([1,2,3,3,2,3,2,4,1]);   % set consists only of unique elements
    % Test m ∈ S -- "m is an element in set S"
        ismember(m, S)
    % A ∪ B -- union; a set of all elements either in set A or in set B.
	union(A, B)
    % A ∩ B -- intersection; a set of all elements in both set A and set B.
	intersect(A, B)
    % A ∖ B -- difference; a set of all elements in set A, except those in set B.
	setdiff(A, B)
    % A ⊆ B -- subset; true if every element in set A is also in set B.
        all(ismember(A, B))
    % A = B -- equality; true if every element of set A is in set B and vice-versa.
        isempty(setxor(A, B))

```



## Maxima

<lang>/* illustrating some functions on sets; names are self-explanatory */

a: {1, 2, 3, 4};
{1, 2, 3, 4}

b: {2, 4, 6, 8};
{2, 4, 6, 8}

intersection(a, b);
{2, 4}

union(a, b);
{1, 2, 3, 4, 6, 8}

powerset(a);
set_partitions(a);
{{{1}, {2}, {3}, {4}}, {{1}, {2}, {3, 4}}, {{1}, {2, 3}, {4}}, {{1}, {2, 3, 4}}, {{1}, {2, 4}, {3}}, {{1, 2}, {3}, {4}},
{{1, 2}, {3, 4}}, {{1, 2, 3}, {4}}, {{1, 2, 3, 4}}, {{1, 2, 4}, {3}}, {{1, 3}, {2}, {4}}, {{1, 3}, {2, 4}}, {{1, 3, 4}, {2}},
setdifference(a, b);
{1, 3}

emptyp(a);
false

elementp(2, a);
true

cardinality(a);
4

cartesian_product(a, b);
{[1, 2], [1, 4], [1, 6], [1, 8], [2, 2], [2, 4], [2, 6], [2, 8], [3, 2], [3, 4], [3, 6], [3, 8], [4, 2], [4, 4], [4, 6], [4, 8]}

subsetp(a, b);
false

symmdifference(a, b);
{1, 3, 6, 8}

partition_set(union(a, b), evenp);
[{1, 3}, {2, 4, 6, 8}]

c: setify(makelist(fib(n), n, 1, 20));
{1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765}

equiv_classes(c, lambda([m, n], mod(m - n, 3) = 0));
disjointp(a, b);
false

adjoin(7, a);
{1, 2, 3, 4, 7}

a;
{1, 2, 3, 4}

disjoin(1, a);
{2, 3, 4}

a;
{1, 2, 3, 4}

subset(c, primep);
{2, 3, 5, 13, 89, 233, 1597}

permutations(a);
{[1, 2, 3, 4], [1, 2, 4, 3], [1, 3, 2, 4], [1, 3, 4, 2], [1, 4, 2, 3], [1, 4, 3, 2],
 [2, 1, 3, 4], [2, 1, 4, 3], [2, 3, 1, 4], [2, 3, 4, 1], [2, 4, 1, 3], [2, 4, 3, 1],
 [3, 1, 2, 4], [3, 1, 4, 2], [3, 2, 1, 4], [3, 2, 4, 1], [3, 4, 1, 2], [3, 4, 2, 1],
 [4, 1, 2, 3], [4, 1, 3, 2], [4, 2, 1, 3], [4, 2, 3, 1], [4, 3, 1, 2], [4, 3, 2, 1]}

setequalp(a, b);
false
```



## Nemerle

The <tt>Nemerle.Collections</tt> namespace provides an implementation of a Set.

```Nemerle
using System.Console;
using Nemerle.Collections;

module RCSet
{
    HasSubset[T](this super : Set[T], sub : Set[T]) : bool
    {
        super.ForAll(x => sub.Contains(x))
    }

    Main() : void
    {
        def names1 = Set(["Bob", "Billy", "Tom", "Dick", "Harry"]);
        def names2 = Set(["Bob", "Mary", "Alice", "Louisa"]);
        //def names3 = Set(["Bob", "Bob"]);        // unfortunately, duplicated elements are not well handled by the stock
                                                   // implementation, this statement would throw an ArgumentException
        def elem = names1.Contains("Bob");         // element test
        def names1u2 = names1.Sum(names2);         // union
        def names1d2 = names1.Subtract(names2);    // difference
        def names1i2 = names1.Intersect(names2);   // intersection
        def same = names1.Equals(names2);          // equality
        def sub12 = names1.HasSubset(names2);      // subset

        WriteLine($"$names1u2\n$names1d2\n$names1i2");
        WriteLine($"$same\t$sub12");
    }
}
```



## Nim


```nim
var # creation
  s = {0,3,5,10}
  t = {3..20, 50..55}

if 5 in s: echo "5 is in!" # element test

var
  c = s + t # union
  d = s * t # intersection
  e = s - t # difference

if s <= t: echo "s ⊆ t" # subset

if s <= t: echo "s ⊂ t" # strong subset

if s == t: echo "s = s" # equality

s.incl(4) # add 4 to set
s.excl(5) # remove 5 from set
```


=={{header|Objective-C}}==

```objc
#import <Foundation/Foundation.h>


int main (int argc, const char *argv[]) {
  @autoreleasepool {

    NSSet *s1 = [NSSet setWithObjects:@"a", @"b", @"c", @"d", @"e", nil];
    NSSet *s2 = [NSSet setWithObjects:@"b", @"c", @"d", @"e", @"f", @"h", nil];
    NSSet *s3 = [NSSet setWithObjects:@"b", @"c", @"d", nil];
    NSSet *s4 = [NSSet setWithObjects:@"b", @"c", @"d", nil];
    NSLog(@"s1: %@", s1);
    NSLog(@"s2: %@", s2);
    NSLog(@"s3: %@", s3);
    NSLog(@"s4: %@", s4);

    // Membership
    NSLog(@"b in s1: %d", [s1 containsObject:@"b"]);
    NSLog(@"f in s1: %d", [s1 containsObject:@"f"]);

    // Union
    NSMutableSet *s12 = [NSMutableSet setWithSet:s1];
    [s12 unionSet:s2];
    NSLog(@"s1 union s2: %@", s12);

    // Intersection
    NSMutableSet *s1i2 = [NSMutableSet setWithSet:s1];
    [s1i2 intersectSet:s2];
    NSLog(@"s1 intersect s2: %@", s1i2);

    // Difference
    NSMutableSet *s1_2 = [NSMutableSet setWithSet:s1];
    [s1_2 minusSet:s2];
    NSLog(@"s1 - s2: %@", s1_2);

    // Subset of
    NSLog(@"s3 subset of s1: %d", [s3 isSubsetOfSet:s1]);

    // Equality
    NSLog(@"s3 = s4: %d", [s3 isEqualToSet:s4]);

    // Cardinality
    NSLog(@"size of s1: %lu", [s1 count]);

    // Has intersection (not disjoint)
    NSLog(@"does s1 intersect s2? %d", [s1 intersectsSet:s2]);

    // Adding and removing elements from a mutable set
    NSMutableSet *mut_s1 = [NSMutableSet setWithSet:s1];
    [mut_s1 addObject:@"g"];
    NSLog(@"mut_s1 after adding g: %@", mut_s1);
    [mut_s1 addObject:@"b"];
    NSLog(@"mut_s1 after adding b again: %@", mut_s1);
    [mut_s1 removeObject:@"c"];
    NSLog(@"mut_s1 after removing c: %@", mut_s1);

  }
  return 0;
}
```

```txt

s1: {(
    d,
    b,
    e,
    c,
    a
)}
s2: {(
    d,
    b,
    e,
    c,
    h,
    f
)}
s3: {(
    b,
    c,
    d
)}
s4: {(
    b,
    c,
    d
)}
b in s1: 1
f in s1: 0
s1 union s2: {(
    c,
    h,
    d,
    e,
    a,
    f,
    b
)}
s1 intersect s2: {(
    d,
    b,
    e,
    c
)}
s1 - s2: {(
    a
)}
s3 subset of s1: 1
s3 = s4: 1
size of s1: 5
does s1 intersect s2? 1
mut_s1 after adding g: {(
    d,
    b,
    g,
    e,
    c,
    a
)}
mut_s1 after adding b again: {(
    d,
    b,
    g,
    e,
    c,
    a
)}
mut_s1 after removing c: {(
    d,
    b,
    g,
    e,
    a
)}

```



## OCaml

OCaml offers a functional, persistent set data structure in its <code>Set</code> module. It is implemented using a binary search tree. <code>Set</code> works in the functor model, which means you need to first use the <code>Set.Make</code> functor to create a module for your kind of set that you can use. You must give the functor an argument that is a module containing the type and ordering function.

In the interactive toplevel:
```ocaml
# module IntSet = Set.Make(struct type t = int let compare = compare end);; (* Create a module for our type of set *)
module IntSet :
  sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end
# IntSet.empty;; (* Empty set. A set is an abstract type that will not display in the interpreter *)
- : IntSet.t = <abstr>
# IntSet.elements (IntSet.empty);; (* Get the previous set into a list *)
- : IntSet.elt list = []
# let s1 = IntSet.of_list [1;2;3;4;3];;
val s1 : IntSet.t = <abstr>
# IntSet.elements s1;;
- : IntSet.elt list = [1; 2; 3; 4]
# let s2 = IntSet.of_list [3;4;5;6];;
val s2 : IntSet.t = <abstr>
# IntSet.elements s2;;
- : IntSet.elt list = [3; 4; 5; 6]
# IntSet.elements (IntSet.union s1 s2);; (* Union *)
- : IntSet.elt list = [1; 2; 3; 4; 5; 6]
# IntSet.elements (IntSet.inter s1 s2);; (* Intersection *)
- : IntSet.elt list = [3; 4]
# IntSet.elements (IntSet.diff s1 s2);; (* Difference *)
- : IntSet.elt list = [1; 2]
# IntSet.subset s1 s1;; (* Subset *)
- : bool = true
# IntSet.subset (IntSet.of_list [3;1]) s1;;
- : bool = true
# IntSet.equal (IntSet.of_list [3;2;4;1]) s1;; (* Equality *)
- : bool = true
# IntSet.equal s1 s2;;
- : bool = false
# IntSet.mem 2 s1;; (* Membership *)
- : bool = true
# IntSet.mem 10 s1;;
- : bool = false
# IntSet.cardinal s1;; (* Cardinality *)
- : int = 4
# IntSet.elements (IntSet.add 99 s1);; (* Create a new set by inserting *)
- : IntSet.elt list = [1; 2; 3; 4; 99]
# IntSet.elements (IntSet.remove 3 s1);; (* Create a new set by deleting *)
- : IntSet.elt list = [1; 2; 4]
```


(Note: <code>of_list</code> is only available in OCaml 4.02+. In earlier versions, you can implement one yourself like
<code ocaml>let of_list lst = List.fold_right IntSet.add lst IntSet.empty;;</code>)

Regular lists can also be used as sets.

In addition, you can use imperative hash tables from the <code>Hashtbl</code> module as a hash table-based set, using the unit type as the "value" for each key.


## Ol


```scheme

; test set
(define set1 '(1 2 3 4 5 6 7 8 9))
(define set2 '(3 4 5 11 12 13 14))
(define set3 '(4 5 6 7))
(define set4 '(1 2 3 4 5 6 7 8 9))

; union
(print (union set1 set2))
; ==> (1 2 6 7 8 9 3 4 5 11 12 13 14)

; intersection
(print (intersect set1 set2))
; ==> (3 4 5)

; difference
(print (diff set1 set2))
; ==> (1 2 6 7 8 9)

; subset (no predefined function)
(define (subset? a b)
   (all (lambda (i) (has? b i)) a))
(print (subset? set3 set1))
; ==> #true
(print (subset? set3 set2))
; ==> #false

; equality
(print (equal? set1 set2))
; ==> #false
(print (equal? set1 set4))
; ==> #true

```



## ooRexx


```ooRexx
-- Set creation
-- Using the OF method
s1 = .set~of(1, 2, 3, 4, 5, 6)
-- Explicit addition of individual items
s2 = .set~new
s2~put(2)
s2~put(4)
s2~put(6)
-- group addition
s3 = .set~new
s3~putall(.array~of(1, 3, 5))
-- Test m ? S -- "m is an element in set S"
say s1~hasindex(1) s3~hasindex(2)  -- "1 0", which is "true" and "false"
--    A ? B -- union; a set of all elements either in set A or in set B.
s4 = s2~union(s3)   -- {1, 2, 3, 4, 5, 6}
Call show 's4',s4
--    A ? B -- intersection; a set of all elements in both set A and set B.
s5 = s1~intersection(s2)   -- {2, 4, 6}
Call show 's5',s5
--    A ? B -- difference; a set of all elements in set A, except those in set B.
s6 = s1~difference(s2)   -- {1, 3, 5}
Call show 's6',s6
--    A ? B -- subset; true if every element in set A is also in set B.
say s1~subset(s2) s2~subset(s1) --  "0 1"
--    A = B -- equality; true if every element of set A is in set B and vice-versa.
-- No direct equivalence method, but the XOR method can be used to determine this
say s1~xor(s4)~isempty   -- true
Exit
show: Procedure
  Use Arg set_name,set
  Say set_name':' set~makearray~makestring((LINE),',')
  return
```

The set operators don't come out too well :-(
```txt
1 0
s4: 1,2,3,4,5,6
s5: 2,4,6
s6: 1,3,5
0 1
1
```



## PARI/GP

Aside from ⊆, all operations are already a part of GP.

```parigp
setsubset(s,t)={
  for(i=1,#s,
    if(!setsearch(t,s[i]), return(0))
  );
  1
};
s=Set([1,2,2])
t=Set([4,2,4])
setsearch(s,1)
setunion(s,t)
setintersect(s,t)
setminus(s,t)
setsubset(s,t)
s==t
```


```txt
%1 = [1, 2]
%2 = [2, 4]
%3 = 1
%4 = [1, 2, 4]
%5 = [2]
%6 = [1]
%7 = 0
%8 = 0
```



## Pascal

Freepascal/object pascal handles sets very well.
--[[User:Guionardo|Guionardo]] 22:03, 7 January 2012 (UTC)


```pascal
program Rosetta_Set;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes;

{$R *.res}
type
  CharSet = set of char;

var
  A, B, C, S: CharSet;
  M: char;

  function SetToString(const ASet: CharSet): string;
  var
    J: char;
  begin
    Result := '';
    // Test all chars
    for J in char do
      // If the char is in set, add to result
      if J in ASet then
        Result := Result + J + ', ';
    // Clear the result
    if Result > '' then
      Delete(Result, Length(Result) - 1, 2);
  end;

  procedure PrintSet(const ASet: CharSet; const ASetName: string;
  const ATitle: string = '');
  begin
    if ATitle > '' then
      WriteLn(ATitle);
    WriteLn(ASetName, ' = [', SetToString(ASet), ']', #10);
  end;

  procedure ShowEqual(const ASetA, ASetB: CharSet; const ASetNameA, ASetNameB: string);
  begin
    WriteLn(ASetNameA, ' = [', SetToString(ASetA), ']');
    WriteLn(ASetNameB, ' = [', SetToString(ASetB), ']');
    if ASetA = ASetB then
      WriteLn(ASetNameA, ' = ', ASetNameB)
    else
      WriteLn(ASetNameA, ' <> ', ASetNameB);
  end;


begin
  // Set Creation
  A := ['A', 'B', 'C', 'D', 'E', 'F'];
  B := ['E', 'F', 'G', 'H', 'I', 'J'];
  PrintSet(A, 'A', 'Set Creation');
  PrintSet(B, 'B');

  // Test m ∈ S -- "m is an element in set S"
  M := 'A';
  if M in A then
    WriteLn('"A" is in set A');

  // A ∪ B -- union; a set of all elements either in set A or in set B.
  S := A + B;
  PrintSet(S, 'S', 'S = A U B -- union; a set of all elements either in set A or in set B.');

  // A ∩ B -- intersection; a set of all elements in both set A and set B.
  S := A * B;
  PrintSet(S, 'S',
    'S = A ∩ B -- intersection; a set of all elements in both set A and set B.');

  // A \ B -- difference; a set of all elements in set A, except those in set B.
  S := A - B;
  PrintSet(S, 'S',
    'S = A \ B -- difference; a set of all elements in set A, except those in set B.');

  // A ⊆ B -- subset; true if every element in set A is also in set B.
  Writeln('A ⊆ B -- subset; true if every element in set A is also in set B.');
  if A <= B then
    WriteLn('A in B')
  else
    Writeln('A is not in B');
  Writeln;
  //A = B -- equality; true if every element of set A is in set B and vice-versa.
  Writeln('A = B -- equality; true if every element of set A is in set B and vice-versa.');

  ShowEqual(A, B, 'A', 'B');
  S := A * B;
  C := ['E', 'F'];
  ShowEqual(S, C, 'S', 'C');

  readln;

end.
```



## Perl

For real code, try Set::Object from CPAN. Here we provide a primitive implementation using hashes.

```perl
use strict;

package Set; # likely will conflict with stuff on CPAN
use overload
	'""'	=> \&str,
	'bool'	=> \&count,
	'+='	=> \&add,
	'-='	=> \&del,
	'-'	=> \&diff,
	'=='	=> \&eq,
	'&'	=> \&intersection,
	'|'	=> \&union,
	'^'	=> \&xdiff;

sub str {
	my $set = shift;
	# This has drawbacks: stringification is used as set key
	# if the set is added to another set as an element, which
	# may cause inconsistencies if the element set is modified
	# later.  In general, a hash key loses its object identity
	# anyway, so it's not unique to us.
	"Set{ ".  join(", " => sort map("$_", values %$set)) . " }"
}

sub new {
	my $pkg = shift;
	my $h = bless {};
	$h->add($_) for @_;
	$h
}

sub add {
	my ($set, $elem) = @_;
	$set->{$elem} = $elem;
	$set
}

sub del {
	my ($set, $elem) = @_;
	delete $set->{$elem};
	$set
}

sub has { # set has element
	my ($set, $elem) = @_;
	exists $set->{$elem}
}

sub union {
	my ($this, $that) = @_;
	bless { %$this, %$that }
}

sub intersection {
	my ($this, $that) = @_;
	my $s = new Set;
	for (keys %$this) {
		$s->{$_} = $this->{$_} if exists $that->{$_}
	}
	$s
}

sub diff {
	my ($this, $that) = @_;
	my $s = Set->new;
	for (keys %$this) {
		$s += $this->{$_} unless exists $that->{$_}
	}
	$s
}

sub xdiff { # xor, symmetric diff
	my ($this, $that) = @_;
	my $s = new Set;
	bless { %{ ($this - $that) | ($that - $this) } }
}

sub count { scalar(keys %{+shift}) }

sub eq {
	my ($this, $that) = @_;
	!($this - $that) && !($that - $this);
}

sub contains { # this is a superset of that
	my ($this, $that) = @_;
	for (keys %$that) {
		return 0 unless $this->has($_)
	}
	return 1
}

package main;
my ($x, $y, $z, $w);

$x = Set->new(1, 2, 3);
$x += $_ for (5 .. 7);
$y = Set->new(1, 2, 4, $x); # not the brightest idea

print "set x is: $x\nset y is: $y\n";
for (1 .. 4, $x) {
	print "$_ is", $y->has($_) ? "" : " not", " in y\n";
}

print "union: ", $x | $y, "\n";
print "intersect: ", $x & $y, "\n";
print "z = x - y = ", $z = $x - $y, "\n";
print "y is ", $x->contains($y) ? "" : "not ", "a subset of x\n";
print "z is ", $x->contains($z) ? "" : "not ", "a subset of x\n";
print "z = (x | y) - (x & y) = ", $z = ($x | $y) - ($x & $y), "\n";
print "w = x ^ y = ", $w = ($x ^ $y), "\n";
print "w is ", ($w == $z) ? "" : "not ", "equal to z\n";
print "w is ", ($w == $x) ? "" : "not ", "equal to x\n";
```



## Perl 6


```perl6
use Test;

my $a = set <a b c>;
my $b = set <b c d>;
my $c = set <a b c d e>;

ok 'c' ∈ $a, "c is an element in set A";
nok 'd' ∈ $a, "d is not an element in set A";

is-deeply $a ∪ $b, set(<a b c d>), "union; a set of all elements either in set A or in set B";
is-deeply $a ∩ $b, set(<b c>), "intersection; a set of all elements in both set A and set B";
is $a (-) $b, set(<a>), "difference; a set of all elements in set A, except those in set B";

ok $a ⊆ $c, "subset; true if every element in set A is also in set B";
nok $c ⊆ $a, "subset; false if every element in set A is not also in set B";
ok $a ⊂ $c, "strict subset; true if every element in set A is also in set B";
nok $a ⊂ $a, "strict subset; false for equal sets";
ok $a === set(<a b c>), "equality; true if every element of set A is in set B and vice-versa";
nok $a === $b, "equality; false for differing sets";
```

```txt
ok 1 - c is an element in set A
ok 2 - d is not an element in set A
ok 3 - union; a set of all elements either in set A or in set B
ok 4 - intersection; a set of all elements in both set A and set B
ok 5 - difference; a set of all elements in set A, except those in set B
ok 6 - subset; true if every element in set A is also in set B
ok 7 - subset; false if every element in set A is not also in set B
ok 8 - strict subset; true if every element in set A is also in set B
ok 9 - strict subset; false for equal sets
ok 10 - equality; true if every element of set A is in set B and vice-versa
ok 11 - equality; false for differing sets
```



## Phix

First, a simple implementaion using native sequences:

```Phix
sequence set1 = {1,2,3},
         set2 = {3,4,5}

function element(object x, sequence set)
    return find(x,set)!=0
end function

function union(sequence set1, sequence set2)
    for i=1 to length(set2) do
        if not element(set2[i],set1) then
            set1 = append(set1,set2[i])
        end if
    end for
    return set1
end function

function intersection(sequence set1, sequence set2)
sequence res = {}
    for i=1 to length(set1) do
        if element(set1[i],set2) then
            res = append(res,set1[i])
        end if
    end for
    return res
end function

function difference(sequence set1, sequence set2)
sequence res = {}
    for i=1 to length(set1) do
        if not element(set1[i],set2) then
            res = append(res,set1[i])
        end if
    end for
    return res
end function

function subset(sequence set1, sequence set2)
    for i=1 to length(set1) do
        if not element(set1[i],set2) then
            return false
        end if
    end for
    return true
end function

function equality(sequence set1, sequence set2)
    if length(set1)!=length(set2) then
        return false
    end if
    return subset(set1,set2)
end function

--test code:
?element(3,set1)        -- 1
?element(4,set1)        -- 0
?union(set1,set2)       -- {1,2,3,4,5}
?intersection(set1,set2) -- {3}
?difference(set1,set2)  -- {1,2}
?subset(set1,set2)      -- 0
?subset({1,2},set1)     -- 1
?equality(set1,set2)    -- 0
?equality(set1,{3,1,2}) -- 1
```

```txt

1
0
{1,2,3,4,5}
{3}
{1,2}
0
1
0
1

```

Alternative using dictionaries, which needs several additional visitor routines (at a pinch they could be merged),
but performance is better on larger sets:

```Phix
integer set1 = new_dict(),
        set2 = new_dict()
setd(3,0,set1)
setd(1,0,set1)
setd(2,0,set1)
setd(5,0,set2)
setd(3,0,set2)
setd(4,0,set2)

function element(object x, integer set)
    return getd_index(x,set)!=0
end function

function u_visitor(object key, object data, object user_data)
    integer {union_set,set2} = user_data
    if set2=0
    or not element(key,union_set) then
        setd(key,data,union_set)
    end if
    return 1
end function

function union(integer set1, integer set2)
integer union_set = new_dict()
    traverse_dict(routine_id("u_visitor"),{union_set,0},set1)
    traverse_dict(routine_id("u_visitor"),{union_set,set2},set2)
    return union_set
end function

function i_visitor(object key, object data, object user_data)
    integer {union_set,set2} = user_data
    if element(key,set2) then
        setd(key,data,union_set)
    end if
    return 1
end function

function intersection(integer set1, integer set2)
integer union_set = new_dict()
    traverse_dict(routine_id("i_visitor"),{union_set,set2},set1)
    return union_set
end function

function d_visitor(object key, object data, object user_data)
    integer {union_set,set2} = user_data
    if not element(key,set2) then
        setd(key,data,union_set)
    end if
    return 1
end function

function difference(integer set1, integer set2)
integer union_set = new_dict()
    traverse_dict(routine_id("d_visitor"),{union_set,set2},set1)
    return union_set
end function

integer res
function s_visitor(object key, object data, object user_data)
    integer set2 = user_data
    if not element(key,set2) then
        res = 0
        return 0 -- cease traversal
    end if
    return 1
end function

function subset(integer set1, integer set2)
    res = 1
    traverse_dict(routine_id("s_visitor"),set2,set1)
    return res
end function

function equality(integer set1, integer set2)
    if dict_size(set1)!=dict_size(set2) then
        return false
    end if
    return subset(set1,set2)
end function

include builtins/map.e -- for keys()

-- matching test code:
?element(3,set1)        -- 1
?element(4,set1)        -- 0
?keys(union(set1,set2)) -- {1,2,3,4,5}
?keys(intersection(set1,set2)) -- {3}
?keys(difference(set1,set2)) -- {1,2}
?subset(set1,set2)      -- 0
integer set3 = new_dict()
setd(2,0,set3)
setd(1,0,set3)
?subset(set3,set1)      -- 1
?equality(set1,set2)    -- 0
setd(3,0,set3)
?equality(set1,set3)    -- 1
```

same output as above


## PicoLisp

We may use plain lists, or '[http://software-lab.de/doc/refI.html#idx idx]' structures for sets. A set may contain any type of data.

### Using lists


```PicoLisp
(setq
   Set1 (1 2 3 7 abc "def" (u v w))
   Set2 (2 3 5 hello (x y z))
   Set3 (3 hello (x y z)) )


# Element tests (any non-NIL value means "yes")
: (member "def" Set1)
-> ("def" (u v w))

: (member "def" Set2)
-> NIL

: (member '(x y z) Set2)
-> ((x y z))


# Union
: (uniq (append Set1 Set2))
-> (1 2 3 7 abc "def" (u v w) 5 hello (x y z))


# Intersection
: (sect Set1 Set2)
-> (2 3)


# Difference
: (diff Set1 Set2)
-> (1 7 abc "def" (u v w))


# Test for subset
: (not (diff Set1 Set2))
-> NIL  # Set1 is not a subset of Set2

: (not (diff Set3 Set2))
-> T  # Set3 is a subset of Set2


# Test for equality
: (= (sort (copy Set1)) (sort (copy Set2)))
-> NIL

: (= (sort (copy Set2)) (sort (copy Set2)))
-> T
```

===Using 'idx' structures===

```PicoLisp
# Create three test-sets
(balance 'Set1 (1 2 3 7 abc "def" (u v w)))
(balance 'Set2 (2 3 5 hello (x y z)))
(balance 'Set3 (3 hello (x y z)))


# Get contents
: (idx 'Set1)
-> (1 2 3 7 abc "def" (u v w))

: (idx 'Set2)
-> (2 3 5 hello (x y z))


# Element tests (any non-NIL value means "yes")
: (idx 'Set1 "def")
-> ("def" (abc) (u v w))

: (idx 'Set2 "def")
-> NIL

: (idx 'Set2 '(x y z))
-> ((x y z))


# Union
: (use S
   (balance 'S (idx 'Set1))
   (balance 'S (idx 'Set2) T)
   (idx 'S) )
-> (1 2 3 5 7 abc "def" hello (u v w) (x y z))


# Intersection
: (sect (idx 'Set1) (idx 'Set2))
-> (2 3)


# Difference
: (diff (idx 'Set1) (idx 'Set2))
-> (1 7 abc "def" (u v w))


# Test for subset
: (not (diff (idx 'Set1) (idx 'Set2)))
-> NIL  # Set1 is not a subset of Set2

: (not (diff (idx 'Set3) (idx 'Set2)))
-> T  # Set3 is a subset of Set2


# Test for equality
: (= (idx 'Set1) (idx 'Set2))
-> NIL

: (= (idx 'Set2) (idx 'Set2))
-> T
```



## PowerShell

.NET offers the '''HashSet''' type which seems to act in most ways like a set.

When used in PowerShell, the syntax is clumsy.  In addition, the "reference" set (<code>$set1</code>) is modified in place to become the result.
(All examples assume the variable <code>$set1</code> contains the value <code>@(1,2,3,4)</code>)

```PowerShell

[System.Collections.Generic.HashSet[object]]$set1 = 1..4
[System.Collections.Generic.HashSet[object]]$set2 = 3..6

#            Operation           +     Definition      +          Result
#--------------------------------+---------------------+-------------------------
$set1.UnionWith($set2)           # Union                 $set1 = 1, 2, 3, 4, 5, 6
$set1.IntersectWith($set2)       # Intersection          $set1 = 3, 4
$set1.ExceptWith($set2)          # Difference            $set1 = 1, 2
$set1.SymmetricExceptWith($set2) # Symmetric difference  $set1 = 1, 2, 6, 5
$set1.IsSupersetOf($set2)        # Test superset         False
$set1.IsSubsetOf($set2)          # Test subset           False
$set1.Equals($set2)              # Test equality         False
$set1.IsProperSupersetOf($set2)  # Test proper superset  False
$set1.IsProperSubsetOf($set2)    # Test proper subset    False

5 -in $set1                      # Test membership       False
7 -notin $set1                   # Test non-membership   True

```



## Prolog

Works with SWI-Prolog, library(lists).

```Prolog
:- use_module(library(lists)).

set :-
	A = [2, 4, 1, 3],
	B = [5, 2, 3, 2],
	(   is_set(A) -> format('~w is a set~n', [A])
	;   format('~w is not a set~n', [A])),
	(   is_set(B) -> format('~w is a set~n', [B])
	;   format('~w is not a set~n', [B])),

	% create a set from a list

	list_to_set(B, BS),
	(   is_set(BS) -> format('~nCreate a set from a list~n~w is a set~n', [BS])
	;   format('~w is not a set~n', [BS])),

	intersection(A, BS, I),
	format('~n~w intersection ~w => ~w~n', [A, BS, I]),
	union(A, BS, U),
	format('~w union ~w => ~w~n', [A, BS, U]),
	difference(A, BS, D),
	format('~w difference ~w => ~w~n', [A, BS, D]),

	X = [1,2],
	(   subset(X, A) -> format('~n~w is a subset of ~w~n', [X, A])
	;   format('~w is not a subset of ~w~n', [X, A])),
	Y = [1,5],
	(   subset(Y, A) -> format('~w is a subset of ~w~n', [Y, A])
	;   format('~w is not a subset of ~w~n', [Y, A])),
	Z = [1, 2, 3, 4],
	(  equal(Z, A) -> format('~n~w is equal to ~w~n', [Z, A])
	;   format('~w is not equal to ~w~n', [Z, A])),
	T = [1, 2, 3],
	(  equal(T, A) -> format('~w is equal to ~w~n', [T, A])
	;   format('~w is not equal to ~w~n', [T, A])).



% compute difference of sets
difference(A, B, D) :-
	exclude(member_(B), A, D).

member_(L, X) :-
	member(X, L).

equal([], []).
equal([H1 | T1], B) :-
	select(H1, B, B1),
	equal(T1, B1).

```

```txt
 ?- set.
[2,4,1,3] is a set
[5,2,3,2] is not a set

Create a set from a list
[5,2,3] is a set

[2,4,1,3] intersection [5,2,3] => [2,3]
[2,4,1,3] union [5,2,3] => [4,1,5,2,3]
[2,4,1,3] difference [5,2,3] => [4,1]

[1,2] is a subset of [2,4,1,3]
[1,5] is not a subset of [2,4,1,3]

[1,2,3,4] is equal to [2,4,1,3]
[1,2,3] is not equal to [2,4,1,3]
true.

```



SWI-Prolog provides a standard [http://www.swi-prolog.org/pldoc/man?section=ordsets:Link library(ordsets)].
It is loaded by default. I demonstrate almost all of these predicates in the interactive top-level (`?-` is the prompt).
Variables prefixed with `$` refer back to the value of the last instantiation.
It treats sets as ordered lists of unique elements:


```prolog

%%  Set creation

?- list_to_ord_set([1,2,3,4], A), list_to_ord_set([2,4,6,8], B).
A = [1, 2, 3, 4],
B = [2, 4, 6, 8].

%% Test m ∈ S -- "m is an element in set S"

?- ord_memberchk(2, $A).
true.

%% A ∪ B -- union; a set of all elements either in set A or in set B.

?- ord_union($A, $B, Union).
Union = [1, 2, 3, 4, 6, 8].

%% A ∩ B -- intersection; a set of all elements in both set A and set B.

?- ord_intersection($A, $B, Intersection).
Intersection = [2, 4].

%% A ∖ B -- difference; a set of all elements in set A, except those in set B.

?- ord_subtract($A, $B, Diff).
Diff = [1, 3].

%% A ⊆ B -- subset; true if every element in set A is also in set B.

?- ord_subset($A, $B).
false.

?- ord_subset([2,4], $B).
true.

%% A = B -- equality; true if every element of set A is in set B and vice-versa.

?- $A == $B.
false.

?- $A == [1,2,3,4].
true.

%% Definition of a proper subset:

ord_propsubset(A, B) :-
    ord_subset(A, B),
    \+(A == B).

%% add/remove elements

?- ord_add_element($A, 19, NewA).
NewA = [1, 2, 3, 4, 19].

?- ord_del_element($NewA, 3, NewerA).
NewerA = [1, 2, 4, 19].

```



## PureBasic

This solution uses PureBasic's maps (hash tables).

```purebasic
Procedure.s booleanText(b) ;returns 'True' or 'False' for a boolean input
  If b: ProcedureReturn "True": EndIf
  ProcedureReturn "False"
EndProcedure

Procedure.s listSetElements(Map a(), delimeter.s = " ") ;format elements for display
  Protected output$

  ForEach a()
    output$ + MapKey(a()) + delimeter
  Next

  ProcedureReturn "(" + RTrim(output$, delimeter) + ")"
EndProcedure

Procedure.s listSortedSetElements(Map a(), delimeter.s = " ") ;format elements for display as sorted for easy comparison
  Protected output$
  NewList b.s()

  ForEach a()
    AddElement(b()): b() = MapKey(a())
  Next
  SortList(b(), #PB_Sort_Ascending | #PB_Sort_NoCase)
  ForEach b()
    output$ + b() + delimeter
  Next

  ProcedureReturn "(" + RTrim(output$, delimeter) + ")"
EndProcedure

Procedure cardinalityOf(Map a())
  ProcedureReturn MapSize(a())
EndProcedure

Procedure createSet(elements.s, Map o(), delimeter.s = " ", clearSet = 1)
  Protected i, elementCount

  If clearSet: ClearMap(o()): EndIf
  elementCount = CountString(elements, delimeter) + 1 ;add one for the last element which won't have a delimeter
  For i = 1 To elementCount
    AddMapElement(o(), StringField(elements, i, delimeter))
  Next

  ProcedureReturn MapSize(o())
EndProcedure

Procedure adjoinTo(elements.s, Map o(), delimeter.s = " ")
  ProcedureReturn createSet(elements, o(), delimeter, 0)
EndProcedure

Procedure disjoinFrom(elements.s, Map o(), delimeter.s = " ")
  Protected i, elementCount

  elementCount = CountString(elements, delimeter) + 1 ;add one for the last element which won't have a delimeter
  For i = 1 To elementCount
    DeleteMapElement(o(), StringField(elements, i, delimeter))
  Next

  ProcedureReturn MapSize(o())
EndProcedure

Procedure isElementOf(element.s, Map a())
  ProcedureReturn FindMapElement(a(), element)
EndProcedure



Procedure unionOf(Map a(), Map b(), Map o())
  CopyMap(a(), o())
  ForEach b()
    AddMapElement(o(), MapKey(b()))
  Next

  ProcedureReturn MapSize(o())
EndProcedure

Procedure intersectionOf(Map a(), Map b(), Map o())
  ClearMap(o())
  ForEach a()
    If FindMapElement(b(), MapKey(a()))
      AddMapElement(o(), MapKey(a()))
    EndIf
  Next

  ProcedureReturn MapSize(o())
EndProcedure

Procedure differenceOf(Map a(), Map b(), Map o())
  CopyMap(a(), o())
  ForEach b()
    If FindMapElement(o(), MapKey(b()))
      DeleteMapElement(o())
    Else
      AddMapElement(o(), MapKey(b()))
    EndIf
  Next

  ProcedureReturn MapSize(o())
EndProcedure

Procedure isSubsetOf(Map a(), Map b()) ;boolean
  ForEach a()
    If Not FindMapElement(b(), MapKey(a()))
      ProcedureReturn 0
    EndIf
  Next
  ProcedureReturn 1
EndProcedure

Procedure isProperSubsetOf(Map a(), Map b()) ;boolean
  If MapSize(a()) = MapSize(b())
    ProcedureReturn 0
  EndIf
  ProcedureReturn isSubsetOf(a(), b())
EndProcedure

Procedure isEqualTo(Map a(), Map b())
  If MapSize(a()) = MapSize(b())
    ProcedureReturn isSubsetOf(a(), b())
  EndIf
  ProcedureReturn 0
EndProcedure

Procedure isEmpty(Map a()) ;boolean
  If MapSize(a())
    ProcedureReturn 0
  EndIf
  ProcedureReturn 1
EndProcedure

If OpenConsole()
  NewMap a()
  NewMap b()
  NewMap o() ;for output sets
  NewMap c()

  createSet("red blue green orange yellow", a())
  PrintN("Set A = " + listSortedSetElements(a()) + " of cardinality " + Str(cardinalityOf(a())) + ".")
  createSet("lady green red", b())
  PrintN("Set B = " + listSortedSetElements(b()) + " of cardinality " + Str(cardinalityOf(b())) + ".")
  PrintN("'red' is an element of A is " + booleanText(isElementOf("red", a())) + ".")
  PrintN("'red' is an element of B is " + booleanText(isElementOf("red", b())) + ".")
  PrintN("'blue' is an element of B is " + booleanText(isElementOf("blue", b())) + ".")

  unionOf(a(), b(), o())
  PrintN(#crlf$ + "Union of A & B is " + listSortedSetElements(o()) + ".")
  intersectionOf(a(), b(), o())
  PrintN("Intersection of  A & B is " + listSortedSetElements(o()) + ".")
  differenceOf(a(), b(), o())
  PrintN("Difference of  A & B is " + listSortedSetElements(o()) + ".")

  PrintN(listSortedSetElements(a()) + " equals " + listSortedSetElements(a()) + " is " + booleanText(isEqualTo(a(), a())) + ".")
  PrintN(listSortedSetElements(a()) + " equals " + listSortedSetElements(b()) + " is " + booleanText(isEqualTo(a(), b())) + ".")

  createSet("red green", c())
  PrintN(#crlf$ + listSortedSetElements(c()) + " is a subset of " + listSortedSetElements(a()) + " is "+ booleanText(isSubsetOf(c(), a())) + ".")
  PrintN(listSortedSetElements(c()) + " is a proper subset of " + listSortedSetElements(b()) + " is "+ booleanText(isProperSubsetOf(c(), b())) + ".")
  PrintN(listSortedSetElements(c()) + " is a proper subset of " + listSortedSetElements(a()) + " is "+ booleanText(isProperSubsetOf(c(), a())) + ".")
  PrintN(listSortedSetElements(b()) + " is a proper subset of " + listSortedSetElements(b()) + " is "+ booleanText(isProperSubsetOf(b(), b())) + ".")

  PrintN(#crlf$ + "Set C = " + listSortedSetElements(c()) + " of cardinality " + Str(cardinalityOf(c())) + ".")
  adjoinTo("dog cat mouse", c())
  PrintN("Add 'dog cat mouse' to C to get " + listSortedSetElements(c()) + " of cardinality " + Str(cardinalityOf(c())) + ".")
  disjoinFrom("red green dog", c())
  PrintN("Take away 'red green dog' from C to get " + listSortedSetElements(c()) + " of cardinality " + Str(cardinalityOf(c())) + ".")


  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

```txt
Set A = (blue green orange red yellow) of cardinality 5.
Set B = (green lady red) of cardinality 3.
'red' is an element of A is True.
'red' is an element of B is True.
'blue' is an element of B is False.

Union of A & B is (blue green lady orange red yellow).
Intersection of  A & B is (green red).
Difference of  A & B is (blue lady orange yellow).
(blue green orange red yellow) equals (blue green orange red yellow) is True.
(blue green orange red yellow) equals (green lady red) is False.

(green red) is a subset of (blue green orange red yellow) is True.
(green red) is a proper subset of (green lady red) is True.
(green red) is a proper subset of (blue green orange red yellow) is True.
(green lady red) is a proper subset of (green lady red) is False.

Set C = (green red) of cardinality 2.
Add 'dog cat mouse' to C to get (cat dog green mouse red) of cardinality 5.
Take away 'red green dog' from C to get (cat mouse) of cardinality 2.
```



## Python

In Python, <code>[http://docs.python.org/py3k/library/stdtypes.html#set-types-set-frozenset set]</code> is a standard type since Python 2.4.
There is also <code>frozenset</code> which is an immutable version of <code>set</code>. (In Python 2.3, they were provided as <code>Set</code> and <code>ImmutableSet</code> types in the module <code>sets</code>.)

Language syntax for set literals is supported starting in Python 3.0 and 2.7.
(For versions prior to 2.7, use <code>set([1, 2, 3, 4])</code> instead of <code>{1, 2, 3, 4}</code>. Even in Python 2.7+ and 3.0+, it is necessary to write <code>set()</code> to express the empty set.)
```python
>>>
 s1, s2 = {1, 2, 3, 4}, {3, 4, 5, 6}
>>> s1 | s2 # Union
{1, 2, 3, 4, 5, 6}
>>> s1 & s2 # Intersection
{3, 4}
>>> s1 - s2 # Difference
{1, 2}
>>> s1 < s1 # True subset
False
>>> {3, 1} < s1 # True subset
True
>>> s1 <= s1 # Subset
True
>>> {3, 1} <= s1 # Subset
True
>>> {3, 2, 4, 1} == s1 # Equality
True
>>> s1 == s2 # Equality
False
>>> 2 in s1 # Membership
True
>>> 10 not in s1 # Non-membership
True
>>> {1, 2, 3, 4, 5} > s1 # True superset
True
>>> {1, 2, 3, 4} > s1 # True superset
False
>>> {1, 2, 3, 4} >= s1 # Superset
True
>>> s1 ^ s2 # Symmetric difference
{1, 2, 5, 6}
>>> len(s1) # Cardinality
4
>>> s1.add(99) # Mutability
>>> s1
{99, 1, 2, 3, 4}
>>> s1.discard(99) # Mutability
>>> s1
{1, 2, 3, 4}
>>> s1 |= s2 # Mutability
>>> s1
{1, 2, 3, 4, 5, 6}
>>> s1 -= s2 # Mutability
>>> s1
{1, 2}
>>> s1 ^= s2 # Mutability
>>> s1
{1, 2, 3, 4, 5, 6}
>>>
```



## Racket



```racket

#lang racket

(define A (set 1 2 3 4))
(define B (set 3 4 5 6))
(define C (set 4 5))

(set-union A B)     ; gives (set 1 2 3 4 5 6)
(set-intersect A B) ; gives (set 3 4)
(set-subtract A B)  ; gives (set 1 2)
(set=? A B)         ; gives #f
(subset? C A)       ; gives #f
(subset? C B)       ; gives #t

```



## REXX

REXX doesn't have native set support, but can be easily coded to handle lists as sets.

```rexx
/*REXX program  demonstrates some  common   SET   functions.                            */
truth.0= 'false';            truth.1= "true"    /*two common names for a truth table.   */
set.=                                           /*the  order  of sets isn't important.  */

call setAdd 'prime',2 3 2 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
call setSay 'prime'                             /*a small set of some  prime numbers.   */

call setAdd 'emirp',97 97 89 83 79 73 71 67 61 59 53 47 43 41 37 31 29 23 19 17 13 11 7 5 3 2
call setSay 'emirp'                             /*a small set of  backward  primes.     */

call setAdd 'happy',1 7 10 13 19 23 28 31 32 44 49 68 70 79 82 86 91 100 94 97 97 97 97 97
call setSay 'happy'                             /*a small set of some  happy  numbers.  */

      do j=11  to 100  by 10                    /*see if  PRIME  contains some numbers. */
      call setHas  'prime', j
      say '             prime contains'     j":"     truth.result
      end   /*j*/

call setUnion  'prime','happy','eweion';  call setSay 'eweion'                /* (sic). */
call setCommon 'prime','happy','common';  call setSay 'common'
call setDiff   'prime','happy','diff'  ;  call setSay 'diff';        _=left('', 12)
call setSubset 'prime','happy'         ;  say _ 'prime is a subset of happy:' truth.result
call setEqual  'prime','emirp'         ;  say _ 'prime is  equal   to emirp:' truth.result
exit                                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
setHas:    procedure expose set.; arg _ .,! .; return wordpos(!, set._)\==0
setAdd:    return set$('add'    , arg(1), arg(2))
setDiff:   return set$('diff'   , arg(1), arg(2), arg(3))
setSay:    return set$('say'    , arg(1), arg(2))
setUnion:  return set$('union'  , arg(1), arg(2), arg(3))
setCommon: return set$('common' , arg(1), arg(2), arg(3))
setEqual:  return set$('equal'  , arg(1), arg(2))
setSubset: return set$('subSet' , arg(1), arg(2))
/*──────────────────────────────────────────────────────────────────────────────────────*/
set$: procedure expose set.;   arg $,_1,_2,_3;   set_=set._1;   t=_3;   s=t;   !=1
      if $=='SAY'    then do;   say "[set."_1']= 'set._1;   return set._1;   end
      if $=='UNION'  then do
                          call set$ 'add', _3, set._1
                          call set$ 'add', _3, set._2
                          return set._3
                          end
      add=$=='ADD';  common=$=='COMMON'; diff=$=='DIFF'; eq=$=='EQUAL'; subset=$=='SUBSET'
      if common | diff | eq | subset  then s=_2
      if add  then do;  set_=_2;  t=_1;  s=_1;  end

          do j=1  for words(set_);       _=word(set_, j);       has=wordpos(_, set.s)\==0
          if (add    & \has) |,
             (common &  has) |,
             (diff   & \has)       then set.t=space(set.t _)
          if (eq | subset) & \has  then return 0
          end    /*j*/

      if subset  then return 1
      if eq      then  if arg()>3  then return 1
                                   else return set$('equal', _2, _1, 1)
      return set.t
```

'''output'''

```txt

[set.PRIME]=2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
[set.EMIRP]=97 89 83 79 73 71 67 61 59 53 47 43 41 37 31 29 23 19 17 13 11 7 5 3 2
[set.HAPPY]=1 7 10 13 19 23 28 31 32 44 49 68 70 79 82 86 91 100 94 97
             prime contains 11: true
             prime contains 21: false
             prime contains 31: true
             prime contains 41: true
             prime contains 51: false
             prime contains 61: true
             prime contains 71: true
             prime contains 81: false
             prime contains 91: false
[set.EWEION]=2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 1 10 28 32 44 49 68 70 82 86 91 100 94
[set.COMMON]=7 13 19 23 31 79 97
[set.DIFF]=2 3 5 11 17 29 37 41 43 47 53 59 61 67 71 73 83 89
             prime is a subset of happy: false
             prime is  equal   to emirp: true

```



## Ring


```ring

# Project : Set

arr = ["apple", "banana", "cherry", "date", "elderberry", "fig", "grape"]
for n = 1 to 25
     add(arr,"")
next
seta = "1010101"
see "Set A: " + arrset(arr,seta) + nl
setb = "0111110"
see "Set B: " + arrset(arr,setb) + nl
elementm = "0000010"
see "Element M: " + arrset(arr,elementm) + nl

temp = arrsetinsec(elementm,seta)
if len(temp) > 0
   see "M is an element of set A" + nl
else
   see "M is not an element of set A" + nl
ok
temp = arrsetinsec(elementm,setb)
if len(temp) > 0
   see "M is an element of set B" + nl
else
   see "M is not an element of set B" + nl
ok

see "The union of A and B is: "
see arrsetunion(seta,setb) + nl
see "The intersection of A and B is: "
see  arrsetinsec(seta,setb) + nl
see "The difference of A and B is: "
see arrsetnot(seta,setb) + nl

flag = arrsetsub(seta,setb)
if flag = 1
   see "Set A is a subset of set B" + nl
else
   see "Set A is not a subset of set B" + nl
ok
if seta = setb
   see "Set A is equal to set B" + nl
else
   see "Set A is not equal to set B" + nl
ok

func arrset(arr,set)
       o = ""
       for i = 1 to 7
            if set[i] = "1"
                o = o + arr[i] + ", "
            ok
       next
       return left(o,len(o)-2)

func arrsetunion(seta,setb)
       o = ""
       union = list(len(seta))
       for n = 1 to len(seta)
            if seta[n] = "1" or setb[n] = "1"
               union[n] = "1"
            else
               union[n] = "0"
            ok
       next
       for i = 1 to len(union)
            if union[i] = "1"
                o = o + arr[i] + ", "
            ok
       next
       return o

func arrsetinsec(setc,setd)
       o = ""
       union = list(len(setc))
       for n = 1 to len(setc)
            if setc[n] = "1" and setd[n] = "1"
               union[n] = "1"
            else
               union[n] = "0"
            ok
       next
       for i = 1 to len(union)
            if union[i] = "1"
                o = o + arr[i] + ", "
            ok
       next
       return o

func arrsetnot(seta,setb)
       o = ""
       union = list(len(seta))
       for n = 1 to len(seta)
            if seta[n] = "1" and setb[n] = "0"
               union[n] = "1"
            else
               union[n] = "0"
            ok
       next
       for i = 1 to len(union)
            if union[i] = "1"
                o = o + arr[i] + ", "
            ok
       next
       return o

func arrsetsub(setc,setd)
       flag = 1
       for n = 1 to len(setc)
            if setc[n] = "1" and setd[n] = "0"
               flag = 0
            ok
       next
       return flag

```

Output:

```txt

Set A: apple, cherry, elderberry, grape
Set B: banana, cherry, date, elderberry, fig
Element M: fig
M is not an element of set A
M is an element of set B
The union of A and B is: apple, banana, cherry, date, elderberry, fig, grape,
The intersection of A and B is: cherry, elderberry,
The difference of A and B is: apple, grape,
Set A is not a subset of set B
Set A is not equal to set B

```



## Ruby

Ruby's standard library contains a "set" package, which provides <code>Set</code> and <code>SortedSet</code> classes.

```ruby
>>
 require 'set'
=> true
>> s1, s2 = Set[1, 2, 3, 4], [3, 4, 5, 6].to_set # different ways of creating a set
=> [#<Set: {1, 2, 3, 4}>, #<Set: {5, 6, 3, 4}>]
>> s1 | s2 # Union
=> #<Set: {5, 6, 1, 2, 3, 4}>
>> s1 & s2 # Intersection
=> #<Set: {3, 4}>
>> s1 - s2 # Difference
=> #<Set: {1, 2}>
>> s1.proper_subset?(s1) # Proper subset
=> false
>> Set[3, 1].proper_subset?(s1) # Proper subset
=> true
>> s1.subset?(s1) # Subset
=> true
>> Set[3, 1].subset?(s1) # Subset
=> true
>> Set[3, 2, 4, 1] == s1 # Equality
=> true
>> s1 == s2 # Equality
=> false
>> s1.include?(2) # Membership
=> true
>> Set[1, 2, 3, 4, 5].proper_superset?(s1) # Proper superset
=> true
>> Set[1, 2, 3, 4].proper_superset?(s1) # Proper superset
=> false
>> Set[1, 2, 3, 4].superset?(s1) # Superset
=> true
>> s1 ^ s2 # Symmetric difference
=> #<Set: {5, 6, 1, 2}>
>> s1.size # Cardinality
=> 4
>> s1 << 99 # Mutability (or s1.add(99) )
=> #<Set: {99, 1, 2, 3, 4}>
>> s1.delete(99) # Mutability
=> #<Set: {1, 2, 3, 4}>
>> s1.merge(s2) # Mutability
=> #<Set: {5, 6, 1, 2, 3, 4}>
>> s1.subtract(s2) # Mutability
=> #<Set: {1, 2}>
>>
```


## Run BASIC


```Runbasic

A$	= "apple cherry elderberry grape"
B$	= "banana cherry date elderberry fig"
C$	= "apple cherry elderberry grape orange"
D$	= "apple cherry elderberry grape"
E$	= "apple cherry elderberry"
M$	= "banana"

print "A = ";A$
print "B = ";B$
print "C = ";C$
print "D = ";D$
print "E = ";E$
print "M = ";M$

if instr(A$,M$) = 0 then a$ = "not "
print "M is ";a$; "an element of Set A"
a$ = ""
if instr(B$,M$) = 0 then a$ = "not "
print "M is ";a$; "an element of Set B"

un$ = A$ + " "
for i = 1 to 5
 if instr(un$,word$(B$,i)) = 0 then un$ = un$ + word$(B$,i) + " "
next i
print "union(A,B) = ";un$

for i = 1 to 5
 if instr(A$,word$(B$,i)) <> 0 then ins$ = ins$ + word$(B$,i) + " "
next i
print "Intersection(A,B) = ";ins$

for i = 1 to 5
 if instr(B$,word$(A$,i)) = 0 then dif$ = dif$ + word$(A$,i) + " "
next i
print "Difference(A,B) = ";dif$

a = subs(A$,B$,"AB")
a = subs(A$,C$,"AC")
a = subs(A$,D$,"AD")
a = subs(A$,E$,"AE")

a = eqs(A$,B$,"AB")
a = eqs(A$,C$,"AC")
a = eqs(A$,D$,"AD")
a = eqs(A$,E$,"AE")
end

function subs(a$,b$,sets$)
 for i = 1 to 5
   if instr(b$,word$(a$,i)) <> 0 then subs = subs + 1
 next i
if subs = 4 then
  print left$(sets$,1);" is a subset of ";right$(sets$,1)
else
  print left$(sets$,1);" is not a subset of ";right$(sets$,1)
end if
end function

function eqs(a$,b$,sets$)
for i = 1 to 5
 if word$(a$,i) <> "" then a = a + 1
 if word$(b$,i) <> "" then b = b + 1
 if instr(b$,word$(a$,i)) <> 0 then c = c + 1
next i
if (a = b) and (a = c) then
  print left$(sets$,1);" is equal ";right$(sets$,1)
else
  print left$(sets$,1);" is not equal ";right$(sets$,1)
end if
end function
```
Output:

```txt
A = apple cherry elderberry grape
B = banana cherry date elderberry fig
C = apple cherry elderberry grape orange
D = apple cherry elderberry grape
E = apple cherry elderberry
M = banana
M is not an element of Set A
M is an element of Set B
union(A,B) = apple cherry elderberry grape banana date fig
Intersection(A,B) = cherry elderberry
Difference(A,B) = apple grape
A is not a subset of B
A is a subset of C
A is a subset of D
A is not a subset of E
A is not equal B
A is not equal C
A is equal D
A is not equal E
```



## Rust



```rust
use std::collections::HashSet;

fn main() {
  let a = vec![1, 3, 4].into_iter().collect::<HashSet<i32>>();
  let b = vec![3, 5, 6].into_iter().collect::<HashSet<i32>>();

  println!("Set A: {:?}", a.iter().collect::<Vec<_>>());
  println!("Set B: {:?}", b.iter().collect::<Vec<_>>());
  println!("Does A contain 4? {}", a.contains(&4));
  println!("Union: {:?}", a.union(&b).collect::<Vec<_>>());
  println!("Intersection: {:?}", a.intersection(&b).collect::<Vec<_>>());
  println!("Difference: {:?}", a.difference(&b).collect::<Vec<_>>());
  println!("Is A a subset of B? {}", a.is_subset(&b));
  println!("Is A equal to B? {}", a == b);
}
```



## Scala


```scala

object sets {
  val set1 = Set(1,2,3,4,5)
  val set2 = Set(3,5,7,9)
  println(set1 contains 3)
  println(set1 | set2)
  println(set1 & set2)
  println(set1 diff set2)
  println(set1 subsetOf set2)
  println(set1 == set2)
}

```



## Scheme

Implemented based on lists.  Not efficient on large sets.

```lisp
(define (element? a lst)
  (and (not (null? lst))
       (or (eq? a (car lst))
	   (element? a (cdr lst)))))

; util, not strictly needed
(define (uniq lst)
  (if (null? lst) lst
    (let ((a (car lst)) (b (cdr lst)))
      (if (element? a b)
	(uniq b)
	(cons a (uniq b))))))

(define (intersection a b)
  (cond ((null? a) '())
	((null? b) '())
	(else
	  (append (intersection (cdr a) b)
		  (if (element? (car a) b)
		    (list (car a))
		    '())))))

(define (union a b)
  (if (null? a) b
    (union (cdr a)
	   (if (element? (car a) b)
	     b
	     (cons (car a) b)))))

(define (diff a b) ; a - b
  (if (null? a) '()
    (if (element? (car a) b)
      (diff (cdr a) b)
      (cons (car a) (diff (cdr a) b)))))

(define (subset? a b) ; A ⊆ B
  (if (null? a) #t
    (and (element? (car a) b)
	 (subset? (cdr a) b))))

(define (set-eq? a b)
  (and (subset? a b)
       (subset? b a)))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const type: charSet is set of char;
enable_output(charSet);

const proc: main is func
  local
    const charSet: A is {'A', 'B', 'C', 'D', 'E', 'F'};
    var charSet: B is charSet.value;
    var char: m is 'A';
  begin
    B := {'E', 'F', 'G', 'H', 'I', 'K'};
    incl(B, 'J');        # Add 'J' to set B
    excl(B, 'K');        # Remove 'K' from set B
    writeln("A: " <& A);
    writeln("B: " <& B);
    writeln("m: " <& m);
    writeln("m in A -- m is an element in A: " <& m in A);
    writeln("A | B  -- union:                " <& A | B);
    writeln("A & B  -- intersection:         " <& A & B);
    writeln("A - B  -- difference:           " <& A - B);
    writeln("A >< B -- symmetric difference: " <& A >< B);
    writeln("A <= A -- subset:               " <& A <= A);
    writeln("A < A  -- proper subset:        " <& A < A);
    writeln("A = B  -- equality:             " <& A = B);
  end func;
```


```txt

A: {A, B, C, D, E, F}
B: {E, F, G, H, I, J}
m: A
m in A -- m is an element in A: TRUE
A | B  -- union:                {A, B, C, D, E, F, G, H, I, J}
A & B  -- intersection:         {E, F}
A - B  -- difference:           {A, B, C, D}
A >< B -- symmetric difference: {A, B, C, D, G, H, I, J}
A <= A -- subset:               TRUE
A < A  -- proper subset:        FALSE
A = B  -- equality:             FALSE

```



## SETL


```SETL

A := {1, 2, 3, 4};
B := {3, 4, 5, 6};
C := {4, 5};

-- Union, Intersection, Difference, Subset, Equality
print(A + B);       -- {1, 2, 3, 4, 5, 6}
print(A * B);       -- {3, 4}
print(A - B);       -- {1, 2}
print(C subset B);  -- #T
print(C = B);       -- #F

```



## Sidef

```ruby
class MySet(*set) {

    method init {
        var elems = set
        set = Hash()
        elems.each { |e| self += e }
    }

    method +(elem) {
        set{elem} = elem
        self
    }

    method del(elem) {
        set.delete(elem)
    }

    method has(elem) {
        set.has_key(elem)
    }

    method ∪(MySet that) {
        MySet(set.values..., that.values...)
    }

    method ∩(MySet that) {
        MySet(set.keys.grep{ |k| k ∈ that } \
                    .map { |k| set{k} }...)
    }

    method ∖(MySet that) {
        MySet(set.keys.grep{|k| !(k ∈ that) } \
                    .map {|k| set{k} }...)
    }

    method ^(MySet that) {
        var d = ((self ∖ that) ∪ (that ∖ self))
        MySet(d.values...)
    }

    method count { set.len }

    method ≡(MySet that) {
        (self ∖ that -> count.is_zero) && (that ∖ self -> count.is_zero)
    }

    method values { set.values }

    method ⊆(MySet that) {
        that.set.keys.each { |k|
            k ∈ self || return false
        }
        return true
    }

    method to_s {
        "Set{" + set.values.map{|e| "#{e}"}.sort.join(', ') + "}"
    }
}

class Object {
    method ∈(MySet set) {
        set.has(self)
    }
}
```


Usage example:

```ruby
var x = MySet(1, 2, 3)
5..7 -> each { |i| x += i }

var y = MySet(1, 2, 4, x)

say "set x is: #{x}"
say "set y is: #{y}"

[1,2,3,4,x].each { |elem|
    say ("#{elem} is ", elem ∈ y ? '' : 'not', " in y")
}

var (w, z)
say ("union: ", x ∪ y)
say ("intersect: ", x ∩ y)
say ("z = x ∖ y = ", z = (x ∖ y) )
say ("y is ", x ⊆ y ? "" : "not ", "a subset of x")
say ("z is ", x ⊆ z ? "" : "not ", "a subset of x")
say ("z = (x ∪ y) ∖ (x ∩ y) = ", z = ((x ∪ y) ∖ (x ∩ y)))
say ("w = x ^ y = ", w = (x ^ y))
say ("w is ", w ≡ z ? "" : "not ", "equal to z")
say ("w is ", w ≡ x ? "" : "not ", "equal to x")
```

```txt

set x is: Set{1, 2, 3, 5, 6, 7}
set y is: Set{1, 2, 4, Set{1, 2, 3, 5, 6, 7}}
1 is  in y
2 is  in y
3 is not in y
4 is  in y
Set{1, 2, 3, 5, 6, 7} is  in y
union: Set{1, 2, 3, 4, 5, 6, 7, Set{1, 2, 3, 5, 6, 7}}
intersect: Set{1, 2}
z = x ∖ y = Set{3, 5, 6, 7}
y is not a subset of x
z is a subset of x
z = (x ∪ y) ∖ (x ∩ y) = Set{3, 4, 5, 6, 7, Set{1, 2, 3, 5, 6, 7}}
w = x ^ y = Set{3, 4, 5, 6, 7, Set{1, 2, 3, 5, 6, 7}}
w is equal to z
w is not equal to x

```



## Simula


```simula
SIMSET
BEGIN

    ! WE DON'T SUBCLASS HEAD BUT USE COMPOSITION FOR CLASS SET ;
    CLASS SET;
    BEGIN
        PROCEDURE ADD(E); REF(ELEMENT) E;
        BEGIN
            IF NOT ISIN(E, THIS SET) THEN E.CLONE.INTO(H);
        END**OF**ADD;

        BOOLEAN PROCEDURE EMPTY; EMPTY := H.EMPTY;
        REF(LINK) PROCEDURE FIRST; FIRST :- H.FIRST;

        REF(HEAD) H;
        H :- NEW HEAD;
    END**OF**SET;

    ! WE SUBCLASS LINK FOR THE ELEMENTS CONTAINED IN THE SET ;
    LINK CLASS ELEMENT;
    VIRTUAL:
        PROCEDURE ISEQUAL IS
            BOOLEAN PROCEDURE ISEQUAL(OTHER); REF(ELEMENT) OTHER;;
        PROCEDURE REPR IS
            TEXT PROCEDURE REPR;;
        PROCEDURE REPR IS
            REF(ELEMENT) PROCEDURE CLONE;;
    BEGIN
    END**OF**ELEMENT;

    REF(SET) PROCEDURE UNION(S1, S2); REF(SET) S1, S2;
    BEGIN REF(SET) SU, S;
        SU :- NEW SET;
        FOR S :- S1, S2 DO
        BEGIN
            IF NOT S.EMPTY THEN
            BEGIN REF(ELEMENT) E;
                E :- S.FIRST;
                WHILE E =/= NONE DO
                BEGIN SU.ADD(E); E :- E.SUC;
                END;
            END;
        END;
        UNION :- SU;
    END**OF**UNION;

    REF(SET) PROCEDURE INTERSECTION(S1, S2); REF(SET) S1, S2;
    BEGIN REF(SET) SI;
        SI :- NEW SET;
        IF NOT S1.EMPTY THEN
        BEGIN REF(ELEMENT) E;
            E :- S1.FIRST;
            WHILE E =/= NONE DO
            BEGIN IF ISIN(E, S2) THEN SI.ADD(E); E :- E.SUC;
            END;
        END;
        INTERSECTION :- SI;
    END**OF**INTERSECTION;

    REF(SET) PROCEDURE MINUS(S1, S2); REF(SET) S1, S2;
    BEGIN REF(SET) SM;
        SM :- NEW SET;
        IF NOT S1.EMPTY THEN
        BEGIN REF(ELEMENT) E;
            E :- S1.FIRST;
            WHILE E =/= NONE DO
            BEGIN IF NOT ISIN(E, S2) THEN SM.ADD(E); E :- E.SUC;
            END;
        END;
        MINUS :- SM;
    END**OF**MINUS;

    BOOLEAN PROCEDURE ISSUBSET(S1, S2); REF(SET) S1, S2;
    BEGIN BOOLEAN B;
        B := TRUE;
        IF NOT S1.EMPTY THEN
        BEGIN REF(ELEMENT) E;
            E :- S1.FIRST;
            WHILE B AND E =/= NONE DO
            BEGIN
                B := ISIN(E, S2);
                E :- E.SUC;
            END;
        END;
        ISSUBSET := B;
    END**OF**ISSUBSET;

    BOOLEAN PROCEDURE ISEQUAL(S1, S2); REF(SET) S1, S2;
    BEGIN
        ISEQUAL := ISSUBSET(S1, S2) AND THEN ISSUBSET(S2, S1)
    END**OF**ISEQUAL;

    BOOLEAN PROCEDURE ISIN(ELE,S); REF(ELEMENT) ELE; REF(SET) S;
    BEGIN
        REF(ELEMENT) E; BOOLEAN FOUND;
        IF NOT S.EMPTY THEN
        BEGIN
            E :- S.FIRST;
            FOUND := E.ISEQUAL(ELE);
            WHILE NOT FOUND AND E =/= NONE DO
            BEGIN FOUND := E.ISEQUAL(ELE); E :- E.SUC;
            END;
        END;
        ISIN := FOUND
    END**OF**ISIN;

    PROCEDURE OUTSET(S); REF(SET) S;
    BEGIN
        REF(ELEMENT) E;
        OUTCHAR('{');
        IF NOT S.EMPTY THEN
        BEGIN
            E :- S.FIRST; OUTTEXT(E.REPR);
            FOR E :- E.SUC WHILE E =/= NONE DO
            BEGIN OUTTEXT(", "); OUTTEXT(E.REPR);
            END;
        END;
        OUTCHAR('}');
    END**OF**OUTSET;


    COMMENT
### =========== EXAMPLE USING SETS OF NUMBERS ===========
 ;


    ELEMENT CLASS NUMBER(N); INTEGER N;
    BEGIN
        BOOLEAN PROCEDURE ISEQUAL(OTHER); REF(ELEMENT) OTHER;
            ISEQUAL := N = OTHER QUA NUMBER.N;
        TEXT PROCEDURE REPR;
        BEGIN TEXT T; INTEGER I;
            T :- BLANKS(20); T.PUTINT(N);
            T.SETPOS(1);
            WHILE T.GETCHAR = ' ' DO;
            REPR :- T.SUB(T.POS - 1, T.LENGTH - T.POS + 2);
        END;
        REF(ELEMENT) PROCEDURE CLONE;
            CLONE :- NEW NUMBER(N);
    END**OF**NUMBER;

    PROCEDURE REPORT(S1, MSG1, S2, MSG2, S3); REF(SET) S1, S2, S3; TEXT MSG1, MSG2;
    BEGIN
        OUTSET(S1);    OUTCHAR(' ');
        OUTTEXT(MSG1); OUTCHAR(' ');
        OUTSET(S2);    OUTCHAR(' ');
        OUTTEXT(MSG2); OUTCHAR(' ');
        OUTSET(S3);
        OUTIMAGE;
    END**OF**REPORT;

    PROCEDURE REPORTBOOL(S1, MSG1, S2, MSG2, B); REF(SET) S1, S2; TEXT MSG1, MSG2; BOOLEAN B;
    BEGIN
        OUTSET(S1);    OUTCHAR(' ');
        OUTTEXT(MSG1); OUTCHAR(' ');
        OUTSET(S2);    OUTCHAR(' ');
        OUTTEXT(MSG2); OUTCHAR(' ');
        OUTTEXT(IF B THEN "T" ELSE "F");
        OUTIMAGE;
    END**OF**REPORTBOOL;

    PROCEDURE REPORTNUMBOOL(N1, MSG1, S1, MSG2, B); REF(ELEMENT) N1; REF(SET) S1; TEXT MSG1, MSG2; BOOLEAN B;
    BEGIN
        OUTTEXT(N1.REPR);    OUTCHAR(' ');
        OUTTEXT(MSG1); OUTCHAR(' ');
        OUTSET(S1);    OUTCHAR(' ');
        OUTTEXT(MSG2); OUTCHAR(' ');
        OUTTEXT(IF B THEN "T" ELSE "F");
        OUTIMAGE;
    END**OF**REPORTNUMBOOL;

    REF(SET) S1, S2, S3, S4, S5;
    REF(ELEMENT) E;
    INTEGER I;

    S1 :- NEW SET; FOR I := 1, 2, 3, 4    DO S1.ADD(NEW NUMBER(I));
    S2 :- NEW SET; FOR I := 3, 4, 5, 6    DO S2.ADD(NEW NUMBER(I));
    S3 :- NEW SET; FOR I := 3, 1          DO S3.ADD(NEW NUMBER(I));
    S4 :- NEW SET; FOR I := 1, 2, 3, 4, 5 DO S4.ADD(NEW NUMBER(I));
    S5 :- NEW SET; FOR I := 4, 3, 2, 1    DO S5.ADD(NEW NUMBER(I));

    REPORT(S1, "UNION", S2, " = ", UNION(S1, S2));

    REPORT(S1, "INTERSECTION", S2, " = ", INTERSECTION(S1, S2));

    REPORT(S1, "MINUS", S2, " = ", MINUS(S1, S2));

    REPORT(S2, "MINUS", S1, " = ", MINUS(S2, S1));

    E :- NEW NUMBER(2);
    REPORTNUMBOOL(E, "IN", S1, " = ", ISIN(E, S1));

    E :- NEW NUMBER(10);
    REPORTNUMBOOL(E, "NOT IN", S1, " = ", NOT ISIN(E, S1));

    REPORTBOOL(S1, "IS SUBSET OF", S1, " = ", ISSUBSET(S1, S1));
    REPORTBOOL(S3, "IS SUBSET OF", S1, " = ", ISSUBSET(S3, S1));
    REPORTBOOL(S4, "IS SUPERSET OF", S1, " = ", ISSUBSET(S1, S4));

    REPORTBOOL(S1, "IS EQUAL TO", S2, " = ", ISEQUAL(S1, S2));
    REPORTBOOL(S2, "IS EQUAL TO", S2, " = ", ISEQUAL(S2, S2));
    REPORTBOOL(S1, "IS EQUAL TO", S5, " = ", ISEQUAL(S1, S5));

END.

```

```txt

{1, 2, 3, 4} UNION {3, 4, 5, 6}  =  {1, 2, 3, 4, 5, 6}
{1, 2, 3, 4} INTERSECTION {3, 4, 5, 6}  =  {3, 4}
{1, 2, 3, 4} MINUS {3, 4, 5, 6}  =  {1, 2}
{3, 4, 5, 6} MINUS {1, 2, 3, 4}  =  {5, 6}
2 IN {1, 2, 3, 4}  =  T
10 NOT IN {1, 2, 3, 4}  =  T
{1, 2, 3, 4} IS SUBSET OF {1, 2, 3, 4}  =  T
{3, 1} IS SUBSET OF {1, 2, 3, 4}  =  T
{1, 2, 3, 4, 5} IS SUPERSET OF {1, 2, 3, 4}  =  T
{1, 2, 3, 4} IS EQUAL TO {3, 4, 5, 6}  =  F
{3, 4, 5, 6} IS EQUAL TO {3, 4, 5, 6}  =  T
{1, 2, 3, 4} IS EQUAL TO {4, 3, 2, 1}  =  T


```



## Smalltalk

```smalltalk

#(1 2 3) asSet union: #(2 3 4) asSet.
"a Set(1 2 3 4)"

#(1 2 3) asSet intersection: #(2 3 4) asSet.
"a Set(2 3)"

#(1 2 3) asSet difference: #(2 3 4) asSet.
"a Set(1)"

#(1 2 3) asSet includesAllOf: #(1 3) asSet.
"true"

#(1 2 3) asSet includesAllOf: #(1 3 4) asSet.
"false"

#(1 2 3) asSet = #(2 1 3) asSet.
"true"

#(1 2 3) asSet = #(1 2 4) asSet.
"false"

```



## SQL

```sql

-- set of numbers is a table
-- create one set with 3 elements

create table myset1 (element number);

insert into myset1 values (1);
insert into myset1 values (2);
insert into myset1 values (3);

commit;

-- check if 1 is an element

select 'TRUE' BOOL from dual
where 1 in
(select element from myset1);

-- create second set with 3 elements

create table myset2 (element number);

insert into myset2 values (1);
insert into myset2 values (5);
insert into myset2 values (6);

commit;

-- union sets

select element from myset1
union
select element from myset2;

-- intersection

select element from myset1
intersect
select element from myset2;

-- difference

select element from myset1
minus
select element from myset2;

-- subset

-- change myset2 to only have 1 as element

delete from myset2 where not element = 1;

commit;

-- check if myset2 subset of myset1

select 'TRUE' BOOL from dual
where 0 =  (select count(*) from
(select element from myset2
minus
select element from myset1));

-- equality

-- change myset1 to only have 1 as element

delete from myset1 where not element = 1;

commit;

 -- check if myset2 subset of myset1 and
 -- check if myset1 subset of myset2 and

select 'TRUE' BOOL from dual
where
0 =  (select count(*) from
(select element from myset2
minus
select element from myset1)) and
0 =
(select count(*) from
(select element from myset1
minus
select element from myset2));

```



```txt

SQL>
SQL> -- set of numbers is a table
SQL> -- create one set with 3 elements
SQL>
SQL> create table myset1 (element number);

Table created.

SQL>
SQL> insert into myset1 values (1);

1 row created.

SQL> insert into myset1 values (2);

1 row created.

SQL> insert into myset1 values (3);

1 row created.

SQL>
SQL> commit;

Commit complete.

SQL>
SQL> -- check if 1 is an element
SQL>
SQL> select 'TRUE' BOOL from dual
  2  where 1 in
  3  (select element from myset1);

BOOL
----
TRUE

SQL>
SQL> -- create second set with 3 elements
SQL>
SQL> create table myset2 (element number);

Table created.

SQL>
SQL> insert into myset2 values (1);

1 row created.

SQL> insert into myset2 values (5);

1 row created.

SQL> insert into myset2 values (6);

1 row created.

SQL>
SQL> commit;

Commit complete.

SQL>
SQL> -- union sets
SQL>
SQL> select element from myset1
  2  union
  3  select element from myset2;

   ELEMENT
----------
         1
         2
         3
         5
         6

SQL>
SQL> -- intersection
SQL>
SQL> select element from myset1
  2  intersect
  3  select element from myset2;

   ELEMENT
----------
         1

SQL>
SQL> -- difference
SQL>
SQL> select element from myset1
  2  minus
  3  select element from myset2;

   ELEMENT
----------
         2
         3

SQL>
SQL> -- subset
SQL>
SQL> -- change myset2 to only have 1 as element
SQL>
SQL> delete from myset2 where not element = 1;

2 rows deleted.

SQL>
SQL> commit;

Commit complete.

SQL>
SQL> -- check if myset2 subset of myset1
SQL>
SQL> select 'TRUE' BOOL from dual
  2  where 0 =  (select count(*) from
  3  (select element from myset2
  4  minus
  5  select element from myset1));

BOOL
----
TRUE

SQL>
SQL> -- equality
SQL>
SQL> -- change myset1 to only have 1 as element
SQL>
SQL> delete from myset1 where not element = 1;

2 rows deleted.

SQL>
SQL> commit;

Commit complete.

SQL>
SQL>  -- check if myset2 subset of myset1 and
SQL>  -- check if myset1 subset of myset2 and
SQL>
SQL> select 'TRUE' BOOL from dual
  2  where
  3  0 =  (select count(*) from
  4  (select element from myset2
  5  minus
  6  select element from myset1)) and
  7  0 =
  8  (select count(*) from
  9  (select element from myset1
 10  minus
 11  select element from myset2));

BOOL
----
TRUE

```



## Swift

```swift
var s1 : Set<Int>
 = [1, 2, 3, 4]
let s2 : Set<Int> = [3, 4, 5, 6]
println(s1.union(s2)) // union; prints "[5, 6, 2, 3, 1, 4]"
println(s1.intersect(s2)) // intersection; prints "[3, 4]"
println(s1.subtract(s2)) // difference; prints "[2, 1]"
println(s1.isSubsetOf(s1)) // subset; prints "true"
println(Set<Int>([3, 1]).isSubsetOf(s1)) // subset; prints "true"
println(s1.isStrictSubsetOf(s1)) // proper subset; prints "false"
println(Set<Int>([3, 1]).isStrictSubsetOf(s1)) // proper subset; prints "true"
println(Set<Int>([3, 2, 4, 1]) == s1) // equality; prints "true"
println(s1 == s2) // equality; prints "false"
println(s1.contains(2)) // membership; prints "true"
println(Set<Int>([1, 2, 3, 4]).isSupersetOf(s1)) // superset; prints "true"
println(Set<Int>([1, 2, 3, 4]).isStrictSupersetOf(s1)) // proper superset; prints "false"
println(Set<Int>([1, 2, 3, 4, 5]).isStrictSupersetOf(s1)) // proper superset; prints "true"
println(s1.exclusiveOr(s2)) // symmetric difference; prints "[5, 6, 2, 1]"
println(s1.count) // cardinality; prints "4"
s1.insert(99) // mutability
println(s1) // prints "[99, 2, 3, 1, 4]"
s1.remove(99) // mutability
println(s1) // prints "[2, 3, 1, 4]"
s1.unionInPlace(s2) // mutability
println(s1) // prints "[5, 6, 2, 3, 1, 4]"
s1.subtractInPlace(s2) // mutability
println(s1) // prints "[2, 1]"
s1.exclusiveOrInPlace(s2) // mutability
println(s1) // prints "[5, 6, 2, 3, 1, 4]"
```



## Tcl

Sets in Tcl are modeled as lists of items, with operations that preserve uniqueness of membership.
```tcl
package require struct::set

# Many ways to build sets
set s1 [list 1 2 3 4]
set s2 {3 4 5 6}
struct::set add s3 {2 3 4 3 2};   # $s3 will be proper set...
set item 5

puts "union: [struct::set union $s1 $s2]"
puts "intersection: [struct::set intersect $s1 $s2]"
puts "difference: [struct::set difference $s1 $s2]"
puts "membership predicate: [struct::set contains $s1 $item]"
puts "subset predicate: [struct::set subsetof $s1 $s2]";   # NB: not strict subset test!
puts "equality predicate: [struct::set equal $s1 $s2]"

# Adding an element to a set (note that we pass in the name of the variable holding the set):
struct::set include s3 $item
# Removing an element from a set:
struct::set exclude s3 $item
# Getting the cardinality:
puts "cardinality: [struct::set size $s3]
```



## VBA


```vb
'Implementation of "set" using the built in Collection datatype.
'A collection can hold any object as item. The examples here are only strings.
'A collection stores item, key pairs. With the key you can retrieve the item.
'The keys are hidden and cannot be changed. No duplicate keys are allowed.
'For the "set" implementation item is the same as the key. And keys must
'be a string.
Private Function createSet(t As Variant) As Collection
    Dim x As New Collection
    For Each elem In t
        x.Add elem, elem
    Next elem
    Set createSet = x
End Function
Private Function isElement(s As Variant, x As Collection) As Boolean
    Dim errno As Integer, t As Variant
    On Error GoTo err
    t = x(s)
    isElement = True
    Exit Function
err:
    isElement = False
End Function
Private Function setUnion(A As Collection, B As Collection) As Collection
    Dim x As New Collection
    For Each elem In A
        x.Add elem, elem
    Next elem
    For Each elem In B
        On Error Resume Next 'Trying to add a duplicate throws an error
        x.Add elem, elem
    Next elem
    Set setUnion = x
End Function
Private Function intersection(A As Collection, B As Collection) As Collection
    Dim x As New Collection
    For Each elem In A
        If isElement(elem, B) Then x.Add elem, elem
    Next elem
    For Each elem In B
        If isElement(elem, A) Then
            On Error Resume Next
            x.Add elem, elem
        End If
    Next elem
    Set intersection = x
End Function
Private Function difference(A As Collection, B As Collection) As Collection
    Dim x As New Collection
    For Each elem In A
        If Not isElement(elem, B) Then x.Add elem, elem
    Next elem
    Set difference = x
End Function
Private Function subset(A As Collection, B As Collection) As Boolean
    Dim flag As Boolean
    flag = True
    For Each elem In A
        If Not isElement(elem, B) Then
            flag = False
            Exit For
        End If
    Next elem
    subset = flag
End Function
Private Function equality(A As Collection, B As Collection) As Boolean
    Dim flag As Boolean
    flag = True
    If A.Count = B.Count Then
        For Each elem In A
            If Not isElement(elem, B) Then
                flag = False
                Exit For
            End If
        Next elem
    Else
        flag = False
    End If
    equality = flag
End Function
Private Function properSubset(A As Collection, B As Collection) As Boolean
    Dim flag As Boolean
    flag = True
    If A.Count < B.Count Then
        For Each elem In A
            If Not isElement(elem, B) Then
                flag = False
                Exit For
            End If
        Next elem
    Else
        flag = False
    End If
    properSubset = flag
End Function
Public Sub main()
    'Set creation
    Dim s As Variant
    Dim A As Collection, B As Collection, C As Collection
    s = [{"Apple","Banana","Pear","Pineapple"}]
    Set A = createSet(s) 'Fills the collection A with the elements of s
    'Test m ? S -- "m is an element in set S"
    Debug.Print isElement("Apple", A) 'returns True
    Debug.Print isElement("Fruit", A) 'returns False
    'A ? B -- union; a set of all elements either in set A or in set B.
    s = [{"Fruit","Banana","Pear","Orange"}]
    Set B = createSet(s)
    Set C = setUnion(A, B)
    'A n B -- intersection; a set of all elements in both set A and set B.
    Set C = intersection(A, B)
    'A \ B -- difference; a set of all elements in set A, except those in set B.
    Set C = difference(A, B)
    'A ? B -- subset; true if every element in set A is also in set B.
    Debug.Print subset(A, B)
    'A = B -- equality; true if every element of set A is in set B and vice versa.
    Debug.Print equality(A, B)
    'Proper subset
    Debug.Print properSubset(A, B)
    'Modify -remove an element by key
    A.Remove "Apple"
    'Modify -remove the first element in the collection/set
    A.Remove 1
    'Add "10" to A
    A.Add "10", "10"
End Sub
```


## zkl

A simplistic implementation that is fine for smallish sets

```zkl
var [const] unique = Utils.Helpers.listUnique;
class Set {
   fcn init { var [const] set = (vm.arglist.copy() : unique(_)) }
   fcn holds(x) { set.holds(x) }
   fcn union(setB) { self(set.xplode(),setB.set.xplode()) }
   fcn intersection(setB){ sb:=setB.set;
      C:=self(); sc:=C.set;
      foreach x in (set){ if (sb.holds(x)) sc.append(x) }
      C
   }
   fcn diff(setB){ C:=self(); C.set.extend(set);
      setB.set.pump(Void,C.set.remove);
      C
   }
   fcn isSubset(setB){ sb:=setB.set;
      set.pump(Void,'wrap(x){
         if (not sb.holds(x)) return(Void.Stop,False); True
      })
   }
   fcn __opEQ(setB) { ((set.len() == setB.set.len()) and self.isSubset(setB)) }
}
```


```txt

A := Set(1,2,3,3,3,4);
A.set.println();    //--> L(1,2,3,4)
A.holds(3).println();  //--> True
A.holds(9).println();  //--> False

B:=Set("one",2,"three");
A.union(B).set.println(); // -->L(1,2,3,4,"one","three")
B.union(A).set.println(); // -->L("one",2,"three",1,3,4)
A.union(B).diff(B.union(A)).set.println(); // -->L()

A.intersection(B).set.println(); //-->L(2)
B.intersection(A).set.println(); //-->L(2)

A.diff(B).set.println();  //-->L(1,3,4)
B.diff(A).set.println();  //-->L("one","three")

A.isSubset(B).println();  //-->False
A.isSubset(A).println();  //-->True
Set("three",2,2,2,2,2).isSubset(B).println();  //-->True

(A==B).println();  //-->False
(A==A).println();  //-->True
(A==Set(2,3,1,4)).println(); //-->True

```


