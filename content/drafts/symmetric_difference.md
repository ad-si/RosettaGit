+++
title = "Symmetric difference"
description = ""
date = 2019-08-24T20:19:26Z
aliases = []
[extra]
id = 5032
[taxonomies]
categories = []
tags = []
+++

{{task|Discrete math}}

;Task
Given two [[set]]s ''A'' and ''B'', compute <math>(A \setminus B) \cup (B \setminus A).</math>

That is, enumerate the items that are in ''A'' or ''B'' but not both. This set is called the [[wp:Symmetric difference|symmetric difference]] of ''A'' and ''B''.

In other words: <math>(A \cup B) \setminus (A \cap B)</math> (the set of items that are in at least one of ''A'' or ''B'' minus the set of items that are in both ''A'' and ''B'').

Optionally, give the individual differences (<math>A \setminus B</math> and <math>B \setminus A</math>) as well.


;Test cases
 A = {John, Bob, Mary, Serena}
 B = {Jim, Mary, John, Bob}


;Notes
# If your code uses lists of items to represent sets then ensure duplicate items in lists are correctly handled. For example two lists representing sets of <code>a = ["John", "Serena", "Bob", "Mary", "Serena"]</code> and <code>b = ["Jim", "Mary", "John", "Jim", "Bob"]</code> should produce the result of just two strings: <code>["Serena", "Jim"]</code>, in any order.
# In the mathematical notation above <code>A \ B</code> gives the set of items in A that are not in B; <code>A ∪ B</code> gives the set of items in both A and B, (their ''union''); and <code>A ∩ B</code> gives the set of items that are in both A and B (their ''intersection'').




## Ada

Ada has the lattice operation '''xor''' predefined on Boolean, modular types, 1D arrays, set implementations from the standard library. The provided solution uses arrays:

```Ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_XOR is
   type Person is (John, Bob, Mary, Serena, Jim);
   type Group is array (Person) of Boolean;
   procedure Put (Set : Group) is
      First : Boolean := True;
   begin
      for I in Set'Range loop
         if Set (I) then
            if First then
               First := False;
            else
               Put (',');
            end if;
            Put (Person'Image (I));
         end if;
      end loop;
   end Put;

   A : Group := (John | Bob | Mary | Serena => True, others => False);
   B : Group := (Jim | Mary | John | Bob    => True, others => False);
begin
   Put ("A xor B = "); Put (A xor B);     New_Line;
   Put ("A - B   = "); Put (A and not B); New_Line;
   Put ("B - A   = "); Put (B and not A); New_Line;
end Test_XOR;
```

Sample output:

```txt

A xor B = SERENA,JIM
A - B   = SERENA
B - A   = JIM

```



## Aime


```aime
show_sdiff(record u, x)
{
    record r;
    text s;

    r.copy(u);

    for (s in x) {
        if (r.key(s)) {
            r.delete(s);
        } else {
            r.p_integer(s, 0);
        }
    }

    r.vcall(o_, 0, "\n");
}

new_set(...)
{
    record r;

    ucall(r_p_integer, 1, r, 0);

    r;
}

main(void)
{
    show_sdiff(new_set("John", "Bob", "Mary", "Serena"),
               new_set("Jim", "Mary", "John", "Bob"));

    0;
}
```

{{out}}

```txt
Jim
Serena
```



## Apex


```Java>Set<String> setA = new Set<String
{'John', 'Bob', 'Mary', 'Serena'};
Set<String> setB = new Set<String>{'Jim', 'Mary', 'John', 'Bob'};

// Option 1
Set<String> notInSetA = setB.clone();
notInSetA.removeAll(setA);

Set<String> notInSetB = setA.clone();
notInSetB.removeAll(setB);

Set<String> symmetricDifference = new Set<String>();
symmetricDifference.addAll(notInSetA);
symmetricDifference.addAll(notInSetB);

// Option 2
Set<String> union = setA.clone();
union.addAll(setB);

Set<String> intersection = setA.clone();
intersection.retainAll(setB);

Set<String> symmetricDifference2 = union.clone();
symmetricDifference2.removeAll(intersection);

System.debug('Not in set A: ' + notInSetA);
System.debug('Not in set B: ' + notInSetB);
System.debug('Symmetric Difference: ' + symmetricDifference);
System.debug('Symmetric Difference 2: ' + symmetricDifference2);
```

{{out}}

```txt
Not in set A: {Jim}
Not in set B: {Serena}
Symmetric Difference: {Jim, Serena}
Symmetric Difference 2: {Jim, Serena}
```




## AppleScript

{{Trans|JavaScript}} (ES6 Functional JS)

```AppleScript
-- SYMMETRIC DIFFERENCE -------------------------------------------

-- symmetricDifference :: [a] -> [a] -> [a]
on symmetricDifference(xs, ys)
    union(difference(xs, ys), difference(ys, xs))
end symmetricDifference

-- TEST -----------------------------------------------------------
on run
    set a to ["John", "Serena", "Bob", "Mary", "Serena"]
    set b to ["Jim", "Mary", "John", "Jim", "Bob"]

    symmetricDifference(a, b)

    -->  {"Serena", "Jim"}
end run


-- GENERIC FUNCTIONS ----------------------------------------------

-- delete :: Eq a => a -> [a] -> [a]
on |delete|(x, xs)
    set mbIndex to elemIndex(x, xs)
    set lng to length of xs

    if mbIndex is not missing value then
        if lng > 1 then
            if mbIndex = 1 then
                items 2 thru -1 of xs
            else if mbIndex = lng then
                items 1 thru -2 of xs
            else
                tell xs to items 1 thru (mbIndex - 1) & ¬
                    items (mbIndex + 1) thru -1
            end if
        else
            {}
        end if
    else
        xs
    end if
end |delete|

-- difference :: [a] -> [a] -> [a]
on difference(xs, ys)
    script
        on |λ|(a, y)
            if a contains y then
                my |delete|(y, a)
            else
                a
            end if
        end |λ|
    end script

    foldl(result, xs, ys)
end difference

-- elemIndex :: a -> [a] -> Maybe Int
on elemIndex(x, xs)
    set lng to length of xs
    repeat with i from 1 to lng
        if x = (item i of xs) then return i
    end repeat
    return missing value
end elemIndex

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- nub :: [a] -> [a]
on nub(xs)
    if (length of xs) > 1 then
        set x to item 1 of xs
        [x] & nub(|delete|(x, items 2 thru -1 of xs))
    else
        xs
    end if
end nub

-- union :: [a] -> [a] -> [a]
on union(xs, ys)
    script flipDelete
        on |λ|(xs, x)
            my |delete|(x, xs)
        end |λ|
    end script

    set sx to nub(xs)
    sx & foldl(flipDelete, nub(ys), sx)
end union
```

{{Out}}

```AppleScript
{"Serena", "Jim"}
```



## AutoHotkey


```autohotkey
setA = John, Bob, Mary, Serena
setB = Jim, Mary, John, Bob
MsgBox,, Singles, % SymmetricDifference(setA, setB)

setA = John, Serena, Bob, Mary, Serena
setB = Jim, Mary, John, Jim, Bob
MsgBox,, Duplicates, % SymmetricDifference(setA, setB)

;---------------------------------------------------------------------------
SymmetricDifference(A, B) { ; returns the symmetric difference of A and B
;---------------------------------------------------------------------------
    StringSplit, A_, A, `,, %A_Space%
    Loop, %A_0%
        If Not InStr(B, A_%A_Index%)
        And Not InStr(Result, A_%A_Index%)
            Result .= A_%A_Index% ", "
    StringSplit, B_, B, `,, %A_Space%
    Loop, %B_0%
        If Not InStr(A, B_%A_Index%)
        And Not InStr(Result, B_%A_Index%)
            Result .= B_%A_Index% ", "
    Return, SubStr(Result, 1, -2)
}
```

Message boxes show:

```txt
Singles
---------------------------
Serena, Jim

OK
```


```txt
Duplicates
---------------------------
Serena, Jim

OK
```



## AWK


```AWK

# syntax: GAWK -f SYMMETRIC_DIFFERENCE.AWK
BEGIN {
    load("John,Bob,Mary,Serena",A)
    load("Jim,Mary,John,Bob",B)
    show("A \\ B",A,B)
    show("B \\ A",B,A)
    printf("symmetric difference: ")
    for (i in C) {
      if (!(i in A && i in B)) {
        printf("%s ",i)
      }
    }
    printf("\n")
    exit(0)
}
function load(str,arr,  i,n,temp) {
    n = split(str,temp,",")
    for (i=1; i<=n; i++) {
      arr[temp[i]]
      C[temp[i]]
    }
}
function show(str,a,b,  i) {
    printf("%s: ",str)
    for (i in a) {
      if (!(i in b)) {
        printf("%s ",i)
      }
    }
    printf("\n")
}

```

<p>output:</p>

```txt

A \ B: Serena
B \ A: Jim
symmetric difference: Serena Jim

```



## BBC BASIC

Here sets are represented as integers, hence there are a maximum of 32 elements in a set.

```bbcbasic
      DIM list$(4)
      list$() = "Bob", "Jim", "John", "Mary", "Serena"

      setA% = %11101
      PRINT "Set A: " FNlistset(list$(), setA%)
      setB% = %01111
      PRINT "Set B: " FNlistset(list$(), setB%)

      REM Compute symmetric difference:
      setC% = setA% EOR setB%
      PRINT '"Symmetric difference: " FNlistset(list$(), setC%)

      REM Optional:
      PRINT "Set A \ Set B: " FNlistset(list$(), setA% AND NOT setB%)
      PRINT "Set B \ Set A: " FNlistset(list$(), setB% AND NOT setA%)
      END

      DEF FNlistset(list$(), set%)
      LOCAL i%, o$
      FOR i% = 0 TO 31
        IF set% AND 1 << i% o$ += list$(i%) + ", "
      NEXT
      = LEFT$(LEFT$(o$))
```

'''Output:'''

```txt

Set A: Bob, John, Mary, Serena
Set B: Bob, Jim, John, Mary

Symmetric difference: Jim, Serena
Set A \ Set B: Serena
Set B \ Set A: Jim

```



## Bracmat

Walk through the concatenation of the two lists, using backtracking (forced by the ~ operator).
If an element is in both lists, or if the element already is in the accumulated result <code>symdiff</code>, continue. Otherwise add the element to <code>symdiff</code>. When all elements are done and backtracking therefore finally fails, return the contents of <code>symdiff</code>. The flag <code>%</code> in the pattern <code>%@?x</code> ensures that only nontrivial elements (i.e. non-empty strings in this case) are matched. The <code>@</code> flag ensures that at most one string is matched. Together these flags ensure that exactly one element is matched.

```bracmat
(SymmetricDifference=
  A B x symdiff
.   !arg:(?A.?B)
  & :?symdiff
  & (   !A !B
      :   ?
          ( %@?x
          & ( !A:? !x ?&!B:? !x ?
            | !symdiff:? !x ?
            | !symdiff !x:?symdiff
            )
          & ~
          )
          ?
    | !symdiff
    ));
```

Run:

```bracmat
SymmetricDifference$(john serena bob mary serena.jim mary john jim bob)
```

Output:

```bracmat>serena jim</lang



## C

Simple method:

```c
#include <stdio.h>
#include <string.h>

const char *A[] = { "John", "Serena", "Bob", "Mary", "Serena" };
const char *B[] = { "Jim", "Mary", "John", "Jim", "Bob" };

#define LEN(x) sizeof(x)/sizeof(x[0])

/* null duplicate items */
void uniq(const char *x[], int len)
{
	int i, j;
	for (i = 0; i < len; i++)
		for (j = i + 1; j < len; j++)
			if (x[j] && x[i] && !strcmp(x[i], x[j])) x[j] = 0;
}

int in_set(const char *const x[], int len, const char *match)
{
	int i;
	for (i = 0; i < len; i++)
		if (x[i] && !strcmp(x[i], match))
			return 1;
	return 0;
}

/* x - y */
void show_diff(const char *const x[], int lenx, const char *const y[], int leny)
{
	int i;
	for (i = 0; i < lenx; i++)
		if (x[i] && !in_set(y, leny, x[i]))
			printf("  %s\n", x[i]);
}

/* X ^ Y */
void show_sym_diff(const char *const x[], int lenx, const char *const y[], int leny)
{
	show_diff(x, lenx, y, leny);
	show_diff(y, leny, x, lenx);
}

int main()
{
	uniq(A, LEN(A));
	uniq(B, LEN(B));
	printf("A \\ B:\n"); show_diff(A, LEN(A), B, LEN(B));
	printf("\nB \\ A:\n"); show_diff(B, LEN(B), A, LEN(A));
	printf("\nA ^ B:\n");  show_sym_diff(A, LEN(A), B, LEN(B));

	return 0;
}
```
output

```txt

A \ B:
  Serena

B \ A:
  Jim

A ^ B:
  Serena
  Jim

```

If you prefer something elaborate:

```c
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

const char *mary="Mary";
const char *bob="Bob";
const char *jim="Jim";
const char *john="John";
const char *serena="Serena";

const char *setA[] = {john,bob,mary,serena};
const char *setB[] = {jim,mary,john,bob};

#define XSET(j)  j, (sizeof(j)/sizeof(*j))
#define TALLOC(n,typ) malloc(n*sizeof(typ))

typedef enum {
    esdDIFFERENCE,
    esdSYMMETRIC } EsdFunction;
/** * * * * * * * * * * * * * * * * * * * *
 * return value is difference or symmetric difference set
 *    its size is returned in sym_size
 *    f determinse whether it is a symmetric difference, or normal difference
 * * * * * * * * * * * * * * * * * * * * **/
const char ** symmdiff( int *sym_size, EsdFunction f, const char *setA[], int setAsize, const char *setB[], int setBsize)
{
    int union_size;
    int max_union_size;
    int diff_size;
    const char **union_set;
    const char **diff_set;
    int *union_xor;
    int ix, ixu;

    max_union_size = setAsize + setBsize;
    union_set = TALLOC(max_union_size, const char *);
    union_xor = TALLOC(max_union_size, int);

    /* I'm assuming here that setA has no duplicates,
     * i.e. is a set in mathematical sense */
    for (ix=0; ix<setAsize; ix++) {
        union_set[ix] = setA[ix];
        union_xor[ix] = 1;
    }
    diff_size = union_size = setAsize;
    for (ix=0; ix<setBsize; ix++) {
        for (ixu=0; ixu<union_size; ixu++) {
            if (union_set[ixu] == setB[ix]) break;
        }
        if (ixu < union_size) {	/* already in union */
            union_xor[ixu] = 1-union_xor[ixu];
            diff_size--;
        }
        else {		/* not already in union -add */
            if (f == esdSYMMETRIC) {
                union_set[ixu] = setB[ix];
                union_xor[ixu] = 1;
                union_size++;
                diff_size++;
            }
        }
    }
    /* Put results in symdiff set */
    diff_set = TALLOC(diff_size, const char *);
    ix = 0;
    for (ixu=0; ixu<union_size; ixu++) {
        if (union_xor[ixu]) {
            if (ix == diff_size) {
                printf("Short of space in diff_set\n");
                exit(1);
            }
            diff_set[ix] = union_set[ixu];
            ix++;
        }
    }
    *sym_size = diff_size;
    free(union_xor);
    free(union_set);
    return diff_set;
}

/* isSet tests that elements of list are unique, that is, that the list is a
 * mathematical set.  The uniqueness test implemented here is strcmp. */
int isSet(const char *list[], int lsize)
{
    int i, j;
    const char *e;
    if (lsize == 0) {
        return 1;
    }
    for (i = lsize-1; i>0; i--) {
        e = list[i];
        for (j = i-1; j>=0; j--) {
            if (strcmp(list[j], e) == 0) {
                return 0;
            }
        }
    }
    return 1;
}

void printSet (const char *set[], int ssize)
{
    int ix;
    printf(" = {");
    for (ix=0;ix<ssize; ix++) {
        printf( "%s ", set[ix]);
    }
    printf("}\n");
}

int main()
{
    const char **symset;
    int sysize;

    /* Validate precondition stated by task, that inputs are sets. */
    assert(isSet(XSET(setA)));
    assert(isSet(XSET(setB)));

    printf ("A symmdiff B");
    symset = symmdiff( &sysize, esdSYMMETRIC, XSET(setA), XSET(setB));
    printSet(symset, sysize);
    free(symset);
    printf ("A - B");
    symset = symmdiff( &sysize, esdDIFFERENCE, XSET(setA), XSET(setB));
    printSet(symset, sysize);
    printf ("B - A");
    symset = symmdiff( &sysize, esdDIFFERENCE, XSET(setB), XSET(setA));
    printSet(symset, sysize);
    free(symset);

    return 0;
}
```

Output

```txt

 A symmdiff B = {Serena Jim }
 A - B = {Serena }
 B - A = {Jim }

```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace RosettaCode.SymmetricDifference
{
    public static class IEnumerableExtension
    {
        public static IEnumerable<T> SymmetricDifference<T>(this IEnumerable<T> @this, IEnumerable<T> that)
        {
            return @this.Except(that).Concat(that.Except(@this));
        }
    }

    class Program
    {
        static void Main()
        {
            var a = new[] { "John", "Bob", "Mary", "Serena" };
            var b = new[] { "Jim", "Mary", "John", "Bob" };

            foreach (var element in a.SymmetricDifference(b))
            {
                Console.WriteLine(element);
            }
        }
    }
}
```

Output:

```txt

Serena
Jim

```



## C++


```cpp
#include <iostream>
#include <set>
#include <algorithm>
#include <iterator>
#include <string>

using namespace std;

int main( ) {
   string setA[] = { "John", "Bob" , "Mary", "Serena" };
   string setB[] = { "Jim" , "Mary", "John", "Bob"  };
   set<string>
       firstSet( setA , setA + 4 ),
       secondSet( setB , setB + 4 ),
       symdiff;

   set_symmetric_difference( firstSet.begin(), firstSet.end(),
                             secondSet.begin(), secondSet.end(),
                             inserter( symdiff, symdiff.end() ) );

   copy( symdiff.begin(), symdiff.end(), ostream_iterator<string>( cout , " " ) );
   cout << endl;
   return 0;
}
```


Output:
Jim Serena


## Clojure


```clojure
(use '[clojure.set])

(defn symmetric-difference [s1 s2]
  (union (difference s1 s2) (difference s2 s1)))

(symmetric-difference #{:john :bob :mary :serena} #{:jim :mary :john :bob})
```



## Common Lisp


```lisp
(set-exclusive-or
  (remove-duplicates '(John Serena Bob Mary Serena))
  (remove-duplicates '(Jim Mary John Jim Bob)))
```

Output:
 (JIM SERENA)


## D

Generic version.

```d
import std.stdio, std.algorithm, std.array;

struct Set(T) {
    immutable T[] items;

    Set opSub(in Set other) const pure nothrow {
        return items.filter!(x => !other.items.canFind(x)).array.Set;
    }

    Set opAdd(in Set other) const pure nothrow {
        return Set(this.items ~ (other - this).items);
    }
}

Set!T symmetricDifference(T)(in Set!T left, in Set!T right)
pure nothrow {
    return (left - right) + (right - left);
}

void main() {
    immutable A = ["John", "Bob", "Mary", "Serena"].Set!string;
    immutable B = ["Jim", "Mary", "John", "Bob"].Set!string;

    writeln("        A\\B: ", (A - B).items);
    writeln("        B\\A: ", (B - A).items);
    writeln("A symdiff B: ", symmetricDifference(A, B).items);
}
```

{{out}}

```txt
        A\B: ["Serena"]
        B\A: ["Jim"]
A symdiff B: ["Serena", "Jim"]
```


=={{header|Déjà Vu}}==
Déjà Vu has no real set type. Instead, it uses a dictionary whose keys are the set values. The <code>set{</code> constructor uses <code>true</code> as a dummy value, and sets <code>false</code> as a dummy value.

```dejavu
set :setA set{ :John :Bob :Mary :Serena }
set :setB set{ :Jim :Mary :John :Bob }

symmetric-difference A B:
	}
	for a in keys A:
		if not has B a:
			a
	for b in keys B:
		if not has A b:
			b
	set{

!. symmetric-difference setA setB
```

{{out}}

```txt
set{ :Serena :Jim }
```



## E


```e
? def symmDiff(a, b) { return (a &! b) | (b &! a) }
# value: <symmDiff>

? symmDiff(["John", "Bob", "Mary", "Serena"].asSet(), ["Jim", "Mary", "John", "Bob"].asSet())
# value: ["Jim", "Serena"].asSet()
```



## Elixir

{{works with|Elixir|1.2}}

```elixir
iex(1)> a = ~w[John Bob Mary Serena] |> MapSet.new
#MapSet<["Bob", "John", "Mary", "Serena"]>
iex(2)> b = ~w[Jim Mary John Bob] |> MapSet.new
#MapSet<["Bob", "Jim", "John", "Mary"]>
iex(3)> sym_dif = fn(a,b) -> MapSet.difference(MapSet.union(a,b), MapSet.intersection(a,b)) end
#Function<12.54118792/2 in :erl_eval.expr/5>
iex(4)> sym_dif.(a,b)
#MapSet<["Jim", "Serena"]>
```



## Erlang


```erlang
%% Implemented by Arjun Sunel
-module(symdiff).
-export([main/0]).

main() ->
	SetA = sets:from_list(["John","Bob","Mary","Serena"]),
	SetB = sets:from_list(["Jim","Mary","John","Bob"]),
	AUnionB = sets:union(SetA,SetB),
	AIntersectionB = sets:intersection(SetA,SetB),
	SymmDiffAB = sets:subtract(AUnionB,AIntersectionB),
	sets:to_list(SymmDiffAB).

```


{{out}}

```txt
["Serena","Jim"]

```


=={{header|F Sharp|F#}}==

```fsharp>
 let a = set ["John"; "Bob"; "Mary"; "Serena"]
  let b = set ["Jim"; "Mary"; "John"; "Bob"];;

val a : Set<string> = set ["Bob"; "John"; "Mary"; "Serena"]
val b : Set<string> = set ["Bob"; "Jim"; "John"; "Mary"]

> (a-b) + (b-a);;
val it : Set<string> = set ["Jim"; "Serena"]
```

Or, if you don't like the infix operators:

```fsharp>
 Set.union (Set.difference a b) (Set.difference b a);;
val it : Set<string> = set ["Jim"; "Serena"]
```



## Eiffel


```Eiffel
note
	description: "Summary description for {SYMETRIC_DIFFERENCE_EXAMPLE}."
	URI: "http://rosettacode.org/wiki/Symmetric_difference"

class
	SYMETRIC_DIFFERENCE_EXAMPLE

create
	make

feature {NONE} -- Initialization

	make
		local
			a,a1,b,b1: ARRAYED_SET [STRING]
		do
			create a.make (4)
			create b.make (4)
			a.compare_objects
			b.compare_objects
			a.put ("John")
			a.put ("Bob")
			a.put ("Mary")
			a.put ("Serena")

			create a1.make (4)
			a1.copy (a)

			b.put ("Jim")
			b.put ("Mary")
			b.put ("John")
			b.put ("Bob")

			create b1.make (4)
			b1.copy (b)

		    a1.subtract (b1)
		    b.subtract (a)
		    a1.merge (b)
		    across a1 as c loop
		    	print (" " + c.item)
		    end
		end

end
```



## Factor


```factor
: symmetric-diff ( a b -- c )
    [ diff ] [ swap diff ] 2bi append ;

{ "John" "Bob" "Mary" "Serena" } { "Jim" "Mary" "John" "Bob" } symmetric-diff .
```



## Forth

GForth 0.7.0 tested.

```Forth
: elm	( n -- ; one cell per set )
	[ cell 8 * 1- ] literal umin CREATE 1 swap lshift ,
DOES> 	( -- 2^n ) @ ;

: universe	( u "name" -- )
	dup 0 DO I elm latest swap LOOP
	CREATE dup , 0 DO , LOOP
	DOES>  ( n a -- )  dup @ tuck cells +
		swap 0
		DO	( n a' )
			over I rshift 1 AND
			IF dup @ name>string space type THEN
			1 cells -
		LOOP	2drop ;

5 universe john bob mary serena jim	persons
john bob mary serena or or or
jim mary john bob    or or or

2dup xor           persons
2dup -1 xor and cr persons
swap -1 xor and cr persons
cr bye

```

Output:

```txt

$ gforth wrk.fs
 serena jim
 serena
 jim
$

```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program Symmetric_difference
implicit none

  character(6) :: a(4) = (/ "John  ", "Bob   ", "Mary  ", "Serena" /)
  character(6) :: b(4) = (/ "Jim   ", "Mary  ", "John  ", "Bob   " /)
  integer :: i, j

outer1: do i = 1, size(a)
          do j = 1, i-1
            if(a(i) == a(j)) cycle outer1   ! Do not check duplicate items
          end do
          if(.not. any(b == a(i))) write(*,*) a(i)
        end do outer1

outer2: do i = 1, size(b)
          do j = 1, i-1
            if(b(i) == b(j)) cycle outer2   ! Do not check duplicate items
          end do
          if(.not. any(a == b(i))) write(*,*) b(i)
        end do outer2

end program
```

Output

```txt

Serena
Jim

```



## GAP


```gap
SymmetricDifference := function(a, b)
  return Union(Difference(a, b), Difference(b, a));
end;

a := ["John", "Serena", "Bob", "Mary", "Serena"];
b := ["Jim", "Mary", "John", "Jim", "Bob"];
SymmetricDifference(a,b);
[ "Jim", "Serena" ]
```



## Go


```go
package main

import "fmt"

var a = map[string]bool{"John": true, "Bob": true, "Mary": true, "Serena": true}
var b = map[string]bool{"Jim": true, "Mary": true, "John": true, "Bob": true}

func main() {
    sd := make(map[string]bool)
    for e := range a {
        if !b[e] {
            sd[e] = true
        }
    }
    for e := range b {
        if !a[e] {
            sd[e] = true
        }
    }
    fmt.Println(sd)
}
```

Output:

```txt

map[Jim:true Serena:true]

```

Alternatively, the following computes destructively on a.  The result is the same.

```go
func main() {
    for e := range b {
        delete(a, e)
    }
    fmt.Println(a)
}
```



## Groovy

Solution:

```groovy
def symDiff = { Set s1, Set s2 ->
    assert s1 != null
    assert s2 != null
    (s1 + s2) - (s1.intersect(s2))
}
```

Test:

```groovy
Set a = ['John', 'Serena', 'Bob', 'Mary', 'Serena']
Set b = ['Jim', 'Mary', 'John', 'Jim', 'Bob']

assert a.size() == 4
assert a == (['Bob', 'John', 'Mary', 'Serena'] as Set)
assert b.size() == 4
assert b == (['Bob', 'Jim', 'John', 'Mary'] as Set)

def aa = symDiff(a, a)
def ab = symDiff(a, b)
def ba = symDiff(b, a)
def bb = symDiff(b, b)

assert aa.empty
assert bb.empty
assert ab == ba
assert ab == (['Jim', 'Serena'] as Set)
assert ab == (['Serena', 'Jim'] as Set)

println """
a: ${a}
b: ${b}

Symmetric Differences

### ===============

a <> a: ${aa}
a <> b: ${ab}
b <> a: ${ba}
b <> b: ${bb}


"""

Set apostles = ['Matthew', 'Mark', 'Luke', 'John', 'Peter', 'Paul', 'Silas']
Set beatles = ['John', 'Paul', 'George', 'Ringo', 'Peter', 'Stuart']
Set csny = ['Crosby', 'Stills', 'Nash', 'Young']
Set ppm = ['Peter', 'Paul', 'Mary']

def AA = symDiff(apostles, apostles)
def AB = symDiff(apostles, beatles)
def AC = symDiff(apostles, csny)
def AP = symDiff(apostles, ppm)

def BA = symDiff(beatles, apostles)
def BB = symDiff(beatles, beatles)
def BC = symDiff(beatles, csny)
def BP = symDiff(beatles, ppm)

def CA = symDiff(csny, apostles)
def CB = symDiff(csny, beatles)
def CC = symDiff(csny, csny)
def CP = symDiff(csny, ppm)

def PA = symDiff(ppm, apostles)
def PB = symDiff(ppm, beatles)
def PC = symDiff(ppm, csny)
def PP = symDiff(ppm, ppm)

assert AB == BA
assert AC == CA
assert AP == PA
assert BC == CB
assert BP == PB
assert CP == PC

println """
apostles: ${apostles}
 beatles: ${beatles}
    csny: ${csny}
     ppm: ${ppm}

Symmetric Differences

### ===============

apostles <> apostles: ${AA}
apostles <> beatles:  ${AB}
apostles <> csny:     ${AC}
apostles <> ppm:      ${AP}

beatles <> apostles:  ${BA}
beatles <> beatles:   ${BB}
beatles <> csny:      ${BC}
beatles <> ppm:       ${BP}

csny <> apostles:     ${CA}
csny <> beatles:      ${CB}
csny <> csny:         ${CC}
csny <> ppm:          ${CP}

ppm <> apostles:      ${PA}
ppm <> beatles:       ${PB}
ppm <> csny:          ${PC}
ppm <> ppm:           ${PP}
"""
```


Output:
<pre style="height:30ex;overflow:scroll;">a: [Mary, Bob, Serena, John]
b: [Mary, Bob, Jim, John]

Symmetric Differences

### ===============

a <> a: []
a <> b: [Jim, Serena]
b <> a: [Jim, Serena]
b <> b: []




apostles: [Paul, Mark, Silas, Peter, Luke, John, Matthew]
 beatles: [Paul, Stuart, Ringo, Peter, John, George]
    csny: [Crosby, Young, Nash, Stills]
     ppm: [Paul, Mary, Peter]

Symmetric Differences

### ===============

apostles <> apostles: []
apostles <> beatles:  [Mark, Silas, Stuart, Ringo, Luke, Matthew, George]
apostles <> csny:     [Paul, Crosby, Mark, Silas, Young, Peter, Luke, John, Matthew, Nash, Stills]
apostles <> ppm:      [Mark, Mary, Silas, Luke, John, Matthew]

beatles <> apostles:  [Mark, Stuart, Ringo, Silas, Luke, Matthew, George]
beatles <> beatles:   []
beatles <> csny:      [Paul, Crosby, Stuart, Ringo, Young, Peter, John, Nash, Stills, George]
beatles <> ppm:       [Mary, Stuart, Ringo, John, George]

csny <> apostles:     [Paul, Crosby, Mark, Silas, Young, Peter, Luke, John, Nash, Stills, Matthew]
csny <> beatles:      [Paul, Crosby, Stuart, Ringo, Young, Peter, John, Nash, Stills, George]
csny <> csny:         []
csny <> ppm:          [Paul, Crosby, Mary, Young, Peter, Nash, Stills]

ppm <> apostles:      [Mark, Mary, Silas, Luke, John, Matthew]
ppm <> beatles:       [Mary, Stuart, Ringo, John, George]
ppm <> csny:          [Paul, Crosby, Mary, Young, Peter, Nash, Stills]
ppm <> ppm:           []
```



## Haskell


```haskell
import Data.Set

a = fromList ["John", "Bob",  "Mary", "Serena"]
b = fromList ["Jim",  "Mary", "John", "Bob"]

(-|-) :: Ord a => Set a -> Set a -> Set a
x -|- y = (x \\ y) `union` (y \\ x)
  -- Equivalently: (x `union` y) \\ (x `intersect` y)
```

Symmetric difference:

```haskell
*Main> a -|- b
fromList ["Jim","Serena"]
```

Individual differences:

```haskell
*Main> a \\ b
fromList ["Serena"]

*Main> b \\ a
fromList ["Jim"]
```



## HicEst


```HicEst
CALL SymmDiff("John,Serena,Bob,Mary,Serena,", "Jim,Mary,John,Jim,Bob,")
CALL SymmDiff("John,Bob,Mary,Serena,", "Jim,Mary,John,Bob,")

SUBROUTINE SymmDiff(set1, set2)
  CHARACTER set1, set2, answer*50
  answer = " "
  CALL setA_setB( set1, set2, answer )
  CALL setA_setB( set2, set1, answer )
  WRITE(Messagebox,Name) answer          ! answer = "Serena,Jim," in both cases
END

SUBROUTINE setA_setB( set1, set2, differences )
  CHARACTER set1, set2, differences, a*100
  a = set1
  EDIT(Text=a, $inLeXicon=set2)     ! eg   a <= $John,Serena,$Bob,$Mary,Serena,
  EDIT(Text=a, Right="$", Mark1, Right=",", Mark2, Delete, DO) ! Serena,Serena,
  EDIT(Text=a, Option=1, SortDelDbls=a) ! Option=1: keep case;          Serena,
  differences = TRIM( differences ) // a
END
```


=={{header|Icon}} and {{header|Unicon}}==
Set operations are built into Icon/Unicon.

```Icon
procedure main()

a := set(["John", "Serena", "Bob", "Mary", "Serena"])
b := set(["Jim", "Mary", "John", "Jim", "Bob"])

showset("a",a)
showset("b",b)
showset("(a\\b) \xef (b\\a)",(a -- b) ++ (b -- a))
showset("(a\\b)",a -- b)
showset("(b\\a)",b -- a)
end


procedure showset(n,x)
writes(n," = { ")
every writes(!x," ")
write("}")
return
end
```

Sample output:

```txt
a = { Serena Mary Bob John }
b = { Mary Bob Jim John }
(a\b) ∩ (b\a) = { Serena Jim }
(a\b) = { Serena }
(b\a) = { Jim }
```



## J


```j
   A=: ~.;:'John Serena Bob Mary Serena'
   B=: ~. ;:'Jim Mary John Jim Bob'

   (A-.B) , (B-.A)   NB. Symmetric Difference
┌──────┬───┐
│Serena│Jim│
└──────┴───┘
   A (-. , -.~) B    NB. Tacit equivalent
┌──────┬───┐
│Serena│Jim│
└──────┴───┘
```

To illustrate some of the underlying mechanics used here:

```j
   A -. B            NB. items in A but not in B
┌──────┐
│Serena│
└──────┘
   A -.~ B           NB. items in B but not in A
┌───┐
│Jim│
└───┘
   A                 NB. A is a sequence without duplicates
┌────┬──────┬───┬────┐
│John│Serena│Bob│Mary│
└────┴──────┴───┴────┘
```

Here's an alternative implementation:

```j
   A (, -. [ -. -.) B
┌──────┬───┐
│Serena│Jim│
└──────┴───┘
```

Here, <code>(,)</code> contains all items from A and B and <code>([ -. -.)</code> is the idiom for set intersection, and their difference is the symmetric difference.  (Note: an individual word in a J sentence may be placed inside a parenthesis with no change in evaluation, and this can also be used for emphasis when a word might get lost.)


## Java


```java
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class SymmetricDifference {
    public static void main(String[] args) {
        Set<String> setA = new HashSet<String>(Arrays.asList("John", "Serena", "Bob", "Mary", "Serena"));
        Set<String> setB = new HashSet<String>(Arrays.asList("Jim", "Mary", "John", "Jim", "Bob"));

        // Present our initial data set
        System.out.println("In set A: " + setA);
        System.out.println("In set B: " + setB);

        // Option 1: union of differences
        // Get our individual differences.
        Set<String> notInSetA = new HashSet<String>(setB);
        notInSetA.removeAll(setA);
        Set<String> notInSetB = new HashSet<String>(setA);
        notInSetB.removeAll(setB);

        // The symmetric difference is the concatenation of the two individual differences
        Set<String> symmetricDifference = new HashSet<String>(notInSetA);
        symmetricDifference.addAll(notInSetB);

        // Option 2: union minus intersection
        // Combine both sets
        Set<String> union = new HashSet<String>(setA);
        union.addAll(setB);

        // Get the intersection
        Set<String> intersection = new HashSet<String>(setA);
        intersection.retainAll(setB);

        // The symmetric difference is the union of the 2 sets minus the intersection
        Set<String> symmetricDifference2 = new HashSet<String>(union);
        symmetricDifference2.removeAll(intersection);

        // Present our results
        System.out.println("Not in set A: " + notInSetA);
        System.out.println("Not in set B: " + notInSetB);
        System.out.println("Symmetric Difference: " + symmetricDifference);
        System.out.println("Symmetric Difference 2: " + symmetricDifference2);
    }
}
```

Output:

```txt
In set A: [Mary, Bob, Serena, John]
In set B: [Mary, Bob, Jim, John]
Not in set A: [Jim]
Not in set B: [Serena]
Symmetric Difference: [Jim, Serena]
Symmetric Difference 2: [Jim, Serena]
```



## JavaScript



### ES5


### =Iterative=


{{works with|JavaScript|1.6}}
{{works with|Firefox|1.5}}
{{works with|SpiderMonkey}} for the <code>print()</code> function.

Uses the Array function <code>unique()</code> defined [[Create a Sequence of unique elements#JavaScript|here]].

```javascript
// in A but not in B
function relative_complement(A, B) {
    return A.filter(function(elem) {return B.indexOf(elem) == -1});
}

// in A or in B but not in both
function symmetric_difference(A,B) {
    return relative_complement(A,B).concat(relative_complement(B,A));
}

var a = ["John", "Serena", "Bob", "Mary", "Serena"].unique();
var b = ["Jim", "Mary", "John", "Jim", "Bob"].unique();

print(a);
print(b);
print(symmetric_difference(a,b));
```

outputs

```txt
Bob,John,Mary,Serena
Bob,Jim,John,Mary
Serena,Jim
```


'''Clear JavaScript'''


```javascript
function Difference(A,B)
{
    var a = A.length, b = B.length, c = 0, C = [];
    for (var i = 0; i < a; i++)
     { var j = 0, k = 0;
       while (j < b && B[j] !== A[i]) j++;
       while (k < c && C[k] !== A[i]) k++;
       if (j == b && k == c) C[c++] = A[i];
     }
    return C;
}

function SymmetricDifference(A,B)
{
    var D1 = Difference(A,B), D2 = Difference(B,A),
        a = D1.length, b = D2.length;
    for (var i = 0; i < b; i++) D1[a++] = D2[i];
    return D1;
}


/* Example
   A = ['John', 'Serena', 'Bob', 'Mary', 'Serena'];
   B = ['Jim', 'Mary', 'John', 'Jim', 'Bob'];

   Difference(A,B);           // 'Serena'
   Difference(B,A);           // 'Jim'
   SymmetricDifference(A,B);  // 'Serena','Jim'
*/
```



### ES6


### =Functional=

By composition of generic functions;

```JavaScript
(() => {
    'use strict';

    const symmetricDifference = (xs, ys) =>
        union(difference(xs, ys), difference(ys, xs));


    // GENERIC FUNCTIONS ------------------------------------------------------

    // First instance of x (if any) removed from xs
    // delete_ :: Eq a => a -> [a] -> [a]
    const delete_ = (x, xs) => {
        const i = xs.indexOf(x);
        return i !== -1 ? (xs.slice(0, i)
            .concat(xs.slice(i, -1))) : xs;
    };

    //  (\\)  :: (Eq a) => [a] -> [a] -> [a]
    const difference = (xs, ys) =>
        ys.reduce((a, x) => filter(z => z !== x, a), xs);

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f => (a, b) => f.apply(null, [b, a]);

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // nub :: [a] -> [a]
    const nub = xs => {
        const mht = unconsMay(xs);
        return mht.nothing ? xs : (
            ([h, t]) => [h].concat(nub(t.filter(s => s !== h)))
        )(mht.just);
    };

    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);

    // unconsMay :: [a] -> Maybe (a, [a])
    const unconsMay = xs => xs.length > 0 ? {
        just: [xs[0], xs.slice(1)],
        nothing: false
    } : {
        nothing: true
    };

    // union :: [a] -> [a] -> [a]
    const union = (xs, ys) => {
        const sx = nub(xs);
        return sx.concat(foldl(flip(delete_), nub(ys), sx));
    };

    // TEST -------------------------------------------------------------------
    const
        a = ["John", "Serena", "Bob", "Mary", "Serena"],
        b = ["Jim", "Mary", "John", "Jim", "Bob"];

    return show(
        symmetricDifference(a, b)
    );
})();
```

{{Out}}

```JavaScript
["Serena", "Jim"]
```


### =Procedural =


```JavaScript

const symmetricDifference = (...args) => {
    let result = new Set();
    for (const x of args)
        for (const e of new Set(x))
            if (result.has(e)) result.delete(e)
    		else result.add(e);

    return [...result];
}
 // TEST -------------------------------------------------------------------
console.log(symmetricDifference(["Jim", "Mary", "John", "Jim", "Bob"],["John", "Serena", "Bob", "Mary", "Serena"]));
console.log(symmetricDifference([1, 2, 5], [2, 3, 5], [3, 4, 5]));


```

{{Out}}

```JavaScript
["Jim", "Serena"]
[1, 4, 5]

```



## jq

The following implementation of symmetric_difference(a;b) makes no
assumptions about the input lists except that neither contains null;
given these assumptions, it is quite efficient.  To workaround the
no-null requirement would be tedious but straightforward.

```jq
# The following implementation of intersection (but not symmetric_difference) assumes that the
# elements of a (and of b) are unique and do not include null:
def intersection(a; b):
  reduce ((a + b) | sort)[] as $i
    ([null, []]; if .[0] == $i then [null, .[1] + [$i]] else [$i, .[1]] end)
  | .[1] ;

def symmetric_difference(a;b):
  (a|unique) as $a | (b|unique) as $b
  | (($a + $b) | unique) - (intersection($a;$b));

```


Example:
```jq
symmetric_difference( [1,2,1,2]; [2,3] )
[1,3]
```



## Julia

{{works with|Julia|0.6}}
Built-in function.

```julia
A = ["John", "Bob", "Mary", "Serena"]
B = ["Jim", "Mary", "John", "Bob"]
@show A B symdiff(A, B)
```


{{out}}

```txt
A = String["John", "Bob", "Mary", "Serena"]
B = String["Jim", "Mary", "John", "Bob"]
symdiff(A, B) = String["Serena", "Jim"]
```



## K


```k
  A: ?("John";"Bob";"Mary";"Serena")
  B: ?("Jim";"Mary";"John";"Bob")

  A _dvl B               / in A but not in B
"Serena"
  B _dvl A               / in B but not in A
"Jim"
  (A _dvl B;B _dvl A)    / Symmetric difference
("Serena"
 "Jim")
```



## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
    val a = setOf("John", "Bob", "Mary", "Serena")
    val b = setOf("Jim", "Mary", "John", "Bob")
    println("A     = $a")
    println("B     = $b")
    val c =  a - b
    println("A \\ B = $c")
    val d = b - a
    println("B \\ A = $d")
    val e = c.union(d)
    println("A Δ B = $e")
}
```


{{out}}

```txt

A     = [John, Bob, Mary, Serena]
B     = [Jim, Mary, John, Bob]
A \ B = [Serena]
B \ A = [Jim]
A Δ B = [Serena, Jim]

```



## Lasso


```lasso

[
var(
    'a'  = array(
       'John'
      ,'Bob'
      ,'Mary'
      ,'Serena'
    )

   ,'b'  = array

);

$b->insert( 'Jim' ); // Alternate method of populating array
$b->insert( 'Mary' );
$b->insert( 'John' );
$b->insert( 'Bob' );

$a->sort( true ); // arrays must be sorted (true = ascending) for difference to work
$b->sort( true );

$a->difference( $b )->union( $b->difference( $a ) );

]

```



## Logo

{{works with|UCB Logo}}

```logo
to diff :a :b [:acc []]
  if empty? :a [output sentence :acc :b]
  ifelse member? first :a :b ~
    [output (diff butfirst :a  remove first :a :b  :acc)] ~
    [output (diff butfirst :a  :b    lput first :a :acc)]
end

make "a [John Bob Mary Serena]
make "b [Jim Mary John Bob]

show diff :a :b   ; [Serena Jim]
```



## Lua


```lua
A = { ["John"] = true, ["Bob"] = true, ["Mary"] = true, ["Serena"] = true }
B = { ["Jim"] = true, ["Mary"] = true, ["John"] = true, ["Bob"] = true }

A_B = {}
for a in pairs(A) do
    if not B[a] then A_B[a] = true end
end

B_A = {}
for b in pairs(B) do
    if not A[b] then B_A[b] = true end
end

for a_b in pairs(A_B) do
    print( a_b )
end
for b_a in pairs(B_A) do
    print( b_a )
end
```


Object-oriented approach:


```lua
SetPrototype = {
    __index = {
        union = function(self, other)
            local res = Set{}
            for k in pairs(self) do res[k] = true end
            for k in pairs(other) do res[k] = true end
            return res
        end,
        intersection = function(self, other)
            local res = Set{}
            for k in pairs(self) do res[k] = other[k] end
            return res
        end,
        difference = function(self, other)
            local res = Set{}
            for k in pairs(self) do
                if not other[k] then res[k] = true end
            end
            return res
        end,
        symmetric_difference = function(self, other)
            return self:difference(other):union(other:difference(self))
        end
    },
    -- return string representation of set
    __tostring = function(self)
        -- list to collect all elements from the set
        local l = {}
        for k in pairs(self) do l[#l+1] = k end
        return "{" .. table.concat(l, ", ") .. "}"
    end,
    -- allow concatenation with other types to yield string
    __concat = function(a, b)
        return (type(a) == 'string' and a or tostring(a)) ..
            (type(b) == 'string' and b or tostring(b))
    end
}

function Set(items)
    local _set = {}
    setmetatable(_set, SetPrototype)
    for _, item in ipairs(items) do _set[item] = true end
    return _set
end

A = Set{"John", "Serena", "Bob", "Mary", "Serena"}
B = Set{"Jim", "Mary", "John", "Jim", "Bob"}

print("Set A: " .. A)
print("Set B: " .. B)

print("\nSymm. difference (A\\B)∪(B\\A): " .. A:symmetric_difference(B))
print("Union            A∪B        : " .. A:union(B))
print("Intersection     A∩B        : " .. A:intersection(B))
print("Difference       A\\B        : " .. A:difference(B))
print("Difference       B\\A        : " .. B:difference(A))
```


'''Output:'''

    Set A: {Serena, Mary, John, Bob}
    Set B: {Mary, Jim, John, Bob}

    Symm. difference (A\B)∪(B\A): {Serena, Jim}
    Union            A∪B        : {John, Serena, Jim, Mary, Bob}
    Intersection     A∩B        : {Mary, John, Bob}
    Difference       A\B        : {Serena}
    Difference       B\A        : {Jim}


## Maple

Maple has built-in support for set operations.  Assign the sets A and B:

```Maple
A := {John, Bob, Mary, Serena};
B := {Jim, Mary, John, Bob};
```

Now compute the symmetric difference with the '''symmdiff''' command:

```Maple
symmdiff(A, B);
```

{{out}}
                         {Jim, Serena}


## Mathematica

Mathematica has built-in support for operations on sets, using its generic symbolic lists. This function finds the entries in each list that are not present in the intersection of the two lists.

```Mathematica
SymmetricDifference[x_List,y_List] := Join[Complement[x,Intersection[x,y]],Complement[y,Intersection[x,y]]]
```

For large lists, some performance improvement could be made by caching the intersection of the two lists to avoid computing it twice:

```Mathematica
CachedSymmetricDifference[x_List,y_List] := Module[{intersect=Intersection[x,y]},Join[Complement[x,intersect],Complement[y,intersect]]]
```

Also, due to Mathematica's symbolic nature, these functions are automatically applicable to lists of any content, such as strings, integers, reals, graphics, or undefined generic symbols (e.g. unassigned variables).


## MATLAB

If you are using a vector of numbers as the sets of which you like to find the symmetric difference, then there are already utilities that operate on these types of sets built into MATLAB. This code will take the symmetric difference of two vectors:

```MATLAB>>
 [setdiff([1 2 3],[2 3 4]) setdiff([2 3 4],[1 2 3])]

ans =

     1     4
```

On the other hand, if you are using cell-arrays as sets, there are no built-in set utilities to operate on those data structures, so you will have to program them yourself. Also, the only way to have a set of strings is to put each string in a cell of a cell array, trying to put them into a vector will cause all of the strings to concatenate.

This code will return the symmetric difference of two sets and will take both cell arrays and vectors (as in the above example) as inputs.


```MATLAB
function resultantSet = symmetricDifference(set1,set2)

    assert( ~xor(iscell(set1),iscell(set2)), 'Both sets must be of the same type, either cells or matricies, but not a combination of the two' );
%% Helper function definitions

    %Define what set equality means for cell arrays
    function trueFalse = equality(set1,set2)
        if xor(iscell(set1),iscell(set2)) %set1 or set2 is a set and the other isn't
            trueFalse = false;
            return
        elseif ~(iscell(set1) || iscell(set2)) %set1 and set2 are not sets
            if ischar(set1) && ischar(set2) %set1 and set2 are chars or strings
                trueFalse = strcmp(set1,set2);
            elseif xor(ischar(set1),ischar(set2)) %set1 or set2 is a string but the other isn't
                trueFalse = false;
            else %set1 and set2 are not strings
                if numel(set1) == numel(set2) %Since they must be matricies if the are of equal cardinality then they can be compaired
                    trueFalse = all((set1 == set2));
                else %If they aren't of equal cardinality then they can't be equal
                    trueFalse = false;
                end
            end
            return
        else %set1 and set2 are both sets

            for x = (1:numel(set1))
                trueFalse = false;
                for y = (1:numel(set2))

                    %Compair the current element of set1 with every element
                    %in set2
                    trueFalse = equality(set1{x},set2{y});

                    %If the element of set1 is equal to the current element
                    %of set2 remove that element from set2 and break out of
                    %this inner loop
                    if trueFalse
                        set2(y) = [];
                        break
                    end
                end

                %If the loop completes without breaking then the current
                %element of set1 is not contained in set2 therefore the two
                %sets are not equal and we can return an equality of false
                if (~trueFalse)
                    return
                end
            end

            %If, after checking every element in both sets, there are still
            %elements in set2 then the two sets are not equivalent
            if ~isempty(set2)
                trueFalse = false;
            end
            %If the executation makes it here without the previous if
            %statement evaluating to true, then this function will return
            %true.
        end
    end %equality

    %Define the relative complement for cell arrays
    function set1 = relativeComplement(set1,set2)

        for k = (1:numel(set2))

            if numel(set1) == 0
                return
            end

            j = 1;
            while j <= numel(set1)
                if equality(set1{j},set2{k})
                    set1(j) = [];
                    j = j-1;
                end
                j = j+1;
            end
        end
    end %relativeComplement

%% The Symmetric Difference Algorithm
    if iscell(set1) && iscell(set2)
        resultantSet = [relativeComplement(set1,set2) relativeComplement(set2,set1)];
    else
        resultantSet = [setdiff(set1,set2) setdiff(set2,set1)];
    end

    resultantSet = unique(resultantSet); %Make sure there are not duplicates

end %symmetricDifference
```

Solution Test:

```MATLAB>>
 A = {'John','Bob','Mary','Serena'}

A =

    'John'    'Bob'    'Mary'    'Serena'

>> B = {'Jim','Mary','John','Bob'}

B =

    'Jim'    'Mary'    'John'    'Bob'

>> symmetricDifference(A,B)

ans =

    'Serena'    'Jim' %Correct

>> symmetricDifference([1 2 3],[2 3 4])

ans =

     1     4 %Correct
```



## Maxima


```maxima
/* builtin */
symmdifference({"John", "Bob", "Mary", "Serena"},
               {"Jim", "Mary", "John", "Bob"});
{"Jim", "Serena"}
```



## Mercury


```mercury
:- module symdiff.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, set, string.

main(!IO) :-
    A = set(["John", "Bob", "Mary", "Serena"]),
    B = set(["Jim", "Mary", "John", "Bob"]),
    print_set("A\\B", DiffAB @ (A `difference` B), !IO),
    print_set("B\\A", DiffBA @ (B `difference` A), !IO),
    print_set("A symdiff B", DiffAB `union` DiffBA, !IO).

:- pred print_set(string::in, set(T)::in, io::di, io::uo) is det.

print_set(Desc, Set, !IO) :-
   to_sorted_list(Set, Elems),
   io.format("%11s: %s\n", [s(Desc), s(string(Elems))], !IO).
```



## Nim


```nim
import sets

var setA = ["John", "Bob", "Mary", "Serena"].toSet
var setB = ["Jim", "Mary", "John", "Bob"].toSet
echo setA -+- setB # Symmetric difference
echo setA - setB # Difference
echo setB - setA # Difference
```

Output:

```txt
{Serena, Jim}
{Serena}
{Jim}
```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h


int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSSet* setA = [NSSet setWithObjects:@"John", @"Serena", @"Bob", @"Mary", @"Serena", nil];
    NSSet* setB = [NSSet setWithObjects:@"Jim", @"Mary", @"John", @"Jim", @"Bob", nil];

    // Present our initial data set
    NSLog(@"In set A: %@", setA);
    NSLog(@"In set B: %@", setB);

    // Get our individual differences.
    NSMutableSet* notInSetA = [NSMutableSet setWithSet:setB];
    [notInSetA minusSet:setA];
    NSMutableSet* notInSetB = [NSMutableSet setWithSet:setA];
    [notInSetB minusSet:setB];

    // The symmetric difference is the concatenation of the two individual differences
    NSMutableSet* symmetricDifference = [NSMutableSet setWithSet:notInSetA];
    [symmetricDifference unionSet:notInSetB];

    // Present our results
    NSLog(@"Not in set A: %@", notInSetA);
    NSLog(@"Not in set B: %@", notInSetB);
    NSLog(@"Symmetric Difference: %@", symmetricDifference);

  }
  return 0;
}
```



## OCaml


```ocaml
let unique lst =
  let f lst x = if List.mem x lst then lst else x::lst in
  List.rev (List.fold_left f [] lst)

let ( -| ) a b =
  unique (List.filter (fun v -> not (List.mem v b)) a)

let ( -|- ) a b = (b -| a) @ (a -| b)
```

in the toplevel:

```ocaml
# let a = [ "John"; "Bob"; "Mary"; "Serena" ]
  and b = [ "Jim"; "Mary"; "John"; "Bob" ]
  ;;
val a : string list = ["John"; "Bob"; "Mary"; "Serena"]
val b : string list = ["Jim"; "Mary"; "John"; "Bob"]

# a -|- b ;;
- : string list = ["Jim"; "Serena"]

# a -| b ;;
- : string list = ["Serena"]

# b -| a ;;
- : string list = ["Jim"]
```



## ooRexx


```ooRexx
a = .set~of("John", "Bob", "Mary", "Serena")
b = .set~of("Jim", "Mary", "John", "Bob")
-- the xor operation is a symmetric difference
do item over a~xor(b)
   say item
end
```

Output:

```txt

Serena
Jim

```



## Oz

Oz does not have a general set data type. We can implement some basic set operations in terms of list functions and use them to define the symmetric difference:

```oz
declare
  fun {SymDiff A B}
     {Union {Diff A B} {Diff B A}}
  end

  %% implement sets in terms of lists
  fun {MakeSet Xs}
     set({Nub2 Xs nil})
  end

  fun {Diff set(A) set(B)}
     set({FoldL B List.subtract A})
  end

  fun {Union set(A) set(B)}
     set({Append A B})
  end

  %% --
  fun {Nub2 Xs Ls}
     case Xs of nil then nil
     [] X|Xr andthen {Member X Ls} then {Nub2 Xr Ls}
     [] X|Xr then X|{Nub2 Xr X|Ls}
     end
  end
in
  {Show {SymDiff
	 {MakeSet [john bob mary serena]}
	 {MakeSet [jim mary john bob]}}}
  {Show {SymDiff
	 {MakeSet [john serena bob mary serena]}
	 {MakeSet [jim mary john jim bob]}}}
```

Oz <em>does</em> have a type for finite sets of non-negative integers. This is part of the constraint programming support. For the given task, we could use it like this if we assume numbers instead of names:

```oz
declare
  fun {SymDiff A B}
     {FS.union {FS.diff A B} {FS.diff B A}}
  end

  A = {FS.value.make [1 2 3 4]}
  B = {FS.value.make [5 3 1 2]}
in
  {Show {SymDiff A B}}
```



## Pascal

{{works with|FPC 3.0.2}}

```Pascal
PROGRAM Symmetric_difference;

TYPE
  TName = (Bob, Jim, John, Mary, Serena);
  TList = SET OF TName;

PROCEDURE Put(txt : String; ResSet : TList);
VAR
  I : TName;

BEGIN
  Write(txt);
  FOR I IN ResSet DO Write(I,' ');
  WriteLn
END;

VAR
  ListA : TList = [John, Bob, Mary, Serena];
  ListB : TList = [Jim, Mary, John, Bob];

BEGIN
  Put('ListA          -> ', ListA);
  Put('ListB          -> ', ListB);
  Put('ListA >< ListB -> ', ListA >< ListB);
  Put('ListA -  ListB -> ', ListA -  ListB);
  Put('ListB -  ListA -> ', ListB -  ListA);
  ReadLn;
END.
```

{{out}}

```txt
ListA          -> Bob John Mary Serena
ListB          -> Bob Jim John Mary
ListA >< ListB -> Jim Serena
ListA -  ListB -> Serena
ListB -  ListA -> Jim
```



## PARI/GP


```parigp
sd(u,v)={
  my(r=List());
  u=vecsort(u,,8);
  v=vecsort(v,,8);
  for(i=1,#u,if(!setsearch(v,u[i]),listput(r,u[i])));
  for(i=1,#v,if(!setsearch(u,v[i]),listput(r,v[i])));
  Vec(r)
};
sd(["John", "Serena", "Bob", "Mary", "Serena"],["Jim", "Mary", "John", "Jim", "Bob"])
```



## Perl


```perl
sub symm_diff {
        # two lists passed in as references
        my %in_a = map(($_=>1), @{+shift});
        my %in_b = map(($_=>1), @{+shift});

        my @a = grep { !$in_b{$_} } keys %in_a;
        my @b = grep { !$in_a{$_} } keys %in_b;

        # return A-B, B-A, A xor B as ref to lists
        return \@a, \@b, [ @a, @b ]
}

my @a = qw(John Serena Bob  Mary Serena);
my @b = qw(Jim  Mary   John Jim  Bob   );

my ($a, $b, $s) = symm_diff(\@a, \@b);
print "A\\B: @$a\nB\\A: @$b\nSymm: @$s\n";
```



## Perl 6


```perl6>my \A = set <John Serena Bob Mary Serena
;
my \B = set <Jim Mary John Jim Bob>;

say  A ∖ B; # Set subtraction
say  B ∖ A; # Set subtraction
say  A ⊖ B; # Symmetric difference
```

{{out}}

```txt
set(Serena)
set(Jim)
set(Jim, Serena)
```



## Phix


```Phix
function Union(sequence a, sequence b)
    for i=1 to length(a) do
        if not find(a[i],b) then
            b = append(b,a[i])
        end if
    end for
    return b
end function

function Difference(sequence a, sequence b)
sequence res = {}
    for i=1 to length(a) do
        if not find(a[i],b)
        and not find(a[i],res) then
            res = append(res,a[i])
        end if
    end for
    return res
end function

function Symmetric_Difference(sequence a, sequence b)
    return Union(Difference(a, b), Difference(b, a))
end function

sequence a = {"John", "Serena", "Bob", "Mary", "Serena"},
         b = {"Jim", "Mary", "John", "Jim", "Bob"}
?Symmetric_Difference(a,a)
?Symmetric_Difference(a,b)
?Symmetric_Difference(b,a)
?Symmetric_Difference(b,b)
```

{{Out}}

```txt

{}
{"Jim","Serena"}
{"Serena","Jim"}
{}

```



## PHP


```php
<?php
$a = array('John', 'Bob', 'Mary', 'Serena');
$b = array('Jim', 'Mary', 'John', 'Bob');

// Remove any duplicates
$a = array_unique($a);
$b = array_unique($b);

// Get the individual differences, using array_diff()
$a_minus_b = array_diff($a, $b);
$b_minus_a = array_diff($b, $a);

// Simply merge them together to get the symmetric difference
$symmetric_difference = array_merge($a_minus_b, $b_minus_a);

// Present our results.
echo 'List A:               ', implode(', ', $a),
   "\nList B:               ", implode(', ', $b),
  "\nA \\ B:                ", implode(', ', $a_minus_b),
  "\nB \\ A:                ", implode(', ', $b_minus_a),
   "\nSymmetric difference: ", implode(', ', $symmetric_difference), "\n";
?>
```

This outputs:

```txt
List A:               John, Bob, Mary, Serena
List B:               Jim, Mary, John, Bob
A \ B:                Serena
B \ A:                Jim
Symmetric difference: Serena, Jim
```



## PicoLisp


```PicoLisp
(de symdiff (A B)
   (uniq (conc (diff A B) (diff B A))) )
```

Output:

```txt
(symdiff '(John Serena Bob Mary Serena) '(Jim Mary John Jim Bob))
-> (Serena Jim)
```



## Pike

The set type in Pike is 'multiset', that is, a value may appear multiple times and the difference operator only removes equal amounts of duplicates.

```Pike>
 multiset(string) A = (< "John", "Serena", "Bob", "Mary", "Bob", "Serena" >);
> multiset(string) B = (< "Jim", "Mary", "Mary", "John", "Bob", "Jim" >);

> A^B;
Result: (< "Bob", "Serena", "Serena", "Mary", "Jim", "Jim" >)
```

The <code>^</code> operator treats arrays like multisets.

```Pike>
 array(string) A = ({ "John", "Serena", "Bob", "Mary", "Serena", "Bob" });
> array(string) B = ({ "Jim", "Mary", "John", "Jim", "Bob", "Mary" });
> A^B;
Result: ({ "Serena", "Serena", "Bob", "Jim", "Jim", "Mary"})

> Array.uniq((A-B)+(B-A));
Result: ({ "Serena", "Jim" })
```

Set operations are also possible with mappings. Here the difference operator works as expected:

```Pike>
 mapping(string:int) A = ([ "John":1, "Serena":1, "Bob":1, "Mary":1 ]);
> mapping(string:int) B = ([ "Jim":1, "Mary":1, "John":1, "Bob":1 ]);

> A^B;
Result: ([ "Jim": 1, "Serena": 1 ])
```

Lastly, there is a Set class.

```Pike>
 ADT.Set A = ADT.Set((< "John", "Serena", "Bob", "Mary", "Serena", "Bob" >));
> ADT.Set B = ADT.Set((< "Jim", "Mary", "John", "Jim", "Bob", "Mary" >));
> (A-B)+(B-A);
Result: ADT.Set({ "Serena", "Jim" })
```



## PL/I


```pli
/* PL/I ***************************************************************
* 17.08.2013 Walter Pachl
**********************************************************************/
*process source attributes xref;
 sd: Proc Options(main);
 Dcl a(4) Char(20) Var Init('John','Bob','Mary','Serena');
 Dcl b(4) Char(20) Var Init('Jim','Mary','John','Bob');
 Call match(a,b);
 Call match(b,a);
 match: Proc(x,y);
 Dcl (x(*),y(*)) Char(*) Var;
 Dcl (i,j) Bin Fixed(31);
 Do i=1 To hbound(x);
   Do j=1 To hbound(y);
     If x(i)=y(j) Then Leave;
     End;
   If j>hbound(y) Then
     Put Edit(x(i))(Skip,a);
   End;
 End;
 End;
```

Output:

```txt

Serena
Jim

```



## PowerShell


```powershell
$A = @( "John"
        "Bob"
        "Mary"
        "Serena" )

$B = @( "Jim"
        "Mary"
        "John"
        "Bob" )

#  Full commandlet name and full parameter names
Compare-Object -ReferenceObject $A -DifferenceObject $B

#  Same commandlet using an alias and positional parameters
Compare $A $B

#  A - B
Compare $A $B | Where SideIndicator -eq "<=" | Select -ExpandProperty InputObject

#  B - A
Compare $A $B | Where SideIndicator -eq "=>" | Select -ExpandProperty InputObject
```

{{out}}

```txt
InputObject SideIndicator
----------- -------------
Jim         =>
Serena      <=

InputObject SideIndicator
----------- -------------
Jim         =>
Serena      <=

Serena

Jim
```



## Prolog

{{Works with|SWI-Prolog}}

```Prolog
sym_diff :-
    A = ['John', 'Serena', 'Bob', 'Mary', 'Serena'],
    B = ['Jim', 'Mary', 'John', 'Jim', 'Bob'],
    format('A : ~w~n', [A]),
    format('B : ~w~n', [B]),
    list_to_set(A, SA),
    list_to_set(B, SB),
    format('set from A : ~w~n', [SA]),
    format('set from B : ~w~n', [SB]),
    subtract(SA, SB, DAB),
    format('difference A\\B : ~w~n', [DAB]),
    subtract(SB, SA, DBA),
    format('difference B\\A : ~w~n', [DBA]),
    union(DAB, DBA, Diff),
    format('symetric difference : ~w~n', [Diff]).
```

output :

```txt
A : [John,Serena,Bob,Mary,Serena]
B : [Jim,Mary,John,Jim,Bob]
set from A : [John,Serena,Bob,Mary]
set from B : [Jim,Mary,John,Bob]
difference A\B : [Serena]
difference B\A : [Jim]
symetric difference : [Serena,Jim]
true.
```



## PureBasic


### Simple approach


```PureBasic
Dim A.s(3)
Dim B.s(3)

A(0)="John": A(1)="Bob": A(2)="Mary": A(3)="Serena"
B(0)="Jim":  B(1)="Mary":B(2)="John": B(3)="Bob"

For a=0 To ArraySize(A())    ; A-B
  For b=0 To ArraySize(B())
    If A(a)=B(b)
      Break
    ElseIf b=ArraySize(B())
      Debug A(a)
    EndIf
  Next b
Next a

For b=0 To ArraySize(B())     ; B-A
  For a=0 To ArraySize(A())
    If A(a)=B(b)
      Break
    ElseIf a=ArraySize(A())
      Debug B(b)
    EndIf
  Next a
Next b
```


### Solution using lists


```PureBasic
DataSection
  SetA:
  Data.i 4
  Data.s "John", "Bob", "Mary", "Serena"
  ; Data.i 5
  ; Data.s "John", "Serena", "Bob", "Mary", "Serena"
  SetB:
  Data.i 4
  Data.s "Jim", "Mary", "John", "Bob"
  ; Data.i 5
  ; Data.s "Jim", "Mary", "John", "Jim", "Bob"
EndDataSection

Procedure addElementsToSet(List x.s())
  ;requires the read pointer to be set prior to calling by using 'Restore'
  Protected i, count

  Read.i count
  For i = 1 To count
    AddElement(x())
    Read.s x()
  Next
EndProcedure

Procedure displaySet(List x.s())
  Protected i, count = ListSize(x())
  FirstElement(x())
  For i = 1 To count
    Print(x())
    NextElement(x())
    If i <> count: Print(", "): EndIf
  Next
  PrintN("")
EndProcedure

Procedure symmetricDifference(List a.s(), List b.s(), List result.s())
  Protected ACount = ListSize(a()), BCount = ListSize(b()), prev.s

  ;this may leave set a and b in a different order
  SortList(a(),#PB_Sort_Ascending)
  SortList(b(),#PB_Sort_Ascending)

  FirstElement(a())
  FirstElement(b())
  LastElement(result()) ;add to end of result()
  While ACount > 0 Or BCount > 0
    If ACount <> 0 And BCount <> 0 And a() = b()
      ACount - 1: NextElement(a())
      BCount - 1: NextElement(b())
    ElseIf BCount = 0 Or (ACount <> 0 And a() < b())
      AddElement(result()): result() = a()
      prev = a(): Repeat: ACount - 1: NextElement(a()): Until ACount = 0 Or (a() <> prev)
    ElseIf ACount = 0 Or (BCount <> 0 And a() > b())
      AddElement(result()): result() = b()
      prev = b(): Repeat: BCount - 1: NextElement(b()): Until BCount = 0 Or (b() <> prev)
    EndIf
  Wend
EndProcedure

If OpenConsole()
  NewList a.s(): Restore SetA: addElementsToSet(a())
  NewList b.s(): Restore SetB: addElementsToSet(b())
  Print("Set A: "): displaySet(a())
  Print("Set B: "): displaySet(b())

  NewList sd.s()
  symmetricDifference(a(), b(), sd())
  Print("Symmetric Difference: "): displaySet(sd())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
Set A: John, Bob, Mary, Serena
Set B: Jim, Mary, John, Bob
Symmetric Difference: Jim, Serena
```



## Python

Python's <code>set</code> type supports difference as well as symmetric difference operators.

'''Python 3.x''' and '''Python 2.7''' have syntax for set literals:


```python>>>
 setA = {"John", "Bob", "Mary", "Serena"}
>>> setB = {"Jim", "Mary", "John", "Bob"}
>>> setA ^ setB # symmetric difference of A and B
{'Jim', 'Serena'}
>>> setA - setB # elements in A that are not in B
{'Serena'}
>>> setB - setA # elements in B that are not in A
{'Jim'}
>>> setA | setB # elements in A or B (union)
{'John', 'Bob', 'Jim', 'Serena', 'Mary'}
>>> setA & setB # elements in both A and B (intersection)
{'Bob', 'John', 'Mary'}
```


Note that the order of set elements is undefined.

Earlier versions of Python:


```python>>>
 setA = set(["John", "Bob", "Mary", "Serena"])
>>> setB = set(["Jim", "Mary", "John", "Bob"])
>>> setA ^ setB # symmetric difference of A and B
set(['Jim', 'Serena'])
>>> setA - setB # elements in A that are not in B
set(['Serena'])
>>> # and so on...
```


There is also a method call interface for these operations. In contrast to the operators above, they accept any iterables as arguments not just sets.


```python>>>
 setA.symmetric_difference(setB)
{'Jim', 'Serena'}
>>> setA.difference(setB)
{'Serena'}
>>> setB.difference(setA)
{'Jim'}
>>> setA.union(setB)
{'Jim', 'Mary', 'Serena', 'John', 'Bob'}
>>> setA.intersection(setB)
{'Mary', 'John', 'Bob'}
```



## R


```R
a <- c( "John", "Bob", "Mary", "Serena" )
b <- c( "Jim", "Mary", "John", "Bob" )
c(setdiff(b, a), setdiff(a, b))

a <- c("John", "Serena", "Bob", "Mary", "Serena")
b <- c("Jim", "Mary", "John", "Jim", "Bob")
c(setdiff(b, a), setdiff(a, b))
```

In both cases answer is:

```R
[1] "Jim"    "Serena"
```



## Racket



```racket

#lang racket
(define A (set "John" "Bob" "Mary" "Serena"))
(define B (set "Jim" "Mary" "John" "Bob"))

(set-symmetric-difference A B)
(set-subtract A B)
(set-subtract B A)

```



## REBOL


```rebol
a: [John Serena Bob Mary Serena]
b: [Jim Mary John Jim Bob]
difference a b
```

Result is

```txt

[Serena Jim]

```



## REXX


### version 1

This REXX version shows the symmetric difference and symmetric   ''AND''   between two lists, the lists have duplicate elements to show their proper handling.

The lists (and output) are formatted as a   '''set'''.

The   '''set'''   elements may contain any character permitted with a REXX literal, including the literal character itself (expressed as a double literal delimiter), blanks, brackets, commas, and also a   ''null''   value.

```rexx
/*REXX program finds  symmetric difference  and  symmetric AND  (between two lists).    */
a= '["John", "Serena", "Bob", "Mary", "Serena"]' /*note the duplicate element:  Serena  */
b= '["Jim", "Mary", "John", "Jim", "Bob"]'       /*  "   "       "       "      Jim     */
a.=0;   SD.=0;   SA.=0;    SD=;     SA=          /*falsify booleans; zero & nullify vars*/
a.1=a;         say '──────────────list A ='  a   /*assign a list and display it to term.*/
a.2=b;         say '──────────────list B ='  b   /*   "   "   "   "     "     "  "   "  */
                                                 /* [↓]  parse the two lists.           */
    do k=1  for 2                                /*process both lists  (stemmed array). */
    a.k=strip( strip(a.k, , "["), ,']')          /*strip leading and trailing brackets. */
               do j=1  until a.k=''              /*parse names  [they may have blanks]. */
               a.k=strip(a.k, , ',')             /*strip all commas (if there are any). */
               parse var  a.k   '"'  _  '"'  a.k /*obtain the name of the list.         */
               a.k.j=_                           /*store the name of the list.          */
               a.k._=1                           /*make a boolean value.                */
               end   /*j*/
    a.k.0=j-1                                    /*the number of this list  (of names). */
    end              /*k*/
say                                              /* [↓]  find the symmetric difference. */
    do k=1  for 2;             ko=word(2 1, k)   /*process both lists;   KO=other list. */
      do j=1  for a.k.0;       _=a.k.j           /*process the list names.              */
      if \a.ko._ & \SD._  then do;   SD._=1      /*if not in both, then  ···            */
                               SD=SD  '"'_'",'   /*add to symmetric difference list.    */
                               end
      end   /*j*/
    end     /*k*/
                                                 /* [↓]  SD ≡  symmetric difference.    */
SD= "["strip( strip(SD), 'T', ",")']'            /*clean up and add brackets [ ]  to it.*/
say 'symmetric difference ='   SD                /*display the symmetric difference.    */
                                                 /* [↓]  locate the symmetric AND.      */
   do j=1  for a.1.0;     _=a.1.j                /*process the   A   list names.        */
   if a.1._ & a.2._ & \SA._  then do;   SA._=1   /*if it's common to both, then  ···    */
                                  SA=SA '"'_'",' /*add to symmetric AND  list.          */
                                  end
   end   /*j*/
say                                              /* [↓]  SA ≡  symmetric AND.           */
SA= "["strip( strip(SA), 'T', ",")']'            /*clean up and add brackets [ ]  to it.*/
say '       symmetric AND ='   SA                /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the in-program lists:}}

```txt

──────────────list A = ["John", "Serena", "Bob", "Mary", "Serena"]
──────────────list B = ["Jim", "Mary", "John", "Jim", "Bob"]

symmetric difference = ["Serena", "Jim"]

       symmetric AND = ["John", "Bob", "Mary"]

```



### version 1.5

This REXX version shows the symmetric difference and symmetric AND between two lists, the lists have items that have imbedded blanks in them as well as some punctuation, and also a ''null'' element.

```rexx
/*REXX pgm finds symmetric difference and symm. AND (between two lists).*/
a.=0                                              /*falsify the booleans*/
a= '["Zahn", "Yi", "Stands with a Fist", "", "Hungry Wolf", "Yi"]'
b= '["Draag Ng [Jr.]", "Patzy", "Zahn", "Yi", "Robert the Bruce"]'
 ∙
 ∙
 ∙
```

{{out|output|text=  when using the in-program lists (which has imbedded blanks):}}

```txt

──────────────list A = ["Zahn", "Yi", "Stands with a Fist", "", "Hungry Wolf", "Yi"]
──────────────list B = ["Draag Ng [Jr.]", "Patzy", "Zahn", "Yi", "Robert the Bruce"]

symmetric difference = ["Stands with a Fist", "", "Hungry Wolf", "Draag Ng [Jr.]", "Patzy"]

       symmetric AND = ["Zahn", "Yi"]

```



### version 2


```rexx
/* REXX ---------------------------------------------------------------
* 14.12.2013 Walter Pachl  a short solution
* 16.12.2013 fix duplicate element problem in input
* 16.12.2013 added duplicate to t.
* Handles only sets the elements of which do not contain blanks
*--------------------------------------------------------------------*/
s='John Bob Mary Serena'
t='Jim Mary John Bob Jim '
Say difference(s,t)
Exit
difference:
Parse Arg a,b
res=''
Do i=1 To words(a)
  If wordpos(word(a,i),b)=0 Then
    Call out word(a,i)
  End
Do i=1 To words(b)
  If wordpos(word(b,i),a)=0 Then
    Call out word(b,i)
  End
Return strip(res)
out: parse Arg e
If wordpos(e,res)=0 Then res=res e
Return
```

Output:

```txt
Serena Jim
```



## Ring


```ring

alist = []
blist = []
alist = ["john", "bob", "mary", "serena"]
blist = ["jim", "mary", "john", "bob"]

alist2 = []
for i = 1 to len(alist)
    flag = 0
    for j = 1 to len(blist)
        if alist[i] = blist[j]  flag = 1 ok
    next
    if (flag = 0) add(alist2, alist[i]) ok
next

blist2 = []
for j = 1 to len(alist)
    flag = 0
    for i = 1 to len(blist)
        if alist[i] = blist[j]  flag = 1 ok
    next
    if (flag = 0) add(blist2, blist[j]) ok
next
see "a xor b :" see nl
see alist2
see blist2 see nl
see "a-b :" see nl
see alist2 see nl
see "b-a :" see nl
see blist2 see nl

```



## Ruby

With arrays:

```ruby
a = ["John", "Serena", "Bob", "Mary", "Serena"]
b = ["Jim", "Mary", "John", "Jim", "Bob"]
# the union minus the intersection:
p sym_diff = (a | b)-(a & b)  # => ["Serena", "Jim"]
```

Class Set has a symmetric difference operator built-in:

```ruby
require 'set'
a = Set["John", "Serena", "Bob", "Mary", "Serena"] #Set removes duplicates
b = Set["Jim", "Mary", "John", "Jim", "Bob"]
p sym_diff = a ^ b # => #<Set: {"Jim", "Serena"}>
```



## Run BASIC


```runbasic

setA$ = "John,Bob,Mary,Serena"
setB$ = "Jim,Mary,John,Bob"

x$ = b$(setA$,setB$)
print word$(x$,1,",")
c$ = c$ + x$

x$ = b$(setB$,setA$)
print word$(x$,1,",")
print c$;x$
end
function b$(a$,b$)
 i = 1
 while word$(a$,i,",") <> ""
  a1$ = word$(a$,i,",")
  j   = instr(b$,a1$)
  if j <> 0 then b$ = left$(b$,j-1) + mid$(b$,j+len(a1$)+1)
  i   = i + 1
wend
end function
```


```txt

Jim
Serena
Jim,Serena

```



## Scala


```Scala>scala
 val s1 = Set("John", "Serena", "Bob", "Mary", "Serena")
s1: scala.collection.immutable.Set[java.lang.String] = Set(John, Serena, Bob, Mary)

scala> val s2 = Set("Jim", "Mary", "John", "Jim", "Bob")
s2: scala.collection.immutable.Set[java.lang.String] = Set(Jim, Mary, John, Bob)

scala> (s1 diff s2) union (s2 diff s1)
res46: scala.collection.immutable.Set[java.lang.String] = Set(Serena, Jim)
```



## Scheme



###  Pure R7RS


In pure Scheme, to illustrate implementation of the algorithms:


```scheme

(import (scheme base)
        (scheme write))

;; -- given two sets represented as lists, return (A \ B)
(define (a-without-b a b)
  (cond ((null? a)
         '())
        ((member (car a) (cdr a)) ; drop head of a if it's a duplicate
         (a-without-b (cdr a) b))
        ((member (car a) b) ; head of a is in b so drop it
         (a-without-b (cdr a) b))
        (else ; head of a not in b, so keep it
          (cons (car a) (a-without-b (cdr a) b)))))

;; -- given two sets represented as lists, return symmetric difference
(define (symmetric-difference a b)
  (append (a-without-b a b)
          (a-without-b b a)))

;; -- test case
(define A '(John Bob Mary Serena))
(define B '(Jim Mary John Bob))

(display "A\\B: ") (display (a-without-b A B)) (newline)
(display "B\\A: ") (display (a-without-b B A)) (newline)
(display "Symmetric difference: ") (display (symmetric-difference A B)) (newline)
;; -- extra test as we are using lists
(display "Symmetric difference 2: ")
(display (symmetric-difference '(John Serena Bob Mary Serena)
                               '(Jim Mary John Jim Bob))) (newline)

```


{{out}}

```txt

A\B: (Serena)
B\A: (Jim)
Symmetric difference: (Serena Jim)
Symmetric difference 2: (Serena Jim)

```



###  Using a standard library


{{libheader|Scheme/SRFIs}}

SRFI 1 is one of the most popular SRFIs.  It deals with lists, but also has functions treating lists as sets.  The lset functions assume the inputs are sets, so we must delete duplicates if this property is not guaranteed on input.


```scheme

(import (scheme base)
        (scheme write)
        (srfi 1))

(define (a-without-b a b)
  (lset-difference equal?
                   (delete-duplicates a)
                   (delete-duplicates b)))

(define (symmetric-difference a b)
  (lset-xor equal?
            (delete-duplicates a)
            (delete-duplicates b)))

;; -- test case
(define A '(John Bob Mary Serena))
(define B '(Jim Mary John Bob))

(display "A\\B: ") (display (a-without-b A B)) (newline)
(display "B\\A: ") (display (a-without-b B A)) (newline)
(display "Symmetric difference: ") (display (symmetric-difference A B)) (newline)
;; -- extra test as we are using lists
(display "Symmetric difference 2: ")
(display (symmetric-difference '(John Serena Bob Mary Serena)
                               '(Jim Mary John Jim Bob))) (newline)

```




## Seed7


```seed7
$ include "seed7_05.s7i";

const type: striSet is set of string;

enable_output(striSet);

const proc: main is func
  local
    const striSet: setA is {"John", "Bob" , "Mary", "Serena"};
    const striSet: setB is {"Jim" , "Mary", "John", "Bob"   };
  begin
    writeln(setA >< setB);
  end func;
```


Output:

```txt

{Jim, Serena}

```



## Sidef


```ruby
var a = ["John", "Serena", "Bob", "Mary", "Serena"];
var b = ["Jim", "Mary", "John", "Jim", "Bob"];
a ^ b -> unique.dump.say;
```

{{out}}

```txt

["Serena", "Jim"]

```



## Smalltalk


```smalltalk
|A B|
A := Set new.
B := Set new.
A addAll: #( 'John' 'Bob' 'Mary' 'Serena' ).
B addAll: #( 'Jim' 'Mary' 'John' 'Bob' ).

( (A - B) + (B - A) ) displayNl.
```

Output is

```txt

Set ('Jim' 'Serena' )

```


=={{header|SQL}}/{{header|PostgreSQL}}==

```SQL
create or replace function arrxor(anyarray,anyarray) returns anyarray as $$
select ARRAY(
        (
        select r.elements
        from    (
                (select 1,unnest($1))
                union all
                (select 2,unnest($2))
                ) as r (arr, elements)
        group by 1
        having min(arr) = max(arr)
        )
)
$$ language sql strict immutable;
```

Usage:

```sql
select arrxor('{this,is,a,test}'::text[],'{also,part,of,a,test}'::text[]);
```

Output:

```txt

          arrxor
------------------------
 also,is,of,part,this

```



## Swift

Swift's <code>Set</code> type supports difference as well as symmetric difference operators.
{{works with|Swift|1.2+}}

```swift>let setA : Set<String
 = ["John", "Bob", "Mary", "Serena"]
let setB : Set<String> = ["Jim", "Mary", "John", "Bob"]
println(setA.exclusiveOr(setB)) // symmetric difference of A and B
println(setA.subtract(setB)) // elements in A that are not in B
```

{{out}}

```txt

["Jim", "Serena"]
["Serena"]

```



## Tcl

It's common to represent sets as an unordered list of elements. (It is also the most efficient representation.) The <code>struct::set</code> package contains operations for working on such sets-as-lists.
{{tcllib|struct::set}}

```tcl
package require struct::set

set A {John Bob Mary Serena}
set B {Jim Mary John Bob}

set AnotB   [struct::set difference $A $B]
set BnotA   [struct::set difference $B $A]
set SymDiff [struct::set union $AnotB $BnotA]

puts "A\\B = $AnotB"
puts "B\\A = $BnotA"
puts "A\u2296B = $SymDiff"

# Of course, the library already has this operation directly...
puts "Direct Check: [struct::set symdiff $A $B]"
```

Produces this output:
```txt

A\B = Serena
B\A = Jim
A⊖B = Jim Serena
Direct Check: Jim Serena

```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
a="John'Bob'Mary'Serena"
b="Jim'Mary'John'Bob"

DICT names CREATE

SUBMACRO checknames
!var,val
PRINT val,": ",var
 LOOP n=var
  DICT names APPEND/QUIET n,num,cnt,val;" "
 ENDLOOP
ENDSUBMACRO

CALL checknames (a,"a")
CALL checknames (b,"b")

DICT names UNLOAD names,num,cnt,val

LOOP n=names,v=val
PRINT n," in: ",v
ENDLOOP
```

Output:

```txt

a: John'Bob'Mary'Serena
b: Jim'Mary'John'Bob
John in: a b
Bob in: a b
Mary in: a b
Serena in: a
Jim in: b

```



## UNIX Shell

{{works with|Bash}}

```bash
uniq() {
  u=("$@")
  for ((i=0;i<${#u[@]};i++)); do
    for ((j=i+1;j<=${#u[@]};j++)); do
      [ "${u[$i]}" = "${u[$j]}" ] && unset u[$i]
    done
  done
  u=("${u[@]}")
}

a=(John Serena Bob Mary Serena)
b=(Jim Mary John Jim Bob)

uniq "${a[@]}"
au=("${u[@]}")
uniq "${b[@]}"
bu=("${u[@]}")

ab=("${au[@]}")
for ((i=0;i<=${#au[@]};i++)); do
  for ((j=0;j<=${#bu[@]};j++)); do
    [ "${ab[$i]}" = "${bu[$j]}" ] && unset ab[$i]
  done
done
ab=("${ab[@]}")

ba=("${bu[@]}")
for ((i=0;i<=${#bu[@]};i++)); do
  for ((j=0;j<=${#au[@]};j++)); do
    [ "${ba[$i]}" = "${au[$j]}" ] && unset ba[$i]
  done
done
ba=("${ba[@]}")

sd=("${ab[@]}" "${ba[@]}")

echo "Set A = ${a[@]}"
echo "      = ${au[@]}"
echo "Set B = ${b[@]}"
echo "      = ${bu[@]}"
echo "A - B = ${ab[@]}"
echo "B - A = ${ba[@]}"
echo "Symmetric difference = ${sd[@]}"
```

Output:

```txt
Set A = John Serena Bob Mary Serena
      = John Bob Mary Serena
Set B = Jim Mary John Jim Bob
      = Mary John Jim Bob
A - B = Serena
B - A = Jim
Symmetric difference = Serena Jim
```



## Ursala


```Ursala
a = <'John','Bob','Mary','Serena'>
b = <'Jim','Mary','John','Bob'>

#cast %sLm

main =

<
   'a': a,
   'b': b,
   'a not b': ~&j/a b,
   'b not a': ~&j/b a,
   'symmetric difference': ~&jrljTs/a b>
```

output:

```txt
<
   'a': <'John','Bob','Mary','Serena'>,
   'b': <'Jim','Mary','John','Bob'>,
   'a not b': <'Serena'>,
   'b not a': <'Jim'>,
   'symmetric difference': <'Jim','Serena'>>
```



## Yabasic


```Yabasic
lista1$ = "John Serena Bob Mary Serena"
lista2$ = "Jim Mary John Jim Bob"

lista1$ = quitadup$(lista1$)
lista2$ = quitadup$(lista2$)
res$ = quitacomun$(lista1$, lista2$)
res$ = res$ + quitacomun$(lista2$, lista1$)
print res$


sub quitadup$(l$)
	l$ = l$ + " "
	return quitarep$(l$)
end sub


sub quitacomun$(l1$, l2$)
	l1$ = l1$ + " "
	l2$ = l2$ + " "
	return quitarep$(l1$, l2$)
end sub


sub quitarep$(l1$, l2$)
	local pos, n, x, listar$, nombre$, largo

	largo = len(l1$)
	pos = 1
	while(true)
		n = instr(l1$, " ", pos)
		if n > 0 then
			nombre$ = mid$(l1$, pos, n-pos)
			if numparams = 1 then
				x = instr(listar$, nombre$)
			else
				x = instr(l2$, nombre$)
			end if
			if x = 0 listar$ = listar$ + nombre$ + " "
			pos = n + 1
		else
			return listar$
		end if
	wend
end sub

```



## zkl


```zkl
fcn setCommon(list1,list2){ list1.filter(list2.holds); }
fcn sdiff(list1,list2)
   { list1.extend(list2).copy().removeEach(setCommon(list1,list2)) }
```


```zkl
a:=T("John","Bob","Mary","Serena");
b:=T("Jim","Mary","John","Bob");
sdiff(a,b).println();
```

To deal with duplicates, use [[Remove duplicate elements#zkl]]:

```zkl
a:=T("John", "Serena", "Bob", "Mary", "Serena");
b:=T("Jim", "Mary", "John", "Jim", "Bob");
sdiff(a,b) : Utils.Helpers.listUnique(_).println();
```

{{out}}

```txt

L("Serena","Jim")
L("Serena","Jim")

```

