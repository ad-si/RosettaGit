+++
title = "JortSort"
description = ""
date = 2019-04-30T06:32:43Z
aliases = []
[extra]
id = 18815
[taxonomies]
categories = ["Sorting", "Algorithms", "Humor", "task"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "elixir",
  "factor",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "k",
  "kotlin",
  "lua",
  "maple",
  "mathematica",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "ssem",
  "swift",
  "tcl",
  "vbscript",
  "zkl",
]
+++

## Task

{{noticebox||Note: jortSort is considered a work of satire. It achieves its result in an intentionally roundabout way. You are encouraged to write your solutions in the spirit of the original rather than trying to give the most concise or idiomatic solution.}}
jortSort is a sorting toolset that makes the user do the work and guarantees efficiency because you don't have to sort ever again. It was originally presented by Jenn "Moneydollars" Schiffer at the prestigious [https://www.youtube.com/watch?v=pj4U_W0OFoE JSConf].

jortSort is a function that takes a single array of comparable objects as its argument. It then sorts the array in ascending order and compares the sorted array to the originally provided array. If the arrays match (i.e. the original array was already sorted), the function returns true. If the arrays do not match (i.e. the original array was not sorted), the function returns false.


## Ada



```Ada
with Ada.Text_IO, Ada.Containers.Generic_Array_Sort;

procedure Jortsort is

   function Jort_Sort(List: String) return Boolean is
      procedure Sort is new Ada.Containers.Generic_Array_Sort
	(Positive, Character, Array_Type => String);
      Second_List: String := List;
   begin
      Sort(Second_List);
      return Second_List = List;
   end Jort_Sort;

   use Ada.Text_IO;
begin
   Put_Line("""abbigail"" sorted: " & Boolean'Image(Jort_Sort("abbigail")));
   Put_Line("""abbey"" sorted: " & Boolean'Image(Jort_Sort("abbey")));
end Jortsort;
```


{{out}}


```txt
"abbigail" sorted: FALSE
"abbey" sorted: TRUE
```



## AutoHotkey


```AutoHotkey
JortSort(Array){
	sorted:=[]
	for index, val in Array
		sorted[val]:=1
	for key, val in sorted
		if (key<>Array[A_Index])
			return 0
	return 1
}
```

Examples:
```AutoHotkey
Array1 := ["a", "d", "b" , "c"]
Array2 := ["a", "b", "c" , "d"]
MsgBox % JortSort(Array1) "`n" JortSort(Array2)
return
```

Outputs:
```txt
0
1
```



## C

{{works with|GCC}}
This program tells if an array of integers numbers passed as input is sorted or not and gives the user some unpolite answers so, as asked by the specifications, "you don't have to sort ever again".
As others did in this page, this example doesn't follow the request to sort the input array and then compare the sorted version to the original one to check if it was already sorted. It only checks if the input is sorted and behaves accordingly.
I've tested this code only with gcc but should work with any C compiler.

```c

#include <stdio.h>
#include <stdlib.h>



int number_of_digits(int x){
    int NumberOfDigits;
    for(NumberOfDigits=0;x!=0;NumberOfDigits++){
        x=x/10;
    }
    return NumberOfDigits;
}

int* convert_array(char array[], int NumberOfElements)  //converts integer arguments from char to int
{
    int *convertedArray=malloc(NumberOfElements*sizeof(int));
    int originalElement, convertedElement;

    for(convertedElement=0, originalElement=0; convertedElement<NumberOfElements; convertedElement++)
    {
       convertedArray[convertedElement]=atoi(&array[originalElement]);
       originalElement+=number_of_digits(convertedArray[convertedElement])+1; //computes where is the beginning of the next element

    }
    return convertedArray;
}



int isSorted(int array[], int numberOfElements){
    int sorted=1;
    for(int counter=0;counter<numberOfElements;counter++){
        if(counter!=0 && array[counter-1]>array[counter]) sorted--;

    }
    return sorted;
}
int main(int argc, char* argv[])
{
    int* convertedArray;


    convertedArray=convert_array(*(argv+1), argc-1);



    if(isSorted(convertedArray, argc-1)==1) printf("Did you forgot to turn on your brain?! This array is already sorted!\n");
    else if(argc-1<=10) printf("Am I really supposed to sort this? Sort it by yourself!\n");
    else printf("Am I really supposed to sort this? Bhahahaha!\n");
    free(convertedArray);
    return 0;



}

```



## C++


```cpp

#include <algorithm>
#include <string>
#include <iostream>
#include <iterator>

class jortSort {
public:
    template<class T>
    bool jort_sort( T* o, size_t s ) {
        T* n = copy_array( o, s );
        sort_array( n, s );
        bool r = false;

        if( n ) {
            r = check( o, n, s );
            delete [] n;
        }
        return r;
    }

private:
    template<class T>
    T* copy_array( T* o, size_t s ) {
        T* z = new T[s];
        memcpy( z, o, s * sizeof( T ) );
        //std::copy( o, o + s, z );
        return z;
    }
    template<class T>
    void sort_array( T* n, size_t s ) {
        std::sort( n, n + s );
    }
    template<class T>
    bool check( T* n, T* o, size_t s ) {
        for( size_t x = 0; x < s; x++ )
            if( n[x] != o[x] ) return false;
        return true;
    }
};

jortSort js;

template<class T>
void displayTest( T* o, size_t s ) {
    std::copy( o, o + s, std::ostream_iterator<T>( std::cout, " " ) );
    std::cout << ": -> The array is " << ( js.jort_sort( o, s ) ? "sorted!" : "not sorted!" ) << "\n\n";
}

int main( int argc, char* argv[] ) {
    const size_t s = 5;
    std::string oStr[] = { "5", "A", "D", "R", "S" };
    displayTest( oStr, s );
    std::swap( oStr[0], oStr[1] );
    displayTest( oStr, s );

    int oInt[] = { 1, 2, 3, 4, 5 };
    displayTest( oInt, s );
    std::swap( oInt[0], oInt[1] );
    displayTest( oInt, s );

    return 0;
}

```

{{out}}

```txt

5 A D R S : -> The array is sorted!
A 5 D R S : -> The array is not sorted!

1 2 3 4 5 : -> The array is sorted!
2 1 3 4 5 : -> The array is not sorted!

```


## C#
{{trans|JavaScript}}

```c#
using System;

class Program
{
  public static bool JortSort<T>(T[] array) where T : IComparable, IEquatable<T>
  {
    // sort the array
    T[] originalArray = (T[]) array.Clone();
    Array.Sort(array);

    // compare to see if it was originally sorted
    for (var i = 0; i < originalArray.Length; i++)
    {
      if (!Equals(originalArray[i], array[i]))
      {
        return false;
      }
    }

    return true;
  }
}
```



## Clojure


```clojure
(defn jort-sort [x] (= x (sort x)))
```



## Common Lisp


```lisp

(defun jort-sort (x) (equalp (copy-seq x) (sort x #'< )))

```



## Crystal


```ruby
def jort_sort(array)
  array == array.sort
end
```



## D


```d

module jortsort;

import std.algorithm : sort, SwapStrategy;

bool jortSort(T)(T[] array) {
	auto originalArray = array.dup;
	sort!("a < b", SwapStrategy.stable)(array);
	return originalArray == array;
}

unittest {
	assert(jortSort([1, 2, 3]));
	assert(!jortSort([1, 6, 3]));
	assert(jortSort(["apple", "banana", "orange"]));
	assert(!jortSort(["two", "one", "three"]));
}

```



## Elixir


```elixir
iex(1)> jortsort = fn list -> list == Enum.sort(list) end
#Function<6.90072148/1 in :erl_eval.expr/5>
iex(2)> jortsort.([1,2,3,4])
true
iex(3)> jortsort.([1,2,5,4])
false
```



## Factor


```factor
USING: kernel sorting ;
: jortsort ( seq -- ? ) dup natural-sort = ;
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Although it's possible to create generic sorting routines using macros in FreeBASIC
' here we will just use Integer arrays.

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

Function jortSort(a() As Integer) As Boolean
  ' copy the array
  Dim lb As Integer = LBound(a)
  Dim ub As Integer = UBound(a)
  Dim b(lb To ub) As Integer
  ' this could be done more quickly using memcpy
  ' but we just copy element by element here
  For i As Integer = lb To ub
    b(i) = a(i)
  Next
  ' sort "b"
  quickSort(b(), lb, ub)
  ' now compare with "a" to see if it's already sorted
  For i As Integer = lb To ub
    If a(i) <> b(i) Then Return False
  Next
  Return True
End Function

Sub printResults(a() As Integer)
  For i As Integer = LBound(a) To UBound(a)
    Print a(i); " ";
  Next
  Print " => "; IIf(jortSort(a()), "sorted", "not sorted")
End Sub

Dim a(4) As Integer = {1, 2, 3, 4, 5}
printResults(a())
Print
Dim b(4) As Integer = {2, 1, 3, 4, 5}
PrintResults(b())
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

 1  2  3  4  5  => sorted

 2  1  3  4  5  => not sorted

```



## Go


```go

package main

import (
  "log"
  "sort"
)

func main() {
  log.Println(jortSort([]int{1, 2, 1, 11, 213, 2, 4})) //false
  log.Println(jortSort([]int{0, 1, 0, 0, 0, 0}))       //false
  log.Println(jortSort([]int{1, 2, 4, 11, 22, 22}))    //true
  log.Println(jortSort([]int{0, 0, 0, 1, 2, 2}))       //true
}

func jortSort(a []int) bool {
  c := make([]int, len(a))
  copy(c, a)
  sort.Ints(a)
  for k, v := range c {
    if v == a[k] {
      continue
    } else {
      return false
    }
  }
  return true
}


```



## Haskell

For lists:

```haskell
import Data.List (sort)

jortSort :: (Ord a) => [a] -> Bool
jortSort list = list == sort list
```



## J

Ironically, in J, implementing in the spirit of the original happens to also be the most concise and idiomatic way of expressing this algorithm:

'''Solution'''

```J
   jortSort=: -: /:~
```


More in line with the spirit of the task would be:


```J
   jortSort=: assert@(-: /:~)
```


Of course, ideally, assert would be replaced with something more assertive. Perhaps deleting all storage? But even better would be to send email to your boss and leadership explaining (at great length) exactly why they are all idiots. Do enough of this and you will never have to sort again...

'''Example Usage'''

```J
   jortSort 1 2 4 3
0
   jortSort 'sux'
1
   jortSort&> 1 2 4 3;14 6 8;1 3 8 19;'ac';'sux';'CVGH';'PQRST'
0 0 1 1 1 0 1
```



## Java

Optimized version of JortSort. Even less funny. Doesn't bother with sorting, but simply returns ''true''. Very fast. Use only when you're ''absolutely sure'' that the input is already sorted. You may have to use an unoptimized version of JortSort to ascertain this.

```java
public class JortSort {
    public static void main(String[] args) {
        System.out.println(jortSort(new int[]{1, 2, 3}));
    }

    static boolean jortSort(int[] arr) {
        return true;
    }
}
```



```txt
true
```



## JavaScript


The original JavaScript implementation courtesy of the author, [https://github.com/jennschiffer/jortsort Jenn "Moneydollars" Schiffer].

```javascript
var jortSort = function( array ) {

  // sort the array
  var originalArray = array.slice(0);
  array.sort( function(a,b){return a - b} );

  // compare to see if it was originally sorted
  for (var i = 0; i < originalArray.length; ++i) {
    if (originalArray[i] !== array[i]) return false;
  }

  return true;
};
```



## jq


```jq>def jortsort: . == sort;</lang

'''Example''':

```jq
[1, "snort", "sort", [1,2], {"1":2}] | jortsort
```

{{Out}}

```sh>true</lang



## Jsish

Based on the Javascript satire.

```javascript
/* jortSort in Jsish, based on the original satire, modified for jsish */
var jortSort = function(arr:array):boolean {
    // make a copy
    var originalArray = arr.slice(0);
    // sort
    arr.sort( function(a,b) { return a - b; } );
    // compare to see if it was originally sorted
    for (var i = 0; i < originalArray.length; ++i) {
        if (originalArray[i] !== arr[i]) return false;
    }
    // yes, the data came in sorted
    return true;
};

if (Interp.conf('unitTest')) {
;    jortSort([1,2,3]);
;    jortSort([3,2,1]);
;    jortSort([1, 'snort', 'sort', [1,2], {1:2}]);
;    jortSort(['snort', 'sort', 1, [1,2], {1:2}]);
}

/*
=!EXPECTSTART!=
jortSort([1,2,3]) ==> true
jortSort([3,2,1]) ==> false
jortSort([1, 'snort', 'sort', [1,2], {1:2}]) ==> true
jortSort(['snort', 'sort', 1, [1,2], {1:2}]) ==> false
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u jortSort.jsi
[PASS] jortSort.jsi
```



## Julia


```julia

jortsort(A) = sort(A) == A

```


{{out}}

```txt

julia> jortsort([1, 2, 3])
true

julia> jortsort([1, 4, 3])
false

julia> jortsort(['a', 'b', 'c'])
true

julia> jortsort(['a', 'd', 'c'])
false


```



## K

<lang>jortsort:{x~x@<x}
```

'''Example''':
<lang>jortsort 1 2 3
```


```txt
1

```



## Kotlin


```scala
// version 1.0.6

fun <T> jortSort(a: Array<T>): Boolean {
    val b = a.copyOf()
    b.sort()
    for (i in 0 until a.size)
        if (a[i] != b[i]) return false
    return true
}

fun <T> printResults(a: Array<T>) {
    println(a.joinToString(" ") + " => " + if (jortSort(a)) "sorted" else "not sorted")
}

fun main(args: Array<String>) {
    val a = arrayOf(1, 2, 3, 4, 5)
    printResults(a)
    val b = arrayOf(2, 1, 3, 4, 5)
    printResults(b)
    println()
    val c = arrayOf('A', 'B', 'C', 'D', 'E')
    printResults(c)
    val d = arrayOf('C', 'D', 'A', 'E', 'B')
    printResults(d)
}
```


{{out}}

```txt

1 2 3 4 5 => sorted
2 1 3 4 5 => not sorted

A B C D E => sorted
C D A E B => not sorted

```



## Lua


```Lua
function copy (t)
    local new = {}
    for k, v in pairs(t) do new[k] = v end
    return new
end

function jortSort (array)
    local originalArray = copy(array)
    table.sort(array)
    for i = 1, #originalArray do
        if originalArray[i] ~= array[i] then return false end
    end
    return true
end
```



## Maple


```Maple
jortSort := proc(arr)
	local copy:
	copy := sort(Array([seq(arr[i], i=1..numelems(arr))])):
	return ArrayTools:-IsEqual(copy,arr):
end proc:
#Examples
jortSort(Array([5,6,7,2,1]));
jortSort(Array([-5,0,7,12,21]));
jortSort(Array(StringTools:-Explode("abcdefg")));
```

{{Out|Output}}

```txt
false
true
true
```




## Mathematica


```Mathematica
jortSort[list_] := list == Sort[list];
Print[jortSort[Range[100]]];
Print[jortSort[RandomSample[Range[100]]]];
```

{{out}}

```txt
True
False
```



## Objeck


```objeck
function : JortSort(elems : CompareVector) ~ Bool {
  sorted := CompareVector->New(elems);
  sorted->Sort();

  each(i : sorted) {
    if(sorted->Get(i)->Compare(elems->Get(i)) <> 0) {
      return false;
    };
  };

  return true;
}
```



## OCaml

For lists:

```ocaml
let jortSortList lst =
  lst = List.sort compare lst
```

For arrays:

```ocaml
let jortSortArray ary =
  let originalArray = Array.copy ary in
  Array.sort compare ary;
  originalArray = ary
```



## Oforth



```Oforth>: jortSort  dup sort == ;</lang


{{out}}

```txt

[ [ 1, 2, 4, 3], [1.3, 2, 3.1 ], [ 14, 6, 8], [ 'a', 'c'], [ "abc", "def" ], "abcde", "abdce" ] map(#jortSort) println
[0, 1, 0, 1, 1, 1, 0]

```



## ooRexx

{{trans|REXX}}

```oorexx
jortSort: Parse Arg list
/*---------------------------------------------------------------------
* Determine if list is sorted
* << is used to avoid numeric comparison
* 3 4e-1 is sorted
*--------------------------------------------------------------------*/
Do i=2 To words(list)
  If word(list,i)<<word(list,i-1) Then
    Leave
  End
Return (i=words(list)+1)|(list='')

```



## PARI/GP


```parigp
jortSort(v)=vecsort(v)==v
```



## Perl


```perl
sub jortsort {
  my @s=sort @_;  # Default standard string comparison
  for (0..$#s) {
    return 0 unless $_[$_] eq $s[$_];
  }
  1;
}
```

The task wants us to sort, but we could implement this by just using <tt>cmp</tt> on the input array elements, which would be faster (especially with unsorted input).


## Perl 6


```perl6
sub jort-sort { @_ eqv @_.sort }
```

Actually, there's a better internal sort that seems to work best for lists that are already completely sorted, but tends to fails for any other list. The name of this sort, <tt>[!after]</tt>, is completely opaque, so we're pretty much forced to hide it inside a subroutine to prevent widespread panic.

```perl6
sub jort-sort-more-better-sorta { [!after] @_ }
```

However, since Perl 6 has a really good inliner, there's really little point anyway in using the <tt>[!after]</tt> reduction operator directly, and <tt>jort-sort-more-better-sorta</tt> is really much more self-documenting, so please don't use the reduction operator if you can.  For example:
{{out}}

```txt
$ perl6
> [!after] <a b c>  # DON'T do it this way
True
> [!after] 1,3,2    # DON'T do it this way either
False
```

Please do your part to uphold and/or downhold our community standards.

## Phix


```Phix
type JortSort(sequence s)
    return s=sort(s)
end type
```

Then any variable or constant delared as type JortSort raises an error if used incorrectly, eg

```Phix
JortSort ok = {1,2,3}
JortSort bad = {5,4,6}
```

{{out}}

```txt

C:\Program Files (x86)\Phix\test.exw:2
type check failure, bad is {5,4,6}

```

Amusingly the compiler itself uses a variant of jortsort, in that pttree.e declares a whole bunch of ternary tree node constants for all the language keywords and builtins such as

```Phix
global constant T_while         = 336   tt_stringF("while",T_while)
```

and if you change that to 338 and try to recompile the compiler, you'll immediately get:

```txt

while should be 336(not 338)

```

It does that because at the lowest level a cmp imm is at least twice as fast as a cmp [mem], and the only other way it could know these constants at compile-time would be to build a 5000-node ternary tree, though I will concede that any sane person would have written a program to write an include file rather than hacking these things by hand.


## PicoLisp


```PicoLisp

(de jortSort (L) (= L (sort L)))
(jortSort (1 2 3))

```

{{out}}
T


## PowerShell


```PowerShell

function jortsort($a) { -not (Compare-Object $a ($a | sort) -SyncWindow 0)}
jortsort @(1,2,3)
jortsort @(2,3,1)

```

<b>Output:</b>

```txt

True
False

```



## PureBasic


```purebasic
Macro isSort(liste)
  If OpenConsole()
    Print("[ ") : ForEach liste : Print(liste+Space(1)) : Next : Print("] = ")
    If jortSort(liste) : PrintN("True") : Else : PrintN("False") : EndIf
  EndIf
EndMacro

Procedure.b jortSort(List jortS.s())
  NewList cpy.s() : CopyList(jortS(),cpy()) : SortList(cpy(),#PB_Sort_Ascending)
  ForEach jortS()
    SelectElement(cpy(),ListIndex(jortS()))
    If Not jortS()=cpy() : ProcedureReturn #False : EndIf
  Next
  ProcedureReturn #True
EndProcedure

NewList l1.s()
For i=1 To 10 : AddElement(l1()) : l1()=Chr(Random(90,65)) : Next
isSort(l1()) : SortList(l1(),#PB_Sort_Ascending) : isSort(l1())
Input()
```

{{out}}

```txt
[ A Z Q G B N E B G Y ] = False
[ A B B E G G N Q Y Z ] = True
```



## Python


```python>>>
 def jortsort(sequence):
	return list(sequence) == sorted(sequence)
>>> for data in [(1,2,4,3), (14,6,8), ['a', 'c'], ['s', 'u', 'x'], 'CVGH', 'PQRST']:
	print(f'jortsort({repr(data)}) is {jortsort(data)}')
jortsort((1, 2, 4, 3)) is False
jortsort((14, 6, 8)) is False
jortsort(['a', 'c']) is True
jortsort(['s', 'u', 'x']) is True
jortsort('CVGH') is False
jortsort('PQRST') is True
>>>
```



## Racket


```Racket
#lang racket/base
(define (jort-sort l [<? <])
  (equal? l (sort l <?)))
```


Racket's <tt>sort</tt> function is efficient in that it starts by
checking the input, so the above could be made more efficient with a
pointer equality test:

```Racket
#lang racket/base
(define (jort-sort l [<? <])
  (eq? l (sort l <?)))
```


And an explicit implementation that checks the order (note that Racket's
<tt>sort</tt> expects a “smaller-than” comparator):

```Racket
#lang racket/base
(define (jort-sort l [<? <])
  (or (null? l)
      (for/and ([x (in-list l)] [y (in-list (cdr l))])
        (not (&lt;? y x))))) ; same as (&lt;= x y) but using only &lt;?
```



## REXX

REXX has no built-in sort, so an   ''exchange sort''   is included here.

The array elements (items) may be any form of number that REXX supports, and/or they can be alphabetic characters.

### using sort


```rexx
/*REXX program  verifies  that  an array  is sorted  using  a   jortSort   algorithm.   */
parse arg $                                      /*obtain the list of numbers from C.L. */
if $=''  then $=1 2 4 3                          /*Not specified?  Then use the default.*/
say 'array items='  space($)                     /*display the list to the terminal.    */
if jortSort($)  then say  'The array is sorted.'
                else say  "The array isn't sorted."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
eSort:    procedure expose @.;                    h=@.0       /*exchange sort.*/
                    do while h>1;                 h=h%2
                      do i=1  for @.0-h;          j=i;      k=h+i
                        do  while @.k<@.j;        t=@.j;    @.j=@.k;    @.k=t
                        if h>=j  then leave;      j=j-h;    k=k-h
                        end   /*while @.k<@.j*/
                      end     /*i*/
                    end       /*while h>1*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
jortSort: parse arg x;   @.0=words(x)                         /*assign # items in list. */
                      do j=1  for @.0; !.j=word(x,j); @.j=!.j /*save a copy of original.*/
                      end   /*j*/
          call eSort                                          /*sort with exchange sort.*/
                      do k=1  for @.0
                      if !.k\==@.k  then return 0             /*the array isn't sorted. */
                      end   /*k*/
          return 1                                            /*the array is    sorted. */
```

'''output'''   when using the default input:   <tt> 1   2   4  3 </tt>

```txt

array items= 1 2 4 3
The array is not sorted.

```

'''output'''   when using the input:   <tt>   0   -0   +0   0.0e-9   1   01   001   +1   1.0   1e8 </tt>

```txt

array items= 0 -0 +0 0.0e-9 1 01 001 +1 1.0 1e8
The array is sorted.

```

'''output'''   when using the input:   <tt> cat dog eye fox gnu hog pig wombat something </tt>

```txt

array items= cat dog eye fox gnu hog pig wombat something
The array is not sorted.

```



### using comparisons

In the   ''' http://jort.technology/ '''   webpage, the   '''jortSort'''   is defined as:

jortSort checks if your inputs are sorted.

Nothing is mentioned how it does this, and it certainly doesn't say that it sorts the input to verify if it's in order.

```rexx
/*REXX program  verifies  that  an array  is sorted  using  a   jortSort   algorithm.   */
parse arg $                                      /*obtain the list of numbers from C.L. */
if $=''  then $=1 2 4 3                          /*Not specified?  Then use the default.*/
say 'array items='  space($)                     /*display the list to the terminal.    */
if jortSort($)  then say  'The array is sorted.'
                else say  "The array isn't sorted."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
jortSort: parse arg x
          p=word(x,1)
                       do j=2  to words(x);  _=word(x,j)
                       if _<p  then return 0                      /*array  isn't sorted.*/
                       p=_
                       end   /*j*/
          return 1                                                /*array  is    sorted.*/
```

'''output'''   is the same as the 1<sup>st</sup> REXX version.




## Ring


```ring

aList = [4,2,3,1]
see jortSort(aList) + nl

func jortSort array
     originalArray = array
     array = sort(array)
     for i= 1 to len(originalArray)
         if originalArray[i] != array[i] return false ok
     next
     return true

```



## Ruby


```Ruby
def jort_sort(array)
  array == array.sort
end
```


{{trans|JavaScript}}

```Ruby
def jort_sort(array)
  # sort the array
  original_array = array.dup
  array.sort!

  # compare to see if it was originally sorted
  original_array.length.times do |i|
    return false if original_array[i] != array[i]
  end

  true
end
```



## Rust

{{trans|JavaScript}}

```rust
use std::cmp::{Ord, Eq};

fn jort_sort<T: Ord + Eq + Clone>(array: Vec<T>) -> bool {
    // sort the array
    let mut sorted_array = array.to_vec();
    sorted_array.sort();

    // compare to see if it was originally sorted
    for i in 0..array.len() {
        if array[i] != sorted_array[i] {
            return false;
        }
    }

    return true;
}
```



## Scala


```scala

def jortSort[K:Ordering]( a:Array[K] ) = a.sorted.deep == a.deep

```



## Sidef


```ruby
func jort_sort(array) { array == array.sort };
```



## SSEM

This program expects to find a zero-terminated array of positive integers in sequential storage addresses beginning at address 27. If the array is correctly sorted into ascending order, the machine will halt with all accumulator bits clear; if not, it will halt with all accumulator bits set.

Like one or two of the other solutions, the SSEM implementation does not <u>first</u> sort the array and <u>then</u> test the sorted version for equality with the original (something that would probably require more storage space than we have at our disposal): it simply reads through the array in order, checking that each element is not less than the previous one. This difference should be considered an implementation detail.

There are a couple of limitations that make the program less useful than it would otherwise be. Firstly, it is essentially a single-shot application: if you want to test a second array, you will need to manually reset storage address 0 to 16411 and storage address 26 to 0. Secondly, the SSEM's modest storage capacity means that the largest array you can sort (or not sort) using this program consists of (the terminating zero, and) four integers. Subject to those provisos, however, the program should be found to meet the specification satisfactorily.

```ssem
11011000000000100000000000000000   0. -27 to c
00000000000000110000000000000000   1. Test
11101000000000000000000000000000   2. 23 to CI
10011000000001100000000000000000   3. c to 25
10011000000000100000000000000000   4. -25 to c
01011000000000010000000000000000   5. Sub. 26
00000000000000110000000000000000   6. Test
10101000000001000000000000000000   7. Add 21 to CI
00011000000000000000000000000000   8. 24 to CI
10011000000000100000000000000000   9. -25 to c
01011000000001100000000000000000  10. c to 26
00000000000000100000000000000000  11. -0 to c
10101000000000010000000000000000  12. Sub. 21
00000000000001100000000000000000  13. c to 0
00000000000000100000000000000000  14. -0 to c
00000000000001100000000000000000  15. c to 0
01101000000000000000000000000000  16. 22 to CI
11111000000000100000000000000000  17. -31 to c
00000000000001110000000000000000  18. Stop
10101000000000100000000000000000  19. -21 to c
00000000000001110000000000000000  20. Stop
10000000000000000000000000000000  21. 1
11111111111111111111111111111111  22. -1
00001000000000000000000000000000  23. 16
01001000000000000000000000000000  24. 18
```



## Swift


```Swift>func jortSort<T:Comparable
(array: [T]) -> Bool {
    return array == sorted(array)
}
```


{{trans|JavaScript}}

```Swift>func jortSort<T:Comparable
(inout array: [T]) -> Bool {

    // sort the array
    let originalArray = array
    array.sort({$0 < $1})

    // compare to see if it was originally sorted
    for var i = 0; i < originalArray.count; ++i {
        if originalArray[i] != array[i] { return false }
    }

    return true
}
```



## Tcl



```tcl

proc jortsort {args} {
    set list [lindex $args end]
    set list [list {*}$list]    ;# ensure canonical list form
    set options [lrange $args 0 end-1]
    expr {[lsort {*}$options $list] eq $list}
}

```

This supports all of the options known to the native '''lsort''' command, making it quite natural to use.  The commented line ensures it will do the right thing for any list, even if it has funny formatting because it's embedded in source:

```txt

% jortsort -decreasing -integer {  222 33  1 }
0
% jortsort -ascii {
    "1"
    {222}
    33
}
1

```



## VBScript


```vb
Function JortSort(s)
	JortSort = True
	arrPreSort = Split(s,",")
	Set arrSorted = CreateObject("System.Collections.ArrayList")
	'Populate the resorted arraylist.
	For i = 0 To UBound(arrPreSort)
		arrSorted.Add(arrPreSort(i))
	Next
	arrSorted.Sort()
	'Compare the elements of both arrays.
	For j = 0 To UBound(arrPreSort)
		If arrPreSort(j) <> arrSorted(j) Then
			JortSort = False
			Exit For
		End If
	Next
End Function

WScript.StdOut.Write JortSort("1,2,3,4,5")
WScript.StdOut.WriteLine
WScript.StdOut.Write JortSort("1,2,3,5,4")
WScript.StdOut.WriteLine
WScript.StdOut.Write JortSort("a,b,c")
WScript.StdOut.WriteLine
WScript.StdOut.Write JortSort("a,c,b")
```

{{out}}

```txt
True
False
True
False
```



## zkl

Two "solutions", a linear one and one that actually sorts.

```zkl
fcn jort(list){ False!=list.reduce(fcn(a,b){ (a>b) and return(Void.Stop,False); b }) }
```


```zkl
fcn jort(list){ list==list.copy().sort() }
```

{{out}}

```txt

zkl: jort(T(1,2,4,3))
False
zkl: jort(T(14,6,8))
False
zkl: jort(T("a","c"))
True
zkl: jort(T("s","u","x"))
True
zkl: jort("CVGH")
False
zkl: jort("PQRST")
True
zkl: var a=List(11,2,3); jort(a)
False
zkl: a
L(11,2,3)
zkl: jort(List)
True
zkl: jort(List(1))
True

```

