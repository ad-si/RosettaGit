+++
title = "Retrieving an Element of an Array"
description = ""
date = 2019-04-23T17:36:38Z
aliases = []
[extra]
id = 1599
[taxonomies]
categories = []
tags = []
+++

{{DeprecatedTask}}

'''Please do not add new code, and merge existing code to the [[Arrays]] task.'''

In this task, the goal is to retrieve an element of an [[array]].

==[[4D]]==
   ` first element
 $elem:=$array{1}

==[[ActionScript]]==

```actionscript

var arr:Array = new Array(1,2,3);
var myVar:Number = arr[1];
// the value of myVar is: 2

```


==[[Ada]]==
Array indexed by an enumerated type. Ada enumerated types are discrete non-numeric types.

```ada

type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
type Daily_Counts is array(Days) of Natural;
This_week : Daily_Counts := (200, 212, 175 220, 201, 120, 0);
Monday_Sales : Natural;
Monday_Sales := This_Week(Mon); 

```

Monday_Sales is assigned 200

==[[ALGOL 68]]==
{{trans|FORTRAN}}

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}

[[ELLA ALGOL 68]] translator has restrictions on the number of dimensions and hence cannot compile this code.

```algol68
main:(
 PROC get item = (REF [] INT array, INT index)INT:(
   array[index]
 );
 [4]INT array := (222,444,666,888);
 print((get item(array, 3), newline));

 OP INIT = (REF[]REAL array)REF[]REAL:( FOR i FROM LWB array TO UPB array DO array[i]:=0.0 OD; array);
 OP INIT = (REF[,]REAL array)REF[,]REAL:( FOR i FROM LWB array TO UPB array DO INIT array[i,] OD; array);

 [-10:20]REAL a; INIT a;    # a one-dimensional real array indexed from -10 to 20 #
 REAL x, y, z;
 [5,4]REAL p, q, r; INIT p; # two-dimensional arrays row-indexed from 1 to 5, column-indexed from 1 to 3 #
 [2,3,2,2,3,4,2]REAL f;     # a seven-dimensional array (max dimensions allowed is 7) #
   
 x := a[-5];                # gets element at index -5 #
 y := a[0];                 # gets element at index 0 #
 z := a[20];                # gets element at index 20 #
 z := p[5,2];               # gets element in row 5, column 2 #
 
 p := q;                    # gets all elements of Q into P #
 p[:,2] := a[1:5];          # gets elements at indices 1 to 5 of A into the 2nd column of P #

                            # Note: ALGOL 68 does not have the concept of a slice with a stride #

 r[1:4,] := p[2:5,];        # gets 4x4 subarray of P starting in 2nd row into 4x4 subarray of R starting in 1st row #
 
 r[3:5,] := f[1,1,1,1,,,1]  # gets a 3x4 subarray of F into a 3x4 subarray of R starting in row 3 #

)
```

Output:

```txt

       +666

```


==[[AppleScript]]==
 on getArrayValue(array, location)
     -- very important -- The list index starts at 1 not 0
     return item location in array
 end getArrayValue

==[[AutoHotkey]]==
Arrays use one-based indexing. Array0 contains the number of elements.

```autohotkey
string = abcde
StringSplit, array, string
Loop, % array0
  MsgBox, % array%A_Index%
```


==[[AWK]]==
This shows how a string is split into elements of the array a, which is iterated over, and its elements printed to stdout. Note that the order is not as original, because the array is implemented as a hash table.
 $ awk 'BEGIN{split("a b c d",a);for(i in a)print a[i]}'
 d
 a
 b
 c

==[[C]]==

```c
  int array_index(int array[], int index) {
    return array[index];
  }
```


==[[C sharp|C#]]==
  int getArrayValue( int values[], int index ) {
    return values[index];
  }

==[[C++]]==


```cpp>  template<typename T

  T array_index(T array[], size_t index) {
    return array[index];
  }
```


==[[ColdFusion]]==
 <cfset arr = ArrayNew(1)>
 <cfset arr[1] = "one">
 <cfset arr[2] = "2">
 <cfset arr[3] = 3>
 <cfset var = arr[1]>
The value of '''var''' is "one"

''ColdFusion Arrays are '''NOT''' zero-based, their index begins at '''1'''''

==[[Common Lisp]]==
   (defun array-value (array index)
     (aref array index))

==[[D]]==
Generic, template-based method. Allows retriving elements
of arrays and objects having opIndex method implemented.


```D

// GetElem.d
module GetElem;

import tango.core.Variant;
import tango.core.Traits;

/// objects must have opIndex method
template GetItemType(T) { alias ReturnTypeOf!(T.opIndex) GetItemType; }
// specialization for arrays
template GetItemType(T : T[]) { alias T GetItemType; }

GetItemType!(T) GetElem(T, U)(T array, U idx)
{   
    return array[idx];
}

```


Sample usage:

```D

import tango.io.Stdout;

import GetElem;

class SampleContainer {
    static char[][] data = [ "lazy", "fox" "jumped", "over", "dog" ];
public:
    char[] opIndex(uint pos) { return data[pos]; }
}

void main()
{   
    auto y = new SampleContainer;
    auto x =  [5, 1, 7, 3, 6, 4, 2 ];

    Stdout (GetElem(x, 3)).newline;
    Stdout (GetElem(y, 3)).newline;

    // generate exception
    Stdout (GetElem(x, -1)).newline;
}

```


==[[Delphi]]/[[Object Pascal]]/Standard [[Pascal]]==

Arrays in all the flavors of pascal can be of any valid base type, or user defined type (which are all made up of base types) and are multi-dimensional. With Delphi dynamic arrays were defined but had been used in pascal since its inception.

A Static array definition:

```pascal
 foo : array[1..10] of integer; { The base index is ONE }
```

The base index can be freely chosen:

```pascal
 foo: array[7 .. 16] of integer; { The base index is 7 }
```

Indeed, the "1 .. 10" resp. "7 .. 16" are actually ''types'': they are integer subrange types. Arrays can also be indexed by enumeration types or enumeration subrange types:

```pascal
 type
  rainbowcolor = (red, orange, yellow, green, blue, violet);
 var
  foo: array[rainbowcolor] of integer;
  bar: array[yellow .. blue] of integer;
  i: integer
 begin
  i := foo[red]; { allowed indices are red, orange, yellow, green, blue, violet }
  i := bar[green]; { allowed indices are yellow, green, blue }
 end;
```

A Dynamic Array type in Delphi:

```delphi> foo : array of integer ; // The base index is ZERO</lang

An "old school" dynamic array in the various flavors of pascal

```delphi
 foo : array[0..0] of integer;  // The base index is ZERO
```

A dynamic array in Extended Pascal:

```pascal
 type
  intarray(n: integer) = array[1 .. n] of integer; { base index 1 }
 var
  foo: ^intarray;
 begin
  new(foo, 10); { foo now has index 1 to 10 }
  i := foo[2];
  dispose(foo); { get rid of the array }
 end;
```

In the case of the static array, the compiler generates the code to allocate the required memory to hold 10 integers.

In the Delphi style ---dynamic--- array you must set its length:

```delphi
 SetLength(foo,10);  // this array will no hold 10 integers
```

In the "old school" style of dynamic arrays, you created a point to the zero length declaration and then allocated memory to it with GetMem

```pascal
 pFoo : ^Foo ;
 Foo  : array[0..0] of integer ;
```


All arrays are accessed the same way regardless of declaration method.


```pascal
 i : integer ;
 
 i := foo[n] ;
```

where n is the array index who's base is either 1 or 0 depending on how it was declared.

==[[E]]==


```e
def value := array[index]
```


==[[Erlang]]==
:''Array module work with arrays'':

 Value = array:get(Index, Array).

==[[Fortran]]==
In ANSI Fortran 90 or later:
  real, dimension(-10:20) :: a           ! a one-dimensional real array indexed from -10 to 20
  real :: x, y, z
  real, dimension(5,4) :: p, q, r        ! two-dimensional arrays row-indexed from 1 to 5, column-indexed from 1 to 3
  real, dimension(2,3,2,2,3,4,2) :: f    ! a seven-dimensional array (max dimensions allowed is 7)
    
  x = a(-5)             ! gets element at index -5
  y = a(0)              ! gets element at index 0
  z = a(20)             ! gets element at index 20
  z = p(5,2)            ! gets element in row 5, column 2
  
  p = q                 ! gets all elements of Q into P
  p(:,2) = a(1:5)       ! gets elements at indices 1 to 5 of A into the 2nd column of P
  q(1,:) = a(-10:20:10) ! gets elements at indices -10, 0, 10, and 20 into the 1st row of Q (stride of 10)
  q(1,:) = a(::10)      ! gets same elements previous assignment (implicit first and last elements, stride of 10)
  r(1:4,:) = p(2:5,:)   ! gets 4x4 subarray of P starting in 2nd row into 4x4 subarray of R starting in 1st row
  
  a(0:-10:-1) = a(5:15) ! gets elements at indices 5 through 15 and places them in elements at indices 0 through -10
                        ! (in reverse order, stride of -1)
  
  r(3:5,:) = f(1,1,1,1,:,:,1) ! gets a 3x4 subarray of F into a 3x4 subarray of R starting in row 3


==[[Groovy]]==
Define an array
 arr = ['groovy', 'is', 'a', 'great', 'language']

First element
 arr[0] // *** 'groovy'

Last element, negative indexes
 arr[-1] // *** 'language'

Ranges
 arr[-3..-1] // *** ['a', 'great', 'language']

Mix n Match
 arr[0..2, -1] // *** ['groovy', 'is', 'a', 'language']

==[[Haskell]]==

Arrays can have arbitrary bounds (not restricted to integer values, any instance of ''Ix'' will do):

 import Data.Array
 
 example = listArray (2,5) ["This", "is", "an", example"]
 
 result = example ! 4

Here, ''result'' will be equal to "an". It should be noted that in Haskell lists are often used instead of arrays.

==[[IDL]]==
  ; this is allowed:
  result = arr(5) 
  ; but this is preferred:
  result = arr[5]

The form with square brackets is preferred as it unambiguously constitutes array access, while the version with round ones can conflict with a function call if there are both a function and an array with the same name <tt>arr</tt>.

==[[J]]==

Arrays are the only way J handles data, so every program that produces a portion of a given array is relevant here. In these few examples emphasis is on the primary verb <tt>{</tt> ("from").

The natural unit of access in J is the item. (Every piece of data can be treated as a list of [zero or more] items):

    NB. Define example one-axis array, in which each item is an atom
    ]ex1=: 10+ i. 6  NB. ] means display the full array (after =: defines it)
 10 11 12 13 14 15
    4 { ex1          NB. Single specific item 
 14
    _1 { ex1         NB. Last item, using negative indexing
 15
    {: ex1           NB. {: is another way to specify the final item
 15
    0 5 2 { ex1      NB. Multiple specific items
 10 15 12
  
    NB. Two-axis array, in which each item is a one-axis array
    ]ex2=: 4 6 $ 10+i. 24 
 10 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27
 28 29 30 31 32 33
    3 0 { ex2        NB. Item selection is the same as in prior examples
 28 29 30 31 32 33
 10 11 12 13 14 15
    (<3 4) { ex2     NB. Atom selection (index list length equals array shape length)
 32
  
    NB. Four-axis array, shaped 5 by 3 by 2 by 2
    ex4=: 5 3 2 2 $ i. 60
    (<2 1) { ex4     NB. Subarray selection: table at given indexing of top two axes
 28 29
 30 31

==[[Java]]==
 Object element = array[index];

==[[Mathematica]]==

```Mathematica

 element = array[[index]]

```

First index is 1, negative indices count from the back, -1 being the last element. [ [ 0 ] ] returns the head. The [ [ ] ]-command (Part) works not only on a list, it works on everything: graphics, equations, matrices of any dimension et cetera. To dig deeper you can specifiy multiple indices [ [ a, b, c ] ] to get the c<sup>th</sup> element of the b<sup>th</sup> element of the a<sup>th</sup> element. For any level you can specify a range using ;;, i.e.: a;;b;;c would give elements a through b in steps of c. If a is not given it will assume 1, if b is not give it will be -1, if c is not given it will be 1.

==[[MAXScript]]==
 item = arr[index]

==[[mIRC Scripting Language]]==
{{works with|mIRC Script Editor}}
{{works with|mArray Snippet}}
[[Category:mArray Snippet]]

  alias readmyarray { echo -a $array_read(MyArray, 2, 3) }

==[[Modula-3]]==

```txt

VAR arr := ARRAY [1..3] OF INTEGER {1, 2, 3};
VAR myVar := a[2];

```


==[[Nial]]==
Nial is an array programming language. Thus it has a variety of ways
to retrieve elements of an (even multi-dimensional) array.

Define example one-axis array
 myarr := count 6
 = 1 2 3 4 5 6
retrieve a specific item
 myarr@0
 = 1
use pick (pick allows specifying another array as the index for a multidimensional array)
 1 pick myarr
 = 2
scheme like operations
 first myarr
 =1
 rest myarr
 =2 3 4 5 6
 front myarr
 =1 2 3 4 5

There are quite a few others (too large to list here).

==[[Objective-C]]==
{{works with|GNUstep}}

{{works with|Cocoa}}


```objc
 NSArray *array;
 //...
 id element = [array objectAtIndex:index];
```



## OCaml


```ocaml
let element = array.(index)
```


==[[Octave]]==


```octave
a = [1:10];
disp(a(3));       % display third element of the vector
disp(a(3:6));     % display elements from 3 to 6 ("subarray")
disp(a(1:2:10));  % display elements at odd indexes (1, 3, 5...)
disp(a(2:2:10));  % display elements at even indexes (2, 4, 6...)
disp(a(10:-1:1)); % display elements in reversed order
```



==[[Perl]]==
{{works with|Perl|5.8.8}} 
 $elem = $array[0];

==[[PHP]]==

```php
 $array = array('php', 'has', 'arrays');
 // First element 
 $elem  = $array[0];
```


==[[Pop11]]==
 lvars ar = {1 two 'three'};
 lvars elem;
 ;;; Access second element and assign to variable elem
 ar(2) -> elem;

This example uses the simplest possible array (a vector). Pop11 has more general arrays, but in all cases access follows the same pattern, and look the same as procedure (function) call.

==[[Python]]==
{{works with|Python|2.5}}
The item is an element in a list at a given index
  item = aList[index]

or

To use a list like a stack be it FIFO/LIFO
  aList.pop()  # Pop last item in a list
  aList.pop(0) # Pop first item in a list
'''Note:''' When using the pop() method, the element is removed from the list.

==[[R]]==

An element from a vector can be retrieved with the []; slices can be taken using ranges or another vector of indexes.


```R
a <- 1:10           # create a vector made of number from 1 to 10
print(a[1])         # print the first element
print(a[4:6])       # print elements from 4 to 6
print(a[c(1,8, 9)]) # print element 1, 8 and 9
```


Comma separated indexes can be used to index matrix, e.g.

```R
a <- matrix(c(1,2,3,4), 2, 2)
print(a[2,1])
```


==[[Ruby]]==

```ruby
  ary = ['Ruby','rules','big','time']
  #the first element
  element = ary[0]
  #or
  element = ary.first
  # => element = 'Ruby'

  #the last element
  element = ary[-1]
  #or
  element = ary.last
  # => element = 'time'

  #retrieving different values at once
  elements = ary.values_at(0,2,3)
  # => elements = ['Ruby','big','time']

  #select the first element of length 3
  element = ary.find{|el|el.length==3}
  # => element = "big"
```



## Scala

{{libheader|Scala}}

```Scala
val selectedElement = array(index)
```


==[[Slate]]==

```slate
{$a. #(1 2). 'b'. True} at: 2
```


==[[Smalltalk]]==

```smalltalk
   #($a $b $c) at: 2
```


==[[Standard ML]]==
 val element = Array.sub (array, index)

==[[Tcl]]==
All arrays in Tcl are associative. If "key" is the variable that holds the key of the element to be retrieved, then

```tcl
set result $array($key)
```

If you are looking for a collection of values that are indexed by number, you want a list. List are indexed into with the <tt>lindex</tt> command:

```tcl
set result [lindex $list $index]
```


==[[Toka]]==
This retrieves the value 20 from the second item in the array:

  3 cells is-array table
 
  ( Populate the array )
  10 0 table array.put
  20 1 table array.put
  30 2 table array.put
  
  table 1 array.get

==[[X86 assembly]]==
{{works with|nasm}}

This retrieves the '''third''' 32bit element of an array made of 4-bytes long (32bit) elements. 

```asm
 mov esi, array_offset
 mov ebx, 2
 mov eax, [esi+ebx*4]
```

