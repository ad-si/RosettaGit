+++
title = "Filter"
description = ""
date = 2019-10-18T10:07:22Z
aliases = []
[extra]
id = 1647
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "acl2",
  "actionscript",
  "ada",
  "aime",
  "algol_68",
  "amigae",
  "antlang",
  "apex",
  "apl",
  "applescript",
  "arturo",
  "autohotkey",
  "awk",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "brat",
  "burlesque",
  "c",
  "c_sharp",
  "clean",
  "clojure",
  "coffeescript",
  "common_lisp",
  "c_plus_plus",
  "d",
  "delphi",
  "dyalect",
  "e",
  "echolisp",
  "ela",
  "elena",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "free_basic",
  "frink",
  "futhark",
  "gambas",
  "gap",
  "go",
  "groovy",
  "haskell",
  "idl",
  "j",
  "java",
  "javafx_script",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lisaac",
  "logo",
  "lua",
  "m2000_interpreter",
  "maple",
  "matlab",
  "maxima",
  "maxscript",
  "min",
  "ml",
  "mlite",
  "mumps",
  "nemerle",
  "netrexx",
  "newlisp",
  "nial",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "peloton",
  "perl",
  "perl_6",
  "phix",
  "phl",
  "php",
  "picolisp",
  "pl_i",
  "postscript",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "red",
  "rebol",
  "rexx",
  "ruby",
  "rust",
  "sather",
  "scala",
  "scheme",
  "seed7",
  "smalltalk",
  "sql",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "unix_shell",
  "xpl0",
  "xquery",
  "xslt",
  "zkl",
  "zx_spectrum_basic"
]
+++

## Task

Select certain elements from an Array into a new Array in a generic way.


To demonstrate, select all even numbers from an Array.

As an option, give a second solution which filters destructively,
by modifying the original Array rather than creating a new Array.





## ACL2


```Lisp
(defun filter-evens (xs)
   (cond ((endp xs) nil)
         ((evenp (first xs))
          (cons (first xs) (filter-evens (rest xs))))
         (t (filter-evens (rest xs)))))
```



## ActionScript


```actionscript
var arr:Array = new Array(1, 2, 3, 4, 5);
var evens:Array = new Array();
for (var i:int = 0; i < arr.length(); i++) {
    if (arr[i] % 2 == 0)
        evens.push(arr[i]);
}
```


'''Actionscript 3'''

```actionscript
var arr:Array = new Array(1, 2, 3, 4, 5);
arr = arr.filter(function(item:int, index:int, array:Array) {
  return item % 2 == 0;
});

```



## Ada


```ada
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Text_Io; use Ada.Text_Io;

procedure Array_Selection is
   type Array_Type is array (Positive range <>) of Integer;
   Null_Array : Array_Type(1..0);

   function Evens (Item : Array_Type) return Array_Type is
   begin
      if Item'Length > 0 then
         if Item(Item'First) mod 2 = 0 then
            return Item(Item'First) & Evens(Item((Item'First + 1)..Item'Last));
         else
            return Evens(Item((Item'First + 1)..Item'Last));
         end if;
      else
         return Null_Array;
      end if;
   end Evens;

   procedure Print(Item : Array_Type) is
   begin
      for I in Item'range loop
         Put(Item(I));
         New_Line;
      end loop;
   end Print;

   Foo : Array_Type := (1,2,3,4,5,6,7,8,9,10);
begin
   Print(Evens(Foo));
end Array_Selection;
```

Here is a non-recursive solution:

```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Array_Selection is
   type Array_Type is array (Positive range <>) of Integer;

   function Evens (Item : Array_Type) return Array_Type is
      Result : Array_Type (1..Item'Length);
      Index  : Positive := 1;
   begin
      for I in Item'Range loop
         if Item (I) mod 2 = 0 then
            Result (Index) := Item (I);
            Index := Index + 1;
         end if;
      end loop;
      return Result (1..Index - 1);
   end Evens;

   procedure Put (Item : Array_Type) is
   begin
      for I in Item'range loop
         Put (Integer'Image (Item (I)));
      end loop;
   end Put;
begin
   Put (Evens ((1,2,3,4,5,6,7,8,9,10)));
   New_Line;
end Array_Selection;
```



## Aime


```aime
integer
even(integer e)
{
    return !(e & 1);
}

list
filter(list l, integer (*f)(integer))
{
    integer i;
    list v;

    i = 0;
    while (i < l_length(l)) {
        integer e;

        e = l_q_integer(l, i);
        if (f(e)) {
            lb_p_integer(v, e);
        }

        i += 1;
    }

    return v;
}

integer
main(void)
{
    integer i;
    list l;

    i = 0;
    while (i < 10) {
        lb_p_integer(l, i);
        i += 1;
    }

    l = filter(l, even);

    i = 0;
    while (i < l_length(l)) {
        o_space(1);
        o_integer(l_q_integer(l, i));
        i += 1;
    }
    o_byte('\n');

    return 0;
}
```

```txt
 0 2 4 6 8
```



## ALGOL 68

```algol68
MODE TYPE = INT;

PROC select = ([]TYPE from, PROC(TYPE)BOOL where)[]TYPE:
BEGIN
  FLEX[0]TYPE result;
  FOR key FROM LWB from TO UPB from DO
    IF where(from[key]) THEN
      [UPB result+1]TYPE new result;
      new result[:UPB result] := result;
      new result[UPB new result] := from[key];
      result := new result
    FI
  OD;
  result
END;

[]TYPE from values = (1,2,3,4,5,6,7,8,9,10);
PROC where even = (TYPE value)BOOL: NOT ODD value;

print((select(from values, where even), new line));

# Or as a simple one line query #
print((select((1,4,9,16,25,36,49,64,81,100), (TYPE x)BOOL: NOT ODD x ), new line))
```

```txt

         +2         +4         +6         +8        +10
         +4        +16        +36        +64       +100

```



## AmigaE


```amigae
PROC main()
  DEF l : PTR TO LONG, r : PTR TO LONG, x
  l := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  r := List(ListLen(l))
  SelectList({x}, l, r, `Mod(x,2)=0)
  ForAll({x}, r, `WriteF('\d\n', x))
ENDPROC
```



## AntLang


```AntLang
x:range[100]
{1- x mod 2}hfilter x
```



## Apex


```Apex>List<Integer> integers = new List<Integer
{1,2,3,4,5};
Set<Integer> evenIntegers = new Set<Integer>();
for(Integer i : integers)
{
    if(math.mod(i,2) == 0)
    {
        evenIntegers.add(i);
    }
}
system.assert(evenIntegers.size() == 2, 'We should only have two even numbers in the set');
system.assert(!evenIntegers.contains(1), '1 should not be a number in the set');
system.assert(evenIntegers.contains(2), '2 should be a number in the set');
system.assert(!evenIntegers.contains(3), '3 should not be a number in the set');
system.assert(evenIntegers.contains(4), '4 should be a number in the set');
system.assert(!evenIntegers.contains(5), '5 should not be a number in the set');
```



## APL


```APL
      (0=2|x)/x←⍳20
2 4 6 8	10 12 14 16 18 20
```



## AppleScript


```applescript
set array to {1, 2, 3, 4, 5, 6}
set evens to {}
repeat with i in array
	if (i mod 2 = 0) then set end of evens to i's contents
end repeat
return evens
```
Result is (a list):
```txt
{2, 4, 6}
```


Here's how you might implement a more generic filter, passing a script object to represent the test that elements must pass (obviously overkill for this simple example):


```AppleScript
to filter(inList, acceptor)
  set outList to {}
  repeat with anItem in inList
    if acceptor's accept(anItem) then
      set end of outList to contents of anItem
    end
  end
  return outList
end

script isEven
  to accept(aNumber)
    aNumber mod 2 = 0
  end accept
end script

filter({1,2,3,4,5,6}, isEven)
```



We can simplify and generalise this further by lifting any ordinary predicate handler into a script on the fly.

In this example, as with JavaScript filter lambdas, the lifted handler can optionally have one or two additional arguments:
# The index of the current element
# A reference to the whole list.


This allows for context-sensitive filters, which can take account of following or preceding elements in a sequence.


```AppleScript
-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

-- TEST -----------------------------------------------------------------------

-- isEven :: (a -> Bool)
on isEven(x)
    x mod 2 = 0
end isEven

on run

    filter(isEven, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10})

    -- {0, 2, 4, 6, 8, 10}
end run

-- GENERIC FUNCTION -----------------------------------------------------------

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
```

```txt
{0, 2, 4, 6, 8, 10}
```



## Arturo


```arturo
arr #(1 2 3 4 5 6 7 8 9 10)

print $(filter arr { even & })
```


```txt
#(2 4 6 8 10)
```



## AutoHotkey


```autohotkey
array = 1,2,3,4,5,6,7
loop, parse, array, `,
{
    if IsEven(A_LoopField)
        evens = %evens%,%A_LoopField%
}
stringtrimleft, evens, evens, 1
msgbox % evens
return

IsEven(number)
{
    return !mod(number, 2)
}


; ----- Another version: always with csv string ------
array = 1,2,3,4,5,6,7

even(s) {
	loop, parse, s, `,
		if !mod(A_LoopField, 2)
			r .= "," A_LoopField
	return SubStr(r, 2)
}

MsgBox % "Array => " array "`n" "Result => " even(array)


; ----- Yet another version: with array (requires AutoHotKey_L) ------
array2 := [1,2,3,4,5,6,7]

even2(a) {
	r := []
	For k, v in a
		if !mod(v, 2)
			r.Insert(v)
	return r
}

; Add "join" method to string object (just like python)
s_join(o, a) {
	Loop, % a.MaxIndex()
		r .= o a[A_Index]
	return SubStr(r, StrLen(o) + 1)
}
"".base.join := Func("s_join")

MsgBox % "Array => " ",".join(array2) "`n"  "Result => " ",".join(even2(array2))



```



## AWK

In this example, an array is filled with the numbers 1..9.
In a loop, even elements are collected into the string ''r''.
Note that sequence is not necessarily maintained.

'''One-liner:'''

```awk
$ awk 'BEGIN{split("1 2 3 4 5 6 7 8 9",a);for(i in a)if(!(a[i]%2))r=r" "a[i];print r}'
```

```txt
4 6 8 2
```


'''Regular script:'''

```awk

BEGIN {
  split("1 2 3 4 5 6 7 8 9",a);
  for(i in a)  if( !(a[i]%2) )  r = r" "a[i];
  print r
}

```

Same output.


## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

set numberarray=1 2 3 4 5 6 7 8 9 10
for %%i in (%numberarray%) do (
  set /a tempcount+=1
  set numberarray!tempcount!=%%i
)

echo Filtering all even numbers from numberarray into newarray...
call:filternew numberarray
echo numberarray - %numberarray%
echo newarray    -%newarray%
echo.
echo Filtering numberarray so that only even entries remain...
call:filterdestroy numberarray
echo numberarray -%numberarray%
pause>nul
exit /b

:filternew
set arrayname=%1
call:arraylength %arrayname%
set tempcount=0
for /l %%i in (1,1,%length%) do (
  set /a cond=!%arrayname%%%i! %% 2
  if !cond!==0 (
    set /a tempcount+=1
    set newarray!tempcount!=!%arrayname%%%i!
    set newarray=!newarray! !%arrayname%%%i!
  )
)
exit /b

:filterdestroy
set arrayname=%1
call:arraylength %arrayname%
set tempcount=0
set "%arrayname%="
for /l %%i in (1,1,%length%) do (
  set /a cond=!%arrayname%%%i! %% 2
  if !cond!==0 (
    set /a tempcount+=1
    set %arrayname%!tempcount!=!%arrayname%%%i!
    set %arrayname%=!%arrayname%! !%arrayname%%%i!
  )
)
exit /b

:arraylength
set tempcount=0
set lengthname=%1
set length=0
:lengthloop
set /a tempcount+=1
if "!%lengthname%%tempcount%!"=="" exit /b
set /a length+=1
goto lengthloop

```

```txt

Filtering all even numbers from numberarray into newarray...
numberarray - 1 2 3 4 5 6 7 8 9 10
newarray    - 2 4 6 8 10

Filtering numberarray so that only even entries remain...
numberarray - 2 4 6 8 10

```




## BBC BASIC


```bbcbasic
      REM Create the test array:
      items% = 1000
      DIM array%(items%)
      FOR index% = 1 TO items%
        array%(index%) = RND
      NEXT

      REM Count the number of filtered items:
      filtered% = 0
      FOR index% = 1 TO items%
        IF FNfilter(array%(index%)) filtered% += 1
      NEXT

      REM Create a new array containing the filtered items:
      DIM new%(filtered%)
      filtered% = 0
      FOR index% = 1 TO items%
        IF FNfilter(array%(index%)) THEN
          filtered% += 1
          new%(filtered%) = array%(index%)
        ENDIF
      NEXT

      REM Alternatively modify the original array:
      filtered% = 0
      FOR index% = 1 TO items%
        IF FNfilter(array%(index%)) THEN
          filtered% += 1
          array%(filtered%) = array%(index%)
        ENDIF
      NEXT
      END

      DEF FNfilter(A%) = ((A% AND 1) = 0)
```



## Bracmat


```bracmat
( :?odds
& ( 1 2 3 4 5 6 7 8 9 10 16 25 36 49 64 81 100:? (=.!sjt*1/2:/&!odds !sjt:?odds)$() ()
  | !odds
  )
)

```


```txt
1 3 5 7 9 25 49 81
```



## Brat


```brat
#Prints [2, 4, 6, 8, 10]
p 1.to(10).select { x | x % 2 == 0 }
```



## Burlesque



```burlesque

blsq ) 1 13r@{2.%n!}f[
{2 4 6 8 10 12}

```





## C


```c
#include <stdio.h>
#include <stdlib.h>

int even_sel(int x) { return !(x & 1); }
int tri_sel(int x) { return x % 3; }

/* using a predicate function sel() to select elements */
int* grep(int *in, int len, int *outlen, int (*sel)(int), int inplace)
{
	int i, j, *out;

	if (inplace)	out = in;
	else		out = malloc(sizeof(int) * len);

	for (i = j = 0; i < len; i++)
		if (sel(in[i]))
			out[j++] = in[i];

	if (!inplace && j < len)
		out = realloc(out, sizeof(int) * j);

	*outlen = j;
	return out;
}

int main()
{
	int in[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
	int i, len;

	int *even = grep(in, 10, &len, even_sel, 0);
	printf("Filtered even:");
	for (i = 0; i < len; i++) printf(" %d", even[i]);
	printf("\n");

	grep(in, 8, &len, tri_sel, 1);
	printf("In-place filtered not multiple of 3:");
	for (i = 0; i < len; i++) printf(" %d", in[i]);

	printf("\n");

	return 0;
}
```

```txt
Filtered even: 2 4 6 8 10
In-place filtered not multiple of 3: 1 2 4 5 7 8 10
```


## C#
```c#
ArrayList array = new ArrayList( new int[] { 1, 2, 3, 4, 5 } );
ArrayList evens = new ArrayList();
foreach( int i in array )
{
        if( (i%2) == 0 )
                evens.Add( i );
}
foreach( int i in evens )
       System.Console.WriteLine( i.ToString() );
```

```csharp>List<int> array = new List<int
( new int[] { 1, 2, 3, 4, 5 } );
List<int> evens = array.FindAll( delegate( int i ) { return (i%2)==0; } );
foreach( int i in evens )
       System.Console.WriteLine( i.ToString() );
```

```csharp>IEnumerable<int> array = new List<int
( new int[] { 1, 2, 3, 4, 5 } );
IEnumerable<int> evens = array.Where( delegate( int i ) { return (i%2)==0; } );
foreach( int i in evens )
       System.Console.WriteLine( i.ToString() );
```

Replacing the delegate with the more concise lambda expression syntax.

```c#
int[] array = { 1, 2, 3, 4, 5 };
int[] evens = array.Where(i => (i % 2) == 0).ToArray();

foreach (int i in evens)
    Console.WriteLine(i);
```



## C++



```cpp
#include <vector>
#include <algorithm>
#include <functional>
#include <iterator>
#include <iostream>

int main() {
  std::vector<int> ary;
  for (int i = 0; i < 10; i++)
    ary.push_back(i);
  std::vector<int> evens;
  std::remove_copy_if(ary.begin(), ary.end(), back_inserter(evens),
                      std::bind2nd(std::modulus<int>(), 2)); // filter copy
  std::copy(evens.begin(), evens.end(),
            std::ostream_iterator<int>(std::cout, "\n"));

  return 0;
}
```



```cpp
#include <vector>
#include <algorithm>
#include <iterator>
#include <iostream>

using namespace std;

int main() {
  vector<int> ary = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  vector<int> evens;

  copy_if(ary.begin(), ary.end(), back_inserter(evens),
      [](int i) { return i % 2 == 0; });

  // print result
  copy(evens.begin(), evens.end(), ostream_iterator<int>(cout, "\n"));
}
```



## Clean

The standard environment is required for list and array comprehensions. We specify the types of the functions because array comprehensions are overloaded. Clean provides lazy, strict, and unboxed arrays.


```clean
module SelectFromArray

import StdEnv
```


Create a lazy array where each element comes from the list 1 to 10.


```clean
array :: {Int}
array = {x \\ x <- [1 .. 10]}
```


Create (and print) a strict array where each element (coming from another array) is even.


```clean
Start :: {!Int}
Start = {x \\ x <-: array | isEven x}
```



## Clojure



```lisp
;; range and filter create lazy seq's
(filter even? (range 0 100))
;; vec will convert any type of seq to an array
(vec (filter even? (vec (range 0 100))))
```



## CoffeeScript


```coffee
[1..10].filter (x) -> not (x%2)
```


```txt
[ 2,
  4,
  6,
  8,
  10 ]
```



## Common Lisp

Common Lisp has many ways of accomplishing this task. Most of them involve higher-order sequence functions that take a predicate as the first argument and a list as the second argument. A predicate is a function that returns a boolean. The higher-order functions call the predicate for each element in list, testing the element.

In this example, the goal is to find the even numbers. The most straight-forward function is to use remove-if-not, which removes elements from the list that does ''not'' pass the predicate. The predicate, in this case, tests to see if an element is even. Therefore, the remove-if-not acts like a filter:


```lisp
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
> (2 4 6 8 10)
```


However, this function is non-destructive, meaning the function creates a brand new list. This might be too prohibitive for very large lists.


###  Destructive

There is a destructive version that modifies the list in-place:


```lisp
(delete-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
> (2 4 6 8 10)
```



## D


```d
void main() {
    import std.algorithm: filter, equal;

    immutable data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto evens = data.filter!(x => x % 2 == 0); // Lazy.
    assert(evens.equal([2, 4, 6, 8, 10]));
}
```


### Tango Version

```d
import tango.core.Array, tango.io.Stdout;

void main() {
    auto array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // removeIf places even elements at the beginnig of the array and returns number of found evens
    auto evens = array.removeIf( ( typeof(array[0]) i ) { return (i % 2) == 1; } );
    Stdout("Evens - ")( array[0 .. evens] ).newline; // The order of even elements is preserved
    Stdout("Odds - ")( array[evens .. $].sort ).newline; // Unlike odd elements
}
```

```txt
 Evens - [ 2, 4, 6, 8, 10 ]
 Odds - [ 1, 3, 5, 7, 9 ]
```



## Delphi


```Delphi
program FilterEven;

{$APPTYPE CONSOLE}

uses SysUtils, Types;

const
  SOURCE_ARRAY: array[0..9] of Integer = (0,1,2,3,4,5,6,7,8,9);
var
  i: Integer;
  lEvenArray: TIntegerDynArray;
begin
  for i in SOURCE_ARRAY do
  begin
    if not Odd(i) then
    begin
      SetLength(lEvenArray, Length(lEvenArray) + 1);
      lEvenArray[Length(lEvenArray) - 1] := i;
    end;
  end;

  for i in lEvenArray do
    Write(i:3);
  Writeln;
end.
```


=={{header|Déjà Vu}}==

===Non-destructively===

```dejavu
filter pred lst:
	]
	for value in copy lst:
		if pred @value:
			@value
	[

even x:
	= 0 % x 2

!. filter @even [ 0 1 2 3 4 5 6 7 8 9 ]
```


```txt
[ 0 2 4 6 8 ]
```



### Destructively


```dejavu
local :lst [ 0 1 2 3 4 5 6 7 8 9 ]

filter-destructively pred lst:
	local :tmp []
	while lst:
		pop-from lst
		if pred dup:
			push-to tmp
		else:
			drop
	while tmp:
		push-to lst pop-from tmp

filter-destructively @even lst

!. lst
```


```txt
[ 0 2 4 6 8 ]
```



## Dyalect


===Non-destructively===

```Dyalect
func Array.filter(pred) {
    var arr = []
    for x in this when pred(x) {
        arr.add(x)
    }
    arr
}

var arr = [1..20].filter(x => x % 2 == 0)
print(arr)
```


```txt
[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
```



### Destructively


```Dyalect
func Array.filter(pred) {
    var i = 0
    while i < this.len() {
        if !pred(this[i]) {
            this.removeAt(i)
        }
        i += 1
    }
}

var arr = [1..20]
arr.filter(x => x % 2 == 0)
print(arr)
```


```txt
[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
```



### Idiomatic approach


Idiomatic approach in Dy is to use non-strict iterators (which can be combined without intermedate data structures) and translate the result to an array if needed:


```Dyalect
func Iterator.filter(pred) {
    for x in this when pred(x) {
        yield x
    }
}

func Iterator.select(proj) {
    for x in this {
        yield proj(x)
    }
}

var xs = [1..20]
var arr = xs.iter().filter($0 % 2 == 0).select($0.toString())
print(arr.toArray())
```


```txt
["2", "4", "6", "8", "10", "12", "14", "16", "18", "20"]
```



## E


There are several ways this could be done.


```e
pragma.enable("accumulator")
accum [] for x ? (x %% 2 <=> 0) in [1,2,3,4,5,6,7,8,9,10] { _.with(x) }
```



```e
var result := []
for x ? (x %% 2 <=> 0) in [1,2,3,4,5,6,7,8,9,10] {
    result with= x
}
result
```



```e>def makeSeries := <elang:control.makeSeries

makeSeries([1,2,3,4,5,6,7,8,9,10]).filter(fn x,_{x %% 2 <=> 0}).asList()
```



## EchoLisp


```scheme

(iota 12) → { 0 1 2 3 4 5 6 7 8 9 10 11 }

;; lists
(filter even? (iota 12))
    → (0 2 4 6 8 10)

;; array (non destructive)
(vector-filter even? #(1 2 3 4 5 6 7 8 9 10 11 12 13))
    → #( 2 4 6 8 10 12)

;; sequence, infinite, lazy
(lib 'sequences)
(define evens (filter even? [0 ..]))

(take evens 12)
    → (0 2 4 6 8 10 12 14 16 18 20 22)


```




## Ela


===Using higher-order function (non-strict version)===


```ela
open list

evenList = filter' (\x -> x % 2 == 0) [1..]
```


===Using comprehension (non-strict version)===


```ela
evenList = [& x \\ x <- [1..] | x % 2 == 0]
```


## Elena

ELENA 4.1 :

```elena
import system'routines;
import system'math;
import extensions;
import extensions'routines;

public program()
{
    auto array := new int[]::(1,2,3,4,5);

    var evens := array.filterBy:(n => n.mod:2 == 0).toArray();

    evens.forEach:printingLn
}
```

Using strong typed collections and extensions:

```elena
import system'collections;
import system'routines'stex;
import system'math;
import extensions;

public program()
{
    int[] array := new int[]::(1,2,3,4,5);

    array
        .filterBy:(int n => n.mod:2 == 0)
        .forEach:(int i){ console.printLine(i) }
}
```

```txt

2
4

```



## Elixir


```elixir
iex(10)> numbers = Enum.to_list(1..9)
[1, 2, 3, 4, 5, 6, 7, 8, 9]
iex(11)> Enum.filter(numbers, fn x -> rem(x,2)==0 end)
[2, 4, 6, 8]
iex(12)> for x <- numbers, rem(x,2)==0, do: x          # comprehension
[2, 4, 6, 8]
```



## Erlang


```erlang
Numbers = lists:seq(1, 5).
EvenNumbers = lists:filter(fun (X) -> X rem 2 == 0 end, Numbers).
```


Or using a list comprehension:


```erlang
EvenNumbers = [X || X <- Numbers, X rem 2 == 0].
```



## Euphoria


```euphoria
sequence s, evens
s = {1, 2, 3, 4, 5, 6}
evens = {}
for i = 1 to length(s) do
    if remainder(s[i], 2) = 0 then
        evens = append(evens, s[i])
    end if
end for
? evens
```


```txt
{2,4,6}

```


=={{header|F Sharp|F#}}==

```fsharp
let lst = [1;2;3;4;5;6]
List.filter (fun x -> x % 2 = 0) lst;;

val it : int list = [2; 4; 6]
```



## Factor

This code uses ''filter'' on an array.


```factor>10 <iota>
array [ even? ] filter .
! prints { 0 2 4 6 8 }
```


''10 <iota>'' is already a sequence, so we can skip the conversion to array.


```factor>10 <iota
 [ even? ] filter .
! prints V{ 0 2 4 5 8 }
```



###  Destructive

This uses ''filter!'' to modify the original vector.


```factor
USE: vectors
10 <iota> >vector [ even? ] filter! .
! prints V{ 0 2 4 5 8 }
```


To prove that ''filter!'' is destructive but ''filter'' is non-destructive, I assign the original vector to ''v''.


```factor
USE: locals
10 <iota> >vector [| v |
    v [ even? ] filter drop
    v pprint " after filter" print
    v [ even? ] filter! drop
    v pprint " after filter!" print
] call
! V{ 0 1 2 3 4 5 6 7 8 9 } after filter
! V{ 0 2 4 6 8 } after filter!
```



## Fantom



```fantom

class Main
{
  Void main ()
  {
    items := [1, 2, 3, 4, 5, 6, 7, 8]
    // create a new list with just the even numbers
    evens := items.findAll |i| { i.isEven }
    // display the result
    echo (evens.join(","))
  }
}

```




## Forth


```forth
: sel ( dest 0 test src len -- dest len )
  cells over + swap do   ( dest len test )
    i @ over execute if
      i @ 2over cells + !
      >r 1+ r>
    then
  cell +loop drop ;

create nums 1 , 2 , 3 , 4 , 5 , 6 ,
create evens 6 cells allot

: .array  0 ?do dup i cells + @ . loop drop ;

: even? ( n -- ? ) 1 and 0= ;

evens 0 ' even? nums 6 sel .array        \ 2 4 6
```




## Fortran



```fortran
module funcs
  implicit none
contains
  pure function iseven(x)
    logical :: iseven
    integer, intent(in) :: x
    iseven = mod(x, 2) == 0
  end function iseven
end module funcs
```



```fortran
program Filter
  use funcs
  implicit none

  integer, parameter                 :: N = 100
  integer, dimension(N)              :: array
  integer, dimension(:), pointer     :: filtered

  integer :: i

  forall(i=1:N) array(i) = i

  filtered => filterwith(array, iseven)
  print *, filtered

contains

  function filterwith(ar, testfunc)
    integer, dimension(:), pointer        :: filterwith
    integer, dimension(:), intent(in)     :: ar
    interface
       elemental function testfunc(x)
         logical :: testfunc
         integer, intent(in) :: x
       end function testfunc
    end interface

    integer :: i, j, n

    n = count( testfunc(ar) )
    allocate( filterwith(n) )

    j = 1
    do i = lbound(ar, dim=1), ubound(ar, dim=1)
       if ( testfunc(ar(i)) ) then
          filterwith(j) = ar(i)
          j = j + 1
       end if
    end do

  end function filterwith

end program Filter
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type FilterType As Function(As Integer) As Boolean

Function isEven(n As Integer) As Boolean
  Return n Mod 2  = 0
End Function

Sub filterArray(a() As Integer, b() As Integer, filter As FilterType)
  If UBound(a) = -1 Then Return  '' empty array
  Dim count As Integer = 0
  Redim b(0 To UBound(a) - LBound(a))
  For i As Integer = LBound(a) To UBound(a)
    If filter(a(i)) Then
      b(count) = a(i)
      count += 1
    End If
  Next

  If count > 0 Then Redim Preserve b(0 To count - 1) '' trim excess elements
End Sub

' Note that da() must be a dynamic array as static arrays can't be redimensioned
Sub filterDestructArray(da() As Integer, filter As FilterType)
  If UBound(da) = -1 Then Return  '' empty array
  Dim count As Integer = 0
  For i As Integer = LBound(da) To UBound(da)
    If i > UBound(da) - count Then Exit For
    If Not filter(da(i)) Then '' remove this element by moving those still to be examined down one
      For j As Integer = i + 1 To UBound(da) - count
        da(j - 1) = da(j)
      Next j
      count += 1
      i -= 1
    End If
  Next i

  If count > 0 Then
    Redim Preserve da(LBound(da) To UBound(da) - count) '' trim excess elements
  End If
End Sub

Dim n As Integer = 12
Dim a(1 To n) As Integer '' creates dynamic array as upper bound is a variable
For i As Integer = 1 To n : Read a(i) : Next
Dim b() As Integer '' array to store results
filterArray a(), b(), @isEven
Print "The even numbers are (in new array)      : ";
For i As Integer = LBound(b) To UBound(b)
  Print b(i); " ";
Next
Print : Print
filterDestructArray a(), @isEven
Print "The even numbers are (in original array) : ";
For i As Integer = LBound(a) To UBound(a)
  Print a(i); " ";
Next
Print : Print
Print "Press any key to quit"
Sleep
End

Data 1, 2, 3, 7, 8, 10, 11, 16, 19, 21, 22, 27
```


```txt

The even numbers are (in new array)      :  2  8  10  16  22

The even numbers are (in original array) :  2  8  10  16  22

```



## Frink


```frink

b = array[1 to 100]
c = select[b, {|x| x mod 2  == 0}]

```



## Futhark

```Futhark

fun main(as: []int): []int =
  filter (fn x => x%2 == 0) as

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=e73bc5db1e3bb56c598f89aa669a0825 Click this link to run this code]'''

```gambas
sRandom As New String[]
'______________________________________________________________________________________________________
Public Sub Main()
Dim siCount As Short

For siCount = 0 To 19
  sRandom.Add(Rand(1, 100))
Next

Print sRandom.join(",")

NewArray
Destructive

End
'______________________________________________________________________________________________________
Public Sub NewArray() 'Select certain elements from an array into a new array in a generic way
Dim sEven As New String[]
Dim siCount As Short

For siCount = 0 To sRandom.Max
  If Even(sRandom[siCount]) Then sEven.Add(sRandom[siCount])
Next

Print sEven.join(",")

End
'______________________________________________________________________________________________________
Public Sub Destructive() 'Give a second solution which filters destructively
Dim siIndex As New Short[]
Dim siCount As Short

For siCount = 0 To sRandom.Max
  If Odd(sRandom[siCount]) Then siIndex.Add(siCount)
Next

For siCount = siIndex.max DownTo 0
  sRandom.Extract(siIndex[siCount], 1)
Next

Print sRandom.join(",")

End
```

Output:

```txt

36,13,21,37,68,6,47,4,53,80,90,95,60,29,76,39,6,93,83,91
36,68,6,4,80,90,60,76,6
36,68,6,4,80,90,60,76,6

```



## GAP


```gap
# Built-in

Filtered([1 .. 100], IsPrime);
# [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ]

Filtered([1 .. 10], IsEvenInt);
# [ 2, 4, 6, 8, 10 ]

Filtered([1 .. 10], IsOddInt);
# [ 1, 3, 5, 7, 9 ]
```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
)

func main() {
    a := rand.Perm(20)
    fmt.Println(a)       // show array to filter
    fmt.Println(even(a)) // show result of non-destructive filter
    fmt.Println(a)       // show that original array is unchanged
    reduceToEven(&a)     // destructive filter
    fmt.Println(a)       // show that a is now changed
    // a is not only changed, it is changed in place.  length and capacity
    // show that it still has its original allocated capacity but has now
    // been reduced in length.
    fmt.Println("a len:", len(a), "cap:", cap(a))
}

func even(a []int) (r []int) {
    for _, e := range a {
        if e%2 == 0 {
            r = append(r, e)
        }
    }
    return
}

func reduceToEven(pa *[]int) {
    a := *pa
    var last int
    for _, e := range a {
        if e%2 == 0 {
            a[last] = e
            last++
        }
    }
    *pa = a[:last]
}
```

```txt

[15 1 7 3 4 8 19 0 17 18 14 5 16 9 13 11 12 10 2 6]
[4 8 0 18 14 16 12 10 2 6]
[15 1 7 3 4 8 19 0 17 18 14 5 16 9 13 11 12 10 2 6]
[4 8 0 18 14 16 12 10 2 6]
a len: 10 cap: 20

```



## Groovy


```groovy
 def evens = [1, 2, 3, 4, 5].findAll{it % 2 == 0}
```



## Haskell


In Haskell, a list is often more basic than an array:


```haskell
ary = [1..10]
evens = [x | x <- ary, even x]
```

or

```haskell>evens = filter even ary</lang


To do the same operation on an array, the simplest way it to convert it lazily into a list:


```haskell
import Data.Array

ary = listArray (1,10) [1..10]
evens = listArray (1,n) l where
  n = length l
  l = [x | x <- elems ary, even x]
```


Note that the bounds must be known before creating the array, so the temporary list will be fully evaluated before the array is created.

=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()

every put(A := [],1 to 10)              # make a list of 1..10
every put(B := [],iseven(!A))           # make a second list and filter out odd numbers
every writes(!B," ") | write()          # show
end

procedure iseven(x)                     #: return x if x is even or fail
if x % 2 = 0 then return x
end
```



## IDL

The <tt>where()</tt> function can select elements on any logical expression. For example


```idl
result = array[where(NOT array AND 1)]
```



## J

'''Solution:'''

With any verb (function) <code>f</code> that returns a boolean for each element of a vector <code>v</code>, the following is the generic solution:

```j
   (#~ f) v
```


'''Examples:'''

```j
   ] v=: 20 ?@$ 100   NB. vector of 20 random integers between 0 and 99
63 92 51 92 39 15 43 89 36 69 40 16 23 2 29 91 57 43 55 22

   v #~ -.2| v
92 92 36 40 16 2 22
```


Or using the generic form suggested above:

```j
   isEven=: 0 = 2&|    NB. verb testing for even numbers
   (#~ isEven) v
92 92 36 40 16 2 22
```


We might decide that we use this pattern so often that it is worthwhile creating a new adverb <code>select</code> that filters an array using the verb to its left.

```j
   select=: adverb def '(#~ u)'
   isPrime=: 1&p:

   isEven select v
92 92 36 40 16 2 22
   isPrime select v
43 89 23 2 29 43
   (isEven *. isPrime) select v
2
```


Destructive example:


```j>   v=: isEven select v</lang


(That said, note that in a highly parallel computing environment the destruction either happens after the filtering or you have to repeatedly stall the filtering to ensure that some sort of partially filtered result has coherency.)


## Java


```java
int[] array = {1, 2, 3, 4, 5 };
List<Integer> evensList = new ArrayList<Integer>();
for (int  i: array) {
    if (i % 2 == 0) evensList.add(i);
}
int[] evens = evensList.toArray(new int[0]);
```


----

A Java 8 solution with stream and generic types:

```java>public static <T
 T[] filter(T[] input, Predicate<T> filterMethod) {
    return Arrays.stream(input)
        .filter(filterMethod)
        .toArray(size -> (T[]) Array.newInstance(input.getClass().getComponentType(), size));
}
```

Methodcall:

```java
Integer[] array = {1, 2, 3, 4, 5};
Integer[] result = filter(array, i -> (i % 2) == 0);
```

Warning: This solution works not with primitive types!<br/>
For arrays with a primitive type use the wrapper class.


## JavaFX Script


```javafx
def array = [1..100];
def evens = array[n | n mod 2 == 0];
```



## JavaScript


### ES5

The standard way is to use the [https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Global_Objects/Array/filter Array.prototype.filter] function ({{works with|JavaScript|1.6}}):

```javascript
var arr = [1,2,3,4,5];
var evens = arr.filter(function(a) {return a % 2 == 0});
```


Other ways:

```javascript
var arr = [1,2,3,4,5];
var evens = [];
for (var i=0, ilen=arr.length; i<ilen; i++)
      if (arr[i] % 2 == 0)
              evens.push(arr[i]);
```


```javascript
var numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
var evens = [i for (i in numbers) if (i % 2 == 0)];

function range(limit) {
  for(var i = 0; i < limit; i++) {
    yield i;
  }
}

var evens2 = [i for (i in range(100)) if (i % 2 == 0)];
```


```javascript
Functional.select("+1&1", [1,2,3,4])   // [2, 4]
```



### ES6



```JavaScript
(() => {
    'use strict';

    // isEven :: Int -> Bool
    const isEven = n => n % 2 === 0;


    // TEST

    return [1,2,3,4,5,6,7,8,9]
        .filter(isEven);

    // [2, 4, 6, 8]
})();
```


```JavaScript
[2, 4, 6, 8]
```



## jq

jq's "select" filter is designed to make it easy to filter both arrays and streams:

```jq
(1,2,3,4,5,6,7,8,9) | select(. % 2 == 0)
```

 2
 4
 6
 8

```jq

[range(1;10)] | map( select(. % 2 == 0) )

```

 [2,4,6,8]

## Julia

```julia
@show filter(iseven, 1:10)
```


```txt
filter(iseven, 1:10) = [2, 4, 6, 8, 10]
```



## K


```K
   / even is a boolean function
   even:{0=x!2}
   even 1 2 3 4 5
0 1 0 1 0

   / filtering the even numbers
   a@&even'a:1+!10
2 4 6 8 10

   / as a function
   evens:{x@&even'x}
   a:10?100
45 5 79 77 44 15 83 88 33 99
   evens a
44 88
```


Alternative syntax:

```K
   {x[&0=x!2]}
   {x[&even x]}
```


Destructive:

```K
   a:evens a
44 88
```



## Kotlin


```scala
// version 1.0.5-2

fun main(args: Array<String>) {
    val array = arrayOf(1, 2, 3, 4, 5, 6, 7, 8, 9)
    println(array.joinToString(" "))

    val filteredArray = array.filter{ it % 2 == 0 }
    println(filteredArray.joinToString(" "))

    val mutableList = array.toMutableList()
    mutableList.retainAll { it % 2 == 0 }
    println(mutableList.joinToString(" "))
}
```


```txt

1 2 3 4 5 6 7 8 9
2 4 6 8
2 4 6 8

```



## Lang5


```lang5
: filter  over swap execute select ;
10 iota "2 % not" filter . "\n" .

# [    0     2     4     6     8  ]
```




## Lasso


```Lasso
local(original = array(1,2,3,4,5,6,7,8,9,10))
local(evens = (with item in #original where #item % 2 == 0 select #item) -> asstaticarray)
#evens
```


```txt
staticarray(2, 4, 6, 8, 10)
```


Modifying the original array

``` lasso
local(original = array(1,2,3,4,5,6,7,8,9,10))
with item in #original where #item % 2 != 0 do #original ->removeall(#item)
#original
```


```txt
array(2, 4, 6, 8, 10)
```



## Liberty BASIC


```lb
' write random nos between 1 and 100
' to array1 counting matches as we go
dim array1(100)
count=100
for i = 1 to 100
    array1(i) = int(rnd(0)*100)+1
    count=count-(array1(i) mod 2)
next

'dim the extract and fill it
dim array2(count)
for i = 1 to 100
    if not(array1(i) mod 2) then
        n=n+1
        array2(n)=array1(i)
    end if
next

for n=1 to count
    print array2(n)
next
```



## Lisaac


```Lisaac
+ a, b : ARRAY[INTEGER];
a := ARRAY[INTEGER].create_with_capacity 10 lower 0;
b := ARRAY[INTEGER].create_with_capacity 10 lower 0;
1.to 10 do { i : INTEGER;
  a.add_last i;
};
a.foreach { item : INTEGER;
  (item % 2 = 0).if {
    b.add_last item;
  };
};
```



## Logo


```logo
to even? :n
  output equal? 0 modulo :n 2
end
show filter "even? [1 2 3 4]    ; [2 4]

show filter [equal? 0 modulo ? 2] [1 2 3 4]
```


## Lua


```lua
function filter(t, func)
  local ret = {}
  for i, v in ipairs(t) do
    ret[#ret+1] = func(v) and v or nil
  end
  return ret
end

function even(a) return a % 2 == 0 end

print(unpack(filter({1, 2, 3, 4 ,5, 6, 7, 8, 9, 10}, even)))
```


The destructive version is even simpler, since tables are passed by reference:


```lua
function filter(t, func)
  for i, v in ipairs(t) do
    if not func(v) then table.remove(t, i) end
  end
end

function even(a) return a % 2 == 0 end

local values = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
filter(values, even)
print(unpack(values))
```



## M2000 Interpreter


### Using Filter for arrays


```M2000 Interpreter

Module Checkit {
      Print (1,2,3,4,5,6,7,8)#filter(lambda ->number mod 2=0)
}
Checkit

```



### Old style

Function GetEvenNumbers can get pointer to array or array and return a pointer to array.

Module Filter2EvenNumbers get an array by reference and first place numbers to stack and then make stack an array and then copy to array.

Module Filter2EvenNumbers change definition and now place numbers in A() and at the last statement A() change dimension, preserving values.

We can use Base 1 arrays too: Dim Base 1, A(5) :  A(1)=10,3,6,7,11




```M2000 Interpreter

Module CheckIt {
      Function GetEvenNumbers (A as array){
            If len(A)=0 then =(,) : exit
            Flush  ' empty current stack (of values)
            n=each(A)
            While n {
                  if array(n) mod 2 = 0 then data array(n)
            }
            \\ [] return a stack object, leave an empty stack as current stack
            =Array([])
      }

      Dim A(5), B()
      A(0)=10,3,6,7,11
      B()=GetEvenNumbers(A())
      Print B()  ' print 10,6
      Print GetEvenNumbers((1,2,3,4,5,6,7,8))  ' 2 4 6 8

      Module Filter2EvenNumbers (&A()) {
            If len(A())=0 then  exit
            Stack New {
                  Flush  ' empty current stack (of values)
                  n=each(A())
                  While n {
                        if array(n) mod 2 = 0 then data array(n)
                  }
                  \\ [] return a stack object, leave an empty stack as current stack
                  A()=Array([])
            }
      }
      A(0)=10,3,6,7,11
      Filter2EvenNumbers &A()
      Print A()  ' 10 6
      Module Filter2EvenNumbers (&A()) {
            If len(A())=0 then  exit
            n=each(A())
            x=Dimension(A(), 0)-1  ' base of array (0 or 1)
            k=-x
            While n {
                  if array(n) mod 2 = 0 then x++ : A(x)=Array(n)
            }
            Dim A(x+k)
       }
      Dim A(5)
      A(0)=10,3,6,7,11
      Filter2EvenNumbers &A()
      Print A()  ' 10 6
}
CheckIt
}
CheckIt

```



## Maple


```Maple

evennum:=proc(nums::list(integer))
	return select(x->type(x, even), nums);
end proc;

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Check for even integers:

```Mathematica
Select[{4, 5, Pi, 2, 1.3, 7, 6, 8.0}, EvenQ]
```

gives:

```Mathematica
{4, 2, 6}
```

To check also for approximate number (like 8.0 in the example above) a possible solution is:

```Mathematica
Select[{4, 5, Pi, 2, 1.3, 7, 6, 8.0}, Mod[#, 2] == 0 &]
```

gives:

```Mathematica
{4, 2, 6, 8.}
```

notice that the function returns 8. not 8 (the dot indicates that it is a float number, not an integer).


## MATLAB


```MATLAB
function evens = selectEvenNumbers(list)

    evens = list( mod(list,2) == 0 );

end
```


```MATLAB>>
 selectEvenNumbers([0 1 2 3 4 5 6 7 8 9 10])

ans =

     0     2     4     6     8    10
```




## Maxima


```maxima
a: makelist(i, i, 1, 20);
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

sublist(a, evenp);
[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

sublist(a, lambda([n], mod(n, 3) = 0));
[3, 6, 9, 12, 15, 18]
```



## MAXScript


```maxscript
arr = #(1, 2, 3, 4, 5, 6, 7, 8, 9)
newArr = for i in arr where (mod i 2 == 0) collect i
```



## min

```min
(1 2 3 4 5 6 7 8 9 10) 'even? filter print
```

```txt

(2 4 6 8 10)

```



## ML

=
## Standard ML
=

```sml
val ary = [1,2,3,4,5,6];
List.filter (fn x => x mod 2 = 0) ary
```

=
## MLite
=
MLite is similar to Standard ML, though '=>' becomes '=' and 'List.' is elided:

```ocaml
val ary = [1,2,3,4,5,6];
filter (fn x = x mod 2 = 0) ary;
```



## MUMPS


```MUMPS
FILTERARRAY
 ;NEW I,J,A,B - Not making new, so we can show the values
 ;Populate array A
 FOR I=1:1:10 SET A(I)=I
 ;Move even numbers into B
 SET J=0 FOR I=1:1:10 SET:A(I)#2=0 B($INCREMENT(J))=A(I)
 QUIT
```

Testing:
```txt
WRITE

A(1)=1
A(2)=2
A(3)=3
A(4)=4
A(5)=5
A(6)=6
A(7)=7
A(8)=8
A(9)=9
A(10)=10
B(1)=2
B(2)=4
B(3)=6
B(4)=8
B(5)=10
I=10
J=5
```



## Nemerle

Lists have a built-in method for filtering:

```Nemerle
def original = $[1 .. 100];
def filtered = original.Filter(fun(n) {n % 2 == 0});
WriteLine($"$filtered");
```

The following would work for arrays:

```Nemerle
Filter[T] (a : array[T], f : T -> bool) : array[T]
{
    def b = $[x | x in a, (f(x))];
    b.ToArray()
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary
numeric digits 5000

--
### =======================================================================

class RFilter public
  properties indirect
    filter = RFilter.ArrayFilter
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method main(args = String[]) public static
    arg = Rexx(args)
    RFilter().runSample(arg)
    return
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method runSample(arg) public
    sd1 = Rexx[]
    sd2 = Rexx[]

    say 'Test data:'
    sd1 = makeSampleData(100)
    display(sd1)
    setFilter(RFilter.EvenNumberOnlyArrayFilter())
    say
    say 'Option 1 (copy to a new array):'
    sd2 = getFilter().filter(sd1)
    display(sd2)
    say
    say 'Option 2 (replace the original array):'
    sd1 = getFilter().filter(sd1)
    display(sd1)
    return
  -- ---------------------------------------------------------------------------
  method display(sd = Rexx[]) public static
    say '-'.copies(80)
    loop i_ = 0 to sd.length - 1
      say sd[i_] '\-'
      end i_
    say
    return
  -- ---------------------------------------------------------------------------
  method makeSampleData(size) public static returns Rexx[]
    sd = Rexx[size]
    loop e_ = 0 to size - 1
      sd[e_] = (e_ + 1 - size / 2) / 2
      end e_
    return sd

--
### =======================================================================

class RFilter.ArrayFilter abstract
  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method filter(array = Rexx[]) public abstract returns Rexx[]
-- = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
class RFilter.EvenNumberOnlyArrayFilter extends RFilter.ArrayFilter
  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  method filter(array = Rexx[]) public returns Rexx[]
    clist = ArrayList(Arrays.asList(array))
    li = clist.listIterator()
    loop while li.hasNext()
      e_ = Rexx li.next
      if \e_.datatype('w'), e_ // 2 \= 0 then li.remove()
      end
    ry = Rexx[] clist.toArray(Rexx[clist.size()])
    return ry

```

```txt

Test data:
--------------------------------------------------------------------------------
-24.5 -24 -23.5 -23 -22.5 -22 -21.5 -21 -20.5 -20 -19.5 -19 -18.5 -18 -17.5 -17 -16.5 -16 -15.5 -15 -14.5 -14 -13.5 -13 -12.5 -12 -11.5 -11 -10.5 -10 -9.5 -9 -8.5 -8 -7.5 -7 -6.5 -6 -5.5 -5 -4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5 5 5.5 6 6.5 7 7.5 8 8.5 9 9.5 10 10.5 11 11.5 12 12.5 13 13.5 14 14.5 15 15.5 16 16.5 17 17.5 18 18.5 19 19.5 20 20.5 21 21.5 22 22.5 23 23.5 24 24.5 25

Option 1 (copy to a new array):
--------------------------------------------------------------------------------
-24 -22 -20 -18 -16 -14 -12 -10 -8 -6 -4 -2 0 2 4 6 8 10 12 14 16 18 20 22 24

Option 2 (replace the original array):
--------------------------------------------------------------------------------
-24 -22 -20 -18 -16 -14 -12 -10 -8 -6 -4 -2 0 2 4 6 8 10 12 14 16 18 20 22 24

```



## NewLISP


```NewLISP>
 (filter (fn (x) (= (% x 2) 0)) '(1 2 3 4 5 6 7 8 9 10))
(2 4 6 8 10)

```



## Nial


```nial
filter (= [0 first,  mod [first, 2 first] ] ) 0 1 2 3 4 5 6 7 8 9 10
=0 2 4 6 8 10
```



## Nim


```nim
import sequtils

let values = @[0,1,2,3,4,5,6,7,8,9]

let evens = values.filter(proc (x: int): bool = x mod 2 == 0)

let odds = values.filterIt(it mod 2 == 1)
```



## Objeck


```objeck

use Structure;

bundle Default {
  class Evens {
    function : Main(args : String[]) ~ Nil {
      values := IntVector->New([1, 2, 3, 4, 5]);
      f := Filter(Int) ~ Bool;
      evens := values->Filter(f);

      each(i : evens) {
        evens->Get(i)->PrintLine();
      };
    }

    function : Filter(v : Int) ~ Bool {
      return v % 2 = 0;
    }
  }
}

```


=={{header|Objective-C}}==
```objc
NSArray *numbers = [NSArray arrayWithObjects:[NSNumber numberWithInt:1],
                                             [NSNumber numberWithInt:2],
                                             [NSNumber numberWithInt:3],
                                             [NSNumber numberWithInt:4],
                                             [NSNumber numberWithInt:5], nil];
NSArray *evens = [numbers objectsAtIndexes:[numbers indexesOfObjectsPassingTest:
  ^BOOL(id obj, NSUInteger idx, BOOL *stop) { return [obj intValue] % 2 == 0; } ]];
```


```objc
NSArray *numbers = [NSArray arrayWithObjects:[NSNumber numberWithInt:1],
                                             [NSNumber numberWithInt:2],
                                             [NSNumber numberWithInt:3],
                                             [NSNumber numberWithInt:4],
                                             [NSNumber numberWithInt:5], nil];
NSPredicate *isEven = [NSPredicate predicateWithFormat:@"modulus:by:(SELF, 2) == 0"];
NSArray *evens = [numbers filteredArrayUsingPredicate:isEven];
```


```objc>#import <Foundation/Foundation.h


@interface NSNumber ( ExtFunc )
-(int) modulo2;
@end

@implementation NSNumber ( ExtFunc )
-(int) modulo2
{
  return [self intValue] % 2;
}
@end

int main()
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  NSArray *numbers = [NSArray arrayWithObjects:[NSNumber numberWithInt:1],
                                               [NSNumber numberWithInt:2],
                                               [NSNumber numberWithInt:3],
                                               [NSNumber numberWithInt:4],
                                               [NSNumber numberWithInt:5], nil];

  NSPredicate *isEven = [NSPredicate predicateWithFormat:@"modulo2 == 0"];
  NSArray *evens = [numbers filteredArrayUsingPredicate:isEven];

  NSLog(@"%@", evens);


  [pool release];
  return 0;
}
```



## OCaml

It is easier to do it with a list:

```ocaml
let lst = [1;2;3;4;5;6]
let even_lst = List.filter (fun x -> x mod 2 = 0) lst
```



## Octave


```octave
arr = [1:100];
evennums = arr( mod(arr, 2) == 0 );
disp(evennums);
```



## Oforth



```Oforth
100 seq filter(#isEven)
```



## ooRexx


```oorexx
 Call random ,,1234567
 a=.array~new
 b=.array~new
 Do i=1 To 10
   a[i]=random(1,9999)
   End
 Say 'Unfiltered values:' a~makestring(line,' ')
 /* copy even numbers to array b */
 j=0
 Do i=1 to 10
   If filter(a[i]) Then Do
     j = j + 1
     b[j]=a[i]
     End
   end
 Say 'Filtered values (in second array):      ' b~makestring(line,' ')
 /* destructive filtering: copy within array a */
 j=0
 Do i=1 to 10
   If filter(a[i]) Then Do
     j = j + 1
     a[j]=a[i]
     End
   end
 /* destructive filtering: delete the remaining elements */
 Do i=10 To j+1 By -1
   a~delete(i)
   End
 Say 'Filtered values (destructive filtering):' a~makestring(line,' ')
 Exit
 filter: Return arg(1)//2=0
```

```txt
Unfiltered values: 1412 2244 6778 4002 439 3335 5877 8273 7882 1469
Filtered values (in second array):       1412 2244 6778 4002 7882
Filtered values (destructive filtering): 1412 2244 6778 4002 7882
```



## Oz

It is easier to do it with a list:

```oz
declare
Lst = [1 2 3 4 5]
LstEven = {Filter Lst IsEven}
```



## PARI/GP

```parigp
iseven(n)=n%2==0
select(iseven, [2, 3, 4, 5, 7, 8, 9, 11, 13, 16, 17])
```


Or in anonymous form

```parigp
select(n -> n%2==0, [2, 3, 4, 5, 7, 8, 9, 11, 13, 16, 17])
```



## Pascal


Arrays are supported in all versions of pascal so this simple example will cover the entire gamut.

```pascal
const

numbers:array[0..9] of integer = (0,1,2,3,4,5,6,7,8,9);

for x = 1 to 10 do
     if odd(numbers[x]) then
         writeln( 'The number ',numbers[x],' is odd.');
     else
         writeln( 'The number ',numbers[x],' is even.');
```


The odd() function is a standard library function of pascal as is the function even().


## Peloton

Fixed length English dialect

```sgml><@ LETCNWLSTLIT
numbers|1 2 3 4 5 6 7 8 9 10 11 12</@>
<@ DEFLST>evens</@>
<@ ENULSTLIT>numbers|
<@ TSTEVEELTLST>...</@>
<@ IFF>
<@ LETLSTELTLST>evens|...</@>
</@>
</@>
```



## Perl


```perl
my @a = (1, 2, 3, 4, 5, 6);
my @even = grep { $_%2 == 0 } @a;
```



## Perl 6

```perl6
my @a = 1..6;
my @even = grep * %% 2, @a;
```


Alternatively:


```perl6
my @a = 1..6;
my @even = @a.grep(* %% 2);
```


Destructive:


```perl6
my @a = 1..6;
@a .= grep(* %% 2);
```



## Phix

A classic non-destructive solution might look like this:

```Phix
function filternd(sequence array, integer filterid)
sequence res = {}
    for i=1 to length(array) do
        if call_func(filterid,{array[i]}) then
            res = append(res,array[i])
        end if
    end for
    return res
end function
```


However, Phix is reference counted so the distinction between destructive and
non-destructive is more subtle. In the following code filterd() acts both ways.


```Phix
function filterd(sequence a, integer filterid)
integer l = 0
    for i=1 to length(a) do
        if call_func(filterid,{a[i]}) then
            l += 1
            a[l] = a[i]
        end if
    end for
    return a[1..l]
end function

function even(integer i)
    return and_bits(i,1)=0
end function
constant r_even = routine_id("even")

procedure main()
sequence s = tagset(10), t
    t = filterd(s,r_even)
    ?s
    ?t
    -- automatic pass by reference occurs here for s:
    s = filterd(s,r_even)
    ?s
end procedure
main()
```

```txt

{1,2,3,4,5,6,7,8,9,10}
{2,4,6,8,10}
{2,4,6,8,10}

```


In the t=f(s) call, s is preserved because
of copy-on-write semantics. Modifying a does not modify s. In the s=f(s) call,
s is automatically passed by reference, ie the local s is <no value> over the
duration of the call whereas parameter a of filterd() contains the only reference to the previous content of s,
and no copy-on-write occurs. Technically modifying a is still not modifying
s, but since it has a reference count of 1 it modifies the data that used to be referenced by s, in situ.
Note: adding t=s before the s=f(s) call would make it non-destructive again,
as t must be preserved and there is now a reference count >1 on that data.

There is one case in the interpreter (pEmit2.e/rebuild_callback()) where it
has to circumvent this behaviour. For performance reasons it does not populate
the symbol table with actual names until a fatal error or trace event occurs.
At that time, the symbol table may have a reference count>1, so it deliberately
patches it to 1 over the name population call to switch off the copy-on-write
semantics, and later restores the reference count before carrying on.


## PHL



```phl
module var;

extern printf;

@Integer main [
	var arr = 1..9;
	var evens = arr.filter(#(i) i % 2 == 0);
	printf("%s\n", evens::str);

	return 0;
]
```



## PHP

Using a standard loop

```php
$arr = range(1,5);
$evens = array();
foreach ($arr as $val){
      if ($val % 2 == 0) $evens[] = $val);
}
print_r($evens);
```


Using a filter function

```php
function is_even($var) { return(!($var & 1)); }
$arr = range(1,5);
$evens = array_filter($arr, "is_even");
print_r($evens);
```



## PicoLisp


```PicoLisp
(filter '((N) (not (bit? 1 N)))
   (1 2 3 4 5 6 7 8 9) )
```

```txt
-> (2 4 6 8)
```



## PL/I


```pli
(subscriptrange):
filter_values: procedure options (main); /* 15 November 2013 */
   declare a(20) fixed, b(*) fixed controlled;
   declare (i, j, n) fixed binary;

   a = random()*99999; /* fill the array with random elements from 0-99998 */
   put list ('Unfiltered values:');
   put skip edit (a) (f(6));
   /* Loop to count the number of elements that will be filtered */
   n = 0;
   do i = 1 to hbound(a);
      n = n + filter(a(i));
   end;
   allocate b(n);
   j = 0;
   do i = 1 to hbound(a);
      if filter(a(i)) then do; j = j + 1; b(j) = a(i); end;
   end;
   put skip list ('Filtered values:');
   put skip edit (b) (f(6));

filter: procedure (value) returns (bit(1));
   declare value fixed;

   return (iand(abs(value), 1) = 0);
end filter;

end filter_values;
```

Results:

```txt

Unfiltered values:
 44270  6008 80477 17004 91587 48669 29623 74640 29841 20019 77833 59865 49647  2272 54781
 36154 40114 71893 25960 76863
Filtered values:
 44270  6008 17004 74640  2272 36154 40114 25960

```



## Pop11

Most natural solution in Pop11 would probably use list. Below we accumulate filtered elements on the stack and then allocate array for the result:


```pop11
;;; Generic filtering procedure which selects from ar elements
;;; satisfying pred
define filter_array(ar, pred);
lvars i, k;
    stacklength() -> k;
    for i from 1 to length(ar) do
       ;;; if element satisfies pred we leave it on the stack
       if pred(ar(i)) then ar(i) endif;
    endfor;
    ;;; Collect elements from the stack into a vector
    return (consvector(stacklength() - k));
enddefine;
;;; Use it
filter_array({1, 2, 3, 4, 5},
             procedure(x); not(testbit(x, 0)); endprocedure) =>
```



## PostScript

```postscript

[1 2 3 4 5 6 7 8 9 10] {2 mod 0 eq} find

```



## PowerShell


```powershell
$array = -15..37
$array | Where-Object { $_ % 2 -eq 0 }
```



## Prolog


### findall


```prolog
evens(D, Es) :- findall(E, (member(E, D), E mod 2 =:= 0), Es).
```


Usage:


```prolog
?- evens([1,2,3,4,5,6,7,8,9,10],E).
E = [2, 4, 6, 8, 10]
```



### Anonymous functions

Works with SWI-Prolog and <b>module(lambda)</b> written by <b>Ulrich Neumerkel</b>, "lambda.pl" can be found there : http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl

```Prolog
?- use_module(library(lambda)).
true.

?- include((\X^(X mod 2 =:= 0)), [1,2,3,4,5,6,7,8,9], L).
L = [2,4,6,8].
```



### filter and anonymous functions

Works with SWI-Prolog and <b>module(lambda)</b> written by <b>Ulrich Neumerkel</b>, "lambda.pl" can be found there : http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl

```prolog
:- use_module(lambda).

%% filter(Pred, LstIn, LstOut)
%%
filter(_Pre, [], []).

filter(Pred, [H|T], L) :-
	filter(Pred, T, L1),
	(   call(Pred,H) -> L = [H|L1]; L = L1).

```

Usage :

```prolog
 ?- filter(\X^(X mod 2 =:= 0), [1,2,3,4,5,6,7,8,9], L).
L = [2,4,6,8] .

```



## PureBasic


```PureBasic
Dim Tal.i(9)
Dim Evens.i(0)

;- Set up an array with random numbers
For i=0 To ArraySize(Tal())
  Tal(i)=Random(100)
Next

;- Pick out all Even and save them
j=0
For i=0 To ArraySize(Tal())
  If Tal(i)%2=0
    ReDim Evens(j)    ; extend the Array as we find new Even's
    Evens(j)=tal(i)
    j+1
  EndIf
Next

;- Display the result
PrintN("List of Randoms")
For i=0 To ArraySize(Tal())
  Print(Str(Tal(i))+" ")
Next
PrintN(#CRLF$+#CRLF$+"List of Even(s)")
For i=0 To ArraySize(Evens())
  Print(Str(Evens(i))+" ")
Next
```


 List of Randoms
 32 35 89 91 11 33 12 22 42 43

 List of Even(s)
 32 12 22 42


## Python

```python
values = range(10)
evens = [x for x in values if not x & 1]
ievens = (x for x in values if not x & 1) # lazy
# alternately but less idiomatic:
evens = filter(lambda x: not x & 1, values)
```


Alternative using the slice syntax with its optional "stride" expression:


```python
values = range(10)
evens = values[::2]
```


This works for all versions of Python (at least as far back as 1.5).  Lists (arrays) can be "sliced" by indexing them with a range (lower and upper bounds).  Thus mylist[1:9] evaluates  into a list from the second item (excluding the first item which is mylist[0], of course) up to but not including the ninth item.  In Python the expression mylist[:] is synonymous with mylist[0:len(mylist)] ... returning a copy of the complete list.  also mylist[:x] returns the first x items from the list and negative numbers can be used such that mylist[-x:] returns the last x items from the list.  The relatively obscure and optional stride expression can skip items and/or force the evaluation from the end of the list downward towards it's lower elements.  Thus mylist[::-1] returns a reversed copy of the list, mylist[::2] returns all even elements, mylist[1::2] returns all odd elements, and so on.

Since strings in Python can be treated as a sort of immutable list of characters then the slicing and extended slicing can also be used with them as well.  Thus mystring[::-2] will return every other character from the reverse order of the string.

One can also assign to a slice (of a list or other mutable indexed object.  Thus the following:


```python
values = range(10)
values[::2] = [11,13,15,17,19]
print values
11, 1, 13, 3, 15, 5, 17, 7, 19, 9
```



Or in functional terms, by descending generality and increasing brevity:
```python
'''Functional filtering - by descending generality and increasing brevity'''

from functools import (reduce)
from itertools import (chain)
import inspect
import re


def f1(xs):
    '''Catamorphism: fold / reduce.
       See [The expressiveness and universality of fold]
       (http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)'''
    return reduce(lambda a, x: a + [x] if even(x) else a, xs, [])


def f2(xs):
    '''List monad bind/inject operator (concatMap combined with
       an (a -> [b]) function which wraps its result in a
       possibly empty list). This is the universal abstraction
       which underlies list comprehensions.'''
    return concatMap(lambda x: [x] if even(x) else [])(xs)


def f3(xs):
    '''Built-in syntactic sugar for list comprehensions.
       Convenient, and encouraged as 'Pythonic',
       but less general and expressive than a fold.'''
    return (x for x in xs if even(x))


def f4(xs):
    '''Built-in filter function'''
    return filter(even, xs)


def main():
    '''Tests'''
    xs = enumFromTo(0)(10)
    print(
        tabulated(showReturn)(
            'By descending generality and increasing brevity:\n'
        )(
            lambda f: list(f(xs))
        )([f1, f2, f3, f4])
    )


# GENERIC -------------------------------------------------


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''Concatenated list over which a function has been mapped.
       The list monad can be derived by using a function of the type
       (a -> [b]) which wraps its output in list
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# even :: Int -> Bool
def even(x):
    '''Predicate'''
    return 0 == x % 2


# showReturn :: (a -> b) -> String
def showReturn(f):
    '''Stringification of final (return) expression in function body.'''
    return re.split('return ', inspect.getsource(f))[-1].strip()


# tabulated :: (a -> String) -> String -> (a -> b) -> [a] -> String
def tabulated(fShow):
    '''heading -> function -> input List -> tabulated output string'''
    def go(s, f, xs):
        w = max(len(fShow(x)) for x in xs)
        return s + '\n' + '\n'.join([
            fShow(x).rjust(w, ' ') +
            ' -> ' + str(f(x)) for x in xs
        ])
    return lambda s: lambda f: lambda xs: go(s, f, xs)


if __name__ == '__main__':
    main()
```

```txt
By descending generality and increasing brevity:

reduce(lambda a, x: a + [x] if even(x) else a, xs, []) -> [0, 2, 4, 6, 8, 10]
       concatMap(lambda x: [x] if even(x) else [])(xs) -> [0, 2, 4, 6, 8, 10]
                            (x for x in xs if even(x)) -> [0, 2, 4, 6, 8, 10]
                                      filter(even, xs) -> [0, 2, 4, 6, 8, 10]
```



## Q


```q>x where 0=x mod 2</lang



## R


```R
a <- 1:100
evennums <- a[ a%%2 == 0 ]
print(evennums)
```



## Racket

The classic way:

```Racket

-> (filter even? '(0 1 2 3 4 5 6 7 8 9))
'(0 2 4 6 8)

```

getting the list of non-evens too:

```Racket

-> (partition even? '(0 1 2 3 4 5 6 7 8 9))
'(0 2 4 6 8)
'(1 3 5 7 9)

```

Finally, using a for loop, similar to list comprehension:

```Racket

-> (for/list ([x '(0 1 2 3 4 5 6 7 8 9)] #:when (even? x)) x)
'(0 2 4 6 8)

```



## Raven


```raven
[ 0 1 2 3 4 5 6 7 8 9 ] as nums
group nums each
    dup 1 & if drop
list as evens
```



## REBOL


```REBOL
a: []  repeat i 100 [append a i] ; Build and load array.

evens: []  repeat element a [if even? element [append evens element]]

print mold evens
```


```txt
[2 4 6 8 10 12 14 16 18 20 22 24
26 28 30 32 34 36 38 40 42 44 46 48 50
52 54 56 58 60 62 64 66 68 70 72 74 76
78 80 82 84 86 88 90 92 94 96 98 100]
```


## Red


```Red
Red []
orig: [] repeat i 10 [append orig i]
?? orig
cpy: [] forall orig [if even? orig/1 [append cpy orig/1]]
;; or - because we know each second element is even :- )
;; cpy: extract next orig 2
?? cpy
remove-each ele orig [odd? ele]    ;; destructive
?? orig

```

```txt
orig: [1 2 3 4 5 6 7 8 9 10]
cpy: [2 4 6 8 10]
orig: [2 4 6 8 10]
>>

```


## REXX


### using two arrays

This example uses two arrays.   The   '''random'''   BIF is used to generate the numbers.

```rexx
/*REXX program selects all  even numbers  from an array and puts them  ──►  a new array.*/
parse arg N seed .                               /*obtain optional arguments from the CL*/
if N=='' | N==","      then N=50                 /*Not specified?  Then use the default.*/
if datatype(seed,'W')  then call random ,,seed   /*use the RANDOM seed for repeatability*/
old.=                                            /*the OLD array,  all are null so far. */
new.=                                            /* "  NEW    "     "   "    "   "  "   */
               do i=1  for N                     /*generate  N  random numbers ──►  OLD */
               old.i=random(1,99999)             /*generate random number   1  ──► 99999*/
               end   /*i*/
#=0                                              /*number of elements in  NEW  (so far).*/
      do j=1  for N                              /*process the elements of the OLD array*/
      if old.j//2 \== 0  then iterate            /*if element isn't even,  then skip it.*/
      #=#+1                                      /*bump the number of  NEW  elements.   */
      new.#=old.j                                /*assign the number to the  NEW  array.*/
      end   /*j*/

      do k=1  for #                              /*display all the  NEW   numbers.      */
      say right('new.'k, 20) "=" right(new.k,9)  /*display a line  (an array element).  */
      end   /*k*/                                /*stick a fork in it,  we're all done. */
```

Programming note:   the REXX statement

```rexx>      if old.j//2 \== 0  then iterate</lang

could've been replaced with

```rexx>      if old.j//2        then iterate</lang

but that would've assumed the numbers are integers   (no matter what form they're expressed in).

As it happens, the REXX program uses the numbers generated from the   '''random'''   BIF, which are integers.

'''output'''   when the following is used for input:   <tt> ,   1234567 </tt>

The '''1234567''' is the '''random''' BIF   ''seed''   so that the random numbers can be repeated when re-running the REXX program.

```txt

               new.1 =     17520
               new.2 =     77326
               new.3 =     36128
               new.4 =     19124
               new.5 =       202
               new.6 =     82314
               new.7 =     96140
               new.8 =      4066
               new.9 =      3254
              new.10 =     91178
              new.11 =     18806
              new.12 =     60646
              new.13 =     26428
              new.14 =     16790
              new.15 =     24868
              new.16 =     61954
              new.17 =     63424
              new.18 =     97538
              new.19 =     82278
              new.20 =     33360
              new.21 =     74026
              new.22 =     48472
              new.23 =     44360

```



### using one array with a control array

This version uses a control array, which isn't fully populated   (in REXX terms, a sparse array.)

```rexx
/*REXX program finds all  even  numbers from an array,  and  marks a control array.     */
parse arg N seed .                               /*obtain optional arguments from the CL*/
if N=='' | N==","      then N=50                 /*Not specified?  Then use the default.*/
if datatype(seed,'W')  then call random ,,seed   /*use the RANDOM seed for repeatability*/

               do i=1  for N                     /*generate  N  random numbers ──►  OLD */
               @.i=random(1,99999)               /*generate random number   1  ──► 99999*/
               end   /*i*/
!.=0                                             /*number of elements in  NEW  (so far).*/
      do j=1  for N                              /*process the  OLD  array elements.    */
      if @.j//2 \==0  then !.j=1                 /*mark the   !  array that it's ¬even. */
      end   /*j*/

      do k=1  for N                              /*display all the   @   even numbers.  */
      if !.k  then iterate                       /*if it's marked as not even,  skip it.*/
      say right('array.'k, 20) "=" right(@.k,9)  /*display a even number, filtered array*/
      end   /*k*/                                /*stick a fork in it,  we're all done. */
```

For the following input:   <tt> , 1234567 </tt>

the output is the same as the 1<sup>st</sup> REXX version   (using two arrays).

===using one array, destructive===
This version just uses one array to perform the filtering instead of creating a   ''new''   array.

This method doesn't need as much memory to hold the sparse array.

```rexx
/*REXX program finds all  even  numbers from an array, and marks the  not even  numbers.*/
parse arg N seed .                               /*obtain optional arguments from the CL*/
if N=='' | N==","      then N=50                 /*Not specified?  Then use the default.*/
if datatype(seed,'W')  then call random ,,seed   /*use the RANDOM seed for repeatability*/

               do i=1  for N                     /*generate N random numbers  ──►  OLD  */
               @.i=random(1,99999)               /*generate a random number 1 ──► 99999 */
               end   /*i*/

      do j=1  for N                              /*process the  OLD  array elements.    */
      if @.j//2 \==0  then @.j=                  /*mark the  @  array that it's not even*/
      end   /*j*/

      do k=1  for N                              /*display all the   @   even  numbers. */
      if @.k==''  then iterate                   /*if it's marked not even, then skip it*/
      say right('array.'k, 20) "=" right(@.k,9)  /*display a line  (an array element).  */
      end   /*k*/                                /*stick a fork in it,  we're all done. */
```

'''output'''   when the following is used for input:   <tt> ,   1234567 </tt>

Output is the same as the 1<sup>st</sup> REXX version   (using two arrays).




## Ring

<lang>
aList = [1, 2, 3, 4, 5, 6]
bArray = list(3)
see evenSelect(aList)

func evenSelect aArray
i = 0
for n = 1 to len(aArray)
    if (aArray[n] % 2) = 0
       i = i + 1
       bArray[i] = aArray[n] ok
next
return bArray

```



## Ruby

Enumerable#select is the filter that returns a new Array.


```ruby
# Enumerable#select returns a new array.
ary = [1, 2, 3, 4, 5, 6]
even_ary = ary.select {|elem| elem.even?}
p even_ary # => [2, 4, 6]

# Enumerable#select also works with Range.
range = 1..6
even_ary = range.select {|elem| elem.even?}
p even_ary # => [2, 4, 6]
```



###  Destructive

Array#select! is the destructive version which modifies the original Array.


```ruby
ary = [1, 2, 3, 4, 5, 6]
ary.select! {|elem| elem.even?}
p ary # => [2, 4, 6]
```



## Run BASIC


```runbasic
dim a1(100)
count	= 100
for i = 1 to 100
    a1(i) = int(rnd(0)*100)+1
    count = count - (a1(i) mod 2)
next

'dim the extract and fill it
dim a2(count)
for i = 1 to 100
    if not(a1(i) mod 2) then
        n	= n+1
        a2(n)	= a1(i)
    end if
next

for i = 1 to count
    print a2(i)
next
```



## Rust


```rust
fn main() {
    println!("new vec filtered: ");
    let nums: Vec<i32> = (1..20).collect();
    let evens: Vec<i32> = nums.iter().cloned().filter(|x| x % 2 == 0).collect();
    println!("{:?}", evens);

    // Filter an already existing vector
    println!("original vec filtered: ");
    let mut nums: Vec<i32> = (1..20).collect();
    nums.retain(|x| x % 2 == 0);
    println!("{:?}", nums);
}
```


```txt
new vec filtered:
[2, 4, 6, 8, 10, 12, 14, 16, 18]
original vec filtered:
[2, 4, 6, 8, 10, 12, 14, 16, 18]
```



## Salmon


In this example, [1...10] is a list of the integers from 1 to 10.  The comprehend expression walks over this list and selects only the even elements.  The result of the comprehend expression is a new list with only the even elements.  Then an iterate statement is used to walk over the list of even elements and print them out.


```Salmon
iterate(x; comprehend(y; [1...10]; y % 2 == 0) (y))
    x!;
```


Here's a version that walks an array destructively removing the non-even elements:


```Salmon
variable my_array := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
variable write_position := 0;
iterate (read_position; [0...9])
  {
    immutable elem := my_array[read_position];
    if (elem % 2 == 0)
      {
        my_array[write_position] := elem;
        ++write_position;
      };
  };
// Chop off the end of the array.
my_array := my_array[0...write_position - 1];
iterate(x; my_array)
    x!;
```



## Sather


```sather
class MARRAY{T} < $ARR{T} is
  include ARRAY{T};

  filter_by(r:ROUT{T}:BOOL):SAME is
    o:MARRAY{T} := #;
    loop e ::= elt!;
      if r.call(e) then
        o := o.append(#MARRAY{T}(|e|));
      end;
    end;
    return o;
  end;

end;

class MAIN is
  main is
    a ::= #MARRAY{INT}(|5, 6, 7, 8, 9, 10, 11|);
    sel ::= a.filter_by( bind(_.is_even) );
    loop #OUT + sel.elt! + " "; end;
    #OUT + "\n";
  end;
end;
```



## Scala


```scala
(1 to 100).filter(_ % 2 == 0)
```



## Scheme

In the interactive prompt:

```scheme>
 (filter even? '(1 2 3 4 5 6 7 8 9 10))
(2 4 6 8 10)
```

Or as a function:

```scheme
(define (select-even lst)
  (filter even? lst))

(select-even '(1 2 3 4 5 6 7 8 9 10))
```




## Seed7


```seed7
var array integer: arr is [] (1, 2, 3, 4, 5);
var array integer: evens is 0 times 0;
var integer: number is 0;

for number range arr do
  if not odd(number) then
    evens &:= [] (number);
  end if;
end for;
```



## Sidef


```ruby
var arr = [1,2,3,4,5];

# Creates a new array
var new = arr.grep {|i| i %% 2};
say new.dump;     # => [2, 4]

# Destructive (at variable level)
arr.grep! {|i| i %% 2};
say arr.dump;    # => [2, 4]
```



## SequenceL

Filters are primarily written in SequenceL using partial Indexed Functions.


```sequencel
evens(x(1))[i] := x[i] when x[i] mod 2 = 0;
```


```txt

cmd:>evens(1...5)
[2,4]

```



## Slate


```slate
#(1 2 3 4 5) select: [| :number | number isEven].
```



## Smalltalk


```smalltalk
#(1 2 3 4 5) select: [:number | number even]
```



## SQL

Task: Select certain elements from an Array into a new Array in a generic way. To demonstrate, select all even numbers from an Array.

```sql
--Create the original array (table #nos) with numbers from 1 to 10
create table #nos (v int)
declare @n int set @n=1
while @n<=10 begin insert into #nos values (@n) set @n=@n+1 end

--Select the subset that are even into the new array (table #evens)
select v into #evens from #nos where v % 2 = 0

-- Show #evens
select * from #evens

-- Clean up so you can edit and repeat:
drop table #nos
drop table #evens
```


'{{works with|MySQL}}

```sql
create temporary table nos (v int);
insert into nos values (1),(2),(3),(4),(5),(6),(7),(8),(9),(10);
create temporary table evens (v int);
insert into evens select v from nos where v%2=0;
select * from evens order by v; /*2,4,6,8,10*/
drop table nos;
drop table evens;
```


Or to be shorter, you could create the table evens directly from the query result :

```sql
create temporary table evens select * from nos where v%2=0;
```



## Stata


```stata
mata
a=2,9,4,7,5,3,6,1,8

// Select even elements of a
select(a,mod(a,2):==0)

// Select the indices of even elements of a
selectindex(mod(a,2):==0)
end
```


## Tcl

Tcl doesn't really have a concept of a "number" per se - strictly speaking its only data type is the string (but a string can be interpreted as a number, of course). The generic way of getting certain elements from an array looks roughly like this:


```tcl
foreach key [array names arr] {if { <condition> } then {puts $arr($key)}}
```


In this case, we can do this particular challenge with:

```tcl
foreach {key val} [array get srcArray] {
    if {[string is integer -strict $key] && !($key%2)} {
        set dstArray($key) $val
    }
}
```


If we were using Tcl's lists and interpreting the challenge to mean getting just the elements at index 0, 2, 4, ...

```tcl
foreach {even odd} $srcList {
    lappend dstList $even
}
```



## Swift


```swift
let numbers = [1,2,3,4,5,6]
let even_numbers = numbers.filter { $0 % 2 == 0 }
println(even_numbers)
```

```txt

[2, 4, 6]

```



## Toka


```toka
10 cells is-array table
10 cells is-array even
{
  variable source
  [ swap source ! >r reset r> 0
    [ i source @ array.get
      dup 2 mod 0 <> [ drop ] ifTrue
    ] countedLoop
    depth 0 swap [ i even array.put ] countedLoop
  ]
} is copy-even
10 0 [ i i table array.put ] countedLoop
table 10 copy-even
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
arr="1'4'9'16'25'36'49'64'81'100",even=""
LOOP nr=arr
rest=MOD (nr,2)
IF (rest==0) even=APPEND (even,nr)
ENDLOOP
PRINT even

```

```txt

4'16'36'64'100

```



## UNIX Shell

```bash
a=(1 2 3 4 5)
unset e[@]
for ((i=0;i<${#a[@]};i++)); do
  [ $((a[$i]%2)) -eq 0 ] && e[$i]="${a[$i]}"
done
```


Or, using '''grep''':


```bash
a=(1 2 3 4 5)
read -a e -d\n < <(printf '%s\n' "${a[@]}" | grep '[02468]$')
```


Either way, to display the results:


```bash
echo "${a[@]}"
echo "${e[@]}"
```


```txt
1 2 3 4 5
2 4
```



## UnixPipes


```bash
yes \ | cat -n | while read a; do ; expr $a % 2 >/dev/null && echo $a ; done
```



## Ursala


Ursala doesn't have arrays, except when the run time system
transparently converts a list to an array
as needed for an external math library function call.
However, selection can be done on lists.


###  Unary predicates


The most common way to select items from a list according to a unary
predicate <code>p</code> is to write <code>p*~</code>, as shown below.


```Ursala
#import std
#import nat

x = <89,36,13,15,41,39,21,3,15,92,16,59,52,88,33,65,54,88,93,43>

#cast %nL

y = (not remainder\2)*~ x
```


```txt

<36,92,16,52,88,54,88>

```



###  Binary predicates


Selection is so frequently useful that the language has a couple
of other ways to do it.
Selecting according to a binary predicate can be done like this.

```Ursala
z = (not remainder)~| (36,<1,2,3,4,5,6,7,8,9,10,11,12>)
```

The value of <code>z</code> will be the divisors of 36 appearing in the list.

```txt

<1,2,3,4,6,9,12>

```

This usage has the advantage over writing <code>(not remainder/36)*~</code> with the operator
above that it allows the 36 to be part of the argument rather than
being hard coded into the function.


###  Operator suffixes


Many operators in Ursala allow suffixes that modify their semantics.
For example, the suffix <code>ihB</code> on the identity function <code>~&</code> makes it
<code>~&ihB</code>, a predicate to detect odd numbers by inspecting the binary
representation. If an operator with this kind of suffix is further
modified by appending an <code>F</code>, it becomes a selection filter.
For example

```Ursala
shortcut = ~&ihBF x
```

using the <code>x</code> defined above will evaluate to

```txt

<89,13,15,41,39,21,3,15,59,33,65,93,43>

```

There are also suffixes corresponding to the <code>~|</code> operator.


## V


```v>[even? dup 2 /
int 2 * - zero?].

[1 2 3 4 5 6 7 8 9] [even?] filter
=[2 4 6 8]
```



## VBA


```vb

Option Explicit

Sub Main()
Dim evens() As Long, i As Long
Dim numbers() As Long

    For i = 1 To 100000
        ReDim Preserve numbers(1 To i)
        numbers(i) = i
    Next i

    evens = FilterInNewArray(numbers)

    Debug.Print "Count of initial array : " & UBound(numbers) & ", first item : " & numbers(LBound(numbers)) & ", last item : " & numbers(UBound(numbers))
    Debug.Print "Count of new array : " & UBound(evens) & ", first item : " & evens(LBound(evens)) & ", last item : " & evens(UBound(evens))

    FilterInPlace numbers

    Debug.Print "Count of initial array (filtered): " & UBound(numbers) & ", first item : " & numbers(LBound(numbers)) & ", last item : " & numbers(UBound(numbers))
End Sub

Private Function FilterInNewArray(arr() As Long) As Long()
Dim i As Long, t() As Long, cpt As Long
    For i = LBound(arr) To UBound(arr)
        If IsEven(arr(i)) Then
            cpt = cpt + 1
            ReDim Preserve t(1 To cpt)
            t(cpt) = i
        End If
    Next i
    FilterInNewArray = t
End Function

Private Sub FilterInPlace(arr() As Long)
Dim i As Long, cpt As Long
    For i = LBound(arr) To UBound(arr)
        If IsEven(arr(i)) Then
            cpt = cpt + 1
            arr(cpt) = i
        End If
    Next i
    ReDim Preserve arr(1 To cpt)
End Sub

Private Function IsEven(Number As Long) As Boolean
    IsEven = (CLng(Right(CStr(Number), 1)) And 1) = 0
End Function
```


```txt
Count of initial array : 100000, first item : 1, last item : 100000
Count of new array : 50000, first item : 2, last item : 100000
Count of initial array (filtered): 50000, first item : 2, last item : 100000
```




## VBScript


```vb

test_arr_1 = Array(1,2,3,4,5,6,7,8,9,10)
test_arr_2 = Array(1,2,3,4,5,6,7,8,9,10)

WScript.StdOut.Write "Scenario 1: Create a new array"
WScript.StdOut.WriteLine
WScript.StdOut.Write "Input: " & Join(test_arr_1,",")
WScript.StdOut.WriteLine
WScript.StdOut.Write "Output: " & filter_create(test_arr_1)
WScript.StdOut.WriteBlankLines(2)

WScript.StdOut.Write "Scenario 2: Destructive approach"
WScript.StdOut.WriteLine
WScript.StdOut.Write "Input: " & Join(test_arr_2,",")
WScript.StdOut.WriteLine
WScript.StdOut.Write "Output: " & filter_destruct(test_arr_2)
WScript.StdOut.WriteBlankLines(2)

Function filter_create(arr)
	ReDim arr_new(0)
	For i = 0 To UBound(arr)
		If arr(i) Mod 2 = 0 Then
			If arr_new(0) = "" Then
				arr_new(0) = arr(i)
			Else
				ReDim Preserve arr_new(UBound(arr_new)+1)
				arr_new(UBound(arr_new)) = arr(i)
			End If
		End If
	Next
	filter_create = Join(arr_new,",")
End Function

Function filter_destruct(arr)
	count = 0
	For i = 0 To UBound(arr)
		If arr(i) Mod 2 <> 0 Then
			count = count + 1
			For j = i To UBound(arr)
				If j + 1 <= UBound(arr) Then
					arr(j) = arr(j+1)
				End If
			Next
		End If
	Next
	ReDim Preserve arr(UBound(arr)-count)
	filter_destruct = Join(arr,",")
End Function
```


```txt

Scenario 1: Create a new array
Input: 1,2,3,4,5,6,7,8,9,10
Output: 2,4,6,8,10

Scenario 2: Destructive approach
Input: 1,2,3,4,5,6,7,8,9,10
Output: 2,4,6,8,10

```



## Visual Basic .NET

```vbnet
Module Filter

    Sub Main()
        Dim array() As Integer = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
        Dim newEvenArray() As Integer

        Console.WriteLine("Current Array:")
        For Each i As Integer In array
            Console.WriteLine(i)
        Next

        newEvenArray = filterArrayIntoNewArray(array)

        Console.WriteLine("New Filtered Array:")
        For Each i As Integer In newEvenArray
            Console.WriteLine(i)
        Next

        array = changeExistingArray(array)

        Console.WriteLine("Orginal Array After Filtering:")
        For Each i As Integer In array
            Console.WriteLine(i)
        Next
    End Sub

    Private Function changeExistingArray(array() As Integer) As Integer()
        Return filterArrayIntoNewArray(array)
    End Function

    Private Function filterArrayIntoNewArray(array() As Integer) As Integer()
        Dim result As New List(Of Integer)
        For Each element As Integer In array
            If element Mod 2 = 0 Then
                result.Add(element)
            End If
        Next
        Return result.ToArray
    End Function

End Module

```


```txt

Current Array:
1
2
3
4
5
6
7
8
9
10
New Filtered Array:
2
4
6
8
10
Orginal Array After Filtering:
2
4
6
8
10

```



## WDTE


```WDTE>let a =
 import 'arrays';
let s => import 'stream';

a.stream [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
-> s.filter (@ even n => % n 2 -> == 0)
-> s.collect
-- io.writeln io.stdout
;
```


```txt
[2; 4; 6; 8; 10]
```


Doing this in a destructive manner is not possible normally in WDTE as everything is immutable.


## Wrapl


```wrapl
VAR a <- ALL 1:to(10);
```

<tt>a</tt> will be the list <tt>[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]</tt>

```wrapl
VAR e <- ALL a:values \ $ % 2 = 0;
```

<tt>e</tt> will be the list <tt>[2, 4, 6, 8, 10]</tt>


## XPL0

This uses the kludge of making the first element of an array its size.
There is no 'sizeof' operator, unfortunately.


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

proc Filter(A, B, Option);      \Select all even numbers from array A
int  A, B, Option;              \ and return them in B, unless Option = true
int  I, J;
[J:= 0;
for I:= 1 to A(0) do
        if (A(I)&1) = 0 then
                [J:= J+1;
                if Option then
                        A(J):= A(I)
                else    B(J):= A(I);
                ];
if Option then A(0):= J else B(0):= J;
];

int Array, Evens(11), I;
[Array:= [10, 3, 1, 4, 1, 5, 9, 2, 6, 5, 4];
Filter(Array, Evens, false);
for I:= 1 to Evens(0) do
        [IntOut(0, Evens(I));  ChOut(0, ^ )];
CrLf(0);

Filter(Array, Evens \not used\, true);
for I:= 1 to Array(0) do
        [IntOut(0, Array(I));  ChOut(0, ^ )];
CrLf(0);
]
```


```txt

4 2 6 4
4 2 6 4

```



## XQuery


```xquery

(: Sequence of numbers from 1 to 10 :)
let $array := (1 to 10)

(: Short version :)
let $short := $array[. mod 2 = 0]

(: Long version with a FLWOR expression :)
let $long := for $value in $array
             where $value mod 2 = 0
             return $value

(: Show the results :)
return
  <result>
    <short>{$short}</short>
    <long>{$long}</long>
  </result>

```


```xml

<?xml version="1.0" encoding="UTF-8"?>
<result>
   <short>2 4 6 8 10</short>
   <long>2 4 6 8 10</long>
</result>

```



## XSLT


```xml
<xsl:for-each select="nodes[@value mod 2 = 0]">
  <xsl:value-of select="@value" />
</xsl:for-each>
```



## zkl


```zkl
T(1,4,9,16,25,36,"37",49,64,81,100, True,self)
   .filter(fcn(n){(0).isType(n) and n.isOdd})
//-->L(1,9,25,49,81)
```



## ZX Spectrum Basic


```zxbasic
10 LET items=100: LET filtered=0
20 DIM a(items)
30 FOR i=1 TO items
40 LET a(i)=INT (RND*items)
50 NEXT i
60 FOR i=1 TO items
70 IF FN m(a(i),2)=0 THEN LET filtered=filtered+1: LET a(filtered)=a(i)
80 NEXT i
90 DIM b(filtered)
100 FOR i=1 TO filtered
110 LET b(i)=a(i): PRINT b(i);" ";
120 NEXT i
130 DIM a(1): REM To free memory (well, almost all)
140 DEF FN m(a,b)=a-INT (a/b)*b
```
