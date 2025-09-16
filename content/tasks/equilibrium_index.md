+++
title = "Equilibrium index"
description = ""
date = 2019-10-18T11:58:34Z
aliases = []
[extra]
id = 8361
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "abap",
  "ada",
  "aime",
  "algol_68",
  "applescript",
  "autohotkey",
  "awk",
  "batch_file",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elena",
  "elixir",
  "erre",
  "euphoria",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "int_index_void",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "liberty_basic",
  "logo",
  "matlab",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "seed7",
  "sidef",
  "tcl",
  "ursala",
  "vbscript",
  "xpl0",
  "yorick",
  "zkl",
  "zx_spectrum_basic",
]
+++

An equilibrium index of a sequence is an index into the sequence such that the sum of elements at lower indices is equal to the sum of elements at higher indices.


For example, in a sequence   <big><math>A</math></big>:

:::::   <big><math>A_0 = -7</math></big>
:::::   <big><math>A_1 =  1</math></big>
:::::   <big><math>A_2 =  5</math></big>
:::::   <big><math>A_3 =  2</math></big>
:::::   <big><math>A_4 = -4</math></big>
:::::   <big><math>A_5 =  3</math></big>
:::::   <big><math>A_6 =  0</math></big>

3   is an equilibrium index, because:

:::::   <big><math>A_0 + A_1 + A_2 = A_4 + A_5 + A_6</math></big>

6   is also an equilibrium index, because:

:::::   <big><math>A_0 + A_1 + A_2 + A_3 + A_4 + A_5 = 0</math></big>

(sum of zero elements is zero)

7   is not an equilibrium index, because it is not a valid index of sequence <big><math>A</math></big>.


;Task;
Write a function that, given a sequence, returns its equilibrium indices (if any).

Assume that the sequence may be very long.





## 11l

```11l
F eqindex(arr)
   R (0 .< arr.len).filter(i -> sum(@arr[0.<i]) == sum(@arr[i+1..]))

print(eqindex([-7, 1, 5, 2, -4, 3, 0]))
```


```txt
[3, 6]
```



## ABAP


```ABAP
REPORT equilibrium_index.

TYPES: y_i TYPE STANDARD TABLE OF i WITH EMPTY KEY.

cl_demo_output=>display( REDUCE y_i( LET sequences = VALUE y_i( ( -7 ) ( 1 ) ( 5 ) ( 2 ) ( -4 ) ( 3 ) ( 0 ) )
                                         total_sum = REDUCE #( INIT sum = 0
                                                                FOR sequence IN sequences
                                                               NEXT sum = sum + ( sequence ) ) IN
                                      INIT x = VALUE y_i( )
                                           y = 0
                                       FOR i = 1 UNTIL i > lines( sequences )
                                       LET z = sequences[ i ] IN
                                      NEXT x = COND #( WHEN y = ( total_sum - y - z ) THEN VALUE y_i( BASE x ( i - 1 ) ) ELSE x )
                                           y = y + z ) ).
```



## Ada

Generic solution that returns a Vector of Indices.

equilibrium.ads:

```Ada
with Ada.Containers.Vectors;

generic
   type Index_Type is range <>;
   type Element_Type is private;
   Zero : Element_Type;
   with function "+" (Left, Right : Element_Type) return Element_Type is <>;
   with function "-" (Left, Right : Element_Type) return Element_Type is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   type Array_Type is private;
   with function Element (From : Array_Type; Key : Index_Type) return Element_Type is <>;
package Equilibrium is
   package Index_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Index_Type);

   function Get_Indices (From : Array_Type) return Index_Vectors.Vector;

end Equilibrium;
```

equilibrium.adb:

```Ada
package body Equilibrium is

   function Get_Indices (From : Array_Type) return Index_Vectors.Vector is
      Result : Index_Vectors.Vector;
      Right_Sum, Left_Sum : Element_Type := Zero;
   begin
      for Index in Index_Type'Range loop
         Right_Sum := Right_Sum + Element (From, Index);
      end loop;
      for Index in Index_Type'Range loop
         Right_Sum := Right_Sum - Element (From, Index);
         if Left_Sum = Right_Sum then
            Index_Vectors.Append (Result, Index);
         end if;
         Left_Sum := Left_Sum + Element (From, Index);
      end loop;
      return Result;
   end Get_Indices;

end Equilibrium;
```

Test program using two different versions, one with vectors and one with arrays:

```Ada
with Ada.Text_IO;
with Equilibrium;
with Ada.Containers.Vectors;

procedure Main is
   subtype Index_Type is Positive range 1 .. 7;
   package Vectors is new Ada.Containers.Vectors
      (Element_Type => Integer, Index_Type => Index_Type);
   type Plain_Array is array (Index_Type) of Integer;
   function Element (From : Plain_Array; Key : Index_Type) return Integer is
   begin
      return From (Key);
   end Element;

   package Vector_Equilibrium is new Equilibrium
      (Index_Type => Index_Type,
       Element_Type => Integer,
       Zero => 0,
       Array_Type => Vectors.Vector,
       Element => Vectors.Element);
   package Array_Equilibrium is new Equilibrium
      (Index_Type => Index_Type,
       Element_Type => Integer,
       Zero => 0,
       Array_Type => Plain_Array);

   My_Vector : Vectors.Vector;
   My_Array : Plain_Array := (-7, 1, 5, 2, -4, 3, 0);
   Vector_Result : Vector_Equilibrium.Index_Vectors.Vector;
   Array_Result : Array_Equilibrium.Index_Vectors.Vector :=
      Array_Equilibrium.Get_Indices (My_Array);
begin
   Vectors.Append (My_Vector, -7);
   Vectors.Append (My_Vector, 1);
   Vectors.Append (My_Vector, 5);
   Vectors.Append (My_Vector, 2);
   Vectors.Append (My_Vector, -4);
   Vectors.Append (My_Vector, 3);
   Vectors.Append (My_Vector, 0);
   Vector_Result := Vector_Equilibrium.Get_Indices (My_Vector);
   Ada.Text_IO.Put_Line ("Results:");
   Ada.Text_IO.Put ("Array: ");
   for I in Array_Result.First_Index .. Array_Result.Last_Index loop
      Ada.Text_IO.Put (Integer'Image (Array_Equilibrium.Index_Vectors.Element (Array_Result, I)));
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("Vector: ");
   for I in Vector_Result.First_Index .. Vector_Result.Last_Index loop
      Ada.Text_IO.Put (Integer'Image (Vector_Equilibrium.Index_Vectors.Element (Vector_Result, I)));
   end loop;
   Ada.Text_IO.New_Line;
end Main;
```

(Index_Type is based on 1):

```txt
Results:
Array:  4 7
Vector:  4 7
```


Version that works with Ada 95, too:
equilibrium.adb:

```Ada
with Ada.Text_IO;

procedure Equilibrium is

   type Integer_Sequence is array (Positive range <>) of Integer;
   function Seq_Img (From : Integer_Sequence) return String is
   begin
      if From'First /= From'Last then
         return " " & Integer'Image (From (From'First)) &
            Seq_Img (From (From'First + 1 .. From'Last));
      else
         return " " & Integer'Image (From (From'First));
      end if;
   end Seq_Img;

   type Boolean_Sequence is array (Positive range <>) of Boolean;
   function Seq_Img (From : Boolean_Sequence) return String is
   begin
      if From'First > From'Last then
         return "";
      end if;
      if From (From'First) then
         return Integer'Image (From'First) &
            Seq_Img (From (From'First + 1 .. From'Last));
      else
         return Seq_Img (From (From'First + 1 .. From'Last));
      end if;
   end Seq_Img;

   function Get_Indices (From : Integer_Sequence) return Boolean_Sequence is
      Result : Boolean_Sequence (From'Range) := (others => False);
      Left_Sum, Right_Sum : Integer := 0;
   begin
      for Index in From'Range loop
         Right_Sum := Right_Sum + From (Index);
      end loop;
      for Index in From'Range loop
         Right_Sum := Right_Sum - From (Index);
         Result (Index) := Left_Sum = Right_Sum;
         Left_Sum  := Left_Sum  + From (Index);
      end loop;
      return Result;
   end Get_Indices;

   X1 : Integer_Sequence := (-7,  1, 5,  2, -4,  3, 0);
   X1_Result : Boolean_Sequence := Get_Indices (X1);
   X2 : Integer_Sequence := ( 2,  4, 6);
   X2_Result : Boolean_Sequence := Get_Indices (X2);
   X3 : Integer_Sequence := ( 2,  9, 2);
   X3_Result : Boolean_Sequence := Get_Indices (X3);
   X4 : Integer_Sequence := ( 1, -1, 1, -1,  1 ,-1, 1);
   X4_Result : Boolean_Sequence := Get_Indices (X4);

begin
   Ada.Text_IO.Put_Line ("Results:");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("X1:" & Seq_Img (X1));
   Ada.Text_IO.Put_Line ("Eqs:" & Seq_Img (X1_Result));
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("X2:" & Seq_Img (X2));
   Ada.Text_IO.Put_Line ("Eqs:" & Seq_Img (X2_Result));
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("X3:" & Seq_Img (X3));
   Ada.Text_IO.Put_Line ("Eqs:" & Seq_Img (X3_Result));
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("X4:" & Seq_Img (X4));
   Ada.Text_IO.Put_Line ("Eqs:" & Seq_Img (X4_Result));
end Equilibrium;
```

```txt
Results:

X1: -7  1  5  2 -4  3  0
Eqs: 4 7

X2:  2  4  6
Eqs:

X3:  2  9  2
Eqs: 2

X4:  1 -1  1 -1  1 -1  1
Eqs: 1 2 3 4 5 6 7
```




## Aime


```aime
list
eqindex(list l)
{
    integer e, i, s, sum;
    list x;

    s = sum = 0;
    l.ucall(add_i, 1, sum);
    for (i, e in l) {
        if (s * 2 + e == sum) {
            x.append(i);
        }
        s += e;
    }

    x;
}

integer
main(void)
{
    list(-7, 1, 5, 2, -4, 3, 0).eqindex.ucall(o_, 0, "\n");

    0;
}
```




## ALGOL 68

```algol68
MODE YIELDINT = PROC(INT)VOID;

PROC gen equilibrium index = ([]INT arr, YIELDINT yield)VOID:
(
    INT sum := 0;
    FOR i FROM LWB arr TO UPB arr DO
        sum +:= arr[i]
    OD;

    INT left:=0, right:=sum;
    FOR i FROM LWB arr TO UPB arr DO
        right -:= arr[i];
        IF left = right THEN yield(i) FI;
        left +:= arr[i]
    OD
);

test:(
  []INT arr = []INT(-7, 1, 5, 2, -4, 3, 0)[@0];
# FOR INT index IN # gen equilibrium index(arr, # ) DO ( #
##   (INT index)VOID:
     print(index)
# OD # );
  print(new line)
)
```

```txt

         +3         +6

```



## AppleScript

{{Trans|JavaScript}}(ES6 version)

```applescript
-- equilibriumIndices :: [Int] -> [Int]
on equilibriumIndices(xs)

    script balancedPair
        on |λ|(a, pair, i)
            set {x, y} to pair
            if x = y then
                {i - 1} & a
            else
                a
            end if
        end |λ|
    end script

    script plus
        on |λ|(a, b)
            a + b
        end |λ|
    end script

    -- Fold over zipped pairs of sums from left
    -- and sums from right

    foldr(balancedPair, {}, ¬
        zip(scanl1(plus, xs), scanr1(plus, xs)))

end equilibriumIndices

-- TEST -----------------------------------------------------------------------
on run

    map(equilibriumIndices, {¬
        {-7, 1, 5, 2, -4, 3, 0}, ¬
        {2, 4, 6}, ¬
        {2, 9, 2}, ¬
        {1, -1, 1, -1, 1, -1, 1}, ¬
        {1}, ¬
        {}})

    --> {{3, 6}, {}, {1}, {0, 1, 2, 3, 4, 5, 6}, {0}, {}}
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

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

-- foldr :: (a -> b -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldr

-- init :: [a] -> [a]
on init(xs)
    set lng to length of xs
    if lng > 1 then
        items 1 thru -2 of xs
    else if lng > 0 then
        {}
    else
        missing value
    end if
end init

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

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

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

-- scanl :: (b -> a -> b) -> b -> [a] -> [b]
on scanl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        set lst to {startValue}
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
            set end of lst to v
        end repeat
        return lst
    end tell
end scanl

-- scanl1 :: (a -> a -> a) -> [a] -> [a]
on scanl1(f, xs)
    if length of xs > 0 then
        scanl(f, item 1 of xs, tail(xs))
    else
        {}
    end if
end scanl1

-- scanr :: (b -> a -> b) -> b -> [a] -> [b]
on scanr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        set lst to {startValue}
        repeat with i from lng to 1 by -1
            set v to |λ|(v, item i of xs, i, xs)
            set end of lst to v
        end repeat
        return reverse of lst
    end tell
end scanr

-- scanr1 :: (a -> a -> a) -> [a] -> [a]
on scanr1(f, xs)
    if length of xs > 0 then
        scanr(f, item -1 of xs, init(xs))
    else
        {}
    end if
end scanr1

-- tail :: [a] -> [a]
on tail(xs)
    if length of xs > 1 then
        items 2 thru -1 of xs
    else
        {}
    end if
end tail

-- zip :: [a] -> [b] -> [(a, b)]
on zip(xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    repeat with i from 1 to lng
        set end of lst to {item i of xs, item i of ys}
    end repeat
    return lst
end zip
```

```txt
```



## AutoHotkey


```AutoHotkey
Equilibrium_index(list, BaseIndex=0){
	StringSplit, A, list, `,
	Loop % A0 {
		i := A_Index	, Pre := Post := 0
		loop, % A0
			if (A_Index < i)
				Pre += A%A_Index%
			else if (A_Index > i)
				Post += A%A_Index%
		if (Pre = Post)
			Res .= (Res?", ":"") i - (BaseIndex?0:1)
	}
	return Res
}
```

Examples:
```AutoHotkey
list = -7, 1, 5, 2, -4, 3, 0
MsgBox % Equilibrium_index(list)
```

```txt
3, 6
```



## AWK


```AWK

# syntax: GAWK -f EQUILIBRIUM_INDEX.AWK
BEGIN {
    main("-7 1 5 2 -4 3 0")
    main("2 4 6")
    main("2 9 2")
    main("1 -1 1 -1 1 -1 1")
    exit(0)
}
function main(numbers,  x) {
    x = equilibrium(numbers)
    printf("numbers: %s\n",numbers)
    printf("indices: %s\n\n",length(x)==0?"none":x)
}
function equilibrium(numbers,  arr,i,leftsum,leng,str,sum) {
    leng = split(numbers,arr," ")
    for (i=1; i<=leng; i++) {
      sum += arr[i]
    }
    for (i=1; i<=leng; i++) {
      sum -= arr[i]
      if (leftsum == sum) {
        str = str i " "
      }
      leftsum += arr[i]
    }
    return(str)
}

```

<p>Output:</p>

```txt

numbers: -7 1 5 2 -4 3 0
indices: 4 7

numbers: 2 4 6
indices: none

numbers: 2 9 2
indices: 2

numbers: 1 -1 1 -1 1 -1 1
indices: 1 2 3 4 5 6 7

```


## Batch File


```dos
@echo off
setlocal enabledelayedexpansion

call :equilibrium-index "-7 1 5 2 -4 3 0"
call :equilibrium-index "2 4 6"
call :equilibrium-index "2 9 2"
call :equilibrium-index "1 -1 1 -1 1 -1 1"
pause>nul
exit /b

	%== The Function ==%
:equilibrium-index <sequence with quotes>
	::Set the pseudo-array sequence...
set "seq=%~1"
set seq.length=0
for %%S in (!seq!) do (
	set seq[!seq.length!]=%%S
	set /a seq.length+=1
)
	::Initialization of other variables...
set "equilms="
set /a last=seq.length - 1
	::The main checking...
for /l %%e in (0,1,!last!) do (
	set left=0
	set right=0

	for /l %%i in (0,1,!last!) do (
		if %%i lss %%e (set /a left+=!seq[%%i]!)
		if %%i gtr %%e (set /a right+=!seq[%%i]!)
	)
	if !left!==!right! (
		if defined equilms (
			set "equilms=!equilms! %%e"
		) else (
			set "equilms=%%e"
		)
	)
)
echo [!equilms!]
goto :EOF
	%==/The Function ==%
```

```txt
[3 6]
[]
[1]
[0 1 2 3 4 5 6]
```



## BBC BASIC

BBC BASIC's '''SUM''' function is useful for this task.

```bbcbasic
      DIM list(6)
      list() = -7, 1, 5, 2, -4, 3, 0
      PRINT "Equilibrium indices are " FNequilibrium(list())
      END

      DEF FNequilibrium(l())
      LOCAL i%, r, s, e$
      s = SUM(l())
      FOR i% = 0 TO DIM(l(),1)
        IF r = s - r - l(i%) THEN e$ += STR$(i%) + ","
        r += l(i%)
      NEXT
      = LEFT$(e$)
```

'''Output:'''

```txt

Equilibrium indices are 3,6

```



## C


```c
#include <stdio.h>
#include <stdlib.h>

int list[] = {-7, 1, 5, 2, -4, 3, 0};

int eq_idx(int *a, int len, int **ret)
{
	int i, sum, s, cnt;
	/* alloc long enough: if we can afford the original list,
	 * we should be able to afford to this.  Beats a potential
         * million realloc() calls.  Even if memory is a real concern,
         * there's no garantee the result is shorter than the input anyway */
        cnt = s = sum = 0;
	*ret = malloc(sizeof(int) * len);

	for (i = 0; i < len; i++)
                sum += a[i];

	for (i = 0; i < len; i++) {
		if (s * 2 + a[i] == sum) {
			(*ret)[cnt] = i;
                        cnt++;
                }
		s += a[i];
	}

        /* uncouraged way to use realloc since it can leak memory, for example */
	*ret = realloc(*ret, cnt * sizeof(int));

	return cnt;
}

int main()
{
	int i, cnt, *idx;
	cnt = eq_idx(list, sizeof(list) / sizeof(int), &idx);

	printf("Found:");
	for (i = 0; i < cnt; i++)
                printf(" %d", idx[i]);
	printf("\n");

	return 0;
}
```



## C++


```cpp
#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>

template <typename T>
std::vector<size_t> equilibrium(T first, T last)
{
    typedef typename std::iterator_traits<T>::value_type value_t;

    value_t left  = 0;
    value_t right = std::accumulate(first, last, value_t(0));
    std::vector<size_t> result;

    for (size_t index = 0; first != last; ++first, ++index)
    {
        right -= *first;
        if (left == right)
        {
            result.push_back(index);
        }
        left += *first;
    }
    return result;
}

template <typename T>
void print(const T& value)
{
    std::cout << value << "\n";
}

int main()
{
    const int data[] = { -7, 1, 5, 2, -4, 3, 0 };

    std::vector<size_t> indices(equilibrium(data, data + 7));

    std::for_each(indices.begin(), indices.end(), print<size_t>);
}
```

```txt

3
6

```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static IEnumerable<int> EquilibriumIndices(IEnumerable<int> sequence)
    {
        var left = 0;
        var right = sequence.Sum();
        var index = 0;
        foreach (var element in sequence)
        {
            right -= element;
            if (left == right)
            {
                yield return index;
            }
            left += element;
            index++;
        }
    }

    static void Main()
    {
        foreach (var index in EquilibriumIndices(new[] { -7, 1, 5, 2, -4, 3, 0 }))
        {
            Console.WriteLine(index);
        }
    }
}
```

<lang>3
6
```



## Clojure

```clojure
(defn equilibrium [lst]
  (loop [acc '(), i 0, left 0, right (apply + lst), lst lst]
     (if (empty? lst)
	 (reverse acc)
	 (let [[x & xs] lst
	       right    (- right x)
	       acc      (if (= left right) (cons i acc) acc)]
	   (recur acc (inc i) (+ left x) right xs)))))
```

```txt

> (equilibrium [-7, 1, 5, 2, -4, 3, 0])
(3 6)

```



## Common Lisp


```lisp
(defun dflt-on-nil (v dflt)
  (if v v dflt))

(defun eq-index (v)
  (do*
       ((stack nil)
        (i 0 (+ 1 i))
        (rest v (cdr rest))
        (lsum 0)
        (rsum (apply #'+ (cdr v))))
       ;; Reverse here is not strictly necessary
       ((null rest) (reverse stack))
    (if (eql lsum rsum) (push i stack))
    (setf lsum (+ lsum (car rest)))
    (setf rsum (- rsum (dflt-on-nil (cadr rest) 0)))))
```

<lang>(eq-index '(-7 1 5 2 -4 3 0))
(3 6)
```



## D


### More Functional Style


```d
import std.stdio, std.algorithm, std.range, std.functional;

auto equilibrium(Range)(Range r) pure nothrow @safe /*@nogc*/ {
    return r.length.iota.filter!(i => r[0 .. i].sum == r[i + 1 .. $].sum);
}

void main() {
    [-7, 1, 5, 2, -4, 3, 0].equilibrium.writeln;
}
```

```txt
[3, 6]
```



### Less Functional Style

Same output.

```d
import std.stdio, std.algorithm;

size_t[] equilibrium(T)(in T[] items) @safe pure nothrow {
    size_t[] result;
    T left = 0, right = items.sum;

    foreach (immutable i, e; items) {
        right -= e;
        if (right == left)
            result ~= i;
        left += e;
    }
    return result;
}

void main() {
    [-7, 1, 5, 2, -4, 3, 0].equilibrium.writeln;
}
```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'routines;
import system'collections;
import extensions'routines;

class EquilibriumEnumerator : Enumerator
{
    int        left;
    int        right;
    int        index;
    Enumerator en;

    constructor(Enumerator en)
    {
        this en := en;

        self.reset()
    }

    constructor(Enumerable list)
        <= (list.enumerator());

    constructor(o)
        <= (cast Enumerable(o));

    bool next()
    {
        index += 1;

        while(en.next())
        {
            var element := en.get();
            right -= element;
            bool found := (left == right);
            left += element;

            if (found)
            {
                ^ true
            };

            index += 1
        };

        ^ false
    }

    reset()
    {
        en.reset();

        left := 0;
        right := en.summarize();
        index := -1;

        en.reset();
    }

    get() = index;

    enumerable() => en;
}

public program()
{
    new EquilibriumEnumerator(new int[]::( -7, 1, 5, 2, -4, 3, 0 ))
        .forEach:printingLn
}
```


```txt

3
6

```



## Elixir

computes either side each time.

```elixir
defmodule Equilibrium do
  def index(list) do
    last = length(list)
    Enum.filter(0..last-1, fn i ->
      Enum.sum(Enum.slice(list, 0, i)) == Enum.sum(Enum.slice(list, i+1..last))
    end)
  end
end
```


faster version:

```elixir
defmodule Equilibrium do
  def index(list), do: index(list,0,0,Enum.sum(list),[])

  defp index([],_,_,_,acc), do: Enum.reverse(acc)
  defp index([h|t],i,left,right,acc) when left==right-h, do: index(t,i+1,left+h,right-h,[i|acc])
  defp index([h|t],i,left,right,acc)                   , do: index(t,i+1,left+h,right-h,acc)
end
```


'''Test:'''

```elixir
indices = [
  [-7, 1, 5, 2,-4, 3, 0],
  [2, 4, 6],
  [2, 9, 2],
  [1,-1, 1,-1, 1,-1, 1]
]
Enum.each(indices, fn list ->
  IO.puts "#{inspect list} => #{inspect Equilibrium.index(list)}"
end)
```


```txt

[-7, 1, 5, 2, -4, 3, 0] => [3, 6]
[2, 4, 6] => []
[2, 9, 2] => [1]
[1, -1, 1, -1, 1, -1, 1] => [0, 1, 2, 3, 4, 5, 6]

```



## ERRE


```ERRE

PROGRAM EQUILIBRIUM

DIM LISTA[6]

PROCEDURE EQ(LISTA[]->RES$)
   LOCAL I%,R,S,E$
   FOR I%=0 TO UBOUND(LISTA,1) DO
      S+=LISTA[I%]
   END FOR
   FOR I%=0 TO UBOUND(LISTA,1) DO
      IF R=S-R-LISTA[I%] THEN E$+=STR$(I%)+"," END IF
      R+=LISTA[I%]
   END FOR
   RES$=LEFT$(E$,LEN(E$)-1)
END PROCEDURE

BEGIN
   LISTA[]=(-7,1,5,2,-4,3,0)
   EQ(LISTA[]->RES$)
   PRINT("Equilibrium indices are";RES$)
END PROGRAM

```

'''Output:'''

```txt

Equilibrium indices are 3, 6

```



## Euphoria


```euphoria
function equilibrium(sequence s)
    integer lower_sum, higher_sum
    sequence indices
    lower_sum = 0
    higher_sum = 0
    for i = 1 to length(s) do
        higher_sum += s[i]
    end for
    indices = {}
    for i = 1 to length(s) do
        higher_sum -= s[i]
        if lower_sum = higher_sum then
            indices &= i
        end if
        lower_sum += s[i]
    end for
    return indices
end function

? equilibrium({-7,1,5,2,-4,3,0})
```

''(Remember that indices are 1-based in Euphoria)''

```txt
{4,7}
```



## Factor

Executed in the listener. Note that <code>accum-left</code> and <code>accum-right</code> have different outputs than <code>accumulate</code> as they drop the final result.

```factor
USE: math.vectors
: accum-left ( seq id quot -- seq ) accumulate nip ; inline
: accum-right ( seq id quot -- seq ) [ <reversed> ] 2dip accum-left <reversed> ; inline
: equilibrium-indices ( seq -- inds )
  0 [ + ] [ accum-left ] [ accum-right ] 3bi [ = ] 2map
  V{ } swap dup length iota [ [ suffix ] curry [ ] if ] 2each ;
```

```factor
( scratchpad ) { -7 1 5 2 -4 3 0 } equilibrium-indices .
V{ 3 6 }
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Equilibrium_index this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran

Array indices are 1-based.

```fortran
program Equilibrium
  implicit none

  integer :: array(7) = (/ -7, 1, 5, 2, -4, 3, 0 /)

  call equil_index(array)

contains

subroutine equil_index(a)
  integer, intent(in) :: a(:)
  integer :: i

  do i = 1, size(a)
    if(sum(a(1:i-1)) == sum(a(i+1:size(a)))) write(*,*) i
  end do

end subroutine
end program
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub equilibriumIndices (a() As Integer, b() As Integer)
  If UBound(a) = -1 Then Return  '' empty array

  Dim sum As Integer = 0
  Dim count As Integer = 0
  For i As Integer = LBound(a) To UBound(a) : sum += a(i) : Next
  Dim sumLeft As Integer = 0, sumRight As Integer = 0

  For i As Integer = LBound(a) To UBound(a)
     sumRight = sum - sumLeft - a(i)
     If sumLeft = sumRight Then
       Redim Preserve b(0 To Count)
       b(count) = i
       count += 1
     End If
     sumLeft += a(i)
  Next
End Sub

Dim a(0 To 6) As Integer = { -7, 1, 5, 2, -4, 3, 0 }
Dim b() As Integer
equilibriumIndices a(), b()
If UBound(b) = -1 Then
  Print "There are no equilibrium indices"
ElseIf UBound(b) = LBound(b) Then
  Print "The only equilibrium index is : "; b(LBound(b))
Else
  Print "The equilibrium indices are : "
  For i As Integer = LBound(b) To UBound(b) : Print b(i); " "; : Next
End If

Print
Print "Press any key to quit"
Sleep
```


```txt

The equilibrium indices are :
 3  6

```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    fmt.Println(ex([]int32{-7, 1, 5, 2, -4, 3, 0}))

    // sequence of 1,000,000 random numbers, with values
    // chosen so that it will be likely to have a couple
    // of equalibrium indexes.
    rand.Seed(time.Now().UnixNano())
    verylong := make([]int32, 1e6)
    for i := range verylong {
        verylong[i] = rand.Int31n(1001) - 500
    }
    fmt.Println(ex(verylong))
}

func ex(s []int32) (eq []int) {
    var r, l int64
    for _, el := range s {
        r += int64(el)
    }
    for i, el := range s {
        r -= int64(el)
        if l == r {
            eq = append(eq, i)
        }
        l += int64(el)
    }
    return
}
```

```txt

[3 6]
[145125 947872]

```



## Haskell


```haskell
import System.Random (randomRIO)
import Data.List (elemIndices, takeWhile)
import Control.Monad (replicateM)
import Control.Arrow ((&&&))

equilibr xs =
  elemIndices True .
  map (\(a, b) -> sum a == sum b) . takeWhile (not . null . snd) $
  flip ((&&&) <$> take <*> (drop . pred)) xs <$> [1 ..]

langeSliert = replicateM 2000 (randomRIO (-15, 15) :: IO Int) >>= print . equilibr
```

Small example

```haskell
*Main> equilibr [-7, 1, 5, 2, -4, 3, 0]
[3,6]
```

Long random list in langeSliert (several tries for this one)

```haskell
*Main> langeSliert
[231,245,259,265,382,1480,1611,1612]
```



Or, using default Prelude functions:


```haskell
equilibriumIndices :: [Int] -> [Int]
equilibriumIndices xs =
  foldr
    (\(x, y, i) a ->
        (if x == y
           then i : a
           else a))
    []
    (zip3
       (scanl1 (+) xs) -- Sums from the left
       (scanr1 (+) xs) -- Sums from the right
       [0 ..] -- Indices
     )

-- TEST -----------------------------------------------------------------------
main :: IO ()
main =
  mapM_
    print
    (equilibriumIndices <$>
     [ [-7, 1, 5, 2, -4, 3, 0]
     , [2, 4, 6]
     , [2, 9, 2]
     , [1, -1, 1, -1, 1, -1, 1]
     , [1]
     , []
     ])
```

```txt
[3,6]
[]
[1]
[0,1,2,3,4,5,6]
[0]
[]
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
L := if *arglist > 0 then arglist else [-7, 1, 5, 2, -4, 3, 0]   # command line args or default
every writes( "equilibrium indicies of [ " | (!L ||" ") | "] = " | (eqindex(L)||" ") | "\n" )
end

procedure eqindex(L) # generate equilibrium points in a list L or fail
local s,l,i

every (s := 0, i := !L) do
   s +:= numeric(i) | fail              # sum and validate

every (l := 0, i := 1 to *L) do {
   if l = (s-L[i])/2 then suspend i
   l +:= L[i]                           # sum of left side
   }
end
```

```txt
equilibrium indicies of [ -7 1 5 2 -4 3 0 ] = 4 7
```



## J


```j
equilidx=: +/\ I.@:= +/\.
```

```j
   equilidx _7 1 5 2 _4 3 0
3 6
```



## Java

```java5

public class Equlibrium {
	public static void main(String[] args) {
		int[] sequence = {-7, 1, 5, 2, -4, 3, 0};
		equlibrium_indices(sequence);
	}

	public static void equlibrium_indices(int[] sequence){
		//find total sum
		int totalSum = 0;
		for (int n : sequence) {
			totalSum += n;
		}
		//compare running sum to remaining sum to find equlibrium indices
		int runningSum = 0;
		for (int i = 0; i < sequence.length; i++) {
			int n = sequence[i];
			if (totalSum - runningSum - n == runningSum) {
				System.out.println(i);
			}
			runningSum += n;
		}
	}
}

```

```txt

3
6

```



## JavaScript



### ES5


```javascript
function equilibrium(a) {
  var N = a.length, i, l = [], r = [], e = []
  for (l[0] = a[0], r[N - 1] = a[N - 1], i = 1; i<N; i++)
    l[i] = l[i - 1] + a[i], r[N - i - 1] = r[N - i] + a[N - i - 1]
  for (i = 0; i < N; i++)
    if (l[i] === r[i]) e.push(i)
  return e
}

// test & output
[ [-7, 1, 5, 2, -4, 3, 0], // 3, 6
  [2, 4, 6], // empty
  [2, 9, 2], // 1
  [1, -1, 1, -1, 1, -1, 1], // 0,1,2,3,4,5,6
  [1], // 0
  [] // empty
].forEach(function(x) {
  console.log(equilibrium(x))
});
```

```JavaScript
[[3,6],[],[1],[0,1,2,3,4,5,6],[0],[]]
```


===ES6 - two pass O(n), return index===

```JavaScript
function equilibrium(arr) {
  let sum = arr.reduce((a, b) => a + b);
  let leftSum = 0;

  for (let i = 0; i < arr.length; ++i) {
    sum -= arr[i];

    if (leftSum === sum) {
      return i;
    }

    leftSum += arr[i];
  }

  return -1;
}

```

```JavaScript
3, -1, 1, 0, 0
```



## jq

The following implementation will work with jq 1.4 but for input
arrays larger than 1e4 in length, a version of jq with tail-call
optimization (TCO) should probably be used.

Since the task description indicates that the array might be very long:
* the implementation uses a 0-arity inner function to do the heavy lifting;
* the algorithm walks along the array so as to minimize both memory requirements and the number of arithmetic operations;
* the answers are emitted as a stream.

The top-level function is defined as a 0-arity filter that emits answers as a stream, as is idiomatic in jq.

```jq
# The index origin is 0 in jq.
def equilibrium_indices:
  def indices(a; mx):
    def report: # [i, headsum, tailsum]
      .[0] as $i
      | if $i == mx then empty          # all done
        else .[1] as $h
        | (.[2] - a[$i]) as $t
        | (if $h == $t then $i else empty end),
          ( [ $i + 1, $h + a[$i], $t ] | report )
        end;
    [0, 0, (a|add)] | report;
  . as $in | indices($in; $in|length);
```

'''Example 1:'''

```jq
[-7, 1, 5, 2, -4, 3, 0] | equilibrium_indices
```

 $ jq -M -n -f equilibrium_indices.jq
 3
 6
'''Example 2:'''

```jq
def count(g): reduce g as $i (0; .+1);

# Create an array of length n with "init" elements:
def array(n;init): reduce range(0;n) as $i ([]; . + [0]);

count( array(1e4;0) | equilibrium_indices )
```

 {{out}}
 $ jq -M -n -f equilibrium_indices.jq
 10000


## Julia

```julia
function equindex2pass(data::Array)
    rst = Vector{Int}(0)
    suml, sumr, ddelayed = 0, sum(data), 0
    for (i, d) in enumerate(data)
        suml += ddelayed
        sumr -= d
        ddelayed = d
        if suml == sumr
            push!(rst, i)
        end
    end
    return rst
end

@show equindex2pass([1, -1, 1, -1, 1, -1, 1])
@show equindex2pass([1, 2, 2, 1])
@show equindex2pass([-7, 1, 5, 2, -4, 3, 0])
```


```txt
equindex2pass([1, -1, 1, -1, 1, -1, 1]) = [1, 2, 3, 4, 5, 6, 7]
equindex2pass([1, 2, 2, 1]) = Int64[]
equindex2pass([-7, 1, 5, 2, -4, 3, 0]) = [4, 7]
```



## K


```K
   f:{&{(+/y# x)=+/(y+1)_x}[x]'!#x}

   f -7 1 5 2 -4 3 0
3 6

   f 2 4 6
!0

   f 2 9 2
,1

  f 1 -1 1 -1 1 -1 1
0 1 2 3 4 5 6
```



## Kotlin


```scala
// version 1.1

fun equilibriumIndices(a: IntArray): MutableList<Int> {
   val ei = mutableListOf<Int>()
   if (a.isEmpty()) return ei // empty list
   val sumAll  = a.sumBy { it }
   var sumLeft = 0
   var sumRight: Int
   for (i in 0 until a.size) {
       sumRight = sumAll - sumLeft - a[i]
       if (sumLeft == sumRight) ei.add(i)
       sumLeft += a[i]
   }
   return ei
}

fun main(args: Array<String>) {
    val a = intArrayOf(-7, 1, 5, 2, -4, 3, 0)
    val ei = equilibriumIndices(a)
    when (ei.size) {
         0     -> println("There are no equilibrium indices")
         1     -> println("The only equilibrium index is : ${ei[0]}")
         else  -> println("The equilibrium indices are : ${ei.joinToString(", ")}")
    }
}
```


```txt

The equilibrium indices are : 3, 6

```



## Liberty BASIC


```lb

a(0)=-7
a(1)=1
a(2)=5
a(3)=2
a(4)=-4
a(5)=3
a(6)=0

print "EQ Indices are ";EQindex$("a",0,6)

wait

function EQindex$(b$,mini,maxi)
    if mini>=maxi then exit function
    sum=0
    for i = mini to maxi
        sum=sum+eval(b$;"(";i;")")
    next
    sumA=0:sumB=sum
    for i = mini to maxi
        sumB = sumB - eval(b$;"(";i;")")
        if sumA=sumB then EQindex$=EQindex$+str$(i)+", "
        sumA = sumA + eval(b$;"(";i;")")
    next
    if len(EQindex$)>0 then EQindex$=mid$(EQindex$, 1, len(EQindex$)-2) 'remove last ", "
end function

```

```txt
EQ Indices are 3, 6
```



## Logo


```logo
to equilibrium.iter :i :before :after :tail :ret
  if equal? :before :after [make "ret lput :i :ret]
  if empty? butfirst :tail [output :ret]
  output equilibrium.iter :i+1 (:before+first :tail) (:after-first butfirst :tail) (butfirst :tail) :ret
end
to equilibrium.index :list
  output equilibrium.iter 1 0 (apply "sum butfirst :list) :list []
end

show equilibrium_index [-7 1 5 2 -4 3 0]    ; [4 7]
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica indexes are 1-based so the output of this program will be shifted up by one compared to solutions in languages with 0-based arrays.

```Mathematica
equilibriumIndex[data_]:=Reap[
    Do[If[Total[data[[;; n - 1]]] == Total[data[[n + 1 ;;]]],Sow[n]],
    {n, Length[data]}]][[2, 1]]
```

```txt
equilibriumIndex[{-7 , 1, 5 , 2, -4 , 3, 0}]
{4, 7}
```



## MATLAB

MATLAB arrays are 1-based so the output of this program will be shifted up by one compared to solutions in languages with 0-based arrays.

```MATLAB
function indicies = equilibriumIndex(list)

    indicies = [];

    for i = (1:numel(list))
        if ( sum(-list(1:i)) == sum(-list(i:end)) )
            indicies = [indicies i];
        end
    end

end
```

```matlab
>
 equilibriumIndex([-7 1 5 2 -4 3 0])

ans =

     4     7
```



## NetRexx

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numeric digits 20
runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- @see http://www.geeksforgeeks.org/equilibrium-index-of-an-array/
method equilibriumIndex(sequence) private static
  es = ''
  loop ix = 1 to sequence.words()
    sum = 0
    loop jx = 1 to sequence.words()
      if jx < ix then sum = sum + sequence.word(jx)
      if jx > ix then sum = sum - sequence.word(jx)
      end jx
      if sum = 0 then es = es ix
    end ix
  return es

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  -- Note: A Rexx object based list of "words" starts at index 1
  sequences = [ -
    '-7  1  5  2 -4  3  0', -  -- 4 7
    ' 2  4  6'            , -  -- (no equilibrium point)
    ' 0  2  4  0  6  0'   , -  -- 4
    ' 2  9  2'            , -  -- 2
    ' 1 -1  1 -1  1 -1  1'  -  -- 1 2 3 4 5 6 7
    ]
  loop sequence over sequences
    say 'For sequence "'sequence.space(1, ',')'" the equilibrium indices are: \-'
    say '"'equilibriumIndex(sequence).space(1, ',')'"'
    end sequence
  return


```

```txt

For sequence "-7,1,5,2,-4,3,0" the equilibrium indices are: "4,7"
For sequence "2,4,6" the equilibrium indices are: ""
For sequence "0,2,4,0,6,0" the equilibrium indices are: "4"
For sequence "2,9,2" the equilibrium indices are: "2"
For sequence "1,-1,1,-1,1,-1,1" the equilibrium indices are: "1,2,3,4,5,6,7"

```



## Nim

```nim
import math, sequtils

iterator eqindex(data) =
  var suml, ddelayed = 0
  var sumr = sum(data)
  for i,d in data:
    suml += ddelayed
    sumr -= d
    ddelayed = d
    if suml == sumr:
      yield i

const d = @[@[-7, 1, 5, 2, -4, 3, 0],
            @[2, 4, 6],
            @[2, 9, 2],
            @[1, -1, 1, -1, 1, -1, 1]]

for data in d:
  echo "d = ", data
  echo "eqIndex(d) -> ", toSeq(eqindex(data))
```



## Objeck

```objeck
class Rosetta {
  function : Main(args : String[]) ~ Nil {
    sequence := [-7, 1, 5, 2, -4, 3, 0];
    EqulibriumIndices(sequence);
  }

  function : EqulibriumIndices(sequence : Int[]) ~ Nil {
    # find total sum
    totalSum := 0;
    each(i : sequence) {
      totalSum += sequence[i];
    };

    # compare running sum to remaining sum to find equlibrium indices
    runningSum := 0;
    each(i : sequence) {
      n := sequence[i];
      if (totalSum - runningSum - n = runningSum) {
        i->PrintLine();
      };
      runningSum += n;
    };
  }
}
```


Output:

```txt

3
6

```



## OCaml


```ocaml
let lst = [ -7; 1; 5; 2; -4; 3; 0 ]
let sum = List.fold_left ( + ) 0 lst

let () =
  let rec aux acc i left right = function
  | x::xs ->
      let right = right - x in
      let acc = if left = right then i::acc else acc in
      aux acc (succ i) (left + x) right xs
  | [] -> List.rev acc
  in
  let res = aux [] 0 0 sum lst in
  print_string "Results:";
  List.iter (Printf.printf " %d") res;
  print_newline()
```



## Oforth


Oforth collections are 1-based


```Oforth
: equilibrium(l)
| ls rs i e |
   0 ->ls
   l sum ->rs
   ListBuffer new l size loop: i [
      l at(i) ->e
      rs e - dup ->rs ls == ifTrue: [ i over add ]
      ls e + ->ls
      ] ;
```


```txt

>equilibrium([-7, 1, 5, 2, -4, 3, 0]) println
[4, 7]

```



## PARI/GP

This uses 1-based vectors instead of 0-based arrays; subtract 1 from each index if you prefer the other style.

```parigp
equilib(v)={
  my(a=sum(i=2,#v,v[i]),b=0,u=[]);
  for(i=1,#v-1,
    if(a==b, u=concat(u,i));
    b+=v[i];
    a-=v[i+1]
  );
  if(b,u,concat(u,#v))
};
```



## Pascal


```pascal
Program EquilibriumIndexDemo(output);

function ArraySum(list: array of integer; first, last: integer): integer;
  var
    i: integer;
  begin
    ArraySum := 0;
    for i := first to last do  // not taken if first > last
      ArraySum := ArraySum + list[i];
  end;

procedure EquilibriumIndex(list: array of integer; offset: integer);
  var
    i: integer;
  begin
    for i := low(list) to high(list) do
      if ArraySum(list, low(list), i-1) = ArraySum(list, i+1, high(list)) then
        write(offset + i:3);
  end;

var
{** The base index of the array is fully taken care off and can be any number. **}
  numbers: array [1..7] of integer = (-7, 1, 5, 2, -4, 3, 0);
  i: integer;

begin
  write('List of numbers: ');
  for i := low(numbers) to high(numbers) do
    write(numbers[i]:3);
  writeln;
  write('Equilibirum indices: ');
  EquilibriumIndex(numbers, low(numbers));
  writeln;
end.
```

```txt
:> ./EquilibriumIndex
List of numbers:  -7  1  5  2 -4  3  0
Equilibirum indices:   4  7
```


### alternative

slightly modified.Calculating the sum only once.Using a zero-based array type.Data type could be any type of  signed integer or float.
But beware, that during building the sum, the limits of the data type mustn't be violated.

```pascal
Program EquilibriumIndexDemo(output);
{$IFDEF FPC}{$Mode delphi}{$ENDIF}
type
  tEquiData = shortInt;//Int64;extended ,double
  tnumList = array of tEquiData;
  tresList = array of LongInt;
const
  cNumbers: array [11..17] of tEquiData = (-7, 1, 5, 2, -4, 3, 0);

function ArraySum(const list: tnumList):tEquiData;
var
  i: integer;
begin
  result := 0;
  for i := Low(list) to High(list) do
    result := result+list[i];
end;

procedure EquilibriumIndex(const    list:tnumList;
                              var indices:tresList);
var
  pC : ^tEquiData;
  LeftSum,
  RightSum : tEquiData;
  i,idx,HiList: integer;

begin
  HiList := High(List);
  RightSum :=ArraySum(list);
  setlength(indices,10);
  idx := 0;

  i := -Hilist;
  pC := @List[0];
  LeftSum:= 0;
  repeat
    Rightsum:= RightSum-pC^;
    IF LeftSum = RightSum then
    Begin
      indices[idx] := Hilist+i;
      inc(idx);
      IF idx > high(indices) then
        setlength(indices, idx+10);
    end;
    inc(i);
    leftSum := leftsum+pC^;
    inc(pC);
  until i>=0;
  leftSum := leftsum+pC^;
  IF LeftSum = RightSum then
  Begin
    indices[idx] := Hilist+i;
    inc(idx);
  end;
  setlength(indices,idx);
end;

procedure TestRun(const numbers:tnumList);
var
  indices : tresList;
  i: integer;
Begin
  write('List of numbers:     ');
  for i := low(numbers) to high(numbers) do
    write(numbers[i]:3);
  writeln;
  EquilibriumIndex(numbers,indices);
  write('Equilibirum indices: ');
  EquilibriumIndex(numbers,indices);
  for i := low(indices) to high(indices) do
    write(indices[i]:3);
  writeln;
  writeln;
end;

var
  numbers: tnumList;
  I: integer;
begin
  setlength(numbers,High(cNumbers)-Low(cNumbers)+1);
  move(cNumbers[Low(cNumbers)],numbers[0],sizeof(cnumbers));
  TestRun(numbers);
  for i := low(numbers) to high(numbers) do
    numbers[i]:= 0;
  TestRun(numbers);
end.
```
```txt
List of numbers:      -7  1  5  2 -4  3  0
Equilibirum indices:   3  6

List of numbers:       0  0  0  0  0  0  0
Equilibirum indices:   0  1  2  3  4  5  6
```



## Perl

```perl
sub eq_index {
    my ( $i, $sum, %sums ) = ( 0, 0 );

    for (@_) {
        push @{ $sums{ $sum * 2 + $_  } }, $i++;
        $sum += $_;
    }

    return join ' ', @{ $sums{$sum} || [] }, "\n";
}

print eq_index qw(  -7  1  5  2 -4  3  0 ); # 3 6
print eq_index qw(   2  4  6             ); # (no eq point)
print eq_index qw(   2  9  2             ); # 1
print eq_index qw(   1 -1  1 -1  1 -1  1 ); # 0 1 2 3 4 5 6
```



## Perl 6


```perl6
sub equilibrium_index(@list) {
    my ($left,$right) = 0, [+] @list;

    gather for @list.kv -> $i, $x {
        $right -= $x;
        take $i if $left == $right;
        $left += $x;
    }
}

my @list = -7, 1, 5, 2, -4, 3, 0;
.say for equilibrium_index(@list).grep(/\d/);
```

And here's an FP solution that manages to remain O(n):

```perl6
sub equilibrium_index(@list) {
    my @a = [\+] @list;
    my @b = reverse [\+] reverse @list;
    ^@list Zxx (@a »==« @b);
}
```

The <tt>[\+]</tt> is a reduction that returns a list of partial results. The <tt>»==«</tt> is a vectorized equality comparison; it returns a vector of true and false.  The <tt>Zxx</tt> is a zip with the list replication operator, so we return only the elements of the left list where the right list is true (which is taken to mean 1 here).  And the <tt>^@list</tt> is just shorthand for <tt>0 ..^ @list</tt>.  We could just as easily have used <tt>@list.keys</tt> there.
=== Single-pass solution ===
The task can be restated in a way that removes the "right side" from the calculation.

C is the current element,

L is the sum of elements left of C,

R is the sum of elements right of C,

S is the sum of the entire list.

By definition, L + C + R == S for any choice of C, and L == R for any C that is an equilibrium point.

Therefore (by substituting L for R), L + C + L == S at all equilibrium points.

Restated, 2L + C == S.

```perl6
# Original example, with expanded calculations:
    0    1    2    3    4    5    6   # Index
   -7    1    5    2   -4    3    0   # C (Value at index)
    0   -7   -6   -1    1   -3    0   # L (Sum of left)
   -7  -13   -7    0   -2   -3    0   # 2L+C
```

If we build a hash as we walk the list, with 2L+C as hash keys, and arrays of C-indexes as hash values, we get:

```perl6
{
     -7 => [ 0, 2 ],
    -13 => [ 1    ],
      0 => [ 3, 6 ],
     -2 => [ 4    ],
     -3 => [ 5    ],
}
```

After we have finished walking the list, we will have the sum (S), which we look up in the hash. Here S=0, so the equilibrium points are 3 and 6.

Note: In the code below, it is more convenient to calculate 2L+C *after* L has already been incremented by C; the calculation is simply 2L-C, because each L has an extra C in it. 2(L-C)+C == 2L-C.

```perl6
sub eq_index ( *@list ) {
    my $sum = 0;

    my %h = @list.keys.classify: {
        $sum += @list[$_];
        $sum * 2 - @list[$_];
    };

    return %h{$sum} // [];
}

say eq_index < -7  1  5  2 -4  3  0 >; # 3 6
say eq_index <  2  4  6             >; # (no eq point)
say eq_index <  2  9  2             >; # 1
say eq_index <  1 -1  1 -1  1 -1  1 >; # 0 1 2 3 4 5 6
```

The <tt>.classify</tt> method creates a hash, with its code block's return value as key. Each hash value is an Array of all the inputs that returned that key.

We could have used <tt>.pairs</tt> instead of <tt>.keys</tt> to save the cost of <tt>@list</tt> lookups, but that would change each <tt>%h</tt> value to an Array of Pairs, which would complicate the return line.


## Phix


```Phix
function equilibrium(sequence s)
atom lower_sum = 0
atom higher_sum = sum(s)
sequence res = {}
    for i=1 to length(s) do
        higher_sum -= s[i]
        if lower_sum=higher_sum then
            res &= i
        end if
        lower_sum += s[i]
    end for
    return res
end function

? equilibrium({-7,1,5,2,-4,3,0})
```

(Remember that indices are 1-based in Phix)

```txt

{4,7}

```



## PHP


```php
<?php
$arr = array(-7, 1, 5, 2, -4, 3, 0);

function getEquilibriums($arr) {
    $right = array_sum($arr);
    $left = 0;
    $equilibriums = array();
    foreach($arr as $key => $value){
        $right -= $value;
        if($left == $right) $equilibriums[] = $key;
        $left += $value;
    }
    return $equilibriums;
}

echo "# results:\n";
foreach (getEquilibriums($arr) as $r) echo "$r, ";
?>
```

```txt

# results:
3, 6,

```



## PicoLisp


```PicoLisp
(de equilibria (Lst)
   (make
      (let Sum 0
         (for ((I . L) Lst L (cdr L))
            (and (= Sum (sum prog (cdr L))) (link I))
            (inc 'Sum (car L)) ) ) ) )
```

```txt
: (equilibria (-7 1 5 2 -4 3 0))
-> (4 7)

: (equilibria (make (do 10000 (link (rand -10 10)))))
-> (4091 6174 6198 7104 7112 7754)
```



## PowerShell

In real life in PowerShell, one would likely leverage pipelines, ForEach-Object, Where-Object, and Measure-Object for tasks such as this. Normally in PowerShell, speed is an important, but not primary consideration, and the advantages of pipelines tend to outweigh the overhead incurred. However, for this particular task, keeping in mind that “the sequence may be very long,” this code was optimized primarly for speed.

```PowerShell

function Get-EquilibriumIndex ( $Sequence )
    {
    $Indexes = 0..($Sequence.Count - 1)
    $EqulibriumIndex = @()

    ForEach ( $TestIndex in $Indexes )
        {
        $Left = 0
        $Right = 0
        ForEach ( $Index in $Indexes )
            {
            If     ( $Index -lt $TestIndex ) { $Left  += $Sequence[$Index] }
            ElseIf ( $Index -gt $TestIndex ) { $Right += $Sequence[$Index] }
            }

        If ( $Left -eq $Right )
            {
            $EqulibriumIndex += $TestIndex
            }
        }
    return $EqulibriumIndex
    }

```


```PowerShell

Get-EquilibriumIndex -7, 1, 5, 2, -4, 3, 0

```

```txt

3
6

```



## Prolog



```prolog
equilibrium_index(List, Index) :-
    append(Front, [_|Back], List),
    sumlist(Front, Sum),
    sumlist(Back,  Sum),
    length(Front, Len),
    Index is Len.
```


Example:


```prolog
?- equilibrium_index([-7, 1, 5, 2, -4, 3, 0], Index).
Index = 3 ;
Index = 6 ;
false.
```



## PureBasic

```PureBasic
If OpenConsole()
  Define i, c=CountProgramParameters()-1
  For i=0 To c
    Define j, LSum=0, RSum=0
    For j=0 To c
      If j<i
        LSum+Val(ProgramParameter(j))
      ElseIf j>i
        RSum+Val(ProgramParameter(j))
      EndIf
    Next j
    If LSum=RSum: PrintN(Str(i)): EndIf
  Next i
EndIf
```

```txt
> Equilibrium.exe -7 1 5 2 -4 3 0
3
6
```



## Python


### Two Pass

Uses an initial summation of the whole list then visits each item of the list adding it to the left-hand sum (after a delay); and subtracting the item from the right-hand sum. I think it should be quicker than algorithms that scan the list creating left and right sums for each index as it does ~2N add/subtractions rather than n*n.

```python
def eqindex2Pass(data):
    "Two pass"
    suml, sumr, ddelayed = 0, sum(data), 0
    for i, d in enumerate(data):
        suml += ddelayed
        sumr -= d
        ddelayed = d
        if suml == sumr:
            yield i
```


### Multi Pass

This is the version that does more summations, but may be faster for some sizes of input as the sum function is implemented in C internally:

```python
def eqindexMultiPass(data):
    "Multi pass"
    for i in range(len(data)):
        suml, sumr = sum(data[:i]), sum(data[i+1:])
        if suml == sumr:
            yield i
```

Shorter alternative:

```python
def eqindexMultiPass(s):
    return [i for i in xrange(len(s)) if sum(s[:i]) == sum(s[i+1:])]

print eqindexMultiPass([-7, 1, 5, 2, -4, 3, 0])
```


### One Pass

This routine would need careful evaluation against the two-pass solution above as, although it only runs through the data once, it may create a dict that is as long as the input data in its worst case of an input of say a simple 1, 2, 3, ... counting sequence.

```python
from collections import defaultdict

def eqindex1Pass(data):
    "One pass"
    l, h = 0, defaultdict(list)
    for i, c in enumerate(data):
        l += c
        h[l * 2 - c].append(i)
    return h[l]
```


### Tests


```python
f = (eqindex2Pass, eqindexMultiPass, eqindex1Pass)
d = ([-7, 1, 5, 2, -4, 3, 0],
     [2, 4, 6],
     [2, 9, 2],
     [1, -1, 1, -1, 1, -1, 1])

for data in d:
    print("d = %r" % data)
    for func in f:
        print("  %16s(d) -> %r" % (func.__name__, list(func(data))))
```

```txt
d = [-7, 1, 5, 2, -4, 3, 0]
      eqindex2Pass(d) -> [3, 6]
  eqindexMultiPass(d) -> [3, 6]
      eqindex1Pass(d) -> [3, 6]
d = [2, 4, 6]
      eqindex2Pass(d) -> []
  eqindexMultiPass(d) -> []
      eqindex1Pass(d) -> []
d = [2, 9, 2]
      eqindex2Pass(d) -> [1]
  eqindexMultiPass(d) -> [1]
      eqindex1Pass(d) -> [1]
d = [1, -1, 1, -1, 1, -1, 1]
      eqindex2Pass(d) -> [0, 1, 2, 3, 4, 5, 6]
  eqindexMultiPass(d) -> [0, 1, 2, 3, 4, 5, 6]
      eqindex1Pass(d) -> [0, 1, 2, 3, 4, 5, 6]
```



### In terms of itertools.accumulate


The '''left''' scan is efficiently derived by the '''accumulate''' function in the '''itertools''' module.

The ''right'' scan can be derived from the left as a map or equivalent list comprehension:

```python
"""Equilibrium index"""

from itertools import (accumulate)


# equilibriumIndices :: [Num] -> [Int]
def equilibriumIndices(xs):
    '''List indices at which the sum of values to the left
       equals the sum of values to the right.'''
    def go(xs):
        '''Left scan from accumulate, right scan derived from left'''
        ls = list(accumulate(xs))
        n = ls[-1]
        return [i for (i, (x, y)) in enumerate(zip(
            ls,
            [n] + [n - x for x in ls[0:-1]]
        )) if x == y]
    return go(xs) if xs else []


# TEST -------------------------------------------------
# main :: IO ()
def main():
    '''Tabulated test results'''
    print(
        tabulated('Equilibrium indices:\n')(
            equilibriumIndices
        )([
            [-7, 1, 5, 2, -4, 3, 0],
            [2, 4, 6],
            [2, 9, 2],
            [1, -1, 1, -1, 1, -1, 1],
            [1],
            []
        ])
    )


# GENERIC -------------------------------------------------


# tabulated :: String -> (a -> b) -> [a] -> String
def tabulated(s):
    '''heading -> function -> input List -> tabulated output string'''
    def go(f, xs):
        def width(x):
            return len(str(x))
        w = width(max(xs, key=width))
        return s + '\n' + '\n'.join([
            str(x).rjust(w, ' ') + ' -> ' + str(f(x)) for x in xs
        ])
    return lambda f: lambda xs: go(f, xs)


if __name__ == '__main__':
    main()
```

```txt
Equilibrium indices:

 [-7, 1, 5, 2, -4, 3, 0] -> [3, 6]
               [2, 4, 6] -> []
               [2, 9, 2] -> [1]
[1, -1, 1, -1, 1, -1, 1] -> [0, 1, 2, 3, 4, 5, 6]
                     [1] -> [0]
                      [] -> []
```



## Racket


```racket

#lang racket
(define (subsums xs)
  (for/fold ([sums '()] [sum 0]) ([x xs])
    (values (cons (+ x sum) sums)
            (+ x sum))))

(define (equivilibrium xs)
  (define-values (sums total) (subsums xs))
  (for/list ([sum (reverse sums)]
             [x xs]
             [i (in-naturals)]
             #:when (= (- sum x) (- total sum)))
    i))

(equivilibrium '(-7 1 5 2 -4 3 0))

```

```racket

'(3 6)

```



## REXX


### version 1

This REXX version utilizes a   ''zero-based''   stemmed array to mimic the illustrative example in this Rosetta Code task's

prologue,   which uses a   ''zero-based''   index.

```rexx
/*REXX program calculates and displays the equilibrium index for a numeric array (list).*/
parse arg x                                      /*obtain the optional arguments from CL*/
if x=''  then x=copies(" 7 -7", 50)   7          /*Not specified?  Then use the default.*/
say '         array list: '     space(x)         /*echo the array list to the terminal. */
#=words(x)                                       /*the number of elements in the X list.*/
              do j=0  for #;    @.j=word(x, j+1) /*zero─start is for zero─based array.  */
              end   /*j*/                        /* [↑]  assign   @.0   @.1   @.3  ···  */
say                                              /*  ··· and also display a blank line. */
answer=equilibriumIDX();        w=words(answer)  /*calculate the  equilibrium index.    */
say 'equilibrium'  word("(none) index: indices:",   1 + (w>0) + (w>1))        answer
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
equilibriumIDX: $=;       do i=0  for #;           sum=0
                                  do k=0  for #;   sum=sum + @.k*sign(k-i);     end  /*k*/
                          if sum==0  then $=$ i
                          end   /*i*/            /* [↑] Zero? Found an equilibrium index*/
                return $                         /*return equilibrium list (may be null)*/
```

'''output'''   using the input:   <tt> -7   1   5   2   -4   3   0 </tt>

```txt

         array list:  -7 1 5 2 -4 3 0

equilibrium indices:  3 6

```

'''output'''   using the input:   <tt> 2   9   2 </tt>

```txt

         array list:  2 9 2

equilibrium index:  1

```

'''output'''   using the input:   <tt> 5   4   4   5 </tt>

```txt

         array list:  5 4 4 5

equilibrium (none)

```

'''output'''   using the default input:

```txt

         array list:  7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7 -7 7

equilibrium indices:  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100

```



### version 2


```rexx
/* REXX ---------------------------------------------------------------
* 30.06.2014 Walter Pachl
*--------------------------------------------------------------------*/
parse arg l
say '         array list:' strip(l)
x.=0
Do i=1 To words(l)
  x.i=word(l,i)
  End
n=i-1
ans=strip(equilibriumIndices())
n=words(ans)
Select
  When n=0 Then Say 'There''s no equilibrium index'
  When n=1 Then Say 'equilibrium index  :' ans
  Otherwise     Say 'equilibrium indices:' ans
  End
Say '---'
exit
equilibriumIndices: procedure expose x. n
sum.=0
sum=0
eil=''
Do i=1 To n
  sum=sum+x.i
  sum.i=sum
  End
Do i=1 To n
  im1=i-1
  If sum.im1=(sum.n-x.i)/2 Then
    eil=eil im1
  End
Return eil
```

'''output'''

```txt
         array list: -7 1 5 2 -4 3 0
equilibrium indices: 3 6
---
         array list: 2 9 2
equilibrium index  : 1
---
         array list: 1 -1 1 -1 1 -1 1
equilibrium indices: 0 1 2 3 4 5 6
---
         array list: 1 1
There's no equilibrium index
---
```



## Ring


```ring

list  = [-7, 1, 5, 2, -4, 3, 0]
see "equilibrium indices are : " + equilibrium(list) + nl

func equilibrium l
     r = 0 s = 0 e = ""
     for n = 1 to len(l)
         s += l[n]
     next
     for i = 1 to len(l)
         if r = s - r - l[i]  e += string(i-1) + "," ok
         r += l[i]
     next
     e = left(e,len(e)-1)
     return e

```

Output:

```txt

equilibrium indices are : 3,6

```



## Ruby

;Functional Style

```ruby
def eq_indices(list)
  list.each_index.select do |i|
    list[0...i].inject(0, :+) == list[i+1..-1].inject(0, :+)
  end
end
```

;Tail Recursion
* This one would be good if Ruby did tail-call optimization (TCO).
* [[MRI]] does not do TCO; so this function fails with a long list (by overflowing the call stack).

```ruby
def eq_indices(list)
  result = []
  list.empty? and return result
  final = list.size - 1

  helper = lambda do |left, current, right, index|
    left == right and result << index   # Push index to result?
    index == final and return           # Terminate recursion?
    new = list[index + 1]
    helper.call(left + current, new, right - new, index + 1)
  end
  helper.call 0, list.first, list.drop(1).inject(:+), 0
  result
end
```

;Imperative Style (faster)

```ruby
def eq_indices(list)
  left, right = 0, list.inject(0, :+)
  equilibrium_indices = []

  list.each_with_index do |val, i|
    right -= val
    equilibrium_indices << i if right == left
    left += val
  end

  equilibrium_indices
end
```


;Test

```ruby
indices = [
  [-7, 1, 5, 2,-4, 3, 0],
  [2, 4, 6],
  [2, 9, 2],
  [1,-1, 1,-1, 1,-1, 1]
]
indices.each do |x|
  puts "%p => %p" % [x, eq_indices(x)]
end
```

```txt

[-7, 1, 5, 2, -4, 3, 0] => [3, 6]
[2, 4, 6] => []
[2, 9, 2] => [1]
[1, -1, 1, -1, 1, -1, 1] => [0, 1, 2, 3, 4, 5, 6]

```


## Scala



```Scala
 def getEquilibriumIndex(A: Array[Int]): Int = {
      val bigA: Array[BigInt] = A.map(BigInt(_))
      val partialSums: Array[BigInt] = bigA.scanLeft(BigInt(0))(_+_).tail
      def lSum(i: Int): BigInt = if (i == 0) 0 else partialSums(i - 1)
      def rSum(i: Int): BigInt = partialSums.last - partialSums(i)
      def isRandLSumEqual(i: Int): Boolean = lSum(i) == rSum(i)
      (0 until partialSums.length).find(isRandLSumEqual).getOrElse(-1)
    }
```




## Seed7


```seed7
$ include "seed7_05.s7i";

const array integer: numList is [] (-7, 1, 5, 2, -4, 3, 0);

const func array integer: equilibriumIndex (in array integer: elements) is func
  result
    var array integer: indexList is 0 times 0;
  local
    var integer: element is 0;
    var integer: index is 0;
    var integer: sum is 0;
    var integer: subSum is 0;
    var integer: count is 0;
  begin
    indexList := length(elements) times 0;
    for element range elements do
      sum +:= element;
    end for;
    for element key index range elements do
      if 2 * subSum + element = sum then
        incr(count);
        indexList[count] := index;
      end if;
      subSum +:= element;
    end for;
    indexList := indexList[.. count];
  end func;

const proc: main is func
  local
    var array integer: indexList is 0 times 0;
    var integer: element is 0;
  begin
    indexList := equilibriumIndex(numList);
    write("Found:");
    for element range indexList do
      write(" " <& element);
    end for;
    writeln;
  end func;
```

```txt

Found: 4 7

```



## Sidef


```ruby
func eq_index(nums) {
    var (i, sum, sums) = (0, 0, Hash.new);
    nums.each { |n|
        sums{2*sum + n} := [] -> append(i++);
        sum += n;
    }
    sums{sum} \\ [];
}
```


Test:

```ruby
var indices = [
  [-7, 1, 5, 2,-4, 3, 0],
  [2, 4, 6],
  [2, 9, 2],
  [1,-1, 1,-1, 1,-1, 1],
]

for x in indices {
    say ("%s => %s" % @|[x, eq_index(x)].map{.dump});
}
```

```txt

[-7, 1, 5, 2, -4, 3, 0] => [3, 6]
[2, 4, 6] => []
[2, 9, 2] => [1]
[1, -1, 1, -1, 1, -1, 1] => [0, 1, 2, 3, 4, 5, 6]

```



## Tcl


```tcl
proc listEquilibria {list} {
    set after 0
    foreach item $list {incr after $item}
    set result {}
    set idx 0
    set before 0
    foreach item $list {
	incr after [expr {-$item}]
	if {$after == $before} {
	    lappend result $idx
	}
	incr before $item
	incr idx
    }
    return $result
}
```

;Example of use

```tcl
set testData {-7 1 5 2 -4 3 0}
puts Equilibria=[join [listEquilibria $testData] ", "]
```

```txt
Equilibria=3, 6
```



## Ursala


```Ursala
#import std
#import int

edex = num@yK33ySPtK33xtS2px; ~&nS+ *~ ==+ ~~r sum:-0

#cast %nL

example = edex <-7,1,5,2,-4,3,0>
```

```txt

<3,6>

```



## VBScript

Solution adopted from http://www.geeksforgeeks.org/equilibrium-index-of-an-array/ .

```vb
arr = Array(-7,1,5,2,-4,3,0)
WScript.StdOut.Write equilibrium(arr,UBound(arr))
WScript.StdOut.WriteLine

Function equilibrium(arr,n)
	sum = 0
	leftsum = 0
	'find the sum of the whole array
	For i = 0 To UBound(arr)
		sum = sum + arr(i)
	Next
	For i = 0 To UBound(arr)
		sum = sum - arr(i)
		If leftsum = sum Then
			equilibrium = equilibrium & i & ", "
		End If
		leftsum = leftsum + arr(i)
	Next
End Function
```


```txt
Indices: 3, 6,
```



## XPL0


```XPL0
code Ran=1, ChOut=8, IntOut=11;
def Size = 1_000_000;
int I, S, A(Size), Hi(Size), Lo(Size);
[for I:= 0 to Size-1 do A(I):= Ran(100) - 50;
S:= 0;
for I:= 0 to Size-1 do [S:= S+A(I);  Lo(I):= S];
S:= 0;
for I:= Size-1 downto 0 do [S:= S+A(I);  Hi(I):= S];
for I:= 0 to Size-1 do
    if Lo(I) = Hi(I) then [IntOut(0, I);  ChOut(0, ^ )];
]
```


```txt

502910 504929 508168

```



## Yorick

Yorick arrays are 1-based so the output of this program will be shifted up by one compared to solutions in languages with 0-based arrays.

```yorick
func equilibrium_indices(A) {
    return where(A(psum) == A(::-1)(psum)(::-1));
}
```

```txt
> equilibrium_indices([-7, 1, 5, 2, -4, 3, 0])
[4,7]
```



## zkl

```zkl
fcn equilibrium(lst){  // two pass
   reg acc=List(), left=0,right=lst.sum(0),i=0;
   foreach x in (lst){
      right-=x;
      if(left==right) acc.write(i);
      i+=1; left+=x;
   }
   acc
}
```

```zkl
fcn equilibrium(lst){  // lst should immutable, n^2
   (0).filter(lst.len(),'wrap(n){ lst[0,n].sum(0) == lst[n+1,*].sum(0) })
}
```

If the input list is immutable, no new lists are generated (other than accumulating the result).

```zkl
equilibrium(T(-7, 1, 5, 2, -4, 3, 0)).println();
```

```txt
L(3,6)
```



## ZX Spectrum Basic

```zxbasic
10 DATA 7,-7,1,5,2,-4,3,0
20 READ n
30 DIM a(n): LET sum=0: LET leftsum=0: LET s$=""
40 FOR i=1 TO n: READ a(i): LET sum=sum+a(i): NEXT i
50 FOR i=1 TO n
60 LET sum=sum-a(i)
70 IF leftsum=sum THEN LET s$=s$+STR$ i+" "
80 LET leftsum=leftsum+a(i)
90 NEXT i
100 PRINT "Numbers: ";
110 FOR i=1 TO n: PRINT a(i);" ";: NEXT i
120 PRINT '"Indices: ";s$
```

