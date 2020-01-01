+++
title = "Loop over multiple arrays simultaneously"
description = ""
date = 2019-10-18T11:08:07Z
aliases = []
[extra]
id = 4655
[taxonomies]
categories = []
tags = []
+++

{{task|Iteration}}

;Task:
Loop over multiple arrays   (or lists or tuples or whatever they're called in
your language)   and display the   <big><big> ''i'' <sup>th</sup> </big></big>   element of each.

Use your language's   "for each"   loop if it has one, otherwise iterate
through the collection in order with some other loop.


For this example, loop over the arrays:
     (a,b,c)
     (A,B,C)
     (1,2,3)
to produce the output:
     aA1
     bB2
     cC3



If possible, also describe what happens when the arrays are of different lengths.


;Related tasks:
*   [[Loop over multiple arrays simultaneously]]
*   [[Loops/Break]]
*   [[Loops/Continue]]
*   [[Loops/Do-while]]
*   [[Loops/Downward for]]
*   [[Loops/For]]
*   [[Loops/For with a specified step]]
*   [[Loops/Foreach]]
*   [[Loops/Increment loop index within loop body]]
*   [[Loops/Infinite]]
*   [[Loops/N plus one half]]
*   [[Loops/Nested]]
*   [[Loops/While]]
*   [[Loops/with multiple ranges]]
*   [[Loops/Wrong ranges]]





## 360 Assembly


```360asm
*        Loop over multiple arrays simultaneously  09/03/2017
LOOPSIM  CSECT
         USING  LOOPSIM,R12        base register
         LR     R12,R15
         LA     R6,1               i=1
         LA     R7,3               counter=3
LOOP     LR     R1,R6              i
         SLA    R1,1               *2
         LH     R2,R-2(R1)         r(i)
         XDECO  R2,PG              edit r(i)
         LA     R1,S-1(R6)         @s(i)
         MVC    PG+3(1),0(R1)      output s(i)
         LA     R1,Q-1(R6)         @q(i)
         MVC    PG+7(1),0(R1)      output q(i)
         XPRNT  PG,80              print s(i),q(i),r(i)
         LA     R6,1(R6)           i++
         BCT    R7,LOOP            decrement and loop
         BR     R14                exit
S        DC     C'a',C'b',C'c'
Q        DC     C'A',C'B',C'C'
R        DC     H'1',H'2',H'3'
PG       DC     CL80' '            buffer
         YREGS
         END    LOOPSIM
```

{{out}}

```txt

   a   A   1
   b   B   2
   c   C   3

```




## ACL2


```Lisp
(defun print-lists (xs ys zs)
   (if (or (endp xs) (endp ys) (endp zs))
       nil
       (progn$ (cw (first xs))
               (cw "~x0~x1~%"
                   (first ys)
                   (first zs))
               (print-lists (rest xs)
                            (rest ys)
                            (rest zs)))))

(print-lists '("a" "b" "c") '(A B C) '(1 2 3))
```



## Ada


```Ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Array_Loop_Test is
   type Array_Index is range 1..3;
   A1 : array (Array_Index) of Character := "abc";
   A2 : array (Array_Index) of Character := "ABC";
   A3 : array (Array_Index) of Integer   := (1, 2, 3);
begin
   for Index in Array_Index'Range loop
      Put_Line (A1 (Index) & A2 (Index) & Integer'Image (A3
(Index))(2));
   end loop;
end Array_Loop_Test;
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release
[http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download

 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested
 with release
[http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download

 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''}}

```algol68
[]UNION(CHAR,INT) x=("a","b","c"), y=("A","B","C"),
z=(1,2,3);
FOR i TO UPB x DO
  printf(($ggd$, x[i], y[i], z[i], $l$))
OD
```

{{out}}

```txt

aA1
bB2
cC3

```



## ALGOL W


```algolw
begin
    % declare the three arrays                                               %
    string(1) array a, b ( 1 :: 3 );
    integer   array c    ( 1 :: 3 );
    % initialise the arrays - have to do this element by element in Algol W  %
    a(1) := "a"; a(2) := "b"; a(3) := "c";
    b(1) := "A"; b(2) := "B"; b(3) := "C";
    c(1) :=  1;  c(2) :=  2;  c(3) :=  3;
    % loop over the arrays                                                   %
    for i := 1 until 3 do write( i_w := 1, s_w := 0, a(i), b(i), c(i) );
end.
```


If the arrays are not the same length, a subscript range error would occur when a non-existant element was accessed.



## AppleScript


{{Trans|JavaScript}}
(Functional ES 5 zipListsWith version)

If we have a generic Applescript '''map''' function, we can use it to write a generic '''zipListsWith''', which applies a given function over lists derived from the nth members of an arbitrary list of (equal-length) lists. (Where lists are of uneven length, items beyond the maximum shared length are ignored).


```AppleScript
-- ZIP LISTS WITH FUNCTION ---------------------------------------------------

-- zipListsWith :: ([a] -> b) -> [[a]] -> [[b]]
on zipListsWith(f, xss)
    set n to length of xss

    script
        on |λ|(_, i)
            script
                on |λ|(xs)
                    item i of xs
                end |λ|
            end script

            if i ≤ n then
                apply(f, (map(result, xss)))
            else
                {}
            end if
        end |λ|
    end script

    if n > 0 then
        map(result, item 1 of xss)
    else
        []
    end if
end zipListsWith


-- TEST  ( zip lists with concat ) -------------------------------------------
on run

    intercalate(linefeed, ¬
        zipListsWith(concat, ¬
            [["a", "b", "c"], ["A", "B", "C"], [1, 2, 3]]))

end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- apply (a -> b) -> a -> b
on apply(f, a)
    mReturn(f)'s |λ|(a)
end apply

-- concat :: [[a]] -> [a] | [String] -> String
on concat(xs)
    if length of xs > 0 and class of (item 1 of xs) is string then
        set acc to ""
    else
        set acc to {}
    end if
    repeat with i from 1 to length of xs
        set acc to acc & item i of xs
    end repeat
    acc
end concat

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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

{{Out}}

```txt
aA1
bB2
cC3
```


But a transpose function might be simpler:


```Applescript
-- CONCAT MAPPED OVER A TRANSPOSITION ----------------------------------------
on run

    unlines(map(concat, transpose([["a", "b", "c"], ["A", "B", "C"], [1, 2, 3]])))

end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

-- concat :: [[a]] -> [a] | [String] -> String
on concat(xs)
    if length of xs > 0 and class of (item 1 of xs) is string then
        set acc to ""
    else
        set acc to {}
    end if
    repeat with i from 1 to length of xs
        set acc to acc & item i of xs
    end repeat
    acc
end concat

-- intercalate :: String -> [String] -> String
on intercalate(s, xs)
    set {dlm, my text item delimiters} to {my text item delimiters, s}
    set str to xs as text
    set my text item delimiters to dlm
    return str
end intercalate

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

-- transpose :: [[a]] -> [[a]]
on transpose(xss)
    script column
        on |λ|(_, iCol)
            script row
                on |λ|(xs)
                    item iCol of xs
                end |λ|
            end script

            map(row, xss)
        end |λ|
    end script

    map(column, item 1 of xss)
end transpose

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines
```

{{Out}}

```txt
aA1
bB2
cC3
```



## AutoHotkey

=== Pseudo-arrays ===
[http://www.autohotkey.com/docs/commands/StringSplit.htm StringSplit]
creates a pseudo-array

```autohotkey
List1 = a,b,c
List2 = A,B,C
List3 = 1,2,3
MsgBox, % LoopMultiArrays()

List1 = a,b,c,d,e
List2 = A,B,C,D
List3 = 1,2,3
MsgBox, % LoopMultiArrays()


;---------------------------------------------------------------------------
LoopMultiArrays()

 { ; print the ith element of each
;---------------------------------------------------------------------------


    local Result
    StringSplit, List1_, List1, `,
    StringSplit, List2_, List2, `,
    StringSplit, List3_, List3, `,
    Loop, % List1_0
        Result .= List1_%A_Index% List2_%A_Index% List3_%A_Index% "`n"
    Return, Result
}
```

An array that is too short on creation will return empty strings when
trying to retrieve further elements. The 2nd Message box shows:

```txt
aA1
bB2
cC3
dD
e
```


###  Real arrays

{{works with|AutoHotkey_L}}
In [[AutoHotkey_L]], we can use true arrays
([http://l.autohotkey.com/docs/Objects.htm Objects]) and the
[http://l.autohotkey.net/docs/commands/For.htm For loop].

```AHK
List1 := ["a", "b", "c"]
List2 := ["A", "B", "C"]
List3 := [ 1 ,  2 ,  3 ]
MsgBox, % LoopMultiArrays()

List1 := ["a", "b", "c", "d", "e"]
List2 := ["A", "B", "C", "D"]
List3 := [1,2,3]
MsgBox, % LoopMultiArrays()


LoopMultiArrays() {
    local Result
    For key, value in List1
        Result .= value . List2[key] . List3[key] "`n"
    Return, Result
}
```

The output from this script is identical to the first one.


## AWK


```awk
BEGIN {
  split("a,b,c", a, ",");
  split("A,B,C", b, ",");
  split("1,2,3", c, ",");

  for(i = 1; i <= length(a); i++) {
    print a[i] b[i] c[i];
  }
}
```



## Axe

Note that in this example, we use a few bytes from each of L₁, L₂, and
L₃ for simplicity. In practice, one would want to arrange the arrays to
all fit within L₁ to avoid volatility issues with L₂ and L₃.

```axe
'a'→{L₁}
'b'→{L₁+1}
'c'→{L₁+2}
'A'→{L₂}
'B'→{L₂+1}
'C'→{L₂+2}
1→{L₃}
2→{L₃+1}
3→{L₃+2}
For(I,0,2)
Disp {L₁+I}►Char,{L₂+I}►Char,{L₃+I}►Dec,i
End
```



## Babel

There are two ways to do this in Babel.
First, you could transpose the lists:


```babel
main: { (('a' 'b' 'c')('A' 'B' 'C')('1' '2' '3'))
simul_array }

simul_array!:
    { trans
    { { << } each "\n" << } each }
```


The 'trans' operator substitutes nil in the portions of each transposed
column
wherever a row list was shorter than the longest row list. The
'<<' operator
prints nothing if the top-of-stack is nil.

A more literal solution to the problem as presented would be to iterate
across
each list using a user-defined cdrall operator:


```babel
main: { (('a' 'b' 'c')('A' 'B' 'C')('1' '2' '3'))
simul_array }

simul_array!:
    {{ dup
        { car << } each
        cdrall }
        { allnil? not }
    while }

cdrall!: { { { cdr } each -1 take } nest }

-- only returns true if all elements of a list are nil
allnil?!:
    { 1 <->
    { car nil?
        { zap 0 last }
        { nil }
    if} each }
```


This solution is formally identical to the first and will handle lists
of varying
lengths by printing inserting nil and printing nothing for the tail ends
 of the
short lists.


## BBC BASIC


```bbcbasic
      DIM array1$(2), array2$(2), array3%(2)
      array1$() = "a", "b", "c"
      array2$() = "A", "B", "C"
      array3%() = 1, 2, 3

      FOR index% = 0 TO 2
        PRINT array1$(index%) ; array2$(index%) ; array3%(index%)
      NEXT
```



## Befunge

There's no concept of an array data type in Befunge, but you'd typically store your arrays as sequences of data in the Befunge code space.
You'd then loop over the range of indices required to access those arrays, and use the loop variable as an offset into each data area.
For arrays of differing lengths, you'd need to manually check for an out-of-range index and deal with it appropriately.


```befunge>0
:2g,:3g,:4gv
@_^#`2:+1,+55,<
abc
ABC
123
```



## C


Given several arrays, especially if they are heterogeneous, the most
ordinary way to loop over all of them is to simply use an index
variable. Determining when to stop is generally done in some
application-specific way.


```c
#include <stdio.h>

char a1[] = {'a','b','c'};
char a2[] = {'A','B','C'};
int a3[] = {1,2,3};

int main(void) {
    for (int i = 0; i < 3; i++) {
        printf("%c%c%i\n", a1[i], a2[i], a3[i]);
    }
}
```


(Note: Some compilers may require a flag to accept this modern C code,
such as <code>gcc -std=c99</code>.)

On the other hand, it is possible to write a more generic higher-order
iteration scheme, as demonstrated in
[[Loop_over_multiple_arrays_simultaneously/C-Elaboration|this example]].
 There, a type for arrays with runtime-specified lengths and polymorphic
 printing is defined, and the iteration continues up to the length of
the shortest array.

=={{header|C sharp|C#}}==

```csharp
class Program
{
    static void Main(string[] args)
    {
        char[] a = { 'a', 'b', 'c' };
        char[] b = { 'A', 'B', 'C' };
        int[] c = { 1, 2, 3 };
        int min = Math.Min(a.Length, b.Length);
        min = Math.Min(min, c.Length);
        for (int i = 0; i < min; i++)
            Console.WriteLine("{0}{1}{2}", a[i], b[i], c[i]);
    }
}
```



Using Enumerable.Zip (stops when either source runs out of elements):


```csharp

int[] numbers = { 1, 2, 3, 4 };
string[] words = { "one", "two", "three" };
Console.WriteLine(numbers.Zip(words, (first, second) => first + " " +
 second));


```



Like how a perl programmer would write it (still using Zip):


```csharp

Console.WriteLine((new[] { 1, 2, 3, 4 }).Zip(new[] { "a", "b", "c" },
(f, s) => f + " " + s));

```



Custom implementation for arrays of different lengths that pads with spaces after the end of the shorter arrays:

```csharp

        public static void Multiloop(char[] A, char[] B, int[] C)
        {
            var max = Math.Max(Math.Max(A.Length, B.Length), C.Length);
            for (int i = 0; i < max; i++)
               Console.WriteLine($"{(i < A.Length ? A[i] : ' ')}, {(i < B.Length ? B[i] : ' ')}, {(i < C.Length ? C[i] : ' ')}");
        }

```

usage:

```csharp
Multiloop(new char[] { 'a', 'b', 'c', 'd' }, new char[] { 'A', 'B', 'C' }, new int[] { 1, 2, 3, 4, 5 });
```



## C++

With <code>std::vector</code>s:

```cpp
#include <iostream>
#include <vector>

int main(int argc, char* argv[])
{
   std::vector<char> ls(3); ls[0] = 'a'; ls[1] = 'b'; ls[2] = 'c';
   std::vector<char> us(3); us[0] = 'A'; us[1] = 'B'; us[2] = 'C';
   std::vector<int> ns(3);  ns[0] = 1;   ns[1] = 2;   ns[2] = 3;

   std::vector<char>::const_iterator lIt = ls.begin();
   std::vector<char>::const_iterator uIt = us.begin();
   std::vector<int>::const_iterator nIt = ns.begin();
   for(; lIt != ls.end() && uIt != us.end() && nIt !=
ns.end();
       ++lIt, ++uIt, ++nIt)
   {
      std::cout << *lIt << *uIt << *nIt << "\n";
   }
}
```


Using static arrays:

```cpp
#include <iostream>

int main(int argc, char* argv[])
{
   char ls[] = {'a', 'b', 'c'};
   char us[] = {'A', 'B', 'C'};
   int ns[] = {1, 2, 3};

   for(size_t li = 0, ui = 0, ni = 0;
       li < sizeof(ls) && ui < sizeof(us) && ni
< sizeof(ns) / sizeof(int);
       ++li, ++ui, ++ni)
   {
      std::cout << ls[li] << us[ui] << ns[ni] <<
 "\n";
   }
}
```



### C++11

With <code>std::vector</code>s:

```cpp
#include <iostream>
#include <vector>

int main(int argc, char* argv[])
{
    auto lowers = std::vector<char>({'a', 'b', 'c'});
    auto uppers = std::vector<char>({'A', 'B', 'C'});
    auto nums = std::vector<int>({1, 2, 3});

    auto ilow = lowers.cbegin();
    auto iup = uppers.cbegin();
    auto inum = nums.cbegin();

    for(; ilow != lowers.end()
        and iup != uppers.end()
        and inum != nums.end()
        ; ++ilow, ++iup, ++inum)
    {
       std::cout << *ilow << *iup << *inum << "\n";
    }
}
```


Using static arrays:

```cpp
#include <iostream>
#include <iterator>

int main(int argc, char* argv[])
{
    char lowers[] = {'a', 'b', 'c'};
    char uppers[] = {'A', 'B', 'C'};
    int nums[] = {1, 2, 3};

    auto ilow = std::begin(lowers);
    auto iup = std::begin(uppers);
    auto inum = std::begin(nums);

    for(; ilow != std::end(lowers)
        and iup != std::end(uppers)
        and inum != std::end(nums)
        ; ++ilow, ++iup, ++inum )
    {
       std::cout << *ilow << *iup << *inum << "\n";
    }
}
```


With <code>std::array</code>s:

```cpp
#include <iostream>
#include <array>

int main(int argc, char* argv[])
{
    auto lowers = std::array<char, 3>({'a', 'b', 'c'});
    auto uppers = std::array<char, 3>({'A', 'B', 'C'});
    auto nums = std::array<int, 3>({1, 2, 3});

    auto ilow = lowers.cbegin();
    auto iup = uppers.cbegin();
    auto inum = nums.cbegin();

    for(; ilow != lowers.end()
        and iup != uppers.end()
        and inum != nums.end()
        ; ++ilow, ++iup, ++inum )
    {
       std::cout << *ilow << *iup << *inum << "\n";
    }
}
```


With <code>std::array</code>s by indexes:

```cpp
#include <iostream>
#include <array>
#include <algorithm>

int main(int argc, char* argv[])
{
    auto lowers = std::array<char, 3>({'a', 'b', 'c'});
    auto uppers = std::array<char, 3>({'A', 'B', 'C'});
    auto nums = std::array<int, 3>({1, 2, 3});

    auto const minsize = std::min(
                lowers.size(),
                std::min(
                    uppers.size(),
                    nums.size()
                    )
                );

    for(size_t i = 0; i < minsize; ++i)
    {
       std::cout << lowers[i] << uppers[i] << nums[i] << "\n";
    }
}
```



## Chapel



```chapel
var a1 = [ "a", "b", "c" ];
var a2 = [ "A", "B", "C" ];
var a3 = [  1,   2,   3  ];

for (x,y,z) in zip(a1, a2, a3) do
    writeln(x,y,z);
```



## Clojure


```clojure
(doseq [s (map #(str %1 %2 %3) "abc" "ABC" "123")]
  (println s))
```

The sequence stops when the shortest list is exhausted.


## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Loop-Over-Multiple-Tables.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A VALUE "abc".
           03  A-Vals PIC X OCCURS 3 TIMES.

       01  B VALUE "ABC".
           03  B-Vals PIC X OCCURS 3 TIMES.

       01  C VALUE "123".
           03  C-Vals PIC 9 OCCURS 3 TIMES.

       01  I PIC 9.

       PROCEDURE DIVISION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL 3 < I
               DISPLAY A-Vals (I) B-Vals (I) C-Vals (I)
           END-PERFORM

           GOBACK
           .
```



## Common Lisp



###  Using functional application


```lisp
(mapc (lambda (&rest args)
        (format t "~{~A~}~%" args))
      '(|a| |b| |c|)
      '(a b c)
      '(1 2 3))
```

If lists are different lengths, it stops after the shortest one.


###  Using LOOP


```lisp

  (loop for x in '("a" "b" "c")
        for y in '(a b c)
        for z in '(1 2 3)
        do (format t "~a~a~a~%" x y z))

```



## D


```d
import std.stdio, std.range;

void main () {
    foreach (a, b, c; zip("abc", "ABC", [1, 2, 3]))
        writeln(a, b, c);
}
```

{{out}}

```txt
aA1
bB2
cC3
```

zip() allows to specify the stopping policy.
On default it stops when the shortest range is exhausted
(same as StoppingPolicy.shortest):

```d
import std.stdio, std.range;

void main () {
    auto a1 = [1, 2];
    auto a2 = [1, 2, 3];
    alias StoppingPolicy sp;

    // Stops when the shortest range is exhausted
    foreach (p; zip(sp.shortest, a1, a2))
        writeln(p.tupleof);
    writeln();

    // Stops when the longest range is exhausted
    foreach (p; zip(sp.longest, a1, a2))
        writeln(p.tupleof);
    writeln();

    // Requires that all ranges are equal
    foreach (p; zip(sp.requireSameLength, a1, a2))
        writeln(p.tupleof);
}
```

{{out}}

```txt
11
22

11
22
03

11
22
```

Followed by an exception with message "Inequal-length ranges passed to
Zip".

There is also std.range.lockstep:

```d
import std.stdio, std.range;

void main() {
    auto arr1 = [1, 2, 3, 4, 5];
    auto arr2 = [6, 7, 8, 9, 10];

    foreach (ref a, ref b; lockstep(arr1, arr2))
        a += b;

    assert(arr1 == [7, 9, 11, 13, 15]);

    // Lockstep also supports iteration with an index variable
    foreach (index, a, b; lockstep(arr1, arr2))
        writefln("Index %s:  a = %s, b = %s", index, a, b);
}
```

Lower level code that stops at the shortest length:

```d
import std.stdio, std.algorithm;

void main () {
    auto s1 = "abc";
    auto s2 = "ABC";
    auto a1 = [1, 2];

    foreach (i; 0 .. min(s1.length, s2.length, a1.length))
        writeln(s1[i], s2[i], a1[i]);
}
```

{{out}}

```txt
aA1
bB2
```



## Delphi


```Delphi
program LoopOverArrays;

{$APPTYPE CONSOLE}

uses SysUtils;

const
  ARRAY1: array [1..3] of string = ('a', 'b', 'c');
  ARRAY2: array [1..3] of string = ('A', 'B', 'C');
  ARRAY3: array [1..3] of Integer = (1, 2, 3);
var
  i: Integer;
begin
  for i := 1 to 3 do
    Writeln(Format('%s%s%d', [ARRAY1[i], ARRAY2[i], ARRAY3[i]]));

  Readln;
end.
```



## DWScript

If the arrays don't have the same bounds, an index out of bound
exception will be triggered when attempting to access a non-existing
element.


```delphi
const a1 = ['a', 'b', 'c'];
const a2 = ['A', 'B', 'C'];
const a3 = [1, 2, 3];

var i : Integer;
for i := 0 to 2 do
   PrintLn(Format('%s%s%d', [a1[i], a2[i], a3[i]]));
```



## E


E lacks a nice way to do this; this is
[http://wiki.erights.org/wiki/Parallel_iteration to be fixed, once we
figure out what to do]. However, iteration over an List produces its
indexes as keys, so a not entirely awful idiom exists:


```e
def a1 := ["a","b","c"]
def a2 := ["A","B","C"]
def a3 := ["1","2","3"]

for i => v1 in a1 {
    println(v1, a2[i], a3[i])
}
```


This will obviously fail if a2 or a3 are shorter than a1, and omit items
 if a2 or a3 are longer.

Given a parallel iteration utility, we might write this:


```e
for [v1, v2, v3] in zip(a1, a2, a3) {
    println(v1, v2, v3)
}
```


<code>zip</code> cannot yet be defined for all collections
(other than by iterating over each one and storing the results in a List
 first); but we can define it for numeric-indexed collections such as
Lists, as below. Both a definition for any number of collections and two
 collections is given; the latter in order to demonstrate the principle
without the clutter resulting from handling a variable number of
collections.


```e
def zip {
  to run(l1, l2) {
    def zipped {
      to iterate(f) {
        for i in int >= 0 {
          f(i, [l1.fetch(i, fn { return }),
                l2.fetch(i, fn { return })])
        }
      }
    }
    return zipped
  }

  match [`run`, lists] {
    def zipped {
      to iterate(f) {
        for i in int >= 0 {
          var tuple := []
          for l in lists {
            tuple with= l.fetch(i, fn { return })
          }
          f(i, tuple)
        }
      }
    }
    zipped
  }
}
```


(This will stop when the end of the shortest collection is reached.)


## EchoLisp


```scheme

;; looping over different sequences : infinite stream, string, list and vector
;; loop stops as soon a one sequence ends.
;; the (iota 6) = ( 0 1 2 3 4 5) sequence will stop first.


(for ((i (in-naturals 1000)) (j "ABCDEFGHIJK") (k (iota 6)) (m #(o p q r s t u v w)))
    (writeln i j k m))

1000     "A"     0     o
1001     "B"     1     p
1002     "C"     2     q
1003     "D"     3     r
1004     "E"     4     s
1005     "F"     5     t

```



## Efene


```efene
@public
run = fn () {
    lists.foreach(fn ((A, B, C)) { io.format("~s~n", [[A, B, C]]) },
lists.zip3("abc", "ABC", "123"))
}

```


If the lists are not all the same length, an error is thrown.


## Ela



```ela
open monad io list imperative

xs = zipWith3 (\x y z -> show x ++ show y ++ show z) ['a','b','c']
  ['A','B','C'] [1,2,3]

print x = do putStrLn x

print_and_calc xs = do
  xss <- return xs
  return $ each print xss

print_and_calc xs ::: IO
```


The code above can be written shorter. First there is no need in lists
as soon as strings in Ela can be treated as lists. Also instead of
explicit labmda one can use partial application and a standard
composition operator:


```ela
xs = zipWith3 (\x -> (x++) >> (++)) "abc" "ABC"
 "123"
```



## Elixir

'''string list:'''

```elixir
l1 = ["a", "b", "c"]
l2 = ["A", "B", "C"]
l3 = ["1", "2", "3"]
IO.inspect List.zip([l1,l2,l3]) |> Enum.map(fn x-> Tuple.to_list(x) |> Enum.join end)
#=> ["aA1", "bB2", "cC3"]
```


'''char_list:'''

```elixir
l1 = 'abc'
l2 = 'ABC'
l3 = '123'
IO.inspect List.zip([l1,l2,l3]) |> Enum.map(fn x-> Tuple.to_list(x) end)
#=> ['aA1', 'bB2', 'cC3']
```


When the length of the list is different:

```elixir
iex(1)> List.zip(['abc','ABCD','12345']) |> Enum.map(&Tuple.to_list(&1))
['aA1', 'bB2', 'cC3']
iex(2)> List.zip(['abcde','ABC','12']) |> Enum.map(&Tuple.to_list(&1))
['aA1', 'bB2']
```

The zipping finishes as soon as any enumerable completes.


## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

public program()
{
    var a1 := new string[]::("a","b","c");
    var a2 := new string[]::("A","B","C");
    var a3 := new int[]::(1,2,3);

    for(int i := 0, i < a1.Length, i += 1)
    {
        console.printLine(a1[i], a2[i], a3[i])
    };

    programUsingZip();

    console.readChar()
}
```


Using zipBy extension:

```elena
import system'routines.
import extensions.

public program
{
    var a1 := new string[]::("a","b","c");
    var a2 := new string[]::("A","B","C");
    var a3 := new int[]::(1,2,3);
    var zipped := a1.zipBy(a2,(first,second => first + second.toString() ))
                       .zipBy(a3, (first,second => first + second.toString() ));

    zipped.forEach:(e)
        { console.writeLine:e };

    console.readChar();
}
```

{{out}}

```txt

aA1
bB2
cC3

```



## Erlang

Shortest option:

```erlang
lists:zipwith3(fun(A,B,C)->
io:format("~s~n",[[A,B,C]]) end, "abc", "ABC", "123").
```

However, as every expression in Erlang has to return something, printing text returns 'ok'. A list with as many 'ok's as there are lines printed will thus be created. The technically cleanest way to do things would be with
<tt>lists:foreach/2</tt>, which also guarantees evaluation
order:

```erlang
lists:foreach(fun({A,B,C}) ->
io:format("~s~n",[[A,B,C]]) end,
              lists:zip3("abc", "ABC", "123")).
```

If the lists are not all the same length, an error is thrown.


## Euphoria

There are many ways to do this.  All of them rely on what strings really
 are.
If they are all "strings", it's quite easy:

```Euphoria

sequence a, b, c

a = "abc"
b = "ABC"
c = "123"

for i = 1 to length(a) do
    puts(1, a[i] & b[i] & c[i] & "\n")
end for

```


If not, and the other sequence is known to contain only integers:


```Euphoria

sequence a, b, c

a = "abc"
b = "ABC"
c = {1, 2, 3}

for i = 1 to length(a) do
    printf(1, "%s%s%g\n", {a[i], b[i], c[i]})
end for

```


A general solution for any arbitrary strings of characters or numbers
can get a bit complex.  This is because of how sequences are stored and
printed out.  One possible answer is as follows, if you know that only
alphanumeric characters are used:

```Euphoria

for i = 1 to length(a) do
    if (a[i] >= '0' and a[i] <= '9') then
	a[i] -= '0'
    end if
    if (b[i] >= '0' and b[i] <= '9') then
	b[i] -= '0'
    end if
    if (c[i] >= '0' and c[i] <= '9') then
	c[i] -= '0'
    end if
    printf(1, "%s%s%s\n", {a[i], b[i], c[i]})
end for

```

Just as in Java, using single quotes around a character gives you its
"char value".  In Euphoria, though, it is simply that character's code
in ASCII.

With all three of the above solutions, if any of the strings are smaller
 than the first, it will return an error.

=={{header|F_Sharp|F#}}==

```fsharp
for c1,c2,n in Seq.zip3 ['a';'b';'c'] ['A';'B';'C']
[1;2;3] do
  printfn "%c%c%d" c1 c2 n
```


When one sequence is exhausted, any remaining elements in the other
sequences are ignored.


## Factor


```factor
"abc" "ABC" "123" [ [ write1 ] tri@ nl ]
3each
```



## Fantom


This will stop when it reaches the end of the shortest list.

```fantom

class LoopMultiple
{
  public static Void main ()
  {
    List arr1 := ["a", "b", "c"]
    List arr2 := ["A", "B", "C"]
    List arr3 := [1, 2, 3]
    [arr1.size, arr2.size, arr3.size].min.times |Int i|
    {
      echo ("${arr1[i]}${arr2[i]}${arr3[i]}")
    }
  }
}

```



## Forth


```forth
create a  char a , char b , char c ,
create b  char A , char B , char C ,
create c  char 1 , char 2 , char 3 ,

: main
  3 0 do cr
    a i cells + @ emit
    b i cells + @ emit
    c i cells + @ emit
  loop
  cr
  a b c
  3 0 do cr
    3 0 do
      rot dup @ emit cell+
    loop
  loop
  drop drop drop
;
```



## Fortran



```fortran
program main
 implicit none

 integer,parameter :: n_vals = 3
 character(len=*),dimension(n_vals),parameter :: ls = ['a','b','c']
 character(len=*),dimension(n_vals),parameter :: us = ['A','B','C']
 integer,dimension(n_vals),parameter          :: ns = [1,2,3]

 integer :: i  !counter

 do i=1,n_vals
      write(*,'(A1,A1,I1)') ls(i),us(i),ns(i)
 end do

end program main

```


If the arrays are of different length (say, array ns has no third element), then when its turn comes the next unit of storage along from the second element will be accessed, its content interpreted as an integer, and its decimal value printed... If however, array bound checking is activated (or there is a memory access protection scheme that would detect this), a feature unavailable via many compilers and not the default on the rest, then an error will be detected and the run will be terminated, possibly with a somewhat helpful message.

If instead of reading the action had been to store a value into the array, then in the absence of bound checking, arbitrary damage will be done (to code or data) that will possibly result in something going wrong. And if you're lucky, it will happen swiftly.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function min(x As Integer, y As Integer) As Integer
  Return IIf(x < y, x, y)
End Function

Dim arr1(1 To 3) As String = {"a", "b", "c"}
Dim arr2(1 To 3) As String = {"A", "B", "C"}
Dim arr3(1 To 3) As Integer = {1, 2, 3}

For i As Integer = 1 To 3
  Print arr1(i) & arr2(i) & arr3(i)
Next

Print

' For arrays of different lengths we would need to iterate up to the mimimm length of all 3 in order
' to  get a contribution from each one. For example:

Dim arr4(1 To 4) As String = {"A", "B", "C", "D"}
Dim arr5(1 To 2) As Integer = {1, 2}

Dim ub As Integer = min(UBound(arr1), min(UBound(arr4), UBound(arr5)))
For i As Integer = 1 To ub
  Print arr1(i) & arr2(i) & arr3(i)
Next

Print
Sleep
```


{{out}}

```txt

aA1
bB2
cC3

aA1
bB2

```



## FunL


```funl
import lists.zip3

for x <- zip3( ['a', 'b', 'c'], ['A', 'B', 'C'], [1, 2, 3] )
    println( x.mkString() )
```


{{out}}


```txt

aA1
bB2
cC3

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=3a69e733694aeab3b72c6a5c0316535b Click this link to run this code]'''

```gambas
Public Sub Main()
Dim a1 As String[] = ["a", "b", "c"]
Dim a2 As String[] = ["A", "B", "C"]
Dim a3 As String[] = ["1", "2", "3"]
Dim siC As Short

For siC = 0 To a1.Max
  Print a1[siC] & a2[siC] & a3[siC]
Next

End
```


Output:

```txt

aA1
bB2
cC3

```



## GAP


```gap

# The Loop function will apply some function to every tuple built by
taking
# the i-th element of each list. If one of them is exhausted before the
others,
# the loop continues at its begining. Only the longests lists will be
precessed only once.
Loop := function(a, f)
    local i, j, m, n, v;
    n := Length(a);
    v := List(a, Length);
    m := Maximum(v);
    for j in [1 .. m] do
        f(List([1 .. n], i -> a[i][1 + RemInt(j - 1, v[i])]));
    od;
end;

# Here we simply print each "row"
f := function(u)
    Perform(u, Print);
    Print("\n");
end;

Loop([["a", "b", "c"], ["A", "B", "C"], [1, 2, 3]], f);

aA1
bB2
cC3

Loop([["a", "b"], ["A", "B", "C", "D", "E"], [1, 2, 3]], f);

aA1
bB2
aC3
bD1
aE2
```



## Go

Go's "range clause" of a for statement only looks at a single iterable
value (array, slice, etc).
To access the three in parallel, they have to be explicitly indexed.

If <code>a2</code> or <code>a3</code> were
shorter, the program would panic with "runtime error: index out of
range".
If <code>a2</code> or <code>a3</code> were
longer, extra elements would be ignored.
Go's philosophy is that you should explicitly check for whatever
conditions are meaningful in your application and explicitly handle
whatever errors are plausible.

```go
package main

import "fmt"

var a1 = []string{"a", "b", "c"}
var a2 = []byte{'A', 'B', 'C'}
var a3 = []int{1, 2, 3}

func main() {
	for i := range a1 {
		fmt.Printf("%v%c%v\n", a1[i], a2[i], a3[i])
	}
}
```



## Golfscript


```golfscript
["a" "b" "c"]:a;
["A" "B" "C"]:b;
["1" "2" "3"]:c;
[a b c]zip{puts}/
```


If there are arrays of different size, the shorter are treated as
"null-padded" array.


## Groovy

Solution:

```groovy
def synchedConcat = { a1, a2, a3 ->
    assert a1 && a2 && a3
    assert a1.size() == a2.size()
    assert a2.size() == a3.size()
    [a1, a2, a3].transpose().collect { "${it[0]}${it[1]}${it[2]}" }
}
```


Test:

```groovy
def x = ['a', 'b', 'c']
def y = ['A', 'B', 'C']
def z = [1, 2, 3]

synchedConcat(x, y, z).each { println it }
```


{{out}}

```txt
aA1
bB2
cC3
```



## Harbour

'''Using FOR EACH ... NEXT statement'''

```visualfoxpro

PROCEDURE Main()
	LOCAL a1 := { "a", "b", "c" }, ;
	      a2 := { "A", "B", "C", "D" }, ; // the last element "D" of this array will be ignored
	      a3 := { 1, 2, 3 }
	LOCAL e1, e2, e3

	FOR EACH e1, e2, e3 IN a1, a2, a3
		Qout( e1 + e2 + hb_ntos( e3 ) )
	NEXT
	RETURN

```

Output:
   aA1
   bB2
   cC3
If the arrays are not of equal length, the iteration stops after the last item of the smaller array has been processed;
any extra items of lengthier arrays are ignored (or in other words, the iteration counter never exceeds the length of the smaller array, thus preventing an 'out of subscript range' error).




## Haskell

'''Using list comprehension'''


```haskell
{-# LANGUAGE ParallelListComp #-}
main = sequence [ putStrLn [x, y, z] | x <- "abd" | y <- "ABC" | z <- "123"]
```


'''Using Transpose'''

In this special case of transposing strings.


```haskell
import Data.List
main = mapM putStrLn $ transpose ["abd", "ABC", "123"]
```


'''Using ZipWith*'''


```haskell
import Data.List
main = mapM putStrLn $ zipWith3 (\a b c -> [a,b,c]) "abc" "ABC" "123"
```


'''Using applicative ZipLists'''

ZipLists generalize zipWith to any number of parameters

```haskell
import Control.Applicative
main = sequence $ getZipList $ (\x y z -> putStrLn [x, y, z]) <$> ZipList "abd" <*> ZipList "ABC" <*> ZipList "123"
```



## Haxe


```haxe
using Lambda;
using Std;

class Main
{

	static function main()
	{
		var a = ['a', 'b', 'c'];
		var b = ['A', 'B', 'C'];
		var c = [1, 2, 3];

		//Find smallest array
		var len = [a, b, c]
			.map(function(a) return a.length)
			.fold(Math.min, 0x0FFFFFFF)
			.int();

		for (i in 0...len)
			Sys.println(a[i] + b[i] + c[i].string());
	}
}
```



## HicEst


```HicEst
CHARACTER :: A = "abc"
REAL ::  C(3)

C = $ ! 1, 2, 3

DO i = 1, 3
   WRITE() A(i), "ABC"(i), C(i)
ENDDO
```


=={{header|Icon}} and {{header|Unicon}}==

The first solution uses co-expressions to produce parallel evaluation.

```Icon
procedure main()
a := create !["a","b","c"]
b := create !["A","B","C"]
c := create !["1","2","3"]
while write(@a,@b,@c)
end
```


The second solution is more like other procedural languages
and also handles unequal list lengths.

```Icon
link numbers  # for max

procedure main()

a := ["a","b","c"]
b := ["A","B","C","D"]
c := [1,2,3]

every i := 1 to max(*a,*b,*c) do
   write(a[i]|"","\t",b[i]|"","\t",c[i]|"")
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/procs/numbers.htm Uses max from
numbers]


## J

For arrays of different types:

```J
   ,.&:(":"0@>)/ 'abc' ; 'ABC' ; 1 2 3
aA1
bB2
cC3
```

This approach works by representing the digits as characters.

Where arrays are all the same type (all numeric or all string):

```J
   ,.&:>/ 'abc' ; 'ABC' ; '123'
aA1
bB2
cC3
```


Both of these implementations reject arrays with conflicting lengths.

Other options include:


```J
   |: 'abc', 'ABC' ,:;":&> 1 2 3
aA1
bB2
cC3
```


```J
   |: 'abc', 'ABC',: '123'
aA1
bB2
cC3
```

These implementations pad short arrays with spaces.

Or:


```J
   |:>]&.>L:_1 'abc';'ABC';<1 2 3
┌─┬─┬─┐
│a│A│1│
├─┼─┼─┤
│b│B│2│
├─┼─┼─┤
│c│C│3│
└─┴─┴─┘
```


This implementation puts each item from each of the original lists
into a box and forms an array of boxes.
(A "box" is a immutable pointer to immutable data
-- in other words value semantics instead of reference semantics --
and "putting an item into a box" is obtaining one of these pointers for
that item.)
This implementation extends any short array by providing empty boxes
to represent the missing elements.
(An "empty box" is what a programmer in another language might call
"a pointer to a zero length array".)


## Java

{{trans|JavaScript}}

```java
String[] a = {"a","b","c"};
String[] b = {"A","B","C"};
int[] c = {1,2,3};
for(int i = 0;i < a.length;i++){
    System.out.println(a[i] + b[i] + c[i]);
}
```

If the first array is too short, it will stop
when it gets to the end of the first array.
If one of the other arrays is too short,
an <code>ArrayIndexOutOfBoundException</code> will be
thrown.


## JavaScript



### Imperative

This loops over the indices of the first array,
and uses that to index into the others.

```javascript
var a = ["a","b","c"],
    b = ["A","B","C"],
    c = [1,2,3],
    output = "",
    i;
for (i = 0; i < a.length; i += 1) {
    output += a[i] + b[i] + c[i] + "\n";
}
```

If the b or c arrays are too "short",
you will see the string "undefined" appear in the output.

Alternatively, we can nest a couple of calls to '''.forEach()''': one for the array of three arrays, and one for each of the three index positions:


```JavaScript
var lstOut = ['', '', ''];

[["a", "b", "c"], ["A", "B", "C"], ["1", "2", "3"]].forEach(
  function (a) {
    [0, 1, 2].forEach(
      function (i) {
        // side-effect on an array outside the function
        lstOut[i] += a[i];
      }
    );
  }
);

// lstOut --> ["aA1", "bB2", "cC3"]
```



### Functional composition


### =ES5=


Functional options include folding across an array of arrays with the built-in '''Array.reduce()''',
using a zipWith() function of suitable arity, or mapping over the output of a generic (any arity) zip() function.

(The generic zip function is the most tolerant – it simply ignores further elements in any arrays which are longer than the shortest array).

Reduce / fold:


```JavaScript
(function (lstArrays) {

    return lstArrays.reduce(
        function (a, e) {
            return [
                a[0] + e[0],
                a[1] + e[1],
                a[2] + e[2]
            ];
        }, ['', '', ''] // initial copy of the accumulator
    ).join('\n');

})([
    ["a", "b", "c"],
    ["A", "B", "C"],
    ["1", "2", "3"]
]);
```


A fixed arity ZipWith:


```JavaScript
(function (x, y, z) {

    // function of arity 3 mapped over nth items of each of 3 lists
    // (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
    function zipWith3(f, xs, ys, zs) {
        return zs.length ? [f(xs[0], ys[0], zs[0])].concat(
            zipWith3(f, xs.slice(1), ys.slice(1), zs.slice(1))) : [];
    }

    function concat(x, y, z) {
        return ''.concat(x, y, z);
    }

    return zipWith3(concat, x, y, z).join('\n')

})(["a", "b", "c"], ["A", "B", "C"], [1, 2, 3]);
```


Or we could write a generic '''zipListsWith''' applying some supplied function overs lists derived from the nth members of an arbitrary list of (equal-length) lists.


```JavaScript
(function () {
    'use strict';

    // zipListsWith :: ([a] -> b) -> [[a]] -> [[b]]
    function zipListsWith(f, xss) {
        return (xss.length ? xss[0] : [])
            .map(function (_, i) {
                return f(xss.map(function (xs) {
                    return xs[i];
                }));
            });
    }

    // concat :: [a] -> s
    function concat(lst) {
        return ''.concat.apply('', lst);
    }

    // TEST
    return zipListsWith(
        concat,
        [["a", "b", "c"], ["A", "B", "C"], [1, 2, 3]]
    )
    .join('\n');
})();
```

{{Out}}

```JavaScript
aA1
bB2
cC3
```



### =ES6=


By transposition:

```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS -----------------------------------------------------

    // concat :: [[a]] -> [a]
    const concat = xs =>
        xs.length > 0 ? (() => {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        })() : [];

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, col) => xs.map(row => row[col]));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // TEST ------------------------------------------------------------------
    const xs = [
        ['a', 'b', 'c'],
        ['A', 'B', 'C'],
        [1, 2, 3]
    ];

    return unlines(
        map(concat, transpose(xs))
    );
})();
```

{{Out}}

```txt
aA1
bB2
cC3
```



## jq

The following solution is based on the assumption that all the arrays
can be presented as an array of arrays.
This allows any number of arrays to be handled.

Specifically, zip/0 expects an array of 0 or more arrays as its input.
The first array determines the number of items in the output;
nulls are used for padding.

```jq
# zip/0 emits [] if input is [].

def zip:
  . as $in
  | [range(0; $in[0]|length) as $i | $in | map( .[$i] ) ];
```


Example 1:
 [["a","b","c"], ["A","B","C"], [1,2,3]] | zip
{{Out}}

```txt

 [["a","A",1],["b","B",2],["c","C",3]]

```

To obtain the compact output used in the the task description,
we can filter the results through a "pretty-print" function:
 def pp: reduce .[] as $i (""; . + "\($i)");

Example 2:
 [["a","b","c"], ["A","B","C"], [1,2,3]] | zip | map(pp)
{{Out}}

```txt

 [
  "aA1",
  "bB2",
  "cC3"
 ]

```


As already mentioned, the above definition of zip/0 privileges the first
 array,
and if the subsequent arrays are of different lengths, null is used as a
 filler.
Thus:
 [["a","b","c"], ["A","B"], [1]] | zip

produces:
 [["a","A",1],["b","B",null],["c",null,null]]

'''Handling jagged input'''
An alternative approach would be use a variant of zip/0
that pads all arrays shorter than the longest with nulls.
Here is such a variant:

```jq

# transpose a possibly jagged matrix
def transpose:
  if . == [] then []
  else (.[1:] | transpose) as $t
  | .[0] as $row
  | reduce range(0; [($t|length), (.[0]|length)] | max) as $i
         ([]; . + [ [ $row[$i] ] + $t[$i] ])
  end;

```



## Jsish


```javascript
/* Loop over multiple arrays, in Jsish */
var a1 = ['a', 'b', 'c'];
var a2 = ['A', 'B', 'C'];
var a3 = [1, 2, 3];

puts('Equal sizes');
var arr = [a1, a2, a3];
var m = arr[0].length;
for (var a of arr) if (a.length > m) m = a.length;
for (var i = 0; i < m; i++) printf("%q%q%q\n", a1[i], a2[i], a3[i]);

puts('\nUnequal sizes');
var a4 = [];
var a5 = [4,5,6,7];

arr = [a1, a2, a3, a4, a5];
m = arr[0].length;
for (a of arr) if (a.length > m) m = a.length;
for (i = 0; i < m; i++) printf("%q%q%q%q%q\n", a1[i], a2[i], a3[i], a4[i], a5[i]);

/*
=!EXPECTSTART!=
Equal sizes
aA1
bB2
cC3

Unequal sizes
aA1undefined4
bB2undefined5
cC3undefined6
/home/btiffin/forge/jsi/jsi-test/rosetta/loopOverMultipleArrays.jsi:19: warn: call with undefined var for argument arg 2 '...', in call to 'printf' <undefined>.    (at or near "%q%q%q%q%q
")

undefinedundefinedundefinedundefined7
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u loopOverMultipleArrays.jsi
[PASS] loopOverMultipleArrays.jsi
```



## Julia

'''With a higher order function''':

```julia
foreach(println, ('a', 'b', 'c'), ('A', 'B', 'C'), (1, 2, 3))
```


'''With a loop''':

```julia
for (i, j, k) in zip(('a', 'b', 'c'), ('A', 'B', 'C'), (1, 2, 3))
    println(i, j, k)
end
```


{{out}}

```txt
aA1
bB2
cC3
```



## K


```K
{,/$x}'+("abc";"ABC";1 2 3)
```


{{out}}

```txt

("aA1"
 "bB2"
 "cC3")

```


If the length of the arrays are different,
then K croaks with "length error".

The following is a more general approach where

```K

      &/#:'x

```


calculates the minimum length of the arrays
and is used to index the first elements in each array.


```K

   {+x[;!(&/#:'x)]}("abc";"ABC";"1234")

```


{{out}}

```txt

("aA1"
 "bB2"
 "cC3")

```


If the arrays are of different type,
then the arrays must be converted to strings.


```K

   {a:,/'($:'x);+a[;!(&/#:'a)]}("abc";"ABC";1 2 3 4)

```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val a1 = charArrayOf('a', 'b', 'c')
    val a2 = charArrayOf('A', 'B', 'C')
    val a3 = intArrayOf(1, 2, 3)
    for(i in 0 .. 2) println("${a1[i]}${a2[i]}${a3[i]}")
    println()
    // For arrays of different sizes we would need to iterate up to the mimimm size of all 3 in order
    // to  get a contribution from each one.
    val a4 = intArrayOf(4, 5, 6, 7)
    val a5 = charArrayOf('d', 'e')
    val minSize = Math.min(a2.size, Math.min(a4.size, a5.size))  // minimum size of a2, a4 and a5
    for(i in 0 until minSize) println("${a2[i]}${a4[i]}${a5[i]}")
}
```


{{out}}

```txt

aA1
bB2
cC3

A4d
B5e

```



## LFE


```lisp

(lists:zipwith3
  (lambda (i j k)
    (io:format "~s~s~p~n" `(,i ,j ,k)))
    '(a b c)
    '(A B C)
    '(1 2 3))

```


If any of the data lists differ in size from the other,
the results will print out up to the shortest data list,
and then raise a <code>function_clause</code> error.

Erlang, and thus LFE, have <code>zipwith</code> and
<code>zipwith3</code> for working with 2 and 3 simultaneous
sets of data respectively.
If you need more than that, you'll need to create your own "zip"
function with something like <code lisp>(: lists map
...)</code>.


## Liberty BASIC


```lb
a$(1)="a" : a$(2)="b" : a$(3)="c"
b$(1)="A" : b$(2)="B" : b$(3)="C"
c(1)=1 : c(2)=2 : c(3)=3


for i = 1 to 3
    print a$(i);b$(i);c(i)
next
```



## Lisaac


```Lisaac
Section Header

+ name := ARRAY_LOOP_TEST;

Section Public

- main <- (
  + a1, a2 : ARRAY[CHARACTER];
  + a3 : ARRAY[INTEGER];

  a1 := ARRAY[CHARACTER].create 1 to 3;
  a2 := ARRAY[CHARACTER].create 1 to 3;
  a3 := ARRAY[INTEGER].create 1 to 3;

  1.to 3 do { i : INTEGER;
    a1.put ((i - 1 + 'a'.code).to_character) to i;
    a2.put ((i - 1 + 'A'.code).to_character) to i;
    a3.put i to i;
  };

  1.to 3 do { i : INTEGER;
    a1.item(i).print;
    a2.item(i).print;
    a3.item(i).print;
    '\n'.print;
  };
);
```



## LiveCode

Arrays

```LiveCode
command loopArrays
    local lowA, uppA, nums, z
    put "a,b,c" into lowA
    put "A,B,C" into uppA
    put "1,2,3" into nums

    split lowA by comma
    split uppA by comma
    split nums by comma

    repeat with n = 1 to the number of elements of lowA
        put lowA[n] & uppA[n] & nums[n] & return after z
    end repeat
    put z

end loopArrays
```

"list" processing

```LiveCode
command loopDelimitedList
    local lowA, uppA, nums, z
    put "a,b,c" into lowA
    put "A,B,C" into uppA
    put "1,2,3" into nums

    repeat with n = 1 to the number of items of lowA
        put item n of lowA & item n of uppA & item n of nums
& return after z
    end repeat
    put z

end loopDelimitedList
```

Output - both behave similarly for this exercise.

```txt
aA1
bB2
cC3
```


When there are fewer elements than the first (or whatever the loop is
based on), livecode will add an "empty" value. If we add a "d" to lowA
and a 4 to nums we get the following:

```txt
aA1
bB2
cC3
d4
```



## Logo

{{works with|UCB Logo}}


```logo
show (map [(word ?1 ?2 ?3)] [a b c] [A B C] [1 2 3])
 ; [aA1 bB2 cC3]

(foreach [a b c] [A B C] [1 2 3] [print (word ?1 ?2 ?3)])  ; as above,
one per line
```



## Lua

This can be done with a simple for loop:

```lua

a1, a2, a3 = {'a' , 'b' , 'c' } , { 'A' , 'B' , 'C' } , { 1 , 2 , 3 }
for i = 1, 3 do print(a1[i]..a2[i]..a3[i]) end

```

but it may be more enlightening
(and in line with the spirit of the challenge) to use the generic for:

```lua

function iter(a, b, c)
  local i = 0
  return function()
    i = i + 1
    return a[i], b[i], c[i]
  end
end

for u, v, w in iter(a1, a2, a3) do print(u..v..w) end

```



## Mathematica

This can be done with a built-in function:

```mathematica

MapThread[Print, {{"a", "b", "c"}, {"A", "B", "C"}, {1, 2, 3}}];

```

All arguments must be lists of the same length.


## Mercury

<lang>
:- module multi_array_loop.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, list, string.

main(!IO) :-
    A = ['a', 'b', 'c'],
    B = ['A', 'B', 'C'],
    C = [1, 2, 3],
    list.foldl_corresponding3(print_elems, A, B, C, !IO).

:- pred print_elems(char::in, char::in, int::in, io::di, io::uo) is det.

print_elems(A, B, C, !IO) :-
    io.format("%c%c%i\n", [c(A), c(B), i(C)], !IO).

```

The foldl_corresponding family of procedures all throw a
software_error/1
exception if the lengths of the lists are not the same.

=={{header|Modula-3}}==

```modula3
MODULE MultiArray EXPORTS Main;

IMPORT IO, Fmt;

TYPE ArrIdx = [1..3];

VAR
  arr1 := ARRAY ArrIdx OF CHAR {'a', 'b', 'c'};
  arr2 := ARRAY ArrIdx OF CHAR {'A', 'B', 'C'};
  arr3 := ARRAY ArrIdx OF INTEGER {1, 2, 3};

BEGIN
  FOR i := FIRST(ArrIdx) TO LAST(ArrIdx) DO
    IO.Put(Fmt.Char(arr1[i]) & Fmt.Char(arr2[i]) &
Fmt.Int(arr3[i]) & "\n");
  END;
END MultiArray.
```



## MUMPS

Pieces of String version

```MUMPS

LOOPMULT
 N A,B,C,D,%
 S A="a,b,c,d"
 S B="A,B,C,D"
 S C="1,2,3"
 S D=","
 F %=1:1:$L(A,",") W !,$P(A,D,%),$P(B,D,%),$P(C,D,%)
 K A,B,C,D,%
 Q

```

When there aren't enough elements,
a null string will be returned from the $Piece function.

{{out}}

```txt
USER>d LOOPMULT^ROSETTA

aA1
bB2
cC3
dD
```


Local arrays version

```MUMPS

LOOPMULU
 N A,B,C,D,%
 S A(1)="a",A(2)="b",A(3)="c",A(4)="d"
 S B(1)="A",B(2)="B",B(3)="C",B(4)="D"
 S C(1)="1",C(2)="2",C(3)="3"
 ; will error    S %=$O(A("")) F  Q:%=""  W !,A(%),B(%),C(%) S
%=$O(A(%))
 S %=$O(A("")) F  Q:%=""  W !,$G(A(%)),$G(B(%)),$G(C(%)) S %=$O(A(%))
 K A,B,C,D,%

```


The commented out line will throw an <UNDEFINED> error when trying
 to look up D(4). Using the $Get function as a wrapper means that if the
 subscript for the array doesn't exist, a null string will be returned.
This same syntax is used for globals (permanent variables, that have a
caret "^" as the first character).
{{out}}

```txt
USER>D LOOPMULU^ROSETTA

aA1
bB2
cC3
dD
USER>D LOOPMULV^ROSETTA

aA1
bB2
cC3
dD
 S %=$O(A("")) F  Q:%=""  W !,A(%),B(%),C(%) S %=$O(A(%))
                                        ^
<UNDEFINED>LOOPMULV+5^ROSETTA *C(4)
```



## Nemerle

It "feels" better to use zip() for this,
unfortunately the built in zip() only takes two lists.

```Nemerle
using System;
using System.Console;

module LoopMultiple
{
    Zip3[T1, T2, T3] (x : list[T1], y : list[T2], z : list[T3]) :
list[T1 * T2 * T3]
    {
        |(x::xs, y::ys, z::zs) => (x, y, z)::Zip3(xs, ys, zs)
        |([], [], [])          => []
        |(_, _, [])            => throw ArgumentNullException()
        |(_, [], _)            => throw ArgumentNullException()
        |([], _, _)            => throw ArgumentNullException()
    }

    Main() : void
    {
        def first  = ['a', 'b', 'c'];
        def second = ["A", "B", "C"];
        def third  = [1, 2, 3];

        foreach ((x, y, z) in Zip3(first, second, third))
            WriteLine($"$x$y$z");
    }
}
```


Alternately: {{trans|C#}}

```Nemerle
using System.Console;

module LoopMult
{
    Main() : void
    {
        def first  = array['a', 'b', 'c'];
        def second = array['A', 'B', 'C'];
        def third  = array[1, 2, 3];

        when (first.Length == second.Length && second.Length ==
third.Length)
            foreach (i in [0 .. (first.Length - 1)])
                WriteLine("{0}{1}{2}", first[i], second[i], third[i]);
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

say 'Using arrays'
aa = ['a', 'b', 'c', 'd']
bb = ['A', 'B', 'C']
cc = [1, 2, 3, 4]

loop x_ = 0 for aa.length
  do
    ax = aa[x_]
  catch ArrayIndexOutOfBoundsException
    ax = ' '
  end
  do
    bx = bb[x_]
  catch ArrayIndexOutOfBoundsException
    bx = ' '
  end
  do
    cx = cc[x_]
  catch ArrayIndexOutOfBoundsException
    cx = ' '
  end

  say ax || bx || cx
  end x_

say 'Using indexed strings (associative arrays)'
ai = sampleData('a b c d')
bi = sampleData('A B C')
ci = sampleData('1 2 3 4')

loop x_ = 1 to ai[0]
  say ai[x_] || bi[x_] || ci[x_]
  end x_

method sampleData(arg) public static returns Rexx
  smp = ' '
  smp[0] = arg.words
  loop i_ = 1 to smp[0]
    smp[i_] = arg.word(i_)
    end i_

  return smp

```

{{out}}
<pre style="overflow:scroll">
Using arrays
aA1
bB2
cC3
d 4
Using indexed strings (associative arrays)
aA1
bB2
cC3
d 4

```



## NewLISP


```NewLISP
(map println '(a b c) '(A B C) '(1 2
3))
```



## Nim


```nim
let
  a = @['a','b','c']
  b = @["A","B","C"]
  c = @[1,2,3]

for i in 0..2:
  echo a[i], b[i], c[i]
```


=={{header|Oberon-2}}==
Works with oo2c version 2

```oberon2

MODULE LoopMArrays;
IMPORT
	Out;
VAR
	x,y: ARRAY 3 OF CHAR;
	z: ARRAY 3 OF INTEGER;

PROCEDURE DoLoop;
VAR
	i: INTEGER;
BEGIN
	i := 0;
	WHILE i < LEN(x) DO
		Out.Char(x[i]);Out.Char(y[i]);Out.LongInt(z[i],0);Out.Ln;
		INC(i)
	END
END DoLoop;

BEGIN
	x[0] := 'a';y[0] := 'A';z[0] := 1;
	x[1] := 'b';y[1] := 'B';z[1] := 2;
	x[2] := 'c';y[2] := 'C';z[2] := 3;
	DoLoop
END LoopMArrays.

```

Output:<br/>

```txt

aA1
bB2
cC3

```



## Objeck


```objeck

class MultipleArrayAccess {
  function : Main(args : String[]) ~ Nil {
    a := ["a", "b", "c"];
    b := ["A", "B", "C"];
    c := [1, 2, 3];

    each(i : a) {
      a[i]->Append(b[i]);
      a[i]->Append(c[i]);
      a[i]->PrintLine();
    };
  }
}

```


If the arrays are different lengths,
then an out-of-bounds error will be raised.


## OCaml

an immediate solution:

```ocaml
let a1 = [| 'a'; 'b'; 'c' |]
and a2 = [| 'A'; 'B'; 'C' |]
and a3 = [| '1'; '2'; '3' |] ;;

Array.iteri (fun i c1 ->
  print_char c1;
  print_char a2.(i);
  print_char a3.(i);
  print_newline()
) a1 ;;
```


a more generic solution could be to use a function
which iterates over a list of arrays:


```ocaml
let n_arrays_iter ~f = function
  | [] -> ()
  | x::xs as al ->
      let len = Array.length x in
      let b = List.for_all (fun a -> Array.length a = len) xs in
      if not b then invalid_arg "n_arrays_iter: arrays of different
length";
      for i = 0 to pred len do
        let ai = List.map (fun a -> a.(i)) al in
        f ai
      done
```


this function raises Invalid_argument exception if arrays have different
 length,
and has this signature:


```ocaml
val n_arrays_iter : f:('a list -> unit) -> 'a
array list -> unit
```


how to use it with arrays a1, a2 and a3 defined before:


```ocaml
let () =
  n_arrays_iter [a1; a2; a3] ~f:(fun l ->
    List.iter print_char l;
    print_newline());
;;
```



## Oforth


If arrays don't have the same size, zipAll reduces to the minimum size


```Oforth
[ "a", "b", "c" ] [ "A", "B", "C" ] [ 1, 2, 3 ]
zipAll(3) apply(#[ apply(#print) printcr ])
```


{{out}}

```txt

aA1
bB2
cC3

```



## ooRexx


```ooRexx

x = .array~of("a", "b", "c")
y = .array~of("A", "B", "C")
z = .array~of(1, 2, 3)

loop i = 1 to x~size
    say x[i]y[i]z[i]
end

```



## Oz


```oz
for
   I in [a b c]
   J in ['A' 'B' 'C']
   K in [1 2 3]
do
   {System.showInfo I#J#K}
end
```


The loop will stop when the shortest list is exhausted.


## PARI/GP

This version stops when the shortest vector is exhausted.

```parigp
loopMultiple(V)={
  my(l=#V[1]);
  for(i=2,#V,l=min(l,#V[i]));
  for(i=1,#V[1],
    for(j=1,#V,
      print1(V[j][i])
    );
    print()
  )
};
```


This version prints blanks when a vector is exhausted.

```parigp
loopMultiple(V)={
  my(l=0);
  for(i=1,#V,l=max(l,#V[i]));
  for(i=1,#V[1],
    for(j=1,#V,
      if(#V[j]<i,
        print1(" ")
      ,
        print1(V[j][i])
      )
    );
    print()
  )
};
```



## Pascal

See [[Loop_over_multiple_arrays_simultaneously#Delphi | Delphi]]


## Perl


```perl
sub zip (&@)
{
        my $code = shift;
        my $min;
        $min = $min && $#$_ > $min ? $min : $#$_ for @_;

        for my $i(0..$min){ $code->(map $_->[$i] ,@_) }
}
my @a1 = qw( a b c );
my @a2 = qw( A B C );
my @a3 = qw( 1 2 3 );

zip { print @_,"\n" }\(@a1, @a2, @a3);
```


This implementation will stop producing items when the shortest array
ends.



## Perl 6

{{works with|rakudo|2015.12}}


```perl6>for <a b c> Z <A B C
 Z 1, 2, 3 -> ($x, $y, $z) {
   say $x, $y, $z;
}
```


The <code>Z</code> operator stops emitting items as soon as the shortest input list is exhausted. However, short lists are easily extended by replicating all or part of the list, or by appending any kind of lazy list generator to supply default values as necessary.

Since <code>Z</code> will return a list of lists (in this example, the first list is <code>('a', 'A', 1)</code>, parentheses are used around in the lambda signature <code>($x, $y, $z)</code> to unpack the list for each iteration.


###  Factoring out concatenation


Note that we can also factor out the concatenation by making the <tt>Z</tt> metaoperator apply the <tt>~</tt> concatenation operator across each triple:


```perl6>.say for <a b c
 Z~ <A B C> Z~ 1, 2, 3;
```


We could also use the zip-to-string with the reduction metaoperator:


```perl6
.say for [Z~] [<a b c>], [<A B C>], [1,2,3]
```



###  A list and its indices


The common case of iterating over a list and a list of its indices can be done using the same method:


```perl6
for ^Inf Z <a b c d> -> ($i, $letter) { ... }
```


or by using the <code>.kv</code> (key and value) method on the list (and dropping the parentheses because the list returned by <code>.kv</code> is a flattened list):


```perl6>for <a b c d
.kv -> $i, $letter { ... }
```



## Phix

Assumes a and b are strings and c is a sequence of integers.

If the arguments were not all the same length, attempts to retrieve non-existent elements would trigger a fatal run-time error.

```Phix
procedure print3(sequence a, b, c)
    for i=1 to min({length(a),length(b),length(c)}) do
        printf(1, "%s%s%g\n", {a[i], b[i], c[i]})
    end for
end procedure

print3("abc","ABC",{1, 2, 3})
```

{{out}}

```txt

aA1
bB2
cC3

```



## PHP


```PHP
$a = array('a', 'b', 'c');
$b = array('A', 'B', 'C');
$c = array('1', '2', '3'); //These don't *have* to be strings, but it
saves PHP from casting them later

if ((sizeOf($a) !== sizeOf($b)) || (sizeOf($b) !== sizeOf($c))){
  throw new Exception('All three arrays must be the same length');
}
foreach ($a as $key => $value){
  echo "{$a[$key]}{$b[$key]}{$c[$key]}\n";
}
```


This implementation throws an exception if the arrays are not all the
same length.


## PicoLisp


```PicoLisp
(mapc prinl
   '(a b c)
   '(A B C)
   (1 2 3) )
```

The length of the first argument list controls the operation.
If subsequent lists are longer, their remaining values are ignored.
If they are shorter, NIL is passed to the function.


## PL/I


```pli

declare P(3) character (1) initial ('a', 'b', 'c'),
        Q(3) character (1) initial ('A', 'B', 'C'),
        R(3) fixed decimal (1) initial (1, 2, 3);

do i = lbound(P,1) to hbound(P,1);
   put skip edit (P(i), Q(i), R(i)) (2 A, F(1));
end;

```



## PostScript

{{libheader|initlib}}

```postscript

% transpose is defined in initlib like this.
/transpose {
    [ exch {
        { {empty? exch pop} map all?} {pop exit} ift
        [ exch {} {uncons {exch cons} dip exch} fold counttomark 1 roll]
 uncons
    } loop ] {reverse} map
}.

% using it.
[[/a /b /c] [/A /B /C] [1 2 3]] transpose

```



## PowerBASIC


```powerbasic
FUNCTION PBMAIN () AS LONG
    DIM x(2), y(2) AS STRING * 1
    DIM z(2) AS LONG

    'data
    ARRAY ASSIGN x() = ("a", "b", "c")
    ARRAY ASSIGN y() = ("A", "B", "C")
    ARRAY ASSIGN z() = (1, 2, 3)

    'set upper bound
    C& = UBOUND(x)
    IF UBOUND(y) > C& THEN C& = UBOUND(y)
    IF UBOUND(z) > C& THEN C& = UBOUND(z)

    OPEN "output.txt" FOR OUTPUT AS 1
    FOR L& = 0 TO C&
        IF L& <= UBOUND(x) THEN PRINT #1, x(L&);
        IF L& <= UBOUND(y) THEN PRINT #1, y(L&);
        IF L& <= UBOUND(z) THEN PRINT #1, TRIM$(STR$(z(L&)));
        PRINT #1,
    NEXT
    CLOSE
END FUNCTION
```



## PowerShell

A cheap and chEasy 'zip' function:

```PowerShell

function zip3 ($a1, $a2, $a3)
{
    while ($a1)
    {
        $x, $a1 = $a1
        $y, $a2 = $a2
        $z, $a3 = $a3
        [Tuple]::Create($x, $y, $z)
    }
}

```


```PowerShell

zip3 @('a','b','c') @('A','B','C') @(1,2,3)

```

{{Out}}

```txt

Item1 Item2 Item3
----- ----- -----
a     A         1
b     B         2
c     C         3

```


```PowerShell

zip3 @('a','b','c') @('A','B','C') @(1,2,3) | ForEach-Object {$_.Item1 + $_.Item2 + $_.Item3}

```

{{Out}}

```txt

aA1
bB2
cC3

```




## Prolog

Works with SWI-Prolog

```Prolog
multiple_arrays(L1, L2, L3) :-
	maplist(display, L1, L2, L3).

display(A,B,C) :-
	writef('%s%s%s\n', [[A],[B],[C]]).

```

{{out}}

```txt
 ?- multiple_arrays("abc", "ABC", "123").
aA1
bB2
cC3
true.

 ?- multiple_arrays("abc", "AB", "123").
aA1
bB2
false.

```



## PureBasic


```PureBasic
OpenConsole()
; Fill arrays
Dim a.s(2)
Dim b.s(2)
Dim c(2)
For Arrayposition = 0 To ArraySize(a())
  a(Arrayposition) = Chr(Asc("a") + Arrayposition)
  b(Arrayposition) = Chr(Asc("A") + Arrayposition)
  c(Arrayposition) = Arrayposition + 1
Next
; loop over them
For Arrayposition = 0 To ArraySize(a())
  PrintN(a(Arrayposition) + b(Arrayposition) + Str(c(Arrayposition)))
Next
Input() ;wait for Enter before ending
```


If they have different lengths there are two cases:

a() is the shortest one: Only elements up to maximum index of a() are
printed

a() is bigger than another one:  if exceeding index to much, program
crashes,

else it may work because there is some "free space" after end of
assigned array memory.

For example if a has size 4, line dD4 will also be printed. size 20
leads to an crash

This is because ReDim becomes slow if everytime there is a change to
array size new memory has to be allocated.


## Python

Using <tt>zip()</tt>:

```python>>>
 print ( '\n'.join(''.join(x) for x in
zip('abc', 'ABC', '123')) )
aA1
bB2
cC3
>>>
```

If lists are different lengths, <tt>zip()</tt> stops after
the shortest one.

Using <tt>map()</tt>:

```python>>>
 print(*map(''.join, zip('abc', 'ABC', '123')), sep='\n')
aA1
bB2
cC3
>>>
```

If lists are different lengths, <tt>map()</tt> in Python 2.x pretends that the shorter lists were extended with
<tt>None</tt> items; <tt>map()</tt> in Python 3.x stops after the shortest one.

Using <tt>itertools.imap()</tt> (Python 2.x):

```python
from itertools import imap

def join3(a,b,c):
   print a+b+c

imap(join3,'abc','ABC','123')
```

If lists are differnt lengths, <tt>imap()</tt> stops after
the shortest is exhausted.

Python 3.X has <tt>zip_longest</tt> which fills shorter iterables with its
fillvalue argument which defaults to <tt>None</tt> (similar to the behavior of
''map()'' in Python 2.x):

```python>>>
 from itertools import zip_longest
>>> print ( '\n'.join(''.join(x) for x in zip_longest('abc',
'ABCD', '12345', fillvalue='#')) )
aA1
bB2
cC3
#D4
##5
>>>
```

(The Python 2.X equivalent is itertools.izip_longest)


## R


```R
multiloop <- function(...)
{
   # Retrieve inputs and convert to a list of character strings
   arguments <- lapply(list(...), as.character)

   # Get length of each input
   lengths <- sapply(arguments, length)

   # Loop over elements
   for(i in seq_len(max(lengths)))
   {
      # Loop over inputs
      for(j in seq_len(nargs()))
      {
         # print a value or a space (if that input has finished)
         cat(ifelse(i <= lengths[j], arguments[[j]][i], " "))
      }
      cat("\n")
   }
}
multiloop(letters[1:3], LETTERS[1:3], 1:3)
```


Same thing as a single function call.
But throws error if the arrays differ in length.


```R

apply(data.frame(letters[1:3], LETTERS[1:3], 1:3), 1,
      function(row) { cat(row, "\n", sep='') })

```



## Racket


Racket <tt>for</tt> loops can loop over an arbitrary number
of sequences of any kind at once:


```racket

#lang racket

(for ([x '(a b c)] ; list
      [y #(A B C)] ; vector
      [z "123"]
      [i (in-naturals 1)]) ; 1, 2, ... infinitely
  (printf "~s: ~s ~s ~s\n" i x y z))

```


The loop stops as soon as the first sequence terminates -- in the above
case <tt>i</tt> can iterate forever but looping stops when we reach the
end of the list/vector/string.  (The same holds for multiple containers
of different sizes.)


## Red

The word repeat evaluates a given block! a specified number of times
and exposes the count value to the block! being executed.
When a variable is used in a path notation, we put a colon in front of it.   :counter


```Red>>
blk: [["a" "b" "c"] ["A" "B" "C"] [1 2 3]]
== [["a" "b" "c"] ["A" "B" "C"] [1 2 3]]

>> repeat counter 3 [print [blk/1/:counter blk/2/:counter blk/3/:counter]]
a A 1
b B 2
c C 3
```



## REXX


### same size arrays

If any of the array's elements are missing or it is a short list,
a blank is substituted to retain visual fidelity in the output.



When   ''all''   elements are blank, then it signifies the end of the arrays.

```rexx
/*REXX program shows how to  simultaneously  loop over  multiple arrays.*/
x. = ' ';      x.1 =  "a";      x.2 = 'b';      x.3 = "c"
y. = ' ';      y.1 =  "A";      y.2 = 'B';      y.3 = "C"
z. = ' ';      z.1 =  "1";      z.2 = '2';      z.3 = "3"

           do j=1  until output=''
           output = x.j || y.j || z.j
           say output
           end    /*j*/                /*stick a fork in it, we're done.*/
```

'''output'''

```txt

aA1
bB2
cC3

```



### dissimilar sized arrays

In this example, two of the arrays are extended (past the 1<sup>st</sup> example).

Also note that REXX doesn't require quotes around non-negative numbers  (they're optional).

```rexx
/*REXX program shows how to  simultaneously  loop over  multiple arrays.*/
x.=' ';      x.1="a";      x.2='b';      x.3="c";      x.4='d'
y.=' ';      y.1="A";      y.2='B';      y.3="C";
z.=' ';      z.1= 1 ;      z.2= 2 ;      z.3= 3 ;      z.4= 4;      z.5= 5

       do j=1  until output=''
       output=x.j || y.j || z.j
       say output
       end   /*j*/                     /*stick a fork in it, we're done.*/
```

'''output'''

```txt

aA1
bB2
cC3
d 4
  5

```



### dissimilar sized lists


```rexx
/*REXX program shows how to  simultaneously  loop over  multiple  lists.*/
x = 'a b c d'
y = 'A B C'
z =  1 2 3 4
               do j=1  until  output=''
               output = word(x,j) || word(y,j) || word(z,j)
               say output
               end    /*j*/            /*stick a fork in it, we're done.*/
```

'''output'''

```txt

aA1
bB2
cC3
d4

```



### idiomatic method for lists


```rexx
/*REXX program shows how to  simultaneously  loop over  multiple  lists.*/
x = 'a b c d'
y = 'A B C'
z =  1 2 3 4 ..LAST
                     do j=1  for max(words(x), words(y), words(z))
                     say word(x,j) || word(y,j) || word(z,j)
                     end    /*j*/      /*stick a fork in it, we're done.*/
```

'''output'''

```txt

aA1
bB2
cC3
d4
..LAST

```



## Ring


```ring

array1 = ["a", "b", "c"]
array2 = ["A", "B", "C"]
array3 = [1, 2, 3]

for n = 1 to 3
    see array1[n] + array2[n] + array3[n] + nl
next

```



## Ruby


```ruby
['a','b','c'].zip(['A','B','C'], [1,2,3]) {|i,j,k| puts "#{i}#{j}#{k}"}
```

or

```ruby
['a','b','c'].zip(['A','B','C'], [1,2,3]) {|a| puts a.join}
```


Both of these loops print <code>aA1</code>, <code>bB2</code>, <code>cC3</code>.

<code>Array#zip</code> iterates once for each element of the receiver.
If an argument array is longer, the excess elements are ignored.
If an argument array is shorter, the value <code>nil</code> is supplied.

```ruby
irb(main):001:0> ['a','b','c'].zip(['A','B'], [1,2,3,4]) {|a| puts a.join}
aA1
bB2
c3
=> nil
irb(main):002:0> ['a','b','c'].zip(['A','B'], [1,2,3,4])
=> [["a", "A", 1], ["b", "B", 2], ["c", nil, 3]]
```



## Run BASIC


```runbasic
for i = 1 to 3
 a$(i) = chr$(i+96)
 b$(i) = chr$(i+64)
 c(i)  = i
next i

for i = 1 to 3
    print a$(i);b$(i);c(i)
next
```



## Rust



```rust
fn main() {
    let a1 = ["a", "b", "c"];
    let a2 = ["A", "B", "C"];
    let a3 = [1, 2, 3];

    for ((&x, &y), &z) in a1.iter().zip(a2.iter()).zip(a3.iter()) {
        println!("{}{}{}", x, y, z);
    }
}
```

{{out}}

```txt
aA1
bB2
cC3
```



## Salmon


```Salmon
// First, we'll define a general-purpose zip() to zip
 any
// number of lists together.
function zip(...)
  {
    variable result;
    variable list_num := 0;
    iterate(arg; arguments)
      {
        variable elem_num := 0;
        iterate (x; arg)
          {
            result[elem_num][list_num] := x;
            ++elem_num;
          };
        ++list_num;
      };
    return result;
  };

immutable a := ["a", "b", "c"],
          b := ["A", "B", "C"],
          c := [1, 2, 3];
iterate (x; zip(a, b, c))
    print(x[0], x[1], x[2], "\n");;
```


The preceding code will throw an exception if the lists aren't the same
length.
Here's an example that will print a number of lines equal to the length
of the longest list and print nothing for elements that are missing if
some lists are shorter than the longest:


```Salmon
// First, we'll define a general-purpose zip() to zip
 any
// number of lists together.
function zip(...)
  {
    variable result := [];
    variable list_num := 0;
    iterate(arg; arguments)
      {
        variable elem_num := 0;
        iterate (x; arg)
          {
            if (elem_num >= length(result))
                result[elem_num] := <<(* --> "")>>;;
            result[elem_num][list_num] := x;
            ++elem_num;
          };
        ++list_num;
      };
    return result;
  };

immutable a := ["a", "b", "c"],
          b := ["A", "B", "C"],
          c := [1, 2, 3];
iterate (x; zip(a, b, c))
    print(x[0], x[1], x[2], "\n");;
```



## Sather


```sather
class MAIN is
  main is
    a :ARRAY{STR} := |"a", "b", "c"|;
    b :ARRAY{STR} := |"A", "B", "C"|;
    c :ARRAY{STR} := |"1", "2", "3"|;
    loop i ::= 0.upto!(2);
      #OUT + a[i] + b[i] + c[i] + "\n";
    end;
  end;
end;
```


If the index ''i'' is out of bounds, a runtime error is raised.


## Scala


```scala

("abc", "ABC", "123").zipped foreach { (x, y, z) =>
  println(x.toString + y + z)
}

```



## Scheme


Scheme provides <code>for-each</code> and
<code>map</code> to iterate a function over one or more
lists.
The <code>map</code> form is used to collect the results
into a new list.


```scheme

(let ((a '("a" "b" "c"))
      (b '("A" "B" "C"))
      (c '(1 2 3)))
  (for-each
    (lambda (i1 i2 i3)
      (display i1)
      (display i2)
      (display i3)
      (newline))
    a b c))

```


Scheme has a <code>vector</code> datatype with constant-time
 retrieval of items held in an ordered sequence.  Use srfi-43 to get
similar iterators for vectors, <code>vector-for-each</code>
and <code>vector-map</code>:


```scheme

(let ((a (vector "a" "b" "c"))
      (b (vector "A" "B" "C"))
      (c (vector 1 2 3)))
  (vector-for-each
    (lambda (current-index i1 i2 i3)
      (display i1)
      (display i2)
      (display i3)
      (newline))
    a b c))

```


Note, the lists or vectors must all be of the same length.


## Sidef

The simplest way is by using the Array.zip{} method:

```ruby
[%w(a b c),%w(A B C),%w(1 2 3)].zip { |i,j,k|
    say (i, j, k)
}
```

{{out}}

```txt

aA1
bB2
cC3

```



## Smalltalk

{{works with|GNU Smalltalk}}


```smalltalk
|a b c|
a := OrderedCollection new addAll: #('a' 'b' 'c').
b := OrderedCollection new addAll: #('A' 'B' 'C').
c := OrderedCollection new addAll: #(1 2 3).

1 to: (a size) do: [ :i |
  (a at: i) display.
  (b at: i) display.
  (c at: i) displayNl.
].
```


If index ''i'' is out of bound, a runtime error is raised.


## Standard ML

The below code will combine arbitrarily many lists of strings
into a single list with length equal to that of the shortest list.

```Standard ML

(*
 * val combine_lists : string list list -> string list
 *)
fun combine_lists nil = nil
|   combine_lists (l1::ls) = List.foldl (ListPair.map (fn (x,y) => y ^
 x)) l1 ls;

(* ["a1Ax","b2By","c3Cz"] *)
combine_lists[["a","b","c"],["1","2","3"],["A","B","C"],["x","y","z"]];

```



## Stata

Use an index variable.


```stata
local u a b c
local v A B C
matrix w=1,2,3
forv i=1/3 {
	di "`: word `i' of `u''`: word `i' of `v''`=el("w",1,`i')'"
}
```



###  Mata



```stata
mata
u="a","b","c"
v="A","B","C"
w=1,2,3

for (i=1; i<=3; i++) {
        printf("%s%s%f\n",u[i],v[i],w[i])
}
end
```



## SuperCollider


Using three variables and indexing (SuperCollider posts the last statement in the REPL)

```SuperCollider

#x, y, z = [["a", "b", "c"], ["A", "B", "C"], ["1", "2", "3"]];
3.collect { |i| x[i] ++ y[i] ++ z[i] }

```


A more idiomatic way of writing it, independent of the number of dimensions:

```SuperCollider

[["a", "b", "c"], ["A", "B", "C"], ["1", "2", "3"]].flop.collect { |x| x.join }

```


Or simpler:

```SuperCollider

[["a", "b", "c"], ["A", "B", "C"], ["1", "2", "3"]].flop.collect(_.join)

```



Same with lamination (a concept from APL/[http://rosettacode.org/wiki/Category:J#The_J_language J]):

```SuperCollider

["a", "b", "c"] +++ ["A", "B", "C"] +++ ["1", "2", "3"]

```


Independent of dimensions:

```SuperCollider

[["a", "b", "c"], ["A", "B", "C"], ["1", "2", "3"]].reduce('+++')

```



## Swift


```Swift
let a1 = ["a", "b", "c"]
let a2 = ["A", "B", "C"]
let a3 = [1, 2, 3]

for i in 0 ..< a1.count {
    println("\(a1[i])\(a2[i])\(a3[i])")
}
```

{{out}}

```txt
aA1
bB2
cC3
```



## Tailspin

Simplest iteration with an ordinary "loop" that will error on uneven sizes

```tailspin

def x: ['a', 'b', 'c'];
def y: ['A', 'B', 'C'];
def z: [1, 2, 3];

1..$x::length -> '$x($);$y($);$z($);
' -> !OUT::write

```

{{out}}

```txt

aA1
bB2
cC3

```

A simple transpose method that gives the same output and also errors on uneven sizes

```tailspin

templates transpose
  def a: $;
  def n: $(1)::length;
  [ 1..$n -> $a(1..-1; $) ] !
end transpose

[$x, $y, $z] -> transpose... -> '$...;
' -> !OUT::write

```

A more complex transpose that uses "foreach" more in line with the task proposal and handles uneven arrays

```tailspin

def u: ['a', 'b'];
def v: ['A', 'B', 'C'];
def w: [1];

templates transpose2
  @: [];
  $... -> [i](
    <?($i <..$@transpose2::length>)> ..|@transpose2($i): $;
    <> ..|@transpose2: [$];) -> !VOID
  $@ !
end transpose2

[$x, $y, $z] -> transpose2... -> '$...;
' -> !OUT::write

'
' -> !OUT::write

[$u,$v,$w] -> transpose2... -> '$...;
' -> !OUT::write

```

{{out}}

```txt

aA1
bB2
cC3

aA1
bB
C

```



## Tcl


```tcl
set list1 {a b c}
set list2 {A B C}
set list3 {1 2 3}
foreach i $list1 j $list2 k $list3 {
    puts "$i$j$k"
}
```

If lists are different lengths, the manual
[http://www.tcl.tk/man/tcl8.5/TclCmd/foreach.htm] says:
"The total number of loop iterations is large enough to use up all the
values from all the value lists.
If a value list does not contain enough elements for each of its loop
variables in each iteration, empty values are used for the missing
elements."


## TorqueScript



```Torquescript

$var[0] = "a b c"
$var[1] = "A B C";
$var[2] = "1 2 3";

for(%i=0;%i<3;%i++)
	echo(getWord($var[0],%i) @ getWord($var[1],%i) @ getWord($var[2],%i));

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
arr1="a'b'c"
arr2="a'b'C"
arr3="1'2'3"
LOOP a=arr1,b=arr2,c=arr3
PRINT a,b,c
ENDLOOP

```

{{out}}

```txt

aa1
bb2
cC3

```



## TXR



### Pattern language



```bash
$ txr -c '@(bind a ("a" "b" "c"))
@(bind b ("A" "B" "C"))
@(bind c ("1" "2" "3"))
@(output)
@  (repeat)
@a@b@c
@  (end)
@(end)'
aA1
bB2
cC3
```


===TXR Lisp, using <code>mapcar</code>===

Here we actually loop over four things: three strings and an infinite
list of newlines. The output is built up as one string object that is
finally printed in one go.


```bash
$ txr -e '(pprint (mappend (op list) "abc" "ABC" "123"
(repeat "\n"))))'
aA1
bB2
cC3
```


===TXR Lisp, using <code>each</code>===


```bash
$ txr -e '(each ((x "abc") (y "ABC") (z "123"))
(put-line `@x@y@z`))'
aA1
bB2
cC3
```



### Translation of Scheme


{{trans|Scheme}}


```txrlisp
;; Scheme's vector-for-each: a one-liner in TXR
;; that happily works over strings and lists.
;; We don't need "srfi-43".
(defun vector-for-each (fun . vecs)
  [apply mapcar fun (range) vecs])

(defun display (obj : (stream *stdout*))
  (pprint obj stream))

(defun newline (: (stream *stdout*))
  (display #\newline stream))

(let ((a (vec "a" "b" "c"))
      (b (vec "A" "B" "C"))
      (c (vec 1 2 3)))
  (vector-for-each
    (lambda (current-index i1 i2 i3)
      (display i1)
      (display i2)
      (display i3)
      (newline))
    a b c))
```



### Translation of Logo


{{trans|Logo}}


```txrlisp
(macro-time
  (defun question-var-to-meta-num (var)
    ^(sys:var ,(int-str (cdr (symbol-name var))))))

(defmacro map (square-fun . square-args)
  (tree-bind [(fun . args)] square-fun
    ^[apply mapcar (op ,fun ,*[mapcar question-var-to-meta-num args])
            (macrolet ([(. args) ^(quote ,args)])
               (list ,*square-args))]))

(defun word (. items)
  [apply format nil "~a~a~a" items])

(defun show (x) (pprinl x))

(show (map [(word ?1 ?2 ?3)] [a b c] [A B C] [1 2 3]))
```


{{out}}

```txt
(aA1 bB2 cC3)
```



## UNIX Shell

With the Bourne shell, its <code>for</code> loop (from
[[Loops/Foreach#UNIX Shell]]) can iterate only one list.
We use an index <code>i</code> to access the other lists:
<code>set -- $list</code> loads the positional parameters,
and <code>shift $i</code> moves our element to
<code>$1</code>.

{{works with|Bourne Shell}}


```bash
a=a:b:c
b=A:B:C
c=1:2:3

oldifs=$IFS
IFS=:
i=0
for wa in $a; do
	set -- $b; shift $i; wb=$1
	set -- $c; shift $i; wc=$1

	printf '%s%s%s\n' $wa $wb $wc

	i=`expr $i + 1`
done
IFS=$oldifs
```


{{out}}

```txt

aA1
bB2
cC3

```


When the lists have different lengths, this code uses the length of list
 <code>a</code>. Longer lists ignore their extra elements,
and shorter lists give extra empty strings.



----
Inspired by the previous example, below is the way to
loop over two arrays simultaneously using <code>set --
$ARGS</code>.
It is less general than the previous example
but it is shorter and works just fine.

{{works with|Bourne Shell}}


```bash
A='a1 a2 a3'
B='b1 b2 b3'

set -- $B
for a in $A
do
    printf "$a $1\n"
    shift
done
```


{{out}}

```txt

a1 b1
a2 b2
a3 b3

```



----
Some shells have real arrays, so the iteration is much more simple and
easy.

{{works with|bash}}
{{works with|ksh93}}


```bash
a=(a b c)
b=(A B C)
c=(1 2 3)
for ((i = 0; i < ${#a[@]}; i++)); do
  echo "${a[$i]}${b[$i]}${c[$i]}"
done
```


{{out}}

```txt

aA1
bB2
cC3

```


{{works with|ksh93}}
{{works with|pdksh}}


```bash
set -A a a b c
set -A b A B C
set -A c 1 2 3
((i = 0))
while ((i < ${#a[@]})); do
  echo "${a[$i]}${b[$i]}${c[$i]}"
  ((i++))
done
```


{{works with|zsh}}


```bash
a=(a b c)
b=(A B C)
c=(1 2 3)
for ((i = 1; i <= $#a; i++)); do
  echo "$a[$i]$b[$i]$c[$i]"
done
```


=
## C Shell
=
Uses the length of array ''a''.
Longer arrays ignore their extra elements, but shorter arrays force the
shell to exit with an error like ''b: Subscript out of range.''


```csh
set a=(a b c)
set b=(A B C)
set c=(1 2 3)
@ i = 1
while ( $i <= $#a )
	echo "$a[$i]$b[$i]$c[$i]"
	@ i += 1
end
```



## Ursa

Looping over multiple arrays in an interactive session:

```ursa>> decl string<
 a b c
> append (split "abc" "") a
> append (split "ABC" "") b
> append (split "123" "") c
> for (decl int i) (< i (size a)) (inc i)
..	out a<i> b<i> c<i> endl console
..end
aA1
bB2
cC3
> _
```

If either of the arrays are smaller than (size a), then an indexerror is thrown. This could be caught with a <code>try...catch</code> block.


## Ursala

Compute the transpose of the list formed of the three lists.
If they're of unequal lengths, an exception occurs.

```Ursala
#show+

main = ~&K7 <'abc','ABC','123'>
```

{{out}}

```txt

aA1
bB2
cC3

```



## Visual FoxPro


```vfp

LOCAL i As Integer, n As Integer, c As String
LOCAL ARRAY a1[3], a2[3], a3[4], a[3]
*!* Populate the arrays and store the array lengths in a
a1[1] = "a"
a1[2] = "b"
a1[3] = "c"
a[1] = ALEN(a1)
a2[1] = "A"
a2[2] = "B"
a2[3] = "C"
a[2] = ALEN(a2)
a3[1] = "1"
a3[2] = "2"
a3[3] = "3"
a3[4] = "4"
a[3] = ALEN(a3)
*!* Find the maximum length of the arrays
*!* In this case, 4
n = MAX(a[1], a[2], a[3])
? "Simple Loop"
FOR i = 1 TO n
    c = ""
    c = c + IIF(i <= a[1], a1[i], "#")
    c = c + IIF(i <= a[2], a2[i], "#")
    c = c + IIF(i <= a[3], a3[i], "#")
   ? c
ENDFOR
*!* Solution using a cursor
CREATE CURSOR tmp (c1 C(1), c2 C(1), c3 C(1), c4 C(3))
INSERT INTO tmp (c1, c2, c3) VALUES ("a", "A", "1")
INSERT INTO tmp (c1, c2, c3) VALUES ("b", "B", "2")
INSERT INTO tmp (c1, c2, c3) VALUES ("c", "C", "3")
INSERT INTO tmp (c1, c2, c3) VALUES ("#", "#", "4")
REPLACE c4 WITH c1 + c2 + c3 ALL
? "Solution using a cursor"
LIST OFF FIELDS c4

```

{{out}}

```txt

Simple Loop
aA1
bB2
cC3
##4

Solution using a cursor

aA1
bB2
cC3
##4

```



## Visual Basic .NET

Two implementations: one determines the shortest of the arrays and uses a simple For loop with element accesses to each array separately; one uses Enumerable.Zip (which can only zip two sequences at once) twice to create 3-tuples. Enumerable.Zip stops when either source runs out of elements, so the behavior of the two implementations is identical for arrays of different lengths.

```vbnet

Module Program
    Sub Main()
        Dim a As Char() = {"a"c, "b"c, "c"c}
        Dim b As Char() = {"A"c, "B"c, "C"c}
        Dim c As Integer() = {1, 2, 3}

        Dim minLength = {a.Length, b.Length, c.Length}.Min()
        For i = 0 To minLength - 1
            Console.WriteLine(a(i) & b(i) & c(i))
        Next

        Console.WriteLine()

        For Each el As (a As Char, b As Char, c As Integer) In a.Zip(b, Function(l, r) (l, r)).Zip(c, Function(x, r) (x.l, x.r, r))
            Console.WriteLine(el.a & el.b & el.c)
        Next
    End Sub
End Module
```


{{out}}

```txt
aA1
bB2
cC3

aA1
bB2
cC3
```



## Wart


```wart
each (x X n) (zip '(a b c) '(A B C) '(1 2 3))
  prn x X n
```



## XPL0


```XPL0
string 0;               \use zero terminated strings
include c:\cxpl\codes;  \intrinsic 'code' declarations
char A1, A2;
int A3, I;
[A1:= "abc";
 A2:= "ABC";
 A3:= [1,2,3];
 for I:= 0 to 2 do
        [ChOut(0, A1(I));
         ChOut(0, A2(I));
        IntOut(0, A3(I));
        CrLf(0);
        ];
]
```


{{out}}

```txt

aA1
bB2
cC3

```



## zkl


```zkl
foreach a,b,c in (["a".."c"].zip(T("A","B","C"),[1..])){ println(a,b,c) }
```

or

```zkl
Utils.zipWith(False,fcn{vm.arglist.concat().println()},
   ["a".."c"],T("A","B","C"),[1..])
```

{{out}}

```txt

aA1
bB2
cC3

```

zip[With] stops at the end of the shortest sequence,
which means it can work with infinite sequences


## ZX Spectrum Basic



```zxbasic
10 LET sza = 3: REM size of a
20 LET szb = 3: REM size of b
30 LET szc = 3: REM size of c
40 DIM a$(sza): DIM b$(szb): DIM c$(szc)
50 LET max = sza: REM assume a is the biggest
60 IF szb > max THEN LET max = szb: REM now try b
70 IF szc > max THEN LET max = szc: REM or c
80 REM populate our arrays, and as a bonus we already have our demo loop
90 REM we might as well print as we populate showing the arrays in
columns
100 FOR l = 1 TO max
110 IF l <= sza THEN READ a$(l): PRINT a$(l);
120 IF l <= szb THEN READ b$(l): PRINT b$(l);
130 IF l <= szc THEN READ c$(l): PRINT c$(l);
140 PRINT: REM newline
150 NEXT l
150 PRINT "The arrays are shown in columns."
160 PRINT "A$ runs down the left hand side,"
170 PRINT "and C$ runs down the right."
180 STOP
200 DATA "a","b","c","A","B","C","1","2","3"
```


Simplification


```zxbasic
10 READ size: DIM a$(size): DIM b$(size): DIM c$(size)
20 FOR i=1 TO size
30 READ a$(i),b$(i),c$(i)
40 PRINT a$(i);b$(i);c$(i)
50 NEXT i
60 DATA 3,"a","A","1","b","B","2","c","C","3"
```

