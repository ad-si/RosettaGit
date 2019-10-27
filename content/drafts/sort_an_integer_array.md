+++
title = "Sort an integer array"
description = ""
date = 2019-10-18T19:37:44Z
aliases = []
[extra]
id = 1846
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting}}
Sort an array (or list) of integers in ascending numerical order. 

;Task:
Use a sorting facility provided by the language/library if possible.





## 4D


### English



```4d
ARRAY INTEGER($nums;0)
APPEND TO ARRAY($nums;2)
APPEND TO ARRAY($nums;4)
APPEND TO ARRAY($nums;3)
APPEND TO ARRAY($nums;1)
APPEND TO ARRAY($nums;2)
SORT ARRAY($nums)  ` sort in ascending order
SORT ARRAY($nums;<)  ` sort in descending order
```


===Français===


```4d
TABLEAU ENTIER($nombres;0)
AJOUTER A TABLEAU($nombres;2)
AJOUTER A TABLEAU($nombres;4)
AJOUTER A TABLEAU($nombres;3)
AJOUTER A TABLEAU($nombres;1)
AJOUTER A TABLEAU($nombres;2)
TRIER TABLEAU($nombres)  ` pour effectuer un tri par ordre croissant
TRIER TABLEAU($nombres;<)  ` pour effectuer un tri par ordre décroissant
```


## 8th


```forth

[ 10,2,100 ] ' n:cmp a:sort . cr

```

Output is:  [2,10,100]


## ActionScript


```ActionScript
//Comparison function must returns Numbers even though it deals with integers.
function compare(x:int, y:int):Number
{
	return Number(x-y);
}
var nums:Vector.<int> = Vector.<int>([5,12,3,612,31,523,1,234,2]);
nums.sort(compare);
```



## Ada

{{works with|GNAT|GPL 2006}}

```ada
with Gnat.Heap_Sort_G;
 
procedure Integer_Sort is
   -- Heap sort package requires data to be in index values starting at
   -- 1 while index value 0 is used as temporary storage
   type Int_Array is array(Natural range <>) of Integer;
   Values : Int_Array := (0,1,8,2,7,3,6,4,5);
   
   -- define move and less than subprograms for use by the heap sort package
   procedure Move_Int(From : Natural; To : Natural) is
   begin
      Values(To) := Values(From);
   end Move_Int;
   
   function Lt_Int(Left, Right : Natural) return Boolean is
   begin
      return Values(Left) < Values (Right);
   end Lt_Int;
  
   -- Instantiate the generic heap sort package
   package Heap_Sort is new Gnat.Heap_Sort_G(Move_Int, Lt_Int);

begin
   Heap_Sort.Sort(8);
end Integer_Sort;

requires an Ada05 compiler, e.g GNAT GPL 2007
with Ada.Containers.Generic_Array_Sort;
 
procedure Integer_Sort is
   -- 
   type Int_Array is array(Natural range <>) of Integer;
   Values : Int_Array := (0,1,8,2,7,3,6,4,5);
   
   -- Instantiate the generic sort package from the standard Ada library
   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Natural,
      Element_Type => Integer,
      Array_Type   => Int_Array);

begin
   Sort(Values);
end Integer_Sort;
```



## ALGOL 68

{{trans|python}}

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
CO PR READ "shell_sort.a68" PR CO
MODE TYPE = INT;

PROC in place shell sort = (REF[]TYPE seq)REF[]TYPE:(
    INT inc := ( UPB seq + LWB seq + 1 ) OVER 2;
    WHILE inc NE 0 DO
        FOR index FROM LWB seq TO UPB seq DO
            INT i := index;
            TYPE el = seq[i];
            WHILE ( i  - LWB seq >= inc | seq[i - inc] > el | FALSE ) DO
                seq[i] := seq[i - inc];
                i -:= inc
            OD;
            seq[i] := el
        OD;
        inc := IF inc = 2 THEN 1 ELSE ENTIER(inc * 5 / 11) FI
    OD;  
    seq  
);    
   
PROC shell sort = ([]TYPE seq)[]TYPE:
  in place shell sort(LOC[LWB seq: UPB seq]TYPE:=seq);

print((shell sort((2, 4, 3, 1, 2)), new line))
```

Output:

```txt

         +1         +2         +2         +3         +4

```



## ALGOL W

Algol W doesn't have standard sorting facilities. This uses the Algol W quicksort sample in the Sorting Algorithms Quicksort task.

```algolw
begin
    % use the quicksort procedure from the Sorting_Algorithms/Quicksort task %
    % Quicksorts in-place the array of integers v, from lb to ub - external  %
    procedure quicksort ( integer array v( * )
                        ; integer value lb, ub
                        ) ; algol "sortingAlgorithms_Quicksort" ;
    % sort an integer array with the quicksort routine                       %
    begin
        integer array t ( 1 :: 5 );
        integer p;
        p := 1;
        for v := 2, 3, 1, 9, -2 do begin t( p ) := v; p := p + 1; end;
        quicksort( t, 1, 5 );
        for i := 1 until 5 do writeon( i_w := 1, s_w := 1, t( i ) )
    end
end.
```

{{out}}

```txt

-2 1 2 3 9

```



## APL

{{works with|APL2}}

```apl
      X←63 92 51 92 39 15 43 89 36 69
      X[⍋X]
15 36 39 43 51 63 69 89 92 92
```




## AppleScript


AppleScript has no native sort function.

Later versions of AppleScript (OS X 10.10 onwards) do allow access to the ObjC NSArray library, 
but while this approach can yield reasonably fast sorts, it is slow in terms of scripter time,
requiring digestion of the ObjC library documentation, and leading to code like the '''sort''' function 
below, which is possibly more messy than it is worth for the purposes of casual end-user scripting,
for which AppleScript was presumably designed.


```AppleScript
use framework "Foundation"

-- sort :: [a] -> [a]
on sort(lst)
    ((current application's NSArray's arrayWithArray:lst)'s ¬
        sortedArrayUsingSelector:"compare:") as list
end sort

-- TEST -----------------------------------------------------------------------
on run
    
    map(sort, [[9, 1, 8, 2, 8, 3, 7, 0, 4, 6, 5], ¬
        ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", ¬
            "theta", "iota", "kappa", "lambda", "mu"]])
    
end run


-- GENERIC FUNCTIONS  ---------------------------------------------------------

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
{{0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 9}, 
{"alpha", "beta", "delta", "epsilon", "eta", "gamma", 
"iota", "kappa", "lambda", "mu", "theta", "zeta"}}
```



## AutoHotkey

    

```AutoHotkey
numbers = 5 4 1 2 3
sort, numbers, N D%A_Space%
Msgbox % numbers
```



## AWK


```AWK

# syntax: GAWK -f SORT_AN_INTEGER_ARRAY.AWK
BEGIN {
    split("9,10,3,1234,99,1,200,2,0,-2",arr,",")
    show("@unsorted","unsorted")
    show("@val_num_asc","sorted ascending")
    show("@val_num_desc","sorted descending")
    exit(0)
}
function show(sequence,description,  i) {
    PROCINFO["sorted_in"] = sequence
    for (i in arr) {
      printf("%s ",arr[i])
    }
    printf("\t%s\n",description)
}

```

<p>output:</p>

```txt

9 10 3 1234 99 1 200 2 0 -2     unsorted
-2 0 1 2 3 9 10 99 200 1234     sorted ascending
1234 200 99 10 9 3 2 1 0 -2     sorted descending

```



## Axe

There is no ascending sort function in Axe, but there is a descending sort function. One can either implement a custom ascending sorting function or simply reverse the output from SortD.


```axe
2→{L₁}
4→{L₁+1}
3→{L₁+2}
1→{L₁+3}
2→{L₁+4}

SortD(L₁,5)
```



## Babel


Use the sortval operator to sort an array of integers (val-array in Babel terminology). The following code creates a list of random values, converts it to a val-array, sorts that val-array, then converts it back to a list for display using the lsnum utility.


```babel>babel
 nil { zap {1 randlf 100 rem} 20 times collect ! } nest dup lsnum ! --> Create a list of random numbers
( 20 47 69 71 18 10 92 9 56 68 71 92 45 92 12 7 59 55 54 24 )
babel> ls2lf                                                              --> Convert list to array for sorting
babel> dup {fnord} merge_sort                                             --> The internal sort operator
babel> ar2ls lsnum !                                                      --> Display the results
( 7 9 10 12 18 20 24 45 47 54 55 56 59 68 69 71 71 92 92 92 )
```


In Babel, lists and arrays are distinct. If you want to sort a list, use the lssort utility:


```babel>babel
 ( 68 73 63 83 54 67 46 53 88 86 49 75 89 83 28 9 34 21 20 90 )
babel> {lt?} lssort ! lsnum !
( 9 20 21 28 34 46 49 53 54 63 67 68 73 75 83 83 86 88 89 90 )
```


To reverse the sort-order, use the 'gt?' predicate instead of the 'lt?' predicate:


```babel>babel
 ( 68 73 63 83 54 67 46 53 88 86 49 75 89 83 28 9 34 21 20 90 ) {gt?} lssort ! lsnum !
( 90 89 88 86 83 83 75 73 68 67 63 54 53 49 46 34 28 21 20 9 )
```



## BaCon


```freebasic
' Sort an integer array
DECLARE values[5] TYPE NUMBER
values[0] = 23
values[1] = 32
values[2] = 12
values[3] = 21
values[4] = 01

SORT values

FOR i = 0 TO 3
    PRINT values[i], ", ";
NEXT
PRINT values[4]
```


{{out}}

```txt
prompt$ ./sort-integer
1, 12, 21, 23, 32
```

Use SORT array DOWN for descending sort order.


## BBC BASIC

{{works with|BBC BASIC for Windows}}
Uses the supplied SORTLIB library.

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      sort% = FN_sortinit(0,0)
      
      DIM array(8)
      array() = 8, 2, 5, 9, 1, 3, 6, 7, 4
      
      C% = DIM(array(),1) + 1
      CALL sort%, array(0)
      
      FOR i% = 0 TO DIM(array(),1) - 1
        PRINT ; array(i%) ", ";
      NEXT
      PRINT ; array(i%)
```

Output:

```txt

1, 2, 3, 4, 5, 6, 7, 8, 9

```



## Befunge

{{works with|befungee}}
Elements of the array are read from standard input, preceded by their quantity. The algorithm uses counting sort and allows numbers between 1 and 60, inclusive.

```Befunge
v 
> 543** >     :#v_ $&>           :#v_ 1 > :0g >    :#v_ $ 1+: 543** `! #v_ 25*,@
        ^-1p0\0:<    ^-1 p0\+1 g0:&<          ^-1\.:\<
                                        ^                               <
```



## Bracmat

As a Computer Algebra system, Bracmat transforms expressions to a canonical form.
Terms in a sum are sorted and, where possible, added together.
So the task is partially solved by expressing the list as a sum of terms.
Evaluating the list sorts the list, but also adds like terms.
To illustrate, this is what happens when entering our list at the prompt:

```txt
{?} (9.)+(-2.)+(1.)+(2.)+(8.)+(0.)+(1.)+(2.)
{!} (-2.)+(0.)+2*(1.)+2*(2.)+(8.)+(9.)
```

The use of a computationally inert operator like the dot <code>.</code> is essential:

```txt
{?} (9)+(-2)+(1)+(2)+(8)+(0)+(1)+(2)
{!} 21
```

To complete the task need to unfold the terms with a numerical factor >1:

```bracmat
{sort takes a list of space-separated integers}
(sort=
  sum elem sorted n
.   0:?sum
  &   whl
    ' (!arg:%?elem ?arg&(!elem.)+!sum:?sum)
  & :?sorted
  &   whl
    ' ( !sum:?n*(?elem.)+?sum
      &   whl
        ' ( !n+-1:~<0:?n
          & !sorted !elem:?sorted
          )
      )
  & !sorted);
  
  out$sort$(9 -2 1 2 8 0 1 2);
```

Output:

```txt
-2 0 1 1 2 2 8 9
```

This solution becomes very ineffective for long lists. To add a single term to an already sorted sum of N terms requires on average N/2 steps. It is much more efficient to merge two already sorted sums of about equal length.
Also, adding elements to the end of the list 'sorted' is costly. Better is to prepend elements to a list, which will have inverted sorting order, and to invert this list in an extra loop.


## Burlesque


```burlesque
{1 3 2 5 4}><
```



## C


```c>#include <stdlib.h
  /* qsort() */
#include <stdio.h>   /* printf() */

int intcmp(const void *aa, const void *bb)
{
    const int *a = aa, *b = bb;
    return (*a < *b) ? -1 : (*a > *b);
}

int main()
{
    int nums[5] = {2,4,3,1,2};
    qsort(nums, 5, sizeof(int), intcmp);
    printf("result: %d %d %d %d %d\n",
      nums[0], nums[1], nums[2], nums[3], nums[4]);
    return 0;
}
```


''Caution:'' An older version of <tt>intcmp()</tt> did <tt>return *a - *b</tt>. This is only correct when the subtraction does not overflow. Suppose that <tt>*a = 2000000000</tt> and <tt>*b = -2000000000</tt> on a machine with 32-bit <tt>int</tt>. The subtraction <tt>*a - *b</tt> would overflow to <tt>-294967296</tt>, and <tt>intcmp()</tt> would believe <tt>*a < *b</tt>, but the correct answer is <tt>*a > *b</tt>.


## C++

{{works with|g++|4.0.1}}


### Simple Array


```cpp>#include <algorithm


int main()
{
    int nums[] = {2,4,3,1,2};
    std::sort(nums, nums+sizeof(nums)/sizeof(int));
    return 0;
}
```



### <tt>std::vector</tt>


```cpp>#include <algorithm

#include <vector>

int main()
{
    std::vector<int> nums;
    nums.push_back(2);
    nums.push_back(4);
    nums.push_back(3);
    nums.push_back(1);
    nums.push_back(2);
    std::sort(nums.begin(), nums.end());
    return 0;
}
```



### <tt>std::list</tt>


```cpp>#include <list


int main()
{
    std::list<int> nums;
    nums.push_back(2);
    nums.push_back(4);
    nums.push_back(3);
    nums.push_back(1);
    nums.push_back(2);
    nums.sort();
    return 0;
}
```


=={{header|C sharp|C#}}==


```csharp
using System;
using System.Collections.Generic;

public class Program {
    static void Main() {
        int[] unsorted = { 6, 2, 7, 8, 3, 1, 10, 5, 4, 9 };
        Array.Sort(unsorted);
    }
}
```



## Clean

We use list and array comprehensions to convert an array to and from a list in order to use the built-in <tt>sort</tt> on lists.

```clean
import StdEnv

sortArray :: (a e) -> a e | Array a e & Ord e
sortArray array = {y \\ y <- sort [x \\ x <-: array]}

Start :: {#Int}
Start = sortArray {2, 4, 3, 1, 2}
```



## Clojure


```clojure
(sort [5 4 3 2 1]) ; sort can also take a comparator function
(1 2 3 4 5)
```



## COBOL

{{works with|Visual COBOL}}

```cobol
       PROGRAM-ID. sort-ints.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  array-area             VALUE "54321".
           03  array              PIC 9 OCCURS 5 TIMES.
       01  i                      PIC 9.
       
       PROCEDURE DIVISION.
       main-line.
           PERFORM display-array
           SORT array ASCENDING array
           PERFORM display-array
       
           GOBACK
           .
       display-array.
           PERFORM VARYING i FROM 1 BY 1 UNTIL 5 < i
               DISPLAY array (i) " " NO ADVANCING
           END-PERFORM
           DISPLAY SPACE
           .
```



## Common Lisp

In Common Lisp, the ''sort'' function takes a predicate that is used as the comparator. This parameter can be any two-argument function. To sort a sequence (list or array) of integers, call ''sort'' with the <  operator as the predicate:

```lisp
CL-USER> (sort #(9 -2 1 2 8 0 1 2) #'<)
#(-2 0 1 1 2 2 8 9)
```



## Crystal

Example demonstrating the support for copy sort and in-place sort (like Ruby)

```Ruby

a = [5, 4, 3, 2, 1]
puts a.sort
# => [1, 2, 3, 4, 5]

puts a
# => [5, 4, 3, 2, 1]

a.sort!
puts a
# => [1, 2, 3, 4, 5]

```



## D


```d
import std.stdio, std.algorithm;

void main() {
    auto data = [2, 4, 3, 1, 2];
    data.sort(); // in-place
    assert(data == [1, 2, 2, 3, 4]);
}
```



## Delphi


```Delphi
uses Types, Generics.Collections;

var
  a: TIntegerDynArray;
begin
  a := TIntegerDynArray.Create(5, 4, 3, 2, 1);
  TArray.Sort<Integer>(a);
end;
```

=={{header|Déjà Vu}}==

```dejavu
!. sort [ 5 4 3 2 1 ]
```

{{out}}

```txt
[ 1 2 3 4 5 ]
```



## DWScript


```Delphi
var a : array of Integer := [5, 4, 3, 2, 1];
a.Sort; // ascending natural sort
PrintLn(a.Map(IntToStr).Join(','));  // 1,2,3,4,5
```



## E


```e
[2,4,3,1,2].sort()
```


## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;
 
public program()
{
    var unsorted := new int[]::(6, 2, 7, 8, 3, 1, 10, 5, 4, 9);
 
    console.printLine(unsorted.clone().sort(ifOrdered).asEnumerable())
}
```



## Elixir


```elixir
list = [2, 4, 3, 1, 2]
IO.inspect Enum.sort(list)
IO.inspect Enum.sort(list, &(&1>&2))
```


{{out}}

```txt

[1, 2, 2, 3, 4]
[4, 3, 2, 2, 1]

```



## Erlang


```erlang
List = [2, 4, 3, 1, 2].
SortedList = lists:sort(List).
```



## Euphoria


```euphoria
include sort.e
print(1,sort({20, 7, 65, 10, 3, 0, 8, -60}))
```



## EGL

{{works with|EDT}}
The following works in EDT with Rich UI and stand-alone programs.

```EGL
program SortExample

    function main()
        test1 int[] = [1,-1,8,-8,2,-2,7,-7,3,-3,6,-6,9,-9,4,-4,5,-5,0];
        test1.sort(sortFunction);

	for(i int from 1 to test1.getSize())
	    SysLib.writeStdout(test1[i]);
	end
    end
    
    function sortFunction(a any in, b any in) returns (int)
        return (a as int) - (b as int);
    end
	
end
```

{{works with|RBD}}
The following works in RBD but only with Rich UI programs.

```EGL
test1 int[] = [1,-1,8,-8,2,-2,7,-7,3,-3,6,-6,9,-9,4,-4,5,-5,0];
RUILib.sort(test1, sortFunction);

    
function sortFunction(a any in, b any in) returns (int)
    return ((a as int) - (b as int));
end
```



## Factor


```factor
{ 1 4 9 2 3 0 5 } natural-sort .
```



## Fantom


The List collection contains a sort method which uses the usual comparison method for the data in the list; the sort is done 'in place'.


```txt

fansh> a := [5, 1, 4, 2, 3]
[5, 1, 4, 2, 3]
fansh> a.sort
[1, 2, 3, 4, 5]
fansh> a
[1, 2, 3, 4, 5]

```



## Forth

{{works with|Win32Forth|4.2}}

### Win32Forth


```forth
create test-data 2 , 4 , 3 , 1 , 2 ,
test-data 5 cell-sort

```


### ANS/ISO Forth

{{works with|GForth}}
Uses quicksort http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Forth

Standard Forth does not have a library sort

```forth
100000 CONSTANT SIZE

CREATE MYARRAY   SIZE CELLS ALLOT

: []   ( n addr -- addr[n])  SWAP CELLS + ;

: FILLIT ( -- ) ( reversed order)
  SIZE 0  DO   SIZE I -   I MYARRAY [] !  LOOP ;

: SEEIT  ( -- ) 
  SIZE 0 DO  I MYARRAY [] ?   LOOP ;

\ define non-standard words used by Quicksort author
1 CELLS CONSTANT CELL
CELL NEGATE CONSTANT -CELL
: CELL-   CELL - ;

: MID ( l r -- mid ) OVER - 2/ -CELL AND + ;

: EXCH    ( addr1 addr2 -- ) 
  OVER @ OVER @        ( read values)
  SWAP ROT ! SWAP ! ;  ( exchange values)

: PARTITION ( l r -- l r r2 l2 )
  2DUP MID @ >R ( r: pivot )
  2DUP
  BEGIN
    SWAP BEGIN  DUP @  R@  < WHILE CELL+ REPEAT
    SWAP BEGIN  R@ OVER @  < WHILE CELL- REPEAT
    2DUP <= IF 2DUP EXCH  >R CELL+ R> CELL-  THEN
    2DUP >
  UNTIL
  R> DROP ;

: QSORT ( l r -- )
  PARTITION  SWAP ROT
  2DUP < IF RECURSE ELSE 2DROP THEN
  2DUP < IF RECURSE ELSE 2DROP THEN ;

: QUICKSORT ( array len -- )
  DUP 2 < IF 2DROP EXIT THEN  1- CELLS OVER + QSORT ;</LANG>
Test at the console

```forth
FILLIT ok
MYARRAY SIZE QUICKSORT ok
```



## Fortran

{{works with|Silverfrost FTN95}}

```fortran
CALL ISORT@(b, a, n)
! n = number of elements
! a = array to be sorted
! b = array of indices of a. b(1) 'points' to the minimum value etc.
```



## FreeBASIC

Qsort is not buildin, but include in the compiler package.

```freebasic
' version 11-03-2016
' compile with: fbc -s console

#Include Once "crt/stdlib.bi"      ' needed for qsort subroutine

' Declare Sub qsort (ByVal As Any Ptr, <== point to start of array
'                    ByVal As size_t,  <== size of array
'                    ByVal As size_t,  <== size of array element
' ByVal As Function(ByVal As Any Ptr, ByVal As Any Ptr) As Long)  <== callback function
' declare callback function with Cdecl to ensures that the parameters are passed in the correct order
'
' size of long: 4 bytes on 32bit OS, 8 bytes on 64bit OS

' ascending
 Function callback Cdecl (ByVal element1 As Any Ptr, ByVal element2 As Any Ptr) As Long
     Function = *Cast(Long Ptr, element1) - *Cast(Long Ptr, element2)
End Function

' Function callback Cdecl (ByVal element1 As Any Ptr, ByVal element2 As Any Ptr) As Long
' Dim As Long e1 = *Cast(Long Ptr, element1)
' Dim As Long e2 = *Cast(Long Ptr, element2)
' Dim As Long result = Sgn(e1 - e2)
' If Sgn(e1) = -1 And Sgn(e2) = -1 Then result = -result
'     Function = result
' End Function

' ------=< MAIN >=------

Dim As Long i, array(20)

Dim As Long lb = LBound(array)
Dim As Long ub = UBound(array)

For i = lb To ub     ' fill array
    array(i) = 10 - i
Next

Print
Print "unsorted array"
For i = lb To ub     ' display array
    Print Using "###";array(i);
Next
Print : Print

' sort array
qsort(@array(lb), ub - lb +1, SizeOf(array), @callback)

Print "sorted array"
For i = lb To ub     ' show sorted array
    Print Using "###";array(i);
Next
Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
unsorted array
 10  9  8  7  6  5  4  3  2  1  0 -1 -2 -3 -4 -5 -6 -7 -8 -9-10

sorted array
-10 -9 -8 -7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7  8  9 10
```



## Frink

The following sorts an array in-place.

```frink
a = [5, 2, 4, 1, 6, 7, 9, 3, 8, 0]
sort[a]
```


=={{header|F_Sharp|F#}}==

```fsharp
// sorting an array in place
let nums = [| 2; 4; 3; 1; 2 |]
Array.sortInPlace nums

// create a sorted copy of a list
let nums2 = [2; 4; 3; 1; 2]
let sorted = List.sort nums2
```



## FunL


```funl
nums = [5, 2, 78, 2, 578, -42]
println( sort(nums) )           // sort in ascending order
println( nums.sortWith((>)) )   // sort in descending order
```


{{out}}


```txt

[-42, 2, 2, 5, 78, 578]
[578, 78, 5, 2, 2, -42]

```



## GAP


```gap
a := [ 8, 2, 5, 9, 1, 3, 6, 7, 4 ];
# Make a copy (with "b := a;", b and a would point to the same list)
b := ShallowCopy(a);

# Sort in place
Sort(a);
a;
# [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

# Sort without changing the argument
SortedList(b);
# [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
b;
# [ 8, 2, 5, 9, 1, 3, 6, 7, 4 ]
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=1f1d244aa95c329eb87cb538f0d5fc4a Click this link to run this code]'''

```gambas
Public Sub Main()
Dim iArray As Integer[] = [8, 2, 5, 9, 1, 3, 6, 7, 4]
Dim iTemp As Integer
Dim sOutput As String

For Each iTemp In iArray.Sort()
  sOutput &= iTemp & ", "
Next

Print Left(sOutput, -2)

End
```


Output:

```txt

1, 2, 3, 4, 5, 6, 7, 8, 9

```



## Go


```go
package main
import "fmt"
import "sort"

func main() {
  nums := []int {2, 4, 3, 1, 2}
  sort.Ints(nums)
  fmt.Println(nums)
}
```



## Golfscript


```golfscript
[2 4 3 1 2]$
```



## Groovy


```groovy
println ([2,4,0,3,1,2,-12].sort())
```


Output: 

```txt
[-12, 0, 1, 2, 2, 3, 4]
```



## Haskell

{{works with|GHC|GHCi|6.6}}
 

```haskell
nums = [2,4,3,1,2] :: [Int]
sorted = List.sort nums
```



## HicEst


```hicest
DIMENSION array(100)

   array = INT( RAN(100) )
   SORT(Vector=array, Sorted=array) 
```



## Huginn


```huginn
main() {
  nums = [2, 4, 3, 1, 2];
  nums.sort();
}
```



## IDL


```idl
result = array[sort(array)]
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon lists allow mixed type and the built-in function 'sort' will deal with mixed type arrays by sorting by type first then value.  Integers sort before, reals, strings, lists, tables, etc. As a result a list of mixed numeric valuess (i.e. integers and reals) will not sort by numeric value, rather the reals will appear after the integers.  Sort returns a sorted copy of it's argument.  It will also perform some type conversion, such converting an unordered set into an ordered list.

In the example below, L will remain an unsorted list and S will be sorted.

```Icon
S := sort(L:= [63, 92, 51, 92, 39, 15, 43, 89, 36, 69])  # will sort a list
```



## Inform 7


```inform7
let L be {5, 4, 7, 1, 18};
sort L;
```



## Io


```lua
mums := list(2,4,3,1,2)
sorted := nums sort  # returns a new sorted array.  'nums' is unchanged
nums sortInPlace  # sort 'nums' "in-place"
```



## J


```j
/:~
```

The verb<tt> /:~ </tt>sorts <i>anything</i> that J can represent.  For example:


```j
   ] a=: 10 ?@$ 100    NB. random vector
63 92 51 92 39 15 43 89 36 69
   /:~ a
15 36 39 43 51 63 69 89 92 92
```

Arrays of any rank are treated as lists of component arrays. Thus <tt>/:~</tt> sorts not only atoms within a list, but whole lists within a table, tables within a three-axis array, and so on. The level of structure at which sorting occurs may also be specified, so that <tt>/:~"1</tt> sorts the atoms within the finest-grained list within the array, regardless of the overall rank of the array. See the [https://code.jsoftware.com/wiki/Essays/The_TAO_of_J Total Array Ordering essay] on the JWiki for more details.

This code also applies to any data type.


## Java


### Array


```java
import java.util.Arrays;

public class Example {
    public static void main(String[] args)
    {
        int[] nums = {2,4,3,1,2};
        Arrays.sort(nums);
    }
}
```



### List

{{works with|Java|1.5+}}

```java5
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class Example {
    public static void main(String[] args)
    {
        List<Integer> nums = Arrays.asList(2,4,3,1,2);
        Collections.sort(nums);
    }
}
```



## JavaScript

{{works with|Firefox|2.0}}

JavaScript sorts lexically by default, so "10000" comes before "2". To sort numerically, a custom comparator is used.


```javascript
function int_arr(a, b) {
  return a - b;
}
var numbers = [20, 7, 65, 10, 3, 0, 8, -60];
numbers.sort(int_arr);
document.write(numbers);
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
   val ints = intArrayOf(6, 2, 7, 8, 3, 1, 10, 5, 4, 9)
   ints.sort()
   println(ints.joinToString(prefix = "[", postfix = "]"))
}
```


{{out}}

```txt

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

```



## Lasso


```Lasso
local(array) = array(5,20,3,2,6,1,4)
#array->sort
#array // 1, 2, 3, 4, 5, 6, 20

// Reverse the sort order
#array->sort(false)
#array // 20, 6, 5, 4, 3, 2, 1
```



## jq

jq's builtin <code>sort</code> filter sorts the elements of an array in ascending order:

```jq
[2,1,3] | sort  # => [1,2,3]
```



## Julia

Julia has both out-of-place (<code>sort</code>) and in-place (<code>sort!</code>) sorting functions in its standard-library:

```julia>julia
 a = [4,2,3,1]
4-element Int32 Array:
 4
 2
 3
 1
julia> sort(a) #out-of-place/non-mutating sort
4-element Int32 Array:
 1
 2
 3
 4

julia> a
4-element Int32 Array:
 4
 2
 3
 1

julia> sort!(a) # in-place/mutating sort
4-element Int32 Array:
 1
 2
 3
 4

julia> a
4-element Int32 Array:
 1
 2
 3
 4
```



## K


```k
  num: -10?10              / Integers from 0 to 9 in random order
5 9 4 2 0 3 6 1 8 7

  srt: {x@<x}              / Generalized sort ascending
  srt num
0 1 2 3 4 5 6 7 8 9
```



## Liberty BASIC

LB has an array-sort command. Parameters are arrayname, start term, finish term.

```lb
N =20
dim IntArray( N)

print "Original order"
for i =1 to N
    t =int( 1000 *rnd( 1))
    IntArray( i) =t
    print t
next i

sort IntArray(), 1, N

print "Sorted oprder"
for i =1 to N
    print IntArray( i)
next i
```



## Lingo


```lingo
l = [7, 4, 23]
l.sort()
put l
-- [4, 7, 23]
```



## LiveCode

LiveCode can sort lines or items natively. The delimiter for items can be set to any single character, but defaults to comma.

```LiveCode
put "3,2,5,4,1" into X
sort items of X numeric
put X
-- outputs "1,2,3,4,5"
```



## Lua


```lua
t = {4, 5, 2}
table.sort(t)
print(unpack(t))
```



## Maple


```Maple
sort([5,7,8,3,6,1]);
sort(Array([5,7,8,3,6,1]))
```



## Mathematica


```mathematica
numbers = Sort[{2,4,3,1,2}]
```



## MATLAB


```Matlab
a = [4,3,7,-2,9,1]; b = sort(a)     % b contains elements of a in ascending order
[b,idx] = sort(a)                   % b contains a(idx)
```



## Maxima


```maxima
sort([9, 4, 3, 7, 6, 1, 10, 2, 8, 5]);
```



## MAXScript


```maxscript
arr = #(5, 4, 3, 2, 1)
arr = sort arr
```



## Mercury

<lang>:- module sort_int_list.
:- interface.
:- import_module io.

:- pred main(io::di, uo::uo) is det.

:- implementation.
:- import_module list.

main(!IO) :-
  Nums = [2, 4, 0, 3, 1, 2],
  list.sort(Nums, Sorted),
  io.write(Sorted, !IO),
  io.nl(!IO).
```



## min

{{works with|min|0.19.3}}

```min
(5 2 1 3 4) '> sort print
```

{{out}}

```txt

(1 2 3 4 5)

```


=={{header|Modula-3}}==
Modula-3 provides a generic <tt>ArraySort</tt> module, as well as an instance of that module for integers called <tt>IntArraySort</tt>.

```modula3
MODULE ArraySort EXPORTS Main;

IMPORT IntArraySort;

VAR arr := ARRAY [1..10] OF INTEGER{3, 6, 1, 2, 10, 7, 9, 4, 8, 5};

BEGIN
  IntArraySort.Sort(arr);
END ArraySort.
```



## MUMPS


```MUMPS
SORTARRAY(X,SEP)
 ;X is the list of items to sort
 ;X1 is the temporary array
 ;SEP is the separator string between items in the list X
 ;Y is the returned list
 ;This routine uses the inherent sorting of the arrays
 NEW I,X1,Y
 SET Y=""
 FOR I=1:1:$LENGTH(X,SEP) SET X1($PIECE(X,SEP,I))=""
 SET I="" FOR  SET I=$O(X1(I)) Q:I=""  SET Y=$SELECT($L(Y)=0:I,1:Y_SEP_I)
 KILL I,X1
 QUIT Y
```

Output:
```txt
USER>W $$SORTARRAY^ROSETTA("3,5,1,99,27,16,0,-1",",")
-1,0,1,3,5,16,27,99

```



## Neko


```ActionScript
/**
 <doc><h2>Sort integer array, in Neko</h2>
   <p>Array sort function modified from Haxe codegen with -D neko-source</p>
   <p>The Neko target emits support code for Haxe basics, sort is included</p>
   <p>Tectonics:<br />prompt$ nekoc sort.neko<br />prompt$ neko sort</p>
 </doc>
**/

var sort = function(a) {
    var i = 0;
    var len = $asize(a);
    while ( i < len ) {
        var swap = false;
        var j = 0;
        var max = (len - i) - 1;
        while ( j < max ) {
            if ( (a[j] - a[j + 1]) > 0 ) {
                var tmp = a[j + 1];
                a[j + 1] = a[j];
                a[j] = tmp;
                swap = true;
            }
            j += 1;
        }
        if ( $not(swap) )
            break;;
        i += 1;
    }
    return a;
}

var arr = $array(5,3,2,1,4)
$print(arr, "\n")

/* Sorts in place */
sort(arr)
$print(arr, "\n")

/* Also returns the sorted array for chaining */
$print(sort($array(3,1,4,1,5,9,2,6,5,3,5,8)), "\n")
```


{{out}}

```txt
prompt$ nekoc sort.neko
prompt$ neko sort.n
[5,3,2,1,4]
[1,2,3,4,5]
[1,1,2,3,3,4,5,5,5,6,8,9]
```



## Nemerle


```Nemerle
using System.Console;

module IntSort
{
    Main() : void
    {
        def nums = [1, 5, 3, 7, 2, 8, 3, 9];
        def sorted = nums.Sort((x, y) => x.CompareTo(y));
        
        WriteLine(nums);
        WriteLine(sorted);
    }
}
```

Output:

```txt
[1, 5, 3, 7, 2, 8, 3, 9]
[1, 2, 3, 3, 5, 7, 8, 9]
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

ia = int[]
ia = [ 2, 4, 3, 1, 2, -1, 0, -2 ]

display(ia)
Arrays.sort(ia)
display(ia)

-- Display results
method display(in = int[]) public static

  sorted = Rexx('')

  loop ix = 0 for in.length
    sorted = sorted || Rexx(in[ix]).right(4)
    end ix

  say sorted.strip('t')

  return
```


'''Output'''
<pre style="overflow:scroll">
   2   4   3   1   2  -1   0  -2 
  -2  -1   0   1   2   2   3   4 

```


NetRexx reimplementations of the [[#REXX|Rexx]] samples from below:


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols

/*REXX program to sort an integer array.*/

numeric digits 20    /*handle larger numbers.*/
a = ''
a[ 1]=               1
a[ 2]=               0
a[ 3]=              -1
a[ 4]=               0
a[ 5]=               5
a[ 6]=               0
a[ 7]=             -61
a[ 8]=               0
a[ 9]=            1385
a[10]=               0
a[11]=          -50521
a[12]=               0
a[13]=         2702765
a[14]=               0
a[15]=      -199360981
a[16]=               0
a[17]=     19391512145
a[18]=               0
a[19]=  -2404879675441
a[20]=               0
a[21]= 370371188237525

size = 21                          /*we have a list of 21 Euler numbers.*/
tell('un-sorted', a, size)
a[0] = size
esort(a, 1)
tell('   sorted', a, size)

return

/*----------------------------------ESORT subroutine--------------------*/
method esort(a, size) public static
--esort: procedure expose a.;

  h = a[0]
 
  loop while h > 1
    h = h % 2
    loop i = 1 for a[0] - h
      j = i
      k = h + i
      loop while a[k] < a[j]
        t    = a[j]
        a[j] = a[k]
        a[k] = t
        if h >= j then leave
        j = j - h
        k = k - h
        end
      end i
    end

return

/*----------------------------------TELL subroutine---------------------*/
method tell(arg, a, size) public static
--tell:

  say arg.center(40, '-')
  loop j = 1 for size
    say arg 'array element' j.right(size.length)'='a[j].right(25)
    end j
  say

  return
```


'''Output'''
<pre style="height:30ex;overflow:scroll">
---------------un-sorted----------------
un-sorted array element  1=                        1
un-sorted array element  2=                        0
un-sorted array element  3=                       -1
un-sorted array element  4=                        0
un-sorted array element  5=                        5
un-sorted array element  6=                        0
un-sorted array element  7=                      -61
un-sorted array element  8=                        0
un-sorted array element  9=                     1385
un-sorted array element 10=                        0
un-sorted array element 11=                   -50521
un-sorted array element 12=                        0
un-sorted array element 13=                  2702765
un-sorted array element 14=                        0
un-sorted array element 15=               -199360981
un-sorted array element 16=                        0
un-sorted array element 17=              19391512145
un-sorted array element 18=                        0
un-sorted array element 19=           -2404879675441
un-sorted array element 20=                        0
un-sorted array element 21=          370371188237525

---------------   sorted----------------
   sorted array element  1=           -2404879675441
   sorted array element  2=               -199360981
   sorted array element  3=                   -50521
   sorted array element  4=                      -61
   sorted array element  5=                       -1
   sorted array element  6=                        0
   sorted array element  7=                        0
   sorted array element  8=                        0
   sorted array element  9=                        0
   sorted array element 10=                        0
   sorted array element 11=                        0
   sorted array element 12=                        0
   sorted array element 13=                        0
   sorted array element 14=                        0
   sorted array element 15=                        0
   sorted array element 16=                        1
   sorted array element 17=                        5
   sorted array element 18=                     1385
   sorted array element 19=                  2702765
   sorted array element 20=              19391512145
   sorted array element 21=          370371188237525

```



```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols

/*REXX program to sort an interesting integer list.*/

bell = '1 1 2 5 15 52 203 877 4140 21147 115975'      /*some Bell numbers.*/
bern = '1 -1 1 0 -1 0 1 0 -1 0 5 0 -691 0 7 0 -3617'  /*some Bernoulli num*/
perrin = '3 0 2 3 2 5 5 7 10 12 17 22 29 39 51 68 90' /*some Perrin nums. */
list = bell bern perrin                               /*combine the three.*/

size = list.words

a = 0
loop j = 1 for size
  a[j] = list.word(j)
  end j

say '  as is='list
a[0] = size
esort(a, size)
bList = ''

loop j = 1 for size
  bList = bList a[j]
  end j

blist = bList.strip
say ' sorted='bList

return

/*----------------------------------ESORT subroutine--------------------*/
method esort(a, size) public static
--esort: procedure expose a.;

  h = a[0]
 
  loop while h > 1
    h = h % 2
    loop i = 1 for a[0] - h
      j = i
      k = h + i
      loop while a[k] < a[j]
        t    = a[j]
        a[j] = a[k]
        a[k] = t
        if h >= j then leave
        j = j - h
        k = k - h
        end
      end i
    end

return
```


'''Output'''
<pre style="overflow:scroll">
  as is=1 1 2 5 15 52 203 877 4140 21147 115975 1 -1 1 0 -1 0 1 0 -1 0 5 0 -691 0 7 0 -3617 3 0 2 3 2 5 5 7 10 12 17 22 29 39 51 68 90
 sorted=-3617 -691 -1 -1 -1 0 0 0 0 0 0 0 0 1 1 1 1 1 2 2 2 3 3 5 5 5 5 7 7 10 12 15 17 22 29 39 51 52 68 90 203 877 4140 21147 115975

```



## Nial


```nial>sort 
= 9 6 8 7 1 10
= 10 9 8 7 6 1
```



## Nim


```nim
import algorithm
  
var a: array[0..8,int] = [2,3,5,8,4,1,6,9,7]
a.sort(system.cmp[int], Ascending)
for x in a:
   echo(x)
```

{{out}}

```txt
1
2
3
4
5
6
7
8
9
```



## Niue

'''Library'''

```Niue
2 6 1 0 3 8 sort .s
0 1 2 3 6 8
```


=={{header|Objective-C}}==

```objc
NSArray *nums = @[@2, @4, @3, @1, @2];
NSArray *sorted = [nums sortedArrayUsingSelector:@selector(compare:)];
```



## Objeck


```objeck
bundle Default {
  class Sort {
    function : Main(args : System.String[]) ~ Nil {
      nums := Structure.IntVector->New([2,4,3,1,2]);
      nums->Sort();
    }
  }
}
```



## OCaml


### Array


```ocaml
let nums = [|2; 4; 3; 1; 2|]
Array.sort compare nums
```



### List


```ocaml
let nums = [2; 4; 3; 1; 2]
let sorted = List.sort compare nums
```



## Octave


The variable <tt>v</tt> can be a vector or a matrix (columns will be sorted).


```octave
sortedv = sort(v);
```



## Oforth



```Oforth
[ 8, 2, 5, 9, 1, 3, 6, 7, 4 ] sort
```



## ooRexx


```rexx
a = .array~of(4, 1, 6, -2, 99, -12)
say "The sorted numbers are"
say a~sortWith(.numericComparator~new)~makeString
```

Output:

```txt

The sorted numbers are
-12
-2
1
4
6
99

```



## Order

Passing the less-than operator to the built-in sequence (i.e. list) sort function:

```c>#include <order/interpreter.h


ORDER_PP( 8seq_sort(8less, 8seq(2, 4, 3, 1, 2)) )
```



## Oz


```oz
declare
  Nums = [2 4 3 1 2]
  Sorted = {List.sort Nums Value.'<'}
in
  {Show Sorted}
```



## PARI/GP


```parigp
vecsort(v)
```



## Peloton

Sorting a list of numbers as strings and as numbers (from the manual.)

```sgml
Construct a list of numbers 
<@ LETCNSLSTLIT>L|65^84^1^25^77^4^47^2^42^44^41^25^69^3^51^45^4^39^</@> 
Numbers sort as strings
<@ ACTSRTENTLST>L</@> 
<@ SAYDMPLST>L</@> 
<@ ACTSRTENTLSTLIT>L|__StringDescending</@> 
<@ SAYDMPLST>L</@>
 
Construct another list of numbers
<@ LETCNSLSTLIT>list|65^84^1^25^77^4^47^2^42^44^41^25^69^3^51^45^4^39^</@> 
Numbers sorted as numbers
<@ ACTSRTENTLSTLIT>list|__Numeric</@> 
<@ SAYDMPLST>list</@> 
<@ ACTSRTENTLSTLIT>list|__NumericDescending</@> 
<@ SAYDMPLST>list</@>
```


Output

```html
Construct a list of numbers 
 
Numbers sort as strings
 
1^2^25^25^3^39^4^4^41^42^44^45^47^51^65^69^77^84^ 
 
84^77^69^65^51^47^45^44^42^41^4^4^39^3^25^25^2^1^
 
Construct another list of numbers
 
Numbers sorted as numbers
 
1^2^3^4^4^25^25^39^41^42^44^45^47^51^65^69^77^84^ 
 
84^77^69^65^51^47^45^44^42^41^39^25^25^4^4^3^2^1^
```



## Perl

{{works with|Perl|5.8.6}}

```perl
@nums = (2,4,3,1,2);
@sorted = sort {$a <=> $b} @nums;
```



## Perl 6

If <code>@a</code> contains only numbers:


```perl6>my @sorted = sort @a;</lang


For an in-place sort:


```perl6>@a .= sort;</lang



## Phix


```Phix
?sort({9, 10, 3, 1, 4, 5, 8, 7, 6, 2})
```



## PHP

{{works with|PHP|4.4.4 CLI}}

```php
<?php
$nums = array(2,4,3,1,2);
sort($nums);
?>
```



## PicoLisp

The [http://software-lab.de/doc/refS.html#sort sort] function in PicoLisp
returns already by default an ascending list (of any type, not only integers):

```PicoLisp
(sort (2 4 3 1 2))
-> (1 2 2 3 4)
```



## PL/I

{{works with|IBM PL/I|7.5}}

```pli
DCL (T(10)) FIXED BIN(31); /* scratch space of length N/2 */

MERGE: PROCEDURE (A,LA,B,LB,C);
   DECLARE (A(*),B(*),C(*)) FIXED BIN(31);
   DECLARE (LA,LB) FIXED BIN(31) NONASGN;
   DECLARE (I,J,K) FIXED BIN(31);
   
   I=1; J=1; K=1;
   DO WHILE ((I <= LA) & (J <= LB));
      IF(A(I) <= B(J)) THEN
         DO; C(K)=A(I); K=K+1; I=I+1; END;
      ELSE
         DO; C(K)=B(J); K=K+1; J=J+1; END;
   END;
   DO WHILE (I <= LA);
      C(K)=A(I); I=I+1; K=K+1;
   END;
   RETURN;
END MERGE;

MERGESORT: PROCEDURE (A,N) RECURSIVE ;
     DECLARE (A(*))               FIXED BINARY(31);
     DECLARE N                    FIXED BINARY(31) NONASGN;
     DECLARE Temp                 FIXED BINARY;
     DECLARE (M,I)                FIXED BINARY;
     DECLARE AMP1(N)              FIXED BINARY(31) BASED(P);
     DECLARE P POINTER;
    IF (N=1) THEN RETURN;
   M = trunc((N+1)/2);
   IF (M>1) THEN CALL MERGESORT(A,M);
   P=ADDR(A(M+1)); 
   IF (N-M > 1) THEN CALL MERGESORT(AMP1,N-M);
   IF A(M) <= AMP1(1) THEN RETURN;
   DO I=1 to M; T(I)=A(I); END;
   CALL MERGE(T,M,AMP1,N-M,A);
   RETURN;
END MERGESORT;
```



## Pop11

Pop11 library function sorts lists. So we first convert array to list, then sort and finally convert back:


```pop11
lvars ar = {2 4 3 1 2};
;;; Convert array to list.
;;; destvector leaves its results and on the pop11 stack + an integer saying how many there were
destvector(ar);
;;; conslist uses the items left on the stack plus the integer, to make a list of those items.
lvars ls = conslist();
;;; Sort it
sort(ls) -> ls;
;;; Convert list to array
destlist(ls);
consvector() -> ar;
```


The above can be abbreviated to more economical, but possibly more opaque, syntax, using pop11 as a functional language:


```pop11
lvars ar = {2 4 3 1 2};
consvector(destlist(sort(conslist(destvector(ar))))) -> ar;
;;; print the sorted vector:
ar =>
** {1 2 2 3 4}
```


(The list created by conslist will be garbage-collected.)

Alternatively, using the datalist function, even more economically: 


```pop11
lvars ar = {2 4 3 1 2};
consvector(destlist(sort(datalist(ar)))) -> ar;
```

 

or in Forth-like pop11 postfix syntax:


```pop11
lvars ar = {2 4 3 1 2};
ar.datalist.sort.destlist.consvector -> ar;
```



## Potion


```potion
(7, 5, 1, 2, 3, 8, 9) sort join(", ") print
```



## PowerBASIC

PowerBASIC has several options available for sorting. At its simplest, an array (of any type) is sorted using <code>ARRAY SORT</code>:

```powerbasic
ARRAY SORT x()
```


Options are available to limit sorting to only part of the array, collate string arrays, sort multiple arrays together, etc. (Details [http://www.powerbasic.com/support/help/pbwin/html/ARRAY_SORT_statement.htm here].)


## PowerShell


```powershell
34,12,23,56,1,129,4,2,73 | Sort-Object
```



## Prolog


```txt
 ?- msort([10,5,13,3, 85,3,1], L).
L = [1,3,3,5,10,13,85].
```

Note that [http://www.swi-prolog.org/pldoc/man?predicate=sort/2 sort/2] removes duplicates.


## PureBasic


```PureBasic
Dim numbers(20)
For i = 0 To 20
   numbers(i) = Random(1000)
Next

SortArray(numbers(), #PB_Sort_Ascending)
```



## Python

{{works with|Python|2.3}}

```python
nums = [2,4,3,1,2]
nums.sort()
```


'''Note:''' The array <tt>nums</tt> is sorted in place.

'''Interpreter:''' [[Python]] 2.4 (and above)

You could also use the built-in sorted() function


```python
nums = sorted([2,4,3,1,2])
```



## R


```r
nums <- c(2,4,3,1,2)
sorted <- sort(nums)
```



## Racket


```Racket

-> (sort '(1 9 2 8 3 7 4 6 5) <)
'(1 2 3 4 5 6 7 8 9)

```



## Rascal

Rascal has a built-in sort function that sort the elements of a list. Additionally, one can give a LessThenOrEqual function to compare the elements (See [http://tutor.rascal-mpl.org/Courses/Rascal/Rascal.html#/Courses/Rascal/Libraries/Prelude/List/sort/sort.html documentation]).

```rascal>rascal
import List;
ok

rascal>a = [1, 4, 2, 3, 5];
list[int]: [1,4,2,3,5]

rascal>sort(a)
list[int]: [1,2,3,4,5]

rascal>sort(a, bool(int a, int b){return a >= b;})
list[int]: [5,4,3,2,1]
```



## Raven

Sort list in place:


```raven
[ 2 4 3 1 2 ] sort
```



## REBOL


```rebol
sort [2 4 3 1 2]
```



## Red


```Red>>
 nums: [3 2 6 4 1 9 0 5 7]
== [3 2 6 4 1 9 0 5 7]
>> sort nums
== [0 1 2 3 4 5 6 7 9]
```



## REXX


### sort an array

This REXX version creates an array with over a score of Euler numbers (integers), then sorts it.

```rexx
/*REXX program sorts an array (using E─sort), in this case, the array contains integers.*/
numeric digits 30                                /*enables handling larger Euler numbers*/
                          @.  =              0;            @.1 =               1
                          @.3 =             -1;            @.5 =               5
                          @.7 =            -61;            @.9 =            1385
                          @.11=         -50521;            @.13=         2702765
                          @.15=     -199360981;            @.17=     19391512145
                          @.19= -2404879675441;            @.21= 370371188237525
#= 21                                            /*indicate there're  21 Euler  numbers.*/
call tell  'unsorted'                            /*display the array before the  eSort. */
call eSort     #                                 /*sort the array of some Euler numbers.*/
call tell  '  sorted'                            /*display the array  after  the eSort. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
eSort: procedure expose @.;   parse arg N;     h=N                   /*an eXchange sort.*/
              do  while h>1;                   h= h%2                /*define a segment.*/
                 do i=1  for N-h;              j=i;     k= h+i       /*sort top segment.*/
                    do  while  @.k<@.j                               /*see if need swap.*/
                        parse value  @.j @.k   with   @.k @.j        /*swap two elements*/
                        if h>=j  then leave;   j= j-h;   k= k-h      /*this part sorted?*/
                        end   /*while @.k<@.j*/
                    end       /*i*/
              end             /*while h>1*/
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell:  say copies('─', 65);       _= left('',9);                       w= length(#)
              do j=1  for #;  say _ arg(1)  'array element'   right(j, w)"="right(@.j, 20)
              end   /*j*/
       return
```

{{out|output|text=  when using the default internal input:}}

```txt

─────────────────────────────────────────────────────────────────
          unsorted array element  1=                   1
          unsorted array element  2=                   0
          unsorted array element  3=                  -1
          unsorted array element  4=                   0
          unsorted array element  5=                   5
          unsorted array element  6=                   0
          unsorted array element  7=                 -61
          unsorted array element  8=                   0
          unsorted array element  9=                1385
          unsorted array element 10=                   0
          unsorted array element 11=              -50521
          unsorted array element 12=                   0
          unsorted array element 13=             2702765
          unsorted array element 14=                   0
          unsorted array element 15=          -199360981
          unsorted array element 16=                   0
          unsorted array element 17=         19391512145
          unsorted array element 18=                   0
          unsorted array element 19=      -2404879675441
          unsorted array element 20=                   0
          unsorted array element 21=     370371188237525
─────────────────────────────────────────────────────────────────
            sorted array element  1=      -2404879675441
            sorted array element  2=          -199360981
            sorted array element  3=              -50521
            sorted array element  4=                 -61
            sorted array element  5=                  -1
            sorted array element  6=                   0
            sorted array element  7=                   0
            sorted array element  8=                   0
            sorted array element  9=                   0
            sorted array element 10=                   0
            sorted array element 11=                   0
            sorted array element 12=                   0
            sorted array element 13=                   0
            sorted array element 14=                   0
            sorted array element 15=                   0
            sorted array element 16=                   1
            sorted array element 17=                   5
            sorted array element 18=                1385
            sorted array element 19=             2702765
            sorted array element 20=         19391512145
            sorted array element 21=     370371188237525

```



### sort a list

This REXX version creates a list with a bunch of interesting integers, then sorts it.

Because it so much more efficient to sort an array,   an array is built from the list,

it is then sorted,   and then the list is re-constituted.

```rexx
/*REXX program sorts  (using E─sort)  and displays a list of some interesting integers. */
  Bell=  1 1 2 5 15 52 203 877 4140 21147 115975           /*a few  Bell          "     */
  Bern= '1 -1 1 0 -1 0 1 0 -1 0 5 0 -691 0 7 0 -3617'      /*"  "   Bernoulli     "     */
Perrin=  3 0 2 3 2 5 5 7 10 12 17 22 29 39 51 68 90        /*"  "   Perrin        "     */
list=Bell  Bern  Perrin                                    /*throw them all ───► a pot. */
say 'unsorted =' list                                      /*display what's being shown.*/
size=words(list)                                           /*nice to have # of elements.*/
                              do j=1  for size             /*build an array, a single   */
                              @.j=word(list,j)             /*     ··· element at a time.*/
                              end    /*j*/
call eSort size                                            /*sort the collection of #s. */
$=                                                         /*list: define as null so far*/
                              do k=1  for size             /*build a list from the array*/
                              $=$ @.k                      /*append a number to the list*/
                              end    /*k*/
say '  sorted =' space($)                                  /*display the sorted list.   */
exit                                              /*stick a fork in it,  we're all done.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
eSort: procedure expose @.;   parse arg N;     h=N                   /*an eXchange sort.*/
              do  while h>1;                   h= h%2                /*define a segment.*/
                 do i=1  for N-h;              j=i;     k= h+i       /*sort top segment.*/
                    do  while  @.k<@.j                               /*see if need swap.*/
                        parse value  @.j @.k   with   @.k @.j        /*swap two elements*/
                        if h>=j  then leave;   j= j-h;   k= k-h      /*this part sorted?*/
                        end   /*while @.k<@.j*/
                    end       /*i*/
              end             /*while h>1*/
       return
```

{{out|output|text=  when using the default internal inputs:}}

```txt

unsorted = 1 1 2 5 15 52 203 877 4140 21147 115975 1 -1 1 0 -1 0 1 0 -1 0 5 0 -691 0 7 0 -3617 3 0 2 3 2 5 5 7 10 12 17 22 29 39 51 68 90
  sorted = -3617 -691 -1 -1 -1 0 0 0 0 0 0 0 0 1 1 1 1 1 2 2 2 3 3 5 5 5 5 7 7 10 12 15 17 22 29 39 51 52 68 90 203 877 4140 21147 115975

```



## Ring


```ring
aArray = [2,4,3,1,2]
see sort(aArray)
```



## Ruby


```ruby
nums = [2,4,3,1,2]
sorted = nums.sort      # returns a new sorted array.  'nums' is unchanged
p sorted                #=> [1, 2, 2, 3, 4]
p nums                  #=> [2, 4, 3, 1, 2]

nums.sort!              # sort 'nums' "in-place"
p nums                  #=> [1, 2, 2, 3, 4]
```



## Rust

Uses merge sort in place (undocumented), allocating ~2*n memory where n is a length of an array.

```rust
fn main() {
    let mut a = vec!(9, 8, 7, 6, 5, 4, 3, 2, 1, 0);

    a.sort();
    println!("{:?}", a);
}
```



## Scala

{{libheader|Scala}}

### Array

Scala's "default" Array is a ''mutable'' data structure, very close to Java's Array. Generally speaking, that means an "array" is not very Scala-lesque, even as mutable data structures go. It can serves a purpose, though. If array is the right data type for your need, then that is how you sort it.
```Scala
import scala.compat.Platform

object Sort_an_integer_array extends App {
  val array = Array((for (i <- 0 to 10) yield scala.util.Random.nextInt()):
    _* /*Sequence is passed as multiple parameters to Array(xs : T*)*/)

  /** Function test the array if it is in order */
  def isSorted[T](arr: Array[T]) = array.sliding(2).forall(pair => pair(0) <= pair(1))

  assert(!isSorted(array), "Not random")
  scala.util.Sorting.quickSort(array)
  assert(isSorted(array), "Not sorted")

  println(s"Array in sorted order.\nSuccessfully completed without errors. [total ${Platform.currentTime - executionStart} ms]")
}
```


### List


```Scala
println(List(5,2,78,2,578,-42).sorted)
//--> List(-42, 2, 2, 5, 78, 578)
```



## Scheme

{{works with|Guile}}
Same as [[Common Lisp]]

```scheme
(sort #(9 -2 1 2 8 0 1 2) #'<)
```


{{libheader|Scheme/SRFIs}}

Sorting is also available through SRFIs.  SRFI 132 provides separate list-sort and vector-sort routines:


```scheme

> (import (srfi 132))
> (list-sort < '(9 -2 1 2 8 0 1 2))
(-2 0 1 1 2 2 8 9)

> (vector-sort < #(9 -2 1 2 8 0 1 2))
#(-2 0 1 1 2 2 8 9)

```


SRFI 132 replaced the older SRFI 95, which is still found in many implementations.  SRFI 95 provides a generic sort function (but note the order of the sequence and comparator!):


```scheme

> (import (srfi 95))
> (sort '(9 -2 1 2 8 0 1 2) <)
(-2 0 1 1 2 2 8 9)
> (sort #(9 -2 1 2 8 0 1 2) <)
#(-2 0 1 1 2 2 8 9)

```



## Seed7


```seed7
var array integer: nums is [] (2, 4, 3, 1, 2);

nums := sort(nums);
```



## Sidef


```ruby
var nums = [2,4,3,1,2];
var sorted = nums.sort;  # returns a new sorted array.
nums.sort!;              # sort 'nums' "in-place"
```



## Slate


```slate
 #(7 5 2 9 0 -1) sort
```



## Smalltalk


```smalltalk
 #(7 5 2 9 0 -1) asSortedCollection
```

or destructive:

```smalltalk
 #(7 5 2 9 0 -1) sort
```



## Sparkling


```sparkling
var arr = { 2, 8, 1, 4, 6, 5, 3, 7, 0, 9 };
sort(arr);
```



## Standard ML

The Standard ML Basis library does not have any sorting facilities. But each implementation of Standard ML has its own.


### Array

{{works with|SML/NJ}}

```sml
- val nums = Array.fromList [2, 4, 3, 1, 2];
val nums = [|2,4,3,1,2|] : int array
- ArrayQSort.sort Int.compare nums;
val it = () : unit
- nums;
val it = [|1,2,2,3,4|] : int array
```


{{works with|Moscow ML}}

```sml
- load "Arraysort";
> val it = () : unit
- load "Int";
> val it = () : unit
- val nums = Array.fromList [2, 4, 3, 1, 2];
> val nums = <array> : int array
- Arraysort.sort Int.compare nums;
> val it = () : unit
- Array.foldr op:: [] nums;
> val it = [1, 2, 2, 3, 4] : int list
```



### List

{{works with|SML/NJ}}

```sml
- val nums = [2, 4, 3, 1, 2];
val nums = [2,4,3,1,2] : int list
- val sorted = ListMergeSort.sort op> nums;
val sorted = [1,2,2,3,4] : int list
```


{{works with|Moscow ML}}

```sml
- load "Listsort";
> val it = () : unit
- load "Int";
> val it = () : unit
- val nums = [2, 4, 3, 1, 2];
> val nums = [2, 4, 3, 1, 2] : int list
- val sorted = Listsort.sort Int.compare nums;
> val sorted = [1, 2, 2, 3, 4] : int list
```



## Stata


###  Sort a Stata dataset 

See '''[https://www.stata.com/help.cgi?sort sort]''' in Stata help.


```stata
. clear
. matrix a=(2,9,4,7,5,3,6,1,8)'
. qui svmat a
. sort a
. list

     +----+
     | a1 |
     |----|
  1. |  1 |
  2. |  2 |
  3. |  3 |
  4. |  4 |
  5. |  5 |
     |----|
  6. |  6 |
  7. |  7 |
  8. |  8 |
  9. |  9 |
     +----+
```



###  Sort a macro list 

See '''[https://www.stata.com/help.cgi?macrolists macrolists]''' in Stata help for other functions on lists stored in macros.


```stata
. local a 2 9 4 7 5 3 6 1 8
. di "`: list sort a'"
1 2 3 4 5 6 7 8 9
```



###  Mata 

See Mata's '''[http://www.stata.com/help.cgi?mf_sort sort]''' function.


```stata
mata
: a=2\9\4\7\5\3\6\1\8

: sort(a,1)
       1
    +-----+
  1 |  1  |
  2 |  2  |
  3 |  3  |
  4 |  4  |
  5 |  5  |
  6 |  6  |
  7 |  7  |
  8 |  8  |
  9 |  9  |
    +-----+
end
```



## Swift


### Sort in place

{{works with|Swift|2.x+}}

```swift
var nums = [2, 4, 3, 1, 2]
nums.sortInPlace()
print(nums)
```

or

```swift
var nums = [2, 4, 3, 1, 2]
nums.sortInPlace(<)
print(nums)
```


{{works with|Swift|1.x}}

```swift
var nums = [2, 4, 3, 1, 2]
nums.sort(<)
println(nums)
```

or

```swift
var nums = [2, 4, 3, 1, 2]
sort(&nums)
println(nums)
```

or

```swift
var nums = [2, 4, 3, 1, 2]
sort(&nums, <)
println(nums)
```



### Return new array

You could also create a new sorted array without affecting the original one:

{{works with|Swift|2.x+}}

```swift
let nums = [2,4,3,1,2].sort()
print(nums)
```

or

```swift
let nums = [2,4,3,1,2].sort(<)
print(nums)
```


{{works with|Swift|1.x}}

```swift
let nums = sorted([2,4,3,1,2])
println(nums)
```

or

```swift
let nums = [2,4,3,1,2].sorted(<)
println(nums)
```



## Tcl


```tcl
set result [lsort -integer $unsorted_list]
```


=={{header|TI-83 BASIC}}==
Store input into L<sub>1</sub>, run prgmSORTBTIN, and L<sub>2</sub> will be L<sub>1</sub>, only sorted.
 :L<sub>1</sub>→L<sub>2</sub>
 :SortA(L<sub>2</sub>)
SortA is found via: [LIST] → ENTER. SortD is also available for a descending sort.


## Toka

This can be done by using the bubble sort library:


```toka
needs bsort
arrayname number_elements bsort
```


See the Toka entry on [[Bubble Sort]] for a full example.


## UNIX Shell

Each shell parameter separates the integers using the default IFS whitespace (space, tab, newline).


```bash
nums="2 4 3 1 5"
sorted=`printf "%s\n" $nums | sort -n`
echo $sorted  # prints 1 2 3 4 5
```


Alternate solution: <tt>sorted=`for i in $nums; do echo $i; done | sort -n`</tt>

----
Some shells have real arrays. You still need IFS to split the string from <tt>sort -n</tt> to an array.

{{works with|pdksh|5.2.14}}

```bash
set -A nums 2 4 3 1 5
set -A sorted $(printf "%s\n" ${nums[*]} | sort -n)
echo ${sorted[*]}  # prints 1 2 3 4 5
```


Users of [[bash]], [[ksh93]] and [[mksh]] can probably use the <tt>nums=(2 4 3 1 2)</tt> syntax.


## Ursa


```ursa>decl int<
 nums
append 2 4 3 1 2 nums
sort nums
```



## Ursala

using the built in sort operator, -<, with the nleq library function
for comparing natural numbers

```Ursala
#import nat

#cast %nL

example = nleq-< <39,47,40,53,14,23,88,52,78,62,41,92,88,66,5,40>
```

output:

```txt
<5,14,23,39,40,40,41,47,52,53,62,66,78,88,88,92>
```



## WDTE


```WDTE>let a =
 import 'arrays';
a.sort [39; 47; 40; 53; 14; 23; 88; 52; 78; 62; 41; 92; 88; 66; 5; 40] < -- io.writeln io.stdout;
```



## Wortel


```wortel
@sort [39 47 40 53 14 23 88 52 78 62 41 92 88 66 5 40]
```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

proc    SSort(A, N);            \Shell sort array in ascending order
int     A;                      \address of array
int     N;                      \number of elements in array (size)
int     I, J, Gap, JG, T;
[Gap:= N>>1;
while Gap > 0 do
        [for I:= Gap to N-1 do
                [J:= I - Gap;
                loop    [JG:= J + Gap;
                        if A(J) <= A(JG) then quit;
                        T:= A(J);   A(J):= A(JG);   A(JG):= T;  \swap elements
                        J:= J - Gap;
                        if J < 0 then quit;
                        ];
                ];
        Gap:= Gap>>1;
        ];
];      \SSort

int A, I;
[A:= [3, 1, 4, 1, 5, 9, 2, 6, 5, 4];
SSort(A, 10);
for I:= 0 to 10-1 do [IntOut(0, A(I));  ChOut(0, ^ )];
CrLf(0);
]
```


Output:

```txt

1 1 2 3 4 4 5 5 6 9 

```



## Yabasic


```Yabasic
export sub shell_sort(x())
// Shell sort based on insertion sort

   local gap, i, j, first, last, tempi, tempj

   last = arraysize(x(),1)
   gap = int(last / 10) + 1
   while(TRUE)
	first = gap + 1
	for i = first to last
	   	tempi = x(i)
	    	j = i - gap
	    	while(TRUE)
			tempj = x(j)
			if tempi >= tempj then
		    		j = j + gap
		    		break
			end if
			x(j+gap) = tempj
			if j <= gap then
		    		break
			end if
			j = j - gap
	    	wend
	    	x(j) = tempi
	next i
	if gap = 1 then
	   	return
	else
	   	gap = int(gap / 3.5) + 1
	end if
   wend
end sub

if peek$("library") = "main" then

	clear screen
	
	ITEMS = 100
	dim numeros(ITEMS)
	
	for n = 1 to ITEMS
		numeros(n) = ran(ITEMS + 1)
	next n
	
	print time$
	shell_sort(numeros())
	print time$
	print "Press a key to see ordered numbers."
	inkey$
	
	for n = 1 to ITEMS
		print numeros(n),", ";
	next n

end if
```



## Yorick

In Yorick, ''sort'' returns an index list into the array that will put it in sorted order.

```yorick

nums = [2,4,3,1,2];
nums = nums(sort(nums));

```



## zkl

In place sorting read/write list:

```zkl
a:=L(4,5,2,6); a.sort(); a.println() //--> L(2,4,5,6)
```

Sort a read only list:

```zkl
a:=T(4,5,2,6); b:=a.sort();
b.println(); //--> L(2,4,5,6)
a.println(); //--> L(4,5,2,6)
```

