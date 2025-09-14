+++
title = "Two Sum"
description = ""
date = 2019-09-08T05:34:33Z
aliases = []
[extra]
id = 21143
[taxonomies]
categories = ["Arithmetic operations", "Arrays", "Algorithms", "task"]
tags = []
languages = [
  "aime",
  "algol_68",
  "applescript",
  "autohotkey",
  "awk",
  "befunge",
  "c",
  "cpp",
  "csharp",
  "d",
  "dart",
  "elixir",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jsish",
  "julia",
  "kotlin",
  "lua",
  "maple",
  "miniscript",
  "nim",
  "objeck",
  "ocaml",
  "oorexx",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "stata",
  "vba",
  "visual_basic_dotnet",
  "zkl",
]
+++

## Task
Given a sorted array of integers (with possibly duplicates), is it possible to find a pair of integers from that array that sum up to a given sum? If so, return indices of the two integers or an empty array if not. The solution is not necessarily unique.


### Example
Given numbers = [0, 2, 11, 19, 90], sum = 21,<br/>
Because numbers[1] + numbers[3] = 2 + 19 = 21,<br/>
return [1, 3].


### Source
[http://stackoverflow.com/questions/8334981/find-pair-of-numbers-in-array-that-add-to-given-sum Stack Overflow: Find pair of numbers in array that add to given sum]
<br/><br/>


## Aime


```aime
integer i, u, v;
index x;
list l;

l_bill(l, 0, 0, 2, 11, 19, 90);

for (i, u in l) {
    x[u] = i;
    if (i_jack(v, x, 21 - u)) {
        o_(v, " ", i, "\n");
        break;
    }
}
```

{{Out}}

```txt
1 3
```



## ALGOL 68

{{trans|Lua}}

```algol68
# returns the subscripts of a pair of numbers in a that sum to sum, a is assumed to be sorted #
# if there isn't a pair of numbers that summs to sum, an empty array is returned              #
PRIO TWOSUM = 9;
OP   TWOSUM = ( []INT a, INT sum )[]INT:
     BEGIN
        BOOL found := FALSE;
        INT i := LWB a;
        INT j := UPB a;
        WHILE i < j AND NOT found DO
            INT s = a[ i ] + a[ j ];
            IF s = sum THEN
                found  := TRUE
            ELIF s < sum THEN
                i +:= 1
            ELSE
                j -:= 1
            FI
        OD;
        IF found THEN ( i, j ) ELSE () FI
     END # TWOSUM # ;

# test the TWOSUM operator #
PROC print twosum = ( []INT a, INT sum )VOID:
     BEGIN
         []INT pair = a[ AT 0 ] TWOSUM sum;
         IF LWB pair > UPB pair THEN
             # no pair with the required sum #
             print( ( "[]", newline ) )
         ELSE
             # have a pair #
             print( ( "[", whole( pair[ LWB pair ], 0 ), ", ", whole( pair[ UPB pair ], 0 ), "]", newline ) )
         FI
     END # print twosum # ;
print twosum( (  0, 2, 11, 19, 90 ),         21 ); # should be [1, 3]             #
print twosum( ( -8, -2,  0,  1,  5, 8, 11 ),  3 ); # should be [0, 6] (or [1, 4]) #
print twosum( ( -3, -2,  0,  1,  5, 8, 11 ), 17 ); # should be []                 #
print twosum( ( -8, -2, -1,  1,  5, 9, 11 ),  0 )  # should be [2, 3]             #
```

{{out}}

```txt

[1, 3]
[0, 6]
[]
[2, 3]

```



## AppleScript

{{Trans|JavaScript}}
{{Trans|Haskell}}


Nesting concatMap or (>>=) (flip concatMap) yields the cartesian product of the list with itself. Skipping products where the y index is lower than the x index (see the use of 'drop' below) ignores the 'lower triangle' of the cartesian grid, excluding mirror-image and duplicate number pairs.


```AppleScript
-- sumTo :: Int -> [Int] -> [(Int, Int)]
on sumTwo(n, xs)
    set ixs to zip(enumFromTo(0, |length|(xs) - 1), xs)

    script ijIndices
        on |λ|(ix)
            set {i, x} to ix

            script jIndices
                on |λ|(jy)
                    set {j, y} to jy

                    if (x + y) = n then
                        {{i, j}}
                    else
                        {}
                    end if
                end |λ|
            end script

            |>>=|(drop(i + 1, ixs), jIndices)
        end |λ|
    end script

    |>>=|(ixs, ijIndices)
end sumTwo

-- TEST ----------------------------------------------------------------------
on run
    sumTwo(21, [0, 2, 11, 19, 90, 10])

    --> {{1, 3}, {2, 5}}
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
on |>>=|(xs, f)
    concat(map(f, xs))
end |>>=|

-- concat :: [[a]] -> [a] | [String] -> String
on concat(xs)
    script append
        on |λ|(a, b)
            a & b
        end |λ|
    end script

    if length of xs > 0 and class of (item 1 of xs) is string then
        set empty to ""
    else
        set empty to {}
    end if
    foldl(append, empty, xs)
end concat

--  drop :: Int -> a -> a
on drop(n, a)
    if n < length of a then
        if class of a is text then
            text (n + 1) thru -1 of a
        else
            items (n + 1) thru -1 of a
        end if
    else
        {}
    end if
end drop

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

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

-- length :: [a] -> Int
on |length|(xs)
    length of xs
end |length|

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

{{Out}}

```AppleScript
{{1, 3}, {2, 5}}
```



## AutoHotkey


```AutoHotkey
TwoSum(a, target){
	i := 1,	j := a.MaxIndex()
	while(i < j){
		if  (a[i] + a[j] = target)
			return i ", " j
		else if (a[i] + a[j] <  target)
			i++
		else if (a[i] + a[j] >  target)
			j--
	}
	return "not found"
}
```

Examples:
```AutoHotkey
MsgBox % TwoSum([0, 2, 11, 19, 90], 21) ; returns 2, 4 (first index is 1 not 0)
```

Outputs:
```txt
2,4
```



## AWK


```AWK

# syntax: GAWK -f TWO_SUM.AWK
BEGIN {
    numbers = "0,2,11,19,90"
    print(two_sum(numbers,21))
    print(two_sum(numbers,25))
    exit(0)
}
function two_sum(numbers,sum,  arr,i,j,s) {
    i = 1
    j = split(numbers,arr,",")
    while (i < j) {
      s = arr[i] + arr[j]
      if (s == sum) {
        return(sprintf("[%d,%d]",i,j))
      }
      else if (s < sum) {
        i++
      }
      else {
        j--
      }
    }
    return("[]")
}

```

{{out}}

```txt

[2,4]
[]

```



## Befunge


```befunge>
000pv
     >&:0\`#v_00g:1+00p6p
v           >$&50p110p020p
v>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>+50g-!#v_48*10g8p10g1+:00g1-`v  >
v                                                     >10g20g..@           v_v
"""""""""""""""""""""""""""""""""""""""""""""""v ">"p4\"v"p02:+1p4\">":g02$< :
                                               >  20g8p20g1+:00g1-`#v_0     ^1
"""""""""""""""""""""""""""""""""""""""""""""""                     "        0
>^                                                                  l        p
                                                                    i        "
                                                                    a        ^
                                                                    F        "
                                                                    "        \
                                                                    >:#,_@   8
                                                                             p
                                                                             > ^
```

There are a couple of caveats due to limitations of the language. The target cannot be above 127, there can be no more than 47 elements in the list and the list must be delimited by a negative number before the target value as follows:

```txt

0 2 11 19 90 -1 21

```


{{out}}

```txt
1 3
```



## C


```C

#include<stdio.h>

int main()
{
	int arr[5] = {0, 2, 11, 19, 90},sum = 21,i,j,check = 0;

	for(i=0;i<4;i++){
		for(j=i+1;j<5;j++){
			if(arr[i]+arr[j]==sum){
				printf("[%d,%d]",i,j);
				check = 1;
				break;
			}
		}
	}

	if(check==0)
		printf("[]");

	return 0;
}

```

Output :

```txt

[1,3]

```



## C++

{{trans|C#}}

```cpp
#include <iostream>
#include <map>
#include <tuple>
#include <vector>

using namespace std;

pair<int, int> twoSum(vector<int> numbers, int sum) {
	auto m = map<int, int>();
	for (size_t i = 0; i < numbers.size(); ++i) {
		// see if the complement is stored
		auto key = sum - numbers[i];

		if (m.find(key) != m.end()) {
			return make_pair(m[key], i);
		}
		m[numbers[i]] = i;
	}

	return make_pair(-1, -1);
}

int main() {
	auto numbers = vector<int>{ 0, 2, 11, 19, 90 };
	const int sum = 21;

	auto ts = twoSum(numbers, sum);
	if (ts.first != -1) {
		cout << "{" << ts.first << ", " << ts.second << "}" << endl;
	} else {
		cout << "no result" << endl;
	}

	return 0;
}
```

{{out}}

```txt
{1,3}
```


## C#

```c#
using System;
using System.Collections.Generic;

public class Program
{
    public static void Main(string[] args)
    {
        int[] arr = { 0, 2, 11, 19, 90 };
        const int sum = 21;

        var ts = TwoSum(arr, sum);
        Console.WriteLine(ts != null ? $"{ts[0]}, {ts[1]}" : "no result");

        Console.ReadLine();
    }

    public static int[] TwoSum(int[] numbers, int sum)
    {
        var map = new Dictionary<int, int>();
        for (int i = 0; i < numbers.Length; i++)
        {
            // see if the complement is stored
            var key = sum - numbers[i];
            if (map.ContainsKey(key))
            {
                return new[] { map[key], i };
            }
            map.Add(numbers[i], i);
        }
        return null;
    }
}

```

{{out}}

```txt
1, 3
```



## D


```D
import std.stdio;

void main() {
    const arr = [0, 2, 11, 19, 90];
    const sum = 21;

    writeln(arr.twoSum(21));
}

/**
 * Searches arr for two indexes whose value adds to sum, and returns those indexes.
 * Returns an empty array if no such indexes exist.
 * The values of arr are assumed to be sorted.
 */
int[] twoSum(const int[] arr, const int sum) in {
    import std.algorithm.sorting : isSorted;
    assert(arr.isSorted);
} out(result) {
    assert(result.length == 0 || arr[result[0]] + arr[result[1]] == sum);
} body {
    int i=0;
    int j=arr.length-1;

    while (i <= j) {
        auto temp = arr[i] + arr[j];
        if (temp == sum) {
            return [i, j];
        }

        if (temp < sum) {
            i++;
        } else {
            j--;
        }
    }

    return [];
}
```

{{out}}

```txt
[1, 3]
```



## Dart

<lang>
main() {
  var a = [1,2,3,4,5];
  var s=25,c=0;
  var z=(a.length*(a.length-1))/2;
   for (var x = 0; x < a.length; x++) {
   print(a[x]);
   }
 for (var x = 0; x < a.length; x++) {
    for(var y=x+1;y< a.length; y++)
    {
      if(a[x]+a[y]==s)
      {
        print([a[x],a[y]]);
        break;
      }
      else
      {
       c++;
      }
    }
 }
if(c==z)
{
 print("such pair doesn't exist");
}
}



```



## Elixir


```elixir
defmodule RC do
  def two_sum(numbers, sum) do
    Enum.with_index(numbers) |>
    Enum.reduce_while([], fn {x,i},acc ->
      y = sum - x
      case Enum.find_index(numbers, &(&1 == y)) do
        nil -> {:cont, acc}
        j   -> {:halt, [i,j]}
      end
    end)
  end
end

numbers = [0, 2, 11, 19, 90]
IO.inspect RC.two_sum(numbers, 21)
IO.inspect RC.two_sum(numbers, 25)
```


{{out}}

```txt

[1, 3]
[]

```


=={{header|F_Sharp|F#}}==

```fsharp

// Two Sum : Nigel Galloway December 5th., 2017
let fN n i =
  let rec fN n e =
    match n with
    |n::g when n < i -> match List.mapi(fun g i-> (n,i,g)) g |> List.tryFind(fun (n,g,l)->(n+g)=i) with
                        |Some (n,g,l) -> [e;e+l+1]
                        |_            -> fN g (e+1)
    |_               -> []
  fN n 0
printfn "%A" (fN [0; 2; 11; 19; 90] 21)

```

{{out}}

```txt

[1; 3]

```



## Factor


```factor
USING: combinators fry kernel locals math prettyprint sequences ;
IN: rosetta-code.two-sum

:: two-sum ( seq target -- index-pair )
    0 seq length 1 - :> ( x! y! ) [
        x y [ seq nth ] bi@ + :> sum {
            { [ sum target = x y = or ] [ f ] }
            { [ sum target > ] [ y 1 - y! t ] }
            [ x 1 + x! t ]
        } cond
    ] loop
    x y = { } { x y } ? ;

{ 21 55 11 } [ '[ { 0 2 11 19 90 } _ two-sum . ] call ] each
```

{{out}}

```txt

{ 1 3 }
{ }
{ 0 2 }

```



## Forth

{{works with|Gforth|0.7.3}}

```forth
CREATE A CELL ALLOT
: A[] ( n -- A[n]) CELLS A @ + @ ;
:NONAME   1- ;
:NONAME   R> DROP R> DROP TRUE ;
:NONAME   SWAP 1+ SWAP ;
CREATE VTABLE , , ,
: CMP ( n n' -- -1|0|1)  - DUP IF DUP ABS / THEN ;
: (TWOSUM) ( addr n n' -- u1 u2 t | f)
   >R SWAP A !  0 SWAP 1-  ( lo hi) ( R: n')
   BEGIN OVER OVER < WHILE
     OVER A[]  OVER A[]  + R@
     CMP  1+ CELLS VTABLE + @ EXECUTE
   REPEAT
   DROP DROP R> DROP FALSE ;
: TWOSUM ( addr n n' --)  [CHAR] [ EMIT
   (TWOSUM) IF SWAP 0 .R [CHAR] , EMIT SPACE 0 .R THEN
   [CHAR] ] EMIT ;
CREATE TEST0  0 ,  2 , 11 , 19 , 90 ,            DOES> 5 ;
CREATE TEST1 -8 , -2 ,  0 ,  1 ,  5 ,  8 , 11 ,  DOES> 7 ;
TEST0 21 TWOSUM CR
TEST0 25 TWOSUM CR
TEST1 3  TWOSUM CR
TEST1 8  TWOSUM CR
BYE
```

{{out}}

```txt
[1, 3]
[]
[0, 6]
[2, 5]
```



## Fortran


```fortran
program twosum
  implicit none

  integer, parameter, dimension(5) :: list = (/ 0, 2, 11, 19, 90/)
  integer, parameter :: target_val = 21
  integer :: nelem
  integer :: i, j
  logical :: success = .false.

  nelem = size(list)
  outer:do i = 1,nelem
     do j = i+1,nelem
        success = list(i) + list(j) == target_val
        if (success) exit outer
     end do
  end do outer

  if (success) then
     !Just some fancy formatting for nicer output
     print('("(",2(i3.1,1X),")",3(A1,i3.1))'), i,j, ":", list(i), "+", list(j), "=", target_val
  else
     print*, "Failed"
  end if

end program twosum

```


{{out}}

```txt
(  2   4 ):  2+ 19= 21
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' "a" is the array of sorted non-negative integers
' "b" is the array to contain the result and is assumed to be empty initially

Sub twoSum (a() As UInteger, b() As Integer, targetSum As UInteger)
  Dim lb As Integer = LBound(a)
  Dim ub As Integer = UBound(a)
  If ub = -1 Then Return  '' empty array
  Dim sum As UInteger

  For i As Integer = lb To ub - 1
    If a(i) <= targetSum Then
      For j As Integer = i + 1 To ub
        sum = a(i) + a(j)
        If sum = targetSum Then
          Redim b(0 To 1)
          b(0) = i : b(1) = j
          Return
        ElseIf sum > targetSum Then
          Exit For
        End If
      Next j
    Else
      Exit For
    End If
  Next i
End Sub

Dim a(0 To 4) As UInteger = {0, 2, 11, 19, 90}
Dim b() As Integer
Dim targetSum As UInteger = 21
twoSum a(), b(), targetSum
If UBound(b) = -1 Then
  Print "No two numbers were found whose sum is "; targetSum
Else
  Print "The numbers with indices"; b(LBound(b)); " and"; b(UBound(b)); " sum to "; targetSum
End If
Print
Print "Press any number to quit"
Sleep
```


{{out}}

```txt

The numbers with indices 1 and 3 sum to 21

```


## Go

{{trans|Kotlin}}

```go
package main

import "fmt"

func twoSum(a []int, targetSum int) (int, int, bool) {
    len := len(a)
    if len < 2 {
        return 0, 0, false
    }
    for i := 0; i < len - 1; i++ {
        if a[i] <= targetSum {
            for j := i + 1; j < len; j++ {
                sum := a[i] + a[j]
                if sum == targetSum {
                    return i, j, true
                }
                if sum > targetSum {
                    break
                }
            }
        } else {
            break
        }
    }
    return 0, 0, false
}

func main() {
    a := []int {0, 2, 11, 19, 90}
    targetSum := 21
    p1, p2, ok := twoSum(a, targetSum)
    if (!ok) {
        fmt.Println("No two numbers were found whose sum is", targetSum)
    } else {
        fmt.Println("The numbers with indices", p1, "and", p2, "sum to", targetSum)
    }
}
```


{{out}}

```txt

The numbers with indices 1 and 3 sum to 21

```



## Haskell


### =Returning first match=


```Haskell
twoSum::(Num a,Ord a) => a -> [a] -> [Int]
twoSum num list = sol ls (reverse ls)
  where
  ls = zip list [0..]
  sol [] _ = []
  sol _ [] = []
  sol xs@((x,i):us) ys@((y,j):vs) = ans
    where
    s = x + y
    ans | s == num  = [i,j]
        | j <= i    = []
        | s < num   = sol (dropWhile ((<num).(+y).fst) us) ys
        | otherwise = sol xs $ dropWhile ((num <).(+x).fst) vs

main = print $ twoSum 21 [0, 2, 11, 19, 90]
```

{{out}}

```txt
[1,3]
```



### =Returning all matches=

Listing multiple solutions (as zero-based indices) where they exist:


```haskell
sumTo :: Int -> [Int] -> [(Int, Int)]
sumTo n ns =
  let ixs = zip [0 ..] ns
  in ixs >>=
     (\(i, x) ->
         drop (i + 1) ixs >>=
         \(j, y) ->
            [ (i, j)
            | (x + y) == n ])

main :: IO ()
main = mapM_ print $ sumTo 21 [0, 2, 11, 19, 90, 10]
```


Or, resugaring a little – pulling more into the scope of the list comprehension:

```Haskell
sumTo :: Int -> [Int] -> [(Int, Int)]
sumTo n ns =
  let ixs = zip [0 ..] ns
  in [ (i, j)
     | (i, x) <- ixs
     , (j, y) <- drop (i + 1) ixs
     , (x + y) == n ]

main :: IO ()
main = mapM_ print $ sumTo 21 [0, 2, 11, 19, 90, 10]
```

{{Out}}

```txt
(1,3)
(2,5)
```


=={{header|Icon}} and {{header|Unicon}}==
{{Trans|Lua}}
Icon and Unicon are ordinal languages, first index is one.

<tt>fullimag</tt> library used to pretty print lists.


```unicon
#
# twosum.icn, find two array elements that add up to a given sum
# Dedicated to the public domain
#
link fullimag
procedure main(arglist)
    sum := pop(arglist) | 21
    L := []
    if *arglist > 0 then every put(L, integer(!arglist)) & L := sort(L)
    else L := [0, 2, 11, 19, 90]

    write(sum)
    write(fullimage(L))
    write(fullimage(twosum(sum, L)))
end

# assume sorted list, only interested in zero or one solution
procedure twosum(sum, L)
    i := 1
    j := *L
    while i < j do {
        try := L[i] + L[j]
        if try = sum then return [i,j]
        else
            if try < sum then
                i +:= 1
            else
                j -:= 1
    }
    return []
end
```


{{out}}

```txt
$ unicon -s twosum.icn -x
21
[0,2,11,19,90]
[2,4]

```



## J


So, first off, our basic approach will be to find the sums:

```J
   =+/~0 2 11 19 90
 0  2  11  19  90
 2  4  13  21  92
11 13  22  30 101
19 21  30  38 109
90 92 101 109 180
```


And, check if any of them are our desired value:

```J
   21=+/~0 2 11 19 90
0 0 0 0 0
0 0 0 1 0
0 0 0 0 0
0 1 0 0 0
0 0 0 0 0
```


Except, we want indices here, so let's toss the structure so we can get those:

```J
   ,21=+/~0 2 11 19 90
0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
   I.,21=+/~0 2 11 19 90
8 16
```


Except, we really needed that structure - in this case, since we had a five by five table, we want to interpret this result as a base five pair of numbers:


```J
   $21=+/~0 2 11 19 90
5 5
   5 5#:I.,21=+/~0 2 11 19 90
1 3
3 1
```


Or, taking advantage of being able to use verbs to represent combining their results, when we use three of them:

```J
   ($ #: I.@,)21=+/~0 2 11 19 90
1 3
3 1
```


But to be more like the other task implementations here, we don't want all the results, we just want zero or one result. We can't just take the first result, though, because that would fill in a 0 0 result if there were none, and 0 0 could have been a valid result which does not make sense for the failure case. So, instead, let's package things up so we can add an empty to the end and take the first of those:


```J
   ($ <@#: I.@,)21=+/~0 2 11 19 90
┌───┬───┐
│1 3│3 1│
└───┴───┘
   a:,~($ <@#: I.@,)21=+/~0 2 11 19 90
┌───┬───┬┐
│1 3│3 1││
└───┴───┴┘
   {.a:,~($ <@#: I.@,)21=+/~0 2 11 19 90
┌───┐
│1 3│
└───┘
   ;{.a:,~($ <@#: I.@,)21=+/~0 2 11 19 90
1 3
```


Finally, let's start pulling our arguments out using that three verbs combining form:


```J
   ;{.a:,~($ <@#: I.@,) 21([ = +/~@])0 2 11 19 90
1 3
   ;{.a:,~21 ($ <@#: I.@,)@([ = +/~@])0 2 11 19 90
1 3
```


a: is not a verb, but we can use a noun as the left verb of three as an implied constant verb whose result is itself:

```J
   ;{. 21 (a:,~ ($ <@#: I.@,)@([ = +/~@]))0 2 11 19 90
1 3
```


And, let's finish the job, give this a name, and test it out:

```J
   twosum=: ;@{.@(a:,~ ($ <@#: I.@,)@([ = +/~@]))
   21 twosum 0 2 11 19 90
1 3
```


Except that looks like a bit of a mess. A lot of the reason for this is that ascii is ugly to look at. (Another issue, though, is that a lot of people are not used to architecting control flow as expressions.)

So... let's do this over again, using a more traditional implementation where we name intermediate results. (We're going to stick with our architecture, though, because changing the architecture to the more traditional approach would change the space/time tradeoff to require more time.)


```J
two_sum=:dyad define
  sums=. +/~ y
  matches=.  x = sums
  sum_inds=. I. , matches
  pair_inds=. ($matches) #: sum_inds
  ; {. a: ,~ <"1 pair_inds
)
```


And, testing:


```J
   21 two_sum 0 2 11 19 90
1 3
```


Or, we could go slightly more traditional and instead of doing that boxing at the end, use an if/else statement:


```J
two_sum=:dyad define
  sums=. +/~ y
  matches=.  x = sums
  sum_inds=. I. , matches
  pair_inds=. ($matches) #: sum_inds
  if. #pair_inds do.
    {.pair_inds
  else.
    i.0
  end.
)
```


Then again, most people don't read J anyways, so maybe just stick with the earlier implementation:


```J
twosum=: ;@{.@(a:,~ ($ <@#: I.@,)@([ = +/~@]))
```


'''Alternative approach'''

An alternative method for identifying and returning non-duplicate indicies of the pairs follows.


```j
   21 (= +/~) 0 2 11 19 90
0 0 0 0 0
0 0 0 1 0
0 0 0 0 0
0 1 0 0 0
0 0 0 0 0
```

The array is symmetrical so we can zero one half to remove duplicate pairs and then retrieve the remaining indicies using sparse array functionality.

```j
zeroLowerTri=: * [: </~ i.@#
getIdx=: 4 $. $.
twosum_alt=: getIdx@zeroLowerTri@(= +/~)
```


Testing ...

```j
   21 twosum_alt 0 2 11 19 90
1 3
```



## Java

{{trans|Lua}}

```java
import java.util.Arrays;

public class TwoSum {

    public static void main(String[] args) {
        long sum = 21;
        int[] arr = {0, 2, 11, 19, 90};

        System.out.println(Arrays.toString(twoSum(arr, sum)));
    }

    public static int[] twoSum(int[] a, long target) {
        int i = 0, j = a.length - 1;
        while (i < j) {
            long sum = a[i] + a[j];
            if (sum == target)
                return new int[]{i, j};
            if (sum < target) i++;
            else j--;
        }
        return null;
    }
}
```


```txt
[1, 3]
```




## JavaScript



### ES5


Nesting concatMap yields the cartesian product of the list with itself, and
functions passed to Array.map() have access to the array index in their second argument.
Returning [] where the y index is lower than or equal to the x index ignores the 'lower triangle'
of the cartesian grid, skipping mirror-image and duplicate number pairs.
Returning [] where a sum condition is not met similarly acts as a filter – all of the empty lists
in the map result are eliminated by the concat.


```JavaScript
(function () {
    var concatMap = function (f, xs) {
        return [].concat.apply([], xs.map(f))
    };

    return function (n, xs) {
        return concatMap(function (x, ix) {
            return concatMap(function (y, iy) {
                return iy <= ix ? [] : x + y === n ? [
                    [ix, iy]
                ] : []
            }, xs)
        }, xs)
    }(21, [0, 2, 11, 19, 90]);
})();

```


{{Out}}

```JavaScript
[[1,3]]
```



### ES6


Composing a solution from generic functions like zip, bind (>>=, or flip concatMap) etc.
{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // SUMTWO ----------------------------------------------------------------

    // sumTwo :: Int -> [Int] -> [(Int, Int)]
    function sumTwo(n, xs) {
        const ixs = zip(enumFromTo(0, length(xs) - 1), xs);
        return bind(ixs,
            ([i, x]) => bind(drop(i + 1, ixs),
                ([j, y]) => (x + y === n) ? [
                    [i, j]
                ] : []
            )
        );
    };

    // GENERIC FUNCTIONS -----------------------------------------------------

    // bind (>>=) :: [a] -> (a -> [b]) -> [b]
    const bind = (xs, f) => [].concat.apply([], xs.map(f));

    // drop :: Int -> [a] -> [a]
    const drop = (n, xs) => xs.slice(n);

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // length :: [a] -> Int
    const length = xs => xs.length;

    // show :: a -> String
    const show = (...x) =>
        JSON.stringify.apply(
            null, x.length > 1 ? [x[0], null, x[1]] : x
        );

    // zip :: [a] -> [b] -> [(a,b)]
    const zip = (xs, ys) =>
        xs.slice(0, Math.min(xs.length, ys.length))
        .map((x, i) => [x, ys[i]]);


    // TEST ------------------------------------------------------------------
    return show(
        sumTwo(21, [0, 2, 11, 19, 90, 10])
    );
})();
```

{{Out}}

```txt
[[1,3],[2,5]]
```



## Jsish

Based on Javascript entry.

```javascript
/* Two Sum, in Jsish */
function twoSum(target, list) {
    var concatMap = function (f, xs) {
        return [].concat.apply([], xs.map(f));
    };

    return function (n, xs) {
        return concatMap(function (x, ix) {
            return concatMap(function (y, iy) {
                return iy <= ix ? [] : x + y === n ? [
                    [ix, iy]
                ] : [];
            }, xs);
        }, xs);
    }(target, list);
}

var list = [0, 2, 11, 19, 90];
;list;
;twoSum(21, list);
;list[twoSum(21, list)[0][0]];
;list[twoSum(21, list)[0][1]];
```


{{out}}

```txt
prompt$ jsish --U twoSum.jsi
list ==> [ 0, 2, 11, 19, 90 ]
twoSum(21, list) ==> [ [ 1, 3 ] ]
list[twoSum(21, list)[0][0]] ==> 2
list[twoSum(21, list)[0][1]] ==> 19
```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}

```julia
function twosum(v::Vector, s)
    i = 1
    j = length(v)
    while i < j
        if v[i] + v[j] == s
            return [i, j]
        elseif v[i] + v[j] < s
            i += 1
        else
            j -= 1
        end
    end
    return similar(v, 0)
end

@show twosum([0, 2, 11, 19, 90], 21)
```


{{out}}

```txt
twosum([0, 2, 11, 19, 90], 21) = [2, 4]
```



## Kotlin


```scala
// version 1.1

fun twoSum(a: IntArray, targetSum: Int): Pair<Int, Int>? {
    if (a.size < 2) return null
    var sum: Int
    for (i in 0..a.size - 2) {
        if (a[i] <= targetSum) {
            for (j in i + 1..a.size - 1) {
                sum = a[i] + a[j]
                if (sum == targetSum) return Pair(i, j)
                if (sum > targetSum) break
            }
        } else {
            break
        }
    }
    return null
}

fun main(args: Array<String>) {
    val a = intArrayOf(0, 2, 11, 19, 90)
    val targetSum = 21
    val p = twoSum(a, targetSum)
    if (p == null) {
        println("No two numbers were found whose sum is $targetSum")
    } else {
        println("The numbers with indices ${p.first} and ${p.second} sum to $targetSum")
    }
}
```


{{out}}

```txt

The numbers with indices 1 and 3 sum to 21

```



## Lua

Lua uses one-based indexing.

```lua
function twoSum (numbers, sum)
    local i, j, s = 1, #numbers
    while i < j do
        s = numbers[i] + numbers[j]
        if s == sum then
            return {i, j}
        elseif s < sum then
            i = i + 1
        else
            j = j - 1
        end
    end
    return {}
end

print(table.concat(twoSum({0,2,11,19,90}, 21), ","))
```

{{out}}

```txt
2,4
```



## Maple


```Maple
two_sum := proc(arr, sum)
	local i,j,temp:
	i,j := 1,numelems(arr):
	while (i < j) do
		temp := arr[i] + arr[j]:
		if temp = sum then
			return [i,j]:
		elif temp < sum then
			i := i + 1:
		else
			j := j-1:
		end if:
	end do:
	return []:
end proc:
L := Array([0,2,2,11,19,19,90]);
two_sum(L, 21);
```

{{Out|Output}}
Note that Maple does 1 based indexing.

```txt

[2,5]

```



## MiniScript


```MiniScript
twoSum = function(numbers, sum)
    // Make a map of values to their indices in the numbers array
    // as we go, so we will know when we've found a match.
    map = {}
    for i in numbers.indexes
        key = sum - numbers[i]
        if map.hasIndex(key) then return [map[key], i]
        map[numbers[i]] = i
    end for
end function

print twoSum([0, 2, 11, 19, 90], 21)
```


Output:

```txt
[1, 3]
```


=={{header|Modula-2}}==

```modula2
MODULE TwoSum;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,ReadChar;

TYPE
    Pair = RECORD
        f,s : INTEGER;
    END;

PROCEDURE TwoSum(CONST arr : ARRAY OF INTEGER; CONST sum : INTEGER) : Pair;
VAR i,j,temp : INTEGER;
BEGIN
    i := 0;
    j := HIGH(arr)-1;

    WHILE i<=j DO
        temp := arr[i] + arr[j];
        IF temp=sum THEN
            RETURN Pair{i,j};
        END;
        IF temp<sum THEN
            INC(i);
        ELSE
            DEC(j);
        END;
    END;

    RETURN Pair{-1,-1};
END TwoSum;

VAR
    buf : ARRAY[0..63] OF CHAR;
    arr : ARRAY[0..4] OF INTEGER;
    res : Pair;
BEGIN
    arr[0]:=0;
    arr[1]:=2;
    arr[2]:=11;
    arr[3]:=19;
    arr[4]:=90;

    res := TwoSum(arr, 21);
    FormatString("[%i, %i]\n", buf, res.f, res.s);
    WriteString(buf);
    ReadChar;
END TwoSum.
```



## Nim


```nim
proc twoSum (src : openarray[int], target : int ) : array[2, int] =
  if src.len < 2:
    return

  for base in 0 .. (src.len - 2):
    for ext in (base + 1) .. < src.len:
      if (src[base] + src[ext]) == target:
        result[0] = base
        result[1] = ext


proc main =
  var data0 = [0, 2, 11, 19, 90]
  var res = twoSum(data0, 21)
  assert(res == [1, 3])

  var data1 = [0, 2, 11, 19, 90]
  res = twoSum(data1, 22)
  assert(res == [0, 0])

  var data2 = [1]
  res = twoSum(data2, 22)
  assert(res == [0, 0])

  var data3 = [1, 99]
  res = twoSum(data3, 100)
  assert(res == [0, 1])

  var data4 = [1, 99]
  res = twoSum(data4, 101)
  assert(res == [0, 0])


main()
```



## Objeck

{{trans|Java}}

```objeck
class TwoSum {
  function : Main(args : String[]) ~ Nil {
    sum := 21;
    arr := [0, 2, 11, 19, 90];
    Print(TwoSum(arr, sum));
  }

  function : TwoSum(a : Int[], target : Int) ~ Int[] {
    i := 0;
    j := a->Size() - 1;

    while (i < j) {
      sum := a[i] + a[j];
      if(sum = target) {
        r := Int->New[2];
        r[0] := i;
        r[1] := j;
        return r;
      };

      if (sum < target) {
        i++;
      }
      else {
        j--;
      };
    };

    return Nil;
  }

  function : Print(r : Int[]) ~ Nil {
    '['->Print();
    each(i : r) {
      r[i]->Print();
      if(i + 1 < r->Size()) {
        ','->Print();
      };
    };
    ']'->PrintLine();
  }
}

```


Output:

```txt

[1,3]

```




## OCaml

{{trans|C}}


```ocaml
let get_sums ~numbers ~sum =
  let n = Array.length numbers in
  let res = ref [] in
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      if numbers.(i) + numbers.(j) = sum then
        res := (i, j) :: !res
    done
  done;
  !res


let () =
  let numbers = [| 0; 2; 11; 19; 90 |]
  and sum = 21
  in
  let res = get_sums ~numbers ~sum in

  List.iter (fun (i, j) ->
    Printf.printf "# Found: %d %d\n" i j
  ) res
```


Will return all possible sums, not just the first one found.

{{out}}

```txt

$ ocaml two_sum.ml
# Found: 1 3

```



## ooRexx


```oorexx
a=.array~of( -5, 26, 0, 2, 11, 19, 90)
x=21
n=0
do i=1 To a~items
  Do j=i+1 To a~items
    If a[i]+a[j]=x Then Do
      Say '['||i-1||','||j-1||']'
      n=n+1
      End
    End
  End
If n=0 Then
  Say '[] - no items found'
```

{{out}}

```txt
[0,1]
[3,5]
```



## Pascal

A little bit lengthy. Implemented an unsorted Version with quadratic runtime too and an extra test case with 83667 elements that needs 83667*86666/2 ~ 3.5 billion checks ( ~1 cpu-cycles/check, only if data in cache ).

```pascal
program twosum;
{$IFDEF FPC}{$MODE DELPHI}{$ELSE}{$APPTYPE CONSOLE}{$ENDIF}
uses
  sysutils;
type
  tSolRec = record
              SolRecI,
              SolRecJ : NativeInt;
            end;
  tMyArray = array of NativeInt;
const
// just a gag using unusual index limits
  ConstArray :array[-17..-13] of NativeInt = (0, 2, 11, 19, 90);

function Check2SumUnSorted(const A  :tMyArray;
                                 sum:NativeInt;
                           var   Sol:tSolRec):boolean;
//Check every possible sum A[max] + A[max-1..0]
//than A[max-1] + A[max-2..0] etc pp.
//quadratic runtime: maximal  (max-1)*max/ 2 checks
//High(A) always checked for dynamic array, even const
//therefore run High(A) to low(A), which is always 0 for dynamic array
label
  SolFound;
var
  i,j,tmpSum: NativeInt;
Begin
  Sol.SolRecI:=0;
  Sol.SolRecJ:=0;
  i := High(A);
  while i > low(A) do
  Begin
    tmpSum := sum-A[i];
    j := i-1;
    while j >= low(A) do
    begin
      //Goto is bad, but fast...
      if tmpSum = a[j] Then
        GOTO SolFound;
      dec(j);
    end;
    dec(i);
  end;
  result := false;
  exit;
SolFound:
  Sol.SolRecI:=j;Sol.SolRecJ:=i;
  result := true;
end;

function Check2SumSorted(const  A  :tMyArray;
                                sum:NativeInt;
                         var    Sol:tSolRec):boolean;
var
  i,j,tmpSum: NativeInt;
Begin
  Sol.SolRecI:=0;
  Sol.SolRecJ:=0;
  i := low(A);
  j := High(A);
  while(i < j) do
  Begin
    tmpSum := a[i] + a[j];
    if tmpSum = sum then
    Begin
      Sol.SolRecI:=i;Sol.SolRecJ:=j;
      result := true;
      EXIT;
    end;
    if tmpSum < sum then
    begin
      inc(i);
      continue;
    end;
    //if tmpSum > sum then
    dec(j);
  end;
  writeln(i:10,j:10);
  result := false;
end;

var
  Sol :tSolRec;
  CheckArr : tMyArray;
  MySum,i : NativeInt;

Begin
  randomize;
  setlength(CheckArr,High(ConstArray)-Low(ConstArray)+1);
  For i := High(CheckArr) downto low(CheckArr) do
    CheckArr[i] := ConstArray[i+low(ConstArray)];

  MySum  := 21;
  IF Check2SumSorted(CheckArr,MySum,Sol) then
    writeln('[',Sol.SolRecI,',',Sol.SolRecJ,'] sum to ',MySum)
  else
    writeln('No solution found');

  //now test a bigger sorted array..
  setlength(CheckArr,83667);
  For i := High(CheckArr) downto 0 do
    CheckArr[i] := i;
  MySum := CheckArr[Low(CheckArr)]+CheckArr[Low(CheckArr)+1];
  writeln(#13#10,'Now checking array of ',length(CheckArr),
          ' elements',#13#10);
  //runtime about 1 second
  IF Check2SumUnSorted(CheckArr,MySum,Sol) then
    writeln('[',Sol.SolRecI,',',Sol.SolRecJ,'] sum to ',MySum)
  else
    writeln('No solution found');
  //runtime not measurable
  IF Check2SumSorted(CheckArr,MySum,Sol) then
    writeln('[',Sol.SolRecI,',',Sol.SolRecJ,'] sum to ',MySum)
  else
    writeln('No solution found');
end.
```

{{out}}

```txt

[1,3] sum to 21

Now checking array of 83667 elements

[0,1] sum to 1
[0,1] sum to 1

real    0m1.013s
```



## Perl

{{trans|Python}}

```perl
use strict;
use warnings;
use feature 'say';

sub two_sum{
  my($sum,@numbers) = @_;
  my $i = 0;
  my $j = $#numbers - 1;
  my @indices;
  while ($i < $j) {
    if    ($numbers[$i] + $numbers[$j] == $sum) { push @indices, ($i, $j); $i++; }
    elsif ($numbers[$i] + $numbers[$j]  < $sum) { $i++ }
    else                                        { $j-- }
  }
  return @indices
}

my @numbers = <0 2 11 19 90>;
my @indices = two_sum(21, @numbers);
say join(', ', @indices) || 'No match';

@indices = two_sum(25, @numbers);
say join(', ', @indices) || 'No match';
```

{{out}}

```txt
1, 3
No match
```



## Perl 6



### Procedural

{{trans|zkl}}

```perl6
sub two_sum ( @numbers, $sum ) {
    die '@numbers is not sorted' unless [<=] @numbers;

    my ( $i, $j ) = 0, @numbers.end;
    while $i < $j {
        given $sum <=> @numbers[$i,$j].sum {
            when Order::More { $i += 1 }
            when Order::Less { $j -= 1 }
            when Order::Same { return $i, $j }
        }
    }
    return;
}

say two_sum ( 0, 2, 11, 19, 90 ), 21;
say two_sum ( 0, 2, 11, 19, 90 ), 25;
```

{{out}}

```txt
(1 3)
Nil
```



### Functional

The two versions differ only in how one 'reads' the notional flow of processing: left-to-right versus right-to-left.
Both return all pairs that sum to the target value, not just the first (e.g. for input of <code>0 2 10 11 19 90</code> would get indices 1/4 and 2/3).

```perl6
sub two-sum-lr (@a, $sum) {
  # (((^@a X ^@a) Z=> (@a X+ @a)).grep($sum == *.value)>>.keys.map:{ .split(' ').sort.join(' ')}).unique
    (
     (
      (^@a X ^@a) Z=> (@a X+ @a)
     ).grep($sum == *.value)>>.keys
     .map:{ .split(' ').sort.join(' ')}
    ).unique
}

sub two-sum-rl (@a, $sum) {
  # unique map {.split(' ').sort.join(' ')}, keys %(grep {.value == $sum}, ((^@a X ^@a) Z=> (@a X+ @a)))
    unique
    map {.split(' ').sort.join(' ')},
    keys %(
     grep {.value == $sum}, (
      (^@a X ^@a) Z=> (@a X+ @a)
     )
    )
}

my @a = <0 2 11 19 90>;
for 21, 25 {
    say two-sum-rl(@a, $_);
    say two-sum-lr(@a, $_);
}
```

{{out}}

```txt
(1 3)
(1 3)
()
()
```



## Phix


```Phix
function twosum(sequence s, integer t)
    for i=1 to length(s) do
        for j=i+1 to length(s) do
            if s[i]+s[j]=t then
                return {i,j}
            end if
        end for
    end for
    return {}
end function
?twosum({0, 2, 11, 19, 90},21)
```

{{trans|Perl 6}}

```Phix
function twosum(sequence numbers, integer total)
integer i=1, j=length(numbers)
    while i<j do
        switch compare(numbers[i]+numbers[j],total) do
            case -1: i += 1
            case  0: return {i, j}
            case +1: j -= 1
        end switch
    end while
    return {}
end function
```

{{Out}}
Phix uses 1-based indexes

```txt

{2,4}

```



## PicoLisp


```PicoLisp
(de twosum (Lst N)
   (for  ((I . A) Lst  A  (cdr A))
      (T
         (for ((J . B) (cdr Lst)  B  (cdr B))
            (T (= N (+ (car A) (car B)))
               (cons I (inc J)) ) )
         @ ) ) )
(println
   (twosum (0 2 11 19 90) 21)
   (twosum (-3 -2 0 1 5 8 11) 17)
   (twosum (-8 -2 -1 1 5 9 11) 0) )
```

{{out}}

```txt
(2 . 4) NIL (3 . 4)
```



## PowerShell

Lazy, '''very''' lazy.

```PowerShell

$numbers = @(0, 2, 11, 19, 90)
$sum = 21

$totals = for ($i = 0; $i -lt $numbers.Count; $i++)
{
    for ($j = $numbers.Count-1; $j -ge 0; $j--)
    {
        [PSCustomObject]@{
            FirstIndex  = $i
            SecondIndex = $j
            TargetSum   = $numbers[$i] + $numbers[$j]
        }
    }
}

$totals | Where-Object TargetSum -EQ $sum |
          Select-Object -First 1 `
                        -Property @{
                                        Name       = "Sum"
                                        Expression = { $_.TargetSum }
                                  },
                                  @{
                                        Name       = "Indices"
                                        Expression = { @($_.FirstIndex, $_.SecondIndex) }
                                  }

```

{{out}}

```txt

Sum Indices
--- -------
 21 {1, 3}

```



## Python

{{trans|Perl 6}}

```python
def two_sum(arr, num):
    i = 0
    j = len(arr) - 1
    while i < j:
        if arr[i] + arr[j] == num:
            return (i, j)
        if arr[i] + arr[j] < num:
            i += 1
        else:
            j -= 1
    return None


numbers = [0, 2, 11, 19, 90]
print(two_sum(numbers, 21))
print(two_sum(numbers, 25))
```


or, in terms of '''itertools.product''':
{{Works with|Python|3.7}}

```python
'''Finding two integers that sum to a target value.'''

from itertools import (product)


# sumTwo :: [Int] -> Int -> [(Int, Int)]
def sumTwo(xs):
    '''All the pairs of integers in xs which
       sum to n.
    '''
    def go(n):
        ixs = list(enumerate(xs))
        return [
            (fst(x), fst(y)) for (x, y) in (
                product(ixs, ixs[1:])
            ) if fst(x) < fst(y) and n == snd(x) + snd(y)
        ]
    return lambda n: go(n)


# TEST ----------------------------------------------------

# main :: IO ()
def main():
    '''Tests'''

    xs = [0, 2, 11, 19, 90, 10]

    print(
        fTable(
            'The indices of any two integers drawn from ' + repr(xs) +
            '\nthat sum to a given value:\n'
        )(str)(
            lambda x: str(x) + ' = ' + ', '.join(
                ['(' + str(xs[a]) + ' + ' + str(xs[b]) + ')' for a, b in x]
            ) if x else '(none)'
        )(
            sumTwo(xs)
        )(enumFromTo(10)(25))
    )


# GENERIC -------------------------------------------------

# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# DISPLAY -------------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
The indices of any two integers drawn from [0, 2, 11, 19, 90, 10]
that sum to a given value:

10 -> [(0, 5)] = (0 + 10)
11 -> [(0, 2)] = (0 + 11)
12 -> [(1, 5)] = (2 + 10)
13 -> [(1, 2)] = (2 + 11)
14 -> (none)
15 -> (none)
16 -> (none)
17 -> (none)
18 -> (none)
19 -> [(0, 3)] = (0 + 19)
20 -> (none)
21 -> [(1, 3), (2, 5)] = (2 + 19), (11 + 10)
22 -> (none)
23 -> (none)
24 -> (none)
25 -> (none)
```



or, a little more parsimoniously (not generating the entire cartesian product), in terms of '''concatMap''':
{{Works with|Python|3.7}}

```python
'''Finding two integers that sum to a target value.'''

from itertools import chain


# sumTwo :: Int -> [Int] -> [(Int, Int)]
def sumTwo(n, xs):
    '''All the pairs of integers in xs which
       sum to n.
    '''
    def go(vs):
        return [vs[0]] if n == sum(vs[1]) else []
    ixs = list(enumerate(xs))
    return list(
        bind(ixs)(
            lambda ix: bind(ixs[ix[0]:])(
                lambda jy: go(tuple(zip(*(ix, jy))))
            )
        )
    )


# TEST ----------------------------------------------------

# main :: IO ()
def main():
    '''Tests'''

    for n in [21, 25]:
        print(
            sumTwo(n, [0, 2, 11, 19, 90, 10])
        )


# GENERIC -------------------------------------------------

# bind (>>=) :: [a] -> (a -> [b]) -> [b]
def bind(xs):
    '''List monad injection operator.
       Two computations sequentially composed,
       with any value produced by the first
       passed as an argument to the second.
    '''
    return lambda f: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
[(1, 3), (2, 5)]
[]
```



## Racket


```racket
#lang racket/base
(define (two-sum v m)
  (let inr ((l 0) (r (sub1 (vector-length v))))
    (and
     (not (= l r))
     (let ((s (+ (vector-ref v l) (vector-ref v r))))
       (cond [(= s m) (list l r)] [(> s m) (inr l (sub1 r))] [else (inr (add1 l) r)])))))

(module+ test
  (require rackunit)
  ;; test cases
  ;; no output indicates returns are as expected
  (check-equal? (two-sum #( 0  2 11 19 90)      21) '(1 3))
  (check-equal? (two-sum #(-8 -2  0  1  5 8 11)  3) '(0 6))
  (check-equal? (two-sum #(-3 -2  0  1  5 8 11) 17) #f)
  (check-equal? (two-sum #(-8 -2 -1  1  5 9 11)  0) '(2 3)))
```



## REXX


### version 1


```rexx
/* REXX */
list='-5 26 0 2 11 19 90'
Do i=0 By 1 Until list=''
  Parse Var list a.i list
  End
n=i
x=21
z=0
do i=0 To n
  Do j=i+1 To n
    s=a.i+a.j
    If s=x Then Do
      z=z+1
      Say '['i','j']' a.i a.j s
      End
    End
  End
If z=0 Then
  Say '[] - no items found'
Else
  Say z 'solutions found'
```

{{out}}

```txt
[0,1] -5 26 21
[3,5] 2 19 21
2 solutions found
```



### version 2

All solutions are listed  (if any),   along with a count of the number of solutions.

Also, it's mentioned that the indices are zero─based,   and formatted solutions are shown.

The list of numbers can be in any format,   not just integers.   Also, they need not be unique.

The list of integers need   ''not''   be sorted.

A   '''numeric digits 500'''   statement was added just in case some rather large numbers were entered.

No verification was performed to ensure that all items were numeric.

A little extra code was added to have the output columns aligned.

```rexx
/*REXX program finds two numbers in a list of numbers that  sum  to a particular target.*/
numeric digits 500                               /*be able to handle some larger numbers*/
parse arg targ list                              /*obtain optional arguments from the CL*/
if targ='' | targ=","  then targ= 21             /*Not specified?  Then use the defaults*/
if list='' | list=","  then list= 0 2 11 19 90   /* "      "         "   "   "     "    */
say 'the list:       '   list                    /*echo the     list     to the terminal*/
say 'the target sum: '   targ                    /*  "   "   target sum   "  "     "    */
@solution= 'a solution:  zero─based indices   '  /*a SAY literal for space conservation.*/
sol=0;                  w=0                      /*number of solutions found  (so far). */
      do #=0  for words(list); _=word(list, #+1) /*examine the list, construct an array.*/
      @.#= _;           w= max(w, length(_) )    /*assign a number to an indexed array. */
      end  /*#*/                                 /*W:  the maximum width of any number. */
L= length(#)                                     /*L:   "     "      "    "  "  index.  */
say                                              /* [↓] look for sum of 2 numbers=target*/
      do    a=0    for #                         /*scan up to the last number in array. */
         do b=a+1  to  #-1;   if @.a + @.b\=targ  then iterate    /*is sum not correct? */
         sol= sol + 1                            /*bump count of the number of solutions*/
         say @solution        center( "["right(a, L)','       right(b, L)"]",     L+L+5) ,
             right(@.a, w*4)      " + "       right(@.b, w)       ' = '           targ
         end   /*b*/                             /*show the 2 indices and the summation.*/
      end      /*a*/
say
if sol==0  then sol= 'None'                      /*prettify the number of solutions if 0*/
say 'number of solutions found: '   sol          /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default inputs:}}

```txt

the list:        0 2 11 19 90
the target sum:  21

a solution:  zero─based indices    [1, 3]         2  +  19  =  21

number of solutions found:  1

```

{{out|output|text=  when using the input of:     <tt> 21     -78 -5 1 0 -1 -4 11 14 23.5 5 +3 2. 18 -2.50 +2 16 19 018 23 24 25 26 199 2 3 17 +18 19 03 3 .18e2 </tt>}}

```txt

the list:        -78 -5 1 0 -1 -4 11 14 23.5 5 +3 2. 18 -2.50 +2 16 19 018 23 24 25 26 199 2 3 17 +18 19 03 3 .18e2
the target sum:  21

a solution:  zero─based indices    [ 1, 21]                    -5  +     26  =  21
a solution:  zero─based indices    [ 5, 20]                    -4  +     25  =  21
a solution:  zero─based indices    [ 8, 13]                  23.5  +  -2.50  =  21
a solution:  zero─based indices    [ 9, 15]                     5  +     16  =  21
a solution:  zero─based indices    [10, 12]                    +3  +     18  =  21
a solution:  zero─based indices    [10, 17]                    +3  +    018  =  21
a solution:  zero─based indices    [10, 26]                    +3  +    +18  =  21
a solution:  zero─based indices    [10, 30]                    +3  +  .18e2  =  21
a solution:  zero─based indices    [11, 16]                    2.  +     19  =  21
a solution:  zero─based indices    [11, 27]                    2.  +     19  =  21
a solution:  zero─based indices    [12, 24]                    18  +      3  =  21
a solution:  zero─based indices    [12, 28]                    18  +     03  =  21
a solution:  zero─based indices    [12, 29]                    18  +      3  =  21
a solution:  zero─based indices    [14, 16]                    +2  +     19  =  21
a solution:  zero─based indices    [14, 27]                    +2  +     19  =  21
a solution:  zero─based indices    [16, 23]                    19  +      2  =  21
a solution:  zero─based indices    [17, 24]                   018  +      3  =  21
a solution:  zero─based indices    [17, 28]                   018  +     03  =  21
a solution:  zero─based indices    [17, 29]                   018  +      3  =  21
a solution:  zero─based indices    [23, 27]                     2  +     19  =  21
a solution:  zero─based indices    [24, 26]                     3  +    +18  =  21
a solution:  zero─based indices    [24, 30]                     3  +  .18e2  =  21
a solution:  zero─based indices    [26, 28]                   +18  +     03  =  21
a solution:  zero─based indices    [26, 29]                   +18  +      3  =  21
a solution:  zero─based indices    [28, 30]                    03  +  .18e2  =  21
a solution:  zero─based indices    [29, 30]                     3  +  .18e2  =  21

number of solutions found:  26

```



## Ring


```ring

# Project : Two Sum

numbers = [0, 2, 11, 19, 90]
sum = 21

see "order list: "
for n=1 to len(numbers)
    see " " + numbers[n]
next
see " (using a zero index.)" + nl
for n=1 to len(numbers)
    for m=n to len(numbers)
        if numbers[n] + numbers[m] = sum
           see "target sum:  " + sum + nl
           see "a solution: ["
           see  "" + (n-1) + " " + (m-1) + "]" + nl
        ok
    next
next

```

Output:

```txt

order list:  0 2 11 19 90 (using a zero index.)
target sum:  21
a solution: [1 3]

```



## Ruby


```ruby
def two_sum(numbers, sum)
  numbers.each_with_index do |x,i|
    if j = numbers.index(sum - x) then return [i,j] end
  end
  []
end

numbers = [0, 2, 11, 19, 90]
p two_sum(numbers, 21)
p two_sum(numbers, 25)
```


{{out}}

```txt

[1, 3]
[]

```


When the size of the Array is bigger, the following is more suitable.

```ruby
def two_sum(numbers, sum)
  numbers.each_with_index do |x,i|
    key = sum - x
    if j = numbers.bsearch_index{|y| key<=>y}
      return [i,j]
    end
  end
  []
end
```



## Rust


```Rust
use std::cmp::Ordering;
use std::ops::Add;

fn two_sum<T>(arr: &[T], sum: T) -> Option<(usize, usize)>
where
    T: Add<Output = T> + Ord + Copy,
{
    if arr.len() == 0 {
        return None;
    }

    let mut i = 0;
    let mut j = arr.len() - 1;

    while i < j {
        match (arr[i] + arr[j]).cmp(&sum) {
            Ordering::Equal => return Some((i, j)),
            Ordering::Less => i += 1,
            Ordering::Greater => j -= 1,
        }
    }

    None
}

fn main() {
    let arr = [0, 2, 11, 19, 90];
    let sum = 21;

    println!("{:?}", two_sum(&arr, sum));
}
```


{{out}}

```txt

Some((1, 3))

```



## Sidef

{{trans|Perl 6}}

```ruby
func two_sum(numbers, sum) {
    var (i, j) = (0, numbers.end)
    while (i < j) {
        given (sum <=> numbers[i]+numbers[j]) {
            when (-1) { --j }
            when (+1) { ++i }
            default { return [i, j] }
        }
    }
    return []
}

say two_sum([0, 2, 11, 19, 90], 21)
say two_sum([0, 2, 11, 19, 90], 25)
```

{{out}}

```txt

[1, 3]
[]

```


## Scala


```Scala
import java.util

object TwoSum extends App {
  val (sum, arr)= (21, Array(0, 2, 11, 19, 90))
  println(util.Arrays.toString(twoSum(arr, sum)))

  private def twoSum(a: Array[Int], target: Long): Array[Int] = {
    var (i, j) = (0, a.length - 1)
    while (i < j) {
      val sum = a(i) + a(j)
      if (sum == target) return Array[Int](i, j)
      if (sum < target) i += 1 else j -= 1
    }
    null
  }

}
```

{{Out}}See it running in your browser by [https://scalafiddle.io/sf/GxVfCE7/0 ScalaFiddle (JavaScript, non JVM)] or by [https://scastie.scala-lang.org/S5aks2gRTcqcy1VUWJ6GzQ Scastie (JVM)].

## Stata

Notice that array indexes start at 1 in Stata.

```stata
function find(a, x) {
	i = 1
	j = length(a)
	while (i<j) {
		s = a[i]+a[j]
		if (s<x) i++
		else if (s>x) j--
		else return((i,j))
	}
}

find((0,2,11,19,90),21)
       1   2
    +---------+
  1 |  2   4  |
    +---------+
```



## VBA


```vb
Option Explicit
Function two_sum(a As Variant, t As Integer) As Variant
    Dim i, j As Integer
    i = 0
    j = UBound(a)
    Do While (i < j)
        If (a(i) + a(j) = t) Then
            two_sum = Array(i, j)
            Exit Function
        ElseIf (a(i) + a(j) < t) Then i = i + 1
        ElseIf (a(i) + a(j) > t) Then j = j - 1
        End If
    Loop
    two_sum = Array()
End Function
Sub prnt(a As Variant)
    If UBound(a) = 1 Then
        Selection.TypeText Text:="(" & a(0) & ", " & a(1) & ")" & vbCrLf
    Else
        Selection.TypeText Text:="()" & vbCrLf
    End If
End Sub
Sub main()
    Call prnt(two_sum(Array(0, 2, 11, 19, 90), 21))
    Call prnt(two_sum(Array(-8, -2, 0, 1, 5, 8, 11), 3))
    Call prnt(two_sum(Array(-3, -2, 0, 1, 5, 8, 11), 17))
    Call prnt(two_sum(Array(-8, -2, -1, 1, 5, 9, 11), 0))
End Sub
```

{{out}}

```txt

(1, 3)
(0, 6)
()
(2, 3)


```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Function TwoSum(numbers As Integer(), sum As Integer) As Integer()
        Dim map As New Dictionary(Of Integer, Integer)
        For index = 1 To numbers.Length
            Dim i = index - 1
            ' see if the complement is stored
            Dim key = sum - numbers(i)
            If map.ContainsKey(key) Then
                Return {map(key), i}
            End If
            map.Add(numbers(i), i)
        Next
        Return Nothing
    End Function

    Sub Main()
        Dim arr = {0, 2, 1, 19, 90}
        Const sum = 21

        Dim ts = TwoSum(arr, sum)
        Console.WriteLine(If(IsNothing(ts), "no result", $"{ts(0)}, {ts(1)}"))
    End Sub

End Module
```

{{out}}

```txt
1, 3
```



## zkl

The sorted O(n) no external storage solution:

```zkl
fcn twoSum(sum,ns){
   i,j:=0,ns.len()-1;
   while(i<j){
      if((s:=ns[i] + ns[j]) == sum) return(i,j);
      else if(s<sum) i+=1;
      else if(s>sum) j-=1;
   }
}
```


```zkl
twoSum2(21,T(0,2,11,19,90)).println();
twoSum2(25,T(0,2,11,19,90)).println();
```

{{out}}

```txt

L(1,3)
False

```

The unsorted O(n!) all solutions solution:

```zkl
fcn twoSum2(sum,ns){
   Utils.Helpers.combosKW(2,ns).filter('wrap([(a,b)]){ a+b==sum })  // lazy combos
   .apply('wrap([(a,b)]){ return(ns.index(a),ns.index(b)) })
}
```


```zkl
twoSum2(21,T(0,2,11,19,90,21)).println();
twoSum2(25,T(0,2,11,19,90,21)).println();
```

{{out}}

```txt

L(L(0,5),L(1,3))
L()

```

