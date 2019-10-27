+++
title = "Sort numbers lexicographically"
description = ""
date = 2019-08-30T12:51:09Z
aliases = []
[extra]
id = 21919
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Sorting]]

;Task:
Given an integer   '''n''',   return   '''1──►n'''   (inclusive)   in lexicographical order.

Show all output here on this page.

;Example:
Given   '''13''',

return:   '''[1,10,11,12,13,2,3,4,5,6,7,8,9].'''





## Ada


```Ada
WITH Ada.Containers.Generic_Array_Sort, Ada.Text_IO;
USE  Ada.Text_IO;
PROCEDURE Main IS
   TYPE Natural_Array IS ARRAY (Positive RANGE <>) OF Natural;
   FUNCTION Less (L, R : Natural) RETURN Boolean IS (L'Img < R'Img);
   PROCEDURE Sort_Naturals IS NEW Ada.Containers.Generic_Array_Sort
     (Positive, Natural, Natural_Array, Less);
   PROCEDURE Show (Last : Natural) IS
      A : Natural_Array (1 .. Last);
   BEGIN
      FOR I IN A'Range LOOP A (I) := I; END LOOP;
      Sort_Naturals (A);
      FOR I IN  A'Range LOOP Put (A (I)'Img); END LOOP;
      New_Line;
   END Show;
BEGIN
   Show (13);
   Show (21);
END Main;

```


{{out}}

```txt

 1 10 11 12 13 2 3 4 5 6 7 8 9
 1 10 11 12 13 14 15 16 17 18 19 2 20 21 3 4 5 6 7 8 9

```

{{out}}


## AWK


### Robust with checks


```AWK

# syntax: GAWK -f SORT_NUMBERS_LEXICOGRAPHICALLY.AWK
#
# sorting:
#   PROCINFO["sorted_in"] is used by GAWK
#   SORTTYPE is used by Thompson Automation's TAWK
#
BEGIN {
    prn(0)
    prn(1)
    prn(13)
    prn(9,10)
    prn(-11,+11)
    prn(-21)
    prn("",1)
    prn(+1,-1)
    exit(0)
}
function prn(n1,n2) {
    if (n1 <= 0 && n2 == "") {
      n2 = 1
    }
    if (n2 == "") {
      n2 = n1
      n1 = 1
    }
    printf("%d to %d: %s\n",n1,n2,snl(n1,n2))
}
function snl(start,stop,  arr,i,str) {
    if (start == "") {
      return("error: start=blank")
    }
    if (start > stop) {
      return("error: start>stop")
    }
    for (i=start; i<=stop; i++) {
      arr[i]
    }
    PROCINFO["sorted_in"] = "@ind_str_asc" ; SORTTYPE = 2
    for (i in arr) {
      str = sprintf("%s%s ",str,i)
    }
    sub(/ $/,"",str)
    return(str)
}

```

{{out}}

```txt

0 to 1: 0 1
1 to 1: 1
1 to 13: 1 10 11 12 13 2 3 4 5 6 7 8 9
9 to 10: 10 9
-11 to 11: -1 -10 -11 -2 -3 -4 -5 -6 -7 -8 -9 0 1 10 11 2 3 4 5 6 7 8 9
-21 to 1: -1 -10 -11 -12 -13 -14 -15 -16 -17 -18 -19 -2 -20 -21 -3 -4 -5 -6 -7 -8 -9 0 1
0 to 1: error: start=blank
1 to -1: error: start>stop

```

===Alternative, using GAWK's builtin sort===
This version explicitly casts integers as strings during list generation and uses the builtin sort available in GAWK on element values.

```AWK
BEGIN {
    n=13
    for (i=1; i<=n; i++) 
        a[i]=i""
    asort(a)
    for (k in a)
        printf "%d ", a[k]
}
```

{{out}}

```txt

1 10 11 12 13 2 3 4 5 6 7 8 9 
```



## C


```c>#include <math.h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int compareStrings(const void *a, const void *b) {
    const char **aa = (const char **)a;
    const char **bb = (const char **)b;
    return strcmp(*aa, *bb);
}

void lexOrder(int n, int *ints) {
    char **strs;
    int i, first = 1, last = n, k = n, len;
    if (n < 1) {
        first = n; last = 1; k = 2 - n;
    } 
    strs = malloc(k * sizeof(char *));
    for (i = first; i <= last; ++i) {
        if (i >= 1) len = (int)log10(i) + 2;
        else if (i == 0) len = 2;
        else len = (int)log10(-i) + 3; 
        strs[i-first] = malloc(len);
        sprintf(strs[i-first], "%d", i);
    }
    qsort(strs, k, sizeof(char *), compareStrings);
    for (i = 0; i < k; ++i) {
        ints[i] = atoi(strs[i]);
        free(strs[i]);
    }
    free(strs);
}

int main() {
    int i, j, k, n,  *ints;
    int numbers[5] = {0, 5, 13, 21, -22};
    printf("In lexicographical order:\n\n");
    for (i = 0; i < 5; ++i) {
        k = n = numbers[i];
        if (k < 1) k = 2 - k;
        ints = malloc(k * sizeof(int));
        lexOrder(n, ints);
        printf("%3d: [", n);
        for (j = 0; j < k; ++j) {
            printf("%d ", ints[j]);
        }
        printf("\b]\n");
        free(ints);
    }
    return 0;
}
```


{{out}}

```txt

In lexicographical order:

  0: [0 1]
  5: [1 2 3 4 5]
 13: [1 10 11 12 13 2 3 4 5 6 7 8 9]
 21: [1 10 11 12 13 14 15 16 17 18 19 2 20 21 3 4 5 6 7 8 9]
-22: [-1 -10 -11 -12 -13 -14 -15 -16 -17 -18 -19 -2 -20 -21 -22 -3 -4 -5 -6 -7 -8 -9 0 1]

```



## C sharp

{{works with|C sharp|7}}

```csharp
using static System.Console;
using static System.Linq.Enumerable;

public class Program
{
    public static void Main() {
        foreach (int n in new [] { 0, 5, 13, 21, -22 }) WriteLine($"{n}: {string.Join(", ", LexOrder(n))}");
    }

    public static IEnumerable<int> LexOrder(int n) => (n < 1 ? Range(n, 2 - n) : Range(1, n)).OrderBy(i => i.ToString());
}
```

{{out}}

```txt

0: 0, 1
5: 1, 2, 3, 4, 5
13: 1, 10, 11, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9
21: 1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 2, 20, 21, 3, 4, 5, 6, 7, 8, 9
-22: -1, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -2, -20, -21, -22, -3, -4, -5, -6, -7, -8, -9, 0, 1

```



## Clojure


```clojure
(def n 13)
(sort-by str (range 1 (inc n)))
```

{{out}}

```txt
(1 10 11 12 13 2 3 4 5 6 7 8 9)
```



## Factor


```factor
USING: formatting kernel math.parser math.ranges sequences
sorting ;
IN: rosetta-code.lexicographical-numbers

: lex-order ( n -- seq )
    [1,b] [ number>string ] map natural-sort
    [ string>number ] map ;
    
{ 13 21 -22 } [ dup lex-order "%3d: %[%d, %]\n" printf ] each
```

{{out}}

```txt

 13: { 1, 10, 11, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9 }
 21: { 1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 2, 20, 21, 3, 4, 5, 6, 7, 8, 9 }
-22: { -1, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -2, -20, -21, -22, -3, -4, -5, -6, -7, -8, -9, 0, 1 }

```



## Go


```go
package main

import (
    "fmt"
    "sort"
    "strconv"
)

func lexOrder(n int) []int {
    first, last, k := 1, n, n
    if n < 1 {
        first, last, k = n, 1, 2-n
    }
    strs := make([]string, k)
    for i := first; i <= last; i++ {
        strs[i-first] = strconv.Itoa(i)
    }
    sort.Strings(strs)
    ints := make([]int, k)
    for i := 0; i < k; i++ {
        ints[i], _ = strconv.Atoi(strs[i])
    }
    return ints
}

func main() {
    fmt.Println("In lexicographical order:\n")
    for _, n := range []int{0, 5, 13, 21, -22} {
        fmt.Printf("%3d: %v\n", n, lexOrder(n))
    }
}
```


{{out}}

```txt

In lexicographical order:

  0: [0 1]
  5: [1 2 3 4 5]
 13: [1 10 11 12 13 2 3 4 5 6 7 8 9]
 21: [1 10 11 12 13 14 15 16 17 18 19 2 20 21 3 4 5 6 7 8 9]
-22: [-1 -10 -11 -12 -13 -14 -15 -16 -17 -18 -19 -2 -20 -21 -22 -3 -4 -5 -6 -7 -8 -9 0 1]

```



## Java

{{trans|Kotlin}}


Requires Java 8 or later.

```java
import java.util.List;
import java.util.stream.*;

public class LexicographicalNumbers {

    static List<Integer> lexOrder(int n) {
        int first = 1, last = n;
        if (n < 1) {
            first = n;
            last = 1;
        }
        return IntStream.rangeClosed(first, last)
                        .mapToObj(Integer::toString)
                        .sorted()
                        .map(Integer::valueOf)
                        .collect(Collectors.toList());
    }

    public static void main(String[] args) {
        System.out.println("In lexicographical order:\n");
        int[] ints = {0, 5, 13, 21, -22};
        for (int n : ints) {
           System.out.printf("%3d: %s\n", n, lexOrder(n));
        }
    }
}
```


{{out}}

```txt

In lexicographical order:

  0: [0, 1]
  5: [1, 2, 3, 4, 5]
 13: [1, 10, 11, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9]
 21: [1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 2, 20, 21, 3, 4, 5, 6, 7, 8, 9]
-22: [-1, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -2, -20, -21, -22, -3, -4, -5, -6, -7, -8, -9, 0, 1]

```



## Julia


```julia
lexorderedsequence(n) = sort(collect(n > 0 ? (1:n) : n:1), lt=(a,b) -> string(a) < string(b))

for i in [0, 5, 13, 21, -32]
    println(lexorderedsequence(i))
end

```
{{out}}

```txt

[0, 1]
[1, 2, 3, 4, 5]
[1, 10, 11, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9]
[1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 2, 20, 21, 3, 4, 5, 6, 7, 8, 9]
[-1, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -2, -20, -21, -22, -23, -24, -25, -26, -27, -28, -29, -3, -30, -31, -32, -4, -5, -6, -7, -8, -9, 0, 1]

```




## Kotlin


```scala
// Version 1.2.51

fun lexOrder(n: Int): List<Int> {
    var first = 1
    var last = n
    if (n < 1) {
        first = n
        last = 1
    }
    return (first..last).map { it.toString() }.sorted().map { it.toInt() }
}

fun main(args: Array<String>) {
    println("In lexicographical order:\n")
    for (n in listOf(0, 5, 13, 21, -22)) {
        println("${"%3d".format(n)}: ${lexOrder(n)}")
    }
}
```


{{output}}

```txt

In lexicographical order:

  0: [0, 1]
  5: [1, 2, 3, 4, 5]
 13: [1, 10, 11, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9]
 21: [1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 2, 20, 21, 3, 4, 5, 6, 7, 8, 9]
-22: [-1, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -2, -20, -21, -22, -3, -4, -5, -6, -7, -8, -9, 0, 1]

```



## Lua

Lua's in-built table.sort function will sort a table of strings into lexicographical order by default. This task therefore becomes trivial by converting each number to a string before adding it to the table.

```lua
function lexNums (limit)
  local numbers = {}
  for i = 1, limit do
    table.insert(numbers, tostring(i))
  end
  table.sort(numbers)
  return numbers
end

local numList = lexNums(13)
print(table.concat(numList, " "))
```

{{out}}

```txt
1 10 11 12 13 2 3 4 5 6 7 8 9
```



## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      Function lexicographical(N) {
            const nl$=Chr$(13)+Chr$(10)
            If N<>0 then {
                  if N=1 then =(1,) : Exit
                  Document A$
                  For k=1 to N-1 
                        A$=Str$(k,"")+{
                        }
                  Next k
                  A$=Str$(N,"")
                  Method A$, "SetBinaryCompare"
                  Sort A$
                  Flush
                  \\ convert strings to numbers in one statement
                  \\ in stack of values
                  Data Param(Replace$(nl$,",", a$))
                  \\ return stack as array 
                  =Array([])
            }  else =(0,)   ' empty array
      }
      Print lexicographical(5)  ' 1 2 3 4 5
      Print lexicographical(13) ' 1 10 11 12 13 2 3 4 5 6 7 8 9
      Print lexicographical(21) ' 1 10 11 12 13 14 15 16 17 18 19 2 20 21 3 4 5 6 7 8 9
      Print lexicographical(-22) ' -1 -10 -11 -12 -13 -14 -15 -16 -17 -18 -19 -2 -20 -21 -22 -3 -4 -5 -6 -7 -8 -9 0 1
}
}
Checkit
Module Checkit {
      Function lexicographical$(N) {
            const nl$=Chr$(13)+Chr$(10)
            If N<>0 then {
                  if N=1 then =(1,) : Exit
                  Document A$
                  For k=1 to N-1 
                        A$=Str$(k,"")+{
                        }
                  Next k
                  A$=Str$(N,"")
                  \\ by default id TextCompare, so 0 an 1 comes first in -22
                  Method A$, "SetBinaryCompare"
                  Sort A$
                  Flush
                  ="["+Replace$(nl$," ", a$)+"]"
                  
            }  else =("",)   ' empty array
      }
      Print lexicographical$(5)  ' [1 2 3 4 5]
      Print lexicographical$(13) ' [1 10 11 12 13 2 3 4 5 6 7 8 9]
      Print lexicographical$(21) '[1 10 11 12 13 14 15 16 17 18 19 2 20 21 3 4 5 6 7 8 9]
      Print lexicographical$(-22) ' [-1 -10 -11 -12 -13 -14 -15 -16 -17 -18 -19 -2 -20 -21 -22 -3 -4 -5 -6 -7 -8 -9 0 1]
}
Checkit

```



## Microsoft Small Basic

In Small Basic there is no string comparison: “a”>”b” the result is “False”, “b”>”a” the result is also “False”. It doesn’t help at all.

```smallbasic
' Lexicographical numbers - 25/07/2018
xx="000000000000000"
For n=1 To 3
  nn=Text.GetSubText("   5  13  21",n*4-3,4)
  ll=Text.GetLength(nn)
  For i=1 To nn
    t[i]=i
  EndFor
  i=nn-1
  k=0
  For i=i To 1 Step -1
    ok=1
    For j=1 To i
      k=j+1
      tj=Text.GetSubText(Text.Append(t[j],xx),1,ll)
      tk=Text.GetSubText(Text.Append(t[k],xx),1,ll)
      If tj>tk Then 
        w=t[j]
        t[j]=t[k]
        t[k]=w
        ok=0
      EndIf
    EndFor
    If ok=1 Then
      Goto exitfor
    EndIf
  EndFor
exitfor:
  x=""
  For i=1 To nn
    x=x+","+t[i]
  EndFor
  TextWindow.WriteLine(nn+":"+Text.GetSubTextToEnd(x,2))
EndFor 
```

{{out}}
   5:1,2,3,4,5
  13:1,10,11,12,13,2,3,4,5,6,7,8,9
  21:1,10,11,12,13,14,15,16,17,18,19,2,20,21,3,4,5,6,7,8,9


## Perl


```perl
printf("%4d: [%s]\n", $_, join ',', sort $_ > 0 ? 1..$_ : $_..1) for 13, 21, -22
```

{{out}}

```txt
  13: [1,10,11,12,13,2,3,4,5,6,7,8,9]
  21: [1,10,11,12,13,14,15,16,17,18,19,2,20,21,3,4,5,6,7,8,9]
 -22: [-1,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-2,-20,-21,-22,-3,-4,-5,-6,-7,-8,-9,0,1]
```


## Perl 6

{{works with|Rakudo|2018.06}}

```perl6
sub lex (Int $n) { (1…$n).sort: ~* }
 
# TESTING
printf("%4d: [%s]\n", $_, .&lex.join: ',') for 13, 21, -22
```

{{Out}}

```txt
  13: [1,10,11,12,13,2,3,4,5,6,7,8,9]
  21: [1,10,11,12,13,14,15,16,17,18,19,2,20,21,3,4,5,6,7,8,9]
 -22: [-1,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-2,-20,-21,-22,-3,-4,-5,-6,-7,-8,-9,0,1]
```



## Phix

Idiomatic version - crashes if n<1, and calls sprint() 76 times.

```Phix
function lexographic(integer i, j) 
    return compare(sprint(i),sprint(j))
end function 

function lex_order(integer n)
    return custom_sort(routine_id("lexographic"), tagset(n)) 
end function

?lex_order(13)
```

{{out}}

```txt

{1,10,11,12,13,2,3,4,5,6,7,8,9}

```

Alternative version, handles n<1, and for n=13 (say) it calls sprint() only 13 times instead of 76.

```Phix
function lex_order(integer n)
    integer {lo,hi} = iff(n<1?{n-1,1}:{0,n}), l = hi-lo
    sequence s = repeat(0,l)
    for i=1 to l do s[i] = {sprint(lo+i),lo+i} end for
    s = sort(s)
    for i=1 to l do s[i] = s[i][2] end for
    return s
end function

?lex_order(13)
?lex_order(0)
?lex_order(-22)
```

{{out}}

```txt

{1,10,11,12,13,2,3,4,5,6,7,8,9}
{0,1}
{-1,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-2,-20,-21,-22,-3,-4,-5,-6,-7,-8,-9,0,1}

```



## PureBasic

{{trans|Go}}

```purebasic
EnableExplicit

Procedure lexOrder(n, Array ints(1))
    Define first = 1, last = n, k = n, i  
    If n < 1
        first = n
        last = 1
        k = 2 - n
    EndIf
    Dim strs.s(k - 1)
    For i = first To last
        strs(i - first) = Str(i)
    Next
    SortArray(strs(), #PB_Sort_Ascending)
    For i = 0 To k - 1
        ints(i) = Val(Strs(i))
    Next
EndProcedure

If OpenConsole()
    PrintN(~"In lexicographical order:\n")
    Define i, j, n, k
    For i = 0 To 4
        Read n
        k = n
        If n < 1
            k = 2 - n
        EndIf
        Dim ints(k - 1)
        lexOrder(n, ints())
        Define.s ns = RSet(Str(n), 3)
        Print(ns + ": [")
        For j = 0 To k - 1
            Print(Str(ints(j)) + " ")
        Next j
        PrintN(~"\b]")
    Next i
    Input()
    End

    DataSection
        Data.i 0, 5, 13, 21, -22
    EndDataSection
EndIf
```


{{out}}

```txt

In lexicographical order:

  0: [0 1]
  5: [1 2 3 4 5]
 13: [1 10 11 12 13 2 3 4 5 6 7 8 9]
 21: [1 10 11 12 13 14 15 16 17 18 19 2 20 21 3 4 5 6 7 8 9]
-22: [-1 -10 -11 -12 -13 -14 -15 -16 -17 -18 -19 -2 -20 -21 -22 -3 -4 -5 -6 -7 -8 -9 0 1]

```



## Python


```python
n=13
print(sorted(range(1,n+1), key=str))
```

{{out}}

```txt
[1, 10, 11, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9]

```



## Racket



```racket
#lang racket

(define (lex-sort n) (sort (if (< 0 n) (range 1 (add1 n)) (range n 2))
                           string<? #:key number->string))

(define (show n) (printf "~a: ~a\n" n (lex-sort n)))

(show 0)
(show 1)
(show 5)
(show 13)
(show 21)
(show -22)
```


{{out}}

```txt

0: (0 1)
1: (1)
5: (1 2 3 4 5)
13: (1 10 11 12 13 2 3 4 5 6 7 8 9)
21: (1 10 11 12 13 14 15 16 17 18 19 2 20 21 3 4 5 6 7 8 9)
-22: (-1 -10 -11 -12 -13 -14 -15 -16 -17 -18 -19 -2 -20 -21 -22 -3 -4 -5 -6 -7 -8 -9 0 1)

```



## REXX

This REXX version allows the starting and ending integer to be specified via the command line (CL).

```rexx
/*REXX pgm displays a horizontal list of a  range of integers  sorted lexicographically.*/
parse arg LO HI .                                /*obtain optional arguments from the CL*/
if LO=='' | LO==","  then LO=  1                 /*Not specified?  Then use the default.*/
if HI=='' | HI==","  then HI= 13                 /* "      "         "   "   "     "    */
#=0                                              /*for actual sort, start array with  1.*/
             do j=LO  to  HI;  #= # + 1;   @.#=j /*construct an array from  LO   to  HI.*/
             end   /*j*/
call lSort #                                     /*sort integer array with a simple sort*/
$=                                               /*initialize horizontal integer list.  */
             do k=1  for  #;   $= $','@.k        /*construct      "         "      "    */
             end   /*k*/                         /* [↑]  prefix each number with a comma*/

say 'for '     LO"──►"HI'  (inclusive), '      #      "elements sorted lexicographically:"
say  '['strip($, "L", ',')"]"                    /*strip leading comma, bracket the list*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
lSort: procedure expose @.;  parse arg n;  m=n-1 /*N: is the number of @ array elements.*/
       do m=m  by -1  until ok;     ok=1         /*keep sorting the  @ array until done.*/
          do j=1  for m;   k=j+1;  if @.j>>@.k  then parse value @.j @.k 0 with @.k @.j ok
          end   /*j*/                            /* [↑]  swap 2 elements, flag as ¬done.*/
       end      /*m*/;     return
```

{{out|output|text=  when using the default input:}}

```txt

for  1──►13  (inclusive),  13 elements sorted lexicographically:
[1,10,11,12,13,2,3,4,5,6,7,8,9]

```

{{out|output|text=  when using the input of:     <tt> 1   34 </tt>}}

```txt

for  1──►34  (inclusive),  34 elements sorted lexicographically:
[1,10,11,12,13,14,15,16,17,18,19,2,20,21,22,23,24,25,26,27,28,29,3,30,31,32,33,34,4,5,6,7,8,9]

```

{{out|output|text=  when using the input of:     <tt> -11   22 </tt>}}

```txt

for  -11──►22  (inclusive),  34 elements sorted lexicographically:
[-1,-10,-11,-2,-3,-4,-5,-6,-7,-8,-9,0,1,10,11,12,13,14,15,16,17,18,19,2,20,21,22,3,4,5,6,7,8,9]

```



## Ring


```ring

# Project : Lexicographical numbers

lex = 1:13
strlex = list(len(lex))
for n = 1 to len(lex)
     strlex[n] = string(lex[n])
next
strlex = sort(strlex)
see "Lexicographical numbers = "
showarray(strlex)

func showarray(vect)
        see "["
        svect = ""
        for n = 1 to len(vect)
              svect = svect + vect[n] + ","
        next
        svect = left(svect, len(svect) - 1)
        see svect + "]" + nl

```

Output:

```txt

Lexicographical numbers = [1,10,11,12,13,2,3,4,5,6,7,8,9]

```



## Ruby


```ruby
n = 13
p (1..n).sort_by(&:to_s)

```

{{out}}

```txt
[1, 10, 11, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9]

```



## Scala

{{Out}}Best seen in running your browser either by [https://scalafiddle.io/sf/KpWHYNR/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/BnxJXLjCRvObdOv3VKTtYA Scastie (remote JVM)].

```Scala
object LexicographicalNumbers extends App {  def ints = List(0, 5, 13, 21, -22)

  def lexOrder(n: Int): Seq[Int] = (if (n < 1) n to 1 else 1 to n).sortBy(_.toString)

  println("In lexicographical order:\n")
  for (n <- ints) println(f"$n%3d: ${lexOrder(n).mkString("[",", ", "]")}%s")

}
```


## VBA


```VB
Public Function sortlexicographically(N As Integer)
    Dim arrList As Object
    Set arrList = CreateObject("System.Collections.ArrayList")
    For i = 1 To N
        arrList.Add CStr(i)
    Next i
    arrList.Sort
    Dim item As Variant
    For Each item In arrList
        Debug.Print item & ", ";
    Next
End Function

Public Sub main()
    Call sortlexicographically(13)
End Sub
```

{{out}}

```txt
1, 10, 11, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9, 
```



## Sidef


```ruby
func lex_order (n) {
    [range(1, n, n.sgn)...].sort_by { Str(_) }
}

[13, 21, -22].each {|n|
    printf("%4s: %s\n", n, lex_order(n))
}
```

{{out}}

```txt

  13: [1, 10, 11, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9]
  21: [1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 2, 20, 21, 3, 4, 5, 6, 7, 8, 9]
 -22: [-1, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -2, -20, -21, -22, -3, -4, -5, -6, -7, -8, -9, 0, 1]

```



## zkl


```zkl
fcn lexN(n){ n.pump(List,'+(1),"toString").sort().apply("toInt") }
```


```zkl
foreach n in (T(5,13,21)){ println("%2d: %s".fmt(n,lexN(n).concat(","))) }
```

{{out}}

```txt

 5: 1,2,3,4,5
13: 1,10,11,12,13,2,3,4,5,6,7,8,9
21: 1,10,11,12,13,14,15,16,17,18,19,2,20,21,3,4,5,6,7,8,9

```

