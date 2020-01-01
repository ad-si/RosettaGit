+++
title = "Leonardo numbers"
description = ""
date = 2019-10-21T19:47:11Z
aliases = []
[extra]
id = 21379
[taxonomies]
categories = []
tags = []
+++

{{task|Arithmetic operations}}
[[Category:Classic CS problems and programs]]

<!--  Leonardo numbers are also known as the Leonardo series.  -->

<!--  The following  <math>  tag doesn't render properly as it does correctly on Wikipedia:.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

<math>
  L(n) =
  \begin{cases}
    1                       & \mbox{if } n = 0 \\
    1                       & \mbox{if } n = 1 \\
    L(n - 1) + L(n - 2) + 1 & \mbox{if } n > 1 \\
  \end{cases}
 </math>

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                               -->

The   '''Leonardo numbers'''   are a sequence of numbers defined by:
  <big>      L(0) = 1                                          [1<sup>st</sup> equation]  </big>
  <big>      L(1) = 1                                          [2<sup>nd</sup> equation]  </big>
  <big>      L(n) = L(n-1)  +    L(n-2)   +  1                 [3<sup>rd</sup> equation]  </big>
                     ─── also ───
  <big>      L(n) =      2  *  Fib(n+1)   -  1                 [4<sup>th</sup> equation]  </big>

::::   where the   '''+ 1'''   will herein be known as the   ''add''   number.
::::   where the   '''FIB'''   is the   [[wp:Fibonacci number|Fibonacci number]]s.


The task will be using the 3<sup>rd</sup> equation (above) to calculate the Leonardo numbers.


[[wp:Edsger W. Dijkstra|Edsger W. Dijkstra]]   used them as an integral part of
his   [[wp:smoothsort|smoothsort]]   [[wp:algorithm|algorithm]].

<!--  The following  <math>  tag doesn't render properly as it does correctly on Wikipedia:.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:<math>1,\;1,\;3,\;5,\;9,\;15,\;25,\;41,\;67,\;109,\;177,\;287,\;465,\;753,\;1219,\;1973,\;3193,\;5167,\;8361, \ldots</math>

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                               -->

The first few Leonardo numbers are:
    ''' 1   1   3   5   9   15   25   41   67   109   177   287   465   753   1219   1973   3193   5167   8361  ··· '''


;Task:
::*   show the 1<sup>st</sup>   '''25'''   Leonardo numbers, starting at '''L(0)'''.
::*   allow the first two Leonardo numbers to be specified   [for '''L(0)''' and '''L(1)'''].
::*   allow the   ''add''   number to be specified   ('''1''' is the default).
::*   show the 1<sup>st</sup>   '''25'''   Leonardo numbers, specifying '''0''' and '''1''' for '''L(0)''' and '''L(1)''', and '''0''' for the ''add'' number.

(The last task requirement will produce the Fibonacci numbers.)


Show all output here.


;Related tasks:
*   [[Fibonacci number]]
*   [[Fibonacci n-step number sequences ]]


;See also:
*   [[wp:Leonardo number|Wikipedia, Leonardo numbers]]
*   [[wp:Fibonacci number|Wikipedia, Fibonacci numbers]]
*   [[oeis:A001595|OEIS Leonardo numbers]]





## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Leonardo is

   function Leo
     (N      : Natural;
      Step   : Natural := 1;
      First  : Natural := 1;
      Second : Natural := 1) return Natural   is
      L : array (0..1) of Natural := (First, Second);
	begin
		for i in 1 .. N loop
			L := (L(1), L(0)+L(1)+Step);
		end loop;
		return L (0);
	end Leo;

begin
   Put_Line ("First 25 Leonardo numbers:");
   for I in 0 .. 24 loop
      Put (Integer'Image (Leo (I)));
   end loop;
   New_Line;
   Put_Line ("First 25 Leonardo numbers with L(0) = 0, L(1) = 1, " &
             "step = 0 (fibonacci numbers):");
   for I in 0 .. 24 loop
      Put (Integer'Image (Leo (I, 0, 0, 1)));
   end loop;
   New_Line;
end Leonardo;
```

{{out}}

```txt

First 25 Leonardo numbers:
 1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
First 25 Leonardo numbers with L(0) = 0, L(1) = 1, step = 0 (fibonacci numbers):
 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## ALGOL 68


```algol68
BEGIN
    # leonardo number parameters #
    MODE LEONARDO = STRUCT( INT l0, l1, add number );
    # default leonardo number parameters #
    LEONARDO leonardo numbers = LEONARDO( 1, 1, 1 );
    # operators to allow us to specify non-default parameters #
    PRIO WITHLZERO = 9, WITHLONE = 9, WITHADDNUMBER = 9;
    OP   WITHLZERO     = ( LEONARDO parameters, INT l0         )LEONARDO:
         LEONARDO( l0, l1 OF parameters, add number OF parameters );
    OP   WITHLONE      = ( LEONARDO parameters, INT l1         )LEONARDO:
         LEONARDO( l0 OF parameters, l1, add number OF parameters );
    OP   WITHADDNUMBER = ( LEONARDO parameters, INT add number )LEONARDO:
         LEONARDO( l0 OF parameters, l1 OF parameters, add number );
    # show the first n Leonardo numbers with the specified parameters #
    PROC show = ( INT n, LEONARDO parameters )VOID:
         IF n > 0 THEN
            INT l0         = l0         OF parameters;
            INT l1         = l1         OF parameters;
            INT add number = add number OF parameters;
            print( ( whole( l0, 0 ), " " ) );
            IF n > 1 THEN
                print( ( whole( l1, 0 ), " " ) );
                INT lp := l0;
                INT ln := l1;
                FROM 2 TO n - 1 DO
                    INT next = ln + lp + add number;
                    lp := ln;
                    ln := next;
                    print( ( whole( ln, 0 ), " " ) )
                OD
            FI
         FI # show # ;

    # first series #
    print( ( "First 25 Leonardo numbers", newline ) );
    show( 25, leonardo numbers );
    print( ( newline ) );
    # second series #
    print( ( "First 25 Leonardo numbers from 0, 1 with add number = 0", newline ) );
    show( 25, leonardo numbers WITHLZERO 0 WITHADDNUMBER 0 );
    print( ( newline ) )
END
```

{{out}}

```txt

First 25 Leonardo numbers
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
First 25 Leonardo numbers from 0, 1 with add number = 0
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```


## AppleScript

{{Trans|Python}} (Generator version)


Drawing N items from a non-finite generator:

```applescript
-- leo :: Int -> Int -> Int -> Generator [Int]
on leo(L0, L1, delta)
    script
        property x : L0
        property y : L1
        on |λ|()
            set n to x
            set {x, y} to {y, x + y + delta}
            return n
        end |λ|
    end script
end leo


-- TEST ---------------------------------------------------
on run
    set leonardo to leo(1, 1, 1)
    set fibonacci to leo(0, 1, 0)

    unlines({"First 25 Leonardo numbers:", ¬
        twoLines(take(25, leonardo)), "", ¬
        "First 25 Fibonacci numbers:", ¬
        twoLines(take(25, fibonacci))})
end run


-- FORMATTING ---------------------------------------------

-- twoLines :: [Int] -> String
on twoLines(xs)
    script row
        on |λ|(ns)
            tab & showList(ns)
        end |λ|
    end script
    return unlines(map(row, chunksOf(16, xs)))
end twoLines


-- GENERIC -----------------------------------------------


-- chunksOf :: Int -> [a] -> [[a]]
on chunksOf(n, xs)
    set lng to length of xs
    script go
        on |λ|(a, i)
            set x to (i + n) - 1
            if x ≥ lng then
                a & {items i thru -1 of xs}
            else
                a & {items i thru x of xs}
            end if
        end |λ|
    end script
    foldl(go, {}, enumFromThenTo(1, n, lng))
end chunksOf

-- enumFromThenTo :: Int -> Int -> Int -> [Int]
on enumFromThenTo(x1, x2, y)
    set xs to {}
    set d to max(1, (x2 - x1))
    repeat with i from x1 to y by d
        set end of xs to i
    end repeat
    return xs
end enumFromThenTo

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

-- intercalate :: String -> [String] -> String
on intercalate(sep, xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, sep}
    set s to xs as text
    set my text item delimiters to dlm
    return s
end intercalate

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

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

-- max :: Ord a => a -> a -> a
on max(x, y)
    if x > y then
        x
    else
        y
    end if
end max

-- showList :: [a] -> String
on showList(xs)
    "[" & intercalate(", ", xs) & "]"
end showList

-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    set c to class of xs
    if list is c then
        if 0 < n then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    else if string is c then
        if 0 < n then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else if script is c then
        set ys to {}
        repeat with i from 1 to n
            set v to xs's |λ|()
            if missing value is v then
                return ys
            else
                set end of ys to v
            end if
        end repeat
        return ys
    else
        missing value
    end if
end take

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines
```

{{Out}}

```txt
First 25 Leonardo numbers:
    [1, 1, 3, 5, 9, 15, 25, 41, 67, 109, 177, 287, 465, 753, 1219, 1973]
    [3193, 5167, 8361, 13529, 21891, 35421, 57313, 92735, 150049]

First 25 Fibonacci numbers:
    [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610]
    [987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368]
```



## AWK


```AWK

# syntax: GAWK -f LEONARDO_NUMBERS.AWK
BEGIN {
    leonardo(1,1,1,"Leonardo")
    leonardo(0,1,0,"Fibonacci")
    exit(0)
}
function leonardo(L0,L1,step,text,  i,tmp) {
    printf("%s numbers (%d,%d,%d):\n",text,L0,L1,step)
    for (i=1; i<=25; i++) {
      if (i == 1) {
        printf("%d ",L0)
      }
      else if (i == 2) {
        printf("%d ",L1)
      }
      else {
        printf("%d ",L0+L1+step)
        tmp = L0
        L0 = L1
        L1 = tmp + L1 + step
      }
    }
    printf("\n")
}

```

{{out}}

```txt

Leonardo numbers (1,1,1):
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
Fibonacci numbers (0,1,0):
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## BASIC

=
## BASIC256
=

```BASIC256

subroutine leonardo(L0, L1, suma, texto)
	print "Numeros de " + texto + " (" + L0 + "," + L1 + "," + suma + "):"
	for i = 1 to 25
		if i = 1 then
			print L0 + " ";
		else
			if i = 2 then
				print L1 + " ";
			else
				print L0 + L1 + suma + " ";
				tmp = L0
				L0 = L1
				L1 = tmp + L1 + suma
			end if
		end if
	next i
	print chr(10)
end subroutine

#--- Programa Principal ---
call leonardo(1,1,1,"Leonardo")
call leonardo(0,1,0,"Fibonacci")
end

```

{{out}}

```txt

Numeros de Leonardo (1,1,1):
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

Numeros de Fibonacci (0,1,0):
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Leonardo.bas"
110 INPUT PROMPT "Enter values of L0, L1, and ADD, separated by comas: ":L0,L1,ADD
120 PRINT L0;L1;
130 FOR I=3 TO 25
140   LET T=L1:LET L1=L1+L0+ADD:LET L0=T
160   PRINT L1;
170 NEXT
180 PRINT
```


=
## Sinclair ZX81 BASIC
=
Runs on the 1k RAM model with room to spare; hence the long(ish) variable names. The parameters are read from the keyboard.

```basic
 10 INPUT L0
 20 INPUT L1
 30 INPUT ADD
 40 PRINT L0;" ";L1;
 50 FOR I=3 TO 25
 60 LET TEMP=L1
 70 LET L1=L0+L1+ADD
 80 LET L0=TEMP
 90 PRINT " ";L1;
100 NEXT I
```

{{in}}

```txt
1
1
1
```

{{out}}

```txt
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
```

{{in}}

```txt
0
1
0
```

{{out}}

```txt
 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368
```



## BBC BASIC

It's a shame when fonts don't make much of a distinction between <tt>l</tt> lower-case L and <tt>1</tt> the number One.

```bbcbasic>REM
leonardo
:
PRINT "Enter values of L0, L1, and ADD, separated by commas:"
INPUT l0%, l1%, add%
PRINT l0% ' l1%
FOR i% = 3 TO 25
  temp% = l1%
  l1% += l0% + add%
  l0% = temp%
  PRINT l1%
NEXT
PRINT
END
```

{{out}}

```txt
Enter values of L0, L1, and ADD, separated by commas:
?1, 1, 1
         1
         1
         3
         5
         9
        15
        25
        41
        67
       109
       177
       287
       465
       753
      1219
      1973
      3193
      5167
      8361
     13529
     21891
     35421
     57313
     92735
    150049
```


```txt
Enter values of L0, L1, and ADD, separated by commas:
?0, 1, 0
         0
         1
         1
         2
         3
         5
         8
        13
        21
        34
        55
        89
       144
       233
       377
       610
       987
      1597
      2584
      4181
      6765
     10946
     17711
     28657
     46368
```


## Burlesque


```burlesque
blsq ) 1 1 1{.+\/.+}\/+]23!CCLm]wdsh
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

blsq ) 0 1 0{.+\/.+}\/+]23!CCLm]wdsh
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368
```


## C

This implementation fulfills the task requirements which state that the first 2 terms and the step increment should be specified. Many other implementations on this page only print out the first 25 numbers.

```C

#include<stdio.h>

void leonardo(int a,int b,int step,int num){

	int i,temp;

	printf("First 25 Leonardo numbers : \n");

	for(i=1;i<=num;i++){
		if(i==1)
			printf(" %d",a);
		else if(i==2)
			printf(" %d",b);
		else{
			printf(" %d",a+b+step);
			temp = a;
			a = b;
			b = temp+b+step;
		}
	}
}

int main()
{
	int a,b,step;

	printf("Enter first two Leonardo numbers and increment step : ");

	scanf("%d%d%d",&a,&b,&step);

	leonardo(a,b,step,25);

	return 0;
}

```

Output :
Normal Leonardo Series :

```txt

Enter first two Leonardo numbers and increment step : 1 1 1
First 25 Leonardo numbers :
 1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

```

Fibonacci Series :

```txt

Enter first two Leonardo numbers and increment step : 0 1 0
First 25 Leonardo numbers :
 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## C++


```cpp

#include <iostream>

void leoN( int cnt, int l0 = 1, int l1 = 1, int add = 1 ) {
    int t;
    for( int i = 0; i < cnt; i++ ) {
        std::cout << l0 << " ";
        t = l0 + l1 + add; l0 = l1; l1 = t;
    }
}
int main( int argc, char* argv[] ) {
    std::cout << "Leonardo Numbers: "; leoN( 25 );
    std::cout << "\n\nFibonacci Numbers: "; leoN( 25, 0, 1, 0 );
    return 0;
}

```

{{out}}
```txt

Leonardo Numbers: 1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

Fibonacci Numbers: 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## C#

{{works with|C sharp|7}}

```c#
using System;
using System.Linq;

public class Program
{
    public static void Main() {
        Console.WriteLine(string.Join(" ", Leonardo().Take(25)));
        Console.WriteLine(string.Join(" ", Leonardo(L0: 0, L1: 1, add: 0).Take(25)));
    }

    public static IEnumerable<int> Leonardo(int L0 = 1, int L1 = 1, int add = 1) {
        while (true) {
            yield return L0;
            (L0, L1) = (L1, L0 + L1 + add);
        }
    }
}
```

{{out}}

```txt

1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```




## Common Lisp


```lisp

;;;
;;; leo - calculates the first n number from a leo sequence.
;;; The first argument n is the number of values to return. The next three arguments a, b, add are optional.
;;; Default values provide the "original" leonardo numbers as defined in the task.
;;; a and b are the first and second element of the leonardo sequence.
;;; add is the "add number" as defined in the task definition.
;;;

(defun leo (n &optional (a 1) (b 1) (add 1))
  (labels ((iterate (n foo)
             (if (zerop n) (reverse foo)
                           (iterate (- n 1)
                                    (cons (+ (first foo) (second foo) add) foo)))))
     (cond ((= n 1) (list a))
           (T       (iterate (- n 2) (list b a))))))

```


{{out}}

```txt

> (leo 25)
(1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049)
> (leo 25 0 1 0)
(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368)

```



## Crystal

{{trans|Python}}

```ruby
def leonardo(l_zero, l_one, add, amount)
    terms = [l_zero, l_one]
    while terms.size < amount
        new = terms[-1] + terms[-2]
        new += add
        terms << new
    end
    terms
end

puts "First 25 Leonardo numbers: \n#{ leonardo(1,1,1,25) }"
puts "Leonardo numbers with fibonacci parameters:\n#{ leonardo(0,1,0,25) }"

```

{{out}}

```txt

First 25 Leonardo numbers:
[1, 1, 3, 5, 9, 15, 25, 41, 67, 109, 177, 287, 465, 753, 1219, 1973, 3193, 5167, 8361, 13529, 21891, 35421, 57313, 92735, 150049]
Leonardo numbers with fibonacci parameters:
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368]

```



## D

{{trans|C++}}

```D

import std.stdio;

void main() {
    write("Leonardo Numbers: ");
    leonardoNumbers( 25 );

    write("Fibonacci Numbers: ");
    leonardoNumbers( 25, 0, 1, 0 );
}

void leonardoNumbers(int count, int l0=1, int l1=1, int add=1) {
    int t;
    for (int i=0; i<count; ++i) {
        write(l0, " ");
        t = l0 + l1 + add;
        l0 = l1;
        l1 = t;
    }
    writeln();
}

```

{{out}}
```txt

Leonardo Numbers: 1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
Fibonacci Numbers: 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```


=={{header|F#|F sharp}}==
{{trans|Haskell}}

```fsharp
open System

let leo l0 l1 d =
    Seq.unfold (fun (x, y) -> Some (x, (y, x + y + d))) (l0, l1)

let leonardo = leo 1 1 1
let fibonacci = leo 0 1 0

[<EntryPoint>]
let main _ =
    let leoNums = Seq.take 25 leonardo |> Seq.chunkBySize 16
    printfn "First 25 of the (1, 1, 1) Leonardo numbers:\n%A" leoNums
    Console.WriteLine()

    let fibNums = Seq.take 25 fibonacci |> Seq.chunkBySize 16
    printfn "First 25 of the (0, 1, 0) Leonardo numbers (= Fibonacci number):\n%A" fibNums

    0 // return an integer exit code
```

{{out}}

```txt
First 25 of the (1, 1, 1) Leonardo numbers:
seq
  [[|1; 1; 3; 5; 9; 15; 25; 41; 67; 109; 177; 287; 465; 753; 1219; 1973|];
   [|3193; 5167; 8361; 13529; 21891; 35421; 57313; 92735; 150049|]]

First 25 of the (0, 1, 0) Leonardo numbers (= Fibonacci number):
seq
  [[|0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610|];
   [|987; 1597; 2584; 4181; 6765; 10946; 17711; 28657; 46368|]]
```



## Factor

<lang>USING: fry io kernel math prettyprint sequences ;
IN: rosetta-code.leonardo-numbers

: first25-leonardo ( vector add -- seq )
    23 swap '[ dup 2 tail* sum _ + over push ] times ;

: print-leo ( seq -- ) [ pprint bl ] each nl ;

"First 25 Leonardo numbers:" print
V{ 1 1 } 1 first25-leonardo print-leo

"First 25 Leonardo numbers with L(0)=0, L(1)=1, add=1:" print
V{ 0 1 } 0 first25-leonardo print-leo
```

{{out}}

```txt

First 25 Leonardo numbers:
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
First 25 Leonardo numbers with L(0)=0, L(1)=1, add=1:
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## Fortran

Happily, no monster values result for the trial run, so ordinary 32-bit integers suffice. The source style uses the F90 facilities only to name the subroutine being ended (i.e. <code>END SUBROUTINE LEONARDO</code> rather than just <code>END</code>) and the I0 format code that shows an integer without a fixed space allowance, convenient in produced well-formed messages. The "$" format code signifies that the end of output from its WRITE statement should not trigger the starting of a new line for the next WRITE statement, convenient when rolling a sequence of values to a line of output one-by-one as they are concocted. Otherwise, the values would have to be accumulated in a suitable array and then written in one go.

Many versions of Fortran have enabled parameters to be optionally supplied and F90 has standardised a protocol, also introducing a declaration syntax that can specify multiple attributes in one statement which in this case would be <code>INTEGER, OPTIONAL:: AF</code> rather than two statements concerning AF. However, in a test run with <code>CALL LEONARDO(25,1,1)</code> the Compaq F90/95 compiler rejected this attempt because there was another invocation with four parameters, not three, in the same program unit. By adding the rigmarole for declaring a MODULE containing the subroutine LEONARDO, its worries would be assuaged. Many compilers (and linkers, for separately-compiled routines) would check neither the number nor the type of parameters so no such complaint would be made - but when run, the code might produce wrong results or crash.

The method relies on producing a sequence of values, rather than calculating L(n) from the start each time a value from the sequence is required.
```Fortran
      SUBROUTINE LEONARDO(LAST,L0,L1,AF)	!Show the first LAST values of the sequence.
       INTEGER LAST	!Limit to show.
       INTEGER L0,L1	!Starting values.
       INTEGER AF	!The "Add factor" to deviate from Fibonacci numbers.
       OPTIONAL AF	!Indicate that this parameter may be omitted.
       INTEGER EMBOLISM	!The bloat to employ.
       INTEGER N,LN,LNL1,LNL2	!Assistants to the calculation.
        IF (PRESENT(AF)) THEN	!Perhaps the last parameter has not been given.
          EMBOLISM = AF			!It has. Take its value.
         ELSE			!But if not,
          EMBOLISM = 1			!This is the specified default.
        END IF			!Perhaps there should be some report on this?
        WRITE (6,1) LAST,L0,L1,EMBOLISM	!Announce.
    1   FORMAT ("The first ",I0,	!The I0 format code avoids excessive spacing.
     1   " numbers in the Leonardo sequence defined by L(0) = ",I0,
     2   " and L(1) = ",I0," with L(n) = L(n - 1) + L(n - 2) + ",I0)
        IF (LAST .GE. 1) WRITE (6,2) L0	!In principle, LAST may be small.
        IF (LAST .GE. 2) WRITE (6,2) L1	!!So, suspicion rules.
    2   FORMAT (I0,", ",$)	!Obviously, the $ sez "don't finish the line".
        LNL1 = L0	!Syncopation for the sequence's initial values.
        LN = L1		!Since the parameters ought not be damaged.
        DO N = 3,LAST	!Step away.
          LNL2 = LNL1		!Advance the two state variables one step.
          LNL1 = LN		!Ready to make a step forward.
          LN = LNL1 + LNL2 + EMBOLISM	!Thus.
          WRITE (6,2) LN	!Reveal the value. Overflow is distant...
        END DO		!On to the next step.
        WRITE (6,*)	!Finish the line.
      END SUBROUTINE LEONARDO	!Only speedy for the sequential production of values.

      PROGRAM POKE

      CALL LEONARDO(25,1,1,1)	!The first 25 Leonardo numbers.
      CALL LEONARDO(25,0,1,0)	!Deviates to give the Fibonacci sequence.
      END
```

Output:

```txt

The first 25 numbers in the Leonardo sequence defined by L(0) = 1 and L(1) = 1 with L(n) = L(n - 1) + L(n - 2) + 1
1, 1, 3, 5, 9, 15, 25, 41, 67, 109, 177, 287, 465, 753, 1219, 1973, 3193, 5167, 8361, 13529, 21891, 35421, 57313, 92735, 150049,
The first 25 numbers in the Leonardo sequence defined by L(0) = 0 and L(1) = 1 with L(n) = L(n - 1) + L(n - 2) + 0
0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368,

```



## FreeBASIC


```freebasic

Sub leonardo(L0 As Integer, L1 As Integer, suma As Integer, texto As String)
    Dim As Integer i, tmp
    Print "Numeros de " &texto &" (" &L0 &"," &L1 &"," &suma &"):"
    For i = 1 To 25
        If i = 1 Then
            Print L0;
        Elseif i = 2 Then
            Print L1;
        Else
            Print L0 + L1 + suma;
            tmp = L0
            L0 = L1
            L1 = tmp + L1 + suma
        End If
    Next i
    Print Chr(10)
End Sub

'--- Programa Principal ---
leonardo(1,1,1,"Leonardo")
leonardo(0,1,0,"Fibonacci")
End

```

{{out}}

```txt

Numeros de Leonardo (1,1,1):
 1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

Numeros de Fibonacci (0,1,0):
 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## Go


```go
package main

import "fmt"

func leonardo(n, l0, l1, add int) []int {
    leo := make([]int, n)
    leo[0] = l0
    leo[1] = l1
    for i := 2; i < n; i++ {
        leo[i] = leo[i - 1] + leo[i - 2] + add
    }
    return leo
}

func main() {
    fmt.Println("The first 25 Leonardo numbers with L[0] = 1, L[1] = 1 and add number = 1 are:")
    fmt.Println(leonardo(25, 1, 1, 1))
    fmt.Println("\nThe first 25 Leonardo numbers with L[0] = 0, L[1] = 1 and add number = 0 are:")
    fmt.Println(leonardo(25, 0, 1, 0))
}
```


{{out}}

```txt

The first 25 Leonardo numbers with L[0] = 1, L[1] = 1 and add number = 1 are:
[1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049]

The first 25 Leonardo numbers with L[0] = 0, L[1] = 1 and add number = 0 are:
[0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368]

```



## Haskell


```Haskell
import Data.List.Split (chunksOf)
import Data.List (unfoldr)

-- LEONARDO NUMBERS -----------------------------------------------------------
-- L0 -> L1 -> Add number -> Series (infinite)
leo :: Integer -> Integer -> Integer -> [Integer]
leo l0 l1 d = unfoldr (\(x, y) -> Just (x, (y, x + y + d))) (l0, l1)

leonardo :: [Integer]
leonardo = leo 1 1 1

fibonacci :: [Integer]
fibonacci = leo 0 1 0

-- TEST -----------------------------------------------------------------------
main :: IO ()
main = do
  let twoLines = unlines . fmap (('\t' :) . show) . chunksOf 16
  (putStrLn . unlines)
    [ "First 25 default (1, 1, 1) Leonardo numbers:\n"
    , twoLines $ take 25 leonardo
    , "First 25 of the (0, 1, 0) Leonardo numbers (= Fibonacci numbers):\n"
    , twoLines $ take 25 fibonacci
    ]
```

{{Out}}

```txt
First 25 default (1, 1, 1) Leonardo numbers:

    [1,1,3,5,9,15,25,41,67,109,177,287,465,753,1219,1973]
    [3193,5167,8361,13529,21891,35421,57313,92735,150049]

First 25 of the (0, 1, 0) Leonardo numbers (= Fibonacci numbers):

    [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]
    [987,1597,2584,4181,6765,10946,17711,28657,46368]
```



## J


```J

leo =:  (] , {.@[ + _2&{@] + {:@])^:(_2&+@{:@[)

```

{{Out}}

```txt

 1 25 leo 1 1
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

 0 25 leo 0 1
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## Java

{{trans|Kotlin}}

```Java
import java.util.Arrays;
import java.util.List;

@SuppressWarnings("SameParameterValue")
public class LeonardoNumbers {
    private static List<Integer> leonardo(int n) {
        return leonardo(n, 1, 1, 1);
    }

    private static List<Integer> leonardo(int n, int l0, int l1, int add) {
        Integer[] leo = new Integer[n];
        leo[0] = l0;
        leo[1] = l1;
        for (int i = 2; i < n; i++) {
            leo[i] = leo[i - 1] + leo[i - 2] + add;
        }
        return Arrays.asList(leo);
    }

    public static void main(String[] args) {
        System.out.println("The first 25 Leonardo numbers with L[0] = 1, L[1] = 1 and add number = 1 are:");
        System.out.println(leonardo(25));
        System.out.println("\nThe first 25 Leonardo numbers with L[0] = 0, L[1] = 1 and add number = 0 are:");
        System.out.println(leonardo(25, 0, 1, 0));
    }
}
```

{{out}}

```txt
The first 25 Leonardo numbers with L[0] = 1, L[1] = 1 and add number = 1 are:
[1, 1, 3, 5, 9, 15, 25, 41, 67, 109, 177, 287, 465, 753, 1219, 1973, 3193, 5167, 8361, 13529, 21891, 35421, 57313, 92735, 150049]

The first 25 Leonardo numbers with L[0] = 0, L[1] = 1 and add number = 0 are:
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368]
```



## JavaScript


### ES6


```JavaScript
const leoNum = (c, l0=1, l1=1, add=1) => new Array(c).fill(add).reduce(
  (p, c, i) => i > 1 ? p.push(p[i-1] + p[i-2] + c) && p : p, [l0, l1]
);
console.log(leoNum(25));
console.log(leoNum(25, 0, 1, 0));

```



```txt

[1, 1, 3, 5, 9, 15, 25, 41, 67, 109, 177, 287, 465, 753, 1219, 1973, 3193, 5167, 8361, 13529, 21891, 35421, 57313, 92735, 150049]
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368]
```



Or, taking N terms from a non-finite Javascript generator:
{{Trans|Python}}

```javascript
(() => {
    'use strict';

    // leo :: Int -> Int -> Int -> Generator [Int]
    function* leo(L0, L1, delta) {
        let [x, y] = [L0, L1];
        while (true) {
            yield x;
            [x, y] = [y, x + y + delta];
        }
    }

    // main :: IO ()
    const main = () => {
        const
            leonardo = leo(1, 1, 1),
            fibonacci = leo(0, 1, 0);

        console.log(
            unlines([
                'First 25 Leonardo numbers:',
                twoLines(take(25, leonardo)),
                '',
                'First 25 Fibonacci numbers:',
                twoLines(take(25, fibonacci))
            ])
        );
    };


    // FORMATTING -----------------------------------------

    // twoLines :: [Int] -> String
    const twoLines = xs =>
        unlines(map(
            ns => '\t' + showJSON(ns),
            chunksOf(16, xs)
        ));

    // GENERIC FUNCTIONS ----------------------------------


    // chunksOf :: Int -> [a] -> [[a]]
    const chunksOf = (n, xs) =>
        enumFromThenTo(0, n - 1, xs.length - 1)
        .reduce(
            (a, i) => a.concat([xs.slice(i, i + n)]),
            []
        );

    // enumFromThenTo :: Int -> Int -> Int -> [Int]
    const enumFromThenTo = (x1, x2, y) => {
        const d = x2 - x1;
        return Array.from({
            length: Math.floor(y - x2) / d + 2
        }, (_, i) => x1 + (d * i));
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // showJSON :: a -> String
    const showJSON = x => JSON.stringify(x);

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
First 25 Leonardo numbers:
    [1,1,3,5,9,15,25,41,67,109,177,287,465,753,1219,1973]
    [3193,5167,8361,13529,21891,35421,57313,92735,150049]

First 25 Fibonacci numbers:
    [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]
    [987,1597,2584,4181,6765,10946,17711,28657,46368]
```



## jq


### Naive Implementation


```jq
def Leonardo(zero; one; incr):
  def leo:
    if . == 0 then zero
    elif . == 1 then one
    else ((.-1) |leo) + ((.-2) | leo) +  incr
    end;
  leo;
```


### Implementation with Caching

An array is used for caching, with `.[n]` storing the value L(n).

```jq
def Leonardo(zero; one; incr):
  def leo(n):
    if .[n] then .
    else leo(n-1)   # optimization of leo(n-2)|leo(n-1)
    | .[n] = .[n-1] + .[n-2] +  incr
    end;
  . as $n | [zero,one] | leo($n) | .[$n];
```


(To compute the sequence of Leonardo numbers L(1) ... L(n) without redundant computation, the last element of the pipeline in the last line of the function above should be dropped.)

'''Examples'''


```jq
[range(0;25) | Leonardo(1;1;1)]
```

{{out}}

```txt
[1,1,3,5,9,15,25,41,67,109,177,287,465,753,1219,1973,3193,5167,8361,13529,21891,35421,57313,92735,150049]
```



```jq
[range(0;25) | Leonardo(0;1;0)]
```

{{out}}

```txt
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368]
```



## Julia

{{works with|Julia|0.6}}


```julia
function L(n, add::Int=1, firsts::Vector=[1, 1])
    l = max(maximum(n) .+ 1, length(firsts))
    r = Vector{Int}(l)
    r[1:length(firsts)] = firsts
    for i in 3:l
        r[i] = r[i - 1] + r[i - 2] + add
    end
    return r[n .+ 1]
end

# Task 1
println("First 25 Leonardo numbers: ", join(L(0:24), ", "))

# Task 2
@show L(0) L(1)

# Task 4
println("First 25 Leonardo numbers starting with [0, 1]: ", join(L(0:24, 0, [0, 1]), ", "))
```


{{out}}

```txt
First 25 Leonardo numbers: 1, 1, 3, 5, 9, 15, 25, 41, 67, 109, 177, 287, 465, 753, 1219, 1973, 3193, 5167, 8361, 13529, 21891, 35421, 57313, 92735, 150049
L(0) = 1
L(1) = 1
First 25 Leonardo numbers starting with 0, 1: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368
```



## Kotlin


```scala
// version 1.1.2

fun leonardo(n: Int, l0: Int = 1, l1: Int = 1, add: Int = 1): IntArray {
    val leo = IntArray(n)
    leo[0] = l0
    leo[1] = l1
    for (i in 2 until n) leo[i] = leo[i - 1] + leo[i - 2] + add
    return leo
}

fun main(args: Array<String>) {
    println("The first 25 Leonardo numbers with L[0] = 1, L[1] = 1 and add number = 1 are:")
    println(leonardo(25).joinToString(" "))
    println("\nThe first 25 Leonardo numbers with L[0] = 0, L[1] = 1 and add number = 0 are:")
    println(leonardo(25, 0, 1, 0).joinToString(" "))
}
```


{{out}}

```txt

The first 25 Leonardo numbers with L[0] = 1, L[1] = 1 and add number = 1 are:
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

The first 25 Leonardo numbers with L[0] = 0, L[1] = 1 and add number = 0 are:
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## Lua


```lua
function leoNums (n, L0, L1, add)
  local L0, L1, add = L0 or 1, L1 or 1, add or 1
  local lNums, nextNum = {L0, L1}
  while #lNums < n do
    nextNum = lNums[#lNums] + lNums[#lNums - 1] + add
    table.insert(lNums, nextNum)
  end
  return lNums
end

function show (msg, t)
  print(msg .. ":")
  for i, x in ipairs(t) do
    io.write(x .. " ")
  end
  print("\n")
end

show("Leonardo numbers", leoNums(25))
show("Fibonacci numbers", leoNums(25, 0, 1, 0))
```

{{out}}

```txt
Leonardo numbers:
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

Fibonacci numbers:
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368
```


=={{Header|Maple}}==


```Maple
L := proc(n, L_0, L_1, add)
if n = 0 then
  return L_0;
elif n = 1 then
  return L_1;
else
  return L(n - 1) + L(n - 2) + add;
end if;
end proc:

Leonardo := n -> (L(1, 1, 1),[seq(0..n - 1)])

Fibonacci := n -> (L(0, 1, 0), [seq(0..n - 1)])
```


```txt
[1, 1, 3, 5, 9, 15, 25, 41, 67, 109, 177, 287, 465, 753, 1219, 1973, 3193, 5167, 8361, 13529, 21891, 35421, 57313, 92735, 150049]
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368]
```


=={{Header|Mathematica}}==

{{incorrect|Mathematica|

 The wrong formula is being used (the 4<sup>th</sup> formula is being used, instead, the 3<sup>rd</sup> formula is to be used.

 Also, output is missing for the Fibonacci series calculated via the Leonardo series, the 3<sup>rd</sup> formula.

}}


```Mathematica
L[n_] := 2 Fibonacci[n + 1] - 1; L /@ Range[25]
```


```txt
{1, 3, 5, 9, 15, 25, 41, 67, 109, 177, 287, 465, 753, 1219, 1973, 3193, 5167, 8361, 13529, 21891, 35421, 57313, 92735, 150049, 242785}
```



## min

{{works with|min|0.19.3}}

```min
(over over + rolldown pop pick +) :next
(('print dip " " print! next) 25 times newline) :leo

"First 25 Leonardo numbers:" puts!
1 1 1 leo
"First 25 Leonardo numbers with add=0, L(0)=0, L(1)=1:" puts!
0 0 1 leo
```

{{out}}

```txt

First 25 Leonardo numbers:
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
First 25 Leonardo numbers with add=0, L(0)=0, L(1)=1:
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```


=={{header|Modula-2}}==

```modula2
MODULE Leonardo;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE leonardo(a,b,step,num : INTEGER);
VAR
    buf : ARRAY[0..63] OF CHAR;
    i,temp : INTEGER;
BEGIN
    FOR i:=1 TO num DO
        IF i=1 THEN
            FormatString(" %i", buf, a);
            WriteString(buf)
        ELSIF i=2 THEN
            FormatString(" %i", buf, b);
            WriteString(buf)
        ELSE
            FormatString(" %i", buf, a+b+step);
            WriteString(buf);

            temp := a;
            a := b;
            b := temp + b + step
        END
    END;
    WriteLn
END leonardo;

BEGIN
    leonardo(1,1,1,25);
    leonardo(0,1,0,25);

    ReadChar
END Leonardo.
```



## Nim


```Nim
import strformat

proc leonardoNumbers(count: int, L0: int = 1,
                     L1: int = 1, ADD: int = 1) =
  var t = 0
  var (L0_loc, L1_loc) = (L0, L1)
  for i in 0..<count:
    write(stdout, fmt"{L0_loc:7}")
    t = L0_loc + L1_loc + ADD
    L0_loc = L1_loc
    L1_loc = t
    if i mod 5 == 4:
      write(stdout, "\n")
  write(stdout, "\n")

echo "Leonardo Numbers:"
leonardoNumbers(25)
echo "Fibonacci Numbers: "
leonardoNumbers(25, 0, 1, 0)
```

{{out}}

```txt

Leonardo Numbers:
      1      1      3      5      9
     15     25     41     67    109
    177    287    465    753   1219
   1973   3193   5167   8361  13529
  21891  35421  57313  92735 150049

Fibonacci Numbers:
      0      1      1      2      3
      5      8     13     21     34
     55     89    144    233    377
    610    987   1597   2584   4181
   6765  10946  17711  28657  46368

```



## Perl



```perl
no warnings 'experimental::signatures';
use feature 'signatures';

sub leonardo ($n, $l0 = 1, $l1 = 1, $add = 1) {
  ($l0, $l1) = ($l1, $l0+$l1+$add)  for 1..$n;
  $l0;
}

my @L = map { leonardo($_) } 0..24;
print "Leonardo[1,1,1]: @L\n";
my @F = map { leonardo($_,0,1,0) } 0..24;
print "Leonardo[0,1,0]: @F\n";
```

{{out}}

```txt
Leonardo[1,1,1]: 1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
Leonardo[0,1,0]: 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## Perl 6



```perl6
sub 𝑳 ( $𝑳0 = 1, $𝑳1 = 1, $𝑳add = 1 ) { $𝑳0, $𝑳1, { $^n2 + $^n1 + $𝑳add } ... * }

# Part 1
say "The first 25 Leonardo numbers:";
put 𝑳()[^25];

# Part 2
say "\nThe first 25 numbers using 𝑳0 of 0, 𝑳1 of 1, and adder of 0:";
put 𝑳( 0, 1, 0 )[^25];
```

{{out}}

```txt
The first 25 Leonardo numbers:
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

The first 25 numbers using 𝑳0 of 0, 𝑳1 of 1, and adder of 0:
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368
```



## Phix


```Phix
function leonardo(integer n, l1=1, l2=1, step=1)
--return the first n leonardo numbers, starting {l1,l2}, with step as the add number
sequence res = {l1,l2}
    while length(res)<n do
        res = append(res,res[$]+res[$-1]+step)
    end while
    return res
end function
?{"Leonardo",leonardo(25)}
?{"Fibonacci",leonardo(25,0,1,0)}
```

{{out}}

```txt

{"Leonardo",{1,1,3,5,9,15,25,41,67,109,177,287,465,753,1219,1973,3193,5167,8361,13529,21891,35421,57313,92735,150049}}
{"Fibonacci",{0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368}}

```



## PicoLisp


```PicoLisp
(de leo (A B C)
   (default A 1  B 1  C 1)
   (make
      (do 25
         (inc
            'B
            (+ (link (swap 'A B)) C) ) ) ) )

(println 'Leonardo (leo))
(println 'Fibonacci (leo 0 1 0))
```

{{out}}

```txt
Leonardo (1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049)
Fibonacci (0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368)
```



## PureBasic


```purebasic

EnableExplicit

#N = 25

Procedure leon_R(a.i, b.i, s.i = 1, n.i = #N)

  If n>2
    Print(Space(1) + Str(a + b + s))
    ProcedureReturn leon_R(b, a + b + s, s, n-1)
  EndIf

EndProcedure

If OpenConsole()

  Define r$

  Print("Enter first two Leonardo numbers and increment step (separated by space) : ")
  r$ = Input()
  PrintN("First " + Str(#N) + " Leonardo numbers : ")
  Print(StringField(r$, 1, Chr(32)) + Space(1) +
        StringField(r$, 2, Chr(32)))

  leon_R(Val(StringField(r$, 1, Chr(32))),
         Val(StringField(r$, 2, Chr(32))),
         Val(StringField(r$, 3, Chr(32))))

  r$ = Input()
EndIf

```

{{out}}

```txt

Enter first two Leonardo numbers and increment step (separated by space) : 1 1 1
First 25 Leonardo numbers :
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
Enter first two Leonardo numbers and increment step (separated by space) : 0 1 0
First 25 Leonardo numbers :
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## Python


### Finite iteration


```python
def Leonardo(L_Zero, L_One, Add, Amount):
    terms = [L_Zero,L_One]
    while len(terms) < Amount:
        new = terms[-1] + terms[-2]
        new += Add
        terms.append(new)
    return terms

out = ""
print "First 25 Leonardo numbers:"
for term in Leonardo(1,1,1,25):
    out += str(term) + " "
print out

out = ""
print "Leonardo numbers with fibonacci parameters:"
for term in Leonardo(0,1,0,25):
    out += str(term) + " "
print out

```

{{out}}

```txt

First 25 Leonardo numbers:
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
Leonardo numbers with fibonacci parameters:
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```


===Non-finite generation===
Or, for a non-finite stream of Leonardos, we can use a Python generator:
{{Works with|Python|3}}

```python
'''Leonardo numbers'''

from functools import (reduce)
from itertools import (islice)


# leo :: Int -> Int -> Int -> Generator [Int]
def leo(L0, L1, delta):
    '''A number series of the
       Leonardo and Fibonacci pattern,
       where L0 and L1 are the first two terms,
       and delta = 1 for (L0, L1) == (1, 1)
       yields the Leonardo series, while
       delta = 0 defines the Fibonacci series.'''
    (x, y) = (L0, L1)
    while True:
        yield x
        (x, y) = (y, x + y + delta)


# main :: IO()
def main():
    '''Tests.'''

    print('\n'.join([
        'First 25 Leonardo numbers:',
        folded(16)(take(25)(
            leo(1, 1, 1)
        )),
        '',
        'First 25 Fibonacci numbers:',
        folded(16)(take(25)(
            leo(0, 1, 0)
        ))
    ]))


# FORMATTING ----------------------------------------------

# folded :: Int -> [a] -> String
def folded(n):
    '''Long list folded to rows of n terms each.'''
    return lambda xs: '[' + ('\n '.join(
        str(ns)[1:-1] for ns in chunksOf(n)(xs)
    ) + ']')


# GENERIC -------------------------------------------------

# chunksOf :: Int -> [a] -> [[a]]
def chunksOf(n):
    '''A series of lists of length n,
       subdividing the contents of xs.
       Where the length of xs is not evenly divible,
       the final list will be shorter than n.'''
    return lambda xs: reduce(
        lambda a, i: a + [xs[i:n + i]],
        range(0, len(xs), n), []
    ) if 0 < n else []


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
First 25 Leonardo numbers:
[1, 1, 3, 5, 9, 15, 25, 41, 67, 109, 177, 287, 465, 753, 1219, 1973
 3193, 5167, 8361, 13529, 21891, 35421, 57313, 92735, 150049]

First 25 Fibonacci numbers:
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610
 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368]
```



## R


```rsplus

leonardo_numbers <- function(add = 1, l0 = 1, l1 = 1, how_many = 25) {
	result <- c(l0, l1)
	for (i in 3:how_many)
		result <- append(result, result[[i - 1]] + result[[i - 2]] + add)
	result
}
cat("First 25 Leonardo numbers\n")
cat(leonardo_numbers(), "\n")

cat("First 25 Leonardo numbers from 0, 1 with add number = 0\n")
cat(leonardo_numbers(0, 0, 1), "\n")

```

{{out}}

```txt

First 25 Leonardo numbers
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
First 25 Leonardo numbers from 0, 1 with add number = 0
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```


## Racket


```racket
#lang racket
(define (Leonardo n #:L0 (L0 1) #:L1 (L1 1) #:1+ (1+ 1))
  (cond [(= n 0) L0]
        [(= n 1) L1]
        [else
         (let inr ((n (- n 2)) (L_n-2 L0) (L_n-1 L1))
           (let ((L_n (+ L_n-1 L_n-2 1+)))
             (if (zero? n) L_n (inr (sub1 n) L_n-1 L_n))))]))

(module+ main
  (map Leonardo (range 25))
  (map (curry Leonardo #:L0 0 #:L1 1 #:1+ 0) (range 25)))

(module+ test
  (require rackunit)
  (check-equal? (Leonardo 0) 1)
  (check-equal? (Leonardo 1) 1)
  (check-equal? (Leonardo 2) 3)
  (check-equal? (Leonardo 3) 5))
```


{{out}}

```txt
'(1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049)
'(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368)
```



## REXX


```rexx
/*REXX pgm computes Leonardo numbers, allowing the specification of L(0), L(1), and ADD#*/
numeric digits 500                               /*just in case the user gets ka-razy.  */
@.=1                                             /*define the default for the  @. array.*/
parse arg N L0 L1 a# .                           /*obtain optional arguments from the CL*/
if  N =='' |  N ==","  then    N= 25             /*Not specified?  Then use the default.*/
if L0\=='' & L0\==","  then  @.0= L0             /*Was     "         "   "   "   value. */
if L1\=='' & L1\==","  then  @.1= L1             /* "      "         "   "   "     "    */
if a#\=='' & a#\==","  then  @.a= a#             /* "      "         "   "   "     "    */
say 'The first '   N   " Leonardo numbers are:"  /*display a title for the output series*/
if @.0\==1 | @.1\==1  then say 'using '     @.0     " for L(0)"
if @.0\==1 | @.1\==1  then say 'using '     @.1     " for L(1)"
if @.a\==1            then say 'using '     @.a     " for the  add  number"
say                                              /*display blank line before the output.*/
$=                                               /*initialize the output line to "null".*/
             do j=0  for N                       /*construct a list of Leonardo numbers.*/
             if j<2  then z=@.j                  /*for the 1st two numbers, use the fiat*/
                     else do                     /*··· otherwise, compute the Leonardo #*/
                          _=@.0                  /*save the old primary Leonardo number.*/
                          @.0=@.1                /*store the new primary number in old. */
                          @.1=@.0  +  _  +  @.a  /*compute the next Leonardo number.    */
                          z=@.1                  /*store the next Leonardo number in Z. */
                          end                    /* [↑]  only 2 Leonardo #s are stored. */
             $=$ z                               /*append the just computed # to $ list.*/
             end   /*j*/                         /* [↓]  elide the leading blank in  $. */
say strip($)                                     /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

The first  25  Leonardo numbers are:

1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

```


{{out|output|text=  when using the input of:     <tt> 12   0   1   0 </tt>}}

```txt

The first  25  Leonardo numbers are:
using  0  for L(0)
using  1  for L(1)
using  0  for the  add  number

0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## Ring



```ring

# Project : Leanardo numbers

n0 = 1
n1 = 1
add = 1
see "First 25 Leonardo numbers:" + nl
leonardo()
n0 = 1
n1 = 1
add = 0
see "First 25 Leonardo numbers with L(0) = 0, L(1) = 1, step = 0 (fibonacci numbers):" + nl
see "" + add + " "
leonardo()

func leonardo()
        see "" + n0 + " " + n1
        for i=3 to 25
              temp=n1
              n1=n0+n1+add
              n0=temp
             see " "+ n1
        next
        see nl

```

Output:

```txt

First 25 Leonardo numbers:
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
First 25 Leonardo numbers with L(0) = 0, L(1) = 1, step = 0 (fibonacci numbers):
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025

```



## Ruby

Enumerators are nice for this.

```ruby
def leonardo(l0=1, l1=1, add=1)
  return to_enum(__method__,l0,l1,add) unless block_given?
  loop do
    yield l0
    l0, l1 = l1, l0+l1+add
  end
end

p leonardo.take(25)
p leonardo(0,1,0).take(25)

```

{{out}}

```txt
[1, 1, 3, 5, 9, 15, 25, 41, 67, 109, 177, 287, 465, 753, 1219, 1973, 3193, 5167, 8361, 13529, 21891, 35421, 57313, 92735, 150049]
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368]

```


## Run BASIC


```Runbasic
sqliteconnect #mem, ":memory:"
#mem execute("CREATE TABLE lno (name,L0,L1,ad)")
#mem execute("INSERT INTO lno VALUES('Leonardo',1,1,1),('Fibonacci',0,1,0);")
#mem execute("SELECT * FROM lno")
for j = 1 to 2
#row  = #mem #nextrow()
name$ = #row name$()
L0    = #row L0()
L1    = #row L1()
ad    = #row ad()
print :print name$;" add=";ad :print" ";L0;" ";L1;" ";
for i = 3 to 25
  temp  = L1
  L1    = L0 + L1 + ad
  L0    = temp
  print L1;" ";
next i
next j
end
```


```txt
Leonardo add=1
 1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
Fibonacci add=0
 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## Scala


```scala
def leo( n:Int, n1:Int=1, n2:Int=1, addnum:Int=1 ) : BigInt = n match {
  case 0 => n1
  case 1 => n2
  case n => leo(n - 1, n1, n2, addnum) + leo(n - 2, n1, n2, addnum) + addnum
}

{
println( "The first 25 Leonardo Numbers:")
(0 until 25) foreach { n => print( leo(n) + " " ) }

println( "\n\nThe first 25 Fibonacci Numbers:")
(0 until 25) foreach { n => print( leo(n, n1=0, n2=1, addnum=0) + " " ) }
}

```

{{out}}

```txt
The first 25 Leonardo Numbers:
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

The first 25 Fibonacci Numbers:
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: leonardo (in var integer: l0, in var integer: l1, in integer: add, in integer: count) is func
  local
    var integer: temp is 0;
  begin
    for count do
      write(" " <& l0);
      temp := l0 + l1 + add;
      l0 := l1;
      l1 := temp;
    end for;
    writeln;
  end func;

const proc: main is func
  begin
    write("Leonardo Numbers:");
    leonardo(1, 1, 1, 25);
    write("Fibonacci Numbers:");
    leonardo(0, 1, 0, 25);
  end func;
```


{{out}}

```txt

Leonardo Numbers: 1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
Fibonacci Numbers: 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```



## Sidef


```ruby
func 𝑳(n, 𝑳0 = 1, 𝑳1 = 1, 𝑳add = 1) {
    { (𝑳0, 𝑳1) = (𝑳1, 𝑳0 + 𝑳1 + 𝑳add) } * n
    return 𝑳0
}

say "The first 25 Leonardo numbers:"
say 25.of { 𝑳(_) }

say "\nThe first 25 numbers using 𝑳0 of 0, 𝑳1 of 1, and adder of 0:"
say 25.of { 𝑳(_, 0, 1, 0) }
```

{{out}}

```txt

The first 25 Leonardo numbers:
[1, 1, 3, 5, 9, 15, 25, 41, 67, 109, 177, 287, 465, 753, 1219, 1973, 3193, 5167, 8361, 13529, 21891, 35421, 57313, 92735, 150049]

The first 25 numbers using 𝑳0 of 0, 𝑳1 of 1, and adder of 0:
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368]

```



## VBA



```vb

Option Explicit

Private Sub LeonardoNumbers()
Dim L, MyString As String
    Debug.Print "First 25 Leonardo numbers :"
    L = Leo_Numbers(25, 1, 1, 1)
    MyString = Join(L, "; ")
    Debug.Print MyString
    Debug.Print "First 25 Leonardo numbers from 0, 1 with add number = 0"
    L = Leo_Numbers(25, 0, 1, 0)
    MyString = Join(L, "; ")
    Debug.Print MyString
    Debug.Print "If the first prarameter is too small :"
    L = Leo_Numbers(1, 0, 1, 0)
    MyString = Join(L, "; ")
    Debug.Print MyString
End Sub

Public Function Leo_Numbers(HowMany As Long, L_0 As Long, L_1 As Long, Add_Nb As Long)
Dim N As Long, Ltemp

    If HowMany > 1 Then
        ReDim Ltemp(HowMany - 1)
        Ltemp(0) = L_0: Ltemp(1) = L_1
        For N = 2 To HowMany - 1
             Ltemp(N) = Ltemp(N - 1) + Ltemp(N - 2) + Add_Nb
        Next N
    Else
        ReDim Ltemp(0)
        Ltemp(0) = "The first parameter is too small"
    End If
    Leo_Numbers = Ltemp
End Function

```

{{out}}

```txt
First 25 Leonardo numbers :
1; 1; 3; 5; 9; 15; 25; 41; 67; 109; 177; 287; 465; 753; 1219; 1973; 3193; 5167; 8361; 13529; 21891; 35421; 57313; 92735; 150049
First 25 Leonardo numbers from 0, 1 with add number = 0
0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610; 987; 1597; 2584; 4181; 6765; 10946; 17711; 28657; 46368
If the first prarameter is too small :
The first parameter is too small
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Iterator Function Leonardo(Optional L0 = 1, Optional L1 = 1, Optional add = 1) As IEnumerable(Of Integer)
        While True
            Yield L0
            Dim t = L0 + L1 + add
            L0 = L1
            L1 = t
        End While
    End Function

    Sub Main()
        Console.WriteLine(String.Join(" ", Leonardo().Take(25)))
        Console.WriteLine(String.Join(" ", Leonardo(0, 1, 0).Take(25)))
    End Sub

End Module
```

{{out}}

```txt
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368
```



## zkl


```zkl
fcn leonardoNumber(n, n1=1,n2=1,addnum=1){
   if(n==0) return(n1);
   if(n==1) return(n2);
   self.fcn(n-1,n1,n2,addnum) + self.fcn(n-2,n1,n2,addnum) + addnum
}
```


```zkl
println("The first 25 Leonardo Numbers:");
foreach n in (25){ print(leonardoNumber(n)," ") }
println("\n");

println("The first 25 Fibonacci Numbers:");
foreach n in (25){ print(leonardoNumber(n, 0,1,0)," ") }
println();
```

{{out}}

```txt

The first 25 Leonardo Numbers:
1 1 3 5 9 15 25 41 67 109 177 287 465 753 1219 1973 3193 5167 8361 13529 21891 35421 57313 92735 150049

The first 25 Fibonacci Numbers:
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368

```

