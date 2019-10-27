+++
title = "Sierpinski triangle"
description = ""
date = 2019-09-23T13:42:14Z
aliases = []
[extra]
id = 2705
[taxonomies]
categories = []
tags = []
+++

{{task|Fractals}}

;Task
Produce an ASCII representation of a [[wp:Sierpinski triangle|Sierpinski triangle]] of order '''N'''.  


;Example
The Sierpinski triangle of order '''4''' should look like this: 

```txt

                       *
                      * *
                     *   *
                    * * * *
                   *       *
                  * *     * *
                 *   *   *   *
                * * * * * * * *
               *               *
              * *             * *
             *   *           *   *
            * * * *         * * * *
           *       *       *       *
          * *     * *     * *     * *
         *   *   *   *   *   *   *   *
        * * * * * * * * * * * * * * * *

```



;Related tasks
* [[Sierpinski triangle/Graphical]] for graphics images of this pattern.  
* [[Sierpinski carpet]]





## ACL2


```Lisp
(defun pascal-row (prev)
   (if (endp (rest prev))
       (list 1)
       (cons (+ (first prev) (second prev))
             (pascal-row (rest prev)))))

(defun pascal-triangle-r (rows prev)
   (if (zp rows)
       nil
       (let ((curr (cons 1 (pascal-row prev))))
          (cons curr (pascal-triangle-r (1- rows) curr)))))

(defun pascal-triangle (rows)
   (cons (list 1)
         (pascal-triangle-r rows (list 1))))

(defun print-odds-row (row)
   (if (endp row)
       (cw "~%")
       (prog2$ (cw (if (oddp (first row)) "[]" "  "))
               (print-odds-row (rest row)))))

(defun print-spaces (n)
   (if (zp n)
       nil
       (prog2$ (cw " ")
               (print-spaces (1- n)))))

(defun print-odds (triangle height)
   (if (endp triangle)
       nil
       (progn$ (print-spaces height)
               (print-odds-row (first triangle))
               (print-odds (rest triangle) (1- height)))))

(defun print-sierpenski (levels)
   (let ((height (1- (expt 2 levels))))
      (print-odds (pascal-triangle height)
                  height)))
```



## Ada

This Ada example creates a string of the binary value for each line, converting the '0' values to spaces.

```ada
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;

procedure Sieteri_Triangles is
   subtype Practical_Order is Unsigned_32 range 0..4;
   
   
   function Pow(X : Unsigned_32; N : Unsigned_32) return Unsigned_32 is
   begin
      if N = 0 then
         return 1;
      else
         return X * Pow(X, N - 1);
      end if;
   end Pow;
   
   procedure Print(Item : Unsigned_32) is
      use Ada.Strings.Fixed;
      package Ord_Io is new Ada.Text_Io.Modular_Io(Unsigned_32);
      use Ord_Io;
      Temp : String(1..36) := (others => ' ');
      First : Positive;
      Last  : Positive;
   begin
      Put(To => Temp, Item => Item, Base => 2);
      First := Index(Temp, "#") + 1;
      Last  := Index(Temp(First..Temp'Last), "#") - 1;
      for I in reverse First..Last loop
         if Temp(I) = '0' then
            Put(' ');
         else
            Put(Temp(I));
         end if;
      end loop;
      New_Line;
   end Print;
   
   procedure Sierpinski (N : Practical_Order) is
      Size : Unsigned_32 := Pow(2, N);
      V : Unsigned_32 := Pow(2, Size);
   begin
      for I in 0..Size - 1 loop
         Print(V);
         V := Shift_Left(V, 1) xor Shift_Right(V,1);
      end loop;
   end Sierpinski;
   
begin
   for N in Practical_Order loop
      Sierpinski(N);
   end loop;
end Sieteri_Triangles;
```


alternative using modular arithmetic:

```Ada
with Ada.Command_Line;
with Ada.Text_IO;

procedure Main is
   subtype Order is Natural range 1 .. 32;
   type Mod_Int is mod 2 ** Order'Last;

   procedure Sierpinski (N : Order) is
   begin
      for Line in Mod_Int range 0 .. 2 ** N - 1 loop
         for Col in Mod_Int range 0 .. 2 ** N - 1 loop
            if (Line and Col) = 0 then
               Ada.Text_IO.Put ('X');
            else
               Ada.Text_IO.Put (' ');
            end if;
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Sierpinski;

   N : Order := 4;
begin
   if Ada.Command_Line.Argument_Count = 1 then
      N := Order'Value (Ada.Command_Line.Argument (1));
   end if;
   Sierpinski (N);
end Main;
```

{{out}}

```txt
XXXXXXXXXXXXXXXX
X X X X X X X X
XX  XX  XX  XX
X   X   X   X
XXXX    XXXX
X X     X X
XX      XX
X       X
XXXXXXXX
X X X X
XX  XX
X   X
XXXX
X X
XX
X
```



## ALGOL 68

{{trans|python}}
{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - test missing transput}} -->

```algol68
PROC sierpinski = (INT n)[]STRING: (
    FLEX[0]STRING d := "*";
    FOR i TO n DO
        [UPB d * 2]STRING next;
        STRING sp := " " * (2 ** (i-1));
        FOR x TO UPB d DO
          STRING dx = d[x];
          next[x] := sp+dx+sp;
          next[UPB d+x] := dx+" "+dx
        OD;
        d := next
    OD;
    d
);

printf(($gl$,sierpinski(4)))
```





## AppleScript

{{Trans|JavaScript}}
{{Trans|Haskell}}
Centering any previous triangle block over two adjacent duplicates:

```AppleScript
-- SIERPINKSI TRIANGLE -------------------------------------------------------

-- sierpinski :: Int -> [String]
on sierpinski(n)
    if n > 0 then
        set previous to sierpinski(n - 1)
        set padding to replicate(2 ^ (n - 1), space)
        
        script alignedCentre
            on |λ|(s)
                concat(padding & s & padding)
            end |λ|
        end script
        
        script adjacentDuplicates
            on |λ|(s)
                unwords(replicate(2, s))
            end |λ|
        end script
        
        -- Previous triangle block centered,
        -- and placed on 2 adjacent duplicates.
        map(alignedCentre, previous) & map(adjacentDuplicates, previous)
    else
        {"*"}
    end if
end sierpinski


-- TEST ----------------------------------------------------------------------
on run
    unlines(sierpinski(4))
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

-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}
    
    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- unlines, unwords :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines

on unwords(xs)
    intercalate(space, xs)
end unwords
```

{{Out}}

```txt
               *               
              * *              
             *   *             
            * * * *            
           *       *           
          * *     * *          
         *   *   *   *         
        * * * * * * * *        
       *               *       
      * *             * *      
     *   *           *   *     
    * * * *         * * * *    
   *       *       *       *   
  * *     * *     * *     * *  
 *   *   *   *   *   *   *   * 
* * * * * * * * * * * * * * * *
```


Or generating each line as an XOR / Rule 90 / Pascal triangle rewrite of the previous line.
{{Trans|JavaScript}}

```AppleScript
-- SIERPINSKI TRIANGLE BY XOR / RULE 90 --------------------------------------

-- sierpinskiTriangle :: Int -> String
on sierpinskiTriangle(intOrder)
    
    -- A Sierpinski triangle of order N
    -- is a Pascal triangle (of N^2 rows)
    -- mod 2
    
    -- pascalModTwo :: Int -> [[String]]
    script pascalModTwo
        on |λ|(intRows)
            
            -- addRow [[Int]] -> [[Int]]
            script addRow
                
                -- nextRow :: [Int] -> [Int]
                on nextRow(row)
                    -- The composition of AsciiBinary . mod two . add
                    -- is reduced here to a rule from
                    -- two parent characters above,
                    -- to the child character below.
                    
                    -- Rule 90 also reduces to this XOR relationship 
                    -- between left and right neighbours.
                    
                    -- rule :: Character -> Character -> Character
                    script rule
                        on |λ|(a, b)
                            if a = b then
                                space
                            else
                                "*"
                            end if
                        end |λ|
                    end script
                    
                    zipWith(rule, {" "} & row, row & {" "})
                end nextRow
                
                on |λ|(xs)
                    xs & {nextRow(item -1 of xs)}
                end |λ|
            end script
            
            foldr(addRow, {{"*"}}, enumFromTo(1, intRows - 1))
        end |λ|
    end script
    
    -- The centring foldr (fold right) below starts from the end of the list, 
    -- (the base of the triangle) which has zero indent.
    
    -- Each preceding row has one more indent space than the row below it.
    
    script centred
        on |λ|(sofar, row)
            set strIndent to indent of sofar
            
            {triangle:strIndent & intercalate(space, row) & linefeed & ¬
                triangle of sofar, indent:strIndent & space}
        end |λ|
    end script
    
    triangle of foldr(centred, {triangle:"", indent:""}, ¬
        pascalModTwo's |λ|(intOrder ^ 2))
    
end sierpinskiTriangle


-- TEST ----------------------------------------------------------------------
on run
    
    set strTriangle to sierpinskiTriangle(4)
    
    set the clipboard to strTriangle
    strTriangle
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```txt
               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *

```



## ATS


```ATS

(* ****** ****** *)
//
// How to compile:
//
// patscc -DATS_MEMALLOC_LIBC -o sierpinski sierpinski.dats
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

#define SIZE 16

implement
main0 () =
{
//
var x: int
//
val () =
for (x := SIZE-1; x >= 0; x := x-1)
{
  var i: int
  val () =
  for (i := 0; i < x; i := i+1)
  {
    val () = print_char(' ')
  }
  var y: int
  val () =
  for (y := 0; y + x < SIZE; y := y+1)
  {
    val y = g0int2uint_int_uint(y)
    val x = g0int2uint_int_uint(x)
    val () = print_string(if (x land y) != 0 then "  " else "* ")
  }
  val ((*flushed*)) = print_newline()
}
//
} (* end of [main0] *)

```



## AutoHotkey

ahk [http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=150 discussion]

```autohotkey
Loop 6
   MsgBox % Triangle(A_Index)

Triangle(n,x=0,y=1) { ; Triangle(n) -> string of dots and spaces of Sierpinski triangle
   Static t, l                                  ; put chars in a static string
   If (x < 1) {                                 ; when called with one parameter
      l := 2*x := 1<<(n-1)                      ; - compute location, string size
      VarSetCapacity(t,l*x,32)                  ; - allocate memory filled with spaces
      Loop %x%
         NumPut(13,t,A_Index*l-1,"char")        ; - new lines in the end of rows
   }
   If (n = 1)                                   ; at the bottom of recursion
      Return t, NumPut(46,t,x-1+(y-1)*l,"char") ; - write "." (better at proportional fonts)
   u := 1<<(n-2)
   Triangle(n-1,x,y)                            ; draw smaller triangle here
   Triangle(n-1,x-u,y+u)                        ; smaller triangle down-left
   Triangle(n-1,x+u,y+u)                        ; smaller triangle down right
   Return t
}
```
 


## AWK


```AWK
# WST.AWK - Waclaw Sierpinski's triangle contributed by Dan Nielsen
# syntax: GAWK -f WST.AWK [-v X=anychar] iterations
# example: GAWK -f WST.AWK -v X=* 2
BEGIN {
    n = ARGV[1] + 0 # iterations
    if (n !~ /^[0-9]+$/) { exit(1) }
    if (n == 0) { width = 3 }
    row = split("X,X X,X   X,X X X X",A,",") # seed the array
    for (i=1; i<=n; i++) { # build triangle
      width = length(A[row])
      for (j=1; j<=row; j++) {
        str = A[j]
        A[j+row] = sprintf("%-*s %-*s",width,str,width,str)
      }
      row *= 2
    }
    for (j=1; j<=row; j++) { # print triangle
      if (X != "") { gsub(/X/,substr(X,1,1),A[j]) }
      sub(/ +$/,"",A[j])
      printf("%*s%s\n",width-j+1,"",A[j])
    }
    exit(0)
}
```


=={{header|BASH (feat. sed & tr)}}==
This version completely avoids any number-theoretic workarounds.
Instead, it repeatedly replaces characters by "blocks of characters".
The strategy is in no way bash-specific, it would work with any 
other language just as well, but is particularly well suited for
tools like sed and tr.

```bash

#!/bin/bash

# Basic principle:
# 
#
#  x ->  dxd       d -> dd      s -> s
#        xsx            dd           s
#
# In the end all 'd' and 's' are removed.
# 0x7F800000
function rec(){
  if [ $1 == 0 ]
  then 
    echo "x"
  else
    rec $[ $1 - 1 ] | while read line ; do 
      echo "$line" | sed "s/d/dd/g" | sed "s/x/dxd/g"
      echo "$line" | sed "s/d/dd/g" | sed "s/x/xsx/g"
    done
  fi
}

rec $1 | tr 'dsx' '  *'

```



## BASIC

{{works with|QBasic}}
{{works with|FreeBASIC}}
<!-- {{works with|RapidQ}} doesn't work for me -- Erik Siers, 12 March 2012 -->


```qbasic
DECLARE SUB triangle (x AS INTEGER, y AS INTEGER, length AS INTEGER, n AS INTEGER)

CLS
triangle 1, 1, 16, 5

SUB triangle (x AS INTEGER, y AS INTEGER, length AS INTEGER, n AS INTEGER)
    IF n = 0 THEN
        LOCATE y, x: PRINT "*";
    ELSE
        triangle x, y + length, length / 2, n - 1
        triangle x + length, y, length / 2, n - 1
        triangle x + length * 2, y + length, length / 2, n - 1
    END IF
END SUB
```


Note: The total height of the triangle is 2 * parameter ''length''. It should be power of two so that the pattern matches evenly with the character cells. Value 16 will thus create pattern of 32 lines.

=
## BBC BASIC
=

```bbcbasic
      MODE 8
      OFF
      
      order% = 5
      PROCsierpinski(0, 0, 2^(order%-1))
      REPEAT UNTIL GET
      END
      
      DEF PROCsierpinski(x%, y%, l%)
      IF l% = 0 THEN
        PRINT TAB(x%,y%) "*";
      ELSE
        PROCsierpinski(x%, y%+l%, l% DIV 2)
        PROCsierpinski(x%+l%, y%, l% DIV 2)
        PROCsierpinski(x%+l%+l%, y%+l%, l% DIV 2)
      ENDIF
      ENDPROC
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Triangle.bas"
110 TEXT 40
120 CALL TRIANGLE(1,1,8)
130 DEF TRIANGLE(X,Y,L)
140   IF L=0 THEN
150     PRINT AT Y,X:"*"
160   ELSE
170     CALL TRIANGLE(X,Y+L,INT(L/2))
180     CALL TRIANGLE(X+L,Y,INT(L/2))
190     CALL TRIANGLE(X+2*L,Y+L,INT(L/2))
200   END IF
210 END DEF
```



## Befunge


This is a version of the cellular automaton (''rule 90'') construction. The order, ''N'', is specified by the first number on the stack. It uses a single line of the playfield for the cell buffer, so the upper limit for ''N'' should be 5 on a standard Befunge-93 implementation. Interpreters with poor memory handling may not work with anything over 3, though, and a Befunge-98 interpreter should theoretically be unlimited.


```befunge>41+2
\#*1#2-#<:#\_$:1+v
v:$_:#`0#\\#00#:p#->#1<
>2/1\0p:2/\::>1-:>#v_1v
>8#4*#*+#+,#5^#5g0:<  1
vg11<\*g11!:g 0-1:::<p<
>!*+!!\0g11p\ 0p1-:#^_v
@$$_\#!:#::#-^#1\$,+55<
```



## C


```C>#include <stdio.h


#define SIZE (1 << 4)
int main()
{
	int x, y, i;
	for (y = SIZE - 1; y >= 0; y--, putchar('\n')) {
		for (i = 0; i < y; i++) putchar(' ');
		for (x = 0; x + y < SIZE; x++)
			printf((x & y) ? "  " : "* ");
	}
	return 0;
}
```



### Automaton

This solution uses a cellular automaton (''rule 90'') with a proper initial status.

```c>#include <stdio.h

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#ifndef _POSIX_C_SOURCE
char *strdup(const char *s)
{
  int l = strlen(s);
  char *r = malloc(l+1);
  memcpy(r, s, l+1);
  return r;
}
#endif

#define truth(X) ((X)=='*'?true:false)
void rule_90(char *evstr)
{
  int i;
  int l = strlen(evstr);
  bool s[3];
  char *cp = strdup(evstr);

  for(i=0;i < l; i++) {
    s[1] = truth(cp[i]);
    s[0] = (i-1) < 0 ? false : truth(cp[i-1]);
    s[2] = (i+1) < l ? truth(cp[i+1]) : false;
    if ( (s[0] && !s[2]) || (!s[0] && s[2]) ) {
      evstr[i] = '*';
    } else {
      evstr[i] = ' ';
    }
  }
  free(cp);
}
```



```c
void sierpinski_triangle(int n)
{
  int i;
  int l = 1<<(n+1);
  char *b = malloc(l+1);

  memset(b, ' ', l);
  b[l] = 0;
  b[l>>1] = '*';

  printf("%s\n", b);
  for(i=0; i < l/2-1;i++) {
    rule_90(b);
    printf("%s\n", b);
  }

  free(b);
}
```



```c
int main()
{
  sierpinski_triangle(4);
  return EXIT_SUCCESS;
}
```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Collections;

namespace RosettaCode {
    class SierpinskiTriangle {
        int len;
        BitArray b;

        public SierpinskiTriangle(int n) {
            if (n < 1) {
                throw new ArgumentOutOfRangeException("Order must be greater than zero");
            }
            len = 1 << (n+1);
            b = new BitArray(len+1, false);
            b[len>>1] = true;
        }

        public void Display() {
            for (int j = 0; j < len / 2; j++) {
                for (int i = 0; i < b.Count; i++) {
                    Console.Write("{0}", b[i] ? "*" : " ");
                }
                Console.WriteLine();
                NextGen();
            }
        }

        private void NextGen() {
            BitArray next = new BitArray(b.Count, false);
            for (int i = 0; i < b.Count; i++) {
                if (b[i]) {
                    next[i - 1] = next[i - 1] ^ true;
                    next[i + 1] = next[i + 1] ^ true;
                }
            }
            b = next;
        }
    }
}
```



```csharp
namespace RosettaCode {
    class Program {
        static void Main(string[] args) {
            SierpinskiTriangle t = new SierpinskiTriangle(4);
            t.Display();
        }
    }
}
```


{{trans|C}} 
{{works with|C sharp|C#|6.0+}}

```csharp
using static System.Console;
class Sierpinsky
{
    static void Main(string[] args)
    {
        int order;
        if(!int.TryParse(args.Length > 0 ? args[0] : "", out order)) order = 4;
        int size = (1 << order);
        for (int y = size - 1; y >= 0; y--, WriteLine())
        {
            for (int i = 0; i < y; i++) Write(' ');
            for (int x = 0; x + y < size; x++)
                Write((x & y) != 0 ? "  " : "* ");
        }
    }
}
```


{{trans|OCaml}} 
{{works with|C sharp|C#|3.0+}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    public static List<String> Sierpinski(int n)
    {
        var lines = new List<string> { "*" };
        string space = " ";

        for (int i = 0; i < n; i++)
        {
            lines = lines.Select(x => space + x + space)
                         .Concat(lines.Select(x => x + " " + x)).ToList();
            space += space;
        }

        return lines;
    }

    static void Main(string[] args)
    {
        foreach (string s in Sierpinski(4))
            Console.WriteLine(s);
    }
}
```


Or, with fold / reduce (a.k.a. aggregate):


```csharp
using System;
using System.Collections.Generic;
using System.Linq;
 
class Program
{
    static List<string> Sierpinski(int n)
    {
	return Enumerable.Range(0, n).Aggregate(
	   new List<string>(){"*"},
	     (p, i) => {
		string SPACE = " ".PadRight((int)Math.Pow(2, i));

		var temp =  new List<string>(from x in p select SPACE + x + SPACE);
		temp.AddRange(from x in p select x + " " + x);

		return temp;
	     }
	);
    }

    static void Main ()
    {
	foreach(string s in Sierpinski(4)) { Console.WriteLine(s); }
    }
}
```



## C++

{{works with|C++11}}
A STL-centric recursive solution that uses the new lambda functions in C++11.

```cpp>#include <iostream

#include <string>
#include <list>
#include <algorithm>
#include <iterator>

using namespace std;

template<typename OutIt>
void sierpinski(int n, OutIt result)
{
    if( n == 0 )
    {
        *result++ = "*";
    }
    else
    {
        list<string> prev;
        sierpinski(n-1, back_inserter(prev));

        string sp(1 << (n-1), ' ');
        result = transform(prev.begin(), prev.end(),
            result,
            [sp](const string& x) { return sp + x + sp; });
        transform(prev.begin(), prev.end(),
            result,
            [sp](const string& x) { return x + " " + x; });
    }
}

int main()
{
    sierpinski(4, ostream_iterator<string>(cout, "\n"));
    return 0;
}
```



## Clojure

{{trans|Ada}}
{{trans|Common Lisp}}
With a touch of Clojure's sequence handling.

```clojure
(ns example
  (:require [clojure.contrib.math :as math]))

; Length of integer in binary
; (copied from a private multimethod in clojure.contrib.math)
(defmulti #^{:private true} integer-length class)

(defmethod integer-length java.lang.Integer [n]
  (count (Integer/toBinaryString n)))
(defmethod integer-length java.lang.Long [n]
  (count (Long/toBinaryString n)))
(defmethod integer-length java.math.BigInteger [n]
  (count (.toString n 2)))

(defn sierpinski-triangle [order]
  (loop [size (math/expt 2 order)
         v    (math/expt 2 (- size 1))]
    (when (pos? size)
      (println
       (apply str (map #(if (bit-test v %) "*" " ")
		       (range (integer-length v)))))
      (recur 
       (dec size)
       (bit-xor (bit-shift-left v 1) (bit-shift-right v 1))))))

(sierpinski-triangle 4)
```



## COBOL

{{trans|Fortran}} and retains a more Fortran-like coding style than is really idiomatic in COBOL.

```cobol
identification division.
program-id. sierpinski-triangle-program.
data division.
working-storage section.
01  sierpinski.
    05 n              pic 99.
    05 i              pic 999.
    05 k              pic 999.
    05 m              pic 999.
    05 c              pic 9(18).
    05 i-limit        pic 999.
    05 q              pic 9(18).
    05 r              pic 9.
procedure division.
control-paragraph.
    move 4 to n.
    multiply n by 4 giving i-limit.
    subtract 1 from i-limit.
    perform sierpinski-paragraph
    varying i from 0 by 1 until i is greater than i-limit.
    stop run.
sierpinski-paragraph.
    subtract i from i-limit giving m.
    multiply m by 2 giving m.
    perform m times,
    display space with no advancing,
    end-perform.
    move 1 to c.
    perform inner-loop-paragraph
    varying k from 0 by 1 until k is greater than i.
    display ''.
inner-loop-paragraph.
    divide c by 2 giving q remainder r.
    if r is equal to zero then display '  * ' with no advancing.
    if r is not equal to zero then display '    ' with no advancing.
    compute c = c * (i - k) / (k + 1).
```



## Common Lisp


```lisp
(defun print-sierpinski (order)
  (loop with size = (expt 2 order)
        repeat size
        for v = (expt 2 (1- size)) then (logxor (ash v -1) (ash v 1))
        do (fresh-line)
           (loop for i below (integer-length v)
                 do (princ (if (logbitp i v) "*" " ")))))
```


Printing each row could also be done by printing the integer in base 2 and replacing zeroes with spaces: <tt>(princ (substitute #\Space #\0 (format nil "~%~2,vR" (1- (* 2 size)) v)))</tt>

Replacing the iteration with <tt>for v = 1 then (logxor v (ash v 1))</tt> produces a "right" triangle instead of an "equilateral" one.


Alternate approach:

```lisp
(defun sierpinski (n)
  (if (= n 0) '("*") 
      (nconc (mapcar (lambda (e) (format nil "~A~A~0@*~A" (make-string (expt 2 (1- n)) :initial-element #\ ) e)) (sierpinski (1- n)))
	     (mapcar (lambda (e) (format nil "~A ~A" e e)) (sierpinski (1- n))))))

(mapc #'print (sierpinski 4))
```



## D

===Run-time Version===

```d
void main() /*@safe*/ {
    import std.stdio, std.algorithm, std.string, std.array;

    enum level = 4;
    auto d = ["*"];
    foreach (immutable n; 0 .. level) {
        immutable sp = " ".replicate(2 ^^ n);
        d = d.map!(a => sp ~ a ~ sp).array ~
            d.map!(a => a ~ " " ~ a).array;
    }
    d.join('\n').writeln;
}
```

{{out}}

```txt
               *               
              * *              
             *   *             
            * * * *            
           *       *           
          * *     * *          
         *   *   *   *         
        * * * * * * * *        
       *               *       
      * *             * *      
     *   *           *   *     
    * * * *         * * * *    
   *       *       *       *   
  * *     * *     * *     * *  
 *   *   *   *   *   *   *   * 
* * * * * * * * * * * * * * * *
```


===Compile-time Version===
Same output.

```d
import std.string, std.range, std.algorithm;

string sierpinski(int level) pure nothrow /*@safe*/ {
    auto d = ["*"];
    foreach (immutable i; 0 .. level) {
        immutable sp = " ".replicate(2 ^^ i);
        d = d.map!(a => sp ~ a ~ sp).array ~
            d.map!(a => a ~ " " ~ a).array;
    }
    return d.join('\n');
}

pragma(msg, 4.sierpinski);
void main() {}
```



### Simple Version

{{trans|C}}
Same output.

```d
void showSierpinskiTriangle(in uint order) nothrow @safe @nogc {
    import core.stdc.stdio: putchar;

    foreach_reverse (immutable y; 0 .. 2 ^^ order) {
        foreach (immutable _; 0 .. y)
            ' '.putchar;
        foreach (immutable x; 0 .. 2 ^^ order - y) {
            putchar((x & y) ? ' ' : '*');
            ' '.putchar;
        }
        '\n'.putchar;
    }
}

void main() nothrow @safe @nogc {
    4.showSierpinskiTriangle;
}
```



### Alternative Version

This uses a different algorithm and shows a different output.

```d
import core.stdc.stdio: putchar;
import std.algorithm: swap;


void showSierpinskiTriangle(in uint nLevels) nothrow @safe
in {
    assert(nLevels > 0);
} body {
    alias Row = bool[];

    static void applyRules(in Row r1, Row r2) pure nothrow @safe @nogc {
        r2[0] = r1[0] || r1[1];
        r2[$ - 1] = r1[$ - 2] || r1[$ - 1];
        foreach (immutable i; 1 .. r2.length - 1)
            r2[i] = r1[i - 1] != r1[i] || r1[i] != r1[i + 1];
    }

    static void showRow(in Row r) nothrow @safe @nogc {
        foreach (immutable b; r)
            putchar(b ? '#' : ' ');
        '\n'.putchar;
    }

    immutable width = 2 ^^ (nLevels + 1) - 1;
    auto row1 = new Row(width);
    auto row2 = new Row(width);
    row1[width / 2] = true;

    foreach (immutable _; 0 .. 2 ^^ nLevels) {
        showRow(row1);
        applyRules(row1, row2);
        row1.swap(row2);
    }
}


void main() @safe nothrow {
    foreach (immutable i; 1 .. 6) {
        i.showSierpinskiTriangle;
        '\n'.putchar;
    }
}
```

{{out}}

```txt
 # 
###

   #   
  ###  
 ## ## 
#######

       #       
      ###      
     ## ##     
    #######    
   ##     ##   
  ####   ####  
 ##  ## ##  ## 
###############

               #               
              ###              
             ## ##             
            #######            
           ##     ##           
          ####   ####          
         ##  ## ##  ##         
        ###############        
       ##             ##       
      ####           ####      
     ##  ##         ##  ##     
    ########       ########    
   ##      ##     ##      ##   
  ####    ####   ####    ####  
 ##  ##  ##  ## ##  ##  ##  ## 
###############################

                               #                               
                              ###                              
                             ## ##                             
                            #######                            
                           ##     ##                           
                          ####   ####                          
                         ##  ## ##  ##                         
                        ###############                        
                       ##             ##                       
                      ####           ####                      
                     ##  ##         ##  ##                     
                    ########       ########                    
                   ##      ##     ##      ##                   
                  ####    ####   ####    ####                  
                 ##  ##  ##  ## ##  ##  ##  ##                 
                ###############################                
               ##                             ##               
              ####                           ####              
             ##  ##                         ##  ##             
            ########                       ########            
           ##      ##                     ##      ##           
          ####    ####                   ####    ####          
         ##  ##  ##  ##                 ##  ##  ##  ##         
        ################               ################        
       ##              ##             ##              ##       
      ####            ####           ####            ####      
     ##  ##          ##  ##         ##  ##          ##  ##     
    ########        ########       ########        ########    
   ##      ##      ##      ##     ##      ##      ##      ##   
  ####    ####    ####    ####   ####    ####    ####    ####  
 ##  ##  ##  ##  ##  ##  ##  ## ##  ##  ##  ##  ##  ##  ##  ## 
###############################################################
```



## Delphi

{{trans|DWScript}}

```delphi
program SierpinskiTriangle;

{$APPTYPE CONSOLE}

procedure PrintSierpinski(order: Integer);
var
  x, y, size: Integer;
begin
  size := (1 shl order) - 1;
  for y := size downto 0 do
  begin
    Write(StringOfChar(' ', y));
    for x := 0 to size - y do
    begin
      if (x and y) = 0 then
        Write('* ')
      else
        Write('  ');
    end;
    Writeln;
  end;
end;

begin
  PrintSierpinski(4);
end.
```



## DWScript

{{trans|E}}

```delphi
procedure PrintSierpinski(order : Integer);
var
   x, y, size : Integer;
begin
   size := (1 shl order)-1;
   for y:=size downto 0 do begin
      Print(StringOfChar(' ', y));
      for x:=0 to size-y do begin
         if (x and y)=0 then
            Print('* ')
         else Print('  ');
      end;
      PrintLn('');
   end;
end;

PrintSierpinski(4);

```



## E


```e
def printSierpinski(order, out) {
    def size := 2**order
    for y in (0..!size).descending() { 
        out.print(" " * y)
        for x in 0..!(size-y) {
            out.print((x & y).isZero().pick("* ", "  "))
        }
        out.println()
    }
}
```



```e
? printSierpinski(4, stdout)
```


Non-ASCII version (quality of results will depend greatly on text renderer):

```e
def printSierpinski(order, out) {
    def size := 2**order
    for y in (0..!size).descending() { 
        out.print("　" * y)
        for x in 0..!(size-y) {
            out.print((x & y).isZero().pick("◢◣", "　　"))
        }
        out.println()
    }
}
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule RC do
  def sierpinski_triangle(n) do
    f = fn(x) -> IO.puts "#{x}" end
    Enum.each(triangle(n, ["*"], " "), f)
  end
  
  defp triangle(0, down, _), do: down
  defp triangle(n, down, sp) do
    newDown = (for x <- down, do: sp<>x<>sp) ++ (for x <- down, do: x<>" "<>x)
    triangle(n-1, newDown, sp<>sp)
  end
end

RC.sierpinski_triangle(4)
```


## Elm

{{trans|Haskell}} 

```elm
import String exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)
import Html.App exposing (beginnerProgram)
import Result exposing (..)

sierpinski : Int -> List String
sierpinski n =
  let down n = sierpinski (n - 1)
      space n = repeat (2 ^ (n - 1)) " "
  in case n of
       0 -> ["*"]
       _ ->    List.map ((\st -> space n ++ st) << (\st -> st ++ space n)) (down n) 
            ++ List.map (join " " << List.repeat 2) (down n)

main = beginnerProgram { model = "4", view = view, update = update }

update newStr oldStr = newStr

view : String -> Html String
view levelString =
  div []
    ([ Html.form 
          [] 
          [ label [ myStyle ] [ text "Level: "]
          , input
            [ placeholder "triangle level."
            , value levelString
            , on "input" targetValue 
            , type' "number"
            , A.min "0"
            , myStyle
            ]
            []
          ]
     ] ++ 
     [ pre [] (levelString 
               |> toInt 
               |> withDefault 0 
               |> sierpinski  
               |> List.map (\s -> div [] [text s]))
     ])

myStyle : Attribute msg
myStyle =
  style
    [ ("height", "20px")
    , ("padding", "5px 0 0 5px")
    , ("font-size", "1em")
    , ("text-align", "left")
    ]
```


Link to live demo: http://dc25.github.io/sierpinskiElm/


## Erlang

{{trans|OCaml}} 

```erlang
-module(sierpinski).
-export([triangle/1]).

triangle(N) ->
    F = fun(X) -> io:format("~s~n",[X]) end,
    lists:foreach(F, triangle(N, ["*"], " ")).

triangle(0, Down, _) -> Down;
triangle(N, Down, Sp) ->
    NewDown = [Sp++X++Sp || X<-Down]++[X++" "++X || X <- Down],
    triangle(N-1, NewDown, Sp++Sp).
```



## Euphoria

{{trans|BASIC}}

```euphoria
procedure triangle(integer x, integer y, integer len, integer n)
    if n = 0 then
        position(y,x) puts(1,'*')
    else
        triangle (x,       y+len, floor(len/2), n-1)
        triangle (x+len,   y,     floor(len/2), n-1)
        triangle (x+len*2, y+len, floor(len/2), n-1)
    end if
end procedure

clear_screen()
triangle(1,1,8,4)
```


=={{header|F Sharp|F#}}==

```fsharp
let sierpinski n =
  let rec loop down space n =
    if n = 0 then
      down
    else
      loop (List.map (fun x -> space + x + space) down @
              List.map (fun x -> x + " " + x) down)
        (space + space)
        (n - 1)
  in loop ["*"] " " n
 
let () =
  List.iter (fun (i:string) -> System.Console.WriteLine(i)) (sierpinski 4)
```



## Factor

{{trans|OCaml}}

```factor
USING: io kernel math sequences ;
IN: sierpinski

: iterate-triangle ( triange spaces -- triangle' )
    [ [ dup surround ] curry map ]
    [ drop [ dup " " glue ] map ] 2bi append ;

: (sierpinski) ( triangle spaces n -- triangle' )
    dup 0 = [ 2drop "\n" join ] [
        [
            [ iterate-triangle ]
            [ nip dup append ] 2bi
        ] dip 1 - (sierpinski)
    ] if ;

: sierpinski ( n -- )
    [ { "*" } " " ] dip (sierpinski) print ;
```



## FALSE

{{incorrect|FALSE|Different results with different interpreters (like http://morphett.info/false/false.html and http://www.quirkster.com/iano/js/false-js.html}}

```false
[[$][$1&["*"]?$~1&[" "]?2/]#%"
"]s:                 { stars }
[$@$@|@@&~&]x:        { xor }
[1\[$][1-\2*\]#%]e:   { 2^n }
[e;!1\[$][\$s;!$2*x;!\1-]#%%]t:
4t;!
```



## Forth


```forth
: stars ( mask -- )
  begin
    dup 1 and if [char] * else bl then emit
    1 rshift  dup
  while space repeat drop ;

: triangle ( order -- )
  1 swap lshift   ( 2^order )
  1 over 0 do
    cr  over i - spaces  dup stars
    dup 2* xor
  loop 2drop ;
 
5 triangle
```



## Fortran

{{works with|Fortran|90 and later}} 
This method calculates a Pascal's triangle and replaces every odd number with a * and every even number with a space. The limitation of this approach is the size of the numbers in the Pascal's triangle. Tryng to print an order 8 Sierpinski's triangle will overflow a 32 bit integer and an order 16 will overflow a 64 bit integer.

```fortran
program Sierpinski_triangle
  implicit none
  
  call Triangle(4)

contains

subroutine Triangle(n)
  implicit none
  integer, parameter :: i64 = selected_int_kind(18)
  integer, intent(in) :: n
  integer :: i, k
  integer(i64) :: c
  
  do i = 0, n*4-1
    c = 1
    write(*, "(a)", advance="no") repeat(" ", 2 * (n*4 - 1 - i))
    do k = 0, i
      if(mod(c, 2) == 0) then
        write(*, "(a)", advance="no") "    "
      else
        write(*, "(a)", advance="no") "  * "
      end if
      c = c * (i - k) / (k + 1)
    end do
    write(*,*)
  end do
end subroutine Triangle
end program Sierpinski_triangle
```



## GAP


```gap
# Using parity of binomial coefficients
SierpinskiTriangle := function(n)
	local i, j, s, b;
	n := 2^n - 1;
	b := " ";
	while Size(b) < n do
		b := Concatenation(b, b);
	od;
	for i in [0 .. n] do
		s := "";
		for j in [0 .. i] do
			if IsEvenInt(Binomial(i, j)) then
				Append(s, "  ");
			else
				Append(s, "* ");
			fi;
		od;
		Print(b{[1 .. n - i]}, s, "\n");
	od;
end;

SierpinskiTriangle(4);
               * 
              * * 
             *   * 
            * * * * 
           *       * 
          * *     * * 
         *   *   *   * 
        * * * * * * * * 
       *               * 
      * *             * * 
     *   *           *   * 
    * * * *         * * * * 
   *       *       *       * 
  * *     * *     * *     * * 
 *   *   *   *   *   *   *   * 
* * * * * * * * * * * * * * * * 
```



## gnuplot

Making and printing a text string, using bit-twiddling to decide whether each character should be a space or a star.


```gnuplot
# Return a string space or star to print at x,y.
# Must have x<y.  x<0 is the left side of the triangle.
# If x<-y then it's before the left edge and the return is a space.
char(x,y) = (y+x>=0 && ((y+x)%2)==0 && ((y+x)&(y-x))==0 ? "*" : " ")

# Return a string which is row y of the triangle from character
# position x through to the right hand end x==y, inclusive.
row(x,y) = (x<=y ? char(x,y).row(x+1,y) : "\n")

# Return a string of stars, spaces and newlines which is the
# Sierpinski triangle from row y to limit, inclusive.
# The first row is y=0.
triangle(y,limit) = (y <= limit ? row(-limit,y).triangle(y+1,limit) : "")

# Print rows 0 to 15, which is the order 4 triangle per the task.
print triangle(0,15)
```



## Go

"Δ" (Greek capital letter delta) looks good for grain, as does Unicode triangle, "△".  ASCII "." and "^" are pleasing. "/\\", "´`", and "◢◣"" make interesting wide triangles.

```go
package main

import (
    "fmt"
    "strings"
    "unicode/utf8"
)

var order = 4
var grain = "*"

func main() {
    t := []string{grain + strings.Repeat(" ", utf8.RuneCountInString(grain))}
    for ; order > 0; order-- {
        sp := strings.Repeat(" ", utf8.RuneCountInString(t[0])/2)
        top := make([]string, len(t))
        for i, s := range t {
            top[i] = sp + s + sp
            t[i] += s
        }
        t = append(top, t...)
    }
    for _, r := range t {
        fmt.Println(r)
    }
}
```



## Groovy

Solution:

```groovy
def stPoints;
stPoints = { order, base=[0,0] ->
    def right = [base[0], base[1]+2**order]
    def up = [base[0]+2**(order-1), base[1]+2**(order-1)]
    (order == 0) \
        ? [base]
        : (stPoints(order-1, base) + stPoints(order-1, right) + stPoints(order-1, up))
}

def stGrid = { order ->
    def h = 2**order
    def w = 2**(order+1) - 1
    def grid = (0..<h).collect { (0..<w).collect { ' ' } }
    stPoints(order).each { grid[it[0]][it[1]] = (order%10).toString() }
    grid
}
```


Test:

```groovy
stGrid(0).reverse().each { println it.sum() }
println()
stGrid(1).reverse().each { println it.sum() }
println()
stGrid(2).reverse().each { println it.sum() }
println()
stGrid(3).reverse().each { println it.sum() }
println()
stGrid(4).reverse().each { println it.sum() }
println()
stGrid(5).reverse().each { println it.sum() }
println()
stGrid(6).reverse().each { println it.sum() }
```


{{out}}
<pre style="height:30ex;overflow:scroll;">
0

 1 
1 1

   2   
  2 2  
 2   2 
2 2 2 2

       3       
      3 3      
     3   3     
    3 3 3 3    
   3       3   
  3 3     3 3  
 3   3   3   3 
3 3 3 3 3 3 3 3

               4               
              4 4              
             4   4             
            4 4 4 4            
           4       4           
          4 4     4 4          
         4   4   4   4         
        4 4 4 4 4 4 4 4        
       4               4       
      4 4             4 4      
     4   4           4   4     
    4 4 4 4         4 4 4 4    
   4       4       4       4   
  4 4     4 4     4 4     4 4  
 4   4   4   4   4   4   4   4 
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4

                               5                               
                              5 5                              
                             5   5                             
                            5 5 5 5                            
                           5       5                           
                          5 5     5 5                          
                         5   5   5   5                         
                        5 5 5 5 5 5 5 5                        
                       5               5                       
                      5 5             5 5                      
                     5   5           5   5                     
                    5 5 5 5         5 5 5 5                    
                   5       5       5       5                   
                  5 5     5 5     5 5     5 5                  
                 5   5   5   5   5   5   5   5                 
                5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5                
               5                               5               
              5 5                             5 5              
             5   5                           5   5             
            5 5 5 5                         5 5 5 5            
           5       5                       5       5           
          5 5     5 5                     5 5     5 5          
         5   5   5   5                   5   5   5   5         
        5 5 5 5 5 5 5 5                 5 5 5 5 5 5 5 5        
       5               5               5               5       
      5 5             5 5             5 5             5 5      
     5   5           5   5           5   5           5   5     
    5 5 5 5         5 5 5 5         5 5 5 5         5 5 5 5    
   5       5       5       5       5       5       5       5   
  5 5     5 5     5 5     5 5     5 5     5 5     5 5     5 5  
 5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5 
5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5

                                                               6                                                               
                                                              6 6                                                              
                                                             6   6                                                             
                                                            6 6 6 6                                                            
                                                           6       6                                                           
                                                          6 6     6 6                                                          
                                                         6   6   6   6                                                         
                                                        6 6 6 6 6 6 6 6                                                        
                                                       6               6                                                       
                                                      6 6             6 6                                                      
                                                     6   6           6   6                                                     
                                                    6 6 6 6         6 6 6 6                                                    
                                                   6       6       6       6                                                   
                                                  6 6     6 6     6 6     6 6                                                  
                                                 6   6   6   6   6   6   6   6                                                 
                                                6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6                                                
                                               6                               6                                               
                                              6 6                             6 6                                              
                                             6   6                           6   6                                             
                                            6 6 6 6                         6 6 6 6                                            
                                           6       6                       6       6                                           
                                          6 6     6 6                     6 6     6 6                                          
                                         6   6   6   6                   6   6   6   6                                         
                                        6 6 6 6 6 6 6 6                 6 6 6 6 6 6 6 6                                        
                                       6               6               6               6                                       
                                      6 6             6 6             6 6             6 6                                      
                                     6   6           6   6           6   6           6   6                                     
                                    6 6 6 6         6 6 6 6         6 6 6 6         6 6 6 6                                    
                                   6       6       6       6       6       6       6       6                                   
                                  6 6     6 6     6 6     6 6     6 6     6 6     6 6     6 6                                  
                                 6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6                                 
                                6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6                                
                               6                                                               6                               
                              6 6                                                             6 6                              
                             6   6                                                           6   6                             
                            6 6 6 6                                                         6 6 6 6                            
                           6       6                                                       6       6                           
                          6 6     6 6                                                     6 6     6 6                          
                         6   6   6   6                                                   6   6   6   6                         
                        6 6 6 6 6 6 6 6                                                 6 6 6 6 6 6 6 6                        
                       6               6                                               6               6                       
                      6 6             6 6                                             6 6             6 6                      
                     6   6           6   6                                           6   6           6   6                     
                    6 6 6 6         6 6 6 6                                         6 6 6 6         6 6 6 6                    
                   6       6       6       6                                       6       6       6       6                   
                  6 6     6 6     6 6     6 6                                     6 6     6 6     6 6     6 6                  
                 6   6   6   6   6   6   6   6                                   6   6   6   6   6   6   6   6                 
                6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6                                 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6                
               6                               6                               6                               6               
              6 6                             6 6                             6 6                             6 6              
             6   6                           6   6                           6   6                           6   6             
            6 6 6 6                         6 6 6 6                         6 6 6 6                         6 6 6 6            
           6       6                       6       6                       6       6                       6       6           
          6 6     6 6                     6 6     6 6                     6 6     6 6                     6 6     6 6          
         6   6   6   6                   6   6   6   6                   6   6   6   6                   6   6   6   6         
        6 6 6 6 6 6 6 6                 6 6 6 6 6 6 6 6                 6 6 6 6 6 6 6 6                 6 6 6 6 6 6 6 6        
       6               6               6               6               6               6               6               6       
      6 6             6 6             6 6             6 6             6 6             6 6             6 6             6 6      
     6   6           6   6           6   6           6   6           6   6           6   6           6   6           6   6     
    6 6 6 6         6 6 6 6         6 6 6 6         6 6 6 6         6 6 6 6         6 6 6 6         6 6 6 6         6 6 6 6    
   6       6       6       6       6       6       6       6       6       6       6       6       6       6       6       6   
  6 6     6 6     6 6     6 6     6 6     6 6     6 6     6 6     6 6     6 6     6 6     6 6     6 6     6 6     6 6     6 6  
 6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6   6 
6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6

```



## Haskell


```haskell
sierpinski 0 = ["*"]
sierpinski n = map ((space ++) . (++ space)) down ++
               map (unwords . replicate 2) down
    where down = sierpinski (n - 1)
          space = replicate (2 ^ (n - 1)) ' '

main = mapM_ putStrLn $ sierpinski 4
```

{{out}}

```txt

               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *
```


We can see how the approach above (centering a preceding block over two duplicates) generates a framing rectangle at each stage, by making the right padding (plus the extra space between duplicates) more distinct and visible:

```haskell
import Data.List (intercalate)

sierpinski :: Int -> [String]
sierpinski 0 = ["▲"]
sierpinski n =
  concat $
  (<$> sierpinski (n - 1)) <$>                  -- Previous triangle,
  [ flip intercalate ([replicate (2 ^ (n - 1))] <*> " -") -- centred,
  , (++) <*> ('+' :)               -- above singly spaced duplicates.
  ]

main :: IO ()
main = mapM_ putStrLn $ sierpinski 4
```

{{Out}}

```txt
               ▲---------------
              ▲+▲--------------
             ▲-+ ▲-------------
            ▲+▲+▲+▲------------
           ▲---+   ▲-----------
          ▲+▲--+  ▲+▲----------
         ▲-+ ▲-+ ▲-+ ▲---------
        ▲+▲+▲+▲+▲+▲+▲+▲--------
       ▲-------+       ▲-------
      ▲+▲------+      ▲+▲------
     ▲-+ ▲-----+     ▲-+ ▲-----
    ▲+▲+▲+▲----+    ▲+▲+▲+▲----
   ▲---+   ▲---+   ▲---+   ▲---
  ▲+▲--+  ▲+▲--+  ▲+▲--+  ▲+▲--
 ▲-+ ▲-+ ▲-+ ▲-+ ▲-+ ▲-+ ▲-+ ▲-
▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲
```



Using bitwise and between x and y coords:

```haskell
import Data.Bits ((.&.))

sierpinski n = map row [m, m-1 .. 0] where
	m = 2^n - 1
	row y = replicate y ' ' ++ concatMap cell [0..m - y] where
		cell x	| y .&. x == 0 = " *"
			| otherwise = "  "

main = mapM_ putStrLn $ sierpinski 4
```


{{Trans|JavaScript}}


```Haskell
import Data.List (intersperse)

-- Top down, each row after the first is an XOR / Rule90 rewrite.
-- Bottom up, each line above the base is indented 1 more space.
sierpinski :: Int -> String
sierpinski = fst . foldr spacing ([], []) . rule90 . (2 ^)
  where
    rule90 = scanl next "*" . enumFromTo 1 . subtract 1
      where
        next = const . ((zipWith xor . (' ' :)) <*> (++ " "))
        xor l r
          | l == r = ' '
          | otherwise = '*'
    spacing x (s, w) = (concat [w, intersperse ' ' x, "\n", s], w ++ " ")

main :: IO ()
main = putStr $ sierpinski 4
```


Or simply as a right fold:


```haskell
sierpinski :: Int -> [String]
sierpinski n =
  foldr
    (\i xs -> 
        let s = replicate (2 ^ i) ' '
        in fmap ((s ++) . (++ s)) xs ++ fmap ((++) <*> (' ' :)) xs)
    ["*"]
    [n - 1,n - 2 .. 0]

main :: IO ()
main = (putStrLn . unlines. sierpinski) 4
```


{{Out}}

```txt
               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *
```



## Haxe


```haxe
class Main 
{
	static function main() 
	{
		triangle(3);
	}
	
	static inline var SPACE = ' ';
	static inline var STAR = '*';
	
	static function triangle(o) {
		var n = 1 << o;
		var line = new Array<String>();
		
		for (i in 0...(n*2)) line[i] = SPACE;
		
		line[n] = '*';
		
		for (i in 0...n) {
			Sys.println(line.join(''));
			var u ='*';
			var start = n - i;
			var end = n + i + 1;
			var t = SPACE;
			for (j in start...end) {
				t = (line[j-1] == line[j+1] ? SPACE : STAR);
				line[j-1] = u;
				u = t;
			}
			
			line[n+i] = t;
			line[n+i+1] = STAR;
		}
	}
}
```


=={{header|Icon}} and {{header|Unicon}}==
This is a text based adaption of a program from the IPL and Icon Graphics book.  The triangle is presented with a twist. Based on an idea from "Chaos and Fractals" by Peitgen, Jurgens, and Saupe.

```Icon
# text based adaptaion of 

procedure main(A)

   width := 2 ^ ( 1 + (order := 0 < integer(\A[1]) | 4))  # order of arg[1] or 4
   write("Triangle order= ",order)

   every !(canvas := list(width)) := list(width," ")      # prime the canvas
   every y := 1 to width & x := 1 to width do             # traverse it
      if iand(x - 1, y - 1) = 0 then canvas[x,y] := "*"   # fill

   every x := 1 to width & y := 1 to width do
      writes((y=1,"\n")|"",canvas[x,y]," ")               # print

end
```


{{libheader|Icon Programming Library}}  
Adapted from [http://www.cs.arizona.edu/icon/library/src/gprogs/sier1.icn graphics/sier1.icn] 

{{out|Sample output for order 3}}

```txt
Triangle order = 2

* * * * * * * *
*   *   *   *
* *     * *
*       *
* * * *
*   *
* *
*
```



## IDL

The only 'special' thing here is that the math is done in a byte array, filled with the numbers 32 and 42 and then output through a "<tt>string(array)</tt>" which prints the ascii representation of each individual element in the array.

```idl
pro sierp,n
  s = (t = bytarr(3+2^(n+1))+32b)
  t[2^n+1] = 42b  
  for lines = 1,2^n do begin
        print,string( (s = t) )
        for i=1,n_elements(t)-2 do if s[i-1] eq s[i+1] then t[i]=32b else t[i]=42b
  end
end
```



## J

There are any number of succinct ways to produce this in J.  
Here's one that exploits self-similarity:

```j
   |. _31]\ ,(,.~ , ])^:4 ,: '* '
```


Here, (,.~ , ])^:4 ,: '* ' is the basic structure (with 4 iterations) and the rest of it is just formatting.

Here's one that leverages the relationship between Sierpinski's and Pascal's triangles:

```j
   ' *' {~ '1' = (- |."_1 [: ": 2 | !/~) i._16
```


Here, !/~ i._16 gives us [[Pascal's_triangle|pascal's triangle]] (and we want a power of 2 (or, for the formatting we are using here a negative of a power of 2) for the size of the square in which contains the triangle, and (2 + |/~) i._16 is a [[Boolean_values#J|boolean]] representation where the 1s correspond to odd values in pascal's triangle, and the rest is just formatting.

(Aside: it's popular to say that booleans are not integers, but this is a false representation of [[Greatest_common_divisor#J|George Boole's work]].)


## Java

{{trans|JavaScript}}

```java
public static void triangle(int n){
        n= 1 << n;
        StringBuilder line= new StringBuilder(); //use a "mutable String"
        char t= 0;
        char u= 0; // avoid warnings
        for(int i= 0;i <= 2 * n;++i)
                line.append(" "); //start empty
        line.setCharAt(n, '*'); //with the top point of the triangle
        for(int i= 0;i < n;++i){
                System.out.println(line);
                u= '*';
                for(int j= n - i;j < n + i + 1;++j){
                        t= (line.charAt(j - 1) == line.charAt(j + 1) ? ' ' : '*');
                        line.setCharAt(j - 1, u);
                        u= t;
                }
                line.setCharAt(n + i, t);
                line.setCharAt(n + i + 1, '*');
        }
}
```


{{trans|Haskell}}
{{works with|Java|1.5+}}

```java
import java.util.*;

public class Sierpinski
{
    public static List<String> sierpinski(int n)
    {
        List<String> down = Arrays.asList("*");
        String space = " ";
        for (int i = 0; i < n; i++) {
            List<String> newDown = new ArrayList<String>();
            for (String x : down)
                newDown.add(space + x + space);
            for (String x : down)
                newDown.add(x + " " + x);

            down = newDown;
            space += space;
        }
        return down;
    }

    public static void main(String[] args)
    {
        for (String x : sierpinski(4))
            System.out.println(x);
    }
}
```



## JavaFX Script

{{trans|Python}}

```javafx
function sierpinski(n : Integer) {
  var down = ["*"];
  var space = " ";
  for (i in [1..n]) {
    down = [for (x in down) "{space}{x}{space}", for (x in down) "{x} {x}"];
    space = "{space}{space}";
  }

  for (x in down) {
    println("{x}")
  }
}

sierpinski(4);
```



## JavaScript



### ES5


### =Functional=


Using a functional idiom of JavaScript, we can construct a Sierpinksi triangle as a Pascal triangle (mod 2), 
mapping the binary pattern to centred strings.


```JavaScript
(function (order) {

    // Sierpinski triangle of order N constructed as
    // Pascal triangle of 2^N rows mod 2
    // with 1 encoded as "▲"
    // and 0 encoded as " "
    function sierpinski(intOrder) {
        return function asciiPascalMod2(intRows) {
            return range(1, intRows - 1)
                .reduce(function (lstRows) {
                    var lstPrevRow = lstRows.slice(-1)[0];

                    // Each new row is a function of the previous row
                    return lstRows.concat([zipWith(function (left, right) {
                        // The composition ( asciiBinary . mod 2 . add )
                        // reduces to a rule from 2 parent characters
                        // to a single child character
                       
                        // Rule 90 also reduces to the same XOR 
                        // relationship between left and right neighbours  

                        return left === right ? " " : "▲";
                    }, [' '].concat(lstPrevRow), lstPrevRow.concat(' '))]);
                }, [
                    ["▲"] // Tip of triangle
                ]);
        }(Math.pow(2, intOrder))

        // As centred lines, from bottom (0 indent) up (indent below + 1)
        .reduceRight(function (sofar, lstLine) {
            return {
                triangle: sofar.indent + lstLine.join(" ") + "\n" +
                    sofar.triangle,
                indent: sofar.indent + " "
            };
        }, {
            triangle: "",
            indent: ""
        }).triangle;
    };

    var zipWith = function (f, xs, ys) {
            return xs.length === ys.length ? xs
                .map(function (x, i) {
                    return f(x, ys[i]);
                }) : undefined;
        },
        range = function (m, n) {
            return Array.apply(null, Array(n - m + 1))
                .map(function (x, i) {
                    return m + i;
                });
        };

    // TEST
    return sierpinski(order);

})(4);

```


Output (N=4)


```txt
               ▲
              ▲ ▲
             ▲   ▲
            ▲ ▲ ▲ ▲
           ▲       ▲
          ▲ ▲     ▲ ▲
         ▲   ▲   ▲   ▲
        ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲
       ▲               ▲
      ▲ ▲             ▲ ▲
     ▲   ▲           ▲   ▲
    ▲ ▲ ▲ ▲         ▲ ▲ ▲ ▲
   ▲       ▲       ▲       ▲
  ▲ ▲     ▲ ▲     ▲ ▲     ▲ ▲
 ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲
▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲
```



### =Imperative=



```javascript
function triangle(o) {
    var n = 1 << o,
        line = new Array(2 * n),
        i, j, t, u;
    for (i = 0; i < line.length; ++i) line[i] = ' ';
    line[n] = '*';
    for (i = 0; i < n; ++i) {
        document.write(line.join('') + "\n");
        u = '*';
        for (j = n - i; j < n + i + 1; ++j) {
            t = (line[j - 1] == line[j + 1] ? ' ' : '*');
            line[j - 1] = u;
            u = t;
        }
        line[n + i] = t;
        line[n + i + 1] = '*';
    }
}
document.write("
```txt
\n");
triangle(6);
document.write("
```
");
```



### ES6

Directly in terms of the built-in Array methods '''.map''', '''.reduce''', and '''.from''', and without much abstraction, possibly at the cost of some legibility:

```javascript
(() => {
    'use strict';

    // sierpinski :: Int -> String
    const sierpinski = n =>
        Array.from({
            length: n
        })
        .reduce(
            (xs, _, i) => {
                const s = ' '.repeat(Math.pow(2, i));
                return xs.map(x => s + x + s)
                    .concat(
                        xs.map(x => x + ' ' + x)
                    )
            },
            ['*']
        ).join('\n');

    // TEST -------------------------------------------
    console.log(
        sierpinski(4)
    );
})();
```


{{Trans|Haskell}}
Centering any preceding triangle block over two adjacent duplicates: 

```JavaScript
(() => {
    'use strict';

    // LINES OF SIERPINSKI TRIANGLE AT LEVEL N -------------------------------

    // sierpinski :: Int -> [String]
    const sierpTriangle = n =>
        // Previous triangle centered with left and right padding,
        (n > 0) ? concat(ap([
            map(xs => intercalate(xs, ap(
                [s => concat(replicate(Math.pow(2, (n - 1)), s))], [' ', '-']
            ))),
            
            // above a pair of duplicates, placed one character apart.
            map(xs => intercalate('+', [xs, xs]))
        ], [sierpTriangle(n - 1)])) : ['▲'];


    // GENERIC FUNCTIONS -----------------------------------------------------

    // replicate :: Int -> a -> [a]
    const replicate = (n, a) => {
        let v = [a],
            o = [];
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o.concat(v);
            n >>= 1;
            v = v.concat(v);
        }
        return o.concat(v);
    };

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // map :: (a -> b) -> [a] -> [b]
    const map = curry((f, xs) => xs.map(f));

    // Apply a list of functions to a list of arguments
    // <*> :: [(a -> b)] -> [a] -> [b]
    const ap = (fs, xs) => //
        [].concat.apply([], fs.map(f => //
            [].concat.apply([], xs.map(x => [f(x)]))));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // intercalate :: String -> [a] -> String
    const intercalate = (s, xs) => xs.join(s);

    // concat :: [[a]] -> [a] || [String] -> String
    const concat = xs => {
        if (xs.length > 0) {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        } else return [];
    };

    // TEST ------------------------------------------------------------------
    return unlines(sierpTriangle(4));
})();
```

{{Out}}

```txt
               ▲---------------
              ▲+▲--------------
             ▲-+ ▲-------------
            ▲+▲+▲+▲------------
           ▲---+   ▲-----------
          ▲+▲--+  ▲+▲----------
         ▲-+ ▲-+ ▲-+ ▲---------
        ▲+▲+▲+▲+▲+▲+▲+▲--------
       ▲-------+       ▲-------
      ▲+▲------+      ▲+▲------
     ▲-+ ▲-----+     ▲-+ ▲-----
    ▲+▲+▲+▲----+    ▲+▲+▲+▲----
   ▲---+   ▲---+   ▲---+   ▲---
  ▲+▲--+  ▲+▲--+  ▲+▲--+  ▲+▲--
 ▲-+ ▲-+ ▲-+ ▲-+ ▲-+ ▲-+ ▲-+ ▲-
▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲+▲
```


Or constructed as 2^N lines of Pascal's triangle mod 2, 
and mapped to centred {1:asterisk, 0:space} strings.

```JavaScript

(order => {
    // sierpinski :: Int -> [Bool]
    let sierpinski = intOrder => {

        // asciiPascalMod2 :: Int -> [[Int]]
        let asciiPascalMod2 = nRows =>
            range(1, nRows - 1)
            .reduce(sofar => {
                let lstPrev = sofar.slice(-1)[0];

                // The composition of (asciiBinary . mod 2 . add)
                // is reduced here to a rule from two parent characters
                // to a single child character.

                // Rule 90 also reduces to the same XOR 
                // relationship between left and right neighbours.

                return sofar
                    .concat([zipWith(
                        (left, right) => left === right ? ' ' : '*',
                        [' '].concat(lstPrev),
                        lstPrev.concat(' ')
                    )]);
            }, [
                ['*'] // Tip of triangle
            ]);

        // Reduce/folding from the last item (base of list)
        // which has zero left indent.

        // Each preceding row has one more indent space than the row beneath it
        return asciiPascalMod2(Math.pow(2, intOrder))
            .reduceRight((a, x) => {
                return {
                    triangle: a.indent + x.join(' ') + '\n' + a.triangle,
                    indent: a.indent + ' '
                }
            }, {
                triangle: '',
                indent: ''
            }).triangle
    };

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    let zipWith = (f, xs, ys) =>
            xs.length === ys.length ? (
                xs.map((x, i) => f(x, ys[i]))
            ) : undefined,

        // range(intFrom, intTo, optional intStep)
        // Int -> Int -> Maybe Int -> [Int]
        range = (m, n, step) => {
            let d = (step || 1) * (n >= m ? 1 : -1);

            return Array.from({
                length: Math.floor((n - m) / d) + 1
            }, (_, i) => m + (i * d));
        };

    return sierpinski(order);

})(4);
```


{{Out}}

```txt
               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *
```



## Julia

{{works with|Julia|0.6}}


```julia
function sierpinski(n, token::AbstractString="*")
	x = fill(token, 1, 1)
	for _ in 1:n
		h, w = size(x)
		s = fill(" ", h,(w + 1) ÷ 2)
		t = fill(" ", h,1)
		x = [[s x s] ; [x t x]]
	end
	return x
end

function printsierpinski(m::Matrix)
    for r in 1:size(m, 1)
        println(join(m[r, :]))
    end
end

sierpinski(4) |> printsierpinski
```



## Kotlin

{{trans|C}}

```scala
// version 1.1.2

const val ORDER = 4
const val SIZE  = 1 shl ORDER

fun main(args: Array<String>) {
    for (y in SIZE - 1 downTo 0) {
        for (i in 0 until y) print(" ")
        for (x in 0 until SIZE - y) print(if ((x and y) != 0) "  " else "* ")
        println()
    }
}
```


{{out}}

```txt

               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *

```



## Liberty BASIC


```lb
nOrder=4
call triangle 1, 1, nOrder
end

SUB triangle x, y, n
    IF n = 0 THEN
        LOCATE x,y: PRINT "*";
    ELSE
        n=n-1
        length=2^n
        call triangle x, y+length, n
        call triangle x+length, y, n
        call triangle x+length*2, y+length, n
    END IF
END SUB
```



## Logo


```logo
; Print rows of the triangle from 0 to :limit inclusive.
; limit=15 gives the order 4 form per the task.
; The range of :y is arbitrary, any rows of the triangle can be printed.

make "limit 15
for [y 0 :limit] [
  for [x -:limit :y] [
    type ifelse (and :y+:x >= 0                ; blank left of triangle
                     (remainder :y+:x 2) = 0   ; only "even" squares
                     (bitand :y+:x :y-:x) = 0  ; Sierpinski bit test
                ) ["*] ["| |]                  ; star or space
  ]
  print []
]
```



## Lua

Ported from the list-comprehension Python version.


```lua
function sierpinski(depth)
   lines = {}
   lines[1] = '*'

   for i = 2, depth+1 do
      sp = string.rep(' ', 2^(i-2))
      tmp = {}
      for idx, line in ipairs(lines) do
         tmp[idx] = sp .. line .. sp
         tmp[idx+#lines] = line .. ' ' .. line
      end
      lines = tmp
   end
   return table.concat(lines, '\n')
end

print(sierpinski(4))
```

{{out}}

```txt

               *               
              * *              
             *   *             
            * * * *            
           *       *           
          * *     * *          
         *   *   *   *         
        * * * * * * * *        
       *               *       
      * *             * *      
     *   *           *   *     
    * * * *         * * * *    
   *       *       *       *   
  * *     * *     * *     * *  
 *   *   *   *   *   *   *   * 
* * * * * * * * * * * * * * * *

```




## Maple


```maple
S := proc(n)
    local i, j, values, position;
    values := [ seq(" ",i=1..2^n-1), "*" ];
    printf("%s\n",cat(op(values)));
    for i from 2 to 2^n do
        position := [ ListTools:-SearchAll( "*", values ) ];
        values := Array([ seq(0, i=1..2^n+i-1) ]);
        for j to numelems(position) do
            values[position[j]-1] := values[position[j]-1] + 1;
            values[position[j]+1] := values[position[j]+1] + 1;
        end do;
        values := subs( { 2 = " ", 0 = " ", 1 = "*"}, values );  
        printf("%s\n",cat(op(convert(values, list))));
    end do:
end proc:
```

{{out}}

```txt

               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *

```



## Mathematica

Cellular automaton (rule 90) based solution:

```mathematica
n=4;Grid[CellularAutomaton[90,{{1},0},2^n-1]/.{0->" ",1->"*"},ItemSize->All]
```

Using built-in function:

```mathematica
SierpinskiMesh[3]
```



## MATLAB

STRING was introduced in version R2016b.

```MATLAB
n = 4;
d = string('*');
for k = 0 : n - 1
  sp = repelem(' ', 2 ^ k);
  d = [sp + d + sp, d + ' ' + d];
end
disp(d.join(char(10)))

```

{{out}}

```txt

               *               
              * *              
             *   *             
            * * * *            
           *       *           
          * *     * *          
         *   *   *   *         
        * * * * * * * *        
       *               *       
      * *             * *      
     *   *           *   *     
    * * * *         * * * *    
   *       *       *       *   
  * *     * *     * *     * *  
 *   *   *   *   *   *   *   * 
* * * * * * * * * * * * * * * *

```


### Cellular Automaton Version


```MATLAB
n = 2 ^ 4 - 1;
tr = + ~(-n : n);
for k = 1:n
  tr(k + 1, :) = bitget(90, 1 + filter2([4 2 1], tr(k, :)));
end
char(10 * tr + 32)
```



### Mixed Version


```matlab
spy(mod(abs(pascal(32,1)),2)==1)
```



## NetRexx

{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numeric digits 1000
runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  BLACK_UPPOINTING_TRIANGLE = '\u25b2'
  parse arg ordr filr .
  if ordr = '' | ordr = '.' then ordr = 4
  if filr = '' | filr = '.' then filler = BLACK_UPPOINTING_TRIANGLE
  else                           filler = filr
  drawSierpinskiTriangle(ordr, filler)
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method drawSierpinskiTriangle(ordr, filler = Rexx '^') public static
  n = 1 * (2 ** ordr)
  line = ' '.copies(2 * n)
  line = line.overlay(filler, n + 1) -- set the top point of the triangle
  loop row = 1 to n -- NetRexx arrays, lists etc. index from 1
    say line.strip('t')
    u = filler
    loop col = 2 + n - row to n + row
      cl = line.substr(col - 1, 1)
      cr = line.substr(col + 1, 1)
      if cl == cr then t = ' '
      else             t = filler
      line = line.overlay(u, col - 1)
      u = t
      end col
      j2 = n + row - 1
      j3 = n + row
      line = line.overlay(t, j2 + 1)
      line = line.overlay(filler, j3 + 1)
    end row
  return

```

{{out}}

```txt

                ▲
               ▲ ▲
              ▲   ▲
             ▲ ▲ ▲ ▲
            ▲       ▲
           ▲ ▲     ▲ ▲
          ▲   ▲   ▲   ▲
         ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲
        ▲               ▲
       ▲ ▲             ▲ ▲
      ▲   ▲           ▲   ▲
     ▲ ▲ ▲ ▲         ▲ ▲ ▲ ▲
    ▲       ▲       ▲       ▲
   ▲ ▲     ▲ ▲     ▲ ▲     ▲ ▲
  ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲
 ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲

```



## Nim

{{trans|C}}

```nim
const size = 1 shl 4 - 1

for y in countdown(size, 0):
  for i in 0 .. <y:
    stdout.write " "
  for x in 0 .. size-y:
    if (x and y) != 0:
      stdout.write "  "
    else:
      stdout.write "* "
  stdout.write "\n"
```



## OCaml


```ocaml
let sierpinski n =
  let rec loop down space n =
    if n = 0 then
      down
    else
      loop (List.map (fun x -> space ^ x ^ space) down @
              List.map (fun x -> x ^ " " ^ x) down)
        (space ^ space)
        (n - 1)
  in loop ["*"] " " n

let () =
  List.iter print_endline (sierpinski 4)
```



## Oforth


This solution uses a cellular automaton (rule 90 for triangle). 

automat(rule, n) runs cellular automaton for rule "rule" for n generations.


```Oforth
: nextGen(l, r)
| i |
   StringBuffer new
   l size loop: i [
      l at(i 1 -) '*' == 4 *
      l at(i)     '*' == 2 * +
      l at(i 1 +) '*' == +
      2 swap pow r bitAnd ifTrue: [ '*' ] else: [ ' ' ] over addChar
      ] ;

: automat(rule, n)
   StringBuffer new " " <<n(n) "*" over + +
   #[ dup println rule nextGen ] times(n) drop ;

: sierpinskiTriangle(n) 
   90 4 n * automat ;
```


{{out}}

```txt

>4 sierpinskiTriangle
                *
               * *
              *   *
             * * * *
            *       *
           * *     * *
          *   *   *   *
         * * * * * * * *
        *               *
       * *             * *
      *   *           *   *
     * * * *         * * * *
    *       *       *       *
   * *     * *     * *     * *
  *   *   *   *   *   *   *   *
 * * * * * * * * * * * * * * * *
ok
>

```



## Oz


```oz
declare
  fun {NextTriangle Triangle}
     Sp = {Spaces {Length Triangle}}
  in
   {Flatten
      [{Map Triangle fun {$ X} Sp#X#Sp end}
       {Map Triangle fun {$ X} X#" "#X end}
      ]}
  end

  fun {Spaces N} if N == 0 then nil else & |{Spaces N-1} end end
  
  fun lazy {Iterate F X}
     X|{Iterate F {F X}}
  end

  SierpinskiTriangles = {Iterate NextTriangle ["*"]}
in
  {ForAll {Nth SierpinskiTriangles 5} System.showInfo}
```



## PARI/GP

{{trans|C}}

```parigp
Sierpinski(n)={
  my(s=2^n-1);
  forstep(y=s,0,-1,
    for(i=1,y,print1(" "));
    for(x=0,s-y,
      print1(if(bitand(x,y)," ","*"))
    );
    print()
  )
};
Sierpinski(4)
```

{{out}}

```txt
               *
              **
             * *
            ****
           *   *
          **  **
         * * * *
        ********
       *       *
      **      **
     * *     * *
    ****    ****
   *   *   *   *
  **  **  **  **
 * * * * * * * *
****************
```



## Pascal

{{trans|C}}
{{works with|Free Pascal}}

```pascal
program Sierpinski;

function ipow(b, n	: Integer) : Integer;
var
   i : Integer;
begin 
   ipow := 1;
   for i := 1 to n do
      ipow := ipow * b
end;

function truth(a : Char) : Boolean;
begin
   if a = '*' then
      truth := true
   else
      truth := false
end;
```



```pascal
function rule_90(ev :  String) : String;
var
   l, i	: Integer;
   cp	: String;
   s	: Array[0..1] of Boolean;
begin
   l := length(ev);
   cp := copy(ev, 1, l);
   for i := 1 to l do begin
      if (i-1) < 1 then
	 s[0] := false
      else
	 s[0] := truth(ev[i-1]);
      if (i+1) > l then
	 s[1] := false
      else
	 s[1] := truth(ev[i+1]);
      if ( (s[0] and not s[1]) or (s[1] and not s[0]) ) then
	 cp[i] := '*'
      else
	 cp[i] := ' ';
   end;
   rule_90 := cp
end;

procedure triangle(n : Integer);
var
   i, l	: Integer;
   b	: String;
begin 
   l := ipow(2, n+1);
   b := ' ';
   for i := 1 to l do
      b := concat(b, ' ');
   b[round(l/2)] := '*';
   writeln(b);
   for i := 1 to (round(l/2)-1) do begin
      b := rule_90(b);
      writeln(b)
   end
end;
```



```pascal
begin
   triangle(4)
end.
```



## Perl


```perl
sub sierpinski {
    my ($n) = @_;
    my @down = '*';
    my $space = ' ';
    foreach (1..$n) {
        @down = (map("$space$_$space", @down), map("$_ $_", @down));
        $space = "$space$space";
    }
    return @down;
}

print "$_\n" foreach sierpinski 4;
```



## Perl 6

{{trans|Perl}}

```perl6
sub sierpinski ($n) {
    my @down  = '*';
    my $space = ' ';
    for ^$n {
        @down = |("$space$_$space" for @down), |("$_ $_" for @down);
        $space x= 2;
    }
    return @down;
}
 
.say for sierpinski 4;
```



## Phix

{{Trans|C}}

```Phix
procedure sierpinski(integer n)
integer lim = power(2,n)-1
    for y=lim to 0 by -1 do
        puts(1,repeat(' ',y))
        for x=0 to lim-y do
            puts(1,iff(and_bits(x,y)?"  ":"* "))
        end for
        puts(1,"\n")
    end for
end procedure

for i=1 to 5 do
    sierpinski(i)
end for
```

{{out}}
<pre style="font-size: 2px">
 *
* *
   *
  * *
 *   *
* * * *
       *
      * *
     *   *
    * * * *
   *       *
  * *     * *
 *   *   *   *
* * * * * * * *
               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *
                               *
                              * *
                             *   *
                            * * * *
                           *       *
                          * *     * *
                         *   *   *   *
                        * * * * * * * *
                       *               *
                      * *             * *
                     *   *           *   *
                    * * * *         * * * *
                   *       *       *       *
                  * *     * *     * *     * *
                 *   *   *   *   *   *   *   *
                * * * * * * * * * * * * * * * *
               *                               *
              * *                             * *
             *   *                           *   *
            * * * *                         * * * *
           *       *                       *       *
          * *     * *                     * *     * *
         *   *   *   *                   *   *   *   *
        * * * * * * * *                 * * * * * * * *
       *               *               *               *
      * *             * *             * *             * *
     *   *           *   *           *   *           *   *
    * * * *         * * * *         * * * *         * * * *
   *       *       *       *       *       *       *       *
  * *     * *     * *     * *     * *     * *     * *     * *
 *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

```



## PicoLisp

{{trans|Python}}

```PicoLisp
(de sierpinski (N)
   (let (D '("*")  S " ")
      (do N
         (setq
            D (conc
               (mapcar '((X) (pack S X S)) D)
               (mapcar '((X) (pack X " " X)) D) )
            S (pack S S) ) )
      D ) )

(mapc prinl (sierpinski 4))
```



## PL/I


```PL/I
sierpinski: procedure options (main); /* 2010-03-30 */
   declare t (79,79) char (1);
   declare (i, j, k) fixed binary;
   declare (y, xs, ys, xll, xrr, ixrr, limit) fixed binary;

   t = ' ';
   xs = 40; ys = 1;
   /* Make initial triangle */
   call make_triangle (xs, ys);
   y = ys + 4;
   xll = xs-4; xrr = xs+4;
   do k = 1 to 3;
      limit = 0;
      do forever;
         ixrr = xrr;
         do i = xll to xll+limit by 8;
            if t(y-1, i) = ' ' then
               do;
                  call make_triangle (i, y);
                  if t(y+3,i-5) = '*' then
                     t(y+3,i-4), t(y+3,ixrr+4) = '*';
                  call make_triangle (ixrr, y);
               end;
            ixrr = ixrr - 8;
         end;
         xll = xll - 4; xrr = xrr + 4;
         y = y + 4;
         limit = limit + 8;
         if xll+limit > xs-1 then leave;
      end;
      t(y-1,xs) = '*';
   end;

   /* Finished generation; now print the Sierpinski triangle. */
   put edit (t) (skip, (hbound(t,2)) a);

make_triangle: procedure (x, y);
   declare (x, y) fixed binary;
   declare i fixed binary;

   do i = 0 to 3;
      t(y+i, x-i), t(y+i, x+i) = '*';
   end;
   do i = x-2 to x+2;  /* The base of the triangle. */
      t(y+3, i) = '*';
   end;
end make_triangle;

end sierpinski;
```



## Pop11

Solution using line buffer in an integer array oline, 0 represents ' '
(space), 1 represents '*' (star).

```pop11
define triangle(n);
    lvars k = 2**n, j, l, oline, nline;
    initv(2*k+3) -> oline;
    initv(2*k+3) -> nline;
    for l from 1 to 2*k+3 do 0 -> oline(l) ; endfor;
    1 -> oline(k+2);
    0 -> nline(1);
    0 -> nline(2*k+3);
    for j from 1 to k do
        for l from 1 to 2*k+3 do
            printf(if oline(l) = 0 then ' ' else '*' endif);
        endfor;
        printf('\n');
        for l from 2 to 2*k+2 do
            (oline(l-1) + oline(l+1)) rem 2 -> nline(l);
        endfor;
        (oline, nline) -> (nline, oline);
    endfor;
enddefine;

triangle(4);
```


Alternative solution, keeping all triangle as list of strings

```pop11
define triangle2(n);
    lvars acc = ['*'], spaces = ' ', j;
    for j from 1 to n do
        maplist(acc, procedure(x); spaces >< x >< spaces ; endprocedure)
         <> maplist(acc, procedure(x); x >< ' ' >< x ; endprocedure) -> acc;
        spaces >< spaces -> spaces;
    endfor;
    applist(acc, procedure(x); printf(x, '%p\n'); endprocedure);
enddefine;

triangle2(4);
```



## PostScript

This draws the triangles in a string-rewrite fashion, where all edges form a single polyline. 9 page document showing progession.

```postscript
%!PS-Adobe-3.0
%%BoundingBox 0 0 300 300

/F { 1 0 rlineto } def
/+ { 120 rotate } def
/- {-120 rotate } def
/v {.5 .5 scale } def
/^ { 2  2 scale } def
/!0{ dup 1 sub dup -1 eq not } def

/X { !0 { v X + F - X - F + X ^ } { F } ifelse pop } def

0 1 8 { 300 300 scale 0 1 12 div moveto
        X + F + F fill showpage         } for
%%EOF
```



## PowerShell

{{Trans|JavaScript}}

```powershell
function triangle($o) {
    $n = [Math]::Pow(2, $o)
    $line = ,' '*(2*$n+1)
    $line[$n] = '█'
    $OFS = ''
    for ($i = 0; $i -lt $n; $i++) {
        Write-Host $line
        $u = '█'
        for ($j = $n - $i; $j -lt $n + $i + 1; $j++) {
            if ($line[$j-1] -eq $line[$j+1]) {
                $t = ' '
            } else {
                $t = '█'
            }
            $line[$j-1] = $u
            $u = $t
        }
        $line[$n+$i] = $t
        $line[$n+$i+1] = '█'
    }
}
```



## Prolog

Works with SWI-Prolog;

```Prolog
sierpinski_triangle(N) :-
	Len is 2 ** (N+1) - 1,
	length(L, Len),
	numlist(1, Len, LN),
	maplist(init(N), L, LN),
	atomic_list_concat(L, Line),
	writeln(Line),
	NbTours is 2**N - 1,
	loop(NbTours, LN, Len, L).

init(N, Cell, Num) :-
	(   Num is 2 ** N + 1  -> Cell = *; Cell = ' ').

loop(0, _, _, _) :- !.

loop(N, LN, Len, L) :-
	maplist(compute_next_line(Len, L), LN, L1),
	atomic_list_concat(L1, Line),
	writeln(Line),
	N1 is N - 1,
	loop(N1, LN, Len, L1).



compute_next_line(Len, L, I, V) :-
	I1 is I - 1,
	I2 is I+1,
	(   I = 1 ->  V0 = ' '; nth1(I1, L, V0)),
	nth1(I, L, V1),
	(   I = Len -> V2 = ' '; nth1(I2, L, V2)),
	rule_90(V0, V1, V2, V).

rule_90('*','*','*', ' ').
rule_90('*','*',' ', '*').
rule_90('*',' ','*', ' ').
rule_90('*',' ',' ', '*').
rule_90(' ','*','*', '*').
rule_90(' ','*',' ', ' ').
rule_90(' ',' ','*', '*').
rule_90(' ',' ',' ', ' ').

```

{{out}}

```txt
 ?- sierpinski_triangle(4).
                *                
               * *               
              *   *              
             * * * *             
            *       *            
           * *     * *           
          *   *   *   *          
         * * * * * * * *         
        *               *        
       * *             * *       
      *   *           *   *      
     * * * *         * * * *     
    *       *       *       *    
   * *     * *     * *     * *   
  *   *   *   *   *   *   *   *  
 * * * * * * * * * * * * * * * * 
true 
```



## PureBasic


```PureBasic
Procedure Triangle (X,Y, Length, N)
   If N = 0 
      DrawText( Y,X, "*",#Blue)
   Else
      Triangle (X+Length,          Y, Length/2, N-1)
      Triangle (X,   Y+Length,        Length/2, N-1)
      Triangle (X+Length, Y+Length*2, Length/2, N-1)
   EndIf
EndProcedure 
      

OpenWindow(0, 100, 100,700,500 ,"Sierpinski triangle",  #PB_Window_SystemMenu |1) 
StartDrawing(WindowOutput(0))
   DrawingMode(#PB_2DDrawing_Transparent )
   Triangle(10,10,120,5)
StopDrawing()

Repeat
Until WaitWindowEvent()=#PB_Event_CloseWindow
End
```



## Python


```python
def sierpinski(n):
    d = ["*"]
    for i in xrange(n):
        sp = " " * (2 ** i)
        d = [sp+x+sp for x in d] + [x+" "+x for x in d]
    return d

print "\n".join(sierpinski(4))
```



Or, using fold / reduce {{works with|Python|3.x}}

```python
import functools

def sierpinski(n):

    def aggregate(TRIANGLE, I):
        SPACE = " " * (2 ** I)
        return [SPACE+X+SPACE for X in TRIANGLE] + [X+" "+X for X in TRIANGLE]

    return functools.reduce(aggregate, range(n), ["*"])

print("\n".join(sierpinski(4)))
```


and fold/reduce, wrapped as concatMap, can provide the list comprehensions too:

```python
from functools import (reduce)
from operator import (add)


# sierpinski :: Int -> String
def sierpinski(n):
    def go(xs, i):
        s = ' ' * (2 ** i)
        return concatMap(lambda x: [s + x + s])(xs) + (
            concatMap(lambda x: [x + ' ' + x])(xs)
        )
    return '\n'.join(reduce(go, range(n), '*'))


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    return lambda xs: (
        reduce(add, map(f, xs), [])
    )


print(sierpinski(4))
```



Use Python's long integer and bit operator to make an infinite triangle:

```python
x = 1
while True:
	print(bin(x)[2:].replace('0', ' '))
	x ^= x<<1
```



## R

Based on C# but using some of R's functionality to abbreviate code where possible.

```r
sierpinski.triangle = function(n) {
	len <- 2^(n+1)
	b <- c(rep(FALSE,len/2),TRUE,rep(FALSE,len/2))
	for (i in 1:(len/2))
	{
		cat(paste(ifelse(b,"*"," "),collapse=""),"\n")
		n <- rep(FALSE,len+1)
		n[which(b)-1]<-TRUE
		n[which(b)+1]<-xor(n[which(b)+1],TRUE)
		b <- n
	}
}
sierpinski.triangle(5)
```


Shortened to a function of one line.

```r
sierpinski.triangle = function(n) {
	c(paste(ifelse(b<<- c(rep(FALSE,2^(n+1)/2),TRUE,rep(FALSE,2^(n+1)/2)),"*"," "),collapse=""),replicate(2^n-1,paste(ifelse(b<<-xor(c(FALSE,b[1:2^(n+1)]),c(b[2:(2^(n+1)+1)],FALSE)),"*"," "),collapse="")))
}
cat(sierpinski.triangle(5),sep="\n")
```



## Racket



```Racket

#lang racket
(define (sierpinski n)
  (if (zero? n)
    '("*")
    (let ([spaces (make-string (expt 2 (sub1 n)) #\space)]
          [prev   (sierpinski (sub1 n))])
      (append (map (λ(x) (~a spaces x spaces)) prev)
              (map (λ(x) (~a x " " x)) prev)))))
(for-each displayln (sierpinski 5))

```



## REXX


```rexx
/*REXX program constructs and displays a  Sierpinski triangle of up to around order 10k.*/
parse arg n mark .                               /*get the order of Sierpinski triangle.*/
if n==''   | n==","  then n=4                    /*Not specified?  Then use the default.*/
if mark==''          then mark=  "*"             /*MARK  was specified as  a character. */
if length(mark)==2   then mark=x2c(mark)         /*  "    "      "     in  hexadecimal. */
if length(mark)==3   then mark=d2c(mark)         /*  "    "      "      "      decimal. */
numeric digits 12000                             /*this should handle the biggy numbers.*/
                                                 /* [↓]  the blood-'n-guts of the pgm.  */
   do j=0  for n*4;  !=1;  z=left('', n*4 -1-j)  /*indent the line to be displayed.     */
         do k=0  for j+1                         /*construct the line with  J+1  parts. */
         if !//2==0  then z=z'  '                /*it's either a    blank,   or    ···  */
                     else z=z mark               /* ··· it's one of 'em thar characters.*/
         !=! * (j-k) % (k+1)                     /*calculate handy-dandy thing-a-ma-jig.*/
         end   /*k*/                             /* [↑]  finished constructing a line.  */
   say z                                         /*display a line of the triangle.      */
   end         /*j*/                             /* [↑]  finished showing triangle.     */
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default input of order:   <tt> 4 </tt>

(Shown at three quarter size.)
<b>
<pre style="font-size:75%">
                *
               * *
              *   *
             * * * *
            *       *
           * *     * *
          *   *   *   *
         * * * * * * * *
        *               *
       * *             * *
      *   *           *   *
     * * * *         * * * *
    *       *       *       *
   * *     * *     * *     * *
  *   *   *   *   *   *   *   *
 * * * * * * * * * * * * * * * *

```

</b>
'''output'''   when using the input of:   <tt> 8   1e </tt>

(Shown at half size.)
<b>
<pre style="font-size:50%">
                                ▲
                               ▲ ▲
                              ▲   ▲
                             ▲ ▲ ▲ ▲
                            ▲       ▲
                           ▲ ▲     ▲ ▲
                          ▲   ▲   ▲   ▲
                         ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲
                        ▲               ▲
                       ▲ ▲             ▲ ▲
                      ▲   ▲           ▲   ▲
                     ▲ ▲ ▲ ▲         ▲ ▲ ▲ ▲
                    ▲       ▲       ▲       ▲
                   ▲ ▲     ▲ ▲     ▲ ▲     ▲ ▲
                  ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲
                 ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲
                ▲                               ▲
               ▲ ▲                             ▲ ▲
              ▲   ▲                           ▲   ▲
             ▲ ▲ ▲ ▲                         ▲ ▲ ▲ ▲
            ▲       ▲                       ▲       ▲
           ▲ ▲     ▲ ▲                     ▲ ▲     ▲ ▲
          ▲   ▲   ▲   ▲                   ▲   ▲   ▲   ▲
         ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲                 ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲
        ▲               ▲               ▲               ▲
       ▲ ▲             ▲ ▲             ▲ ▲             ▲ ▲
      ▲   ▲           ▲   ▲           ▲   ▲           ▲   ▲
     ▲ ▲ ▲ ▲         ▲ ▲ ▲ ▲         ▲ ▲ ▲ ▲         ▲ ▲ ▲ ▲
    ▲       ▲       ▲       ▲       ▲       ▲       ▲       ▲
   ▲ ▲     ▲ ▲     ▲ ▲     ▲ ▲     ▲ ▲     ▲ ▲     ▲ ▲     ▲ ▲
  ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲   ▲
 ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲ ▲

```

</b>
'''output'''   when using the input of:   <tt> 32   db </tt>

(Shown at one tenth size.)
<b>
<pre style="font-size:10%">
                                                                                                                                █
                                                                                                                               █ █
                                                                                                                              █   █
                                                                                                                             █ █ █ █
                                                                                                                            █       █
                                                                                                                           █ █     █ █
                                                                                                                          █   █   █   █
                                                                                                                         █ █ █ █ █ █ █ █
                                                                                                                        █               █
                                                                                                                       █ █             █ █
                                                                                                                      █   █           █   █
                                                                                                                     █ █ █ █         █ █ █ █
                                                                                                                    █       █       █       █
                                                                                                                   █ █     █ █     █ █     █ █
                                                                                                                  █   █   █   █   █   █   █   █
                                                                                                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
                                                                                                                █                               █
                                                                                                               █ █                             █ █
                                                                                                              █   █                           █   █
                                                                                                             █ █ █ █                         █ █ █ █
                                                                                                            █       █                       █       █
                                                                                                           █ █     █ █                     █ █     █ █
                                                                                                          █   █   █   █                   █   █   █   █
                                                                                                         █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █
                                                                                                        █               █               █               █
                                                                                                       █ █             █ █             █ █             █ █
                                                                                                      █   █           █   █           █   █           █   █
                                                                                                     █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █
                                                                                                    █       █       █       █       █       █       █       █
                                                                                                   █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █
                                                                                                  █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █
                                                                                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
                                                                                                █                                                               █
                                                                                               █ █                                                             █ █
                                                                                              █   █                                                           █   █
                                                                                             █ █ █ █                                                         █ █ █ █
                                                                                            █       █                                                       █       █
                                                                                           █ █     █ █                                                     █ █     █ █
                                                                                          █   █   █   █                                                   █   █   █   █
                                                                                         █ █ █ █ █ █ █ █                                                 █ █ █ █ █ █ █ █
                                                                                        █               █                                               █               █
                                                                                       █ █             █ █                                             █ █             █ █
                                                                                      █   █           █   █                                           █   █           █   █
                                                                                     █ █ █ █         █ █ █ █                                         █ █ █ █         █ █ █ █
                                                                                    █       █       █       █                                       █       █       █       █
                                                                                   █ █     █ █     █ █     █ █                                     █ █     █ █     █ █     █ █
                                                                                  █   █   █   █   █   █   █   █                                   █   █   █   █   █   █   █   █
                                                                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
                                                                                █                               █                               █                               █
                                                                               █ █                             █ █                             █ █                             █ █
                                                                              █   █                           █   █                           █   █                           █   █
                                                                             █ █ █ █                         █ █ █ █                         █ █ █ █                         █ █ █ █
                                                                            █       █                       █       █                       █       █                       █       █
                                                                           █ █     █ █                     █ █     █ █                     █ █     █ █                     █ █     █ █
                                                                          █   █   █   █                   █   █   █   █                   █   █   █   █                   █   █   █   █
                                                                         █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █
                                                                        █               █               █               █               █               █               █               █
                                                                       █ █             █ █             █ █             █ █             █ █             █ █             █ █             █ █
                                                                      █   █           █   █           █   █           █   █           █   █           █   █           █   █           █   █
                                                                     █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █
                                                                    █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █
                                                                   █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █
                                                                  █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █
                                                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
                                                                █                                                                                                                               █
                                                               █ █                                                                                                                             █ █
                                                              █   █                                                                                                                           █   █
                                                             █ █ █ █                                                                                                                         █ █ █ █
                                                            █       █                                                                                                                       █       █
                                                           █ █     █ █                                                                                                                     █ █     █ █
                                                          █   █   █   █                                                                                                                   █   █   █   █
                                                         █ █ █ █ █ █ █ █                                                                                                                 █ █ █ █ █ █ █ █
                                                        █               █                                                                                                               █               █
                                                       █ █             █ █                                                                                                             █ █             █ █
                                                      █   █           █   █                                                                                                           █   █           █   █
                                                     █ █ █ █         █ █ █ █                                                                                                         █ █ █ █         █ █ █ █
                                                    █       █       █       █                                                                                                       █       █       █       █
                                                   █ █     █ █     █ █     █ █                                                                                                     █ █     █ █     █ █     █ █
                                                  █   █   █   █   █   █   █   █                                                                                                   █   █   █   █   █   █   █   █
                                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █                                                                                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
                                                █                               █                                                                                               █                               █
                                               █ █                             █ █                                                                                             █ █                             █ █
                                              █   █                           █   █                                                                                           █   █                           █   █
                                             █ █ █ █                         █ █ █ █                                                                                         █ █ █ █                         █ █ █ █
                                            █       █                       █       █                                                                                       █       █                       █       █
                                           █ █     █ █                     █ █     █ █                                                                                     █ █     █ █                     █ █     █ █
                                          █   █   █   █                   █   █   █   █                                                                                   █   █   █   █                   █   █   █   █
                                         █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █                                                                                 █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █
                                        █               █               █               █                                                                               █               █               █               █
                                       █ █             █ █             █ █             █ █                                                                             █ █             █ █             █ █             █ █
                                      █   █           █   █           █   █           █   █                                                                           █   █           █   █           █   █           █   █
                                     █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █                                                                         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █
                                    █       █       █       █       █       █       █       █                                                                       █       █       █       █       █       █       █       █
                                   █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █                                                                     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █
                                  █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █                                                                   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █
                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █                                                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
                                █                                                               █                                                               █                                                               █
                               █ █                                                             █ █                                                             █ █                                                             █ █
                              █   █                                                           █   █                                                           █   █                                                           █   █
                             █ █ █ █                                                         █ █ █ █                                                         █ █ █ █                                                         █ █ █ █
                            █       █                                                       █       █                                                       █       █                                                       █       █
                           █ █     █ █                                                     █ █     █ █                                                     █ █     █ █                                                     █ █     █ █
                          █   █   █   █                                                   █   █   █   █                                                   █   █   █   █                                                   █   █   █   █
                         █ █ █ █ █ █ █ █                                                 █ █ █ █ █ █ █ █                                                 █ █ █ █ █ █ █ █                                                 █ █ █ █ █ █ █ █
                        █               █                                               █               █                                               █               █                                               █               █
                       █ █             █ █                                             █ █             █ █                                             █ █             █ █                                             █ █             █ █
                      █   █           █   █                                           █   █           █   █                                           █   █           █   █                                           █   █           █   █
                     █ █ █ █         █ █ █ █                                         █ █ █ █         █ █ █ █                                         █ █ █ █         █ █ █ █                                         █ █ █ █         █ █ █ █
                    █       █       █       █                                       █       █       █       █                                       █       █       █       █                                       █       █       █       █
                   █ █     █ █     █ █     █ █                                     █ █     █ █     █ █     █ █                                     █ █     █ █     █ █     █ █                                     █ █     █ █     █ █     █ █
                  █   █   █   █   █   █   █   █                                   █   █   █   █   █   █   █   █                                   █   █   █   █   █   █   █   █                                   █   █   █   █   █   █   █   █
                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █                                 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
                █                               █                               █                               █                               █                               █                               █                               █
               █ █                             █ █                             █ █                             █ █                             █ █                             █ █                             █ █                             █ █
              █   █                           █   █                           █   █                           █   █                           █   █                           █   █                           █   █                           █   █
             █ █ █ █                         █ █ █ █                         █ █ █ █                         █ █ █ █                         █ █ █ █                         █ █ █ █                         █ █ █ █                         █ █ █ █
            █       █                       █       █                       █       █                       █       █                       █       █                       █       █                       █       █                       █       █
           █ █     █ █                     █ █     █ █                     █ █     █ █                     █ █     █ █                     █ █     █ █                     █ █     █ █                     █ █     █ █                     █ █     █ █
          █   █   █   █                   █   █   █   █                   █   █   █   █                   █   █   █   █                   █   █   █   █                   █   █   █   █                   █   █   █   █                   █   █   █   █
         █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █                 █ █ █ █ █ █ █ █
        █               █               █               █               █               █               █               █               █               █               █               █               █               █               █               █
       █ █             █ █             █ █             █ █             █ █             █ █             █ █             █ █             █ █             █ █             █ █             █ █             █ █             █ █             █ █             █ █
      █   █           █   █           █   █           █   █           █   █           █   █           █   █           █   █           █   █           █   █           █   █           █   █           █   █           █   █           █   █           █   █
     █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █         █ █ █ █
    █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █       █
   █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █     █ █
  █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █   █
 █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █

```

</b> 




Output with an input of   '''64'''   can be viewed at:   [[Sierpinski triangle/REXX output 64]]






## Ring


```ring

# Project : Sierpinski triangle

norder=4
xy = list(40)
for i = 1 to 40
    xy[i] = "                               "
next 
triangle(1, 1, norder)
for i = 1 to 36
    see xy[i] + nl
next 
 
func triangle(x, y, n)
     if n = 0
        xy[y] = left(xy[y],x-1) + "*" + substr(xy[y],x+1)
     else
        n=n-1
        length=pow(2,n)
        triangle(x, y+length, n)
        triangle(x+length, y, n)
        triangle(x+length*2, y+length, n)
     ok

```

Output:

```txt

               *               
              * *              
             *   *             
            * * * *            
           *       *           
          * *     * *          
         *   *   *   *         
        * * * * * * * *        
       *               *       
      * *             * *      
     *   *           *   *     
    * * * *         * * * *    
   *       *       *       *   
  * *     * *     * *     * *  
 *   *   *   *   *   *   *   * 
* * * * * * * * * * * * * * * *

```



## Ruby

From the command line:

```ruby
ruby -le'16.times{|y|print" "*(15-y),*(0..y).map{|x|~y&x>0?"  ":" *"}}'
```


or, {{trans|Python}}

```ruby
def sierpinski_triangle(n)
  triangle = ["*"]
  n.times do |i|
    sp = " " * (2**i)
    triangle = triangle.collect {|x| sp + x + sp} +
               triangle.collect {|x| x + " " + x}
  end
  triangle
end

puts sierpinski_triangle(4)
```


Using fold / reduce (aka. inject):


```ruby
def sierpinski_triangle(n)
  (0...n).inject(["*"]) {|triangle, i| 
    space = " " * (2**i)
    triangle.map {|x| space + x + space} + triangle.map {|x| x + " " + x}
  }
end

puts sierpinski_triangle(4)
```



## Run BASIC


```runbasic
nOrder=4
dim xy$(40)
for i = 1 to 40
    xy$(i) = "                               "
next i
call triangle 1, 1, nOrder
for i = 1 to 36
    print xy$(i)
next i
end
 
SUB triangle x, y, n
    IF n = 0 THEN
        xy$(y) = left$(xy$(y),x-1) + "*" + mid$(xy$(y),x+1)
    ELSE
        n=n-1
        length=2^n
        call triangle x, y+length, n
        call triangle x+length, y, n
        call triangle x+length*2, y+length, n
    END IF
END SUB
```


```txt
               *               
              * *              
             *   *             
            * * * *            
           *       *           
          * *     * *          
         *   *   *   *         
        * * * * * * * *        
       *               *       
      * *             * *      
     *   *           *   *     
    * * * *         * * * *    
   *       *       *       *   
  * *     * *     * *     * *  
 *   *   *   *   *   *   *   * 
* * * * * * * * * * * * * * * *
```



## Scala

The Ruby command-line version (on Windows):

```scala
scala -e "for(y<-0 to 15){println(\" \"*(15-y)++(0 to y).map(x=>if((~y&x)>0)\"  \"else\" *\")mkString)}"
```


The Forth version:

```scala
def sierpinski(n: Int) {
  def star(n: Long) = if ((n & 1L) == 1L) "*" else " "
  def stars(n: Long): String = if (n == 0L) "" else star(n) + " " + stars(n >> 1)
  def spaces(n: Int) = " " * n
  ((1 << n) - 1 to 0 by -1).foldLeft(1L) {
    case (bitmap, remainingLines) =>
      println(spaces(remainingLines) + stars(bitmap))
      (bitmap << 1) ^ bitmap
  }
}
```


The Haskell version:

```scala
def printSierpinski(n: Int) {
  def sierpinski(n: Int): List[String] = {
    lazy val down = sierpinski(n - 1)
    lazy val space = " " * (1 << (n - 1))
    n match {
      case 0 => List("*")
      case _ => (down map (space + _ + space)) :::
                (down map (List.fill(2)(_) mkString " "))
    }
  }
  sierpinski(n) foreach println
}
```



## Scheme

{{trans|Haskell}}

```scheme
(define (sierpinski n)
  (for-each
   (lambda (x) (display (list->string x)) (newline))
   (let loop ((acc (list (list #\*))) (spaces (list #\ )) (n n))
     (if (zero? n)
         acc
         (loop
          (append
           (map (lambda (x) (append spaces x spaces)) acc)
           (map (lambda (x) (append x (list #\ ) x)) acc))
          (append spaces spaces)
          (- n 1))))))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func array string: sierpinski (in integer: n) is func
  result
    var array string: parts is 1 times "*";
  local
    var integer: i is 0;
    var string: space is " ";
    var array string: parts2 is 0 times "";
    var string: x is "";
  begin
    for i range 1 to n do
      parts2 := 0 times "";
      for x range parts do
        parts2 &:= [] (space & x & space);
      end for;
      for x range parts do
        parts2 &:= [] (x & " " & x);
      end for;
      parts := parts2;
      space &:= space;
    end for;
  end func;
 
const proc: main is func
  begin
    writeln(join(sierpinski(4), "\n"));
  end func;
```



## Sidef


```ruby
func sierpinski_triangle(n) {
    var triangle = ['*']
    { |i|
        var sp = (' ' * 2**i)
        triangle = (triangle.map {|x| sp + x + sp} +
                    triangle.map {|x| x + ' ' + x})
    } * n
    triangle.join("\n")
}
 
say sierpinski_triangle(4)
```



## Swift

{{trans|Java}}
<lang>import Foundation

// Easy get/set of charAt
extension String {
    subscript(index:Int) -> String {
        get {
            var array = Array(self)
            var charAtIndex = array[index]
            return String(charAtIndex)
        }
        
        set(newValue) {
            var asChar = Character(newValue)
            var array = Array(self)
            array[index] = asChar
            self = String(array)
        }
    }
}

func triangle(var n:Int) {
    n = 1 << n
    var line = ""
    var t = ""
    var u = ""
    
    for (var i = 0; i <= 2 * n; i++) {
        line += " "
    }
    
    line[n] = "*"
    
    for (var i = 0; i < n; i++) {
        println(line)
        u = "*"
        for (var j = n - i; j < n + i + 1; j++) {
            t = line[j-1] == line[j + 1] ? " " : "*"
            line[j - 1] = u
            u = t
        }
        line[n + i] = t
        line[n + i + 1] = "*"
    }
}
```



## Tcl

{{trans|Perl}}

```tcl
package require Tcl 8.5

proc map {lambda list} {
    foreach elem $list {
        lappend result [apply $lambda $elem]
    }
    return $result
}

proc sierpinski_triangle n {
    set down [list *]
    set space " "
    for {set i 1} {$i <= $n} {incr i} {
        set down [concat \
            [map [subst -nocommands {x {expr {"$space[set x]$space"}}}] $down] \
            [map {x {expr {"$x $x"}}} $down] \
        ]
        append space $space
    }
    return [join $down \n]
}

puts [sierpinski_triangle 4]
```


=={{header|TI-83 BASIC}}==
Uses Wolfram Rule 90.

```ti83b
PROGRAM:SIRPNSKI
:ClrHome
:Output(1,8,"^")
:{0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0}→L1
:{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}→L2
:L2→L3
:For(X,2,8,1)
:For(Y,2,17,1)
:If L1(Y-1)
:Then
:4→N
:End
:If L1(Y)
:Then
:N+2→N
:End
:If L1(Y+1)
:Then
:N+1→N
:End
:If N=1 or N=3 or N=4 or N=6
:Then
:1→L2(Y)
:Output(X,Y-1,"^")
:End
:0→N
:End
:L2→L1
:L3→L2
:End

```



## uBasic/4tH

<lang>Input "Triangle order: ";n
n = 2^n

For y = n - 1 To 0 Step -1

  For i = 0 To y
    Print " ";
  Next

  x = 0

  For x = 0 Step 1 While ((x + y) < n)
     If AND (x,y) Then
        Print "  ";
     Else
        Print "* ";
     EndIf
  Next

  Print
Next
End
```



## Unlambda


```Unlambda
```ci``s``s`ks``s`k`s``s`kc``s``s``si`kr`k. `k.*k
`k``s``s``s``s`s`k`s``s`ksk`k``s``si`kk`k``s`kkk
`k``s`k`s``si`kk``s`kk``s``s``s``si`kk`k`s`k`s``s`ksk`k`s`k`s`k`si``si`k`ki
`k``s`k`s``si`k`ki``s`kk``s``s``s``si`kk`k`s`k`s`k`si`k`s`k`s``s`ksk``si`k`ki
`k`ki``s`k`s`k`si``s`kkk
```

This produces an infinite, left-justified triangle:
<pre style="height:30ex;overflow:scroll;">
*
**
* *
****
*   *
**  **
* * * *
********
*       *
**      **
* *     * *
****    ****
*   *   *   *
**  **  **  **
* * * * * * * *
****************
*               *
**              **
* *             * *
****            ****
*   *           *   *
**  **          **  **
* * * *         * * * *
********        ********
*       *       *       *
**      **      **      **
* *     * *     * *     * *
****    ****    ****    ****
*   *   *   *   *   *   *   *
**  **  **  **  **  **  **  **
* * * * * * * * * * * * * * * *
********************************
*                               *
**                              **
* *                             * *
            ........

```



## Ursala

the straightforward recursive solution

```Ursala
#import nat

triangle = ~&a^?\<<&>>! ^|RNSiDlrTSPxSxNiCK9xSx4NiCSplrTSPT/~& predecessor
```

the cheeky cellular automaton solution

```Ursala
#import std
#import nat

rule       = -$<0,&,0,0,&,0,0,0>@rSS zipp0*ziD iota8
evolve "n" = @iNC ~&x+ rep"n" ^C\~& @h rule*+ swin3+ :/0+ --<0>
sierpinski = iota; --<&>@NS; iota; ^H/evolve@z @NS ^T/~& :/&
```

an example of each (converting from booleans to characters)

```Ursala
#show+

examples = mat0 ~&?(`*!,` !)*** <sierpinski3,triangle4>
```

{{out}}
<pre style="height:30ex;overflow:scroll;">
        *        
       * *       
      *   *      
     * * * *     
    *       *    
   * *     * *   
  *   *   *   *  
 * * * * * * * * 

               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *

```



## VBA

{{Trans|Phix}}
```vb
Sub sierpinski(n As Integer)
    Dim lim As Integer: lim = 2 ^ n - 1
    For y = lim To 0 Step -1
        Debug.Print String$(y, " ")
        For x = 0 To lim - y
            Debug.Print IIf(x And y, "  ", "# ");
        Next
        Debug.Print
    Next y
End Sub
Public Sub main()
    Dim i As Integer
    For i = 1 To 5
        sierpinski i
    Next i
End Sub
```
{{out}}
<pre style="font-size: 4px"> 
# 

# # 
   
# 
  
# # 
 
#   # 

# # # # 
       
# 
      
# # 
     
#   # 
    
# # # # 
   
#       # 
  
# #     # # 
 
#   #   #   # 

# # # # # # # # 
               
# 
              
# # 
             
#   # 
            
# # # # 
           
#       # 
          
# #     # # 
         
#   #   #   # 
        
# # # # # # # # 
       
#               # 
      
# #             # # 
     
#   #           #   # 
    
# # # #         # # # # 
   
#       #       #       # 
  
# #     # #     # #     # # 
 
#   #   #   #   #   #   #   # 

# # # # # # # # # # # # # # # # 
                               
# 
                              
# # 
                             
#   # 
                            
# # # # 
                           
#       # 
                          
# #     # # 
                         
#   #   #   # 
                        
# # # # # # # # 
                       
#               # 
                      
# #             # # 
                     
#   #           #   # 
                    
# # # #         # # # # 
                   
#       #       #       # 
                  
# #     # #     # #     # # 
                 
#   #   #   #   #   #   #   # 
                
# # # # # # # # # # # # # # # # 
               
#                               # 
              
# #                             # # 
             
#   #                           #   # 
            
# # # #                         # # # # 
           
#       #                       #       # 
          
# #     # #                     # #     # # 
         
#   #   #   #                   #   #   #   # 
        
# # # # # # # #                 # # # # # # # # 
       
#               #               #               # 
      
# #             # #             # #             # # 
     
#   #           #   #           #   #           #   # 
    
# # # #         # # # #         # # # #         # # # # 
   
#       #       #       #       #       #       #       # 
  
# #     # #     # #     # #     # #     # #     # #     # # 
 
#   #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
```


## VBScript

{{trans|PowerShell}}

```vb

Sub triangle(o)
	n = 2 ^ o
	Dim line()
	ReDim line(2*n)
	line(n) = "*"
	i = 0
	Do While i < n
		WScript.StdOut.WriteLine Join(line,"")
		u = "*"
		j = n - i
		Do While j < (n+i+1)
			If line(j-1) = line(j+1) Then
				t = " "
			Else
				t = "*"
			End If
			line(j-1) = u
			u = t
			j = j + 1
		Loop
		line(n+i) = t
		line(n+i+1) = "*"
		i = i + 1
	Loop
End Sub

triangle(4)

```



## Vedit macro language


### Iterative

{{trans|JavaScript}}
The macro writes the fractal into an edit buffer where it can be viewed and saved to file if required.
This allows creating images larger than screen, the size is only limited by free disk space.

```vedit
#3 = 16    // size (height) of the triangle
Buf_Switch(Buf_Free)				// Open a new buffer for output
Ins_Char(' ', COUNT, #3*2+2)			// fill first line with spaces
Ins_Newline
Line(-1) Goto_Col(#3)
Ins_Char('*', OVERWRITE)			// the top of triangle
for (#10=0; #10 < #3-1; #10++) {
    BOL Reg_Copy(9,1) Reg_Ins(9)		// duplicate the line
    #20 = '*'
    for (#11 = #3-#10; #11 < #3+#10+1; #11++) {
        Goto_Col(#11-1)
	if (Cur_Char==Cur_Char(2)) { #21=' ' } else { #21='*' }
	Ins_Char(#20, OVERWRITE)
	#20 = #21
    }
    Ins_Char(#21, OVERWRITE)
    Ins_Char('*', OVERWRITE)
}
```



### Recursive

{{trans|BASIC}}
Vedit macro language does not have recursive functions, so some pushing and popping is needed to implement recursion.

```vedit
#1 = 1		// x
#2 = 1		// y
#3 = 16		// length (height of the triangle / 2)
#4 = 5		// depth of recursion

Buf_Switch(Buf_Free)		// Open a new buffer for output
Ins_Newline(#3*2)		// Create as many empty lines as needed
Call("Triangle")		// Draw the triangle
BOF
Return

:Triangle:
if (#4 == 0) {
    Goto_Line(#2)
    EOL Ins_Char(' ', COUNT, #1-Cur_Col+1) 	// add spaces if needed
    Goto_Col(#1)
    Ins_Char('*', OVERWRITE)
} else {
    Num_Push(1,4)
    #2 += #3; #3 /= 2; #4--; Call("Triangle")
    Num_Pop(1,4)
    Num_Push(1,4)
    #1 += #3; #3 /= 2; #4--; Call("Triangle")
    Num_Pop(1,4)
    Num_Push(1,4)
    #1 += 2*#3; #2 += #3; #3 /= 2; #4--; Call("Triangle")
    Num_Pop(1,4)
}
Return
```



## X86 Assembly

Translation of XPL0. Assemble with tasm, tlink /t

```asm
        .model  tiny
        .code
        .486
        org     100h
start:  xor     ebx, ebx        ;S1:= 0
        mov     edx, 8000h      ;S2:= $8000
        mov     cx, 16          ;for I:= Size downto 1
tri10:  mov     ebx, edx        ; S1:= S2
tri15:  test    edx, edx        ; while S2#0
        je      tri20
         mov    al, '*'         ; ChOut
         test   dl, 01h         ;  if S2&1 then '*' else ' '
         jne    tri18
          mov   al, ' '
tri18:   int    29h
         shr    edx, 1          ; S2>>1
         jmp    tri15
tri20:  mov     al, 0Dh         ;new line
        int     29h
        mov     al, 0Ah
        int     29h
        shl     ebx, 1          ;S2:= S2 xor S1<<1
        xor     edx, ebx
        shr     ebx, 2          ;S2:= S2 xor S1>>1
        xor     edx, ebx
        loop    tri10           ;next I
        ret
        end     start
```


{{out}}

```txt

               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *

```



## XPL0


```XPL0
code ChOut=8, CrLf=9;
def Order=4, Size=1<<Order;
int S1, S2, I;
[S1:= 0;  S2:= $8000;
for I:= 0 to Size-1 do
    [S1:= S2;
    while S2 do
        [ChOut(0, if S2&1 then ^* else ^ );  S2:= S2>>1];
    CrLf(0);
    S2:= S2 xor S1<<1;
    S2:= S2 xor S1>>1;
    ];
]
```


{{out}}

```txt

               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *

```



## Yabasic

{{trans|Phix}}

```Yabasic
sub rep$(n, c$)
    local i, s$
	
    for i = 1 to n
	s$ = s$ + c$ 
    next
    return s$
end sub

sub sierpinski(n)
    local lim, y, x
	
    lim = 2**n - 1
    for y = lim to 0 step -1
        print rep$(y, " ");
        for x = 0 to lim-y
            if and(x, y) then print "  "; else print "* "; end if
        next
        print
    next
end sub

for i = 1 to 5
    sierpinski(i)
next

```



## zkl

{{trans|D}}

```zkl
level,d := 3,T("*");
foreach n in (level + 1){
   sp:=" "*(2).pow(n);
   d=d.apply('wrap(a){ String(sp,a,sp) }).extend(
     d.apply(fcn(a){ String(a," ",a) }));
}
d.concat("\n").println();
```

{{out}}

```txt

               *               
              * *              
             *   *             
            * * * *            
           *       *           
          * *     * *          
         *   *   *   *         
        * * * * * * * *        
       *               *       
      * *             * *      
     *   *           *   *     
    * * * *         * * * *    
   *       *       *       *   
  * *     * *     * *     * *  
 *   *   *   *   *   *   *   * 
* * * * * * * * * * * * * * * *

```

