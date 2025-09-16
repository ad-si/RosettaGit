+++
title = "Catalan numbers/Pascal's triangle"
description = ""
date = 2019-08-24T20:20:39Z
aliases = []
[extra]
id = 13378
[taxonomies]
categories = ["Mathematics", "Number theory", "task"]
tags = []
languages = [
  "11l",
  "360_assembly",
  "ada",
  "algol_68",
  "algol_w",
  "apl",
  "autohotkey",
  "awk",
  "batch_file",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "elixir",
  "erlang",
  "erre",
  "factor",
  "freebasic",
  "fsharp",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "nim",
  "ocaml",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "purebasic",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scilab",
  "seed7",
  "sidef",
  "smart_basic",
  "tcl",
  "vbscript",
  "visual_basic",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task
Print out the first   '''15'''   Catalan numbers by extracting them from Pascal's triangle.


## See also
*   [https://archive.is/0IrNp Catalan Numbers and the Pascal Triangle]. <!-- Relation Pascal Triangle and the Catalan Numbers Radoslav Jovanovic -->     This method enables calculation of Catalan Numbers using only addition and subtraction.
<!-- '''http://milan.milanovic.org/math/english/fibo/fibo4.html is broken. -->
*   [http://mathworld.wolfram.com/CatalansTriangle.html Catalan's Triangle] for a Number Triangle that generates Catalan Numbers using only addition.
*   Sequence [http://oeis.org/A000108 A000108] on OEIS has a lot of information on Catalan Numbers.

## Related Tasks
[[Pascal's triangle]]





## 11l

{{trans|Python}}

```11l
V n = 15
V t = [0] * (n + 2)
t[1] = 1
L(i) 1 .. n
   L(j) (i .< 1).step(-1)
      t[j] += t[j - 1]
   t[i + 1] = t[i]
   L(j) (i + 1 .< 1).step(-1)
      t[j] += t[j - 1]
   print(t[i + 1] - t[i], end' ‘ ’)
```

{{out}}

```txt

1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```



## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set.

```360asm
CATALAN  CSECT
         USING  CATALAN,R13,R12
SAVEAREA B      STM-SAVEAREA(R15)
         DC     17F'0'
         DC     CL8'CATALAN'
STM      STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15
         LA     R12,4095(R13)
         LA     R12,1(R12)
*        ----   CODE
         LA     R0,1
         ST     R0,T            t(1)=1
         LA     R4,0            ix:i=1
         LA     R6,1            by 1
         LH     R7,N            to n
LOOPI    BXH    R4,R6,ENDLOOPI  loop i
         LR     R5,R4           ix:j=i+1
         LA     R5,2(R5)        i+2
         LA     R8,0
         BCTR   R8,0            by -1
         LA     R9,1            to 2
LOOP1J   BXLE   R5,R8,ENLOOP1J  loop j
         LR     R10,R5          j
         BCTR   R10,0
         SLA    R10,2
         L      R2,T(R10)       r2=t(j)
         LR     R1,R10          j
         SH     R1,=H'4'
         L      R3,T(R1)        r3=t(j-1)
         AR     R2,R3           r2=r2+r3
         ST     R2,T(R10)       t(j)=t(j)+t(j-1)
         B      LOOP1J
ENLOOP1J EQU    *
         LR     R1,R4           i
         BCTR   R1,0
         SLA    R1,2
         L      R3,T(R1)        t(i)
         LA     R1,4(R1)
         ST     R3,T(R1)        t(i+1)
         LR     R5,R4           ix:j=i+2
         LA     R5,3(R5)        i+3
         LA     R8,0
         BCTR   R8,0            by -1
         LA     R9,1            to 2
LOOP2J   BXLE   R5,R8,ENLOOP2J  loop j
         LR     R10,R5          j
         BCTR   R10,0
         SLA    R10,2
         L      R2,T(R10)       r2=t(j)
         LR     R1,R10          j
         SH     R1,=H'4'
         L      R3,T(R1)        r3=t(j-1)
         AR     R2,R3           r2=r2+r3
         ST     R2,T(R10)       t(j)=t(j)+t(j-1)
         B      LOOP2J
ENLOOP2J EQU    *
         LR     R1,R4           i
         BCTR   R1,0
         SLA    R1,2
         L      R2,T(R1)        t(i)
         LA     R1,4(R1)
         L      R3,T(R1)        t(i+1)
         SR     R3,R2
         CVD    R3,P
         UNPK   Z,P
         MVC    C,Z
         OI     C+L'C-1,X'F0'
         MVC    WTOBUF(8),C+8
         WTO    MF=(E,WTOMSG)
         B      LOOPI
ENDLOOPI EQU    *
*        ----   END CODE
         CNOP   0,4
         L      R13,4(0,R13)
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14
*        ----   DATA
N        DC     H'15'
T        DC     17F'0'
P        DS     PL8
Z        DS     ZL16
C        DS     CL16
WTOMSG   DS     0F
         DC     H'80'
         DC     H'0'
WTOBUF   DC     CL80' '
         YREGS
         END
```

{{out}}

```txt
00000001
00000002
00000005
00000014
00000042
00000132
00000429
00001430
00004862
00016796
00058786
00208012
00742900
02674440
09694845
```



## Ada


Uses package Pascal from the Pascal triangle solution[[http://rosettacode.org/wiki/Pascal%27s_triangle#Ada]]


```Ada
with Ada.Text_IO, Pascal;

procedure Catalan is

   Last: Positive := 15;
   Row: Pascal.Row := Pascal.First_Row(2*Last+1);

begin
   for I in 1 .. Last loop
      Row := Pascal.Next_Row(Row);
      Row := Pascal.Next_Row(Row);
      Ada.Text_IO.Put(Integer'Image(Row(I+1)-Row(I+2)));
   end loop;
end Catalan;
```


{{out}}


```txt
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
```



## ALGOL 68

{{trans|C++}}

```algol68
INT n = 15;
[ 0 : n + 1 ]INT t;
t[0] := 0;
t[1] := 1;
FOR i TO n DO
    FOR j FROM i   BY -1 TO 2 DO t[j] := t[j] + t[j-1] OD;
    t[i+1] := t[i];
    FOR j FROM i+1 BY -1 TO 2 DO t[j] := t[j] + t[j-1] OD;
    print( ( whole( t[i+1] - t[i], 0 ), " " ) )
OD
```

{{out}}

```txt

1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```



## ALGOL W


```algolw
begin
    % print the first 15 Catalan numbers from Pascal's triangle %
    integer n;
    n := 15;
    begin
        integer array pascalLine ( 1 :: n + 1 );
        % the Catalan numbers are the differences between the middle and middle - 1 numbers of the odd %
        % lines of Pascal's triangle (lines with 3 or more numbers)                                    %
        % note - we only need to calculate the left side of the triangle                               %
        pascalLine( 1 ) := 1;
        for c := 2 until n + 1 do begin
            % even line %
            for i := c - 1 step -1 until 2 do pascalLine( i ) := pascalLine( i - 1 ) + pascalLine( i );
            pascalLine( c ) := pascalLine( c - 1 );
            % odd line %
            for i := c     step -1 until 2 do pascalLine( i ) := pascalLine( i - 1 ) + pascalLine( i );
            writeon( i_w := 1, s_w := 0, " ", pascalLine( c ) - pascalLine( c - 1 ) )
        end for_c
    end
end.
```

{{out}}

```txt

 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```



## APL


```apl

      ⍝ Based heavily on the J solution
      CATALAN←{¯1↓↑-/1 ¯1↓¨(⊂⎕IO+0 0)⍉¨0 2⌽¨⊂(⎕IO-⍨⍳N){+\⍣⍺⊢⍵}⍤0 1⊢1⍴⍨N←⍵+2}

```

{{out}}

```txt

      CATALAN 15
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```



## AutoHotkey

{{works with|AutoHotkey_L}}

```AutoHotkey
/* Generate Catalan Numbers
//
// smgs: 20th Feb, 2014
*/
Array := [], Array[2,1] := Array[2,2] := 1 ; Array inititated and 2nd row of pascal's triangle assigned
INI := 3 ; starts with calculating the 3rd row and as such the value
Loop, 31 ; every odd row is taken for calculating catalan number as such to obtain 15 we need 2n+1
{
	if ( A_index > 2 )
	{
		Loop, % A_INDEX
		{
			old := ini-1, 		index := A_index, 		index_1 := A_index + 1
			Array[ini, index_1] := Array[old, index] + Array[old, index_1]
			Array[ini, 1] := Array[ini, ini] := 1
			line .= Array[ini, A_index] " "
		}
	;~ MsgBox % line ; gives rows of pascal's triangle
	; calculating every odd row starting from 1st so as to obtain catalan's numbers
		if ( mod(ini,2) != 0)
		{
			StringSplit, res, line, %A_Space%
			ans := res0//2, ans_1 := ans++
			result := result . res%ans_1% - res%ans% " "
		}
	line :=
	ini++
	}
}
MsgBox % result
```

{{out|Produces}}

```txt

1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```


## AWK


```AWK

# syntax: GAWK -f CATALAN_NUMBERS_PASCALS_TRIANGLE.AWK
# converted from C
BEGIN {
    printf("1")
    for (n=2; n<=15; n++) {
      num = den = 1
      for (k=2; k<=n; k++) {
        num *= (n + k)
        den *= k
        catalan = num / den
      }
      printf(" %d",catalan)
    }
    printf("\n")
    exit(0)
}

```

{{out}}

```txt

1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```



## Batch File


```dos
@echo off
setlocal ENABLEDELAYEDEXPANSION
set n=15
set /A nn=n+1
for /L %%i in (0,1,%nn%) do set t.%%i=0
set t.1=1
for /L %%i in (1,1,%n%) do (
    set /A ip=%%i+1
    for /L %%j in (%%i,-1,1) do (
        set /A jm=%%j-1
	    set /A t.%%j=t.%%j+t.!jm!
	)
    set /A t.!ip!=t.%%i
    for /L %%j in (!ip!,-1,1) do (
        set /A jm=%%j-1
	    set /A t.%%j=t.%%j+t.!jm!
	)
    set /A ci=t.!ip!-t.%%i
	echo !ci!
  )
)
pause
```

{{Out}}

```txt
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
9694845
```



## C


```c

//This code implements the print of 15 first Catalan's Numbers
//Formula used:
//  __n__
//   | |  (n + k) / k  n>0
//   k=2

#include <stdio.h>
#include <stdlib.h>

//the number of Catalan's Numbers to be printed
const int N = 15;

int main()
{
    //loop variables (in registers)
    register int k, n;

    //necessarily ull for reach big values
    unsigned long long int num, den;

    //the nmmber
    int catalan;

    //the first is not calculated for the formula
    printf("1 ");

    //iterating from 2 to 15
    for (n=2; n<=N; ++n) {
        //initializaing for products
        num = den = 1;
        //applying the formula
        for (k=2; k<=n; ++k) {
            num *= (n+k);
            den *= k;
            catalan = num /den;
        }

        //output
        printf("%d ", catalan);
    }

    //the end
    printf("\n");
    return 0;
}

```


{{out}}

```txt

1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```



## C++


```cpp
// Generate Catalan Numbers
//
// Nigel Galloway: June 9th., 2012
//
#include <iostream>
int main() {
  const int N = 15;
  int t[N+2] = {0,1};
  for(int i = 1; i<=N; i++){
    for(int j = i; j>1; j--) t[j] = t[j] + t[j-1];
    t[i+1] = t[i];
    for(int j = i+1; j>1; j--) t[j] = t[j] + t[j-1];
    std::cout << t[i+1] - t[i] << " ";
  }
  return 0;
}
```

{{out|Produces}}

```txt

1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```



## C#

{{trans|C++}}

```c#

int n = 15;
List<int> t = new List<int>() { 0, 1 };
for (int i = 1; i <= n; i++)
{
    for (var j = i; j > 1; j--) t[j] += t[j - 1];
    t.Add(t[i]);
    for (var j = i + 1; j > 1; j--) t[j] += t[j - 1];
    Console.Write(((i == 1) ? "" : ", ") + (t[i + 1] - t[i]));
}

```

{{out|Produces}}

```txt

1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845

```




## Common Lisp



```Lisp
(defun catalan (n)
  "Return the n-th Catalan number"
  (if (<= n 1)  1
    (let ((result 2))
      (dotimes (k (- n 2) result)
        (setq result (* result (/ (+ n k 2) (+ k 2)))) ))))


(dotimes (n 15)
  (print (catalan (1+ n))) )
```


{{out}}


```txt
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
9694845
```




## D

{{trans|C++}}

```d
void main() {
    import std.stdio;

    enum uint N = 15;
    uint[N + 2] t;
    t[1] = 1;

    foreach (immutable i; 1 .. N + 1) {
        foreach_reverse (immutable j; 2 .. i + 1)
            t[j] += t[j - 1];
        t[i + 1] = t[i];
        foreach_reverse (immutable j; 2 .. i + 2)
            t[j] += t[j - 1];
        write(t[i + 1] - t[i], ' ');
    }
}
```

{{out}}

```txt
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
```



## EchoLisp


```scheme

(define dim 100)
(define-syntax-rule  (Tidx i j)  (+ i (* dim j)))

;; generates Catalan's triangle
;; T (i , j) = T(i-1,j) + T (i, j-1)

(define (T n)
	(define i (modulo n dim))
	(define j (quotient n dim))
	(cond
		((zero? i) 1) ;; left column = 1
		((= i j) (T (Tidx (1- i) j))) ;; diagonal value = left value
		(else (+ (T (Tidx (1- i) j)) (T (Tidx i (1- j)))))))

(remember 'T #(1))

```

{{out}}

```scheme

;; take elements on diagonal = Catalan numbers
(for ((i (in-range 0 16))) (write (T (Tidx i i))))

 → 1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```



## Elixir


```elixir
defmodule Catalan do
  def numbers(num) do
    {result,_} = Enum.reduce(1..num, {[],{0,1}}, fn i,{list,t0} ->
      t1 = numbers(i, t0)
      t2 = numbers(i+1, Tuple.insert_at(t1, i+1, elem(t1, i)))
      {[elem(t2, i+1) - elem(t2, i) | list], t2}
    end)
    Enum.reverse(result)
  end

  defp numbers(0, t), do: t
  defp numbers(n, t), do: numbers(n-1, put_elem(t, n, elem(t, n-1) + elem(t, n)))
end

IO.inspect Catalan.numbers(15)
```


{{out}}

```txt

[1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845]

```


## Erlang


```erlang

-module(catalin).
-compile(export_all).
mul(N,D,S,S)->
	N2=N*(S+S),
	D2=D*S,
	K = N2 div D2 ;
mul(N,D,S,L)->
	N2=N*(S+L),
	D2=D*L,
	K = mul(N2,D2,S,L+1).

catl(Ans,16) -> Ans;
catl(D,S)->
	C=mul(1,1,S,2),
	catl([D|C],S+1).
main()->
	Ans=catl(1,2).

```


## ERRE


```ERRE

PROGRAM CATALAN

!$DOUBLE

DIM CATALAN[50]

FUNCTION ODD(X)
    ODD=FRC(X/2)<>0
END FUNCTION

PROCEDURE GETCATALAN(L)
    LOCAL J,K,W
    LOCAL DIM PASTRI[100]

    L=L*2
    PASTRI[0]=1
    J=0
    WHILE J<L DO
       J+=1
       K=INT((J+1)/2)
       PASTRI[K]=PASTRI[K-1]
       FOR W=K TO 1 STEP -1 DO
          PASTRI[W]+=PASTRI[W-1]
       END FOR
       IF NOT(ODD(J)) THEN
          K=INT(J/2)
          CATALAN[K]=PASTRI[K]-PASTRI[K-1]
       END IF
    END WHILE
END PROCEDURE

BEGIN
   LL=15
   GETCATALAN(LL)
   FOR I=1 TO LL DO
      WRITE("### ####################";I;CATALAN[I])
   END FOR
END PROGRAM

```

{{out}}

```txt

  1                    1
  2                    2
  3                    5
  4                   14
  5                   42
  6                  132
  7                  429
  8                 1430
  9                 4862
 10                16796
 11                58786
 12               208012
 13               742900
 14              2674440
 15              9694845

```



## F#


```F#

let mutable nm=uint64(1)
let mutable dm=uint64(1)
let mutable a=uint64(1)

printf "1, "
for i = 2 to 15 do
    nm<-uint64(1)
    dm<-uint64(1)
    for k = 2 to i do
        nm <-uint64( uint64(nm) * (uint64(i)+uint64(k)))
        dm <-uint64( uint64(dm) * uint64(k))
    let a = uint64(uint64(nm)/uint64(dm))
    printf "%u"a
    if(i<>15) then
        printf ", "

```



## Factor


```factor
USING: arrays grouping io kernel math prettyprint sequences ;
IN: rosetta-code.catalan-pascal

: next-row ( seq -- seq' )
    2 clump [ sum ] map 1 prefix 1 suffix ;

: pascal ( n -- seq )
    1 - { { 1 } } swap [ dup last next-row suffix ] times ;

15 2 * pascal [ length odd? ] filter [
    dup length 1 = [ 1 ]
    [ dup midpoint@ dup 1 + 2array swap nths first2 - ] if
    pprint bl
] each drop
```

{{out}}

```txt

1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440

```



## FreeBASIC


```freebasic
' version 15-09-2015
' compile with: fbc -s console

#Define size 31                 ' (N * 2 + 1)

Sub pascal_triangle(rows As Integer, Pas_tri() As ULongInt)

    Dim As Integer x, y

    For x = 1 To rows
        Pas_tri(1,x) = 1
        Pas_tri(x,1) = 1
    Next

    For x = 2 To rows
        For y = 2 To rows + 1 - x
            Pas_tri(x, y) = pas_tri(x - 1 , y) + pas_tri(x, y - 1)
        Next
    Next

End Sub

' ------=< MAIN >=------

Dim As Integer count, row
Dim As ULongInt triangle(1 To size, 1 To size)

pascal_triangle(size, triangle())

'  1   1   1   1   1   1
'  1   2   3   4   5   6
'  1   3   6  10  15  21
'  1   4  10  20  35  56
'  1   5  15  35  70 126
'  1   6  21  56 126 252
' The Pascal triangle is rotated 45 deg.
' to find the Catalan number we need to follow the diagonal
' for top left to bottom right
' take the number on diagonal and subtract the number in de cell
' one up and one to right
' 1 (2 - 1), 2 (6 - 4), 5 (20 - 15) ...


Print "The first 15 Catalan numbers are" : print
count = 1 : row = 2
Do
    Print Using "###: #########"; count; triangle(row, row) - triangle(row +1, row -1)
    row = row + 1
    count =  count + 1
Loop Until count > 15

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
The first 15 Catalan numbers are

  1:         1
  2:         2
  3:         5
  4:        14
  5:        42
  6:       132
  7:       429
  8:      1430
  9:      4862
 10:     16796
 11:     58786
 12:    208012
 13:    742900
 14:   2674440
 15:   9694845
```



## Go

{{trans|C++}}

```go
package main

import "fmt"

func main() {
    const n = 15
    t := [n + 2]uint64{0, 1}
    for i := 1; i <= n; i++ {
        for j := i; j > 1; j-- {
            t[j] += t[j-1]
        }
        t[i+1] = t[i]
        for j := i + 1; j > 1; j-- {
            t[j] += t[j-1]
        }
        fmt.Printf("%2d : %d\n", i, t[i+1]-t[i])
    }
}
```


{{out}}

```txt

 1 : 1
 2 : 2
 3 : 5
 4 : 14
 5 : 42
 6 : 132
 7 : 429
 8 : 1430
 9 : 4862
10 : 16796
11 : 58786
12 : 208012
13 : 742900
14 : 2674440
15 : 9694845

```



## Groovy

{{trans|C}}

```Groovy

class Catalan
{
 public static void main(String[] args)
  {
    BigInteger N = 15;
    BigInteger k,n,num,den;
    BigInteger  catalan;
      print(1);
       for(n=2;n<=N;n++)
          {
            num = 1;
            den = 1;
              for(k=2;k<=n;k++)
                 {
                    num = num*(n+k);
                    den = den*k;
                    catalan = num/den;
                 }
            print(" " + catalan);
          }

  }
}
​
```

{{out}}

```txt

1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```



## Haskell

As required by the task this implementation extracts the Catalan numbers from Pascal's triangle, rather
than calculating them directly.  Also, note that it (correctly) produces [1, 1] as the first two numbers.

```haskell
import System.Environment (getArgs)

-- Pascal's triangle.
pascal :: [[Integer]]
pascal = [1] : map (\row -> 1 : zipWith (+) row (tail row) ++ [1]) pascal

-- The Catalan numbers from Pascal's triangle.  This uses a method from
-- http://www.cut-the-knot.org/arithmetic/algebra/CatalanInPascal.shtml
-- (see "Grimaldi").
catalan :: [Integer]
catalan = map (diff . uncurry drop) $ zip [0..] (alt pascal)
  where alt (x:_:zs) = x : alt zs -- every other element of an infinite list
        diff (x:y:_) = x - y
        diff (x:_)   = x

main :: IO ()
main = do
  ns <- fmap (map read) getArgs :: IO [Int]
  mapM_ (print . flip take catalan) ns
```


{{out}}

```txt

./catalan 15
[1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440]

```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.  It avoids computing elements in Pascal's triangle
that aren't used.


```unicon
link math

procedure main(A)
    limit := (integer(A[1])|15)+1
    every write(right(binocoef(i := 2*seq(0)\limit,i/2)-binocoef(i,i/2+1),30))
end
```


Sample run:

```txt

->cn
                             1
                             2
                             5
                            14
                            42
                           132
                           429
                          1430
                          4862
                         16796
                         58786
                        208012
                        742900
                       2674440
                       9694845
->

```



## J



```j
   Catalan=. }:@:(}.@:((<0 1)&|:) - }:@:((<0 1)&|:@:(2&|.)))@:(i. +/\@]^:[ #&1)@:(2&+)
```

{{out|Example use}}

```j
   Catalan 15
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```


A structured derivation of Catalan follows:


```j
   o=. @: NB. Composition of verbs (functions)
   ( PascalTriangle=. i. ((+/\@]^:[)) #&1 ) 5
1 1  1  1  1
1 2  3  4  5
1 3  6 10 15
1 4 10 20 35
1 5 15 35 70
   ( MiddleDiagonal=. (<0 1)&|: )               o PascalTriangle 5
1 2 6 20 70
   ( AdjacentLeft=.   MiddleDiagonal o (2&|.) ) o PascalTriangle 5
1 4 15 1 5

   ( Catalan=. }: o (}. o MiddleDiagonal - }: o AdjacentLeft) o PascalTriangle o (2&+) f. ) 5
1 2 5 14 42

   Catalan
}:@:(}.@:((<0 1)&|:) - }:@:((<0 1)&|:@:(2&|.)))@:(i. +/\@]^:[ #&1)@:(2&+)
```



## Java

{{trans|C++}}

```java
public class Test {
    public static void main(String[] args) {
        int N = 15;
        int[] t = new int[N + 2];
        t[1] = 1;

        for (int i = 1; i <= N; i++) {

            for (int j = i; j > 1; j--)
                t[j] = t[j] + t[j - 1];

            t[i + 1] = t[i];

            for (int j = i + 1; j > 1; j--)
                t[j] = t[j] + t[j - 1];

            System.out.printf("%d ", t[i + 1] - t[i]);
        }
    }
}
```



```txt
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
```



## JavaScript


### ES5

Iteration
{{trans|C++}}

```javascript
var n = 15;
for (var t = [0, 1], i = 1; i <= n; i++) {
    for (var j = i; j > 1; j--) t[j] += t[j - 1];
    t[i + 1] = t[i];
    for (var j = i + 1; j > 1; j--) t[j] += t[j - 1];
    document.write(i == 1 ? '' : ', ', t[i + 1] - t[i]);
}
```

{{out}}

```txt

1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845

```



### ES6

Functional composition
{{Trans|Haskell}}


```JavaScript
(() => {
    'use strict';

    // CATALAN

    // catalanSeries :: Int -> [Int]
    let catalanSeries = n => {
        let alternate = xs => xs.reduce(
                (a, x, i) => i % 2 === 0 ? a.concat([x]) : a, []
            ),
            diff = xs => xs.length > 1 ? xs[0] - xs[1] : xs[0];

        return alternate(pascal(n * 2))
            .map((xs, i) => diff(drop(i, xs)));
    }

    // PASCAL

    // pascal :: Int -> [[Int]]
    let pascal = n => until(
            m => m.level <= 1,
            m => {
                let nxt = zipWith(
                    (a, b) => a + b, [0].concat(m.row), m.row.concat(0)
                );
                return {
                    row: nxt,
                    triangle: m.triangle.concat([nxt]),
                    level: m.level - 1
                }
            }, {
                level: n,
                row: [1],
                triangle: [
                    [1]
                ]
            }
        )
        .triangle;


    // GENERIC FUNCTIONS

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    let zipWith = (f, xs, ys) =>
        xs.length === ys.length ? (
            xs.map((x, i) => f(x, ys[i]))
        ) : undefined;

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    let until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    }

    // drop :: Int -> [a] -> [a]
    let drop = (n, xs) => xs.slice(n);

    // tail :: [a] -> [a]
    let tail = xs => xs.length ? xs.slice(1) : undefined;

    return tail(catalanSeries(16));
})();
```


{{Out}}

```JavaScript
[1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440,9694845]
```



## jq

The first identity (C(2n,n) - C(2n, n-1)) given in the reference is used in accordance with the task description, but it would of course be more efficient to factor out C(2n,n) and use the expression C(2n,n)/(n+1). See also [[Catalan_numbers#jq]] for other alternatives.

''Warning'': jq uses IEEE 754 64-bit arithmetic,
so the algorithm used here for Catalan numbers loses precision for n > 30 and fails completely for n > 510.

```jq
def binomial(n; k):
  if k > n / 2 then binomial(n; n-k)
  else reduce range(1; k+1) as $i (1; . * (n - $i + 1) / $i)
  end;

# Direct (naive) computation using two numbers in Pascal's triangle:
def catalan_by_pascal: . as $n | binomial(2*$n; $n) - binomial(2*$n; $n-1);
```


'''Example''':
 (range(0;16), 30, 31, 510, 511) | [., catalan_by_pascal]
{{Out}}

```sh
$ jq -n -c -f Catalan_numbers_Pascal.jq
[0,0]
[1,1]
[2,2]
[3,5]
[4,14]
[5,42]
[6,132]
[7,429]
[8,1430]
[9,4862]
[10,16796]
[11,58786]
[12,208012]
[13,742900]
[14,2674440]
[15,9694845]
[30,3814986502092304]
[31,14544636039226880]
[510,5.491717746183512e+302]
[511,null]
```



## Julia

{{trans|Matlab}}

```julia
# v0.6

function pascal(n::Int)
    r = ones(Int, n, n)
    for i in 2:n, j in 2:n
        r[i, j] = r[i-1, j] + r[i, j-1]
    end
    return r
end

function catalan_num(n::Int)
    p = pascal(n + 2)
    p[n+4:n+3:end-1] - diag(p, 2)
end

@show catalan_num(15)
```


{{out}}

```txt
catalan_num(15) = [1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845]
```



## Kotlin


```scala
// version 1.1.2

import java.math.BigInteger

val ONE = BigInteger.ONE

fun pascal(n: Int, k: Int): BigInteger {
    if (n == 0 || k == 0) return ONE
    val num = (k + 1..n).fold(ONE) { acc, i -> acc * BigInteger.valueOf(i.toLong()) }
    val den = (2..n - k).fold(ONE) { acc, i -> acc * BigInteger.valueOf(i.toLong()) }
    return num / den
}

fun catalanFromPascal(n: Int) {
    for (i in 1 until n step 2) {
        val mi = i / 2 + 1
        val catalan = pascal(i, mi) - pascal(i, mi - 2)
        println("${"%2d".format(mi)} : $catalan")
    }
}

fun main(args: Array<String>) {
    val n = 15
    catalanFromPascal(n * 2)
}
```


{{out}}

```txt

 1 : 1
 2 : 2
 3 : 5
 4 : 14
 5 : 42
 6 : 132
 7 : 429
 8 : 1430
 9 : 4862
10 : 16796
11 : 58786
12 : 208012
13 : 742900
14 : 2674440
15 : 9694845

```



## Lua

For each line of odd-numbered length from Pascal's triangle, print the middle number minus the one immediately to its right.
This solution is heavily based on the Lua code to generate Pascal's triangle from the page for that task.

```Lua
function nextrow (t)
    local ret = {}
    t[0], t[#t + 1] = 0, 0
    for i = 1, #t do ret[i] = t[i - 1] + t[i] end
    return ret
end

function catalans (n)
    local t, middle = {1}
    for i = 1, n do
        middle = math.ceil(#t / 2)
        io.write(t[middle] - (t[middle + 1] or 0) .. " ")
        t = nextrow(nextrow(t))
    end
end

catalans(15)
```

{{out}}

```txt
1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440
```


## M2000 Interpreter

{{trans|FreeBasic}}
We have to add -1 in For x=2 to rows, because in FreeBasic when x=rows then inner loop never happen  because end value for y is 1, so lower than start value 2. In M2000 this should run from 2 to 1, so we have to exclude this situation from  outer loop, adding -1, and before loop we have to include en exit from sub if rows are less than 2.

We can define integer variables (16 bit), and we can use integer literals numbers using % as last char.

Inside triangle array we use decimal numbers, using @ for first literals, so all additions next produce decimals too.

We use & to pass by reference, here anarray, to sub, but because a sub can see anything in module we can change array name inside sub to same as triangle and we can remove arguments (including size).


```M2000 Interpreter

Module CatalanNumbers {
      Def Integer count, t_row, size=31
      Dim triangle(1 to size, 1 to size)

      \\ call sub
      pascal_triangle(size, &triangle())


      Print "The first 15 Catalan numbers are"
      count = 1% : t_row = 2%

      Do {
            Print  Format$("{0:0:-3}:{1:0:-15}", count, triangle(t_row, t_row) - triangle(t_row +1, t_row -1))
            t_row++
            count++
      } Until count > 15
      End

      Sub pascal_triangle(rows As Integer, &Pas_tri())
          Local x=0%, y=0%
          For x = 1 To rows
              Pas_tri( 1%, x ) = 1@
              Pas_tri( x, 1% ) = 1@
          Next x
          if rows<2 then exit sub
          For x = 2 To rows-1
              For y = 2 To rows + 1 - x
                  Pas_tri(x, y) = pas_tri(x - 1 , y) + pas_tri(x, y - 1)
              Next y
          Next x
      End Sub
}
CatalanNumbers

```

{{out}}

```txt

  1:              1
  2:              2
  3:              5
  4:             14
  5:             42
  6:            132
  7:            429
  8:           1430
  9:           4862
 10:          16796
 11:          58786
 12:         208012
 13:         742900
 14:        2674440
 15:        9694845

```



=={{header|Mathematica}} / {{header|Wolfram Language}}==
This builds the entire Pascal triangle that's needed and holds it in memory. Very inefficienct, but seems to be what is asked in the problem.

```Mathematica
nextrow[lastrow_] := Module[{output},
  output = ConstantArray[1, Length[lastrow] + 1];
  Do[
   output[[i + 1]] = lastrow[[i]] + lastrow[[i + 1]];
   , {i, 1, Length[lastrow] - 1}];
  output
  ]
pascaltriangle[size_] := NestList[nextrow, {1}, size]
catalannumbers[length_] := Module[{output, basetriangle},
  basetriangle = pascaltriangle[2 length];
  list1 = basetriangle[[# *2 + 1, # + 1]] & /@ Range[length];
  list2 = basetriangle[[# *2 + 1, # + 2]] & /@ Range[length];
  list1 - list2
  ]
(* testing *)
catalannumbers[15]
```

{{out}}

```txt
{1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845}
```


=={{header|MATLAB}} / {{header|Octave}}==


```MATLAB
n = 15;
p = pascal(n + 2);
p(n + 4 : n + 3 : end - 1)' - diag(p, 2)
```

{{Out}}

```txt
ans =
         1
         2
         5
        14
        42
       132
       429
      1430
      4862
     16796
     58786
    208012
    742900
   2674440
   9694845
```



## Nim

{{trans|Python}}

```nim
const n = 15
var t = newSeq[int](n + 2)

t[1] = 1
for i in 1..n:
  for j in countdown(i, 1): t[j] += t[j-1]
  t[i+1] = t[i]
  for j in countdown(i+1, 1): t[j] += t[j-1]
  stdout.write t[i+1] - t[i], " "
```

{{Out}}

```txt
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
```



## OCaml


```ocaml

let catalan : int ref = ref 0 in
Printf.printf "%d ," 1 ;
for i = 2 to 9  do
let nm : int ref = ref 1 in
let den : int ref = ref 1 in
for k = 2 to i do
nm := (!nm)*(i+k);
den := (!den)*k;
catalan := (!nm)/(!den) ;
done;
print_int (!catalan); print_string "," ;
done;;

```

{{out}}

```txt

OUTPUT:
1 ,2,5,14,42,132,429,1430,4862

```



## Oforth



```Oforth
import: mapping

: pascal( n -- [] )
   [ 1 ] n #[ dup [ 0 ] + [ 0 ] rot + zipWith( #+ ) ] times ;

: catalan( n -- m )
   n 2 * pascal at( n 1+ ) n 1+ / ;
```


{{out}}

```txt

>#catalan 15 seq map .
[1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845]

```



## PARI/GP


```parigp
vector(15,n,binomial(2*n,n)-binomial(2*n,n+1))
```

{{out}}

```txt
%1 = [1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845]
```



## Pascal


```pascal
type
  tElement = Uint64;
var
  Catalan : array[0..50] of tElement;
procedure GetCatalan(L:longint);
var
  PasTri : array[0..100] of tElement;
  j,k: longInt;
begin
  l := l*2;
  PasTri[0] := 1;
  j    := 0;
  while (j<L) do
  begin
    inc(j);
    k := (j+1) div 2;
    PasTri[k] :=PasTri[k-1];
    For k := k downto 1 do
      inc(PasTri[k],PasTri[k-1]);
    IF NOT(Odd(j)) then
    begin
      k := j div 2;
      Catalan[k] :=PasTri[k]-PasTri[k-1];
    end;
  end;
end;

var
  i,l: longint;
Begin
  l := 15;
  GetCatalan(L);
  For i := 1 to L do
    Writeln(i:3,Catalan[i]:20);
end.
```


```txt
  1                   1
  2                   2
  3                   5
  4                  14
  5                  42
  6                 132
  7                 429
  8                1430
  9                4862
 10               16796
 11               58786
 12              208012
 13              742900
 14             2674440
 15             9694845


```



## Perl

{{trans|C++}}

```perl
use constant N =
 15;
my @t = (0, 1);
for(my $i = 1; $i <= N; $i++) {
    for(my $j = $i; $j > 1; $j--) { $t[$j] += $t[$j-1] }
    $t[$i+1] = $t[$i];
    for(my $j = $i+1; $j>1; $j--) { $t[$j] += $t[$j-1] }
    print $t[$i+1] - $t[$i], " ";
}
```

{{out}}

```txt
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
```


After the 28th Catalan number, this overflows 64-bit integers.  We could add <tt>use bigint;</tt> <tt>use Math::GMP ":constant";</tt> to make it work, albeit not at a fast pace.  However we can use a module to do it much faster:
{{libheader|ntheory}}

```Perl
use ntheory qw/binomial/;
print join(" ", map { binomial( 2*$_, $_) / ($_+1) } 1 .. 1000), "\n";
```


The <tt>Math::Pari</tt> module also has a binomial, but isn't as fast and overflows its stack after 3400.


## Perl 6

{{works with|Rakudo|2015.12}}

```perl6
constant @pascal = [1], -> @p { [0, |@p Z+ |@p, 0] } ... *;

constant @catalan = gather for 2, 4 ... * -> $ix {
    my @row := @pascal[$ix];
    my $mid = +@row div 2;
    take [-] @row[$mid, $mid+1]
}

.say for @catalan[^20];
```

{{out}}

```txt
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
9694845
35357670
129644790
477638700
1767263190
6564120420
```



## Phix

Calculates the minimum pascal triangle in minimum memory. Inspired by the comments in, but not the code of the FreeBasic example

```Phix
constant N = 15 -- accurate to 30, nan/inf for anything over 514 (bigatom version is below).
sequence catalan = {},      -- (>=1 only)
         p = repeat(1,N+1)
atom p1
for i=1 to N do
    p1 = p[1]*2
    catalan = append(catalan,p1-p[2])
    for j=1 to N-i+1 do
        p1 += p[j+1]
        p[j] = p1
    end for
--  ?p[1..N-i+1]
end for
?catalan
```

{{out}}

```txt

{1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440,9694845}

```

Explanatory comments to accompany the above

```Phix
-- FreeBASIC said:
--'  1   1   1   1   1   1
--'  1   2   3   4   5   6
--'  1   3   6  10  15  21
--'  1   4  10  20  35  56
--'  1   5  15  35  70 126
--'  1   6  21  56 126 252
--' The Pascal triangle is rotated 45 deg.
--' to find the Catalan number we need to follow the diagonal
--' for top left to bottom right
--' take the number on diagonal and subtract the number in de cell
--' one up and one to right
--' 1 (2 - 1), 2 (6 - 4), 5 (20 - 15) ...
--
-- The first thing that struck me was it is twice as big as it needs to be,
--  something like this would do...
--    1   1   1   1   1   1
--        2   3   4   5   6
--            6  10  15  21
--               20  35  56
--                   70 126
--                      252
-- It is more obvious from the upper square that the diagonal on that, which is
--  that same as column 1 on this, is twice the previous, which on the second
--  diagram is in column 2. Further, once we have calculated the value for column
--  one above, we can use it immediately to calculate the next catalan number and
--  do not need to store it. Lastly we can overwrite row 1 with row 2 etc in situ,
--  and the following shows what we need for subsequent rounds:
--    1   1   1   1   1
--    3   4   5   6
--   10  15  21
--   35  56
--  126  (unused)
```



###  gmp version

{{libheader|mpfr}}

```Phix
include builtins\mpfr.e

function catalanB(integer n)    -- very very fast!
sequence catalan = mpz_inits(n),
         p = mpz_inits(n+1,1)
mpz p1 = mpz_init(1)
    if n=0 then return p1 end if
    for i=1 to n do
        mpz_mul_si(p1,p[1],2)
        mpz_sub(catalan[i],p1,p[2])
        for j=1 to n-i+1 do
            mpz_add(p1,p1,p[j+1])
            mpz_set(p[j],p1)
        end for
    end for
    return catalan[n]
end function

printf(1,"%d: %s (%s)\n",{100,mpz_get_str(catalanB(100))})
printf(1,"%d: %s (%s)\n",{250,mpz_get_str(catalanB(250))})
```

{{out}}

```txt

100: 896519947090131496687170070074100632420837521538745909320
250: 465116795969233796497747947259667807407291160080922096111953326525143875193659257831340309862635877995262413955019878805418475969029457769094808256

```

The above is significantly faster than the equivalent(s) on [[Catalan_numbers#Phix|Catalan_numbers]],
a quick comparison showing the latter getting exponentially worse (then again I memoised the slowest recursive version):

```txt

            800 2000  4000 8000
catalanB:  0.6s 3.5s 14.5s  64s
catalan2m: 0.7s 7.0s 64.9s 644s

```



## PicoLisp


```PicoLisp
(de bino (N K)
   (let f
      '((N)
         (if (=0 N) 1 (apply * (range 1 N))) )
      (/
         (f N)
         (* (f (- N K)) (f K)) ) ) )

(for N 15
  (println
     (-
        (bino (* 2 N) N)
        (bino (* 2 N) (inc N)) ) ) )
(bye)
```



## PureBasic

{{trans|C}}

```PureBasic
#MAXNUM = 15
Declare catalan()

If OpenConsole("Catalan numbers")
  catalan()
  Input()
  End 0
Else
  End -1
EndIf

Procedure catalan()
  Define k.i, n.i, num.d, den.d, cat.d

  Print("1 ")

  For n=2 To #MAXNUM
    num=1 : den =1
    For k=2 To n
      num * (n+k)
      den * k
      cat = num / den
    Next
    Print(Str(cat)+" ")
  Next
EndProcedure
```

{{out}}

```txt

1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
```



## Python


### Procedural

{{trans|C++}}

```python
>>
 n = 15
>>> t = [0] * (n + 2)
>>> t[1] = 1
>>> for i in range(1, n + 1):
	for j in range(i, 1, -1): t[j] += t[j - 1]
	t[i + 1] = t[i]
	for j in range(i + 1, 1, -1): t[j] += t[j - 1]
	print(t[i+1] - t[i], end=' ')


1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
>>>
```


{{Works with|Python|2.7}}

```python
def catalan_number(n):
    nm = dm = 1
    for k in range(2, n+1):
      nm, dm = ( nm*(n+k), dm*k )
    return nm/dm

print [catalan_number(n) for n in range(1, 16)]

[1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845]
```



### Composition of pure functions

Note that sequence [http://oeis.org/A000108 A000108]  on OEIS (referenced in the task description) confirms that the first four Catalan numbers are indeed 1, 1, 2, 5 ...

(Several scripts on this page appear to lose the first 1).

{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Catalan numbers from Pascal's triangle'''

from itertools import (islice)
from operator import (add)


# nCatalans :: Int -> [Int]
def nCatalans(n):
    '''The first n Catalan numbers,
       derived from Pascal's triangle.'''

    # diff :: [Int] -> Int
    def diff(xs):
        '''Difference between the first two items in the list,
           if its length is more than one.
           Otherwise, the first (only) item in the list.'''
        return (
            xs[0] - (xs[1] if 1 < len(xs) else 0)
        ) if xs else None
    return list(map(
        compose(diff)(uncurry(drop)),
        enumerate(map(fst, take(n)(
            everyOther(
                pascalTriangle()
            )
        )))
    ))


# pascalTriangle :: Gen [[Int]]
def pascalTriangle():
    '''A non-finite stream of
       Pascal's triangle rows.'''
    return iterate(nextPascal)([1])


# nextPascal :: [Int] -> [Int]
def nextPascal(xs):
    '''A row of Pascal's triangle
       derived from a preceding row.'''
    return zipWith(add)([0] + xs)(xs + [0])


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''First 16 Catalan numbers.'''

    print(
        nCatalans(16)
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# drop :: Int -> [a] -> [a]
# drop :: Int -> String -> String
def drop(n):
    '''The sublist of xs beginning at
       (zero-based) index n.'''
    def go(xs):
        if isinstance(xs, list):
            return xs[n:]
        else:
            take(n)(xs)
            return xs
    return lambda xs: go(xs)


# everyOther :: Gen [a] -> Gen [a]
def everyOther(g):
    '''Every other item of a generator stream.'''
    while True:
        yield take(1)(g)
        take(1)(g)      # Consumed, not yielded.


# fst :: (a, b) -> a
def fst(tpl):
    '''First component of a pair.'''
    return tpl[0]


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated applications of f to x.'''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


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


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    '''A function over a tuple
       derived from a curried function.'''
    return lambda xy: f(xy[0])(
        xy[1]
    )


# zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
def zipWith(f):
    '''A list constructed by zipping with a
       custom function, rather than with the
       default tuple constructor.'''
    return lambda xs: lambda ys: (
        list(map(f, xs, ys))
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
[1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845]
```



## Racket


```Racket

#lang racket

(define (next-half-row r)
  (define r1 (for/list ([x r] [y (cdr r)]) (+ x y)))
  `(,(* 2 (car r1)) ,@(for/list ([x r1] [y (cdr r1)]) (+ x y)) 1 0))

(let loop ([n 15] [r '(1 0)])
  (cons (- (car r) (cadr r))
        (if (zero? n) '() (loop (sub1 n) (next-half-row r)))))
;; -> '(1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900
;;      2674440 9694845)

```



## REXX


### explicit subscripts

All of the REXX program examples can handle arbitrary large numbers.

```rexx
/*REXX program  obtains and displays  Catalan numbers  from  a  Pascal's triangle.      */
parse arg N .                                    /*Obtain the optional argument from CL.*/
if N=='' | N==","  then N=15                     /*Not specified?  Then use the default.*/
numeric digits max(9, N%2 + N%8)                 /*so we can handle huge Catalan numbers*/
@.=0;   @.1=1                                    /*stem array default; define 1st value.*/

  do i=1  for N;                               ip=i+1
                      do j=i   by -1  for N;   jm=j-1;   @.j=@.j+@.jm;    end /*j*/
  @.ip=@.i;           do k=ip  by -1  for N;   km=k-1;   @.k=@.k+@.km;    end /*k*/
  say  @.ip - @.i                                /*display the   Ith   Catalan number.  */
  end   /*i*/                                    /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default input:

```txt

1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
9694845

```



### implicit subscripts


```rexx
/*REXX program  obtains and displays  Catalan numbers  from  a  Pascal's triangle.      */
parse arg N .                                    /*Obtain the optional argument from CL.*/
if N=='' | N==","  then N=15                     /*Not specified?  Then use the default.*/
numeric digits max(9, N%2 + N%8)                 /*so we can handle huge Catalan numbers*/
@.=0;  @.1=1                                     /*stem array default; define 1st value.*/
               do i=1  for N;  ip=i+1
                                      do j=i   by -1  for N;  @.j=@.j+@(j-1);   end  /*j*/
               @.ip=@.i;              do k=ip  by -1  for N;  @.k=@.k+@(k-1);   end  /*k*/
               say  @.ip - @.i                   /*display the   Ith   Catalan number.  */
               end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
@:  parse arg !;   return @.!                    /*return the value of   @.[arg(1)]     */
```

'''output'''   is the same as the 1<sup>st</sup> version.


### using binomial coefficients


```rexx
/*REXX program  obtains and displays  Catalan numbers  from  a  Pascal's triangle.      */
parse arg N .                                    /*Obtain the optional argument from CL.*/
if N=='' | N==","  then N=15                     /*Not specified?  Then use the default.*/
numeric digits max(9, N%2 + N%8)                 /*so we can handle huge Catalan numbers*/
                      do j=1  for N              /* [↓]  display   N   Catalan numbers. */
                      say  comb(j+j, j) % (j+1)  /*display the   Jth   Catalan number.  */
                      end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
!:    procedure; parse arg z;   _=1;     do j=1  for arg(1);  _=_*j;  end;        return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
comb: procedure; parse arg x,y;        if x=y  then return 1;   if y>x  then return 0
      if x-y<y  then y=x-y;     _=1;   do j=x-y+1  to x;  _=_*j;  end;       return _/!(y)
```

'''output'''   is the same as the 1<sup>st</sup> version.

===binomial coefficients, memoized===
This REXX version uses memoization for the calculation of factorials.

```rexx
/*REXX program  obtains and displays  Catalan numbers  from  a  Pascal's triangle.      */
parse arg N .                                    /*Obtain the optional argument from CL.*/
if N=='' | N==","  then N=15                     /*Not specified?  Then use the default.*/
numeric digits max(9, N%2 + N%8)                 /*so we can handle huge Catalan numbers*/
!.=.
                      do j=1  for N              /* [↓]  display   N   Catalan numbers. */
                      say  comb(j+j, j) % (j+1)  /*display the   Jth   Catalan number.  */
                      end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
!:    procedure expose !.;  parse arg z;     if !.z\==. then return !.z;  _=1
                         do j=1  for arg(1);   _=_*j;   end;        !.z=_;   return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
comb: procedure expose !.;  parse arg x,y;   if x=y  then return 1;  if y>x  then return 0
      if x-y<y  then y=x-y;     _=1;   do j=x-y+1  to x;  _=_*j;  end;       return _/!(y)
```

'''output'''   is the same as the 1<sup>st</sup> version.




## Ring


```ring

n=15
cat = list(n+2)
cat[1]=1
for i=1 to n
    for j=i+1 to 2 step -1
        cat[j]=cat[j]+cat[j-1]
    next
    cat[i+1]=cat[i]
    for j=i+2 to 2 step -1
        cat[j]=cat[j]+cat[j-1]
    next
    see "" + (cat[i+1]-cat[i]) + " "
next

```

Output:

```txt

1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```



## Ruby


```tcl
def catalan(num)
  t = [0, 1] #grows as needed
  (1..num).map do |i|
    i.downto(1){|j| t[j] += t[j-1]}
    t[i+1] = t[i]
    (i+1).downto(1) {|j| t[j] += t[j-1]}
    t[i+1] - t[i]
  end
end

p catalan(15)
```

{{out}}

```txt

[1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845]

```



## Run BASIC


```runbasic
n = 15
dim t(n+2)
t(1) = 1
for i = 1 to n
  for  j = i to 1 step -1  : t(j) = t(j) + t(j-1): next j
  t(i+1) = t(i)
  for  j = i+1 to 1 step -1: t(j) = t(j) + t(j-1 : next j
print t(i+1) - t(i);" ";
next i
```

{{out}}

```txt
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
```



## Rust


```rust


fn main()
{let n=15usize;
 let mut t= [0; 17];
 t[1]=1;
 let mut j:usize;
 for i in 1..n+1
 {
	j=i;
	loop{
	    if j==1{
		      break;
		}
		t[j]=t[j] + t[j-1];
		j=j-1;
	}
	t[i+1]= t[i];
	j=i+1;
	loop{
		if j==1{
		break;
		}
		t[j]=t[j] + t[j-1];
		j=j-1;
	}
	print!("{} ", t[i+1]-t[i]);
 }
}

```

{{out}}

```txt
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
```



## Scala


```Scala
def catalan(n: Int): Int =
  if (n <= 1) 1
  else (0 until n).map(i => catalan(i) * catalan(n - i - 1)).sum

(1 to 15).map(catalan(_))
```

{{Out}}See it in running in your browser by [https://scastie.scala-lang.org/2ybpRZxCTOyrx3mIy8yIDw Scastie (JVM)].

## Scilab

<lang>n=15
t=zeros(1,n+2)
t(1)=1
for i=1:n
  for j=i+1:-1:2
    t(j)=t(j)+t(j-1)
  end
  t(i+1)=t(i)
  for j=i+2:-1:2
    t(j)=t(j)+t(j-1)
  end
  disp(t(i+1)-t(i))
end
```

{{out}}

```txt
    1.
    2.
    5.
    14.
    42.
    132.
    429.
    1430.
    4862.
    16796.
    58786.
    208012.
    742900.
    2674440.
    9694845.
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const integer: N is 15;
    var array integer: t is [] (1) & N times 0;
    var integer: i is 0;
    var integer: j is 0;
  begin
    for i range 1 to N do
      for j range i downto 2 do
        t[j] +:= t[j - 1];
      end for;
      t[i + 1] := t[i];
      for j range i + 1 downto 2 do
        t[j] +:= t[j - 1];
      end for;
      write(t[i + 1] - t[i] <& " ");
    end for;
    writeln;
  end func;
```


{{out}}

```txt

1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845

```



## Sidef

{{trans|Ruby}}

```ruby
func catalan(num) {
  var t = [0, 1]
  (1..num).map { |i|
    flip(^i    ).each {|j| t[j+1] += t[j] }
    t[i+1] = t[i]
    flip(^i.inc).each {|j| t[j+1] += t[j] }
    t[i+1] - t[i]
  }
}

say catalan(15).join(' ')
```

{{out}}

```txt
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
```



## smart BASIC


```qbasic
PRINT "Catalan Numbers from Pascal's Triangle"!PRINT
x = 15
DIM t(x+2)
t(1) = 1
FOR n = 1 TO x
  FOR  m = n TO 1 STEP -1
    t(m) = t(m) + t(m-1)
  NEXT m
    t(n+1) = t(n)
  FOR  m = n+1 TO 1 STEP -1
    t(m) = t(m) + t(m-1)
  NEXT m
PRINT n,"#######":t(n+1) - t(n)
NEXT n
```



## Tcl


```tcl
proc catalan n {
    set result {}
    array set t {0 0 1 1}
    for {set i 1} {[set k $i] <= $n} {incr i} {
	for {set j $i} {$j > 1} {} {incr t($j) $t([incr j -1])}
	set t([incr k]) $t($i)
	for {set j $k} {$j > 1} {} {incr t($j) $t([incr j -1])}
	lappend result [expr {$t($k) - $t($i)}]
    }
    return $result
}

puts [catalan 15]
```

{{out}}

```txt
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
```


=={{header|TI-83 BASIC}}==

```ti83b
"CATALAN
15→N
seq(0,I,1,N+2)→L1
1→L1(1)
For(I,1,N)
For(J,I+1,2,-1)
L1(J)+L1(J-1)→L1(J)
End
L1(I)→L1(I+1)
For(J,I+2,2,-1)
L1(J)+L1(J-1)→L1(J)
End
Disp L1(I+1)-L1(I)
End
```

{{out}}

```txt
               1
               2
               5
              14
              42
             132
             429
            1430
            4862
           16796
           58786
          208012
          742900
         2674440
         9694845
            Done
```



## VBScript

To run in console mode with cscript.

```vbscript
dim t()
if Wscript.arguments.count=1 then
  n=Wscript.arguments.item(0)
else
  n=15
end if
redim t(n+1)
't(*)=0
t(1)=1
for i=1 to n
  ip=i+1
  for j = i to 1 step -1
    t(j)=t(j)+t(j-1)
  next 'j
  t(i+1)=t(i)
  for j = i+1 to 1 step -1
    t(j)=t(j)+t(j-1)
  next 'j
  Wscript.echo t(i+1)-t(i)
next 'i
```



## Visual Basic

{{trans|Rexx}}
{{works with|Visual Basic|VB6 Standard}}

```vb

Sub catalan()
    Const n = 15
    Dim t(n + 2) As Long
    Dim i  As Integer, j As Integer
    t(1) = 1
    For i = 1 To n
        For j = i + 1 To 2 Step -1
            t(j) = t(j) + t(j - 1)
        Next j
        t(i + 1) = t(i)
        For j = i + 2 To 2 Step -1
            t(j) = t(j) + t(j - 1)
        Next j
        Debug.Print i, t(i + 1) - t(i)
    Next i
End Sub 'catalan

```

{{Out}}

```txt
 1
 2
 5
 14
 42
 132
 429
 1430
 4862
 16796
 58786
 208012
 742900
 2674440
 9694845

```



## zkl

{{trans|PARI/GP}} using binomial coefficients.

```zkl
fcn binomial(n,k){ (1).reduce(k,fcn(p,i,n){ p*(n-i+1)/i },1,n) }
(1).pump(15,List,fcn(n){ binomial(2*n,n)-binomial(2*n,n+1) })
```

{{out}}

```txt

L(1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440,9694845)

```



## ZX Spectrum Basic

{{trans|C++}}

```zxbasic
10 LET N=15
20 DIM t(N+2)
30 LET t(2)=1
40 FOR i=2 TO N+1
50 FOR j=i TO 2 STEP -1: LET t(j)=t(j)+t(j-1): NEXT j
60 LET t(i+1)=t(i)
70 FOR j=i+1 TO 2 STEP -1: LET t(j)=t(j)+t(j-1): NEXT j
80 PRINT t(i+1)-t(i);" ";
90 NEXT i
```

