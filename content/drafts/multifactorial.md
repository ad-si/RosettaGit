+++
title = "Multifactorial"
description = ""
date = 2019-07-10T00:56:07Z
aliases = []
[extra]
id = 12576
[taxonomies]
categories = []
tags = []
+++

{{task}}

The factorial of a number, written as <math>n!</math>, is defined as <math>n! = n(n-1)(n-2)...(2)(1)</math>.

[http://mathworld.wolfram.com/Multifactorial.html Multifactorials] generalize factorials as follows:
: <math>n! = n(n-1)(n-2)...(2)(1)</math>
: <math>n!! = n(n-2)(n-4)...</math>
: <math>n!! ! = n(n-3)(n-6)...</math>
: <math>n!! !! = n(n-4)(n-8)...</math>
: <math>n!! !! ! = n(n-5)(n-10)...</math>

In all cases, the terms in the products are positive integers.

If we define the degree of the multifactorial as the difference in successive terms that are multiplied together for a multifactorial (the number of exclamation marks), then the task is twofold:
# Write a function that given n and the degree, calculates the multifactorial.
# Use the function to generate and display here a table of the first ten members (1 to 10) of the first five degrees of multifactorial.


'''Note:''' The [[wp:Factorial#Multifactorials|wikipedia entry on multifactorials]] gives a different formula. This task uses the [http://mathworld.wolfram.com/Multifactorial.html Wolfram mathworld definition].




## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set (S/360 1964 POP).

```360asm
*        Multifactorial            09/05/2016
MULFACR  CSECT
         USING MULFACR,13
SAVEAR   B     STM-SAVEAR(15)
         DC    17F'0'
STM      STM   14,12,12(13) prolog
         ST    13,4(15)     "
         ST    15,8(13)     "
         LR    13,15        "
         LA    I,1          i=1
LOOPI    C     I,D          do i=1 to deg
         BH    ELOOPI       leave i
         LA    L,W+4          l=@p
         LA    J,1            j=1
LOOPJ    C     J,N            do j=1 to num
         BH    ELOOPJ         leave j
         LA    R,1              r=1
         LCR   S,I              s=-i
         LR    K,J              k=j
LOOPK    C     K,=F'2'          do k=j to 2 by s
         BL    ELOOPK           leave k
         MR    RR,K               r=r*k
         AR    K,S                k=k+s
         B     LOOPK            next k
ELOOPK   CVD   R,Y              pack r
         MVC   X,=XL12'402020202020202020202120' ed mask
         ED    X,Y+2            edit r
         MVC   0(8,L),X+4       output r
         LA    L,8(L)           l=l+8
         LA    J,1(J)           j=j+1
         B     LOOPJ          next j
ELOOPJ   WTO   MF=(E,W)
         LA    I,1(I)         i=i+1
         B     LOOPI        next i
ELOOPI   L     13,4(0,13)   epilog
         LM    14,12,12(13) "
         XR    15,15        "
         BR    14           "
N        DC    F'10'        number
D        DC    F'5'         degree
W        DC    0F,H'84',H'0',CL80' ' length,zero,text
X        DS    CL12         temp
Y        DS    D            packed PL8
I        EQU   6
J        EQU   7
K        EQU   8
S        EQU   9
RR       EQU   10           even reg of R for MR opcode
R        EQU   11
L        EQU   12
         END   MULFACR
```

{{out}}

```txt

       1       2       6      24     120     720    5040   40320  362880 3628800
       1       2       3       8      15      48     105     384     945    3840
       1       2       3       4      10      18      28      80     162     280
       1       2       3       4       5      12      21      32      45     120
       1       2       3       4       5       6      14      24      36      50

```



## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;
procedure Mfact is

   function MultiFact (num : Natural; deg : Positive) return Natural is
      Result, N : Integer := num;
   begin
      if N = 0 then return 1; end if;
      loop
         N := N - deg; exit when N <= 0; Result := Result * N;
      end loop; return Result;
   end MultiFact;

begin
   for deg in 1..5 loop
      Put("Degree"& Integer'Image(deg) &":");
      for num in 1..10 loop Put(Integer'Image(MultiFact(num,deg))); end loop;
      New_line;
   end loop;
end Mfact;
```

{{out}}

```txt

Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50

```



## Aime


```aime
mf(integer a, n)
{
    integer o;

    o = 1;
    do {
        o *= a;
    } while (0 < (a -= n));

    o;
}

main(void)
{
    integer i, j;

    i = 0;
    while ((i += 1) <= 5) {
        o_("degree ", i, ":");
        j = 0;
        while ((j += 1) <= 10) {
            o_("\t", mf(j, i));
        }
        o_("\n");
    }

    0;
}
```

{{out}}

```txt
degree 1:       1       2       6       24      120     720     5040    40320  362880   3628800
degree 2:       1       2       3       8       15      48      105     384    945      3840
degree 3:       1       2       3       4       10      18      28      80     162      280
degree 4:       1       2       3       4       5       12      21      32     45       120
degree 5:       1       2       3       4       5       6       14      24     36       50
```



## ALGOL 68

Translation of C.

```Algol68
BEGIN
   INT highest degree = 5;
   INT largest number = 10;
CO Recursive implementation of multifactorial function CO
   PROC multi fact = (INT n, deg) INT :
   (n <= deg | n | n * multi fact(n - deg, deg));
CO Iterative implementation of multifactorial function CO
   PROC multi fact i = (INT n, deg) INT :
   BEGIN
      INT result := n, nn := n;
      WHILE (nn >= deg + 1) DO
	 result TIMESAB nn - deg;
	 nn MINUSAB deg
      OD;
      result
   END;
CO Print out multifactorials CO
   FOR i TO highest degree DO
      printf (($l, "Degree ", g(0), ":"$, i));
      FOR j TO largest number DO
	 printf (($xg(0)$, multi fact (j, i)))
      OD
   OD
END

```

{{out}}

```txt


Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50

```



## ALGOL W

Iterative multifactorial based on Ada, AutoHotkey, etc.

```algolw
begin
    % returns the multifactorial of n with the specified degree %
    integer procedure multifactorial ( integer value n, degree ) ;
        begin
            integer mf, v;
            mf := v := n;
            while begin
                      v := v - degree;
                      v > 1
            end do mf := mf * v;
            mf
        end multifactorial ;

    % tests as per task %
    for degree := 1 until 5 do begin
        i_w := 1; s_w := 0; % output formatting %
        write( "Degree: ", degree, ":" );
        for v := 1 until 10 do begin
            writeon( " ", multifactorial( v, degree ) )
        end for_v
    end for_degree
end.
```

{{out}}

```txt

Degree: 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree: 2: 1 2 3 8 15 48 105 384 945 3840
Degree: 3: 1 2 3 4 10 18 28 80 162 280
Degree: 4: 1 2 3 4 5 12 21 32 45 120
Degree: 5: 1 2 3 4 5 6 14 24 36 50

```



## ANSI Standard BASIC


Translation of FreeBASIC.


```ANSI Standard BASIC
100 FUNCTION multiFactorial (n, degree)
110    IF  n < 2 THEN
120       LET multiFactorial = 1
130       EXIT FUNCTION
140    END IF
150    LET result = n
160    FOR i = n - degree TO 2 STEP -degree
170       LET result = result * i
180    NEXT i
190    LET multiFactorial = result
200 END FUNCTION
210
220 FOR degree = 1 TO 5
230    PRINT "Degree"; degree; " => ";
240    FOR n = 1 TO 10
250       PRINT multiFactorial(n, degree); " ";
260    NEXT n
270    PRINT
280 NEXT degree
290 END
```



## AutoHotkey


```AutoHotkey
Loop, 5 {
    Output .= "Degree " (i := A_Index) ": "
    Loop, 10
        Output .= MultiFact(A_Index, i) (A_Index = 10 ? "`n" : ", ")
}
MsgBox, % Output

MultiFact(n, d) {
    Result := n
    while 1 < n -= d
        Result *= n
    return, Result
}
```

'''Output:'''

```txt
Degree 1: 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800
Degree 2: 1, 2, 3, 8, 15, 48, 105, 384, 945, 3840
Degree 3: 1, 2, 3, 4, 10, 18, 28, 80, 162, 280
Degree 4: 1, 2, 3, 4, 5, 12, 21, 32, 45, 120
Degree 5: 1, 2, 3, 4, 5, 6, 14, 24, 36, 50
```


## AWK


```AWK

# syntax: GAWK -f MULTIFACTORIAL.AWK
# converted from Go
BEGIN {
    for (k=1; k<=5; k++) {
      printf("degree %d:",k)
      for (n=1; n<=10; n++) {
        printf(" %d",multi_factorial(n,k))
      }
      printf("\n")
    }
    exit(0)
}
function multi_factorial(n,k,  r) {
    r = 1
    for (; n>1; n-=k) {
      r *= n
    }
    return(r)
}

```

{{out}}

```txt

degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
degree 2: 1 2 3 8 15 48 105 384 945 3840
degree 3: 1 2 3 4 10 18 28 80 162 280
degree 4: 1 2 3 4 5 12 21 32 45 120
degree 5: 1 2 3 4 5 6 14 24 36 50

```



## BBC BASIC


```bbcbasic>REM
multifact
FOR i% = 1 TO 5
  PRINT "Degree "; i%; ":";
  FOR j% = 1 TO 10
    PRINT " ";FNmultifact(j%, i%);
  NEXT
  PRINT
NEXT
END
:
DEF FNmultifact(n%, degree%)
LOCAL i%, mfact%
mfact% = 1
FOR i% = n% TO 1 STEP -degree%
  mfact% = mfact% * i%
NEXT
= mfact%
```

{{out}}

```txt
Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50
```



## C

{{uses from|Library|C Runtime|component1=printf}}

```c

/* Include statements and constant definitions */
#include <stdio.h>
#define HIGHEST_DEGREE 5
#define LARGEST_NUMBER 10

/* Recursive implementation of multifactorial function */
int multifact(int n, int deg){
   return n <= deg ? n : n * multifact(n - deg, deg);
}

/* Iterative implementation of multifactorial function */
int multifact_i(int n, int deg){
   int result = n;
   while (n >= deg + 1){
      result *= (n - deg);
      n -= deg;
   }
   return result;
}

/* Test function to print out multifactorials */
int main(void){
   int i, j;
   for (i = 1; i <= HIGHEST_DEGREE; i++){
      printf("\nDegree %d: ", i);
      for (j = 1; j <= LARGEST_NUMBER; j++){
         printf("%d ", multifact(j, i));
      }
   }
}

```

{{out}}

```txt

Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50

```



## C#


```c#
namespace RosettaCode.Multifactorial
{
    using System;
    using System.Linq;

    internal static class Program
    {
        private static void Main()
        {
            Console.WriteLine(string.Join(Environment.NewLine,
                                          Enumerable.Range(1, 5)
                                                    .Select(
                                                        degree =>
                                                        string.Join(" ",
                                                                    Enumerable.Range(1, 10)
                                                                              .Select(
                                                                                  number =>
                                                                                  Multifactorial(number, degree))))));
        }

        private static int Multifactorial(int number, int degree)
        {
            if (degree < 1)
            {
                throw new ArgumentOutOfRangeException("degree");
            }

            var count = 1 + (number - 1) / degree;
            if (count < 1)
            {
                throw new ArgumentOutOfRangeException("number");
            }

            return Enumerable.Range(0, count)
                             .Aggregate(1, (accumulator, index) => accumulator * (number - degree * index));
        }
    }
}
```

Output:

```txt
1 2 6 24 120 720 5040 40320 362880 3628800
1 2 3 8 15 48 105 384 945 3840
1 2 3 4 10 18 28 80 162 280
1 2 3 4 5 12 21 32 45 120
1 2 3 4 5 6 14 24 36 50
```



## C++


```cpp

#include <algorithm>
#include <iostream>
#include <iterator>
/*Generate multifactorials to 9

  Nigel_Galloway
  November 14th., 2012.
*/
int main(void) {
   for (int g = 1; g < 10; g++) {
     int v[11], n=0;
     generate_n(std::ostream_iterator<int>(std::cout, " "), 10, [&]{n++; return v[n]=(g<n)? v[n-g]*n : n;});
     std::cout << std::endl;
   }
   return 0;
}

```

{{out}}

```txt

1 2 6 24 120 720 5040 40320 362880 3628800
1 2 3 8 15 48 105 384 945 3840
1 2 3 4 10 18 28 80 162 280
1 2 3 4 5 12 21 32 45 120
1 2 3 4 5 6 14 24 36 50
1 2 3 4 5 6 7 16 27 40
1 2 3 4 5 6 7 8 18 30
1 2 3 4 5 6 7 8 9 20
1 2 3 4 5 6 7 8 9 10

```



## Clojure



```Clojure
(defn !! [m n]
  (->> (iterate #(- % m) n) (take-while pos?) (apply *)))

(doseq [m (range 1 6)]
  (prn m (map #(!! m %) (range 1 11))))
```


{{out}}


```txt
1 (1 2 6 24 120 720 5040 40320 362880 3628800)
2 (1 2 3 8 15 48 105 384 945 3840)
3 (1 2 3 4 10 18 28 80 162 280)
4 (1 2 3 4 5 12 21 32 45 120)
5 (1 2 3 4 5 6 14 24 36 50)
```



## Common Lisp


```lisp

(defun mfac (n m)
  (reduce #'* (loop for i from n downto 1 by m collect i)))

(loop for i from 1 to 10
      do (format t "~2@a: ~{~a~^ ~}~%"
                 i (loop for j from 1 to 10
                         collect (mfac j i))))

```

{{out}}

```txt

 1: 1 2 6 24 120 720 5040 40320 362880 3628800
 2: 1 2 3 8 15 48 105 384 945 3840
 3: 1 2 3 4 10 18 28 80 162 280
 4: 1 2 3 4 5 12 21 32 45 120
 5: 1 2 3 4 5 6 14 24 36 50
 6: 1 2 3 4 5 6 7 16 27 40
 7: 1 2 3 4 5 6 7 8 18 30
 8: 1 2 3 4 5 6 7 8 9 20
 9: 1 2 3 4 5 6 7 8 9 10
10: 1 2 3 4 5 6 7 8 9 10

```



## D


```d
import std.stdio, std.algorithm, std.range;

T multifactorial(T=long)(in int n, in int m) pure /*nothrow*/ {
    T one = 1;
    return reduce!q{a * b}(one, iota(n, 0, -m));
}

void main() {
    foreach (immutable m; 1 .. 11)
        writefln("%2d: %s", m, iota(1, 11)
                               .map!(n => multifactorial(n, m)));
}
```

{{out}}

```txt
 1: 1 2 6 24 120 720 5040 40320 362880 3628800
 2: 1 2 3 8 15 48 105 384 945 3840
 3: 1 2 3 4 10 18 28 80 162 280
 4: 1 2 3 4 5 12 21 32 45 120
 5: 1 2 3 4 5 6 14 24 36 50
 6: 1 2 3 4 5 6 7 16 27 40
 7: 1 2 3 4 5 6 7 8 18 30
 8: 1 2 3 4 5 6 7 8 9 20
 9: 1 2 3 4 5 6 7 8 9 10
10: 1 2 3 4 5 6 7 8 9 10
```


## Dart


```dart

main()
{
  int n=5,d=3;
int z= fact(n,d);
print('$n factorial of degree $d is $z');
for(var j=1;j<=5;j++)
{
  print('first 10 numbers of degree $j :');
  for(var i=1;i<=10;i++)
  {
    int z=fact(i,j);
 print('$z');
}
  print('\n');
}
}

int fact(int a,int b)
{

  if(a<=b||a==0)
    return a;
  if(a>1)
    return a*fact((a-b),b);
}

```


## Elixir

{{trans|Erlang}}

```elixir
defmodule RC do
  def multifactorial(n,d) do
    Enum.take_every(n..1, d) |> Enum.reduce(1, fn x,p -> x*p end)
  end
end

Enum.each(1..5, fn d ->
  multifac = for n <- 1..10, do: RC.multifactorial(n,d)
  IO.puts "Degree #{d}: #{inspect multifac}"
end)
```


{{out}}

```txt

Degree 1: [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]
Degree 2: [1, 2, 3, 8, 15, 48, 105, 384, 945, 3840]
Degree 3: [1, 2, 3, 4, 10, 18, 28, 80, 162, 280]
Degree 4: [1, 2, 3, 4, 5, 12, 21, 32, 45, 120]
Degree 5: [1, 2, 3, 4, 5, 6, 14, 24, 36, 50]

```



## Erlang


```erlang
-module(multifac).
-compile(export_all).

multifac(N,D) ->
    lists:foldl(fun (X,P) -> X * P end, 1, lists:seq(N,1,-D)).

main() ->
    Ds = lists:seq(1,5),
    Ns = lists:seq(1,10),
    lists:foreach(fun (D) ->
                          io:format("Degree ~b: ~p~n",[D, [ multifac(N,D) || N <- Ns]])
                  end, Ds).
```

{{out}}

```erlang>5
 multifac:main().
Degree 1: [1,2,6,24,120,720,5040,40320,362880,3628800]
Degree 2: [1,2,3,8,15,48,105,384,945,3840]
Degree 3: [1,2,3,4,10,18,28,80,162,280]
Degree 4: [1,2,3,4,5,12,21,32,45,120]
Degree 5: [1,2,3,4,5,6,14,24,36,50]
ok
```



## ERRE


```ERRE

PROGRAM MULTIFACTORIAL

PROCEDURE MULTI_FACT(NUM,DEG->MF)
   RESULT=NUM
   N=NUM
   IF N=0 THEN
      MF=1
      EXIT PROCEDURE
   END IF
   LOOP
      N-=DEG
      EXIT IF N<=0
      RESULT*=N
   END LOOP
   MF=RESULT
END PROCEDURE

BEGIN
  PRINT(CHR$(12);)
  FOR DEG=1 TO 10 DO
      PRINT("Degree";DEG;":";)
      FOR NUM=1 TO 10 DO
          MULTI_FACT(NUM,DEG->MF)
          PRINT(MF;)
      END FOR
      PRINT
  END FOR
END PROGRAM

```


```txt

Degree 1 : 1  2  6  24  120  720  5040  40320  362880  3628800
Degree 2 : 1  2  3  8  15  48  105  384  945  3840
Degree 3 : 1  2  3  4  10  18  28  80  162  280
Degree 4 : 1  2  3  4  5  12  21  32  45  120
Degree 5 : 1  2  3  4  5  6  14  24  36  50
Degree 6 : 1  2  3  4  5  6  7  16  27  40
Degree 7 : 1  2  3  4  5  6  7  8  18  30
Degree 8 : 1  2  3  4  5  6  7  8  9  20
Degree 9 : 1  2  3  4  5  6  7  8  9  10
Degree 10 : 1  2  3  4  5  6  7  8  9  10

```


=={{header|F_Sharp|F#}}==


```fsharp
let rec mfact d = function
    | n when n <= d   -> n
    | n -> n * mfact d (n-d)

[<EntryPoint>]
let main argv =
    let (|UInt|_|) = System.UInt32.TryParse >> function | true, v -> Some v | false, _ -> None
    let (maxDegree, maxN) =
        match argv with
            | [| UInt d; UInt n |] -> (int d, int n)
            | [| UInt d |]         -> (int d, 10)
            | _                    -> (5, 10)
    let showFor d = List.init maxN (fun i -> mfact d (i+1)) |> printfn "%i: %A" d
    ignore (List.init maxDegree (fun i -> showFor (i+1)))
    0

```


```txt
1: [1; 2; 6; 24; 120; 720; 5040; 40320; 362880; 3628800]
2: [1; 2; 3; 8; 15; 48; 105; 384; 945; 3840]
3: [1; 2; 3; 4; 10; 18; 28; 80; 162; 280]
4: [1; 2; 3; 4; 5; 12; 21; 32; 45; 120]
5: [1; 2; 3; 4; 5; 6; 14; 24; 36; 50]
```



## Factor

<lang>USING: formatting io kernel math math.ranges prettyprint
sequences ;
IN: rosetta-code.multifactorial

: multifactorial ( n degree -- m )
    neg 1 swap <range> product ;

: mf-row ( degree -- )
    dup "Degree %d: " printf
    10 [1,b] [ swap multifactorial pprint bl ] with each ;

: main ( -- )
    5 [1,b] [ mf-row nl ] each ;

MAIN: main
```

{{out}}

```txt

Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50

```



## Forth

<lang>: !n negate swap 1 dup rot do i * over +loop nip ;
: test cr 6 1 ?do 11 1 ?do i j !n . loop cr loop ;
```

{{out}}

```txt
test
1 2 6 24 120 720 5040 40320 362880 3628800
1 2 3 8 15 48 105 384 945 3840
1 2 3 4 10 18 28 80 162 280
1 2 3 4 5 12 21 32 45 120
1 2 3 4 5 6 14 24 36 50
 ok
```



## Fortran

{{works with|Fortran|95 and later}}

```fortran
program test
  implicit none
  integer :: i, j, n

  do i = 1, 5
    write(*, "(a, i0, a)", advance = "no") "Degree ", i, ": "
    do j = 1, 10
      n = multifactorial(j, i)
      write(*, "(i0, 1x)", advance = "no") n
    end do
    write(*,*)
  end do

contains

function multifactorial (range, degree)
  integer :: multifactorial, range, degree
  integer :: k

  multifactorial = product((/(k, k=range, 1, -degree)/))

end function multifactorial
end program test
```

{{out}}

```txt

Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function multiFactorial (n As UInteger, degree As Integer) As UInteger
  If  n < 2 Then Return 1
  Var result = n
  For i As Integer = n - degree To 2 Step -degree
    result *= i
  Next
  Return result
End Function

For degree As Integer = 1 To 5
  Print "Degree"; degree; " => ";
  For n As Integer = 1 To 10
    Print multiFactorial(n, degree); " ";
  Next n
  Print
Next degree

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Degree 1 => 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2 => 1 2 3 8 15 48 105 384 945 3840
Degree 3 => 1 2 3 4 10 18 28 80 162 280
Degree 4 => 1 2 3 4 5 12 21 32 45 120
Degree 5 => 1 2 3 4 5 6 14 24 36 50

```


## FunL


```funl
def multifactorial( n, d ) = product( n..1 by -d )

for d <- 1..5
  println( d, [multifactorial(i, d) | i <- 1..10] ))
```


{{out}}


```txt

1, [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]
2, [1, 2, 3, 8, 15, 48, 105, 384, 945, 3840]
3, [1, 2, 3, 4, 10, 18, 28, 80, 162, 280]
4, [1, 2, 3, 4, 5, 12, 21, 32, 45, 120]
5, [1, 2, 3, 4, 5, 6, 14, 24, 36, 50]

```



## GAP


```gap
MultiFactorial := function(n, k)
    local r;
    r := 1;
    while n > 1 do
        r := r*n;
        n := n - k;
    od;
    return r;
end;

PrintArray(List([1 .. 10], n -> List([1 .. 5], k -> MultiFactorial(n, k))));
[ [        1,        1,        1,        1,        1 ],
  [        2,        2,        2,        2,        2 ],
  [        6,        3,        3,        3,        3 ],
  [       24,        8,        4,        4,        4 ],
  [      120,       15,       10,        5,        5 ],
  [      720,       48,       18,       12,        6 ],
  [     5040,      105,       28,       21,       14 ],
  [    40320,      384,       80,       32,       24 ],
  [   362880,      945,      162,       45,       36 ],
  [  3628800,     3840,      280,      120,       50 ] ]
```



## Go


```go
package main

import "fmt"

func multiFactorial(n, k int) int {
    r := 1
    for ; n > 1; n -= k {
        r *= n
    }
    return r
}

func main() {
    for k := 1; k <= 5; k++ {
        fmt.Print("degree ", k, ":")
        for n := 1; n <= 10; n++ {
            fmt.Print(" ", multiFactorial(n, k))
        }
        fmt.Println()
    }
}
```

{{out}}

```txt

degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
degree 2: 1 2 3 8 15 48 105 384 945 3840
degree 3: 1 2 3 4 10 18 28 80 162 280
degree 4: 1 2 3 4 5 12 21 32 45 120
degree 5: 1 2 3 4 5 6 14 24 36 50

```



## Haskell


```haskell
mulfac k = 1:s where s = [1 .. k] ++ zipWith (*) s [k+1..]

-- for single n
mulfac1 k n = product [n, n-k .. 1]

main = mapM_ (print . take 10 . tail . mulfac) [1..5]
```


{{out}}

```txt

[1,2,6,24,120,720,5040,40320,362880,3628800]
[1,2,3,8,15,48,105,384,945,3840]
[1,2,3,4,10,18,28,80,162,280]
[1,2,3,4,5,12,21,32,45,120]
[1,2,3,4,5,6,14,24,36,50]

```


==Icon and {{header|Unicon}}==

The following is Unicon specific but can be readily translated into Icon:

```unicon
procedure main(A)
    l := integer(A[1]) | 10
    every writeRow(n := !l, [: mf(!10,n) :])
end

procedure writeRow(n, r)
    writes(right(n,3),": ")
    every writes(right(!r,8)|"\n")
end

procedure mf(n, m)
    if n <= 0 then return 1
    return n*mf(n-m, m)
end
```


Sample run:

```txt

->mf 5
  1:        1       2       6      24     120     720    5040   40320  362880 3628800
  2:        1       2       3       8      15      48     105     384     945    3840
  3:        1       2       3       4      10      18      28      80     162     280
  4:        1       2       3       4       5      12      21      32      45     120
  5:        1       2       3       4       5       6      14      24      36      50
->

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Multifac.bas"
110 FOR I=1 TO 5
120   PRINT "Degree";I;":";
130   FOR N=1 TO 10
140     PRINT MFACT(N,I);
150   NEXT
160   PRINT
170 NEXT
180 DEF MFACT(N,D)
190   NUMERIC I,RES
200   IF N<2 THEN LET MFACT=1:EXIT DEF
210   LET RES=N
220   FOR I=N-D TO 2 STEP-D
230     LET RES=RES*I
240   NEXT
250   LET MFACT=RES
260 END DEF
```



## J


```J

   NB. tacit implementation of the recursive c function
   NB. int multifact(int n,int deg){return n<=deg?n:n*multifact(n-deg,deg);}

   multifact=: [`([ * - $: ])@.(<~)
   (a:,<'       degree'),multifact table >:i.10
┌─────────┬──────────────────────────────────────┐
│         │       degree                         │
├─────────┼──────────────────────────────────────┤
│multifact│      1    2   3   4  5  6  7  8  9 10│
├─────────┼──────────────────────────────────────┤
│ 1       │      1    1   1   1  1  1  1  1  1  1│
│ 2       │      2    2   2   2  2  2  2  2  2  2│
│ 3       │      6    3   3   3  3  3  3  3  3  3│
│ 4       │     24    8   4   4  4  4  4  4  4  4│
│ 5       │    120   15  10   5  5  5  5  5  5  5│
│ 6       │    720   48  18  12  6  6  6  6  6  6│
│ 7       │   5040  105  28  21 14  7  7  7  7  7│
│ 8       │  40320  384  80  32 24 16  8  8  8  8│
│ 9       │ 362880  945 162  45 36 27 18  9  9  9│
│10       │3628800 3840 280 120 50 40 30 20 10 10│
└─────────┴──────────────────────────────────────┘

```



## Java


```java
public class MultiFact {
	private static long multiFact(long n, int deg){
		long ans = 1;
		for(long i = n; i > 0; i -= deg){
			ans *= i;
		}
		return ans;
	}

	public static void main(String[] args){
		for(int deg = 1; deg <= 5; deg++){
			System.out.print("degree " + deg + ":");
			for(long n = 1; n <= 10; n++){
				System.out.print(" " + multiFact(n, deg));
			}
			System.out.println();
		}
	}
}
```

{{out}}

```txt
degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
degree 2: 1 2 3 8 15 48 105 384 945 3840
degree 3: 1 2 3 4 10 18 28 80 162 280
degree 4: 1 2 3 4 5 12 21 32 45 120
degree 5: 1 2 3 4 5 6 14 24 36 50
```



## JavaScript



### Iterative

{{trans|C}}

```JavaScript

function multifact(n, deg){
	var result = n;
	while (n >= deg + 1){
		result *= (n - deg);
		n -= deg;
	}
	return result;
}

```



```JavaScript

function test (n, deg) {
	for (var i = 1; i <= deg; i ++) {
		var results = '';
		for (var j = 1; j <= n; j ++) {
			results += multifact(j, i) + ' ';
		}
		console.log('Degree ' + i + ': ' + results);
	}
}

```


{{out}}

```JavaScript

test(10, 5)
Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50

```



### Recursive


{{trans|C}}

```JavaScript
function multifact(n, deg){
    return n <= deg ? n : n * multifact(n - deg, deg);
}
```


Test

```JavaScript
function test (n, deg) {
    for (var i = 1; i <= deg; i ++) {
        var results = '';
        for (var j = 1; j <= n; j ++) {
            results += multifact(j, i) + ' ';
        }
        console.log('Degree ' + i + ': ' + results);
    }
}
```

{{Out}}

```JavaScript

test(10, 5)
Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50
```



## jq

{{works with|jq|1.4}}

```jq
# Input: n
# Output: n * (n - d) * (n - 2d) ...
def multifactorial(d):
  . as $n
  | ($n / d | floor) as $k
  | reduce ($n - (d * range(0; $k))) as $i (1; . * $i);
```



```jq
# Print out a d-by-n table of multifactorials neatly:
def table(d; n):
  def lpad(i): tostring | (i - length) * " " + .;
  def pp(stream): reduce stream as $i (""; . + ($i | lpad(8)));

  range(1; d+1) as $d | "Degree \($d): \( pp(range(1; n+1) | multifactorial($d)) )";
```

The specific task:

```jq
table(5; 10)
```

{{out}}

```sh
$ jq -n -r -f Multifactorial.jq
Degree 1:        1       2       6      24     120     720    5040   40320  362880 3628800
Degree 2:        1       2       3       8      15      48     105     384     945    3840
Degree 3:        1       1       3       4       5      18      28      40     162     280
Degree 4:        1       1       1       4       5       6       7      32      45      60
Degree 5:        1       1       1       1       5       6       7       8       9      50
```



## Julia

{{works with|Julia|0.6}}


```julia
function multifact(n::Integer, k::Integer)
    n > 0 && k > 0 || throw(DomainError())
    k > 1 || factorial(n)
    return prod(n:-k:2)
end

const khi = 5
const nhi = 10
println("Showing multifactorial for n in [1, $nhi] and k in [1, $khi].")
for k = 1:khi
    a = multifact.(1:nhi, k)
    lab = "n" * "!" ^ k
    @printf("  %-6s →  %s\n", lab, a)
end
```


{{out}}

```txt
Showing multifactorial for n in [1, 10] and k in [1, 5].
  n!     →  [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]
  n!!    →  [1, 2, 3, 8, 15, 48, 105, 384, 945, 3840]
  n!!!   →  [1, 2, 3, 4, 10, 18, 28, 80, 162, 280]
  n!!!!  →  [1, 2, 3, 4, 5, 12, 21, 32, 45, 120]
  n!!!!! →  [1, 2, 3, 4, 5, 6, 14, 24, 36, 50]
```



## Kotlin


```scala
fun multifactorial(n: Long, d: Int) : Long {
    val r = n % d
    return (1..n).filter { it % d == r } .reduce { i, p -> i * p }
}

fun main(args: Array<String>) {
    val m = 5
    val r = 1..10L
    for (d in 1..m) {
        print("%${m}s:".format( "!".repeat(d)))
        r.forEach { print(" " + multifactorial(it, d)) }
        println()
    }
}
```

{{Out}}

```txt
    !: 1 2 6 24 120 720 5040 40320 362880 3628800
   !!: 1 2 3 8 15 48 105 384 945 3840
  !!!: 1 2 3 4 10 18 28 80 162 280
 !!!!: 1 2 3 4 5 12 21 32 45 120
!!!!!: 1 2 3 4 5 6 14 24 36 50
```



## Lua


```Lua
function multiFact (n, degree)
    local fact = 1
    for i = n, 2, -degree do
        fact = fact * i
    end
    return fact
end

print("Degree\t|\tMultifactorials 1 to 10")
print(string.rep("-", 52))
for d = 1, 5 do
    io.write(" " .. d, "\t| ")
    for n = 1, 10 do
        io.write(multiFact(n, d) .. " ")
    end
    print()
end
```

{{out}}

```txt
Degree  |       Multifactorials 1 to 10
----------------------------------------------------
 1      | 1 2 6 24 120 720 5040 40320 362880 3628800
 2      | 1 2 3 8 15 48 105 384 945 3840
 3      | 1 2 3 4 10 18 28 80 162 280
 4      | 1 2 3 4 5 12 21 32 45 120
 5      | 1 2 3 4 5 6 14 24 36 50
```



## Maple

{{output?|Maple}}

```Maple

f := proc (n, m)
	local fac, i;
	fac := 1;
	for i from n by -m to 1 do
		fac := fac*i;
	end do;
	return fac;
end proc:

a:=Matrix(5,10):
for i from 1 to 5 do
	for j from 1 to 10 do
		a[i,j]:=f(j,i);
	end do;
end do;
a;

```



## Mathematica


```mathematica
Multifactorial[n_, m_] := Abs[ Apply[ Times, Range[-n, -1, m]]]
Table[ Multifactorial[j, i], {i, 5}, {j, 10}] // TableForm
```

{{out}}

```txt
1: 1 2 6 24 120 720 5040 40320 362880 3628800
2: 1 2 3 8 15 48 105 384 945 3840
3: 1 2 3 4 10 18 28 80 162 280
4: 1 2 3 4 5 12 21 32 45 120
5: 1 2 3 4 5 6 14 24 36 50
```



## min

{{works with|min|0.19.3}}

```min
(:d (dup 0 <=) (pop 1) (dup d -) (*) linrec) :multifactorial
(:d 1 (dup d multifactorial print! " " print! succ) 10 times newline pop) :row

1 (dup "Degree " print! print ": " print! row succ) 5 times
```

{{out}}

```txt

Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50

```


=={{header|МК-61/52}}==
<lang>П1	<->	П0	П2	ИП0	ИП1	1	+	-	x>=0
23	ИП2	ИП0	ИП1	-	*	П2	ИП0	ИП1	-
П1	БП	04	ИП2	С/П
```


Instruction: ''number'' ^ ''degree'' В/О С/П


## Nim


```nim
# Recursive
proc multifact(n, deg): int =
  result = (if n <= deg: n else: n * multifact(n - deg, deg))

# Iterative
proc multifactI(n, deg): int =
  result = n
  var n = n
  while n >= deg + 1:
    result *= n - deg
    n -= deg

for i in 1..5:
  stdout.write "\nDegree ", i, ": "
  for j in 1..10:
    stdout.write multifactI(j, i), " "
```

Output:

```txt
Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50
```



## Objeck

{{trans|C}}

```objeck

class Multifact {
   function : MultiFact(n : Int, deg : Int) ~ Int {
      result := n;
      while (n >= deg + 1){
         result *= (n - deg);
         n -= deg;
      };

      return result;
   }

   function : Main(args : String[]) ~ Nil {
      for (i := 1; i <= 5; i+=1;){
         IO.Console->Print("Degree ")->Print(i)->Print(": ");
         for (j := 1; j <= 10; j+=1;){
            IO.Console->Print(' ')->Print(MultiFact(j, i));
         };
         IO.Console->PrintLine();
      };
   }
}

```


Output:

```txt

Degree 1:  1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2:  1 2 3 8 15 48 105 384 945 3840
Degree 3:  1 2 3 4 10 18 28 80 162 280
Degree 4:  1 2 3 4 5 12 21 32 45 120
Degree 5:  1 2 3 4 5 6 14 24 36 50

```




## Oforth



```Oforth
: multifact(n, deg)  1 while( n 0 > ) [ n * n deg - ->n ] ;

: printMulti
| i |
   5 loop: i [ System.Out i << " : " << 10 seq map(#[ i multifact]) << cr ] ;
```


{{out}}

```txt

1 : [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]
2 : [1, 2, 3, 8, 15, 48, 105, 384, 945, 3840]
3 : [1, 2, 3, 4, 10, 18, 28, 80, 162, 280]
4 : [1, 2, 3, 4, 5, 12, 21, 32, 45, 120]
5 : [1, 2, 3, 4, 5, 6, 14, 24, 36, 50]

```



## PARI/GP


```parigp
fac(n,d)=prod(k=0,(n-1)\d,n-k*d)
for(k=1,5,for(n=1,10,print1(fac(n,k)" "));print)
```


```txt
1 2 6 24 120 720 5040 40320 362880 3628800
1 2 3 8 15 48 105 384 945 3840
1 2 3 4 10 18 28 80 162 280
1 2 3 4 5 12 21 32 45 120
1 2 3 4 5 6 14 24 36 50
```



## Perl


```perl
{ # <-- scoping the cache and bigint clause
	my @cache;
	use bigint;
	sub mfact {
		my ($s, $n) = @_;
		return 1 if $n <= 0;
		$cache[$s][$n] //= $n * mfact($s, $n - $s);
	}
}

for my $s (1 .. 10) {
	print "step=$s: ";
	print join(" ", map(mfact($s, $_), 1 .. 10)), "\n";
}
```

{{out}}

```txt

step=1: 1 2 6 24 120 720 5040 40320 362880 3628800
step=2: 1 2 3 8 15 48 105 384 945 3840
step=3: 1 2 3 4 10 18 28 80 162 280
step=4: 1 2 3 4 5 12 21 32 45 120
step=5: 1 2 3 4 5 6 14 24 36 50
step=6: 1 2 3 4 5 6 7 16 27 40
step=7: 1 2 3 4 5 6 7 8 18 30
step=8: 1 2 3 4 5 6 7 8 9 20
step=9: 1 2 3 4 5 6 7 8 9 10
step=10: 1 2 3 4 5 6 7 8 9 10

```


We can also do this iteratively.  ntheory's vecprod makes bigint products if needed, so we don't have to worry about it.
{{libheader|ntheory}}

```perl
use ntheory qw/vecprod/;

sub mfac {
  my($n,$d) = @_;
  vecprod(map { $n - $_*$d } 0 .. int(($n-1)/$d));
}

for my $degree (1..5) {
  say "$degree: ",join(" ",map{mfac($_,$degree)} 1..10);
}
```

{{out}}

```txt
1: 1 2 6 24 120 720 5040 40320 362880 3628800
2: 1 2 3 8 15 48 105 384 945 3840
3: 1 2 3 4 10 18 28 80 162 280
4: 1 2 3 4 5 12 21 32 45 120
5: 1 2 3 4 5 6 14 24 36 50
```



## Perl 6


```perl6
for 1 .. 5 -> $degree {
    sub mfact($n) { [*] $n, *-$degree ...^ * <= 0 };
    say "$degree: ", map &mfact, 1..10
}
```

{{out}}

```txt
1: 1 2 6 24 120 720 5040 40320 362880 3628800
2: 1 2 3 8 15 48 105 384 945 3840
3: 1 2 3 4 10 18 28 80 162 280
4: 1 2 3 4 5 12 21 32 45 120
5: 1 2 3 4 5 6 14 24 36 50
```



## Phix


```Phix
function multifactorial(integer n, integer order)
atom res = 1
    if n>0 then
        res = n*multifactorial(n-order,order)
    end if
    return res
end function

sequence s = repeat(0,10)
for i=1 to 5 do
    for j=1 to 10 do
        s[j] = multifactorial(j,i)
    end for
    ?s
end for
```

{{out}}

```txt

{1,2,6,24,120,720,5040,40320,362880,3628800}
{1,2,3,8,15,48,105,384,945,3840}
{1,2,3,4,10,18,28,80,162,280}
{1,2,3,4,5,12,21,32,45,120}
{1,2,3,4,5,6,14,24,36,50}

```



## PicoLisp

{{trans|C}}

```PicoLisp
(de multifact (N Deg)
   (let Res N
      (while (> N Deg)
         (setq Res (* Res (dec 'N Deg))) )
      Res ) )

(for I 5
   (prin "Degree " I ":")
   (for J 10
      (prin " " (multifact J I)) )
   (prinl) )
```

Output:

```txt
Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50
```



## PL/I

<lang>
multi: procedure options (main);      /* 29 October 2013 */
   declare (i, j, n) fixed binary;
   declare text character (6) static initial ('n!!!!!');

   do i = 1 to 5;
      put skip edit (substr(text, 1, i+1), '=' ) (A, COLUMN(8));
      do n = 1 to 10;
         put edit ( trim( multifactorial(n,i) ) ) (X(1), A);
      end;
   end;

multifactorial: procedure (n, j) returns (fixed(15));
   declare (n, j) fixed binary;
   declare f fixed (15), m fixed(15);

      f, m = n;
      do while (m > j); f = f * (m-fixed(j)); m = m - j; end;
      return (f);
end multifactorial;

end multi;

```

Output:

```txt

n!     = 1 2 6 24 120 720 5040 40320 362880 3628800
n!!    = 1 2 3 8 15 48 105 384 945 3840
n!!!   = 1 2 3 4 10 18 28 80 162 280
n!!!!  = 1 2 3 4 5 12 21 32 45 120
n!!!!! = 1 2 3 4 5 6 14 24 36 50

```




## plainTeX

Works with an etex engine.


```tex
\long\def\antefi#1#2\fi{#2\fi#1}
\def\fornum#1=#2to#3(#4){%
	\edef#1{\number\numexpr#2}\edef\fornumtemp{\noexpand\fornumi\expandafter\noexpand\csname fornum\string#1\endcsname
		{\number\numexpr#3}{\ifnum\numexpr#4<0 <\else>\fi}{\number\numexpr#4}\noexpand#1}\fornumtemp
}
\long\def\fornumi#1#2#3#4#5#6{\def#1{\unless\ifnum#5#3#2\relax\antefi{#6\edef#5{\number\numexpr#5+(#4)\relax}#1}\fi}#1}
\newcount\result
\def\multifact#1#2{%
	\result=1
	\fornum\multifactiter=#1 to 1(-#2){\multiply\result\multifactiter}%
	\number\result
}
\fornum\degree=1 to 5(+1){Degree \degree: \fornum\ii=1 to 10(+1){\multifact\ii\degree\space\space}\par}
\bye
```


Output pdf looks like:

```txt
Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50
```



## Python


### Python: Iterative


```python>>>
 from functools import reduce
>>> from operator import mul
>>> def mfac(n, m): return reduce(mul, range(n, 0, -m))

>>> for m in range(1, 11): print("%2i: %r" % (m, [mfac(n, m) for n in range(1, 11)]))

 1: [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]
 2: [1, 2, 3, 8, 15, 48, 105, 384, 945, 3840]
 3: [1, 2, 3, 4, 10, 18, 28, 80, 162, 280]
 4: [1, 2, 3, 4, 5, 12, 21, 32, 45, 120]
 5: [1, 2, 3, 4, 5, 6, 14, 24, 36, 50]
 6: [1, 2, 3, 4, 5, 6, 7, 16, 27, 40]
 7: [1, 2, 3, 4, 5, 6, 7, 8, 18, 30]
 8: [1, 2, 3, 4, 5, 6, 7, 8, 9, 20]
 9: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
10: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
>>>
```



### Python: Recursive


```python>>>
 def mfac2(n, m): return n if n <= (m + 1) else n * mfac2(n - m, m)

>>> for m in range(1, 6): print("%2i: %r" % (m, [mfac2(n, m) for n in range(1, 11)]))

 1: [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]
 2: [1, 2, 3, 8, 15, 48, 105, 384, 945, 3840]
 3: [1, 2, 3, 4, 10, 18, 28, 80, 162, 280]
 4: [1, 2, 3, 4, 5, 12, 21, 32, 45, 120]
 5: [1, 2, 3, 4, 5, 6, 14, 24, 36, 50]
>>>
```



## R


```R

#x is Input
#n is Factorial Number
multifactorial=function(x,n){
  if(x<=n+1){
    return(x)
  }else{
    return(x*multifactorial(x-n,n))
  }
}

```



## Racket


```racket
#lang racket

(define (multi-factorial-fn m)
  (lambda (n)
    (let inner ((acc 1) (n n))
      (if (<= n m) (* acc n)
          (inner (* acc n) (- n m))))))

;; using (multi-factorial-fn m) as a first-class function
(for*/list ([m (in-range 1 (add1 5))] [mf-m (in-value (multi-factorial-fn m))])
  (for/list ([n (in-range 1 (add1 10))])
  (mf-m n)))

(define (multi-factorial m n) ((multi-factorial-fn m) n))

(for/list ([m (in-range 1 (add1 5))])
  (for/list ([n (in-range 1 (add1 10))])
  (multi-factorial m n)))
```

Output:

```txt
'((1 2 6 24 120 720 5040 40320 362880 3628800)
  (1 2 3 8 15 48 105 384 945 3840)
  (1 2 3 4 10 18 28 80 162 280)
  (1 2 3 4 5 12 21 32 45 120)
  (1 2 3 4 5 6 14 24 36 50))
'((1 2 6 24 120 720 5040 40320 362880 3628800)
  (1 2 3 8 15 48 105 384 945 3840)
  (1 2 3 4 10 18 28 80 162 280)
  (1 2 3 4 5 12 21 32 45 120)
  (1 2 3 4 5 6 14 24 36 50))
```



## REXX

This version also handles zero as well as positive integers.

```rexx
/*REXX program calculates and displays K-fact (multifactorial) of non-negative integers.*/
numeric digits 1000                              /*get ka-razy with the decimal digits. */
parse arg num deg .                              /*get optional arguments from the C.L. */
if num=='' | num==","   then num=15              /*Not specified?  Then use the default.*/
if deg=='' | deg==","   then deg=10              /* "      "         "   "   "     "    */
say '═══showing multiple factorials (1 ──►'     deg")  for numbers  1 ──►"      num
say
     do d=1  for deg                             /*the factorializing (degree)  of  !'s.*/
     _=                                          /*the list of factorials  (so far).    */
            do f=1  for num                      /* ◄── perform a ! from  1 ───► number.*/
            _=_  Kfact(f, d)                     /*build a  list  of factorial products.*/
            end   /*f*/                          /* [↑]    D   can default to  unity.   */

     say right('n'copies("!", d), 1+deg)    right('['d"]", 2+length(num) )':'     _
     end          /*d*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Kfact: procedure; !=1;   do j=arg(1)  to 2  by -word(arg(2) 1,1);  !=!*j;  end;   return !
```

'''output'''   when using the default input:

```txt

═══showing multiple factorials (1 ──► 10)  for numbers  1 ──► 15

         n!  [1]:  1 2 6 24 120 720 5040 40320 362880 3628800 39916800 479001600 6227020800 87178291200 1307674368000
        n!!  [2]:  1 2 3 8 15 48 105 384 945 3840 10395 46080 135135 645120 2027025
       n!!!  [3]:  1 2 3 4 10 18 28 80 162 280 880 1944 3640 12320 29160
      n!!!!  [4]:  1 2 3 4 5 12 21 32 45 120 231 384 585 1680 3465
     n!!!!!  [5]:  1 2 3 4 5 6 14 24 36 50 66 168 312 504 750
    n!!!!!!  [6]:  1 2 3 4 5 6 7 16 27 40 55 72 91 224 405
   n!!!!!!!  [7]:  1 2 3 4 5 6 7 8 18 30 44 60 78 98 120
  n!!!!!!!!  [8]:  1 2 3 4 5 6 7 8 9 20 33 48 65 84 105
 n!!!!!!!!!  [9]:  1 2 3 4 5 6 7 8 9 10 22 36 52 70 90
n!!!!!!!!!! [10]:  1 2 3 4 5 6 7 8 9 10 11 24 39 56 75

```



## Ring


```ring

see "Degree  " +  "|" + "           Multifactorials 1 to 10" + nl
see copy("-", 52) + nl
for d = 1 to 5
    see "" + d + "       " + "| "
    for n = 1 to 10
        see "" + multiFact(n, d) + " "
    next
    see nl
next

func multiFact n, degree
     fact = 1
     for i = n to 2 step -degree
         fact = fact * i
     next
     return fact

```

Output:

```txt

Degree  |           Multifactorials 1 to 10
----------------------------------------------------
1       | 1 2 6 24 120 720 5040 40320 362880 3628800
2       | 1 2 3 8 15 48 105 384 945 3840
3       | 1 2 3 4 10 18 28 80 162 280
4       | 1 2 3 4 5 12 21 32 45 120
5       | 1 2 3 4 5 6 14 24 36 50

```



## Ruby


```ruby

def multifact(n, d)
  n.step(1, -d).inject( :* )
end

(1..5).each {|d| puts "Degree #{d}: #{(1..10).map{|n| multifact(n, d)}.join "\t"}"}

```

'''output'''
<pre style="overflow:scroll">
Degree 1: 1	2	6	24	120	720	5040	40320	362880	3628800
Degree 2: 1	2	3	8	15	48	105	384	945	3840
Degree 3: 1	2	3	4	10	18	28	80	162	280
Degree 4: 1	2	3	4	5	12	21	32	45	120
Degree 5: 1	2	3	4	5	6	14	24	36	50

```


=
## Run BASIC
=

```runbasic

print "Degree  " +  "|" + "           Multifactorials 1 to 10" + nl
print copy("-", 52) + nl
for d = 1 to 5
    print "" + d + "       " + "| "
    for n = 1 to 10
        print "" + multiFact(n, d) + " ";
    next
    print
next

function multiFact(n,degree)
     fact = 1
     for i = n to 2 step -degree
         fact = fact * i
     next
     multiFact = fact
 end function
```


```txt
Degree  |           Multifactorials 1 to 10
--------|---------------------------------------------
1       | 1 2 6 24 120 720 5040 40320 362880 3628800
2       | 1 2 3 8 15 48 105 384 945 3840
3       | 1 2 3 4 10 18 28 80 162 280
4       | 1 2 3 4 5 12 21 32 45 120
5       | 1 2 3 4 5 6 14 24 36 50

```



## Scala


```scala

def multiFact(n : BigInt, degree : BigInt) = (n to 1 by -degree).product

for{
  degree <- 1 to 5
  str = (1 to 10).map(n => multiFact(n, degree)).mkString(" ")
} println(s"Degree $degree: $str")

```


{{out}}

```txt
Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50
```



## Scheme



```scheme

(import (scheme base)
        (scheme write)
        (srfi 1))

(define (multi-factorial n m)
  (fold * 1 (iota (ceiling (/ n m)) n (- m))))

(for-each
  (lambda (degree)
    (display (string-append "degree "
                            (number->string degree)
                            ": "))
    (for-each
      (lambda (num)
        (display (string-append (number->string (multi-factorial num degree))
                                " ")))
      (iota 10 1))
    (newline))
  (iota 5 1))

```


{{out}}

```txt

degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
degree 2: 1 2 3 8 15 48 105 384 945 3840
degree 3: 1 2 3 4 10 18 28 80 162 280
degree 4: 1 2 3 4 5 12 21 32 45 120
degree 5: 1 2 3 4 5 6 14 24 36 50

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: multiFact (in var integer: num, in integer: degree) is func
  result
    var integer: multiFact is 1;
  begin
    while num > 1 do
      multiFact *:= num;
      num -:= degree;
    end while;
  end func;

const proc: main is func
  local
    var integer: degree is 0;
    var integer: num is 0;
  begin
    for degree range 1 to 5 do
      write("Degree " <& degree <& ": ");
      for num range 1 to 10 do
        write(multiFact(num, degree) <& " ");
      end for;
      writeln;
    end for;
  end func;
```


{{out}}

```txt

Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50

```



## Sidef


```ruby
func mfact(s, n) {
    n > 0 ? (n * mfact(s, n-s)) : 1
}

{ |s|
    say "step=#{s}: #{{|n| mfact(s, n)}.map(1..10).join(' ')}"
} << 1..10
```

{{out}}

```txt

step=1: 1 2 6 24 120 720 5040 40320 362880 3628800
step=2: 1 2 3 8 15 48 105 384 945 3840
step=3: 1 2 3 4 10 18 28 80 162 280
step=4: 1 2 3 4 5 12 21 32 45 120
step=5: 1 2 3 4 5 6 14 24 36 50
step=6: 1 2 3 4 5 6 7 16 27 40
step=7: 1 2 3 4 5 6 7 8 18 30
step=8: 1 2 3 4 5 6 7 8 9 20
step=9: 1 2 3 4 5 6 7 8 9 10
step=10: 1 2 3 4 5 6 7 8 9 10

```



## Swift



```swift
func multiFactorial(_ n: Int, k: Int) -> Int {
  return stride(from: n, to: 0, by: -k).reduce(1, *)
}

let multis = (1...5).map({degree in
  (1...10).map({member in
    multiFactorial(member, k: degree)
  })
})

for (i, degree) in multis.enumerated() {
  print("Degree \(i + 1): \(degree)")
}
```


{{out}}

```txt
Degree 1: [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]
Degree 2: [1, 2, 3, 8, 15, 48, 105, 384, 945, 3840]
Degree 3: [1, 2, 3, 4, 10, 18, 28, 80, 162, 280]
Degree 4: [1, 2, 3, 4, 5, 12, 21, 32, 45, 120]
Degree 5: [1, 2, 3, 4, 5, 6, 14, 24, 36, 50]
```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

proc mfact {n m} {
    set mm [expr {-$m}]
    for {set r $n} {[incr n $mm] > 1} {set r [expr {$r * $n}]} {}
    return $r
}

foreach n {1 2 3 4 5 6 7 8 9 10} {
    puts $n:[join [lmap m {1 2 3 4 5 6 7 8 9 10} {mfact $m $n}] ,]
}
```

{{out}}

```txt

1:1,2,6,24,120,720,5040,40320,362880,3628800
2:1,2,3,8,15,48,105,384,945,3840
3:1,2,3,4,10,18,28,80,162,280
4:1,2,3,4,5,12,21,32,45,120
5:1,2,3,4,5,6,14,24,36,50
6:1,2,3,4,5,6,7,16,27,40
7:1,2,3,4,5,6,7,8,18,30
8:1,2,3,4,5,6,7,8,9,20
9:1,2,3,4,5,6,7,8,9,10
10:1,2,3,4,5,6,7,8,9,10

```



## uBasic/4tH

{{Trans|Run BASIC}}
<lang>print "Degree  |           Multifactorials 1 to 10"
for x = 1 to 53 : print "-"; : next : print
for d = 1 to 5
  print d;"       ";"| ";
  for n = 1 to 10
    print FUNC(_multiFact(n, d));" ";
  next
  print
next

end

_multiFact param (2)
  local (2)
  c@ = 1
  for d@ = a@ to 2 step -b@
    c@ = c@ * d@
  next
return (c@)
```

{{Out}}

```txt
Degree  |           Multifactorials 1 to 10
-----------------------------------------------------
1       | 1 2 6 24 120 720 5040 40320 362880 3628800
2       | 1 2 3 8 15 48 105 384 945 3840
3       | 1 2 3 4 10 18 28 80 162 280
4       | 1 2 3 4 5 12 21 32 45 120
5       | 1 2 3 4 5 6 14 24 36 50

0 OK, 0:1063
```


## VBScript


```vb

Function multifactorial(n,d)
	If n = 0 Then
		multifactorial = 1
	Else
		For i = n To 1 Step -d
			If i = n Then
				multifactorial = n
			Else
				multifactorial = multifactorial * i
			End If
		Next
	End If
End Function

For j = 1 To 5
	WScript.StdOut.Write "Degree " & j & ": "
	For k = 1 To 10
		If k = 10 Then
			WScript.StdOut.Write multifactorial(k,j)
		Else
			WScript.StdOut.Write multifactorial(k,j) & " "
		End If
	Next
	WScript.StdOut.WriteLine
Next

```


{{Out}}

```txt

Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50

```



## Wortel


```wortel
@let {
  facd  &[d n]?{<= n d n @prod@range[n 1 @-d]}
  ; tacit implementation
  facdt ^(!?(/^> .1 ^(@prod @range ~1jdtShj &^!(@- @id))) @,)
  ; recursive
  facdrec &[n d] ?{<= n d n *n !!facdrec -n d d}
  ; output
  l @to 10
  ~@each @to 5 &n !console.log "Degree {n}: {@join @s !*\facd n l}"
}
```

Output

```txt
Degree 1: 1 2 6 24 120 720 5040 40320 362880 3628800
Degree 2: 1 2 3 8 15 48 105 384 945 3840
Degree 3: 1 2 3 4 10 18 28 80 162 280
Degree 4: 1 2 3 4 5 12 21 32 45 120
Degree 5: 1 2 3 4 5 6 14 24 36 50
```



## XPL0


```XPL0
code ChOut=8, CrLf=9, IntOut=11;

func MultiFac(N, D);    \Return multifactorial of N in degree D
int  N, D;
int  F;
[F:= 1;
repeat  F:= F*N;
        N:= N-D;
until   N <= 1;
return F;
];

int I, J;               \generate table of multifactorials
for J:= 1 to 5 do
    [for I:= 1 to 10 do
        [IntOut(0, MultiFac(I, J));  ChOut(0, 9\tab\)];
    CrLf(0);
    ]
```


{{out}}

```txt

1       2       6       24      120     720     5040    40320   362880  3628800
1       2       3       8       15      48      105     384     945     3840
1       2       3       4       10      18      28      80      162     280
1       2       3       4       5       12      21      32      45      120
1       2       3       4       5       6       14      24      36      50

```



## zkl


```zkl
fcn mfact(n,m){ [n..1,-m].reduce('*,1) }
foreach m in ([1..5]){ println("%d: %s".fmt(m,[1..10].apply(mfact.fp1(m)))) }
```

{{out}}

```txt

1: L(1,2,6,24,120,720,5040,40320,362880,3628800)
2: L(1,2,3,8,15,48,105,384,945,3840)
3: L(1,2,3,4,10,18,28,80,162,280)
4: L(1,2,3,4,5,12,21,32,45,120)
5: L(1,2,3,4,5,6,14,24,36,50)

```

