+++
title = "Population count"
description = ""
date = 2019-09-30T17:12:27Z
aliases = []
[extra]
id = 17360
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "algol_w",
  "applescript",
  "autohotkey",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "free_pascal",
  "gambas",
  "go",
  "haskell",
  "idris",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "min",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "seed7",
  "sidef",
  "swift",
  "tcl",
  "unix_shell",
  "vba",
  "vbscript",
  "visual_basic_dotnet",
  "yabasic",
  "zkl",
]
+++

{{task}}[[Category:Mathematics]]
The ''[[wp:Hamming weight|population count]]''   is the number of   <big>'''1'''</big>s   (ones)   in the binary representation of a non-negative integer.

''Population count'' is also known as   ''pop count'',   ''popcount'',   ''sideways sum'',   and   ''Hamming weight''.

: For example,   <big>'''5'''</big>   (which is   <big>'''101'''</big>   in binary)   has a population count of   <big>'''2'''</big>.


''[http://mathworld.wolfram.com/EvilNumber.html Evil numbers]''   are non-negative integers  that have an   ''even''   population count.

''[http://mathworld.wolfram.com/OdiousNumber.html Odious numbers]''      are  positive integers that have an    ''odd''   population count.


## Task

* write a function (or routine) to return the population count of a non-negative integer.
* all computation of the lists below should start with   <big>'''0'''</big>   (zero indexed).
:* display the   ''pop count''   of the   1<sup>st</sup>   thirty powers of   <big>'''3'''</big>       (<big>'''3<sup>0</sup>''',   '''3<sup>1</sup>''',   '''3<sup>2</sup>''',   '''3<sup>3</sup>''',   '''3<sup>4</sup>''',   <b>∙∙∙</b>   '''3<sup>29</sup>'''</big>).
:* display the   1<sup>st</sup>   thirty     ''evil''        numbers.
:* display the   1<sup>st</sup>   thirty          ''odious''           numbers.
* display each list of integers on one line   (which may or may not include a title),   each set of integers being shown should be properly identified.


## See also

* The On-Line Encyclopedia of Integer Sequences:   [http://oeis.org/A000069 A000069 odious numbers].
* The On-Line Encyclopedia of Integer Sequences:   [http://oeis.org/A001969 A001969 evil numbers].





## 360 Assembly

Use of the old " Unnormalized Double Floating Point" feature, a bit forgotten, to have 56-bit integers. And also use of ICM (Insert Characters Under Mask) and TM (Test under Mask) to handle bits.

Let's note:
* in   Normalized Double Floating Point, one is implemented X'4110000000000000'
* in Unnormalized Double Floating Point, one is implemented X'4E00000000000001'



```360asm
*        Population count          09/05/2019
POPCNT   CSECT
         USING  POPCNT,R13         base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LD     F0,UN              1
         STD    F0,BB              bb=1
         MVC    PG(7),=C'pow  3:'  init buffer
         L      R10,NN             nn
         BCTR   R10,0              nn-1
         LA     R9,PG+7            @pg
         LA     R6,0               i=0
       DO WHILE=(CR,R6,LE,R10)     do i=0 to nn-1
         LM     R0,R1,BB             r0r1=bb
         BAL    R14,POPCOUNT         call popcount(bb)
         LR     R1,R0                popcount(bb)
         XDECO  R1,XDEC              edit popcount(bb)
         MVC    0(3,R9),XDEC+9       output popcount(bb)
         LD     F0,BB                bb
         AW     F0,BB                bb*2
         AW     F0,BB                bb*3
         STD    F0,BB                bb=bb*3
         LA     R9,3(R9)             @pg
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         XPRNT  PG,L'PG            print buffer
         SR     R7,R7              j=0
       DO WHILE=(C,R7,LE,=F'1')    do j=0 to 1
         MVC    PG,=CL132' '         clear buffer
       IF   LTR,R7,Z,R7 THEN         if j=0 then
         MVC    PG(7),=C'evil:  '      init buffer
       ELSE     ,                    else
         MVC    PG(7),=C'odious:'      init buffer
       ENDIF    ,                    endif
         LA     R9,PG+7              @pg
         SR     R8,R8                n=0
         SR     R6,R6                i=0
       DO WHILE=(C,R8,LT,NN)         do i=0 by 1 while(n<nn)
         XR     R0,R0                  r0=0
         LR     R1,R6                  r1=i
         BAL    R14,POPCOUNT           r0=popcount(i)
         SRDA   R0,32                  ~
         D      R0,=F'2'               popcount(i)/2
       IF    CR,R0,EQ,R7 THEN          if popcount(i)//2=j then
         LA     R8,1(R8)                 n=n+1
         XDECO  R6,XDEC                  edit i
         MVC    0(3,R9),XDEC+9           output i
         LA     R9,3(R9)                 @pg
       ENDIF    ,                      endif
         LA     R6,1(R6)             i++
       ENDDO    ,                    enddo i
         XPRNT  PG,L'PG              print buffer
         LA     R7,1(R7)             j++
       ENDDO    ,                  enddo j
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
*------- ----   ------------------
POPCOUNT EQU    *                  popcount(x)
         ICM    R0,B'1000',=X'00'  zap exponant part
         XR     R3,R3              y=0
         LA     R4,56              mantissa size = 56
LOOP     STC    R1,CC              do i=1 to 56
         TM     CC,X'01'             if bit(x,i)=1
         BNO    NOTONE               then{
         LA     R3,1(R3)               y++}
NOTONE   SRDA   R0,1                 shift right double arithmetic
         BCT    R4,LOOP            enddo i
         LR     R0,R3              return(y)
         BR     R14                return
*------- ----   ------------------
NN       DC     F'30'              nn=30
BB       DS     D                  bb
UN       DC     X'4E00000000000001'  un=1 (unnormalized)
PG       DC     CL132' '           buffer
XDEC     DS     CL12               temp for xdeco
CC       DS     C
         REGEQU
         END    POPCNT
```

```txt

pow  3:  1  2  2  4  3  6  6  5  6  8  9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil:    0  3  5  6  9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious:  1  2  4  7  8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## Ada


Specification and implementation of an auxiliary package "Population_Count". The same package is used for [[Pernicious numbers#Ada]]


```ada
with Interfaces;

package Population_Count is
   subtype Num is Interfaces.Unsigned_64;
   function Pop_Count(N: Num) return Natural;
end Population_Count;
```



```Ada
package body Population_Count is

   function Pop_Count(N: Num) return Natural is
      use Interfaces;
      K5555:  constant Unsigned_64 := 16#5555555555555555#;
      K3333:  constant Unsigned_64 := 16#3333333333333333#;
      K0f0f:  constant Unsigned_64 := 16#0f0f0f0f0f0f0f0f#;
      K0101:  constant Unsigned_64 := 16#0101010101010101#;
      X: Unsigned_64 := N;
   begin
      X :=  X            - (Shift_Right(X, 1)   and k5555);
      X := (X and k3333) + (Shift_Right(X, 2)   and k3333);
      X := (X            +  (Shift_Right(X, 4)) and K0f0f);
      X := Shift_Right((x * k0101), 56);
      return Natural(X);
   end Pop_Count;

end Population_Count;
```


The main program:


```Ada
with Ada.Text_IO, Population_Count; use Ada.Text_IO; use Population_Count;

procedure Test_Pop_Count is

   X: Num; use type Num;

begin
   Put("Pop_Cnt(3**i):"); -- print pop_counts of powers of three
   X := 1; -- X=3**0
   for I in 1 .. 30 loop
      Put(Natural'Image(Pop_Count(X)));
      X := X * 3;
   end loop;
   New_Line;

   Put("Evil:         ");    -- print first thirty evil numbers
   X := 0;
   for I in 1 .. 30 loop
      while Pop_Count(X) mod 2 /= 0 loop -- X is not evil
         X := X + 1;
      end loop;
      Put(Num'Image(X));
      X := X + 1;
   end loop;
   New_Line;

   Put("Odious:       "); -- print thirty oudous numbers
   X := 1;
   for I in 1 .. 30 loop
      while Pop_Count(X) mod 2 /= 1 loop -- X is not odious
         X := X + 1;
      end loop;
      Put(Num'Image(X));
      X := X + 1;
   end loop;
   New_Line;
end Test_Pop_Count;
```


```txt
Pop_Cnt(3**i): 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil:          0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious:        1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
```



## ALGOL 68


```algol68
# returns the population count (number of bits on) of the non-negative       #
# integer n                                                                  #
PROC population count = ( LONG INT n )INT:
     BEGIN
        LONG INT number := n;
        INT      result := 0;
        WHILE number > 0 DO
            IF ODD number THEN result +:= 1 FI;
            number OVERAB 2
        OD;
        result
     END # population # ;

# population count of 3^0, 3^1, 3*2, ..., 3^29                               #
LONG INT  power of three := 1;
print( ( "3^x pop counts:" ) );
FOR power FROM 0 TO 29 DO
    print( ( " ", whole( population count( power of three ), 0 ) ) );
    power of three *:= 3
OD;
print( ( newline ) );
# print the first thirty evil numbers (even population count)                #
INT evil count := 0;
print( ( "evil numbers  :" ) );
FOR n FROM 0 WHILE evil count < 30 DO
    IF NOT ODD population count( n ) THEN
        print( ( " ", whole( n, 0 ) ) );
        evil count +:= 1
    FI
OD;
print( ( newline ) );
# print the first thirty odious numbers (odd population count)               #
INT odious count := 0;
print( ( "odious numbers:" ) );
FOR n WHILE odious count < 30 DO
    IF ODD population count( n ) THEN
        print( ( " ", whole( n, 0 ) ) );
        odious count +:= 1
    FI
OD;
print( ( newline ) )

```

```txt

3^x pop counts: 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil numbers  : 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious numbers: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## ALGOL W


```algolw
begin
    % returns the population count (number of bits on) of the non-negative integer n %
    integer procedure populationCount( integer value n ) ;
            begin
                integer v, count;
                v     := n;
                count := 0;
                while v > 0 do begin
                    if odd( v ) then count := count + 1;
                    v     := v div 2
                end while_v_gt_0 ;
                count
            end populationCount ;
    % returns the sum of population counts of the elements of the array n            %
    %         the bounds of n must be 1 :: length                                    %
    integer procedure arrayPopulationCount( integer array n ( * ); integer value length ) ;
            begin
                integer count;
                count := 0;
                for i := 1 until length do count := count + populationCount( n( i ) );
                count
            end arrayPopulationCount ;
    begin %task requirements %
        integer array power( 1 :: 8 );
        integer n, count, carry;
        % population counts of the first 30 powers of three %
        % Algol W integers are 32-bit, so we simulate 64-bit with an array of integers %
        % the only operation we need is multiplication by 3                            %
        % we use 8 bits of each number                                                 %
        % start with 3^0, which is 1 %
        for i := 1 until 8 do power( i ) := 0;
        power( 1 ) := 1;
        write( i_w := 1, s_w := 0, "3^x  population: ", arrayPopulationCount( power, 8 ) );
        for p := 1 until 29 do begin
            carry := 0;
            for b := 1 until 8 do begin
                integer bValue;
                bValue     := ( power( b ) * 3 ) + carry;
                carry      := bValue div 256;
                power( b ) := bValue rem 256
            end for_b ;
            writeon( i_w := 1, s_w := 0, " ", arrayPopulationCount( power, 8 ) )
        end for_p ;

        % evil numbers (even population count) %
        write( "evil    numbers:" );
        n     := 0;
        count := 0;
        while count < 30 do begin
            if not odd( populationCount( n ) ) then begin
                writeon( i_w := 1, s_w := 0, " ", n );
                count := count + 1
            end if_not_odd_populationCount ;
            n := n + 1
        end evil_numbers_loop ;

        % odious numbers (odd population count %
        write( "odious  numbers:" );
        n     := 0;
        count := 0;
        while count < 30 do begin
            if odd( populationCount( n ) ) then begin
                writeon( i_w := 1, s_w := 0, " ", n );
                count := count + 1
            end if_odd_populationCount ;
            n := n + 1
        end odious_numbers_loop
   end
end.
```

```txt

3^x  population: 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil    numbers: 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious  numbers: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## AppleScript

```AppleScript
-- popCount :: Int -> Int
on popCount(n)
    script bitSum
        on |λ|(a, x)
            a + (x as integer)
        end |λ|
    end script

    foldl(bitSum, 0, characters of showIntAtBase(n, 2))
end popCount


-- TEST -----------------------------------------------------------------------
on run
    script powerOfThreePopCount
        on |λ|(x)
            popCount(3 ^ x)
        end |λ|
    end script

    script popCountisEven
        on |λ|(x)
            popCount(x) mod 2 = 0
        end |λ|
    end script

    {popCounts:¬
        map(powerOfThreePopCount, enumFromTo(0, 30)), evenThenOdd:¬
        partition(popCountisEven, enumFromTo(0, 59))}
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

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

-- partition :: predicate -> List -> (Matches, nonMatches)
-- partition :: (a -> Bool) -> [a] -> ([a], [a])
on partition(f, xs)
    tell mReturn(f)
        set lst to {{}, {}}
        repeat with x in xs
            set v to contents of x
            set end of item ((|λ|(v) as integer) + 1) of lst to v
        end repeat
    end tell
    {item 2 of lst, item 1 of lst}
end partition

-- showIntAtBase :: Int -> Int -> String
on showIntAtBase(n, base)
    if base > 1 then
        if n > 0 then
            set m to n mod base
            set r to n - m
            if r > 0 then
                set prefix to showIntAtBase(r div base, base)
            else
                set prefix to ""
            end if

            if m < 10 then
                set baseCode to 48 -- "0"
            else
                set baseCode to 55 -- "A" - 10
            end if

            prefix & character id (baseCode + m)
        else
            "0"
        end if
    else
        missing value
    end if
end showIntAtBase
```

```AppleScript
{popCounts:
{1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25, 25},
evenThenOdd:
{{0, 3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57, 58},
{1, 2, 4, 7, 8, 11, 13, 14, 16, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59}}}
```



## AutoHotkey


```AutoHotkey
Loop, 30
	Out1 .= PopCount(3 ** (A_Index - 1)) " "
Loop, 60
	i := A_Index - 1
	, PopCount(i) & 0x1 ? Out3 .= i " " : Out2 .= i " "
MsgBox, % "3^x:`t" Out1 "`nEvil:`t" Out2 "`nOdious:`t" Out3

PopCount(x) {	;https://en.wikipedia.org/wiki/Hamming_weight#Efficient_implementation
	x -= (x >> 1) & 0x5555555555555555
	, x := (x & 0x3333333333333333) + ((x >> 2) & 0x3333333333333333)
	, x := (x + (x >> 4)) & 0x0f0f0f0f0f0f0f0f
	return (x * 0x0101010101010101) >> 56
}
```

```txt
3^x:	1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil:	0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious:	1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
```



## C

```c
#include <stdio.h>

int main() {
  {
    unsigned long long n = 1;
    for (int i = 0; i < 30; i++) {
      // __builtin_popcount() for unsigned int
      // __builtin_popcountl() for unsigned long
      // __builtin_popcountll() for unsigned long long
      printf("%d ", __builtin_popcountll(n));
      n *= 3;
    }
    printf("\n");
  }

  int od[30];
  int ne = 0, no = 0;
  printf("evil  : ");
  for (int n = 0; ne+no < 60; n++) {
    if ((__builtin_popcount(n) & 1) == 0) {
      if (ne < 30) {
	printf("%d ", n);
	ne++;
      }
    } else {
      if (no < 30) {
	od[no++] = n;
      }
    }
  }
  printf("\n");
  printf("odious: ");
  for (int i = 0; i < 30; i++) {
    printf("%d ", od[i]);
  }
  printf("\n");

  return 0;
}
```

```txt

1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil  : 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



GCC's builtin doesn't exist prior to 3.4, and the LL version is broken in 3.4 to 4.1.  In 4.2+, if the platform doesn't have a good popcount instruction or isn't enabled (e.g. not compiled with <code>-march=native</code>), it typically emits unoptimized code which is over 2x slower than the C below.  Alternative:

```c
#if defined(__POPCNT__) && defined(__GNUC__) && (__GNUC__> 4 || (__GNUC__== 4 && __GNUC_MINOR__> 1))
#define HAVE_BUILTIN_POPCOUNTLL
#endif
static uint64_t bitcount64(uint64_t b) {
  b -= (b >> 1) & 0x5555555555555555;
  b = (b & 0x3333333333333333) + ((b >> 2) & 0x3333333333333333);
  b = (b + (b >> 4)) & 0x0f0f0f0f0f0f0f0f;
  return (b * 0x0101010101010101) >> 56;
}
/* For 32-bit, an 8-bit table may or may not be a little faster */
static uint32_t bitcount32(uint32_t b) {
  b -= (b >> 1) & 0x55555555;
  b = (b & 0x33333333) + ((b >> 2) & 0x33333333);
  b = (b + (b >> 4)) & 0x0f0f0f0f;
  return (b * 0x01010101) >> 24;
}
```



## C++

```cpp
#include <iostream>
#include <bitset>
#include <climits>

size_t popcount(unsigned long long n) {
  return std::bitset<CHAR_BIT * sizeof n>(n).count();
}

int main() {
  {
    unsigned long long n = 1;
    for (int i = 0; i < 30; i++) {
      std::cout << popcount(n) << " ";
      n *= 3;
    }
    std::cout << std::endl;
  }

  int od[30];
  int ne = 0, no = 0;
  std::cout << "evil  : ";
  for (int n = 0; ne+no < 60; n++) {
    if ((popcount(n) & 1) == 0) {
      if (ne < 30) {
	std::cout << n << " ";
	ne++;
      }
    } else {
      if (no < 30) {
	od[no++] = n;
      }
    }
  }
  std::cout << std::endl;
  std::cout << "odious: ";
  for (int i = 0; i < 30; i++) {
    std::cout << od[i] << " ";
  }
  std::cout << std::endl;

  return 0;
}
```

```txt

1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil  : 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```


## C#

```c#

using System;
using System.Linq;

namespace PopulationCount
{
    class Program
    {
        private static int PopulationCount(long n)
        {
            string binaryn = Convert.ToString(n, 2);
            return binaryn.ToCharArray().Where(t => t == '1').Count();
        }

        static void Main(string[] args)
        {
            Console.WriteLine("Population Counts:");
            Console.Write("3^n :   ");

            int count = 0;

            while (count < 30)
            {
                double n = Math.Pow(3f, (double)count);
                int popCount = PopulationCount((long)n);
                Console.Write(string.Format("{0} ", popCount));
                count++;
            }

            Console.WriteLine();
            Console.Write("Evil:   ");

            count = 0;
            int i = 0;

            while (count < 30)
            {
                int popCount = PopulationCount(i);

                if (popCount % 2 == 0)
                {
                    count++;
                    Console.Write(string.Format("{0} ", i));
                }

                i++;
            }

            Console.WriteLine();
            Console.Write("Odious: ");

            count = 0;
            i = 0;

            while (count < 30)
            {
                int popCount = PopulationCount(i);

                if (popCount % 2 != 0)
                {
                    count++;
                    Console.Write(string.Format("{0} ", i));
                }

                i++;
            }

            Console.ReadKey();
        }
    }
}

```

```txt

Population Counts:
3^n :   1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil:   0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## Common Lisp


```lisp
(format T "3^x: ~{~a ~}~%"
        (loop for i below 30
              collect (logcount (expt 3 i))))

(multiple-value-bind
  (evil odious)
  (loop for i below 60
        if (evenp (logcount i)) collect i into evil
        else collect i into odious
        finally (return (values evil odious)))
  (format T "evil: ~{~a ~}~%" evil)
  (format T "odious: ~{~a ~}~%" odious))
```

```txt
3^x: 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil: 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
```



## D


```d
void main() {
    import std.stdio, std.algorithm, std.range, core.bitop;

    enum pCount = (ulong n) => popcnt(n & uint.max) + popcnt(n >> 32);
    writefln("%s\nEvil: %s\nOdious: %s",
             uint.max.iota.map!(i => pCount(3L ^^ i)).take(30),
             uint.max.iota.filter!(i => pCount(i) % 2 == 0).take(30),
             uint.max.iota.filter!(i => pCount(i) % 2).take(30));
}
```

```txt
[1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25]
Evil: [0, 3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57, 58]
Odious: [1, 2, 4, 7, 8, 11, 13, 14, 16, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59]
```



## Elixir


```elixir
defmodule Population do

  def count(n), do: count(<<n :: integer>>, 0)

  defp count(<<>>, acc), do: acc

  defp count(<<bit :: integer-1, rest :: bitstring>>, sum), do: count(rest, sum + bit)

  def evil?(n), do: n >= 0 and rem(count(n),2) == 0

  def odious?(n), do: n >= 0 and rem(count(n),2) == 1

end

IO.puts "Population count of the first thirty powers of 3:"
IO.inspect Stream.iterate(1, &(&1*3)) |> Enum.take(30) |> Enum.map(&Population.count(&1))
IO.puts "first thirty evil numbers:"
IO.inspect Stream.iterate(0, &(&1+1)) |> Stream.filter(&Population.evil?(&1)) |> Enum.take(30)
IO.puts "first thirty odious numbers:"
IO.inspect Stream.iterate(0, &(&1+1)) |> Stream.filter(&Population.odious?(&1)) |> Enum.take(30)
```


```txt

Population count of the first thirty powers of 3:
[1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25]
first thirty evil numbers:
[0, 3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57, 58]
first thirty odious numbers:
[1, 2, 4, 7, 8, 11, 13, 14, 16, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59]

```



## Erlang


```erlang
-module(population_count).
-export([popcount/1]).

-export([task/0]).

popcount(N) ->
    popcount(N,0).

popcount(0,Acc) ->
    Acc;
popcount(N,Acc) ->
    popcount(N div 2, Acc + N rem 2).

threes(_,0,Acc) ->
    lists:reverse(Acc);
threes(Threes,N,Acc) ->
    threes(Threes * 3, N-1, [popcount(Threes)|Acc]).

threes(N) ->
    threes(1,N,[]).

evil(_,0,Acc) ->
    lists:reverse(Acc);
evil(N,Count,Acc) ->
    case popcount(N) rem 2 of
        0 ->
            evil(N+1,Count-1,[N|Acc]);
        1 ->
            evil(N+1,Count,Acc)
    end.
evil(Count) ->
    evil(0,Count,[]).

odious(_,0,Acc) ->
    lists:reverse(Acc);
odious(N,Count,Acc) ->
    case popcount(N) rem 2 of
        1 ->
            odious(N+1,Count-1,[N|Acc]);
        0 ->
            odious(N+1,Count,Acc)
    end.
odious(Count) ->
    odious(1,Count,[]).


task() ->
    io:format("Powers of 3: ~p~n",[threes(30)]),
    io:format("Evil:~p~n",[evil(30)]),
    io:format("Odious:~p~n",[odious(30)]).
```

```erlang>61
 population_count:task().
Powers of 3: [1,2,2,4,3,6,6,5,6,8,9,13,10,11,14,15,11,14,14,17,17,20,19,22,16,18,24,30,25,
 25]
Evil:[0,3,5,6,9,10,12,15,17,18,20,23,24,27,29,30,33,34,36,39,40,43,45,46,48,
      51,53,54,57,58]
Odious:[1,2,4,7,8,11,13,14,16,19,21,22,25,26,28,31,32,35,37,38,41,42,44,47,49,
        50,52,55,56,59]
ok
```



## Factor


```factor
USING: formatting kernel lists lists.lazy math math.bits
math.functions namespaces prettyprint.config sequences ;
IN: rosetta-code.population-count

: pop-count ( n -- m ) make-bits [ t = ] count ;
: 3^n ( obj -- obj' ) [ 3 swap ^ pop-count ] lmap-lazy ;
: evil ( obj -- obj' ) [ pop-count even? ] lfilter ;
: odious ( obj -- obj' ) [ pop-count odd? ] lfilter ;

: pop-count-demo ( -- )
    100 margin set 0 lfrom [ 3^n ] [ evil ] [ odious ] tri
    [ 30 swap ltake list>array ] tri@
    "3^n:    %u\nEvil:   %u\nOdious: %u\n" printf ;

MAIN: pop-count-demo
```

```txt

3^n:    { 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25 }
Evil:   { 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58 }
Odious: { 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59 }

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Population_count this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

```Forth
: popcnt ( n -- u)  0 swap
   BEGIN dup WHILE tuck 1 AND +  swap 1 rshift REPEAT
   DROP ;
: odious? ( n -- t|f)  popcnt 1 AND ;
: evil? ( n -- t|f)  odious? 0= ;

CREATE A 30 ,
: task1   1 0  ." 3**i popcnt: "
   BEGIN dup A @ < WHILE
     over popcnt .  1+ swap 3 * swap
   REPEAT  DROP DROP CR ;
: task2   0 0  ." evil       : "
   BEGIN dup A @ < WHILE
     over evil? IF over . 1+ THEN swap 1+ swap
   REPEAT  DROP DROP CR ;
: task3   0 0  ." odious     : "
   BEGIN dup A @ < WHILE
     over odious? IF over . 1+ THEN swap 1+ swap
   REPEAT  DROP DROP CR ;
task1 task2 task3 BYE
```

```txt
3**i popcnt: 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil       : 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious     : 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
```



## Fortran

```fortran
program population_count
  implicit none

  integer, parameter :: i64 = selected_int_kind(18)
  integer(i64) :: x
  integer :: i, n

  x = 1
  write(*, "(a8)", advance = "no") "3**i :"
  do i = 1, 30
    write(*, "(i3)", advance = "no") popcnt(x)
    x = x * 3
  end do

  write(*,*)
  write(*, "(a8)", advance = "no") "Evil :"
  n = 0
  x = 0
  do while(n < 30)
    if(mod(popcnt(x), 2) == 0) then
      n = n + 1
      write(*, "(i3)", advance = "no") x
    end if
    x = x + 1
  end do

  write(*,*)
  write(*, "(a8)", advance = "no") "Odious :"
  n = 0
  x = 0
  do while(n < 30)
    if(mod(popcnt(x), 2) /= 0) then
      n = n + 1
      write(*, "(i3)", advance = "no") x
    end if
    x = x + 1
  end do

contains

integer function popcnt(x)
  integer(i64), intent(in) :: x
  integer :: i

  popcnt = 0
  do i = 0, 63
    if(btest(x, i)) popcnt = popcnt + 1
  end do

end function
end program
```

```txt
  3**i : 1  2  2  4  3  6  6  5  6  8  9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
  Evil : 0  3  5  6  9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious : 1  2  4  7  8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
```



## Free Pascal

The <code>system</code> unit in the RTL (run-time library) shipped with every FPC (Free Pascal compiler) distribution contains the function <code>popCnt</code>.
It accepts one integer parameter and is defined for all unsigned integer types.
Therefore its implementation is skipped.

```pascal
program populationCount(input, output, stdErr);
var
	// general advice: iterator variables are _signed_
	iterator: int64;
	// the variable we’d like to count the set bits in
	number: qWord;
	// how many evil numbers we’ve already found
	evilCount: int64;
	// odious numbers
	odiousNumber: array[1..30] of qWord;
	odiousIterator: int64;
begin
	// population count for powers of three
	for iterator := 0 to 29 do
	begin
		number := round(exp(ln(3) * iterator));
		write(popCnt(number):3);
	end;
	writeLn();

	// evil numbers
	// (while preserving calculated odious numbers for next sub-task)
	evilCount := 0;
	odiousIterator := low(odiousNumber);

	// for-loop: because we (pretend to) don’t know,
	// when and where we’ve found the first 30 numbers of each
	for iterator := 0 to high(iterator) do
	begin
		// implicit typecast: popCnt only accepts _un_-signed integers
		number := iterator;
		if odd(popCnt(number)) then
		begin
			if odiousIterator <= high(odiousNumber) then
			begin
				odiousNumber[odiousIterator] := number;
				inc(odiousIterator);
			end;
		end
		else
		begin
			if evilCount < 30 then
			begin
				write(number:20);
				inc(evilCount);
			end;
		end;

		if evilCount + odiousIterator > 60 then
		begin
			break;
		end;
	end;
	writeLn();

	// odious numbers
	for number in odiousNumber do
	begin
		write(number:20);
	end;
	writeLn();
end.
```

```txt
  1  2  2  4  3  6  6  5  6  8  9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
                   0                   3                   5                   6                   9                  10                  12                  15                  17                  18                  20                  23                  24                  27                  29                  30                  33                  34                  36                  39                  40                  43                  45                  46                  48                  51                  53                  54                  57                  58
                   1                   2                   4                   7                   8                  11                  13                  14                  16                  19                  21                  22                  25                  26                  28                  31                  32                  35                  37                  38                  41                  42                  44                  47                  49                  50                  52                  55                  56                  59
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=538335b7b71f5ea7b59c0c82fbb0ea3e Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sEvil, sOdious As String                         'To store the output for printing Evil and Odious
Dim iCount, iEvil, iOdious As Integer                'Counters

Print "First 30 numbers ^3\t";                       'Print title

For iCount = 0 To 29                                 'Count 30 times
  Print Len(Replace(Bin(3 ^ iCount), "0", ""));;     'Get the Bin of the number, take out the '0's and the remaining
Next                                                 'length is the Population count e.g. 3^2=9, Bin=1001, remove '0's='11', length=2

iCount = 0                                           'Reset iCount

Repeat                                               'Repeat/Until loop
  If Even(Len(Replace(Bin(iCount), "0", ""))) Then   'If (as above) the result is Even then
    sEvil &= Str(icount) & " "                       'Add it to sEvil
    Inc iEvil                                        'Increase iEvil
  End If
  If Odd(Len(Replace(Bin(iCount), "0", ""))) Then    'If (as above) the result is Odd then
    sOdious &= Str(icount) & " "                     'Add it to sOdious
    Inc iOdious                                      'Increase iOdious
  End If
  Inc iCount                                         'Increase iCount
Until iEvil = 30 And iOdious = 30                    'Until both iEvil and iOdious = 30 then exit the loop

Print "\n1st 30 Evil numbers =\t" & sEvil            'Print Evil
Print "1st 30 Odious numbers =\t" & sOdious          'Print Odious

End
```

Output:

```txt

First 30 numbers ^3     1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
1st 30 Evil numbers =   0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
1st 30 Odious numbers = 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## Go


### Standard Library

As of Go 1.9, this function is in the standard Library.

```go
package main

import (
    "fmt"
    "math/bits"
)

func main() {
    fmt.Println("Pop counts, powers of 3:")
    n := uint64(1) // 3^0
    for i := 0; i < 30; i++ {
        fmt.Printf("%d ", bits.OnesCount64(n))
        n *= 3
    }
    fmt.Println()
    fmt.Println("Evil numbers:")
    var od [30]uint64
    var ne, no int
    for n = 0; ne+no < 60; n++ {
        if bits.OnesCount64(n)&1 == 0 {
            if ne < 30 {
                fmt.Printf("%d ", n)
                ne++
            }
        } else {
            if no < 30 {
                od[no] = n
                no++
            }
        }
    }
    fmt.Println()
    fmt.Println("Odious numbers:")
    for _, n := range od {
        fmt.Printf("%d ", n)
    }
    fmt.Println()
}
```

```txt

Pop counts, powers of 3:
1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil numbers:
0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious numbers:
1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```


### Implementation

Method of WP example '''popcount_3''':

```go
func pop64(w uint64) int {
    const (
        ff    = 1<<64 - 1
        mask1 = ff / 3
        mask3 = ff / 5
        maskf = ff / 17
        maskp = maskf >> 3 & maskf
    )
    w -= w >> 1 & mask1
    w = w&mask3 + w>>2&mask3
    w = (w + w>>4) & maskf
    return int(w * maskp >> 56)
}
```

Method of WP example '''popcount_4''':

```go
func pop64(w uint64) (c int) {
    for w != 0 {
        w &= w - 1
        c++
    }
    return
}
```



## Haskell

```haskell
import Data.Bits (popCount)

printPops :: (Show a, Integral a) => String -> [a] -> IO ()
printPops title counts = putStrLn $ title ++ show (take 30 counts)

main :: IO ()
main = do
  printPops "popcount " $ map popCount $ iterate (*3) (1 :: Integer)
  printPops "evil     " $ filter (even . popCount) ([0..] :: [Integer])
  printPops "odious   " $ filter ( odd . popCount) ([0..] :: [Integer])
```

```txt

popcount [1,2,2,4,3,6,6,5,6,8,9,13,10,11,14,15,11,14,14,17,17,20,19,22,16,18,24,30,25,25]
evil     [0,3,5,6,9,10,12,15,17,18,20,23,24,27,29,30,33,34,36,39,40,43,45,46,48,51,53,54,57,58]
odious   [1,2,4,7,8,11,13,14,16,19,21,22,25,26,28,31,32,35,37,38,41,42,44,47,49,50,52,55,56,59]
```



Or, if we want to write our own popCount, perhaps something like:


```haskell
import Data.List (partition, unfoldr)
import Data.Bifoldable (biList)
import Data.Tuple (swap)
import Data.Bool (bool)

-- POPCOUNT -------------------------------------------------------------
popCount :: Int -> Int
popCount =
  sum . unfoldr ((bool Nothing . Just . swap . flip quotRem 2) <*> (0 <))

-- TEST -----------------------------------------------------------------
main :: IO ()
main =
  mapM_ putStrLn $
  zipWith
    (\k xs -> k ++ ":\n" ++ show xs ++ "\n")
    ["Population count of powers of 3", "evil", "odious"]
    ((popCount . (3 ^) <$> [0 .. 29]) :
     biList (partition (even . popCount) [0 .. 59]))
```

```txt
Population count of powers of 3:
[1,2,2,4,3,6,6,5,6,8,9,13,10,11,14,15,11,14,14,17,17,20,19,22,16,18,24,30,25,25]

evil:
[0,3,5,6,9,10,12,15,17,18,20,23,24,27,29,30,33,34,36,39,40,43,45,46,48,51,53,54,57,58]

odious:
[1,2,4,7,8,11,13,14,16,19,21,22,25,26,28,31,32,35,37,38,41,42,44,47,49,50,52,55,56,59]
```



## Idris



```Idris
module Main
import Data.Vect

isOdd : (x : Int) -> Bool
isOdd x = case mod x 2 of
            0 => False
            1 => True

popcnt : Int -> Int
popcnt 0 = 0
popcnt x = case isOdd x of
  False => popcnt (shiftR x 1)
  True => 1 + popcnt (shiftR x 1)

isOdious : Int -> Bool
isOdious k = isOdd (popcnt k)

isEvil : Int -> Bool
isEvil k = not (isOdious k)

filterUnfoldN : (n : Nat) ->
                (pred : Int -> Bool) -> (f : Int -> a) ->
                (next : Int -> Int) -> (seed : Int) ->
                Vect n a
filterUnfoldN Z pred f next seed = []
filterUnfoldN (S k) pred f next seed =
  if pred seed
  then (f seed) :: filterUnfoldN k pred f next (next seed)
  else filterUnfoldN (S k) pred f next (next seed)

printCompact : (Show a) => Vect n a -> IO ()
printCompact v = putStrLn (unwords (map show (toList v)))

main : IO ()
main = do putStr "popcnt(3**i): "
          printCompact (filterUnfoldN 30 (\_ => True) popcnt (3 *) 1)
          putStr "Evil:         "
          printCompact (filterUnfoldN 30 isEvil id (1 +) 0)
          putStr "Odious:       "
          printCompact (filterUnfoldN 30 isOdious id (1 +) 0)
```

```txt
popcnt(3**i): 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil:         0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious:       1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
```



## J


Implementation:


```J
countPopln=: +/"1@#:
isOdd=: 1 = 2&|
isEven=: 0 = 2&|
```



Task:


```J
   countPopln 3^i.30x
1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
   30{.(#~ isOdd@countPopln) i. 100 NB. odd population count (aka "ODious numbers")
1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
   30{.(#~ isEven@countPopln) i. 100 NB. even population count (aka "EVil numbers")
0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
```



## Java


```java
import java.math.BigInteger;

public class PopCount {
    public static void main(String[] args) {
	{ // with int
	    System.out.print("32-bit integer: ");
	    int n = 1;
	    for (int i = 0; i < 20; i++) {
		System.out.printf("%d ", Integer.bitCount(n));
		n *= 3;
	    }
	    System.out.println();
	}
	{ // with long
	    System.out.print("64-bit integer: ");
	    long n = 1;
	    for (int i = 0; i < 30; i++) {
		System.out.printf("%d ", Long.bitCount(n));
		n *= 3;
	    }
	    System.out.println();
	}
	{ // with BigInteger
	    System.out.print("big integer   : ");
	    BigInteger n = BigInteger.ONE;
	    BigInteger three = BigInteger.valueOf(3);
	    for (int i = 0; i < 30; i++) {
		System.out.printf("%d ", n.bitCount());
		n = n.multiply(three);
	    }
	    System.out.println();
	}

	int[] od = new int[30];
	int ne = 0, no = 0;
	System.out.print("evil   : ");
	for (int n = 0; ne+no < 60; n++) {
	    if ((Integer.bitCount(n) & 1) == 0) {
		if (ne < 30) {
		    System.out.printf("%d ", n);
		    ne++;
		}
	    } else {
		if (no < 30) {
		    od[no++] = n;
		}
	    }
	}
	System.out.println();
	System.out.print("odious : ");
	for (int n : od) {
	    System.out.printf("%d ", n);
	}
	System.out.println();
    }
}
```

```txt

32-bit integer: 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17
64-bit integer: 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
big integer   : 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil   : 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious : 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## Javascript



### ES6


```JavaScript
(() => {

    // popCount :: Int -> Int
    const popCount = n =>
        foldl(
            (a, x) => a + (x === '1' ? 1 : 0),
            0,
            splitOn('', showIntAsBinary(n))
        );

    // GENERIC FUNCTIONS ------------------------------------------------------

    // (++) :: [a] -> [a] -> [a]
    const append = (xs, ys) => xs.concat(ys);

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // length :: [a] -> Int
    const length = xs => xs.length;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // raise :: Num -> Int -> Num
    const raise = Math.pow;

    // showIntAsBinary :: Int -> String
    const showIntAsBinary = n => n.toString(2);

    // splitOn :: String -> String -> [String]
    const splitOn = (cs, xs) => xs.split(cs);

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    }

    // TEST -------------------------------------------------------------------

    // { popCounts : [Int], evenThenOdd : ([Int], [Int]) }
    return {
        popCounts: map(x => popCount(raise(3, x)), enumFromTo(0, 30)),
        evenThenOdd: until(
                m => length(m.evenOdd[0]) >= 30 && length(m.evenOdd[1]) >= 30,
                m => ({
                    x: m.x + 1,
                    evenOdd: popCount(m.x) % 2 === 0 ? (
                        [append(m.evenOdd[0], m.x), m.evenOdd[1]]
                    ) : [m.evenOdd[0], append(m.evenOdd[1], m.x)]
                }), {
                    x: 0,
                    evenOdd: [
                        [],
                        []
                    ]
                }
            )
            .evenOdd
    };
})();
```

```JavaScript
{"popCounts":[1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25, 25],
"evenThenOdd":[
[0, 3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57, 58],
[1, 2, 4, 7, 8, 11, 13, 14, 16, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59]]}
```



## jq

```jq
def popcount:
  def bin: recurse( if . == 0 then empty else ./2 | floor end ) % 2;
  [bin] | add;

def firstN(count; condition):
  if count > 0 then
    if condition then ., (1+.| firstN(count-1; condition))
    else (1+.) | firstN(count; condition)
    end
  else empty
  end;

def task:
  def pow(n): . as $m | reduce range(0;n) as $i (1; . * $m);

  "The pop count of the first thirty powers of 3:",
   [range(0;30) as $n | 3 | pow($n) | popcount],

  "The first thirty evil numbers:",
   [0 | firstN(30; (popcount % 2) == 0)],

  "The first thirty odious numbers:",
   [0 | firstN(30; (popcount % 2) == 1)]
;

task
```

 $ jq -n -r -c -f Population_count.jq
 The pop count of the first thirty powers of 3:
 [1,2,2,4,3,6,6,5,6,8,9,13,10,11,14,15,11,14,14,17,17,20,19,22,16,18,24,30,25,25]
 The first thirty evil numbers:
 [0,3,5,6,9,10,12,15,17,18,20,23,24,27,29,30,33,34,36,39,40,43,45,46,48,51,53,54,57,58]
 The first thirty odious numbers:
 [1,2,4,7,8,11,13,14,16,19,21,22,25,26,28,31,32,35,37,38,41,42,44,47,49,50,52,55,56,59]


## Julia

```julia
popcount(n) = sum(digits(n, 2))

println("First 3 ^ i, up to 29 pop. counts: ", join((popcount(3 ^ n) for n in 0:29), ", "))
println("Evil numbers: ", join(filter(x -> iseven(popcount(x)), 0:59), ", "))
println("Odious numbers: ", join(filter(x -> isodd(popcount(x)), 0:59), ", "))
```


```txt
First 3 ^ i, up to 29 pop. counts: 1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25
Evil numbers: 0, 3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57, 58
Odious numbers: 1, 2, 4, 7, 8, 11, 13, 14, 16, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59
```



## Kotlin


```scala
// version 1.0.6

fun popCount(n: Long) = when {
    n < 0L -> throw IllegalArgumentException("n must be non-negative")
    else   -> java.lang.Long.bitCount(n)
}

fun main(args: Array<String>) {
    println("The population count of the first 30 powers of 3 are:")
    var pow3 = 1L
    for (i in 1..30) {
        print("${popCount(pow3)} ")
        pow3 *= 3L
    }
    println("\n")
    println("The first thirty evil numbers are:")
    var count = 0
    var i = 0
    while (true) {
        val pc = popCount(i.toLong())
        if (pc % 2 == 0) {
           print("$i ")
           if (++count == 30) break
        }
        i++
    }
    println("\n")
    println("The first thirty odious numbers are:")
    count = 0
    i = 1
    while (true) {
        val pc = popCount(i.toLong())
        if (pc % 2 == 1) {
            print("$i ")
            if (++count == 30) break
        }
        i++
    }
    println()
}
```


```txt

The population count of the first 30 powers of 3 are:
1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25

The first thirty evil numbers are:
0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58

The first thirty odious numbers are:
1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## Lua


```Lua
-- Take decimal number, return binary string
function dec2bin (n)
    local bin, bit = ""
    while n > 0 do
        bit = n % 2
        n = math.floor(n / 2)
        bin = bit .. bin
    end
    return bin
end

-- Take decimal number, return population count as number
function popCount (n)
    local bin, count = dec2bin(n), 0
    for pos = 1, bin:len() do
        if bin:sub(pos, pos) == "1" then count = count + 1 end
    end
    return count
end

-- Implement task requirements
function firstThirty (mode)
    local numStr, count, n, remainder = "", 0, 0
    if mode == "Evil" then remainder = 0 else remainder = 1 end
    while count < 30 do
        if mode == "3^x" then
            numStr = numStr .. popCount(3 ^ count) .. " "
            count = count + 1
        else
            if popCount(n) % 2 == remainder then
                numStr = numStr .. n .. " "
                count = count + 1
            end
            n = n + 1
        end
    end
    print(mode .. ":" , numStr)
end

-- Main procedure
firstThirty("3^x")
firstThirty("Evil")
firstThirty("Odious")
```

```txt
3^x:    1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil:   0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
```



## Mathematica


```Mathematica
popcount[n_Integer] := IntegerDigits[n, 2] // Total
Print["population count of powers of 3"]
popcount[#] & /@ (3^Range[0, 30])
(*******)
evilQ[n_Integer] := popcount[n] // EvenQ
evilcount = 0;
evillist = {};
i = 0;
While[evilcount < 30,
 If[evilQ[i], AppendTo[evillist, i]; evilcount++];
 i++
 ]
Print["first thirty evil numbers"]
evillist
(*******)
odiousQ[n_Integer] := popcount[n] // OddQ
odiouscount = 0;
odiouslist = {};
i = 0;
While[odiouscount < 30,
 If[odiousQ[i], AppendTo[odiouslist, i]; odiouscount++];
 i++
 ]
Print["first thirty odious numbers"]
odiouslist
```

```txt
population count of powers of 3
{1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25, 25}
first thirty evil numbers
{0, 3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57, 58}
first thirty odious numbers
{1, 2, 4, 7, 8, 11, 13, 14, 16, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59}
```



## min

```min
(2 over over mod 'div dip) :divmod2

(
  :n () =list
  (n 0 >) (n divmod2 list append #list @n) while
  list (1 ==) filter size
) :pop-count

(:n 0 () (over swap append 'succ dip) n times) :iota

"3^n:    " print! 30 iota (3 swap pow int pop-count) map puts!
60 iota (pop-count odd?) partition
"Evil:   " print! puts! "Odious: " print! puts!
```

```txt

3^n:    (1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25)
Evil:   (0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58)
Odious: (1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59)

```



## Oforth


```oforth
: popcount(n)
   0 while ( n ) [ n isOdd + n bitRight(1) ->n ] ;

: test
| i count |
   30 seq map(#[ 3 swap 1- pow ]) map(#popcount) println

   0 ->count
   0 while( count 30 <> ) [ dup popcount isEven ifTrue: [ dup . count 1+ ->count ] 1+ ] drop printcr

   0 ->count
   0 while( count 30 <> ) [ dup popcount isOdd ifTrue: [ dup . count 1+ ->count ] 1+ ] drop ;
```


```txt

>test
[1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25]
0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59 ok

```



## PARI/GP


```parigp
vector(30,n,hammingweight(3^(n-1)))
od=select(n->hammingweight(n)%2,[0..100]); ev=setminus([0..100],od);
ev[1..30]
od[1..30]
```

```txt
%1 = [1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25]
%2 = [0, 3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57, 58]
%3 = [1, 2, 4, 7, 8, 11, 13, 14, 16, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59]
```



## Pascal

Like Ada a unit is used.

```pascal
unit popcount;
{$IFDEF FPC}
   {$MODE DELPHI}
   {$OPTIMIZATION ON,ASMCSE,CSE,PEEPHOLE}
   {$Smartlink OFF}
{$ENDIF}

interface
  function popcnt(n:Uint64):integer;overload;
  function popcnt(n:Uint32):integer;overload;
  function popcnt(n:Uint16):integer;overload;
  function popcnt(n:Uint8):integer;overload;

implementation
const
//K1  = $0101010101010101;
  K33  = $3333333333333333;
  K55  = $5555555555555555;
  KF1 = $0F0F0F0F0F0F0F0F;
  KF2 = $00FF00FF00FF00FF;
  KF4 = $0000FFFF0000FFFF;
  KF8 = $00000000FFFFFFFF;
{
function popcnt64(n:Uint64):integer;
begin
  n := n- (n shr 1) AND K55;
  n := (n AND K33)+ ((n shr 2) AND K33);
  n := (n + (n shr 4)) AND KF1;
  n := (n*k1) SHR 56;
  result := n;
end;
}
function popcnt(n:Uint64):integer;overload;
// on Intel Haswell 2x faster for fpc 32-Bit
begin
  n := (n AND K55)+((n shr  1)  AND K55);
  n := (n AND K33)+((n shr  2)  AND K33);
  n := (n AND KF1)+((n shr  4)  AND KF1);
  n := (n AND KF2)+((n shr  8)  AND KF2);
  n := (n AND KF4)+((n shr 16)  AND KF4);
  n := (n AND KF8)+ (n shr 32);
  result := n;
end;

function popcnt(n:Uint32):integer;overload;
var
  c,b : NativeUint;
begin
  b := n;
  c := (b shr 1) AND NativeUint(K55);   b := (b AND NativeUint(K55))+C;
  c := ((b shr 2)  AND NativeUint(K33));b := (b AND NativeUint(K33))+C;
  c:= ((b shr 4)  AND NativeUint(KF1)); b := (b AND NativeUint(KF1))+c;
  c := ((b shr 8)  AND NativeUint(KF2));b := (b AND NativeUint(KF2))+c;
  c := b shr 16; b := (b AND NativeUint(KF4))+ C;
  result := b;
end;

function popcnt(n:Uint16):integer;overload;
var
  c,b : NativeUint;
begin
  b := n;
  c := (b shr 1) AND NativeUint(K55);  b := (b AND NativeUint(K55))+C;
  c :=((b shr 2)  AND NativeUint(K33)); b := (b AND NativeUint(K33))+C;
  c:= ((b shr 4)  AND NativeUint(KF1)); b := (b AND NativeUint(KF1))+c;
  c :=  b shr 8; b := (b AND NativeUint(KF2))+c;
  result := b;
end;

function popcnt(n:Uint8):integer;overload;
var
  c,b : NativeUint;
begin
  b := n;
  c := (b shr 1) AND NativeUint(K55);  b := (b AND NativeUint(K55))+C;
  c :=((b shr 2)  AND NativeUint(K33));b := (b AND NativeUint(K33))+C;
  c:=   b shr 4;
  result := (b AND NativeUint(KF1))+c;
end;

Begin
End.
```

The program

```pascal
program pcntTest;
uses
  sysutils,popCount;

function Odious(n:Uint32):boolean;inline;
Begin
  Odious := boolean(PopCnt(n) AND 1)
end;

function EvilNumber(n:Uint32):boolean;inline;
begin
  EvilNumber := boolean(NOT(PopCnt(n)) AND 1);
end;

var
  s : String;
  i : Uint64;
  k : LongWord;
Begin
  s :='PopCnt 3^i     :';
  i:= 1;
  For k := 1 to 30 do
  Begin
    s := s+InttoStr(PopCnt(i)) +' ';
    i := 3*i;
  end;
  writeln(s);writeln;

  s:='Evil numbers   :';i := 0;k := 0;
  repeat
    IF EvilNumber(i) then
    Begin
      inc(k);s := s+InttoStr(i) +' ';
    end;
    inc(i);
  until k = 30;
  writeln(s);writeln;s:='';


  s:='Odious numbers :';i := 0;k := 0;
  repeat
    IF Odious(i) then
    Begin
      inc(k);s := s+InttoStr(i) +' ';
    end;
    inc(i);
  until k = 30;
  writeln(s);
end.
```

;Output:

```txt
PopCnt 3^i     :1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil numbers   :0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious numbers :1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```


Some processors define the <code>card</code> function, which can be used in conjunction with sets:

```pascal
var
	i: integer;
	f: set of 0..(bitSizeOf(i)-1) absolute i; // same address as i, but different interpretation
begin
	writeLn(card(f));
end;
```



## Perl

We'll emulate infinite lists with closures.


```perl
use strict;
use warnings;

sub population_count {
    my $n = shift;
    die "argument can't be negative" if $n < 0;
    my $c;
    for ($c = 0; $n; $n >>= 1) {
        $c += $n & 1;
    }
    $c;
}

print join ' ', map { population_count(3**$_) } 0 .. 30 - 1;
print "\n";
sub evil {
    my $i = 0;
    sub { $i++ while population_count($i) % 2; $i++ }
}
sub odious {
    my $i = 0;
    sub { $i++ until population_count($i) % 2; $i++ }
}

my ($evil, $odious) = (evil, odious);
my (@evil, @odious);
for (1 .. 30) {
    push @evil, $evil->();
    push @odious, $odious->();
}

printf "Evil  : %s\n", join ' ', @evil;
printf "Odious: %s\n", join ' ', @odious;
```

```txt
1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil  : 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
```



A faster population count can be done with pack/unpack:
<lang>say unpack("%b*",pack "J*", 1234567); # J = UV
```


Various modules can also perform a population count, with the first of these being faster than the pack/unpack builtins.  The first three easily support bigints, the last will with some adjustment.

```perl
use ntheory qw/hammingweight/;
say hammingweight(1234567);

use Math::GMPz qw/Rmpz_popcount/;
say Rmpz_popcount(Math::GMPz->new(1234567));

use Math::BigInt;
say 0 + (Math::BigInt->new(1234567)->as_bin() =~ tr/1//);

use Bit::Vector;
say Bit::Vector->new_Dec(64,1234567)->Norm;
```



## Perl 6


```perl6
sub population-count(Int $n where * >= 0) { [+] $n.base(2).comb }

say map &population-count, 3 «**« ^30;
say "Evil: ", (grep { population-count($_) %% 2 }, 0 .. *)[^30];
say "Odious: ", (grep { population-count($_)  % 2 }, 0 .. *)[^30];
```

```txt
1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil: 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
```

That's the convenient way to write it, but the following avoids string processing and is therefore about twice as fast:

```perl6
sub population-count(Int $n is copy where * >= 0) {
    loop (my $c = 0; $n; $n +>= 1) {
        $c += $n +& 1;
    }
    $c;
}
```



## Phix


```Phix
function pop_count(atom n)
    if n<0 then ?9/0 end if
    integer res = 0
    while n!=0 do
        res += and_bits(n,1)
        n = floor(n/2)
    end while
    return res
end function

sequence s = {}
for i=0 to 29 do
    s &= pop_count(power(3,i))
end for
puts(1,"3^x pop_counts:") ?s

procedure eo(integer b0, string name)
    integer k=0, l=1
    while l<=30 do
        if and_bits(pop_count(k),1)=b0 then
            s[l] = k
            l += 1
        end if
        k += 1
    end while
    puts(1,name&" numbers:") ?s
end procedure
eo(0,"  evil")
eo(1,"odious")
```

```txt

3^x pop_counts:{1,2,2,4,3,6,6,5,6,8,9,13,10,11,14,15,11,14,14,17,17,20,19,22,16,18,24,30,25,25}
  evil numbers:{0,3,5,6,9,10,12,15,17,18,20,23,24,27,29,30,33,34,36,39,40,43,45,46,48,51,53,54,57,58}
odious numbers:{1,2,4,7,8,11,13,14,16,19,21,22,25,26,28,31,32,35,37,38,41,42,44,47,49,50,52,55,56,59}

```



## PicoLisp


```PicoLisp
(de popz (N)
   (cnt
      '((N) (= "1" N))
      (chop (bin N)) ) )

(println
   'pops:
   (mapcar
      '((N) (popz (** 3 N)))
      (range 0 29) ) )
(setq N -1)
(println
   'evil:
   (make
      (for (C 0 (> 30 C))
         (unless (bit? 1 (popz (inc 'N)))
            (link N)
            (inc 'C) ) ) ) )
(setq N -1)
(println
   'odio:
   (make
      (for (C 0 (> 30 C))
         (when (bit? 1 (popz (inc 'N)))
            (link N)
            (inc 'C) ) ) ) )
```

```txt

pops: (1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25)
evil: (0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58)
odio: (1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59)

```



## PHP


```PHP

function convertToBinary($integer) {
    $binary = "";

    do {
        $quotient = (int) ($integer / 2);
        $binary .= $integer % 2;
        $integer = $quotient;
    } while ($quotient > 0);

    return $binary;
}

function getPopCount($integer) {
    $binary = convertToBinary($integer);
    $offset = 0;
    $popCount = 0;

    do {
        $pos = strpos($binary, "1", $offset);
        if($pos !== FALSE) $popCount++;
        $offset = $pos + 1;
    } while ($pos !== FALSE);

    return $popCount;
}

function print3PowPopCounts() {
    for ($p = 0; $p < 30; $p++) {
        echo " " . getPopCount(3 ** $p);
    }
}

function printFirst30Evil() {
    $counter = 0;
    $pops = 0;

    while ($pops < 30) {
        $popCount = getPopCount($counter);
        if ($popCount % 2 == 0)  {
            echo " " . $counter;
            $pops++;
        }
        $counter++;
    }
}

function printFirst30Odious() {
    $counter = 1;
    $pops = 0;

    while ($pops < 30) {
        $popCount = getPopCount($counter);
        if ($popCount % 2 != 0)  {
            echo " " . $counter;
            $pops++;
        }
        $counter++;
    }
}

echo "3 ^ x pop counts:";
print3PowPopCounts();

echo "\nfirst 30 evil numbers:";
printFirst30Evil();

echo "\nfirst 30 odious numbers:";
printFirst30Odious();

```

```txt

03 ^ x pop counts: 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
first 30 evil numbers: 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
first 30 odious numbers: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```





## PowerShell


```PowerShell

function pop-count($n) {
    (([Convert]::ToString($n, 2)).toCharArray() | where {$_ -eq '1'}).count
}
"pop_count 3^n: $(1..29 | foreach -Begin {$n = 1; (pop-count $n)} -Process {$n = 3*$n; (pop-count $n)} )"
"even pop_count: $($m = $n = 0; while($m -lt 30) {if(0 -eq ((pop-count $n)%2)) {$m += 1; $n}; $n += 1} )"
"odd pop_count: $($m = $n = 0; while($m -lt 30) {if(1 -eq ((pop-count $n)%2)) {$m += 1; $n}; $n += 1} )"

```

<b>Output:</b>

```txt

pop_count 3^n: 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
even pop_count: 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odd pop_count: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## Python


### Procedural


```python>>>
 def popcount(n): return bin(n).count("1")
...
>>> [popcount(3**i) for i in range(30)]
[1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25]
>>> evil, odious, i = [], [], 0
>>> while len(evil) < 30 or len(odious) < 30:
...     p = popcount(i)
...     if p % 2: odious.append(i)
...     else: evil.append(i)
...     i += 1
...
>>> evil[:30]
[0, 3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57, 58]
>>> odious[:30]
[1, 2, 4, 7, 8, 11, 13, 14, 16, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59]
>>>
```



### Composition of pure functions

```python
'''Population count'''

from functools import reduce


# popCount :: Int -> Int
def popCount(n):
    '''The count of non-zero digits in the binary
       representation of the positive integer n.'''
    def go(x):
        return Just(divmod(x, 2)) if 0 < x else Nothing()
    return sum(unfoldl(go)(n))


# TEST ----------------------------------------------------
def main():
    '''Tests'''

    print('Population count of first 30 powers of 3:')
    print(
        [popCount(pow(3, x)) for x in enumFromTo(0)(29)]
    )

    evilNums, odiousNums = partition(
        compose(even)(popCount)
    )(enumFromTo(0)(59))

    print("\nFirst thirty 'evil' numbers:")
    print(evilNums)

    print("\nFirst thirty 'odious' numbers:")
    print(odiousNums)


# GENERIC -------------------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# even :: Int -> Bool
def even(x):
    '''True if x is a
       multiple of two.'''
    return 0 == x % 2


# partition :: (a -> Bool) -> [a] -> ([a], [a])
def partition(p):
    '''The pair of lists of those elements in xs
       which respectively, do and don't
       satisfy the predicate p.'''
    def go(a, x):
        ts, fs = a
        return (ts + [x], fs) if p(x) else (ts, fs + [x])
    return lambda xs: reduce(go, xs, ([], []))


# unfoldl(lambda x: Just(((x - 1), x)) if 0 != x else Nothing())(10)
# -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
def unfoldl(f):
    '''Dual to reduce or foldr.
       Where catamorphism reduces a list to a summary value,
       the anamorphic unfoldr builds a list from a seed value.
       As long as f returns Just(a, b), a is prepended to the list,
       and the residual b is used as the argument for the next
       application of f.
       When f returns Nothing, the completed list is returned.'''
    def go(xr):
        mb = f(xr[0])
        if mb.get('Nothing'):
            return []
        else:
            y, r = mb.get('Just')
            return go((y, r)) + [r]

    return lambda x: go((x, x))


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Population count of first 30 powers of 3:
[1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25]

First thirty 'evil' numbers:
[0, 3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57, 58]

First thirty 'odious' numbers:
[1, 2, 4, 7, 8, 11, 13, 14, 16, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59]
```



## Racket


```racket
#lang racket
;; Positive version from "popcount_4" in:
;;   https://en.wikipedia.org/wiki/Hamming_weight#Efficient_implementation
;; negative version follows R6RS definition documented in:
;;   http://docs.racket-lang.org/r6rs/r6rs-lib-std/r6rs-lib-Z-H-12.html?q=bitwise-bit#node_idx_1074
(define (population-count n)
  (if (negative? n)
      (bitwise-not (population-count (bitwise-not n)))
      (let inr ((x n) (rv 0))
        (if (= x 0) rv (inr (bitwise-and x (sub1 x)) (add1 rv))))))

(define (evil? x)
  (and (not (negative? x))
       (even? (population-count x))))

(define (odious? x)
  (and (positive? x)
       (odd? (population-count x))))

(define tasks
  (list
   "display the pop count of the 1st thirty powers of 3 (3^0, 3^1, 3^2, 3^3, 3^4, ...)."
   (for/list ((i (in-range 30))) (population-count (expt 3 i)))
   "display the 1st thirty evil numbers."
   (for/list ((_ (in-range 30)) (e (sequence-filter evil? (in-naturals)))) e)
   "display the 1st thirty odious numbers."
   (for/list ((_ (in-range 30)) (o (sequence-filter odious? (in-naturals)))) o)))

(for-each displayln tasks)

(module+ test
  (require rackunit)
  (check-equal?
   (for/list ((p (sequence-map population-count (in-range 16)))) p)
   '(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4))
  (check-true (evil? 0) "0 has just *got* to be evil")
  (check-true (evil? #b011011011) "six bits... truly evil")
  (check-false (evil? #b1011011011) "seven bits, that's odd!")
  (check-true (odious? 1) "the least odious number")
  (check-true (odious? #b1011011011) "seven (which is odd) bits")
  (check-false (odious? #b011011011) "six bits... is evil"))
```

```txt

display the pop count of the 1st thirty powers of 3 (3^0, 3^1, 3^2, 3^3, 3^4, ...).
(1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25)
display the 1st thirty evil numbers.
(0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58)
display the 1st thirty odious numbers.
(1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59)

```



## REXX


```rexx
/*REXX program counts the number of "one" bits in the binary version of a decimal number*/
/*─────────────────── and also generates a specific number of  EVIL and ODIOUS  numbers.*/
parse arg N B .                                  /*get optional arguments from the C.L. */
if N==''  |  N==","   then N=30                  /*N not specified?   Then use default. */
if B==''  |  B==","   then B= 3                  /*B  "      "          "   "      "    */
numeric digits 2000                              /*be able to handle  gihugeic  numbers.*/
numeric digits max(20, length(B**N) )            /*whittle the  precision  down to size.*/
$=                                               /* [↑]  a little calculation for sizing*/
     do j=0  for  N;   $=$ popcount(B**j)        /*generate N popCounts for some powers.*/
     end   /*j*/                                 /* [↑]  append popCount to the $ list. */
                                                 /* [↓]  display popcounts of "3" powers*/
call showList  'popcounts of the powers of'  B   /*display the list with a header/title.*/

     do j=0  until  #>=N                         /*generate   N   evil  numbers.        */
     if popCount(j) // 2  then iterate           /*if  odd population count, skip it.   */
     #=# + 1;       $=$ j                        /*bump evil # count;  add it to $ list.*/
     end   /*j*/                                 /* [↑]  build a list of evil numbers.  */
                                                 /* [↓]  display the evil number list.  */
call showList  'evil numbers'                    /*display the  $  list with a header.  */

     do j=0  until  #>=N                         /*generate   N   odious  numbers.      */
     if popCount(j) // 2 ==0  then iterate       /*if even population count, then skip. */
     #=# + 1;       $=$ j                        /*bump odious # count;  add to $ list. */
     end   /*j*/                                 /* [↑]  build a list of odious numbers.*/
                                                 /* [↓]  display the odious number list.*/
call showList  'odious numbers'                  /*display the   $  list with a header. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
d2b:      return word( strip( x2b( d2x( arg(1) ) ), 'L', 0)  0, 1)        /*dec ──► bin.*/
popCount: return length( space( translate( d2b(arg(1) ), , 0), 0) )       /*count ones. */
showList: say;   say 'The 1st'   N   arg(1)':';   say strip($);     #=0;     $=;    return
```

```txt

The 1st 30 popcounts of the powers of 3:
1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25

The 1st 30 evil numbers:
0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58

The 1st 30 odious numbers:
1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## Ring


```ring

# Project : Population count

load "stdlib.ring"
n = 0
neven = 0
nodd = 0
binodd = []
bineven = []
binpow = []
while true
        n = n + 1
        numb = 0
        bin = binarydigits(n)
        for nr = 1 to len(bin)
             if bin[nr] = "1"
                numb = numb + 1
             ok
        next
        if numb % 2 = 0
           neven = neven + 1
           if neven < 31
              add(bineven, n)
           ok
        else
           nodd = nodd + 1
           if nodd < 31
              add(binodd, n)
           ok
        ok
        if neven > 30 and nodd > 30
           exit
        ok
end

see "3^x:" + nl
for n = 0 to 29
      numb = 0
      bin = binarydigits(pow(3,n))
      for nr = 1 to len(bin)
            if bin[nr] = "1"
                numb = numb + 1
            ok
      next
      add(binpow, numb)
next
showarray(binpow)
see nl

see "Evil numbers :" + nl
showarray(bineven)
see nl
see "Odious numbers:" + nl
showarray(binodd)
see nl

func showarray(vect)
        see "["
        svect = ""
        for n = 1 to len(vect)
              svect = svect + vect[n] + ", "
        next
        svect = left(svect, len(svect) - 2)
        see svect
        see "]" + nl

```

Output:

```txt

3^x:
[1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25]

Evil numbers :
[3, 4, 5, 6, 9, 10, 12, 15, 16, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57]

Odious numbers:
[1, 2, 7, 8, 11, 13, 14, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59, 61, 62]

```



## Ruby

Demonstrating lazy enumerators.

```ruby
class Integer

  def popcount
    digits(2).count(1)     #pre Ruby 2.4: self.to_s(2).count("1")
  end

  def evil?
    self >= 0 && popcount.even?
  end

end

puts "Powers of 3:",  (0...30).map{|n| (3**n).popcount}.join(' ')
puts "Evil:"  , 0.step.lazy.select(&:evil?).first(30).join(' ')
puts "Odious:", 0.step.lazy.reject(&:evil?).first(30).join(' ')
```

```txt

Powers of 3:
1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil:
0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious:
1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```


=={{Header|Rust}}==

```Rust
fn main() {
    let mut num = 1u64;
    let mut vec = Vec::new();
    for _ in 0..30 {
        vec.push(num.count_ones());
        num *= 3;
    }
    println!("pop count of 3^0, 3^1 ... 3^29:\n{:?}",vec);
    let mut even = Vec::new();
    let mut odd  = Vec::new();
    num = 1;
    while even.len() < 30 || odd.len() < 30 {
        match 0 == num.count_ones()%2 {
            true if even.len() < 30 => even.push(num),
            false if odd.len() < 30 => odd.push(num),
            _                       => {}
        }
        num += 1;
    }
    println!("\nFirst 30 even pop count:\n{:?}",even);
    println!("\nFirst 30 odd pop count:\n{:?}",odd);
}
```

```txt
pop count of 3^0, 3^1 ... 3^29:
[1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25]

First 30 even pop count:
[3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57, 58, 60]

First 30 odd pop count:
[1, 2, 4, 7, 8, 11, 13, 14, 16, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59]
```

=={{Header|Scala}}==
{{Out}}See it yourself by running in your browser either by [https://scalafiddle.io/sf/1IYuvtd/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/w0oHalRXS1mtI59tXh1NaA Scastie (remote JVM)].
```Scala
import java.lang.Long.bitCount

object PopCount extends App {
  val nNumber = 30

  def powersThree(start: Long): LazyList[Long] = start #:: powersThree(start * 3L)

  println("Population count of 3ⁿ :")
  println(powersThree(1L).map(bitCount).take(nNumber).mkString(", "))

  def series(start: Long): LazyList[Long] = start #:: series(start + 1L)

  println("Evil numbers:")
  println(series(0L).filter(bitCount(_) % 2 == 0).take(nNumber).mkString(", "))

  println("Odious numbers:")
  println(series(0L).filter(bitCount(_) % 2 != 0).take(nNumber).mkString(", "))

}
```



## Seed7

The function <code>popcount</code> below [http://seed7.sourceforge.net/libraries/bitset.htm#bitset(in_integer) converts]
the integer into a [http://seed7.sourceforge.net/libraries/bitset.htm bitset].
The function [http://seed7.sourceforge.net/libraries/bitset.htm#card(in_bitset) card]
is used to compute the population count of the bitset.


```seed7
$ include "seed7_05.s7i";

const func integer: popcount (in integer: n) is
    return card(bitset(n));

const proc: main is func
  local
    var integer: count is 0;
    var integer: num is 0;
  begin
    for num range 0 to 29 do
      write(popcount(3 ** num) <& " ");
    end for;
    writeln;
    write("evil:   ");
    for num range 0 to integer.last until count >= 30 do
      if not odd(popcount(num)) then
        write(num <& " ");
	incr(count);
      end if;
    end for;
    writeln;
    write("odious: ");
    count := 0;
    for num range 0 to integer.last until count >= 30 do
      if odd(popcount(num)) then
        write(num <& " ");
        incr(count);
      end if;
    end for;
    writeln;
  end func;
```

```txt
1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil:   0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
```



## Sidef


```ruby
func population_count(n) { n.as_bin.count('1') }
say "#{0..29 «**« 3 «call« population_count -> join(' ')}"
 
var numbers = 60.of { |i|
    [i, population_count(i)]
}
 
say "Evil:   #{numbers.grep{_[1] %% 2}.map{.first}.join(' ')}"
say "Odious: #{numbers.grep{_[1] &  1}.map{.first}.join(' ')}"
```

```txt

1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil:   0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## Swift


```swift
func populationCount(n: Int) -> Int {
  guard n >= 0 else { fatalError() }

  return String(n, radix: 2).filter({ $0 == "1" }).count
}

let pows = (0...)
    .lazy
    .map({ Int(pow(3, Double($0))) })
    .map(populationCount)
    .prefix(30)

let evils = (0...)
    .lazy
    .filter({ populationCount(n: $0) & 1 == 0 })
    .prefix(30)

let odious = (0...)
    .lazy
    .filter({ populationCount(n: $0) & 1 == 1 })
    .prefix(30)

print("Powers:", Array(pows))
print("Evils:", Array(evils))
print("Odious:", Array(odious))
```


```txt
Powers: [1, 2, 2, 4, 3, 6, 6, 5, 6, 8, 9, 13, 10, 11, 14, 15, 11, 14, 14, 17, 17, 20, 19, 22, 16, 18, 24, 30, 25, 25]
Evils: [0, 3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, 29, 30, 33, 34, 36, 39, 40, 43, 45, 46, 48, 51, 53, 54, 57, 58]
Odious: [1, 2, 4, 7, 8, 11, 13, 14, 16, 19, 21, 22, 25, 26, 28, 31, 32, 35, 37, 38, 41, 42, 44, 47, 49, 50, 52, 55, 56, 59]
```



## Tcl

```tcl
package require Tcl 8.6

proc hammingWeight {n} {
    tcl::mathop::+ {*}[split [format %llb $n] ""]
}
for {set n 0;set l {}} {$n<30} {incr n} {
    lappend l [hammingWeight [expr {3**$n}]]
}
puts "p3: $l"
for {set n 0;set e [set o {}]} {[llength $e]<30||[llength $o]<30} {incr n} {
    lappend [expr {[hammingWeight $n]&1 ? "o" : "e"}] $n
}
puts "evil: [lrange $e 0 29]"
puts "odious: [lrange $o 0 29]"
```

```txt

p3: 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil: 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## UNIX Shell

```bash
popcount() {
    local -i n=$1
    (( n < 0 )) && return 1
    local ones=0
    while (( n > 0 )); do
        (( ones += n%2 ))
        (( n /= 2 ))
    done
    echo $ones
}

popcount_3s=()
n=1
for (( i=0; i<30; i++ )); do
    popcount_3s+=( $(popcount $n) )
    (( n *= 3 ))
done
echo "powers of 3 popcounts: ${popcount_3s[*]}"

evil=()
odious=()
n=0
while (( ${#evil[@]} < 30 || ${#odious[@]} < 30 )); do
    p=$( popcount $n )
    if (( $p%2 == 0 )); then
        evil+=( $n )
    else
        odious+=( $n )
    fi
    (( n++ ))
done
echo "evil nums:   ${evil[*]:0:30}"
echo "odious nums: ${odious[*]:0:30}"
```

```txt
powers of 3 popcounts: 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil nums:   0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious nums: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59
```




## VBA

The Decimal subtype of Variant does the job to expand 32-bit integers (Long) to 28-digit integers (Decimal).

```vb
Sub Population_count()
    nmax = 30
    b = 3
    n = 0: List = "": bb = 1
    For i = 0 To nmax - 1
        List = List & " " & popcount(bb)
        bb = bb * b
    Next 'i
    Debug.Print "popcounts of the powers of " & b
    Debug.Print List
    For j = 0 To 1
        If j = 0 Then c = "evil numbers" Else c = "odious numbers"
        n = 0: List = "": i = 0
        While n < nmax
            If (popcount(i) Mod 2) = j Then
                n = n + 1
                List = List & " " & i
            End If
            i = i + 1
        Wend
        Debug.Print c
        Debug.Print List
    Next 'j
End Sub 'Population_count

Private Function popcount(x)
    Dim y, xx, xq, xr
    xx = x
    While xx > 0
        xq = Int(xx / 2)
        xr = xx - xq * 2
        If xr = 1 Then y = y + 1
        xx = xq
    Wend
    popcount = y
End Function 'popcount
```

```txt

popcounts of the powers of 3:
 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil numbers:
 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious numbers:
' 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```




## VBScript

Use of the variant currency subtype. Currency mode is a gray area where some operators do not work,
for instance: ^ \ Mod

```vb
' Population count - VBScript - 10/05/2019
	nmax=30
	b=3
	n=0: list="": bb=1
	For i=0 To nmax-1
		list=list & " " & popcount(bb)
		bb=bb*b
	Next 'i
	Msgbox list,,"popcounts of the powers of " & b
	For j=0 to 1
		If j=0 Then c="evil numbers": Else c="odious numbers"
		n=0: list="": i=0
		While n<nmax
			If (popcount(i) Mod 2)=j Then
				n=n+1
				list=list & " " & i
			End If
			i=i+1
		Wend
		Msgbox list,,c
	Next 'j

Function popcount(x)
	Dim y,xx,xq,xr
	xx=x
	While xx>0
		xq=Int(xx/2)
		xr=xx-xq*2
		If xr=1 Then y=y+1
		xx=xq
	Wend
	popcount=y
End Function 'popcount
```

```txt

popcounts of the powers of 3:
 1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
evil numbers:
 0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
odious numbers:
 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

```



## Visual Basic .NET

```vbnet
Module Module1

    Function PopCnt(ByVal n As Long) As Integer
        Return Convert.ToString(n, 2).ToCharArray().Where(Function(x) x = "1").Count()
    End Function

    Sub Aline(ByVal a As List(Of Integer), ByVal title As String)
        Console.WriteLine("{0, -8}{1}", title, String.Join(" ", a.Take(30)))
    End Sub

    Sub Main(ByVal args As String())
        Console.WriteLine("Population Counts:")
        Dim t As New List(Of Integer), e As New List(Of Integer), o As New List(Of Integer)
        For count As Integer = 0 To 99
            If PopCnt(count) Mod 2 = 0 Then e.Add(count) Else o.Add(count)
            If count < 30 Then t.Add(PopCnt(Math.Pow(3, count)))
        Next
        Aline(t, "3^n :") : Aline(e, "Evil:") : Aline(o, "Odious:")
''' Extra:
        Dim eo As Boolean = e.Contains(0), res As String = "", i As Integer = 0
        e.Add(0) : o.Add(0)
        Do
            If eo Then
                If e(i + 1) = e(i) + 1 Then
                    res += "͞ " : i += 1
                ElseIf o(i) = e(i) + 1 Then
                    res += "↓" : eo = Not eo
                Else
                    res += "\" : eo = Not eo : i += 1
                End If
            Else
                If o(i + 1) = o(i) + 1 Then
                    res += "͢ " : i += 1
                ElseIf e(i) = o(i) + 1 Then
                    res += "↑" : eo = Not eo
                Else
                    res += "/" : eo = Not eo : i += 1
                End If
            End If
        Loop Until i >= e.Count - 1
        Console.WriteLine(vbLf & "Pattern:{0}", res.Substring(0, res.Count() - 1))
        If System.Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub
End Module

```

Added a "Pattern" line.  Not quite getting the arrows I wanted, but the "Pattern" line shows the sequence pattern of integers for the Evil and Odious output.  The pattern goes to about 50, whereas only the first 30 Evil and Odious integers are shown.

```txt
Population Counts:
3^n :   1 2 2 4 3 6 6 5 6 8 9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
Evil:   0 3 5 6 9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
Odious: 1 2 4 7 8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

Pattern:↓͢ ↑\↑͞ ↓͢ ↑͞ ↓/↓͢ ↑\↑͞ ↓/↓͢ ↑͞ ↓͢ ↑\↑͞ ↓͢ ↑͞ ↓/↓͢ ↑͞ ↓͢ ↑\↑͞ ↓/↓͢ ↑\↑͞ ↓͢ ↑͞ ↓/↓͢ ↑\↑͞ ↓/↓͢ ↑͞ ↓͢ ↑\↑͞ ↓/↓͢ ↑\↑͞ ↓͢ ↑͞ ↓/↓͢ ↑͞ ↓͢ ↑
```
'''P.S.''', The underscore-right-arrows and overscore characters on the Pattern line may not appear properly on some browsers.



## Yabasic


```Yabasic
print "Pop count (3^x): "

for i = 0 to 29
    print population(3^i);
next
print "\n"

print "Evil: "
EvilOdious(30)
print "\n"

print "Odious: "
EvilOdious(30, 1)
print "\n"

sub EvilOdious(limit, type)
    local i, count, eo

    repeat
        eo = mod(population(i), 2)
        if (type and eo) or (not type and not eo) count = count + 1 : print i;
        i = i + 1
    until(count = limit)
end sub

sub population(number)
    local i, binary$, popul

    binary$ = bin$(number)
    for i = 1 to len(binary$)
        popul = popul + val(mid$(binary$, i, 1))
    next
    return popul
end sub
```



## zkl

Ints have the 1s count as a property.

```zkl
n:=1; do(30){ print(n.num1s,","); n*=3 } println();

println("evil: ",[0..].filter(30,fcn(n){ n.num1s.isEven }).concat(","));

// now, as an iterator aka lazy:
println("odious: ",(0).walker(*).tweak(   // 0,1,2,3,4... iterator
   fcn(n){ if(n.num1s.isEven) Void.Skip else n }).walk(30).concat(","));
```

```txt

1,2,2,4,3,6,6,5,6,8,9,13,10,11,14,15,11,14,14,17,17,20,19,22,16,18,24,30,25,25,
evil: 0,3,5,6,9,10,12,15,17,18,20,23,24,27,29,30,33,34,36,39,40,43,45,46,48,51,53,54,57,58
odious: 1,2,4,7,8,11,13,14,16,19,21,22,25,26,28,31,32,35,37,38,41,42,44,47,49,50,52,55,56,59

```

