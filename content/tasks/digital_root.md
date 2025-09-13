+++
title = "Digital root"
description = ""
date = 2019-10-17T19:24:59Z
aliases = []
[extra]
id = 12061
[taxonomies]
categories = ["task"]
tags = []
+++

The digital root, <math>X</math>, of a number, <math>n</math>, is calculated:
: find <math>X</math> as the sum of the digits of <math>n</math>
: find a new <math>X</math> by summing the digits of <math>X</math>, repeating until <math>X</math> has only one digit.

The additive persistence is the number of summations required to obtain the single digit.

The task is to calculate the additive persistence and the digital root of a number, e.g.:
:<math>627615</math> has additive persistence <math>2</math> and digital root of <math>9</math>;
:<math>39390</math> has additive persistence <math>2</math> and digital root of <math>6</math>;
:<math>588225</math> has additive persistence <math>2</math> and digital root of <math>3</math>;
:<math>393900588225</math> has additive persistence <math>2</math> and digital root of <math>9</math>;

The digital root may be calculated in bases other than 10.


;See:
* [[Casting out nines]] for this wiki's use of this procedure.
* [[Digital root/Multiplicative digital root]]
* [[Sum digits of an integer]]
* [[oeis:A010888|Digital root sequence on OEIS]]
* [[oeis:A031286|Additive persistence sequence on OEIS]]
* [[Iterated digits squaring]]





## 11l

```11l
F digital_root(=n)
   V ap = 0
   L n >= 10
      n = sum(String(n).map(digit -> Int(digit)))
      ap++
   R (ap, n)

L(n) [Int64(627615), 39390, 588225, 393900588225, 55]
   Int64 persistance, root
   (persistance, root) = digital_root(n)
   print(‘#12 has additive persistance #2 and digital root #..’.format(n, persistance, root))
```

```txt

      627615 has additive persistance  2 and digital root 9.
       39390 has additive persistance  2 and digital root 6.
      588225 has additive persistance  2 and digital root 3.
393900588225 has additive persistance  2 and digital root 9.
          55 has additive persistance  2 and digital root 1.

```



## 360 Assembly


```360asm
*        Digital root              21/04/2017
DIGROOT  CSECT
         USING  DIGROOT,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,=A((PG-T)/4))  do i=1 to hbound(t)
         LR     R1,R6                i
         SLA    R1,2                 *4
         L      R10,T-4(R1)          nn=t(i)
         LR     R7,R10               n=nn
         SR     R9,R9                ap=0
       DO WHILE=(C,R7,GE,=A(10))     do while(n>=10)
         SR     R8,R8                  x=0
       DO WHILE=(C,R7,GE,=A(10))       do while(n>=10)
         LR     R4,R7                    n
         SRDA   R4,32                    >>r5
         D      R4,=A(10)                m=n//10
         LR     R7,R5                    n=n/10
         AR     R8,R4                    x=x+m
       ENDDO    ,                      end
         AR     R7,R8                  n=x+n
         LA     R9,1(R9)               ap=ap+1
       ENDDO    ,                    end
         XDECO  R10,XDEC             nn
         MVC    PG+7(10),XDEC+2
         XDECO  R9,XDEC              ap
         MVC    PG+31(3),XDEC+9
         XDECO  R7,XDEC              n
         MVC    PG+41(1),XDEC+11
         XPRNT  PG,L'PG              print
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
T        DC     F'627615',F'39390',F'588225',F'2147483647'
PG       DC     CL80'number=xxxxxxxxxx  persistence=xxx  root=x'
XDEC     DS     CL12
         YREGS
         END    DIGROOT
```

```txt

number=    627615  persistence=  2  root=9
number=     39390  persistence=  2  root=6
number=    588225  persistence=  2  root=3
number=2147483647  persistence=  3  root=1

```




## Ada

We first specify a Package "Generic_Root" with a generic procedure "Compute". The package is reduced for the implementation of multiplicative digital roots [[http://rosettacode.org/wiki/Digital_root/Multiplicative_digital_root#Ada]]. Further note the tunable parameter for the number base (default 10).


```Ada
package Generic_Root is
   type Number is range 0 .. 2**63-1;
   type Number_Array is array(Positive range <>) of Number;
   type Base_Type is range 2 .. 16; -- any reasonable base to write down numb

   generic
      with function "&"(X, Y: Number) return Number;
      -- instantiate with "+" for additive digital roots
      -- instantiate with "*" for multiplicative digital roots
   procedure Compute_Root(N: Number;
                     Root, Persistence: out Number;
                     Base: Base_Type := 10);
   -- computes Root and Persistence of N;

end Generic_Root;
```


The implementation is straightforward: If the input N is a digit, then the root is N and the persistence is zero. Else, commute the digit-sum DS. The root of N is the root of DS, the persistence of N is 1 + (the persistence of DS).


```Ada
package body Generic_Root is

   procedure Compute_Root(N: Number;
                     Root, Persistence: out Number;
                     Base: Base_Type := 10) is

      function Digit_Sum(N: Number) return Number is
      begin
         if N < Number(Base) then
            return N;
         else
            return (N mod Number(Base)) & Digit_Sum(N / Number(Base));
         end if;
      end Digit_Sum;

   begin
      if N < Number(Base) then
         Root := N;
         Persistence := 0;
      else
         Compute_Root(Digit_Sum(N), Root, Persistence, Base);
         Persistence := Persistence + 1;
      end if;
   end Compute_Root;

end Generic_Root;
```


Finally the main program.  The procedure "Print_Roots" is for our convenience.


```Ada
with Generic_Root, Ada.Text_IO; use Generic_Root;

procedure Digital_Root is

   procedure Compute is new Compute_Root("+");
     -- "+" for additive digital roots

   package TIO renames Ada.Text_IO;

    procedure Print_Roots(Inputs: Number_Array; Base: Base_Type) is
      package NIO is new TIO.Integer_IO(Number);
      Root, Pers: Number;
   begin
      for I in Inputs'Range loop
         Compute(Inputs(I), Root, Pers, Base);
         NIO.Put(Inputs(I), Base => Integer(Base), Width => 12);
         NIO.Put(Root, Base => Integer(Base), Width => 9);
         NIO.Put(Pers, Base => Integer(Base), Width => 12);
         TIO.Put_Line("   " & Base_Type'Image(Base));
      end loop;
   end Print_Roots;
begin
   TIO.Put_Line("      Number     Root Persistence  Base");
   Print_Roots((961038, 923594037444, 670033, 448944221089), Base => 10);
   Print_Roots((16#7e0#, 16#14e344#, 16#12343210#), Base => 16);
end Digital_Root;
```


```txt
      Number     Root Persistence  Base
      961038        9           2    10
923594037444        9           2    10
      670033        1           3    10
448944221089        1           3    10
     16#7E0#    16#6#       16#2#    16
  16#14E344#    16#F#       16#2#    16
16#12343210#    16#1#       16#2#    16
```



## ALGOL 68


```algol68
# calculates the digital root and persistance of n #
PROC digital root = ( LONG LONG INT n, REF INT root, persistance )VOID:
     BEGIN
         LONG LONG INT number := ABS n;
         persistance := 0;
         WHILE persistance PLUSAB 1;
               LONG LONG INT digit sum := 0;
               WHILE number > 0
               DO
                   digit sum PLUSAB number MOD 10;
                   number    OVERAB 10
               OD;
               number := digit sum;
               number > 9
         DO
               SKIP
         OD;
         root := SHORTEN SHORTEN number
     END; # digital root #

# calculates and prints the digital root and persistace of number #
PROC print digital root and persistance = ( LONG LONG INT number )VOID:
     BEGIN
         INT    root, persistance;
         digital root( number, root, persistance );
         print( ( whole( number, -15 ), " root: ", whole( root, 0 ), " persistance: ", whole( persistance, -3 ), newline ) )
     END; # print digital root and persistance #

# test the digital root proc #
BEGIN print digital root and persistance(       627615 )
    ; print digital root and persistance(        39390 )
    ; print digital root and persistance(       588225 )
    ; print digital root and persistance( 393900588225 )
END
```

```txt

         627615 root: 9 persistance:   2
          39390 root: 6 persistance:   2
         588225 root: 3 persistance:   2
   393900588225 root: 9 persistance:   2

```



## ALGOL W


```algolw
begin

    % calculates the digital root and persistence of an integer in base 10   %
    % in order to allow for numbers larger than 2^31, the number is passed   %
    % as the lower and upper digits e.g. 393900588225 can be processed by    %
    % specifying upper = 393900, lower = 58825                               %
    procedure findDigitalRoot( integer value  upper, lower
                             ; integer result digitalRoot, persistence
                             ) ;
    begin

        integer procedure sumDigits( integer value n ) ;
        begin
            integer digits, sum;

            digits := abs n;
            sum    := 0;

            while digits > 0
            do begin
                sum    := sum + ( digits rem 10 );
                digits := digits div 10
            end % while digits > 0 % ;

            % result: % sum
        end sumDigits;

        digitalRoot := sumDigits( upper ) + sumDigits( lower );
        persistence := 1;

        while digitalRoot > 9
        do begin
            persistence := persistence + 1;
            digitalRoot := sumDigits( digitalRoot );
        end % while digitalRoot > 9 % ;

    end findDigitalRoot ;

    % calculates and prints the digital root and persistence                 %
    procedure printDigitalRootAndPersistence( integer value upper, lower ) ;
    begin
        integer digitalRoot, persistence;
        findDigitalRoot( upper, lower, digitalRoot, persistence );
        write( s_w := 0  % set field saeparator width for this statement %
             , i_w := 8  % set integer field width for this statement    %
             , upper
             , ", "
             , lower
             , i_w := 2  % change integer field width %
             , ": digital root: "
             , digitalRoot
             , ", persistence: "
             , persistence
             )
    end printDigitalRootAndPersistence ;

    % test the digital root and persistence procedures %
    printDigitalRootAndPersistence(      0, 627615 );
    printDigitalRootAndPersistence(      0,  39390 );
    printDigitalRootAndPersistence(      0, 588225 );
    printDigitalRootAndPersistence( 393900, 588225 )

end.
```

```txt

       0,   627615: digital root:  9, persistence:  2
       0,    39390: digital root:  6, persistence:  2
       0,   588225: digital root:  3, persistence:  2
  393900,   588225: digital root:  9, persistence:  2

```



## AppleScript


```applescript
on digitalroot(N as integer)
	script math
		to sum(L)
			if L = {} then return 0
			(item 1 of L) + sum(rest of L)
		end sum
	end script

	set i to 0
	set M to N

	repeat until M < 10
		set digits to the characters of (M as text)
		set M to math's sum(digits)
		set i to i + 1
	end repeat

	{N:N, persistences:i, root:M}
end digitalroot


digitalroot(627615)
```


```txt
{N:627615, persistences:2, root:9}
```



## Applesoft BASIC


```ApplesoftBasic
1 GOSUB 430"BASE SETUP
2 FOR E = 0 TO 1 STEP 0
3     GOSUB 7"READ
4     ON E + 1 GOSUB 50, 10
5 NEXT E
6 END

7 READ N$
8 E = N$ = ""
9 RETURN

10 GOSUB 7"READ BASE
20 IF E THEN RETURN
30 BASE = VAL(N$)
40 READ N$

50 GOSUB 100"DIGITAL ROOT
60 GOSUB 420: PRINT " HAS AD";
70 PRINT "DITIVE PERSISTENCE";
80 PRINT " "P" AND DIGITAL R";
90 PRINT "OOT "X$";" : RETURN

REM DIGITAL ROOT OF N$, RETURNS X$ AND P

100 P = 0 : L = LEN(N$)
110 X$ = MID$(N$, 2, L - 1)
120 N = LEFT$(X$, 1) = "-"
130 IF NOT N THEN X$ = N$
140 FOR P = 0 TO 1E38
150     L = LEN(X$)
160     IF L < 2 THEN RETURN
170     GOSUB 200"DIGIT SUM
180     X$ = S$
190 NEXT P : STOP

REM DIGIT SUM OF X$, RETURNS S$

200 S$ = "0"
210 R$ = X$
220 L = LEN(R$)
230 FOR L = L TO 1 STEP -1
240     E$ = "" : V$ = RIGHT$(R$, 1)
250     GOSUB 400 : S = LEN(S$)
260     ON R$ <> "0" GOSUB 300
270     R$ = MID$(R$, 1, L - 1)
280 NEXT L
290 RETURN

REM ADD V TO S$

300 FOR C = V TO 0 STEP 0
310     V$ = RIGHT$(S$, 1)
320     GOSUB 400 : S = S - 1
330     S$ = MID$(S$, 1, S)
340     V = V + C : C = V >= BASE
350     IF C THEN V = V - BASE
360     GOSUB 410 : E$ = V$ + E$
370     IF S THEN NEXT C
380 IF C THEN S$ = "1"
390 S$ = S$ + E$ : RETURN

REM BASE VAL
400 V = V(ASC(V$)) : RETURN

REM BASE STR$
410 V$ = V$(V) : RETURN

REM BASE DISPLAY
420 PRINT N$;
421 IF BASE = 10 THEN RETURN
422 PRINT "("BASE")";
423 RETURN

REM BASE SETUP
430 IF BASE = 0 THEN BASE = 10
440 DIM V(127), V$(35)
450 FOR I = 0 TO 35
460     V = 55 + I - (I < 10) * 7
470     V$(I) = CHR$(V)
480     V(V) = I
490 NEXT I : RETURN

500  DATA627615,39390,588225
510  DATA393900588225
1000 DATA,30
1010 DATADIGITALROOT
63999DATA,
```

```txt
627615 HAS ADDITIVE PERSISTENCE 2 AND DIGITAL ROOT 9;
39390 HAS ADDITIVE PERSISTENCE 2 AND DIGITAL ROOT 6;
588225 HAS ADDITIVE PERSISTENCE 2 AND DIGITAL ROOT 3;
393900588225 HAS ADDITIVE PERSISTENCE 2 AND DIGITAL ROOT 9;
DIGITALROOT(30) HAS ADDITIVE PERSISTENCE 2 AND DIGITAL ROOT Q;
```



## AutoHotkey


```AutoHotkey
p := {}
for key, val in [30,1597,381947,92524902,448944221089]
{
    n := val
    while n > 9
    {
        m := 0
        Loop, Parse, n
            m += A_LoopField
        n := m, i := A_Index
    }
    p[A_Index] := [val, n, i]
}

for key, val in p
    Output .= val[1] ": Digital Root = " val[2] ", Additive Persistence = " val[3] "`n"

MsgBox, 524288, , % Output
```

```txt
          30: Digital Root = 3, Additive Persistence = 1
        1597: Digital Root = 4, Additive Persistence = 2
      381947: Digital Root = 5, Additive Persistence = 2
    92524902: Digital Root = 6, Additive Persistence = 2
448944221089: Digital Root = 1, Additive Persistence = 3
```



## AWK


```AWK
# syntax: GAWK -f DIGITAL_ROOT.AWK
BEGIN {
    n = split("627615,39390,588225,393900588225,10,199",arr,",")
    for (i=1; i<=n; i++) {
      dr = digitalroot(arr[i],10)
      printf("%12.0f has additive persistence %d and digital root of %d\n",arr[i],p,dr)
    }
    exit(0)
}
function digitalroot(n,b) {
    p = 0 # global
    while (n >= b) {
      p++
      n = digitsum(n,b)
    }
    return(n)
}
function digitsum(n,b,  q,s) {
    while (n != 0) {
      q = int(n / b)
      s += n - q * b
      n = q
    }
    return(s)
}
```

```txt

      627615 has additive persistence 2 and digital root of 9
       39390 has additive persistence 2 and digital root of 6
      588225 has additive persistence 2 and digital root of 3
393900588225 has additive persistence 2 and digital root of 9
          10 has additive persistence 1 and digital root of 1
         199 has additive persistence 3 and digital root of 1

```



## BASIC

This calculates the result "the hard way", but is limited to the limits of a 32-bit signed integer (+/-2,147,483,647) and therefore can't calculate the digital root of 393,900,588,225.

```qbasic
DECLARE SUB digitalRoot (what AS LONG)

'test inputs:
digitalRoot 627615
digitalRoot 39390
digitalRoot 588225

SUB digitalRoot (what AS LONG)
    DIM w AS LONG, t AS LONG, c AS INTEGER

    w = ABS(what)
    IF w > 10 THEN
        DO
            c = c + 1
            WHILE w
                t = t + (w MOD (10))
                w = w \ 10
            WEND
            w = t
            t = 0
        LOOP WHILE w > 9
    END IF
    PRINT what; ": additive persistance "; c; ", digital root "; w
END SUB
```

 627615 : additive persistance  2 , digital root  9
 39390 : additive persistance  2 , digital root  6
 588225 : additive persistance  2 , digital root  3


## Batch File


```dos
::
::Digital Root Task from Rosetta Code Wiki
::Batch File Implementation
::
::Base 10...
::

@echo off
setlocal enabledelayedexpansion

::THE MAIN THING...
for %%x in (9876543214 393900588225 1985989328582 34559) do (
	call :droot %%x
)
echo.
pause
exit /b
::/THE MAIN THING...

::THE FUNCTION
:droot
set inp2sum=%1&set persist=1

:cyc1
set sum=0
set scan_digit=0
:cyc2
set digit=!inp2sum:~%scan_digit%,1!
if "%digit%"=="" (goto :sumdone)
set /a sum+=%digit%
set /a scan_digit+=1
goto :cyc2

:sumdone
if %sum% lss 10 (
	echo.
	echo ^(%1^)
	echo Additive Persistence=%persist% Digital Root=%sum%.
	goto :EOF
)
set /a persist+=1
set inp2sum=%sum%
goto :cyc1
::/THE FUNCTION
```

```txt
(9876543214)
Additive Persistence=3 Digital Root=4.

(393900588225)
Additive Persistence=2 Digital Root=9.

(1985989328582)
Additive Persistence=3 Digital Root=5.

(34559)
Additive Persistence=2 Digital Root=8.

Press any key to continue . . .
```



## BBC BASIC

```bbcbasic
      *FLOAT64
      PRINT "Digital root of 627615 is "; FNdigitalroot(627615, 10, p) ;
      PRINT " (additive persistence " ; p ")"
      PRINT "Digital root of 39390 is "; FNdigitalroot(39390, 10, p) ;
      PRINT " (additive persistence " ; p ")"
      PRINT "Digital root of 588225 is "; FNdigitalroot(588225, 10, p) ;
      PRINT " (additive persistence " ; p ")"
      PRINT "Digital root of 393900588225 is "; FNdigitalroot(393900588225, 10, p) ;
      PRINT " (additive persistence " ; p ")"
      PRINT "Digital root of 9992 is "; FNdigitalroot(9992, 10, p) ;
      PRINT " (additive persistence " ; p ")"
      END

      DEF FNdigitalroot(n, b, RETURN c)
      c = 0
      WHILE n >= b
        c += 1
        n = FNdigitsum(n, b)
      ENDWHILE
      = n

      DEF FNdigitsum(n, b)
      LOCAL q, s
      WHILE n <> 0
        q = INT(n / b)
        s += n - q * b
        n = q
      ENDWHILE
      = s
```

```txt

Digital root of 627615 is 9 (additive persistence 2)
Digital root of 39390 is 6 (additive persistence 2)
Digital root of 588225 is 3 (additive persistence 2)
Digital root of 393900588225 is 9 (additive persistence 2)
Digital root of 9992 is 2 (additive persistence 3)

```



## Befunge

The number, ''n'', is read as a string from stdin in order to support a larger range of values than would typically be accepted by the numeric input of most Befunge implementations. After the initial value has been summed, though, subsequent iterations are simply calculated as integer sums.


```befunge
0" :rebmun retnE">:#,_0 0v
v\1:/+55p00<v\`\0::-"0"<~<
#>:55+%00g+^>9`+#v_+\ 1+\^
>|`9:p000<_v#`1\$<  v"gi"<
|> \ 1 + \ >0" :toor lat"^
>$$00g\1+^@,+<v"Di",>#+ 5<
>:#,_$ . 5 5 ^>:#,_\.55+,v
^"Additive Persistence: "<
```


{{out}} (multiple runs)

```txt
Enter number: 1003201

Digital root: 7
Additive Persistence: 1

Enter number: 393900588225

Digital root: 9
Additive Persistence: 2

Enter number: 448944221089

Digital root: 1
Additive Persistence: 3
```



## Bracmat


```bracmat
  ( root
  =   sum persistence n d
    .   !arg:(~>9.?)
      |   !arg:(?n.?persistence)
        & 0:?sum
        & ( @( !n
             :   ?
                 (#%@?d&!d+!sum:?sum&~)
                 ?
             )
          | root$(!sum.!persistence+1)
          )
  )
& (   627615 39390 588225 393900588225 10 199
    :   ?
        ( #%@?N
        & root$(!N.0):(?Sum.?Persistence)
        &   out
          $ ( !N
              "has additive persistence"
              !Persistence
              "and digital root of"
              !Sum
            )
        & ~
        )
        ?
  | done
  );
```

```txt
627615 has additive persistence 2 and digital root of 9
39390 has additive persistence 2 and digital root of 6
588225 has additive persistence 2 and digital root of 3
393900588225 has additive persistence 2 and digital root of 9
10 has additive persistence 1 and digital root of 1
199 has additive persistence 3 and digital root of 1
```



## C


```c
#include <stdio.h>

int droot(long long int x, int base, int *pers)
{
	int d = 0;
	if (pers)
		for (*pers = 0; x >= base; x = d, (*pers)++)
			for (d = 0; x; d += x % base, x /= base);
	else if (x && !(d = x % (base - 1)))
			d = base - 1;

	return d;
}

int main(void)
{
	int i, d, pers;
	long long x[] = {627615, 39390, 588225, 393900588225LL};

	for (i = 0; i < 4; i++) {
		d = droot(x[i], 10, &pers);
		printf("%lld: pers %d, root %d\n", x[i], pers, d);
	}

	return 0;
}
```


## C#

```c#
using System;
using System.Linq;

class Program
{
    static Tuple<int, int> DigitalRoot(long num)
    {
        int additivepersistence = 0;
        while (num > 9)
        {
            num = num.ToString().ToCharArray().Sum(x => x - '0');
            additivepersistence++;
        }
        return new Tuple<int, int>(additivepersistence, (int)num);
    }
    static void Main(string[] args)
    {
        foreach (long num in new long[] { 627615, 39390, 588225, 393900588225 })
        {
            var t = DigitalRoot(num);
            Console.WriteLine("{0} has additive persistence {1} and digital root {2}", num, t.Item1, t.Item2);
        }
    }
}
```

```txt
627615 has additive persistence 2 and digital root 9
39390 has additive persistence 2 and digital root 6
588225 has additive persistence 2 and digital root 3
393900588225 has additive persistence 2 and digital root 9
```



## C++

For details of SumDigits see: http://rosettacode.org/wiki/Sum_digits_of_an_integer

```cpp
// Calculate the Digital Root and Additive Persistance of an Integer - Compiles with gcc4.7
//
// Nigel Galloway. July 23rd., 2012
//
#include <iostream>
#include <cmath>
#include <utility>

template<class P_> P_ IncFirst(const P_& src) {return P_(src.first + 1, src.second);}

std::pair<int, int> DigitalRoot(unsigned long long digits, int base = 10)
{
    int x = SumDigits(digits, base);
    return x < base ? std::make_pair(1, x) : IncFirst(DigitalRoot(x, base));  // x is implicitly converted to unsigned long long; this is lossless
}

int main() {
    const unsigned long long ip[] = {961038,923594037444,670033,448944221089};
    for (auto i:ip){
        auto res = DigitalRoot(i);
        std::cout << i << " has digital root " << res.second << " and additive persistance " << res.first << "\n";
    }
    std::cout << "\n";
    const unsigned long long hip[] = {0x7e0,0x14e344,0xd60141,0x12343210};
    for (auto i:hip){
        auto res = DigitalRoot(i,16);
        std::cout << std::hex << i << " has digital root " << res.second << " and additive persistance " << res.first << "\n";
    }
    return 0;
}
```

```txt
961038 has digital root 9 and additive persistance 2
923594037444 has digital root 9 and additive persistance 2
670033 has digital root 1 and additive persistance 3
448944221089 has digital root 1 and additive persistance 3

7e0 has digital root 6 and additive persistance 2
14e344 has digital root f and additive persistance 2
d60141 has digital root a and additive persistance 2
12343210 has digital root 1 and additive persistance 2
```



## Component Pascal

```oberon2

MODULE DigitalRoot;
IMPORT StdLog, Strings, TextMappers, DevCommanders;

PROCEDURE CalcDigitalRoot(x: LONGINT; OUT dr,pers: LONGINT);
VAR
	str: ARRAY 64 OF CHAR;
	i: INTEGER;
BEGIN
	dr := 0;pers := 0;
	LOOP
		Strings.IntToString(x,str);
		IF LEN(str$) = 1 THEN dr := x ;EXIT END;
		i := 0;dr := 0;
		WHILE (i < LEN(str$)) DO
			INC(dr,ORD(str[i]) - ORD('0'));
			INC(i)
		END;
		INC(pers);
		x := dr
	END;
END CalcDigitalRoot;

PROCEDURE Do*;
VAR
	dr,pers: LONGINT;
	s: TextMappers.Scanner;
BEGIN
	s.ConnectTo(DevCommanders.par.text);
	s.SetPos(DevCommanders.par.beg);
	REPEAT
		s.Scan;
		IF (s.type = TextMappers.int) OR (s.type = TextMappers.lint) THEN
			CalcDigitalRoot(s.int,dr,pers);
			StdLog.Int(s.int);
			StdLog.String(" Digital root: ");StdLog.Int(dr);
			StdLog.String(" Persistence: ");StdLog.Int(pers);StdLog.Ln
		END
	UNTIL s.rider.eot;
END Do;
END DigitalRoot.

```

Execute:
^Q DigitalRoot.Do 627615 39390 588225 393900588~
```txt

 627615 Digital root:  9 Persistence:  2
 39390 Digital root:  6 Persistence:  2
 588225 Digital root:  3 Persistence:  2
 393900588 Digital root:  9 Persistence:  2

```


## Common Lisp

Using <code>SUM-DIGITS</code> from the task "[[Sum_digits_of_an_integer#Common_Lisp|Sum digits of an integer]]".

```lisp
(defun digital-root (number &optional (base 10))
  (loop for n = number then s
        for ap = 1 then (1+ ap)
        for s = (sum-digits n base)
        when (< s base)
          return (values s ap)))

(loop for (nr base) in '((627615 10) (393900588225 10) (#X14e344 16) (#36Rdg9r 36))
      do (multiple-value-bind (dr ap) (digital-root nr base)
           (format T "~vR (base ~a): additive persistence = ~a, digital root = ~vR~%"
                   base nr base ap base dr)))
```

```txt
627615 (base 10): additive persistence = 2, digital root = 9
393900588225 (base 10): additive persistence = 2, digital root = 9
14E344 (base 16): additive persistence = 2, digital root = F
DG9R (base 36): additive persistence = 2, digital root = U
```



## D


```d
import std.stdio, std.typecons, std.conv, std.bigint, std.math,
       std.traits;

Tuple!(uint, Unqual!T) digitalRoot(T)(in T inRoot, in uint base)
pure nothrow
in {
    assert(base > 1);
} body {
    Unqual!T root = inRoot.abs;
    uint persistence = 0;
    while (root >= base) {
        auto num = root;
        root = 0;
        while (num != 0) {
            root += num % base;
            num /= base;
        }
        persistence++;
    }
    return typeof(return)(persistence, root);
}

void main() {
    enum f1 = "%s(%d): additive persistance= %d, digital root= %d";
    foreach (immutable b; [2, 3, 8, 10, 16, 36]) {
        foreach (immutable n; [5, 627615, 39390, 588225, 393900588225])
            writefln(f1, text(n, b), b, n.digitalRoot(b)[]);
        writeln;
    }

    enum f2 = "<BIG>(%d): additive persistance= %d, digital root= %d";
    immutable n = BigInt("581427189816730304036810394583022044713" ~
                         "00738980834668522257090844071443085937");
    foreach (immutable b; [2, 3, 8, 10, 16, 36])
        writefln(f2, b, n.digitalRoot(b)[]); // Shortened output.
}
```

```txt
101(2): additive persistance= 2, digital root= 1
10011001001110011111(2): additive persistance= 3, digital root= 1
1001100111011110(2): additive persistance= 3, digital root= 1
10001111100111000001(2): additive persistance= 3, digital root= 1
101101110110110010011011111110011000001(2): additive persistance= 3, digital root= 1

12(3): additive persistance= 2, digital root= 1
1011212221000(3): additive persistance= 3, digital root= 1
2000000220(3): additive persistance= 2, digital root= 2
1002212220010(3): additive persistance= 3, digital root= 1
1101122201121110011000000(3): additive persistance= 3, digital root= 1

5(8): additive persistance= 0, digital root= 5
2311637(8): additive persistance= 3, digital root= 2
114736(8): additive persistance= 3, digital root= 1
2174701(8): additive persistance= 3, digital root= 1
5566623376301(8): additive persistance= 3, digital root= 4

5(10): additive persistance= 0, digital root= 5
627615(10): additive persistance= 2, digital root= 9
39390(10): additive persistance= 2, digital root= 6
588225(10): additive persistance= 2, digital root= 3
393900588225(10): additive persistance= 2, digital root= 9

5(16): additive persistance= 0, digital root= 5
9939F(16): additive persistance= 2, digital root= 15
99DE(16): additive persistance= 2, digital root= 15
8F9C1(16): additive persistance= 2, digital root= 15
5BB64DFCC1(16): additive persistance= 2, digital root= 15

5(36): additive persistance= 0, digital root= 5
DG9R(36): additive persistance= 2, digital root= 30
UE6(36): additive persistance= 2, digital root= 15
CLVL(36): additive persistance= 2, digital root= 15
50YE8N29(36): additive persistance= 2, digital root= 25

<BIG>(2): additive persistance= 4, digital root= 1
<BIG>(3): additive persistance= 4, digital root= 1
<BIG>(8): additive persistance= 3, digital root= 3
<BIG>(10): additive persistance= 3, digital root= 4
<BIG>(16): additive persistance= 3, digital root= 7
<BIG>(36): additive persistance= 3, digital root= 17
```



## Dc

Tested on GNU dc.
Procedure <code>p</code> is for breaking up the number into individual digits.
Procedure <code>q</code> is for summing all digits left by procedure <code>p</code>.
Procedure <code>r</code> is for overall control (when to stop).

```Dc
?[10~rd10<p]sp[+z1<q]sq[lpxlqxd10<r]dsrxp
```


## DCL


```DCL
$ x = p1
$ count = 0
$ sum = x
$ loop1:
$  length = f$length( x )
$  if length .eq. 1 then $ goto done
$  i = 0
$  sum = 0
$  loop2:
$   digit = f$extract( i, 1, x )
$   sum = sum + digit
$   i = i + 1
$   if i .lt. length then $ goto loop2
$  x = f$string( sum )
$  count = count + 1
$  goto loop1
$ done:
$ write sys$output p1, " has additive persistence ", count, " and digital root of ", sum
```

```txt
$ @digital_root 627615
627615 has additive persistence 2 and digital root of 9
$ @digital_root 6
6 has additive persistence 0 and digital root of 6
$ @digital_root 99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999998
99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999998 has additive persistence 3 and digital root of 8
```



## Eiffel


```Eiffel

class
	APPLICATION

inherit
	ARGUMENTS

create
	make

feature {NONE} -- Initialization

	digital_root_test_values: ARRAY [INTEGER_64]
			-- Test values.
		once
		 	Result := <<670033, 39390, 588225, 393900588225>> -- base 10
		end

	digital_root_expected_result: ARRAY [INTEGER_64]
			-- Expected result values.
		once
			Result := <<1, 6, 3, 9>> -- base 10
		end

	make
		local
			results: ARRAY [INTEGER_64]
			i: INTEGER
		do
			from
				i := 1
			until
				i > digital_root_test_values.count
			loop
				results := compute_digital_root (digital_root_test_values [i], 10)
				if results [2] ~ digital_root_expected_result [i] then
					print ("%N" + digital_root_test_values [i].out + " has additive persistence " + results [1].out + " and digital root " + results [2].out)
				else
					print ("Error in the calculation of the digital root of " + digital_root_test_values [i].out + ". Expected value: " + digital_root_expected_result [i].out + ", produced value: " + results [2].out)
				end
				i := i	+ 1
			end
		end

compute_digital_root (a_number: INTEGER_64;  a_base: INTEGER): ARRAY [INTEGER_64]
				-- Returns additive persistence and digital root of `a_number' using `a_base'.
		require
                        valid_number: a_number >= 0
                        valid_base: a_base > 1
                local
			temp_num: INTEGER_64
		do
			create Result.make_filled (0, 1, 2)
			from
				Result [2] := a_number
			until
				Result [2] < a_base
			loop
				from
					temp_num := Result [2]
					Result [2] := 0
				until
					temp_num = 0
				loop
					Result [2] := Result [2] + (temp_num \\ a_base)
					temp_num := temp_num // a_base
				end
				Result [1] := Result [1] + 1
			end
		end

```

```txt

670033 has additive persistence 3 and digital root 1
39390 has additive persistence 2 and digital root 6
588225 has additive persistence 2 and digital root 3
393900588225 has additive persistence 2 and digital root 9

```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'routines;
import system'collections;

extension op
{
    get DigitalRoot()
    {
        int  additivepersistence := 0;
        long num := self;

        while (num > 9)
        {
            num := num.Printable.toArray().selectBy:(ch => ch.toInt() - 48).summarize(new LongInteger());

            additivepersistence += 1
        };

        ^ new Tuple<int,int>(additivepersistence, num.toInt())
    }
}

public program()
{
    new long[]::(627615l, 39390l, 588225l, 393900588225l).forEach:(num)
    {
        var t := num.DigitalRoot;

        console.printLineFormatted("{0} has additive persistence {1} and digital root {2}", num, t.Item1, t.Item2)
    }
}
```

```txt

627615 has additive persistence 2 and digital root 9
39390 has additive persistence 2 and digital root 6
588225 has additive persistence 2 and digital root 3
393900588225 has additive persistence 2 and digital root 9

```



## Elixir

```elixir
defmodule Digital do
  def root(n, base\\10), do: root(n, base, 0)

  defp root(n, base, ap) when n < base, do: {n, ap}
  defp root(n, base, ap) do
    Integer.digits(n, base) |> Enum.sum |> root(base, ap+1)
  end
end

data = [627615, 39390, 588225, 393900588225]
Enum.each(data, fn n ->
  {dr, ap} = Digital.root(n)
  IO.puts "#{n} has additive persistence #{ap} and digital root of #{dr}"
end)

base = 16
IO.puts "\nBase = #{base}"
fmt = "~.#{base}B(#{base}) has additive persistence ~w and digital root of ~w~n"
Enum.each(data, fn n ->
  {dr, ap} = Digital.root(n, base)
  :io.format fmt, [n, ap, dr]
end)
```


```txt

627615 has additive persistence 2 and digital root of 9
39390 has additive persistence 2 and digital root of 6
588225 has additive persistence 2 and digital root of 3
393900588225 has additive persistence 2 and digital root of 9

Base = 16
9939F(16) has additive persistence 2 and digital root of 15
99DE(16) has additive persistence 2 and digital root of 15
8F9C1(16) has additive persistence 2 and digital root of 15
5BB64DFCC1(16) has additive persistence 2 and digital root of 15

```



## Erlang

Using [[Sum_digits_of_an_integer]].

```Erlang
-module( digital_root ).

-export( [task/0] ).

task() ->
    Ns = [N || N <- [627615, 39390, 588225, 393900588225]],
    Persistances = [persistance_root(X) || X <-	Ns],
    [io:fwrite("~p has additive persistence ~p and digital root of ~p~n", [X, Y, Z]) || {X, {Y, Z}} <- lists:zip(Ns, Persistances)].


persistance_root( X ) -> persistance_root( sum_digits:sum_digits(X), 1 ).

persistance_root( X, N ) when X	< 10 ->	{N, X};
persistance_root( X, N ) -> persistance_root( sum_digits:sum_digits(X),	N + 1 ).

```

```txt

11> digital_root:task().
627615 has additive persistence 2 and digital root of 9
39390 has additive persistence 2 and digital root of 6
588225 has additive persistence 2 and digital root of 3
393900588225 has additive persistence 2 and digital root of 9

```



=={{header|F_Sharp|F#}}==
This code uses sumDigits from [[Sum_digits_of_an_integer#or_Generically]]

```fsharp

//Find the Digital Root of An Integer - Nigel Galloway: February 1st., 2015
//This code will work with any integer type
let inline digitalRoot N BASE =
  let rec root(p,n) =
    let s = sumDigits n BASE
    if s < BASE then (s,p) else root(p+1, s)
  root(LanguagePrimitives.GenericZero<_> + 1, N)

```

```txt

> digitalRoot 627615 10;;
val it : int * int = (9, 2)
> digitalRoot 39390 10;;
val it : int * int = (6, 2)
> digitalRoot 588225 10;;
val it : int * int = (3, 2)
> digitalRoot 393900588225L 10L;;
val it : int64 * int = (9L, 2)
> digitalRoot 123456789123456789123456789123456789123456789I 10I;;
val it : System.Numerics.BigInteger * int = (9 {IsEven = false;
                                                IsOne = false;
                                                IsPowerOfTwo = false;
                                                IsZero = false;
                                                Sign = 1;}, 2)

```



## Factor


```factor
USING: arrays formatting kernel math math.text.utils sequences ;
IN: rosetta-code.digital-root

: digital-root ( n -- persistence root )
    0 swap [ 1 digit-groups dup length 1 > ] [ sum [ 1 + ] dip ]
    while first ;

: print-root ( n -- )
    dup digital-root
    "%-12d has additive persistence %d and digital root %d.\n"
    printf ;

{ 627615 39390 588225 393900588225 } [ print-root ] each
```

```txt
627615       has additive persistence 2 and digital root 9.
39390        has additive persistence 2 and digital root 6.
588225       has additive persistence 2 and digital root 3.
393900588225 has additive persistence 2 and digital root 9.
```



## Fortran


```Fortran

program prec
implicit none
integer(kind=16) :: i
i = 627615
call root_pers(i)
i = 39390
call root_pers(i)
i = 588225
call root_pers(i)
i = 393900588225
call root_pers(i)
end program

subroutine root_pers(i)
implicit none
integer(kind=16) :: N, s, a, i
write(*,*) 'Number: ', i
n = i
a = 0
do while(n.ge.10)
  a = a + 1
  s = 0
  do while(n.gt.0)
    s = s + n-int(real(n,kind=8)/10.0D0,kind=8) * 10_8
    n = int(real(n,kind=16)/real(10,kind=8),kind=8)
  end do
  n = s
end do
write(*,*) 'digital root = ', s
write(*,*) 'additive persistance = ', a
end subroutine

```



```txt

 Number:                627615
 digital root =                     9
 additive persistance =                     2
 Number:                 39390
 digital root =                     6
 additive persistance =                     2
 Number:                588225
 digital root =                     3
 additive persistance =                     2
 Number:          393900588225
 digital root =                     9
 additive persistance =                     2

```



## Forth

This is trivial to do in Forth, because radix control is one of its most prominent feature. The 32-bits version just takes two lines:

```forth
: (Sdigit) 0 swap begin base @ /mod >r + r> dup 0= until drop ;
: digiroot 0 swap begin (Sdigit) >r 1+ r> dup base @ < until ;
```

This will take care of most numbers:

```txt

627615 digiroot . . 9 2  ok
39390 digiroot . . 6 2  ok
588225 digiroot . . 3 2  ok

```

For the last one we will need a "double number" version. '''MU/MOD''' is not available in some Forth implementations, but it is easy to define:

```forth
[UNDEFINED] mu/mod [IF] : mu/mod >r 0 r@ um/mod r> swap >r um/mod r> ; [THEN]

: (Sdigit) 0. 2swap begin base @ mu/mod 2>r s>d d+ 2r> 2dup d0= until 2drop ;
: digiroot 0 -rot begin (Sdigit) 2>r 1+ 2r> 2dup base @ s>d d< until d>s ;
```

That one will take care of the last one:

```txt

393900588225. digiroot . . 9 2  ok

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function digitalRoot(n As UInteger, ByRef ap As Integer, base_ As Integer = 10) As Integer
  Dim dr As Integer
  ap = 0
  Do
    dr = 0
    While n > 0
      dr += n Mod base_
      n = n \ base_
    Wend
    ap += 1
    n = dr
  Loop until dr < base_
  Return dr
End Function

Dim As Integer dr, ap
Dim a(3) As UInteger = {627615, 39390, 588225, 393900588225}
For i As Integer = 0 To 3
 ap = 0
 dr = digitalRoot(a(i), ap)
 Print a(i), "Additive Persistence ="; ap, "Digital root ="; dr
 Print
Next
Print "Press any key to quit"
Sleep
```


```txt

627615        Additive Persistence = 2    Digital root = 9

39390         Additive Persistence = 2    Digital root = 6

588225        Additive Persistence = 2    Digital root = 3

393900588225  Additive Persistence = 2    Digital root = 9

```



## Go

With function <code>Sum</code> from [[Sum digits of an integer#Go]].


```go
package main

import (
	"fmt"
	"log"
	"strconv"
)

func Sum(i uint64, base int) (sum int) {
	b64 := uint64(base)
	for ; i > 0; i /= b64 {
		sum += int(i % b64)
	}
	return
}

func DigitalRoot(n uint64, base int) (persistence, root int) {
	root = int(n)
	for x := n; x >= uint64(base); x = uint64(root) {
		root = Sum(x, base)
		persistence++
	}
	return
}

// Normally the below would be moved to a *_test.go file and
// use the testing package to be runnable as a regular test.

var testCases = []struct {
	n           string
	base        int
	persistence int
	root        int
}{
	{"627615", 10, 2, 9},
	{"39390", 10, 2, 6},
	{"588225", 10, 2, 3},
	{"393900588225", 10, 2, 9},
	{"1", 10, 0, 1},
	{"11", 10, 1, 2},
	{"e", 16, 0, 0xe},
	{"87", 16, 1, 0xf},
	// From Applesoft BASIC example:
	{"DigitalRoot", 30, 2, 26}, // 26 is Q base 30
	// From C++ example:
	{"448944221089", 10, 3, 1},
	{"7e0", 16, 2, 0x6},
	{"14e344", 16, 2, 0xf},
	{"d60141", 16, 2, 0xa},
	{"12343210", 16, 2, 0x1},
	// From the D example:
	{"1101122201121110011000000", 3, 3, 1},
}

func main() {
	for _, tc := range testCases {
		n, err := strconv.ParseUint(tc.n, tc.base, 64)
		if err != nil {
			log.Fatal(err)
		}
		p, r := DigitalRoot(n, tc.base)
		fmt.Printf("%12v (base %2d) has additive persistence %d and digital root %s\n",
			tc.n, tc.base, p, strconv.FormatInt(int64(r), tc.base))
		if p != tc.persistence || r != tc.root {
			log.Fatalln("bad result:", tc, p, r)
		}
	}
}
```

```txt

      627615 (base 10) has additive persistence 2 and digital root 9
       39390 (base 10) has additive persistence 2 and digital root 6
      588225 (base 10) has additive persistence 2 and digital root 3
393900588225 (base 10) has additive persistence 2 and digital root 9
           1 (base 10) has additive persistence 0 and digital root 1
          11 (base 10) has additive persistence 1 and digital root 2
           e (base 16) has additive persistence 0 and digital root e
          87 (base 16) has additive persistence 1 and digital root f
 DigitalRoot (base 30) has additive persistence 2 and digital root q
448944221089 (base 10) has additive persistence 3 and digital root 1
         7e0 (base 16) has additive persistence 2 and digital root 6
      14e344 (base 16) has additive persistence 2 and digital root f
      d60141 (base 16) has additive persistence 2 and digital root a
    12343210 (base 16) has additive persistence 2 and digital root 1
1101122201121110011000000 (base  3) has additive persistence 3 and digital root 1

```



## Haskell


```haskell
import Data.Tuple (swap)
import Data.List (unfoldr)

digSum :: Int -> Int -> Int
digSum base = sum . unfoldr f
  where
    f 0 = Nothing
    f n = Just (swap (quotRem n base))

digRoot :: Int -> Int -> (Int, Int)
digRoot base =
  head . dropWhile ((>= base) . snd) . zip [0 ..] . iterate (digSum base)

main :: IO ()
main = do
  putStrLn "in base 10:"
  mapM_ (print . ((,) <*> digRoot 10)) [627615, 39390, 588225, 393900588225]
```

```txt

in base 10:
(627615,(2,9))
(39390,(2,6))
(588225,(2,3))
(393900588225,(2,9))

```



```haskell
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Data.List (elemIndex, unfoldr)
import Numeric (readInt, showIntAtBase)

-- Return a pair consisting of the additive persistence and digital root of a
-- base b number.
digRoot :: Integer -> Integer -> (Integer, Integer)
digRoot b = find . zip [0 ..] . iterate (sum . toDigits b)
  where
    find = head . dropWhile ((>= b) . snd)

-- Print the additive persistence and digital root of a base b number (given as
-- a string).
printDigRoot :: Integer -> String -> IO ()
printDigRoot b s = do
  let (p, r) = digRoot b $ strToInt b s
  (putStrLn . unwords)
    [s, "-> additive persistence:", show p, "digital root:", intToStr b r]

--
-- Utility methods for dealing with numbers in different bases.
--
-- Convert a base b number to a list of digits, from least to most significant.
toDigits
  :: Integral a
  => a -> a -> [a]
toDigits b = unfoldr f
  where
    f 0 = Nothing
    f n = Just (swap (quotRem n b))

-- A list of digits, for bases up to 36.
digits :: String
digits = ['0' .. '9'] ++ ['A' .. 'Z']

-- Return a number's base b string representation.
intToStr
  :: (Integral a, Show a)
  => a -> a -> String
intToStr b n
  | b < 2 || b > 36 = error "intToStr: base must be in [2..36]"
  | otherwise = showIntAtBase b (digits !!) n ""

-- Return the number for the base b string representation.
strToInt
  :: Integral a
  => a -> String -> a
strToInt b =
  fst . head . readInt b (`elem` digits) (fromJust . (`elemIndex` digits))

main :: IO ()
main =
  mapM_
    (uncurry printDigRoot)
    [ (2, "1001100111011110")
    , (3, "2000000220")
    , (8, "5566623376301")
    , (10, "39390")
    , (16, "99DE")
    , (36, "50YE8N29")
    , (36, "37C71GOYNYJ25M3JTQQVR0FXUK0W9QM71C1LVN")
    ]
```

```txt

1001100111011110 -> additive persistence: 3 digital root: 1
2000000220 -> additive persistence: 2 digital root: 2
5566623376301 -> additive persistence: 3 digital root: 4
39390 -> additive persistence: 2 digital root: 6
99DE -> additive persistence: 2 digital root: F
50YE8N29 -> additive persistence: 2 digital root: P
37C71GOYNYJ25M3JTQQVR0FXUK0W9QM71C1LVN -> additive persistence: 2 digital root: N
```



## Huginn


```huginn
main( argv_ ) {
	if ( size( argv_ ) < 2 ) {
		throw Exception( "usage: digital-root {NUM}" );
	}
	n = argv_[1];
	if ( ( size( n ) == 0 ) || ( n.find_other_than( "0123456789" ) >= 0 ) ) {
		throw Exception( "{} is not a number".format( n ) );
	}
	shift = integer( '0' ) + 1;
	acc = 0;
	for ( d : n ) {
		acc = 1 + ( acc + integer( d ) - shift ) % 9;
	}
	print( "{}\n".format( acc ) );
	return ( 0 );
}
```


=={{header|Icon}} and {{header|Unicon}}==
The following works in both languages:

```unicon
procedure main(A)
    every m := n := integer(!A) do {
       ap := 0
       while (*n > 1) do (ap +:= 1, n := sumdigits(n))
       write(m," has additive persistence of ",ap," and digital root of ",n)
       }
end

procedure sumdigits(n)
    s := 0
    n ? while s +:= move(1)
    return s
end
```

```txt

->dr 627615 39390 588225 393900588225
627615 has additive persistence of 2 and digital root of 9
39390 has additive persistence of 2 and digital root of 6
588225 has additive persistence of 2 and digital root of 3
393900588225 has additive persistence of 2 and digital root of 9
->

```



## J



```J
digrot=: +/@(#.inv~&10)^:_
addper=: _1 + [: # +/@(#.inv~&10)^:a:
```


Example use:


```J
   (, addper, digrot)&> 627615 39390 588225 393900588225
      627615 2 9
       39390 2 6
      588225 2 3
393900588225 2 9
```


Here's an equality operator for comparing these digital roots:


```J
equals=: =&(9&|)"0
```


table of results:


```J
   equals table i. 10
┌──────┬───────────────────┐
│equals│0 1 2 3 4 5 6 7 8 9│
├──────┼───────────────────┤
│0     │1 0 0 0 0 0 0 0 0 1│
│1     │0 1 0 0 0 0 0 0 0 0│
│2     │0 0 1 0 0 0 0 0 0 0│
│3     │0 0 0 1 0 0 0 0 0 0│
│4     │0 0 0 0 1 0 0 0 0 0│
│5     │0 0 0 0 0 1 0 0 0 0│
│6     │0 0 0 0 0 0 1 0 0 0│
│7     │0 0 0 0 0 0 0 1 0 0│
│8     │0 0 0 0 0 0 0 0 1 0│
│9     │1 0 0 0 0 0 0 0 0 1│
└──────┴───────────────────┘
```


If digital roots other than 10 are desired, the modifier ~&10 can be removed from the above definitions of <code>digrot</code> and <code>addper</code>, and the base can be supplied as a left argument.  Since this is a simplification, these definitions are shown here:


```J
digrt=: +/@(#.inv)^:_
addpr=: _1 + [: # +/@(#.inv)^:a:
```


Note that these routines merely calculate results, which are numbers.  If you want the result to be displayed in some other base converting the result from numbers to character strings needs an additional step.  Since that's currently not a part of the task, this is left as an exercise for the reader.

Example use (note: names spelled slightly different for the updated definitions):


```J
   10 digrt 627615
9
   10 addpr 627615
2
```



## Java

;<nowiki>Code:</nowiki>

```java
import java.math.BigInteger;

class DigitalRoot
{
  public static int[] calcDigitalRoot(String number, int base)
  {
    BigInteger bi = new BigInteger(number, base);
    int additivePersistence = 0;
    if (bi.signum() < 0)
      bi = bi.negate();
    BigInteger biBase = BigInteger.valueOf(base);
    while (bi.compareTo(biBase) >= 0)
    {
      number = bi.toString(base);
      bi = BigInteger.ZERO;
      for (int i = 0; i < number.length(); i++)
        bi = bi.add(new BigInteger(number.substring(i, i + 1), base));
      additivePersistence++;
    }
    return new int[] { additivePersistence, bi.intValue() };
  }

  public static void main(String[] args)
  {
    for (String arg : args)
    {
      int[] results = calcDigitalRoot(arg, 10);
      System.out.println(arg + " has additive persistence " + results[0] + " and digital root of " + results[1]);
    }
  }
}
```

```txt
java DigitalRoot 627615 39390 588225 393900588225
627615 has additive persistence 2 and digital root of 9
39390 has additive persistence 2 and digital root of 6
588225 has additive persistence 2 and digital root of 3
393900588225 has additive persistence 2 and digital root of 9
```



## JavaScript


```JavaScript
/// Digital root of 'x' in base 'b'.
/// @return {addpers, digrt}
function digitalRootBase(x,b) {
   if (x < b)
      return {addpers:0, digrt:x};

   var fauxroot = 0;
   while (b <= x) {
      x = (x / b) | 0;
      fauxroot += x % b;
   }

   var rootobj = digitalRootBase(fauxroot,b);
   rootobj.addpers += 1;
   return rootobj;
}
```



## jq

digital_root(n) is defined here for decimals and strings representing decimals.

```jq
def do_until(condition; next):
  def u: if condition then . else (next|u) end;
  u;

# n may be a decimal number or a string representing a decimal number
def digital_root(n):
  # string-only version
  def dr:
    # state: [mdr, persist]
    do_until( .[0] | length == 1;
              [ (.[0] | explode | map(.-48) | add | tostring), .[1] + 1 ]
              );
  [n|tostring, 0] | dr | .[0] |= tonumber;

def neatly:
  . as $in
  | range(0;length)
  | "\(.): \($in[.])";

def rjust(n): tostring | (n-length)*" " + .;
```

'''Examples''':

```jq
(
 "          i : [DR, P]",
 (961038, 923594037444, 670033, 448944221089
 ) as $i
   | "\($i|rjust(12)): \(digital_root($i))"
),
 "",
 "digital_root(\"1\" * 100000) => \(digital_root( "1" * 100000))"
```

```sh
$ jq -M -n -r -c -f Digital_root.jq

          i : [DR, P]
      961038: [9,2]
923594037444: [9,2]
      670033: [1,3]
448944221089: [1,3]

digital_root("1" * 100000) => [1,2]
```



## Julia

```julia
function digitalroot(n::Integer, bs::Integer=10)
    if n < 0 || bs < 2 throw(DomainError()) end
    ds, pers = n, 0
    while bs ≤ ds
        ds = sum(digits(ds, bs))
        pers += 1
    end
    return pers, ds
end

for i in [627615, 39390, 588225, 393900588225, big(2) ^ 100]
    pers, ds = digitalroot(i)
    println(i, " has persistence ", pers, " and digital root ", ds)
end
```


```txt
627615 has persistence 2 and digital root 9
39390 has persistence 2 and digital root 6
588225 has persistence 2 and digital root 3
393900588225 has persistence 2 and digital root 9
1267650600228229401496703205376 has persistence 2 and digital root 7
```



## K


```K

/ print digital root and additive persistence
prt: {`"Digital root = ", x, `"Additive persistence = ",y}
/ sum of digits of an integer
sumdig: {d::(); (0<){d::d,x!10; x%:10}/x; +/d}
/ compute digital root and additive persistence
digroot: {sm::sumdig x; ap::0; (9<){sm::sumdig x;ap::ap+1; x:sm}/x; prt[sm;ap]}

```


```txt

    digroot 627615
(`"Digital root = ";9;`"Additive persistence = ";2)
    digroot 39390
(`"Digital root = ";6;`"Additive persistence = ";2)
    digroot 588225
(`"Digital root = ";3;`"Additive persistence = ";2)
    digroot 393900588225
(`"Digital root = ";9;`"Additive persistence = ";2)
    digroot 14
(`"Digital root = ";5;`"Additive persistence = ";1)
    digroot 3
(`"Digital root = ";3;`"Additive persistence = ";0)



```



## Kotlin


```scala
// version 1.0.6

fun sumDigits(n: Long): Int = when {
        n < 0L -> throw IllegalArgumentException("Negative numbers not allowed")
        else   -> {
            var sum = 0
            var nn  = n
            while (nn > 0L) {
                sum += (nn % 10).toInt()
                nn /= 10
            }
            sum
        }
    }

fun digitalRoot(n: Long): Pair<Int, Int> = when {
        n < 0L  -> throw IllegalArgumentException("Negative numbers not allowed")
        n < 10L -> Pair(n.toInt(), 0)
        else    -> {
            var dr = n
            var ap = 0
            while (dr > 9L) {
                dr = sumDigits(dr).toLong()
                ap++
            }
            Pair(dr.toInt(), ap)
        }
    }

fun main(args: Array<String>) {
    val a = longArrayOf(1, 14, 267, 8128, 627615, 39390, 588225, 393900588225)
    for (n in a) {
        val(dr, ap) = digitalRoot(n)
        println("${n.toString().padEnd(12)} has additive persistence $ap and digital root of $dr")
    }
}
```


```txt

1            has additive persistence 0 and digital root of 1
14           has additive persistence 1 and digital root of 5
267          has additive persistence 2 and digital root of 6
8128         has additive persistence 3 and digital root of 1
627615       has additive persistence 2 and digital root of 9
39390        has additive persistence 2 and digital root of 6
588225       has additive persistence 2 and digital root of 3
393900588225 has additive persistence 2 and digital root of 9

```



## Lua

With function sum_digits from [http://rosettacode.org/wiki/Sum_digits_of_an_integer#Lua]

```lua
function digital_root(n, base)
    p = 0
    while n > 9.5 do
        n = sum_digits(n, base)
        p = p + 1
    end
    return n, p
end

print(digital_root(627615, 10))
print(digital_root(39390, 10))
print(digital_root(588225, 10))
print(digital_root(393900588225, 10))
```

```txt
9       2
6       2
3       2
9       2
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
seq[n_, b_] := FixedPointList[Total[IntegerDigits[#, b]] &, n];
root[n_Integer, base_: 10] := If[base == 10, #, BaseForm[#, base]] &[Last[seq[n, base]]]
persistance[n_Integer, base_: 10] := Length[seq[n, base]] - 2;
```

```txt
 root /@ {627615, 39390, 588225 , 393900, 588225, 670033, 448944221089}
{9, 6, 3, 6, 3, 1, 1}

persistance /@ {627615, 39390, 588225 , 393900, 588225, 670033, 448944221089}
{2, 2, 2, 2, 2, 3, 3}

root[16^^14E344, 16]
f
 16
```


=={{header|Modula-2}}==

```modula2
MODULE DigitalRoot;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE Root =
    RECORD
        persistence,root : LONGINT;
    END;

PROCEDURE digitalRoot(inRoot,base : LONGINT) : Root;
VAR root,persistence,num : LONGINT;
BEGIN
    root := ABS(inRoot);
    persistence := 0;
    WHILE root>=base DO
        num := root;
        root := 0;
        WHILE num#0 DO
            root := root + (num MOD base);
            num := num DIV base;
        END;
        INC(persistence)
    END;
    RETURN Root{persistence, root}
END digitalRoot;

PROCEDURE Print(n,b : LONGINT);
VAR
    buf : ARRAY[0..63] OF CHAR;
    r : Root;
BEGIN
    r := digitalRoot(n,b);
    FormatString("%u (base %u): persistence=%u, digital root=%u\n", buf, n, b, r.persistence, r.root);
    WriteString(buf)
END Print;

VAR
    buf : ARRAY[0..63] OF CHAR;
    b,n : LONGINT;
    r : Root;
BEGIN
    Print(1,10);
    Print(14,10);
    Print(267,10);
    Print(8128,10);
    Print(39390,10);
    Print(627615,10);
    Print(588225,10);

    ReadChar
END DigitalRoot.
```



## NetRexx


```NetRexx
/* NetRexx ************************************************************
* Test digroot
**********************************************************************/
Say 'number -> digital_root persistence'
test_digroot(7           ,7, 0)
test_digroot(627615      ,9, 2)
test_digroot(39390       ,6, 2)
test_digroot(588225      ,3, 2)
test_digroot(393900588225,9, 2)
test_digroot(393900588225,9, 3)   /* test error case */

method test_digroot(n,dx,px) static
res=digroot(n)
Parse res d p
If d=dx & p=px Then tag='ok'
               Else tag='expected:' dx px
Say n '->' d p tag

method digroot(n) static
/**********************************************************************
* Compute the digital root and persistence of the given decimal number
* 19.08.2012 Walter Pachl derived from Rexx
**************************** Bottom of Data **************************/
p=0                                 /* persistence                   */
Loop While n.length()>1             /* more than one digit in n      */
  s=0                               /* initialize sum                */
  p=p+1                             /* increment persistence         */
  Loop while n<>''                  /* as long as there are digits   */
    Parse n c +1 n                  /* pick the first one            */
    s=s+c                           /* add to the new sum            */
    End
  n=s                               /* the 'new' number              */
  End
return n p                          /* return root and persistence   */
```

```txt

number -> digital_root persistence
7 -> 7 0 ok
627615 -> 9 2 ok
39390 -> 6 2 ok
588225 -> 3 2 ok
393900588225 -> 9 2 ok
393900588225 -> 9 2 expected: 9 3

```



## Nim


```nim
import strutils

proc droot(n: int64): auto =
  var x = @[n]
  while x[x.high] > 10:
    var s = 0'i64
    for dig in $x[x.high]:
      s += parseInt("" & dig)
    x.add s
  return (x.len - 1, x[x.high])

for n in [627615'i64, 39390'i64, 588225'i64, 393900588225'i64]:
  let (a, d) = droot(n)
  echo align($n, 12)," has additive persistance ",a," and digital root of ",d
```

```txt
      627615 has additive persistance 2 and digital root of 9
       39390 has additive persistance 2 and digital root of 6
      588225 has additive persistance 2 and digital root of 3
393900588225 has additive persistance 2 and digital root of 9
```




## Oforth


Using result of sum digit task :


```Oforth
: sumDigits(n, base)  0 while(n) [ n base /mod ->n + ] ;

: digitalRoot(n, base)
   0 while(n 9 >) [ 1 + sumDigits(n, base) ->n ] n swap Pair new ;
```


```txt

[ 627615, 39390 , 588225, 393900588225 ] map(#[ 10 digitalRoot ]) println
[[9, 2], [6, 2], [3, 2], [9, 2]]

```



## Pascal

```Pascal
program DigitalRoot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, StrUtils;

// FPC has no Big mumbers implementation, Int64 will suffice.

procedure GetDigitalRoot(Value: Int64; Base: Byte; var DRoot, Pers: Integer);
var
  i: Integer;
  DigitSum: Int64;
begin
  Pers := 0;
  repeat
    Inc(Pers);
    DigitSum := 0;
    while Value > 0 do
    begin
      Inc(DigitSum, Value mod Base);
      Value := Value div Base;
    end;
    Value := DigitSum;
  until Value < Base;
  DRoot := Value;
End;

function IntToStrBase(Value: Int64; Base: Byte):String;
const
  // usable up to 36-Base
  DigitSymbols = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXY';
begin
  Result := '';
  while Value > 0 do
  begin
    Result := DigitSymbols[Value mod Base+1] + Result;
    Value := Value div Base;
  End;

End;

procedure Display(const Value: Int64; Base: Byte = 10);
var
  DRoot, Pers: Integer;
  StrValue: string;
begin
  GetDigitalRoot(Value, Base, DRoot, Pers);
  WriteLn(Format('%s(%d) has additive persistence %d and digital root %d.',
    [IntToStrBase(Value, Base), Base, Pers, DRoot]));
End;

begin
  WriteLn('--- Examples in 10-Base ---');
  Display(627615);
  Display(39390);
  Display(588225);
  Display(393900588225);

  WriteLn('--- Examples in 16-Base ---');
  Display(627615, 16);
  Display(39390, 16);
  Display(588225, 16);
  Display(393900588225, 16);

  ReadLn;
End.
```

```txt
--- Examples in 10-Base ---
627615(10) has additive persistence 2 and digital root 9.
39390(10) has additive persistence 2 and digital root 6.
588225(10) has additive persistence 2 and digital root 3.
393900588225(10) has additive persistence 2 and digital root 9.
--- Examples in 16-Base ---
9939F(16) has additive persistence 2 and digital root 15.
99DE(16) has additive persistence 2 and digital root 15.
8F9C1(16) has additive persistence 2 and digital root 15.
5BB64DFCC1(16) has additive persistence 2 and digital root 15.
```



## PARI/GP


```parigp
dsum(n)=my(s); while(n, s+=n%10; n\=10); s
additivePersistence(n)=my(s); while(n>9, s++; n=dsum(n)); s
digitalRoot(n)=if(n, (n-1)%9+1, 0)
```



## Perl


```perl
#!perl
use strict;
use warnings;
use List::Util qw(sum);

my @digit = (0..9, 'a'..'z');
my %digit = map { +$digit[$_], $_ } 0 .. $#digit;

sub base {
   my ($n, $b) = @_;
   $b ||= 10;
   die if $b > @digit;
   my $result = '';
   while( $n ) {
      $result .= $digit[ $n % $b ];
      $n = int( $n / $b );
   }
   reverse($result) || '0';
}

sub digi_root {
   my ($n, $b) = @_;
   my $inbase = base($n, $b);
   my $additive_persistance = 0;
   while( length($inbase) > 1 ) {
      ++$additive_persistance;
      $n = sum @digit{split //, $inbase};
      $inbase = base($n, $b);
   }
   $additive_persistance, $n;
}

MAIN: {
   my @numbers = (5, 627615, 39390, 588225, 393900588225);
   my @bases = (2, 3, 8, 10, 16, 36);
   my $fmt = "%25s(%2s): persistance = %s, root = %2s\n";

   if( eval { require Math::BigInt; 1 } ) {
      push @numbers, Math::BigInt->new("5814271898167303040368".
      "1039458302204471300738980834668522257090844071443085937");
   }

   for my $base (@bases) {
      for my $num (@numbers) {
         my $inbase = base($num, $base);
         $inbase = 'BIG' if length($inbase) > 25;
         printf $fmt, $inbase, $base, digi_root($num, $base);
      }
      print "\n";
   }
}
```

```txt

                      101( 2): persistance = 2, root =  1
     10011001001110011111( 2): persistance = 3, root =  1
         1001100111011110( 2): persistance = 3, root =  1
     10001111100111000001( 2): persistance = 3, root =  1
                      BIG( 2): persistance = 3, root =  1
                      BIG( 2): persistance = 4, root =  1

                       12( 3): persistance = 2, root =  1
            1011212221000( 3): persistance = 3, root =  1
               2000000220( 3): persistance = 2, root =  2
            1002212220010( 3): persistance = 3, root =  1
1101122201121110011000000( 3): persistance = 3, root =  1
                      BIG( 3): persistance = 4, root =  1

                        5( 8): persistance = 0, root =  5
                  2311637( 8): persistance = 3, root =  2
                   114736( 8): persistance = 3, root =  1
                  2174701( 8): persistance = 3, root =  1
            5566623376301( 8): persistance = 3, root =  4
                      BIG( 8): persistance = 3, root =  3

                        5(10): persistance = 0, root =  5
                   627615(10): persistance = 2, root =  9
                    39390(10): persistance = 2, root =  6
                   588225(10): persistance = 2, root =  3
             393900588225(10): persistance = 2, root =  9
                      BIG(10): persistance = 3, root =  4

                        5(16): persistance = 0, root =  5
                    9939f(16): persistance = 2, root = 15
                     99de(16): persistance = 2, root = 15
                    8f9c1(16): persistance = 2, root = 15
               5bb64dfcc1(16): persistance = 2, root = 15
                      BIG(16): persistance = 3, root =  7

                        5(36): persistance = 0, root =  5
                     dg9r(36): persistance = 2, root = 30
                      ue6(36): persistance = 2, root = 15
                     clvl(36): persistance = 2, root = 15
                 50ye8n29(36): persistance = 2, root = 25
                      BIG(36): persistance = 3, root = 17

```



## Perl 6


```perl6
sub digroot ($r, :$base = 10) {
    my $root = $r.base($base);
    my $persistence = 0;
    while $root.chars > 1 {
        $root = [+]($root.comb.map({:36($_)})).base($base);
        $persistence++;
    }
    $root, $persistence;
}

my @testnums =
    627615,
    39390,
    588225,
    393900588225,
    58142718981673030403681039458302204471300738980834668522257090844071443085937;

for 10, 8, 16, 36 -> $b {
    for @testnums -> $n {
        printf ":$b\<%s>\ndigital root %s, persistence %s\n\n",
            $n.base($b), digroot $n, :base($b);
    }
}
```

```txt
:10<627615>
digital root 9, persistence 2

:10<39390>
digital root 6, persistence 2

:10<588225>
digital root 3, persistence 2

:10<393900588225>
digital root 9, persistence 2

:10<58142718981673030403681039458302204471300738980834668522257090844071443085937>
digital root 4, persistence 3

:8<2311637>
digital root 2, persistence 3

:8<114736>
digital root 1, persistence 3

:8<2174701>
digital root 1, persistence 3

:8<5566623376301>
digital root 4, persistence 3

:8<10021347156245115014463623107370014314341751427033746320331121536631531505161175135161>
digital root 3, persistence 3

:16<9939F>
digital root F, persistence 2

:16<99DE>
digital root F, persistence 2

:16<8F9C1>
digital root F, persistence 2

:16<5BB64DFCC1>
digital root F, persistence 2

:16<808B9CDCA526832679323BE018CC70FA62E1BF3341B251AF666B345389F4BA71>
digital root 7, persistence 3

:36<DG9R>
digital root U, persistence 2

:36<UE6>
digital root F, persistence 2

:36<CLVL>
digital root F, persistence 2

:36<50YE8N29>
digital root P, persistence 2

:36<37C71GOYNYJ25M3JTQQVR0FXUK0W9QM71C1LVNCBWNRVNOJYPD>
digital root H, persistence 3
```

Or if you are more inclined to the functional programming persuasion, you can use the <tt>...</tt> sequence operator to calculate the values without side effects:

```perl6
sub digroot ($r, :$base = 10) {
    my &sum = { [+](.comb.map({:36($_)})).base($base) }

    return .[*-1], .elems-1
        given $r.base($base), &sum ...  { .chars == 1 }
}
```


## Phix


```Phix
procedure digital_root(atom n, integer base=10)
integer root, persistence = 1
atom work = n
    while 1 do
        root = 0
        while work!=0 do
            root += remainder(work,base)
            work = floor(work/base)
        end while
        if root<base then exit end if
        work = root
        persistence += 1
    end while
    printf(1,"%15d root: %d persistence: %d\n",{n,root,persistence})
end procedure

digital_root(627615)
digital_root(39390)
digital_root(588225)
digital_root(393900588225)
```

```txt

         627615 root: 9 persistence: 2
          39390 root: 6 persistence: 2
         588225 root: 3 persistence: 2
   393900588225 root: 9 persistence: 2

```



## PicoLisp


```PicoLisp
(for N (627615 39390 588225 393900588225)
   (for ((A . I) N  T  (sum format (chop I)))
      (T (> 10 I)
         (prinl N " has additive persistance " (dec A) " and digital root of " I ";") ) ) )
```

```txt
627615 has additive persistance 2 and digital root of 9;
39390 has additive persistance 2 and digital root of 6;
588225 has additive persistance 2 and digital root of 3;
393900588225 has additive persistance 2 and digital root of 9;
```



## PL/I


```pli
 digrt: Proc Options(main);
 /* REXX ***************************************************************
 * Test digroot
 **********************************************************************/

 Call digrtst('7');
 Call digrtst('627615');
 Call digrtst('39390');
 Call digrtst('588225');
 Call digrtst('393900588225');

 digrtst: Proc(n);
 Dcl n Char(100) Var;
 Dcl dr Pic'9';
 Dcl p  Dec Fixed(5);
 Call digroot(n,dr,p);
 Put Edit(n,dr,p)(skip,a,col(20),f(1),f(3));
 End;

 digroot: Proc(n,dr,p);
 /**********************************************************************
 * Compute the digital root and persistence of the given decimal number
 * 27.07.2012 Walter Pachl (derived from REXX)
 **********************************************************************/
 Dcl n Char(100) Var;
 Dcl dr Pic'9';
 Dcl p  Dec Fixed(5);
 Dcl s  Pic'(14)Z9';
 Dcl v  Char(100) Var;
 p=0;
 v=strip(n);                         /* copy the number               */
 If length(v)=1 Then
   dr=v;
 Else Do;
   Do While(length(v)>1);            /* more than one digit in v      */
     s=0;                            /* initialize sum                */
     p+=1;                           /* increment persistence         */
     Do i=1 To length(v);            /* loop over all digits          */
       dig=substr(v,i,1);            /* pick a digit                  */
       s=s+dig;                      /* add to the new sum            */
       End;
   /*Put Skip Data(v,p,s);*/
     v=strip(s);                     /* the 'new' number              */
     End;
   dr=Decimal(s,1,0);
   End;
 Return;
 End;

 strip: Proc(x) Returns(Char(100) Var);
 Dcl x Char(*);
 Dcl res Char(100) Var Init('');
 Do i=1 To length(x);
   If substr(x,i,1)>' ' Then
     res=res||substr(x,i,1);
   End;
 Return(res);
 End;
 End;
```

```txt

7                  7  0
627615             9  2
39390              6  2
588225             3  2
393900588225       9  2

```

Alternative:

```PL/I
digital: procedure options (main);  /* 29 April 2014 */
   declare 1 pict union,
             2 x picture '9999999999999',
             2 d(13) picture '9';
   declare ap fixed, n fixed (15);

   do n = 5, 627615, 39390, 588225, 393900588225, 99999999999;
      x = n;
      do ap = 1 by 1 until (x < 10);
         x = sum(d);
      end;
      put skip data (n, x, ap);
   end;

end digital;
```

Results:

```txt

N=                 5    PICT.X=0000000000005    AP=       1;
N=            627615    PICT.X=0000000000009    AP=       2;
N=             39390    PICT.X=0000000000006    AP=       2;
N=            588225    PICT.X=0000000000003    AP=       2;
N=      393900588225    PICT.X=0000000000009    AP=       2;
N=       99999999999    PICT.X=0000000000009    AP=       3;

```



## Potion


```potion
digital = (x) :
   dr = x string  # Digital Root.
   ap = 0  # Additive Persistence.
   while (dr length > 1) :
      sum = 0
      dr length times (i): sum = sum + dr(i) number integer.
      dr = sum string
      ap++
   .
   (x, " has additive persistence ", ap,
      " and digital root ", dr, ";\n") join print
.

digital(627615)
digital(39390)
digital(588225)
digital(393900588225)
```



## PowerShell

Uses the recursive function from the 'Sum Digits of an Integer' task.

```Powershell
function Get-DigitalRoot ($n)
{
    function Get-Digitalsum ($n)
    {
        if ($n -lt 10) {$n}
        else {
            ($n % 10) + (Get-DigitalSum ([math]::Floor($n / 10)))
        }
    }

    $ap = 0
    do {$n = Get-DigitalSum $n; $ap++}
    until ($n -lt 10)
    $DigitalRoot = [pscustomobject]@{
        'Sum' = $n
        'Additive Persistence' = $ap
    }
    $DigitalRoot
}
```

Command:

```txt

Get-DigitalRoot 65536

```

```txt

Sum                          Additive Persistence
---                          --------------------
  7                                             2

```


### Alternative Method


```powershell
function Get-DigitalRoot {
    param($n)
    $ap = 0
    do {$n = Invoke-Expression ("0"+([string]$n -split "" -join "+")+"0"); $ap++} while ($n -ge 10)
    [PSCustomObject]@{
        DigitalRoot = $n
        AdditivePersistence = $ap
    }
}
```

Command:

```txt

Get-DigitalRoot 627615

```

```txt

Name                 Value
----                 -----
AdditivePersistence  2
DigitalRoot          9

```



## PureBasic


```purebasic
; if you just want the DigitalRoot
; Procedure.q DigitalRoot(N.q) apparently will do
; i must have missed something because it seems too simple
; http://en.wikipedia.org/wiki/Digital_root#Congruence_formula

Procedure.q DigitalRoot(N.q)
Protected M.q=N%9
if M=0:ProcedureReturn 9
Else  :ProcedureReturn M:EndIf
EndProcedure

; there appears to be a proof guarantying that Len(N$)<=1 for some X
; http://en.wikipedia.org/wiki/Digital_root#Proof_that_a_constant_value_exists

Procedure.s DigitalRootandPersistance(N.q)
Protected r.s,t.s,X.q,M.q,persistance,N$=Str(N)
M=DigitalRoot(N.q) ; just a test to see if we get the same DigitalRoot via the Congruence_formula

Repeat
X=0:Persistance+1

For i=1 to Len(N$)       ; finding X as the sum of the digits of N
X+Val(Mid(N$,i,1))
Next

N$=Str(X)
If Len(N$)<=1:Break:EndIf ; If Len(N$)<=1:Break:EndIf
Forever

If Not (X-M)=0:t.s=" Error in my logic":else:t.s=" ok":EndIf

r.s=RSet(Str(N),15)+" has additive persistance "+Str(Persistance)
r.s+" and digital root of X(slow) ="+Str(X)+" M(fast) ="+Str(M)+t.s
ProcedureReturn r.s
EndProcedure

NewList Nlist.q()
AddElement(Nlist()) : Nlist()=627615
AddElement(Nlist()) : Nlist()=39390
AddElement(Nlist()) : Nlist()=588225
AddElement(Nlist()) : Nlist()=393900588225

FirstElement(Nlist())

ForEach Nlist()
N.q=Nlist()
; cw(DigitalRootandPersistance(N))
Debug DigitalRootandPersistance(N)
Next
```

```txt

         627615 has additive persistance 2 and digital root of X(slow) =9 M(fast) =9 ok
          39390 has additive persistance 2 and digital root of X(slow) =6 M(fast) =6 ok
         588225 has additive persistance 2 and digital root of X(slow) =3 M(fast) =3 ok
   393900588225 has additive persistance 2 and digital root of X(slow) =9 M(fast) =9 ok

```



## Python


### Procedural


```python
def digital_root (n):
    ap = 0
    n = abs(int(n))
    while n >= 10:
        n = sum(int(digit) for digit in str(n))
        ap += 1
    return ap, n

if __name__ == '__main__':
    for n in [627615, 39390, 588225, 393900588225, 55]:
        persistance, root = digital_root(n)
        print("%12i has additive persistance %2i and digital root %i."
              % (n, persistance, root))
```


```txt
      627615 has additive persistance  2 and digital root 9.
       39390 has additive persistance  2 and digital root 6.
      588225 has additive persistance  2 and digital root 3.
393900588225 has additive persistance  2 and digital root 9.
          55 has additive persistance  2 and digital root 1.
```



### Composition of pure functions


A useful functional abstraction for this kind of pattern is '''until''' ''p f x'' (predicate, function, start value).

For the digit sum, we can fuse the two-pass composition of '''sum''' and '''for''' in the procedural version to a single [[Catamorphism|'fold' or catamorphism]] using '''reduce'''.

The tabulation of '''f(x)''' values can be derived by a generalised function over the '''f''', a header string '''s''', and the input '''xs''':


```python
from functools import (reduce)


# main :: IO ()
def main():
    print (
        tabulated(digitalRoot)(
            'Integer -> (additive persistence, digital root):'
        )([627615, 39390, 588225, 393900588225, 55])
    )


# digitalRoot :: Int -> (Int, Int)
def digitalRoot(n):
    '''Integer -> (additive persistence, digital root)'''

    # f :: (Int, Int) -> (Int, Int)
    def f(pn):
        p, n = pn
        return (
            1 + p,
            reduce(lambda a, x: a + int(x), str(n), 0)
        )

    # p :: (Int , Int) -> Bool
    def p(pn):
        return 10 > pn[1]

    return until(p)(f)(
        (0, abs(int(n)))
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    return lambda f: lambda x: g(f(x))


# tabulated :: (a -> b) -> String -> String
def tabulated(f):
    '''function -> heading -> input List -> tabulated output string'''
    def go(s, xs):
        fw = compose(len)(str)
        w = fw(max(xs, key=fw))
        return s + '\n' + '\n'.join(list(map(
            lambda x: str(x).rjust(w, ' ') + ' -> ' + str(f(x)), xs
        )))
    return lambda s: lambda xs: go(s, xs)


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


if __name__ == '__main__':
    main()
```

```txt
Integer -> (additive persistence, digital root):
      627615 -> (2, 9)
       39390 -> (2, 6)
      588225 -> (2, 3)
393900588225 -> (2, 9)
          55 -> (2, 1)
```



## R

The code prints digital root and persistence seperately

```R
y=1
digital_root=function(n){
  x=sum(as.numeric(unlist(strsplit(as.character(n),""))))
  if(x<10){
    k=x
  }else{
    y=y+1
    assign("y",y,envir = globalenv())
    k=digital_root(x)
  }
  return(k)
}
print("Given number has additive persistence",y)
```



## Racket


```racket
#lang racket
(define/contract (additive-persistence/digital-root n (ap 0))
  (->* (natural-number/c) (natural-number/c) (values natural-number/c natural-number/c))
  (define/contract (sum-digits x (acc 0))
    (->* (natural-number/c) (natural-number/c) natural-number/c)
    (if (= x 0)
        acc
        (let-values (((q r) (quotient/remainder x 10)))
          (sum-digits q (+ acc r)))))
  (if (< n 10)
      (values ap n)
      (additive-persistence/digital-root (sum-digits n) (+ ap 1))))

(module+ test
  (require rackunit)

  (for ((n (in-list '(627615 39390 588225 393900588225)))
        (ap (in-list '(2 2 2 2)))
        (dr (in-list '(9 6 3 9))))
    (call-with-values
      (lambda () (additive-persistence/digital-root n))
      (lambda (a d)
        (check-equal? a ap)
        (check-equal? d dr)
        (printf ":~a has additive persistence ~a and digital root of ~a;~%" n a d)))))
```

```txt
627615 has additive persistence 2 and digital root of 9
39390 has additive persistence 2 and digital root of 6
588225 has additive persistence 2 and digital root of 3
393900588225 has additive persistence 2 and digital root of 9
```



## REXX


### version 1


```rexx
/* REXX ***************************************************************
* Test digroot
**********************************************************************/
                                                 /*           n r p */
say right(7           ,12) digroot(7           ) /*           7 7 0 */
say right(627615      ,12) digroot(627615      ) /*      627615 9 2 */
say right(39390       ,12) digroot(39390       ) /*       39390 6 2 */
say right(588225      ,12) digroot(588225      ) /*      588225 3 2 */
say right(393900588225,12) digroot(393900588225) /*393900588225 9 2 */
  Exit
digroot: Procedure
/**********************************************************************
* Compute the digital root and persistence of the given decimal number
* 25.07.2012 Walter Pachl
**************************** Bottom of Data **************************/
Parse Arg n                         /* the number                    */
p=0                                 /* persistence                   */
Do While length(n)>1                /* more than one digit in n      */
  s=0                               /* initialize sum                */
  p=p+1                             /* increment persistence         */
  Do while n<>''                    /* as long as there are digits   */
    Parse Var n c +1 n              /* pick the first one            */
    s=s+c                           /* add to the new sum            */
    End
  n=s                               /* the 'new' number              */
  End
return n p                          /* return root and persistence   */
```



### version 2


```rexx
/*REXX program  calculates and displays the  digital root  and  additive persistence.   */
say 'digital'                                    /*display the  1st  line of the header.*/
say "  root  persistence" center('number',77)    /*   "     "   2nd    "   "  "     "   */
say "═══════ ═══════════"   left('', 77, "═")    /*   "     "   3rd    "   "  "     "   */
call digRoot       627615
call digRoot        39390
call digRoot       588225
call digRoot 393900588225
call digRoot 89999999999999999999999999999999999999999999999999999999999999999999999999999
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
digRoot: procedure;  parse arg x 1 ox            /*get the number, also get another copy*/
             do pers=0  while length(x)\==1; $=0 /*keep summing until digRoot ≡ 1 digit.*/
               do j=1  for length(x)             /*add each digit in the decimal number.*/
               $=$+substr(x,j,1)                 /*add a decimal digit to digital root. */
               end   /*j*/
             x=$                                 /*a  'new' num,  it may be multi-digit.*/
             end     /*pers*/
         say center(x,7)   center(pers,11)   ox  /*display a nicely formatted line.     */
         return
```

'''output'''   when using the (internal) set of numbers:

```txt

digital
  root  persistence                                    number
═══════ ═══════════ ═════════════════════════════════════════════════════════════════════════════
   9         2      627615
   6         2      39390
   3         2      588225
   9         2      393900588225
   8         3      89999999999999999999999999999999999999999999999999999999999999999999999999999

```



### version 3

This subroutine version can handle numbers with signs, blanks, and/or decimal points.

```rexx
   ∙
   ∙
   ∙
/*──────────────────────────────────────────────────────────────────────────────────────*/
digRoot: procedure;  parse arg x 1 ox            /*get the number, also get another copy*/
             do pers=0  while length(x)\==1; $=0 /*keep summing until digRoot ≡ 1 digit.*/
                 do j=1  for length(x)           /*add each digit in the decimal number.*/
                 ?=substr(x, j, 1)               /*pick off a character, maybe a digit ?*/
                 if datatype(?, 'W')  then $=$+? /*add a decimal digit to digital root. */
                 end   /*j*/
             x=$                                 /*a  'new' num,  it may be multi-digit.*/
             end       /*pers*/
         say center(x,7)   center(pers,11)   ox  /*display a nicely formatted line.     */
         return
```

'''output'''   is the same as the 2<sup>nd</sup> REXX version.




## Ring


```ring

c = 0
see "Digital root of 627615 is " + digitRoot(627615, 10) + " persistance is " + c + nl
see "Digital root of 39390  is " + digitRoot(39390, 10) +  " persistance is " + c + nl
see "Digital root of 588225 is " + digitRoot(588225, 10) +  " persistance is " + c + nl
see "Digital root of 9992   is " + digitRoot(9992, 10) +  " persistance is " + c + nl

func digitRoot n,b
     c = 0
     while n >= b
           c = c + 1
           n = digSum(n, b)
     end
     return n

func digSum n, b
     s = 0
     while n != 0
           q = floor(n / b)
           s = s + n - q * b
           n = q
     end
     return s

```



## Ruby


```ruby
class String
  def digroot_persistence(base=10)
    num = self.to_i(base)
    persistence = 0
    until num < base do
      num = num.to_s(base).each_char.reduce(0){|m, c| m + c.to_i(base) }
      persistence += 1
    end
    [num.to_s(base), persistence]
  end
end

puts "--- Examples in 10-Base ---"
%w(627615 39390 588225 393900588225).each do |str|
  puts "%12s has a digital root of %s and a persistence of %s." % [str, *str.digroot_persistence]
end
puts "\n--- Examples in other Base ---"
format = "%s base %s has a digital root of %s and a persistence of %s."
[["101101110110110010011011111110011000001", 2],
 [ "5BB64DFCC1", 16],
 ["5", 8],
 ["50YE8N29", 36]].each do |(str, base)|
   puts format % [str, base, *str.digroot_persistence(base)]
end
```

```txt

--- Examples in 10-Base ---
      627615 has a digital root of 9 and a persistence of 2.
       39390 has a digital root of 6 and a persistence of 2.
      588225 has a digital root of 3 and a persistence of 2.
393900588225 has a digital root of 9 and a persistence of 2.

--- Examples in other Base ---
101101110110110010011011111110011000001 base 2 has a digital root of 1 and a persistence of 3.
5BB64DFCC1 base 16 has a digital root of f and a persistence of 2.
5 base 8 has a digital root of 5 and a persistence of 0.
50YE8N29 base 36 has a digital root of p and a persistence of 2.

```



## Run BASIC


```runbasic
print "Digital root of 627615       is "; digitRoot$(627615, 10)
print "Digital root of 39390        is "; digitRoot$(39390, 10)
print "Digital root of 588225       is "; digitRoot$(588225, 10)
print "Digital root of 393900588225 is "; digitRoot$(393900588225, 10)
print "Digital root of 9992         is "; digitRoot$(9992, 10)
END

function digitRoot$(n,b)
WHILE n >= b
  c = c + 1
  n = digSum(n, b)
wend
digitRoot$ = n;" persistance is ";c
end function

function digSum(n, b)
WHILE n <> 0
  q = INT(n / b)
  s = s + n - q * b
  n = q
wend
digSum = s
end function
```

```txt
Digital root of 627615       is 9 persistance is 2
Digital root of 39390        is 6 persistance is 2
Digital root of 588225       is 3 persistance is 2
Digital root of 393900588225 is 9 persistance is 2
Digital root of 9992         is 2 persistance is 3
```



## Rust


```rust
fn sum_digits(mut n: u64, base: u64) -> u64 {
    let mut sum = 0u64;
    while n > 0 {
        sum = sum + (n % base);
        n = n / base;
    }
    sum
}

// Returns tuple of (additive-persistence, digital-root)
fn digital_root(mut num: u64, base: u64) -> (u64, u64) {
    let mut pers = 0;
    while num >= base {
        pers = pers + 1;
        num = sum_digits(num, base);
    }
    (pers, num)
}

fn main() {

    // Test base 10
    let values = [627615u64, 39390u64, 588225u64, 393900588225u64];
    for &value in values.iter() {
        let (pers, root) = digital_root(value, 10);
        println!("{} has digital root {} and additive persistance {}",
                 value,
                 root,
                 pers);
    }

    println!("");

    // Test base 16
    let values_base16 = [0x7e0, 0x14e344, 0xd60141, 0x12343210];
    for &value in values_base16.iter() {
        let (pers, root) = digital_root(value, 16);
        println!("0x{:x} has digital root 0x{:x} and additive persistance 0x{:x}",
                 value,
                 root,
                 pers);
    }
}
```

```txt

627615 has digital root 9 and additive persistance 2
39390 has digital root 6 and additive persistance 2
588225 has digital root 3 and additive persistance 2
393900588225 has digital root 9 and additive persistance 2

0x7e0 has digital root 0x6 and additive persistance 0x2
0x14e344 has digital root 0xf and additive persistance 0x2
0xd60141 has digital root 0xa and additive persistance 0x2
0x12343210 has digital root 0x1 and additive persistance 0x2

```



## Scala


```scala
def digitalRoot(x:BigInt, base:Int=10):(Int,Int) = {
  def sumDigits(x:BigInt):Int=x.toString(base) map (_.asDigit) sum
  def loop(s:Int, c:Int):(Int,Int)=if (s < 10) (s, c) else loop(sumDigits(s), c+1)
  loop(sumDigits(x), 1)
}

Seq[BigInt](627615, 39390, 588225, BigInt("393900588225")) foreach {x =>
  var (s, c)=digitalRoot(x)
  println("%d has additive persistance %d and digital root of %d".format(x,c,s))
}
var (s, c)=digitalRoot(0x7e0, 16)
println("%x has additive persistance %d and digital root of %d".format(0x7e0,c,s))
```

```txt
627615 has additive persistance 2 and digital root of 9
39390 has additive persistance 2 and digital root of 6
588225 has additive persistance 2 and digital root of 3
393900588225 has additive persistance 2 and digital root of 9
7e0 has additive persistance 2 and digital root of 6
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const func bigInteger: digitalRoot (in var bigInteger: num, in bigInteger: base, inout bigInteger: persistence) is func
  result
     var bigInteger: sum is 0_;
  begin
    persistence := 0_;
    while num >= base do
      sum := 0_;
      while num > 0_ do
        sum +:= num rem base;
        num := num div base;
      end while;
      num := sum;
      incr(persistence);
    end while;
  end func;

const proc: main is func
  local
    var bigInteger: num is 0_;
    var bigInteger: root is 0_;
    var bigInteger: persistence is 0_;
  begin
    for num range [] (627615_, 39390_, 588225_, 393900588225_) do
      root := digitalRoot(num, 10_, persistence);
      writeln(num <& " has additive persistence " <& persistence <& " and digital root of " <& root);
    end for;
  end func;
```

```txt

627615 has additive persistence 2 and digital root of 9
39390 has additive persistence 2 and digital root of 6
588225 has additive persistence 2 and digital root of 3
393900588225 has additive persistence 2 and digital root of 9

```



## Sidef

```ruby
func digroot (r, base = 10) {
    var root = r.base(base)
    var persistence = 0
    while (root.len > 1) {
        root = root.chars.map{|n| Number(n, 36) }.sum(0).base(base)
        ++persistence
    }
    return(persistence, root)
}

var nums = [5, 627615, 39390, 588225, 393900588225]
var bases = [2, 3, 8, 10, 16, 36]
var fmt = "%25s(%2s): persistance = %s, root = %2s\n"

nums << (550777011503 *
         105564897893993412813307040538786690718089963180462913406682192479)

bases.each { |b|
    nums.each { |n|
        var x = n.base(b)
        x = 'BIG' if (x.len > 25)
        fmt.printf(x, b, digroot(n, b))
    }
    print "\n"
}
```

```txt

                      101( 2): persistance = 2, root =  1
     10011001001110011111( 2): persistance = 3, root =  1
         1001100111011110( 2): persistance = 3, root =  1
     10001111100111000001( 2): persistance = 3, root =  1
                      BIG( 2): persistance = 3, root =  1
                      BIG( 2): persistance = 4, root =  1

                       12( 3): persistance = 2, root =  1
            1011212221000( 3): persistance = 3, root =  1
               2000000220( 3): persistance = 2, root =  2
            1002212220010( 3): persistance = 3, root =  1
1101122201121110011000000( 3): persistance = 3, root =  1
                      BIG( 3): persistance = 4, root =  1

                        5( 8): persistance = 0, root =  5
                  2311637( 8): persistance = 3, root =  2
                   114736( 8): persistance = 3, root =  1
                  2174701( 8): persistance = 3, root =  1
            5566623376301( 8): persistance = 3, root =  4
                      BIG( 8): persistance = 3, root =  3

                        5(10): persistance = 0, root =  5
                   627615(10): persistance = 2, root =  9
                    39390(10): persistance = 2, root =  6
                   588225(10): persistance = 2, root =  3
             393900588225(10): persistance = 2, root =  9
                      BIG(10): persistance = 3, root =  4

                        5(16): persistance = 0, root =  5
                    9939f(16): persistance = 2, root =  f
                     99de(16): persistance = 2, root =  f
                    8f9c1(16): persistance = 2, root =  f
               5bb64dfcc1(16): persistance = 2, root =  f
                      BIG(16): persistance = 3, root =  7

                        5(36): persistance = 0, root =  5
                     dg9r(36): persistance = 2, root =  u
                      ue6(36): persistance = 2, root =  f
                     clvl(36): persistance = 2, root =  f
                 50ye8n29(36): persistance = 2, root =  p
                      BIG(36): persistance = 3, root =  h

```



## SmileBASIC


```smilebasic
DEF DIGITAL_ROOT N OUT DR,AP
 AP=0
 DR=N
 WHILE DR>9
  INC AP
  STRDR$=STR$(DR)
  NEWDR=0
  FOR I=0 TO LEN(STRDR$)-1
   INC NEWDR,VAL(MID$(STRDR$,I,1))
  NEXT
  DR=NEWDR
 WEND
END
```



## Tcl


```tcl
package require Tcl 8.5
proc digitalroot num {
    for {set p 0} {[string length $num] > 1} {incr p} {
	set num [::tcl::mathop::+ {*}[split $num ""]]
    }
    list $p $num
}

foreach n {627615 39390 588225 393900588225} {
    lassign [digitalroot $n] p r
    puts [format "$n has additive persistence $p and digital root of $r"]
}
```

```txt

627615 has additive persistence 2 and digital root of 9
39390 has additive persistence 2 and digital root of 6
588225 has additive persistence 2 and digital root of 3
393900588225 has additive persistence 2 and digital root of 9

```


=={{header|TI-83 BASIC}}==

```ti83b
:ClrHome
­:1→X
:Input ">",Str1
:Str1→Str2
:Repeat L≤1
:Disp Str1
:length(Str1→L
:L→dim(L₁
:seq(expr(sub(Str1,A,1)),A,1,L)→L₁
:sum(L₁→N
:{0,.5,1→L₂
:NL₂→L₃
:Med-Med L₂,L₃,Y₁
:Equ►String(Y₁,Str1
:sub(Str1,1,length(Str1)-3→Str1
:X+1→X
:End
:Pause
:ClrHome
:Disp Str2,"DIGITAL ROOT",expr(Str1),"ADDITIVE","PERSISTENCE",X
:Pause
```

```txt
627615
DIGITAL ROOT 9
ADDITIVE PERSISTENCE 2

39390
DIGITAL ROOT 6
ADDITIVE PERSISTENCE 2

588225
DIGITAL ROOT 3
ADDITIVE PERSISTENCE 2

393900588225
DIGITAL ROOT 9
ADDITIVE PERSISTENCE 2
```



## uBasic/4tH

<lang>PRINT "Digital root of 627615 is "; FUNC(_FNdigitalroot(627615, 10)) ;
PRINT " (additive persistence " ; Pop(); ")"

PRINT "Digital root of 39390 is "; FUNC(_FNdigitalroot(39390, 10)) ;
PRINT " (additive persistence " ; Pop(); ")"

PRINT "Digital root of 588225 is "; FUNC(_FNdigitalroot(588225, 10)) ;
PRINT " (additive persistence " ; Pop(); ")"

PRINT "Digital root of 9992 is "; FUNC(_FNdigitalroot(9992, 10)) ;
PRINT " (additive persistence " ; Pop(); ")"
END


_FNdigitalroot Param(2)
  Local (1)
  c@ = 0
  Do Until a@ < b@
    c@ = c@ + 1
    a@ = FUNC(_FNdigitsum (a@, b@))
  Loop
  Push (c@)                            ' That's how uBasic handles an extra
Return (a@)                            ' return value: on the stack

_FNdigitsum Param (2)
  Local (2)
  d@ =0
  Do While a@ # 0
    c@ = a@ / b@
    d@ = d@ + a@ - (c@ * b@)
    a@ = c@
  Loop
Return (d@)
```

```txt
Digital root of 627615 is 9 (additive persistence 2)
Digital root of 39390 is 6 (additive persistence 2)
Digital root of 588225 is 3 (additive persistence 2)
Digital root of 9992 is 2 (additive persistence 3)

0 OK, 0:737
```



## UNIX Shell


```bash
#!/usr/bin/env bash

numbers=(627615 39390 588225 393900588225 55)
declare root

for number in "${numbers[@]}"; do
    declare -i iterations
    root="${number}"
    while [[ "${#root}" -ne 1 ]]; do
        root="$(( $(fold -w1 <<<"${root}" | xargs | sed 's/ /+/g') ))"
        iterations+=1
    done
    echo -e "${number} has additive persistence ${iterations} and digital root ${root}"
    unset iterations
done | column -t
```

```txt
627615        has  additive  persistence  2  and  digital  root  9
39390         has  additive  persistence  2  and  digital  root  6
588225        has  additive  persistence  2  and  digital  root  3
393900588225  has  additive  persistence  2  and  digital  root  9
55            has  additive  persistence  2  and  digital  root  1
```



## VBA


```vb
Option Base 1
Private Sub digital_root(n As Variant)
    Dim s As String, t() As Integer
    s = CStr(n)
    ReDim t(Len(s))
    For i = 1 To Len(s)
        t(i) = Mid(s, i, 1)
    Next i
    Do
        dr = WorksheetFunction.Sum(t)
        s = CStr(dr)
        ReDim t(Len(s))
        For i = 1 To Len(s)
            t(i) = Mid(s, i, 1)
        Next i
        persistence = persistence + 1
    Loop Until Len(s) = 1
    Debug.Print n; "has additive persistence"; persistence; "and digital root of "; dr & ";"
End Sub
Public Sub main()
    digital_root 627615
    digital_root 39390
    digital_root 588225
    digital_root 393900588225#
End Sub
```
```txt
 627615 has additive persistence 2 and digital root of 9;
 39390 has additive persistence 2 and digital root of 6;
 588225 has additive persistence 2 and digital root of 3;
 393900588225 has additive persistence 2 and digital root of 9;
```


## VBScript


```vb
Function digital_root(n)
	ap = 0
	Do Until Len(n) = 1
		x = 0
		For i = 1 To Len(n)
			x = x + CInt(Mid(n,i,1))
		Next
		n = x
		ap = ap + 1
	Loop
	digital_root = "Additive Persistence = " & ap & vbCrLf &_
		"Digital Root = " & n & vbCrLf
End Function

WScript.StdOut.Write digital_root(WScript.Arguments(0))
```


```txt
F:\>cscript /nologo digital_root.vbs 627615
Additive Persistence = 2
Digital Root = 9

F:\>cscript /nologo digital_root.vbs 39390
Additive Persistence = 2
Digital Root = 6

F:\>cscript /nologo digital_root.vbs 588225
Additive Persistence = 2
Digital Root = 3

F:\>cscript /nologo digital_root.vbs 393900588225
Additive Persistence = 2
Digital Root = 9
```



## Visual Basic .NET

```vbnet
Module Module1

    Function DigitalRoot(num As Long) As Tuple(Of Integer, Integer)
        Dim additivepersistence = 0
        While num > 9
            num = num.ToString().ToCharArray().Sum(Function(x) Integer.Parse(x))
            additivepersistence = additivepersistence + 1
        End While
        Return Tuple.Create(additivepersistence, CType(num, Integer))
    End Function

    Sub Main()
        Dim nums = {627615, 39390, 588225, 393900588225}
        For Each num In nums
            Dim t = DigitalRoot(num)
            Console.WriteLine("{0} has additive persistence {1} and digital root {2}", num, t.Item1, t.Item2)
        Next
    End Sub

End Module
```

```txt
627615 has additive persistence 2 and digital root 9
39390 has additive persistence 2 and digital root 6
588225 has additive persistence 2 and digital root 3
393900588225 has additive persistence 2 and digital root 9
```



## Wortel


```wortel
@let {
  sumDigits ^(@sum @arr)
  drootl    &\@rangef [. sumDigits ^(\~>1 #@arr)]

  droot     ^(@last drootl)
  apers     ^(#-drootl)

  [
    !console.log "[number]: [digital root] [additive persistence] [intermediate sums]"
    ~@each [627615 39390 588225 393900588225]
      &n !console.log "{n}: {!droot n} {!apers n} {@str !drootl n}"
  ]
}
```

```txt
[number]: [digital root] [additive persistence] [intermediate sums]
627615: 9 2 [627615 27 9]
39390: 6 2 [39390 24 6]
588225: 3 2 [588225 30 3]
393900588225: 9 2 [393900588225 54 9]
```



## XPL0

Since integers are only 32 bits, floating point is used to get the extra
precision needed.


```XPL0
include c:\cxpl\codes;                  \intrinsic 'code' declarations

func DRoot(N, B, P);                    \Return digital root and persistance P
real N, B; int P;
int  S;
[P(0):= 0;
while N >= B do
        [S:= 0;
        repeat  S:= S + fix(Mod(N,B));  \sum last digit
                N:= N/B;                \remove last digit
                N:= N - Mod(N,1.);
        until   N < 0.1;                \(beware of rounding errors)
        P(0):= P(0)+1;                  \increment persistance
        N:= float(S);
        ];
return fix(N);
];

real Tbl;
int  I, Root, Pers;
[Tbl:= [627615., 39390., 588225., 393900588225.];
for I:= 0 to 4-1 do
        [Root:= DRoot(Tbl(I), 10., @Pers);
        IntOut(0, Pers);  ChOut(0, ^ );  IntOut(0, Root);  CrLf(0);
        ];
]
```

```txt

2 9
2 6
2 3
2 9

```



## zkl


```zkl
fcn sum(n,b){ n.split(b).sum(0) }
fcn droot(n,b=10,X=0) // -->(digital root, additive persistence)
   { if(n<b)return(n,X); return(self.fcn(sum(n,b),b,X+1)) }
```


```zkl
droot(627615)
droot(39390)
droot(588225)
droot(393900588225)
droot(7,2)
droot(0x7e0,16)
```

```txt

L(9,2)  //627615
L(6,2)  //39390
L(3,2)  //588225
L(9,2)  //393900588225
L(1,3)  //111 base 2: 111-->11-->10-->1
L(6,2)  //7e0 base 16: 0x7e0-->0x15-->0x6

```



## zonnon


```zonnon

module Main;
type
	longint = integer{64};

type {public,ref}
	Response = object (dr,p: longint)
	var {public,immutable}
		digitalRoot,persistence: longint;

	procedure {public} Writeln;
	begin
		writeln("digital root: ",digitalRoot:2," persistence: ",persistence:2)
	end Writeln;

	begin
		self.digitalRoot := dr;
		self.persistence := p;
	end Response;

	procedure DigitalRoot(n:longint):Response;
	var
		sum,p: longint;
	begin
		p := 0;
		loop
			inc(p);sum := 0;
			while (n > 0) do
				inc(sum,n mod 10);
				n := n div 10;
			end;
			if sum < 10 then return new Response(sum,p) else n := sum end
		end
	end DigitalRoot;

begin
	write(627615:22,":> ");DigitalRoot(627615).Writeln;
	write(39390:22,":> ");DigitalRoot(39390).Writeln;
	write(588225:22,":> ");DigitalRoot(588225).Writeln;
	write(max(integer{64}):22,":> ");DigitalRoot(max(integer{64})).Writeln;
end Main.

```

```txt

                627615 :> digital root:  9 persistence:  2
                 39390 :> digital root:  6 persistence:  2
                588225 :> digital root:  3 persistence:  2
   9223372036854775807 :> digital root:  7 persistence:  3


```


## ZX Spectrum Basic

```zxbasic
10 DATA 4,627615,39390,588225,9992
20 READ j: LET b=10
30 FOR i=1 TO j
40 READ n
50 PRINT "Digital root of ";n;" is"
60 GO SUB 1000
70 NEXT i
80 STOP
1000 REM Digital Root
1010 LET c=0
1020 IF n>=b THEN LET c=c+1: GO SUB 2000: GO TO 1020
1030 PRINT n;" persistance is ";c''
1040 RETURN
2000 REM Digit sum
2010 LET s=0
2020 IF n<>0 THEN LET q=INT (n/b): LET s=s+n-q*b: LET n=q: GO TO 2020
2030 LET n=s
2040 RETURN
```

