+++
title = "Josephus problem"
description = ""
date = 2019-09-18T18:48:12Z
aliases = []
[extra]
id = 12472
[taxonomies]
categories = []
tags = []
+++

{{task|Puzzles}}
[[wp:Josephus problem|Josephus problem]] is a math puzzle with a grim description: <math>n</math> prisoners are standing on a circle, sequentially numbered from <math>0</math> to <math>n-1</math>.

An executioner walks along the circle, starting from prisoner <math>0</math>,
removing every <math>k</math>-th prisoner and killing him.

As the process goes on, the circle becomes smaller and smaller, until only one prisoner remains, who is then freed. >

For example, if there are <math>n=5</math> prisoners and <math>k=2</math>, the order the prisoners are killed in (let's call it the "killing sequence") will be 1, 3, 0, and 4, and the survivor will be #2.


;Task:
Given any   <big><math>n, k > 0</math></big>,   find out which prisoner will be the final survivor.

In one such incident, there were 41 prisoners and every 3<sup>rd</sup> prisoner was being killed   (<big><math>k=3</math></big>).

Among them was a clever chap name Josephus who worked out the problem, stood at the surviving position, and lived on to tell the tale.

Which number was he?


;Extra:
The captors may be especially kind and let <math>m</math> survivors free,

and Josephus might just have   <big><math>m-1</math></big>   friends to save.

Provide a way to calculate which prisoner is at any given position on the killing sequence.


;Notes:
# You can always play the executioner and follow the procedure exactly as described, walking around the circle, counting (and cutting off) heads along the way.  This would yield the complete killing sequence and answer the above questions, with a complexity of probably <math>O(kn)</math>.  However, individually it takes no more than <math>O(m)</math> to find out which prisoner is the <math>m</math>-th to die.
# If it's more convenient, you can number prisoners from   <math>1</math> to <math>n</math>   instead.   If you choose to do so, please state it clearly.
# An alternative description has the people committing assisted suicide instead of being executed, and the last person simply walks away. These details are not relevant, at least not mathematically.





## 360 Assembly

{{trans|REXX}}
The program uses two ASSIST macros (XDECO,XPRNT) to keep the code as short as possible.

```360asm
*      Josephus problem               10/02/2017
JOSEPH CSECT
       USING  JOSEPH,R13              base register
       B      72(R15)                 skip savearea
       DC     17F'0'                  savearea
       STM    R14,R12,12(R13)         prolog
       ST     R13,4(R15)              " <-
       ST     R15,8(R13)              " ->
       LR     R13,R15                 " addressability
       LA     R7,1                    m=1
       DO WHILE=(C,R7,LE,=A(NPROB))   do m=1 to nprob
         LR     R1,R7                   m
         MH     R1,=H'6'                *6
         LH     R2,PROB-6(R1)
         ST     R2,N                    n=prob(m,1)
         LH     R2,PROB-4(R1)
         ST     R2,W                    w=prob(m,2)
         LH     R2,PROB-2(R1)
         ST     R2,S                    s=prob(m,3)
         MVC    PG,=CL80'josephus'      init buffer
         L      R1,N                    n
         XDECO  R1,DEC                  edit
         MVC    PG+8(4),DEC+8           output
         L      R1,W                    w
         XDECO  R1,DEC                  edit
         MVC    PG+12(4),DEC+8          output
         L      R1,S                    s
         XDECO  R1,DEC                  edit
         MVC    PG+16(4),DEC+8          output
         XPRNT  PG,L'PG                 print buffer
         MVI    DEAD,X'00'              dead(1)='0'B;
         MVC    DEAD+1(255),DEAD        dead(*)='0'B;
         L      R11,N                   nx=n
         L      R8,=F'-1'               p=-1
         DO UNTIL=(C,R11,EQ,S)          do until n=s
           SR     R9,R9                   found=0
           DO UNTIL=(C,R9,EQ,W)           do until found=w
             LA     R8,1(R8)                p=p+1
             IF C,R8,EQ,N THEN              if p=nn then
               SR     R8,R8                   p=0
             ENDIF  ,                       end if
             LA     R2,DEAD(R8)             @dead(p+1)
             IF CLI,0(R2),EQ,X'00' THEN     if not dead(p+1) then
               LA     R9,1(R9)                found=found+1
             ENDIF  ,                       end if
           ENDDO  ,                       end do
           LA     R2,DEAD(R8)             @dead(p+1)
           MVI    0(R2),X'01'             dead(p+1)='1'B
           BCTR   R11,0                   nx=nx-1
         ENDDO  ,                       end do
         MVC    PG,=CL80' '             clear buffer
         LA     R10,PG                  ipg=0
         L      R9,N                    nn
         BCTR   R9,0                    nn-1
         SR     R6,R6                   i=0
         DO WHILE=(CR,R6,LE,R9)         do i=0 to nn-1
           LA     R2,DEAD(R6)             @dead(i+1)
           IF CLI,0(R2),EQ,X'00' THEN     if not dead(i+1) then
             XDECO  R6,DEC                  edit i
             MVC    0(4,R10),DEC+8          output
             LA     R10,4(R10)              ipg=ipg+4
           ENDIF  ,                       end if
           LA     R6,1(R6)                i=i+1
         ENDDO  ,                       end do
         XPRNT  PG,L'PG                 print buffer
         LA     R7,1(R7)                m=m+1
       ENDDO  ,                       end do
       L      R13,4(0,R13)            epilog
       LM     R14,R12,12(R13)         " restore
       XR     R15,R15                 " rc=0
       BR     R14                     exit
PROB   DC     H'41',H'3',H'1'         round 1
       DC     H'41',H'3',H'3'         round 2
NPROB  EQU    (*-PROB)/6              number of rounds
N      DS     F                       n number of prisoners
W      DS     F                       w killing count
S      DS     F                       s number of prisoners to survive
PG     DS     CL80                    buffer
DEC    DS     CL12                    temp for xdeco
DEAD   DS     256X                    n max
       YREGS
       END    JOSEPH
```

{{out}}

```txt

josephus  41   3   1
  30
josephus  41   3   3
  15  30  34

```



## 6502 Assembly

This subroutine expects to be called with the value of <i>n</i> in the accumulator and the value of <i>k</i> in index register <tt>X</tt>. It returns with the index of the survivor in the accumulator, and also leaves an array beginning at address 1000 hex giving the order in which the prisoners died. For example, in the case where <i>n</i> = 5 and <i>k</i> = 2, the values stored in the array are 2, 0, 4, 1, 3. From this we see that prisoner 1 was the first to die, then prisoner 3, and so on. (Note that prisoner 2 in this instance is the survivor.)

```6502asm
JSEPHS: STA  $D0        ; n
        STX  $D1        ; k
        LDA  #$FF
        LDX  #$00
SETUP:  STA  $1000,X    ; populate array with hex FF
        INX
        CPX  $D0
        BEQ  KILL
        JMP  SETUP
KILL:   LDA  #$00       ; number killed so far
        STA  $D2
        LDX  #$00       ; position within array
        LDY  #$01       ; counting up to k
FIND:   INY
SCAN:   INX
        CPX  $D0
        BMI  TEST
        LDX  #$00       ; circle back around
TEST:   LDA  $1000,X
        CMP  #$FF
        BNE  SCAN       ; already been killed
        CPY  $D1
        BMI  FIND       ; if y < k keep going round
        LDA  $D2
        STA  $1000,X    ; mark as dead
        CLC
        ADC  #$01
        STA  $D2
        CMP  $D0        ; have we killed all but 1?
        BPL  RETURN
        LDY  #$00
        JMP  FIND
RETURN: TXA             ; a <- index of survivor
        RTS
```



## Ada

The procedure reads up to 4 parameters from the command line: the number N of prisoners, the step size K, the number M of survivors, and an indicator whether the executions shall be printed ("1") or only surviving prisoners (any other input). The defaults are 41, 3, 1, 1. The prison cells are numbered from 0 to N-1.

```Ada
with Ada.Command_Line, Ada.Text_IO;

procedure Josephus is

   function Arg(Idx, Default: Positive) return Positive is -- read Argument(Idx)
      (if Ada.Command_Line.Argument_Count >= Index
         then Positive'Value(Ada.Command_Line.Argument(Index)) else Default);

   Prisoners:  constant Positive := Arg(Idx => 1, Default => 41);
   Steps:      constant Positive := Arg(Idx => 2, Default =>  3);
   Survivors:  constant Positive := Arg(Idx => 3, Default =>  1);
   Print:               Boolean := (Arg(Idx => 4, Default =>  1) = 1);

   subtype Index_Type is Natural range 0 .. Prisoners-1;
   Next: array(Index_Type) of Index_Type;
   X: Index_Type := (Steps-2) mod Prisoners;

begin
   Ada.Text_IO.Put_Line
     ("N =" & Positive'Image(Prisoners) & ",  K =" & Positive'Image(Steps) &
        (if Survivors > 1 then ",  #survivors =" & Positive'Image(Survivors)
        else ""));
   for Idx in Next'Range loop -- initialize Next
      Next(Idx) := (Idx+1) mod Prisoners;
   end loop;
   if Print then
      Ada.Text_IO.Put("Executed: ");
   end if;
   for Execution in reverse 1 .. Prisoners loop
      if Execution = Survivors then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("Surviving: ");
         Print := True;
      end if;
      if Print then
         Ada.Text_IO.Put(Positive'Image(Next(X)));
      end if;
      Next(X) := Next(Next(X)); -- "delete" a prisoner
      for Prisoner in 1 .. Steps-1 loop
         X := Next(X);
      end loop;
   end loop;
end Josephus;
```

{{out}}

```txt
$ ./josephus
N = 41,  K = 3
Executed:  2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15
Surviving:  30

$ ./josephus 23482 3343 3 0
N = 23482,  K = 3343,  #survivors = 3

Surviving:  13317 1087 1335
```



## ALGOL 68

Translated from the C

```algol68
BEGIN
   PROC josephus = (INT n, k, m) INT :
   CO Return m-th on the reversed kill list; m=0 is final survivor. CO
   BEGIN
      INT lm := m;			CO Local copy of m CO
      FOR a FROM m+1 WHILE a <= n DO lm := (lm+k) %* a OD;
      lm
   END;
   INT n = 41, k=3;
   printf (($"n = ", g(0), ", k = ", g(0), ", final survivor: ", g(0)l$,
	    n, k, josephus (n, k, 0)))
END
```

{{out}}

```txt
n = 41, k = 3, final survivor: 30
```


## ANSI Standard BASIC

Translated from ALGOL 68

```ANSI Standard BASIC
100 FUNCTION josephus (n, k, m)
110 ! Return m-th on the reversed kill list; m=0 is final survivor.
120    LET lm = m  ! Local copy OF m
130    FOR a = m+1  TO n
140       LET lm = MOD(lm+k, a)
150    NEXT a
160    LET josephus = lm
170 END FUNCTION
180 LET n = 41
190 LET k=3
200 PRINT "n =";n, "k =";k,"final survivor =";josephus(n, k, 0)
210 END

```



## AutoHotkey


```AHK
; Since AutoHotkey is 1-based, we're numbering prisoners 1-41.
nPrisoners := 41
kth        := 3

; Build a list, purposefully ending with a separator
Loop % nPrisoners
	list .= A_Index . "|"

; iterate and remove from list
i := 1
Loop
{
	; Step by 2; the third step was done by removing the previous prisoner
	i += kth - 1
	if (i > nPrisoners)
		i := Mod(i, nPrisoners)
	; Remove from list
	end := InStr(list, "|", 0, 1, i)
	bgn := InStr(list, "|", 0, 1, i-1)
	list := SubStr(list, 1, bgn) . SubStr(list, end+1)
	nPrisoners--
}
Until (nPrisoners = 1)
MsgBox % RegExReplace(list, "\|") ; remove the final separator
```

{{out}}

```txt
31
```

Note that since this is one-based, the answer is correct, though it differs with many other examples.

### Using Objects


```AHK
nPrisoners := 41
kth        := 3
list       := []

; Build a list of 41 items
Loop % nPrisoners
	list.insert(A_Index)

; iterate and remove from list
i := 1
Loop
{
	; Step by 3
	i += kth - 1
	if (i > list.MaxIndex())
		i := Mod(i, list.MaxIndex())
	list.remove(i)
}
Until (list.MaxIndex() = 1)
MsgBox % list.1 ; there is only 1 element left
```



## AWK


```AWK

# syntax: GAWK -f JOSEPHUS_PROBLEM.AWK
# converted from PL/I
BEGIN {
    main(5,2,1)
    main(41,3,1)
    main(41,3,3)
    exit(0)
}
function main(n,k,s,  dead,errors,found,i,killed,nn,p,survived) {
# n - number of prisoners
# k - kill every k'th prisoner
# s - number of survivors
    printf("\nn=%d k=%d s=%d\n",n,k,s) # show arguments
    if (s > n) { print("s>n"); errors++ }
    if (k <= 0) { print("k<=0"); errors++ }
    if (errors > 0) { return(0) }
    nn = n                             # wrap around boundary
    p = -1                             # start here
    while (n != s) {                   # until survivor count is met
      found = 0                        # start looking
      while (found != k) {             # until we have the k-th prisoner
        if (++p == nn) { p = 0 }       # wrap around
        if (dead[p] != 1) { found++ }  # if prisoner is alive increment found
      }
      dead[p] = 1                      # kill the unlucky one
      killed = killed p " "            # build killed list
      n--                              # reduce size of circle
    }
    for (i=0; i<=nn-1; i++) {
      if (dead[i] != 1) {
        survived = survived i " "      # build survivor list
      }
    }
    printf("killed: %s\n",killed)
    printf("survived: %s\n",survived)
    return(1)
}

```

{{out}}

```txt

n=5 k=2 s=1
killed: 1 3 0 4
survived: 2

n=41 k=3 s=1
killed: 2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15
survived: 30

n=41 k=3 s=3
killed: 2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3
survived: 15 30 34

```



## BASIC

Unstructured implementation: see solutions listed under specific BASIC dialects for structured versions.

```basic
10 N=41
20 K=3
30 M=0
40 FOR I=M+1 TO N
50 M=INT(I*((M+K)/I-INT((M+K)/I))+0.5)
60 NEXT I
70 PRINT "Survivor is number";M
```

{{out}}

```txt
Survivor is number 30
```


=
## Applesoft BASIC
=
Translated from the BASIC implementation above and the ANSI Standard BASIC.

```Applesoft BASIC

 10  DEF  FN MOD(X) = X - INT (X / A) * A
 20  LM = 0: INPUT "GIVE N AND K (N,K): ";N,K
 30  IF N < 1 or K < 1 THEN GOTO 20
 40  FOR A = 1 TO N: LM =  FN MOD(LM + K): NEXT A
 50  PRINT "N = ";N;", K = ";K;", SURVIVOR: ";LM

```

{{out}}

```txt
GIVE N AND K (N,K): 41,3
N = 41, K = 3, SURVIVOR: 30
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Josephus.bas"
110 INPUT PROMPT "Number of prisoners: ":NP
120 INPUT PROMPT "Execution step: ":EX
130 INPUT PROMPT "How many survivors:  ":SU
140 PRINT "Survivors:";
150 FOR S=0 TO SU-1
160   PRINT JOSEPHUS(NP,EX,S);
170 NEXT
180 DEF JOSEPHUS(N,K,M)
190   FOR I=M+1 TO N
200     LET M=MOD((M+K),I)
210   NEXT
220   LET JOSEPHUS=M
230 END DEF
```



## Batch File

Uses C's <code>jos()</code> function.
{{trans|C}}

```dos
@echo off
setlocal enabledelayedexpansion

set "prison=41"		%== Number of prisoners ==%
set "step=3"		%== The step... ==%
set "survive=1"		%== Number of survivors ==%
call :josephus

set "prison=41"
set "step=3"
set "survive=3"
call :josephus
pause
exit /b 0

	%== The Procedure ==%
:josephus
set "surv_list="
for /l %%S in (!survive!,-1,1) do (

	set /a "m = %%S - 1"
	for /l %%X in (%%S,1,!prison!) do (
		set /a "m = (m + step) %% %%X"
	)
	if defined surv_list (
		set "surv_list=!surv_list! !m!"
	) else (
		set "surv_list=!m!"
	)
)
echo !surv_list!
goto :EOF
```

{{Out}}

```txt
30
34 15 30
Press any key to continue . . .
```



## BBC BASIC


```bbcbasic>REM
josephus
PRINT "Survivor is number "; FNjosephus(41, 3, 0)
END
:
DEF FNjosephus(n%, k%, m%)
LOCAL i%
FOR i% = m% + 1 TO n%
  m% = (m% + k%) MOD i%
NEXT
= m%
```

{{out}}

```txt
Survivor is number 30
```



## Befunge

The number of prisoners and step size are read from stdin.


```befunge>
0" :srenosirP">:#,_&>>00p>>v
v0p01<&_,#!>#:<"Step size: "<
>1+:20p00g`!#v_0"  :rovivru"v
^g02%g02+g01<<@.$_,#!>#:<"S"<
```


{{out}}

```txt
Prisoners: 41
Step size: 3
Survivor:  30
```



## C


```c
#include <stdio.h>

// m-th on the reversed kill list; m = 0 is final survivor
int jos(int n, int k, int m) {
	int a;
	for (a = m + 1; a <= n; a++)
		m = (m + k) % a;
	return m;
}

typedef unsigned long long xint;

// same as jos(), useful if n is large and k is not
xint jos_large(xint n, xint k, xint m) {
	if (k <= 1) return n - m - 1;

	xint a = m;
	while (a < n) {
		xint q = (a - m + k - 2) / (k - 1);

		if (a + q > n)	q = n - a;
		else if (!q)	q = 1;

		m = (m + q * k) % (a += q);
	}

	return m;
}

int main(void) {
	xint n, k, i;

	n = 41;
	k = 3;
	printf("n = %llu, k = %llu, final survivor: %d\n", n, k, jos(n, k, 0));

	n = 9876543210987654321ULL;
	k = 12031;
	printf("n = %llu, k = %llu, three survivors:", n, k);

	for (i = 3; i--; )
		printf(" %llu", jos_large(n, k, i));
	putchar('\n');

	return 0;
}
```

{{out}}

```txt

n = 41, k = 3, final survivor: 30
n = 9876543210987654321, k = 12031, three survivors: 6892710366467541051 1946357796579138992 3554846299321782413

```



## C#


```csharp

namespace Josephus
{
    using System;
    using System.Collections;
    using System.Collections.Generic;

    public class Program
    {
        public static int[] JosephusProblem(int n, int m)
        {
            var circle = new List<int>();
            var order = new int[n];

            for (var i = 0; i < n; ++i)
            {
                circle.Add(i);
            }

            var l = 0;
            var j = 0;
            var k = 0;

            while (circle.Count != 0)
            {
                j++;
                if (j == m)
                {
                    order[k] = circle[l];
                    circle.RemoveAt(l);

                    k++;
                    l--;
                    j = 0;
                }

                if (k == n - 1)
                {
                    order[k] = circle[0];
                    circle.RemoveAt(0);
                }

                if (l == circle.Count - 1)
                {
                    l = 0;
                }
                else
                {
                    l++;
                }
            }

            return order;
        }

        static void Main(string[] args)
        {
            try
            {
                var n = 7;
                var m = 2;

                var result = JosephusProblem(n, m);

               for (var i = 0; i < result.Length; i++)
               {
                   Console.WriteLine(result[i]);//1 3 5 0 4 2 6
               }
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
            }
            finally
            {
                Console.ReadLine();
            }
        }

    }
}

```



## C++


```cpp

#include <iostream>
#include <vector>

//--------------------------------------------------------------------------------------------------
using namespace std;
typedef unsigned long long bigint;

//--------------------------------------------------------------------------------------------------
class josephus
{
public:
    bigint findSurvivors( bigint n, bigint k, bigint s = 0 )
    {
	bigint i = s + 1;
	for( bigint x = i; x <= n; x++, i++ )
	    s = ( s + k ) % i;

	return s;
    }

    void getExecutionList( bigint n, bigint k, bigint s = 1 )
    {
	cout << endl << endl << "Execution list: " << endl;

	prisoners.clear();
	for( bigint x = 0; x < n; x++ )
	    prisoners.push_back( x );

	bigint index = 0;
	while( prisoners.size() > s )
	{
	    index += k - 1;
	    if( index >= prisoners.size() ) index %= prisoners.size();
	    cout << prisoners[static_cast<unsigned int>( index )] << ", ";

	    vector<bigint>::iterator it = prisoners.begin() + static_cast<unsigned int>( index );
	    prisoners.erase( it );
	}
    }

private:
    vector<bigint> prisoners;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    josephus jo;
    bigint n, k, s;
    while( true )
    {
	system( "cls" );
	cout << "Number of prisoners( 0 to QUIT ): "; cin >> n;
	if( !n ) return 0;
	cout << "Execution step: "; cin >> k;
	cout << "How many survivors: "; cin >> s;

	cout << endl << "Survivor";
	if( s == 1 )
	{
	    cout << ": " << jo.findSurvivors( n, k );
	    jo.getExecutionList( n, k );
	}
	else
	{
	    cout << "s: ";
	    for( bigint x = 0; x < s; x++ )
		cout << jo.findSurvivors( n, k, x ) << ", ";

	    jo.getExecutionList( n, k, s );
	}

	cout << endl << endl;
	system( "pause" );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------

```

{{out}}

```txt

Number of prisoners( 0 to QUIT ): 41
Execution step: 3
How many survivors: 1

Survivor: 30

Execution list:
2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36
, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3, 34, 15,


Number of prisoners( 0 to QUIT ): 41
Execution step: 3
How many survivors: 3

Survivors: 30, 15, 34,

Execution list:
2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36
, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3,


Number of prisoners( 0 to QUIT ): 71
Execution step: 47
How many survivors: 11

Survivors: 29, 58, 41, 14, 39, 28, 35, 45, 64, 49, 27,

Execution list:
46, 22, 70, 48, 26, 5, 56, 36, 17, 0, 54, 38, 23, 9, 66, 55, 43, 33, 25, 16, 11,
6, 2, 69, 68, 1, 4, 10, 15, 24, 32, 42, 53, 65, 20, 40, 60, 19, 47, 8, 44, 13,
52, 31, 12, 62, 57, 50, 51, 61, 7, 30, 59, 34, 18, 3, 21, 37, 67, 63,

```



## Clojure


```clojure
(defn rotate [n s] (lazy-cat (drop n s) (take n s)))

(defn josephus [n k]
   (letfn [(survivor [[ h & r :as l] k]
             (cond (empty? r) h
                   :else      (survivor (rest (rotate (dec k) l)) k)))]
     (survivor (range n) k)))

(let [n 41 k 3]
   (println (str "Given " n " prisoners in a circle numbered 1.." n
                 ", an executioner moving around the"))
   (println (str "circle " k " at a time will leave prisoner number "
                 (inc (josephus n k)) " as the last survivor.")))
```


{{Output}}

```txt
Given 41 prisoners in a circle numbered 1..41, an executioner moving around the
circle 3 at a time will leave prisoner number 31 as the last survivor.
```



## Common Lisp

Using a loop:

```lisp
(defun kill (n k &aux (m 0))
  (loop for a from (1+ m) upto n do
        (setf m (mod (+ m k) a)))
  m)
```

Using a circular list.

```lisp
(defun make-circular-list (n)
  (let* ((list (loop for i below n
                     collect i))
         (last (last list)))
    (setf (cdr last) list)
    list))

(defun kill (n d)
  (let ((list (make-circular-list n)))
    (flet ((one-element-clist-p (list)
             (eq list (cdr list)))
           (move-forward ()
             (loop repeat (1- d)
                   until (eq list (cdr list))
                   do (setf list (cdr list))))
           (kill-item ()
             (setf (car list) (cadr list)
                   (cdr list) (cddr list))))
      (loop until (one-element-clist-p list) do
            (move-forward)
            (kill-item))
      (first list))))
```

{{out|Example}}
 CL-USER > (kill 41 3)
 30

## Crystal

{{trans|Ruby}}

```ruby
n = ARGV.fetch(0, 41).to_i  # n default is 41 or ARGV[0]
k = ARGV.fetch(1,  3).to_i  # k default is 3 or ARGV[1]

prisoners = (0...n).to_a
while prisoners.size > 1; prisoners.rotate!(k-1).shift end
puts "From #{n} prisoners, eliminating each prisoner #{k} leaves prisoner #{prisoners.first}."

```

{{out}}

```txt

$ crystal josephus.cr
From 41 prisoners, eliminating each prisoner 3 leaves prisoner 30.

$ crystal josephus.cr 123
From 123 prisoners, eliminating each prisoner 3 leaves prisoner 54.

$ crystal josephus.cr 123 47
From 123 prisoners, eliminating each prisoner 47 leaves prisoner 101.

```



## D

{{trans|Python}}

```d
import std.stdio, std.algorithm, std.array, std.string, std.range;

T pop(T)(ref T[] items, in size_t i) pure /*nothrow*/ @safe /*@nogc*/ {
    auto aux = items[i];
    items = items.remove(i);
    return aux;
}

string josephus(in int n, in int k) pure /*nothrow*/ @safe {
    auto p = n.iota.array;
    int i;
    immutable(int)[] seq;
    while (!p.empty) {
        i = (i + k - 1) % p.length;
        seq ~= p.pop(i);
    }

    return format("Prisoner killing order:\n%(%(%d %)\n%)." ~
                  "\nSurvivor: %d",
                  seq[0 .. $ - 1].chunks(20), seq[$ - 1]);
}

void main() /*@safe*/ {
    josephus(5, 2).writeln;
    writeln;
    josephus(41, 3).writeln;
}
```

{{out}}

```txt
Prisoner killing order:
1 3 0 4.
Survivor: 2

Prisoner killing order:
2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27
31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15.
Survivor: 30
```



{{trans|Javascript}}

```d
import std.stdio, std.algorithm, std.range;

int[][] Josephus(in int n, int k, int s=1) {
    int[] ks, ps = n.iota.array;
    for (int i=--k; ps.length>s; i=(i+k)%ps.length) {
        ks ~= ps[i];
        ps = remove(ps, i);
    }
    writefln("Josephus(%d,%d,%d) -> %(%d %) / %(%d %)%s", n, k, s, ps, ks[0..min($,45)], ks.length<45 ? "" : " ..." );
    return [ps, ks];
}

void main() {
    Josephus(5, 2);
    Josephus(41, 3);
    Josephus(23482, 3343, 3);
}}
```

{{out}}

```txt
Josephus(5,1,1) -> 2 / 1 3 0 4
Josephus(41,2,1) -> 30 / 2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15
Josephus(23482,3342,3) -> 1087 1335 13317 / 3342 6685 10028 13371 16714 20057 23400 3261 6605 9949 13293 16637 19981 23325 3187 6532 9877 13222 16567 19912 23257 3120 6466 9812 13158 16504 19850 23196 3060 6407 9754 13101 16448 19795 23142 3007 6355 9703 13051 16399 19747 23095 2961 6310 9659 ...
```



## EchoLisp

We use a circular list and apply the 'process'. Successive rests are marked 🔫 (killed) or 😥 (remaining). NB: the '''(mark)''' function marks lists and sub-lists, not items in lists. The printed mark appears before the first item in the list.

```lisp

;; input
(define N 41)
(define K 3)
(define prisoners (apply circular-list (iota N)))
(define last-one prisoners) ; current position

;; kill returns current position = last killed
(define (kill lst skip)
(cond
    ((eq? (mark? lst) '🔫 )(kill (cdr lst) skip)) ;; dead ? goto next
    ((zero? skip) (mark lst '🔫)) ;; all skipped ? kill
    (else (mark lst '😥 )  ;; relieved face
           (kill (cdr lst ) (1- skip))))) ;; skip 1 and goto next

```

{{out}}

```lisp

;; kill N-1
    (for ((i (1- N) )) (set! last-one (kill last-one  (1- K))))
;; look at prisoners
prisoners
→ ( 🔄 🔫 0 🔫 1 🔫 2 🔫 3 🔫 4 🔫 5 🔫 6 🔫 7 🔫 8 🔫 9 🔫 10 🔫 11 🔫 12 🔫 13 🔫 14 🔫 15 🔫 16
 🔫 17 🔫 18 🔫 19 🔫 20 🔫 21 🔫 22 🔫 23 🔫 24 🔫 25 🔫 26 🔫 27 🔫 28 🔫 29 😥 30 🔫 31 🔫 32
 🔫 33 🔫 34 🔫 35 🔫 36 🔫 37 🔫 38 🔫 39 🔫 40 🔫 0 🔫 1  … ∞)

;; #30 seems happy
;; kill last
(set! last-one (kill last-one (1- K)))
last-one
  → ( 🔫 30 🔫 31 🔫 32 …🔃 ) ;; #30 was the last

;; extra : we want more survivors
(define SURVIVORS 3)
(for ((i (- N SURVIVORS) )) (set! last-one (kill last-one  (1- K))))

prisoners
→  ( 🔄 🔫 0 🔫 1 🔫 2 🔫 3 🔫 4 🔫 5 🔫 6 🔫 7 🔫 8 🔫 9 🔫 10 🔫 11 🔫 12 🔫 13 🔫 14 😥 15 🔫 16
   🔫 17 🔫 18 🔫 19 🔫 20 🔫 21 🔫 22 🔫 23 🔫 24 🔫 25 🔫 26 🔫 27 🔫 28 🔫 29 😥 30 🔫 31 🔫 32
   🔫 33 😥 34 🔫 35 🔫 36 🔫 37 🔫 38 🔫 39 🔫 40 🔫 0 🔫 1  🔫 0 … ∞)


```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
		do
			io.put_string ("Survivor is prisoner: " + execute (12, 4).out)
		end

	execute (n, k: INTEGER): INTEGER
			-- Survivor of 'n' prisoners, when every 'k'th is executed.
		require
			n_positive: n > 0
			k_positive: k > 0
			n_larger: n > k
		local
			killidx: INTEGER
			prisoners: LINKED_LIST [INTEGER]
		do
			create prisoners.make
			across
				0 |..| (n - 1) as c
			loop
				prisoners.extend (c.item)
			end
			io.put_string ("Prisoners are executed in the order:%N")
			killidx := 1
			from
			until
				prisoners.count <= 1
			loop
				killidx := killidx + k - 1
				from
				until
					killidx <= prisoners.count
				loop
					killidx := killidx - prisoners.count
				end
				io.put_string (prisoners.at (killidx).out + "%N")
				prisoners.go_i_th (killidx)
				prisoners.remove
			end
			Result := prisoners.at (1)
		ensure
			Result_in_range: Result >= 0 and Result < n
		end

end

```


{{out}}

```txt

Prisoners are executed in the order:
3
7
11
4
9
2
10
6
5
8
1
Survivor is prisoner: 0

```



## Elixir


```Elixir

defmodule Josephus do
  def find(n,k) do
    find(Enum.to_list(0..n-1),0..k-2,k..n)
  end

  def find([_|[r|_]],_,_..d) when d < 3 do
    IO.inspect r
  end

  def find(arr,a..c,b..d) when length(arr) >= 3 do
    find(Enum.slice(arr,b..d) ++ Enum.slice(arr,a..c),a..c,b..d-1)
  end
end

Josephus.find(41,3)

```


{{out}}

```txt
30
```



## Emacs Lisp


```Lisp

(defun jo(n k)
  (if (= 1 n) 1 (1+ (% (+ (1- k)
			  (jo (1- n) k)) n ) ) ))
(princ-list (jo 50 2) "\n" (jo 60 3))
```


{{out}}

```txt

37
41
```



## Erlang


```Erlang

-module( josephus_problem ).

-export( [general_solution/3, task/0] ).

general_solution( Prisoners, Kill, Survive ) -> general_solution( Prisoners, Kill, Survive, erlang:length(Prisoners), [] ).

task() -> general_solution( lists:seq(0, 40), 3, 1 ).



general_solution( Prisoners, _Kill, Survive, Survive, Kills ) ->
        {Prisoners, lists:reverse(Kills)};
general_solution( Prisoners, Kill, Survive, Prisoners_length, Kills ) ->
        {Skipped, [Killed | Rest]} = kill( Kill, Prisoners, Prisoners_length ),
        general_solution( Rest ++ Skipped, Kill, Survive, Prisoners_length - 1, [Killed | Kills] ).

kill( Kill, Prisoners, Prisoners_length ) when Kill < Prisoners_length ->
    lists:split( Kill - 1, Prisoners );
kill( Kill, Prisoners, Prisoners_length ) ->
    kill_few( Kill rem Prisoners_length, Prisoners ).

kill_few( 0, Prisoners ) ->
    [Last | Rest] = lists:reverse( Prisoners ),
    {lists:reverse( Rest ), [Last]};
kill_few( Kill, Prisoners ) ->
    lists:split( Kill - 1, Prisoners ).

```


{{out}}

```txt

11> josephus_problem:task().
{[30],
 [2,5,8,11,14,17,20,23,26,29,32,35,38,0,4,9,13,18,22,27,31,
  36,40,6,12,19,25|...]}

```

The general solution can handle other items than numbers.

```txt

12> josephus_problem:general_solution( [joe, jack, william, averell, ratata], 2, 1 ).
{[william],[jack,averell,joe,ratata]}

```



## ERRE


```ERRE

PROGRAM JOSEPHUS

!
! for rosettacode.org
!

!$INTEGER

DIM DEAD[100]

PROCEDURE MAIN(N,K,S->ERRORS)
! n - number of prisoners
! k - kill every k'th prisoner
! s - number of survivors
    LOCAL KILLED$,SURVIVED$,FOUND,P,NN,I
    ERRORS=0
    FOR I=0 TO 100 DO
        DEAD[I]=0
    END FOR   ! prepare array
    PRINT("N=";N,"K=";K,"S=";S)        ! show arguments
    IF S>N THEN PRINT("S>N";) ERRORS+=1 END IF
    IF K<=0 THEN PRINT("K<=0";) ERRORS+=1 END IF
    IF ERRORS>0 THEN EXIT PROCEDURE END IF
    NN=N                               ! wrap around boundary
    P=-1                               ! start here
    WHILE N<>S DO                      ! until survivor count is met
      FOUND=0                          ! start looking
      WHILE FOUND<>K DO                ! until we have the k-th prisoner
        P+=1
        IF P=NN THEN P=0 END IF        ! wrap around
        IF DEAD[P]<>1 THEN
            FOUND+=1
        END IF                         ! if prisoner is alive increment found
      END WHILE
      DEAD[P]=1                        ! kill the unlucky one
      KILLED$=KILLED$+STR$(P)          ! build killed list
      N-=1                             ! reduce size of circle
    END WHILE
    FOR I=0 TO NN-1 DO
      IF DEAD[I]<>1 THEN
        SURVIVED$=SURVIVED$+STR$(I)    ! build survivor list
      END IF
    END FOR
    PRINT("Killed:";KILLED$)
    PRINT("Survived:";SURVIVED$)
END PROCEDURE

BEGIN
    ERRORS=0
    MAIN(5,2,1->ERRORS)
    MAIN(41,3,1->ERRORS)
    MAIN(41,3,3->ERRORS)
END PROGRAM

```

Note: Adapted from AWK version! Output is the same.


## Factor


```factor
USING: kernel locals math math.ranges sequences ;
IN: josephus

:: josephus ( k n -- m )
    n [1,b] 0 [ [ k + ] dip mod ] reduce ;
```


```txt
IN: scratchpad 3 41 josephus .
30

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Josephus_problem this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth>: josephus  0 1 begin dup 41 <= while  swap 3 + over mod swap  1+ repeat drop ;</lang


```txt
josephus .
30

```



## Fortran

Naive approach: prisonners are put in a "linked buffer" (implemented as an array giving number of "next living prisonner"). Then we iterate, killing one after each loop, until there is only one left.

```fortran
program josephus
   implicit none
   integer :: n, i, k, p
   integer, allocatable :: next(:)
   read *, n, k
   allocate(next(0:n - 1))
   do i = 0, n - 2
      next(i) = i + 1
   end do
   next(n - 1) = 0
   p = 0
   do while(next(p) /= p)
      do i = 1, k - 2
         p = next(p)
      end do
      print *, "Kill", next(p)
      next(p) = next(next(p))
      p = next(p)
   end do
   print *, "Alive", p
   deallocate(next)
end program
```




## FreeBASIC


```freebasic

Function Josephus (n As Integer, k As Integer, m As Integer) As Integer
    Dim As Integer lm = m
    For i As Integer = m + 1  To n
        lm = (lm + k) Mod i
    Next i
    Josephus = lm
End Function

Dim As Integer n = 41 'prisioneros
Dim As Integer k = 3  'orden de ejecución

Print "n ="; n, "k ="; k, "superviviente = "; Josephus(n, k, 0)

```

{{out}}

```txt

n = 41        k = 3         superviviente =  30

```




## friendly interactive shell


```fishshell
function execute
    # If the list is empty, don't do anything.
    test (count $argv) -ge 2; or return
    # If the list has only one element, return it
    if test (count $argv) -eq 2
        echo $argv[2]
        return
    end
    # Rotate prisoners
    for i in (seq 2 $argv[1])
        set argv $argv[1 3..-1 2]
    end
    # Mention killed prisoner
    echo $argv[2]
    # Kill rest recursively
    execute $argv[1 3..-1]
end

echo Prisoner (execute 3 (seq 0 40))[-1] survived.
```

{{out}}

```txt
Prisoner 30 survived.
```

It's also possible to calculate more than one survivor.

```fishshell
echo Prisoners (execute 3 (seq 0 40))[-3..-1] survived.
```

{{out}}

```txt
Prisoners 34 15 30 survived.
```

Prisoners don't have to be numbers.

```fishshell
echo Prisoner (execute 2 Joe Jack William Averell Rantanplan)[-1] survived.
```

{{out}}

```txt
Prisoner William survived.
```


## Groovy


```groovy
int[] Josephus (int size, int kill, int survivors) {
    // init user pool
    def users = new int[size];

    // give initial values such that [0] = 1 (first person) [1] = 2 (second person) etc
    users.eachWithIndex() {obj, i -> users[i] = i + 1};

    // keep track of which person we are on (ranging from 1 to kill)
    def person = 1;

    // keep going until we have the desired number of survivors
    while (users.size() > survivors)
    {
        // for each person, if they are the kill'th person, set them to -1 to show eliminated
        users.eachWithIndex() {obj, i ->
            if (person++ % kill == 0) {
                users[i] = -1;
            }

            // if person overflowed kill then reset back to 1
            if (person > kill) {person = 1;}
        }

        // clear out all eliminated persons
        users = users.findAll{w -> w >= 0};
    }

    // resulting set is the safe positions
    return users;
}

// Run some test cases

println "Final survivor for n = 10201 and k = 17: " + Josephus(10201,17,1)[0];

println "4 safe spots for n = 10201 and k = 17: " + Josephus(10201,17,4);

```

{{out}}

```txt

Final survivor for n = 10201 and k = 17: 7450
4 safe spots for n = 10201 and k = 17: [3413, 7244, 7450, 7605]

```



## Go


```go
package main

import "fmt"

// basic task function
func finalSurvivor(n, k int) int {
    // argument validation omitted
    circle := make([]int, n)
    for i := range circle {
        circle[i] = i
    }
    k--
    exPos := 0
    for len(circle) > 1 {
        exPos = (exPos + k) % len(circle)
        circle = append(circle[:exPos], circle[exPos+1:]...)
    }
    return circle[0]
}

// extra
func position(n, k, pos int) int {
    // argument validation omitted
    circle := make([]int, n)
    for i := range circle {
        circle[i] = i
    }
    k--
    exPos := 0
    for len(circle) > 1 {
        exPos = (exPos + k) % len(circle)
        if pos == 0 {
            return circle[exPos]
        }
        pos--
        circle = append(circle[:exPos], circle[exPos+1:]...)
    }
    return circle[0]
}

func main() {
    // show basic task function on given test case
    fmt.Println(finalSurvivor(41, 3))
    // show extra function on all positions of given test case
    fmt.Println("Position  Prisoner")
    for i := 0; i < 41; i++ {
        fmt.Printf("%5d%10d\n", i, position(41, 3, i))
    }
}
```

{{out}}

```txt

30
Position  Prisoner
    0         2
    1         5
    2         8
    3        11
    4        14
    5        17
    6        20
    7        23
    8        26
    9        29
   10        32
   11        35
   12        38
   13         0
   14         4
   15         9
   16        13
   17        18
   18        22
   19        27
   20        31
   21        36
   22        40
   23         6
   24        12
   25        19
   26        25
   27        33
   28        39
   29         7
   30        16
   31        28
   32        37
   33        10
   34        24
   35         1
   36        21
   37         3
   38        34
   39        15
   40        30

```



## Haskell

Shows only the surviving prisoners. Change "print $ snd" to just "print" to show the killed prisoners, too.
The arguments to the "main" function are: n = number of prisoners, k = kill every kth prisoner,
m = show at most m survivors

```Haskell
import Data.List ((\\))
import System.Environment (getArgs)

prisoners :: Int -> [Int]
prisoners n = [0 .. n - 1]

counter :: Int -> [Int]
counter k = cycle [k, k-1 .. 1]

killList :: [Int] -> [Int] -> ([Int], [Int], [Int])
killList xs cs = (killed, survivors, newCs)
    where
        (killed, newCs) = kill xs cs []
        survivors = xs \\ killed
        kill [] cs rs = (rs, cs)
        kill (x:xs) (c:cs) rs
            | c == 1 =
                let ts = rs ++ [x]
                in  kill xs cs ts
            | otherwise =
                kill xs cs rs

killRecursive :: [Int] -> [Int] -> Int -> ([Int], [Int])
killRecursive xs cs m = killR ([], xs, cs)
    where
        killR (killed, remaining, counter)
            | length remaining <= m = (killed, remaining)
            | otherwise =
                let (newKilled, newRemaining, newCounter) =
                        killList remaining counter
                    allKilled = killed ++ newKilled
                in  killR (allKilled, newRemaining, newCounter)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [n, k, m] -> print $ snd $ killRecursive (prisoners (read n))
                        (counter (read k)) (read m)
        _         -> print $ snd $ killRecursive (prisoners 41) (counter 3) 1

```


Using modulo and list split, indices are 1-based. This is much faster than cycled list for larger numbers:

```Haskell
jseq :: Int -> Int -> [Int]
jseq n k = f n [1 .. n]
  where
    f 0 _ = []
    f m s = x : f (m - 1) (right ++ left)
      where
        (left, x:right) = splitAt (mod (k - 1) m) s

-- the final survivor is ((k + ...((k + ((k + 0)`mod` 1)) `mod` 2) ... ) `mod` n)
jos :: Int -> Int -> Int
jos n k = 1 + foldl (mod . (k +)) 0 [2 .. n]

main :: IO ()
main = do
  print $ jseq 41 3
  print $ jos 10000 100
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.


```unicon
procedure main(A)
   m := integer(A[1]) | 41
   c := integer(A[2]) | 3
   write("With ",m," men, counting to ",c," last position is: ", j(m,c))
end

procedure j(m,c)
   return if m==1 then 0 else (j(m-1,c)+c)%m
end
```


{{out}}

```txt

->josephus
With 41 men, counting to 3 last position is: 30
->

```


Extra 'credit' version:

This is done awkwardly, but I've had this laying around since the late 1980's...


```unicon
procedure main(args)
   n := total := integer(args[1]) | 41		# Number of people
   k := count := integer(args[2]) | 3		# Count
   s := integer(args[3])-1 | 0                  # Number to save
   write("With ",n," people, counting by ",k,", the ",s+1," safe places are:")
   every write("\t",j(n,k,(n-s) to n))
end

procedure j(n,k,s)
   a := k*(n-s) + 1
   q := k/(k-1.0)
   nk := n*k
   olda := a
   while a <= nk do {
      olda := a
      a := ceil(a,q)
      }
   t := nk - olda
   return t
end

procedure ceil(a,q)
  n := a*q
  if n = integer(n) then return integer(n)
  n ?:= integer(tab(upto('.'))) + 1
  return n
end
```


Sample run:


```txt

->josephus2 41 3 4
With 41 people, counting by 3, the 4 safe places are:
	3
        34
        15
        30
->

```



## J

Using the executioner's algorithm.

###  Tacit version



```J
   3 ([ (1 }. <:@[ |. ])^:(1 < #@])^:_ i.@]) 41
30
```

Structured derivation of the fixed tacit code

```J
   DropNext=. 1 }. <:@[ |. ]
   MoreThanOne=. 1 < #@]
   WhileMoreThanOne=. (^:MoreThanOne f.) (^:_)
   prisoners=. i.@]

   [ DropNext WhileMoreThanOne prisoners f.
[ (1 }. <:@[ |. ])^:(1 < #@])^:_ i.@]
```



###  Explicit version



```J
Josephus =: dyad define NB. explicit form, assume executioner starts at position 0
 NB. use:  SKIP josephus NUMBER_OF_PRISONERS
 N =: y
 K =: N | x
 EXECUTIONER =: 0
 PRISONERS =: i. N
 kill =: ] #~ (~: ([: i. #))
 while. 1 (< #) PRISONERS do.
  EXECUTIONER =: (# PRISONERS) | <: K + EXECUTIONER
  PRISONERS =: EXECUTIONER kill PRISONERS
 end.
)

   3 Josephus 41
30
```




###  Explicit version 2


```J
   NB. this is a direct translation of the algo from C code above.
   Josephus2 =: 4 : '(| x&+)/i. - 1+y'

   3 Josephus2 41
30
```



## Java

{{works with|Java|1.5+}}

```java5
import java.util.ArrayList;

public class Josephus {
    public static int execute(int n, int k){
        int killIdx = 0;
        ArrayList<Integer> prisoners = new ArrayList<Integer>(n);
        for(int i = 0;i < n;i++){
            prisoners.add(i);
        }
        System.out.println("Prisoners executed in order:");
        while(prisoners.size() > 1){
            killIdx = (killIdx + k - 1) % prisoners.size();
            System.out.print(prisoners.get(killIdx) + " ");
            prisoners.remove(killIdx);
        }
        System.out.println();
        return prisoners.get(0);
    }

    public static ArrayList<Integer> executeAllButM(int n, int k, int m){
        int killIdx = 0;
        ArrayList<Integer> prisoners = new ArrayList<Integer>(n);
        for(int i = 0;i < n;i++){
            prisoners.add(i);
        }
        System.out.println("Prisoners executed in order:");
        while(prisoners.size() > m){
            killIdx = (killIdx + k - 1) % prisoners.size();
            System.out.print(prisoners.get(killIdx) + " ");
            prisoners.remove(killIdx);
        }
        System.out.println();
        return prisoners;
    }

    public static void main(String[] args){
        System.out.println("Survivor: " + execute(41, 3));
        System.out.println("Survivors: " + executeAllButM(41, 3, 3));
    }
}
```

{{out}}

```txt
Prisoners executed in order:
2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15
Survivor: 30
Prisoners executed in order:
2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3
Survivors: [15, 30, 34]
```


{{trans|Javascript}}

```java5
import java.util.ArrayList;
import java.util.List;

public class Josephus {

	public static void main(String[] args) {
		execute(5, 1);
		execute(41, 2);
		execute(23482, 3342, 3);
	}

	public static int[][] execute(int n, int k) {
		return execute(n, k, 1);
	}

	public static int[][] execute(int n, int k, int s) {
		List<Integer> ps = new ArrayList<Integer>(n);
		for (int i=0; i<n; i+=1) ps.add(i);
		List<Integer> ks = new ArrayList<Integer>(n-s);
		for (int i=k; ps.size()>s; i=(i+k)%ps.size()) ks.add(ps.remove(i));
		System.out.printf("Josephus(%d,%d,%d) -> %s / %s\n", n, k, s, toString(ps), toString(ks));
		return new int[][] {
			ps.stream().mapToInt(Integer::intValue).toArray(),
			ks.stream().mapToInt(Integer::intValue).toArray()
		};
	}

	private static String toString(List <Integer> ls) {
		String dot = "";
		if (ls.size() >= 45) {
			dot = ", ...";
			ls = ls.subList(0, 45);
		}
		String s = ls.toString();
		return s.substring(1, s.length()-1) + dot;
	}
}
```

{{out}}

```txt
Josephus(5,1,1) -> 2 / 1, 3, 0, 4
Josephus(41,2,1) -> 30 / 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3, 34, 15
Josephus(23482,3342,3) -> 1087, 1335, 13317 / 3342, 6685, 10028, 13371, 16714, 20057, 23400, 3261, 6605, 9949, 13293, 16637, 19981, 23325, 3187, 6532, 9877, 13222, 16567, 19912, 23257, 3120, 6466, 9812, 13158, 16504, 19850, 23196, 3060, 6407, 9754, 13101, 16448, 19795, 23142, 3007, 6355, 9703, 13051, 16399, 19747, 23095, 2961, 6310, 9659, ...

```



## JavaScript

Labels are 1-based, executioner's solution:

```javascript
var Josephus = {
  init: function(n) {
    this.head = {};
    var current = this.head;
    for (var i = 0; i < n-1; i++) {
      current.label = i+1;
      current.next = {prev: current};
      current = current.next;
    }
    current.label = n;
    current.next = this.head;
    this.head.prev = current;
    return this;
  },
  kill: function(spacing) {
    var current = this.head;
    while (current.next !== current) {
      for (var i = 0; i < spacing-1; i++) {
        current = current.next;
      }
      current.prev.next = current.next;
      current.next.prev = current.prev;
      current = current.next;
    }
    return current.label;
  }
}
```

{{out}}

```txt

> Josephus.init(30).kill(2)
29

```


With Array methods:

```javascript
function Josephus(n, k, s) {
	s = s | 1
	for (var ps=[], i=n; i--; ) ps[i]=i
	for (var ks=[], i=--k; ps.length>s; i=(i+k)%ps.length) ks.push(ps.splice(i, 1))
	document.write((arguments.callee+'').split(/\s|\(/)[1], '(', [].slice.call(arguments, 0), ') -> ', ps, ' / ', ks.length<45?ks:ks.slice(0,45)+',...' , '
')
	return [ps, ks]
}
```

{{out}}

```txt

Josephus(5,1) -> 2 / 1,3,0,4
Josephus(41,2) -> 30 / 2,5,8,11,14,17,20,23,26,29,32,35,38,0,4,9,13,18,22,27,31,36,40,6,12,19,25,33,39,7,16,28,37,10,24,1,21,3,34,15
Josephus(23482,3342,3) -> 1087,1335,13317 / 3342,6685,10028,13371,16714,20057,23400,3261,6605,9949,13293,16637,19981,23325,3187,6532,9877,13222,16567,19912,23257,3120,6466,9812,13158,16504,19850,23196,3060,6407,9754,13101,16448,19795,23142,3007,6355,9703,13051,16399,19747,23095,2961,6310,9659,...

```



## Julia

{{works with|Julia|0.6}}

'''Recursive (with Memoize)''':

```julia
using Memoize
@memoize josephus(n::Integer, k::Integer, m::Integer=1) = n == m ? collect(0:m .- 1) : mod.(josephus(n - 1, k, m) + k, n)

@show josephus(41, 3)
@show josephus(41, 3, 5)
```


{{out}}

```txt
josephus(41, 3) = [30]
josephus(41, 3, 5) = [3, 15, 21, 30, 34]
```


'''Iterative''':

```julia
function josephus(n::Integer, k::Integer, m::Integer=1)
    p, i, seq = collect(0:n-1), 0, Vector{typeof(n)}(0)
    while length(p) > m
        i = (i + k - 1) % length(p)
        push!(seq, splice!(p, i + 1))
    end
    return seq, p
end

seq, surv = josephus(41, 3)
println("Prisoner killing in order: $seq\nSurvivor: $surv")

seq, surv = josephus(41, 3, 3)
println("Prisoner killing in order: $seq\nSurvivor: $surv")
```


{{out}}

```txt
Prisoner killing in order: [2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3, 34, 15]
Survivor: [30]
Prisoner killing in order: [2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3]
Survivor: [15, 30, 34]
```



## jq

{{works with|jq|1.4}}
This section illustrates how a simulation can be directly modeled in jq
while being fast enough to solve problems such as [n,k,m] = [23482, 3343, 3].

The prisoners are numbered from 0 to (n-1) in keeping with jq's array index origin of 0, but the nature of their labeling is immaterial to the algorithm.

```jq
# A control structure, for convenience:
# as soon as "condition" is true, then emit . and stop:
def do_until(condition; next):
  def u: if condition then . else (next|u) end;
  u;

# n is the initial number; every k-th prisoner is removed until m remain.
# Solution by simulation
def josephus(n;k;m):
    reduce range(0;n) as $i ([]; . + [$i])    # Number the prisoners from 0 to (n-1)
    | do_until( length < k or length <= m; .[k:] + .[0:k-1] )
    | do_until( length <= m; (k % length) as $i | .[$i:] + .[0:$i-1] );
```

'''Examples''':

```jq
def task(n;k;m):
   "Survivors for n=\(n), k=\(k), m=\(m): \( josephus(n;k;m) )";

task(41;3;1),
task(23482; 3343; 3)
```

{{out}}
 $ jq -M -r -n -f josephus.jq
 Survivors for n=41, k=3, m=1: [30]
 Survivors for n=23482, k=3343, m=3: [13317,1087,1335]


## Kotlin


```scala
// version 1.1.3

fun josephus(n: Int, k: Int, m: Int): Pair<List<Int>, List<Int>> {
    require(k > 0 && m > 0 && n > k && n > m)
    val killed = mutableListOf<Int>()
    val survived = MutableList(n) { it }
    var start = k - 1
    outer@ while (true) {
        val end = survived.size - 1
        var i = start
        var deleted = 0
        while (i <= end) {
            killed.add(survived.removeAt(i - deleted))
            if (survived.size == m) break@outer
            deleted++
            i += k
        }
        start = i - end - 1
    }
    return Pair(survived, killed)
}

fun main(args: Array<String>) {
    val triples = listOf(Triple(5, 2, 1), Triple(41, 3, 1), Triple(41, 3, 3))
    for (triple in triples) {
        val(n, k, m) = triple
        println("Prisoners = $n, Step = $m, Survivors = $m")
        val (survived, killed)  = josephus(n, k, m)
        println("Survived   : $survived")
        println("Kill order : $killed")
        println()
    }
}
```


{{out}}

```txt

Prisoners = 5, Step = 1, Survivors = 1
Survived   : [2]
Kill order : [1, 3, 0, 4]

Prisoners = 41, Step = 1, Survivors = 1
Survived   : [30]
Kill order : [2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3, 34, 15]

Prisoners = 41, Step = 3, Survivors = 3
Survived   : [15, 30, 34]
Kill order : [2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3]

```



## Lua

Lua indexes tables starting at 1. Positions are stored from 0,n-1.

```lua
function josephus(n, k, m)
    local positions={}
    for i=1,n do
        table.insert(positions, i-1)
    end
    local i,j=1,1
    local s='Execution order: '
    while #positions>m do
        if j==k then
            s=s .. positions[i] .. ', '
            table.remove(positions, i)
            i=i-1
        end
        i=i+1
        j=j+1
        if i>#positions then i=1 end
        if j>k then j=1 end
    end
    print(s:sub(1,#s-2) .. '.')
    local s='Survivors: '
    for _,v in pairs(positions) do s=s .. v .. ', ' end
    print(s:sub(1,#s-2) .. '.')
end
josephus(41,3, 1)

```

{{out}}

```txt
Execution order: 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3, 34, 15.
Survivors: 30.

```



## MATLAB



```MATLAB
function [indAlive] = josephus(numPeople,count)
% Josephus: Given a circle of numPeople individuals, with a count of count,
% find the index (starting at 1) of the survivor [see Josephus Problem]

%% Definitions:
%   0 = dead position
%   1 = alive position
%   index = # of person

%% Setting up
arrPeople = ones(1, numPeople);
currInd = 0;

%% Counting
while (length(arrPeople(arrPeople == 1)) > 1)     % While more than 1 person is alive
    counter = 0;
    while counter ~= count                       % Counting until we hit the count
        currInd = currInd + 1;                  % Move to the next person

        if currInd > numPeople                  % If overflow, wraparound
            currInd = currInd - numPeople;
        end

        if arrPeople(currInd)                   % If the current person is alive
            counter = counter + 1;                % Add 1 person to the count
            %fprintf("Index: %d \t| Counter: %d\n", currInd, counter)           % Uncomment to display index and counter location
        end

    end

    arrPeople(currInd) = 0;                     % Kill the person we reached
    %fprintf("Killed person %d \n", currInd)                                   % Uncomment to display order of killing
    %disp(arrPeople)                                                           % Uncomment to display current status of people
end

indAlive = find(arrPeople);

end

```



## Mathematica


```mathematica
survivor[n_, k_] := Nest[Most[RotateLeft[#, k]] &, Range[0, n - 1], n - 1]
survivor[41, 3]
```

{{out}}

```txt

{30}

```


=={{header|Modula-2}}==

```modula2
MODULE Josephus;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE Josephus(n,k : INTEGER) : INTEGER;
VAR a,m : INTEGER;
BEGIN
    m := 0;
    FOR a:=1 TO n DO
        m := (m + k) MOD a;
    END;
    RETURN m
END Josephus;

VAR
    buf : ARRAY[0..63] OF CHAR;
    n,k,i : INTEGER;
    nl,kl,il : LONGCARD;
BEGIN
    n := 41;
    k := 3;
    FormatString("n = %i, k = %i, final survivor: %i\n", buf, n, k, Josephus(n, k));
    WriteString(buf);

    ReadChar
END Josephus.
```



## NetRexx

{{trans|REXX}}
Hardly any changes at all...

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

/* REXX **************************************************************
* 15.11.2012 Walter Pachl - my own solution
* 16.11.2012 Walter Pachl generalized n prisoners + w killing distance
*                         and s=number of survivors
**********************************************************************/
dead = 0                               /* nobody's dead yet          */
n = 41                                 /* number of alive prisoners  */
nn = n                                 /* wrap around boundary       */
w = 3                                  /* killing count              */
s = 1                                  /* nuber of survivors         */
p = -1                                 /* start here                 */
killed = ''                            /* output of killings         */
Loop until n = s                       /* until one alive prisoner   */
  found = 0                            /* start looking              */
  Loop Until found = w                 /* until we have the third    */
    p = p + 1                          /* next position              */
    If p = nn Then p = 0               /* wrap around                */
    If dead[p] = 0 Then                /* a prisoner who is alive    */
      found = found + 1                /* increment found count      */
    End
  dead[p] = 1
  n = n - 1                            /* shoot the one on this pos. */
  killed = killed p                    /* add to output              */
  End                                  /* End of main loop           */
Say 'killed:'killed.subword(1, 20)     /* output killing sequence    */
Say '       'killed.subword(21)        /* output killing sequence    */
Say 'Survivor(s):'                     /* show                       */
Loop i = 0 To 40                       /* look for the surviving p's */
  If dead[i] = 0 Then Say i            /* found one                  */
  End
```

{{out}}

```txt

killed:2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27
       31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15
Survivor(s):
30

```



## Nim


{{trans|Python}}


```nim
import sequtils, strutils, future

proc j(n, k): string =
  var
    p = toSeq(0 .. < n)
    i = 0
    s = newSeq[int]()

  while p.len > 0:
    i = (i + k - 1) mod p.len
    s.add p[i]
    system.delete(p, i)

  result = "Prisoner killing order: "
  result.add s.map((x: int) => $x).join(", ")
  result.add ".\nSurvivor: "
  result.add($s[s.high])

echo j(5,2)
echo j(41,3)
```

{{out}}

```txt
Prisoner killing order: 1, 3, 0, 4, 2.
Survivor: 2
Prisoner killing order: 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3, 34, 15, 30.
Survivor: 30
```



## Objeck


```objeck
class Josephus {
  function : Execute(n : Int, k : Int) ~ Int {
    killIdx := 0;
    prisoners := Collection.IntVector->New();
    for(i := 0;i < n;i+=1;){
      prisoners->AddBack(i);
    };

    "Prisoners executed in order:"->PrintLine();
    while(prisoners->Size() > 1){
      killIdx := (killIdx + k - 1) % prisoners->Size();
      executed := prisoners->Get(killIdx);
      "{$executed} "->Print();
      prisoners->Remove(killIdx);
    };
    '\n'->Print();
    return prisoners->Get(0);
  }

  function : ExecuteAllButM(n : Int, k : Int, m : Int) ~ Collection.IntVector {
    killIdx := 0;
    prisoners := Collection.IntVector->New();
    for(i := 0;i < n;i+=1;){
      prisoners->AddBack(i);
    };
    "Prisoners executed in order:"->PrintLine();
    while(prisoners->Size() > m){
      killIdx := (killIdx + k - 1) % prisoners->Size();
      executed := prisoners->Get(killIdx);
      "{$executed} "->Print();
      prisoners->Remove(killIdx);
    };
    '\n'->Print();
    return prisoners;
  }

  function : Main(args : String[]) ~ Nil {
    result := Execute(41, 3);
    "Survivor: {$result}"->PrintLine();

    results := ExecuteAllButM(41, 3, 3);
    "Survivors: "->Print();
    each(i : results) {
    results->Get(i)->Print();
      if(i + 1 < results->Size()) {
        ' '->Print();
      };
    };
  }
}

```



## Oforth


Oforth lists are 1-based : prisoners are numbered from 1 to n.


```Oforth
: josephus(n, k)
| prisoners killed i |
   n seq asListBuffer ->prisoners
   ListBuffer newSize(n) ->killed

   0 n 1- loop: i [
      k 1- + prisoners size mod dup 1+ prisoners removeAt
      killed add
      ] drop

   System.Out "Killed : " << killed << "\nSurvivor : " << prisoners << cr
;

```


{{out}}

```txt

>josephus(41, 3)
Killed : [3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 1, 5, 10, 14, 19, 23, 28, 32, 37, 41, 7, 13, 20, 26, 34, 40, 8, 17, 29, 38, 11, 25, 2, 22, 4, 35, 16]
Survivor : [31]

```



## PARI/GP


```parigp
Josephus(n, k)=if(n<2, n>0, my(t=(Josephus(n-1, k)+k)%n); if(t, t, n))
```



## Perl

{{trans|Perl6}}

```Perl
my @prisoner = 0 .. 40;
my $k = 3;
until (@prisoner == 1) {
    push @prisoner, shift @prisoner for 1 .. $k-1;
    shift @prisoner;
}

print "Prisoner @prisoner survived.\n"
```

{{Out}}

```txt
Prisoner 30 survived.
```



## Perl 6

{{Works with|rakudo|2015-11-12}}
Straightforward implementation of the executioner's algorithm:

```perl6
sub Execute(@prisoner, $k) {
    until @prisoner == 1 {
	@prisoner.=rotate($k - 1);
	@prisoner.shift;
    }
}

my @prisoner = ^41;
Execute @prisoner, 3;
say "Prisoner {@prisoner} survived.";

# We don't have to use numbers.  Any list will do:

my @dalton = <Joe Jack William Averell Rantanplan>;
Execute @dalton, 2;
say "{@dalton} survived.";
```


{{out}}

```txt
Prisoner 30 survived.
William survived.
```



## Phix

Note indexes and results are 1-based. Prisoners do not have to be numbers. Based on AWK, but replacing killed prisoners in-situ.

```Phix
function Josephus(sequence prisoners, integer step, survivors)
    integer n = length(prisoners), nn = n
    integer p = 0
    while n>survivors do
        integer found = 0
        while found!=step do
            p = iff(p=nn?1:p+1)
            found += prisoners[p]!=-1
        end while
        -- (if you want a kill list, build it here!)
        prisoners[p] = -1
        n -= 1
    end while
    return remove_all(-1,prisoners)
end function

?Josephus(tagset(5),2,1)
?Josephus(tagset(41),3,1)
?Josephus(tagset(41),3,3)
?Josephus({"Joe","Jack","William","John","James"},2,1)
```

{{out}}

```txt

{3}
{31}
{16,31,35}
{"William"}

```



## PHP


```php
<?php //Josephus.php
function Jotapata($n=41,$k=3,$m=1){$m--;
	$prisoners=array_fill(0,$n,false);//make a circle of n prisoners, store false ie: dead=false
	$deadpool=1;//count to next execution
	$order=0;//death order and *dead* flag, ie. deadpool
	while((array_sum(array_count_values($prisoners))<$n)){//while sum of count of unique values dead times < n (they start as all false)
		foreach($prisoners as $thisPrisoner=>$dead){
			if(!$dead){//so yeah...if not dead...
				if($deadpool==$k){//if their time is up in the deadpool...
					$order++;
					//set the deadpool value or enumerate as survivor
					$prisoners[$thisPrisoner]=((($n-$m)>($order)?$order:(($n)==$order?'Call me *Titus Flavius* Josephus':'Joe\'s friend '.(($order)-($n-$m-1)))));
					$deadpool=1;//reset count to next execution
				}else{$duckpool++;}
			}
		}
	}
	return $prisoners;
}
echo '
```txt
'.print_r(Jotapata(41,3,5),true).'
```txt
';

```




## PicoLisp

The counting starts from one instead of zero. The last remaining person is returned.

```PicoLisp

#general solution
(de jo (N K)
   (if (=1 N)
      1
      (inc
         (%
            (+ (dec K) (jo (dec N) K))
            N ) ) ) )

#special case when K is 2; much faster than general version.
(de jo2(N)
   (let P 1
      (while (<= P N)
         (setq P (* 2 P))
         (+ (- (* 2 N) P) 1) ) ) )

# find the survivor using an optimal solution
(de survivor (N K)
   (if (=0 (% N 2))
      (jo2 N)
      (jo N K) ) )
(print (survivor 5 2))
(print (survivor 41 3))

```

{{out}}

```txt

3
31

```



## PL/I


```pli
*process or(!) source attributes xref;
 joseph: Proc Options(main);
 /* REXX **************************************************************
 * 15.11.2012 Walter Pachl - my own solution
 * 16.11.2012 Walter Pachl generalized n prisoners + w killing distance
 *                         and s=number of survivors
 * 03.05.2013 Walter Pachl Translated From REXX Version 1
 **********************************************************************/
 Dcl dead(0:100) Bit(1);
 Dcl (n,nn,w,s,p,found) Bin Fixed(15);
 Dcl pp Pic'99';
 Dcl killed Char(300) Var Init('killed: '); /* output of killings     */
 Dcl survived Char(300) Var Init('Survivor(s): ');
 dead='';                               /* nobody's dead yet          */
 n=41;                                  /* number of alive prisoners  */
 nn=n;                                  /* wrap around boundary       */
 w=3;                                   /* killing count              */
 s=1;                                   /* number of survivors         */
 p=-1;                                  /* start here                 */
 Do Until(n=s);                         /* until one alive prisoner   */
   found=0;                             /* start looking              */
   Do Until(found=w);                   /* until we have the third    */
     p=p+1;                             /* next position              */
     If p=nn Then p=0;                  /* wrap around                */
     If ^dead(p) Then                   /* a prisoner who is alive    */
       found=found+1;                   /* increment found count      */
     End;
   dead(p)='1'b;                        /* shoot the one on this pos. */
   n=n-1;
   pp=p;
   killed=killed!!' '!!pp;              /* add to output              */
   End;                                 /* End of main loop           */
 Call o(killed);
 Do i=0 To nn-1;                        /* look for the surviving p's */
   If ^dead(i) Then Do;                 /* found one                  */
     pp=i;
     survived=survived!!' '!!pp;
     End;
   End;
 Call o(survived);

 o: Proc(s);
 /*********************************************************************
 * Formatted Output of given string:
 * xxxxxxxxxx xxx xx xx xxx ---
 *         xx xxx xxx
 *         xxxxx xxx
 *********************************************************************/
 Dcl s Char(*) Var;
 Dcl p Bin Fixed(15);
 Dcl ll Bin Fixed(15) Init(72);
 Do While(length(s)>ll);
   Do p=ll+1 To 10 By -1;
     If substr(s,p,1)=' ' Then
       Leave;
     End;
   Put Edit(left(s,p))(Skip,a);
   s=repeat(' ',8)!!substr(s,p+1);
   End;
 Put Edit(s)(Skip,a);
 End;

 End;
```

{{out}}

```txt
killed:  02 05 08 11 14 17 20 23 26 29 32 35 38 00 04 09 13 18 22 27 31
         36 40 06 12 19 25 33 39 07 16 28 37 10 24 01 21 03 34 15
Survivor(s):  30

```



## PowerShell

{{works with|PowerShell|2}}
Adapted from the iterative algorithm in Sidef.

Rotating the circle K prisoners is equivalent to the executioner walking around the circle K prisoners.
We rotate the circle to bring the next selectee to the "front" of the circle, then "select" him
by moving past him to the remaining circle. After repeating through the entire prisoner population, we
are left with the prisoners sorted into the order in which they are selected.

The lonely comma in the line where we create the $Prisoners arraylist is to prevent PowerShell from being too helpful.
Normally when we present the PowerShell parser with an array within an array, it treats it as a cast, and
we end up with the single array of elements. In those cases where we need an array to be treated as a single element of a parent array, we can use the unary comma to force PowerShell to treat it as an element.

```PowerShell

function Get-JosephusPrisoners ( [int]$N, [int]$K )
    {
    #  Just for convenience
    $End = $N - 1

    #  Create circle of prisoners
    $Prisoners = New-Object System.Collections.ArrayList ( , (0..$End) )

    #  For each starting point of the reducing circle...
    ForEach ( $Start in 0..($End - 1) )
        {
        #  We subtract one from K for the one we advanced by incrementing $Start
        #  Then take K modulus the length of the remaining circle
        $RoundK = ( $K - 1 ) % ( $End - $Start + 1 )

        #  Rotate the remaining prisoners K places around the remaining circle
        $Prisoners.SetRange( $Start, $Prisoners[ $Start..$End ][ ( $RoundK + $Start - $End - 1 )..( $RoundK - 1 ) ] )
        }
    return $Prisoners
    }

```


```PowerShell

#  Get the prisoner order for a circle of 41 prisoners, selecting every third
$Prisoners = Get-JosephusPrisoners -N 41 -K 3

#  Display the prisoner order
$Prisoners -join " "

#  Display the last remaining prisoner
"Last prisoner remmaining: " + $Prisoners[-1]

#  Display the last three remaining prisoners
$S = 3
"Last $S remaining: " + $Prisoners[-$S..-1]

```

{{out}}

```txt

2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15 30
Last prisoner remmaining: 30
Last 3 remaining: 34 15 30

```




## Processing

Translation of Java example.

```processing
void setup() {
  println("Survivor: " + execute(41, 3));
  println("Survivors: " + executeAllButM(41, 3, 3));
}

int execute(int n, int k) {
  int killIdx = 0;
  IntList prisoners = new IntList(n);
  for (int i = 0; i < n; i++) {
    prisoners.append(i);
  }
  println("Prisoners executed in order:");
  while (prisoners.size() > 1) {
    killIdx = (killIdx + k - 1) % prisoners.size();
    print(prisoners.get(killIdx) + " ");
    prisoners.remove(killIdx);
  }
  println();
  return prisoners.get(0);
}

IntList executeAllButM(int n, int k, int m) {
  int killIdx = 0;
  IntList prisoners = new IntList(n);
  for (int i = 0; i < n; i++) {
    prisoners.append(i);
  }
  println("Prisoners executed in order:");
  while (prisoners.size() > m) {
    killIdx = (killIdx + k - 1) % prisoners.size();
    print(prisoners.get(killIdx) + " ");
    prisoners.remove(killIdx);
  }
  println();
  return prisoners;
}
```




## PureBasic


```purebasic
NewList prisoners.i()

Procedure f2l(List p.i())
  FirstElement(p())    : tmp.i=p()
  DeleteElement(p(),1) : LastElement(p())
  AddElement(p())      : p()=tmp
EndProcedure

Procedure l2f(List p.i())
  LastElement(p())   : tmp.i=p()
  DeleteElement(p()) : FirstElement(p())
  InsertElement(p()) : p()=tmp
EndProcedure

OpenConsole()
Repeat
  Print(#LF$+#LF$)
  Print("Josephus problem - input prisoners : ") : n=Val(Input())
  If n=0 : Break : EndIf
  Print("                 - input steps     : ") : k=Val(Input())
  Print("                 - input survivors : ") : s=Val(Input()) : If s<1 : s=1 : EndIf
  ClearList(prisoners()) : For i=0 To n-1 : AddElement(prisoners()) : prisoners()=i : Next
  If n<100 : Print("Executed : ") : EndIf
  While ListSize(prisoners())>s And n>0 And k>0 And k<n
    For j=1 To k : f2l(prisoners()) : Next
    l2f(prisoners()) : FirstElement(prisoners()) : If n<100 : Print(Str(prisoners())+Space(2)) : EndIf
    DeleteElement(prisoners())
  Wend
  Print(#LF$+"Surviving: ")
  ForEach prisoners()
    Print(Str(prisoners())+Space(2))
  Next
ForEver
End
```

{{out}}

```txt
Josephus problem - input prisoners : 5
                 - input steps     : 2
                 - input survivors : 1
Executed : 1  3  0  4
Surviving: 2

Josephus problem - input prisoners : 41
                 - input steps     : 3
                 - input survivors : 1
Executed : 2  5  8  11  14  17  20  23  26  29  32  35  38  0  4  9  13  18  22  27  31  36  40  6  12  19  25  33  39  7  16  28  37  10  24  1  21  3  34  15
Surviving: 30

Josephus problem - input prisoners : 41
                 - input steps     : 3
                 - input survivors : 3
Executed : 2  5  8  11  14  17  20  23  26  29  32  35  38  0  4  9  13  18  22  27  31  36  40  6  12  19  25  33  39  7  16  28  37  10  24  1  21  3
Surviving: 15  30  34

Josephus problem - input prisoners : 71
                 - input steps     : 47
                 - input survivors : 11
Executed : 46  22  70  48  26  5  56  36  17  0  54  38  23  9  66  55  43  33  25  16  11  6  2  69  68  1  4  10  15  24  32  42  53  65  20  40  60  19  47  8  44  13  52  31  12  62  57  50  51  61  7  30  59  34  18  3  21  37  67  63
Surviving: 64  14  27  28  29  35  39  41  45  49  58

Josephus problem - input prisoners :
```



## Python


```python>>>
 def j(n, k):
	p, i, seq = list(range(n)), 0, []
	while p:
		i = (i+k-1) % len(p)
		seq.append(p.pop(i))
	return 'Prisoner killing order: %s.\nSurvivor: %i' % (', '.join(str(i) for i in seq[:-1]), seq[-1])

>>> print(j(5, 2))
Prisoner killing order: 1, 3, 0, 4.
Survivor: 2
>>> print(j(41, 3))
Prisoner killing order: 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3, 34, 15.
Survivor: 30
>>>
```


Faster way to solve in python, it does not show the killing order.

```python>>>
def josephus(n, k):
        r = 0
        for i in xrange(1, n+1):
            r = (r+k)%i
        return 'Survivor: %d' %r

>>> print(josephus(5, 2))
Survivor: 2
>>> print(josephus(41, 3))
Survivor: 30
>>>
```



### Alternate solution with a circular linked list


The function returns the killing order. The last in the list stays alive. Notice that the result is a permutation of [0, 1, ... n - 1].
In the program, a[p] is the index of the next living prisoner after 'p'. The program stops when p = a[p], that is, when there remains only one living prisoner.


```python
def josephus(n, k):
    a = list(range(1, n + 1))
    a[n - 1] = 0
    p = 0
    v = []
    while a[p] != p:
        for i in range(k - 2):
            p = a[p]
        v.append(a[p])
        a[p] = a[a[p]]
        p = a[p]
    v.append(p)
    return v

josephus(10, 2)
[1, 3, 5, 7, 9, 2, 6, 0, 8, 4]

josephus(41, 3)[-1]
30
```



### learning iter in python


```python
from itertools import compress, cycle
def josephus(prisoner, kill, surviver):
    p = range(prisoner)
    k = [0] * kill
    k[kill-1] = 1
    s = [1] * kill
    s[kill -1] = 0
    queue = p

    queue = compress(queue, cycle(s))
    try:
        while True:
            p.append(queue.next())
    except StopIteration:
        pass

    kil=[]
    killed = compress(p, cycle(k))
    try:
        while True:
            kil.append(killed.next())
    except StopIteration:
        pass

    print 'The surviver is: ', kil[-surviver:]
    print 'The kill sequence is ', kil[:prisoner-surviver]

josephus(41,3,2)
The surviver is:  [15, 30]
The kill sequence is  [2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3, 34]
josephus(5,2,1)
The surviver is:  [2]
The kill sequence is  [1, 3, 0, 4]

```



## R


```R

jose <-function(s, r,n){
y <- 0:(r-1)
 for (i in (r+1):n)
  y <- (y + s) %% i
 return(y)
}
> jose(3,1,41) # r is the number of remained prisoner.
[1] 30

```




## Racket


```Racket
#lang racket
(define (josephus n k (m 0))
  (for/fold ((m (add1 m)))
    ((a (in-range (add1 m) (add1 n))))
    (remainder (+ m k) a)))

(josephus 41 3) ; ->30
```



## REBOL

Works in Rebol 2 or 3

```REBOL
Rebol []

execute: func [death-list [block!] kill [integer!]] [
    assert [not empty? death-list]
    until [
        loop kill - 1 [append death-list take death-list]
        (1 == length? remove death-list)
    ]
]

prisoner: [] for n 0 40 1 [append prisoner n]
execute prisoner 3
print ["Prisoner" prisoner "survived"]
```

{{out}}

```txt
Prisoner 30 survived
```

And any kind of list will do:

```REBOL
for-the-chop: [Joe Jack William Averell Rantanplan]
execute for-the-chop 2
print [for-the-chop "survived"]
```

{{out}}

```txt
William survived
```



## REXX


### version 1


```rexx
/* REXX **************************************************************
* 15.11.2012 Walter Pachl - my own solution
* 16.11.2012 Walter Pachl generalized n prisoners + w killing distance
*                         and s=number of survivors
* 09.05.2013 Walter Pachl accept arguments n w s and fix output
*                         thanks for the review/test
* I see no need for specifying a start count (actually a start number)
* This program should work on EVERY REXX.
* Pls report if this is not the case and let us know what's a problem.
**********************************************************************/
Parse Arg n w s .
If n='?' Then Do
  Say 'Invoke the program with the following arguments:'
  Say 'n number of prisoners            (default 41)'
  Say 'w killing count                  (default  3)'
  Say 's number of prisoners to survive (default  1)'
  Exit
  End
If n='' Then n=41                      /* number of alive prisoners  */
If w='' Then w=3                       /* killing count              */
If s='' Then s=1                       /* nuber of survivors         */
dead.=0                                /* nobody's dead yet          */
nn=n                                   /* wrap around boundary       */
p=-1                                   /* start here                 */
killed=''                              /* output of killings         */
Do until n=s                           /* until one alive prisoner   */
  found=0                              /* start looking              */
  Do Until found=w                     /* until we have the third    */
    p=p+1                              /* next position              */
    If p=nn Then p=0                   /* wrap around                */
    If dead.p=0 Then                   /* a prisoner who is alive    */
      found=found+1                    /* increment found count      */
    End
  dead.p=1
  /*
  Say 'killing' p 'now'
  */
  n=n-1                                /* shoot the one on this pos. */
  killed=killed p                      /* add to output              */
  End                                  /* End of main loop           */
Say 'killed:'killed                    /* output killing sequence    */
s=''
Do i=0 To nn-1                            /* look for the surviving p's */
  If dead.i=0 Then s=s i               /* found one                  */
  End
Say 'Survivor(s):'s                    /* show                       */
```


{{out}}

```txt
killed: 2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15
Survivor(s): 30
```



### version 2

This version allows the user to specify:
::*   the number of prisoners
::*   the count-off   [every K<sup>th</sup> prisoner]
::*   the start count   [zero or one]
::*   the number of survivors
::*   the solving of the extra credit task requirement of multiple survivors
The output echoes the choices specified and was made "English" readable.

This solution is an   ''executor's''   solution.

```rexx
/*REXX program solves  Josephus problem:   N  men standing in a circle,  every Kth kilt.*/
parse arg N K Z R .                              /*obtain optional arguments from the CL*/
if N=='' | N==","   then  N= 41                  /*    men  not specified?  Use default.*/
if K=='' | K==","   then  K=  3                  /*   kilt   "      "        "     "    */
if Z=='' | Z==","   then  Z=  0                  /*  start   "      "        "     "    */
if R=='' | R==","   then  R=  1                  /*remaining "      "        "     "    */
$=;       do i=Z  for N;  $=$ i;  end  /*i*/     /*populate prisoner's circle (with a #)*/
x=                                               /*the list of prisoners to be removed. */
      do c=k  by k;         p=words($)           /*keep removing until  R  are remaining*/
      if c>p then do                             /*   [↓] remove (kill) some prisoner(s)*/
                    do j=1  for words(x);    $=delword($, word(x, j) + 1 - j,   1)
                    if words($)==R  then leave c /*The slaying finished? (R people left)*/
                    end   /*j*/
                  c=(c//p) // words($);   x=     /*adjust prisoner count-off and circle.*/
                  end
      if c\==0  then x=x c                       /*the list of prisoners to be removed. */
      end   /*remove*/                           /*remove 'til   R   prisoners are left.*/

say 'removing every '   th(K)   " prisoner out of "    N    ' (starting at'   Z")  with ",
                           R    ' survivor's(R)",  leaving prisoner"s(R)':'   $
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:  if arg(1)==1  then return arg(3);            return word( arg(2) 's', 1)   /*plurals*/
th: y=arg(1);    return y || word('th st nd rd', 1+ y // 10 * (y//100%10\==1) * (y//10<4))
```

{{out|output|text=  when using the default inputs:}}

```txt

removing every  3rd  prisoner out of  41  (starting at 0)  with  1  survivor,  leaving prisoner:  30

```

{{out|output|text=  when using the input of:   <tt> 41   3   1 </tt>}}

```txt

removing every  3rd  prisoner out of  41  (starting at 1)  with  1  survivor,  leaving prisoner:  31

```

{{out|output|text=  when using the input of:   <tt> 41   3   1   2 </tt>

```txt

removing every  3rd  prisoner out of  41  (starting at 1)  with  2  survivors,  leaving prisoners:  16 31

```

{{out|output|text=  when using the input of:   <tt> 5   2 </tt>

```txt

removing every  2nd  prisoner out of  5  (starting at 0)  with  1  survivor,  leaving prisoner:  2

```



## Ring


```ring

n = 41
k=3
see "n =" + n + " k = " + k + " final survivor = " + josephus(n, k, 0) + nl

func josephus (n, k, m)
lm = m
for a = m+1  to n
     lm = (lm+k) % a
next
josephus = lm
return josephus

```

Output:

```txt

n =41 k = 3 final survivor = 30

```



## Ruby


```Ruby
n = (ARGV[0] || 41).to_i
k = (ARGV[1] || 3).to_i

prisoners = (0...n).to_a
prisoners.rotate!(k-1).shift  while prisoners.length > 1
puts prisoners.first
```



## Rust


```rust
const N: usize = 41;
const K: usize = 3;
const M: usize = 3;
const POSITION: usize = 5;

fn main() {
    let mut prisoners: Vec<usize> = Vec::new();
    let mut executed: Vec<usize> = Vec::new();
    for pos in 0..N {
        prisoners.push(pos);
    }

    let mut to_kill: usize = 0;
    let mut len: usize = prisoners.len();

    while len > M {
        to_kill = (to_kill + K - 1) % len;
        executed.push(prisoners.remove(to_kill));
        len -= 1;
    }

    println!("JOSEPHUS n={}, k={}, m={}", N, K, M);
    println!("Executed: {:?}", executed);
    println!("Executed position number {}: {}", POSITION, executed[POSITION - 1]);
    println!("Survivors: {:?}", prisoners);
}
```

{{out}}

```txt

JOSEPHUS n=41, k=3, m=3
Executed: [2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3]
Executed position number 5: 14
Survivors: [15, 30, 34]

```



## Scala

Executioner's Solution, not Josephus'

(Prisoners labeled 0 to n-1)

```scala
def executed( prisonerCount:Int, step:Int ) = {

  val prisoners = ((0 until prisonerCount) map (_.toString)).toList

  def behead( dead:Seq[String], alive:Seq[String] )(countOff:Int) : (Seq[String], Seq[String]) = {
    val group = if( alive.size < countOff ) countOff - alive.size else countOff

    (dead ++ alive.take(group).drop(group-1), alive.drop(group) ++ alive.take(group-1))
  }

  def beheadN( dead:Seq[String], alive:Seq[String] ) : (Seq[String], Seq[String]) =
    behead(dead,alive)(step)

  def execute( t:(Seq[String], Seq[String]) ) : (Seq[String], Seq[String]) = t._2 match {
    case x :: Nil => (t._1, Seq(x))
    case x :: xs => execute(beheadN(t._1,t._2))
  }

  execute((List(),prisoners))
}

val (dead,alive) = executed(41,3)

println( "Prisoners executed in order:" )
print( dead.mkString(" ") )

println( "\n\nJosephus is prisoner " + alive(0) )
```

{{out}}

```txt
Prisoners executed in order:
2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15

Josephus is prisoner 30
```



## Seed7

The main task (find one survivor) is a special case of the extra task (find m survivors).
The function ''executeAllButM'' solves the extra task and is called with m=1 to solve the main task.
The function ''str'' converts an array of integer elements to a string.
The function [http://seed7.sourceforge.net/libraries/enable_output.htm#enable_output%28in_type%29 enable_output]
uses ''str'' to define everything necessary to write an array of integers.
This way the main program can write the survivor array.

```seed7
$ include "seed7_05.s7i";

const func array integer: executeAllButM (in integer: n, in integer: k, in integer: m) is func
  result
    var array integer: prisoners is [0 .. -1] times 0;
  local
    var integer: killIdx is 0;
    var integer: prisonerNum is 0;
  begin
    for prisonerNum range 0 to pred(n) do
      prisoners &:= prisonerNum;
    end for;
    writeln("Prisoners executed in order:");
    while length(prisoners) > m do
      killIdx := (killIdx + k - 1) rem length(prisoners);
      write(prisoners[killIdx] <& " ");
      ignore(remove(prisoners, killIdx));
    end while;
    writeln;
  end func;

const func string: str (in array integer: intArr) is func
  result
    var string: stri is "";
  local
    var integer: index is 0;
  begin
    for key index range intArr do
      if index <> minIdx(intArr) then
        stri &:= ", ";
      end if;
      stri &:= str(intArr[index]);
    end for;
  end func;

enable_output(array integer);

const proc: main is func
  begin
    writeln("Survivor: " <& executeAllButM(41, 3, 1));
    writeln("Survivors: " <& executeAllButM(41, 3, 3));
  end func;
```


{{out}}

```txt

Prisoners executed in order:
2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15
Survivor: 30
Prisoners executed in order:
2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3
Survivors: 15, 30, 34

```



## SequenceL

{{trans|Python}}

```sequencel
main := josephus(41, 3);

josephus(n, k) := josephusHelper(n, k, 1, 0);

josephusHelper(n, k, i, r) :=
        r when i > n
    else
        josephusHelper(n, k, i + 1, (r + k) mod i);
```


{{out}}

```txt

30

```



## Sidef

Iterative:

```ruby
func josephus(n, k) {
    var prisoners = @^n
    while (prisoners.len > 1) {
        prisoners.rotate!(k - 1).shift
    }
    return prisoners[0]
}
```


Recursive:

```ruby
func josephus(n, k) {
    n == 1 ? 0 : ((__FUNC__(n-1, k) + k) % n)
};
```


Calling the function:

```ruby
var survivor = josephus(41, 3);
say "Prisoner #{survivor} survived.";
```

{{out}}

```txt
Prisoner 30 survived.
```



## Swift


```Swift
class Josephus {

    class func lineUp(#numberOfPeople:Int) -> [Int] {
        var people = [Int]()
        for (var i = 0; i < numberOfPeople; i++) {
            people.append(i)
        }
        return people
    }

    class func execute(#numberOfPeople:Int, spacing:Int) -> Int {
        var killIndex = 0
        var people = self.lineUp(numberOfPeople: numberOfPeople)

        println("Prisoners executed in order:")
        while (people.count > 1) {
            killIndex = (killIndex + spacing - 1) % people.count
            executeAndRemove(&people, killIndex: killIndex)
        }
        println()
        return people[0]
    }

    class func executeAndRemove(inout people:[Int], killIndex:Int) {
        print("\(people[killIndex]) ")
        people.removeAtIndex(killIndex)
    }

    class func execucteAllButM(#numberOfPeople:Int, spacing:Int, save:Int) -> [Int] {
        var killIndex = 0
        var people = self.lineUp(numberOfPeople: numberOfPeople)

        println("Prisoners executed in order:")
        while (people.count > save) {
            killIndex = (killIndex + spacing - 1) % people.count
            executeAndRemove(&people, killIndex: killIndex)
        }
        println()
        return people
    }
}

println("Josephus is number: \(Josephus.execute(numberOfPeople: 41, spacing: 3))")
println()
println("Survivors: \(Josephus.execucteAllButM(numberOfPeople: 41, spacing: 3, save: 3))")
```

{{out}}

```txt

Prisoners executed in order:
2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15
Josephus is number: 30

Prisoners executed in order:
2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3
Survivors: [15, 30, 34]

```



## Tcl


```tcl
proc josephus {number step {survivors 1}} {
    for {set i 0} {$i<$number} {incr i} {lappend l $i}
    for {set i 1} {[llength $l]} {incr i} {
	# If the element is to be killed, append to the kill sequence
	if {$i%$step == 0} {
	    lappend killseq [lindex $l 0]
	    set l [lrange $l 1 end]
	} else {
	    # Roll the list
	    set l [concat [lrange $l 1 end] [list [lindex $l 0]]]
	}
    }
    return [lrange $killseq end-[expr {$survivors-1}] end]
}
```

Demonstrating:

```tcl
puts "remaining:   [josephus 41 3]"
puts "remaining 4: [join [josephus 41 3 4] ,]"
```

{{out}}

```txt

remaining:   30
remaining 4: 3,34,15,30

```



## VBScript


```vb

Function josephus(n,k,s)
	Set prisoner = CreateObject("System.Collections.ArrayList")
	For i = 0 To n - 1
		prisoner.Add(i)
	Next
	index = -1
	Do Until prisoner.Count = s
		step_count = 0
		Do Until step_count = k
			If index+1 <= prisoner.Count-1 Then
				index = index+1
			Else
				index = (index+1)-(prisoner.Count)
			End If
			step_count = step_count+1
		Loop
		prisoner.RemoveAt(index)
		index = index-1
	Loop
	For j = 0 To prisoner.Count-1
		If j < prisoner.Count-1 Then
			josephus = josephus & prisoner(j) & ","
		Else
			josephus = josephus & prisoner(j)
		End If
	Next
End Function

'testing the function
WScript.StdOut.WriteLine josephus(5,2,1)
WScript.StdOut.WriteLine josephus(41,3,1)
WScript.StdOut.WriteLine josephus(41,3,3)

```


{{Out}}

```txt

2
30
15,30,34

```



## Vedit macro language

This macro first creates a list of prisoners in an edit buffer.

Then the prisoners are deleted in loop until specified number of survivors are left.

When the macro finishes, you can see the list of survivors in the edit buffer.


```vedit
#1 = 41		// number of prisoners
#2 = 3		// step size
#3 = 1		// number of survivors

Buf_Switch(Buf_Free)
for (#5=0; #5<#1; #5++) {
    Ins_Text("prisoner ") Num_Ins(#5, LEFT)
}

BOF
#4=1
while (#1 > #3) {
    if (#4++ % #2 == 0) {
	Del_Line(1)
        #1--
    } else {
	Line(1)
    }
    if (At_EOF) { BOF }
}
```


{{out}}

```txt

prisoner 30

```


{{out}} when the number of survivors is set to 3:

```txt

prisoner 15
prisoner 30
prisoner 34

```



## Visual Basic .NET

{{trans|D}}

```vbnet
Module Module1

    'Determines the killing order numbering prisoners 1 to n
    Sub Josephus(n As Integer, k As Integer, m As Integer)
        Dim p = Enumerable.Range(1, n).ToList()
        Dim i = 0

        Console.Write("Prisoner killing order:")
        While p.Count > 1
            i = (i + k - 1) Mod p.Count
            Console.Write(" {0}", p(i))
            p.RemoveAt(i)
        End While
        Console.WriteLine()

        Console.WriteLine("Survivor: {0}", p(0))
    End Sub

    Sub Main()
        Josephus(5, 2, 1)
        Console.WriteLine()
        Josephus(41, 3, 1)
    End Sub

End Module
```

{{out}}

```txt
Prisoner killing order: 2 4 1 5
Survivor: 3

Prisoner killing order: 3 6 9 12 15 18 21 24 27 30 33 36 39 1 5 10 14 19 23 28 32 37 41 7 13 20 26 34 40 8 17 29 38 11 25 2 22 4 35 16
Survivor: 31
```



## XPL0


```XPL0
include c:\cxpl\codes;

func Prisoner(N, K);            \Return final surviving prisoner
int  N, K;                      \number of prisoners, number to skip
int  I, J;
char A;
[A:= Reserve(N);
for I:= 0 to N-1 do A(I):= I;
I:= 0;
repeat  I:= I+K-1;                              \skip to next prisoner
        I:= rem(I/N);                           \wrap to start if necessary
        IntOut(0, A(I)); ChOut(0, ^ );          \show killed prisoner
        for J:= I to N-2 do A(J):= A(J+1);      \shift survivors down
        N:= N-1;                                \one less prisoner
until   N=1;
return A(0);
];

[IntOut(0, Prisoner(5, 2));  CrLf(0);
 IntOut(0, Prisoner(41, 3));  CrLf(0);
]
```

{{out}}

```txt

1 3 0 4 2
2 5 8 11 14 17 20 23 26 29 32 35 38 0 4 9 13 18 22 27 31 36 40 6 12 19 25 33 39 7 16 28 37 10 24 1 21 3 34 15 30

```



## zkl

{{trans|Julia}}

```zkl
fcn j(n,k){
   reg p=[0..n-1].walk().copy(), i=0, seq=L();
   while(p){
      i=(i+k-1)%p.len();
      seq.append(p.pop(i));
   }
   "Prisoner killing order: %s.\nSurvivor: %d"
   .fmt(seq[0,-1].concat(","),seq[-1]);
}
```

{{out}}

```txt

j(41,3).println();
Prisoner killing order: 2,5,8,11,14,17,20,23,26,29,32,35,38,0,4,9,13,18,22,27,31,
            36,40,6,12,19,25,33,39,7,16,28,37,10,24,1,21,3,34,15.
Survivor: 30

```


```zkl
fcn j2(n,k,m){
   reg p=[0..n-1].walk().copy(), i=0, seq=L();
   while(p.len()>m){
      i=(i+k-1)%p.len();
      seq.append(p.pop(i));
   }
   "Prisoner killing order: %s.\nSurvivors: [%s]"
   .fmt(seq.concat(","),p.concat(","))
}
```

{{out}}

```txt

j2(41,3,3).println();
Prisoner killing order: 2,5,8,11,14,17,20,23,26,29,32,35,38,0,4,9,13,18,22,27,
          31,36,40,6,12,19,25,33,39,7,16,28,37,10,24,1,21,3.
Survivors: [15,30,34]

```



## ZX Spectrum Basic

{{trans|ANSI Standard BASIC}}

```zxbasic
10 LET n=41: LET k=3: LET m=0
20 GO SUB 100
30 PRINT "n= ";n;TAB (7);"k= ";k;TAB (13);"final survivor= ";lm
40 STOP
100 REM Josephus
110 REM Return m-th on the reversed kill list; m=0 is final survivor.
120 LET lm=m: REM Local copy of m
130 FOR a=m+1 TO n
140 LET lm=FN m(lm+k,a)
150 NEXT a
160 RETURN
200 DEF FN m(x,y)=x-INT (x/y)*y: REM MOD function

```


{{omit from|GUISS}}
