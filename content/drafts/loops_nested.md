+++
title = "Loops/Nested"
description = ""
date = 2019-08-28T17:09:33Z
aliases = []
[extra]
id = 4373
[taxonomies]
categories = []
tags = []
+++

{{task|Iteration}} [[Category:Loop modifiers]]

Show a nested loop which searches a two-dimensional array filled with random numbers uniformly distributed over <math>[1,\ldots,20]</math>.

The loops iterate rows and columns of the array printing the elements until the value <math>20</math> is met.

Specifically, this task also shows how to [[Loop/Break|break]] out of nested loops.


;Related tasks:
*   [[Loop over multiple arrays simultaneously]]
*   [[Loops/Break]]
*   [[Loops/Continue]]
*   [[Loops/Do-while]]
*   [[Loops/Downward for]]
*   [[Loops/For]]
*   [[Loops/For with a specified step]]
*   [[Loops/Foreach]]
*   [[Loops/Increment loop index within loop body]]
*   [[Loops/Infinite]]
*   [[Loops/N plus one half]]
*   [[Loops/Nested]]
*   [[Loops/While]]
*   [[Loops/with multiple ranges]]
*   [[Loops/Wrong ranges]]





## 360 Assembly


```360asm
*        Loop nested               12/08/2015
LOOPNEST CSECT
         USING  LOOPNEST,R12
         LR     R12,R15
BEGIN    LA     R6,0               i
         LA     R8,1
         LA     R9,20
LOOPI1   BXH    R6,R8,ELOOPI1      do i=1 to hbound(x,1)
         LA     R7,0               j
         LA     R10,1
         LA     R11,20
LOOPJ1   BXH    R7,R10,ELOOPJ1     do j=1 to hbound(x,2)
         L      R5,RANDSEED        n
         M      R4,=F'397204094'   r4r5=n*const
         D      R4,=X'7FFFFFFF'    r5=r5 div (2^31-1)
         ST     R4,RANDSEED        r4=r5 mod (2^31-1) ; n=r4
         LR     R5,R4              r5=n
         LA     R4,0
         D      R4,=F'20'          r5=n div nn; r4=n mod nn
         LR     R2,R4              r2=randint(nn) [0:nn-1]
         LA     R2,1(R2)           randint(nn)+1
         LR     R1,R6              i
         BCTR   R1,0
         MH     R1,=H'20'
         LR     R5,R7              j
         BCTR   R5,0
         AR     R1,R5
         SLA    R1,2
         ST     R2,X(R1)           x(i,j)=randint(20)+1
         B      LOOPJ1
ELOOPJ1  B      LOOPI1
ELOOPI1  MVC    MVCZ,=CL80' '
         LA     R6,0               i
         LA     R8,1
         LA     R9,20
LOOPI2   BXH    R6,R8,ELOOPI2      do i=1 to hbound(x,1)
         LA     R7,0               j
         LA     R10,1
         LA     R11,20
LOOPJ2   BXH    R7,R10,ELOOPJ2     do j=1 to hbound(x,2)
         LR     R1,R6
         BCTR   R1,0
         MH     R1,=H'20'
         LR     R5,R7
         BCTR   R5,0
         AR     R1,R5
         SLA    R1,2
         L      R5,X(R1)           x(i,j)
         LR     R2,R5
         LA     R3,MVCZ
         AH     R3,MVCI
         XDECO  R2,XDEC
         MVC    0(4,R3),XDEC+8
         LH     R3,MVCI
         LA     R3,4(R3)
         STH    R3,MVCI
         L      R5,X(R1)
         C      R5,=F'20'          if x(i,j)=20
         BE     ELOOPI2            then exit
         B      LOOPJ2
ELOOPJ2  XPRNT  MVCZ,80
         MVC    MVCI,=H'0'
         MVC    MVCZ,=CL80' '
         B      LOOPI2
ELOOPI2  XPRNT  MVCZ,80
RETURN   XR     R15,R15
         BR     R14
X        DS     400F
MVCZ     DS     CL80
MVCI     DC     H'0'
XDEC     DS     CL16
RANDSEED DC     F'16807'           running n
         YREGS
         END    LOOPNEST
```

{{out}}

```txt
   3   4   1  11  13  17  11   9   8   2  15  19  16  18   1   9   7  16  12   3
  11  13  13   6  13  19   9  18  11   4   7   8   6   7   2  10  14   4   5   1
  16  14  13   6  11  20
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Test_Loop_Nested is
   type Value_Type is range 1..20;
   package Random_Values is new Ada.Numerics.Discrete_Random (Value_Type);
   use Random_Values;
   Dice : Generator;
   A : array (1..10, 1..10) of Value_Type :=
          (others => (others => Random (Dice)));
begin

Outer :
   for I in A'Range (1) loop
      for J in A'Range (2) loop
         Put (Value_Type'Image (A (I, J)));
         exit Outer when A (I, J) = 20;
      end loop;
      New_Line;
   end loop Outer;
end Test_Loop_Nested;
```

{{out|Sample output}}

```txt

 16 3 1 17 13 5 4 2 19 1
 5 5 17 15 17 2 5 5 17 13
 16 10 10 20

```



## ALGOL 60

{{works with|ALGOL 60|OS/360}}

```algol60
'BEGIN' 'COMMENT' Loops/Nested - ALGOL60 - 19/06/2018;
  'INTEGER' SEED;
  'INTEGER' 'PROCEDURE' RANDOM(N);
  'VALUE' N; 'INTEGER' N;
  'BEGIN'
    SEED:=(SEED*19157+12347) '/' 21647;
    RANDOM:=SEED-(SEED '/' N)*N+1
  'END' RANDOM;
  'INTEGER' 'ARRAY' A(/1:10,1:10/);
  'INTEGER' I,J;
  SEED:=31569;
  'FOR' I:=1 'STEP' 1 'UNTIL' 10 'DO'
    'FOR' J:=1 'STEP' 1 'UNTIL' 10 'DO'
      A(/I,J/):=RANDOM(20);
  SYSACT(1,6,120);SYSACT(1,8,60);SYSACT(1,12,1);'COMMENT' open print;
  'FOR' I:=1 'STEP' 1 'UNTIL' 10 'DO'
    'FOR' J:=1 'STEP' 1 'UNTIL' 10 'DO' 'BEGIN'
      OUTINTEGER(1,A(/I,J/));
      'IF' A(/I,J/)=20 'THEN' 'GOTO' LAB;
	'END';
LAB:
'END'
```

{{out}}

```txt

        +19           +5           +1           +4          +17           +6           +2          +18          +12
         +3          +13           +6           +8           +6          +10           +9          +15          +20

```




## ALGOL 68

{{trans|C}} - note: This specimen retains the original C coding style.
{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards)}}

```algol68
main: (
    [10][10]INT a; INT i, j;

    FOR i FROM LWB a TO UPB a DO
        FOR j FROM LWB a[i] TO UPB a[i] DO
            a[i][j] := ENTIER (random * 20 + 1)
        OD
    OD ;
    FOR i FROM LWB a TO UPB a DO
        FOR j FROM LWB a[i] TO UPB a[i] DO
            print(whole(a[i][j], -3));
            IF a[i][j] = 20 THEN
                GO TO xkcd com 292 # http://xkcd.com/292/ #
            FI
        OD;
        print(new line)
    OD;
xkcd com 292:
    print(new line)
)
```

{{out|Sample output}}

```txt

  8 14 17  6 18  1  1  7  9  6
  8  9  1 15  3  1 10 19  6  7
 12 20

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program loopnested.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall


.equ NBVALUECOL,      10
.equ NBLIGNES,        10
.equ MAXVALUE,        20

/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessResult:        .ascii " "
sMessValeur:        .fill 11, 1, ' '            @ size => 11
szCarriageReturn:   .asciz "\n"


.align 4
iGraine:  .int 314159

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
tiValues:                .skip  4 * NBVALUECOL * NBLIGNES
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                             @ entry of program
    ldr r3,iAdrtiValues
    mov r4,#0                                     @ loop indice
    mov r5,#0
    mov r7,#4 * NBVALUECOL
1:                                                @ begin loop 1
    mov r0,#MAXVALUE + 1
    bl genereraleas                               @ result 0 to MAXVALUE

    mul r6,r5,r7
    add r6,r4,lsl #2
    str r0,[r3,r6]
    add r4,#1
    cmp r4,#NBVALUECOL
    blt 1b
    mov r4,#0
    add r5,#1
    cmp r5,#NBLIGNES
    blt 1b

    mov r4,#0                                     @ loop indice
    mov r5,#0                                     @ total
    ldr r3,iAdrtiValues                           @ table values address
2:
    mul r6,r5,r7
    add r6,r4,lsl #2
    ldr r0,[r3,r6]
    ldr r1,iAdrsMessValeur                        @ display value
    bl conversion10                               @ call conversion decimal
    mov r1,#0
    ldr r0,iAdrsMessResult
    strb r1,[r0,#4]
    ldr r0,iAdrsMessResult
    bl affichageMess                              @ display message
    ldr r0,[r3,r6]
    cmp r0,#MAXVALUE
    beq 3f
    add r4,#1
    cmp r4,#NBVALUECOL
    blt 2b
    ldr r0,iAdrszCarriageReturn
    bl affichageMess                              @ display message
    mov r4,#0
    add r5,#1
    cmp r5,#NBLIGNES
    blt 2b
    b 100f
3:
    ldr r0,iAdrszCarriageReturn
    bl affichageMess                              @ display message

100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iAdrtiValues:             .int tiValues

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                      @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                   @ so here r2 contains the length of the message
    mov r1,r0                                      @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */
    bx lr                                          @ return
/******************************************************************/
/*     Converting a register to a decimal unsigned                */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                                 @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:                                                  @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0
    subne r2,#1                                     @ else previous position
    bne 1b                                          @ and loop
                                                    @ and move digit from left of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]
    add r2,#1
    add r4,#1
    cmp r2,#LGZONECAL
    ble 2b
                                                      @ and move spaces in end on area
    mov r0,r4                                         @ result length
    mov r1,#' '                                       @ space
3:
    strb r1,[r3,r4]                                   @ store space in area
    add r4,#1                                         @ next position
    cmp r4,#LGZONECAL
    ble 3b                                            @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                                    @ restaur registres
    bx lr                                             @return

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    //mov r3,#0xCCCD                                   @ r3 <- magic_number lower  raspberry 3
    //movt r3,#0xCCCC                                  @ r3 <- magic_number higter raspberry 3
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function
iMagicNumber:  	.int 0xCCCCCCCD
/***************************************************/
/*   Generation random number                  */
/***************************************************/
/* r0 contains limit  */
genereraleas:
    push {r1-r4,lr}                                    @ save registers
    ldr r4,iAdriGraine
    ldr r2,[r4]
    ldr r3,iNbDep1
    mul r2,r3,r2
    ldr r3,iNbDep1
    add r2,r2,r3
    str r2,[r4]                                        @ maj de la graine pour l appel suivant
    cmp r0,#0
    beq 100f
    mov r1,r0                                          @ divisor
    mov r0,r2                                          @ dividende
    bl division
    mov r0,r3                                          @ résult = remainder

100:                                                   @ end function
    pop {r1-r4,lr}                                     @ restaur registers
    bx lr                                              @ return
/*****************************************************/
iAdriGraine: .int iGraine
iNbDep1: .int 0x343FD
iNbDep2: .int 0x269EC3
/***************************************************/
/* integer division unsigned                       */
/***************************************************/
division:
    /* r0 contains dividend */
    /* r1 contains divisor */
    /* r2 returns quotient */
    /* r3 returns remainder */
    push {r4, lr}
    mov r2, #0                                         @ init quotient
    mov r3, #0                                         @ init remainder
    mov r4, #32                                        @ init counter bits
    b 2f
1:                                                     @ loop
    movs r0, r0, LSL #1                                @ r0 <- r0 << 1 updating cpsr (sets C if 31st bit of r0 was 1)
    adc r3, r3, r3                                     @ r3 <- r3 + r3 + C. This is equivalent to r3 ? (r3 << 1) + C
    cmp r3, r1                                         @ compute r3 - r1 and update cpsr
    subhs r3, r3, r1                                   @ if r3 >= r1 (C=1) then r3 <- r3 - r1
    adc r2, r2, r2                                     @ r2 <- r2 + r2 + C. This is equivalent to r2 <- (r2 << 1) + C
2:
    subs r4, r4, #1                                    @ r4 <- r4 - 1
    bpl 1b                                             @ if r4 >= 0 (N=0) then loop
    pop {r4, lr}
    bx lr

```




## AutoHotkey


```AutoHotkey
Loop, 10
{
  i := A_Index
  Loop, 10
  {
    j := A_Index
    Random, a%i%%j%, 1, 20
  }
}

Loop, 10
{
  i := A_Index
  Loop, 10
  {
    j := A_Index
    If (a%i%%j% == 20)
      Goto finish
  }
}

finish:
  MsgBox % "a[" . i . "][" . j . "]" is 20
Return
```



## AWK

To break from two loops, this program uses two <tt>break</tt> statements and one <tt>b</tt> flag.

```awk
BEGIN {
	rows = 5
	columns = 5

	# Fill ary[] with random numbers from 1 to 20.
	for (r = 1; r <= rows; r++) {
		for (c = 1; c <= columns; c++)
			ary[r, c] = int(rand() * 20) + 1
	}

	# Find a 20.
	b = 0
	for (r = 1; r <= rows; r++) {
		for (c = 1; c <= columns; c++) {
			v = ary[r, c]
			printf " %2d", v
			if (v == 20) {
				print
				b = 1
				break
			}
		}
		if (b) break
		print
	}
}
```



## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
DIM a(1 TO 10, 1 TO 10) AS INTEGER
CLS
FOR row = 1 TO 10
        FOR col = 1 TO 10
                a(row, col) = INT(RND * 20) + 1
        NEXT col
NEXT row

FOR row = LBOUND(a, 1) TO UBOUND(a, 1)
        FOR col = LBOUND(a, 2) TO UBOUND(a, 2)
                PRINT a(row, col)
                IF a(row, col) = 20 THEN END
        NEXT col
NEXT row
```


=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM.

A couple of points to note: (1) since the values we want are small enough to fit into an unsigned byte, we cast them to characters and store them in an array of strings—thereby using only a fifth of the storage space that an array of numbers would take up; (2) the <code>GOTO</code> statement in line <tt>100</tt> breaks out of both the enclosing loops and also, since its target is higher than any line number in the program, causes execution to terminate normally.


```basic
 10 DIM A$(20,20)
 20 FOR I=1 TO 20
 30 FOR J=1 TO 20
 40 LET A$(I,J)=CHR$ (1+INT (RND*20))
 50 NEXT J
 60 NEXT I
 70 FOR I=1 TO 20
 80 FOR J=1 TO 20
 90 PRINT CODE A$(I,J);" ";
100 IF CODE A$(I,J)=20 THEN GOTO 130
110 NEXT J
120 NEXT I
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM array(10,10)
      FOR row% = 0 TO 10
        FOR col% = 0 TO 10
          array(row%,col%) = RND(20) + 1
        NEXT
      NEXT row%
      FOR row% = 0 TO 10
        FOR col% = 0 TO 10
          PRINT "row "; row%, "col ";col%, "value "; array(row%,col%)
          IF array(row%,col%) = 20 EXIT FOR row%
        NEXT
      NEXT row%

```

EXIT FOR can jump out of multiple nested loops by specifying a control variable.


## bc

Arrays have only one dimension, so we use ''a[i * c + j]'' instead of ''a[i, j]''.
{{trans|AWK}}

```bc
s = 1	/* Seed of the random number generator */

/* Random number from 1 to 20. */
define r() {
	auto r
	while (1) {
		/*
		 * Formula (from POSIX) for random numbers of low
		 * quality, from 0 to 32767.
		 */
		s = (s * 1103515245 + 12345) % 4294967296
		r = (s / 65536) % 32768

		/* Prevent modulo bias. */
		if (r >= 32768 % 20) break
	}
	return ((r % 20) + 1)
}

r = 5	/* Total rows */
c = 5	/* Total columns */

/* Fill array a[] with random numbers from 1 to 20. */
for (i = 0; i < r; i++) {
	for (j = 0; j < c; j++) {
		a[i * c + j] = r()
	}
}

/* Find a 20. */
b = 0
for (i = 0; i < r; i++) {
	for (j = 0; j < c; j++) {
		v = a[i * c + j]
		v	/* Print v and a newline. */
		if (v == 20) {
			b = 1
			break
		}
	}
	if (b) break
	/* Print "==" and a newline. */
	"==
"
}
quit
```



## C

Using goto (note: gotos are [http://en.wikipedia.org/wiki/Considered_harmful considered harmful]):

```cpp
#include <iostream>
#include <time.h>
#include <stdio.h>

int main() {
    int a[10][10], i, j;

    srand(time(NULL));
    for (i = 0; i < 10; i++)
        for (j = 0; j < 10; j++)
            a[i][j] = rand() % 20 + 1;

    for (i = 0; i < 10; i++) {
        for (j = 0; j < 10; j++) {
            printf(" %d", a[i][j]);
            if (a[i][j] == 20)
                goto Done;
        }
        printf("\n");
    }
Done:
    printf("\n");
    return 0;
}
```



## C++

Lambda call:
{{works with|C++11}}

```cpp
#include <cstdlib>
#include<ctime>
#include<iostream>

using namespace std;
int main()
{
    int arr[10][10];
    srand(time(NULL));
    for(auto& row: arr)
        for(auto& col: row)
            col = rand() % 20 + 1;

    ([&](){
       for(auto& row : arr)
           for(auto& col: row)
           {
               cout << col << endl;
               if(col == 20)return;
           }
    })();
    return 0;
}
```

Goto statement:
{{works with|C++11}}

```cpp
#include <cstdlib>
#include <ctime>
#include <iostream>

using namespace std;
int main()
{
    int arr[10][10];
    srand(time(NULL));
    for(auto& row: arr)
        for(auto& col: row)
            col = rand() % 20 + 1;

    for(auto& row : arr) {
        for(auto& col: row) {
            cout << ' ' << col;
            if (col == 20) goto out;
        }
        cout << endl;
    }
    out:

    return 0;
}
```


## C#
Uses goto as C# has no way to break from multiple loops

```c#
using System;

class Program {
    static void Main(string[] args) {
        int[,] a = new int[10, 10];
        Random r = new Random();

        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j++) {
                a[i, j] = r.Next(0, 21) + 1;
            }
        }

        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j++) {
                Console.Write(" {0}", a[i, j]);
                if (a[i, j] == 20) {
                    goto Done;
                }
            }
            Console.WriteLine();
        }
    Done:
        Console.WriteLine();
    }
}
```



Same using Linq :

```c#
using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main(string[] args) {
            int[,] a = new int[10, 10];
            Random r = new Random();

            // prepare linq statement with two 'from' which makes nested loop
            var pairs = from i in Enumerable.Range(0, 10)
                        from j in Enumerable.Range(0, 10)
                        select new { i = i, j = j};

            // iterates through the full nested loop with a sigle foreach statement
            foreach (var p in pairs)
            {
                a[p.i, p.j] = r.Next(0, 21) + 1;
            }

            // iterates through the nested loop until find element = 20
            pairs.Any(p => { Console.Write(" {0}", a[p.i, p.j]); return a[p.i, p.j] == 20; });
            Console.WriteLine();
    }
}
```



## Chapel


```chapel
use Random;

var nums:[1..10, 1..10] int;
var rnd = new RandomStream();

[ n in nums ] n = floor(rnd.getNext() * 21):int;
delete rnd;

// this shows a clumsy explicit way of iterating, to actually create nested loops:
label outer for i in nums.domain.dim(1) {
        for j in nums.domain.dim(2) {
                write(" ", nums(i,j));
                if nums(i,j) == 20 then break outer;
        }
        writeln();
}
```



## Clojure

We explicitly return a status flag from the inner loop:

```clojure
(ns nested)

(defn create-matrix [width height]
  (for [_ (range width)]
    (for [_ (range height)]
      (inc (rand-int 20)))))

(defn print-matrix [matrix]
  (loop [[row & rs] matrix]
    (when (= (loop [[x & xs] row]
               (println x)
               (cond (= x 20) :stop
                     xs (recur xs)
                     :else :continue))
             :continue)
      (when rs (recur rs)))))

(print-matrix (create-matrix 10 10))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested-Loop.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       78  Table-Size VALUE 10.
       01  Table-Area.
           03  Table-Row OCCURS Table-Size TIMES
                   INDEXED BY Row-Index.
               05  Table-Element PIC 99 OCCURS Table-Size TIMES
                   INDEXED BY Col-Index.

       01  Current-Time PIC 9(8).
       PROCEDURE DIVISION.
*          *> Seed RANDOM.
           ACCEPT Current-Time FROM TIME
           MOVE FUNCTION RANDOM(Current-Time) TO Current-Time

*          *> Put random numbers in the table.
*          *> The AFTER clause is equivalent to a nested PERFORM VARYING
*          *> statement.
           PERFORM VARYING Row-Index FROM 1 BY 1
                       UNTIL Table-Size < Row-Index
                   AFTER Col-Index FROM 1 BY 1
                       UNTIL Table-Size < Col-Index
               COMPUTE Table-Element (Row-Index, Col-Index) =
                   FUNCTION MOD((FUNCTION RANDOM * 1000), 20) + 1
           END-PERFORM

*          *> Search through table for 20.
*          *> Using proper nested loops.
           PERFORM VARYING Row-Index FROM 1 BY 1
                   UNTIL Table-Size < Row-Index
               PERFORM VARYING Col-Index FROM 1 BY 1
                       UNTIL Table-Size < Col-Index
                   IF Table-Element (Row-Index, Col-Index) = 20
                       EXIT PERFORM
                   ELSE
                       DISPLAY Table-Element (Row-Index, Col-Index)
                   END-IF
               END-PERFORM
           END-PERFORM

           GOBACK
           .
```



## ColdFusion


```cfm

<Cfset RandNum = 0>
<Cfloop condition="randNum neq 20">
  <Cfloop from="1" to="5" index="i">
    <Cfset randNum = RandRange(1, 20)>
    <Cfoutput>#randNum# </Cfoutput>
    <Cfif RandNum eq 20><cfbreak></Cfif>
  </Cfloop>


</Cfloop>

```



## Common Lisp


```lisp
(let ((a (make-array '(10 10))))
  (dotimes (i 10)
    (dotimes (j 10)
      (setf (aref a i j) (1+ (random 20)))))

  (block outer
    (dotimes (i 10)
      (dotimes (j 10)
        (princ " ")
        (princ (aref a i j))
        (if (= 20 (aref a i j))
            (return-from outer)))
      (terpri))
    (terpri)))
```



## D


```d
import std.stdio, std.random;

void main() {
    int[10][10] mat;
    foreach (ref row; mat)
        foreach (ref item; row)
            item = uniform(1, 21);

    outer:
    foreach (row; mat)
        foreach (item; row) {
            write(item, ' ');
            if (item == 20)
                break outer;
        }

    writeln();
}
```


=={{header|Delphi}}/{{header|Pascal}}==

```delphi
var
  matrix: array[1..10,1..10] of Integer;
  row, col: Integer;
  broken: Boolean;
begin
  // Launch random number generator
  randomize;
  // Filling matrix with random numbers
  for row := 1 to 10 do
    for col := 1 to 10 do
      matrix[row, col] := Succ(Random(20));
  // Displaying values one by one, until at the end or reached number 20
  Broken := False;
  for row := 1 to 10 do
  begin
    for col := 1 to 10 do
    begin
      ShowMessage(IntToStr(matrix[row, col]));
      if matrix[row, col] = 20 then
      begin
        Broken := True;
        break;
      end;
    end;
    if Broken then break;
  end;
end;
```



## dc

A single ''Q'' command can break multiple nested loops.
{{trans|bc}}

```dc
1 ss  [Seed of the random number generator.]sz

[*
 * lrx -- (number)
 * Push a random number from 1 to 20.
 *]sz
[
 [                [If preventing modulo bias:]sz
  sz               [Drop this random number.]sz
  lLx              [Loop.]sz
 ]SI
 [                [Loop:]sz
  [*
   * Formula (from POSIX) for random numbers of low quality.
   * Push a random number from 0 to 32767.
   *]sz
  ls 1103515245 * 12345 + 4294967296 % ss
  ls 65536 / 32768 %

  d 32768 20 % >I  [Prevent modulo bias.]sz
 ]d SL x
 20 % 1 +         [Be from 1 to 20.]sz
 LLsz LIsz        [Restore L, I.]sz
]sr


5 sb           [b = Total rows]sz
5 sc           [c = Total columns]sz

[Fill array a[] with random numbers from 1 to 20.]sz
[              [Inner loop for j:]sz
 lrx            [Push random number.]sz
 li lc * lj +   [Push index of a[i, j].]sz
 :a             [Put in a[].]sz
 lj 1 + d sj    [j += 1]sz
 lc >I          [Loop while c > j.]sz
]sI
[              [Outer loop for i:]sz
 0 d sj         [j = 0]sz
 lc >I          [Enter inner loop.]sz
 li 1 + d si    [i += 1]sz
 lb >L          [Loop while b > i.]sz
]sL
0 d si         [i = 0]sz
lb >L          [Enter outer loop.]sz

[Find a 20.]sz
[              [If detecting a 20:]sz
 li lj + 3 + Q  [Break outer loop.]sz
]sD
[              [Inner loop for j:]sz
 li lc * lj +   [Push index of a[i,j].]sz
 ;a             [Push value from a[].]sz
 p              [Print value and a newline.]sz
 20 =D          [Detect a 20.]sz
 lj 1 + d sj    [j += 1]sz
 lc >I          [Loop while c > j.]sz
]sI
[              [Outer loop for i:]sz
 0 d sj         [j = 0]sz
 lc >I          [Enter inner loop.]sz
 [==
]P              [Print "==" and a newline.]sz
 li 1 + d si    [i += 1]sz
 lb >L          [Loop while b > i.]sz
]sL
0 d si         [i = 0]sz
lb >L          [Enter outer loop.]sz
```

In this program, ''li lj + 3 + Q'' breaks both the inner loop and the outer loop. We must count how many levels of string execution to break. Our loops use tail recursion, so each iteration is a level of string execution. We have i + 1 calls to outer loop L, and j + 1 calls to inner loop I, and 1 call to condition D; so we break i + j + 3 levels with ''li lj + 3 + Q''.


## Dyalect


There is no direct way to break out of a nested loop in Dyalect, <code>goto</code> is also not supported, however the desired effect can be achieved by placing a nested loop in an expression context and make it return <code>true</code> if we need to break out of the parent loop:


```dyalect
const array = [[2, 12, 10, 4], [18, 11, 20, 2]]

for row in array {
    if {
        for element in row {
            print("\(element)")
            if element == 20 {
                break true
            }
        }
    } {
        break
    }
}
print("*Done")
```


{{out}}


```txt
2
12
10
4
18
11
20
*Done
```



## E


```e
def array := accum [] for i in 1..5 { _.with(accum [] for i in 1..5 { _.with(entropy.nextInt(20) + 1) }) }

escape done {
    for row in array {
        for x in row {
            print(`$x$\t`)
            if (x == 20) {
                done()
            }
        }
        println()
    }
}
println("done.")
```



## EchoLisp


```lisp

(lib 'math) ;; for 2D-arrays
(define array (build-array 42 42 (lambda(i j) (1+ (random 20)))))
 → array

;;
(for* ((row array) (aij row)) (write aij) #:break (= aij 20))
  → 9 8 11 1 14 11 1 9 16 1 10 5 5 6 5 4 13 17 14 13 6 10 16 4 8 5 1 17 16 19 4 6 18 1 15 3 4 13 19
  6 12 5 5 17 19 16 3 7 2 15 16 14 16 16 19 18 14 16 6 18 14 17 20

```



## Elixir

{{works with|Elixir|1.2}}

```elixir
defmodule Loops do
  def nested do
    list = Enum.shuffle(1..20) |> Enum.chunk(5)
    IO.inspect list, char_lists: :as_lists
    try do
      nested(list)
    catch
      :find -> IO.puts "done"
    end
  end

  def nested(list) do
    Enum.each(list, fn row ->
      Enum.each(row, fn x ->
        IO.write "#{x} "
        if x == 20, do: throw(:find)
      end)
      IO.puts ""
    end)
  end
end

Loops.nested
```


{{out|Sample output}}

```txt

[[3, 11, 4, 15, 18], [8, 7, 12, 17, 9], [6, 20, 14, 1, 16], [2, 5, 10, 19, 13]]
3 11 4 15 18
8 7 12 17 9
6 20 done

```


'''used Enum.any?'''

```elixir
list = Enum.shuffle(1..20) |> Enum.chunk(5)
IO.inspect list, char_lists: :as_lists
Enum.any?(list, fn row ->
  IO.puts ""
  Enum.any?(row, fn x ->
    IO.write "#{x} "
    x == 20
  end)
end)
IO.puts "done"
```


{{out|Sample output}}

```txt

[[17, 15, 18, 14, 16], [5, 11, 10, 4, 2], [8, 20, 7, 19, 1], [6, 9, 3, 12, 13]]

17 15 18 14 16
5 11 10 4 2
8 20 done

```



## Erlang


```Erlang

-module( loops_nested ).

-export( [task/0] ).

task() ->
       Size = 20,
       Two_dimensional_array = [random_array(Size) || _X <- lists:seq(1, Size)],
       print_until_found( [], 20, Two_dimensional_array ).



print_until_found( [], N, [Row | T] ) -> print_until_found( print_until_found_row(N, Row), N, T );
print_until_found( _Found, _N, _Two_dimensional_array ) -> io:fwrite( "~n" ).

print_until_found_row( _N, [] ) -> [];
print_until_found_row( N, [N | T] ) -> [N | T];
print_until_found_row( N, [H | T] ) ->
        io:fwrite( "~p ", [H] ),
        print_until_found_row( N, T ).

random_array( Size ) -> [random:uniform(Size) || _X <- lists:seq(1, Size)].

```



## ERRE


```ERRE

DIM A%[10,10]                      ! in declaration part
.............
PRINT(CHR$(12);) !CLS
FOR ROW=1 TO 10 DO
   FOR COL=1 TO 10 DO
     A%[ROW,COL]=INT(RND(1)*20)+1  ! INT and RND are ERRE predeclared functions
                                   ! RND generates random numbers between 0 and 1
   END FOR
END FOR

FOR ROW=1 TO 10 DO
   FOR COL=1 TO 10 DO
     PRINT(A%[ROW,COL])
     EXIT IF A%[ROW,COL]=20
   END FOR
   EXIT IF A%[ROW,COL]=20  ! EXIT breaks the current loop only: you must repeat it,
                           ! use a boolean variable or a GOTO label statement
END FOR

```



## Euphoria


```euphoria
sequence a
a = rand(repeat(repeat(20, 10), 10))

integer wantExit
wantExit = 0

for i = 1 to 10 do
    for j = 1 to 10 do
	printf(1, "%g ", {a[i][j]})
	if a[i][j] = 20 then
	    wantExit = 1
	    exit
	end if
    end for
    if wantExit then
	exit
    end if
end for
```

<code>exit</code> only breaks out of the innermost loop.  A better way to do this would be a procedure.


## F#


```fsharp

//Nigel Galloway: November 10th., 2017
let n = System.Random()
let g = Array2D.init 8 8 (fun _ _ -> 1+n.Next()%20)
Array2D.iter (fun n -> printf "%d " n) g; printfn ""
g |> Seq.cast<int> |> Seq.takeWhile(fun n->n<20) |> Seq.iter (fun n -> printf "%d " n)

```

{{out}}

```txt

3 7 5 8 7 5 12 14 6 10 7 8 4 8 10 2 12 16 9 19 14 10 1 1 14 2 8 18 1 1 6 19 5 16 15 16 11 19 19 17 3 9 9 15 14 12 20 18 14 8 5 12 20 14 5 14 7 5 15 13 5 15 14 13
3 7 5 8 7 5 12 14 6 10 7 8 4 8 10 2 12 16 9 19 14 10 1 1 14 2 8 18 1 1 6 19 5 16 15 16 11 19 19 17 3 9 9 15 14 12

```



## Factor

Calling <code>return</code> from inside a <code>with-return</code> quotation allows one to break out of the quotation.

```factor
USING: continuations formatting io kernel math.ranges
prettyprint random sequences ;
IN: rosetta-code.loops-nested

: rand-table ( -- seq )
    10 [ 20 [ 20 [1,b] random ] replicate ] replicate ;

rand-table [
    [ [ dup "%4d" printf 20 = [ return ] when ] each nl ] each
] with-return drop
```

{{out}}

```txt

  13   6   7   6  14   6   8  15  15  12  15  13  18  17  19   9  17  18  19   9
   1   2  15   5   9  13   8  17   2   3  19  15   4   8  18   4   5   3   2   1
  18  12   2   9  20

```



## Fantom

There is no specific way to break out of nested loops (such as a labelled break, or goto).  Instead, we can use exceptions and a try-catch block.

```fantom
class Main
{
  public static Void main ()
  {
    rows := 10
    cols := 10
    // create and fill an array of given size with random numbers
    Int[][] array := [,]
    rows.times
    {
      row := [,]
      cols.times { row.add(Int.random(1..20)) }
      array.add (row)
    }
    // now do the search
    try
    {
      for (i := 0; i < rows; i++)
      {
        for (j := 0; j < cols; j++)
        {
          echo ("now at ($i, $j) which is ${array[i][j]}")
          if (array[i][j] == 20) throw (Err("found it"))
        }
      }
    }
    catch (Err e)
    {
      echo (e.msg)
      return // and finish
    }
    echo ("No 20")
  }
}
```



## Forth


```forth
include random.fs

10 constant X
10 constant Y

: ,randoms ( range n -- ) 0 do dup random 1+ , loop drop ;

create 2darray 20 X Y * ,randoms

: main
  Y 0 do
    cr
    X 0 do
      j X * i + cells 2darray + @
      dup .
      20 = if unloop unloop exit then
    loop
  loop ;
```



## Fortran

{{works with|Fortran|77 and later}}

```fortran
      PROGRAM LOOPNESTED
        INTEGER A, I, J, RNDINT

C       Build a two-dimensional twenty-by-twenty array.
        DIMENSION A(20,20)

C       It doesn't matter what number you put here.
        CALL SDRAND(123)

C       Fill the array with random numbers.
        DO 20 I = 1, 20
          DO 10 J = 1, 20
            A(I, J) = RNDINT(1, 20)
   10     CONTINUE
   20   CONTINUE

C       Print the numbers.
        DO 40 I = 1, 20
          DO 30 J = 1, 20
            WRITE (*,5000) I, J, A(I, J)

C           If this number is twenty, break out of both loops.
            IF (A(I, J) .EQ. 20) GOTO 50
   30     CONTINUE
   40   CONTINUE

C       If we had gone to 40, the DO loop would have continued. You can
C       label STOP instead of adding another CONTINUE, but it is good
C       form to only label CONTINUE statements as much as possible.
   50   CONTINUE
        STOP

C       Print the value so that it looks like one of those C arrays that
C       makes everybody so comfortable.
 5000   FORMAT('A[', I2, '][', I2, '] is ', I2)
      END

C FORTRAN 77 does not come with a random number generator, but it is
C easy enough to type "fortran 77 random number generator" into your
C preferred search engine and to copy and paste what you find.
C The following code is a slightly-modified version of:
C
C     http://www.tat.physik.uni-tuebingen.de/
C         ~kley/lehre/ftn77/tutorial/subprograms.html
      SUBROUTINE SDRAND (IRSEED)
        COMMON  /SEED/ UTSEED, IRFRST
        UTSEED = IRSEED
        IRFRST = 0
        RETURN
      END
      INTEGER FUNCTION RNDINT (IFROM, ITO)
        INTEGER IFROM, ITO
        PARAMETER (MPLIER=16807, MODLUS=2147483647,                     &
     &              MOBYMP=127773, MOMDMP=2836)
        COMMON  /SEED/ UTSEED, IRFRST
        INTEGER HVLUE, LVLUE, TESTV, NEXTN
        SAVE    NEXTN
        IF (IRFRST .EQ. 0) THEN
          NEXTN = UTSEED
          IRFRST = 1
        ENDIF
        HVLUE = NEXTN / MOBYMP
        LVLUE = MOD(NEXTN, MOBYMP)
        TESTV = MPLIER*LVLUE - MOMDMP*HVLUE
        IF (TESTV .GT. 0) THEN
          NEXTN = TESTV
        ELSE
          NEXTN = TESTV + MODLUS
        ENDIF
        IF (NEXTN .GE. 0) THEN
          RNDINT = MOD(MOD(NEXTN, MODLUS), ITO - IFROM + 1) + IFROM
        ELSE
          RNDINT = MOD(MOD(NEXTN, MODLUS), ITO - IFROM + 1) + ITO + 1
        ENDIF
        RETURN
      END
```

{{out|Sample output}}

```txt
A[ 1][ 1] is  2
A[ 1][ 2] is 16
A[ 1][ 3] is 16
A[ 1][ 4] is  3
A[ 1][ 5] is 16
A[ 1][ 6] is 15
A[ 1][ 7] is 18
A[ 1][ 8] is 14
A[ 1][ 9] is  9
A[ 1][10] is 10
A[ 1][11] is 12
A[ 1][12] is 15
A[ 1][13] is  3
A[ 1][14] is 19
A[ 1][15] is 20
```

{{works with|Fortran|90 and later}}
Here the special feature is that later Fortran allows loops to be labelled (with "outer" in this example) on their first and last statements. Any EXIT or CYCLE statements can then mention the appropriate label so as to be clear just which loop is involved, otherwise the assumption is the innermost loop only. And no "GO TO" statements need appear.

```fortran
program Example
  implicit none

  real :: ra(5,10)
  integer :: ia(5,10)
  integer :: i, j

  call random_number(ra)
  ia = int(ra * 20.0) + 1

outer: do i = 1, size(ia, 1)
         do j = 1, size(ia, 2)
           write(*, "(i3)", advance="no") ia(i,j)
           if (ia(i,j) == 20) exit outer
         end do
         write(*,*)
       end do outer

end program Example
```

{{out|Sample output}}

```txt

 14  2  1 11  8  1 14 11  3 15
  7 15 16  6  7 17  3 20

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Randomize
Dim a(1 To 20, 1 To 20) As Integer
For i As Integer = 1 To 20
  For j As Integer = 1 To 20
    a(i, j) = Int(Rnd * 20) + 1
  Next j
Next i

For i As Integer = 1 To 20
  For j As Integer = 1 To 20
    Print Using "##"; a(i, j);
    Print " ";
    If a(i, j) = 20 Then Exit For, For '' Exits both for loops
  Next j
  Print
Next i

Print
Print "Press any key to quit"
Sleep
```


Sample output :

{{out}}

```txt

13  3 16 13 16 11 15 19 10  5 12  7 17  1  6 11  2 19 11 11
12 17 20

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=77521a9ffe6bebdfe2e34df8faab5e78 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siArray As New Short[5, 5]
Dim siCount0, siCount1 As Short
Dim bBreak As Boolean

For siCount0 = 0 To 4
  For siCount1 = 0 To 4
    siArray[siCount0, siCount1] = Rand(1, 20)
    siArray[siCount0, siCount1] = Rand(1, 20)
  Next
Next

For siCount0 = 0 To 4
  For siCount1 = 0 To 4
    If siArray[siCount0, siCount1] = 20 Then
      bBreak = True
      Break
    Endif
  Next
  If bBreak Then Break
Next

Print "Row " & Str(siCount0) & " column " & Str(siCount1) & " = 20"

End
```

Output:

```txt

Row 5 column 4 = 20

```



## GAP


```gap
# You can't break an outer loop unless you return from the whole function.
n := 40;
a := List([1 .. n], i -> List([1 .. n], j -> Random(1, 20)));;

Find := function(a, x)
    local i, j, n;
    n := Length(a);
    for i in [1 .. n] do
        for j in [1 .. n] do
            if a[i][j] = x then
                return [i, j];
            fi;
        od;
    od;
    return fail;
end;

Find(a, 20);
```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano())

    values := make([][]int, 10)
    for i := range values {
        values[i] = make([]int, 10)
        for j := range values[i] {
            values[i][j] = rand.Intn(20) + 1
        }
    }

outerLoop:
    for i, row := range values {
        fmt.Printf("%3d)", i)
        for _, value := range row {
            fmt.Printf(" %3d", value)
            if value == 20 {
                break outerLoop
            }
        }
        fmt.Printf("\n")
    }
    fmt.Printf("\n")
}
```



## Groovy

{{Trans|Java}}
Solution:

```groovy
final random = new Random()
def a = []
(0..<10).each {
    def row = []
    (0..<10).each {
        row << (random.nextInt(20) + 1)
    }
    a << row
}

a.each { println it }
println ()

Outer:
for (i in (0..<a.size())) {
    for (j in (0..<a[i].size())) {
        if (a[i][j] == 20){
            println ([i:i, j:j])
            break Outer
        }
    }
}
```

{{out}}

```txt
[1, 19, 14, 16, 3, 12, 14, 18, 12, 6]
[6, 3, 8, 9, 17, 4, 10, 15, 17, 17]
[5, 12, 13, 1, 8, 18, 8, 15, 3, 20]
[8, 9, 6, 7, 2, 20, 17, 13, 6, 16]
[18, 6, 11, 13, 16, 20, 7, 3, 1, 14]
[6, 6, 19, 9, 9, 7, 16, 16, 3, 20]
[7, 6, 12, 7, 16, 14, 13, 18, 15, 15]
[19, 14, 14, 6, 4, 19, 5, 10, 13, 12]
[7, 6, 6, 12, 3, 9, 17, 12, 20, 7]
[10, 7, 15, 4, 17, 13, 14, 16, 8, 8]

[i:2, j:9]
```



## Haskell


```haskell
import Data.List
breakIncl :: (a -> Bool) -> [a] -> [a]
breakIncl p =  uncurry ((. take 1). (++)). break p

taskLLB k = map (breakIncl (==k)). breakIncl (k `elem`)
```

{{out|Example}}

```haskell

*Main> mapM_ (mapM_ print) $ taskLLB 20 [[2,6,17,5,14],[1,9,11,18,10],[13,20,8,7,4],[16,15,19,3,12]]
2
6
17
5
14
1
9
11
18
10
13
20
```



## HicEst


```hicest
REAL :: n=20, array(n,n)

array = NINT( RAN(10,10) )

DO row = 1, n
  DO col = 1, n
    WRITE(Name) row, col, array(row,col)
    IF( array(row, col) == 20 ) GOTO 99
  ENDDO
ENDDO

99 END
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon use 'break' to exit loops and execute an expression argument.  To exit nested loops 'break' is repeated as the expression.

```Icon
procedure main()

every !(!(L  := list(10)) := list(10))  := ?20   # setup a 2d array of random numbers up to 20

every i := 1 to *L do                            # using nested loops
   every j := 1 to *L[i] do
      if L[i,j] = 20 then
         break break write("L[",i,",",j,"]=20")

end
```


```Icon
every x := L[i := 1 to *L,1 to *L[i]] do
    if x = 20 then break write("L[",i,",",j,"]=20")  # more succinctly

every if !!L = 20 then break write("Found !!L=20")   # even more so (but looses the values of i and j
```



## J

In J, using loops is usually a bad idea.

Here's how the problem statement (ignoring the "requirement" for loops) could be solved, without loops:

```J
use=: ({.~ # <. 1+i.&20)@:,
```

Here's how the problem could be solved, using loops:

```J
doubleLoop=:verb define
  for_row.i.#y do.
    for_col.i.1{$y do.
      smoutput t=.(<row,col) { y
      if.20=t do.''return.end.
    end.
  end.
)
```

{{out|Example use}}

```txt
   use ?.20 20 $ 21
6 17 13 3 5 16 10 4 20
   doubleLoop ?.20 20 $ 21
6
17
13
3
5
16
10
4
20

```

The first approach is probably a couple thousand times faster than the second.

(In real life, good problem definitions might typically involve "use cases" (which are specified in terms of the problem domain, instead in terms of irrelevant details).  Of course "Rosetta Code" is about how concepts would be expressed in different languages.  However, even here, tasks which dwell on language-specific issues are probably not a good use of people's time.)


## Java


```java
import java.util.Random;

public class NestedLoopTest {
    public static final Random gen = new Random();
    public static void main(String[] args) {
        int[][] a = new int[10][10];
        for (int i = 0; i < a.length; i++)
            for (int j = 0; j < a[i].length; j++)
                a[i][j] = gen.nextInt(20) + 1;

        Outer:for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < a[i].length; j++) {
                System.out.print(" " + a[i][j]);
                if (a[i][j] == 20)
                    break Outer; //adding a label breaks out of all loops up to and including the labelled loop
            }
            System.out.println();
        }
        System.out.println();
    }
}
```



## JavaScript

Demonstrates use of <code>break</code> with a label.
Uses <code>print()</code> function from [[Rhino]].

```javascript
// a "random" 2-D array
var a = [[2, 12, 10, 4], [18, 11, 9, 3], [14, 15, 7, 17], [6, 19, 8, 13], [1, 20, 16, 5]];

outer_loop:
for (var i in a) {
    print("row " + i);
    for (var j in a[i]) {
        print(" " + a[i][j]);
        if (a[i][j] == 20)
            break outer_loop;
    }
}
print("done");
```


In a functional idiom of JavaScript, however, we can not use a loop statement, as statements return no value and can not be composed within other functional expressions. Functional JavaScript often replaces a loop with a map or fold. In this case, we can achieve the same task by defining the standard list-processing function '''takeWhile''', which terminates when a condition returns true.

We can then search the groups in the nested array by nesting takeWhile inside itself, and finally terminate when the 20 is found by one further application of takeWhile.

Using the same data as above, and returning the trail of numbers up to twenty from a nested and composable expression:


```JavaScript
var lst = [[2, 12, 10, 4], [18, 11, 9, 3], [14, 15, 7, 17], [6, 19, 8, 13], [1,
  20, 16, 5]];

var takeWhile = function (lst, fnTest) {
    'use strict';
    var varHead = lst.length ? lst[0] : null;

    return varHead ? (
      fnTest(varHead) ? [varHead].concat(
        takeWhile(lst.slice(1), fnTest)
      ) : []
    ) : []
  },

  // The takeWhile function terminates when notTwenty(n) returns false
  notTwenty = function (n) {
    return n !== 20;
  },

  // Leftward groups containing no 20
  // takeWhile nested within takeWhile
  lstChecked = takeWhile(lst, function (group) {
    return takeWhile(
      group,
      notTwenty
    ).length === 4;
  });


// Return the trail of numbers preceding 20 from a composable expression

console.log(
  // Numbers before 20 in a group in which it was found
  lstChecked.concat(
    takeWhile(
      lst[lstChecked.length], notTwenty
    )
  )
  // flattened
  .reduce(function (a, x) {
    return a.concat(x);
  }).join('\n')
);
```


Output:

```JavaScript
2
12
10
4
18
11
9
3
14
15
7
17
6
19
8
13
6
19
8
13
1
```



## jq


jq has a `break` statement for breaking out of nested loops,
and in this entry, it is used in the following function:


```jq
# Given an m x n matrix,
# produce a stream of the matrix elements (taken row-wise)
# up to but excluding the first occurrence of $max
def stream($max):
  . as $matrix
  | length as $m
  | (.[0] | length) as $n
  | label $ok
  | {i: range(0;$m), j: range(0;$n)}
  | $matrix[.i][.j] as $m
  | if $m == $max then break $ok else $m end ;
```


The nesting above could be made more visually explicit, for example, by using
the equivalent form:

    range(0;$m) as $i
    | range(0;$n) as $j

but the previous formulation illustrates a concise alternative.

To generate the random matrix, and to accomplish the "pretty-printing"
component of the task, the following function for converting a stream
to an array of arrays is useful:


```jq
# Create an array of arrays by using the items in the stream, s,
# to create successive rows, each row having at most n items.
def reshape(s; n):
  reduce s as $s ({i:0, j:0, matrix: []};
    .matrix[.i][.j] = $s
    | if .j + 1 == n then .i += 1 | .j = 0
      else .j += 1
      end)
  | .matrix;
```


Assuming the availability of rand/1 (e.g. as defined below),
we can now readily define functions to create the matrix and pretty-print the
items as required:

```jq
# Create an m x n matrix filled with numbers in [1 .. max]
def randomMatrix(m; n; max):
  reshape(limit(m * n; rand(max) + 1); n);

# Present the matrix up to but excluding the first occurrence of $max
def show($m; $n; $max):
  reshape( randomMatrix($m; $n; $max) | stream($max); $n)[] ;

# Main program for the problem at hand.
show(20; 4; 20)
```


{{out}}

Assuming proper placement of PRNG functions as defined below, the following invocation:

    $ jq -cn -f program.jq --arg seed 17

produces:


```txt
[1,17,19,12]
[13,8,18,10]
[18,15,3,18]
[11,12,3,10]
[4,8,1,14]
[12,1,10,9]
[3,16,19,13]
[10,12,13]
```



''' PRNG '''

```jq
# LCG::Microsoft generates 15-bit integers using the same formula
# as rand() from the Microsoft C Runtime.
# Input: [ count, state, random ]
def next_rand_Microsoft:
  .[0] as $count
  | ((214013 * .[1]) + 2531011) % 2147483648 # mod 2^31
  | [$count+1 , ., (. / 65536 | floor) ];

def rand_Microsoft(seed):
  [0,seed]
  | next_rand_Microsoft  # the seed is not so random
  | recurse( next_rand_Microsoft )
  | .[2];

# A random integer in [0 ... (n-1)]:
# rand_Microsoft returns an integer in 0 .. 32767
def rand(n): n * (rand_Microsoft($seed|tonumber) / 32768) | trunc;
```



## Jsish


```javascript
/* Loops/Nested in Jsish */
Math.srand(0);
var nrows = Math.floor(Math.random() * 4) + 4;
var ncols = Math.floor(Math.random() * 6) + 6;

var matrix = new Array(nrows).fill(0).map(function(v, i, a):array { return new Array(ncols).fill(0); } );

var i,j;
for (i = 0; i < nrows; i++) for (j = 0; j < ncols; j++) matrix[i][j] = Math.floor(Math.random() * 20) + 1;

/* Labelled break point */
outer_loop:
for (i in matrix) {
    printf("row %d:", i);
    for (j in matrix[i]) {
        printf(" %d", matrix[i][j]);
        if (matrix[i][j] == 20) {
            printf("\n");
            break outer_loop;
        }
    }
    printf("\n");
}
puts(matrix);

/*
=!EXPECTSTART!=
row 0: 2 18 12 16 14 8 18 15 9 8
row 1: 15 6 8 16 17 12 15 2 10 3
row 2: 11 8 12 20
[ [ 2, 18, 12, 16, 14, 8, 18, 15, 9, 8 ],
  [ 15, 6, 8, 16, 17, 12, 15, 2, 10, 3 ],
  [ 11, 8, 12, 20, 18, 4, 6, 6, 19, 9 ],
  [ 16, 3, 2, 19, 1, 4, 8, 4, 11, 18 ] ]
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u loopsNested.jsi
[PASS] loopsNested.jsi
```



## Julia


```Julia

M = [rand(1:20) for i in 1:5, j in 1:10]
R, C = size(M)

println("The full matrix is:")
println(M, "\n")

println("Find the first 20:")
for i in 1:R, j in 1:C
    n = M[i,j]
    @printf "%4d" n
    if n == 20
        println()
        break
    elseif j == C
        println()
    end
end

```

{{out}}

```txt

The full matrix is:
[9 17 10 8 6 10 13 12 7 12
 15 14 13 7 8 12 15 2 12 1
 8 3 4 14 19 1 3 13 11 15
 19 16 18 2 9 3 4 17 16 10
 16 4 20 19 8 1 18 14 12 4]

Find the first 20:
   9  17  10   8   6  10  13  12   7  12
  15  14  13   7   8  12  15   2  12   1
   8   3   4  14  19   1   3  13  11  15
  19  16  18   2   9   3   4  17  16  10
  16   4  20

```

Julia is column ordered, but this program searches in row order to be consistent with the other solutions of this task.


## Kotlin


```scala
import java.util.Random

fun main(args: Array<String>) {
    val r = Random()
    val a = Array(10) { IntArray(10) { r.nextInt(20) + 1 } }
    println("array:")
    for (i in a.indices) println("row $i: " + a[i].asList())

    println("search:")
    Outer@ for (i in a.indices) {
        print("row $i: ")
        for (j in a[i].indices) {
            print(" " + a[i][j])
            if (a[i][j] == 20) break@Outer
        }
        println()
    }
    println()
}
```

{{out}}

```txt
array:
row 0: [10, 8, 19, 17, 19, 7, 13, 16, 16, 4]
row 1: [6, 2, 6, 1, 11, 10, 2, 8, 1, 14]
row 2: [3, 6, 4, 6, 10, 2, 10, 20, 18, 1]
row 3: [16, 14, 6, 13, 18, 8, 18, 7, 4, 18]
row 4: [14, 10, 13, 11, 2, 17, 16, 19, 1, 1]
row 5: [4, 20, 6, 17, 20, 12, 20, 15, 16, 15]
row 6: [2, 20, 6, 5, 5, 15, 1, 2, 6, 18]
row 7: [14, 6, 8, 10, 12, 8, 12, 3, 14, 10]
row 8: [1, 5, 15, 12, 7, 14, 9, 7, 16, 11]
row 9: [20, 16, 5, 13, 15, 9, 3, 2, 2, 16]
search:
row 0:  10 8 19 17 19 7 13 16 16 4
row 1:  6 2 6 1 11 10 2 8 1 14
row 2:  3 6 4 6 10 2 10 20
```



## Lasso



```Lasso
local(a) = array(
    array(2, 12, 10, 4),
    array(18, 11, 9, 3),
    array(14, 15, 7, 17),
    array(6, 19, 8, 13),
    array(1, 20, 16, 5)
)

// Query expression
with i in delve(#a) do {
    stdoutnl(#i)
    #i == 20 ? return
}

// Nested loops
#a->foreach => {
    #1->foreach => {
        stdoutnl(#1)
        #1 == 20 ? return
    }
}
```



## Liberty BASIC


```lb
dim ar(10,10)
for i = 1 to 10
    for j = 1 to 10
        ar(i, j) = int(rnd(1) * 20) + 1
    next
next

flag=0
for x = 1 to 10
    for y = 1 to 10
        print ar(x,y)
        if ar(x,y) = 20 then
            flag=1
            exit for
        end if
    next
    if flag then exit for
next
print "Completed row ";x;" and column ";y
```



## Lingo


```lingo
-- create two-dimensional array with random numbers
a = []
repeat with i = 1 to 20
  a[i] = []
  repeat with j = 1 to 20
    a[i][j] = random(20)
  end repeat
end repeat

-- iterate over rows and columns, print value, exit both loops if it's 20
repeat with i = 1 to 20
  repeat with j = 1 to 20
    v = a[i][j]
    put v
    if v=20 then exit repeat
  end repeat
  if v=20 then exit repeat
end repeat
```



## Lisaac


```Lisaac
Section Header

+ name := TEST_LOOP_NESTED;

- external := `#include <time.h>`;

Section Public

- main <- (
  + a : ARRAY2[INTEGER];
  + i, j: INTEGER;

  `srand(time(NULL))`;
  a := ARRAY2[INTEGER].create(0, 0) to (9, 9);
  0.to 9 do { ii : INTEGER;
    0.to 9 do { jj : INTEGER;
      a.put (`rand()`:INTEGER % 20 + 1) to (ii, jj);
    };
  };
  { i < 10 }.while_do {
    j := 0;
    { j < 10 }.while_do {
      ' '.print;
      a.item(i, j).print;
      (a.item(i, j) = 20).if {
        i := 999;
        j := 999;
      };
      j := j + 1;
    };
    i := i + 1;
    '\n'.print;
  };
  '\n'.print;
);
```



## LiveCode


```LiveCode
repeat with i = 1 to 10
    repeat with j = 1 to 10
        put random(20) into aNums[i,j]
    end repeat
end repeat

repeat with i = 1 to 10
    repeat with j = 1 to 10
        if aNums[i,j] = 20 then
            put true into exitLoop
            exit repeat
        end if
    end repeat
    if exitLoop then exit repeat
end repeat

if exitLoop then
    put "20 found in" && i & comma & j
else
    put "20 not found"
end if
```



## Logo


```logo
make "a mdarray [10 10]

for [j 1 10] [for [i 1 10] [mdsetitem list :i :j :a (1 + random 20)]]

to until.20
  for [j 1 10] [
    for [i 1 10] [
      type mditem list :i :j :a
      type "| |
      if equal? 20 mditem list :i :j :a [stop]
    ]
    print "||
  ]
end
until.20
```



## Lua


```lua
t = {}
for i = 1, 20 do
  t[i] = {}
  for j = 1, 20 do t[i][j] = math.random(20) end
end
function exitable()
  for i = 1, 20 do
    for j = 1, 20 do
      if t[i][j] == 20 then
        return i, j
      end
    end
  end
end
print(exitable())
```



## M2000 Interpreter

We can use a number as a label, so instead of using "then goto there" we can use "then 1000" if label is 1000.

No numeric labels may have only comments in same line.

Numeric labels may have 1 to 5 digits, including leading zeros. So 00010 is label 10. Numeric labels have no : after, but if we place one then this isn't fault, because : is a statement separator.

In this example we execute nested for two times, using a third for.



```M2000 Interpreter

Module Checkit {
            Dim A(10,10)<<Random(1, 20)
            For k=1 to 2 {
                  For i=0 to 9 {
                        For j=0 to 9 {
                              Print A(i,j)
                              if A(i,j)=20 then goto there
                        }
                  }
                  there:
                  Print "...ok", k
            }
}
Checkit

```




## Maple


```Maple
(m,n) := LinearAlgebra:-Dimensions(M):
for i from 1 to m do
  for j from 1 to n do
    print(M[i,j]);
    if M[i,j] = 20 then
      (i,j):=m,n; next;
    end if;
  end do;
end do:
```



## Mathematica


```Mathematica
Do[ Print[m[[i, j]]];
    If[m[[i, j]] === 20, Return[]],
  {i, 1, Dimensions[m][[1]]},
  {j, 1, Dimensions[m][[2]]}]
```


=={{header|MATLAB}} / {{header|Octave}}==
Loops are considered slow in Matlab and Octave, it is preferable to vectorize the code.

```Matlab
	a = ceil(rand(100,100)*20);
	[ix,iy]=find(a==20,1)
```

A non-vectorized version of the code is shown below in Octave


## Maxima


```maxima
data: apply(matrix, makelist(makelist(random(100), 20), 20))$

find_value(a, x) := block(
   [p, q],
   [p, q]: matrix_size(a),
   catch(
      for i thru p do
         for j thru q do
            if a[i, j] = x then throw([i, j]),
      'not\ found
   )
)$

find_value(data, 100);
not found
```



## MAXScript


```MAXScript

fn scan_Nested arr =
(
	for subArray in arr where classof subArray == Array do
	(
		for item in subArray do
		(
			print item as string
			if item == 20 do return OK
		)
	)
)

```


Example:

```MAXScript

testArray = #(#(1,5,2,19),#(11,20,7,2))
scan_nested testArray

#(#(1, 5, 2, 19), #(11, 20, 7, 2))
1
5
2
19
11
20
OK


```



## Microsoft Small Basic


```smallbasic
For row = 0 To 10
  For col = 0 To 10
    array[row][col] = Math.GetRandomNumber(20)
  EndFor
EndFor
For row = 0 To 10
  For col = 0 To 10
    TextWindow.WriteLine("row "+row+" col "+col+" value "+array[row][col])
    If array[row][col] = 20 Then
      Goto exit_for_row
    EndIf
  EndFor
EndFor
exit_for_row:
```

{{out}}

```txt
row 0 col 0 value 11
row 0 col 1 value 19
row 0 col 2 value 19
row 0 col 3 value 1
row 0 col 4 value 20
```



## MOO


```moo
a = make(10, make(10));
for i in [1..10]
  for j in [1..10]
    a[i][j] = random(20);
  endfor
endfor
for i in [1..10]
  s = "";
  for j in [1..10]
    s += tostr(" ", a[i][j]);
    if (a[i][j] == 20)
      break i;
    endif
  endfor
  player:tell(s);
  s = "";
endfor
player:tell(s);
```



## MUMPS


```MUMPS
NESTLOOP
 ;.../loops/nested
 ;set up the 2D array with random values
 NEW A,I,J,K,FLAG,TRIGGER
 SET K=15 ;Magic - just to give us a size to work with
 SET TRIGGER=20 ;Magic - the max value, and the end value
 FOR I=1:1:K FOR J=1:1:K SET A(I,J)=$RANDOM(TRIGGER)+1
 ;Now, search through the array, halting when the value of TRIGGER is found
 SET FLAG=0
 SET (I,J)=0
 FOR I=1:1:K Q:FLAG  W ! FOR J=1:1:K WRITE A(I,J),$SELECT(J'=K:", ",1:"") SET FLAG=(A(I,J)=TRIGGER) Q:FLAG
 KILL A,I,J,K,FLAG,TRIGGER
 QUIT
```

{{out}}

```txt
USER>D NESTLOOP^ROSETTA

16, 4, 6, 20,
USER>D NESTLOOP^ROSETTA

9, 10, 10, 13, 2, 9, 6, 10, 1, 12, 12, 10, 8, 1, 13
7, 14, 12, 9, 14, 3, 20,
```



## Neko


```ActionScript
/**
 Loops/Nested in Neko
 Tectonics:
   nekoc loops-nested.neko
   neko loops-nested.neko
*/

var random = $loader.loadprim("std@random_new", 0)();
var random_int = $loader.loadprim("std@random_int", 2);

var values = $amake(10);
var row = 0;
var col = 0;

while row < 10 {
  values[row] = $amake(10);
  col = 0;
  while col < 10 {
    values[row][col] = random_int(random, 20) + 1;
    col += 1;
  }
  row += 1;
}

/* Look for a 20 */
/*
 To break out of nested loops, (without using labels and $goto),
   Neko needs the value of the inner loop(s).
 The break statement sets the return value of a loop expression.
 Without a break, the value of a loop expression is unspecified.
*/
var inner;
row = 0;
while row < 10 {
  col = 0;
  inner = while col < 10 {
    $print("values[", row, "][", col, "] = ", values[row][col], "\n");
    if values[row][col] == 20 break true;
    col += 1;
  }
  if $istrue(inner) break;
  row += 1;
}
```


{{out}}

```txt
prompt$ nekoc loops-nested.neko
prompt$ neko loops-nested
values[0][0] = 17
values[0][1] = 1
values[0][2] = 8
values[0][3] = 5
values[0][4] = 18
values[0][5] = 17
values[0][6] = 17
values[0][7] = 19
values[0][8] = 2
values[0][9] = 1
values[1][0] = 11
values[1][1] = 4
values[1][2] = 16
values[1][3] = 11
values[1][4] = 12
values[1][5] = 20
```



## Nemerle

{{trans|C#}}
Nemerle can jump out of a named block by invoking the blocks name with an optional return value.

```Nemerle
using System;
using System.Console;
using Nemerle.Imperative;

module NestedLoops
{
    Main() : void
    {
        def arr = array(10, 10);
        def rnd = Random();

        foreach ((i, j) in $[(i, j) | i in [0 .. 9], j in [0 .. 9]])
            arr[i, j] = rnd.Next(1, 21);

        Finish:
        {
            foreach ((i, j) in $[(i, j) | i in [0 .. 9], j in [0 .. 9]])
            {
                Write("{0}  ", arr[i, j]);
                when (arr[i, j] == 20) Finish();
            }
        }
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

  say
  say 'Loops/Nested'

  rnd = Random()
  dim2 = int[10, 10]

  -- build sample data
  loop i1 = 0 for dim2.length
    loop i2 = 0 for dim2[i1].length
      dim2[i1, i2] = rnd.nextInt(20) + 1
      end i2
    end i1

  -- run test
  loop x1 = 0 for dim2.length
    say Rexx(x1 + 1).right(4)': \-'
    loop x2 = 0 for dim2[x1].length
      say Rexx(dim2[x1, x2]).right(3) || '\-'
      if dim2[x1, x2] = 20 then leave x1
      finally
        say
      end x2
    finally
      say
    end x1
```

I was somewhat disappointed by the performance of the above program
and started a little performance analysis on solutions of this task
for the languages I know.

I created a test program with a 500 x 500 matrix, all elements set to 0
except for the last one, which I set to 20.
Then I repeat the search 100 times.

The timings are:

```txt

 Seconds elapsed
 3.978      NetRexx as above
 0.032      Netrex with option binary
 7.223      ooRexx with x[i,j]
 6.490      ooRexx with x.i.j
 0.188      PL/I Matrix as coded: FIXED
 0.058      PL/I Matrix BIN FIXED(15)
14.217      the REXX program run with Regina
10.109      the REXX program run with ooRexx

```



## NewLISP


```NewLISP
(let (a (array 10 10))
  (dotimes (i 10)
    (dotimes (j 10)
      (setf (a i j) (rand 21))))
  (catch
      (dotimes (i 10)
	(dotimes (j 10)
	  (print (a i j))
	  (print " ")
	  (if (= 20 (a i j))
	      (throw))))))
```



## Nim


```nim
import math, strutils

const arrSize = 10

var a: array[0..arrSize-1, array[0..arrSize-1, int]]
var s: string = ""

randomize()   # different results each time this runs

for i in 0 .. arrSize-1:
   for j in countup(0,arrSize-1):
      a[i][j] = random(20)+1

block outer:
   for i in countup(0,arrSize-1):
      for j in 0 .. arrSize-1:
         if a[i][j] < 10:
            s.add(" ")
         addf(s,"$#",$a[i][j])
         if a[i][j] == 20:
            break outer
         s.add(", ")
      s.add("\n")
echo(s)
```

{{out}}

```txt
 9, 16,  3, 18,  4, 17,  2, 16,  7,  6,
 1,  6,  1, 11,  9,  8, 12,  7, 19,  8,
13, 16,  4,  5,  2, 20
```



## OCaml


In the interactive interpreter:


```ocaml
$ ocaml

# Random.self_init();;
- : unit = ()

# let m = Array.make_matrix 10 10 0 ;;
val m : int array array =
  [|[|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]|]

# for i = 0 to pred 10 do
    for j = 0 to pred 10 do
      m.(i).(j) <- 1 + Random.int 20
    done;
  done;;
- : unit = ()

# try
    for i = 0 to pred 10 do
      for j = 0 to pred 10 do
        Printf.printf " %d" m.(i).(j);
        if m.(i).(j) = 20 then raise Exit;
      done;
      print_newline()
    done;
  with Exit ->
    print_newline()
  ;;
 15 8 15 9 9 6 1 18 6 18
 17 1 13 15 13 1 16 4 13 9
 15 3 5 19 17 3 1 11 5 2
 1 1 6 19 20
- : unit = ()
```



## Octave

Octave has no way of exiting nested loop; so we need a control variable, or we can use the trick of embedding the loops into a function and use the <tt>return</tt> statement. (The search for "exactly 20" is changed into a search for "almost 20")

```octave
function search_almost_twenty()
% create a 100x100 matrix...
m = unifrnd(0,20, 100,100);
for i = 1:100
  for j = 1:100
    disp( m(i,j) )
    if ( abs(m(i,j) - 20) < 1e-2 )
      return
    endif
  endfor
endfor
endfunction

search_almost_twenty()

% avoiding function, we need a control variable.
m = unifrnd(0,20, 100,100);
innerloopbreak = false;
for i = 1:100
  for j = 1:100
    disp( m(i,j) )
    if ( abs(m(i,j) - 20) < 1e-2 )
      innerloopbreak = true;
      break;
    endif
  endfor
  if ( innerloopbreak )
    break;
  endif
endfor
```



## OoRexx


```oorexx
numbers = .array~new()
do i = 1 to 10
    do j = 1 to 10
        numbers[i,j] = random(1, 20)
    end
end

do i = 1 to numbers~dimension(1)
    do j = 1 to numbers~dimension(2)
        say numbers[i,j]
        if numbers[i,j] = 20 then
            leave i
    end
end
```



## Oz

We can directly access and use the outer loop's break procedure:

```oz
declare
  fun {CreateMatrix Width Height}
    Matrix = {List.make Height}
  in
    for Row in Matrix do
       Row = {List.make Width}
       for X in Row do
          X = {OS.rand} mod 20 +1
       end
    end
    Matrix
  end

  proc {PrintMatrix Matrix}
    %% print until we see 20
     for Row in Matrix break:OuterBreak do
        for X in Row do
           {Show X}
           if X == 20 then {OuterBreak} end
        end
     end
  end
in
  {PrintMatrix {CreateMatrix 10 10}}
```



## PARI/GP


```parigp
M=matrix(10,10,i,j,random(20)+1);
for(i=1,10,for(j=1,10,if(M[i,j]==20,break(2))))
```



## Pascal

{{works with|FreePascal|1.0}}

```pascal
program LoopNested;
uses SysUtils;
const Ni=10; Nj=20;
var
  tab: array[1..Ni,1..Nj] of Integer;
  i, j: Integer;
label loopend;
begin
  for i := 1 to Ni do
    for j := 1 to Nj do
      tab[i,j]:=random(20)+1;
  for i := 1 to Ni do
  begin
    for j := 1 to Nj do
    begin
      WriteLn(tab[i,j]);
      if tab[i,j]=20 then goto loopend
    end
  end;
loopend:
end.
```



## Perl


```perl
my $a = [ map [ map { int(rand(20)) + 1 } 1 .. 10 ], 1 .. 10];

Outer:
foreach (@$a) {
    foreach (@$_) {
        print " $_";
        if ($_ == 20) {
            last Outer;
        }
    }
    print "\n";
}
print "\n";
```



## Perl 6

{{works with|rakudo|2015-09-18}}

```perl6
my @a = [ (1..20).roll(10) ] xx *;

LINE: for @a -> @line {
    for @line -> $elem {
        print " $elem";
        last LINE if $elem == 20;
    }
    print "\n";
}
print "\n";
```

{{out}}

```txt
 15 6 14 13 14 7 9 16 8 18
 7 6 18 11 19 13 12 5 18 8
 17 17 9 5 4 8 17 8 3 11
 9 20
```



## Phix

use an explicit flag

```Phix
constant s = sq_rand(repeat(repeat(20,20),20))
integer found = 0
for i=1 to 20 do
    for j=1 to 20 do
        printf(1,"%d",s[i][j])
        if s[i][j]=20 then
            found = 1
            exit
        end if
        printf(1,", ")
    end for
    printf(1,"\n")
    if found then exit end if
end for
```

alternatively you can use a procedure

```Phix
procedure till20()
    for i=1 to 20 do
        for j=1 to 20 do
            printf(1,"%d",s[i][j])
            if s[i][j]=20 then return end if
            printf(1,", ")
        end for
        printf(1,"\n")
    end for
end procedure
till20()
printf(1,"\n")
```

or even inline assembly to effect a goto

```Phix
for i=1 to 20 do
    for j=1 to 20 do
        printf(1,"%d",s[i][j])
        if s[i][j]=20 then #ilASM{jmp :%done} end if
        printf(1,", ")
    end for
    printf(1,"\n")
end for
#ilASM{:%done}
printf(1,"\n")
```



## PHP


```php
<?php
for ($i = 0; $i < 10; $i++)
    for ($j = 0; $j < 10; $j++)
        $a[$i][$j] = rand(1, 20);

foreach ($a as $row) {
    foreach ($row as $element) {
        echo " $element";
        if ($element == 20)
            break 2; // 2 is the number of loops we want to break out of
    }
    echo "\n";
}
echo "\n";
?>
```



## PicoLisp


```PicoLisp
(for Lst (make (do 10 (link (make (do 10 (link (rand 1 20)))))))
   (T
      (for N Lst
         (printsp N)
         (T (= N 20) T) ) ) )
```

or:

```PicoLisp
(catch NIL
   (for Lst (make (do 10 (link (make (do 10 (link (rand 1 20)))))))
      (for N Lst
         (printsp N)
         (and (= N 20) (throw)) ) ) )
```



## PL/I


```PL/I
   declare x(20,20) fixed;  /* 16 August 2010. */
   x = random()*20 + 1;
loops:
   do i = 1 to hbound(x,1);
      do j = 1 to hbound(x,2);
         put (x(i,j));
         if x(i,j) = 20 then leave loops;
      end;
      if x(i,j) = 20 then leave;
   end;
```



## PureBasic


```PureBasic
; Creating and filling array
Dim Value(10, 5)
For a = 0 To 10
  For b = 0 To 5
    Value(a, b) = Random(19) + 1
  Next
Next
; iterating trough array
For a = 0 To 10
  For b = 0 To 5
    Debug Value(a, b)
    If Value(a, b) = 20
      ; 2 indicates, that there are two nested lopps to break out
      Break 2
    EndIf
  Next
Next
```



## Python

Python has only inner loop breaks. The normal way to solve this problem in Python is to move the code in a function, and use return:

```python
from random import randint

def do_scan(mat):
    for row in mat:
        for item in row:
            print item,
            if item == 20:
                print
                return
        print
    print

mat = [[randint(1, 20) for x in xrange(10)] for y in xrange(10)]
do_scan(mat)
```

The , after print element suppresses printing a line break. The code needs some minor changes for Python 3.

Two more solutions around this problem, the first uses exception handling:

```python
from random import randint

class Found20(Exception):
    pass

mat = [[randint(1, 20) for x in xrange(10)] for y in xrange(10)]

try:
    for row in mat:
        for item in row:
            print item,
            if item == 20:
                raise Found20
        print
except Found20:
    print
```

The second uses a flag variable:

```python
from random import randint

mat = [[randint(1, 20) for x in xrange(10)] for y in xrange(10)]

found20 = False
for row in mat:
    for item in row:
        print item,
        if item == 20:
            found20 = True
            break
    print
    if found20:
        break
```



## Qi


```Qi

(define random-list
  0 -> []
  M -> [(1+ (RANDOM 20)) | (random-list (1- M))])

(define random-array
  0 _ -> []
  N M -> [(random-list M) | (random-array (1- N) M)])

(define array->list
  _    []                 -> []                                  \ "end outer loop" \
  Stop [[]          | Ra] -> (array->list Stop Ra)               \ "outer loop" \
  Stop [[Stop | _ ] | _ ] -> []                                  \ "break out from inner loop" \
  Stop [[X    | Rl] | Ra] -> [X | (array->list Stop [Rl | Ra])]) \ "inner loop" \

(array->list 20 (random-array 10 10))

```



## R


```R
m <- 10
n <- 10
mat <- matrix(sample(1:20L, m*n, replace=TRUE), nrow=m); mat
done <- FALSE
for(i in seq_len(m))
{
   for(j in seq_len(n))
   {
      cat(mat[i,j])
      if(mat[i,j] == 20)
      {
         done <- TRUE
         break
      }
      cat(", ")
   }
   if(done)
   {
      cat("\n")
      break
   }
}
```


or


```R

m <- 10; n <- 10; mat <- matrix(sample(1:20L, m*n, replace=TRUE), nrow=m);
x<-which(mat==20,arr.ind=TRUE,useNames=FALSE)
x<-x[order(x[,1]),]
for(i in mat[1:x[1,1]-1,]) print(i)
for(i in mat[x[1,1],1:x[1,2]]) print(i)

```



## Racket


```racket

#lang racket
(define (scan xss)
  (for* ([xs xss]
         [x  xs]
         #:final (= x 20))
    (displayln x)))

(define matrix
  (for/list ([x 10])
    (for/list ([y 10])
      (+ (random 20) 1))))

(scan matrix)
```



## REBOL


```REBOL
REBOL [
	Title: "Loop/Nested"
	URL: http://rosettacode.org/wiki/Loop/Nested
]

; Number formatting.
zeropad: func [pad n][
    n: to-string n  insert/dup n "0" (pad - length? n)  n]

; Initialize random number generator from current time.
random/seed now

; Create array and fill with random numbers, range 1..20.
soup: array [10 10]
repeat row soup [forall row [row/1: random 20]]

print "Loop break using state variable:"
done: no
for y 1 10 1 [
	for x 1 10 1 [
		prin rejoin [zeropad 2 soup/:x/:y  " "]
		if 20 = soup/:x/:y [done: yes  break]
	]
	prin crlf
	if done [break]
]

print [crlf "Loop break with catch/throw:"]
catch [
	for y 1 10 1 [
		for x 1 10 1 [
			prin rejoin [zeropad 2 soup/:x/:y  " "]
			if 20 = soup/:x/:y [throw 'done]
		]
		prin crlf
	]
]
prin crlf
```

{{out}}

```txt
Loop break using state variable:
15 09 11 03 17 07 09 16 03 07
03 15 04 06 13 05 10 06 02 14
17 05 06 12 03 19 03 03 17 04
17 15 14 17 15 07 06 16 13 11
02 08 12 16 04 14 03 19 02 02
02 13 14 14 15 01 10 07 17 03
07 17 20

Loop break with catch/throw:
15 09 11 03 17 07 09 16 03 07
03 15 04 06 13 05 10 06 02 14
17 05 06 12 03 19 03 03 17 04
17 15 14 17 15 07 06 16 13 11
02 08 12 16 04 14 03 19 02 02
02 13 14 14 15 01 10 07 17 03
07 17 20
```



## REXX

Since the two-dimensional array could potentially not be large enough
to contain the target (20), it's possible to not find the target.

Code was added to this REXX program to reflect that possibility and issue an appropriate message (whether the target was found or not).

```rexx
/*REXX program loops through a two-dimensional array to search for a  '20'    (twenty). */
parse arg rows cols targ .                       /*obtain optional arguments from the CL*/
if rows=='' | rows==","  then rows=60            /*Rows not specified?  Then use default*/
if cols=='' | cols==","  then cols=10            /*Cols  "      "         "   "     "   */
if targ=='' | targ==","  then targ=20            /*Targ  "      "         "   "     "   */
w=max(length(rows), length(cols), length(targ))  /*W:  used for formatting the output.  */
not= 'not'                                       /* [↓]  construct the 2─dimension array*/
          do     row=1  for rows                 /*ROW  is the 1st dimension of array.  */
              do col=1  for cols                 /*COL   "  "  2nd     "      "   "     */
              @.row.col=random(1, targ)          /*create some positive random integers.*/
              end   /*row*/
          end       /*col*/

          do     r=1  for rows    /* ◄───────────────── now, search for the target {20}.*/
              do c=1  for cols
              say left('@.'r"."c, 3+w+w) '=' right(@.r.c, w)    /*show an array element.*/
              if @.r.c==targ  then do; not=; leave r; end       /*found the targ number?*/
              end   /*c*/
          end       /*r*/

say right( space( 'Target'  not  "found:" )    targ, 33, '─')
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default inputs:

```txt

@.1.1   = 19
@.1.2   = 14
@.1.3   = 16
@.1.4   =  8
@.1.5   =  1
@.1.6   =  4
@.1.7   = 11
@.1.8   =  7
@.1.9   = 15
@.1.10  = 16
@.2.1   = 11
@.2.2   =  4
@.2.3   =  3
@.2.4   =  6
@.2.5   = 18
@.2.6   =  7
@.2.7   =  5
@.2.8   =  7
@.2.9   =  2
@.2.10  =  7
@.3.1   = 20
─────────────────Found target: 20

```

'''output'''   when using the input of:   <tt> 2   2 </tt>

```txt

@.1.1   = 14
@.1.2   =  6
@.2.1   = 13
@.2.2   = 13
─────────────Target not found: 20

```



## Ring


```ring

size = 5
array = newlist(size,size)
for row = 1 to size
    for col = 1 to size
        array[row][col] = random(19) + 1
    next
next

for row = 1 to size
    for col = 1 to size
        see "row " + row + " col " + col + "value : " + array[row][col] + nl
        if array[row][col] = 20 exit for row ok
    next
next

func newlist x, y
     if isstring(x) x=0+x ok
     if isstring(y) y=0+y ok
     aList = list(x)
     for t in aList
         t = list(y)
     next
     return aList

```

Output:

```txt

row 1 col 1 value : 10
row 1 col 2 value : 3
row 1 col 3 value : 8
row 1 col 4 value : 8
row 1 col 5 value : 1
row 2 col 1 value : 3
row 2 col 2 value : 3
row 2 col 3 value : 4
row 2 col 4 value : 6
row 2 col 5 value : 8
row 3 col 1 value : 14
row 3 col 2 value : 12
row 3 col 3 value : 2
row 3 col 4 value : 11
row 3 col 5 value : 9
row 4 col 1 value : 17
row 4 col 2 value : 9
row 4 col 3 value : 19
row 4 col 4 value : 12
row 4 col 5 value : 12
row 5 col 1 value : 7
row 5 col 2 value : 6
row 5 col 3 value : 17
row 5 col 4 value : 5
row 5 col 5 value : 6

```



## Ruby

As the break command only jumps out of the innermost loop,
this task requires Ruby's <code>catch/throw</code> functionality.

```ruby
ary = (1..20).to_a.shuffle.each_slice(4).to_a
p ary

catch :found_it do
  for row in ary
    for element in row
      print "%2d " % element
      throw :found_it if element == 20
    end
    puts ","
  end
end

puts "done"
```

{{out}}

```txt
[[2, 12, 10, 4], [18, 11, 9, 3], [14, 15, 7, 17], [6, 19, 8, 13], [1, 20, 16, 5]]
 2 12 10  4 ,
18 11  9  3 ,
14 15  7 17 ,
 6 19  8 13 ,
 1 20 done
```


However, for-loops are not very popular. This is more idiomatic ruby, which avoids loops and breaking out of them:

```ruby
p slices = [*1..20].shuffle.each_slice(4)

slices.any? do |slice|
  puts
  slice.any? do |element|
    print "#{element} "
    element == 20
  end
end
puts "done"
```

{{out}}

```txt

#<Enumerator: [1, 4, 9, 13, 15, 10, 3, 5, 14, 17, 18, 8, 2, 12, 6, 19, 20, 11, 7, 16]:each_slice(4)>

1 4 9 13
15 10 3 5
14 17 18 8
2 12 6 19
20 done

```



## Run BASIC


```runbasic
dim a(10,10)
cls
for row = 1 TO 10
        for col = 1 TO 10
                a(row,col) = INT(20 * RND(1) + 1)
        next col
next row

for row = 1 to 10
        for col = 1 to 10
               print a(row, col)
                if a(row, col) = 20 then goto [end]
        next col
next row
[end]
print "At row:";row;" col:";col
```




## Rust

{{libheader|rand}}

```rust
use rand::Rng;

extern crate rand;

fn main() {
    let mut matrix = [[0u8; 10]; 10];
    let mut rng = rand::thread_rng();

    for row in matrix.iter_mut() {
        for item in row.iter_mut() {
            *item = rng.gen_range(0, 21);
        }
    }

    'outer: for row in matrix.iter() {
        for &item in row.iter() {
            print!("{:2} ", item);
            if item == 20 { break 'outer }
        }
        println!();
    }
}
```

{{out}}

```txt
 5  3  8 18 13  2  5 13  6 17
 5 14 20
```



## Sather


```sather
class MAIN is
  main is
    a:ARRAY2{INT} := #(10,10);
    i, j :INT;

    RND::seed(1230);
    loop i := 0.upto!(9);
      loop j := 0.upto!(9);
         a[i, j] := RND::int(1, 20);
      end;
    end;

    loopthis ::= true;
    loop i := 0.upto!(9); while!( loopthis );
      loop j := 0.upto!(9);
        #OUT  + " " + a[i, j];
        if a[i, j] = 20 then
	  loopthis := false;
	  break!;
	end;
      end;
    end;
  end;
end;
```



## Scala

In Scala there is no build-in 'break' keyword. That functionality comes from a library.

```scala
import scala.util.control.Breaks._
val a=Array.fill(5,4)(scala.util.Random.nextInt(21))
println(a map (_.mkString("[", ", ", "]")) mkString "\n")
breakable {
  for(row <- a; x <- row){
    println(x)
    if (x==20) break
  }
}
```

{{out}}

```txt
[14, 16, 5, 7]
[0, 15, 13, 20]
[0, 3, 8, 17]
[4, 20, 2, 2]
[12, 6, 11, 15]
14
16
5
7
0
15
13
20
```



## Scheme

Using call/cc:

```scheme
(call-with-current-continuation
 (lambda (return)
   (for-each (lambda (a)
	       (for-each (lambda (b)
			   (cond ((= 20 b)
				  (newline)
				  (return))
				 (else
				  (display " ")(display b))))
			 a)
	       (newline))
	     array)))
```

Using tail-call:

```scheme
(let loop ((a array))
  (if (pair? a)
      (let loop2 ((b (car a)))
	(cond ((null? b)
	       (newline)
	       (loop (cdr a)))
	      ((= 20 (car b))
	       (newline))
	      (else
	       (display " ")(display (car b))
	       (loop2 (cdr b)))))))
```



## Scilab

{{works with|Scilab|5.5.1}}
<lang>ni=3;nj=4
t=int(rand(ni,nj)*20)+1
for i=1:ni
    for j=1:nj
        printf("%2d ",t(i,j))
        if t(i,j)==11 then break; end
    end
    printf("\n")
    if t(i,j)==11 then break; end
end
```

{{out}}

```txt
 5 18 19  8
 5 14  5  6
 5  7  7 12
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: i is 0;
    var integer: j is 0;
    var array array integer: a is 10 times 10 times 0;
    const EXCEPTION: FOUND20 is enumlit;
  begin
    for i range 1 to 10 do
      for j range 1 to 10 do
        a[i][j] := rand(1, 20);
      end for;
    end for;
    block
      for i range 1 to 10 do
        for j range 1 to 10 do
          write(a[i][j] lpad 2 <& ", ");
          if a[i][j] = 20 then
            raise FOUND20;
          end if;
        end for;
        writeln;
      end for;
    exception
      catch FOUND20: writeln;
    end block;
  end func;
```


{{out}}

```txt

15, 10,  5,  9, 10, 13,  1,  9, 11, 10,
 5,  6, 10, 13,  4, 13, 11, 12,  2,  4,
 4, 16, 20,

```



## Sidef


```ruby
var arr = 10.of{ 10.of{ 20.irand + 1 } }

for row in arr {
    for num in row {
        "%3d".printf(num);
        num == 20 && goto :OUT
    }
    print "\n"
} @:OUT

print "\n"
```

{{out}}

```txt
  9 17 14 17 17  7  1  3  9 18
  1 12  1 19  9  5  1 17 19  3
 17  2 18 12 15 10  8 13 13 14
 12 16 13 13  2 11  3 15  2  4
 15 15  8 11  5  2  1 16  8 13
 17  3  1  1  8 12  4 20
```



## Smalltalk


Notice that the original answer (see below) was wrong (never say never say never...).

{{works with|Smalltalk/X}}

it looks a bit wierd, but here is: loopWithExit


```smalltalk
|i|

i := 1.
[:exit |
    Transcript showCR:i.
    i == 5 ifTrue:[ exit value:'stopped' ].
    i := i + 1.
] loopWithExit
```

these can also be nested, and exited from the inner loop:

```smalltalk
|i|

i := 1.
[:exit1 |
    |j|

    j := 0.
    [:exit2 |
        Transcript showCR:('i is %1 / j is %2' bindWith:i with:j).
        j == 5 ifTrue:[ exit2 value: nil ].
        i == 5 ifTrue:[ exit1 value: nil ].
        j := j + 1.
    ] loopWithExit.
    i := i + 1
] loopWithExit
```

in case your smalltalk does not have it, here's the definition:

```smalltalk
!Block methodsFor:'looping'!
loopWithExit
    "the receiver must be a block of one argument.  It is evaluated in a loop forever,
     and is passed a block, which, if sent a value:-message, will exit the receiver block,
     returning the parameter of the value:-message. Used for loops with exit in the middle."

    |exitBlock|

    exitBlock := [:exitValue | ^ exitValue].
    [true] whileTrue:[ self value:exitBlock ]
```

in the same spirit, exits could be added to many other loop constructs. However, this is really only very rarely needed in Smalltalk, because a ^(return) out of a block returns from the enclosing method which usually used to exit early from search utility methods.

There is also valueWithExit, which can be used to get out of a block early and provide an alternative value. Using that, the tasks solution is:

```smalltalk
|v result|

v := 1 to:20 collect:[:i |
        1 to:20 collect:[:j | Random nextIntegerBetween:1 and:20 ]
     ].

result :=
    [:exit |
        1 to:20 do:[:row |
            1 to:20 do:[:col |
                |element|

                (element := (v at:row) at:col) printCR.
                element == 20 ifTrue:[ exit value:(row @ col) ].
            ]
        ].
        nil
    ] valueWithExit.

result isNil ifTrue:[
    'ouch - no 20 found' printCR.
] ifFalse:[
    '20 found at ' print. result printCR
]
```

{{out}}

```txt
19
6
1
7
12
20
20 found at 1@6
```



{{works with|GNU Smalltalk}}
Smalltalk has no ways of escaping from loops (single or nested), even if it is possible to extend its iteration capabilities in several ways.

The following code implements a BiArray class with a method that allows iteration over the elements (by columns and then by rows) and execution of a block if a condition is true.

```smalltalk
"this simple implementation of a bidimensional array
 lacks controls over the indexes, but has a way of iterating
 over array's elements, from left to right and top to bottom"
Object subclass: BiArray [
  |cols rows elements|
  BiArray class >> columns: columns  rows: howManyRows [
      ^ super basicNew init: columns per: howManyRows
  ]
  init: columns per: howManyRows [
     cols := columns.
     rows := howManyRows.
     elements := Array new: ( columns * howManyRows )
  ]
  calcIndex: biIndex [ "column, row (x,y) to linear"
    ^ ( (biIndex at: 1) + (((biIndex at: 2) - 1) * cols) )
  ]
  at: biIndex [ "biIndex is an indexable containing column row"
     ^ elements at: (self calcIndex: biIndex).
  ]
  directAt: i [ ^ elements at: i ]
  at: biIndex put: anObject [
     elements at: (self calcIndex: biIndex) put: anObject
  ]
  whileTrue: aBlock do: anotherBlock [
     |i lim|
     i := 1. lim := rows * cols.
     [ ( i <= lim )
         & (aBlock value: (self directAt: i) )
     ] whileTrue: [
         anotherBlock value: (self directAt: i).
         i := i + 1.
       ]
  ]
].

|biarr|
biarr := BiArray columns: 10 rows: 10.

"fill the array; this illustrates nested loop but not how to
 escape from them"
1 to: 10 do: [ :c |
  1 to: 10 do: [ :r |
     biarr at: {c . r} put: (Random between: 1 and: 20)
  ]
].

"loop searching for 20; each block gets the element passed as argument"
biarr whileTrue: [ :v | v ~= 20 ]
      do: [ :v | v displayNl ]
```


## SPL


```spl
'fill array
mx,my = 30
> y, 1..my
  > x, 1..mx
    a[x,y] = #.rnd(20)+1
  <
<
'scan array
> y, 1..my
  > x, 1..mx
    #.output("x=",x,", y=",y, ", a=",a[x,y])
    << a[x,y] = 20
  <
  << x!>mx
<
```

{{out}}

```txt

x=1, y=1, a=7
x=2, y=1, a=7
x=3, y=1, a=19
x=4, y=1, a=1
x=5, y=1, a=20

```



## Stata

In Stata macro language, one can only break the innermost loop, with '''[https://www.stata.com/help.cgi?continue continue, break]'''. There are several ways to cope with this.

First, build the matrix:


```stata
matrix a=J(20,20,0)
forv i=1/20 {
	forv j=1/20 {
		matrix a[`i',`j']=runiformint(1,20)
	}
}
```


Use nested '''[https://www.stata.com/help.cgi?forvalues forvalues]'''. If 20 is found, set a flag and break the inner loop. In the outer loop, check the flag and break the outer loop if 20 was found.


```stata
local q 0
forv i=1/20 {
	forv j=1/20 {
		display "check `i',`j'"
		if el("a",`i',`j')==20 {
			display "found at `i',`j'"
			local q 1
			continue, break
		}
	}

	if `q' continue, break
}
if !`q' {
	display "not found"
}
```


Use nested '''[https://www.stata.com/help.cgi?while while]''' loops, and check both the loop indices and a flag. One could also use an inner forvalue loop together with an outer while loop.


```stata
local q 0
local i=1
while !`q' & `i'<=20 {
	local j=1
	while !`q' & `j'<=20 {
		display "check `i',`j'"
		if el("a",`i',`j')==20 {
			display "found at `i',`j'"
			local q 1
		}
		local ++j
	}
	local ++i
}
if !`q' {
	display "not found"
}
```


Use the exit/capture exception mechanism: '''[https://www.stata.com/help.cgi?exit_program exit]''' tos throw an exception, and '''[https://www.stata.com/help.cgi?capture capture]''' to catch it. Since this catches all exception, you have then to check the value of '''[https://www.stata.com/help.cgi?_variables _rc]'''.


```stata
capture {
	forv i=1/20 {
		forv j=1/20 {
			display "check `i',`j'"
			if el("a",`i',`j')==20 {
				display "found at `i',`j'"
				exit -1
			}
		}
	}
}
if _rc==-1 {
	// value was found
}
else if _rc==0 {
	display "not found"
}
else exit _rc
```



###  Mata

In Mata, the situation is simpler: one may '''[https://www.stata.com/help.cgi?m2_return return]''' from a program without resort to exceptions, or use the '''[https://www.stata.com/help.cgi?m2_goto goto]''' statement. It's still possible to use '''[https://www.stata.com/help.cgi?m2_break break]''' and flags though.


```stata
function findval1(a,x,i0,j0) {
	n=rows(a)
	p=cols(a)
	for (i=1; i<=n; i++) {
		for (j=1; j<=p; j++) {
			if (a[i,j]==x) {
				i0=i
				j0=j
				return(1)
			}
		}
	}
	return(0)
}

function findval2(a,x,i0,j0) {
	n=rows(a)
	p=cols(a)
	q=0
	for (i=1; i<=n; i++) {
		for (j=1; j<=p; j++) {
			if (a[i,j]==x) {
				i0=i
				j0=j
				q=1
				goto END
			}
		}
	}
END:
	return(q)
}

function findval3(a,x,i0,j0) {
	n=rows(a)
	p=cols(a)
	q=0
	for (i=1; i<=n; i++) {
		for (j=1; j<=p; j++) {
			if (a[i,j]==x) {
				i0=i
				j0=j
				q=1
				break
			}
		}
		if (q) {
			break
		}
	}
	return(q)
}
```


Then with any of these functions, the return value indicates whether x has been found in a, and i,j are the indices where it has been found.


```stata
a=st_matrix("a")
findval1(a,20,i=.,j=.)
findval2(a,20,i=.,j=.)
findval3(a,20,i=.,j=.)
```



## Swift


```Swift
let array = [[2, 12, 10, 4], [18, 11, 20, 2]]

loop: for row in array {
    for element in row {
        println(" \(element)")
        if element == 20 { break loop }
    }
}
print("done")
```

{{out}}

```txt
 2
 12
 10
 4
 18
 11
 20
done

```



## Tcl

Tcl only supports single-level breaks; exiting more deeply nested looping requires the use of exceptions, which are considerably more verbose before Tcl 8.6.
{{works with|Tcl|8.6}}

```tcl
set ary [subst [lrepeat 10 [lrepeat 5 {[expr int(rand()*20+1)]}]]]

try {
    foreach row $ary {
        foreach col $row {
            puts -nonewline [format %3s $col]
            if {$col == 20} {
                throw MULTIBREAK "we're done"
            }
        }
        puts ,
    }
} trap MULTIBREAK {} {}
puts " done"
```

{{out}}

```txt
 12 13 14 13 15,
  1 14  7 16  3,
 12 11  5  1  9,
 12  5  1  4  2,
  6 11 11  4 11,
  7 14 20 done
```


=={{header|TI-83 BASIC}}==

```ti83b
PROGRAM:LOOP
(A,B)→dim([C])
For(I,1,A)
For(J,1,B)
int(rand*20+1)→[C](I,J)
End
End
For(I,1,A)
For(J,1,B)
Disp [C](I,J)
If [C](I,J)=20
Then
Stop
End
End
End

3→A:4→B:prgmLOOP
```


=={{header|TI-89 BASIC}}==
The <code>Stop</code> statement exits the containing ''program''.

```ti89b
Prgm
  Local mat,i,j
  © randMat(5, 5) exists but returns -9 to 9 rather than 1 to 20
  newMat(5, 5) → mat
  For i,1,rowDim(mat)
    For j,1,colDim(mat)
      rand(20) → mat[i,j]
    EndFor
  EndFor
  Disp mat
  Pause "Press a key."
  ClrIO
  For i,1,rowDim(mat)
    For j,1,colDim(mat)
      If mat[i,j] = 20 Then
        Stop
      Else
        Output i*8, j*18, mat[i,j]
      EndIf
    EndFor
  EndFor
EndPrgm
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
LOOP
row=""
 LOOP/CLEAR x=1,10
 x=RANDOM_NUMBERS (1,20,1)
 row=APPEND(row," ",x)
  IF (x==20) THEN
   PRINT row
   EXIT,EXIT
  ENDIF
 ENDLOOP
 PRINT row
ENDLOOP
```

{{out}}

```txt

9 6 6 5 10 18 11 17 17 9
5 16 2 4 2 15 13 13 4 9
12 4 6 19 3 1 3 12 13 8
3 7 4 8 15 12 1 20

```



## UNIX Shell

{{works with|Bash}}
Bash doesn't have two-dimentional arrays, so we fake it for this example

```bash
size=10

for ((i=0;i<size;i++)); do
  unset t[@]
  for ((j=0;j<size;j++)); do
    t[$j]=$((RANDOM%20+1))
  done
  a[$i]="${t[*]}"
done

for ((i=0;i<size;i++)); do
  t=(${a[$i]})
  for ((j=0;j<size;j++)); do
    printf "%2d " ${t[$j]}
    [ ${t[$j]} -eq 20 ] && break 2
  done
  echo
done
echo
```

{{out|Example output}}

```txt
 7  5  4  6  4  5  2 15 10  7
15  4 14  9 10 14 14  3  3  5
14 20
```



## VBA


```VB
Public Sub LoopsNested()
    Dim a(1 To 10, 1 To 10) As Integer
    Randomize
    For i = 1 To 10
        For j = 1 To 10
            a(i, j) = Int(20 * Rnd) + 1
        Next j
    Next i
    For i = 1 To 10
        For j = 1 To 10
            If a(i, j) <> 20 Then
                Debug.Print a(i, j),
            Else
                i = 10 'Upperbound iterator outerloop
                Exit For 'Exit For exits only innerloop
            End If
        Next j
        Debug.Print
    Next i
End Sub
```



## Visual Basic .NET

VB.NET doesn't have labelled loops, but the Exit statement discriminates between different types of block, allowing for several workarounds other than using a goto.

The set-up code:

```vbnet
Module Program
    Sub Main()
        Const ROWS = 10
        Const COLS = 10

        ' Initialize with seed 0 to get deterministic output (may vary across .NET versions, though).
        Dim rand As New Random(0)

        ' VB uses max index array declarations
        Dim nums(ROWS - 1, COLS - 1) As Integer

        For r = 0 To ROWS - 1
            For c = 0 To COLS - 1
                nums(r, c) = rand.Next(0, 21) ' Upper bound is exclusive.
            Next
        Next

        ' MISSING IMPLEMENTATION
    End Sub
End Module
```


'''Implementations:'''

Perhaps the simplest solution is to use a goto.

```vbnet
        For r = 0 To ROWS - 1
            For c = 0 To COLS - 1
                Dim val = nums(r, c)
                Console.WriteLine(val)
                If val = 20 Then GoTo BREAK
            Next
        Next
        BREAK:
```


If, ''for some reason'', a goto is undesirable, an alternative would be to exit a dummy outer block (in this case a single-iteration Do loop).

```vbnet
        Do
            For r = 0 To ROWS - 1
                For c = 0 To COLS - 1
                    Dim val = nums(r, c)
                    Console.WriteLine(val)
                    If val = 20 Then Exit Do
                Next
            Next
        Loop While False
```


Either For loop can also be converted to a different type of loop.

```vbnet
        For r = 0 To ROWS - 1
            Dim c = 0
            Do While c <= COLS - 1
                Dim val = nums(r, c)
                Console.WriteLine(val)
                If val = 20 Then Exit For
                c += 1
            Loop
        Next
```


The search can also be factored out to a separate method

```vbnet
    Sub Find20Impl(arr As Integer(,))
        For r = 0 To arr.GetLength(0) - 1
            For c = 0 To arr.GetLength(1) - 1
                Dim val = arr(r, c)
                Console.WriteLine(val)
                If val = 20 Then Exit Sub
               'If val = 20 Then Return ' Equivalent to above.
            Next
        Next
    End Sub
```


and called from Main():

```vbnet
        Find20Impl(nums)
```


A translation of the VBA above, that sets the iteration variable of the outer For loop to an out-of-range value and exits the inner loop regularly.

```vbnet
        For r = 0 To ROWS - 1
            For c = 0 To COLS - 1
                Dim val = nums(r, c)
                Console.WriteLine(val)
                If val = 20 Then
                    r = ROWS
                    Exit For
                End If
            Next
        Next
```


Similarly, a flag variable can be checked by the outer loop.

```vbnet
        Dim done = False
        For r = 0 To ROWS - 1
            For c = 0 To COLS - 1
                Dim val = nums(r, c)
                Console.WriteLine(val)
                If val = 20 Then
                    done = True
                    Exit For
                End If
            Next
            If done Then Exit For
        Next
```


{{out}}

```txt
15
17
16
11
4
11
19
9
20
```



## XPL0


```XPL0
include c:\cxpl\codes;                  \intrinsic 'code' declarations
def  Size=10;

proc Nest(A);   \Display 2-dimensional array A contents until 20 is found
int  A;
int  I, J, K;
[for J:= 0 to Size-1 do
    for I:= 0 to Size-1 do
        [K:= A(I,J);
        IntOut(0, K);  ChOut(0, ^ );
        if K = 20 then return;          \there is no 'goto' instruction
        ];
]; \Nest

proc Fill(A);   \Fill 2-dimensional array A with random numbers 1..20
int  A;
int  I, J;
[for J:= 0 to Size-1 do
    for I:= 0 to Size-1 do
        A(I,J):= Ran(20)+1;
]; \Fill

int Array(Size,Size);
[Fill(Array);
 Nest(Array);
]
```



## zkl

Using nested lists to simulate a 5x5 array. No breakbreak so an exception is used to long jump out of the loop (just 'cause, a state var could also be used).

```zkl
const N=5;
var rows=(0).pump(N,List, (0).pump.fp(N,List,(1).random.fpM("1-",21)) );
try{
   foreach r in (N){
      foreach c in (N){
         x:=rows[r][c]; x.print(",");
         if (x==20) { println("Found it!"); throw(Exception.Generic); }
      }
   }
   println("Not found");
}catch(Generic){}
```

{{out}}

```txt

14,13,17,11,11,3,15,20,Found it!
3,2,10,14,10,2,2,15,2,13,11,5,12,4,17,8,10,5,14,11,10,14,11,4,1,Not found

```



## ZX Spectrum Basic


```zxbasic
10 DIM a(10,10)
20 FOR i=1 TO 10: FOR j=1 TO 10
30 LET a(i,j)=INT (RND*20)+1
40 NEXT j: NEXT i
50 LET b=0: REM flag to abort loops
60 FOR i=1 TO 10: FOR j=1 TO 10
70 PRINT (" " AND a(i,j)<10);a(i,j);" ";
80 IF a(i,j)=20 THEN LET i=10: LET j=10: LET b=1: REM abort loops
90 NEXT j
100 IF b=0 THEN PRINT
110 NEXT i
120 STOP
```

{{out|Example output}}

```txt

16  7  8  6 14  8 12 14 12  9
 7  9 14  8 18 17  3 16  1 19
 5 13  7 11 13 14  4  7 19 14
20

```


{{omit from|GUISS|No loops or nesting, and we can't read our values}}
