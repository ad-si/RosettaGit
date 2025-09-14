+++
title = "Loops/Break"
description = ""
date = 2019-08-27T20:57:33Z
aliases = []
[extra]
id = 4334
[taxonomies]
categories = ["task", "Iteration"]
tags = []
languages = [
  "11l",
  "360_assembly",
  "6502_assembly",
  "ada",
  "aime",
  "algol_60",
  "algol_68",
  "applescript",
  "arc",
  "arm_assembly",
  "autohotkey",
  "awk",
  "axe",
  "bacon",
  "basic",
  "batch_file",
  "bbc_basic",
  "bc",
  "befunge",
  "c",
  "chapel",
  "chef",
  "clojure",
  "cobol",
  "coffeescript",
  "coldfusion",
  "commodore_basic",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dc",
  "delphi",
  "dwscript",
  "e",
  "easylang",
  "ela",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "fsharp",
  "futurebasic",
  "gambas",
  "gap",
  "gml",
  "go",
  "groovy",
  "harbour",
  "haskell",
  "hexiscript",
  "hicest",
  "holyc",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lang5",
  "lasso",
  "liberty_basic",
  "lingo",
  "lisaac",
  "livecode",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "mathematica",
  "maxima",
  "maxscript",
  "moo",
  "mumps",
  "neko",
  "nemerle",
  "netrexx",
  "newlisp",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "ol",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pl_i",
  "postscript",
  "powershell",
  "purebasic",
  "python",
  "qi",
  "quickbasic",
  "r",
  "racket",
  "rebol",
  "related_tasks",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "sas",
  "sather",
  "scala",
  "scheme",
  "scilab",
  "sidef",
  "simula",
  "smalltalk",
  "snabel",
  "snobol4",
  "spin",
  "spl",
  "sql_pl",
  "stata",
  "suneido",
  "swift",
  "tcl",
  "torquescript",
  "tuscript",
  "ubasic_4th",
  "unix_shell",
  "ursa",
  "vba",
  "vbscript",
  "visual_basic_dotnet",
  "xpl0",
  "zkl",
]
+++

{{task|Iteration}} [[Category:Loop modifiers]] [[Category:Simple]]

## Task

Show a loop which prints random numbers (each number newly generated each loop) from 0 to 19 (inclusive).

If a number is 10, stop the loop after printing it, and do not generate any further numbers.

Otherwise, generate and print a second random number before restarting the loop.

If the number 10 is never generated as the first number in a loop, loop forever.


## Related tasks

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





## 11l

```11l
L
   V a = random:(20)
   print(a)
   I a == 10
      L.break
   V b = random:(20)
   print(b)
```



## 360 Assembly


```360asm
*        Loops Break               15/02/2017
LOOPBREA CSECT
         USING  LOOPBREA,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
LOOP     MVC    PG,=CL80' '        clean buffer
         LA     R8,PG              ipg=0
         BAL    R14,RANDINT        call randint
         C      R6,=F'10'          if k=10 then leave
         BE     ENDLOOP             <-- loop break
         BAL    R14,RANDINT        call randint
         XPRNT  PG,L'PG            print buffer
         B      LOOP               loop forever
ENDLOOP  XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
RANDINT  L      R5,RANDSEED        randint
         M      R4,=F'397204091'   "
         D      R4,=X'7FFFFFFF'    "
         ST     R4,RANDSEED        "
         LR     R5,R4              "
         SR     R4,R4              "
         D      R4,=F'20'          "
         LR     R6,R4              k=randint(20)
         XDECO  R6,XDEC            edit k
         MVC    0(4,R8),XDEC+8     output k
         LA     R8,4(R8)           ipg=ipg+4
         BR     R14                return
RANDSEED DC     F'39710831'        seed
PG       DS     CL80               buffer
XDEC     DS     CL12
         YREGS
         END    LOOPBREA
```

```txt

   2   3
   9  10
  14   5
  18  16
   5   0
   1   3
   7  17
  19   8
  17  12
  10

```



## 6502 Assembly

Code is called as a subroutine (i.e. JSR LoopBreakSub).  Specific OS/hardware routines for generating random numbers and printing are left unimplemented.

```6502asm
LoopBreakSub:	PHA			;push accumulator onto stack


BreakLoop:	JSR GenerateRandomNum	;routine not implemented
		;generates random number and puts in memory location RandomNumber

		LDA RandomNumber
		JSR DisplayAccumulator	;routine not implemented
		CMP #10
		BEQ Break
		JSR GenerateRandomNum
		LDA RandomNumber
		JSR DisplayAccumulator
		JMP BreakLoop

Break:		PLA			;restore accumulator from stack
		RTS			;return from subroutine
```



## Ada


```Ada
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Test_Loop_Break is
   type Value_Type is range 0..19;
   package Random_Values is new Ada.Numerics.Discrete_Random (Value_Type);
   use Random_Values;
   Dice : Generator;
   A, B : Value_Type;
begin
   loop
      A := Random (Dice);
      Put_Line (Value_Type'Image (A));
      exit when A = 10;
      B := Random (Dice);
      Put_Line (Value_Type'Image (B));
   end loop;
end Test_Loop_Break;
```



## Aime


```aime
integer
main(void)
{
    integer a, b;

    while (1) {
        a = drand(19);
        o_integer(a);
        o_byte('\n');
        if (a == 10) {
            break;
        }

        b = drand(19);
        o_integer(b);
        o_byte('\n');
    }

    return 0;
}
```



## ALGOL 60

```algol60
'BEGIN' 'COMMENT' Loops/Break - ALGOL60 - 18/06/2018;
  'INTEGER' SEED;
  'INTEGER' 'PROCEDURE' RANDOM(N);
  'VALUE' N; 'INTEGER' N;
  'BEGIN'
    SEED:=(SEED*19157+12347) '/' 21647;
    RANDOM:=SEED-(SEED '/' N)*N+1
  'END' RANDOM;
  'INTEGER' I,J,K;
  SYSACT(1,6,120);SYSACT(1,8,60);SYSACT(1,12,1);'COMMENT' open print;
  SEED:=31567;
  J:=0;
  'FOR' I:=1, I+1 'WHILE' I 'LESS' 100 'DO' 'BEGIN'
    J:=J+1;
    K:=RANDOM(20);
    OUTINTEGER(1,K);
    'IF' J=8 'THEN' 'BEGIN'
       SYSACT(1,14,1);  'COMMENT' skip line;
       J:=0
    'END';
    'IF' K=10 'THEN' 'GOTO' LAB
  'END';
LAB:
  SYSACT(1,14,1);  'COMMENT' skip line;
'END'
```

```txt

        +17           +4          +20           +3          +16           +5           +1          +17
        +11           +2          +12           +5           +7           +6          +10

```




## ALGOL 68

```algol68
main: (
    INT a, b;
    INT seed := 4; # chosen by a fair dice roll, guaranteed to be random c.f. http://xkcd.com/221/ #
    # first random; #
    WHILE
        a := ENTIER (next random(seed) * 20);
        print((a));
  # WHILE # NOT (a = 10) DO
        b := ENTIER (next random(seed) * 20);
        print((b, new line))
    OD;
    print(new line)
)
```

<pre style="height:25ex;overflow:scroll">
        +13          +6
         +1          +8
        +13          +2
         +1         +12
         +0         +12
        +14          +8
         +9          +2
        +19         +13
         +0          +4
         +8         +14
        +17          +7
        +11          +9
         +7          +8
         +2          +1
        +11          +2
        +13         +18
         +3          +7
        +11         +17
         +4         +13
        +16         +12
        +19         +17
         +9          +7
         +8          +5
         +4          +8
         +7          +5
         +0         +18
         +8         +13
         +7          +4
        +10

```



## AppleScript


```AppleScript
repeat
	set a to random number from 0 to 19
	if a is 10 then
		log a
		exit repeat
	end if
	set b to random number from 0 to 19
	log a & b
end repeat
```



<pre style="height:25ex;overflow:scroll">(*12, 6*)
(*7, 8*)
(*17, 4*)
(*7, 2*)
(*0, 5*)
(*6, 3*)
(*5, 5*)
(*3, 14*)
(*7, 7*)
(*3, 11*)
(*5, 16*)
(*18, 2*)
(*5, 2*)
(*15, 17*)
(*16, 10*)
(*4, 18*)
(*8, 5*)
(*4, 15*)
(*11, 14*)
(*7, 2*)
(*1, 7*)
(*7, 7*)
(*4, 9*)
(*12, 17*)
(*8, 16*)
(*9, 1*)
(*16, 15*)
(*8, 2*)
(*9, 6*)
(*13, 6*)
(*17, 0*)
(*17, 18*)
(*4, 7*)
(*8, 10*)
(*11, 0*)
(*14, 17*)
(*9, 8*)
(*2, 17*)
(*1, 5*)
(*4, 5*)
(*5, 2*)
(*10*)
```



## Arc


```Arc
(point break
  (while t
    (let x (rand 20)
      (prn "a: " x)
      (if (is x 10)
        (break)))
    (prn "b: " (rand 20))))
```



## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program loopbreak.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessEndLoop: .asciz "loop break with value : \n"
szMessResult:  .ascii "Resultat = "      @ message result
sMessValeur:   .fill 12, 1, ' '
                   .asciz "\n"
.align 4
iGraine:  .int 123456
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                @ entry of program
    push {fp,lr}      @ saves 2 registers
1:    @ begin loop
    mov r4,#20
2:
    mov r0,#19
    bl genereraleas               @ generate number
    cmp r0,#10                       @ compar value
    beq 3f                         @ break if equal
    ldr r1,iAdrsMessValeur     @ display value
    bl conversion10             @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
    subs r4,#1                   @ decrement counter
    bgt 2b                      @ loop if greather
    b 1b                          @ begin loop one

3:
    mov r2,r0             @ save value
    ldr r0,iAdrszMessEndLoop
    bl affichageMess            @ display message
    mov r0,r2
    ldr r1,iAdrsMessValeur
    bl conversion10       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message

100:   @ standard end of the program
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    svc #0                       @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszMessResult:         .int szMessResult
iAdrszMessEndLoop:        .int szMessEndLoop
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}      @ save  registres
    mov r2,#0                  @ counter length
1:      @ loop length calculation
    ldrb r1,[r0,r2]           @ read octet start position + index
    cmp r1,#0                  @ if 0 its over
    addne r2,r2,#1            @ else add 1 in the length
    bne 1b                    @ and loop
                                @ so here r2 contains the length of the message
    mov r1,r0        			@ address message in r1
    mov r0,#STDOUT      		@ code to write to the standard output Linux
    mov r7, #WRITE             @ code call system "write"
    svc #0                      @ call systeme
    pop {r0,r1,r2,r7,lr}        @ restaur des  2 registres */
    bx lr                       @ return
/******************************************************************/
/*     Converting a register to a decimal                                 */
/******************************************************************/
/* r0 contains value and r1 address area   */
conversion10:
    push {r1-r4,lr}    @ save registers
    mov r3,r1
    mov r2,#10

1:	   @ start loop
    bl divisionpar10 @ r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48        @ digit
    strb r1,[r3,r2]  @ store digit on area
    sub r2,#1         @ previous position
    cmp r0,#0         @ stop if quotient = 0 */
    bne 1b	          @ else loop
    @ and move spaces in first on area
    mov r1,#' '   @ space
2:
    strb r1,[r3,r2]  @ store space in area
    subs r2,#1       @ @ previous position
    bge 2b           @ loop if r2 >= zéro

100:
    pop {r1-r4,lr}    @ restaur registres
    bx lr	          @return
/***************************************************/
/*   division par 10   signé                       */
/* Thanks to http://thinkingeek.com/arm-assembler-raspberry-pi/*
/* and   http://www.hackersdelight.org/            */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10:
  /* r0 contains the argument to be divided by 10 */
    push {r2-r4}   /* save registers  */
    mov r4,r0
    mov r3,#0x6667   @ r3 <- magic_number  lower
    movt r3,#0x6666  @ r3 <- magic_number  upper
    smull r1, r2, r3, r0   @ r1 <- Lower32Bits(r1*r0). r2 <- Upper32Bits(r1*r0)
    mov r2, r2, ASR #2     /* r2 <- r2 >> 2 */
    mov r1, r0, LSR #31    /* r1 <- r0 >> 31 */
    add r0, r2, r1         /* r0 <- r2 + r1 */
    add r2,r0,r0, lsl #2   /* r2 <- r0 * 5 */
    sub r1,r4,r2, lsl #1   /* r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10) */
    pop {r2-r4}
    bx lr                  /* leave function */

/***************************************************/
/*   Generation random number                  */
/***************************************************/
/* r0 contains limit  */
genereraleas:
    push {r1-r4,lr}    @ save registers
    ldr r4,iAdriGraine
    ldr r2,[r4]
    ldr r3,iNbDep1
    mul r2,r3,r2
    ldr r3,iNbDep1
    add r2,r2,r3
    str r2,[r4]     @ maj de la graine pour l appel suivant

    mov r1,r0        @ divisor
    mov r0,r2        @ dividende
    bl division
    mov r0,r3       @  résult = remainder

100:                @ end function
    pop {r1-r4,lr}   @ restaur registers
    bx lr            @ return
/********************************************************************/
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
    mov r2, #0                @ init quotient
    mov r3, #0                @ init remainder
    mov r4, #32               @ init counter bits
    b 2f
1:          @ loop
    movs r0, r0, LSL #1     @ r0 <- r0 << 1 updating cpsr (sets C if 31st bit of r0 was 1)
    adc r3, r3, r3           @ r3 <- r3 + r3 + C. This is equivalent to r3 <- (r3 << 1) + C
    cmp r3, r1               @ compute r3 - r1 and update cpsr
    subhs r3, r3, r1        @ if r3 >= r1 (C=1) then r3 <- r3 - r1
    adc r2, r2, r2           @ r2 <- r2 + r2 + C. This is equivalent to r2 <- (r2 << 1) + C
2:
    subs r4, r4, #1          @ r4 <- r4 - 1
    bpl 1b                  @ if r4 >= 0 (N=0) then loop
    pop {r4, lr}
    bx lr




```



## AutoHotkey


```AutoHotkey
Loop
{
  Random, var, 0, 19
  output = %output%`n%var%
  If (var = 10)
    Break
  Random, var, 0, 19
  output = %output%`n%var%
}
MsgBox % output
```



## AWK


```awk
BEGIN {
	for (;;) {
		print n = int(rand() * 20)
		if (n == 10)
			break
		print int(rand() * 20)
	}
}
```



## Axe

Because Axe only supports breaking out of loops as end conditions, the behavior must be simulated using a return statement. Note, however, that this will exit the current call context, not the necessarily just the current loop.


```axe
While 1
 rand^20→A
 Disp A▶Dec
 ReturnIf A=10
 rand^20→B
 Disp B▶Dec,i
End
```



## BASIC


=
## BaCon
=

```freebasic

REPEAT
    number = RANDOM(20)
    PRINT "first  " ,number
        IF number = 10 THEN
            BREAK
        ENDIF
    PRINT "second  ",RANDOM(20)
UNTIL FALSE
```


=
## Commodore BASIC
=
In Commodore BASIC, the function RND() generates a floating point number from 0.0 to 1.0 (exclusive).

```commodorebasic
10 X = RND(-TI) : REM SEED RN GENERATOR
20 A = INT(RND(1)*20)
30 PRINT A
40 IF A = 10 THEN 80
50 B = INT(RND(1)*20)
60 PRINT B
70 GOTO 20
80 END
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 RANDOMIZE
110 DO
120   LET A=RND(20)+1
130   PRINT A,
140   IF A=10 THEN EXIT DO
150   PRINT RND(20)+1
160 LOOP
```


=
## QuickBASIC
=
```qbasic
do
    a = int(rnd * 20)
    print a
    if a = 10 then exit loop 'EXIT FOR works the same inside FOR loops
    b = int(rnd * 20)
    print b
loop
```


=== {{header|ZX Spectrum Basic}} ===
On the ZX Spectrum, for loops must be terminated through the NEXT statement, otherwise a memory leak will occur. To terminate a loop prematurely, set the loop counter to the last iterative value and jump to the NEXT statement:


```zxbasic
10 FOR l = 1 TO 20
20 IF l = 10 THEN LET l = 20: GO TO 40: REM terminate the loop
30 PRINT l
40 NEXT l
50 STOP
```


The correct solution:


```zxbasic
10 LET a = INT (RND * 20)
20 PRINT a
30 IF a = 10 THEN STOP
40 PRINT INT (RND * 20)
50 GO TO 10
```



## Batch File


```dos
@echo off
:loop
  set /a N=%RANDOM% %% 20
  echo %N%
  if %N%==10 exit /b
  set /a N=%RANDOM% %% 20
  echo %N%
goto loop
```



## BBC BASIC

```bbcbasic
      REPEAT
        num% = RND(20)-1
        PRINT num%
        IF num%=10 THEN EXIT REPEAT
        PRINT RND(20)-1
      UNTIL FALSE
```



## bc


```bc
s = 1  /* seed of the random number generator */
scale = 0

/* Random number from 0 to 20. */
define r() {
	auto a
	while (1) {
		/* Formula (from POSIX) for random numbers of low quality. */
		s = (s * 1103515245 + 12345) % 4294967296
		a = s / 65536       /* a in [0, 65536) */
		if (a >= 16) break  /* want a >= 65536 % 20 */
	}
	return (a % 20)
}


while (1) {
	n = r()
	n    /* print 1st number */
	if (n == 10) break
	r()  /* print 2nd number */
}
quit
```



## Befunge


```Befunge

>60v  *2\<
  >?>\1-:|
   1+    $
   >^    7
 v.:%++67<
 >55+-#v_@
       >60v  *2\<
         >?>\1-:|
          1+    $
          >^    7
^         .%++67<

```



## C



```c

int main(){
	time_t t;
	int a, b;
	srand((unsigned)time(&t));
	for(;;){
		a = rand() % 20;
		printf("%d\n", a);
		if(a == 10)
			break;
		b = rand() % 20;
		printf("%d\n", b);
	}
	return 0;
}
```

Output (example):

```txt

12
18
2
8
10
18
9
9
4
10

```


## C#

```c#
class Program
{
    static void Main(string[] args)
    {
        Random random = new Random();
        while (true)
        {
            int a = random.Next(20);
            Console.WriteLine(a);
            if (a == 10)
                break;
            int b = random.Next(20)
            Console.WriteLine(b);
        }

        Console.ReadLine();
    }
}
```



## C++


```cpp
#include <iostream>
#include <ctime>
#include <cstdlib>

int main(){
	srand(time(NULL)); // randomize seed
	while(true){
		const int a = rand() % 20; // biased towards lower numbers if RANDMAX % 20 > 0
		std::cout << a << std::endl;
		if(a == 10)
			break;
		const int b = rand() % 20;
		std::cout << b << std::endl;
	}
	return 0;
}
```



## Chapel


```chapel
use Random;

var r = new RandomStream();
while true {
        var a = floor(r.getNext() * 20):int;
        writeln(a);
        if a == 10 then break;
        var b = floor(r.getNext() * 20):int;
        writeln(b);
}
delete r;
```



## Chef

"Liquify" is now depreciated in favor of "Liquefy", but my interpreter/compiler ([http://search.cpan.org/~smueller/Acme-Chef/ Acme::Chef]) works only with "Liquify" so that's how I'm leaving it. At least it'll work no matter which version you use.
<div style='width:full;overflow:scroll'>

```Chef
Healthy Vita-Sauce Loop - Broken.

Makes a whole lot of sauce for two people.

Ingredients.
0 g Vitamin A
1 g Vitamin B
2 g Vitamin C
3 g Vitamin D
4 g Vitamin E
5 g Vitamin F
6 g Vitamin G
7 g Vitamin H
8 g Vitamin I
9 g Vitamin J
10 g Vitamin K
11 g Vitamin L
12 g Vitamin M
13 g Vitamin N
14 g Vitamin O
15 g Vitamin P
16 g Vitamin Q
17 g Vitamin R
18 g Vitamin S
19 g Vitamin T
20 g Vitamin U
21 g Vitamin V
22 g Vitamin W
32 g Vitamin X
24 g Vitamin Y
25 g Vitamin Z

Method.
Liquify Vitamin X.
Put Vitamin N into 1st mixing bowl.
Fold Vitamin Y into 1st mixing bowl.
Liquify Vitamin Y.
Clean 1st mixing bowl.
Put Vitamin K into 1st mixing bowl.
Fold Vitamin Z into 1st mixing bowl.
Liquify Vitamin Z.
Clean 1st mixing bowl.
Put Vitamin Y into 4th mixing bowl.
Put Vitamin Z into 4th mixing bowl.
Pour contents of the 4th mixing bowl into the 2nd baking dish.
Put Vitamin A into 2nd mixing bowl. Put Vitamin B into 2nd mixing bowl. Put Vitamin C into 2nd mixing bowl. Put Vitamin D into 2nd mixing bowl. Put Vitamin E into 2nd mixing bowl. Put Vitamin F into 2nd mixing bowl. Put Vitamin G into 2nd mixing bowl. Put Vitamin H into 2nd mixing bowl. Put Vitamin I into 2nd mixing bowl. Put Vitamin J into 2nd mixing bowl. Put Vitamin K into 2nd mixing bowl. Put Vitamin L into 2nd mixing bowl. Put Vitamin M into 2nd mixing bowl. Put Vitamin N into 2nd mixing bowl. Put Vitamin O into 2nd mixing bowl. Put Vitamin P into 2nd mixing bowl. Put Vitamin Q into 2nd mixing bowl. Put Vitamin R into 2nd mixing bowl. Put Vitamin S into 2nd mixing bowl. Put Vitamin T into 2nd mixing bowl.
Verb the Vitamin V.
Mix the 2nd mixing bowl well.
Fold Vitamin U into 2nd mixing bowl.
Put Vitamin U into 3rd mixing bowl.
Remove Vitamin K from 3rd mixing bowl.
Fold Vitamin V into 3rd mixing bowl.
Put Vitamin X into 1st mixing bowl.
Put Vitamin V into 1st mixing bowl.
Verb until verbed.
Pour contents of the 1st mixing bowl into the 1st baking dish.

Serves 2.
```

</div>


## Clojure


```lisp
(loop [[a b & more] (repeatedly #(rand-int 20))]
  (println a)
  (when-not (= 10 a)
    (println b)
    (recur more)))
```



## COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Random-Nums.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Num  PIC Z9.

       PROCEDURE DIVISION.
       Main.
           PERFORM FOREVER
               PERFORM Generate-And-Display-Num

               IF Num = 10
                   EXIT PERFORM
               ELSE
                   PERFORM Generate-And-Display-Num
               END-IF
           END-PERFORM

           GOBACK
           .

       Generate-And-Display-Num.
           COMPUTE Num =  FUNCTION REM(FUNCTION RANDOM * 100, 20)
           DISPLAY Num
           .
```



## CoffeeScript

We can use print from the Rhino JavaScript shell as in the JavaScript example or console.log, with a result like this:

```coffeescript

loop
  print a = Math.random() * 20 // 1
  break if a == 10
  print Math.random() * 20 // 1

```



## ColdFusion


```cfm

<Cfset randNum = 0>
<cfloop condition="randNum neq 10">
  <Cfset randNum = RandRange(0, 19)>
  <Cfoutput>#randNum#</Cfoutput>
  <Cfif randNum eq 10><cfbreak></Cfif>
  <Cfoutput>#RandRange(0, 19)#</Cfoutput>
  <Br>
</cfloop>

```

My first two test outputs (I swear this is true)
<pre style="height:25ex;overflow:scroll">
6 0
9 6
12 3
6 0
14 10
19 12
18 14
19 8
3 2
19 1
11 12
16 9
11 15
3 19
13 8
6 4
4 4
13 17
16 9
5 12
12 6
4 14
1 10
3 7
11 15
11 8
0 16
16 14
8 14
11 10
8 8
16 11
4 7
19 10
8 2
15 11
18 10
1 2
18 9
4 9
6 6
11 8
14 6
17 15
13 2
2 0
2 17
8 17
18 13
11 5
15 18
17 8
15 3
7 17
7 13
15 14
11 9
10

```


```txt

10

```



## Common Lisp


```lisp
(loop for a = (random 20)
      do (print a)
      until (= a 10)
      do (print (random 20)))
```



## D


```d
import std.stdio, std.random;

void main() {
    while (true) {
        int r = uniform(0, 20);
        write(r, " ");
        if (r == 10)
            break;
        write(uniform(0, 20), " ");
    }
}
```

```txt
2 4 9 5 3 7 4 4 14 14 3 7 13 8 13 6 10
```



## dc

```dc
1 ss  [s = seed of the random number generator]sz
0k    [scale = 0]sz

[Function r: Push a random number from 0 to 20.]sz
[
 [2Q]SA
 [
  [Formula (from POSIX) for random numbers of low quality.]sz
  ls 1103515245 * 12345 + 4294967296 % d ss  [Compute next s]sz
  65536 /     [it = s / 65536]sz
  d 16 !>A    [Break loop if 16 <= it]sz
  sz 0 0 =B   [Forget it, continue loop]sz
 ]SB 0 0 =B
 20 %         [Push it % 20]sz
 LA sz LB sz  [Restore A, B]sz
]sr


[2Q]sA
[
 0 0 =r p     [Print 1st number.]sz
 10 =A        [Break if 10 == it.]sz
 0 0 =r p sz  [Print 2nd number.]sz
 0 0 =B       [Continue loop.]sz
]sB 0 0 =B
```



## Delphi



```Delphi
program Project5;

{$APPTYPE CONSOLE}

var
  num:Integer;
begin
  Randomize;
  while true do
  begin
    num:=Random(20);
    Writeln(num);
    if num=10 then break;
  end;
end.


```



## DWScript



```delphi

while True do begin
   var num := RandomInt(20);
   PrintLn(num);
   if num=10 then Break;
end;
```



## E


```e
while (true) {
    def a := entropy.nextInt(20)
    print(a)
    if (a == 10) {
        println()
        break
    }
    println(" ", entropy.nextInt(20))
}
```



## EasyLang

<lang>repeat
  a = random 20
  print a
  until a = 10
  print random 20
.
```



## Ela


This implementation uses .NET Framework Math.Randomize function.
Current ticks multiplied by an iteration index are used as a seed.
As a result, an output looks almost truly random:


```ela
open datetime random monad io

loop = loop' 1
       where loop' n t = do
                dt <- datetime.now
                seed <- return <| toInt <| (ticks <| dt) * n
                r <- return $ rnd seed 0 19
                putStrLn (show r)
                if r <> t then loop' (n + 1) t else return ()


loop 10 ::: IO
```



## Elixir

```elixir
defmodule Loops do
  def break, do: break(random)

  defp break(10), do: IO.puts 10
  defp break(r) do
    IO.puts "#{r},\t#{random}"
    break(random)
  end

  defp random, do: Enum.random(0..19)
end

Loops.break
```


```txt

13,     7
12,     7
2,      16
3,      19
17,     10
5,      17
14,     0
7,      6
5,      19
5,      12
4,      2
8,      14
1,      17
13,     5
10

```



## Erlang


```erlang
%% Implemented by Arjun Sunel
-module(forever).
-export([main/0, for/0]).

main() ->
	for().

for() ->
	K = random:uniform(19),
        io:fwrite( "~p ", [K] ),
	if  K==10 ->
		ok;
	true ->
		M = random:uniform(19),
		io:format("~p~n",[M]),
   		for()
	end.

```



## ERRE


```ERRE

LOOP
    A=INT(RND(1)*20)
    PRINT(A)
    IF A=10 THEN EXIT LOOP END IF !EXIT FOR works the same inside FOR loops
    PRINT(INT(RND(1)*20))
END LOOP

```

The <code>RND(X)</code> function returns a random integer from 0 to 1. X is a dummy argument.


## Euphoria


```euphoria
integer i
while 1 do
    i = rand(20) - 1
    printf(1, "%g ", {i})
    if i = 10 then
        exit
    end if
    printf(1, "%g ", {rand(20)-1})
end while
```

The <code>rand()</code> function returns a random integer from 1 to the integer provided.


## F#


```F#

let mutable a=21
let mutable b=22
let mutable c=23
while(a<>10) do
    b <- (new System.Random()).Next(0, 20)
    if(a<>b) then
        printf "%i " b
    c <- (new System.Random(b)).Next(0, 20)
    if(b<>10) then
        if(a<>b) then
            printfn "%i " c
    a<-b

```



## Factor

Using <code>with-return</code>:

```factor
[
    [ 20 random [ . ] [ 10 = [ return ] when ] bi 20 random . t ] loop
] with-return
```


Idiomatic Factor:

```factor
[ 20 random [ . ] [ 10 = not ] bi dup [ 20 random . ] when ] loop
```



## Fantom



```fantom

class ForBreak
{
  public static Void main ()
  {
    while (true)
    {
      a := Int.random(0..19)
      echo (a)
      if (a == 10) break
      echo (Int.random(0..19))
    }
  }
}

```



## Forth


```forth
include random.fs

: main
  begin  20 random dup . 10 <>
  while  20 random .
  repeat ;

\ use LEAVE to break out of a counted loop
: main
  100 0 do
    i random dup .
    10 = if leave then
    i random .
  loop ;
```



## Fortran

```fortran
program Example
  implicit none

  real :: r
  integer :: a, b

  do
     call random_number(r)
     a = int(r * 20)
     write(*,*) a
     if (a == 10) exit
     call random_number(r)
     b = int(r * 20)
     write(*,*) b
  end do

end program Example
```


```fortran
      PROGRAM LOOPBREAK
        INTEGER I, RNDINT

C       It doesn't matter what number you put here.
        CALL SDRAND(123)

C       Because FORTRAN 77 semantically lacks many loop structures, we
C       have to use GOTO statements to do the same thing.
   10   CONTINUE
C         Print a random number.
          I = RNDINT(0, 19)
          WRITE (*,*) I

C         If the random number is ten, break (i.e. skip to after the end
C         of the "loop").
          IF (I .EQ. 10) GOTO 20

C         Otherwise, print a second random number.
          I = RNDINT(0, 19)
          WRITE (*,*) I

C         This is the end of our "loop," meaning we jump back to the
C         beginning again.
          GOTO 10

   20   CONTINUE

        STOP
      END

C FORTRAN 77 does not come with a random number generator, but it
C is easy enough to type "fortran 77 random number generator" into your
C preferred search engine and to copy and paste what you find. The
C following code is a slightly-modified version of:
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


Anyone who attempts to produce random numbers via a computation is already in a state of sin, so, one might as well be hung as a goat rather than as a lamb. Here is a version using the RANDU generator, in the style of Fortran 66 as offered by the IBM1130. No logical-if statements and reliance on implicit type declarations. Sixteen-bit integers result. The standard advice is to start IX off as an odd number. Note that RANDU does ''not'' update IX (the "seed"); the caller must do so. Since integer overflow producing negative numbers is undone by adding 32768 (trusting that the compiler will not attempt to combine constants, thus + 32767 + 1) in the absence of an AND operation, possible values for IY are presumably zero to 32767. Since IY is divided by 32767.0 (''not'' 32768.0 for example), the range for YFL is zero to one ''inclusive'', though further inspection shows that zero is not attained for proper starts - should IX be zero it will never change, thus the span is (0,1]; a more common arrangement is [0,1).

Because the upper bound ''is'' attainable, multiplying YFL by 19 and truncating the result will mean that 19 appears only as an edge event when IY = 32767. Multiplying by 20 will ensure that 19 gets its fair share along with each other integer, but, the edge event might now occasionally produce a 20. There is no MIN function available, so, explicit testing results. Rather than repeat this code with its consequent litter of labels, a helper function IR19 does the work once. These out-by-one opportunities are vexing.

The RANDU routine is so notorious that latter-day compilers can supply their own RANDU (using a better method), and further, disregard a user-supplied RANDU routine so it may have to be called RANDUU or some other name!

```Fortran

      SUBROUTINE RANDU(IX,IY,YFL)
Copied from the IBM1130 Scientific Subroutines Package (1130-CM-02X): Programmer's Manual, page 60.
CAUTION! This routine's 32-bit variant is reviled by Prof. Knuth and many others for good reason!
        IY = IX*899
        IF (IY) 5,6,6
    5   IY = IY + 32767 + 1
    6   YFL = IY
        YFL = YFL/32767.
      END

      FUNCTION IR19(IX)
        CALL RANDU(IX,IY,YFL)
        IX = IY
        I = YFL*20
        IF (I - 20) 12,11,11
   11   I = 19
   12   IR19 = I
      END

      IX = 1
Commence the loop.
   10 I = IR19(IX)
      WRITE (6,11) I
   11 FORMAT (I3)
      IF (I - 10) 12,20,12
   12 I = IR19(IX)
      WRITE (6,11) I
      GO TO 10
Cease.
   20 CONTINUE
      END

```

Output, converted to along the line:
  0 13  4 19  1  7  2 12  4  7 14 11  6  4  0  9  5 12 16 19 18  2  0 13  2  7 10
This source will compile with later compilers (possibly after adding INTEGER*2 declarations to not use larger integers), as well as earlier compilers. But the IBM1620's Fortran II ran on a decimal computer (and the compiler allowed an option to specify how many digits in a number) so the assumption of sixteen-bit two's-complement arithmetic would fail. There was once much more variety in computer design, not just always a power of two in word sizes.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim i As Integer
Randomize
Do
  i = Int(Rnd * 20)
  Print Using "##"; i;
  Print "  ";
  If i = 10 Then Exit Do
  i = Int(Rnd * 20)
  Print Using "##"; i;
  Print"  ";
Loop

Print
Sleep
```


Sample output

```txt

 6  12   2  16   5  19   9   6  16   1  16  10   1   4  18   3   2   9  19   0
19  13   0   0  12  17  13  12  18  10   8  13   9   5  14   7  10

```



## FutureBasic


```futurebasic

include "ConsoleWindow"
randomize
dim as short stopGo, goOn

while ( stopGo != 10 )
   stopGo = rnd(19) : print "stopGo ="; stopGo,
   goOn   = rnd(19) : print "goOn ="; goOn
wend

```

Sample output:

```txt

stopGo = 14     goOn = 17
stopGo = 5      goOn = 8
stopGo = 18     goOn = 1
stopGo = 1      goOn = 11
stopGo = 13     goOn = 3
stopGo = 16     goOn = 6
stopGo = 14     goOn = 4
stopGo = 7      goOn = 17
stopGo = 14     goOn = 9
stopGo = 8      goOn = 2
stopGo = 15     goOn = 19
stopGo = 17     goOn = 2
stopGo = 13     goOn = 13
stopGo = 8      goOn = 1
stopGo = 11     goOn = 19
stopGo = 3      goOn = 4
stopGo = 14     goOn = 2
stopGo = 18     goOn = 7
stopGo = 15     goOn = 17
stopGo = 15     goOn = 15
stopGo = 15     goOn = 8
stopGo = 12     goOn = 9
stopGo = 2      goOn = 8
stopGo = 12     goOn = 12
stopGo = 10     goOn = 4

```



## Gambas


```gambas
Public Sub Form_Open()
Dim iRand As Integer

Repeat
  iRand = Rnd * 20
  Print iRand
Until iRand = 10

End
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=65d2287312298a938e7e8eea8899e38b Click this link to run this code]'''

```gambas
Public Sub Main()
Dim byNo As Byte

Do
  byNo = Rand(0, 19)
  Print byNo;;
  If byNo = 10 Then Break
  byNo = Rand(0, 19)
  Print byNo;;
Loop

End
```

Output:

```txt

0 5 12 8 1 13 16 5 4 11 5 7 15 12 16 7 9 10 13 19 4 10 2 13 16 7 0 1 16 3 17 10 0 16 14 0 0 8 6 2 1 5 9 12 2 18 15 1 1 17 9 18 8 17 19 12 6 19 9 5 15 1 2 7 2 11 18 1 15 19 10

```



## GAP


```gap
while true do
    a := Random(0, 19);
    Print(a);
    if a = 10 then
        Print("\n");
        break;
    fi;
    a := Random(0, 19);
    Print("\t", a, "\n");
od;

# 11      6
# 5       8
# 1       4
# 5       10
# 1       16
# 10
```



## GML


```GML
while(1)
    {
    a = floor(random(19))
    show_message(string(a))
    if(a = 10)
        break
    b = floor(random(19))
    show_message(string(a))
    }

```



## Go


```go
package main

import "fmt"
import "math/rand"
import "time"

func main() {
    rand.Seed(time.Now().UnixNano())
    for {
        a := rand.Intn(20)
        fmt.Println(a)
        if a == 10 {
            break
        }
        b := rand.Intn(20)
        fmt.Println(b)
    }
}
```



## Groovy


```groovy
final random = new Random()

while (true) {
    def random1 = random.nextInt(20)
    print random1
    if (random1 == 10) break
    print '     '
    println random.nextInt(20)
}
```


=={{header|GW-BASIC}}==

```qbasic
10 NUM = 0
20 WHILE NUM <> 10
30     NUM = INT(RND * 20)
40     PRINT NUM
50 WEND
```



## Harbour


```visualfoxpro
PROCEDURE Loop()

   LOCAL n

   DO WHILE .T.
      ? n := hb_RandomInt( 0, 19 )
      IF n == 10
         EXIT
      ENDIF
      ? hb_RandomInt( 0, 19 )
   ENDDO

   RETURN
```



## Haskell


```haskell
import Control.Monad
import System.Random

loopBreak n k = do
  r <- randomRIO (0,n)
  print r
  unless (r==k) $ do
    print =<< randomRIO (0,n)
    loopBreak n k
```

Use:

```haskell>loopBreak 19 10</lang



## hexiscript


```hexiscript
while true
  let r rand 20
  println r
  if r = 10
    break
  endif
  println rand 20
endwhile
```



## HicEst


```hicest
1  DO i = 1, 1E20 ! "forever"
     a = INT( RAN(10, 10) )
     WRITE(name) a
     IF( a == 10) GOTO 10
     b = INT( RAN(10, 10) )
     WRITE(name) b
   ENDDO
10
 END
```



## HolyC



```holyc
U16 a, b;
while (1) {
  a = RandU16 % 20;
  Print("%d\n", a);

  if (a == 10) break;

  b = RandU16 % 20;
  Print("%d\n", b);
}

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
    while 10 ~= writes(?20-1) do write(", ",?20-1)
end
```

Notes:
* For any positive integer i, ?i produces a value j where 1 <= j <= i
* Although this can be written with a break (e.g. repeat expression & break), there is no need to actually use one. (And it's ugly).
* Programmers new to Icon/Unicon need to understand that just about everything returns values including comparison operators, I/O functions like write/writes.
* This program will perform similarly but not identically under Icon and Unicon because the random operator ?i behaves differently.  While both produce pseudo-random numbers a different generator is used.  Also, the sequence produced by Icon begins with the same seed value and is repeatable whereas the sequence produced by Unicon does not.  One way to force Icon to use different random sequences on each call would be to add the line
```Icon
&random := integer(map("smhSMH","Hh:Mm:Ss",&clock))
```
 at the start of the <tt>main</tt> procedure to set the random number seed based on the time of day.


## Io


```io
loop(
    a := Random value(0,20) floor
    write(a)
    if( a == 10, writeln ; break)
    b := Random value(0,20) floor
    writeln(" ",b)
)
```



## J


```j
loopexample=: verb define
  while. 1 do.
    smoutput n=. ?20
    if. 10=n do. return. end.
    smoutput ?20
  end.
)
```


Note that <code>break.</code> could have been used in place of <code>return.</code>.


## Java


```java
import java.util.Random;

Random rand = new Random();
while(true){
    int a = rand.nextInt(20);
    System.out.println(a);
    if(a == 10) break;
    int b = rand.nextInt(20);
    System.out.println(b);
}
```



## JavaScript


```javascript
for (;;) {
  var a = Math.floor(Math.random() * 20);
  print(a);
  if (a == 10)
    break;
  a = Math.floor(Math.random() * 20);
  print(a);
}
```

The <code>print()</code> function is available in the [[Rhino]] JavaScript shell.


If we step back for a moment from imperative assumptions about repetitive processes and their interruption, we may notice that there is actually no necessary connection between repetitive process and loops.

In a functional idiom of JavaScript, we might instead write something like:


```JavaScript
(function streamTillInitialTen() {
    var nFirst = Math.floor(Math.random() * 20);

    console.log(nFirst);

    if (nFirst === 10) return true;

    console.log(
        Math.floor(Math.random() * 20)
    );

    return streamTillInitialTen();
})();
```


Obtaining runs like:


```txt
18
10
16
10
8
0
13
3
2
14
15
17
14
7
10
8
0
2
0
2
5
16
3
16
6
7
19
0
16
9
7
11
17
10
```


Though returning a value composes better, and costs less IO traffic, than firing off side-effects from a moving thread:


```JavaScript
console.log(
  (function streamTillInitialTen() {
    var nFirst = Math.floor(Math.random() * 20);

    if (nFirst === 10) return [10];

    return [
      nFirst,
      Math.floor(Math.random() * 20)
    ].concat(
      streamTillInitialTen()
    );
  })().join('\n')
);
```


Sample result:

```txt
17
14
3
4
13
10
15
5
10
```



## jq


With the functions defined below, the task can be accomplished using the following jq filter:

    take( rand(20); . != 10 )

Here, `rand(n)` is a pseudo-random number generator, and `take(stream; cond)` will continue taking from the stream so long as the condition is satisfied.  When the condition is no longer satisfied, the PRNG is immediately terminated.

Using the built-in `foreach` construct, the above is equivalent to:

    label $done | foreach rand(20) as $n (null; $n; if . == 10 then break $done else . end)

'''PRNG'''

Currently, jq does not have a built-in random-number generator, so here we borrow one of the linear congruential generators defined at https://rosettacode.org/wiki/Linear_congruential_generator -

```jq
# 15-bit integers generated using the same formula as rand()
# from the Microsoft C Runtime.
# Input: [ count, state, rand ]
def next_rand_Microsoft:
  .[0] as $count | .[1] as $state
  | ( (214013 * $state) + 2531011) % 2147483648 # mod 2^31
  | [$count+1 , ., (. / 65536 | floor) ];

def rand_Microsoft(seed):
  [0,seed]
  | next_rand_Microsoft  # the seed is not so random
  | recurse( next_rand_Microsoft )
  | .[2];

# Generate random integers from 0 to (n-1):
def rand(n): n * (rand_Microsoft(17) / 32768) | trunc;
```


'''"take"'''


```jq
def take(s; cond):
  label $done
  | foreach s as $n (null; $n; if $n | cond | not then break $done else . end);
```


'''"count"'''

Since the PRNG used here is deterministic, we'll just count the number of integers generated:

```jq
def count(s): reduce s as $i (0; . + 1);
```


'''Example'''
    count(take(rand(20); . != 10))
    12


## Julia


```Julia

while true
    n = rand(0:19)
    @printf "%4d" n
    if n == 10
        println()
        break
    end
    n = rand(0:19)
    @printf "%4d\n" n
end

```

```txt

   0  11
  11   7
   4  19
   7  19
   5   2
   5  17
  12   5
  14  18
   1  10
  18  14
  16   0
  17   1
  10

```



## Kotlin

```scala
import java.util.Random

fun main(args: Array<String>) {
    val rand = Random()
    while (true) {
        val a = rand.nextInt(20)
        println(a)
        if (a == 10) break
        println(rand.nextInt(20))
    }
}
```



## Lang5


```lang5>do 20 ? int dup . 10 == if break then 20 ? int . loop</lang



## Lasso


```Lasso
local(x = 0)
while(#x != 10) => {^
	#x = integer_random(19,0)
	#x
	#x == 10 ? loop_abort
	', '+integer_random(19,0)+'\r'
^}
```



## Liberty BASIC

The task specifies a "number".

```lb>while num<
10
    num=rnd(1)*20
    print num
    if num=10 then exit while
    print rnd(1)*20
wend

```
If "integer" was meant, this code fulfils that requirement.

```lb>while num<
10
    num=int(rnd(1)*20)
    print num
    if num=10 then exit while
    print int(rnd(1)*20)
wend

```



## Lingo


```lingo
repeat while TRUE
  n = random(20)-1
  put n
  if n = 10 then exit repeat
  put random(20)-1
end repeat
```



## Lisaac


```Lisaac
Section Header

+ name := TEST_LOOP_BREAK;

Section Public

- main <- (
  + a, b : INTEGER;

  `srand(time(NULL))`;
  {
    a := `rand()`:INTEGER % 20; // not exactly uniformly distributed, but doesn't matter
    a.print;
    '\n'.print;
    a == 10
  }.until_do {
    b := `rand()`:INTEGER % 20; // not exactly uniformly distributed, but doesn't matter
    b.print;
    '\n'.print;
  }
);
```



## LiveCode


```LiveCode
command loopForeverRandom
    repeat forever
        put random(20) - 1 into tRand
        put tRand
        if tRand is 10 then exit repeat
        put random(20) - 1
    end repeat
end loopForeverRandom

```



## Lua


```lua
repeat
  k = math.random(19)
  print(k)
  if k == 10 then break end
  print(math.random(19)
until false
```




## M2000 Interpreter

We use block of module to loop. Break also can be used, but breaks nested blocks (without crossing modules/functions). Using break in second Checkit module we break three blocks.

```M2000 Interpreter

Module Checkit {
      M=Random(0, 19)
      Print M
      If M=10 then Continue  ' because loop flag is false, continue act as Exit
      Print Random(0, 19)
      loop
}
Checkit

Module Checkit {
      do {
            do {
                  {
                        M=Random(0, 19)
                        Print M
                        If M=10 then Break
                        Print Random(0, 19)
                        loop
                  }
                  Print "no print this"
            } always
            Print "no print this"
      } always
      Print "print ok"
}
Checkit

```



## M4


```M4
define(`randSeed',141592653)dnl
define(`setRand',
   `define(`randSeed',ifelse(eval($1<10000),1,`eval(20000-$1)',`$1'))')dnl
define(`rand_t',`eval(randSeed^(randSeed>>13))')dnl
define(`random',
   `define(`randSeed',eval((rand_t^(rand_t<<18))&0x7fffffff))randSeed')dnl
dnl
define(`loopbreak',`define(`a',eval(random%20))`a='a
ifelse(a,10,`',`define(`b',eval(random%20))`b='b
loopbreak')')dnl
dnl
loopbreak
```


```txt

a=17
b=3
a=0
b=15
a=10

```



## Maple


```Maple
r := rand( 0 .. 19 ):
do
        n := r();
        printf( "%d\n", n );
        if n = 10 then
                break
        end if;
        printf( "%d\n", r() );
end do:
```



## Mathematica


```Mathematica
While[(Print[#];#!=10)&[RandomIntger[{0,19}]],
         Print[RandomInteger[{0,19}]
        ]
```



## Maxima


```maxima
/* To exit the innermost block, use return(<value>) */

block([n],
   do (
      n: random(20),
      ldisp(n),
      if n = 10 then return(),
      n: random(20),
      ldisp(n)
   )
)$

/* To exit any level of block, use catch(...) and throw(<value>);
they are not used for catching exceptions, but for non-local
return. Use errcatch(...) for exceptions. */

block([n],
   catch(
      do (
         n: random(20),
         ldisp(n),
         if n = 10 then throw('done),
         n: random(20),
         ldisp(n)
      )
   )
)$

/* There is also break(<value>, ...) in Maxima. It makes Maxima
stop the evaluation and enter a read-eval loop where one can change
variable values, then return to the function after exit; For example */

block([x: 1], break(), ldisp(x));
> x: 2;
> exit;
2
```



## MAXScript


```MAXScript

while true do
(
	a = random 0 19
	format ("A: % \n") a
	if a == 10 do exit
	b = random 0 19
	format ("B: % \n") b
)

```


=={{header|MK-61/52}}==
<lang>СЧ	2	0	*	П0
1	0	-	[x]	x#0	18
СЧ	2	0	*	П1
БП	00	ИП0	С/П
```


=={{header|Modula-3}}==

```modula3
MODULE Break EXPORTS Main;

IMPORT IO, Fmt, Random;

VAR a,b: INTEGER;

BEGIN
  WITH rand = NEW(Random.Default).init() DO
    LOOP
      a := rand.integer(min := 0, max := 19);
      IO.Put(Fmt.Int(a) & "\n");
      IF a = 10 THEN EXIT END;
      b := rand.integer(min := 0, max := 19);
      IO.Put(Fmt.Int(b) & "\n");
    END;
  END;
END Break.
```



## MOO


```moo
while (1)
  a = random(20) - 1;
  player:tell(a);
  if (a == 10)
    break;
  endif
  b = random(20) - 1;
  player:tell(b);
endwhile
```



## MUMPS


```MUMPS
BREAKLOOP
 NEW A,B
 SET A=""
 FOR  Q:A=10  DO
 .SET A=$RANDOM(20)
 .WRITE !,A
 .Q:A=10
 .SET B=$RANDOM(20)
 .WRITE ?6,B
 KILL A,B
 QUIT
 ;A denser version that doesn't require two tests
 NEW A,B
 FOR  SET A=$RANDOM(20) WRITE !,A QUIT:A=10  SET B=$RANDOM(20) WRITE ?6,B
 KILL A,B QUIT
```

```txt
USER>D BREAKLOOP^ROSETTA

5     3
9     13
3     12
9     19
16    4
11    17
18    2
4     18
10
USER>D BREAKLOOP+11^ROSETTA

6     13
15    3
0     8
8     18
7     13
15    10
15    13
10
```



## Neko


```ActionScript
/**
 Loops/Break in Neko
 Tectonics:
   nekoc loops-break.neko
   neko loops-break
*/

var random_new = $loader.loadprim("std@random_new", 0);
var random_int = $loader.loadprim("std@random_int", 2);

var random = random_new();

while true {
  var r = random_int(random, 20);
  $print(r, " ");

  if r == 10 break;

  r = random_int(random, 20);
  $print(r, " ");
}
$print("\n");
```


```txt
prompt$ nekoc loops-break.neko
prompt$ neko loops-break
0 8 17 12 4 18 7 6 19 11 13 6 12 7 6 6 6 18 14 7 18 10 15 6 9 5 4 14 10
```



## Nemerle

```Nemerle
using System;
using System.Console;
using Nemerle.Imperative;

module Break
{
    Main() : void
    {
        def rnd = Random();
        while (true)
        {
            def a = rnd.Next(20);
            WriteLine(a);
            when (a == 10) break;
            def b = rnd.Next(20);
            WriteLine(b);
        }
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

  say
  say 'Loops/Break'
  rn = Rexx
  rnd = Random()

  loop label lb forever
    rn = rnd.nextInt(19)
    say rn.right(3)'\-'
    if rn = 10 then leave lb
    rn = rnd.nextInt(19)
    say rn.right(3)'\-'
    end lb
  say

```



## NewLISP


```NewLISP
(until (= 10 (println (rand 20)))
  (println (rand 20)))
```



## Nim

```nim
import math

while true:
  let a = random(20)
  echo a
  if a == 10:
    break
  let b = random(20)
  echo b
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 I=RND(20)
20 PRINT I
30 IF I=10 THEN STOP
40 PRINT RND(20)
50 GOTO 10
```


=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE LoopBreak;
IMPORT
  RandomNumbers,
  Out;

PROCEDURE Do();
VAR
  rn: LONGINT;
BEGIN
  LOOP
    rn := RandomNumbers.RND(20);
    Out.LongInt(rn,0);Out.Ln;
    IF rn = 10 THEN EXIT END;
    rn := RandomNumbers.RND(20);
    Out.LongInt(rn,0);Out.Ln
  END
END Do;

BEGIN
  Do
END LoopBreak.

```



## Objeck


```objeck

while(true) {
  a := (Float->Random() * 20.0)->As(Int);
  a->PrintLine();
  if(a = 10) {
    break;
  };
  a := (Float->Random() * 20.0)->As(Int);
  a->PrintLine();
}

```



## OCaml


```ocaml
# Random.self_init();;
- : unit = ()

# while true do
    let a = Random.int 20 in
    print_int a;
    print_newline();
    if a = 10 then raise Exit;
    let b = Random.int 20 in
    print_int b;
    print_newline()
  done;;
15
18
2
13
10
Exception: Pervasives.Exit.
```



## Octave


```octave
while(1)
  a = floor(unifrnd(0,20, 1));
  disp(a)
  if ( a == 10 )
    break
  endif
  b = floor(unifrnd(0,20, 1));
  disp(b)
endwhile
```



## Oforth


```Oforth
while(true) [
      19 rand dup print ":" print
      10 == ifTrue: [ break ]
      19 rand print " " print
   ]
```



## Ol


```scheme

(import (otus random!))

(call/cc (lambda (break)
   (let loop ()
      (if (= (rand! 20) 10)
         (break #t))
      (print (rand! 20))
      (loop))))

```



## ooRexx


```ooRexx
/*REXX ****************************************************************
* Three Ways to leave a Loop
* ooRexx added the possibility to leave an outer loop
* without using a control variable
* 12.05.2013 Walter Pachl
**********************************************************************/
do i1=1 To 2                           /* an outer loop              */
  Say 'i1='i1                          /* tell where we are          */
  Call random ,,123                    /* seed to be reproducable    */
  do forever                           /* inner loop                 */
    a=random(19)
    Say a
    if a=6  then leave                 /* leaces the innermost loop  */
    end
  end

do i2=1 To 2
  Say 'i2='i2
  Call random ,,123
  do forever
    a=random(19)
    Say a
    if a=6  then leave i2    /* leaves loop with control variable i2 */
    end
  end

Parse Version v
Select
  When pos('ooRexx',v)>0 Then supported=1
  Otherwise                   supported=0
  End
If supported Then Do
  Say 'Leave label-name is supported in' v
do Label i3 Forever
  Say 'outer loop'
  Call random ,,123
  do forever
    a=random(19)
    Say a
    if a=6  then leave i3          /* leaves loop with label name i3 */
    end
  end
End
Else
  Say 'Leave label-name is probably not supported in' v
```

```txt
i1=1
14
14
5
6
i1=2
14
14
5
6
i2=1
14
14
5
6
Leave label-name is supported in REXX-ooRexx_4.1.2(MT) 6.03 28 Aug 2012
outer loop
14
14
5
6

```



## Oz

We can implement this either with recursion or with a special type of the for-loop. Both can be considered idiomatic.

```oz
for break:Break do
   R = {OS.rand} mod 20
in
   {Show R}
   if R == 10 then {Break}
   else {Show {OS.rand} mod 20}
   end
end
```



## PARI/GP


```parigp
while(1,
  t=random(20);
  print(t);
  if(t==10, break);
  print(random(20))
)
```



## Pascal

See [[Loops/Break#Delphi | Delphi]]


## Perl


```perl
while (1) {
    my $a = int(rand(20));
    print "$a\n";
    if ($a == 10) {
        last;
    }
    my $b = int(rand(20));
    print "$b\n";
}
```



## Perl 6

```perl6
loop {
    say my $n = (0..19).pick;
    last if $n == 10;
    say (0..19).pick;
}
```



## Phix

The rand() function returns a random integer from 1 to the integer provided.

```Phix
integer i
while 1 do
    i = rand(20)-1
    printf(1, "%g ", {i})
    if i=10 then exit end if
    printf(1, "%g\n", {rand(20)-1})
end while
```

```txt

2 10
1 7
3 16
10

```



## PHP


```php
while (true) {
    $a = rand(0,19);
    echo "$a\n";
    if ($a == 10)
        break;
    $b = rand(0,19);
    echo "$b\n";
}
```



## PicoLisp

Literally:

```PicoLisp
(use R
   (loop
      (println (setq R (rand 1 19)))
      (T (= 10 R))
      (println (rand 1 19)) ) )
```

Shorter:

```PicoLisp
(until (= 10 (println (rand 1 19)))
   (println (rand 1 19)) )
```



## Pike


```pike
int main(){
   while(1){
      int a = random(20);
      write(a + "\n");
      if(a == 10){
         break;
      }
      int b = random(20);
      write(b + "\n");
   }
}
```



## PL/I


```PL/I

do forever;
   k = trunc(random()*20);
   put (k);
   if k = 10 then leave;
   k = trunc(random()*20);
   put skip list (k);
end;

```



## PostScript


```postscript
realtime srand          % init RNG
{
    rand 20 mod         % generate number between 0 and 19
    dup =               % print it
    10 eq { exit } if   % exit if 10
} loop
```



## PowerShell


```powershell
$r = New-Object Random
for () {
    $n = $r.Next(20)
    Write-Host $n
    if ($n -eq 10) {
        break
    }
    Write-Host $r.Next(20)
}
```



## PureBasic


```PureBasic
If OpenConsole()

  Repeat
    a = Random(19)
    PrintN(Str(a))
    If a = 10
      Break
    EndIf
    b = Random(19)
    PrintN(Str(b))
    PrintN("")
  ForEver

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```



## Python


```python
from random import randrange

while True:
    a = randrange(20)
    print(a)
    if a == 10:
        break
    b = randrange(20)
    print(b)
```



## R

```R
sample0to19 <- function() sample(0L:19L, 1,replace=TRUE)
repeat
{
  result1 <- sample0to19()
  if (result1 == 10L)
  {
    print(result1)
    break
  }
  result2 <- sample0to19()
  cat(result1, result2, "\n")
}
```



## Qi


```qi

(define loop -> (if (= 10 (PRINT (random 20)))
                    true
                    (do (PRINT (random 20))
                        (loop))))
(loop)

```



## Racket


```racket

#lang racket
(let loop ()
  (let/ec break
    (define a (random 20))
    (displayln a)
    (when (= a 10) (break))
    (displayln (random 20))
    (loop)))

```



## REBOL


```REBOL
REBOL [
	Title: "Loop/Break"
	URL: http://rosettacode.org/wiki/Loop/Break
]

random/seed 1 ; Make repeatable.
; random/seed now ; Uncomment for 'true' randomness.

r20: does [(random 20) - 1]

forever [
	prin x: r20
	if 10 = x [break]
	print rejoin [" " r20]
]
print ""
```


```txt
14 11
19 15
6 11
12 11
3 14
10
```



## Retro


```Retro
doc{
A couple of helper functions to make the rest of the
code more readable.
}doc

: rand  ( -n )  random 20 mod ;
: .  ( n- )  putn space ;

doc{
One approach is to use a simple repeat/again loop, and
a conditional exit. For instance:
}doc

: foo   ( - )
  repeat rand dup . 10 = if; rand . again ;

doc{
The other approach uses a structured while loop with the
second printing handled by a conditional clause.
}doc

[ rand dup . 10 <> [ [ rand . ] ifTrue ] sip ] while

```



## REXX


```rexx
/*REXX program demonstrates a    FOREVER   DO  loop  with a test to    LEAVE   (break). */
                                                 /*REXX's RANDOM BIF returns an integer.*/
    do forever                                   /*perform loop until da cows come home.*/
    a=random(19)                                 /*same as:    random(0, 19)            */
    call charout , right(a, 5)                   /*show   A   right─justified, column 1.*/
    if a==10  then leave                         /*is random #=10?  Then cows came home.*/
    b=random(19)                                 /*same as:    random(0, 19)            */
    say right(b, 5)                              /*show   B   right─justified, column 2.*/
    end   /*forever*/                            /* [↑]  CHAROUT , xxx   writes to term.*/
                                                 /*stick a fork in it,  we're all done. */
```

(A long run was chosen)

```txt

    1    0
   16    3
    8   15
   11    8
   12   14
   15    4
    0    0
    6   11
   15    5
   14    0
   18   16
   15    0
   14    5
    3    5
    9    4
    4    4
   17    6
    4   10
    6    2
    9   13
   12    6
   14   16
   17    0
    8    6
    9    2
    0    6
    9    9
   12    8
   11    3
   11    4
    7    1
    3   13
    4    8
   14   14
   14   13
   12    7
    1    0
   16   15
    8   19
   12    7
   18    9
    7   18
   19   13
    6    2
    6    7
    2    1
    8    2
    9    7
    6   13
   19   15
   10

```



## Ring


```ring

while true
      a = random(20)
      see a + nl
      if a = 10 exit ok
end

```



## Ruby


```ruby
loop do
  a = rand(20)
  print a
  if a == 10
    puts
    break
  end
  b = rand(20)
  puts "\t#{b}"
end
```

or

```ruby
loop do
  print a = rand(20)
  puts or break if a == 10
  puts "\t#{rand(20)}"
end
```


```txt

0       4
11      0
8       2
12      13
3       0
6       9
2       8
12      10
8       17
12      6
10

```



## Rust

```rust
// cargo-deps: rand

extern crate rand;
use rand::{thread_rng, Rng};

fn main() {
    let mut rng = thread_rng();
    loop {
        let num = rng.gen_range(0, 20);
        if num == 10 {
            println!("{}", num);
            break;
        }
        println!("{}", rng.gen_range(0, 20));
    }
}
```



## SAS


```sas
data _null_;
do while(1);
   n=floor(uniform(0)*20);
   put n;
   if n=10 then leave;    /* 'leave' to break a loop */
end;
run;
```



## Sather


```sather
-- help class for random number sequence
class RANDOM is
  attr seed:INT;

  create(seed:INT):SAME is
    res:RANDOM := new;
    res.seed := seed;
    return res;
  end;
  -- this code is taken from rand's man (C)
  next:INT is
    seed := seed * 1103515245 + 12345;
    return (seed/65536) % 32768;
  end;
end;

class MAIN is
  main is
    a, b :INT;
    rnd:RANDOM := #(1);
    loop
      a := rnd.next % 20;
      #OUT + a + "\n";
      if a = 10 then break!; end; -- here we break
      b := rnd.next % 20;
      #OUT + b + "\n";
    end;
  end;
end;
```



## Scala


```scala>scala
 import util.control.Breaks.{breakable, break}
import util.control.Breaks.{breakable, break}

scala> import util.Random
import util.Random

scala> breakable {
     |   while(true) {
     |     val a = Random.nextInt(20)
     |     println(a)
     |     if(a == 10)
     |       break
     |     val b = Random.nextInt(20)
     |     println(b)
     |   }
     | }
5
4
10

```



## Scheme


```scheme

(let loop ((first (random 20)))
  (print first)
  (if (not (= first 10))
      (begin
        (print (random 20))
        (loop (random 20)))))

```


Or by using call/cc to break out:


```scheme

(call/cc
 (lambda (break)
   (let loop ((first (random 20)))
     (print first)
     (if (= first 10)
         (break))
     (print (random 20))
     (loop (random 20)))))

```



## Scilab

<lang>while %T
    a=int(rand()*20)  // [0..19]
    printf("%2d ",a)
    if a==10 then break; end
    b=int(rand()*20)
    printf("%2d\n",b)
end
printf("\n")
```

<pre style="height:20ex">
 4 15
 0  6
13 12
16 13
17  1
11 13
14  3
10

```



## Sidef


```ruby
var lim = 20;
loop {
    say (var n = lim.rand.int);
    n == 10 && break;
    say lim.rand.int;
}
```



## Simula

```simula
! Loops/Break - simula67 - 08/03/2017;
begin
  integer num,seed;
  seed:=0;
  while true do
  begin
    num:=randint(1,20,seed);
    outint(num,2); outimage;
    if num=10 then goto lab;
  end;
lab:
end
```

```txt

 1
 9
 8
10

```




## Smalltalk

```smalltalk
[
    |a b done|

    a := Random nextIntegerBetween:0 and:19.
    Stdout print: a; cr.
    (done := (a == 10)) ifFalse:[
        b := Random nextIntegerBetween:0 and:19.
        Stdout print:' '; print: b; cr.
    ].
    done
] whileFalse
```


alternative:


```smalltalk
[:exit |
    |first|

    Stdout printCR: (first := Random nextIntegerBetween:0 and:19).
    first == 10 ifTrue:[ exit value:nil ].
    Stdout print:' '; printCR: (Random nextIntegerBetween:0 and:19).
] loopWithExit.
```



## Snabel

Uses a ranged random generator as iterator.

```snabel

let: rnd 19 random;

@rnd {
  $ str say
  10 = &break when
  @rnd pop str say
} for

```



## SNOBOL4

Most Snobols lack a built-in rand( ) function. Kludgy "Linux-only" implementation:

```snobol
	input(.random,io_findunit(),1,"/dev/urandom")
while	&ALPHABET random @rand
	output = rand = rand - (rand / 20) * 20
	eq(rand,10)	 :f(while)
end
```


Or using a library function:


```SNOBOL4
* rand(n) -> real x | 0 <= x < n
-include 'random.sno'

loop    ne(output = convert(rand(20)'integer'),10) :s(loop)
end
```



## Spin

```spin
con
  _clkmode = xtal1 + pll16x
  _clkfreq = 80_000_000

obj
  ser : "FullDuplexSerial.spin"

pub main | r, s
  ser.start(31, 30, 0, 115200)

  s := 1337 ' PRNG seed

  repeat
    r := ||?s // 20
    ser.dec(r)
    ser.tx(32)
    if r == 10
      quit
    r := ||?s // 20
    ser.dec(r)
    ser.tx(32)

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)
```

```txt

8 13 1 7 19 1 15 16 9 6 5 9 1 15 5 0 6 3 9 19 8 9 10

```



## SPL

Direct approach:

```spl>

  n = #.rnd(20)
  #.output(n)
  << n=10
  n = #.rnd(20)
  #.output(n)
<
```

With reusable code:

```spl>

  :1
  n = #.rnd(20)
  #.output(n)
  <-
  << n=10
  1 <->
<
```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON@

BEGIN
 DECLARE VAL INTEGER;
 LOOP: WHILE (TRUE = TRUE) DO
  SET VAL = INTEGER(RAND() * 20);
  CALL DBMS_OUTPUT.PUT_LINE(VAL);
  IF (VAL = 10) THEN
   LEAVE LOOP;
  END IF;
  SET VAL = INTEGER(RAND() * 20);
  CALL DBMS_OUTPUT.PUT_LINE(VAL);
 END WHILE LOOP;
END @

```

Output:

```txt

db2 -td@
db2 => SET SERVEROUTPUT ON@
DB20000I  The SET SERVEROUTPUT command completed successfully.
db2 => BEGIN
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.

4
16
9
1
10

```

Since V11.1, the builtin module can be used instead of RAND, like this:

```sql pl

SET VAL = CALL DBMS_RANDOM.VALUE(0,20);

```



## Stata


```stata
while 1 {
	local n=runiformint(0,19)
	display `n'
	if `n'==10 continue, break
	display runiformint(0,19)
}
```



###  Mata


```stata
for (; 1; ) {
	printf("%f\n",n=runiformint(1,1,0,19))
	if (n==10) break
	printf("%f\n",runiformint(1,1,0,19))
}
```



## Suneido


```Suneido
forever
    {
    Print(i = Random(20))
    if i is 10
        break
    Print(i = Random(20))
    }

```



## Swift


```Swift
while true
{
  let a = Int(arc4random()) % (20)
  print("a: \(a)",terminator: "   ")
  if (a == 10)
  {
    break
  }
  let b = Int(arc4random()) % (20)
  print("b: \(b)")
}

```
```txt

a: 2   b: 7
a: 16   b: 13
a: 18   b: 16
a: 10

```



## Tcl


```tcl
while true {
    set a [expr int(20*rand())]
    puts $a
    if {$a == 10} {
        break
    }
    set b [expr int(20*rand())]
    puts $b
}
```


=={{header|TI-89 BASIC}}==


```ti89b
Local x
Loop
  rand(20)-1 → x
  Disp x                     © new line and text
  If x = 10 Then
    Exit
  EndIf
  Output 64, 50, rand(20)-1  © paint text to the right on same line
EndLoop
```


=={{header|Transact-SQL}}==

<lang Transact-SQL>
DECLARE @i INT;
WHILE 1=1
BEGIN
    SET @i = ABS(CHECKSUM(NewId())) % 20;
    PRINT @i;
    IF @i=10 BREAK;
    PRINT ABS(CHECKSUM(NewId())) % 20;
END;

```



## TorqueScript



```Torque
for(%a = 0; %a > -1; %a++)
{
    %number = getRandom(0, 19);
    if(%number == 10)
        break;
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
LOOP
a=RANDOM_NUMBERS (0,19,1)
IF (10==a) THEN
 PRINT "a=",a
 STOP
ELSE
 b=RANDOM_NUMBERS (0,19,1)
 PRINT "a=",a," b=",b
ENDIF
IF (10==a,b) STOP
ENDLOOP

```

```txt

a=0 b=17
a=11 b=13
a=3 b=16
a=17 b=13
a=8 b=11
a=8 b=0
a=6 b=2
a=10

```



## uBasic/4tH

<lang>Do
  n = RND(20)
  Print n
  Until n = 10
  Print RND(20)
Loop
```


## UNIX Shell

This script gets random numbers from jot(1).
If there is any error with jot(1), the script exits.

```bash
while true; do
	a=`jot -w %d -r 1 0 20` || exit $?
	echo $a
	test 10 -eq $a && break
	b=`jot -w %d -r 1 0 20` || exit $?
	echo $b
done
```


Korn Shells have a RANDOM parameter.

```bash
while true; do
  echo $((a=RANDOM%20))
  [ $a -eq 10 ] && break
  echo $((b=RANDOM%20))
done
```



## Ursa

```ursa
decl ursa.util.random r
decl int a b
while true
        set a (r.getint 19)
        out a endl console
        if (= a 10)
                break
        end while
        set b (r.getint 19)
        out b endl console
end while
```



## VBA


```VB
Public Sub LoopsBreak()
    Dim value As Integer
    Randomize
    Do While True
        value = Int(20 * Rnd)
        Debug.Print value
        If value = 10 Then Exit Do
        Debug.Print Int(20 * Rnd)
    Loop
End Sub
```



## VBScript

Based on BASIC version. Demonstrates breaking out of Do/Loop and For/Next (Exit is good for getting out of functions and subs as well).


```vb
Dim a, b, i

Do
    a = Int(Rnd * 20)
    WScript.StdOut.Write a
    If a = 10 Then Exit Do
    b = Int(Rnd * 20)
    WScript.Echo vbNullString, b
Loop

For i = 1 To 100000
    a = Int(Rnd * 20)
    WScript.StdOut.Write a
    If a = 10 Then Exit For
    b = Int(Rnd * 20)
    WScript.Echo vbNullString, b
Next
```



## Visual Basic .NET

```vbnet
Module Program
    Sub Main()
        ' Initialize with seed 0 to get deterministic output (may vary across .NET versions, though).
        Dim rand As New Random(0)

        Do
            Dim first = rand.Next(20) ' Upper bound is exclusive.
            Console.Write(first & " ")

            If first = 10 Then Exit Do

            Dim second = rand.Next(20)
            Console.Write(second & " ")
        Loop
    End Sub
End Module
```

```txt
14 16 15 11 4 11 18 8 19 5 5 9 12 9 19 0 17 19 13 6 16 16 19 0 13 10 18 13 10
```



## XPL0


```XPL0
include c:\cxpl\codes;
int N;
loop    [N:= Ran(20);
        IntOut(0, N);
        if N=10 then quit;
        ChOut(0, 9\tab\);
        IntOut(0, Ran(20));
        CrLf(0);
        ]
```


```txt

7       17
13      2
2       10
0       4
2       9
15      15
14      19
10

```



## zkl


```zkl
while(1){n:=(0).random(20); n.print("  ");
   if (n==10){ println(); break; } (0).random().println();
}
```

```txt

7  2139341079
4  3217334923
18  2050357211
2  2061361000
10

```


