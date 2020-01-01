+++
title = "Loops/Increment loop index within loop body"
description = ""
date = 2019-10-08T16:53:37Z
aliases = []
[extra]
id = 21733
[taxonomies]
categories = []
tags = []
+++

{{task|iterations}}
[[Category:Simple]]

Sometimes, one may need   (or want)   a loop which
its   ''iterator''   (the index
variable)   is modified within the

loop body   '' in addition to the normal incrementation by the   ('''do''')   loop structure index''.


;Goal:
Demonstrate the best way to accomplish this.


<!-- ··· with a nod to Douglas Adams  (42 being the ultimate answer to THE question). !-->

;Task:
Write a loop which:
::*   starts the index (variable) at   '''42'''
::*   (at iteration time)   increments the index by unity
::*   if the index is prime:
::::*   displays the count of primes found (so far) and the prime   (to the terminal)
::::*   increments the index such that the new index is now the (old) index plus that prime
::*   terminates the loop when   '''42'''   primes are shown


Extra credit:   because of the primes get rather large, use commas
within the displayed primes to ease comprehension.


Show all output here.


;Note:
Not all programming languages allow the modification of a
loop's index.   If that is the case, then use whatever method that
is appropriate or idiomatic for that language.   Please add a note
if the loop's index isn't modifiable.


;Related tasks:
*   [[Loop over multiple arrays simultaneously]]
*   [[Loops/Break]]
*   [[Loops/Continue]]
*   [[Loops/Do-while]]
*   [[Loops/Downward for]]
*   [[Loops/For]]
*   [[Loops/For with a specified step]]
*   [[Loops/Foreach]]
*   [[Loops/Infinite]]
*   [[Loops/N plus one half]]
*   [[Loops/Nested]]
*   [[Loops/While]]
*   [[Loops/with multiple ranges]]
*   [[Loops/Wrong ranges]]





## 360 Assembly

Assembler 360 provides 3 instructions to create loops: BCT, BXH and BXLE, the register which contains the loop index can be modified at any time. Nothing exceptional for an assembly, banning to modify the loop index begins with high level languages.

This task is a good example of the use of ED instruction to format a number.
For macro use (IF,DO,...), see [[360_Assembly_macros#360_Assembly_Structured_Macros|Structured Macros]].

```360asm
*        Loops/Increment loop index within loop body - 16/07/2018
LOOPILWB PROLOG
         SR     R6,R6              i=0
         ZAP    N,=P'42'           n=42
       DO WHILE=(C,R6,LT,IMAX)     do while(i<imax)
         BAL    R14,ISPRIME          call isprime(n)
       IF C,R0,EQ,=F'1' THEN         if n is prime then
         LA     R6,1(R6)               i=i+1
         XDECO  R6,XDEC                edit i
         MVC    PG+2(2),XDEC+10        output i
         MVC    ZN,EM                  load edit mask
         ED     ZN,N                   edit n
         MVC    PG+7(L'ZN),ZN          output n
         XPRNT  PG,L'PG                print buffer
         ZAP    WP,N                   n
         AP     WP,N                   +n
         SP     WP,=P'1'               +1
         ZAP    N,WP                   n=n+n-1
       ENDIF    ,                    endif
         ZAP    WP,N                 n
         AP     WP,=P'1'             +1
         ZAP    N,WP                 n=n+1
       ENDDO    ,                  enddo
         EPILOG
ISPRIME  EQU    *                  isprime(n) -----------------------
         CP     N,=P'2'            if n=2
         BE     RETURN1            then return(1)
         CP     N,=P'3'            if n=3
         BE     RETURN1            then return(1)
         ZAP    WDP,N              n
         DP     WDP,=PL8'2'        /2
         CP     WDP+8(8),=P'0'     if mod(n,2)=0
         BE     RETURN0            then return(0)
         ZAP    WDP,N              n
         DP     WDP,=PL8'3'        /3
         CP     WDP+8(8),=P'0'     if mod(n,3)=0
         BE     RETURN0            then return(0)
         ZAP    J,=P'5'            j=5
LWHILE   ZAP    WP,J               j
         MP     WP,J               *j
         CP     WP,N               while(j*j<=n)
         BH     EWHILE             ~
         ZAP    WDP,N                n
         DP     WDP,J                /j
         CP     WDP+8(8),=P'0'       if mod(n,j)=0
         BE     RETURN0              then return(0)
         ZAP    WP,J                 j
         AP     WP,=P'2'             +2
         ZAP    WDP,N                n
         DP     WDP,WP               n/(j+2)
         CP     WDP+8(8),=P'0'       if mod(n,j+2)=0
         BE     RETURN0              then return(0)
         ZAP    WP,J                 j
         AP     WP,=P'6'             +6
         ZAP    J,WP                 j=j+6
         B      LWHILE             loopwhile
EWHILE   B      RETURN1            return(1)
RETURN0  LA     R0,0               rc=0
         B      RETURNX
RETURN1  LA     R0,1               rc=1
RETURNX  BR     R14                return to caller -----------------
IMAX     DC     F'42'              limit
EM       DC     XL20'402020206B2020206B2020206B2020206B202120'  mask
N        DS     PL8                n
J        DS     PL8                j
PG       DC     CL80'i=00 :  000,000,000,000,000'   buffer
XDEC     DS     CL12               temp for XDECO
WP       DS     PL8                temp for AP,SP,MP
WDP      DS     PL16               temp for DP
CW       DS     CL16               temp for UNPK
ZN       DS     CL20
         REGEQU
         END    LOOPILWB
```

{{out}}
<pre style="height:40ex">
i= 1 :                   43
i= 2 :                   89
i= 3 :                  179
i= 4 :                  359
i= 5 :                  719
i= 6 :                1,439
i= 7 :                2,879
i= 8 :                5,779
i= 9 :               11,579
i=10 :               23,159
i=11 :               46,327
i=12 :               92,657
i=13 :              185,323
i=14 :              370,661
i=15 :              741,337
i=16 :            1,482,707
i=17 :            2,965,421
i=18 :            5,930,887
i=19 :           11,861,791
i=20 :           23,723,597
i=21 :           47,447,201
i=22 :           94,894,427
i=23 :          189,788,857
i=24 :          379,577,741
i=25 :          759,155,483
i=26 :        1,518,310,967
i=27 :        3,036,621,941
i=28 :        6,073,243,889
i=29 :       12,146,487,779
i=30 :       24,292,975,649
i=31 :       48,585,951,311
i=32 :       97,171,902,629
i=33 :      194,343,805,267
i=34 :      388,687,610,539
i=35 :      777,375,221,081
i=36 :    1,554,750,442,183
i=37 :    3,109,500,884,389
i=38 :    6,219,001,768,781
i=39 :   12,438,003,537,571
i=40 :   24,876,007,075,181
i=41 :   49,752,014,150,467
i=42 :   99,504,028,301,131

```



## ALGOL 68

In Algol 68, the FOR loop counter cannot be modified in the loop. This uses a WHILE loop testing at the top but is otherwise largely a translation of the Kotlin entry.

```algol68
BEGIN
    # returns TRUE if n is prime, FALSE otherwise #
    PROC is prime = ( LONG INT n )BOOL:
         IF   n MOD 2 = 0 THEN n = 2
         ELIF n MOD 3 = 0 THEN n = 3
         ELSE
            LONG INT d := 5;
            BOOL result := TRUE;
            WHILE IF   d * d > n   THEN FALSE
                  ELIF n MOD d = 0 THEN result := FALSE
                  ELIF d +:= 2;
                       n MOD d = 0 THEN result := FALSE
                  ELSE d +:= 4; TRUE
                  FI
            DO SKIP OD;
            result
         FI # is prime # ;

    LONG INT i := 42;
    LONG INT n := 0;
    WHILE n < 42 DO
        IF is prime( i ) THEN
            n +:= 1;
            print( ( "n = "
                   , whole( n,  -2 )
                   , "  "
                   , whole( i, -19 )
                   , newline
                   )
                 );
            i +:= i - 1
        FI;
        i +:= 1
    OD
END
```

{{out}}

```txt

n =  1                   43
n =  2                   89
n =  3                  179
n =  4                  359
n =  5                  719
n =  6                 1439
n =  7                 2879
n =  8                 5779
n =  9                11579
n = 10                23159
n = 11                46327
n = 12                92657
n = 13               185323
n = 14               370661
n = 15               741337
n = 16              1482707
n = 17              2965421
n = 18              5930887
n = 19             11861791
n = 20             23723597
n = 21             47447201
n = 22             94894427
n = 23            189788857
n = 24            379577741
n = 25            759155483
n = 26           1518310967
n = 27           3036621941
n = 28           6073243889
n = 29          12146487779
n = 30          24292975649
n = 31          48585951311
n = 32          97171902629
n = 33         194343805267
n = 34         388687610539
n = 35         777375221081
n = 36        1554750442183
n = 37        3109500884389
n = 38        6219001768781
n = 39       12438003537571
n = 40       24876007075181
n = 41       49752014150467
n = 42       99504028301131

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program loopinc96.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessMultOver:     .asciz "Multiplication 64 : Dépassement de capacité.\n"
sMessResult:        .ascii "Index  : "
sMessIndex:         .fill 11, 1, ' '            @ size => 11
                    .ascii "Value  : "
sMessValeur:        .fill 21, 1, ' '            @ size => 21
szCarriageReturn:   .asciz "\n"

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                              @ entry of program
    mov r7,#0                                      @ counter
    mov r5,#42                                     @ start index low bits
    mov r6,#0                                      @ start index high bits
1:                                                 @ begin loop
    mov r0,r5
    mov r1,r6
    bl isPrime                                     @ prime ?
    bcs 100f                                       @ error overflow ?
    cmp r0,#1                                      @ is prime ?
    beq 2f                                         @ yes
    adds r5,#1                                     @ no -> increment index
    addcs r6,#1
    b 1b                                           @ and loop
2:                                                 @ display index and prime
    add r7,#1                                      @ increment counter
    mov r0,r7
    ldr r1,iAdrsMessIndex                          @ conversion index
    bl conversion10
    mov r0,r5
    mov r1,r6                                      @ conversion value
    ldr r2,iAdrsMessValeur
    bl conversionRegDoubleU                        @ conversion double -> ascii
    ldr r0,iAdrsMessResult
    bl affichageMess

    adds r5,r5
    add r6,r6
    addcs r6,#1
    cmp r7,#42                                     @ end ?
    blt 1b                                         @ no loop

100:                                               @ standard end of the program
    mov r0, #0                                     @ return code
    mov r7, #EXIT                                  @ request to exit program
    svc #0                                         @ perform the system call

iAdrsMessIndex:           .int sMessIndex
iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult


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
    push {r1-r4,lr}                                   @ save registers
    mov r3,r1
    mov r2,#LGZONECAL
1:                                                    @ start loop
    bl divisionpar10U                                 @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                        @ digit
    strb r1,[r3,r2]                                   @ store digit on area
    cmp r0,#0                                         @ stop if quotient = 0
    subne r2,#1                                       @ else previous position
    bne 1b                                            @ and loop
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
/* r0 quotient   */
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
/*   number is prime ?              */
/***************************************************/
/* r0 contains low bytes of double */
/* r1 contains high bytes of double */
/* r0 returns 1 if prime else 0  */
@2147483647
@4294967297
@131071
isPrime:
    push {r1-r5,lr}                      @ save registers
    mov r4,r0                            @ save double
    mov r5,r1
    subs r2,r0,#1                        @ exposant n - 1
    sbcs r3,r1,#0

    mov r0,#2                            @ base  2
    mov r1,#0
    bl moduloPuR96                       @ compute modulo
    bcs 100f                             @ overflow error
    cmp r0,#1                            @ modulo <> 1 -> no prime
    bne 90f

    mov r0,#3                            @ base 3
    mov r1,#0
    bl moduloPuR96
    bcs 100f                             @ overflow error
    cmp r0,#1
    bne 90f

    mov r0,#5                            @ base 5
    mov r1,#0
    bl moduloPuR96
    bcs 100f                             @ overflow error
    cmp r0,#1
    bne 90f

    mov r0,#7                            @ base 7
    mov r1,#0
    bl moduloPuR96
    bcs 100f                             @ overflow error
    cmp r0,#1
    bne 90f

    mov r0,#11                           @ base 11
    mov r1,#0
    bl moduloPuR96
    bcs 100f                             @ overflow error
    cmp r0,#1
    bne 90f

    mov r0,#13                           @ base 13
    mov r1,#0
    bl moduloPuR96
    bcs 100f                             @ overflow error
    cmp r0,#1
    bne 90f

    mov r0,#17                           @ base 17
    mov r1,#0
    bl moduloPuR96
    bcs 100f                             @ overflow error
    cmp r0,#1
    bne 90f
    mov r0,#1                            @ is prime
    msr     cpsr_f, #0                   @ no error overflow zero -> flags
    b 100f
90:
    mov r0,#0                            @ no prime
    msr     cpsr_f, #0                   @ no error overflow zero -> flags
100:                                     @ fin standard de la fonction
    pop {r1-r5,lr}                       @ restaur registers
    bx lr                                @ return


/********************************************************/
/*   compute  b pow e modulo m  */
/*                                             */
/********************************************************/
/* r0 base double low bits */
/* r1 base double high bits */
/* r2 exposant low bitss  */
/* r3 exposant high bits */
/* r4 modulo low bits */
/* r5 modulo high bits */
/* r0 returns result low bits */
/* r1 returns result high bits */
/* if overflow , flag carry is set else is clear */
moduloPuR96:
    push {r2-r12,lr}                       @ save registers
    cmp r0,#0                              @ control low byte <> zero
    bne 1f
    cmp r1,#0                              @ control high bytes <> zero
    beq 100f
1:
    mov r9,r4                              @ modulo PB
    mov r10,r5                             @ modulo PH
    mov r5,r2                              @ exposant **
    mov r6,r3                              @ exposant
    mov r7,r0                              @ base PB
    mov r8,r1                              @ base PH
    mov r2,#0
    mov r3,r9
    mov r4,r10
    mov r11,#1                             @ result PB
    mov r12,#0                             @ result PH
/* r0 contient partie basse dividende */
/* r1 contient partie moyenne dividende */
/* r2 contient partie haute du diviseur */
/* r3 contient partie basse diviseur  */
/* r4 contient partie haute diviseur  */
/* r0 retourne partie basse du quotient */
/* r1 retourne partie moyenne du quotient */
/* r2 retourne partie haute du quotient */
/* r3 retourne partie basse du reste */
/* r4 retourne partie haute du reste */
    bl divisionReg96DU
    mov r7,r3                               @ base <- remainder
    mov r8,r4
2:
    tst r5,#1                               @ test du bit 0
    beq 3f
    mov r0,r7
    mov r1,r8
    mov r2,r11
    mov r3,r12
    bl multiplicationR96U
    bcs 100f                                @ error overflow
    mov r3,r9
    mov r4,r10
    bl divisionReg96DU
    mov r11,r3                              @ result <- remainder
    mov r12,r4
3:
    mov r0,r7
    mov r1,r8
    mov r2,r7
    mov r3,r8
    bl multiplicationR96U
    bcs 100f                                @ error overflow
    mov r3,r9
    mov r4,r10
    bl divisionReg96DU
    mov r7,r3                               @ base <- remainder
    mov r8,r4

    lsr r5,#1
    lsrs r6,#1
    orrcs r5,#0x80000000
    cmp r5,#0
    bne 2b
    cmp r6,#0
    bne 2b
    mov r0,r11
    mov r1,r12
    msr     cpsr_f, #0                       @ no error overflow zero -> flags
100:                                         @ end function
   	pop {r2-r12,lr}                          @ restaur registers
    bx lr                                    @ return
/***************************************************/
/*   multiplication 2 registers (64 bits) unsigned */
/*   result in 3 registers 96 bits                 */
/***************************************************/
/* r0 low bits number 1    */
/* r1 high bits number 1    */
/* r2 low bits number 2    */
/* r3 high bits number 2    */
/* r0 returns low bits résult   */
/* r1 returns median bits résult */
/* r2 returns high bits résult  */
/* if overflow , flag carry is set else is clear */
multiplicationR96U:
    push {r3-r8,lr}           @ save registers
    umull r5,r6,r0,r2         @ mult low bits
    umull r4,r8,r0,r3         @ mult low bits 1 high bits 2
    mov r0,r5                 @ result low bits ok
    adds r4,r6                @ add results
    addcs  r8,#1              @ carry
    umull r6,r7,r1,r2         @ mult high bits 1 low bits 2
    adds r4,r6                @ add results
    addcs  r8,#1              @ carry
    adds r8,r7                @ add results
    bcs 99f                   @ overflow ?
    umull r6,r7,r1,r3         @ mult high bits 1 high bits 2
    cmp r7,#0                 @ error overflow ?
    bne 99f
    adds r8,r6                @ add results
    bcs 99f                   @ error overflow
    mov r1,r4                 @ return median bytes
    mov r2,r8                 @ return high bytes
    msr cpsr_f, #0            @ no error overflow zero -> flags
    b 100f
99:                           @ display message overflow
	ldr r0,iAdrszMessMultOver @
	bl affichageMess
    mov r0,#0
    mov r1,#0
    msr cpsr_f, #1<<29        @ maj flag carry à 1  et tous les autres à 0
100:                          @ end function
   	pop {r3-r8,lr}            @ restaur registers
    bx lr                     @ return
iAdrszMessMultOver:         .int szMessMultOver
/***************************************************/
/*   division number (3 registers) 92 bits by number (2 registers) 64 bits */
/*           unsigned                            */
/***************************************************/
/* r0 low bits dividende */
/* r1 median bits dividende */
/* r2 high bits dividende */
/* r3 low bits divisor  */
/* r4 high bits divis0r  */
/* r0 returns low bits quotient */
/* r1 returns median bits quotient */
/* r2 returns high bits quotien */
/* r3 returns low bits remainder */
/* r4 returns high bits remainder */
/* remainder do not is 3 registers */
divisionReg96DU:
    push {r5-r10,lr}    @ save registers
    mov r7,r3           @ low bits divisor
    mov r8,r4           @ high bits divisor
    mov r4,r0           @ low bits dividende -> low bits quotient
    mov r5,r1           @ median bits dividende  -> median bits quotient
    mov r6,r2           @ high bits dividende -> high bits quotient

                        @
    mov r0,#0           @ low bits remainder
    mov r1,#0           @ median bits remainder
    mov r2,#0           @ high bits remainder (not useful)
    mov r9,#96          @ counter loop (32 bits * 3)
    mov r10,#0          @ last bit
1:
    lsl   r2,#1         @ shift left high bits remainder
    lsls  r1,#1         @ shift left median bits remainder
    orrcs r2,#1         @ left bit median -> right bit high
    lsls r0,#1          @ shift left low bits remainder
    orrcs r1,#1         @ left bit low -> right bit median
    lsls r6,#1          @ shift left high bits quotient
    orrcs r0,#1         @ left bit high -> right bit low remainder
    lsls r5,#1          @ shift left median bits quotient
    orrcs r6,#1         @ left bit median -> right bit high
    lsls r4,#1          @ shift left low bits quotient
    orrcs r5,#1         @ left bit low -> right bit median
    orr r4,r10          @ last bit -> bit 0 quotient
    mov r10,#0          @ raz du bit
                        @ compare  remainder and divisor
    cmp r2,#0           @ high bit remainder
    bne 2f
    cmp r1,r8           @ compare median bits
    blo 3f              @ lower
    bhi 2f              @ highter
    cmp r0,r7           @ equal -> compare low bits
    blo 3f              @ lower
2:                      @ remainder > divisor
    subs r0,r7          @ sub divisor of remainder
    sbcs r1,r8
    mov r10,#0          @ reuse ponctuelle  r10
    sbc r2,r2,r10       @ carry
    mov r10,#1          @ last bit à 1
3:
    subs r9,#1          @ increment counter loop
    bgt 1b              @ and loop
    lsl r6,#1           @ shift left high bits quotient
    lsls r5,#1          @ shift left median bits quotient
    orrcs r6,#1         @ left bit median -> right bit high
    lsls r4,#1          @ shift left low bits quotient
    orrcs r5,#1         @ left bit low -> right bit median
    orr r4,r10          @ last bit -> bit 0 quotient
    mov r3,r0           @ low bits remainder
    mov r0,r4           @ low bits quotient
    mov r4,r1           @ high bits remainder
    mov r1,r5           @ median bits quotient
    //mov r5,r2
    mov r2,r6           @ high bits quotient

100:                    @ end function
   	pop {r5-r10,lr}     @ restaur registers
    bx lr               @ return

/***************************************************/
/*   Conversion double integer 64bits in ascii     */
/***************************************************/
/* r0 contains low bits     */
/* r1 contains high bits    */
/* r2 contains address area */
conversionRegDoubleU:
    push {r0-r5,lr}         @ save registers
    mov r5,r2
    mov r4,#19              @ start location
    mov r2,#10              @ conversion decimale
1:                          @ begin loop
    bl divisionReg64U       @ division by 10
    add r3,#48              @ -> digit ascii
    strb r3,[r5,r4]         @ store digit in area index r4
    sub r4,r4,#1            @ decrement index
    cmp r0,#0               @ low bits quotient = zero ?
    bne 1b	                @ no -> loop
    cmp r1,#0               @ high bits quotient = zero ?
    bne 1b                  @ no -> loop
                            @ spaces -> begin area
    mov r3,#' '             @ space
2:
    strb r3,[r5,r4]         @ store space in area
    subs r4,r4,#1           @ decrement index
    bge 2b                  @ and loop if > zéro

100:                        @ end fonction
   	pop {r0-r5,lr}          @ restaur registers
    bx lr                   @ return
/***************************************************/
/*   division number 64 bits / number 32 bits      */
/***************************************************/
/* r0 contains low bits dividende  */
/* r1 contains high bits dividente */
/* r2 contains divisor             */
/* r0 returns low bits quotient    */
/* r1 returns high bits quotient   */
/* r3 returns remainder            */
divisionReg64U:
    push {r4,r5,lr}    @ save registers
    mov r5,#0          @ raz remainder R
    mov r3,#64         @ loop counter
    mov r4,#0          @ last bit
1:
    lsl r5,#1          @ shift left remainder one bit
    lsls r1,#1         @ shift left high bits quotient one bit
    orrcs r5,#1        @ and bit -> remainder
    lsls r0,#1         @ shift left low bits quotient one bit
    orrcs r1,#1        @ and left bit -> high bits
    orr r0,r4          @ last bit  quotient
    mov r4,#0          @ raz last bit
    cmp r5,r2          @ compare remainder divisor
    subhs r5,r2        @ if highter sub divisor of remainder
    movhs r4,#1        @  and 1 -> last bit
3:
    subs r3,#1         @ decrement counter loop
    bgt 1b             @ and loop if not zero
    lsl r1,#1          @ else shift left higt bits quotient
    lsls r0,#1         @ and shift  left low bits
    orrcs r1,#1
    orr r0,r4          @ last bit quotient
    mov r3,r5
100:                   @ end function
    pop {r4,r5,lr}     @ restaur registers
    bx lr              @ return

```

{{out}}
<Pre>
pi@raspberrypi:~/asm/rosetta/ASS3 $ loopsinc96
Index  : 1          Value  :                   43
Index  : 2          Value  :                   89
Index  : 3          Value  :                  179
Index  : 4          Value  :                  359
Index  : 5          Value  :                  719
Index  : 6          Value  :                 1439
Index  : 7          Value  :                 2879
Index  : 8          Value  :                 5779
Index  : 9          Value  :                11579
Index  : 10         Value  :                23159
Index  : 11         Value  :                46327
Index  : 12         Value  :                92657
Index  : 13         Value  :               185323
Index  : 14         Value  :               370661
Index  : 15         Value  :               741337
Index  : 16         Value  :              1482707
Index  : 17         Value  :              2965421
Index  : 18         Value  :              5930887
Index  : 19         Value  :             11861791
Index  : 20         Value  :             23723597
Index  : 21         Value  :             47447201
Index  : 22         Value  :             94894427
Index  : 23         Value  :            189788857
Index  : 24         Value  :            379577741
Index  : 25         Value  :            759155483
Index  : 26         Value  :           1518310967
Index  : 27         Value  :           3036621941
Index  : 28         Value  :           6073243889
Index  : 29         Value  :          12146487779
Index  : 30         Value  :          24292975649
Index  : 31         Value  :          48585951311
Index  : 32         Value  :          97171902629
Index  : 33         Value  :         194343805267
Index  : 34         Value  :         388687610539
Index  : 35         Value  :         777375221081
Index  : 36         Value  :        1554750442183
Index  : 37         Value  :        3109500884389
Index  : 38         Value  :        6219001768781
Index  : 39         Value  :       12438003537571
Index  : 40         Value  :       24876007075181
Index  : 41         Value  :       49752014150467
Index  : 42         Value  :       99504028301131

```



## Arturo

{{trans|Python}}

```arturo
i 42
n 0

loop n<42 {
    if $(isPrime i) {
        n n+1
        print "n = " + $(padRight $(toString n) 2) + $(padRight $(toString i) 20)
        i 2*i-1
    }
    i i+1
}
```

{{out}}

```txt
n =  1                  43
n =  2                  89
n =  3                 179
n =  4                 359
n =  5                 719
n =  6                1439
n =  7                2879
n =  8                5779
n =  9               11579
n = 10               23159
n = 11               46327
n = 12               92657
n = 13              185323
n = 14              370661
n = 15              741337
n = 16             1482707
n = 17             2965421
n = 18             5930887
n = 19            11861791
n = 20            23723597
n = 21            47447201
n = 22            94894427
n = 23           189788857
n = 24           379577741
n = 25           759155483
n = 26          1518310967
n = 27          3036621941
n = 28          6073243889
n = 29         12146487779
n = 30         24292975649
n = 31         48585951311
n = 32         97171902629
n = 33        194343805267
n = 34        388687610539
n = 35        777375221081
n = 36       1554750442183
n = 37       3109500884389
n = 38       6219001768781
n = 39      12438003537571
n = 40      24876007075181
n = 41      49752014150467
n = 42      99504028301131
```


## C

The following uses a 'for' rather than a 'do/while' loop but otherwise is similar to the Kotlin
entry.

The 'thousands separator' aspect (using the ' flag in printf and setting the locale appropriately) works fine when compiled with gcc on Ubuntu 14.04 but may not work on some other systems as this is not a standard flag.

```c
#include <stdio.h>
#include <locale.h>

#define LIMIT 42

int is_prime(long long n) {
    if (n % 2 == 0) return n == 2;
    if (n % 3 == 0) return n == 3;
    long long d = 5;
    while (d * d <= n) {
        if (n % d == 0) return 0;
        d += 2;
        if (n % d == 0) return 0;
        d += 4;
    }
    return 1;
}

int main() {
    long long i;
    int n;
    setlocale(LC_NUMERIC, "");
    for (i = LIMIT, n = 0; n < LIMIT; i++)
        if (is_prime(i)) {
            n++;
            printf("n = %-2d  %'19lld\n", n, i);
            i += i - 1;
        }
    return 0;
}
```


{{out}}

```txt

Same as Kotlin entry

```


## C++


```cpp

#include "stdafx.h"
#include <iostream>
#include <math.h>
using namespace std;

bool isPrime(double number)
{
    for (double i = number - 1; i >= 2; i--) {
        if (fmod(number, i) == 0)
	    return false;
    }
    return true;
}
int main()
{
    double i = 42;
    int n = 0;
    while (n < 42)
    {
        if (isPrime(i))
        {
            n++;
	    cout.width(1); cout << left << "n = " << n;
            //Only for Text Alignment
            if (n < 10)
	    {
	        cout.width(40); cout << right << i << endl;
	    }
	    else
	    {
		cout.width(39); cout << right << i << endl;
	    }
            i += i - 1;
	}
	i++;
    }
    return 0;
}
```



## C#


```c#

using System;
using System.Globalization;

namespace PrimeNumberLoopcs
{
    class Program
    {
        static bool isPrime(double number)
        {
            for(double i = number - 1; i > 1; i--)
            {
                if (number % i == 0)
                    return false;
            }
            return true;
        }
        static void Main(string[] args)
        {
            NumberFormatInfo nfi = new CultureInfo("en-US", false).NumberFormat;
            nfi.NumberDecimalDigits = 0;
            double i = 42;
            int n = 0;
            while (n < 42)
            {
                if (isPrime(i))
                {
                    n++;
                    Console.WriteLine("n = {0,-20} {1,20}", n, i.ToString("N", nfi));
                    i += i - 1;
                }
                i++;
            }
        }
    }
}
```

{{out}}

```txt

n = 1                                      43
n = 2                                      89
n = 3                                     179
n = 4                                     359
n = 5                                     719
n = 6                                   1,439
n = 7                                   2,879
n = 8                                   5,779
n = 9                                  11,579
n = 10                                 23,159
n = 11                                 46,327
n = 12                                 92,657
n = 13                                185,323
n = 14                                370,661
n = 15                                741,337
n = 16                              1,482,707
n = 17                              2,965,421
n = 18                              5,930,887
n = 19                             11,861,791
n = 20                             23,723,597
n = 21                             47,447,201
n = 22                             94,894,427
n = 23                            189,788,857
n = 24                            379,577,741
n = 25                            759,155,483
n = 26                          1,518,310,967
n = 27                          3,036,621,941
n = 28                          6,073,243,889
n = 29                         12,146,487,779
n = 30                         24,292,975,649
n = 31                         48,585,951,311
n = 32                         97,171,902,629
n = 33                        194,343,805,267
n = 34                        388,687,610,539
n = 35                        777,375,221,081
n = 36                      1,554,750,442,183
n = 37                      3,109,500,884,389
n = 38                      6,219,001,768,781
n = 39                     12,438,003,537,571
n = 40                     24,876,007,075,181
n = 41                     49,752,014,150,467
n = 42                     99,504,028,301,131

```



## Dyalect



```Dyalect
func isPrime(number) {
    if number <= 1 {
        return false
    }
    else if number % 2 == 0 {
        return number == 2
    }

    var i = 3

    while (i * i) < number {
        if number % i == 0 {
            return false
        }
        i += 2
    }

    return true
}

var i = 42
var n = 0

while n < 42 {
    if isPrime(i) {
        n += 1
        print("n = \(n)\t\(i)")
        i += i - 1
    }
    i += 1
}
```


Output:


```txt
n = 1   43
n = 2   89
n = 3   179
n = 4   359
n = 5   719
n = 6   1439
n = 7   2879
n = 8   5779
n = 9   11579
n = 10  23159
n = 11  46327
n = 12  92657
n = 13  185323
n = 14  370661
n = 15  741337
n = 16  1482707
n = 17  2965421
n = 18  5930887
n = 19  11861791
n = 20  23723597
n = 21  47447201
n = 22  94894427
n = 23  189788857
n = 24  379577741
n = 25  759155483
n = 26  1518310967
n = 27  3036621941
n = 28  6073243889
n = 29  12146487779
n = 30  24292975649
n = 31  48585951311
n = 32  97171902629
n = 33  194343805267
n = 34  388687610539
n = 35  777375221081
n = 36  1554750442183
n = 37  3109500884389
n = 38  6219001768781
n = 39  12438003537571
n = 40  24876007075181
n = 41  49752014150467
n = 42  99504028301131
```


=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Well I don't do loops. Nigel Galloway: March 17th., 2019. Let me try to explain where the loopy variables are, for the imperatively constrained.
// cUL allows me to claim the rather trivial extra credit (commas in the numbers)
let cUL=let g=System.Globalization.CultureInfo("en-GB") in (fun (n:uint64)->n.ToString("N0",g))
// fN is primality by trial division
let fN g=pCache|>Seq.map uint64|>Seq.takeWhile(fun n->n*n<g)|>Seq.forall(fun n->g%n>0UL)
// unfold is sort of a loop incremented by 1 in this case
let fG n=Seq.unfold(fun n->Some(n,(n+1UL))) n|>Seq.find(fN)
// unfold is sort of a loop with fG as an internal loop incremented by the exit value of the internal loop in this case.
Seq.unfold(fun n->let n=fG n in Some(n,n+n)) 42UL|>Seq.take 42|>Seq.iteri(fun n g->printfn "%2d -> %s"  (n+1) (cUL g))

```

{{out}}

```txt

 1 -> 43
 2 -> 89
 3 -> 179
 4 -> 359
 5 -> 719
 6 -> 1,439
 7 -> 2,879
 8 -> 5,779
 9 -> 11,579
10 -> 23,159
11 -> 46,327
12 -> 92,657
13 -> 185,323
14 -> 370,661
15 -> 741,337
16 -> 1,482,707
17 -> 2,965,421
18 -> 5,930,887
19 -> 11,861,791
20 -> 23,723,597
21 -> 47,447,201
22 -> 94,894,427
23 -> 189,788,857
24 -> 379,577,741
25 -> 759,155,483
26 -> 1,518,310,967
27 -> 3,036,621,941
28 -> 6,073,243,889
29 -> 12,146,487,779
30 -> 24,292,975,649
31 -> 48,585,951,311
32 -> 97,171,902,629
33 -> 194,343,805,267
34 -> 388,687,610,539
35 -> 777,375,221,081
36 -> 1,554,750,442,183
37 -> 3,109,500,884,389
38 -> 6,219,001,768,781
39 -> 12,438,003,537,571
40 -> 24,876,007,075,181
41 -> 49,752,014,150,467
42 -> 99,504,028,301,131

```



## Factor

Explicit loop indices are non-idiomatic, but Factor is certainly capable of using them. Factor has a for loop near-equivalent, <code><range> [ ] each</code>, but since it doesn't mesh well with mutation, a while loop is used.

###  Using two numbers on the data stack


```factor
USING: formatting kernel math math.primes
tools.memory.private ;
IN: rosetta-code.loops-inc-body

42
0
[ dup 42 < ] [
    over prime? [
        1 + 2dup swap commas
        "n = %-2d  %19s\n" printf
        [ dup + 1 - ] dip
    ] when
    [ 1 + ] dip
] while
2drop
```


###  Using lexical variables

Factor provides lexical variables for situations where they improve readability.

```factor
USING: formatting kernel math math.primes
tools.memory.private ;
IN: rosetta-code.loops-inc-body

[let
    42 :> i!
    0  :> n!
    [ n 42 < ] [
        i prime? [
            n 1 + n!
            n i commas "n = %-2d  %19s\n" printf
            i i + 1 - i!
        ] when
        i 1 + i!
    ] while
]
```

{{out}}

```txt

n = 1                    43
n = 2                    89
n = 3                   179
n = 4                   359
n = 5                   719
n = 6                 1,439
n = 7                 2,879
n = 8                 5,779
n = 9                11,579
n = 10               23,159
n = 11               46,327
n = 12               92,657
n = 13              185,323
n = 14              370,661
n = 15              741,337
n = 16            1,482,707
n = 17            2,965,421
n = 18            5,930,887
n = 19           11,861,791
n = 20           23,723,597
n = 21           47,447,201
n = 22           94,894,427
n = 23          189,788,857
n = 24          379,577,741
n = 25          759,155,483
n = 26        1,518,310,967
n = 27        3,036,621,941
n = 28        6,073,243,889
n = 29       12,146,487,779
n = 30       24,292,975,649
n = 31       48,585,951,311
n = 32       97,171,902,629
n = 33      194,343,805,267
n = 34      388,687,610,539
n = 35      777,375,221,081
n = 36    1,554,750,442,183
n = 37    3,109,500,884,389
n = 38    6,219,001,768,781
n = 39   12,438,003,537,571
n = 40   24,876,007,075,181
n = 41   49,752,014,150,467
n = 42   99,504,028,301,131

```



## Fortran

Fortran does not allow to modify the index inside the loop.

```fortran
do i=1,10
  write(*,*) i
  i=i+1
end do
```


```txt

Error - I is currently being used as a DO or implied DO control variable
Compilation failed.

```



### Fortran 95


```fortran
! Loops Increment loop index within loop body - 17/07/2018
      integer*8 n
      imax=42
      i=0; n=42
      Do While(i<imax)
        If (isprime(n)==1) Then
          i=i+1
          Write (*,'(I2,1X,I20)') i,n
          n=n+n-1
        EndIf
        n=n+1
      EndDo
      End

      Function isprime(n)
        integer*8 n,i
        If (n==2 .OR. n==3) Then
          isprime=1
          return
        ElseIf (Mod(n,2)==0 .OR. Mod(n,3)==0) Then
          isprime=0
          return
        Else
          i=5
          Do While(i*i<=n)
            If (Mod(n,i)==0 .OR. Mod(n,i+2)==0) Then
              isprime=0
              return
            EndIf
            i=i+6
          EndDo
          isprime=1
          return
        EndIf
      EndFunction
```

{{out}}
<pre style="height:60ex">
 1                   43
 2                   89
 3                  179
 4                  359
 5                  719
 6                 1439
 7                 2879
 8                 5779
 9                11579
10                23159
11                46327
12                92657
13               185323
14               370661
15               741337
16              1482707
17              2965421
18              5930887
19             11861791
20             23723597
21             47447201
22             94894427
23            189788857
24            379577741
25            759155483
26           1518310967
27           3036621941
28           6073243889
29          12146487779
30          24292975649
31          48585951311
32          97171902629
33         194343805267
34         388687610539
35         777375221081
36        1554750442183
37        3109500884389
38        6219001768781
39       12438003537571
40       24876007075181
41       49752014150467
42       99504028301131

```



### Fortran IV

The limit is set to 25 due to the size of integer in Fortran IV.

```fortran
C LOOPS INCREMENT LOOP INDEX WITHIN LOOP BODY - 17/07/2018
      IMAX=25
      I=0
      N=42
  10  IF(I.GE.IMAX)GOTO 30
        IF(ISPRIME(N).NE.1)GOTO 20
          I=I+1
          WRITE(*,301) I,N
 301      FORMAT(I2,1X,I10)
          N=N+N-1
  20    N=N+1
      GOTO 10
  30  CONTINUE
      END

      FUNCTION ISPRIME(M)
        IF(M.NE.2 .AND. M.NE.3)GOTO 10
          ISPRIME=1
          RETURN
  10    IF(MOD(M,2).NE.0 .AND. MOD(M,3).NE.0)GOTO 20
          ISPRIME=0
          RETURN
  20      I=5
  30      IF(I*I.GT.M)GOTO 50
            IF(MOD(M,I).NE.0 .AND. MOD(M,I+2).NE.0)GOTO 40
              ISPRIME=0
              RETURN
  40        I=I+6
          GOTO 30
  50      ISPRIME=1
          RETURN
      END
```

{{out}}
<pre style="height:60ex">
 1         43
 2         89
 3        179
 4        359
 5        719
 6       1439
 7       2879
 8       5779
 9      11579
10      23159
11      46327
12      92657
13     185323
14     370661
15     741337
16    1482707
17    2965421
18    5930887
19   11861791
20   23723597
21   47447201
22   94894427
23  189788857
24  379577741
25  759155483

```


## FreeBASIC


```freebasic
' version 18-01-2019
' compile with: fbc -s console

Function isprime(number As ULongInt) As UInteger

    If number Mod 2 = 0 Then Return 0
    If number Mod 3 = 0 Then Return 0
    Dim As UInteger i, max = Sqr(number)

    For i = 5 To max Step 2
        If number Mod i = 0 Then Return 0
    Next

    Return 1

End Function

' ------=< MAIN >=------

Dim As UInteger counter
Dim As ULongInt i

Print : Print
counter = 0
For i = 42 To &HFFFFFFFFFFFFFFFF    ' for next loop, loop maximum = 2^64-1
    If isprime(i) Then
        counter += 1
        Print Using "n =### ##################,"; counter; i
        If counter >= 42 Then Exit for
        i += i -1
    End If
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}
<pre style="height:40ex">n =  1                  43
n =  2                  89
n =  3                 179
n =  4                 359
n =  5                 719
n =  6               1,439
n =  7               2,879
n =  8               5,779
n =  9              11,579
n = 10              23,159
n = 11              46,327
n = 12              92,657
n = 13             185,323
n = 14             370,661
n = 15             741,337
n = 16           1,482,707
n = 17           2,965,421
n = 18           5,930,887
n = 19          11,861,791
n = 20          23,723,597
n = 21          47,447,201
n = 22          94,894,427
n = 23         189,788,857
n = 24         379,577,741
n = 25         759,155,483
n = 26       1,518,310,967
n = 27       3,036,621,941
n = 28       6,073,243,889
n = 29      12,146,487,779
n = 30      24,292,975,649
n = 31      48,585,951,311
n = 32      97,171,902,629
n = 33     194,343,805,267
n = 34     388,687,610,539
n = 35     777,375,221,081
n = 36   1,554,750,442,183
n = 37   3,109,500,884,389
n = 38   6,219,001,768,781
n = 39  12,438,003,537,571
n = 40  24,876,007,075,181
n = 41  49,752,014,150,467
n = 42  99,504,028,301,131
```



## Go

This uses Go's 'for' loop but is otherwise similar to the Kotlin entry.

The 'thousands separator' aspect is dealt with by a couple of external packages (in the 'import' declarations) which can be installed using 'go get'.

```go
package main

import(
    "golang.org/x/text/language"
    "golang.org/x/text/message"
)

func isPrime(n uint64) bool {
    if n % 2 == 0 {
        return n == 2
    }
    if n % 3 == 0 {
        return n == 3
    }
    d := uint64(5)
    for d * d <= n {
        if n % d == 0 {
            return false
        }
        d += 2
        if n % d == 0 {
            return false
        }
        d += 4
    }
    return true
}

const limit = 42

func main() {
    p := message.NewPrinter(language.English)
    for i, n := uint64(limit), 0; n < limit; i++ {
        if isPrime(i) {
            n++
            p.Printf("n = %-2d  %19d\n", n, i)
            i += i - 1
        }
    }
}
```


{{out}}

```txt

Same as Kotlin entry

```



## J

Fun with j.  The verb tacit_loop implements the computation.

```j

tacit_loop =: _1&(>:@:{`[`]})@:(, (1&p: # _1 2&p.)@:{:)@:]^:(0 ~: (>: #))^:_ x:

```

Now derive it from the python solution.  The monadic verb loop fairly straightforwardly matches the python solution except that loop returns the vector of computed values rather than displays them.

```j

isPrime =: 1&p:
assert 1 1 0 -: isPrime 2 3 4   NB. test and example

loop =: verb define
 i =. x: y
 n =. i. 0
 while. y > # n do.
  if. isPrime i do.
   n =. n , i
   i =. _1 2 p. i
  end.
  i =. i + 1
 end.
 n
)

```

Store the vector of indexes using its tail as the current index, removing the `n' variable.  In doing so the last item of `i' is not part of the solution, hence change less than to less or equal, and discard the tail value.  Also extract the conversion to extended precision x: .

```J

loop =: verb define@:x:
 i =. y
 while. y >: # i do.
  if. isPrime {: i do.
   i =. (, _1 2 p. {:) i
  end.
  i =. _1 (>:@:{)`[`]} i
 end.
 }: i
)

```


Replace the "if" statement with a computation.  This one works by appending onto the solution vector isPrime copies of the proposed new index.

```J

loop =: verb define@:x:
 i =. y
 while. y >: # i do.
  i =. (, (isPrime # _1 2&p.)@:{:) i
  i =. _1 (>:@:{)`[`]} i
 end.
 }: i
)

```

Names are an issue brought forth in the j forums.  Names have most meaning to the person who wrote them, so there's a bit of J philosophy that says "show the code".  J doesn't enforce "code only", and definitions can encapsulate useful chunks of code.  If the names I've chosen don't work in your experience or language you could replace them with `a' and `b'.

```J

save_if_prime =: , (isPrime # _1 2&p.)@:{:
increment_tail =: _1&(>:@:{`[`]})

loop =: verb define@:x:
 i =. y
 while. y >: # i do.
  i =. save_if_prime i
  i =. increment_tail i
 end.
 }: i
)

```

Why make two assignments when j can increment at save?

```J

loop =: verb define@:x:
 i =. y
 while. y >: # i do.
  i =. increment_tail@:save_if_prime i
 end.
 }: i
)

```

Next replace the while loop with double application of J's generalized power conjunction.

```J

While =: conjunction def 'u^:(0~:v)^:_'

loop =: verb define@:x:
 i =. y
 }: increment_tail@:save_if_prime While(y >: #) i
)

```

By inspection the variable `i' doesn't contribute anything useful whatsoever.  The verb's argument, y, remains.
Finally, implemented as an hook [http://www.jsoftware.com/help/dictionary/dictf.htm verb trains] with 'y' and `i' as left ([) and right (]) arguments the complete definitions for tacit_loop are

```J

isPrime =: 1&p:
save_if_prime =: , (isPrime # _1 2&p.)@:{:
increment_tail =: _1&(>:@:{`[`]})
While =: conjunction def 'u^:(0~:v)^:_'
tacit_loop =: [: }: (increment_tail@:save_if_prime@:]While(>: #) x:)

```

Include the index numbers with demonstration:

```J

   9!:37 ] 0 2048 0 222  NB. output control permit lines of 2^11 columns

   (>:@:i. ,: tacit_loop) 42
 1  2   3   4   5    6    7    8     9    10    11    12     13     14     15      16      17      18       19       20       21       22        23        24        25         26         27         28          29          30          31          32           33           34           35            36            37            38             39             40             41             42
43 89 179 359 719 1439 2879 5779 11579 23159 46327 92657 185323 370661 741337 1482707 2965421 5930887 11861791 23723597 47447201 94894427 189788857 379577741 759155483 1518310967 3036621941 6073243889 12146487779 24292975649 48585951311 97171902629 194343805267 388687610539 777375221081 1554750442183 3109500884389 6219001768781 12438003537571 24876007075181 49752014150467 99504028301131


   NB. fix the definition.  Here's the code.
   tacit_loop f.
[: }: (_1&(>:@:{`[`]})@:(, (1&p: # _1 2&p.)@:{:)@:]^:(0 ~: (>: #))^:_ x:)


```

If the loop must require the output side effect, this save_if_prime definition does the trick.  Without the output hook it is probably more efficient than the copying version because it evaluates the hook
```txt
(, _1 2&p.@:{:)
```
 only when isPrime is true.

```J

   extra_credit =: ([: }. ,@(',' ,.~ _3 [\ ])&.|.@:":)&>
   show =: [ ([: echo@:deb@:({. , ' ' , {:)@:extra_credit # , {:)
   save_if_prime =: (, _1 2&p.@:{:)@:show^:(isPrime@:{:)
   empty@:tacit_loop 42
1 43
2 89
3 179
4 359
5 719
6 1,439
7 2,879
8 5,779
9 11,579
10 23,159
11 46,327
12 92,657
13 185,323
14 370,661
15 741,337
16 1,482,707
17 2,965,421
18 5,930,887
19 11,861,791
20 23,723,597
21 47,447,201
22 94,894,427
23 189,788,857
24 379,577,741
25 759,155,483
26 1,518,310,967
27 3,036,621,941
28 6,073,243,889
29 12,146,487,779
30 24,292,975,649
31 48,585,951,311
32 97,171,902,629
33 194,343,805,267
34 388,687,610,539
35 777,375,221,081
36 1,554,750,442,183
37 3,109,500,884,389
38 6,219,001,768,781
39 12,438,003,537,571
40 24,876,007,075,181
41 49,752,014,150,467
42 99,504,028,301,131

```



## Java

The following uses a 'for' rather than a 'do/while' loop but otherwise is similar to the Kotlin
entry.

```java
public class LoopIncrementWithinBody {

    static final int LIMIT = 42;

    static boolean isPrime(long n) {
        if (n % 2 == 0) return n == 2;
        if (n % 3 == 0) return n == 3;
        long d = 5;
        while (d * d <= n) {
            if (n % d == 0) return false;
            d += 2;
            if (n % d == 0) return false;
            d += 4;
        }
        return true;
    }

    public static void main(String[] args) {
        long i;
        int n;
        for (i = LIMIT, n = 0; n < LIMIT; i++)
            if (isPrime(i)) {
                n++;
                System.out.printf("n = %-2d  %,19d\n", n, i);
                i += i - 1;
            }
    }
}
```


{{out}}

```txt

Same as Kotlin entry

```



## Julia

Julia's <code>for</code> loop iterator is an iterator type which cannot be incremented as a simple variable would to change looping.

```julia
using Primes, Formatting

function doublemyindex(n=42)
    shown = 0
    i = BigInt(n)
    while shown < n
        if isprime(i + 1)
            shown += 1
            println("The index is ", format(shown, commas=true), " and ",
                                     format(i + 1, commas=true), " is prime.")
            i += i
        end
        i += 1
    end
end

doublemyindex()

```
 {{output}}
```txt

The index is 1 and 43 is prime.
The index is 2 and 89 is prime.
The index is 3 and 179 is prime.
The index is 4 and 359 is prime.
The index is 5 and 719 is prime.
The index is 6 and 1,439 is prime.
The index is 7 and 2,879 is prime.
The index is 8 and 5,779 is prime.
The index is 9 and 11,579 is prime.
The index is 10 and 23,159 is prime.
The index is 11 and 46,327 is prime.
The index is 12 and 92,657 is prime.
The index is 13 and 185,323 is prime.
The index is 14 and 370,661 is prime.
The index is 15 and 741,337 is prime.
The index is 16 and 1,482,707 is prime.
The index is 17 and 2,965,421 is prime.
The index is 18 and 5,930,887 is prime.
The index is 19 and 11,861,791 is prime.
The index is 20 and 23,723,597 is prime.
The index is 21 and 47,447,201 is prime.
The index is 22 and 94,894,427 is prime.
The index is 23 and 189,788,857 is prime.
The index is 24 and 379,577,741 is prime.
The index is 25 and 759,155,483 is prime.
The index is 26 and 1,518,310,967 is prime.
The index is 27 and 3,036,621,941 is prime.
The index is 28 and 6,073,243,889 is prime.
The index is 29 and 12,146,487,779 is prime.
The index is 30 and 24,292,975,649 is prime.
The index is 31 and 48,585,951,311 is prime.
The index is 32 and 97,171,902,629 is prime.
The index is 33 and 194,343,805,267 is prime.
The index is 34 and 388,687,610,539 is prime.
The index is 35 and 777,375,221,081 is prime.
The index is 36 and 1,554,750,442,183 is prime.
The index is 37 and 3,109,500,884,389 is prime.
The index is 38 and 6,219,001,768,781 is prime.
The index is 39 and 12,438,003,537,571 is prime.
The index is 40 and 24,876,007,075,181 is prime.
The index is 41 and 49,752,014,150,467 is prime.
The index is 42 and 99,504,028,301,131 is prime.

```



## Kotlin

Unlike many other C-family languages (notably Java), Kotlin's 'for' statement doesn't allow either the iteration variable or the step to be modified within the loop body.

So instead we use a do/while loop here which has no such restrictions.

```scala
// version 1.2.60

fun isPrime(n: Long): Boolean {
    if (n % 2L == 0L) return n == 2L
    if (n % 3L == 0L) return n == 3L
    var d = 5L
    while (d * d <= n) {
        if (n % d == 0L) return false
        d += 2L
        if (n % d == 0L) return false
        d += 4L
    }
    return true
}

fun main(args: Array<String>) {
    var i = 42L
    var n = 0
    do {
        if (isPrime(i)) {
            n++
            System.out.printf("n = %-2d  %,19d\n", n, i)
            i += i - 1
        }
        i++
    }
    while (n < 42)
}
```

{{out}}
<pre style="height:35ex">n = 1                    43
n = 2                    89
n = 3                   179
n = 4                   359
n = 5                   719
n = 6                 1,439
n = 7                 2,879
n = 8                 5,779
n = 9                11,579
n = 10               23,159
n = 11               46,327
n = 12               92,657
n = 13              185,323
n = 14              370,661
n = 15              741,337
n = 16            1,482,707
n = 17            2,965,421
n = 18            5,930,887
n = 19           11,861,791
n = 20           23,723,597
n = 21           47,447,201
n = 22           94,894,427
n = 23          189,788,857
n = 24          379,577,741
n = 25          759,155,483
n = 26        1,518,310,967
n = 27        3,036,621,941
n = 28        6,073,243,889
n = 29       12,146,487,779
n = 30       24,292,975,649
n = 31       48,585,951,311
n = 32       97,171,902,629
n = 33      194,343,805,267
n = 34      388,687,610,539
n = 35      777,375,221,081
n = 36    1,554,750,442,183
n = 37    3,109,500,884,389
n = 38    6,219,001,768,781
n = 39   12,438,003,537,571
n = 40   24,876,007,075,181
n = 41   49,752,014,150,467
n = 42   99,504,028,301,131
```


Although Kotlin is predominantly an object-oriented/procedural language, it does have some features which enable one to program in a functional style. These features include 'tail recursion' which, of course, is commonly used in place of loops in purely functional languages.

In such cases, the Kotlin compiler optimizes out the recursion, leaving behind a fast and efficient loop based version instead.

The following version uses a tail recursive function rather than a while loop to achieve the same effect:


```scala
// version 1.2.60

fun isPrime(n: Long): Boolean {
    if (n % 2L == 0L) return n == 2L
    if (n % 3L == 0L) return n == 3L
    var d = 5L
    while (d * d <= n) {
        if (n % d == 0L) return false
        d += 2L
        if (n % d == 0L) return false
        d += 4L
    }
    return true
}

tailrec fun loop(index: Long, numPrimes: Int) {
    if (numPrimes == 42) return
    var i = index
    var n = numPrimes
    if (isPrime(i)) {
        n++
        System.out.printf("n = %-2d  %,19d\n", n, i)
        loop(2 * i - 1, n)
    }
    else loop(++i, n)
}

fun main(args: Array<String>) {
    loop(42, 0)
}
```


{{out}}

```txt

Same as 'while' loop version.

```



## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      Function IsPrime (x) {
            if x<=5 OR frac(x) then {
                  if x == 2 OR x == 3 OR x == 5 then =true
                  Break
            }
            if frac(x/2 ) else exit
            if frac(x/3) else exit
            x1=sqrt(x): d=5
            {if frac(x/d ) else exit
                  d += 2: if d>x1 then =true : exit
                  if frac(x/d) else exit
                  d += 4: if d<= x1 else =true: exit
                  loop
             }
      }
      \\ For Next loops or For {} loops can't change iterator variable (variable has a copy of real iterator)
      \\ In those loops we have to use Continue to skip lines and repeat the loop.
      \\ so we have to use Block iterator, using Loop which set a flag current block to repeat itself once.
      def long Limit=42, n
      def currency i
      i=Limit
      {
            if n<limit Else exit
            if isPrime(i)  then n++ : Print format$("n={0::2}: {1:-20}", n, str$(i,"#,###")) : i+=i-1
            i++
            loop
      }
}
CheckIt

```


{{out}}

```txt

Same as Kotlin entry

```



## Maple

A translation of Kotlin entry

```Maple
i := 42:
count := 0:
while(count < 42) do
	i := i+1:
	if type(i,prime) then
		count := count + 1:
		printf("n=%-2d     %19d\n", count,i):
		i := 2*i -1:
	end if:
end do:
```

{{Out|Output}}

```txt
n=1                       43
n=2                       89
n=3                      179
n=4                      359
n=5                      719
n=6                     1439
n=7                     2879
n=8                     5779
n=9                    11579
n=10                   23159
n=11                   46327
n=12                   92657
n=13                  185323
n=14                  370661
n=15                  741337
n=16                 1482707
n=17                 2965421
n=18                 5930887
n=19                11861791
n=20                23723597
n=21                47447201
n=22                94894427
n=23               189788857
n=24               379577741
n=25               759155483
n=26              1518310967
n=27              3036621941
n=28              6073243889
n=29             12146487779
n=30             24292975649
n=31             48585951311
n=32             97171902629
n=33            194343805267
n=34            388687610539
n=35            777375221081
n=36           1554750442183
n=37           3109500884389
n=38           6219001768781
n=39          12438003537571
n=40          24876007075181
n=41          49752014150467
n=42          99504028301131
```



## Microsoft Small Basic

Small Basic allows to modify the index inside the loop.

```smallbasic
'Loops Increment loop index within loop body - 16/07/2018
imax=42
i=0
n=42
While i<imax
  isprime_n()
  If ret_isprime_n Then
    i=i+1
    format_i()
    format_n()
    TextWindow.WriteLine("i="+ret_format_i+" : "+ret_format_n)
    n=n+n-1
  EndIf
  n=n+1
EndWhile

Sub isprime_n
  If n=2 Or n=3 Then
    ret_isprime_n="True"
  ElseIf Math.Remainder(n,2)=0 Or Math.Remainder(n,3)=0 Then
    ret_isprime_n="False"
  Else
    j=5
    While j*j<=n
      If Math.Remainder(n,j)=0 Or Math.Remainder(n,j+2)=0 Then
        ret_isprime_n="False"
        Goto exitsub
      EndIf
      j=j+6
    EndWhile
    ret_isprime_n="True"
  EndIf
exitsub:
EndSub 'isprime_n

Sub format_i
  ret_format_i=Text.GetSubText("   ",1,3-Text.GetLength(i))+i
EndSub 'format_i

Sub format_n
  nn=""
  l=-1
  For k=Text.GetLength(n) To 1 Step -1
    l=l+1
    cc=Text.GetSubText(n,k,1)
    If l=3 Then
      cv=","
      l=0
    Else
      cv=""
    EndIf
    nn=Text.Append(cc,Text.Append(cv,nn))
  EndFor
  space="                    "
  nn=Text.GetSubText(space,1,Text.GetLength(space)-Text.GetLength(nn))+nn
  ret_format_n=nn
EndSub 'format_n
```

{{out}}
<pre style="height:60ex">
i= 1 :                   43
i= 2 :                   89
i= 3 :                  179
i= 4 :                  359
i= 5 :                  719
i= 6 :                1,439
i= 7 :                2,879
i= 8 :                5,779
i= 9 :               11,579
i=10 :               23,159
i=11 :               46,327
i=12 :               92,657
i=13 :              185,323
i=14 :              370,661
i=15 :              741,337
i=16 :            1,482,707
i=17 :            2,965,421
i=18 :            5,930,887
i=19 :           11,861,791
i=20 :           23,723,597
i=21 :           47,447,201
i=22 :           94,894,427
i=23 :          189,788,857
i=24 :          379,577,741
i=25 :          759,155,483
i=26 :        1,518,310,967
i=27 :        3,036,621,941
i=28 :        6,073,243,889
i=29 :       12,146,487,779
i=30 :       24,292,975,649
i=31 :       48,585,951,311
i=32 :       97,171,902,629
i=33 :      194,343,805,267
i=34 :      388,687,610,539
i=35 :      777,375,221,081
i=36 :    1,554,750,442,183
i=37 :    3,109,500,884,389
i=38 :    6,219,001,768,781
i=39 :   12,438,003,537,571
i=40 :   24,876,007,075,181
i=41 :   49,752,014,150,467
i=42 :   99,504,028,301,131

```



## NewLISP


```newlisp

#! /usr/local/bin/newlisp

(define (prime? n)
 (and
   (set 'lst (factor n))
   (= (length lst) 1)))

(define (thousands_separator i)
    (setq i (string i))
    (setq len (length i))
    (setq i (reverse (explode i)))
    (setq o "")
    (setq count3 0)
    (dolist (x i)
        (setq o (string o x))
        (inc count3)
        (if (and (= 3 count3) (< (+ $idx 1) len))
            (begin
            (setq o (string o "_"))
            (setq count3 0))))

    (reverse o))


;- - - Main begins here
(setq i 42)
(setq n 0)
(while (< n 42)
    (if (prime? i)
        (begin
            (inc n)
            (println (string "n = " n " -> " (thousands_separator i)))
            (setq i (+ i i -1))))
    (inc i)
)

(exit)

```



```txt

n = 1 -> 43
n = 2 -> 89
n = 3 -> 179
n = 4 -> 359
n = 5 -> 719
n = 6 -> 1_439
n = 7 -> 2_879
n = 8 -> 5_779
n = 9 -> 11_579
n = 10 -> 23_159
n = 11 -> 46_327
n = 12 -> 92_657
n = 13 -> 185_323
n = 14 -> 370_661
n = 15 -> 741_337
n = 16 -> 1_482_707
n = 17 -> 2_965_421
n = 18 -> 5_930_887
n = 19 -> 11_861_791
n = 20 -> 23_723_597
n = 21 -> 47_447_201
n = 22 -> 94_894_427
n = 23 -> 189_788_857
n = 24 -> 379_577_741
n = 25 -> 759_155_483
n = 26 -> 1_518_310_967
n = 27 -> 3_036_621_941
n = 28 -> 6_073_243_889
n = 29 -> 12_146_487_779
n = 30 -> 24_292_975_649
n = 31 -> 48_585_951_311
n = 32 -> 97_171_902_629
n = 33 -> 194_343_805_267
n = 34 -> 388_687_610_539
n = 35 -> 777_375_221_081
n = 36 -> 1_554_750_442_183
n = 37 -> 3_109_500_884_389
n = 38 -> 6_219_001_768_781
n = 39 -> 12_438_003_537_571
n = 40 -> 24_876_007_075_181
n = 41 -> 49_752_014_150_467
n = 42 -> 99_504_028_301_131


```



## Perl

Messing with the loop iterator value doesn't go well in Perl, so use the <tt>while</tt> loop alternative. The <code>ntheory</code> module is used to test for primes.
{{trans|Kotlin}}
{{libheader|ntheory}}

```perl
use ntheory qw(is_prime);

$i = 42;
while ($n < 42) {
    if (is_prime($i)) {
        $n++;
        printf "%2d %21s\n", $n, commatize($i);
        $i += $i - 1;
    }
    $i++;
}

sub commatize {
    (my $s = reverse shift) =~ s/(.{3})/$1,/g;
    $s =~ s/,$//;
    $s = reverse $s;
}
```

{{out}}
<pre  style="height:30ex"> 1                    43
 2                    89
 3                   179
 4                   359
 5                   719
 6                 1,439
 7                 2,879
 8                 5,779
 9                11,579
10                23,159
11                46,327
12                92,657
13               185,323
14               370,661
15               741,337
16             1,482,707
17             2,965,421
18             5,930,887
19            11,861,791
20            23,723,597
21            47,447,201
22            94,894,427
23           189,788,857
24           379,577,741
25           759,155,483
26         1,518,310,967
27         3,036,621,941
28         6,073,243,889
29        12,146,487,779
30        24,292,975,649
31        48,585,951,311
32        97,171,902,629
33       194,343,805,267
34       388,687,610,539
35       777,375,221,081
36     1,554,750,442,183
37     3,109,500,884,389
38     6,219,001,768,781
39    12,438,003,537,571
40    24,876,007,075,181
41    49,752,014,150,467
42    99,504,028,301,131
```



## Perl 6

Hmm.
<blockquote>Demonstrate the best way to accomplish this. </blockquote>
The ''best'' way is probably to not use an explicit loop. Just calculate the sequence directly.


```perl6
# the actual sequence logic
my @seq = grep *.is-prime, (42, { .is-prime ?? $_+<1 !! $_+1 } … *);

# display code
say (1+$_).fmt("%-4s"), @seq[$_].flip.comb(3).join(',').flip.fmt("%20s") for ^42;
```

{{out}}

```txt
1                     43
2                     89
3                    179
4                    359
5                    719
6                  1,439
7                  2,879
8                  5,779
9                 11,579
10                23,159
11                46,327
12                92,657
13               185,323
14               370,661
15               741,337
16             1,482,707
17             2,965,421
18             5,930,887
19            11,861,791
20            23,723,597
21            47,447,201
22            94,894,427
23           189,788,857
24           379,577,741
25           759,155,483
26         1,518,310,967
27         3,036,621,941
28         6,073,243,889
29        12,146,487,779
30        24,292,975,649
31        48,585,951,311
32        97,171,902,629
33       194,343,805,267
34       388,687,610,539
35       777,375,221,081
36     1,554,750,442,183
37     3,109,500,884,389
38     6,219,001,768,781
39    12,438,003,537,571
40    24,876,007,075,181
41    49,752,014,150,467
42    99,504,028,301,131
```



## Phix

Phix does not allow for loop variables to be modified, so we must use a while loop and manual increment for this sort of thing. There is not, as yet, an is_prime() builtin. We can use prime_factors() returns {}, though it is probably a little bit slower as it builds the full list rather than yielding false asap - but at least we don't have to define an is_prime() function.

```Phix
atom i=42, n=1
while n<=42 do
    if prime_factors(i)={} then
        printf(1,"n = %-2d  %,19d\n", {n, i})
        n += 1
        i += i-1
    end if
    i += 1
end while
```

{{out}}

```txt

n = 1                    43
n = 2                    89
n = 3                   179
n = 4                   359
n = 5                   719
n = 6                 1,439
n = 7                 2,879
n = 8                 5,779
n = 9                11,579
n = 10               23,159
n = 11               46,327
n = 12               92,657
n = 13              185,323
n = 14              370,661
n = 15              741,337
n = 16            1,482,707
n = 17            2,965,421
n = 18            5,930,887
n = 19           11,861,791
n = 20           23,723,597
n = 21           47,447,201
n = 22           94,894,427
n = 23          189,788,857
n = 24          379,577,741
n = 25          759,155,483
n = 26        1,518,310,967
n = 27        3,036,621,941
n = 28        6,073,243,889
n = 29       12,146,487,779
n = 30       24,292,975,649
n = 31       48,585,951,311
n = 32       97,171,902,629
n = 33      194,343,805,267
n = 34      388,687,610,539
n = 35      777,375,221,081
n = 36    1,554,750,442,183
n = 37    3,109,500,884,389
n = 38    6,219,001,768,781
n = 39   12,438,003,537,571
n = 40   24,876,007,075,181
n = 41   49,752,014,150,467
n = 42   99,504,028,301,131

```



## Python


```Python
def isPrime(n):
    for x in 2, 3:
        if not n % x:
            return n == x
    d = 5
    while d * d <= n:
        for x in 2, 4:
            if not n % d:
                return False
            d += x
    return True

i = 42
n = 0
while n < 42:
    if isPrime(i):
        n += 1
        print('n = {:2} {:20,}'.format(n, i))
        i += i - 1
    i += 1
```

{{out}}

```txt
n =  1                   43
n =  2                   89
n =  3                  179
n =  4                  359
n =  5                  719
n =  6                1,439
n =  7                2,879
n =  8                5,779
n =  9               11,579
n = 10               23,159
n = 11               46,327
n = 12               92,657
n = 13              185,323
n = 14              370,661
n = 15              741,337
n = 16            1,482,707
n = 17            2,965,421
n = 18            5,930,887
n = 19           11,861,791
n = 20           23,723,597
n = 21           47,447,201
n = 22           94,894,427
n = 23          189,788,857
n = 24          379,577,741
n = 25          759,155,483
n = 26        1,518,310,967
n = 27        3,036,621,941
n = 28        6,073,243,889
n = 29       12,146,487,779
n = 30       24,292,975,649
n = 31       48,585,951,311
n = 32       97,171,902,629
n = 33      194,343,805,267
n = 34      388,687,610,539
n = 35      777,375,221,081
n = 36    1,554,750,442,183
n = 37    3,109,500,884,389
n = 38    6,219,001,768,781
n = 39   12,438,003,537,571
n = 40   24,876,007,075,181
n = 41   49,752,014,150,467
n = 42   99,504,028,301,131
```



## Racket


Racket's <code>for</code> doesn't allow modification of index on the fly. The usual idiom for writing this kind of loop is to use named let, as shown here.


```racket
#lang racket

(require math/number-theory)

(define (comma x)
  (string-join
   (reverse
    (for/list ([digit (in-list (reverse (string->list (~a x))))] [i (in-naturals)])
      (cond
        [(and (= 0 (modulo i 3)) (> i 0)) (string digit #\,)]
        [else (string digit)])))
   ""))

(let loop ([x 42] [cnt 0])
  (cond
    [(= cnt 42) (void)]
    [(prime? x) (printf "~a: ~a\n" (add1 cnt) (comma x))
                (loop (* 2 x) (add1 cnt))]
    [else (loop (add1 x) cnt)]))
```


{{out}}

```txt

1: 43
2: 89
3: 179
4: 359
5: 719
6: 1,439
7: 2,879
8: 5,779
9: 11,579
10: 23,159
11: 46,327
12: 92,657
13: 185,323
14: 370,661
15: 741,337
16: 1,482,707
17: 2,965,421
18: 5,930,887
19: 11,861,791
20: 23,723,597
21: 47,447,201
22: 94,894,427
23: 189,788,857
24: 379,577,741
25: 759,155,483
26: 1,518,310,967
27: 3,036,621,941
28: 6,073,243,889
29: 12,146,487,779
30: 24,292,975,649
31: 48,585,951,311
32: 97,171,902,629
33: 194,343,805,267
34: 388,687,610,539
35: 777,375,221,081
36: 1,554,750,442,183
37: 3,109,500,884,389
38: 6,219,001,768,781
39: 12,438,003,537,571
40: 24,876,007,075,181
41: 49,752,014,150,467
42: 99,504,028,301,131

```



## REXX


```rexx
/*REXX pgm displays primes found:  starting Z at 42, if Z is a prime, add Z, else add 1.*/
numeric digits 20;              d=digits()       /*ensure enough decimal digits for  Z. */
parse arg limit .                                /*obtain optional arguments from the CL*/
if limit=='' | limit==","  then limit=42         /*Not specified?  Then use the default.*/
n=0                                              /*the count of number of primes found. */
     do z=42  until n==limit                     /* ◄──this DO loop's index is modified.*/
     if isPrime(z)  then do;  n=n + 1            /*Z  a prime?  Them bump prime counter.*/
                              say right('n='n, 9)     right(commas(z), d)
                              z=z + z - 1        /*also, bump the  DO  loop index  Z.   */
                         end
     end   /*z*/                                 /* [↑] a small tribute to Douglas Adams*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas:  parse arg _;  do j=length(_)-3  to 1  by -3; _=insert(',', _, j); end;   return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure; parse arg #;         if wordpos(#, '2 3 5 7')\==0  then return 1
                                         if # // 2==0 | # // 3    ==0  then return 0
           do j=5  by 6  until j*j>#;    if # // j==0 | # // (J+2)==0  then return 0
           end   /*j*/                           /*           ___                       */
         return 1                                /*Exceeded  √ #  ?    Then # is prime. */
```

{{out|output}}

```txt

      n=1                   43
      n=2                   89
      n=3                  179
      n=4                  359
      n=5                  719
      n=6                1,439
      n=7                2,879
      n=8                5,779
      n=9               11,579
     n=10               23,159
     n=11               46,327
     n=12               92,657
     n=13              185,323
     n=14              370,661
     n=15              741,337
     n=16            1,482,707
     n=17            2,965,421
     n=18            5,930,887
     n=19           11,861,791
     n=20           23,723,597
     n=21           47,447,201
     n=22           94,894,427
     n=23          189,788,857
     n=24          379,577,741
     n=25          759,155,483
     n=26        1,518,310,967
     n=27        3,036,621,941
     n=28        6,073,243,889
     n=29       12,146,487,779
     n=30       24,292,975,649
     n=31       48,585,951,311
     n=32       97,171,902,629
     n=33      194,343,805,267
     n=34      388,687,610,539
     n=35      777,375,221,081
     n=36    1,554,750,442,183
     n=37    3,109,500,884,389
     n=38    6,219,001,768,781
     n=39   12,438,003,537,571
     n=40   24,876,007,075,181
     n=41   49,752,014,150,467
     n=42   99,504,028,301,131

```



## Ring


```ring

# Project : Loops/Increment loop index within loop body

load "stdlib.ring"
i = 42
n = 0
while n < 42
         if isprime(i)
            n = n + 1
            see "n = " + n + "    " + i + nl
            i = i + i - 1
         ok
         i = i + 1
end

```

Output:

```txt

n = 1                    43
n = 2                    89
n = 3                   179
n = 4                   359
n = 5                   719
n = 6                 1,439
n = 7                 2,879
n = 8                 5,779
n = 9                11,579
n = 10               23,159
n = 11               46,327
n = 12               92,657
n = 13              185,323
n = 14              370,661
n = 15              741,337
n = 16            1,482,707
n = 17            2,965,421
n = 18            5,930,887
n = 19           11,861,791
n = 20           23,723,597
n = 21           47,447,201
n = 22           94,894,427
n = 23          189,788,857
n = 24          379,577,741
n = 25          759,155,483
n = 26        1,518,310,967
n = 27        3,036,621,941
n = 28        6,073,243,889
n = 29       12,146,487,779
n = 30       24,292,975,649
n = 31       48,585,951,311
n = 32       97,171,902,629
n = 33      194,343,805,267
n = 34      388,687,610,539
n = 35      777,375,221,081
n = 36    1,554,750,442,183
n = 37    3,109,500,884,389
n = 38    6,219,001,768,781
n = 39   12,438,003,537,571
n = 40   24,876,007,075,181
n = 41   49,752,014,150,467
n = 42   99,504,028,301,131
```



## Scala

Like most other [[wp:en:Block_(programming)|Block structured languages]] (apparently with the exception of Java), Scala's 'for' statement is for the sake of fallibility aka side effect or mutability, limited and doesn't allow either the iteration variable or the step to be modified within the loop body. Both are for serious reasons immutable.

### Demonstrate the best way to accomplish this.

So instead we use tail recursion here which, with the use of immutable variables and no side effects, has no such restrictions, and we are save.
{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/4HJPkBM/1 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/yzqldLOuRriR6ojqLKaYPQ Scastie (remote JVM)].

```Scala
import scala.annotation.tailrec

object LoopIncrementWithinBody extends App {
  private val (limit, offset) = (42L, 1)

  @tailrec
  private def loop(i: Long, n: Int): Unit = {

    def isPrime(n: Long) =
      n > 1 && ((n & 1) != 0 || n == 2) && (n % 3 != 0 || n == 3) &&
        ((5 to math.sqrt(n).toInt by 2).par forall (n % _ != 0))

    if (n < limit + offset)
      if (isPrime(i)) {
        printf("n = %-2d  %,19d%n".formatLocal(java.util.Locale.GERMANY, n, i))
        loop(i + i + 1, n + 1)
      } else loop(i + 1, n)
  }

  loop(limit, offset)
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func boolean: isPrime (in integer: number) is func
  result
    var boolean: result is FALSE;
  local
    var integer: count is 2;
  begin
    if number = 2 then
      result := TRUE;
    elsif number > 2 then
      while number rem count <> 0 and count * count <= number do
        incr(count);
      end while;
      result := number rem count <> 0;
    end if;
  end func;

const proc: main is func
  local
    var integer: i is 42;
    var integer: n is 0;
  begin
    for i range 42 to integer.last until n >= 42 do
      if isPrime(i) then
        incr(n);
        writeln("n = " <& n lpad 2 <& i lpad 16);
        i +:= i - 1;
      end if;
    end for;
  end func;
```


{{out}}

```txt

n =  1              43
n =  2              89
n =  3             179
n =  4             359
n =  5             719
n =  6            1439
n =  7            2879
n =  8            5779
n =  9           11579
n = 10           23159
n = 11           46327
n = 12           92657
n = 13          185323
n = 14          370661
n = 15          741337
n = 16         1482707
n = 17         2965421
n = 18         5930887
n = 19        11861791
n = 20        23723597
n = 21        47447201
n = 22        94894427
n = 23       189788857
n = 24       379577741
n = 25       759155483
n = 26      1518310967
n = 27      3036621941
n = 28      6073243889
n = 29     12146487779
n = 30     24292975649
n = 31     48585951311
n = 32     97171902629
n = 33    194343805267
n = 34    388687610539
n = 35    777375221081
n = 36   1554750442183
n = 37   3109500884389
n = 38   6219001768781
n = 39  12438003537571
n = 40  24876007075181
n = 41  49752014150467
n = 42  99504028301131

```



## Tcl

Inspired by Java and Kotlin variants.

Tcl allows modifying the loop variable. Everything can be implemented straightforward.

```tcl
proc isPrime n {
  if {[expr $n % 2] == 0} {
    return [expr $n == 2]
  }
  if {[expr $n % 3] == 0} {
    return [expr $n == 3]
  }
  for {set d 5} {[expr $d * $d] <= $n} {incr d 4} {
    if {[expr $n % $d] == 0} {return 0}
    incr d 2
    if {[expr $n % $d] == 0} {return 0}
  }
  return 1
}

set LIMIT 42

for {set i $LIMIT; set n 0} {$n < $LIMIT} {incr i} {
  if [isPrime $i] {
    incr n
    puts "n=$n, i=$i"
    incr i [expr $i -1]
  }
}
```

{{Out}}

```txt
n=1, i=43
n=2, i=89
n=3, i=179
n=4, i=359
n=5, i=719
n=6, i=1439
n=7, i=2879
n=8, i=5779
n=9, i=11579
n=10, i=23159
n=11, i=46327
n=12, i=92657
n=13, i=185323
n=14, i=370661
n=15, i=741337
n=16, i=1482707
n=17, i=2965421
n=18, i=5930887
n=19, i=11861791
n=20, i=23723597
n=21, i=47447201
n=22, i=94894427
n=23, i=189788857
n=24, i=379577741
n=25, i=759155483
n=26, i=1518310967
n=27, i=3036621941
n=28, i=6073243889
n=29, i=12146487779
n=30, i=24292975649
n=31, i=48585951311
n=32, i=97171902629
n=33, i=194343805267
n=34, i=388687610539
n=35, i=777375221081
n=36, i=1554750442183
n=37, i=3109500884389
n=38, i=6219001768781
n=39, i=12438003537571
n=40, i=24876007075181
n=41, i=49752014150467
n=42, i=99504028301131
```



## VBA

Visual Basic for Application (VBA) allows to modify the index inside the loop.
{{trans|Visual Basic .NET}}
{{works with|VBA|VBA Excel 2013}}

```vb
    Sub Main()
        'Loops Increment loop index within loop body - 17/07/2018
        Dim imax, i As Integer
        Dim n As Currency
        imax = 42
        i = 0: n = 42
        Do While i < imax
            If IsPrime(n) Then
                i = i + 1
                Debug.Print ("i=" & RightX(i, 2) & " : " & RightX(Format(n, "#,##0"), 20))
                n = n + n - 1
            End If
            n = n + 1
        Loop
    End Sub 'Main

    Function IsPrime(n As Currency)
        Dim i As Currency
        If n = 2 Or n = 3 Then
            IsPrime = True
        ElseIf ModX(n, 2) = 0 Or ModX(n, 3) = 0 Then
            IsPrime = False
        Else
            i = 5
            Do While i * i <= n
                If ModX(n, i) = 0 Or ModX(n, i + 2) = 0 Then
                    IsPrime = False
                    Exit Function
                End If
                i = i + 6
            Loop
            IsPrime = True
        End If
    End Function 'IsPrime

    Function ModX(a As Currency, b As Currency) As Currency
        ModX = a - Int(a / b) * b
    End Function 'ModX

    Function RightX(c, n)
        RightX = Right(Space(n) & c, n)
    End Function 'RightX
```

{{out}}
<pre style="height:60ex">
i= 1 :                   43
i= 2 :                   89
i= 3 :                  179
i= 4 :                  359
i= 5 :                  719
i= 6 :                1,439
i= 7 :                2,879
i= 8 :                5,779
i= 9 :               11,579
i=10 :               23,159
i=11 :               46,327
i=12 :               92,657
i=13 :              185,323
i=14 :              370,661
i=15 :              741,337
i=16 :            1,482,707
i=17 :            2,965,421
i=18 :            5,930,887
i=19 :           11,861,791
i=20 :           23,723,597
i=21 :           47,447,201
i=22 :           94,894,427
i=23 :          189,788,857
i=24 :          379,577,741
i=25 :          759,155,483
i=26 :        1,518,310,967
i=27 :        3,036,621,941
i=28 :        6,073,243,889
i=29 :       12,146,487,779
i=30 :       24,292,975,649
i=31 :       48,585,951,311
i=32 :       97,171,902,629
i=33 :      194,343,805,267
i=34 :      388,687,610,539
i=35 :      777,375,221,081
i=36 :    1,554,750,442,183
i=37 :    3,109,500,884,389
i=38 :    6,219,001,768,781
i=39 :   12,438,003,537,571
i=40 :   24,876,007,075,181
i=41 :   49,752,014,150,467
i=42 :   99,504,028,301,131
```


## Visual Basic .NET

Visual Basic .Net allows to modify the index inside the loop.
{{trans|Visual Basic}}
{{works with|Visual Basic .NET|2013}}

```vbnet
Module LoopsIliwlb

    Sub Main()
        'Loops Increment loop index within loop body - 17/07/2018
        Dim imax, i As Int32
        Dim n As Int64
        imax = 42
        i = 0 : n = 42
        While i < imax
            If IsPrime(n) Then
                i = i + 1
                Console.WriteLine("i=" & RightX(i, 2) & " : " & RightX(Format(n, "#,##0"), 20))
                n = n + n - 1
            End If
            n = n + 1
        End While
    End Sub

    Function IsPrime(n As Int64)
        Dim i As Int64
        If n = 2 Or n = 3 Then
            IsPrime = True
        ElseIf (n Mod 2) = 0 Or (n Mod 3) = 0 Then
            IsPrime = False
        Else
            i = 5
            While i * i <= n
                If (n Mod i) = 0 Or (n Mod (i + 2)) = 0 Then
                    IsPrime = False
                    Exit Function
                End If
                i = i + 6
            End While
            IsPrime = True
        End If
    End Function 'IsPrime

    Function RightX(c, n)
        RightX = Right(Space(n) & c, n)
    End Function

End Module
```

{{out}}
<pre style="height:60ex">i= 1 :                   43
i= 2 :                   89
i= 3 :                  179
i= 4 :                  359
i= 5 :                  719
i= 6 :                1,439
i= 7 :                2,879
i= 8 :                5,779
i= 9 :               11,579
i=10 :               23,159
i=11 :               46,327
i=12 :               92,657
i=13 :              185,323
i=14 :              370,661
i=15 :              741,337
i=16 :            1,482,707
i=17 :            2,965,421
i=18 :            5,930,887
i=19 :           11,861,791
i=20 :           23,723,597
i=21 :           47,447,201
i=22 :           94,894,427
i=23 :          189,788,857
i=24 :          379,577,741
i=25 :          759,155,483
i=26 :        1,518,310,967
i=27 :        3,036,621,941
i=28 :        6,073,243,889
i=29 :       12,146,487,779
i=30 :       24,292,975,649
i=31 :       48,585,951,311
i=32 :       97,171,902,629
i=33 :      194,343,805,267
i=34 :      388,687,610,539
i=35 :      777,375,221,081
i=36 :    1,554,750,442,183
i=37 :    3,109,500,884,389
i=38 :    6,219,001,768,781
i=39 :   12,438,003,537,571
i=40 :   24,876,007,075,181
i=41 :   49,752,014,150,467
i=42 :   99,504,028,301,131
```


## zkl

Uses libGMP (GNU MP Bignum Library) for easy prime detection
rather than write that bit of code and pollute this solution.

```zkl
var [const] BN=Import("zklBigNum");  // libGMP
n,p := 1,BN(42);
do{
   if(p.probablyPrime()){ println("n = %2d %,20d".fmt(n,p)); p.add(p); n+=1; }
   p.add(1);
}while(n<=42);
```

zkl loop variables are iterators that don't allow direct manipulation of
their underlying source. The compiler names these iterators __<index>Walker.
However, by using the look ahead stack, we can keep the iterator from
advancing through the source.

```zkl
p:=BN(42);
foreach n in ([1..42]){
   if(p.probablyPrime()){ println("n = %2d %,20d".fmt(n,p)); p.add(p); }
   else{ p.add(1); __nWalker.push(n); }  // p not prime, don't advance n
}
```

{{out}}
<pre style="height:35ex">
n =  1                   43
n =  2                   89
n =  3                  179
n =  4                  359
n =  5                  719
n =  6                1,439
n =  7                2,879
n =  8                5,779
n =  9               11,579
n = 10               23,159
n = 11               46,327
n = 12               92,657
n = 13              185,323
n = 14              370,661
n = 15              741,337
n = 16            1,482,707
n = 17            2,965,421
n = 18            5,930,887
n = 19           11,861,791
n = 20           23,723,597
n = 21           47,447,201
n = 22           94,894,427
n = 23          189,788,857
n = 24          379,577,741
n = 25          759,155,483
n = 26        1,518,310,967
n = 27        3,036,621,941
n = 28        6,073,243,889
n = 29       12,146,487,779
n = 30       24,292,975,649
n = 31       48,585,951,311
n = 32       97,171,902,629
n = 33      194,343,805,267
n = 34      388,687,610,539
n = 35      777,375,221,081
n = 36    1,554,750,442,183
n = 37    3,109,500,884,389
n = 38    6,219,001,768,781
n = 39   12,438,003,537,571
n = 40   24,876,007,075,181
n = 41   49,752,014,150,467
n = 42   99,504,028,301,131

```

