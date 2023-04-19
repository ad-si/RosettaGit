+++
title = "FizzBuzz/Assembly"
description = ""
date = 2015-09-22T11:30:38Z
aliases = []
[extra]
id = 19591
[taxonomies]
categories = []
tags = []
+++

{{collection|FizzBuzz}}


## 360 Assembly

<lang>FIZZBUZZ CSECT                       A SECTION OF CODE STARTS HERE, LABEL IT FIZZBUZZ
**********HOUSE KEEPING AREA**********************
         USING *,12                  FOR THIS PROGRAM WE ARE GOING TO USE REGISTER 12
         STM   14,12,12(13)          SAVE REGISTERS 14,15, AND 0-12 IN CALLER'S SAVE AREA
         LR    12,15                 PUT OUR ENTRY ADDRESS(IN R15) INTO OUR BASE REGISTER
         LA    15,SAVE               POINT R15 AT THE *OUR* SAVE AREA (DEFINED AT THE END)
         ST    15,8(13)              SET FORWARD CHAIN
         ST    13,4(15)              SET BACKWARD CHAIN
         LR    13,15                 SET R13 TO THE ADDRESS OF OUT NEW SAVE AREA
**********MAIN*PROGRAM****************************
         LA    10,LOOP               PUT THE LOOP START ADDRESS IN R10
         LA    8,100                 PUT THE NUMBER OF ITERATIONS IN R8
         LA    5,=F'1'               INITIALIZE BINARY COUNTER TO ONE
LOOP     EQU   *                     LABEL THE LOOP START
         A     5,=F'1'               ADD TO BINARY LOOP COUNTER
         AP    NUM,=PL1'1'           ADD TO PACKED LOOP COUNTER
         B     CHK15                 CHECK IF COUNTER IS % 12
LCHK15   EQU   *                     IF NOT, COME BACK
         B     CHK3                  CHECK IF COUNTER IS % 3
LCHK3    EQU   *                     IF NOT, COME BACK
         B     CHK5                  CHECK IF COUNTER IS % 4
LCHK5    EQU   *                     IF NOT, COME BACK
         MVC   EOUT,EMSK             PREPARE TO PKD->EBCDIC
         EDMK  EOUT,NUM              PKD->EBCDIC
ENLOOP   EQU   *                     IF A TEST WAS POSITIVE RETURN HERE
         WTO   MF=(E,WTOSTART)       PRINT RESULT OF LOOP
         BCTR  8,10                  START OVER
**********HOUSE KEEPING AREA**********************
         L     13,4(13)              RESTORE ADDRESS TO CALLER'S SAVE AREA
         LM    14,12,12(13)          RESTORE REGISTERS AS ON ENTRY
         XR    15,15                 XOR R15 SO IT IS ALL 0 (R15 CREATES THE PROGRAM RETURN CODE)
         BR    14                    RETURN WHERE YOU CAME FROM
**********SUBROUTINE AREA*************************
*////////CHK3////////////////////////////////////*
CHK3     EQU   *                     LABEL ENTRY POINT
         LR    6,5                   LOAD R6 WITH R5(THE BINARY LOOP INDEX)
         A     6,=F'1'               ADD ONE TO R6
         SRDA  6,32                  SHIFT RD VAL 32 BITS RIGHT(TO R7)
         D     6,=F'3'               DIVIDE BY 3
         C     6,=F'0'               IS REMAINDER 0?
         BE    DIV3                  IF SO GOTO DIV3 ROUTINE
         B     LCHK3                 IF NOT GO BACK TO LOOP
*////////CHK15///////////////////////////////////*
CHK15    EQU   *                     LABEL ENTRY POINT
         LR    6,5                   LOAD R6 WITH R5(THE BINARY LOOP INDEX)
         A     6,=F'1'               ADD ONE TO R6
         SRDA  6,32                  SHIFT RD VAL 32 BITS RIGHT(TO R7)
         D     6,=F'15'              DIVIDE BY 15
         C     6,=F'0'               IS REMAINDER 0?
         BE    DIV15                 IF SO GOTO DIV15 ROUTINE
         B     LCHK15                IF NOT GO BACK TO LOOP
*////////CHK5////////////////////////////////////*
CHK5     EQU   *                     LABEL ENTRY POINT
         LR    6,5                   LOAD R6 WITH R5(THE BINARY LOOP INDEX)
         A     6,=F'1'               ADD ONE TO R6
         SRDA  6,32                  SHIFT RD VAL 32 BITS RIGHT(TO R7)
         D     6,=F'5'               DIVIDE BY 5
         C     6,=F'0'               IS REMAINDER 0?
         BE    DIV5                  IF SO GOTO DIV5 ROUTINE
         B     LCHK5                 IF NOT GO BACK TO LOOP
*////////////////////////////////////////////////*
DIV3     EQU   *                     LABEL ENRTY POINT
         MVC   EOUT,FIZZ             SAY FIZZ
         B     ENLOOP                RETURN TO LOOP
*////////////////////////////////////////////////*
DIV5     EQU   *                     LABEL ENTRY POINT
         MVC   EOUT,BUZZ             SAY BUZZ
         B     ENLOOP                RETURN TO LOOP
*////////////////////////////////////////////////*
DIV15    EQU   *                     LABEL ENTRY POINT
         MVC   EOUT,FIZZBUZ          SAY FIZZBUZZ
         B     ENLOOP                RETURN TO LOOP
**********VARIABLE STORAGE************************
FIZZBUZ  DC    CL10'FIZZBUZZ!'       CREATE A STRING IN MEMORY, LABEL THE ADDRESS FIZZBUZ
FIZZ     DC    CL10'FIZZ!'           CREATE A STRING IN MEMORY, LABEL THE ADDRESS FIZZ
BUZZ     DC    CL10'BUZZ!'           CREATE A STRING IN MEMORY, LABEL THE ADDRESS BUZZ
NUM      DC    PL3'0'                CREATE A DECIMAL IN MEMORY, MAKE IT ZERO, LABEL IT NUM
TEMP     DS    D                     RESERVE A DOUBLE WORD (8 BYTES) IN MEMORY, LABEL IT TEMP
EMSK     DC    X'402020202020'       CREATE A HEX ARRAY IN MEMORY, LABEL IT EMSK
WTOSTART DC    Y(WTOEND-*,0)         LABEL THIS WTOSTART, DEFINE A CONSTANT ADDRESS EQUAL TO
*                                    "WTOEND" MINUS HERE(*)
EOUT     DS    CL10                  RESERVE SPACE FOR 10 CHARACTERS, LABEL THIS EOUT
WTOEND   EQU   *                     THE MEMORY ADDRESS LOCATED HERE IS LABELED WTOEND
**********HOUSE KEEPING AREA**********************
SAVE     DS    18F
         END   HELLO
```




## 6502 Assembly

The modulus operation is rather expensive on the 6502,
so a simple counter solution was chosen.
<lang>	.lf  fzbz6502.lst
	.cr  6502
	.tf  fzbz6502.obj,ap1
;------------------------------------------------------
; FizzBuzz for the 6502 by barrym95838 2013.04.04
; Thanks to sbprojects.com for a very nice assembler!
; The target for this assembly is an Apple II with
;   mixed-case output capabilities and Applesoft
;   BASIC in ROM (or language card)
; Tested and verified on AppleWin 1.20.0.0
;------------------------------------------------------
; Constant Section
;
FizzCt	 =   3		;Fizz Counter (must be < 255)
BuzzCt	 =   5		;Buzz Counter (must be < 255)
Lower	 =   1		;Loop start value (must be 1)
Upper	 =   100	;Loop end value (must be < 255)
CharOut	 =   $fded	;Specific to the Apple II
IntOut	 =   $ed24	;Specific to ROM Applesoft
;
### ================================================

	.or  $0f00
;------------------------------------------------------
; The main program
;
main	ldx  #Lower	;init LoopCt
	lda  #FizzCt
	sta  Fizz	;init FizzCt
	lda  #BuzzCt
	sta  Buzz	;init BuzzCt
next	ldy  #0		;reset string pointer (y)
	dec  Fizz	;LoopCt mod FizzCt == 0?
	bne  noFizz	;  yes:
	lda  #FizzCt
	sta  Fizz	;    restore FizzCt
	ldy  #sFizz-str	;    point y to "Fizz"
	jsr  puts	;    output "Fizz"
noFizz	dec  Buzz	;LoopCt mod BuzzCt == 0?
	bne  noBuzz	;  yes:
	lda  #BuzzCt
	sta  Buzz	;    restore BuzzCt
	ldy  #sBuzz-str	;    point y to "Buzz"
	jsr  puts	;    output "Buzz"
noBuzz	dey  		;any output yet this cycle?
	bpl  noInt	;  no:
	txa  		;    save LoopCt
	pha
	lda  #0		;    set up regs for IntOut
	jsr  IntOut	;    output itoa(LoopCt)
	pla
	tax  		;    restore LoopCt
noInt	ldy  #sNL-str
	jsr  puts	;output "\n"
	inx  		;increment LoopCt
	cpx  #Upper+1	;LoopCt >= Upper+1?
	bcc  next	;  no:  loop back
	rts  		;  yes:  end main
;------------------------------------------------------
; Output zero-terminated string @ (str+y)
;   (Entry point is puts, not outch)
;
outch	jsr  CharOut	;output string char
	iny  		;advance string ptr
puts	lda  str,y	;get a string char
	bne  outch	;output and loop if non-zero
	rts  		;return
;------------------------------------------------------
; String literals (in '+128' ascii, Apple II style)
;
str:	;		string base offset
sFizz	.az	-"Fizz"
sBuzz	.az	-"Buzz"
sNL	.az	-#13
;------------------------------------------------------
; Variable Section
;
Fizz	.da	#0
Buzz	.da	#0
;------------------------------------------------------
	.en
```



## 68000 Assembly

This implementation uses two counters instead of divisions for the moduli.

```68000devpac
;
; FizzBuzz for Motorola 68000 under AmigaOs 2+ by Thorham
;
; Uses counters instead of divisions.
;
_LVOOpenLibrary equ -552
_LVOCloseLibrary equ -414
_LVOVPrintf equ -954

execBase=4

start
    move.l  execBase,a6

    lea     dosName,a1
    moveq   #36,d0
    jsr     _LVOOpenLibrary(a6)
    move.l  d0,dosBase
    beq     exit

    move.l  dosBase,a6
    lea     counter,a2

    moveq   #3,d3   ; fizz counter
    moveq   #5,d4   ; buzz counter

    moveq   #1,d7
.loop
    clr.l   d5

; fizz
    subq.l  #1,d3
    bne     .noFizz
    moveq   #1,d5
    moveq   #3,d3
    move.l  #fizz,d1
    clr.l   d2
    jsr     _LVOVPrintf(a6)
.noFizz

; buzz
    subq.l  #1,d4
    bne     .noBuzz
    moveq   #1,d5
    moveq   #5,d4
    move.l  #buzz,d1
    clr.l   d2
    jsr     _LVOVPrintf(a6)
.noBuzz

; number
    tst.l   d5
    bne     .noNumber
    move.l  d7,(a2)
    move.l  #number,d1
    move.l  a2,d2
    jsr     _LVOVPrintf(a6)

.noNumber
    move.l  #newLine,d1
    clr.l   d2
    jsr     _LVOVPrintf(a6)

    addq.l  #1,d7
    cmp.l   #100,d7
    ble     .loop

exit
    move.l  execBase,a6
    move.l  dosBase,a1
    jsr     _LVOCloseLibrary(a6)
    rts
;
; variables
;
dosBase
    dc.l    0

counter
    dc.l    0
;
; strings
;
dosName
    dc.b    "dos.library",0

newLine
    dc.b    10,0

number
    dc.b    "%ld",0

fizz
    dc.b    "Fizz",0

buzz
    dc.b    "Buzz",0
```



## 8086 Assembly

Assembly programs that output a number on the screen are programmable in two ways: calculating the number in binary to convert it next in ASCII for output,
or keeping the number in Binary Coded Decimal (BCD) notation
to speed up the output to the screen, because
no binary to decimal conversion needs to be applied.

The first approach is the most useful because the binary number
is immediately recognizable to the computer, but, in a problem
where the calculations are very few and simple and the final result
is mainly text on the screen, using binary numbers would speed up
calculations, but will greatly slow down the output.

The BCD used is based on the ASCII text encoding:
zero is the hexadecimal byte 30, and nine is the hexadecimal byte 39.

The BCD number is kept in the DX register,
the most significant digit in DH and the less significant digit in DL.

See the comments for further explaining of the program's structure,
which is meant for speed and compactness rather than modularity:
there are no subroutines reusable in another program without being edited.

This program is 102 bytes big when assembled.
The program is written to be run in an IBM PC because the 8086 processor
alone does not provide circuitry for any kind of direct screen output.

At least, I should point out that this program is a little bugged:
the biggest number representable with the BCD system chosen is 99,
but the last number displayed is 100, which would be written as :0
because the program does provide overflow detecting only for the units,
not for tens (39 hex + 1 is 3A, that is the colon symbol in ASCII).

However, this bug is hidden by the fact that the number 100
is a multiple of five, so the number is never displayed,
because it is replaced by the string "buzz".

```asm
                    ; Init the registers
mov dx,03030h       ; For easier printing, the number is
                    ;kept in Binary Coded Decimal, in
----

                    ;the DX register.
mov ah,0Eh          ; 0Eh is the IBM PC interrupt 10h
                    ;function that does write text on
                    ;the screen in teletype mode.
mov bl,100d         ; BL is the counter (100 numbers).
xor cx,cx           ; CX is a counter that will be used
                    ;for screen printing.
xor bh,bh           ; BH is the counter for counting
                    ;multiples of three.

writeloop:          ; Increment the BCD number in DX.
inc dl              ; Increment the low digit
cmp dl,3Ah          ; If it does not overflow nine,
jnz writeloop1      ;continue with the program,
mov dl,30h          ;otherwise reset it to zero and
inc dh              ;increment the high digit
writeloop1:
inc bh              ; Increment the BH counter.
cmp bh,03h          ; If it reached three, we did
                    ;increment the number three times
                    ;from the last time the number was
                    ;a multiple of three, so the number
                    ;is now a multiple of three now,
jz writefizz        ;then we need to write "fizz" on the
                    ;screen.
cmp dl,30h          ; The number isn't a multiple of
jz writebuzz        ;three, so we check if it's a
cmp dl,35h          ;multiple of five. If it is, we
jz writebuzz        ;need to write "buzz". The program
                    ;checks if the last digit is zero or
                    ;five.
mov al,dh           ; If we're here, there's no need to
int 10h             ;write neither "fizz" nor "buzz", so
mov al,dl           ;the program writes the BCD number
int 10h             ;in DX
writespace:
mov al,020h         ;and a white space.
int 10h
dec bl              ; Loop if we didn't process 100
jnz writeloop       ;numbers.

programend:         ; When we did reach 100 numbers,
cli                 ;the program flow falls here, where
hlt                 ;interrupts are cleared and the
jmp programend      ;program is stopped.

writefizz:          ; There's need to write "fizz":
mov si,offset fizz  ; SI points to the "fizz" string,
call write          ;that is written on the screen.
xor bh,bh           ; BH, the counter for computing the
                    ;multiples of three, is cleared.
cmp dl,30h          ; We did write "fizz", but, if the
jz writebuzz        ;number is a multiple of five, we
cmp dl,35h          ;could need to write "buzz" also:
jnz writespace      ;check if the number is multiple of
                    ;five. If not, write a space and
                    ;return to the main loop.
writebuzz:          ; (The above code falls here if
                    ;the last digit is five, otherwise
                    ;it jumps)
mov si,offset buzz  ;SI points to the "buzz" string,
call write          ;that is written on the screen.
jmp writespace      ; Write a space to return to the main
                    ;loop.

write:              ; Write subroutine:
mov cl,04h          ; Set CX to the lenght of the string:
                    ;both strings are 4 bytes long.
write1:
mov al,[si]         ; Load the character to write in AL.
inc si              ; Increment the counter SI.
int 10h             ; Call interrupt 10h, function 0Eh to
                    ;write the character and advance the
                    ;text cursor (teletype mode)
loop write1         ; Decrement CX: if CX is not zero, do
ret                 ;loop, otherwise return from
                    ;subroutine.

fizz:               ;The "fizz" string.
db "fizz"

buzz:               ;The "buzz" string.
db "buzz"
```



## Z80 Assembly

For the Amstrad CPC (should work with e.g. the built-in assembler in JavaCPC; use <tt>call &4000</tt> to start from BASIC):

```z80
org &4000		; put code at memory address 0x4000
wr_char equ &bb5a ; write ASCII character in register A to screen
cursor equ &bb78 ; get cursor position

push bc
push de
push hl

ld b,100		; loop from 100 to 1
loop:

; check for Fizz condition
ld a,(count3)
dec a
jr nz,next3
push bc
ld b,4
ld de,fizz
printfizz:
ld a,(de)
call wr_char
inc de
djnz printfizz
pop bc
ld a,3
next3:
ld (count3),a

; check for Buzz condition
ld a,(count5)
dec a
jr nz,next5
push bc
ld b,4
ld de,buzz
printbuzz:
ld a,(de)
call wr_char
inc de
djnz printbuzz
pop bc
ld a,5
next5:
ld (count5),a

; test if cursor is still in first column
;   (i.e., no Fizz or Buzz has been printed)
call cursor
ld a,h
dec a
jr nz,skipnum

; print number
push bc
ld b,3
ld de,count
loop2:
ld a,(de)
call wr_char
inc de
djnz loop2
pop bc

skipnum:
; print carriage return/line feed
ld a,13
call wr_char
ld a,10
call wr_char

; increment rightmost digit
ld hl,count+2
inc (hl)
ld a,(hl)
; check if value is 10 (ASCII 58)
;   if so, set to 48 (ASCII 0) and increase 10's digit
cp 58
jr nz,noinc
ld a,48
ld (count+2),a
ld (hl),a
dec hl
inc (hl)

ld a,(hl)
; check second-to-right digit, if it is 10 (0), carry over to 100's
cp 58
jr nz,noinc
ld a,48
ld (count+1),a
ld (hl),a
dec hl
inc (hl)

noinc:

djnz loop

pop hl
pop de
pop bc

; return to BASIC
ret

count:
db "001"

count3:
db 3

count5:
db 5

fizz:
db "Fizz"

buzz:
db "Buzz"
```

