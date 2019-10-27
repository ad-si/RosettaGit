+++
title = "99 Bottles of Beer/Assembly"
description = ""
date = 2017-11-05T17:30:09Z
aliases = []
[extra]
id = 18274
[taxonomies]
categories = []
tags = []
+++

<!-- 
=Assembly= 
-->
{{collection|99 Bottles of Beer}} [[implementation of task::99 Bottles of Beer| ]]
[[99 Bottles of Beer]] done in any of the assembler-languages.

<!--
See [[99 Bottles of Beer/Assembly]]
-->

<!-- still missing:
MMIX
-->

__toc__


## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set.

```360asm
*        99 Bottles of Beer        04/09/2015
BOTTLES  CSECT
         USING  BOTTLES,R12
         LR     R12,R15
BEGIN    LA     R2,99              r2=99 number of bottles
         LR     R3,R2
LOOP     BCTR   R3,0               r3=r2-1
         CVD    R2,DW              binary to pack decimal 
         MVC    ZN,EDMASKN         load mask
         ED     ZN,DW+6            pack decimal (PL2) to char (CL4)
         CH     R2,=H'1'           if r2<>1
         BNE    NOTONE1            then goto notone1
         MVI    PG1+13,C' '        1 bottle
         MVI    PG2+13,C' '        1 bottle
NOTONE1  MVC    PG1+4(2),ZN+2      insert bottles
         MVC    PG2+4(2),ZN+2      insert bottles
         CVD    R3,DW              binary to pack decimal 
         MVC    ZN,EDMASKN         load mask
         ED     ZN,DW+6            pack decimal (PL2) to char (CL4)
         MVC    PG4+4(2),ZN+2      insert bottles
         WTO    MF=(E,PG1)
         WTO    MF=(E,PG2)
         WTO    MF=(E,PG3)
         CH     R3,=H'1'           if r3<>1
         BNE    NOTONE2            then goto notone2
         MVI    PG4+13,C' '        1 bottle
NOTONE2  LTR    R3,R3              if r3=0
         BZ     ZERO               then goto zero
         WTO    MF=(E,PG4)
         B      PR5
ZERO     WTO    MF=(E,PG4Z)
PR5      WTO    MF=(E,PG5)
         BCT    R2,LOOP
RETURN   XR     R15,R15
         BR     R14
         CNOP   0,4
PG1      DC     H'40',H'0',CL40'xx bottles of beer on the wall'
PG2      DC     H'40',H'0',CL40'xx bottles of beer'
PG3      DC     H'40',H'0',CL40'Take one down, pass it around'
PG4      DC     H'40',H'0',CL40'xx bottles of beer on the wall'
PG5      DC     H'40',H'0',CL40' '
PG4Z     DC     H'40',H'0',CL40'No more bottles of beer on the wall'
DW       DS     0D,PL8             15num
ZN       DS     CL4
EDMASKN  DC     X'40202120'        CL4  3num
WTOMSG   CNOP   0,4
         DC     H'80'              length of WTO buffer
         DC     H'0'               must be binary zeroes
         YREGS
         END    BOTTLES
```

{{out}}
<pre style="height:20ex">
...
 5 bottles of beer on the wall
 5 bottles of beer
Take one down, pass it around
 4 bottles of beer on the wall
 
 4 bottles of beer on the wall
 4 bottles of beer
Take one down, pass it around
 3 bottles of beer on the wall
 
 3 bottles of beer on the wall
 3 bottles of beer
Take one down, pass it around
 2 bottles of beer on the wall
 
 2 bottles of beer on the wall
 2 bottles of beer
Take one down, pass it around
 1 bottle  of beer on the wall
 
 1 bottle  of beer on the wall
 1 bottle  of beer
Take one down, pass it around
No more bottles of beer on the wall

```



## 6502 Assembly

IMPORTANT NOTE:  This assembly language solution is targeted at the Apple 1. 

The Apple 1 was an innovative device for its time, 
but it's quite primitive by modern standards, 
and it had NO support for lower-case letters.  

Therefore, the UPPER-CASE output of this example accurately represents 
the only reasonable one for this device, 
and cannot be "fixed" due to non-compliance, only deleted.


```6502 Assembly
   .CR 6502
   .TF AP1BEER.O,AP1
   .LF AP1BEER.LST
   .OR $0BEE
;-------------------------------------;
; BEER SONG IN 6502 ASSEMBLY LANGUAGE ;
;       BY BARRYM 2010-05-30          ;
; THANKS TO SBPROJECTS.COM FOR LOTS   ;
;   OF VALUABLE INFORMATION AND A     ;
;   VERY NICE ASSEMBLER!              ;
;-------------------------------------;
; THE TARGET MACHINE FOR THIS PROGRAM ;
;   IS THE APPLE 1, BUT IT WOULD BE   ;
;   EASY TO MAKE IT RUN ON OTHER 65XX ;
;   MACHINES BY CHANGING THE NEXT TWO ;
;   EQUATES.  SOME MACHINE-TESTED     ;
;   EXAMPLES:                         ;
;   APPLE II, +, E, C: $FDED, $80     ;
;   COMMODORE 64:      $FFD2, $00     ;
;-------------------------------------;
ECHO     =  $FFEF  ;EMIT A REG AS ASCII
ORMASK   =  $80    ;($00 FOR + ASCII)
;
MAXBEER  =  99     ;INITIAL BEER COUNT
;-------------------------------------;
; X REG. IS THE BOTTLE COUNTER.       ;
; Y REG. IS THE STRING INDEX POINTER, ;
;   AND THE TENS DIGIT IN THE BINARY- ;
;   TO-ASCII CONVERSION ROUTINE.      ;
; A REG. HANDLES EVERYTHING ELSE WITH ;
;   A LITTLE HELP FROM THE STACK.     ;
; ZERO PAGE ISN'T DIRECTLY DISTURBED. ;
;-------------------------------------;
; EMIT COMPLETE CORRECT SONG ADJUSTED ;
;   FOR UPPER-CASE 40-COLUMN DISPLAY. ;
;-------------------------------------;
   LDX #MAXBEER    ;X=MAXBEER
   BNE PRSONG      ;SING THE SONG & RTS
;-------------------------------------;
; EMIT WHOLE SONG UP TO LAST SENTENCE.;
;-------------------------------------;
BEERME:
   LDY #TAKE1-TXT  ;? "TAKE ... AROUND,"
   JSR PRBOB       ;? X;" BOT ... WALL."
PRSONG: ;          ;?
   LDY #CR-TXT     ;? X;" BOT ... WALL," 
   JSR PRBOB       ;? X;" BOT ... BEER."
   DEX             ;X=X-1
   BPL BEERME      ;IF X>=0 THEN BEERME
;-------------------------------------;
; EMIT LAST SENTENCE AND FALL THROUGH.;
;-------------------------------------;
   LDX #MAXBEER    ;X=MAXBEER:
;                  ;? "GO TO ... MORE,"
;-------------------------------------;
; PRINT A PROPERLY PUNCTUATED "BOTTLE ;
;   OF BEER" SENTENCE.                ;
;-------------------------------------;
PRBOB:
   TYA
   PHA             ;SAVE THE PRE$ PTR
   JSR PUTS        ;? PRE$;
   TXA             ;IF X=0 THEN
   BEQ PRBOTT      ;   ? "NO MORE";
   LDY #"0"-1      ;ELSE
   SEC             ;(
DIV10:
   SBC #10         ;   Y=INT(X/10)
   INY
   BCS DIV10
   ADC #10+'0'
   CPY #"0"
   BEQ ONEDIG
   PHA             ;   IF Y>0 THEN
   TYA                   ? Y;
   JSR PUTCH
   PLA             ;   ? X MOD 10;
ONEDIG:
   LDY #BOTTL-TXT  ;)
PRBOTT:
   JSR PUTCH       ;? " BOTTLE";
   CPX #1
   BNE PLURAL
   INY             ;IF X<>1 THEN ? "S";
PLURAL:
   JSR PUTS        ;? " OF BEER";
   PLA             ;RECALL THE PRE$ PTR
   CMP #COMCR-TXT
   BEQ PRDOT
   PHA             ;IF APPROPRIATE THEN
   JSR PUTS        ;   ? " ON THE WALL";
   PLA
   LDY #COMCR-TXT  ;IF APPROPRIATE THEN
   CMP #CR-TXT     ;   ? ",":
   BEQ PRBOB       ;   ? X;" ... BEER";
PRDOT:
   LDY #DOTCR-TXT  ;? "."
;-------------------------------------;
; EMIT A HI-BIT-SET TERMINATED STRING ;
;   @ OFFSET Y AND EXIT WITH Y @ THE  ;
;   BEGINNING OF THE NEXT STRING.     ;
;-------------------------------------;
PUTS:
   LDA TXT,Y       ;GRAB A STRING CHAR
   INY             ;ADVANCE STRING PTR
PUTCH:
   PHA
   ORA #ORMASK
   AND #ORMASK+127 ;FORMAT CHAR FOR ECHO
   JSR ECHO        ;SHOOT IT TO CONSOLE
   PLA
   BPL PUTS        ;LOOP IF APPROPRIATE
   RTS
;-------------------------------------;
; OPTIMIZED SONG LYRIC STRINGS.       ;
;-------------------------------------;
TXT:
TAKE1:
   .AS "TAKE ONE DOWN AND"
   .AS " PASS IT AROUND"
COMCR:
   .AS ","
CR:
   .AT #13
   .AS "NO MORE"
BOTTL:
   .AT " BOTTLE"
   .AT "S OF BEER"
   .AT " ON THE WALL"
DOTCR:
   .AT ".",#13
   .AS "GO TO THE STORE AND"
   .AT " BUY SOME MORE,",#13
   .EN
;-------------------------------------;
; APPLE 1 MONITOR HEX DUMP FOLLOWS.   ;
; ENTER THE 200 BYTES AS SHOWN INTO   ;
;   WOZMON AND LET THE BEER FLOW!!    ;
;-------------------------------------;
0BEE
:A2 63 D0 05 A0 00 20 01 0C A0 21 20 01
:0C CA 10 F3 A2 63 98 48 20 3C 0C 8A F0
:16 A0 AF 38 E9 0A C8 B0 FB 69 3A C0 B0
:F0 06 48 98 20 40 0C 68 A0 29 20 40 0C
:E0 01 D0 01 C8 20 3C 0C 68 C9 20 F0 0B
:48 20 3C 0C 68 A0 20 C9 21 F0 C7 A0 45
:B9 4C 0C C8 48 09 80 29 FF 20 EF FF 68
:10 F1 60 54 41 4B 45 20 4F 4E 45 20 44
:4F 57 4E 20 41 4E 44 20 50 41 53 53 20
:49 54 20 41 52 4F 55 4E 44 2C 8D 4E 4F
:20 4D 4F 52 45 20 42 4F 54 54 4C C5 53
:20 4F 46 20 42 45 45 D2 20 4F 4E 20 54
:48 45 20 57 41 4C CC 2E 8D 47 4F 20 54
:4F 20 54 48 45 20 53 54 4F 52 45 20 41
:4E 44 20 42 55 59 20 53 4F 4D 45 20 4D
:4F 52 45 2C 8D
BEER
```



## 6800 Assembly



```6800 Assembly
        .cr  6800
        .tf  beer6800.obj,AP1
        .lf  beer6800
;
### ===============================================
;
;    Beer Song for the Motorola 6800 microprocessor   ;
;                 by barrym 2011-04-19                ;
;-----------------------------------------------------;
; Prints the correct, complete song lyrics to a full  ;
;   ascii terminal (console) connected to a 1970s     ;
;   vintage SWTPC 6800 system, which is the target    ;
;   device for this assembly.                         ;
; Many thanks to:                                     ;
;   swtpc.com for hosting Michael Holley's documents! ;
;   sbprojects.com for a very nice assembler!         ;
;   swtpcemu.com for a very capable emulator!         ;
; The 6800 microprocessor is the slightly older, less ;
;   popular, and more expensive step-brother of the   ;
;   6502.  Numerous similarities exist between the    ;
;   assembly languages of the two, but the 6800 has   ;
;   its own distinct flavor, which is (judging by how ;
;   compact the code ended up) well suited to this    ;
;   type of small program.  I am especially impressed ;
;   with the two-byte 'bsr' instruction, and I make   ;
;   extensive use of it here.                         ;
; Effort was made to keep the code footprint as small ;
;   as possible by re-using substrings and code in a  ;
;   hacker-like style that makes the program flow a   ;
;   bit strange to the human eye (the 6800 gobbles it ;
;   up without complaint).  The final tally: 97 bytes ;
;   of instructions, 108 bytes of text, and about 11  ;
;   bytes of stack.  This includes integer-to-ascii   ;
;   conversion, blank line between verses, removal of ;
;   "s" from "1 bottles", substitution of "no more"   ;
;   for "0", and proper capitalization of "No more".  ;
; reg b is the beer counter                           ;
; reg x is the string pointer                         ;
; reg a handles everything else (with a little help   ;
;                             from the system stack)  ;
;-----------------------------------------------------;
outeee   =   $e1d1      ;ROM: console putchar routine
stbeer   =   99         ;Must be in the range [0..99]
        .or  $0f00
;
### ===============================================
;
; Initialize, sing the song, and exit                 ;
;-----------------------------------------------------;
main    ldab #stbeer    ;Beer count = stbeer
        bsr  prsong     ;Sing the entire song
        swi             ;Return to the monitor.
;
### ===============================================
;
; Emit the entire song up to the last sentence        ;
;-----------------------------------------------------;
beerme  bsr  prbob2     ;Emit second sentence of verse
prsong  ldx  #nline     ;Blank line between verses
        ldaa #'N'       ;First sentence type = 'N'
        bsr  prbob      ;Emit 1st sentence of verse
        decb            ;Beer count -= 1
        bpl  beerme     ;If beer count >= 0 then beerme
;
### ===============================================
;
; Set up the last sentence and fall through to prbob2 ;
;-----------------------------------------------------;
        ldab #stbeer    ;Beer count = stbeer
        ldx  #store     ;x$ = "Go to the store ..."
;
### ===============================================
;
; Emit a properly punctuated bottle-of-beer sentence, ;
;   using beer counter in reg b, pre-string pointer   ;
;   in reg x, and the sentence type in reg a ('N' =   ;
;   sentence 1, 'o' = sentence 1.5, 'n' = sentence 2) ;
;-----------------------------------------------------;
prbob2  ldaa #'n'       ;Second sentence type = 'n'
prbob   psha            ;Stack sentence type for later
        bsr  puts       ;Emit pre-string
        pula            ;Check sentence type and use
        psha            ;  it to prepare the upper- or
        anda #'n'       ;  lower-case of "no more"
        ldx  #omore     ;x$ = "o more bottle"
        tstb            ;If beer count = 0 then
        beq  prbott     ;  skip over the i-to-a
        ldx  #bottl     ;x$ = " bottle"
;
### ===============================================
;
; I-to-A (inline): convert int in b to ascii and emit ;
;    with leading zero suppression (0 <= # <= 99)!    ;
;-----------------------------------------------------;
        pshb            ;Stack beer count
        ldaa #-1        ;  (divten trashes it)
divten  subb #10        ;b = ones digit - 10
        inca            ;a = tens digit
        bcc  divten     ;If a = 0 then
        beq  onedig     ;  suppress leading zero
        adda #"0"       ;else translate tens digit to
        bsr  putch      ;  shifted ascii and emit
onedig  addb #'0'+10    ;Translate ones digit to ascii
        tba             ;  and leave it in a for putch
        pulb            ;Restore beer count
;-----------------------------------------------------;
prbott  bsr  putch      ;Emit a;x$;
        cmpb #1         ;If beer count = 1
        bne  plural     ;then
        inx             ;  skip over the "s"
plural  bsr  puts       ;Emit " ... beer";
        pula            ;Restore sentence type
        cmpa #'o'       ;If type <> 'o'
        beq  putdot     ;then
        psha            ;  emit " on the wall";
        bsr  puts       ;  if type = 'N' then loop
        pula            ;    back to finish the
        adda #33        ;    first sentence with
        bpl  prbob      ;    type = 'o', x$ = ", "
putdot  ldx  #dotnl     ;x$ = ".\n"
;
### ===============================================
;
; Emit string @ x and leave x @ start of next string  ;
;-----------------------------------------------------;
puts    ldaa 0,x        ;a = raw character removed
        inx             ;  from the beginning of x$
;
### ===============================================
;
; Emit a as ascii and loop into x$ if hi-bit is clear ;
;-----------------------------------------------------;
putch   psha            ;Stack raw char
        anda #$7f       ;Mask off the hi-bit
        jsr  outeee     ;Emit a as 7-bit ascii
        pula            ;Restore raw char
        tsta            ;If hi-bit is clear then
        bpl  puts       ;  loop back into x$
        rts             ;All 8 'bsr's use this 'rts'!
;
### ===============================================
;
; Optimized song lyric strings, carefully arranged to ;
;   allow the prbob subroutine to take full advantage ;
;   of the x register side-effects of puts            ;
;-----------------------------------------------------;
omore   .as  "o more"
bottl   .at  " bottle"
        .at  "s of beer"
        .at  " on the wall"
        .at  ", "
dotnl   .as  "."
nline   .at  #13,#10
        .at  "Take one down and pass it around, "
store   .at  "Go to the store and buy some more, "
        .en
;
### ===============================================
;
; The following is a hex dump of the object file,     ;
;   suitable for copying and pasting into the 6800    ;
;   emulator available at swtpcemu.com!               ;
;-----------------------------------------------------;
e0F00 C6 63 8D 03 3F 8D 0F CE 0F 86 86 4E 8D 0A 5A 2A
e0F10 F4 C6 63 CE 0F AA 86 6E 36 8D 38 32 36 84 6E CE
e0F20 0F 61 5D 27 15 CE 0F 67 37 86 FF C0 0A 4C 24 FB
e0F30 27 04 8B B0 8D 20 17 8B 3A 33 8D 1A C1 01 26 01
e0F40 08 8D 10 32 81 6F 27 08 36 8D 08 32 8B 21 2A C8
e0F50 CE 0F 85 A6 00 08 36 84 7F BD E1 D1 32 4D 2A F3
e0F60 39 6F 20 6D 6F 72 65 20 62 6F 74 74 6C E5 73 20
e0F70 6F 66 20 62 65 65 F2 20 6F 6E 20 74 68 65 20 77
e0F80 61 6C EC 2C A0 2E 0D 8A 54 61 6B 65 20 6F 6E 65
e0F90 20 64 6F 77 6E 20 61 6E 64 20 70 61 73 73 20 69
e0FA0 74 20 61 72 6F 75 6E 64 2C A0 47 6F 20 74 6F 20
e0FB0 74 68 65 20 73 74 6F 72 65 20 61 6E 64 20 62 75
e0FC0 79 20 73 6F 6D 65 20 6D 6F 72 65 2C A0
j0F00
```



## ARM Assembly

<!--  printf missing ? --->
<lang ARM_Assembly>
.global main

main:
    mov r0, #99

loop:
    push {r0}
    mov r1, r0
    mov r2, r0
    sub r3, r0, #1
    ldr r0, =lyric
    bl printf
    pop {r0}
    
    sub r0, r0, #1
    cmp r0, #0
    bgt loop

    ldr r0, =last_lyric
    bl printf

    mov r7, #1
    swi 0
    
lyric:
    .ascii "%d bottles of beer on the wall\n"
    .ascii "%d bottles of beer\n"
    .ascii "Take one down, pass it around\n"
    .ascii "%d bottles of beer on the wall\n\n\000"

last_lyric:
    .ascii "No more bottles of beer on the wall, no more bottles of beer.\n"
    .ascii "Go to the store and buy some more, 99 bottles of beer on the wall\n\000"

```



## LLVM


```llvm
; "99 Bottles of Beer on the Wall" in LLVM Assembly

; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; The song lyrics are global constants.
; Lyrics for plural verses:
@pluralVerse = private constant [120 x i8]
c"%d bottles of beer on the wall, %d bottles of beer.\0ATake one down and pass it around, %d bottles of beer on the wall.\0A\0A\00"
; Lyrics for the singular verse:
@singularVerse = private constant [121 x i8]
    c"1 bottle of beer on the wall, 1 bottle of beer.\0ATake one down and pass it around, no more bottles of beer on the wall.\0A\0A\00"
; Lyrics for the final verse:
@finalVerse = private constant [130 x i8]
    c"No more bottles of beer on the wall, no more bottles of beer.\0AGo to the store and buy some more, %d bottles of beer on the wall.\0A\00"

; Initial number of bottles of beer.
; This must be a natural number.
@initialVerseNumber = private constant i32 99

; The declaration for the external C printf function.
declare i32 @printf(i8*, ...)

; Prints a verse, with %numberOfBottles being the initial number of bottles
; in that verse.
define fastcc void @printVerse(i32 %numberOfBottles) {
    switch i32 %numberOfBottles, 
        label %pluralVerse
        [ i32 1, label %singularVerse
          i32 0, label %finalVerse ]

pluralVerse:
    %pluralVersePointer = getelementptr [120 x i8]* @pluralVerse, i64 0, i64 0
    %newNumberOfBottles = sub i32 %numberOfBottles, 1
    call i32(i8*, ...)* @printf(
        i8* %pluralVersePointer,
        i32 %numberOfBottles,
        i32 %numberOfBottles,
        i32 %newNumberOfBottles)
    ret void

singularVerse:
    %singularVersePointer = getelementptr [121 x i8]* @singularVerse,i64 0,i64 0
    call i32(i8*, ...)* @printf(i8* %singularVersePointer)
    ret void

finalVerse:
    %finalVersePointer = getelementptr [130 x i8]* @finalVerse, i64 0, i64 0
    %initialVerseNumberL = load i32* @initialVerseNumber
    call i32(i8*, ...)* @printf(i8* %finalVersePointer,i32 %initialVerseNumberL)
    ret void
}

define i32 @main() { 
loopHeader:
    %initialVerseNumberL = load i32* @initialVerseNumber
    br label %loop ; This br terminates the first basic block.
loop:
    %verseNumber = 
        phi i32 [%initialVerseNumberL, %loopHeader], [%nextVerseNumber, %do]
    %cond = icmp eq i32 -1, %verseNumber
    br i1 %cond, label %break, label %do
do:
    call fastcc void @printVerse(i32 %verseNumber)
    %nextVerseNumber = sub i32 %verseNumber, 1
    br label %loop
break:
    ret i32 0
}
```


<!-- missing here:

## MMIX

-->


## OASYS Assembler

The following example demonstrates the use of pointer variables (the argument <tt>,^#</tt> to the <tt>&VERSE#</tt> method), and therefore may not be as efficient as one which does not use pointer variables.
<lang oasys_oaa>
; Beer program with OASYS assembler.

[&]
%@*>"Type 'beer' for beer.~Type 'quit' to quit.~"PS

['BEER]
,#99>:+,#&VERSE#/"No more bottles of beer on the wall.~"PS

[&VERSE#,^#]
,^#<<PI" bottles of beer on the wall.~"PS
,^#<<PI" bottles of beer.~Take one down and pass it around,~"PS
,^#<,^#<<DN>,^#<<\
,^#<<PI" bottles of beer on the wall.~"PS CR 1RF: 0RF

['QUIT]
GQ

```



## X86 Assembly



### Using Windows/MASM32


```asm
.386
.model flat, stdcall
option casemap :none

include \masm32\include\kernel32.inc
include \masm32\include\masm32.inc
include \masm32\include\user32.inc
includelib \masm32\lib\kernel32.lib
includelib \masm32\lib\masm32.lib
includelib \masm32\lib\user32.lib

.DATA
 buffer db 1024 dup(?)
 str1 db "%d bottles of beer on the wall.",10,13,0
 str2 db "%d bottles of beer",10,13,0
 str3 db "Take one down, pass it around",10,13,0
 str4 db "No more bottles of beer on the wall!",10,13,0
 nline db 13,10,0

 bottles dd 99

.CODE
 start:
  INVOKE wsprintfA, offset buffer, offset str1, [bottles]
  INVOKE StdOut, offset buffer

  INVOKE wsprintfA, offset buffer, offset str2, [bottles]
  INVOKE StdOut, offset buffer

  INVOKE StdOut, offset str3

  DEC [bottles]

  INVOKE wsprintfA, offset buffer, offset str1, [bottles]
  INVOKE StdOut, offset buffer
  INVOKE StdOut, offset nline

  CMP [bottles], 1
  JNE start

  INVOKE StdOut, offset str4
  INVOKE ExitProcess, 0
 end start
```



### using DOS/BIOS


```asm

[bits 16]
DrinkBeer:
	push ds
	push si
	push ax
	mov ax, cs
	mov ds, ax
	mov ax, 99
	.beer_loop:
		call .printHexNumber
		mov si, .dataBeerSong1
		call .printString
		call .printHexNumber
		mov si, .dataBeerSong2
		call .printString
		dec ax
		call .printHexNumber
		mov si, .dataBeerSong3
		call .printString
		test ax, ax
		jnz .beer_loop
	pop ax
	pop si
	pop ds
	ret
	
	.printString:
		push ax
		push si
	 .looping:
		lodsb
		test al, al
		jz .done
		mov ah, 0Eh
		int 10h
		jmp .looping
	 .done:
		pop si
		pop ax
		ret
		
	.printHexNumber:
			pusha
			push ds
			mov ax, cs
			mov ds, ax
			push word 0
			mov bx, ax
			xor dx, dx
			mov cx, 4r
	 .convert_loop:
			mov ax, bx
			and ax, 0Fh
			cmp ax, 9
			ja  .greater_than_9
			add ax, '0'
			jmp .converted
	 .greater_than_9:
			add ax, 'A'-0Ah
	 .converted:
			push ax
			shr bx, 4
			dec cx
			jnz .convert_loop
	 .popoff:
			pop ax
			cmp ax, 0
			je .done
			mov ah, 0Eh
			int 10h
			jmp .popoff
		.done:
			pop ds
			popa
			ret
		
	.dataHelloWorld: db "Hello World!", 0
	.dataBeerSong1: db " bottles of beer on the wall ", 0
	.dataBeerSong2: db " bottles of beer", 13, 10, "Take one down, pass it around "
	.dataBeerSong3: db 0, " bottles of beer on the wall", 0


```



### Implemented in the nasm preprocessor


```asm
bits 32

section .data 
str:
%assign bottles 99 
%rep 99
  %defstr bottles_str bottles
  %if bottles == 1
    %define bottle_plur " bottle"
  %else
    %define bottle_plur " bottles"
  %endif
  db bottles_str, bottle_plur, " of beer on the wall", 10
  db bottles_str, bottle_plur, " of beer", 10
  db "Take one down, pass it around", 10, 10
%assign bottles bottles-1 
%endrep
db "0 bottles of beer on the wall", 10
str_len: equ $ - str

section .text
global _start 
_start:
    mov edx, str_len
    mov ecx, str
    mov ebx, 1
    mov eax, 4
    int 0x80

    mov ebx, 0
    mov eax, 1
    int 0x80
```


===x86_64 (GAS)===
Could maybe have done it all with macros, but I wanted to write my own itoa function. Plus I feel like using the preprocessor for its looping directives takes the challenge out of the problem. To extend to larger numbers, all you have to do is increase the size of the buffer and modify START_BOTTLES.
<lang>// Compiles with `gcc -nostdlib`
#define SYS_EXIT    $60
#define SYS_WRITE   $1

#define STDOUT          $1

// Some numbers:
#define START_BOTTLES   99
#define NUM_LOCALS      $8
#define WRL1_LEN        $30
#define WRL2_LEN        $53
#define WRL3_LEN        $31

.global _start
.text

.macro WRITE
    movq    STDOUT, %rdi
    movq    SYS_WRITE, %rax
    syscall
.endm

.macro WRITENUM
    movq    8(%rsp), %rdx
    movq    (%rsp), %rsi
    WRITE
.endm

/* void* itoa(long, char *) 
             0q     8q
   - char * points to the *back* of the string. itoa writes from ls digit.
   - clobbers rdi, rax, rdx
   - returns pointer to beginning of string
*/
itoa:
    pushq   %rbp
    movq    %rsp, %rbp
    
    movq    16(%rsp), %rdi
    movq    24(%rsp), %rax
    
    cycledigits:
        xorq    %rdx, %rdx
        divq    decimal
        addq    $48, %rdx       // Add 48 to remainder, store digit
        movb    %dl, (%rdi)     // Copy char
        decq    %rdi            // Next digit
        cmpq    $0, %rax
        jg      cycledigits     // No more digits?
    
    leaq    1(%rdi), %rax       // return value

    popq    %rbp
    ret

_start:
    // Set up stack
    movq    %rsp, %rbp
    /*  
        bptr = itoa(counter, numstring)
        do
        {
            write(stdout, bptr, bptr-numstring+1) // number
            write(stdout, regstring, 30)
            write(stdout, bptr, bptr-numstring+1) // number
            write(stdout, regstring2, 52)
            counter-=1
            bptr = itoa(counter, numstring)
            write(stdout, bptr, bptr-numstring+1) // number
            write(stdout, regstring3, 30)
        } while(counter>0)
    */
    subq    NUM_LOCALS, %rsp
    pushq   counter
    pushq   $numstring
    precall:
    call    itoa
    addq    $16, %rsp       // clean args
    movq    %rax, (%rsp)      // bptr = itoa(counter, numstring)
    
    subq    $numstring, %rax
    negq    %rax
    leaq    1(%rax), %rdx
    movq    %rdx, 8(%rsp)   // Save the calculation
    
    printloop:
        WRITENUM       // write(stdout, bptr, bptr-numstring+1)
        
        writeline1:
        movq    WRL1_LEN, %rdx
        movq    $regstring, %rsi
        WRITE               // write(stdout, regstring, 30)
        
        WRITENUM       // write(stdout, bptr, bptr-numstring+1)
        
        writeline2:
        movq    WRL2_LEN, %rdx
        movq    $regstring2, %rsi
        WRITE               // write(stdout, regstring2, 52)
        decq    counter     // counter--
        
        pluralcheck:
        cmpq    $1, counter
        jg      norm
        cmpq    $0, counter
        je      zeroconfirm
        
        oneconfirm:
        movq    $regstring, %rdx
        movb    $0x20 , 7(%rdx)
        movq    $regstring2, %rdx
        movb    $0x20 , 7(%rdx)
        movq    $regstring3, %rdx
        movb    $0x20 , 7(%rdx)
        jg      norm
        
        zeroconfirm:
        movq    $regstring, %rdx
        movb    $'s, 7(%rdx)
        movq    $regstring2, %rdx
        movb    $'s , 7(%rdx)
        movq    $regstring3, %rdx
        movb    $'s , 7(%rdx)
        
        norm:
        pushq   counter
        pushq   $numstring
        call    itoa
        addq    $16, %rsp
        movq    %rax, (%rsp)    // bptr = itoa(counter, numstring)
        
        write3:
        subq    $numstring, %rax
        negq    %rax
        leaq    1(%rax), %rdx
        movq    %rdx, 8(%rsp)
        movq    (%rsp), %rsi
        WRITE               // write(stdout, bptr, bptr-numstring+1)
        
        writeline3:
        movq    WRL3_LEN, %rdx
        movq    $regstring3, %rsi
        WRITE               // write(stdout, regstring, 30)
        
        cmpq    $0, counter
        jg      printloop

exit:
    movq    SYS_EXIT, %rax
    xorq    %rdi, %rdi              // The exit code.
    syscall

.data
    /* Begin Data Section: */
    decimal:  // base 10
        .quad 10
    counter:
        .quad   START_BOTTLES    
    buffer:
        .ascii "xxx"     /* Separated out because want back of string */
    numstring:
        .byte 'x
    regstring:
        .ascii " bottles of beer on the wall,\n"
    regstring2:
        .ascii " bottles of beer.\nTake one down, and pass it around:\n"
    regstring3:
        .ascii " bottles of beer on the wall.\n\n"

```



## Z80 Assembly

For Sinclair ZX Spectrum.

```z80
org 32768

start:
 ld      a, 2                  ; Spectrum: channel 2 = "S" for screen
 call    $1601                 ; Spectrum: Select print channel using ROM

 ld c,99                       ; Number of bottles to start with

loopstart:
 call printc                   ; Print the number of bottles
 ld hl,line1                   ; Print the rest of the first line
 call printline

 call printc                   ; Print the number of bottles
 ld hl,line2_3                 ; Print rest of the 2nd and 3rd lines
 call printline

 dec c                         ; Take one bottle away
 call printc                   ; Print the number of bottles
 ld hl,line4                   ; Print the rest of the fourth line
 call printline

 ld a,c
 cp 0                          ; Out of beer bottles?
 jp nz,loopstart               ; If not, loop round again
 ret                           ; Return to BASIC

printc:                        ; Routine to print C register as ASCII decimal
 ld a,c
 call dtoa2d                   ; Split A register into D and E

 ld a,d                        ; Print first digit in D
 cp '0'                        ; Don't bother printing leading 0
 jr z,printc2
 rst 16                        ; Spectrum: Print the character in 'A'

printc2:
 ld a,e                        ; Print second digit in E
 rst 16                        ; Spectrum: Print the character in 'A'
 ret

printline:                     ; Routine to print out a line
 ld a,(hl)                     ; Get character to print
 cp '$'                        ; See if it '$' terminator
 jp z,printend                 ; We're done if it is
 rst 16                        ; Spectrum: Print the character in 'A'
 inc hl                        ; Move onto the next character
 jp printline                  ; Loop round

printend:
 ret

dtoa2d:                        ; Decimal to ASCII (2 digits only), in: A, out: DE
 ld d,'0'                      ; Starting from ASCII '0' 
 dec d                         ; Because we are inc'ing in the loop
 ld e,10                       ; Want base 10 please
 and a                         ; Clear carry flag

dtoa2dloop:
 inc d                         ; Increase the number of tens
 sub e                         ; Take away one unit of ten from A
 jr nc,dtoa2dloop              ; If A still hasn't gone negative, do another
 add a,e                       ; Decreased it too much, put it back
 add a,'0'                     ; Convert to ASCII
 ld e,a                        ; Stick remainder in E
 ret

; Data
line1:    defb ' bottles of beer on the wall,',13,'$'
line2_3:  defb ' bottles of beer,',13,'Take one down, pass it around,',13,'$'
line4:    defb ' bottles of beer on the wall.',13,13,'$'
```

