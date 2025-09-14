+++
title = "Draw a pixel"
description = ""
date = 2019-10-10T14:07:00Z
aliases = []
[extra]
id = 21825
[taxonomies]
categories = ["task", "GUI"]
tags = []
languages = [
  "arm_assembly",
  "basic256",
  "bbc_basic",
  "c",
  "commodore_basic",
  "factor",
  "go",
  "java",
  "julia",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "nim",
  "oberon",
  "objeck",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "processing",
  "purebasic",
  "python",
  "qbasic",
  "racket",
  "ring",
  "robotic",
  "rust",
  "scala",
  "smilebasic",
  "vba",
  "wee_basic",
  "zx_spectrum_basic",
]
+++

{{task|GUI}}[[Category:Basic language learning]]{{requires|Graphics}} [[Category:Simple]]

## Task

Create a window and draw a pixel in it, subject to the following:

::#  the window is 320 x 240
::#  the color of the pixel must be red (255,0,0)
::#  the position of the pixel is x = 100, y = 100


## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program dpixel.s        */

/* compile with as         */
/* link with gcc and options -lX11 -L/usr/lpp/X11/lib   */

/********************************************/
/*Constantes                                */
/********************************************/
.equ STDOUT,              1     @ Linux output console
.equ EXIT,                1     @ Linux syscall
.equ WRITE,               4     @ Linux syscall
/* constantes X11 */
.equ KeyPressed,          2
.equ ButtonPress,         4
.equ MotionNotify,        6
.equ EnterNotify,         7
.equ LeaveNotify,         8
.equ Expose,              12
.equ ClientMessage,       33
.equ KeyPressMask,        1
.equ ButtonPressMask,     4
.equ ButtonReleaseMask,   8
.equ ExposureMask,        1<<15
.equ StructureNotifyMask, 1<<17
.equ EnterWindowMask,     1<<4
.equ LeaveWindowMask,     1<<5 
.equ ConfigureNotify,     22


/*******************************************/
/* DONNEES INITIALISEES                    */
/*******************************************/ 
.data
szWindowName:            .asciz "Windows Raspberry"
szRetourligne:           .asciz  "\n"
szMessDebutPgm:          .asciz "Program start. \n"
szMessErreur:            .asciz "Server X not found.\n"
szMessErrfen:            .asciz "Can not create window.\n"
szMessErreurX11:         .asciz "Error call function X11. \n"
szMessErrGc:             .asciz "Can not create graphics context.\n"
szTitreFenRed:           .asciz "Pi"    
szTexte1:                .asciz "<- red pixel is here !!"
.equ LGTEXTE1, . - szTexte1
szTexte2:                .asciz "Press q for close window or clic X in system menu."
.equ LGTEXTE2, . - szTexte2
szLibDW: .asciz "WM_DELETE_WINDOW"    @ special label for correct close error

/*************************************************/
szMessErr: .ascii	"Error code hexa : "
sHexa: .space 9,' '
         .ascii "  decimal :  "
sDeci: .space 15,' '
         .asciz "\n"

/*******************************************/
/* DONNEES NON INITIALISEES                    */
/*******************************************/ 
.bss
.align 4
ptDisplay:          .skip 4      @ pointer display
ptEcranDef:         .skip 4      @ pointer screen default
ptFenetre:          .skip 4      @ pointer window
ptGC:               .skip 4      @ pointer graphic context
ptGC1:              .skip 4      @ pointer graphic context1
key:                .skip 4      @ key code
wmDeleteMessage:    .skip 8      @ ident close message
event:              .skip 400    @ TODO event size ??
PrpNomFenetre:      .skip 100    @ window name proprety
buffer:             .skip 500 
iWhite:             .skip 4      @ rgb code for white pixel
iBlack:             .skip 4      @ rgb code for black pixel
/**********************************************/
/* -- Code section                            */
/**********************************************/
.text
.global main

main:                               @ entry of program 
    ldr r0,iAdrszMessDebutPgm   @
    bl affichageMess            @ display start message on console linux
    /* attention r6  pointer display*/
    /* attention r8  pointer graphic context   */
    /* attention r9 ident window  */
    /*****************************/
    /*    OPEN SERVER X11        */
    /*****************************/
    mov r0,#0
    bl XOpenDisplay             @ open X server
    cmp r0,#0                   @ error ?
    beq erreurServeur
    ldr r1,iAdrptDisplay
    str r0,[r1]                 @ store display address 
    mov r6,r0                   @ and in register r6
    ldr r2,[r0,#+132]           @ load default_screen
    ldr r1,iAdrptEcranDef
    str r2,[r1]                 @ store default_screen
    mov r2,r0
    ldr r0,[r2,#+140]           @ load pointer screen list
    ldr r5,[r0,#+52]            @ load value white pixel
    ldr r4,iAdrWhite            @ and store in memory
    str r5,[r4]
    ldr r3,[r0,#+56]            @ load value black pixel
    ldr r4,iAdrBlack            @ and store in memory
    str r3,[r4]
    ldr r4,[r0,#+28]            @ load bits par pixel
    ldr r1,[r0,#+8]             @ load root windows
    /**************************/
    /* CREATE WINDOW          */
    /**************************/
    mov r0,r6                   @ address display
    mov r2,#0                   @ window position X
    mov r3,#0                   @ window position Y
    mov r8,#0                   @ for stack alignement
    push {r8}
    push {r3}                   @  background  = black pixel
    push {r5}                   @  border = white pixel
    mov r8,#2                   @  border size
    push {r8}
    mov r8,#240                 @ hauteur
    push {r8}
    mov r8,#320                 @ largeur 
    push {r8}   
    bl XCreateSimpleWindow
    add sp,#24                  @ stack alignement  6 push (4 bytes * 6)
    cmp r0,#0                   @ error ?
    beq erreurF

    ldr r1,iAdrptFenetre
    str r0,[r1]                 @ store window address in memory
    mov r9,r0                   @ and in register r9
    /*****************************/
    /* add window property       */
    /*****************************/
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    ldr r2,iAdrszWindowName     @ window name
    ldr r3,iAdrszTitreFenRed    @ window name reduced
    mov r4,#0
    push {r4}                   @ parameters not use
    push {r4}
    push {r4}
    push {r4}
    bl XSetStandardProperties
    add sp,sp,#16               @ stack alignement for 4 push
    /**************************************/
    /* for correction window close error  */
    /**************************************/
    mov r0,r6                   @ display address
    ldr r1,iAdrszLibDW          @ atom address
    mov r2,#1                   @ False  créate atom if not exists
    bl XInternAtom
    cmp r0,#0                   @ error X11 ?
    ble erreurX11
    ldr r1,iAdrwmDeleteMessage  @ recept address
    str r0,[r1]
    mov r2,r1                   @ return address
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    mov r3,#1                   @ number of protocols
    bl XSetWMProtocols
    cmp r0,#0                   @ error X11 ?
    ble erreurX11
    /**********************************/
    /*  create graphic context        */
    /**********************************/
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    mov r2,#0                   @ not use for simply context
    mov r3,#0
    bl XCreateGC
    cmp r0,#0                   @ error ?
    beq erreurGC
    ldr r1,iAdrptGC
    str r0,[r1]                 @ store address graphic context
    mov r8,r0                   @ and in r8
    @ create other GC
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    mov r2,#0                   @ not use for simply context
    mov r3,#0
    bl XCreateGC
    cmp r0,#0                   @ error ?
    beq erreurGC
    ldr r1,iAdrptGC1
    str r0,[r1]                 @ store address graphic context 1
    mov r1,r0
    mov r0,r6
    mov r2,#0xFF0000            @ color red
    bl XSetForeground
    cmp r0,#0
    beq erreurGC
    /****************************/
    /* modif window background  */
    /****************************/
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    ldr r2,iGris1               @ background color
    bl XSetWindowBackground   
    cmp r0,#0                   @ error ?
    ble erreurX11
    /***************************/
    /* OUF!! window display    */
    /***************************/
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    bl XMapWindow
    /****************************/
    /* Write text1 in the window */
    /****************************/
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    mov r2,r8                   @ address graphic context
    mov r3,#105                 @ position x 
    sub sp,#4                   @ stack alignement
    mov r4,#LGTEXTE1  - 1       @ size string 
    push {r4}                   @ on the stack
    ldr r4,iAdrszTexte1         @ string address
    push {r4}
    mov r4,#105                 @ position y 
    push {r4}
    bl XDrawString
    add sp,sp,#16               @ stack alignement 3 push and 1 stack alignement
    cmp r0,#0                   @ error ?
    blt erreurX11
    /****************************/
    /* Write text2 in the window */
    /****************************/
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    mov r2,r8                   @ address graphic context
    mov r3,#10                  @ position x 
    sub sp,#4                   @ stack alignement
    mov r4,#LGTEXTE2  - 1       @ size string 
    push {r4}                   @ on the stack
    ldr r4,iAdrszTexte2         @ string address
    push {r4}
    mov r4,#200                 @ position y 
    push {r4}
    bl XDrawString
    add sp,sp,#16               @ stack alignement 3 push and 1 stack alignement
    cmp r0,#0                   @ error ?
    blt erreurX11
    /****************************************/
    /* draw pixel                           */
    /****************************************/
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    ldr r2,iAdrptGC1
    ldr r2,[r2]                 @  address graphic context 1
    mov r3,#100                 @ position x
    sub sp,sp,#4                @ stack alignement
    mov r4,#100                 @ position y
    push {r4}                   @ on the stack
    bl XDrawPoint
    add sp,sp,#8                @ stack alignement 1 push and 1 stack alignement

    cmp r0,#0                   @ error ?
    blt erreurX11
    /****************************/
    /* Autorisations            */
    /****************************/
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    ldr r2,iFenetreMask         @ autorisation mask
    bl XSelectInput
    cmp r0,#0                   @ error ?
    ble erreurX11
    /****************************/
    /* Events loop              */
    /****************************/
1:
    mov r0,r6                   @ display address
    ldr r1,iAdrevent            @ events address
    bl XNextEvent               @ event ?
    ldr r0,iAdrevent
    ldr r0,[r0]                 @ code event
    cmp r0,#KeyPressed          @ key ?
    bne 2f
    ldr r0,iAdrevent            @ yes read key in buffer
    ldr r1,iAdrbuffer
    mov r2,#255
    ldr r3,iAdrkey
    mov r4,#0
    push {r4}                   @ stack alignement
    push {r4}
    bl XLookupString 
    add sp,#8                   @ stack alignement 2 push
    cmp r0,#1                   @ is character key ?
    bne 2f
    ldr r0,iAdrbuffer           @ yes -> load first buffer character
    ldrb r0,[r0]
    cmp r0,#0x71                @ character q for quit
    beq 5f                      @ yes -> end
    b 4f
2:
    /*                                  */
    /* for example  clic mouse button   */
    /************************************/
    cmp r0,#ButtonPress         @ clic mouse buton
    bne 3f
    ldr r0,iAdrevent
    ldr r1,[r0,#+32]            @ position X mouse clic
    ldr r2,[r0,#+36]            @ position Y
    @ etc for eventuel use
    b 4f
3:
    cmp r0,#ClientMessage       @ code for close window within error
    bne 4f
    ldr r0,iAdrevent
    ldr r1,[r0,#+28]            @ code message address 
    ldr r2,iAdrwmDeleteMessage  @ equal code window créate ???
    ldr r2,[r2]
    cmp r1,r2
    beq 5f                      @ yes -> end window 

4:  @ loop for other event
    b 1b
    /***********************************/
    /* Close window -> free ressources */
    /***********************************/
5:
    mov r0,r6                  @ display address
    ldr r1,iAdrptGC
    ldr r1,[r1]                @ load context graphic address 
    bl XFreeGC
    cmp r0,#0
    blt erreurX11
    mov r0,r6                  @ display address 
    mov r1,r9                  @ window address
    bl XDestroyWindow
    cmp r0,#0
    blt erreurX11
    mov r0,r6                  @ display address
    bl XCloseDisplay
    cmp r0,#0
    blt erreurX11
    mov r0,#0                  @ return code OK
    b 100f
erreurF:   @ create error window but possible not necessary. Display error by server
    ldr r1,iAdrszMessErrfen
    bl   displayError
    mov r0,#1                  @ return error code
    b 100f
erreurGC:                      @ error create graphic context
    ldr r1,iAdrszMessErrGc
    bl   displayError
    mov r0,#1
    b 100f
erreurX11:    @ erreur X11
    ldr r1,iAdrszMessErreurX11
    bl   displayError
    mov r0,#1
    b 100f
erreurServeur:                 @ error no found X11 server see doc putty and Xming
    ldr r1,iAdrszMessErreur
    bl   displayError
    mov r0,#1
    b 100f

100:                           @ standard end of the program 
    mov r7, #EXIT
    svc 0 
iFenetreMask:        .int  KeyPressMask|ButtonPressMask|StructureNotifyMask
iGris1:              .int 0xFFA0A0A0
iAdrWhite:           .int iWhite
iAdrBlack:           .int iBlack
iAdrptDisplay:       .int ptDisplay
iAdrptEcranDef:      .int ptEcranDef
iAdrptFenetre:       .int ptFenetre
iAdrptGC:            .int ptGC
iAdrptGC1:           .int ptGC1
iAdrevent:           .int event
iAdrbuffer:          .int buffer
iAdrkey:             .int key
iAdrszLibDW:         .int szLibDW
iAdrszMessDebutPgm:  .int szMessDebutPgm
iAdrszMessErreurX11: .int szMessErreurX11
iAdrszMessErrGc:     .int szMessErrGc
iAdrszMessErreur:    .int szMessErreur
iAdrszMessErrfen:    .int szMessErrfen
iAdrszWindowName:    .int szWindowName
iAdrszTitreFenRed:   .int szTitreFenRed
iAdrszTexte1:            .int szTexte1
iAdrszTexte2:        .int szTexte2
iAdrPrpNomFenetre:   .int PrpNomFenetre
iAdrwmDeleteMessage: .int wmDeleteMessage

/******************************************************************/
/*     display text with size calculation                         */ 
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                   @ save  registres
    mov r2,#0                               @ counter length 
1:                                          @ loop length calculation 
    ldrb r1,[r0,r2]                         @ read octet start position + index 
    cmp r1,#0                               @ if 0 its over 
    addne r2,r2,#1                          @ else add 1 in the length 
    bne 1b                                  @ and loop 
                                            @ so here r2 contains the length of the message 
    mov r1,r0                               @ address message in r1 
    mov r0,#STDOUT                          @ code to write to the standard output Linux 
    mov r7, #WRITE                          @ code call system "write" 
    svc #0                                  @ call systeme 
    pop {r0,r1,r2,r7,lr}                    @ restaur registers
    bx lr                                   @ return
/***************************************************/
/*   display error message                         */
/***************************************************/
/* r0 contains error code  r1 : message address */
displayError:
    push {r0-r2,lr}                         @ save registers
    mov r2,r0                               @ save error code
    mov r0,r1
    bl affichageMess
    mov r0,r2                               @ error code
    ldr r1,iAdrsHexa
    bl conversion16                         @ conversion hexa
    mov r0,r2                               @ error code
    ldr r1,iAdrsDeci                        @ result address
    bl conversion10                         @ conversion decimale
    ldr r0,iAdrszMessErr                    @ display error message
    bl affichageMess
100:
    pop {r0-r2,lr}                          @ restaur registers
    bx lr                                   @ return 
iAdrszMessErr:                 .int szMessErr
iAdrsHexa:                     .int sHexa
iAdrsDeci:                     .int sDeci
/******************************************************************/
/*     Converting a register to hexadecimal                      */ 
/******************************************************************/
/* r0 contains value and r1 address area   */
conversion16:
    push {r1-r4,lr}                                    @ save registers
    mov r2,#28                                         @ start bit position
    mov r4,#0xF0000000                                 @ mask
    mov r3,r0                                          @ save entry value
1:                                                     @ start loop
    and r0,r3,r4                                       @value register and mask
    lsr r0,r2                                          @ move right 
    cmp r0,#10                                         @ compare value
    addlt r0,#48                                       @ <10  ->digit	
    addge r0,#55                                       @ >10  ->letter A-F
    strb r0,[r1],#1                                    @ store digit on area and + 1 in area address
    lsr r4,#4                                          @ shift mask 4 positions
    subs r2,#4                                         @  counter bits - 4 <= zero  ?
    bge 1b                                             @  no -> loop

100:
    pop {r1-r4,lr}                                     @ restaur registers 
    bx lr                                              @return
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
/* r0 quotient    */
/* r1 remainder   */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0) 
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5 
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function 
iMagicNumber:  	.int 0xCCCCCCCD
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
1:                                                    @ loop 
    movs r0, r0, LSL #1                               @ r0 <- r0 << 1 updating cpsr (sets C if 31st bit of r0 was 1)
    adc r3, r3, r3                                     @ r3 <- r3 + r3 + C. This is equivalent to r3 ? (r3 << 1) + C 
    cmp r3, r1                                         @ compute r3 - r1 and update cpsr 
    subhs r3, r3, r1                                  @ if r3 >= r1 (C=1) then r3 <- r3 - r1 
    adc r2, r2, r2                                     @ r2 <- r2 + r2 + C. This is equivalent to r2 <- (r2 << 1) + C 
2:
    subs r4, r4, #1                                   @ r4 <- r4 - 1 
    bpl 1b                                            @ if r4 >= 0 (N=0) then loop
    pop {r4, lr}
    bx lr

```



## BASIC256

It seems that the program should be this.  And the BASIC256 tutorial programs work on my ubuntu system.  This program neither draws the pixel nor resizes the window.  Can't see anything when I plot many spots.  Oh well.  I've tried the rgb(255,0,0) function as well as the fastgraphics/refresh statements.

```BASIC256

rem http://rosettacode.org/wiki/Draw_a_pixel

graphsize 320, 240
color red
plot 100, 100

```



## BBC BASIC

```bbcbasic
      VDU 23, 22, 320; 240; 8, 8, 8, 0, 18, 0, 1, 25, 69, 100; 100;
```



## C

Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.

```C

#include<graphics.h>

int main()
{
	initwindow(320,240,"Red Pixel");
	
	putpixel(100,100,RED);
	
	getch();
	
	return 0;
}

```



## Commodore BASIC


'''Example 1:''' Commodore 64

There are no graphics commands in Commodore 64 BASIC. High resolution (hires) graphics are programmed by directly manipulating the hardware registers and memory. 

The Commodore 64 hires bitmap is 320&times;200, subdivided into 8&times;8 cells starting at the top left and moving right. Each cell is addressed top to bottom by 8 bytes. Each byte controls a horizontal row of 8 bits. This requires calculation on the programmer's part to translate X,Y coordinates into a specific memory address/value combination (lines 30 through 60).


```gwbasic
10 REM PLOT A RED PIXEL AT 100,100
20 REM INITIALIZE BITMAP MODE
21 POKE 53280,0:PRINT CHR$(147);"CLEARING BITMAP... PLEASE WAIT..."
22 BASE=8192:FOR I=BASE TO BASE+7999:POKE I,0:NEXT
23 PRINT CHR$(147);
24 POKE 53272,PEEK(53272) OR 8:REM SET BITMAP MEMORY AT 8192 ($2000)
25 POKE 53265,PEEK(53265) OR 32:REM ENTER BITMAP MODE
26 REM PLOT PIXEL
30 X=100:Y=100
40 MEM=BASE+INT(Y/8)*320+INT(X/8)*8+(Y AND 7)
50 PX=7-(X AND 7)
60 POKE MEM,PEEK(MEM) OR 2^PX
65 REM WAIT FOR KEYPRESS
70 GET K$:IF K$="" THEN 70
75 REM CLEAR PIXEL, RETURN TO TEXT MODE
80 POKE MEM,0:POKE 53265,PEEK(53265) AND 223:POKE 53272,PEEK(53272) AND 247
90 POKE 53280,14:POKE 53281,6:POKE 646,14:END
```


'''Example 2:''' Commodore Plus 4 and 128

```gwbasic
10 COLOR 0,1:COLOR 1,3: REM SET BORDER TO BLACK AND PIXEL COLOR TO RED
15 GRAPHIC 1,1 : REM ENTER BITMAP GRAPHICS MODE AND CLEAR SCREEN
20 DRAW 1,100,100 : REM PLOT PIXEL AT 100,100
30 GET K$:IF K$="" THEN 30
40 GRAPHIC 0,1 : REM RETURN TO TEXT MODE
```



## Factor


```factor
USING: accessors arrays images images.testing images.viewer
kernel literals math sequences ;
IN: rosetta-code.draw-pixel

: draw-pixel ( -- )
    B{ 255 0 0 } 100 100 <rgb-image> 320 240 [ 2array >>dim ]
    [ * ] 2bi [ { 0 0 0 } ] replicate B{ } concat-as >>bitmap
    [ set-pixel-at ] keep image-window ;

MAIN: draw-pixel
```


=={{Header|FreeBASIC}}==

```freebasic
' version 27-06-2018
' compile with: fbc -s console
'           or: fbc -s gui

Screen 13                  ' Screen 18: 320x200,  8bit colordepth
'ScreenRes 320, 200, 24    ' Screenres: 320x200, 24bit colordepth

If ScreenPtr = 0 Then
    Print "Error setting video mode!"
    End
End If

Dim As UInteger depth, x = 100, y = 100

' what is color depth
ScreenInfo ,,depth

If depth = 8 Then
    PSet(x, y), 40   ' palette, index 40 = RGB(255, 0, 0)
Else
    PSet(x, y), RGB(255, 0, 0) ' red
End If

' empty keyboard buffer
While Inkey <> "" : Wend
WindowTitle IIf(depth = 8, "Palette","True Color") + ", hit any key to end program"
Sleep
End
```


=={{Header|GB BASIC}}==
The resolution of the Game Boy's screen is only 160×144, so the window requirement is out, but the pixel is easy. Also, GB BASIC doesn't support displaying the color red, so the Game Boy's grayscale equivalent, dark gray, is used instead: 

```GB BASIC
10 color 1
20 point 100,100
```



## Go


```go
package main

import (
    "fmt"
    "image"
    "image/color"
    "image/draw"
)

func main() {
    rect := image.Rect(0, 0, 320, 240)
    img := image.NewRGBA(rect)

    // Use green background, say.
    green := color.RGBA{0, 255, 0, 255}
    draw.Draw(img, rect, &image.Uniform{green}, image.ZP, draw.Src)

    // Set color of pixel at (100, 100) to red
    red := color.RGBA{255, 0, 0, 255}
    img.Set(100, 100, red)

    // Check it worked.
    cmap := map[color.Color]string{green: "green", red: "red"}
    c1 := img.At(0, 0)
    c2 := img.At(100, 100)
    fmt.Println("The color of the pixel at (  0,   0) is", cmap[c1], "\b.")
    fmt.Println("The color of the pixel at (100, 100) is", cmap[c2], "\b.")
}
```


```txt

The color of the pixel at (  0,   0) is green.
The color of the pixel at (100, 100) is red.

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 SET VIDEO X 40:SET VIDEO Y 26:SET VIDEO MODE 5:SET VIDEO COLOR 0
110 OPEN #101:"video:"
120 DISPLAY #101:AT 1 FROM 1 TO 26
130 SET PALETTE BLACK,RED
140 PLOT 100,100
```


## Java

Basic Implementation via subclass of JFrame:

```Java
import java.awt.Color;
import java.awt.Graphics;
import javax.swing.JFrame;

public class DrawAPixel extends JFrame{
	public DrawAPixel() {
		super("Red Pixel");
		setSize(320, 240);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
	}
	@Override
	public void paint(Graphics g) {
		g.setColor(new Color(255, 0, 0));
		g.drawRect(100, 100, 1, 1);
	}
	public static void main(String[] args) {
		new DrawAPixel();
	}
}

```

Advanced Implementation via subclass of JPanel (more powerful especially while repainting):

```Java
import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import javax.swing.JFrame;
import javax.swing.JPanel;

public class DrawAPixel extends JPanel{	
	private BufferedImage puffer;
	private JFrame window;
	private Graphics g;
	public DrawAPixel() {
		window = new JFrame("Red Pixel");
		window.setSize(320, 240);
		window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		window.setLayout(null);
		setBounds(0, 0, 320, 240);
		window.add(this);
		window.setVisible(true);
	}
	@Override
	public void paint(Graphics gr) {
		if(g == null) {
			puffer = (BufferedImage) createImage(getWidth(), getHeight());
			g = puffer.getGraphics();
		}
		g.setColor(new Color(255, 0, 0));
		g.drawRect(100, 100, 1, 1);
		gr.drawImage(puffer, 0, 0, this);
	}
	public static void main(String[] args) {
		new DrawAPixel();
	}
}

```



## Julia


```julia
using Gtk, Graphics
 
const can = @GtkCanvas()
const win = GtkWindow(can, "Draw a Pixel", 320, 240)

draw(can) do widget
    ctx = getgc(can)
    set_source_rgb(ctx, 255, 0, 0)
    move_to(ctx, 100, 100)
    line_to(ctx, 101,100)
    stroke(ctx)
end
 
show(can)
const cond = Condition()
endit(w) = notify(cond)
signal_connect(endit, win, :destroy)
wait(cond)

```




## Kotlin

This task seems very similar to the [[Bitmap]] task and so therefore is the code to accomplish it.

```scala
// Version 1.2.41

import java.awt.Color
import java.awt.Graphics
import java.awt.image.BufferedImage

class BasicBitmapStorage(width: Int, height: Int) {
    val image = BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    fun fill(c: Color) {
        val g = image.graphics
        g.color = c
        g.fillRect(0, 0, image.width, image.height)
    }

    fun setPixel(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB())

    fun getPixel(x: Int, y: Int) = Color(image.getRGB(x, y))
}

fun main(args: Array<String>) {
    val bbs = BasicBitmapStorage(320, 240)
    with (bbs) {
        fill(Color.white) // say
        setPixel(100, 100, Color.red)
        // check it worked
        val c = getPixel(100, 100)
        print("The color of the pixel at (100, 100) is ")
        println(if (c == Color.red) "red" else "white")
    }
}
```


```txt

The color of the pixel at (100, 100) is red

```



## M2000 Interpreter

Version 2

M2000 Console used for text and graphics too. So for this version we plot a pixel in console and in a form (a window above console). Form is open as modal, but console is something different, so we can show or hide it.

All forms are above console (for any kind, modal or not modal form), and we can hide console using Title statement. So in this example we hide/show  console (behind form window) by clicking the form. In any state of showing console, when we close form and Interpreter need to input from console (like in prompt state), console state get visible.

There is also a PSet statement for pixel drawing (from 9.5 version). Just replace the module call to PlotPixel with Pset.


```M2000 Interpreter



Module CheckIt {
      Module PlotPixel (a as single, b as single) {
            Move a*TwipsX, b*TwipsX
            Draw TwipsX, TwipsY
      }
      Cls 5,0  \\ clear console with Magenta (5) and set split  screen from 0 (no split screen)
      Pen #55FF77 {
            PlotPixel 1000, 200
      }
      Wait 1000
      Title "", 1
      Declare DrawPixelForm Form
      With DrawPixelForm, "Title", "Draw a Pixel at 100,100"
      Layer DrawPixelForm {
            \\ 12 for 12pt fonts
            \\ use ; to center window
            Window 12, 320*twipsx, 240*twipsy;
            Cls #333333
            Pen Color(255,0,0) {
                  PlotPixel 100, 100
            }
      }
      \\ code to show/hide console clicking form
      \\ console shown behind form
      k=0
      Function DrawPixelForm.Click {
            Title "Rosetta Code Example", abs(k)
            if k then show
            k~
      }
      Method DrawPixelForm, "Show", 1
      Declare DrawPixelForm Nothing
}
CheckIt

```

[https://www.dropbox.com/s/uscltz6pwy06jxv/DrawPixelForm.png?dl=0 OutPut]


## Nim


```nim
import rapid/gfx

var
  window = initRWindow()
    .size(320, 240)
    .title("Rosetta Code - draw a pixel")
    .open()
  surface = window.openGfx()

surface.loop:
  draw ctx, step:
    ctx.clear(gray(0))
    ctx.begin()
    ctx.point((100.0, 100.0, rgb(255, 0, 0)))
    ctx.draw(prPoints)
    discard step # Prevent unused variable warnings
  update step:
    discard step
```



## Lua


The luasdl2 library is required.


```lua
local SDL = require "SDL"

local ret = SDL.init { SDL.flags.Video }
local window = SDL.createWindow {
	title	= "Pixel",
	height	= 320,
	width	= 240
}

local renderer = SDL.createRenderer(window, 0, 0)

renderer:clear()
renderer:setDrawColor(0xFF0000)
renderer:drawPoint({x = 100,y = 100})
renderer:present()

SDL.delay(5000)

```



## Oberon


With basic module XYplane, the size of the drawing plane cannot be set. Tested with [https://miasap.se/obnc OBNC].


```Oberon
MODULE pixel;

	IMPORT XYplane;
	
BEGIN
	XYplane.Open;
	XYplane.Dot(100, 100, XYplane.draw);
	REPEAT UNTIL XYplane.Key() = "q"
END pixel.

```



## Objeck

```objeck
use Game.SDL2;
use Game.Framework;

class DrawPixel {
  @framework : GameFramework;
          
  function : Main(args : String[]) ~ Nil {
    DrawPixel->New()->Run();
  }

  New() {
    @framework := GameFramework->New(320, 240, "RGB");
    @framework->SetClearColor(Color->New(0, 0, 0));
  }

  method : Run() ~ Nil {
    if(@framework->IsOk()) {
      e := @framework->GetEvent();
      
      quit := false;
      while(<>quit) {
        @framework->FrameStart();
        @framework->Clear();

        while(e->Poll() <> 0) {
          if(e->GetType() = EventType->SDL_QUIT) {
            quit := true;
          };
        };
        
        @framework->GetRenderer()->SetDrawColor(255, 0, 0, 255);
        @framework->GetRenderer()->DrawPoint(100, 100);  

        @framework->Show();
        @framework->FrameEnd();
      };
    }
    else {
      "--- Error Initializing Game Environment ---"->ErrorLine();
      return;
    };

    leaving {
      @framework->Quit();
    };
  }
}

```




## OCaml


Using the Graphics library provided with the standard OCaml distribution:


```ocaml
module G = Graphics

let () =
  G.open_graph "";
  G.resize_window 320 240;
  G.set_color G.red;
  G.plot 100 100;
  ignore (G.read_key ())
```

run with:

```txt
$ ocaml graphics.cma draw_a_pixel.ml

```


```ocaml
open Sdl

let () =
  let width, height = (320, 240) in
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer ~width ~height ~flags:[]
  in
  let rgb = (255, 0, 0) and a = 255 in
  Render.set_draw_color renderer ~rgb ~a;
  Render.draw_point renderer (100, 100);
  Render.render_present renderer;
  Timer.delay 3000;
  Sdl.quit ()
```

run with:

```txt
$ ocaml -I $(ocamlfind query sdl2) sdl2.cma draw_a_pixel.ml

```




## Perl

```perl
use Gtk3 '-init';

my $window = Gtk3::Window->new();
$window->set_default_size(320, 240);
$window->set_border_width(10);
$window->set_title("Draw a Pixel");
$window->set_app_paintable(TRUE);

my $da = Gtk3::DrawingArea->new();
$da->signal_connect('draw' => \&draw_in_drawingarea);
$window->add($da);
$window->show_all();

Gtk3->main;

sub draw_in_drawingarea
{
  my ($widget, $cr, $data) = @_;
  $cr->set_source_rgb(1, 0, 0);
  $cr->set_line_width(1);
  $cr->rectangle( 100, 100, 1, 1);
  $cr->stroke;
}
```



## Perl 6

Really? Draw a single pixel? Sigh.


```perl6
use GTK::Simple;
use GTK::Simple::DrawingArea;
use Cairo;

my $app = GTK::Simple::App.new(:title('Draw a Pixel'));
my $da  = GTK::Simple::DrawingArea.new;
gtk_simple_use_cairo;

$app.set-content( $da );
$app.border-width = 5;
$da.size-request(320,240);

sub rect-do( $d, $ctx ) {
    given $ctx {
        .rgb(1, 0, 0);
        .rectangle(100, 100, 1, 1);
        .fill;
    }
}

my $ctx = $da.add-draw-handler( &rect-do );
$app.run;
```



## Phix


```Phix
include pGUI.e

cdCanvas cdcanvas

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    cdCanvasActivate(cdcanvas)
    cdCanvasPixel(cdcanvas, 100, 100, CD_RED) 
    cdCanvasFlush(cdcanvas)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    return IUP_DEFAULT
end function

procedure main()
    IupOpen()
    Ihandle canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "320x240")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    Ihandle dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Draw a pixel")
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))
    IupCloseOnEscape(dlg) 
    IupShow(dlg)
    IupMainLoop()
    IupClose()
end procedure

main()
```




## Processing


```Processing

void setup()
{
  size(320, 240);
  background(255);
  stroke(255, 0, 0);
  point(100, 100);
}

```




## PureBasic



```PureBasic

  If OpenWindow(0, 0, 0, 320, 240, "Rosetta Code Draw A Pixel in PureBasic")
    If CreateImage(0, 320, 240) And StartDrawing(ImageOutput(0))
          Plot(100, 100,RGB(255,0,0))
      StopDrawing() 
      ImageGadget(0, 0, 0, 320, 240, ImageID(0))
    EndIf
    Repeat
      Event = WaitWindowEvent()
    Until Event = #PB_Event_CloseWindow
  EndIf

```



## Python

```Python
from PIL import Image

img = Image.new('RGB', (320, 240))
pixels = img.load()
pixels[100,100] = (255,0,0)
img.show()

```



## QBASIC


```qbasic
' http://rosettacode.org/wiki/Draw_a_pixel
' This program can run in QBASIC, QuickBASIC, gw-BASIC (adding line numbers) and VB-DOS
SCREEN 1
COLOR , 0
PSET (100, 100), 2
END

```



## Racket


```racket
#lang racket
(require racket/draw)
(let ((b (make-object bitmap% 320 240)))
  (send b set-argb-pixels 100 100 1 1 (bytes 255 0 0 255))
  b)
```



## Ring


```ring
# Project  : Draw a pixel

load "guilib.ring"

new qapp {
       win1 = new qwidget() {
                  setwindowtitle("Drawing Pixels")
                  setgeometry(100,100,320,240)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,300,200)
                              settext("")
                  }
                  new qpushbutton(win1) {
                         setgeometry(200,200,100,30)
                         settext("draw")
                         setclickevent("draw()")
                  }
                  show()
       }
       exec()
}

func draw()
        p1 = new qpicture()
        color = new qcolor() {
                   setrgb(255,0,0,255)
                  }
        pen = new qpen() {
                  setcolor(color)
                  setwidth(5)
                  }
        new qpainter() {
               begin(p1)
               setpen(pen)
               drawpoint(100,100)
               endpaint()
               }
               label1 { setpicture(p1) show() }
```

Output image:
[https://www.dropbox.com/s/gbtrkx7bx7hogxm/PixelColor.jpg?dl=0 Draw a pixel]


## Robotic

We can technically draw a pixel through the use of Sprites.

One of the requirements (fixed screen resolution) could not be met.

```robotic

. "Set the sprite's reference character located at the"
. "upper-left corner of the board (char 0)"
set "SPR0_REFX" to 0
set "SPR0_REFY" to 0

. "Offset that reference by 256, leading to the first character"
. "in the extended character set"
set "SPR0_OFFSET" to 256

. "Set the width and height of the sprite"
set "SPR0_WIDTH" to 1
set "SPR0_HEIGHT" to 1

. "Unbound the sprite, removing the grid restriction"
set "SPR0_UNBOUND" to 1

. "Mark the sprite for display on the overlay (this may not be necessary)"
set "SPR0_OVERLAY" to 1

set "xPos" to 100
set "yPos" to 100

. "Display the sprite at the given location"
put c0c Sprite p00 at "('xPos')" "('yPos')"

```


I highly recommend you check out [https://www.digitalmzx.net/wiki/index.php?title=Sprite_(Tutorial) this tutorial] for the usage of Sprites in order to get a better understanding of all this.


## Rust

{{out|Output Image}} [https://cdn1.imggmi.com/uploads/2018/10/3/71d1e0b8722bca5d239eb733320f6df6-full.png RustOut]



```rust
extern crate piston_window;
extern crate image;

use piston_window::*;

fn main() {
    let (width, height) = (320, 240);
    
    let mut window: PistonWindow =
        WindowSettings::new("Red Pixel", [width, height])
        .exit_on_esc(true).build().unwrap();

    // Since we cant manipulate pixels directly, we need to manipulate the pixels on a canvas.
    // Only issue is that sub-pixels exist (which is probably why the red pixel looks like a smear on the output image)
    let mut canvas = image::ImageBuffer::new(width, height);
    canvas.put_pixel(100, 100, image::Rgba([0xff, 0, 0, 0xff]));

    // Transform into a texture so piston can use it.
    let texture: G2dTexture = Texture::from_image(
        &mut window.factory,
        &canvas,
        &TextureSettings::new()
    ).unwrap();

    // The window event loop.
    while let Some(event) = window.next() {
        window.draw_2d(&event, |context, graphics| {
            clear([1.0; 4], graphics);
            image(&texture,
            context.transform,
            graphics);
        });
    }
}

```



## Scala


### Scala idiom

A more Scalesque version could be with the use of its idiom:
Best experienced in your browser [https://scastie.scala-lang.org/AHtZh6zhRTWGj3M8azw28g with Scastie (remote JVM)].

```scala
import java.awt.image.BufferedImage
import java.awt.Color
import scala.language.reflectiveCalls

object RgbBitmap extends App {

  class RgbBitmap(val dim: (Int, Int)) {
    def width = dim._1
    def height = dim._2

    private val image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    def apply(x: Int, y: Int) = new Color(image.getRGB(x, y))

    def update(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB)

    def fill(c: Color) = {
      val g = image.getGraphics
      g.setColor(c)
      g.fillRect(0, 0, width, height)
    }
  }

  object RgbBitmap {
    def apply(width: Int, height: Int) = new RgbBitmap(width, height)
  }

  /** Even Javanese style testing is still possible.
    */
  private val img0 = new RgbBitmap(50, 60) { // Wrappers to enable adhoc Javanese style
    def getPixel(x: Int, y: Int) = this(x, y)
    def setPixel(x: Int, y: Int, c: Color) = this(x, y) = c
  }

  img0.fill(Color.CYAN)
  img0.setPixel(5, 6, Color.BLUE)
  // Testing in Java style
  assert(img0.getPixel(0, 1) == Color.CYAN)
  assert(img0.getPixel(5, 6) == Color.BLUE)
  assert(img0.width == 50)
  assert(img0.height == 60)
  println("Tests successfully completed with no errors found.")

}
```



## SmileBASIC

I almost said that the 320x240 requirement was not going to happen. Then I realised the 3DS bottom screen is exactly 320x240.

```smilebasic
XSCREEN 3
DISPLAY 1
GPSET 100, 100, RGB(255, 0, 0)
WAIT 60
```



## VBA

Word 

```vb
Sub draw()
    Dim sh As Shape, sl As Shape
    Set sh = ActiveDocument.Shapes.AddCanvas(100, 100, 320, 240)
    Set sl = sh.CanvasItems.AddLine(100, 100, 101, 100)
    sl.Line.ForeColor.RGB = RGB(Red:=255, Green:=0, Blue:=0)
End Sub
```



## Wee Basic

Since the resolution of the Nintendo DS's bottom screen is only 256×192 (the same applies to the top screen), the window requirement is out, but the pixel is easy:

```Wee Basic
keyhide
plot 0 100,100,5
end
```



## ZX Spectrum Basic

The ZX Spectrum screen is only 256x224 (unless you're running one of the games like Starion or Dark Star which used scary machine code timing tricks to draw on the border), meaning the window requirement is out, but the pixel is easy:

```zxbasic
PLOT INK 2;100,100
```

