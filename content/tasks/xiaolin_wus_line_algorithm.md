+++
title = "Xiaolin Wu's line algorithm"
description = ""
date = 2019-09-20T08:20:47Z
aliases = []
[extra]
id = 3998
[taxonomies]
categories = ["task", "Raster graphics operations"]
tags = []
+++

## Task

{{task|Raster graphics operations}}[[Category:Graphics algorithms]]
Implement the [[wp:Xiaolin Wu's line algorithm|Xiaolin Wu's line algorithm]] as described in Wikipedia. This algorithm draw antialiased lines. See [[Bresenham's line algorithm]] for ''aliased'' lines.


## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program xiaolin1.s   */

/* REMARK 1 : this program use routines in a include file 
   see task Include a file language arm assembly 
   for the routine affichageMess displayerror 
   see at end oh this program the instruction include */

/* REMARK 2 : display use a FrameBuffer device : see raspberry pi FrameBuffer documentation
              this solution write directly on the screen of raspberry pi
              other solution is to use X11 windows but X11 has a function drawline !! */

/* REMARK 3 : this program do not respect the convention for use, save and restau registers
              in rhe routine call !!!!   */

/*******************************************/
/* Constantes    */
/*******************************************/
.equ STDOUT,              1     @ Linux output console
.equ EXIT,                1     @ Linux syscall
.equ WRITE,               4     @ Linux syscall
.equ OPEN,                5
.equ CLOSE,               6
.equ IOCTL,               0x36
.equ MMAP,                0xC0
.equ UNMAP,               0x5B
.equ O_RDWR,              0x0002    @ open for reading and writing
.equ MAP_SHARED,          0x01      @ Share changes.
.equ PROT_READ,           0x1       @ Page can be read.
.equ PROT_WRITE,          0x2       @ Page can be written.

/*******************************************/
/* Initialized data                        */
/*******************************************/
.data
szMessErreur:   .asciz "File open error.\n"
szMessErreur1:  .asciz "File close error.\n"
szMessErreur2:  .asciz "File mapping error.\n"
szMessDebutPgm: .asciz "Program start. \n"
szMessFinOK:    .asciz "Normal end program. \n"
szMessErrFix:   .asciz  "Read error info fix framebuffer  \n"
szMessErrVar:   .asciz  "Read error info var framebuffer  \n"
szRetourligne:  .asciz  "\n"
szParamNom:     .asciz "/dev/fb0"         @ FrameBuffer device name
szLigneVar:     .ascii "Variables info : "
sWidth:        .fill 11, 1, ' ' 
                .ascii " * "
sHeight:        .fill 11, 1, ' ' 
                .ascii " Bits par pixel : "
sBits:           .fill 11, 1, ' '
                .asciz  "\n"
/*************************************************/
szMessErr: .ascii	"Error code hexa : "
sHexa: .space 9,' '
         .ascii "  decimal :  "
sDeci: .space 15,' '
         .asciz "\n"
.align 4
/* codes fonction pour la récupération des données fixes et variables */
FBIOGET_FSCREENINFO: .int 0x4602  @ function code for read infos fixes Framebuffer
FBIOGET_VSCREENINFO: .int 0x4600  @ function code for read infos variables Framebuffer

/*******************************************/
/* UnInitialized data */
/*******************************************/ 
.bss
.align 4
fix_info: .skip FBFIXSCinfo_fin                @ memory reserve for structure FSCREENINFO
.align 4
var_info: .skip FBVARSCinfo_fin                @ memory reserve for structure VSCREENINFO
/**********************************************/
/* -- Code section                            */
/**********************************************/
.text
.global main

main:
    ldr r0,iAdrszMessDebutPgm
    bl affichageMess                  @ display message
    ldr r0,iAdrszParamNom             @ frameBuffer device name
    mov r1,#O_RDWR                    @ flags read/write
    mov r2,#0                         @ mode 
    mov r7,#OPEN                      @ open device FrameBuffer 
    svc 0 
    cmp r0,#0                         @ error ?
    ble erreur
    mov r10,r0                        @ save FD du device FrameBuffer in r10
    
    ldr r1,iAdrFBIOGET_VSCREENINFO    @ read variables datas of FrameBuffer
    ldr r1,[r1]                       @ load code function
    ldr r2,iAdrvar_info               @ structure memory address
    mov r7, #IOCTL                    @ call system
    swi 0 
    cmp r0,#0
    blt erreurVar
    ldr r2,iAdrvar_info
    ldr r0,[r2,#FBVARSCinfo_xres]     @ load screen width
    ldr r1,iAdrsWidth                 @ and convert in string for display
    bl conversion10S
    ldr r0,[r2,#FBVARSCinfo_yres]     @ load screen height 
    ldr r1,iAdrsHeight                @ and convert in string for display
    bl conversion10S
    ldr r0,[r2,#FBVARSCinfo_bits_per_pixel]  @ load bits by pixel  
    ldr r1,iAdrsBits                  @ and convert in string for display
    bl conversion10S
    ldr r0,iAdrszLigneVar             @ display result 
    bl affichageMess

    mov r0,r10                        @ FD du FB
    ldr r1,iAdrFBIOGET_FSCREENINFO    @ read fixes datas of FrameBuffe
    ldr r1,[r1]                       @ load code function
    ldr r2,iAdrfix_info               @ structure memory address
    mov r7, #IOCTL                    @ call system
    svc 0 
    cmp r0,#0                         @ error ?
    blt erreurFix
    ldr r0,iAdrfix_info

    ldr r1,iAdrfix_info               @ read size memory for datas
    ldr r1,[r1,#FBFIXSCinfo_smem_len] @ in octets
                                      @ datas mapping
    mov r0,#0
    ldr r2,iFlagsMmap
    mov r3,#MAP_SHARED
    mov r4,r10
    mov r5,#0
    mov r7, #MMAP                     @ 192 call system for mapping
    swi #0 
    cmp r0,#0                         @ error ?
    beq erreur2    
    mov r9,r0                         @ save mapping address in r9
    /*************************************/
    /* display draw                      */
    bl dessin
    /************************************/
    mov r0,r9                         @ mapping close
    ldr r1,iAdrfix_info
    ldr r1,[r1,#FBFIXSCinfo_smem_len] @ mapping memory size
    mov r7,#UNMAP                     @call system 91 for unmapping
    svc #0                            @ error ?
    cmp r0,#0
    blt erreur1    
                                      @ close device FrameBuffer
    mov r0,r10                        @ load FB du device
    mov r7, #CLOSE                    @ call system
    swi 0 
    ldr r0,iAdrszMessFinOK            @ display end message
    bl affichageMess
    mov r0,#0                         @ return code = OK
    b 100f
erreurFix:                            @ display read error datas fix
    ldr r1,iAdrszMessErrFix           @ message address
    bl   displayError                 @ call display
    mov r0,#1                         @ return code = error
    b 100f
erreurVar:                            @ display read error datas var
    ldr r1,iAdrszMessErrVar
    bl   displayError
    mov r0,#1
    b 100f
erreur:                               @ display open error 
    ldr r1,iAdrszMessErreur
    bl   displayError
    mov r0,#1
    b 100f
erreur1:                              @ display unmapped error
    ldr r1,iAdrszMessErreur1
    bl   displayError
    mov r0,#1
    b 100f
erreur2:                              @ display mapped error
    ldr r1,iAdrszMessErreur2
    bl   displayError
    mov r0,#1
    b 100f
100:                                  @ end program
    mov r7, #EXIT
    svc 0 
/************************************/
iAdrszParamNom:           .int szParamNom
iFlagsMmap:               .int PROT_READ|PROT_WRITE
iAdrszMessErreur:         .int szMessErreur
iAdrszMessErreur1:        .int szMessErreur1
iAdrszMessErreur2:        .int szMessErreur2
iAdrszMessDebutPgm:       .int szMessDebutPgm
iAdrszMessFinOK:          .int szMessFinOK
iAdrszMessErrFix:         .int szMessErrFix
iAdrszMessErrVar:         .int szMessErrVar
iAdrszLigneVar:           .int szLigneVar
iAdrvar_info:             .int var_info
iAdrfix_info:             .int fix_info
iAdrFBIOGET_FSCREENINFO:  .int FBIOGET_FSCREENINFO
iAdrFBIOGET_VSCREENINFO:  .int FBIOGET_VSCREENINFO
iAdrsWidth:               .int sWidth
iAdrsHeight:              .int sHeight
iAdrsBits:                .int sBits
/***************************************************/
/*   dessin                  */
/***************************************************/
/* r9 framebuffer memory address   */
dessin:
    push {r1-r12,lr}                  @ save registers
    mov r0,#255                       @ red
    mov r1,#255                       @ green
    mov r2,#255                       @ blue    3 bytes 255 = white
    bl codeRGB                        @ code color RGB  32 bits
    mov r1,r0                         @ background color
    ldr r0,iAdrfix_info               @ load memory mmap size 
    ldr r0,[r0,#FBFIXSCinfo_smem_len]    
    bl coloriageFond                  @
    /* draw line 1  */
    mov r0,#200                       @ X start line
    mov r1,#200                       @ Y start line
    mov r2,#200                       @ X end line
    mov r3,#100                       @ Y end line
    ldr r4,iAdrvar_info
    ldr r4,[r4,#FBVARSCinfo_xres]     @ load screen width
    bl drawLine
    /* draw line 2  */
    mov r0,#200
    mov r1,#200
    mov r2,#200
    mov r3,#300
    ldr r4,iAdrvar_info
    ldr r4,[r4,#FBVARSCinfo_xres]
    bl drawLine
    /* draw line 3  */
    mov r0,#200
    mov r1,#200
    mov r2,#100
    mov r3,#200
    ldr r4,iAdrvar_info
    ldr r4,[r4,#FBVARSCinfo_xres]
    bl drawLine
    /* draw line 4  */
    mov r0,#200
    mov r1,#200
    mov r2,#300
    mov r3,#200
    ldr r4,iAdrvar_info
    ldr r4,[r4,#FBVARSCinfo_xres]
    bl drawLine
    /* draw line 5  */
    mov r0,#200
    mov r1,#200
    mov r2,#100
    mov r3,#100
    ldr r4,iAdrvar_info
    ldr r4,[r4,#FBVARSCinfo_xres]
    bl drawLine
    /* draw line 6  */
    mov r0,#200
    mov r1,#200
    mov r2,#100         
    mov r3,#300
    ldr r4,iAdrvar_info
    ldr r4,[r4,#FBVARSCinfo_xres]
    bl drawLine
    /* draw line 7  */
    mov r0,#200
    mov r1,#200
    mov r2,#300
    mov r3,#300
    ldr r4,iAdrvar_info
    ldr r4,[r4,#FBVARSCinfo_xres]
    bl drawLine
    /* draw line 8  */
    mov r0,#200
    mov r1,#200
    mov r2,#300
    mov r3,#100
    ldr r4,iAdrvar_info
    ldr r4,[r4,#FBVARSCinfo_xres]
    bl drawLine

100:
    pop {r1-r12,lr}                 @ restaur registers
    bx lr                           @ end function

/********************************************************/
/*   set background color                               */
/********************************************************/
/* r0 contains size screen memory  */
/* r1 contains rgb code color      */
/* r9 contains screen memory address */
coloriageFond:
    push {r2,lr}
    mov r2,#0                     @ counter 
1:                                @ begin loop
    str r1,[r9,r2]
    add r2,#4
    cmp r2,r0
    blt 1b
    pop {r2,lr}
    bx lr
/********************************************************/
/*   Xiaolin Wu  line algorithm                        */
/*  no floating point compute,  multiply value for 128  */
/*  for integer compute                                 */
/********************************************************/
/* r0  x1 start line */
/* r1  y1 start line */
/* r2  x2 end line */
/* r3  y2 end line */
/* r4  screen width */
drawLine:
    push {fp,lr}      @ save registers ( no other registers save )
    mov r5,r0         @ save x1
    mov r6,r1         @ save y1
    cmp r2,r5         @ compar x2,x1
    subgt r1,r2,r5
    suble r1,r5,r2    @ compute dx=abs val de x1-x2
    cmp r3,r6         @ compar y2,y1
    subgt r0,r3,r6
    suble r0,r6,r3    @ compute dy = abs val de y1-y2
    cmp r1,r0         @ compare dx , dy
    blt 5f            @ dx < dy
                      @ dx > dy
    cmp r2,r5         @ compare x2,x1
    movlt r8,r5       @ x2 < x1 
    movlt r5,r2       @ swap x2,x1
    movlt r2,r8
    movlt r8,r6       @ swap y2,y1
    movlt r6,r3
    movlt r3,r8
    lsl r0,#7         @ * by 128
    mov r7,r2         @ save x2
    mov r8,r3         @ save y2
    cmp r1,#0         @ divisor = 0 ?
    moveq r10,#128
    beq 1f
    bl division       @ gradient compute (* 128)
    mov r10,r2        @ r10 contient le gradient
1:
    @ display start points
    mov r0,#64
    bl colorPixel
    mov r3,r0              @ RGB color 
    mov r0,r5              @ x1
    mov r1,r6              @ y1
    mov r2,r4              @ screen witdh 
    bl aff_pixel_codeRGB32 @ display pixel
    add r1,#1              @ increment y1
    bl aff_pixel_codeRGB32
    @ display end points
    mov r0,r7              @ x2
    mov r1,r8              @ y2
    bl aff_pixel_codeRGB32
    add r1,#1              @ increment y2 
    bl aff_pixel_codeRGB32
    cmp r8,r6              @ compar y2,y1
    blt 3f                 @ y2 < y1
    mov r4,r5              @ x =  x1 
    lsl r5,r6,#7           @ compute y1 * 128
    add r5,r10             @ compute intery = (y1 * 128 + gradient * 128)
2:                         @ start loop draw line pixels
    lsr r1,r5,#7           @ intery / 128  = y
    lsl r8,r1,#7
    sub r6,r5,r8           @ reminder of intery /128 = brightness
    mov r0,r6
    bl colorPixel          @ compute rgb color brightness
    mov r3,r0              @ rgb color
    mov r0,r4              @ x 
    bl aff_pixel_codeRGB32 @ display pixel 1
    add r1,#1              @ increment y
    rsb r0,r6,#128         @ compute 128 - brightness
    bl colorPixel          @ compute new rgb color
    mov r3,r0
    mov r0,r4
    bl aff_pixel_codeRGB32 @ display pixel 2
    add r5,r10             @ add gradient to intery
    add r4,#1              @ increment x
    cmp r4,r7              @ x < x2
    ble 2b                 @ yes -> loop
    b 100f                 @ else end
3:                         @ y2 < y1  
    mov r4,r7              @ x = x2 
    mov r7,r5              @ save x1
    lsl r5,r8,#7           @ y = y1 * 128 
    add r5,r10             @ compute intery = (y1 * 128 + gradient * 128)
4:
    lsr r1,r5,#7           @ y = ent(intery / 128)
    lsl r8,r1,#7
    sub r8,r5,r8           @ brightness = remainder
    mov r0,r8
    bl colorPixel
    mov r3,r0
    mov r0,r4
    bl aff_pixel_codeRGB32
    add r1,#1
    rsb r0,r8,#128
    bl colorPixel
    mov r3,r0
    mov r0,r4
    bl aff_pixel_codeRGB32
    add r5,r10
    sub r4,#1             @ decrement x
    cmp r4,r7             @ x > x1
    bgt 4b                @ yes -> loop
    b 100f
5:                        @ dx < dy
    cmp r3,r6             @ compare y2,y1
    movlt r8,r5           @ y2 < y1 
    movlt r5,r2           @ swap x1,x2
    movlt r2,r8
    movlt r8,r6           @ swap y1,y2
    movlt r6,r3
    movlt r3,r8
    mov r8,r1             @ swap r0,r1 for routine division
    mov r1,r0
    lsl r0,r8,#7          @ dx * by 128
    mov r7,r2             @ save x2
    mov r8,r3             @ save y2
    cmp r1,#0             @ dy = zero ?
    moveq r10,#128
    beq 6f
    bl division           @  compute gradient * 128
    mov r10,r2            @  gradient -> r10
6:
    @ display start points
    mov r0,#64
    bl colorPixel
    mov r3,r0             @ color pixel
    mov r0,r5             @ x1
    mov r1,r6             @ y1
    mov r2,r4             @ screen width
    bl aff_pixel_codeRGB32
    add r1,#1
    bl aff_pixel_codeRGB32
    @ display end points
    mov r0,r7
    mov r1,r8
    bl aff_pixel_codeRGB32
    add r1,#1
    bl aff_pixel_codeRGB32
    cmp r5,r7                  @ x1 < x2 ?
    blt 8f
    mov r4,r6                  @  y = y1
    lsl r5,#7                  @ compute x1 * 128
    add r5,r10                 @ compute interx
7:
    lsr r1,r5,#7               @ compute x = ent ( interx / 128)
    lsl r3,r1,#7
    sub r6,r5,r3               @ brightness = remainder
    mov r0,r6
    bl colorPixel
    mov r3,r0
    mov r0,r1                  @ new x
    add r7,r0,#1
    mov r1,r4                  @ y
    bl aff_pixel_codeRGB32
    rsb r0,r6,#128
    bl colorPixel
    mov r3,r0
    mov r0,r7                  @ new x + 1
    mov r1,r4                  @ y
    bl aff_pixel_codeRGB32
    add r5,r10
    add r4,#1
    cmp r4,r8
    ble 7b
    b 100f
8:
    mov r4,r8                  @  y = y2
    lsl r5,#7                  @ compute x1 * 128
    add r5,r10                 @ compute interx
9:
    lsr r1,r5,#7               @ compute x
    lsl r3,r1,#7
    sub r8,r5,r3
    mov r0,r8
    bl colorPixel
    mov r3,r0
    mov r0,r1                  @ new x
    add r7,r0,#1
    mov r1,r4                  @ y
    bl aff_pixel_codeRGB32
    rsb r0,r8,#128
    bl colorPixel
    mov r3,r0
    mov r0,r7                  @ new x + 1
    mov r1,r4                  @ y
    bl aff_pixel_codeRGB32
    add r5,r10
    sub r4,#1
    cmp r4,r6
    bgt 9b
    b 100f
100:
    pop {fp,lr}
    bx lr
/********************************************************/
/*   brightness color pixel                              */
/********************************************************/
/* r0 % brightness ( 0 to 128)  */
colorPixel:
    push {r1,r2,lr}    /* save des  2 registres frame et retour */
    cmp r0,#0
    beq 100f
    cmp r0,#128
    mov r0,#127
    lsl r0,#1          @ red = brightness * 2 ( 2 to 254)
    mov r1,r0          @ green = red
    mov r2,r0          @ blue = red
    bl codeRGB         @ compute rgb code color 32 bits
100:
    pop {r1,r2,lr}
    bx lr 

/***************************************************/
/*   display pixels  32 bits                       */
/***************************************************/
/* r9 framebuffer memory address */
/* r0 = x */
/* r1 = y */
/* r2 screen width in pixels */
/* r3 code color RGB 32 bits  */
aff_pixel_codeRGB32:
    push {r0-r4,lr}       @  save registers
                          @ compute location pixel
    mul r4,r1,r2          @ compute y * screen width
    add r0,r0,r4          @ + x
    lsl r0,#2             @ * 4 octets
    str r3,[r9,r0]        @ store rgb code in mmap memory
    pop {r0-r4,lr}        @ restaur registers
    bx lr
/********************************************************/
/*   Code color RGB                                     */
/********************************************************/
/* r0 red r1 green  r2 blue */
/* r0 returns RGB code      */
codeRGB:
    lsl r0,#16               @ shift red color 16 bits
    lsl r1,#8                @ shift green color 8 bits
    eor r0,r1                @ or two colors
    eor r0,r2                @ or 3 colors in r0
    bx lr

/***************************************************/
/*      ROUTINES INCLUDE                 */
/***************************************************/
.include "./affichage.inc"

/***************************************************/
/*      DEFINITION DES STRUCTURES                 */
/***************************************************/
/* structure FSCREENINFO */    
/* voir explication détaillée : https://www.kernel.org/doc/Documentation/fb/api.txt */
    .struct  0
FBFIXSCinfo_id:          /* identification string eg "TT Builtin" */
    .struct FBFIXSCinfo_id + 16  
FBFIXSCinfo_smem_start:    /* Start of frame buffer mem */
    .struct FBFIXSCinfo_smem_start + 4   
FBFIXSCinfo_smem_len:       /* Length of frame buffer mem */
    .struct FBFIXSCinfo_smem_len + 4   
FBFIXSCinfo_type:    /* see FB_TYPE_*        */
    .struct FBFIXSCinfo_type + 4  
FBFIXSCinfo_type_aux:      /* Interleave for interleaved Planes */
    .struct FBFIXSCinfo_type_aux + 4  
FBFIXSCinfo_visual:    /* see FB_VISUAL_*        */
    .struct FBFIXSCinfo_visual + 4  
FBFIXSCinfo_xpanstep:    /* zero if no hardware panning  */
    .struct FBFIXSCinfo_xpanstep + 2      
FBFIXSCinfo_ypanstep:    /* zero if no hardware panning  */
    .struct FBFIXSCinfo_ypanstep + 2 
FBFIXSCinfo_ywrapstep:      /* zero if no hardware ywrap    */
    .struct FBFIXSCinfo_ywrapstep + 4 
FBFIXSCinfo_line_length:    /* length of a line in bytes    */
    .struct FBFIXSCinfo_line_length + 4 
FBFIXSCinfo_mmio_start:     /* Start of Memory Mapped I/O   */
    .struct FBFIXSCinfo_mmio_start + 4     
FBFIXSCinfo_mmio_len:        /* Length of Memory Mapped I/O  */
    .struct FBFIXSCinfo_mmio_len + 4 
FBFIXSCinfo_accel:     /* Indicate to driver which    specific chip/card we have    */
    .struct FBFIXSCinfo_accel + 4 
FBFIXSCinfo_capabilities:     /* see FB_CAP_*            */
    .struct FBFIXSCinfo_capabilities + 4 
FBFIXSCinfo_reserved:     /* Reserved for future compatibility */
    .struct FBFIXSCinfo_reserved + 8    
FBFIXSCinfo_fin:

/* structure VSCREENINFO */    
    .struct  0
FBVARSCinfo_xres:           /* visible resolution        */ 
    .struct FBVARSCinfo_xres + 4  
FBVARSCinfo_yres:          
    .struct FBVARSCinfo_yres + 4 
FBVARSCinfo_xres_virtual:          /* virtual resolution        */
    .struct FBVARSCinfo_xres_virtual + 4 
FBVARSCinfo_yres_virtual:          
    .struct FBVARSCinfo_yres_virtual + 4 
FBVARSCinfo_xoffset:          /* offset from virtual to visible resolution */
    .struct FBVARSCinfo_xoffset + 4 
FBVARSCinfo_yoffset:          
    .struct FBVARSCinfo_yoffset + 4 
FBVARSCinfo_bits_per_pixel:          /* bits par pixel */
    .struct FBVARSCinfo_bits_per_pixel + 4     
FBVARSCinfo_grayscale:          /* 0 = color, 1 = grayscale,  >1 = FOURCC    */
    .struct FBVARSCinfo_grayscale + 4 
FBVARSCinfo_red:          /* bitfield in fb mem if true color, */
    .struct FBVARSCinfo_red + 4 
FBVARSCinfo_green:          /* else only length is significant */
    .struct FBVARSCinfo_green + 4 
FBVARSCinfo_blue:          
    .struct FBVARSCinfo_blue + 4 
FBVARSCinfo_transp:          /* transparency            */
    .struct FBVARSCinfo_transp + 4     
FBVARSCinfo_nonstd:          /* != 0 Non standard pixel format */
    .struct FBVARSCinfo_nonstd + 4 
FBVARSCinfo_activate:          /* see FB_ACTIVATE_*        */
    .struct FBVARSCinfo_activate + 4     
FBVARSCinfo_height:              /* height of picture in mm    */
    .struct FBVARSCinfo_height + 4 
FBVARSCinfo_width:           /* width of picture in mm     */
    .struct FBVARSCinfo_width + 4 
FBVARSCinfo_accel_flags:          /* (OBSOLETE) see fb_info.flags */
    .struct FBVARSCinfo_accel_flags + 4 
/* Timing: All values in pixclocks, except pixclock (of course) */    
FBVARSCinfo_pixclock:          /* pixel clock in ps (pico seconds) */
    .struct FBVARSCinfo_pixclock + 4     
FBVARSCinfo_left_margin:          
    .struct FBVARSCinfo_left_margin + 4 
FBVARSCinfo_right_margin:          
    .struct FBVARSCinfo_right_margin + 4 
FBVARSCinfo_upper_margin:          
    .struct FBVARSCinfo_upper_margin + 4 
FBVARSCinfo_lower_margin:          
    .struct FBVARSCinfo_lower_margin + 4 
FBVARSCinfo_hsync_len:          /* length of horizontal sync    */
    .struct FBVARSCinfo_hsync_len + 4     
FBVARSCinfo_vsync_len:          /* length of vertical sync    */
    .struct FBVARSCinfo_vsync_len + 4 
FBVARSCinfo_sync:          /* see FB_SYNC_*        */
    .struct FBVARSCinfo_sync + 4 
FBVARSCinfo_vmode:          /* see FB_VMODE_*        */
    .struct FBVARSCinfo_vmode + 4 
FBVARSCinfo_rotate:          /* angle we rotate counter clockwise */
    .struct FBVARSCinfo_rotate + 4     
FBVARSCinfo_colorspace:          /* colorspace for FOURCC-based modes */
    .struct FBVARSCinfo_colorspace + 4     
FBVARSCinfo_reserved:          /* Reserved for future compatibility */
    .struct FBVARSCinfo_reserved + 16        
FBVARSCinfo_fin:


```


## AutoHotkey

```AutoHotkey
#SingleInstance, Force
#NoEnv
SetBatchLines, -1

pToken := Gdip_Startup()
global pBitmap := Gdip_CreateBitmap(500, 500)
drawLine(100,50,400,400)
Gdip_SaveBitmapToFile(pBitmap, A_ScriptDir "\linetest.png")
Gdip_DisposeImage(pBitmap)
Gdip_Shutdown(pToken)
Run, % A_ScriptDir "\linetest.png"
ExitApp

plot(x, y, c) {
    A := DecToBase(255 * c, 16)
    Gdip_SetPixel(pBitmap, x, y, "0x" A "000000")
}
 
; integer part of x
ipart(x) {
    return x // 1
}
 
rnd(x) {
    return ipart(x + 0.5)
}
 
; fractional part of x
fpart(x) {
    if (x < 0)
        return 1 - (x - floor(x))
    return x - floor(x)
}
 
rfpart(x) {
    return 1 - fpart(x)
}
 
drawLine(x0,y0,x1,y1) {
    steep := abs(y1 - y0) > abs(x1 - x0)
 
    if (steep) {
        temp := x0, x0 := y0, y0 := temp
        temp := x1, x1 := y1, y1 := temp
    }
    if (x0 > x1 then) {
        temp := x0, x0 := x1, x1 := temp
        temp := y0, y0 := y1, y1 := temp
    }
 
    dx := x1 - x0
    dy := y1 - y0
    gradient := dy / dx
 
    ; handle first endpoint
    xend := rnd(x0)
    yend := y0 + gradient * (xend - x0)
    xgap := rfpart(x0 + 0.5)
    xpxl1 := xend ; this will be used in the main loop
    ypxl1 := ipart(yend)
    if (steep) {
        plot(ypxl1,   xpxl1, rfpart(yend) * xgap)
        plot(ypxl1+1, xpxl1,  fpart(yend) * xgap)
    }   
    else {
        plot(xpxl1, ypxl1  , rfpart(yend) * xgap)
        plot(xpxl1, ypxl1+1,  fpart(yend) * xgap)
    }
    intery := yend + gradient ; first y-intersection for the main loop
 
    ; handle second endpoint
    xend := rnd(x1)
    yend := y1 + gradient * (xend - x1)
    xgap := fpart(x1 + 0.5)
    xpxl2 := xend ;this will be used in the main loop
    ypxl2 := ipart(yend)
    if (steep) {
        plot(ypxl2  , xpxl2, rfpart(yend) * xgap)
        plot(ypxl2+1, xpxl2,  fpart(yend) * xgap)
    }
    else {
        plot(xpxl2, ypxl2,  rfpart(yend) * xgap)
        plot(xpxl2, ypxl2+1, fpart(yend) * xgap)
    }
 
    ; main loop
    while (x := xpxl1 + A_Index) < xpxl2 {
        if (steep) {
            plot(ipart(intery)  , x, rfpart(intery))
            plot(ipart(intery)+1, x,  fpart(intery))
        }
        else {
            plot(x, ipart (intery),  rfpart(intery))
            plot(x, ipart (intery)+1, fpart(intery))
        }
        intery := intery + gradient
    }
}

DecToBase(n, Base) {
    static U := A_IsUnicode ? "w" : "a"
    VarSetCapacity(S,65,0)
    DllCall("msvcrt\_i64to" U, "Int64",n, "Str",S, "Int",Base)
    return, S
}
```



## BBC BASIC

```bbcbasic
      PROCdrawAntiAliasedLine(100, 100, 600, 400, 0, 0, 0)
      END
      
      DEF PROCdrawAntiAliasedLine(x1, y1, x2, y2, r%, g%, b%)
      LOCAL dx, dy, xend, yend, grad, yf, xgap, ix1%, iy1%, ix2%, iy2%, x%
      
      dx = x2 - x1
      dy = y2 - y1
      IF ABS(dx) < ABS(dy) THEN
        SWAP x1, y1
        SWAP x2, y2
        SWAP dx, dy
      ENDIF
      
      IF x2 < x1 THEN
        SWAP x1, x2
        SWAP y1, y2
      ENDIF
      
      grad = dy / dx
      
      xend = INT(x1 + 0.5)
      yend = y1 + grad * (xend - x1)
      xgap = xend + 0.5 - x1
      ix1% = xend
      iy1% = INT(yend)
      PROCplot(ix1%, iy1%, r%, b%, g%, (INT(yend) + 1 - yend) * xgap)
      PROCplot(ix1%, iy1% + 1, r%, b%, g%, (yend - INT(yend)) * xgap)
      yf = yend + grad
      
      xend = INT(x2 + 0.5)
      yend = y2 + grad * (xend - x2)
      xgap = x2 + 0.5 - xend
      ix2% = xend
      iy2% = INT(yend)
      PROCplot(ix2%, iy2%, r%, b%, g%, (INT(yend) + 1 - yend) * xgap)
      PROCplot(ix2%, iy2% + 1, r%, b%, g%, (yend - INT(yend)) * xgap)
      
      FOR x% = ix1% + 1 TO ix2% - 1
        PROCplot(x%, INT(yf), r%, b%, g%, INT(yf) + 1 - yf)
        PROCplot(x%, INT(yf) + 1, r%, b%, g%, yf - INT(yf))
        yf += grad
      NEXT
      ENDPROC
      
      DEF PROCplot(X%, Y%, R%, G%, B%, a)
      LOCAL C%
      C% = TINT(X%*2,Y%*2)
      COLOUR 1, R%*a + (C% AND 255)*(1-a), \
      \         G%*a + (C% >> 8 AND 255)*(1-a), \
      \         B%*a + (C% >> 16 AND 255)*(1-a)
      GCOL 1
      LINE X%*2, Y%*2, X%*2, Y%*2
      ENDPROC
```



## C


This implementation follows straightforwardly the pseudocode given on Wikipedia. (Further analysis of the code could give suggestions for improvements). 


```c
void draw_line_antialias(
        image img,
        unsigned int x0, unsigned int y0,
        unsigned int x1, unsigned int y1,
        color_component r,
        color_component g,
        color_component b );
```



```c
inline void _dla_changebrightness(rgb_color_p from,
				  rgb_color_p to, float br)
{
  if ( br > 1.0 ) br = 1.0;
  /* linear... Maybe something more complex could give better look */
  to->red = br * (float)from->red;
  to->green = br * (float)from->green;
  to->blue = br * (float)from->blue;
}

#define plot_(X,Y,D) do{ rgb_color f_;				\
  f_.red = r; f_.green = g; f_.blue = b;			\
  _dla_plot(img, (X), (Y), &f_, (D)) ; }while(0)

inline void _dla_plot(image img, int x, int y, rgb_color_p col, float br)
{
  rgb_color oc;
  _dla_changebrightness(col, &oc, br);
  put_pixel_clip(img, x, y, oc.red, oc.green, oc.blue);
}

#define ipart_(X) ((int)(X))
#define round_(X) ((int)(((double)(X))+0.5))
#define fpart_(X) (((double)(X))-(double)ipart_(X))
#define rfpart_(X) (1.0-fpart_(X))

#define swap_(a, b) do{ __typeof__(a) tmp;  tmp = a; a = b; b = tmp; }while(0)
void draw_line_antialias(
  image img,
  unsigned int x1, unsigned int y1,
  unsigned int x2, unsigned int y2,
  color_component r,
  color_component g,
  color_component b )
{
  double dx = (double)x2 - (double)x1;
  double dy = (double)y2 - (double)y1;
  if ( fabs(dx) > fabs(dy) ) {
    if ( x2 < x1 ) {
      swap_(x1, x2);
      swap_(y1, y2);
    }
    double gradient = dy / dx;
    double xend = round_(x1);
    double yend = y1 + gradient*(xend - x1);
    double xgap = rfpart_(x1 + 0.5);
    int xpxl1 = xend;
    int ypxl1 = ipart_(yend);
    plot_(xpxl1, ypxl1, rfpart_(yend)*xgap);
    plot_(xpxl1, ypxl1+1, fpart_(yend)*xgap);
    double intery = yend + gradient;

    xend = round_(x2);
    yend = y2 + gradient*(xend - x2);
    xgap = fpart_(x2+0.5);
    int xpxl2 = xend;
    int ypxl2 = ipart_(yend);
    plot_(xpxl2, ypxl2, rfpart_(yend) * xgap);
    plot_(xpxl2, ypxl2 + 1, fpart_(yend) * xgap);

    int x;
    for(x=xpxl1+1; x < xpxl2; x++) {
      plot_(x, ipart_(intery), rfpart_(intery));
      plot_(x, ipart_(intery) + 1, fpart_(intery));
      intery += gradient;
    }
  } else {
    if ( y2 < y1 ) {
      swap_(x1, x2);
      swap_(y1, y2);
    }
    double gradient = dx / dy;
    double yend = round_(y1);
    double xend = x1 + gradient*(yend - y1);
    double ygap = rfpart_(y1 + 0.5);
    int ypxl1 = yend;
    int xpxl1 = ipart_(xend);
    plot_(xpxl1, ypxl1, rfpart_(xend)*ygap);
    plot_(xpxl1 + 1, ypxl1, fpart_(xend)*ygap);
    double interx = xend + gradient;

    yend = round_(y2);
    xend = x2 + gradient*(yend - y2);
    ygap = fpart_(y2+0.5);
    int ypxl2 = yend;
    int xpxl2 = ipart_(xend);
    plot_(xpxl2, ypxl2, rfpart_(xend) * ygap);
    plot_(xpxl2 + 1, ypxl2, fpart_(xend) * ygap);

    int y;
    for(y=ypxl1+1; y < ypxl2; y++) {
      plot_(ipart_(interx), y, rfpart_(interx));
      plot_(ipart_(interx) + 1, y, fpart_(interx));
      interx += gradient;
    }
  }
}
#undef swap_
#undef plot_
#undef ipart_
#undef fpart_
#undef round_
#undef rfpart_

```


== {{header|C++}}  ==


```c++

#include <functional>
#include <algorithm>
#include <utility>

void WuDrawLine(float x0, float y0, float x1, float y1,
                const std::function<void(int x, int y, float brightess)>& plot) {
    auto ipart = [](float x) -> int {return int(std::floor(x));};
    auto round = [](float x) -> float {return std::round(x);};
    auto fpart = [](float x) -> float {return x - std::floor(x);};
    auto rfpart = [=](float x) -> float {return 1 - fpart(x);};
        
    const bool steep = abs(y1 - y0) > abs(x1 - x0);
    if (steep) {
        std::swap(x0,y0);
        std::swap(x1,y1);
    }
    if (x0 > x1) {
        std::swap(x0,x1);
        std::swap(y0,y1);
    }
        
    const float dx = x1 - x0;
    const float dy = y1 - y0;
    const float gradient = (dx == 0) ? 1 : dy/dx;
        
    int xpx11;
    float intery;
    {
        const float xend = round(x0);
        const float yend = y0 + gradient * (xend - x0);
        const float xgap = rfpart(x0 + 0.5);
        xpx11 = int(xend);
        const int ypx11 = ipart(yend);
        if (steep) {
            plot(ypx11,     xpx11, rfpart(yend) * xgap);
            plot(ypx11 + 1, xpx11,  fpart(yend) * xgap);
        } else {
            plot(xpx11, ypx11,    rfpart(yend) * xgap);
            plot(xpx11, ypx11 + 1, fpart(yend) * xgap);
        }
        intery = yend + gradient;
    }
    
    int xpx12;
    {
        const float xend = round(x1);
        const float yend = y1 + gradient * (xend - x1);
        const float xgap = rfpart(x1 + 0.5);
        xpx12 = int(xend);
        const int ypx12 = ipart(yend);
        if (steep) {
            plot(ypx12,     xpx12, rfpart(yend) * xgap);
            plot(ypx12 + 1, xpx12,  fpart(yend) * xgap);
        } else {
            plot(xpx12, ypx12,    rfpart(yend) * xgap);
            plot(xpx12, ypx12 + 1, fpart(yend) * xgap);
        }
    }
        
    if (steep) {
        for (int x = xpx11 + 1; x < xpx12; x++) {
            plot(ipart(intery),     x, rfpart(intery));
            plot(ipart(intery) + 1, x,  fpart(intery));
            intery += gradient;
        }
    } else {
        for (int x = xpx11 + 1; x < xpx12; x++) {
            plot(x, ipart(intery),     rfpart(intery));
            plot(x, ipart(intery) + 1,  fpart(intery));
            intery += gradient;
        }
    }
}

```


== {{header|C#}}  ==

```c

public class Line
    {
        private double x0, y0, x1, y1;
        private Color foreColor;
        private byte lineStyleMask;
        private int thickness;
        private float globalm;

        public Line(double x0, double y0, double x1, double y1, Color color, byte lineStyleMask, int thickness)
        {
            this.x0 = x0;
            this.y0 = y0;
            this.y1 = y1;
            this.x1 = x1;

            this.foreColor = color;

            this.lineStyleMask = lineStyleMask;

            this.thickness = thickness;

        }

        private void plot(Bitmap bitmap, double x, double y, double c)
        {
            int alpha = (int)(c * 255);
            if (alpha > 255) alpha = 255;
            if (alpha < 0) alpha = 0;
            Color color = Color.FromArgb(alpha, foreColor);
            if (BitmapDrawHelper.checkIfInside((int)x, (int)y, bitmap))
            {
                bitmap.SetPixel((int)x, (int)y, color);
            }
        }

        int ipart(double x) { return (int)x;}

        int round(double x) {return ipart(x+0.5);}
    
        double fpart(double x) {
            if(x<0) return (1-(x-Math.Floor(x)));
            return (x-Math.Floor(x));
        }
    
        double rfpart(double x) {
            return 1-fpart(x);
        }


        public void draw(Bitmap bitmap) {
            bool steep = Math.Abs(y1-y0)>Math.Abs(x1-x0);
             double temp;
            if(steep){
                temp=x0; x0=y0; y0=temp;
                temp=x1;x1=y1;y1=temp;
            }
            if(x0>x1){
                temp = x0;x0=x1;x1=temp;
                temp = y0;y0=y1;y1=temp;
            }

            double dx = x1-x0;
            double dy = y1-y0;
            double gradient = dy/dx;

            double xEnd = round(x0);
            double yEnd = y0+gradient*(xEnd-x0);
            double xGap = rfpart(x0+0.5);
            double xPixel1 = xEnd;
            double yPixel1 = ipart(yEnd);

            if(steep){
                plot(bitmap, yPixel1,   xPixel1, rfpart(yEnd)*xGap);
                plot(bitmap, yPixel1+1, xPixel1,  fpart(yEnd)*xGap);
            }else{
                plot(bitmap, xPixel1,yPixel1, rfpart(yEnd)*xGap);
                plot(bitmap, xPixel1, yPixel1+1, fpart(yEnd)*xGap);
            }
            double intery = yEnd+gradient;

            xEnd = round(x1);
            yEnd = y1+gradient*(xEnd-x1);
            xGap = fpart(x1+0.5);
            double xPixel2 = xEnd;
            double yPixel2 = ipart(yEnd);
            if(steep){
                plot(bitmap, yPixel2,   xPixel2, rfpart(yEnd)*xGap);
                plot(bitmap, yPixel2+1, xPixel2, fpart(yEnd)*xGap);
            }else{
                plot(bitmap, xPixel2, yPixel2, rfpart(yEnd)*xGap);
                plot(bitmap, xPixel2, yPixel2+1, fpart(yEnd)*xGap);
            }

            if(steep){
                for(int x=(int)(xPixel1+1);x<=xPixel2-1;x++){
                    plot(bitmap, ipart(intery), x, rfpart(intery));
                    plot(bitmap, ipart(intery)+1, x, fpart(intery));
                    intery+=gradient;
                }
            }else{
                for(int x=(int)(xPixel1+1);x<=xPixel2-1;x++){
                    plot(bitmap, x,ipart(intery), rfpart(intery));
                    plot(bitmap, x, ipart(intery)+1, fpart(intery));
                    intery+=gradient;
                }
            }
        }
    }

```



## D

This performs the mixing of the colors, both in grey scale and RGB.

```d
import std.math, std.algorithm, grayscale_image;

/// Plots anti-aliased line by Xiaolin Wu's line algorithm.
void aaLine(Color)(ref Image!Color img,
                   double x1, double y1,
                   double x2, double y2,
                   in Color color) pure nothrow @safe @nogc {
    // Straight translation of Wikipedia pseudocode.

    // std.math.round is not pure. **
    static double round(in double x) pure nothrow @safe @nogc {
        return floor(x + 0.5);
    }

    static double fpart(in double x) pure nothrow @safe @nogc {
        return x - x.floor;
    }

    static double rfpart(in double x) pure nothrow @safe @nogc {
        return 1 - fpart(x);
    }

    auto dx = x2 - x1;
    auto dy = y2 - y1;
    immutable ax = dx.abs;
    immutable ay = dy.abs;

    static Color mixColors(in Color c1, in Color c2, in double p)
    pure nothrow @safe @nogc {
        static if (is(Color == RGB))
            return Color(cast(ubyte)(c1.r * p + c2.r * (1 - p)),
                         cast(ubyte)(c1.g * p + c2.g * (1 - p)),
                         cast(ubyte)(c1.b * p + c2.b * (1 - p)));
        else
            // This doesn't work for every kind of Color.
            return Color(cast(ubyte)(c1 * p + c2 * (1 - p)));
    }

    // Plot function set here to handle the two cases of slope.
    void function(ref Image!Color, in int, in int, in double, in Color)
    pure nothrow @safe @nogc plot;

    if (ax < ay) {
        swap(x1, y1);
        swap(x2, y2);
        swap(dx, dy);
        //plot = (img, x, y, p, col) {
        plot = (ref img, x, y, p, col) {
            assert(p >= 0.0 && p <= 1.0);
            img[y, x] = mixColors(col, img[y, x], p);
        };
    } else {
        //plot = (img, x, y, p, col) {
        plot = (ref img, x, y, p, col) {
            assert(p >= 0.0 && p <= 1.0);
            img[x, y] = mixColors(col, img[x, y], p);
        };
    }

    if (x2 < x1) {
        swap(x1, x2);
        swap(y1, y2);
    }
    immutable gradient = dy / dx;

    // Handle first endpoint.
    auto xEnd = round(x1);
    auto yEnd = y1 + gradient * (xEnd - x1);
    auto xGap = rfpart(x1 + 0.5);
    // This will be used in the main loop.
    immutable xpxl1 = cast(int)xEnd;
    immutable ypxl1 = cast(int)yEnd.floor;
    plot(img, xpxl1, ypxl1, rfpart(yEnd) * xGap, color);
    plot(img, xpxl1, ypxl1 + 1, fpart(yEnd) * xGap, color);
    // First y-intersection for the main loop.
    auto yInter = yEnd + gradient;

    // Handle second endpoint.
    xEnd = round(x2);
    yEnd = y2 + gradient * (xEnd - x2);
    xGap = fpart(x2 + 0.5);
    // This will be used in the main loop.
    immutable xpxl2 = cast(int)xEnd;
    immutable ypxl2 = cast(int)yEnd.floor;
    plot(img, xpxl2, ypxl2, rfpart(yEnd) * xGap, color);
    plot(img, xpxl2, ypxl2 + 1, fpart(yEnd) * xGap, color);

    // Main loop.
    foreach (immutable x; xpxl1 + 1 .. xpxl2) {
        plot(img, x, cast(int)yInter.floor, rfpart(yInter), color);
        plot(img, x, cast(int)yInter.floor + 1, fpart(yInter), color);
        yInter += gradient;
    }
}

void main() {
    auto im1 = new Image!Gray(400, 300);
    im1.clear(Gray.white);
    im1.aaLine(7.4, 12.3, 307, 122.5, Gray.black);
    im1.aaLine(177.4, 12.3, 127, 222.5, Gray.black);
    im1.savePGM("xiaolin_lines1.pgm");

    auto im2 = new Image!RGB(400, 300);
    im2.clear(RGB(0, 255, 0));
    immutable red = RGB(255, 0, 0);
    im2.aaLine(7.4, 12.3, 307, 122.5, red);
    im2.aaLine(177.4, 12.3, 127, 222.5, red);
    im2.savePPM6("xiaolin_lines2.ppm");
}
```


## FreeBASIC

This implementation follows the pseudocode given on Wikipedia.
Only changed xend=round() in xend=ipart() to make it more in line with FreeBASIC's own line drawing routine. Rfpart give me some trouble so I changed if somewhat.
The small functions where all converted into macro's

```FreeBASIC
' version 21-06-2015
' compile with: fbc -s console or fbc -s gui
' Xiaolin Wu’s line-drawing algorithm
'shared var and macro's

Dim Shared As UInteger wu_color

#Macro ipart(x)
Int(x)             ' integer part
#EndMacro

#Macro round(x)
Int((x) + .5)      ' round off
#EndMacro

#Macro fpart(x)
Frac(x)            ' fractional part
#EndMacro

#Macro rfpart(x)
' 1 - Frac(x)    ' seems to give problems for very small x
IIf(1 - Frac(x) >= 1, 1, 1 - Frac(x))
#EndMacro

#Macro plot(x, y , c)
' use the alpha channel to set the amount of color
PSet(x,y), wu_color Or (Int(c * 255)) Shl 24
#EndMacro

Sub drawline(x0 As Single, y0 As Single, x1 As Single, y1 As Single,_
    col As UInteger = RGB(255,255,255))

    wu_color = col And &HFFFFFF ' strip off the alpha channel information

    Dim As Single gradient
    Dim As Single xend, yend, xgap, intery
    Dim As UInteger xpxl1, ypxl1, xpxl2, ypxl2, x
    Dim As Integer steep = Abs(y1 - y0) > Abs(x1 - x0) ' boolean

    If steep Then
        Swap x0, y0
        Swap x1, y1
    End If

    If x0 > x1 Then
        Swap x0, x1
        Swap y0, y1
    End If

    gradient = (y1 - y0) / (x1 - x0)

    ' first endpoint
    ' xend = round(x0)
    xend = ipart(x0)
    yend = y0 + gradient * (xend - x0)
    xgap = rfpart(x0 + .5)
    xpxl1 = xend              ' this will be used in the main loop
    ypxl1 = ipart(yend)
    If steep Then
        plot(ypxl1,   xpxl1, rfpart(yend) * xgap)
        plot(ypxl1+1, xpxl1,  fpart(yend) * xgap)
    Else
        plot(xpxl1, ypxl1,   rfpart(yend) * xgap)
        plot(xpxl1, ypxl1+1,  fpart(yend) * xgap)
    End If
    intery = yend + gradient  ' first y-intersecction for the main loop

    ' handle second endpoint
    ' xend = round(x1)
    xend = ipart(x1)
    yend = y1 + gradient * (xend - x1)
    xgap = fpart(x1 + .5)
    xpxl2 = xend              ' this will be used in the main loop
    ypxl2 = ipart(yend)
    If steep Then
        plot(ypxl2,   xpxl2, rfpart(yend) * xgap)
        plot(ypxl2+1, xpxl2,  fpart(yend) * xgap)
    Else
        plot(xpxl2, ypxl2,   rfpart(yend) * xgap)
        plot(xpxl2, ypxl2+1,  fpart(yend) * xgap)
    End If

    ' main loop
    If steep Then
        For x = xpxl1 + 1 To xpxl2 - 1
            plot(ipart(intery),   x, rfpart(intery))
            plot(ipart(intery)+1, x,  fpart(intery))
            intery = intery + gradient
        Next
    Else
        For x = xpxl1 + 1 To xpxl2 - 1
            plot(x, ipart(intery),   rfpart(intery))
            plot(x, ipart(intery)+1,  fpart(intery))
            intery = intery + gradient
        Next
    End If

End Sub

' ------=< MAIN >=------

#Define W_  600
#Define H_  600

#Include Once "fbgfx.bi"   ' needed setting the screen attributes
Dim As Integer i
Dim As String fname = __FILE__

ScreenRes W_, H_, 32,, FB.GFX_ALPHA_PRIMITIVES

Randomize Timer

For i = 0 To H_ Step H_\30
    drawline(0, 0, W_, i, Int(Rnd * &HFFFFFF))
Next

For i = 0 To W_ Step W_\30
    drawline(0, 0, i, H_, Int(Rnd * &HFFFFFF))
Next

i = InStr(fname,".bas")
fname = Left(fname, Len(fname)-i+1)
WindowTitle fname + "    hit any key to end program"

While Inkey <> "" : Wend
Sleep
End
```



## Go


```go
package raster

import "math"

func ipart(x float64) float64 {
    return math.Floor(x)
}

func round(x float64) float64 {
    return ipart(x + .5)
}

func fpart(x float64) float64 {
    return x - ipart(x)
}

func rfpart(x float64) float64 {
    return 1 - fpart(x)
}

// AaLine plots anti-aliased line by Xiaolin Wu's line algorithm.
func (g *Grmap) AaLine(x1, y1, x2, y2 float64) {
    // straight translation of WP pseudocode
    dx := x2 - x1
    dy := y2 - y1
    ax := dx
    if ax < 0 {
        ax = -ax
    }
    ay := dy
    if ay < 0 {
        ay = -ay
    }
    // plot function set here to handle the two cases of slope
    var plot func(int, int, float64)
    if ax < ay {
        x1, y1 = y1, x1
        x2, y2 = y2, x2
        dx, dy = dy, dx
        plot = func(x, y int, c float64) {
            g.SetPx(y, x, uint16(c*math.MaxUint16))
        }
    } else {
        plot = func(x, y int, c float64) {
            g.SetPx(x, y, uint16(c*math.MaxUint16))
        }
    }
    if x2 < x1 {
        x1, x2 = x2, x1
        y1, y2 = y2, y1
    }
    gradient := dy / dx

    // handle first endpoint
    xend := round(x1)
    yend := y1 + gradient*(xend-x1)
    xgap := rfpart(x1 + .5)
    xpxl1 := int(xend) // this will be used in the main loop
    ypxl1 := int(ipart(yend))
    plot(xpxl1, ypxl1, rfpart(yend)*xgap)
    plot(xpxl1, ypxl1+1, fpart(yend)*xgap)
    intery := yend + gradient // first y-intersection for the main loop

    // handle second endpoint
    xend = round(x2)
    yend = y2 + gradient*(xend-x2)
    xgap = fpart(x2 + 0.5)
    xpxl2 := int(xend) // this will be used in the main loop
    ypxl2 := int(ipart(yend))
    plot(xpxl2, ypxl2, rfpart(yend)*xgap)
    plot(xpxl2, ypxl2+1, fpart(yend)*xgap)

    // main loop
    for x := xpxl1 + 1; x <= xpxl2-1; x++ {
        plot(x, int(ipart(intery)), rfpart(intery))
        plot(x, int(ipart(intery))+1, fpart(intery))
        intery = intery + gradient
    }
}
```

Demonstration program:

```go
package main

// Files required to build supporting package raster are found in:
// * This task (immediately above)
// * Bitmap
// * Grayscale image
// * Write a PPM file

import "raster"

func main() {
    g := raster.NewGrmap(400, 300)
    g.AaLine(7.4, 12.3, 307, 122.5)
    g.AaLine(177.4, 12.3, 127, 222.5)
    g.Bitmap().WritePpmFile("wu.ppm")
}
```



## Haskell


Example makes use of [http://hackage.haskell.org/package/JuicyPixels <tt>JuicyPixels</tt>] for serialization to PNG format and and [http://hackage.haskell.org/package/primitive <tt>primitive</tt>] to abstract away memory-related operations. This is a fairly close translation of the algorithm as described on [https://en.wikipedia.org/wiki/Xiaolin_Wu%27s_line_algorithm Wikipedia]:


```haskell
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Codec.Picture (writePng)
import Codec.Picture.Types (Image, MutableImage(..), Pixel, PixelRGB8(..), createMutableImage, unsafeFreezeImage, writePixel)
import Control.Monad (void)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (foldlM)

type MImage m px = MutableImage (PrimState m) px

-- | Create an image given a function to apply to an empty mutable image
withMutableImage
    :: (Pixel px, PrimMonad m)
    => Int                      -- ^ image width
    -> Int                      -- ^ image height
    -> px                       -- ^ background colour
    -> (MImage m px -> m ())    -- ^ function to apply to mutable image
    -> m (Image px)             -- ^ action
withMutableImage w h px f = createMutableImage w h px >>= \m -> f m >> unsafeFreezeImage m

-- | Plot a pixel at the given point in the given colour
plot
    :: (Pixel px, PrimMonad m)
    => MImage m px  -- ^ mutable image
    -> Int          -- ^ x-coordinate of point
    -> Int          -- ^ y-coordinate of point
    -> px           -- ^ colour
    -> m ()         -- ^ action
plot = writePixel

-- | Draw an antialiased line from first point to second point in given colour
drawAntialiasedLine
    :: forall px m . (Pixel px, PrimMonad m)
    => MImage m px      -- ^ mutable image
    -> Int              -- ^ x-coordinate of first point
    -> Int              -- ^ y-coordinate of first point
    -> Int              -- ^ x-coordinate of second point
    -> Int              -- ^ y-coordinate of second point
    -> (Double -> px)   -- ^ colour generator function
    -> m ()             -- ^ action
drawAntialiasedLine m p1x p1y p2x p2y colour = do
    let steep = abs (p2y - p1y) > abs (p2x - p1x)
        ((p3x, p4x), (p3y, p4y)) = swapIf steep ((p1x, p2x), (p1y, p2y))
        ((ax, ay), (bx, by)) = swapIf (p3x > p4x) ((p3x, p3y), (p4x, p4y))
        dx = bx - ax
        dy = by - ay
        gradient = if dx == 0 then 1.0 else fromIntegral dy / fromIntegral dx

    -- handle first endpoint
    let xpxl1 = ax -- round (fromIntegral ax)
        yend1 = fromIntegral ay + gradient * fromIntegral (xpxl1 - ax)
        xgap1 = rfpart (fromIntegral ax + 0.5)
    endpoint steep xpxl1 yend1 xgap1

    -- handle second endpoint
    let xpxl2 = bx -- round (fromIntegral bx)
        yend2 = fromIntegral by + gradient * fromIntegral (xpxl2 - bx)
        xgap2 = fpart (fromIntegral bx + 0.5)
    endpoint steep xpxl2 yend2 xgap2

    -- main loop
    let intery = yend1 + gradient
    void $ if steep
        then foldlM (\i x -> do
            plot m (ipart i) x (colour (rfpart i))
            plot m (ipart i + 1) x (colour (fpart i))
            pure $ i + gradient) intery [xpxl1 + 1..xpxl2 - 1]
        else foldlM (\i x -> do
            plot m x (ipart i) (colour (rfpart i))
            plot m x (ipart i + 1) (colour (fpart i))
            pure $ i + gradient) intery [xpxl1 + 1..xpxl2 - 1]

    where
        endpoint :: Bool -> Int -> Double -> Double -> m ()
        endpoint True xpxl yend xgap = do
            plot m ypxl xpxl (colour (rfpart yend * xgap))
            plot m (ypxl + 1) xpxl (colour (fpart yend * xgap))
            where ypxl = ipart yend
        endpoint False xpxl yend xgap = do
            plot m xpxl ypxl (colour (rfpart yend * xgap))
            plot m xpxl (ypxl + 1) (colour (fpart yend * xgap))
            where ypxl = ipart yend

swapIf :: Bool -> (a, a) -> (a, a)
swapIf False p = p
swapIf True (x, y) = (y, x)

ipart :: Double -> Int
ipart = truncate

fpart :: Double -> Double
fpart x
    | x > 0 = x - temp
    | otherwise = x - (temp + 1)
    where temp = fromIntegral (ipart x)

rfpart :: Double -> Double
rfpart x = 1 - fpart x

main :: IO ()
main = do
    -- We start and end the line with sufficient clearance from the edge of the
    -- image to be able to see the endpoints
    img <- withMutableImage 640 480 (PixelRGB8 0 0 0) $ \m@(MutableImage w h _) ->
            drawAntialiasedLine m 2 2 (w - 2) (h - 2)
            (\brightness -> let level = round (brightness * 255) in PixelRGB8 level level level)

    -- Write it out to a file on disc
    writePng "xiaolin-wu-algorithm.png" img
```


Building and running this program will generate an output PNG file named <code>xiaolin-wu-algorithm.png</code> showing a white antialiased diagonal line.


## J

'''Solution:'''

```j
load'gl2'
coinsert'jgl2'

drawpt=:4 :0"0 1
   glrgb <.(-.x)*255 255 255
   glpixel y
)

drawLine=:3 :0 NB. drawline x1,y1,x2,y2
   pts=. 2 2$y
   isreversed=. </ |d=. -~/pts
   r=. |.^:isreversed"1
   pts=. /:~ pts \:"1 |d
   gradient=. %~/ (\:|)d

   'x y'=. |:pts
   xend=. <.0.5+ x
   yend=. y + gradient* xend-x
   xgap=. -.1|x+0.5

   n=. i. >: -~/ xend
   'xlist ylist'=. (n*/~1,gradient) + ({.xend),({.yend)
   weights=. ((2&}.,~ xgap*2&{.)&.(_1&|.) (,.~-.) 1|ylist)
   weights (drawpt r)"1 2 (,:+&0 1)"1 xlist,.<.ylist
)
```


'''Example use:'''

```j
   wd'pc win closeok; xywh 0 0 300 200;cc g isigraph; pas 0 0; pshow;' NB. J6 or earlier
   wd'pc win closeok; minwh 600 400;cc g isidraw flush; pshow;'        NB. J802 or later
   glpaint glclear ''
   glpaint drawLine 10 10 590 390
```



## Java

[[File:xiaolinwu_java.png|200px|thumb|right]]
```java
import java.awt.*;
import static java.lang.Math.*;
import javax.swing.*;

public class XiaolinWu extends JPanel {

    public XiaolinWu() {
        Dimension dim = new Dimension(640, 640);
        setPreferredSize(dim);
        setBackground(Color.white);
    }

    void plot(Graphics2D g, double x, double y, double c) {
        g.setColor(new Color(0f, 0f, 0f, (float)c));
        g.fillOval((int) x, (int) y, 2, 2);
    }

    int ipart(double x) {
        return (int) x;
    }

    double fpart(double x) {
        return x - floor(x);
    }

    double rfpart(double x) {
        return 1.0 - fpart(x);
    }

    void drawLine(Graphics2D g, double x0, double y0, double x1, double y1) {

        boolean steep = abs(y1 - y0) > abs(x1 - x0);
        if (steep)
            drawLine(g, y0, x0, y1, x1);

        if (x0 > x1)
            drawLine(g, x1, y1, x0, y0);

        double dx = x1 - x0;
        double dy = y1 - y0;
        double gradient = dy / dx;

        // handle first endpoint
        double xend = round(x0);
        double yend = y0 + gradient * (xend - x0);
        double xgap = rfpart(x0 + 0.5);
        double xpxl1 = xend; // this will be used in the main loop
        double ypxl1 = ipart(yend);

        if (steep) {
            plot(g, ypxl1, xpxl1, rfpart(yend) * xgap);
            plot(g, ypxl1 + 1, xpxl1, fpart(yend) * xgap);
        } else {
            plot(g, xpxl1, ypxl1, rfpart(yend) * xgap);
            plot(g, xpxl1, ypxl1 + 1, fpart(yend) * xgap);
        }

        // first y-intersection for the main loop
        double intery = yend + gradient;

        // handle second endpoint
        xend = round(x1);
        yend = y1 + gradient * (xend - x1);
        xgap = fpart(x1 + 0.5);
        double xpxl2 = xend; // this will be used in the main loop
        double ypxl2 = ipart(yend);

        if (steep) {
            plot(g, ypxl2, xpxl2, rfpart(yend) * xgap);
            plot(g, ypxl2 + 1, xpxl2, fpart(yend) * xgap);
        } else {
            plot(g, xpxl2, ypxl2, rfpart(yend) * xgap);
            plot(g, xpxl2, ypxl2 + 1, fpart(yend) * xgap);
        }

        // main loop
        for (double x = xpxl1 + 1; x <= xpxl2 - 1; x++) {
            if (steep) {
                plot(g, ipart(intery), x, rfpart(intery));
                plot(g, ipart(intery) + 1, x, fpart(intery));
            } else {
                plot(g, x, ipart(intery), rfpart(intery));
                plot(g, x, ipart(intery) + 1, fpart(intery));
            }
            intery = intery + gradient;
        }
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;

        drawLine(g, 550, 170, 50, 435);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Xiaolin Wu's line algorithm");
            f.setResizable(false);
            f.add(new XiaolinWu(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## Julia

```julia
using Images

fpart(x) = mod(x, one(x))
rfpart(x) = one(x) - fpart(x)

function drawline!(img::Matrix{Gray{N0f8}}, x0::Integer, y0::Integer, x1::Integer, y1::Integer)
    steep = abs(y1 - y0) > abs(x1 - x0)

    if steep
        x0, y0 = y0, x0
        x1, y1 = y1, x1
    end
    if x0 > x1
        x0, x1 = x1, x0
        y0, y1 = y1, y0
    end

    dx = x1 - x0
    dy = y1 - y0
    grad = dy / dx

    if iszero(dx)
        grad = oftype(grad, 1.0)
    end

    # handle first endpoint
    xend = round(Int, x0)
    yend = y0 + grad * (xend - x0)
    xgap = rfpart(x0 + 0.5)
    xpxl1 = xend
    ypxl1 = floor(Int, yend)

    if steep
        img[ypxl1,   xpxl1] = rfpart(yend) * xgap
        img[ypxl1+1, xpxl1] =  fpart(yend) * xgap
    else
        img[xpxl1, ypxl1  ] = rfpart(yend) * xgap
        img[xpxl1, ypxl1+1] =  fpart(yend) * xgap
    end
    intery = yend + grad # first y-intersection for the main loop

    # handle second endpoint
    xend = round(Int, x1)
    yend = y1 + grad * (xend - x1)
    xgap = fpart(x1 + 0.5)
    xpxl2 = xend
    ypxl2 = floor(Int, yend)
    if steep
        img[ypxl2,   xpxl2] = rfpart(yend) * xgap
        img[ypxl2+1, xpxl2] =  fpart(yend) * xgap
    else
        img[xpxl2, ypxl2  ] = rfpart(yend) * xgap
        img[xpxl2, ypxl2+1] =  fpart(yend) * xgap
    end

    # main loop
    if steep
        for x in xpxl1+1:xpxl2-1
            img[floor(Int, intery),   x] = rfpart(intery)
            img[floor(Int, intery)+1, x] =  fpart(intery)
            intery += grad
        end
    else
        for x in xpxl1+1:xpxl2-1
            img[x, floor(Int, intery)  ] = rfpart(intery)
            img[x, floor(Int, intery)+1] =  fpart(intery)
            intery += grad
        end
    end

    return img
end

img = fill(Gray(1.0N0f8), 250, 250);
drawline!(img, 8, 8, 192, 154)
```



## Kotlin

```scala
// version 1.1.2

import java.awt.*
import javax.swing.*

class XiaolinWu: JPanel() {
    init {
        preferredSize = Dimension(640, 640)
        background = Color.white
    }

    private fun plot(g: Graphics2D, x: Double, y: Double, c: Double) {
        g.color = Color(0f, 0f, 0f, c.toFloat())
        g.fillOval(x.toInt(), y.toInt(), 2, 2)
    }

    private fun ipart(x: Double) = x.toInt()

    private fun fpart(x: Double) = x - Math.floor(x)

    private fun rfpart(x: Double) = 1.0 - fpart(x)

    private fun drawLine(g: Graphics2D, x0: Double, y0: Double, x1: Double, y1: Double) {
        val steep = Math.abs(y1 - y0) > Math.abs(x1 - x0)
        if (steep) drawLine(g, y0, x0, y1, x1)
        if (x0 > x1) drawLine(g, x1, y1, x0, y0)

        val dx = x1 - x0
        val dy = y1 - y0
        val gradient = dy / dx

        // handle first endpoint
        var xend = Math.round(x0).toDouble()
        var yend = y0 + gradient * (xend - x0)
        var xgap = rfpart(x0 + 0.5)
        val xpxl1 = xend  // this will be used in the main loop
        val ypxl1 = ipart(yend).toDouble()

        if (steep) {
            plot(g, ypxl1, xpxl1, rfpart(yend) * xgap)
            plot(g, ypxl1 + 1.0, xpxl1, fpart(yend) * xgap)
        }
        else {
            plot(g, xpxl1, ypxl1, rfpart(yend) * xgap)
            plot(g, xpxl1, ypxl1 + 1.0, fpart(yend) * xgap)
        }

        // first y-intersection for the main loop
        var intery = yend + gradient

        // handle second endpoint
        xend = Math.round(x1).toDouble()
        yend = y1 + gradient * (xend - x1)
        xgap = fpart(x1 + 0.5)
        val xpxl2 = xend  // this will be used in the main loop
        val ypxl2 = ipart(yend).toDouble()

        if (steep) {
            plot(g, ypxl2, xpxl2, rfpart(yend) * xgap)
            plot(g, ypxl2 + 1.0, xpxl2, fpart(yend) * xgap)
        }
        else {
            plot(g, xpxl2, ypxl2, rfpart(yend) * xgap)
            plot(g, xpxl2, ypxl2 + 1.0, fpart(yend) * xgap)
        }

        // main loop
        var x = xpxl1 + 1.0
        while (x <=  xpxl2 - 1) {
            if (steep) {
                plot(g, ipart(intery).toDouble(), x, rfpart(intery))
                plot(g, ipart(intery).toDouble() + 1.0, x, fpart(intery))
            }
            else {
                plot(g, x, ipart(intery).toDouble(), rfpart(intery))
                plot(g, x, ipart(intery).toDouble() + 1.0, fpart(intery))
            }
            intery += gradient
            x++
        }
    }

    override protected fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        drawLine(g, 550.0, 170.0, 50.0, 435.0)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        f.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        f.title = "Xiaolin Wu's line algorithm"
        f.isResizable = false
        f.add(XiaolinWu(), BorderLayout.CENTER)
        f.pack()
        f.setLocationRelativeTo(null)
        f.isVisible = true
    }
}
```



## Liberty BASIC


```lb

NoMainWin
WindowWidth = 270
WindowHeight = 290
UpperLeftX=int((DisplayWidth-WindowWidth)/2)
UpperLeftY=int((DisplayHeight-WindowHeight)/2)

Global variablesInitialized : variablesInitialized = 0
Global BackColor$ : BackColor$ = "0 0 0"
'    BackColor$ = "255 255 255"
    'now, right click randomizes BG
Global size : size = 1'4
global mousepoints.mouseX0,  mousepoints.mouseY0, mousepoints.mouseX1, mousepoints.mouseY1

'StyleBits #main.gbox, 0, _WS_BORDER, 0, 0
GraphicBox #main.gbox, 0, 0, 253, 252

Open "Click Twice to Form Line" For Window As #main
Print #main, "TrapClose quit"
Print #main.gbox, "Down; Color Black"
Print #main.gbox, "Down; fill ";BackColor$
Print #main.gbox, "When leftButtonUp gBoxClick"
Print #main.gbox, "When rightButtonUp RandomBG"
Print #main.gbox, "Size "; size

result = drawAntiAliasedLine(126.5, 0, 126.5, 252, "255 0 0")
result = drawAntiAliasedLine(0, 126, 253, 126, "255 0 0")
result = drawAntiAliasedLine(0, 0, 253, 252, "255 0 0")
result = drawAntiAliasedLine(253, 0, 0, 252, "255 0 0")
Wait


    Sub quit handle$
        Close #main
        End
    End Sub

sub RandomBG handle$, MouseX, MouseY
    BackColor$ = int(rnd(1)*256);" ";int(rnd(1)*256);" ";int(rnd(1)*256)
    Print #main.gbox, "CLS; fill ";BackColor$
    variablesInitialized = 0
end sub

    Sub gBoxClick handle$, MouseX, MouseY
        'We will use the mousepoints "struct" to hold the values
        'that way they are retained between subroutine calls
        If variablesInitialized = 0 Then
            Print #main.gbox, "CLS; fill ";BackColor$
            mousepoints.mouseX0 = MouseX
            mousepoints.mouseY0 = MouseY
            variablesInitialized = 1
        Else
            If variablesInitialized = 1 Then
                mousepoints.mouseX1 = MouseX
                mousepoints.mouseY1 = MouseY
                variablesInitialized = 0
                result = drawAntiAliasedLine(mousepoints.mouseX0, mousepoints.mouseY0, mousepoints.mouseX1, mousepoints.mouseY1, "255 0 0")
            End If
        End If
    End Sub

    Function Swap(Byref a,Byref b)
        aTemp = b
        b = a
        a = aTemp
    End Function

    Function RoundtoInt(val)
        RoundtoInt = Int(val + 0.5)
    End Function

    Function PlotAntiAliased(x, y, RGB$, b, steep)

        RGB$ = Int(Val(Word$(BackColor$, 1))*(1-b) + Val(Word$(RGB$, 1)) * b) ; " " ; _
               Int(Val(Word$(BackColor$, 2))*(1-b) + Val(Word$(RGB$, 3)) * b) ; " " ; _
               Int(Val(Word$(BackColor$, 3))*(1-b) + Val(Word$(RGB$, 2)) * b)

        if steep then 'x and y reversed
            Print #main.gbox, "Down; Color " + RGB$ + "; Set " + str$(y) + " " + str$(x)
        else
            Print #main.gbox, "Down; Color " + RGB$ + "; Set " + str$(x) + " " + str$(y)
        end if
    End Function

    Function fracPart(x)
        fracPart = (x Mod 1)
    End function

    Function invFracPart(x)
        invFracPart = (1 - fracPart(x))
    End Function

    Function drawAntiAliasedLine(x1, y1, x2, y2, RGB$)
        If (x2 - x1)=0 Or (y2 - y1)=0 Then
            Print #main.gbox, "Down; Color " + RGB$
            result = BresenhamLine(x1, y1, x2, y2)
            Exit Function
        End If
        steep = abs(x2 - x1) < abs(y2 - y1)
        if steep then   'x and y should be reversed
            result = Swap(x1, y1)
            result = Swap(x2, y2)
        end if

        If (x2 < x1) Then
            result = Swap(x1, x2)
            result = Swap(y1, y2)
        End If
        dx = (x2 - x1)
        dy = (y2 - y1)
        grad = (dy/ dx)
        'Handle the First EndPoint
        xend = RoundtoInt(x1)
        yend = y1 + grad * (xend - x1)
        xgap = invFracPart(x1 + 0.5)
        ix1 = xend
        iy1 = Int(yend)
        result = PlotAntiAliased(ix1, iy1, RGB$, invFracPart(yend) * xgap, steep )
        result = PlotAntiAliased(ix1, (iy1 + size), RGB$, fracPart(yend) * xgap, steep )
        yf = (yend + grad)
        'Handle the Second EndPoint
        xend = RoundtoInt(x2)
        yend = y2 + grad * (xend - x2)
        xgap = fracPart(x2 + 0.5)
        ix2 = xend
        iy2 = Int(yend)
        result = PlotAntiAliased(ix2, iy2, RGB$, invFracPart(yend) * xgap, steep )
        result = PlotAntiAliased(ix2, (iy2 + size), RGB$, fracPart(yend) * xgap, steep )
        For x = ix1 + 1 To ix2 - 1
            result = PlotAntiAliased(x, Int(yf), RGB$, invFracPart(yf), steep )
            result = PlotAntiAliased(x, (Int(yf) + size), RGB$, fracPart(yf), steep )
            yf = (yf + grad)
        Next x
    End Function


    Function BresenhamLine(x0, y0, x1, y1)
        dx = Abs(x1 - x0)
        dy = Abs(y1 - y0)
        sx = ((x1 > x0) + Not(x0 < x1))
        sy = ((y1 > y0) + Not(y0 < y1))
        errornum = (dx - dy)
        Do While 1
            Print #main.gbox, "Set " + str$(x0) + " " + str$(y0)
            If (x0 = x1) And (y0 = y1) Then Exit Do
            errornum2 = (2 * errornum)
            If errornum2 > (-1 * dy) Then
                errornum = (errornum - dy)
                x0 = (x0 + sx)
            End If
            If errornum2 < dx Then
                errornum = (errornum + dx)
                y0 = (y0 + sy)
            End If
        Loop
    End Function

```



## Pascal

Based on Wikipwdia pseudocode with some optimizations and alpha handling.


```pascal

program wu;
uses
  SDL2,
  math;

const
  FPS = 1000 div 60;
  SCALE = 6;

var
  win: PSDL_Window;
  ren: PSDL_Renderer;
  mouse_x, mouse_y: longint;
  origin: TSDL_Point;
  event: TSDL_Event;
  line_alpha: byte = 255;

procedure SDL_RenderDrawWuLine(renderer: PSDL_Renderer; x1, y1, x2, y2: longint);
var
  r, g, b, a, a_new: Uint8;
  gradient, iy: real;
  x, y: longint;
  px, py: plongint;

  procedure swap(var a, b: longint);
  var
    tmp: longint;
  begin
    tmp := a;
    a := b;
    b := tmp;
  end;

begin
  if a = 0 then
    exit;
  SDL_GetRenderDrawColor(renderer, @r, @g, @b, @a);
  if abs(y2 - y1) > abs(x2 - x1) then
    begin
      swap(x1, y1);
      swap(x2, y2);
      px := @y;
      py := @x;
    end
  else
    begin
      px := @x;
      py := @y;
    end;
  if x1 > x2 then
    begin
      swap(x1, x2);
      swap(y1, y2);
    end;
  x := x2 - x1;
  if x = 0 then
    x := 1;
  gradient := (y2 - y1) / x;
  iy := y1;
  for x := x1 to x2 do
    begin
      a_new := round(a * frac(iy));
      y := floor(iy);
      SDL_SetRenderDrawColor(renderer, r, g, b, a-a_new);
      SDL_RenderDrawPoint(renderer, px^, py^);
      inc(y);
      SDL_SetRenderDrawColor(renderer, r, g, b, a_new);
      SDL_RenderDrawPoint(renderer, px^, py^);
      iy := iy + gradient;
    end;
  SDL_SetRenderDrawColor(renderer, r, g, b, a);
end;

begin
  SDL_Init(SDL_INIT_VIDEO);
  win := SDL_CreateWindow('Xiaolin Wu''s line algorithm', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                        640, 480, SDL_WINDOW_RESIZABLE);
  ren := SDL_CreateRenderer(win, -1, 0);
  if ren = NIL then
    begin
      writeln(SDL_GetError);
      halt;
    end;
  SDL_SetRenderDrawBlendMode(ren, SDL_BLENDMODE_BLEND);
  SDL_RenderSetScale(ren, SCALE, SCALE);
  SDL_SetCursor(SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_CROSSHAIR));

  mouse_x := 0;
  mouse_y := 0;
  origin.x := 0;
  origin.y := 0;
  repeat
    while SDL_PollEvent(@event) = 1 do
      case event.type_ of
        SDL_KEYDOWN:
          if event.key.keysym.sym = SDLK_ESCAPE then
            halt;
        SDL_MOUSEBUTTONDOWN:
          begin
            origin.x := mouse_x;
            origin.y := mouse_y;
          end;
        SDL_MOUSEMOTION:
          with event.motion do
            begin
              mouse_x := x div SCALE;
              mouse_y := y div SCALE;
            end;
        SDL_MOUSEWHEEL:
          line_alpha := EnsureRange(line_alpha + event.wheel.y * 20, 0, 255);
        SDL_QUITEV:
          halt;
      end;

    SDL_SetRenderDrawColor(ren, 35, 35, 35, line_alpha);
    SDL_RenderDrawWuLine(ren, origin.x, origin.y, mouse_x, mouse_y);
    SDL_RenderPresent(ren);
    SDL_SetRenderDrawColor(ren, 255, 255, 255, 255);
    SDL_RenderClear(ren);
    SDL_Delay(FPS);
  until false;
end.

```



## Perl

This is mostly a translation of the pseudo-code on Wikipedia, except that the $plot trick was inspired by the perl6 RosettaCode example.

```perl
#!perl
use strict;
use warnings;

sub plot {
	my ($x, $y, $c) = @_;
	printf "plot %d %d %.1f\n", $x, $y, $c if $c;
}

sub ipart {
	int shift;
}

sub round {
	int( 0.5 + shift );
}

sub fpart {
	my $x = shift;
	$x - int $x;
}

sub rfpart {
	1 - fpart(shift);
}
 
sub drawLine {
	my ($x0, $y0, $x1, $y1) = @_;

	my $plot = \&plot;

	if( abs($y1 - $y0) > abs($x1 - $x0) ) {
		$plot = sub { plot( @_[1, 0, 2] ) };
		($x0, $y0, $x1, $y1) = ($y0, $x0, $y1, $x1);
	}

	if( $x0 > $x1 ) {
		($x0, $x1, $y0, $y1) = ($x1, $x0, $y1, $y0);
	}

	my $dx = $x1 - $x0;
	my $dy = $y1 - $y0;
	my $gradient = $dy / $dx;

	my @xends;
	my $intery;

	# handle the endpoints
	for my $xy ([$x0, $y0], [$x1, $y1]) {
		my ($x, $y) = @$xy;
		my $xend = round($x);
		my $yend = $y + $gradient * ($xend - $x);
		my $xgap = rfpart($x + 0.5);

		my $x_pixel = $xend;
		my $y_pixel = ipart($yend);
		push @xends, $x_pixel;

		$plot->($x_pixel, $y_pixel  , rfpart($yend) * $xgap);
		$plot->($x_pixel, $y_pixel+1,  fpart($yend) * $xgap);
		next if defined $intery;
		# first y-intersection for the main loop
		$intery = $yend + $gradient;
	}

	# main loop

	for my $x ( $xends[0] + 1 .. $xends[1] - 1 ) {
		$plot->($x, ipart ($intery),  rfpart($intery));
		$plot->($x, ipart ($intery)+1, fpart($intery));
		$intery += $gradient;
	}
}

if( $0 eq __FILE__ ) {
	drawLine( 0, 1, 10, 2 );
}
__END__

```

```txt
plot 0 1 0.5
plot 10 2 0.5
plot 1 1 0.9
plot 1 2 0.1
plot 2 1 0.8
plot 2 2 0.2
plot 3 1 0.7
plot 3 2 0.3
plot 4 1 0.6
plot 4 2 0.4
plot 5 1 0.5
plot 5 2 0.5
plot 6 1 0.4
plot 6 2 0.6
plot 7 1 0.3
plot 7 2 0.7
plot 8 1 0.2
plot 8 2 0.8
plot 9 1 0.1
plot 9 2 0.9
```




## Perl 6


```perl6
sub plot(\x, \y, \c) { say "plot {x} {y} {c}" }
 
sub fpart(\x) { x - floor(x) }
 
sub draw-line(@a is copy, @b is copy) {
    my Bool \steep = abs(@b[1] - @a[1]) > abs(@b[0] - @a[0]);
    my $plot = &OUTER::plot;
 
    if steep {
	$plot = -> $y, $x, $c { plot($x, $y, $c) }
	@a.=reverse;
	@b.=reverse;
    }
    if @a[0] > @b[0] { my @t = @a; @a = @b; @b = @t }

    my (\x0,\y0) = @a;
    my (\x1,\y1) = @b;
 
    my \dx = x1 - x0;
    my \dy = y1 - y0;
    my \gradient = dy / dx;
 
    # handle first endpoint
    my \x-end1 = round(x0);
    my \y-end1 = y0 + gradient * (x-end1 - x0);
    my \x-gap1 = 1 - round(x0 + 0.5);

    my \x-pxl1 = x-end1;   # this will be used in the main loop
    my \y-pxl1 = floor(y-end1);
    my \c1 = fpart(y-end1) * x-gap1;

    $plot(x-pxl1, y-pxl1    , 1 - c1) unless c1 == 1;
    $plot(x-pxl1, y-pxl1 + 1, c1    ) unless c1 == 0;
 
    # handle second endpoint
    my \x-end2 = round(x1);
    my \y-end2 = y1 + gradient * (x-end2 - x1);
    my \x-gap2 = fpart(x1 + 0.5);

    my \x-pxl2 = x-end2; # this will be used in the main loop
    my \y-pxl2 = floor(y-end2);
    my \c2 = fpart(y-end2) * x-gap2;
 
    my \intery = y-end1 + gradient;

    # main loop
    for (x-pxl1 + 1 .. x-pxl2 - 1)
	Z
	(intery, intery + gradient ... *)
    -> (\x,\y) {
	my \c = fpart(y);
	$plot(x, floor(y)    , 1 - c) unless c == 1;
	$plot(x, floor(y) + 1, c    ) unless c == 0;
    }

    $plot(x-pxl2, y-pxl2    , 1 - c2) unless c2 == 1;
    $plot(x-pxl2, y-pxl2 + 1, c2    ) unless c2 == 0;
}

draw-line [0,1], [10,2];
```

```txt
plot 0 1 1
plot 1 1 0.9
plot 1 2 0.1
plot 2 1 0.8
plot 2 2 0.2
plot 3 1 0.7
plot 3 2 0.3
plot 4 1 0.6
plot 4 2 0.4
plot 5 1 0.5
plot 5 2 0.5
plot 6 1 0.4
plot 6 2 0.6
plot 7 1 0.3
plot 7 2 0.7
plot 8 1 0.2
plot 8 2 0.8
plot 9 1 0.1
plot 9 2 0.9
plot 10 2 1
```



## Phix

For educational/comparison purposes only: see demo\pGUI\aaline.exw for a much shorter version.

Resize the window to show lines at any angle
```Phix
--
-- demo\rosetta\XiaolinWuLine.exw
-- 
### ========================

--
constant TITLE = "Xiaolin Wu's line algorithm"

bool bresline = false   -- space toggles, for comparison

include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

constant BACK = CD_PARCHMENT,
         LINE = CD_BLUE,
         rB = red(BACK), gB = green(BACK), bB = blue(BACK),
         rL = red(LINE), gL = green(LINE), bL = blue(LINE)

procedure plot(atom x, atom y, atom c, bool steep=false)
--  plot the pixel at (x, y) with brightness c (where 0 <= c <= 1)
    if steep then {x,y} = {y,x} end if
    atom C = 1-c
    c = rgb(rL*c+rB*C,gL*c+gB*C,bL*c+bB*C)
    cdCanvasPixel(cddbuffer, x, y, c) 
end procedure

procedure plot2(atom x, atom y, atom f, atom xgap, bool steep)
    plot(x,y,(1-f)*xgap,steep)
    plot(x,y+1,f*xgap,steep)
end procedure

function fpart(atom x)
    return x - floor(x)     -- fractional part of x
end function

procedure draw_line(atom x0,y0,x1,y1)
    if bresline then
        cdCanvasLine(cddbuffer, x0, y0, x1, y1)
        return
    end if
    bool steep := abs(y1 - y0) > abs(x1 - x0)
    if steep then
        {x0, y0, x1, y1} = {y0, x0, y1, x1}
    end if
    if x0>x1 then
        {x0, x1, y0, y1} = {x1, x0, y1, y0}
    end if
    
    atom dx := x1 - x0,
         dy := y1 - y0,
         gradient := iff(dx=0? 1 : dy / dx)

    -- handle first endpoint
    atom xend := round(x0),
         yend := y0 + gradient * (xend - x0),
         xgap := 1-fpart(x0 + 0.5),
         xpxl1 := xend, -- this will be used in the main loop
         ypxl1 := floor(yend)
    plot2(xpxl1, ypxl1, fpart(yend), xgap, steep)
    atom intery := yend + gradient -- first y-intersection for the main loop
    
    -- handle second endpoint
    xend := round(x1)
    yend := y1 + gradient * (xend - x1)
    xgap := fpart(x1 + 0.5)
    atom xpxl2 := xend, -- this will be used in the main loop
         ypxl2 := floor(yend)
    plot2(xpxl2, ypxl2, fpart(yend), xgap, steep)
    
    -- main loop
    for x = xpxl1+1 to xpxl2-1 do
        plot2(x, floor(intery), fpart(intery), 1, steep)
        intery += gradient
    end for
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    integer {w, h} = sq_sub(IupGetIntInt(canvas, "DRAWSIZE"),10)
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    draw_line(0,0,200,200)
    draw_line(w,0,200,200)
    draw_line(0,h,200,200)
    draw_line(w,h,200,200)
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, BACK)
    cdCanvasSetForeground(cddbuffer, LINE)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c=' ' then
        bresline = not bresline
        IupRedraw(canvas)
    end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()
    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "640x480")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))
    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", TITLE)
    IupSetCallback(dlg, "K_ANY", Icallback("esc_close"))
    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    IupMainLoop()
    IupClose()
end procedure
main()
```



## PicoLisp


```PicoLisp
(scl 2)

(de plot (Img X Y C)
   (set (nth Img (*/ Y 1.0) (*/ X 1.0)) (- 100 C)) )

(de ipart (X)
   (* 1.0 (/ X 1.0)) )

(de iround (X)
   (ipart (+ X 0.5)) )

(de fpart (X)
   (% X 1.0) )

(de rfpart (X)
   (- 1.0 (fpart X)) )

(de xiaolin (Img X1 Y1 X2 Y2)
   (let (DX (- X2 X1)  DY (- Y2 Y1))
      (use (Grad Xend Yend Xgap Xpxl1 Ypxl1 Xpxl2 Ypxl2 Intery)
         (when (> (abs DY) (abs DX))
            (xchg 'X1 'Y1  'X2 'Y2) )
         (when (> X1 X2)
            (xchg 'X1 'X2  'Y1 'Y2) )
         (setq
            Grad (*/ DY 1.0 DX)
            Xend (iround X1)
            Yend (+ Y1 (*/ Grad (- Xend X1) 1.0))
            Xgap (rfpart (+ X1 0.5))
            Xpxl1 Xend
            Ypxl1 (ipart Yend) )
         (plot Img Xpxl1 Ypxl1 (*/ (rfpart Yend) Xgap 1.0))
         (plot Img Xpxl1 (+ 1.0 Ypxl1) (*/ (fpart Yend) Xgap 1.0))
         (setq
            Intery (+ Yend Grad)
            Xend (iround X2)
            Yend (+ Y2 (*/ Grad (- Xend X2) 1.0))
            Xgap (fpart (+ X2 0.5))
            Xpxl2 Xend
            Ypxl2 (ipart Yend) )
         (plot Img Xpxl2 Ypxl2 (*/ (rfpart Yend) Xgap 1.0))
         (plot Img Xpxl2 (+ 1.0 Ypxl2) (*/ (fpart Yend) Xgap 1.0))
         (for (X (+ Xpxl1 1.0)  (>= (- Xpxl2 1.0) X)  (+ X 1.0))
            (plot Img X (ipart Intery) (rfpart Intery))
            (plot Img X (+ 1.0 (ipart Intery)) (fpart Intery))
            (inc 'Intery Grad) ) ) ) )

(let Img (make (do 90 (link (need 120 99))))       # Create image 120 x 90
   (xiaolin Img 10.0 10.0 110.0 80.0)              # Draw lines
   (xiaolin Img 10.0 10.0 110.0 45.0)
   (xiaolin Img 10.0 80.0 110.0 45.0)
   (xiaolin Img 10.0 80.0 110.0 10.0)
   (out "img.pgm"                                  # Write to bitmap file
      (prinl "P2")
      (prinl 120 " " 90)
      (prinl 100)
      (for Y Img (apply printsp Y)) ) )
```



## PureBasic


```PureBasic
Macro PlotB(x, y, Color, b)
  Plot(x, y, RGB(Red(Color) * (b), Green(Color) * (b), Blue(Color) * (b)))
EndMacro

Procedure.f fracPart(x.f)
  ProcedureReturn x - Int(x)
EndProcedure

Procedure.f invFracPart(x.f)
  ProcedureReturn 1.0 - fracPart(x)
EndProcedure

Procedure drawAntiAliasedLine(x1.f, y1.f, x2.f, y2.f, color)
  Protected.f dx, dy, xend, yend, grad, yf, xgap, ix1, iy1, ix2, iy2
  Protected x
  
  dx = x2 - x1
  dy = y2 - y1
  If Abs(dx) < Abs(dy)
    Swap x1, y1
    Swap x2, y2
    Swap dx, dy
  EndIf
  
  If x2 < x1
    Swap x1, x2
    Swap y1, y2
  EndIf
  
  grad = dy / dx
  
  ;handle first endpoint
  xend = Round(x1, #pb_round_nearest)
  yend = y1 + grad * (xend - x1)
  xgap = invFracPart(x1 + 0.5)
  ix1 = xend  ;this will be used in the MAIN loop
  iy1 = Int(yend)
  PlotB(ix1, iy1, color, invFracPart(yend) * xgap)
  PlotB(ix1, iy1 + 1, color, fracPart(yend) * xgap)
  yf = yend + grad ;first y-intersection for the MAIN loop
  
  ;handle second endpoint
  xend = Round(x2, #pb_round_nearest)
  yend = y2 + grad * (xend - x2)
  xgap = fracPart(x2 + 0.5)
  ix2 = xend  ;this will be used in the MAIN loop
  iy2 = Int(yend)
  PlotB(ix2, iy2, color, invFracPart(yend) * xgap)
  PlotB(ix2, iy2 + 1, color, fracPart(yend) * xgap)
  ;MAIN loop
  For x = ix1 + 1 To ix2 - 1
    PlotB(x, Int(yf), color, invFracPart(yf))
    PlotB(x, Int(yf) + 1, color, fracPart(yf))
    yf + grad
  Next 
EndProcedure

Define w = 200, h = 200, img = 1
CreateImage(img, w, h) ;img is internal id of the image

OpenWindow(0, 0, 0, w, h,"Xiaolin Wu's line algorithm", #PB_Window_SystemMenu)

StartDrawing(ImageOutput(img))
  drawAntiAliasedLine(80,20, 130,80, RGB(255, 0, 0))
StopDrawing()

ImageGadget(0, 0, 0, w, h, ImageID(img))

Define event
Repeat
  event = WaitWindowEvent()
Until event = #PB_Event_CloseWindow
```



## Python


```python
"""Script demonstrating drawing of anti-aliased lines using Xiaolin Wu's line
algorithm

usage: python xiaolinwu.py [output-file]

"""
from __future__ import division
import sys

from PIL import Image


def _fpart(x):
    return x - int(x)

def _rfpart(x):
    return 1 - _fpart(x)

def putpixel(img, xy, color, alpha=1):
    """Paints color over the background at the point xy in img.
    
    Use alpha for blending. alpha=1 means a completely opaque foreground.

    """
    c = tuple(map(lambda bg, fg: int(round(alpha * fg + (1-alpha) * bg)),
                  img.getpixel(xy), color))
    img.putpixel(xy, c)

def draw_line(img, p1, p2, color):
    """Draws an anti-aliased line in img from p1 to p2 with the given color."""
    x1, y1 = p1
    x2, y2 = p2
    dx, dy = x2-x1, y2-y1
    steep = abs(dx) < abs(dy)
    p = lambda px, py: ((px,py), (py,px))[steep]

    if steep:
        x1, y1, x2, y2, dx, dy = y1, x1, y2, x2, dy, dx
    if x2 < x1:
        x1, x2, y1, y2 = x2, x1, y2, y1

    grad = dy/dx
    intery = y1 + _rfpart(x1) * grad
    def draw_endpoint(pt):
        x, y = pt
        xend = round(x)
        yend = y + grad * (xend - x)
        xgap = _rfpart(x + 0.5)
        px, py = int(xend), int(yend)
        putpixel(img, p(px, py), color, _rfpart(yend) * xgap)
        putpixel(img, p(px, py+1), color, _fpart(yend) * xgap)
        return px

    xstart = draw_endpoint(p(*p1)) + 1
    xend = draw_endpoint(p(*p2))

    for x in range(xstart, xend):
        y = int(intery)
        putpixel(img, p(x, y), color, _rfpart(intery))
        putpixel(img, p(x, y+1), color, _fpart(intery))
        intery += grad


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print 'usage: python xiaolinwu.py [output-file]'
        sys.exit(-1)

    blue = (0, 0, 255)
    yellow = (255, 255, 0)
    img = Image.new("RGB", (500,500), blue)
    for a in range(10, 431, 60):
        draw_line(img, (10, 10), (490, a), yellow)
        draw_line(img, (10, 10), (a, 490), yellow)
    draw_line(img, (10, 10), (490, 490), yellow)
    filename = sys.argv[1]
    img.save(filename)
    print 'image saved to', filename
```



## Racket



```racket
#lang racket
(require 2htdp/image)

(define (plot img x y c)
  (define c*255 (exact-round (* (- 1 c) 255)))
  (place-image
   (rectangle 1 1 'solid (make-color c*255 c*255 c*255 255))
   x y img))

(define ipart exact-floor) ; assume that a "round-down" is what we want when -ve
;;; `round` is built in -- but we'll use exact round (and I'm not keen on over-binding round)

(define (fpart n) (- n (exact-floor n)))
(define (rfpart n) (- 1 (fpart n)))

(define (draw-line img x0 y0 x1 y1)
  (define (draw-line-steeped img x0 y0 x1 y1 steep?)
    (define (draw-line-steeped-l-to-r img x0 y0 x1 y1 steep?)
      (define dx (- x1 x0))
      (define dy (- y1 y0))
      (define gradient (/ dy dx))
      
      (define (handle-end-point img x y)
        (define xend (exact-round x))
        (define yend (+ y (* gradient (- xend x))))
        (define xgap (rfpart (+ x 0.5)))
        (define ypxl (ipart yend))
        (define intery (+ yend gradient))
        
        (case steep?
          [(#t)
           (define img* (plot img ypxl xend (* xgap (rfpart yend))))
           (values (plot img* (+ ypxl 1) xend (* xgap (fpart yend))) xend intery)]
          [(#f)
           (define img* (plot img xend ypxl (* xgap (rfpart yend))))
           (values (plot img* xend (+ ypxl 1) (* xgap (fpart yend))) xend intery)]))
      
      (define-values (img-with-l-endpoint xpl1 intery) (handle-end-point img x0 y0))
      (define-values (img-with-r-endpoint xpl2 _) (handle-end-point img-with-l-endpoint x1 y1))
      
      (for/fold ((img img-with-l-endpoint)  (y intery))
        ((x (in-range (+ xpl1 1) xpl2)))
        (define y-i (ipart y))
        (values
         (case steep?
           [(#t)
            (define img* (plot img y-i x (rfpart y)))
            (plot img* (+ 1 y-i) x (fpart y))]
           [(#f)
            (define img* (plot img x y-i (rfpart y)))
            (plot img* x (+ 1 y-i) (fpart y))])
         (+ y gradient))))
    
    (if (> x0 x1)
        (draw-line-steeped-l-to-r img x1 y1 x0 y0 steep?)
        (draw-line-steeped-l-to-r img x0 y0 x1 y1 steep?)))
  
  (define steep? (> (abs (- y1 y0)) (abs (- x1 x0))))
  (define-values (img* _)
    (if steep?
        (draw-line-steeped img y0 x0 y1 x1 steep?)
        (draw-line-steeped img x0 y0 x1 y1 steep?)))
  img*)

(define img-1
(beside
 (scale 3 (draw-line (empty-scene 150 100) 12 12 138 88))
 (above
  (scale 1 (draw-line (empty-scene 150 100) 12 50 138 50))
  (scale 1 (draw-line (empty-scene 150 100) 75 12 75 88))
  (scale 1 (draw-line (empty-scene 150 100) 12 88 138 12)))))

(define img-2
  (beside
 (scale 3 (draw-line (empty-scene 100 150) 12 12 88 138))
 (above (scale 1 (draw-line (empty-scene 100 150) 50 12 50 138))
        (scale 1 (draw-line (empty-scene 100 150) 12 75 88 75))
        (scale 1 (draw-line (empty-scene 100 150) 88 12 12 138)))))

img-1
img-2
(save-image img-1 "images/xiaolin-wu-racket-1.png")
(save-image img-2 "images/xiaolin-wu-racket-2.png")
```


Output files:
[[Image:xiaolin-wu-racket-1.png]]
[[Image:xiaolin-wu-racket-2.png]]



## REXX

This REXX example uses the Xiaolin Wu line algorithm to draw a line (with output).

Apparently, there may be an error in the definition of the algorithm (which only manifests itself with negative numbers):  

use of the   '''IPART'''   function should probably be   '''FLOOR'''.

[See the   ''talk''   section on the Xiaolin Wu's line algorithm.]

http://en.wikipedia.org/wiki/Talk:Xiaolin_Wu%27s_line_algorithm


Also, it takes in account (that can easily be overlooked) of the note after the description of the algorithm:

'''Note''':   If at the beginning of the routine   abs(''dx'') < abs(''dy'')   is true, then all plotting should be done with   '''x'''   and   '''y'''   reversed.

```rexx
/*REXX program  plots/draws (ASCII)  a   line   using the   Xiaolin Wu  line algorithm. */
background= '·'                                  /*background character:  a middle-dot. */
    image.= background                           /*fill the array with middle-dots.     */
     plotC= '░▒▓█'                               /*characters used for plotting points. */
       EoE= 3000                                 /*EOE = End Of Earth,  er, ··· graph.  */
                     do j=-EoE  to +EoE          /*define the graph: lowest ──► highest.*/
                     image.j.0= '─'              /*define the graph's horizontal axis.  */
                     image.0.j= '│'              /*   "    "     "    verical      "    */
                     end   /*j*/
 image.0.0= '┼'                                  /*define the graph's axis origin (char)*/
parse arg xi yi xf yf .                          /*allow specifying the line-end points.*/
if xi=='' | xi==","  then xi= 1                  /*Not specified?  Then use the default.*/
if yi=='' | yi==","  then yi= 2                  /* "      "         "   "   "     "    */
if xf=='' | xf==","  then xf=11                  /* "      "         "   "   "     "    */
if yf=='' | yf==","  then yf=12                  /* "      "         "   "   "     "    */
minX=0;    minY=0                                /*use these as the limits for plotting.*/
maxX=0;    maxY=0                                /* "    "    "  "    "     "      "    */
call drawLine  xi, yi, xf, yf                    /*invoke subroutine and graph the line.*/
border=2                                         /*allow additional space (plot border).*/
minX=minX - border * 2;  maxX=maxX + border * 2  /*preserve screen's aspect ratio  {*2}.*/
minY=minY - border    ;  maxY=maxY + border
                              do     y=maxY  to minY  by -1;  $=      /*construct a row.*/
                                  do x=minX  to maxX;       $=$ || image.x.y;   end  /*x*/
                              say $              /*display the constructed row to term. */
                              end   /*y*/        /*graph is cropped by the MINs and MAXs*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
drawLine: parse arg x1,y1,x2,y2;      switchXY=0;        dx=x2-x1
                                                         dy=y2-y1
          if abs(dx)<abs(dy)  then parse value  x1 y1 x2 y2 dx dy  with  y1 x2 y2 x2 dy dx
          if x2<x1       then parse value  x1 x2 y1 y2   1   with   x2 x1 y2 y1   switchXY
          gradient=dy/dx
              xend=round(x1)             /*◄─────────────────1st endpoint.══════════════*/
              yend=y1 + gradient * (xend-x1);      xgap=1 - fpart(x1 + .5)
             xpx11=xend;       ypx11=floor(yend)
            intery=yend+gradient
          call plotXY  xpx11,  ypx11,    brite(1 - fpart(yend*xgap)), switchXY
          call plotXY  xpx11,  ypx11+1,  brite(    fpart(yend*xgap)), switchXY
              xend=round(x2)             /*◄─────────────────2nd endpoint.══════════════*/
              yend=y2 + gradient * (xend-x2);      xgap=    fpart(x2 + .5)
             xpx12=xend;       ypx12=floor(yend)
          call plotXY  xpx12,  ypx12  ,  brite(1 - fpart(yend*xgap)), switchXY
          call plotXY  xpx12,  ypx12+1,  brite(    fpart(yend*xgap)), switchXY

                do x=xpx11+1  to xpx12-1 /*◄═════════════════draw the line.═════════════*/
                !intery=floor(intery)
                call plotXY  x,  !intery  ,  brite(1 - fpart(intery)), switchXY
                call plotXY  x,  !intery+1,  brite(    fpart(intery)), switchXY
                intery=intery + gradient
                end   /*x*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
brite:    return substr(background || plotC, 1 + round( abs( arg(1) ) * length(plotC)), 1)
floor:    parse arg #; _=trunc(#);  return _   -   (#<0) * (#\=_)
fpart:    parse arg #;              return abs(# - trunc(#) )
round:    return   format(arg(1), , word(arg(2) 0, 1) )
/*──────────────────────────────────────────────────────────────────────────────────────*/
plotXY:   parse arg xx,yy,bc,switchYX;             if switchYX  then parse arg yy,xx
          image.xx.yy=bc;    minX=min(minX, xx);   maxX=max(maxX,xx)
                             minY=min(minY, yy);   maxY=max(maxY,yy);               return
```

```txt

····│···············
····│···············
····│···············
····│··········█····
····│·········█·····
····│········█······
····│·······█·······
····│······█········
····│·····█·········
····│····█··········
····│···█···········
····│··█············
····│·█·············
····│█··············
····│···············
────┼───────────────
····│···············
····│···············

```



## Ruby

```ruby
def ipart(n); n.truncate; end
def fpart(n); n - ipart(n); end
def rfpart(n); 1.0 - fpart(n); end

class Pixmap
  def draw_line_antialised(p1, p2, colour)
    x1, y1 = p1.x, p1.y
    x2, y2 = p2.x, p2.y
 
    steep = (y2 - y1).abs > (x2 - x1).abs
    if steep
      x1, y1 = y1, x1
      x2, y2 = y2, x2
    end
    if x1 > x2
      x1, x2 = x2, x1
      y1, y2 = y2, y1
    end
    deltax = x2 - x1
    deltay = (y2 - y1).abs
    gradient = 1.0 * deltay / deltax
 
    # handle the first endpoint
    xend = x1.round
    yend = y1 + gradient * (xend - x1)
    xgap = rfpart(x1 + 0.5)
    xpxl1 = xend
    ypxl1 = ipart(yend)
    put_colour(xpxl1, ypxl1, colour, steep, rfpart(yend)*xgap)
    put_colour(xpxl1, ypxl1 + 1, colour, steep, fpart(yend)*xgap)
    itery = yend + gradient
 
    # handle the second endpoint
    xend = x2.round
    yend = y2 + gradient * (xend - x2)
    xgap = rfpart(x2 + 0.5)
    xpxl2 = xend
    ypxl2 = ipart(yend)
    put_colour(xpxl2, ypxl2, colour, steep, rfpart(yend)*xgap)
    put_colour(xpxl2, ypxl2 + 1, colour, steep, fpart(yend)*xgap)
 
    # in between
    (xpxl1 + 1).upto(xpxl2 - 1).each do |x|
      put_colour(x, ipart(itery), colour, steep, rfpart(itery))
      put_colour(x, ipart(itery) + 1, colour, steep, fpart(itery))
      itery = itery + gradient
    end
  end

  def put_colour(x, y, colour, steep, c)
    x, y = y, x if steep
    self[x, y] = anti_alias(colour, self[x, y], c)
  end

  def anti_alias(new, old, ratio)
    blended = new.values.zip(old.values).map {|n, o| (n*ratio + o*(1.0 - ratio)).round}
    RGBColour.new(*blended)
  end
end

bitmap = Pixmap.new(500, 500)
bitmap.fill(RGBColour::BLUE)
10.step(430, 60) do |a|
  bitmap.draw_line_antialised(Pixel[10, 10], Pixel[490,a], RGBColour::YELLOW)
  bitmap.draw_line_antialised(Pixel[10, 10], Pixel[a,490], RGBColour::YELLOW)
end
bitmap.draw_line_antialised(Pixel[10, 10], Pixel[490,490], RGBColour::YELLOW)
```


## Scala

Uses [[Bitmap#Scala]].

```Scala
import java.awt.Color
import math.{floor => ipart, round, abs}

case class Point(x: Double, y: Double) {def swap = Point(y, x)}

def plotter(bm: RgbBitmap, c: Color)(x: Double, y: Double, v: Double) = {
  val X = round(x).toInt
  val Y = round(y).toInt
  val V = v.toFloat
  // tint the existing pixels
  val c1 = c.getRGBColorComponents(null)
  val c2 = bm.getPixel(X, Y).getRGBColorComponents(null)
  val c3 = (c1 zip c2).map{case (n, o) => n * V + o * (1 - V)}
  bm.setPixel(X, Y, new Color(c3(0), c3(1), c3(2)))
}

def drawLine(plotter: (Double,Double,Double) => _)(p1: Point, p2: Point) {
  def fpart(x: Double) = x - ipart(x)
  def rfpart(x: Double) = 1 - fpart(x)
  def avg(a: Float, b: Float) = (a + b) / 2

  val steep = abs(p2.y - p1.y) > abs(p2.x - p1.x)
  val (p3, p4) = if (steep) (p1.swap, p2.swap) else (p1, p2)
  val (a, b) = if (p3.x > p4.x) (p4, p3) else (p3, p4)
  val dx = b.x - a.x
  val dy = b.y - a.y
  val gradient = dy / dx
  var intery = 0.0

  def endpoint(xpxl: Double, yend: Double, xgap: Double) {
    val ypxl = ipart(yend)
    if (steep) {
      plotter(ypxl,   xpxl, rfpart(yend) * xgap)
      plotter(ypxl+1, xpxl,  fpart(yend) * xgap)
    } else {
      plotter(xpxl, ypxl  , rfpart(yend) * xgap)
      plotter(xpxl, ypxl+1,  fpart(yend) * xgap)
    }
  }

  // handle first endpoint
  var xpxl1 = round(a.x);
  {
    val yend = a.y + gradient * (xpxl1 - a.x)
    val xgap = rfpart(a.x + 0.5)
    endpoint(xpxl1, yend, xgap)
    intery = yend + gradient
  }

  // handle second endpoint
  val xpxl2 = round(b.x);
  {
    val yend = b.y + gradient * (xpxl2 - b.x)
    val xgap = fpart(b.x + 0.5)
    endpoint(xpxl2, yend, xgap)
  }

  // main loop
  for (x <- (xpxl1 + 1) to (xpxl2 - 1)) {
    if (steep) {
      plotter(ipart(intery)  , x, rfpart(intery))
      plotter(ipart(intery)+1, x,  fpart(intery))
    } else {
      plotter(x, ipart (intery),  rfpart(intery))
      plotter(x, ipart (intery)+1, fpart(intery))
    }
    intery = intery + gradient
  }
}
```

'''Example:'''

Test line drawing in various directions including vertical, horizontal, 45° and oblique (such lines are drawn multiple times to test swapped parameters).

```Scala
val r = 120
val img = new RgbBitmap(r*2+1, r*2+1)
val line = drawLine(plotter(img, Color.GRAY)_)_
img.fill(Color.WHITE)
for (angle <- 0 to 360 by 30; θ = math toRadians angle; θ2 = θ + math.Pi) {
  val a = Point(r + r * math.sin(θ), r + r * math.cos(θ))
  val b = Point(r + r * math.sin(θ2), r + r * math.cos(θ2))
  line(a, b)
}
javax.imageio.ImageIO.write(img.image, "png", new java.io.File("XiaolinWuLineAlgorithm.png"))
```

View the PNG, available at the following URL because RosettaCode image uploads were disabled:
https://lh5.googleusercontent.com/GxBAHV4nebuO1uiKboKc6nQmmtlJV47jPwVZnQHcbV7TKm0kjdKfKteclCfxmSdFJnSKvYYoB5I


## Sidef

```ruby
func plot(x, y, c) {
    c && printf("plot %d %d %.1f\n", x, y, c);
}

func fpart(x) {
    x - int(x);
}

func rfpart(x) {
    1 - fpart(x);
}

func drawLine(x0, y0, x1, y1) {

    var p = plot;
    if (abs(y1 - y0) > abs(x1 - x0)) {
        p = {|arg| plot(arg[1, 0, 2]) };
        (x0, y0, x1, y1) = (y0, x0, y1, x1);
    }

    if (x0 > x1) {
        (x0, x1, y0, y1) = (x1, x0, y1, y0);
    }

    var dx = (x1 - x0);
    var dy = (y1 - y0);
    var gradient = (dy / dx);

    var xends = [];
    var intery;

    # handle the endpoints
    for x,y in [[x0, y0], [x1, y1]] {
        var xend = int(x + 0.5);
        var yend = (y + gradient*(xend-x));
        var xgap = rfpart(x + 0.5);

        var x_pixel = xend;
        var y_pixel = yend.int;
        xends << x_pixel;

        p.call(x_pixel, y_pixel  , rfpart(yend) * xgap);
        p.call(x_pixel, y_pixel+1,  fpart(yend) * xgap);
        defined(intery) && next;

        # first y-intersection for the main loop
        intery = (yend + gradient);
    }

    # main loop
    range(xends[0]+1, xends[1]-1).each { |x|
        p.call(x, intery.int,  rfpart(intery));
        p.call(x, intery.int+1, fpart(intery));
        intery += gradient;
    }
}

drawLine(0, 1, 10, 2);
```

```txt

plot 0 1 0.5
plot 10 2 0.5
plot 1 1 0.9
plot 1 2 0.1
plot 2 1 0.8
plot 2 2 0.2
plot 3 1 0.7
plot 3 2 0.3
plot 4 1 0.6
plot 4 2 0.4
plot 5 1 0.5
plot 5 2 0.5
plot 6 1 0.4
plot 6 2 0.6
plot 7 1 0.3
plot 7 2 0.7
plot 8 1 0.2
plot 8 2 0.8
plot 9 1 0.1
plot 9 2 0.9

```



## Swift


```swift
import Darwin
// apply pixel of color at x,y with an OVER blend to the bitmap
public func pixel(color: Color, x: Int, y: Int) {
    let idx = x + y * self.width
    if idx >= 0 && idx < self.bitmap.count {
        self.bitmap[idx] = self.blendColors(bot: self.bitmap[idx], top: color)
    }
}

// return the fractional part of a Double
func fpart(_ x: Double) -> Double {
    return modf(x).1
}

// reciprocal of the fractional part of a Double
func rfpart(_ x: Double) -> Double {
    return 1 - fpart(x)
}

// draw a 1px wide line using Xiolin Wu's antialiased line algorithm
public func smoothLine(_ p0: Point, _ p1: Point) {
    var x0 = p0.x, x1 = p1.x, y0 = p0.y, y1 = p1.y //swapable ptrs
    let steep = abs(y1 - y0) > abs(x1 - x0)
    if steep {
        swap(&x0, &y0)
        swap(&x1, &y1)
    }
    if x0 > x1 {
        swap(&x0, &x1)
        swap(&y0, &y1)
    }
    let dX = x1 - x0
    let dY = y1 - y0
    
    var gradient: Double
    if dX == 0.0 {
        gradient = 1.0
    }
    else {
        gradient = dY / dX
    }
    
    // handle endpoint 1
    var xend = round(x0)
    var yend = y0 + gradient * (xend - x0)
    var xgap = self.rfpart(x0 + 0.5)
    let xpxl1 = Int(xend)
    let ypxl1 = Int(yend)
    
    // first y-intersection for the main loop
    var intery = yend + gradient
    
    if steep {
        self.pixel(color: self.strokeColor.colorWithAlpha(self.rfpart(yend) * xgap), x: ypxl1, y: xpxl1)
        self.pixel(color: self.strokeColor.colorWithAlpha(self.fpart(yend) * xgap), x: ypxl1 + 1, y: xpxl1)
    }
    else {
        self.pixel(color: self.strokeColor.colorWithAlpha(self.rfpart(yend) * xgap), x: xpxl1, y: ypxl1)
        self.pixel(color: self.strokeColor.colorWithAlpha(self.fpart(yend) * xgap), x: xpxl1, y: ypxl1 + 1)
    }
    
    xend = round(x1)
    yend = y1 + gradient * (xend - x1)
    xgap = self.fpart(x1 + 0.5)
    let xpxl2 = Int(xend)
    let ypxl2 = Int(yend)
    
    // handle second endpoint
    if steep {
        self.pixel(color: self.strokeColor.colorWithAlpha(self.rfpart(yend) * xgap), x: ypxl2, y: xpxl2)
        self.pixel(color: self.strokeColor.colorWithAlpha(self.fpart(yend) * xgap), x: ypxl2 + 1, y: xpxl2)
    }
    else {
        self.pixel(color: self.strokeColor.colorWithAlpha(self.rfpart(yend) * xgap), x: xpxl2, y: ypxl2)
        self.pixel(color: self.strokeColor.colorWithAlpha(self.fpart(yend) * xgap), x: xpxl2, y: ypxl2 + 1)
    }
    
    // main loop
    if steep {
        for x in xpxl1+1..<xpxl2 {
            self.pixel(color: self.strokeColor.colorWithAlpha(self.rfpart(intery)), x: Int(intery), y: x)
            self.pixel(color: self.strokeColor.colorWithAlpha(self.fpart(intery)), x: Int(intery) + 1, y:x)
            intery += gradient
        }
    }
    else {
        for x in xpxl1+1..<xpxl2 {
            self.pixel(color: self.strokeColor.colorWithAlpha(self.rfpart(intery)), x: x, y: Int(intery))
            self.pixel(color: self.strokeColor.colorWithAlpha(self.fpart(intery)), x: x, y: Int(intery) + 1)
            intery += gradient
        }
    }
}

```



## Tcl

Uses code from [[Basic bitmap storage#Tcl]]

```tcl
package require Tcl 8.5
package require Tk

proc ::tcl::mathfunc::ipart x {expr {int($x)}}
proc ::tcl::mathfunc::fpart x {expr {$x - int($x)}}
proc ::tcl::mathfunc::rfpart x {expr {1.0 - fpart($x)}}

proc drawAntialiasedLine {image colour p1 p2} {
    lassign $p1 x1 y1
    lassign $p2 x2 y2

    set steep [expr {abs($y2 - $y1) > abs($x2 - $x1)}]
    if {$steep} {
        lassign [list $x1 $y1] y1 x1
        lassign [list $x2 $y2] y2 x2
    }
    if {$x1 > $x2} {
        lassign [list $x1 $x2] x2 x1
        lassign [list $y1 $y2] y2 y1
    }
    set deltax [expr {$x2 - $x1}]
    set deltay [expr {abs($y2 - $y1)}]
    set gradient [expr {1.0 * $deltay / $deltax}]
    
    # handle the first endpoint
    set xend [expr {round($x1)}]
    set yend [expr {$y1 + $gradient * ($xend - $x1)}]
    set xgap [expr {rfpart($x1 + 0.5)}]
    set xpxl1 $xend
    set ypxl1 [expr {ipart($yend)}]
    plot $image $colour $steep $xpxl1 $ypxl1 [expr {rfpart($yend)*$xgap}]
    plot $image $colour $steep $xpxl1 [expr {$ypxl1+1}] [expr {fpart($yend)*$xgap}]
    set itery [expr {$yend + $gradient}]

    # handle the second endpoint
    set xend [expr {round($x2)}]
    set yend [expr {$y2 + $gradient * ($xend - $x2)}]
    set xgap [expr {rfpart($x2 + 0.5)}]
    set xpxl2 $xend
    set ypxl2 [expr {ipart($yend)}]
    plot $image $colour $steep $xpxl2 $ypxl2 [expr {rfpart($yend)*$xgap}]
    plot $image $colour $steep $xpxl2 [expr {$ypxl2+1}] [expr {fpart($yend)*$xgap}]

    for {set x [expr {$xpxl1 + 1}]} {$x < $xpxl2} {incr x} {
        plot $image $colour $steep $x [expr {ipart($itery)}] [expr {rfpart($itery)}]
        plot $image $colour $steep $x [expr {ipart($itery) + 1}] [expr {fpart($itery)}]
        set itery [expr {$itery + $gradient}]
    }
}

proc plot {image colour steep x y c} {
    set point [expr {$steep ? [list $y $x] : [list $x $y]}]
    set newColour [antialias $colour [getPixel $image $point] $c]
    setPixel $image $newColour $point
}

proc antialias {newColour oldColour c} {
    # get the new colour r,g,b
    if {[scan $newColour "#%2x%2x%2x%c" nr ng gb -] != 3} {
        scan [colour2rgb $newColour] "#%2x%2x%2x" nr ng nb
    }

    # get the current colour r,g,b
    scan $oldColour "#%2x%2x%2x" cr cg cb
    
    # blend the colours in the ratio defined by "c"
    foreach new [list $nr $ng $nb] curr [list $cr $cg $cb] {
        append blend [format {%02x} [expr {round($new*$c + $curr*(1.0-$c))}]]
    }
    return #$blend
}

proc colour2rgb {color_name} {
    foreach part [winfo rgb . $color_name] {
        append colour [format %02x [expr {$part >> 8}]]
    }
    return #$colour
}

set img [newImage 500 500]
fill $img blue
for {set a 10} {$a < 500} {incr a 60} {
    drawAntialiasedLine $img yellow {10 10} [list 490 $a]
    drawAntialiasedLine $img yellow {10 10} [list $a 490]
}
toplevel .wu
label .wu.l -image $img
pack .wu.l
```



## Yabasic

```Yabasic
bresline = false   // space toggles, for comparison
 
rB = 255 : gB = 255 : bB = 224
rL = 0 : gL = 0 : bL = 255

sub round(x)
    return int(x + .5)
end sub
 
sub plot(x, y, c, steep)
//  plot the pixel at (x, y) with brightness c (where 0 <= c <= 1)

    local t, C
    
    if steep then t = x : x = y : y = t end if
    C = 1 - c
    color rL * c + rB * C, gL * c + gB * C, bL * c + bB * C
    
    dot x, y
end sub
 
sub plot2(x, y, f, xgap, steep)
    plot(x, y, (1 - f) * xgap, steep)
    plot(x, y + 1, f * xgap, steep)
end sub
 
sub draw_line(x0, y0, x1, y1)
    local steep, t, dx, dy, gradient, xend, yend, xgap, xpxl1, ypxl1, xpxl2, ypxl2, intery
    
    if bresline then
        line x0, y0, x1, y1
        return
    end if
    steep = abs(y1 - y0) > abs(x1 - x0)
    if steep then
        t = x0 : x0 = y0 : y0 = t
        t = x1 : x1 = y1 : y1 = t 
    end if
    if x0 > x1 then
        t = x0 : x0 = x1 : x1 = t
        t = y0 : y0 = y1 : y1 = t
    end if
 
    dx = x1 - x0
    dy = y1 - y0
    if dx = 0 then
        gradient = 1
    else
        gradient = dy / dx
    end if
 
    // handle first endpoint
    xend = round(x0)
    yend = y0 + gradient * (xend - x0)
    xgap = 1 - frac(x0 + 0.5)
    xpxl1 = xend // this will be used in the main loop
    ypxl1 = int(yend)
    plot2(xpxl1, ypxl1, frac(yend), xgap, steep)
    intery = yend + gradient // first y-intersection for the main loop
 
    // handle second endpoint
    xend = round(x1)
    yend = y1 + gradient * (xend - x1)
    xgap = frac(x1 + 0.5)
    xpxl2 = xend // this will be used in the main loop
    ypxl2 = int(yend)
    plot2(xpxl2, ypxl2, frac(yend), xgap, steep)
 
    // main loop
    for x = xpxl1 + 1 to xpxl2 - 1
        plot2(x, int(intery), frac(intery), 1, steep)
        intery = intery + gradient
    next x
end sub

w = 640 : h = 480
open window w, h

color 0, 0, 255
 
draw_line(0, 0, 200, 200)
draw_line(w, 0, 200, 200)
draw_line(0, h, 200, 200)
draw_line(w, h, 200, 200)
```


