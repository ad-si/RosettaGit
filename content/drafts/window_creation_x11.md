+++
title = "Window creation/X11"
description = ""
date = 2019-07-28T15:07:52Z
aliases = []
[extra]
id = 3109
[taxonomies]
categories = []
tags = []
+++

{{task|GUI}}{{requires|Graphics}}

;Task:
Create a simple '''X11''' application,   using an '''X11''' protocol library such as Xlib or XCB,   that draws a box and   "Hello World"   in a window. 

Implementations of this task should   ''avoid using a toolkit''   as much as possible.





## ALGOL 68

{{works with|ALGOL 68G|tested with release mk15-0.8b.fc9.i386}}

Using the X11 & plotutil libraries is not part of any of the original UNESCO/IFIP [[ALGOL 68]]
reports. As at December 2008 only [[ALGOL 68G]] comes with these built in.

```algol68
FILE window;
draw device (window, "X", "600x400");
open (window, "Hello, World!", stand draw channel);
  draw erase (window);
  draw move (window, 0.25, 0.5);
  draw colour (window, 1, 0, 0);
  draw text (window, "c", "c", "hello world");
  draw show (window);
  VOID (read char); 
close (window)
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program windows1.s        */

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
szTexte1:                .asciz "Hello world."
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
    mov r8,#400                 @ hauteur
    push {r8}
    mov r8,#600                 @ largeur 
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
    /* Write text in the window */
    /****************************/
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    mov r2,r8                   @ address graphic context
    mov r3,#50                  @ position x 
    sub sp,#4                   @ stack alignement
    mov r4,#LGTEXTE1  - 1       @ size string 
    push {r4}                   @ on the stack
    ldr r4,iAdrszTexte1            @ string address
    push {r4}
    mov r4,#100                 @ position y 
    push {r4}
    bl XDrawString
    add sp,sp,#16               @ stack alignement 3 push and 1 stack alignement
    cmp r0,#0                   @ error ?
    blt erreurX11
    /* write text 2 */
    mov r0,r6                   @ display address
    mov r1,r9                   @ window address
    mov r2,r8                   @ address graphic context
    mov r3,#10                  @ position x 
    sub sp,#4                   @ stack alignement
    mov r4,#LGTEXTE2  - 1       @ size string 
    push {r4}                   @ on the stack
    ldr r4,iAdrszTexte2            @ string address
    push {r4}
    mov r4,#350                 @ position y 
    push {r4}
    bl XDrawString
    add sp,sp,#16               @ stack alignement 3 push and 1 stack alignement
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
iAdrszTexte1:        .int szTexte1
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
    pop {r0,r1,r2,r7,lr}                    @ restaur registers */ 
    bx lr                                   @ return
/***************************************************/
/*   affichage message d erreur              */
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

```



## C


###  Xlib 

{{libheader|Xlib}}

Compile with:
* gcc hello-x.c -L/usr/X11R6/lib -lX11 -o hello-x


```c>#include <X11/Xlib.h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
   Display *d;
   Window w;
   XEvent e;
   const char *msg = "Hello, World!";
   int s;

   d = XOpenDisplay(NULL);
   if (d == NULL) {
      fprintf(stderr, "Cannot open display\n");
      exit(1);
   }

   s = DefaultScreen(d);
   w = XCreateSimpleWindow(d, RootWindow(d, s), 10, 10, 100, 100, 1,
                           BlackPixel(d, s), WhitePixel(d, s));
   XSelectInput(d, w, ExposureMask | KeyPressMask);
   XMapWindow(d, w);

   while (1) {
      XNextEvent(d, &e);
      if (e.type == Expose) {
         XFillRectangle(d, w, DefaultGC(d, s), 20, 20, 10, 10);
         XDrawString(d, w, DefaultGC(d, s), 10, 50, msg, strlen(msg));
      }
      if (e.type == KeyPress)
         break;
   }

   XCloseDisplay(d);
   return 0;
}
```



###  XCB 

{{libheader|XCB}}

Compile with:
* gcc -o helloxcb helloxcb.c -lxcb


```c>#include <stdlib.h

#include <stdio.h>
#include <string.h>

#include <xcb/xcb.h>

int main ()
{
  xcb_connection_t    *c;
  xcb_screen_t        *screen;
  xcb_drawable_t       win;
  xcb_gcontext_t       foreground;
  xcb_gcontext_t       background;
  xcb_generic_event_t *e;
  uint32_t             mask = 0;
  uint32_t             values[2];

  char string[] = "Hello, XCB!";
  uint8_t string_len = strlen(string);

  xcb_rectangle_t rectangles[] = {
    {40, 40, 20, 20},
  };

  c = xcb_connect (NULL, NULL);

  /* get the first screen */
  screen = xcb_setup_roots_iterator (xcb_get_setup (c)).data;

  /* root window */
  win = screen->root;

  /* create black (foreground) graphic context */
  foreground = xcb_generate_id (c);
  mask = XCB_GC_FOREGROUND | XCB_GC_GRAPHICS_EXPOSURES;
  values[0] = screen->black_pixel;
  values[1] = 0;
  xcb_create_gc (c, foreground, win, mask, values);

  /* create white (background) graphic context */
  background = xcb_generate_id (c);
  mask = XCB_GC_BACKGROUND | XCB_GC_GRAPHICS_EXPOSURES;
  values[0] = screen->white_pixel;
  values[1] = 0;
  xcb_create_gc (c, background, win, mask, values);

  /* create the window */
  win = xcb_generate_id(c);
  mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
  values[0] = screen->white_pixel;
  values[1] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_KEY_PRESS;
  xcb_create_window (c,                             /* connection    */
                     XCB_COPY_FROM_PARENT,          /* depth         */
                     win,                           /* window Id     */
                     screen->root,                  /* parent window */
                     0, 0,                          /* x, y          */
                     150, 150,                      /* width, height */
                     10,                            /* border_width  */
                     XCB_WINDOW_CLASS_INPUT_OUTPUT, /* class         */
                     screen->root_visual,           /* visual        */
                     mask, values);                 /* masks         */

  /* map the window on the screen */
  xcb_map_window (c, win);

  xcb_flush (c);

  while ((e = xcb_wait_for_event (c))) {
    switch (e->response_type & ~0x80) {
    case XCB_EXPOSE:
      xcb_poly_rectangle (c, win, foreground, 1, rectangles);
      xcb_image_text_8 (c, string_len, win, background, 20, 20, string);
      xcb_flush (c);
      break;
    case XCB_KEY_PRESS:
      goto endloop;
    }
    free (e);
  }
  endloop:

  return 0;
}
```



## COBOL

Tested with GnuCOBOL, and only on 64bit architecture, GNU/Linux.  Nods to the C XLib version.


```cobol
       identification division.
       program-id. x11-hello.
       installation. cobc -x x11-hello.cob -lX11
       remarks. Use of private data is likely not cross platform.

       data division.
       working-storage section.
       01 msg.
          05 filler            value z"S'up, Earth?".
       01 msg-len              usage binary-long value 12.

       01 x-display            usage pointer.
       01 x-window             usage binary-c-long.

      *> GnuCOBOL does not evaluate C macros, need to peek at opaque
      *> data from Xlib.h
      *> some padding is added, due to this comment in the header
      *> "there is more to this structure, but it is private to Xlib"
       01 x-display-private    based.
          05 x-ext-data        usage pointer sync.
          05 private1          usage pointer.
          05 x-fd              usage binary-long.
          05 private2          usage binary-long.
          05 proto-major-version   usage binary-long.
          05 proto-minor-version   usage binary-long.
          05 vendor            usage pointer sync.
          05 private3          usage pointer.
          05 private4          usage pointer.
          05 private5          usage pointer.
          05 private6          usage binary-long.
          05 allocator         usage program-pointer sync.
          05 byte-order        usage binary-long.
          05 bitmap-unit       usage binary-long.
          05 bitmap-pad        usage binary-long.
          05 bitmap-bit-order  usage binary-long.
          05 nformats          usage binary-long.
          05 screen-format     usage pointer sync.
          05 private8          usage binary-long.
          05 x-release         usage binary-long.
          05 private9          usage pointer sync.
          05 private10         usage pointer sync.
          05 qlen              usage binary-long.
          05 last-request-read usage binary-c-long unsigned sync.
          05 request           usage binary-c-long unsigned sync.
          05 private11         usage pointer sync.
          05 private12         usage pointer.
          05 private13         usage pointer.
          05 private14         usage pointer.
          05 max-request-size  usage binary-long unsigned.
          05 x-db              usage pointer sync.
          05 private15         usage program-pointer sync.
          05 display-name      usage pointer.
          05 default-screen    usage binary-long.
          05 nscreens          usage binary-long.
          05 screens           usage pointer sync.
          05 motion-buffer     usage binary-c-long unsigned.
          05 private16         usage binary-c-long unsigned.
          05 min-keycode       usage binary-long.
          05 max-keycode       usage binary-long.
          05 private17         usage pointer sync.
          05 private18         usage pointer.
          05 private19         usage binary-long.
          05 x-defaults        usage pointer sync.
          05 filler            pic x(256).

       01 x-screen-private     based.
          05 scr-ext-data      usage pointer sync.
          05 display-back      usage pointer.
          05 root              usage binary-c-long.
          05 x-width           usage binary-long.
          05 x-height          usage binary-long.
          05 m-width           usage binary-long.
          05 m-height          usage binary-long.
          05 x-ndepths         usage binary-long.
          05 depths            usage pointer sync.
          05 root-depth        usage binary-long.
          05 root-visual       usage pointer sync.
          05 default-gc        usage pointer.
          05 cmap              usage pointer.
          05 white-pixel       usage binary-c-long unsigned sync.
          05 black-pixel       usage binary-c-long unsigned.
          05 max-maps          usage binary-long.
          05 min-maps          usage binary-long.
          05 backing-store     usage binary-long.
          05 save_unders       usage binary-char.
          05 root-input-mask   usage binary-c-long sync.
          05 filler            pic x(256).

       01 event.
          05 e-type usage      binary-long.
          05 filler            pic x(188).
          05 filler            pic x(256).
       01 Expose               constant as 12.
       01 KeyPress             constant as 2.

      *> ExposureMask or-ed with KeyPressMask, from X.h
       01 event-mask           usage binary-c-long value 32769.

      *> make the box around the message wide enough for the font
       01 x-char-struct.
          05 lbearing          usage binary-short.
          05 rbearing          usage binary-short.
          05 string-width      usage binary-short.
          05 ascent            usage binary-short.
          05 descent           usage binary-short.
          05 attributes        usage binary-short unsigned.
       01 font-direction       usage binary-long.
       01 font-ascent          usage binary-long.
       01 font-descent         usage binary-long.

       01 XGContext            usage binary-c-long.
       01 box-width            usage binary-long.
       01 box-height           usage binary-long.

      *> ***************************************************************
       procedure division.

       call "XOpenDisplay" using by reference null returning x-display
           on exception
               display function module-id " Error: "
                       "no XOpenDisplay linkage, requires libX11"
                  upon syserr
               stop run returning 1
       end-call
       if x-display equal null then
           display function module-id " Error: "
                   "XOpenDisplay returned null" upon syserr
           stop run returning 1
       end-if
       set address of x-display-private to x-display

       if screens equal null then
           display function module-id " Error: "
                   "XOpenDisplay associated screen null" upon syserr
           stop run returning 1
       end-if
       set address of x-screen-private to screens

       call "XCreateSimpleWindow" using
           by value x-display root 10 10 200 50 1
                    black-pixel white-pixel
           returning x-window
       call "XStoreName" using
           by value x-display x-window by reference msg

       call "XSelectInput" using by value x-display x-window event-mask

       call "XMapWindow" using by value x-display x-window

       call "XGContextFromGC" using by value default-gc
           returning XGContext
       call "XQueryTextExtents" using by value x-display XGContext
           by reference msg by value msg-len
           by reference font-direction font-ascent font-descent
           x-char-struct
       compute box-width = string-width + 8
       compute box-height = font-ascent + font-descent + 8

       perform forever
          call "XNextEvent" using by value x-display by reference event
          if e-type equal Expose then
              call "XDrawRectangle" using
                  by value x-display x-window default-gc 5 5
                           box-width box-height
              call "XDrawString" using
                  by value x-display x-window default-gc 10 20
                  by reference msg by value msg-len
          end-if
          if e-type equal KeyPress then exit perform end-if
       end-perform

       call "XCloseDisplay" using by value x-display

       goback.
       end program x11-hello.
```



## Common Lisp


{{field attention|Common Lisp|X11|This example was written in near-total ignorance of X11 by consulting the [http://www.cliki.net/CLX%20Manual CLX manual] to find equivalents for the parts of the C example. It was also only tested with [[Mac OS X]] X11, which is not exactly normal. — (ps: Testing on Linux (Fedora 21/x86-64), it seems to display black-on-black, so the "Hello" is lost.). Testing new code changes (i.e. adding background and foreground) should fix the issue, it was tested on LinuxMint 17.1/x86-64.
Also tested on Debian; with a minor change on the text position it creates a pixel perfect replica of the C example. Could this warning be removed then?}}

{{trans|C}}

{{libheader|CLX}}

This example uses CLX, which is the de facto standard X11 library for Common Lisp. CLX is not a binding to Xlib; it is a Lisp library implementing the X11 protocol.


```lisp
;;; Single-file/interactive setup; large applications should define an ASDF system instead

(let* ((display (open-default-display))
       (screen (display-default-screen display))
       (root-window (screen-root screen))
       (black-pixel (screen-black-pixel screen))
       (white-pixel (screen-white-pixel screen))
       (window (create-window :parent root-window
                              :x 10 :y 10
                              :width 100 :height 100
                              :background white-pixel
                              :event-mask '(:exposure :key-press)))
       (gc (create-gcontext :drawable window
                            :foreground black-pixel
                            :background white-pixel)))
  (map-window window)
  (unwind-protect
       (event-case (display :discard-p t)
         (exposure ()
                   (draw-rectangle window gc 20 20 10 10 t)
                   (draw-glyphs window gc 10 50 "Hello, World!")
                   nil #| continue receiving events |#)
         (key-press ()
                    t #| non-nil result signals event-case to exit |#))
    (when window
      (destroy-window window))
    (when gc
      (free-gcontext gc))
    (close-display display)))


```



## Go

{{trans|C}}

```go
package main

import (
    "code.google.com/p/x-go-binding/xgb"
    "fmt"
)

func main() {
    c, err := xgb.Dial("")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer c.Close()

    strBytes := []byte("Hello XGB!")
    rectangles := []xgb.Rectangle{{40, 40, 20, 20}}

    // get the first screen
    s := c.DefaultScreen()

    // root window
    win := s.Root

    // create black (foreground) graphic context
    fg := c.NewId()
    mask := uint32(xgb.GCForeground | xgb.GCGraphicsExposures)
    values := []uint32{s.BlackPixel, 0}
    c.CreateGC(fg, win, mask, values)

    // create white (background) graphic context
    bg := c.NewId()
    mask = uint32(xgb.GCBackground | xgb.GCGraphicsExposures)
    values[0] = s.WhitePixel // (values[1] still 0)
    c.CreateGC(bg, win, mask, values)

    //  create the window
    win = c.NewId()
    mask = xgb.CWBackPixel | xgb.CWEventMask
    // values[0] still s.WhitePixel
    values[1] = xgb.EventMaskExposure | xgb.EventMaskKeyPress
    c.CreateWindow(0, win, s.Root, 0, 0, 150, 150, 10,
        xgb.WindowClassInputOutput, s.RootVisual, mask, values)
    c.MapWindow(win)

    for {
        event, err := c.WaitForEvent()
        if err != nil {
            fmt.Println(err)
            return
        }
        switch event.(type) {
        case xgb.ExposeEvent:
            c.PolyRectangle(win, fg, rectangles)
            c.ImageText8(win, bg, 20, 20, strBytes)
        case xgb.KeyPressEvent:
            return
        }
    }
}
```

Screen capture:

[[file:Go-x11.png]]


## Groovy

Run: 
 groovy WindowCreation.groovy


```groovy
import javax.swing.*
import java.awt.*
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.awt.geom.Rectangle2D

class WindowCreation extends JApplet implements Runnable {
    void paint(Graphics g) {
        (g as Graphics2D).with {
            setStroke(new BasicStroke(2.0f))
            drawString("Hello Groovy!", 20, 20)
            setPaint(Color.blue)
            draw(new Rectangle2D.Double(10d, 50d, 30d, 30d))
        }
    }

    void run() {
        new JFrame("Groovy Window Demo").with {
            addWindowListener(new WindowAdapter() {
                void windowClosing(WindowEvent e) {
                    System.exit(0)
                }
            })

            getContentPane().add("Center", new WindowCreation())
            pack()
            setSize(new Dimension(150, 150))
            setVisible(true)
        }
    }
}
```



## GUISS

Graphical User Interface Support Script is really a language for operating a computer, rather than programming one, so we cannot do this via X11 libraries. The example uses leafpad for our open window, and the box symbols to enclose our text:


```guiss
Start,Programs,Applications,Editors,Leafpad,Textbox,
Type:[openbox]Hello World[pling][closebox]
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon provide a portable graphics implementation that does not rely upon a toolkit.  The intent is to be platform independent and the same code runs on multiple platforms without change and producing results with only minor variations.  Icon and Unicon graphics are implemented in X-Windows as well as MS-Windows and others.  There are additional 3D graphics capabilities implemented using opengl.

```Icon
procedure main()
   W1 := open("X-Window","g","size=250,250","bg=black","fg=red") | stop("unable to open window")
   FillRectangle(W1,50,50,150,150)
   WDone(W1)
end

link graphics
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/graphics.icn graphics.icn provides graphics] 

Additionally, the '''WOpen''' procedure and Window.App methods are available.


## Haskell

Using {{libheader|X11}} from [http://hackage.haskell.org/packages/hackage.html HackageDB]

```haskell
import Graphics.X11.Xlib
import Control.Concurrent (threadDelay)

main = do
  display <- openDisplay ""
  let defScr = defaultScreen display
  rw <- rootWindow display defScr

  xwin <- createSimpleWindow display rw
      0 0 400 200 1
      (blackPixel display defScr)
      (whitePixel display defScr)

  setTextProperty display xwin "Rosetta Code: X11 simple window" wM_NAME

  mapWindow display xwin

  sync display False
  threadDelay (5000000)

  destroyWindow display xwin
  closeDisplay display

```



## Java

{{libheader|Java}}compile
 javac WindowExample.java
run
 java WindowExample

```java
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

public class WindowExample {

  public static void main(String[] args) {
    Runnable runnable = new Runnable() {
      public void run() {
	createAndShow();
      }
    };
    SwingUtilities.invokeLater(runnable);
  }
	
  static void createAndShow() {
    JFrame frame = new JFrame("Hello World");
    frame.setSize(640,480);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setVisible(true);
  }
}
```
The previous example works but doesn't write any text or draw any box; the following does both.
```java
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;

public class WindowExample extends JApplet {
    public void paint(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;

        g2.setStroke(new BasicStroke(2.0f));
        g2.drawString("Hello java", 20, 20);
        g2.setPaint(Color.blue);
        g2.draw(new Rectangle2D.Double(40, 40, 20, 20));
    }

    public static void main(String s[]) {
        JFrame f = new JFrame("ShapesDemo2D");
        f.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {System.exit(0);}
        });
        JApplet applet = new ShapesDemo2D();
        f.getContentPane().add("Center", applet);
        f.pack();
        f.setSize(new Dimension(150, 150));
        f.setVisible(true);
    }
}
```



## Julia

This was based on https://en.wikipedia.org/wiki/Xlib, and mostly quoted from from the XLib.jl test2() testing code function.

```julia

using Xlib

function x11demo()
    # Open connection to the server.
    dpy = XOpenDisplay(C_NULL)
    dpy == C_NULL && error("unable to open display")
    scr = DefaultScreen(dpy)

    # Create a window.
    win = XCreateSimpleWindow(dpy, RootWindow(dpy, scr), 10, 10, 300, 100, 1,
                              BlackPixel(dpy, scr), WhitePixel(dpy, scr))

    # Select the kind of events we are interested in.
    XSelectInput(dpy, win, ExposureMask | KeyPressMask)

    # Show or in x11 terms map window.
    XMapWindow(dpy, win)

    # Run event loop.
    evt = Ref(XEvent())
    while true
        XNextEvent(dpy, evt)

        # Draw or redraw the window.
        if EventType(evt) == Expose
            XFillRectangle(dpy, win, DefaultGC(dpy, scr), 24, 24, 16, 16)
            XDrawString(dpy, win, DefaultGC(dpy, scr), 50, 50, "Hello, World! Press any key to exit.")
        end

        # Exit whenever a key is pressed.
        if EventType(evt) == KeyPress
            break
        end
    end

    # Shutdown server connection
    XCloseDisplay(dpy)
end

x11demo()

```



## Kotlin

{{trans|C}}
{{libheader|Xlib}}

```scala
// Kotlin Native v0.3

import kotlinx.cinterop.*
import Xlib.*

fun main(args: Array<String>) {
    val msg = "Hello, World!"
    val d = XOpenDisplay(null)
    if (d == null) {
        println("Cannot open display")
        return
    }

    val s = XDefaultScreen(d)
    val w = XCreateSimpleWindow(d, XRootWindow(d, s), 10, 10, 160, 160, 1,
                                XBlackPixel(d, s), XWhitePixel(d, s))
    XSelectInput(d, w, ExposureMask or KeyPressMask)
    XMapWindow(d, w)
    val e = nativeHeap.alloc<XEvent>()
    
    while (true) {
        XNextEvent(d, e.ptr)
        if (e.type == Expose) {
            XFillRectangle(d, w, XDefaultGC(d, s), 55, 40, 50, 50)
            XDrawString(d, w, XDefaultGC(d, s), 45, 120, msg, msg.length)
        }
        else if (e.type == KeyPress) break
    }

    XCloseDisplay(d)
    nativeHeap.free(e)
}
```


## M2000 Interpreter

M2000 interpteter is a Visual Basic 6 (vb6) gui application, so we can't use X11. When we use Wine, connecting to X11 may occur but this is invisible for interpreter scope.

Ao for this task we use the M2000 way to make a form and do something on it.


```M2000 Interpreter

\\ M2000 froms (windows) based on a flat, empty with no title bar, vb6 window and a user control.
\\ title bar is a user control in M2000 form
\\ On linux, wine application run M2000 interpreter traslating vb6 calls to window system.
Module SquareAndText2Window {
	Const black=0, white=15
	Declare form1 form
	\\ defaultwindow  title is the name of variable,here is: form1 
	Rem With form1, "Title",  "A title for this window"
	Method form1,"move", 2000,3000,10000,8000  ' in twips
	layer form1 {
		Cls white
		Font "Verdana"
		Rem Window 12 , 10000,8000;  ' hide modr 13 using a REM before
		Rem 
		Mode 12    ' 12 in pt
		Cls white,  2      ' fill white, set third raw for top of scrolling frame
		Pen  black
		Move 1000,2000  ' absolute coordinated in twips - use Step for relative coordinates
		\\ polygon use relative coordinates in twips
		polygon white, 1000,0,0,-1000,-1000,0,0,-1000
		\\ write text using graphic coordinates
		Move 1000, 3000 : Legend "Hello World", "Arial", 12
		\\ write text using layer as console (font Verdana)
		Print @(15,5),"Goodbye Wolrd"
		\\ 9120  7920 if we use window 12,10000, 8000 (cut exactly for console use)
		\\ 10000 8000 if we use Mode 12
		\\ scale.y include window height (including header)
		\\ the layer extend bellow 
		Print scale.x, scale.y 
	}
	\\ show form1 modal (continue to next line when we close the window)
	Method form1,"Show", 1
	\\ closing meand hide in M2000
	wait 1000  ' in msec
	Method form1,"move", random(2000, 10000),random(3000, 8000)
	\\ now show again
	Method form1,"Show", 1
	\\ now we close the form, releasing resources
	Declare form1 Nothing
}
SquareAndText2Window

```


## Mathematica

Note that GUIKit is a high-level wrapper for Swing.

<lang>
 Needs["GUIKit`"]
 ref = GUIRun[Widget["Panel", {
    Widget[
     "ImageLabel", {"data" -> 
       Script[ExportString[Graphics[Rectangle[{0, 0}, {1, 1}]], 
         "GIF"]]}],
    Widget["Label", { "text" -> "Hello World!"}]}
   ]]

```



## OCaml

{{libheader|OCaml-Xlib}}

execute as a script with:
 ocaml -I +Xlib Xlib.cma script.ml
or compile to native code:
 ocamlopt -I +Xlib Xlib.cmxa prog.ml -o prog

or to make a standalone script add these lines at the beginning of the file:
 #!/usr/bin/env ocaml
 #directory "+Xlib"
 #load "Xlib.cma"


```ocaml
open Xlib

let () =
  let d = xOpenDisplay "" in
  let s = xDefaultScreen d in
  let w = xCreateSimpleWindow d (xRootWindow d s) 10 10 100 100 1
                                (xBlackPixel d s) (xWhitePixel d s) in
  xSelectInput d w [ExposureMask; KeyPressMask];
  xMapWindow d w;

  let msg = "Hello, World!" in

  let rec main_loop() =
    match xEventType(xNextEventFun d) with
    | Expose ->
        xFillRectangle d w (xDefaultGC d s) 20 20 10 10;
        xDrawString d w (xDefaultGC d s) 10 50 msg;
        main_loop()
    | KeyPress -> ()  (* exit main loop *)
    | _ -> main_loop()
  in
  main_loop();
  xCloseDisplay d;
;;
```





## Pascal

{{trans|C}}
{{libheader|xLib, x ,ctypes}}
from wiki.freepascal.org/X11#Examples.
Compiled with Freepascal 2.6.4-32

```pascal
program xshowwindow;
{$mode objfpc}{$H+}

uses
  xlib, x, ctypes;
 
procedure ModalShowX11Window(AMsg: string);
var
  d: PDisplay;
  w: TWindow;
  e: TXEvent;
  msg: PChar;
  s: cint;
begin
  msg := PChar(AMsg);
 
  { open connection with the server }
  d := XOpenDisplay(nil);
  if (d = nil) then
  begin
    WriteLn('[ModalShowX11Window] Cannot open display');
    exit;
  end;
  s := DefaultScreen(d);
 
  { create window }
  w := XCreateSimpleWindow(d, RootWindow(d, s), 10, 10, 100, 50, 1,
                           BlackPixel(d, s), WhitePixel(d, s));
 
  { select kind of events we are interested in }
  XSelectInput(d, w, ExposureMask or KeyPressMask);
 
  { map (show) the window }
  XMapWindow(d, w);
 
  { event loop }
  while (True) do
  begin
    XNextEvent(d, @e);
    { draw or redraw the window }
    if (e._type = Expose) then
    begin
      XFillRectangle(d, w, DefaultGC(d, s), 0, 10, 100, 3);
      XFillRectangle(d, w, DefaultGC(d, s), 0, 30, 100, 3);      
      XDrawString   (d, w, DefaultGC(d, s), 5, 25, msg, strlen(msg));
    end;
    { exit on key press }
    if (e._type = KeyPress) then Break;
  end;
 
  { close connection to server }
  XCloseDisplay(d);
end;
 
begin
  ModalShowX11Window('Hello, World!');
end.

```



## Perl


###  X11::Protocol 


```perl
#!/usr/bin/perl -w
use strict;
use X11::Protocol;

my $X = X11::Protocol->new;

my $window = $X->new_rsrc;
$X->CreateWindow ($window,
                  $X->root,         # parent window
                  'InputOutput',    # class
                  0,                # depth, copy from parent
                  0,                # visual, copy from parent
                  0,0,              # X,Y (window manager will override)
                  300,100,          # width,height
                  0,                # border width
                  background_pixel => $X->black_pixel,
                  event_mask       => $X->pack_event_mask('Exposure',
                                                          'ButtonPress'),
                 );

my $gc = $X->new_rsrc;
$X->CreateGC ($gc, $window,
              foreground => $X->white_pixel);

$X->{'event_handler'} = sub {
  my %event = @_;
  my $event_name = $event{'name'};

  if ($event_name eq 'Expose') {
    $X->PolyRectangle ($window, $gc, [ 10,10,     # x,y top-left corner
                                       30,20 ]);  # width,height
    $X->PolyText8 ($window, $gc,
                   10, 55,    # X,Y of text baseline
                   [ 0,  # delta X
                     'Hello ... click mouse button to exit.' ]);

  } elsif ($event_name eq 'ButtonPress') {
    exit 0;
  }
};

$X->MapWindow ($window);
for (;;) {
  $X->handle_input;
}
```



## Perl 6

{{trans|C}}

There is not yet a X11 library in Perl 6 but we can write the minimal C mappings for this task.


```perl6
use NativeCall;

class Display is repr('CStruct') {
    has int32 $!screen;
    has int32 $!window;
 }
class GC      is repr('CStruct') {
    has int32 $!context;
}
class XEvent  is repr('CStruct') {
    has int32 $.type;
    method init { $!type = 0 }
}

sub XOpenDisplay(Str $name = ':0') returns Display is native('X11') { * }
sub XDefaultScreen(Display $) returns int32 is native('X11') { * }
sub XRootWindow(Display $, int32 $screen_number) returns int32 is native('X11') { * }
sub XBlackPixel(Display $, int32 $screen_number) returns int32 is native('X11') { * }
sub XWhitePixel(Display $, int32 $screen_number) returns int32 is native('X11') { * }
sub XCreateSimpleWindow(
    Display $, int32 $parent_window, int32 $x, int32 $y,
    int32 $width, int32 $height, int32 $border_width,
    int32 $border, int32 $background
) returns int32 is native('X11') { * }
sub XMapWindow(Display $, int32 $window) is native('X11') { * }
sub XSelectInput(Display $, int32 $window, int32 $mask) is native('X11') { * }
sub XFillRectangle(
    Display $, int32 $window, GC $, int32 $x, int32 $y, int32 $width, int32 $height
) is native('X11') { * }
sub XDrawString(
    Display $, int32 $window, GC $, int32 $x, int32 $y, Str $, int32 $str_length
) is native('X11') { * }
sub XDefaultGC(Display $, int32 $screen) returns GC is native('X11') { * }
sub XNextEvent(Display $, XEvent $e)              is native('X11') { * }
sub XCloseDisplay(Display $)                      is native('X11') { * }

my Display $display = XOpenDisplay()
    or die "Can not open display";

my int $screen = XDefaultScreen($display);
my int $window = XCreateSimpleWindow(
    $display,
    XRootWindow($display, $screen),
    10, 10, 100, 100, 1,
    XBlackPixel($display, $screen), XWhitePixel($display, $screen)
);
XSelectInput($display, $window, 1 +< 15 +| 1);
XMapWindow($display, $window);

my Str $msg = 'Hello, World!';
my XEvent $e .= new; $e.init;
loop {
    XNextEvent($display, $e);
    if $e.type == 12 {
	    XFillRectangle($display, $window, XDefaultGC($display, $screen), 20, 20, 10, 10);
	    XDrawString($display, $window, XDefaultGC($display, $screen), 10, 50, $msg, my int $ = $msg.chars);
    }
    elsif $e.type == 2 {
	    last;
    }
}
XCloseDisplay($display);
```



## Phix

{{trans|TXR}}
See [[Window_creation#Phix]]


## PicoLisp

The following script works in the 32-bit version, using inlined C code

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(load "@lib/misc.l" "@lib/gcc.l")

(gcc "x11" '("-lX11") 'simpleWin)

#include <X11/Xlib.h>

any simpleWin(any ex) {
   any x = cdr(ex);
   int dx, dy;
   Display *disp;
   int scrn;
   Window win;
   XEvent ev;

   x = cdr(ex),  dx = (int)evCnt(ex,x);
   x = cdr(x),  dy = (int)evCnt(ex,x);
   x = evSym(cdr(x));
   if (disp = XOpenDisplay(NULL)) {
      char msg[bufSize(x)];

      bufString(x, msg);
      scrn = DefaultScreen(disp);
      win = XCreateSimpleWindow(disp, RootWindow(disp,scrn), 0, 0, dx, dy,
                           1, BlackPixel(disp,scrn), WhitePixel(disp,scrn) );
      XSelectInput(disp, win, ExposureMask | KeyPressMask | ButtonPressMask);
      XMapWindow(disp, win);
      for (;;) {
         XNextEvent(disp, &ev);
         switch (ev.type) {
         case Expose:
            XDrawRectangle(disp, win, DefaultGC(disp, scrn), 10, 10, dx-20, dy-20);
            XDrawString(disp, win, DefaultGC(disp, scrn), 30, 40, msg, strlen(msg));
            break;
         case KeyPress:
         case ButtonPress:
            XCloseDisplay(disp);
            return Nil;
         }
      }
   }
   return mkStr("Can't open Display");
}
/**/

(simpleWin 300 200 "Hello World")
(bye)
```



## Python



###  Xlib 

{{field attention|Python|X11|Note (stolen from CLX example): This example was written in near-total ignorance of X11 by consulting the python-xlib's examples (included in its distribution) to find equivalents for the parts of the C example.}}

{{trans|C}}

{{libheader|python-xlib}}

Download Python X library from http://sourceforge.net/projects/python-xlib/ .
python-xlib is a pure python library therefore the example should work anywhere where python does and where there is an X server.
Run:
* python xlib_hello_world.py


```python
from Xlib import X, display

class Window:
    def __init__(self, display, msg):
        self.display = display
        self.msg = msg
        
        self.screen = self.display.screen()
        self.window = self.screen.root.create_window(
            10, 10, 100, 100, 1,
            self.screen.root_depth,
            background_pixel=self.screen.white_pixel,
            event_mask=X.ExposureMask | X.KeyPressMask,
            )
        self.gc = self.window.create_gc(
            foreground = self.screen.black_pixel,
            background = self.screen.white_pixel,
            )

        self.window.map()

    def loop(self):
        while True:
            e = self.display.next_event()
                
            if e.type == X.Expose:
                self.window.fill_rectangle(self.gc, 20, 20, 10, 10)
                self.window.draw_text(self.gc, 10, 50, self.msg)
            elif e.type == X.KeyPress:
                raise SystemExit

                
if __name__ == "__main__":
    Window(display.Display(), "Hello, World!").loop()
```



###  XCB 

{{libheader|python-xcb}}


```python
import xcb
from xcb.xproto import *
import xcb.render

def main():
  conn = xcb.connect()
  conn.render = conn(xcb.render.key)

  setup = conn.get_setup()
  root = setup.roots[0].root
  depth = setup.roots[0].root_depth
  visual = setup.roots[0].root_visual
  white = setup.roots[0].white_pixel

  window = conn.generate_id()
  conn.core.CreateWindow(depth, window, root,
                         0, 0, 640, 480, 0,
                         WindowClass.InputOutput,
                         visual,
                         CW.BackPixel | CW.EventMask,
                         [ white, EventMask.Exposure |
                                  EventMask.KeyPress ])

  conn.core.MapWindow(window)
  conn.flush()

  while True:
    event = conn.wait_for_event()

    if isinstance(event, ExposeEvent):
      color = (0, 0, 65535, 65535)
      rectangle = (20, 20, 40, 40)
      # TODO, fixme:
      # I haven't been able to find what I should put for the parameter "op"
   #  conn.render.FillRectangles(op, window, color, 1, rectangle)
      conn.flush()

    elif isinstance(event, KeyPressEvent):
      break

  conn.disconnect()

main()
```



## Racket

Using Racket's GUI which is implemented using gtk.  It's not low level, but OTOH it works on Windows and OS X too.


```Racket
#lang racket/gui

(define frame (new frame%
                   [label "Example"]
                   [width 300]
                   [height 300]))
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Don't Panic!" 0 0))])
(send frame show #t)
```



## Scala

{{libheader|Scala}}
```Scala
import scala.swing.{ MainFrame, SimpleSwingApplication }
import scala.swing.Swing.pair2Dimension

object WindowExample extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Hello!"
    centerOnScreen
    preferredSize = ((200, 150))
  }
}
```



## Tcl

Tcl does not come with a low-level connection to the X protocol, as it has long been distributed with [[Tk]] which offers a [[Simple Windowed Application|much higher-level interface]] (and which is portable to other platforms too). This means that the interface has to be crafted directly. This can be done with either [http://www.swig.org/ SWIG] or [[critcl]]. This example shows how to do it the latter way:


### Low level interface

{{libheader|critcl}}

```tcl
package provide xlib 1
package require critcl

critcl::clibraries -L/usr/X11/lib -lX11
critcl::ccode {
    #include <X11/Xlib.h>
    static Display *d;
    static GC gc;
}

# Display connection functions
critcl::cproc XOpenDisplay {Tcl_Interp* interp char* name} ok {
    d = XOpenDisplay(name[0] ? name : NULL);
    if (d == NULL) {
	Tcl_AppendResult(interp, "cannot open display", NULL);
	return TCL_ERROR;
    }
    gc = DefaultGC(d, DefaultScreen(d));
    return TCL_OK;
}
critcl::cproc XCloseDisplay {} void {
    XCloseDisplay(d);
}

# Basic window functions
critcl::cproc XCreateSimpleWindow {
    int x  int y  int width  int height  int events
} int {
    int s = DefaultScreen(d);
    Window w = XCreateSimpleWindow(d, RootWindow(d,s), x, y, width, height, 0,
	    BlackPixel(d,s), WhitePixel(d,s));
    XSelectInput(d, w, ExposureMask | events);
    return (int) w;
}
critcl::cproc XDestroyWindow {int w} void {
    XDestroyWindow(d, (Window) w);
}
critcl::cproc XMapWindow {int w} void {
    XMapWindow(d, (Window) w);
}
critcl::cproc XUnmapWindow {int w} void {
    XUnmapWindow(d, (Window) w);
}

# Event receiver
critcl::cproc XNextEvent {Tcl_Interp* interp} char* {
    XEvent e;
    XNextEvent(d, &e);
    switch (e.type) {
	case Expose:	return "type expose";
	case KeyPress:	return "type key";
	/* etc. This is a cheap hack version. */
	default:	return "type ?";
    }
}

# Painting functions
critcl::cproc XFillRectangle {int w int x int y int width int height} void {
    XFillRectangle(d, (Window)w, gc, x, y, width, height);
}
critcl::cproc XDrawString {int w int x int y Tcl_Obj* msg} void {
    int len;
    const char *str = Tcl_GetStringFromObj(msg, &len);
    XDrawString(d, (Window)w, gc, x, y, str, len);
}
```

Note that this only does enough for this demo. A full adaptation is too long for RosettaCode...

This could then be used like this:

```tcl
package require xlib

XOpenDisplay {}
set w [XCreateSimpleWindow 10 10 100 100 1]
XMapWindow $w
while {[lindex [XNextEvent] 0] == "expose"} {
    XFillRectangle $w 20 20 10 10
    XDrawString $w 10 50 "Hello, World!"
}
XDestroyWindow $w
XCloseDisplay
```


### Higher level interface

Just because there is a low level package does not mean that it is pleasant to use from Tcl code. Therefore this second package wraps it up inside a higher-level package that provides a more natural way of interacting.


{{works with|Tcl|8.6}} or {{libheader|TclOO}}

```tcl
package require TclOO
package provide x11 1

namespace eval ::x {
    namespace export {[a-z]*}
    namespace ensemble create
    variable mask
    array set mask {
	KeyPress	1
	KeyRelease	2
	ButtonPress	4
	ButtonRelease	8
    }

    proc display {script} {
	XOpenDisplay {}
	catch {uplevel 1 $script} msg opts
	XCloseDisplay
	return -options $opts $msg
    }
    proc eventloop {var handlers} {
	upvar 1 $var v
	while 1 {
	    set v [XNextEvent]
	    uplevel 1 [list switch [dict get $v type] $handlers]
	}
    }

    oo::class create window {
	variable w
        constructor {x y width height events} {
	    set m 0
	    variable ::x::mask
	    foreach e $events {catch {incr m $mask($e)}}
	    set w [XCreateSimpleWindow $x $y $width $height $m]
	}
	method map {} {
	    XMapWindow $w
	}
	method unmap {} {
	    XUnmapWindow $w
	}
	method fill {x y width height} {
	    XFillRectangle $w $x $y $width $height
	}
	method text {x y string} {
	    XDrawString $w $x $y $string
	}
	destructor {
	    XDestroyWindow $w
	}
    }
}
```

This script puts the pieces together to carry out the details of the task.

```tcl
package require x11

# With a display connection open, create and map a window
x display {
    set w [x window new 10 10 100 100 KeyPress]
    $w map

    x eventloop ev {
	expose {
	    # Paint the window
	    $w fill 20 20 10 10
	    $w text 10 50 "Hello, World!"
	}
	key {
	    # Quit the event loop
	    break
	}
    }

    $w destroy
}
```

Improving this by adding more sophisticated event handling and more window methods is left as an exercise.


## TXR


See [[Window_creation#TXR]].

{{omit from|ACL2}}
{{omit from|AutoHotkey}}
{{omit from|AWK|Does not provide native X11 support}}
{{omit from|Batch File|Does not have the necessary capabilities of interfacing with an X server.}}
{{omit from|Erlang|Does not provide native X11 support}}
{{omit from|Locomotive Basic}}
{{omit from|Logtalk}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|PostScript}}
{{omit from|PowerBASIC|DOS & Windows only}}
{{omit from|PureBasic}}
{{omit from|SNOBOL4}}
{{omit from|SQL PL|It does not handle GUI}}
{{omit from|TI-83 BASIC|No X11 server access.}}
{{omit from|TI-89 BASIC|No X11 server access.}} <!-- Does not have a local X server or network access. -->
{{omit from|TPP}}
{{omit from|Unlambda|No X11 server access.}}
{{omit from|Retro}}
{{omit from|ZX Spectrum Basic|No X11 server access.}}
