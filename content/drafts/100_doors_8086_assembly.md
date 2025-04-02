+++
title = "100 doors/8086 Assembly"
description = ""
date = 2010-03-08T23:57:50Z
aliases = []
[extra]
id = 5314
[taxonomies]
categories = []
tags = []
+++

*Part of: [100 doors](/tasks/100 doors)*

**not optimized**

This example is 51 bytes big when assembled, it is fully commented, and it isn't optimized. The doors are represented in RAM by 100 bytes, which can have value 00 if the door is closed, FF is the door is opened. This example is designed to be run on any 8086 based computer, and it requires to be loaded in RAM, with 100 bytes available to it after the code, to be used for door storing. See the program comments for further explaining.

```asm
;   ** Initialize
;      Open all the doors.
;       The first pass in toggling the doors starts
;      with skipping none: this results in all the
;      doors being opened, no one is left closed.
;       So, it's pointless to initialize the doors
;      as closed because all the doors would be
;      opened in the first pass: it's just a waste
;      of time. For this reason the initial state does
;      have all the doors open.

init:
mov bx,offset doors     ; Load in BX the starting offset
                        ;of the doors data.
mov ax,0FFFFh           ; Set the AX register with
                        ;FFFF: open two doors (a byte=a door=
                        ;FF, two bytes=a word=two doors=FFFF).
mov cl,032h             ; Set CL to 50 decimal = 32 hex
                        ;The half of 100 because we will
                        ;write two bytes at once, opening
                        ;two doors and reducing the time needed
                        ;to do the task.
and cl,cl               ; Set the flags according to cl. 

initloop:
jz dodoor               ; If the counter is zero continue
                        ;to the main loop.
mov [bx],ax             ; Open two doors (write a FFFF word).
inc bx                  ; Increment the pointer.
inc bx
dec cl                  ; Decrement the counter.
jmp initloop            ; Loop.

;   ** Main
;      Do toggle the doors in memory.
   
dodoor:
mov cl,01h              ; Set the CL counter register with 01.
doorloop:
mov bx,offset doors-1   ; Set the pointer BX to the doors
                        ;address minus one because otherwise
                        ;zero would count as one ("doors"
                        ;points to the first door).
inc cl                  ; Increment the counter register CX.
                        ;The first time this instruction is
                        ;executed, CX will be two.
cmp cl,065h             ; If the counter reached 101, we did
jz programend           ;100 iterations of the loop, so we
                        ;have finished.
doorloop1:
add bx,cx               ; Does add to the pointer in BX the
                        ;current counter value, getting the
                        ;pointer to the next door to toggle.
cmp bx,offset doors+065h; If we are going over the 100th door
jnc doorloop            ;quit this loop to increment the
                        ;counter.
mov al,[bx]             ; Load the current door value in AL.
not al                  ; Complement the "door". That is, if
                        ;AL is 00, it goes FF, if it is FF it
                        ;goes 00. In other words, an opened
                        ;door is closed, a closed one is
                        ;opened.
mov [bx],al             ; Update the door overwriting his
                        ;byte with the new one.
jmp doorloop1           ; Loop. 

;   ** End
;      This program written to be run even on a standalone 8086
;      cannot be ended by usual methods by calling the Operating
;      system to unload the program from memory, because the
;      OS could be absent. So the program does simply freeze the
;      CPU.

programend:
cli                     ; Clear the interrupt flag, disabling
                        ;any software and most of the hardware
                        ;interrupts.
hlt                     ; Freeze the processor.
jmp programend          ; If a Non Maskable Interrupt does pull
                        ;the CPU from his freezed state, do
                        ;freeze it again.

doors:                  ; Pointer to the end of the program,
                        ;where the RAM space is free to keep the
                        ;100 doors bytes.
```

**optimized**

This example is 42 bytes big when assembled, it is fully commented, and it is optimized. See the unoptimized program's notes and the program comments for further explaining.

```asm
;   ** Initialize
;      Close all the doors.

init:
mov bx,offset doors     ; Load in BX the starting offset
                        ;of the doors data.
xor ax,ax               ; Set the AX register with
                        ;0000: close two doors (a byte=a door=
                        ;00, two bytes=a word=two doors=0000)
                        ;This pass is done by XORing the word
                        ;in AX by itself, clearing it to zero.
                        ;This occupies two bytes in memory,
                        ;instead of the four needed for
                        ;the equivalent operation mov ax,0000h.
mov cl,032h             ; Set CL to 50 decimal = 32 hex.
                        ;The half of 100 because we will
                        ;write two bytes at once, closing
                        ;two doors and reducing the time needed
                        ;to do the task.

initloop:
mov [bx],ax             ; Close two doors (write a 0000 word).
inc bx                  ; Increment the pointer.
inc bx
dec cl                  ; Decrement the counter.
jnz initloop            ; Loop or else fall through.

;   ** Main
;      Do toggle the doors in memory.
;       The non optimized program output shows that the open
;      rooms are separed by a number of closed rooms that's
;      the number of doors opened so far multiplied by two.
;       The formula is:
;      p+(i*2)+1
;      where p is the last door found and i is the number of
;      doors opened so far. When recursively applied with
;      p=i=0 in the first step, this formula will produce a list
;      of perfect squares of integer numbers:
;      1=0+(0*2)+1
;      4=1+(1*2)+1
;      9=4+(2*2)+1
;      16=9+(3*2)+1
;       This program does use this approach for speeding the
;      calculations instead of calculating the perfect squares
;      by the usual method (x*x), to save the time needed to
;      reinitialize the index every time.

dodoor:
xor cx,cx               ; Set the CX register to 0000.
mov bx,offset doors-1   ; Set the pointer BX to the doors
                        ;address minus one because otherwise
                        ;zero would count as one ("doors"
                        ;points to the first door).
doorloop:
add bx,cx               ; Set the BX address to BX + (CX * 2) + 1
add bx,cx               ;This sets the address of the door to
inc bx                  ;open.
mov [bx],0FFh           ; Open the door overwriting his
                        ;byte with the new one.
inc cl                  ; Increment the counter register CX
cmp cl,0Ah              ; If cl is not 10 (in fact 11 because zero
jnz doorloop            ;counts), we didn't finish, then loop.

programend:             ; The above loop does fall here when it
                        ;ends.
cli                     ; Clear the interrupt flag, disabling
                        ;any software and most of the hardware
                        ;interrupts (the maskable interrupts).
hlt                     ; Freeze the processor.
jmp programend          ; If a Non Maskable Interrupt does pull
                        ;the CPU from his freezed state, do
                        ;freeze it again.

doors:                  ; Pointer to the end of the program,
                        ;where the RAM space is free to keep the
                        ;100 doors bytes.
```

The program could be written scrapping the init code and adding the initialized bytes directly in the source code, after the "doors" pointer, but the initialization code takes less space than 100 $FF bytes.
