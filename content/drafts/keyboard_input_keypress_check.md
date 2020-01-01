+++
title = "Keyboard input/Keypress check"
description = ""
date = 2019-10-10T19:45:31Z
aliases = []
[extra]
id = 8434
[taxonomies]
categories = []
tags = []
+++

{{task|Keyboard input}}
[[user input::task| ]]

Determine if a key has been pressed and store this in a variable.

If no key has been pressed, the program should continue without waiting.





## Ada



```Ada
Ch : Character;
Available : Boolean;

Ada.Text_IO.Get_Immediate (Ch, Available);
```


If key was pressed, Available is set to True and Ch contains the value.
If not, Available is set to False.

## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* Programme assembleur ARM Raspberry */
/* Assembleur ARM Raspberry  : Vincent Leboulou */
/* Blog : http://assembleurarmpi.blogspot.fr/  */
/* modèle B 512MO   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDIN,     0     @ Linux input console
.equ STDOUT,    1     @ Linux output console
.equ EXIT,      1     @ Linux syscall
.equ READ,      3     @ Linux syscall
.equ WRITE,     4     @ Linux syscall
.equ IOCTL,     0x36  @ Linux syscall
.equ SIGACTION, 0x43  @ Linux syscall
.equ SYSPOLL,   0xA8  @ Linux syscall

.equ TCGETS,    0x5401
.equ TCSETS,    0x5402
.equ ICANON,    2
.equ ECHO,     10
.equ POLLIN,    1

.equ SIGINT,   2    @ Issued if the user sends an interrupt signal (Ctrl + C)
.equ SIGQUIT,  3    @ Issued if the user sends a quit signal (Ctrl + D)
.equ SIGTERM, 15    @ Software termination signal (sent by kill by default)
.equ SIGTTOU, 22    @

.equ TAILLEBUFFER,   10


/*******************************************/
/* Structures                               */
/********************************************/
/* structure termios see doc linux*/
    .struct  0
term_c_iflag:                    @ input modes
    .struct  term_c_iflag + 4
term_c_oflag:                    @ output modes
    .struct  term_c_oflag + 4
term_c_cflag:                    @ control modes
    .struct  term_c_cflag + 4
term_c_lflag:                    @ local modes
    .struct  term_c_lflag + 4
term_c_cc:                       @ special characters
    .struct  term_c_cc + 20      @ see length if necessary
term_fin:

/* structure sigaction see doc linux */
    .struct  0
sa_handler:
    .struct  sa_handler + 4
sa_mask:
    .struct  sa_mask + 4
sa_flags:
    .struct  sa_flags + 4
sa_sigaction:
    .struct  sa_sigaction + 4
sa_fin:

/* structure poll see doc linux */
    .struct  0
poll_fd:                            @   File Descriptor
    .struct  poll_fd + 4
poll_events:                        @  events mask
    .struct  poll_events + 4
poll_revents:                       @ events returned
    .struct  poll_revents + 4
poll_fin:

/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessPgmOk:        .asciz "End program OK.\n"
szMessErreur:       .asciz "Error detected.\n"
sMessResult:        .ascii "Value  : "
sMessValeur:        .fill 11, 1, ' '            @ size => 11
szCarriageReturn:   .asciz "\n"
/*************************************************/
szMessErr: .ascii	"Error code hexa : "
sHexa: .space 9,' '
         .ascii "  decimal :  "
sDeci: .space 15,' '
         .asciz "\n"
szMessKey:             .ascii  "Value key ==>"
sHexaKey: .space 9,' '
        .asciz "\n"
.align 4

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
.align 4
iEnd:           .skip 4                        @ 0 loop  1 = end loop
iTouche:        .skip 4                        @ value key pressed
stOldtio:       .skip term_fin                 @ old terminal state
stCurtio:       .skip term_fin                 @ current terminal state
stSigAction:    .skip sa_fin                   @ area signal structure
stSigAction1:   .skip sa_fin
stPoll1:        .skip poll_fin                 @ area poll structure
stPoll2:        .skip poll_fin

/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                            @ entry of program

    /* read terminal state */
    mov r0,#STDIN                                @ input console
    mov r1,#TCGETS
    ldr r2,iAdrstOldtio
    mov r7, #IOCTL                               @ call system Linux
    svc #0
    cmp r0,#0                                    @ error ?
    beq 1f
    ldr r1,iAdrszMessErreur                      @ error message
    bl   displayError
    b 100f
1:
    adr r0,sighandler                            @ adresse routine traitement signal
    ldr r1,iAdrstSigAction                       @ adresse structure sigaction
    str r0,[r1,#sa_handler]                      @ maj handler
    mov r0,#SIGINT                               @ signal type
    ldr r1,iAdrstSigAction
    mov r2,#0                                    @ NULL
    mov r7, #SIGACTION                           @ call system
    svc #0
    cmp r0,#0                                    @ error ?
    bne 98f
    mov r0,#SIGQUIT
    ldr r1,iAdrstSigAction
    mov r2,#0                                    @ NULL
    mov r7, #SIGACTION                           @ call system
    svc #0
    cmp r0,#0                                    @ error ?
    bne 98f
    mov r0,#SIGTERM
    ldr r1,iAdrstSigAction
    mov r2,#0                                    @ NULL
    mov r7, #SIGACTION                           @ appel systeme
    svc #0
    cmp r0,#0
    bne 98f
    @
    adr r0,iAdrSIG_IGN                           @ address signal igonre function
    ldr r1,iAdrstSigAction1
    str r0,[r1,#sa_handler]
    mov r0,#SIGTTOU                              @invalidate other process signal
    ldr r1,iAdrstSigAction1
    mov r2,#0                                    @ NULL
    mov r7,#SIGACTION                            @ call system
    svc #0
    cmp r0,#0
    bne 98f
    @
    /* read terminal current state  */
    mov r0,#STDIN
    mov r1,#TCGETS
    ldr r2,iAdrstCurtio                          @ address current termio
    mov r7,#IOCTL                                @ call systeme
    svc #0
    cmp r0,#0                                    @ error ?
    bne 98f
    mov r2,#ICANON | ECHO                        @ no key pressed echo on display
    mvn r2,r2                                    @ and one key
    ldr r1,iAdrstCurtio
    ldr r3,[r1,#term_c_lflag]
    and r3,r2                                    @ add flags
    str r3,[r1,#term_c_lflag]                    @ and store
    mov r0,#STDIN                                @ maj terminal current state
    mov r1,#TCSETS
    ldr r2,iAdrstCurtio
    mov r7, #IOCTL                               @ call system
    svc #0
    cmp r0,#0
    bne 98f
    @
2:                                               @ loop waiting key
    ldr r0,iAdriEnd                              @ if signal ctrl-c  -> end
    ldr r0,[r0]
    cmp r0,#0
    bne 3f
    ldr r0,iAdrstPoll1                            @ address structure poll
    mov r1,#STDIN
    str r1,[r0,#poll_fd]                          @ maj FD
    mov r1,#POLLIN                                @ action code
    str r1,[r0,#poll_events]
    mov r1,#1                                     @ items number structure poll
    mov r2,#0                                     @ timeout = 0
    mov r7,#SYSPOLL                               @ call system POLL
    svc #0
    cmp r0,#0                                     @ key pressed ?
    ble 2b                                        @ no key pressed -> loop
                                                  @ read key
    mov r0,#STDIN                                 @ File Descriptor
    ldr r1,iAdriTouche                            @ buffer address
    mov r2,#TAILLEBUFFER                          @ buffer size
    mov r7,#READ                                  @ read key
    svc #0
    cmp r0,#0                                     @ error ?
    ble 98f
    ldr r2,iAdriTouche                            @ key address
    ldr r0,[r2]
    ldr r1,iAdrsHexaKey
    bl conversion16                               @ conversion hexa
    ldr r0,iAdrszMessKey                          @ display key value in hexa
    bl affichageMess
    ldrb r0,[r2]                                  @ key value
    cmp r0,#113                                   @ saisie q ?
    beq 3f
    cmp r0,#81                                    @ saisie Q ?
    beq 3f
    mov r0,#0
    str r0,[r2]                                   @ raz key area
    b 2b                                          @ loop

3:                                                @ end loop display Ok message
    ldr r0,iAdrszMessPgmOk
    bl affichageMess
    b 99f
98:                                               @ error display
    ldr r1,iAdrszMessErreur                       @ error message
    bl   displayError
99:                                               @ end then restaur begin state terminal
    mov r0,#STDIN
    mov r1,#TCSETS
    ldr r2,iAdrstOldtio
    mov r7,#IOCTL                                 @ call system
    svc #0
    cmp r0,#0
    beq 100f
    ldr r1,iAdrszMessErreur                       @ error message
    bl   displayError

100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszMessErreur:         .int szMessErreur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrstOldtio:             .int stOldtio
iAdrstCurtio:             .int stCurtio
iAdrstSigAction:          .int stSigAction
iAdrstSigAction1:         .int stSigAction1
iAdrszMessPgmOk:          .int szMessPgmOk
iAdrsMessResult:          .int sMessResult
iAdrSIG_IGN:              .int 1
iAdriEnd:                 .int iEnd
iAdrstPoll1:              .int stPoll1
iAdriTouche:              .int iTouche
iAdrszMessKey:            .int szMessKey
iAdrsHexaKey:             .int sHexaKey
/******************************************************************/
/*     traitement du signal                                       */
/******************************************************************/
/* r0 contains  */
sighandler:
    push {r0,r1}
    ldr r0,iAdriEnd
    mov r1,#1                 @ maj zone end
    str r1,[r0]
    pop {r0,r1}
    bx lr
/***************************************************/
/*   display error message                         */
/***************************************************/
/* r0 contains error code  r1 : message address */
displayError:
    push {r0-r2,lr}                            @ save registers
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
    pop {r0-r2,lr}                             @ restaur registers
    bx lr                                   @ return
iAdrszMessErr:                 .int szMessErr
iAdrsHexa:                     .int sHexa
iAdrsDeci:                     .int sDeci
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
    pop {r0,r1,r2,r7,lr}                           @ restaur registers
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


```



## AutoHotkey

Waits for the user to type a string (not supported on Windows 9x: it does nothing).

```AutoHotkey
; Input [, OutputVar, Options, EndKeys, MatchList]
Input, KeyPressed, L1 T2    ; Length = 1, Timeout = 2 seconds
```



Checks if a keyboard key or mouse/joystick button is down or up. Also retrieves joystick status.

```AutoHotkey
; GetKeyState, OutputVar, KeyName [, Mode]
GetKeyState, State, RButton    ; Right mouse button.
```



Function version of GetKeyState.

```AutoHotkey
; KeyIsDown := GetKeyState("KeyName" [, "Mode"])
State := GetKeyState("RButton", "P")    ; Right mouse button. P = Physical state.
```



## Axe

Note that while the syntax for getting the most recent keypress is identical to [[#TI-83_BASIC|TI-83 BASIC]], the keycodes themselves are different.

```axe
getKey→K
```



## BASIC


=
## BaCon
=

```freebasic

PRINT "Press <escape> to exit now..."
key = GETKEY
IF key = 27 THEN
    END
END IF
```


=
## Applesoft BASIC
=

```ApplesoftBasic
K = PEEK(-16384) : IF K > 127 THEN POKE -16368,0 : K$ = CHR$(K)
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET K$=INKEY$
```


or

<lang IS-BASIC>100 GET K$
```


=
## ZX Spectrum Basic
=
{{works with|Locomotive Basic}}


```zxbasic
10 REM k$ will be empty, if no key has been pressed
20 LET k$ = INKEY$
```


=
## BBC BASIC
=

```bbcbasic
      key$ = INKEY$(0)
```

If there was no keypress an empty string is returned.  Alternatively a numeric key-code may be obtained; if there was no keypress -1 is returned:

```bbcbasic
      key% = INKEY(0)
```



## C

For POSIX systems. Ctrl-C to stop:
```c
#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>

void set_mode(int want_key)
{
	static struct termios old, new;
	if (!want_key) {
		tcsetattr(STDIN_FILENO, TCSANOW, &old);
		return;
	}

	tcgetattr(STDIN_FILENO, &old);
	new = old;
	new.c_lflag &= ~(ICANON | ECHO);
	tcsetattr(STDIN_FILENO, TCSANOW, &new);
}

int get_key()
{
	int c = 0;
	struct timeval tv;
	fd_set fs;
	tv.tv_usec = tv.tv_sec = 0;

	FD_ZERO(&fs);
	FD_SET(STDIN_FILENO, &fs);
	select(STDIN_FILENO + 1, &fs, 0, 0, &tv);

	if (FD_ISSET(STDIN_FILENO, &fs)) {
		c = getchar();
		set_mode(0);
	}
	return c;
}

int main()
{
	int c;
	while(1) {
		set_mode(1);
		while (!(c = get_key())) usleep(10000);
		printf("key %d\n", c);
	}
}
```


## C#

```c#
string chr = string.Empty;
if(Console.KeyAvailable)
  chr = Console.ReadKey().Key.ToString();
```



## Clojure

{{libheader|jline}}

Note: If you run it with Leiningen, use the special trampoline run to prevent issues:


```txt
$ lein trampoline run
```



```clojure

(ns keypress.core
  (:import jline.Terminal)
  (:gen-class))

(def keypress (future (.readCharacter (Terminal/getTerminal) System/in)))

(defn prompt []
  (println "Awaiting char...\n")
  (Thread/sleep 2000)
  (if-not (realized? keypress)
    (recur)
    (println "key: " (char @keypress))))

(defn -main [& args]
  (prompt)
  (shutdown-agents))

```



## D


```d
extern (C) {
    void _STI_conio();
    void _STD_conio();
    int kbhit();
    int getch();
}

void main() {
    _STI_conio();

    char c;
    if (kbhit())
        c = cast(char)getch();

    _STD_conio();
}
```



## Delphi

This is valid for a GUI application!

```Delphi
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TForm1 = class(TForm)
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    SavedPressedKey: Char;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  SavedPressedKey := Key;
end;

end.
```


## Elena

{{trans|C#}}
ELENA 4.x :

```elena
import extensions;

public program()
{
    auto chr := emptyString;
    if (console.KeyAvailable)
    {
        chr := console.readChar()
    }
}
```



## ERRE


```ERRE

!$KEY
.........
GET(K$)
.........

```

Note: If no key was pressed K$ is empty string "".


## Euphoria


```Euphoria
integer key
key = get_key() -- if key was not pressed get_key() returns -1
```



=={{header|F_Sharp|F#}}==
{{trans|C#}}

```fsharp
open System;

let chr = if Console.KeyAvailable then Console.ReadKey().Key.ToString() else String.Empty
```



## Forth


```forth
variable last-key
: check   key? if key last-key ! then ;
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim k As String
Do
  k = Inkey
Loop Until k <> ""

If Len(k) = 1 Then
  Print "The key pressed was "; k; " (ascii "; Asc(k); ")"
Else
  Print "An extended key was pressed"
End If

Sleep
```


Sample input/output

{{out}}

```txt

The key pressed was A (ascii 65)

```



## Gambas


```gambas
Public Sub Form_KeyPress()

'Requires a TextBox or similar on the Form to work
Print Key.Text;

End
```

Output:

```txt

Hello world!

```



## Go

{{libheader|Curses}}

```go
package main

import (
    "log"
    "time"

    gc "code.google.com/p/goncurses"
)

func main() {
    s, err := gc.Init()
    if err != nil {
        log.Fatal("init:", err)
    }
    defer gc.End()
    gc.Cursor(0)
    s.Move(20, 0)
    s.Print("Key check in ")
    for i := 3; i >= 1; i-- {
        s.MovePrint(20, 13, i)
        s.Refresh()
        time.Sleep(500 * time.Millisecond)
    }
    s.Println()
    gc.Echo(false)

    // task requirement next two lines
    s.Timeout(0)
    k := s.GetChar()

    if k == 0 {
        s.Println("No key pressed")
    } else {
        s.Println("You pressed", gc.KeyString(k))
    }
    s.Refresh()
    s.Timeout(-1)
    gc.FlushInput()
    gc.Cursor(1)
    s.GetChar()
}
```



## Haskell


```haskell
import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.IO

main = do
    c <- newEmptyMVar
    hSetBuffering stdin NoBuffering
    forkIO $ do
      a <- getChar
      putMVar c a
      putStrLn $ "\nChar '" ++ [a] ++
                 "' read and stored in MVar"
    wait c
  where wait c = do
          a <- tryTakeMVar c
          if isJust a then return ()
          else putStrLn "Awaiting char.." >>
               threadDelay 500000 >> wait c

```

Output:

```txt

Awaiting char..
Awaiting char..
Awaiting char..
d
Char 'd' read and stored in MVar

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
   delay(1000)                              # give user a chance to input
   if kbhit() then                          # test for input
      write("You entered ",x := getch())    # read w/o echo
   else                                     # use getche for echo
      write("No input waiting")
end
```



## Java

{{works with|java|8}}

```java
import java.awt.event.*;
import javax.swing.*;

public class Test extends JFrame {

    Test() {
        addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                int keyCode = e.getKeyCode();
                System.out.println(keyCode);
            }
        });
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            Test f = new Test();
            f.setFocusable(true);
            f.setVisible(true);
        });
    }
}
```



## Julia


```Julia
using Gtk

function keypresswindow()
    tcount = 0
    txt = "Press a Key"
    win = GtkWindow("Keypress Test", 500, 30) |> (GtkFrame() |> ((vbox = GtkBox(:v)) |> (lab = GtkLabel(txt))))
    function keycall(w, event)
        ch = Char(event.keyval)
        tcount += 1
        set_gtk_property!(lab, :label, "You have typed $tcount chars including $ch this time")
    end
    signal_connect(keycall, win, "key-press-event")

    cond = Condition()
    endit(w) = notify(cond)
    signal_connect(endit, win, :destroy)
    showall(win)
    wait(cond)
end

keypresswindow()

```




## Kotlin

Translated from the Java entry but then modified so as to quit the program when the Enter key is pressed:

```scala
// version 1.1

import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import javax.swing.JFrame
import javax.swing.SwingUtilities

class Test : JFrame() {
    init {
        println("Press any key to see its code or 'enter' to quit\n")
        addKeyListener(object : KeyAdapter() {
            override fun keyPressed(e: KeyEvent) {
                if (e.keyCode == KeyEvent.VK_ENTER) {
                    isVisible = false
                   dispose()
                   System.exit(0)
                }
                else
                   println(e.keyCode)
            }
        })
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = Test()
        f.isFocusable = true
        f.isVisible = true
    }
}
```



## Lingo


```lingo
-- in some movie script

-- event handler
on keyDown
  pressedKey = _key.key
  put "A key was pressed:" && pressedKey
end
```



## LiveCode

LiveCode is message based and all stacks, cards and gui controls can have their own keyup/down message handler. You would ordinarily add the relevant event handler to do something when a key is pressed. There is a function however that can be executed that returns a list of keycodes for the keys currently pressed, called keysDown.

Example

```LiveCode
repeat 100 times
-- exit loop if "." or the escapeKey is pressed
    if 46 is in the keysDown or 65307 is in the keysdown then
        answer "exiting"
        exit repeat
    else
        -- do stuff
        wait 200 millisec
    end if
end repeat
```

Example of event message handling (at stack, card or control level)

```LiveCode
on keyDown k
-- do stuff, keycode is held in k
if k is not 46 then pass keyDown  // will be trapped if "." is pressed, others will be passed on through the message path
end keyDown
```

You can substitute keyUp, rawKeyUp, rawKeyDown for keyUp in above. The non-raw handlers do not readily cope with special key presses, and they have their own handlers such as escapeKey, enterKey, altkey, commandKey... look up "key" in the LC dictionary to find more.


## Logo


```logo
if key? [make "keyhit readchar]
```



## M2000 Interpreter


### Without delay


```M2000 Interpreter

k$=inkey$

```



### Using delay


```M2000 Interpreter

K$=""
If Inkey(2000)<>-1 then k$=Key$
Print k$

```



### Check specific key


```M2000 Interpreter

k$=""
If keypress(32) then k$=" "

```


## Oforth


```Oforth
import: console

: checkKey
| key |
   System.Console receiveTimeout(2000000) ->key   // Wait a key pressed for 2 seconds
   key ifNotNull: [ System.Out "Key pressed : " << key << cr ]
   "Done" println ;
```


Other options :

```Oforth
System.Console receive ->key                 // Wait until a key is pressed ( = receiveTimeout(null) )
System.Console receiveChar ->aChar           // Wait until a character is pressed. All other keys are ignored
System.Console receiveTimeout(0) ->key       // Check if a key is pressed and return immediatly
```



## Perl


```perl
#!/usr/bin/perl
use strict;
use warnings;
use Term::ReadKey;
ReadMode 4;
my $key;
until(defined($key = ReadKey(-1))){
	# anything
	sleep 1;
}
print "got key '$key'\n";
ReadMode('restore');
```


In many cases one does not want to wait for each step end. In this case you can use two parallel processes:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Carp;
use POSIX;
use Term::ReadKey;
$| = 1; # don't buffer; stdout is hot now

#avoid creation of zombies
sub _cleanup_and_die{
	ReadMode('restore');
	print "process $$ dying\n";
	die;
}
sub _cleanup_and_exit{
	ReadMode('restore');
	print "process $$ exiting\n";
	exit 1;
}
$SIG{'__DIE__'} = \&_cleanup_and_die;
$SIG{'INT'}     = \&_cleanup_and_exit;
$SIG{'KILL'}    = \&_cleanup_and_exit;
$SIG{'TERM'}    = \&_cleanup_and_exit;
$SIG{__WARN__}  = \&_warn;

# fork into two processes:
# child process is doing anything
# parent process just listens to keyboard input
my $pid = fork();
if(not defined $pid){
	print "error: resources not available.\n";
	die "$!";
}elsif($pid == 0){ # child
	for(0..9){
		print "doing something\n";
		sleep 1;
	}
	exit 0;
}else{ # parent
	ReadMode('cbreak');
	# wait until child has exited/died
	while(waitpid($pid, POSIX::WNOHANG) == 0){
		my $seq = ReadKey(-1);
		if(defined $seq){
			print "got key '$seq'\n";
		}
		sleep 1; # if ommitted, the cpu-load will reach up to 100%
	}
	ReadMode('restore');
}
```


This code prints <code>"doing something"</code> 10 times and then ends. Parallelly another process prints every key you type in.


## Perl 6

{{works with|Rakudo|2018.10}}


```perl6
use Term::ReadKey;

react {
    whenever key-pressed(:!echo) {
        given .fc {
            when 'q' { done }
            default { .uniname.say }
        }
    }
}
```



## Phix


```Phix
integer key = get_key() -- if key was not pressed get_key() returns -1
```



## PicoLisp


```PicoLisp
(setq *LastKey (key))
```



## PowerShell

The following uses the special <code>$Host</code> variable which points to an instance of the PowerShell host application. Since the host's capabilities may vary this may not work in all PowerShell hosts. In particular, this works in the console host, but not in the PowerShell ISE.

```powershell
if ($Host.UI.RawUI.KeyAvailable) {
    $key = $Host.UI.RawUI.ReadKey()
}
```



## Python


```python
#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, unicode_literals, print_function

import tty, termios
import sys
if sys.version_info.major < 3:
    import thread as _thread
else:
    import _thread
import time


try:
    from msvcrt import getch  # try to import Windows version
except ImportError:
    def getch():   # define non-Windows version
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)
        try:
            tty.setraw(sys.stdin.fileno())
            ch = sys.stdin.read(1)
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        return ch

def keypress():
    global char
    char = getch()

def main():
    global char
    char = None
    _thread.start_new_thread(keypress, ())

    while True:
        if char is not None:
            try:
                print("Key pressed is " + char.decode('utf-8'))
            except UnicodeDecodeError:
                print("character can not be decoded, sorry!")
                char = None
            _thread.start_new_thread(keypress, ())
            if char == 'q' or char == '\x1b':  # x1b is ESC
                exit()
            char = None
        print("Program is running")
        time.sleep(1)

if __name__ == "__main__":
    main()

```



## PureBasic

Returns a character string if a key is pressed during the call of Inkey(). It doesn't interrupt (halt) the program flow.

If special keys (non-ASCII) have to be handled, RawKey() should be called after Inkey().

```PureBasic
k$ = Inkey()
```



## Racket


Using <tt>stty</tt> to get the terminal into raw mode.


```racket

#lang racket
(define-syntax-rule (with-raw body ...)
  (let ([saved #f])
    (define (stty x) (system (~a "stty " x)) (void))
    (dynamic-wind (λ() (set! saved (with-output-to-string (λ() (stty "-g"))))
                       (stty "raw -echo opost"))
                  (λ() body ...)
                  (λ() (stty saved)))))

(with-raw
  (printf "Press a key, or not\n")
  (sleep 2)
  (if (char-ready?)
    (printf "You pressed ~a\n" (read-char))
    (printf "You didn't press a key\n")))

```



## REXX

The REXX language doesn't have any keyboard tools, but some REXX interpreters have added the functionality via different methods.

This version   ''only''   works with:
:::* PC/REXX
:::* Personal REXX

```rexx
/*REXX program demonstrates if any key has been presssed.               */

  ∙
  ∙
  ∙
somechar=inkey('nowait')
  ∙
  ∙
  ∙
```



## Ring


```ring

if getchar() see "A key was pressed" + nl
else see "No key was pressed" + nl ok

```



## Robotic


```robotic

: "loop"
if "KEY_PRESSED" != 0 then "#store"
* "Last key pressed: &storedKey&"
goto "loop"

: "#store"
set "storedKey" to "KEY_PRESSED"
goto "#return"

```



## Ruby


```ruby

begin
  check = STDIN.read_nonblock(1)
rescue IO::WaitReadable
  check = false
end

puts check if check

```

Test in unix shell:

```bash

% ruby keypress_check.rb
% echo -n y | ruby keypress_check.rb
y

```




## Scala


```Scala
import java.awt.event.{KeyAdapter, KeyEvent}

import javax.swing.{JFrame, SwingUtilities}

class KeypressCheck() extends JFrame {

  addKeyListener(new KeyAdapter() {
    override def keyPressed(e: KeyEvent): Unit = {
      val keyCode = e.getKeyCode
      if (keyCode == KeyEvent.VK_ENTER) {
        dispose()
        System.exit(0)
      }
      else
        println(keyCode)
    }
  })
}

object KeypressCheck extends App {
  println("Press any key to see its code or 'enter' to quit\n")
  SwingUtilities.invokeLater(() => {
    def foo() = {
      val f = new KeypressCheck
      f.setFocusable(true)
      f.setVisible(true)
      f.setSize(200, 200)
      f.setEnabled(true)
    }

    foo()
  })
}
```


## Seed7

The library [http://seed7.sourceforge.net/libraries/keybd.htm keybd.s7i] defines
the file [http://seed7.sourceforge.net/libraries/keybd.htm#KEYBOARD KEYBOARD] and
the function [http://seed7.sourceforge.net/libraries/keybd.htm#keypressed%28in_keyboard_file%29 keypressed],
which can be used to determine if a key has been pressed.

```seed7
if keypressed(KEYBOARD) then
  writeln("A key was pressed");
else
  writeln("No key was pressed");
end if;
```



## Tcl

There are two ways to handle listening for a key from the terminal. The first is to put the channel connected to the terminal into non-blocking mode and do a <code>read</code> on it:

```tcl
fconfigure stdin -blocking 0
set ch [read stdin 1]
fconfigure stdin -blocking 1

if {$ch eq ""} {
    # Nothing was read
} else {
    # Got the character $ch
}
```

The second method is to set up an event listener to perform callbacks when there is at least one character available:

```tcl
fileevent stdin readable GetChar
proc GetChar {} {
    set ch [read stdin 1]
    if {[eof stdin]} {
        exit
    }
    # Do something with $ch here
}

vwait forever; # run the event loop if necessary
```

Note that in both cases, if you want to get characters as users actually type them then you have to [http://wiki.tcl.tk/14693 put the terminal in raw mode]. That's formally independent of the actual reading of a character.

=={{header|TI-83 BASIC}}==
TI-83 BASIC has a built in getKey function.

```ti83b

:getKey→G

```

This returns the key code of the key pressed which is the row number followed by the column number. The left up and down arrow keys are grouped with row 2 as 24, 25, and 26, and the down arrow key is grouped with row 3 as 34.


## Wee Basic

This returns the key code of the key pressed.

```Wee Basic
let keycode=0
print 1 "Press any key and its key code will appear."
while keycode=0
let keycode=key()
wend
print 1 keycode
end
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
int K, N;
[N:= 0;
repeat  K:= KeyHit;     \counts up until a key is pressed
        IntOut(0, N);  ChOut(0, ^ );
        N:= N+1;
until   K;
]
```


{{omit from|ACL2}}
{{omit from|GUISS}}
{{omit from|PARI/GP}}

[[Category:Simple]]
[[Category:Keyboard input]]
[[Category:Terminal control]]
