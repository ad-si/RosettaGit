+++
title = "Simulate input/Keyboard"
description = ""
date = 2019-02-13T00:15:55Z
aliases = []
[extra]
id = 4298
[taxonomies]
categories = ["task", "GUI"]
tags = []
languages = [
  "autohotkey",
  "autoit",
  "c",
  "clojure",
  "go",
  "guiss",
  "java",
  "kotlin",
  "labview",
  "ocaml",
  "oz",
  "perl",
  "perl_6",
  "picolisp",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "rust",
  "scala",
  "tcl",
  "vbscript",
]
+++

## Task

{{task|GUI}}[[Category:Testing]]
Send simulated keystrokes to a GUI window, or terminal.
You should specify whether the target may be externally created
(i.e., if the keystrokes are going to an application
other than the application that is creating them).


## AutoHotkey

Target may be externally created.

```AutoHotkey
run, cmd /k
WinWait, ahk_class ConsoleWindowClass
controlsend, ,hello console, ahk_class ConsoleWindowClass
```



## AutoIt

Code assumes you're working on a windows box. Run() can use any program, and WinWaitActive() requires the title of the program as it will be when it opens.

```AutoIt
Run("notepad")
WinWaitActive("Untitled - Notepad")
Send("The answer is 42")
```


It takes user input in variable using "input box"
and displays that in "message box"

```AutoIt
$name="type your name here"
$name = InputBox("Name","Your name please ?",$name)
MsgBox(0,"Name","Your name is: "&$name)
```



## C


Compile with:
 gcc -o simkeypress -L/usr/X11R6/lib -lX11 simkeypress.c


```c
#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

int main(int argc, char *argv[])
{
  Display *dpy;
  Window win;
  GC gc;
  int scr;
  Atom WM_DELETE_WINDOW;
  XEvent ev;
  XEvent ev2;
  KeySym keysym;
  int loop;

  /* open connection with the server */
  dpy = XOpenDisplay(NULL);
  if (dpy == NULL) {
    fputs("Cannot open display", stderr);
    exit(1);
  }
  scr = XDefaultScreen(dpy);

  /* create window */
  win = XCreateSimpleWindow(dpy,
          XRootWindow(dpy, scr),
          /* x, y, width, height, border_width */
          10, 10, 300, 200, 1,
          /* border, background */
          XBlackPixel(dpy, scr), XWhitePixel(dpy, scr));

  /* set window name */
  XStoreName(dpy, win, argv[0]);

  /* select kind of events we are interested in */
  XSelectInput(dpy, win, ExposureMask | KeyPressMask | ButtonPressMask);

  /* map (show) the window */
  XMapWindow(dpy, win);
  XFlush(dpy);

  /* default graphics context */
  gc = XDefaultGC(dpy, scr);

  /* connect the close button in the window handle */
  WM_DELETE_WINDOW = XInternAtom(dpy, "WM_DELETE_WINDOW", True);
  XSetWMProtocols(dpy, win, &WM_DELETE_WINDOW, 1);

  /* event loop */
  loop = 1;
  while (loop) {
    XNextEvent(dpy, &ev);
    switch (ev.type)
    {
      case Expose:
        /* draw or redraw the window */
        {
          char msg1[] = "Clic in the window to generate";
          char msg2[] = "a key press event";
          XDrawString(dpy, win, gc, 10, 20, msg1, sizeof(msg1)-1);
          XDrawString(dpy, win, gc, 10, 35, msg2, sizeof(msg2)-1);
        }
        break;

      case ButtonPress:
        puts("ButtonPress event received");
        /*
        printf("> button (x,y) : %d %d\n",
               ev.xbutton.x,
               ev.xbutton.y);
        */
        ev2.type = KeyPress;
        ev2.xkey.state = ShiftMask;
        ev2.xkey.keycode = 24 + (rand() % 33);
        ev2.xkey.same_screen = True;
        XSendEvent(dpy, win, True/*propagate*/, KeyPressMask, &ev2);
        break;

      case ClientMessage:
        /* delete window event */
        if (ev.xclient.data.l[0] == WM_DELETE_WINDOW)
          loop = 0;
        break;

      case KeyPress:
        /* handle key press */
        puts("KeyPress event received");
        printf("> keycode: %d\n", ev.xkey.keycode);
        /* exit if q or escape are pressed */
        keysym = XLookupKeysym(&(ev.xkey), 0);
        if (keysym == XK_q ||
            keysym == XK_Escape) {
          loop = 0;
        } else {
          char buffer[] = "  ";
          int nchars = XLookupString(
                &(ev.xkey),
                buffer,
                2,  /* buffer size */
                &keysym,
                NULL );
          if (nchars == 1)
            printf("> Key '%c' pressed\n", buffer[0]);
        }
        break;
    }
  }
  XDestroyWindow(dpy, win);
  /* close connection to server */
  XCloseDisplay(dpy);
  return 1;
}
```



## Clojure

```Clojure
(import java.awt.Robot)
(import java.awt.KeyEvent)
(defn keytype [str]
  (let [robot (new Robot)]
       (doseq [ch str]
	      (if (Character/isUpperCase ch)
		  (doto robot
			(.keyPress (. KeyEvent VK_SHIFT))
			(.keyPress (int ch))
			(.keyRelease (int ch))
			(.keyRelease (. KeyEvent VK_SHIFT)))
		  (let [upCh (Character/toUpperCase ch)]
		       (doto robot
			     (.keyPress (int upCh))
			     (.keyRelease (int upCh))))))))
```



## Go

Should also work on Windows 10 though I haven't tested it.

The program runs a directory listing by sending the keys 'd', 'i', 'r', 'enter' to the terminal.

N.B. You may need to execute: 'sudo chmod +0666 /dev/uinput' first if running on Linux.

```go
package main

import (
    "github.com/micmonay/keybd_event"
    "log"
    "runtime"
    "time"
)

func main() {
    kb, err := keybd_event.NewKeyBonding()
    if err != nil {
        log.Fatal(err)
    }

    // For linux, need to wait 2 seconds
    if runtime.GOOS == "linux" {
        time.Sleep(2 * time.Second)
    }

    //set keys
    kb.SetKeys(keybd_event.VK_D, keybd_event.VK_I, keybd_event.VK_R, keybd_event.VK_ENTER)

    //launch
    err = kb.Launching()
    if err != nil {
        log.Fatal(err)
    }
}
```



## GUISS



```guiss
Start,Programs,Accessories,Notepad,Textbox,Type:Hello World[pling]
```



## Java

Keystrokes when this function is executed will go to whatever application has focus at the time. Special cases may need to be made for certain symbols, but most of the VK values in KeyEvent map to the ASCII values of characters.

```java5
import java.awt.Robot
public static void type(String str){
   Robot robot = new Robot();
   for(char ch:str.toCharArray()){
      if(Character.isUpperCase(ch)){
         robot.keyPress(KeyEvent.VK_SHIFT);
         robot.keyPress((int)ch);
         robot.keyRelease((int)ch);
         robot.keyRelease(KeyEvent.VK_SHIFT);
      }else{
         char upCh = Character.toUpperCase(ch);
         robot.keyPress((int)upCh);
         robot.keyRelease((int)upCh);
      }
   }
}
```



## Kotlin


```scala
// version 1.1.2

import java.awt.Robot
import java.awt.event.KeyEvent

fun sendChars(s: String, pressReturn: Boolean = true) {
    val r = Robot()
    for (c in s) {
        val ci = c.toUpperCase().toInt()
        r.keyPress(ci)
        r.keyRelease(ci)
    }
    if (pressReturn) {
        r.keyPress(KeyEvent.VK_ENTER)
        r.keyRelease(KeyEvent.VK_ENTER)
    }
}

fun main(args: Array<String>) {
    sendChars("dir")  // runs 'dir' command
}
```



## LabVIEW

Uses .NET for simplicity. Run it with Highlight Execution on, to give notepad time to open. {{VI solution|LabVIEW_Simulate_input_Keyboard.png}}


## OCaml


This example uses the Xlib to create a window, then when the user clics in this window an <code>XKeyPressedEvent</code> is sent with the function <code>xSendEvent</code>.

run with:
 ocaml -I +Xlib Xlib.cma keysym.cma send_event.ml


```ocaml
open Xlib

let () =
  (* open connection with the server *)
  let d = xOpenDisplay "" in
  let s = xDefaultScreen d in

  Random.self_init();

  (* create window *)
  let w = xCreateSimpleWindow d (xRootWindow d s) 10 10 300 200 1
                                (xBlackPixel d s) (xWhitePixel d s) in

  (* set window name *)
  xStoreName d w Sys.argv.(0);

  (* select kind of events we are interested in *)
  xSelectInput d w [ExposureMask; KeyPressMask; ButtonPressMask];

  (* map (show) the window *)
  xMapWindow d w;
  xFlush d;

  let dbl = w in
  let gc = xDefaultGC d s in

  (* connect the close button in the window handle *)
  let wm_delete_window = xInternAtom d "WM_DELETE_WINDOW" true in
  xSetWMProtocols d w wm_delete_window 1;

  (* event loop *)
  let e = new_xEvent() in
  try while true do
    xNextEvent d e;

    (* draw or redraw the window *)
    match xEventKind e with
    | XExposeEvent _ ->
        xDrawString d dbl gc 10 20 "Clic in the window to generate";
        xDrawString d dbl gc 10 35 "a key press event";

    | XButtonPressedEvent event ->
        let dat = xButtonEvent_datas event in
        (*
        Printf.printf "button x,y : %d %d\n%!"
            dat.button_x
            dat.button_y;
        *)
        let xKeyEvent_contents = {
            key_serial     = dat.button_serial;
            key_send_event = dat.button_send_event;
            key_display    = dat.button_display;
            key_window     = dat.button_window;
            key_root       = dat.button_root;
            key_subwindow  = dat.button_subwindow;
            key_time       = dat.button_time;
            key_x          = dat.button_x;
            key_y          = dat.button_y;
            key_x_root     = dat.button_x_root;
            key_y_root     = dat.button_y_root;

            key_state = [ShiftMask];
            key_keycode = (24 + Random.int 33);
            key_same_screen = true;
        } in
        let propagate = true in
        let event_mask = KeyPressMask in
        xSendEvent d w propagate event_mask (XKeyPressedEvCnt xKeyEvent_contents);

    (* delete window event *)
    | XClientMessageEvent xclient ->
        let atom = xEvent_xclient_data xclient in
        if atom = wm_delete_window then
          raise Exit

    (* handle key press *)
    | XKeyPressedEvent event ->
        print_endline "Key Pressed Event";
        begin
          let d = xKeyEvent_datas event in
          Printf.printf "keycode: %d\n%!" d.key_keycode;
        end;
        (* exit if q or escape are pressed *)
        let keysym = xLookupKeysym event 0 in
        if keysym = Keysym.xK_q ||
           keysym = Keysym.xK_Escape then
          raise Exit
        else
          let printable, c =
            let buf = "  " in
            let n, _ = xLookupString event buf in
            if (n = 1)
            then (true, buf.[0])
            else (false, '\000')
          in
          if printable then
            Printf.printf "Key '%c' pressed\n%!" c;

    | _ -> ()
  done with
  | Exit ->
      xDestroyWindow d w;
      (* close connection to server *)
      xCloseDisplay d;
;;
```



## Oz

Oz' default GUI toolkit is based on Tk. So we can do the same thing as in Tcl:

```oz
declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}
  Entry
  Window = {QTk.build td(entry(handle:Entry))}
in
  {Window show}
  {Entry getFocus(force:true)}

  for C in "Hello, world!" do
     Key = if C == 32 then "<space>" else [C] end
  in
     {Delay 100}
     {Tk.send event(generate Entry Key)}
  end
```


This only works with internal windows.



## Perl

Perl on linux can do this for PseudoTerminals (ptys) and Terminals (ttys) using [http://www.kernel.org/doc/man-pages/online/pages/man4/tty_ioctl.4.html IOCTL TIOCSTI].  The same can be done with C or any other language that can use IOCTL either natively or via [http://en.wikipedia.org/wiki/Foreign_function_interface FFI] such as [http://en.wikipedia.org/wiki/Java_Native_Interface JNI] or [http://en.wikipedia.org/wiki/Java_Native_Access JNA].

Target may be externally created, but process must be able to open tty/pty for writing.


```perl
$target = "/dev/pts/51";
### How to get the correct value for $TIOCSTI is discussed here : http://www.perlmonks.org/?node_id=10920
$TIOCSTI = 0x5412 ;
open(TTY,">$target") or die "cannot open $target" ;
$b="sleep 99334 &\015";
@c=split("",$b);
sleep(2);
foreach $a ( @c ) { ioctl(TTY,$TIOCSTI,$a); select(undef,undef,undef,0.1);} ;
print "DONE\n";
```




Perl on X11 can do this using the SendKeys function from [http://search.cpan.org/%7ectrondlp/X11-GUITest/GUITest.pm X11::GUITest]

Perl on Windows can do this using the SendKeys function from [http://search.cpan.org/%7ekarasik/Win32-GuiTest/lib/Win32/GuiTest.pm  Win32::GUITest]

Target may be externally created.


```perl
SendKeys("Hello, how are you?\n");
```



## Perl 6

Use libxdo bindings to send text / keystrokes to any application that will accept keystrokes from X11.


```perl6
use X11::libxdo;

my $xdo = Xdo.new;

my $active = $xdo.get-active-window;

my $command = $*VM.config<os> eq 'darwin' ?? 'open' !! 'xdg-open';

shell "$command https://www.google.com";

sleep 1;

my $match = rx[^'Google -'];

say my $w = $xdo.search(:name($match))<ID>;

sleep .25;

if $w {
    $xdo.activate-window($w);
    say "Window name: ", $xdo.get-window-name( $w );
    $xdo.type($w, 'Perl 6');
    sleep .25;
    $xdo.send-sequence($w, 'Tab');
    sleep .5;
    $xdo.send-sequence($w, 'Tab');
    sleep .5;
    $xdo.send-sequence($w, 'Tab');
    sleep .5;
    $xdo.send-sequence($w, 'Return');
}
```



## PicoLisp

PicoLisp comes with a dedicated browser GUI. A library based on web scraping (in
"lib/scrape.l") can be used to drive that GUI under program control. It allows
to read GUI pages, click on HTML links, enter text into forms, and press submit
buttons. In that way one application can control another application.

The documented [http://software-lab.de/doc/app.html#minApp demo application],
which is also available online at [http://7fach.de/8080 app.7fach.de], is used
in the following example. Keyboard input is simulated with the function 'enter'
to fill the login form's name and password fields.

```PicoLisp
(load "@lib/http.l" "@lib/scrape.l")

# Connect to the demo app at http://7fach.de/8080
(scrape "7fach.de" 80 "8080")

# Log in
(expect "'admin' logged in"
   (enter 3 "admin")       # Enter user name into 3rd field
   (enter 4 "admin")       # Enter password into 4th field
   (press "login") )       # Press the "login" button

(click "Items")         # Open "Items" dialog
(click "Spare Part")    # Click on "Spare Part" article
(prinl (value 8))       # Print the price (12.50)
(click "logout")        # Log out
```

```txt
12.50
```

The same example is used in the related task [[Simulate input/Mouse#PicoLisp]].


## PowerShell

The Start-Sleep CmdLet must be used because no application loads instantaneously.  The -Milliseconds parameter should be
adjusted accordingly for every application.

```PowerShell

Add-Type -AssemblyName Microsoft.VisualBasic
Add-Type -AssemblyName System.Windows.Forms
calc.exe
Start-Sleep -Milliseconds 300
[Microsoft.VisualBasic.Interaction]::AppActivate(“Calc”)
[System.Windows.Forms.SendKeys]::SendWait(“2{ADD}2=”)

```



## PureBasic

```PureBasic
If AW_WinActivate("Calc")
  AW_SendKeys("123+3=")
EndIf
```



## Python

```Python
import autopy
autopy.key.type_string("Hello, world!") # Prints out "Hello, world!" as quickly as OS will allow.
autopy.key.type_string("Hello, world!", wpm=60) # Prints out "Hello, world!" at 60 WPM.
autopy.key.tap(autopy.key.Code.RETURN)
autopy.key.tap(autopy.key.Code.F1)
autopy.key.tap(autopy.key.Code.LEFT_ARROW)
```


Target may be externally created.


```Python
>>>
 import pyautogui
>>> pyautogui.typewrite('Hello world!')                 # prints out "Hello world!" instantly
>>> pyautogui.typewrite('Hello world!', interval=0.25)  # prints out "Hello world!" with a quarter second delay after each character
>>> pyautogui.press('enter')  # press the Enter key
>>> pyautogui.press('f1')     # press the F1 key
>>> pyautogui.press('left')   # press the left arrow key
>>> pyautogui.keyDown('shift')  # hold down the shift key
>>> pyautogui.press('left')     # press the left arrow key
>>> pyautogui.keyUp('shift')    # release the shift key
>>> pyautogui.hotkey('ctrl', 'shift', 'esc')
```




## Racket


```Racket
#lang racket/gui

(define frame (new frame%
                   (label "Example")
                   (width 300)
                   (height 300)))           ; Defines an instance of a frame to put the canvas in

(define simulate-key-canvas%
  (class canvas%
    (define/public (simulate-key key)
      (send this on-char key))              ; Creates a class that inherits from the standard canvas class, that can receive simulated key presses

    (define/override (on-char key)
      (displayln (send key get-key-code)))  ; Changes the method that receives key presses to show some output

    (super-new)))

(define canvas
  (new simulate-key-canvas%
       (parent frame)))                     ; Defines an instance of the newly created class

(send frame show #t)                        ; Shows the frame with a white canvas inside
(send canvas simulate-key (new key-event% (key-code #\k)))  ; Sends the simulated key press (with a key-event% instance)
;outputs k
```



## REXX

Note:   this REXX program   ''only''   works with the above two REXXes.

```rexx
/*REXX pgm shows how to use the REXX/PC  PRESS cmd to simulate keyboard input.*/

call press 'This text will be put into a buffer as if it came from the keyboard'

        /* [↑]  text will be available for any program to use (including DOS).*/
                                       /*stick a fork in it,  we're all done. */
```






## Rust

```Rust
extern crate autopilot;
fn main() {
    autopilot::key::type_string("Hello, world!", None, None, &[]);
}
```


## Scala

```scala
import java.awt.Robot
import java.awt.event.KeyEvent

/** Keystrokes when this function is executed will go to whatever application has focus at the time.
 *  Special cases may need to be made for certain symbols, but most of
 *  the VK values in KeyEvent map to the ASCII values of characters.
 */

object Keystrokes extends App {
  def keystroke(str: String) {
    val robot = new Robot()
    for (ch <- str) {
      if (Character.isUpperCase(ch)) {
        robot.keyPress(KeyEvent.VK_SHIFT)
        robot.keyPress(ch)
        robot.keyRelease(ch)
        robot.keyRelease(KeyEvent.VK_SHIFT)
      } else {
        val upCh = Character.toUpperCase(ch)
        robot.keyPress(upCh)
        robot.keyRelease(upCh)
      }
    }
  }
  keystroke(args(0))
}
```



## Tcl

This only works with windows created by Tk;
it sends a single key "x" to the given window.

```tcl
set key "x"
event generate $target <Key-$key>
```

To send multiple keys, call repeatedly in order.
Alphabetic keys can be used directly as events, " " has to be mapped to "<space>".

```Tcl
package require Tk
pack [text .t]
focus -force .t
foreach c [split "hello world" ""] {
   event generate .t [expr {$c eq " "?"<space>": $c}]
}
```

Note also that the task on [[Keyboard macros#Tcl|keyboard macros]]
illustrates a very closely related method.


## VBScript

The keystrokes are sent to the active window.


```vbscript
Dim WshShell
Set WshShell = WScript.CreateObject("WScript.Shell")
WshShell.SendKeys "{Down}{F2}"
WScript.Sleep 1000 ' one-second delay
WshShell.SendKeys "{Left}{Left}{BkSp}{BkSp}Some text here.~" ' ~ -> Enter
```

