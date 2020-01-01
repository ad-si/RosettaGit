+++
title = "Keyboard macros"
description = ""
date = 2019-09-02T14:52:08Z
aliases = []
[extra]
id = 4263
[taxonomies]
categories = []
tags = []
+++

{{task|GUI}}
Show how to link user defined methods to user defined keys.

An example of this is the facility provided by emacs for [http://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html key bindings].

These key bindings may be application-specific or system-wide; state which you have done.





## AutoHotkey


```AutoHotkey
Loop, 200  ; loop 200 times while not paused
{
  TrayTip, counting, %A_Index% press alt-p to pause
  Sleep, 1000
}

!p::  ; links alt-p key combination to the method pauseme() (system wide)
  pauseMe()
Return

!r::  ; links alt-r key combination to the method resume()  (system wide)
  resume()
Return

pauseMe()
{
  MsgBox, pausing`, press alt-r to resume
  Pause
}

resume()
{
  TrayTip, resume, resuming, 2
  Pause, off
}
```

See [http://www.autohotkey.com/forum/topic44290.html&highlight=vim ahk-viper-mode] for a context sensitive vi key bindings example.


## BBC BASIC


### Native


```bbcbasic
      *KEY 1 |!|A
      *KEY 2 |!|B
      REPEAT
        key% = INKEY(1)
        CASE key% OF
          WHEN &81: PROCmethod1
          WHEN &82: PROCmethod2
        ENDCASE
      UNTIL FALSE
      END

      DEF PROCmethod1
      PRINT "You pressed F1"
      ENDPROC

      DEF PROCmethod2
      PRINT "You pressed F2"
      ENDPROC
```


### Windows

{{works with|BBC BASIC for Windows}}

```bbcbasic
      FVIRTKEY = 1
      VK_F1 = &70
      VK_F2 = &71

      nsc% = 2
      DIM accel{(nsc%-1) fVirt&, pad&, key{l&,h&}, cmd{l&,h&}}
      accel{(0)}.fVirt& = FVIRTKEY : accel{(1)}.fVirt& = FVIRTKEY
      accel{(0)}.key.l& = VK_F1 : accel{(0)}.cmd.l& = &81
      accel{(1)}.key.l& = VK_F2 : accel{(1)}.cmd.l& = &82
      SYS "CreateAcceleratorTable", accel{(0)}, nsc% TO haccel%
      @haccel% = haccel%
      @hwacc% = @hwnd%

      ON SYS PROCsys(@wparam%) : RETURN
      REPEAT
        WAIT 1
      UNTIL FALSE
      END

      DEF PROCsys(W%)
      CASE W% AND &FFFF OF
        WHEN &81: PROCmethod1
        WHEN &82: PROCmethod2
      ENDCASE
      ENDPROC

      DEF PROCmethod1
      PRINT "You pressed F1"
      ENDPROC

      DEF PROCmethod2
      PRINT "You pressed F2"
      ENDPROC
```



## C

{{libheader|Xlib}}

The following example grabs Alt+F6 and Alt+F7 system-wide on a X server.

```c
#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>

int main()
{
  Display *d;
  XEvent event;

  d = XOpenDisplay(NULL);
  if ( d != NULL ) {
                /* or simply XK_F7 should work too */
    XGrabKey(d, XKeysymToKeycode(d, XStringToKeysym("F7")),
	     Mod1Mask, /* normally it's Alt */
	     DefaultRootWindow(d), True, GrabModeAsync, GrabModeAsync);
    XGrabKey(d, XKeysymToKeycode(d, XStringToKeysym("F6")),
	     Mod1Mask,
	     DefaultRootWindow(d), True, GrabModeAsync, GrabModeAsync);

    for(;;)
    {
      XNextEvent(d, &event);
      if ( event.type == KeyPress ) {
	KeySym s = XLookupKeysym(&event.xkey, 0);
	if ( s == XK_F7 ) {
	  printf("something's happened\n");
	} else if ( s == XK_F6 ) {
	  break;
	}
      }
    }

    XUngrabKey(d, XKeysymToKeycode(d, XStringToKeysym("F7")), Mod1Mask, DefaultRootWindow(d));
    XUngrabKey(d, XKeysymToKeycode(d, XStringToKeysym("F6")), Mod1Mask, DefaultRootWindow(d));
  }
  return EXIT_SUCCESS;
}
```



## Clojure

{{libheader|seesaw}}

The following example is at application level, printing out what key was pressed:

```clojure

(ns hello-seesaw.core
  (:use seesaw.core))

(defn -main [& args]
  (invoke-later
    (-> (frame
           :listen [:key-pressed (fn [e] (println (.getKeyChar e) " key pressed"))]
           :on-close :exit)
     pack!
     show!)))

```



## EchoLisp

The '''(meta-key "key-value" "bound-string")''' function binds a modifier+key keypress to a string or function call.

```lisp

;; see initial bindings : GREEK DICTIONARY
(meta-keys) → (("0" "❌") ("1" "❗️") ("2" "❓") ("3" "✔️") ("4" "⛔️") ("5" "✅") ("6" "🚩") ("7" "⌚️")
("8" "🏁") ("9" "😜") ("a" "α") ("b" "β") ("g" "γ") ("d" "δ") ("e" "ε") ("z" "ζ") ("h" "η") ("t" "τ")
("i" "ι") ("k" "κ") ("l" "λ") ("m" "μ") ("n" "ν") ("x" "ξ") ("q" "ο") ("p" "π") ("r" "ρ") ("w" "ς")
("s" "σ") ("u" "υ") ("f" "φ") ("c" "χ") ("y" "ψ") ("o" "ω") ("A" "Α") ("B" "Β") ("G" "Γ") ("D" "Δ")
("E" "Ε") ("Z" "Ζ") ("H" "Η") ("T" "Τ") ("I" "Ι") ("K" "Κ") ("L" "Λ") ("M" "Μ") ("N" "Ν") ("X" "Ξ") ("Q" "Ο") ("P" "Π") ("R" "Ρ") ("S" "Σ") ("U" "Υ") ("F" "Φ") ("C" "Χ") ("Y" "Ψ") ("O" "Ω"))

;; define modifier to use : Control key
(define-modifier-key "ctrl")

;; type : ctrl-R, ctrl-O, ...
  - ΡΩΣΕΤΤΑ

;; custom bindings
(meta-key "A" "Antoinette") ; string
(meta-key "H" "(begin (writeln 'HELLO) (date 'today))") ; function call

```



## Go

{{libheader|Xlib}}
{{trans|C}}


Note that 'cgo' does not support C unions as such - it expresses them as byte arrays. Consequently, the easiest way to access a field of a union (such as XEvent) is to write a C assessor function for it and then invoke that function from the Go side.

Note also that if you pass 'nil' to the XOpenDisplay function, it defaults to the value of the DISPLAY environment variable which has to be in a certain format to enable a connection to the X server to be established - check [https://www.x.org/releases/X11R7.7/doc/libX11/libX11/libX11.html the documentation] for details.

```go
package main

/*
#cgo LDFLAGS: -lX11
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>

static inline Window DefaultRootWindow_macro(Display *dpy) {
    return ScreenOfDisplay(dpy, DefaultScreen(dpy))->root;
}

static inline int getXEvent_type(XEvent event) {
    return event.type;
}

static inline XKeyEvent getXEvent_xkey(XEvent event) {
    return event.xkey;
}
*/
import "C"
import "fmt"
import "unsafe"

func main() {
    d := C.XOpenDisplay(nil)
    f7, f6 := C.CString("F7"), C.CString("F6")
    defer C.free(unsafe.Pointer(f7))
    defer C.free(unsafe.Pointer(f6))

    if d != nil {
        C.XGrabKey(d, C.int(C.XKeysymToKeycode(d, C.XStringToKeysym(f7))),
            C.Mod1Mask, /* normally it's Alt */
            C.DefaultRootWindow_macro(d), C.True, C.GrabModeAsync, C.GrabModeAsync)
        C.XGrabKey(d, C.int(C.XKeysymToKeycode(d, C.XStringToKeysym(f6))),
            C.Mod1Mask,
            C.DefaultRootWindow_macro(d), C.True, C.GrabModeAsync, C.GrabModeAsync)

        var event C.XEvent
        for {
            C.XNextEvent(d, &event)
            if C.getXEvent_type(event) == C.KeyPress {
                xkeyEvent := C.getXEvent_xkey(event)
                s := C.XLookupKeysym(&xkeyEvent, 0)
                if s == C.XK_F7 {
                    fmt.Println("something's happened")
                } else if s == C.XK_F6 {
                    break
                }
            }
        }

        C.XUngrabKey(d, C.int(C.XKeysymToKeycode(d, C.XStringToKeysym(f7))), C.Mod1Mask, C.DefaultRootWindow_macro(d))
        C.XUngrabKey(d, C.int(C.XKeysymToKeycode(d, C.XStringToKeysym(f6))), C.Mod1Mask, C.DefaultRootWindow_macro(d))
    } else {
        fmt.Println("XOpenDisplay did not succeed")
    }
}
```



## HicEst


```hicest
! bound to application
   CALL F2
   ! ...
 END

SUBROUTINE F2 ! this text shows as tooltip text. F2 ... F9 are possible
   !  synchronous call:     CALL F2
   ! asynchronous calls:    F2 key
   !                        mouse click on the F2 toolbar tool
   !                        ALARM( delay_seconds, 2 )

   ! check if a modifier key is pressed, or a lock key is activated to control program flow:
   KEY(SHift=shft, Control=cntl, ALt=alt, CApital=caps, Numlock=num, SCRoll=scrl)
   WRITE(ClipBoard, Name) shft, cntl, alt, caps, num, scrl
   ! shft=1; cntl=2; alt=4; caps=8; num=16; scrl=32;
   ! is copied to clipboard if all 6 keys are activated and the F2 tool is clicked

   ! Alarm methods F2 ... F9 suspend program flow, which is resumed when finished
   ! If Fn is running and Fm is called:
   !       Fn is suspended if m > n AND n <= 5, else Fm is queued
   ! ...
END
```


=={{header|Icon}} and {{header|Unicon}}==

This is application-specific and works in both languages:

```unicon

global kMap

procedure main()
    kMap := table()
    kMap["1"] := italicsOn
    kMap["2"] := italicsOff
    kMap["\x4"] := exit    # ^D terminates
    while writes(exec(getch()))
end

procedure exec(c)
    return (\kMap[c])() | c
end

procedure italicsOn()
    return "<i>"
end

procedure italicsOff()
    return "<\\i>"
end
```


Sample run:

```txt

->km
this is <i>italics<\i>.
->

```



## Java


```java

package keybord.macro.demo;

import javax.swing.JFrame;
import javax.swing.JLabel;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

class KeyboardMacroDemo {
    public static void main( String [] args ) {
        final JFrame frame = new JFrame();

        String directions = "<html><b>Ctrl-S</b> to show frame title
"
                                 +"<b>Ctrl-H</b> to hide it</html>";

        frame.add( new JLabel(directions));
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        frame.addKeyListener( new KeyAdapter(){
            public void keyReleased( KeyEvent e ) {
                if( e.isControlDown() && e.getKeyCode() == KeyEvent.VK_S){
                    frame.setTitle("Hello there");
                }else if( e.isControlDown() && e.getKeyCode() == KeyEvent.VK_H){
                    frame.setTitle("");
                }
            }
        });
        frame.pack();
        frame.setVisible(true);
    }
}

```



## JavaScript

The example below captures the F7 key when pressed, if the document (that is, the web page) has focus. If the function returns ''false'', the event processing is halted. If it returns any other value, including ''undefined'', the event continues up the DOM tree ('bubbling').


```javascript
document.onkeydown = function(evt) {
  if (evt.keyCode === 118) {
    alert("You pressed F7!");
    return false;
  }
}
```


See [http://www.quirksmode.org/js/keys.html quirksmode] for more information about key detection in JavaScript.


## Julia

Macros are within the Gtk window.

```julia
using Gtk

function keypresswindow()
    tcount = 0
    txt = "Press a Number Key"
    win = GtkWindow("Keyboard Macros Test", 300, 50) |> (GtkFrame() |> ((vbox = GtkBox(:v)) |> (lab = GtkLabel(txt))))
    function keycall(w, event)
        ch = Char(event.keyval)
        if isdigit(ch)
            set_gtk_property!(lab, :label, "Keyboard Macro Number $ch Invoked.")
        end
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

{{trans|Java}}

```scala
// version 1.2.31

import javax.swing.JFrame
import javax.swing.JLabel
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent

fun main(args: Array<String>) {
    val directions = "<html><b>Ctrl-S</b> to show frame title
" +
                     "<b>Ctrl-H</b> to hide it</html>"
    with (JFrame()) {
        add(JLabel(directions))
        defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        addKeyListener(object : KeyAdapter() {
            override fun keyReleased(e: KeyEvent) {
                if (e.isControlDown() && e.keyCode == KeyEvent.VK_S)
                    title = "Hello there"
                 else if( e.isControlDown() && e.keyCode == KeyEvent.VK_H)
                    title = ""
            }
        })
        pack()
        isVisible = true
    }
}
```



## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      \\ Scan statement exist from version 1
      \\ We can make targets and use function keys, in console
      \\ Scan track mouse click on targets and functions keys state
      \\ when any of these selected then a call to a global module happen
      \\ show$(hide$("&8%", "master",3), "code1", 10)="123"
      Global a$
      Module Global GetIt {
                  Input "Password:"; a$
                  if a$<>"" then a$<=show$(hide$(a$, "code1", 10), "master", 3)
      }
      Module Global myHelp {
            Print "Press F1 for help, F3 to enter password, F5 exit"
      }
      Fkey 1, "myHelp"
      Fkey 3, "GetIt"
      Fkey 5, {a$="---"}
      myHelp
      tries=0
      Repeat {
            Scan .1
            if a$="---" then 1000
            if a$="&8%" then Exit
            if a$<>"" then tries++ : if tries>2 then 1000
            a$<=""
      } Always
      Print "Enter ok"

      1000 Print "Exit ", tries : End
}
Checkit

```



## Mathematica

Map the keystroke t to a simple script

```Mathematica
SetOptions[EvaluationNotebook[], NotebookEventActions -> {{"KeyDown", "t"} :> Print["You pressed \"t\""]}]
```



## Oz

Window-specific key bindings:

```oz
declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}
  Label
  Window = {QTk.build td(label(text:"Hello" handle:Label))}
in
  {Window bind(event:"<Control-x><Control-s>"
	       action:proc {$}
			 {Label set(text:"This is not Emacs.")}
		      end
	      )}
  {Window bind(event:"<Control-x><Control-c>"
	       action:toplevel#close
	      )}
  {Window show}
```



## Perl


```perl
use strict;
use warnings;
use Term::ReadKey;

ReadMode 4; # change to raw input mode

sub logger { my($message) = @_; print "$message\n" }

while (1) {
    if (my $c = ReadKey 0) { # read a single character
        if ($c eq 'q') { logger "QUIT"; last }
        elsif ($c =~ /\n|\r/) { logger "CR" }
        elsif ($c eq "j") { logger "down" }
        elsif ($c eq "k") { logger "up" }
        elsif ($c eq "h") { logger "left" }
        elsif ($c eq "l") { logger "right" }

        elsif ($c eq "J") { logger "DOWN" }
        elsif ($c eq "K") { logger "UP" }
        elsif ($c eq "H") { logger "LEFT" }
        elsif ($c eq "L") { logger "RIGHT" }

        elsif ($c eq "\e") { # handle a few escape sequences
            my $esc  = ReadKey 0;
               $esc .= ReadKey 0;
            if    ($esc eq "[A") { logger "up" }
            elsif ($esc eq "[B") { logger "down" }
            elsif ($esc eq "[C") { logger "right" }
            elsif ($esc eq "[D") { logger "left" }
            elsif ($esc eq "[5") { logger "page up" }
            elsif ($esc eq "[6") { logger "page down" }
            else { logger "Unrecognized escape: $esc"; }
        }

        else { logger "you typed: $c"; }
    }
}

ReadMode 0; # reset the terminal to normal mode
```



## Perl 6


```perl6
my $TTY = open("/dev/tty");
my @INPUT;

sub log($mess) { print "$mess\r\n" }

INIT { shell "stty raw -echo min 1 time 1"; log "(raw mode)"; }
END { shell "stty sane"; log "(sane mode)"; }

loop {
    push @INPUT, $TTY.getc unless @INPUT;
    given @INPUT.shift {
        when "q" | "\c4" { log "QUIT"; last; }

        when "\r" { log "CR" }

        when "j" { log "down" }
        when "k" { log "up" }
        when "h" { log "left" }
        when "l" { log "right" }

        when "J" { log "DOWN" }
        when "K" { log "UP" }
        when "H" { log "LEFT" }
        when "L" { log "RIGHT" }

        when "\e" {
            my $esc = '';
            repeat until my $x ~~ /^<alpha>$/ {
                push @INPUT, $TTY.getc unless @INPUT;
                $x = @INPUT.shift;
                $esc ~= $x;
            }
            given $esc {
                when "[A" { log "up" }
                when "[B" { log "down" }
                when "[C" { log "right" }
                when "[D" { log "left" }
                when "[1;2A" { log "UP" }
                when "[1;2B" { log "DOWN" }
                when "[1;2C" { log "RIGHT" }
                when "[1;2D" { log "LEFT" }
                default { log "Unrecognized escape: $esc"; }
            }
        }
        default { log "Unrecognized key: $_"; }
    }
}
```



## Phix


### application

{{libheader|pGUI}}
Shows how to link a specific key (C) to a specific function, and
a general key handler.
Obviously K_C and K_F2 could be swapped without any problem.

```Phix
include pGUI.e

function C_Keyed(Ihandle /*ih*/, atom /*c*/)
-- (Note without K_c below this does not respond to 'c', just 'C')
    ?"you pressed C"
    return IUP_DEFAULT
end function

procedure F2_keyed()
    ?"you pressed F2"
end procedure

function key_cb(Ihandle /*ih*/, atom c)
    if c=K_F2 then F2_keyed()
    elsif c=K_ESC then return IUP_CLOSE
    end if
    return IUP_DEFAULT
end function

IupOpen()
Ihandle dlg = IupDialog(IupLabel("hello"),"TITLE=\"Press F2\"")
IupSetCallback(dlg, "K_C", Icallback("C_Keyed"))
--IupSetCallback(dlg, "K_c", Icallback("C_Keyed"))
IupSetCallback(dlg, "K_ANY", Icallback("key_cb"))
IupShow(dlg)
IupMainLoop()
IupClose()
```



### system

A low-level (windows 32-bit only) demo for system-wide keyboard macros,
that has been used to simplify some repetitive tasks in InstallShield,
and could equally be used on any third-party application. As it stands,
the key selection is a bit overkill, it always sends {delete,down},
perhaps not precisely what the task asked for, but fits my interpretation
of "keyboard macros" - though of course you could easily replace that
SendInput call with any routine of your choosing.

```Phix
--
-- demo\arwendemo\hotkey.exw
--
### ===================

--
--  Author: Pete Lomax, credit to Aku Saya for HotKey
--                            and Thomas Parslow for sendkeys
--
--  http://phix.x10.mx
--
--/**/with gui
include arwen.ew

constant
    MOD_ALT = #1,
    MOD_CONTROL = #2,
    MOD_SHIFT = #4,
    MOD_WIN = #8

integer Modifier, vKeyCode

constant Main = create(Window, "Hotkey", 0, 0, 36, 99, 294, 201, 0)
constant MainHwnd = getHwnd(Main)
constant mFile = create(Menu,"File",0,Main,190,63,0,0,0)
constant mExit = create(MenuItem,"Exit",0,mFile,194,53,0,0,0)
constant mHelp = create(Menu,"Help",0,Main,182,57,0,0,0)
constant mAbout = create(MenuItem,"About",0,mHelp,184,45,0,0,0)
constant AltKey = create(CheckBox, "Alt", 0, Main, 8, 5, 62, 20, 0)
constant ShiftKey = create(CheckBox, "Shift", 0, Main, 8, 28, 56, 20, 0)
constant CtrlKey = create(CheckBox, "Ctrl", 0, Main, 8, 52, 70, 20, 0)
constant WinKey = create(CheckBox, "Windows", 0, Main, 8, 76, 72, 20, 0)
constant KeyList = create(ComboDropDown, "KeyList", 0, Main, 89, 11, 100, 652, 0)
constant KeyInfoText = create(Label, "", 0, Main, 90, 72, 186, 20, SS_LEFTNOWORDWRAP)
constant SetButton = create(Button, "setHotKey", 0, Main, 8, 99, 176, 40, BS_DEFPUSHBUTTON)
constant KillButton = create(Button, "killHotKey", 0, Main, 195, 99, 80, 40, 0)

sequence KeyCodes

procedure initialise()
string text
    for f=1 to 11 do    -- F1 to F11 (F12 is reserved)
        text = sprintf("F%d",f)
        void = insertItem(KeyList,text,0)
    end for
    KeyCodes = {VK_F1, VK_F2, VK_F3, VK_F4, VK_F5, VK_F6, VK_F7, VK_F8, VK_F9, VK_F10, VK_F11}
    for ch='A' to 'Z' do
        text = sprintf("%s",ch)
        void = insertItem(KeyList,text,0)
        KeyCodes &= ch -- (as char, not string)
    end for
    for i=1 to 9 do
        text = sprintf("%d",i)
        void = insertItem(KeyList,text,0)
        KeyCodes &= i -- (as char, not string)
    end for
end procedure
initialise()

constant INPUT_KEYBOARD=1

procedure pokeKey(atom pKey, integer key)
-- see http://msdn.microsoft.com/en-us/library/windows/desktop/ms646270(v=vs.85).aspx
-- and http://msdn.microsoft.com/en-us/library/windows/desktop/ms646271(v=vs.85).aspx
integer ScanCode
    ScanCode = c_func(xVkKeyScan,{key})
    poke4(pKey+KEYBDINPUT_type,INPUT_KEYBOARD)
    poke2(pKey+KEYBDINPUT_wVk,key)
    poke4(pKey+KEYBDINPUT_dwFlags,0)
    poke4(pKey+KEYBDINPUT_wScan,ScanCode)
    poke4(pKey+KEYBDINPUT_time,0)
    poke4(pKey+KEYBDINPUT_dwExtraInfo,0)
end procedure

function MainHandler(integer id, integer msg, atom wParam, object lParam)
string text
atom pKeys, pKey
integer nRes

    if msg=WM_SETFOCUS then
        if id=SetButton then
            if not getIndex(KeyList) then
                setFocus(KeyList)
                void = messageBox("HotKey","Select a key from the drop-down",MB_OK)
            else
                Modifier = isChecked(AltKey) * MOD_ALT +
                           isChecked(ShiftKey) * MOD_SHIFT +
                           isChecked(CtrlKey) * MOD_CONTROL +
                           isChecked(WinKey) * MOD_WIN
                vKeyCode = KeyCodes[getIndex(KeyList)]
                text = sprintf("setHotKey(Main, #%02x, #%02x) [%s]",{Modifier,vKeyCode,getText(KeyList)})
                setText(KeyInfoText, text)
            end if
        elsif id=KillButton then
            text = sprintf("killHotKey(Main) [%s]",{getText(KeyList)})
            setText(KeyInfoText, text)
        end if
    elsif msg=WM_COMMAND then
        if id=mExit then
            closeWindow(Main)
        elsif id=mAbout then
            text = "Simple hotkey/sendinput wrapper.\n\n"&

                   "Author Pete Lomax.\n"&
                   "Written in phix (http://phix.x10.mx) but could easily be ported\n"&
                   "to any language (that can invoke RegisterHotKey and SendInput).\n\n"&

                   "First, use the checkboxes and dropdown to select a hotkey (eg F7).\n"&
                   "Currently always sends {delete,down}, but that could easily be changed.\n"&
                   "Used on build02 to perform the GUID stripping.\n\n"&

                   "Note that Windows Server 2008 requires this to be run in admin mode, \n"&
                   "as otherwise something called UIPI will block it and not say why.\n"
            void = messageBox("HotKey",text,MB_OK)
        elsif id=SetButton then
            -- see http://msdn.microsoft.com/en-us/library/windows/desktop/ms646309(v=vs.85).aspx
            if c_func(xRegisterHotKey,{MainHwnd, 0, Modifier, vKeyCode})=0 then
                void = messageBox("HotKey","Register Hotkey failed",MB_OK)
            end if
        elsif id=KillButton then
            if c_func(xUnregisterHotKey,{MainHwnd, 0})=0 then
                void = messageBox("HotKey","UnRegister Hotkey failed",MB_OK)
            end if
        end if
    elsif msg=WM_HOTKEY then
        setText(Main,sprintf("%g",time()))
        pKeys = allocate(sizeofstruct(KEYBDINPUT)*2)
        pKey = pKeys
        pokeKey(pKey,VK_DELETE)
        pKey += sizeofstruct(KEYBDINPUT)
        pokeKey(pKey,VK_DOWN)

        -- see http://msdn.microsoft.com/en-us/library/windows/desktop/ms646310(v=vs.85).aspx
        nRes = c_func(xSendInput,{2,pKeys,sizeofstruct(KEYBDINPUT)})
        if nRes!=2 then
            nRes = c_func(xGetLastError,{})
            text = sprintf("SendInput failed[%d]",nRes)
            void = messageBox("HotKey",text,MB_OK)
        end if
    end if
    if wParam or object(lParam) then end if -- suppress warnings
    return 0
end function
setHandler({Main,SetButton,KillButton,mExit,mAbout},routine_id("MainHandler"))

WinMain(Main,SW_NORMAL)
```



## PicoLisp

The 'fkey' function associates a key with an executable body. Some common key
codes are predefined in "lib/term.l". Here we use 'F1' to store the value 1 in a
global variable, 'Up' and 'Down' arrows to increment or decrement that value,
and 'Home' to print the current value to the console.

```PicoLisp
(load "@lib/term.l")

(fkey *XtF1
   (prinl "Initialized value to " (setq *Number 1)) )

(fkey *XtUp
   (prinl "Incremented to " (inc '*Number)) )

(fkey *XtDown
   (prinl "Decremented to " (dec '*Number)) )

(fkey *XtHome
   (prinl "Current value is " *Number) )
```

Output when hitting 'F1', 'Down', 'Up', 'Up' and 'Home':

```txt
Initialized value to 1
Decremented to 0
Incremented to 1
Incremented to 2
Current value is 2
```



## PureBasic

PureBasic has support for shortcut/macro creation in any window that supports events. This allows for creation of both single and combinations as shown in the code below.
For full set of combinations on PC, Mac & Linux please see the official manual, [http://www.purebasic.com/documentation/window/addkeyboardshortcut.html here].

```PureBasic
#Win   = 0
#Demo1 = 0
#Demo2 = 1

If OpenWindow(#Win,50,50,200,60,"PureBasic",#PB_Window_SystemMenu)
  ;
  AddKeyboardShortcut(#Win,#PB_Shortcut_F1, #Demo1)
  AddKeyboardShortcut(#Win,#PB_Shortcut_F|#PB_Shortcut_Alt, #Demo2)
  ;
  Repeat
    WEvent = WaitWindowEvent()
    Select  WEvent
      Case #PB_Event_Menu
        Select EventMenu()
          Case #Demo1
            MessageRequester("Info", "You Pressed F1")

          Case #Demo2
            MessageRequester("Info", "You Pressed Alt-F")

        EndSelect
      Case #PB_Event_CloseWindow
        Break
    EndSelect
  ForEver
EndIf
```



## Python

Works on Unix platforms.


```python
#!/usr/bin/env python
import curses

def print_message():
    stdscr.addstr('This is the message.\n')

stdscr = curses.initscr()
curses.noecho()
curses.cbreak()
stdscr.keypad(1)

stdscr.addstr('CTRL+P for message or q to quit.\n')
while True:
    c = stdscr.getch()
    if c == 16: print_message()
    elif c == ord('q'): break

curses.nocbreak()
stdscr.keypad(0)
curses.echo()
curses.endwin()

```



## Racket



```racket

#lang racket

(define-syntax-rule (with-raw body ...)
  (let ([saved #f])
    (define (stty x) (system (~a "stty " x)) (void))
    (dynamic-wind (λ() (set! saved (with-output-to-string (λ() (stty "-g"))))
                       (stty "raw -echo opost"))
                  (λ() body ...)
                  (λ() (stty saved)))))

(define (->bytes x)
  (cond [(bytes? x)  x]
        [(string? x) (string->bytes/utf-8 x)]
        [(not (list? x)) (error '->bytes "don't know how to convert: ~e" x)]
        [(andmap byte? x) (list->bytes x)]
        [(andmap char? x) (->bytes (list->string x))]))
(define (open x)
  (open-input-bytes (->bytes x)))

(define macros (make-vector 256 #f))
(define (macro-set! seq expansion)
  (let loop ([bs (bytes->list (->bytes seq))] [v (vector macros)] [i 0])
    (if (null? bs)
      (vector-set! v i expansion)
      (begin (unless (vector-ref v i) (vector-set! v i (make-vector 256 #f)))
             (loop (cdr bs) (vector-ref v i) (car bs))))))

;; Some examples
(macro-set! "\3" exit)
(macro-set! "ME" "Random J. Hacker")
(macro-set! "EMAIL" (λ() (open "ME <me@example.com>")))
(macro-set! "\r" "\r\n")
(macro-set! "\n" "\r\n")
(for ([c "ABCD"]) (macro-set! (~a "\eO" c) (~a "\e[" c)))

(with-raw
  (printf "Type away, `C-c' to exit...\n")
  (let loop ([inps (list (current-input-port))] [v macros] [pending '()])
    (define b (read-byte (car inps)))
    (if (eq? b eof) (loop (cdr inps) v pending)
        (let mloop ([m (vector-ref v b)])
          (cond [(vector? m) (loop inps m (cons b pending))]
                [(input-port? m) (loop (cons m inps) macros '())]
                [(or (bytes? m) (string? m))
                 (display m) (flush-output) (loop inps macros '())]
                [(procedure? m) (mloop (m))]
                [(and m (not (void? m))) (error "bad macro mapping!")]
                [(pair? pending)
                 (define rp (reverse (cons b pending)))
                 (write-byte (car rp)) (flush-output)
                 (loop (if (null? (cdr rp)) inps
                           (cons (open (list->bytes (cdr rp))) inps))
                       macros '())]
                [else (write-byte b) (flush-output) (loop inps v '())])))))

```



## REBOL


```REBOL
REBOL [
    Title: "Keyboard Macros"
    URL: http://rosettacode.org/wiki/Keyboard_macros
]

; Application specific keyboard bindings using REBOL VID
; dialect. Implementation of the "Averageman" calculator --
; See http://www.atariarchives.org/bcc2/showpage.php?page=63 for details.

view layout [
	style btn button coal 46
	across

	display: h1 100 red maroon right ""  return

; Key shortcuts are specified as character arguments to widget
; descriptions in the layout.

	btn "1" #"1" [set-face display "1"]
	btn "+" #"+" [set-face display ""]
	return

	pad 54
	btn "=" #"=" [set-face display "3"]

	pad 1x100 return
	text "(c) 1977 G. Beker"
]
```



## REXX

{{works with| PC/REXX and Personal REXX}}
This REXX program   ''only''   works with PC/REXX or Personal REXX under "DOS" or under "DOS" in Microsoft Windows 1, 2, 3, 95, 98, or 2000.

It '''won't''' work for the Microsoft Windows/NT family (Windows/NT/XP/Vista/7/8 ···).


If under Microsoft Windows, the change is only for the current "DOS" session (i.e., that DOS window).

If under a native DOS, the change is system wide.


Almost every keyboard key (including the '''F''' (function) keys, numeric keypad, can be re-defined.


REXX programs not included are '''$T''' which is only used when specific options are used (used when TOPS is specified),

the '''$ERR''' program which issues errors, and '''$H''' which shows '''help''' and other documentation.

```rexx
/*REXX program can re-define most keys (including  F  keys)  on a PC keyboard.*/
trace off
parse arg !
if !all(arg())  then exit
if !cms  then address ''

signal on halt
signal on noValue
signal on syntax

                                                   /*if not DOS, issue error. */
if \!dos     then call er 23,', DOS[environment]'

                                                   /*if not PC/REXX, issue err*/
if \!pcrexx  then call er 23,', PC/REXX[interpreter]'

                                                   /*if Windows/NT, issue err.*/
if !nt       then call er 23,!fn 'Windows/95/98/2000 REXX-program'

      /* This program requires  ANSI.SYS  if any keys are set or (re─)defined.*/
      /* ANSI.SYS won't function correctly under Windows/NT (XP, Vista, 7, 8).*/

call homeDrive                                     /*get the homeDrive envVar.*/

$home=p(!var('$HOME') homeDrive)                   /*get homeDrive of \$\ dir.*/
$home=appenda($home,':')                           /*make the drive ──► drive:*/
$path=p(!var('$PATH') '\$')                        /*get path name of \$  dir.*/
$path=prefixa($PATH,'\')                           /*make the path  ──► \dir  */
$path=appenda($path,'\')                           /*make the path  ──► dir\  */

if \hasCol($path)  then $path=$home || $path       /*prefix with  $HOME  ?    */

@DOSKEY    = 'DOSKEY'                              /*point to the DOSKEY   cmd*/
@ECHO      = 'ECHO'                                /*point to the ECHO     cmd*/
@TYPE      = 'TYPE'                                /*point to the TYPE     cmd*/
defFid     = #path'LOGS\'!fn".LOG"
oldFid     = #path'LOGS\'!fn".OLD"
tops       = '.BOX= .C=blue .H=darkcyan .E=1'
fops       = '.EF='defFid
functionKey= 0
autoEnter  =
useAuto    = 0
@offon     = 'OFF ON ,'
@warns     = 'WARNIFOFF WARNIFON ,'
sepLine    = copies('═',5)  copies('═',73)
y          = space(!!)

  do  forever                                      /*process any & all options*/
  parse var y k1 2 1 k y
  uk=k; upper uk

  if uk=='[ENTER]'    then do
                           useAuto=1
                           autoEnter=13
                           iterate
                           end

  if uk=='[NOENTER]'  then do
                           useAuto=1
                           autoEnter=
                           iterate
                           end

  if k1\=='.'         then leave
  tops=tops k
  fops=fops k
  end   /*forever*/

tops=space(tops)
fops=space(fops)
origk=space(k)
upper k

if k=='??'  |,
   k=="???" |,
   k=="????"  then do
                   !cls
                   if y==''      then y=defFid
                   @type y
                   say sepLine
                   if k=="???"   then call $defkey "ALLLOCKS , WARNIFON"

                   if k=="????"  then do
                                      call $t ".P=1 .C=blue" centre('DOSKEY macros',79,"─")
                                      @doskey '/macro'
                                      call $t ".C=blue" copies('─',79)
                                      end
                   exit rc
                   end

if k=='CLEARLOG' then do
                      lFID=defFid

                      if lFID==defFid  then do
                                            call dosdel oldFid
                                            call dosrename defFid,oldFid
                                            end
                                       else call dosdel lFID

                      call whenstamp lFID,'log file was cleared by' !fn"."
                      _='ECHO' sepLine">>"lFID
                      _
                      'ECHO  key         new value>>'lFID
                      _
                      exit
                      end

shiftkeys='NUMLOCK CAPSLOCK SCROLLLOCK ALLLOCKS'

if abbrev('BLINKLOCKKEYS',k,5)  then
     do
     parse var o . k times secs _
     if _\==''                            then call er 59
     if k=='' | k==","                    then k="ALLLOCKS"
     if wordpos(k,shiftkeys)==0           then call er 50,'shiftlock-key' origk
     if times=='' | times==','            then times="ANYKEY"
     if times\=='ANYKEY' & \isint(times)  then call er 53,times 'times'
     if secs=='' | secs==','              then secs=.1
     if \isNum(secs)                      then call er 53,times "seconds-delay-time"
     secs=secs/1
     if secs<.1 | secs>99                 then call er 81,.1 99 secs 'seconds-delay-time'
     dids=0

       do forever

         do j=1  for 3

           do jo=2  to 1  by -1
           dakey=word(shiftkeys,j)
           if k=='ALLLOCKS' | k==dakey then call "$DEFKEY" dakey word(@offon,jo)
           if secs\==0                 then call delay secs
           end   /*jo*/

         end     /*j*/

       dids=dids+1
       if times\=='ANYKEY' & dids>=times  then exit
                                          else if inkey("NOWAIT")\=='' then exit
       end   /*forever*/
     end

if wordpos(k,shiftkeys)\==0  then
     do
     _=words(y)
     if _>2  then call er 59
     onoff=
     warnif=0
     iswas='is'
     if y==','  then y=

     if y\==''  then do

                     if _==2 then do
                                  _=word(y,2)
                                  warnif=wordpos(translate(_),@warns)
                                  if warnif==0 then call er 55,_ k 'WARN'
                                  if warnif==3 then warnif=0
                                  y=subword(y,1,1)
                                  end

                     onoff=wordpos(translate(y),@offon)
                     if onoff==0 then call er 50,'ON-or-OFF' y
                     if onoff\==3 then iswas='was'
                     end

     if y==','  then y=

       do j=1  for 3
       dakey=word(shiftkeys,j)
       if warnif\==0  then if shiftstate(dakey)+1==warnif then call $t ".J=r" tops dakey iswas'('word(@offon,warnif)")"

       if k=="ALLLOCKS" | k==dakey  then
          do
          if y\=='' &,
             onoff\==3  then call shiftstate dakey,onoff-1
                        else if warnif==0 then call $t ".I=25" tops dakey 'is ('word(@offon,shiftstate(dakey)+1)")"
          end

       end   /*j*/

     exit
     end

if y==''  then call er 54
cod=
codz='Z'

if pos('-',k)\==0  then do
                        parse var k cod '-' k
                        _='S SHIFT C CTRL CONTROL A ALT ALTERNATE'
                        if cod=='' | wordpos(cod,_)==0  then call er 50,"key" origk
                        cod=left(cod,1)
                        codl=lower(cod)
                        codz=cod
                        if k==''  then call er 50,"key" origk
                        end

if abbrev('APOSTROPHE',k,5)               then k = "'"
if k=='ASTERISKKEYPAD' | k=='STARKEYPAD'  then k = "*KEYPAD"
if k=='BACKSLASH'                         then k = "\"
if k=='COMMA'                             then k = ","
if k=='DEL'                               then k = "DELETE"
if k=='DELKEYPAD'                         then k = "DELETEKEYPAD"
if k=='ENT'                               then k = "ENTER"
if k=='ENTKEYPAD'                         then k = "ENTERKEYPAD"
if k=='EQUAL'                             then k = "="
if k=='FIVEKEYPAD'                        then k = "5KEYPAD"
if k=="GRAVEACCENT" | k=='GRAVE'          then k = "`"
if k=='INSKEYPAD'                         then k = "INSKEYPAD"
if k=='LEFTBRACKET'                       then k = "["
if k=='MINUS'                             then k = "-"
if k=='MINUSKEYPAD'                       then k = "-KEYPAD"
if k=="PAUSE" | k=='BREAK'                then k = "PAUSEBREAK"
if k=='PGDN'                              then k = "PAGEDOWN"
if k=='PGDNKEYPAD'                        then k = "PAGEDOWNKEYPAD"
if k=='PGUP'                              then k = "PAGEUP"
if k=='PGUPKEYPAD'                        then k = "PAGEUPKEYPAD"
if k=='PLUSKEYPAD'                        then k = "+KEYPAD"
if k=='PRINTSCRN'                         then k = "PRINTSCREEN"
if k=='RIGHTBRACKET'                      then k = "]"
if k=='SEMICOLON'                         then k = ";"
if k=='SPACE' | k=="SPACEBAR"             then k = 'BLANK'

if wordpos(k,'PERIOD DOT DECIMAL DECIMALPOINT')\==0                       then k="."
if wordpos(k,'SLASH SOLIDUS VIRGULE OBELUS')\==0                          then k="/"
if wordpos(k,'SLASHKEYPAD SOLIDUSKEYPAD VIRGULEKEYPAD OBELUSKEYPAD')\==0  then k="/KEYPAD"
base=

  do 1                     /*the "1" enables the use of the LEAVE instruction.*/
  len1=(length(k)==1)
  uppc=isUpp(k)
  numb=isint(k)

  if len1 then do
               dkey=c2d(k)
               if uppc then do
                            if cod=='A' then do
                                             _='30 48 46 32 18 33 34 35 23 36 37 38 50 49 24 25 16 19 31 20 22 47 17 45 21 44'
                                             base='0;'word(_,dkey-96)
                                             end
                            d.z=21
                            d.s=0
                            d.c=-64
                            base=d.codz+dkey
                            end

               if numb then do
                            dakey=dkey-47
                            if cod=''   then base=dkey
                            if cod=='S' then base=word("41 33 64 35 36 37 94 38 42 49",dakey)

                            if cod=='A' then if k<3 then base="0;"word(129 120,dakey)
                                                    else base="0;"119+dakey

                            if cod=='C' then do
                                             if k==2 then base=0
                                             if k==6 then base=30
                                             end
                            end

               if base\==''  then leave
               call er 50,'key' origk
               end

  ik=wordpos(k,'DELETE DOWNARROW END HOME INSERT LEFTARROW PAGEDOWN PAGEUP RIGHTARROW UPARROW')

    select
    when left(k,1)=='F' then do
                        functionKey=1
                        fn=substr(k,2)
                        if \isint(fn) | fn<1 | fn>12  then call er 81,1 12 k "FunctionKey"
                        d.z=58
                        d.s=83
                        d.c=93
                        d.a=103
                        if fn<11  then base='0;' || (d.codz+fn)
                                  else do
                                       d.z=133-11
                                       d.s=135-11
                                       d.c=137-11
                                       d.a=139-11
                                       base='0;' || (d.codz+fn)
                                       end
                        end

    when ik\==0  then do
                      d.z='83 80 79 71 82 75 81 73 77 72'
                      d.s=d.z
                      d.c='147 145 117 119 146 115 118 132 116 141'
                      d.a='163 154 159 151 162 155 161 153 157 152'
                      base='224;'word(d.codz,ik)
                      end

    when k=='PRINTSCREEN' & cod="C"  then base='0;114'
    when k=='PAUSEBREAK'  & cod="C"  then base='0;0'
    when k=='NULL'        & cod==''  then base="0;3"

    when k=='BACKSPACE'  then do
                              d.z=8
                              d.s=8
                              d.c=127
                              d.a=0
                              base=d.codz
                              end

    when k=='TAB'   then do
                         d.z=9
                         d.s='0;15'
                         d.c='0;148'
                         d.z='0;165'
                         base=d.codz
                         end

    when k=='BLANK' then do
                         d.z=92
                         d.s=124
                         d.c=28
                         d.a='0;43'
                         base=d.codz
                         end

    when k=='ENTER' then do
                         d.z=13
                         d.s=
                         d.c=10
                         d.a='0;28'
                         base=d.codz
                         end

    when k=='-'  then do
                      d.z=45
                      d.s=95
                      d.c=31
                      d.a='0;130'
                      base=d.codz
                      end

    when k=='='  then do
                      d.z=61
                      d.s=43
                      d.c=
                      d.a='0;131'
                      base=d.codz
                      end

    when k=='['  then do
                      d.z=91
                      d.s=123
                      d.c=27
                      d.a='0;26'
                      base=d.codz
                      end

    when k==']'  then do
                      d.z=93
                      d.s=125
                      d.c=29
                      d.a='0;27'
                      base=d.codz
                      end

    when k=='\'  then do
                      d.z=92
                      d.s=124
                      d.c=28
                      d.a='0;43'
                      base=d.codz
                      end

    when k==';'  then do
                      d.z=59
                      d.s=58
                      d.c=
                      d.a='0;39'
                      base=d.codz
                      end

    when k=="'"  then do
                      d.z=39
                      d.s=34
                      d.c=
                      d.a='0;40'
                      base=d.codz
                      end

    when k==','  then do
                      d.z=44
                      d.s=60
                      d.c=
                      d.a='0;51'
                      base=d.codz
                      end

    when k=='.'  then do
                      d.z=46
                      d.s=62
                      d.c=
                      d.a='0;52'
                      base=d.codz
                      end

    when k=='/'  then do
                      d.z=47
                      d.s=63
                      d.c=
                      d.a='0;53'
                      base=d.codz
                      end

    when k=='`'  then do
                      d.z=96
                      d.s=126
                      d.c=
                      d.a='0;41'
                      base=d.codz
                      end

    when k=='HOMEKEYPAD'  then do
                               d.z='0;71'
                               d.s=55
                               d.c='0;119'
                               base=d.codz
                               end

    when k=='UPARROWKEYPAD'  then do
                                  d.z='0;72'
                                  d.s=55
                                  d.c='0;141'
                                  base=d.codz
                                  end

    when k=='PAGEUPKEYPAD'  then do
                                 d.z='0;73'
                                 d.s=57
                                 d.c='0;132'
                                 base=d.codz
                                 end

    when k=='LEFTARROWKEYPAD'  then do
                                    d.z='0;75'
                                    d.s=52
                                    d.c='0;115'
                                    base=d.codz
                                    end

    when k=='5KEYPAD'  then do
                            d.z='0;76'
                            d.s=53
                            d.c='0;143'
                            base=d.codz
                            end

    when k=='RIGHTARROWKEYPAD'  then do
                                     d.z='0;77'
                                     d.s=54
                                     d.c='0;116'
                                     base=d.codz
                                     end

    when k=='ENDKEYPAD'  then do
                              d.z='0;79'
                              d.s=49
                              d.c='0;117'
                              base=d.codz
                              end

    when k=='DOWNARROWKEYPAD'  then do
                                    d.z='0;80'
                                    d.s=50
                                    d.c='0;145'
                                    base=d.codz
                                    end

    when k=='PAGEDOWNKEYPAD'  then do
                                   d.z='0;81'
                                   d.s=51
                                   d.c='0;118'
                                   base=d.codz
                                   end

    when k=='INSERTKEYPAD'  then do
                                 d.z='0;82'
                                 d.s=48
                                 d.c='0;146'
                                 base=d.codz
                                 end

    when k=='DELETEKEYPAD'  then do
                                 d.z='0;83'
                                 d.s=46
                                 d.c='0;147'
                                 base=d.codz
                                 end

    when k=='ENTERKEYPAD'  then do
                                d.z=13
                                d.c=10
                                d.a='0;166'
                                base=d.codz
                                end

    when k=='/KEYPAD'  then do
                            d.z=47
                            d.s=d.z
                            d.c='0;142'
                            d.a='0;74'
                            base=d.codz
                            end

    when k=='*KEYPAD'  then do
                            d.z=42
                            d.s='o;144'
                            d.c='0;78'
                            base=d.codz
                            end

    when k=='-KEYPAD'  then do
                            d.z=45
                            d.s=d.z
                            d.c='0;149'
                            d.a='0;164'
                            base=d.codz
                            end

    when k=='+KEYPAD'  then do
                            d.z=43
                            d.s=d.z
                            d.c='0;150'
                            d.a='0;55'
                            base=d.codz
                            end
    otherwise  nop
    end   /*select*/

  if base\==''  then leave
  call er 50,'key' origk
  end    /*do 1*/

jy=words(y)
yy=

  do j=1  for jy
  w=word(y,j)
  lw=length(w)
  lc=left(w,1)
  rc2=right(w,2);  upper rc2

  if ((lc=='"' & rc2=='"X') | (lc=="'" & rc2=="'X")) & lw>3  then
     do
     if (lw-3)//2\==0  then call er 56,w 'hexdigits for the hexstring' w
     wm=substr(w,2,lw-3)
     if \isHex(wm)     then call er 40,w
     w=x2c(wm)
     end

  yy=yy w
  end   /*j*/
                                     /*if useAuto=1, then use AUTOENTER as is.*/
                                     /*if useAuto=0 & funcKey, then use ENTER.*/
if \useAuto & functionKey  then autoEnter=13
yy=substr(yy,2)
!!='1b'x"["                          /* ESC[s  ───►  save    cursor position. */
                                     /* ESC[u  ───►  restore cursor position. */
                                     /* ESC[1A ───►  move    cursor up 1 line.*/

@echo !!"s"!! || base';"'yy'";'autoEnter'p'!!"u"!!'1A'     /*issue the define.*/
nk=k
if cod\==''  then nk=codl"-"k

call $t '.Q=1' fops right(nk,max(length(nk),5)) "──►" yy
exit                                   /*stick a fork in it,  we're all done. */

/*═════════════════════════════one─liner subroutines══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
!all:  !!=!;!=space(!);upper !;call !FID;!nt=right(!var('OS'),2)=="NT";!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,"? ?SAMPLES ?AUTHOR ?FLOW")==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:  if symbol('!CALL')\=="VAR"  then !call=; return !call
!env:  !env='ENVIRONMENT';  if !sys=='MSDOS' | !brexx | !r4 | !roo  then !env='SYSTEM';  if !os2  then !env='OS2'!env;  !ebcdic=1=='f0'x;   return
!FID:  parse upper source !sys !fun !FID . 1 . . !fn !ft !fm .; call !sys; if !dos  then do; _=lastpos('\',!fn); !fm=left(!fn,_); !fn=substr(!fn,_+1); parse var !fn !fn '.' !ft; end; return word(0 !fn !ft !fm,1+("0"arg(1)))
!rex:  parse upper version !ver !vernum !verdate .;  !brexx='BY'==!vernum;  !kexx='KEXX'==!ver;  !pcrexx='REXX/PERSONAL'==!ver | 'REXX/PC'==!ver;  !r4='REXX-R4'==!ver;  !regina='REXX-REGINA'==left(!ver,11);  !roo='REXX-ROO'==!ver;  call !env;  return
!sys:  !cms=!sys=='CMS';  !os2=!sys=="OS2";  !tso=!sys=='TSO' | !sys=="MVS";  !vse=!sys=='VSE';  !dos=pos("DOS",!sys)\==0 | pos('WIN',!sys)\==0 | !sys=="CMD";  call !rex;    return
!var:  call !FID;  if !kexx  then return space(dosenv(arg(1)));   return space(value(arg(1),,!env))

$defkey:   !call=']$DEFKEY';  call "$DEFKEY" arg(1);  !call=;     return result
$t:        !call=']$T';       call "$T" arg(1);       !call=;     return result
appenda:   procedure;  parse arg x,_;  if right(x,length(_))\==_  then x=x || _;            return x
er:        parse arg _1,_2;  call '$ERR' "14"p(_1) p(word(_1,2) !FID(1)) _2;  if _1<0  then return _1;    exit result
halt:      call er .1
hasCol:    return pos(':',arg(1))\==0
homeDrive: if symbol('HOMEDRIVE')\=="VAR"  then homeDrive=p(!var('HOMEDRIVE') 'C:');   return homeDrive
isHex:     return datatype(arg(1),'X')
isint:     return datatype(arg(1),'W')
isNum:     return datatype(arg(1),'N')
isUpp:     return datatype(arg(1),'U')
it:        "ARG"(1);if rc==0  then return;  call er 68,rc arg(1)
noValue:   !sigl=sigl;  call er 17,!FID(2) !FID(3) !sigl condition('D') sourceline(!sigl)
p:         return word(arg(1),1)
prefixa:   procedure;  parse arg x,_;  if left(x,length(_))\==_  then x=_ || x;   return x
squish:    return space(translate(arg(1),,word(arg(2) ',',1)),0)
syntax:    !sigl=sigl;  call er 13,!FID(2) !FID(3) !sigl !cal() condition('D') sourceline(!sigl)
whenstamp: arg whenFID;  call lineout whenFID,strip(left(date('U'),6)left(date("S"),4) time() arg(2));  call lineout whenFID,' ';  call lineout whenFID;   return

```



## Ring


```ring

load "guilib.ring"

app = new qApp {
      win = new qWidget() {
                setWindowTitle("Don't accept Spaces")
                move(100,100)    resize(400,400)
                new qLineedit(win) {
                    myfilter = new qAllEvents(win) {
                    setkeypressevent("keypress()")
                }
                installeventfilter(myfilter)
                }
                show()
            }
            exec()
      }

func keypress
     nKey = myfilter.getkeycode()
     switch nKey
            on 16777264 see "You pressed F1 " + nl
            on 16777265 see "You pressed F2 " + nl
     off

```

Output:

```txt

You pressed F1
You pressed F2

```



## Ruby

{{libheader|Shoes}}
Here's a sample from the Shoes manual showing how to capture key sequences. This is application specific.


```ruby
Shoes.app do
  @info = para "NO KEY is PRESSED."
  keypress do |k|
    @info.replace "#{k.inspect} was PRESSED."
  end
end
```


A more specific example, using some emacs bindings


```ruby
Shoes.app do
  keypress do |key|
    case key
    when "\x04"  # control-d
      delete_char
    when :backspace
      delete_previous_char
    when "\x14"  # control-t
      transpose_chars
    when :alt_t
      transpose_words
    when "\x18"  # control-x
      @ctrl_x = true
    when "\x13"  # control-s
      if @ctrl_x
        save_text
        @ctrl_x = false
      end
    when "\x11"  # control-q
      exit if @ctrl_x
    end
  end
end
```



## Scala


### Java Swing Interoperability

{{libheader|Scala Java Swing interoperability}}

```Scala
import java.awt.event.{KeyAdapter, KeyEvent}

import javax.swing.{JFrame, JLabel, WindowConstants}


object KeyboardMacroDemo extends App {
  val directions = "<html><b>Ctrl-S</b> to show frame title
" + "<b>Ctrl-H</b> to hide it</html>"

  new JFrame {
    add(new JLabel(directions))

    addKeyListener(new KeyAdapter() {
      override def keyReleased(e: KeyEvent): Unit = {
        if (e.isControlDown && e.getKeyCode == KeyEvent.VK_S) setTitle("Hello there")
        else if (e.isControlDown && e.getKeyCode == KeyEvent.VK_H) setTitle("")
      }
    })

    pack()
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setVisible(true)
  }

}
```



## Tcl

{{libheader|Tk}}
All Tk bindings are bound to a context that is no wider than a particular application and is frequently smaller (e.g., a single dialog box or an individual widget).

```tcl
package require Tk
# Show off some emacs-like bindings...
pack [label .l -text "C-x C-s to save, C-x C-c to quit"]
focus .
bind . <Control-x><Control-s> {
    tk_messageBox -message "We would save here"
}
bind . <Control-x><Control-c> {exit}
```

===Key-to-key mapping macros===
A more direct macro-like facility (substituting one key sequence for another) would be:

```tcl>bind . <F1
 {
    foreach c [split "Macro demo!" {}] {
        event generate %W $c
    }
}
```

This can then be wrapped up like this:

```tcl
package require Tk
proc sendMacro {w string} {
    foreach c [split $string {}] {
        # Will not work for “<” character...
        event generate $w $c
    }
}
proc macro {key translation} {
    bind . <$key> [list sendMacro %W [string map {% %%} $translation]]
}

# Some demonstration macros
macro F1 "Help me!"
macro F2 "You pressed the F2 key"
macro F3 "I'm getting bored here..."
pack [text .t];   # A place for you to test the macros
```



## Vedit macro language


```vedit
// Configure a key to access menu item.
// The menu item may then contain the commands directly, or it may call a macro from disk.
// This has the advantage that the key binding is shown in the menu.
 Key_Add("Shft-F6","[MENU]TV", OK)

// Configure a key to perform visual mode edit operations (similar to recorded key macro):
 Key_Add("Numpad.Enter", "[CURSOR LEFT][LINE END][RETURN]", OK)

// Configure a key to execute macro language commands:
 Key_Add("Numpad+", '[VISUAL EXIT] if(BB!=-1) { RCB(10,BB,BE) BB(-1) S("|@(10)",SET+CONFIRM) } else { S("",CONFIRM) }', OK)

// Configure a key to call a macro from disk. The old key assignment is not removed.
 Key_Add("Ctrl-Shft-N",'[VISUAL EXIT] Call_File(100,"aspell.vdm","NEXT_ERROR")',INSERT+OK)

// Configure a two-key sequence (C-x C-c to save file).
 Key_Add("^X C","[MENU]FS", OK)

// Remove a key assignment. If INSERT option was used when the key was assigned, the old assignment will come in effect again.
 Key_Delete("Ctrl-Shft-N")
```




{{omit from|ACL2}}
{{omit from|C++}}
{{omit from|M4}}
{{omit from|Modula-3}}
{{omit from|PARI/GP}}
