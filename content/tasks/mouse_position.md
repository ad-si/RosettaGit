+++
title = "Mouse position"
description = ""
date = 2019-07-07T04:34:02Z
aliases = []
[extra]
id = 4261
[taxonomies]
categories = ["task", "GUI"]
tags = []
languages = [
  "actionscript3",
  "ada",
  "applescript",
  "autohotkey",
]
+++

## Task

{{task|GUI}} [[Category:Testing]] [[Category:Hardware]] [[Category:Pointing devices]]
Get the current location of the mouse cursor relative to the active window.
Please specify if the window may be externally created.


## ActionScript

This shows the mouse position in a text field at the bottom-right corner
and updates when the mouse is moved.

```ActionScript3

package  {

    import flash.display.Sprite;
    import flash.events.Event;
    import flash.text.TextField;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFormat;

    public class MousePosition extends Sprite {

        private var _textField:TextField = new TextField();

        public function MousePosition() {
            if ( stage ) init();
            else addEventListener(Event.ADDED_TO_STAGE, init);
        }

        private function init(e:Event = null):void {
            removeEventListener(Event.ADDED_TO_STAGE, init);

            _textField.autoSize = TextFieldAutoSize.RIGHT;
            _textField.x = stage.stageWidth - 10;
            _textField.defaultTextFormat = new TextFormat(null, 15);
            _textField.text = "Mouse position: X = 0, Y = 0";
            _textField.y = stage.stageHeight - _textField.textHeight - 14;
            addChild(_textField);

            addEventListener(Event.ENTER_FRAME, _onEnterFrame);
        }

        private function _onEnterFrame(e:Event):void {
            _textField.text = "Mouse position: X = " + stage.mouseX + ", Y = " + stage.mouseY;
        }

    }

}

```



## Ada

The [[GTK]] procedure is Get_Pointer.
It returns mouse coordinates relatively to a window (internally created).
The following program shows a button, which when pressed indicates coordinates of the mouse pointer relatively to the main window:

```Ada
with GLib;        use GLib;
with Gtk.Button;  use Gtk.Button;
with Gtk.Label;   use Gtk.Label;
with Gtk.Window;  use Gtk.Window;
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Table;   use Gtk.Table;

with Gtk.Handlers;
with Gtk.Main;

procedure Tell_Mouse is
   Window : Gtk_Window;
   Grid   : Gtk_Table;
   Button : Gtk_Button;
   Label  : Gtk_Label;

   package Handlers is new Gtk.Handlers.Callback (Gtk_Widget_Record);
   package Return_Handlers is
      new Gtk.Handlers.Return_Callback (Gtk_Widget_Record, Boolean);

   function Delete_Event (Widget : access Gtk_Widget_Record'Class)
      return Boolean is
   begin
      return False;
   end Delete_Event;

   procedure Destroy (Widget : access Gtk_Widget_Record'Class) is
   begin
      Gtk.Main.Main_Quit;
   end Destroy;

   procedure Clicked (Widget : access Gtk_Widget_Record'Class) is
      X, Y : GInt;
   begin
      Get_Pointer (Window, X, Y);
      Set_Text (Label, "At" & GInt'Image (X) & GInt'Image (Y));
   end Clicked;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk_New (Grid, 1, 2, False);
   Add (Window, Grid);
   Gtk_New (Label);
   Attach (Grid, Label, 0, 1, 0, 1);
   Gtk_New (Button, "Click me");
   Attach (Grid, Button, 0, 1, 1, 2);
   Return_Handlers.Connect
   (  Window,
      "delete_event",
      Return_Handlers.To_Marshaller (Delete_Event'Access)
   );
   Handlers.Connect
   (  Window,
      "destroy",
      Handlers.To_Marshaller (Destroy'Access)
   );
   Handlers.Connect
   (  Button,
      "clicked",
      Handlers.To_Marshaller (Clicked'Access)
   );
   Show_All (Grid);
   Show (Window);

   Gtk.Main.Main;
end Tell_Mouse;
```



## AppleScript

The window can be any window on the system, externally created or otherwise, provided it can acquire focus.  Mouse location is retrieved with help from AppleScriptObjC.


```AppleScript
use application "System Events"

property process : a reference to (first process whose frontmost = true)
property window : a reference to front window of my process


set [Wx, Wy] to my window's position
set [Cx, Cy] to {x, y} of the mouse's coordinates()


script mouse
	use framework "Foundation"

	property this : a reference to current application
	property NSEvent : a reference to NSEvent of this
	property NSScreen : a reference to NSScreen of this

	on coordinates()
		-- Screen dimensions
		set display to NSDeviceSize of deviceDescription() ¬
			of item 1 of NSScreen's screens() as record

		-- Mouse position relative to bottom-left of screen
		set mouseLoc to (NSEvent's mouseLocation as record)
		-- Flip mouse y-coordinate so it's relative to top of screen
		set mouseLoc's y to (display's height) - (mouseLoc's y)

		mouseLoc
	end coordinates
end script

```


```AppleScript
-- Cursor position relative to active window
[Cx - Wx as integer, Cy - Wy as integer]
```



## AutoHotkey

The window may be an externally created window.

```AutoHotkey
#i:: ; (Optional) Assigns a hotkey to display window info, Windows+i
MouseGetPos, x, y ; Gets x/y pos relative to active window
WinGetActiveTitle, WinTitle ; Gets active window title
Traytip, Mouse position, x: %x%`ny: %y%`rWindow: %WinTitle%, 4 ; Displays the info as a Traytip for 4 seconds
return
```



### with DllCall

Source: [https://github.com/jNizM/AHK_DllCall_WinAPI/ GetCursorPos @github] by jNizM

```AutoHotkey
GetCursorPos()
{
    static POINT, init := VarSetCapacity(POINT, 8, 0) && NumPut(8, POINT, "Int")
    if (DllCall("User32.dll\GetCursorPos", "Ptr", &POINT))
    {
        return, { 0 : NumGet(POINT, 0, "Int"), 1 : NumGet(POINT, 4, "Int") }
    }
}
GetCursorPos := GetCursorPos()

MsgBox, % "GetCursorPos function`n"
        . "POINT structure`n`n"
        . "x-coordinate:`t`t"     GetCursorPos[0]   "`n"
        . "y-coordinate:`t`t"     GetCursorPos[1]
```

Source: [https://github.com/jNizM/AHK_DllCall_WinAPI/ GetPhysicalCursorPos @github] by jNizM

```AutoHotkey
GetPhysicalCursorPos()
{
    static POINT, init := VarSetCapacity(POINT, 8, 0) && NumPut(8, POINT, "Int")
    if (DllCall("User32.dll\GetPhysicalCursorPos", "Ptr", &POINT))
    {
        return, { 0 : NumGet(POINT, 0, "Int"), 1 : NumGet(POINT, 4, "Int") }
    }
}
GetPhysicalCursorPos := GetPhysicalCursorPos()

MsgBox, % "GetPhysicalCursorPos function`n"
        . "POINT structure`n`n"
        . "x-coordinate:`t`t"     GetPhysicalCursorPos[0]   "`n"
        . "y-coordinate:`t`t"     GetPhysicalCursorPos[1]
```



## BBC BASIC

The mouse coordinates are relative to the bottom-left corner
of the BBC BASIC main output window:

```bbcbasic
      MOUSE xmouse%, ymouse%, buttons%
      PRINT xmouse%, ymouse%
```



## C

```c
#include <stdio.h>
#include <X11/Xlib.h>

int main()
{
  Display *d;
  Window inwin;      /* root window the pointer is in */
  Window inchildwin; /* child win the pointer is in */
  int rootx, rooty; /* relative to the "root" window; we are not interested in these,
                       but we can't pass NULL */
  int childx, childy;  /* the values we are interested in */
  Atom atom_type_prop; /* not interested */
  int actual_format;   /* should be 32 after the call */
  unsigned int mask;   /* status of the buttons */
  unsigned long n_items, bytes_after_ret;
  Window *props; /* since we are interested just in the first value, which is
		    a Window id */

  /* default DISPLAY */
  d = XOpenDisplay(NULL);

  /* ask for active window (no error check); the client must be freedesktop
     compliant */
  (void)XGetWindowProperty(d, DefaultRootWindow(d),
			   XInternAtom(d, "_NET_ACTIVE_WINDOW", True),
			   0, 1, False, AnyPropertyType,
			   &atom_type_prop, &actual_format,
			   &n_items, &bytes_after_ret, (unsigned char**)&props);

  XQueryPointer(d, props[0], &inwin,  &inchildwin,
		&rootx, &rooty, &childx, &childy, &mask);
  printf("relative to active window: %d,%d\n", childx, childy);

  XFree(props);           /* free mem */
  (void)XCloseDisplay(d); /* and close the display */
  return 0;
}
```



## Common Lisp


With the ltk library.


```lisp

(ql:quickload "ltk")
(in-package :ltk-user)
(defun motion (event)
    (format t "~a x position is ~a~&" event (event-x event)))

(with-ltk ()
    ;; create a small window. Enter the mouse to see lots of events.
    (bind *tk* "<Motion>" #'motion))

```

This prints a lot of events of the form


  #S(EVENT
   :X 0
   :Y 85
   :KEYCODE ??
   :CHAR ??
   :WIDTH ??
   :HEIGHT ??
   :ROOT-X 700
   :ROOT-Y 433
   :MOUSE-BUTTON ??)

The <code>#S</code> indicates we get a structure, so we can access the x position with <code>(event-x event)</code>.

=={{header|c_sharp|C#}}==
Writes the absolute Mouse Position of the Screen into the Console

```c#

using System;
using System.Windows.Forms;
static class Program
{
    [STAThread]
    static void Main()
    {
        Console.WriteLine(Control.MousePosition.X);
        Console.WriteLine(Control.MousePosition.Y);
    }
}

```


## Clojure


```lisp
(let [point (.. java.awt.MouseInfo getPointerInfo getLocation)] [(.getX point) (.getY point)])
```



## Delphi

Shows mouse-position on a label on the form.


```Delphi
procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   lblMousePosition.Caption := ('X:' + IntToStr(X) + ', Y:' + IntToStr(Y));
end;
```



'''Delphi Console Program'''

The following program will help capture the mouse position
with the help of Windows and classes units.


```Delphi
program Project1;
{$APPTYPE CONSOLE}
uses
  SysUtils, Controls, Windows;

var
  MyMouse : TMouse;
begin
  MyMouse := TMouse.Create;
  While True do
  begin
    WriteLn('(X, y) = (' + inttostr(TPoint(MyMouse.CursorPos).x) + ',' + inttostr(TPoint(MyMouse.CursorPos).y) + ')');
    sleep(300);
  end
end.
```



## EasyLang


[https://easylang.online/apps/run.html?code=on%20mouse_move%0Acolor%20889%0Arect%20100%20100%0Acolor%20000%0Atext%20mouse_x%20%26%20%22%20%22%20%26%20mouse_y%0A. Run it]

<lang>on mouse_move
  color 889
  rect 100 100
  color 000
  text mouse_x & " " & mouse_y
.
```



## EchoLisp

When the '''plot''' library is loaded, the mouse position - relative to the plotting area coordinates, is displayed inside the info panel.

```lisp

(lib 'plot)
(plot-x-minmax 10) ; set logical dimensions of plotting area
(plot-y-minmax 100)
  → (("x" 0 10) ("y" 0 100))
;; press ESC to see the canvas
;; the mouse position is displayed as , for example,  [ x: 5.6 y : 88.7]
;; 0 <= x <= 10, 0 <= y <= 100

```


--[[User:Neo.abhinav|Neo.abhinav]] 17:00, 6 May 2011 (UTC)


## Elm


```elm
import Graphics.Element exposing (Element, show)
import Mouse


main : Signal Element
main =
  Signal.map show Mouse.position
```



## Emacs Lisp

For Emacs' own frames (which is what Emacs calls its window system windows),
the frame the mouse is over and where the mouse is within that frame can be obtained with


```Lisp
(mouse-pixel-position)
=>
(FRAME . (X . Y))
```


Or <code>mouse-position</code> for characters instead of pixels.
Works when the mouse is over any Emacs frame, not just when that frame is the active focused window.

There's no particularly easy way to inquire about a non-Emacs focused window,
only generic X11 or similar approaches run externally.


## ERRE

This example program, taken from distribution disk, shows the mouse position in a text field at the bottom-right corner and updates when the mouse is moved.

```ERRE

!
! MOUSE WITH 'MOUSE.LIB' LIBRARY
!

PROGRAM MOUSE

!$KEY

!$INCLUDE="PC.LIB"

!$INCLUDE="MOUSE.LIB"

PROCEDURE GETMONITORTYPE(->MONITOR$)
      !$RCODE="DEF SEG=0"
      STATUS=PEEK($463)
      !$RCODE="DEF SEG"
      MONITOR$=""
      IF STATUS=$B4 THEN
         !$RCODE="STATUS=(INP(&H3BA) AND &H80)"
         FOR DELAYLOOP=1 TO 30000 DO
             !$RCODE="XX=((INP(&H3BA) AND &H80)<>STATUS)"
             IF XX THEN MONITOR$="HERC" END IF
         END FOR
         IF MONITOR$="" THEN MONITOR$="MONO" END IF
       ELSE
          REGAX%=$1A00
          EXECUTEASM($10)
          IF (REGAX% AND $FF)=$1A THEN
                MONITOR$="VGA"
             ELSE
                REGAX%=$1200  REGBX%=$10
                EXECUTEASM($10)
                IF (REGBX% AND $FF)=$10 THEN
                     MONITOR$="CGA"
                  ELSE
                     MONITOR$="EGA"
                END IF
          END IF
      END IF
END PROCEDURE

BEGIN
    INITASM
    GETMONITORTYPE(->MONITOR$)
    COLOR(7,0)
    CLS
    LOCATE(1,50)  PRINT("MONITOR TYPE  ";MONITOR$)
    MOUSE_RESETANDSTATUS(->STATUS,BUTTONS)
    IF STATUS<>-1 THEN
        BEEP
        CLS
        PRINT("MOUSE DRIVER NOT INSTALLED OR MOUSE NOT FOUND")
        REPEAT
           GET(IN$)
        UNTIL IN$<>""
     ELSE
        MOUSE_SETEXTCURSOR
        MOUSE_SETCURSORLIMITS(8,199,0,639)
        MOUSE_SETSENSITIVITY(30,30,50)
        MOUSE_SHOWCURSOR
        REPEAT
          OLDX=X  OLDY=Y
          MOUSE_GETCURSORPOSITION(->X,Y,LEFT%,RIGHT%,BOTH%,MIDDLE%)
          GET(IN$)
          COLOR(15,0)
          LOCATE(1,2)
          PRINT("X =";INT(X/8)+1;"  Y =";INT(Y/8)+1;"  ";)
          IF LEFT%   THEN LOCATE(1,37) COLOR(10,0)  PRINT("LEFT";) END IF
          IF RIGHT%  THEN LOCATE(1,37) COLOR(12,0)  PRINT("RIGHT";) END IF
          IF MIDDLE% THEN LOCATE(1,37) COLOR(14,0)  PRINT("MIDDLE";) END IF
          IF NOT RIGHT% AND NOT LEFT% AND NOT MIDDLE% THEN
              LOCATE(1,37)  PRINT("       ";)
          END IF
          IF NOT (X=OLDX AND Y=OLDY) THEN MOUSE_SHOWCURSOR END IF
        UNTIL IN$=CHR$(27)
   END IF
   MOUSE_HIDECURSOR
   CLS
END PROGRAM

```



## Factor

Works only in the graphical listener.
Replaces the text in the button with the relative and absolute coordinates of the mouse

```factor
: replace-text ( button text -- )
    [ drop children>> pop drop ] [ >label add-gadget drop ] 2bi ;
: present-locations ( loc1 loc2 -- string )
    [
      first2 [ number>string ] bi@ "," glue
    ] bi@ ";" glue ;
: example ( -- ) "click me"
[
  dup hand-rel ! location relative to the button
  hand-loc get ! location relative to the window
  present-locations replace-text
]
<border-button> gadget. ;

```


=={{header|F_Sharp|F#}}==
Have to do a lot of interop here.
Primarily because the active window may not be a .NET form/control.
If the question was for the point relative to the current window,
life would be much simpler.

```fsharp
open System.Windows.Forms
open System.Runtime.InteropServices

#nowarn "9"
[<Struct; StructLayout(LayoutKind.Sequential)>]
type POINT =
    new (x, y) = { X = x; Y = y }
    val X : int
    val Y : int

[<DllImport("user32.dll")>]
extern nativeint GetForegroundWindow();
[<DllImport("user32.dll", CharSet=CharSet.Auto, SetLastError=true, ExactSpelling=true)>]
extern int ScreenToClient(nativeint hWnd, POINT &pt);

let GetMousePosition() =
    let hwnd = GetForegroundWindow()
    let pt = Cursor.Position
    let mutable ptFs = new POINT(pt.X, pt.Y)
    ScreenToClient(hwnd, &ptFs) |> ignore
    ptFs

```



## Gambas


In gambas, the position of the pointer can only be determined
when the click button is held down.
A window with a drawing area is required, because
this is the only widget that can track pointer movement within gambas.


```gambas

PUBLIC SUB Form1_MouseMove()
PRINT mouse.X
PRINT Mouse.Y
END

```



## Go

The active window may be externally created.

```go
package main

import (
    "fmt"
    "github.com/go-vgo/robotgo"
)

func isInside(x, y, w, h, mx, my int) bool {
    rx := x + w - 1
    ry := y + h - 1
    return mx >= x && mx <= rx && my >= y && my <= ry
}

func main() {
    name := "gedit" // say
    fpid, err := robotgo.FindIds(name)
    if err == nil && len(fpid) > 0 {
        pid := fpid[0]
        robotgo.ActivePID(pid) // make gedit active window
        x, y, w, h := robotgo.GetBounds(pid)
        fmt.Printf("The active window's top left corner is at (%d, %d)\n", x, y)
        fmt.Printf("Its width is %d and its height is %d\n", w, h)
        mx, my := robotgo.GetMousePos()
        fmt.Printf("The screen location of the mouse cursor is (%d, %d)\n", mx, my)
        if !isInside(x, y, w, h, mx, my) {
            fmt.Println("The mouse cursor is outside the active window")
        } else {
            wx := mx - x
            wy := my - y
            fmt.Printf("The window location of the mouse cursor is (%d, %d)\n", wx, wy)
        }
    } else {
        fmt.Println("Problem when finding PID(s) of", name)
    }
}
```


A first run after placing mouse cursor inside active window:

```txt

The active window's top left corner is at (547, 167)
Its width is 792 and its height is 506
The screen location of the mouse cursor is (949, 446)
The window location of the mouse cursor is (402, 279)

```

A second run after moving mouse cursor outside active window:

```txt

The active window's top left corner is at (547, 167)
Its width is 792 and its height is 506
The screen location of the mouse cursor is (334, 639)
The mouse cursor is outside the active window

```



## Groovy

Based on Java solution:

```groovy
1.upto(5) {
    Thread.sleep(1000)
    def p = java.awt.MouseInfo.pointerInfo.location
    println "${it}: x=${p.@x} y=${p.@y}"
}
```


Sample output:

```txt
1: x=857 y=0
2: x=765 y=447
3: x=0 y=535
4: x=715 y=283
5: x=1335 y=83
```



## HicEst

Mouse click positions for windows created internally.
X and Y are in units of current xy axes (optional: invisible).

```hicest
   WINDOW(WINdowhandle=handle)
   AXIS(WINdowhandle=handle, MouSeCall=Mouse_Callback, MouSeX=X, MouSeY=Y)
 END

SUBROUTINE Mouse_Callback()
   WRITE(Messagebox, Name) X, Y
 END
```


=={{header|Icon}} and {{header|Unicon}}==
In Icon/Unicon the mouse position may be tracked between button presses for any window created by the program.
The following code snippet taken from the Icon Graphics Book on page 197-198 shows how to track the mouse.

```Icon
until *Pending() > 0 & Event() == "q" do { # loop until there is something to do
   px := WAttrib("pointerx")
   py := WAttrib("pointery")
   #  do whatever is needed
   WDelay(5)  # wait and share processor
   }

```



## Java

The mouse position can be checked at any time by calling

```java5
Point mouseLocation = MouseInfo.getPointerInfo().getLocation();
```

This returns a point on the entire screen, rather than relative to a particular window. This call can be combined with <code>getLocation()</code> from any <code>Component</code> (including a <code>Frame</code>, which is a window) to get the location relative to that <code>Component</code>.


## JavaScript


In a browser environment, it's impossible to actually get the cursor position
at the specific moment.
You must wait for user input (movement, click, etc).
One of many ways to add an event listener:


```javascript
document.addEventListener('mousemove', function(e){
  var position = { x: e.clientX, y: e.clientY }
}
```


In the above example, the window may not be external.
It must in fact be a web browser window, which runs the script.



## Julia


```julia
using Gtk

const win = GtkWindow("Get Mouse Position", 600, 800)
const butn = GtkButton("Click Me Somewhere")
push!(win, butn)

callback(b, evt) = set_gtk_property!(win, :title, "Mouse Position: X is $(evt.x), Y is $(evt.y)")
signal_connect(callback, butn, "button-press-event")

showall(win)

c = Condition()
endit(w) = notify(c)
signal_connect(endit, win, :destroy)
wait(c)

```



## Kotlin

```scala
// version 1.1.2

import java.awt.MouseInfo

fun main(args: Array<String>) {
    (1..5).forEach {
        Thread.sleep(1000)
        val p = MouseInfo.getPointerInfo().location  // gets screen coordinates
        println("${it}: x = ${"%-4d".format(p.x)} y = ${"%-4d".format(p.y)}")
    }
}
```

Sample output:
```txt

1: x = 752  y = 483
2: x = 1112 y = 478
3: x = 1331 y = 511
4: x = 269  y = 562
5: x = 247  y = 505

```



## Liberty BASIC

This example gets the mouse position based on the active window.
Click on other windows to get relative mouse position based on those windows.

```lb
    nomainwin

    UpperLeftX = DisplayWidth-WindowWidth
    UpperLeftY = DisplayHeight-WindowHeight

    struct point, x as long, y as long

    stylebits #main.st ,0,0,_WS_EX_STATICEDGE,0
    statictext #main.st "",16,16,100,26

    stylebits #main ,0,0,_WS_EX_TOPMOST,0
    open "move your mouse" for window_nf as #main

    #main "trapclose [quit]"
    timer 100, [mm]
    wait

[mm]
    CallDll #user32, "GetForegroundWindow", WndHandle as uLong
    #main.st CursorPos$(WndHandle)
    wait

[quit]
    close #main
    end

function CursorPos$(handle)
    Calldll #user32, "GetCursorPos",_
        point as struct,_
        result as long
    Calldll #user32, "ScreenToClient",_
        handle As Ulong,_
        point As struct,_
        result as long
    x = point.x.struct
    y = point.y.struct
    CursorPos$=x; ",";y
end function
```



## Lingo


```lingo
put _mouse.mouseLoc
-- point(310, 199)
```



## Logo


```logo
show mousepos   ; [-250 250]
```



## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
	\\ works when console is the active window
	\\ pressing right mouse button exit the loop
	While mouse<>2
		Print mouse.x, mouse.y
	End While
	\\ end of part one, now we make a form with title Form1 (same name as the variable name)
	Declare Form1 Form
	Layer Form1 {
		window 16, 10000,8000  ' 16pt font at maximum 10000 twips x 8000 twips
		cls #335522, 1 \\ from 2nd line start the split screen (for form's layer)
		pen 15 ' white
	}
	Function Form1.MouseMove {
		Read new button, shift, x, y  ' we use new because call is local, same scope as Checkit.
		Layer Form1 {
			print x, y, button
			refresh
		}
	}
	Function Form1.MouseDown {
		Read new button, shift, x, y
		\\ when we press mouse button we print in console
		\\ but only the first time
		print x, y, button
		refresh
	}
	\\ open Form1 as modal window above console
	Method Form1, "Show", 1
	Declare Form1 Nothing
}
CheckIt

```




## Mathematica


```Mathematica
MousePosition["WindowAbsolute"]
```


## MAXScript

Creates a window on the screen and shows mouse position as it moves.


```MAXScript

try destroydialog mousePos catch ()

rollout mousePos "Mouse position" width:200
(
	label mousePosText "Current mouse position:" pos:[0,0]
	label mousePosX "" pos:[130,0]
	label mousePosSep "x" pos:[143,0]
	label mousePosY "" pos:[160,0]

	timer updateTimer interval:50 active:true

	on updateTimer tick do
	(
		mousePosX.text = (mouse.screenpos.x as integer) as string
		mousePosY.text = (mouse.screenpos.y as integer) as string
	)

)

createdialog mousepos

```



## MATLAB


```MATLAB
get(0,'PointerLocation')
```



## Nanoquery


```nanoquery
import Nanoquery.Util.Windows

// a function to handle the mouse moved event
def mouse_moved($caller, $e)
	println "(" + $e.getX() + ", " + $e.getY() + ")"
end

// create a window, set the handler for mouse moved, and show it
$w = new("Window")
$w.setSize(500, 500)
$w.setHandler($w.mouseMoved, $mouse_moved)
$w.show()
```



## OCaml

equivalent to the C example, uses the Xlib binding [http://www.linux-nantes.org/~fmonnier/OCaml/Xlib/ ocaml-xlib]

```OCaml
open Xlib

let () =
  let d = xOpenDisplay "" in

  (* ask for active window (no error check);
     the client must be freedesktop compliant *)
  let _, _, _, _, props =
    xGetWindowProperty_window d
        (xDefaultRootWindow d)
        (xInternAtom d "_NET_ACTIVE_WINDOW" true)
        0 1 false AnyPropertyType
  in

  let _, _, _, child, _ = xQueryPointer d props in
  begin match child with
  | Some(_, childx, childy) ->
      Printf.printf "relative to active window: %d,%d\n%!" childx childy;
  | None ->
      print_endline "the pointer is not on the same screen as the specified window"
  end;

  xCloseDisplay d;
;;
```




## Octave

To get the X,Y coordinates of N mouse clicks in the current figure,
one can use this:

```Octave
[X, Y, BUTTONS] = ginput(N);
```

Example:

```txt
>>  [X, Y, BUTTONS] = ginput(4)
X =

   0.16113
   0.26300
   0.44343
   0.23164

Y =

   0.87357
   0.87596
   0.73330
   0.54911

BUTTONS =

   1
   3
   1
   2

```



## Oz

Repeatedly shows the mouse coordinates relative to the foremost window of the application.

```oz
declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}
  WindowClosed = {NewCell false}
  Label
  Window = {QTk.build
	    td(action:proc {$} WindowClosed := true {Window close} end
	       label(text:"" handle:Label))}
in
  {Window show}

  for while:{Not @WindowClosed} do
     TopmostWindow = {List.last {String.tokens {Tk.return wm(stackorder '.')} & }}
     Winfo = {Record.mapInd winfo(rootx:_ rooty:_ pointerx:_ pointery:_)
	      fun {$ I _}
		 {Tk.returnInt winfo(I TopmostWindow)}
	      end}
  in
     {Label set(text:"x: "#(Winfo.pointerx - Winfo.rootx)
		#", y: "#(Winfo.pointery - Winfo.rooty))}
     {Delay 250}
  end
```



## Perl

==={{libheader|Perl/SDL}}===
The following code will use the SDL module, a wrapper for the libSDL C-library.
When you move the mouse over the created window,
the mouse position get printed and the program terminates.

```Perl
use SDL;
use SDL::Events;
use SDLx::App;

my $app = SDLx::App->new;
$app->add_event_handler( sub {
	my $event = shift;
	if( $event->type == SDL_MOUSEMOTION ) {
		printf( "x=%d y=%d\n", $event->motion_x, $event->motion_y );
		$app->stop
	}
} );
$app->run;

```

```txt
x=15 y=304
```



## Perl 6

```perl6
use java::awt::MouseInfo:from<java>
;

given MouseInfo.getPointerInfo.getLocation {
    say .getX, 'x', .getY;
}
```


An implementation that will work on any X11 windowing system. Reports mouse position, window ID# and window name for whichever window the mouse pointer is over. Automatically moves mouse for hands-off testing purposes.

```perl6
use X11::libxdo;
my $xdo = Xdo.new;

my ($dw, $dh) = $xdo.get-desktop-dimensions( 0 );

sleep .25;

for ^$dw -> $mx {
    my $my = (($mx / $dh * τ).sin * 500).abs.Int + 200;
    $xdo.move-mouse( $mx, $my, 0 );
    my ($x, $y, $window-id, $screen) = $xdo.get-mouse-info;
    my $name = (try $xdo.get-window-name($window-id) if $window-id)
       // 'No name set';

    my $line = "Mouse location: x=$x : y=$y\nWindow under mouse:\nWindow ID: " ~
       $window-id ~ "\nWindow name: " ~ $name ~ "\nScreen #: $screen";

    print "\e[H\e[J", $line;
    sleep .001;
}

say '';
```



## Phix

The following example shows three labels being updated with the mouse position.

The globalmotion label is updated whenever the mouse moves anywhere within the window, but not outside it.

The canvasmotion label is updated whenever the mouse moves within the canvas, but not elsewhere in the window (or beyond).

The timer label is updated every three seconds and gets mouse positions anywhere on the whole screen, whether it moves or not.

Note that canvasmotion coordinates are relative to the top left of the canvas, whereas the other two are absolute.
```Phix
--
-- demo\rosetta\Mouse_position.exw
--
include pGUI.e

Ihandle global_lbl, canvas_lbl, timer_lbl

function globalmotion_cb(integer x, integer y, atom /*pStatus*/)
    IupSetStrAttribute(global_lbl,"TITLE","globalmotion_cb %d, %d",{x,y})
    return IUP_DEFAULT
end function

function canvas_motion_cb(Ihandle /*canvas*/, integer x, integer y, atom pStatus)
    IupSetStrAttribute(canvas_lbl,"TITLE","canvasmotion_cb %d, %d",{x,y})
    return IUP_DEFAULT;
end function

function OnTimer(Ihandle /*ih*/)
    integer {x,y} = IupGetIntInt(NULL,"CURSORPOS")
    IupSetStrAttribute(timer_lbl,"TITLE","timer %d, %d",{x,y})
    return IUP_IGNORE
end function

function esc_close(Ihandle /*ih*/, atom c)
    return iff(c=K_ESC?IUP_CLOSE:IUP_CONTINUE)
end function

procedure main()
Ihandle separator1, separator2,
        canvas, frame_1, frame_2,
        dialog

    IupOpen()

    global_lbl = IupLabel("Move the mouse anywhere on the window","EXPAND=HORIZONTAL")
    separator1 = IupLabel(NULL,"SEPARATOR=HORIZONTAL")
    canvas_lbl = IupLabel("Move the mouse anywhere on the canvas","EXPAND=HORIZONTAL")
    separator2 = IupLabel(NULL,"SEPARATOR=HORIZONTAL")
    timer_lbl  = IupLabel("This one runs on a three second timer","EXPAND=HORIZONTAL")

    frame_1 = IupFrame(IupVbox({global_lbl,
                                separator1,
                                canvas_lbl,
                                separator2,
                                timer_lbl}),
                      "TITLE=IupLabel, SIZE=200x")

    canvas = IupCanvas("MOTION_CB", Icallback("canvas_motion_cb"),
                       "EXPAND=HORIZONTAL, RASTERSIZE=200x200")
    frame_2 = IupFrame(canvas, "TITLE=IupCanvas")

    dialog = IupDialog(IupHbox({frame_1,frame_2}, "MARGIN=5x5, GAP=5"))
    IupSetAttribute(dialog,"TITLE","Mouse motion");
    IupSetCallback(dialog, "K_ANY", Icallback("esc_close"));

    IupSetGlobal("INPUTCALLBACKS", "Yes");
    IupSetGlobalFunction("GLOBALMOTION_CB", Icallback("globalmotion_cb"));

    Ihandle hTimer = IupTimer(Icallback("OnTimer"), 3000)

    IupShow(dialog)

    IupMainLoop()
    IupClose()
end procedure
main()
```



## PicoLisp

The following works in an XTerm window.
After calling (mousePosition), click
into the current terminal window.
The returned value is (X . Y), where X is the
column and Y the line number.

```PicoLisp
(de mousePosition ()
   (prog2
      (prin "^[[?9h")  # Mouse reporting on
      (and
         (= "^[" (key))
         (key 200)
         (key 200)
         (key)
         (cons
            (- (char (key)) 32)
            (- (char (key)) 32) ) )
      (prin "^[[?9l") ) )  # Mouse reporting off
```

```txt
: (mousePosition)
-> (7 . 3)
```



## PureBasic

The mouse position can be obtained by these two commands:

```PureBasic
x = WindowMouseX(#MyWindow)
y = WindowMouseY(#MyWindow)
```

This example repeatedly shows the mouse coordinates
relative to the window created in the application.

```PureBasic
#MyWindow = 0
#Label_txt = 0
#MousePos_txt = 1
If OpenWindow(#MyWindow,0,0,200,200,"Test",#PB_Window_SystemMenu)
  TextGadget(#Label_txt,0,0,100,20,"Mouse Position (x,y):",#PB_Text_Right)
  TextGadget(#MousePos_txt,120,0,60,20,"()")

  Repeat
    Repeat
      event = WaitWindowEvent(10)
      If event = #PB_Event_CloseWindow
        Break 2 ;exit program
      EndIf
    Until event = 0

    x = WindowMouseX(#MyWindow)
    y = WindowMouseY(#MyWindow)
    SetGadgetText(#MousePos_txt,"(" + Str(x) + "," + Str(y) + ")")
  ForEver
EndIf
```



## Python


Mouse position using Tkinter graphics library nearly universally included in installations of Python.
There are other alternatives but they are platform specific.

Shows position of mouse while it is over the program windows and
changes color of window when mouse is near (<10) hot spot 100,100.

Code is based on post in Daniweb: http://www.daniweb.com/forums/post616327.html#post616327 by ZZucker

```python

import Tkinter as tk

def showxy(event):
    xm, ym = event.x, event.y
    str1 = "mouse at x=%d  y=%d" % (xm, ym)
    # show cordinates in title
    root.title(str1)
    # switch color to red if mouse enters a set location range
    x,y, delta = 100, 100, 10
    frame.config(bg='red'
                 if abs(xm - x) < delta and abs(ym - y) < delta
                 else 'yellow')

root = tk.Tk()
frame = tk.Frame(root, bg= 'yellow', width=300, height=200)
frame.bind("<Motion>", showxy)
frame.pack()

root.mainloop()

```

----------------

```python

#simple way of ,get cursor xy data

from Tkinter import *
win=Tk()
win.geometry("200x300")
def xy(event):
    xm, ym = event.x, event.y
    xy_data = "x=%d  y=%d" % (xm, ym)
    lab=Label(win,text=xy_data)
    lab.grid(row=0,column=0)

win.bind("<Motion>",xy)
mainloop()

```



## Scala


```scala
import java.awt.MouseInfo
object MousePosition extends App {
  val mouseLocation = MouseInfo.getPointerInfo.getLocation
  println (mouseLocation)
}
```



## Racket


The mouse position can be queried at any time with a function in a GUI context.


```racket

#lang racket/gui
(define-values [point _] (get-current-mouse-state))
(send point get-x)
(send point get-y)

```



## Retro

This requires running Retro on a VM with support for the ''canvas'' device.


```Retro
needs canvas'
^canvas'mouse
```



## Ring


In the next example the user can move a label using the mouse
The movement procedure uses the mouse position information


```Ring

Load "guilib.ring"

lPress = false
nX = 0
nY = 0

new qApp {

        win1 = new qWidget()
        {

                setWindowTitle("Move this label!")
                setGeometry(100,100,400,400)
                setstylesheet("background-color:white;")

                Label1 = new qLabel(Win1){
                        setGeometry(100,100,200,50)
                        setText("Welcome")
                        setstylesheet("font-size: 30pt")
                        myfilter = new qallevents(label1)
                        myfilter.setEnterevent("pEnter()")
                        myfilter.setLeaveevent("pLeave()")
                        myfilter.setMouseButtonPressEvent("pPress()")
                        myfilter.setMouseButtonReleaseEvent("pRelease()")                                                               myfilter.setMouseMoveEvent("pMove()")
                        installeventfilter(myfilter)
                }

                show()
        }

        exec()
}

Func pEnter
        Label1.setStyleSheet("background-color: purple; color:white;font-size: 30pt;")

Func pLeave
        Label1.setStyleSheet("background-color: white; color:black;font-size: 30pt;")

Func pPress
        lPress = True
        nX = myfilter.getglobalx()
        ny = myfilter.getglobaly()

Func pRelease
        lPress = False
        pEnter()

Func pMove
        nX2 = myfilter.getglobalx()
        ny2 = myfilter.getglobaly()
        ndiffx = nX2 - nX
        ndiffy = nY2 - nY
        if lPress
                Label1 {
                        move(x()+ndiffx,y()+ndiffy)
                        setStyleSheet("background-color: Green;
                                 color:white;font-size: 30pt;")
                        nX = nX2
                        ny = nY2
                }
        ok

```


## Ruby


```Ruby
Shoes.app(:title => "Mouse Position", :width => 400, :height => 400) do
  @position = para "Position : ?, ?", :size => 12, :margin => 10

  motion do |x, y|
    @position.text = "Position : #{x}, #{y}"
  end
end
```



## Rust

Prints current location of the mouse cursor relative to any active window.
This example relies on the Windows API.

```rust
// rustc 0.9 (7613b15 2014-01-08 18:04:43 -0800)

use std::libc::{BOOL, HANDLE, LONG};
use std::ptr::mut_null;

type HWND = HANDLE;

#[deriving(Eq)]
struct POINT {
    x: LONG,
    y: LONG
}

#[link_name = "user32"]
extern "system" {
    fn GetCursorPos(lpPoint:&mut POINT) -> BOOL;
    fn GetForegroundWindow() -> HWND;
    fn ScreenToClient(hWnd:HWND, lpPoint:&mut POINT);
}

fn main() {
    let mut pt = POINT{x:0, y:0};
    loop {
        std::io::timer::sleep(100); // sleep duration in milliseconds

        let pt_prev = pt;
        unsafe { GetCursorPos(&mut pt) };
        if pt != pt_prev {
            let h = unsafe { GetForegroundWindow() };
            if h == mut_null() { continue }

            let mut pt_client = pt;
            unsafe { ScreenToClient(h, &mut pt_client) };
            println!("x: {}, y: {}", pt_client.x, pt_client.y);
        }
    }
}
```

```txt
x: 550, y: 614
x: 650, y: 248
x: 612, y: 0
x: 620, y: 0
x: 405, y: 0
x: 0, y: 0
^C
```



## Seed7

The functions [http://seed7.sourceforge.net/libraries/graph.htm#pointerXPos%28in_PRIMITIVE_WINDOW%29 pointerXPos]
and [http://seed7.sourceforge.net/libraries/graph.htm#pointerYPos%28in_PRIMITIVE_WINDOW%29 pointerYPos] from the
[http://seed7.sourceforge.net/libraries/graph.htm graph.s7i] library determine the actual X and Y position of the mouse pointer, relative to the given window:

```seed7
xPos := pointerXPos(curr_win);
yPos := pointerYPos(curr_win);
```



## Smalltalk

Sending the message <tt>position</tt> to the <tt>activeHand</tt> of the <tt>World</tt> returns a <tt>Point</tt> object:


```smalltalk

World activeHand position. " (394@649.0)"

```



## Tcl

```tcl
package require Tk 8.5
set curWindow [lindex [wm stackorder .] end]
# Everything below will work with anything from Tk 8.0 onwards
set x [expr {[winfo pointerx .] - [winfo rootx $curWindow]}]
set y [expr {[winfo pointery .] - [winfo rooty $curWindow]}]
tk_messageBox -message "the mouse is at ($x,$y) in window $curWindow"
```



## Visual Basic

There are two methods for determining where the mouse pointer is.
The first only works when the pointer is actually over the window containing the code.
This actually works for any control that has a MouseMove event,
but it doesn't work if the pointer is over ''anything else'',
including controls on the form
(so if the pointer is over a button on the current form,
the event will only fire for the button, ''not'' the form).

```vb
Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    'X and Y are in "twips" -- 15 twips per pixel
    Me.Print "X:" & X
    Me.Print "Y:" & Y
End Sub
```


The second method uses the [[wp:Windows API|Windows API]],
and can be easily translated to any language that can make API calls.
This example uses a <code>Timer</code> control to check the mouse position.

```vb
Private Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long

Private Type POINTAPI
    'X and Y are in pixels, with (0,0) being the top left corner of the primary display
    X As Long
    Y As Long
End Type

Private Pt As POINTAPI

Private Sub Timer1_Timer()
    GetCursorPos Pt
    Me.Cls
    Me.Print "X:" & Pt.X
    Me.Print "Y:" & Pt.Y
End Sub
```



## XPL0

GetMousePosition(0) = X coordinate; 1 = Y coordinate.
For video modes $0..$E and $13 the maximum coordinates are 639x199,
minus the size of the pointer.
For modes $F..$12 the coordinates are the same as the pixels.
VESA graphic modes are (usually) 639x479 regardless of the resolution.
For 80-column text modes divide the mouse coordinates by 8 to get the
character cursor position.

The mouse cursor location is always relative to the upper-left corner of the screen.
A window may be externally created ... using a fair amount of
code because the version of XPL0 used with these Rosetta Code examples
does not make Windows applications (however see "Simple windowed application").


```XPL0
include c:\cxpl\stdlib;
if not OpenMouse then Text(0, "A mouse is required")
else [ShowMouse(true);
      IntOut(0, GetMousePosition(0));  ChOut(0, ^,);
      IntOut(0, GetMousePosition(1));  CrLf(0);
     ]
```


```txt

320,96

```
