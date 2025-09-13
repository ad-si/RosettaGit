+++
title = "Simulate input/Mouse"
description = ""
date = 2019-02-13T00:16:39Z
aliases = []
[extra]
id = 4257
[taxonomies]
categories = ["task", "GUI"]
tags = []
+++

## Task

{{task|GUI}}[[Category:Testing]]Simulate the click of a mouse button by the user. Specify if the target GUI may be externally created.

## AutoHotkey

target gui may be externally created.

```AutoHotkey
WinActivate, ahk_class MozillaUIWindowClass
Click 200, 200 right  ; relative to external window (firefox)
sleep, 2000
WinMinimize
CoordMode, Mouse, Screen
Click 400, 400 right  ; relative to top left corner of the screen.
```



## C


### Windows

Animates the movement of the mouse pointer from the screen center to the bottom left corner where the Windows button is usually present on most Windows desktops. Once there, a left click is simulated. The exact speed, motion and behaviour of the pointer will vary from desktop to desktop. Compatible with MinGW or GCC for Windows.

```C

#define WINVER 0x500
#include<windows.h>

int main()
{
	int maxX = GetSystemMetrics(SM_CXSCREEN), maxY = GetSystemMetrics(SM_CYSCREEN);
	int x = maxX/2, y = maxY/2;
	double factorX = 65536.0 / maxX,factorY = 65536.0 / maxY;

	INPUT ip;

	ZeroMemory(&ip,sizeof(ip));

	ip.type = INPUT_MOUSE;

	while(x > 5 || y < maxY-5){

	ip.mi.mouseData = 0;
	ip.mi.dx = x * factorX;
	ip.mi.dy = y * factorY;
	ip.mi.dwFlags = MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_MOVE;

	SendInput(1,&ip,sizeof(ip));
	sleep(1);
	if(x>3)
		x-=1;
	if(y<maxY-3)
		y+=1;
	}

	ip.mi.dwFlags = MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_LEFTDOWN | MOUSEEVENTF_LEFTUP;

	SendInput(1,&ip,sizeof(ip));

	return 0;
}

```



## Common Lisp

The xdotool have to be installed on the machine (installable through apt-get). Tested on Lubuntu 14.04.

```lisp

(defun sh (cmd)
#+clisp (shell cmd)
#+ecl (si:system cmd)
#+sbcl (sb-ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*)
#+clozure (ccl:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*))
(sh "xdotool mousemove 0 0 click 1")
(sleep 2)
(sh "xdotool mousemove 300 300 click 1")

```



## Fantom


You can simulate a mouse click on a button by asking that button to fire its event listeners.  This approach only works for the program's own GUI:


```fantom

using fwt
using gfx

class Main
{
  public static Void main ()
  {
    button1 := Button
    {
      text = "don't click!"
      onAction.add |Event e|
      {
        echo ("clicked by code")
      }
    }
    button2 := Button
    {
      text = "click"
      onAction.add |Event e|
      {
        // fire all the event listeners on button1
        button1.onAction.fire(e)
      }
    }
    Window
    {
      title = "simulate mouse event"
      size = Size (300, 200)
      button1,
      button2,
    }.open
  }
}

```


Alternatively, if you are running on the Java Runtime, you can use Java's 'robot' library to click anywhere on the screen, and so interact with widgets from other programs:


```fantom

using [java] java.awt::Robot
using [java] java.awt.event::InputEvent
using fwt
using gfx

class Main
{
  public static Void main ()
  {
    button := Button
    {
      text = "click for robot"
      onAction.add |Event e|
      {
        robot := Robot ()
        robot.mouseMove (50, 50) // move to screen point 50, 50
        robot.mousePress (InputEvent.BUTTON1_MASK) // and click mouse
        robot.mouseRelease (InputEvent.BUTTON1_MASK)
      }
    }
    Window
    {
      title = "simulate mouse event"
      size = Size (300, 200)
      button,
    }.open
  }
}

```



## Go

The target GUI may be externally created.

```go
package main

import "github.com/go-vgo/robotgo"

func main() {
    robotgo.MouseClick("left", false) // single clicks left mouse button
    robotgo.MouseClick("right", true) // double clicks right mouse button
}
```



## GUISS



```guiss
Start,Programs,Accessories,Notepad,Textbox,Type:Hello World[pling],Menu:File,Save,
Inputbox:filename>greetings.txt,Button:Save
```



## Java

You can click on any Component using a Robot and the Component's location:

```java
Point p = component.getLocation();
Robot robot = new Robot();
robot.mouseMove(p.getX(), p.getY()); //you may want to move a few pixels closer to the center by adding to these values
robot.mousePress(InputEvent.BUTTON1_MASK); //BUTTON1_MASK is the left button,
                                       //BUTTON2_MASK is the middle button, BUTTON3_MASK is the right button
robot.mouseRelease(InputEvent.BUTTON1_MASK);
```

If you don't have a reference to the component, you'll need to guess at where it is.

If you have a reference to the AbstractButton this is simpler:

```java
button.doClick(); //optionally, give an integer argument for the number of milliseconds to hold the button down
```




## Julia

This may be done using Julia's C call FFI:


```julia

# Wrap win32 API function mouse_event() from the User32 dll.
function mouse_event_wrapper(dwFlags,dx,dy,dwData,dwExtraInfo)
    ccall((:mouse_event, "User32"),stdcall,Void,(UInt32,UInt32,UInt32,UInt32,UInt),dwFlags,dx,dy,dwData,dwExtraInfo)
end

function click()
    mouse_event_wrapper(0x2,0,0,0,0)
    mouse_event_wrapper(0x4,0,0,0,0)
end

```



## Kotlin


```scala
// version 1.1.2

import java.awt.Robot
import java.awt.event.InputEvent

fun sendClick(buttons: Int) {
    val r = Robot()
    r.mousePress(buttons)
    r.mouseRelease(buttons)
}

fun main(args: Array<String>) {
    sendClick(InputEvent.BUTTON3_DOWN_MASK) // simulate a click of the mouse's right button
}

```




## Oz

Using Tk events, this only works with internal windows.

```oz
declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}
  Button
  Window = {QTk.build td(button(text:"Click me" handle:Button))}
in
  {Window show}
  {Delay 500}
  {Tk.send event(generate Button "<ButtonPress-1>")}
  {Delay 500}
  {Tk.send event(generate Button "<ButtonRelease-1>")}
```



## Perl 6

Using bindings to libxdo so any window managed by an X11 display server can receive mouse events.


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

#`[ There are several available routines controlling mouse position and button events.

.move-mouse( $x, $y, $screen ) # Move the mouse to a specific location.

.move-mouse-relative( $delta-x, $delta-y ) # Move the mouse relative to it's current position.

.move-mouse-relative-to-window( $x, $y, $window ) # Move the mouse to a specific location relative to the top-left corner of a window.

.get-mouse-location() # Get the current mouse location (coordinates and screen ID number).

.get-mouse-info() # Get all mouse location-related data.

.wait-for-mouse-to-move-from( $origin-x, $origin-y ) # Wait for the mouse to move from a location.

.wait-for-mouse-to-move-to( $dest-x, $dest-y ) # Wait for the mouse to move to a location.

.mouse-button-down( $window, $button ) # Send a mouse press (aka mouse down) for a given button at the current mouse location.

.mouse-button-up( $window, $button ) # Send a mouse release (aka mouse up) for a given button at the current mouse location.

.mouse-button-click( $window, $button ) # Send a click for a specific mouse button at the current mouse location.

.mouse-button-multiple( $window, $button, $repeat = 2, $delay? ) # Send a one or more clicks of a specific mouse button at the current mouse location.
]

```



## PicoLisp

PicoLisp comes with a dedicated browser GUI. A library based on web scraping (in
"lib/scrape.l") can be used to drive that GUI under program control. It allows
to read GUI pages, click on HTML links, enter text into forms, and press submit
buttons. In that way one application can control another application.

The documented [http://software-lab.de/doc/app.html#minApp demo application],
which is also available online at [http://7fach.de/8080 app.7fach.de], is used
in the following example. Mouse input is simulated with the functions 'click'
(click on a HTML link) and 'press' (press a submit button).

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

Output:

```txt
12.50
```

The same example is used in the related task [[Simulate input/Keyboard#PicoLisp]].


## PureBasic

This code is Windows only.

```PureBasic
Macro Click()
  mouse_event_(#MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0)
  mouse_event_(#MOUSEEVENTF_LEFTUP, 0, 0, 0, 0)
EndMacro

; Click at the current location
Click()

Delay(1000) ; Wait a second

; Move to a new location and click it
SetCursorPos_(50, 50)
Click()
```


```PureBasic
; The same function as above, but using AutoWin UserLibray
AW_MouseClick()
Delay(1000)
AW_MouseClick(#PB_MouseButton_Left, 50, 50)
```



## Python

In Windows (GUI can be externally created):

```Python
import ctypes

def click():
    ctypes.windll.user32.mouse_event(0x2, 0,0,0,0)    # Mouse LClick Down, relative coords, dx=0, dy=0
    ctypes.windll.user32.mouse_event(0x4, 0,0,0,0)    # Mouse LClick Up, relative coords, dx=0, dy=0

click()
```



```Python
import autopy
import math
import time
import random

TWO_PI = math.pi * 2.0


def sine_mouse_wave():
    """
    Moves the mouse in a sine wave from the left edge of the screen to
    the right.
    """
    width, height = autopy.screen.get_size()
    height /= 2
    height -= 10  # Stay in the screen bounds.

    for x in xrange(width):
        y = int(height * math.sin((TWO_PI * x) / width) + height)
        autopy.mouse.move(x, y)
        time.sleep(random.uniform(0.001, 0.003))

sine_mouse_wave()
```


```Python
import pyautogui

pyautogui.moveTo(100, 200)      # moves mouse to X of 100, Y of 200.
pyautogui.moveTo(None, 500)     # moves mouse to X of 100, Y of 500.
pyautogui.moveTo(600, None)     # moves mouse to X of 600, Y of 500.
pyautogui.moveTo(100, 200, 2)   # moves mouse to X of 100, Y of 200 over 2 seconds

pyautogui.moveRel(0, 50)        # move the mouse down 50 pixels.
pyautogui.moveRel(-30, 0)       # move the mouse left 30 pixels.

pyautogui.click()                          # Left button click on current position
pyautogui.click(clicks=2)
pyautogui.click(clicks=2, interval=0.25)   # with a quarter second pause in between clicks

pyautogui.click(10, 5)                     # Mouse left button click, x=10, y=5
pyautogui.click(200, 250, button='right')  # Mouse right button click, x=200, y=250

pyautogui.scroll(10)   # scroll up 10 "clicks"
pyautogui.scroll(10, x=100, y=100)  # move mouse cursor to 100, 200, then scroll up 10 "clicks"




```



## Racket

Same as the Python entry: use a User32 function to simulate a mouse click.


```Racket

#lang at-exp racket

(require ffi/unsafe)

(define mouse-event
  (get-ffi-obj "mouse_event" (ffi-lib "user32")
               (_fun _int32 _int32 _int32 _int32 _pointer -> _void)))

(mouse-event #x2 0 0 0 #f)
(mouse-event #x4 0 0 0 #f)

```



## Ring


```ring

# Project : Simulate input/Mouse

load "guilib.ring"
load "stdlib.ring"

paint = null

new qapp
        {
        win1 = new qwidget() {
                  setwindowtitle("")
                  setgeometry(100,100,800,600)
                  setwindowtitle("Mouse events")

                  line1 = new qlineedit(win1) {
                              setgeometry(150,450,300,30)
                              settext("")}

                  line2 = new qlineedit(win1) {
                              setgeometry(150,400,300,30)
                              settext("")}

                  new qpushbutton(win1) {
                          setgeometry(150,500,300,30)
                          settext("draw")
                          myfilter = new qallevents(win1)
                          myfilter.setMouseButtonPressevent("drawpress()")
                          myfilter.setMouseButtonReleaseevent("drawrelease()")
                          installeventfilter(myfilter)
                  }
                  show()
        }
        exec()
        }

func drawpress()
        line2.settext("")
        line1.settext("Mouse was pressed")

func drawrelease()
        line1.settext("")
        line2.settext("Mouse was released")

```

Output:

https://www.dropbox.com/s/kvm9s8qesaufyej/MouseEvents.jpg?dl=0


## Rust

```Rust
extern crate autopilot;
extern crate rand;
use rand::Rng;

// Moves the mouse in a sine wave across the screen.
const TWO_PI: f64 = std::f64::consts::PI * 2.0;
fn sine_mouse_wave() -> Result<(), autopilot::mouse::MouseError> {
    let screen_size = autopilot::screen::size();
    let scoped_height = screen_size.height / 2.0 - 10.0; // Stay in screen bounds.
    for x in 0..screen_size.width as u64 {
        let y = (scoped_height * ((TWO_PI * x as f64) / screen_size.width).sin() + scoped_height)
            .round();
        let duration: u64 = rand::thread_rng().gen_range(1, 3);
        try!(autopilot::mouse::move_to(autopilot::geometry::Point::new(
            x as f64,
            y as f64
        )));
        std::thread::sleep(std::time::Duration::from_millis(duration));
    }
    Ok(())
}

fn main() {
    sine_mouse_wave().expect("Unable to move mouse");
}
```



## Scala

```Scala
  val (p , robot)= (component.location, new Robot())
  robot.mouseMove(p.getX().toInt, p.getY().toInt) //you may want to move a few pixels closer to the center by adding to these values
  robot.mousePress(InputEvent.BUTTON1_MASK) //BUTTON1_MASK is the left button
  robot.mouseRelease(InputEvent.BUTTON1_MASK)
```



## Tcl


### Within an Application

```tcl
# Simulate a full click cycle: button down and up
event generate .okBtn <ButtonPress-1> -x 5 -y 5
event generate .okBtn <ButtonRelease-1> -x 5 -y 5
```

Note that many of Tk's windows also need appropriate <Enter> and <Leave> events in order to work correctly. For the process of actually simulating a click on a button, it is actually easier to work at the method-call level rather than the event generation level:

```tcl>.okBtn invoke</lang


{{omit from|Befunge}} <!-- No mouse support -->
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have a pointing device. -->
