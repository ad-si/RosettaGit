+++
title = "GUI/Maximum window dimensions"
description = ""
date = 2019-08-07T12:44:11Z
aliases = []
[extra]
id = 8843
[taxonomies]
categories = ["task", "GUI"]
tags = []
+++

## Task

The task is to determine the maximum height and width of a window that can fit within the physical display area of the screen without scrolling.

This is effectively the screen size (not the total desktop area, which could be bigger than the screen display area) in pixels minus any adjustments for window decorations and menubars.

The idea is to determine the physical display parameters for the maximum height and width of the usable display area in pixels (without scrolling).

The values calculated should represent the usable desktop area of a window maximized to fit the the screen.


;Considerations:

;--- Multiple Monitors:
For multiple monitors, the values calculated should represent the size of the usable display area on the monitor which is related to the task (i.e.:   the monitor which would display a window if such instructions were given).

;--- Tiling Window Managers
For a tiling window manager, the values calculated should represent the maximum height and width of the display area of the maximum size a window can be created (without scrolling). This would typically be a full screen window (minus any areas occupied by desktop bars), unless the window manager has restrictions that prevents the creation of a full screen window, in which case the values represent the usable area of the desktop that occupies the maximum permissible window size (without scrolling).





## Ada

```Ada
with Gtk.Main;
with Glib;
with Gtk.Window;  use Gtk.Window;
with Gtk.Enums;   use Gtk.Enums;
with Ada.Text_IO; use Ada.Text_IO;

procedure Max_Size is

   Win          : Gtk_Window;
   Win_W, Win_H : Glib.Gint;
   package Int_Io is new Integer_IO (Glib.Gint);
   Hid : Gtk.Main.Quit_Handler_Id;

begin
   Gtk.Main.Init;
   Gtk_New (Win);
   Initialize (Win, Window_Toplevel);
   Maximize (Win);
   Show (Win);
   Get_Size (Win, Win_W, Win_H);
   Put ("Maximum dimensions of window : W ");
   Int_Io.Put (Win_W, Width => 4);
   Put (" x H ");
   Int_Io.Put (Win_H, Width => 4);
   New_Line;
   Hid := Gtk.Main.Quit_Add_Destroy (0, Win);
end Max_Size;
```

Output (on a 1280 x 800 screen with Windows XP):

```txt
Maximum dimensions of window : W 1280 x H  734

```



## AutoHotkey

This is a modified example taken from the AutoHotkey documentation for the [http://ahkscript.org/docs/commands/SysGet.htm SysGet] command.
Also, the built in variables [http://ahkscript.org/docs/Variables.htm#Screen A_ScreenHeight] and [http://ahkscript.org/docs/Variables.htm#Screen A_ScreenWidth] contain the width and height of the primary monitor, in pixels.

```AutoHotkey
SysGet, MonitorCount, MonitorCount
SysGet, MonitorPrimary, MonitorPrimary
MsgBox, Monitor Count:`t%MonitorCount%`nPrimary Monitor:`t%MonitorPrimary%
Loop, %MonitorCount%
{
    SysGet, MonitorName, MonitorName, %A_Index%
    SysGet, Monitor, Monitor, %A_Index%
    SysGet, MonitorWorkArea, MonitorWorkArea, %A_Index%
    MsgBox, % "Monitor:`t#" A_Index
            . "`nName:`t" MonitorName
            . "`nLeft:`t" MonitorLeft "(" MonitorWorkAreaLeft " work)"
            . "`nTop:`t" MonitorTop " (" MonitorWorkAreaTop " work)"
            . "`nRight:`t" MonitorRight " (" MonitorWorkAreaRight " work)"
            . "`nBottom:`t" MonitorBottom " (" MonitorWorkAreaBottom " work)"
}
```

'''Output:'''

```txt
Monitor Count:    1
Primary Monitor:  1

Monitor:    #1
Name:       \\.\DISPLAY1
Left:       0(0 work)
Top:        0 (0 work)
Right:      1920 (1920 work)
Bottom:     1080 (1040 work)
```



## Axe


Because Axe is currently (6/22/2015) only available on the TI-83/84 black and white calculators, the screen dimensions are fixed at 96 by 64 pixels.


## BBC BASIC

```bbcbasic
      SPI_GETWORKAREA = 48
      DIM rc{l%,t%,r%,b%}
      SYS "SystemParametersInfo", SPI_GETWORKAREA, 0, rc{}, 0
      PRINT "Maximum width = " ; rc.r% - rc.l%
      PRINT "Maximum height = " ; rc.b% - rc.t%
```

'''Output:'''

```txt

Maximum width = 1367
Maximum height = 1021

```



## C


### Windows

The following implementation has been tested on Windows 8.1, may not work on Linux systems.

```C

#include<windows.h>
#include<stdio.h>

int main()
{
	printf("Dimensions of the screen are (w x h) : %d x %d pixels",GetSystemMetrics(SM_CXSCREEN),GetSystemMetrics(SM_CYSCREEN));
	return 0;
}

```

Output :

```txt

Dimensions of the screen are (w x h) : 1536 x 864 pixels

```



## C#

'''Compiler:''' Roslyn C# (language version >= 6)
{{works with|.NET Framework|4.7.2}} (simple enough that it should probably work on every Framework version--.NET Core 3.0 will support Windows Forms on Windows only)
Must be referenced:
<!--Wrap in a span with display:flex to remove libheader's line break-->
<span style="display:flex">{{libheader|GDI+}}<span style="white-space:pre"> (managed interface) [System.Drawing]</span></span>
<span style="display:flex">{{libheader|Windows Forms}}<span style="white-space:pre"> [System.Windows.Forms]</span></span>

Bounds are the screen's dimensions; working area is the is the region that excludes "taskbars, docked windows, and docked tool bars" (from Framework documentation).


```c#
using System;
using System.Drawing;
using System.Windows.Forms;

static class Program
{
    static void Main()
    {
        Rectangle bounds = Screen.PrimaryScreen.Bounds;
        Console.WriteLine($"Primary screen bounds:  {bounds.Width}x{bounds.Height}");

        Rectangle workingArea = Screen.PrimaryScreen.WorkingArea;
        Console.WriteLine($"Primary screen working area:  {workingArea.Width}x{workingArea.Height}");
    }
}
```


```txt
Primary screen bounds:  1714x1143
Primary screen working area:  1714x1103
```


Alternatively, use the dimensions of a borderless form with WindowState set to FormWindowState.Maximized (i.e. a full-screen window that is shown above the taskbar).


```c#
using System;
using System.Drawing;
using System.Windows.Forms;

static class Program
{
    static void Main()
    {
        using (var f = new Form() { FormBorderStyle = FormBorderStyle.None, WindowState = FormWindowState.Maximized })
        {
            f.Show();
            Console.WriteLine($"Size of maximized borderless form:  {f.Width}x{f.Height}");
        }
    }
}
```


```txt
Size of maximized borderless form:  1714x1143
```



## Creative Basic


```Creative Basic

DEF Win:WINDOW
DEF Close:CHAR

DEF ScreenSizeX,ScreenSizeY:INT
DEF L,T,ClientWidth,ClientHeight:INT

GETSCREENSIZE(ScreenSizeX,ScreenSizeY)

WINDOW Win,0,0,ScreenSizeX,ScreenSizeY,@MINBOX|@MAXBOX|@SIZE|@MAXIMIZED,0,"Get Client Size",MainHandler

'Left and top are always zero for this function.
GETCLIENTSIZE(Win,L,T,ClientWidth,ClientHeight)

PRINT Win,"Maximum drawing area values: width is"+STR$(ClientWidth)+" and height is"+STR$(ClientHeight)+"."

WAITUNTIL Close=1

CLOSEWINDOW Win

END

SUB MainHandler

	SELECT @CLASS

	CASE @IDCLOSEWINDOW

	Close=1

	ENDSELECT

RETURN

Output: Maximum drawing area values: width is 1280 and height is 749.

```



## EGL

To get the size of the window in a RuiHandler a JavaScript function is needed that is not natively supported by EGL. Therefore an external type is created to wrap the JavaScript function.

File 'Browser.js' in folder 'utils' in the WebContent folder of a rich UI project.

```txt

egl.defineClass(
	'utils', 'Browser',
{
	"getViewportWidth" : function () {
		return window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth;
	},
	"getViewportHeight" : function(){
		return window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight;
	}
});

```

The external type to wrap the JavaScript functions.

```txt

ExternalType Browser type JavaScriptObject{
	relativePath = "utils",
	javaScriptName = "Browser"
}

	function getViewportWidth() returns (int);

	function getViewportHeight() returns (int);

end

```

Usage of the Browser external type in a RuiHandler.

```EGL

browser Browser{};
bvh int = browser.getViewportHeight();
bvw int = browser.getViewportWidth();
SysLib.writeStdout("ViewportHeight: " + bvh);
SysLib.writeStdout("ViewportWidth: " + bvw);

```

Output

```txt

ViewportHeight: 860
ViewportWidth: 1680

```



## FBSL

In the graphics mode, Windows does it all automatically and displays a form that fills the entire area not obscured by the taskbar on your primary monitor:

```qbasic>#INCLUDE <Include\Windows.inc

ShowWindow(ME, SW_MAXIMIZE)
BEGIN EVENTS
END EVENTS
```


Alternatively, one can obtain the unobscured area's dimensions using the following console script:

```qbasic
#APPTYPE CONSOLE
#INCLUDE <Include\Windows.inc>

TYPE RECT
	%Left
	%Top
	%Right
	%Bottom
END TYPE

DIM rc AS RECT
SystemParametersInfo(SPI_GETWORKAREA, 0, @rc, 0)
PRINT "width = ", rc.Right - rc.Left, ", height = ", rc.Bottom - rc.Top

PAUSE
```


A typical output for a 1680x1050 primary monitor will be:

```txt
width = 1680, height = 1017

Press any key to continue...
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Using SystemParametersInfo function in Win32 API
Dim As Any Ptr library = DyLibLoad("user32")
Dim Shared SystemParametersInfo As Function (ByVal As ULong, ByVal As ULong, ByVal As Any Ptr, ByVal As ULong) As Long
SystemParametersInfo = DyLibSymbol(library, "SystemParametersInfoA")

Type Rect
  As Long left, top, right, bottom
End Type

#Define SPI_GETWORKAREA &H30
Dim r As Rect
SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0)
DyLibFree(library)
Print "Maximum usable desktop area :  W" ; r.right - r.left; " x H"; r.bottom - r.top; " pixels"
Print
Print "Press any key to quit"
Sleep
```

Output for my machine:
```txt

Maximum usable desktop area :  W 1366 x H 728 pixels

```


== {{header|Gambas}} ==


###  Overview


In gambas, the trick to determining the maximum window size that will fit on the screen is to create a form that is maximized and then query its dimensions from within a Form_Resize() event. Note that the form can be invisible during this process, and typically we would use the main modal window (FMain in this example).


###  Creating the form


From with the project create a form (FMain) with the following properties set:


```gambas
FMain.Maximized = True
FMain.Visible = False    ' The form can be invisible
```


From within the projectview, rightclick the FMain form and select Edit class from the contextmenu. This will display a form class file (FMain.class) as follows:


```gambas
PUBLIC SUB _new()

END

PUBLIC SUB Form_Open()

END
```



###  Adding the form resize event


We can now add a Form_Resize() event to the class file with the necessary code to obtain the screen dimensions as follows:


```gambas
PUBLIC SUB Form_Resize()
  PRINT "The maximum window size that can be used without scrolling is "; FMain.Width; " x "; FMain.Height
END
```



## Gambas


```gambas
Public Sub Form_Open()

Print Desktop.Width
Print Desktop.Height

End
```

Output:

```txt

1920
1055

```



## Go

```go
package main

import (
    "fmt"
    "github.com/go-vgo/robotgo"
)

func main() {
    w, h := robotgo.GetScreenSize()
    fmt.Printf("Screen size: %d x %d\n", w, h)
    fpid, err := robotgo.FindIds("firefox")
    if err == nil && len(fpid) > 0 {
        pid := fpid[0]
        robotgo.ActivePID(pid)
        robotgo.MaxWindow(pid)
        _, _, w, h = robotgo.GetBounds(pid)
        fmt.Printf("Max usable : %d x %d\n", w, h)
    }
}
```


On my machine the figures are:

```txt

Screen size: 1366 x 768
Max usable : 1301 x 744

```



## Groovy


```groovy
def window = java.awt.GraphicsEnvironment.localGraphicsEnvironment.maximumWindowBounds

println "width: $window.width, height: $window.height"
```



## Haskell


```Haskell
import Graphics.UI.Gtk
import Control.Monad (when)
import Control.Monad.Trans (liftIO)

maximumWindowDimensions :: IO ()
maximumWindowDimensions = do
    -- initialize the internal state of the GTK toolkit
    initGUI
    -- create a window
    window <- windowNew
    -- quit the application when the window is closed
    on window objectDestroy mainQuit
    -- query the size of the window when its dimensions change
    on window configureEvent printSize
    -- get the screen the window will be drawn upon
    screen <- windowGetScreen window
    -- get the size of the screen
    x <- screenGetWidth screen
    y <- screenGetHeight screen
    -- print the dimensions of the screen
    putStrLn ("The screen is " ++ show x ++ " pixels wide and " ++
        show y ++ " pixels tall for an undecorated fullscreen window.")
    -- maximize the window and show it. printSize will then be called
    windowMaximize window
    widgetShowAll window
    -- run the main GTK loop.
    -- close the window manually.
    mainGUI

-- On my Xfce4 desktop, the configure_event is called three times when a
-- top level window is maximized. The first time, the window size
-- returned is the size prior to maximizing, and the last two times
-- it is the size after maximizing.
-- If the window is (un)maximized manually, the size returned is always
-- the size of the unmaximized window.
-- That means: either GTK or Xfce4 does not handle window maximization
-- correctly, or the GTK bindings for Haskell are buggy, or there is an
-- error in this program.

printSize :: EventM EConfigure Bool
printSize = do
    -- get the window that has been resized
    w <- eventWindow
    -- is the window maximized?
    s <- liftIO $ drawWindowGetState w
    when (WindowStateMaximized `elem` s) $ do
        -- get the size of the window that has been resized
        (x, y) <- eventSize
        -- print the dimensions out
        liftIO $ putStrLn ("The inner window region is now " ++ show x ++
            " pixels wide and " ++ show y ++ " pixels tall.")
    return True
```

=={{header|Icon}} and {{header|Unicon}}==
Raise and query a hidden window.

```Icon
link graphics

procedure main()  # Window size

W  := WOpen("canvas=hidden")
dh := WAttrib("displayheight")
dw := WAttrib("displaywidth")
WClose(W)

write("The display size is w=",dw,", h=",dh)
end
```


## IWBASIC


```IWBASIC

DEF Win:WINDOW
DEF Close:CHAR

DEF ScreenSizeX,ScreenSizeY:UINT
DEF L,T,ClientWidth,ClientHeight:INT

GETSCREENSIZE(ScreenSizeX,ScreenSizeY)

OPENWINDOW Win,0,0,ScreenSizeX,ScreenSizeY,@MAXBOX|@MINBOX|@SIZE|@MAXIMIZED,NULL,"Get client area",&MainHandler

'Left and top are always zero for this function.
GETCLIENTSIZE (Win,L,T,ClientWidth,ClientHeight)

PRINT Win,"Maximum drawing area values: width is"+STR$(ClientWidth)+" and height is"+STR$(ClientHeight)+"."

WAITUNTIL Close=1

CLOSEWINDOW WIN

END

SUB MainHandler

	SELECT @MESSAGE

	CASE @IDCLOSEWINDOW

	Close=1

	ENDSELECT

RETURN
ENDSUB

Output: Maximum drawing area values: width is 1280 and height is 749.

```



## Java


```java
import java.awt.*;
import javax.swing.JFrame;

public class Test extends JFrame {

    public static void main(String[] args) {
        new Test();
    }

    Test() {
        Toolkit toolkit = Toolkit.getDefaultToolkit();

        Dimension screenSize = toolkit.getScreenSize();
        System.out.println("Physical screen size: " + screenSize);

        Insets insets = toolkit.getScreenInsets(getGraphicsConfiguration());
        System.out.println("Insets: " + insets);

        screenSize.width -= (insets.left + insets.right);
        screenSize.height -= (insets.top + insets.bottom);
        System.out.println("Max available: " + screenSize);
    }
}
```


Output:


```txt
Physical screen size: java.awt.Dimension[width=1920,height=1080]
Insets: java.awt.Insets[top=0,left=0,bottom=30,right=0]
Max available: java.awt.Dimension[width=1920,height=1050]
```



## Julia

Uses the Gtk library.

```julia

win = GtkWindow("hello", 100, 100)
fullscreen(win)
sleep(10)
println(width(win), " ", height(win))
destroy(win)

```

```txt

1920 1080

```



## Kotlin

```scala
// version 1.1

import java.awt.Toolkit
import javax.swing.JFrame

class Test : JFrame() {
    init {
        val r = Regex("""\[.*\]""")
        val toolkit = Toolkit.getDefaultToolkit()
        val screenSize = toolkit.screenSize
        println("Physical screen size : ${formatOutput(screenSize, r)}")
        val insets = toolkit.getScreenInsets(graphicsConfiguration)
        println("Insets               : ${formatOutput(insets, r)}")
        screenSize.width  -= (insets.left + insets.right)
        screenSize.height -= (insets.top + insets.bottom)
        println("Max available        : ${formatOutput(screenSize, r)}")
    }

    private fun formatOutput(output: Any, r: Regex) = r.find(output.toString())!!.value.replace(",", ", ")
}

fun main(args: Array<String>) {
    Test()
}
```

Sample output:
```txt

Physical screen size : [width=1366, height=768]
Insets               : [top=0, left=0, bottom=40, right=0]
Max available        : [width=1366, height=728]

```



## Lingo


```lingo
put _system.desktopRectList
-- [rect(0, 0, 1360, 768), rect(1360, 0, 2960, 1024)]
```



## M2000 Interpreter

Move console to all monitors, at full screen (at each monitor). Then open a form and resize it to fill all monitors, the move the form to monitor with bigger area and expand form to fill that monitor. At the end close the window. All actions performed with once running threads, using After milliseconds { }


Unit for screen is twip (not pixel)

We can read twipsX and twipsY  as twips per pixel in X and Y direction


```M2000 Interpreter

Module CheckAllMonitors {
      mode  16 ' font size
      i=-1
      Flush
      Do {
            i++
            Window mode, i
            Print Window=i
            Wait 100
            Form ;   ' expand Background to fill monitor (form without arguments cut that frame)
            if window=i Then {
                  Background {
                        Cls 0, 0
                        data i,  scale.x, scale.y, motion.x, motion.y
                  }
            } else exit
      }  Always
      Dim Scrx(i), ScrY(i), ScrLeft(i), ScrTop(i)
      While Not Empty {
            Read i
            Read Scrx(i), ScrY(i), ScrLeft(i), ScrTop(i)
      }
      \\ check if we have same left top point
      For i=0 to Len(Scrx())-1 {
            Print "Monitor:", i, "left top (";ScrLeft(i);",";ScrTop(i);") size: (";Scrx(i);",";ScrY(i);")"
      }

      A=ScrLeft(0)
      B=ScrTop(0)
      LeftMargin=A
      TopMargin=B
      RightMargin=Scrx(0)+A
      BottomMargin=Scry(0)+B
      MaxArea=Scrx(0)*Scry(0)
      ChooseMonitor=0
      Out=True
      If Len(Scrx())>1 then {
            For i=1 to Len(Scrx())-1 {
                   LeftMargin=Min.Data(A, ScrLeft(i))
                   TopMargin=Min.Data(B, ScrTop(i))
                   RightMargin=Max.Data(RightMargin, Scrx(i)+Scrleft(i))
                   BottomMargin=Max.Data(BottomMargin, Scry(i)+ScrTop(i))
                   Out=Out and (A=ScrLeft(i) and  B=ScrTop(i))
                   if MaxArea<Scrx(i)*Scry(i) then MaxArea=Scrx(i)*Scry(i) : ChooseMonitor=i
            }
      }
      If Len(Scrx())=1 then {
            Print "One Monitor"
      } else  Print If$(Out ->"Clone Monitors", "Multiple Monitors ")
      Print "Left Top Corner:", LeftMargin, TopMargin
      Print "Width, Height", RightMargin-LeftMargin, BottomMargin-TopMargin
      Declare Form1 Form
      \\ After 100ms Form1 expand to all monitors
      After 100  {
            Method Form1,"Move", LeftMargin, TopMargin, RightMargin-LeftMargin, BottomMargin-TopMargin
      }
      \\ After 2000-100ms Form1 move to montior ChooseMonitor,  and has same width and height
      After 2000 {
                  Try {
                        Method Form1,"Move", ScrLeft(ChooseMonitor),ScrTop(ChooseMonitor), Scrx(ChooseMonitor), Scry(ChooseMonitor)
                  }
      }
      \\ after 4000 ms from other threads, form1 close
      After 4000 {
                  Try {
                        Method Form1, "CloseNow"
                  }
      }
      Method Form1, "Show", 1
      Declare Form1 Nothing
      Threads Erase
}
CheckAllMonitors

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Example output on a 1280x1024 system.

```Mathematica
Differences@Transpose@SystemInformation["Devices"][[1, 2, 1, 1, 2]]
->{{1260, 951}}
```



## Nim

==={{libheader|Gdk2}}===
==={{libheader|Gtk2}}===

```nim
import
  gtk2, gdk2

nim_init()
var w = gdk2.screen_width()
var h = gdk2.screen_height()
echo("WxH=",w,"x",h)
```

```txt
WxH=1280x800
```

==={{libheader|IUP}}===

```nim
import
  iup

# assumes you have the iup  .dll or .so installed

discard iup.open(nil,nil)

var scrnFullSize = GetGlobal("FULLSIZE")
var scrnSize = GetGlobal("SCREENSIZE")
var scrnMInfo = GetGlobal("MONITORSINFO")
var scrnVScreen = GetGlobal("VIRTUALSCREEN")

var dlg = Dialog(nil)
SetAttribute(dlg, "SIZE", "FULL")
var scrnXSize = GetAttribute(dlg,"MAXSIZE")

echo scrnFullSize, "\n", scrnSize, "\n", scrnMInfo, "\n", scrnVScreen, "\n", scrnXSize

discard iup.Alarm("Screen client size", scrnFullSize ,"Ok",nil, nil)

#discard iup.mainloop()
iup.close()
```

```txt
1280x800
1280x800
0 0 1280 800

0 0 1280 800
65535x65535
```



## PARI/GP


```parigp
plothsizes()[1..2]
```



## Perl

==={{libheader|Perl/Tk}}===

```Perl

use strict;
use warnings;
use Tk;

sub get_size {
	my $mw = MainWindow->new();
	return ($mw->maxsize);
}

```

get_size returns (1425,870) here.


## Perl 6

This is kind-of a silly task. The maximum window size is going to depend on your OS, hardware, display server and graphics toolkit, not your programming language. Taken at face value, using a Linux system running an X11 display server, the maximum displayable window size is the resolution of your monitor. Basically, the size of your desktop viewport. The Perl 6 module X11::libxdo returns the desktop viewport size for get-desktop-dimensions which is the effective maximum window size.


```perl6
use X11::libxdo;

my $xdo = Xdo.new;

my ($dw, $dh) = $xdo.get-desktop-dimensions( 0 );

say "Desktop viewport dimensions: (maximum-fullscreen size) $dw x $dh";
```

```txt
Desktop viewport dimensions: (maximum-fullscreen size) 1920 x 1080
```



## Phix

```Phix
include pGUI.e

IupOpen()

string scrnFullSize = IupGetGlobal("FULLSIZE")
string scrnSize = IupGetGlobal("SCREENSIZE")
string scrnMInfo = IupGetGlobal("MONITORSINFO")
string scrnVScreen = IupGetGlobal("VIRTUALSCREEN")

Ihandle dlg = IupDialog(NULL,"SIZE=FULL")
string scrnXSize = IupGetAttribute(dlg,"MAXSIZE")

?{scrnFullSize, scrnSize, scrnMInfo, scrnVScreen, scrnXSize}

IupClose()
```

```txt

{"1920x1080","1920x1080","0 0 1920 1080\n","0 0 1920 1080","65535x65535"}

```

You could instead use atom {x,y} = IupGetIntInt(NULL,"FULLSIZE"|"SCREENSIZE"|"MAXSIZE") to get numbers instead of strings.


## PicoLisp

The following works on ErsatzLisp, the Java version of PicoLisp.

```PicoLisp
(let Frame (java "javax.swing.JFrame" T "Window")
   (java Frame 'setExtendedState
      (java (public "javax.swing.JFrame" 'MAXIMIZED_BOTH)) )
   (java Frame 'setVisible T)
   (wait 200)
   (let Size (java (java Frame 'getContentPane) 'getSize)
      (prinl "Width: " (java (public Size 'width)))
      (prinl "Height: " (java (public Size 'height))) )
   (java Frame 'dispose) )
```

Output (on a 1024x768 screen):

```txt
Width: 1010
Height: 735
```



## PureBasic


```PureBasic
If OpenWindow(0, 0, 0, 5, 5, "", #PB_Window_Maximize + #PB_Window_Invisible)
  maxX = WindowWidth(0)
  maxY = WindowHeight(0)
  CloseWindow(0)
  MessageRequester("Result", "Maximum Window Width: " + Str(maxX) + ", Maximum Window Height: " + Str(maxY))
EndIf
```

Sample output for a screen area 1600 x 1200:

```txt
Maximum Window Width: 1600, Maximum Window Height: 1181
```



## Python


```Python

#!/usr/bin/env python3

import tkinter as tk # import the module.

root = tk.Tk() # Create an instance of the class.
root.state('zoomed') # Maximized the window.
root.update_idletasks() # Update the display.
tk.Label(root, text=(str(root.winfo_width())+ " x " +str(root.winfo_height())),
         font=("Helvetica", 25)).pack() # add a label and set the size to text.
root.mainloop()

```

Sample output for 1366 x 768 screen:

```txt
1366 x 706
```



## Racket


```racket

#lang racket/gui
(define-values [W H]
  (let ([f (new frame% [label "test"])])
    (begin0 (send* f (maximize #t) (show #t) (get-client-size))
      (send f show #f))))
(printf "~ax~a\n" W H)

```



## Ring


```ring

load "guilib.ring"
new qApp {
         win1 = new qWidget() {
                new qPushButton(win1) {
                    resize(200,200)
                    settext("Info")
                    setclickevent(' win1{ setwindowtitle("Width: " +  width() + " Height : " +  height() ) }')
                }
                showMaximized()}
                exec()
                }

```


Output:

[[File:CalmoSoftWindowSize.jpg]]


## Run BASIC

Run Basic uses javaScript to return the width of the browser window.
IE browser uses different functions than everyone else.
So you write code for the world, and also for IE

```runbasic

html "<INPUT TYPE='HIDDEN' id='winHigh' name='winHigh' VALUE='";winHigh;"'></input>"
html "<INPUT TYPE='HIDDEN' id='winWide' name='winWide' VALUE='";winWide;"'></input>"

html "<script>
<!--

function winSize()
{
var myWide	= 0, myHigh = 0;
if( typeof( window.innerWidth ) == 'number' ) {
//Non-IE
myWide		= window.innerWidth;
myHigh		= window.innerHeight;
} else if( document.documentElement && ( document.documentElement.clientWidth || document.documentElement.clientHeight ) ) {
//IE 6+ in 'standards compliant mode'
myWide		= document.documentElement.clientWidth;
myHigh		= document.documentElement.clientHeight;
} else if( document.body && ( document.body.clientWidth || document.body.clientHeight ) ) {
//IE 4 compatible
myWide		= document.body.clientWidth;
myHigh		= document.body.clientHeight;
}
// window.alert( 'Width = ' + myWide + ' Height = ' + myHigh );
document.getElementById('winHigh').value = myHigh;
document.getElementById('winWide').value = myWide;
}

window.onresize  = function()
{
var x = winSize();
}
var x = winSize();
//--></script>
"
```



## Scala


```Scala
import java.awt.{Dimension, Insets, Toolkit}

import javax.swing.JFrame

class MaxWindowDims() extends JFrame {
  val toolkit: Toolkit = Toolkit.getDefaultToolkit
  val (insets0, screenSize) = (toolkit.getScreenInsets(getGraphicsConfiguration),  toolkit.getScreenSize)

  println("Physical screen size: " + screenSize)
  System.out.println("Insets: " + insets0)
  screenSize.width -= (insets0.left + insets0.right)
  screenSize.height -= (insets0.top + insets0.bottom)
  System.out.println("Max available: " + screenSize)
}

object MaxWindowDims {
  def main(args: Array[String]): Unit = {
    new MaxWindowDims
  }
}
```


## Sidef

Using the Tk library:

```ruby
require('Tk')

func max_window_size() -> (Number, Number) {
    %s'MainWindow'.new.maxsize;
}

var (width, height) = max_window_size();
say (width, 'x', height);
```


```txt

1905x1050

```



## Tcl

```tcl
package require Tk
proc maxSize {} {
    # Need a dummy window; max window can be changed by scripts
    set top .__defaultMaxSize__
    if {![winfo exists $top]} {
        toplevel $top
        wm withdraw $top
    }
    # Default max size of window is value we want
    return [wm maxsize $top]
}
```

On this system, that returns <code>1440 836</code>. Further discussion of related matters, including platform limitations, is on [http://wiki.tcl.tk/10872 the Tcler's Wiki].

== {{header|Visual Basic}} ==


###  Method 1


The first method involves querying the screen dimensions and then subtracting pixels used by the frame and desktop bars:


```vb
TYPE syswindowstru
  screenheight AS INTEGER
  screenwidth AS INTEGER
  maxheight AS INTEGER
  maxwidth AS INTEGER
END TYPE

DIM syswindow AS syswindowstru

' Determine the height and width of the screen

syswindow.screenwidth = Screen.Width / Screen.TwipsPerPixelX
syswindow.screenheight=Screen.Height / Screen.TwipsPerPixelY

' Make adjustments for window decorations and menubars
```



###  Method 2


The alternative method is to create a form that is maximized and then query its dimensions (similar to the method used in gambas).


## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic (language version >= 14, e.g. with Visual Studio 2015)
{{works with|.NET Framework|4.7.2}} (simple enough that it should probably work on every Framework version--.NET Core 3.0 will support Windows Forms on Windows only)
Must be referenced:
<!--Wrap in a span with display:flex to remove libheader's line break-->
<span style="display:flex">{{libheader|GDI+}}<span style="white-space:pre"> (managed interface) [System.Drawing]</span></span>
<span style="display:flex">{{libheader|Windows Forms}}<span style="white-space:pre"> [System.Windows.Forms]</span></span>

Bounds are the screen's dimensions; working area is the is the region that excludes "taskbars, docked windows, and docked tool bars" (from Framework documentation).


```vbnet
Imports System.Drawing
Imports System.Windows.Forms

Module Program
    Sub Main()
        Dim bounds As Rectangle = Screen.PrimaryScreen.Bounds
        Console.WriteLine($"Primary screen bounds:  {bounds.Width}x{bounds.Height}")

        Dim workingArea As Rectangle = Screen.PrimaryScreen.WorkingArea
        Console.WriteLine($"Primary screen working area:  {workingArea.Width}x{workingArea.Height}")
    End Sub
End Module
```


```txt
Primary screen bounds:  1714x1143
Primary screen working area:  1714x1103
```


Alternatively, use the dimensions of a borderless form with WindowState set to FormWindowState.Maximized (i.e. a full-screen window that is shown above the taskbar).


```vbnet
Imports System.Drawing
Imports System.Windows.Forms

Module Program
    Sub Main()
        Using f As New Form() With {
            .WindowState = FormWindowState.Maximized,
            .FormBorderStyle = FormBorderStyle.None
            }

            f.Show()
            Console.WriteLine($"Size of maximized borderless form:  {f.Width}x{f.Height}")
        End Using
    End Sub
End Module
```


```txt
Size of maximized borderless form:  1714x1143
```


