+++
title = "Hello world/Graphical"
description = ""
date = 2019-10-15T14:19:41Z
aliases = []
[extra]
id = 2575
[taxonomies]
categories = ["task", "GUI"]
tags = []
+++

## Task

Display the string       '''Goodbye, World!'''       on a [[GUI]] object   (alert box, plain window, text area, etc.).


## Related tasks

*   [[Hello world/Text]]





## ActionScript


```actionscript

var textField:TextField = new TextField();
stage.addChild(textField);
textField.text =  "Goodbye, World!"

```



## Ada

```ada
with Gdk.Event;   use Gdk.Event;
with Gtk.Label;   use Gtk.Label;
with Gtk.Window;  use Gtk.Window;
with Gtk.Widget;  use Gtk.Widget;

with Gtk.Handlers;
with Gtk.Main;

procedure Windowed_Goodbye_World is
   Window : Gtk_Window;
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

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk_New (Label, "Goodbye, World!");
   Add (Window, Label);
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
   Show_All (Label);
   Show (Window);

   Gtk.Main.Main;
end Windowed_Goodbye_World;
```


## ALGOL 68

The code below is a gentle re-write (including a bug fix) of that in
the Algol 68 Genie documentation.

```algol68

BEGIN
   FILE window;
   open (window, "Hello!", stand draw channel);
   draw device (window, "X", "600x400");
   draw erase (window);
   draw move (window, 0.25, 0.5);
   draw colour (window, 1, 0, 0);
   draw text (window, "c", "c", "Goodbye, world!");
   draw show (window);
   close (window)
END

```

## App Inventor


###  No Blocks solution

This solution requires no code blocks as the text is entered directly into the Title properties TextBox of the Designer.

[https://lh4.googleusercontent.com/-RO_nNXm3sw8/UuwFSbeGk6I/AAAAAAAAJ-U/TH1rbpQ9HRE/s1600/noblocks.PNG VIEW THE DESIGNER]


### Three blocks solution

This solution uses three blocks to assign the text to the Title bar:

Screen1.Initialize and

set Screen1.Title to "Goodbye World!"

[https://lh6.googleusercontent.com/-0MGq3ZZTgT8/UuwF5KBd7-I/AAAAAAAAJ-c/HirntN5II9g/s1600/blocks.PNG VIEW THE BLOCKS AND ANDROID APP SCREEN]


## AppleScript


```applescript
display dialog "Goodbye, World!" buttons {"Bye"}
```


## Applesoft BASIC


```Applesoft BASIC
  1 LET T$ = "GOODBYE, WORLD!"
  2 LET R = 5:GX = 3:GY = 2:O = 3:XC = R + GX:YC = R * 2 + GY
  3 TEXT : HOME : TEXT : HGR : HCOLOR= 7: HPLOT 0,0: CALL 62454: HCOLOR= 6
  4 LET L =  LEN (T$): FOR I = 1 TO L:K =  ASC ( MID$ (T$,I,1)):XO = XC:YO = YC: GOSUB 5:XC = XO + 1:YC = YO: GOSUB 7: NEXT : END
  5 IF K > 64 THEN K = K + LC: GOSUB 20:LC = 32: RETURN
  6 LET LC = 0: ON K >  = 32 GOTO 20: RETURN
  7 GOSUB 20:XC = XC + R * 2 + GX: IF XC > 279 - R THEN XC = R + GX:YC = YC + GY + R * 5
  8 RETURN
  9 LET XC = XC - R * 2: RETURN
 10 LET Y = R:D = 1 - R:X = 0
 11 IF D >  = 0 THEN Y = Y - 1:D = D - Y * 2
 12 LET D = D + X * 2 + 3
 13 IF O = 1 OR O = 3 THEN  GOSUB 17
 14 IF O = 2 OR O = 3 THEN  GOSUB 19
 15 LET X = X + 1: IF X < Y THEN 11
 16 LET O = 3:E = 0: RETURN
 17 HPLOT XC - X,YC + Y: HPLOT XC + X,YC + Y: HPLOT XC - Y,YC + X: IF  NOT E THEN  HPLOT XC + Y,YC + X
 18 RETURN
 19 HPLOT XC - X,YC - Y: HPLOT XC + X,YC - Y: HPLOT XC - Y,YC - X: HPLOT XC + Y,YC - X: RETURN
 20 LET M = K - 31
 21 ON M GOTO 32,33,34,35,36,37,38,39,40,41,42,43,44
 22 LET M = M - 32
 23 ON M GOTO 64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87
 24 LET M = M - 32
 25 ON M GOTO 96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,10,112,113,114,115,116,117,118,119,120,121
 32 RETURN
 33 HPLOT XC - R,YC - R * 2 TO XC - R,YC + R - GY: HPLOT XC - R,YC + R: GOTO 9: REM !
 44 HPLOT XC - R,YC + R + R / 2 TO XC - R,YC + R: GOTO 9: REM ,
 71 LET O = 2:YC = YC - R: GOSUB 10:YC = YC + R: HPLOT XC - R,YC TO XC - R,YC - R: HPLOT XC + R / 2,YC TO XC + R,YC TO XC + R,YC + R:O = 1: GOTO 10: REM G
 87 HPLOT XC - R,YC - R * 2 TO XC - R,YC + R TO XC,YC TO XC + R,YC + R TO XC + R,YC - R * 2: RETURN : REM W
 98 HPLOT XC - R,YC - R * 2 TO XC - R,YC + R: GOTO 10: RETURN : REM B
100 HPLOT XC + R,YC - R * 2 TO XC + R,YC + R: GOTO 10: REM D
101 HPLOT XC - R,YC TO XC + R,YC:E = 1: GOTO 10: REM E
108 HPLOT XC - R,YC - R * 2 TO XC - R,YC + R: GOTO 9: REM L
114 HPLOT XC - R,YC - R TO XC - R,YC + R:O = 2: GOTO 10: REM R
121 HPLOT XC - R,YC - R TO XC,YC + R: HPLOT XC + R,YC - R TO XC - R,YC + R * 3: RETURN : REM Y
```



## Arendelle



```txt
// title

   "Hello, World!"

// first spacings

   [ 5 , rd ]

// body

   /* H */ [7,pd][4,u][3,pr][3,d][7,pu]drr
   /* E */ [6,pd][4,pr]l[3,u][2,lp][3,u][3,pr]r
   /* L */ [7,pd]u[3,rp][6,u]rr
   /* L */ [7,pd]u[3,rp][6,u]rr
   /* O */ [7,pd]u[2,rp]r[6,pu][3,pl][5,r]
   /* , */ [5,d]prpd[3,pld][9,u][5,r]
   /*   */ rrr
   /* W */ [4,pd][2,prd][2,pru][5,pu][5,d][2,prd][2,pru][5,pu]rrd
   /* O */ [7,pd]u[2,rp]r[6,pu][3,pl][5,r]
   /* R */ [7,pd][7,u][3,rp][3,pd][3,pl]rrdpr[2,dp][6,u]rr
   /* L */ [7,pd]u[3,rp][6,u]rr
   /* D */ [6,pd][3,pr][5,up]u[2,lp]p[4,r]
   /* ! */ r[5,pd]dp[6,u]rr

// done
```


## Arturo

```arturo
use ~gui

window @mainWindow #{
	:title 	"Hello World"
	:size 	#(200 200)

	label "Hello World!" #{}
}

app @main "TestHello" mainWindow #{}
main.run
```


## ATS


```ATS
//
#include
"share/atspre_define.hats"
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

staload UN = $UNSAFE

(* ****** ****** *)

staload "{$GLIB}/SATS/glib.sats"

(* ****** ****** *)

staload "{$GTK}/SATS/gdk.sats"
staload "{$GTK}/SATS/gtk.sats"
staload "{$GLIB}/SATS/glib-object.sats"

(* ****** ****** *)

%{^
typedef char **charpp ;
%} ;
abstype charpp = $extype"charpp"

(* ****** ****** *)

fun hello
(
  widget: !GtkWidget1, _: gpointer
) : void = print ("Goodbye, world!\n")

fun on_delete_event
(
  widget: !GtkWidget1
, event: &GdkEvent, udata: gpointer
) : gboolean = let
  val () = print ("delete event occurred\n")
in
  GTRUE // handling of delete-event is finished
end // end of [on_delete_event]

fun on_destroy
  (widget: !GtkWidget1, _: gpointer): void = gtk_main_quit ()
// end of [on_destroy]

(* ****** ****** *)

macdef nullp = the_null_ptr

(* ****** ****** *)

implement
main0 (argc, argv) =
{
//
var argc: int = argc
var argv: charpp = $UN.castvwtp1{charpp}(argv)
//
val () = $extfcall (void, "gtk_init", addr@(argc), addr@(argv))
//
val window =
  gtk_window_new (GTK_WINDOW_TOPLEVEL)
val () = assertloc (ptrcast(window) > 0)
//
val _(*id*) =
g_signal_connect (
  window, (gsignal)"destroy", (G_CALLBACK)on_destroy, (gpointer)nullp
) (* end of [val] *)
val _(*id*) =
g_signal_connect (
  window, (gsignal)"delete_event", (G_CALLBACK)on_delete_event, (gpointer)nullp
) (* end of [val] *)
//
val () = gtk_container_set_border_width (window, (guint)10)
val button = gtk_button_new_with_label (gstring("Goodbye, world!"))
val () = assertloc (ptrcast(button) > 0)
//
val () = gtk_widget_show (button)
val () = gtk_container_add (window, button)
val () = gtk_widget_show (window)
//
val _(*id*) =
g_signal_connect
(
  button, (gsignal)"clicked", (G_CALLBACK)hello, (gpointer)nullp
)
val _(*id*) =
g_signal_connect_swapped
(
  button, (gsignal)"clicked", (G_CALLBACK)gtk_widget_destroy, window
)
//
val () = g_object_unref (button)
val () = g_object_unref (window) // ref-count becomes 1!
//
val ((*void*)) = gtk_main ()
//
} (* end of [main0] *)
```



## AutoHotkey



```autohotkey
MsgBox, Goodbye`, World!
```


```autohotkey
ToolTip, Goodbye`, World!
```


```autohotkey
Gui, Add, Text,   x4    y4,   To be announced:
Gui, Add, Edit,   xp+90 yp-3, Goodbye, World!
Gui, Add, Button, xp+98 yp-1, OK
Gui, Show, w226 h22     , Rosetta Code
Return
```


```autohotkey
SplashTextOn, 100, 100, Rosetta Code, Goodbye, World!
```



## AutoIt


```autoit>#include <GUIConstantsEx.au3


$hGUI = GUICreate("Hello World") ; Create the main GUI
GUICtrlCreateLabel("Goodbye, World!", -1, -1) ; Create a label dispalying "Goodbye, World!"

GUISetState() ; Make the GUI visible

While 1 ; Infinite GUI loop
	$nMsg = GUIGetMsg() ; Get any messages from the GUI
	Switch $nMsg ; Switch for a certain event
		Case $GUI_EVENT_CLOSE ; When an user closes the windows
			Exit ; Exit

	EndSwitch
WEnd

```



```autoit
MsgBox(0, "Goodbye", "Goodbye, World!")
```



```autoit
ToolTip("Goodbye, World!")
```



## AWK

Awk has no GUI, but can execute system-commands.

E.g. the Windows-commandline provides a command for a messagebox,

see below
at  [[Hello_world/Graphical#Batch_File|Batch_File]]
and [[Hello_world/Graphical#UNIX_Shell|UNIX_Shell]].


```awk
# Usage:  awk -f hi_win.awk
BEGIN { system("msg * Goodbye, Msgbox !") }

```



## Axe

This example is almost identical to the [[#TI-83_BASIC|TI-83 BASIC version]].

```axe
ClrHome
Text(0,0,"Goodbye, world!")
Pause 5000
```



## BaCon

Uses HUG (High level Universal GUI) shipped in hug.bac


```qbasic
REM GUI greeting

INCLUDE "hug.bac"

mainwindow = WINDOW("BaCon greeting", 225, 40)

greeting = ENTRY("Goodbye, World!", 115, 30)
ATTACH(mainwindow, greeting, 5, 5)

REM gtk event loop...
DISPLAY

```



## BASIC

```freebasic
' Demonstrate a simple Windows application using FreeBasic

#include once "windows.bi"

Declare Function WinMain(ByVal hInst As HINSTANCE, _
      ByVal hPrev As HINSTANCE, _
      ByVal szCmdLine as String, _
      ByVal iCmdShow As Integer) As Integer
End WinMain( GetModuleHandle( null ), null, Command( ), SW_NORMAL )

Function WinMain (ByVal hInst As HINSTANCE, _
                  ByVal hPrev As HINSTANCE, _
                  ByVal szCmdLine As String, _
                  ByVal iCmdShow As Integer) As Integer
    MessageBox(NULL, "Goodbye World", "Goodbye World", MB_ICONINFORMATION)
    function = 0
End Function
```



```freebasic
' Demonstrate a simple Windows/Linux application using GTK/FreeBasic

#INCLUDE "gtk/gtk.bi"

gtk_init(@__FB_ARGC__, @__FB_ARGV__)

VAR win = gtk_window_new (GTK_WINDOW_TOPLEVEL)
gtk_window_set_title (gtk_window (win), "Goodbye, World")
g_signal_connect(G_OBJECT (win), "delete-event", @gtk_main_quit, 0)
gtk_widget_show_all (win)

gtk_main()

END 0
```



## BASIC256


```BASIC256
clg
font "times new roman", 20,100
color orange
rect 10,10, 140,30
color red
text 10,10, "Goodbye, World!"
```



## Basic Casio

To configure the "Graphical screen"

```Basic Casio
ViewWindow 1,127,1,1,63,1
AxesOff
CoordOff
GridOff
LabelOff
```

ViewWindow parameters depend on the calculator resolution (These are the most common).

To print text on the "Graphical screen" of the calculator:

```Basic Casio
Text 1,1,"Goodbye, World!"
ClrGraph
```



## Batch File

From Window 7 and later, pure Batch File does not completely provide GUI. However, <code>MSHTA.EXE</code> provides command-line JavaScript/VBScript access.

```dos
@echo off

::Output to message box [Does not work in Window 7 and later]
msg * "Goodbye, World!" 2>nul

::Using MSHTA.EXE Hack::
@mshta javascript:alert("Goodbye, World!");code(close());
@mshta vbscript:Execute("msgbox(""Goodbye, World!""):code close")
pause
```



## BBC BASIC

```bbcbasic
      SYS "MessageBox", @hwnd%, "Goodbye, World!", "", 0
```



## BML


```bml
msgbox Goodbye, World!
```



## C

```c
#include <gtk/gtk.h>

int main (int argc, char **argv) {
  GtkWidget *window;
  gtk_init(&argc, &argv);

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), "Goodbye, World");
  g_signal_connect (G_OBJECT (window), "delete-event", gtk_main_quit, NULL);
  gtk_widget_show_all (window);

  gtk_main();
  return 0;
}
```


Where hWnd is a valid window handle corresponding to a control in the application

```c
#include <windows.h>
```

void SayGoodbyeWorld(HWND hWnd)
{
  SetWindowText(hWnd, _T("Goodbye, World!"));
}
```


Or simplest way:

```c
#include <windows.h>

MessageBox(NULL, _T("Goodbye, World!"), _T("Rosettacode"), MB_OK | MB_ICONINFORMATION);
/* different buttons and icons can be used. please read MS documentation for details. */

```


## C#

```c#
using System;
using System.Windows.Forms;

class Program {
    static void Main(string[] args) {
        Application.EnableVisualStyles(); //Optional.
        MessageBox.Show("Goodbye, World!");
    }
}
```


```c#
using Gtk;
using GtkSharp;

public class GoodbyeWorld {
  public static void Main(string[] args) {
    Gtk.Window window = new Gtk.Window();
    window.Title = "Goodbye, World";
    window.DeleteEvent += delegate { Application.Quit(); };
    window.ShowAll();
    Application.Run();
  }
}
```



## C++

{{libheader|GTK}}<!-- c++ bindings -->

```cpp
#include <gtkmm.h>

int main(int argc, char *argv[])
{
   Gtk::Main app(argc, argv);
   Gtk::MessageDialog msg("Goodbye, World!");
   msg.run();
}
```

All Win32 APIs work in C++ the same way as they do in C.  See the C example.

Where pWnd is a pointer to a CWnd object corresponding to a valid window in the application.

```cpp
#include "afx.h"
void ShowGoodbyeWorld(CWnd* pWnd)
{
    pWnd->SetWindowText(_T("Goodbye, World!"));
}
```


```cpp

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Box.H>

int main(int argc, char **argv) {
  Fl_Window *window = new Fl_Window(300,180);
  Fl_Box *box = new Fl_Box(20,40,260,100,"Goodbye, World!");
  box->box(FL_UP_BOX);
  box->labelsize(36);
  box->labelfont(FL_BOLD+FL_ITALIC);
  box->labeltype(FL_SHADOW_LABEL);
  window->end();
  window->show(argc, argv);
  return Fl::run();
}

```



## C++/CLI


```cpp

using namespace System::Windows::Forms;

int main(array<System::String^> ^args)
{
  MessageBox::Show("Goodbye, World!", "Rosetta Code");
  return 0;
}

```



## Clean

```clean
import StdEnv, StdIO

Start :: *World -> *World
Start world = startIO NDI Void (snd o openDialog undef hello) [] world
where
    hello = Dialog "" (TextControl "Goodbye, World!" [])
                                     [WindowClose (noLS closeProcess)]
```



## Clojure


```lisp
(ns experimentation.core
  (:import (javax.swing JOptionPane JFrame JTextArea JButton)
     (java.awt FlowLayout)))

(JOptionPane/showMessageDialog nil "Goodbye, World!")
(let [button (JButton. "Goodbye, World!")
      window (JFrame. "Goodbye, World!")
      text (JTextArea. "Goodbye, World!")]
  (doto window
    (.setLayout (FlowLayout.))
    (.add button)
    (.add text)
    (.pack)
    (.setDefaultCloseOperation (JFrame/EXIT_ON_CLOSE))
    (.setVisible true)))
```



## COBOL


### GUI

The following are in the Managed COBOL dialect.
```cobol
       CLASS-ID ProgramClass.
       METHOD-ID Main STATIC.
       PROCEDURE DIVISION.
           INVOKE TYPE Application::EnableVisualStyles() *> Optional
           INVOKE TYPE MessageBox::Show("Goodbye, World!")
       END METHOD.
       END CLASS.
```


gui.xaml.cbl:

```cobol
       CLASS-ID GoodbyeWorldWPF.Window IS PARTIAL
                 INHERITS TYPE System.Windows.Window.
       METHOD-ID NEW.
       PROCEDURE DIVISION.
           INVOKE self::InitializeComponent()
       END METHOD.
       END CLASS.
```


gui.xaml:

```xaml
<Window x:Class="COBOL_WPF.Window1"
    xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
    Title="Hello world/Graphical">
    <TextBox>Goodbye, World!</TextBox>
</Window>
```



### GTK

```cobol
      *>
      *> cobweb-gui-hello, using gtk-label
      *> Tectonics:
      *>   cobc -w -xj cobweb-gui-hello.cob cobweb-gtk.cob \
      *>        `pkg-config --libs gtk+-3.0`
      *>
       identification division.
       program-id. cobweb-gui-hello.

       environment division.
       configuration section.
       repository.
           function new-window
           function new-box
           function new-label
           function gtk-go
           function all intrinsic.

       data division.
       working-storage section.

       01 TOPLEVEL                     usage binary-long value 0.
       01 HORIZONTAL                   usage binary-long value 0.
       01 VERTICAL                     usage binary-long value 1.

       01 width-hint                   usage binary-long value 160.
       01 height-hint                  usage binary-long value 16.

       01 spacing                      usage binary-long value 8.
       01 homogeneous                  usage binary-long value 0.

       01 extraneous                   usage binary-long.

       01 gtk-window-data.
          05 gtk-window                usage pointer.
       01 gtk-container-data.
          05 gtk-container             usage pointer.

       01 gtk-box-data.
          05 gtk-box                   usage pointer.
       01 gtk-label-data.
          05 gtk-label                 usage pointer.

       procedure division.
       cobweb-hello-main.

      *> Main window and top level container
       move new-window("Hello", TOPLEVEL, width-hint, height-hint)
         to gtk-window-data
       move new-box(gtk-window, VERTICAL, spacing, homogeneous)
         to gtk-container-data

      *> Box, across, with simple label
       move new-box(gtk-container, HORIZONTAL, spacing, homogeneous)
         to gtk-box-data
       move new-label(gtk-box, "Goodbye, World!") to gtk-label-data

      *> GTK+ event loop now takes over
       move gtk-go(gtk-window) to extraneous

       goback.
       end program cobweb-gui-hello.
```



### TUI

The program gets the lines and columns of the screen and positions the text in the middle. Program waits for a return key.


```cobol
  program-id. ghello.
  data division.
  working-storage section.
  01  var pic x(1).
  01  lynz  pic 9(3).
  01  colz  pic 9(3).
  01  msg pic x(15) value "Goodbye, world!".
  procedure division.
    accept lynz from lines end-accept
    divide lynz by 2 giving lynz.
    accept colz from columns end-accept
    divide colz by 2 giving colz.
    subtract 7 from colz giving colz.
    display msg
      at line number lynz
      column number colz
    end-display
    accept var end-accept
    stop run.
```



## Cobra


Requires {{libheader|GTK#}} GUI library.


```cobra

@args -pkg:gtk-sharp-2.0

use Gtk

class MainProgram
  def main
    Application.init
    dialog = MessageDialog(nil,
      DialogFlags.DestroyWithParent,
      MessageType.Info,
      ButtonsType.Ok,
      "Goodbye, World!")
    dialog.run
    dialog.destroy

```



## CoffeeScript


```coffeescript
alert "Goodbye, World!"
```



## Common Lisp

This can be done using the extension package ''ltk'' that provides an interface to the ''Tk'' library.
```lisp
(use-package :ltk)

(defun show-message (text)
  "Show message in a label on a Tk window"
  (with-ltk ()
      (let* ((label (make-instance 'label :text text))
             (button (make-instance 'button :text "Done"
                                    :command (lambda ()
                                               (ltk::break-mainloop)
                                               (ltk::update)))))
              (pack label :side :top :expand t :fill :both)
              (pack button :side :right)
              (mainloop))))

(show-message "Goodbye World")
```


This can also be done using the ''CLIM 2.0'' specification. The following code runs on both SBCL and the LispWorks
IDE:
```lisp

((in-package :clim-user)

(defclass hello-world-pane
    (clim-stream-pane) ())

(define-application-frame hello-world ()
  ((greeting :initform "Goodbye World"
             :accessor greeting))
  (:pane (make-pane 'hello-world-pane)))

;;; Behaviour defined by the Handle Repaint Protocol
(defmethod handle-repaint ((pane hello-world-pane) region)
  (let ((w (bounding-rectangle-width pane))
        (h (bounding-rectangle-height pane)))
    ;; Blank the pane out
    (draw-rectangle* pane 0 0 w h
                     :filled t
                     :ink (pane-background pane))
    ;; Draw greeting in center of pane
    (draw-text* pane
                (greeting *application-frame*)
                (floor w 2) (floor h 2)
                :align-x :center
                :align-y :center)))

(run-frame-top-level
 (make-application-frame 'hello-world
   :width 200 :height 200))
```



## Creative Basic


```Creative Basic

DEF Win:WINDOW
DEF Close:CHAR
DEF ScreenSizeX,ScreenSizeY:INT

GETSCREENSIZE(ScreenSizeX,ScreenSizeY)

WINDOW Win,0,0,ScreenSizeX,ScreenSizeY,0,0,"Goodbye program",MainHandler

PRINT Win,"Goodbye, World!"
'Prints in the upper left corner of the window (position 0,0).

WAITUNTIL Close=1

CLOSEWINDOW Win

END

SUB MainHandler

    IF @CLASS=@IDCLOSEWINDOW THEN Close=1

RETURN

```



## D

```d
import gtk.MainWindow, gtk.Label, gtk.Main;

class GoodbyeWorld : MainWindow {
    this() {
        super("GtkD");
        add(new Label("Goodbye World"));
        showAll();
    }
}

void main(string[] args) {
    Main.init(args);
    new GoodbyeWorld();
    Main.run();
}
```



## Delphi


```Delphi
program HelloWorldGraphical;

uses
  Dialogs;

begin
  ShowMessage('Goodbye, World!');
end.
```



## Dylan

(This works entered into the interactive shell):

```dylan
notify-user("hello world!", frame: make(<frame>));
```



## E


This is a complete application. If it were part of a larger application, the portions related to <code>interp</code> would be removed.


```e>def <widget
 := <swt:widgets.*>
def SWT := <swt:makeSWT>

def frame := <widget:makeShell>(currentDisplay)
  frame.setText("Rosetta Code")
  frame.setBounds(30, 30, 230, 60)
  frame.addDisposeListener(def _ { to widgetDisposed(event) {
    interp.continueAtTop()
  }})

def label := <widget:makeLabel>(frame, SWT.getLEFT())
  label.setText("Goodbye, World!")
  swtGrid`$frame: $label`

frame.open()

interp.blockAtTop()
```



## EasyLang


<lang>move 10 20
text "Goodbye, World!"
```



## eC

MessageBox:


```ec
import "ecere"
MessageBox goodBye { contents = "Goodbye, World!" };
```


Label:


```ec
import "ecere"
Label label { text = "Goodbye, World!", hasClose = true, opacity = 1, size = { 320, 200 } };
```


Titled Form + Surface Output:


```ec
import "ecere"

class GoodByeForm : Window
{
   text = "Goodbye, World!";
   size = { 320, 200 };
   hasClose = true;

   void OnRedraw(Surface surface)
   {
      surface.WriteTextf(10, 10, "Goodbye, World!");
   }
}

GoodByeForm form {};
```



## EchoLisp


```lisp

(alert "Good bye, world!")

```



## Elena

ELENA 4.x :

```elena
import forms;

public class MainWindow : SDIDialog
{
    Label goodByeWorldLabel;
    Button closeButton;

    constructor new()
       <= new()
    {
        goodByeWorldLabel := new Label();
        closeButton       := new Button();

        self
            .appendControl(goodByeWorldLabel)
            .appendControl(closeButton);

        self.setRegion(250, 200, 200, 110);

        goodByeWorldLabel.Caption := "Goodbye, World!";
        goodByeWorldLabel.setRegion(40, 10, 150, 30);

        closeButton.Caption := "Close";
        closeButton.setRegion(20, 40, 150, 30);
        closeButton.onClick := (args){ forward program.stop() };
    }
}
```



## Euphoria


### Message box


```euphoria
include msgbox.e

integer response
response = message_box("Goodbye, World!","Bye",MB_OK)
```



## EGL

Allows entry of any name into a text field (using "World" as the default entry). Then, when the "Say Goodbye" button is pressed, sets a text label to the value "Goodbye, <name>!".

```egl

import org.eclipse.edt.rui.widgets.*;
import dojo.widgets.*;

handler HelloWorld type RUIhandler{initialUI =[ui]}

    ui Box {columns=1, children=[nameField, helloLabel, goButton]};

    nameField DojoTextField {placeHolder = "What's your name?", text = "World"};
    helloLabel TextLabel {};
    goButton DojoButton {text = "Say Goodbye", onClick ::= onClick_goButton};

    function onClick_goButton(e Event in)
        helloLabel.text = "Goodbye, " + nameField.text + "!";
    end

end

```



=={{header|F_Sharp|F#}}==
Just display the text in a message box.

```fsharp
#light
open System
open System.Windows.Forms
[<EntryPoint>]
let main _ =
    MessageBox.Show("Hello World!") |> ignore
    0
```



## Factor

To be pasted in the listener :
    USING: ui ui.gadgets.labels ;
    [ "Goodbye World" <label> "Rosetta Window" open-window ] with-ui


## Fantom


```fantom

using fwt

class Hello
{
  public static Void main ()
  {
    Dialog.openInfo (null, "Goodbye world")
  }
}

```



## Forth


```forth
HWND z" Goodbye, World!" z" (title)" MB_OK MessageBox
```


Alternative:
```forth
 s" Goodbye, World!" MsgBox
```



## Fortran



###  MS Windows

Here are solutions for '''Microsoft Windows''', using the [https://msdn.microsoft.com/en-us/library/windows/desktop/ms645505.aspx MessageBox] API function. Both programs use modules provided by the compiler vendor.

```fortran
program hello
    use windows
    integer :: res
    res = MessageBoxA(0, LOC("Hello, World"), LOC("Window Title"), MB_OK)
end program
```


Compile with <code>af90 hello.f90 user32.lib</code> or for a 64-bit executable <code>af90 -i8 -m64 hello.f90 user32.lib</code>.

```fortran
program hello
    use user32
    integer :: res
    res = MessageBox(0, "Hello, World", "Window Title", MB_OK)
end program
```


Compile with <code>ifort hello.f90</code>.


###  Linux

Using [https://github.com/jerryd/gtk-fortran gtk-fortran]  library
```fortran

module handlers_m
  use iso_c_binding
  use gtk
  implicit none

 contains

   subroutine destroy (widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    call gtk_main_quit ()
  end subroutine destroy

end module handlers_m

program test
  use iso_c_binding
  use gtk
  use handlers_m
  implicit none

  type(c_ptr) :: window
  type(c_ptr) :: box
  type(c_ptr) :: button

  call gtk_init ()
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  call gtk_window_set_default_size(window, 500, 20)
  call gtk_window_set_title(window, "gtk-fortran"//c_null_char)
  call g_signal_connect (window, "destroy"//c_null_char, c_funloc(destroy))
  box = gtk_hbox_new (TRUE, 10_c_int);
  call gtk_container_add (window, box)
  button = gtk_button_new_with_label ("Goodbye, World!"//c_null_char)
  call gtk_box_pack_start (box, button, FALSE, FALSE, 0_c_int)
  call g_signal_connect (button, "clicked"//c_null_char, c_funloc(destroy))
  call gtk_widget_show (button)
  call gtk_widget_show (box)
  call gtk_widget_show (window)
  call gtk_main ()

end program test

```

Compile with
<code>gfortran gtk2_mini.f90 -o gtk2_mini.x `pkg-config --cflags --libs gtk-2-fortran`</code>


## FreeBASIC


'''Graphics Mode'''

```FreeBASIC
screen 1 'Mode 320x200
locate 12,15
? "Goodbye, World!"
sleep
```


'''Windows API'''

```FreeBASIC
#INCLUDE "windows.bi"
MessageBox(0, "Goodbye, World!", "Message",0)
```



## Frege



```frege
package HelloWorldGraphical where

import Java.Swing

main _ = do
    frame <- JFrame.new "Goodbye, world!"
    frame.setDefaultCloseOperation(JFrame.dispose_on_close)
    label <- JLabel.new "Goodbye, world!"
    cp <- frame.getContentPane
    cp.add label
    frame.pack
    frame.setVisible true
```



## Frink

This brings up an infinitely-rescalable graphic window containing "Goodbye, World" drawn graphically.

All Frink graphics can be written to arbitrary coordinates;  Frink will automatically scale and center any drawn graphics to be visible in the window (greatly simplifying programming,) so the exact coordinates used below are rather arbitrary.  (This means that if you wrote "Hello World" instead of "Goodbye, World", you could just change that string and everything would still center perfectly.)

The graphics are infinitely-scalable and can be rendered at full quality to any resolution.  This program "shows off" by rotating the text by 10 degrees, and also rendering it to a printer (which can include tiling across multiple pages) and rendering to a graphics file.   (Frink can automatically render the same graphics object to many image formats, including PNG, JPG, SVG, HTML5 canvas, animated GIF, bitmapped image in memory, and more.)


```frink

g = new graphics
g.font["SansSerif", 10]
g.text["Goodbye, World!", 0, 0, 10 degrees]
g.show[]

g.print[]                           // Optional: render to printer
g.write["GoodbyeWorld.png", 400, 300] // Optional: write to graphics file

```



## FunL


```funl
native javax.swing.{SwingUtilities, JPanel, JLabel, JFrame}
native java.awt.Font

def createAndShowGUI( msg ) =
  f = JFrame()
  f.setTitle( msg )
  f.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE )
  p = JPanel()
  l = JLabel( msg )
  l.setFont( Font.decode(Font.SERIF + ' 150') )
  p.add( l )
  f.add( p )
  f.pack()
  f.setResizable( false )
  f.setVisible( true )

SwingUtilities.invokeLater( createAndShowGUI.runnable('Goodbye, World!') )
```



## Gambas



```gambas
Message.Info("Goodbye, World!")    ' Display a simple message box
```



## Genie


```genie
[indent=4]
/*
  Genie GTK+ hello
  valac --pkg gtk+-3.0 hello-gtk.gs
  ./hello-gtk
*/
uses Gtk

init
    Gtk.init (ref args)
    var window = new Window (WindowType.TOPLEVEL)
    var label = new Label("Goodbye, World!")
    window.add(label)
    window.set_default_size(160, 100)
    window.show_all()
    window.destroy.connect(Gtk.main_quit)
    Gtk.main()
```



## GlovePIE

The text is rendered using Braille text characters.

```GlovePIE
debug="⡧⢼⣟⣋⣇⣀⣇⣀⣏⣹⠀⠀⣇⣼⣏⣹⡯⡽⣇⣀⣏⡱⢘⠀"
```



## GML


```gml
draw_text(0,0,"Goodbye World!");
```



## Go

```go
package main

import "github.com/mattn/go-gtk/gtk"

func main() {
   gtk.Init(nil)
   win := gtk.NewWindow(gtk.WINDOW_TOPLEVEL)
   win.SetTitle("Goodbye, World!")
   win.SetSizeRequest(300, 200)
   win.Connect("destroy", gtk.MainQuit)
   button := gtk.NewButtonWithLabel("Goodbye, World!")
   win.Add(button)
   button.Connect("clicked", gtk.MainQuit)
   win.ShowAll()
   gtk.Main()
}
```



## Groovy

```groovy
import groovy.swing.SwingBuilder
import javax.swing.JFrame

new SwingBuilder().edt {
  optionPane().showMessageDialog(null, "Goodbye, World!")
  frame(title:'Goodbye, World!', defaultCloseOperation:JFrame.EXIT_ON_CLOSE, pack:true, show: true) {
    flowLayout()
    button(text:'Goodbye, World!')
    textArea(text:'Goodbye, World!')
  }
}

```



## GUISS


Here we display the message on the system notepad:


```guiss
Start,Programs,Accessories,Notepad,Type:Goodbye[comma][space]World[pling]
```



## Harbour


```visualfoxpro
PROCEDURE Main()
   RETURN wapi_MessageBox(,"Goodbye, World!","")

```



## Haskell

Using {{libheader|gtk}} from [http://hackage.haskell.org/packages/hackage.html HackageDB]

```haskell
import Graphics.UI.Gtk
import Control.Monad

messDialog = do
  initGUI
  dialog <- messageDialogNew Nothing [] MessageInfo ButtonsOk "Goodbye, World!"

  rs <- dialogRun dialog
  when (rs == ResponseOk || rs == ResponseDeleteEvent) $ widgetDestroy dialog

  dialog `onDestroy` mainQuit

  mainGUI
```

Run in GHCi interpreter:

```haskell
*Main> messDialog
```



## HicEst


```hicest
WRITE(Messagebox='!') 'Goodbye, World!'
```



## HPPPL

With an alert box:

```HPPPL
MSGBOX("Goodbye, World!");
```

By drawing directly to the screen:

```HPPPL
RECT();
TEXTOUT_P("Goodbye, World!", GROBW_P(G0)/4, GROBH_P(G0)/4, 7);
WAIT(-1);
```



## i


```i
graphics {
	display("Goodbye, World!")
}
```



## HolyC


```holyc
PopUpOk("Goodbye, World!");
```


== Icon and Unicon ==

=
## Icon
=

```Icon
link graphics
procedure main()
   WOpen("size=100,20") | stop("No window")
   WWrites("Goodbye, World!")
   WDone()
end
```

[http://www.cs.arizona.edu/icon/library/src/gprocs/graphics.icn graphics is required ]

=
## Unicon
=


```unicon

import gui
$include "guih.icn"

class WindowApp : Dialog ()

  # -- automatically called when the dialog is created
  method component_setup ()
    # add 'hello world' label
    label := Label("label=Hello world","pos=0,0")
    add (label)

    # make sure we respond to close event
    connect(self, "dispose", CLOSE_BUTTON_EVENT)
  end
end

# create and show the window
procedure main ()
  w := WindowApp ()
  w.show_modal ()
end

```



## Integer BASIC


40&times;40 isn't great resolution, but it's enough!


```basic

10 REM FONT DERIVED FROM 04B-09 BY YUJI OSHIMOTO
20 GR
30 COLOR = 12
40 REM G
50 HLIN 0,5 AT 0 : HLIN 0,5 AT 1
60 VLIN 2,9 AT 0 : VLIN 2,9 AT 1
70 HLIN 2,5 AT 9 : HLIN 2,5 AT 8
80 VLIN 4,7 AT 5 : VLIN 4,7 AT 4
90 VLIN 4,5 AT 3
100 REM O
110 HLIN 7,12 AT 2 : HLIN 7,12 AT 3
120 HLIN 7,12 AT 8 : HLIN 7,12 AT 9
130 VLIN 4,7 AT 7 : VLIN 4,7 AT 8
140 VLIN 4,7 AT 11 : VLIN 4,7 AT 12
150 REM O
160 HLIN 14,19 AT 2 : HLIN 14,19 AT 3
170 HLIN 14,19 AT 8 : HLIN 14,19 AT 9
180 VLIN 4,7 AT 14 : VLIN 4,7 AT 15
190 VLIN 4,7 AT 18 : VLIN 4,7 AT 19
200 REM D
210 HLIN 21,24 AT 2 : HLIN 21,24 AT 3
220 HLIN 21,26 AT 8 : HLIN 21,26 AT 9
230 VLIN 4,7 AT 21 : VLIN 4,7 AT 22
240 VLIN 0,7 AT 25 : VLIN 0,7 AT 26
250 REM -
260 HLIN 28,33 AT 4 : HLIN 28,33 AT 5
270 REM B
280 VLIN 11,20 AT 0 : VLIN 11,20 AT 1
290 HLIN 2,5 AT 20 : HLIN 2,5 AT 19
300 VLIN 15,18 AT 5 : VLIN 15,18 AT 4
310 HLIN 2,5 AT 14 : HLIN 2,5 AT 13
320 REM Y
330 VLIN 13,20 AT 7 : VLIN 13,20 AT 8
340 VLIN 19,20 AT 9 : VLIN 19,20 AT 10
350 VLIN 13,24 AT 11 : VLIN 13,24 AT 12
360 VLIN 23,24 AT 10 : VLIN 23,24 AT 9
370 REM E
380 VLIN 13,20 AT 14 : VLIN 13,20 AT 15
390 HLIN 16,19 AT 13 : HLIN 16,19 AT 14
400 HLIN 18,19 AT 15 : HLIN 18,19 AT 16
410 HLIN 16,17 AT 17 : HLIN 16,17 AT 18
420 HLIN 16,19 AT 19 : HLIN 16,19 AT 20
430 REM ,
440 VLIN 17,22 AT 21 : VLIN 17,22 AT 22
450 REM W
460 VLIN 24,33 AT 0 : VLIN 24,33 AT 1 : VLIN 24,33 AT 3
470 VLIN 24,33 AT 4 : VLIN 24,33 AT 6 : VLIN 24,33 AT 7
480 HLIN 0,7 AT 33 : HLIN 0,7 AT 32
490 REM O
500 HLIN 9,14 AT 26 : HLIN 9,14 AT 27
510 HLIN 9,14 AT 32 : HLIN 9,14 AT 33
520 VLIN 28,31 AT 9 : VLIN 28,31 AT 10
530 VLIN 28,31 AT 13 : VLIN 28,31 AT 14
540 REM R
550 HLIN 16,21 AT 26 : HLIN 16,21 AT 27
560 VLIN 28,33 AT 16 : VLIN 28,33 AT 17
570 REM L
580 VLIN 24,33 AT 23 : VLIN 24,33 AT 24
590 REM D
600 HLIN 26,29 AT 26 : HLIN 26,29 AT 27
610 HLIN 26,29 AT 32 : HLIN 26,29 AT 33
620 VLIN 28,33 AT 26 : VLIN 28,33 AT 27
630 VLIN 24,33 AT 30 : VLIN 24,33 AT 31
640 REM !
650 VLIN 24,29 AT 33 : VLIN 24,29 AT 34
660 VLIN 32,33 AT 33 : VLIN 32,33 AT 34
670 END

```



## Ioke

```ioke
import(
  :javax:swing, :JOptionPane, :JFrame, :JTextArea, :JButton
)
import java:awt:FlowLayout

JOptionPane showMessageDialog(nil, "Goodbye, World!")
button = JButton new("Goodbye, World!")
text = JTextArea new("Goodbye, World!")
window = JFrame new("Goodbye, World!") do(
  layout = FlowLayout new
  add(button)
  add(text)
  pack
  setDefaultCloseOperation(JFrame field:EXIT_ON_CLOSE)
  visible = true
)
```


## IWBASIC


```IWBASIC

DEF Win:WINDOW
DEF Close:CHAR
DEF ScreenSizeX,ScreenSizeY:UINT

GETSCREENSIZE(ScreenSizeX,ScreenSizeY)

OPENWINDOW Win,0,0,ScreenSizeX,ScreenSizeY,NULL,NULL,"Goodbye program",&MainHandler

PRINT Win,"Goodbye, World!"
'Prints in upper left corner of the window (position 0,0).

WAITUNTIL Close=1

CLOSEWINDOW Win

END

SUB MainHandler

    IF @MESSAGE=@IDCLOSEWINDOW THEN Close=1

RETURN
ENDSUB

```



## J


```j
wdinfo 'Goodbye, World!'
```



## Java

```java
import javax.swing.*;
import java.awt.*;

public class OutputSwing {

    public static void main(String[] args) {

        SwingUtilities.invokeLater(new Runnable(){
            public void run() {
                JOptionPane.showMessageDialog (null, "Goodbye, World!"); // in alert box
                JFrame frame = new JFrame("Goodbye, World!");            // on title bar
                JTextArea text = new JTextArea("Goodbye, World!");       // in editable area
                JButton button = new JButton("Goodbye, World!");         // on button

                frame.setLayout(new FlowLayout());
                frame.add(button);
                frame.add(text);
                frame.pack();
                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                frame.setVisible(true);
            }
        });
    }
}
```


Using Java 8 lambdas syntax:


```java
import javax.swing.*;
import java.awt.*;

public class HelloWorld {
    public static void main(String[] args) {

        SwingUtilities.invokeLater(() -> {
            JOptionPane.showMessageDialog(null, "Goodbye, world!");
            JFrame frame = new JFrame("Goodbye, world!");
            JTextArea text = new JTextArea("Goodbye, world!");
            JButton button = new JButton("Goodbye, world!");

            frame.setLayout(new FlowLayout());
            frame.add(button);
            frame.add(text);
            frame.pack();
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setVisible(true);
        });
    }
}
```



## JavaScript


```javascript
 alert("Goodbye, World!");
```



## jq

In the following, which generates SVG in a way that can be readily viewed using a web browser, the "Goodbye, World!" text is shaded using a linear gradient.

The approach used here to generate SVG is based on these principles:
* a JSON object is used to specify CSS styles
** this makes it easy to combine default specifications with partial specifications, because in jq, for JSON objects, "+" is defined so that (default + partial) is the combination which gives precedence to the right-hand-side operand;
* for other defaults, the jq "//" operator can be used; thus all SVG parameters can be easily given defaults.

'''Part 1: Generic SVG-related functions'''

```jq
# Convert a JSON object to a string suitable for use as a CSS style value
# e.g: "font-size: 40px; text-align: center;" (without the quotation marks)
def to_s:
  reduce to_entries[] as $pair (""; . + "\($pair.key): \($pair.value); ");

# Defaults: 100%, 100%
def svg(width; height):
  "<svg width='\(width // "100%")' height='\(height // "100%")'
           xmlns='http://www.w3.org/2000/svg'>";

# Defaults:
#  id: "linearGradient"
#  color1: rgb(0,0,0)
#  color2: rgb(255,255,255)
def linearGradient(id; color1; color2):
  "<defs>
    <linearGradient id='\(id//"linearGradient")' x1='0%' y1='0%' x2='100%' y2='0%'>
      <stop offset='0%' style='stop-color:\(color1//"rgb(0,0,0)");stop-opacity:1' />
      <stop offset='100%' style='stop-color:\(color2//"rgb(255,255,255)");stop-opacity:1' />
    </linearGradient>
  </defs>";

# input: the text string
# "style" should be a JSON object (see for example the default ($dstyle));
# the style actually used is (default + style), i.e. whatever is specified in "style" wins.
# Defaults:
#  x: 0
#  y: 0
def text(x; y; style):
  . as $in
  | {"font-size": "40px", "text-align": "center", "text-anchor": "left", "fill": "black"} as $dstyle
  | (($dstyle + style) | to_s) as $style
  | "<text x='\(x//0)' y='\(y//0)' style='\($style)'>
       \(.)",
     "</text>";
```

'''Part 2: "Goodbye, World!"'''

```jq
def task:
  svg(null;null),                                                   # use the defaults
  linearGradient("gradient"; "rgb(255,255,0)"; "rgb(255,0,0)"),     # define "gradient"
  ("Goodbye, World!" | text(10; 50; {"fill": "url(#gradient)"})),   # notice how the default for "fill" is overridden
  "</svg>";

task
```

 jq -n -r -f Hello_word_Graphical.jq > Hello_word_Graphical.svg


## Jsish

Using JSI CData processing, C, and linking to libAgar.
```txt
prompt$ jsish
Jsish interactive: see 'help [cmd]'.  \ cancels > input.  ctrl-c aborts running script.
# require('JsiAgarGUI')
1
# JsiAgarGUI.alert("Goodbye, World!");
#
```

Window pops up with message and Ok button.

That is based on JSI CData, a blend of typed Javascript and C, interwoven via a preprocessor.


```c
extension JsiAgarGUI = { // libAgar GUI from Jsi
    /*
      Alert popup, via libAgar and Jsish CData
      tectonics:
        jsish -c JsiAgar.jsc
        gcc `jsish -c -cflags true JsiAgar.so` `agar-config --cflags --libs`
        jsish -e 'require("JsiAgar"); JsiAgar.alert("Goodbye, World!");'
    */
    #include <agar/core.h>
    #include <agar/gui.h>

    /* Terminate on close */
    void windDown(AG_Event *event) {
        AG_Terminate(0);
    }

    function alert(msg:string):void { // Display a JsiAgar windowed message
        /* Native C code block (in a JSI function wrapper) */
        AG_Window *win;
        AG_Box *box;

        Jsi_RC rc = JSI_OK;

        if (AG_InitCore(NULL, 0) == -1 || AG_InitGraphics(0) == -1) return (JSI_ERROR);
        AG_BindStdGlobalKeys();

        win = AG_WindowNew(0);

        box = AG_BoxNew(win, AG_BOX_VERT, 0);
        AG_LabelNewS(box, AG_LABEL_HFILL, msg);
        AG_ButtonNewFn(box, AG_BUTTON_HFILL, "Ok", AGWINDETACH(win), "%p", win);

        AG_SetEvent(win, "window-detached", windDown, NULL);
        AG_WindowShow(win);

        AG_EventLoop();

        AG_DestroyGraphics();
        AG_Destroy();

        return rc;
    }
};
```


Build rules are ''jsish -c'' preprocessor, query ''jsish'' for C compile time flags, compile the C, load the module into jsish via ''require''.


```txt
prompt$ make -B -f Makefile.jsc hello
jsish -c JsiAgarGUI.jsc
gcc `jsish -c -cflags true JsiAgarGUI.so` `agar-config --cflags --libs`
jsish -e 'require("JsiAgarGUI"); JsiAgarGUI.alert(" Goodbye, World! ");'
```


And a window pops up with the message and an Ok button.

First command ''jsish -c'' runs a JSI to C preprocessor, generating a ''.h'' C source file.

For the second step, gcc is called with the output of a ''jsish -c -cflags true'' query, libagar runtime is linked in with more substitution for Agar compiler commands. The query output will be something like (this is site local, details will change per machine setup):


```txt
prompt$ jsish -c -cflags true JsiAgarGUI.so
-g -Og -O0 -Wall -fPIC -DJSI__SQLITE=1 -DJSI__READLINE=1 -fno-diagnostics-show-caret -Wc++-compat
 -Wwrite-strings -DCDATA_MAIN=1 -x c -rdynamic -I/home/btiffin/forge/jsi/jsish/src -DJSI__WEBSOCKET=1
 -I/home/btiffin/forge/jsi/jsish/websocket/src/lib  -I/home/btiffin/forge/jsi/jsish/websocket/src/build
 -I/home/btiffin/forge/jsi/jsish/websocket/unix -I/home/btiffin/forge/jsi/jsish/websocket/build/unix
 -o JsiAgarGUI.so JsiAgarGUI.h -lm -shared -DCDATA_SHARED=1 -L /home/btiffin/forge/jsi/jsish/websocket/build/unix/
 -lwebsockets -I/home/btiffin/forge/jsi/jsish/sqlite/src -L /home/btiffin/forge/jsi/jsish/sqlite/build/unix/
 -lsqlite3 -lm -ldl -lpthread

prompt$ agar-config --cflags --libs
-I/usr/local/include/agar -I/usr/include/SDL -D_GNU_SOURCE=1 -D_REENTRANT -I/usr/include/freetype2
 -I/usr/include/libpng16 -I/usr/include/freetype2 -I/usr/include/libpng16 -I/usr/include/libpng16
 -L/usr/local/lib -lag_gui -lag_core -lSDL -lpthread -lfreetype -lfontconfig -lfreetype
 -L/usr/local/lib -lGL -lX11 -lXinerama -lm -L/usr/lib -ljpeg -L/usr/lib64 -lpng16 -ldl
```


A JSI ready module is created, the C build rules managed by CData along with the ''.jsc'' JSI to C to JSI code generation.

As listed at the top, this GUI can be called up while in the interactive console.


## Just Basic


```Just Basic

print "Goodbye, World!"
'Prints in the upper left corner of the default text window: mainwin, a window with scroll bars.

```



## Julia

```julia
using Tk

window = Toplevel("Hello World", 200, 100, false)
pack_stop_propagate(window)

fr = Frame(window)
pack(fr, expand=true, fill="both")

txt = Label(fr, "Hello World")
pack(txt, expand=true)

set_visible(window, true)

# sleep(7)
```



## KonsolScript

Popping a dialog-box.

```KonsolScript
function main() {
  Konsol:Message("Goodbye, World!", "")
}
```


Displaying it in a Window.

```KonsolScript
function main() {
  Screen:PrintString("Goodbye, World!")
  while (B1 == false) {
    Screen:Render()
  }
}
```



## Kotlin

```scala
import java.awt.*
import javax.swing.*

fun main(args: Array<String>) {
    JOptionPane.showMessageDialog(null, "Goodbye, World!") // in alert box
    with(JFrame("Goodbye, World!")) {                      // on title bar
        layout = FlowLayout()
        add(JButton("Goodbye, World!"))                    // on button
        add(JTextArea("Goodbye, World!"))                  // in editable area
        pack()
        defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        isVisible = true
    }
}
```



## LabVIEW

## Lasso

On OS X machines:

```lasso
sys_process('/usr/bin/osascript', (: '-e', 'display dialog "Goodbye, World!"'))->wait
```



## Liberty BASIC


```lb
NOTICE "Goodbye, world!"
```



## Lingo


Display in alert box:

```lingo
_player.alert("Goodbye, World!")
```


Display in main window ("stage"):

```lingo
-- create a field
m = new(#field)
m.rect = rect(0,0,320,240)
m.alignment = "center"
m.fontsize = 24
m.fontStyle = "bold"
m.text = "Goodbye, World!"

-- create sprite, assign field
_movie.puppetSprite(1, TRUE)
sprite(1).member = m
sprite(1).loc = point(0,105)

-- force immediate update
_movie.updateStage()
```



## LiveCode


```LiveCode
answer "Goodbye, World!"
```

A dialog box can be modified as appropriate for the context by setting a "iconType", button text and title

```LiveCode
answer warning "Goodbye, World!" with "Goodbye, World!" titled "Goodbye, World!"
```



## Lobster


```Lobster
gl_window("graphical hello world", 800, 600)
gl_setfontname("data/fonts/Droid_Sans/DroidSans.ttf")
gl_setfontsize(30)

while gl_frame():
    gl_clear([ 0.0, 0.0, 0.0, 1.0 ])
    gl_text("Goodbye, World!")
```



## Logo

Among the turtle commands are some commands for drawing text in the graphical area. Details and capabilities differ among Logo implementations.

```logo
LABEL [Hello, World!]
SETLABELHEIGHT 2 * last LABELSIZE
LABEL [Goodbye, World!]
```



## Lua

==={{libheader|IUPLua}}===

```lua
require "iuplua"

dlg = iup.dialog{iup.label{title="Goodbye, World!"}; title="test"}
dlg:show()

if (not iup.MainLoopLevel or iup.MainLoopLevel()==0) then
  iup.MainLoop()
end
```


==={{libheader|LÖVE}}===

To actually run this LÖVE-program, the following code
needs to be in a file '''main.lua''', in its own folder.

This folder usually also contains other resources for a game,
such as pictures, sound, music, other source-files, etc.

To run the program, on windows,
drag that folder onto either love.exe
or a shortcut to love.exe.


```lua

function love.draw()
    love.graphics.print("Goodbye, World!", 400, 300)
end

```



## M2000 Interpreter

A window with a click event to open a message box, and print returned number to window form, scrolling at the lower part of form's layer.

```M2000 Interpreter

Module CheckIt {
      Declare Simple Form
      \\ we can define form before open
      Layer Simple {
            \\ center Window with 12pt font, 12000 twips width and 6000 twips height
            \\ ; at the end command to center the form in current screen
            Window 12, 12000, 6000;
            \\ make layer gray and split screen 0
            Cls #333333, 0
            \\   set split screen to 3rd line, like Cls ,2 without clear screen
            Scroll Split 2
            Cursor 0, 2
      }
      With Simple, "Title", "Hello Form"
      Function Simple.Click {
            Layer Simple {
                  \\ open msgbox
                  Print Ask("Hello World")
                  Refresh
            }
      }
      \\ now open as modal
      Method Simple, "Show", 1
      \\ now form deleted
      Declare Simple Nothing
}
CheckIt

```


A simple Window only

```M2000 Interpreter

Module CheckIt {
      Declare Simple Form
      With Simple, "Title", "Hello World"
      Method Simple, "Show", 1
      Declare Simple Nothing
}
CheckIt

```






## Maple


```Maple

Maplets:-Display( Maplets:-Elements:-Maplet( [ "Goodbye, World!" ] ) );

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
CreateDialog["Hello world"]
```



## MATLAB


```matlab
msgbox('Goodbye, World!')
```


Add text to a graphical plot.

```Matlab
 text(0.2,0.2,'Hello World!')
```



## MAXScript



```maxscript
messageBox "Goodbye world"
```



## mIRC Scripting Language


```mirc
alias goodbyegui {
  dialog -m Goodbye Goodbye
}

dialog Goodbye {
  title "Goodbye, World!"
  size -1 -1 80 20
  option dbu
  text "Goodbye, World!", 1, 20 6 41 7
}
```


=={{header|Modula-3}}==
```modula3
MODULE GUIHello EXPORTS Main;

IMPORT TextVBT, Trestle;

<*FATAL ANY*>

VAR v := TextVBT.New("Goodbye, World!");

BEGIN
  Trestle.Install(v);
  Trestle.AwaitDelete(v);
END GUIHello.
```

This code requires an m3makefile.

```txt

import ("ui")
implementation ("GUIHello")
program ("Hello")

```

This tells the compiler to link with the UI library, the file name of the implementation code, and to output a program named "Hello".


## N/t/roff

Whether the output is graphical or text depends largely the compiler with which the /.ROFF/ source code below is compiled.
If it is compiled with an Nroff compiler, its output is comparable to that of a typewriter.  Therefore, output from Nroff is typically seen on a text terminal.  If it is compiled with a Troff compiler, its output is comparable to that of a typesetter.  Therefore, output from Troff is typically seen on a PostScript or PDF output using a document viewer.  Furthermore, output from Troff is also usually seen on paper, so that may count as graphical as well.
In conclusion, although the code is compatible with both Nroff and Troff, it should be compiled using Troff to guarantee graphical output.

Because /.ROFF/ is a document formatting language in and of itself, it is extremely likely that a user of /.ROFF/ will be typing mostly textual content in a natural language.  Therefore, there are no special routines or procedures to be called to output normal text, as all text will get formatted onto paper automatically.

```N/t/roff
Goodbye, World!
```



## Neko

The NekoVM uses a C FFI that requires marshaling of C types to Neko ''value'' types.
```c
/*
  Tectonics:
    gcc -shared -fPIC -o nekoagar.ndll nekoagar.c `agar-config --cflags --libs`
*/

/* Neko primitives for libAgar http://www.libagar.org */
#include <stdio.h>
#include <neko.h>
#include <agar/core.h>
#include <agar/gui.h>

#define val_widget(v)       ((AG_Widget *)val_data(v))
DEFINE_KIND(k_agar_widget);

/* Initialize Agar Core given appname and flags */
value agar_init_core(value appname, value flags) {
#ifdef DEBUG
  if (!val_is_null(appname))  val_check(appname, string);
  val_check(flags, int);
#endif
  if (AG_InitCore(val_string(appname), val_int(flags)) == -1)  return alloc_bool(0);
  return alloc_bool(1);
}
DEFINE_PRIM(agar_init_core, 2);

/* Initialize Agar GUI given graphic engine driver */
value agar_init_gui(value driver) {
#ifdef DEBUG
  if (!val_is_null(driver))  val_check(driver, string);
#endif
  if (AG_InitGraphics(val_string(driver)) == -1)  return alloc_bool(0);
  AG_BindStdGlobalKeys();
  return alloc_bool(1);
}
DEFINE_PRIM(agar_init_gui, 1);

/* Initialize Agar given appname, flags and GUI driver */
value agar_init(value appname, value flags, value driver) {
#ifdef DEBUG
  if (!val_is_null(appname))  val_check(appname, string);
  val_check(flags, int);
  if (!val_is_null(driver))  val_check(driver, string);
#endif
  if (!val_bool(agar_init_core(appname, flags)))  return alloc_bool(0);
  if (!val_bool(agar_init_gui(driver)))  return alloc_bool(0);
  return alloc_bool(1);
}
DEFINE_PRIM(agar_init, 3);


/* end the Agar event loop on window-close */
void rundown(AG_Event *event) {
  AG_Terminate(0);
}


/* Create an Agar window, given UInt flags (which might use 32 bits...) */
value agar_window(value flags) {
#ifdef DEBUG
  val_check(flags, int);
#endif
  AG_Window *win;
  win = AG_WindowNew(val_int(flags));
  AG_SetEvent(win, "window-close", rundown, "%p", win);

  if ( win == NULL) return alloc_bool(0);
  return alloc_abstract(k_agar_widget, win);
}
DEFINE_PRIM(agar_window, 1);


/* Show a window */
value agar_window_show(value w) {
  AG_Window *win;

#ifdef DEBUG
  val_check_kind(w, k_agar_widget);
#endif
  win = (AG_Window *)val_widget(w);
  AG_WindowShow(win);
  return alloc_bool(1);
}
DEFINE_PRIM(agar_window_show, 1);


/* New box */
value agar_box(value parent, value type, value flags) {
  AG_Box *b;

#ifdef DEBUG
  val_check_kind(parent, k_agar_widget);
#endif
  b = AG_BoxNew(val_widget(parent), val_int(type), val_int(flags));
  return alloc_abstract(k_agar_widget, b);
}
DEFINE_PRIM(agar_box, 3);

/* New label */
value agar_label(value parent, value flags, value text) {
  AG_Label *lw;

#ifdef DEBUG
  val_check_kind(parent, k_agar_widget);
#endif
  lw = AG_LabelNewS(val_widget(parent), val_int(flags), val_string(text));
  return alloc_abstract(k_agar_widget, lw);
}
DEFINE_PRIM(agar_label, 3);


/* Event Loop */
value agar_eventloop(void) {
  int rc;
  rc = AG_EventLoop();
  return alloc_int(rc);
}
DEFINE_PRIM(agar_eventloop, 0);
```


The C file above is used to create a Neko friendly Dynamic Shared Object file, nekoagar.ndll.
The DSO functions are then loaded and exposed to Neko.

The Neko program follows:


```ActionScript
/**
 <doc>
```txt

 Hello world, graphical, in Neko, via Agar label
 Tectonics:
   gcc -shared -fPIC -o nekoagar.ndll rosetta-nekoagar.c `agar-config --cflags --libs`
   nekoc hello-graphical.neko
   neko hello-graphical

```
</doc>
*/

/* Load some libagar bindings  http://www.libagar.org/mdoc.cgi?man=AG_Intro.3 */
var agar_init = $loader.loadprim("nekoagar@agar_init", 3);
var agar_window = $loader.loadprim("nekoagar@agar_window", 1);
var agar_window_show = $loader.loadprim("nekoagar@agar_window_show", 1);
var agar_box = $loader.loadprim("nekoagar@agar_box", 3);
var agar_label = $loader.loadprim("nekoagar@agar_label", 3);
var agar_eventloop = $loader.loadprim("nekoagar@agar_eventloop", 0);

/* Init with driver; NULL for best choice on current system */
try {
  var rc = agar_init("nekoagar", 0, val_null);
  if $not(rc) $throw("Error: agar_init non zero");
} catch e {
  $throw("Error: agar_init exception");
}

/* Put up a window, with a box, and a label in the box */
var w = agar_window(0);
var box = agar_box(w, 1, 0);
var label = agar_label(box, 0, "Goodbye, World!");
agar_window_show(w);

/* Run the event loop */
agar_eventloop();
```


```txt
prompt$ gcc -shared -DDEBUG -fPIC -o nekoagar.ndll rosetta-nekoagar.c `agar-config --cflags --libs`
prompt$ nekoc hello-graphical.neko
prompt$ neko hello-graphical
```


''Rosetta Code no longer supports uploading images, sorry.''


## Nemerle

Compile with:
```txt
ncc -reference:System.Windows.Forms goodbye.n
```


```Nemerle
using System;
using System.Windows.Forms;

MessageBox.Show("Goodbye, World!")
```



## NetRexx

Using [[Java|Java's]] [http://download.oracle.com/javase/6/docs/technotes/guides/swing/index.html Swing Foundation Classes].
```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import javax.swing.

msgText = 'Goodbye, World!'
JOptionPane.showMessageDialog(null, msgText)

```


An alternative version using other Swing classes.
```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import javax.swing.

msgText = 'Goodbye, World!'

window = JFrame(msgText)
text = JTextArea()
minSize = Dimension(200, 100)

text.setText(msgText)

window.setLayout(FlowLayout())
window.add(text)
window.setMinimumSize(minSize)
window.pack
window.setVisible(isTrue)
window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

return

method isTrue() public static returns boolean
  return 1 == 1

method isFalse() public static returns boolean
  return \isTrue

```


An example using [[Java|Java's]] [http://download.oracle.com/javase/6/docs/technotes/guides/awt/index.html Abstract Window Toolkit (AWT)]
```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

class RCHelloWorld_GraphicalAWT_01 extends Dialog implements ActionListener

  properties private constant
    msgText = 'Goodbye, World!'

  properties indirect
    ok = boolean
    can = boolean
    okButton = Button
    canButton = Button
    buttonPanel = Panel

method RCHelloWorld_GraphicalAWT_01(frame = Frame, msg = String, canaction = boolean) public
  super(frame, 'Default', isTrue)
  setLayout(BorderLayout())
  add(BorderLayout.CENTER, Label(msg))
  addOKCancelPanel(canaction)
  createFrame()
  pack()
  setVisible(isTrue)

  return

method RCHelloWorld_GraphicalAWT_01(frame = Frame, msg = String) public
  this(frame, msg, isFalse)
  return

method addOKCancelPanel(canaction = boolean)
  setButtonPanel(Panel())
  getButtonPanel.setLayout(FlowLayout())
  createOKButton()
  if canaction then do
    createCancelButton()
    end
  add(BorderLayout.SOUTH, getButtonPanel)
  return

method createOKButton()
  setOkButton(Button('OK'))
  getButtonPanel.add(getOkButton)
  getOkButton.addActionListener(this)
  return

method createCancelButton()
  setCanButton(Button('Cancel'))
  getButtonPanel.add(getCanButton)
  getCanButton.addActionListener(this)
  return

method createFrame()
  dim = getToolkit().getScreenSize
  setLocation(int(dim.width / 3), int(dim.height / 3))
  return

method actionPerformed(ae = ActionEvent) public
  if ae.getSource == getOkButton then do
    setOk(isTrue)
    setCan(isFalse)
    setVisible(isFalse)
    end
  else if ae.getSource == getCanButton then do
    setCan(isTrue)
    setOk(isFalse)
    setVisible(isFalse)
    end
  return

method main(args = String[]) public constant
  mainFrame = Frame()
  mainFrame.setSize(200, 200)
  mainFrame.setVisible(isTrue)
  message = RCHelloWorld_GraphicalAWT_01(mainFrame, msgText, isTrue)
  if message.isOk then
    say 'OK pressed'
  if message.isCan then
    say 'Cancel pressed'
  message.dispose
  mainFrame.dispose
  return

method isTrue() public static returns boolean
  return 1 == 1

method isFalse() public static returns boolean
  return \isTrue

```



## newLISP


NewLISP uses a lightweight Java GUI server that it communicates with over a pipe, similar how some languages use Tcl/Tk. This takes advantage of Java's cross platform GUI capability.


```NewLISP
; hello-gui.lsp
; oofoe 2012-01-18

; Initialize GUI server.
(load (append (env "NEWLISPDIR") "/guiserver.lsp"))
(gs:init)

; Create window frame.
(gs:frame 'Goodbye 100 100 300 200 "Goodbye!")
(gs:set-resizable 'Goodbye nil)
(gs:set-flow-layout 'Goodbye "center")

; Add final message.
(gs:label 'Message "Goodbye, World!" "center")
(gs:add-to 'Goodbye 'Message)

; Show frame.
(gs:set-visible 'Goodbye true)

; Start event loop.
(gs:listen)

(exit) ; NewLisp normally goes to listener after running script.

```



```NewLISP
 ; Nehal-Singhal 2018-06-05

> (! "dialog --msgbox GoodbyeWorld! 5 20")
; A dialog message box appears on terminal similar to yes/no box.
```



## Nim

==={{libheader|GTK2}}===

```nim
import dialogs, gtk2
gtk2.nim_init()

info(nil, "Hello World")
```

==={{libheader|IUP}}===

```nim
import iup

discard iup.open(nil, nil)
message("Hello", "Hello World")
close()
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 LOCATE 6,11
20 PRINT "GOODBYE, WORLD!"
```



## Nyquist

===Audacity plug-in (Lisp syntax)===

```lisp
;nyquist plug-in
;version 4
;type tool
;name "Goodbye World"

(print "Goodbye, World!")
```


===Audacity plug-in (SAL syntax)===

```SAL
;nyquist plug-in
;version 4
;type tool
;codetype sal
;name "Goodbye World"

return "Goodbye, World!"
```


=={{header|Objective-C}}==
To show a modal alert (Mac):

```objc
NSAlert *alert = [[NSAlert alloc] init];
[alert setMessageText:@"Goodbye, World!"];
[alert runModal];
```


To show a modal alert (iOS):

```objc
UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Goodbye, World!" message:nil delegate:nil cancelButtonTitle:@"OK" otherButtonTitles:nil];
[alert show];
```



## Objeck

```objeck

use Qt;

bundle Default {
  class QtExample {
    function : Main(args : String[]) ~ Nil {
      app := QAppliction->New();
      win := QWidget->New();
      win->Resize(400, 300);
      win->SetWindowTitle("Goodbye, World!");
      win->Show();
      app->Exec();
      app->Delete();
    }
  }
}

```




## OCaml

```ocaml
let delete_event evt = false

let destroy () = GMain.Main.quit ()

let main () =
  let window = GWindow.window in
  let _ = window#set_title "Goodbye, World" in
  let _ = window#event#connect#delete ~callback:delete_event in
  let _ = window#connect#destroy ~callback:destroy in
  let _ = window#show () in
  GMain.Main.main ()
;;

let _ = main () ;;
```


* [[Xlib_simple_window#OCaml|Simple window with OCaml and Xlib]]

 ocaml -I +labltk labltk.cma

Just output as a label in a window:

```ocaml
let () =
  let main_widget = Tk.openTk () in
  let lbl = Label.create ~text:"Goodbye, World" main_widget in
  Tk.pack [lbl];
  Tk.mainLoop();;
```


Output as text on a button that exits the current application:

```ocaml
let () =
  let action () = exit 0 in
  let main_widget = Tk.openTk () in
  let bouton_press =
    Button.create main_widget ~text:"Goodbye, World" ~command:action in
  Tk.pack [bouton_press];
  Tk.mainLoop();;
```



## Ol

```ol

(import (lib winapi))
(MessageBox #f (c-string "Hello, World!") (c-string "Rosettacode") #x40)

```





## OxygenBasic

Windows MessageBox:

```txt


print "Hello World!"


```



## Oxygene


### Glade

[[File:OxygeneGladeHw.png|left|HelloWorld]]
<br clear=both>
Requires a Glade GUI description file. 'ere be one I produced earlier:

```xml

<?xml version="1.0" standalone="no"?> <!--*- mode: xml -*-->
<!DOCTYPE glade-interface SYSTEM "http://glade.gnome.org/glade-2.24.dtd">

<glade-interface>

<widget class="GtkWindow" id="hworld">
  <property name="visible">True</property>
  <property name="title">Hello World</property>
  <property name="modal">False</property>
  <property name="resizable">True</property>
  <property name="default_width">200</property>
  <property name="default_height">100</property>
  <signal name="delete_event" handler="on_hworld_delete_event"/>
  <child>
    <widget class="GtkLabel" id="label1">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label" translatable="yes">Farewell, cruel world.</property>
    </widget>
  </child>
</widget>

</glade-interface>

```

And finally the Oxygene:

```oxygene

// Display a Message in a GUI Window
//
// Nigel Galloway, April 18th., 2012.
//
namespace HelloWorldGUI;

interface

uses
  Glade, Gtk, System;

type
  Program = public static class
  public
    class method Main(args: array of String);
  end;

  MainForm = class(System.Object)
  private
    var
      [Widget] hworld: Gtk.Window;
  public
    constructor(args: array of String);
    method on_hworld_delete_event(aSender: Object; args: DeleteEventArgs);
  end;

implementation

class method Program.Main(args: array of String);
begin
  new MainForm(args);
end;

constructor MainForm(args: array of String);
begin
  inherited constructor;
  Application.Init();
  with myG := new Glade.XML(nil, 'HelloWorldGUI.Main.glade', 'hworld', nil) do myG.Autoconnect(self);
  Application.Run();
end;

method MainForm.on_hworld_delete_event(aSender: Object; args: DeleteEventArgs);
begin
  Application.Quit();
end;

end.

```


### .NET

[[File:OxygeneNEThw.jpg|left|HelloWorld]]
<br clear=both>

```oxygene

namespace HelloWorldNET;

interface

type
  App = class
  public
    class method Main;
  end;

implementation

class method App.Main;
begin
  System.Windows.MessageBox.Show("Farewell cruel world");
end;

end.

```



## Oz


```oz
declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}
  Window = {QTk.build td(label(text:"Goodbye, World!"))}
in
  {Window show}
```



## OpenEdge/Progress

<lang Progress (OpenEdge ABL)>MESSAGE "Goodbye, World!" VIEW-AS ALERT-BOX.
```


## PARI/GP


```parigp
plotinit(1, 1, 1, 1);
plotstring(1, "Goodbye, World!");
plotdraw([1, 0, 15]);
```



## Panoramic


```Panoramic
print "Goodbye, World!"
'Prints in the upper left corner of the window.

```



## Pascal

Variant of the C example:

```pascal
program HelloWorldGraphical;

uses
  glib2, gdk2, gtk2;

var
  window: PGtkWidget;

begin
  gtk_init(@argc, @argv);

  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title (GTK_WINDOW (window), 'Goodbye, World');
  g_signal_connect (G_OBJECT (window),
                    'delete-event',
        G_CALLBACK (@gtk_main_quit),
        NULL);
  gtk_widget_show_all (window);

  gtk_main();
end.
```



## Perl

==={{libheader|Perl/Tk}}===
Just output as a label in a window:


```perl

use strict;
use warnings;
use Tk;

my $main = MainWindow->new;
$main->Label(-text => 'Goodbye, World')->pack;
MainLoop();
```


Output as text on a button that exits the current application:


```perl
use strict;
use warnings;
use Tk;

my $main = MainWindow->new;
$main->Button(
  -text => 'Goodbye, World',
  -command => \&exit,
)->pack;
MainLoop();
```


==={{libheader|Perl/Gtk2}}===

```perl
use strict;
use warnings;
use Gtk2 '-init';

my $window = Gtk2::Window->new;
$window->set_title('Goodbye world');
$window->signal_connect(
  destroy => sub { Gtk2->main_quit; }
);

my $label = Gtk2::Label->new('Goodbye, world');
$window->add($label);

$window->show_all;
Gtk2->main;
```


==={{libheader|Perl/Qt}}===

```perl
use strict;
use warnings;
use QtGui4;

my $app = Qt::Application(\@ARGV);
my $label = Qt::Label('Goodbye, World');
$label->show;
exit $app->exec;
```



## Perl 6

```perl6
use GTK::Simple;
use GTK::Simple::App;

my GTK::Simple::App $app .= new;
$app.border-width = 20;
$app.set-content( GTK::Simple::Label.new(text => "Goodbye, World!") );
$app.run;
```



## Phix

```Phix
include pGUI.e
IupOpen()
IupMessage("Bye","Goodbye, World!")
IupClose()
```



## PHP

```php
if (!class_exists('gtk'))
{
    die("Please load the php-gtk2 module in your php.ini\r\n");
}

$wnd = new GtkWindow();
$wnd->set_title('Goodbye world');
$wnd->connect_simple('destroy', array('gtk', 'main_quit'));

$lblHello = new GtkLabel("Goodbye, World!");
$wnd->add($lblHello);

$wnd->show_all();
Gtk::main();
```



## PicoLisp


```PicoLisp
(call 'dialog "--msgbox" "Goodbye, World!" 5 20)
```



## PostScript

In the general Postscript context, the <tt>show</tt> command will render the string that is topmost on the stack at the <tt>currentpoint</tt> in the previously <tt>setfont</tt>. Thus a minimal PostScript file that will print on a PostScript printer or previewer might look like this:


```postscript
%!PS
% render in Helvetica, 12pt:
/Helvetica findfont 12 scalefont setfont
% somewhere in the lower left-hand corner:
50 dup moveto
% render text
(Goodbye, World!) show
% wrap up page display:
showpage
```



## PowerBASIC

```powerbasic
FUNCTION PBMAIN() AS LONG
    MSGBOX "Goodbye, World!"
END FUNCTION
```



## PowerShell

{{libheader|WPK}}<br/>
```powershell
New-Label "Goodbye, World!" -FontSize 24 -Show
```

```powershell
$form = New-Object System.Windows.Forms.Form
$label = New-Object System.Windows.Forms.Label

$label.Text = "Goodbye, World!"
$form.AutoSize = $true
$form.AutoSizeMode = [System.Windows.Forms.AutoSizeMode]::GrowAndShrink
$form.Controls.Add($label)

$Form.ShowDialog() | Out-Null
```

Alternatively, simply as a message box:

```powershell
[System.Windows.Forms.MessageBox]::Show("Goodbye, World!")
```



## Processing

Uses default Processing methods and variables.

```Processing
  text("Goodbye, World!",0,height/2);
```



## Prolog

Works with SWI-Prolog and XPCE.


A simple message box :

```Prolog
send(@display, inform, 'Goodbye, World !').
```

A more sophisticated window :

```Prolog
goodbye :-
    new(D, window('Goodbye')),
    send(D, size, size(250, 100)),
    new(S, string("Goodbye, World !")),
    new(T, text(S)),
    get(@display, label_font, F),
    get(F, width(S), M),
    XT is (250 - M)/2,
    get(F, height, H),
    YT = (100-H)/2,
    send(D, display, T, point(XT, YT)),
    send(D, open).

```



## PureBasic


```PureBasic
MessageRequester("Hello","Goodbye, World!")
```

Using the Windows API:

```PureBasic
MessageBox_(#Null,"Goodbye, World!","Hello")
```



## Pure Data


```txt

#N canvas 321 432 450 300 10;
#X obj 100 52 loadbang;
#X msg 100 74 Goodbye\, World!;
#X obj 100 96 print -n;
#X connect 0 0 1 0;
#X connect 1 0 2 0;

```

* While there is no easy (intuitive) way to print a comma (or semicolon) this pd script will do.
* When writing messages to the terminal window, Pd prepends the name of the print object and a colon, or "print: " if no name is specified, which can be avoided by using "-n" instead of a name. This behaviour, however, has not been adopted by Pd-extended :-(


## Python

==={{libheader|Tkinter}}===

```python
import tkMessageBox

result = tkMessageBox.showinfo("Some Window Label", "Goodbye, World!")
```


'''Note:''' The result is a string of the button that was pressed.

```python
from tkinter import messagebox

result = messagebox.showinfo("Some Window Label", "Goodbye, World!")
```



==={{libheader|PyQt}}===

```python
import PyQt4.QtGui
app = PyQt4.QtGui.QApplication([])
pb = PyQt4.QtGui.QPushButton('Hello World')
pb.connect(pb,PyQt4.QtCore.SIGNAL("clicked()"),pb.close)
pb.show()
exit(app.exec_())
```


==={{libheader|PyGTK}}===

```python
import pygtk
pygtk.require('2.0')
import gtk

window = gtk.Window()
window.set_title('Goodbye, World')
window.connect('delete-event', gtk.main_quit)
window.show_all()
gtk.main()
```


==={{libheader|VPython}}===
```python

# HelloWorld for VPython - HaJo Gurt - 2014-09-20
from visual import *

scene.title = "VPython Demo"
scene.background = color.gray(0.2)

scene.width  = 600
scene.height = 400
scene.range  = 4
#scene.autocenter = True

S = sphere(pos=(0,0,0), radius=1, material=materials.earth)
rot=0.005

txPos=(0, 1.2, 0)

from visual.text import *
# Old 3D text machinery (pre-Visual 5.3): numbers and uppercase letters only:
T1 = text(pos=txPos, string='HELLO', color=color.red, depth=0.3, justify='center')

import vis
# new text object, can render text from any font (default: "sans") :
T2 = vis.text(pos=txPos, text="Goodbye", color=color.green, depth=-0.3, align='center')
T2.visible=False

Lbl_w = label(pos=(0,0,0), text='World', color=color.cyan,
              xoffset=80, yoffset=-40)     # in screen-pixels

L1 = label(pos=(0,-1.5,0), text='Drag with right mousebutton to rotate view',   box=0)
L2 = label(pos=(0,-1.9,0), text='Drag up+down with middle mousebutton to zoom', box=0)
L3 = label(pos=(0,-2.3,0), text='Left-click to change', color=color.orange,     box=0)

print "Hello World"     # Console


cCount = 0
def change():
    global rot, cCount
    cCount=cCount+1
    print "change:", cCount
    rot=-rot
    if T1.visible:
        T1.visible=False
        T2.visible=True
    else:
        T1.visible=True
        T2.visible=False

scene.bind( 'click', change )

while True:
  rate(100)
  S.rotate( angle=rot, axis=(0,1,0) )


```


==={{libheader|WxPython}}===

```python
import wx

app = wx.App(False)
frame = wx.Frame(None, wx.ID_ANY, "Hello, World")
frame.Show(True)
app.MainLoop()
```



==={{libheader|Kivy}}===

```python

from kivy.app import App
from kivy.uix.floatlayout import FloatLayout
from kivy.uix.button import Button
from kivy.uix.popup import Popup
from kivy.uix.label import Label


class GoodByeApp(App):
    def build(self, *args, **kwargs):
        layout = FloatLayout()
        ppp = Popup(title='Goodbye, World!',
                    size_hint=(0.75, 0.75), opacity=0.8,
                    content=Label(font_size='50sp', text='Goodbye, World!'))
        btn = Button(text='Goodbye', size_hint=(0.3, 0.3),
                     pos_hint={'center': (0.5, 0.5)}, on_press=ppp.open)
        layout.add_widget(btn)
        return layout


GoodByeApp().run()

```



==={{libheader|Kivy}}===
With kv language

```python


from kivy.app import App
from kivy.lang.builder import Builder

kv = '''
#:import Factory kivy.factory.Factory

FloatLayout:
    Button:
        text: 'Goodbye'
        size_hint: (0.3, 0.3)
        pos_hint: {'center': (0.5, 0.5)}
        on_press: Factory.ThePopUp().open()

<ThePopUp@Popup>:
    title: 'Goodbye, World!'
    size_hint: (0.75, 0.75)
    opacity: 0.8
    Label:
        text: 'Goodbye, World!'
        font_size: '50sp'
'''


class GoodByeApp(App):
    def build(self, *args, **kwargs):
        return Builder.load_string(kv)


GoodByeApp().run()

```



## RapidQ


```rapidq
MessageBox("Goodbye, World!", "RapidQ example", 0)
```



## R

Rather minimalist, but working...

```R
library(RGtk2)   # bindings to Gtk
w <- gtkWindowNew()
l <- gtkLabelNew("Goodbye, World!")
w$add(l)
```



## Racket



```Racket
 #lang racket/gui
(require racket/gui/base)

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Goodbye, World!"]))

; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))

; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             (callback (lambda (button event)
                         (send msg set-label "Button click"))))

; Show the frame by calling its show method
(send frame show #t)
```



## Rascal


```rascal

import vis::Figure;
import vis::Render;

public void GoodbyeWorld() =
  render(box(text("Goodbye World")));

```

Output:
[[File:Goodbyeworld.png]]


## REALbasic


```vb

MsgBox("Goodbye, World!")

```



## REBOL


```REBOL
alert "Goodbye, World!"
```



## Red


```Red>>
 view [ text "Hello World !"]
```



## REXX


### version 1

This REXX example only works with:
:::*   PC/REXX
:::*   Personal REXX

```rexx
/*REXX (using PC/REXX)  to display a message in a window (which is bordered). */
if fcnPkg('rxWindow') ¬== 1  then do
                                  say 'RXWINDOW function package not loaded.'
                                  exit 13
                                  end
if pcVideo()==3  then normal= 7
                 else normal=13

window#=w_open(1, 1, 3, 80, normal)
call w_border  window#
call w_put     window#, 2, 2, center("Goodbye, World!", 80-2)

                                       /*stick a fork in it, all we're done. */
```

'''output'''

```txt

╔══════════════════════════════════════════════════════════════════════════════╗
║                               Goodbye, World!                                ║
╚══════════════════════════════════════════════════════════════════════════════╝

```



### version 2

This REXX example only works with:
:::*   PC/REXX
:::*   Personal REXX


and it creates two windows, the first (main) window contains the   '''Goodbye, World!'''   text,

the other "help" window contains a message about how to close the windows.

```rexx
/*REXX program shows a "hello world" window (and another to show how to close)*/
parse upper version !ver .;     !pcrexx= !ver=='REXX/PERSONAL' | !ver=='REXX/PC'
if ¬!pcrexx  then call ser  "This isn't PC/REXX"     /*this isn't  PC/REXX ?  */
rxWin=fcnPkg('rxwindow')                             /*is the function around?*/

if rxWin¬==1  then do 1;     'RXWINDOW  /q'
                   if fcnPkg('rxwindow')==1 then leave   /*the function is OK.*/
                   say 'error loading RXWINDOW !';     exit 13
                   end

top=1;         normal=31;       border=30;   curpos=cursor()
width=40;      height=11;       line.=;      line.1= 'Goodbye, World!'
w=w_open(2, 3, height+2, width, normal);     call w_border  w,,,,,border
helpLine= 'press the  ESC  key to quit.'
helpW=w_open(2, 50, 3, length(helpLine)+4, normal)
call w_border helpw,,,,,border;  call w_put helpW, 2, 3, helpLine
call w_hide w, 'n'
                             do k=0  to height-1
                             _=top+k;      call w_put w, k+2, 3, line._, width-4
                             end   /*k*/
call w_unhide w
                             do forever;   if inKey()=='1b'x  then leave;  end
                                                   /*   ↑                     */
call w_close  w                                    /*   └──◄ the  ESCape  key.*/
call w_close  helpw
if rxWin¬==1  then 'RXUNLOAD rxwindow'
parse var curPos row  col
call      cursor row, col
                                       /*stick a fork in it,  we're all done. */
```



## Ring


```ring

Load "guilib.ring"
New qApp {
        new qWidget() {
                setwindowtitle("Hello World")
                show()
        }
        exec()
}

```



## Robotic

Since visuals are already built in, [[Hello world/Newbie#Robotic|this link]] does the same thing.


## Ruby

```ruby
require 'gtk2'

window = Gtk::Window.new
window.title = 'Goodbye, World'
window.signal_connect(:delete-event) { Gtk.main_quit }
window.show_all

Gtk.main
```


```ruby
require 'tk'
root = TkRoot.new("title" => "User Output")
TkLabel.new(root, "text"=>"CHUNKY BACON!").pack("side"=>'top')
Tk.mainloop
```


```ruby
#_Note: this code MUST be executed through the Shoes GUI!!

Shoes.app do
  para "CHUNKY BACON!", :size => 72
end
```


```ruby

require 'gosu'

class Window < Gosu::Window

  def initialize
    super(150, 50, false)
    @font = Gosu::Font.new(self, "Arial", 32)
  end

  def draw
    @font.draw("Hello world", 0, 10, 1, 1, 1)
  end

end

Window.new.show
```


```ruby

#_Note: this code must not be executed through a GUI
require 'green_shoes'

Shoes.app do
  para "Hello world"
end

```


```ruby

require 'win32ole'
WIN32OLE.new('WScript.Shell').popup("Hello world")

```



## Run BASIC


```runbasic
' do it with javascript
html "<script>alert('Goodbye, World!');</script>"
```



## Rust

==={{libheader|GTK}}===

```rust
// cargo-deps:  gtk
extern crate gtk;
use gtk::traits::*;
use gtk::{Window, WindowType, WindowPosition};
use gtk::signal::Inhibit;

fn main() {
    gtk::init().unwrap();
    let window = Window::new(WindowType::Toplevel).unwrap();

    window.set_title("Goodbye, World!");
    window.set_border_width(10);
    window.set_window_position(WindowPosition::Center);
    window.set_default_size(350, 70);
    window.connect_delete_event(|_,_| {
        gtk::main_quit();
        Inhibit(false)
    });

    window.show_all();
    gtk::main();
}
```



## Scala

### Ad hoc REPL solution

Ad hoc solution as [http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop REPL] script:

```scala
swing.Dialog.showMessage(message = "Goodbye, World!")
```


### JVM Application

Longer example, as an application:

```scala
import swing._

object GoodbyeWorld extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Goodbye, World!"
    contents = new FlowPanel {
      contents += new Button  ("Goodbye, World!")
      contents += new TextArea("Goodbye, World!")
    }
  }
}
```


### .Net Framework


```Scala
import swing._

object HelloDotNetWorld {
  def main(args: Array[String]) {
    System.Windows.Forms.MessageBox.Show
                      ("Goodbye, World!")
  }
}
```



## Scilab


```scilab
messagebox("Goodbye, World!")
```



## Scheme


```ruby

#!r6rs

;; PS-TK example: display frame + label

(import (rnrs)
        (lib pstk main) ; change this to refer to your PS/Tk installation
        )

(define tk (tk-start))
(tk/wm 'title tk "PS-Tk Example: Label")

(let ((label (tk 'create-widget 'label 'text: "Goodbye, world")))
  (tk/place label 'height: 20 'width: 50 'x: 10 'y: 20))

(tk-event-loop tk)

```



## Scratch


[[File:Scratch_Hello_World_Graphical.png]]


## Seed7


Seed7 does not work with an event handling function like gtk_main().
The progam stays in control and does not depend on callbacks.
The graphic library manages redraw, keyboard and mouse events.
The contents of a window are automatically restored when it is
uncovered. It is possible to copy areas from a window even when
the area is currently covered or off screen.


```seed7
$ include "seed7_05.s7i";
  include "draw.s7i";
  include "keybd.s7i";
  include "bitmapfont.s7i";
  include "stdfont24.s7i";
  include "pixmap_file.s7i";

const proc: main is func
  local
    var text: screen is STD_NULL;
  begin
    screen(400, 100);
    clear(curr_win, white);
    KEYBOARD := GRAPH_KEYBOARD;
    screen := openPixmapFontFile(curr_win);
    color(screen, black, white);
    setFont(screen, stdFont24);
    setPosXY(screen, 68,  60);
    write(screen, "Goodbye, World");
    ignore(getc(KEYBOARD));
  end func;
```



## Sidef

```ruby
var tk = require('Tk');
var main = %s'MainWindow'.new;
main.Button(
    '-text'    => 'Goodbye, World!',
    '-command' => 'exit',
).pack;
tk.MainLoop;
```


```ruby
var gtk2 = require('Gtk2') -> init;

var window = %s'Gtk2::Window'.new;
var label  = %s'Gtk2::Label'.new('Goodbye, World!');

window.set_title('Goodbye, World!');
window.signal_connect(destroy => func(*_){ gtk2.main_quit });

window.add(label);
window.show_all;

gtk2.main;
```



## Smalltalk

```smalltalk
MessageBox show: 'Goodbye, world.'
```

```smalltalk
Dialog information: 'Goodbye, world.'
```



## SmileBASIC


```smilebasic
DIALOG "Goodbye, world."
```



## SSEM

Ok, I know this is cheating. But it isn't <i>completely</i> cheating: the SSEM uses Williams tube storage, so the memory is basically a CRT device; and this is an executable program, up to a point, because the first line includes a <tt>111 Stop</tt> instruction (disguised as a little flourish joining the tops of the <b>d</b> and the <b>b</b>).

```ssem
01100000000001110000000000000000
10000000000001010000000000000000
10011101110111011101010111000000
10010101010101010101010101000000
10010101010101010101010111000000
10011101110111011100110100000010
10000000000000000000010011000010
10011000000000000000100000000100
01101000000000000001000000000000
00000000000000000000000000000000
00000000000000000000000000000000
00100100100000000010000100100000
00100100100000000010000100100000
00100100101110111010011100100000
00100100101010100010010100100000
00100100101010100010010100000000
00100100101110100011011100100000
00011011000000000000000000000000
```

Once you've keyed it in, the first eighteen words of storage will look a bit like this:

```txt
 oo          ooo
o            o o
o  ooo ooo ooo ooo o o ooo
o  o o o o o o o o o o o o
o  o o o o o o o o o o ooo
o  ooo ooo ooo ooo  oo o      o
o                    o  oo    o
o  oo               o        o
 oo o              o


  o  o  o         o    o  o
  o  o  o         o    o  o
  o  o  o ooo ooo o  ooo  o
  o  o  o o o o   o  o o  o
  o  o  o o o o   o  o o
  o  o  o ooo o   oo ooo  o
   oo oo
```



## Stata


```stata
window stopbox note "Goodbye, World!"
```


## Supernova


```Supernova
I want window and the window title is "Goodbye, World".
```



## Swift

```Swift
import Cocoa

let alert = NSAlert()
alert.messageText = "Goodbye, World!"
alert.runModal()
```



## Tcl

Just output as a label in a window:

```tcl
pack [label .l -text "Goodbye, World"]
```


Output as text on a button that exits the current application:

```tcl
pack [button .b -text "Goodbye, World" -command exit]
```

''Note:'' If you name this program "button.tcl", you might get strange errors.

Don't use the name of any internal tcl/tk-command as a filename for a tcl-script.

This shows our text in a message box:

```tcl
tk_messageBox -message "Goodbye, World"
```


=={{header|TI-83 BASIC}}==

```ti83b
PROGRAM:GUIHELLO
:Text(0,0,"GOODBYE, WORLD!")
```


=={{header|TI-89 BASIC}}==


```ti89b
Dialog
  Text "Goodbye, World!"
EndDlog
```



## TXR



### = Microsoft Windows =



```txrlisp
(with-dyn-lib "user32.dll"
  (deffi messagebox "MessageBoxW" int (cptr wstr wstr uint)))

(messagebox cptr-null "Hello" "World" 0) ;; 0 is MB_OK
```



## VBScript



```vbscript

MsgBox("Goodbye, World!")

```



## Vedit macro language

Displaying the message on status line. The message remains visible until the next keystroke, but macro execution continues.

```vedit
Statline_Message("Goodbye, World!")
```


Displaying a dialog box with the message and default OK button:

```vedit
Dialog_Input_1(1,"`Vedit example`,`Goodbye, World!`")
```



## UNIX Shell


### In a virtual terminal

Using whiptail or dialog

```bash

whiptail --title 'Farewell' --msgbox 'Goodbye, World!' 7 20

```


```bash

dialog --title 'Farewell' --msgbox 'Goodbye, World!' 7 20

```


### In a graphical environment

Using the simple dialog command xmessage, which uses the X11 Athena Widget library

```bash

xmessage 'Goodbye, World!'

```

Using the zenity modal dialogue command (wraps GTK library) available with many distributions of [[Linux]]

```bash

zenity --info --text='Goodbye, World!'

```

Using yad (a fork of zenity with many more advanced options)

```bash

yad --title='Farewell' --text='Goodbye, World!'

```



## Vala


```vala
#!/usr/local/bin/vala --pkg gtk+-3.0
using Gtk;

void main(string[] args) {
    Gtk.init(ref args);

    var window = new Window();
    window.title = "Goodbye, world!";
    window.border_width = 10;
    window.window_position = WindowPosition.CENTER;
    window.set_default_size(350, 70);
    window.destroy.connect(Gtk.main_quit);

    var label = new Label("Goodbye, world!");

    window.add(label);
    window.show_all();

    Gtk.main();
}
```



## VBA

```vb
Public Sub hello_world_gui()
    MsgBox "Goodbye, World!"
End Sub
```


## Visual Basic


```vb
Sub Main()
    MsgBox "Goodbye, World!"
End Sub
```



## Visual Basic .NET

```vbnet
Imports System.Windows.Forms

Module GoodbyeWorld
    Sub Main()
        Messagebox.Show("Goodbye, World!")
    End Sub
End Module
```



## Visual FoxPro


```vfp
* Version 1:
MESSAGEBOX("Goodbye, World!")

* Version 2:
? "Goodbye, World!"
```



## Wee Basic


```Wee Basic
print 1 at 10,12 "Hello world!"
end
```



## X86 Assembly

This example used the Windows MessageBox function to do the work for us.
Windows uses the stdcall calling convention where the caller pushes
function parameters onto the stack and the stack has been fixed up
when the callee returns.

```assembly
;;; hellowin.asm
;;;
;;; nasm -fwin32 hellowin.asm
;;; link -subsystem:console -out:hellowin.exe -nodefaultlib -entry:main \
;;;    hellowin.obj user32.lib kernel32.lib

        global _main
        extern _MessageBoxA@16
        extern _ExitProcess@4

        MessageBox equ _MessageBoxA@16
        ExitProcess equ _ExitProcess@4

        section .text
_main:
        push 0                  ; MB_OK
        push title              ;
        push message            ;
        push 0                  ;
        call MessageBox         ; eax = MessageBox(0,message,title,MB_OK);
        push eax                ;
        call ExitProcess        ; ExitProcess(eax);
message:
        db 'Goodbye, World',0
title:
        db 'RosettaCode sample',0

```

```assembly

;use win32ax for 32 bit
;use win64ax for 64 bit
include 'win64ax.inc'

.code
   start:
      invoke MessageBox,HWND_DESKTOP,"Goodbye,World!","Goodbye",MB_OK
      invoke ExitProcess,0
.end start

```



## Web 68


```web68
@1Introduction.
Define the structure of the program.

@aPROGRAM goodbye world CONTEXT VOID USE standard
BEGIN
@<Included declarations@>
@<Logic at the top level@>
END
FINISH

@ Include the graphical header file.

@iforms.w@>

@ Program code.

@<Logic...@>=
open(LOC FILE,"",arg channel);
fl initialize(argc,argv,NIL,0);
fl show messages("Goodbye World!");
fl finish

@ Declare the necessary macros.

@<Include...@>=
macro fl initialize;
macro fl show messages;
macro fl finish;

@ The end.
```



## XSLT


The output is an SVG document. The idea is that it's straightforward to use XSLT to turn an existing SVG into an instantiable template.


```xml
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<xsl:output method="xml"/>
	<xsl:template match="/*">
		<!--
			Use a template to insert some text into a simple SVG graphic
			with hideous colors.
		-->
		<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 200">
			<rect x="0" y="0" width="400" height="200" fill="cyan"/>
			<circle cx="200" cy="100" r="50" fill="yellow"/>
			<text x="200" y="115"
				style="font-size: 40px;
					text-align: center;
					text-anchor: middle;
					fill: black;">
				<!-- The text inside the element -->
				<xsl:value-of select="."/>
			</text>
		</svg>
	</xsl:template>
</xsl:stylesheet>
```


Sample input:


```xml><message
Goodbye, World!</message>
```


Sample output (with formatting non-destructively adjusted):


```xml
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 200">
	<rect x="0" y="0" width="400" height="200" fill="cyan"/>
	<circle cx="200" cy="100" r="50" fill="yellow"/>
	<text x="200" y="115" style="font-size: 40px;
				text-align: center;
				text-anchor: middle;
				fill: black;">Goodbye, World!</text>
</svg>
```



## Yabasic


```Yabasic
open window 200, 100
text 10, 20, "Hello world"
color 255, 0, 0 : text 10, 40, "Good bye world", "roman14"
```



## zkl

zkl doesn't have a decent GUI ffi but, on my Linux box, the following work:

```zkl
System.cmd(0'|zenity --info --text="Goodbye, World!"|); // GTK+ pop up
System.cmd(0'|notify-send "Goodbye, World!"|); // desktop notification
System.cmd(0'|xmessage -buttons Ok:0,"Not sure":1,Cancel:2 -default Ok -nearmouse "Goodbye, World!" -timeout 10|); // X Windows dialog
```

The quote quote syntax is 0'<char>text<char> or you can use \ (eg "\"Goodbye, World!\"")

