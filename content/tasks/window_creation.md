+++
title = "Window creation"
description = ""
date = 2019-10-12T12:34:15Z
aliases = []
[extra]
id = 1555
[taxonomies]
categories = ["task", "GUI"]
tags = []
+++

## Task

Display a [[GUI]] window. The window need not have any contents, but should respond to requests to be closed.


## Ada

```ada
with Gtk.Window;  use Gtk.Window;
with Gtk.Widget;  use Gtk.Widget;

with Gtk.Handlers;
with Gtk.Main;

procedure Windowed_Application is
   Window : Gtk_Window;

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
   Show (Window);

   Gtk.Main.Main;
end Windowed_Application;
```



## ALGOL 68

'''Compile command:''' ca -l gtk-3 -l gdk-3 -l atk-1.0 -l gio-2.0 -l pangocairo-1.0 -l gdk_pixbuf-2.0 -l cairo-gobject -l pango-1.0 -l cairo -l gobject-2.0 -l glib-2.0 firstgtk.a68

```algol68
PROGRAM firstgtk CONTEXT VOID
USE standard
BEGIN
MODE GDATA = REF BITS;
MODE GUINT = BITS;
MODE GSIZE = BITS;
MODE GTYPE = GSIZE;
MODE GTYPECLASS = STRUCT(GTYPE g type);
MODE GTYPEINSTANCE = STRUCT(REF GTYPECLASS g class);
MODE GTKWIDGETPRIVATE = REF BITS;

MODE GOBJECT = STRUCT(
                GTYPEINSTANCE g type instance,
                GUINT ref count,
                REF GDATA qdata);

MODE GTKWIDGET = STRUCT(
                GOBJECT parent instance,
      REF GTKWIDGETPRIVATE priv);

PROC(REF INT,REF CCHARPTRPTR)VOID gtk init = ALIEN "GTK_INIT"
        "#define GTK_INIT(argc,argv) gtk_init(argc,argv)";
PROC(INT)REF GTKWIDGET gtk window new = ALIEN "GTK_WINDOW_NEW"
        "#define GTK_WINDOW_NEW(type) (void *)gtk_window_new(type)";
PROC(REF GTKWIDGET)VOID gtk widget show = ALIEN "GTK_WIDGET_SHOW"
        "#define GTK_WIDGET_SHOW(widget) gtk_widget_show((void *)widget)";
PROC gtk main = VOID: CODE "gtk_main();";

INT gtk window toplevel = 0;
FILE argf;
REF GTKWIDGET window;

open(argf,"",arg channel);
gtk init(argc,argv);
window:=gtk window new(gtk window toplevel);
gtk widget show(window);
gtk main
END
FINISH
```


## Arturo

```arturo
use ~gui

window @mainWindow #{
	:title 	"Hello World"
	:size 	#(300 300)
}

app @main "TestWindow" mainWindow #{}
main.run
```



## AutoHotkey



```AutoHotkey
Gui, Add, Text,, Hello
Gui, Show
```



## AutoIt


```AutoIt
GUICreate("Test")
GUISetState(@SW_SHOW)

Do
	Switch GUIGetMsg()
		Case -3 ; $GUI_EVENT_CLOSE
			Exit
	EndSwitch
Until False
```



## AurelBasic


```AurelBasic
WIN 0 0 400 300 "New Window"
```



## BaCon

BaCon includes a Highlevel Universal GUI abstraction layer (HUG).  The implementation is based on GTK.


```freebasic
REM empty window
INCLUDE "hug.bac"

mainwin = WINDOW("Rosetta Code empty", 400, 300)

REM start gtk event loop...
DISPLAY
```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"WINLIB2"
      dlg% = FN_newdialog("GUI Window", 0, 0, 200, 150, 8, 1000)
      PROC_showdialog(dlg%)
```



## C


### SDL

'''Compile Command:''' gcc `sdl-config --cflags` `sdl-config --libs` SDL_Window.c -o window

```c
/*
 *   Opens an 800x600 16bit color window.
 *   Done here with ANSI C.
 */

#include <stdio.h>
#include <stdlib.h>
#include "SDL.h"

int main()
{
  SDL_Surface *screen;

  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    fprintf(stderr, "Unable to initialize SDL: %s\n", SDL_GetError());
    return 1;
  }
  atexit(SDL_Quit);
  screen = SDL_SetVideoMode( 800, 600, 16, SDL_SWSURFACE | SDL_HWPALETTE );

  return 0;
}
```



### GTK

'''Compile command:''' gcc `gtk-config --cflags` `gtk-config --libs` -o window window.c


```c
#include <gtk/gtk.h>


int
main(int argc, char *argv[])
{
  GtkWidget *window;

  gtk_init(&argc, &argv);
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_signal_connect(GTK_OBJECT(window), "destroy",
    GTK_SIGNAL_FUNC(gtk_main_quit), NULL);
  gtk_widget_show(window);
  gtk_main();

  return 0;
}
```



### GTK2

'''Compile command:''' gcc -Wall -pedantic `pkg-config --cflags gtk+-2.0` `pkg-config --libs gtk+-2.0` -o window window.c


```c
#include <gtk/gtk.h>

int
main(int argc, char *argv[])
{
  GtkWidget *window;

  gtk_init(&argc, &argv);
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  g_signal_connect (window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
  gtk_widget_show(window);
  gtk_main();

  return 0;
}
```



### GLUT

'''Compile command:''' gcc -I /usr/include/ -lglut  -o window window_glut.c

Note that we aren't registering a painting or drawing callback, so the window will be created with nothing drawn in it.  This is almost certain to lead to a strange appearance; On many systems, dragging the window around will appear to drag a copy of what was underneath where the window was when it was originally created.

We ''are'' registering a keypress callback, which isn't strictly necessary; It simply allows us to use a keypress to close the program rather than depending on the windowing system the program is run under.


```c
// A C+GLUT implementation of the Creating a Window task at Rosetta Code
// http://rosettacode.org/wiki/Creating_a_Window
#include <stdlib.h>
#include <GL/glut.h>

// This function is not strictly necessary to meet the requirements of the task.
void onKeyPress(unsigned char key, int x, int y)
{
	// If you have any cleanup or such, you need to use C's
	// onexit routine for registering cleanup callbacks.
	exit(0);

}

int main(int argc, char **argv)
{
	// Pulls out any command-line arguments that are specific to GLUT,
	// And leaves a command-line argument set without any of those arguments
	// when it returns.
	// (If you want a copy, take a copy first.)
	glutInit(&argc, argv);

	// Tell GLUT we want to create a window.
	// It won't *actually* be created until we call glutMainLoop below.
	glutCreateWindow("Goodbye, World!");

	// Register a callback to handle key press events (so we can quit on
	// when someone hits a key)  This part is not necessary to meet the
	// requirements of the task.
	glutKeyboardFunc(&onKeyPress);

	// Put the execution of the app in glut's hands. Most GUI environments
	// involve a message loop that communicate window events. GLUT handles
	// most of these with defaults, except for any we register callbacks
	// for. (Like the onKeyPress above.)
	glutMainLoop();

	return 0;

}


```



## C++

'''Compiler command:''' qmake -pro; qmake


```cpp
#include <QApplication>
#include <QMainWindow>

int main(int argc, char *argv[])
{
 QApplication app(argc, argv);
 QMainWindow window;
 window.show();
 return app.exec();
}
```


'''Compiler command:''' g++ filename.cc -o test `pkg-config --cflags --libs gtkmm-2.4`


```cpp
#include <iostream>
#include <gtkmm.h>

int
main( int argc, char* argv[] )
{
 try
 {
  Gtk::Main m( argc, argv ) ;
  Gtk::Window win ;
  m.run( win ) ;
 }

 catch( std::exception const & exc )
 {
  std::cout << exc.what() << std::endl ;
  exit( -1 ) ;
 }

 exit( 0 ) ;
}
```


## C#

```c#
using System;
using System.Windows.Forms;

public class Window {
    [STAThread]
    static void Main() {
        Form form = new Form();

        form.Text = "Window";
        form.Disposed += delegate { Application.Exit(); };

        form.Show();
        Application.Run();
    }
}
```



## Clojure

```clojure
(import '(javax.swing JFrame))

(let [frame (JFrame. "A Window")]
	   (doto frame
		 (.setSize 600 800)
		 (.setVisible true)))
```



## Common Lisp


==={{libheader|CAPI}}===

```lisp
(capi:display (make-instance 'capi:interface :title "A Window"))
```


==={{libheader|CLIM}}===

Setting up the environment:


```lisp
(require :mcclim)
(cl:defpackage #:rc-window
  (:use #:clim-lisp #:clim))
(cl:in-package #:rc-window)
```


The actual definition and display:


```lisp
(define-application-frame rc-window ()
  ()
  (:layouts (:default)))

(run-frame-top-level (make-application-frame 'rc-window))
```


Note: This creates a small, useless window ("frame"). Useful frames will have some ''panes'' defined inside them.

==={{libheader|Swing}}===
Works with the Armed Bear Common Lisp implementation that targets the JVM.


```lisp
(defun create-window ()
  "Creates a window"
  (let ((window (jnew (jconstructor "javax.swing.JFrame"))))
	(jcall (jmethod "javax.swing.JFrame" "setVisible" "boolean")
		   window (make-immediate-object t :boolean))))

(create-window)
```



## D

```d
 module Window;

 import fltk4d.all;

 void main() {
     auto window = new Window(300, 300, "A window");
     window.show;
     FLTK.run;
 }
```


```d
 import derelict.sdl.sdl;

 int main(char[][] args)
 {
     DerelictSDL.load();

     SDL_Event event;
     auto done = false;

     SDL_Init(SDL_INIT_VIDEO);
     scope(exit) SDL_Quit();

     SDL_SetVideoMode(1024, 768, 0, SDL_OPENGL);
     SDL_WM_SetCaption("My first Window", "SDL test");

     while (!done)
     {
         if (SDL_PollEvent(&event) == 1)
         {
             switch (event.type)
 	     {
                 case SDL_QUIT:
 	              done = true;
 		          break;
 		 default:
 		      break;
 	     }
 	 }
     }

    return 0;
 }
```


QD is a simple and easy-to-use wrapper around SDL.

```d
 import qd;

 void main() {
   screen(640, 480);
   while (true) events();
 }
```




## Delphi


This first example is a minimalist approach using Delphi's standard Window (form) creation procedure.  In Delphi 7, this will create a single Window executable of 362KB.


```Delphi


// The project file (Project1.dpr)
program Project1;

uses
  Forms,
  // Include file with Window class declaration (see below)
  Unit0 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


// The Window class declaration
unit Unit1;

interface

uses
  Forms;

type
  TForm1 = class(TForm)
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm} // The window definition resource (see below)

end.

// A textual rendition of the Window (form) definition file (Unit1.dfm)
object Form1: TForm1
  Left = 469
  Top = 142
  Width = 800
  Height = 600
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
end


```


This second example demonstrates a 'pure' Windows API approach (i.e. NOT using the Delphi Visual Component Library). In Delphi 7, this will create a single Window executable of 15KB.


```Delphi

program Project3;

uses
  Windows,
  Messages;

var
  WndClass: TWndClass;
  Msg: TMsg;
  winT, winL: Integer;

// Initial height/width of the window
const
  winW: Integer = 800;
  winH: Integer = 600;

// Callback function to processes messages sent to the window
function WindowProc(hWnd,Msg,wParam,lParam:Integer): Integer; stdcall;
begin
  // Trap the WM_DESTROY message
  if (Msg = WM_DESTROY) then PostQuitMessage(0);
  Result := DefWindowProc(hWnd,Msg,wParam,lParam);
end;

begin
  // Fill the WndClass structure with the window class attributes
  // to be registered by the RegisterClass function
  with WndClass do
    begin
      lpszClassName:= 'Form1';
      lpfnWndProc :=  @WindowProc; // Pointer to our message handling callback
      style := CS_OWNDC or         // Request a unique device context
               CS_VREDRAW or       // Redraw window when resized vertically
               CS_HREDRAW;         // Redraw window when resized horizontally
      hInstance := hInstance;      // The instance that the window procedure of this class is within
      hbrBackground := HBRUSH(COLOR_BTNFACE+1); // Background colour of the window
    end;

  // Register the window class for use by CreateWindow
  RegisterClass(WndClass);

  // Calculate initial top and left positions of the window
  winT := (GetSystemMetrics(SM_CYFULLSCREEN) - winH) div 2;
  winL := (GetSystemMetrics(SM_CXFULLSCREEN) - winW) div 2;

  // Create the window
  CreateWindow(WndClass.lpszClassName,              // Class name
               'Form1',                             // Window name
               WS_OVERLAPPEDWINDOW or WS_VISIBLE,   // Window style
               winL,                                // Horizontal Position (Left)
               winT,                                // Vertical Position (Top)
               winW,                                // Width
               winH,                                // Height
               0,                                   // Window parent/owner handle
               0,                                   // Menu handle
               hInstance,                           // Handle to application instance
               nil);                                // Pointer to window creation data

 // Handle messages
 while GetMessage(Msg,0,0,0) do
   DispatchMessage(Msg);

end.

```



## Dragon

```dragon
select "GUI"

window = newWindow("Window")
window.setSize(400,600)
window.setVisible()


```



## E


Swing

 when (currentVat.morphInto("awt")) -> {
     def w := <swing:makeJFrame>("Window")
     w.setContentPane(<swing:makeJLabel>("Contents"))
     w.pack()
     w.show()
 }


## Eiffel


Platform independent EiffelVision 2 Library


```eiffel
class
    APPLICATION
inherit
    EV_APPLICATION
create
    make_and_launch
feature {NONE} -- Initialization
    make_and_launch
            -- Initialize and launch application
        do
            default_create
            create first_window
            first_window.show
            launch
        end
feature {NONE} -- Implementation
    first_window: MAIN_WINDOW
            -- Main window.
end
```



```eiffel
class
    MAIN_WINDOW
inherit
    EV_TITLED_WINDOW
        redefine
            initialize
        end
create
    default_create
feature {NONE} -- Initialization
    initialize
            -- Build the interface for this window.
        do
                -- Call initialize in parent class EV_TITLED_WINDOW
            Precursor {EV_TITLED_WINDOW}
                -- Build a container for widgets for this window
            build_main_container
                -- Add the container to this window
            extend (main_container)
                -- Add `request_close_window' to the actions taken when the user clicks
                -- on the cross in the title bar.
            close_request_actions.extend (agent request_close_window)
                -- Set the title of the window
            set_title ("Rosetta Code")
                -- Set the initial size of the window
            set_size (400, 400)
        end
feature {NONE} -- Implementation, Close event
    request_close_window
            -- The user wants to close the window
        do
                -- Destroy this window
            destroy;
                -- Destroy application
            (create {EV_ENVIRONMENT}).application.destroy
        end
feature {NONE} -- Implementation
    main_container: EV_VERTICAL_BOX
            -- Main container contains all widgets displayed in this window.
            -- In this case a single text area.
    build_main_container
            -- Create and populate `main_container'.
        require
            main_container_not_yet_created: main_container = Void
        do
            create main_container
            main_container.extend (create {EV_TEXT})
        ensure
            main_container_created: main_container /= Void
        end
end
```



```eiffel
class
    APPLICATION
inherit
    WINFORMS_FORM
        rename
            make as make_form
        end
create
    make
feature {NONE} -- Initialization
    make
            -- Run application.
        do
                -- Set window title
            set_text ("Rosetta Code")
                -- Launch application
            {WINFORMS_APPLICATION}.run_form (Current)
        end
end
```



## Emacs Lisp


```lisp

(make-frame)

```



## Euphoria


### ARWEN

```euphoria
include arwen.ew

constant win = create(Window, "ARWEN window", 0, 0,100,100,640,480,{0,0})

WinMain(win, SW_NORMAL)

```



### EuGTK

```euphoria
include GtkEngine.e

constant win = create(GtkWindow)
	connect(win,"destroy",quit)
	set(win,"title","Simple Window")
	set(win,"default size",300,100)
	set(win,"position",GTK_WIN_POS_CENTER)

show_all(win)
main()
```



### EuWinGUI

```euphoria
include EuWinGUI.ew

Window("EuWinGUI window",100,100,640,480)

-- Event loop
while True do
    WaitEvent()
end while

CloseApp(0)
```



### Win32Lib

```euphoria
include Win32Lib.ew

constant win = createEx( Window, "Win32Lib", 0, Default, Default, 640, 480, 0, 0 )

WinMain( win, Normal )
```



### wxEuphoria

```euphoria
include wxeu/wxeud.e

constant win = create( wxFrame, {0, -1, "wxEuphoria window", -1, -1, 640, 480} )

wxMain( win )
```



## Factor


```factor
USING: ui ui.gadgets.labels ;

"This is a window..." <label> "Really?" open-window
```



## Fantom


```fantom

using fwt

class Main
{
  public static Void main ()
  {
    Window().open
  }
}

```



## Forth

'''gtk-server command:''' gtk-server -fifo=ffl-fifo &


```forth
include ffl/gsv.fs

\ Open the connection to the gtk-server and load the Gtk2 definitions
s" gtk-server.cfg" s" ffl-fifo" gsv+open 0= [IF]

\ Convert the string event to a widget id
: event>widget
  0. 2swap >number 2drop d>s
;

0 value window

: window-creation
  gtk_init

  \ Create the window
  GTK_WINDOW_TOPLEVEL gtk_window_new to window

  window gtk_widget_show

  \ Wait for an event
  BEGIN
    s" WAIT" gtk_server_callback
    event>widget window =
  UNTIL

  0 gtk_exit
;

window-creation

gsv+close drop
[THEN]
```



## FreeBASIC


```FreeBasic

#Include "windows.bi"

Dim As HWND Window_Main
Dim As MSG msg

'Create the window:
Window_Main = CreateWindow("#32770", "I am a window - close me!", WS_OVERLAPPEDWINDOW Or WS_VISIBLE, 100, 100, 350, 200, 0, 0, 0, 0)

'Windows message loop:
While GetMessage(@msg, Window_Main, 0, 0)
  TranslateMessage(@msg)
  DispatchMessage(@msg)
  If msg.hwnd = Window_Main And msg.message = WM_COMMAND Then End
Wend

End

```



## Frink


```frink

g=(new graphics).show[]

```


=={{header|F_Sharp|F#}}==
Everything is provided by the .NET runtime so this is almost identical to [[C_sharp]].

```fsharp
 open System.Windows.Forms

 [<System.STAThread>]
 do
     Form(Text = "F# Window")
     |> Application.Run
```



## Gambas


```gambas
Public Sub Form_Open()

End
```



## Go


### GTK

```go
package main

import (
    "github.com/mattn/go-gtk/glib"
    "github.com/mattn/go-gtk/gtk"
)

func main() {
    gtk.Init(nil)
    window := gtk.NewWindow(gtk.WINDOW_TOPLEVEL)
    window.Connect("destroy",
        func(*glib.CallbackContext) { gtk.MainQuit() }, "")
    window.Show()
    gtk.Main()
}
```



### SDL

```go
package main

import (
    "log"

    "github.com/veandco/go-sdl2/sdl"
)

func main() {
    window, err := sdl.CreateWindow("RC Window Creation",
        sdl.WINDOWPOS_UNDEFINED, sdl.WINDOWPOS_UNDEFINED,
        320, 200, 0)
    if err != nil {
        log.Fatal(err)
    }
    for {
        if _, ok := sdl.WaitEvent().(*sdl.QuitEvent); ok {
            break
        }
    }
    window.Destroy()
}
```



### X11


```go
package main

import (
    "code.google.com/p/x-go-binding/ui"
    "code.google.com/p/x-go-binding/ui/x11"
    "log"
)

func main() {
    win, err := x11.NewWindow()
    if err != nil {
        log.Fatalf("Error: %v\n", err)
    }
    defer win.Close()

    for ev := range win.EventChan() {
        switch e := ev.(type) {
        case ui.ErrEvent:
            log.Fatalf("Error: %v\n", e.Err)
        }
    }
}
```



## Groovy


```txt

 import groovy.swing.SwingBuilder

 new SwingBuilder().frame(title:'My Window', size:[200,100]).show()

```



## GUISS


We will open notepad as a window here.


```guiss
Start,Programs,Accessories,Notepad
```


To close the window:


```guiss
Window:Notepad,Button:Close
```



## Haskell

Using {{libheader|HGL}} from [http://hackage.haskell.org/packages/hackage.html HackageDB].

A simple graphics library, designed to give the programmer access to most interesting parts of the Win32 Graphics Device Interface and X11 library without exposing the programmer to the pain and anguish usually associated with using these interfaces.

```haskell
import Graphics.HGL

aWindow =  runGraphics $
  withWindow_ "Rosetta Code task: Creating a window" (300, 200) $ \ w -> do
	drawInWindow w $ text (100, 100) "Hello World"
	getKey w
```



## HicEst


```hicest
WINDOW(WINdowhandle=handle, Width=80, Height=-400, X=1, Y=1/2, TItle="Rosetta Window_creation Example")
! window units: as pixels < 0,  as relative window size 0...1,  ascurrent character sizes > 1

WRITE(WINdowhandle=handle) '... some output ...'
```



## IDL

With some example values filled in:

 Id = WIDGET_BASE(TITLE='Window Title',xsize=200,ysize=100)
 WIDGET_CONTROL, /REALIZE, id

== Icon and Unicon ==
Icon and Unicon windowing is portable between Windows and X-Windows environments.
=
## Icon
=

```Icon
link graphics

procedure main(arglist)

WOpen("size=300, 300", "fg=blue", "bg=light gray")
WDone()
end
```


[http://www.cs.arizona.edu/icon/library/src/gprocs/graphics.icn graphics is required ]

=
## Unicon
=
The Icon solution works in Unicon.  An Unicon-only version is as follows:


```unicon

import gui
$include "guih.icn"

class WindowApp : Dialog ()

  # -- automatically called when the dialog is created
  method component_setup ()
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



## J

A minimalist modal dialog:

```j
   wdinfo 'Hamlet';'To be, or not to be: that is the question:'
```


A free-standing window:

```j
MINWDW=: noun define
pc minwdw;
pas 162 85;pcenter;
)

minwdw_run=: monad define
  wd MINWDW
  wd 'pshow;'
)

minwdw_close=: monad define
  wd'pclose'
)

minwdw_run ''
```



## Java

```java
import javax.swing.JFrame;

public class Main {
     public static void main(String[] args) throws Exception {
         JFrame w = new JFrame("Title");
         w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
         w.setSize(800,600);
         w.setVisible(true);
     }
}
```



## JavaScript

    window.open("webpage.html", "windowname", "width=800,height=600");


## Julia


```julia
# v0.6

using Tk

w = Toplevel("Example")
```



## Kotlin

```kotlin
import javax.swing.JFrame

fun main(args : Array<String>) {
    JFrame("Title").apply {
        setSize(800, 600)
        defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        isVisible = true
    }
}
```



## Lingo


```lingo
win = window().new("New Window")
w = 320
h = 240
firstScreen = _system.deskTopRectList[1]
x = firstScreen.width/2 - w/2
y = firstScreen.height/2- h/2
win.rect = rect(x,y,x+w,y+h)
-- Director needs a binary movie file (*.dir) for opening new windows. But this
-- movie file can be totally empty, and if it's write protected in the filesystem,
-- it can be re-used for multiple windows.
win.filename = _movie.path & "empty.dir"
win.open()
```



## Lua

```lua
local iup = require "iuplua"

iup.dialog{
  title = "Window";
  iup.vbox{
    margin = "10x10";
    iup.label{title = "A window"}
  }
}:show()

iup.MainLoop()

```



## M2000 Interpreter

Window has title by default the name of variable, here MyForm
Window by default has 6000 twips width by 4000 twips height

Here we open MyForm as modal window (with 1 as first parameter, after "Show")

Window show a square in left side of title (we can change this to be at right), so this close the window (if we have proper event function, we can quit the closing). Also using key combination Alt-F4 close window too (also send event).

Window open in the monitor where we see the mouse pointer.

M2000 Windows have own decoration, independent from platform (same to Xp, Windows 7, Windows 8, Windows 10 and Linux using Wine).



```M2000 Interpreter

Module DisplayWindow {
      Declare MyForm Form
      Method MyForm,"Show",1
}
DisplayWindow

```



## Mathematica


```Mathematica

CreateDocument[]

```



## Liberty BASIC

Minimum code required to fulfill the task.

```lb
nomainwin
open "GUI Window" for window as #1
wait

```

As it would properly be used in a real program.

```lb
nomainwin
open "GUI Window" for window as #1
#1 "trapclose Quit"
wait
sub Quit hndl$
    close #hndl$
    end
end sub

```



## mIRC Scripting Language

'''Switches:'''
C = Center Window
p = Picture Window
d = Desktop Window

 alias CreateMyWindow {
  .window -Cp +d @WindowName 600 480
 }


## NetRexx

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import javax.swing.JFrame
import javax.swing.JLabel
import java.awt.BorderLayout
import java.awt.Font
import java.awt.Color

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse arg showText .
select
  when showText.length = 0          then addText = isTrue
  when 'YES'.abbrev(showText.upper) then addText = isTrue
  when showText = '.'               then addText = isTrue
  otherwise                              addText = isFalse
  end
title = 'Rosetta Code - Window Creation'
createFrame(title, addText)

return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method createFrame(title, addText = boolean 0) public static
  do
    fenester = JFrame(title)
    fenester.setSize(600, 200)
    fenester.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    if addText then decorate(fenester)
    fenester.setVisible(isTrue)

  catch ex = Exception
    ex.printStackTrace()
  end

  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method decorate(fenester = JFrame, textStr = 'This page intentionally left blank.') private static returns JFrame
  textlbl  = JLabel(textStr)
  textfont = Font(Font.SERIF, Font.BOLD, 20)
  textlbl.setHorizontalAlignment(JLabel.CENTER)
  textlbl.setFont(textfont)
  textlbl.setForeground(Color.ORANGE)
  fenester.add(textlbl, BorderLayout.CENTER)

  return fenester

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method isTrue() public static returns boolean
  return (1 == 1)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method isFalse() public static returns boolean
  return \(1 == 1)

```



## Nim


###  gtk2

This is example 9 from the Araq/Nim github repository (modified to include a quit button)

```nim
import
  gdk2, glib2, gtk2

proc thisDestroy(widget: pWidget, data: pgpointer){.cdecl.} =
  main_quit()

const
  Inside: cstring = "Mouse is over label"
  OutSide: cstring = "Mouse is not over label"

var
  OverButton: bool

nim_init()
var window = window_new(gtk2.WINDOW_TOPLEVEL)
var stackbox = vbox_new(TRUE, 10)
var button1 = button_new("Move mouse over button")
var buttonstyle = copy(get_style(Button1))
ButtonStyle.bg[STATE_PRELIGHT].pixel = 0
ButtonStyle.bg[STATE_PRELIGHT].red = -1'i16
ButtonStyle.bg[STATE_PRELIGHT].blue = 0'i16
ButtonStyle.bg[STATE_PRELIGHT].green = 0'i16
set_style(button1, buttonstyle)
var button2 = button_new()
var ALabel = label_new(Outside)
var button3 = button_new("Quit")


proc ChangeLabel(P: PWidget, Event: gdk2.PEventCrossing,
                 Data: var bool){.cdecl.} =
  if Not Data: set_text(ALabel, Inside)
  else: set_text(ALabel, Outside)
  Data = Not Data


add(button2, ALAbel)
pack_start(stackbox, button1, TRUE, TRUE, 0)
pack_start(stackbox, button2, TRUE, TRUE, 0)
pack_start(stackbox, button3, TRUE, TRUE, 0)
set_border_width(Window, 5)
add(window, stackbox)
discard signal_connect(window, "destroy",
                   SIGNAL_FUNC(thisDestroy), nil)
overbutton = False
discard signal_connect(button1, "enter_notify_event",
                   SIGNAL_FUNC(ChangeLabel), addr(OverButton))
discard signal_connect(button1, "leave_notify_event",
                   SIGNAL_FUNC(ChangeLabel), addr(OverButton))
discard signal_connect(button3, "clicked",
                   SIGNAL_FUNC(thisDestroy), nil)
show_all(window)
main()
```



###  SDL


```nim
import
  sdl, sdl_image, colors

var
  screen, greeting: PSurface
  r: TRect
  event: TEvent
  bgColor = colChocolate.int32

if init(INIT_VIDEO) != 0:
  quit "SDL failed to initialize!"

screen = SetVideoMode(640, 480, 16, SWSURFACE or ANYFORMAT)
if screen.isNil:
  quit($sdl.getError())

greeting = IMG_load("tux.png")
if greeting.isNil:
  echo "Failed to load tux.png"
else:
  ## convert the image to alpha and free the old one
  var s = greeting.displayFormatAlpha()
  swap(greeting, s)
  s.freeSurface()

r.x = 0
r.y = 0

block game_loop:
  while true:

    while pollEvent(addr event) > 0:
      case event.kind
      of QUITEV:
        break game_loop
      of KEYDOWN:
        if EvKeyboard(addr event).keysym.sym == K_ESCAPE:
          break game_loop
      else:
        discard

    discard fillRect(screen, nil, bgColor)
    discard blitSurface(greeting, nil, screen, addr r)
    discard flip(screen)

greeting.freeSurface()
screen.freeSurface()
sdl.Quit()
```



###  X11


```nim
import xlib, xutil, x, keysym

const
  WINDOW_WIDTH = 400
  WINDOW_HEIGHT = 300

var
  width, height: cuint
  display: PDisplay
  screen: cint
  depth: int
  win: TWindow
  sizeHints: TXSizeHints

proc create_window =
  width = WINDOW_WIDTH
  height = WINDOW_HEIGHT

  display = XOpenDisplay(nil)
  if display == nil:
    echo("Verbindung zum X-Server fehlgeschlagen")
    quit(1)

  screen = XDefaultScreen(display)
  depth = XDefaultDepth(display, screen)
  var rootwin = XRootWindow(display, screen)
  win = XCreateSimpleWindow(display, rootwin, 100, 10,
                            width, height, 5,
                            XBlackPixel(display, screen),
                            XWhitePixel(display, screen))
  size_hints.flags = PSize or PMinSize or PMaxSize
  size_hints.min_width = width.cint
  size_hints.max_width = width.cint
  size_hints.min_height = height.cint
  size_hints.max_height = height.cint
  discard XSetStandardProperties(display, win, "Simple Window", "window",
                         0, nil, 0, addr(size_hints))
  discard XSelectInput(display, win, ButtonPressMask or KeyPressMask or
                                     PointerMotionMask)
  discard XMapWindow(display, win)

proc close_window =
  discard XDestroyWindow(display, win)
  discard XCloseDisplay(display)

var
  xev: TXEvent

proc process_event =
  var key: TKeySym
  case int(xev.theType)
  of KeyPress:
    key = XLookupKeysym(cast[ptr TXKeyEvent](addr(xev)), 0)
    if key.int != 0:
      echo("keyboard event",$key.int)
    if key.int == 65307:    # <Esc>
      quit(1)
  of ButtonPressMask, PointerMotionMask:
    Echo("Mouse event")
  else: nil

proc eventloop =
  discard XFlush(display)
  var num_events = int(XPending(display))
  while num_events != 0:
    dec(num_events)
    discard XNextEvent(display, addr(xev))
    process_event()

create_window()
while true:
  eventloop()
close_window()

```



###  glut


```nim
import glut

var win: int = 0

proc myOnKeyPress(c: int8, v1, v2: cint) {.cdecl.} =
   echo(c)
   if c == 27:
      glutDestroyWindow(win)

glutInit()
win = glutCreateWindow("Goodbye, World!")
glutKeyboardFunc(TGlut1Char2IntCallback(myOnKeyPress))
glutMainLoop()
```



###  win


```nim
# test a Windows GUI application

import
  windows, shellapi, nb30, mmsystem, shfolder

#proc MessageBox(hWnd: int, lpText, lpCaption: CString, uType: uint): int
# {stdcall, import: "MessageBox", header: "<windows.h>"}

discard MessageBox(0, "Hello World!", "Nim GUI Application", 0)
```



###  IUP


```nim
import iup

# assumes you have the iup  .dll or .so installed

discard iup.open(nil,nil)


# now use a Dialog box to show a message
var lbl = label("Hello World")
setAttribute(lbl,"PADDING","10x10")

var contents = hbox(lbl, nil)
#SetAttribute(contents, "MARGIN", "5x5")

var dlg = dialog(contents)
#SetAttribute(dlg, "SIZE", "100x50")

discard dlg.show()

# a window via a quick message box, sitting on top of the main dialog window
discard Alarm("MyTitle","Hello World","Ok", "Not Ok", nil)

discard mainloop()
iup.close()
```



## Objeck


```objeck

use Gtk2;

bundle Default {
  class GtkHello {
    function : Main(args : String[]) ~ Nil {
      window := GtkWindow->New();
      delete_callback := Events->DeleteEvent(GtkWidget) ~ Nil;
      window->SignalConnect(Signal->Destroy, window->As(GtkWidget), delete_callback);
      window->SetTitle("Title");
      window->Show();
      Appliction->Main();
    }
  }
}

```


=={{header|Objective-C}}==
It opens a 800&times;600 window, centered on the screen, with title "A Window".


```objc
#include <Foundation/Foundation.h>
#include <AppKit/AppKit.h>

@interface Win : NSWindow
{
}
- (void)applicationDidFinishLaunching: (NSNotification *)notification;
- (BOOL)applicationShouldTerminateAfterLastWindowClosed: (NSNotification *)notification;
@end


@implementation Win : NSWindow
-(instancetype) init
{
  if ((self = [super
    initWithContentRect: NSMakeRect(0, 0, 800, 600)
    styleMask: (NSTitledWindowMask | NSClosableWindowMask)
    backing: NSBackingStoreBuffered
    defer: NO])) {

    [self setTitle: @"A Window"];
    [self center];
  }
  return self;
}

- (void)applicationDidFinishLaunching: (NSNotification *)notification
{
  [self orderFront: self];
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed: (NSNotification *)notification
{
  return YES;
}
@end

int main()
{
  @autoreleasepool {

    [NSApplication sharedApplication];
    Win *mywin = [[Win alloc] init];
    [NSApp setDelegate: mywin];
    [NSApp runModalForWindow: mywin];

  }
  return EXIT_SUCCESS;
}
```



## OCaml


```ocaml
let () =
  let top = Tk.openTk() in
  Wm.title_set top "An Empty Window";
  Wm.geometry_set top "240x180";
  Tk.mainLoop ();
;;
```

execute with:
 ocaml -I +labltk labltk.cma sample.ml

with the [http://caml.inria.fr/pub/docs/manual-ocaml/manual039.html Graphics] module:

```ocaml
open Graphics

let () =
  open_graph " 800x600";
  let _ = read_line() in
  close_graph ()
```

execute with:
 ocaml graphics.cma tmp.ml

```ocaml
open GMain

let window = GWindow.window ~border_width:2 ()
let button = GButton.button ~label:"Hello World" ~packing:window#add ()

let () =
  window#event#connect#delete  ~callback:(fun _ -> true);
  window#connect#destroy ~callback:Main.quit;
  button#connect#clicked ~callback:window#destroy;
  window#show ();
  Main.main ()
```

execute with:
 ocaml -I +lablgtk2  lablgtk.cma gtkInit.cmo sample.ml

```ocaml
let () =
  Sdl.init [`VIDEO];
  let _ = Sdlvideo.set_video_mode 200 200 [] in
  Sdltimer.delay 2000;
  Sdl.quit ()
```

execute with:
 ocaml bigarray.cma -I +sdl sdl.cma sample.ml

```ocaml
open Sdl

let () =
  let width, height = (640, 480) in
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer
        ~width ~height ~flags:[]
  in
  let rgb = (0, 255, 0) in
  let a = 255 in
  Render.set_draw_color renderer rgb a;
  Render.clear renderer;
  Render.render_present renderer;
  Timer.delay 3000;
  Sdl.quit ()
```

execute with:
 ocaml -I +sdl2 sdl2.cma sample.ml

```ocaml
let () =
  let app = SFRenderWindow.make (640, 480) "OCaml-SFML Windowing" in

  let rec loop () =
    let continue =
      match SFRenderWindow.pollEvent app with
      | Some SFEvent.Closed -> false
      | _ -> true
    in
    SFRenderWindow.clear app SFColor.black;
    SFRenderWindow.display app;
    if continue then loop ()
  in
  loop ()
```

execute with:
 ocaml -I /tmp/ocaml-sfml/src sfml_system.cma sfml_window.cma sfml_graphics.cma win.ml

```ocaml
open Xlib

let () =
  let d = xOpenDisplay "" in
  let s = xDefaultScreen d in
  let w = xCreateSimpleWindow d (xRootWindow d s) 10 10 100 100 1
                                (xBlackPixel d s) (xWhitePixel d s) in
  xSelectInput d w [KeyPressMask];
  xMapWindow d w;
  let _ = xNextEventFun d in  (* waits any key-press event *)
  xCloseDisplay d;
;;
```

execute with:
 ocaml -I +Xlib Xlib.cma sample.ml


## OpenEdge ABL/Progress 4GL



```OpenEdgeABL


  DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "OpenEdge Window Display"
         HEIGHT             = 10.67
         WIDTH              = 95.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 95.4
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 95.4
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.

    VIEW C-Win.

    ON WINDOW-CLOSE OF C-Win /* <insert window title> */
    DO:
      /* This event will close the window and terminate the procedure.  */
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY.
    END.

    WAIT-FOR CLOSE OF THIS-PROCEDURE.


```



## Oz


```oz
functor
import
   Application
   QTk at 'x-oz://system/wp/QTk.ozf'
define
   proc {OnClose}
      {Application.exit 0}
   end

   %% Descripe the GUI in a declarative style.
   GUIDescription = td(label(text:"Hello World!")
		       action:OnClose %% Exit app when window closes.
		      )

   %% Create a window object from the description and show it.
   Window = {QTk.build GUIDescription}
   {Window show}
end
```



## Pascal

```pascal
Program WindowCreation_SDL;

{$linklib SDL}

uses
  SDL,
  SysUtils;

var
  screen: PSDL_Surface;

begin
  SDL_Init(SDL_INIT_VIDEO);
  screen := SDL_SetVideoMode( 800, 600, 16, (SDL_SWSURFACE or SDL_HWPALETTE) );
  sleep(2000);
  SDL_Quit;
end.
```



## Perl

==={{libheader|Perl/Tk}}===

```perl
  use Tk;

  MainWindow->new();
  MainLoop;
```


==={{libheader|Perl/SDL}}===

```perl
  use SDL::App;
  use SDL::Event;

  $app = SDL::App->new;
  $app->loop({
    SDL_QUIT() => sub { exit 0; },
  });
```


==={{libheader|Perl/Gtk3}}===

```perl
  use Gtk3 '-init';

  $window = Gtk3::Window->new;
  $window->signal_connect(
    destroy => sub { Gtk3->main_quit; }
  );
  $window->show_all;
  Gtk3->main;
```


==={{libheader|Perl/Qt}}===

```perl
use strict;
use warnings;
use QtGui4;

my $app = Qt::Application(\@ARGV);
my $window = Qt::MainWindow;
$window->show;
exit $app->exec;
```


==={{libheader|Perl/Wx}}===

```perl
  use Wx;

  $window = Wx::Frame->new(undef, -1, 'title');
  $window->Show;
  Wx::SimpleApp->new->MainLoop;
```



## Perl 6

<b>Library</b> [https://github.com/perl6/gtk-simple GTK]

Exit either by clicking the button or the close window control in the upper corner.

```perl6
use GTK::Simple;
use GTK::Simple::App;

my GTK::Simple::App $app .= new(title => 'Simple GTK Window');

$app.size-request(250, 100);

$app.set-content(
    GTK::Simple::VBox.new(
        my $button = GTK::Simple::Button.new(label => 'Exit'),
    )
);

$app.border-width = 40;

$button.clicked.tap: { $app.exit }

$app.run;
```



## Phix

{{libheader|pGUI}} (works on Windows/Linux, 32/64-bit)

```Phix
-- demo\rosetta\Window_creation.exw
include pGUI.e

IupOpen()
Ihandle dlg = IupDialog(IupVbox({IupLabel("hello")},"MARGIN=200x200"))
IupSetAttribute(dlg,"TITLE","Hello")
IupCloseOnEscape(dlg)
IupShow(dlg)
IupMainLoop()
IupClose()
```



## PicoLisp

```PicoLisp
(load "@lib/openGl.l")

(glutInit)
(glutCreateWindow "Goodbye, World!")
(keyboardFunc '(() (bye)))
(glutMainLoop)
```



## PowerShell

```powershell
New-Window -Show
```

```powershell
$form = New-Object Windows.Forms.Form
$form.Text = "A Window"
$form.Size = New-Object Drawing.Size(150,150)
$form.ShowDialog() | Out-Null
```



## Prolog

Works with SWI-Prolog which has a graphic interface XPCE.

```Prolog
?- new(D, window('Prolog Window')), send(D, open).
```


## PureBasic


```PureBasic
Define MyWin.i, Event.i

MyWin = OpenWindow(#PB_Any, 412, 172, 402, 94, "PureBasic")

; Event loop
Repeat
   Event = WaitWindowEvent()
   Select Event
      Case #PB_Event_Gadget
         ; Handle any gadget events here
      Case #PB_Event_CloseWindow
         Break
   EndSelect
ForEver
```



## Python

```python
  import Tkinter

  w = Tkinter.Tk()
  w.mainloop()
```


```python
  from wxPython.wx import *

  class MyApp(wxApp):
    def OnInit(self):
      frame = wxFrame(NULL, -1, "Hello from wxPython")
      frame.Show(true)
      self.SetTopWindow(frame)
      return true

  app = MyApp(0)
  app.MainLoop()
```


```python
  import win32ui
  from pywin.mfc.dialog import Dialog

  d = Dialog(win32ui.IDD_SIMPLE_INPUT)
  d.CreateWindow()
```


```python
  import gtk

  window = gtk.Window()
  window.show()
  gtk.main()
```


```python
  from PyQt4.QtGui import *

  app = QApplication([])
  win = QWidget()
  win.show()

  app.exec_()
```



## R

Although R cannot create windows itself, it has wrappers for several GUI toolkits.  tcl/tk is shipped with R by default, and you can create windows with that.

```r

win <- tktoplevel()

```


The gWidgets packages let you write GUIs in a toolkit independent way.  You can create a window with

```r

library(gWidgetstcltk) #or e.g. gWidgetsRGtk2
win <- gwindow()

```



## Racket



```racket

#lang racket/gui
(send (new frame%
           [label "New Window"]
           [width 100] [height 100])
      show #t)

```



## RapidQ

   create form as qform
      center
      width=500
      height=400
   end create
   form.showModal


## REBOL



```REBOL

view layout [size 100x100]

```


'size' needed to show the close-window button.

## Red

Empty Window with close [X] button

```Red>>
view []

```


## Ring


```ring

Load "guilib.ring"

MyApp = New qApp {
        win1 = new qWidget() {
               setwindowtitle("Hello World")
               setGeometry(100,100,370,250)
               show()}
        exec()}

```



## Ruby

```ruby
 require 'tk'

 window = TkRoot::new()
 window::mainloop()
```


```ruby
 require 'gtk2'

 window = Gtk::Window.new.show
 Gtk.main
```


```ruby
Shoes.app {}
```



## Run BASIC

Show a empty browser with a button to "Close Me"

```runbasic
html "Close me!"
button #c, "Close Me", [doExit]
wait

' -----------------------------------------------------------------------------------
' Get outta here. depending on how may layers you are into the window (history level)
' If you are at the top level then close the window
' ----------------------------------------------------------------------------------
[doExit]
html "<script language='javascript' type='text/javascript'>
var a = history.length;
a = a - 1;
window.open('','_parent','');
window.close();
history.go(-a);
</script>"
wait
```



## Scheme

```scheme

#!r6rs

;; PS-TK example: display simple frame

(import (rnrs)
        (lib pstk main) ; change this to refer to your installation of PS/Tk
        )

(define tk (tk-start))
(tk/wm 'title tk "PS-Tk Example: Frame")

(tk-event-loop tk)

```



## Scala

```scala
import javax.swing.JFrame

object ShowWindow{
  def main(args: Array[String]){
    var jf = new JFrame("Hello!")

    jf.setSize(800, 600)
    jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    jf.setVisible(true)
  }
}
```


Using native Scala libraries (which are wrappers over Java libraries):


```scala
import scala.swing._
import scala.swing.Swing._

object SimpleWindow extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Hello!"
    preferredSize = ((800, 600):Dimension)
  }
}
```



## Seed7

Seed7 has a graphics library ([http://seed7.sourceforge.net/libraries/draw.htm draw.s7i]),
which is operating system independend.
Seed7 programs run on all supported operating systems without a change.
The program below opens a small window.
The program waits until a key is pressed and exits.


```seed7
$ include "seed7_05.s7i";
  include "draw.s7i";
  include "keybd.s7i";

const proc: main is func
  begin
    screen(200, 200);
    KEYBOARD := GRAPH_KEYBOARD;
    ignore(getc(KEYBOARD));
  end func;
```



## Sidef


### Tk


```ruby
var tk = require('Tk');
%s'MainWindow'.new;
tk.MainLoop;
```



### Gtk2


```ruby
var gtk2 = require('Gtk2') -> init;
var window = %s'Gtk2::Window'.new;
window.signal_connect(destroy => func(*_) { gtk2.main_quit });
window.show_all;
gtk2.main;
```



## Smalltalk

```smalltalk>SystemWindow new openInWorld.</lang


```smalltalk
|top|
top := TopView new.
top add: (Label label: 'Hello World') in:(0.0@0.0 corner:1.0@1.0).
top open
```



## Tcl

Loading the [[Tk]] package is all that is required to get an initial window:

```tcl>package require Tk</lang

If you need an additional window:

```tcl>toplevel .top</lang

If you are using the increasingly popular [http://www.equi4.com/tclkit.html tclkit] under MS Windows, all you have to do is associate the tclkit with the extension “<tt>.tcl</tt>” and then create an <i>empty</i> file with, e.g., with the name <tt>nothing.tcl</tt>. Double-clicking that will “open a window” (an empty one).

=={{header|TI-89 BASIC}}==


```ti89b
:Text "Rosetta Code"
```



## Toka

Toka does not inherently know about graphical environments, but can interact with them using external libraries. This example makes use of the [[SDL]] library bindings included with Toka.

 needs sdl
 800 600 sdl_setup


## TorqueScript



```TorqueScript

new GuiControl(GuiName)
{
	profile = "GuiDefaultProfile";
	horizSizing = "right";
	vertSizing = "bottom";
	position = "0 0";
	extent = "640 480";
	minExtent = "8 2";
	enabled = 1;
	visible = 1;
	clipToParent = 1;

	new GuiWindowCtrl()
	{
		profile = "GuiWindowProfile";
		horizSizing = "right";
		vertSizing = "bottom";
		position = "0 0";
		extent = "100 200";
		minExtent = "8 2";
		enabled = 1;
		visible = 1;
		clipToParent = 1;
		command = "canvas.popDialog(GuiName);";
		accelerator = "escape";
		maxLength = 255;
		resizeWidth = 1;
		resizeHeight = 1;
		canMove = 1;
		canClose = 1;
		canMinimize = 1;
		canMaximize = 1;
		minSize = "50 50";
		closeCommand = "canvas.popDialog(GuiName);";
	};
};

canvas.pushDialog(GuiName);

```



## TXR


TXR has no library module for connecting to SDL, X11's Xlib, or GTK2.

All of these examples are completely self-contained, using the FFI capability in TXR, which can bind to any library whose interface is defined in terms of C functions and types.

No C header file is processed, and not a single line of C has to be compiled.


### SDL


A wait for a SDL key-up event is added, missing in the C version, so that the window does not just appear and disappear.

Note that SDL's header file uses a <code>enum</code> for the event constants like <code>SDL_KEYUP</code>. But then in <code>union SD_Event</code>, the event field declared as <code>UInt8</code>. (That's how it appears on my Ubuntu system; newer versions of SDL seems to have switched the type field, and other fields of the event structures, to <code>UInt32</code>.)

Here, we exploit TXR's capability to define enumerations of specific types: we make the event enumeration based on <code>uint8</code>, giving it a <code>typedef</code> name, and then use that <code>typedef</code> in the <code>SD_Event</code> union.


```txrlisp
(defvarl SDL_INIT_VIDEO #x00000020)
(defvarl SDL_SWSURFACE #x00000000)
(defvarl SDL_HWPALETTE #x20000000)

(typedef SDL_Surface (cptr SDL_Surface))

(typedef SDL_EventType (enumed uint8 SDL_EventType
                          (SDL_KEYUP 3)
                          (SDL_QUIT 12)))

(typedef SDL_Event (union SD_Event
                     (type SDL_EventType)
		     (pad (array 8 uint32))))


(with-dyn-lib "libSDL.so"
  (deffi SDL_Init "SDL_Init" int (uint32))
  (deffi SDL_SetVideoMode "SDL_SetVideoMode"
    SDL_Surface (int int int uint32))
  (deffi SDL_GetError "SDL_GetError" str ())
  (deffi SDL_WaitEvent "SDL_WaitEvent" int ((ptr-out SDL_Event)))
  (deffi SDL_Quit "SDL_Quit" void ()))

(when (neql 0 (SDL_Init SDL_INIT_VIDEO))
  (put-string `unable to initialize SDL: @(SDL_GetError)`)
  (exit nil))

(unwind-protect
  (progn
    (SDL_SetVideoMode 800 600 16 (logior SDL_SWSURFACE SDL_HWPALETTE))
    (let ((e (make-union (ffi SDL_Event))))
      (until* (memql (union-get e 'type) '(SDL_KEYUP SDL_QUIT))
        (SDL_WaitEvent e))))
  (SDL_Quit))
```



### X11


One difference between the C original and this one is that the XLib macros for direct structure access, like <code>DefaultGC</code>, <code>DefaultScreen</code> or <code>WhitePixel</code> are not used; rather the correspoding C functions are used via FFI: <code>XDefaultScreen</code> and so on. The macro approach can be mimiced in detail, at the cost of a significant increase in verbosity (cloning the full declaration of the <code>_XDisplay</code> struct declaration, and reproducing the macros).

Also, this uses an enumeration for the events, so when the event type is decoded from the <code>XEvent</code> union, it comes out as a Lisp symbol.


```txrlisp
(typedef XID uint32)

(typedef Window XID)

(typedef Drawable XID)

(typedef Display (cptr Display))

(typedef GC (cptr GC))

(typedef XEventType (enum _XEventType
                      (KeyPress 2)
                      (Expose 12)))

(defvarl KeyPressMask (ash 1 0))
(defvarl ExposureMask (ash 1 15))

(typedef XEvent (union _XEvent
                  (type XEventType)
                  (pad (array 24 long))))

(defvarl NULL cptr-null)

(with-dyn-lib "libX11.so"
  (deffi XOpenDisplay "XOpenDisplay" Display (bstr))
  (deffi XCloseDisplay "XCloseDisplay" int (Display))
  (deffi XDefaultScreen "XDefaultScreen"  int (Display))
  (deffi XRootWindow "XRootWindow" Window (Display int))
  (deffi XBlackPixel "XBlackPixel" ulong (Display int))
  (deffi XWhitePixel "XWhitePixel" ulong (Display int))
  (deffi XCreateSimpleWindow "XCreateSimpleWindow" Window (Display
                                                           Window
                                                           int int
                                                           uint uint uint
                                                           ulong ulong))
  (deffi XSelectInput "XSelectInput" int (Display Window long))
  (deffi XMapWindow "XMapWindow" int (Display Window))
  (deffi XNextEvent "XNextEvent" int (Display (ptr-out XEvent)))
  (deffi XDefaultGC "XDefaultGC" GC (Display int))
  (deffi XFillRectangle "XFillRectangle" int (Display Drawable GC
                                              int int uint uint))
  (deffi XDrawString "XDrawString" int (Display Drawable GC
                                        int int bstr int)))

(let* ((msg "Hello, world!")
       (d (XOpenDisplay nil)))
  (when (equal d NULL)
    (put-line "Cannot-open-display" *stderr*)
    (exit 1))

  (let* ((s (XDefaultScreen d))
         (w (XCreateSimpleWindow d (XRootWindow d s) 10 10 100 100 1
                                 (XBlackPixel d s) (XWhitePixel d s))))
    (XSelectInput d w (logior ExposureMask KeyPressMask))
    (XMapWindow d w)

    (while t
      (let ((e (make-union (ffi XEvent))))
        (XNextEvent d e)
        (caseq (union-get e 'type)
          (Expose
            (XFillRectangle d w (XDefaultGC d s) 20 20 10 10)
            (XDrawString d w (XDefaultGC d s) 10 50 msg (length msg)))
          (KeyPress (return)))))

    (XCloseDisplay d)))
```



### GTK2


```txrlisp
(typedef GtkObject* (cptr GtkObject))
(typedef GtkWidget* (cptr GtkWidget))

(typedef GtkWidget* (cptr GtkWidget))

(typedef GtkWindowType (enum GtkWindowType
                         GTK_WINDOW_TOPLEVEL
                         GTK_WINDOW_POPUP))

(with-dyn-lib "libgtk-x11-2.0.so.0"
  (deffi gtk_init "gtk_init" void ((ptr int) (ptr (ptr (zarray str)))))
  (deffi gtk_window_new "gtk_window_new" GtkWidget* (GtkWindowType))
  (deffi gtk_signal_connect_full "gtk_signal_connect_full"
    ulong (GtkObject* str closure closure val closure int int))
  (deffi gtk_widget_show "gtk_widget_show" void (GtkWidget*))
  (deffi gtk_main "gtk_main" void ())
  (deffi-sym gtk_main_quit "gtk_main_quit"))

(defmacro GTK_OBJECT (cptr)
  ^(cptr-cast 'GtkObject ,cptr))

(defmacro gtk_signal_connect (object name func func-data)
  ^(gtk_signal_connect_full ,object ,name ,func cptr-null
                            ,func-data cptr-null 0 0))

(gtk_init (length *args*) (vec-list *args*))

(let ((window (gtk_window_new 'GTK_WINDOW_TOPLEVEL)))
  (gtk_signal_connect (GTK_OBJECT window) "destroy" gtk_main_quit nil)
  (gtk_widget_show window)
  (gtk_main))
```



### Win32/Win64


This solution is based on the "Your First Windows Program" example in MSDN. It registers a Window class, creates a Window and runs a Windows message loop against a custom <code>WndProc</code> function that is written in Lisp, which handles <code>WM_QUIT</code> and <code>WM_PAINT</code> events exactly like its C counterpart. All necessary basic types, structures, constants and foreign functions are declared using the TXR FFI language.

Note that the <code>CW_USEDEFAULT</code> constant in the Windows header files is defined as <code>0x80000000</code>. This is out of range of the signed <code>int</code> arguments of <code>CreateWindowEx</code> with which it is used. Microsoft is relying on an implementation-defined C conversion to turn this value into the most negative <code>int</code>. When the original constant was used in the TXR translation, TXR's FFI '''uncovered this little problem''' by throwing an exception arising from the out-of-range conversion attempt. The fix is to specify the correct value directly as <code>#x-80000000</code>.


```txrlisp
(typedef LRESULT int-ptr-t)
(typedef LPARAM int-ptr-t)
(typedef WPARAM uint-ptr-t)

(typedef UINT uint32)
(typedef LONG int32)
(typedef WORD uint16)
(typedef DWORD uint32)
(typedef LPVOID cptr)
(typedef BOOL (bool int32))
(typedef BYTE uint8)

(typedef HWND (cptr HWND))
(typedef HINSTANCE (cptr HINSTANCE))
(typedef HICON (cptr HICON))
(typedef HCURSOR (cptr HCURSOR))
(typedef HBRUSH (cptr HBRUSH))
(typedef HMENU (cptr HMENU))
(typedef HDC (cptr HDC))

(typedef ATOM WORD)
(typedef LPCTSTR wstr)

(defvarl NULL cptr-null)

(typedef WNDCLASS (struct WNDCLASS
                    (style UINT)
                    (lpfnWndProc closure)
                    (cbClsExtra int)
                    (cbWndExtra int)
                    (hInstance HINSTANCE)
                    (hIcon HICON)
                    (hCursor HCURSOR)
                    (hbrBackground HBRUSH)
                    (lpszMenuName LPCTSTR)
                    (lpszClassName LPCTSTR)))

(defmeth WNDCLASS :init (me)
  (zero-fill (ffi WNDCLASS) me))

(typedef POINT (struct POINT
                 (x LONG)
                 (y LONG)))

(typedef MSG (struct MSG
               (hwnd HWND)
               (message UINT)
               (wParam WPARAM)
               (lParam LPARAM)
               (time DWORD)
               (pt POINT)))

(typedef RECT (struct RECT
                (left LONG)
                (top LONG)
                (right LONG)
                (bottom LONG)))

(typedef PAINTSTRUCT (struct PAINTSTRUCT
                       (hdc HDC)
                       (fErase BOOL)
                       (rcPaint RECT)
                       (fRestore BOOL)
                       (fIncUpdate BOOL)
                       (rgbReserved (array 32 BYTE))))

(defvarl CW_USEDEFAULT #x-80000000)
(defvarl WS_OVERLAPPEDWINDOW #x00cf0000)

(defvarl SW_SHOWDEFAULT 5)

(defvarl WM_DESTROY 2)
(defvarl WM_PAINT 15)

(defvarl COLOR_WINDOW 5)

(deffi-cb wndproc-fn LRESULT (HWND UINT LPARAM WPARAM))

(with-dyn-lib "kernel32.dll"
  (deffi GetModuleHandle "GetModuleHandleW" HINSTANCE (wstr)))

(with-dyn-lib "user32.dll"
  (deffi RegisterClass "RegisterClassW" ATOM ((ptr-in WNDCLASS)))
  (deffi CreateWindowEx "CreateWindowExW" HWND (DWORD
                                               LPCTSTR LPCTSTR
                                               DWORD
                                               int int int int
                                               HWND HMENU HINSTANCE
                                               LPVOID))
  (deffi ShowWindow "ShowWindow" BOOL (HWND int))
  (deffi GetMessage "GetMessageW"  BOOL ((ptr-out MSG) HWND UINT UINT))
  (deffi TranslateMessage "TranslateMessage"  BOOL ((ptr-in MSG)))
  (deffi DispatchMessage "DispatchMessageW"  LRESULT ((ptr-in MSG)))
  (deffi PostQuitMessage "PostQuitMessage" void (int))
  (deffi DefWindowProc "DefWindowProcW" LRESULT (HWND UINT LPARAM WPARAM))
  (deffi BeginPaint "BeginPaint" HDC (HWND (ptr-out PAINTSTRUCT)))
  (deffi EndPaint "EndPaint" BOOL (HWND (ptr-in PAINTSTRUCT)))
  (deffi FillRect "FillRect" int (HDC (ptr-in RECT) HBRUSH)))

(defun WindowProc (hwnd uMsg wParam lParam)
  (caseql* uMsg
    (WM_DESTROY
      (PostQuitMessage 0)
      0)
    (WM_PAINT
      (let* ((ps (new PAINTSTRUCT))
             (hdc (BeginPaint hwnd ps)))
        (FillRect hdc ps.rcPaint (cptr-int (succ COLOR_WINDOW) 'HBRUSH))
        (EndPaint hwnd ps)
        0))
    (t (DefWindowProc hwnd uMsg wParam lParam))))

(let* ((hInstance (GetModuleHandle nil))
       (wc (new WNDCLASS
                lpfnWndProc [wndproc-fn WindowProc]
                hInstance hInstance
                lpszClassName "Sample Window Class")))
  (RegisterClass wc)
  (let ((hwnd (CreateWindowEx 0 wc.lpszClassName "Learn to Program Windows"
                              WS_OVERLAPPEDWINDOW
                              CW_USEDEFAULT CW_USEDEFAULT
                              CW_USEDEFAULT CW_USEDEFAULT
                              NULL NULL hInstance NULL)))
    (unless (equal hwnd NULL)
      (ShowWindow hwnd SW_SHOWDEFAULT)

      (let ((msg (new MSG)))
        (while (GetMessage msg NULL 0 0)
          (TranslateMessage msg)
          (DispatchMessage msg))))))
```



## Vedit macro language

Creates an empty window with ID 'A' near the upper left corner of document area, with height of 20 text lines and width of 80 characters.

```vedit
Win_Create(A, 2, 5, 20, 80)
```

Note: if you run this command while in Visual Mode, you should adjust your active window smaller so that the created window will not be hidden behind it (since the active window is always on top).

## VBA


```txt
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Need to reference the following object library (From the Tools menu, choose References)
    Microsoft Forms 2.0 Object Library
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
And :
Programmatic Access to Visual Basic Project must be trusted. See it in Macro's security!
```


```vb

Option Explicit

Sub InsertForm()
Dim myForm As Object, strname As String
    Set myForm = ThisWorkbook.VBProject.VBComponents.Add(3)
    strname = myForm.Name
    VBA.UserForms.Add(strname).Show
End Sub
```



## Visual Basic .NET



```vb
    Dim newForm as new Form
    newForm.Text = "It's a new window"

        newForm.Show()
```




## X86 Assembly

```asm

;GTK imports and defines etc.
%define GTK_WINDOW_TOPLEVEL 0

extern gtk_init
extern gtk_window_new
extern gtk_widget_show
extern gtk_signal_connect
extern gtk_main
extern g_print
extern gtk_main_quit

bits 32

section .text
	global _main

        ;exit signal
	sig_main_exit:
		push exit_sig_msg
		call g_print
		add esp, 4
		call gtk_main_quit
		ret

	_main:
		mov ebp, esp
		sub esp, 8
		push argv
		push argc
		call gtk_init
		add esp, 8				;stack alignment.
		push GTK_WINDOW_TOPLEVEL
		call gtk_window_new
		add esp, 4
		mov [ebp-4], eax		;ebp-4 now holds our GTKWindow pointer.
		push 0
		push sig_main_exit
		push gtk_delete_event
		push dword [ebp-4]
		call gtk_signal_connect
		add esp, 16
		push dword [ebp-4]
		call gtk_widget_show
		add esp, 4
		call gtk_main

section .data
;sudo argv
argc                dd 1
argv                dd args
args                dd title
						  dd 0

title               db "GTK Window",0
gtk_delete_event   db 'delete_event',0
exit_sig_msg      db "-> Rage quitting..",10,0


```



```asm

.586
.model flat, stdcall
option casemap:none

include /masm32/include/windows.inc
include /masm32/include/kernel32.inc
include /masm32/include/user32.inc

includelib /masm32/lib/kernel32.lib
includelib /masm32/lib/user32.lib

WinMain proto :dword,:dword,:dword,:dword

.data
   ClassName db "WndClass",0
   AppName   db "Window!",0
.data?
   hInstance   dd ?
   CommandLine dd ?

.code
start:
   invoke GetModuleHandle, NULL
   mov hInstance, eax
   invoke GetCommandLine
   mov CommandLine, eax
   invoke WinMain, hInstance, NULL, CommandLine, SW_SHOWDEFAULT

   WinMain proc hInst:dword, hPervInst:dword, CmdLine:dword, CmdShow:dword
   LOCAL wc:WNDCLASSEX
   LOCAL msg:MSG
   LOCAL hwnd:HWND

   wc.cbSize, sizeof WNDCLASSEX
   wc.style, CS_HREDRAW or CS_VREDRAW
   wc.lpfnWndPRoc, offset WndProc
   wc.cbClsExtra,NULL
   wc.cbWndExtra, NULL
   push hInstance
   pop wc.hInstance
   mov wc.hbrBackground, COLOR_BTNFACE+1
   mov wc.lpszMenuName NULL
   mov wc.lpszClassName, offset ClassName
   invoke LoadIcon, NULL, IDI_APPLICATION
   mov wc.hIcon, eax
   mov wc.hIconSm, eax
   invoke LoadCursor, NULL, IDC_ARROW
   mov wc.hCursor, eax
   invoke RegisterClassEx, addr wc
   invoke CreateWindowEx, NULL, addr ClassName, addr AppName, WS_OVERLAPPEDWINDOW, CS_USEDEFAULT, CW_USEDEFAUT,\
   CW_USEDEFAUT, CW_USEDEFAUT, NULL, NULL, hInst, NULL
   mov hwnd, eax
   invoke ShowWindow, hwnd, SW_SHOWNORMAL
   invoke UpdateWindow, hwnd
   .while TRUE
      invoke GetMessage, addr msg, NULL, 0,0
      .break .if (!eax)
      invoke TranslateMessage, addr msg
      invoke DispatchMessage, addr msg
   .endw
   mov eax, msg.wParam
   ret
   WinMain endp

   WndProc proc hWnd:dword, uMsg:dword, wParam:dword, lParam:dword
   mov eax, uMsg
   .if eax==WM_DESTROY
      invoke PostQuitMessage, NULL
   .else
      invoke DefWindowProc, hWnd, uMsg, wParam, lParam
   .endif
   xor eax, eax
   ret
   WndProc endp
end start

```


