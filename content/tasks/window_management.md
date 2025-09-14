+++
title = "Window management"
description = ""
date = 2018-12-22T18:14:09Z
aliases = []
[extra]
id = 4266
[taxonomies]
categories = ["task", "GUI"]
tags = []
languages = [
  "autohotkey",
  "bbc_basic",
  "c",
  "gambas",
  "go",
  "hicest",
  "java",
  "julia",
  "mathematica",
  "nim",
  "oz",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "purebasic",
  "python",
  "racket",
  "ring",
  "tcl",
]
+++

## Task

Treat windows or at least window identities as [[wp:First-class_object|first class objects]].

*Store window identities in variables, compare them for equality.
*Provide examples of performing some of the following:
**hide, show, close, minimize, maximize, move, and resize a window. 
The window of interest may or may not have been created by your program.


## AutoHotkey


```AutoHotkey
F1::  ;; when user hits the F1 key, do the following
WinGetTitle, window, A  ; get identity of active window into a variable
WinMove, %window%, , 100, 100, 800, 800 ; move window to coordinates, 100, 100
                                        ; and change size to 800 x 800 pixels
sleep, 2000
WinHide, % window    ; hide window
TrayTip, hidden, window is hidden, 2
sleep, 2000
WinShow, % window  ; show window again
loop,
{
  inputbox, name, what was the name of your window? 
  if (name = window) ; compare window variables for equality
  {
    msgbox you got it
    break
  }
; else try again
}
WinClose, % window     
return
```



## BBC BASIC

```bbcbasic
      SWP_NOMOVE = 2
      SWP_NOZORDER = 4
      SW_MAXIMIZE = 3
      SW_MINIMIZE = 6
      SW_RESTORE = 9
      SW_HIDE = 0
      SW_SHOW = 5
      
      REM Store window handle in a variable:
      myWindowHandle% = @hwnd%
      
      PRINT "Hiding the window in two seconds..."
      WAIT 200
      SYS "ShowWindow", myWindowHandle%, SW_HIDE
      WAIT 200
      SYS "ShowWindow", myWindowHandle%, SW_SHOW
      PRINT "Windows shown again."
      
      PRINT "Minimizing the window in two seconds..."
      WAIT 200
      SYS "ShowWindow", myWindowHandle%, SW_MINIMIZE
      WAIT 200
      SYS "ShowWindow", myWindowHandle%, SW_RESTORE
      PRINT "Maximizing the window in two seconds..."
      WAIT 200
      SYS "ShowWindow", myWindowHandle%, SW_MAXIMIZE
      WAIT 200
      SYS "ShowWindow", myWindowHandle%, SW_RESTORE
      PRINT "Now restored to its normal size."
      
      PRINT "Resizing the window in two seconds..."
      WAIT 200
      SYS "SetWindowPos", myWindowHandle%, 0, 0, 0, 400, 200, \
      \    SWP_NOMOVE OR SWP_NOZORDER
      
      PRINT "Closing the window in two seconds..."
      WAIT 200
      QUIT
```



## C

C does not have any standard windowing library, although cross platform libraries are available, and although it's technically possible to create windows from scratch given the '''''Dark powers''''' that C commands, I chose the simplest option, the Windows API. On running this program, the user is taken over all the sub tasks listed in this task one by one.

### Windows


```C

#include<windows.h>
#include<unistd.h>
#include<stdio.h>

const char g_szClassName[] = "weirdWindow";

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch(msg)
    {
        case WM_CLOSE:
            DestroyWindow(hwnd);
        break;
        case WM_DESTROY:
            PostQuitMessage(0);
        break;
        default:
            return DefWindowProc(hwnd, msg, wParam, lParam);
    }
    return 0;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
    LPSTR lpCmdLine, int nCmdShow)
{
    WNDCLASSEX wc;
    HWND hwnd[3];
    MSG Msg;
	int i,x=0,y=0;
	char str[3][100];
	int maxX = GetSystemMetrics(SM_CXSCREEN), maxY = GetSystemMetrics(SM_CYSCREEN);
	
	char messages[15][180] = {"Welcome to the Rosettacode Window C implementation.",
	"If you can see two blank windows just behind this message box, you are in luck.",
	"Let's get started....",
	"Yes, you will be seeing a lot of me :)",
	"Now to get started with the tasks, the windows here are stored in an array of type HWND, the array is called hwnd (yes, I know it's very innovative.)",
	"Let's compare the windows for equality.",
	"Now let's hide Window 1.",
	"Now let's see Window 1 again.",
	"Let's close Window 2, bye, bye, Number 2 !",
	"Let's minimize Window 1.",
	"Now let's maximize Window 1.",
	"And finally we come to the fun part, watch Window 1 move !",
	"Let's double Window 1 in size for all the good work.",
	"That's all folks ! (You still have to close that window, that was not part of the contract, sue me :D )"};

    wc.cbSize        = sizeof(WNDCLASSEX);
    wc.style         = 0;
    wc.lpfnWndProc   = WndProc;
    wc.cbClsExtra    = 0;
    wc.cbWndExtra    = 0;
    wc.hInstance     = hInstance;
    wc.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
    wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
    wc.lpszMenuName  = NULL;
    wc.lpszClassName = g_szClassName;
    wc.hIconSm       = LoadIcon(NULL, IDI_APPLICATION);

    if(!RegisterClassEx(&wc))
    {
        MessageBox(NULL, "Window Registration Failed!", "Error!",MB_ICONEXCLAMATION | MB_OK);
        return 0;
    }

	for(i=0;i<2;i++){
		
		sprintf(str[i],"Window Number %d",i+1);
		
		hwnd[i] = CreateWindow(g_szClassName,str[i],WS_OVERLAPPEDWINDOW,i*maxX/2 , 0, maxX/2-10, maxY/2-10,NULL, NULL, hInstance, NULL);
		
		if(hwnd[i] == NULL)
		{
			MessageBox(NULL, "Window Creation Failed!", "Error!",MB_ICONEXCLAMATION | MB_OK);
			return 0;
		}

		ShowWindow(hwnd[i], nCmdShow);
		UpdateWindow(hwnd[i]);
	}
	
	for(i=0;i<6;i++){
			MessageBox(NULL, messages[i], "Info",MB_APPLMODAL| MB_ICONINFORMATION | MB_OK);		
	}
	
	if(hwnd[0]==hwnd[1])
			MessageBox(NULL, "Window 1 and 2 are equal.", "Info",MB_APPLMODAL| MB_ICONINFORMATION | MB_OK);
	else
			MessageBox(NULL, "Nope, they are not.", "Info",MB_APPLMODAL| MB_ICONINFORMATION | MB_OK);
		
	MessageBox(NULL, messages[6], "Info",MB_APPLMODAL| MB_ICONINFORMATION | MB_OK);
	
	ShowWindow(hwnd[0], SW_HIDE);
	
	MessageBox(NULL, messages[7], "Info",MB_APPLMODAL| MB_ICONINFORMATION | MB_OK);
	
	ShowWindow(hwnd[0], SW_SHOW);
	
	MessageBox(NULL, messages[8], "Info",MB_APPLMODAL| MB_ICONINFORMATION | MB_OK);
	
	ShowWindow(hwnd[1], SW_HIDE);
	
	MessageBox(NULL, messages[9], "Info",MB_APPLMODAL| MB_ICONINFORMATION | MB_OK);
	
	ShowWindow(hwnd[0], SW_MINIMIZE);
	
	MessageBox(NULL, messages[10], "Info",MB_APPLMODAL| MB_ICONINFORMATION | MB_OK);
	
	ShowWindow(hwnd[0], SW_MAXIMIZE);
	
	MessageBox(NULL, messages[11], "Info",MB_APPLMODAL| MB_ICONINFORMATION | MB_OK);
	
	ShowWindow(hwnd[0], SW_RESTORE);
	
	while(x!=maxX/2||y!=maxY/2){
		if(x<maxX/2)
			x++;
		if(y<maxY/2)
			y++;
		
		MoveWindow(hwnd[0],x,y,maxX/2-10, maxY/2-10,0);
		sleep(10);
	}
	
	MessageBox(NULL, messages[12], "Info",MB_APPLMODAL| MB_ICONINFORMATION | MB_OK);
	
	MoveWindow(hwnd[0],0,0,maxX, maxY,0);
	
	MessageBox(NULL, messages[13], "Info",MB_APPLMODAL| MB_ICONINFORMATION | MB_OK);

    while(GetMessage(&Msg, NULL, 0, 0) > 0)
    {
        TranslateMessage(&Msg);
        DispatchMessage(&Msg);
    }
    return Msg.wParam;
}

```



## Gambas


```gambas
sWindow As New String[4]
'________________________
Public Sub Form_Open()

Manipulate

End
'________________________
Public Sub Manipulate()
Dim siDelay As Short = 2

Me.Show
Print "Show"
Wait siDelay

sWindow[0] = Me.Width
sWindow[1] = Me.Height
sWindow[2] = Me.X
sWindow[3] = Me.y

Me.Hide
Print "Hidden"
CompareWindow
Wait siDelay

Me.Show
Print "Show"
CompareWindow
Wait siDelay

Me.Minimized = True
Print "Minimized"
CompareWindow
Wait siDelay

Me.Show
Print "Show"
CompareWindow
Wait siDelay

Me.Maximized = True
Print "Maximized"
CompareWindow
Wait siDelay

Me.Maximized = False
Print "Not Maximized"
CompareWindow
Wait siDelay

Me.Height = 200
Me.Width = 300
Print "Resized"
CompareWindow
Wait siDelay

Me.x = 10
Me.Y = 10
Print "Moved"
CompareWindow
Wait siDelay

Me.Close

End
'________________________
Public Sub CompareWindow()
Dim sNewWindow As New String[4]
Dim siCount As Short
Dim bMatch As Boolean = True

sNewWindow[0] = Me.Width
sNewWindow[1] = Me.Height
sNewWindow[2] = Me.X
sNewWindow[3] = Me.y

For siCount = 0 To 3
  If sWindow[siCount] <> sNewWindow[siCount] Then bMatch = False
Next

If bMatch Then 
  Print "Windows identities match the original window size"
Else
  Print "Windows identities DONOT match the original window size"
End If

End
```

Output:

```txt

Show
Hidden
Windows identities match the original window size
Show
Windows identities match the original window size
Minimized
Windows identities match the original window size
Show
Windows identities match the original window size
Maximized
Windows identities match the original window size
Not Maximized
Windows identities DONOT match the original window size
Resized
Windows identities DONOT match the original window size
Moved
Windows identities DONOT match the original window size

```



## Go

```go
package main

import (
    "github.com/gotk3/gotk3/gtk"
    "log"
    "time"
)

func check(err error, msg string) {
    if err != nil {
        log.Fatal(msg, err)
    }
}

func main() {
    gtk.Init(nil)

    window, err := gtk.WindowNew(gtk.WINDOW_TOPLEVEL)
    check(err, "Unable to create window:")
    window.SetResizable(true)
    window.SetTitle("Window management")
    window.SetBorderWidth(5)
    window.Connect("destroy", func() {
        gtk.MainQuit()
    })

    stackbox, err := gtk.BoxNew(gtk.ORIENTATION_VERTICAL, 10)
    check(err, "Unable to create stack box:")

    bmax, err := gtk.ButtonNewWithLabel("Maximize")
    check(err, "Unable to create maximize button:")
    bmax.Connect("clicked", func() {
        window.Maximize()
    })

    bunmax, err := gtk.ButtonNewWithLabel("Unmaximize")
    check(err, "Unable to create unmaximize button:")
    bunmax.Connect("clicked", func() {
        window.Unmaximize()
    })

    bicon, err := gtk.ButtonNewWithLabel("Iconize")
    check(err, "Unable to create iconize button:")
    bicon.Connect("clicked", func() {
        window.Iconify()
    })

    bdeicon, err := gtk.ButtonNewWithLabel("Deiconize")
    check(err, "Unable to create deiconize button:")
    bdeicon.Connect("clicked", func() {
        window.Deiconify()
    })

    bhide, err := gtk.ButtonNewWithLabel("Hide")
    check(err, "Unable to create hide button:")
    bhide.Connect("clicked", func() {
        // not working on Ubuntu 16.04 but window 'dims' after a few seconds
        window.Hide() 
        time.Sleep(10 * time.Second)
        window.Show()
    })

    bshow, err := gtk.ButtonNewWithLabel("Show")
    check(err, "Unable to create show button:")
    bshow.Connect("clicked", func() {
        window.Show()
    })

    bmove, err := gtk.ButtonNewWithLabel("Move")
    check(err, "Unable to create move button:")
    isShifted := false
    bmove.Connect("clicked", func() {
        w, h := window.GetSize()
        if isShifted {
            window.Move(w-10, h-10)
        } else {
            window.Move(w+10, h+10)
        }
        isShifted = !isShifted
    })

    bquit, err := gtk.ButtonNewWithLabel("Quit")
    check(err, "Unable to create quit button:")
    bquit.Connect("clicked", func() {
        window.Destroy()
    })

    stackbox.PackStart(bmax, true, true, 0)
    stackbox.PackStart(bunmax, true, true, 0)
    stackbox.PackStart(bicon, true, true, 0)
    stackbox.PackStart(bdeicon, true, true, 0)
    stackbox.PackStart(bhide, true, true, 0)
    stackbox.PackStart(bshow, true, true, 0)
    stackbox.PackStart(bmove, true, true, 0)
    stackbox.PackStart(bquit, true, true, 0)

    window.Add(stackbox)
    window.ShowAll()
    gtk.Main()
}
```



## HicEst


```hicest
CHARACTER title="Rosetta Window_management"
REAL :: w=-333, h=25, x=1, y=0.5 ! pixels < 0,  relative window size 0...1,  script character size > 1

  WINDOW(WINdowhandle=wh, Width=w, Height=h, X=x, Y=y, TItle=title) ! create, on return size/pos VARIABLES are set to script char
  WINDOW(WIN=wh, MINimize)    ! minimize
  WINDOW(WIN=wh, SHowNormal)  ! restore
  WINDOW(WIN=wh, X=31, Y=7+4) !<-- move upper left here (col 31, row 7 + ~4 rows for title, menus, toolbar. Script window in upper left screen)
  WINDOW(WIN=wh, MAXimize)    ! maximize (hides the script window)
  WINDOW(Kill=wh)             ! close
END
```


=={{header|Icon}} and {{header|Unicon}}==
The demo program opens three windows, one with a running commentary on the action.

```Icon
link graphics

procedure main()

   Delay := 3000

   W1 := open("Window 1","g","resize=on","size=400,400","pos=100,100","bg=black","fg=red") |
         stop("Unable to open window 1")
   W2 := open("Window 2","g","resize=on","size=400,400","pos=450,450","bg=blue","fg=yellow") |
         stop("Unable to open window 2")
   W3 := open("Window 3","g","resize=on","size=400,400","pos=600,150","bg=orange","fg=black") |
         stop("Unable to open window 3")
   WWrite(W3,"Opened three windows")
   
   WWrite(W3,"Window 1&2 with rectangles")
   every Wx := W1 | W2 | W3 do
      if Wx ~=== W3 then 
         FillRectangle(Wx,50,50,100,100)     
   
   delay(Delay) 
   WWrite(W3,"Window 1 rasied")   
   Raise(W1)
 
   delay(Delay)
   WWrite(W3,"Window 2 hidden")   
   WAttrib(W2,"canvas=hidden")
   
   delay(Delay) 
   WWrite(W3,"Window 2 maximized")    
   WAttrib(W2,"canvas=maximal")
   Raise(W3)

   delay(Delay)   
   WWrite(W3,"Window 2 restored & resized") 
   WAttrib(W2,"canvas=normal")
   WAttrib(W2,"size=600,600")
   
   delay(Delay)
   WWrite(W3,"Window 2 moved")    
   WAttrib(W2,"posx=700","posy=300")
   
   delay(Delay) 
   WWrite(W3,"Window 2 minimized")     
   WAttrib(W2,"canvas=iconic")  
   
   delay(Delay) 
   WWrite(W3,"Window 2 restored")
   WAttrib(W2,"canvas=normal")  # restore as maximal, possible bug
   
   delay(Delay)
   WWrite(W3,"Enter Q or q here to quit") 
   WDone(W3)         
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/graphics.icn graphics.icn provides graphics]


## Java

Java cannot (easily) manipulate windows created by other programs. This code manipulates windows that it has created, but any window created in the same JVM can be controlled similarly. This example uses Swing - for AWT or SWT go figure.

```Java
import java.awt.BorderLayout;
import java.awt.EventQueue;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.lang.reflect.InvocationTargetException;
import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

public class WindowController extends JFrame {
   // Create UI on correct thread
   public static void main( final String[] args ) {
      EventQueue.invokeLater( () -> new WindowController() );
   }

   private JComboBox<ControlledWindow> list;

   // Button class to call the right method
   private class ControlButton extends JButton {
      private ControlButton( final String name ) {
         super(
            new AbstractAction( name ) {
               public void actionPerformed( final ActionEvent e ) {
                  try {
                     WindowController.class.getMethod( "do" + name )
                        .invoke ( WindowController.this );
                  } catch ( final Exception x ) { // poor practice
                     x.printStackTrace();        // also poor practice
                  }
               }
            }
         );
      }
   }

   // UI for controlling windows
   public WindowController() {
      super( "Controller" );

      final JPanel main = new JPanel();
      final JPanel controls = new JPanel();

      setLocationByPlatform( true );
      setResizable( false );
      setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
      setLayout( new BorderLayout( 3, 3 ) );
      getRootPane().setBorder( new EmptyBorder( 3, 3, 3, 3 ) );
      add( new JLabel( "Add windows and control them." ), BorderLayout.NORTH );
      main.add( list = new JComboBox<>() );
      add( main, BorderLayout.CENTER );
      controls.setLayout( new GridLayout( 0, 1, 3, 3 ) );
      controls.add( new ControlButton( "Add"      ) );
      controls.add( new ControlButton( "Hide"     ) );
      controls.add( new ControlButton( "Show"     ) );
      controls.add( new ControlButton( "Close"    ) );
      controls.add( new ControlButton( "Maximise" ) );
      controls.add( new ControlButton( "Minimise" ) );
      controls.add( new ControlButton( "Move"     ) );
      controls.add( new ControlButton( "Resize"   ) );
      add( controls, BorderLayout.EAST );
      pack();
      setVisible( true );
   }

   // These are the windows we're controlling, but any JFrame would do
   private static class ControlledWindow extends JFrame {
      private int num;

      public ControlledWindow( final int num ) {
         super( Integer.toString( num ) );
         this.num = num;
         setLocationByPlatform( true );
         getRootPane().setBorder( new EmptyBorder( 3, 3, 3, 3 ) );
         setDefaultCloseOperation( JFrame.DISPOSE_ON_CLOSE );
         add( new JLabel( "I am window " + num + ". Use the controller to control me." ) );
         pack();
         setVisible( true );
      }

      public String toString() {
         return "Window " + num;
      }
   }

   // Here comes the useful bit - window control code
   // Everything else was just to allow us to do this!

   public void doAdd() {
      list.addItem( new ControlledWindow( list.getItemCount () + 1 ) );
      pack();
   }

   public void doHide() {
      final JFrame window = getWindow();
      if ( null == window ) {
         return;
      }
      window.setVisible( false );
   }

   public void doShow() {
      final JFrame window = getWindow();
      if ( null == window ) {
         return;
      }
      window.setVisible( true );
   }

   public void doClose() {
      final JFrame window = getWindow();
      if ( null == window ) {
         return;
      }
      window.dispose();
   }

   public void doMinimise() {
      final JFrame window = getWindow();
      if ( null == window ) {
         return;
      }
      window.setState( Frame.ICONIFIED );
   }

   public void doMaximise() {
      final JFrame window = getWindow();
      if ( null == window ) {
         return;
      }
      window.setExtendedState( Frame.MAXIMIZED_BOTH );
   }

   public void doMove() {
      final JFrame window = getWindow();
      if ( null == window ) {
         return;
      }
      final int hPos = getInt( "Horizontal position?" );
      if ( -1 == hPos ) {
         return;
      }
      final int vPos = getInt( "Vertical position?" );
      if ( -1 == vPos ) {
         return;
      }
      window.setLocation ( hPos, vPos );
   }

   public void doResize() {
      final JFrame window = getWindow();
      if ( null == window ) {
         return;
      }
      final int width = getInt( "Width?" );
      if ( -1 == width ) {
         return;
      }
      final int height = getInt( "Height?" );
      if ( -1 == height ) {
         return;
      }
      window.setBounds ( window.getX(), window.getY(), width, height );
   }

   private JFrame getWindow() {
      final JFrame window = ( JFrame ) list.getSelectedItem();
      if ( null == window ) {
         JOptionPane.showMessageDialog( this, "Add a window first" );
      }
      return window;
   }

   private int getInt(final String prompt) {
      final String s = JOptionPane.showInputDialog( prompt );
      if ( null == s ) {
         return -1;
      }
      try {
         return Integer.parseInt( s );
      } catch ( final NumberFormatException x ) {
         JOptionPane.showMessageDialog( this, "Not a number" );
         return -1;
      }
   }
}

```




## Julia

Uses the Gtk windowing package, so the package can manage Gtk's windows.

```julia
using Gtk

function controlwindow(win, lab)
    sleep(4)
    set_gtk_property!(lab, :label, "Hiding...")
    sleep(1)
    println("Hiding widow")
    set_gtk_property!(win, :visible, false)
    sleep(5)
    set_gtk_property!(lab, :label, "Showing...")
    println("Showing window")
    set_gtk_property!(win, :visible, true)
    sleep(5)
    set_gtk_property!(lab, :label, "Resizing...")
    println("Resizing window")
    resize!(win, 300, 300)
    sleep(4)
    set_gtk_property!(lab, :label, "Maximizing...")
    println("Maximizing window")
    sleep(1)
    maximize(win)
    set_gtk_property!(lab, :label, "Closing...")
    sleep(5)
    println("Closing window")
    destroy(win)
    sleep(2)
    exit(0)
end

function runwindow()
    win = GtkWindow("Window Control Test", 500, 30) |> (GtkFrame() |> (vbox = GtkBox(:v)))
    lab = GtkLabel("Window under external control")
    push!(vbox, lab)
    @async(controlwindow(win, lab))

    cond = Condition()
    endit(w) = notify(cond)
    signal_connect(endit, win, :destroy)
    showall(win)
    wait(cond)
end

runwindow()

```



## Mathematica

Mathematica can only easily access and control windows created by itself.


```Mathematica
nb=NotebookCreate[]; (*Create a window and store in a variable*)
nb===nb2 (*test for equality with another window object*)
SetOptions[nb,Visible->False](*Hide*)
SetOptions[nb,Visible->True](*Show*)
NotebookClose[nb] (*Close*)
SetOptions[nb,WindowMargins->{{x,Automatic},{y,Automatic}}](*Move to x,y screen position*)
SetOptions[nb,WindowSize->{100,100}](*Resize*)
```



## Nim

==={{libheader|Gtk2}}===

```nim
import
  gdk2, glib2, gtk2,
  os

proc thisDestroy(widget: pWidget, data: pgpointer){.cdecl.} =
  main_quit()
proc thisMax(widget: pWidget, data: pgpointer){.cdecl.} =
  maximize(get_parent_window(widget))
proc thisUnmax(widget: pWidget, data: pgpointer){.cdecl.} =
  unmaximize(get_parent_window(widget))
proc thisIcon(widget: pWidget, data: pgpointer){.cdecl.} =
  iconify(get_parent_window(widget))
proc thisDeicon(widget: pWidget, data: pgpointer){.cdecl.} =
  deiconify(get_parent_window(widget))
proc thisHide(widget: pWidget, data: pgpointer){.cdecl.} =
  hide(get_parent_window(widget))
  sleep(5)
  show(get_parent_window(widget))
  
proc thisShow(widget: pWidget, data: pgpointer){.cdecl.} =
  show(get_parent_window(widget))

var isshifted: bool = false
  
proc thisMove(widget: pWidget, data: pgpointer){.cdecl.} =
  var w, h: gint
  get_size(get_parent_window(widget), Addr(w), Addr(h))
  if isshifted:
     move(get_parent_window(widget), w-10, h-10)
  else:
     move(get_parent_window(widget), w+10, h+10)
  isshifted = not isshifted
  

nim_init()
var window = window_new(gtk2.WINDOW_TOPLEVEL)
discard allow_grow(window)
set_title(window,"Window management")
var stackbox = vbox_new(TRUE, 10)
var bmax = button_new("maximize")
var bunmax = button_new("unmaximize")
var bicon = button_new("iconize")
var bdeicon = button_new("deiconize")
var bhide = button_new("hide")
var bshow = button_new("show")
var bmove = button_new("move")
var bquit = button_new("Quit")
   
pack_start(stackbox, bmax, TRUE, TRUE, 0)
pack_start(stackbox, bunmax, TRUE, TRUE, 0)
pack_start(stackbox, bicon, TRUE, TRUE, 0)
pack_start(stackbox, bdeicon, TRUE, TRUE, 0)
pack_start(stackbox, bhide, TRUE, TRUE, 0)
pack_start(stackbox, bshow, TRUE, TRUE, 0)
pack_start(stackbox, bmove, TRUE, TRUE, 0)
pack_start(stackbox, bquit, TRUE, TRUE, 0)
set_border_width(Window, 5)
add(window, stackbox)
discard signal_connect(window, "destroy",
                   SIGNAL_FUNC(thisDestroy), nil)

discard signal_connect(bicon, "clicked",
                   SIGNAL_FUNC(thisIcon), nil)
discard signal_connect(bdeicon, "clicked",
                   SIGNAL_FUNC(thisDeicon), nil)
discard signal_connect(bmax, "clicked",
                   SIGNAL_FUNC(thisMax), nil)
discard signal_connect(bunmax, "clicked",
                   SIGNAL_FUNC(thisUnmax), nil)
discard signal_connect(bhide, "clicked",
                   SIGNAL_FUNC(thisHide), nil)
discard signal_connect(bshow, "clicked",
                   SIGNAL_FUNC(thisShow), nil)
discard signal_connect(bmove, "clicked",
                   SIGNAL_FUNC(thismove), nil)                   
discard signal_connect(bquit, "clicked",
                   SIGNAL_FUNC(thisDestroy), nil)
show_all(window)
main()
```

==={{libheader|IUP}}===

```nim
import iup

# assumes you have the iup  .dll or .so installed

proc toCB(fp: proc): ICallback =
   return cast[ICallback](fp)

discard iup.open(nil,nil)

var btnRestore = button("restore","")
var btnFull = button("Full screen","")
var btnMin = button("minimize","")
var btnMax = button("maximize","")
var btnHide = button("Transparent","")
#var btnHide = button("Hide (close)","")
var btnShow = button("Show","")

var hbox = Hbox(btnRestore, btnFull, btnMax, btnMin, btnShow, btnHide, nil)
setAttribute(hbox,"MARGIN", "10x10")
setAttribute(hbox,"PADDING", "5x5")

var dlg = Dialog(hbox)
#SetAttribute(dlg, "SIZE", "100x50")

proc doFull(ih:PIhandle): cint {.cdecl.} =
    setAttribute(dlg,"FULLSCREEN","YES")
    return IUP_DEFAULT

proc doMax(ih:PIhandle): cint {.cdecl.} =
    #setAttribute(dlg,"FULLSCREEN","YES")
    setAttribute(dlg,"PLACEMENT","MAXIMIZED")
    # this is a work-around to get the dialog minimised (on win platform)
    setAttribute(dlg,"VISIBLE","YES")
    return IUP_DEFAULT

proc doMin(ih:PIhandle): cint {.cdecl.} =
    setAttribute(dlg,"PLACEMENT","MINIMIZED")
    # this is a work-around to get the dialog minimised (on win platform)
    setAttribute(dlg,"VISIBLE","YES")
    return IUP_DEFAULT

proc doRestore(ih:PIhandle): cint {.cdecl.} =
    setAttribute(dlg,"OPACITY","255")
    setAttribute(dlg,"FULLSCREEN","NO")
    setAttribute(dlg,"PLACEMENT","NORMAL")
    setAttribute(dlg,"VISIBLE","YES")
    return IUP_DEFAULT

proc doHide(ih:PIhandle): cint {.cdecl.} =
    #setAttribute(dlg,"VISIBLE","NO")
    setAttribute(dlg,"OPACITY","60")
    return IUP_DEFAULT

proc doShow(ih:PIhandle): cint {.cdecl.} =
    setAttribute(dlg,"OPACITY","255")
    setAttribute(dlg,"VISIBLE","YES")
    return IUP_DEFAULT
    
discard setCallback(btnRestore,"ACTION", toCB(doRestore))
discard setCallback(btnFull,"ACTION", toCB(doFull))
discard setCallback(btnMax,"ACTION", toCB(doMax))
discard setCallback(btnMin,"ACTION", toCB(doMin))
discard setCallback(btnShow,"ACTION", toCB(doShow))
discard setCallback(btnHide,"ACTION", toCB(doHide))

discard dlg.show()
discard mainloop()
iup.Close()
```



## Oz

We use QTk, Oz' default GUI toolkit.
QTk takes a declarative description of the GUI and from this creates objects which represent the GUI parts. So windows are represented by objects and thus have an identity.

We create two windows with a simple GUI. The user can use each window to send messages to the window or its neighboring window. (Sending messages is the same as 'calling methods' in Oz.)

We also wrap the Window objects in a procedure in order to extend their functionality. This is interesting
because it shows how to extend an object's interface even when we don't have control over object creation.


```oz
declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}

  %% The messages that can be sent to the windows.
  WindowActions =
  [hide show close
   iconify deiconify
   maximize restore
   set(minsize:minsize(width:400 height:400))
   set(minsize:minsize(width:200 height:200))
   set(geometry:geometry(x:0 y:0))
   set(geometry:geometry(x:500 y:500))
  ]

  %% Two windows, still uninitialized.
  Windows = windows(window1:_
                    window2:_)

  fun {CreateWindow}
     Message = {NewCell WindowActions.1}
     ReceiverName = {NewCell {Arity Windows}.1}
     fun {ButtonText}
        "Send"#"  "#{ValueToString @Message}#"  to  "#@ReceiverName
     end
     Button
     Desc =
     td(title:"Window Management"
        lr(listbox(init:{Arity Windows}
                   glue:nswe
                   tdscrollbar:true
                   actionh:proc {$ W}
                              ReceiverName := {GetSelected W}
                              {Button set(text:{ButtonText})}
                           end
                  )
           listbox(init:{Map WindowActions ValueToString}
                   glue:nswe
                   tdscrollbar:true
                   actionh:proc {$ A}
                              Message := {GetSelected A}
                              {Button set(text:{ButtonText})}
                           end
                  )
           glue:nswe
          )
        button(text:{ButtonText}
               glue:we
               handle:Button
               action:proc {$}
                         {Windows.@ReceiverName @Message}
                      end
              )
       )
     Window = {Extend {QTk.build Desc}}
  in
     {Window show}
     Window
  end

  %% Adds two methods to a toplevel instance.
  %% For maximize and restore we have to interact directly with Tk
  %% because that functionality is not part of the QTk library.
  fun {Extend Toplevel}
     proc {$ A}
        case A of maximize then
           {Tk.send wm(state Toplevel zoomed)}
        [] restore then
           {Tk.send wm(state Toplevel normal)}
        else
           {Toplevel A}
        end
     end
  end

  %% Returns the current entry of a listbox
  %% as an Oz value.
  fun {GetSelected LB}
     Entries = {LB get($)}
     Index = {LB get(firstselection:$)}
  in
     {Compiler.virtualStringToValue {Nth Entries Index}}
  end

  fun {ValueToString V}
     {Value.toVirtualString V 100 100}
  end
in
  {Record.forAll Windows CreateWindow}
```



## Perl

==={{libheader|Perl/Tk}}===
This is a translation of the Tcl solution for this task.

The preferred way of using Tk with perl is through the (relatively modern) Tkx module, not the (quite old) Tk module (also known as perl/tk), which my code uses.

I wrote the code using the Tk module, as that's what I had on my computer, and I was too lazy to install Tkx.  Translating from perl/tk to Tkx should be fairly trivial.


```Perl
#!perl
use strict;
use warnings;
use Tk;

my $mw;
my $win;
my $lab;

# How to open a window.
sub openWin {
	if( $win ) {
		$win->deiconify;
		$win->wm('state', 'normal');
	} else {
		eval { $win->destroy } if $win;
		$win = $mw->Toplevel;
		$win->Label(-text => "This is the window being manipulated")
			->pack(-fill => 'both', -expand => 1);
		$lab->configure(-text => "The window object is:\n$win");
	}
}

# How to close a window
sub closeWin {
	return unless $win;
	$win->destroy;
	$lab->configure(-text => '');
	undef $win;
}

# How to minimize a window
sub minimizeWin {
	return unless $win;
	$win->iconify;
}

# How to maximize a window
sub maximizeWin {
	return unless $win;
	$win->wm('state', 'zoomed');
	eval { $win->wmAttribute(-zoomed => 1) }; # Hack for X11
}

# How to move a window
sub moveWin {
	return unless $win;
	my ($x, $y) = $win->geometry() =~ /\+(\d+)\+(\d+)\z/ or die;
	$_ += 10 for $x, $y;
	$win->geometry("+$x+$y");
}

# How to resize a window
sub resizeWin {
	return unless $win;
	my ($w, $h) = $win->geometry() =~ /^(\d+)x(\d+)/ or die;
	$_ += 10 for $w, $h;
	$win->geometry($w . "x" . $h);
}

$mw = MainWindow->new;
for my $label0 ($mw->Label(-text => 'Window handle:')) {
	$lab = $mw->Label(-text => '');
	$label0->grid($lab);
}

my @binit = ('Open/Restore' => \&openWin, Close => \&closeWin,
	Minimize => \&minimizeWin, Maximize => \&maximizeWin,
	Move => \&moveWin, Resize => \&resizeWin);

while( my ($text, $callback) = splice @binit, 0, 2 ) {
	$mw->Button(-text => $text, -command => $callback)->grid('-');
}

MainLoop();

__END__

```


In the Tcl code I translated from, the second label of the main window had a textvariable attribute.  For some reason, this didn't work correctly for me, either due to a bug in Tk.pm, or some other reason.  Because of that, I kept around a handle to that second label ($lab), and called configure on it when I needed it's text to change.

Doubtless some more modern Tk binding (such as Tkx, or perhaps Tcl::Tk) would handle that better.


## Perl 6

This are generic window handling routines. They work for any window managed by the X11 display server, not just windows created by the program.


```perl6
use X11::libxdo;

my $xdo = Xdo.new;

say 'Visible windows:';
printf "Class: %-21s ID#: %10d  pid: %5d  Name: %s\n", $_<class ID pid name>
    for $xdo.get-windows.sort(+*.key)».value;
sleep 2;

my $id = $xdo.get-active-window;

my ($w,  $h ) = $xdo.get-window-size( $id );
my ($wx, $wy) = $xdo.get-window-location( $id );
my ($dw, $dh) = $xdo.get-desktop-dimensions( 0 );

$xdo.move-window( $id, 150, 150 );

$xdo.set-window-size( $id, 350, 350, 0 );

sleep .25;

for flat 1 .. $dw - 350, $dw - 350, {$_ - 1} … 1 -> $mx { #
    my $my = (($mx / $dw * τ).sin * 500).abs.Int;
    $xdo.move-window( $id, $mx, $my );
    $xdo.activate-window($id);
}

sleep .25;

$xdo.move-window( $id, 150, 150 );

my $dx = $dw - 300;
my $dy = $dh - 300;

$xdo.set-window-size( $id, $dx, $dy, 0 );

sleep .25;

my $s = -1;

loop {
    $dx += $s * ($dw / 200).ceiling;
    $dy += $s * ($dh / 200).ceiling;
    $xdo.set-window-size( $id, $dx, $dy, 0 );
    $xdo.activate-window($id);
    sleep .005;
    $s *= -1 if $dy < 200;
    last if $dx >= $dw;
}

sleep .25;

$xdo.set-window-size( $id, $w, $h, 0 );
$xdo.move-window( $id, $wx, $wy );
$xdo.activate-window($id);

sleep .25;

$xdo.minimize($id);
$xdo.activate-window($id);
sleep 1;
$xdo.raise-window($id);
sleep .25;

```



## Phix

```Phix
--
-- demo\rosetta\Window_management.exw
--
include pGUI.e

Ihandle dlg

function doFull(Ihandle /*ih*/)
    IupSetAttribute(dlg,"FULLSCREEN","YES")
    return IUP_DEFAULT
end function
 
function doMax(Ihandle /*ih*/)
    IupSetAttribute(dlg,"PLACEMENT","MAXIMIZED")
    -- this is a work-around to get the dialog minimised (on win platform)
    IupSetAttribute(dlg,"VISIBLE","YES")
    return IUP_DEFAULT
end function
 
function doMin(Ihandle /*ih*/)
    IupSetAttribute(dlg,"PLACEMENT","MINIMIZED")
    -- this is a work-around to get the dialog minimised (on win platform)
    IupSetAttribute(dlg,"VISIBLE","YES")
    return IUP_DEFAULT
end function
 
function doRestore(Ihandle /*ih*/)
    IupSetAttribute(dlg,"OPACITY","255")
    IupSetAttribute(dlg,"FULLSCREEN","NO")
    IupSetAttribute(dlg,"PLACEMENT","NORMAL")
    IupSetAttribute(dlg,"VISIBLE","YES")
    return IUP_DEFAULT
end function
 
function doDim(Ihandle /*ih*/)
    IupSetAttribute(dlg,"OPACITY","60")
    return IUP_DEFAULT
end function
 
function doShow(Ihandle /*ih*/)
    IupSetAttribute(dlg,"OPACITY","255")
    return IUP_DEFAULT
end function

function doMove(Ihandle /*ih*/)
    integer {x,y} = IupGetIntInt(dlg,"SCREENPOSITION")
    integer shift = iff(IupGetInt(NULL,"SHIFTKEY")?-10,+10)
    IupShowXY(dlg,x+shift,y+shift)
    return IUP_DEFAULT
end function
 
procedure main()
    IupOpen()
 
    Ihandle hbox = IupHbox({IupButton("restore",    Icallback("doRestore")),
                            IupButton("full screen",Icallback("doFull")),
                            IupButton("maximize",   Icallback("doMax")),
                            IupButton("minimize",   Icallback("doMin")),
                            IupButton("dim",        Icallback("doDim")),
                            IupButton("show",       Icallback("doShow")),
                            IupButton("move",       Icallback("doMove"))})
    IupSetAttribute(hbox,"MARGIN", "10x10")
    IupSetAttribute(hbox,"PADDING", "5x5")
 
    dlg = IupDialog(hbox)
    IupSetAttribute(dlg,"OPACITY","255")
 
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure
main()
```



## PicoLisp

The following works on ErsatzLisp, the Java version of PicoLisp.

```PicoLisp
$ ersatz/pil +
: (setq
   JFrame         "javax.swing.JFrame"
   MAXIMIZED_BOTH (java (public JFrame 'MAXIMIZED_BOTH))
   ICONIFIED      (java (public JFrame 'ICONIFIED))
   Win            (java JFrame T "Window") )
-> $JFrame

# Compare for equality
: (== Win Win)
-> T

# Set window visible
(java Win 'setLocation 100 100)
(java Win 'setSize 400 300)
(java Win 'setVisible T)

# Hide window
(java Win 'hide)

# Show again
(java Win 'setVisible T)

# Move window
(java Win 'setLocation 200 200)

# Iconify window
(java Win 'setExtendedState
   (| (java (java Win 'getExtendedState)) ICONIFIED) )

# De-conify window
(java Win 'setExtendedState
   (& (java (java Win 'getExtendedState)) (x| (hex "FFFFFFFF") ICONIFIED)) )

# Maximize window
(java Win 'setExtendedState
   (| (java (java Win 'getExtendedState)) MAXIMIZED_BOTH) )

# Close window
(java Win 'dispose)
```



## PureBasic


```PureBasic
;- Create a linked list to store created windows.
NewList Windows()
Define i, j, dh, dw, flags, err$, x, y

;- Used sub-procedure to simplify the error handling
Procedure HandleError(Result, Text.s,ErrorLine=0,ExitCode=0)
  If Not Result
    MessageRequester("Error",Text)
    End ExitCode
  EndIf
  ProcedureReturn Result
EndProcedure

;- Window handling procedures
Procedure Minimize(window)
  SetWindowState(window,#PB_Window_Minimize)
EndProcedure

Procedure Normalize(window)
  SetWindowState(window,#PB_Window_Normal)
EndProcedure

;- Get enviroment data
HandleError(ExamineDesktops(),  "Failed to examine you Desktop.") 
dh=HandleError(DesktopHeight(0),"Could not retrieve DesktopHight")/3
dw=HandleError(DesktopWidth(0), "Could not retrieve DesktopWidth")/3

;- Now, creating 9 windows
flags=#PB_Window_SystemMenu 
err$="Failed to open Window"
For i=0 To 8
  j=HandleError(OpenWindow(#PB_Any,i*10,i*10+30,10,10,Str(i),flags),err$)
  SmartWindowRefresh(j, 1) 
  AddElement(Windows())
  Windows()=j
Next i
Delay(1000)

;- Call a sub-routine for each Window stored in the list.
ForEach Windows()
  Minimize(Windows())
Next
Delay(1000)
;- and again
ForEach Windows()
  Normalize(Windows())
Next
Delay(1000)

;- Spread them evenly
ForEach Windows()
  ResizeWindow(Windows(),x*dw,y*dh,dw-15,dh-35)
  x+1
  If x>2
    x=0: y+1
  EndIf
Next
Delay(2000)

End
```



## Python

Using the tkinter GUI:

```Python

from tkinter import *
import tkinter.messagebox

def maximise():
	"""get screenwidth and screenheight, and resize window to that size. 
	Also move to 0,0"""
	root.geometry("{}x{}+{}+{}".format(root.winfo_screenwidth(), root.winfo_screenheight(), 0, 0))
	
def minimise():
	"""Iconify window to the taskbar. When reopened, the window is 
	unfortunately restored to its original state, NOT its most recent state."""
	root.iconify()
	
def delete():
	"""create a modal dialog. If answer is "OK", quit the program"""
	if tkinter.messagebox.askokcancel("OK/Cancel","Are you sure?"):
		root.quit()
	
root = Tk()

mx=Button(root,text="maximise",command=maximise)
mx.grid()
mx.bind(maximise)

mn=Button(root,text="minimise",command=minimise)
mn.grid()
mn.bind(minimise)

#catch exit events, including "X" on title bar.
root.protocol("WM_DELETE_WINDOW",delete)

mainloop()

```



## Racket


```racket

#lang racket/gui

(define (say . xs) (printf ">>> ~a\n" (apply ~a xs)) (flush-output))

(define frame (new frame% [label "Demo"] [width 400] [height 400]))
(say "frame = " frame) ; plain value

(say 'Show)     (send frame show #t)      (sleep 1)
(say 'Hide)     (send frame show #f)      (sleep 1)
(say 'Show)     (send frame show #t)      (sleep 1)
(say 'Minimize) (send frame iconize #t)   (sleep 1)
(say 'Restore)  (send frame iconize #f)   (sleep 1)
(say 'Maximize) (send frame maximize #t)  (sleep 1)
(say 'Restore)  (send frame maximize #f)  (sleep 1)
(say 'Move)     (send frame move 100 100) (sleep 1)
(say 'Resize)   (send frame resize 100 100) (sleep 1)
(say 'Close)    (send frame show #f) (sleep 1) ; that's how we close a window

```





## Ring


```ring


Load "guilib.ring"

/*
 +--------------------------------------------------------------------------
 +        Program Name : ScreenDrawOnReSize.ring
 +        Date         : 2016.06.16
 +        Author       : Bert Mariani
 +        Purpose      : Re-Draw Chart after ReSize or move
 +--------------------------------------------------------------------------
*/


###-------------------------------
### DRAW CHART  size 1000 x 1000
###
    
###------------------------------

### Window Size
    WinLeft   = 80                  ### 80    Window position on screen
    WinTop    = 80                  ### 80    Window position on screen
    WinWidth  = 1000                ### 1000  Window Size - Horizontal-X WinWidth
    WinHeight = 750                 ### 750   Window Size - Vertical-Y WinHeight
    WinRight  = WinLeft + WinWidth  ### 1080
    WinBottom = WinTop  + WinHeight ### 830
             
### Label Box Size           
    BoxLeft   = 40                  ###  Start corner   Label1 Box Start Position inside WIN1
    BoxTop    = 40                  ###  Start corner 
    BoxWidth  = WinWidth  -80       ###  End   corner   Label1 Box Size
    BoxHeight = WinHeight -80       ###  End   corner  

###----------------------------    
   

New qapp {
        win1 = new qwidget() {
        
                ### Position and Size of WINDOW on the Screen
                setwindowtitle("DrawChart using QPainter")
                setgeometry( WinLeft, WinTop, WinWidth, WinHeight)
                
                win1{ setwindowtitle("Initial Window Position: " +" L " + WinLeft +" T " + WinTop +" Width" + width() +" Height " +  height() ) }

                ### ReSizeEvent ... Call WhereAreWe function
                myfilter = new qallevents(win1)
                myfilter.setResizeEvent("WhereAreWe()")
                installeventfilter(myfilter)
                
                ### Draw within this BOX
                label1 = new qlabel(win1) {
                        setgeometry(BoxLeft, BoxTop, BoxWidth, BoxHeight)
                        settext("We are Here")
                }

                
                ### Button Position and Size ... Call DRAW function
                new qpushbutton(win1) {
                        setgeometry( 30, 30, 80, 20)
                        settext("Draw")
                        setclickevent("Draw()")
                }

                ###---------------

                show()
        }
        
    exec()
}


###-----------------
### FUNCTION Draw
###-----------------

Func WhereAreWe
        Rec = win1.framegeometry()
    
        WinWidth  = win1.width()            ### 1000 Current Values 
        WinHeight = win1.height()           ### 750 
        
        WinLeft   = Rec.left() +8           ### <<< QT FIX because of Win Title
        WinTop    = Rec.top()  +30          ### <<< QT FIX because of Win Title 
        WinRight  = Rec.right()
        WinBottom = Rec.bottom()

        BoxWidth  = WinWidth  -80           ### 950
        BoxHeight = WinHeight -80           ### 700

        win1{ setwindowtitle("Window ReSize: Win " +  WinWidth + "x" + WinHeight + " --- Box " + BoxWidth  + "x" + BoxHeight  + 
                              " --- LT " +  WinLeft + "-"   + WinTop  + " --- RB " + WinRight + "-" + WinBottom      ) }
        
        See "We Are Here - setResizeEvent - " 
        See " Win "  + WinWidth  + "x" + WinHeight + " --- Box  "  + BoxWidth + "x" + BoxHeight  
        See " --- LT " + Winleft   + "-"   + WinTop    + " --- RB " + WinRight + "-"   + WinBottom +nl
        
          win1.setgeometry( WinLeft, WinTop, WinWidth, WinHeight )
        label1.setgeometry( BoxLeft, BoxTop, BoxWidth, BoxHeight )
        
    
return

Func Draw

        win1{ setwindowtitle("Draw Position: Win " +  WinWidth + "x" + WinHeight + " --- Box " + BoxWidth  + "x" + BoxHeight  + 
                              " --- LT " +  WinLeft + "-"   + WinTop  + " --- RB " + WinRight + "-" + WinBottom      ) }
                              
        See "Draw Position: " +  WinWidth + "x" + WinHeight + " --- Box " + BoxWidth  + "x" + BoxHeight  + 
                              " --- LT " +  WinLeft + "-"   + WinTop  + " --- RB " + WinRight + "-" + WinBottom  + nl
                              
  
  #     ##-----------------------------
        ### PEN Colors
        
        p1 = new qpicture()

            colorBlue = new qcolor() { setrgb(0,    0,255,255) }
            penBlue   = new qpen() { setcolor(colorBlue)  setwidth(1) }


        ###-----------------------
        ### PAINT the Chart

        new qpainter() {
                begin(p1)
                setpen(penBlue)

                ###---------------------
                ### Draw Line Chart

                drawline(        1 ,         1 , BoxWidth ,         1 )     ### WinTop line horizonal
                drawline(        1 ,         1 ,        1 , BoxHeight )     ### WinLeft Line vetical
                
                drawline(        1 , BoxHeight , BoxWidth , BoxHeight )     ### Bottom Line horizontal
                drawline( BoxWidth ,         1 , BoxWidth , BoxHeight )     ### WinRight Line vertical
                
                drawline( BoxWidth / 2 ,             1 , BoxWidth / 2 ,   BoxHeight     )    ### Central vertical   
                drawline(            1 , BoxHeight / 2 , BoxWidth     ,   BoxHeight / 2 )    ### Central horizontal

                                      
                ###--------------------------------------------------


                endpaint()
        }
        
        
        label1 { setpicture(p1) show() }
        
return
###--------------------------------------------


```




## Tcl

```tcl
package require Tk

# How to open a window
proc openWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        # Already existing; just reset
        wm deiconify $win
        wm state $win normal
        return
    }
    catch {destroy $win} ;# Squelch the old one
    set win [toplevel .t]
    pack [label $win.label -text "This is the window being manipulated"] \
        -fill both -expand 1
}
# How to close a window
proc closeWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        destroy $win
    }
}
# How to minimize a window
proc minimizeWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        wm state $win iconic
    }
}
# How to maximize a window
proc maximizeWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        wm state $win zoomed
        catch {wm attribute $win -zoomed 1} ;# Hack for X11
    }
}
# How to move a window
proc moveWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        scan [wm geometry $win] "%dx%d+%d+%d" width height x y
        wm geometry $win +[incr x 10]+[incr y 10]
    }
}
# How to resize a window
proc resizeWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        scan [wm geometry $win] "%dx%d+%d+%d" width height x y
        wm geometry $win [incr width 10]x[incr height 10]
    }
}

grid [label .l   -text "Window handle:"] [label .l2 -textvariable win]
grid [button .b1 -text "Open/Reset" -command openWin] -
grid [button .b2 -text "Close"      -command closeWin] -
grid [button .b3 -text "Minimize"   -command minimizeWin] -
grid [button .b4 -text "Maximize"   -command maximizeWin] -
grid [button .b5 -text "Move"       -command moveWin] -
grid [button .b6 -text "Resize"     -command resizeWin] -
```

