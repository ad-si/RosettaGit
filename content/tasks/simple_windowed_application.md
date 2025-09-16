+++
title = "Simple windowed application"
description = ""
date = 2019-07-02T05:14:55Z
aliases = []
[extra]
id = 1952
[taxonomies]
categories = ["task", "GUI"]
tags = []
languages = [
  "ada",
  "apl",
  "autohotkey",
  "autoit",
  "b4j",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "e",
  "echolisp",
  "elena",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "freebasic",
  "gambas",
  "gastona",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "idl",
  "j",
  "java",
  "javafx_script",
  "julia",
  "kotlin",
  "lambdatalk",
  "liberty_basic",
  "lingo",
  "logo",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "maxscript",
  "nanoquery",
  "nim",
  "ocaml",
  "oorexx",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pike",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rapidq",
  "rebol",
  "ring",
  "ruby",
  "run_basic",
  "scala",
  "sidef",
  "smalltalk",
  "tcl",
  "unicon",
  "vedit_macro_language",
  "visual_basic",
  "web_68",
  "xpl0",
  "yorick",
]
+++

{{task|GUI}}[[Category:Basic language learning]]{{requires|Graphics}} [[Category:Simple]]

## Task

Create a window that has:
::#   a label that says   "There have been no clicks yet"
::#   a button that says   "click me"


Upon clicking the button with the mouse, the label should change and show the number of times the button has been clicked.





## Ada

The following solution is based on bindings to GTK+.
Ada as a language does not provide standard GUI.
Apart from GtkAda, there exist numerous other GUI bindings and libraries:
CLAW, AdaGLUT, GWindow, JEWL, win32ada, QtAda etc.

```ada
with Gdk.Event;   use Gdk.Event;
with Gtk.Button;  use Gtk.Button;
with Gtk.Label;   use Gtk.Label;
with Gtk.Window;  use Gtk.Window;
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Table;   use Gtk.Table;

with Gtk.Handlers;
with Gtk.Main;

procedure Simple_Windowed_Application is
   Window : Gtk_Window;
   Grid   : Gtk_Table;
   Button : Gtk_Button;
   Label  : Gtk_Label;
   Count  : Natural := 0;

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
   begin
     Count := Count + 1;
     Set_Text (Label, "The button clicks:" & Natural'Image (Count));
   end Clicked;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk_New (Grid, 1, 2, False);
   Add (Window, Grid);
   Gtk_New (Label, "There have been no clicks yet");
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
end Simple_Windowed_Application;
```



## APL

```APL
∇ WindowedApplication

⍝ define a form with a label and a button
'Frm'⎕WC'Form' 'Clicks' (40 35) (10 15)
'Lbl'Frm.⎕WC'Label' 'There have been no clicks yet.' (10 10)
'Btn'Frm.⎕WC'Button' 'Click Me' (35 35) (25 25) ('Event' 'Select' 'Click')

⍝ callback function
Frm.Clicks←0
Frm.Click←{
    Clicks+←1
    p0←(1+Clicks=1)⊃'have' 'has'
    p1←(1+Clicks=1)⊃'clicks' 'click'
    Lbl.Value←'There ',p0,' been ',(⍕Clicks),' ',p1,'.'
}

∇
```



## AutoHotkey


```Autohotkey
; Create simple windowed application
   Gui, Add, Text, vTextCtl, There have been no clicks yet ; add a Text-Control
   Gui, Add, Button, gButtonClick xm, click me ; add a Button-Control
   Gui, Show, , Simple windowed application ; show the Window
Return ; end of the auto-execute section

ButtonClick: ; the subroutine executed each time the Button-Control is clicked
   count++ ; increment the click-counting var
   GuiControl, , TextCtl, %count% ; update the Text-Control with the click-counting var
Return ; end of the subroutine

GuiClose: ; the subroutine executed when the Window is closed
   ExitApp ; exit this process
Return
```



## AutoIt


```AutoIt

#include <ButtonConstants.au3>
#include <GUIConstantsEx.au3>
#include <StaticConstants.au3>
#include <WindowsConstants.au3>
#Region ### START Koda GUI section ###
Local $GUI = GUICreate("Clicks", 280, 50, (@DesktopWidth - 280) / 2, (@DesktopHeight - 50) / 2)
Local $lblClicks = GUICtrlCreateLabel("There have been no clicks yet", 0, 0, 278, 20, $SS_CENTER)
Local $btnClicks = GUICtrlCreateButton("CLICK ME", 104, 25, 75, 25)
GUISetState(@SW_SHOW)
#EndRegion ### END Koda GUI section ###

Local $counter = 0

While 1
	$nMsg = GUIGetMsg()
	Switch $nMsg
		Case $GUI_EVENT_CLOSE
			Exit

		Case $btnClicks
			$counter += 1
			GUICtrlSetData($lblClicks, "Times clicked: " & $counter)
	EndSwitch
WEnd

```



## B4J


```freebasic

#Region  Project Attributes
	#MainFormWidth: 593
	#MainFormHeight: 179
#End Region

Sub Process_Globals
	Private fx As JFX
	Private MainForm As Form
	Private btnClickMe As Button
	Private lblClickCounter As Label
	Private nClicks As Int = 0
	Private aPlurals() As Object = Array As Object(Array As String("has","click"),Array As String("have","clicks"))
End Sub

Sub AppStart (Form1 As Form, Args() As String)
	MainForm = Form1
	MainForm.RootPane.LoadLayout("Layout1") 'Load the layout file.
	MainForm.Show
End Sub

Sub btnClickMe_Action
	nClicks = nClicks + 1
	Dim aPlural() As Object = aPlurals(IIF(nClicks=1,0,1))
	lblClickCounter.Text = "There " & aPlural(0) & " been " & (nClicks) & " " & aPlural(1) & " so far."
End Sub

Sub IIF(test As Boolean, trueVal As Object, falseVal As Object) As Object
	If test Then
		Return trueVal
	Else
		Return falseVal
	End If
End Sub
```


Layout1.fxml (as B4J uses JavaFX's Scene Builder)

```cfm

<?xml version="1.0" encoding="UTF-8"?>

<?import javafx.scene.text.*?>
<?import java.lang.*?>
<?import java.util.*?>
<?import javafx.collections.*?>
<?import javafx.scene.control.*?>
<?import javafx.scene.image.*?>
<?import javafx.scene.layout.*?>
<?import javafx.scene.paint.*?>
<?import javafx.scene.web.*?>


<AnchorPane id="paneMain" maxHeight="-Infinity" maxWidth="-Infinity" minHeight="-Infinity" minWidth="-Infinity" prefHeight="179.0" prefWidth="593.0" xmlns="http://javafx.com/javafx/8" xmlns:fx="http://javafx.com/fxml/1">
   <children>
      <Button id="btnClickMe" layoutX="254.0" layoutY="90.0" mnemonicParsing="false" text="Click Me">
         <font>
            <Font size="20.0" />
         </font>
      </Button>
      <Label id="lblClickCounter" layoutX="162.0" layoutY="31.0" text="There have been no clicks as yet.">
         <font>
            <Font size="20.0" />
         </font>
      </Label>
   </children>
</AnchorPane>

```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"WINLIB2"
      INSTALL @lib$+"WINLIB5"

      window% = FN_newdialog("Rosetta Code", 100, 100, 120, 52, 8, 1000)
      PROC_static(window%, "There have been no clicks yet", 100, 10, 10, 100, 14, 0)
      PROC_pushbutton(window%, "Click me", FN_setproc(PROCclick), 40, 30, 40, 16, 0)
      PROC_showdialog(window%)

      REPEAT
        WAIT 1
      UNTIL !window% = 0
      QUIT

      DEF PROCclick
      PRIVATE clicks%
      clicks% += 1
      SYS "SetDlgItemText", !window%, 100, "Number of clicks = " + STR$(clicks%)
      ENDPROC
```



## C

```c
#include <stdio.h>
#include <gtk/gtk.h>

const gchar *clickme = "Click Me";
guint counter = 0;

#define MAXLEN 64
void clickedme(GtkButton *o, gpointer d)
{
    GtkLabel *l = GTK_LABEL(d);
    char nt[MAXLEN];

    counter++;
    snprintf(nt, MAXLEN, "You clicked me %d times", counter);
    gtk_label_set_text(l, nt);
}

int main(int argc, char **argv)
{
    GtkWindow *win;
    GtkButton *button;
    GtkLabel *label;
    GtkVBox *vbox;

    gtk_init(&argc, &argv);
    win = (GtkWindow*)gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(win, clickme);
    button = (GtkButton*)gtk_button_new_with_label(clickme);
    label = (GtkLabel*)gtk_label_new("There have been no clicks yet");
    gtk_label_set_single_line_mode(label, TRUE);
    vbox = (GtkVBox*)gtk_vbox_new(TRUE, 1);
    gtk_container_add(GTK_CONTAINER(vbox), GTK_WIDGET(label));
    gtk_container_add(GTK_CONTAINER(vbox), GTK_WIDGET(button));
    gtk_container_add(GTK_CONTAINER(win), GTK_WIDGET(vbox));
    g_signal_connect(G_OBJECT(win), "delete-event", (GCallback)gtk_main_quit, NULL);
    g_signal_connect(G_OBJECT(button), "clicked", (GCallback)clickedme, label);
    gtk_widget_show_all(GTK_WIDGET(win));
    gtk_main();
    return 0;
}
```



## C++

{{libheader|Qt}} 4.4 with source files as shown , built from a Makefile generated by the Qt tool qmake

### clickcounter.h


```cpp
#ifndef CLICKCOUNTER_H
#define CLICKCOUNTER_H

#include <QWidget>
class QLabel ;
class QPushButton ;
class QVBoxLayout ;

class Counter : public QWidget {
    Q_OBJECT
public :
   Counter( QWidget * parent = 0 ) ;
private :
   int number ;
   QLabel *countLabel ;
   QPushButton *clicker ;
   QVBoxLayout *myLayout ;
private slots :
   void countClicks( ) ;
} ;
#endif
```


### clickcounter.cpp


```cpp
#include <QPushButton>
#include <QLabel>
#include <QVBoxLayout>
#include "clickcounter.h"

Counter::Counter( QWidget * parent ) : QWidget( parent ) {
   number = 0 ;
   countLabel = new QLabel( "There have been no clicks yet!" ) ;
   clicker = new QPushButton( "click me" ) ;
   connect ( clicker , SIGNAL( clicked( ) ) , this , SLOT( countClicks( ) ) ) ;
   myLayout = new QVBoxLayout ;
   myLayout->addWidget( countLabel ) ;
   myLayout->addWidget( clicker ) ;
   setLayout( myLayout ) ;
}

void Counter::countClicks( ) {
   number++ ;
   countLabel->setText( QString( "The button has been clicked %1 times!").arg( number ) ) ;
}
```


### main.cpp


```cpp
#include <QApplication>
#include "clickcounter.h"

int main( int argc , char *argv[ ] ) {
   QApplication app( argc , argv ) ;
   Counter counter ;
   counter.show( ) ;
   return app.exec( ) ;
}
```


## C#


```c#
using System.Windows.Forms;

class RosettaForm : Form
{
    RosettaForm()
    {
        var clickCount = 0;

        var label = new Label();
        label.Text = "There have been no clicks yet.";
        label.Dock = DockStyle.Top;
        Controls.Add(label);

        var button = new Button();
        button.Text = "Click Me";
        button.Dock = DockStyle.Bottom;
        button.Click += delegate
                        {
                            clickCount++;
                            label.Text = "Number of clicks: " + clickCount + ".";
                        };
        Controls.Add(button);
    }

    static void Main()
    {
        Application.Run(new RosettaForm());
    }
}

```


## Clojure


```clojure
(ns counter-window
  (:import (javax.swing JFrame JLabel JButton)))

(defmacro on-action [component event & body]
  `(. ~component addActionListener
      (proxy [java.awt.event.ActionListener] []
        (actionPerformed [~event] ~@body))))

(defn counter-app []
  (let [counter (atom 0)
        label (JLabel. "There have been no clicks yet")
        button (doto (JButton. "Click me!")
                 (on-action evnt
                   (.setText label
                      (str "Counter: " (swap! counter inc)))))
        panel (doto (JPanel.)
                (.setOpaque true)
                (.add label)
                (.add button))]
    (doto (JFrame. "Counter App")
      (.setContentPane panel)
      (.setSize 300 100)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))))

```


## Common Lisp


```lisp
(defpackage #:rcswa
  (:use #:clim #:clim-lisp))
(in-package #:rcswa)
```


This version uses CLIM's command system:


```lisp
(define-application-frame simple-windowed-application ()
  ((clicks :initform 0
           :accessor clicks-of))
  (:menu-bar t)
  (:pane
   (make-pane 'application-pane
              :width '(40 :character)
              :height '(3 :character)
              :display-time :command-loop
              :display-function
                (lambda (pane stream)
                  (declare (ignore pane))
                  (format stream "~[There have been no clicks yet.~
                                    ~:;~:*There ~[have~;has~:;have~]~:* been ~R click~:P.~]"
                          (clicks-of *application-frame*))))))

(define-simple-windowed-application-command (com-click-me :menu t)
  ()
  (incf (clicks-of *application-frame*)))
```


This version uses an explicit pushbutton gadget, and may be used if more direct control over the UI layout and behavior is needed:


```lisp
(define-application-frame simple-windowed-application ()
  ((clicks :initform 0
           :accessor clicks-of))
  (:panes
   (the-label :application
              :width '(40 :character)
              :height '(3 :character)
              :display-time t
              :display-function
                (lambda (pane stream)
                  (declare (ignore pane))
                  (format stream "~[There have been no clicks yet.~
                                    ~:;~:*There ~[have~;has~:;have~]~:* been ~R click~:P.~]"
                          (clicks-of *application-frame*))))
   (the-button :push-button
               :label "Click Me"
               :activate-callback
                 (lambda (button)
                   (declare (ignore button))
                   (incf (clicks-of *application-frame*))
                   (redisplay-frame-pane *application-frame*
                                         (find-pane-named *application-frame* 'the-label)
                                         :force-p t))))
  (:layouts (default
             (vertically (:equalize-width nil :align-x :center)
               the-label
               (spacing (:thickness 10) the-button)))))
```


In either case, the window is opened with:


```lisp
(run-frame-top-level (make-application-frame 'simple-windowed-application))
```



## D


### DFL

```d
module winapp ;
import dfl.all ;
import std.string ;

class MainForm: Form {
  Label label ;
  Button button ;
  this() {
    width = 240 ;
    with(label = new Label) {
      text = "There have been no clicks yet" ;
      dock = DockStyle.TOP ;
      parent = this ;
    }
    with(button = new Button) {
      dock = DockStyle.BOTTOM ;
      text = "Click Me" ;
      parent = this ;
      click ~= &onClickButton ;
    }
    height = label.height + button.height + 36 ;
  }
  private void onClickButton(Object sender, EventArgs ea) {
    static int count = 0 ;
    label.text = "You had been clicked me " ~ std.string.toString(++count) ~ " times." ;
  }
}

void main() {
    Application.run(new MainForm);
}
```



### Hybrid

Hybrid uses config files to describe GUI layout.
SimpleWindow.cfg:
 import "themes/default.cfg"

 new FramedTopLevelWindow main {
 	frame.text = "Simple Window";

 	new Label label {
 		text = "There have been no clicks yet";
 	}

 	new Button button {
 		text = "Click me";
 		size = 201 20;
 	}
 }
SimpleWindow.d:

```d
module SimpleWindow;
import tango.text.convert.Integer;
import tango.core.Thread; // For Thread.yield

import xf.hybrid.Hybrid; //For widgets and general API
import xf.hybrid.backend.GL; // For OpenGL Renderer

void main() {
	//load config file
	scope cfg = loadHybridConfig(`./SimpleWindow.cfg`);
	scope renderer = new Renderer;
	auto counter = 0;

	bool programRunning = true;
	while (programRunning) {
		// Tell Hybrid what config to use
		gui.begin(cfg);
		// Exit program if user clicks the Close button
		if (gui().getProperty!(bool)("main.frame.closeClicked")) {
			programRunning = false;
		}
		// Update text on the label
		if (counter != 0)
			Label("main.label").text = toString(counter);
		// Increment counter if the button has been clicked
		if (Button("main.button").clicked) {
			counter++;
		}
		// Finalize. Prepare to render
		gui.end();
		// Render window using OpenGL Renderer
		gui.render(renderer);

		Thread.yield();
	}
}
```



## Delphi

Creating a "basic windowed application" in Delphi is quite simple. Start up the IDE and the default application is a single windowed app. This will have two files, the main project file "project1" and a unit file, "unit1". What I a going to do here is going to be somewhat different. I will do it all dynamically from the main "program file". Create this in any text editor and you can then either open it in the IDE to build it, or invoke the command line compiler. There is quite a bit going on behind the scenes with the VCL and the RTL.

Filename = SingleWinApp.dpr

'''NOTE:''' The project name here must match the name of the file.


```delphi
-- begin file --

   Program SingleWinApp ;

   // This is the equivalent of the C #include
   Uses Forms, Windows, Messages, Classes, Graphics, Controls, StdCtrls ;


   type

     // The only reason for this declaration is to allow the connection of the
     // on click method to the forms button object. This class declaration adds
     // a procedure.

     TMainForm class(tform)
       Procedure AddClicks(sender : tObject);
     end;


   // Use these globals.
   var

     MainForm : tForm ;
     aLabel   : tLabel ;
     aButton  : tButton ;
     i        : integer = 0 ;


    // This is the Method call that we connect to the button object
    // to start counting the clicks.
    Procedure tMainForm.AddClicks(sender :tObject)
    begin
      inc(i);
      aLabel.Caption := IntToStr(i) + ' Clicks since startup' ;
    end;


    Begin
      // Do all the behind the scenes stuff that sets up the Windows environment
      Application.Initialize ;

      // Create the form

      // Forms can either be created with an owner, like I have done here, or with
      // the owner set to Nil. In pascal (all versions of Borland) '''NIL''' is a
      // reserved, (the equivalent of '''NULL''' in Ansi C) word and un-sets any pointer
      // variable. Setting the owner to the application object will ensure that the form is
      // freed by the application object when the application shuts down. If I had set
      // the owner to NIL then i would have had to make sure I freed the form explicitly
      // or it would have been orphaned, thus creating a memory leak.

      // I must direct your attention to the CreateNew constructor.  This is
      // a non standard usage.  Normally the constructor Create() will call this
      // as part of the initialization routine for the form. Normally as you drop
      // various components on a form in deign mode, a DFM file is created with
      // all the various initial states of the controls. This bypasses the
      // DFM file altogether although all components AND the form are created
      // with default values. (see the Delphi help file).

      MainForm          := tMainForm.CreateNew(Application);
      MainForm.Parent   := Application ;
      MainForm.Position := poScreenCenter ;
      MainForm.Caption  := 'Single Window Application' ;

      // Create the Label, set its owner as MaiaForm
      aLabel          := tLabel.Create(mainForm);
      aLabel.Parent   := MainForm;
      aLabel.Caption  := IntToStr(i) + ' Clicks since startup' ;
      aLabel.Left     := 20 ;
      aLabel.Top      := MainForm.ClientRect.Bottom div 2 ;

      // Create the button, set its owner to MainForm
      aButton         := tButton.Create(MainForm);
      aButton.Parent  := MainForm ;
      aButton.Caption := 'Click Me!';
      aButton.Left    := (MainForm.ClientRect.Right div 2)-(aButton.Width div 2 );
      aButton.Top     := MainForm.ClientRect.Bottom - aButton.Height - 10 ;
      aButton.OnClick := AddClicks ;

      // Show the main form, Modaly. The ONLY reason to do this is because in this
      // demonstration if you only call the SHOW method, the form will appear and
      // disappear in a split second.
      MainForm.ShowModal ;

      Application.Run ;

   end. // Program
```



## E

```e
when (currentVat.morphInto("awt")) -> {
    var clicks := 0
    def w := <swing:makeJFrame>("Rosetta Code 'Simple Windowed Application'")
    w.setContentPane(JPanel`
        ${def l := <swing:makeJLabel>("There have been no clicks yet.")} $\
            ${def b := <swing:makeJButton>("Click Me")}
    `)
    b.addActionListener(def _ {
        to actionPerformed(_) {
            clicks += 1
            l.setText(`Number of clicks: $clicks`)
        }
    })
    w.pack()
    w.show()
}
```



## EchoLisp

UI elements are HTML DOM Nodes. '''ui-add''' adds an element to the UI. '''ui-on-event''' adds a (Lisp) event handler to an element.

```scheme

(define (ui-add-button text) ;; helper
    (define b (ui-create-element "button" '((type "button"))))
    (ui-set-html b text)
    (ui-add b))

(define (panel )
    (ui-clear)
    (define *clicks* 0)
    (define text (ui-create-element "span" '((style "font-weight:bold"))))
    (ui-add text)
    (ui-set-html text "No click yet")

    (define btn (ui-add-button "Click-me"))
    (define (count-clicks elem)
        (++ *clicks*)
        (ui-set-html text *clicks*))
    (ui-on-click btn count-clicks)

    (stdout-hide #t)
    (stdin-hide #t)) ;; end panel definition

(panel)

```



## Elena

ELENA 4.0:

```elena
import forms;
import extensions;

public class MainWindow : SDIDialog
{
    Label  lblClicks;
    Button btmClickMe;

    //Store how much clicks the user doed
    int clicksCount;

    constructor new()
       <= new()
    {
        lblClicks := new Label();
        btmClickMe := new Button();

        clicksCount := 0;
        self
            .appendControl(lblClicks)
            .appendControl(btmClickMe);

        self.Caption := "Rosseta Code";
        self.setRegion(100, 100, 160, 80);

        lblClicks.Caption := "Clicks: 0";
        lblClicks.setRegion(10, 2, 160, 20);

        btmClickMe.Caption := "Click me";
        btmClickMe.setRegion(7, 20, 140, 30);

        btmClickMe.onClick := (args){ self.onButtonClick(); };
    }

    private onButtonClick()
    {
        clicksCount := clicksCount + 1;

        lblClicks.Caption := "Clicks: " + clicksCount.toString();
    }
}
```



## Euphoria


### EuWinGUI

```euphoria
include EuWinGUI.ew

Window("EuWinGUI - Simple windowed application",100,100,360,100)
constant Button1 = Control(Button,"Click me",250,20,80,25)
constant Label1 = Control(Label,"There have been no clicks yet",10,25,200,18)

integer clicks
clicks = 0

-- Event loop
while 1 do
    WaitEvent()
    if EventOwner = Button1 and Event = Click then
        clicks += 1
        SetText(Label1,sprintf("You clicked me %d times",clicks))
    end if
end while

CloseApp(0)
```



=={{header|F_Sharp|F#}}==
```fsharp
open System.Windows.Forms

let mutable clickCount = 0

let form = new Form()

let label = new Label(Text = "There have been no clicks yet.", Dock = DockStyle.Top)
form.Controls.Add(label)

let button = new Button(Text = "Click me", Dock = DockStyle.Bottom)
button.Click.Add(fun _ ->
    clickCount <- clickCount+1
    label.Text <- sprintf "Number of clicks: %i." clickCount)
form.Controls.Add(button)

Application.Run(form)
```



## Factor


```factor
USING: accessors arrays kernel math math.parser namespaces
sequences ui ui.gadgets ui.gadgets.borders ui.gadgets.buttons
ui.gadgets.grids ui.gadgets.labels ui.gadgets.worlds ;
IN: rosetta-code.simple-windowed-application

SYMBOL: n

CONSTANT: on-btn-press [
    parents second n get number>string <label> { 0 1 }
    grid-add drop n inc
]

: build-ui ( -- ) [
    f
    T{ world-attributes { title "Simple windowed application" } }
    clone
    "click me" on-btn-press <border-button> 1array
    "There have been no clicks yet" <label> 1array
    2array <grid>
    { 100 100 } <border>
    >>gadgets
    open-window
] with-ui ;

: main ( -- ) 1 n set build-ui ;

MAIN: main
```



## Fantom



```fantom

using fwt
using gfx

class SimpleApplication
{
  public static Void main ()
  {
    Window
    {
      title = "Simple Window Application"
      size = Size(350, 50)
      clicked := 0
      label := Label
      {
        text = "There have been no clicks yet"
      }
      Button
      {
        text = "Click me"
        onAction.add |Event e|
        {
          clicked += 1
          label.text = "There have been $clicked clicks"
        }
      },
      label,
    }.open
  }
}

```



## Forth

```forth
also minos
text-label ptr click-label
Variable click#  click# off
: click-win ( -- ) screen self window new window with
    X" There have been no clicks yet" text-label new
      dup F bind click-label
    ^ S[ 1 click# +!
         click# @ 0 <# #S s" Number of clicks: " holds #>
         click-label assign ]S X" Click me" button new
    &2 vabox new panel s" Clicks" assign show endwith ;
click-win
```



### The same with Theseus

```forth
#! xbigforth
\ automatic generated code
\ do not edit

also editor also minos also forth

component class ccount
public:
  early widget
  early open
  early dialog
  early open-app
  text-label ptr click#
 ( [varstart] ) cell var clicks ( [varend] )
how:
  : open     new DF[ 0 ]DF s" Click counter" open-component ;
  : dialog   new DF[ 0 ]DF s" Click counter" open-dialog ;
  : open-app new DF[ 0 ]DF s" Click counter" open-application ;
class;

ccount implements
 ( [methodstart] )  ( [methodend] )
  : widget  ( [dumpstart] )
        X" There have been no clicks yet" text-label new  ^^bind click#
        ^^ S[ 1 clicks +!
clicks @ 0 <# #S s" Number of clicks: " holds #> click# assign ]S ( MINOS ) X" Click me"  button new
      &2 vabox new panel
    ( [dumpend] ) ;
  : init  ^>^^  assign  widget 1 :: init ;
class;

: main
  ccount open-app
  $1 0 ?DO  stop  LOOP bye ;
script? [IF]  main  [THEN]
previous previous previous
```



## FreeBASIC


```FreeBasic

#Include "windows.bi"

Dim As HWND Window_Main, Static_Text, Button_Click
Dim As MSG msg
Dim As Integer Num_Click
Dim As String Text

'Create a window with a static text control and a button:
Window_Main = CreateWindow("#32770", "Simple Windowed Application", WS_OVERLAPPEDWINDOW Or WS_VISIBLE, 100, 100, 350, 200, 0, 0, 0, 0)
Static_Text = CreateWindow("STATIC", "There have been no clicks yet", WS_VISIBLE Or WS_CHILD Or WS_BORDER, 10, 30, 300, 20, Window_Main, 0, 0, 0)
Button_Click = CreateWindow("BUTTON", "Click me", WS_VISIBLE Or WS_CHILD, 100, 70, 100, 20, Window_Main, 0, 0, 0)

'Windows message loop:
Num_Click = 0
While GetMessage(@msg, Window_Main, 0, 0)
  TranslateMessage(@msg)
  DispatchMessage(@msg)
  Select Case msg.hwnd
    Case Button_Click
      If msg.message = WM_LBUTTONDOWN Then
        Num_Click = Num_Click + 1
        If Num_Click = 1 Then
          Text = "Button has been clicked once"
        Else
          Text = "Button has been clicked " + Str(Num_Click) + " times"
        End If
	SetWindowText(Static_Text, Text)
      End If
    Case Window_Main
      If msg.message = WM_COMMAND Then End
  End Select
Wend

End

```



## Gambas


```gambas
iCount As Integer                                                 'Counter of clicks!
hLabel As Label                                                   'We need a Label

Public Sub Form_Open()
Dim hButton As Button                                             'We need a Button

With Me                                                           'Set the Form's Properties..
  .height = 75                                                    'Set the Height
  .Width = 300                                                    'Set the Width
  .Arrangement = Arrange.Vertical                                 'Arrange items vertically
  .Padding = 5                                                    'Border area
  .Title = "Click counter!"                                       'Title displayed on the Form
End With

hlabel = New Label(Me)                                            'Add a Label to the form

With hlabel                                                       'Set the Label's Properties..
  .expand = True                                                  'Expand the Label to fit the Form
  .Text = "There have been no clicks yet"                         'Add Text to the Label
  .Alignment = Align.Center                                       'Center the Text
End With

hButton = New Button(Me) As "Button1"                             'Add a Button to the form as Event "Button1"

With hButton                                                      'Set the Button's Properties..
  .Height = 28                                                    'Set the Height
  .Text = "&Click me"                                             'Add Text (The '&' adds a keyboard shortcut)
End With

End

Public Sub Button1_Click()                                        'When the Button is clicked..

Inc iCount                                                        'Increase the value of iCount
hLabel.text = "The button has been clicked " & iCount & " times"  'Display the amount of clicks"

End
```



## Gastona


```gastona
#javaj#

   <frames> main, Simple click counter

   <layout of main>
      PANEL, X
      bClick me, lClicks

#data#

   <NN> 0
   <lClicks> //There have been no clicks yet

#listix#

   <-- bClick me>
      NUM=, NN, NN+1
      -->, lClicks data!,, //@<NN> clicks so far


```


## Go

```go
package main

import (
    "fmt"
    "github.com/mattn/go-gtk/gtk"
)

func main() {
    gtk.Init(nil)
    window := gtk.NewWindow(gtk.WINDOW_TOPLEVEL)
    window.SetTitle("Click me")
    label := gtk.NewLabel("There have been no clicks yet")
    var clicks int
    button := gtk.NewButtonWithLabel("click me")
    button.Clicked(func() {
        clicks++
        if clicks == 1 {
            label.SetLabel("Button clicked 1 time")
        } else {
            label.SetLabel(fmt.Sprintf("Button clicked %d times",
                clicks))
        }
    })
    vbox := gtk.NewVBox(false, 1)
    vbox.Add(label)
    vbox.Add(button)
    window.Add(vbox)
    window.Connect("destroy", func() {
        gtk.MainQuit()
    })
    window.ShowAll()
    gtk.Main()
}
```



## Groovy


```groovy
import groovy.swing.SwingBuilder

count = 0
new SwingBuilder().edt {
  frame(title:'Click frame', pack: true, show: true) {
    vbox {
      countLabel = label("There have been no clicks yet.")
      button('Click Me', actionPerformed: {count++; countLabel.text = "Clicked ${count} time(s)."})
    }
  }
}
```


'''with binding:'''

```groovy
import groovy.swing.SwingBuilder
import groovy.beans.Bindable

@Bindable class Model {
   Integer count = 0
}
model = new Model()
new SwingBuilder().edt {
  frame(title:'Click frame', pack: true, show: true) {
    vbox {
      label(text: bind(source: model, sourceProperty: 'count',
        converter: { v -> !v ? "There have been no clicks yet." : "Clicked ${v} time(s)."}))
      button('Click Me', actionPerformed: {model.count++})
    }
  }
}
```



## Haskell

{{libheader|Gtk}} from [http://hackage.haskell.org/packages/hackage.html HackageDB]

```haskell
import Graphics.UI.Gtk
import Data.IORef

main :: IO ()
main = do
  initGUI
  window <- windowNew
  window `onDestroy` mainQuit
  windowSetTitle window "Simple Windowed App"
  set window [ containerBorderWidth := 10 ]

  hbox <- hBoxNew True 5

  set window [ containerChild := hbox ]

  lab <- labelNew (Just "There have been no clicks yet")
  button <- buttonNewWithLabel "Click me"
  set hbox [ containerChild := lab ]
  set hbox [ containerChild := button ]

  m <- newIORef 0

  onClicked button $ do
    v <- readIORef m
    writeIORef m (v+1)
    set lab [ labelText := "There have been " ++ show (v+1) ++ " clicks" ]

  widgetShowAll window

  mainGUI
```



## HicEst


```hicest
 CHARACTER label="There have been no clicks yet"

    DO count = 1, 1E100 ! "forever"
      DLG(Button="Click me", Width=3, TItle=label) ! Width=3 to display full length label
      label = "Clicked " // count // "time(s)"
    ENDDO

 END
```


==Icon and {{header|Unicon}}==

This version is Unicon-specific:

```unicon
import gui
$include "guih.icn"

procedure main()
   SimpleWindow().show_modal()
end

class SimpleWindow : Dialog(label, button, count)
   method component_setup()
      self.set_attribs("size=222,139")
      label := Label()
      label.set_pos("24", "24")
      label.set_internal_alignment("l")
      label.set_label("There have been no clicks yet.")
      self.add(label)
      button := TextButton()
      button.set_pos(24, 53)
      button.set_label("click me")
      button.set_internal_alignment("c")
      button.connect(self, "incr", ACTION_EVENT)
      self.add(button)
   end

   method incr()
       /count := 0
       label.set_label("There have been "||(count+:=1)||" clicks.")
    end

   initially
      self.Dialog.initially()
end
```



## IDL


```idl
pro counter, ev
  widget_control, ev.top, get_uvalue=tst
  tst[1] = tst[1]+1
  widget_control, tst[0], set_value="Number of clicks: "+string(tst[1],format='(i0)')
  widget_control, ev.top, set_uvalue=tst
end

id = widget_base(title = 'Window Title',column=1)
ld = widget_label(id, value = 'There have been no clicks yet.')
widget_control, /realize, id, set_uvalue=[ld,0]
dummy = widget_button(id,value=' Click Me ',event_pro='counter')
xmanager, "Simple", Id

end
```


## J

'''J 8.x'''

```j
SIMPLEAPP=: noun define
pc simpleApp;
cc inc button;cn "Click me";
cc shownText static;cn "There have been no clicks yet.";
)

simpleApp_run=: verb define
  wd SIMPLEAPP
  simpleApp_accum=: 0   NB. initialize accumulator
  wd 'pshow;'
)

simpleApp_inc_button=: verb define
  wd 'set shownText text ','Button-use count:  ',": simpleApp_accum=: >: simpleApp_accum
)

simpleApp_close=: wd bind 'pclose'
simpleApp_cancel=: simpleApp_close

simpleApp_run''
```


'''J 6.x'''

```j
SIMPLEAPP=: noun define
pc simpleApp;
xywh 131 11 44 12;cc inc button;cn "Click me";
xywh 7 10 115 11;cc shownText static;cn "There have been no clicks yet.";
pas 6 6;pcenter;
rem form end;
)

simpleApp_run=: verb define
  wd SIMPLEAPP
  simpleApp_accum=: 0   NB. initialize accumulator
  wd 'pshow;'
)

simpleApp_inc_button=: verb define
  wd 'set shownText *','Button-use count:  ',": simpleApp_accum=: >: simpleApp_accum
)

simpleApp_close=: wd bind 'pclose'
simpleApp_cancel=: simpleApp_close

simpleApp_run''
```



## Java

```java
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
public class Clicks extends JFrame{
	private long clicks = 0;

	public Clicks(){
		super("Clicks");//set window title
		JLabel label = new JLabel("There have been no clicks yet");
		JButton clicker = new JButton("click me");
		clicker.addActionListener(//listen to the button
			new ActionListener(){
				@Override
				public void actionPerformed(ActionEvent e) {
					label.setText("There have been " + (++clicks) + " clicks");//change the text
				}
			}
		);
		setLayout(new BorderLayout());//handles placement of components
		add(label,BorderLayout.CENTER);//add the label to the biggest section
		add(clicker,BorderLayout.SOUTH);//put the button underneath it
		label.setPreferredSize(new Dimension(300,100));//nice big label
		label.setHorizontalAlignment(JLabel.CENTER);//text not up against the side
		pack();//fix layout
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);//stop the program on "X"
		setVisible(true);//show it
	}
	public static void main(String[] args){
		SwingUtilities.invokeLater( //Swing UI updates should not happen on the main thread
			() -> new Clicks() //call the constructor where all the magic happens
		);
	}
}
```



## JavaFX Script

```javafx
import javafx.stage.*;
import javafx.scene.*;
import javafx.scene.layout.*;
import javafx.scene.control.*;

Stage {
   scene: Scene {
      width: 300 height: 200
      content: VBox {
         var clicks: Integer;

         spacing: 10
         content: [
            Label {
               def varText = bind if (clicks == 0) then "no clicks yet" else "{clicks} clicks"
               text : bind "There have been {varText}"
            }
            Button {
               text: "click me"
               onMouseClicked: function(e) { clicks++; }
            }
         ]
      }
   }
}
```



## Julia

Uses the Gtk library.

```julia

using Gtk.ShortNames


function clickwindow()
    clicks = 0
    win = Window("Click Counter", 300, 100) |> (Frame() |> (vbox = Box(:v)))
    lab = Label("There have been no clicks yet.")
    but = Button("click me")
    push!(vbox, lab)
    push!(vbox, but)
    setproperty!(vbox, :expand, lab, true)
    setproperty!(vbox, :spacing, 20)
    callback(w) = (clicks += 1; setproperty!(lab, :label, "There have been $clicks button clicks."))
    id = signal_connect(callback, but, "clicked")
    c = Condition()
    endit(w) = notify(c)
    signal_connect(endit, win, :destroy)
    showall(win)
    wait(c)
end


clickwindow()

```



## Kotlin

```scala
// version 1.0.6

import java.awt.BorderLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import javax.swing.*

class Clicks : JFrame(), ActionListener {
    private var clicks = 0
    private val label: JLabel
    private val clicker: JButton
    private var text: String

    init {
        text = "There have been no clicks yet"
        label = JLabel(text)
        clicker = JButton("click me")
        clicker.addActionListener(this)        // listen to the button
        layout = BorderLayout()                // handles placement of components
        add(label, BorderLayout.CENTER)        // add the label to the biggest section
        add(clicker, BorderLayout.SOUTH)       // put the button underneath it
        setSize(300, 200)                       // stretch out the window
        defaultCloseOperation = EXIT_ON_CLOSE  // stop the program on "X"
        isVisible = true                       // show it
    }

    override fun actionPerformed(arg0: ActionEvent) {
        if (arg0.source == clicker) {           // if they clicked the button
            if (clicks == 0) text = "There has been " + (++clicks) + " click"
            else text = "There have been " + (++clicks) + " clicks"
            label.text = text                  // change the text
        }
    }
}

fun main(args: Array<String>) {
    Clicks()  // call the constructor where all the magic happens
}
```


```scala

import tornadofx.*

class ClicksView: View("Clicks example") {
    var clicks = 0
    override val root = vbox(5) {
        var label1 = label("There have been no clicks yet")
        button("Click me!") { action { label1.text = "Clicked ${++clicks} times." } }
    }
}

class ClicksApp: App(ClicksView::class)

fun main(args: Array<String>) = launch<ClicksApp>(args)

```



## Lambdatalk

The environment is a small wiki: http://epsilonwiki.free.fr/lambdaway/

The code is tested in this page: http://epsilonwiki.free.fr/lambdaway/?view=popup

```Scheme

1) the label: {div {@ id="label"} There have been no clicks yet }
2) the button: {input {@ type="button" value="click me" onclick="CLICKAPP.inc()" }}
3) the script: {script °° code °°} where code is a single function:

var CLICKAPP = (function() {
  var counter = 0;
  var inc = function() {
      counter++;
      getId('label').innerHTML =
      'There are ' + counter + ' clicks now';
  };
  return {inc:inc}
})();

```



## Liberty BASIC


```lb
nomainwin
    button #demo.btn, "Click Me", [btnClick], UL, 20, 50
    statictext #demo.num, "There have been no clicks yet.", 20, 100, 240, 30
    open "Rosetta Task: Simple windowed application" for window as #demo
    #demo "trapclose [quit]"
    nClicks = 0
wait

[quit]
    close #demo
end

[btnClick]
    nClicks = nClicks + 1
    #demo.num "The button has been clicked ";nClicks;" times."
wait
```



## Lingo

The standard approach in Lingo for creating GUIs and assigning scripts for handling user interaction is to use the graphical IDE/GUI-Builder "Director". But windows and sprites (visual elements) can also be created and scripted programmatically only:

```lingo
on startMovie

  -- window settings
  _movie.stage.title = "Hello World!"
  _movie.stage.titlebarOptions.visible = TRUE
  _movie.stage.rect = rect(0,0,320, 240)
  _movie.centerStage = 1

  -- create a label (called "field" in Director)
  m = new(#field)
  m.name = "label"
  m.rect = rect(0,0,320,0)
  m.text = "There have been no clicks yet"
  m.alignment = "center"

  -- create sprite, assign field
  _movie.puppetSprite(1, TRUE)
  sprite(1).member = m
  sprite(1).loc = point(0,80)

  -- create a button
  m = new(#button)
  m.rect = rect(0,0,220,0)
  m.text = "click me"
  m.alignment = "center"

  -- create sprite, assign button
  _movie.puppetSprite(2, TRUE)
  sprite(2).member = m
  sprite(2).loc = point(50,105)

  -- create new script at runtime, assign it to button sprite
  m = new(#script)
  m.scriptType = #score
  m.scriptText = "on mouseDown"&RETURN&\
  "  m=member("&QUOTE&"label"&QUOTE&")"&RETURN&\
  "  m.text=string(integer(m.text)+1)"&RETURN&\
  "end"
  sprite(2).scriptInstanceList.add(m.script.new())

  -- force immediate update
  _movie.updateStage()

  -- show the window
  _movie.stage.visible = 1

end
```



## Logo


```logo
to clickwindow
windowCreate "root "clickWin [Click that button!!!] 0 0 100 100 []
Make "i 0
staticCreate "clickWin "clickSt [There have been no clicks yet] 0 0 100 20
buttonCreate "clickWin "clickBtn [click me] 10 20 80 20 ~
	[Make "i :i+1 ~
	ifelse :i=1 [staticUpdate "clickSt (list "clicked :i "time)] ~
	            [staticUpdate "clickSt (list "clicked :i "times)]]
end
```


The window is opened with:

```logo
clickwindow>
```



## Lua


```lua
require"iuplua"
l = iup.label{title="There have been no clicks yet."}
b = iup.button{title="Click me!"}
clicks = 0
function b:button_cb()
  clicks = clicks + 1
  l.title = "There have been " .. clicks/2 .. " clicks so far." --yes, this is correct.
end
dlg = iup.dialog{iup.vbox{l, b}, title="Simple Windowed Application"}
dlg:show()

if (not iup.MainLoopLevel or iup.MainLoopLevel()==0) then
  iup.MainLoop()
end
```



## M2000 Interpreter

For labels M2000 window manager use Buttons with locked property to true. We can press enter because when the form open, default control is the Button1 (the second button we make). edit

For label1 we make a linked property, named Caption$ (we can use dot, so we can use label1.caption$). See that we make as second linked property for the same property but with a number (cap). How this is possible?

Μ2000 controls are internal COM objects and as those they can use Variant type properties and these properties can convert as they need. So if need a string and we pass a number then the number change to string.



```M2000 Interpreter

Module CheckIt {
      Declare Form1  Form
      Declare Label1 Button Form Form1
      Declare Button1 Button Form Form1
      Method Label1,"move", 2000, 2000, 4000, 600
      Method Button1,"move", 2000, 3000, 4000, 600
      With Label1, "Caption" as caption$, "Locked", true, "Caption" as cap
      With Button1, "Caption", "click me", "Default", True   ' make this the default control
      caption$="There have been no clicks yet"
      m=0
      Function Button1.Click {
                  m++
                  cap=m
      }
      Method Form1, "Show",1
      Declare Form1 Nothing
}
Checkit

```



## Mathematica


```Mathematica
DynamicModule[{n = 0},
 CreateDialog[{Dynamic@
    TextCell@If[n == 0, "There have been no clicks yet", n],
   Button["click me", n++]}]]
```



## MAXScript


```maxscript
rollout buttonClick "Button Click"
(
    label l "There have been no clicks yet"
    button clickMe "Click me"
    local clickCount = 0

    on clickMe pressed do
    (
        clickCount += 1
        l.text = ("Number of clicks: " + clickCount as string)
    )
)
createDialog buttonClick
```


=={{header|Modula-3}}==
This code uses <tt>Trestle</tt>, a windowing toolkit developed for Modula-3.

```modula3
MODULE Click EXPORTS Main;

IMPORT Fmt, TextVBT, ButtonVBT, VBT, Axis, HVSplit, TrestleComm, Trestle;

VAR label := TextVBT.New("There have been no clicks yet.");
    button := ButtonVBT.New(TextVBT.New("Click me!"), Clicked);
    main := HVSplit.Cons(Axis.T.Ver, label, button, adjustable := FALSE);
    count := 0;

PROCEDURE Clicked(<*UNUSED*>button: ButtonVBT.T; <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    INC(count);
    TextVBT.Put(label, "Button pressed: " & Fmt.Int(count));
  END Clicked;

<*FATAL TrestleComm.Failure*>
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Click.
```


To compile the above code, you need to create a file called <tt>m3makefile</tt> which contains:

```txt

import("ui")
import("libm3")
implementation("Click")
program("Click")

```



## Nanoquery


```nanoquery
import Nanoquery.Util.Windows

// define the necessary objects
$w = new("Window")
$b = new("Button")
$l = new("Label")

$b.setParent($w)
$l.setParent($w)

// define the amount of clicks
$clicks = 0

// a function to update the label
def updateLabel($caller, $event)
        global $clicks
        global $l

        $clicks = $clicks+1
        $l.setText(str($clicks))

        global $clicks = $clicks
end

// prepare the components to be displayed
$w.setSize(200,200)
$b.setText("click me")
$b.setPosition(0,100)
$l.setText("There have been no clicks yet")

// set the button's event handler to the function updateLabel
$b.setHandler($updateLabel)

// show the window
$w.show()
```



## Nim


```nim

import gtk2

var
  win = windowNew WINDOW_TOPLEVEL
  button = buttonNew "Click me"
  label = labelNew  "There have been no clicks yet"
  vbox = vboxNew(true, 1)
  counter = 0

proc clickedMe(o: var PButton, l: PLabel) =
  inc counter
  l.setText "You clicked me " & $counter & " times"

nim_init()
win.setTitle "Click me"
vbox.add label
vbox.add button
win.add vbox
discard win.signal_connect("delete-event", SignalFunc mainQuit, nil)
discard button.signal_connect("clicked", SignalFunc clickedMe, label)
win.showAll()
main()
```


=={{header|Objective-C}}==
```objc
#include <Foundation/Foundation.h>
#include <AppKit/AppKit.h>

@interface ClickMe : NSWindow
{
  NSButton *_button;
  NSTextField *_text;
  int _counter;
}
- (void)applicationDidFinishLaunching: (NSNotification *)notification;
- (BOOL)applicationShouldTerminateAfterLastWindowClosed: (NSNotification *)notification;
- (void)advanceCounter: (id)sender;
@end
```



```objc
@implementation ClickMe : NSWindow
-(instancetype) init
{
  NSButton *button = [[NSButton alloc] init];
  [button setButtonType: NSToggleButton];
  [button setTitle: @"Click Me"];
  [button sizeToFit];
  [button setTarget: self];
  [button setAction: @selector(advanceCounter:)];
  NSRect buttonRect = [button frame];

  NSTextField *text = [[NSTextField alloc]
	   initWithFrame: NSMakeRect(buttonRect.origin.x, buttonRect.size.height,
				     buttonRect.size.width, buttonRect.size.height)];
  [text setAlignment: NSCenterTextAlignment];
  [text setEditable: NO];
  [text setStringValue: @"There have been no clicks yet"];
  [text sizeToFit];

  // reset size of button according to size of (larger...?) text
  [button
    setFrameSize: NSMakeSize( [text frame].size.width, buttonRect.size.height ) ];

  int totalWindowHeight = buttonRect.size.height + [text frame].size.height;

  if ((self = [super initWithContentRect: NSMakeRect(100, 100,
				    [text frame].size.width, totalWindowHeight)
        styleMask: (NSTitledWindowMask | NSClosableWindowMask)
      backing: NSBackingStoreBuffered
      defer: NO])) {
    _counter = 0;
    _button = button;
    _text = text;

    [[self contentView] addSubview: _text];
    [[self contentView] addSubview: _button];

    [self setTitle: @"Click Me!"];
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

- (void)advanceCounter: (id)sender
{
  [_text setStringValue: [NSString stringWithFormat: @"Clicked %d times", ++_counter]];
}
@end


int main()
{
  @autoreleasepool {
    NSApplication *app =  [NSApplication sharedApplication];
    ClickMe *clickme = [[ClickMe alloc] init];
    [app setDelegate: clickme];
    [app run];
  }
  return 0;
}
```



## OCaml

* with '''Labltk''', the '''Tk''' OCaml binding:

```ocaml
#directory "+labltk"
#load "labltk.cma"

let () =
  let top = Tk.openTk() in
  Wm.title_set top "Tk-OCaml Example";
  let label = Label.create ~text:"There have been no clicks yet" top in
  let b =
    Button.create
        ~text:"click me"
        ~command:(fun () -> Tk.closeTk (); exit 0)
        top
  in
  Tk.pack [Tk.coe label; Tk.coe b];
  Tk.mainLoop ();
;;
```


* with '''LablGTK2''', the '''GTK2''' OCaml binding:

```ocaml
open GMain

let window = GWindow.window ~border_width:2 ()
let vbox = GPack.vbox ~packing:window#add ()
let label = GMisc.label ~text:"There have been no clicks yet" ~packing:vbox#pack ()
let button = GButton.button ~label:"click me" ~packing:vbox#pack ()

let () =
  window#event#connect#delete ~callback:(fun _ -> true);
  window#connect#destroy ~callback:Main.quit;
  button#connect#clicked ~callback:window#destroy;
  window#show ();
  Main.main ()
```



## ooRexx


```oorexx
/* REXX ***************************************************************************************
* 18.06.2014 Walter Pachl shortened from Rony Flatscher's bsf4oorexx (see sourceforge) samples
* Look there for ShowCount.rxj
* bsf4oorexx lets the ooRexx program use Java classes
**********************************************************************************************/
userData=.directory~new    -- a directory which will be passed to Rexx with the event

   --  create a framed Java window, set a title text
win=.bsf~new("java.awt.Frame", "Show Count")
   -- Create a Java RexxProxy for controlling the closing of the application
rexxCloseEH =.RexxCloseAppEventHandler~new   -- Rexx event handler
   -- Create Java RexxProxy for the Rexx event handler
rpCloseEH=BsfCreateRexxProxy(rexxCloseEH,,"java.awt.event.WindowListener" )
win~addWindowListener(rpCloseEH) -- add RexxProxy event handler

   -- create a Java push button
but=.bsf~new("java.awt.Button","Press me!")
   -- Create a RexxProxy for the button Rexx event handler
rp=BsfCreateRexxProxy(.RexxShowCountEventHandler~new,userData,"java.awt.event.ActionListener")
but~addActionListener(rp)        -- add RexxProxy event handler

lab=.bsf~new("java.awt.Label")   -- create a Java label,set it to show the text centered
userData~label=lab               -- save label object for later use in event handler
lab~setAlignment(lab~center)     -- set alignment to center
lab~setText("Button was not yet pressed") -- assign initial text to the label

win ~~add("Center",lab) ~~add("South",but) -- add the label and the button to the frame
win ~~pack                       -- now calculate all widget dimensions
call enlargeWidth win,but,120    -- enlarge the width of the frame and the button

win ~~setVisible(.true) ~~toFront   -- make frame visible and move it to the front

userData~i=0                     -- set counter to 0

rexxCloseEH~waitForExit          -- wait until we are allowed to end the program

-- if Java was loaded by Rexx,then terminate Java's RexxEngine to inhibit callbacks from Java
call BSF.terminateRexxEngine

::requires BSF.CLS      -- get Java support

   /* enlarge the width of the frame and of the button without using a layout manager  */
::routine enlargeWidth
  use arg win,but,addToWidth
  winDim=win~getSize            -- get frame's dimension
  winDim~width+=addToWidth      -- increase width
  win~setSize(winDim)           -- set frame's dimension

/* ------------------------------------------------------------------------ */
/* Rexx event handler to set "close app" indicator */
::class RexxCloseAppEventHandler
::method init                   -- constructor
  expose closeApp
  closeApp  = .false            -- if set to .true, then it is safe to close the app

::attribute closeApp            -- indicates whether app should be closed

::method unknown                -- intercept unhandled events,do nothing

::method windowClosing          -- event method (from WindowListener)
  expose closeApp
  closeApp=.true                -- indicate that the app should close

::method waitForExit            -- method blocks until attribute is set to .true
  expose closeApp
  guard on when closeApp=.true

/* ------------------------------------------------------------------------ */
/* Rexx event handler to process tab changes */
::class RexxShowCountEventHandler
::method actionPerformed
  use arg eventObject,slotDir
  call showCount slotDir~userData

/* ------------------------------------------------------------------------ */
::routine ShowCount             -- increment counter and show text
   use arg userData
   userData~i+=1                -- increment counter in directory object
   Select                       -- construct text part
     When userData~i=1 Then how_often='once'
     When userData~i=2 Then how_often='twice'
     When userData~i=3 Then how_often='three times'
     Otherwise how_often=userData~i 'times'
   End
   userData~label~setText("Button was pressed" how_often) -- display text
```



## Oz


```oz
functor
import
   Application
   QTk at 'x-oz://system/wp/QTk.ozf'
define
   Count = {NewCell 0}
   Label
   GUI = td(action:proc {$} {Application.exit 0} end %% exit on close
	    label(text:"There have been no clicks yet." handle:Label)
	    button(text:"Click Me"
		   action:proc {$}
			     Count := @Count + 1
			     {Label set(text:"Number of clicks: "#@Count#".")}
			  end
		  ))
   Window = {QTk.build GUI}
   {Window show}
end

```



## Pascal

Ported from the C example.

```pascal
Program SimpleWindowApplication;

uses
  SysUtils,
  glib2,
  Gtk2;

const
  clickme = 'Click Me';
  MAXLEN = 64;

var
  counter: integer = 0;

procedure clickedme(o: PGtkButton; d: pointer); cdecl;
  var
    nt: Pchar;
    l: PGtkLabel;
  begin
    l := Gtk_LABEL(d);
    inc(counter);
    nt := Pchar('You clicked me ' + inttostr(counter) + ' times');
    Gtk_label_set_text(l, nt);
  end;

var
  win:     PGtkWindow;
  button:  PGtkButton;
  Mylabel: PGtkLabel;
  vbox:    PGtkVBox;

begin
  Gtk_init(@argc, @argv);
  win := PGtkWindow(Gtk_window_new(Gtk_WINDOW_TOPLEVEL));
  Gtk_window_set_title(win, clickme);
  button := PGtkButton(Gtk_button_new_with_label(clickme));
  Mylabel := PGtkLabel(Gtk_label_new('There have been no clicks yet'));
  Gtk_label_set_single_line_mode(Mylabel, TRUE);
  vbox := PGtkVBox(Gtk_vbox_new(TRUE, 1));
  Gtk_container_add(Gtk_CONTAINER(vbox), Gtk_WIDGET(Mylabel));
  Gtk_container_add(Gtk_CONTAINER(vbox), Gtk_WIDGET(button));
  Gtk_container_add(Gtk_CONTAINER(win), Gtk_WIDGET(vbox));
  g_signal_connect(G_OBJECT(win), 'delete-event', TGCallBack(@Gtk_main_quit), NULL);
  g_signal_connect(G_OBJECT(button), 'clicked', TGCallBack(@clickedme), Mylabel);
  Gtk_widget_show_all(Gtk_WIDGET(win));
  Gtk_main();
end.
```



## Perl

==={{libheader|Perl/Tk}}===

```perl
use Tk;

$main = MainWindow->new;
$l = $main->Label('-text' => 'There have been no clicks yet.')->pack;
$count = 0;
$main->Button(
  -text => ' Click Me ',
  -command => sub { $l->configure(-text => 'Number of clicks: '.(++$count).'.'); },
)->pack;
MainLoop();
```


==={{libheader|GTK}} {{works with|Perl/Gtk}}===

```perl
use Gtk '-init';

# Window.
$window = Gtk::Window->new;
$window->signal_connect('destroy' => sub { Gtk->main_quit; });

# VBox.
$vbox = Gtk::VBox->new(0, 0);
$window->add($vbox);

# Label.
$label = Gtk::Label->new('There have been no clicks yet.');
$vbox->add($label);

# Button.
$count = 0;
$button = Gtk::Button->new(' Click Me ');
$vbox->add($button);
$button->signal_connect('clicked', sub {
  $label->set_text(++$count);
});

# Show.
$window->show_all;

# Main loop.
Gtk->main;
```



## Perl 6

```perl6
use GTK::Simple;
use GTK::Simple::App;

my GTK::Simple::App $app .= new(title => 'Simple Windowed Application');

$app.size-request(350, 100);

$app.set-content(
    GTK::Simple::VBox.new(
        my $label  = GTK::Simple::Label.new( text  => 'There have been no clicks yet'),
        my $button = GTK::Simple::Button.new(label => 'click me'),
    )
);

$app.border-width = 40;

$button.clicked.tap: {
    state $clicks += 1;
    $label.text = "There has been $clicks click{ 's' if $clicks != 1 }";
}

$app.run;
```



## Phix

```Phix
include pGUI.e

Ihandle dlg, lbl, btn, vbox
integer clicks = 0

function click_cb(Ihandle /*btn*/)
    clicks += 1
    IupSetStrAttribute(lbl,"TITLE","clicked %d times",{clicks})
    return IUP_DEFAULT;
end function

IupOpen()
lbl = IupLabel("There have been no clicks yet")
btn = IupButton("Click me", Icallback("click_cb"))
vbox = IupVbox({lbl, IupHbox({IupFill(),btn,IupFill()})})
dlg = IupDialog(vbox,"MARGIN=10x10, GAP=10, RASTERSIZE=400x0")
IupSetAttribute(dlg, "TITLE", "Simple windowed application")
IupShow(dlg)
IupMainLoop()
IupClose()
```

The above is cross platform (win/lnx), 32/64 bit. On request, I have restored the following arwen version (win32-only).

The included demo\edita contains a window painter that lets you reposition/resize this very easily (alas the equivalent for the above is progressing rather leisurely).

```Phix
include arwen.ew

constant main  = create(Window,"Simple windowed application",0,0,100,100,300,200, 0)
constant label = create(Label, "There have been no clicks yet",0,main,10,10,250,30,0)
constant btn   = create(Button,"Click me",0,main,100,50,100,30,0)
integer count = 0

function mainHandler(integer id, integer msg, atom /*wParam*/, object /*lParam*/)
    if id=btn and msg=WM_COMMAND then
        count += 1
        setText(label,sprintf("clicked %d times",count))
    end if
    return 0
end function
setHandler(btn,routine_id("mainHandler"))

WinMain(main,SW_NORMAL)
```



## PicoLisp

The standard PicoLisp GUI is HTTP based. Connect your browser to
http://localhost:8080 after starting the following script.

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(load "@ext.l" "@lib/http.l" "@lib/xhtml.l" "@lib/form.l")

(zero *Count)

(de start ()
   (app)
   (action
      (html 0 "Clicks" NIL NIL
         (form NIL
            (gui '(+Init +TextField) "There have been no clicks yet")
            (----)
            (gui '(+JS +Button) "click me"
               '(set> (field -1)
                  (pack "Clicked " (inc '*Count) " times") ) ) ) ) ) )

(server 8080 "!start")
(wait)
```



## Pike

```Pike
GTK2.Widget mainwindow,clickcnt,clicker;
int clicks;

void click()
{
        clickcnt->set_text("Clicks: "+(++clicks));
}

int main()
{
        GTK2.setup_gtk();
        mainwindow=GTK2.Window(GTK2.WindowToplevel);
        mainwindow->set_title("Click counter");
        mainwindow->add(GTK2.Vbox(0,10)
                ->add(clickcnt=GTK2.Label("There have been no clicks yet"))
                ->add(clicker=GTK2.Button("Click me"))
        )->show_all();
        mainwindow->signal_connect("delete_event",lambda() {exit(0);});
        clicker->signal_connect("clicked",click);
        return -1;
}

```



## PowerShell


### Windows Forms

```PowerShell

$Label1  = [System.Windows.Forms.Label]@{
            Text = 'There have been no clicks yet'
            Size = '200, 20' }
$Button1 = [System.Windows.Forms.Button]@{
            Text = 'Click me'
            Location = '0, 20' }

$Button1.Add_Click(
    {
    $Script:Clicks++
    If ( $Clicks -eq 1 ) { $Label1.Text = "There has been 1 click" }
    Else                 { $Label1.Text = "There have been $Clicks clicks" }
    } )

$Form1 = New-Object System.Windows.Forms.Form
$Form1.Controls.AddRange( @( $Label1, $Button1 ) )

$Clicks = 0

$Result = $Form1.ShowDialog()

```

```PowerShell

Add-Type -AssemblyName System.Windows.Forms

$Label1 = New-Object System.Windows.Forms.Label
$Label1.Text = 'There have been no clicks yet'
$Label1.Size = '200, 20'

$Button1 = New-Object System.Windows.Forms.Button
$Button1.Text = 'Click me'
$Button1.Location = '0, 20'

$Button1.Add_Click(
    {
    $Script:Clicks++
    If ( $Clicks -eq 1 ) { $Label1.Text = "There has been 1 click" }
    Else                 { $Label1.Text = "There have been $Clicks clicks" }
    } )

$Form1 = New-Object System.Windows.Forms.Form
$Form1.Controls.AddRange( @( $Label1, $Button1 ) )

$Clicks = 0

$Result = $Form1.ShowDialog()

```


### WPF


```PowerShell

[xml]$Xaml = @"
<Window
    xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
    x:Name = "Window1"
    Width  = "200"
    Height = "120"
    ShowInTaskbar = "True">
    <StackPanel>
        <Label
            x:Name  = "Label1"
            Height  = "40"
            Width   = "200"
            Content = "There have been no clicks"/>
        <Button
            x:Name  = "Button1"
            Height  = "25"
            Width   = "60"
            Content = "Click me"/>
    </StackPanel>
</Window>
"@

$Window1 = [Windows.Markup.XamlReader]::Load( [System.Xml.XmlNodeReader]$Xaml )

$Label1  = $Window1.FindName( "Label1"  )
$Button1 = $Window1.FindName( "Button1" )

$Button1.Add_Click(
    {
    $Script:Clicks++
    If ( $Clicks -eq 1 ) { $Label1.Content = "There has been 1 click" }
    Else                 { $Label1.Content = "There have been $Clicks clicks" }
    } )

$Clicks = 0

$Result = $Window1.ShowDialog()

```



## Prolog

Works with SWI-Prolog and XPCE.

```Prolog
:- dynamic click/1.

dialog('Simple windowed application',
       [ object        :=
	   Simple_windowed_application,
	 parts         :=
	   [ Simple_windowed_application :=
	       dialog('Simple windowed application'),
	     Name                       :=
	       label(name, 'There have been no clicks yet'),
	     BtnClick                     :=
	       button(button)
	   ],
	 modifications :=
	   [ BtnClick := [ label := 'Click me !'
		       ]
	   ],
	 layout        :=
	   [ area(Name,
		  area(40, 20, 200, 18)),
	     area(BtnClick,
		  area(90, 60, 80, 24))
	   ],
	 behaviour :=
	 [
	  BtnClick := [message := message(@prolog, btnclick, Name)]
	 ]
       ]).

btnclick(Label) :-
	retract(click(V)),
	V1 is V+1,
	assert(click(V1)),
	sformat(A, '~w click(s)', [V1]),
	send(Label, selection, A).

simple_windowed :-
	retractall(click(_)),
	assert(click(0)),
	make_dialog(D, 'Simple windowed application'),
	send(D, open).


```



## PureBasic


```PureBasic
Define window_0
Define window_0_Text_0, window_0_Button_1
Define clicks, txt$, flags

flags = #PB_Window_SystemMenu | #PB_Window_SizeGadget | #PB_Window_ScreenCentered
window_0 = OpenWindow(#PB_Any, 408, 104, 280, 45, "Simple windowed application", flags)
If window_0
  SmartWindowRefresh(window_0, #True)
  window_0_Text_0 = TextGadget(#PB_Any, 5, 5, 165, 20, "There have been no clicks yet")
  window_0_Button_1 = ButtonGadget(#PB_Any, 190, 10, 85, 30, "Click me")

  Repeat
    Select WaitWindowEvent()
      Case #PB_Event_Gadget
        Select EventGadget()
          Case window_0_Text_0
          Case window_0_Button_1
            clicks + 1
            txt$ = "You Clicked " + Str(clicks) + " time"
            If clicks > 1: txt$ + "s": EndIf
            SetGadgetText(window_0_Text_0, txt$)
        EndSelect
      Case #PB_Event_CloseWindow
        End
    EndSelect
  ForEver
EndIf
```



## Python


==={{libheader|Tkinter}}===

```python
from functools import partial

import tkinter as tk


def on_click(label: tk.Label,
             counter: tk.IntVar) -> None:
    counter.set(counter.get() + 1)
    label["text"] = f"Number of clicks: {counter.get()}"


def main():
    window = tk.Tk()
    label = tk.Label(master=window,
                     text="There have been no clicks yet")
    label.pack()
    counter = tk.IntVar()
    update_counter = partial(on_click,
                             label=label,
                             counter=counter)
    button = tk.Button(master=window,
                       text="click me",
                       command=update_counter)
    button.pack()
    window.mainloop()


if __name__ == '__main__':
    main()

```

The same in OO manner:

```python
import tkinter as tk


class ClickCounter(tk.Frame):
    def __init__(self, master=None):
        super().__init__(master)
        tk.Pack.config(self)
        self.label = tk.Label(self, text='There have been no clicks yet')
        self.label.pack()
        self.button = tk.Button(self,
                                text='click me',
                                command=self.click)
        self.button.pack()
        self.count = 0

    def click(self):
        self.count += 1
        self.label['text'] = f'Number of clicks: {self.count}'


if __name__ == "__main__":
    ClickCounter().mainloop()


```


==={{libheader|PyQt}}===

```python
from functools import partial
from itertools import count

from PyQt5.QtWidgets import (QApplication,
                             QLabel,
                             QPushButton,
                             QWidget)
from PyQt5.QtCore import QRect

LABEL_GEOMETRY = QRect(0, 15, 200, 25)
BUTTON_GEOMETRY = QRect(50, 50, 100, 25)


def on_click(_, label, counter=count(1)):
    label.setText(f"Number of clicks: {next(counter)}")


def main():
    application = QApplication([])
    window = QWidget()
    label = QLabel(text="There have been no clicks yet",
                   parent=window)
    label.setGeometry(LABEL_GEOMETRY)
    button = QPushButton(text="click me",
                         parent=window)
    button.setGeometry(BUTTON_GEOMETRY)
    update_counter = partial(on_click,
                             label=label)
    button.clicked.connect(update_counter)
    window.show()
    application.lastWindowClosed.connect(window.close)
    application.exec_()


if __name__ == '__main__':
    main()

```


==={{libheader|wxPython}}===

```python
import wx


class ClickCounter(wx.Frame):
    def __init__(self):
        super().__init__(parent=None)
        self.count = 0
        self.button = wx.Button(parent=self,
                                label="Click me!")
        self.label = wx.StaticText(parent=self,
                                   label="There have been no clicks yet")
        self.Bind(event=wx.EVT_BUTTON,
                  handler=self.click,
                  source=self.button)

        self.sizer = wx.BoxSizer(wx.VERTICAL)
        self.sizer.Add(window=self.button,
                       proportion=1,
                       flag=wx.EXPAND)
        self.sizer.Add(window=self.label,
                       proportion=1,
                       flag=wx.EXPAND)
        self.SetSizer(self.sizer)
        self.sizer.Fit(self)

    def click(self, _):
        self.count += 1
        self.label.SetLabel(f"Count: {self.count}")


if __name__ == '__main__':
    app = wx.App()
    frame = ClickCounter()
    frame.Show()
    app.MainLoop()

```



## R

gWidgetsRGtk2 or gWidgetsrJava can be used as an alternative to gWidgetstcltk.

```r
library(gWidgets)
library(gWidgetstcltk)
win <- gwindow()
lab <- glabel("There have been no clicks yet", container=win)
btn <- gbutton("click me", container=win, handle=function(h, ...)
   {
      val <- as.numeric(svalue(lab))
      svalue(lab) <- ifelse(is.na(val) ,"1", as.character(val + 1))
   }
)
```



## Racket



```racket

#lang racket/gui

(define frame (new frame% [label "There have been no clicks yet"]))

(define num-clicks 0)
(define (cb obj me)
  (set! num-clicks (add1 num-clicks))
  (send frame set-label (format "~a" num-clicks)))

(new button% [parent frame] [label "Click me"] [callback cb])
(send frame show #t)

```



## RapidQ

RapidQ has form designer that produces RapidQ Basic source code. You can get the same result by writing the code yourself with a text editor. Then compile it either from within IDE or by using the command line compiler.


```rapidq
DECLARE SUB buttonClick

CREATE form AS QForm
    Center
    Height = 120
    Width = 300

    CREATE text AS QLabel
        Caption = "There have been no clicks yet."
        Left = 30: Top = 20
    END CREATE

    CREATE button1 AS QButton
        Caption = "Click me"
        Left = 100: Top = 50: Height = 25: Width = 100
        OnClick = buttonClick
    END CREATE
END CREATE

SUB buttonClick
    STATIC count AS Integer
    count = count+1
    text.Caption = "Clicked " + STR$(count) + " times."
END SUB

form.ShowModal
```



## REBOL


```REBOL
REBOL [
	Title: "Simple Windowed Application"
	URL: http://rosettacode.org/wiki/Simple_Windowed_Application
]

clicks: 0

; Simple GUI's in REBOL can be defined with 'layout', a
; special-purpose language (dialect, in REBOL-speak) for specifying
; interfaces. In the example below, I describe a gradient background
; with a text label and a button. The block in the button section
; details what should happen when it's clicked on -- increment the
; number of clicks and update the label text.

; The 'view' function paints the layout on the screen and listens for
; events.

view layout [
	backdrop effect [gradient 0x1 black coal]

	label: vtext "There have been no clicks yet."

	button maroon "click me" [
		clicks: clicks + 1
		set-face label reform ["clicks:" clicks]
	]
]
```



## Ring


```ring

Load "guilib.ring"

MyApp = New qApp {
        num = 0
        win1 = new qWidget() {
               setwindowtitle("Hello World")
               setGeometry(100,100,370,250)

               btn1 = new qpushbutton(win1) {
                    setGeometry(150,200,100,30)
                    settext("click me")
                    setclickevent("clickme()")}

                    Lineedit1 = new qlineedit(win1) {
                                setGeometry(10,100,350,30)}
        show()}
        Exec()}

func clickme
     num += 1
     lineedit1.settext( "you clicked me " + num + " times")

```

Output:

[[File:CalmoSoftWindow.jpg]]


## Ruby

```ruby
require 'tk'
str = TkVariable.new("no clicks yet")
count = 0
root = TkRoot.new
TkLabel.new(root, "textvariable" => str).pack
TkButton.new(root) do
  text "click me"
  command {str.value = count += 1}
  pack
end
Tk.mainloop
```


```ruby
Shoes.app do
  stack do
    @count = 0
    @label = para "no clicks yet"
    button "click me" do
      @count += 1
      @label.text = "click: #@count"
    end
  end
end
```



## Run BASIC


```runbasic
msg$ = "There have been no clicks yet"
[loop] cls                                        ' clear screen
print msg$
button #c, "Click Me", [clickMe]                  'create a button with handle and goto [tag]
wait

[clickMe]
clicks = clicks + 1
msg$ = "Button has been clicked ";clicks;" times"
goto [loop]
```



## Scala

```Scala
import scala.swing.{ BorderPanel, Button, Label, MainFrame, SimpleSwingApplication }
import scala.swing.event.ButtonClicked

object SimpleApp extends SimpleSwingApplication {
  def top = new MainFrame {
    contents = new BorderPanel {
      var nClicks = 0

      val (button, label) = (new Button { text = "click me" },
        new Label { text = "There have been no clicks yet" })

      layout(button) = BorderPanel.Position.South
      layout(label) = BorderPanel.Position.Center
      listenTo(button)
      reactions += {
        case ButtonClicked(_) =>
          nClicks += 1
          label.text = s"There have been ${nClicks} clicks"
      }
    }
  }
}
```



## Sidef


```ruby
require('Gtk2') -> init

# Window.
var window = %s<Gtk2::Window>.new
window.signal_connect('destroy' => { %s<Gtk2>.main_quit })

# VBox.
var vbox = %s<Gtk2::VBox>.new(0, 0)
window.add(vbox)

# Label.
var label = %s<Gtk2::Label>.new('There have been no clicks yet.')
vbox.add(label)

# Button.
var count = 0
var button = %s<Gtk2::Button>.new(' Click Me ')
vbox.add(button)
button.signal_connect('clicked' => {
    label.set_text(++count)
})

# Show.
window.show_all

# Main loop.
%s<Gtk2>.main
```



## Smalltalk

```smalltalk
|top clickCount lh button|

clickCount := 0.
lh := ValueHolder with:'There have been no clicks yet'.

top := StandardSystemView label:'Rosetta Simple Window'.
top extent:300@100.
top add:((Label new labelChannel:lh) origin: 0 @ 10 corner: 1.0 @ 40).
top add:((button := Button label:'Eat Me') origin: 10 @ 50 corner: 100 @ 80).

button action:[
        clickCount := clickCount + 1.
        lh value: ('number of clicks: %1' bindWith:clickCount)
       ].

top open
```



## Tcl

```tcl
package require Tk
pack [label .l -text "There have been no clicks yet"]
set count 0
pack [button .b -text "click me" -command upd]
proc upd {} {
    .l configure -text "Number of clicks: [incr ::count]"
}
```


=={{header|TI-89 BASIC}}==

The Ti-89 does not have general onscreen buttons; this program uses the OK/Cancel choice of a dialog box to implement the UI.


```ti89b
Prgm
  Local clicks
  0 → clicks
  1 → ok    © System variable also set by Dialog statement
  While ok = 1
    Dialog
      Title "Rosetta Code"
      Text "There have been " & string(clicks) & " OKs"
    EndDlog
    clicks + 1 → clicks
  EndWhile
EndPrgm
```



## Unicon



```unicon

import gui
$include "guih.icn"

class SimpleApp : Dialog (label)

  # -- automatically called when the dialog is created
  method component_setup()
    # create and add the label
    label := Label("label=There have been no clicks yet","pos=50%,33%", "align=c,c")
    add (label)

    # create and add the button
    button := TextButton("label=Click me", "pos=50%,66%", "align=c,c")
    button.connect(self, "clicked", ACTION_EVENT)
    add (button)

    # some cosmetic settings for the window
    attrib("size=180,70", "bg=light gray")
  end

  method clicked ()
    static count := 0
    count +:= 1
    label.set_label ("Clicked " || count || " times")
  end
end

procedure main()
  local d := SimpleApp ()
  d.show_modal()
end

```



## Vedit macro language


```vedit
Reg_Set(10, "There have been no clicks yet")
#1 = 0
repeat (ALL) {
    #2 = Dialog_Input_1(3, "`Simple Windowed Application`,
                        `|@(10)`,
                        `[&Click me]`,`[&Exit]`",
                        APP+CENTER, 0, 0)
    if (#2 != 1) { break }    // ESC or Exit
    #1++
    Num_Str(#1, 10)
    Reg_Set(10, "Clicked", INSERT)
    Reg_Set(10, " times", APPEND)
}
```



## Visual Basic

In VB, windows are usually created in the IDE. The generated code is hidden from the user unless viewed outside of VB. For the sake of this task, I have included that code, but normally it is hidden from the programmer.


```vb
VERSION 5.00
Begin VB.Form Form2
   Caption         =   "There have been no clicks yet"
   ClientHeight    =   2940
   ClientLeft      =   60
   ClientTop       =   600
   ClientWidth     =   8340
   LinkTopic       =   "Form1"
   ScaleHeight     =   2940
   ScaleWidth      =   8340
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command1
      Caption         =   "Click me!"
      Height          =   495
      Left            =   3600
      TabIndex        =   0
      Top             =   1200
      Width           =   1215
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'-----user-written code begins here; everything above this line is hidden in the GUI-----
Private clicked As Long

Private Sub Command1_Click()
    clicked = clicked + 1
    Me.Caption = clicked & " clicks."
End Sub

```



## Web 68


```web68
@1Introduction.
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public Licence as published by
the Free Software Foundation, either version 3 of the Licence, or
(at your option) any later version.

Copyright (c) 2012 Sian Mountbatten.

@m cvs simpleapp = "$Id: $"

@a@<Prelude@>
BEGIN
@<Included declarations@>
@<Plain values in the outer reach@>
@<Names in the outer reach@>
@<Procedures in the outer reach@>
@<Logic in the outer reach@>
END
@<Postlude@>

@ The local compiler requires a special prelude.@^system dependencies@>

@<Prel...@>=
PROGRAM simpleapp CONTEXT VOID
USE @<Library preludes@>

@ And a special postlude.@^system dependencies@>

@<Post...@>=
FINISH

@ The program requires the predefined forms and the standard prelude.

@<Library...@>=
simpleapp fd,standard

@ This Web 68 file provides the Xforms prelude.

@iforms.w@>

@1Initialisation.
The initial processing consists of initialising the Xforms library.

@<Logic...@>=
open(argf,"",arg channel);
fl initialize(argc,argv,"simpleapp",NIL,0);

@ Declare the !REF FILE!.

@<Names...@>=
FILE @!argf;

@1Main processing.
The form is created in !simpleapp fd! the source code of which is 108 lines long.
Firstly, define the callback for the button.

@<Proc...@>=
button cb:=(REF FLOBJECT obj,INT data)VOID:
(
	clicks +:= 1;
	fl set object label(text box OF click form,whole(clicks,0)+" click"+
	                    (clicks=1|""|"s")+" on the button")
);

@ Declare !clicks!.

@<Plain...@>=
INT clicks:=0;

@ Create the form, show it and hand control to the Xforms library.

@<Logic...@>=
click form:=create form click;
fl show form(click OF click form,fl place center,fl full border,"SimpleApp");
fl do forms

@ Declare the form.

@<Names...@>=
REF FDCLICK click form;

@1Macro declarations.
All the macros used in the program are declared here.

@<Include...@>=
macro fl do forms;
macro fl initialize;
macro fl set object label;
macro fl show form;

@ To compile the program, use this command:

```txt

   ca -l mod -l forms simpleapp.w68

```

The predefined form will have been compiled with this command:

```txt

   ca -m mod simpleappfd.w68

```

The predefined form was created by the <b>fdesign</b> program for the Xforms library,
and the resulting form definition file was converted to Web 68 by the program
<b>fdtow68</b>.
```



## XPL0


```XPL0
include c:\cxpl\stdlib;         \standard library provides mouse routines, etc.
def Ww=40, Wh=12, Wx=(80-Ww)/2, Wy=(25-Wh)/2;           \window width, etc.
def Bw=11, Bh=4, Bx=Wx+(Ww-Bw)/2, By=Wy+3*(Wh-Bh)/4;    \button size & position
int Clicks, Mx, My;             \number of clicks and mouse coordinates

[ShowCursor(false);                             \turn off flashing cursor
Attrib($1F);                                    \bright white characters on blue
SetWind(Wx, Wy, Wx+Ww, Wy+Wh, 2, true);         \blue window with no scroll
DrawBox(Wx, Wy, Wx+Ww, Wy+Wh, 3);               \draw borders
Cursor(Wx+5, Wy+3);  Text(6, "There have been no clicks yet.");
DrawBox(Bx, By, Bx+Bw, By+Bh, 0);               \draw button
Cursor(Bx+2, By+2);  Text(6, "Click me");

OpenMouse;
ShowMouse(true);
Clicks:= 0;
repeat  if GetMouseButton(0) then               \left button down
                [while GetMouseButton(0) do []; \wait for release
                Mx:= GetMousePosition(0) / 8;   \character coordinates
                My:= GetMousePosition(1) / 8;
                if Mx>=Bx & Mx<=Bx+Bw & My>=By & My<=By+Bh then
                        [Clicks:= Clicks+1;     \mouse pointer is on the button
                        Cursor(Wx+4, Wy+3);
                        Text(6, "Times button has been clicked: ");
                        IntOut(6, Clicks);
                        ];
                ];
until   ChkKey;         \keystroke terminates program
SetVid(3);              \turn off mouse and turn on flashing cursor
]
```



## Yorick

Yorick does not include a GUI toolkit. However, it does provide a plotting system that can emulate some basic GUI features, such as buttons and labels. The above sample uses a built-in library "button.i", which is itself written in Yorick.


```yorick
#include "button.i"

window, 0;
btn_click = Button(text="click me", x=.395, y=.65, dx=0.04368, dy=0.0091);
btn_quit = Button(text="quit", x=.395, y=.6, dx=0.02184, dy=0.0091);
count = 0;
msg = "There have been no clicks yet";
finished = 0;
do {
    fma;
    plt, msg, .395, .7, justify="CH";
    button_plot, btn_click;
    button_plot, btn_quit;
    xy = mouse(0, 0, "");
    if(button_test(btn_click, xy(1), xy(2))) {
        count++;
        msg = swrite(format="Number of clicks: %d", count);
    } else if(button_test(btn_quit, xy(1), xy(2))) {
        finished = 1;
        winkill, 0;
    }
} while(!finished);
```


