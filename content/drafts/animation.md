+++
title = "Animation"
description = ""
date = 2019-09-15T15:32:55Z
aliases = []
[extra]
id = 4381
[taxonomies]
categories = []
tags = []
+++

{{task|Temporal media}}
[[Category:GUI]]
{{requires|Graphics}}

'''Animation''' is integral to many parts of [[GUI]]s, including both the fancy effects when things change used in window managers, and of course games.   The core of any animation system is a scheme for periodically changing the display while still remaining responsive to the user.   This task demonstrates this.


;Task:
Create a window containing the string "<code>Hello World! </code>" (the trailing space is significant).

Make the text appear to be rotating right by periodically removing one letter from the end of the string and attaching it to the front.

When the user clicks on the (windowed) text, it should reverse its direction.





## ActionScript


```ActionScript
//create the text box
var textBox:TextField = new TextField();
addChild(textBox);

var text = "Hello, World! ";
var goingRight = true;

//modify the string and update it in the text box
function animate(e:Event)
{
	if(goingRight)
		text = text.slice(text.length-1,text.length) + text.slice(0, text.length - 1);
	else
		text = text.slice(1) + text.slice(0,1);
	textBox.text = text;
}

//event handler to perform the animation
textBox.addEventListener(Event.ENTER_FRAME, animate);
//event handler to register clicks
textBox.addEventListener(MouseEvent.MOUSE_DOWN, function(){goingRight = !goingRight;});

```



## Ada

{{works with|GNAT}}
{{libheader|GtkAda}}

animation.adb:

```Ada
with Gtk.Main;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Button;
with Gtk.Window;
with Glib.Main;

procedure Animation is
   Scroll_Forwards : Boolean := True;

   package Button_Callbacks is new Gtk.Handlers.Callback
     (Gtk.Button.Gtk_Button_Record);

   package Label_Timeout is new Glib.Main.Generic_Sources
     (Gtk.Label.Gtk_Label);

   package Window_Callbacks is new Gtk.Handlers.Return_Callback
     (Gtk.Window.Gtk_Window_Record, Boolean);

   --  Callback for click event
   procedure On_Button_Click
     (Object : access Gtk.Button.Gtk_Button_Record'Class);

   --  Callback for delete event
   function On_Main_Window_Delete
     (Object : access Gtk.Window.Gtk_Window_Record'Class)
      return   Boolean;

   function Scroll_Text (Data : Gtk.Label.Gtk_Label) return Boolean;

   procedure On_Button_Click
     (Object : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      Scroll_Forwards := not Scroll_Forwards;
   end On_Button_Click;

   function On_Main_Window_Delete
     (Object : access Gtk.Window.Gtk_Window_Record'Class)
      return   Boolean
   is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Main_Quit;
      return True;
   end On_Main_Window_Delete;

   function Scroll_Text (Data : Gtk.Label.Gtk_Label) return Boolean is
      Text : constant String := Gtk.Label.Get_Text (Data);
   begin
      if Scroll_Forwards then
         Gtk.Label.Set_Text
           (Label => Data,
            Str   => Text (Text'First + 1 .. Text'Last) & Text (Text'First));
      else
         Gtk.Label.Set_Text
           (Label => Data,
            Str   => Text (Text'Last) & Text (Text'First .. Text'Last - 1));
      end if;
      return True;
   end Scroll_Text;

   Main_Window     : Gtk.Window.Gtk_Window;
   Text_Button     : Gtk.Button.Gtk_Button;
   Scrolling_Text  : Gtk.Label.Gtk_Label;
   Timeout_ID      : Glib.Main.G_Source_Id;
   pragma Unreferenced (Timeout_ID);

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window => Main_Window);
   Gtk.Label.Gtk_New (Label => Scrolling_Text, Str => "Hello World! ");
   Gtk.Button.Gtk_New (Button => Text_Button);
   Gtk.Button.Add (Container => Text_Button, Widget => Scrolling_Text);
   Button_Callbacks.Connect
     (Widget => Text_Button,
      Name   => "clicked",
      Marsh  => Button_Callbacks.To_Marshaller (On_Button_Click'Access));
   Timeout_ID :=
     Label_Timeout.Timeout_Add
       (Interval => 125,
        Func     => Scroll_Text'Access,
        Data     => Scrolling_Text);
   Gtk.Window.Add (Container => Main_Window, Widget => Text_Button);
   Window_Callbacks.Connect
     (Widget => Main_Window,
      Name   => "delete_event",
      Marsh  => Window_Callbacks.To_Marshaller (On_Main_Window_Delete'Access));
   Gtk.Window.Show_All (Widget => Main_Window);
   Gtk.Main.Main;
end Animation;
```



## AutoHotkey


```AutoHotkey
SetTimer, Animate ; Timer runs every 250 ms.
String := "Hello World "
Gui, Add, Text, vS gRev, %String%
Gui, +AlwaysOnTop -SysMenu
Gui, Show
Return

Animate:
	String := (!Reverse) ? (SubStr(String, 0) . Substr(String, 1, StrLen(String)-1)) : (SubStr(String, 2) . SubStr(String, 1, 1))
	GuiControl,,S, %String%
return

Rev: ; Runs whenever user clicks on the text control
	Reverse := !Reverse
return
```



## BASIC256


```BASIC256
str$="Hello, World! "
direction=0	#value of 0: to the right, 1: to the left.
tlx=10		#Top left x
tly=10		#Top left y
fastgraphics
font "Arial",20,100	#The Font configuration (Font face, size, weight)

main:
while clickb=0
	if direction=0 then
		str$=RIGHT(str$,1) + LEFT(str$,length(str$)-1)
	else
		str$=MID(str$,2,length(str$)-1)+LEFT(str$,1)
	end if
	refresh
	clg
	color red
	text tlx,tly,str$
	pause .1
end while
	#Note: textheight() and textwidth() depends on the latest configuration of the FONT command.
if clickb=1 and clickx>=tlx and clickx<=tlx+textwidth(str$) and clicky>=tly and clicky<=tly+textheight() then
	direction=NOT direction
end if
clickclear
goto main
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      VDU 23,22,212;40;16,32,16,128
      txt$ = "Hello World! "
      direction% = TRUE
      ON MOUSE direction% = NOT direction% : RETURN
      OFF
      REPEAT
        CLS
        PRINT txt$;
        IF direction% THEN
          txt$ = RIGHT$(txt$) + LEFT$(txt$)
        ELSE
          txt$ = MID$(txt$,2) + LEFT$(txt$,1)
        ENDIF
        WAIT 20
      UNTIL FALSE
```



## C

{{libheader|GTK}}
(NB: implicitly, through GTK, it uses also Pango library)

```cpp
#include <iostream>
#include <string.h>
#include <gtk/gtk.h>

const gchar *hello = "Hello World! ";
gint direction = -1;
gint cx=0;
gint slen=0;

GtkLabel *label;

void change_dir(GtkLayout *o, gpointer d)
{
  direction = -direction;
}

gchar *rotateby(const gchar *t, gint q, gint l)
{
  gint i, cl = l, j;
  gchar *r = malloc(l+1);
  for(i=q, j=0; cl > 0; cl--, i = (i + 1)%l, j++)
    r[j] = t[i];
  r[l] = 0;
  return r;
}

gboolean scroll_it(gpointer data)
{
  if ( direction > 0 )
    cx = (cx + 1) % slen;
  else
    cx = (cx + slen - 1 ) % slen;
  gchar *scrolled = rotateby(hello, cx, slen);
  gtk_label_set_text(label, scrolled);
  free(scrolled);
  return TRUE;
}


int main(int argc, char **argv)
{
  GtkWidget *win;
  GtkButton *button;
  PangoFontDescription *pd;

  gtk_init(&argc, &argv);
  win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(win), "Basic Animation");
  g_signal_connect(G_OBJECT(win), "delete-event", gtk_main_quit, NULL);

  label = (GtkLabel *)gtk_label_new(hello);

  // since we shift a whole character per time, it's better to use
  // a monospace font, so that the shifting seems done at the same pace
  pd = pango_font_description_new();
  pango_font_description_set_family(pd, "monospace");
  gtk_widget_modify_font(GTK_WIDGET(label), pd);

  button = (GtkButton *)gtk_button_new();
  gtk_container_add(GTK_CONTAINER(button), GTK_WIDGET(label));

  gtk_container_add(GTK_CONTAINER(win), GTK_WIDGET(button));
  g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(change_dir), NULL);

  slen = strlen(hello);

  g_timeout_add(125, scroll_it, NULL);

  gtk_widget_show_all(GTK_WIDGET(win));
  gtk_main();
  return 0;
}
```



## C#



```c#
using System;
using System.Drawing;
using System.Windows.Forms;

namespace BasicAnimation
{
  class BasicAnimationForm : Form
  {
    bool isReverseDirection;
    Label textLabel;
    Timer timer;

    internal BasicAnimationForm()
    {
      this.Size = new Size(150, 75);
      this.Text = "Basic Animation";

      textLabel = new Label();
      textLabel.Text = "Hello World! ";
      textLabel.Location = new Point(3,3);
      textLabel.AutoSize = true;
      textLabel.Click += new EventHandler(textLabel_OnClick);
      this.Controls.Add(textLabel);

      timer = new Timer();
      timer.Interval = 500;
      timer.Tick += new EventHandler(timer_OnTick);
      timer.Enabled = true;

      isReverseDirection = false;
    }

    private void timer_OnTick(object sender, EventArgs e)
    {
      string oldText = textLabel.Text, newText;
      if(isReverseDirection)
        newText = oldText.Substring(1, oldText.Length - 1) + oldText.Substring(0, 1);
      else
        newText = oldText.Substring(oldText.Length - 1, 1) + oldText.Substring(0, oldText.Length - 1);
      textLabel.Text = newText;
    }

    private void textLabel_OnClick(object sender, EventArgs e)
    {
      isReverseDirection = !isReverseDirection;
    }
  }

   class Program
   {
      static void Main()
      {
	Application.Run(new BasicAnimationForm());
      }
   }
}
```



## Ceylon

module.ceylon

```ceylon
native("jvm")
module animation "1.0.0" {
	import java.desktop "8";
}

```

run.ceylon

```ceylon
import javax.swing {
	JFrame,
	JLabel,
	Timer
}
import java.awt.event {
	MouseAdapter,
	MouseEvent,
	ActionListener,
	ActionEvent
}

shared void run() {
 	value initialText = "Hello World! ";
 	value label = JLabel(initialText);
	variable value forward = true;
	label.addMouseListener(object extends MouseAdapter() {
        	mouseClicked(MouseEvent? mouseEvent) =>
                	forward = !forward;
	});

	Timer(1k, object satisfies ActionListener {
		shared actual void actionPerformed(ActionEvent? actionEvent) {
			String left;
			String right;
			if(forward) {
				left = label.text.last?.string else "?";
				right = "".join(label.text.exceptLast);
			} else {
				left = label.text.rest;
				right = label.text.first?.string else "?";
			}
			label.text = left + right;
		}
	}).start();

	value frame = JFrame();
	frame.defaultCloseOperation = JFrame.\iEXIT_ON_CLOSE;
	frame.title = "Rosetta Code Animation Example";
	frame.add(label);
	frame.pack();
	frame.visible = true;
}
```



## Clojure

Clojure is a JVM language so this example uses Swing, and illustrates Clojure's platform integration.

```clojure
(import '[javax.swing JFrame JLabel])
(import '[java.awt.event MouseAdapter])

(def text "Hello World! ")
(def text-ct (count text))
(def rotations
  (vec
    (take text-ct
      (map #(apply str %)
        (partition text-ct 1 (cycle text))))))

(def pos (atom 0))  ;position in rotations vector being displayed
(def dir (atom 1))  ;direction of next position (-1 or 1)

(def label (JLabel. text))

(.addMouseListener label
  (proxy [MouseAdapter] []
    (mouseClicked [evt] (swap! dir -))))

(defn animator []
  (while true
    (Thread/sleep 100)
    (swap! pos #(-> % (+ @dir) (mod text-ct)))
    (.setText label (rotations @pos))))

(doto (JFrame.)
  (.add label)
  (.pack)
  (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
  (.setVisible true))

(future-call animator)  ;simple way to run animator on a separate thread
```



## Common Lisp

The ltk package provides a lisp interface to Tk for creating graphical interfaces. Assuming ''ltk'' has been installed somewhere the following will work as per the Tcl example.
{{libheader|Tk}}

```lisp
(use-package :ltk)

(defparameter *message* "Hello World! ")
(defparameter *direction* :left)
(defun animate (label)
  (let* ((n (length *message*))
         (i (if (eq *direction* :left) 0 (1- n)))
         (c (char *message* i)))
    (if (eq *direction* :left)
        (setq *message* (concatenate 'string
				     (subseq *message* 1 n)
				     (list c)))
	(setq *message* (concatenate 'string (list c)
				     (subseq *message* 0 (1- n)))))
    (setf (ltk:text label) *message*)
    (ltk:after 125 (lambda () (animate label)))))

(defun basic-animation ()
  (ltk:with-ltk ()
      (let* ((label (make-instance 'label
                                   :font "Courier 14")))
        (setf (text label) *message*)
        (ltk:bind label "<Button-1>"
                  (lambda (event)
                    (declare (ignore event))
                    (cond
                     ((eq *direction* :left) (setq *direction* :right))
                     ((eq *direction* :right) (setq *direction* :left)))))
        (ltk:pack label)
        (animate label)
        (ltk:mainloop))))

(basic-animation)
```



## D

{{libheader|QD}}
uses {{libheader|SDL}}
{{libheader|SDL_ttf}}
{{libheader|tools}}

```d
module test26;

import qd, SDL_ttf, tools.time;

void main() {
  screen(320, 200);
  auto last = sec();
  string text = "Hello World! ";
  auto speed = 0.2;
  int dir = true;
  while (true) {
    cls;
    print(10, 10, Bottom|Right, text);
    if (sec() - last > speed) {
      last = sec();
      if (dir == 0) text = text[$-1] ~ text[0 .. $-1];
      else text = text[1 .. $] ~ text[0];
    }
    flip; events;
    if (mouse.clicked
      && mouse.pos in display.select(10, 10, 100, 20)
    ) dir = !dir;
  }
}
```



## Dart



```dart

import 'dart:html';
import 'dart:async';

const frameTime = const Duration(milliseconds: 100);

void main() {
  String text = "Hello World! ";
  bool rotateRight = true;

  Element writeHere =
      querySelector('#output'); // assumes you have a pre with that ID
  writeHere.onClick.listen((event) => rotateRight = !rotateRight);

  new Timer.periodic(frameTime, (_) {
    text = changeText(text, rotateRight);
    writeHere.text = text;
  });
}

String changeText(extt, rotateRight) {
  if (rotateRight) {
    return extt.substring(extt.length - 1) + extt.substring(0, extt.length - 1);
  } else {
    return extt.substring(1) + extt.substring(0, 1);
  }
}

```



## E


{{works with|E-on-Java}} (Java Swing; tested on Mac OS X 10.5.7)


```e
# State
var text := "Hello World! "
var leftward := false

# Window
def w := <swing:makeJFrame>("RC: Basic Animation")

# Text in window
w.setContentPane(def l := <swing:makeJLabel>(text))
l.setOpaque(true) # repaints badly if not set!
l.addMouseListener(def mouseListener {
    to mouseClicked(_) {
        leftward := !leftward
    }
    match _ {}
})

# Animation
def anim := timer.every(100, fn _ { # milliseconds
    def s := text.size()
    l.setText(text := if (leftward) {
        text(1, s) + text(0, 1)
    } else {
        text(s - 1, s) + text(0, s - 1)
    })
})

# Set up window shape and close behavior
w.pack()
w.setLocationRelativeTo(null)
w.addWindowListener(def windowListener {
    to windowClosing(_) { anim.stop() }
    match _ {}
})

# Start everything
w.show()
anim.start()
```


Text-only version (no Java dependency; no clicking, use reverse() and stop() to control):


```e
def [reverse, stop] := {
    var text := "Hello World! "
    var leftward := false

    def anim := timer.every(100, fn _ { # milliseconds
        def s := text.size()
        text := if (leftward) {
            text(1, s) + text(0, 1)
        } else {
            text(s - 1, s) + text(0, s - 1)
        }
        print("\b" * s, text)
    })
    print("\n", text)
    anim.start()
    [def _() { leftward := !leftward; null }, anim.stop]
}
```


=={{header|F Sharp|F#}}==
{{libheader|Windows Presentation Foundation}}

```fsharp
open System.Windows

let str = "Hello world! "
let mutable i = 0
let mutable d = 1

[<System.STAThread>]
do
  let button = Controls.Button()
  button.Click.Add(fun _ -> d <- str.Length - d)
  let update _ =
    i <- (i + d) % str.Length
    button.Content <- str.[i..] + str.[..i-1]
  Media.CompositionTarget.Rendering.Add update
  (Application()).Run(Window(Content=button)) |> ignore
```



## Factor


```Factor
USING: accessors timers calendar kernel models sequences ui
ui.gadgets ui.gadgets.labels ui.gestures ;
FROM: models => change-model ;
IN: rosetta.animation

CONSTANT: sentence "Hello World! "

TUPLE: animated-label < label-control reversed alarm ;
: <animated-label> ( model -- <animated-model> )
    sentence animated-label new-label swap >>model
    monospace-font >>font ;
: update-string ( str reverse -- str )
    [ unclip-last prefix ] [ unclip suffix ] if ;
: update-model ( model reversed? -- )
    [ update-string ] curry change-model ;

animated-label
    H{
        { T{ button-down } [ [ not ] change-reversed drop ] }
     } set-gestures

M: animated-label graft*
  [ [ [ model>> ] [ reversed>> ] bi update-model ] curry 400 milliseconds every ] keep
  alarm<< ;
M: animated-label ungraft*
    alarm>> stop-timer ;
: main ( -- )
   [ sentence <model> <animated-label> "Rosetta" open-window ] with-ui ;

MAIN: main
```



## Fantom



```fantom

using concurrent
using fwt
using gfx

const class RotateString : Actor
{
  new make (Label label) : super (ActorPool ())
  {
    Actor.locals["rotate-label"] = label
    Actor.locals["rotate-string"] = label.text
    Actor.locals["direction"] = "forward"
    sendLater (1sec, "update")
  }

  // responsible for calling appropriate methods to process each message
  override Obj? receive (Obj? msg)
  {
    switch (msg)
    {
      case "update":
        Desktop.callAsync |->| { update }  // make sure we update GUI in event thread
        sendLater (1sec, "update")
      case "reverse":
        Desktop.callAsync |->| { reverse }
    }

    return null
  }

  // change the stored string indicating the direction to rotate
  Void reverse ()
  {
    Actor.locals["direction"] =
        (Actor.locals["direction"] == "forward" ? "backward" : "forward")
  }

  // update the text on the label according to the stored direction
  Void update ()
  {
    label := Actor.locals["rotate-label"] as Label
    str := Actor.locals["rotate-string"] as Str
    if (label != null)
    {
      newStr := ""
      if (Actor.locals["direction"] == "forward")
        newStr = str[1..-1] + str[0].toChar
      else
        newStr = str[-1].toChar + str[0..<-1]
      label.text = newStr
      Actor.locals["rotate-string"] = newStr
    }
  }
}

class Animate
{
  public static Void main ()
  {
    label := Label
    {
      text = "Hello world! "
      halign = Halign.center
    }
    ticker := RotateString (label)
    label.onMouseDown.add |Event e|
    {
      ticker.send ("reverse")
    }
    Window
    {
      title = "Animate"
      label,
    }.open
  }
}

```



## FBSL



```qbasic>#INCLUDE <Include\Windows.inc


FBSLSETTEXT(ME, "Hello world! ")
RESIZE(ME, 0, 0, 220, 0)
CENTER(ME)
SHOW(ME)
SetTimer(ME, 1000, 100, NULL)

BEGIN EVENTS
	STATIC bForward AS BOOLEAN = TRUE
	IF CBMSG = WM_TIMER THEN
		Marquee(bForward)
		RETURN 0
	ELSEIF CBMSG = WM_NCLBUTTONDOWN THEN
		IF CBWPARAM = HTCAPTION THEN bForward = NOT bForward
	ELSEIF CBMSG = WM_CLOSE THEN
		KillTimer(ME, 1000)
	END IF
END EVENTS

SUB Marquee(BYVAL forward AS BOOLEAN)
	STATIC caption AS STRING = FBSLGETTEXT(ME)
	STATIC length AS INTEGER = STRLEN(caption)
	IF forward THEN
		caption = RIGHT(caption, 1) & LEFT(caption, length - 1)
	ELSE
		caption = MID(caption, 2) & LEFT(caption, 1)
	END IF
	FBSLSETTEXT(ME, caption)
END SUB
```




## FreeBASIC


Any mouse button on the text will change the direction.

To exit, push the window's closing cross. (255 + 107 is the combination that is passed to INKEY$ by that button.)


```freebasic
DIM C AS STRING = "Hello World! ", SIZE AS USHORT = LEN(C)
DIM DIRECTION AS BYTE = 0
DIM AS INTEGER X, Y, BTNS
DIM HELD AS BYTE = 0

SCREEN 19

DO
 LOCATE 1, 1
 PRINT C

 GETMOUSE X, Y, , BTNS

 IF BTNS <> 0 AND HELD = 0 THEN
  'remember if it was pressed, to not react every frame
  HELD = 1
  IF X >= 0 AND X < SIZE * 8 AND Y >= 0 AND Y < 16 THEN
   DIRECTION = 1 - DIRECTION
  END IF
 ELSE
  HELD = 0
 END IF

 IF INKEY = CHR(255) + CHR(107) THEN EXIT DO

 IF DIRECTION = 0 THEN
  C = RIGHT(C, 1) + LEFT(C, SIZE - 1)
 ELSE
  C = RIGHT(C, SIZE - 1) + LEFT(C, 1)
 END IF

 SLEEP 100, 1
LOOP
```



## Gambas


```gambas
'This code will create the necessary controls on a GUI Form

hLabel As Label                                   'Label needed to display the 'Hello World! " message
hTimer As Timer                                   'Timer to rotate the display
bDirection As Boolean                             'Used to control the direction of the text
'__________________________________________________
Public Sub Form_Open()
Dim hPanel As Panel                               '2 Panels used to centre the Label vertically on the Form
Dim hHBox As HBox                                 'Box to hold the Label

With Me                                           'Set the Form Properties
  .Arrangement = Arrange.Vertical                 'Arrange controls vertically
  .Title = "Animation"                            'Give the Form a Title
  .Height = 75                                    'Set the Height of the Form
  .Width = 225                                    'Set the Width of the Form
End With

hPanel = New Panel(Me)                            'Add a Panel to the Form
HPanel.Expand = True                              'Expand the Panel

hHBox = New HBox(Me)                              'Add a HBox to the Form
hHBox.Height = 28                                 'Set the HBox Height

hLabel = New Label(hHBox) As "LabelAnimation"     'Add new Lable with Event name

With hLabel                                       'Change the hLabel properties
  .Height = 35                                    'Set the Height of the Label
  .Expand = True                                  'Expand the hLabel
  .Font.Bold = True                               'Set the Font to Bold
  .Font.size = 20                                 'Set the Font Size
  .Alignment = Align.Center                       'Centre align the text
  .Tooltip = "Click me to reverse direction"      'Add a Tooltip
  .Border = Border.Plain                          'Add a Border
  .Text = "Hello World! "                         'Add the Text
End With

hPanel = New Panel(Me)                            'Add another Panel
HPanel.Expand = True                              'Expand the Panel

hTimer = New Timer As "Timer1"                    'Add a Timer
hTimer.delay = 500                                'Set the Timer Delay
hTimer.Start                                      'Start the Timer

End
'__________________________________________________
Public Sub LabelAnimation_MouseDown()             'If the hLabel is clicked..

bDirection = Not bDirection                       'Change the value of bDirection

End
'__________________________________________________
Public Sub Timer1_Timer()                         'Timer
Dim sString As String = hLabel.Text               'To hold the text in the Label
Dim sTemp As String                               'Temp string

If bDirection Then                                'If the text is to rotate left then..
  sTemp = Left(sString, 1)                        'Get the first charater of the Label Text e.g 'H'
  sString = Right(sString, -1) & sTemp            'Recreate sString with all the text less the 1st character e.g. 'ello World! ' and add the 1st character to the end e.g. 'ello World! H'
Else                                              'Else if text is to rotate right then..
  sTemp = Right(sString, 1)                       'Get the last charater of the Label Text e.g '!'
  sString = sTemp & Left(sString, -1)             'Recreate sString with all the text less the last character e.g. ' Hello World' and add the last character to the begining e.g. '! Hello World'
End If

hLabel.text = sString                             'Display the result

End
```

'''[http://www.cogier.com/gambas/Animation.png Click here for image of running code]'''


## Go


```go
package main

import (
    "log"
    "time"

    "github.com/gdamore/tcell"
)

const (
    msg             = "Hello World! "
    x0, y0          = 8, 3
    shiftsPerSecond = 4
    clicksToExit    = 5
)

func main() {
    s, err := tcell.NewScreen()
    if err != nil {
        log.Fatal(err)
    }
    if err = s.Init(); err != nil {
        log.Fatal(err)
    }
    s.Clear()
    s.EnableMouse()
    tick := time.Tick(time.Second / shiftsPerSecond)
    click := make(chan bool)
    go func() {
        for {
            em, ok := s.PollEvent().(*tcell.EventMouse)
            if !ok || em.Buttons()&0xFF == tcell.ButtonNone {
                continue
            }
            mx, my := em.Position()
            if my == y0 && mx >= x0 && mx < x0+len(msg) {
                click <- true
            }
        }
    }()
    for inc, shift, clicks := 1, 0, 0; ; {
        select {
        case <-tick:
            shift = (shift + inc) % len(msg)
            for i, r := range msg {
                s.SetContent(x0+((shift+i)%len(msg)), y0, r, nil, 0)
            }
            s.Show()
        case <-click:
            clicks++
            if clicks == clicksToExit {
                s.Fini()
                return
            }
            inc = len(msg) - inc
        }
    }
}
```



## Haskell

Using simple graphics {{libheader|HGL}} from [http://hackage.haskell.org/packages/hackage.html HackageDB]

```haskell
import Graphics.HGL.Units (Time, Point, Size, )
import Graphics.HGL.Draw.Monad (Graphic, )
import Graphics.HGL.Draw.Text
import Graphics.HGL.Draw.Font
import Graphics.HGL.Window
import Graphics.HGL.Run
import Graphics.HGL.Utils

import Control.Exception (bracket, )

runAnim = runGraphics $
 bracket
  (openWindowEx "Basic animation task" Nothing (250,50) DoubleBuffered (Just 110))
  closeWindow
  (\w -> do
    f <- createFont (64,28) 0 False False "courier"
    let loop t dir = do
	  e <- maybeGetWindowEvent w
	  let d = case e of
		  Just (Button _ True False)  -> -dir
		  _ -> dir
	      t' = if d == 1 then last t : init t else tail t ++ [head t]
	  setGraphic w (withFont f $ text (5,10) t') >> getWindowTick w
	  loop  t' d

    loop "Hello world ! " 1  )
```

Run within interpreter GHCi:

```haskell
*Main> runAnim
```



## HicEst


```hicest
CHARACTER string="Hello World! "

   WINDOW(WINdowhandle=wh, Height=1, X=1, TItle="left/right click to rotate left/right, Y-click-position sets milliseconds period")
   AXIS(WINdowhandle=wh, PoinT=20, X=2048, Y, Title='ms', MiN=0, MaX=400, MouSeY=msec, MouSeCall=Mouse_callback, MouSeButton=button_type)
   direction = 4
   msec = 100    ! initial milliseconds
   DO tic = 1, 1E20
      WRITE(WIN=wh, Align='Center Vertical') string
      IF(direction == 4) string = string(LEN(string)) // string ! rotate left
      IF(direction == 8) string = string(2:) // string(1)       ! rotate right
      WRITE(StatusBar, Name) tic, direction, msec
      SYSTEM(WAIT=msec)
   ENDDO
 END

SUBROUTINE Mouse_callback()
   direction = button_type ! 4 == left button up, 8 == right button up
 END
```


=={{header|Icon}} and {{header|Unicon}}==
The following code uses features exclusive to Unicon.


```Unicon
import gui
$include "guih.icn"

class WindowApp : Dialog (label, direction)

  method rotate_left (msg)
    return msg[2:0] || msg[1]
  end

  method rotate_right (msg)
    return msg[-1:0] || msg[1:-1]
  end

  method reverse_direction ()
    direction := 1-direction
  end

  # this method gets called by the ticker, and updates the label
  method tick ()
    static msg := "Hello World! "
    if direction = 0
      then msg := rotate_left (msg)
      else msg := rotate_right (msg)
    label.set_label(msg)
  end

  method component_setup ()
    direction := 1 # start off rotating to the right
    label := Label("label=Hello World! ", "pos=0,0")
    # respond to a mouse click on the label
    label.connect (self, "reverse_direction", MOUSE_RELEASE_EVENT)
    add (label)

    connect (self, "dispose", CLOSE_BUTTON_EVENT)
    # tick every 100ms
    self.set_ticker (100)
  end
end

# create and show the window
procedure main ()
  w := WindowApp ()
  w.show_modal ()
end

```


The following code uses features exclusive to Icon.

```Icon

link graphics

procedure main()
    s := "Hello World! "
    WOpen("size=640,400", "label=Animation")
    Font("typewriter,60,bold")
    direction := 1
    w := TextWidth(s)
    h := WAttrib("fheight")
    x := (WAttrib("width") - w) / 2
    y := (WAttrib("height") - 20 + h) / 2

    repeat
    {   if *Pending() > 0 then if (Event() = &lrelease) & (x < &x < x + w) & (y > &y > y-h) then direction := ixor(direction, 1)
        s := s[2 - 3 * direction:0] || s[1:2 - 3 * direction]
        EraseArea(x, y, w, -h)
        DrawString(x,y - WAttrib("descent")-1,s)
        WFlush()
        delay(250)
    }
end

```



## J

{{works with|J8}}

```j
coinsert'jgl2' [ require'gl2'

MESSAGE        =: 'Hello World! '
TIMER_INTERVAL =: 0.5 * 1000                                          NB. Milliseconds
DIRECTION      =: -1                                                  NB. Initial direction is right -->

ANIM           =: noun define
  pc anim closeok;pn "Basic Animation in J";
  minwh 350 5;
  cc isi isigraph flush;
  pcenter;pshow;
)

anim_run        =: verb def 'wd ANIM,''; ptimer '',":TIMER_INTERVAL'  NB. Set form timer
anim_timer      =: verb def 'glpaint MESSAGE=: DIRECTION |. MESSAGE'  NB. Rotate MESSAGE according to DIRECTION
anim_isi_mbldown=: verb def 'DIRECTION=: - DIRECTION'                 NB. Reverse direction when user clicks
anim_close      =: verb def 'wd ''timer 0; pclose; reset'' '          NB. Shut down timer
anim_isi_paint  =:  verb define
  glclear ''                                                          NB.  Clear out old drawing
  glrgb 255 0 0
  gltextcolor''
  glfont '"courier new" 36'
  gltext MESSAGE
)

anim_run ''
```


{{works with|J6}}

```j
coinsert'jgl2' [ require'gl2'

MESSAGE          =:  'Hello World! '
TIMER_INTERVAL   =:  0.5 * 1000                                            NB.  Milliseconds
DIRECTION        =:  -1                                                    NB.  Initial direction is right -->

ANIM             =:  noun define
  pc anim nomax nosize;pn "Basic Animation in J";
  xywh 1 1 174 24;cc isi isigraph rightmove bottommove;
  pas 0 0;pcenter;pshow;
)

anim_run         =:  verb def ' wd ANIM,''; timer '',":TIMER_INTERVAL '
sys_timer_z_     =:  verb def ' isiMsg MESSAGE=:  DIRECTION |. MESSAGE '   NB.  Rotate MESSAGE according to DIRECTION
anim_isi_mbldown =:  verb def ' DIRECTION=:  - DIRECTION '                 NB.  Reverse direction when user clicks
anim_close       =:  verb def ' wd ''timer 0; pclose; reset;'' '           NB.  Shut down timer

isiMsg           =:  verb define
  wd'psel anim'
  glclear ''                                                               NB.  Clear out old drawing
  glfont '"courier new" 36'
  gltext y
  glpaint ''                                                               NB.  Copy to screen
)

anim_run ''
```



## Java

{{libheader|Swing}}

```java
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Timer;
import java.util.TimerTask;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.WindowConstants;

public class Rotate {

    private static class State {
        private final String text = "Hello World! ";
        private int startIndex = 0;
        private boolean rotateRight = true;
    }

    public static void main(String[] args) {
        State state = new State();

        JLabel label = new JLabel(state.text);
        label.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent event) {
                state.rotateRight = !state.rotateRight;
            }
        });

        TimerTask task = new TimerTask() {
            public void run() {
                int delta = state.rotateRight ? 1 : -1;
                state.startIndex = (state.startIndex + state.text.length() + delta) % state.text.length();
                label.setText(rotate(state.text, state.startIndex));
            }
        };
        Timer timer = new Timer(false);
        timer.schedule(task, 0, 500);

        JFrame rot = new JFrame();
        rot.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        rot.add(label);
        rot.pack();
        rot.setLocationRelativeTo(null);
        rot.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosed(WindowEvent e) {
                timer.cancel();
            }
        });
        rot.setVisible(true);
    }

    private static String rotate(String text, int startIdx) {
        char[] rotated = new char[text.length()];
        for (int i = 0; i < text.length(); i++) {
            rotated[i] = text.charAt((i + startIdx) % text.length());
        }
        return String.valueOf(rotated);
    }
}
```


=={{header|JavaScript}} + {{header|HTML}}==

```javascript><html> <head

    <title>RC: Basic Animation</title>
    <script type="text/javascript">
        function animate(id) {
            var element = document.getElementById(id);
            var textNode = element.childNodes[0]; // assuming no other children

            var text = textNode.data;
            var reverse = false;

            element.onclick = function () { reverse = !reverse; };

            setInterval(function () {
                if (reverse)
                    text = text.substring(1) + text[0];
                else
                    text = text[text.length - 1] + text.substring(0, text.length - 1);
                textNode.data = text;
            }, 100);
        }
    </script>
</head> <body onload="animate('target')">
    <pre id="target">Hello World!
```

</body> </html>
```


=={{header|JavaScript}} + {{header|SVG}}==

```javascript
<svg xmlns="http://www.w3.org/2000/svg"
     width="100" height="40">
    <script type="text/javascript">
        function animate(element) {
            var textNode = element.childNodes[0]; // assuming no other children
            var text = textNode.data;
            var reverse = false;

            element.onclick = function () { reverse = !reverse; };

            setInterval(function () {
                if (reverse)
                    text = text.substring(1) + text[0];
                else
                    text = text[text.length - 1] + text.substring(0, text.length - 1);
                textNode.data = text;
            }, 100);
        }
    </script>

    <rect width="100" height="40" fill="yellow"/>
    <text x="2" y="20" onload="animate(this);">Hello World! </text>
</svg>
```



## Julia

{{libheader|Julia/Tk}}

```Julia

using Tk

const frameinterval = 0.12 # partial seconds between change on screen display

function windowanim(stepinterval::Float64)
    wind = Window("Animation", 300, 100)
    frm = Frame(wind)
    hello = "Hello World!                                           "
    but = Button(frm, width=30, text=hello)
    rightward = true
    callback(s) = (rightward = !rightward)
    bind(but, "command", callback)
    pack(frm, expand=true, fill = "both")
    pack(but, expand=true, fill = "both")
    permut = [hello[i:end] * hello[1:i-1] for i in length(hello)+1:-1:2]
    ppos = 1
    pmod = length(permut)
    while true
        but[:text] = permut[ppos]
        sleep(stepinterval)
        if rightward
            ppos += 1
            if ppos > pmod
                ppos = 1
            end
        else
            ppos -= 1
            if ppos < 1
                ppos = pmod
            end
        end
    end
end

windowanim(frameinterval)

```

{{libheader|Julia/Gtk}}

```julia

using Gtk.ShortNames

const frameinterval = 0.12 # partial seconds between change on screen display

function textanimation(stepinterval::Float64)
    hello = "Hello World!                        "
    win = Window("Animation", 210, 40) |> (Frame() |> (but = Button("Switch Directions")))
    rightward = true
    switchdirections(s) = (rightward = !rightward)
    signal_connect(switchdirections, but, "clicked")
    permut = [hello[i:end] * hello[1:i-1] for i in length(hello)+1:-1:2]
    ppos = 1
    pmod = length(permut)
    nobreak = true
    endit(w) = (nobreak = false)
    signal_connect(endit, win, :destroy)
    showall(win)
    while nobreak
        setproperty!(but, :label, permut[ppos])
        sleep(stepinterval)
        if rightward
            ppos += 1
            if(ppos > pmod)
                ppos = 1
            end
        else
            ppos -= 1
            if(ppos < 1)
                ppos = pmod
            end
        end
    end
end

textanimation(frameinterval)

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.0

import java.awt.Dimension
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.util.*
import javax.swing.JFrame
import javax.swing.JLabel

class Rotate : JFrame() {
    val text = "Hello World! "
    val label = JLabel(text)
    var rotRight = true
    var startIdx = 0

    init {
        preferredSize = Dimension(96, 64)
        label.addMouseListener(object: MouseAdapter() {
            override fun mouseClicked(evt: MouseEvent) {
                rotRight = !rotRight
            }
        })
        add(label)
        pack()
        defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        isVisible = true
    }
}

fun getRotatedText(text: String, startIdx: Int): String {
    val ret = StringBuilder()
    var i = startIdx
    do {
        ret.append(text[i++])
        i %= text.length
    }
    while (i != startIdx)
    return ret.toString()
}

fun main(args: Array<String>) {
    val rot = Rotate()
    val task = object : TimerTask() {
        override fun run() {
            if (rot.rotRight) {
                if (--rot.startIdx < 0) rot.startIdx += rot.text.length
            }
            else {
                if (++rot.startIdx >= rot.text.length) rot.startIdx -= rot.text.length
            }
            rot.label.text = getRotatedText(rot.text, rot.startIdx)
        }
    }
    Timer(false).schedule(task, 0, 500)
}
```



## LabVIEW

{{VI snippet}}<br/>
[[File:LabVIEW Animation.png]]

## Liberty BASIC


```lb
    txt$ = "Hello World! "
    txtLength = len(txt$)
    direction=1

    NoMainWin

    open "Rosetta Task: Animation" for graphics_nsb as #demo
    #demo "Trapclose [quit]"
    #demo "down"
    #demo "Font Verdana 20 Bold"
    #demo "When leftButtonUp [changedirection]"

    timer 150 , [draw]
    wait

[draw]
    if direction then
        txt$ = right$(txt$, 1);left$(txt$, txtLength - 1)
    else
        txt$ = right$(txt$, txtLength - 1);left$(txt$, 1)
    end if
    #demo "discard"
    #demo "place 50 100"
    #demo "\";txt$
    wait

[changedirection]
    direction = not(direction)
    wait

[quit]
    timer 0
    close #demo
    end
```



## Logo

{{works with|UCB Logo}}

```logo
to rotate.left :thing
  output lput first :thing butfirst :thing
end
to rotate.right :thing
  output fput last :thing butlast :thing
end

make "text "|Hello World! |
make "right? "true

to step.animation
  label :text			; graphical
  ; type char 13  type :text	; textual
  wait 6			; 1/10 second
  if button <> 0 [make "right? not :right?]
  make "text ifelse :right? [rotate.right :text] [rotate.left :text]
end

hideturtle
until [key?] [step.animation]
```



## M2000 Interpreter


### Using Blink Event

Any Button can use a blink, a timer which return a changed value, true or false, but here we didn't use it (so we didn't Read from event's stack of values. We can place Stack statement to view this stack in console).


```M2000 Interpreter

Module UseBlink {
      Def boolean direction=True
      rotating$ =lambda$ a$="Hello World! " (direction as boolean)->{
            =a$
            a$=if$(direction->right$(a$,1)+mid$(a$,1, len(a$)-1), mid$(a$,2)+left$(a$,1))
      }
      Declare MyForm Form
      Declare MyButton Button  Form MyForm
      With MyButton, "Caption" as MyButtonCaption$, "Blink", 200
      Method MyForm,"Move", 1000,1000,6000,4000
      Method MyButton,"Move", 1000,1700,4000,600
      Function MyButton.Blink {
            Rem Stack : Refresh  ' to refresh the console window
            MyButtonCaption$=rotating$(direction)
      }
      Function MyButton.Click {
            direction~
      }
      Function MyForm.Click {
            direction~
      }
      With MyForm, "Title", "Animation"
      Method MyForm, "Show", 1
      Threads Erase
}
UseBlink


```


### Using Every Statement

We can use console as window too. Although we don't have events for this window, we can read mouse button. Also we use split screen so first line (line 0) has the title, and all other lines are cleared before we print on it, every 200ms


```M2000 Interpreter

Module UseEvery {
            Window 16, 6000,4000;
            Print "Animation"
            Def boolean direction=True
            rotating$ =lambda$ a$="Hello World! " (direction as boolean)->{
                  =a$
                  a$=if$(direction->right$(a$,1)+mid$(a$,1, len(a$)-1), mid$(a$,2)+left$(a$,1))
            }
            Every 200 {
                  Cls #225577,1
                  Print @(2,height/2),rotating$(direction)
                  if mouse=1 then direction~
                  if mouse=2 then exit
            }
}
UseEvery

```



### Using Thread

We use form layer as console (we can't use input statements for those layers). We setup a thread to change form layer, at an interval of 200ms.

When a thread created saves the current layer, to use it in each iteration.


```M2000 Interpreter

Module UseThread {
      Def boolean direction=True
      rotating$ =lambda$ a$="Hello World! " (direction as boolean)->{
            =a$
            a$=if$(direction->right$(a$,1)+mid$(a$,1, len(a$)-1), mid$(a$,2)+left$(a$,1))
      }
      Declare MyForm Form
      Layer MyForm {
            Thread {
                  Cls #225577,0
                  Print @(2,height/2),rotating$(direction)
            } as M interval 200
      }
      Function MyForm.Click {
            direction~
      }

      With MyForm, "Title", "Animation"

      Method MyForm, "Show", 1
      Threads Erase
}
UseThread

```



## Maple

First, create a textbox, and by right-clicking it, and selecting component properties, change its name to "Text". Then, create 2 buttons, changing the caption on one to "Reverse", and the other to "Forward". In the edit click action of each respective button, you will put:

```Maple

ScrollText(GP("Text",value),"Forward",65);

```

and:

```Maple

ScrollText(GP("Text",value),"Reverse",65);

```

Then in the startup actions, accessed by clicking the 2 gears, add this:

```Maple

macro(SP=DocumentTools:-SetProperty, GP=DocumentTools:-GetProperty);
SP("Text",value,"Hello World! ");
ScrollText := proc( msg, direction::identical("Forward","Reverse"),n::posint:=20 )
    local word, count;
    word:=msg;
    count:=0;

    while count<n do
        if direction = "Reverse" then
            word:=cat(word[2..-1],word[1]):
        else
            word:=cat(word[-1],word[1..-2]):
        end if;
        SP("Text",value,word,refresh);
        Threads:-Sleep(0.1);
                   count:=count+1:
    end do:
end proc:

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
mystring = "Hello World! ";
Scroll[str_, dir_] := StringJoin @@ RotateLeft[str // Characters, dir];
GiveString[dir_] := (mystring = Scroll[mystring, dir]);
CreateDialog[{
   DynamicModule[{direction = -1},
    EventHandler[
     Dynamic[TextCell[
       Refresh[GiveString[direction], UpdateInterval -> 1/8]],
      TrackedSymbols -> {}], {"MouseClicked" :> (direction *= -1)}]]
   }];
```



## MAXScript

Creates a window with a rotating label and 2 buttons to change the direction.


```MAXScript

try destroydialog animGUI catch ()

global userDirection = "right"

fn reverseStr str: direction: =
(
	local lastChar = str[str.count]
	local firstChar = str[1]
	local newStr = ""
	local dir

	if direction == unsupplied then dir = "left" else dir = direction

	case dir of
	(
		"right": (newstr = lastChar + (substring str 1 (str.count-1)))
		"left": (newstr = (substring str 2 (str.count))+firstChar)
	)

	return newstr
)

rollout animGUI "Hello World" width:200
(
	button switchToLeft "<--" pos:[40,0] height:15
	label HelloWorldLabel "Hello World! " pos:[80,0]
	button switchToRight "-->" pos:[150,0] height:15

	timer activeTimer interval:70 active:true

	on activeTimer tick do
	(
		HelloWorldLabel.text = reverseStr str:(HelloWorldLabel.text) direction:userDirection
	)

	on switchToLeft pressed do
	(
		userDirection = "left"
	)

	on switchToRight pressed do
	(
		userDirection = "right"
	)
)

createdialog animGUI

```



## Oz


```oz
functor
import
   Application
   QTk at 'x-oz://system/wp/QTk.ozf'
define
   proc {Start}
      Label
      Window = {CreateWindow ?Label}
      Animation = {New LabelAnimation init(Label delay:125)}
   in
      {Window show}
      {Animation go}
   end

   fun {CreateWindow ?Label}
      Courier = {QTk.newFont font(family:courier size:14)}
      GUI = td(
               title:"Basic Animation"
               label(text:"Hello World! " handle:Label font:Courier)
               action:proc {$} {Application.exit 0} end
               )
   in
      {QTk.build GUI}
   end

   class LabelAnimation from Time.repeat
      attr
         activeShifter:ShiftRight
         otherShifter:ShiftLeft
      feat
         label

      meth init(Label delay:Delay<=100)
         self.label = Label
         {self setRepAll(action:Animate delay:Delay)}
         {Label bind(event:"<Button-1>" action:self#Revert)}
      end

      meth Animate
         OldText = {self.label get(text:$)}
         NewText = {@activeShifter OldText}
      in
         {self.label set(text:NewText)}
      end

      meth Revert
         otherShifter := (activeShifter := @otherShifter)
      end
   end

   fun {ShiftRight Xs}
      {List.last Xs}|{List.take Xs {Length Xs}-1}
   end

   fun {ShiftLeft Xs}
      {Append Xs.2 [Xs.1]}
   end

   {Start}
end
```


## Pascal

===LCL (Lazarus)===

```Pascal
program HelloWorldAnimatedGUI;

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls;

type
  { TFrmHelloWorldAnim }
  TFrmHelloWorldAnim = class(TForm)
    constructor CreateNew(AOwner: TComponent; Num: integer = 0); override;
    procedure lblTextAnimateClick(Sender: TObject);
    procedure tmrAnimateTimer(Sender: TObject);
  private
    { private declarations }
    lblTextAnimate: TLabel;
    tmrAnimate: TTimer;
    FDirection: boolean;
  public
    { public declarations }
  end;

var
  FrmHelloWorldAnim: TFrmHelloWorldAnim;

  { TFrmHelloWorldAnim }

  constructor TFrmHelloWorldAnim.CreateNew(AOwner: TComponent; Num: integer);
  begin
    inherited CreateNew(AOwner, Num);
    Height := 50;
    lblTextAnimate := TLabel.Create(self);
    with lblTextAnimate do
    begin
      Caption := 'Hello World! ';
      Align := alClient;
      Alignment := taCenter;
      font.Name := 'Courier New';
      font.size := 20;
      OnClick := @lblTextAnimateClick;
      Parent := self;
    end;
    tmrAnimate := TTimer.Create(self);
    with tmrAnimate do
    begin
      Interval := 100;
      OnTimer := @tmrAnimateTimer;
    end;
  end;

  procedure TFrmHelloWorldAnim.lblTextAnimateClick(Sender: TObject);
  begin
    FDirection := not FDirection;
  end;

  procedure TFrmHelloWorldAnim.tmrAnimateTimer(Sender: TObject);
  begin
    if FDirection then
      lblTextAnimate.Caption :=
        copy(lblTextAnimate.Caption, length(lblTextAnimate.Caption), 1) +
        copy(lblTextAnimate.Caption, 1, length(lblTextAnimate.Caption) - 1)
    else
      lblTextAnimate.Caption :=
        copy(lblTextAnimate.Caption, 2, length(lblTextAnimate.Caption) - 1) +
        copy(lblTextAnimate.Caption, 1, 1);
  end;

begin
  RequireDerivedFormResource := False;
  Application.Initialize;
  Application.CreateForm(TFrmHelloWorldAnim, FrmHelloWorldAnim);
  Application.Run;
end.
```

Example can be compiled by FreePascal/Lazarus.
Did all in one file, normally you have the project-file, the form-unit- and the form-resource-file.


## Perl


```perl
use Tk;
use Time::HiRes qw(sleep);

my $msg    = 'Hello World! ';
my $first  = '.+';
my $second = '.';

my $mw = Tk::MainWindow->new(-title => 'Animated side-scroller',-bg=>"white");
$mw->geometry ("400x150+0+0");

$mw->optionAdd('*Label.font', 'Courier 24 bold' );

my $scroller = $mw->Label(-text => "$msg")->grid(-row=>0,-column=>0);
$mw->bind('all'=> '<Key-Escape>' => sub {exit;});
$mw->bind("<Button>" => sub { ($second,$first) = ($first,$second) });

$scroller->after(1, \&display );
MainLoop;

sub display {
    while () {
        sleep 0.25;
        $msg =~ s/($first)($second)/$2$1/;
        $scroller->configure(-text=>"$msg");
        $mw->update();
    }
}
```



## Perl 6

{{works with|Rakudo|2019.03}}


```perl6
use v6;

use GTK::Simple;
use GTK::Simple::App;

my $app    = GTK::Simple::App.new(:title<Animation>);
my $button = GTK::Simple::Button.new(label => 'Hello World! ');
my $vbox   = GTK::Simple::VBox.new($button);

my $repeat = $app.g-timeout(100); # milliseconds

my $dir = 1;
$button.clicked.tap({ $dir *= -1 });

$repeat.tap( &{$button.label = $button.label.comb.List.rotate($dir).join} );

$app.set-content($vbox);

$app.run;
```



## Phix

{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\Animation.exw
--
include pGUI.e

string hw = "Hello World! "
bool direction = true
Ihandle label

function timer_cb(Ihandle /*ih*/)
    hw = iff(direction?hw[$]&hw[1..-2]:hw[2..$]&hw[1])
    IupSetAttribute(label,"TITLE",hw)
    return IUP_IGNORE
end function

function key_cb(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c=K_UP then direction = not direction end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()
    label = IupLabel(hw,"FONT=\"Verdana, 18\"")
    Ihandle dlg = IupDialog(label,"TITLE=Animation, DIALOGFRAME=YES, CHILDOFFSET=70x40, SIZE=200x80")
    IupSetCallback(dlg, "K_ANY", Icallback("key_cb"))
    IupShow(dlg)
    Ihandle hTimer = IupTimer(Icallback("timer_cb"), 160)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PicoLisp


### Plain text

A plain text version. The following script works in an XTerm window.

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(prin "^[[?9h")  # Mouse reporting on

(setq Dir 1  Text (chop "Hello World! "))

(loop
   (prin (do Dir (rot Text)))
   (when (= "^[" (key 200))
      (key) (key)
      (when (= " " (key))  # Left button
         (setq Dir (if (= 1 Dir) 12 1)) )
      (key) (key) )
   (do (length Text) (prin "^H")) )
```


### HTML/JavaScript

The standard PicoLisp GUI is HTTP based. Connect your browser to
http://localhost:8080 after starting the following script.

The scrolling text is displayed in a button. Clicking on the button
reverses the scroll direction.

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(load "@ext.l" "@lib/http.l" "@lib/xhtml.l" "@lib/form.l")

(one *Dir)

(de start ()
   (app)
   (action
      (html 0 "Animation" "@lib.css" NIL
         (form NIL
            (gui '(+Button)
               '(pack (do *Dir (rot '`(chop "Hello World! "))))
               '(setq *Dir (if (= 1 *Dir) 12 1)) )
            (gui '(+Click +Auto +Button) 400 'This 1000 "Start") ) ) ) )

(server 8080 "!start")
(wait)
```



### Java/Swing

This solution works on ErsatzLisp, the Java version of PicoLisp.

```PicoLisp
#!ersatz/pil

(setq
   Dir 1
   Text (chop "Hello World! ")
   Frame (java "javax.swing.JFrame" T "Animation")
   Label (java "javax.swing.JLabel" T (pack Text)) )

(java Label 'addMouseListener
   (interface "java.awt.event.MouseListener"
      'mouseClicked '((Ev) (setq Dir (if (= 1 Dir) 12 1)))
      'mouseEntered nil
      'mouseExited nil
      'mousePressed nil
      'mouseReleased nil ) )

(java Frame 'add Label)
(java Frame 'pack)
(java Frame 'setVisible T)
(loop
   (wait 200)
   (java Label 'setText (pack (do Dir (rot Text)))) )
```




## Processing


```processing
String txt = "Hello, world! ";
boolean dir = true;

void draw(){
  background(128);
  text(txt, 10, height/2);
  if(frameCount%10==0){
    if(dir) {
      txt = rotate(txt, 1);
    } else {
      txt = rotate(txt, txt.length()-1);
    }
  }
}

void mouseReleased(){
  dir = !dir;
}

String rotate(String text, int startIdx) {
  char[] rotated = new char[text.length()];
  for (int i = 0; i < text.length(); i++) {
    rotated[i] = text.charAt((i + startIdx) % text.length());
  }
  return String.valueOf(rotated);
}
```




## Prolog

SWI-Prolog has a grapghic interface XPCE.

```Prolog
:- use_module(library(pce)).

animation :-
    new(D, window('Animation')),
    new(Label, label(hello, 'Hello world ! ')),
    send(D, display, Label, point(1,10)),
    new(@animation, animation(Label)),
    send(D, recogniser,
         new(_G, my_click_gesture(left, ''))),

    send(D, done_message, and(message(@animation, free),
                  message(@receiver, destroy))),
    send(D, open),
    send(@animation?mytimer, start).


:- pce_begin_class(animation(label), object).
variable(label, object,  both, "Display window").
variable(delta,    object, both,  "increment of the angle").
variable(mytimer, timer, both, "timer of the animation").

initialise(P, W:object) :->
        "Creation of the object"::
        send(P, label, W),
        send(P, delta, to_left),
    send(P, mytimer, new(_, timer(0.5,message(P, anim_message)))).

% method called when the object is destroyed
% first the timer is stopped
% then all the resources are freed
unlink(P) :->
    send(P?mytimer, stop),
    send(P, send_super, unlink).


% message processed by the timer
anim_message(P) :->
    get(P, label, L),
    get(L, selection, S),
    get(P, delta, Delta),
    compute(Delta, S, S1),
    new(A, name(S1)),
    send(L, selection, A).


:- pce_end_class.

:- pce_begin_class(my_click_gesture, click_gesture,
           "Click in a window").

class_variable(button, button_name, left,
           "By default click with left button").

terminate(G, Ev:event) :->
    send(G, send_super, terminate, Ev),
    get(@animation, delta, D),
    (   D = to_left -> D1 = to_right; D1 = to_left),
    send(@animation, delta, D1).

:- pce_end_class.


% compute next text to be dispalyed
compute(to_right, S, S1) :-
    get(S, size, Len),
    Len1 is Len - 1,
    get(S, sub, Len1, Str),
    get(S, delete_suffix, Str, V),
    get(Str, append, V, S1).

compute(to_left, S, S1) :-
    get(S, sub, 0, 1, Str),
    get(S, delete_prefix, Str, V),
    get(V, append, Str, S1).

```


## PureBasic


```PureBasic
OpenWindow(0,0,0,500,100,"Hello World!",#PB_Window_ScreenCentered|#PB_Window_SystemMenu)

text$ = "Hello World! "
direction = 1

LoadFont(0,"",60)
ButtonGadget(0,2,2,496,96,text$) : SetGadgetFont(0,FontID(0))

Repeat
  event = WaitWindowEvent(50)
  Select event
    Case #PB_Event_Gadget
      If EventGadget() = 0
        direction*-1
      EndIf
    Case #PB_Event_CloseWindow
      End
  EndSelect

  If ElapsedMilliseconds()-tick > 400
    offset+direction
    If offset > Len(text$)-1
      offset = 0
    ElseIf offset < 0
      offset = Len(text$)-1
    EndIf
    SetGadgetText(0,Mid(text$,offset+1)+Left(text$,offset))
    tick = ElapsedMilliseconds()
  EndIf
ForEver
```



## Python


### Using PyQt

{{libheader|PyQt5}}

```python
#!/usr/bin/env python3
import sys

from PyQt5.QtCore import QBasicTimer, Qt
from PyQt5.QtGui import QFont
from PyQt5.QtWidgets import QApplication, QLabel


class Marquee(QLabel):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.right_to_left_direction = True
        self.initUI()
        self.timer = QBasicTimer()
        self.timer.start(80, self)

    def initUI(self):
        self.setWindowFlags(Qt.FramelessWindowHint)
        self.setAttribute(Qt.WA_TranslucentBackground)
        self.setText("Hello World! ")
        self.setFont(QFont(None, 50, QFont.Bold))
        # make more irritating for the authenticity with <marquee> element
        self.setStyleSheet("QLabel {color: cyan; }")

    def timerEvent(self, event):
        i = 1 if self.right_to_left_direction else -1
        self.setText(self.text()[i:] + self.text()[:i])  # rotate

    def mouseReleaseEvent(self, event):  # change direction on mouse release
        self.right_to_left_direction = not self.right_to_left_direction

    def keyPressEvent(self, event):  # exit on Esc
        if event.key() == Qt.Key_Escape:
            self.close()


app = QApplication(sys.argv)
w = Marquee()
# center widget on the screen
w.adjustSize()  # update w.rect() now
w.move(QApplication.instance().desktop().screen().rect().center()
       - w.rect().center())
w.show()
sys.exit(app.exec())
```


### Using pygame

{{libheader|pygame}}

```python
import pygame, sys
from pygame.locals import *
pygame.init()

YSIZE = 40
XSIZE = 150

TEXT = "Hello World! "
FONTSIZE = 32

LEFT = False
RIGHT = True

DIR = RIGHT

TIMETICK = 180
TICK = USEREVENT + 2

TEXTBOX = pygame.Rect(10,10,XSIZE,YSIZE)

pygame.time.set_timer(TICK, TIMETICK)

window = pygame.display.set_mode((XSIZE, YSIZE))
pygame.display.set_caption("Animation")

font = pygame.font.SysFont(None, FONTSIZE)
screen = pygame.display.get_surface()

def rotate():
    index = DIR and -1 or 1
    global TEXT
    TEXT = TEXT[index:]+TEXT[:index]

def click(position):
    if TEXTBOX.collidepoint(position):
        global DIR
        DIR = not DIR

def draw():
    surface = font.render(TEXT, True, (255,255,255), (0,0,0))
    global TEXTBOX
    TEXTBOX = screen.blit(surface, TEXTBOX)

def input(event):
    if event.type == QUIT:
        sys.exit(0)
    elif event.type == MOUSEBUTTONDOWN:
        click(event.pos)
    elif event.type == TICK:
        draw()
        rotate()

while True:
    input(pygame.event.wait())
    pygame.display.flip()
```


### Using Tkinter


```python
import Tkinter as tki

def scroll_text(s, how_many):
    return s[how_many:] + s[:how_many]

direction = 1
tk = tki.Tk()
var = tki.Variable(tk)

def mouse_handler(point):
    global direction
    direction *= -1

def timer_handler():
    var.set(scroll_text(var.get(),direction))
    tk.after(125, timer_handler)

var.set('Hello, World! ')
tki.Label(tk, textvariable=var).pack()
tk.bind("<Button-1>", mouse_handler)
tk.after(125, timer_handler)
tk.title('Python Animation')
tki.mainloop()

```



## Quick BASIC

{{works with|QB 4.5}}
{{works with|QB 7.1}}

Before, load the QB.EXE with /L QB.QLB key, or QBX.EXE with /L QBX.QLB key, or just /L key any of them. That's the easiest way to get the mouse in DOS.
Pressing to change direction should be on the text. To exit push the right button anywhere.

{{libheader|QB.QLB / QBX.QLB}}


```qbasic

'here accordingly to the version, QB or QBX
REM $INCLUDE: 'QBX.BI'

DIM REGS AS REGTYPE
DIM C AS STRING, SIZ AS INTEGER
DIM I AS DOUBLE, DIRE AS INTEGER
C = "Hello World! "
SIZ = LEN(C)

SCREEN 12
'turn the cursor visible
REGS.AX = 1
INTERRUPT 51, REGS, REGS

DO
 I = TIMER
 LOCATE 1, 1
 PRINT C

 REGS.AX = 5            'read mouse's queue of occurred pressings
 REGS.BX = 0            'the left button
 INTERRUPT 51, REGS, REGS

 'BX is the selected button's quantity of occurred pressings
 IF REGS.BX <> 0 THEN
  IF REGS.CX >= 0 AND REGS.CX < SIZ * 8 AND REGS.DX >= 0 AND REGS.DX < 16 THEN
   DIRE = 1 - DIRE
  END IF
 END IF

 'AX is all buttons' status
 IF (REGS.AX AND 2) <> 0 THEN EXIT DO

 IF DIRE = 0 THEN
  C = RIGHT$(C, 1) + LEFT$(C, SIZ - 1)
 ELSE
  C = RIGHT$(C, SIZ - 1) + LEFT$(C, 1)
 END IF

 DO WHILE TIMER < I + .08
  IF TIMER < I THEN I = I - 86400       'midnight checking
 LOOP
LOOP
```



## R

{{libheader|gWidgets}}

The basic principle is:create a window with a label in it, then add a handler to the label for rotating the string, and another for changing direction on a click.  Use of the tag function allows you to store the text flow direction as an attribute of the label.

```r

rotate_string <- function(x, forwards)
{
   nchx <- nchar(x)
   if(forwards)
   {                                  
      paste(substr(x, nchx, nchx), substr(x, 1, nchx - 1), sep = "")
   } else
   {                                                          
      paste(substr(x, 2, nchx), substr(x, 1, 1), sep = "")
   }
}

handle_rotate_label <- function(obj, interval = 100)
{
  addHandlerIdle(obj,
    handler = function(h, ...)
    {
       svalue(obj) <- rotate_string(svalue(obj), tag(obj, "forwards"))
    },
    interval = interval
  )
}

handle_change_direction_on_click <- function(obj)
{
  addHandlerClicked(obj,
    handler = function(h, ...)
    {
      tag(h$obj, "forwards") <- !tag(h$obj, "forwards")
    }
  )
}

library(gWidgets)
library(gWidgetstcltk) #or library(gWidgetsRGtk2) or library(gWidgetsrJava)             
lab <- glabel("Hello World! ", container = gwindow())  
tag(lab, "forwards") <- TRUE
handle_rotate_label(lab)
handle_change_direction_on_click(lab)

```



## Racket



```racket

#lang racket/gui

;; One of 'left or 'right
(define direction 'left)

;; Set up the GUI
(define animation-frame%
  (class frame%
    (super-new [label "Animation"])
    ;; reverse direction on a click
    (define/override (on-subwindow-event win evt)
      (when (send evt button-down?)
        (set! direction
              (if (eq? direction 'left)
                  'right
                  'left))))))

(define frame (new animation-frame%))
(define msg (new message%
                 [label "Hello World! "]
                 [parent frame]))

;; Timer callback to adjust the message
(define (callback)
  (define old-label (send msg get-label))
  (define len (string-length old-label))
  (if (eq? direction 'left)
      (send msg set-label
            (string-append (substring old-label 1)
                           (substring old-label 0 1)))
      (send msg set-label
            (string-append (substring old-label (- len 1) len)
                           (substring old-label 0 (- len 1))))))

;; Set a timer and go
(define timer (new timer%
                   [notify-callback callback]
                   [interval 500]))

(send frame show #t)

```



## REBOL


```REBOL
REBOL [
	Title: "Basic Animation"
	URL: http://rosettacode.org/wiki/Basic_Animation
]

message: "Hello World! "  how: 1

roll: func [
	"Shifts a text string right or left by one character."
	text [string!] "Text to shift."
	direction [integer!] "Direction to shift -- right: 1, left: -1."
	/local h t
][
	either direction > 0 [
		h: last text  t: copy/part text ((length? text) - 1)
	][
		h: copy skip text 1  t: text/1
	]
	rejoin [h t]
]

; This next bit specifies the GUI panel. The window will have a
; gradient backdrop, over which will be composited the text, in a
; monospaced font with a drop-shadow. A timer (the 'rate' bit) is set
; to update 24 times per second. The 'engage' function in the 'feel'
; block listens for events on the text face. Time events update the
; animation and mouse-down change the animation's direction.

view layout [
	backdrop effect [gradient 0x1 coal black]

	vh1 as-is message ; 'as-is' prevents text trimming.
	font [name: font-fixed]
	rate 24
	feel [
		engage: func [f a e] [
			case [
				'time = a [set-face f message: roll message how] ; Animate.
				'down = a [how: how * -1] ; Change direction.
			]
		]
	]
]
```



## REXX

This REXX version <u>only</u> works with REXX/Personal or REXX/PC.

```rexx
/*REXX prg displays a text string (in one direction), and reverses when a key is pressed*/
parse upper version !ver !vernum .;     !pcRexx= 'REXX/PERSONAL'==!ver  |  'REXX/PC'==!ver
if \!pcRexx  then do
                  say
                  say '***error***  This REXX program requires REXX/PERSONAL or REXX/PC.'
                  say
                  exit 1
                  end
parse arg $                                      /*obtain optional text message from CL.*/
if $=''  then $= 'Hello World!'                  /*Not specified?  Then use the default.*/
if right($, 1)\==' '  then $= $' '               /*ensure msg text has a trailing blank.*/
signal on halt                                   /*handle a HALT if user quits this way.*/
way = 0                                          /*default direction for marquee display*/
                  y =
        do  until y=='Q';  'CLS'                 /*if user presses  Q  or  q, then quit.*/
        call lineout ,$;   call delay .2         /*display output; delay 1/5 of a second*/
        y= inKey('Nowait');  upper y             /*maybe get a pressed key; uppercase it*/
        if y\==''  then way= \way                /*change the direction of the marquee. */
        if way  then $= substr($, 2)left($, 1)   /*display marquee in a direction or ···*/
                else $=  right($, 1)substr($, 1, length($) - 1)        /* ··· the other·*/
        end   /*until*/
halt:                                            /*stick a fork in it,  we're all done. */
```





## Ring


```ring

# Project : Animation

Load "guilib.ring"
load "stdlib.ring"
rotate = false

MyApp = New qApp {
              win1 = new qWidget() {
                         setwindowtitle("Hello World")
                         setGeometry(100,100,370,250)

                         lineedit1 = new qlineedit(win1) {
                                         setGeometry(10,100,350,30)
                                         lineedit1.settext(" Hello World! ")
                         myfilter = new qallevents(lineedit1)
                         myfilter.setMouseButtonPressevent("rotatetext()")
                         installeventfilter(myfilter)}
              show()}
exec()}

func rotatetext()
        rotate = not rotate
        strold = " Hello World! "
        for n = 1 to 15
                 if rotate = true
                    see "str = " + '"' + strold + '"' + nl
                    strnew = right(strold, 1) + left(strold, len(strold) - 1)
                    lineedit1.settext(strnew)
                    strold = strnew
                    sleep(1)
                 ok
                 if rotate = false
                    see "str = " + '"' + strold + '"' + nl
                    strnew = right(strold, len(strold) - 1) + left(strold, 1)
                    lineedit1.settext(strnew)
                    strold = strnew
                    sleep(1)
                 ok
        next
        see nl

```

Output:

```txt

str = " Hello World! "
str = "  Hello World!"
str = "!  Hello World"
str = "d!  Hello Worl"
str = "ld!  Hello Wor"
str = "rld!  Hello Wo"
str = "orld!  Hello W"
str = "World!  Hello "
str = " World!  Hello"
str = "o World!  Hell"
str = "lo World!  Hel"
str = "llo World!  He"
str = "ello World!  H"
str = "Hello World!  "
str = " Hello World! "

str = " Hello World! "
str = "Hello World!  "
str = "ello World!  H"
str = "llo World!  He"
str = "lo World!  Hel"
str = "o World!  Hell"
str = " World!  Hello"
str = "World!  Hello "
str = "orld!  Hello W"
str = "rld!  Hello Wo"
str = "ld!  Hello Wor"
str = "d!  Hello Worl"
str = "!  Hello World"
str = "  Hello World!"
str = " Hello World! "

```



## Ruby

{{trans|Tcl}}
{{libheader|Ruby/Tk}}

```ruby
require 'tk'
$str = TkVariable.new("Hello World! ")
$dir = :right

def animate
  $str.value = shift_char($str.value, $dir)
  $root.after(125) {animate}
end

def shift_char(str, dir)
  case dir
  when :right then str[-1,1] + str[0..-2]
  when :left  then str[1..-1] + str[0,1]
  end
end

$root = TkRoot.new("title" => "Basic Animation")

TkLabel.new($root) do
  textvariable $str
  font "Courier 14"
  pack {side 'top'}
  bind("ButtonPress-1") {$dir = {:right=>:left,:left=>:right}[$dir]}
end

animate
Tk.mainloop
```


{{libheader|Shoes}}

```ruby
Shoes.app do
  @direction = 1
  @label = para "Hello World! ", :family => 'monospace'

  click {|button, left, top| @direction *= -1 if button == 1}

  animate(8) do |f|
    t = @label.text
    @label.text = @direction > 0 ? t[-1] + t[0..-2] : t[1..-1] + t[0]
  end
end
```



## Rust

==={{libheader|GTK}}===

```rust
#[cfg(feature = "gtk")]
mod graphical {
    extern crate gtk;

    use self::gtk::traits::*;
    use self::gtk::{Inhibit, Window, WindowType};
    use std::ops::Not;
    use std::sync::{Arc, RwLock};

    pub fn create_window() {
        gtk::init().expect("Failed to initialize GTK");

        let window = Window::new(WindowType::Toplevel);
        window.connect_delete_event(|_, _| {
            gtk::main_quit();
            Inhibit(false)
        });
        let button = gtk::Button::new_with_label("Hello World! ");
        window.add(&button);

        let lock = Arc::new(RwLock::new(false));

        let lock_button = lock.clone();
        button.connect_clicked(move |_| {
            let mut reverse = lock_button.write().unwrap();
            *reverse = reverse.not();
        });

        let lock_thread = lock.clone();
        gtk::timeout_add(100, move || {
            let reverse = lock_thread.read().unwrap();
            let mut text = button.get_label().unwrap();
            let len = &text.len();

            if *reverse {
                let begin = &text.split_off(1);
                text.insert_str(0, begin);
            } else {
                let end = &text.split_off(len - 1);
                text.insert_str(0, end);
            }

            button.set_label(&text);

            gtk::Continue(true)
        });

        window.show_all();
        gtk::main();
    }
}


#[cfg(feature = "gtk")]
fn main() {
    graphical::create_window();
}

#[cfg(not(feature = "gtk"))]
fn main() {}
```



## Scala

{{works with|Scala|2.8}}

```scala
import scala.actors.Actor.{actor, loop, reactWithin, exit}
import scala.actors.TIMEOUT
import scala.swing.{SimpleSwingApplication, MainFrame, Label}
import scala.swing.event.MouseClicked

case object Revert

object BasicAnimation extends SimpleSwingApplication {
  val label = new Label("Hello World! ")
  val rotator = actor {
    var goingRight = true
    loop {
      reactWithin(250 /*ms*/) {
        case Revert => goingRight = !goingRight
        case TIMEOUT =>
          if (goingRight)
            label.text = label.text.last + label.text.init
          else
            label.text = label.text.tail + label.text.head
        case unknown => println("Unknown message "+unknown); exit()
      }
    }
  }
  def top = new MainFrame {
    title = "Basic Animation"
    contents = label
  }
  listenTo(label.mouse.clicks) // use "Mouse" instead of "mouse" on Scala 2.7
  reactions += {
    case _ : MouseClicked => rotator ! Revert
  }
}
```



## Scratch

Scratch is event and animation oriented, so this is a natural task for this language.  This solution is hosted at the [https://scratch.mit.edu/projects/64784694/ scratch website], where one can see it in action and read its code.

'''Solution Summary and Comments'''

The solution consists of two blocks of code.  The main block initializes variables upon invocation and sets up a loop to display the crawling "Hello World! " message.  The crawl is accomplished by manipulation of the list containing the message as individual characters.  The other block of code is attached to the scratch sprite, and all it does is change the direction of the text crawl when the sprite is clicked.

I tried a couple of techniques for introducing a delay into the text crawl loop.  The command "Say message for 2 seconds" resulted in poor performance; the application's response to clicks was spotty.  Using the timer and waiting for it to exceed 2 seconds before resetting it and advancing the crawl worked much better.  (I also tried waiting for the timer to equal 2 seconds, and unsurprisingly this resulted in an application freeze-up.)



## smart BASIC


```smart BASIC
'Animation, by rbytes and Dutchman
word$="Hello World! "
'use button window with text
SET BUTTONS CUSTOM
SET BUTTONS FONT SIZE 40
DRAW COLOR 0,0,0
DO 'the button is redrawn each loop
BUTTON "anim" TEXT word$ AT 130,100
PAUSE .1
'touching the button reverses the scrolling
IF BUTTON_PRESSED("anim") THEN flag=1-flag
IF flag THEN 'shift right
  word$=RIGHT$(word$,1)&LEFT$(word$,LEN(word$)-1)
ELSE 'shift left
  word$=RIGHT$(word$,LEN(word$)-1)&LEFT$(word$,1)
ENDIF
UNTIL 0</Lang>

This program can be concatenated onto a single line using ! as the concatenator:

<Lang smart BASIC>'by rbytes and Dutchman!word$="Hello World! "!'use button window with text!
SET BUTTONS CUSTOM!SET BUTTONS FONT SIZE 40!DRAW COLOR 0,0,0!DO!'the button is redrawn each loop!BUTTON "anim" TEXT word$ AT 130,100!PAUSE .1!'touching the button reverses the scrolling!IF BUTTON_PRESSED("anim") THEN flag=1-flag!IF flag THEN!'shift right!word$=RIGHT$(word$,1)&LEFT$(word$,LEN(word$)-1)!ELSE!'shift left!word$=RIGHT$(word$,LEN(word$)-1)&LEFT$(word$,1)!ENDIF!UNTIL 0</Lang>

Mr. Kibernetik, the creator of smart Basic, offered this ultra-compact one-line version:

<Lang smart BASIC>w$="Hello World! "!1 BUTTON 0 TEXT w$ AT 0,0!PAUSE .1!IF BUTTON_PRESSED("0") THEN f=1-f!IF f THEN w$=RIGHT$(w$,1)&LEFT$(w$,LEN(w$)-1) ELSE w$=RIGHT$(w$,LEN(w$)-1)&LEFT$(w$,1)!GOTO 1</Lang>

and smart Basic Forum member sarossell found a way to shorten even that! See if you can spot what is changed.

<Lang smart BASIC>w$="Hello World! "!1 BUTTON 0 TEXT w$ AT 0,0!PAUSE .1!IF BUTTON_PRESSED("0") THEN f=1-f!k=LEN(w$)-1!IF f THEN w$=RIGHT$(w$,1)&LEFT$(w$,k) ELSE w$=RIGHT$(w$,k)&LEFT$(w$,1)!GOTO 1</Lang>


## Suneido


```Suneido
Window(Controller
    {
    Xmin: 50
    Ymin: 50
    New()
        {
        super(.layout())
        .txt = .FindControl('text')
        .moveTimer = SetTimer(NULL, 0, 600, .moveTimerFunc)
        }
    direction: -1
    moveTimer: false
    layout()
        {
        return #(Vert (Static 'Hello World! ', size: 12, weight: 600,
            notify:, name: 'text'))
        }
    moveTimerFunc(@unused)
        {
        str = .txt.Get()
        .txt.Set(str.Substr(1 * .direction) $ str.Substr(0, (1 * .direction)))
        }
    Static_Click()
        {
        .direction = .direction * -1
        }
    Destroy()
        {
        if .moveTimer isnt false
            {
            KillTimer(NULL, .moveTimer)
            ClearCallback(.moveTimerFunc)
            }
        super.Destroy()
        }
    })
```


=={{header|SVG}} (no scripts)==

{{works with|Batik|1.7}}

This animation is defined as a smooth movement rather than by moving whole characters, because that is more natural in SVG (without scripting); by characters would require 13 different text elements displayed in sequence.


```xml
<svg xmlns="http://www.w3.org/2000/svg" width="100" height="30">
    <g id="all">
        <rect width="100%" height="100%" fill="yellow"/>
        <g style="font: 18 'Times New Roman', serif;
                  fill: black;
                  stroke: white; stroke-width: 0.001; /* workaround for Batik oddity */ ">
            <text x="0" y="20" textLength="95">Hello World!</text>
            <text x="-100" y="20" textLength="95">Hello World!</text>
            <animateMotion restart="whenNotActive" repeatCount="indefinite" dur="2s"
                           begin="0s;all.click" end="all.click"
                           from="0,0"   by="100,0"/>
            <animateMotion restart="whenNotActive" repeatCount="indefinite" dur="2s"
                           begin="all.click" end="all.click"
                           from="100,0" by="-100,0"/>
        </g>
    </g>
</svg>
```


(Does not work in Safari 4.0.2 because it apparently does not implement toggled animations correctly ([http://www.w3.org/TR/2001/REC-smil-animation-20010904/#RestartAttribute see spec]). Dreadful workaround: set the two animations to <code>id="a" begin="0s;all.click" end="all.mousedown"</code> and <code>begin="a.end" end="all.click"</code>, respectively.)


## Tcl

{{libheader|Tk}}

```tcl
package require Tk
set s "Hello World! "
set dir 0
# Periodic animation callback
proc animate {} {
    global dir s
    if {$dir} {
        set s [string range $s 1 end][string index $s 0]
    } else {
        set s [string index $s end][string range $s 0 end-1]
    }
    # We will run this code ~8 times a second (== 125ms delay)
    after 125 animate
}
# Make the label (constant width font looks better)
pack [label .l -textvariable s -font {Courier 14}]
# Make a mouse click reverse the direction
bind .l <Button-1> {set dir [expr {!$dir}]}
# Start the animation
animate
```


=={{header|TI-89 BASIC}}==

The requirements contain "When the user clicks on the text…". The TI-89 does not have a graphical cursor, so just for the sake of overdoing it, and to have a little more complex animation (overlapping objects), this program implements one. Use the arrow keys and ENTER to control the cursor.

<pre style="font-family:'TI Uni'">rcanimat()
Prgm
  Local leftward,s,i,k,x,y

  false → leftward
  "Hello World! " → s
  0 → k      © last keypress found
  6*3 → x    © cursor position
  5 → y

  ClrIO
  While k ≠ 4360 and k ≠ 277 and k ≠ 264  © QUIT,HOME,ESC keys

    © Handle Enter key
    If k = 13 Then
      If x ≥ 40 and x < 40+6*dim(s) and y ≥ 25 and y < 35 Then © On text?
        not leftward → leftward
      ElseIf x ≥ 5 and x < 5+6*dim("[Quit]") and y ≥ 55 and y < 65 Then © On quit?
        Exit
      EndIf
    EndIf

    © Cursor movement keys
    If k=338 or k=340 or k=344 or k=337 or k=339 or k=342 or k=345 or k=348 Then
      Output y, x, " " © Blank old cursor pos
      If     k = 338 or k = 339 or k = 342 Then: y-6→y
      ElseIf k = 344 or k = 345 or k = 348 Then: y+6→y :EndIf
      If     k = 337 or k = 339 or k = 345 Then: x-6→x
      ElseIf k = 340 or k = 342 or k = 348 Then: x+6→x :EndIf
      min(max(y, 0), 64)→y
      min(max(x, 0), 152)→x
    EndIf

    © Drawing
    Output 0, 0, "Use arrows, ENTER key"
    Output 60, 5, "[Quit]"
    Output 30, 40, s
    Output y, x, "Ŵ"              © should be diamond symbol

    © Animation
    If leftward Then
      right(s, dim(s)-1) & left(s, 1) → s
    Else
      right(s, 1) & left(s, dim(s)-1) → s
    EndIf

    0 → i
    getKey() → k                  © reads most recent keypress or 0
    While i < 2 and k = 0         © Delay loop. Better solution?
      getKey() → k
      i + 1 → i
    EndWhile

  EndWhile
  DispHome
EndPrgm
```



## Vedit macro language

It is not possible to detect mouse clicks while a macro is running. Therefore, the direction is controlled with Caps Lock key.

```vedit
Buf_Switch(Buf_Free)
Win_Create(Buf_Num, 1, 1, 2, 14)
Ins_Text("Hello World! ")
#2 = Cur_Pos
Repeat(ALL) {
    if (Key_Shift_Status & 64) {
        BOL
        Block_Copy(#2-1, #2, DELETE)
    } else {
        Block_Copy(0, 1, DELETE)
    }
    EOL
    Update()
    Sleep(2)
}
```



## Visual Basic


This example shows code that is hidden by the IDE. (Form creation is done graphically within the IDE, not at runtime.)


```vb
VERSION 5.00
Begin VB.Form Form1
   Begin VB.Timer Timer1
      Interval = 250
   End
   Begin VB.Label Label1
      AutoSize = -1  'True
      Caption  = "Hello World! "
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'Everything above this line is hidden when in the IDE.

Private goRight As Boolean

Private Sub Label1_Click()
    goRight = Not goRight
End Sub

Private Sub Timer1_Timer()
    If goRight Then
        x = Mid(Label1.Caption, 2) & Left(Label1.Caption, 1)
    Else
        x = Right(Label1.Caption, 1) & Left(Label1.Caption, Len(Label1.Caption) - 1)
    End If
    Label1.Caption = x
End Sub
```



## XPL0


```XPL0
include c:\cxpl\codes;
int     CpuReg, Dir, I, J;
char    Str;
string  0;                      \use zero-terminated strings, instead of MSb set
[CpuReg:= GetReg;               \provides access to 8086 CPU registers
\      0123456789012
Str:= "Hello World! ";
Clear;
Dir:= -1;                       \make string initially scroll to the right
I:= 0;                          \index to start of displayed portion of string
repeat  Cursor(0, 0);           \set cursor position to upper-left corner
        for J:= 0 to 12 do
                [ChOut(0, Str(I));  I:= I+1;  if I>12 then I:= 0];
        Sound(0, 2, 1);         \delay about 1/9 second
        I:= I+Dir;              \step starting position of displayed string
        if I<0 then I:=12;      \wraparound
        if I>12 then I:= 0;
        CpuReg:= GetReg;        \get mouse button press information
        CpuReg(0):= 5;  CpuReg(1):= 0;
        SoftInt($33);           \reverse direction if left button was pressed
        if CpuReg(1) then Dir:= -Dir;
until   KeyHit;                 \any keystroke terminates program
]
```



## Yabasic


```Yabasic
clear screen
open window 400, 150
backcolor 0, 0, 0
clear window

color 250, 120, 0
texto$ = "Hello world! "
l = len(texto$)
dir = 1
do
    release$ = inkey$(.25)
    if mouseb(release$) = -1 then
        dir = -dir
    end if
    clear window
    text 100, 90, texto$, "modern30"
    if dir = 1 then
        texto$ = right$(texto$, l-1) + left$(texto$, 1)
    else
        texto$ = right$(texto$, 1) + left$(texto$, l-1)
    end if
loop
```



## ZX Spectrum Basic

{{trans|BBC BASIC}}
Replaces the detection of mouse click by pressing a key (Spectrum does not detect this device by default).

```zxbasic
10 LET t$="Hello world! ": LET lt=LEN t$
20 LET direction=1
30 PRINT AT 0,0;t$
40 IF direction THEN LET t$=t$(2 TO )+t$(1): GO TO 60
50 LET t$=t$(lt)+t$( TO lt-1)
60 IF INKEY$<>"" THEN LET direction=NOT direction
70 PAUSE 5: GO TO 30
```


{{omit from|ACL2}}
{{omit from|AWK|Does not have native terminal control or click handler}}
{{omit from|GUISS}}
{{omit from|LFE}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|PARI/GP}}
{{omit from|PHP}}
{{omit from|SQL PL|It does not handle GUI}}

[[Category:Animation]]
