+++
title = "GUI enabling/disabling of controls"
description = ""
date = 2019-08-10T09:25:53Z
aliases = []
[extra]
id = 8059
[taxonomies]
categories = []
tags = []
+++

{{task|GUI}} {{requires|Graphics}}
{{omit from|ACL2}}
{{omit from|Applesoft BASIC|no concept of a GUI}}
{{omit from|AWK|no concept of a GUI}}
{{omit from|Batch File}}
{{omit from|Blast}}
{{omit from|Brainfuck}}
{{omit from|Commodore BASIC}}
{{omit from|GUISS|Can only do what the installed applications can do}}
{{omit from|Integer BASIC|no concept of a GUI}}
{{omit from|Lilypond}}
{{omit from|Logtalk}}
{{omit from|Lotus 123}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|PostScript}}
{{omit from|Retro}}
{{omit from|SQL PL|It does not handle GUI}}

In addition to fundamental [[GUI component interaction]], an application should
dynamically enable and disable GUI components, to give some guidance to the
user, and prohibit (inter)actions which are inappropriate in the current state
of the application.


;Task:
Similar to the task [[GUI component interaction]], write a program
that presents a form with three components to the user:
::#   a numeric input field ("Value")
::#   a button   ("increment")
::#   a button   ("decrement")



The field is initialized to zero.
The user may manually enter a new value into the field,
increment its value with the "increment" button,
or decrement the value with the "decrement" button.

The input field should be enabled only when its value is zero.
The "increment" button only as long as the field's value is less then 10:
When the value 10 is reached, the button should go into a disabled state.
Analogously, the "decrement" button should be enabled only as long as
the value is greater than zero.

Effectively, the user can now either increment up to 10, or down to zero.
Manually entering values outside that range is still legal,
but the buttons should reflect that and enable/disable accordingly.





## Ada

{{libheader|GtkAda}}

disabling.adb:

```Ada
with Ada.Strings.Fixed;
with Gtk.Main;
with Gtk.Handlers;
with Gtk.Button;
with Gtk.Window;
with Gtk.GEntry;
with Gtk.Editable;
with Gtk.Box;
with Gtk.Widget;
with Glib.Values;

procedure Disabling is
   type My_Natural is range 0 .. 10;

   The_Value : My_Natural := 0;

   Main_Window      : Gtk.Window.Gtk_Window;
   Content          : Gtk.Box.Gtk_Vbox;
   Increment_Button : Gtk.Button.Gtk_Button;
   Decrement_Button : Gtk.Button.Gtk_Button;
   Entry_Field      : Gtk.GEntry.Gtk_Entry;

   package Entry_Callbacks is new Gtk.Handlers.Callback
     (Gtk.GEntry.Gtk_Entry_Record);

   package Button_Callbacks is new Gtk.Handlers.Callback
     (Gtk.Button.Gtk_Button_Record);

   package Window_Callbacks is new Gtk.Handlers.Return_Callback
     (Gtk.Window.Gtk_Window_Record, Boolean);

   --  update displayed text
   procedure Update_Entry is
   begin
      Gtk.GEntry.Set_Text
        (The_Entry => Entry_Field,
         Text      =>
           Ada.Strings.Fixed.Trim
             (Source => My_Natural'Image (The_Value),
              Side   => Ada.Strings.Both));
   end Update_Entry;

   procedure Check_Value is
   begin
      Gtk.Widget.Set_Sensitive
        (Gtk.Widget.Gtk_Widget (Decrement_Button),
         The_Value > 0);
      Gtk.Widget.Set_Sensitive
        (Gtk.Widget.Gtk_Widget (Increment_Button),
         The_Value < 10);
      Gtk.Widget.Set_Sensitive
        (Gtk.Widget.Gtk_Widget (Entry_Field),
         The_Value = 0);
   end Check_Value;

   procedure On_Changed_Text
     (Object : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Params : Glib.Values.GValues)
   is
      pragma Unreferenced (Params, Object);
   begin
      The_Value := My_Natural'Value (Gtk.GEntry.Get_Text (Entry_Field));
      Check_Value;
      Update_Entry;
   exception
      when Constraint_Error =>
         The_Value := 0;
   end On_Changed_Text;

   --  make sure that only numbers are entered
   procedure On_Insert_Text
     (Object : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Params : Glib.Values.GValues)
   is
      Length : constant Glib.Gint :=
        Glib.Values.Get_Int (Glib.Values.Nth (Params, 2));
      Text   : constant String    :=
        Glib.Values.Get_String (Glib.Values.Nth (Params, 1), Length);
   begin
      declare
         Number : My_Natural;
         pragma Unreferenced (Number);
      begin
         Number := My_Natural'Value (Text);
      exception
         when Constraint_Error =>
            --  refuse values that are not parsable
            Gtk.Handlers.Emit_Stop_By_Name
              (Object => Object,
               Name   => Gtk.Editable.Signal_Insert_Text);
      end;
   end On_Insert_Text;

   --  Callback for click event
   procedure On_Increment_Click
     (Object : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      The_Value := The_Value + 1;
      Check_Value;
      Update_Entry;
   end On_Increment_Click;

   --  Callback for click event
   procedure On_Decrement_Click
     (Object : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      The_Value := The_Value - 1;
      Check_Value;
      Update_Entry;
   end On_Decrement_Click;

   --  Callback for delete event
   function On_Main_Window_Delete
     (Object : access Gtk.Window.Gtk_Window_Record'Class)
      return   Boolean
   is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Main_Quit;
      return True;
   end On_Main_Window_Delete;

begin

   Gtk.Main.Init;

   Gtk.GEntry.Gtk_New (Widget => Entry_Field);
   Entry_Callbacks.Connect
     (Widget => Entry_Field,
      Name   => Gtk.Editable.Signal_Insert_Text,
      Cb     => On_Insert_Text'Access);
   Entry_Callbacks.Connect
     (Widget => Entry_Field,
      Name   => Gtk.Editable.Signal_Changed,
      Cb     => On_Changed_Text'Access);

   Gtk.Button.Gtk_New (Button => Increment_Button, Label => "Increment");
   Gtk.Button.Gtk_New (Button => Decrement_Button, Label => "Decrement");

   Button_Callbacks.Connect
     (Widget => Increment_Button,
      Name   => Gtk.Button.Signal_Clicked,
      Marsh  => Button_Callbacks.To_Marshaller (On_Increment_Click'Access));
   Button_Callbacks.Connect
     (Widget => Decrement_Button,
      Name   => Gtk.Button.Signal_Clicked,
      Marsh  => Button_Callbacks.To_Marshaller (On_Decrement_Click'Access));

   Gtk.Box.Gtk_New_Vbox (Box => Content);
   Gtk.Box.Add (Container => Content, Widget => Entry_Field);
   Gtk.Box.Add (Container => Content, Widget => Increment_Button);
   Gtk.Box.Add (Container => Content, Widget => Decrement_Button);

   Gtk.Window.Gtk_New (Window => Main_Window);
   Gtk.Window.Add (Container => Main_Window, Widget => Content);

   Window_Callbacks.Connect
     (Widget => Main_Window,
      Name   => Gtk.Widget.Signal_Delete_Event,
      Cb     => On_Main_Window_Delete'Access);
   Gtk.Window.Show_All (Widget => Main_Window);
   Update_Entry;

   Gtk.Main.Main;
end Disabling;
```



## AutoHotkey


```AutoHotkey
GUI, Add, Edit, w150 number vValue gEnableDisable, 0 ; Number specifies a numbers-only edit field. g<Subroutine> specifies a subroutine to run when the value of control changes.
GUI, Add, button,, Increment
GUI, Add, button, xp+70 yp, Decrement ; xp+70 and yp are merely positioning options
GUI, Show, w200 y200, Title	      ; Shows the GUI. Add your own title if you wish
;No timer is needed
return 				      ; ----------End Auto-Execute Section----------

ButtonIncrement:
    GUI, Submit, NoHide         ; "Set the contents of each variable to the contents of their corresponding controls without hiding the GUI"
    If ( value < 10 )           ; Just in case EnableDisable didn't disable the button it in time.
        Value++                 ; Increment Value
    GUIControl,, Value, %value% ; "Set the text of the control which alters the variable 'value' to the contents of 'value'"
return


ButtonDecrement:
    GUI, Submit, Nohide
    If value > 0
        Value--
    GuiControl,, Value, %value%
return


EnableDisable:
    GUI, Submit, Nohide
    If ( value < 10 )
        GuiControl, enable, Increment
    Else
        GuiControl, disable, Increment

    If ( value > 0)
        GuiControl, enable, Decrement
    Else
        GuiControl, disable, Decrement

    If ( value = 0 )
        GuiControl, enable, Edit1
    Else
        GuiControl, disable, Edit1
return


GuiClose:
    ExitApp
; Ensures the script ends when the GUI is closed.
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"WINLIB2"
      INSTALL @lib$+"WINLIB5"
      ES_NUMBER = 8192

      form% = FN_newdialog("Rosetta Code", 100, 100, 100, 52, 8, 1000)
      idInc% = FN_setproc(PROCinc)
      idDec% = FN_setproc(PROCdec)

      PROC_static(form%, "Value:", 100, 10, 10, 24, 14, 0)
      PROC_editbox(form%, "0", 101, 40, 8, 52, 14, ES_NUMBER)
      PROC_pushbutton(form%, "Increment", idInc%, 7, 30, 40, 16, 0)
      PROC_pushbutton(form%, "Decrement", idDec%, 52, 30, 40, 16, 0)
      PROC_showdialog(form%)

      REPEAT
        WAIT 1
        SYS "GetDlgItemInt", !form%, 101, 0, 1 TO number%
        SYS "GetDlgItem", !form%, 101 TO hedit%
        SYS "EnableWindow", hedit%, number% = 0
        SYS "GetDlgItem", !form%, idInc% TO hinc%
        SYS "EnableWindow", hinc%, number% < 10
        SYS "GetDlgItem", !form%, idDec% TO hdec%
        SYS "EnableWindow", hdec%, number% > 0
      UNTIL !form% = 0
      QUIT

      DEF PROCinc
      LOCAL number%
      SYS "GetDlgItemInt", !form%, 101, 0, 1 TO number%
      SYS "SetDlgItemInt", !form%, 101, number% + 1, 1
      ENDPROC

      DEF PROCdec
      LOCAL number%
      SYS "GetDlgItemInt", !form%, 101, 0, 1 TO number%
      SYS "SetDlgItemInt", !form%, 101, number% - 1, 1
      ENDPROC
```

{{out}}
<p>
[[File:Guienabbc.gif]]


## C

{{works with|C (with Win32 API)}}


```txt
file main.c
```



```c>#include <windows.h

#include "resource.h"

#define MIN_VALUE    0
#define MAX_VALUE   10

BOOL CALLBACK DlgProc( HWND hwnd, UINT msg, WPARAM wPar, LPARAM lPar );
void Increment( HWND hwnd );
void Decrement( HWND hwnd );
void SetControlsState( HWND hwnd );

int WINAPI WinMain( HINSTANCE hInst, HINSTANCE hPInst, LPSTR cmdLn, int show ) {
    return DialogBox( hInst, MAKEINTRESOURCE(IDD_DLG), NULL, DlgProc );
}

BOOL CALLBACK DlgProc( HWND hwnd, UINT msg, WPARAM wPar, LPARAM lPar ) {
    switch( msg ) {

        case WM_INITDIALOG:
            srand( GetTickCount() );
            SetDlgItemInt( hwnd, IDC_INPUT, 0, FALSE );
            break;

        case WM_COMMAND:
            switch( LOWORD(wPar) ) {
                case IDC_INCREMENT:
                    Increment( hwnd );
                    break;
                case IDC_DECREMENT:
                    Decrement( hwnd );
                    break;
                case IDC_INPUT:
                    // update controls' state according
                    // to the contents of the input field
                    if( HIWORD(wPar) == EN_CHANGE ) SetControlsState( hwnd );
                    break;
                case IDC_QUIT:
                    SendMessage( hwnd, WM_CLOSE, 0, 0 );
                    break;
                default: ;
            }
            break;

        case WM_CLOSE: {
            int reply = MessageBox( hwnd,
                "Do you really want to quit?",
                "Quit confirmation", MB_ICONQUESTION|MB_YESNO );
            if( reply == IDYES )
                EndDialog( hwnd, 0 );
            } break;

        default: ;
    }

    return 0;
}

void Increment( HWND hwnd ) {
    UINT n = GetDlgItemInt( hwnd, IDC_INPUT, NULL, FALSE );

    if( n < MAX_VALUE ) {
        SetDlgItemInt( hwnd, IDC_INPUT, ++n, FALSE );
        SetControlsState( hwnd );
    }
}

void Decrement( HWND hwnd ) {
    UINT n = GetDlgItemInt( hwnd, IDC_INPUT, NULL, FALSE );

    if( n > MIN_VALUE ) {
        SetDlgItemInt( hwnd, IDC_INPUT, --n, FALSE );
        SetControlsState( hwnd );
    }
}

void SetControlsState( HWND hwnd ) {
    UINT n = GetDlgItemInt( hwnd, IDC_INPUT, NULL, FALSE );
    EnableWindow( GetDlgItem(hwnd,IDC_INCREMENT), n<MAX_VALUE );
    EnableWindow( GetDlgItem(hwnd,IDC_DECREMENT), n>MIN_VALUE );
}
```



```txt
file resource.h
```



```c
#define IDD_DLG                                  101
#define IDC_INPUT                               1001
#define IDC_INCREMENT                           1002
#define IDC_DECREMENT                           1003
#define IDC_QUIT                                1004
```



```txt
file resource.rc
```



```c>#include <windows.h

#include "resource.h"

LANGUAGE LANG_NEUTRAL, SUBLANG_NEUTRAL
IDD_DLG DIALOG 0, 0, 154, 46
STYLE DS_3DLOOK | DS_CENTER | DS_MODALFRAME | DS_SHELLFONT | WS_CAPTION | WS_VISIBLE | WS_POPUP | WS_SYSMENU
CAPTION "GUI Component Interaction"
FONT 12, "Ms Shell Dlg" {
    EDITTEXT        IDC_INPUT, 33, 7, 114, 12, ES_AUTOHSCROLL | ES_NUMBER
    PUSHBUTTON      "Increment", IDC_INCREMENT, 7, 25, 50, 14
    PUSHBUTTON      "Decrement", IDC_DECREMENT, 62, 25, 50, 14, WS_DISABLED
    PUSHBUTTON      "Quit", IDC_QUIT, 117, 25, 30, 14
    RTEXT           "Value:", -1, 10, 8, 20, 8
}
```



## C++

with Qt 4.4, creating project file with qmake -project, Makefile with qmake -o Makefile <projectfile> and finally make

```txt
file task.h
```


```cpp
#ifndef TASK_H
#define TASK_H

#include <QWidget>

class QPushButton ;
class QString ;
class QLineEdit ;
class QLabel ;
class QVBoxLayout ;
class QHBoxLayout ;

class MyWidget : public QWidget {

    Q_OBJECT
public:
   MyWidget( QWidget *parent = 0 ) ;
private slots:
   void buttonChange( const QString & ) ;
   void addField( ) ;
   void subtractField( ) ;
private :
   QVBoxLayout *thisWidgetLayout ;
   QLabel *instruction ;
   QPushButton *increment ;
   QPushButton *decrement ;
   QLineEdit *entryField ;
   QHBoxLayout *lowerPart ;
} ;
#endif
```



```txt
file task.cpp
```


```cpp>#include <QtGui

#include <QString>
#include "task.h"

MyWidget::MyWidget ( QWidget *parent )
   : QWidget( parent ) {
   thisWidgetLayout = new QVBoxLayout ( this )  ;
   instruction = new QLabel ;
   instruction->setText( "Enter a number between 1 and 10 ! Numbers above 10 are decremented, below 0 incremented!" ) ;
   instruction->setWordWrap( true ) ;
   lowerPart = new QHBoxLayout ;
   entryField = new QLineEdit( "0" ) ;
   increment = new QPushButton( "Increment" ) ;
   decrement = new QPushButton( "Decrement" ) ;
   increment->setDefault( true ) ;
   connect( entryField , SIGNAL ( textChanged ( const QString &  ) ) ,
	    this , SLOT ( buttonChange( const QString & )) ) ;
   connect( entryField , SIGNAL ( textEdited ( const QString &  ) ) ,
	    this , SLOT ( buttonChange( const QString & )) ) ;
   connect( increment , SIGNAL ( clicked( ) ) , this ,
	 SLOT ( addField( ) )) ;
   connect( decrement , SIGNAL ( clicked( ) ) , this ,
	 SLOT ( subtractField( ))) ;
   lowerPart->addWidget( entryField ) ;
   lowerPart->addWidget( increment ) ;
   lowerPart->addWidget( decrement ) ;
   thisWidgetLayout->addWidget( instruction ) ;
   thisWidgetLayout->addLayout( lowerPart ) ;
   setLayout( thisWidgetLayout ) ;
}

void MyWidget::buttonChange( const QString & text ) {
   bool ok ;
   increment->setEnabled( text.toInt( &ok, 10 ) < 10 ) ;
   increment->setDisabled( text.toInt( &ok, 10 ) > 9 ) ;
   decrement->setEnabled( text.toInt( &ok, 10 ) > 0 ) ;
   decrement->setDisabled( text.toInt( &ok, 10 ) <= 0 ) ;
   if ( ! ( text == "0" ) )
      entryField->setReadOnly( true ) ;
}

void MyWidget::addField( ) {
   bool ok ;
   int number = entryField->text( ).toInt( &ok , 10 ) ;
   number++ ;
   entryField->setText( QString("%1").arg( number )) ;
}

void MyWidget::subtractField( ) {
   bool ok ;
   int number = entryField->text( ).toInt( &ok , 10 ) ;
   number-- ;
   entryField->setText( QString("%1").arg( number )) ;
}
```



```txt
main.cpp
```


```cpp>#include <QApplication

#include "task.h"

int main( int argc, char *argv[ ] ) {
   QApplication app( argc , argv ) ;
   MyWidget theWidget ;
   theWidget.show( ) ;
   return app.exec( ) ;
}
```



## C sharp

Using Windows Forms; compile with csc -t:winexe Program.cs (on MS.NET) or gmcs -t:winexe Program.cs (on Mono)

```csharp
using System;
using System.ComponentModel;
using System.Windows.Forms;

class RosettaInteractionForm : Form
{

    // Model used for DataBinding.
    // Notifies bound controls about Value changes.
    class NumberModel: INotifyPropertyChanged
    {
        // initialize event with empty delegate to avoid checks on null
        public event PropertyChangedEventHandler PropertyChanged = delegate {};

        int _value;
        public int Value
        {
            get { return _value; }
            set
            {
                _value = value;
                // Notify bound control about value change
                PropertyChanged(this, new PropertyChangedEventArgs("Value"));
            }
        }
    }

    NumberModel model = new NumberModel{ Value = 0};

    RosettaInteractionForm()
    {
        //MaskedTextBox is a TextBox variety with built-in input validation
        var tbNumber = new MaskedTextBox
                        {
                            Mask="0000",            // allow 4 decimal digits only
                            ResetOnSpace = false,   // don't enter spaces
                            Dock = DockStyle.Top    // place at the top of form
                        };
        // bound TextBox.Text to NumberModel.Value;
        tbNumber.DataBindings.Add("Text", model, "Value");
        var enabledIfZero = new Binding("Enabled", model, "Value");
        EnableControlWhen(tbNumber, value => value == 0);

        var btIncrement = new Button{Text = "Increment", Dock = DockStyle.Bottom};
        btIncrement.Click += delegate
                        {
                            model.Value++;
                        };
        EnableControlWhen(btIncrement, value => value < 10);
        var btDecrement = new Button{Text = "Decrement", Dock = DockStyle.Bottom};
        btDecrement.Click += delegate
                        {
                            model.Value--;
                        };
        EnableControlWhen(btDecrement, value => value > 0);
        Controls.Add(tbNumber);
        Controls.Add(btIncrement);
        Controls.Add(btDecrement);
    }

    // common part of creating bindings for Enabled property
    void EnableControlWhen(Control ctrl, Func<int, bool> predicate)
    {
        // bind Control.Enabled to NumberModel.Value
        var enabledBinding = new Binding("Enabled", model, "Value");
        // Format event is called when model value should be converted to Control value.
        enabledBinding.Format += (sender, args) =>
            {
                // Enabled property is of bool type.
                if (args.DesiredType != typeof(bool)) return;
                // set resulting value by applying condition
                args.Value = predicate((int)args.Value);
            };
        // as a result, control will be enabled if predicate returns true
        ctrl.DataBindings.Add(enabledBinding);
    }

    static void Main()
    {
        Application.Run(new RosettaInteractionForm());
    }
}
```



## Delphi


```delphi
type
  TForm1 = class(TForm)
    MaskEditValue: TMaskEdit; // Set Editmask on: "99;0; "
    SpeedButtonIncrement: TSpeedButton;
    SpeedButtonDecrement: TSpeedButton;
    procedure MaskEditValueChange(Sender: TObject);
    procedure SpeedButtonDecrementClick(Sender: TObject);
    procedure SpeedButtonIncrementClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

procedure TForm1.MaskEditValueChange(Sender: TObject);
begin
  TMaskEdit(Sender).Enabled := StrToIntDef(Trim(TMaskEdit(Sender).Text), 0) = 0;
  SpeedButtonIncrement.Enabled := StrToIntDef(Trim(TMaskEdit(Sender).Text), 0) < 10;
  SpeedButtonDecrement.Enabled := StrToIntDef(Trim(TMaskEdit(Sender).Text), 0) > 0;
end;

procedure TForm1.SpeedButtonDecrementClick(Sender: TObject);
begin
  MaskEditValue.Text := IntToStr(Pred(StrToIntDef(Trim(MaskEditValue.Text), 0)));
end;

procedure TForm1.SpeedButtonIncrementClick(Sender: TObject);
begin
  MaskEditValue.Text := IntToStr(Succ(StrToIntDef(Trim(MaskEditValue.Text), 0)));
end;
```


## Fantom



```fantom
using fwt
using gfx

class Main
{
  public static Void main ()
  {
    Window
    {
      textField := Text
      {
        text = "0"
      }
      incButton := Button
      {
        text = "increment"
        onAction.add |Event e|
        {
          if (textField.text != "")
          {
            try
            {
              Int value := textField.text.toInt
              if (value < 10) value += 1
              textField.text = value.toStr
            }
            catch {}
          }
        }
      }
      decButton := Button
      {
        text = "decrement"
        onAction.add |Event e|
        {
          if (textField.text != "")
          {
            try
            {
              Int value := textField.text.toInt
              if (value > 0) value -= 1
              textField.text = value.toStr
            }
            catch {}
          }
        }
      }
      // add a listener for modifications to the text field
      // which updates the button visibilities
      textField.onModify.add |Event e|
      {
        try
        {
          Int value := textField.text.toInt
          incButton.enabled = (value < 10) // update whether button can be used
          decButton.enabled = (value > 0)  // update whether button can be used
        }
        catch textField.text = "0"  // reset if not an int
      }
      EdgePane
      {
        top = textField
        left = incButton
        right = decButton
      },
    }.open
  }
}
```



## FreeBASIC


```FreeBASIC

#Include "windows.bi"

Dim As HWND Window_Main, Edit_Number, Button_Inc, Button_Dec
Dim As MSG msg
Dim As Integer n
Dim As String text

'Create a window with an input field and two buttons:
Window_Main = CreateWindow("#32770", "GUI Enabling/Disabling of Controls", WS_OVERLAPPEDWINDOW Or WS_VISIBLE, 100, 100, 250, 200, 0, 0, 0, 0)
Var Static_Number = CreateWindow("STATIC", "Value:", WS_VISIBLE Or WS_CHILD, 10, 10, 100, 20, Window_Main, 0, 0, 0)
Edit_Number = CreateWindow("EDIT", "0", WS_BORDER Or WS_VISIBLE Or WS_CHILD Or ES_AUTOHSCROLL Or ES_Number, 110, 10, 100, 20, Window_Main, 0, 0, 0)
Button_Inc = CreateWindow("BUTTON", "Increment", WS_VISIBLE Or WS_CHILD, 110, 40, 100, 20, Window_Main, 0, 0, 0)
Button_Dec = CreateWindow("BUTTON", "Decrement", WS_VISIBLE Or WS_CHILD, 110, 70, 100, 20, Window_Main, 0, 0, 0)

'Windows message loop:
While GetMessage(@msg, Window_Main, 0, 0)
  text = Space(GetWindowTextLength(Edit_Number) + 1)	'Buffer for the text
  GetWindowText(Edit_Number, text, Len(text))
  n = Val(text)
  If n <> 0 Then EnableWindow(Edit_Number, False) Else EnableWindow(Edit_Number, True)
  If n > 9 Then EnableWindow(Button_Inc, False) Else EnableWindow(Button_Inc, True)
  If n > 0 Then EnableWindow(Button_Dec, True) Else EnableWindow(Button_Dec, False)
  TranslateMessage(@msg)
  DispatchMessage(@msg)
  Select Case msg.hwnd
    Case Button_Inc
      'Increment value:
      If msg.message = WM_LBUTTONDOWN Then SetWindowText(Edit_Number, Str(n + 1))
   Case Button_Dec
     'Decrement value:
     If msg.message = WM_LBUTTONDOWN Then SetWindowText(Edit_Number, Str(n - 1))
   Case Window_Main
    If msg.message = WM_COMMAND Then End
  End Select
Wend

End

```



## Gambas


```gambas
hValueBox As ValueBox                                             'We need a ValueBox
hButton0 As Button                                                'We need a button
hButton1 As Button                                                'We need another button

Public Sub Form_Open()

With Me                                                           'Set the Form's Properties..
  .height = 95                                                    'Set the Height
  .Width = 350                                                    'Set the Width
  .Arrangement = Arrange.Vertical                                 'Arrange items vertically
  .Padding = 5                                                    'Border area
  .Title = "GUI enable/disable"                                   'Title displayed on the Form
End With

hValueBox = New ValueBox(Me) As "ValBox"                          'Add a ValueBox to the Form as Event 'ValBox'

With hValueBox                                                    'Set the ValueBox's Properties..
  .Expand = True                                                  'Expand the ValueBox
  .Value = 0                                                      'Set it's value to 0
End With

hButton0 = New Button(Me) As "ButtonInc"                          'Add a Button to the form as Event "ButtonInc"

With hButton0                                                     'Set the Button's Properties..
  .Height = 28                                                    'Set the Height
  .Text = "&Increment"                                            'Add Text (The '&' adds a keyboard shortcut)
End With

hButton1 = New Button(Me) As "ButtonDec"                          'Add a Button to the form as Event "ButtonDec"

With hButton1                                                     'Set the Button's Properties..
  .Height = 28                                                    'Set the Height
  .Text = "&Decrement"                                            'Add Text (The '&' adds a keyboard shortcut)
  .Enabled = False                                                'Disable the button
End With

End

Public Sub ButtonInc_Click()                                      'When the 'Increment' Button is clicked..

hValueBox.Value += 1                                              'Increase the Value in the ValueBox by 1
Checks

End

Public Sub ButtonDec_Click()                                      'When the 'Decrement' Button is clicked..

hValueBox.Value -= 1                                              'Increase the Value in the ValueBox by 1
Checks

End

Public Sub Checks()                                               'Checks the values to see which controls to en/disable

If hValueBox.Value = 0 Then                                       'If the ValueBox = 0 then..
  hValueBox.enabled = True                                        'Enable the control
Else                                                              'Else..
  hValueBox.enabled = False                                       'Disable the control
End If

If hValueBox.Value > 9 Then                                       'If the ValueBox greater than 9 then..
  hButton0.enabled = False                                        'Disable the control
Else                                                              'Else..
  hButton0.enabled = True                                         'Enable the control
End If

If hValueBox.Value < 1 Then                                       'If the ValueBox less than 1 then..
  hButton1.enabled = False                                        'Disable the control
Else                                                              'Else..
  hButton1.enabled = True                                         'Enable the control
End If

End

Public Sub ValBox_Leave()                                         'When the mouse leaves the ValueBox..

Checks                                                            'Rune the Checks routine

End
```



## Go

{{libheader|Gotk3}}
Loosely based on the Vala entry.

```go
package main

import (
    "github.com/gotk3/gotk3/gtk"
    "log"
    "math/rand"
    "strconv"
    "time"
)

func validateInput(window *gtk.Window, str string) (int64, bool) {
    i, err := strconv.ParseInt(str, 10, 64)
    if err != nil {
        dialog := gtk.MessageDialogNew(
            window,
            gtk.DIALOG_MODAL,
            gtk.MESSAGE_ERROR,
            gtk.BUTTONS_OK,
            "Invalid value",
        )
        dialog.Run()
        dialog.Destroy()
        return 0, false
    }
    return i, true
}

func check(err error, msg string) {
    if err != nil {
        log.Fatal(msg, err)
    }
}

func processState(i int64, entry *gtk.Entry, ib, db *gtk.Button) {
    if i == 0 {
        entry.SetSensitive(true)
    } else {
        entry.SetSensitive(false)
    }
    if i < 10 {
        ib.SetSensitive(true)
    } else {
        ib.SetSensitive(false)
    }
    if i > 0 {
        db.SetSensitive(true)
    } else {
        db.SetSensitive(false)
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    gtk.Init(nil)

    window, err := gtk.WindowNew(gtk.WINDOW_TOPLEVEL)
    check(err, "Unable to create window:")
    window.SetTitle("Rosetta Code")
    window.SetPosition(gtk.WIN_POS_CENTER)
    window.Connect("destroy", func() {
        gtk.MainQuit()
    })

    box, err := gtk.BoxNew(gtk.ORIENTATION_HORIZONTAL, 1)
    check(err, "Unable to create box:")
    box.SetBorderWidth(1)

    label, err := gtk.LabelNew("Value:")
    check(err, "Unable to create label:")

    entry, err := gtk.EntryNew()
    check(err, "Unable to create entry:")
    entry.SetText("0") // initialize to zero
    entry.SetSensitive(true)

    // button to increment
    ib, err := gtk.ButtonNewWithLabel("Increment")
    check(err, "Unable to create increment button:")
    ib.SetSensitive(true)

    // button to decrement
    db, err := gtk.ButtonNewWithLabel("Decrement")
    check(err, "Unable to create decrement button:")
    db.SetSensitive(false)

    entry.Connect("activate", func() {
        // read and validate the entered value
        str, _ := entry.GetText()
        i, ok := validateInput(window, str)
        if !ok {
            entry.SetText("0")
        }
        processState(i, entry, ib, db)
    })

    ib.Connect("clicked", func() {
        // read the entered value
        str, _ := entry.GetText()
        i, _ := validateInput(window, str)
        i++
        entry.SetText(strconv.FormatInt(i, 10))
        processState(i, entry, ib, db)
    })

    db.Connect("clicked", func() {
        // read the entered value
        str, _ := entry.GetText()
        i, _ := validateInput(window, str)
        i--
        entry.SetText(strconv.FormatInt(i, 10))
        processState(i, entry, ib, db)
    })

    box.PackStart(label, false, false, 2)
    box.PackStart(entry, false, false, 2)
    box.PackStart(ib, false, false, 2)
    box.PackStart(db, false, false, 2)
    window.Add(box)

    window.ShowAll()
    gtk.Main()
}
```


==Icon and {{header|Unicon}}==

This uses the Unicon specific graphics library.


```Unicon

import gui
$include "guih.icn"

class MessageDialog : Dialog (message)
  method component_setup ()
    label := Label ("label="||message, "pos=20,20")
    add (label)
    button := TextButton("label=OK", "pos=100,60")
    button.connect (self, "dispose", ACTION_EVENT)
    add (button)

    connect (self, "dispose", CLOSE_BUTTON_EVENT)
    attrib ("size=200,100", "bg=light gray")
  end

  initially (message)
    self.Dialog.initially()
    self.message := message
end

class WindowApp : Dialog (button1, button2, field, value)
  method set_enabled ()
    if value = 0
      then field.clear_is_shaded ()
      else field.set_is_shaded ()
    if value <= 0
      then button2.set_is_shaded ()
      else button2.clear_is_shaded ()
    if value >= 10
      then button1.set_is_shaded ()
      else button1.clear_is_shaded ()
  end

  method increment ()
    value +:= 1
    field.set_contents (string(value))
    set_enabled ()
  end

  method decrement ()
    value -:= 1
    field.set_contents (string(value))
    set_enabled ()
  end

  method handle_text_field ()
    if not(integer(field.get_contents ()))
      then {
        warning := MessageDialog ("Not a number")
        warning.show_modal ()
        field.set_contents (string(value))
      }
      else {
        n := integer (field.get_contents ())
        if not (0 <= n <= 10)
          then {
            warning := MessageDialog ("Not in range")
            warning.show_modal ()
            field.set_contents (string(value))
          }
      }
    value := integer (field.get_contents ()) # must be ok
    set_enabled ()
  end

  method component_setup ()
    value := 0
    field := TextField("contents="||value, "pos=20,20", "size=150")
    field.connect (self, "handle_text_field", TEXTFIELD_CHANGED_EVENT)
    add (field)
    button1 := TextButton("label=Increment", "pos=20,60", "size=70")
    button1.connect (self, "increment", ACTION_EVENT)
    add (button1)
    button2 := TextButton("label=Decrement", "pos=100,60", "size=70")
    button2.connect (self, "decrement", ACTION_EVENT)
    add (button2)

    connect (self, "dispose", CLOSE_BUTTON_EVENT)
    attrib ("size=200,100", "bg=light gray")
    set_enabled ()
  end
end

procedure main ()
  w := WindowApp ()
  w.show_modal ()
end

```



## J

'''J 8.x'''

```j
task_run=: wd bind (noun define)
  pc task nosize;
  cc decrement button;cn "Decrement";
  cc increment button;cn "Increment";
  cc Value edit center;set Value text 0;
  set decrement enable 0;
  pas 6 6;pcenter;
  pshow;
)

task_cancel=: task_close=: wd bind 'pclose'

task_Value_button=: update=: verb define
  wd 'set Value text ', ": n=. {. 0 ". Value
  wd 'set Value enable ', ": n=0
  wd 'set increment enable ', ": n<10
  wd 'set decrement enable ', ": n>0
)

task_increment_button=:verb define
  update Value=: ": 1 + 0 ". Value
)

task_decrement_button=:verb define
  update Value=: ": _1 + 0 ". Value
)
```


'''J 6.x'''

```J
task_run=: wd bind (noun define)
  pc task nosize;
  xywh 6 30 48 12;cc decrement button;cn "-";
  xywh 6 18 48 12;cc increment button;cn "+";
  xywh 6  6 48 12;cc Value edit; set Value 0;
  pas 6 6;pcenter;
  pshow;
)

task_close=: wd bind 'pclose'

task_Value_button=: update=: verb define
  wd 'set Value ', ": n=. {. 0 ". Value
  wd 'setenable Value ', ": n=0
  wd 'setenable increment ', ": n<10
  wd 'setenable decrement ', ": n>0
)

task_increment_button=:verb define
  update Value=: ": 1 + 0 ". Value
)
task_decrement_button=:verb define
  update Value=: ": _1 + 0 ". Value
)
```


Example use:

<lang>  task_run''
```



## Java

{{works with|Swing}}
{{works with|AWT}}

```java
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

public class Interact extends JFrame{
	final JTextField numberField;
	final JButton incButton, decButton;

	public Interact(){
		//stop the GUI threads when the user hits the X button
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		numberField = new JTextField();
		incButton = new JButton("Increment");
		decButton = new JButton("Decrement");

		numberField.setText("0");//start at 0
		decButton.setEnabled(false);//we're already at 0

		//listen for button presses in the text field
		numberField.addKeyListener(new KeyListener(){
			@Override
			public void keyTyped(KeyEvent e) {
				//if the entered character is not a digit
				if(!Character.isDigit(e.getKeyChar())){
					//eat the event (i.e. stop it from being processed)
					e.consume();
				}else if(Character.isDigit(e.getKeyChar())){
					//This method is executed from the event thread and updating the GUI
					//from there doesn't always work. invokeLater will ensure that the
					//GUI is updated
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
							String text = numberField.getText();
							if(text.isEmpty()){//default to 0 when all text is erased
								numberField.setText("0");
								decButton.setEnabled(false);
								incButton.setEnabled(true);
								return;
							}
							if(Long.valueOf(text) <= 0){
								decButton.setEnabled(false);
								incButton.setEnabled(true);
							}else if(Long.valueOf(text) >= 10){
								incButton.setEnabled(false);
								decButton.setEnabled(true);
							}else{
								incButton.setEnabled(true);
								decButton.setEnabled(true);
							}
						}
					});
				}
			}
			@Override
			public void keyReleased(KeyEvent e){}
			@Override
			public void keyPressed(KeyEvent e){
				//backspace and delete don't register in keyTyped because they don't
				//display a Unicode character, so they must be handled here
				if(e.getKeyCode() == KeyEvent.VK_BACK_SPACE ||
						e.getKeyCode() == KeyEvent.VK_DELETE){
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
							String text = numberField.getText();
							if(text.isEmpty()){
								numberField.setText("0");
								decButton.setEnabled(false);
								incButton.setEnabled(true);
								return;
							}
							if(Long.valueOf(text) <= 0){
								decButton.setEnabled(false);
								incButton.setEnabled(true);
							}else if(Long.valueOf(text) >= 10){
								incButton.setEnabled(false);
								decButton.setEnabled(true);
							}else{
								incButton.setEnabled(true);
								decButton.setEnabled(true);
							}
						}
					});
				}
			}
		});

		//listen for button clicks on the increment button
		incButton.addActionListener(new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent e) {
				String text = numberField.getText();
				numberField.setText((Long.valueOf(text) + 1) + "");
				if(Long.valueOf(text) + 1 >= 10){
					incButton.setEnabled(false);
				}

				if(Long.valueOf(text) + 1 > 0){
					decButton.setEnabled(true);
				}
			}
		});

		//listen for button clicks on the random button
		decButton.addActionListener(new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent e) {
				String text = numberField.getText();
				numberField.setText((Long.valueOf(text) - 1) + "");
				if(Long.valueOf(text) - 1 <= 0){
					decButton.setEnabled(false);
				}

				if(Long.valueOf(text) - 1 < 10){
					incButton.setEnabled(true);
				}
			}
		});

		//arrange the components in a grid with 2 rows and 1 column
		setLayout(new GridLayout(2, 1));

		//a secondary panel for arranging both buttons in one grid space in the window
		JPanel buttonPanel = new JPanel();

		//the buttons are in a grid with 1 row and 2 columns
		buttonPanel.setLayout(new GridLayout(1, 2));
		//add the buttons
		buttonPanel.add(incButton);
		buttonPanel.add(decButton);

		//put the number field on top of the buttons
		add(numberField);
		add(buttonPanel);
		//size the window appropriately
		pack();

	}

	public static void main(String[] args){
		new Interact().setVisible(true);
	}
}
```



{{libheader|JavaFX}}
{{works with|Java|8}}

```java

import javafx.application.Application;
import javafx.beans.property.LongProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.util.converter.NumberStringConverter;

public class InteractFX extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage stage) throws Exception {

        TextField input = new TextField("0"){
            // only accept numbers as input
            @Override public void replaceText(int start, int end, String text) {
                if (text.matches("[0-9]*")) {
                    super.replaceText(start, end, text);
                }
            }

            // only accept numbers on copy+paste
            @Override public void replaceSelection(String text) {
                if (text.matches("[0-9]*")) {
                    super.replaceSelection(text);
                }
            }
        };

        // when the textfield is empty, replace text with "0"
        input.textProperty().addListener((observable, oldValue, newValue)->{
            if(newValue == null || newValue.trim().isEmpty()){
                input.setText("0");
            }
        });


        // get a bi-directional bound long-property of the input value
        LongProperty inputValue = new SimpleLongProperty();
        input.textProperty().bindBidirectional(inputValue, new NumberStringConverter());

        // textfield is disabled when the current value is other than "0"
        input.disableProperty().bind(inputValue.isNotEqualTo(0));


        Button increment = new Button("Increment");
        increment.setOnAction(event-> inputValue.set(inputValue.get() + 1));

        // incr-button is disabled when input is >= 10
        increment.disableProperty().bind(inputValue.greaterThanOrEqualTo(10));


        Button decrement = new Button("Decrement");
        decrement.setOnAction(event-> inputValue.set(inputValue.get() - 1));

        // decrement button is disabled when input is <=0
        decrement.disableProperty().bind(inputValue.lessThanOrEqualTo(0));


        // layout
        VBox root = new VBox();
        root.getChildren().add(input);
        HBox buttons = new HBox();
        buttons.getChildren().addAll(increment,decrement);
        root.getChildren().add(buttons);

        stage.setScene(new Scene(root));
        stage.sizeToScene();
        stage.show();
    }
}

```



## Julia


```julia

using Tk
w = Toplevel("GUI enabling/disabling")
fr = Frame(w)
pack(fr, {:expand=>true, :fill => "both"})

value = Entry(fr)
increment = Button(fr, "+")
decrement = Button(fr, "-")

formlayout(value, "Value:")
formlayout(increment, " ")
formlayout(decrement, " ")

## value stores a string
set_value(value, "0") ## The field is initialized to zero.
get(value::Tk_Entry) = try parseint(get_value(value)) catch e nothing end

function update()
    cur_value = get(value)
    set_enabled(value,     isa(cur_value, Integer) && cur_value == 0)
    set_enabled(increment, isa(cur_value, Integer) && cur_value < 10)
    set_enabled(decrement, isa(cur_value, Integer) && cur_value > 0)
end

crement = function(step)
  set_enabled(value, true)
  set_value(value, string(get(value) + step))
  update()
end
tk_bind(increment, "command", path -> crement(1))
tk_bind(decrement, "command", path -> crement(-1))
update()

function validate_command(path, P)
    try
        if length(P) > 0 parseint(P); update() end
        tcl("expr", "TRUE")
    catch e
        tcl("expr", "FALSE")
    end
end
function invalid_command(path, W)
    println("Invalid value")
    tcl(W, "delete", "@0", "end")
end

tk_configure(value, {:validate=>"focusout", :validatecommand=>validate_command, :invalidcommand=>invalid_command })



```



## Kotlin

{{libheader|JavaFX}}
{{trans|Java}}

```scala
// version 1.2.21

import javafx.application.Application
import javafx.beans.property.SimpleLongProperty
import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.control.TextField
import javafx.scene.layout.HBox
import javafx.scene.layout.VBox
import javafx.stage.Stage
import javafx.util.converter.NumberStringConverter
import javafx.event.ActionEvent

val digits = Regex("[0-9]*")

class InteractFX : Application() {

    override fun start(stage: Stage) {
        val input = object : TextField("0") {
            // only accept numbers as input
            override fun replaceText(start: Int, end: Int, text: String) {
                if (text.matches(digits)) super.replaceText(start, end, text)
            }

            // only accept numbers on copy + paste
            override fun replaceSelection(text: String) {
                if (text.matches(digits)) super.replaceSelection(text)
            }
        }

        // when the textfield is empty, replace text with "0"
        input.textProperty().addListener { _, _, newValue ->
            if (newValue == null || newValue.trim().isEmpty()) input.text = "0"
        }

        // get a bi-directional bound long-property of the input value
        val inputValue = SimpleLongProperty()
        input.textProperty().bindBidirectional(inputValue, NumberStringConverter())

        // textfield is disabled when the current value is other than "0"
        input.disableProperty().bind(inputValue.isNotEqualTo(0))

        val increment = Button("Increment")
        increment.addEventHandler(ActionEvent.ACTION) { inputValue.set(inputValue.get() + 1) }

        // increment button is disabled when input is >= 10
        increment.disableProperty().bind(inputValue.greaterThanOrEqualTo(10))

        val decrement = Button("Decrement")
        decrement.addEventHandler(ActionEvent.ACTION) { inputValue.set(inputValue.get() - 1) }

        // decrement button is disabled when input is <= 0
        decrement.disableProperty().bind(inputValue.lessThanOrEqualTo(0))

        // layout
        val root = VBox()
        root.children.add(input)
        val buttons = HBox()
        buttons.children.addAll(increment, decrement)
        root.children.add(buttons)

        stage.scene = Scene(root)
        stage.sizeToScene()
        stage.show()
    }
}

fun main(args: Array<String>) {
    Application.launch(InteractFX::class.java, *args)
}
```



## Liberty BASIC


```lb
nomainwin
    textbox #demo.val, 20, 50, 90, 24
    button #demo.dec, "Decrement", [btnDecrement], UL, 20, 90, 90, 24
    button #demo.inc, "Increment", [btnIncrement], UL, 20, 120, 90, 24
    statictext #demo.txt, "Positive or negative whole numbers only.", 20, 170, 240, 24
    open "Rosetta Task: GUI enabling/disabling of controls" for window as #demo
    #demo "trapclose [quit]"
    #demo.val 0
    #demo.dec "!disable"
wait

[quit]
    close #demo
end

[btnDecrement]
    validNum = validNum()
    if validNum = 0 then
        #demo.val "!contents? nVal$"
        notice nVal$;" does not appear to be a valid whole number."
    else
        #demo.val "!contents? nVal"
        if nVal > 0 then
            nVal = nVal - 1
        end if
    end if
    #demo.val nVal
    call disEnableControls nVal
wait

[btnIncrement]
    validNum = validNum()
    if validNum = 0 then
        #demo.val "!contents? nVal$"
        notice nVal$;" does not appear to be a valid whole number."
    else
        #demo.val "!contents? nVal"
        if nVal < 10 then
            nVal = nVal + 1
        end if
    end if
    #demo.val nVal
    call disEnableControls nVal
wait

Function validNum()
    validNum$ = "0123456789"
    #demo.val "!contents? nVal$"
    nVal$ = trim$(nVal$)
    if left$(nVal$, 1) = "-" then
        neg = 1
        nVal$ = mid$(nVal$, 2)
    else
        neg = 0
    end if
    validNum = 1
    for i = 1 to len(nVal$)
        if instr(validNum$, mid$(nVal$, i, 1)) = 0 then
            validNum = 0
        end if
    next i
End Function

Sub disEnableControls nVal
    if nVal > 9 then
        #demo.inc "!disable"
    else
        #demo.inc "!enable"
    end if
    if nVal < 1 then
        #demo.dec "!disable"
    else
        #demo.dec "!enable"
    end if
    if nVal = 0 then
        #demo.val "!enable"
    else
        #demo.val "!disable"
    end if
End Sub
```



## LiveCode

In LiveCode, GUIs are expected to be designed using the IDE, with the developer writing scripts to handle messages that are passed by the LC Engine. It is certainly possible to dynamically create windows (stacks) and all the various controls, however it is somewhat antithetical to the LC approach of rapid development.

1. Create a new mainstack

2. Draw on two buttons

2.1 The first button name is "Button1", with a label of "Increment"

2.2 The second button name is "Button2", with a label of "Decrement"

3. Draw a text field and name it "Field1", enter 0 as its content.

4. Select Object->Card Script and enter the following to handle enabling the buttons.

```LiveCode
command enableButtons v
    switch
        case v <= 0
            put 0 into fld "Field1"
            set the enabled of btn "Button1" to true
            set the enabled of btn "Button2" to false
            break
        case v >= 10
            put 10 into fld "Field1"
            set the enabled of btn "Button1" to false
            set the enabled of btn "Button2" to true
            break
        default
            set the enabled of btn "Button1" to true
            set the enabled of btn "Button2" to true
            put v into fld "Field1"
    end switch
end enableButtons
```


For the increment button, click on Button 1 (in edit mode)  and the select "code" from the toolbar and enter the following to handle clicks
```LiveCode
on mouseUp
    put field "Field1" into x
    add 1 to x
    enableButtons x
end mouseUp
```


Repeat for Button 2 (note we are subtracting here)
```LiveCode
on mouseUp
    put field "Field1" into x
    subtract 1 from x
    enableButtons x
end mouseUp
```


Finally add the code to handle keyboard entry in text field's code. Note this code prevents entering digits outside 0-9 and limits the value between 0 and 10.
```LiveCode
on rawKeyDown k
    if numToChar(k) is among the items of "1,2,3,4,5,6,7,8,9,0" then
        if (numToChar(k) + the text of me) <= 10 then
            enableButtons (numToChar(k) + the text of me)
        end if
    else if k = 65288 then
        pass rawKeyDown
    end if
end rawKeyDown
```


Switch back to the form, and enter run mode to execute.

n.b. Open stack handlers would be prudent to initialise the default value and button states if this were to be saved for standalone execution.


## M2000 Interpreter

Normally a function can't call other except something global or something defined in function (local), or for member functions in Group object, they can call other members public or private at same level or deeper but then only the public members. Using Call Local we call functions like function's part. When an event service function called (by event), interpreter provide the same name as the module's name, where form declared, so this call is a "local call". So a second local call inside event service function,also provide same name. So global local1 called as local, so code executed as part of CheckIt (but without same "current" stack, and other specific to execution object properties). Modules, functions, threads, events are all executed on "execution objects" which carries the execution code.


```M2000 Interpreter

\\ this is global, but call as local in events, which means with local visibility for identifiers
\\ so thispos and this$ has to exist in caller 's context

Function Global Local1(new Feed$) {
            \\ this function can be used from other Integer
            \\ this$ and thispos, exist just before the call of this function
            local sgn$
            if feed$="" and this$="-" then thispos-- :  exit
            if left$(this$,1)="-" then sgn$="-": this$=mid$(this$, 2)
            if this$<>Trim$(this$)  then  this$=Feed$ :  thispos-- : exit
            If Trim$(this$)="" then this$="0" : thispos=2 : exit
            if instr(this$,"+")>0 and sgn$="-" then this$=filter$(this$, "+") : sgn$=""
            if instr(this$,"-")>0  and sgn$="" then this$=filter$(this$, "-") : sgn$="-"
            if filter$(this$,"0123456789")<>"" then this$=Feed$ :  thispos-- : exit
            if len(this$)>1 then While  left$(this$,1)="0" {this$=mid$(this$, 2)}
            this$=sgn$+this$
            if this$="-0" then this$="-" : thispos=2
        }
Module CheckIt {
      Declare form1 form
      Declare textbox1 textbox form form1
      Declare buttonInc Button form form1
      Declare buttonDec Button form form1
      Method textbox1, "move", 2000,2000,4000,600
      Method buttonInc, "move", 2000,3000,2000,600
      Method buttonDec, "move", 4000,3000,2000,600
      With textbox1,"vartext" as textbox1.value$, "Prompt", "Value:" ', "ShowAlways", True
      With buttonInc,"Caption","Increment"
      With buttonDec,"Caption","Decrement","Locked", True
      textbox1.value$="0"

      Function controlIncDec(what$){
            With buttonInc, "locked",  not val(what$)<10
            With buttonDec, "locked", not val(what$)>0
      }
      finishEnter=false
      Function TextBox1.ValidString {
                  \\ this function called direct from textbox
                  Read  New &this$, &thispos
                  Call Local local1(textbox1.value$)
                  Call Local controlIncDec(this$)
      }
      Function TextBox1.Enable {
            With TextBox1, "Enabled", true
            finishEnter=false
      }
      Function TextBox1.Disable {
            With TextBox1, "Enabled", false
            finishEnter=true
      }
      Function TextBox1.Enter {
            Call Local TextBox1.Disable()
      }
      Function buttonInc.Click {
                if  not finishEnter then Call Local TextBox1.Disable()
               textbox1.value$=str$(val(textbox1.value$)+1, "")
                if val(textbox1.value$)=0 then Call Local TextBox1.Enable()
      }
      function buttonDec.Click {
                if  not finishEnter then Call Local TextBox1.Disable()
                textbox1.value$=str$(val(textbox1.value$)-1, "")
                if val(textbox1.value$)=0 then Call Local TextBox1.Enable()
      }
      Call Local controlIncDec(textBox1.Value$)
      Method form1, "show", 1
      Declare form1 nothing
}
Checkit


```



## Maple

For this problem, you will need to open up Maple, go to the Components tab on the left, and insert a Text Area, and 2 Buttons. By right clicking each button, and clicking Component Properties, change one to Increase, and the other to Decrease. Then, click on 'Edit Content Changed Action". In the one labeled increase, type:


```Maple

Increase();

```


In the one labeled Decrease, type:


```Maple

Decrease();

```


Then, by clicking the 2 gears and opening up the start-up commands, enter this:

```Maple

macro(SP=DocumentTools:-SetProperty, GP=DocumentTools:-GetProperty);
with(Maplets[Elements]):
SP("Text",value,0);
Increase:=proc()
	SP("Text",value,parse(GP("Text",value))+1);
	if parse(GP("Text",value))>=10 then
		SP("Button0", enabled, false);
	else
		SP("Button0", enabled, true);
	end if;
	if parse(GP("Text",value))<=0 then
		SP("Button1", enabled, false);
	else
		SP("Button1", enabled, true);
	end if;
end proc;
Decrease:=proc()
	SP("Text",value,parse(GP("Text",value))-1);
	if parse(GP("Text",value))<=0 then
		SP("Button1", enabled, false);
	else
		SP("Button1", enabled, true);
	end if;
	if parse(GP("Text",value))>=10 then
		SP("Button0", enabled, false);
	else
		SP("Button0", enabled, true);
	end if;
end proc;

```



## Mathematica


```mathematica
Manipulate[Null, {{value, 0},
  InputField[Dynamic[value], Number,
    Enabled -> Dynamic[value == 0]] &},
 Row@{Button["increment", value++, Enabled -> Dynamic[value < 10]],
   Button["decrement", value--, Enabled -> Dynamic[value > 0]]}]
```



## NewLISP


```NewLISP
; file:   gui-enable.lsp
; url:    http://rosettacode.org/wiki/GUI_enabling/disabling_of_controls
; author: oofoe 2012-02-02

; Load library and initialize GUI server:
(load (append (env "NEWLISPDIR") "/guiserver.lsp"))
(gs:init)

; The "interlock" function maintains GUI consistency by disabling all
; controls, then selectively re-enabling them depending on the value
; in the textbox.
(define (interlock)
  (gs:disable 'value 'increment 'decrement)
  (let ((v (int (gs:get-text 'value))))
    (if (= 0 v)  (gs:enable 'value))
    (if (< v 10) (gs:enable 'increment))
    (if (< 0 v)  (gs:enable 'decrement))
    ))

; Callbacks.
(define (update f)
  (gs:set-text 'value (string (f (int (gs:get-text 'value)) 1)))
  (interlock))

(define (incrementing id) (update +))

(define (decrementing id) (update -))

(define (valuing id)      (interlock))

; Create main window frame and set layout direction.
(gs:frame 'main 100 100 200 75 "GUI Enable")
(gs:set-flow-layout 'main "center" 4 4)

; Create and add widgets.
(gs:button 'decrement 'decrementing "-" 30)
(gs:text-field 'value 'valuing 8)
(gs:set-text 'value "0")
(gs:button 'increment 'incrementing "+" 30)
(gs:add-to 'main 'decrement 'value 'increment)

; Show main window.
(gs:set-visible 'main true)

; Start event loop.
(gs:listen)

(exit)
```


Screenshot:

[[File:Newlisp-gui-enable.png]]


## Nim

==={{libheader|Gtk2}}===

```nim
import
  gtk2, strutils, glib2

var valu: int = 0
var chngd_txt_hndler: gulong = 0

proc thisCheckBtns   # forward declaration

proc thisDestroy(widget: pWidget, data: pgpointer){.cdecl.} =
  main_quit()

nim_init()
var window = window_new(gtk2.WINDOW_TOPLEVEL)
var content = vbox_new(TRUE,10)
var hbox1 = hbox_new(TRUE,10)
var entry_fld = entry_new()
entry_fld.set_text("0")
var btn_quit = button_new("Quit")
var btn_inc = button_new("Increment")
var btn_dec = button_new("Decrement")
add(hbox1,btn_inc)
add(hbox1,btn_dec)
pack_start(content, entry_fld, TRUE, TRUE, 0)
pack_start(content, hbox1, TRUE, TRUE, 0)
pack_start(content, btn_quit, TRUE, TRUE, 0)
set_border_width(Window, 5)
add(window, content)

proc thisInc(widget: pWidget, data: pgpointer){.cdecl.} =
  inc(valu)
  entry_fld.set_text($valu)
  thisCheckBtns()

proc thisDec(widget: pWidget, data: pgpointer){.cdecl.} =
  dec(valu)
  entry_fld.set_text($valu)
  thisCheckBtns()

proc thisTextChanged(widget: pWidget, data: pgpointer) {.cdecl.} =
  #signal_handler_block(entry_fld, chngd_txt_hndler)
  try:
     valu = parseInt($entry_fld.get_text())
  except EInvalidValue:
     valu = 0
  entry_fld.set_text($valu)
  #signal_handler_unblock(entry_fld, chngd_txt_hndler)
  #signal_emit_stop(entry_fld, signal_lookup("changed",TYPE_EDITABLE()),0)
  thisCheckBtns()

proc thisCheckBtns =
   set_sensitive(btn_inc, valu < 10)
   set_sensitive(btn_dec, valu > 0)
   set_sensitive(entry_fld, valu == 0)

discard signal_connect(window, "destroy",
                   SIGNAL_FUNC(thisDestroy), nil)
discard signal_connect(btn_quit, "clicked",
                   SIGNAL_FUNC(thisDestroy), nil)
discard signal_connect(btn_inc, "clicked",
                   SIGNAL_FUNC(thisInc), nil)
discard signal_connect(btn_dec, "clicked",
                   SIGNAL_FUNC(thisDec), nil)
chngd_txt_hndler = signal_connect(entry_fld, "changed",
                   SIGNAL_FUNC(thisTextChanged), nil)

show_all(window)
thisCheckBtns()
main()
```

==={{libheader|IUP}}===

```nim
import
  iup, strutils, math

# assumes you have the iup  .dll or .so installed

randomize()
discard iup.open(nil,nil)


var lbl = Label("Value:")
setAttribute(lbl,"PADDING","2x2")

var valu = Text(nil)
SetAtt(nil, valu, "PADDING", "2x2", "VALUE", "0", nil)

var txtBox = Hbox(lbl, valu, nil)
SetAttribute(txtBox, "MARGIN", "10x10")

var incBtn = Button("+1", "")
var decBtn = Button("-1", "")
SetAttribute(incBtn,"RASTERSIZE","25x25")
SetAttribute(decBtn,"RASTERSIZE","25x25")
var btnBox = Vbox(incBtn, decBtn, nil)
SetAttribute(btnBox, "MARGIN", "5x5")


proc toCB(fp: proc): ICallback =
   return cast[ICallback](fp)

proc setValuState(value: int) =
    if value == 0:
        SetAttribute(valu, "ACTIVE", $(true))
    else:
        SetAttribute(valu, "ACTIVE", $(false))

proc setBtnsState(value: int) =
   if value <= 0:
      SetAttribute(decBtn,"ACTIVE", $(false))
      SetAtt(nil, incBtn,"ACTIVE", $(true), "FOCUS", $(true), nil)
   elif value >= 10:
      SetAttribute(incBtn,"ACTIVE", $(false))
      SetAtt(nil, decBtn,"ACTIVE", $(true), "FOCUS", $(true), nil)
   else:
      SetAttribute(decBtn,"ACTIVE", $(true))
      SetAttribute(incBtn,"ACTIVE", $(true))


# Click handler for Click button
proc incClick(ih:PIhandle): cint {.cdecl.} =
    var s: string = $(GetAttribute(valu,"VALUE"))
    var x: int = 0
    try:
       x = 1 + parseInt(s)
    except:
       x = 1         # default to 1 if non-numeric entry
    setAttribute(valu,"VALUE", $x)
    setValuState(x)
    setBtnsState(x)
    return IUP_DEFAULT

# Click handler for Decrement button
proc decClick(ih:PIhandle): cint {.cdecl.} =
    var s: string = $(GetAttribute(valu,"VALUE"))
    var x: int = 0
    try:
       x = -1 + parseInt(s)
    except:
       x = 1         # default to 1 if non-numeric entry
    setAttribute(valu,"VALUE", $x)
    setValuState(x)
    setBtnsState(x)
    return IUP_DEFAULT

# Key handler to check for Esc pressed
proc key_cb(ih:PIhandle, c: cint):cint {.cdecl.} =
  #echo c
  if (c == Iup.K_esc) and (Iup.Alarm("Exit?", "Had enough?","Yes","Keep going",nil) == 1):
    return IUP_CLOSE    # Exit application
  return IUP_CONTINUE

var contents = Hbox(txtBox, btnBox, nil)
SetAttribute(contents, "MARGIN", "5x5")

discard setCallback(incBtn,"ACTION", toCB(incClick))
discard setCallback(decBtn,"ACTION", toCB(decClick))
discard setCallback(contents,"K_ANY", toCB(key_cb))

var dlg = Dialog(contents)
SetAtt(nil, dlg, "TITLE","GUI Interaction", "SIZE","200x75", nil)

discard dlg.show()
discard mainloop()
iup.close()
```



## Perl 6

Extremely basic implementation using the GTK library.

```perl6
use GTK::Simple;
use GTK::Simple::App;

my GTK::Simple::App $app .= new( title => 'Controls Enable / Disable' );

$app.set-content(
    my $box = GTK::Simple::HBox.new(
        my $inc   = GTK::Simple::Button.new( label => ' + ' ),
        my $value = GTK::Simple::Entry.new,
        my $dec   = GTK::Simple::Button.new( label => ' - ' )
    )
);

$app.border-width = 10;
$box.spacing = 10;

$value.changed.tap: {
    $value.text.=subst(/\D/, '');
    $inc.sensitive = $value.text < 10;
    $dec.sensitive = $value.text > 0;
}

$value.text = '0';

$inc.clicked.tap: { my $val = $value.text; $val += 1; $value.text = $val.Str }
$dec.clicked.tap: { my $val = $value.text; $val -= 1; $value.text = $val.Str }

$app.run;
```



## Phix

Note: this validates each input char separately, and '-' is not "NUMBER": the only way to get numbers <0 or >9 into the input field is to paste them.
{{libheader|pGUI}}

```Phix
include pGUI.e

Ihandle txt, inc, dec, hbx, vbx, dlg

function activate(integer v)
    IupSetInt(txt,"VALUE",v)
    IupSetAttribute(inc,"ACTIVE",iff(v<10,"YES":"NO"))
    IupSetAttribute(dec,"ACTIVE",iff(v>0,"YES":"NO"))
    IupSetAttribute(txt,"ACTIVE",iff(v=0,"YES":"NO"))
    return IUP_CONTINUE
end function

function valuechanged_cb(Ihandle /*ih*/)
    return activate(IupGetInt(txt,"VALUE"))
end function

function inc_cb(Ihandle /*ih*/)
    return activate(IupGetInt(txt,"VALUE")+1)
end function

function dec_cb(Ihandle /*ih*/)
    return activate(IupGetInt(txt,"VALUE")-1)
end function

function esc_close(Ihandle /*ih*/, atom c)
    return iff(c=K_ESC?IUP_CLOSE:IUP_CONTINUE)
end function

IupOpen()
txt = IupText("VALUECHANGED_CB",Icallback("valuechanged_cb"),"FILTER=NUMBER, EXPAND=YES")
inc = IupButton("increment",Icallback("inc_cb"))
dec = IupButton("decrement",Icallback("dec_cb"),"ACTIVE=NO")
hbx = IupHbox({inc,dec},"MARGIN=0x10, GAP=20")
vbx = IupVbox({txt,hbx},"MARGIN=40x20")
dlg = IupDialog(vbx)
IupSetCallback(dlg, "K_ANY", Icallback("esc_close"))
IupShow(dlg)
IupMainLoop()
IupClose()
```



## PicoLisp

The standard PicoLisp GUI is HTTP based. Connect your browser to
http://localhost:8080 after starting the following script.

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(load "@ext.l" "@lib/http.l" "@lib/xhtml.l" "@lib/form.l")

(de start ()
   (and (app) (zero *Number))
   (action
      (html 0 "Enable/Disable" "@lib.css" NIL
         (form NIL
            (gui '(+Var +Able +NumField) '*Number '(=0 *Number) 20 "Value")
            (gui '(+Able +JS +Button) '(> 10 *Number) "increment"
               '(inc '*Number) )
            (gui '(+Able +JS +Button) '(gt0 *Number) "decrement"
               '(dec '*Number) ) ) ) ) )

(server 8080 "!start")
(wait)
```



## Prolog

Works with SWI-Prolog and XPCE.

```Prolog
dialog('GUI_Interaction',
       [ object        :=
	   GUI_Interaction,
	 parts         :=
	   [ GUI_Interaction :=
	       dialog('Rosetta Code'),
	     Name      := label(name, 'Value :'),
	     Input_field     :=
	       text_item(input_field, '0'),
	     Increment       :=
	       button(increment),
	     Decrement          :=
	       button(decrement)
	   ],
	 modifications :=
	   [ Input_field := [ label  := 'Value :',
			      length := 28,
			      show_label := @off
			    ],
	     Decrement := [active := @off]
	   ],
	 layout        :=
	   [ area(Name,
		  area(50, 26, 25, 24)),
	     area(Input_field,
		  area(95, 24, 200, 24)),
	     area(Increment,
		  area(50, 90, 80, 24)),
	     area(Decrement,
		  area(230, 90, 80, 24))
	   ],
	 behaviour     :=

	   [
	     Increment := [
			 message := message(@prolog,
					    increment,
					    Increment,
					    Decrement,
					    Input_field )
		       ],
	     Decrement := [
			 message := message(@prolog,
					    decrement,
					    Increment,
					    Decrement,
					    Input_field)
			  ],
	     Input_field := [
			 message := message(@prolog,
					    input,
					    Increment,
					    Decrement,
					    @receiver,
					    @arg1)
			  ]
	   ]

       ]).

gui_component :-
	make_dialog(S, 'GUI_Interaction'),
	send(S, open).


increment(Incr, Decr, Input) :-
	get(Input, selection, V),
	atom_number(V, Val),
	Val1 is Val + 1,
	send(Input, selection, Val1),
	test(Val1, Incr, Decr, Input).

decrement(Incr, Decr, Input) :-
	get(Input, selection, V),
	atom_number(V, Val),
	Val1 is Val - 1,
	send(Input, selection, Val1),
	test(Val1, Incr, Decr, Input).



input(Incr, Decr, Input, Selection) :-
	catch( (term_to_atom(T, Selection), number(T)),
	       _,
	       (   send(@display, inform, 'Please type a number !'),
		   T = 0,
		   send(Input,selection, T))),
	test(T, Incr, Decr, Input).


test(V, Incr, Decr, Input) :-
	(   V = 0 -> send(Input, active, @on); send(Input, active, @off)),
	send(Incr, active, @on),
	send(Decr, active, @on),
	(   V < 1
	->  send(Decr, active, @off)
	;   V > 9
	->  send(Incr, active, @off)).


```



## PureBasic


```PureBasic
Enumeration
  #TextGadget
  #AddButton
  #SubButton
EndEnumeration

Procedure UpdateGadgets(Value,UpdateValue=0)
  Overmax=0: UnderMin=0
  If Value>=10
    Overmax=1
  ElseIf Value<=0
    UnderMin=1
  EndIf
  DisableGadget(#AddButton,Overmax)
  DisableGadget(#SubButton,UnderMin)
  If UpdateValue
    SetGadgetText(#TextGadget,Str(Value))
  EndIf
EndProcedure

If OpenWindow(0,#PB_Ignore,#PB_Ignore,110,70,"PB-GUI",#PB_Window_SystemMenu)
  StringGadget(#TextGadget,10,10,90,20,"")
  ButtonGadget(#AddButton,10,40,30,20,"+")
  ButtonGadget(#SubButton,70,40,30,20,"-")
  UpdateGadgets(Value,1)
  Repeat
    Event=WaitWindowEvent()
    If Event=#PB_Event_Gadget
      Gadget=EventGadget()
      Select Gadget
        Case #AddButton
          Value+1
          UpdateGadgets(Value,1)
        Case #SubButton
          Value-1
          UpdateGadgets(Value,1)
        Default
          EType=EventType()
          If EType=#PB_EventType_Change
            Value=Val(GetGadgetText(#TextGadget))
            UpdateGadgets(Value)
          EndIf
      EndSelect
    EndIf
    Until Event=#PB_Event_CloseWindow
EndIf
```



## R


```R

library(gWidgets)
options(guiToolkit="RGtk2") ## using gWidgtsRGtk2

w <- gwindow("Disable components")

g <- ggroup(cont=w, horizontal=FALSE)
e <- gedit("0", cont=g, coerce.with=as.numeric)
bg <- ggroup(cont=g)

down_btn <- gbutton("-", cont=bg)
up_btn <- gbutton("+", cont=bg)

update_ctrls <- function(h,...) {
  val <- svalue(e)
  enabled(down_btn) <- val >= 0
  enabled(up_btn) <- val <= 10
}

rement <- function(h,...) {
  svalue(e) <- svalue(e) + h$action
  update_ctrls(h,...)
}

addHandlerChanged(e, handler=update_ctrls)
addHandlerChanged(down_btn, handler=rement, action=-1)
addHandlerChanged(up_btn, handler=rement, action=1)

```



## Python


```Python

#!/usr/bin/env python3

import tkinter as tk

class MyForm(tk.Frame):

    def __init__(self, master=None):
        tk.Frame.__init__(self, master)
        self.pack(expand=True, fill="both", padx=10, pady=10)
        self.master.title("Controls")
        self.setupUI()

    def setupUI(self):
        self.value_entry = tk.Entry(self, justify=tk.CENTER)
        self.value_entry.grid(row=0, column=0, columnspan=2,
                              padx=5, pady=5, sticky="nesw")
        self.value_entry.insert('end', '0')
        self.value_entry.bind("<KeyPress-Return>", self.eventHandler)

        self.decre_btn = tk.Button(self, text="Decrement", state=tk.DISABLED)
        self.decre_btn.grid(row=1, column=0, padx=5, pady=5)
        self.decre_btn.bind("<Button-1>", self.eventHandler)

        self.incre_btn = tk.Button(self, text="Increment")
        self.incre_btn.grid(row=1, column=1, padx=5, pady=5)
        self.incre_btn.bind("<Button-1>", self.eventHandler)

    def eventHandler(self, event):
        value = int(self.value_entry.get())
        if event.widget == self.value_entry:
            if value > 10:
                self.value_entry.delete("0", "end")
                self.value_entry.insert("end", "0")
            elif value == 10:
                self.value_entry.config(state=tk.DISABLED)
                self.incre_btn.config(state=tk.DISABLED)
                self.decre_btn.config(state=tk.NORMAL)
            elif value == 0:
                self.value_entry.config(state=tk.NORMAL)
                self.incre_btn.config(state=tk.NORMAL)
                self.decre_btn.config(state=tk.DISABLED)
            elif (value > 0) and (value < 10):
                self.value_entry.config(state=tk.DISABLED)
                self.incre_btn.config(state=tk.NORMAL)
                self.decre_btn.config(state=tk.NORMAL)
        else:
            if event.widget == self.incre_btn:
                if (value >= 0) and (value < 10):
                    value += 1
                    self.value_entry.config(state=tk.NORMAL)
                    self.value_entry.delete("0", "end")
                    self.value_entry.insert("end", str(value))
                if value > 0:
                    self.decre_btn.config(state=tk.NORMAL)
                    self.value_entry.config(state=tk.DISABLED)
                if value == 10:
                    self.incre_btn.config(state=tk.DISABLED)
            elif event.widget == self.decre_btn:
                if (value > 0) and (value <= 10):
                    value -= 1
                    self.value_entry.config(state=tk.NORMAL)
                    self.value_entry.delete("0", "end")
                    self.value_entry.insert("end", str(value))
                    self.value_entry.config(state=tk.DISABLED)
                if (value) < 10:
                    self.incre_btn.config(state=tk.NORMAL)
                if (value) == 0:
                    self.decre_btn.config(state=tk.DISABLED)
                    self.value_entry.config(state=tk.NORMAL)

def main():
    app = MyForm()
    app.mainloop()

if __name__ == "__main__":
    main()

```


Result: [https://ibb.co/cEmqy5]


## Racket


```racket

#lang racket/gui

(define frame (new frame% [label "Interaction Demo"]))

(define (changed . _)
  (define s (send inp get-value))
  (define v (string->number s))
  (unless v (set! v (or (string->number (regexp-replace* #rx"[^0-9]+" s "")) 0))
            (send inp set-value (~a v)))
  (send inc-b enable (< v 10))
  (send dec-b enable (> v 0))
  (send inp enable (zero? v)))
(define ((change-value f) . _)
  (send inp set-value (number->string (f (string->number (send inp get-value)))))
  (changed))

(define inp
  (new text-field% [label "Value"] [parent frame] [init-value "0"] [callback changed]))

(define buttons (new horizontal-pane% [parent frame]))
(define inc-b
  (new button% [parent buttons] [label "Increment"] [callback (change-value add1)]))
(define dec-b
  (new button% [parent buttons] [label "Decrement"] [callback (change-value sub1)]))

(send frame show #t)

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
                      setGeometry(50,200,100,30)
                      settext("Increment")
                      setclickevent("Increment()")}

               btn2 = new qpushbutton(win1) {
                      setGeometry(200,200,100,30)
                      settext("Decrement")
                      setclickevent("Decrement()")}

               lineedit1 = new qlineedit(win1) {
                           setGeometry(10,100,350,30)
                           settext("0")}
               show()}
exec()}

func Increment
     lineedit1{ num = text()}
     num = string(number(num)+1)
     lineedit1.settext(num)
     if number(num)>9 btn1.setDisabled(True) ok

func Decrement
     lineedit1{ num = text()}
     num = string(number(num)-1)
     lineedit1.settext(num)
     if number(num)<0 btn2.setDisabled(True) ok

```

Output:
[[File:CalmoSoftGui.jpg]]


## Ruby

{{libheader|Shoes}}

```ruby
Shoes.app do
  @number = edit_line
  @number.change {update_controls}

  @incr = button('Increment') {update_controls(@number.text.to_i + 1)}
  @decr = button('Decrement') {update_controls(@number.text.to_i - 1)}

  def update_controls(value = @number.text.to_i)
    @number.text = value
    @incr.state = value.to_i >= 10 ? "disabled" : nil
    @decr.state = value.to_i <=  0 ? "disabled" : nil
  end

  update_controls 0
end
```



## Scala


```Scala
import swing.{ BoxPanel, Button, GridPanel, Orientation, Swing, TextField }
import swing.event.{ ButtonClicked, Key, KeyPressed, KeyTyped }

object Enabling extends swing.SimpleSwingApplication {
  def top = new swing.MainFrame {
    title = "Rosetta Code >>> Task: GUI enabling/disabling of controls | Language: Scala"

    val numberField = new TextField {
      text = "0" // start at 0
      horizontalAlignment = swing.Alignment.Right
    }

    val (incButton, decButton) = (new Button("Increment"), // Two variables initialized
      new Button("Decrement") { enabled = false })

    // arrange buttons in a grid with 1 row, 2 columns
    val buttonPanel = new GridPanel(1, 2) { contents ++= List(incButton, decButton) }

    // arrange text field and button panel in a grid with 2 row, 1 column
    contents = new BoxPanel(Orientation.Vertical) { contents ++= List(numberField, buttonPanel) }

    val specialKeys = List(Key.BackSpace, Key.Delete)

    // listen for keys pressed in numberField and button clicks
    listenTo(numberField.keys, incButton, decButton)
    reactions += {
      case kt: KeyTyped =>
        if (kt.char.isDigit) // if the entered char is a digit ...
          Swing.onEDT(switching) // ensure GUI-updating
        else kt.consume // ... eat the event (i.e. stop it from being processed)
      case KeyPressed(_, kp, _, _) if (!specialKeys.contains(kp)) =>
        Swing.onEDT(switching) // ensure GUI-updating
      case ButtonClicked(`incButton`) =>
        numberField.text = (numberField.text.toLong + 1).toString
        switching
      case ButtonClicked(`decButton`) =>
        numberField.text = (numberField.text.toLong - 1).toString
        switching
    }

    def switching = {
      val n = (if (numberField.text.isEmpty()) "0" else numberField.text).toLong
      numberField.text = n.toString
      numberField.enabled = n == 0
      incButton.enabled = n < 10
      decButton.enabled = n > 0
    }
    centerOnScreen()
  } // def top(
}
```



## Smalltalk

{{works with|Smalltalk/X}}

```smalltalk
|top input vh incButton decButton|

vh := ValueHolder with:0.

top := StandardSystemView label:'Rosetta GUI interaction'.
top extent:300@100.
top add:((Label label:'Value:') origin: 0 @ 10 corner: 100 @ 40).
top add:(input := EditField origin: 102 @ 10 corner: 1.0 @ 40).
input model:(TypeConverter onNumberValue:vh).
input enableChannel:(BlockValue with:[:v | v = 0] argument:vh).

top add:((incButton := Button label:'Inc') origin: 10 @ 50 corner: 100 @ 80).
top add:((decButton := Button label:'Dec') origin: 110 @ 50 corner: 210 @ 80).

incButton action:[ vh value: (vh value + 1) ].
incButton enableChannel:(BlockValue with:[:v | v < 10] argument:vh).
decButton action:[ vh value: (vh value - 1) ].
decButton enableChannel:(BlockValue with:[:v | v > 0] argument:vh).

top open
```



## Tcl

{{libheader|Tk}}

```tcl
package require Tk

# Model
set field 0

# View
place [ttk::frame .bg] -relwidth 1 -relheight 1; # Hack to make things look nice
pack [ttk::labelframe .val -text "Value"]
pack [ttk::entry .val.ue -textvariable field \
	-validate key -invalidcommand bell \
	-validatecommand {string is integer %P}]
pack [ttk::button .inc -text "increment" -command up]
pack [ttk::button .dec -text "decrement" -command down]

# Controller
proc up {} {
    global field
    incr field
}
proc down {} {
    global field
    incr field -1
}
# Attach this controller to the Model; easier than manual calling
trace add variable field write updateEnables
proc updateEnables {args} {
    global field
    .inc state [expr {$field < 10 ? "!disabled" : "disabled"}]
    .dec state [expr {$field > 0 ? "!disabled" : "disabled"}]
}
updateEnables; # Force initial state of buttons
```



## Vala

{{libheader|Gtk+-3.0}}


```vala
bool validate_input(Gtk.Window window, string str){
    int64 val;
    bool ret = int64.try_parse(str,out val);;

    if(!ret || str == ""){
        var dialog = new Gtk.MessageDialog(window,
                                           Gtk.DialogFlags.MODAL,
                                           Gtk.MessageType.ERROR,
                                           Gtk.ButtonsType.OK,
                                           "Invalid value");
        dialog.run();
        dialog.destroy();
    }
    if(str == ""){
        ret = false;
    }

    return ret;
}

int main (string[] args) {
    Gtk.init (ref args);

    var window = new Gtk.Window();
    window.title = "Rosetta Code";
    window.window_position = Gtk.WindowPosition.CENTER;
    window.destroy.connect(Gtk.main_quit);

    var box = new Gtk.Box(Gtk.Orientation.HORIZONTAL, 1);
    box.set_border_width(1);

    var label = new Gtk.Label("Value:");
    var entry = new Gtk.Entry();
    var ib = new Gtk.Button.with_label("inc");
    var db = new Gtk.Button.with_label("dec");

    ib.set_sensitive(true);  //enable the inc button
    db.set_sensitive(false); //disable the dec button

    entry.set_text("0");   //initialize to zero

    entry.activate.connect(() => {
        var str = entry.get_text();

        if(!validate_input(window, str)){
            entry.set_text("0");   //initialize to zero on wrong input
        }else{
            int num = int.parse(str);
            if(num != 0){
                entry.set_sensitive(false); //disable the field
            }
            if(num > 0 && num < 10){
                ib.set_sensitive(true);   //enable the inc button
                db.set_sensitive(true);   //enable the dec button
            }
            if(num > 9){
                ib.set_sensitive(false);  //disable the inc button
                db.set_sensitive(true);   //enable the dec button
            }
        }
    });

    ib.clicked.connect(() => {
        // read and validate the entered value
        if(!validate_input(window, entry.get_text())){
            entry.set_text("0");   //initialize to zero on wrong input
        }else{
            int num = int.parse(entry.get_text()) + 1;
            entry.set_text(num.to_string());
            if(num > 9){
                ib.set_sensitive(false); //disable the inc button
            }
            if(num != 0){
                db.set_sensitive(true); //enable the dec button
                entry.set_sensitive(false); //disable the field
            }
            if(num == 0){
                entry.set_sensitive(true); //enable the field
            }
            if(num < 0){
                db.set_sensitive(false);   //disable the dec button
            }
        }
    });

    db.clicked.connect(() => {
        // read and validate the entered value
        int num = int.parse(entry.get_text()) - 1;
        entry.set_text(num.to_string());
        if(num == 0){
            db.set_sensitive(false); //disable the dec button
            entry.set_sensitive(true); //enable the field
        }
        if(num < 10){
            ib.set_sensitive(true); //enable the inc button
        }
    });

    box.pack_start(label, false, false, 2);
    box.pack_start(entry, false, false, 2);
    box.pack_start(ib, false, false, 2);
    box.pack_start(db, false, false, 2);

    window.add(box);

    window.show_all();

    Gtk.main();
    return 0;
}

```



## Visual Basic

In VB, windows are usually created in the IDE.
The generated code is hidden from the user unless viewed outside of VB.
For the sake of this task, I have included that code.

(Also, this sort of task would typically be performed
by a "spinner" or "up-down" control, not by buttons.)


```vb
VERSION 5.00
Begin VB.Form Form1
   Caption         =   "Form1"
   ClientHeight    =   2265
   ClientLeft      =   60
   ClientTop       =   600
   ClientWidth     =   2175
   LinkTopic       =   "Form1"
   ScaleHeight     =   2265
   ScaleWidth      =   2175
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cmdDec
      Caption         =   "Decrement"
      Enabled         =   0   'False
      Height          =   495
      Left            =   120
      TabIndex        =   2
      Top             =   1680
      Width           =   1215
   End
   Begin VB.CommandButton cmdInc
      Caption         =   "Increment"
      Height          =   495
      Left            =   120
      TabIndex        =   1
      Top             =   1080
      Width           =   1215
   End
   Begin VB.TextBox txtValue
      Height          =   495
      Left            =   120
      TabIndex        =   0
      Text            =   "0"
      Top             =   240
      Width           =   1215
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'-----user-written code begins here; everything above this line is hidden in the GUI-----
Private Sub cmdDec_Click()
    If Val(txtValue.Text) > 0 Then txtValue.Text = Val(txtValue.Text) - 1
End Sub

Private Sub cmdInc_Click()
    If Val(txtValue.Text) < 10 Then txtValue.Text = Val(txtValue.Text) + 1
End Sub

Private Sub txtValue_Change()
    Select Case Val(txtValue.Text)
        Case Is < 0
            txtValue.Enabled = False
            cmdInc.Enabled = True
            cmdDec.Enabled = False
        Case Is > 9
            txtValue.Enabled = False
            cmdInc.Enabled = False
            cmdDec.Enabled = True
        Case 0
            txtValue.Enabled = True
            cmdInc.Enabled = True
            cmdDec.Enabled = False
        Case Else
            txtValue.Enabled = False
            cmdInc.Enabled = True
            cmdDec.Enabled = True
    End Select
End Sub
```

