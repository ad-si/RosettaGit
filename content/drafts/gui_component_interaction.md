+++
title = "GUI component interaction"
description = ""
date = 2019-10-16T08:23:28Z
aliases = []
[extra]
id = 8058
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
{{omit from|GUISS|Only makes use of installed applications}}
{{omit from|Integer BASIC|no concept of a GUI}}
{{omit from|Lilypond}}
{{omit from|Logtalk}}
{{omit from|Lotus 123}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|PostScript}}
{{omit from|Retro}}
{{omit from|SQL PL|It does not handle GUI}}


Almost every application needs to communicate with the user in some way.
Therefore, a substantial part of the code deals with the interaction
of program logic with GUI components.

Typically, the following is needed:

* put values into input fields under program control
* read and check input from the user
* pop up dialogs to query the user for further information


;Task:
For a minimal "application", write a program that presents a form with three components to the user:
::* a numeric input field ("Value")
::* a button ("increment")
::* a button ("random")


The field is initialized to zero.

The user may manually enter a new value into the field,
or increment its value with the "increment" button.

Entering a non-numeric value should be either impossible,
or issue an error message.

Pressing the "random" button presents a confirmation dialog,
and resets the field's value to a random value if the answer is "Yes".

(This task may be regarded as an extension of the task [[Simple windowed application]]).




## 1C

<lang>
&НаСервере
Процедура ДобавитьЭлементы()

	КЧ = Новый КвалификаторыЧисла(12,2);

	Массив = Новый Массив;
	Массив.Добавить(Тип("Число"));
	ОписаниеТиповЧ = Новый ОписаниеТипов(Массив, , ,КЧ);

	НовыйРеквизит = Новый РеквизитФормы("ВводимоеЧисло", Новый ОписаниеТипов(Массив, , ,КЧ));;

	МассивР = Новый Массив;
	МассивР.Добавить(НовыйРеквизит);
	ИзменитьРеквизиты(МассивР);

	ПолеВвода = Элементы.Добавить("ПолеВвода", Тип("ПолеФормы"));
	ПолеВвода.ПутьКДанным = "ВводимоеЧисло";
	ПолеВвода.вид = ВидПоляФормы.ПолеВвода;

	КомандаИнкримент = Команды.Добавить("Инкримент");
	КомандаРандом 	 = КОманды.Добавить("Рандом");

	КнопкаИнкримент = Элементы.Добавить("КнопкаИнкримент", Тип("КнопкаФормы"));
	КнопкаИнкримент.ИмяКоманды = "Инкримент";
	КнопкаРандом = Элементы.Добавить("КнопкаРандом", Тип("КнопкаФормы"));
	КнопкаРандом.ИмяКоманды = "Рандом";


	КомандаИнкримент.Действие = "Инкримент";
	КомандаРандом.Действие = "Рандом";

КонецПроцедуры

&НаКлиенте
Процедура Инкримент(Команда)

	ЭтотОбъект.ВводимоеЧисло = ЭтотОбъект.ВводимоеЧисло + 1;

КонецПроцедуры

&НаКлиенте
Процедура Рандом(Команда)

	ОписаниеОповещения = Новый ОписаниеОповещения("РандомПослеВыбора", ЭтотОбъект);

	ПоказатьВопрос(ОписаниеОповещения, "Установить случайное число?", РежимДиалогаВопрос.ДаНет);

КонецПроцедуры

&НаКлиенте
Процедура РандомПослеВыбора(РезультатВопроса, ДополнительныеПараметры) Экспорт

	Если РезультатВопроса = КодВозвратаДиалога.Да Тогда
		ГСЧ = Новый ГенераторСлучайныхЧисел();
		ЭтотОбъект.ВводимоеЧисло = ГСЧ.СлучайноеЧисло(0, 999999);
	КонецЕсли;

КонецПроцедуры

```


## Ada

{{libheader|GtkAda}}

interaction.adb:

```Ada
with Ada.Numerics.Discrete_Random;
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
with Gtkada.Dialogs;

procedure Interaction is

   The_Value : Natural := 0;

   package Natural_Random is new Ada.Numerics.Discrete_Random (Natural);
   RNG : Natural_Random.Generator;

   Main_Window      : Gtk.Window.Gtk_Window;
   Content          : Gtk.Box.Gtk_Vbox;
   Increment_Button : Gtk.Button.Gtk_Button;
   Random_Button    : Gtk.Button.Gtk_Button;
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
             (Source => Natural'Image (The_Value),
              Side   => Ada.Strings.Both));
   end Update_Entry;

   --  read from text entry
   procedure Update_Value is
   begin
      The_Value := Natural'Value (Gtk.GEntry.Get_Text (Entry_Field));
   exception
      when Constraint_Error =>
         The_Value := 0;
   end Update_Value;

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
         Number : Natural;
      begin
         Number := Natural'Value (Text);
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
   begin
      Update_Value;
      The_Value := The_Value + 1;
      Update_Entry;
   end On_Increment_Click;

   --  Callback for click event
   procedure On_Random_Click
     (Object : access Gtk.Button.Gtk_Button_Record'Class)
   is
      use type Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if Gtkada.Dialogs.Message_Dialog
        (Msg            => "Really reset to random value?",
         Dialog_Type    => Gtkada.Dialogs.Confirmation,
         Buttons        => Gtkada.Dialogs.Button_Yes or
           Gtkada.Dialogs.Button_No,
         Default_Button => Gtkada.Dialogs.Button_Yes) =
        Gtkada.Dialogs.Button_Yes
      then
         The_Value := Natural_Random.Random (RNG);
         Update_Entry;
      end if;
   end On_Random_Click;

   --  Callback for delete event
   function On_Main_Window_Delete
     (Object : access Gtk.Window.Gtk_Window_Record'Class)
      return   Boolean
   is
   begin
      Gtk.Main.Main_Quit;
      return True;
   end On_Main_Window_Delete;

begin
   --  initialize random number generator
   Natural_Random.Reset (RNG);

   Gtk.Main.Init;

   Gtk.GEntry.Gtk_New (Widget => Entry_Field);
   Update_Entry;
   Entry_Callbacks.Connect
     (Widget => Entry_Field,
      Name   => Gtk.Editable.Signal_Insert_Text,
      Cb     => On_Insert_Text'Access);

   Gtk.Button.Gtk_New (Button => Increment_Button, Label => "Increment");
   Gtk.Button.Gtk_New (Button => Random_Button, Label => "Random");

   Button_Callbacks.Connect
     (Widget => Increment_Button,
      Name   => Gtk.Button.Signal_Clicked,
      Marsh  => Button_Callbacks.To_Marshaller (On_Increment_Click'Access));
   Button_Callbacks.Connect
     (Widget => Random_Button,
      Name   => Gtk.Button.Signal_Clicked,
      Marsh  => Button_Callbacks.To_Marshaller (On_Random_Click'Access));

   Gtk.Box.Gtk_New_Vbox (Box => Content);
   Gtk.Box.Add (Container => Content, Widget => Entry_Field);
   Gtk.Box.Add (Container => Content, Widget => Increment_Button);
   Gtk.Box.Add (Container => Content, Widget => Random_Button);

   Gtk.Window.Gtk_New (Window => Main_Window);
   Gtk.Window.Add (Container => Main_Window, Widget => Content);

   Window_Callbacks.Connect
     (Widget => Main_Window,
      Name   => Gtk.Widget.Signal_Delete_Event,
      Cb     => On_Main_Window_Delete'Access);
   Gtk.Window.Show_All (Widget => Main_Window);

   Gtk.Main.Main;
end Interaction;
```



## AutoHotkey


```AutoHotkey
GUI, add, Edit,Number w50 vUserInput gMakeSure, 0          ; Number Specifies Numbers-only, but other characters can still be pasted in,
						 ; Making our own check necessary. (MakeSure)

GUI, add, Button, gIncrement, Increment 	 ; Instead of an increment button, the UpDown control could be used, but this was not specified.
GUI, add, Button, gRando, Random
Gui, Show, W200 y200, Title			 ; Shows the GUI with a width and height of 200px
return 						 ; End Auto-Execute Section


Increment:
Gui, Submit, NoHide
; The above line assigns all variables associated with controls to the state of that control, but leaves the GUI visible.
If UserInput is not Number
{
	MsgBox, %userInput% is not a number.
	GUIControl,,UserInput, 0 ; Reset the Edit control to 0
}
Else
{
	UserInput++
	GUIControl,, UserInput, %UserInput% ; Sets the value of the Edit control
}
return



Rando:
MsgBox, 4, Title, Are you sure you want to randomize? ; Specify your own title. 4 means YesNo
IfMsgBox, Yes
{
	Random, UserInput, 1, 999		      ; random number from 1-999
	GUIControl,, UserInput, %UserInput%	      ; Sets the value of the Edit control
}
return



MakeSure:
Gui, Submit, NoHide
If UserInput is not Number
{
	If (UserInput<>"")
	{
	Msgbox Error! Numbers Only!
	GUIControl,, UserInput, 0
	}
}
return



GUIClose:
ExitApp  ; Makes sure the script exits when the window is closed,
	 ; Otherwise the script is persistent because it contains
	 ; a timer.
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"WINLIB2"
      INSTALL @lib$+"WINLIB5"

      IDYES = 6
      ES_NUMBER = 8192
      MB_YESNO = 4

      form% = FN_newdialog("Rosetta Code", 100, 100, 100, 52, 8, 1000)
      PROC_static(form%, "Value:", 100, 10, 10, 24, 14, 0)
      PROC_editbox(form%, "0", 101, 40, 8, 52, 14, ES_NUMBER)
      PROC_pushbutton(form%, "Increment", FN_setproc(PROCinc), 7, 30, 40, 16, 0)
      PROC_pushbutton(form%, "Random", FN_setproc(PROCrandom), 52, 30, 40, 16, 0)
      PROC_showdialog(form%)

      REPEAT
        WAIT 1
      UNTIL !form% = 0
      QUIT

      DEF PROCinc
      LOCAL number%
      SYS "GetDlgItemInt", !form%, 101, 0, 1 TO number%
      SYS "SetDlgItemInt", !form%, 101, number% + 1, 1
      ENDPROC

      DEF PROCrandom
      LOCAL reply%
      SYS "MessageBox", !form%, "Set to a random value?", "Confirm", MB_YESNO TO reply%
      IF reply% = IDYES THEN SYS "SetDlgItemInt", !form%, 101, RND(10000), 1
      ENDPROC
```

{{Out}}
<p>[[File:Guiintbbc.gif]]</p>


## C

{{works with|C (with Win32 API)}}


```txt
file main.c
```



```c
#include <windows.h>
#include "resource.h"

BOOL CALLBACK DlgProc( HWND hwnd, UINT msg, WPARAM wPar, LPARAM lPar ) {
    switch( msg ) {

        case WM_INITDIALOG:
            srand( GetTickCount() );
            SetDlgItemInt( hwnd, IDC_INPUT, 0, FALSE );
            break;

        case WM_COMMAND:
            switch( LOWORD(wPar) ) {
                case IDC_INCREMENT: {
                    UINT n = GetDlgItemInt( hwnd, IDC_INPUT, NULL, FALSE );
                    SetDlgItemInt( hwnd, IDC_INPUT, ++n, FALSE );
                    } break;
                case IDC_RANDOM: {
                    int reply = MessageBox( hwnd,
                        "Do you really want to\nget a random number?",
                        "Random input confirmation", MB_ICONQUESTION|MB_YESNO );
                    if( reply == IDYES )
                        SetDlgItemInt( hwnd, IDC_INPUT, rand(), FALSE );
                    } break;
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

int WINAPI WinMain( HINSTANCE hInst, HINSTANCE hPInst, LPSTR cmdLn, int show ) {
    return DialogBox( hInst, MAKEINTRESOURCE(IDD_DLG), NULL, DlgProc );
}
```



```txt
file resource.h
```



```c
#define IDD_DLG          101
#define IDC_INPUT       1001
#define IDC_INCREMENT   1002
#define IDC_RANDOM      1003
#define IDC_QUIT        1004
```



```txt
file resource.rc
```



```c
#include <windows.h>
#include "resource.h"

LANGUAGE LANG_NEUTRAL, SUBLANG_NEUTRAL
IDD_DLG DIALOG 0, 0, 154, 46
STYLE DS_3DLOOK | DS_CENTER | DS_MODALFRAME | DS_SHELLFONT | WS_CAPTION |
WS_VISIBLE | WS_POPUP | WS_SYSMENU
CAPTION "GUI Component Interaction"
FONT 12, "Ms Shell Dlg" {
    EDITTEXT        IDC_INPUT, 7, 7, 140, 12, ES_AUTOHSCROLL | ES_NUMBER
    PUSHBUTTON      "Increment", IDC_INCREMENT, 7, 25, 50, 14
    PUSHBUTTON      "Random", IDC_RANDOM, 62, 25, 50, 14
    PUSHBUTTON      "Quit", IDC_QUIT, 117, 25, 30, 14
}
```



## C++

with library Qt 4.4 , using (under Linux) first qmake -project and then qmake -o Makefile <projectfile>, then make

```txt
file interaction.h
```


```cpp
#ifndef INTERACTION_H
#define INTERACTION_H
#include <QWidget>

class QPushButton ;
class QLineEdit ;
class QVBoxLayout ;
class MyWidget : public QWidget {
   Q_OBJECT

public :
   MyWidget( QWidget *parent = 0 ) ;
private :
   QLineEdit *entryField ;
   QPushButton *increaseButton ;
   QPushButton *randomButton ;
   QVBoxLayout *myLayout ;
private slots :
   void doIncrement( ) ;
   void findRandomNumber( ) ;
} ;
#endif
```


```txt
file interaction.cpp
```


```cpp
#include <QPushButton>
#include <QLineEdit>
#include <QMessageBox>
#include <QString>
#include <QRegExpValidator>
#include <QVBoxLayout>
#include <QRegExp>
#include <ctime> //for the srand initialization
#include <cstdlib> //for the random number
#include "interaction.h"

MyWidget::MyWidget (QWidget *parent ) : QWidget( parent ) {
   myLayout = new QVBoxLayout( ) ;
   entryField = new QLineEdit( "0" ) ;
   QRegExp rx( "\\d+" ) ;
   QValidator *myvalidator = new QRegExpValidator( rx , this ) ;
   entryField->setValidator( myvalidator ) ;
   increaseButton = new QPushButton( "increase" ) ;
   connect( increaseButton, SIGNAL( clicked( ) ) ,
	 this , SLOT( doIncrement( ) )) ;
   randomButton = new QPushButton( "random" ) ;
   connect( randomButton , SIGNAL( clicked( ) ) ,
	 this , SLOT ( findRandomNumber( ) )) ;
   myLayout->addWidget( entryField ) ;
   myLayout->addWidget( increaseButton ) ;
   myLayout->addWidget( randomButton ) ;
   setLayout( myLayout ) ;
}

void MyWidget::doIncrement( ) {
   bool ok ;
   int zahl = entryField->text( ).toInt( &ok, 10 ) ;
   entryField->setText( QString( "%1").arg( ++zahl ) ) ;
}

void MyWidget::findRandomNumber( ) {
   QMessageBox msgBox( this ) ;
   msgBox.setText( "Do you want to create a random number ?" ) ;
   msgBox.setStandardButtons( QMessageBox::Yes | QMessageBox::No ) ;
   int ret = msgBox.exec( ) ;
   switch ( ret ) {
      case QMessageBox::Yes :
	 srand( time( 0 ) ) ;
	 int zahl = random( ) ;
	 entryField->setText( QString( "%1" ).arg( zahl )) ;
	 break ;
   }
}
```


```txt
file main.cpp
```


```cpp
#include <QApplication>
#include "interaction.h"

int main( int argc , char *argv[ ] ) {
   QApplication app( argc, argv ) ;
   MyWidget theWidget ;
   theWidget.show( ) ;
   return app.exec( ) ;
}
```



### C++11

with library Qt 5.4, using (under Linux) first run qmake -project, add CONFIG+=c++11 and QT+=widgets to the resulting .pro file, and then run make.


```txt
main.cpp
```



```cpp
#include <QApplication>
#include <QWidget>
#include <QVBoxLayout>
#include <QPushButton>
#include <QLineEdit>
#include <QIntValidator>
#include <QMessageBox>
#include <QTime>

int main(int argc, char **argv) {
    qsrand(QTime::currentTime().msec());

    QApplication app(argc, argv);

    auto *edit = new QLineEdit("0");
    edit->setValidator(new QIntValidator());

    auto *incButton = new QPushButton("&Increment");
    QObject::connect(incButton, &QPushButton::clicked,
            [edit]() { edit->setText( QString::number(edit->text().toInt() + 1)); } );

    auto *rndButton = new QPushButton("&Random");
    QObject::connect(rndButton, &QPushButton::clicked,
            [edit]() {
                auto result = QMessageBox(
                    QMessageBox::Warning,
                    "Random",
                    "Overwrite current value with a random number ?",
                    QMessageBox::Ok | QMessageBox::Cancel
                ).exec();

                if (result == QMessageBox::Ok)
                    edit->setText( QString::number(qrand()));
            } );

    auto *vbox = new QVBoxLayout;
    vbox->addWidget(edit);
    vbox->addWidget(incButton);
    vbox->addWidget(rndButton);

    QWidget mainWindow;
    mainWindow.setLayout(vbox);
    mainWindow.show();

    return app.exec();
}
```


=={{header|C_sharp|C#}}==
C# 3.0 with Windows Forms; compile as csc -t:winexe Program.cs on MS.NET or as gmcs -t:winexe Program.cs on Mono.

```c#
using System;
using System.ComponentModel;
using System.Windows.Forms;

class RosettaInteractionForm : Form
{
    // Model used for DataBinding.
    // Notifies bound controls about Value changes.
    class NumberModel: INotifyPropertyChanged
    {

        Random rnd = new Random();

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

        public void ResetToRandom(){
            Value = rnd.Next(5000);
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

        var btIncrement = new Button{Text = "Increment", Dock = DockStyle.Bottom};
        btIncrement.Click += delegate
                        {
                            model.Value++;
                        };
        var btDecrement = new Button{Text = "Decrement", Dock = DockStyle.Bottom};
        btDecrement.Click += delegate
                        {
                            model.Value--;
                        };
        var btRandom = new Button{ Text="Reset to Random", Dock = DockStyle.Bottom };
        btRandom.Click += delegate
                        {
                            if (MessageBox.Show("Are you sure?", "Are you sure?", MessageBoxButtons.YesNo) == DialogResult.Yes)
                                model.ResetToRandom();
                        };
        Controls.Add(tbNumber);
        Controls.Add(btIncrement);
        Controls.Add(btDecrement);
        Controls.Add(btRandom);
    }
    static void Main()
    {
        Application.Run(new RosettaInteractionForm());
    }
}

```


## Common Lisp

{{libheader|LTK}}

```lisp

;; Using the LTK library...

(defun gui-test ()
  "the main window for the input test"
  (ltk:with-ltk ()
    (ltk:wm-title ltk:*tk* "GUI Test")
    (ltk:bind ltk:*tk* "<Alt-q>" (lambda (evt)
				   (declare (ignore evt))
				   (setf ltk:*exit-mainloop* t)))
    (let* (;; Initializing random generator
	   (*random-state* (make-random-state t))
	   ;; Creating widgets
	   (the-input (make-instance 'ltk:entry
				     :text "0"
				     :validate :key))
	   (f (make-instance 'ltk:frame))
	   (btn1 (make-instance 'ltk:button :text "random" :master f))
	   (btn2 (make-instance 'ltk:button :text "increment" :master f)))
      ;; Associating actions with widgets
      (ltk:bind btn1 "<Button-1>"
		(lambda (evt)
		  (declare (ignore evt))
		  (when (ltk:ask-yesno "Really reset to random?" :title "Question")
		    (setf (ltk:text the-input) (write-to-string (random 10000))))))
      (ltk:bind btn2 "<Button-1>"
		(lambda (evt)
		  (declare (ignore evt))
		  (setf (ltk:text the-input)
			(write-to-string (1+ (parse-integer (ltk:text the-input)))))))
      (ltk:format-wish "~A configure -validatecommand {string is int %P}"
		       (ltk:widget-path the-input))
      (ltk:focus the-input)
      ;; Placing widgets on the window
      (ltk:pack the-input :side :top)
      (ltk:pack f :side :bottom)
      (ltk:pack btn1 :side :left)
      (ltk:pack btn2 :side :right))))

(gui-test)


```



## Delphi



```Delphi>FILE: Unit1.pas</lang


```Delphi

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    EditInputField: TEdit;
    ButtonRandom: TButton;
    ButtonIncrement: TButton;
    procedure EditInputFieldChange(Sender: TObject);
    procedure ButtonIncrementClick(Sender: TObject);
    procedure ButtonRandomClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.EditInputFieldChange(Sender: TObject);
Var
  Value: Integer;
begin
   if not TryStrToInt(EditInputField.Text, value) then
  begin
     ShowMessage('Error! The Input Value is not numeric!');
      EditInputField.Text := '0';
   end;
end;

procedure TForm1.ButtonIncrementClick(Sender: TObject);
begin
   EditInputField.text := IntToStr  (StrToInt(EditInputField.Text) + 1);
end;

procedure TForm1.ButtonRandomClick(Sender: TObject);
begin
   Randomize;
   EditInputField.Text := IntToStr(Random(High(Integer)));
end;

end.
```


```Delphi
FILE: Unit1.dfm (No manual interaction!!! Will automatically be generated/modified when editing the GUI)
```


```Delphi

object Form1: TForm1
  Left = 1899
  Top = 212
  Width = 266
  Height = 172
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object EditInputField: TEdit
    Left = 16
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '0'
    OnChange = EditInputFieldChange
  end
  object ButtonRandom: TButton
    Left = 96
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Random'
    TabOrder = 1
    OnClick = ButtonRandomClick
  end
  object ButtonIncrement: TButton
    Left = 16
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Increment'
    TabOrder = 2
    OnClick = ButtonIncrementClick
  end
end

```



## Echolisp


```scheme

(require 'interface)

;; helper new button with text
(define (ui-add-button text)
    (define b (ui-create-element "button" '((type "button"))))
    (ui-set-html b text)
    (ui-add b))


(define (panel )
    (ui-clear)
    (info-text "My rosetta application" "blue")

;; input field (checked for numeric)
    (define f-input (ui-create-element "input" '((type number))))
    (ui-set-value f-input 0)
    (ui-add f-input)
    (ui-on-focus  f-input (lambda(e) (info-text "")))

;; Increment button
    (define btn-inc (ui-add-button "Increment"))
    (define (increment elem)
        (define val (ui-get-numvalue f-input))
        (if val ;; checked legal numeric
                (ui-set-value f-input (1+ val))
                (info-text  "Need a number" "red")))
    (ui-on-click btn-inc increment)
    (ui-add btn-inc)

    (define btn-random (ui-add-button "Random"))
    (define (set-random elem)
        (when (confirm "Really random?")
              (ui-set-value f-input (random-prime 1000000))))
    (ui-on-click btn-random set-random)

    (ui-focus btn-inc)
    (stdout-hide #t)
    (stdin-hide #t)) ;; end panel

(panel)

```



## Elena

ELENA 4.x :

```elena
import forms;
import extensions;

public class MainWindow : SDIDialog
{
    Button btmIncrement;
    Button btmRandom;
    Edit   txtNumber;

    constructor new()
        <= new()
    {
        btmIncrement := new Button();
        btmRandom    := new Button();
        txtNumber    := new Edit();

        self
            .appendControl:btmIncrement
            .appendControl:btmRandom
            .appendControl:txtNumber;

        self.Caption := "Rosseta Code";
        self.setRegion(100, 100, 160, 120);

        txtNumber.setRegion(7, 7, 140, 25);
        txtNumber.Caption := "0";

        btmIncrement.setRegion(7, 35, 140, 25);
        btmIncrement.Caption := "Increment";
        btmIncrement.onClick := (args){ self.onButtonIncrementClick() };

        btmRandom.setRegion(7, 65, 140, 25);
        btmRandom.Caption := "Random";
        btmRandom.onClick := (args){ self.onButtonRandomClick() };
    }

    private onButtonIncrementClick()
    {
        var number := txtNumber.Value.toInt();

        number := number + 1;
        self.changeTextBoxValue(number)
    }

    private onButtonRandomClick()
    {
        if(messageDialog.showQuestion("Inf", "Really reset to random value?"))
        {
            self.changeTextBoxValue(randomGenerator.eval(99999999))
        }
    }

    private changeTextBoxValue(number)
    {
        txtNumber.Caption := number.toString()
    }
}
```



## Fantom


```fantom

using fwt
using gfx

class GuiComponent
{
  public static Void main ()
  {
    Window
    {
      title = "Rosetta Code: Gui component"
      size = Size(350, 200)
      textField := Text
      {
        onModify.add |Event e|
        {
          Text thisText := e.widget
          if (thisText.text != "") // if nonempty string
          {
            try (thisText.text.toInt) // try converting to int
            catch thisText.text = ""  // clear field if does not work
          }
        }
      }
      GridPane
      {
        numCols = 1
        textField,
        Button
        {
          text = "increment"
          onAction.add |Event e|
          { // make sure there is a number to increment, else set field to 0
            if (textField.text == "")
            {
              textField.text = "0"
            }
            else
            {
              try
              {
                Int x := textField.text.toInt
                textField.text = (x+1).toStr
              }
              catch
              {
                textField.text = "0"
              }
            }
          }
        },
        Button
        {
          text = "random"
          onAction.add |Event e|
          {
            if (Dialog.openQuestion(e.window, "Make number random?", null, Dialog.yesNo) == Dialog.yes)
            {
              textField.text = Int.random(1..10000).toStr
            }
          }
        },
      },
    }.open
  }
}

```



## FreeBASIC


```freebasic

#Include "windows.bi"

Dim As HWND Window_Main, Edit_Number, Button_Inc, Button_Rnd
Dim As MSG msg
Dim As Integer n
Dim As String text

'Create a window with an input field and two buttons:
Window_Main = CreateWindow("#32770", "GUI Component Interaction", WS_OVERLAPPEDWINDOW Or WS_VISIBLE, 100, 100, 250, 200, 0, 0, 0, 0)
Var Static_Number = CreateWindow("STATIC", "Value:", WS_VISIBLE Or WS_CHILD, 10, 10, 100, 20, Window_Main, 0, 0, 0)
Edit_Number = CreateWindow("EDIT", "0", WS_BORDER Or WS_VISIBLE Or WS_CHILD Or ES_AUTOHSCROLL Or ES_Number, 110, 10, 100, 20, Window_Main, 0, 0, 0)
Button_Inc = CreateWindow("BUTTON", "Increment", WS_VISIBLE Or WS_CHILD, 110, 40, 100, 20, Window_Main, 0, 0, 0)
Button_Rnd = CreateWindow("BUTTON", "Random", WS_VISIBLE Or WS_CHILD, 110, 70, 100, 20, Window_Main, 0, 0, 0)

'Windows message loop:
While GetMessage(@msg, Window_Main, 0, 0)
  TranslateMessage(@msg)
  DispatchMessage(@msg)
  Select Case msg.hwnd
    Case Button_Inc
      If msg.message = WM_LBUTTONDOWN Then
	'Increment value:
	text = Space(GetWindowTextLength(Edit_Number) + 1) 'Buffer for the text
	GetWindowText(Edit_Number, text, Len(text))
	n = Val(text)
	SetWindowText(Edit_Number, Str(n + 1))
      End If
    Case Button_Rnd
      If msg.message = WM_LBUTTONDOWN THEN
	'Random value (max. 100000):
	If MessageBox(0, "Set input field to random value?", "Please confirm", MB_ICONQUESTION Or MB_YESNO) = IDYES Then
	  n = 100000 * RND
	  SetWindowText(Edit_Number, Str(n))
	End If
      End If
    Case Window_Main
      If msg.message = WM_COMMAND Then End
  End Select
Wend

End

```



## Gambas


```gambas
hValueBox As ValueBox                                             'We need a ValueBox

Public Sub Form_Open()
Dim hButton As Button                                             'We need 2 Buttons

With Me                                                           'Set the Form's Properties..
  .height = 95                                                    'Set the Height
  .Width = 350                                                    'Set the Width
  .Arrangement = Arrange.Vertical                                 'Arrange items vertically
  .Padding = 5                                                    'Border area
  .Title = "GUI component interaction"                            'Title displayed on the Form
End With

hValueBox = New ValueBox(Me)                                      'Add a ValueBox to the Form

With hValueBox                                                    'Set the ValueBox's Properties..
  .Expand = True                                                  'Expand the ValueBox
  .Value = 0                                                      'Set it's value to 0
End With

hButton = New Button(Me) As "ButtonInc"                           'Add a Button to the form as Event "ButtonInc"

With hButton                                                      'Set the Button's Properties..
  .Height = 28                                                    'Set the Height
  .Text = "&Increment"                                            'Add Text (The '&' adds a keyboard shortcut)
End With

hButton = New Button(Me) As "ButtonRand"                          'Add a Button to the form as Event "ButtonRand"

With hButton                                                      'Set the Button's Properties..
  .Height = 28                                                    'Set the Height
  .Text = "&Random"                                               'Add Text (The '&' adds a keyboard shortcut)
End With

End

Public Sub ButtonInc_Click()                                      'When the 'Increment' Button is clicked..

hValueBox.Value += 1                                              'Increase the Value in the ValueBox by 1

End

Public Sub ButtonRand_Click()                                     'When the 'Random' Button is clicked..
Dim siRand As Short                                               'To store random number
Dim byAnswer As Byte                                              'To store the answer to the MessageBox question

siRand = Rand(1, 32767)                                           'Create a 'Random' mnumber

byAnswer = Message.Question("Would you like to set the ValueBox to " & siRand & "?", "Yes", "No") ' Ask if the number is OK
If byAnswer = 1 Then                                              'If the user says 'Yes' then
  hValueBox.Value = siRand                                        'Display the 'Random' number in the ValueBox
Else                                                              'ELSE
  hValueBox.Value = 0                                             'Set the ValueBox Value to 0
End If

End
```



## Go

{{libheader|gotk3}}
{{trans|Vala}}

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
    entry.Connect("activate", func() {
        // read and validate the entered value
        str, _ := entry.GetText()
        validateInput(window, str)
    })

    // button to increment
    ib, err := gtk.ButtonNewWithLabel("Increment")
    check(err, "Unable to create increment button:")
    ib.Connect("clicked", func() {
        // read and validate the entered value
        str, _ := entry.GetText()
        if i, ok := validateInput(window, str); ok {
            entry.SetText(strconv.FormatInt(i+1, 10))
        }
    })

    // button to put in a random value if confirmed
    rb, err := gtk.ButtonNewWithLabel("Random")
    check(err, "Unable to create random button:")
    rb.Connect("clicked", func() {
        dialog := gtk.MessageDialogNew(
            window,
            gtk.DIALOG_MODAL,
            gtk.MESSAGE_QUESTION,
            gtk.BUTTONS_YES_NO,
            "Set random value",
        )
        answer := dialog.Run()
        dialog.Destroy()
        if answer == gtk.RESPONSE_YES {
            entry.SetText(strconv.Itoa(rand.Intn(10000)))
        }
    })

    box.PackStart(label, false, false, 2)
    box.PackStart(entry, false, false, 2)
    box.PackStart(ib, false, false, 2)
    box.PackStart(rb, false, false, 2)
    window.Add(box)

    window.ShowAll()
    gtk.Main()
}
```



## Haskell



```Haskell
import Graphics.UI.WX
import System.Random

main :: IO ()
main = start $ do
    frm   <- frame [text := "Interact"]
    fld   <- textEntry frm [text := "0", on keyboard := checkKeys]
    inc   <- button frm [text := "increment", on command := increment fld]
    ran   <- button frm [text := "random", on command := (randReplace fld frm)]
    set frm [layout := margin 5 $ floatCentre $ column 2
            [centre $ widget fld, row 2 [widget inc, widget ran]]]

increment :: Textual w => w -> IO ()
increment field = do
    val <- get field text
    when ((not . null) val) $ set field [text := show $ 1 + read val]

checkKeys :: EventKey -> IO ()
checkKeys (EventKey key _ _) =
    when (elem (show key) $ "Backspace" : map show [0..9]) propagateEvent

randReplace :: Textual w => w -> Window a -> IO ()
randReplace field frame = do
    answer <- confirmDialog frame "Random" "Generate a random number?" True
    when answer $ getStdRandom (randomR (1,100)) >>= \num ->
                  set field [text := show (num :: Int)]
```


==Icon and {{header|Unicon}}==

This example uses the Unicon gui library, and so will not work in Icon.


```Unicon

import gui
$include "guih.icn"

# Provides a basic message dialog
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

# Provides a basic yes/no question dialog
class QuestionDialog : Dialog (answer, message)
  method answered_yes ()
    return answer == "yes"
  end

  method answer_yes ()
    answer := "yes"
    dispose ()
  end

  method answer_no ()
    answer := "no"
    dispose ()
  end

  method component_setup ()
    label := Label ("label="||message, "pos=20,20")
    add (label)
    buttonYes := TextButton("label=Yes", "pos=40,60")
    buttonYes.connect (self, "answer_yes", ACTION_EVENT)
    add (buttonYes)
    buttonNo := TextButton("label=No", "pos=120,60")
    buttonNo.connect (self, "answer_no", ACTION_EVENT)
    add (buttonNo)

    connect (self, "dispose", CLOSE_BUTTON_EVENT)
    attrib ("size=200,100", "bg=light gray")
  end

  initially (message)
    self.Dialog.initially()
    self.answer := "no"
    self.message := message
end

# The main window, displays the different components
class WindowApp : Dialog (field, value)
  method increment ()
    value +:= 1
    field.set_contents (string(value))
  end

  method random ()
    query := QuestionDialog ("Set to random?")
    query.show_modal ()
    if query.answered_yes () then {
      value := ?100
      field.set_contents (string(value))
    }
  end

  method handle_text_field ()
    if not(integer(field.get_contents ()))
      then {
        warning := MessageDialog ("Not a number")
        warning.show_modal ()
        field.set_contents (string(value))
      }
      else {
        value := integer(field.get_contents ())
      }
  end

  method component_setup ()
    value := 0
    field := TextField("contents="||value, "pos=20,20", "size=150")
    field.connect (self, "handle_text_field", TEXTFIELD_CHANGED_EVENT)
    add (field)
    button1 := TextButton("label=Increment", "pos=20,60", "size=70")
    button1.connect (self, "increment", ACTION_EVENT)
    add (button1)
    button2 := TextButton("label=Random", "pos=100,60", "size=70")
    button2.connect (self, "random", ACTION_EVENT)
    add (button2)

    connect (self, "dispose", CLOSE_BUTTON_EVENT)
    attrib ("size=200,100", "bg=light gray")
  end
end

# create and show the window
procedure main ()
  w := WindowApp ()
  w.show_modal ()
end

```



## J

{{works with|J 8.x}}

```j
INTERACT=: noun define
pc interact;
cc Value edit center;
cc increment button;cn "Increment";
cc random button;cn "Random";
pas 6 6;pcenter;
)

interact_run=: verb define
 wd INTERACT
 wd 'set Value text 0;'
 wd 'pshow;'
)

interact_cancel=: interact_close=: verb define
 wd'pclose'
)

interact_Value_button=: verb define
 wd 'set Value text ' , ": {. 0 ". Value
)

interact_increment_button=: verb define
 wd 'set Value text ' , ": 1 + {. 0 ". Value
)

interact_random_button=: verb define
 if. 2 = 2 3 wdquery 'Confirm';'Reset to random number?' do.
  wd 'set Value text ' , ": ?100
 end.
)
```

{{works with|J 6.x}}

```j
INTERACT=: 0 : 0
pc interact closeok;
xywh 6 6 48 12;cc Value edit;
xywh 6 18 48 12;cc increment button;cn "+";
xywh 6 30 48 12;cc random button;cn "?";
pas 6 6;pcenter;
rem form end;
)

interact_run=: 3 : 0
 wd INTERACT
 wd 'set Value 0;'
 wd 'pshow;'
)

interact_close=: 3 : 0
 wd'pclose'
)

interact_Value_button=: 3 : 0
 wd 'set Value ' , ": {. 0 ". Value
)

interact_increment_button=: 3 : 0
 wd 'set Value ' , ": 1 + {. 0 ". Value
)

interact_random_button=: 3 : 0
 if. 0 = 2 wdquery 'Confirm';'Reset to random number?' do.
  wd 'set Value ' , ": ?100
 end.
)
```


Note: I used an edit box for the value, and edit boxes do not get onChange events, so rejection of non-numeric values will be delayed until the use of a button (or pressing enter).

Example use:


```j
interact_run''
```



## Java

{{works with|Swing}}
{{works with|AWT}}
There are nice GUI editors in some IDEs (Netbeans has a notoriously good one), but this GUI was not built automatically, so it's a little easier to understand.

```java
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class Interact extends JFrame{
	final JTextField numberField;
	final JButton incButton, randButton;

	public Interact(){
		//stop the GUI threads when the user hits the X button
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		numberField = new JTextField();
		incButton = new JButton("Increment");
		randButton = new JButton("Random");

		numberField.setText("0");//start at 0

		//listen for button presses in the text field
		numberField.addKeyListener(new KeyListener(){
			@Override
			public void keyTyped(KeyEvent e) {
				//if the entered character is not a digit
				if(!Character.isDigit(e.getKeyChar())){
					//eat the event (i.e. stop it from being processed)
					e.consume();
				}
			}
			@Override
			public void keyReleased(KeyEvent e){}
			@Override
			public void keyPressed(KeyEvent e){}
		});

		//listen for button clicks on the increment button
		incButton.addActionListener(new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent e) {
				String text = numberField.getText();
				if(text.isEmpty()){
					numberField.setText("1");
				}else{
					numberField.setText((Long.valueOf(text) + 1) + "");
				}
			}
		});

		//listen for button clicks on the random button
		randButton.addActionListener(new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent e) {
				//show a dialog and if they answer "Yes"
				if(JOptionPane.showConfirmDialog(null, "Are you sure?") ==
					JOptionPane.YES_OPTION){
					//set the text field text to a random positive long
					numberField.setText(Long.toString((long)(Math.random()
							* Long.MAX_VALUE)));
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
		buttonPanel.add(randButton);

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

{{works with|Java|8+}}

```java5
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

public interface FunctionalKeyListener extends KeyListener {
  @Override
  public default void keyPressed(KeyEvent event) {}
  @Override
  public default void keyTyped(KeyEvent event) {}
  @Override
  public default void keyReleased(KeyEvent event) {}

  @FunctionalInterface
  public static interface Pressed extends FunctionalKeyListener {
    @Override
    public void keyPressed(KeyEvent event);
  }

  @FunctionalInterface
  public static interface Typed extends FunctionalKeyListener {
    @Override
    public void keyTyped(KeyEvent event);
  }

  @FunctionalInterface
  public static interface Released extends FunctionalKeyListener {
    @Override
    public void keyReleased(KeyEvent event);
  }
}
```


```java5
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Stream;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

public interface Interact {
  public static final JFrame FRAME = new JFrame();
  public static final JTextField FIELD = new JTextField();
  public static final JPanel PANEL = new JPanel();

  public static void setDefaultCloseOperation(JFrame frame) {
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  }

  public static void setText(JTextField field) {
    field.setText("0");
  }

  public static void setEditable(JTextField field) {
    field.setEditable(false);
  }

  public static boolean isDigitKeyChar(KeyEvent event) {
    return !Character.isDigit(event.getKeyChar());
  }

  public static void keyTyped(KeyEvent event) {
    Stream.of(event)
      .parallel()
      .filter(Interact::isDigitKeyChar)
      .forEach(KeyEvent::consume)
    ;
  }

  public static void addKeyListener(JTextField field) {
    field.addKeyListener((FunctionalKeyListener.Typed) Interact::keyTyped);
  }

  public static String mapText(String text) {
    return text.isEmpty()
      ? "1"
      : String.valueOf(Long.valueOf(text) + 1)
    ;
  }

  public static void actionPerformedOnIncrementButton(ActionEvent event) {
    Stream.of(FIELD)
      .parallel()
      .map(JTextField::getText)
      .map(Interact::mapText)
      .forEach(FIELD::setText)
    ;
  }

  public static void addActionListenerToIncrementButton(JButton button) {
    button.addActionListener(Interact::actionPerformedOnIncrementButton);
  }

  public static void addIncrementButton(JPanel panel) {
    Stream.of("Increment")
      .parallel()
      .map(JButton::new)
      .peek(Interact::addActionListenerToIncrementButton)
      .forEach(panel::add)
    ;
  }

  public static int showConfirmDialog(String question) {
    return JOptionPane.showConfirmDialog(null, question);
  }

  public static void setFieldText(int integer) {
    FIELD.setText(
      String.valueOf(
        (long) (Math.random() * Long.MAX_VALUE))
      )
    ;
  }

  public static void actionPerformedOnRandomButton(ActionEvent event) {
    Stream.of("Are you sure?")
      .parallel()
      .map(Interact::showConfirmDialog)
      .filter(Predicate.isEqual(JOptionPane.YES_OPTION))
      .forEach(Interact::setFieldText)
    ;
  }

  public static void addActionListenerToRandomButton(JButton button) {
    button.addActionListener(Interact::actionPerformedOnRandomButton);
  }

  public static void addRandomButton(JPanel panel) {
    Stream.of("Random")
      .parallel()
      .map(JButton::new)
      .peek(Interact::addActionListenerToRandomButton)
      .forEach(panel::add)
    ;
  }

  public static void acceptField(Consumer<JTextField> consumer) {
    consumer.accept(FIELD);
  }

  public static void prepareField(JTextField field) {
    Stream.<Consumer<JTextField>>of(
      Interact::setEditable,
      Interact::setText,
      Interact::addKeyListener
    )
      .parallel()
      .forEach(Interact::acceptField)
    ;
  }

  public static void addField(JFrame frame) {
    Stream.of(FIELD)
      .parallel()
      .peek(Interact::prepareField)
      .forEach(frame::add)
    ;
  }

  public static void acceptPanel(Consumer<JPanel> consumer) {
    consumer.accept(PANEL);
  }

  public static void processPanel(JPanel panel) {
    Stream.<Consumer<JPanel>>of(
      Interact::setLayout,
      Interact::addIncrementButton,
      Interact::addRandomButton
    )
      .parallel()
      .forEach(Interact::acceptPanel)
    ;
  }

  public static void addPanel(JFrame frame) {
    Stream.of(PANEL)
      .parallel()
      .peek(Interact::processPanel)
      .forEach(frame::add)
    ;
  }

  public static void setLayout(JFrame frame) {
    frame.setLayout(new GridLayout(2, 1));
  }

  public static void setLayout(JPanel panel) {
    panel.setLayout(new GridLayout(1, 2));
  }

  public static void setVisible(JFrame frame) {
    frame.setVisible(true);
  }

  public static void acceptFrame(Consumer<JFrame> consumer) {
    consumer.accept(FRAME);
  }

  public static void processField(JFrame frame) {
    Stream.<Consumer<JFrame>>of(
      Interact::setDefaultCloseOperation,
      Interact::setLayout,
      Interact::addField,
      Interact::addPanel,
      Interact::setVisible
    )
      .parallel()
      .forEach(Interact::acceptFrame)
    ;
  }

  public static void main(String... arguments) {
    Stream.of(FRAME)
      .parallel()
      .peek(Interact::processField)
      .forEach(JFrame::pack)
    ;
  }
}
```



## Julia


```julia

using Tk
w = Toplevel("Component Interaction Example")
fr = Frame(w)
pack(fr, {:expand=>true, :fill => "both"})
## The task: For a minimal "application", write a program that
## presents a form with three components to the user: A numeric input
## field ("Value") and two buttons ("increment" and "random").

value = Entry(fr, "")
increment = Button(fr, "Increment")
random = Button(fr, "Random")

formlayout(value, "Value:")
formlayout(increment, " ")
formlayout(random, " ")

set_value(value, "0") ## The field is initialized to zero.

tk_bind(increment, "command") do path  ## increment its value with the "increment" button.
  val = get_value(value) | float
  set_value(value, string(val + 1))
end


function validate_command(path, P)
    try
        if length(P) > 0 parsefloat(P) end
        tcl("expr", "TRUE")
    catch e
        tcl("expr", "FALSE")
    end
end
function invalid_command(path, W)
    println("Invalid value")
    tcl(W, "delete", "@0", "end")
end

tk_configure(value, {:validate=>"key", :validatecommand=>validate_command, :invalidcommand=>invalid_command })

## Pressing the "random" button presents a confirmation dialog, and resets the field's value to a random value if the answer is "Yes".
tk_bind(random, "command") do path
   out = Messagebox(w, "Randomize input", "Select a new random number?")
   if out == "ok"
       new_value = floor(100*rand(1))[1]
       set_value(value, string(new_value))
   end
end

```



## Kotlin

{{trans|Java}}


```scala
import java.awt.GridLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.KeyEvent
import java.awt.event.KeyListener
import javax.swing.*

class Interact : JFrame() {
    val numberField = JTextField()
    val incButton = JButton("Increment")
    val randButton = JButton("Random")
    val buttonPanel = JPanel()

    init {
        defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        numberField.text = "0"

        numberField.addKeyListener(object : KeyListener {
            override fun keyTyped(e : KeyEvent) : Unit {
                if (!Character.isDigit(e.keyChar)) e.consume()
            }
            override fun keyReleased(e : KeyEvent?) {}
            override fun keyPressed(e : KeyEvent) {}
        })

        incButton.addActionListener {
            val num = (numberField.text ?: "").toDouble()
            numberField.text = (num + 1).toString()
        }

        randButton.addActionListener(object : ActionListener {
            fun proceedOrNot() = JOptionPane.showConfirmDialog(randButton, "Are you sure?")
            override fun actionPerformed(e : ActionEvent) {
                if(proceedOrNot() == JOptionPane.YES_OPTION)
                    numberField.text = (Math.random() * Long.MAX_VALUE).toString()
            }
        })

        layout = GridLayout(2, 1)
        buttonPanel.layout = GridLayout(1, 2)
        buttonPanel.add(incButton)
        buttonPanel.add(randButton)
        add(numberField)
        add(buttonPanel)
        pack()
    }
}

fun main(args : Array<String>) {
    Interact().isVisible = true
}
```



## Liberty BASIC


### Input Verification


```lb
nomainwin
    textbox #demo.val, 20, 50, 90, 24
    button #demo.inc, "Increment", [btnIncrement], UL, 20, 90, 90, 24
    button #demo.rnd, "Random", [btnRandom], UL, 20, 120, 90, 24
    open "Rosetta Task: GUI component interaction" for window as #demo
    #demo "trapclose [quit]"
    validNum$ = "0123456789."
    #demo.val 0
wait

[quit]
    close #demo
end

[btnIncrement]
    #demo.val "!contents? nVal$"
    nVal$ = trim$(nVal$)
    if left$(nVal$, 1) = "-" then
        neg = 1
        nVal$ = mid$(nVal$, 2)
    else
        neg = 0
    end if
    validNum = 1
    nDecs = 0
    for i = 1 to len(nVal$)
        if instr(validNum$, mid$(nVal$, i, 1)) = 0 then
            validNum = 0
        end if
        if mid$(nVal$, i, 1) = "." then
            nDecs = nDecs + 1
        end if
    next i
    if nDecs > 1 then
        validNum = 0
    end if
    if neg = 1 then
        nVal$ = "-";nVal$
    end if
    if validNum = 0 then
        notice nVal$;" does not appear to be a valid number.  " + _
            "(Commas are not allowed.)"
    else
        nVal = val(nVal$)
        nVal = nVal + 1
    end if
    #demo.val nVal
wait

[btnRandom]
    confirm "Reset value to random number";yn$
    if yn$ = "yes" then
        nVal = int(rnd(1) * 100) + 1
        #demo.val nVal
    end if
wait
```


===Impossible to type non-numeric characters===

```lb
nomainwin
    stylebits #demo.val, _ES_NUMBER, 0, 0, 0
    textbox #demo.val, 20, 50, 90, 24
    button #demo.inc, "Increment", [btnIncrement], UL, 20, 90, 90, 24
    button #demo.rnd, "Random", [btnRandom], UL, 20, 120, 90, 24
    open "Rosetta Task: GUI component interaction" for window as #demo
    #demo "trapclose [quit]"
    #demo.val 0
wait

[quit]
    close #demo
end

[btnIncrement]
    #demo.val "!contents? nVal"
    nVal = nVal + 1
    #demo.val nVal
wait

[btnRandom]
    confirm "Reset value to random number";yn$
    if yn$ = "yes" then
        nVal = int(rnd(1) * 100) + 1
        #demo.val nVal
    end if
wait
```



## M2000 Interpreter

M2000 uses twips for positions and width/height of forms and controls. M2000 Environment has a window manager, which used in Windows and Ubuntu (through Wine) operating systems. The window style is the same to every os.
Version 2, addition a title for form (without title, default title if form variable name, form1).


```M2000 Interpreter

Module CheckIt {
      Declare form1 form
      Declare textbox1 textbox form form1
      Declare buttonInc Button form form1
      Declare buttonRND Button form form1
      Method textbox1, "move", 2000,2000,4000,600
      Method buttonInc, "move", 2000,3000,2000,600
      Method buttonRND, "move", 4000,3000,2000,600
      With form1, "Title", "Rosetta Code: GUI component interaction"
      With textbox1,"vartext" as textbox1.value$, "Prompt", "Value:", "ShowAlways", true
      With buttonInc,"Caption","Increment"
      With buttonRND,"Caption","Random"
      textbox1.value$="0"
      Function Local1(new Feed$) {
            \\ this Function can be used from other Integer
            \\ this$ and thispos, exist just before the call of this Function
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
      Function TextBox1.ValidString {
                  \\ this Function called direct from textbox
                  Read  New &this$, &thispos
                  Call Local local1(textbox1.value$)
      }
      Function buttonInc.Click {
               textbox1.value$=str$(val(textbox1.value$)+1, "")
      }
      Function buttonRND.Click {
            If  AsK$("Change Value with random number", "Question", "Yes", "No")="Yes" Then {
                      textbox1.value$=str$(Random(0, 10000), "")
                      After 100 {Try {Method textbox1,"GetFocus"}}
            }
      }
      \\ open modal
      Method form1, "show", 1
      Declare form1 nothing
}
Checkit

```



## Maple

For this problem, you will need to open up Maple, go to the Components tab on the left, and insert a Text Area, and 2 Buttons. By right clicking each button, and clicking Component Properties, change one to Increase, and the other to Random. Then, click on 'Edit Content Changed Action". In the one labeled increase, type:


```Maple

Increase();

```


In the one labeled Random, type:


```Maple

Random();

```


Then, by clicking the 2 gears and opening up the start-up commands, enter this:

```Maple

macro(SP=DocumentTools:-SetProperty, GP=DocumentTools:-GetProperty);
with(Maplets[Elements]):
SP("Text",value,0);
Increase:=proc()
	SP("Text",value,parse(GP("Text",value))+1);
end proc;
Random:=proc()
	maplet := Maplet(["Are you sure?", [Button("OK", Shutdown("true")), Button("Cancel", Shutdown())]]);
	result := Maplets[Display](maplet);
	if result = "true" then
		j:=rand(1..1000);
		SP("Text",value,j());
	end if;
end proc;

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
<lang>Manipulate[Null, {{value, 0}, InputField[Dynamic[value], Number] &},
 Row@{Button["increment", value++],
   Button["random",
    If[DialogInput[
      Column@{"Are you sure?",
        Row@{Button["Yes", DialogReturn[True]],
          Button["No", DialogReturn[False]]}}],
     value = RandomInteger@10000], Method -> "Queued"]}]
```



## Nim


### Gtk2

{{libheader|Gtk2}}

```nim
import
  gtk2, gdk2, glib2, strutils, math

import
  gtk2, gdk2, glib2, strutils, math

var valu: int = 0
var chngd_txt_hndler: gulong = 0

proc thisDestroy(widget: PWidget, data: Pgpointer) {.cdecl.} =
  main_quit()

randomize()
nim_init()
var win = window_new(gtk2.WINDOW_TOPLEVEL)
var content = vbox_new(true,10)
var hbox1 = hbox_new(true,10)
var hbox2 = hbox_new(false,1)
var lbl = label_new("Value:")
var entry_fld = entry_new()
entry_fld.set_text("0")
var btn_quit = button_new("Quit")
var btn_inc = button_new("Increment")
var btn_rnd = button_new("Random")
add(hbox2,lbl)
add(hbox2,entry_fld)
add(hbox1,btn_inc)
add(hbox1,btn_rnd)
pack_start(content, hbox2, true, true, 0)
pack_start(content, hbox1, true, true, 0)
pack_start(content, btn_quit, true, true, 0)
set_border_width(win, 5)
add(win, content)

proc on_question_clicked: bool =
    var dialog = win.message_dialog_new(0, MESSAGE_QUESTION,
      BUTTONS_YES_NO, "Use a Random number?")
    var response = dialog.run()
    if response == RESPONSE_YES:
       result = true
    elif response == RESPONSE_NO:
       result = false
    dialog.destroy()

proc thisInc(widget: PWidget, data: Pgpointer){.cdecl.} =
  inc(valu)
  entry_fld.set_text($valu)

proc thisRnd(widget: PWidget, data: Pgpointer){.cdecl.} =
  if on_question_clicked():
      valu = random(20)
      entry_fld.set_text($valu)

proc thisTextChanged(widget: PWidget, data: Pgpointer) {.cdecl.} =
  #signal_handler_block(entry_fld, chngd_txt_hndler)
  try:
     valu = parseInt($entry_fld.get_text())
  except EInvalidValue:
     valu = 0
  entry_fld.set_text($valu)
  #signal_handler_unblock(entry_fld, chngd_txt_hndler)
  #signal_emit_stop(entry_fld, signal_lookup("changed",TYPE_EDITABLE()),0)

discard signal_connect(win, "destroy", SIGNAL_FUNC(thisDestroy), nil)
discard signal_connect(btn_quit, "clicked", SIGNAL_FUNC(thisDestroy), nil)
discard signal_connect(btn_inc, "clicked", SIGNAL_FUNC(thisInc), nil)
discard signal_connect(btn_rnd, "clicked", SIGNAL_FUNC(thisRnd), nil)
chngd_txt_hndler = signal_connect(entry_fld, "changed", SIGNAL_FUNC(thisTextChanged), nil)

win.show_all()
main()
```



### IUP

{{libheader|IUP}}

```nim
import
  iup, strutils, math

# assumes you have the iup  .dll or .so installed

randomize()
discard iup.open(nil,nil)


var lbl = Label("Value:")
setAttribute(lbl,"PADDING","2x2")

var valu = Text(nil)
SetAttribute(valu, "PADDING", "2x2")
SetAttribute(valu, "VALUE", "0")

proc toCB(fp: proc): ICallback =
   return cast[ICallback](fp)

# Click handler for Click button
proc incClick(ih:PIhandle): cint {.cdecl.} =
    var s: string = $(GetAttribute(valu,"VALUE"))
    var x: int = 0
    try:
       x = 1 + parseInt(s)
    except:
       x = 1         # default to 1 if non-numeric entry
    setAttribute(valu,"VALUE", $x)
    return IUP_DEFAULT

# Click handler for Random button
proc randClick(ih:PIhandle): cint {.cdecl.} =
    if Iup.Alarm("Random Value?", "Set value to a random numer < 100 ?","Yes","No",nil) == 1:
        setAttribute(valu,"VALUE", $random(100))
    return IUP_DEFAULT

# Key handler to check for Esc pressed
proc key_cb(ih:PIhandle, c: cint):cint {.cdecl.} =
  #echo c
  if (c == Iup.K_esc) and (Iup.Alarm("Exit?", "Had enough?","Yes","Keep going",nil) == 1):
    return IUP_CLOSE    # Exit application
  return IUP_CONTINUE


var txtBox = Hbox(lbl, valu, nil)
SetAttribute(txtBox, "MARGIN", "10x10")

var incBtn = Button("&Increment", "")
var randBtn = Button("&Randomize", "")
var btnBox = Vbox(incBtn, randBtn, nil)
SetAttribute(btnBox, "MARGIN", "5x5")

var contents = Hbox(txtBox, btnBox, nil)
SetAttribute(contents, "MARGIN", "2x2")

discard setCallback(incBtn,"ACTION", toCB(incClick))
discard setCallback(randBtn,"ACTION", toCB(randClick))
discard setCallback(contents,"K_ANY", toCB(key_cb))

var dlg = Dialog(contents)
discard dlg.show()
discard mainloop()
iup.close()
```



## Oz

Using Mozart's standard GUI library, building a small desktop application:

```oz
declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}

  proc {Main}
     MaxValue = 1000
     NumberWidget
     GUI = lr(
              numberentry(init:1 min:0 max:MaxValue handle:NumberWidget)
              button(text:"Increase"
                     action:proc {$}
                               OldVal = {NumberWidget get($)}
                            in
                               {NumberWidget set(OldVal+1)}
                            end)
              button(text:"Random"
                     action:proc {$}
                               if {Ask "Reset to random?"} then
                                  Rnd = {OS.rand} * MaxValue div {OS.randLimits _}
                               in
                                  {NumberWidget set(Rnd)}
                               end
                            end)
              )
     Window = {QTk.build GUI}
  in
     {Window show}
  end

  fun {Ask Msg}
     Result
     Box = {QTk.build
            td(message(init:Msg)
               lr(button(text:"Yes" action:proc {$} Result=true  {Box close} end)
                  button(text:"No"  action:proc {$} Result=false {Box close} end)
                 ))}
  in
     {Box show}
     {Box wait}
     Result
  end
in
  {Main}
```


As a web application, using the "Roads" web programming library. Connect your browser to http://localhost:8080/start after starting the program.

```oz
declare
  [Roads] = {Module.link ['x-ozlib://wmeyer/roads/Roads.ozf']}

  MaxValue = 1000

  fun {Start Session}
     {Page 0}
  end

  fun {Page Val}
     html(
        body(
           %% numerical input with an HTML form
           local NewVal in
              form(
                 {NumberInput Val NewVal}
                 input(type:submit)
                 method:post
                 action:fun {$ _}
                           {Page NewVal}
                        end
                 )
           end
           %% link with button functionality
           a("Increase"
             href:fun {$ _}
                     {Page Val+1}
                  end)
           " "
           %% another "button-link"
           a("Random"
             href:fun {$ S}
                     p("Reset to random? - "
                       a("Yes" href:fun {$ _}
                                       Rnd = {OS.rand} * MaxValue div {OS.randLimits _}
                                    in
                                       {Page Rnd}
                                    end)
                       " "
                       a("No" href:fun {$ _} {Page Val} end)
                      )
                  end)
           ))
  end

  %% a "formlet", managing input of an integer value
  fun {NumberInput OldVal NewVal}
     input(type:text
           validate:int_in(0 MaxValue)
           value:{Int.toString OldVal}
           bind:proc {$ Str} NewVal = {String.toInt Str.escaped} end
          )
  end
in
  {Roads.registerFunction 'start' Start}
  {Roads.run}
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
      (html 0 "Increment" "@lib.css" NIL
         (form NIL
            (gui '(+Var +NumField) '*Number 20 "Value")
            (gui '(+JS +Button) "increment"
               '(inc '*Number) )
            (gui '(+Button) "random"
               '(ask "Reset to a random value?"
                  (setq *Number (rand)) ) ) ) ) ) )

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
	     Input_field     :=
	       text_item(input_field),
	     Increment       :=
	       button(increment),
	     Random          :=
	       button(random)
	   ],
	 modifications :=
	   [ Input_field := [ label  := 'Value :',
			      length := 28
			    ]
	   ],
	 layout        :=
	   [ area(Input_field,
		  area(54, 24, 251, 24)),
	     area(Increment,
		  area(54, 90, 80, 24)),
	     area(Random,
		  area(230, 90, 80, 24))
	   ],
	 behaviour     :=

	   [
	     Increment := [
			 message := message(@prolog,
					    increment,
					    Input_field )
		       ],
	     Random := [
			 message := message(@prolog,
					    my_random,
					    Input_field)
			  ],
	     Input_field := [
			 message := message(@prolog,
					    input,
					    GUI_Interaction,
					    Increment,
					    @receiver,
					    @arg1)
			  ]
	   ]

       ]).

gui_component :-
	make_dialog(S, 'GUI_Interaction'),
	send(S, open).


increment(Input) :-
	get(Input, selection, V),
	atom_number(V, Val),
	Val1 is Val + 1,
	send(Input, selection, Val1).

my_random(Input) :-
        new(D, dialog('GUI Interaction')),
        send(D, append(label(lbl,'Confirm your choice !'))),
        send(D, append(button(ok, message(D, return, ok)))),
        send(D, append(button(cancel, message(D, return, ko)))),
        send(D, default_button(ok)),
        get(D, confirm, Rval),
        free(D),
        (   Rval = ok
	->  X is random(10000),
	    send(Input, selection, X)
	).


input(Gui, Btn, Input, Selection) :-
	catch( (term_to_atom(T, Selection), number(T), send(Gui, focus, Btn)),
	       _,
	       (   send(@display, inform, 'Please type a number !'),
		   send(Input,clear))).

```



## PureBasic


```PureBasic
Enumeration
  #StringGadget
  #Increment
  #Random
EndEnumeration

If OpenWindow(0,#PB_Ignore,#PB_Ignore,180,50,"PB-GUI",#PB_Window_SystemMenu)
  StringGadget(#StringGadget,5,5,170,20,"",#PB_String_Numeric)
  ButtonGadget(#Increment,5,25,80,20, "Increment")
  ButtonGadget(#Random,  90,25,80,20, "Random")
  Repeat
    Event=WaitWindowEvent()
    If Event=#PB_Event_Gadget
      Select EventGadget()
        Case #Increment
          CurrentVal=Val(GetGadgetText(#StringGadget))
          SetGadgetText(#StringGadget,Str(CurrentVal+1))
        Case #Random
          Flag=#PB_MessageRequester_YesNo
          Answer=MessageRequester("Randomize","Are you sure?",Flag)
          If Answer=#PB_MessageRequester_Yes
            SetGadgetText(#StringGadget,Str(Random(#MAXLONG)))
          EndIf
      EndSelect
    EndIf
  Until Event=#PB_Event_CloseWindow
  CloseWindow(0)
EndIf
```



## Perl 6


{{libheader|GTK}}


```perl6
use GTK::Simple;
use GTK::Simple::App;

my GTK::Simple::App $app .= new(title => 'GUI component interaction');

$app.set-content(
    my $box = GTK::Simple::VBox.new(
        my $value     = GTK::Simple::Entry.new(text => '0'),
        my $increment = GTK::Simple::Button.new(label => 'Increment'),
        my $random    = GTK::Simple::Button.new(label => 'Random'),
    )
);

$app.size-request(400, 100);
$app.border-width = 20;
$box.spacing = 10;

$value.changed.tap: {
    ($value.text ||= '0') ~~ s:g/<-[0..9]>//;
}

$increment.clicked.tap: {
    my $val = $value.text; $val += 1; $value.text = $val.Str
}

$random.clicked.tap: {
    # Dirty hack to work around the fact that GTK::Simple doesn't provide
    # access to GTK message dialogs yet :P
    if run «zenity --question --text "Reset to random value?"» {
        $value.text = (^100).pick.Str
    }
}

$app.run;
```



## Phix

{{libheader|pGUI}}

```Phix
include pGUI.e

Ihandle txt, increment, random, hbx, vbx, dlg

function action_cb(Ihandle /*ih*/, integer ch)
    if not find(ch,"0123456789-") then return IUP_IGNORE end if
    return IUP_CONTINUE
end function

function increment_cb(Ihandle /*ih*/)
    integer v = IupGetInt(txt,"VALUE")+1
    IupSetInt(txt,"VALUE",v)
    return IUP_CONTINUE
end function

function random_cb(Ihandle /*ih*/)
    if IupAlarm("Confirm","Replace wth random value?","Yes","No")=1 then
        IupSetInt(txt,"VALUE",rand(1000))
    end if
    return IUP_CONTINUE
end function

function esc_close(Ihandle /*ih*/, atom c)
    return iff(c=K_ESC?IUP_CLOSE:IUP_CONTINUE)
end function

IupOpen()
txt = IupText(Icallback("action_cb"),"EXPAND=YES")
increment = IupButton("increment",Icallback("increment_cb"))
random = IupButton("random",Icallback("random_cb"))
hbx = IupHbox({increment,random},"MARGIN=0x10, GAP=20")
vbx = IupVbox({txt,hbx},"MARGIN=40x20")
dlg = IupDialog(vbx)
IupSetCallback(dlg, "K_ANY", Icallback("esc_close"))
IupShow(dlg)
IupMainLoop()
IupClose()
```



## Python

W/out class and using grid layout
{{libheader|Tkinter}}

```python

import random, tkMessageBox
from Tkinter import *
window = Tk()
window.geometry("300x50+100+100")
options = { "padx":5, "pady":5}
s=StringVar()
s.set(1)
def increase():
    s.set(int(s.get())+1)
def rand():
    if tkMessageBox.askyesno("Confirmation", "Reset to random value ?"):
        s.set(random.randrange(0,5000))
def update(e):
    if not e.char.isdigit():
        tkMessageBox.showerror('Error', 'Invalid input !')
        return "break"
e = Entry(text=s)
e.grid(column=0, row=0, **options)
e.bind('<Key>', update)
b1 = Button(text="Increase", command=increase, **options )
b1.grid(column=1, row=0, **options)
b2 = Button(text="Random", command=rand, **options)
b2.grid(column=2, row=0, **options)
mainloop()
```


{{libheader|Tkinter}}

```python
import random
from Tkinter import *
import tkMessageBox


class Application(Frame):
    def __init__(self, master):
        Frame.__init__(self, master)
        self.counter = 0
        self.contents = StringVar()
        self.contents.set(str(self.counter))
        self.pack(expand=True, fill='both', padx=10, pady=15)
        self.create_widgets()

    def increment(self, *args):
        self.counter += 1
        self.update_entry()

    def random(self):
        if tkMessageBox.askyesno("Confirmation", "Reset to random value ?"):
            self.counter = random.randint(0, 5000)
            self.update_entry()

    def entry_updated(self, event, *args):
        if not event.char:
            return 'break'
        if not event.char.isdigit():
            tkMessageBox.showerror('Error', 'Invalid input !')
            return 'break'
        self.counter = int('%s%s' % (self.contents.get(), event.char))

    def update_entry(self):
        self.contents.set(str(self.counter))
        self.entry['textvariable'] = self.contents

    def create_widgets(self):
        options = {'expand': True, 'fill': 'x', 'side': 'left', 'padx': 5}
        self.entry = Entry(self)
        self.entry.bind('<Key>', self.entry_updated)
        self.entry.pack(**options)
        self.update_entry()
        self.increment_button = Button(self, text='Increment', command=self.increment)
        self.increment_button.pack(**options)
        self.random_button = Button(self, text='Random', command=self.random)
        self.random_button.pack(**options)


if __name__ == '__main__':
    root = Tk()
    try:
        app = Application(master=root)
        app.master.title("Rosetta code")
        app.mainloop()
    except KeyboardInterrupt:
        root.destroy()
```


[[File:GUI_component_interaction_Python.png]]


## R


```R

library(gWidgets)
options(guiToolkit="RGtk2") ## using gWidgtsRGtk2

w <- gwindow("Interaction")

g <- ggroup(cont=w, horizontal=FALSE)
e <- gedit(0, cont=g, coerce.with=as.numeric)
bg <- ggroup(cont=g)

inc_btn <- gbutton("increment", cont=bg)
rdm_btn <- gbutton("random", cont=bg)

addHandlerChanged(e, handler=function(h,...) {
  val <- svalue(e)
  if(is.na(val))
    galert("You need to enter a number", parent=w)
})

addHandlerChanged(inc_btn, handler=function(h,...) {
  val <- svalue(e)
  if(is.na(val))
    galert("Can't increment if not a number", parent=w)
  else
    svalue(e) <- val + 1
})

addHandlerChanged(rdm_btn, handler=function(h,...) {
  if(gconfirm("Really replace value?"))
    svalue(e) <- sample(1:1000, 1)
})

```



## Racket


```racket

#lang racket/gui

(define frame (new frame% [label "Interaction Demo"]))

(define inp
  (new text-field% [label "Value"] [parent frame] [init-value "0"]
       [callback
        (λ(f ev)
          (define v (send f get-value))
          (unless (string->number v)
            (send f set-value (regexp-replace* #rx"[^0-9]+" v ""))))]))

(define buttons (new horizontal-pane% [parent frame]))
(define inc-b
  (new button% [parent buttons] [label "Increment"]
       [callback (λ (b e) (let* ([v (string->number (send inp get-value))]
                                 [v (number->string (add1 v))])
                            (send inp set-value v)))]))
(define rand-b
  (new button% [parent buttons] [label "Random"]
       [callback (λ (b e) (when (message-box "Confirm" "Are you sure?"
                                             frame '(yes-no))
                            (send inp set-value (~a (random 10000)))))]))

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
                      setGeometry(200,200,100,30)
                      settext("Random")
                      setclickevent("Rand()")}

               btn2 = new qpushbutton(win1) {
                      setGeometry(50,200,100,30)
                      settext("Increment")
                      setclickevent("Increment()")}

               lineedit1 = new qlineedit(win1) {
                           setGeometry(10,100,350,30)}
               show()}
exec()}

func Rand
     num = string(random(10000))
     lineedit1.settext(num)

func Increment
     lineedit1{ num = text()}
     num = string(number(num)+1)
     lineedit1.settext(num)

```


Output:

[[File:CalmoSoftRandom.jpg]]


## Ruby

{{libheader|Shoes}}

```ruby
Shoes.app(title: "GUI component interaction") do
  stack do
    textbox = edit_line

    textbox.change do
      textbox.text = textbox.text.gsub(/[^\d]/, '') and alert "Input must be a number!" if textbox.text !~ /^\d*$/
    end

    flow do
      button "Increment" do
        textbox.text = textbox.text.to_i + 1
      end

      button "Random" do
        textbox.text = rand 5000 if confirm "Do you want a random number?"
      end
    end
  end
end
```



## Run BASIC


```runbasic
dim dd$(5)                 ' drop down box
for i = 1 to 5
 dd$(i) = "Drop ";i
next i
value$ = "1234"
notes$ = "Rosetta Code
is good"

bf$	= "<SPAN STYLE='font-family:arial; font-weight:700; font-size:12pt'>"

[screen]
cls
html bf$;"<center><TABLE BORDER=1 CELLPADDING=0 CELLSPACING=0 bgcolor=wheat>"
html "<TR align=center BGCOLOR=tan><TD colspan=2>Rosetta Code</TD></TR><TR>"
html "<TD align=right bgcolor=tan>Value:</TD><TD>"
     textbox #val,value$,5
html "</TD></TR><TR><TD bgcolor=tan align=right>Radio</TD><TD>"
     radiogroup #rdo,"1,2,3,4,5",rdo$
                #rdo horizontal(1)
html "</TD></TR><TR><TD bgcolor=tan align=right>Drop Down</TD><TD>"
     listbox #dd,dd$(),1

html "</TD></TR><TR><TD bgcolor=tan align=right>Notes</TD><TD>"
     textarea 	#notes,notes$,25,3
html "</TD></TR><TR bgcolor=tan><TD colspan=2 ALIGN=CENTER>"
     button #inc, "Increment", [incr]
     button #rnd, "Random", [rand]
     button #ex, "Exit", [exit]
html "</TD></TR></TABLE>"
wait

[incr]
value	= val(#val contents$())
value$  = str$(value + 1)
goto [screen]

[rand]
value$ = str$(int(rnd(1) * 10000))
goto [screen]

[exit]
print "Bye"
end
```

[[File:GuiComponentRunBasic.png]]


## Scala


```Scala
import scala.swing._
import scala.swing.Swing._
import scala.swing.event._

object Interact extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Rosetta Code >>> Task: component interaction | Language: Scala"

    val numberField = new TextField {
      text = "0" // start at 0
      horizontalAlignment = Alignment.Right
    }

    val incButton = new Button { text = "Increment" }
    val randButton = new Button { text = "Random" }

    // arrange buttons in a grid with 1 row, 2 columns
    val buttonPanel = new GridPanel(1, 2) {
      contents ++= List(incButton, randButton)
    }

    // arrange text field and button panel in a grid with 2 row, 1 column
    contents = new GridPanel(2, 1) { contents ++= List(numberField, buttonPanel) }

    // listen for keys pressed in numberField and button clicks
    listenTo(numberField.keys, incButton, randButton)
    reactions += {
      case kt: KeyTyped if (!kt.char.isDigit) => // if the entered char isn't a digit …
        kt.consume // … eat the event (i.e. stop it from being processed)
      case ButtonClicked(`incButton`) =>
        if (numberField.text.isEmpty) numberField.text = "0"
        // we use BigInt to avoid long overflow/number format exception
        numberField.text = (BigInt(numberField.text) + 1).toString
      case ButtonClicked(`randButton`) =>
        import Dialog._
        numberField.text = showOptions(buttonPanel, message = "Are you sure?",
          title = "Choose an option!", entries = List("Yes", "No", "Cancel"),
          initial = 2) match {
            case Result.Yes => (Long.MaxValue * math.random).toLong.toString
            case _          => numberField.text
          }
    }
    centerOnScreen()
  }
}
```



## Smalltalk

{{works with|Smalltalk/X}}

```smalltalk
|top input vh incButton rndButton|

vh := ValueHolder with:0.

top := StandardSystemView label:'Rosetta GUI interaction'.
top extent:300@100.
top add:((Label label:'Value:') origin: 0 @ 10 corner: 100 @ 40).
top add:(input := EditField origin: 102 @ 10 corner: 1.0 @ 40).
input model:(TypeConverter onNumberValue:vh).
input acceptOnLostFocus:true; acceptOnReturn:true.

top add:((incButton := Button label:'Inc') origin: 10 @ 50 corner: 100 @ 80).
top add:((rndButton := Button label:'Rnd') origin: 110 @ 50 corner: 210 @ 80).

incButton action:[ vh value: (vh value + 1) ].
rndButton action:[ vh value: Random nextInteger ].

top open
```

{{Out}}
[[File:Guiintsmalltalkx.png]]


## Tcl

{{libheader|Tk}}

```tcl
package require Tk

###--- Our data Model! ---###
# A single variable will do just fine
set field 0

###--- Lay out the GUI components in our View ---###
# We use the Ttk widget set here; it looks much better on Windows and OSX

# First, a quick hack to make things look even nicer
place [ttk::frame .bg] -relwidth 1 -relheight 1

# A labelled frame containing an entry field constrained to use numbers
pack [ttk::labelframe .val -text "Value"]
pack [ttk::entry .val.ue -textvariable field \
	-validate key -invalidcommand bell \
	-validatecommand {string is integer %P}]
# Now, a pair of buttons
pack [ttk::button .inc -text "increment" -command step]
pack [ttk::button .rnd -text "random" -command random]

###--- Now we define the behaviors, the Controller ---###
# How to respond to a click on the "increment" button
proc step {} {
    global field
    incr field
}
# How to respond to a click on the "random" button
proc random {} {
    global field
    if {[tk_messageBox -type yesno -parent . \
	    -message "Reset to random?"] eq "yes"} {
	set field [expr {int(rand() * 5000)}]
    }
}
```



## Vala

{{libheader|Gtk+-3.0}}


```vala
bool validate_input(Gtk.Window window, string str){
    int64 val;
    bool ret = int64.try_parse(str,out val);;

    if(!ret){
        var dialog = new Gtk.MessageDialog(window,
       	                                   Gtk.DialogFlags.MODAL,
       	                                   Gtk.MessageType.ERROR,
       	                                   Gtk.ButtonsType.OK,
       	                                   "Invalid value");
        dialog.run();
        dialog.destroy();
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


    var label = new Gtk.Label ("Value:");


    var entry = new Gtk.Entry();
    entry.set_text("0");   //initialize to zero
    entry.activate.connect (() => {
        // read and validate the entered value
        validate_input(window, entry.get_text());
    });


    // button to increment
    var ib = new Gtk.Button.with_label("increment");
    ib.clicked.connect(() => {
        // read and validate the entered value
        var str = entry.get_text();
        if(validate_input(window, str)){
            entry.set_text((int.parse(str)+1).to_string());
        }
    });


    // button to put in a random value if confirmed
    var rb = new Gtk.Button.with_label("random");
    rb.clicked.connect (() => {
        var dialog = new Gtk.MessageDialog(window,
        	                               Gtk.DialogFlags.MODAL,
        	                               Gtk.MessageType.QUESTION,
        	                               Gtk.ButtonsType.YES_NO,
        	                               "set random value");
        var answer = dialog.run();
        dialog.destroy();
        if(answer == Gtk.ResponseType.YES){
            entry.set_text(Random.int_range(0,10000).to_string());
        }
    });


    box.pack_start(label, false, false, 2);
    box.pack_start(entry, false, false, 2);
    box.pack_start(ib, false, false, 2);
    box.pack_start(rb, false, false, 2);

    window.add(box);

    window.show_all();

    Gtk.main ();
    return 0;
}

```



## Visual Basic


In VB, windows are usually created in the IDE. The generated code is hidden from the programmer unless viewed outside of the IDE. For the sake of this task, I have included that code.

Note that there are other methods of validating the entered value. The method used is dependant on the program's requirements.


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
   Begin VB.CommandButton cmdRnd
      Caption         =   "Random"
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
'-----user-written code begins here; everything above this line is hidden in the IDE-----
Private Sub Form_Load()
    Randomize Timer
End Sub

Private Sub cmdRnd_Click()
    If MsgBox("Random?", vbYesNo) Then txtValue.Text = Int(Rnd * 11)
End Sub

Private Sub cmdInc_Click()
    If Val(txtValue.Text) < 10 Then txtValue.Text = Val(txtValue.Text) + 1
End Sub

Private Sub txtValue_KeyPress(KeyAscii As Integer)
    Select Case KeyAscii
        Case 8, 43, 45, 48 To 57
            'backspace, +, -, or number
        Case Else
            KeyAscii = 0
    End Select
End Sub
```



## Web 68

Web 68 uses an include file to use the Xforms library.

```web68
@1Rosetta code program.
@aPROGRAM guicomponent CONTEXT VOID USE standard
BEGIN
@<Included declarations@>
@<Modes in the outer reach@>
@<Names in the outer reach@>
@<Callback procedures@>
@<Other routines@>
@<Main logic@>
END
FINISH

@ This file contains all the macros for the Xforms library procedures.
Only macros which are called will actually generate code.

@iforms.w@>

@ Initialise the Xforms library, create the form, set the value in the
input field, show the form and hand control to the Xforms library.

@<Main...@>=
open(argf,"",arg channel);
fl initialize(argc,argv,"GUI interact",NIL,0);
main form:=create form main;
fl set input(main input OF main form,float(value,10,5,2));
fl show form(main OF main form,fl place center,fl fullborder,"GUI interact");
fl do forms

@ The input value will be stored in !value!.

@<Names...@>=
REF FDMAIN main form;
REAL value:=0;
FILE argf;

@1The form.
The following section contains declarations for the form. It was output by
the program 'fdtow68' using the file output by the 'fdesign' program.

@2Modes.
This is the mode declaration for form !main!.

@<Modes...@>=
MODE FDMAIN = STRUCT(
   REF FLFORM main,
   REF FLOBJECT main increment,
   REF FLOBJECT main input,
   REF FLOBJECT main random);

@ This procedure creates form !main!.

@<Other...@>=
PROC create form main = REF FDMAIN:
BEGIN
   REF FLOBJECT obj;
   REF FDMAIN fdui:=HEAP FDMAIN;
   OP(REF FDMAIN)CBPTR TOCBPTR = BIOP 99;
   main OF fdui:=fl bgn form(fl no box,259,126);
   obj:=fl add box(fl up box,0,0,259,126,"");
      fl set object color(obj,fl col1,fl col1);
   main input OF fdui:=obj:=fl add input(fl float input,78,18,160,35,"Value");
      fl set object lsize(obj,fl normal size);
      fl set object callback(obj,main cb,1);
      fl set object return(obj,fl return end changed);
   main increment OF fdui:=obj:=fl add button(fl normal button,20,70,100,40,"Increment");
      fl set object lsize(obj,fl normal size);
      fl set object callback(obj,main cb,2);
      fl set button mouse buttons(obj,BIN 7);
   main random OF fdui:=obj:=fl add button(fl normal button,140,70,100,40,"Random");
      fl set object lsize(obj,fl normal size);
      fl set object callback(obj,main cb,3);
      fl set button mouse buttons(obj,BIN 7);
   fl end form;
   fl adjust form size(main OF fdui);
   fdui OF main OF fdui:=TOCBPTR fdui;
   fdui
END; #create form main#

@2Callback procedures.
There is only one callback procedure.

@<Callback...@>=
PROC main cb = (REF FLOBJECT obj,INT data)VOID:
CASE data
IN
	#input#
	BEGIN
		FILE mf;
		open(mf,fl get input(main input OF main form)+blank,mem channel);
		get(mf,value);  #convert the input to binary#
		close(mf)
	END
	,
	#increment#
	(
		value +:= 1;
		fl set input(main input OF main form,float(value,10,5,2))
	)
	,
	#random#
	(
		value:=random;
		fl set input(main input OF main form,float(value,10,5,2))
	)
ESAC; #main cb#

@2Macro calls.
Here are all the Xforms macro calls in alphabetical order.

@<Include...@>=
macro fl add box;
macro fl add button;
macro fl add input;
macro fl adjust form size;
macro fl bgn form;
macro fl do forms;
macro fl end form;
macro fl get border width;
macro fl get input;
macro fl initialize;
macro fl set border width;
macro fl set button mouse buttons;
macro fl set input;
macro fl set object callback;
macro fl set object color;
macro fl set object lsize;
macro fl set object return;
macro fl show form;

@ The end.
```

