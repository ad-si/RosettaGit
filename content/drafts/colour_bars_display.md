+++
title = "Colour bars/Display"
description = ""
date = 2019-06-14T20:06:43Z
aliases = []
[extra]
id = 9747
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Display a series of vertical color bars across the width of the display. 

The color bars should either use:
:::*   the system palette,   or 
:::*   the sequence of colors: 
::::::*   black
::::::*   red
::::::*   green
::::::*   magenta
::::::*   cyan
::::::*   yellow
::::::*   white




## ActionScript


```ActionScript3

package {
    
    import flash.display.Sprite;
    import flash.events.Event;

    public class ColourBars extends Sprite {
        
        public function ColourBars():void {
            if (stage) init();
            else addEventListener(Event.ADDED_TO_STAGE, init);
        }
        
        private function init(e:Event = null):void {
            
            var colours:Array = [ 0x000000, 0xFF0000, 0x00FF00, 0x0000FF, 0xFF00FF, 0x00FFFF, 0xFFFF00, 0xFFFFFF ];
            var w:Number = stage.stageWidth / 8, h:Number = stage.stageHeight;
            var x:Number = 0, i:uint, c:uint;
            
            for ( i = 0; i < 8; i++ ) {
                c = colours[i];
                graphics.beginFill(c);
                graphics.drawRect(w * i, 0, w, h);
            }
            
        }
        
    }
    
}

```



## AutoHotkey

{{libheader|GDI+}} (available at http://www.autohotkey.net/~tic/Gdip.ahk)

```AutoHotkey
#SingleInstance, Force
#NoEnv
SetBatchLines, -1
 
; Uncomment if Gdip.ahk is not in your standard library
;#Include, Gdip.ahk
 
; Start gdi+
If !pToken := Gdip_Startup()
{
   message =
   ( LTrim
      gdiplus error!, Gdiplus failed to start.
      Please ensure you have gdiplus on your system.
   )
   MsgBox, 48, %message%
   ExitApp
}
OnExit, Exit

; Set the width and height we want as our drawing area, to draw everything in.
; This will be the dimensions of our bitmap
Width := A_ScreenWidth, Height := A_ScreenHeight
 
; Create a layered window
; (+E0x80000 : must be used for UpdateLayeredWindow to work!)
; that is always on top (+AlwaysOnTop), has no taskbar entry or caption
Gui, 1: -Caption +E0x80000 +LastFound +OwnDialogs +Owner +AlwaysOnTop
 
; Show the window
Gui, 1: Show, NA
 
; Get a handle to this window we have created in order to update it later
hwnd1 := WinExist()
 
; Create a gdi bitmap with width and height of what we are going to
; draw into it. This is the entire drawing area for everything
hbm := CreateDIBSection(Width, Height)
 
; Get a device context compatible with the screen
hdc := CreateCompatibleDC()
 
; Select the bitmap into the device context
obm := SelectObject(hdc, hbm)
 
; Get a pointer to the graphics of the bitmap, for use with drawing functions
G := Gdip_GraphicsFromHDC(hdc)
 
; ARGB = Transparency, Red, Green, Blue
Colors := "0xFF000000,0xFFFF0000,0xFF00FF00,0xFF0000FF"
Colors .= ",0xFFFF00FF,0xFF00FFFF,0xFFFFFF00,0xFFFFFFFF"
; This list ^ is Black, Red, Green, Blue, Magenta, Cyan, Yellow, White
StringSplit Colors, Colors, `,
w := Width // Colors0
Loop % Colors0
{
   ; Create a brush to draw a rectangle
   pBrush := Gdip_BrushCreateSolid(Colors%A_Index%)
 
   ; Fill the graphics of the bitmap with a rectangle using the brush created
   Gdip_FillRectangle(G, pBrush, w*(A_Index-1), 0, w, height)
 
   ; Delete the brush as it is no longer needed and wastes memory
   Gdip_DeleteBrush(pBrush)
}
; Update the specified window we have created (hwnd1) with a handle to our
; bitmap (hdc), specifying the x,y,w,h we want it positioned on our screen
; So this will position our gui at (0,0) with the Width and
; Height specified earlier
UpdateLayeredWindow(hwnd1, hdc, 0, 0, Width, Height)
 
 
; Select the object back into the hdc
SelectObject(hdc, obm)
 
; Now the bitmap may be deleted
DeleteObject(hbm)
 
; Also the device context related to the bitmap may be deleted
DeleteDC(hdc)
 
; The graphics may now be deleted
Gdip_DeleteGraphics(G)
Return
 
;#######################################################################
 
GuiEscape:
Exit:
; gdi+ may now be shutdown on exiting the program
Gdip_Shutdown(pToken)
ExitApp
Return
```



## BASIC


=
## Applesoft BASIC
=

```ApplesoftBasic
1 DATA1,12,6,3,14,13,15
2 HOME : GR : FOR I = 1 TO 7
3                 READ C(I) : NEXT
4 FOR I = 0 TO 39
5     COLOR= C(I / 5)
6     VLIN 0,39 AT I : NEXT
```


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}

```bbcbasic
      SW_MAXIMIZE = 3
      SYS "ShowWindow", @hwnd%, SW_MAXIMIZE
      VDU 26
      
      W% = @vdu%!208 / 4
      H% = @vdu%!212 * 2
      
      COLOUR 1,9
      COLOUR 2,10
      COLOUR 3,12
      COLOUR 4,13
      COLOUR 5,14
      COLOUR 6,11
      COLOUR 7,15
      
      FOR C% = 0 TO 7
        GCOL C%
        RECTANGLE FILL C%*W%, 0, W%, H%
      NEXT

```


=
## Commodore BASIC
=

This task achieved only on Commodore computers with color capabilities. The example below tested as-is in VICE on the following machines:
* Commodore 64
* Commodore 128
* Commodore Plus 4
* Commodore CBM-II (with VIC-II video)
* Commodore SX-64


It will also work well on the VIC-20 if you either omit line 310 or comment (REM) it out as the VIC-20 has only an eight-color palette. 

This cross-compatibility is achieved by using the PETSCII control codes (CHR$(x)) for changing color, which are generally the same across all platforms, although the system palettes themselves may differ. This is combined with printing a "reverse video" space (solid block) character in the color selected.


```gwbasic
5 REM COLOR BARS PROGRAM
10 PRINT CHR$(147);:DIM CO(17):C=1
20 READ CO(C):IF CO(C)>0 THEN C=C+1:GOTO 20
25 SP$="     ":H=22:C=C-1:REM SP$=5 SPACES
30 FOR R=1 TO C STEP INT(C/2)
40 FOR H=1 TO 10
50 FOR B=R TO (R+INT(C/2))-1
60 PRINT CHR$(18);CHR$(CO(B));SP$;
70 NEXT B
75 REM CHECK IF PALETTE IS ONLY 8 (VIC-20). 
76 REM IF SO, WE NEED TO FILL THE LAST TWO SPACES.
80 IF C<=8 THEN PRINT "  ";
90 NEXT H:NEXT R
100 GET K$:IF K$="" THEN 100
110 END
280 REM BECAUSE THE CONTROL CODES FOR COLOR ARE NON-SEQUENTIAL,
285 REM WE PUT THEM INTO A SEQUENTIAL ARRAY FOR USE
300 DATA 144,5,28,159,156,30,31,158
305 REM OMIT OR REM THE NEXT LINE FOR VIC-20
310 DATA 129,149,150,151,152,153,154,155
320 DATA 0:REM DATA TERMINATOR
```


=
## Liberty BASIC
=

```lb
nomainwin
colors$="black red green blue pink cyan yellow white"
WindowWidth=DisplayWidth:WindowHeight=DisplayHeight
UpperLeftX=1:UpperLeftY=1
barWidth=DisplayWidth/8
graphicbox #main.g, 0,0,DisplayWidth,DisplayHeight
open "" for window_popup as #main
#main "trapclose [quit]"
#main.g "down; setfocus; when characterInput [quit]"
#main.g "when leftButtonUp [quit]"
#main.g "size ";barWidth

for x = barWidth/2 to DisplayWidth step barWidth
    i=i+1
    if i>8 then i=1
    col$=word$(colors$,i)
    #main.g "color ";col$;"; line ";x;" 0 ";x;" ";DisplayHeight
next
wait
[quit] close #main:end 
 
```


=
## Locomotive Basic
=

[[File:CPC color bars.png|thumb|right]]

Show the default MODE 0 palette (includes two blinking colors at the end):


```locobasic
10 MODE 0:BORDER 23
20 FOR x=0 TO 15
30 ORIGIN x*40,0
40 GRAPHICS PEN x
50 FOR z=0 TO 39 STEP 4:MOVE z,0:DRAW z,400:NEXT
60 NEXT
70 CALL &bb06 ' wait for key press
```


=
## PureBasic
=
Press Enter or Escape to exit the program.

```PureBasic
Dim color(7)
color(0) = RGB($00, $00, $00) ;black
color(1) = RGB($FF, $00, $00) ;red
color(2) = RGB($00, $FF, $00) ;green
color(3) = RGB($00, $00, $FF) ;blue
color(4) = RGB($FF, $00, $FF) ;magenta
color(5) = RGB($00, $FF, $FF) ;cyan
color(6) = RGB($FF, $FF, $00) ;yellow
color(7) = RGB($FF, $FF, $FF) ;white 

If Not InitKeyboard(): End: EndIf    ;can't init keyboard
If Not InitSprite(): End: EndIf      ;can't init sprite/screen library
If Not ExamineDesktops(): End: EndIf ;can't retrieve information about desktop

height = DesktopHeight(0)
width = DesktopWidth(0)
depth = DesktopDepth(0)
If OpenScreen(width, height, depth, "Press ENTER to exit")
  StartDrawing(ScreenOutput())
    For c = 0 To 7
      Box((width * c) / 8, 0, width / 8, height, color(c))
    Next
  StopDrawing()
  FlipBuffers()

  Repeat
    Delay(10)
    ExamineKeyboard()
  Until KeyboardPushed(#PB_Key_Escape) Or KeyboardPushed(#PB_Key_Return)
  CloseScreen()
EndIf
```


### =Alternate method using console=


```PureBasic
DataSection
  ;Black, Red, Green, Blue, Magenta, Cyan, Yellow, White
  Data.i  0, 12, 10, 9, 13, 11, 14, 15
EndDataSection

Dim colors(7)
For c = 0 To 7
  Read.i colors(c)
Next 

If OpenConsole()
  ;The console display is 80 columns wide by 25 rows
  For r = 0 To 24
    For c = 0 To 7 
      ConsoleColor(colors(c), colors(c))
      Print(Space(80 / 8))
    Next
  Next
  EnableGraphicalConsole(1)
  ConsoleLocate(0, 0)
  
  ConsoleTitle("Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```


=
## Run BASIC
=

```runbasic
colors$ = "black,red,green,blue,magenta,cyan,yellow,white"
html "<TABLE BORDER=1 CELLPADDING=0 CELLSPACING=4><tr height=70>"
for i = 1 to 8
  html "<td width=20 bgcolor='";word$(colors$,i,",");"'</td>"
next i
html "</tr></table>"
```


```txt
Output
```


[[File:ColorBarRunBasic.png]]

=
## TI Basic
=
<LANG>100 REM  SMPTE Format Color Bars
110 REM  with 400 Hz reference tone
120 CALL CLEAR
130 CALL SCREEN(2)
140 CALL COLOR(8,15,1)
150 CALL COLOR(9,11,1)
160 CALL COLOR(10,8,1)
170 CALL COLOR(11,13,1)
180 CALL COLOR(12,14,1)
190 CALL COLOR(13,10,1)
200 CALL COLOR(14,5,1)
210 CALL COLOR(15,2,1)
220 CALL COLOR(16,16,1)
230 GOSUB 510
240 REM  color bars
250 CALL VCHAR(1,3,88,96)
260 CALL VCHAR(1,7,96,96)
270 CALL VCHAR(1,11,104,96)
280 CALL VCHAR(1,15,112,96)
290 CALL VCHAR(1,19,120,96)
300 CALL VCHAR(1,23,128,96)
310 CALL VCHAR(1,27,136,96)
320 REM  BLACK BAR
330 CALL HCHAR(19,1,144,224)
340 REM  WHITE BOX
350 FOR I=19 TO 24
360 CALL HCHAR(I,8,152,6)
370 NEXT I
410 REM  SMPTE Complementary boxes
420 CALL HCHAR(18,3,136,4)
430 CALL HCHAR(18,7,144,4)
440 CALL HCHAR(18,11,120,4)
450 CALL HCHAR(18,15,144,4)
460 CALL HCHAR(18,19,104,4)
470 CALL HCHAR(18,23,144,4)
480 CALL HCHAR(18,27,88,4)
490 CALL SOUND(3000,400,12)
500 GOTO 490
510 SQUARE$="FFFFFFFFFFFFFFFF"
520 FOR I = 88 TO 152 STEP 8
530 CALL CHAR(I,SQUARE$)
540 NEXT I
590 RETURN
</LANG>

=
## ZX Spectrum Basic
=

```zxbasic
10 REM The ZX Spectrum display is 32 columns wide, so we have 8 columns of 4 spaces
20 FOR r=0 TO 20: REM There are 21 rows
30 FOR c=0 TO 7: REM We use the native colour sequence here
40 PAPER c: REM set the background colour for the spaces to be printed
50 PRINT "    ";: REM four spaces, the semicolon prevents newline
60 NEXT c
70 REM at this point the cursor has wrapped, so we don't need a newline
80 NEXT r
```



## Befunge

Assuming a terminal with support for ANSI escape sequences, this fills the screen with the colour sequence: black, red, green, blue, magenta, cyan, yellow and white. It then waits for the user to press ''Enter'' before terminating.

```befunge
<v%"P":          <<*"(2"
v_:"P"/"["39*,, :55+/68v
v,,,";1H" ,+*86%+55 ,+*<
73654210v,,\,,,*93"[4m"<
>$:55+%#v_:1-"P"%55+/3g^
39*,,,~@>48*,1-:#v_$"m["
```



## C

This task requires functionality which allows the code to communicate to the video device. This will vary from vendor to vendor. The following examples show two ways of doing this, in the text and graphics mode, using Borland's Turbo C.


### Text Mode

The required functions and structures are in conio.h

```C

#include<conio.h>

#define COLOURS 8

int main()
{
	int colour=0,i,j,MAXROW,MAXCOL;
	struct text_info tInfo;
	gettextinfo(&tInfo);
	MAXROW = tInfo.screenheight;
	MAXCOL = tInfo.screenwidth;
	textbackground(BLACK);     //8 colour constants are defined
	clrscr();
	
	for(colour=0;colour<COLOURS;colour++)
	{
		getch();                              //waits for a key hit
		gotoxy(1+colour*MAXCOL/COLOURS,1);
		textbackground(colour);
		for(j=0;j<MAXROW;j++){
			for(i=0;i<MAXCOL/COLOURS;i++){
				cprintf(" ");
			}
		gotoxy(1+colour*MAXCOL/COLOURS,1+j);
		}
	}

	getch();
	textbackground(BLACK);

	return 0;
}

```



### Graphics Mode

The required functions and structures are in graphics.h, conio.h is included for getch().

```C

#include<graphics.h>
#include<conio.h>

int main()
{
	int d=DETECT,m,maxX,maxY,maxColours,i;
	initgraph(&d,&m,"c:/turboc3/bgi");
	maxX = getmaxx();
	maxY = getmaxy();
	maxColours = getmaxcolor();

	for(i=0;i<maxColours;i++)
	{
		setfillstyle(SOLID_FILL,i);
		bar(i*maxX/maxColours,0,(i+1)*maxX/maxColours,maxY);
	}

	getch();
	closegraph();
	
	return 0;
}

```



## C++

using Qt 4.6

file <code>colorbars.h</code>:


```cpp
#ifndef MYWIDGET_H
#define MYWIDGET_H
#include <QWidget>

class QPaintEvent ;

class MyWidget : public QWidget {
public :
   MyWidget( ) ;

protected :
   void paintEvent( QPaintEvent * ) ;
private :
   int width ;
   int height ;
   const int colornumber ;
} ;
#endif
```


file <code>colorbars.cpp</code>:


```cpp>#include <QtGui

#include "colorbars.h"

MyWidget::MyWidget( ) :
   width( 640 ) ,
   height( 240 ) ,
   colornumber( 8 ) {
      setGeometry( 0, 0 , width , height ) ;
}

void MyWidget::paintEvent ( QPaintEvent * ) {
   int rgbtriplets[ ] = { 0 , 0 , 0 , 255 , 0 , 0 , 0 , 255 , 0 , 
      0 , 0 , 255 , 255 , 0 , 255 , 0 , 255 , 255 , 255 , 255 , 0 ,
      255 , 255 , 255 } ; 
   QPainter myPaint( this ) ;
   int rectwidth = width / colornumber ; //width of one rectangle
   int xstart = 1 ; //x coordinate of the first rectangle
   int offset = -1  ; //to allow for ++offset to define the red value even in the first run of the loop below
   for ( int i = 0 ; i < colornumber ; i++ ) {
      QColor rectColor ;
      rectColor.setRed( rgbtriplets[ ++offset ] ) ;
      rectColor.setGreen( rgbtriplets[ ++offset ] ) ;
      rectColor.setBlue( rgbtriplets[ ++offset ] ) ;
      myPaint.fillRect( xstart , 0 , rectwidth , height - 1 , rectColor ) ;
      xstart += rectwidth + 1 ;
   }
}
```


file <code>main.cpp</code>:


```cpp>#include <QApplication

#include "colorbars.h"

int main( int argc, char * argv[ ] ) {
   QApplication app( argc , argv ) ;
   MyWidget window ;
   window.setWindowTitle( QApplication::translate( "colorslides" , "color slides demonstration" ) ) ;
   window.show( ) ;
   return app.exec( ) ;
}
```



## COBOL

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. terminal-colour-bars.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  width           PIC 9(3).
       01  height          PIC 9(3).

       01  interval        PIC 9(3).

       01  colours-area.
           03  colour-values.
               05  FILLER  PIC 9 VALUE 0. *> Black
               05  FILLER  PIC 9 VALUE 4. *> Red
               05  FILLER  PIC 9 VALUE 2. *> Green
               05  FILLER  PIC 9 VALUE 1. *> Blue
               05  FILLER  PIC 9 VALUE 5. *> Magneta
               05  FILLER  PIC 9 VALUE 3. *> Cyan
               05  FILLER  PIC 9 VALUE 6. *> Yellow
               05  FILLER  PIC 9 VALUE 7. *> White

           03  colour-table REDEFINES colour-values.
               05  colours PIC 9 OCCURS 8 TIMES INDEXED BY colour-index.

       01  i               PIC 9(3).
       01  j               PIC 9(3).

       PROCEDURE DIVISION.
           ACCEPT width FROM COLUMNS
           ACCEPT height FROM LINES
           DIVIDE width BY 8 GIVING interval

           PERFORM VARYING i FROM 1 BY 1 UNTIL height < i
               PERFORM VARYING j FROM 1 BY 1 UNTIL width < j
                  COMPUTE colour-index = (j / interval) + 1
                  
                  IF 8 < colour-index
                      SET colour-index TO 8
                  END-IF

                  *> Some colours come a bit darker than they
                  *> should, with the yellow being orange and the white
                  *> being light-grey.
                  DISPLAY SPACE AT LINE i COLUMN j
                      WITH BACKGROUND-COLOR colours (colour-index)
               END-PERFORM
           END-PERFORM

           ACCEPT i *> Prevent ncurses returning to console immediately.

           GOBACK
           .
```



## EasyLang


[https://easylang.online/apps/run.html?bg=eee&code=col%5B%5D%20%3D%20%5B%20000%20900%20090%20909%20099%20990%20999%20%5D%0Aw%23%20%3D%20100.0%20/%20len%20col%5B%5D%0Afor%20i%20range%20len%20col%5B%5D%0Acolor%20col%5Bi%5D%0Amove%20w%23%20%2A%20i%200%0Arect%20w%23%20100%0A. Run it]


```easyprog.online
col[] = [ 000 900 090 909 099 990 999 ]
w# = 100.0 / len col[]
for i range len col[]
  color col[i]
  move w# * i 0
  rect w# 100
.
```



## Factor


```factor
USING: accessors colors.constants kernel math sequences ui
ui.gadgets ui.gadgets.tracks ui.pens.solid ;
IN: rosetta-code.colour-bars-display

: colors ( -- ) [
horizontal <track>
{
 COLOR: black
 COLOR: red
 COLOR: green
 COLOR: blue
 COLOR: magenta
 COLOR: cyan
 COLOR: yellow
 COLOR: white
}
[ <solid> gadget new swap >>interior ] map
dup length recip
[ track-add ] curry each
{ 640 480 } >>pref-dim
"bars" open-window
] with-ui ;
MAIN: colors
```



## Forth

This program extends ANS Forth to control the TMS9918 Video display processor in the TI-99 computer.

 The color bars are shown in Society of Motion Picture and Television Engineers (SMPTE) order.


```CAMEL99 Forth

\ Color Bars for TI-99 CAMEL99 Forth

NEEDS HCHAR   FROM DSK1.GRAFIX    \ TMS9918 control lexicon
NEEDS CHARSET FROM DSK1.CHARSET   \ restores default character data
NEEDS ENUM    FROM DSK1.ENUM      \ add simple enumerator to Forth

\ Name TI-99 colors
1 ENUM CLR     ENUM BLK    ENUM MGRN   ENUM LGRN
  ENUM BLU     ENUM LBLU   ENUM RED    ENUM CYAN
  ENUM MRED    ENUM LRED   ENUM YEL    ENUM LYEL
  ENUM GRN     ENUM MAG    ENUM GRY    ENUM WHT
DROP

\ square character data
HEX
CREATE SQUARE  FFFF , FFFF , FFFF , FFFF ,

DECIMAL
: COLOR-BARS ( -- )
   24 0 DO
\   col row char wid
\   --- --- ---- ---
      2  I   88   4 HCHAR
      6  I   96   4 HCHAR
     10  I  104   4 HCHAR
     14  I  112   4 HCHAR
     18  I  120   4 HCHAR
     22  I  128   4 HCHAR
     26  I  136   4 HCHAR
   LOOP ;

DECIMAL
: DEFCHARS ( pattern first last -- )
        1+ SWAP ?DO   DUP I CHARDEF  8 +LOOP DROP ;

: SET-COLORS ( -- )
\   charset  fg   bg
\   -------  --   --
    88 SET#  GRY  CLR COLOR
    96 SET#  YEL  CLR COLOR
   104 SET#  CYAN CLR COLOR
   112 SET#  GRN  CLR COLOR
   120 SET#  MAG  CLR COLOR
   128 SET#  RED  CLR COLOR
   136 SET#  BLU  CLR COLOR
   144 SET#  BLK  CLR COLOR ;

\ restore characters and colors
: DEFAULTS   
   8 SCREEN
   4 19 BLK CLR COLORS
   CLEAR
   CHARSET ;

: BARS
   CLEAR  BLK SCREEN
   SET-COLORS
   SQUARE 88 152 DEFCHARS
   COLOR-BARS
   BEGIN ?TERMINAL  UNTIL
   DEFAULTS
;

CR .( Done. Type BARS to run)
</LANG>


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Draw the color bars on an 80 x 25 console using the system palette of 16 colors
' i.e. 5 columns per color
Width 80, 25
Shell "cls"
Locate ,, 0  '' turn cursor off
For clr As UInteger = 0 To 15
  Color 0, clr
  For row As Integer = 1 to 25
    Locate row, clr * 5 + 1 
    Print Space(5);
  Next row
Next clr

Sleep
' restore default settings
Locate ,, 1 '' turn cursor on
Color 7, 0  '' white text on black background
```



## Gambas


```gambas
Public Sub Form_Open()
Dim iColour As Integer[] = [Color.Black, Color.red, Color.Green, Color.Magenta, Color.Cyan, Color.Yellow, Color.white]
Dim hPanel As Panel
Dim siCount As Short

With Me
  .Arrangement = Arrange.Horizontal
  .Height = 300
  .Width = 400
End With

For siCount = 0 To 6
  hpanel = New Panel(Me)
  hpanel.Expand = True
  hpanel.H = 500
  HPanel.Background = iColour[siCount]
Next

End
```


## Go

{{libheader|Go Graphics}}

```go
package main

import "github.com/fogleman/gg"

var colors = [8]string{
    "000000", // black
    "FF0000", // red
    "00FF00", // green
    "0000FF", // blue
    "FF00FF", // magenta
    "00FFFF", // cyan
    "FFFF00", // yellow
    "FFFFFF", // white
}

func drawBars(dc *gg.Context) {
    w := float64(dc.Width() / len(colors))
    h := float64(dc.Height())
    for i := range colors {
        dc.SetHexColor(colors[i])
        dc.DrawRectangle(w*float64(i), 0, w, h)
        dc.Fill()
    }
}

func main() {
    dc := gg.NewContext(400, 400)
    drawBars(dc)
    dc.SavePNG("color_bars.png")
}
```


{{out}}

```txt

Image similar to R entry.

```



## Haskell


Terminal-based version.


```haskell
#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package vty -- -threaded

import Graphics.Vty

colorBars :: Int -> [(Int, Attr)] -> Image
colorBars h bars = horizCat $ map colorBar bars
  where colorBar (w, attr) = charFill attr ' ' w h

barWidths :: Int -> Int -> [Int]
barWidths nBars totalWidth = map barWidth [0..nBars-1]
  where fracWidth = fromIntegral totalWidth / fromIntegral nBars
        barWidth n =
          let n' = fromIntegral n :: Double
          in floor ((n' + 1) * fracWidth) - floor (n' * fracWidth)

barImage :: Int -> Int -> Image
barImage w h = colorBars h $ zip (barWidths nBars w) attrs
  where attrs = map color2attr colors
        nBars = length colors
        colors = [black, brightRed, brightGreen, brightMagenta, brightCyan, brightYellow, brightWhite]
        color2attr c = Attr Default Default (SetTo c)

main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    let output = outputIface vty
    bounds <- displayBounds output
    let showBars (w,h) = do
          let img = barImage w h
              pic = picForImage img
          update vty pic
          e <- nextEvent vty
          case e of
            EvResize w' h' -> showBars (w',h')
            _ -> return ()
    showBars bounds
    shutdown vty
```


Graphical version using SFML.


```haskell
-- Before you can install the SFML Haskell library, you need to install
-- the CSFML C library.  (For example, "brew install csfml" on OS X.)

-- This program runs in fullscreen mode.
-- Press any key or mouse button to exit.

import Control.Exception
import SFML.Graphics
import SFML.SFResource
import SFML.Window hiding (width, height)

withResource :: SFResource a => IO a -> (a -> IO b) -> IO b
withResource acquire = bracket acquire destroy

withResources :: SFResource a => IO [a] -> ([a] -> IO b) -> IO b
withResources acquire = bracket acquire (mapM_ destroy)

colors :: [Color]
colors = [black, red, green, magenta, cyan, yellow, white]

makeBar :: (Float, Float) -> (Color, Int) -> IO RectangleShape
makeBar (barWidth, height) (c, i) = do
  bar <- err $ createRectangleShape
  setPosition  bar $ Vec2f (fromIntegral i * barWidth) 0
  setSize      bar $ Vec2f barWidth height
  setFillColor bar c
  return bar

barSize :: VideoMode -> (Float, Float)
barSize (VideoMode w h _ ) = ( fromIntegral w / fromIntegral (length colors)
                             , fromIntegral h )

loop :: RenderWindow -> [RectangleShape] -> IO ()
loop wnd bars = do
  mapM_ (\x -> drawRectangle wnd x Nothing) bars
  display wnd
  evt <- waitEvent wnd
  case evt of
    Nothing -> return ()
    Just SFEvtClosed -> return ()
    Just (SFEvtKeyPressed {}) -> return ()
    Just (SFEvtMouseButtonPressed {}) -> return ()
    _ -> loop wnd bars

main :: IO ()
main = do
  vMode <- getDesktopMode
  let wStyle = [SFFullscreen]
  withResource (createRenderWindow vMode "color bars" wStyle Nothing) $
     \wnd -> withResources (mapM (makeBar $ barSize vMode) $ zip colors [0..]) $
     \bars -> loop wnd bars
```


=={{header|Icon}} and {{header|Unicon}}==
The procedure below is generalized to take a description of a ''test card'' and display it.
[[File:Colourbars_Simple_Unicon.png|thumb|right]]

```Icon
link graphics,printf
                     
procedure main()  # generalized colour bars 
   DrawTestCard(Simple_TestCard())
   WDone()
end   
   
procedure DrawTestCard(TC)
   size := sprintf("size=%d,%d",TC.width,TC.height)
   &window := TC.window := open(TC.id,"g","bg=black",size) | 
               stop("Unable to open window")   

   every R := TC.bands[r := 1 to *TC.bands -1] do
      every C := R.bars[c := 1 to *R.bars - 1] do {
	     Fg(R.bars[c].colour)
	     FillRectangle( C.left, R.top,
		            R.bars[c+1].left-C.left, TC.bands[r+1].top-R.top )
	     }
   return TC
end

record testcard(window,id,width,height,bands)
record band(top,bars)    
record bar(left,colour)   

procedure Simple_TestCard()  #: return structure simple testcard
   return testcard(,"Simple Test Card",width := 800,height := 600, 
		   [   band( 1, [ bar(  1, "black"),
				  bar(114, "red"),
				  bar(228, "green"),
				  bar(342, "blue"),
                                  bar(456, "magenta"),
                                  bar(570, "cyan"),
                                  bar(684, "yellow"),
                                  bar(width) ] ),
		       band(height) ])
end
```
 

The following example is a wee tiny bit more interesting.  
[[File:Colourbars_SMPTE_Unicon.png|thumb|right]]

```Icon
procedure SMPTE_TestCard()  #: return structure with 480i(ish) testcard
   return testcard(,"SMPTE TV Test Card",width := 672,height := 504, 
		   [   band(  1, [ bar(  1, "#c0c0c0"),
				   bar( 95, "#c0c000"),
   	    		           bar(191, "#00c0c0"),
				   bar(288, "#00c000"),
				   bar(383, "#c000c0"),
				   bar(480, "#c00000"),
				   bar(575, "#0000c0"),
				   bar(width) ] ),
		       band(335, [ bar(  1, "#0000c0"),
				   bar( 95, "#131313"),
				   bar(191, "#c000c0"),
				   bar(288, "#131313"),
				   bar(383, "#00c0c0"),
				   bar(480, "#131313"),
				   bar(575, "#c0c0c0"),
			           bar(width) ] ),
		       band(378, [ bar(  1, "#00214c"), 
				   bar(120, "#ffffff"),
				   bar(240, "#32006a"), 
				   bar(360, "#131313"), 
				   bar(480, "#090909"), 
				   bar(512, "#131313"), 
				   bar(544, "#1d1d1d"), 
				   bar(576, "#131313"),    
				   bar(width) ] ),
			band(height) ])
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/graphics.icn graphics.icn provides graphics] 
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides sprintf]


## J



```j
   load 'viewmat'
   size=: 2{.".wd'qm' NB. J6
   size=: getscreenwh_jgtk_ '' NB. J7
   'rgb'viewmat (|.size){. (>.&.(%&160)|.size)$ 20# 256#.255*#:i.8
```


Note: You need to pick an implementation of <code>size</code> based on the version of J you are using.


## Java


```java

import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JFrame;

public class ColorFrame extends JFrame {
	public ColorFrame(int width, int height) {
		this.setDefaultCloseOperation(EXIT_ON_CLOSE);
		this.setSize(width, height);
		this.setVisible(true);
	}

	@Override
	public void paint(Graphics g) {
		Color[] colors = { Color.black, Color.red, Color.green, Color.blue,
				Color.pink, Color.CYAN, Color.yellow, Color.white };

		for (int i = 0; i < colors.length; i++) {
			g.setColor(colors[i]);
			g.fillRect(this.getWidth() / colors.length * i, 0, this.getWidth()
					/ colors.length, this.getHeight());
		}
	}

	public static void main(String args[]) {
		new ColorFrame(200, 200);
	}
}

```



## Julia

{{works with|Julia|0.6}}


```julia
using Images

colors = [colorant"black", colorant"red", colorant"green", colorant"darkblue",
          colorant"purple", colorant"blue", colorant"yellow", colorant"white"]
wcol = 60 # width of each color bar
h, w = 150, wcol * length(colors) + 1
img = Matrix{RGB{N0f8}}(h, w);
for (j, col) in zip(1:wcol:w, colors)
    img[:, j:j+wcol] = col
end
save("data/colourbars.jpg", img)
```



## Kotlin

{{trans|Java}}

```scala
import java.awt.Color
import java.awt.Graphics
import javax.swing.JFrame

class ColorFrame(width: Int, height: Int): JFrame() {
    init {
        defaultCloseOperation = EXIT_ON_CLOSE
        setSize(width, height)
        isVisible = true
    }

    override fun paint(g: Graphics) {
        val colors = listOf(Color.black, Color.red,  Color.green,  Color.blue,
		            Color.pink,  Color.cyan, Color.yellow, Color.white)
        val size = colors.size
        for (i in 0 until size) {
            g.color = colors[i]
            g.fillRect(width / size * i, 0, width / size, height)
        }
    }
}

fun main(args: Array<String>) {
    ColorFrame(400, 400)
}
```

Editing Babbage problem


## M2000 Interpreter

Calling a module can be done by using name or call name. The later used for recursive call. Here  we use it for beautify the code.

```M2000 Interpreter

Module Checkit {
      Module Bars {
            barwidth=x.twips div 8
            barheight=y.twips
            barcolors=(0,#ff0000,#00ff00, #0000ff, #FF00FF, #00ffff, #ffff00, #ffffff)
            For i=0 to 7
                  Move i*barwidth, 0
                  \\ gradient fill. Here second color are the same as first color
                  Fill barwidth, barheight, array(barcolors, i), array(barcolors, i)
            Next i
      }
      \\ first draw on console
      Call Bars
      Declare Form1 Form
      Layer Form1 {
            window 12, 10000,8000;
            \\ now draw on Form1 layer, above console, in a window
            Call Bars
      }
      Method Form1, "Show", 1 ' open modal
      Declare Form1 Nothing
}
Checkit

```




## Maple


```Maple

with(plottools):
plots:-display([rectangle([0, 0], [.3, 2.1], color = black), rectangle([.3, 0], [.6, 2.1], color = red), rectangle([.6, 0], [.9, 2.1], color = green), rectangle([.9, 0], [1.2, 2.1], color = magenta), rectangle([1.2, 0], [1.5, 2.1], color = cyan), rectangle([1.5, 0], [1.8, 2.1], color = white), rectangle([1.8, 0], [2.1, 2.1], color = yellow)])

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==


```mathematica
ArrayPlot[
 ConstantArray[{Black, Red, Green, Blue, Magenta, Cyan, Yellow, 
   White}, 5]]
```


[[File:ColourBarsMathematica.png]]


## OCaml



```ocaml
open Graphics

let round x =
  int_of_float (floor (x +. 0.5))

let () =
  open_graph "";
  let cols = size_x () in
  let rows = size_y () in
  let colors = [| black; red; green; blue; magenta; cyan; yellow; white |] in
  let n = Array.length colors in
  let bar_width = (float cols) /. (float n) in
  Array.iteri (fun i color ->
    let x1 = bar_width *. (float i) in
    let x2 = bar_width *. (float (succ i)) in
    set_color color;
    fill_rect (round x1) 0 (round x2) rows;
  ) colors;
  ignore (read_key ());
;;
```


execute with:
 $ ocaml graphics.cma display_colour_bars.ml


## Perl


```Perl
#!/usr/bin/perl -w
use strict ;
use GD ;

my %colors = ( white => [ 255 , 255 , 255 ] , red => [255 , 0 , 0 ] ,
      green => [ 0 , 255 , 0 ] , blue => [ 0 , 0 , 255 ] , 
      magenta => [ 255 , 0 , 255 ] , yellow => [ 255 , 255 , 0 ] ,
      cyan => [ 0 , 255 , 255 ] , black => [ 0 , 0 , 0 ] ) ;
my $barwidth = 160 / 8 ;
my $image = new GD::Image( 160 , 100 ) ;
my $start = 0 ;
foreach my $rgb ( values %colors ) {
   my $paintcolor = $image->colorAllocate( @$rgb ) ; 
   $image->filledRectangle( $start * $barwidth , 0 , $start * $barwidth + 
	 $barwidth - 1 , 99 , $paintcolor ) ;
   $start++ ;
}
open ( DISPLAY , ">" , "testprogram.png" ) || die ;
binmode DISPLAY ;
print DISPLAY $image->png ;
close DISPLAY ;#to be watched with <image viewer> testprogram.png
```


## Perl 6

{{works with|Rakudo|2018.10}}

```perl6
my ($x,$y) = 1280, 720;

my @colors = map -> $r, $g, $b { Buf.new: |(($r, $g, $b) xx $x div 8) },
      0,   0,   0,
    255,   0,   0,
      0, 255,   0,
      0,   0, 255,
    255,   0, 255,
      0, 255, 255,
    255, 255,   0,
    255, 255, 255;

my $img = open "colorbars.ppm", :w orelse die "Can't create colorbars.ppm: $_";

$img.print: qq:to/EOH/;
    P6
    # colorbars.ppm
    $x $y
    255
    EOH

for ^$y {
    for ^@colors -> $h {
        $img.write: @colors[$h];
    }
}

$img.close;
```



## Phix

{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\Colour_bars.exw
--
include pGUI.e

constant colours = {CD_BLACK, CD_RED, CD_GREEN, CD_MAGENTA, CD_CYAN, CD_YELLOW, CD_WHITE}

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    cdCanvasActivate(cddbuffer)
    integer {width, height} = IupGetIntInt(canvas, "DRAWSIZE")
    integer x = 0, lc = length(colours)
    for i=1 to lc do
        integer w = floor((width-x)/(lc-i+1))
        cdCanvasSetForeground(cddbuffer, colours[i])
        cdCanvasBox(cddbuffer, x, x+w, 0, height)
        x += w
    end for
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    return IUP_DEFAULT
end function

function unmap_cb(Ihandle /*ih*/)
    cdKillCanvas(cddbuffer)
    cdKillCanvas(cdcanvas)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "600x400") -- initial size

    IupSetCallback(canvas, "MAP_CB",    Icallback("map_cb"))
    IupSetCallback(canvas, "UNMAP_CB",  Icallback("unmap_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Colour bars")
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))

    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL) -- release the minimum limitation

    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)

    IupMainLoop()

    IupClose()
end procedure

main()
```



## PHP

{{trans|Perl}}
Will output result directly to the browser. Use it as CGI/BIN script.

```PHP
<?php
$colors = array(array(  0,   0,   0),   // black
                array(255,   0,   0),   // red
                array(  0, 255,   0),   // green
                array(  0,   0, 255),   // blue
                array(255,   0, 255),   // magenta
                array(  0, 255, 255),   // cyan
                array(255, 255,   0),   // yellow
                array(255, 255, 255));  // white

define('BARWIDTH', 640 / count($colors));
define('HEIGHT',   480);

$image = imagecreate(BARWIDTH * count($colors), HEIGHT);

foreach ($colors as $position => $color) {
    $color = imagecolorallocate($image, $color[0], $color[1], $color[2]);
    imagefilledrectangle($image, $position * BARWIDTH, 0,
                         $position * BARWIDTH + BARWIDTH - 1,
                         HEIGHT - 1, $color);
}

header('Content-type:image/png');
imagepng($image);
imagedestroy($image);
```


Alternately, with HTML output:


```PHP
<?php
$colors = array(
                "000000",   // black
                "FF0000",   // red
                "00FF00",   // green
                "0000FF",   // blue
                "FF00FF",   // magenta
                "00FFFF",   // cyan
                "FFFF00",   // yellow
                "FFFFFF",   // white
                );

echo '<table style="border: 1px solid black; border-spacing: 0;"><tr>';
foreach ($colors as $color) {
    echo '<td style="background-color: #'.$color.'; height: 100px; width: 20px;"></td>';
}
echo '</tr></table>';
```



## PicoLisp

{{trans|UNIX Shell}}

```PicoLisp
(call 'clear)

(let Width (in '(tput cols) (read))
   (do (in '(tput lines) (read))
      (for B (range 0 7)
         (call 'tput 'setab B)
         (space (/ Width 8)) )
      (prinl) ) )

(call 'tput 'sgr0)   # reset
```



## PowerShell


```PowerShell

[string[]]$colors = "Black"   , "DarkBlue"   , "DarkGreen" , "DarkCyan",
                    "DarkRed" , "DarkMagenta", "DarkYellow", "Gray",
                    "DarkGray", "Blue"       , "Green"     , "Cyan",
                    "Red"     , "Magenta"    , "Yellow"    , "White"

for ($i = 0; $i -lt 64; $i++)
{ 
    for ($j = 0; $j -lt $colors.Count; $j++)
    { 
        Write-Host (" " * 12) -BackgroundColor $colors[$j] -NoNewline
    }

    Write-Host
}

```



## Python


```Python

#!/usr/bin/env python
#vertical coloured stripes in window in Python 2.7.1

from livewires import *

horiz=640; vert=480
begin_graphics(width=horiz,height=vert,title="v_stripes",background=Colour.black)
NameColors=["black","red","green","dark_blue","purple","blue","yellow","white"]
stepik=horiz/len(NameColors)

for index,each in enumerate(NameColors):
	ExcStrng="set_colour(Colour."+each+")"
	exec ExcStrng
	box(index*stepik,0,(index+1)*stepik,vert,filled=1)

while keys_pressed() != ['x']: # press x key to terminate program
	pass

end_graphics()

```



## R

Create the color palette, set margins to zero so the image will fill the display, and use image to create the graphic:
[[File:ColorBarR.png|thumb|right]]

```R

pal <- c("black", "red", "green", "blue", "magenta", "cyan", "yellow", "white")
par(mar = rep(0, 4)) 
image(matrix(1:8), col = pal, axes = FALSE)

```



## Racket



```Racket

#lang racket/gui

(define-values [W H] (get-display-size #t))

(define colors
  '("Black" "Red" "Green" "Blue" "Magenta" "Cyan" "Yellow" "White"))

(define (paint-pinstripe canvas dc)
  (send dc set-pen "black" 0 'transparent)
  (for ([x (in-range 0 W (/ W (length colors)))] [c colors])
    (send* dc (set-brush c 'solid) (draw-rectangle x 0 W H))))

(define full-frame%
  (class frame%
    (define/override (on-subwindow-char r e)
      (when (eq? 'escape (send e get-key-code))
        (send this show #f)))
    (super-new
     [label "Color bars"] [width W] [height H]
     [style '(no-caption no-resize-border hide-menu-bar no-system-menu)])
    (define c (new canvas% [parent this] [paint-callback paint-pinstripe]))
    (send this show #t)))

(void (new full-frame%))

```



## REXX

{{works with|PC REXX}}
{{works with|Personal REXX}}
{{works with|R4}}
{{works with|ROO}}



Programming note:   because of the way the REXX interpreters   (being used for this example)   ensure screen output fidelity,   if ninety characters are displayed on a ninety-byte wide screen,   REXX apparently forces an extra blank,   causing to what appears to be a blank line after the line displayed.   Because of this,   the last color bar   ('''_.8''')   has been shortened by one byte.


```rexx
/*REXX program  displays  eight colored vertical bars  on a full screen.                */
parse  value  scrsize()  with sd sw .                 /*the screen depth and width.     */
barWidth=sw%8                                         /*calculate the bar width.        */
_.=copies('db'x, barWidth)                            /*the bar, full width.            */
_.8=left(_.,barWidth-1)                               /*the last bar width,  less one.  */
    $ = x2c('1b5b73')  ||  x2c("1b5b313b33376d")      /* the preamble,  and the header. */
hdr.1 = x2c('1b5b303b33306d')                         /*  "  color black.               */
hdr.2 = x2c('1b5b313b33316d')                         /*  "  color red.                 */
hdr.3 = x2c('1b5b313b33326d')                         /*  "  color green.               */
hdr.4 = x2c('1b5b313b33346d')                         /*  "  color blue.                */
hdr.5 = x2c('1b5b313b33356d')                         /*  "  color magenta.             */
hdr.6 = x2c('1b5b313b33366d')                         /*  "  color cyan.                */
hdr.7 = x2c('1b5b313b33336d')                         /*  "  color yellow.              */
hdr.8 = x2c('1b5b313b33376d')                         /*  "  color white.               */
 tail = x2c('1b5b751b5b303b313b33363b34303b306d')     /*  "  epilogue,  and the trailer.*/
                                                      /* [↓]  last bar width is shrunk. */
       do j=1  for 8                                  /*build the line, color by color. */
       $=$ || hdr.j || _.j                            /*append the color header + bar.  */
       end   /*j*/                                    /* [↑]  color order is the list.  */
                                                      /* [↓]  the tail is overkill.     */
$=$ || tail                                           /*append the epilogue (trailer).  */
                                                      /* [↓]  show full screen of bars. */
       do k=1  for sd                                 /*SD = screen depth (from above). */
       say $                                          /*have REXX display line of bars. */
       end   /*k*/                                    /* [↑]  Note:  SD  could be zero. */
                                                      /*stick a fork in it, we're done. */
```

This REXX program makes use of   '''scrsize'''   REXX program (or BIF) which is used to determine the screen size of the terminal (console). 

The   '''SCRSIZE.REX'''   REXX program is included here   ──►   [[SCRSIZE.REX]]. 



'''output'''   appears identical to the output for   '''Icon and Unicon''',   '''Mathematica''',   and   '''R'''   examples.


## Ring


```ring

load "guilib.ring"

new qapp 
        {
        win1 = new qwidget() {
               setwindowtitle("drawing using qpainter")
               setwinicon(self,"C:\Ring\bin\image\browser.png")
               setgeometry(100,100,500,600)
               label1 = new qlabel(win1) {
                        setgeometry(10,10,400,400)
                        settext("")
               }
               new qpushbutton(win1) {
                   setgeometry(200,400,100,30)
                   settext("draw")
                   setclickevent("draw()")
               }
               show()
        }
        exec()
        }

func draw
     p1 = new qpicture()
          color = new qcolor() {
          setrgb(0,0,255,255)
     }
     pen = new qpen() {
           setcolor(color)
           setwidth(1)
     }
     new qpainter() {
         begin(p1)
         setpen(pen)

     //Black, Red, Green, Blue, Magenta, Cyan, Yellow, White
          
     for n = 1 to 8
         color2 = new qcolor(){
         switch n
                on 1 r=0 g=0 b=0
                on 2 r=255 g=0 b=0
                on 3 r=0 g=255 b=0
                on 4 r=0 g=0 b=255
                on 5 r=255 g=0 b=255
                on 6 r=0 g=255 b=255
                on 7 r=255 g=255 b=0
                on 8 r=255 g=255 b=255
           off
           setrgb(r,g,b,255)
           }
           mybrush = new qbrush() {setstyle(1) setcolor(color2)}
           setbrush(mybrush)
           drawrect(n*25,25,25,70)
     next

     endpaint()
     }
     label1 { setpicture(p1) show() }

```

Output :
[https://lh3.googleusercontent.com/-_y0FSmg0NAs/V1xBPvGV2tI/AAAAAAAAAKk/TXYSJnpdZ346aQLd05yE9vLM8V9lEht1QCLcB/s1600/CalmoSoftColourBars.jpg Colour bars]


## Scala


```scala
import java.awt.Color
import scala.swing._

class ColorBars extends Component {
  override def paintComponent(g:Graphics2D)={
    val colors=List(Color.BLACK, Color.RED, Color.GREEN, Color.BLUE, Color.MAGENTA, Color.CYAN, Color.YELLOW, Color.WHITE)
    val colCount=colors.size
    val deltaX=size.width.toDouble/colCount
    for(x <- 0 until colCount){
      val startX=(deltaX*x).toInt
      val endX=(deltaX*(x+1)).toInt
      g.setColor(colors(x))
      g.fillRect(startX, 0, endX-startX, size.height)
    }
  }
}
```

Open window:
[[File:colorbars_scala.png|thumb|right]]

```scala
new MainFrame(){
  title="Color bars"
  visible=true
  preferredSize=new Dimension(640, 320)
  contents=new ColorBars()
}
```



## Sidef

{{trans|Perl}}

```ruby
require('GD');

var colors = Hash.new(
              white   => [255, 255, 255],
              red     => [255, 0,   0],
              green   => [0,   255, 0],
              blue    => [0,   0,   255],
              magenta => [255, 0,   255],
              yellow  => [255, 255, 0],
              cyan    => [0,   255, 255],
              black   => [0,   0,   0],
             );

var barwidth = 160/8;
var image    = %s'GD::Image'.new(160, 100);
var start    = 0;

colors.values.each { |rgb|
    var paintcolor = image.colorAllocate(rgb...);
    image.filledRectangle(start * barwidth, 0, start*barwidth + barwidth - 1, 99, paintcolor);
    start++;
};

%f'colorbars.png'.open('>:raw').print(image.png);
```



## SmileBASIC


```smilebasic
FOR I=0 TO 7
 READ R,G,B
 GFILL I*50,0,I*50+49,239,RGB(R,G,B)
NEXT
REPEAT UNTIL BUTTON(0) AND #B

DATA 0,0,0
DATA 255,0,0
DATA 0,255,0
DATA 0,0,255
DATA 255,0,255
DATA 0,255,255
DATA 255,255,0
DATA 255,255,255
```



## Tcl

{{libheader|Tk}}

```tcl
package require Tcl 8.5
package require Tk 8.5

wm attributes . -fullscreen 1
pack [canvas .c -highlightthick 0] -fill both -expand 1
set colors {black red green blue magenta cyan yellow white}

for {set x 0} {$x < [winfo screenwidth .c]} {incr x 8} {
    .c create rectangle $x 0 [expr {$x+7}] [winfo screenheight .c] \
            -fill [lindex $colors 0] -outline {}
    set colors [list {*}[lrange $colors 1 end] [lindex $colors 0]]
}
```


== {{header|UNIX Shell}} ==

```sh
#!/bin/sh
clear
WIDTH=`tput cols`
HEIGHT=`tput lines`
NUMBARS=8
BARWIDTH=`expr $WIDTH / $NUMBARS`

l="1"    # Set the line counter to 1
while [ "$l" -lt $HEIGHT ]; do
  b="0"    # Bar counter
  while [ "$b" -lt $NUMBARS ]; do
    tput setab $b
    s="0"
    while [ "$s" -lt $BARWIDTH ]; do
      echo -n " "
      s=`expr $s + 1`
    done
    b=`expr $b + 1`
  done
  echo    # newline
  l=`expr $l + 1`
done

tput sgr0    # reset
```



## XPL0


```XPL0
include c:\cxpl\codes;       \intrinsic code declarations
int W, X0, X1, Y, C;
[SetVid($13);           \320x200x8 graphics
W:= 320/8;              \width of color bar (pixels)
for C:= 0 to 8-1 do
        [X0:= W*C; X1:= X0+W-1;
        for Y:= 0 to 200-1 do
                [Move(X0, Y); Line(X1, Y, C)];
        ];
C:= ChIn(1);            \wait for keystroke
SetVid(3);              \restore normal text mode
]
```


{{omit from|AWK}}
{{omit from|Axe}}
{{omit from|GUISS}}

[[Category:Test card]]
[[Category:Terminal control]]
