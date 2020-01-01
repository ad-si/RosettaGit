+++
title = "Draw a clock"
description = ""
date = 2019-08-24T23:04:02Z
aliases = []
[extra]
id = 10017
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Date and time]]

;Task:
Draw a clock.


More specific:
# Draw a time keeping device. It can be a stopwatch, hourglass, sundial, a mouth counting "one thousand and one", anything. Only showing the seconds is required, e.g.: a watch with just a second hand will suffice. However, it must clearly change every second, and the change must cycle every so often (one minute, 30 seconds, etc.) It must be ''drawn''; printing a string of numbers to your terminal doesn't qualify. Both text-based and graphical drawing are OK.
# The clock is unlikely to be used to control space flights, so it needs not be hyper-accurate, but it should be usable, meaning if one can read the seconds off the clock, it must agree with the system clock.
# A clock is rarely (never?) a major application: don't be a CPU hog and poll the system timer every microsecond, use a proper timer/signal/event from your system or language instead. For a bad example, many OpenGL programs update the frame-buffer in a busy loop even if no redraw is needed, which is very undesirable for this task.
# A clock is rarely (never?) a major application: try to keep your code simple and to the point. Don't write something too elaborate or convoluted, instead do whatever is natural, concise and clear in your language.



;Key points
* animate simple object
* timed event
* polling system resources
* code clarity





## ActionScript


```ActionScript

package  {

    import flash.display.Graphics;
    import flash.display.Shape;
    import flash.display.Sprite;
    import flash.events.Event;
    import flash.events.TimerEvent;
    import flash.utils.Timer;

    public class Clock extends Sprite {

        // Changes of hands (in degrees) per second
        private static const HOUR_HAND_CHANGE:Number = 1 / 120;  // 360 / (60 * 60 * 12)
        private static const MINUTE_HAND_CHANGE:Number = 0.1;      // 360 / (60 * 60)
        private static const SECOND_HAND_CHANGE:Number = 6;        // 360 / 60

        private var _timer:Timer;

        private var _hHand:Shape;
        private var _mHand:Shape;
        private var _sHand:Shape;

        public function Clock() {
            if ( stage ) _init();
            else addEventListener(Event.ADDED_TO_STAGE, _init);
        }

        private function _init(e:Event = null):void {

            var i:uint;

            var base:Shape = new Shape(), hHand:Shape = new Shape(), mHand:Shape = new Shape();
            var sHand:Shape = new Shape(), hub:Shape = new Shape();

            var size:Number = 500;
            var c:Number = size / 2;
            x = 30;
            y = 30;

            var baseGraphics:Graphics = base.graphics;

            baseGraphics.lineStyle(5, 0xEE0000);
            baseGraphics.beginFill(0xFFDDDD);
            baseGraphics.drawCircle(c, c, c);

            var uAngle:Number = Math.PI / 30;

            var markerStart:Number = c - 30;
            var markerEnd:Number = c - 15;

            var markerX1:Number, markerY1:Number, markerX2:Number, markerY2:Number;
            var angle:Number, angleSin:Number, angleCos:Number;

            baseGraphics.endFill();

            var isMajorMarker:Boolean = true;

            for ( i = 0; i < 60; i++ ) {
                // Draw the markers

                angle = uAngle * i;
                angleSin = Math.sin(angle);
                angleCos = Math.cos(angle);

                markerX1 = c + markerStart * angleCos;
                markerY1 = c + markerStart * angleSin;
                markerX2 = c + markerEnd * angleCos;
                markerY2 = c + markerEnd * angleSin;

                if ( i % 5 == 0 ) {
                    baseGraphics.lineStyle(3, 0x000080);
                    isMajorMarker = true;
                }
                else if ( isMajorMarker ) {
                    baseGraphics.lineStyle(1, 0x000080);
                    isMajorMarker = false;
                }

                baseGraphics.moveTo(markerX1, markerY1);
                baseGraphics.lineTo(markerX2, markerY2);
            }

            addChild(base);

            sHand.graphics.lineStyle(2, 0x00BB00);
            sHand.graphics.moveTo(0, 0);
            sHand.graphics.lineTo(0, 40 - c);
            sHand.x = sHand.y = c;

            mHand.graphics.lineStyle(8, 0x444444);
            mHand.graphics.moveTo(0, 0);
            mHand.graphics.lineTo(0, 50 - c);
            mHand.x = mHand.y = c;

            hHand.graphics.lineStyle(8, 0x777777);
            hHand.graphics.moveTo(0, 0);
            hHand.graphics.lineTo(0, 120 - c);
            hHand.x = hHand.y = c;

            hub.graphics.lineStyle(4, 0x664444);
            hub.graphics.beginFill(0xCC9999);
            hub.graphics.drawCircle(c, c, 5);

            _hHand = hHand;
            _mHand = mHand;
            _sHand = sHand;

            addChild(mHand);
            addChild(hHand);
            addChild(sHand);
            addChild(hub);

            var date:Date = new Date();

            // Since millisecond precision is not needed, round it up to the nearest second.
            var seconds:Number = date.seconds + ((date.milliseconds > 500) ? 1 : 0);
            var minutes:Number = date.minutes + seconds / 60;
            var hours:Number = (date.hours + minutes / 60) % 12;

            sHand.rotation = seconds * 6;
            mHand.rotation = minutes * 6;
            hHand.rotation = hours * 30;

            _timer = new Timer(1000);  // 1 second = 1000 ms
            _timer.addEventListener(TimerEvent.TIMER, _onTimerTick);
            _timer.start();

        }

        private function _onTimerTick(e:TimerEvent):void {
            _hHand.rotation += HOUR_HAND_CHANGE;
            _mHand.rotation += MINUTE_HAND_CHANGE;
            _sHand.rotation += SECOND_HAND_CHANGE;
        }

    }

}

```



## AutoHotkey

requires the GDI+ Library from http://www.autohotkey.com/forum/viewtopic.php?t=32238
this code from http://www.autohotkey.com/forum/viewtopic.php?p=231836#231836
draws a very nice clock with GDI+

```AHK
; gdi+ ahk analogue clock example written by derRaphael
; Parts based on examples from Tic's GDI+ Tutorials and of course on his GDIP.ahk

; This code has been licensed under the terms of EUPL 1.0

#SingleInstance, Force
#NoEnv
SetBatchLines, -1

; Uncomment if Gdip.ahk is not in your standard library
;#Include, Gdip.ahk

If !pToken := Gdip_Startup()
{
   MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
   ExitApp
}
OnExit, Exit

SysGet, MonitorPrimary, MonitorPrimary
SysGet, WA, MonitorWorkArea, %MonitorPrimary%
WAWidth := WARight-WALeft
WAHeight := WABottom-WATop

Gui, 1: -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, 1: Show, NA
hwnd1 := WinExist()

ClockDiameter := 180
Width := Height := ClockDiameter + 2         ; make width and height slightly bigger to avoid cut away edges
CenterX := CenterY := floor(ClockDiameter/2) ; Center x

; Prepare our pGraphic so we have a 'canvas' to work upon
   hbm := CreateDIBSection(Width, Height), hdc := CreateCompatibleDC()
   obm := SelectObject(hdc, hbm), G := Gdip_GraphicsFromHDC(hdc)
   Gdip_SetSmoothingMode(G, 4)

; Draw outer circle
   Diameter := ClockDiameter
   pBrush := Gdip_BrushCreateSolid(0x66008000)
   Gdip_FillEllipse(G, pBrush, CenterX-(Diameter//2), CenterY-(Diameter//2),Diameter, Diameter)
   Gdip_DeleteBrush(pBrush)

; Draw inner circle
   Diameter := ceil(ClockDiameter - ClockDiameter*0.08)  ; inner circle is 8 % smaller than clock's diameter
   pBrush := Gdip_BrushCreateSolid(0x80008000)
   Gdip_FillEllipse(G, pBrush, CenterX-(Diameter//2), CenterY-(Diameter//2),Diameter, Diameter)
   Gdip_DeleteBrush(pBrush)

; Draw Second Marks
   R1 := Diameter//2-1                        ; outer position
   R2 := Diameter//2-1-ceil(Diameter//2*0.05) ; inner position
   Items := 60                                ; we have 60 seconds
   pPen := Gdip_CreatePen(0xff00a000, floor((ClockDiameter/100)*1.2)) ; 1.2 % of total diameter is our pen width
   GoSub, DrawClockMarks
   Gdip_DeletePen(pPen)

; Draw Hour Marks
   R1 := Diameter//2-1                       ; outer position
   R2 := Diameter//2-1-ceil(Diameter//2*0.1) ; inner position
   Items := 12                               ; we have 12 hours
   pPen := Gdip_CreatePen(0xc0008000, ceil((ClockDiameter//100)*2.3)) ; 2.3 % of total diameter is our pen width
   GoSub, DrawClockMarks
   Gdip_DeletePen(pPen)

   ; The OnMessage will let us drag the clock
   OnMessage(0x201, "WM_LBUTTONDOWN")
   UpdateLayeredWindow(hwnd1, hdc, WALeft+((WAWidth-Width)//2), WATop+((WAHeight-Height)//2), Width, Height)
   SetTimer, sec, 1000

sec:
; prepare to empty previously drawn stuff
   Gdip_SetSmoothingMode(G, 1)   ; turn off aliasing
   Gdip_SetCompositingMode(G, 1) ; set to overdraw

; delete previous graphic and redraw background
   Diameter := ceil(ClockDiameter - ClockDiameter*0.18)  ; 18 % less than clock's outer diameter

   ; delete whatever has been drawn here
   pBrush := Gdip_BrushCreateSolid(0x00000000) ; fully transparent brush 'eraser'
   Gdip_FillEllipse(G, pBrush, CenterX-(Diameter//2), CenterY-(Diameter//2),Diameter, Diameter)
   Gdip_DeleteBrush(pBrush)

   Gdip_SetCompositingMode(G, 0) ; switch off overdraw
   pBrush := Gdip_BrushCreateSolid(0x66008000)
   Gdip_FillEllipse(G, pBrush, CenterX-(Diameter//2), CenterY-(Diameter//2),Diameter, Diameter)
   Gdip_DeleteBrush(pBrush)
   pBrush := Gdip_BrushCreateSolid(0x80008000)
   Gdip_FillEllipse(G, pBrush, CenterX-(Diameter//2), CenterY-(Diameter//2),Diameter, Diameter)
   Gdip_DeleteBrush(pBrush)

; Draw HoursPointer
   Gdip_SetSmoothingMode(G, 4)   ; turn on antialiasing
   t := A_Hour*360//12 + (A_Min*360//60)//12 +90
   R1 := ClockDiameter//2-ceil((ClockDiameter//2)*0.5) ; outer position
   pPen := Gdip_CreatePen(0xa0008000, floor((ClockDiameter/100)*3.5))
   Gdip_DrawLine(G, pPen, CenterX, CenterY
      , ceil(CenterX - (R1 * Cos(t * Atan(1) * 4 / 180)))
      , ceil(CenterY - (R1 * Sin(t * Atan(1) * 4 / 180))))
   Gdip_DeletePen(pPen)

; Draw MinutesPointer
   t := A_Min*360//60+90
   R1 := ClockDiameter//2-ceil((ClockDiameter//2)*0.25) ; outer position
   pPen := Gdip_CreatePen(0xa0008000, floor((ClockDiameter/100)*2.7))
   Gdip_DrawLine(G, pPen, CenterX, CenterY
      , ceil(CenterX - (R1 * Cos(t * Atan(1) * 4 / 180)))
      , ceil(CenterY - (R1 * Sin(t * Atan(1) * 4 / 180))))
   Gdip_DeletePen(pPen)

; Draw SecondsPointer
   t := A_Sec*360//60+90
   R1 := ClockDiameter//2-ceil((ClockDiameter//2)*0.2) ; outer position
   pPen := Gdip_CreatePen(0xa000FF00, floor((ClockDiameter/100)*1.2))
   Gdip_DrawLine(G, pPen, CenterX, CenterY
      , ceil(CenterX - (R1 * Cos(t * Atan(1) * 4 / 180)))
      , ceil(CenterY - (R1 * Sin(t * Atan(1) * 4 / 180))))
   Gdip_DeletePen(pPen)

   UpdateLayeredWindow(hwnd1, hdc) ;, xPos, yPos, ClockDiameter, ClockDiameter)
return

DrawClockMarks:
   Loop, % Items
      Gdip_DrawLine(G, pPen
         , CenterX - ceil(R1 * Cos(((a_index-1)*360//Items) * Atan(1) * 4 / 180))
         , CenterY - ceil(R1 * Sin(((a_index-1)*360//Items) * Atan(1) * 4 / 180))
         , CenterX - ceil(R2 * Cos(((a_index-1)*360//Items) * Atan(1) * 4 / 180))
         , CenterY - ceil(R2 * Sin(((a_index-1)*360//Items) * Atan(1) * 4 / 180)) )
return

WM_LBUTTONDOWN() {
   PostMessage, 0xA1, 2
   return
}

esc::
Exit:
   SelectObject(hdc, obm)
   DeleteObject(hbm)
   DeleteDC(hdc)
   Gdip_DeleteGraphics(G)
   Gdip_Shutdown(pToken)
   ExitApp
Return
```


## AWK


```AWK

# syntax: GAWK -f DRAW_A_CLOCK.AWK [-v xc="*"]
BEGIN {
#   clearscreen_cmd = "clear" ; sleep_cmd = "sleep 1s" # Unix
    clearscreen_cmd = "CLS" ; sleep_cmd = "TIMEOUT /T 1 >NUL" # MS-Windows
    clock_build_digits()
    while (1) {
      now = strftime("%H:%M:%S")
      t[1] = substr(now,1,1)
      t[2] = substr(now,2,1)
      t[3] = 10
      t[4] = substr(now,4,1)
      t[5] = substr(now,5,1)
      t[6] = 10
      t[7] = substr(now,7,1)
      t[8] = substr(now,8,1)
      if (prev_now != now) {
        system(clearscreen_cmd)
        for (v=1; v<=8; v++) {
          printf("\t")
          for (h=1; h<=8; h++) {
            printf("%-8s",a[t[h],v])
          }
          printf("\n")
        }
        prev_now = now
      }
      system(sleep_cmd)
    }
    exit(0)
}
function clock_build_digits(  arr,i,j,x,y) {
    arr[1] = " 0000     1    2222   3333      4  555555  6666   777777 8888   9999         "
    arr[2] = "0    0   11   2    2 3    3    44  5      6      7     78    8 9    9        "
    arr[3] = "0   00  1 1        2      3   4 4  5      6           7 8    8 9    9   ::   "
    arr[4] = "0  0 0    1       2    333   4  4  555555 66666      7   8888  9    9   ::   "
    arr[5] = "0 0  0    1     22        3 444444      5 6    6    7   8    8  99999        "
    arr[6] = "00   0    1    2          3     4       5 6    6   7    8    8      9   ::   "
    arr[7] = "0    0    1   2      3    3     4  5    5 6    6  7     8    8      9   ::   "
    arr[8] = " 0000  1111111222222  3333      4   5555   6666   7      8888   9999         "
    for (i=1; i<=8; i++) {
      if (xc != "") {
        gsub(/[0-9:]/,substr(xc,1,1),arr[i]) # change "0-9" and ":" to substitution character
      }
      y++
      x = -1
      for (j=1; j<=77; j=j+7) {
        a[++x,y] = substr(arr[i],j,7)
      }
    }
}

```

{{out|Sample run and output}}

```txt

GAWK -f DRAW_A_CLOCK.AWK -v xc="#"

         ####    ####              #     ####            ####    ####
        #    #  #    #            ##    #    #          #    #  #    #
        #   ##  #    #    ##     # #    #    #    ##    #   ##  #   ##
        #  # #   ####     ##       #    #    #    ##    #  # #  #  # #
        # #  #  #    #             #     #####          # #  #  # #  #
        ##   #  #    #    ##       #         #    ##    ##   #  ##   #
        #    #  #    #    ##       #         #    ##    #    #  #    #
         ####    ####           #######  ####            ####    ####

```


## BASIC

=
## Commodore BASIC
=
To be entered in upper/lowercase mode but run in uppercase + graphics mode.

```commodorebasic
10 gosub 1500: rem setup clock digit strings
20 ti$ = "123456"
25 rem do some other stuff after this line
30 print x: x=x+1
40 for i=0 to 500: next
50 gosub 1000: rem display the time
60 goto 30
70 end
1000 t$ = ti$
1010 for i=1 to 6
1020   t(i) = val(mid$(t$,i,1))
1030 next
1040 print chr$(19);
1050 for j=1 to 5
1055   print tab(19);
1060   for i=1 to 6
1070     k=t(i)*3+1
1080     print mid$(z$(j),k,3);
1090     rem if j<5 then print" ";: goto 1130
1100     if i=2 then print" ";
1110     if i=4 then print" ";
1130   next
1140   print
1150 next
1160 return
1500 dim z$(5)
1510 z$(1) = "UCI I UCICCIB BCCCUCIUCIUCI"
1520 z$(2) = "B B B   B  BB BB  B  B BB B"
1530 z$(3) = "B B B UCK CBJCBJCIBCIBCIJCB"
1540 z$(4) = "B B B B    B  B  BB BB B  B"
1550 z$(5) = "JCKCCCJCCCCK  BCCKJCKJCK CK"
1560 return
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Clock.bas"
110 OPTION ANGLE DEGREES
120 LET CH=1:LET CH2=2
130 SET VIDEO MODE 1:SET VIDEO COLOR 1:SET VIDEO X 26:SET VIDEO Y 25
140 OPEN #1:"video:"
150 OPEN #2:"video:"
160 DO
170   LET H=VAL(TIME$(1:2)):LET M=VAL(TIME$(4:5)):LET S=VAL(TIME$(7:))
180   SET #CH:INK 3:PLOT #CH:420,420,ANGLE 90-30*H-M/2;FORWARD 200
190   PLOT #CH:420,420,ANGLE 90-6*M;FORWARD 350
200   SET #CH:INK 2:PLOT #CH:420,420,ANGLE 90-6*S;FORWARD 300
210   SET #CH:INK 1:PLOT #CH:420,420,ELLIPSE 12,12,ELLIPSE 400,400
230   PLOT #CH:394,812,:PRINT #CH:"12":PLOT #CH:784,434,:PRINT #CH:"3"
240   PLOT #CH:406,58,:PRINT #CH:"6":PLOT #CH:28,434,:PRINT #CH:"9"
250   DISPLAY #CH:AT 1 FROM 1 TO 25
260   CLEAR #CH2
270   LET T=CH:LET CH=CH2:LET CH2=T
280 LOOP UNTIL INKEY$=CHR$(27)
290 CLOSE #2
300 CLOSE #1
310 TEXT
```



## Batch File


```dos
::Draw a Clock Task from Rosetta Code Wiki
::Batch File Implementation
::
::Directly open the Batch File...
@echo off & mode 44,8
title Sample Batch Clock
setlocal enabledelayedexpansion

	::Set the characters...
set "#0_1=ÛÛÛÛÛ"
set "#0_2=Û   Û"
set "#0_3=Û   Û"
set "#0_4=Û   Û"
set "#0_5=ÛÛÛÛÛ"

set "#1_1=    Û"
set "#1_2=    Û"
set "#1_3=    Û"
set "#1_4=    Û"
set "#1_5=    Û"

set "#2_1=ÛÛÛÛÛ"
set "#2_2=    Û"
set "#2_3=ÛÛÛÛÛ"
set "#2_4=Û    "
set "#2_5=ÛÛÛÛÛ"

set "#3_1=ÛÛÛÛÛ"
set "#3_2=    Û"
set "#3_3=ÛÛÛÛÛ"
set "#3_4=    Û"
set "#3_5=ÛÛÛÛÛ"

set "#4_1=Û   Û"
set "#4_2=Û   Û"
set "#4_3=ÛÛÛÛÛ"
set "#4_4=    Û"
set "#4_5=    Û"

set "#5_1=ÛÛÛÛÛ"
set "#5_2=Û    "
set "#5_3=ÛÛÛÛÛ"
set "#5_4=    Û"
set "#5_5=ÛÛÛÛÛ"

set "#6_1=ÛÛÛÛÛ"
set "#6_2=Û    "
set "#6_3=ÛÛÛÛÛ"
set "#6_4=Û   Û"
set "#6_5=ÛÛÛÛÛ"

set "#7_1=ÛÛÛÛÛ"
set "#7_2=    Û"
set "#7_3=    Û"
set "#7_4=    Û"
set "#7_5=    Û"

set "#8_1=ÛÛÛÛÛ"
set "#8_2=Û   Û"
set "#8_3=ÛÛÛÛÛ"
set "#8_4=Û   Û"
set "#8_5=ÛÛÛÛÛ"

set "#9_1=ÛÛÛÛÛ"
set "#9_2=Û   Û"
set "#9_3=ÛÛÛÛÛ"
set "#9_4=    Û"
set "#9_5=ÛÛÛÛÛ"

set "#C_1= "
set "#C_2=Û"
set "#C_3= "
set "#C_4=Û"
set "#C_5= "

:clock_loop
	::Clear display [leaving a whitespace]...
for /l %%C in (1,1,5) do set "display%%C= "

	::Get current time [all spaces will be replaced to zero]...
	::Also, all colons will be replaced to "C" because colon has a function in variables...
set "curr_time=%time: =0%"
set "curr_time=%curr_time::=C%"

	::Process the numbers to display [we will now use the formats we SET above]...
for /l %%T in (0,1,7) do (
	::Check for each number and colons...
	for %%N in (0 1 2 3 4 5 6 7 8 9 C) do (
		if "!curr_time:~%%T,1!"=="%%N" (
			::Now, barbeque each formatted char in 5 rows...
			for /l %%D in (1,1,5) do set "display%%D=!display%%D!!#%%N_%%D! "
		)
	)
)

	::Refresh the clock...
cls
echo.
echo.[%display1%]
echo.[%display2%]
echo.[%display3%]
echo.[%display4%]
echo.[%display5%]
echo.
timeout /t 1 /nobreak >nul
goto :clock_loop
```

{{Out}}

```txt

[ █████ █████   █   █ █████   █████ █████ ]
[     █     █ █ █   █     █ █ █     █   █ ]
[ █████ █████   █████     █   █████ █████ ]
[ █     █     █     █     █ █     █     █ ]
[ █████ █████       █     █   █████ █████ ]

```



## C

Draws a crude clock in terminal.  C99, compiled with <code>gcc -std=c99</code>.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <sys/time.h>

#define PI 3.14159265
const char * shades = " .:-*ca&#%@";

/* distance of (x, y) from line segment (0, 0)->(x0, y0) */
double dist(double x, double y, double x0, double y0) {
	double l = (x * x0 + y * y0) / (x0 * x0 + y0 * y0);

	if (l > 1) {
		x -= x0;
		y -= y0;
	} else if (l >= 0) {
		x -= l * x0;
		y -= l * y0;
	}
	return sqrt(x * x + y * y);
}

enum { sec = 0, min, hur }; // for subscripts

void draw(int size)
{
#	define for_i for(int i = 0; i < size; i++)
#	define for_j for(int j = 0; j < size * 2; j++)

	double angle, cx = size / 2.;
	double sx[3], sy[3], sw[3];
	double fade[] = { 1, .35, .35 }; /* opacity of each arm */
	struct timeval tv;
	struct tm *t;

	/* set width of each arm */
	sw[sec] = size * .02;
	sw[min] = size * .03;
	sw[hur] = size * .05;

every_second:
	gettimeofday(&tv, 0);
	t = localtime(&tv.tv_sec);

	angle = t->tm_sec * PI / 30;
	sy[sec] = -cx * cos(angle);
	sx[sec] =  cx * sin(angle);

	angle = (t->tm_min + t->tm_sec / 60.) / 30 * PI;
	sy[min] = -cx * cos(angle) * .8;
	sx[min] =  cx * sin(angle) * .8;

	angle = (t->tm_hour + t->tm_min / 60.) / 6 * PI;
	sy[hur] = -cx * cos(angle) * .6;
	sx[hur] =  cx * sin(angle) * .6;

	printf("\033[s"); /* save cursor position */
	for_i {
		printf("\033[%d;0H", i);  /* goto row i, col 0 */
		double y = i - cx;
		for_j {
			double x = (j - 2 * cx) / 2;

			int pix = 0;
			/* calcs how far the "pixel" is from each arm and set
			 * shade, with some anti-aliasing.  It's ghetto, but much
			 * easier than a real scanline conversion.
			 */
			for (int k = hur; k >= sec; k--) {
				double d = dist(x, y, sx[k], sy[k]);
				if (d < sw[k] - .5)
					pix = 10 * fade[k];
				else if (d < sw[k] + .5)
					pix = (5 + (sw[k] - d) * 10) * fade[k];
			}
			putchar(shades[pix]);
		}
	}
	printf("\033[u"); /* restore cursor pos so you can bg the job -- value unclear */

	fflush(stdout);
	sleep(1); /* sleep 1 can at times miss a second, but will catch up next update */
	goto every_second;
}

int main(int argc, char *argv[])
{
	int s;
	if (argc <= 1 || (s = atoi(argv[1])) <= 0) s = 20;
	draw(s);
	return 0;
}
```




## C++

[[File:clock_cpp.png]]

```cpp

#include <windows.h>
#include <string>
#include <math.h>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int BMP_SIZE = 300, MY_TIMER = 987654, CENTER = BMP_SIZE >> 1, SEC_LEN = CENTER - 20,
          MIN_LEN = SEC_LEN - 20, HOUR_LEN = MIN_LEN - 20;
const float PI = 3.1415926536f;

//--------------------------------------------------------------------------------------------------
class vector2
{
public:
    vector2() { x = y = 0; }
    vector2( int a, int b ) { x = a; y = b; }
    void set( int a, int b ) { x = a; y = b; }
    void rotate( float angle_r )
    {
	float _x = static_cast<float>( x ),
	      _y = static_cast<float>( y ),
	       s = sinf( angle_r ),
	       c = cosf( angle_r ),
	       a = _x * c - _y * s,
	       b = _x * s + _y * c;

	x = static_cast<int>( a );
	y = static_cast<int>( b );
    }
    int x, y;
};
//--------------------------------------------------------------------------------------------------
class myBitmap
{
public:
    myBitmap() : pen( NULL ), brush( NULL ), clr( 0 ), wid( 1 ) {}
    ~myBitmap()
    {
	DeleteObject( pen );
	DeleteObject( brush );
	DeleteDC( hdc );
	DeleteObject( bmp );
    }

    bool create( int w, int h )
    {
	BITMAPINFO    bi;
	ZeroMemory( &bi, sizeof( bi ) );
	bi.bmiHeader.biSize        = sizeof( bi.bmiHeader );
	bi.bmiHeader.biBitCount    = sizeof( DWORD ) * 8;
	bi.bmiHeader.biCompression = BI_RGB;
	bi.bmiHeader.biPlanes      = 1;
	bi.bmiHeader.biWidth       =  w;
	bi.bmiHeader.biHeight      = -h;

	HDC dc = GetDC( GetConsoleWindow() );
	bmp = CreateDIBSection( dc, &bi, DIB_RGB_COLORS, &pBits, NULL, 0 );
	if( !bmp ) return false;

	hdc = CreateCompatibleDC( dc );
	SelectObject( hdc, bmp );
	ReleaseDC( GetConsoleWindow(), dc );

	width = w; height = h;
	return true;
    }

    void clear( BYTE clr = 0 )
    {
	memset( pBits, clr, width * height * sizeof( DWORD ) );
    }

    void setBrushColor( DWORD bClr )
    {
	if( brush ) DeleteObject( brush );
	brush = CreateSolidBrush( bClr );
	SelectObject( hdc, brush );
    }

    void setPenColor( DWORD c )
    {
	clr = c;
	createPen();
    }

    void setPenWidth( int w )
    {
	wid = w;
	createPen();
    }

    void saveBitmap( string path )
    {
	BITMAPFILEHEADER fileheader;
	BITMAPINFO       infoheader;
	BITMAP           bitmap;
	DWORD            wb;

	GetObject( bmp, sizeof( bitmap ), &bitmap );
	DWORD* dwpBits = new DWORD[bitmap.bmWidth * bitmap.bmHeight];

        ZeroMemory( dwpBits, bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD ) );
	ZeroMemory( &infoheader, sizeof( BITMAPINFO ) );
	ZeroMemory( &fileheader, sizeof( BITMAPFILEHEADER ) );

	infoheader.bmiHeader.biBitCount = sizeof( DWORD ) * 8;
	infoheader.bmiHeader.biCompression = BI_RGB;
	infoheader.bmiHeader.biPlanes = 1;
	infoheader.bmiHeader.biSize = sizeof( infoheader.bmiHeader );
	infoheader.bmiHeader.biHeight = bitmap.bmHeight;
	infoheader.bmiHeader.biWidth = bitmap.bmWidth;
	infoheader.bmiHeader.biSizeImage = bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD );

	fileheader.bfType    = 0x4D42;
	fileheader.bfOffBits = sizeof( infoheader.bmiHeader ) + sizeof( BITMAPFILEHEADER );
	fileheader.bfSize    = fileheader.bfOffBits + infoheader.bmiHeader.biSizeImage;

	GetDIBits( hdc, bmp, 0, height, ( LPVOID )dwpBits, &infoheader, DIB_RGB_COLORS );

	HANDLE file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
	WriteFile( file, &fileheader, sizeof( BITMAPFILEHEADER ), &wb, NULL );
	WriteFile( file, &infoheader.bmiHeader, sizeof( infoheader.bmiHeader ), &wb, NULL );
	WriteFile( file, dwpBits, bitmap.bmWidth * bitmap.bmHeight * 4, &wb, NULL );
	CloseHandle( file );

	delete [] dwpBits;
    }

    HDC getDC() const     { return hdc; }
    int getWidth() const  { return width; }
    int getHeight() const { return height; }

private:
    void createPen()
    {
	if( pen ) DeleteObject( pen );
	pen = CreatePen( PS_SOLID, wid, clr );
	SelectObject( hdc, pen );
    }

    HBITMAP bmp;
    HDC     hdc;
    HPEN    pen;
    HBRUSH  brush;
    void    *pBits;
    int     width, height, wid;
    DWORD   clr;
};
//--------------------------------------------------------------------------------------------------
class clock
{
public:
    clock()
    {
	_bmp.create( BMP_SIZE, BMP_SIZE );
	_bmp.clear( 100 );
	_bmp.setPenWidth( 2 );
	_ang = DegToRadian( 6 );
    }

    void setNow()
    {
	GetLocalTime( &_sysTime );
	draw();
    }

    float DegToRadian( float degree ) { return degree * ( PI / 180.0f ); }

    void setHWND( HWND hwnd ) { _hwnd = hwnd; }

private:
    void drawTicks( HDC dc )
    {
	vector2 line;
	_bmp.setPenWidth( 1 );
	for( int x = 0; x < 60; x++ )
	{
	    line.set( 0, 50 );
	    line.rotate( static_cast<float>( x + 30 ) * _ang );
	    MoveToEx( dc, CENTER - static_cast<int>( 2.5f * static_cast<float>( line.x ) ), CENTER - static_cast<int>( 2.5f * static_cast<float>( line.y ) ), NULL );
	    LineTo( dc, CENTER - static_cast<int>( 2.81f * static_cast<float>( line.x ) ), CENTER - static_cast<int>( 2.81f * static_cast<float>( line.y ) ) );
	}

	_bmp.setPenWidth( 3 );
	for( int x = 0; x < 60; x += 5 )
	{
	    line.set( 0, 50 );
	    line.rotate( static_cast<float>( x + 30 ) * _ang );
	    MoveToEx( dc, CENTER - static_cast<int>( 2.5f * static_cast<float>( line.x ) ), CENTER - static_cast<int>( 2.5f * static_cast<float>( line.y ) ), NULL );
	    LineTo( dc, CENTER - static_cast<int>( 2.81f * static_cast<float>( line.x ) ), CENTER - static_cast<int>( 2.81f * static_cast<float>( line.y ) ) );
	}
    }

    void drawHands( HDC dc )
    {
	float hp = DegToRadian( ( 30.0f * static_cast<float>( _sysTime.wMinute ) ) / 60.0f );
	int h = ( _sysTime.wHour > 12 ? _sysTime.wHour - 12 : _sysTime.wHour ) * 5;

	_bmp.setPenWidth( 3 );
	_bmp.setPenColor( RGB( 0, 0, 255 ) );
	drawHand( dc, HOUR_LEN, ( _ang * static_cast<float>( 30 + h ) ) + hp );

	_bmp.setPenColor( RGB( 0, 128, 0 ) );
	drawHand( dc, MIN_LEN, _ang * static_cast<float>( 30 + _sysTime.wMinute ) );

	_bmp.setPenWidth( 2 );
	_bmp.setPenColor( RGB( 255, 0, 0 ) );
	drawHand( dc, SEC_LEN, _ang * static_cast<float>( 30 + _sysTime.wSecond ) );
    }

    void drawHand( HDC dc, int len, float ang )
    {
	vector2 line;
	line.set( 0, len );
	line.rotate( ang );
	MoveToEx( dc, CENTER, CENTER, NULL );
	LineTo( dc, line.x + CENTER, line.y + CENTER );
    }

    void draw()
    {
	HDC dc = _bmp.getDC();

	_bmp.setBrushColor( RGB( 250, 250, 250 ) );
	Ellipse( dc, 0, 0, BMP_SIZE, BMP_SIZE );
	_bmp.setBrushColor( RGB( 230, 230, 230 ) );
	Ellipse( dc, 10, 10, BMP_SIZE - 10, BMP_SIZE - 10 );

	drawTicks( dc );
	drawHands( dc );

	_bmp.setPenColor( 0 ); _bmp.setBrushColor( 0 );
	Ellipse( dc, CENTER - 5, CENTER - 5, CENTER + 5, CENTER + 5 );

	_wdc = GetDC( _hwnd );
	BitBlt( _wdc, 0, 0, BMP_SIZE, BMP_SIZE, dc, 0, 0, SRCCOPY );
	ReleaseDC( _hwnd, _wdc );
    }

    myBitmap   _bmp;
    HWND       _hwnd;
    HDC        _wdc;
    SYSTEMTIME _sysTime;
    float      _ang;
};
//--------------------------------------------------------------------------------------------------
class wnd
{
public:
    wnd() { _inst = this; }
    int wnd::Run( HINSTANCE hInst )
    {
	_hInst = hInst;
	_hwnd = InitAll();
	SetTimer( _hwnd, MY_TIMER, 1000, NULL );
	_clock.setHWND( _hwnd );

	ShowWindow( _hwnd, SW_SHOW );
	UpdateWindow( _hwnd );

	MSG msg;
	ZeroMemory( &msg, sizeof( msg ) );
	while( msg.message != WM_QUIT )
	{
	    if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) != 0 )
	    {
		TranslateMessage( &msg );
		DispatchMessage( &msg );
	    }
	}
	return UnregisterClass( "_MY_CLOCK_", _hInst );
    }
private:
    void wnd::doPaint( HDC dc ) { _clock.setNow(); }
    void wnd::doTimer()         { _clock.setNow(); }
    static int WINAPI wnd::WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
    {
	switch( msg )
	{
	    case WM_DESTROY: PostQuitMessage( 0 ); break;
	    case WM_PAINT:
	    {
		PAINTSTRUCT ps;
		HDC dc = BeginPaint( hWnd, &ps );
		_inst->doPaint( dc );
		EndPaint( hWnd, &ps );
		return 0;
	    }
	    case WM_TIMER: _inst->doTimer(); break;
	    default:
		return DefWindowProc( hWnd, msg, wParam, lParam );
	}
	return 0;
    }

    HWND InitAll()
    {
	WNDCLASSEX wcex;
	ZeroMemory( &wcex, sizeof( wcex ) );
	wcex.cbSize           = sizeof( WNDCLASSEX );
	wcex.style           = CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc   = ( WNDPROC )WndProc;
	wcex.hInstance     = _hInst;
	wcex.hCursor       = LoadCursor( NULL, IDC_ARROW );
	wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
	wcex.lpszClassName = "_MY_CLOCK_";

	RegisterClassEx( &wcex );

	RECT rc = { 0, 0, BMP_SIZE, BMP_SIZE };
	AdjustWindowRect( &rc, WS_SYSMENU | WS_CAPTION, FALSE );
	int w = rc.right - rc.left, h = rc.bottom - rc.top;
	return CreateWindow( "_MY_CLOCK_", ".: Clock -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, w, h, NULL, NULL, _hInst, NULL );
    }

    static wnd* _inst;
    HINSTANCE  _hInst;
    HWND       _hwnd;
    clock      _clock;
};
wnd* wnd::_inst = 0;
//--------------------------------------------------------------------------------------------------
int APIENTRY _tWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow )
{
    wnd myWnd;
    return myWnd.Run( hInstance );
}
//--------------------------------------------------------------------------------------------------

```



## C#


```csharp
using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

public class Clock : Form
{
    static readonly float degrees06 = (float)Math.PI / 30;
    static readonly float degrees30 = degrees06 * 5;
    static readonly float degrees90 = degrees30 * 3;

    readonly int margin = 20;

    private Point p0;

    public Clock()
    {
        Size = new Size(500, 500);
        StartPosition = FormStartPosition.CenterScreen;
        Resize += (sender, args) => ResetSize();
        ResetSize();
        var timer = new Timer() { Interval = 1000, Enabled = true };
        timer.Tick += (sender, e) => Refresh();
        DoubleBuffered = true;
    }

    private void ResetSize()
    {
        p0 = new Point(ClientRectangle.Width / 2, ClientRectangle.Height / 2);
        Refresh();
    }

    protected override void OnPaint(PaintEventArgs e)
    {
        base.OnPaint(e);
        e.Graphics.SmoothingMode = SmoothingMode.AntiAlias;

        drawFace(e.Graphics);

        var time = DateTime.Now;
        int second = time.Second;
        int minute = time.Minute;
        int hour = time.Hour;

        float angle = degrees90 - (degrees06 * second);
        DrawHand(e.Graphics, Pens.Red, angle, 0.95);

        float minsecs = (minute + second / 60.0F);
        angle = degrees90 - (degrees06 * minsecs);
        DrawHand(e.Graphics, Pens.Black, angle, 0.9);

        float hourmins = (hour + minsecs / 60.0F);
        angle = degrees90 - (degrees30 * hourmins);
        DrawHand(e.Graphics, Pens.Black, angle, 0.6);
    }

    private void drawFace(Graphics g)
    {
        int radius = Math.Min(p0.X, p0.Y) - margin;
        g.FillEllipse(Brushes.White, p0.X - radius, p0.Y - radius, radius * 2, radius * 2);

        for (int h = 0; h < 12; h++)
            DrawHand(g, Pens.LightGray, h * degrees30, -0.05);

        for (int m = 0; m < 60; m++)
            DrawHand(g, Pens.LightGray, m * degrees06, -0.025);
    }

    private void DrawHand(Graphics g, Pen pen, float angle, double size)
    {
        int radius = Math.Min(p0.X, p0.Y) - margin;

        int x0 = p0.X + (size > 0 ? 0 : Convert.ToInt32(radius * (1 + size) * Math.Cos(angle)));
        int y0 = p0.Y + (size > 0 ? 0 : Convert.ToInt32(radius * (1 + size) * Math.Sin(-angle)));

        int x1 = p0.X + Convert.ToInt32(radius * (size > 0 ? size : 1) * Math.Cos(angle));
        int y1 = p0.Y + Convert.ToInt32(radius * (size > 0 ? size : 1) * Math.Sin(-angle));

        g.DrawLine(pen, x0, y0, x1, y1);
    }

    [STAThread]
    static void Main()
    {
        Application.Run(new Clock());
    }
}
```



## EasyLang


[https://easylang.online/apps/analog-clock.html Run it]

<lang>func draw hour min sec . .
  # dial
  color 333
  move 50 50
  circle 45
  color 797
  circle 44
  color 333
  for i range 60
    a# = i * 6
    move 50 + sin a# * 40 50 - cos a# * 40
    circle 0.25
  .
  for i range 12
    a# = i * 30
    move 50 + sin a# * 40 50 - cos a# * 40
    circle 1
  .
  # hour
  linewidth 2
  color 000
  a# = (hour * 60 + min) / 2
  move 50 50
  line 50 + sin a# * 32 50 - cos a# * 32
  # min
  linewidth 1.5
  a# = (sec + min * 60) / 10
  move 50 50
  line 50 + sin a# * 40 50 - cos a# * 40
  # sec
  linewidth 1
  color 700
  a# = sec * 6
  move 50 50
  line 50 + sin a# * 40 50 - cos a# * 40
.
on timer
  if t$ <> sys "time"
    t$ = sys "time"
    h$ = sys "time:" & t$
    sec = number substr h$ 17 2
    min = number substr h$ 14 2
    hour = number substr h$ 11 2
    if hour > 12
      hour -= 12
    .
    call draw hour min sec
    timer 0.98
  else
    timer 0.01
  .
.
timer 0
```


=={{header|F_Sharp|F#}}==

```fsharp
open System.Text.RegularExpressions

let numberTemplate = """
 _     _  _     _     __ _  _
/ \ /|  ) _)|_||_  /   /(_)(_) *
\_/  | /_ _)  | _)(_) / (_) /  *
"""
let g =
    numberTemplate.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s ->
        Regex.Matches(s, "...")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.ToString())
        |> Seq.toArray)

let idx c =
    let v c = ((int) c) - ((int) '0')
    let i = v c
    if 0 <= i && i <= 9 then i
    elif c = ':' then 10
    else failwith ("Cannot draw character " + c.ToString())

let draw (s :string) =
    System.Console.Clear()
    g
    |> Array.iter (fun a ->
        s.ToCharArray() |> Array.iter (fun c ->
            let i = idx c
            printf "%s" (a.[i]))
        printfn ""
        )

[<EntryPoint>]
let main argv =
    let showTime _ = draw (System.String.Format("{0:HH:mm:ss}", (System.DateTime.Now)))
    let timer = new System.Timers.Timer(500.)
    timer.AutoReset <- true // The timer triggers cyclically
    timer.Elapsed // An event stream
    |> Observable.subscribe showTime |> ignore // Subscribe to the event stream
    timer.Start() // Now it counts
    System.Console.ReadLine() |> ignore // Until return is hit
    showTime ()
    0
```

{{out}}

```txt
 _  _     _        _  __
  ) _) *   ) /  *  _)  /
 /_ _) *  /_(_) *  _) /
```



## Forth

Display a digital clock in ANS Forth.<BR>
Dependancies:<BR>
1. Assumes there is a video interrupt counter somewhere in the system with 1/60 second interval. <BR>
2. Assumes it is running in a multi-tasking Forth system with the word PAUSE that gives time back to the other tasks on the system.<BR>
3. Assumes a 16 bit CPU.<BR>
4. Assumes big-endian memory organization.<BR>

```Forth

HEX
8379 CONSTANT TICKER   \ address of 1/60 second counter

CREATE PDT ( -- addr) \ bit pattern descriptors for 0..9 and colon
        0038 , 444C , 5464 , 4438 , ( 0)
        0010 , 3010 , 1010 , 1038 , ( 1)
        0038 , 4404 , 1820 , 407C , ( 2)
        007C , 0810 , 0804 , 4438 , ( 3)
        0008 , 1828 , 487C , 0808 , ( 4)
        007C , 4078 , 0404 , 4438 , ( 5)
        0038 , 4040 , 7844 , 4438 , ( 6)
        007C , 0408 , 1020 , 2020 , ( 7)
        0038 , 4444 , 3844 , 4438 , ( 8)
        0038 , 4444 , 3C04 , 0438 , ( 9)
        0000 , 3030 , 0030 , 3000 , ( :)

: ]PDT  ( 0..9 -- addr) [CHAR] 0 - 8 * PDT + ;

: BIG.TYPE ( caddr len -- )
    8 0
    DO
        CR
        2DUP BOUNDS
        ?DO
            I C@ ]PDT J + C@         \ PDT char, byte# J
            2 7 DO                   \ from bit# 7 to 2
                DUP 1 I LSHIFT AND   \ mask out each bit
                IF    [char] * EMIT  \ if true emit a character
                ELSE  SPACE          \ else print space
                THEN
            -1 +LOOP  DROP
        LOOP
    LOOP
    2DROP ;

DECIMAL

CREATE SECONDS  0 , 0 ,   \ 2 CELLS, holds a double integer

: SECONDS++  ( -- )  SECONDS 2@ 1 M+  SECONDS 2! ;

\  subtract old value from new value until ticker changes.
: 1/60  ( -- )
        TICKER DUP @  ( -- addr value)
        BEGIN
             PAUSE    \ *Gives time to other Forth processes while we wait
             OVER @   \ read ticker addr
             OVER -   \ subtract from old value
        UNTIL
        2DROP ;

: SEXTAL ( -- ) 6 BASE ! ;
: 1SEC   ( -- ) 60 0 DO  1/60  LOOP   SECONDS++  ;
: ##:    ( -- ) # SEXTAL # DECIMAL [CHAR] : HOLD  ;
: .TIME  ( d --) <#  ##: ##: # # #> BIG.TYPE ;

: CLOCK  ( -- )
         DECIMAL  \ set task's local radix
         BEGIN
            1SEC
            0 0 AT-XY  SECONDS 2@ .TIME
            ?TERMINAL
         UNTIL
         2DROP ;
```



## Fortran


Uses system commands to clear the screen, sleep and obtain time


```Fortran

!Digital Text implemented as in C version - Anant Dixit (Oct, 2014)
program clock
implicit none
integer :: t(8)
do
  call date_and_time(values=t)
  call sleep(1)
  call system('clear')
  call digital_display(t(5),t(6),t(7))
end do
end program

subroutine digital_display(H,M,S)
!arguments
integer :: H, M, S
!local
character(len=*), parameter :: nfmt='(A8)', cfmt='(A6)'
character(len=88), parameter :: d1 = ' 00000     1     22222   33333      4   5555555  66666  7777777  88888   99999        '
character(len=88), parameter :: d2 = '0     0   11    2     2 3     3    44   5       6     6 7     7 8     8 9     9  ::   '
character(len=88), parameter :: d3 = '0    00  1 1          2       3   4 4   5       6             7 8     8 9     9  ::   '
character(len=88), parameter :: d4 = '0   0 0    1         2        3  4  4   5       6            7  8     8 9     9  ::   '
character(len=88), parameter :: d5 = '0  0  0    1        2      333  4444444 555555  666666      7    88888   999999       '
character(len=88), parameter :: d6 = '0 0   0    1       2          3     4         5 6     6    7    8     8       9  ::   '
character(len=88), parameter :: d7 = '00    0    1      2           3     4         5 6     6   7     8     8       9  ::   '
character(len=88), parameter :: d8 = '0     0    1     2      3     3     4   5     5 6     6  7      8     8 9     9  ::   '
character(len=88), parameter :: d9 = ' 00000  1111111 2222222  33333      4    55555   66666  7        88888   99999        '
integer :: h1, h2, m1, m2, s1, s2
h1 = 1+8*floor(dble(H)/10.D0)
h2 = 1+8*modulo(H,10)
m1 = 1+8*floor(dble(M)/10.D0)
m2 = 1+8*modulo(M,10)
s1 = 1+8*floor(dble(S)/10.D0)
s2 = 1+8*modulo(S,10)

write(*,nfmt,advance='no') d1(h1:h1+8)
write(*,nfmt,advance='no') d1(h2:h2+8)
write(*,cfmt,advance='no') d1(81:88)
write(*,nfmt,advance='no') d1(m1:m1+8)
write(*,nfmt,advance='no') d1(m2:m2+8)
write(*,cfmt,advance='no') d1(81:88)
write(*,nfmt,advance='no') d1(s1:s1+8)
write(*,nfmt) d1(s2:s2+8)

write(*,nfmt,advance='no') d2(h1:h1+8)
write(*,nfmt,advance='no') d2(h2:h2+8)
write(*,cfmt,advance='no') d2(81:88)
write(*,nfmt,advance='no') d2(m1:m1+8)
write(*,nfmt,advance='no') d2(m2:m2+8)
write(*,cfmt,advance='no') d2(81:88)
write(*,nfmt,advance='no') d2(s1:s1+8)
write(*,nfmt) d2(s2:s2+8)

write(*,nfmt,advance='no') d3(h1:h1+8)
write(*,nfmt,advance='no') d3(h2:h2+8)
write(*,cfmt,advance='no') d3(81:88)
write(*,nfmt,advance='no') d3(m1:m1+8)
write(*,nfmt,advance='no') d3(m2:m2+8)
write(*,cfmt,advance='no') d3(81:88)
write(*,nfmt,advance='no') d3(s1:s1+8)
write(*,nfmt) d3(s2:s2+8)

write(*,nfmt,advance='no') d4(h1:h1+8)
write(*,nfmt,advance='no') d4(h2:h2+8)
write(*,cfmt,advance='no') d4(81:88)
write(*,nfmt,advance='no') d4(m1:m1+8)
write(*,nfmt,advance='no') d4(m2:m2+8)
write(*,cfmt,advance='no') d4(81:88)
write(*,nfmt,advance='no') d4(s1:s1+8)
write(*,nfmt) d4(s2:s2+8)

write(*,nfmt,advance='no') d5(h1:h1+8)
write(*,nfmt,advance='no') d5(h2:h2+8)
write(*,cfmt,advance='no') d5(81:88)
write(*,nfmt,advance='no') d5(m1:m1+8)
write(*,nfmt,advance='no') d5(m2:m2+8)
write(*,cfmt,advance='no') d5(81:88)
write(*,nfmt,advance='no') d5(s1:s1+8)
write(*,nfmt) d5(s2:s2+8)

write(*,nfmt,advance='no') d6(h1:h1+8)
write(*,nfmt,advance='no') d6(h2:h2+8)
write(*,cfmt,advance='no') d6(81:88)
write(*,nfmt,advance='no') d6(m1:m1+8)
write(*,nfmt,advance='no') d6(m2:m2+8)
write(*,cfmt,advance='no') d6(81:88)
write(*,nfmt,advance='no') d6(s1:s1+8)
write(*,nfmt) d6(s2:s2+8)

write(*,nfmt,advance='no') d7(h1:h1+8)
write(*,nfmt,advance='no') d7(h2:h2+8)
write(*,cfmt,advance='no') d7(81:88)
write(*,nfmt,advance='no') d7(m1:m1+8)
write(*,nfmt,advance='no') d7(m2:m2+8)
write(*,cfmt,advance='no') d7(81:88)
write(*,nfmt,advance='no') d7(s1:s1+8)
write(*,nfmt) d7(s2:s2+8)

write(*,nfmt,advance='no') d8(h1:h1+8)
write(*,nfmt,advance='no') d8(h2:h2+8)
write(*,cfmt,advance='no') d8(81:88)
write(*,nfmt,advance='no') d8(m1:m1+8)
write(*,nfmt,advance='no') d8(m2:m2+8)
write(*,cfmt,advance='no') d8(81:88)
write(*,nfmt,advance='no') d8(s1:s1+8)
write(*,nfmt) d8(s2:s2+8)

write(*,nfmt,advance='no') d9(h1:h1+8)
write(*,nfmt,advance='no') d9(h2:h2+8)
write(*,cfmt,advance='no') d9(81:88)
write(*,nfmt,advance='no') d9(m1:m1+8)
write(*,nfmt,advance='no') d9(m2:m2+8)
write(*,cfmt,advance='no') d9(81:88)
write(*,nfmt,advance='no') d9(s1:s1+8)
write(*,nfmt) d9(s2:s2+8)

end subroutine

```


Preview:

```txt


 22222   33333           1     88888           1       1
2     2 3     3  ::     11    8     8  ::     11      11
      2       3  ::    1 1    8     8  ::    1 1     1 1
     2        3  ::      1    8     8  ::      1       1
    2      333           1     88888           1       1
   2          3  ::      1    8     8  ::      1       1
  2           3  ::      1    8     8  ::      1       1
 2      3     3  ::      1    8     8  ::      1       1
2222222  33333        1111111  88888        1111111 1111111


```



## FreeBASIC


```freebasic
' version 05-04-2017
' compile with: fbc -s gui

Const As Double deg2rad = Atn(1) / 45
Const As UInteger w = 199, h = 199
Const As UInteger x0 = w \ 2, y0 = h \ 2 ' center

Dim As UInteger x, x1, x2, x3, y, y1, y2, y3
Dim As String sys_time, press
Dim As Integer hours, minutes, seconds
Dim As Double angle, a_sin, a_cos

ScreenRes w, h, 8  ' 8bit color depth (palette)
WindowTitle "Simple Clock"

' create image 8bit (palette) and set pixels to 15 (white)
Dim clockdial As Any Ptr = ImageCreate(w, h, 15, 8)

If clockdial = 0 Then
    Print "Failed to create image."
    Sleep
    End -1
End If

' draw clockdial in memory
Circle clockdial, (x0, y0), 94 ,0
Circle clockdial, (x0, y0), 90 ,0

For x = 0 To 174 Step 6
    a_sin = Sin(x * deg2rad)
    a_cos = Cos(x * deg2rad)

    x1 = 94 * a_sin : y1 = 94 * a_cos
    If x Mod 30 = 0 Then
        x2 = 85 * a_sin : y2 = 85 * a_cos
    Else
        x2 = 90 * a_sin : y2 = 90 * a_cos
    End If

    Line clockdial, (x0 + x1, y0 + y1) - (x0 + x2, y0 + y2), 0
    Line clockdial, (x0 - x1, y0 - y1) - (x0 - x2, y0 - y2), 0
Next

'draw clock
Do
    sys_time = Time
    hours   = (sys_time[0] - Asc("0")) * 10 + sys_time[1] - Asc("0")
    minutes = (sys_time[3] - Asc("0")) * 10 + sys_time[4] - Asc("0")
    seconds = (sys_time[6] - Asc("0")) * 10 + sys_time[7] - Asc("0")

    If hours > 12 Then hours -= 12

    angle = (180 - (hours * 30 + minutes / 2)) * deg2rad
    x1 = 65 * Sin(angle)
    y1 = 65 * Cos(angle)

    angle = (180 - (minutes * 6 + seconds / 10)) * deg2rad
    x2 = 80 * Sin(angle)
    y2 = 80 * Cos(angle)

    angle = (180 - seconds * 6) * deg2rad
    x3 = 90 * Sin(angle)
    y3 = 90 * Cos(angle)

    ScreenLock
    ' load image, setting pixels
    Put (0, 0), clockdial, PSet
    Line (x0, y0) - (x0 + x1, y0 + y1), 1   ' hour hand blue
    Line (x0, y0) - (x0 + x2, y0 + y2), 2   ' minute hand green
    Line (x0, y0) - (x0 + x3, y0 + y3), 12  ' second hand red
    ScreenUnLock

    Sleep 300, 1 ' wait 300 ms, don't respond to keys pressed

    ' press esc or mouse click on close window to stop program
    press = InKey
    If press = Chr(27) Or press = Chr(255) + "k" Then Exit Do

Loop

ImageDestroy(clockdial)

End
```



## FunL



```funl
import concurrent.{scheduleAtFixedRate, scheduler}

val ROW = 10
val COL = 20
val digits = array( [
  "   __",
  " /  /",
  "/__/ ",
  "     ",
  "    /",
  "   / ",
  "   __",
  "  __/",
  "/__  ",
  "   __",
  "  __/",
  " __/ ",
  "     ",
  " /__/",
  "   / ",
  "   __",
  " /__ ",
  " __/ ",
  "   __",
  " /__ ",
  "/__/ ",
  "   __",
  "    /",
  "   / ",
  "   __",
  " /__/",
  "/__/ ",
  "   __",
  " /__/",
  " __/ "
  ] )
val colon = array( [
  "  ",
  " .",
  ". "
  ] )

def displayTime =
  def pad( n ) = if n < 10 then '0' + n else n

  t = $time
  s = (t + $timeZoneOffset)\1000%86400
  time = pad( s\3600 ) + ':' + pad( s%3600\60 ) + ':' + pad( s%60 )

  for row <- 0:3
    print( if $os.startsWith('Windows') then '\n' else '\u001B[' + (ROW + row) + ';' + COL + 'H' )

    for ch <- time
      print( if ch == ':' then colon(row) else digits(int(ch)*3 + row) )

  println()
  t

if not $os.startsWith( 'Windows' )
  print( '\u001B[2J\u001B[?25l' )

scheduleAtFixedRate( displayTime, 1000 - displayTime()%1000, 1000 )
readLine()
scheduler().shutdown()

if not $os.startsWith( 'Windows' )
  print( '\u001B[?25h' )
```


{{out}}

```txt

  __   __     __   __          __
  __/ /  / .  __/    / . /__/    /
/__  /__/ . /__     / .    /    /

```



## Go


```go
package main

import (
	"golang.org/x/net/websocket"
	"flag"
	"fmt"
	"html/template"
	"io"
	"math"
	"net/http"
	"time"
)

var (
	Portnum  string
	Hostsite string
)

type PageSettings struct {
	Host string
	Port string
}

const (
	Canvaswidth  = 512
	Canvasheight = 512
	//color constants
	HourColor   = "#ff7373" // pinkish
	MinuteColor = "#00b7e4" //light blue
	SecondColor = "#b58900" //gold
)

func main() {
	flag.StringVar(&Portnum, "Port", "1234", "Port to host server.")
	flag.StringVar(&Hostsite, "Site", "localhost", "Site hosting server")
	flag.Parse()
	http.HandleFunc("/", webhandler)
	http.Handle("/ws", websocket.Handler(wshandle))
	err := http.ListenAndServe(Hostsite+":"+Portnum, nil)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println("server running")
}

func webhandler(w http.ResponseWriter, r *http.Request) {
	wsurl := PageSettings{Host: Hostsite, Port: Portnum}
	template, _ := template.ParseFiles("clock.html")
	template.Execute(w, wsurl)
}

//Given a websocket connection,
//serves updating time function
func wshandle(ws *websocket.Conn) {
	for {
		hour, min, sec := time.Now().Clock()
		hourx, houry := HourCords(hour, Canvasheight/2)
		minx, miny := MinSecCords(min, Canvasheight/2)
		secx, secy := MinSecCords(sec, Canvasheight/2)
		msg := "CLEAR\n"
		msg += fmt.Sprintf("HOUR %d %d %s\n", hourx, houry, HourColor)
		msg += fmt.Sprintf("MIN %d %d %s\n", minx, miny, MinuteColor)
		msg += fmt.Sprintf("SEC %d %d %s", secx, secy, SecondColor)
		io.WriteString(ws, msg)
		time.Sleep(time.Second / 60.0)
	}
}

//Given current minute or second time(i.e 30 min, 60 minutes)
//and the radius, returns pair of cords to draw line to
func MinSecCords(ctime int, radius int) (int, int) {
	//converts min/sec to angle and then to radians

	theta := ((float64(ctime)*6 - 90) * (math.Pi / 180))
	x := float64(radius) * math.Cos(theta)
	y := float64(radius) * math.Sin(theta)
	return int(x) + 256, int(y) + 256
}

//Given current hour time(i.e. 12, 8) and the radius,
//returns pair of cords to draw line to
func HourCords(ctime int, radius int) (int, int) {
	//converts hours to angle and then to radians
	theta := ((float64(ctime)*30 - 90) * (math.Pi / 180))
	x := float64(radius) * math.Cos(theta)
	y := float64(radius) * math.Sin(theta)
	return int(x) + 256, int(y) + 256
}
```

The following html file, 'clock.html', should be in the same folder as the wsclock binary.

```html
<!DOCTYPE html>
<meta charset="utf-8" />
<title>Clock</title>
<script language="javascript" type="text/javascript">

  var connurl = "ws://{{.Host}}:{{.Port}}/ws";
  //var ctx;
  var secondhand;
  var minutehand;
  var hourhand;
  function wsConnect()
  {
	//get contexts for drawing
    //var canvas = document.getElementById( "canvas" );
    //ctx = canvas.getContext( '2d' );
	var canvas = document.getElementById("rim");
    //draw circle for rim
    rim =  canvas.getContext('2d');
    rim.beginPath();
    rim.arc(256,256,256,0,2*Math.PI);
    rim.stroke();
    //minute hand
    canvas = document.getElementById("minutehand");
    minutehand = canvas.getContext('2d');
    //hour hand
    canvas = document.getElementById("hourhand");
    hourhand = canvas.getContext('2d');
    //second hand
    canvas = document.getElementById("secondhand");
    secondhand = canvas.getContext('2d');

    ws = new WebSocket( connurl );
    ws.onopen = function( e ) {
      console.log( "CONNECTED" );
      ws.send( "READY" );
    };
    /*ws.onclose = function( e ) {
      console.log( "DISCONNECTED" );
    };*/
    ws.onmessage = function( e ) {
      var data = e.data.split("\n");
      for ( var line in data ) {
        var msg = data[line].split(" ");
        var cmd = msg[0];
        if (cmd =="CLEAR"){
          minutehand.clearRect(0,0,512,512);
          secondhand.clearRect(0,0,512,512);
          hourhand.clearRect(0,0,512,512);
        }else if (cmd === "HOUR"){
          renderline(hourhand, msg);
        }else if (cmd === "MIN"){
          renderline(minutehand, msg);
        }else if (cmd === "SEC"){
          renderline(secondhand, msg);
        }else if (cmd ===""){
          cmd = "";
        }else{
          console.log("BAD COMMAND: "+cmd + "; "+msg);
        }
      }
    };
    ws.onerror = function( e ) {
      console.log( 'WS Error: ' + e.data );
    };
  }
  //render line given paramets
  function renderline(ctx, msg){
    ctx.clearRect(0,0,512,512);
    ctx.width = ctx.width;
    var x = parseInt(msg[1],10);
    var y = parseInt(msg[2],10);
    var color = msg[3];
    ctx.strokeStyle = color;
    ctx.beginPath();
    ctx.moveTo(256,256);
    ctx.lineTo(x,y);
    ctx.stroke();
  }

  window.addEventListener( "load", wsConnect, false );

</script>

<body>
    <h2>Clock</h2>

  <canvas id="rim" width="512" height="512" style="position: absolute; left: 0; top: 0; z-index: 0;">
        Sorry, your browser does not support Canvas
  </canvas>
	<canvas id="hourhand" width="512" height="512"style="position: absolute; left: 0; top: 0; z-index: 1;">
        Sorry, your browser does not support Canvas
  </canvas>
	<canvas id="minutehand" width="512" height="512"style="position: absolute; left: 0; top: 0; z-index: 2;">
        Sorry, your browser does not support Canvas
  </canvas>
	<canvas id="secondhand" width="512" height="512"style="position: absolute; left: 0; top: 0; z-index: 3;">
        Sorry, your browser does not support Canvas
  </canvas>

</body>
</html>
```



## GUISS



```guiss
Start,Programs,Accessories,Analogue Clock
```



## Haskell

{{libheader|ansi-terminal}}

```Haskell
import Control.Concurrent
import Data.List
import System.Time

-- Library: ansi-terminal
import System.Console.ANSI

number :: (Integral a) => a -> [String]
number 0 =
  ["██████"
  ,"██  ██"
  ,"██  ██"
  ,"██  ██"
  ,"██████"]
number 1 =
  ["    ██"
  ,"    ██"
  ,"    ██"
  ,"    ██"
  ,"    ██"]
number 2 =
  ["██████"
  ,"    ██"
  ,"██████"
  ,"██    "
  ,"██████"]
number 3 =
  ["██████"
  ,"    ██"
  ,"██████"
  ,"    ██"
  ,"██████"]
number 4 =
  ["██  ██"
  ,"██  ██"
  ,"██████"
  ,"    ██"
  ,"    ██"]
number 5 =
  ["██████"
  ,"██    "
  ,"██████"
  ,"    ██"
  ,"██████"]
number 6 =
  ["██████"
  ,"██    "
  ,"██████"
  ,"██  ██"
  ,"██████"]
number 7 =
  ["██████"
  ,"    ██"
  ,"    ██"
  ,"    ██"
  ,"    ██"]
number 8 =
  ["██████"
  ,"██  ██"
  ,"██████"
  ,"██  ██"
  ,"██████"]
number 9 =
  ["██████"
  ,"██  ██"
  ,"██████"
  ,"    ██"
  ,"██████"]

colon :: [String]
colon =
  ["      "
  ,"  ██  "
  ,"      "
  ,"  ██  "
  ,"      "]

newline :: [String]
newline =
  ["\n"
  ,"\n"
  ,"\n"
  ,"\n"
  ,"\n"]

space :: [String]
space =
  [" "
  ," "
  ," "
  ," "
  ," "]

leadingZero :: (Integral a) => a -> [[String]]
leadingZero num =
  let (tens, ones) = divMod num 10
  in [number tens, space, number ones]

fancyTime :: CalendarTime -> String
fancyTime time =
  let hour   = leadingZero $ ctHour time
      minute = leadingZero $ ctMin time
      second = leadingZero $ ctSec time
      nums   = hour ++ [colon] ++ minute ++ [colon] ++ second ++ [newline]
  in concat $ concat $ transpose nums

main :: IO ()
main = do
  time <- getClockTime >>= toCalendarTime
  putStr $ fancyTime time
  threadDelay 1000000
  setCursorColumn 0
  cursorUp 5
  main
```

Output:
```txt
    ██ ██████      ██████ ██████      ██████ ██████
    ██ ██  ██  ██      ██ ██  ██  ██      ██ ██  ██
    ██ ██████      ██████ ██  ██      ██████ ██  ██
    ██ ██  ██  ██      ██ ██  ██  ██  ██     ██  ██
    ██ ██████      ██████ ██████      ██████ ██████
```



## GUISS



```guiss
Start,Programs,Accessories,Analogue Clock
```


=={{header|Icon}} and {{header|Unicon}}==

Two Examples in Icon:
The clock is resizeable. The clock hands, the displayed hours and the clock itself are resized automatically.

1. Clock using conventional Graphics


```icon

link graphics

global  xsize,
        ysize,
        fontsize

procedure main(args)
    if *args > 0 then xsize := ysize := numeric(args[1])

    /xsize := /ysize := 200
    WIN := WOpen("size=" || xsize || "," || ysize, "label=Clock", "resize=on") | stop("Fenster geht nicht auf!", image(xsize), " - ", image(ysize))
    ziffernblatt()

    repeat
    {   write(&time)

        if *Pending(WIN) > 1 then while *Pending() > 0 do
        {   e := Event()
            ziffernblatt()
        }

        Fg("#CFB53B")
        FillCircle(xsize/2, ysize/2, xsize/2 * 0.81)
        Fg("black")

        clock := &clock
        sec  := clock[7:0]
        min  := clock[4:6]
        hour := clock[1:3]

        if fontsize > 7 then
        {   #Fg("yellow")
            EraseArea(10,0, TextWidth(clock),WAttrib("fheight"))
            DrawString(10,fontsize, clock)
        }

        draw_zeiger(hour, min, sec)

        WFlush()
        delay(100)
    }
end

procedure ziffernblatt()
        xsize := WAttrib("width")
        ysize := WAttrib("height")
        if xsize < ysize then ysize := xsize
        if ysize < xsize then xsize := ysize

        EraseArea(0,0,WAttrib("width"),WAttrib("height"))

        Fg("#CFB53B")
        FillCircle(xsize/2, ysize/2, xsize/2)
        Fg("black")
        fontsize := fontsize := 30 * xsize / 800.0

        every i := 1 to 60 do
        {   winkel := 6 * i / 180.0 * &pi
            if i % 5 = 0 then
            {   laenge := 0.95
                if fontsize > 15 then
                {   Font("mono," || integer(fontsize) || ",bold")
                    WAttrib("linewidth=3")
                }
                if fontsize > 8 then
                {   Font("sans," || integer(fontsize))
                    WAttrib("linewidth=2")
                }

                if fontsize > 8 then DrawString(xsize/2 + 0.90 * xsize/2 * sin(winkel) - fontsize / 2, ysize/2 - 0.90 * ysize/2 * cos(winkel) + fontsize/2, (i/5)("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"))
            }
            else laenge := 0.98
            if fontsize >= 5 then DrawLine(xsize/2 + laenge * xsize/2 * sin(winkel), ysize/2 - laenge * ysize/2 * cos(winkel), xsize/2 + 0.99 * xsize/2 * sin(winkel), ysize/2 - 0.99 * ysize/2 * cos(winkel))
            if fontsize < 5 then if i % 5 = 0 then
            {   WAttrib("linewidth=1")
                DrawLine(xsize/2 + laenge * xsize/2 * sin(winkel), ysize/2 - laenge * ysize/2 * cos(winkel), xsize/2 + 0.99 * xsize/2 * sin(winkel), ysize/2 - 0.99 * ysize/2 * cos(winkel))
            }
        }
        clock := &clock
        sec  := clock[7:0]
        min  := clock[4:6]
        hour := clock[1:3]

        if fontsize > 7 then
        {   EraseArea(10,0, TextWidth(clock),WAttrib("fheight"))
            DrawString(10,fontsize, clock)
        }
        draw_zeiger(hour, min, sec)

        Fg("#D4AF37")
        FillCircle(xsize/2, ysize/2, 5)
        Fg("black")
        WAttrib("linewidth=2")

        DrawCircle(xsize/2, ysize/2,5)

end

procedure draw(laenge, breite, winkel)
    WAttrib("linewidth=" || breite)
    DrawLine(xsize/2,ysize/2,xsize/2 + laenge * sin(winkel), ysize/2 - laenge * cos(winkel))
end

procedure draw_zeiger(h, m, s)
    wh := 30 * ((h % 12) + m / 60.0 + s / 3600.0) / 180 * &pi
    wm := 6 * (m + s / 60.0) / 180.0 * &pi
    ws := 6 * s / 180.0 * &pi

    draw(xsize/2 * 0.5, 5, wh) # Stundenzeiger
    draw(xsize/2 * 0.65, 3, wm) # Minutenzeiger
    draw(xsize/2 * 0.80, 1, ws) # Sekundenzeiger
end


```


2. Clock using Turtle Graphics

```icon

link graphics, turtle

global  xsize,
        ysize,
        fontsize

procedure main(args)
    if *args > 0 then xsize := ysize := numeric(args[1])

    /xsize := /ysize := 200
    WIN := WOpen("size=" || xsize || "," || ysize, "label=Clock", "resize=on") | stop("Fenster geht nicht auf!", image(xsize), " - ", image(ysize))
    ziffernblatt()

    TInit()

#    clocker := create((right("0" || (0 to 23), 2) || ":" || right("0" || (0 to 59), 2) || ":" || right("0" || (0 to 59), 2)))  # simul_clock()

    repeat
    {   write(&time)

        if *Pending(WIN) > 1 then
        {   while *Pending() > 0 do e := Event()
            ziffernblatt()
        }

        Fg("#CFB53B")
        FillCircle(xsize/2, ysize/2, xsize/2 * 0.81)
        Fg("black")

        clock := &clock #clock := @clocker
        sec  := clock[7:0]
        min  := clock[4:6]
        hour := clock[1:3]

        if fontsize > 7 then
        {   altfg := Fg()
            Fg("blue")
            altbg := Bg()
            Bg("black")

            if fh := open("/etc/timezone", "r") then
            {   timezone := read(fh)
                close(fh)
            }

            erase := TextWidth(clock)
            erase <:= TextWidth(&date)
            erase <:= TextWidth(timezone)

            EraseArea(xsize/2 - erase / 2, ysize * 7 / 8, erase, WAttrib("fheight"))
            DrawString(xsize/2 - TextWidth(clock) / 2,ysize * 7 / 8 + WAttrib("fheight") - WAttrib("descent"), clock)

            EraseArea(xsize/2 - erase / 2, ysize * 7 / 8 - WAttrib("fheight"), erase,WAttrib("fheight"))
            DrawString(xsize/2 - TextWidth(&date) / 2,ysize * 7 / 8 - WAttrib("fheight") + WAttrib("fheight") - WAttrib("descent"), &date)


            EraseArea(xsize/2 - erase / 2, ysize * 7 / 8 - 2 * WAttrib("fheight"), erase,WAttrib("fheight"))
            DrawString(xsize/2 - TextWidth(timezone) / 2,ysize * 7 / 8 - 2 * WAttrib("fheight") + WAttrib("fheight") - WAttrib("descent"), timezone)

            Bg(altbg)
            Fg(altfg)
        }

        draw_zeiger(hour, min, sec)

        Fg("#D4AF37")
        FillCircle(xsize/2, ysize/2, 5 * xsize / 400.0)
        Fg("black")
        WAttrib("linewidth=" || 2 * xsize / 400)

        DrawCircle(xsize/2, ysize/2, 5 * xsize / 400.0)

        WAttrib("linewidth=1")

        WFlush()
        delay(50)
    }
end

procedure ziffernblatt()
        xsize := WAttrib("width")
        ysize := WAttrib("height")
        if xsize < ysize then ysize := xsize
        if ysize < xsize then xsize := ysize

        EraseArea(0,0,WAttrib("width"),WAttrib("height"))

        Fg("#CFB53B")
        FillCircle(xsize/2, ysize/2, xsize/2)
        Fg("black")
        fontsize := fontsize := 30 * xsize / 800.0
        WAttrib("linewidth=1")

        every i := 1 to 60 do
        {   winkel := 6 * i / 180.0 * &pi
            TX(xsize/2)
            TY(ysize/2)
            THeading(i * 6)

            if i % 5 = 0 then
            {   laenge := 0.95
                if fontsize > 15 then
                {   Font("mono," || integer(fontsize) || ",bold")
                    WAttrib("linewidth=3")
                }
                if fontsize > 8 then
                {   Font("sans," || integer(fontsize))
                    WAttrib("linewidth=2")
                }

                if fontsize > 8 then DrawString(xsize/2 + 0.90 * xsize/2 * sin(winkel) - fontsize / 2, ysize/2 - 0.90 * ysize/2 * cos(winkel) + fontsize/2, (i/5)("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"))
            }
            else
            {   laenge := 0.98
                if fontsize > 15 then WAttrib("linewidth=3")
                if fontsize >  8 then WAttrib("linewidth=2")
                if fontsize <  5 then WAttrib("linewidth=1")

            }
            if fontsize >= 5 then {TSkip(laenge * xsize/2); TDraw((0.99-laenge) * xsize / 2)}   #DrawLine(xsize/2 + laenge * xsize/2 * sin(winkel), ysize/2 - laenge * ysize/2 * cos(winkel), xsize/2 + 0.99 * xsize/2 * sin(winkel), ysize/2 - 0.99 * ysize/2 * cos(winkel))
            if fontsize < 5 then if i % 5 = 0 then
            {   WAttrib("linewidth=1")
                TSkip(laenge * xsize/2); TDraw((0.99-laenge) * xsize / 2)  #DrawLine(xsize/2 + laenge * xsize/2 * sin(winkel), ysize/2 - laenge * ysize/2 * cos(winkel), xsize/2 + 0.99 * xsize/2 * sin(winkel), ysize/2 - 0.99 * ysize/2 * cos(winkel))
            }
        }
        clock := &clock
        sec  := clock[7:0]
        min  := clock[4:6]
        hour := clock[1:3]

        #if fontsize > 7 then
        #{   EraseArea(10,0, TextWidth(clock),WAttrib("fheight"))
        #    DrawString(10,fontsize, clock)
        #}
        draw_zeiger(hour, min, sec)
end

procedure draw(zeiger, laenge, breite, winkel)
    TX(xsize/2); TY(ysize/2); THeading(winkel - 90)
    WAttrib("linewidth=" || breite)
    TDraw(laenge)
    if zeiger == ("h" | "m" | "s") then
    {
        TSkip((0.05 + breite / 250.0) * xsize / 5)
        WAttrib("linewidth=1")
        TFPoly((0.05 + breite / 250.0) * xsize,3)
    }
    if zeiger == ("h" | "m" | "s") then
    {   Fg("green yellow")
        TFPoly(0.04 * xsize, 3)
        Fg("black")
    }
    WAttrib("linewidth=" || breite)

    if zeiger == "r" then
    {   TSkip(0.025 * xsize)
        TCircle(0.05 * xsize)
    }
    if breite > 7 then
    {   Fg("green yellow")
        TX(xsize/2); TY(ysize/2); THeading(winkel -90)
        TSkip(laenge / 2)
        TFRect(laenge / 2, breite -5)
        Fg("black")
    }
end

procedure draw_zeiger(h, m, s)
    wh := 30 * ((h % 12) + m / 60.0 + s / 3600.0) #/ 180 * &pi
    wm := 6 * (m + s / 60.0) #/ 180.0 * &pi
    ws := 6 * s #/ 180.0 * &pi

    draw("h", xsize/2 * 0.45,20 * xsize / 800, wh) # Stundenzeiger
    draw("r", xsize/2 * 0.15,20 * xsize / 800, wh - 180)

    draw("m", xsize/2 * 0.60,12 * xsize / 800, wm) # Minutenzeiger
    draw("r", xsize/2 * 0.20,12 * xsize / 800, wm - 180)

    draw("s", xsize/2 * 0.70, 4 * xsize / 800, ws) # Sekundenzeiger
    draw("r", xsize/2 * 0.25, 8 * xsize / 800, ws - 180)
end

```



## J


```J

Note'rudimentary 4 second clock'
 advances an arrow at roughly 1 second intervals,
 accurate to the nearest half second.
 Please replace draw with a verb demonstrating one of
 j's fantastic graphical capabilities.
 x draw y
 x are session seconds
 y is the initial value, session seconds at tic start in the example
   tic^:8 seconds''
)

delay=:6!:3    NB. "sleep"
seconds=:6!:1  NB. session time in seconds
Pass_y =: (]`[`)(`:6)  NB. adverb that evaluates the verb and returns y

round =: [: <. 0.5&+   NB. round to nearest integer
PICTURES=: u:16b2190+i.4 NB. whoot arrows
draw=: [: smoutput PICTURES ((|~ #)~ { [) [: round -

tic=: (>. draw Pass_y <.) ([: seconds 0 $ delay@1:)

```

The result of 3.18... is the session time at which the example began.

```txt

tic^:8 seconds''  NB. demonstrate for 8 exciting seconds
↑
→
↓
←
↑
→
↓
←
3.18325

```


Here's a graphical variant:


```J
require'plot'
N=:0.01*i.629
O=: [: j./ 1 2 o./ ]

delay=:6!:3    NB. "sleep"
clock=: [: plot (O N),N*/~0.07 0.11 0.15(*O) 2r24p1 2r60p1 2r60p1*_3{.6!:0 bind ''
delay@1:@clock^:9e99''
```



## Java

{{works with|Java|8}}

```java
import java.awt.*;
import java.awt.event.*;
import static java.lang.Math.*;
import java.time.LocalTime;
import javax.swing.*;

class Clock extends JPanel {

    final float degrees06 = (float) (PI / 30);
    final float degrees30 = degrees06 * 5;
    final float degrees90 = degrees30 * 3;

    final int size = 590;
    final int spacing = 40;
    final int diameter = size - 2 * spacing;
    final int cx = diameter / 2 + spacing;
    final int cy = diameter / 2 + spacing;

    public Clock() {
        setPreferredSize(new Dimension(size, size));
        setBackground(Color.white);

        new Timer(1000, (ActionEvent e) -> {
            repaint();
        }).start();
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawFace(g);

        final LocalTime time  = LocalTime.now();
        int hour = time.getHour();
        int minute = time.getMinute();
        int second = time.getSecond();

        float angle = degrees90 - (degrees06 * second);
        drawHand(g, angle, diameter / 2 - 30, Color.red);

        float minsecs = (minute + second / 60.0F);
        angle = degrees90 - (degrees06 * minsecs);
        drawHand(g, angle, diameter / 3 + 10, Color.black);

        float hourmins = (hour + minsecs / 60.0F);
        angle = degrees90 - (degrees30 * hourmins);
        drawHand(g, angle, diameter / 4 + 10, Color.black);
    }

    private void drawFace(Graphics2D g) {
        g.setStroke(new BasicStroke(2));
        g.setColor(Color.white);
        g.fillOval(spacing, spacing, diameter, diameter);
        g.setColor(Color.black);
        g.drawOval(spacing, spacing, diameter, diameter);
    }

    private void drawHand(Graphics2D g, float angle, int radius, Color color) {
        int x = cx + (int) (radius * cos(angle));
        int y = cy - (int) (radius * sin(angle));
        g.setColor(color);
        g.drawLine(cx, cy, x, y);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Clock");
            f.setResizable(false);
            f.add(new Clock(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

Tested on Gecko.  Put the following in a &lt;script> tag somewhere, and call <code>init_clock()</code> after body load.

```JavaScript
var sec_old = 0;
function update_clock() {
	var t = new Date();
	var arms = [t.getHours(), t.getMinutes(), t.getSeconds()];
	if (arms[2] == sec_old) return;
	sec_old = arms[2];

	var c = document.getElementById('clock');
	var ctx = c.getContext('2d');
	ctx.fillStyle = "rgb(0,200,200)";
	ctx.fillRect(0, 0, c.width, c.height);
	ctx.fillStyle = "white";
	ctx.fillRect(3, 3, c.width - 6, c.height - 6);
	ctx.lineCap = 'round';

	var orig = { x: c.width / 2, y: c.height / 2 };
	arms[1] += arms[2] / 60;
	arms[0] += arms[1] / 60;
	draw_arm(ctx, orig, arms[0] * 30, c.width/2.5 - 15, c.width / 20,  "green");
	draw_arm(ctx, orig, arms[1] * 6,  c.width/2.2 - 10, c.width / 30,  "navy");
	draw_arm(ctx, orig, arms[2] * 6,  c.width/2.0 - 6,  c.width / 100, "maroon");
}

function draw_arm(ctx, orig, deg, len, w, style)
{
	ctx.save();
	ctx.lineWidth = w;
	ctx.lineCap = 'round';
	ctx.translate(orig.x, orig.y);
	ctx.rotate((deg - 90) * Math.PI / 180);
	ctx.strokeStyle = style;
	ctx.beginPath();
	ctx.moveTo(-len / 10, 0);
	ctx.lineTo(len, 0);
	ctx.stroke();
	ctx.restore();
}

function init_clock() {
	var clock = document.createElement('canvas');
	clock.width = 100;
	clock.height = 100;
	clock.id = "clock";
	document.body.appendChild(clock);

	window.setInterval(update_clock, 200);
}
```



###  digital


```javascript
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <style>
        canvas {
            background-color: black;
        }
    </style>
</head>
<body>
    <canvas></canvas>
    <script>
        var canvas = document.querySelector("canvas");
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;

        var g = canvas.getContext("2d");

        // which leds are on or off for each digit
        var masks = ["1110111", "0010010", "1011101", "1011011", "0111010",
            "1101011", "1101111", "1010010", "1111111", "1111011"];

        // horizontal and vertical layouts in scalable units
        var vertices = [
            [
                [0, 0], [1, 1], [7, 1], [8, 0], [7, -1], [1, -1]
            ],
            [
                [0, 0], [-1, 1], [-1, 7], [0, 8], [1, 7], [1, 1]
            ]
        ];

        function Led(x, y, idx, ox, oy) {
            // starting points in scalable units
            this.x = x;
            this.y = y;

            // horizontal or vertical layout
            this.idx = idx;

            // pixel values to create small gaps between the leds
            this.offset_x = ox;
            this.offset_y = oy;
        }

        var leds = [];
        leds.push(new Led(0, 0, 0, 0, -1));
        leds.push(new Led(0, 0, 1, -1, 0));
        leds.push(new Led(8, 0, 1, 1, 0));
        leds.push(new Led(0, 8, 0, 0, 1));
        leds.push(new Led(0, 8, 1, -1, 2));
        leds.push(new Led(8, 8, 1, 1, 2));
        leds.push(new Led(0, 16, 0, 0, 3));

        var onColor, offColor;

        function drawDigitalClock(color1, color2, size) {

            var clockWidth = (6 * 15 + 2 * 10) * size;
            var clockHeight = 20 * size;
            var x = (canvas.width - clockWidth) / 2;
            var y = (canvas.height - clockHeight) / 2;

            onColor = color1;
            offColor = color2;

            g.clearRect(0, 0, canvas.width, canvas.height);

            var date = new Date();
            var segments = [date.getHours(), date.getMinutes(), date.getSeconds()];

            segments.forEach(function (value, index) {
                x = drawDigits(x, y, size, value);
                if (index < 2) {
                    x = drawSeparator(x, y, size);
                }
            });
        }

        function drawDigits(x, y, size, timeUnit) {

            var digit1 = Math.floor(timeUnit / 10);
            var digit2 = timeUnit % 10;

            x = drawLeds(x, y, size, masks[digit1]);
            x = drawLeds(x, y, size, masks[digit2]);

            return x;
        }

        function drawSeparator(x, y, size) {

            g.fillStyle = onColor;
            g.fillRect(x + 0.5 * size, y + 3 * size, 2 * size, 2 * size);
            g.fillRect(x + 0.5 * size, y + 10 * size, 2 * size, 2 * size);

            return x + size * 10;
        }

        function drawLeds(x, y, size, mask) {

            leds.forEach(function (led, i) {

                g.fillStyle = mask[i] == '1' ? onColor : offColor;

                var xx = x + led.x * size + led.offset_x;
                var yy = y + led.y * size + led.offset_y;

                drawLed(xx, yy, size, vertices[led.idx]);
            });

            return x + size * 15;
        }

        function drawLed(x, y, size, vertices) {

            g.beginPath();
            g.moveTo(x, y);

            vertices.forEach(function (vertex) {
                g.lineTo(x + vertex[0] * size, y + vertex[1] * size);
            });

            g.closePath();
            g.fill();
        }

        setInterval(drawDigitalClock, 1000, "#00FF00", "#002200", 12);
    </script>

</body>
</html>
```



## Julia


```julia

using Gtk, Colors, Graphics, Dates

const radius = 300
const win = GtkWindow("Clock", radius, radius)
const can = GtkCanvas()
push!(win, can)

global drawcontext = []

function drawline(ctx, l, color)
    isempty(l) && return
    p = first(l)
    move_to(ctx, p.x, p.y)
    set_source(ctx, color)
    for i = 2:length(l)
        p = l[i]
        line_to(ctx, p.x, p.y)
    end
    stroke(ctx)
end

function clockbody(ctx)
    set_coordinates(ctx, BoundingBox(0, 100, 0, 100))
    rectangle(ctx, 0, 0, 100, 100)
    set_source(ctx, colorant"yellow")
    fill(ctx)
    set_source(ctx, colorant"blue")
    arc(ctx, 50, 50, 45, 45, 360)
    stroke(ctx)
    for hr in 1:12
        radians = hr * pi / 6.0
        drawline(ctx, [Point(50 + 0.95 * 45 * sin(radians),
            50 - 0.95 * 45 * cos(radians)),
            Point(50 + 1.0 * 45 * sin(radians),
            50 - 1.0 * 45 * cos(radians))], colorant"blue")
    end
end

Gtk.draw(can) do widget
    ctx = getgc(can)
    if length(drawcontext) < 1
        push!(drawcontext, ctx)
    else
        drawcontext[1] = ctx
    end
    clockbody(ctx)
end

function update(can)
    dtim = now()
    hr = hour(dtim)
    mi = minute(dtim)
    sec = second(dtim)
    if length(drawcontext) < 1
        return
    end
    ctx = drawcontext[1]
    clockbody(ctx)
    rad = (hr % 12) * pi / 6.0 + mi * pi / 360.0
    drawline(ctx, [Point(50, 50),
        Point(50 + 45 * 0.5 * sin(rad), 50 - 45 * 0.5 * cos(rad))], colorant"black")
    stroke(ctx)
    rad = mi * pi / 30.0  + sec * pi / 1800.0
    drawline(ctx, [Point(50, 50),
        Point(50 + 0.7 * 45 * sin(rad), 50 - 0.7 * 45 * cos(rad))], colorant"darkgreen")
    stroke(ctx)
    rad = sec * pi / 30.0
    drawline(ctx, [Point(50, 50),
        Point(50 + 0.9 * 45 * sin(rad), 50 - 0.9 * 45 * cos(rad))], colorant"red")
    stroke(ctx)
    reveal(can)
end

Gtk.showall(win)
sloc = Base.Threads.SpinLock()
lock(sloc)
signal_connect(win, :destroy) do widget
    unlock(sloc)
end
while !trylock(sloc)
    update(win)
    sleep(1.0)
end

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1

import java.awt.*
import java.time.LocalTime
import javax.swing.*

class Clock : JPanel() {
    private val degrees06: Float = (Math.PI / 30.0).toFloat()
    private val degrees30: Float = degrees06 * 5.0f
    private val degrees90: Float =  degrees30 * 3.0f
    private val size = 590
    private val spacing = 40
    private val diameter = size - 2 * spacing
    private val cx = diameter / 2 + spacing
    private val cy = cx

    init {
        preferredSize = Dimension(size, size)
        background =  Color.white
        Timer(1000) {
            repaint()
        }.start()
    }

    override public fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        drawFace(g)
        val time  = LocalTime.now()
        val hour = time.hour
        val minute = time.minute
        val second = time.second
        var angle: Float = degrees90 - degrees06 * second
        drawHand(g, angle, diameter / 2 - 30, Color.red)
        val minsecs: Float = minute + second / 60.0f
        angle = degrees90 - degrees06 * minsecs
        drawHand(g, angle, diameter / 3 + 10, Color.black)
        val hourmins: Float = hour + minsecs / 60.0f
        angle = degrees90 - degrees30 * hourmins
        drawHand(g, angle, diameter / 4 + 10, Color.black)
    }

    private fun drawFace(g: Graphics2D) {
        g.stroke = BasicStroke(2.0f)
        g.color = Color.yellow
        g.fillOval(spacing, spacing, diameter, diameter)
        g.color = Color.black
        g.drawOval(spacing, spacing, diameter, diameter)
    }

    private fun drawHand(g: Graphics2D, angle: Float, radius: Int, color: Color) {
        val x: Int  = cx + (radius.toDouble() * Math.cos(angle.toDouble())).toInt()
        val y: Int =  cy - (radius.toDouble() * Math.sin(angle.toDouble())).toInt()
        g.color = color
        g.drawLine(cx, cy, x, y)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        f.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        f.title = "Clock"
        f.isResizable = false
        f.add(Clock(), BorderLayout.CENTER)
        f.pack()
        f.setLocationRelativeTo(null)
        f.isVisible = true
    }
}
```



## Liberty BASIC

LB has a timer to call a routine at regular intervals. The example is a cut-down version of the full clock supplied with LB as an example.

```lb

    WindowWidth  =120
    WindowHeight =144
    nomainwin

    open "Clock" for graphics_nsb_nf as #clock
    #clock "trapclose [exit]"
    #clock "fill white"
    for angle =0 to 330 step 30
        #clock "up ; home ; north ; turn "; angle
        #clock "go 40 ; down ; go 5"
    next angle

    #clock "flush"

    timer 1000, [display]
    wait

[display]  ' called only when seconds have changed
    time$   =time$()
    seconds =val( right$( time$, 2))
    ' delete the last drawn segment, if there is one
    if segId >2 then #clock "delsegment "; segId -1
    ' center the turtle
    #clock "up ; home ; down ; north"
    ' erase each hand if its position has changed
    if oldSeconds <>seconds then #clock, "size 1 ; color white ; turn "; oldSeconds *6 ; " ; go 38 ; home ; color black ; north" : oldSeconds =seconds
    ' redraw all three hands, second hand first
    #clock "size 1 ; turn "; seconds * 6 ; " ; go 38"
    ' flush to end segment, then get the next segment id #
    #clock "flush"
    #clock "segment"
    input #clock, segId

    wait

[exit]
    close #clock

    end

```



## Locomotive Basic


Because the Amstrad CPC does not have an RTC, we first have to ask the user for the current time. The seconds hand is drawn in XOR ink mode so that it can be removed without affecting the other hands.


```locobasic
10 mode 1:defint a-y:deg
20 input "Current time (HH:MM)";t$
30 h=val(mid$(t$,1,2))
40 m=val(mid$(t$,4,2))
50 cls
60 r=150:s=-1
70 ph=0:pm=0
80 origin 320,200
90 for a=0 to 360 step 6
100 if a mod 30>0 then z=.9 else z=.8
110 move z*r*sin(a),z*r*cos(a)
120 draw r*sin(a),r*cos(a)
130 next
140 move 0,r
150 for a=0 to 360 step 6
160 draw r*sin(a),r*cos(a)
170 next
180 every 50 gosub 220
190 ' ENDLESS_LOOP
200 goto 200
210 ' NEW_SEC
220 s=s+1
230 if s=60 then s=0:m=m+1
240 if m=60 then m=0:h=h+1
250 if h=24 then h=0
260 if s=0 then gosub 300
270 if s>0 then gosub 420
280 return
290 ' DRAW_ALL
300 locate 1,1
310 print using "##";h;
320 print ":";
330 print using "##";m;
340 frame:move 0,0:draw .5*r*sin(ph),.5*r*cos(ph),0,0
350 frame:move 0,0:draw .7*r*sin(pm),.7*r*cos(pm),0,0
360 frame:move 0,0:draw .8*r*sin(6*59),.8*r*cos(6*59),0,0
370 pm=6*m
380 frame:move 0,0:draw .7*r*sin(pm),.7*r*cos(pm),1,0
390 ph=30*h+.5*m
400 frame:move 0,0:draw .5*r*sin(ph),.5*r*cos(ph),1,0
410 ' DRAW_SEC
420 a=6*s
430 ' uses "frame" and XOR ink mode for drawing -- requires BASIC 1.1
440 if a>0 then frame:move 0,0:draw .8*r*sin(a-6),.8*r*cos(a-6),3,1
450 frame:move 0,0:draw .8*r*sin(a),.8*r*cos(a),3,1
460 return
```



## Lua

==={{libheader|LÖVE}}===
Several nice clocks in the [http://love2d.org/forums/viewtopic.php?f=5&t=77346 LÖVE-forum]

=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
makeHand[fl_, bl_, fw_, bw_] :=  Polygon[{{-bw, -bl}, {bw, -bl}, {fw, fl}, {0, fl + 8 fw}, {-fw, fl}}/9];

hourHand = makeHand[5, 5/3, .1, .3];minuteHand = makeHand[7, 7/3, .1, .3];
secondHand = {Red, EdgeForm[Black], makeHand[7, 7/3, .1/2, .3/2]};

Graphics[{
  {Thickness[.03], Circle[]},(* Rim *)
  {Thickness[.003], Table[Line[{.9 {Cos[a], Sin[a]}, .95 {Cos[a], Sin[a]}}], {a, 0, 2 \[Pi], 2 \[Pi]/60}]}, (* Thin ticks *)
  {Thickness[.01], Table[Line[{.9 {Cos[a], Sin[a]}, .95 {Cos[a], Sin[a]}}], {a, 0, 2 \[Pi], 2 \[Pi]/12}]}, (* Thick ticks *)
  Style[Table[Text[i, .77 {Cos[-i \[Pi]/6 + \[Pi]/2], Sin[-i \[Pi]/6 + \[Pi]/2]}], {i, 1, 12}], FontFamily -> "Helvetica", FontSize -> 36], (* Numbers *)
  Rotate[hourHand, Dynamic[Refresh[-30 Mod[AbsoluteTime[]/3600, 60] \[Degree], UpdateInterval -> 60]], {0, 0}],
  Rotate[minuteHand, Dynamic[Refresh[-6 Mod[AbsoluteTime[]/60, 60] \[Degree], UpdateInterval -> 1]], {0, 0}],
  Rotate[secondHand, Dynamic[Refresh[-6 Mod[AbsoluteTime[], 60] \[Degree], UpdateInterval -> 1/20]], {0, 0}]
  }]
```
[[File:mma_clock.png]]

=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
  u = [0:360]*pi/180;
  while(1)
     s = mod(now*60*24,1)*2*pi;
     plot([0,sin(s)],[0,cos(s)],'-',sin(u),cos(u),'k-');
     pause(1);
  end;
```



## NetRexx

{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import javax.swing.Timer

-- .+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
class RClockSwing public extends JFrame
  -- . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  properties constant
    K_TITLE = String "Clock"
    isTrue = boolean (1 == 1)
    isFalse = \isTrue
  properties inheritable
    content = Container

  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method RClockSwing() public
    this(K_TITLE)
    return

  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method RClockSwing(title = String) public
    super(title)
    initFrame()
    return

  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method initFrame() private
    content = getContentPane()
    content.setLayout(BorderLayout())
    content.add(RClockSwing.Panel(), BorderLayout.CENTER)
    setResizable(isFalse)
    pack()
    return

  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method main(args = String[]) public static
    clockFace = JFrame
    clockFace = RClockSwing()
    clockFace.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    clockFace.setVisible(isTrue)
    return

--..+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
class RClockSwing.Panel shared extends JPanel implements ActionListener
  properties constant
    degrees450 = double Math.PI * 2.5
    degrees006 = double Math.PI / 30.0
    degrees030 = double degrees006 * 5
    size = int 350
    spacing = int 10
    diameter = int size - 2 * spacing
    x1 = int diameter / 2 + spacing
    y1 = int diameter / 2 + spacing

  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method Panel() public
    super()
    initPanel()
    return

  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method initPanel() public
    setPreferredSize(Dimension(size, size))
    setBackground(Color.WHITE)
    ptimer = Timer(1000, this)
    ptimer.start()
    return

  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method paintComponent(gr = Graphics) public
    super.paintComponent(gr)
    g2 = Graphics2D gr
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    gr.setColor(Color.black)
    gr.drawOval(spacing, spacing, diameter, diameter)
    cdate = Calendar.getInstance()
    hours   = cdate.get(Calendar.HOUR)
    minutes = cdate.get(Calendar.MINUTE)
    seconds = cdate.get(Calendar.SECOND)
    angle = double degrees450 - (degrees006 * seconds)
    drawHand(gr, angle, int (diameter / 2 - 10), Color.red)
    minsecs = double (minutes + seconds / 60.0)
    angle = degrees450 - (degrees006 * minsecs)
    drawHand(gr, angle, int (diameter / 3), Color.black)
    hourmins = double (hours + minsecs / 60.0)
    angle = degrees450 - (degrees030 * hourmins)
    drawHand(gr, angle, int (diameter / 4), Color.black)
    return

  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method drawHand(gr = Graphics, angle = double, radius = int, color = Color) public
    x2 = x1 + (int (radius * Math.cos(angle)))
    y2 = y1 + (int (radius * Math.sin(-angle)))
    gr.setColor(color)
    gr.drawLine(x1, y1, x2, y2)
    return

  -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  method actionPerformed(evt = ActionEvent) public
    repaint()
    return

```



## Nim

{{trans|Perl 6}}

```nim
import times, os

const
  t = ["⡎⢉⢵","⠀⢺⠀","⠊⠉⡱","⠊⣉⡱","⢀⠔⡇","⣏⣉⡉","⣎⣉⡁","⠊⢉⠝","⢎⣉⡱","⡎⠉⢱","⠀⠶⠀"]
  b = ["⢗⣁⡸","⢀⣸⣀","⣔⣉⣀","⢄⣀⡸","⠉⠉⡏","⢄⣀⡸","⢇⣀⡸","⢰⠁⠀","⢇⣀⡸","⢈⣉⡹","⠀⠶ "]

while true:
  let x = getClockStr()
  stdout.write "\e[H\e[J"
  for c in x: stdout.write t[c.ord - '0'.ord]
  echo ""
  for c in x: stdout.write b[c.ord - '0'.ord]
  echo ""
  sleep 1000
```



## OCaml


Using only the standard library of OCaml with its [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Graphics.html Graphics] module:


```ocaml
#!/usr/bin/env ocaml
#load "unix.cma"
#load "graphics.cma"
open Graphics

let pi = 4.0 *. atan 1.0
let angle v max = float v /. max *. 2.0 *. pi

let () =
  open_graph "";
  set_window_title "OCaml Clock";
  resize_window 256 256;
  auto_synchronize false;
  let w = size_x ()
  and h = size_y () in
  let rec loop () =
    clear_graph ();

    let point radius r a =
      let x = int_of_float (radius *. sin a)
      and y = int_of_float (radius *. cos a) in
      fill_circle (w/2+x) (h/2+y) r;
    in
    set_color (rgb 192 192 192);
    point 84.0 8 0.0;
    point 84.0 8 (angle  90 360.0);
    point 84.0 8 (angle 180 360.0);
    point 84.0 8 (angle 270 360.0);
    set_color (rgb 224 224 224);
    point 84.0 6 (angle  30 360.0);
    point 84.0 6 (angle  60 360.0);
    point 84.0 6 (angle 120 360.0);
    point 84.0 6 (angle 150 360.0);
    point 84.0 6 (angle 210 360.0);
    point 84.0 6 (angle 240 360.0);
    point 84.0 6 (angle 300 360.0);
    point 84.0 6 (angle 330 360.0);

    set_line_width 9;
    set_color (rgb 192 192 192);
    draw_circle (w/2) (h/2) 100;

    let tm = Unix.localtime (Unix.gettimeofday ()) in
    let sec = angle tm.Unix.tm_sec 60.0 in
    let min = angle tm.Unix.tm_min 60.0 in
    let hour = angle (tm.Unix.tm_hour * 60 + tm.Unix.tm_min) (24.0 *. 60.0) in
    let hour = hour *. 2.0 in

    let hand t radius width color =
      let x = int_of_float (radius *. sin t)
      and y = int_of_float (radius *. cos t) in
      set_line_width width;
      set_color color;
      moveto (w/2) (h/2);  rlineto x y;
    in
    hand sec  90.0 2 (rgb 0 128 255);
    hand min  82.0 4 (rgb 0 0 128);
    hand hour 72.0 6 (rgb 255 0 128);

    synchronize ();
    Unix.sleep 1;
    loop ()
  in
  try loop ()
  with _ -> close_graph ()
```



-----


###  GTK + Cairo


{{libheader|ocaml-cairo}}
{{libheader|LablGTK2}}

Using the libraries GTK2 and Cairo and their OCaml bindings [http://forge.ocamlcore.org/projects/lablgtk/ LablGTK] and [http://forge.ocamlcore.org/projects/cairo/ ocaml-cairo].

 # compile with:
 ocamlopt -I +lablgtk2 -I +cairo -o gtkclock.opt \
          unix.cmxa lablgtk.cmxa cairo.cmxa cairo_lablgtk.cmxa gtkInit.cmx gtkclock.ml


```ocaml
let pi = 4.0 *. atan 1.0
let angle v max = float v /. max *. 2.0 *. pi

let draw area _ =
  let cr = Cairo_lablgtk.create area#misc#window in
  let { Gtk.width = width; Gtk.height = height } = area#misc#allocation in
  let scale p = float (min width height) *. 0.5 *. p in
  let center_x, center_y = float width /. 2.0, float height /. 2.0 in
  let invert_y y = float height -. y in

  Cairo.set_source_rgb cr 0.8 0.8 0.8;
  Cairo.paint cr;  (* background *)

  Cairo.set_source_rgb cr 1.0 1.0 1.0;

  Cairo.arc cr center_x center_y (scale 0.9) 0.0 (2.0 *. pi);
  Cairo.set_line_width cr (scale 0.02);
  Cairo.stroke cr;

  let point a =
    let radius = (scale 0.9) in
    let x = radius *. sin a
    and y = radius *. cos a in
    let r = scale 0.04 in
    Cairo.arc cr (center_x +. x) (invert_y (center_y +. y)) r 0.0 (2.0 *. pi);
    Cairo.fill cr;
  in
  for i = 0 to pred 12 do
    point (angle (i * 30) 360.0)
  done;

  let tm = Unix.localtime (Unix.gettimeofday ()) in
  let sec = angle tm.Unix.tm_sec 60.0 in
  let min = angle tm.Unix.tm_min 60.0 in
  let hour = angle (tm.Unix.tm_hour * 60 + tm.Unix.tm_min) (12.0 *. 60.0) in

  Cairo.set_line_cap cr Cairo.LINE_CAP_ROUND;

  let hand t radius lwidth (r, g, b) =
    let x = radius *. sin t
    and y = radius *. cos t in
    Cairo.set_line_width cr (scale lwidth);
    Cairo.move_to cr center_x center_y;
    Cairo.line_to cr (center_x +. x) (invert_y (center_y +. y));
    Cairo.set_source_rgb cr r g b;
    Cairo.stroke cr;
  in
  hand sec  (scale 0.9) 0.04 (0.0, 0.5, 1.0);
  hand min  (scale 0.7) 0.06 (0.0, 0.0, 0.5);
  hand hour (scale 0.6) 0.09 (1.0, 0.0, 0.5);
  true

let animate area =
  ignore (GMain.Timeout.add 200 (fun () ->
    GtkBase.Widget.queue_draw area#as_widget; true))

let () =
  let w = GWindow.window ~title:"OCaml GtkCairo Clock" () in
  ignore (w#connect#destroy GMain.quit);
  let f = GBin.frame ~shadow_type:`IN ~packing:w#add () in
  let area = GMisc.drawing_area ~width:200 ~height:200 ~packing:f#add () in
  area#misc#set_double_buffered true;
  ignore (area#event#connect#expose (draw area));
  animate area;
  w#show ();
  GMain.main ()
```



## ooRexx


### version 1 runs under Windows

A screenshot of my clock can be seen on my dropbox:

https://www.dropbox.com/sh/h0dycdshv04c5lz/5oHFfI3t14?n=132389230

It runs nicely on Windows 7 with ooRexx installed.

```ooRexx
/* REXX ---------------------------------------------------------------
* 09.02.2014 Walter Pachl with a little, well considerable, help from
*                         a friend (Mark Miesfeld)
* 1) downstripped an example contained in the ooRexx distribution
* 2) constructed the squares for seconds, minutes, and hours
* 3) constructed second-, minute- and hour hand
* 5) removed lots of unnecessary code (courtesy mark Miesfeld again)
* 6) painted the background white
* 7) display date as well as time as text
* 21.02.2014 Attempts to add a minimize icon keep failing
*--------------------------------------------------------------------*/
 d = .drawDlg~new
 if d~initCode <> 0 then do
    say 'The Draw dialog was not created correctly.  Aborting.'
    return d~initCode
 end
 d~execute("SHOWTOP")
 return 0

::requires "ooDialog.cls"
::requires 'rxmath' library

::class 'drawDlg' subclass UserDialog

::attribute interrupted unguarded

::method init
   expose walterFont

   forward class (super) continue
   -- colornames:
   --  1 dark red      7 light grey   13 red
   --  2 dark green    8 pale green   14 light green
   --  3 dark yellow   9 light blue   15 yellow
   --  4 dark blue    10 white        16 blue
   --  5 purple       11 grey         17 pink
   --  6 blue grey    12 dark grey    18 turquoise

   self~interrupted = .true

   -- Create a font to write the nice big letters and digits
   opts = .directory~new
   opts~weight = 700
   walterFont = self~createFontEx("Arial",14,opts)

-- if \self~createcenter(200, 230,"Walter's Clock","MINIMIZEBOX", ,"System",14) then
   if \self~createcenter(200, 230,"Walter's Clock",,,"System",14) then
      self~initCode = 1
-- self~connectDraw(100, "clock", .true)

::method defineDialog
-- self~createPushButton(/*IDC_PB_DRAW*/100,0,0,240,200,"NOTAB OWNERDRAW") -- The drawing surface.
-- self~createPushButton(/*IDC_PB_DRAW*/100,0,0,240,180,"DISABLED NOTAB") -- better. ???
   self~createPushButton(/*IDC_PB_DRAW*/100,0,0,200,200,"DISABLED NOTAB") -- better. ???

   self~createPushButton(IDCANCEL,160,212, 35, 12,,"&Cancel")

::method initDialog unguarded
   expose x y dc myPen change
   change = 0
   x = self~factorx
   y = self~factory
   dc = self~getButtonDC(100)
 --+  myPen   = self~createPen(1,'solid',0)
   t    = .TimeSpan~fromMicroSeconds(500000) -- .5 seconds
   msg  = .Message~new(self, 'clock')
   alrm = .Alarm~new(t, msg)

::method interrupt unguarded

   self~interrupted = .true

::method cancel unguarded   -- Stop the drawing program and quit.
   expose x y
   self~hide
   self~interrupted = .true
   return self~cancel:super

::method leaving unguarded  -- Best place to clean up resources
   expose dc myPen walterFont

 --+  self~deleteObject(myPen)
   self~freeButtonDC(/*IDC_PB_DRAW*/100,dc)
   self~deleteFont(walterFont)

::method clock unguarded                           /* draw individual pixels */
   expose x y dc myPen change walterFont
-- Say 'clock started'
   mx = trunc(20*x); my = trunc(20*y); size = 400

 --+  curPen = self~objectToDC(dc, myPen)

   -- Select the nice big letters and digits into the device context to use to
   -- to write with:
   curFont = self~fontToDC(dc, walterFont)

   -- Create a white brush and select it into the device to paint with.
   whiteBrush = self~createBrush(10)
   curBrush   = self~objectToDC(dc, whiteBrush)

-- Paint the drawing area surface with the white brush
--   self~rectangle(dc, 1, 1, 500, 450, 'FILL') -- how does that relate to the 180 above ???
--   self~rectangle(dc, 1, 1, 480, 400, 'FILL') -- how does that relate to the 180 above ???

   button = self~newPushButton(100)
   clRect = button~clientRect;  -- Say clRect
   self~rectangle(dc, clRect~left+10, clRect~top+10, clRect~right-10, clRect~bottom-10, 'FILL')

   self~transparentText(dc)
   self~writeDirect(dc, 55,20*y,"Walter's Clock")
   self~writeDirect(dc,236, 56,'12')
   self~writeDirect(dc,428,220,'3')
   self~writeDirect(dc,245,375,'6')
   self~writeDirect(dc, 60,220,'9')
   self~opaqueText(dc)

   -- These 5 lines just have the effect of showing "Walter's Clock" first
   -- for a brief instant before the other drawing shows.  If you want it all
   -- to show at once, then remove this.
/*
   if change \= 2 then do
     call msSleep 1000
     change = 2
     end
*/
   self~interrupted = .false

   sec=0
   min=0
   hhh=0
   fact=rxCalcPi()/180
   Parse Value '-1 -1 -1 -1' With hho mmo sso hopo

   do dalpha=0 To 359 by 30 until self~interrupted
     alpha = dalpha*fact
     zxa=trunc(250+124*rxCalcSin(alpha,,'R'))
     zya=trunc(230-110*rxCalcCos(alpha,,'R'))
     hhh=right(hhh,2,0)
     hhh.hhh=right(zxa,3) right(zya,3)
     hhh+=1
     self~draw_square(dc,zxa,zya,3,5)
     self~draw_square(dc,zxa,zya,2,10)
     End
   Do a=0 To 59
     a=right(a,2,0)
     alpha=a*6*fact
     sin.a=rxCalcSin(alpha,,'R')
     cos.a=rxCalcCos(alpha,,'R')
     sin.0mhh.a=sin.a
     cos.0mhh.a=cos.a
     End
   Do hoi=0 To 12*60-1
     hoi=right(hoi,3,0)
     alpha=(hoi/2)*fact
     sin.0hoh.hoi=rxCalcSin(alpha,,'R')
     cos.0hoh.hoi=rxCalcCos(alpha,,'R')
     End
   do dalpha=0 To 359 by 6 until self~interrupted
      alpha = dalpha*fact
      zxa=trunc(250+165*rxCalcSin(alpha,,'R'))
      zya=trunc(230-140*rxCalcCos(alpha,,'R'))
      sec=right(min,2,0)
      sec.sec=right(zxa,3) right(zya,3)
      sec+=1
      self~draw_square(dc,zxa,zya,3,5)
      self~draw_square(dc,zxa,zya,2,10)
      zxa=trunc(250+140*rxCalcSin(alpha,,'R'))
      zya=trunc(230-125*rxCalcCos(alpha,,'R'))
      min=right(min,2,0)
      min.min=right(zxa,3) right(zya,3)
      --Call lineout 'pos.xxx',right(min,2) 'min='min.min
      min+=1
      self~draw_square(dc,zxa,zya,3,5)
      self~draw_square(dc,zxa,zya,2,10)
      End

   do dalpha=0 by 6 until self~interrupted
      alpha=dalpha*fact
      zxa=trunc(250+165*rxCalcSin(alpha,,'R'))
      zya=trunc(230-140*rxCalcCos(alpha,,'R'))
      time=time()
      parse Var time hh ':' mm ':' ss
      If hh>=12 Then hh=right(hh-12,2,0)
      self~writeDirect(dc, 355,40,time)
      date=date()
      self~writeDirect(dc, 355,60,date)
      If hh<>hho Then Do
        If hho>=0 Then Do
          Parse Var hhh.hho hx hy
          self~draw_square(dc,hx,hy,2,10)
          End
        Parse Var hhh.hh hx hy
        self~draw_square(dc,hx,hy,2,2)
        End
      If mm<>mmo Then Do
        If mmo>=0 Then Do
          Parse Var min.mmo mx my
          self~draw_square(dc,mx,my,2,10)
          End
        Parse Var min.mm mx my
        self~draw_square(dc,mx,my,2,2)
        End
      If ss<>sso Then Do
        If sso>=0 Then Do
          Parse Var sec.sso sx sy
          self~draw_square(dc,sx,sy,2,10)
          self~draw_second_hand(dc,sso,sin.,cos.,10)
          End
        Parse Var sec.ss sx sy
        self~draw_square(dc,sx,sy,2, 2)
        self~draw_second_hand(dc,ss,sin.,cos.,16)
        self~draw_square(dc,250,230,4,1)
        hop=right(hh*60+mm,3,0)
        self~draw_hour_hand(dc,hop,sin.,cos.,13)
        self~draw_minute_hand(dc,mm,sin.,cos.,14)
        End
      If mm<>mmo Then Do
        If hopo>=0 Then
          self~draw_hour_hand(dc,hopo,sin.,cos.,10)
        hop=right(hh*60+mm,3,0)
        self~draw_hour_hand(dc,hop,sin.,cos.,13)
        hopo=hop
        If mmo>=0 Then
          self~draw_minute_hand(dc,mmo,sin.,cos.,10)
        self~draw_minute_hand(dc,mm,sin.,cos.,14)
        End
      self~draw_square(dc,250,230,4,1)
      hho=hh
      mmo=mm
      sso=ss

      call msSleep 100
      self~pause
   end
--   if kpix >= size then kpix = 1

   self~interrupted = .true
 --+  self~objectToDC(dc, curPen)
   self~objectToDC(dc, curBrush)

::method pause
   j = msSleep(10)

::method draw_square
  Use Arg dc, x, y, d, c
  Do zx=x-d to x+d
    Do zy=y-d to y+d
      self~drawPixel(dc, zx, zy, c)
      End
    End

::method draw_hour_hand
  Use Arg dc, hp, sin., cos., color
  Do p=1 To 60
    zx=trunc(250+p*sin.0hoh.hp)
    zy=trunc(230-p*cos.0hoh.hp)
    self~draw_square(dc, zx, zy, 2, color)
    End

::method draw_minute_hand
  Use Arg dc, mp, sin., cos., color
  Do p=1 To 80
    zx=trunc(250+p*sin.0mhh.mp)
    zy=trunc(230-p*cos.0mhh.mp)
    self~draw_square(dc, zx, zy, 1, color)
    End

::method draw_second_hand
  Use Arg dc, sp, sin., cos., color
  Do p=1 To 113
    zx=trunc(250+p*sin.sp)
    zy=trunc(230-p*(140/165)*cos.sp)
    self~draw_square(dc, zx, zy, 0, color)
    End

::method quot
  Parse Arg x,y
  If y=0 Then Return '??'
  Else Return x/y
```


===version 2 runs under Windows, Linux, and MacOSX===
{{trans|Java}}
A screenshot of this clock can be seen on my dropbox (clocka.jpg)

https://www.dropbox.com/sh/h0dycdshv04c5lz/5oHFfI3t14?n=132389230

```oorexx
/* REXX ---------------------------------------------------------------
   Name: clock.rxj
   Purpose: create a graphical clock that shows the current time
            -- modelled after the Java program
               at <?http:?//rosettacode.?org/wiki/Draw_a_clock#Java>?

   Needs: - ooRexx (cf. https://sourceforge.net/projects/oorexx/ )
          - BSF4ooRexx (Rexx-Java-bridge, cf.
               https://sourceforge.net/projects/bsf4oorexx/ )
          - Java (cf. http://www.java.com )
   Created: 2014-09-04
   Author:  Rony G. Flatscher
*--------------------------------------------------------------------*/
   -- import Java classes, make them available as ooRexx classes
call bsf.import "java.awt.Color"         , "awtColor"
call bsf.import "java.awt.RenderingHints", "awtRenderingHints"
call bsf.import "java.lang.Math"         , "jMath"
call bsf.import "javax.swing.JFrame"     , "swingJFrame"
call bsf.import "javax.swing.Timer"      , "swingTimer"

rxClock=.RexxClock~new                 -- create Rexx clock object
jrxClock=BSFCreateRexxProxy(rxClock)   -- box Rexx object into a Java object (a Java RexxProxy)

   /* extend Java class JPanel, make sure 'paintComponent' method invocations will get
      forwarded to a RexxProxy object that needs to be supplied upon instantiating this
      extended Java class; this method is defined in JPanel's superclass 'javax.swing.JComponent'  */
exjClz=bsf.createProxyClass("javax.swing.JPanel", "RexxJavaClock", "javax.swing.JComponent paintComponent")
javaClock=exjClz~new(jrxClock)   -- create a Java object, supply it the Java RexxProxy that processes method invocations
javaClock~setPreferredSize(.bsf~new("java.awt.Dimension", rxClock~size, rxClock~size))
javaClock~setBackground(.awtColor~white)

   -- create a JFrame, configure it a little bit
f=.swingJFrame~new
f~defaultCloseOperation=.swingJFrame~EXIT_ON_CLOSE
f~title                ="ooRexx Clock"
f~resizable            =.false
   -- add the clock (a JPanel) to it
f~contentPane~add(javaClock, bsf.loadClass("java.awt.BorderLayout")~CENTER)
f~pack                        -- let the layout manager do its work
f~locationRelativeTo   =.nil  -- no specific location (will be centered)

   /* create Rexx object that sends repaint messages to cause the clock to be updated whenever
      the swing Timer (see below) issues the "actionPerformed" event; to release the lock when
      the 'windowClosing' event is issued    */
rxEH=.RexxEventHandler~new

   /* box Rexx object as a Java object, supply the Java object (javaClock) as user data (will be
      be made available under the entry name "userdata" in the slotDir directory, appended
      to callbacks as additional argument); declare this Java proxy object to implement
      the interfaces 'java.awt.event.ActionListener' and 'java.awt.event.WindowListener'  */
jrxEH=BSFCreateRexxProxy(rxEH, javaClock, "java.awt.event.ActionListener", "java.awt.event.WindowListener")

   /* SwingTimer will cause every second the actionPerformed() event to be issued,
      bsf.dispatch() to bypass ooRexx method resolution into .Object (has a 'start' method)  */
.swingTimer~new(1000, jrxEH)~bsf.dispatch("start")
f~addWindowListener(jrxEH)  -- this allows us to get notified when the JFrame gets closed
f~~setVisible(.true)~~toFront -- show JFrame, make sure it is in the very front

say "..." pp(.DateTime~new) "Rexx main program, now waiting until JFrame gets closed ..."
rxEH~wait      -- wait
say "..." pp(.DateTime~new) "Rexx main program, JFrame got closed."


::requires "BSF.CLS"    -- get the Java camouflaging support for ooRexx

/* This class controls the painting of the clock.  */
::class RexxClock          -- will be used for an extension of javax.swing.JPanel overriding paintComponent
::method init              -- constructor, used for initializing
  expose degrees06 degrees30 degrees90 size spacing diameter x y

  degrees06 = .JMath~toRadians(6)
  degrees30 = degrees06 * 5
  degrees90 = degrees30 * 3

  size = 550
  spacing = 20;
  diameter = size - 2 * spacing
  x = trunc(diameter / 2) + spacing
  y = trunc(diameter / 2) + spacing

::attribute size get       -- make size accessible for clients

::method paintComponent
  expose degrees06 degrees30 degrees90 size spacing diameter x y
  use arg g, slotDir
  -- call dump2 slotDir, .datetime~new "- paintComponent's slotDir:"

  jobj=slotDir~javaObject  -- as the Java object invoked paintComponent the message to the rexx object will supply that Java object
  jobj~paintComponent_forwardToSuper(g)   -- now invoke the method in the (Java) superclass first

  g~setRenderingHint(.awtRenderingHints~KEY_ANTIALIASING, .awtRenderingHints~VALUE_ANTIALIAS_ON)

  g~setColor(.awtColor~black)
  g~drawOval(spacing, spacing, diameter, diameter)
  date=.dateTime~new       -- use ooRexx' date and time

  angle = degrees90 - (degrees06 * date~seconds)
  self~drawHand(g, angle, diameter / 2 - 30, .awtColor~red)

  minsecs = (date~minutes + date~seconds / 60)
  angle = degrees90 - (degrees06 * minsecs)
  self~drawHand(g, angle, diameter / 3 + 10, .awtColor~green)

  hourmins = (date~hours + minsecs / 60)
  angle = degrees90 - (degrees30 * hourmins)
  self~drawHand(g, angle, diameter / 4 + 10, .awtColor~black)


 ::method drawHand
   expose x y
   use arg g, angle, radius, color

   x2 = trunc(x + radius * .jMath~cos(angle))
   y2 = trunc(y + radius * .jMath~sin(-angle))  -- flip y-axis
   g~setColor(color)
   g~drawLine(x, y, x2, y2)



/* The following Rexx class implements the event handlers for a java.awt.event.WindowListener to be
   able to learn when the JFrame gets closed (event "windowClosing").

   In addition it implements the java.awt.event.ActionListener for updating the clock every second
   (using a swing Timer that causes the "actionPerformed" event to be issued).
*/
::class RexxEventHandler
::method init     -- constructor for initialization
  expose wait     -- object variable to serve as a control variable
  wait=.true      -- initialize lock

::method wait     -- method to allow for blocking
  expose wait
  guard on when wait<>.true   -- the caller will be blocked until this condition turns to .false

::method windowClosing        -- Window event when window gets closed, release wait lock
  expose wait
  wait=.false     -- release lock

::method unknown  -- catch all other window-events

::method actionPerformed   -- this event will be caused every second by the swing Timer
  use arg eventObj, slotDir
  slotDir~userData~repaint -- fetch the Java object and send it the repaint message
```

[[out}}

```txt
... [2017-01-26T17:17:51.527000] Rexx main program, now waiting until JFrame gets closed ...
... [2017-01-26T17:17:58.762000] Rexx main program, JFrame got closed.
```



## Perl


{{trans|Perl 6}}

```perl
use utf8;                # interpret source code as UTF8
binmode STDOUT, ':utf8'; # allow printing wide chars without warning
$|++;                    # disable output buffering

my ($rows, $cols) = split /\s+/, `stty size`;
my $x = int($rows / 2 - 1);
my $y = int($cols / 2 - 16);

my @chars = map {[ /(...)/g ]}
            ("┌─┐  ╷╶─┐╶─┐╷ ╷┌─╴┌─╴╶─┐┌─┐┌─┐   ",
             "│ │  │┌─┘╶─┤└─┤└─┐├─┐  │├─┤└─┤ : ",
             "└─┘  ╵└─╴╶─┘  ╵╶─┘└─┘  ╵└─┘╶─┘   ");

while (1) {
    my @indices = map { ord($_) - ord('0') } split //,
                  sprintf("%02d:%02d:%02d", (localtime(time))[2,1,0]);

    clear();
    for (0 .. $#chars) {
      position($x + $_, $y);
      print "@{$chars[$_]}[@indices]";
    }
    position(1, 1);

    sleep 1;
}

sub clear { print "\e[H\e[J" }
sub position { printf "\e[%d;%dH", shift, shift }
```


{{out}}

```txt

     ╷ ┌─╴     ╶─┐ ┌─┐     ┌─┐ ┌─╴
     │ ├─┐  :  ┌─┘ │ │  :  │ │ └─┐
     ╵ └─┘     └─╴ └─┘     └─┘ ╶─┘

```



## Perl 6


```perl6
my ($rows,$cols) = qx/stty size/.words;
my $v = floor $rows / 2;
my $h = floor $cols / 2 - 16;

my @t = < ⡎⢉⢵ ⠀⢺⠀ ⠊⠉⡱ ⠊⣉⡱ ⢀⠔⡇ ⣏⣉⡉ ⣎⣉⡁ ⠊⢉⠝ ⢎⣉⡱ ⡎⠉⢱ ⠀⠶⠀>;
my @b = < ⢗⣁⡸ ⢀⣸⣀ ⣔⣉⣀ ⢄⣀⡸ ⠉⠉⡏ ⢄⣀⡸ ⢇⣀⡸ ⢰⠁⠀ ⢇⣀⡸ ⢈⣉⡹ ⠀⠶⠀>;

loop {
    my @x = DateTime.now.Str.substr(11,8).ords X- ord('0');
    print "\e[H\e[J";
    print "\e[$v;{$h}H";
    print ~@t[@x];
    print "\e[{$v+1};{$h}H";
    print ~@b[@x];
    print "\e[H";
    sleep 1;
}
```

{{out}}

```txt
⠀⢺⠀ ⢀⠔⡇ ⠀⠶⠀ ⠊⠉⡱ ⠊⣉⡱ ⠀⠶⠀ ⣏⣉⡉ ⡎⢉⢵
⢀⣸⣀ ⠉⠉⡏ ⠀⠶⠀ ⣔⣉⣀ ⢄⣀⡸ ⠀⠶⠀ ⢄⣀⡸ ⢗⣁⡸
```



## Phix

Requires 0.7.6 or later. Resizeable, appearance similar to Mathematica.
{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\Clock.exw
--
include pGUI.e

constant USE_OPENGL = 01

Ihandle dlg, canvas, hTimer
cdCanvas cd_canvas

procedure draw_hand(atom degrees, atom r, baseangle, baselen, cx, cy)
    atom a = PI-(degrees+90)*PI/180
    -- tip
    atom x1 = cos(a)*(r)
    atom y1 = sin(a)*(r)
    -- base
    atom x2 = cos(a+PI-baseangle)*baselen
    atom y2 = sin(a+PI-baseangle)*baselen
    atom x3 = cos(a+PI+baseangle)*baselen
    atom y3 = sin(a+PI+baseangle)*baselen
    cdCanvasLineWidth(cd_canvas,1)
    cdCanvasLine(cd_canvas,cx+x1,cy+y1,cx+x2,cy+y2)
    cdCanvasLine(cd_canvas,cx+x2,cy+y2,cx+x3,cy+y3)
    cdCanvasLine(cd_canvas,cx+x3,cy+y3,cx+x1,cy+y1)
    cdCanvasBegin(cd_canvas,CD_FILL)
    cdCanvasVertex(cd_canvas,cx+x1,cy+y1)
    cdCanvasVertex(cd_canvas,cx+x2,cy+y2)
    cdCanvasVertex(cd_canvas,cx+x3,cy+y3)
    cdCanvasEnd(cd_canvas)
end procedure

procedure draw_clock(atom cx, cy, d)
atom w = 2+floor(d/25)
    cdCanvasFont(cd_canvas, "Helvetica", CD_PLAIN, floor(d/15))
    cdCanvasLineWidth(cd_canvas, w)
    cdCanvasArc(cd_canvas, cx, cy, d, d, 0, 360)
    d -= w+8
    w = 1+floor(d/50)
    for i=6 to 360 by 6 do
        integer h = remainder(i,30)=0
        cdCanvasLineWidth(cd_canvas, floor(w*(1+h)/3))
        atom a = PI-(i+90)*PI/180
        atom x1 = cos(a)*d/2, x2 = cos(a)*(d/2-w*(2+h)*.66)
        atom y1 = sin(a)*d/2, y2 = sin(a)*(d/2-w*(2+h)*.66)
        cdCanvasLine(cd_canvas, cx+x1, cy+y1, cx+x2, cy+y2)
        if h then
            x1 = cos(a)*(d/2-w*4.5)
            y1 = sin(a)*(d/2-w*4.5)
            cdCanvasText(cd_canvas,cx+x1,cy+y1,sprintf("%d",{i/30}))
        end if
    end for
    atom {hour,mins,secs,msecs} = date(true)[DT_HOUR..DT_MSEC]
    if IupGetInt(hTimer,"TIME")<1000 then
        -- (if showing once a second, always land on exact
        --  seconds, ie completely ignore msecs, otherwise
        --  show smooth running (fractional) second hand.)
        secs += msecs/1000
    end if
    mins += secs/60
    hour += mins/60
    atom r = d/2
    draw_hand(hour*360/12,r-w*9,0.3,d/20,cx,cy)
    draw_hand(mins*360/60,r-w*2,0.2,d/16,cx,cy)
    cdCanvasSetForeground(cd_canvas, CD_RED)
    draw_hand(secs*360/60,r-w*2,0.05,d/16,cx,cy)
    cdCanvasSetForeground(cd_canvas, CD_BLACK)
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
integer {width, height} = IupGetIntInt(canvas, "DRAWSIZE")
integer r = floor(min(width,height)*0.9)
integer cx = floor(width/2)
integer cy = floor(height/2)
    cdCanvasActivate(cd_canvas)
    cdCanvasClear(cd_canvas)
    draw_clock(cx,cy,r)
    cdCanvasFlush(cd_canvas)
    return IUP_DEFAULT
end function

function timer_cb(Ihandle /*ih*/)
    IupUpdate(canvas)
    return IUP_IGNORE
end function

function map_cb(Ihandle ih)
    if USE_OPENGL then
        atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
        IupGLMakeCurrent(canvas)
        cd_canvas = cdCreateCanvas(CD_GL, "10x10 %g", {res})
    else
        cd_canvas = cdCreateCanvas(CD_IUPDBUFFER, canvas)
    end if
    cdCanvasSetBackground(cd_canvas, CD_WHITE)
    cdCanvasSetForeground(cd_canvas, CD_BLACK)
    {} = cdCanvasTextAlignment(cd_canvas, CD_CENTER)
    return IUP_DEFAULT
end function

function canvas_resize_cb(Ihandle /*canvas*/)
    if USE_OPENGL then
        integer {canvas_width, canvas_height} = IupGetIntInt(canvas, "DRAWSIZE")
        atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
        cdCanvasSetAttribute(cd_canvas, "SIZE", "%dx%d %g", {canvas_width, canvas_height, res})
    end if
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    if USE_OPENGL then
        canvas = IupGLCanvas()
    else
        canvas = IupCanvas()
    end if
    IupSetAttribute(canvas, "RASTERSIZE", "350x350") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))
    IupSetCallback(canvas, "RESIZE_CB", Icallback("canvas_resize_cb"))

    hTimer = IupTimer(Icallback("timer_cb"), 40)    -- smooth secs
--  hTimer = IupTimer(Icallback("timer_cb"), 1000)  -- tick seconds

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Clock")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))

    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupSetAttribute(canvas, "RASTERSIZE", NULL) -- release the minimum limitation
    IupMainLoop()
    IupClose()
end procedure

main()
```

The distribution also contains demo\tinEWGdemo\tindemo\clock.exw, which is a win32-only digital affair (whereas the above should be fine on 32/64 and win/lnx).


## PicoLisp

This is an animated ASCII drawing of the "Berlin-Uhr", a clock built to display the time according to the principles of set theory, which is installed in Berlin since 1975. See [http://www.surveyor.in-berlin.de/berlin/uhr/indexe.html www.surveyor.in-berlin.de/berlin/uhr/indexe.html].
```PicoLisp
(de draw Lst
   (for L Lst
      (for X L
         (cond
            ((num? X) (space X))
            ((sym? X) (prin X))
            (T (do (car X) (prin (cdr X)))) ) )
      (prinl) ) )

(de bigBox (N)
   (do 2
      (prin "|")
      (for I 4
         (prin (if (> I N) "          |" "
### ==
 |")) )
      (prinl) ) )

(call 'clear)          # Clear screen
(call "tput" "civis")  # Set cursor invisible

(push '*Bye '(call "tput" "cnorm"))  # Set cursor visible on exit

(loop
   (call "tput" "cup" 0 0)  # Cursor to top left
   (let Time (time (time))
      (draw (20 (5 . _)) (19 / 5 \\))
      (if (onOff (NIL))
         (draw (18 / 7 \\) (18 \\ 7 /))
         (draw (18 / 2 (3 . "#") 2 \\) (18 \\ 2 (3 . "#") 2 /)) )
      (draw
         (19 \\ (5 . _) /)
         (+ (10 . -) + (10 . -) + (10 . -) + (10 . -) +) )
      (bigBox (/ (car Time) 5))
      (draw (+ (10 . -) + (10 . -) + (10 . -) + (10 . -) +))
      (bigBox (% (car Time) 5))
      (draw (+ (43 . -) +))
      (do 2
         (prin "|")
         (for I `(range 5 55 5)
            (prin
               (cond
                  ((> I (cadr Time)) "   |")
                  ((=0 (% I 3)) " # |")
                  (T " = |") ) ) )
         (prinl) )
      (draw (+ (43 . -) +))
      (bigBox (% (cadr Time) 5))
      (draw (+ (10 . -) + (10 . -) + (10 . -) + (10 . -) +)) )
   (wait 1000) )
```
The six '#' characters in the "circle" on top toggle on/off every second. This is the display at 17:46

```txt
                    _____
                   /     \
                  /  ###  \
                  \  ###  /
                   \_____/
+----------+----------+----------+----------+
|
### ==
 |
### ==
 |
### ==
 |          |
|
### ==
 |
### ==
 |
### ==
 |          |
+----------+----------+----------+----------+
|
### ==
 |
### ==
 |          |          |
|
### ==
 |
### ==
 |          |          |
+-------------------------------------------+
| = | = | # | = | = | # | = | = | # |   |   |
| = | = | # | = | = | # | = | = | # |   |   |
+-------------------------------------------+
|
### ==
 |          |          |          |
|
### ==
 |          |          |          |
+----------+----------+----------+----------+
```



## PureBasic

[[File:PureBasic_Clock.png|thumb|Sample display of PureBasic solution]]

```purebasic
#MiddleX = 90 + 1 ;x,y must be odd numbers, minimum width is 67
#MiddleY = #MiddleX
#len_sh = (#MiddleX - 8) * 0.97 ;length of second-hand
#len_mh = (#MiddleX - 8) * 0.88 ;length of minute-hand
#len_hh = (#MiddleX - 8) * 0.66 ;length of hour-hand
#clockFace_img = 0
#clock_gad = 0
#clock_win = 0

Define cx = #MiddleX, cy = #MiddleY, i, ri.f
Define c_gray = RGB($CC, $CC, $CC), c_mgray = RGB($99, $99, $99)
Define c_white = RGB(255, 255, 255), c_black =RGB(0, 0, 0)
Define c_red = RGB(255, 0, 0), c_blue = RGB(0, 0, 255)
Define c_dcyan = RGB($27, $BC, $D8), c_lgreen = RGB($60, $E0, $9)
Define c_yellow = RGB($F4, $D5, $0B)

CreateImage(#clockFace_img, cx * 2 - 1, cy * 2 - 1)
StartDrawing(ImageOutput(#clockFace_img))
  Box(0, 0, cx * 2 - 1, cy * 2 - 1, c_mgray)
  Circle(cx, cy, cx - 2, c_dcyan)
  For i = 0 To 359 Step 30
    ri = Radian(i)
    Circle(cx + Sin(ri) * (cx - 5), cy + Cos(ri) * (cx - 5), 3, c_gray)
  Next
StopDrawing()
OpenWindow(#clock_win, 0, 0, cx * 2, cy * 2, "Clock")
ImageGadget(#clock_gad, 0, 0, cx * 2, cy * 2, ImageID(#clockFace_img))

Define x, y, rad_s.f, rad_m.f, rad_h.f, t$
Repeat
  event = WaitWindowEvent(25)
  If event = 0
    rad_s = Radian(360 - (Second(Date()) * 6) + 180)
    rad_m = Radian(360 - (Minute(Date()) * 6) + 180)
    rad_h = Radian(360 - (((Hour(Date()) - 1) * 30) + 180) - (Minute(Date()) / 2))

    StartDrawing(ImageOutput(#clockFace_img))
      Circle(cx, cy, cx - 8, c_lgreen)
      t$ = FormatDate("%mm-%dd-%yyyy", Date())
      x = cx - (TextWidth(t$) + 2) / 2
      y = (cy - (TextHeight(t$) + 2) - 4) / 2
      Box(x, y, TextWidth(t$) + 2, TextHeight(t$) + 2, c_black)
      DrawText(x + 2, y + 2, t$, c_black, c_yellow)
      LineXY(cx, cy, cx + Sin(rad_s) * #len_sh, cy + Cos(rad_s) * #len_sh, c_white)
      LineXY(cx, cy, cx + Sin(rad_m) * #len_mh, cy + Cos(rad_m) * #len_mh, c_red)
      LineXY(cx, cy, cx + Sin(rad_h) * #len_hh, cy + Cos(rad_h) * #len_hh, c_black)
      Circle(cx, cy, 4, c_blue)
    StopDrawing()
    SetGadgetState(#clock_gad, ImageID(#clockFace_img))
  EndIf
Until event = #PB_Event_CloseWindow
```


## Python

[http://www.thinkgeek.com/gadgets/watches/6a17/ Think Geek Binary Clock]
{{works with|Python|2.6+, 3.0+}}

### Textmode



```python
import time

def chunks(l, n=5):
    return [l[i:i+n] for i in range(0, len(l), n)]

def binary(n, digits=8):
    n=int(n)
    return '{0:0{1}b}'.format(n, digits)

def secs(n):
    n=int(n)
    h='x' * n
    return "|".join(chunks(h))

def bin_bit(h):
    h=h.replace("1","x")
    h=h.replace("0"," ")
    return "|".join(list(h))


x=str(time.ctime()).split()
y=x[3].split(":")

s=y[-1]
y=map(binary,y[:-1])

print bin_bit(y[0])
print
print bin_bit(y[1])
print
print secs(s)
```


==={{libheader|VPython}}===
There is a 3D analog clock in the
[http://www.vpython.org/contents/contributed/cxvp_clock.py VPython contributed section]


## Racket


Draws an analog clock in a new GUI window:


```racket

#lang racket/gui

(require racket/date slideshow/pict)

(define (clock h m s [r 100])
  (define (draw-hand length angle
                     #:width [width 1]
                     #:color [color "black"])
    (dc (λ (dc dx dy)
          (define old-pen (send dc get-pen))
          (send dc set-pen (new pen% [width width] [color color]))
          (send dc draw-line
                (+ dx r) (+ dy r)
                (+ dx r (* length (sin angle)))
                (+ dy r (* length (cos angle))))
          (send dc set-pen old-pen))
      (* 2 r) (* 2 r)))
  (cc-superimpose
   (for/fold ([pict (circle (* 2 r))])
             ([angle (in-range 0 (* 2 pi) (/ pi 6))]
              [hour (cons 12 (range 1 12))])
     (define angle* angle)
     (define r* (* r 0.8))
     (define txt (text (number->string hour) '(bold . "Helvetica")))
     (define x (- (* r* (sin angle*)) (/ (pict-width txt) 2)))
     (define y (+ (* r* (cos angle*)) (/ (pict-height txt) 2)))
     (pin-over pict (+ r x) (- r y) txt))
   (draw-hand (* r 0.7) (+ pi (* (modulo h 12) (- (/ pi 6))))
              #:width 3)
   (draw-hand (* r 0.5) (+ pi (* m (- (/ pi 30))))
              #:width 2)
   (draw-hand (* r 0.7) (+ pi (* s (- (/ pi 30))))
              #:color "red")
   (disk (* r 0.1))))

(define f (new frame% [label "Clock"] [width 300] [height 300]))

(define c
  (new canvas%
       [parent f]
       [paint-callback
        (λ (c dc)
          (define date (current-date))
          (draw-pict (clock (date-hour date)
                            (date-minute date)
                            (date-second date)
                            (/ (send c get-width) 2))
                     dc 0 0))]))

(define t
  (new timer%
       [notify-callback (λ () (send c refresh-now))]
       [interval 1000]))

(send f show #t)

```



## REXX

This REXX program draws a digital clock;   it shows the seconds if the terminal screen is wide enough.

The   '''$T.REX'''   program does the heavy lifting of actually creating the blocked characters.

If using
:::* '''PC/REXX'''
:::* '''Personal REXX'''
:::* '''R4'''
:::* '''ROO'''
the color of the display can be specified.

The   '''$CLOCK.REX'''   REXX program makes use of   '''$T.REX'''   REXX program which is used to display text and/or create big blocked characters.

The   '''$T.REX'''   REXX program is included here   ──►   [[$T.REX]].

The help for the   '''$T.REX'''   REXX program is included here   ──►   [[$T.HEL]].

The   '''$CLOCK.REX'''   REXX program makes use of   '''$ERR.REX'''   REXX program which is used to display error messages (via   '''$T.REX''').

The   '''$ERR'''   REXX program is included here   ──►   [[$ERR.REX]].

This REXX program makes use of   '''SCRSIZE'''   REXX program (or BIF) which is used to determine the screen size of the terminal (console).

The   '''SCRSIZE.REX'''   REXX program is included here   ──►   [[SCRSIZE.REX]].

Some REXXes have the   '''SCRSIZE'''   routine as a BIF.

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ──►   [[CHANGESTR.REX]].

REXX programs not included are   '''$H.REX'''   which shows '''help''' and other documentation.

```rexx
/**/trace o;parse arg !;if !all(arg()) then exit;if !cms then address ''
signal on halt; signal on novalue; signal on syntax
parse var ! ops;   ops = space(ops)    /*obtain  command line  options. */
@abc = 'abcdefghijklmnopqrstuvwxyz'    /*alphabet str used by ABB/ABBN. */
blinkSecs = 1
creep     = 1
tops      = '.C=blue .BC=░ .BS=1 .BLOCK=12'

  do while ops\=='';    parse var ops _1 2 1 _ . 1 y ops;    upper _
    select
    when _==','                    then nop
    when _1=='.' & pos("=",_)\==0  then tops=tops y
    when abbn('BLINKSECs')         then blinksecs=no()
    when abbn('CREEPs')            then creep=no()
    otherwise                      call er 55,y
    end   /*select*/
  end     /*while ops¬==''*/

if \!pcrexx  then  blinkSecs=0         /*if ¬PC/REXX, turn off BLINKSECS*/
tops=space(tops)                       /*elide extraneous  TOPS  blanks.*/
parse value  scrsize()  with  sd sw .  /*get the term screens dimensions*/
oldTime=
           do until queued()\==0
           ct=time();  mn=substr(ct,4,2);  ss=right(ct,2);   i_=0;   p_=0
           call blinksec
           if ct==oldTime  then if !cms then 'CP SLEEP'; else call delay 1

           if creep  then do;            p_ =  3 + right(mn,1)
                          if sd>26  then p_ = p_ + left(mn,1)
                          if sd>33  then p_ = p_ + left(mn,1)
                          if sd>44  then p_ = p_ + left(mn,1) +right(mn,1)
                          end
           _p=-p_
           i_=2+left(ct,1);     ctt=left(ct,5);    if sw>108  then ctt=ct
           r=$t('.P='_p ".I="i_ tops ctt);         if r\==0   then leave
           oldTime=time()
           end   /*forever*/
exit                                   /*stick a fork in it, we're done.*/
/*═════════════════════════════general 1-line subs════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
!all:!!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal: if symbol('!CALL')\=="VAR" then !call=;return !call
!env: !env='ENVIRONMENT';if !sys=='MSDOS'|!brexx|!r4|!roo then !env='SYSTEM';if !os2 then !env='OS2'!env;!ebcdic=1=='f0'x;return
!fid: parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;call !sys;if !dos then do;_=lastpos('\',!fn);!fm=left(!fn,_);!fn=substr(!fn,_+1);parse var !fn !fn '.' !ft;end;return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex: parse upper version !ver !vernum !verdate .;!brexx='BY'==!vernum;!kexx='KEXX'==!ver;!pcrexx='REXX/PERSONAL'==!ver|'REXX/PC'==!ver;!r4='REXX-R4'==!ver;!regina='REXX-REGINA'==left(!ver,11);!roo='REXX-ROO'==!ver;call !env;return
!sys: !cms=!sys=='CMS';!os2=!sys=='OS2';!tso=!sys=='TSO'|!sys=='MVS';!vse=!sys=='VSE';!dos=pos('DOS',!sys)\==0|pos('WIN',!sys)\==0|!sys=='CMD';call !rex;return
!var: call !fid;if !kexx then return space(dosenv(arg(1)));return space(value(arg(1),,!env))
$t:   !call=']$T';call "$T" arg(1);!call=;return result
abb:  arg abbu;parse arg abb;return abbrev(abbu,_,abbl(abb))
abbl: return verify(arg(1)'a',@abc,'M')-1
abbn: parse arg abbn;return abb(abbn)|abb('NO'abbn)
blinksec: if \blinksecs then return;bsec=' ';ss2=right(ss,2);if sw<=80 then bsec=copies(' ',2+ss2) ss2;call scrwrite 1+right(mn,1),1,bsec,,,1;call cursor sd-right(mn,1),sw-length(bsec);return
er:   parse arg _1,_2;call '$ERR' "14"p(_1) p(word(_1,2) !fid(1)) _2;if _1<0 then return _1;exit result
err:  call er '-'arg(1),arg(2);return ''
erx:  call er '-'arg(1),arg(2);exit ''
halt: call er .1
no:   if arg(1)\=='' then call er 01,arg(2);return left(_,2)\=='NO'
novalue:!sigl=sigl;call er 17,!fid(2) !fid(3) !sigl condition('D') sourceline(!sigl)
p:    return word(arg(1),1)
syntax:!sigl=sigl;call er 13,!fid(2) !fid(3) !sigl !cal() condition('D') sourceline(!sigl)
```

'''output'''

```txt

     ░░░░░░░░     ░░░░░░░░                     ░░       ░░░░░░░░░░                     ░░░     ░░░░░░░░░░
    ░░░░░░░░░░   ░░░░░░░░░░                   ░░░       ░░░░░░░░░░                    ░░░░     ░░░░░░░░░░
    ░░      ░░   ░░      ░░                  ░░░░       ░░                           ░░ ░░     ░░
    ░░      ░░           ░░       ░░           ░░       ░░               ░░         ░░  ░░     ░░
          ░░            ░░        ░░           ░░       ░░               ░░        ░░   ░░     ░░
         ░░           ░░░                      ░░       ░░░░░░░                   ░░░░░░░░░░   ░░░░░░░
       ░░             ░░░                      ░░       ░░░░░░░░                  ░░░░░░░░░░   ░░░░░░░░
     ░░                 ░░                     ░░              ░░                       ░░            ░░
    ░░                   ░░                    ░░               ░░                      ░░             ░░
    ░░      ░░   ░░      ░░       ░░           ░░       ░░      ░░       ░░             ░░     ░░      ░░
    ░░░░░░░░░░   ░░░░░░░░░░       ░░         ░░░░░░     ░░░░░░░░░░       ░░            ░░░░    ░░░░░░░░░░
    ░░░░░░░░░░    ░░░░░░░░                   ░░░░░░      ░░░░░░░░                      ░░░░     ░░░░░░░░

```

'''output'''   (when the terminal screen is less then 109 bytes)

```txt

      ░░░░░░░░     ░░░░░░░░                  ░░░░░░░░    ░░░░░░░░░░
     ░░░░░░░░░░   ░░░░░░░░░░                ░░░░░░░░░░   ░░░░░░░░░░
     ░░      ░░   ░░      ░░                ░░      ░░   ░░
     ░░      ░░           ░░       ░░       ░░      ░░   ░░
           ░░            ░░        ░░             ░░     ░░
          ░░           ░░░                       ░░      ░░░░░░░
        ░░             ░░░                     ░░        ░░░░░░░░
      ░░                 ░░                  ░░                 ░░
     ░░                   ░░                ░░                   ░░
     ░░      ░░   ░░      ░░       ░░       ░░      ░░   ░░      ░░
     ░░░░░░░░░░   ░░░░░░░░░░       ░░       ░░░░░░░░░░   ░░░░░░░░░░
     ░░░░░░░░░░    ░░░░░░░░                 ░░░░░░░░░░    ░░░░░░░░

```



## Ruby

{{libheader|Shoes}}
[[File:shoes_clock.png|thumb|Sample display of Ruby solution]]

```ruby
Shoes.app(:width=>205, :height => 228, :title => "A Clock") do
  def draw_ray(width, start, stop, ratio)
    angle = Math::PI * 2 * ratio - Math::PI/2
    strokewidth width
    cos = Math::cos(angle)
    sin = Math::sin(angle)
    line 101+cos*start, 101+sin*start, 101+cos*stop, 101+sin*stop
  end

  def update
    t = Time.now
    @time.text = t.strftime("%H:%M:%S")
    h, m, s = (t.hour % 12).to_f, t.min.to_f, t.sec.to_f
    s += t.to_f - t.to_i  # add the fractional seconds

    @hands.clear do
      draw_ray(3, 0, 70, (h + m/60)/12)
      draw_ray(2, 0, 90, (m + s/60)/60)
      draw_ray(1, 0, 95, s/60)
    end
  end

  # a place for the text display
  @time = para(:align=>"center", :family => "monospace")

  # draw the clock face
  stack(:width=>203, :height=>203) do
    strokewidth 1
    fill gradient(deepskyblue, aqua)
    oval 1, 1, 200
    fill black
    oval 98, 98, 6
    # draw the minute indicators
    0.upto(59) {|m| draw_ray(1, (m % 5 == 0 ? 96 : 98), 100, m.to_f/60)}
  end.move(0,23)

  # the drawing area for the hands
  @hands = stack(:width=>203, :height=>203) {}.move(0,23)

  animate(5) {update}
end
```


Inspired by the PicoLisp solution, here's an implementation of the Berlin-Uhr clock.
[[File:berlin_uhr.rb.png|thumb|Berlin-Uhr clock]]

```ruby
Shoes.app(:title => "Berlin-Uhr Clock", :width => 209, :height => 300) do
  background lightgrey

  Red = rgb(255, 20, 20)
  Yellow = rgb(173, 255, 47)
  Green = rgb(154, 205, 50)
  Gray = rgb(128, 128, 128)

  @time = para(:align => "center")
  stack do
    fill Gray
    stroke black
    strokewidth 2
    @seconds = oval 75, 3, 50
    @hrs_a  =  4.times.collect {|i| rect   51*i,  56, 48, 30, 4}
    @hrs_b  =  4.times.collect {|i| rect   51*i,  89, 48, 30, 4}
    @mins_a = 11.times.collect {|i| rect 2+18*i, 122, 15, 30, 4}
    @mins_b =  4.times.collect {|i| rect   51*i, 155, 48, 30, 4}
    # some decoration
    fill white
    stroke darkslategray
    rect -10, -30, 75, 70, 10
    rect 140, -30, 75, 70, 10
    rect -13, 192, 105, 100, 10
    rect 110, 192, 105, 100, 10
  end.move(3,20)

  animate(1) do
    now = Time.now
    @time.text = now.strftime("%H:%M:%S")
    @seconds.style(:fill => now.sec.even? ? Green : Gray)
    a, b = now.hour.divmod(5)
    4.times {|i| @hrs_a[i].style(:fill => i < a ? Red : Gray)}
    4.times {|i| @hrs_b[i].style(:fill => i < b ? Red : Gray)}
    a, b = now.min.divmod(5)
    11.times {|i| @mins_a[i].style(:fill => i < a ? (i%3==2 ? Red : Yellow) : Gray)}
    4.times  {|i| @mins_b[i].style(:fill => i < b ? Yellow : Gray)}
  end

  keypress do |key|
    case key
    when :control_q, "\x11" then exit
    end
  end
end
```



## Run BASIC

[[File:Rb_clock.png|thumb|Sample display of RB solution]]

```runbasic
' --------------------------------------------
' clock. I got nothing but time
' ---------------------------------------------
n	= 12 ' num of points
r	= 95 ' radius
pi	= 22/7
alpha	= pi * 2 / n
dim points(n)
graphic #g2, 200, 200
' --------------------------------------
' Draw the clock
' --------------------------------------
#g2 size(2) 'pen size
#g2 down()
#g2 font("arial", 20, "bold")
#g2 place(85,30)
#g2 "\12"
#g2 place(170,105)
#g2 "\3"
#g2 place(10,105)
#g2 "\9"
#g2 place(90,185)
#g2 "\6"
for i = 0 to n - 1
	theta = alpha * i
	px = cos( theta ) * r
	py = sin( theta ) * r
	px = px + 100
	py = py + 100
	#g2 place(px,py)
	#g2 circle(2)
next i

[shoTime]
' -------------------------
' clear previous sec,min,hr
' -------------------------
r	= 63
p	= se
#g2 color("white")
gosub [h2Dot]
r	= 50
p	= mi
#g2 color("white")
gosub [h2Dot]
r	= 30 ' radius
p	= hr * 5
#g2 color("white")
gosub [h2Dot]

' -------------------------
' Show new time
' -------------------------
a$	= time$()
hr	= val(word$(a$,1,":"))
mi	= val(word$(a$,2,":"))
se	= val(word$(a$,3,":"))

' put time on the clock - gimme a hand
#g2 size(4)
' second hand
n	= 60
r	= 63
p	= se
#g2 color("blue")
gosub [h2Dot]

' minute hand
r	= 50
p	= mi
#g2 color("green")
gosub [h2Dot]

' hour hand
r	= 30 ' radius
p	= hr * 5
#g2 color("red")
gosub [h2Dot]

render #g2
end

' a one liner
[h2Dot]
alpha	= pi * 2 / n
i 	= p - 15
theta 	= alpha * i
px 	= cos( theta ) * r
py 	= sin( theta ) * r
px 	= px + 100
py 	= py + 100
#g2 place(px,py)
#g2 circle(2)
#g2 line(100,100,px,py)
RETURN
```



## Rust


```rust
// cargo-deps: time="0.1"
extern crate time;

use std::thread;
use std::time::Duration;

const TOP: &str = " ⡎⢉⢵ ⠀⢺⠀ ⠊⠉⡱ ⠊⣉⡱ ⢀⠔⡇ ⣏⣉⡉ ⣎⣉⡁ ⠊⢉⠝ ⢎⣉⡱ ⡎⠉⢱ ⠀⠶⠀";
const BOT: &str = " ⢗⣁⡸ ⢀⣸⣀ ⣔⣉⣀ ⢄⣀⡸ ⠉⠉⡏ ⢄⣀⡸ ⢇⣀⡸ ⢰⠁⠀ ⢇⣀⡸ ⢈⣉⡹ ⠀⠶⠀";

fn main() {
    let top: Vec<&str> = TOP.split_whitespace().collect();
    let bot: Vec<&str> = BOT.split_whitespace().collect();

    loop {
        let tm = &time::now().rfc822().to_string()[17..25];
        let top_str: String = tm.chars().map(|x| top[x as usize - '0' as usize]).collect();
        let bot_str: String = tm.chars().map(|x| bot[x as usize - '0' as usize]).collect();

        clear_screen();
        println!("{}", top_str);
        println!("{}", bot_str);

        thread::sleep(Duration::from_secs(1));
    }
}

fn clear_screen() {
    println!("{}[H{}[J", 27 as char, 27 as char);
}
```



## Scala


### Circular ASCII clock

Generates and prints a simple ASCII clock every second

```scala
import java.util.{ Timer, TimerTask }
import java.time.LocalTime
import scala.math._

object Clock extends App {
  private val (width, height) = (80, 35)

  def getGrid(localTime: LocalTime): Array[Array[Char]] = {
    val (minute, second) = (localTime.getMinute, localTime.getSecond())
    val grid = Array.fill[Char](height, width)(' ')

    def toGridCoord(x: Double, y: Double): (Int, Int) =
      (floor((y + 1.0) / 2.0 * height).toInt, floor((x + 1.0) / 2.0 * width).toInt)

    def makeText(grid: Array[Array[Char]], r: Double, theta: Double, str: String) {
      val (row, col) = toGridCoord(r * cos(theta), r * sin(theta))
      (0 until str.length).foreach(i =>
        if (row >= 0 && row < height && col + i >= 0 && col + i < width) grid(row)(col + i) = str(i))
    }

    def makeCircle(grid: Array[Array[Char]], r: Double, c: Char) {
      var theta = 0.0
      while (theta < 2 * Pi) {
        val (row, col) = toGridCoord(r * cos(theta), r * sin(theta))
        if (row >= 0 && row < height && col >= 0 && col < width) grid(row)(col) = c
        theta = theta + 0.01
      }
    }

    def makeHand(grid: Array[Array[Char]], maxR: Double, theta: Double, c: Char) {
      var r = 0.0
      while (r < maxR) {
        val (row, col) = toGridCoord(r * cos(theta), r * sin(theta))
        if (row >= 0 && row < height && col >= 0 && col < width) grid(row)(col) = c
        r = r + 0.01
      }
    }

    makeCircle(grid, 0.98, '@')
    makeHand(grid, 0.6, (localTime.getHour() + minute / 60.0 + second / 3600.0) * Pi / 6 - Pi / 2, 'O')
    makeHand(grid, 0.85, (minute + second / 60.0) * Pi / 30 - Pi / 2, '*')
    makeHand(grid, 0.90, second * Pi / 30 - Pi / 2, '.')

    (1 to 12).foreach(n => makeText(grid, 0.87, n * Pi / 6 - Pi / 2, n.toString))
    grid
  } // def getGrid(

  private val timerTask = new TimerTask {
    private def printGrid(grid: Array[Array[Char]]) = grid.foreach(row => println(row.mkString))
    def run() = printGrid(getGrid(LocalTime.now()))
  }
  (new Timer).schedule(timerTask, 0, 1000)
}
```


### Berliner Uhr

See [[http://en.wikipedia.org/wiki/Mengenlehreuhr The Berlin set theory clock]]

```scala
import java.time.LocalTime
import java.awt.{ Color, Graphics }

/** The Berlin clock as a Java (8.0) applet
 */
class QDclock extends java.applet.Applet with Runnable {
  val bclockThread: Thread = new Thread(this, "QDclock")

  override def init() = resize(242, 180) // fixed size, at first... doesn't work...

  override def start() = if (!bclockThread.isAlive()) bclockThread.start()

  def run() {
    while (true) {
      repaint()
      try Thread.sleep(1000) catch { case _: Throwable => sys.exit(-1) }
    }
  }

  override def update(g: Graphics) {
    val now = LocalTime.now

    def booleanToColor(cond: Boolean, colorOn: Color = Color.red): Color =
      if (cond) colorOn else Color.black

    g.setColor(booleanToColor(now.getSecond() % 2 == 0, Color.yellow))
    g.fillOval(100, 4, 40, 40)

    val (stu, min) = (now.getHour(), now.getMinute()) match {
      case (0, 0)     => (24, 0)
      case (hrs, min) => (hrs, min)
    }

    def drawRectangle(color: Color, rect: (Int, Int, Int, Int)) {
      g.setColor(color)
      g.fillRoundRect(rect._1, rect._2, rect._3, rect._4, 4, 4)
    }

    for (i <- 0 until 4) {
      drawRectangle(booleanToColor(stu / ((i + 1) * 5) > 0), (i * 60 + 2, 46, 58, 30))
      drawRectangle(booleanToColor(stu % 5 > i), (i * 60 + 2, 78, 58, 30))
      drawRectangle(booleanToColor(min % 5 > i, Color.yellow), (i * 60 + 2, 142, 58, 30))
    }

    for (i <- 0 until 11) {
      drawRectangle(booleanToColor(min / ((i + 1) * 5) > 0,
        if (2 to 8 by 3 contains i) Color.red else Color.yellow), (i * 20 + 10, 110, 18, 30))
    }
  }
}
```



## Scheme

{{libheader|Scheme/PsTk}}

Translation of a Tcl example at http://wiki.tcl.tk/1011
The program displays an analogue clock with three hands, updating once a second.


```Scheme

(import (scheme base)
        (scheme inexact)
        (scheme time)
        (pstk))

(define PI 3.1415927)

;; Draws the hands on the canvas using the current time, and repeats each second
(define (hands canvas)
  (canvas 'delete 'withtag "hands")

  (let* ((time (current-second)) ; no time locality used, so displays time in GMT
         (hours (floor (/ time 3600)))
         (rem (- time (* hours 3600)))
         (mins (floor (/ rem 60)))
         (secs (- rem (* mins 60)))
         (second-angle (* secs (* 2 PI 1/60)))
         (minute-angle (* mins (* 2 PI 1/60)))
         (hour-angle (* hours (* 2 PI 1/12))))
    (canvas 'create 'line ; second hand
            100 100
            (+ 100 (* 90 (sin second-angle)))
            (- 100 (* 90 (cos second-angle)))
            'width: 1 'tags: "hands")
    (canvas 'create 'line ; minute hand
            100 100
            (+ 100 (* 85 (sin minute-angle)))
            (- 100 (* 85 (cos minute-angle)))
            'width: 3
            'capstyle: "projecting"
            'tags: "hands")
    (canvas 'create 'line ; hour hand
            100 100
            (+ 100 (* 60 (sin hour-angle)))
            (- 100 (* 60 (cos hour-angle)))
            'width: 7
            'capstyle: "projecting"
            'tags: "hands"))
  (tk/after 1000 (lambda () (hands canvas))))

;; Create the initial frame, clock frame and hours
(let ((tk (tk-start)))
  (tk/wm 'title tk "GMT Clock")

  (let ((canvas (tk 'create-widget 'canvas)))
    (tk/pack canvas)
    (canvas 'configure 'height: 200 'width: 200)
    (canvas 'create 'oval 2 2 198 198 'fill: "white" 'outline: "black")
    (do ((h 1 (+ 1 h)))
      ((> h 12) )
      (let ((angle (- (/ PI 2) (* h PI 1/6))))
        (canvas 'create 'text
                (+ 100 (* 90 (cos angle)))
                (- 100 (* 90 (sin angle)))
                'text: (number->string h)
                'font: "{Helvetica -12}")))

    (hands canvas))
  (tk-event-loop tk))

```



## Scratch

One can view the Scratch solution to this task and inspect its code at the [https://scratch.mit.edu/projects/64809384/ Scratch Website].  (The code for visual programming languages is difficult to post directly here at Rosetta Code.)

True to the spirit of the task description, this is a very bare-bones clock.  It's a blank-faced analog clock having second (red), minute (green) and hour (blue) hands.  Each hand is a sprite, which has its own script to provide a movement.  When the program is started, all hands are positioned at the pivot and rotated to indicate the current (apparently local) time.  The second hand is then put into an infinite loop that provides the tick, moving it every second to the current second.  I found that a loop delay of 0.1 seconds resulted is smooth operation of the second hand.  When the second hand reaches 0, it broadcasts a <tt>set_minute</tt> signal.  Acting upon this signal, the minute hand advances to the current minute.  When the minute is 0 modulo 12, is broadcasts a <tt>set_hour</tt> signal.  The hour hand responds to this signal by advancing by a fifth of an hour (so that like the minute and second hands, it advances 60 times as it makes a complete circuit of the clock face).


## Seed7

The example program clock3.sd7 from the Seed7 package can be used for this task.

```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";
  include "draw.s7i";
  include "keybd.s7i";
  include "time.s7i";
  include "duration.s7i";

const integer: WINDOW_WIDTH is 200;
const integer: WINDOW_HEIGHT is 200;
const color: BACKGROUND is White;
const color: FOREGROUND is Black;
const color: CLOCKCOLOR is Aqua;

const proc: main is func
  local
    var char: command is ' ';
    var time: start_time is time.value;
    var float: alpha is 0.0;
    var integer: x is 0;
  begin
    screen(WINDOW_WIDTH, WINDOW_HEIGHT);
    clear(curr_win, BACKGROUND);
    KEYBOARD := GRAPH_KEYBOARD;
    command := busy_getc(KEYBOARD);
    while command <> 'q' do
      start_time := truncToSecond(time(NOW));
      clear(curr_win, BACKGROUND);
      fcircle(100, 100, 95, CLOCKCOLOR);
      circle(100, 100, 95, FOREGROUND);
      for x range 0 to 60 do
        alpha := flt(x-15) * PI / 30.0;
        if x mod 5 = 0 then
          lineTo(100 + round(cos(alpha)*95.0),
                 100 + round(sin(alpha)*95.0),
                 100 + round(cos(alpha)*85.0),
                 100 + round(sin(alpha)*85.0), FOREGROUND);
        else
          lineTo(100 + round(cos(alpha)*95.0),
                 100 + round(sin(alpha)*95.0),
                 100 + round(cos(alpha)*92.0),
                 100 + round(sin(alpha)*92.0), FOREGROUND);
        end if;
      end for;
      alpha := flt(start_time.second-15) * PI / 30.0;
      lineTo(100, 100, 100 + round(cos(alpha)*85.0), 100 + round(sin(alpha)*85.0), FOREGROUND);
      alpha := flt(start_time.minute-15) * PI / 30.0;
      lineTo(100 + round(cos(alpha-PI/2.0)*5.0),
             100 + round(sin(alpha-PI/2.0)*5.0),
             100 + round(cos(alpha)*75.0),
             100 + round(sin(alpha)*75.0), FOREGROUND);
      lineTo(100 + round(cos(alpha+PI/2.0)*5.0),
             100 + round(sin(alpha+PI/2.0)*5.0),
             100 + round(cos(alpha)*75.0),
             100 + round(sin(alpha)*75.0), FOREGROUND);
      alpha := (flt(start_time.hour)+flt(start_time.minute)/60.0-3.0) * PI / 6.0;
      lineTo(100 + round(cos(alpha-PI/2.0)*7.0),
             100 + round(sin(alpha-PI/2.0)*7.0),
             100 + round(cos(alpha)*50.0),
             100 + round(sin(alpha)*50.0), FOREGROUND);
      lineTo(100 + round(cos(alpha+PI/2.0)*7.0),
             100 + round(sin(alpha+PI/2.0)*7.0),
             100 + round(cos(alpha)*50.0),
             100 + round(sin(alpha)*50.0), FOREGROUND);
      fcircle(100, 100, 7, CLOCKCOLOR);
      circle(100, 100, 7, FOREGROUND);
      DRAW_FLUSH;
      await(start_time + 1 . SECONDS);
      command := busy_getc(KEYBOARD);
    end while;
  end func;
```



## Sidef

{{trans|Perl}}

```ruby
STDOUT.autoflush(true)

var (rows, cols) = `stty size`.nums...

var x = (rows/2 - 1  -> int)
var y = (cols/2 - 16 -> int)

var chars = [
                 "┌─┐  ╷╶─┐╶─┐╷ ╷┌─╴┌─╴╶─┐┌─┐┌─┐   ",
                 "│ │  │┌─┘╶─┤└─┤└─┐├─┐  │├─┤└─┤ : ",
                 "└─┘  ╵└─╴╶─┘  ╵╶─┘└─┘  ╵└─┘╶─┘   "
             ].map {|s| s.split(3) }

func position(i,j) {
    "\e[%d;%dH" % (i, j)
}

func indices {
    var t = Time.local
    "%02d:%02d:%02d" % (t.hour, t.min, t.sec) -> split(1).map{|c| c.ord - '0'.ord }
}

loop {
    print "\e[H\e[J"
    for i in ^chars {
      print position(x + i, y)
      print [chars[i][indices()]].join(' ')
    }
    print position(1, 1)
    Sys.sleep(0.1)
}
```

{{out}}

```txt

   ╷ ┌─╴     ╷ ╷ ┌─┐     ╶─┐ ┌─╴
   │ └─┐  :  └─┤ └─┤  :  ╶─┤ └─┐
   ╵ ╶─┘       ╵ ╶─┘     ╶─┘ ╶─┘

```



## SVG


```svg
<svg viewBox="0 0 100 100" width="480px" height="480px" xmlns="http://www.w3.org/2000/svg">
<circle cx="50" cy="50" r="48" style="fill:peru; stroke:black; stroke-width:2" />
<g transform="translate(50,50) rotate(0)" style="fill:none; stroke-linecap:round">
  <line y2="-36" style="stroke:black; stroke-width:5">
    <animateTransform
      attributeName="transform"
      type="rotate"
      by="30"
      dur="3600s"
      accumulate="sum"
      repeatCount="indefinite"/>
  </line>
  <line y2="-42" style="stroke:white; stroke-width:2">
    <animateTransform
      attributeName="transform"
      type="rotate"
      by="6"
      dur="60s"
      accumulate="sum"
      repeatCount="indefinite"/>
  </line>
  <line y2="-46" style="stroke:red; stroke-width:1">
    <animateTransform
      attributeName="transform"
      type="rotate"
      calcMode="discrete"
      by="6"
      dur="1s"
      accumulate="sum"
      repeatCount="indefinite"/>
  </line>
</g>
<circle cx="50" cy="50" r="4" style="fill:gold; stroke:black; stroke-width:1" />
</svg>

```


## Tcl

[[File:Clock tcltk.png|thumb|Sample display of Tcl solution]]
{{libheader|Tk}}

```tcl
package require Tcl 8.5
package require Tk

# GUI code
pack [canvas .c -width 200 -height 200]
.c create oval 20 20 180 180 -width 10 -fill {} -outline grey70
.c create line 0 0 1 1 -tags hour   -width 6 -cap round -fill black
.c create line 0 0 1 1 -tags minute -width 4 -cap round -fill black
.c create line 0 0 1 1 -tags second -width 2 -cap round -fill grey30
proc updateClock t {
    scan [clock format $t -format "%H %M %S"] "%d%d%d" h m s
    # On an analog clock, the hour and minute hands move gradually
    set m [expr {$m + $s/60.0}]
    set h [expr {($h % 12 + $m/60.0) * 5}]
    foreach tag {hour minute second} value [list $h $m $s] len {50 80 80} {
	.c coords $tag 100 100 \
	    [expr {100 + $len*sin($value/30.0*3.14159)}] \
	    [expr {100 - $len*cos($value/30.0*3.14159)}]
    }
}

# Timer code, accurate to within a quarter second
set time 0
proc ticker {} {
    global time
    set t [clock seconds]
    after 250 ticker
    if {$t != $time} {
	set time $t
	updateClock $t
    }
}
ticker
```

Note that though this code does poll the system timer approximately four times a second, this is a cheap operation; the GUI update (the relatively expensive part) only happens once a second. The amount of system processing power consumed by this code isn't noticeable on my system; it vanishes with respect to the other processing normally happening.


## Yabasic


```Yabasic
clear screen
open window 300,100
backcolor 0, 0, 0
window origin "cc"

// Display digital clock
sub digital_clock()
    local t$(1), void
    static as$

    void = token(time$, t$(), "-")

    if t$(3) <> as$ then
        draw_clock(t$(1), t$(2), t$(3))
        as$ = t$(3)
    end if
end sub

sub draw_clock(hour$, mint$, ssec$)
    local d$(1), void

    void = token(date$, d$(), "-")
    clear window
    color 200, 255, 0
    text -140, -30, d$(3) + "/" + d$(2) + "/" + d$(4), "modern12"
    text 0, 0, hour$ + ":" + mint$ + ":" + ssec$, "cc", "swiss50"
end sub

if peek$("library") = "main" then
    repeat
        digital_clock()
    until(upper$(inkey$(.01))="ESC")
    exit
end if
```



## zkl

{{trans|Nim}}

```zkl
var
  t=T("⡎⢉⢵","⠀⢺⠀","⠊⠉⡱","⠊⣉⡱","⢀⠔⡇","⣏⣉⡉","⣎⣉⡁","⠊⢉⠝","⢎⣉⡱","⡎⠉⢱","⠀⠶⠀"),
  b=T("⢗⣁⡸","⢀⣸⣀","⣔⣉⣀","⢄⣀⡸","⠉⠉⡏","⢄⣀⡸","⢇⣀⡸","⢰⠁⠀","⢇⣀⡸","⢈⣉⡹","⠀⠶ ");
while(True){
   x:=Time.Date.ctime()[11,8]  // or Time.Date.to24HString() (no seconds)
      .pump(List,fcn(n){ n.toAsc() - 0x30 }); //-->L(2,3,10,4,3,10,5,2)
   print("\e[H\e[J");	// home and clear screen on ANSI terminals
   println(x.pump(String,t.get),"\n",x.pump(String,b.get));
   Atomic.sleep(1);
}
```

{{out}}

```txt

⠊⠉⡱⠊⣉⡱⠀⠶⠀⢀⠔⡇⠊⣉⡱⠀⠶⠀⣏⣉⡉⠊⠉⡱
⣔⣉⣀⢄⣀⡸⠀⠶ ⠉⠉⡏⢄⣀⡸⠀⠶ ⢄⣀⡸⣔⣉⣀

```


## ZX Spectrum Basic

Chapter 18 of the BASIC manual supplied with the ZX Spectrum includes two programs to implement a clock - each uses different timing methods. The first - using a PAUSE command to hold for a second - is far less accurate, while the second - reading the three-byte system frames counter - is more CPU hungry (since ZX Spectrum Basic can't multitask, this doesn't really matter). With a tweak, the second is shown below.

```zxbasic
10 REM First we draw the clock face
20 FOR n=1 TO 12
30 PRINT AT 10-10*COS (n/6*PI),16+10*SIN (n/6*PI);n
40 NEXT n
50 DEF FN t()=INT (65536*PEEK 23674+256*PEEK 23673+PEEK 23672)/50: REM number of seconds since start
100 REM Now we start the clock
110 LET t1=FN t()
120 LET a=t1/30*PI: REM a is the angle of the second hand in radians
130 LET sx=72*SIN a: LET sy=72*COS a
140 PLOT 131,91: DRAW OVER 1;sx,sy: REM draw hand
200 LET t=FN t()
210 IF INT t<=INT t1 THEN GO TO 200: REM wait for time for next hand; the INTs were not in the original but force it to wait for the next second
220 PLOT 131,91: DRAW OVER 1;sx,sy: REM rub out old hand
230 LET t1=t: GO TO 120
```


{{omit from|ACL2|No access to system time}}
{{omit from|Lilypond}}
{{omit from|TUSCRIPT}}
