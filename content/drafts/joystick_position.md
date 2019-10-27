+++
title = "Joystick position"
description = ""
date = 2019-08-12T22:00:53Z
aliases = []
[extra]
id = 9763
[taxonomies]
categories = []
tags = []
+++

{{task|Joystick}} [[Category:Hardware]]
The task is to determine the joystick position and represent this on the display via a crosshair.
For a centred joystick, the crosshair should appear in the centre of the screen. 
If the joystick is pushed left or right, then the cross hair should move left or right according to the extent that the joystick is pushed. 

If the joystick is pushed forward or pulled back, then the crosshair should move up or down according to the extent that that joystick is pushed or pulled. 
The edges of the display represent maximum extents for joystick movement. 

For example, a joystick pushed fully forward would raise the crosshair to the top centre of the screen. 

A joystick pulled backwards and to the right would move the crosshair to the bottom right of the screen (except for a small area reserved to show joystick status). Implementations can use a graphical display method to produce the crosshair, or alternatively represent the crosshair using a plus symbol on
a terminal, and move the plus symbol position according to the joystick. 
The bottom part of the display can hide or show an alphanumeric sequence to represent the buttons pressed. 

For example, if pushbuttons 1,4 and 10 are depressed, we could display "1 4 A". 

The implemented code should continue to redraw the crosshair according to the joystick position and show the current pushbutton statuses until the task is terminated. 
Digital joysticks that produce no extent data, should have their position indicated as full extent movement of the crosshair.

For the purpose of this task, we assume that the joystick is calibrated and that the first joystick is being used. 

The task implementer could at their option provide a solution that includes a joystick selection facility, enabling the user to choose which joystick is to be used for this task.


## Applesoft BASIC


```ApplesoftBasic
100 GOSUB 400
110 P2 = PDL(2) : IF P2 <> O2 THEN O2 = P2 : VTAB 23 : HTAB 33 : PRINT P2; TAB(37);
120 B2 = FNB(2) : IF B2 <> N2 THEN N2 = B2 : VTAB 24 : HTAB 15 : PRINT P$(B2);
130 P3 = PDL(3) : IF P3 <> O3 THEN O3 = P3 : VTAB 23 : HTAB 37 : PRINT P3; : CALL -868
140 X = INT(P3 * RX) : Y = INT(P2 * RY(F))
150 O = (X1 = X2 AND Y1 = Y2) + 1
160 N = (X = X1 AND Y = Y1) + 1
170 IF X <> X2 OR Y <> Y2 THEN XDRAW N AT X, Y : XDRAW O AT X2, Y2 : X2 = X : Y2 = Y : O = N
200 P0 = PDL(0) : IF P0 <> O0 THEN O0 = P0 : VTAB 22 : HTAB 33 : PRINT P0; TAB(37);
210 B0 = FNB(0) : IF B0 <> N0 THEN N0 = B0 : VTAB 22 : HTAB 15 : PRINT P$(B0);
220 P1 = PDL(1) : IF P1 <> O1 THEN O1 = P1 : VTAB 22 : HTAB 37 : PRINT P1; : CALL -868
230 B1 = FNB(1) : IF B1 <> N1 THEN N1 = B1 : VTAB 23 : HTAB 15 : PRINT P$(B1);
240 X = INT(P0 * RX) : Y = INT(P1 * RY(F))
250 O = (X1 = X2 AND Y1 = Y2) + 1
260 N = (X = X2 AND Y = Y2) + 1
270 IF X <> X1 OR Y <> Y1 THEN XDRAW N AT X, Y : XDRAW O AT X1, Y1 : X1 = X : Y1 = Y
300 K = PEEK(-16384) : IF K < 128 THEN 110
310 POKE-16368,0
320 IF K = 198 OR K = 200 THEN F = PEEK(-16302) ^ 0 : GOTO 110HIDE
330 IF K = 155 OR K = 211 THEN F = PEEK(-16301) * 0 : GOTO 110SHOW
340 TEXT : END
400 HOME : HGR
410 DEF FN B(B) = PEEK(49249 + B) > 127
420 P$(0) = "NOT PRESSED" : P$(1) = "PRESSED    "
430 VTAB 21 : PRINT "BUTTON:";
440 PRINT TAB(28); "JOYSTICK:"
450 PRINT "   OPEN APPLE "P$(0);
460 PRINT TAB(29); "ONE"
470 PRINT " CLOSED APPLE "P$(0);
480 PRINT TAB(29); "TWO" 
490 PRINT TAB(9); "SHIFT "P$(0);
500 RX = 35 / 32 : RY(0) = 5 / 8 : RY(1) = 3 / 4
510 DATA2,0,6,0,3,0,29,15,20,6,0
520 FOR I = 768 TO 778 : READ B
530 POKE I, B : NEXT : POKE 232, 0 : POKE 233, 3
550 ROT = 0 : SCALE = 7 : XDRAW 1 AT X1, Y1
560 O0 = -1 : O1 = O0 : O2 = O0 : O3 = O0
570 RETURN
```



## AutoHotkey

{{works with|AutoHotkey_L}}
{{libheader|GDIP}}
[http://ahkscript.org/boards/viewtopic.php?f=6&t=2347&p=12973#p12973 Forum thread]

[http://i.imgur.com/KsbrpK4.png Image link]

```AutoHotkey
; Uncomment if Gdip.ahk is not in your standard library
; #Include, Gdip.ahk

; Comment for lower CPU usage
SetBatchLines, -1

JoystickNumber := 0     ; (1-16) or (0 = Auto-detect)
CrosshairSize := 100
BarWidth := 50
BarSpacing := BarWidth + 8
Color1 := 0x99000000
Color2 := 0x99ffffff
Color3 := 0x99ff6600
Color4 := 0xff0066ff
Color5 := 0xffff6600
Font := "Arial"
FontSize1 := 20
FontSize2 := 30
Lineweight1 := 8
Lineweight2 := 3
Lineweight3 := 2
Lineweight4 := 4
Show2ndCrosshair := true
AxisLabelHeight := 47

#SingleInstance, Force
#NoEnv
OnExit, Exit
SysGet, MWA, MonitorWorkArea
CrosshairOffset := CrosshairSize // 2
, CircleOffset := CrosshairOffset - 5
, CircleSize := CrosshairSize - 10
, TaskBarHeight := A_ScreenHeight - MWABottom + Lineweight1 // 2
, ScaleX := A_ScreenWidth / 100
, ScaleY1 := (A_ScreenHeight - TaskBarHeight - AxisLabelHeight) / 100
, ScaleY2 := A_ScreenHeight / 100
, BarCenter := (MWABottom - AxisLabelHeight) // 2 + AxisLabelHeight
, BorderBot := MWABottom - Lineweight1 // 2 + 2
, PieSize := 400
, PieX := (A_ScreenWidth - PieSize) // 2
, PieY := (A_ScreenHeight - PieSize) // 2
, BarHeight := A_ScreenHeight - AxisLabelHeight - TaskBarHeight
, AxisTextOffset := BarWidth > 32 ? (BarWidth - 32) // 2 : 0
, Axis_Array := {"X": "X", "Y": "Y"}
, MaxI :=  2

; Auto-detect the joystick number if called for
if (JoystickNumber < 1) {
    Loop, 16 {
        GetKeyState, Joy_Name, %A_Index%JoyName
        if (Joy_Name) {
            JoystickNumber := A_Index
            break
        }
    }
    if (!JoystickNumber) {
        MsgBox The system does not appear to have any joysticks.
        ExitApp
    }
}
else {
    GetKeyState, Joy_Name, %JoystickNumber%JoyName
    if (!Joy_Name) {
        MsgBox The system does not appear to have a joystick number %JoystickNumber%.
        ExitApp
    }
}

if (!pToken := Gdip_Startup()) {
    MsgBox, 48, Gdiplus error!, Gdiplus failed to start. Please ensure you have Gdiplus on your system.
    ExitApp
}

If (!Gdip_FontFamilyCreate(Font)) {
   MsgBox, 48, Font error!, The font you have specified does not exist on your system.
   ExitApp
}

; Get joystick information
SetFormat, FloatFast, 03.2
GetKeyState, Joy_Buttons, % JoystickNumber "JoyButtons"
GetKeyState, Joy_Info, % JoystickNumber "JoyInfo"
Loop, Parse, Joy_Info
    if (A_LoopField != "C" && A_LoopField != "D" && A_LoopField != "P")
        Axis_Array[A_LoopField] := A_LoopField
        , %A_LoopField% := true
        , MaxI++
    else
        %A_LoopField% := true

; Setup Gdip
Gui, 1: -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, 1: Show, NA
hwnd1 := WinExist()
, hbm := CreateDIBSection(A_ScreenWidth, A_ScreenHeight)
, hdc := CreateCompatibleDC()
, obm := SelectObject(hdc, hbm)
, G1 := Gdip_GraphicsFromHDC(hdc)
, Gdip_SetSmoothingMode(G1, 4)
, pPen1 := Gdip_CreatePen(Color1, Lineweight1)
, pPen2 := Gdip_CreatePen(Color2, Lineweight2)
, pPen3 := Gdip_CreatePen(Color4, Lineweight3)
, pPen4 := Gdip_CreatePen(Color5, Lineweight4)
, pBrush1 := Gdip_BrushCreateSolid(Color1)
, pBrush2 := Gdip_BrushCreateSolid(Color3)

; Crosshair 2
if ((R || U) && Show2ndCrosshair) {
    pPen5 := Gdip_CreatePen(Color5, Lineweight3)
    , pPen6 := Gdip_CreatePen(Color4, Lineweight4)
    , joy_r := joy_u := 50
}

; Bar X-offsets
for key, val in Axis_Array
    %val%X := A_ScreenWidth - MaxI * BarSpacing + BarSpacing * (A_Index - 1) + 3

; Info box
IBH1 := 150
, IBW1 := 450
, IBX1 := A_ScreenWidth - MaxI * BarSpacing - IBW1
, IBY1 := A_ScreenHeight - TaskBarHeight - IBH1 + Lineweight1 // 2
, IBH2 := IBH1 - 8
, IBW2 := IBW1 - 8
, IBX2 := IBX1 + 4
, IBY2 := IBY1 + 4
, FontOptions1 := "x" (IBX1 + 8) " y" (IBY1 + 8) " w" IBW1 - 20 " Left c" SubStr(Color2, 3) " r4 s" FontSize1 " Bold"

; Axis box
ABH1 := AxisLabelHeight + 4
, ABW1 := MaxI * BarSpacing
, ABX1 := A_ScreenWidth - MaxI * BarSpacing
, ABY1 := 0
, ABH2 := ABH1 - 16
, ABW2 := ABW1 - 8
, ABX2 := ABX1 + 4
, ABY2 := ABY1 + 4
, FontOptions2 := " y" ABY1 + AxisLabelHeight - 40 " w" ABW1 - 10 " Left c" SubStr(Color2, 3) " r4 s" FontSize2 " Bold"

; Update graphics
Loop, {
    Buttons_Down := ""
    Loop, %Joy_Buttons% {
        GetKeyState, joy%A_Index%, %JoystickNumber%joy%A_Index%
        if (joy%A_Index% = "D")
            Buttons_Down .= " " A_Index
    }

    ; Info & axis boxes
    InfoText := Joy_Name " (#" JoystickNumber "):`n" Axis_Info "`nButtons Down: " Buttons_Down "`n`n(Ctrl+Esc to exit)"
    , Gdip_FillRoundedRectangle(G1, pBrush1, IBX1, IBY1, IBW1, IBH1, 5)
    , Gdip_DrawRoundedRectangle(G1, pPen2, IBX2, IBY2, IBW2, IBH2, 5)
    , Gdip_TextToGraphics(G1, InfoText, FontOptions1, Font, A_ScreenWidth, A_ScreenHeight)
    , Gdip_FillRoundedRectangle(G1, pBrush1, ABX1, ABY1, ABW1, ABH1, 5)
    , Gdip_DrawRoundedRectangle(G1, pPen2, ABX2, ABY2, ABW2, ABH2, 5)

    ; Axis bars
    Axis_Info := ""
    for key, val in Axis_Array {
        GetKeyState, joy_%val%, % JoystickNumber "Joy" val
        Axis_Info .= val joy_%val% "  "
        if (joy_%val% > 50)
            %val%Y := BarCenter
            , %val%h1 := (joy_%val% - 50) * ScaleY1
        else
            %val%Y := AxisLabelHeight + joy_%val% * ScaleY1  ;
            , Sc - (joy_%val% - 50) * ScaleY1
            , %val%h1 := BarCenter - %val%Y
        Gdip_FillRoundedRectangle(G1, pBrush2, %val%X, %val%Y, BarWidth, %val%h1, 2)
        , Gdip_DrawRoundedRectangle(G1, pPen1, %val%X, AxisLabelHeight, BarWidth, BarHeight, 5)
        , Gdip_DrawRoundedRectangle(G1, pPen2, %val%X, AxisLabelHeight, BarWidth, BarHeight, 5)
        , Gdip_TextToGraphics(G1, val, "x" (%val%X + AxisTextOffset) FontOptions2, Font, A_ScreenWidth, A_ScreenHeight)
    }

    ; POV hat
    If (P) {
        GetKeyState, Joy_P, %JoystickNumber%JoyPOV
        Axis_Info .= "  POV" Joy_P
        if (Joy_P > -1) {
			StartAngle := (Joy_P > 33750 || Joy_P <= 2250) ? 247.5 	; up
			: Joy_P > 29250 ? 202.5		; up left
			: Joy_P > 24750 ? 157.5 	; left
			: Joy_P > 20250 ? 112.5 	; down left
			: Joy_P > 15750 ? 67.5 		; down
			: Joy_P > 11250 ? 22.5 		; down right
			: Joy_P > 6750 ? 337.5		; right
			: 292.5 					; up right
            , Gdip_FillPie(G1, pBrush2, PieX, PieY, PieSize, PieSize, StartAngle, 45)
            , Gdip_DrawPie(G1, pPen1, PieX, PieY, PieSize, PieSize, StartAngle, 45)
            , Gdip_DrawPie(G1, pPen2, PieX, PieY, PieSize, PieSize, StartAngle, 45)
        }
    }

    ; Crosshair 1
    CenterX := ScaleX * joy_x
    , CenterY := ScaleY2 * joy_y
    , Gdip_DrawLine(G1, pPen3, CenterX-CrosshairOffset, CenterY, CenterX+CrosshairOffset, CenterY)
    , Gdip_DrawLine(G1, pPen3, CenterX, CenterY-CrosshairOffset, CenterX, CenterY+CrosshairOffset)
    , Gdip_DrawEllipse(G1, pPen4, CenterX-CircleOffset, CenterY-CircleOffset, CircleSize, CircleSize)
    , Gdip_DrawEllipse(G1, pPen4, CenterX-3, CenterY-3, 6, 6)

    ; Crosshair 2
    if ((R || U) && Show2ndCrosshair)
        CenterU := ScaleX * joy_u
        , CenterR := ScaleY2 * joy_r
        , Gdip_DrawLine(G1, pPen5, CenterU-CrosshairOffset, CenterR, CenterU+CrosshairOffset, CenterR)
        , Gdip_DrawLine(G1, pPen5, CenterU, CenterR-CrosshairOffset, CenterU, CenterR+CrosshairOffset)
        , Gdip_DrawEllipse(G1, pPen6, CenterU-CircleOffset, CenterR-CircleOffset, CircleSize, CircleSize)
        , Gdip_DrawEllipse(G1, pPen6, CenterU-3, CenterR-3, 6, 6)

    UpdateLayeredWindow(hwnd1, hdc, 0, 0, A_ScreenWidth, A_ScreenHeight)
    , Gdip_GraphicsClear(G1)
}
return

^Esc::
Exit:
Gdip_Shutdown(pToken)
ExitApp
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      VDU 23,22,512;512;8,16,16,0
      VDU 5
      
      GCOL 4,15
      REPEAT
        B% = ADVAL(0)
        X% = ADVAL(1) / 64
        Y% = 1023 - ADVAL(2) / 64
        PROCjoy(B%,X%,Y%)
        WAIT 4
        PROCjoy(B%,X%,Y%)
      UNTIL FALSE
      END
      
      DEF PROCjoy(B%,X%,Y%)
      LOCAL I%
      LINE X%-32,Y%,X%+32,Y% : LINE X%,Y%-32,X%,Y%+32
      VDU 30
      FOR I% = 0 TO 15
        IF B% AND 1<<I% THEN PRINT "Button "; I%+1 " pressed";
        VDU 10,13
      NEXT
      ENDPROC
```



## Go

{{libheader|termbox-go}}
{{libheader|joystick(go)}}

```go
package main

import (
    "fmt"
    "github.com/nsf/termbox-go"
    "github.com/simulatedsimian/joystick"
    "log"
    "os"
    "strconv"
    "time"
)

func printAt(x, y int, s string) {
    for _, r := range s {
        termbox.SetCell(x, y, r, termbox.ColorDefault, termbox.ColorDefault)
        x++
    }
}

func readJoystick(js joystick.Joystick, hidden bool) {
    jinfo, err := js.Read()
    check(err)

    w, h := termbox.Size()
    tbcd := termbox.ColorDefault
    termbox.Clear(tbcd, tbcd)
    printAt(1, h-1, "q - quit")
    if hidden {
        printAt(11, h-1, "s - show buttons:")
    } else {
        bs := ""
        printAt(11, h-1, "h - hide buttons:")
        for button := 0; button < js.ButtonCount(); button++ {
            if jinfo.Buttons&(1<<uint32(button)) != 0 {
                // Buttons assumed to be numbered from 1, not 0.
                bs += fmt.Sprintf(" %X", button+1)
            }
        }
        printAt(28, h-1, bs)
    }

    // Map axis values in range -32767 to +32768 to termbox co-ordinates.
    x := int(float64((jinfo.AxisData[0]+32767)*(w-1)) / 65535)
    y := int(float64((jinfo.AxisData[1]+32767)*(h-2)) / 65535)
    printAt(x, y, "+") // display crosshair
    termbox.Flush()
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    // Under Linux the id is used to construct the joystick device name.
    // For example: id 0 will open device: "/dev/input/js0"
    // Under Windows the id is the actual numeric id of the joystick.
    jsid := 0
    // Optionally pass joystick id to be used as a command line argument.
    if len(os.Args) > 1 {
        i, err := strconv.Atoi(os.Args[1])
        check(err)
        jsid = i
    }

    js, jserr := joystick.Open(jsid)
    check(jserr)
 
    err := termbox.Init()
    check(err)
    defer termbox.Close()

    eventQueue := make(chan termbox.Event)
    go func() {
        for {
            eventQueue <- termbox.PollEvent()
        }
    }()

    ticker := time.NewTicker(time.Millisecond * 40)
    hidden := false // Controls whether button display hidden or not.

    for doQuit := false; !doQuit; {
        select {
        case ev := <-eventQueue:
            if ev.Type == termbox.EventKey {
                if ev.Ch == 'q' {
                    doQuit = true
                } else if ev.Ch == 'h' {
                    hidden = true
                } else if ev.Ch == 's' {
                    hidden = false
                }
            }
            if ev.Type == termbox.EventResize {
                termbox.Flush()
            }
        case <-ticker.C:
            readJoystick(js, hidden)
        }
    }
}
```



## GUISS

Graphical User Interface Support Script only makes use of installed applications. So for this task, we use the joystick calibration routine, which shows a joystick position indicator:


```guiss
Start,Control Panel, Game Controllers, List:installed controllers,Click:Joystick,
Button:Properties,Button:Test
```



## Haskell

Half-solution of the problem, exhibits X and Y coordinates of the joystick; works on Windows (Haskell Platform):


```Haskell
import qualified Graphics.UI.GLFW as GLFW -- cabal install GLFW-b
import Graphics.Win32.Key
import Control.Monad.RWS.Strict  (liftIO)

main = do
    liftIO $ do
          _ <- GLFW.init
          GLFW.pollEvents
          (jxrot, jyrot) <- liftIO $ getJoystickDirections GLFW.Joystick'1
          putStrLn $ (show jxrot) ++ " " ++ (show jyrot)
          w <- getAsyncKeyState 27 -- ESC pressed?
          if (w<1) then main else do 
                     GLFW.terminate
                     return ()
                     
getJoystickDirections :: GLFW.Joystick -> IO (Double, Double)

getJoystickDirections js = do
    maxes <- GLFW.getJoystickAxes js
    return $ case maxes of
      (Just (x:y:_)) -> (-y, x)
      _ -> ( 0, 0)

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 CLEAR SCREEN
110 DO
120   LET J=JOY(0) OR JOY(1) OR JOY(2)
130   PRINT AT 1,1:"                           ";:PRINT AT 1,1:"";
140   IF J BAND 1 THEN PRINT "right ";
150   IF J BAND 2 THEN PRINT "left ";
160   IF J BAND 4 THEN PRINT "down ";
170   IF J BAND 8 THEN PRINT "up ";
180   IF J BAND 16 THEN PRINT "fire ";
190 LOOP
```



## Julia


```julia
using CSFML.LibCSFML, Gtk.ShortNames, Colors, Graphics, Cairo

#------------ input code ----------------------#

mutable struct Joystick
    devnum::Int
    isconnected::Bool
    hasXaxis::Bool
    nbuttons::Int
    pressed::Vector{Bool}
    ypos::Int
    xpos::Int
    name::String
    Joystick(n, b=2, c=false, x=true) = new(n, c, x, b, fill(false, b), 0, 0)
end

const devnum = 0
const buttons = 2
const jstick = Joystick(devnum, buttons)

function polljoystick(jstick, sleepinterval=0.05)
    while !sfJoystick_isConnected(jstick.devnum)
        sleep(0.25) # wait till connected
        sfJoystick_update()
    end
    jstick.name =  sfJoystick_getIdentification(jstick.devnum).name
    jstick.isconnected = true
    jstick.hasXaxis = sfJoystick_hasAxis(jstick.devnum, 0)
    jstick.nbuttons = sfJoystick_getButtonCount(jstick.devnum)
    while true
        sfJoystick_update()
        for i in 1:jstick.nbuttons
            jstick.pressed[i] =  sfJoystick_isButtonPressed(jstick.devnum, i - 1)
        end
        jstick.ypos = sfJoystick_getAxisPosition(jstick.devnum, 1)
        jstick.xpos = sfJoystick_getAxisPosition(joystick.devnum, 0)
        yield()
        sleep(sleepinterval)
    end
end

#------------ output code -------------------#

makelabel() = "Button 1: " * (jstick.pressed[1] ? "DOWN" : "UP") *
    "  Button 2: " * (jstick.pressed[2] ? "DOWN" : "UP")

const fontpointsize = 80
const wid = 500
const hei = 500
win = Window("Cursor Task", wid, hei) |> (Frame() |> (vbox = Box(:v)))
set_gtk_property!(vbox, :expand, true)
can = Canvas(wid, hei)
label = Label(makelabel())
push!(vbox, can, label)

joytoxpos() = div((jstick.xpos + 100) * width(can), 200)
joytoypos() = div((jstick.ypos + 100) * height(can), 200)
Gtk.showall(win)

@guarded draw(can) do widget
    ctx = getgc(can)
    select_font_face(ctx, "Courier", Cairo.FONT_SLANT_NORMAL, Cairo.FONT_WEIGHT_BOLD)
    set_font_size(ctx, fontpointsize)
    set_source(ctx, colorant"red")
    move_to(ctx, joytoxpos(), joytoypos())
    show_text(ctx, "+")
    set_gtk_property!(label, :label, makelabel())
end

@async polljoystick(jstick)

while true
    draw(can)
    sleep(0.2)
    yield()
end

```



## Liberty BASIC


```lb
    'disable text window
    nomainwin

    'set window size
    WindowWidth  = 308
    WindowHeight = 331

    'center window on screen
    UpperLeftX = int((DisplayWidth-WindowWidth)/2)
    UpperLeftY = int((DisplayHeight-WindowHeight)/2)

    'open graphics window
    open "Joystick Position" for graphics_nf_nsb as #m

    'trap window closing
    #m "trapclose [quit]"

    'put pen down
    #m "down"

    'get center of graphics window
    #m "home"
    #m "posxy CenterX CenterY"

    'draw sprite for crosshair
    #m "backcolor black; color black"
    #m "place 0 20;boxfilled 20 40"
    #m "line  0 10 20 10"
    #m "line 10  0 10 20"
    #m "place 10 10; circle 10"
    #m "backcolor white; color red"
    #m "line  0 30 20 30"
    #m "line 10 20 10 40"
    #m "place 10 30; circle 10"
    #m "flush"

    'get sprite image
    #m "getbmp plus 0 0 20 40"
    #m "cls"

    'create sprite from image
    #m "addsprite crosshair plus"
    #m "centersprite crosshair"
    #m "spritexy crosshair "; CenterX; " "; CenterY
    #m "drawsprites"

    'check joystick every 100 milliseconds
    timer 100, [CheckJoystick]
    wait

[CheckJoystick]
    readjoystick 1

    'calculate crosshair position
    PosX = int(CenterX*Joy1x/32767)
    PosY = int(CenterY*Joy1y/32767)

    'update crosshair position
    #m "spritexy crosshair "; PosX; " "; PosY
    #m "drawsprites"

    'display button information
    if Joy1button1 > 0 then #m "place 0 0;\\Button 1 pressed"
    if Joy1button2 > 0 then #m "place 0 0;\\\Button 2 pressed"

    wait

[quit]
    timer 0
    close #m
    unloadbmp "plus"
    end
```



## Locomotive Basic


```locobasic
10 MODE 1:BORDER 14:x=320:y=200:d=1
20 a=JOY(0)  ' read state of first joystick
30 IF d THEN q$="*" ELSE q$=" "
40 IF a THEN MOVE x-8,y+8:TAG:PRINT q$;:TAGOFF
50 IF (a AND 1) AND y<380 THEN y=y+10
60 IF (a AND 2) AND y>20  THEN y=y-10
70 IF (a AND 4) AND x>20  THEN x=x-10
80 IF (a AND 8) AND x<620 THEN x=x+10
90 IF a AND 16 THEN LOCATE 1,1:PRINT "Fire1 pressed":d=1
100 IF a AND 32 THEN LOCATE 1,2:PRINT "Fire2 pressed":d=0
110 IF a<16 THEN LOCATE 1,1:PRINT "             ":PRINT "             "
120 MOVE x-8,y+8:TAG:PRINT "X";:TAGOFF
130 GOTO 20
```


Output (this version supports drawing with the cursor):

[[File:CPC drawing with joystick.png]]


## Mathematica


```Mathematica
Slider2D[Dynamic[ControllerState[{"X", "Y"}]], ImageSize -> {500, 500}]
```



## OCaml


{{libheader|SFML}}
{{libheader|ocaml-sfml}}


```ocaml
let remove x = List.filter (fun y -> y <> x)
let buttons_string b =
  String.concat " " (List.map string_of_int b)
 
let position app x y =
  let view = SFRenderWindow.getView app in
  let width, height = SFView.getSize view in
  let hw = width /. 2.0 in
  let hh = height /. 2.0 in
  (hw +. ((x /. 100.0) *. hw),
   hh +. ((y /. 100.0) *. hh))
 
let cross =
  [|  1.0,   1.0;  10.0,   1.0;  10.0, -1.0;    1.0, -1.0;
      1.0, -10.0;  -1.0, -10.0;  -1.0, -1.0;  -10.0, -1.0;
    -10.0,   1.0;  -1.0,   1.0;  -1.0, 10.0;    1.0, 10.0; |]
 
let () =
  let app = SFRenderWindow.make (800, 600) "Joystick Position" in
  let text = SFText.make "" in
  let shape = SFShape.create cross in
  SFShape.setFillColor shape SFColor.white;
  SFShape.setOutlineColor shape SFColor.white;
  SFShape.setOutlineThickness shape 1.0;
  let rec display ((x, y), b) =
    SFText.setString text (buttons_string b);
    let x, y = position app x y in
    SFShape.setPosition shape (x, y);
    SFRenderWindow.clear app SFColor.black;
    SFRenderWindow.drawText app text ();
    SFRenderWindow.drawShape app shape ();
    SFRenderWindow.display app;
  and loop joyd =
    let get_joystick (((x, y), b) as joyd) = function
    | SFEvent.JoystickButtonPressed (0, button) -> ((x, y), button::b)
    | SFEvent.JoystickButtonReleased (0, button) -> ((x, y), remove button b)
    | SFEvent.JoystickMoved (0, SFJoystick.X, av) -> ((av, y), b)
    | SFEvent.JoystickMoved (0, SFJoystick.Y, av) -> ((x, av), b)
    | _ -> joyd
    in
    let rec proc_event joyd =
      match SFRenderWindow.pollEvent app with
      | Some SFEvent.KeyPressed (SFKey.Escape,_,_,_,_)
      | Some SFEvent.Closed -> ()
      | Some event ->
          let joyd = get_joystick joyd event in
          proc_event joyd
      | None ->
          display joyd;
          loop joyd
    in
    proc_event joyd
  in
  loop ((0.0, 0.0), [])
```


Run with the command:
 $ ocaml -I /tmp/ocaml-sfml/src sfml_system.cma sfml_window.cma sfml_graphics.cma joy.ml


## PicoLisp

This is for the 64-bit version.
{{libheader|GLUT}}
Note: The code is not yet tested with a real joystick (I don't have one), it was just simulated with dummy functions. Can somebody having a joystick please test it, and remove this message?

```PicoLisp
(load "@lib/openGl.l")

(setq *JoyX 0.0  *JoyY 0.0)

(glutInit)
(glutInitDisplayMode (| GLUT_RGBA GLUT_DOUBLE GLUT_ALPHA GLUT_DEPTH))
(glutInitWindowSize 400 400)
(glutCreateWindow "Joystick")

(glClearColor 0.3 0.3 0.5 0)

(displayPrg
   (glClear GL_COLOR_BUFFER_BIT)
   (glBegin GL_LINES)
   (glVertex2f *JoyX (- *JoyY 0.1))  # Draw crosshair
   (glVertex2f *JoyX (+ *JoyY 0.1))
   (glVertex2f (- *JoyX 0.1) *JoyY)
   (glVertex2f (+ *JoyX 0.1) *JoyY)
   (glEnd)
   (glFlush)
   (glutSwapBuffers) )

# Track joystick position
(native `*GlutLib "glutJoystickFunc" NIL
   (lisp 'joystickFunc
      '((Btn X Y Z)
         (msg                          # Display buttons
            (make
               (for (B 1 (n0 Btn) (inc B))
                  (and (bit? 1 Btn) (link B))
                  (setq Btn (>> 1 Btn)) ) ) )
         (setq                         # Move crosshair
            *JoyX (*/ X 1.0 1000)
            *JoyY (*/ Y 1.0 1000) )
         (glutPostRedisplay) ) )
   100 )

# Exit upon mouse click
(mouseFunc '((Btn State X Y) (bye)))
(glutMainLoop)
```



## PureBasic

This is limited to only digital joysticks.

```PureBasic
If InitJoystick() = 0
  MessageRequester("Error!", "Need to connect a joystick", #PB_MessageRequester_Ok)
  End
EndIf

;some constants for Window positioning
#WindowW = 100: #WindowH = 100
#CrossW = 10
#p1 = (#WindowW - #CrossW) / 2
#p2 = (#WindowW / 2 - #CrossW)

If OpenWindow(0, 0, 0, #WindowW * 2 + 10, #WindowH, "Joystick Position", #PB_Window_SystemMenu)
  CreateImage(0, #WindowW, #WindowW)
  ImageGadget(0, 0, 0, 0, 0, ImageID(0))
  TextGadget(2, #WindowW + 5, 10, #WindowW, 20, "Buttons Pressed:")
  CreateImage(1, #WindowW, 40)
  ImageGadget(1,  #WindowW + 5, 30, 0, 0, ImageID(1))
  
  AddKeyboardShortcut(0, #PB_Shortcut_Escape, 0)
  Define event, x_movement, y_movement
  Repeat 
    Repeat
      event = WindowEvent()
      Select event
        Case #PB_Event_Menu
          If EventMenu() = 0
            End
          EndIf
        Case #PB_Event_CloseWindow
          End
      EndSelect
    Until event = 0
    
    Define pressed.s, buttonNum, buttonX, buttonY, buttonText.s, buttonColor
    pressed.s = ""
    If ExamineJoystick(0)
      x_movement = JoystickAxisX(0)
      y_movement = JoystickAxisY(0)
      
      StartDrawing(ImageOutput(1))
        DrawingMode(#PB_2DDrawing_Transparent)
        Box(0, 0, #WindowW, 50, RGB($D4, $D0, $C8)) ;a Gray
        ; check to see if any of the buttons have been pressed
        For buttonNum = 1 To 10
          buttonX = ((buttonNum - 1) * 20 + 10) % #WindowW
          buttonY = ((buttonNum - 1) / 5) * 20 + 10
          If JoystickButton(0, buttonNum)
            buttonColor = RGB($FF, 0, 0) ;Red
          Else
            buttonColor = RGB($80, $80, $80) ;Gray
          EndIf 
          Circle(buttonX, buttonY, 9, buttonColor)
          buttonText = Str(buttonNum)
          DrawText(buttonX - TextWidth(buttonText) / 2, buttonY - TextHeight(buttonText) / 2, buttonText, RGB($FF, $FF, $FF)) ;White
        Next
      StopDrawing()
      
      SetGadgetState(1, ImageID(1))
    EndIf
    
    
    StartDrawing(ImageOutput(0))
      Box(0,0, #WindowW, #WindowW, RGB($FF, $FF, $FF)) ;White
      Line(#p1 + x_movement * #p2, #WindowW / 2 + y_movement * #p2, #CrossW, 1, RGB($FF, 0, 0)) ;Red
      Line(#WindowW / 2 + x_movement * #p2, #p1 + y_movement * #p2, 1, #CrossW, RGB($FF, 0, 0)) ;Red
    StopDrawing()
    
    SetGadgetState(0, ImageID(0))
    
    Delay(10)
  Until event = #PB_Event_CloseWindow
EndIf
```

[[File:JoystickPosition-PureBasic.png]]


## Python

{{libheader|Pygame}}

```Python
import sys
import pygame

pygame.init()

# Create a clock (for framerating)
clk = pygame.time.Clock()

# Grab joystick 0
if pygame.joystick.get_count() == 0:
    raise IOError("No joystick detected")
joy = pygame.joystick.Joystick(0)
joy.init()

# Create display
size = width, height = 600, 600
screen = pygame.display.set_mode(size)
pygame.display.set_caption("Joystick Tester")

# Frame XHair zone
frameRect = pygame.Rect((45, 45), (510, 510))

# Generate crosshair
crosshair = pygame.surface.Surface((10, 10))
crosshair.fill(pygame.Color("magenta"))
pygame.draw.circle(crosshair, pygame.Color("blue"), (5,5), 5, 0)
crosshair.set_colorkey(pygame.Color("magenta"), pygame.RLEACCEL)
crosshair = crosshair.convert()

# Generate button surfaces
writer = pygame.font.Font(pygame.font.get_default_font(), 15)
buttons = {}
for b in range(joy.get_numbuttons()):
    buttons[b] = [
        writer.render(
            hex(b)[2:].upper(),
            1,
            pygame.Color("red"),
            pygame.Color("black")
        ).convert(),
        # Get co-ords: ((width*slot)+offset, offset). Offsets chosen
        #                                             to match frames.
        ((15*b)+45, 560)
    ]

while True:
    # Pump and check the events queue
    pygame.event.pump()
    for events in pygame.event.get():
        if events.type == pygame.QUIT:
            pygame.quit()
            sys.exit()

    # Black the screen
    screen.fill(pygame.Color("black"))

    # Get joystick axes
    x = joy.get_axis(0)
    y = joy.get_axis(1)

    # Blit to the needed coords:
    # x*amplitude+(centre offset (window size/2))-(xhair offset (xh size/2))
    screen.blit(crosshair, ((x*250)+300-5, (y*250)+300-5))
    pygame.draw.rect(screen, pygame.Color("red"), frameRect, 1)

    # Get and display the joystick buttons
    for b in range(joy.get_numbuttons()):
        if joy.get_button(b):
            screen.blit(buttons[b][0], buttons[b][1])

    # Write the display
    pygame.display.flip()
    clk.tick(40) # Limit to <=40 FPS
```



## Tcl

{{libheader|Tk}}
{{libheader|mkLibsdl}} <!-- from http://mkextensions.sourceforge.net/ -->

```tcl
package require Tk 8.6
package require mkLibsdl

# This code assumes we're dealing with the first pair of axes on the first
# joystick; modern joysticks are complex...

# Callback for all joystick activity
proc display {joyDict} {
    global x y buttons message
    set axis -1
    dict with joyDict {
	if {$joystick != 0} return
	if {[info exist button]} {
	    # Handle button presses...
	    set buttons($button) $value
	    set message "Buttons:"
	    foreach b [lsort -integer [array names buttons]] {
		if {$buttons($b)} {
		    lappend message $b
		}
	    }
	} else {
	    # Handle joystick movement...
	    if {$axis == -1} return
	    set value [expr {$value / 32768.0 * 100 + 120}]
	    if {$axis == 0} {
		set x $value
	    } elseif {$axis == 1} {
		set y $value
	    }
	    .c coords xhairV $x [expr {$y-5}] $x [expr {$y+5}]
	    .c coords xhairH [expr {$x-5}] $y [expr {$x+5}] $y
	}
    }
}

# Make a GUI
set message "Buttons:"
pack [canvas .c -width 240 -height 240] [label .l -textvariable message]
set x [set y 120]
.c create line {120 115 120 125} -tags xhairV
.c create line {115 120 125 120} -tags xhairH
joystick event eval {display [joystick event peek]}
```

<!-- I think it can also be done with Tcl3d as that has SDL Joystick bindings, but damned if I can figure out the API in a reasonable amount of effort. It's definitely not Tclish (but what would you expect with something mashed together with SWIG? Once a stinky C API, always a stinky C API…) -->

=={{Header|ZX Spectrum Basic}}==
Assuming a Kempston joystick interface, fairly standard at the time. Rewriting for the Sinclair Interface 2 involves changing line 80 to 80 LET t=IN 61438, replacing all numbers in the next five lines with 191-said number and reversing each pair of directions.


```zxbasic
10 DIM o$(5)
20 LET xm=0: LET xx=31: REM screen limits
30 LET ym=0: LET yx=18
40 LET xo=16: LET yo=9: REM origin
50 LET xc=xo: LET yc=yo
60 PRINT AT yc,xc;" ": REM entry point for scan loop - blank cursor
70 LET xc=xo: LET yc=yo
80 LET t=IN 31
90 IF t=1 OR t=5 OR t=9 OR t=17 OR t=21 OR t=25 THEN LET xc=xx: LET o$(2)="R": REM right
100 IF t=2 OR t=6 OR t=10 OR t=18 OR t=22 OR t=26 THEN LET xc=xm: LET o$(1)="L": REM left
110 IF t=4 OR t=5 OR t=6 OR t=20 OR t=21 OR t=22 THEN LET yc=yx: LET o$(4)="D": REM down
120 IF t=8 OR t=9 OR t=10 OR t=24 OR t=25 OR t=26 THEN LET yc=ym: LET o$(3)="U": REM up
130 IF t>=16 THEN LET o$(5)="F": REM fire
140 PRINT AT 21,0;"Input:";o$
150 PRINT AT yc,xc;"+"
160 LET o$="     ": REM five spaces to blank output line again
170 GO TO 60
```


{{Out}}

```txt

+

Input:L  D

```


{{omit from|AWK}}
{{omit from|C sharp}}
{{omit from|Lilypond}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|ML/I}}
{{omit from|Maxima}}
{{omit from|Octave}}
{{omit from|PARI/GP}}
{{omit from|R}}
{{omit from|Retro}}
{{omit from|SAS}}
{{omit from|Scilab}}
{{omit from|SQL PL|Cannot connect a joystick}}
{{omit from|Stata}}
{{omit from|TI-83 BASIC|Cannot connect a joystick.}}
{{omit from|zkl}}
