+++
title = "Dragon curve/AutoHotkey"
description = ""
date = 2009-11-24T16:30:06Z
aliases = []
[extra]
id = 4994
[taxonomies]
categories = []
tags = []
+++

{{libheader|GDIP}}


```AutoHotkey
; LSystem RosettaCode

#SingleInstance, Off
#NoEnv

	setbatchlines -1

	pToken := Gdip_Startup()
	OnExit, Cleanup

	;Dragon curve
	DragonCurveIter  := 15
	DragonCurveRules := "X>X+YF,Y>FX-Y"
	DragonCurveStart := "FX"
	DragonCurveCmds  := "F+-"
	DragonCurveCmds2 := "draw 3,angle -90,angle +90"
	
	linecount := DispLSystem( DragonCurveIter, DragonCurveRules, DragonCurveStart, DragonCurveCmds, DragonCurveCmds2, 0 )

	MsgBox % "Done!`nThere are " linecount " lines in this L-System.`nCreated in " errorlevel " seconds.`nThis is an average of " (linecount / errorlevel) " lines per second."
return

Esc::
GuiClose:
Cleanup:
	Gdip_Shutdown(pToken)
	ExitApp

DispLSystem( iter, rules, start, names, commands, live = 0 ) {
	DllCall("QueryPerformanceFrequency", "int64*", qpcf), DllCall("QueryPerformanceCounter", "int64*", qpcs)
	
	turtle := LSystem( iter, rules, start )
	cnt := LSys2Lines( turtle, names, commands, lines, w, h )
	
	MakeLayeredWindow( hwnd, hdc, hbm, obm, gfx, w, h )
;	UpdateLayeredWindow(hwnd, hdc)
	DrawLinesToLayeredWindow( lines, cnt, hwnd, hdc, gfx, live )
	UpdateLayeredWindow(hwnd, hdc)
	CleanupLayeredWindow( hwnd, hdc, hbm, obm, gfx )
	
	DllCall("QueryPerformanceCounter", "int64*", qpce)
	return cnt, ErrorLevel := (qpce - qpcs) / qpcf
}

; lsystem and conversion functions
LSystem( cn, Vr, st ) {
;
; cn: iterations
; Vr: list of variables and their rules
;    (V>Result)[,...]
;    Where
;      V: Any single-character variable (must be valid as an AHK variable)
;          a-z 1-9 _ (case insensitive)
;      Result: The rule that this variable results in when replaced
;          [All characters except for comma] (must not start with a quote char: ")
;          OR
;          "[All characters]" (starts and ends with quote char. any literal quote chars inside must be duplicated)
; st : start string
;
; Any non-variable character is treated as a constant
; 
	it := 1
	while it := RegExMatch(Vr, "\s*(\w)\s*>\s*(?:(?!"")([^,]*)|""((?:[^""]|"""")*)"")\s*(?:,|$)", __, it + StrLen(__))
		%__1% := __2 RegExReplace(__3, """""", """"), Vl .= __1 ;parse Variables+Rules
	loop % cn
	{
		tp := "" st, st := ""
		loop, parse, tp
			st .= InStr(Vl, A_LoopField) ? %A_LoopField% : A_LoopField
	}
	return st
}
LSys2Lines( ls, chars, commands, byref lines, byref w, byref h ) { ;based on l-system
	l := 0
	, st%l%x := 0
	, st%l%y := 0
	, st%l%a := 0
	, i := 0
	lines_cap := VarSetCapacity(lines, strlen(ls) * 8)
	maxx := maxy := minx := miny := 0
	loop, parse, commands, `,, %A_Space%%A_Tab%
		cmd%A_Index% := A_LoopField
	loop, parse, ls
	{
		if !p := InStr(chars, A_LoopField)
			continue
		else if inStr(cmd%p%, "save")
			ol := l++
			, st%l%x := st%ol%x
			, st%l%y := st%ol%y
			, st%l%a := st%ol%a
		else if inStr(cmd%p%, "restore")
			l--
		else if inStr(cmd%p%, "angle")
			st%l%a += substr(cmd%p%, 6)
		else if inStr(cmd%p%, "draw")
		{
			d  := SubStr(cmd%p%, 5) + 0     ;distance (in pixels)
			AddLine( lines, i
				, Round(st%l%x)
				, Round(st%l%y)                           
				, Round(st%l%x += cos(st%l%a/57.2957795) * d)
				, Round(st%l%y += sin(st%l%a/57.2957795) * d) )
			, maxx := maxx > st%l%x ? maxx : st%l%x
			, maxy := maxy > st%l%y ? maxy : st%l%y
			, minx := minx < st%l%x ? minx : st%l%x
			, miny := miny < st%l%y ? miny : st%l%y
		}
	}
	xoff := round(abs(minx)) + 5
	yoff := round(abs(miny)) + 5
	w := round(maxx) + xoff + 5
	h := round(maxy) + yoff + 5
	
	OffsetPoints(lines, i, xoff, yoff)
	return i//4
}

; line functions
AddLine( ByRef lines, ByRef i, x1, y1, x2, y2 ) {
	static lines_sz = 0
	if (i = 0 || lines_sz = 0)
		lines_sz := VarSetCapacity(lines)
	if (lines_sz < i * 4 + 16)
		MemPreAlloc(lines, lines_sz, i + 16), lines_sz := VarSetCapacity(lines) 
	  NumPut(x1, lines, 4 * i++, "int")
	, NumPut(y1, lines, 4 * i++, "int")
	, NumPut(x2, lines, 4 * i++, "int")
	, NumPut(y2, lines, 4 * i++, "int")
}
OffsetPoints( ByRef lines, cnt, xoff, yoff ) {
	i := 0
	while i < cnt
		NumPut(NumGet(lines, i * 4, "int") + (i & 1 ? yoff : xoff), lines, 4 * i++, "int")
}
DrawLinesToLayeredWindow( ByRef lines, cnt, hwnd, hdc, gfx, live = 0, color = 0xFF009900, lnw = 1 ) {
	cnt *= 4
	i := 0
	pPen := Gdip_CreatePen(color, 1)
	while i < cnt
	{
		Gdip_DrawLine(gfx, pPen
			, NumGet(lines, 4 * i++, "int")
			, NumGet(lines, 4 * i++, "int")
			, NumGet(lines, 4 * i++, "int")
			, NumGet(lines, 4 * i++, "int"))
	;	if live && !Mod(A_Index, live)
	;		UpdateLayeredWindow(hwnd, hdc)
	}
	Gdip_DeletePen(pPen)
}

; Display functions
MakeLayeredWindow( ByRef hwnd, ByRef hdc, ByRef hbm, ByRef obm, ByRef gfx, w, h, bg = 0xccCCccCC, guiNum = 5 ) {
	w += 1, h += 1
	Gui, %guiNum%:+LastFound +AlwaysOnTop +Owner +E0x80000 -Caption
	Gui, %guiNum%:Show, w%w% h%h% NoActivate
	OnMessage(0x201, "WM_LBUTTONDOWN")

	;gdi
	hwnd := WinExist()
	hdc  := CreateCompatibleDC()
	hbm  := CreateDIBSection(w, h) ;needs to be the same size as gui
	obm  := SelectObject(hdc, hbm)
	gfx  := Gdip_GraphicsFromHDC(hdc)

	;background
	pBr := Gdip_BrushCreateSolid(bg)
	Gdip_FillRoundedRectangle(gfx, pBr, 0, 0, w, h, 3)
	Gdip_DeleteBrush(pBr)
}
CleanupLayeredWindow( ByRef hwnd, ByRef hdc, ByRef hbm, ByRef obm, ByRef gfx ) {
	SelectObject(hdc, obm), DeleteObject(hbm), DeleteDC(hdc)
	, Gdip_DeleteGraphics(gfx)
}
WM_LBUTTONDOWN() { ;for dragging the gui
		static mx, my, wx, wy, hwnd
		hwnd := WinExist()
		CoordMode, Mouse, Screen
		mousegetpos, mx, my
		WinGetPos, wx, wy
		settimer, GuiDrag, 10
	return

	GuiDrag:
		CoordMode, Mouse, Screen
		setwindelay, -1
		if !GetKeyState( "LButton", "P" )
			settimer, %A_ThisLabel%, Off
		else
		{
			MouseGetPos, nmx, nmy
			WinMove, ahk_id %hwnd%, , wx + (nmx - mx), wy + (nmy - my)
		}
	return
}

; binary memory functions
MemPreAlloc( ByRef var, len, add, fillbyte = 0 ) {
	VarSetCapacity(t, len+add, fillbyte)
	MemMove(&t, &var, len)
	VarSetCapacity(var, len+add, 0)
	MemMove(&var, &t, len+add)
}
MemMove( dest, src, len ) {
	; may need to call VarSetCapacity(var, -1) afterwards for display
	DllCall( "msvcrt\memmove", "UInt", dest+0, "UInt", src+0, "Int", len, "Cdecl int" )
}
```

Output:

[[File:AutoHotkeyDragonCurve.png]]
