+++
title = "Bresenham's Line Algorithm"
description = ""
date = 2019-05-18T10:31:53Z
aliases = []
[extra]
id = 3214
task = """
  Using the data storage type defined on the [[Bitmap]] page
  for raster graphics images,
  draw a line given two points with
  Bresenham's line algorithm.
"""
[taxonomies]
categories = []
tags = ["graphics", "raster-graphics"]
+++


## 360 Assembly

{{trans|Rexx}}

```360asm
*        Bitmap/Bresenham's line algorithm - 13/05/2019
BRESENH  CSECT
         USING  BRESENH,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R10,DATAXY         @dataxy
         LA     R8,1               p=1
     DO WHILE=(C,R8,LE,=A(POINTS)) do p=1 to points
         L      R6,0(R10)            x=dataxy((p-1)*2+1)
         L      R7,4(R10)            y=dataxy((p-1)*2+2)
       IF     C,R8,EQ,=F'1' THEN     if p=1 then
         ST     R6,MINX                minx=x
         ST     R6,MAXX                maxx=x
         ST     R7,MINY                miny=y
         ST     R7,MAXY                maxy=y
       ENDIF    ,                    endif
       IF     C,R6,LT,MINX THEN      if x<minx then
         ST     R6,MINX                minx=x
       ENDIF    ,                    endif
       IF     C,R6,GT,MAXX THEN      if x>maxx then
         ST     R6,MAXX                maxx=x
       ENDIF    ,                    endif
       IF     C,R7,LT,MINY THEN      if y<miny then
         ST     R7,MINY                miny=y
       ENDIF    ,                    endif
       IF     C,R7,GT,MAXY THEN      if y>maxy then
         ST     R7,MAXY                maxy=y
       ENDIF    ,                    endif
         LA     R10,8(R10)           @dataxy+=8
         LA     R8,1(R8)             p++
     ENDDO      ,                  enddo p
         L      R1,MINX            minx
         S      R1,=A(BORDER*2)    -border*2
         ST     R1,MINX            minx=minx-border*2
         L      R1,MAXX            maxx
         A      R1,=A(BORDER*2)    +border*2
         ST     R1,MAXX            maxx=maxx+border*2
         L      R1,MINY            miny
         S      R1,=A(BORDER)      -border
         ST     R1,MINY            miny=miny-border
         L      R1,MAXY            maxy
         A      R1,=A(BORDER)      +border
         ST     R1,MAXY            maxy=maxy+border
         L      R1,MINX            minx
         LCR    R1,R1              -
         A      R1,=F'1'           +1
         ST     R1,OX              ox=-minx+1
         L      R1,MINY            miny
         LCR    R1,R1              -
         A      R1,=F'1'           +1
         ST     R1,OY              oy=-miny+1
         LA     R1,HMAPX           hbound(map,1)
         S      R1,OX              wx=hbound(map,1)-ox
       IF     C,R1,LT,MAXX THEN    if maxx>wx then
         ST     R1,MAXX              maxx=wx
       ENDIF    ,                  endif
         LA     R1,HMAPY           hbound(map,2)
         S      R1,OY              wy=hbound(map,2)-oy
       IF     C,R1,LT,MAXY THEN    if maxy>wy then
         ST     R1,MAXY              maxy=wy
       ENDIF    ,                  endif
         L      R6,MINX            x=minx
       DO WHILE=(C,R6,LE,MAXX)     do x=minx to maxx
         L      R1,OY                oy
         BCTR   R1,0                 1
         MH     R1,=AL2(HMAPX)       dim(x)
         AR     R1,R6                x
         A      R1,OX                ox
         LA     R1,MAP-1(R1)         map(0+oy,x+ox)
         MVC    0(1,R1),=C'-'        map(0+oy,x+ox)='-'
         A      R6,=F'1'             x++
       ENDDO    ,                  enddo x
         L      R7,MINY            y=miny
       DO WHILE=(C,R7,LE,MAXY)     do y=miny to maxy
         LR     R1,R7                y
         A      R1,OY                +oy
         BCTR   R1,0                 -1
         MH     R1,=AL2(HMAPX)       *dim(x)
         A      R1,OX                +ox
         LA     R1,MAP-1(R1)         @map(y+oy,0+ox)
         MVC    0(1,R1),=C'|'        map(y+oy,0+ox)='|'
         A      R7,=F'1'             y++
       ENDDO    ,                  enddo y
         L      R1,OY              +oy
         BCTR   R1,0               -1
         MH     R1,=AL2(HMAPX)     *dim(x)
         A      R1,OX              +ox
         LA     R1,MAP-1(R1)       @map(0+oy,0+ox)
         MVC    0(1,R1),=C'+'      map(0+oy,0+ox)='+'
         LA     R10,POINTS         points
         BCTR   R10,0              pn=points-1
         LA     R9,DATAXY          @dataxy
         LA     R8,1               p=1
       DO WHILE=(CR,R8,LE,R10)     do p=1 to points-1
         L      R6,0(R9)             x=dataxy((p-1)*2+1)
         L      R7,4(R9)             y=dataxy((p-1)*2+2)
         L      R4,8(R9)             xf=dataxy(p*2+1)
         L      R5,12(R9)            yf=dataxy(p*2+2)
         LR     R2,R4                xf
         SR     R2,R6                -x
         LPR    R2,R2                abs()
         ST     R2,DX                dx=abs(xf-x)
         LR     R2,R5                xf
         SR     R2,R7                -y
         LPR    R2,R2                abs()
         ST     R2,DY                dy=abs(yf-y)
       IF    CR,R6,LT,R4 THEN        if x<xf then
         MVC    SX,=F'1'               sx=+1
       ELSE     ,                    else
         MVC    SX,=F'-1'              sx=-1
       ENDIF    ,                    endif
       IF    CR,R7,LT,R5 THEN        if y<yf then
         MVC    SY,=F'1'               sy=+1
       ELSE     ,                    else
         MVC    SY,=F'-1'              sy=-1
       ENDIF    ,                    endif
         L      R2,DX                dx
         S      R2,DY                -dy
         ST     R2,ERR               err=dx-dy
LOOP     EQU    *                    loop forever
         LR     R1,R7                  y
         A      R1,OY                  +oy
         BCTR   R1,0                   -1
         MH     R1,=AL2(HMAPX)         *dim(x)
         AR     R1,R6                  +x
         A      R1,OX                  +ox
         LA     R1,MAP-1(R1)           @map(y+oy,x+ox)
         MVC    0(1,R1),=C'X'          map(y+oy,x+ox)='X'
         CR     R6,R4                  if x=xf
         BNE    STAYDO                 ~
         CR     R7,R5                  if y=yf
         BE     EXITLOOP               if x=xf and y=yf then leave loop
STAYDO   L      R0,ERR                 err
         A      R0,ERR                 err+err
         ST     R0,ERR2                err2=err+err
         L      R0,DY                  dy
         LCR    R0,R0                  -dy
       IF     C,R0,LT,ERR2 THEN        if err2>-dy then
         A      R0,ERR                   -dy+err
         ST     R0,ERR                   err=err-dy
         A      R6,SX                    x=x+sx
       ENDIF    ,                      endif
         L      R0,DX                  dx
       IF     C,R0,GT,ERR2 THEN        if err2<dx then
         L      R0,ERR                   err
         A      R0,DX                    +dx
         ST     R0,ERR                   err=err+dx
         A      R7,SY                    y=y+sy
       ENDIF    ,                      endif
         B      LOOP                 endloop
EXITLOOP LA     R9,8(R9)             @dataxy+=2
         LA     R8,1(R8)             p++
       ENDDO    ,                  enddo p
         LA     R9,MAP+(HMAPX*HMAPY)-HMAPX  @map
         L      R7,MAXY            y=maxy
       DO WHILE=(C,R7,GE,MINY)     do y=maxy to miny by -1
         MVC    PG(HMAPX),0(R9)      output map(x,*)
         XPRNT  PG,L'PG              print buffer
         S      R9,=A(HMAPX)         @pg
         S      R7,=F'1'             y--
       ENDDO    ,                  enddo y
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
BORDER   EQU    2                  border size
POINTS   EQU    (MAP-DATAXY)/L'DATAXY/2
HMAPX    EQU    24                 dim(map,1)
HMAPY    EQU    20                 dim(map,2)
DATAXY   DC     F'1',F'8',F'8',F'16',F'16',F'8',F'8',F'1',F'1',F'8'
MAP      DC     (HMAPX*HMAPY)CL1'.'    map(hmapx,hmapy)
OX       DS     F
OY       DS     F
MINX     DS     F
MAXX     DS     F
MINY     DS     F
MAXY     DS     F
DX       DS     F
DY       DS     F
SX       DS     F
SY       DS     F
ERR      DS     F
ERR2     DS     F
PG       DC     CL80' '            buffer
         REGEQU
         END    BRESENH
```

{{out}}

```txt

...|....................
...|....................
...|.......X............
...|......X.X...........
...|.....X...X..........
...|....X.....X.........
...|...X.......X........
...|...X........X.......
...|..X..........X......
...|.X............X.....
...|X..............X....
...|.X............X.....
...|..X..........X......
...|...X.......XX.......
...|....X.....X.........
...|.....X...X..........
...|......X.X...........
...|.......X............
---+--------------------
...|....................

```




## Ada


```ada
procedure Line (Picture : in out Image; Start, Stop : Point; Color : Pixel) is
   DX  : constant Float := abs Float (Stop.X - Start.X);
   DY  : constant Float := abs Float (Stop.Y - Start.Y);
   Err : Float;
   X   : Positive := Start.X;
   Y   : Positive := Start.Y;
   Step_X : Integer := 1;
   Step_Y : Integer := 1;
begin
   if Start.X > Stop.X then
      Step_X := -1;
   end if;
   if Start.Y > Stop.Y then
      Step_Y := -1;
   end if;
   if DX > DY then
      Err := DX / 2.0;
      while X /= Stop.X loop
         Picture (X, Y) := Color;
         Err := Err - DY;
         if Err < 0.0 then
            Y := Y + Step_Y;
            Err := Err + DX;
         end if;
         X := X + Step_X;
      end loop;
   else
      Err := DY / 2.0;
      while Y /= Stop.Y loop
         Picture (X, Y) := Color;
         Err := Err - DX;
         if Err < 0.0 then
            X := X + Step_X;
            Err := Err + DY;
         end if;
         Y := Y + Step_Y;
      end loop;
   end if;
   Picture (X, Y) := Color; -- Ensure dots to be drawn
end Line;
```

The test program's

```ada
   X : Image (1..16, 1..16);
begin
   Fill (X, White);
   Line (X, ( 1, 8), ( 8,16), Black);
   Line (X, ( 8,16), (16, 8), Black);
   Line (X, (16, 8), ( 8, 1), Black);
   Line (X, ( 8, 1), ( 1, 8), Black);
   Print (X);
```

sample output

```txt

       H
      H H
     H   H
    H     HH
   H        H
  H          H
 H            H
H              H
 H            H
  H          H
   H        H
    H      H
    H     H
     H   H
      H H
       H

```



## ALGOL 68

{{trans|Ada}}
{{works with|ALGOL 68|Revision 1 - one minor extension to language used - PRAGMA READ, similar to C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.6 algol68g-2.6].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: prelude/Bitmap/Bresenhams_line_algorithm.a68'''
```algol68
# -*- coding: utf-8 -*- #

line OF class image := (REF IMAGE picture, POINT start, stop, PIXEL color)VOID:
BEGIN
   REAL dx = ABS (x OF stop - x OF start),
        dy = ABS (y OF stop - y OF start);
   REAL err;
   POINT here := start,
         step := (1, 1);
   IF x OF start > x OF stop THEN
      x OF step := -1
   FI;
   IF y OF start > y OF stop THEN
      y OF step := -1
   FI;
   IF dx > dy THEN
      err := dx / 2;
      WHILE x OF here /= x OF stop DO
         picture[x OF here, y OF here] := color;
         err -:= dy;
         IF err < 0 THEN
            y OF here +:= y OF step;
            err +:= dx
         FI;
         x OF here +:= x OF step
      OD
   ELSE
      err := dy / 2;
      WHILE y OF here /= y OF stop DO
         picture[x OF here, y OF here] := color;
         err -:= dx;
         IF err < 0 THEN
            x OF here +:= x OF step;
            err +:= dy
         FI;
         y OF here +:= y OF step
      OD
   FI;
   picture[x OF here, y OF here] := color # ensure dots to be drawn #
END # line #;

SKIP
```
'''File: test/Bitmap/Bresenhams_line_algorithm.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

PR READ "prelude/Bitmap.a68" PR; # c.f. [[rc:Bitmap]] #
PR READ "prelude/Bitmap/Bresenhams_line_algorithm.a68" PR;

### The test program: ###
test:(
   REF IMAGE x = INIT LOC[1:16, 1:16]PIXEL;
   (fill OF class image)(x, white OF class image);
   (line OF class image)(x, ( 1, 8), ( 8,16), black OF class image);
   (line OF class image)(x, ( 8,16), (16, 8), black OF class image);
   (line OF class image)(x, (16, 8), ( 8, 1), black OF class image);
   (line OF class image)(x, ( 8, 1), ( 1, 8), black OF class image);
   (print OF class image)(x)
)
```
'''Output:'''

```txt

ffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffff000000ffffff000000ffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffff000000ffffffffffffffffff000000ffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffff000000ffffffffffffffffffffffffffffff000000000000ffffffffffffffffffffffff
ffffffffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffff
ffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffff
ffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffff
000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000
ffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffff
ffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffff
ffffffffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffff
ffffffffffffffffffffffff000000ffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffff
ffffffffffffffffffffffff000000ffffffffffffffffffffffffffffff000000ffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffff000000ffffffffffffffffff000000ffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffff000000ffffff000000ffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffff000000ffffffffffffffffffffffffffffffffffffffffffffffff

```



## Assembly

16 bit Intel 8086\80486 Assembly for dos, see [http://en.wikipedia.org/wiki/X86_assembly_language x86 assembly language].
To run this code you will need to use Dos emulator.

```txt

.486
IDEAL
;---------------------------------------------
; case: DeltaY is bigger than DeltaX
; input: p1X p1Y,
; 		 p2X p2Y,
;		 Color -> variable
; output: line on the screen
;---------------------------------------------
Macro DrawLine2DDY p1X, p1Y, p2X, p2Y
	local l1, lp, nxt
	mov dx, 1
	mov ax, [p1X]
	cmp ax, [p2X]
	jbe l1
	neg dx ; turn delta to -1
l1:
	mov ax, [p2Y]
	shr ax, 1 ; div by 2
	mov [TempW], ax
	mov ax, [p1X]
	mov [pointX], ax
	mov ax, [p1Y]
	mov [pointY], ax
	mov bx, [p2Y]
	sub bx, [p1Y]
	absolute bx
	mov cx, [p2X]
	sub cx, [p1X]
	absolute cx
	mov ax, [p2Y]
lp:
	pusha
	call PIXEL
	popa
	inc [pointY]
	cmp [TempW], 0
	jge nxt
	add [TempW], bx ; bx = (p2Y - p1Y) = deltay
	add [pointX], dx ; dx = delta
nxt:
	sub [TempW], cx ; cx = abs(p2X - p1X) = daltax
	cmp [pointY], ax ; ax = p2Y
	jne lp
	call PIXEL
ENDM DrawLine2DDY
;---------------------------------------------
; case: DeltaX is bigger than DeltaY
; input: p1X p1Y,
; 		 p2X p2Y,
;		 Color -> variable
; output: line on the screen
;---------------------------------------------
Macro DrawLine2DDX p1X, p1Y, p2X, p2Y
	local l1, lp, nxt
	mov dx, 1
	mov ax, [p1Y]
	cmp ax, [p2Y]
	jbe l1
	neg dx ; turn delta to -1
l1:
	mov ax, [p2X]
	shr ax, 1 ; div by 2
	mov [TempW], ax
	mov ax, [p1X]
	mov [pointX], ax
	mov ax, [p1Y]
	mov [pointY], ax
	mov bx, [p2X]
	sub bx, [p1X]
	absolute bx
	mov cx, [p2Y]
	sub cx, [p1Y]
	absolute cx
	mov ax, [p2X]
lp:
	pusha
	call PIXEL
	popa
	inc [pointX]
	cmp [TempW], 0
	jge nxt
	add [TempW], bx ; bx = abs(p2X - p1X) = deltax
	add [pointY], dx ; dx = delta
nxt:
	sub [TempW], cx ; cx = abs(p2Y - p1Y) = deltay
	cmp [pointX], ax ; ax = p2X
	jne lp
	call PIXEL
ENDM DrawLine2DDX
Macro absolute a
	local l1
	cmp a, 0
	jge l1
	neg a
l1:
Endm
    MODEL small
    STACK 256
    DATASEG
    TempW dw ?
    pointX dw ?
    pointY dw ?
    point1X dw ?
    point1Y dw ?
    point2X dw ?
    point2Y dw ?
    Color db ?
    CODESEG
start:
        mov ax, @data
        mov ds, ax

	mov ax, 13h
	int 10h ; set graphic mode

	mov [Color], 61
	mov [point1X], 300
	mov [point2X], 6
	mov [point1Y], 122
	mov [point2Y], 88
	call DrawLine2D

	mov ah, 00h
	int 16h
exit:
	mov ax,03h
	int 10h ; set text mode

        mov ax, 4C00h
        int 21h
; procedures
;---------------------------------------------
; input: point1X point1Y,
; 		 point2X point2Y,
;		 Color
; output: line on the screen
;---------------------------------------------
PROC DrawLine2D
	mov cx, [point1X]
	sub cx, [point2X]
	absolute cx
	mov bx, [point1Y]
	sub bx, [point2Y]
	absolute bx
	cmp cx, bx
	jae DrawLine2Dp1 ; deltaX > deltaY
	mov ax, [point1X]
	mov bx, [point2X]
	mov cx, [point1Y]
	mov dx, [point2Y]
	cmp cx, dx
	jbe DrawLine2DpNxt1 ; point1Y <= point2Y
	xchg ax, bx
	xchg cx, dx
DrawLine2DpNxt1:
	mov [point1X], ax
	mov [point2X], bx
	mov [point1Y], cx
	mov [point2Y], dx
	DrawLine2DDY point1X, point1Y, point2X, point2Y
	ret
DrawLine2Dp1:
	mov ax, [point1X]
	mov bx, [point2X]
	mov cx, [point1Y]
	mov dx, [point2Y]
	cmp ax, bx
	jbe DrawLine2DpNxt2 ; point1X <= point2X
	xchg ax, bx
	xchg cx, dx
DrawLine2DpNxt2:
	mov [point1X], ax
	mov [point2X], bx
	mov [point1Y], cx
	mov [point2Y], dx
	DrawLine2DDX point1X, point1Y, point2X, point2Y
	ret
ENDP DrawLine2D
;-----------------------------------------------
; input: pointX pointY,
;           Color
; output: point on the screen
;-----------------------------------------------
PROC PIXEL
	mov bh,0h
	mov cx,[pointX]
	mov dx,[pointY]
	mov al,[Color]
	mov ah,0Ch
	int 10h
	ret
ENDP PIXEL
END start

```



## AutoHotkey



```AutoHotkey
Blue := Color(0,0,255)
White := Color(255,255,255)
Bitmap := Bitmap(100,100,Blue) ;create a 100*100 blue bitmap
Line(Bitmap,White,5,10,60,80) ;draw a white line from (5,10) to (60,80)
Bitmap.Write("Line.ppm") ;write the bitmap to file

Line(ByRef Bitmap,ByRef Color,PosX1,PosY1,PosX2,PosY2)
{
 DeltaX := Abs(PosX2 - PosX1), DeltaY := -Abs(PosY2 - PosY1) ;calculate deltas
 StepX := ((PosX1 < PosX2) ? 1 : -1), StepY := ((PosY1 < PosY2) ? 1 : -1) ;calculate steps
 ErrorValue := DeltaX + DeltaY ;calculate error value
 Loop ;loop over the pixel values
 {
  Bitmap[PosX1,PosX2] := Color
  If (PosX1 = PosX2 && PosY1 = PosY2)
   Break
  Temp1 := ErrorValue << 1, ((Temp1 > DeltaY) ? (ErrorValue += DeltaY, PosX1 += StepX) : ""), ((Temp1 < DeltaX) ? (ErrorValue += DeltaX, PosY1 += StepY) : "") ;move forward
 }
}
```



## AutoIt



```AutoHotkey
Local $var = drawBresenhamLine(2, 3, 2, 6)

Func drawBresenhamLine($iX0, $iY0, $iX1, $iY1)
	Local $iDx = Abs($iX1 - $iX0)
	Local $iSx = $iX0 < $iX1 ? 1 : -1
	Local $iDy = Abs($iY1 - $iY0)
	Local $iSy = $iY0 < $iY1 ? 1 : -1
	Local $iErr = ($iDx > $iDy ? $iDx : -$iDy) / 2, $e2

	While $iX0 <= $iX1
		ConsoleWrite("plot( $x=" & $iX0 & ", $y=" & $iY0 & " )" & @LF)
		If ($iX0 = $iX1) And ($iY0 = $iY1) Then Return
		$e2 = $iErr
		If ($e2 > -$iDx) Then
			$iErr -= $iDy
			$iX0 += $iSx
		EndIf
		If ($e2 < $iDy) Then
			$iErr += $iDx
			$iY0 += $iSy
		EndIf
	WEnd
EndFunc   ;==>drawBresenhamLine
```



## bash


```bash
#! /bin/bash

function line {
  x0=$1
  y0=$2
  x1=$3
  y1=$4

  if (( x0 > x1 ))
  then
    (( dx = x0 - x1 ))
    (( sx = -1 ))
  else
    (( dx = x1 - x0 ))
    (( sx = 1 ))
  fi

  if (( y0 > y1 ))
  then
    (( dy = y0 - y1 ))
    (( sy = -1 ))
  else
    (( dy = y1 - y0 ))
    (( sy = 1 ))
  fi

  if (( dx > dy ))
  then
    (( err = dx ))
  else
    (( err = -dy ))
  fi
  (( err /= 2 ))
  (( e2 = 0 ))

  while :
  do
    echo -en "\e[${y0};${x0}H#\e[K"
    (( x0 == x1 && y0 == y1 )) && return
    (( e2 = err ))
    if (( e2 > -dx ))
    then
      (( err -= dy ))
      ((  x0 += sx ))
    fi
    if (( e2 < dy ))
    then
      (( err += dx ))
      ((  y0 += sy ))
    fi
  done
}

# Draw a full screen diamond
COLS=$( tput cols )
LINS=$( tput lines )
LINS=$((LINS-1))
clear
line $((COLS/2)) 1 $((COLS/4)) $((LINS/2))
line $((COLS/4)) $((LINS/2)) $((COLS/2)) $LINS
line $((COLS/2)) $LINS $((COLS/4*3)) $((LINS/2))
line $((COLS/4*3)) $((LINS/2)) $((COLS/2)) 1
echo -e "\e[${LINS}H"
```



## BASIC


```qbasic
 1500 REM === Draw a line. Ported from C version
 1510 REM Inputs are X1, Y1, X2, Y2: Destroys value of X1, Y1
 1520 DX = ABS(X2 - X1):SX = -1:IF X1 < X2 THEN SX = 1
 1530 DY = ABS(Y2 - Y1):SY = -1:IF Y1 < Y2 THEN SY = 1
 1540 ER = -DY:IF DX > DY THEN ER = DX
 1550 ER = INT(ER / 2)
 1560 PLOT X1,Y1:REM This command may differ depending on BASIC dialect
 1570 IF X1 = X2 AND Y1 = Y2 THEN RETURN
 1580 E2 = ER
 1590 IF E2 > -DX THEN ER = ER - DY:X1 = X1 + SX
 1600 IF E2 < DY THEN ER = ER + DX:Y1 = Y1 + SY
 1610 GOTO 1560
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      Width% = 200
      Height% = 200

      REM Set window size:
      VDU 23,22,Width%;Height%;8,16,16,128

      REM Draw lines:
      PROCbresenham(50,100,100,190,0,0,0)
      PROCbresenham(100,190,150,100,0,0,0)
      PROCbresenham(150,100,100,10,0,0,0)
      PROCbresenham(100,10,50,100,0,0,0)
      END

      DEF PROCbresenham(x1%,y1%,x2%,y2%,r%,g%,b%)
      LOCAL dx%, dy%, sx%, sy%, e
      dx% = ABS(x2% - x1%) : sx% = SGN(x2% - x1%)
      dy% = ABS(y2% - y1%) : sy% = SGN(y2% - y1%)
      IF dx% > dy% e = dx% / 2 ELSE e = dy% / 2
      REPEAT
        PROCsetpixel(x1%,y1%,r%,g%,b%)
        IF x1% = x2% IF y1% = y2% EXIT REPEAT
        IF dx% > dy% THEN
          x1% += sx% : e -= dy% : IF e < 0 e += dx% : y1% += sy%
        ELSE
          y1% += sy% : e -= dx% : IF e < 0 e += dy% : x1% += sx%
        ENDIF
      UNTIL FALSE
      ENDPROC

      DEF PROCsetpixel(x%,y%,r%,g%,b%)
      COLOUR 1,r%,g%,b%
      GCOL 1
      LINE x%*2,y%*2,x%*2,y%*2
      ENDPROC
```

[[File:bresenham_bbc.gif]]


## Batch File


```dos
@echo off
setlocal enabledelayedexpansion

set width=87
set height=51

mode %width%,%height%

set "grid="
set /a resolution=height*width
for /l %%i in (1,1,%resolution%) do (
	set "grid=!grid! "
)
call :line 1 1 5 5
call :line 9 30 60 7
call :line 9 30 60 50
call :line 52 50 32 1
echo:%grid%
pause>nul
exit

:line
	set x1=%1
	set y1=%2
	set x2=%3
	set y2=%4

	set /a dx=x2-x1
	set /a dy=y2-y1

	::Clipping done to avoid overflow

	if %dx% neq 0 set /a o=y1 - ( x1 * dy / dx )
	if %x1% leq %x2% (
		if %x1% geq %width% goto :eof
		if %x2% lss 0 goto :eof

		if %x1% lss 0 (
			if %dx% neq 0 set y1=%o%
			set x1=0
		)
		if %x2% geq %width% (
			set /a x2= width - 1
			if %dx% neq 0 set /a "y2= x2 * dy / dx + o"
		)
	) else (
		if %x2% geq %width% goto :eof
		if %x1% lss 0 goto :eof

		if %x2% lss 0 (
			if %dx% neq 0 set y2=%o%
			set x2=0
		)
		if %x1% geq %width% (
			set /a x1=width - 1
			if %dx% neq 0 set /a "y1= x1 * dy / dx + o"
		)
	)
	if %y1% leq %y2% (
		if %y1% geq %height% goto :eof
		if %y2% lss 0 goto :eof

		if %y1% lss 0 (
			set y1=0
			if %dx% neq 0 set /a x1= - o * dx /dy
		)
		if %y2% geq %height% (
			set /a y2=height-1
			if %dx% neq 0 set /a "x2= (y2 - o) * dx /dy"
		)
	) else (
		if %y2% geq %height% goto :eof
		if %y1% lss 0 goto :eof

		if %y2% lss 0 (
			set y2=0
			if %dx% neq 0 set /a "x2= - o * dx /dy"
		)
		if %y1% geq %height% (
			set /a y1=height-1
			if %dx% neq 0 set /a "x1= (y1 - o) * dx /dy"
		)
	)

	:: Start of Bresenham's algorithm

	set stepy=1
	set stepx=1

	set /a dx=x2-x1
	set /a dy=y2-y1

	if %dy% lss 0 set /a "dy=-dy","stepy=-1"
	if %dx% lss 0 set /a "dx=-dx","stepx=-1"

	set /a "dy <<= 1"
	set /a "dx <<= 1"

	if %dx% gtr %dy% (
		set /a "fraction=dy-(dx>>1)"
		set /a "cursor=y1*width + x1"
		for /l %%x in (%x1%,%stepx%,%x2%) do (
			set /a cursorP=cursor+1
			for /f "tokens=1-2" %%g in ("!cursor! !cursorP!") do set "grid=!grid:~0,%%g!Û!grid:~%%h!"
			if !fraction! geq 0 (
				set /a y1+=stepy
				set /a cursor+=stepy*width
				set /a fraction-=dx
			)
			set /a fraction+=dy
			set /a cursor+=stepx
		)
	) else (
		set /a "fraction=dx-(dy>>1)"
		set /a "cursor=y1*width + x1"
		for /l %%y in (%y1%,%stepy%,%y2%) do (
			set /a cursorP=cursor+1
			for /f "tokens=1-2" %%g in ("!cursor! !cursorP!") do set "grid=!grid:~0,%%g!Û!grid:~%%h!"
			if !fraction! geq 0 (
				set /a x1+=stepx
				set /a cursor+=stepx
				set /a fraction-=dy
			)
			set /a fraction+=dx
			set /a cursor+=width*stepy
		)
	)
	goto :eof
```



## C


Instead of swaps in the initialisation use error calculation for both directions x and y simultaneously:

```C
void line(int x0, int y0, int x1, int y1) {

  int dx = abs(x1-x0), sx = x0<x1 ? 1 : -1;
  int dy = abs(y1-y0), sy = y0<y1 ? 1 : -1;
  int err = (dx>dy ? dx : -dy)/2, e2;

  for(;;){
    setPixel(x0,y0);
    if (x0==x1 && y0==y1) break;
    e2 = err;
    if (e2 >-dx) { err -= dy; x0 += sx; }
    if (e2 < dy) { err += dx; y0 += sy; }
  }
}
```


=={{header|C sharp|C#}}==
Port of the C version.

```csharp
using System;
using System.Drawing;
using System.Drawing.Imaging;
static class Program
{
    static void Main()
    {
        new Bitmap(200, 200)
            .DrawLine(0, 0, 199, 199, Color.Black).DrawLine(199,0,0,199,Color.Black)
            .DrawLine(50, 75, 150, 125, Color.Blue).DrawLine(150, 75, 50, 125, Color.Blue)
            .Save("line.png", ImageFormat.Png);
    }
    static Bitmap DrawLine(this Bitmap bitmap, int x0, int y0, int x1, int y1, Color color)
    {
        int dx = Math.Abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
        int dy = Math.Abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
        int err = (dx > dy ? dx : -dy) / 2, e2;
        for(;;) {
            bitmap.SetPixel(x0, y0, color);
            if (x0 == x1 && y0 == y1) break;
            e2 = err;
            if (e2 > -dx) { err -= dy; x0 += sx; }
            if (e2 < dy) { err += dx; y0 += sy; }
        }
        return bitmap;
    }
}
```



## C++


```cpp

void Line( const float x1, const float y1, const float x2, const float y2, const Color& color )
{
        // Bresenham's line algorithm
  const bool steep = (fabs(y2 - y1) > fabs(x2 - x1));
  if(steep)
  {
    std::swap(x1, y1);
    std::swap(x2, y2);
  }

  if(x1 > x2)
  {
    std::swap(x1, x2);
    std::swap(y1, y2);
  }

  const float dx = x2 - x1;
  const float dy = fabs(y2 - y1);

  float error = dx / 2.0f;
  const int ystep = (y1 < y2) ? 1 : -1;
  int y = (int)y1;

  const int maxX = (int)x2;

  for(int x=(int)x1; x<maxX; x++)
  {
    if(steep)
    {
        SetPixel(y,x, color);
    }
    else
    {
        SetPixel(x,y, color);
    }

    error -= dy;
    if(error < 0)
    {
        y += ystep;
        error += dx;
    }
  }
}

```



## Clojure



```clojure


(defn draw-line
  "Draw a line from x1,y1 to x2,y2 using Bresenham's, to a java BufferedImage in the colour of pixel."
  [buffer x1 y1 x2 y2 pixel]
  (let [dist-x (Math/abs (- x1 x2))
   dist-y (Math/abs (- y1 y2))
   steep (> dist-y dist-x)]
    (let [[x1 y1 x2 y2] (if steep [y1 x1 y2 x2] [x1 y1 x2 y2])]
      (let [[x1 y1 x2 y2] (if (> x1 x2) [x2 y2 x1 y1] [x1 y1 x2 y2])]
  (let  [delta-x (- x2 x1)
     delta-y (Math/abs (- y1 y2))
     y-step (if (< y1 y2) 1 -1)]

    (let [plot (if steep
       #(.setRGB buffer (int %1) (int %2) pixel)
       #(.setRGB buffer (int %2) (int %1) pixel))]

      (loop [x x1 y y1 error (Math/floor (/ delta-x 2)) ]
        (plot x y)
        (if (< x x2)
    ; Rather then rebind error, test that it is less than delta-y rather than zero
    (if (< error delta-y)
      (recur (inc x) (+ y y-step) (+ error (- delta-x delta-y)))
      (recur (inc x) y            (- error delta-y)))))))))))

```



## CoffeeScript



```coffeescript

drawBresenhamLine = (x0, y0, x1, y1) ->
  dx = Math.abs(x1 - x0)
  sx = if x0 < x1 then 1 else -1
  dy = Math.abs(y1 - y0)
  sy = if y0 < y1 then 1 else -1
  err = (if dx>dy then dx else -dy) / 2

  loop
    setPixel(x0, y0)
    break if x0 == x1 && y0 == y1
    e2 = err
    if e2 > -dx
      err -= dy
      x0 += sx
    if e2 < dy
      err += dx
      y0 += sy
  null

```



## Common Lisp



```lisp
(defun draw-line (buffer x1 y1 x2 y2 pixel)
  (declare (type rgb-pixel-buffer buffer))
  (declare (type integer x1 y1 x2 y2))
  (declare (type rgb-pixel pixel))
  (let* ((dist-x (abs (- x1 x2)))
         (dist-y (abs (- y1 y2)))
         (steep (> dist-y dist-x)))
    (when steep
      (psetf x1 y1 y1 x1
             x2 y2 y2 x2))
    (when (> x1 x2)
      (psetf x1 x2 x2 x1
             y1 y2 y2 y1))
    (let* ((delta-x (- x2 x1))
           (delta-y (abs (- y1 y2)))
           (error (floor delta-x 2))
           (y-step (if (< y1 y2) 1 -1))
           (y y1))
      (loop
        :for x :upfrom x1 :to x2
        :do (if steep
                (setf (rgb-pixel buffer x y) pixel)
                (setf (rgb-pixel buffer y x) pixel))
            (setf error (- error delta-y))
            (when (< error 0)
              (incf y y-step)
              (incf error delta-x))))
    buffer))
```



## D

This code uses the Image defined in [[Bitmap]] Task.


```d
module bitmap_bresenhams_line_algorithm;

import std.algorithm, std.math, bitmap;

void drawLine(Color)(Image!Color img,
                        size_t x1,    size_t y1,
                     in size_t x2, in size_t y2,
                     in Color color)
pure nothrow @nogc {
    immutable int dx = x2 - x1;
    immutable int ix = (dx > 0) - (dx < 0);
    immutable size_t dx2 = abs(dx) * 2;
    int dy = y2 - y1;
    immutable int iy = (dy > 0) - (dy < 0);
    immutable size_t dy2 = abs(dy) * 2;
    img[x1, y1] = color;

    if (dx2 >= dy2) {
        int error = dy2 - (dx2 / 2);
        while (x1 != x2) {
            if (error >= 0 && (error || (ix > 0))) {
                error -= dx2;
                y1 += iy;
            }

            error += dy2;
            x1 += ix;
            img[x1, y1] = color;
        }
    } else {
        int error = dx2 - (dy2 / 2);
        while (y1 != y2) {
            if (error >= 0 && (error || (iy > 0))) {
                error -= dy2;
                x1 += ix;
            }

            error += dx2;
            y1 += iy;
            img[x1, y1] = color;
        }
    }
}

version (bitmap_bresenhams_line_algorithm_main) {
    void main() {
        auto img = new Image!RGB(25, 22);
        img.drawLine(5, 5, 15, 20, RGB.white);
        img.drawLine(3, 20, 10, 12, RGB.white);
        img.textualShow();
    }
}
```

To run the demo code compile with <code>-version=bitmap_bresenhams_line_algorithm_main</code>.
{{out}}

```txt
#########################
#########################
#########################
#########################
#########################
#####.###################
######.##################
######.##################
#######.#################
########.################
########.################
#########.###############
##########.##############
#########..##############
########.##.#############
#######.####.############
######.#####.############
######.######.###########
#####.########.##########
####.#########.##########
###.###########.#########
#########################
```



## Delphi



```delphi

procedure drawLine (bitmap : TBitmap; xStart, yStart, xEnd, yEnd : integer; color : TAlphaColor);
// Bresenham's Line Algorithm.  Byte, March 1988, pp. 249-253.
// Modified from http://www.efg2.com/Lab/Library/Delphi/Graphics/Bresenham.txt and tested.
var
      a, b       :  integer;  // displacements in x and y
      d          :  integer;  // decision variable
      diag_inc   :  integer;  // d's increment for diagonal steps
      dx_diag    :  integer;  // diagonal x step for next pixel
      dx_nondiag :  integer;  // nondiagonal x step for next pixel
      dy_diag    :  integer;  // diagonal y step for next pixel
      dy_nondiag :  integer;  // nondiagonal y step for next pixel
      i          :  integer;  // loop index
      nondiag_inc:  integer;  // d's increment for nondiagonal steps
      swap       :  integer;  // temporary variable for swap
      x,y        :  integer;  // current x and y coordinates
begin
    x := xStart;              // line starting point}
    y := yStart;
    // Determine drawing direction and step to the next pixel.
    a := xEnd - xStart;       // difference in x dimension
    b := yEnd - yStart;       // difference in y dimension
    // Determine whether end point lies to right or left of start point.
    if a < 0 then             // drawing towards smaller x values?
       begin
       a := -a;               // make 'a' positive
       dx_diag := -1
       end
    else
       dx_diag := 1;

    // Determine whether end point lies above or below start point.
    if b < 0 then             // drawing towards smaller x values?
       begin
       b := -b;               // make 'a' positive
       dy_diag := -1
       end
    else
       dy_diag := 1;
    // Identify octant containing end point.
    if a < b then
       begin
       swap := a;
       a := b;
       b := swap;
       dx_nondiag := 0;
       dy_nondiag := dy_diag
       end
    else
       begin
       dx_nondiag := dx_diag;
       dy_nondiag := 0
       end;
    d := b + b - a;           // initial value for d is 2*b - a
    nondiag_inc := b + b;     // set initial d increment values
    diag_inc    := b + b - a - a;
    for i := 0 to a do
        begin   /// draw the a+1 pixels
        drawPixel (bitmap, x, y, color);
        if d < 0 then            // is midpoint above the line?
           begin                 // step nondiagonally
           x := x + dx_nondiag;
           y := y + dy_nondiag;
           d := d + nondiag_inc  // update decision variable
           end
        else
           begin                 // midpoint is above the line; step diagonally}
           x := x + dx_diag;
           y := y + dy_diag;
           d := d + diag_inc
           end;
    end;
end;

```



## Elm



```elm



-- Brensenham Line Algorithm

type alias Position =
  {x: Int, y: Int}

type alias BresenhamStatics =
  { finish : Position
  , sx : Int
  , sy : Int
  , dx : Float
  , dy : Float
  }


line : Position -> Position -> List Position
line p q =
  let
    dx = (toFloat << abs) (q.x - p.x)
    dy = (toFloat << abs) (q.y - p.y)

    sx = if p.x < q.x then 1 else -1
    sy = if p.y < q.y then 1 else -1

    error =
      (if dx > dy then dx else -dy) / 2

    statics =
      BresenhamStatics q sx sy dx dy
  in
  bresenhamLineLoop statics error p []


bresenhamLineLoop : BresenhamStatics -> Float -> Position -> List Position -> List Position
bresenhamLineLoop statics error p positions =
  let
    positions_ = p :: positions
    {sx, sy, dx, dy, finish} = statics
  in
  if (p.x == finish.x) && (p.y == finish.y) then
    positions_
  else
    let
      (dErrX, x) =
        if error > -dx then (-dy, sx + p.x)
        else (0, p.x)

      (dErrY, y) =
        if error < dy then (dx, sy + p.y)
        else (0, p.y)

      error_ = error + dErrX + dErrY
    in
      bresenhamLineLoop statics error_ (Position x y) positions_


```



## E


{{trans|C}}


```e
def swap(&left, &right) { # From [[Generic swap]]
  def t := left
  left := right
  right := t
}

def drawLine(image, var x0, var y0, var x1, var y1, color) {
    def steep := (y1 - y0).abs() > (x1 - x0).abs()
    if (steep) {
        swap(&x0, &y0)
        swap(&x1, &y1)
    }
    if (x0 > x1) {
        swap(&x0, &x1)
        swap(&y0, &y1)
    }
    def deltax := x1 - x0
    def deltay := (y1 - y0).abs()
    def ystep := if (y0 < y1) { 1 } else { -1 }
    var error := deltax // 2
    var y := y0
    for x in x0..x1 {
        if (steep) { image[y, x] := color } else { image[x, y] := color }
        error -= deltay
        if (error < 0) {
            y += ystep
            error += deltax
        }
    }
}
```



```e
def i := makeImage(5, 20)
drawLine(i, 1, 1, 3, 18, makeColor.fromFloat(0,1,1))
i.writePPM(<import:java.io.makeFileOutputStream>(<file:~/Desktop/Bresenham.ppm>))
```



## Erlang


```erlang

build_path({Sx, Sy}, {Tx, Ty}) ->
  if
    Tx < Sx -> StepX = -1;
    true -> StepX = 1
  end,
  if
    Ty < Sy -> StepY = -1;
    true -> StepY = 1
  end,

  Dx = abs((Tx-Sx)*2),
  Dy = abs((Ty-Sy)*2),

  if
    Dy > Dx -> Path = through_y({Sx, Sy}, {Tx, Ty}, {StepX, StepY}, {Dx, Dy}, Dx*2-Dy, []);
    true -> Path = through_x({Sx, Sy}, {Tx, Ty}, {StepX, StepY}, {Dx, Dy}, Dy*2-Dx, [])
  end,

  lists:reverse(Path).

through_x({Tx, _}, {Tx, _}, _, _, _, P) -> P;
through_x({Sx, Sy}, {Tx, Ty}, {StepX, StepY}, {Dx, Dy}, F0, P) when F0 >= 0 ->
  Ny = Sy + StepY,
  F1 = F0 - Dx,
  Nx = Sx + StepX,
        F2 = F1 + Dy,
  through_x({Nx, Ny}, {Tx, Ty}, {StepX, StepY}, {Dx, Dy}, F2, [{Nx, Ny}|P]);
through_x({Sx, Sy}, {Tx, Ty}, {StepX, StepY}, {Dx, Dy}, F0, P) when F0 < 0 ->
  Ny = Sy,
  Nx = Sx + StepX,
        F2 = F0 + Dy,
  through_x({Nx, Ny}, {Tx, Ty}, {StepX, StepY}, {Dx, Dy}, F2, [{Nx, Ny}|P]).

through_y({_, Ty}, {_, Ty}, _, _, _, P) -> P;
through_y({Sx, Sy}, {Tx, Ty}, {StepX, StepY}, {Dx, Dy}, F0, P) when F0 >= 0 ->
  Nx = Sx + StepX,
  F1 = F0 - Dy,
  Ny = Sy + StepY,
        F2 = F1 + Dx,
  through_y({Nx, Ny}, {Tx, Ty}, {StepX, StepY}, {Dx, Dy}, F2, [{Nx, Ny}|P]);
through_y({Sx, Sy}, {Tx, Ty}, {StepX, StepY}, {Dx, Dy}, F0, P) when F0 < 0 ->
  Nx = Sx,
  Ny = Sy + StepY,
        F2 = F0 + Dx,
  through_y({Nx, Ny}, {Tx, Ty}, {StepX, StepY}, {Dx, Dy}, F2, [{Nx, Ny}|P]).

```

OR

```erlang

line({X0, Y0}, {X1, Y1}) ->
  SX = step(X0, X1),
  SY = step(Y0, Y1),
  DX = abs(X1 - X0),
  DY = abs(Y1 - Y0),
  Err = DX - DY,
  line({X0, Y0}, {X1, Y1}, {SX, SY}, {DX, DY}, Err, []).

line({X1, Y1}, {X1, Y1}, _, _, _, Acc) ->
  lists:reverse([{X1, Y1} | Acc]);
line({X, Y}, {X1, Y1}, {SX, SY}, {DX, DY}, Err, Acc) ->
  DE = 2 * Err,
  {X0, Err0} = next_x(X, SX, DY, Err, DE),
  {Y0, Err1} = next_y(Y, SY, DX, Err0, DE),
  line({X0, Y0}, {X1, Y1}, {SX, SY}, {DX, DY}, Err1, [{X, Y} | Acc]).

step(P0, P1) when P0 < P1 ->
  1;
step(_, _) ->
  -1.

next_x(X, SX, DY, E, DE) when DE > -DY ->
  {X + SX, E - DY};
next_x(X, _SX, _DY, E, _DE) ->
  {X, E}.

next_y(Y, SY, DX, E, DE) when DE < DX ->
  {Y + SY, E + DX};
next_y(Y, _SY, _DX, E, _DE) ->
  {Y, E}.

```




## ERRE


```ERRE

PROGRAM BRESENHAM

!$INCLUDE="PC.LIB"

PROCEDURE BRESENHAM
! === Draw a line using graphic coordinates
! Inputs are X1, Y1, X2, Y2: Destroys value of X1, Y1
dx=ABS(x2-x1) sx=-1
IF x1<x2 THEN sx=1
dy=ABS(y2-y1) sy=-1
IF y1<y2 THEN sy=1
er=-dy
IF dx>dy THEN er=dx
er=INT(er/2)
LOOP
   PSET(x1,y1,1)
   EXIT IF x1=x2 AND y1=y2
   e2=er
   IF e2>-dx THEN er=er-dy x1=x1+sx
   IF e2<dy THEN er=er+dx y1=y1+sy
END LOOP
END PROCEDURE

BEGIN
  SCREEN(2)
  INPUT(x1,y1,x2,y2)
  BRESENHAM
  GET(A$)
  SCREEN(0)
END PROGRAM

```



## Euphoria


{{trans|C}}


```euphoria
include std/console.e
include std/graphics.e
include std/math.e

-- the new_image function and related code in the 25 or so
-- lines below are from http://rosettacode.org/wiki/Basic_bitmap_storage#Euphoria
-- as of friday, march 2, 2012

-- Some color constants:
constant
    black = #000000,
    white = #FFFFFF,
    red =   #FF0000,
    green = #00FF00,
    blue =  #0000FF

-- Create new image filled with some color
function new_image(integer width, integer height, atom fill_color)
    return repeat(repeat(fill_color,height),width)
end function

--grid used for drawing lines in this program
sequence screenData = new_image(16,16,black)

--the line algorithm
function bresLine(sequence screenData, integer x0, integer y0, integer x1, integer y1, integer color)

    integer deltaX = abs(x1 - x0), deltaY = abs(y1 - y0)
    integer stepX, stepY, lineError, error2

    if x0 < x1 then
        stepX = 1
        else
        stepX = -1
    end if

    if y0 < y1 then
        stepY = 1
        else
        stepY = -1
    end if

    if deltaX > deltaY then
        lineError = deltaX
        else
        lineError = -deltaY
    end if

    lineError = round(lineError / 2, 1)

    while 1 do

        screenData[x0][y0] = color

        if (x0 = x1 and y0 = y1) then
            exit
        end if

        error2 = lineError

        if error2 > -deltaX then
            lineError -= deltaY
            x0 += stepX
        end if
        if error2 < deltaY then
            lineError += deltaX
            y0 += stepY
        end if
    end while
    return screenData -- return modified version of the screenData sequence
end function

--prevents console output wrapping to next line if it is too big for the screen
wrap(0)
--outer diamond
screenData = bresLine(screenData,8,1,16,8,white)
screenData = bresLine(screenData,16,8,8,16,white)
screenData = bresLine(screenData,8,16,1,8,white)
screenData = bresLine(screenData,1,8,8,1,white)
--inner diamond
screenData = bresLine(screenData,8,4,12,8,white)
screenData = bresLine(screenData,12,8,8,12,white)
screenData = bresLine(screenData,8,12,4,8,white)
screenData = bresLine(screenData,4,8,8,4,white)
-- center lines drawing from left to right, and the next from right to left.
screenData = bresLine(screenData,7,7,9,7,white)
screenData = bresLine(screenData,9,9,7,9,white)
--center dot
screenData = bresLine(screenData,8,8,8,8,white)

--print to the standard console output
for i = 1 to 16 do
    puts(1,"\n")
    for j = 1 to 16 do
            if screenData[j][i] = black then
                printf(1, "%s", ".")
            else
                printf(1, "%s", "#")
            end if
    end for
end for

puts(1,"\n\n")
any_key()

--/*
--output was edited to replace the color's hex digits for clearer output graphics.
--to output all the hex digits, use printf(1,"%06x", screenData[j][i])
--to output 'shortened' hex digits, use :
--printf(1, "%x", ( abs( ( (screenData[j][i] / #FFFFF) - 1 ) ) - 1 ) )
--and
--printf(1,"%x", abs( ( (screenData[j][i] / #FFFFF) - 1 ) ) )
--
--,respectively in the last if check.
--*/
```

Output:

```txt

.......#........
......#.#.......
.....#...#......
....#..#..##....
...#..#.#...#...
..#..#...#...#..
.#..#.###.#...#.
#..#...#...#...#
.#..#.###.#...#.
..#..#...#...#..
...#..#.#...#...
....#..#...#....
....#.....#.....
.....#...#......
......#.#.......
.......#........

Press Any Key to continue...

```


=={{header|F Sharp|F#}}==

```fsharp
let inline bresenham fill (x0, y0) (x1, y1) =
  let steep = abs(y1 - y0) > abs(x1 - x0)
  let x0, y0, x1, y1 =
    if steep then y0, x0, y1, x1 else x0, y0, x1, y1
  let x0, y0, x1, y1 =
    if x0 > x1 then x1, y1, x0, y0 else x0, y0, x1, y1
  let dx, dy = x1 - x0, abs(y1 - y0)
  let s = if y0 < y1 then 1 else -1
  let rec loop e x y =
    if x <= x1 then
      if steep then fill y x else fill x y
      if e < dy then
        loop (e-dy+dx) (x+1) (y+s)
      else
        loop (e-dy) (x+1) y
  loop (dx/2) x0 y0
```

The following program tests the above bresenham function by drawing 100 lines into an image and visualizing the result using
{{libheader|Windows Presentation Foundation}}:

```fsharp
open System.Windows
open System.Windows.Media.Imaging

[<System.STAThread>]
do
  let rand = System.Random()
  let n = 256
  let pixel = Array.create (n*n) 0uy
  let rand = System.Random().Next
  for _ in 1..100 do
    bresenham (fun x y -> pixel.[x+y*n] <- 255uy) (rand n, rand n) (rand n, rand n)
  let image = Controls.Image(Stretch=Media.Stretch.Uniform)
  let format = Media.PixelFormats.Gray8
  image.Source <-
    BitmapSource.Create(n, n, 1.0, 1.0, format, null, pixel, n)
  Window(Content=image, Title="Bresenham's line algorithm")
  |> (Application()).Run |> ignore
```



## FBSL

1. In FBSL, successive calls to one and the same subprocedure may be concatenated to a series of argument sets as in Sub Rhombus() below.

2. In FBSL, BASIC-style logical AND and OR operators are "inclusive", i.e. they always evaluate the both of their conditions. C-style logical ANDALSO and ORELSE operators are "exclusive". ANDALSO evaluates the second condition if, and only if, its first condition is TRUE as in Sub Bresenham() below. ORELSE evaluates its second condition if, and only if, its first condition is FALSE.

'''Using pure FBSL's built-in graphics functions:'''

```qbasic
#DEFINE WM_LBUTTONDOWN 513
#DEFINE WM_CLOSE 16

FBSLSETTEXT(ME, "Bresenham") ' Set form caption
FBSLSETFORMCOLOR(ME, RGB(0, 255, 255)) ' Cyan: set persistent background color
DRAWWIDTH(5) ' Adjust point size
FBSL.GETDC(ME) ' Use volatile FBSL.GETDC below to avoid extra assignments

RESIZE(ME, 0, 0, 200, 235)
CENTER(ME)
SHOW(ME)

BEGIN EVENTS
  SELECT CASE CBMSG
    CASE WM_LBUTTONDOWN: Rhombus() ' Draw
    CASE WM_CLOSE: FBSL.RELEASEDC(ME, FBSL.GETDC) ' Clean up
  END SELECT
END EVENTS

SUB Rhombus()
  Bresenham(50, 100, 100, 190)(100, 190, 150, 100)(150, 100, 100, 10)(100, 10, 50, 100)

  SUB Bresenham(x0, y0, x1, y1)
    DIM dx = ABS(x0 - x1), sx = SGN(x0 - x1)
    DIM dy = ABS(y0 - y1), sy = SGN(y0 - y1)
    DIM tmp, er = IIF(dx > dy, dx, -dy) / 2

    WHILE NOT (x0 = x1 ANDALSO y0 = y1)
      PSET(FBSL.GETDC, x0, y0, &HFF) ' Red: Windows stores colors in BGR order
      tmp = er
      IF tmp > -dx THEN: er = er - dy: x0 = x0 + sx: END IF
      IF tmp < +dy THEN: er = er + dx: y0 = y0 + sy: END IF
    WEND
  END SUB
END SUB
```

'''Output:'''   [[File:FBSLBresenham.PNG]]


## Factor

A very ugly imperative implementation similar to the wikipedia pseudocode..

```factor
USING: accessors arrays kernel locals math math.functions
math.ranges math.vectors rosettacode.raster.display
rosettacode.raster.storage sequences ui.gadgets ;
IN: rosettacode.raster.line

:: line-points ( pt1 pt2 -- points )
    pt1 first2 :> y0! :> x0!
    pt2 first2 :> y1! :> x1!
    y1 y0 - abs x1 x0 - abs > :> steep
    steep [
        y0 x0 y0! x0!
        y1 x1 y1! x1!
    ] when
    x0 x1 > [
        x0 x1 x0! x1!
        y0 y1 y0! y1!
    ] when
    x1 x0 - :> deltax
    y1 y0 - abs :> deltay
    0 :> current-error!
    deltay deltax / abs :> deltaerr
    0 :> ystep!
    y0 :> y!
    y0 y1 < [ 1 ystep! ] [ -1 ystep! ] if
    x0 x1 1 <range> [
        y steep [ swap ] when 2array
        current-error deltaerr + current-error!
        current-error 0.5 >= [
            ystep y + y!
            current-error 1 - current-error!
        ] when
    ] { } map-as ;

! Needs rosettacode.raster.storage for the set-pixel function and to create the image
: draw-line ( {R,G,B} pt1 pt2 image -- )
    [ line-points ] dip
    [ set-pixel ] curry with each ;
```



## Forth


```forth
defer steep         \ noop or swap
defer ystep         \ 1+ or 1-

: line ( x0 y0 x1 y1 color bmp -- )
  { color bmp }
  rot swap
  ( x0 x1 y0 y1 )
  2dup  - abs >r
  2over - abs r> <
  if         ['] swap \ swap use of x and y
  else 2swap ['] noop
  then       is steep
  ( y0 y1 x0 x1 )
  2dup >
  if swap 2swap swap  \ ensure x1 > x0
  else    2swap
  then
  ( x0 x1 y0 y1 )
  2dup >
  if   ['] 1-
  else ['] 1+
  then is ystep
  over - abs    { y deltay }
  swap 2dup - dup { deltax }
  2/ rot 1+ rot
  ( error x1+1 x0 )
  do  color i y steep bmp b!
      deltay -
      dup 0<
      if   y ystep to y
           deltax +
      then
  loop
  drop ;

5 5 bitmap value test
0 test bfill
1 0 4 1 red test line
4 1 3 4 red test line
3 4 0 3 red test line
0 3 1 0 red test line
test bshow cr
 **
 * **
*   *
** *
  **
ok
```



## Fortran

{{works with|Fortran|90 and later}}
{{trans|C}}

```fortran
module RCImagePrimitive
  use RCImageBasic

  implicit none

  type point
     integer :: x, y
  end type point

  private :: swapcoord

contains

  subroutine swapcoord(p1, p2)
    integer, intent(inout) :: p1, p2
    integer :: t

    t = p2
    p2 = p1
    p1 = t
  end subroutine swapcoord

  subroutine draw_line(img, from, to, color)
    type(rgbimage), intent(inout) :: img
    type(point), intent(in) :: from, to
    type(rgb), intent(in) :: color

    type(point) :: rfrom, rto
    integer :: dx, dy, error, ystep, x, y
    logical :: steep

    rfrom = from
    rto = to
    steep = (abs(rto%y - rfrom%y) > abs(rto%x - rfrom%x))
    if ( steep ) then
       call swapcoord(rfrom%x, rfrom%y)
       call swapcoord(rto%x, rto%y)
    end if
    if ( rfrom%x > rto%x ) then
       call swapcoord(rfrom%x, rto%x)
       call swapcoord(rfrom%y, rto%y)
    end if

    dx = rto%x - rfrom%x
    dy = abs(rto%y - rfrom%y)
    error = dx / 2
    y = rfrom%y

    if ( rfrom%y < rto%y ) then
       ystep = 1
    else
       ystep = -1
    end if

    do x = rfrom%x, rto%x
       if ( steep ) then
          call put_pixel(img, y, x, color)
       else
          call put_pixel(img, x, y, color)
       end if
       error = error - dy
       if ( error < 0 ) then
          y = y + ystep
          error = error + dx
       end if
    end do

  end subroutine draw_line

end module RCImagePrimitive
```


Usage example:


```fortran
program BasicImageTests
  use RCImageBasic
  use RCImageIO
  use RCImagePrimitive

  implicit none

  type(rgbimage) :: animage
  integer :: x, y

  call alloc_img(animage, 200, 200)
  call fill_img(animage, rgb(255,255,255))

  call draw_line(animage, point(0,0), point(199,199), rgb(0,0,0))

  do y=0,219,20
     call draw_line(animage, point(0,0), point(199, y), &
                    rgb(0,0,0))
  end do

  open(unit=10, file='outputimage.ppm', status='new')
  call output_ppm(10, animage)
  close(10)

  call free_img(animage)

end program BasicImageTests
```


## FreeBASIC


```freebasic
' version 16-09-2015
' compile with: fbc -s console
' OR compile with: fbc -s gui

' Ported from the C version
Sub Br_line(x0 As Integer, y0 As Integer, x1 As Integer, y1 As Integer, Col As Integer = &HFFFFFF)

    Dim As Integer dx = Abs(x1 - x0), dy = Abs(y1 - y0)
    Dim As Integer sx = IIf(x0 < x1, 1, -1)
    Dim As Integer sy = IIf(y0 < y1, 1, -1)
    Dim As Integer er = IIf(dx > dy, dx, -dy) \ 2, e2

    Do
        PSet(x0, y0), col
        If (x0 = x1) And (y0 = y1) Then Exit Do
        e2 = er
        If e2 > -dx Then Er -= dy : x0 += sx
        If e2 <  dy Then Er += dx : y0 += sy
    Loop

End Sub

' ------=< MAIN >=------

Dim As Double x0, y0, x1, y1

ScreenRes 400, 400, 32
WindowTitle" Press key to end demo"
Randomize Timer

Do
    Cls
    For a As Integer = 1 To 20
        Br_line(Rnd*380+10, Rnd*380+10, Rnd*380+10, Rnd*380+10, Rnd*&hFFFFFF)
    Next
    Sleep 2000
Loop Until InKey <> "" ' loop until a key is pressed

End
```



## Go


```go
package raster

// Line draws line by Bresenham's algorithm.
func (b *Bitmap) Line(x0, y0, x1, y1 int, p Pixel) {
    // implemented straight from WP pseudocode
    dx := x1 - x0
    if dx < 0 {
        dx = -dx
    }
    dy := y1 - y0
    if dy < 0 {
        dy = -dy
    }
    var sx, sy int
    if x0 < x1 {
        sx = 1
    } else {
        sx = -1
    }
    if y0 < y1 {
        sy = 1
    } else {
        sy = -1
    }
    err := dx - dy

    for {
        b.SetPx(x0, y0, p)
        if x0 == x1 && y0 == y1 {
            break
        }
        e2 := 2 * err
        if e2 > -dy {
            err -= dy
            x0 += sx
        }
        if e2 < dx {
            err += dx
            y0 += sy
        }
    }
}

func (b *Bitmap) LineRgb(x0, y0, x1, y1 int, c Rgb) {
    b.Line(x0, y0, x1, y1, c.Pixel())
}
```

A demonstration program:

```go
package main

// Files required to build supporting package raster are found in:
// * This task (immediately above)
// * Bitmap
// * Write a PPM file

import (
    "raster"
    "fmt"
)

func main() {
    b := raster.NewBitmap(400, 300)
    b.FillRgb(0xdfefff)
    blue := raster.Rgb(0x8fcfff)
    b.LineRgb(7, 12, 307, 122, blue)
    b.LineRgb(177, 12, 127, 222, blue)
    err := b.WritePpmFile("bresenham.ppm")
    if err != nil {
        fmt.Println(err)
    }
}
```



## Haskell


```haskell
module Bitmap.Line(line) where

import Bitmap
import Control.Monad
import Control.Monad.ST
import qualified Data.STRef

var = Data.STRef.newSTRef
get = Data.STRef.readSTRef
mutate = Data.STRef.modifySTRef

line :: Color c => Image s c -> Pixel -> Pixel -> c -> ST s ()
line i (Pixel (xa, ya)) (Pixel (xb, yb)) c = do
    yV <- var y1
    errorV <- var $ deltax `div` 2
    forM_ [x1 .. x2] (\x -> do
        y <- get yV
        setPix i (Pixel $ if steep then (y, x) else (x, y)) c
        mutate errorV $ subtract deltay
        error <- get errorV
        when (error < 0) (do
            mutate yV (+ ystep)
            mutate errorV (+ deltax)))
  where steep = abs (yb - ya) > abs (xb - xa)
        (xa', ya', xb', yb') = if steep
          then (ya, xa, yb, xb)
          else (xa, ya, xb, yb)
        (x1, y1, x2, y2) = if xa' > xb'
          then (xb', yb', xa', ya')
          else (xa', ya', xb', yb')
        deltax = x2 - x1
        deltay = abs $ y2 - y1
        ystep = if y1 < y2 then 1 else -1
```



## J

'''Solution:'''

Using definitions from [[Basic bitmap storage#J|Basic bitmap storage]].

```j
thru=: <./ + -~ i.@+ _1 ^ >        NB. integers from x through y

NB.*getBresenhamLine v Returns points for a line given start and end points
NB. y is: y0 x0 ,: y1 x1
getBresenhamLine=: monad define
  steep=. ([: </ |@-~/) y
  points=. |."1^:steep y
  slope=. %~/ -~/ points
  ypts=. thru/ {."1 points
  xpts=. ({: + 0.5 <.@:+ slope * ypts - {.) {.points
  |."1^:steep ypts,.xpts
)

NB.*drawLines v Draws lines (x) on image (y)
NB. x is: 2-item list (start and end points) ; (color)
drawLines=: (1&{:: ;~ [: ; [: <@getBresenhamLine"2 (0&{::))@[ setPixels ]
```


'''Example Usage:'''

```j
   myimg=: 0 255 0 makeRGB 20 32                       NB. 32 by 20 green image
   myimg=: ((1 1 ,: 5 11) ; 255 0 0 ) drawLines myimg  NB. draw red line from xy point 1 1 to 11 5

NB. Works for lists of 2 by 2 arrays each defining a line's start and end point.
   Diamond=: _2]\ _2]\ 9 5 5 15 , 5 15 9 25 , 9 25 13 15 , 13 15 9 5
   Square =: _2]\ _2]\ 5 5 5 25 , 5 25 13 25 , 13 25 13 5 , 13 5 5 5
   viewRGB myimg=: (Diamond;255 0 0) drawLines myimg   NB. draw 4 red lines to form a diamond
   viewRGB myimg=: (Square;0 0 255) drawLines myimg    NB. draw 4 blue lines to form a square
   viewRGB (Diamond;255 0 0) drawLines (Square;0 0 255) drawLines myimg
```



## Java


```java
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

public class Bresenham {

    public static void main(String[] args) {
        SwingUtilities.invokeLater(Bresenham::run);
    }

    private static void run() {
        JFrame f = new JFrame();
        f.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        f.setTitle("Bresenham");

        f.getContentPane().add(new BresenhamPanel());
        f.pack();

        f.setLocationRelativeTo(null);
        f.setVisible(true);
    }
}

class BresenhamPanel extends JPanel {

    private final int pixelSize = 10;

    BresenhamPanel() {
        setPreferredSize(new Dimension(600, 500));
        setBackground(Color.WHITE);
    }

    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);

        int w = (getWidth() - 1) / pixelSize;
        int h = (getHeight() - 1) / pixelSize;
        int maxX = (w - 1) / 2;
        int maxY = (h - 1) / 2;
        int x1 = -maxX, x2 = maxX * -2 / 3, x3 = maxX * 2 / 3, x4 = maxX;
        int y1 = -maxY, y2 = maxY * -2 / 3, y3 = maxY * 2 / 3, y4 = maxY;

        drawLine(g, 0, 0, x3, y1); // NNE
        drawLine(g, 0, 0, x4, y2); // ENE
        drawLine(g, 0, 0, x4, y3); // ESE
        drawLine(g, 0, 0, x3, y4); // SSE
        drawLine(g, 0, 0, x2, y4); // SSW
        drawLine(g, 0, 0, x1, y3); // WSW
        drawLine(g, 0, 0, x1, y2); // WNW
        drawLine(g, 0, 0, x2, y1); // NNW
    }

    private void plot(Graphics g, int x, int y) {
        int w = (getWidth() - 1) / pixelSize;
        int h = (getHeight() - 1) / pixelSize;
        int maxX = (w - 1) / 2;
        int maxY = (h - 1) / 2;

        int borderX = getWidth() - ((2 * maxX + 1) * pixelSize + 1);
        int borderY = getHeight() - ((2 * maxY + 1) * pixelSize + 1);
        int left = (x + maxX) * pixelSize + borderX / 2;
        int top = (y + maxY) * pixelSize + borderY / 2;

        g.setColor(Color.black);
        g.drawOval(left, top, pixelSize, pixelSize);
    }

    private void drawLine(Graphics g, int x1, int y1, int x2, int y2) {
        // delta of exact value and rounded value of the dependent variable
        int d = 0;

        int dx = Math.abs(x2 - x1);
        int dy = Math.abs(y2 - y1);

        int dx2 = 2 * dx; // slope scaling factors to
        int dy2 = 2 * dy; // avoid floating point

        int ix = x1 < x2 ? 1 : -1; // increment direction
        int iy = y1 < y2 ? 1 : -1;

        int x = x1;
        int y = y1;

        if (dx >= dy) {
            while (true) {
                plot(g, x, y);
                if (x == x2)
                    break;
                x += ix;
                d += dy2;
                if (d > dx) {
                    y += iy;
                    d -= dx2;
                }
            }
        } else {
            while (true) {
                plot(g, x, y);
                if (y == y2)
                    break;
                y += iy;
                d += dx2;
                if (d > dy) {
                    x += ix;
                    d -= dy2;
                }
            }
        }
    }
}
```



## JavaScript

Instead of swaps in the initialisation use error calculation for both directions x and y simultaneously:

```javascript
function bline(x0, y0, x1, y1) {

  var dx = Math.abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
  var dy = Math.abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
  var err = (dx>dy ? dx : -dy)/2;

  while (true) {
    setPixel(x0,y0);
    if (x0 === x1 && y0 === y1) break;
    var e2 = err;
    if (e2 > -dx) { err -= dy; x0 += sx; }
    if (e2 < dy) { err += dx; y0 += sy; }
  }
}
```



## Julia

{{works with|Julia|0.6}}

```Julia
function drawline!(img::Matrix{T}, x0::Int, y0::Int, x1::Int, y1::Int, col::T) where T
    δx = abs(x1 - x0)
    δy = abs(y1 - y0)
    δe = abs(δy / δx)
    er = 0.0

    y = y0
    for x in x0:x1
        img[x, y] = col
        er += δe
        if er > 0.5
            y  += 1
            er -= 1.0
        end
    end

    return img
end

using Images

img = fill(Gray(255.0), 5, 5);
println("\nImage:")
display(img); println()
drawline!(img, 1, 1, 5, 5, Gray(0.0));
println("\nModified image:")
display(img); println()
```


{{out}}

```txt

Image:
5×5 Array{Gray{Float64},2}:
 Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)
 Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)
 Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)
 Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)
 Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)

Modified image:
5×5 Array{Gray{Float64},2}:
 Gray{Float64}(0.0)    Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)
 Gray{Float64}(255.0)  Gray{Float64}(0.0)    Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)
 Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(0.0)    Gray{Float64}(255.0)  Gray{Float64}(255.0)
 Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(0.0)    Gray{Float64}(255.0)
 Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(255.0)  Gray{Float64}(0.0)
```



## Korn Shell


<lang>function line {
        x0=$1; y0=$2 x1=$3; y1=$4

        if (( x0 > x1 ))
        then
                ((dx = x0 - x1)); ((sx = -1))
        else
                ((dx = x1 - x0)); ((sx = 1))
        fi

        if (( y0 > y1 ))
        then
                ((dy = y0 - y1)); ((sy = -1))
        else
                ((dy = y1 - y0)); ((sy = 1))
        fi

        if (( dx > dy ))
        then
                ((err = dx))
        else
                ((err = -dy))
        fi
        ((err /= 2)); ((e2 = 0))

        while /bin/true
        do
                echo $x0 $y0
                (( x0 == x1 && y0 == y1 )) && return
                ((e2 = err))
                (( e2 > -dx)) && { ((err -= dy )); (( x0 += sx )) }
                (( e2 <  dy)) && { ((err += dx )); (( y0 += sy )) }

        done
}
```


Output from the statement:-
    line 0 0 3 4
(which could be piped to another program)
<lang>0 0
1 1
1 2
2 3
3 4
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

import java.awt.*
import javax.swing.*

class Bresenham(w: Int, h: Int) : JPanel() {
    private val centerX = w / 2
    private val centerY = h / 2

    init {
        preferredSize = Dimension(w, h)
        background = Color.blue
    }

    override fun paintComponent(g: Graphics) {
        super.paintComponent(g)
        drawLine(g, 0, 0, 8, 19)   // NNE
        drawLine(g, 0, 0, 19, 8)   // ENE
        drawLine(g, 0, 0, 19, -8)  // ESE
        drawLine(g, 0, 0, 8, -19)  // SSE
        drawLine(g, 0, 0, -8, -19) // SSW
        drawLine(g, 0, 0, -19, -8) // WSW
        drawLine(g, 0, 0, -19, 8)  // WNW
        drawLine(g, 0, 0, -8, 19)  // NNW
    }

    private fun plot(g: Graphics, x: Int, y: Int) {
        g.color = Color.white
        g.drawOval(centerX + x * 10, centerY -y * 10, 10, 10)
    }

    private fun drawLine(g: Graphics, x1: Int, y1: Int, x2: Int, y2: Int) {
        var d = 0
        val dy = Math.abs(y2 - y1)
        val dx = Math.abs(x2 - x1)
        val dy2 = dy shl 1
        val dx2 = dx shl 1
        val ix = if (x1 < x2)  1 else -1
        val iy = if (y1 < y2)  1 else -1
        var xx = x1
        var yy = y1

        if (dy <= dx) {
            while (true) {
                plot(g, xx, yy)
                if (xx == x2) break
                xx += ix
                d  += dy2
                if (d > dx) {
                    yy += iy
                    d  -= dx2
                }
            }
        }
        else {
            while (true) {
                plot(g, xx, yy)
                if (yy == y2) break
                yy += iy
                d  += dx2
                if (d > dy) {
                    xx += ix
                    d  -= dy2
                }
            }
        }
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        f.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        f.isVisible = true
        f.add(Bresenham(600, 500), BorderLayout.CENTER)
        f.title = "Bresenham"
        f.isResizable = false
        f.pack()
        f.setLocationRelativeTo(null)
    }
}
```



## Maple



```maple
SegmentBresenham := proc (img, x0, y0, x1, y1)
    local deltax, deltay, x, y, ystep, steep, err, img2, x02, y02, x12, y12;
    x02, x12, y02, y12 := y0, y1, x0, x1;
    steep := abs(x12 - x02) < abs(y12 - y02);
    img2 := copy(img);
    if steep then
        x02, y02 := y02, x02;
        x12, y12 := y12, x12;
    end if;
    if x12 < x02 then
        x02, x12 := x12, x02;
        y02, y12 := y12, y02;
    end if;
    deltax := x12 - x02;
    deltay := abs(y12 - y02);
    err := deltax / 2;
    y := y02;
    if y02 < y12 then
        ystep := 1
    else
        ystep := -1
    end if;
    for x from x02 to x12 do
        if steep then
            img2[y, x] := 0
        else
            img2[x, y] := 0
        end if;
        err := err - deltay;
        if err < 0 then
            y := y + ystep;
            err := err + deltax
        end if;
    end do;
    return img2;
end proc:
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==


```Mathematica
Rasterize[ Style[Graphics[Line[{{0, 0}, {20, 10}}]], Antialiasing -> False]]

```



## MATLAB

Note: Store this function in a file named "bresenhamLine.m" in the @Bitmap folder for the Bitmap class defined [[Bitmap#MATLAB|here]].
[[File:Bresenham.png|thumb|MATLAB sample usage output.]]

```MATLAB

%screen     = Bitmap object
%startPoint = [x0,y0]
%endPoint   = [x1,y1]
%color      = [red,green,blue]

function bresenhamLine(screen,startPoint,endPoint,color)

  if( any(color > 255) )
      error 'RGB colors must be between 0 and 255';
  end

  %Check for vertical line, x0 == x1
  if( startPoint(1) == endPoint(1) )
      %Draw vertical line
      for i = (startPoint(2):endPoint(2))
          setPixel(screen,[startPoint(1) i],color);
      end
  end

  %Simplified Bresenham algorithm
  dx = abs(endPoint(1) - startPoint(1));
  dy = abs(endPoint(2) - startPoint(2));

  if(startPoint(1) < endPoint(1))
      sx = 1;
  else
      sx = -1;
  end

  if(startPoint(2) < endPoint(2))
      sy = 1;
  else
      sy = -1;
  end

  err = dx - dy;
  pixel = startPoint;

  while(true)

      screen.setPixel(pixel,color); %setPixel(x0,y0)

      if( pixel == endPoint )
          break;
      end

      e2 = 2*err;

      if( e2 > -dy )
          err = err - dy;
          pixel(1) = pixel(1) + sx;
      end

      if( e2 < dx )
          err = err + dx;
          pixel(2) = pixel(2) + sy;
      end
  end

  assignin('caller',inputname(1),screen); %saves the changes to the object
end

```


Sample Usage:

```MATLAB

>> img = Bitmap(800,600);
>> img.bresenhamLine([400 550],[200 400],[255 255 255]);
>> img.bresenhamLine([400 550],[600 400],[255 255 255]);
>> img.bresenhamLine([200 400],[350 150],[255 255 255]);
>> img.bresenhamLine([600 400],[450 150],[255 255 255]);
>> img.bresenhamLine([350 150],[450 150],[255 255 255]);
>> img.bresenhamLine([400 550],[400 150],[255 255 255]);
>> disp(img)

```



## MAXScript



```maxscript
fn plot img coord steep col =
(
    if steep then
    (
        swap coord[1] coord[2]
    )
    setPixels img coord col
)

fn drawLine img start end col =
(
    local steep = (abs (end.y - start.y)) > (abs (end.x - start.x))

    if steep then
    (
        swap start.x start.y
        swap end.x end.y
    )

    if start.x > end.x then
    (
        swap start.x end.x
        swap start.y end.y
    )

    local deltaX = end.x - start.x
    local deltaY = abs (end.y - start.y)
    local error = deltaX / 2.0
    local yStep = -1
    local y = start.y

    if start.y < end.y then
    (
        yStep = 1
    )

    for x in start.x to end.x do
    (
        plot img [x, y] steep col
        error -= deltaY
        if error < 0 then
        (
            y += yStep
            error += deltaX
        )
    )
    img
)

myBitmap = bitmap 512 512 color:(color 0 0 0)
myBitmap = drawLine myBitmap [0, 511] [511, 0] #((color 255 255 255))
display myBitmap
```



## Metal


For drawing lines between points in an Apple Metal compute shader.


```metal
void drawLine(texture2d<float, access::write> targetTexture, uint2 start, uint2 end);

void drawLine(texture2d<float, access::write> targetTexture, uint2 start, uint2 end)
{
    int x = int(start.x);
    int y = int(start.y);

    int dx = abs(x - int(end.x));
    int dy = abs(y - int(end.y));

    int sx = start.x < end.x ? 1 : -1;
    int sy = start.y < end.y ? 1 : -1;

    int err = (dx > dy ? dx : -dy) / 2;

    while (true)
    {
        targetTexture.write(float4(1.0), uint2(x, y));

        if (x == int(end.x) && y == int(end.y))
        {
            break;
        }

        int e2 = err;

        if (e2 > -dx)
        {
            err -= dy;
            x += sx;
        }

        if (e2 < dy)
        {
            err += dx;
            y += sy;
        }
    }
}
```



## Nim


```nim
import math

proc line(img: var Image, p, q: Point) =
  let
    dx = abs(q.x - p.x)
    sx = if p.x < q.x: 1 else: -1
    dy = abs(q.y - p.y)
    sy = if p.y < q.y: 1 else: -1

  var
    p = p
    q = q
    err = (if dx > dy: dx else: -dy) div 2
    e2 = 0

  while true:
    img[p] = Black
    if p == q:
      break
    e2 = err
    if e2 > -dx:
      err -= dy
      p.x += sx
    if e2 < dy:
      err += dx
      p.y += sy
```



## OCaml



```ocaml
let draw_line ~img ~color ~p0:(x0,y0) ~p1:(x1,y1) =

  let steep = abs(y1 - y0) > abs(x1 - x0) in

  let plot =
    if steep
    then (fun x y -> put_pixel img color y x)
    else (fun x y -> put_pixel img color x y)
  in

  let x0, y0, x1, y1 =
    if steep
    then y0, x0, y1, x1
    else x0, y0, x1, y1
  in
  let x0, x1, y0, y1 =
    if x0 > x1
    then x1, x0, y1, y0
    else x0, x1, y0, y1
  in

  let delta_x = x1 - x0
  and delta_y = abs(y1 - y0) in
  let error = -delta_x / 2
  and y_step =
    if y0 < y1 then 1 else -1
  in
  let rec loop x y error =
    plot x y;
    if x <= x1 then
      let error = error + delta_y in
      let y, error =
        if error > 0
        then (y + y_step), (error - delta_x)
        else y, error
      in
      loop (succ x) y error
  in
  loop x0 y0 error
;;
```



## Pascal


[[Bresenham's_line_algorithm#Delphi | Delphi]]


## Perl


{{libheader|Imlib2}}


```perl
#! /usr/bin/perl
use strict;
use Image::Imlib2;

sub my_draw_line
{
    my ( $img, $x0, $y0, $x1, $y1) = @_;

    my $steep = (abs($y1 - $y0) > abs($x1 - $x0));
    if ( $steep ) {
  ( $y0, $x0 ) = ( $x0, $y0);
  ( $y1, $x1 ) = ( $x1, $y1 );
    }
    if ( $x0 > $x1 ) {
  ( $x1, $x0 ) = ( $x0, $x1 );
  ( $y1, $y0 ) = ( $y0, $y1 );
    }
    my $deltax = $x1 - $x0;
    my $deltay = abs($y1 - $y0);
    my $error = $deltax / 2;
    my $ystep;
    my $y = $y0;
    my $x;
    $ystep = ( $y0 < $y1 ) ? 1 : -1;
    for( $x = $x0; $x <= $x1; $x += 1 ) {
  if ( $steep ) {
      $img->draw_point($y, $x);
  } else {
      $img->draw_point($x, $y);
  }
  $error -= $deltay;
  if ( $error < 0 ) {
      $y += $ystep;
      $error += $deltax;
  }
    }
}

# test
my $img = Image::Imlib2->new(160, 160);
$img->set_color(255, 255, 255, 255); # white
$img->fill_rectangle(0,0,160,160);

$img->set_color(0,0,0,255); # black
my_draw_line($img, 10, 80, 80, 160);
my_draw_line($img, 80, 160, 160, 80);
my_draw_line($img, 160, 80, 80, 10);
my_draw_line($img, 80, 10, 10, 80);

$img->save("test0.png");

# let's try the same using its internal algo
$img->set_color(255, 255, 255, 255); # white
$img->fill_rectangle(0,0,160,160);
$img->set_color(0,0,0,255); # black
$img->draw_line(10, 80, 80, 160);
$img->draw_line(80, 160, 160, 80);
$img->draw_line(160, 80, 80, 10);
$img->draw_line(80, 10, 10, 80);

$img->save("test1.png");

exit 0;
```


Images <tt>test0.png</tt> and <tt>test1.png</tt> look different since Imlib2 draw lines with antialiasing.


## Perl 6

{{works with|Rakudo|2018.03}}
Bitmap class from [[Bitmap#Perl_6|Bitmap]] task.

```perl6
class Pixel { has UInt ($.R, $.G, $.B) }
class Bitmap {
    has UInt ($.width, $.height);
    has Pixel @!data;

    method fill(Pixel $p) {
        @!data = $p.clone xx ($!width*$!height)
    }
    method pixel(
	$i where ^$!width,
	$j where ^$!height
	--> Pixel
    ) is rw { @!data[$i + $j * $!width] }

    method set-pixel ($i, $j, Pixel $p) {
	self.pixel($i, $j) = $p.clone;
    }
    method get-pixel ($i, $j) returns Pixel {
	self.pixel($i, $j);
    }
}

sub line(Bitmap $bitmap, $x0 is copy, $x1 is copy, $y0 is copy, $y1 is copy) {
    my $steep = abs($y1 - $y0) > abs($x1 - $x0);
    if $steep {
        ($x0, $y0) = ($y0, $x0);
        ($x1, $y1) = ($y1, $x1);
    }
    if $x0 > $x1 {
        ($x0, $x1) = ($x1, $x0);
        ($y0, $y1) = ($y1, $y0);
    }
    my $Δx = $x1 - $x0;
    my $Δy = abs($y1 - $y0);
    my $error = 0;
    my $Δerror = $Δy / $Δx;
    my $y-step = $y0 < $y1 ?? 1 !! -1;
    my $y = $y0;
    for $x0 .. $x1 -> $x {
        my $pix = Pixel.new(R => 100, G => 200, B => 0);
        if $steep {
            $bitmap.set-pixel($y, $x, $pix);
        } else {
            $bitmap.set-pixel($x, $y, $pix);
        }
        $error += $Δerror;
        if $error >= 0.5 {
            $y += $y-step;
            $error -= 1.0;
        }
    }
}
```



## Phix

Modified copy of [[Bitmap/Bresenham%27s_line_algorithm#Euphoria|Euphoria]], with a bigger bitmap and a simpler pattern.
Requires new_image() from [[Bitmap#Phix|Bitmap]], write_ppm() from [[Bitmap/Write_a_PPM_file#Phix|Write_a_PPM_file]].
Included as demo\rosetta\Bresenham_line.exw, results may be verified with demo\rosetta\viewppm.exw

```Phix
function bresLine(sequence screenData, integer x0, integer y0, integer x1, integer y1, integer colour)
-- The line algorithm
integer deltaX = abs(x1-x0),
        deltaY = abs(y1-y0),
        stepX = iff(x0<x1,1,-1),
        stepY = iff(y0<y1,1,-1),
        lineError = iff(deltaX>deltaY,deltaX,-deltaY),
        prevle

    lineError = round(lineError/2,1)
    while 1 do
        if x0>=1 and x0<=length(screenData)
        and y0>=1 and y0<=length(screenData[x0]) then
            screenData[x0][y0] = colour
        end if
        if x0=x1 and y0=y1 then exit end if
        prevle = lineError
        if prevle>-deltaX then
            lineError -= deltaY
            x0 += stepX
        end if
        if prevle<deltaY then
            lineError += deltaX
            y0 += stepY
        end if
    end while
    return screenData
end function

sequence screenData = new_image(400,300,black)
    screenData = bresLine(screenData,100,1,50,300,red)
    screenData = bresLine(screenData,1,180,400,240,green)
    screenData = bresLine(screenData,200,1,400,150,white)
    screenData = bresLine(screenData,195,1,205,300,blue)
    write_ppm("bresenham.ppm",screenData)
```



## PicoLisp


```PicoLisp
(de brez (Img X Y DX DY)
   (let SX
      (cond
         ((=0 DX) 0)
         ((gt0 DX) 1)
         (T (setq DX (- DX)) -1) )
      (let SY
         (cond
            ((=0 DY) 0)
            ((gt0 DY) 1)
            (T (setq DY (- DY)) -1) )
         (if (>= DX DY)
            (let E (- (* 2 DY) DX)
               (do DX
                  (set (nth Img Y X) 1)
                  (when (ge0 E)
                     (inc 'Y SY)
                     (dec 'E (* 2 DX)) )
                  (inc 'X SX)
                  (inc 'E (* 2 DY)) ) )
            (let E (- (* 2 DX) DY)
               (do DY
                  (set (nth Img Y X) 1)
                  (when (ge0 E)
                     (inc 'X SX)
                     (dec 'E (* 2 DY)) )
                  (inc 'Y SY)
                  (inc 'E (* 2 DX)) ) ) ) ) ) )

(let Img (make (do 90 (link (need 120 0))))        # Create image 120 x 90
   (brez Img 10 10 100 30)                         # Draw five lines
   (brez Img 10 10 100 50)
   (brez Img 10 10 100 70)
   (brez Img 10 10 60 70)
   (brez Img 10 10 20 70)
   (out "img.pbm"                                  # Write to bitmap file
      (prinl "P1")
      (prinl 120 " " 90)
      (mapc prinl Img) ) )
```



## PL/I


### version 1

{{incorrect|PL/I|The sample output does not start at -1/-3!?! Pls show the complete program producing this output.}}

```PL/I

/* Draw a line from (x0, y0) to (x1, y1).   13 May 2010 */
/* Based on Rosetta code proforma. */

   /* Declarations for image and selected color, for 4-bit colors. */
   declare image(40,40) bit (4), color bit (4) static initial ('1000'b);

draw_line: procedure (xi, yi, xf, yf );
   declare (xi, yi, xf, yf) fixed binary (31) nonassignable;
   declare (x0, y0, x1, y1) fixed binary (31);
   declare (deltax, deltay, x, y, ystep) fixed binary;
   declare (error initial (0), delta_error) float;
   declare steep bit (1);

   x0 = xi; y = YI; y0 = yi; x1 = xf; y1 = yf;
   steep = abs(y1 - y0) > abs (x1 - x0);
   if steep then
      do; call swap (x0, y0); call swap (x1, y1); end;
   if x0 > x1 then
      do; call swap (x0, x1); call swap (y0, y1); end;
   deltax = x1 - x0; deltay = abs(y1 - y0);
   delta_error = deltay/deltax;
   if y0 < y1 then ystep = 1; else ystep = -1;
   do x = x0 to x1;
       if steep then image(y, x) = color; else image(x, y) = color;
       if steep then put skip list (y, x); else put skip list (x, y);
       error = error + delta_error;
       if error >= 0.5 then do; y = y + ystep; error = error - 1; end;
   end;

swap: procedure (a, b);
   declare (a, b) fixed binary (31);
   declare t fixed binary (31);
   t = a; a = b; b = t;
end swap;

end draw_line;

```


Output from the statement:-
   call draw_line(-1, -3, 6, 10);
for a -10:10 x -10:10 grid:
<lang>
..........|..........
..........|..........
..........|..........
..........|..........
..........|..........
..........|..........
..........|.........*
..........|.......**.
..........|.....**...
..........|...**.....
----------+-**-------
..........**.........
........**|..........
.......*..|..........
..........|..........
..........|..........
..........|..........
..........|..........
..........|..........
..........|..........
..........|..........

```



### version 2


```PL/I
*process source xref or(!);
 brbn:Proc Options(main);
 /*********************************************************************
 * 21.05.2014  Walter Pachl
 * Implementing the pseudo code of
 *    http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
 * under 'Simplification' (see also REXX version 2)
 *********************************************************************/
grid.=
 dcl image(-2:7,-4:11) char(1);
 image='.';
 image(*,0)='-';
 image(0,*)='|';
 image(0,0)='+';
 call draw_line(-1,-3,6,10);
 Dcl (i,j) Bin Fixed(31);
 Do j=11 To -4 By -1;
   Put Edit(j,' ')(Skip,f(2),a);
   Do i=-2 To 7;
     Put Edit(image(i,j))(a);
     End;
   End;
 Put Edit('   2101234567')(Skip,a);

 draw_line: procedure (x0,y0,x1,y1);
 dcl (x0,y0,x1,y1) fixed binary(31);
 dcl (dx,dy,sx,sy,err,e2) fixed binary(31);

 dx = abs(x1-x0);
 dy = abs(y1-y0);
 if x0 < x1 then sx = 1;
            else sx = -1;
 if y0 < y1 then sy = 1;
            else sy = -1;
 err = dx-dy;

 Do Until(x0=x1&y0=y1);
   image(x0,y0)='X';
   e2=err*2;
   if e2>-dy then do;
     err=err-dy;
     x0=x0+sx;
     End;
   if e2<dx then do;
     err=err+dx;
     y0=y0+sy;
     End;
   End;
 image(x0,y0)='X';
 end;
 end;
```

'''output'''

```txt
11 ..|.......
10 ..|.....X.
 9 ..|....X..
 8 ..|....X..
 7 ..|...X...
 6 ..|...X...
 5 ..|..X....
 4 ..|..X....
 3 ..|.X.....
 2 ..|.X.....
 1 ..|X......
 0 --+X------
-1 ..X.......
-2 ..X.......
-3 .X|.......
-4 ..|.......
   2101234567
```



## Prolog

Works with SWI-prolog.


```Prolog

use_module(library(pce)).
lindraw(X1,Y1,X2,Y2):-
	new(Win,window("Line")),
	new(Pix,pixmap(@nil,black,white,X2+30,Y2+30)),
	send(Win,size,size(400,400)),
	draw_line(Pix,X1,Y1,X2,Y2),
	new(Bmp,bitmap(Pix)),
	send(Win,display,Bmp,point(0,0)),
	send(Win,open).

draw_recursive_line(_Pict,X,X,_DX,_DY,Y,Y,_D,_Sx,_Sy).%Don't iterate if X and X2 are the same number
draw_recursive_line(Pict,X,X2,DX,DY,Y,Y2,C,Sx,Sy):-
	(   C>0->%If the difference is greater than one, add Y one to Y.
	Y1 is Y+Sy,
	    send(Pict,pixel(X,Y1,colour(black))),
	    C2 is C+(2*DY-2*DX);
	Y1 is Y,
	send(Pict,pixel(X,Y,colour(black))),
	    C2 is C+(2*DY)),
	X0 is X+Sx,%The next iteration
	draw_recursive_line(Pict,X0,X2,DX,DY,Y1,Y2,C2,Sx,Sy).
isneg(X,O):-
	(   X<0->

	O is -1;
	(   X\==0->
	O is 1;
	O is 0)).

draw_line(Pict,X1,Y1,X2,Y2):-
	DY is abs(Y2-Y1),
	DX is abs(X2-X1),
	isneg(DX,Sx),
	isneg(DY,Sy),
	D = 2*DY-DX,%The slope of the line
	draw_recursive_line(Pict,X1,X2,DX,DY,Y1,Y2,D,Sx,Sy).

```



## PureBasic


```PureBasic
Procedure BresenhamLine(x0 ,y0 ,x1 ,y1)
     If Abs(y1 - y0) > Abs(x1 - x0);
        steep =#True
        Swap x0, y0
        Swap x1, y1
     EndIf
     If x0 > x1
         Swap x0, x1
         Swap y0, y1
     EndIf
     deltax = x1 - x0
     deltay = Abs(y1 - y0)
     error = deltax / 2
     y = y0
     If y0 < y1
        ystep = 1
     Else
        ystep = -1
     EndIf
     For x = x0 To x1
         If steep
           Plot(y,x)
         Else
           Plot(x,y)
         EndIf
         error - deltay
         If error < 0
             y + ystep
             error + deltax
         EndIf
     Next
EndProcedure

#Window1   = 0
#Image1    = 0
#ImgGadget = 0
#width     = 300
#height    = 300

Define.i Event
Define.f Angle

If OpenWindow(#Window1, 0, 0, #width, #height, "Bresenham's Line PureBasic Example", #PB_Window_SystemMenu|#PB_Window_ScreenCentered)
   If CreateImage(#Image1, #width, #height)
      ImageGadget(#ImgGadget, 0, 0, #width, #height, ImageID(#Image1))
      StartDrawing(ImageOutput(#Image1))
      FillArea(0,0,-1,$FFFFFF) :FrontColor(0)
      While Angle < 2*#PI
        BresenhamLine(150,150,150+Cos(Angle)*120,150+Sin(Angle)*120)
        Angle + #PI/60
      Wend

      StopDrawing()
      SetGadgetState(#ImgGadget, ImageID(#Image1))
      Repeat
        Event = WaitWindowEvent()
      Until Event = #PB_Event_CloseWindow
   EndIf
EndIf
```



## Python

{{works with|Python|3.1}}

Extending the example given [[Basic_bitmap_storage/Python#Alternative_version|here]] and using the algorithm from the Ada solution:


```python
def line(self, x0, y0, x1, y1):
    "Bresenham's line algorithm"
    dx = abs(x1 - x0)
    dy = abs(y1 - y0)
    x, y = x0, y0
    sx = -1 if x0 > x1 else 1
    sy = -1 if y0 > y1 else 1
    if dx > dy:
        err = dx / 2.0
        while x != x1:
            self.set(x, y)
            err -= dy
            if err < 0:
                y += sy
                err += dx
            x += sx
    else:
        err = dy / 2.0
        while y != y1:
            self.set(x, y)
            err -= dx
            if err < 0:
                x += sx
                err += dy
            y += sy
    self.set(x, y)
Bitmap.line = line

bitmap = Bitmap(17,17)
for points in ((1,8,8,16),(8,16,16,8),(16,8,8,1),(8,1,1,8)):
    bitmap.line(*points)
bitmap.chardisplay()

'''
The origin, 0,0; is the lower left, with x increasing to the right,
and Y increasing upwards.

The chardisplay above produces the following output :
+-----------------+
|        @        |
|       @ @       |
|      @   @      |
|     @     @     |
|    @       @    |
|    @        @   |
|   @          @  |
|  @            @ |
| @              @|
|  @            @ |
|   @          @  |
|    @       @@   |
|     @     @     |
|      @   @      |
|       @ @       |
|        @        |
|                 |
+-----------------+
'''
```



### Not relying on floats

Extending the example given [[Basic_bitmap_storage/Python#Alternative_version|here]].


```python

from fractions import Fraction

def line(self, x0, y0, x1, y1):
    rev = reversed
    if abs(y1 - y0) <= abs(x1 - x0):
        x0, y0, x1, y1 = y0, x0, y1, x1
        rev = lambda x: x
    if x1 < x0:
        x0, y0, x1, y1 = x1, y1, x0, y0
    leny = abs(y1 - y0)
    for i in range(leny + 1):
        self.set(*rev((round(Fraction(i, leny) * (x1 - x0)) + x0, (1 if y1 > y0 else -1) * i + y0)))

Bitmap.line = line

# see test code above

```



## Racket

Port of the Python version.

```racket

#lang racket
(require racket/draw)

(define (draw-line dc x0 y0 x1 y1)
  (define dx (abs (- x1 x0)))
  (define dy (abs (- y1 y0)))
  (define sx (if (> x0 x1) -1 1))
  (define sy (if (> y0 y1) -1 1))
  (cond
    [(> dx dy)
     (let loop ([x x0] [y y0] [err (/ dx 2.0)])
       (unless (= x x1)
         (send dc draw-point x y)
         (define newerr (- err dy))
         (if (< newerr 0)
             (loop (+ x sx) (+ y sy) (+ newerr dx))
             (loop (+ x sx)    y        newerr))))]
    [else
     (let loop ([x x0] [y y0] [err (/ dy 2.0)])
       (unless (= y y1)
         (send dc draw-point x y)
         (define newerr (- err dy))
         (if (< newerr 0)
             (loop (+ x sx) (+ y sy)    newerr)
             (loop    x     (+ y sy) (+ newerr dy)))))]))

(define bm (make-object bitmap% 17 17))
(define dc (new bitmap-dc% [bitmap bm]))
(send dc set-smoothing 'unsmoothed)
(send dc set-pen "red" 1 'solid)
(for ([points '((1 8 8 16) (8 16 16 8) (16 8 8 1) (8 1 1 8))])
  (apply draw-line  (cons dc points)))
bm

```



## RapidQ


Use this routine together with the code from [[Basic_bitmap_storage#RapidQ|Basic bitmap storage]] to create a full application.


```rapidq
SUB draw_line(x1, y1, x2, y2, colour)
    x_dist = abs(x2-x1)
    y_dist = abs(y2-y1)
    IF y2-y1 < -x_dist OR x2-x1 <= -y_dist THEN
        SWAP x1, x2       ' Swap start and end points
  SWAP y1, y2
    END IF
    IF x1 < x2 THEN x_step = 1 ELSE x_step = -1
    IF y1 < y2 THEN y_step = 1 ELSE y_step = -1

    IF y_dist > x_dist THEN     ' steep angle, step by y
  error = y_dist/2
  x = x1
  FOR y = y1 TO y2
      canvas.Pset(x, y, colour)
      error = error - x_dist
      IF error < 0 THEN
          x = x + x_step
    error = error + y_dist
      END IF
  NEXT y
    ELSE          ' not steep, step by x
        error = x_dist/2
  y = y1
  FOR x = x1 TO x2
      canvas.Pset(x, y, colour)
      error = error - y_dist
      IF error < 0 THEN
          y = y + y_step
    error = error + x_dist
      END IF
  NEXT y
    END IF

END SUB
```


Example usage:


```rapidq
SUB PaintCanvas
    draw_line 200,  10, 100, 200, &H00ff00
    draw_line 100, 200, 200, 400, &H00ff00
    draw_line 200, 400, 300, 200, &H00ff00
    draw_line 300, 200, 200,  10, &H00ff00
END SUB
```



## REXX


### version 1

This REXX version has automatic scaling (for displaying the plot),   includes a border,   accepts lines segments from the

command line,   displays a (background) plot field,   and it also handles multiple line segments.

```rexx
/*REXX program  plots/draws line segments  using the  Bresenham's line  (2D) algorithm. */
parse arg data                                   /*obtain optional arguments from the CL*/
if data=''  then data= "(1,8)  (8,16)  (16,8)  (8,1)  (1,8)"         /* ◄──── a rhombus.*/
data=translate(data, , '()[]{}/,:;')             /*elide chaff from the data points.    */
@.='·'                                           /*fill the array with middle─dots chars*/
          do points=1  while data\=''            /*put the data points into an array (!)*/
          parse var data x y data;  !.points=x y /*extract the line segments.           */
          if points==1  then do;  minX=x;  maxX=x;  minY=y;  maxY=y;  end    /*1st case.*/
          minX=min(minX,x); maxX=max(maxX,x);  minY=min(minY,y);  maxY=max(maxY,y)
          end   /*points*/                       /* [↑]  data points pairs in array  !. */
border=2                                         /*border:  is extra space around plot. */
minX=minX-border*2;    maxX=maxX+border*2        /*min and max  X  for the plot display.*/
minY=minY-border  ;    maxY=maxY+border          /* "   "   "   Y   "   "    "     "    */
          do x=minX  to maxX;  @.x.0='─';  end   /*draw a dash from    left ───►  right.*/
          do y=minY  to maxY;  @.0.y='│';  end   /*draw a pipe from  lowest ───► highest*/
@.0.0='┼'                                        /*define the plot's origin axis point. */
          do seg=2  to points-1;   _=seg-1       /*obtain the  X and Y  line coördinates*/
          call draw_line   !._, !.seg            /*draw (plot) a line segment.          */
          end   /*seg*/                          /* [↑]  drawing the line segments.     */
                                                 /* [↓]  display the plot to terminal.  */
          do   y=maxY  to minY  by -1;  _=       /*display the plot one line at a time. */
            do x=minX  to maxX;    _=_ || @.x.y  /*construct/build a line of the plot.  */
            end   /*x*/                          /*      (a line is a "row" of points.) */
          say _                                  /*display a line of the plot──►terminal*/
          end     /*y*/                          /* [↑]  all done plotting the points.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
draw_line: procedure expose @.;    parse arg  x y,xf yf;             plotChar='Θ'
dx=abs(xf-x);   if x<xf  then sx= +1             /*obtain  X  range, determine the slope*/
                         else sx= -1             /*                  negative slope.    */
dy=abs(yf-y);   if y<yf  then sy= +1             /*obtain  Y  range, determine the slope*/
                         else sy= -1             /*                  negative slope.    */
err=dx-dy                                        /*calculate error between adjustments. */
          do  forever;    @.x.y=plotChar         /*plot the points until it's complete. */
          if x=xf & y=yf  then return            /*are the plot points at the finish?   */
          err2=err+err                           /*addition is faster than:   err*2.    */
          if err2 > -dy  then  do;  err=err-dy;  x=x+sx;   end
          if err2 <  dx  then  do;  err=err+dx;  y=y+sy;   end
          end   /*forever*/
```

'''output'''   when using the default input:

```txt

···│····················
···│····················
···│·······Θ············
···│······Θ·Θ···········
···│·····Θ···Θ··········
···│····Θ·····Θ·········
···│···Θ·······Θ········
···│···Θ········Θ·······
···│··Θ··········Θ······
···│·Θ············Θ·····
···│Θ··············Θ····
···│·Θ············Θ·····
···│··Θ··········Θ······
···│···Θ·······ΘΘ·······
···│····Θ·····Θ·········
···│·····Θ···Θ··········
···│······Θ·Θ···········
···│·······Θ············
───┼────────────────────
···│····················

```



### version 2


```rexx
/* REXX ***************************************************************
* 21.05.2014  Walter Pachl
* Implementing the pseudo code of
*    http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
* under 'Simplification'
**********************************************************************/
grid.='.'
Do i=-2 To  7; grid.i.0='-'; End
Do j=-4 To 11; grid.0.j='|'; End
grid.0.0='+'
Call line -1,-3,6,10
Do j=11 To -4 By -1
  ol=format(j,2)' '
  Do i=-2 To 7
    ol=ol||grid.i.j
    End
  Say ol
  End
Say '   2101234567'
Exit
line: Procedure Expose grid.
Parse Arg x0, y0, x1, y1
dx = abs(x1-x0)
dy = abs(y1-y0)
if x0 < x1 then sx = 1
           else sx = -1
if y0 < y1 then sy = 1
           else sy = -1
err = dx-dy

Do Forever
  grid.x0.y0='X'
  if x0 = x1 & y0 = y1 Then Leave
  e2 = 2*err
  if e2 > -dy then do
    err = err - dy
    x0 = x0 + sx
    end
  if e2 < dx then do
    err = err + dx
    y0 = y0 + sy
    end
  end
Return
```

'''output'''

```txt
11 ..|.......
10 ..|.....X.
 9 ..|....X..
 8 ..|....X..
 7 ..|...X...
 6 ..|...X...
 5 ..|..X....
 4 ..|..X....
 3 ..|.X.....
 2 ..|.X.....
 1 ..|X......
 0 --+X------
-1 ..X.......
-2 ..X.......
-3 .X|.......
-4 ..|.......
   2101234567
```



## Ring


```ring

load "guilib.ring"
load "stdlib.ring"

new qapp
       {
       win1 = new qwidget() {
              setwindowtitle("drawing using qpainter")
              setwinicon(self,"c:\ring\bin\image\browser.png")
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

        line = [[50,100,100,190], [100,190,150,100], [150,100,100,10], [100,10,50,100]]

        for n = 1 to 4
            x1=line[n][1] y1=line[n][2] x2=line[n][3] y2=line[n][4]
            dx = fabs(x2 - x1)  sx = sign(x2 - x1)
            dy = fabs(y2 - y1)  sy = sign(y2 - y1)
            if dx < dy e = dx / 2 else e = dy / 2 ok
            while true
                  drawline (x1*2,y1*2,x2*2,y2*2)
                  if x1 = x2 if y1 = y2 exit ok ok
                  if dx > dy
                     x1 += sx  e -= dy if e < 0 e += dx  y1 += sy ok
                  else
                     y1 += sy e -= dx if e < 0 e += dy x1 += sx ok ok
            end
        next

        endpaint()
        }
        label1 { setpicture(p1) show() }

```

Output :
[https://lh3.googleusercontent.com/-6uJvON0dAuo/V2LO2zz4zQI/AAAAAAAAALE/IjEGFuhta6oUSeG2QfuxcPBWMmCyNCjdwCLcB/s1600/CalmoSoftBresenham.jpg Bitmap/Bresenham's algorithm]


## Ruby


```ruby
Pixel = Struct.new(:x, :y)

class Pixmap

  def draw_line(p1, p2, colour)
    validate_pixel(p1.x, p2.y)
    validate_pixel(p2.x, p2.y)

    x1, y1 = p1.x, p1.y
    x2, y2 = p2.x, p2.y

    steep = (y2 - y1).abs > (x2 - x1).abs

    if steep
      x1, y1 = y1, x1
      x2, y2 = y2, x2
    end

    if x1 > x2
      x1, x2 = x2, x1
      y1, y2 = y2, y1
    end

    deltax = x2 - x1
    deltay = (y2 - y1).abs
    error = deltax / 2
    ystep = y1 < y2 ? 1 : -1

    y = y1
    x1.upto(x2) do |x|
      pixel = steep ? [y,x] : [x,y]
      self[*pixel] = colour
      error -= deltay
      if error < 0
        y += ystep
        error += deltax
      end
    end
  end
end

bitmap = Pixmap.new(500, 500)
bitmap.fill(RGBColour::BLUE)
10.step(430, 60) do |a|
  bitmap.draw_line(Pixel[10, 10], Pixel[490,a], RGBColour::YELLOW)
  bitmap.draw_line(Pixel[10, 10], Pixel[a,490], RGBColour::YELLOW)
end
bitmap.draw_line(Pixel[10, 10], Pixel[490,490], RGBColour::YELLOW)
```



## Rust


```Rust

struct Point {
    x: i32,
    y: i32
}

fn main() {
    let result = get_coordinates(1, 1, 69, 28);
    draw_line(result, 70, 30);
}

fn get_coordinates(x1: i32, y1: i32, x2: i32, y2: i32) -> Vec<Point> {
    let mut coordinates = vec![];
    let dx:i32 = i32::abs(x2 - x1);
    let dy:i32 = i32::abs(y2 - y1);
    let sx:i32 = {
        if x1 < x2 {
            1
        } else {
            -1
        }
    };
    let sy:i32 ={
        if y1 < y2 {
            1
        } else {
            -1
        }
    };
    let mut error:i32 = dx - dy;
    let mut current_x:i32 = x1;
    let mut current_y:i32 = y1;
    coordinates.push(Point { x: current_x, y: current_y });
    while current_x != x2 && current_y != y2 {
        let error2:i32 = 2 * error;
        if error2 >= i32::abs(dy) {
            error -= dy;
            current_x += sx;
            coordinates.push(Point { x: current_x, y: current_y });
        } else if error2 <= i32::abs(dx) {
            error += dx;
            current_y += sy;
            coordinates.push(Point { x: current_x, y: current_y });
        }
    }
    coordinates
}

fn draw_line(line: std::vec::Vec<Point>, width: i32, height: i32) {
    for col in 0..height {
        for row in 0..width {
            let is_point_in_line = line.iter().any(| point| point.x == row && point.y == col);
            match is_point_in_line {
                true => print!("❖"),
                _ => {
                    if col == 0 || col == (height - 1) || row == 0 || row == (width - 1) {
                        print!("☗");
                    } else {
                        print!(".");
                    }
                }
            };
        }
        print!("\n");
    }
}

```

'''Output:'''

```txt

☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗
☗❖❖❖.................................................................☗
☗..❖❖❖...............................................................☗
☗....❖❖❖❖............................................................☗
☗.......❖❖❖..........................................................☗
☗.........❖❖❖❖.......................................................☗
☗............❖❖❖.....................................................☗
☗..............❖❖❖❖..................................................☗
☗.................❖❖❖................................................☗
☗...................❖❖❖❖.............................................☗
☗......................❖❖❖...........................................☗
☗........................❖❖❖❖........................................☗
☗...........................❖❖❖......................................☗
☗.............................❖❖❖❖...................................☗
☗................................❖❖❖.................................☗
☗..................................❖❖❖❖..............................☗
☗.....................................❖❖❖............................☗
☗.......................................❖❖❖❖.........................☗
☗..........................................❖❖❖.......................☗
☗............................................❖❖❖❖....................☗
☗...............................................❖❖❖..................☗
☗.................................................❖❖❖❖...............☗
☗....................................................❖❖❖.............☗
☗......................................................❖❖❖❖..........☗
☗.........................................................❖❖❖........☗
☗...........................................................❖❖❖❖.....☗
☗..............................................................❖❖❖...☗
☗................................................................❖❖❖❖☗
☗...................................................................❖☗
☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗☗

```



## Scala

Uses the [[Basic_bitmap_storage#Scala|Scala Basic Bitmap Storage]] class.

```scala
object BitmapOps {
   def bresenham(bm:RgbBitmap, x0:Int, y0:Int, x1:Int, y1:Int, c:Color)={
      val dx=math.abs(x1-x0)
      val sx=if (x0<x1) 1 else -1
      val dy=math.abs(y1-y0)
      val sy=if (y0<y1) 1 else -1

      def it=new Iterator[Tuple2[Int,Int]]{
         var x=x0; var y=y0
         var err=(if (dx>dy) dx else -dy)/2
         def next={
            val res=(x,y)
            val e2=err;
            if (e2 > -dx) {err-=dy; x+=sx}
            if (e2<dy) {err+=dx; y+=sy}
            res;
         }
         def hasNext = (sx*x <= sx*x1 && sy*y <= sy*y1)
      }

      for((x,y) <- it)
         bm.setPixel(x, y, c)
   }
}
```



## Sidef

{{trans|Perl}}

```ruby
func my_draw_line(img, x0, y0, x1, y1) {

    var steep = (abs(y1 - y0) > abs(x1 - x0))

    if (steep) {
        (y0, x0) = (x0, y0)
        (y1, x1) = (x1, y1)
    }
    if (x0 > x1) {
        (x1, x0) = (x0, x1)
        (y1, y0) = (y0, y1)
    }

    var deltax = (x1 - x0)
    var deltay = abs(y1 - y0)
    var error  = (deltax / 2)
    var y = y0
    var ystep = (y0 < y1 ? 1 : -1)

    x0.to(x1).each { |x|
        img.draw_point(steep ? ((y, x)) : ((x, y)))
        error -= deltay
        if (error < 0) {
            y += ystep
            error += deltax
        }
    }
}

require('Image::Imlib2')

var img = %s'Image::Imlib2'.new(160, 160)
img.set_color(255, 255, 255, 255) # white
img.fill_rectangle(0,0,160,160)

img.set_color(0,0,0,255) # black
my_draw_line(img, 10, 80, 80, 160)
my_draw_line(img, 80, 160, 160, 80)
my_draw_line(img, 160, 80, 80, 10)
my_draw_line(img, 80, 10, 10, 80)

img.save("test0.png");

# let's try the same using its internal algo
img.set_color(255, 255, 255, 255) # white
img.fill_rectangle(0,0,160,160)
img.set_color(0,0,0,255) # black
img.draw_line(10, 80, 80, 160)
img.draw_line(80, 160, 160, 80)
img.draw_line(160, 80, 80, 10)
img.draw_line(80, 10, 10, 80)

img.save("test1.png")
```



## Tcl

{{libheader|Tk}}
ref [[Basic bitmap storage#Tcl]]

```tcl
package require Tcl 8.5
package require Tk

proc drawLine {image colour point0 point1} {
    lassign $point0 x0 y0
    lassign $point1 x1 y1

    set steep [expr {abs($y1 - $y0) > abs($x1 - $x0)}]
    if {$steep} {
        lassign [list $x0 $y0] y0 x0
        lassign [list $x1 $y1] y1 x1
    }
    if {$x0 > $x1} {
        lassign [list $x0 $x1] x1 x0
        lassign [list $y0 $y1] y1 y0
    }
    set deltax [expr {$x1 - $x0}]
    set deltay [expr {abs($y1 - $y0)}]
    set error [expr {$deltax / 2}]
    set ystep [expr {$y0 < $y1 ? 1 : -1}]

    for {set x $x0; set y $y0} {$x <= $x1} {incr x} {
        setPixel $image $colour [expr {$steep ? [list $y $x] : [list $x $y]}]
        incr error -$deltay
        if {$error < 0} {
            incr y $ystep
            incr error $deltax
        }
    }
}

# create the image and display it
set img [newImage 200 100]
label .l -image $img
pack .l

fill $img black
drawLine $img yellow {20 20} {180 80}
drawLine $img yellow {180 20} {20 80}
```


=={{header|TI-89 BASIC}}==

{{TI-image-task}}

{{trans|E}}


```ti89b
(lx0, ly0, lx1, ly1)
Prgm
  Local steep, x, y, dx, dy, ystep, error, tmp
  abs(ly1 - ly0) > abs(lx1 - lx0) → steep
  If steep Then
    lx0 → tmp
    ly0 → lx0
    tmp → ly0
    lx1 → tmp
    ly1 → lx1
    tmp → ly1
  EndIf
  If lx0 > lx1 Then
    lx0 → tmp
    lx1 → lx0
    tmp → lx1
    ly0 → tmp
    ly1 → ly0
    tmp → ly1
  EndIf
  lx1 - lx0 → dx
  abs(ly1 - ly0) → dy
  when(ly0 < ly1, 1, –1) → ystep
  intDiv(dx, 2) → error
  ly0 → y
  For x,lx0,lx1
    If steep Then: PxlChg x, y :Else: PxlChg y, x :EndIf
    error - dy → error
    If error < 0 Then
      y + ystep → y
      error + dx → error
    EndIf
  EndFor
EndPrgm
```



## VBScript

{{trans|Rexx}}

```vb
'Bitmap/Bresenham's line algorithm - VBScript - 13/05/2019
	Dim map(48,40), list(10), ox, oy
	data=Array(1,8, 8,16, 16,8, 8,1, 1,8)
	For i=0 To UBound(map,1): For j=0 To UBound(map,2)
		map(i,j)="."
	Next: Next 'j, i
	points=(UBound(data)+1)/2
	For p=1 To points
		x=data((p-1)*2)
		y=data((p-1)*2+1)
		list(p)=Array(x,y)
		If p=1 Then minX=x: maxX=x: minY=y: maxY=y
		If x<minX Then minX=x
		If x>maxX Then maxX=x
		If y<minY Then minY=y
		If y>maxY Then maxY=y
	Next 'p
	border=2
	minX=minX-border*2  : maxX=maxX+border*2
	minY=minY-border    : maxY=maxY+border
	ox =-minX           : oy =-minY
	wx=UBound(map,1)-ox : If maxX>wx Then maxX=wx
	wy=UBound(map,2)-oy : If maxY>wy Then maxY=wy
	For x=minX To maxX: map(x+ox,0+oy)="-": Next 'x
	For y=minY To maxY: map(0+ox,y+oy)="|": Next 'y
	map(ox,oy)="+"
	For p=1 To points-1
		draw_line list(p), list(p+1)
	Next 'p
	For y=maxY To minY Step -1
		line=""
		For x=minX To maxX
			line=line & map(x+ox,y+oy)
		Next 'x
		Wscript.Echo line
	Next 'y

Sub draw_line(p1, p2)
	Dim x,y,xf,yf,dx,dy,sx,sy,err,err2
    x =p1(0)     : y =p1(1)
	xf=p2(0)     : yf=p2(1)
	dx=Abs(xf-x) : dy=Abs(yf-y)
	If x<xf Then sx=+1: Else sx=-1
	If y<yf Then sy=+1: Else sy=-1
	err=dx-dy
	Do
		map(x+ox,y+oy)="X"
		If x=xf And y=yf Then Exit Do
		err2=err+err
		If err2>-dy Then err=err-dy: x=x+sx
		If err2< dx Then err=err+dx: y=y+sy
	Loop
End Sub 'draw_line
```

{{out}}

```txt

...|....................
...|....................
...|.......X............
...|......X.X...........
...|.....X...X..........
...|....X.....X.........
...|...X.......X........
...|...X........X.......
...|..X..........X......
...|.X............X.....
...|X..............X....
...|.X............X.....
...|..X..........X......
...|...X.......XX.......
...|....X.....X.........
...|.....X...X..........
...|......X.X...........
...|.......X............
---+--------------------
...|....................

```





## Vedit macro language



```vedit
//  Daw a line using Bresenham's line algorithm.
//  #1=x1, #2=y1; #3=x2, #4=y2

:DRAW_LINE:
Num_Push(31,35)
#31 = abs(#3-#1)    // x distance
#32 = abs(#4-#2)    // y distance
if (#4-#2 < -#31 || #3-#1 <= -#32) {
    #99=#1; #1=#3; #3=#99 // swap start and end points
    #99=#2; #2=#4; #4=#99
}
if (#1 < #3) { #34=1 } else { #34=-1 }  // x step
if (#2 < #4) { #35=1 } else { #35=-1 }  // y step

if (#32 > #31) {    // steep angle, step by Y
    #33 = #32 / 2   // error distance
    while (#2 <= #4) {
  Call("DRAW_PIXEL")
  #33 -= #31
  if (#33 < 0) {
      #1 += #34   // move right
      #33 += #32
  }
  #2++      // move up
    }
} else {      // not steep, step by X
    #33 = #31 / 2
    while (#1 <= #3) {
  Call("DRAW_PIXEL")
  #33 -= #32
  if (#33 < 0) {
      #2 += #35   // move up
      #33 += #31
  }
  #1++      // move right
    }
}
Num_Pop(31,35)
return
```



## Wart


```wart
# doesn't handle vertical lines
def (line x0 y0 x1 y1)
  let steep ((> abs) y1-y0 x1-x0)
    when steep
      swap! x0 y0
      swap! x1 y1
    when (x0 > x1)
      swap! x0 x1
      swap! y0 y1
    withs (deltax x1-x0
           deltay (abs y1-y0)
           error deltax/2
           ystep (if (y0 < y1) 1 -1)
           y y0)
      for x x0 (x <= x1) ++x
        if steep
          plot y x
          plot x y
        error -= deltay
        when (error < 0)
          y += ystep
          error += deltax
```



## XPL0

Bresenham line draw is built-in.

```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
[SetVid($112);          \set 640x480 graphics in 24-bit color
Move(10, 20);           \set start of line segment
Line(600, 400, $123456);\draw line segment, red=$12, green=$34, blue=$56
if ChIn(1) then [];     \wait for keystroke while viewing graphic screen
SetVid(3);              \restore normal text mode
]
```



## zkl

[[File:Line.zkl.jpg|200px|thumb]]
Algorithm from Wikipedia plus other functions so I can reference this code in other examples.

```zkl
ppm:=PPM(200,200,0xFF|FF|FF);
ppm.line(50,100,  100,190, 0);
ppm.line(100,190, 150,100, 0);
ppm.line(150,100, 100,10,  0);
ppm.line(100,10,  50,100,  0);

ppm.writeJPGFile("line.jpg");
```


```zkl
class PPM{  // (0,0) is logically bottom left
   fcn init(width,height,rgb=0){
      sz:=width*height;
      var [const]
         data=Data(sz*3).fill(rgb.toBigEndian(3).toData()),  // initialize to 24bit Black (RGB=000)
	 w=width, h=height;
   }
   fcn fill(rgb){ data.fill(rgb.toBigEndian(3).toData()) }
   fcn __sGet(x,y)    { data.toBigEndian(3*y*w + 3*x,3); }	  //ppm[x,y]
   fcn __sSet(rgb,x,y){	data[3*y*w + x*3,3]=rgb.toBigEndian(3); rgb } //ppm[x,y]=rgb
   fcn write(out,raw=False){   // write bottom to top to move (0,0) from top left to bottom left
      out.write("P6\n#rosettacode PPM\n%d %d\n255\n".fmt(w,h));
      if(raw) out.write(data);
      else [h-1..0, -1].pump(out,'wrap(h){ data.seek(3*h*w); data.read(3*w) });
   }
   fcn writeJPGFile(fname){	// Linux, using imagemagick
      System.popen(0'|convert ppm:- jpg:"%s"|.fmt(fname),"w") :
      write(_,vm.pasteArgs(1));
   }
   fcn readJPGFile(fileName){	// Linux, using imagemagick
      p:=System.popen("convert \"%s\" ppm:-".fmt(fileName),"r");
	 img:=PPM.readPPM(p);
      p.close();
      img
   }
   fcn readPPMFile(fileName){
      f:=File(fileName,"rb"); ppm:=readPPM(f); f.close();
      ppm
   }
   fcn readPPM(image){ // image is a PPM byte stream
      // header is "P6\n[#comment\n]<w> <h>\nmaxPixelValue\n
      image.readln();  // "P6"
      while("#"==(text:=image.readln().strip())[0]){}
      w,h:=text.split().apply("toInt");
      image.readln(); // max pixel value
      ppm,sz,buffer:=PPM(w,h), 3*w, Data(sz);
      ppm.data.clear(); // gonna write file image data
      // image is stored upside down in my data structure
      do(h){ ppm.data.insert(0, image.read(sz,buffer)) }
      ppm
   }
   fcn circle(x0,y0,r,rgb){
      x:=r; y:=0; radiusError:=1-x;
      while(x >= y){
         __sSet(rgb, x + x0,  y + y0);
	 __sSet(rgb, y + x0,  x + y0);
	 __sSet(rgb,-x + x0,  y + y0);
	 __sSet(rgb,-y + x0,  x + y0);
	 self[-x + x0, -y + y0]=rgb;	// or do it this way, __sSet gets called as above
	 self[-y + x0, -x + y0]=rgb;
	 self[ x + x0, -y + y0]=rgb;
	 self[ y + x0, -x + y0]=rgb;
	 y+=1;
	 if (radiusError<0) radiusError+=2*y + 1;
	 else{ x-=1; radiusError+=2*(y - x + 1); }
      }
   }
   fcn cross(x,y,rgb=0xff|00,len=10){
      a:=len/2; b:=len-a;
      line(x-a,y, x+b,y,rgb); line(x,y-a, x,y+b,rgb);
   }
   fcn line(x0,y0, x1,y1, rgb){
      dx:=(x1-x0).abs();
      dy:=(y1-y0).abs();
      if(x0 < x1) sx:=1 else sx:=-1;
      if(y0 < y1) sy:=1 else sy:=-1;
      err:=dx - dy;
      while(1){
	 __sSet(rgb,x0,y0);
	 if(x0==x1 and y0==y1) break;
	 e2:=2*err;
	 if(e2 > -dy){ err=err - dy; x0=x0 + sx; }
	 if(e2 < dx) { err=err + dx; y0=y0 + sy; }
      }
   }
   fcn flood(x,y, repl){  // slow!
      targ:=self[x,y];
      (stack:=List.createLong(10000)).append(T(x,y));
      while(stack){
	 x,y:=stack.pop();
	 if((0<=y<h) and (0<=x<w)){
	    p:=self[x,y];
	    if(p==targ){
	       self[x,y]=repl;
	       stack.append( T(x-1,y), T(x+1,y), T(x, y-1), T(x, y+1) );
	    }
	 }
      }
   }
}
```


{{omit from|AWK}}
{{omit from|PARI/GP}}

[[Category:Geometry]]
