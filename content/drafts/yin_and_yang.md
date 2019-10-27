+++
title = "Yin and yang"
description = ""
date = 2019-10-14T01:17:14Z
aliases = []
[extra]
id = 9410
[taxonomies]
categories = []
tags = []
+++

{{task|Graphics}}
{{omit from|GUISS}}

;Task:
Create a function that given a variable representing size, generates a [[wp:File:Yin_and_Yang.svg|Yin and yang]] also known as a [[wp:Taijitu|Taijitu]] symbol scaled to that size.

Generate and display the symbol generated for two different (small) sizes.





## Ada

{{libheader|GtkAda}}
Uses the Cairo component of GtkAda to create and save as png
[[file:YinYangAda.png|right]]

```Ada
with Glib; use Glib;
with Cairo; use Cairo;
with Cairo.Png; use Cairo.Png;
with Cairo.Image_Surface; use Cairo.Image_Surface;

procedure YinYang is
   subtype Dub is Glib.Gdouble;

   procedure Draw (C : Cairo_Context; x : Dub; y : Dub; r : Dub) is begin
      Arc (C, x, y, r + 1.0, 1.571, 7.854);
      Set_Source_Rgb (C, 0.0, 0.0, 0.0); Fill (C);
      Arc_Negative (C, x, y - r / 2.0, r / 2.0, 1.571, 4.712);
      Arc (C, x, y + r / 2.0, r / 2.0, 1.571, 4.712);
      Arc_Negative (C, x, y, r, 4.712, 1.571);
      Set_Source_Rgb (C, 1.0, 1.0, 1.0); Fill (C);
      Arc (C, x, y - r / 2.0, r / 5.0, 1.571, 7.854);
      Set_Source_Rgb (C, 0.0, 0.0, 0.0); Fill (C);
      Arc (C, x, y + r / 2.0, r / 5.0, 1.571, 7.854);
      Set_Source_Rgb (C, 1.0, 1.0, 1.0); Fill (C);
   end Draw;

   Surface : Cairo_Surface;
   Context : Cairo_Context;
   Status : Cairo_Status;
begin
   Surface := Create (Cairo_Format_ARGB32, 200, 200);
   Context := Create (Surface);
   Draw (Context, 120.0, 120.0, 75.0);
   Draw (Context, 35.0, 35.0, 30.0);
   Status := Write_To_Png (Surface, "YinYangAda.png");
   pragma Assert (Status = Cairo_Status_Success);
end YinYang;
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - With Currying extensions to language.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due use of Currying.}}

```algol68
INT scale x=2, scale y=1;
CHAR black="#", white=".", clear=" ";

PROC print yin yang = (REAL radius)VOID:(

  PROC in circle = (REAL centre x, centre y, radius, x, y)BOOL:
    (x-centre x)**2+(y-centre y)**2 <= radius**2;

  PROC (REAL, REAL)BOOL
    in big circle = in circle(0, 0, radius, , ),
    in white semi circle  = in circle(0, +radius/2, radius/2, , ),
    in small black circle = in circle(0, +radius/2, radius/6, , ),
    in black semi circle  = in circle(0, -radius/2, radius/2, , ),
    in small white circle = in circle(0, -radius/2, radius/6, , );

  FOR sy FROM +ROUND(radius * scale y) BY -1 TO -ROUND(radius * scale y) DO
    FOR sx FROM -ROUND(radius * scale x) TO +ROUND(radius * scale x) DO
      REAL x=sx/scale x, y=sy/scale y;
      print(
        IF in big circle(x, y) THEN
            IF in white semi circle(x, y) THEN
              IF in small black circle(x, y) THEN black ELSE white FI
            ELIF in black semi circle(x, y) THEN
              IF in small white circle(x, y) THEN white ELSE black FI
            ELIF x < 0 THEN white ELSE black FI
        ELSE
          clear
        FI
      )
    OD;
    print(new line)
  OD
);

main:(
  print yin yang(17);
  print yin yang(8)
)
```

{{out}}

```txt

                                  .                                  
                       ....................###                       
                  ...........................######                  
               ................................#######               
             ....................................#######             
          ........................................#########          
         .......................#####..............#########         
       .......................#########............###########       
      .......................###########...........############      
    .........................###########...........##############    
    ..........................#########............##############    
   .............................#####..............###############   
  ................................................#################  
 ................................................################### 
 ..............................................##################### 
 ............................................####################### 
 ..........................................######################### 
...................................##################################
 .........................########################################## 
 .......................############################################ 
 .....................############################################## 
 ...................################################################ 
  .................################################################  
   ...............##############.....#############################   
    ..............############.........##########################    
    ..............###########...........#########################    
      ............###########...........#######################      
       ...........############.........#######################       
         .........##############.....#######################         
          .........########################################          
             .......####################################             
               .......################################               
                  ......###########################                  
                       ...####################                       
                                  #                                  
                .                
         .............##         
      .................####      
    ...........###......#####    
   ...........#####......#####   
  .............###......#######  
 ......................######### 
 .....................########## 
.................################
 ..........##################### 
 .........###################### 
  .......######...#############  
   .....######.....###########   
    .....######...###########    
      ....#################      
         ..#############         
                #                

```



## Asymptote

[[File:Yinyang-asymptote.svg|thumb|The resulting EPS, converted to SVG]]

```asymptote
unitsize(1 inch);

fill(scale(6)*unitsquare, invisible);

picture yinyang(pair center, real radius) {
    picture p;
    fill(p, unitcircle, white);
    fill(p, arc(0, S, N) -- cycle, black);
    fill(p, circle(N/2, 1/2), white);
    fill(p, circle(S/2, 1/2), black);
    fill(p, circle(N/2, 1/5), black);
    fill(p, circle(S/2, 1/5), white);
    draw(p, unitcircle, linewidth((1/32) * inch) + gray(0.5));
    return shift(center) * scale(radius) * p;
}

add(yinyang((1 + 1/4, 4 + 3/4), 1));
add(yinyang((3 + 3/4, 2 + 1/4), 2));
```



## AutoHotkey

[[file:yin-yang-ahk.png|right]]
Requires the GDI+ Standard Library by tic: http://www.autohotkey.com/forum/viewtopic.php?t=32238

```AHK
Yin_and_Yang(50,  50, A_ScriptDir "\YinYang1.png")
Yin_and_Yang(300, 300,A_ScriptDir "\YinYang2.png")

Yin_and_Yang(width, height, fileName
	, color1=0xFFFFFFFF, color2=0xFF000000, outlineWidth=1){

	pToken 	 := gdip_Startup()
	pBitmap	 := gdip_CreateBitmap(w := width, h := height)
	w-=1, h-=1
	pGraphics:= gdip_GraphicsFromImage(pBitmap)
	pBrushW	 := gdip_BrushCreateSolid(color1)
	pBrushB	 := gdip_BrushCreateSolid(color2)

	gdip_SetSmoothingMode(pGraphics, 4) 			; Antialiasing

	If (outlineWidth){
		pPen := gdip_CreatePen(0xFF000000, outlineWidth)
		gdip_DrawEllipse(pGraphics, pPen, 0, 0, w, h)
		gdip_DeletePen(pPen)
	}

	gdip_FillPie(pGraphics, pBrushB, 0, 0, w, h, -90, 180)
	gdip_FillPie(pGraphics, pBrushW, 0, 0, w, h,  90, 180)
	gdip_FillEllipse(pGraphics, pBrushB, w//4, h//2, w//2, h//2)
	gdip_FillEllipse(pGraphics, pBrushW, w//4, 0   , w//2, h//2)
	gdip_FillEllipse(pGraphics, pBrushB, 5*w//12, h//6, w//6, h//6)
	gdip_FillEllipse(pGraphics, pBrushW, 5*w//12, 4*h//6,w//6,h//6)

	r := gdip_SaveBitmapToFile(pBitmap, filename)

	; cleanup:
	gdip_DeleteBrush(pBrushW), gdip_deleteBrush(pBrushB)
	gdip_DisposeImage(pBitmap)
	gdip_DeleteGraphics(pGraphics)
	gdip_Shutdown(pToken)
	return r
}
```


## AWK


```AWK

# syntax: GAWK -f YIN_AND_YANG.AWK
# converted from PHL
BEGIN {
    yin_and_yang(16)
    yin_and_yang(8)
    exit(0)
}
function yin_and_yang(radius,  black,white,scale_x,scale_y,sx,sy,x,y) {
    black = "#"
    white = "."
    scale_x = 2
    scale_y = 1
    for (sy = radius*scale_y; sy >= -(radius*scale_y); sy--) {
      for (sx = -(radius*scale_x); sx <= radius*scale_x; sx++) {
        x = sx / scale_x
        y = sy / scale_y
        if (in_big_circle(radius,x,y)) {
          if (in_white_semi_circle(radius,x,y)) {
            printf("%s",(in_small_black_circle(radius,x,y)) ? black : white)
          }
          else if (in_black_semi_circle(radius,x,y)) {
            printf("%s",(in_small_white_circle(radius,x,y)) ? white : black)
          }
          else {
            printf("%s",(x<0) ? white : black)
          }
        }
        else {
          printf(" ")
        }
      }
      printf("\n")
    }
}
function in_circle(center_x,center_y,radius,x,y) {
    return (x-center_x)*(x-center_x)+(y-center_y)*(y-center_y) <= radius*radius
}
function in_big_circle(radius,x,y) {
    return in_circle(0,0,radius,x,y)
}
function in_black_semi_circle(radius,x,y) {
    return in_circle(0,0-radius/2,radius/2,x,y)
}
function in_white_semi_circle(radius,x,y) {
    return in_circle(0,radius/2,radius/2,x,y)
}
function in_small_black_circle(radius,x,y) {
    return in_circle(0,radius/2,radius/6,x,y)
}
function in_small_white_circle(radius,x,y) {
    return in_circle(0,0-radius/2,radius/6,x,y)
}

```

{{out}}

```txt

                                .
                     ...................####
                 ..........................#####
              ...............................######
           ...................................########
         ......................................#########
        .....................#######............#########
      ......................#########...........###########
     ......................###########...........###########
    ........................#########...........#############
   ..........................#######............##############
  .............................................################
  ............................................#################
 ............................................###################
 ..........................................#####################
 .......................................########################
.................................################################
 ........................#######################################
 .....................##########################################
 ...................############################################
  .................############################################
  ................#############################################
   ..............############.......##########################
    .............###########.........########################
     ...........###########...........######################
      ...........###########.........######################
        .........############.......#####################
         .........######################################
           ........###################################
              ......###############################
                 .....##########################
                     ....###################
                                #
                .
         .............##
      .................####
    ...........###......#####
   ...........#####......#####
  .............###......#######
 ......................#########
 .....................##########
.................################
 ..........#####################
 .........######################
  .......######...#############
   .....######.....###########
    .....######...###########
      ....#################
         ..#############
                #

```



## BASIC

=
## Applesoft BASIC
=

```ApplesoftBasic
0 GOTO 6
1Y=R:D=1-R:X=0:FORC=0TO1STEP0:M=D>=0:Y=Y-M:D=D-Y*2*M:D=D+X*2+3:HPLOTXC-X,YC+YTOXC+X,YC+Y:HPLOTXC-Y,YC+XTOXC+Y,YC+X:HPLOTXC-X,YC-YTOXC+X,YC-Y:HPLOTXC-Y,YC-XTOXC+Y,YC-X:X=X+1:C=X>=Y:NEXTC:RETURN
2Y=R:D=1-R:X=0:FORC=0TO1STEP0:M=D>=0:Y=Y-M:D=D-Y*2*M:D=D+X*2+3:HPLOTXC-X,YC+Y:HPLOTXC+X,YC+Y:HPLOTXC-Y,YC+X:HPLOTXC+Y,YC+X:HPLOTXC-X,YC-Y:HPLOTXC+X,YC-Y:HPLOTXC-Y,YC-X:HPLOTXC+Y,YC-X:X=X+1:C=X>=Y:NEXTC:RETURN
3Y=R:D=1-R:X=0:FORC=0TO1STEP0:M=D>=0:Y=Y-M:D=D-Y*2*M:D=D+X*2+3:HPLOTXC,YC+YTOXC+X,YC+Y:HPLOTXC,YC+XTOXC+Y,YC+X:HPLOTXC,YC-YTOXC+X,YC-Y:HPLOTXC,YC-XTOXC+Y,YC-X:X=X+1:C=X>=Y:NEXTC:RETURN

6 HGR2 : HCOLOR = 3 : HPLOT 0,0 : CALL 62454
7 XC = 60 : YC = 60 : R = 30 : GOSUB 100YINYANG
8 XC = 180 : YC = 80 : R = 60 : GOSUB 100YINYANG
9 END

100 YP = YC : S = R
110 HCOLOR = 0: GOSUB 3FILLHALFCIRCLE
120 HCOLOR = 3:YC = YP - S / 2 : R = S / 2 : GOSUB 1FILLCIRCLE
130 HCOLOR = 0
140 YC = YP + S / 2 : GOSUB 1FILLCIRCLE
150 YC = YP - S / 2 : R = S / 6 : GOSUB 1FILLCIRCLE
160 HCOLOR = 3
170 YC = YP + S / 2 : GOSUB 1FILLCIRCLE
180 HCOLOR = 0 : YC = YP : R = S : GOSUB 2CIRCLE
190 RETURN
```


=
## BASIC256
=

```BASIC256

graphsize 800, 600
clg

subroutine Taijitu(x, y, r)
	color black: circle(x, y, 2*r+1)
	chord x-2*r, y-2*r, 4*r, 4*r, radians(0), radians(180)
	color white
	chord x-2*r, y-2*r, 4*r, 4*r, radians(180), radians(180)
	circle(x, y-r, r-1)
	color black: circle(x, y+r, r-1)
	circle(x, y-r, r/3)
	color white: circle(x, y+r, r/3)
end subroutine

call Taijitu(110, 110, 45)
call Taijitu(500, 300, 138)
end

```


=
## BBC BASIC
=
[[File:Yinyangbbc.gif|right]]

```bbcbasic
      PROCyinyang(200, 200, 100)
      PROCyinyang(700, 400, 300)
      END
      
      DEF PROCyinyang(xpos%, ypos%, size%)
      CIRCLE xpos%, ypos%, size%
      LINE xpos%, ypos%+size%, xpos%, ypos%-size%
      FILL xpos%+size%/2, ypos%
      CIRCLE FILL xpos%, ypos%-size%/2, size%/2+2
      GCOL 15
      CIRCLE FILL xpos%, ypos%+size%/2, size%/2+2
      CIRCLE FILL xpos%, ypos%-size%/2, size%/6+2
      GCOL 0
      CIRCLE FILL xpos%, ypos%+size%/2, size%/6+2
      CIRCLE xpos%, ypos%, size%
      ENDPROC
```


=
## Commodore BASIC
=
{{works with|Commodore BASIC|7.0}}

Using the built-in graphics statements in BASIC 7.0 on the C-128:


```basic
10 COLOR 0,1:COLOR 1,2:COLOR 4,1:GRAPHIC 1,1
20 X=160:Y=100:R=80
30 CIRCLE 1,X,Y,R
40 CIRCLE 1,X,Y-R/2,R/2,R/2,0,180
50 CIRCLE 1,X,Y+R/2,R/2,R/2,180,360
60 CIRCLE 1,X,Y-R/2,R/8
70 CIRCLE 1,X,Y+R/2,R/8
80 PAINT 1,X,Y+R/2
90 PAINT 1,X-R/2,Y
```


Example of output visible  [http://i.imgur.com/0cFNmrl.png here].

=
## FreeBASIC
=

```freebasic

Screen 19
Color ,7
Cls

Sub Taijitu(x As Integer, y As Integer, r As Integer)
    Circle(x, y), 2 * r, 0,,,, F
    Line (x, y - 2 * r) - (x, y + 2 * r), 7, B
    Paint (x - r, y), 15, 7
    Circle(x, y - r), r - 1, 15,,,, F
    Circle(x, y + r), r - 1,  0,,,, F
    Circle(x, y - r), r / 3,  0,,,, F
    Circle(x, y + r), r / 3, 15,,,, F
End Sub

Taijitu(110, 110, 45)
Taijitu(500, 300, 138)
End

```


=
## Gambas
=

```gambas
Public Sub Form_Open()
Dim hPictureBox As PictureBox
Dim siCount As Short

With Me
  .Title = "Yin and yang"
  .Padding = 5
  .Height = 210
  .Width = 310
  .Arrangement = Arrange.Row
End With

For siCount = 2 DownTo 1
  hPictureBox = New PictureBox(Me)
  With hPictureBox
    .Height = siCount * 100
    .Width = siCount * 100
    .Picture = Picture.Load("../yinyang.png")
    .Stretch = True
  End With
Next

End
```


'''[http://www.cogier.com/gambas/Yin%20and%20yang_270.png Click here to view image]'''

==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "YinYang.bas"
110 GRAPHICS HIRES 2
120 SET PALETTE WHITE,BLACK
130 CALL YINYANG(200,400,150)
140 CALL YINYANG(800,340,300)
150 DEF YINYANG(X,Y,R)
160   PLOT X,Y,ELLIPSE R,R,
170   PLOT X,Y+R/2,ELLIPSE R/2,R/2,ELLIPSE R/6,R/6,PAINT
180   PLOT X,Y-R/2,ELLIPSE R/2,R/2,ELLIPSE R/6,R/6,
190   PLOT X,Y-6,PAINT,X+R/2,Y,PAINT
200   SET INK 0:PLOT X,Y+R/2,ELLIPSE R/2,R/2,
210   SET INK 1:PLOT X,Y,ELLIPSE R,R,
220 END DEF
```


=
## Liberty BASIC
=
[[File:YinYangLB.gif||200px|thumb|right|Liberty BASIC Graphic Output]]

```lb
    WindowWidth  =410
    WindowHeight =440

    open "Yin & Yang" for graphics_nf_nsb as #w

    #w "trapclose [quit]"

    call YinYang 200, 200, 200
    call YinYang 120,  50,  50

    wait

    sub YinYang x, y, size

    #w "up ; goto "; x; " "; y
    #w "backcolor black ; color black"
    #w "down ; circlefilled "; size /2

    #w "color 255 255 255 ; backcolor 255 255 255"
    #w "up   ; goto ";      x -size /2; " "; y -size /2
    #w "down ; boxfilled "; x;          " "; y +size /2

    #w "up ; goto "; x; " "; y -size /4
    #w "down ; backcolor black ; color black   ; circlefilled "; size  /4
    #w "up ; goto "; x; " "; y -size /4
    #w "down ; backcolor white ; color white ; circlefilled "; size /12

    #w "up ; goto "; x; " "; y +size /4
    #w "down ; backcolor white ; color white ; circlefilled "; size  /4
    #w "up ; goto "; x; " "; y +size /4
    #w "down ; backcolor black ; color black ; circlefilled "; size /12

    #w "up ; goto "; x; " "; y
    #w "down ; color black ; circle "; size /2

    #w "flush"

    end sub

    scan

    wait

  [quit]
    close #w
    end
```


=
## PureBasic
=
[[File:Yin And yang.png|300px]]

```PureBasic
Procedure Yin_And_Yang(x, y, radius)
  DrawingMode(#PB_2DDrawing_Outlined)
  Circle(x, y, 2 * radius, #Black)               ;outer circle
  DrawingMode(#PB_2DDrawing_Default)
  LineXY(x, y - 2 * radius, x, y + 2 * radius, #Black)
  FillArea(x + 1, y, #Black, #Black)
  Circle(x, y - radius, radius - 1, #White)
  Circle(x, y + radius, radius - 1, #Black)
  Circle(x, y - radius, radius / 3, #Black)       ;small contrasting inner circles
  Circle(x, y + radius, radius / 3, #White)
EndProcedure

If CreateImage(0, 700, 700) And StartDrawing(ImageOutput(0))
    FillArea(1, 1, -1, #White)
    Yin_And_Yang(105, 105, 50)
    Yin_And_Yang(400, 400, 148)
  StopDrawing()
  ;
  UsePNGImageEncoder()
  path$ = SaveFileRequester("Save image", "Yin And yang.png", "*.png", 0)
  If path$ <> "": SaveImage(0, path$, #PB_ImagePlugin_PNG, 0, 2): EndIf
EndIf
```


=
## VBA
=

```vb
Private Sub yinyang(Top As Integer, Left As Integer, Size As Integer)
    ActiveSheet.Shapes.AddShape(msoShapeChord, Top, Left, Size, Size).Select
    With Selection.ShapeRange
        .Adjustments.Item(1) = 90
        .Fill.ForeColor.RGB = RGB(255, 255, 255)
        .Line.ForeColor.RGB = RGB(0, 0, 0)
    End With
    ActiveSheet.Shapes.AddShape(msoShapeChord, Top, Left, Size, Size).Select
    With Selection.ShapeRange
        .Adjustments.Item(1) = 90
        .IncrementRotation 180
        .Fill.ForeColor.RGB = RGB(0, 0, 0)
        .Line.ForeColor.RGB = RGB(0, 0, 0)
    End With
    ActiveSheet.Shapes.AddShape(msoShapeOval, Top + Size \ 4, Left, Size \ 2, Size \ 2).Select
    With Selection.ShapeRange
        .Fill.ForeColor.RGB = RGB(255, 255, 255)
        .Line.ForeColor.RGB = RGB(255, 255, 255)
    End With
    ActiveSheet.Shapes.AddShape(msoShapeOval, Top + Size \ 4, Left + Size \ 2, Size \ 2, Size \ 2).Select
    With Selection.ShapeRange
        .Fill.ForeColor.RGB = RGB(0, 0, 0)
        .Line.ForeColor.RGB = RGB(0, 0, 0)
    End With
    ActiveSheet.Shapes.AddShape(msoShapeOval, Top + 5 * Size \ 12, Left + Size \ 6, Size \ 6, Size \ 6).Select
    With Selection.ShapeRange
        .Fill.ForeColor.RGB = RGB(0, 0, 0)
        .Line.ForeColor.RGB = RGB(0, 0, 0)
    End With
    ActiveSheet.Shapes.AddShape(msoShapeOval, Top + 5 * Size \ 12, Left + 2 * Size \ 3, Size \ 6, Size \ 6).Select
    With Selection.ShapeRange
        .Fill.ForeColor.RGB = RGB(255, 255, 255)
        .Line.ForeColor.RGB = RGB(255, 255, 255)
    End With
    ActiveSheet.Shapes.SelectAll
    Selection.ShapeRange.Group
End Sub
Public Sub draw()
    yinyang 200, 100, 100
    yinyang 275, 175, 25
End Sub
```


=
## Visual Basic .NET
=


### =GDI graphics=

[[File:YinYang-VBNet.png|Output of this VB.Net program]]

Shows a form with the symbols drawn on it if no command line arguments are given; otherwise, the first and only argument is an integer representing the width and height of the PNG image to generate. The raw data of the generated image is written to the console (redirect to a file to view).


```vbnet
Imports System.Drawing
Imports System.Windows.Forms

Module Program
    ''' <summary>
    ''' Draws a Taijitu symbol on the specified <see cref="Graphics" /> surface at a specified location with a specified size.
    ''' </summary>
    ''' <param name="g">The <see cref="Graphics" /> surface to draw on.</param>
    ''' <param name="location">The coordinates of the upper-left corner of the bounding rectangle that defines the symbol.</param>
    ''' <param name="diameter">The diameter of the symbol, or the width and height of its bounding rectangle.</param>
    ''' <param name="drawOutline">Whether to draw an outline around the symbol.</param>
    Sub DrawTaijitu(g As Graphics, location As PointF, diameter As Single, drawOutline As Boolean)
        Const sixth = 1 / 6

        g.ResetTransform()
        g.TranslateTransform(location.X, location.Y)
        g.ScaleTransform(diameter, diameter)

        g.FillPie(Brushes.Black, x:=0, y:=0, width:=1, height:=1, startAngle:=90, sweepAngle:=180)  ' Left half.
        g.FillPie(Brushes.White, x:=0, y:=0, width:=1, height:=1, startAngle:=270, sweepAngle:=180) ' Right half.
        g.FillEllipse(Brushes.Black, x:=0.25, y:=0, width:=0.5, height:=0.5)                        ' Upper ball.
        g.FillEllipse(Brushes.White, x:=0.25, y:=0.5, width:=0.5, height:=0.5)                      ' Lower ball.
        g.FillEllipse(Brushes.White, x:=0.5 - sixth / 2, y:=sixth, width:=sixth, height:=sixth)     ' Upper dot.
        g.FillEllipse(Brushes.Black, x:=0.5 - sixth / 2, y:=4 * sixth, width:=sixth, height:=sixth) ' Lower dot.

        If drawOutline Then
            Using p As New Pen(Color.Black, width:=2 / diameter)
                g.DrawEllipse(p, x:=0, y:=0, width:=1, height:=1)
            End Using
        End If
    End Sub

    ''' <summary>
    ''' Draws one large and one small Taijitu symbol on the specified <see cref="Graphics" /> surface.
    ''' </summary>
    ''' <param name="g">The <see cref="Graphics" /> surface to draw on.</param>
    ''' <param name="bounds">The width and height of the area to draw in.</param>
    Sub DrawDemo(g As Graphics, bounds As Single)
        Const PADDING = 10
        Dim ACTUAL = bounds - (PADDING * 2)

        g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias

        DrawTaijitu(g, location:=New PointF(PADDING, PADDING), diameter:=ACTUAL / 4, drawOutline:=True)
        DrawTaijitu(g, location:=New PointF(PADDING + (bounds / 5), PADDING + (ACTUAL / 5)), diameter:=ACTUAL * 4 / 5, drawOutline:=True)
    End Sub

    Sub Main(args As String())
        If args.Length = 0 Then
            Using frm As New YinYangForm()
                frm.ShowDialog()
            End Using

        Else
            Dim imageSize = Integer.Parse(args(0), Globalization.CultureInfo.InvariantCulture)

            Using bmp As New Bitmap(imageSize, imageSize),
                  g = Graphics.FromImage(bmp),
                  output = Console.OpenStandardOutput()

                Try
                    DrawDemo(g, imageSize)
                    bmp.Save(output, Imaging.ImageFormat.Png)
                Catch ex As Exception
                    MessageBox.Show("Specified size is too small", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            End Using
        End If
    End Sub

    Private Class YinYangForm
        Inherits Form

        Sub Form_Paint() Handles Me.Paint
            Dim availableSize = Math.Min(Me.DisplayRectangle.Width, Me.DisplayRectangle.Height)
            Dim g As Graphics
            Try
                g = Me.CreateGraphics()
                DrawDemo(g, availableSize)
            Catch ex As Exception
                MessageBox.Show("Window size too small.", "Exception thrown", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Finally
                If g IsNot Nothing Then g.Dispose()
            End Try
        End Sub
    End Class
End Module
```



### =SVG=

{{trans|zkl}}

Uses minimal string literals by favoring proper use of the .NET <code>System.Linq.Xml</code> classes (and VB.NET's XML literals, of course ;).


```vbnet
Imports System.IO

' Yep, VB.NET can import XML namespaces. All literals have xmlns changed, while xmlns:xlink is only
' declared in literals that use it directly (e.g. the output of this program has it defined in both
' of the <use /> tags and not the root, <svg />).
Imports <xmlns="http://www.w3.org/2000/svg">
Imports <xmlns:xlink="http://www.w3.org/1999/xlink">

Module Program
    Sub Main()
        Dim doc =
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg version="1.1" width="30" height="30">
    <defs>
        <g id="y">
            <circle cx="0" cy="0" r="200" stroke="black"
                fill="white" stroke-width="1"/>
            <path d="M0 -200 A 200 200 0 0 0 0 200 100 100 0 0 0 0 0 100 100 0 0 1 0 -200 z" fill="black"/>
            <circle cx="0" cy="100" r="33" fill="white"/>
            <circle cx="0" cy="-100" r="33" fill="black"/>
        </g>
    </defs>
</svg>

        ' XML literals don't support DTDs.
        Dim type As New XDocumentType(name:="svg", publicId:="-//W3C//DTD SVG 1.1//EN", systemId:="http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd", internalSubset:=Nothing)
        doc.AddFirst(type)

        Dim draw_yinyang =
            Sub(trans As Double, scale As Double) doc.Root.Add(<use xlink:href="#y" transform=<%= $"translate({trans},{trans}) scale({scale})" %>/>)

        draw_yinyang(20, 0.05)
        draw_yinyang(8, 0.02)

        Using s = Console.OpenStandardOutput(),
              sw As New StreamWriter(s)
            doc.Save(sw, SaveOptions.OmitDuplicateNamespaces)
            sw.WriteLine()
        End Using
    End Sub
End Module
```


{{out}}

```xml
<?xml version="1.0" encoding="utf-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg version="1.1" width="30" height="30" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <g id="y">
      <circle cx="0" cy="0" r="200" stroke="black" fill="white" stroke-width="1" />
      <path d="M0 -200 A 200 200 0 0 0 0 200 100 100 0 0 0 0 0 100 100 0 0 1 0 -200 z" fill="black" />
      <circle cx="0" cy="100" r="33" fill="white" />
      <circle cx="0" cy="-100" r="33" fill="black" />
    </g>
  </defs>
  <use xlink:href="#y" transform="translate(20,20) scale(0.05)" xmlns:xlink="http://www.w3.org/1999/xlink" />
  <use xlink:href="#y" transform="translate(8,8) scale(0.02)" xmlns:xlink="http://www.w3.org/1999/xlink" />
</svg>
```


====SVG (harder cheating)====
{{trans|Perl 6}}


```vbnet
Module Program
    Sub Main()
        Console.OutputEncoding = Text.Encoding.Unicode
        Dim cheat_harder = Function(scale As Integer) <span style=<%= $"font-size:{scale}%;" %>>&#x262f;</span>
        Console.WriteLine(<div><%= cheat_harder(700) %><%= cheat_harder(350) %></div>)
    End Sub
End Module
```


{{out}}

```html5><div

  <span style="font-size:700%;">☯</span>
  <span style="font-size:350%;">☯</span>
</div>
```


Rendered by RosettaCode (MediaWiki):

<div>  <span style="font-size:700%;">☯</span>  <span style="font-size:350%;">☯</span></div>

=
## Yabasic
=

```Yabasic
open window 640, 480

color 0,0,0
clear window

taijitu(640/2, 480/2, 480/4)
taijitu(100,100,50)

sub taijitu(x,y,r)
	fill circle x,y,r
	color 255,255,255
	fill circle x,y,r-4
	color 0,0,0
	line x, y-r to x, y+r
	infill(x-2, y-2)
	fill circle x,y-r/2,r/2	
	color 255,255,255
	fill circle x,y+r/2-2,r/2-1
	fill circle x,y-r/2-2,r/8-1
	color 0,0,0
	fill circle x,y+r/2-2,r/8-1
end sub

sub infill(x,y)
	local oy,lx,rx,nx,i,m,t,l$,r$,a$,test$
	test$=getbit$(x,y,x,y)		// get a sample of fill area
	oy=y-1 : lx=x : rx=x  : m=1	// m=1 makes go downwards
	for t=1 to 2
		repeat
			repeat
				l$=getbit$(lx,y,lx,y)
				lx=lx-1 : if lx<0 break 	// test how far left to go
			until (l$<>test$)
			repeat
				 r$=getbit$(rx,y,rx,y)
				 rx=rx+1 : if rx>peek("winwidth") break 	// test how far right to go
			until (r$<>test$)
			lx=lx+2 : rx=rx-2 : line lx,y to rx,y  			// draw line across fill area
			nx=0
			for i=lx to rx
				a$=getbit$(i,y+m,i,y+m)				// get sample for next line
				if a$=test$ let nx=i  : break			// test if new cycle reqd
			next i 
			lx=nx : rx=nx
			y=y+m : if (y<0 or y>peek("winheight")) break		// test how far up or down to go
		until (nx=0)
		lx=x : rx=x : y=oy : m=-1					// m=-1 makes go upwards						
	next t
end sub
```

Other solution:

```Yabasic
open window 640, 480
backcolor 255,0,0
color 0,0,0
clear window

taijitu(640/2, 480/2, 480/4)
taijitu(100,100,50)

sub taijitu(x,y,r)
	local n, x1, x2, y1, y2
	
	for n = 0 to pi*1.5 step pi/r
		x1 = x + (r / 2) * cos(n) : y1 = y + (r / 2) * sin(n)
		x2 = x - (r / 2) * cos(n) : y2 = y - (r / 2) * sin(n)
		color 0, 0, 0 : fill circle x1, y1, r/2
		color 255, 255, 255 : fill circle x1, y1, r/4
		color 255, 255, 255 : fill circle x2, y2, r/2
		color 0, 0, 0 : fill circle x2, y2, r/4
		pause .025
	next n
end sub
```


=
## ZX Spectrum Basic
=
ZX Spectrum Basic lacks a flood fill command, so we have to write a subroutine to do it for us; as such it takes a while. Recommend full speed on an emulator.

This could be done with fewer fills by defining the outline with arcs instead of circles, but it'd be just as "fast".


```zxbasic
10 CLS
20 LET i=0
30 PRINT "Recommended size is a multiple  of 4 between 40 and 80": REM smaller sizes don't render properly and larger ones don't fit
40 INPUT "Size? ";s
50 IF size>87 THEN GOTO 50: REM size check
60 INPUT "Position?";t
70 IF t<s OR t+s>254 THEN GOTO 60
80 INK i
90 CIRCLE t,s/2,s/2
100 CIRCLE t,s*1.5,s/2
110 CIRCLE t,s*1.5,s/4
120 CIRCLE t,s/2,s/4: REM we draw the big circle later
130 LET bxl=t-s/4: REM these four variables define the bounding box for the fill routine
140 LET bxr=t+s/4
150 LET byb=s*1.25+1
160 LET byt=s*1.75-1
170 GOSUB 9000: REM fill top small circle first
180 LET bxl=t-s/2
190 LET bxr=t+s/2
200 LET byb=1
210 LET byt=s-1
220 GOSUB 9000: REM lower ring
230 PLOT t,s*.75
240 DRAW OVER 1;s/2,0
250 PLOT t,s*.25
260 DRAW OVER 1;s/2,0: REM fix top and bottom edges of lower circle - the top and bottom of a ZX Basic circle are horizontal lines, which screws with the parity fill
270 CIRCLE t,s/2,s/4
280 CIRCLE t,s,s: REM now draw the big circle - it would have clashed with the ring bounding box earlier
290 LET bxl=t
300 LET bxr=t+s
310 LET byb=s+1
320 LET byt=s*1.25-1
330 GOSUB 9000: REM right half, top, lower quadrant - we have to fill it in three goes
340 LET bxl=t+s*.25+1
350 LET byb=byt+1
360 LET byt=s*1.75
370 GOSUB 9000: REM right half, top, right of spot - we move bxl to the right of the spot to make sure it doesn't clash
380 LET bxl=t
390 LET byb=byt+1
400 LET byt=s*2-2
410 GOSUB 9000: REM finish top right - bounding box stops two pixels short to prevent parity faults
420 LET byb=2
430 LET byt=s/4
440 GOSUB 9000: REM bottom of right side done in similar manner
450 LET bxl=t+s/4+1
460 LET byb=byt+1
470 LET byt=s*.75
480 GOSUB 9000
490 LET bxl=t
500 LET byb=byt+1
510 LET byt=s-1
520 GOSUB 9000
530 PLOT t,s
540 DRAW s-1,0: REM missing line in right side - would have messed up during the fill cycle
550 CIRCLE OVER 1;t,s*1.5,s/2: REM remove top wide circle to clear left loop
560 CIRCLE t,s,s: REM repair big circle, done!
570 INPUT "Again? ";a$
580 IF a$="y" THEN LET i=i+1: GO TO 40
590 INK 0
600 STOP

8999 REM area fill; checks along each pixel line and starts and stops PLOTting if it hits a boundary
9000 FOR y=byb TO byt
9010 LET p=0: REM parity
9020 FOR x=bxl TO bxr
9030 LET r1=POINT (x,y): REM POINT is 1 if the pixel at (x,y) is filled (INK), otherwise 0
9040 LET r2=POINT (x+1,y): REM test next point as well, in case of edges rendered as multiple pixels
9050 IF r1=1 AND r2=0 THEN LET p=p+1: IF p=2 THEN LET p=0: REM boundary check
9060 IF p=1 THEN PLOT x,y
9070 NEXT x
9080 NEXT y
9090 RETURN
```


[https://i.imgur.com/J1DK7qQl.png Resultant image at Imgur] (uses size=40 and position=40, then size=80 and position=160)


## Befunge

{{trans|PicoLisp}}
The radius is specified by the first value on the stack - set to 10 (55+) in this example.

```befunge
55+:#. 00p:2*10p:2/20p6/30p01v
@#!`g01:+1g07,+55$<v0-g010p07_
0g-20g+:*+30g:*`v ^_:2/:*:70g0
3+*:-g02-g00g07:_   0v v!`*:g0
g-20g+:*+20g:*`>v> ^ v1_:70g00
2+*:-g02-g00g07:_   1v v!`*:g0
g-:*+00g:*`#v_$:0`!0\v0_:70g00
0#+g#1,#$<  > 2 #^>#g>#04#1+#:
```


{{out}}

```txt
                   ...                   
           .................##           
       .......................####       
     .........................######     
   ................###........########   
   ..............#######........######   
 ..................###........########## 
 .............................########## 
 .............................########## 
 ...........................############ 
......................###################
 ............########################### 
 ..........############################# 
 ..........############################# 
 ..........########...################## 
   ......########.......##############   
   ........########...################   
     ......#########################     
       ....#######################       
           ..#################           
                   ###                   
```



## C

Writes to stdout a SVG file with two yin-yangs (no, it's really just that big): [[File:yinyang-C.svg]]

```C>#include <stdio.h


void draw_yinyang(int trans, double scale)
{
	printf("<use xlink:href='#y' transform='translate(%d,%d) scale(%g)'/>",
		trans, trans, scale);
}

int main()
{	printf(
	"<?xml version='1.0' encoding='UTF-8' standalone='no'?>\n"
	"<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'\n"
	"	'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>\n"
	"<svg xmlns='http://www.w3.org/2000/svg' version='1.1'\n"
	"	xmlns:xlink='http://www.w3.org/1999/xlink'\n"
	"		width='30' height='30'>\n"
	"	<defs><g id='y'>\n"
	"		<circle cx='0' cy='0' r='200' stroke='black'\n"
	"			fill='white' stroke-width='1'/>\n"
	"		<path d='M0 -200 A 200 200 0 0 0 0 200\n"
	"			100 100 0 0 0 0 0 100 100 0 0 1 0 -200\n"
	"			z' fill='black'/>\n"
	"		<circle cx='0' cy='100' r='33' fill='white'/>\n"
	"		<circle cx='0' cy='-100' r='33' fill='black'/>\n"
	"	</g></defs>\n");
	draw_yinyang(20, .05);
	draw_yinyang(8, .02);
	printf("</svg>");
	return 0;
}
```


=={{header|C sharp|C#}}==

Translation of: Visual Basic .NET
(Cleaned up)


```csharp

    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
            Paint += Form1_Paint;
        }

        private void Form1_Paint(object sender, PaintEventArgs e)
        {
            Graphics g = e.Graphics;
            g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;

            DrawTaijitu(g, new Point(50, 50), 200, true);
            DrawTaijitu(g, new Point(10, 10), 60, true);
        }

        private void DrawTaijitu(Graphics g, Point pt, int width, bool hasOutline)
        {
            g.FillPie(Brushes.Black, pt.X, pt.Y, width, width, 90, 180);
            g.FillPie(Brushes.White, pt.X, pt.Y, width, width, 270, 180);
            float headSize = Convert.ToSingle(width * 0.5);
            float headXPosition = Convert.ToSingle(pt.X + (width * 0.25));
            g.FillEllipse(Brushes.Black, headXPosition, Convert.ToSingle(pt.Y), headSize, headSize);
            g.FillEllipse(Brushes.White, headXPosition, Convert.ToSingle(pt.Y + (width * 0.5)), headSize, headSize);
            float headBlobSize = Convert.ToSingle(width * 0.125);
            float headBlobXPosition = Convert.ToSingle(pt.X + (width * 0.4375));
            g.FillEllipse(Brushes.White, headBlobXPosition, Convert.ToSingle(pt.Y + (width * 0.1875)), headBlobSize, headBlobSize);
            g.FillEllipse(Brushes.Black, headBlobXPosition, Convert.ToSingle(pt.Y + (width * 0.6875)), headBlobSize, headBlobSize);
            if (hasOutline) g.DrawEllipse(Pens.Black, pt.X, pt.Y, width, width);
        }
    }
```


{{out}}
<!-- The code worked as is. I just saved and uploaded the image generated from it. -->
<!-- Could not Upload image to Rosetta Code so I uploaded it to Wikipedia Commons -->
[[File:Yin_and_yang_problem_c_sharp.png|left|Image generated from Source Code.]]

Source Code: http://rosettacode.org/wiki/Yin_and_yang#C.23

Image: [https://upload.wikimedia.org/wikipedia/commons/a/af/Yin_and_yang_problem_c_sharp.png Yin_and_yang_problem_c_sharp.png]


## D

{{trans|Python}}

```d
import std.stdio, std.algorithm, std.array, std.math, std.range,
       std.conv, std.typecons;

string yinYang(in int n) pure /*nothrow @safe*/ {
    enum : char { empty = ' ', white = '.', black = '#' }

    const radii = [1, 3, 6].map!(i => i * n).array;
    auto ranges = radii.map!(r => iota(-r, r + 1).array).array;
    alias V = Tuple!(int,"x", int,"y");
    V[][] squares, circles;
    squares = ranges.map!(r => cartesianProduct(r, r).map!V.array).array;

    foreach (sqrPoints, const radius; zip(squares, radii))
        circles ~= sqrPoints.filter!(p => p[].hypot <= radius).array;
    auto m = squares[$ - 1].zip(empty.repeat).assocArray;
    foreach (immutable p; circles[$ - 1])
        m[p] = black;
    foreach (immutable p; circles[$ - 1])
        if (p.x > 0)
            m[p] = white;
    foreach (immutable p; circles[$ - 2]) {
        m[V(p.x, p.y + 3 * n)] = black;
        m[V(p.x, p.y - 3 * n)] = white;
    }
    foreach (immutable p; circles[$ - 3]) {
        m[V(p.x, p.y + 3 * n)] = white;
        m[V(p.x, p.y - 3 * n)] = black;
    }
    return ranges[$ - 1]
           .map!(y => ranges[$ - 1].retro.map!(x => m[V(x, y)]).text)
           .join('\n');
}

void main() {
    2.yinYang.writeln;
    1.yinYang.writeln;
}
```

{{out}}

```txt
            .            
        ........#        
      ...........##      
     .............##     
    ........#.....###    
   ........###....####   
  ........#####....####  
  .........###....#####  
 ...........#.....###### 
 .................###### 
 ................####### 
 ...............######## 
.............############
 ........############### 
 .......################ 
 ......################# 
 ......#####.########### 
  .....####...#########  
  ....####.....########  
   ....####...########   
    ...#####.########    
     ..#############     
      ..###########      
        .########        
            #            
      .      
   ......#   
  ....#..##  
 ....###..## 
 .....#..### 
 ........### 
.......######
 ...######## 
 ...##.##### 
 ..##...#### 
  ..##.####  
   .######   
      #      
```


A simpler alternative version:
{{trans|PicoLisp}}

```d
void yinYang(in int r) {
    import std.stdio, std.math;

    foreach (immutable y; -r .. r + 1) {
        foreach (immutable x; -2 * r .. 2 * r + 1) {
            enum circle = (in int c, in int r) pure nothrow @safe @nogc =>
                r ^^ 2 >= (x / 2) ^^ 2 + (y - c) ^^ 2;
            write(circle(-r / 2, r / 6) ? '#' :
                  circle( r / 2, r / 6) ? '.' :
                  circle(-r / 2, r / 2) ? '.' :
                  circle( r / 2, r / 2) ? '#' :
                  circle(     0, r    ) ? "#."[x < 0] :
                                          ' ');
        }
        writeln;
    }
}

void main() {
    16.yinYang;
}
```

{{out}}

```txt
                               ...                               
                     ...................####                     
                 ...........................####                 
             .................................######             
           ...................................########           
         .......................................########         
       ........................###..............##########       
     ........................#######............############     
     ......................###########............##########     
   ..........................#######............##############   
   ............................###..............##############   
 ...............................................################ 
 .............................................################## 
 .............................................################## 
 ...........................................#################### 
 .......................................######################## 
..................................###############################
 ........................####################################### 
 ....................########################################### 
 ..................############################################# 
 ..................############################################# 
 ................############################################### 
   ..............##############...############################   
   ..............############.......##########################   
     ..........############...........######################     
     ............############.......########################     
       ..........##############...########################       
         ........#######################################         
           ........###################################           
             ......#################################             
                 ....###########################                 
                     ....###################                     
                               ###                               
```



## Delphi

Instructions: Create an empty project. Paste code below and adjust the interface section for the form. Then assign 'FormCreate' to TForm1.OnCreate and 'FormPaint' to TForm1.OnPaint.

```delphi
procedure DrawYinAndYang(Canv: TCanvas; R: TRect);
begin
  Canv.Brush.Color := clWhite;
  Canv.Pen.Color := clWhite;
  Canv.Pie(R.Left, R.Top, R.Right, R.Bottom,
    (R.Right + R.Left) div 2, R.Top, (R.Right + R.Left) div 2, R.Bottom);
  Canv.Brush.Color := clBlack;
  Canv.Pen.Color := clBlack;
  Canv.Pie(R.Left, R.Top, R.Right, R.Bottom,
    (R.Right + R.Left) div 2, R.Bottom, (R.Right + R.Left) div 2, R.Top);
  Canv.Brush.Color := clWhite;
  Canv.Pen.Color := clWhite;
  Canv.Ellipse((R.Right + 3 * R.Left) div 4, R.Top,
    (3 * R.Right + R.Left) div 4, (R.Top + R.Bottom) div 2);
  Canv.Brush.Color := clBlack;
  Canv.Pen.Color := clBlack;
  Canv.Ellipse((R.Right + 3 * R.Left) div 4, (R.Top + R.Bottom) div 2,
    (3 * R.Right + R.Left) div 4, R.Bottom);

  Canv.Brush.Color := clWhite;
  Canv.Pen.Color := clWhite;
  Canv.Ellipse((7 * R.Right + 9 * R.Left) div 16, (11 * R.Bottom + 5 * R.Top) div 16,
    (9 * R.Right + 7 * R.Left) div 16, (13 * R.Bottom + 3 * R.Top) div 16);
  Canv.Brush.Color := clBlack;
  Canv.Pen.Color := clBlack;
  Canv.Ellipse((7 * R.Right + 9 * R.Left) div 16, (3 * R.Bottom + 13 * R.Top) div 16,
    (9 * R.Right + 7 * R.Left) div 16, (5 * R.Bottom + 11 * R.Top) div 16);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClientWidth := 400;
  ClientHeight := 400;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := clGray;
  Canvas.FillRect(R);

  InflateRect(R, -50, -50);
  OffsetRect(R, -40, -40);
  DrawYinAndYang(Canvas, R);

  InflateRect(R, -90, -90);
  OffsetRect(R, 170, 170);
  DrawYinAndYang(Canvas, R);
end;

```


{{output?}}


## DWScript


{{Trans|D}}

```delphi
type
   TColorFuncX = function (x : Integer) : Integer;

type
   TSquareBoard = class
      Scale : Integer;
      Pix : array of array of Integer;

      constructor Create(aScale : Integer);
      begin
         Scale := aScale;
         Pix := new Integer[aScale*12+1, aScale*12+1];
      end;

      method Print;
      begin
         var i, j : Integer;
         for i:=0 to Pix.High do begin
            for j:=0 to Pix.High do begin
               case Pix[j, i] of
                  1 : Print('.');
                  2 : Print('#');
               else
                  Print(' ');
               end;
            end;
            PrintLn('');
         end;
      end;

      method DrawCircle(cx, cy, cr : Integer; color : TColorFuncX);
      begin
         var rr := Sqr(cr*Scale);
         var x, y : Integer;
         for x := 0 to Pix.High do begin
            for y := 0 to Pix.High do begin
               if Sqr(x-cx*Scale)+Sqr(y-cy*Scale)<=rr then
                  Pix[x, y] := color(x);
            end;
         end;
      end;

      method ColorHalf(x : Integer) : Integer;
      begin
         if (x<6*Scale) then
            Result:=1
         else Result:=2;
      end;

      method ColorYin(x : Integer) : Integer;
      begin
         Result:=2;
      end;

      method ColorYang(x : Integer) : Integer;
      begin
         Result:=1;
      end;

      method YinYang;
      begin
         DrawCircle(6, 6, 6, ColorHalf);
         DrawCircle(6, 3, 3, ColorYang);
         DrawCircle(6, 9, 3, ColorYin);
         DrawCircle(6, 9, 1, ColorYang);
         DrawCircle(6, 3, 1, ColorYin);
      end;

   end;

var sq := new TSquareBoard(2);
sq.YinYang;
sq.Print;

sq := new TSquareBoard(1);
sq.YinYang;
sq.Print;
```

{{out}}

```txt

            .            
        ........#        
      ...........##      
     .............##     
    ........#.....###    
   ........###....####   
  ........#####....####  
  .........###....#####  
 ...........#.....###### 
 .................###### 
 ................####### 
 ...............######## 
............#############
 ........############### 
 .......################ 
 ......################# 
 ......#####.########### 
  .....####...#########  
  ....####.....########  
   ....####...########   
    ...#####.########    
     ..#############     
      ..###########      
        .########        
            #            
      .      
   ......#   
  ....#..##  
 ....###..## 
 .....#..### 
 ........### 
......#######
 ...######## 
 ...##.##### 
 ..##...#### 
  ..##.####  
   .######   
      #      

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Yin_and_yang this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go

There are some emerging third-party 2D graphics libraries for Go; meanwhile, here is an SVG solution using only standard libraries.
[[file:GoYinYang.svg|right]]

```go
package main

import (
    "fmt"
    "os"
    "text/template"
)

var tmpl = `<?xml version="1.0"?>
<svg xmlns="http://www.w3.org/2000/svg"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    width="210" height="150">
<symbol id="yy" viewBox="0 0 200 200">
<circle stroke="black" stroke-width="2" fill="white"
    cx="100" cy="100" r="99" />
<path fill="black"
    d="M100 100 a49 49 0 0 0 0 -98
    v-1 a99 99 0 0 1 0 198
    v-1 a49 49 0 0 1 0 -98" />
<circle fill="black" cx="100" cy="51" r="17" />
<circle fill="white" cx="100" cy="149" r="17" />
</symbol>
{{range .}}<use xlink:href="#yy"
    x="{{.X}}" y="{{.Y}}" width="{{.Sz}}" height="{{.Sz}}"/>
{{end}}</svg>
`

// structure specifies position and size to draw symbol
type xysz struct {
    X, Y, Sz int
}

// example data to specify drawing the symbol twice,
// with different position and size. 
var yys = []xysz{
    {20, 20, 100},
    {140, 30, 60},
}

func main() {
    xt := template.New("")
    template.Must(xt.Parse(tmpl))
    f, err := os.Create("yy.svg")
    if err != nil {
        fmt.Println(err)
        return
    }
    if err := xt.Execute(f, yys); err != nil {
        fmt.Println(err)
    }
    f.Close()
}
```



## Haskell

[[File:YinYang-Haskell.svg|thumb|Yin and Yang Haskell SVG output.]]
This program uses the [http://hackage.haskell.org/package/diagrams diagrams] package to produce the Yin and Yang image.  
The package implements an embedded [http://en.wikipedia.org/wiki/EDSL#Usage_patterns DSL] for producing vector graphics.  
Depending on the command-line arguments, the program can generate SVG, PNG, PDF or PostScript output.  
The sample output was created with the command <tt>yinyang -o YinYang-Haskell.svg</tt>.

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

yinyang = lw 0 $
          perim # lw 0.003 <>
          torus white black # xform id <>
          torus black white # xform negate <>
          clipBy perim base
  where perim      = arc 0 (360 :: Deg) # scale (1/2)
        torus c c' = circle (1/3) # fc c' <> circle 1 # fc c
        xform f    = translateY (f (1/4)) . scale (1/4)
        base       = rect (1/2) 1 # fc white ||| rect (1/2) 1 # fc black

main = defaultMain $ 
       pad 1.1 $ 
       beside (2,-1) yinyang (yinyang # scale (1/4))
```


=={{header|Icon}} and {{header|Unicon}}==
[[File:YinYang-unicon.PNG|thumb|Sample Output]]

```Icon
link graphics

procedure main() 
YinYang(100)
YinYang(40,"blue","yellow","white")
WDone()  # quit on Q/q
end

procedure YinYang(R,lhs,rhs,bg)   # draw YinYang with radius of R pixels and ...
/lhs := "white"                   # left hand side 
/rhs := "black"                   # right hand side
/bg  := "grey"                    # background

wsize  := 2*(C := R + (margin := R/5))

W := WOpen("size="||wsize||","||wsize,"bg="||bg) | stop("Unable to open Window")
WAttrib(W,"fg="||lhs) & FillCircle(W,C,C,R,+dtor(90),dtor(180))        # main halves
WAttrib(W,"fg="||rhs) & FillCircle(W,C,C,R,-dtor(90),dtor(180))     
WAttrib(W,"fg="||lhs) & FillCircle(W,C,C+R/2,R/2,-dtor(90),dtor(180))  # sub halves
WAttrib(W,"fg="||rhs) & FillCircle(W,C,C-R/2,R/2,dtor(90),dtor(180))
WAttrib(W,"fg="||lhs) & FillCircle(W,C,C-R/2,R/8)                      # dots
WAttrib(W,"fg="||rhs) & FillCircle(W,C,C+R/2,R/8)
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/gprocs/graphics.icn graphics.icn provides graphical procedures]


## J


Based on the Python implementation:


```j
yinyang=:3 :0
  radii=. y*1 3 6
  ranges=. i:each radii
  squares=. ,"0/~each ranges
  circles=. radii ([ >: +/"1&.:*:@])each squares
  cInds=. ({:radii) +each circles #&(,/)each squares

  M=. ' *.' {~  circles (*  1 + 0 >: {:"1)&(_1&{::) squares
  offset=. 3*y,0
  M=. '*' ((_2 {:: cInds) <@:+"1 offset)} M
  M=. '.' ((_2 {:: cInds) <@:-"1 offset)} M
  M=. '.' ((_3 {:: cInds) <@:+"1 offset)} M
  M=. '*' ((_3 {:: cInds) <@:-"1 offset)} M
)
```


Note: although the structure of this program is based on the python implementation, some [[Yin_and_yang/J|details]] are different.  In particular, in the python implementation, the elements of squares and circles have no x,y structure -- they are flat list of coordinates.  

Here, the three <code>squares</code> are each 3 dimensional arrays.  The first two dimensions correspond to the x and y values and the last dimension is 2 (the first value being the y coordinate and the second being the x coordinate -- having the dimensions as y,x pairs like this works because in J the first dimension of a matrix is the number of rows and the second dimension is the number of columns).

Also, the three elements in the variable <code>circles</code> are represented by 2 dimensional arrays.  The dimensions correspond to x and y values and the values are bits -- 1 if the corresponding coordinate pair in squares is a member of the circle and 0 if not.

Finally, the variable <code>cInds</code> corresponds very closely to the variable <code>circles</code> in the python code.  Except, instead of having y and x values, cInds has indices into <code>M</code>.  In other words, I added the last value from radii to the y and x values.  In other words, instead of having values in the range -18..18, I would have values in the range 0..36 (but replace 18 and 36 with whatever values are appropriate).

Example use:

<lang>   yinyang 1
      .      
   ......*   
  ....*..**  
 ....***..** 
 .....*..*** 
 ........*** 
.......******
 ...******** 
 ...**.***** 
 ..**...**** 
  ..**.****  
   .******   
      *      
   yinyang 2
            .            
        ........*        
      ...........**      
     .............**     
    ........*.....***    
   ........***....****   
  ........*****....****  
  .........***....*****  
 ...........*.....****** 
 .................****** 
 ................******* 
 ...............******** 
.............************
 ........*************** 
 .......**************** 
 ......***************** 
 ......*****.*********** 
  .....****...*********  
  ....****.....********  
   ....****...********   
    ...*****.********    
     ..*************     
      ..***********      
        .********        
            *            
```



## Java


### Graphical

This example shows how to draw using the built in graphics context of Java.
[[File:Java-yinyang-80.png | right]]
[[File:Java-yinyang-240.png | right]]


```java
package org.rosettacode.yinandyang;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

public class YinYangGenerator
{
    private final int size;

    public YinYangGenerator(final int size)
    {
        this.size = size;
    }

    /**
     *  Draw a yin yang symbol on the given graphics context.
     */
    public void drawYinYang(final Graphics graphics)
    {
        // Preserve the color for the caller
        final Color colorSave = graphics.getColor();

        graphics.setColor(Color.WHITE);
        // Use fillOval to draw a filled in circle
        graphics.fillOval(0, 0, size-1, size-1);
        
        graphics.setColor(Color.BLACK);
        // Use fillArc to draw part of a filled in circle
        graphics.fillArc(0, 0, size-1, size-1, 270, 180);
        graphics.fillOval(size/4, size/2, size/2, size/2);
        
        graphics.setColor(Color.WHITE);
        graphics.fillOval(size/4, 0, size/2, size/2);
        graphics.fillOval(7*size/16, 11*size/16, size/8, size/8);

        graphics.setColor(Color.BLACK);
        graphics.fillOval(7*size/16, 3*size/16, size/8, size/8);
        // Use drawOval to draw an empty circle for the outside border
        graphics.drawOval(0, 0, size-1, size-1);
        
        // Restore the color for the caller
        graphics.setColor(colorSave);
    }

    /**
     *  Create an image containing a yin yang symbol.
     */
    public Image createImage(final Color bg)
    {
        // A BufferedImage creates the image in memory
        final BufferedImage image = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB);
        // Get the graphics object for the image; note in many
        // applications you actually use Graphics2D for the 
        // additional API calls
        final Graphics graphics = image.getGraphics();
        // Color in the background of the image
        graphics.setColor(bg);
        graphics.fillRect(0,0,size,size);
        drawYinYang(graphics);
        return image;
    }

    public static void main(final String args[])
    {
        final int size = Integer.parseInt(args[0]);
        final YinYangGenerator generator = new YinYangGenerator(size);

        final JFrame frame = new JFrame("Yin Yang Generator");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        final Image yinYang = generator.createImage(frame.getBackground());
        // Use JLabel to display an image
        frame.add(new JLabel(new ImageIcon(yinYang)));
        frame.pack();
        frame.setVisible(true);
    }
}
```



### Text

{{trans|PicoLisp}}
{{works with|Java|1.8}}


```java
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.function.BooleanSupplier;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.Collections.singletonMap;

public interface YinYang {
  public static boolean circle(
    int x,
    int y,
    int c,
    int r
  ) {
    return
      (r * r) >=
        ((x = x / 2) * x)
         + ((y = y - c) * y)
    ;
  }

  public static String pixel(int x, int y, int r) {
    return Stream.<Map<BooleanSupplier, Supplier<String>>>of(
      singletonMap(
        () -> circle(x, y, -r / 2, r / 6),
        () -> "#"
      ),
      singletonMap(
        () -> circle(x, y, r / 2, r / 6),
        () -> "."
      ),
      singletonMap(
        () -> circle(x, y, -r / 2, r / 2),
        () -> "."
      ),
      singletonMap(
        () -> circle(x, y, r / 2, r / 2),
        () -> "#"
      ),
      singletonMap(
        () -> circle(x, y, 0, r),
        () -> x < 0 ? "." : "#"
      )
    )
      .sequential()
      .map(Map::entrySet)
      .flatMap(Collection::stream)
      .filter(e -> e.getKey().getAsBoolean())
      .map(Map.Entry::getValue)
      .map(Supplier::get)
      .findAny()
      .orElse(" ")
    ;
  }

  public static void yinYang(int r) {
    IntStream.rangeClosed(-r, r)
      .mapToObj(
        y ->
          IntStream.rangeClosed(
            0 - r - r,
            r + r
          )
            .mapToObj(x -> pixel(x, y, r))
            .reduce("", String::concat)
      )
      .forEach(System.out::println)
    ;
  }

  public static void main(String... arguments) {
    Optional.of(arguments)
      .filter(a -> a.length == 1)
      .map(a -> a[0])
      .map(Integer::parseInt)
      .ifPresent(YinYang::yinYang)
    ;
  }
}

```


Test:

```txt

> java YinYang 18
                                   ...
                         .....................##
                   .............................######
                 .................................######
             .......................................########
           ...........................................########
         ..........................###................##########
       ........................###########............############
       ........................###########............############
     ........................###############............############
   ............................###########............################
   ............................###########............################
   ................................###................################
 .....................................................##################
 ...................................................####################
 .................................................######################
 ...............................................########################
 .............................................##########################
......................................###################################
 ..........................#############################################
 ........................###############################################
 ......................#################################################
 ....................###################################################
 ..................#####################################################
   ................################...################################
   ................############...........############################
   ................############...........############################
     ............############...............########################
       ............############...........########################
       ............############...........########################
         ..........################...##########################
           ........###########################################
             ........#######################################
                 ......#################################
                   ......#############################
                         ..#####################
                                   ###

```



## JavaScript

Another way, a more JavaScript-style way.

```JavaScript

function Arc(posX,posY,radius,startAngle,endAngle,color){//Angle in radians.
this.posX=posX;
this.posY=posY;
this.radius=radius;
this.startAngle=startAngle;
this.endAngle=endAngle;
this.color=color;
}
//0,0 is the top left of the screen
var YingYang=[
new Arc(0.5,0.5,1,0.5*Math.PI,1.5*Math.PI,"white"),//Half white semi-circle
new Arc(0.5,0.5,1,1.5*Math.PI,0.5*Math.PI,"black"),//Half black semi-circle
new Arc(0.5,0.25,.5,0,2*Math.PI,"black"),//black circle
new Arc(0.5,0.75,.5,0,2*Math.PI,"white"),//white circle
new Arc(0.5,0.25,1/6,0,2*Math.PI,"white"),//small white circle
new Arc(0.5,0.75,1/6,0,2*Math.PI,"black")//small black circle
]
//Ying Yang is DONE!
//Now we'll have to draw it.
//We'll draw it in a matrix that way we can get results graphically or by text!
function Array2D(width,height){
this.height=height;
this.width=width;
this.array2d=[];
for(var i=0;i<this.height;i++){
this.array2d.push(new Array(this.width));
}
}
Array2D.prototype.resize=function(width,height){//This is expensive
//nheight and nwidth is the difference of the new and old height
var nheight=height-this.height,nwidth=width-this.width;
if(nwidth>0){
for(var i=0;i<this.height;i++){
if(i<height)
Array.prototype.push.apply(this.array2d[i],new Array(nwidth));
}
}
else if(nwidth<0){
for(var i=0;i<this.height;i++){
if(i<height)
 this.array2d[i].splice(width,nwidth);
}
}
if(nheight>0){
 Array.prototype.push.apply(this.array2d,new Array(width));
}
else if(nheight<0){
 this.array2d.splice(height,nheight)
}
}
Array2D.prototype.loop=function(callback){
for(var i=0;i<this.height;i++)
 for(var i2=0;i2<this.width;i++)
   callback.call(this,this.array2d[i][i2],i,i2);

}
var mat=new Array2D(100,100);//this sounds fine;
YingYang[0];
//In construction.

```



### Text

{{trans|ALGOL 68}}

```JavaScript
YinYang = (function () {
  var scale_x = 2,
    scale_y = 1,
    black = "#",
    white = ".",
    clear = " ",
    out = "";

  function draw(radius) {
    function inCircle(centre_x, centre_y, radius, x, y) {
      return Math.pow(x - centre_x, 2) + Math.pow(y - centre_y, 2) <= Math.pow(radius, 2)
    }
    var bigCircle = function (x, y) {
      return inCircle(0, 0, radius, x, y)
    }, whiteSemiCircle = function (x, y) {
        return inCircle(0, radius / 2, radius / 2, x, y)
      }, smallBlackCircle = function (x, y) {
        return inCircle(0, radius / 2, radius / 6, x, y)
      }, blackSemiCircle = function (x, y) {
        return inCircle(0, -radius / 2, radius / 2, x, y)
      }, smallWhiteCircle = function (x, y) {
        return inCircle(0, -radius / 2, radius / 6, x, y)
      };
    i = 0
    for (var sy = Math.round(radius * scale_y); sy >= -Math.round(radius * scale_y); sy--) {
      //console.log(sy)
      for (var sx = -Math.round(radius * scale_x); sx <= Math.round(radius * scale_x); sx++) {

        var x = sx / scale_x,
          y = sy / scale_y;
        //out+=sx
        //console.log(sx,bigCircle(x,y))
        if (bigCircle(x, y)) {
          //out+="";
          if (whiteSemiCircle(x, y)) {
            //console.log(x,y)
            if (smallBlackCircle(x, y)) {
              out += black
            } else {
              out += white
            }
          } else if (blackSemiCircle(x, y)) {
            if (smallWhiteCircle(x, y)) {
              out += white
            } else {
              out += black
            }
          } else if (x < 0) {
            out += white
          } else {
            out += black
          }

        } else {
          out += clear;
        }

      }
      out += "\n";
    }
    return out;
  }
  return draw
})()
console.log(YinYang(17))
console.log(YinYang(8))
```



### SVG

JavaScript is amazing in this case for the reason 
that it can be embedded in SVG itself!
This is a SVG embedded in a HTML document; 
it can be isolated from the HTML document too, making it a standalone SVG

```JavaScript
<!DOCTYPE html>
<html>

<head>

  <body>
    <svg
    id="svg"
    xmlns="http://www.w3.org/2000/svg"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    version="1.1"
    width="100%"
    height="100%">
      </svg>
      <script>
function makeElem(elemName, attribs) { //atribs must be an Object
  var e = document.createElementNS("http://www.w3.org/2000/svg", elemName),
    a, b, d = attribs.style;
  for (a in attribs) {
    if (attribs.hasOwnProperty(a)) {

      if (a == 'style') {
        for (b in d) {
          if (d.hasOwnProperty(b)) {
            e.style[b] = d[b];
          }
        }
        continue;
      }
      e.setAttributeNS(null, a, attribs[a]);
    }
  }
  return e;
}
var svg = document.getElementById("svg");

function drawYingYang(n, x, y) {
  var d = n / 10;
  h = d * 5, q = h / 2, t = q * 3;
  //A white circle, for the bulk of the left-hand part
  svg.appendChild(makeElem("circle", {
    cx: h,
    cy: h,
    r: h,
    fill: "white"
  }));
  //A black semicircle, for the bulk of the right-hand part
  svg.appendChild(makeElem("path", {
    d: "M " + (h + x) + "," + y + " A " + q + "," + q + " -" + d * 3 + " 0,1 " + (h + x) + "," + (n + y) + " z",
    fill: "black"
  }));
  //Circles to extend each part 
  svg.appendChild(makeElem("circle", {
    cx: h + x,
    cy: q + y,
    r: q,
    fill: "white"
  }));
  svg.appendChild(makeElem("circle", {
    cx: h + x,
    cy: t + y,
    r: q,
    fill: "black"
  }));
  //The spots
  svg.appendChild(makeElem("circle", {
    cx: h + x,
    cy: q + y,
    r: d,
    fill: "black"
  }));
  svg.appendChild(makeElem("circle", {
    cx: h + x,
    cy: t + y,
    r: q,
    fill: "black"
  }));
  svg.appendChild(makeElem("circle", {
    cx: h + x,
    cy: t + y,
    r: d,
    fill: "white"
  }));
  //An outline for the whole shape
  svg.appendChild(makeElem("circle", {
    cx: h + x,
    cy: h + y,
    r: h,
    fill: "none",
    stroke: "gray",
    "stroke-width": d / 3
  }));
  if (svg.height.baseVal.valueInSpecifiedUnits < n) {
    svg.setAttributeNS(null, "height", y * 1.25 + n + "px")
  }
  //svg.appendChild(makeElem("circle",{cx:"100", cy:h, r:"40", stroke:"black", "stroke-width":"2", fill:"red"})) 
}
drawYingYang(100, 30, 30);
drawYingYang(1000, 200, 200);
      </script>
  </body>
</head>

</html>
```



## jq

{{works with|jq|1.4}}

The jq program presented here is adapted from the C version and produces the same image:
[[File:yinyang-C.svg]]

```jq

def svg:
  "<svg width='100%' height='100%' version='1.1'
        xmlns='http://www.w3.org/2000/svg'
	xmlns:xlink='http://www.w3.org/1999/xlink'>" ;

def draw_yinyang(x; scale):
  "<use xlink:href='#y' transform='translate(\(x),\(x)) scale(\(scale))'/>";

def define_yinyang:
  "<defs>
    <g id='y'>
        <circle cx='0' cy='0' r='200' stroke='black'
         fill='white' stroke-width='1'/>
        <path d='M0 -200 A 200 200 0 0 0 0 200
              100 100 0 0 0 0 0 100 100 0 0 1 0 -200
  		 z' fill='black'/>
        <circle cx='0' cy='100' r='33' fill='white'/>
        <circle cx='0' cy='-100' r='33' fill='black'/>
    </g>
  </defs>" ;

def draw:
  svg,
    define_yinyang,
    draw_yinyang(20; .05),
    draw_yinyang(8 ; .02),
  "</svg>" ;

draw
```

To view the image, store the output in a file:

```sh
$ jq -M -r -n -f yin_and_yang.jq > yin_and_yang.svg
```

The image can then be viewed in a browser.


## Julia

{{works with|Julia|0.6}}
{{trans|Python}}


```julia
function yinyang(n::Int=3)
    radii   = (i * n for i in (1, 3, 6))
    ranges  = collect(collect(-r:r) for r in radii)
    squares = collect(collect((x, y) for x in rnge, y in rnge) for rnge in ranges)
    circles = collect(collect((x, y) for (x,y) in sqrpoints if hypot(x, y) ≤ radius)
                      for (sqrpoints, radius) in zip(squares, radii))
    m = Dict((x, y) => ' ' for (x, y) in squares[end])
    for (x, y) in circles[end] m[(x, y)] = x > 0 ? '·' : '*' end
    for (x, y) in circles[end-1]
        m[(x, y + 3n)] = '*'
		m[(x, y - 3n)] = '·'
    end
    for (x, y) in circles[end-2]
        m[(x, y + 3n)] = '·'
		m[(x, y - 3n)] = '*'
    end
    return join((join(m[(x, y)]  for x in reverse(ranges[end])) for y in ranges[end]), '\n')
end

println(yinyang(4))

```



## Kotlin

This is based on the Java entry but I've adjusted the code so that the program displays big and small yin-yangs of a predetermined size in the same frame. Consequently, the program only needs to be run once and doesn't require a command line argument.

```scala
// version 1.1.2

import java.awt.Color
import java.awt.Graphics
import java.awt.Image
import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JLabel

class YinYangGenerator {
    private fun drawYinYang(size: Int, g: Graphics) {
        with(g) {      
            // Preserve the color for the caller
            val colorSave = color
            color = Color.WHITE

            // Use fillOval to draw a filled in circle
            fillOval(0, 0, size - 1, size - 1)
            color = Color.BLACK

            // Use fillArc to draw part of a filled in circle
            fillArc(0, 0, size - 1, size - 1, 270, 180)
            fillOval(size / 4, size / 2, size / 2, size / 2)
            color = Color.WHITE
            fillOval(size / 4, 0, size / 2, size / 2)
            fillOval(7 * size / 16, 11 * size / 16, size /8, size / 8)
            color = Color.BLACK
            fillOval(7 * size / 16, 3 * size / 16, size / 8, size / 8)

            // Use drawOval to draw an empty circle for the outside border
            drawOval(0, 0, size - 1, size - 1)

            // Restore the color for the caller
            color = colorSave
        }
    }

    fun createImage(size: Int, bg: Color): Image {
        // A BufferedImage creates the image in memory
        val image = BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)

        // Get the graphics object for the image
        val g = image.graphics

        // Color in the background of the image
        g.color = bg
        g.fillRect(0, 0, size, size)
        drawYinYang(size, g)
        return image
    }
}

fun main(args: Array<String>) {
    val gen = YinYangGenerator()
    val size = 400 // say    
    val p = JPanel()
    val yinYang = gen.createImage(size, p.background) 
    p.add(JLabel(ImageIcon(yinYang)))

    val size2 = size / 2 // say
    val yinYang2 = gen.createImage(size2, p.background) 
    p.add(JLabel(ImageIcon(yinYang2)))

    val f = JFrame("Big and Small Yin Yang")  
    with (f) {
        defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        add(p)
        pack()
        isVisible = true
    }
}
```



## Logo

[[File:YinYangLogo.png||200px|thumb|right|UCB Logo Graphic Output]]
{{works with|UCB_Logo|5.5}}
{{works with|MSW_Logo|6.5b}}


```logo
to taijitu :r 
  ; Draw a classic Taoist taijitu of the given radius centered on the current
  ; turtle position. The "eyes" are placed along the turtle's heading, the
  ; filled one in front, the open one behind.
 
  ; don't bother doing anything if the pen is not down
  if not pendown? [stop]
 
  ; useful derivative values
  localmake "r2 (ashift :r  -1)
  localmake "r4 (ashift :r2 -1)
  localmake "r8 (ashift :r4 -1)
 
  ; remember where we started
  localmake "start  pos
 
  ; draw outer circle
  pendown
  arc 360 :r
 
  ; draw upper half of S
  penup
  forward :r2
  pendown
  arc 180 :r2
 
  ; and filled inner eye
  arc 360 :r8
  fill
 
  ; draw lower half of S
  penup
  back :r
  pendown
  arc -180 :r2
 
  ; other inner eye
  arc  360 :r8
 
  ; fill this half of the symbol 
  penup
  forward :r4
  fill
 
  ; put the turtle back where it started
  setpos :start
  pendown
end
 
; demo code to produce image at right
clearscreen
pendown
hideturtle
taijitu 100
penup
forward 150
left 90
forward 150
pendown
taijitu 75
```



## Mathematica

[[File:Mathca.png|thumb|200px]]
Mathematica's ability to symbolically build up graphics is often underrated. The following function will create a yin-yang symbol with the parameter size indicating the diameter in multiples of 40 pixels. 

```Mathematica
YinYang[size_] := 
 Graphics[{{Circle[{0, 0}, 2]}, {Disk[{0, 0}, 
     2, {90 Degree, -90 Degree}]}, {White, Disk[{0, 1}, 1]}, {Black, 
    Disk[{0, -1}, 1]}, {Black, Disk[{0, 1}, 1/4]}, {White, 
    Disk[{0, -1}, 1/4]}}, ImageSize -> 40 size]
```



## Maple


```Maple
 
with(plottools):
with(plots):
yingyang := r -> display(
                         circle([0, 0], r), 
                         disk([0, 1/2*r], 1/10*r, colour = black), 
                         disk([0, -1/2*r], 1/10*r, colour = white), 
                         disk([0, -1/2*r], 1/2*r, colour = black), 
                         inequal({1/4*r^2 <= x^2 + (y - 1/2*r)^2, 1/4*r^2 <= x^2 + (y + 1/2*r)^2, x^2 + y^2 <= 
                                  r^2}, x = 0 .. r, y = -r .. r, grid = [100, 100], colour = black), 
                         scaling = constrained, axes = none
                         );

```



## Metapost

[[File:Mp-Yingyang.jpg||200px|thumb|right|Metapost output (once converted to jpg)]]
The "function" yinyang returns a picture (a primitive type) that can be drawn (and transformed of course in any way)

```metapost
vardef yinyang(expr u) =
  picture pic_;
  path p_;
  p_ := halfcircle scaled 2u rotated -90 --
    halfcircle scaled u rotated 90 shifted (0, 1/2u) reflectedabout ((0,1), (0,-1)) --
    halfcircle scaled u rotated -270 shifted (0, -1/2u) -- cycle;
  
  pic_ := nullpicture;
  addto pic_ contour fullcircle scaled 2u withcolor black;
  addto pic_ contour p_ withcolor white;
  addto pic_ doublepath p_ withcolor black withpen pencircle scaled 0.5mm;
  addto pic_ contour fullcircle scaled 1/3u shifted (0, 1/2u) withcolor white;
  addto pic_ contour fullcircle scaled 1/3u shifted (0, -1/2u) withcolor black;
  pic_
enddef;

beginfig(1)
  % let's create a Yin Yang symbol with a radius of 5cm
  draw yinyang(5cm) shifted (5cm, 5cm);
  % and another one, radius 2.5cm, rotated 180 degrees and translated
  draw yinyang(2.5cm) rotated 180 shifted (11cm, 11cm);
endfig;

end.
```



## NetRexx

Writes an SVG document to standard output: [[File:yinyang-NRX.svg]]
{{trans|C}}

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

say "<?xml version='1.0' encoding='UTF-8' standalone='no'?>"
say "<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'"
say "  'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>"
say "<svg xmlns='http://www.w3.org/2000/svg' version='1.1'"
say "  xmlns:xlink='http://www.w3.org/1999/xlink'"
say "  width='30' height='30'>"
say "  <defs><g id='y'>"
say "    <circle cx='0' cy='0' r='200' stroke='black'"
say "      fill='white' stroke-width='1'/>"
say "    <path d='M0 -200 A 200 200 0 0 0 0 200"
say "      100 100 0 0 0 0 0 100 100 0 0 1 0 -200"
say "      z' fill='black'/>"
say "    <circle cx='0' cy='100' r='33' fill='white'/>"
say "    <circle cx='0' cy='-100' r='33' fill='black'/>"
say "  </g></defs>"

say draw_yinyang(20, 0.05)
say draw_yinyang(8, 0.02)

say "</svg>"

return

method draw_yinyang(trans = int, scale = double) inheritable static returns String
  yy = String.format("  <use xlink:href='#y' transform='translate(%d,%d) scale(%g)'/>", -
       [Object Integer(trans), Integer(trans), Double(scale)])
  return yy
```



## OCaml



```ocaml
open Graphics

let draw_yinyang x y radius black white =
  let hr = radius / 2 in
  let sr = radius / 6 in
  set_color black;
  set_line_width 6;
  draw_circle x y radius;
  set_line_width 0;
  set_color black;
  fill_arc x y radius radius 270 450;
  set_color white;
  fill_arc x y radius radius 90 270;
  fill_arc x (y + hr) hr hr 270 450;
  set_color black;
  fill_arc x (y - hr) hr hr 90 270;
  fill_circle x (y + hr) sr;
  set_color white;
  fill_circle x (y - hr) sr

let () =
  open_graph "";
  let width = size_x()
  and height = size_y() in
  set_color (rgb 200 200 200);
  fill_rect 0 0 width height;
  let w = width / 3
  and h = height / 3 in
  let r = (min w h) / 3 in
  draw_yinyang w (h*2) (r*2) black white;
  draw_yinyang (w*2) h r blue magenta;
  ignore(read_key())
```


run with:

```txt
$ ocaml graphics.cma yinyang.ml
```



## PARI/GP


```pari
YinYang(r)={ for(y=-r,r, print(concat(apply( x->
     if( x^2+y^2>r^2, " ",
        [y<0,y>0,x>0][logint((x^2+(abs(y)-r/2)^2)<<8\r^2+1,2)\3+1], "#", "."
     ), [-r..r]
 ))))
}
```


If outside the big circle, we leave blank, else we distinguish three cases depending on D = (x/r)^2+(|y/r|-1/2)^2 or rather log_2(D)+8: Less than 3 (D < 1/32: small circles), black iff y < 0; between 3 and 6 (1/32 < D < 1/4: rings around circles), black iff y > 0; beyond 6 (D > 1/4: left or right half outside rings), black iff x > 0. In all other cases white.

For y we use a <code>for()</code> loop, for x we use <code>apply( x -> ..., [-r .. r])</code>, the anonymous function returns a character for each integer in [-r .. r], which we concatenate and print as one string, followed by a newline.


## Pascal


{{Trans|JavaScript}}

```Pascal
//Written for TU Berlin
//Compiled with fpc
Program yingyang;
Uses Math;
const
 scale_x=2;
 scale_y=1;
 black='#';
 white='.';
 clear=' ';

function inCircle(centre_x:Integer;centre_y:Integer;radius:Integer;x:Integer;y:Integer):Boolean ;
begin
inCircle:=power(x-centre_x,2)+power(y-centre_y,2)<=power(radius,2);
end;

function bigCircle(radius:Integer;x:Integer;y:Integer):Boolean ;
begin
bigCircle:=inCircle(0,0,radius,x,y);
end;

function whiteSemiCircle(radius:Integer;x:Integer;y:Integer):Boolean ;
begin
whiteSemiCircle:=inCircle(0,radius div 2 ,radius div 2,x,y);
end;


function smallBlackCircle(radius:Integer;x:Integer;y:Integer):Boolean ;
begin
smallBlackCircle:=inCircle(0,radius div 2 ,radius div 6,x,y);
end;

function blackSemiCircle(radius:Integer;x:Integer;y:Integer):Boolean ;
begin
blackSemiCircle:=inCircle(0,-radius div 2 ,radius div 2,x,y);
end;

function smallWhiteCircle(radius:Integer;x:Integer;y:Integer):Boolean ;
begin
smallWhiteCircle:=inCircle(0,-radius div 2 ,radius div 6,x,y);
end;

var
radius,sy,sx,x,y:Integer;
begin
   writeln('Please type a radius:');
   readln(radius);
   if radius<3 then begin writeln('A radius bigger than 3');halt end;
   sy:=round(radius*scale_y);
   while(sy>=-round(radius*scale_y)) do begin
      sx:=-round(radius*scale_x);
      while(sx<=round(radius*scale_x)) do begin
        x:=sx div scale_x;
        y:=sy div scale_y;
        if bigCircle(radius,x,y) then begin
                if (whiteSemiCircle(radius,x,y)) then if smallblackCircle(radius,x,y) then write(black) else write(white) else if blackSemiCircle(radius,x,y) then if smallWhiteCircle(radius,x,y) then write(white) else write(black) else if x>0 then write(white) else write(black);
                end
              else write(clear);
        sx:=sx+1
      end;
      writeln;
      sy:=sy-1;
   end;
end.



```

{{out}}

```txt

Please type a radius:
6
           ...
     ##.............
   ####....###........
 ####....#######........
 ######....###..........
 ######.................
###########..............
 #################......
 ##########...####......
 ########.......####....
   ########...####....
     #############..
           ###

Please type a radius:
10
                   ...
           ##.................
       ####.......................
     ######.........................
   ########........###................
   ######........#######..............
 ##########........###..................
 ##########.............................
 ##########.............................
 ############...........................
###################......................
 ###########################............
 #############################..........
 #############################..........
 ##################...########..........
   ##############.......########......
   ################...########........
     #########################......
       #######################....
           #################..
                   ###
   

```



## Perl

[[File:yinyang-perl.svg|thumb]]


```perl
sub circle {
        my ($radius, $cx, $cy, $fill, $stroke) = @_;
        print   "<circle cx='$cx' cy='$cy' r='$radius' ",
                "fill='$fill' stroke='$stroke' stroke-width='1'/>\n";
}

sub yin_yang {
        my ($rad, $cx, $cy, %opt) = @_;
        my ($c, $w) = (1, 0);
        $opt{fill}   //= 'white';
        $opt{stroke} //= 'black';
        $opt{recurangle} //= 0;

        print "<g transform='rotate($opt{angle}, $cx, $cy)'>"
                if $opt{angle};

        if ($opt{flip}) { ($c, $w) = ($w, $c) };

        circle($rad, $cx, $cy, $opt{fill}, $opt{stroke});

        print "<path d='M $cx ", $cy + $rad, "A ",
                $rad/2, " ", $rad/2, " 0 0 $c $cx $cy ",
                $rad/2, " ", $rad/2, " 0 0 $w $cx ", $cy - $rad, " ",
                $rad,   " ", $rad,   " 0 0 $c $cx ", $cy + $rad, " ",
                "z' fill='$opt{stroke}' stroke='none' />";

        if ($opt{recur} and $rad > 1) {
                # recursive "eyes" are slightly larger
                yin_yang($rad/4, $cx, $cy + $rad/2, %opt,
                                angle   => $opt{recurangle},
                                fill    => $opt{stroke},
                                stroke  => $opt{fill}   );
                yin_yang($rad/4, $cx, $cy - $rad/2, %opt,
                                angle   => 180 + $opt{recurangle});
        } else {
                circle($rad/5, $cx, $cy + $rad/2, $opt{fill}, $opt{stroke});
                circle($rad/5, $cx, $cy - $rad/2, $opt{stroke}, $opt{fill});
        }
        print "</g>" if $opt{angle};
}

print <<'HEAD';
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" 
        "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
        xmlns:xlink="http://www.w3.org/1999/xlink">
HEAD

yin_yang(200, 250, 250, recur=>1,
         angle=>0, recurangle=>90, fill=>'white', stroke=>'black');
yin_yang(100, 500, 500);

print "</svg>"
```


Messy code.  Note that the larger yin-yang is drawn recursively.


## Perl 6

[[File:Yin-yang-perl6.svg|thumb]]

Translation / Modification of C and Perl examples.


```perl6
sub circle ($rad, $cx, $cy, $fill = 'white', $stroke = 'black' ){
    say "<circle cx='$cx' cy='$cy' r='$rad' fill='$fill' stroke='$stroke' stroke-width='1'/>";
}

sub yin_yang ($rad, $cx, $cy, :$fill = 'white', :$stroke = 'black', :$angle = 90) {
    my ($c, $w) = (1, 0);
    say "<g transform='rotate($angle, $cx, $cy)'>" if $angle;
    circle($rad, $cx, $cy, $fill, $stroke);
    say "<path d='M $cx {$cy + $rad}A {$rad/2} {$rad/2} 0 0 $c $cx $cy ",
        "{$rad/2} {$rad/2} 0 0 $w $cx {$cy - $rad} $rad $rad 0 0 $c $cx ",
        "{$cy + $rad} z' fill='$stroke' stroke='none' />";
    circle($rad/5, $cx, $cy + $rad/2, $fill, $stroke);
    circle($rad/5, $cx, $cy - $rad/2, $stroke, $fill);
    say "</g>" if $angle;
}

say '<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg height="400" width="400" xmlns="http://www.w3.org/2000/svg" version="1.1"
 xmlns:xlink="http://www.w3.org/1999/xlink">';

yin_yang(100, 130, 130);
yin_yang(50, 300, 300);

say '</svg>';
```


Seems like something of a cheat since it relies on a web browser / 
svg image interpreter to actually view the output image. 
If that's the case, we may as well cheat harder. ;-)


```perl6
sub cheat_harder ($scale) { "<span style=\"font-size:$scale%;\">&#x262f;</span>"; }

say '<div>', cheat_harder(700), cheat_harder(350), '</div>';
```


<div><span style="font-size:700%;">&#x262f;</span><span style="font-size:350%;">&#x262f;</span></div>


## Phix

{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\Yin_and_yang.exw
--
include pGUI.e

Ihandle dlg, canvas
cdCanvas cd_canvas

procedure cdCanvasSecArc(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2) 
-- cdCanvasSector does not draw anti-aliased edges, but cdCanvasArc does, so over-draw...
    cdCanvasSector(hCdCanvas, xc, yc, w, h, angle1, angle2) 
    cdCanvasArc   (hCdCanvas, xc, yc, w, h, angle1, angle2) 
end procedure

procedure yinyang(atom  cx, cy, r)
    cdCanvasArc(cd_canvas, cx, cy, r, r, 0, 360) 
    cdCanvasSecArc(cd_canvas, cx, cy, r, r, 270, 90) 
    cdCanvasSecArc(cd_canvas, cx, cy-r/4, r/2-1, r/2-1, 0, 360) 
    cdCanvasSetForeground(cd_canvas, CD_WHITE)
    cdCanvasSecArc(cd_canvas, cx, cy+r/4, r/2-1, r/2-1, 0, 360) 
    cdCanvasSecArc(cd_canvas, cx, cy-r/4, r/8, r/8, 0, 360) 
    cdCanvasSetForeground(cd_canvas, CD_BLACK)
    cdCanvasSecArc(cd_canvas, cx, cy+r/4, r/8, r/8, 0, 360) 
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
integer {width, height} = IupGetIntInt(canvas, "DRAWSIZE")
integer r = min(width,height)-40
integer cx = floor(width/2)
integer cy = floor(height/2)
    cdCanvasActivate(cd_canvas)
    cdCanvasClear(cd_canvas) 
    yinyang(cx-r*.43,cy+r*.43,r/6)
    yinyang(cx,cy,r)
    cdCanvasFlush(cd_canvas)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    IupGLMakeCurrent(canvas)
    cd_canvas = cdCreateCanvas(CD_GL, "10x10 %g", {res})
    cdCanvasSetBackground(cd_canvas, CD_WHITE)
    cdCanvasSetForeground(cd_canvas, CD_BLACK)
    return IUP_DEFAULT
end function

function canvas_resize_cb(Ihandle /*canvas*/)
    integer {canvas_width, canvas_height} = IupGetIntInt(canvas, "DRAWSIZE")
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    cdCanvasSetAttribute(cd_canvas, "SIZE", "%dx%d %g", {canvas_width, canvas_height, res})
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupGLCanvas()
    IupSetAttribute(canvas, "RASTERSIZE", "340x340") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "RESIZE_CB", Icallback("canvas_resize_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Yin and Yang")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL) -- release the minimum limitation
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PHL


{{trans|ALGOL 68}}


```phl
module circles;

extern printf;

@Boolean in_circle(@Integer centre_x, @Integer centre_y, @Integer radius, @Integer x, @Integer y) [
	return (x-centre_x)*(x-centre_x)+(y-centre_y)*(y-centre_y) <= radius*radius;
]

@Boolean in_big_circle (@Integer radius, @Integer x, @Integer y) [
	return in_circle(0, 0, radius, x, y);
]

@Boolean in_while_semi_circle (@Integer radius, @Integer x, @Integer y) [
	return in_circle(0, radius/2, radius/2, x, y);
]

@Boolean in_small_white_circle (@Integer radius, @Integer x, @Integer y) [
	return in_circle(0, 0-radius/2, radius/6, x, y);
]

@Boolean in_black_semi_circle (@Integer radius, @Integer x, @Integer y) [
	return in_circle(0, 0-radius/2, radius/2, x, y);
]

@Boolean in_small_black_circle (@Integer radius, @Integer x, @Integer y) [
	return in_circle(0, radius/2, radius/6, x, y);
]

@Void print_yin_yang(@Integer radius) [
	var white = '.';
	var black = '#';
	var clear = ' ';

	var scale_y = 1;
	var scale_x = 2;
	for (var sy = radius*scale_y; sy >= -(radius*scale_y); sy=sy-1) {
		for (var sx = -(radius*scale_x); sx <= radius*scale_x; sx=sx+1) {
			var x = sx/(scale_x);
			var y = sy/(scale_y);
			
			if (in_big_circle(radius, x, y)) {
				if (in_while_semi_circle(radius, x, y))
					if (in_small_black_circle(radius, x, y))
						printf("%c", black);
					else
						printf("%c", white);
				else if (in_black_semi_circle(radius, x, y))
					if (in_small_white_circle(radius, x, y))
						printf("%c", white);
					else
						printf("%c", black);
				else 	if (x < 0)
						printf("%c", white);
					else
						printf("%c", black);
			} else printf("%c", clear);
		}
		printf("\n");
	}
]

@Integer main [
	print_yin_yang(17);
	print_yin_yang(8);
	return 0;
]
```


{{out}}

```txt


                                  ###                                 
                        .............##########                       
                  .........................##########                 
                ...............................########               
              ...................................########             
          .......................................############         
          .........................................##########         
        ..........................###..............############       
      ..........................#######............##############     
    ..........................###########............##############   
    ............................#######............################   
    ..............................###..............################   
  .................................................################## 
  ...............................................#################### 
  ...............................................#################### 
  .............................................###################### 
  .........................................########################## 
 ....................................#################################
  ..........................######################################### 
  ......................############################################# 
  ....................############################################### 
  ....................############################################### 
  ..................################################################# 
    ................##############...##############################   
    ................############.......############################   
    ..............############...........##########################   
      ..............############.......##########################     
        ............##############...##########################       
          ..........#########################################         
          ............#######################################         
              ........###################################             
                ........###############################               
                  ..........#########################                 
                        ..........#############                       
                                  ###                                 
                ...               
          .............##         
      ...................####     
    ............###......######   
    ..........#######......####   
  ..............###......######## 
  .......................######## 
  .....................########## 
 ..................###############
  ..........##################### 
  ........####################### 
  ........######...############## 
    ....######.......##########   
    ......######...############   
      ....###################     
          ..#############         
                ###

```



## PicoLisp


```PicoLisp
(de circle (X Y C R)
   (>=
      (* R R)
      (+
         (* (setq X (/ X 2)) X)
         (* (dec 'Y C) Y) ) ) )

(de yinYang (R)
   (for Y (range (- R) R)
      (for X (range (- 0 R R) (+ R R))
         (prin
            (cond
               ((circle X Y (- (/ R 2)) (/ R 6))
                  "#" )
               ((circle X Y (/ R 2) (/ R 6))
                  "." )
               ((circle X Y (- (/ R 2)) (/ R 2))
                  "." )
               ((circle X Y (/ R 2) (/ R 2))
                  "#" )
               ((circle X Y 0 R)
                  (if (lt0 X) "." "#") )
               (T " ") ) ) )
      (prinl) ) )
```


{{out|Test}}

```txt

: (yinYang 18)
                                   ...
                         .....................##
                   .............................######
                 .................................######
             .......................................########
           ...........................................########
         ..........................###................##########
       ........................###########............############
       ........................###########............############
     ........................###############............############
   ............................###########............################
   ............................###########............################
   ................................###................################
 .....................................................##################
 ...................................................####################
 .................................................######################
 ...............................................########################
 .............................................##########################
......................................###################################
 ..........................#############################################
 ........................###############################################
 ......................#################################################
 ....................###################################################
 ..................#####################################################
   ................################...################################
   ................############...........############################
   ................############...........############################
     ............############...............########################
       ............############...........########################
       ............############...........########################
         ..........################...##########################
           ........###########################################
             ........#######################################
                 ......#################################
                   ......#############################
                         ..#####################
                                   ###

```



## PostScript

{{out}} [[File:PSyinyang.png|thumb]]

```PostScript
%!PS-Adobe-3.0
%%BoundingBox: 0 0 400 400

/fs 10 def
/ed { exch def } def
/dist { 3 -1 roll sub dup mul 3 1 roll sub dup mul add sqrt } def
/circ {
    /r exch def
    [r neg 1 r {
        /y exch def
        [ r 2 mul neg 1 r 2 mul {
            /x ed x 2 div y 0 0 dist r .05 add gt {( )}{
                x 2 div y 0 r 2 div dist dup
                r 5 div le { pop (.) } {
                    r 2 div le { (@) }{
                        x 2 div y 0 r 2 div neg dist dup
                        r 5 div le { pop (@)} {
                            r 2 div le {(.)}{
                                x 0 le {(.)}{(@)}ifelse
                            } ifelse
                        } ifelse
                    } ifelse
                } ifelse
            } ifelse
        } for]
    } for]
} def

/dis {  moveto gsave
        {       grestore 0 fs 1.15 mul neg rmoveto gsave
                {show} forall
        } forall grestore
} def

/Courier findfont fs scalefont setfont

11 circ 10 390 dis
6 circ 220 180 dis
showpage
%%EOF
```


=={{header|POV-Ray}}==
[[File:Yys.png|thumb]]

<lang POV-Ray>
// 
### === General Scene setup ===
 
#version 3.7;
global_settings { assumed_gamma 2.2 }

camera{ location <0,2.7,4> look_at <0,.1,0> right x*1.6 
        aperture .2 focal_point <1,0,0> blur_samples 200 variance 1/10000 }
light_source{<2,4,8>, 1 spotlight point_at 0 radius 10}
sky_sphere {pigment {granite scale <1,.1,1> color_map {[0 rgb 1][1 rgb <0,.4,.6>]}}}
#default {finish {diffuse .9 reflection {.1 metallic} ambient .3}
          normal {granite scale .2}}
plane { y, -1 pigment {hexagon color rgb .7 color rgb .75 color rgb .65} 
        normal {hexagon scale 5}}

// 
### === Declare one side of the symbol as a sum and difference of discs ===
 
                                                   
#declare yang = 
difference {
  merge {
    difference {
      cylinder {0 <0,.1,0> 1}               // flat disk
      box {-1 <1,1,0>}                      // cut in half
      cylinder {<.5,-.1,0> <.5,.2,0> .5}    // remove half-cicle on one side
    }
    cylinder {<-.5,0,0> <-.5,.1,0> .5}      // add on the other side
    cylinder {<.5,0,0> <.5,.1,0> .15}       // also add a little dot
  }
  cylinder {<-.5,-.1,0> <-.5,.2,0> .15}     // and carve out a hole
  pigment{color rgb 0.1}
}

// ====== The other side is white and 180-degree turned ====== 

#declare yin = 
object {
  yang
  rotate <0,180,0>
  pigment{color rgb 1}
}

// 
### === Here we put the two together: ===
 

#macro yinyang( ysize )
  union {
    object {yin}
    object {yang}
    scale ysize   
  }
#end

// 
### === Here we put one into a scene: ===
 

object { yinyang(1)
         translate -y*1.08 }

// 
### === And a bunch more just for fun: ===
 

#declare scl=1.1;
#while (scl > 0.01)  
  
  object { yinyang(scl) 
        rotate <0,180,0> translate <-scl*4,scl*2-1,0> 
        rotate <0,scl*360,0> translate <-.5,0,0>}
        
  object { yinyang(scl) 
        translate <-scl*4,scl*2-1,0> 
        rotate <0,scl*360+180,0> translate <.5,0,0>}

  #declare scl = scl*0.85;
#end
```



## Prolog

Works with SWI-Prolog and XPCE.


```Prolog
ying_yang(N) :-
	R is N * 100,
	sformat(Title, 'Yin Yang ~w', [N]),
	new(W, window(Title)),
	new(Wh, colour(@default, 255*255, 255*255, 255*255)),
	new(Bl, colour(@default, 0, 0, 0)),
	CX is R + 50,
	CY is R + 50,
	R1 is R / 2,
	R2 is R / 8,
	CY1 is R1 + 50,
	CY2 is 3 * R1 + 50,

	new(E, semi_disk(point(CX, CY), R, w, Bl)),
	new(F, semi_disk(point(CX, CY), R, e, Wh)),
	new(D1, disk(point(CX, CY1), R, Bl)),
	new(D2, disk(point(CX, CY2), R, Wh)),
	new(D3, disk(point(CX, CY1), R2, Wh)),
	new(D4, disk(point(CX, CY2), R2, Bl)),

	send_list(W, display, [E, F, D1, D2, D3, D4]),

	WD is 2 * R + 100,
	send(W, size, size(WD, WD )),
	send(W, open).

:- pce_begin_class(semi_disk, path, "Semi disk with color ").

initialise(P, C, R, O, Col) :->
        send(P, send_super, initialise),
	get(C, x, CX),
	get(C, y, CY),
	choose(O, Deb, End),
	forall(between(Deb, End, I),
	       (   X is R * cos(I * pi/180) + CX,
		   Y is R * sin(I * pi/180) + CY,
	           send(P, append, point(X,Y)))),
	send(P, closed, @on),
	send(P, fill_pattern, Col).

:- pce_end_class.

choose(s, 0, 180).
choose(n, 180, 360).
choose(w, 90, 270).
choose(e, -90, 90).

:- pce_begin_class(disk, ellipse, "disk with color ").

initialise(P, C, R, Col) :->
        send(P, send_super, initialise, R, R),
	send(P, center, C),
	send(P, pen, 0),
	send(P, fill_pattern, Col).

:- pce_end_class.
```

{{out}}

```txt
 ?- ying_yang(1).
true.

 ?- ying_yang(2).
true.

```

[[File:Prolog-yin-yang.png|center|600px]]


## Python


### Text

For positive integer n > 0, the following generates 
an ASCII representation of the Yin yang symbol.
{{works with|Python|3.x}}

```python
import math
def yinyang(n=3):
	radii   = [i * n for i in (1, 3, 6)]
	ranges  = [list(range(-r, r+1)) for r in radii]
	squares = [[ (x,y) for x in rnge for y in rnge]
		   for rnge in ranges]
	circles = [[ (x,y) for x,y in sqrpoints
		     if math.hypot(x,y) <= radius ]
		   for sqrpoints, radius in zip(squares, radii)]
	m = {(x,y):' ' for x,y in squares[-1]}
	for x,y in circles[-1]:
		m[x,y] = '*'
	for x,y in circles[-1]:
		if x>0: m[(x,y)] = '·'
	for x,y in circles[-2]:
		m[(x,y+3*n)] = '*'
		m[(x,y-3*n)] = '·'
	for x,y in circles[-3]:
		m[(x,y+3*n)] = '·'
		m[(x,y-3*n)] = '*'
	return '\n'.join(''.join(m[(x,y)] for x in reversed(ranges[-1])) for y in ranges[-1])
```


;Sample generated symbols for <code>n = 2</code> and <code>n = 3</code>:

```txt
>>> print(yinyang(2))
            ·            
        ········*        
      ···········**      
     ·············**     
    ········*·····***    
   ········***····****   
  ········*****····****  
  ·········***····*****  
 ···········*·····****** 
 ·················****** 
 ················******* 
 ···············******** 
·············************
 ········*************** 
 ·······**************** 
 ······***************** 
 ······*****·*********** 
  ·····****···*********  
  ····****·····********  
   ····****···********   
    ···*****·********    
     ··*************     
      ··***********      
        ·********        
            *            
>>> print(yinyang(1))
      ·      
   ······*   
  ····*··**  
 ····***··** 
 ·····*··*** 
 ········*** 
·······******
 ···******** 
 ···**·***** 
 ··**···**** 
  ··**·****  
   ·******   
      *      
>>> 
```



### Turtle Graphics

This was inspired by the Logo example but diverged as some of the Python turtle graphics primitives such as filling and the drawing of arcs work differently.
[[File:Yinyang.GIF||200px|thumb|right|Python turtle graphics program output]]

```python
from turtle import *

mode('logo')

def taijitu(r): 
  '''\
  Draw a classic Taoist taijitu of the given radius centered on the current
  turtle position. The "eyes" are placed along the turtle's heading, the
  filled one in front, the open one behind.
  '''

  # useful derivative values
  r2, r4, r8 = (r >> s for s in (1, 2, 3))

  # remember where we started
  x0, y0 = start = pos()
  startcolour = color()
  startheading = heading()
  color('black', 'black')

  # draw outer circle
  pendown()
  circle(r)

  # draw two 'fishes'
  begin_fill(); circle(r, 180); circle(r2, 180); circle(-r2, 180); end_fill()

  # black 'eye'  
  setheading(0); penup(); goto(-(r4 + r8) + x0, y0); pendown()
  begin_fill(); circle(r8); end_fill()

  # white 'eye'
  color('white', 'white'); setheading(0); penup(); goto(-(r+r4+r8) + x0, y0); pendown()
  begin_fill(); circle(r8); end_fill() 

  # put the turtle back where it started
  penup()
  setpos(start)
  setheading(startheading)
  color(*startcolour)


if __name__ == '__main__': 
  # demo code to produce image at right
  reset()
  #hideturtle()
  penup()
  goto(300, 200)
  taijitu(200)
  penup()
  goto(-150, -150)
  taijitu(100)
  hideturtle()
```



## R

[[File:yin_yang.png|thumb|Output of this R program]]

```r
plot.yin.yang <- function(x=5, y=5, r=3, s=10, add=F){
	suppressMessages(require("plotrix"))
	if(!add) plot(1:10, type="n", xlim=c(0,s), ylim=c(0,s), xlab="", ylab="", xaxt="n", yaxt="n", bty="n", asp=1)
	draw.circle(x, y, r, border="white", col= "black")
	draw.ellipse(x, y, r, r, col="white", angle=0, segment=c(90,270), arc.only=F)
	draw.ellipse(x, y - r * 0.5, r * 0.5, r * 0.5, col="black", border="black", angle=0, segment=c(90,270), arc.only=F)
	draw.circle(x, y - r * 0.5, r * 0.125, border="white", col= "white")
	draw.circle(x, y + r * 0.5, r * 0.5, col="white", border="white")
	draw.circle(x, y + r * 0.5, r * 0.125, border="black", lty=1, col= "black")
	draw.circle(x, y, r, border="black")
}
png("yin_yang.png")
plot.yin.yang()
plot.yin.yang(1,7,1, add=T)
dev.off()
```



## Racket

[[File:Rosetta-yin-yang.png||200px|thumb|right]]

```racket

#lang racket
(require slideshow/pict)

(define (yin-yang d)
  (define base 
    (hc-append (inset/clip (circle d) 0 0 (- (/ d 2)) 0)
               (inset/clip (disk d) (- (/ d 2)) 0 0 0)))
  (define with-top
    (ct-superimpose
     base
     (cc-superimpose (colorize (disk (/ d 2)) "white")
                     (disk (/ d 8)))))
  (define with-bottom
    (cb-superimpose
     with-top
     (cc-superimpose (disk (/ d 2))
                     (colorize (disk (/ d 8)) "white"))))
  (cc-superimpose with-bottom (circle d)))

(yin-yang 200)

```



## Rascal

[[File:Yinyang.jpg||200px|thumb|right]]

```Rascal
import util::Math;
import vis::Render;
import vis::Figure;

public void yinyang(){
	template = ellipse(fillColor("white"));
	
	smallWhite = ellipse(fillColor("white"), shrink(0.1), valign(0.75));
	smallBlack = ellipse(fillColor("black"), shrink(0.1), valign(0.25));
	 
	dots= [ellipse(fillColor("white"), shrink(0.000001), align(0.5 + sin(0.0031415*n)/4, 0.25 + cos(0.0031415*n)/-4)) | n <- [1 .. 1000]];
	dots2 = [ellipse(fillColor("black"), shrink(0.000001), align(0.5 + sin(0.0031415*n)/-4, 0.75 + cos(0.0031415*n)/-4)) | n <- [1 .. 1000]];
	dots3= [ellipse(fillColor("black"), shrink(0.000001), align(0.5 + sin(0.0031415*n)/2, 0.5-cos(0.0031415*n)/-2)) | n <- [1 .. 1000]];
	 
	black= overlay([*dots, *dots2, *dots3], shapeConnected(true), shapeClosed(true), shapeCurved(true), fillColor("black"));
	 
	render(hcat([vcat([overlay([template, black, smallWhite, smallBlack], aspectRatio (1.0)), space(), space()]), 
	                   overlay([template, black, smallWhite, smallBlack], aspectRatio (1.0))]));
}
```



## REXX

{{trans|PHL}}
Code was added to this REXX program to try to preserve the aspect ratio when displaying the Yin-Yang symbol.

```rexx
/*REXX program creates & displays an ASCII art version of the Yin─Yang (taijitu) symbol.*/
parse arg s1 s2 .                                /*obtain optional arguments from the CL*/
if s1=='' | s1==","  then s1=17                  /*Not defined?   Then use the default. */
if s2=='' | s2==","  then s2=s1 % 2              /* "      "        "   "   "     "     */
if s1>0              then call  Yin_Yang  s1     /*create & display 1st Yin-Yang symbol.*/
if s2>0              then call  Yin_Yang  s2     /*   "   "    "    2nd    "       "    */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
in@:     procedure;  parse arg cy,r,x,y;           return x**2  +  (y-cy)**2  <=  r**2
big@:      /*in big         circle. */             return in@(  0 ,    r  ,    x,    y )
semi@:     /*in semi        circle. */             return in@( r/2,    r/2,    x,    y )
sB@:       /*in small black circle. */             return in@( r/2,    r/6,    x,    y )
sW@:       /*in small white circle. */             return in@(-r/2,    r/6,    x,    y )
Bsemi@:    /*in black semi  circle. */             return in@(-r/2,    r/2,    x,    y )
/*──────────────────────────────────────────────────────────────────────────────────────*/
Yin_Yang: procedure; parse arg r;  mY=1;  mX=2   /*aspect multiplier for the  X,Y  axis.*/
   do   sy= r*mY  to  -r*mY  by -1;      $=                         /*$ ≡ an output line*/
     do sx=-r*mX  to   r*mX;             x=sx/mX;      y=sy/mY      /*apply aspect ratio*/
     if big@() then if semi@()  then if sB@()     then $=$'Θ';                 else $=$'°'
                                else if Bsemi@()  then if sW@()  then $=$'°';  else $=$'Θ'
                                                  else if x<0    then $=$'°';  else $=$'Θ'
               else $=$' '
     end   /*sy*/
   say strip($, 'T')                             /*display one line of a Yin─Yang symbol*/
   end     /*sx*/;       return
```

{{out|output|text=  when using the inputs of:     <tt> 35   25 </tt>}}


(Shown at one-third size.)
<b>
<pre style="font-size:33%">
                                                                      °
                                                      °°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘ
                                               °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘ
                                          °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘ
                                      °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘ
                                  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘ
                               °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘ
                            °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                          °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                        °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                      °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                    °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
              °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
             °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
            °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
          °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
         °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
        °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
       °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
      °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
      °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
    °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
   °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
   °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
   °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
   °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
    °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
      °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
      °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
       °°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
        °°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
         °°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
          °°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
            °°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
             °°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
              °°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                °°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                  °°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                    °°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                      °°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                        °°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                          °°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                            °°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                               °°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                                  °°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                                      °°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                                          °°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                                               °°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                                                      °°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                                                                      Θ
                                                  °
                                    °°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘ
                               °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘ
                           °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘ
                       °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘ
                    °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘ
                  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘ
                °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘ
              °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘ
            °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
          °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
         °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
        °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
       °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
      °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
    °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
   °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
  °°°°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
   °°°°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
    °°°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
     °°°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
      °°°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
       °°°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
        °°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
         °°°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
          °°°°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
            °°°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
              °°°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                °°°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                  °°°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                    °°°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                       °°°°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                           °°°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                               °°°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                                    °°°°°ΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘΘ
                                                  Θ

```

</b>


## Ruby

[[File:yin_yang.shoes.png|thumb|Output of this Ruby Shoes program]]
{{libheader|Shoes}}

```ruby
Shoes.app(:width => 470, :height => 380) do
  PI = Shoes::TWO_PI/2

  strokewidth 1

  def yin_yang(x, y, radius)
    fill black; stroke black
    arc x, y, radius, radius, -PI/2, PI/2

    fill white; stroke white
    arc x, y, radius, radius, PI/2, -PI/2
    oval x-radius/4, y-radius/2, radius/2-1

    fill black; stroke black
    oval x-radius/4, y, radius/2-1
    oval x-radius/12, y-radius/4-radius/12, radius/6-1

    fill white; stroke white
    oval x-radius/12, y+radius/4-radius/12, radius/6-1

    nofill
    stroke black
    oval x-radius/2, y-radius/2, radius
  end

  yin_yang 190, 190, 360
  yin_yang 410, 90, 90
end
```



## Scala

{{libheader|Scala}}

```scala
import scala.swing.Swing.pair2Dimension
import scala.swing.{ MainFrame, Panel }
import java.awt.{ Color, Graphics2D }

object YinYang extends scala.swing.SimpleSwingApplication {
  var preferedSize = 500

  /** Draw a Taijitu symbol on the given graphics context.
   */
  def drawTaijitu(g: Graphics2D, size: Int) {
    val sizeMinsOne = size - 1
    // Preserve the color for the caller
    val colorSave = g.getColor()

    g.setColor(Color.WHITE)
    // Use fillOval to draw a filled in circle
    g.fillOval(0, 0, sizeMinsOne, sizeMinsOne)

    g.setColor(Color.BLACK)
    // Use fillArc to draw part of a filled in circle
    g.fillArc(0, 0, sizeMinsOne, sizeMinsOne, 270, 180)
    g.fillOval(size / 4, size / 2, size / 2, size / 2)

    g.setColor(Color.WHITE)
    g.fillOval(size / 4, 0, size / 2, size / 2)
    g.fillOval(7 * size / 16, 11 * size / 16, size / 8, size / 8)

    g.setColor(Color.BLACK)
    g.fillOval(7 * size / 16, 3 * size / 16, size / 8, size / 8)
    // Use drawOval to draw an empty circle for the outside border
    g.drawOval(0, 0, sizeMinsOne, sizeMinsOne)

    // Restore the color for the caller
    g.setColor(colorSave)
  }

  def top = new MainFrame {
    title = "Rosetta Code >>> Yin Yang Generator | Language: Scala"
    contents = gui(preferedSize)

    def gui(sizeInterior: Int) = new Panel() {
      preferredSize = (sizeInterior, sizeInterior)

      /** Draw a Taijitu symbol in this graphics context.
       */
      override def paintComponent(graphics: Graphics2D) = {
        super.paintComponent(graphics)

        // Color in the background of the image
        background = Color.RED
        drawTaijitu(graphics, sizeInterior)
      }
    } // def gui(
  }

  override def main(args: Array[String]) = {
    preferedSize = args.headOption.map(_.toInt).getOrElse(preferedSize)
    super.main(args)
  }
}
```



## Scilab

This script uses complex numbers, as in <math>x + i\,y</math>, to represent <math>(x,y)</math> coordinates. [[wp:Euler's formula|Euler's formula]] is used to calculate points over in a circumference. The output is a single graphic window with two images of Yin and yang.

<lang>R = 1;                      //outer radius of first image
scale = 0.5;                //scale of the second image

scf(0); clf();
set(gca(),'isoview','on');
xname('Yin and Yang');

//First one
n_p = 100;                  //number of points per arc
angles = [];                //angles for each arc
angles = linspace(%pi/2, 3*%pi/2, n_p);
Arcs = zeros(7,n_p);

    Arcs(1,:) = R * exp(%i * angles);
    plot2d(real(Arcs(1,:)),imag(Arcs(1,:)));
    line = gce();
    set(line.children,'polyline_style',5);
    set(line.children,'foreground',8);
    
    Arcs(2,:) = -%i*R/2 + R/2 * exp(%i * angles);
    plot2d(real(Arcs(2,:)),imag(Arcs(2,:)));
    line = gce();
    set(line.children,'polyline_style',5);

angles = [];
angles = linspace(-%pi/2, %pi/2, n_p);

    Arcs(3,:) = R * exp(%i * angles);
    plot2d(real(Arcs(3,:)), imag(Arcs(3,:)));
    line = gce();
    set(line.children,'polyline_style',5);
    
    Arcs(4,:) = %i*R/2 + R/2 * exp(%i * angles);
    plot2d(real(Arcs(4,:)),imag(Arcs(4,:)));
    line = gce();
    set(line.children,'polyline_style',5);
    set(line.children,'foreground',8);

angles = [];
angles = linspace(0, 2*%pi, n_p);

    Arcs(5,:) = %i*R/2 + R/8 * exp(%i * angles);
    plot2d(real(Arcs(5,:)),imag(Arcs(5,:)));
    line = gce();
    set(line.children,'polyline_style',5);
    
    Arcs(6,:) = -%i*R/2 + R/8 * exp(%i * angles);
    plot2d(real(Arcs(6,:)),imag(Arcs(6,:)));
    line = gce();
    set(line.children,'polyline_style',5);
    set(line.children,'foreground',8);
    
    Arcs(7,:) = R * exp(%i * angles);
    plot2d(real(Arcs(7,:)),imag(Arcs(7,:)));
    set(gca(),'axes_visible',['off','off','off']);

//Scaling
new_pos = R + 2*R*scale;
Arcs = new_pos + Arcs .* scale;

    plot2d(real(Arcs(1,:)),imag(Arcs(1,:)));
    line = gce();
    set(line.children,'polyline_style',5);
    set(line.children,'foreground',8);

    plot2d(real(Arcs(2,:)),imag(Arcs(2,:)));
    line = gce();
    set(line.children,'polyline_style',5);

    plot2d(real(Arcs(3,:)), imag(Arcs(3,:)));
    line = gce();
    set(line.children,'polyline_style',5);

    plot2d(real(Arcs(4,:)),imag(Arcs(4,:)));
    line = gce();
    set(line.children,'polyline_style',5);
    set(line.children,'foreground',8);

    plot2d(real(Arcs(5,:)),imag(Arcs(5,:)));
    line = gce();
    set(line.children,'polyline_style',5);

    plot2d(real(Arcs(6,:)),imag(Arcs(6,:)));
    line = gce();
    set(line.children,'polyline_style',5);
    set(line.children,'foreground',8);

    plot2d(real(Arcs(7,:)),imag(Arcs(7,:)));
    set(gca(),'axes_visible',['off','off','off']);
```



## Seed7

[[File:yinandyang.png|thumb|Output of the Seed7 program]]

```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";
  include "draw.s7i";
  include "keybd.s7i";

const proc: yinYang (in integer: xPos, in integer: yPos, in integer: size) is func
  begin
    pieslice(xPos, yPos, size, 3.0 * PI / 2.0, PI, black);
    pieslice(xPos, yPos, size, PI / 2.0, PI, white);
    fcircle(xPos, yPos - size div 2, size div 2, white);
    fcircle(xPos, yPos + size div 2, size div 2, black);
    fcircle(xPos, yPos - size div 2, size div 6, black);
    fcircle(xPos, yPos + size div 2, size div 6, white);
    circle(xPos, yPos, size, black);
  end func;

const proc: main is func
  begin
    screen(640, 480);
    clear(white);
    KEYBOARD := GRAPH_KEYBOARD;
    yinYang(100, 100, 80);
    yinYang(400, 250, 200);
    readln(KEYBOARD);
  end func;
```



## Sidef

{{trans|Perl 6}}

```ruby
func circle (rad, cx, cy, fill='white', stroke='black') {
    say "<circle cx='#{cx}' cy='#{cy}' r='#{rad}' fill='#{fill}' stroke='#{stroke}' stroke-width='1'/>";
}

func yin_yang (rad, cx, cy, fill='white', stroke='black', angle=90) {
    var (c, w) = (1, 0);
    angle != 0 && say "<g transform='rotate(#{angle}, #{cx}, #{cy})'>";
    circle(rad, cx, cy, fill, stroke);
    say("<path d='M #{cx} #{cy + rad}A #{rad/2} #{rad/2} 0 0 #{c} #{cx} #{cy} ",
        "#{rad/2} #{rad/2} 0 0 #{w} #{cx} #{cy - rad} #{rad} #{rad} 0 0 #{c} #{cx} ",
        "#{cy + rad} z' fill='#{stroke}' stroke='none' />");
    circle(rad/5, cx, cy + rad/2, fill, stroke);
    circle(rad/5, cx, cy - rad/2, stroke, fill);
    angle != 0 && say "</g>";
}

say '<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" xmlns:xlink="http://www.w3.org/1999/xlink">';

yin_yang(40, 50, 50);
yin_yang(20, 120, 120);

say '</svg>';
```



## SVG

[[File:Yinyang.svg|thumb|A rendering]]
SVG has no proper functions or variables, 
but we can translate and rescale a shape after defining it.


```xml
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" 
  "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    width="600" height="600">

<!-- We create the symbol in the rectangle from (0, 0) to (1, 1)
and then translate it so it's centered on the origin. -->
<symbol id="yinyang">
  <g transform="translate(-0.5, -0.5)">
    <!-- A white circle, for the bulk of the left-hand part -->
    <circle cx="0.5" cy="0.5" r="0.5" fill="white"/>
    <!-- A black semicircle, for the bulk of the right-hand part -->
    <path d="M 0.5,0 A 0.5,0.5 0 0,1 0.5,1 z" fill="black"/>
    <!-- Circles to extend each part -->
    <circle cx="0.5" cy="0.25" r="0.25" fill="white"/>
    <circle cx="0.5" cy="0.75" r="0.25" fill="black"/>
    <!-- The spots -->
    <circle cx="0.5" cy="0.25" r="0.1" fill="black"/>
    <circle cx="0.5" cy="0.75" r="0.1" fill="white"/>
    <!-- An outline for the whole shape -->
    <circle cx="0.5" cy="0.5" r="0.5" fill="none"
      stroke="gray" stroke-width=".01"/>
  </g>
</symbol>

<use xlink:href="#yinyang"
  transform="translate(125, 125) scale(200, 200)"/>

<use xlink:href="#yinyang"
  transform="translate(375, 375) scale(400, 400)"/>

</svg>
```



## Tcl

[[File:yinyang-tcl.gif|thumb|Output of this Tcl program]]
{{libheader|Tk}}

```tcl
package require Tcl 8.5
package require Tk

namespace import tcl::mathop::\[-+\]    ;# Shorter coordinate math
proc yinyang {c x y r {colors {white black}}} {
    lassign $colors a b
    set tt [expr {$r * 2 / 3.0}]
    set h [expr {$r / 2.0}]
    set t [expr {$r / 3.0}]
    set s [expr {$r / 6.0}]
    $c create arc [- $x $r] [- $y $r] [+ $x $r] [+ $y $r] \
	-fill $a -outline {} -extent 180 -start 90
    $c create arc [- $x $r] [- $y $r] [+ $x $r] [+ $y $r] \
	-fill $b -outline {} -extent 180 -start 270
    $c create oval [- $x $h] [- $y $r] [+ $x $h] $y \
	-fill $a -outline {}
    $c create oval [- $x $h] [+ $y $r] [+ $x $h] $y \
	-fill $b -outline {}
    $c create oval [- $x $s] [- $y $tt] [+ $x $s] [- $y $t] \
	-fill $b -outline {}
    $c create oval [- $x $s] [+ $y $tt] [+ $x $s] [+ $y $t] \
	-fill $a -outline {}
}

pack [canvas .c -width 300 -height 300 -background gray50]
yinyang .c 110 110 90
yinyang .c 240 240 40
```



## XPL0

[[File:YinXPL0.png|180px|thumb|right|Output]]

```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

def Black=0, Red=4, White=$F;

proc Circle(X0, Y0, R, CL, CR); \Show a filled circle
int X0, Y0, R, CL, CR;          \left and right half colors
int X, Y;
[for Y:= -R to R do
    for X:= -R to R do
        if X*X + Y*Y <= R*R then
            Point(X+X0, Y+Y0, if X<0 then CL else CR);
]; \Circle

proc YinYang(X0, Y0, R);
int  X0, Y0, R;
[Circle(X0, Y0,     R,   White, Black);
 Circle(X0, Y0-R/2, R/2, White, White);
 Circle(X0, Y0-R/2, R/6, Black, Black);
 Circle(X0, Y0+R/2, R/2, Black, Black);
 Circle(X0, Y0+R/2, R/6, White, White);
]; \YinYang

[SetVid($101);                  \640x480 graphics
Circle(320, 240, 400, Red, Red);\fill screen with background color
YinYang(80, 80, 70);
YinYang(240, 240, 150);
if ChIn(1) then [];             \wait for keystroke
SetVid(3);                      \restore normal text mode
]
```



## zkl

Writes to stdout a SVG file with two yin-yangs.

```zkl
fcn draw_yinyang(trans,scale){
   0'|<use xlink:href="#y" transform="translate(%d,%d) scale(%g)"/>|
   .fmt(trans,trans,scale).print();
}

print(
"<?xml version='1.0' encoding='UTF-8' standalone='no'?>\n"
"<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'\n"
"	'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>\n"
"<svg xmlns='http://www.w3.org/2000/svg' version='1.1'\n"
"	xmlns:xlink='http://www.w3.org/1999/xlink'\n"
"		width='30' height='30'>\n"
"	<defs><g id='y'>\n"
"		<circle cx='0' cy='0' r='200' stroke='black'\n"
"			fill='white' stroke-width='1'/>\n"
"		<path d='M0 -200 A 200 200 0 0 0 0 200\n"
"			100 100 0 0 0 0 0 100 100 0 0 1 0 -200\n"
"			z' fill='black'/>\n"
"		<circle cx='0' cy='100' r='33' fill='white'/>\n"
"		<circle cx='0' cy='-100' r='33' fill='black'/>\n"
"	</g></defs>\n");

draw_yinyang(20, 0.05);
draw_yinyang( 8, 0.02);
print("</svg>");
```

A here doc (#<<<) could be used to wrap the HTML but, depending on the editor used, the formatting may not be what you want (eg "\n" vs "\r\n").
{{out}}

```txt

$ zkl zz  > foo.html 
copy to browswer

```

[[File:yinyang-C.svg]]

[[Category:Geometry]]
