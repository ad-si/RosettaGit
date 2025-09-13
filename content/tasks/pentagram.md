+++
title = "Pentagram"
description = ""
date = 2019-09-09T11:17:59Z
aliases = []
[extra]
id = 19037
[taxonomies]
categories = ["task"]
tags = []
+++

[[File:Pentagram Java.png|300px||right]]

A [[wp:pentagram|pentagram]] is a star polygon, consisting of a central pentagon of which each side forms the base of an isosceles triangle. The vertex of each triangle, a point of the star, is 36 degrees.




## Task

Draw (or print) a regular pentagram, in any orientation. Use a different color (or token) for stroke and fill, and background. For the fill it should be assumed that all points inside the triangles and the pentagon are inside the pentagram.




## See also

* [http://proofsfromthebook.com/2013/08/04/angle-sum-of-a-pentagram/ Angle sum of a pentagram]





## AutoHotkey

[[File:Pentagram Java.png|300px|thumb|right]]

```AutoHotkey

#Include Gdip.ahk	; https://autohotkey.com/boards/viewtopic.php?f=6&t=6517
Width :=A_ScreenWidth, Height := A_ScreenHeight
Gui, 1: +E0x20 +Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, 1: Show, NA
hwnd1 := WinExist()
OnExit, Exit

If !pToken := Gdip_Startup()
{
	MsgBox, 48, gdiplus error!, Gdiplus failed to start.
	. Please ensure you have gdiplus on your system
	ExitApp
}

hbm := CreateDIBSection(Width, Height)
hdc := CreateCompatibleDC()
obm := SelectObject(hdc, hbm)
G := Gdip_GraphicsFromHDC(hdc)
Gdip_SetSmoothingMode(G, 4)
pBrush 	:= Gdip_BrushCreateSolid(0xFF6495ED)
pPen 	:= Gdip_CreatePen(0xff000000, 3)

;---------------------------------
LL := 165
Cx := Floor(A_ScreenWidth/2)
Cy := Floor(A_ScreenHeight/2)
phi := 54
;---------------------------------
loop, 5
{
	theta := abs(180-144-phi)
	p1x := Floor(Cx + LL * Cos(phi * 0.01745329252))
	p1y := Floor(Cy + LL * Sin(phi * 0.01745329252))
	p2x := Floor(Cx - LL * Cos(theta * 0.01745329252))
	p2y := Floor(Cy - LL * Sin(theta * 0.01745329252))
	phi+= 72
	Gdip_FillPolygon(G, pBrush, p1x "," p1y "|" Cx "," Cy "|" p2x "," p2y)
}
loop, 5
{
	theta := abs(180-144-phi)
	p1x := Floor(Cx + LL * Cos(phi * 0.01745329252))
	p1y := Floor(Cy + LL * Sin(phi * 0.01745329252))
	p2x := Floor(Cx - LL * Cos(theta * 0.01745329252))
	p2y := Floor(Cy - LL * Sin(theta * 0.01745329252))
	phi+= 72
	Gdip_DrawLines(G, pPen, p1x "," p1y "|" p2x "," p2y ) ; "|" Cx "," Cy )
}
UpdateLayeredWindow(hwnd1, hdc, 0, 0, Width, Height)
Gdip_DeleteBrush(pBrush)
SelectObject(hdc, obm)
DeleteObject(hbm)
DeleteDC(hdc)
Gdip_DeleteGraphics(G)
return
;----------------------------------------------------------------------
Esc::
Exit:
Gdip_Shutdown(pToken)
ExitApp
Return
```


## C

Interactive program which takes the side lengths of the pentagram's core, it's arms and the colours for filling the background, drawing the figure and then filling it in. Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.


```C>#include<graphics.h

#include<stdio.h>
#include<math.h>

#define pi M_PI

int main(){
	
	char colourNames[][14] = { "BLACK", "BLUE", "GREEN", "CYAN", "RED", "MAGENTA", "BROWN", "LIGHTGRAY", "DARKGRAY",
              "LIGHTBLUE", "LIGHTGREEN", "LIGHTCYAN", "LIGHTRED", "LIGHTMAGENTA", "YELLOW", "WHITE" };
			  
	int stroke=0,fill=0,back=0,i;
	
	double centerX = 300,centerY = 300,coreSide,armLength,pentaLength;
	
	printf("Enter core pentagon side length : ");
	scanf("%lf",&coreSide);
	
	printf("Enter pentagram arm length : ");
	scanf("%lf",&armLength);
	
	printf("Available colours are :\n");
	
	for(i=0;i<16;i++){
		printf("%d. %s\t",i+1,colourNames[i]);
		if((i+1) % 3 == 0){
			printf("\n");
		}
	}
	
	while(stroke==fill && fill==back){
		printf("\nEnter three diffrenet options for stroke, fill and background : ");
		scanf("%d%d%d",&stroke,&fill,&back);
	}
	
	pentaLength = coreSide/(2 * tan(pi/5)) + sqrt(armLength*armLength - coreSide*coreSide/4);
	
	initwindow(2*centerX,2*centerY,"Pentagram");
		
	setcolor(stroke-1);
	
	setfillstyle(SOLID_FILL,back-1);
	
	bar(0,0,2*centerX,2*centerY);
	
	floodfill(centerX,centerY,back-1);
	
	setfillstyle(SOLID_FILL,fill-1);

	for(i=0;i<5;i++){
		line(centerX + coreSide*cos(i*2*pi/5)/(2*sin(pi/5)),centerY + coreSide*sin(i*2*pi/5)/(2*sin(pi/5)),centerX + coreSide*cos((i+1)*2*pi/5)/(2*sin(pi/5)),centerY + coreSide*sin((i+1)*2*pi/5)/(2*sin(pi/5)));
		line(centerX + coreSide*cos(i*2*pi/5)/(2*sin(pi/5)),centerY + coreSide*sin(i*2*pi/5)/(2*sin(pi/5)),centerX + pentaLength*cos(i*2*pi/5 + pi/5),centerY + pentaLength*sin(i*2*pi/5 + pi/5));
		line(centerX + coreSide*cos((i+1)*2*pi/5)/(2*sin(pi/5)),centerY + coreSide*sin((i+1)*2*pi/5)/(2*sin(pi/5)),centerX + pentaLength*cos(i*2*pi/5 + pi/5),centerY + pentaLength*sin(i*2*pi/5 + pi/5));
		
		floodfill(centerX + coreSide*cos(i*2*pi/5 + pi/10)/(2*sin(pi/5)),centerY + coreSide*sin(i*2*pi/5 + pi/10)/(2*sin(pi/5)),stroke-1);
	}
	
	floodfill(centerX,centerY,stroke-1);
	
	getch();
	
	closegraph();
}

```



## EasyLang

[https://easylang.online/apps/run.html?code=floatvars%0Axp%20%3D%2010%0Ayp%20%3D%2040%0Alinewidth%202%0Amove%20xp%20yp%0Awhile%20angle%20%3E%20-720%0Ax%20%3D%20xp%20%2B%20cos%20angle%20%2A%2080%0Ay%20%3D%20yp%20%2B%20sin%20-angle%20%2A%2080%0Aline%20x%20y%0Af%5B%5D%20%26%3D%20x%0Af%5B%5D%20%26%3D%20y%0Axp%20%3D%20x%0Ayp%20%3D%20y%0Aangle%20-%3D%20144%0A.%0Acolor%20900%0Afill%20f%5B%5D Run it]
<lang>floatvars
xp = 10
yp = 40
linewidth 2
move xp yp
while angle > -720
  x = xp + cos angle * 80
  y = yp + sin -angle * 80
  line x y
  f[] &= x
  f[] &= y
  xp = x
  yp = y
  angle -= 144
.
color 900
fill f[]
```



## Go

```go
package main

import (
    "github.com/fogleman/gg"
    "math"
)

func Pentagram(x, y, r float64) []gg.Point {
    points := make([]gg.Point, 5)
    for i := 0; i < 5; i++ {
        fi := float64(i)
        angle := 2*math.Pi*fi/5 - math.Pi/2
        points[i] = gg.Point{x + r*math.Cos(angle), y + r*math.Sin(angle)}
    }
    return points
}

func main() {
    points := Pentagram(320, 320, 250)
    dc := gg.NewContext(640, 640)
    dc.SetRGB(1, 1, 1) // White
    dc.Clear()
    for i := 0; i <= 5; i++ {
        index := (i * 2) % 5
        p := points[index]
        dc.LineTo(p.X, p.Y)
    }
    dc.SetHexColor("#6495ED") // Cornflower Blue
    dc.SetFillRule(gg.FillRuleWinding)
    dc.FillPreserve()
    dc.SetRGB(0, 0, 0) // Black
    dc.SetLineWidth(5)
    dc.Stroke()
    dc.SavePNG("pentagram.png")
}
```


```txt

The image produced is similar to that of the Java entry.

```



## Haskell

This uses the [http://projects.haskell.org/diagrams/ Diagrams] library to create an SVG drawing.  Compiling, then running it like:

```txt

pentagram -w 400 -o pentagram_hs.svg

```

creates a 400x400 SVG file.

```haskell
-- Extract the vertices of a pentagon, re-ordering them so that drawing lines
-- from one to the next forms a pentagram.  Set the line's thickness and its
-- colour, as well as the fill and background colours.  Make the background a
-- bit larger than the pentagram.

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

pentagram = let [a, b, c, d, e] = trailVertices $ pentagon 1
            in [a, c, e, b, d]
               # fromVertices
               # closeTrail
               # strokeTrail
               # lw ultraThick
               # fc springgreen
               # lc blue
               # bgFrame 0.2 bisque

main = mainWith (pentagram :: Diagram B)
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Pentagra.bas"
110 OPTION ANGLE DEGREES
120 GRAPHICS HIRES 4
130 SET PALETTE BLUE,CYAN,YELLOW,BLACK
140 PLOT 640,700,ANGLE 288;
150 FOR I=1 TO 5
160   PLOT FORWARD 700,RIGHT 144;
170 NEXT
180 SET INK 3
190 SET BEAM OFF:PLOT 0,0,PAINT
```



## J


Probably the simplest approach is:


```j
require'plot'
plot j./2 1 o./180p_1 %~ 72*i. 6
```


This will give a pentagram with a blue border and a white interior.


## Java

[[File:Pentagram Java.png|300px|thumb|right]]
```java
import java.awt.*;
import java.awt.geom.Path2D;
import javax.swing.*;

public class Pentagram extends JPanel {

    final double degrees144 = Math.toRadians(144);

    public Pentagram() {
        setPreferredSize(new Dimension(640, 640));
        setBackground(Color.white);
    }

    private void drawPentagram(Graphics2D g, int len, int x, int y,
            Color fill, Color stroke) {
        double angle = 0;

        Path2D p = new Path2D.Float();
        p.moveTo(x, y);

        for (int i = 0; i < 5; i++) {
            int x2 = x + (int) (Math.cos(angle) * len);
            int y2 = y + (int) (Math.sin(-angle) * len);
            p.lineTo(x2, y2);
            x = x2;
            y = y2;
            angle -= degrees144;
        }
        p.closePath();

        g.setColor(fill);
        g.fill(p);

        g.setColor(stroke);
        g.draw(p);
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;

        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        g.setStroke(new BasicStroke(5, BasicStroke.CAP_ROUND, 0));

        drawPentagram(g, 500, 70, 250, new Color(0x6495ED), Color.darkGray);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Pentagram");
            f.setResizable(false);
            f.add(new Pentagram(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## Julia



```julia
using Luxor

function drawpentagram(path::AbstractString, w::Integer=1000, h::Integer=1000)
    Drawing(h, w, path)
    origin()
    setline(16)

    # To get a different color border from the fill, draw twice, first with fill, then without.
    sethue("aqua")
    star(0, 0, 500, 5, 0.39, 3pi/10, :fill)

    sethue("navy")
    verts = star(0, 0, 500, 5, 0.5, 3pi/10, vertices=true)
    poly([verts[i] for i in [1,5,9,3,7,1]], :stroke)
    finish()
    preview()
end

drawpentagram("data/pentagram.png")
```



## Kotlin

```scala
// version 1.1.2

import java.awt.*
import java.awt.geom.Path2D
import javax.swing.*
 
class Pentagram : JPanel() {
    init {
        preferredSize = Dimension(640, 640)
        background = Color.white
    }

    private fun drawPentagram(g: Graphics2D, len: Int, x: Int, y: Int,
                              fill: Color, stroke: Color) {
        var x2 = x.toDouble()
        var y2 = y.toDouble()
        var angle = 0.0
        val p = Path2D.Float()      
        p.moveTo(x2, y2)
 
        for (i in 0..4) {
            x2 += Math.cos(angle) * len
            y2 += Math.sin(-angle) * len
            p.lineTo(x2, y2)
            angle -= Math.toRadians(144.0)
        }

        p.closePath()
        with(g) {
            color = fill
            fill(p)
            color = stroke
            draw(p)
        }
    }

    override fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON)
        g.stroke = BasicStroke(5.0f, BasicStroke.CAP_ROUND, 0)
        drawPentagram(g, 500, 70, 250, Color(0x6495ED), Color.darkGray)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        with(f) { 
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            title = "Pentagram"
            isResizable = false
            add(Pentagram(), BorderLayout.CENTER)
            pack()
            setLocationRelativeTo(null)
            isVisible = true
        }
    }
}
```



## Maple


```maple
with(geometry):
RegularStarPolygon(middle, 5/2, point(c, 0, 0), 1):
v := [seq(coordinates(i), i in DefinedAs(middle))]:
pentagram := plottools[rotate](plottools[polygon](v), Pi/2):
plots[display](pentagram, colour = yellow, axes = none);
```

Note: Plot shown below is generated using interface(plotdevice = char);

```txt

                                       C                                      
                                      C C                                     
                                     C   C                                    
                                    C     C                                   
                                   C       C                                  
                                 CC         CC                                
                                C             C                               
                               C               C                              
   CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  
      CCCC                   C                   C                   CCCC     
          CCCCC             C                     C             CCCCC         
               CCCC         C                     C         CCCC              
                   CCCCC   C                       C   CCCCC                  
                        CCCC                       CCCC                       
                         C  CCCCC             CCCCC  C                        
                        C        CCCC     CCCC        C                       
                      CC             CCCCC             CC                     
                     C           CCCCC   CCCCC           C                    
                    C        CCCC             CCCC        C                   
                   C    CCCCC                     CCCCC    C                  
                  C CCCC                               CCCC C                 
                 CCC                                       CCC               

```



## Mathematica


```mathematica

Graphics[{
    EdgeForm[Directive[Thickness[0.01], RGBColor[0, 0, 1]]],(*Edge coloring*)
    RGBColor[0.5, 0.5, .50], (*Fill coloring*)
    Polygon[AnglePath[Table[6 Pi/5, 5]]]}
    ]

```



## ooRexx


```oorexx
/* REXX ***************************************************************
* Create a BMP file showing a pentagram
**********************************************************************/
pentagram='pentagram.bmp'
'erase' pentagram
s='424d4600000000000000360000002800000038000000280000000100180000000000'X
s=s'1000000000000000000000000000000000000000'x
Say 'sl='length(s)
z.0=0
white='ffffff'x
red  ='00ff00'x
green='ff0000'x
blue ='0000ff'x
rd6=copies(rd,6)
m=133
m=80
n=80
hor=m*8      /* 56 */
ver=n*8      /* 40 */
Say 'hor='hor
Say 'ver='ver
Say 'sl='length(s)
s=overlay(lend(hor),s,19,4)
s=overlay(lend(ver),s,23,4)
Say 'sl='length(s)
z.=copies('ffffff'x,3192%3)
z.=copies('ffffff'x,8*m)
z.0=648
s72 =RxCalcsin(72,,'D')
c72 =RxCalccos(72,,'D')
s144=RxCalcsin(144,,'D')
c144=RxCalccos(144,,'D')
xm=300
ym=300
r=200
p.0x.1=xm
p.0y.1=ym+r
p.0x.2=format(xm+r*s72,3,0)
p.0y.2=format(ym+r*c72,3,0)
p.0x.3=format(xm+r*s144,3,0)
p.0y.3=format(ym+r*c144,3,0)
p.0x.4=format(xm-r*s144,3,0)
p.0y.4=p.0y.3
p.0x.5=format(xm-r*s72,3,0)
p.0y.5=p.0y.2
Do i=1 To 5
  Say p.0x.i p.0y.i
  End
Call line p.0x.1,p.0y.1,p.0x.3,p.0y.3
Call line p.0x.1,p.0y.1,p.0x.4,p.0y.4
Call line p.0x.2,p.0y.2,p.0x.4,p.0y.4
Call line p.0x.2,p.0y.2,p.0x.5,p.0y.5
Call line p.0x.3,p.0y.3,p.0x.5,p.0y.5

Do i=1 To z.0
  s=s||z.i
  End

Call lineout pentagram,s
Call lineout pentagram
Exit

lend:
Return reverse(d2c(arg(1),4))

line: Procedure Expose z. red green blue
Parse Arg x0, y0, x1, y1
Say 'line'  x0  y0  x1  y1
dx = abs(x1-x0)
dy = abs(y1-y0)
if x0 < x1 then sx = 1
           else sx = -1
if y0 < y1 then sy = 1
           else sy = -1
err = dx-dy

Do Forever
  xxx=x0*3+2
  Do yy=y0-1 To y0+1
    z.yy=overlay(copies(blue,5),z.yy,xxx)
    End
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
::requires RxMath Library
```



## Perl


```perl
use SVG;

my $tau   = 2 * 4*atan2(1, 1);
my $dim   = 200;
my $sides = 5;

for $v (0, 2, 4, 1, 3, 0) {
    push @vx, 0.9 * $dim * cos($tau * $v / $sides);
    push @vy, 0.9 * $dim * sin($tau * $v / $sides);
}

my $svg= SVG->new( width => 2*$dim, height => 2*$dim);

my $points = $svg->get_path(
    x     => \@vx,
    y     => \@vy,
    -type => 'polyline',
);

$svg->rect (
    width  => "100%",
    height => "100%",
    style  => {
        'fill' => 'bisque'
    }
);

$svg->polyline (
    %$points,
    style => {
        'fill'         => 'seashell',
        'stroke'       => 'blue',
        'stroke-width' => 3,
    },
    transform => "translate($dim,$dim) rotate(-18)"
);

open  $fh, '>', 'pentagram.svg';
print $fh  $svg->xmlify(-namespace=>'svg');
close $fh;
```

[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/pentagram.svg Pentagram] (offsite image)


## Perl 6

Generate an SVG file to STDOUT. Redirect to a file to capture and display it.

```perl6
use SVG;

constant $dim = 200;
constant $sides = 5;

my @vertices = map { 0.9 * $dim * cis($_ * τ / $sides) }, ^$sides;

my @points   = map |*.reals.fmt("%0.3f"),
  flat @vertices[0, 2 ... *], @vertices[1, 3 ... *], @vertices[0];

say SVG.serialize(
    svg => [
        :width($dim*2), :height($dim*2),
        :rect[:width<100%>, :height<100%>, :style<fill:bisque;>],
        :polyline[ :points(@points.join: ','),
          :style("stroke:blue; stroke-width:3; fill:seashell;"),
          :transform("translate($dim,$dim) rotate(-90)")
        ],
    ],
);
```

See [https://github.com/thundergnat/rc/blob/master/img/pentagram-perl6.svg Pentagram] (offsite svg image)

Ever wondered what a regular 7 sided star looks like? Change $sides to 7 and re-run.
See [https://github.com/thundergnat/rc/blob/master/img/heptagram-perl6.svg Heptagram]


## Phix

Resizable and optionally rotating gui (desktop) version

```Phix
-- demo\rosetta\Pentagram.exw
include pGUI.e

Ihandle dlg, canvas, timer
cdCanvas cddbuffer, cdcanvas

integer rot = 0
enum FILL,BORDER
constant colours = {CD_BLUE,CD_RED},
         modes = {CD_FILL,CD_CLOSED_LINES}

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    integer {w, h} = IupGetIntInt(canvas, "DRAWSIZE"),
            cx = floor(w/2),
            cy = floor(h/2),
            r = floor(min(cx,cy)*0.9)
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    cdCanvasSetFillMode(cddbuffer, CD_WINDING)
    cdCanvasSetLineWidth(cddbuffer, round(radius/100)+1) 
    for mode=FILL to BORDER do
        cdCanvasSetForeground(cddbuffer,colours[mode])
        cdCanvasBegin(cddbuffer,modes[mode])
        for a=90 to 666 by 144 do
            atom ra = (a+rot)*CD_DEG2RAD,
                 x = r*cos(ra)+cx,
                 y = r*sin(ra)+cy
            cdCanvasVertex(cddbuffer, x, y)
        end for
        cdCanvasEnd(cddbuffer)
    end for
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    cdCanvasSetForeground(cddbuffer, CD_GRAY)
    return IUP_DEFAULT
end function

function timer_cb(Ihandle /*ih*/)
    rot = mod(rot+359,360)
    IupRedraw(canvas)
    return IUP_IGNORE
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c=' ' then
        IupSetInt(timer,"RUN",not IupGetInt(timer,"RUN"))
    end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "640x640")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Pentagram")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))

    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    timer = IupTimer(Icallback("timer_cb"), 80, active:=false)
    IupMainLoop()
    IupClose()
end procedure

main()
```

And a quick svg version
```Phix
constant HDR = """
<?xml version="1.0" standalone="no" ?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN"
"http://www.w3.org/TR/2001/PR-SVG-20010719/DTD/svg10.dtd">
<svg height="%d" width="%d" style="" xmlns="http://www.w3.org/2000/svg">
<rect height="100%%" width="100%%" style="fill:black;" />
"""
constant LINE = """
<polyline points="%s"
style="fill:blue; stroke:white; stroke-width:3;"
transform="translate(%d, %d) rotate(-18)" />
"""

function pentagram(integer dim=200, sides=5)

    sequence v = repeat(0,sides)
    for i=1 to sides do
        atom theta = PI*2*(i-1)/5,
             x = cos(theta)*dim,
             y = sin(theta)*dim
        v[i] = {sprintf("%.3f",x),
                sprintf("%.3f",y)}
    end for
    v = append(v,v[1])
    sequence q = {}
    for i=1 to length(v) by 2 do
        q &= v[i]
    end for
    for i=2 to length(v) by 2 do
        q &= v[i]
    end for 
    string res = sprintf(HDR,dim*2)
    res &= sprintf(LINE,{join(q),dim,dim})
    res &= "</svg>\n"
 
    return res
end function
 
puts(1,pentagram())
```

Output identical to sidef


## PostScript


```postscript
%!PS-Adobe-3.0 EPSF
%%BoundingBox: 0 0 200 600
 
/n 5 def % 5-star; can be set to other odd numbers

/s { gsave } def
/r { grestore } def
/g { .7 setgray } def
/t { 100 exch translate } def
/p {
	180 90 n div sub rotate
	0 0 moveto
	n { 0 160 rlineto 180 180 n div sub rotate } repeat
	closepath
} def

s 570 t p s g eofill r stroke r		% even-odd fill
s 370 t p s g fill r stroke r		% non-zero fill
s 170 t p s 2 setlinewidth stroke r g fill r % non-zero, but hide inner strokes

%%EOF
```


The following isn't exactly what the task asks for, but it's kind of fun if you have a PS interpreter that progressively updates.  The program draws a lot of stars, so it's extremely likely that some of them are pentagrams...

```postscript
%!PS-Adobe-3.0 EPSF
%%BoundingBox: 0 0 400 400

% randomly choose from 5- to 35-stars
/maxpoint 35 def
/minpoint 5 def
/maxradius 30 def

/rnd1 { rand 16#80000000 div } def
/rnd { rnd1 mul} def
/rndi { 2 index sub rnd1 mul 1 index div cvi mul add} def
/line { rotate 0 rlineto } def

/star { gsave
	/n minpoint 2 maxpoint rndi def
	/r maxradius rnd def
	/a 180 180 n div sub def 
	/b 360 a n mul sub n div def

	400 rnd 400 rnd translate 360 rnd rotate
	0 0 moveto n { r a line r b line } repeat closepath
	rnd1 rnd1 rnd1 3 { 2 index 1 exch sub } repeat 
	gsave setrgbcolor fill grestore setrgbcolor stroke
	grestore } def

0 setlinewidth 2000 {star} repeat showpage
%%EOF
```



## Python

```python
import turtle

turtle.bgcolor("green")
t = turtle.Turtle()
t.color("red", "blue")
t.begin_fill()
for i in range(0, 5):
    t.forward(200)
    t.right(144)
t.end_fill()
```



## Racket



```racket
#lang racket
(require 2htdp/image)

(overlay
 (star-polygon 100 5 2 "outline" (make-pen "blue" 4 "solid" "round" "round"))
 (star-polygon 100 5 2 "solid" "cyan"))
```



## REXX

```rexx
/* REXX ***************************************************************
* Create a BMP file showing a pentagram
**********************************************************************/
Parse Version v
If pos('Regina',v)>0 Then
  pentagram='pentagrama.bmp'
Else
  pentagram='pentagramx.bmp'
'erase' pentagram
s='424d4600000000000000360000002800000038000000280000000100180000000000'X||,
  '1000000000000000000000000000000000000000'x
Say 'sl='length(s)
z.0=0
white='ffffff'x
red  ='00ff00'x
green='ff0000'x
blue ='0000ff'x
rd6=copies(rd,6)
m=133
m=80
n=80
hor=m*8      /* 56 */
ver=n*8      /* 40 */
Say 'hor='hor
Say 'ver='ver
Say 'sl='length(s)
s=overlay(lend(hor),s,19,4)
s=overlay(lend(ver),s,23,4)
Say 'sl='length(s)
z.=copies('ffffff'x,3192%3)
z.=copies('ffffff'x,8*m)
z.0=648
pi_5=2*3.14159/5
s72 =sin(pi_5  )
c72 =cos(pi_5  )
s144=sin(pi_5*2)
c144=cos(pi_5*2)
xm=300
ym=300
r=200
p.0x.1=xm
p.0y.1=ym+r

p.0x.2=format(xm+r*s72,3,0)
p.0y.2=format(ym+r*c72,3,0)
p.0x.3=format(xm+r*s144,3,0)
p.0y.3=format(ym+r*c144,3,0)
p.0x.4=format(xm-r*s144,3,0)
p.0y.4=p.0y.3
p.0x.5=format(xm-r*s72,3,0)
p.0y.5=p.0y.2
Do i=1 To 5
  Say p.0x.i p.0y.i
  End
Call line p.0x.1,p.0y.1,p.0x.3,p.0y.3
Call line p.0x.1,p.0y.1,p.0x.4,p.0y.4
Call line p.0x.2,p.0y.2,p.0x.4,p.0y.4
Call line p.0x.2,p.0y.2,p.0x.5,p.0y.5
Call line p.0x.3,p.0y.3,p.0x.5,p.0y.5

Do i=1 To z.0
  s=s||z.i
  End

Call lineout pentagram,s
Call lineout pentagram
Exit

lend:
Return reverse(d2c(arg(1),4))

line: Procedure Expose z. red green blue
Parse Arg x0, y0, x1, y1
Say 'line'  x0  y0  x1  y1
dx = abs(x1-x0)
dy = abs(y1-y0)
if x0 < x1 then sx = 1
           else sx = -1
if y0 < y1 then sy = 1
           else sy = -1
err = dx-dy

Do Forever
  xxx=x0*3+2
  Do yy=y0-1 To y0+1
    z.yy=overlay(copies(blue,5),z.yy,xxx)
    End
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

sin: Procedure
/* REXX ****************************************************************
* Return sin(x<,p>) -- with the specified precision
***********************************************************************/
  Parse Arg x,prec
  If prec='' Then prec=9
  Numeric Digits (2*prec)
  Numeric Fuzz   3
  pi=3.14159
  Do While x>pi
    x=x-pi
    End
  Do While x<-pi
    x=x+pi
    End
  o=x
  u=1
  r=x
  Do i=3 By 2
    ra=r
    o=-o*x*x
    u=u*i*(i-1)
    r=r+(o/u)
    If r=ra Then Leave
    End
  Numeric Digits prec
  Return r+0

cos: Procedure
/* REXX ****************************************************************
* Return cos(x) -- with specified precision
***********************************************************************/
  Parse Arg x,prec
  If prec='' Then prec=9
  Numeric Digits (2*prec)
  Numeric Fuzz 3
  o=1
  u=1
  r=1
  Do i=1 By 2
    ra=r
    o=-o*x*x
    u=u*i*(i+1)
    r=r+(o/u)
    If r=ra Then Leave
    End
  Numeric Digits prec
  Return r+0

sqrt: Procedure
/* REXX ***************************************************************
* EXEC to calculate the square root of a = 2 with high precision
**********************************************************************/
  Parse Arg x,prec
  If prec<9 Then prec=9
  prec1=2*prec
  eps=10**(-prec1)
  k = 1
  Numeric Digits 3
  r0= x
  r = 1
  Do i=1 By 1 Until r=r0 | (abs(r*r-x)<eps)
    r0 = r
    r  = (r + x/r) / 2
    k  = min(prec1,2*k)
    Numeric Digits (k + 5)
    End
  Numeric Digits prec
  Return r+0
```



## Ring


```ring

# Project : Pentagram

load "guilib.ring"

paint = null

new qapp 
        {
        win1 = new qwidget() {
                  setwindowtitle("Pentagram")
                  setgeometry(100,100,500,600)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,400,400)
                              settext("")
                  }
                  new qpushbutton(win1) {
                          setgeometry(150,500,100,30)
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
                 setwidth(5)
        }
        paint = new qpainter() {
                  begin(p1)
                  setpen(pen)

        nn = 165
        cx = 800
        cy = 600
        phi = 54

        color = new qcolor()
        color.setrgb(0, 0, 255,255)
        mybrush = new qbrush() {setstyle(1) setcolor(color)}
        setbrush(mybrush)

       for n = 1 to 5
             theta = fabs(180-144-phi)
	     p1x = floor(cx + nn * cos(phi * 0.01745329252))
	     p1y = floor(cy + nn * sin(phi * 0.01745329252))
	     p2x = floor(cx - nn * cos(theta * 0.01745329252))
	     p2y = floor(cy - nn * sin(theta * 0.01745329252))
	     phi+= 72
	     drawpolygon([[p1x,p1y],[cx,cy],[p2x,p2y]],0)
        next
 
        endpaint()
        }
        label1 { setpicture(p1) show() }
        return

```

Output:

https://www.dropbox.com/s/znbcsoatlc00n4w/Pentagram.jpg?dl=0


## Scala


### Java Swing Interoperability


```Scala
import java.awt._
import java.awt.geom.Path2D

import javax.swing._

object Pentagram extends App {

  SwingUtilities.invokeLater(() =>
    new JFrame("Pentagram") {

      class Pentagram extends JPanel {
        setPreferredSize(new Dimension(640, 640))
        setBackground(Color.white)
        final private val degrees144 = Math.toRadians(144)

        override def paintComponent(gg: Graphics): Unit = {
          val g = gg.asInstanceOf[Graphics2D]

          def drawPentagram(g: Graphics2D, x: Int, y: Int, fill: Color): Unit = {
            var (_x, _y, angle) = (x, y, 0.0)
            val p = new Path2D.Float
            p.moveTo(_x, _y)
            for (i <- 0 until 5) {
              val (x2, y2) = (_x + (Math.cos(angle) * 500).toInt, _y + (Math.sin(-angle) * 500).toInt)
              p.lineTo(x2, y2)
              _x = x2
              _y = y2
              angle -= degrees144
            }
            p.closePath()
            g.setColor(fill)
            g.fill(p)
            g.setColor(Color.darkGray)
            g.draw(p)
          }

          super.paintComponent(gg)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          g.setStroke(new BasicStroke(5, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER))
          drawPentagram(g, 70, 250, new Color(0x6495ED))
        }
      }

      add(new Pentagram, BorderLayout.CENTER)
      pack()
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setLocationRelativeTo(null)
      setResizable(false)
      setVisible(true)
    }
  )

}
```


## Sidef

Generates a SVG image to STDOUT.

```ruby
func pentagram(dim=200, sides=5) {
    var pentagram = <<-EOT
    <?xml version="1.0" standalone="no" ?>
    <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN"
    "http://www.w3.org/TR/2001/PR-SVG-20010719/DTD/svg10.dtd">
    <svg height="#{dim*2}" width="#{dim*2}" style="" xmlns="http://www.w3.org/2000/svg">
    <rect height="100%" width="100%" style="fill:black;" />
    EOT

    func cis(x) {
        cos(x) + sin(x).i
    }

    func pline(q) {
        <<-EOT
        <polyline points="#{[q..., q[0], q[1]].map{|n| '%0.3f' % n }.join(' ')}"
        style="fill:blue; stroke:white; stroke-width:3;"
        transform="translate(#{dim}, #{dim}) rotate(-18)" />
        EOT
    }

    var v = sides.range.map {|k| 0.9 * dim * cis(k * Num.tau / sides) }
    pentagram += pline([v[range(0, v.end, 2)], v[range(1, v.end, 2)]].map{.reals})
    pentagram += '</svg>'

    return pentagram
}

say pentagram()
```

```txt
<?xml version="1.0" standalone="no" ?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN"
"http://www.w3.org/TR/2001/PR-SVG-20010719/DTD/svg10.dtd">
<svg height="400" width="400" style="" xmlns="http://www.w3.org/2000/svg">
<rect height="100%" width="100%" style="fill:black;" />
<polyline points="180.000 0.000 -145.623 105.801 55.623 -171.190 55.623 171.190 -145.623 -105.801 180.000 0.000"
style="fill:blue; stroke:white; stroke-width:3;"
transform="translate(200, 200) rotate(-18)" />
</svg>
```



## SPL


```spl
mx,my = #.scrsize()
xc = mx/2
yc = my/2
mr = #.min(mx,my)/3
#.angle(#.degrees)
#.drawcolor(1,0,0)
#.drawsize(10)
> r, mr..0,-1
  #.drawline(xc,yc-r,xc,yc-r)
  > a, 54..630,144
    #.drawline(r*#.cos(a)+xc,r*#.sin(a)+yc)
  <
  #.drawcolor(1,1,0)
  #.drawsize(1)
<
```



## Tcl

This implementation draws a simple pentagram on a [http://wiki.tcl.tk/1415 Canvas] widget.
```Tcl

package require Tk 8.6   	;# lmap is new in Tcl/Tk 8.6

set pi [expr 4*atan(1)]

pack [canvas .c] -expand yes -fill both     ;# create the canvas

update          ;# draw everything so the dimensions are accurate

set w [winfo width .c]          ;# calculate appropriate dimensions
set h [winfo height .c]
set r [expr {min($w,$h) * 0.45}]

set points [lmap n {0 1 2 3 4 5} {
    set n [expr {$n * 2}]
    set y [expr {sin($pi * 2 * $n / 5) * $r + $h / 2}]
    set x [expr {cos($pi * 2 * $n / 5) * $r + $w / 2}]
    list $x $y
}]
set points [concat {*}$points]  ;# flatten the list

puts [.c create line $points]

;# a fun reader exercise is to make the shape respond to mouse events,
;# or animate it!

```



## VBA


```vb
Sub pentagram()
    With ActiveSheet.Shapes.AddShape(msoShape5pointStar, 10, 10, 400, 400)
        .Fill.ForeColor.RGB = RGB(255, 0, 0)
        .Line.Weight = 3
        .Line.ForeColor.RGB = RGB(0, 0, 255)
    End With
End Sub
```


## zkl

Generate an SVG file to STDOUT. Redirect to a file to capture and display it.

```zkl
const DIM=200, SIDES=5, A=360/SIDES, R=DIM.toFloat();
vs:=[0.0..360-A,A].apply("toRad");  // angles of vertices
#<<<
0'|<?xml version="1.0" standalone="no" ?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN"
"http://www.w3.org/TR/2001/PR-SVG-20010719/DTD/svg10.dtd">
<svg height="%d" width="%d" style="" xmlns="http://www.w3.org/2000/svg">
<rect height="100%" width="100%" style="fill:bisque;" />|
#<<<
.fmt(DIM*2, DIM*2).println();
 
var vertices=vs.pump(List,fcn(a){ R.toRectangular(a) }); //( (x,y), (x,y)...
SIDES.pump(String,pline).println();  // the line pairs that draw the pentagram

fcn pline(n){ a:=(n + 2)%SIDES; // (n,a) are the endpoints of the right leg
   pts:=String("\"", ("% 0.3f,% 0.3f "*2), "\" "); // two points
   vs:='wrap(){ T(n,a).pump(List,vertices.get).flatten() }; //(x,y, x,y)
   String(
     (0'|<polyline points=| + pts).fmt(vs().xplode()),
      0'|style="fill:seashell; stroke:blue; stroke-width:3;" |,
      0'|transform="translate(%d,%d) rotate(-18)"|.fmt(DIM,DIM),
      " />\n"
   );
}
println("</svg>");
```

```txt

$ zkl bbb > pentagram.svg 
$ cat pentagram.svg 
<?xml version="1.0" standalone="no" ?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN"
"http://www.w3.org/TR/2001/PR-SVG-20010719/DTD/svg10.dtd">
<svg height="400" width="400" style="" xmlns="http://www.w3.org/2000/svg">
<rect height="100%" width="100%" style="fill:bisque;" />
<polyline points=" 200.000, 0.000 -161.803, 117.557 " style="fill:seashell; stroke:blue; stroke-width:3;" transform="translate(200,200) rotate(-18)" />
<polyline points=" 61.803, 190.211 -161.803,-117.557 " style="fill:seashell; stroke:blue; stroke-width:3;" transform="translate(200,200) rotate(-18)" />
<polyline points="-161.803, 117.557  61.803,-190.211 " style="fill:seashell; stroke:blue; stroke-width:3;" transform="translate(200,200) rotate(-18)" />
<polyline points="-161.803,-117.557  200.000, 0.000 " style="fill:seashell; stroke:blue; stroke-width:3;" transform="translate(200,200) rotate(-18)" />
<polyline points=" 61.803,-190.211  61.803, 190.211 " style="fill:seashell; stroke:blue; stroke-width:3;" transform="translate(200,200) rotate(-18)" />

</svg>

```

Until local image uploading is re-enabled, see [http://www.zenkinetic.com/Images/RosettaCode/pentagram.zkl.svg this image].

