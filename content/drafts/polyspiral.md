+++
title = "Polyspiral"
description = ""
date = 2019-10-14T00:46:17Z
aliases = []
[extra]
id = 20435
[taxonomies]
categories = []
tags = []
+++

{{task}}

A [http://www.otherwise.com/Jurtle/screenshots_win/assets/DisplayWindow_Full.png Polyspiral] is a spiral made of multiple line segments, whereby each segment is larger (or smaller) than the previous one by a given amount. Each segment also changes direction at a given angle. 


;Task
Animate a series of polyspirals, by drawing a complete spiral then incrementing the angle, and (after clearing the background) drawing the next, and so on. Every spiral will be a frame of the animation. The animation may stop as it goes full circle or continue indefinitely. The given input values may be varied.

If animation is not practical in your programming environment, you may show a single frame instead.


;Pseudo code

```txt

    set incr to 0.0

    // animation loop
    WHILE true 

        incr = (incr + 0.05) MOD 360
        x = width / 2
        y = height / 2
        length = 5
        angle = incr

        // spiral loop
        FOR 1 TO 150
            drawline
            change direction by angle
            length = length + 3
            angle = (angle + incr) MOD 360
        ENDFOR
    

```






## C

Straightforward implementation of the pseudocode, incr and angle are integers and incr is incremented by 5 instead of 0.05 as the % operation in C is not defined for non-integers. Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.

```C

#include<graphics.h>
#include<math.h>

#define factor M_PI/180
#define LAG 1000

void polySpiral(int windowWidth,int	windowHeight){
	int incr = 0, angle, i, length;
	double x,y,x1,y1;
	
	while(1){
		incr = (incr + 5)%360; 
		
		x = windowWidth/2;
		y = windowHeight/2;
		
		length = 5;
		angle = incr;
		
		for(i=1;i<=150;i++){
			x1 = x + length*cos(factor*angle);
			y1 = y + length*sin(factor*angle);
			line(x,y,x1,y1);
			
			length += 3;
			
			angle = (angle + incr)%360;
			
			x = x1;
			y = y1;
		}
		delay(LAG);
		cleardevice();	
	}
}	

int main()
{
	initwindow(500,500,"Polyspiral");
	
	polySpiral(500,500);
	
	closegraph();
	
	return 0;
}

```



## C++

This Windows programm has no animation, it will simply save 100 bitmaps onto your harddrive

```cpp

#include <windows.h>
#include <sstream>
#include <ctime>

const float PI = 3.1415926536f, TWO_PI = 2.f * PI;
class vector2
{
public:
    vector2( float a = 0, float b = 0 ) { set( a, b ); }
    void set( float a, float b ) { x = a; y = b; }
    void rotate( float r ) {
        float _x = x, _y = y,
               s = sinf( r ), c = cosf( r ),
               a = _x * c - _y * s, b = _x * s + _y * c;
        x = a; y = b;
    }
    vector2 add( const vector2& v ) {
        x += v.x; y += v.y;
        return *this;
    }
    float x, y;
};
class myBitmap
{
public:
    myBitmap() : pen( NULL ), brush( NULL ), clr( 0 ), wid( 1 ) {}
    ~myBitmap(){
        DeleteObject( pen );
        DeleteObject( brush );
        DeleteDC( hdc );
        DeleteObject( bmp );
    }
    bool create( int w, int h ){
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
    void clear( BYTE clr = 0 ){
        memset( pBits, clr, width * height * sizeof( DWORD ) );
    }
    void setBrushColor( DWORD bClr ){
        if( brush ) DeleteObject( brush );
        brush = CreateSolidBrush( bClr );
        SelectObject( hdc, brush );
    }
    void setPenColor( DWORD c ){
        clr = c; createPen();
    }
    void setPenWidth( int w ){
        wid = w; createPen();
    }
    void saveBitmap( std::string path ){
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
    void createPen(){
        if( pen ) DeleteObject( pen );
        pen = CreatePen( PS_SOLID, wid, clr );
        SelectObject( hdc, pen );
    }
    HBITMAP bmp; HDC hdc;
    HPEN pen; HBRUSH brush;
    void *pBits; int width, height, wid;
    DWORD clr;
};
int main( int argc, char* argv[] ) {
    srand( unsigned( time( 0 ) ) );
    myBitmap bmp;
    bmp.create( 600, 600 ); bmp.clear();
    HDC dc = bmp.getDC();
    float fs = ( TWO_PI ) / 100.f;
    int index = 0;
    std::string a = "f://users//images//test", b;
    float ang, len;
    vector2 p1, p2;

    for( float step = 0.1f; step < 5.1f; step += .05f ) {
        ang = 0; len = 2;
        p1.set( 300, 300 );
        bmp.setPenColor( RGB( rand() % 50 + 200, rand() % 300 + 220, rand() % 50 + 200 ) );
        for( float xx = 0; xx < TWO_PI; xx += fs ) {
            MoveToEx( dc, (int)p1.x, (int)p1.y, NULL );
            p2.set( 0, len ); p2.rotate( ang ); p2.add( p1 );
            LineTo( dc, (int)p2.x, (int)p2.y );
            p1 = p2; ang += step; len += step;
        }
        std::ostringstream ss; ss << index++;
        b = a + ss.str() + ".bmp";
        bmp.saveBitmap( b );
        bmp.clear();
    }
    return 0;
}

```



## C#

{{trans|Java}}

```csharp
using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;
using System.Windows.Threading;

namespace Polyspiral
{
    public partial class Form1 : Form
    {
        private double inc;

        public Form1()
        {
            Width = Height = 640;
            StartPosition = FormStartPosition.CenterScreen;
            SetStyle(
                ControlStyles.AllPaintingInWmPaint |
                ControlStyles.UserPaint |
                ControlStyles.DoubleBuffer,
                true);

            var timer = new DispatcherTimer();
            timer.Tick += (s, e) => { inc = (inc + 0.05) % 360; Refresh(); };
            timer.Interval = new TimeSpan(0, 0, 0, 0, 40);
            timer.Start();
        }

        private void DrawSpiral(Graphics g, int len, double angleIncrement)
        {
            double x1 = Width / 2;
            double y1 = Height / 2;
            double angle = angleIncrement;

            for (int i = 0; i < 150; i++)
            {
                double x2 = x1 + Math.Cos(angle) * len;
                double y2 = y1 - Math.Sin(angle) * len;
                g.DrawLine(Pens.Blue, (int)x1, (int)y1, (int)x2, (int)y2);
                x1 = x2;
                y1 = y2;

                len += 3;

                angle = (angle + angleIncrement) % (Math.PI * 2);
            }
        }

        protected override void OnPaint(PaintEventArgs args)
        {
            var g = args.Graphics;
            g.SmoothingMode = SmoothingMode.AntiAlias;
            g.Clear(Color.White);

            DrawSpiral(g, 5, ToRadians(inc));
        }

        private double ToRadians(double angle)
        {
            return Math.PI * angle / 180.0;
        }
    }
}
```



## Ceylon

Be sure to import javafx.graphics and ceylon.numeric in your module.ceylon file.

```ceylon
import javafx.application {
    Application
}
import javafx.stage {
    Stage
}
import javafx.animation {
    AnimationTimer
}
import ceylon.numeric.float {
    remainder,
    cos,
    sin,
    toRadians
}
import javafx.scene.layout {
    BorderPane
}
import javafx.scene.canvas {
    Canvas
}
import javafx.scene {
    Scene
}
import javafx.scene.paint {
    Color
}

shared void run() {
    Application.launch(`PolySpiralApp`);
}

shared class PolySpiralApp() extends Application() {

    value width = 600.0;
    value height = 600.0;

    variable value incr = 0.0;

    shared actual void start(Stage primaryStage) {

        value canvas = Canvas(width, height);
        value graphics = canvas.graphicsContext2D;

        object extends AnimationTimer() {

            shared actual void handle(Integer now) {

                incr = remainder(incr + 0.05, 360.0);

                variable value x = width / 2.0;
                variable value y = width / 2.0;
                variable value length = 5.0;
                variable value angle = incr;

                graphics.fillRect(0.0, 0.0, width, height);
                graphics.beginPath();
                graphics.moveTo(x, y);

                for (i in 1..150) {
                    value radians = toRadians(angle);
                    x = x + cos(radians) * length;
                    y = y + sin(radians)  * length;
                    graphics.stroke = Color.hsb(angle, 1.0, 1.0);
                    graphics.lineTo(x, y);
                    length += 3;
                    angle = remainder(angle + incr, 360.0);
                }

                graphics.stroke();
            }
        }.start();

        value root = BorderPane();
        root.center = canvas;
        value scene = Scene(root);
        primaryStage.title = "poly-spiral";
        primaryStage.setScene(scene);
        primaryStage.show();
    }
}
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Polyspiral this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Gnuplot

{{Works with|gnuplot|5.0 (patchlevel 3) and above}}

===Plotting a polyspiral file-function for the load command===
'''plotpoly.gp''' file for the load command is the only possible imitation of the fine function in the '''gnuplot'''.

```gnuplot

## plotpoly.gp 1/10/17 aev
## Plotting a polyspiral and writing to the png-file. 
## Note: assign variables: rng, d, clr, filename and ttl (before using load command).
## Direction d (-1 clockwise / 1 counter-clockwise)
reset
set terminal png font arial 12 size 640,640
ofn=filename.".png"
set output ofn
unset border; unset xtics; unset ytics; unset key;
set title ttl font "Arial:Bold,12"
set parametric
c=rng*pi; set xrange[-c:c]; set yrange[-c:c];
set dummy t
plot [0:c] t*cos(d*t), t*sin(d*t) lt rgb @clr
set output

```
 


### Plotting many versions of a polyspiral.

'''Note:''' only 6 versions have pictures here on RC.
[[File:PS0gp.png|right|thumb|Output PS0gp.png]]
[[File:PS1gp.png|right|thumb|Output PS1gp.png]]
[[File:PS3gp.png|right|thumb|Output PS3gp.png]]
[[File:PS4gp.png|right|thumb|Output PS4gp.png]]
[[File:PS5gp.png|right|thumb|Output PS5gp.png]]
[[File:PS6gp.png|right|thumb|Output PS6gp.png]]


```gnuplot

## PSpirals.gp 1/10/17 aev
## Plotting many polyspiral pictures.
## Note: assign variables: rng, d, clr, filename and ttl (before using load command).
## Direction d (-1 clockwise / 1 counter-clockwise)
#cd 'C:\gnupData'

##PS0 smooth spiral (not a polyspiral)
reset
set terminal png font arial 12 size 640,640
set output "PS0gp.png"
set title "Smooth spiral  #0 rng=10" font "Arial:Bold,12"
set parametric
c=10*pi; set trange [0:c]; set xrange[-c:c]; set yrange[-c:c];
set samples 1000
plot t*cos(t), t*sin(t) lt rgb "red"
set output

##PS1 A polyspiral (Same size as PS0).
rng=10; d=1; clr = '"dark-green"';
filename = "PS1gp"; ttl = "Polyspiral #1 rng=10";
load "plotpoly.gp"

##PS3 A polyspiral
rng=20; d=-1; clr = '"red"';
filename = "PS3gp"; ttl = "Polyspiral #3 rng=20";
load "plotpoly.gp"

##PS4 A polyspiral having 4 secondary spirals.
rng=50; d=-1; clr = '"navy"';
filename = "PS4gp"; ttl = "Polyspiral #4 rng=50";
load "plotpoly.gp"

##PS5 Not a polyspiral, but has 8 secondary spirals.
rng=75; d=-1; clr = '"navy"';
filename = "PS5gp"; ttl = "Polyspiral #5 rng=75";
load "plotpoly.gp"

##PS6 Not a polyspiral, just a nice figure (seen in zkl).
rng=100; d=-1; clr = '"navy"';
filename = "PS6gp"; ttl = "Polyspiral #6 rng=100";
load "plotpoly.gp"

##
### ========================

#### NO PICTURES on RC starting from here, test it yourself

##PS2 A polyspiral 
rng=20; d=1; clr = '"red"';
filename = "PS2gp"; ttl = "Polyspiral #2 rng=20";
load "plotpoly.gp"

##PS7 Looks like PS5, but has 5 secondary spirals (not 8)
rng=120; d=-1; clr = '"green"';
filename = "PS7gp"; ttl = "Polyspiral #7 rng=120";
load "plotpoly.gp"

##PS8 Looks like PS4, but more distortion.
rng=150; d=-1; clr = '"green"';
filename = "PS8gp"; ttl = "Polyspiral #8 rng=150";
load "plotpoly.gp"

##PS9 Looks like PS2, but less loops..
rng=175; d=-1; clr = '"green"';
filename = "PS9gp"; ttl = "Polyspiral #9 rng=175";
load "plotpoly.gp"

##PS10 One loop of a spiral
rng=200; d=-1; clr = '"green"';
filename = "PS10gp"; ttl = "Polyspiral #10 rng=200";
load "plotpoly.gp"

##PS11 Polyspiral with line segments crossing other line segments.
rng=30; d=-1; clr = '"navy"';
filename = "PS11gp"; ttl = "Polyspiral #11 rng=30";
load "plotpoly.gp"

##PS12 Looks like PS4, but has 5 secondary spirals.
rng=40; d=-1; clr = '"navy"';
filename = "PS12gp"; ttl = "Polyspiral #12 rng=40";
load "plotpoly.gp"

##PS13 Looks like PS5, but has 8 secondary spirals.
rng=60; d=-1; clr = '"navy"';
filename = "PS13gp"; ttl = "Polyspiral #13 rng=60";
load "plotpoly.gp"

##PS14 Looks like PS4, but has 5 secondary spirals.
rng=80; d=-1; clr = '"navy"'
filename = "PS14gp"; ttl = "Polyspiral #14 rng=80";
load "plotpoly.gp"

##PS15 Not a polyspiral. Hmmm, just a star?
rng=90; d=-1; clr = '"navy"';
filename = "PS15gp"; ttl = "Polyspiral #15 rng=90";
load "plotpoly.gp"

##PS16 Not a polyspiral. Hmmm, just another star?
rng=300; d=-1; clr = '"navy"';
filename = "PS16gp"; ttl = "Polyspiral #16 rng=300";
load "plotpoly.gp"

## Continue plotting starting with a range rng=110 to 400+ step 10 to discover new figures.
## END ##

```
 
{{Output}}

```txt

1. All PSpirals.gp file commands.
2. First 6 plotted png-files: PS0gp.png - PS6gp.

```


===Plotting a polyspiral file-function for the load command (for animation)===
'''plotpolya.gp''' file for the load command is the only possible imitation of the fine function in the '''gnuplot'''.

```gnuplot

## plotpolya.gp 1/19/17 aev 
## Plotting a polyspiral and writing to the png-file. Simple plot for animation.
## Note: assign variables: rng, d, clr, filename (before using load command).
## ====  NO ttl (title) in this version.
reset
set terminal png font arial 12 size 640,640
ofn=filename.".png"
set output ofn
unset border; unset xtics; unset ytics; unset key;
set parametric
c=rng*pi; set xrange[-c:c]; set yrange[-c:c];
set dummy t
plot [0:c] t*cos(d*t), t*sin(d*t) lt rgb @clr
set output

```
 


### Plotting many polyspiral and other pictures for animation.

'''Note:''' No generated pictures here on RC.

```gnuplot

## PSpirals4a.gp 1/19/17 aev
## Plotting many polyspiral and other pictures for animation
## Notes: 1. Assign variables: rng, d, clr, filename(before using load command).
## ====== 2. NO title in this version.
##        3. Primarily range is changed.
## Direction d (-1 clockwise / 1 counter-clockwise)
#cd 'C:\gnupData'

##
### === for PolySpirsAnim.gif =======================================

##PS0 A polyspiral (direction: counter-clockwise).
rng=10; d=1; clr = '"red"'; filename = "PS0"; load "plotpolya.gp";

##PS1 A polyspiral (direction: clockwise).
rng=20; d=-1; clr = '"red"'; filename = "PS1"; load "plotpolya.gp";

##PS2 A polyspiral. Looks like PS1, but less loops..
rng=175; d=-1; clr = '"red"'; filename = "PS2"; load "plotpolya.gp";

##PS3 Polyspiral with line segments crossing other line segments.
rng=30; d=-1; clr = '"red"'; filename = "PS3"; load "plotpolya.gp";

##PS4 A polyspiral having 4 secondary spirals.
rng=50; d=-1; clr = '"red"'; filename = "PS4"; load "plotpolya.gp";

##PS5 A polyspiral. Looks like PS4, but has 5 secondary spirals.
rng=40; d=-1; clr = '"red"'; filename = "PS5"; load "plotpolya.gp";

##PS6 A polyspiral. Looks like PS4, but has more distortion.
rng=150; d=-1; clr = '"red"'; filename = "PS6"; load "plotpolya.gp";

##PS7 A polyspiral. Has 8 secondary spirals and even more distortion.
rng=60; d=-1; clr = '"red"'; filename = "PS7"; load "plotpolya.gp";


##
### === for NiceFigsAnim.gif =======================================

##PS8 Not a polyspiral, but has 8 secondary spirals.
rng=75; d=-1; clr = '"navy"'; filename = "PS8"; load "plotpolya.gp";

##PS9 Looks like PS8, but has 5 secondary spirals.
rng=80; d=-1; clr = '"navy"'; filename = "PS9"; load "plotpolya.gp";

##PS10 Looks like PS8, but has 5 secondary spirals (not 8)
rng=120; d=-1; clr = '"navy"'; filename = "PS10"; load "plotpolya.gp";

##PS11 Not a polyspiral, just nice figure.
rng=100; d=-1; clr = '"navy"'; filename = "PS11"; load "plotpolya.gp";

##PS12 Not a polyspiral. Hmmm, just a star?
rng=90; d=-1; clr = '"navy"'; filename = "PS12"; load "plotpolya.gp";

##PS13 Not a polyspiral. Hmmm, just another star?
rng=300; d=-1; clr = '"navy"'; filename = "PS13"; load "plotpolya.gp";

##PS14 Not a polyspiral, but has many short secondary spirals.
rng=700; d=-1; clr = '"navy"'; filename = "PS14"; load "plotpolya.gp";

```
 
{{Output}}

```txt

1. All PSpirals4a.gp file commands.
2. 15 plotted png-files: PS0.png - PS14.png.

```


===Creating 2 animated gif-files.===
'''Note:''' No gif-files. File upload still not allowed. So, test it yourself.
[[File:PolySpirsAnim.gif|right|thumb|Output PolySpirsAnim.gif]]
[[File:NiceFigsAnim.gif|right|thumb|Output NiceFigsAnim.gif]]


```gnuplot

## Animation for polyspirals PS0 - PS6
reset
set terminal gif animate delay 100 loop 2 size 640,640
set output 'PolySpirsAnim.gif'
unset border; unset xtics; unset ytics; unset key;
unset autoscale
set xrange[0:640]
set yrange[0:640]
do for [i=0:6]{plot 'PS'.i.'.png' binary filetype=png with rgbimage}
set output

## Animation for nice figures PS8 - PS14
reset
set terminal gif animate delay 100 loop 2 size 640,640
set output 'NiceFigsAnim.gif'
unset border; unset xtics; unset ytics; unset key;
unset autoscale
set xrange[0:640]
set yrange[0:640]
do for [i=8:14]{plot 'PS'.i.'.png' binary filetype=png with rgbimage}
set output

```
 
{{Output}}

```txt

2 created gif-files: PolySpirsAnim.gif and NiceFigsAnim.gif

```


===Showing 2 animated gif-files.===
Create 2 the following html-files and envoke them in browser.

```html

<!-- PolySpirsAnim.html -->
<html><body>
  <h3>Gnuplot: Polyspirals animation  >>
  <a href="NiceFigsAnim.html">Next: Nice figures animation</a></h3>
  <img src="PolySpirsAnim.gif">
</body></html>

```
 

```html

<!-- NiceFigsAnim.html -->
<html><body>
  <h3>Gnuplot: Nice figures animation  >>
  <a href="PolySpirsAnim.html">Next: Polyspirals animation</a></h3>
  <img src="NiceFigsAnim.gif">
</body></html>

```
 
{{Output}}

```txt

2 pages with animation.

```



## Go

This uses Go's 'image' packages in its standard library to create an animated GIF. 

When played this is similar to the Java entry but much quicker. The whole animation completes in 72 seconds and repeats indefinitely.

Although the .gif works fine in Firefox it might not do so in EOG due to optimizations made during its creation. If so, then the following ImageMagick command should fix it:

```txt

  $ convert polyspiral.gif -coalesce polyspiral2.gif
  $ eog polyspiral2.gif

```


```go
package main

import (
    "image"
    "image/color"
    "image/gif"
    "log" 
    "math"
    "os"
)

func drawLine(img *image.Paletted, x1, y1, x2, y2 int, ci uint8) {
    var first, last int
    if x2 != x1 {
        m := float64(y2-y1) / float64(x2-x1)
        if x1 < x2 {
            first, last = x1, x2
        } else {
            first, last = x2, x1
        }
        for x := first; x <= last; x++ {
            y := int(m*float64(x-x1)+0.5) + y1
            img.SetColorIndex(x, y, ci)
        }
    } else {
        if y1 < y2 {
            first, last = y1, y2
        } else {
            first, last = y2, y1
        }
        for y := first; y <= last; y++ {
            img.SetColorIndex(x1, y, ci)
        }
    }
}

func setBackgroundColor(img *image.Paletted, w, h int, ci uint8) {
    for x := 0; x < w; x++ {
        for y := 0; y < h; y++ {
            img.SetColorIndex(x, y, ci)
        }
    }
}

func hsb2rgb(hue, sat, bri float64) (r, g, b int) {
    u := int(bri*255 + 0.5)
    if sat == 0 {
        r, g, b = u, u, u
    } else {
        h := (hue - math.Floor(hue)) * 6
        f := h - math.Floor(h)
        p := int(bri*(1-sat)*255 + 0.5)
        q := int(bri*(1-sat*f)*255 + 0.5)
        t := int(bri*(1-sat*(1-f))*255 + 0.5)
        switch int(h) {
        case 0:
            r, g, b = u, t, p
        case 1:
            r, g, b = q, u, p
        case 2:
            r, g, b = p, u, t
        case 3:
            r, g, b = p, q, u
        case 4:
            r, g, b = t, p, u
        case 5:
            r, g, b = u, p, q
        }
    }
    return
}

func main() {
    const degToRad = math.Pi / 180
    const nframes = 360
    const delay = 20 // 200ms
    width, height := 640, 640
    anim := gif.GIF{LoopCount: nframes}
    rect := image.Rect(0, 0, width, height)
    palette := make([]color.Color, 151)
    palette[0] = color.White
    for i := 1; i <= 150; i++ {
        r, g, b := hsb2rgb(float64(i)/150, 1, 1)
        palette[i] = color.RGBA{uint8(r), uint8(g), uint8(b), 255}
    }
    incr := 0
    for f := 1; f <= nframes; f++ {
        incr = (incr + 1) % 360
        x1, y1 := width/2, height/2
        length := 5.0
        img := image.NewPaletted(rect, palette)
        setBackgroundColor(img, width, height, 0) // white background
        angle := incr
        for ci := uint8(1); ci <= 150; ci++ {
            x2 := x1 + int(math.Cos(float64(angle)*degToRad)*length)
            y2 := y1 - int(math.Sin(float64(angle)*degToRad)*length)
            drawLine(img, x1, y1, x2, y2, ci)
            x1, y1 = x2, y2
            length += 3
            angle = (angle + incr) % 360
        }
        anim.Delay = append(anim.Delay, delay)
        anim.Image = append(anim.Image, img)
    }
    file, err := os.Create("polyspiral.gif")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close() 
    if err2 := gif.EncodeAll(file, &anim); err != nil {
        log.Fatal(err2)
    }
}
```



## Haskell

{{Works with|Chrome and Firefox}}

This implementation compiles to javascript that runs in the browser using the [https://github.com/ghcjs/ghcjs ghcjs compiler ] .  The [https://github.com/reflex-frp/reflex-dom reflex-dom ] library is used to help with svg rendering and animation.


```haskell
{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Reflex.Dom.Time
import Data.Text (Text, pack) 
import Data.Map (Map, fromList)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)

type Point = (Float,Float)
type Segment = (Point,Point)

main = mainWidget $ do 

  -- An event that fires every 0.05 seconds.
  dTick <- tickLossy 0.05 =<< liftIO getCurrentTime 

  -- A dynamically updating counter.
  dCounter <- foldDyn (\_ c -> c+1) (0::Int) dTick

  let 
      -- A dynamically updating angle.
      dAngle = fmap (\c -> fromIntegral c / 800.0) dCounter

      -- A dynamically updating spiral
      dSpiralMap = fmap toSpiralMap dAngle

      -- svg parameters
      width = 600
      height = 600

      boardAttrs = 
         fromList [ ("width" , pack $ show width)
                  , ("height", pack $ show height)
                  , ("viewBox", pack $ show (-width/2) ++ " " ++ show (-height/2) ++ " " ++ show width ++ " " ++ show height)
                  ]

  elAttr "h1" ("style" =: "color:black") $ text "Polyspiral" 
  elAttr "a" ("href" =: "http://rosettacode.org/wiki/Polyspiral#Haskell") $ text "Rosetta Code / Polyspiral / Haskell"

  el "br" $ return ()
  elSvgns "svg" (constDyn boardAttrs) (listWithKey dSpiralMap showLine)

  return ()

-- The svg attributes needed to display a line segment.
lineAttrs :: Segment -> Map Text Text
lineAttrs ((x1,y1), (x2,y2)) =
  fromList [ ( "x1",    pack $ show x1)
           , ( "y1",    pack $ show y1)
           , ( "x2",    pack $ show x2)
           , ( "y2",    pack $ show y2)
           , ( "style", "stroke:blue")
           ]    

-- Use svg to display a line segment.
showLine :: MonadWidget t m => Int -> Dynamic t Segment -> m ()
showLine _ dSegment = elSvgns "line" (lineAttrs <$> dSegment) $ return ()

-- Given a point and distance/bearing , get the next point
advance :: Float -> (Point, Float, Float) -> (Point, Float, Float)
advance angle ((x,y), len, rot) = 
  let new_x = x + len * cos rot
      new_y = y + len * sin rot
      new_len = len + 3.0 
      new_rot = rot + angle
  in ((new_x, new_y), new_len, new_rot)

-- Given an angle, generate a map of segments that form a spiral.
toSpiralMap :: Float -> Map Int ((Float,Float),(Float,Float))
toSpiralMap angle =
      fromList                       -- changes list to map (for listWithKey)
  $   zip [0..]                      -- annotates segments with index
  $   (\pts -> zip pts $ tail pts)   -- from points to line segments
  $   take 80                        -- limit the number of points
  $   (\(pt,_,_) -> pt)              -- cull out the (x,y) values
  <$> iterate (advance angle) ((0, 0), 0, 0)  -- compute the spiral

-- Display an element in svg namespace
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
elSvgns t m ma = do
    (el, val) <- elDynAttrNS' (Just "http://www.w3.org/2000/svg") t m ma
    return val
```


Link to live demo: https://dc25.github.io/rosettaCode__Polyspiral_haskell/ 

=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "PolySp.bas"
110 OPTION ANGLE DEGREES
120 LET CH=2
130 SET VIDEO X 40:SET VIDEO Y 27:SET VIDEO MODE 1:SET VIDEO COLOR 0
140 OPEN #1:"video:"
150 OPEN #2:"video:"
160 WHEN EXCEPTION USE POSERROR
170   FOR ANG=40 TO 150 STEP 2
180     IF CH=2 THEN
190       LET CH=1
200     ELSE
210       LET CH=2
220     END IF
230     CLEAR #CH
240     PLOT #CH:640,468,ANGLE 180;
250     FOR D=12 TO 740 STEP 4
260       PLOT #CH:FORWARD D,RIGHT ANG;
270     NEXT
280     DISPLAY #CH:AT 1 FROM 1 TO 27
290   NEXT
300 END WHEN
310 HANDLER POSERROR
320   LET D=740
330   CONTINUE
340 END HANDLER
```



## J

[[File:J-polyspiral.gif|200px|thumb|right]]
{{trans|java}}


```J
require 'gl2 trig media/imagekit'
coinsert 'jgl2'
 
DT       =: %30       NB. seconds
ANGLE    =: 0.025p1   NB. radians
DIRECTION=: 0         NB. radians
 
POLY=: noun define
  pc poly;pn "Poly Spiral";
  minwh 320 320; cc isi isigraph;
)
 
poly_run=: verb define 
  wd POLY,'pshow'
  wd 'timer ',":DT * 1000
)
 
poly_close=: verb define
  wd 'timer 0; pclose'
)
 
sys_timer_z_=: verb define
  recalcAngle_base_ ''
  wd 'psel poly; set isi invalid'
)
 
poly_isi_paint=: verb define
  drawPolyspiral DIRECTION
)
 
recalcAngle=: verb define
  DIRECTION=: 2p1 | DIRECTION + ANGLE
)
 
drawPolyspiral=: verb define
  glclear''
  x1y1 =. (glqwh'')%2
  a=. -DIRECTION
  len=. 5
  for_i. i.150 do.
    glpen glrgb Hue a % 2p1
    x2y2=. x1y1 + len*(cos,sin) a
    gllines <.x1y1,x2y2
    x1y1=. x2y2
    len=. len+3
    a=. 2p1 | a - DIRECTION
  end.
)
 
poly_run''
```


Note that we're using a lot of [[j:Guides/Window_Driver/Command_Reference|wd]] commands here. You'll need to be running [[j:System/Installation|jqt]] for this to work.


## Java

[[File:Polyspiral_java2.png|300px|thumb|right]]
{{works with|Java|8}}

```java
import java.awt.*;
import java.awt.event.ActionEvent;
import javax.swing.*;

public class PolySpiral extends JPanel {
    double inc = 0;

    public PolySpiral() {
        setPreferredSize(new Dimension(640, 640));
        setBackground(Color.white);

        new Timer(40, (ActionEvent e) -> {
            inc = (inc + 0.05) % 360;
            repaint();
        }).start();
    }

    void drawSpiral(Graphics2D g, int len, double angleIncrement) {

        double x1 = getWidth() / 2;
        double y1 = getHeight() / 2;
        double angle = angleIncrement;

        for (int i = 0; i < 150; i++) {

            g.setColor(Color.getHSBColor(i / 150f, 1.0f, 1.0f));

            double x2 = x1 + Math.cos(angle) * len;
            double y2 = y1 - Math.sin(angle) * len;
            g.drawLine((int) x1, (int) y1, (int) x2, (int) y2);
            x1 = x2;
            y1 = y2;

            len += 3;

            angle = (angle + angleIncrement) % (Math.PI * 2);
        }
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawSpiral(g, 5, Math.toRadians(inc));
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("PolySpiral");
            f.setResizable(true);
            f.add(new PolySpiral(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

===Version #1 - Plain===
This Polyspiral Generator page alows user to enjoy hundreds of polyspirals in different colors.

This is inspired by a discovery made while using the gnuplot. 
(See [[Talk:Polyspiral| Discussion ]] for Polyspiral task.)

'''Note:'''
* Some polyspirals would be degenerated to a single branch of it or even to a single line.

* An image uploading is still blocked. But you have a browser!? So, copy/paste/save this page and double click it.
{{Works with|Chrome}} (or any other browser supporting Canvas tag)

```html

<!-- Polyspiral.html -->
<html>
<head><title>Polyspiral Generator</title></head>
<script>
// Basic function for family of Polyspirals
// Where: rng - range (prime parameter), w2 - half of canvas width,
//        d - direction (1 - clockwise, -1 - counter clockwise).
function ppsp(ctx, rng, w2, d) {
  // Note: coefficients c, it, sc, sc2, sc3 are selected to fit canvas.
  var c=Math.PI*rng, it=c/w2, sc=2, sc2=50, sc3=0.1, t, x, y;
  console.log("Polyspiral PARs rng,w2,d:", rng, "/", w2, "/", d);
  if (rng>1000) {sc=sc3}
  ctx.beginPath();
  for(var i=0; i<sc2*c; i++) {
    t=it*i;
    x = sc*t*Math.cos(d*t)+w2; y = sc*t*Math.sin(d*t)+w2;
    ctx.lineTo(x, y);
  }//fend i
  ctx.stroke();
}
// ******************************************
// pspiral() - Generating and plotting Polyspirals
function pspiral() {
  // Setting basic vars for canvas and inpu parameters
  var cvs = document.getElementById('cvsId');
  var ctx = cvs.getContext("2d");
  var w = cvs.width, h = cvs.height;
  var w2=w/2;
  var clr = document.getElementById("color").value; // color
  var d = document.getElementById("dir").value;     // direction
  var rng = document.getElementById("rng").value;   // range
  rng=Number(rng);
  ctx.fillStyle="white"; ctx.fillRect(0,0,w,h);
  ctx.strokeStyle=clr;
  // Plotting spiral.
  ppsp(ctx, rng, w2, d)
}//func end
</script></head>
<body style="font-family: arial, helvatica, sans-serif;">
  <b>Color: </b>
  <select id="color">
    <option value="red">red</option>
    <option value="darkred" selected>darkred</option>
    <option value="green">green</option>
    <option value="darkgreen">darkgreen</option>
    <option value="blue">blue</option>
    <option value="navy">navy</option>
    <option value="brown">brown</option>
    <option value="maroon">maroon</option>
    <option value="black">black</option>
  </select>  
  <b>Direction: </b>
  <input id="dir" value="1" type="number" min="-1" max="1" size="1">  
  <b>Range: </b>
  <input id="rng" value="10" type="number" min="10" max="4000" step="10" size="4">  
  <input type="button" value="Plot it!" onclick="pspiral();">  

  <h3>    Polyspiral</h3>
  <canvas id="cvsId" width="640" height="640" style="border: 2px inset;"></canvas>
</body>
</html>

```

{{Output}} 

```txt

Page with Polyspiral. Right-clicking on the canvas you can save spiral as a png-file, for example.
Try all ranges/colors! But particularly these ranges: 50, 70, 80, 90, 110, 130, 160, 210, 220, 240, 270, 280, 290, 300, 310, 330, 
340, 350, 400, 430, 480, 510, all 1010-2000, a few 3000+, etc.

```


===Version #2 - Animated===
{{trans|Java}}

```javascript
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <style>
        body {
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

        var inc = 0;

        function drawSpiral(len, angleIncrement) {
            var x1 = canvas.width / 2;
            var y1 = canvas.height / 2;
            var angle = angleIncrement;

            for (var i = 0; i < 150; i++) {

                var x2 = x1 + Math.cos(angle) * len;
                var y2 = y1 - Math.sin(angle) * len;

                g.strokeStyle = HSVtoRGB(i / 150, 1.0, 1.0);
                g.beginPath();
                g.moveTo(x1, y1);
                g.lineTo(x2, y2);
                g.stroke();

                x1 = x2;
                y1 = y2;

                len += 3;

                angle = (angle + angleIncrement) % (Math.PI * 2);
            }
        }

        /* copied from stackoverflow */
        function HSVtoRGB(h, s, v) {
            var r, g, b, i, f, p, q, t;

            i = Math.floor(h * 6);
            f = h * 6 - i;
            p = v * (1 - s);
            q = v * (1 - f * s);
            t = v * (1 - (1 - f) * s);
            switch (i % 6) {
                case 0: r = v, g = t, b = p; break;
                case 1: r = q, g = v, b = p; break;
                case 2: r = p, g = v, b = t; break;
                case 3: r = p, g = q, b = v; break;
                case 4: r = t, g = p, b = v; break;
                case 5: r = v, g = p, b = q; break;
            }
            return "rgb("
                + Math.round(r * 255) + ","
                + Math.round(g * 255) + ","
                + Math.round(b * 255) + ")";
        }

        function toRadians(degrees) {
            return degrees * (Math.PI / 180);
        }

        setInterval(function () {
            inc = (inc + 0.05) % 360;
            g.clearRect(0, 0, canvas.width, canvas.height);
            drawSpiral(5, toRadians(inc));
        }, 40);
    </script>

</body>
</html>
```




## Julia


```julia
using Gtk, Graphics, Colors

const can = @GtkCanvas()
const win = GtkWindow(can, "Polyspiral", 360, 360)

const drawiters = 72
const colorseq = [colorant"blue", colorant"red", colorant"green", colorant"black", colorant"gold"]
const angleiters = [0, 0, 0]
const angles = [75, 100, 135, 160]

Base.length(v::Vec2) = sqrt(v.x * v.x + v.y * v.y)

function drawline(ctx, p1, p2, color, width=1)
    move_to(ctx, p1.x, p1.y)
    set_source(ctx, color)
    line_to(ctx, p2.x, p2.y)
    set_line_width(ctx, width)
    stroke(ctx)
end

@guarded draw(can) do widget
    δ(r, θ) = Vec2(r * cospi(θ/180), r * sinpi(θ/180))
    nextpoint(p, r, θ) = (dp = δ(r, θ); Point(p.x + dp.x, p.y + dp.y))
    colorindex = (angleiters[1] % 5) + 1
    colr = colorseq[colorindex]
    ctx = getgc(can)
    h = height(can)
    w = width(can)
    x = 0.5 * w
    y = 0.5 * h
    θ = angleiters[2] * rand() * 3
    δθ = angleiters[2]
    r = 5
    δr = 3
    p1 = Point(x, y)
    for i in 1:drawiters
        if angleiters[3] == 0
            set_source(ctx, colorant"gray90")
            rectangle(ctx, 0, 0, w, h)
            fill(ctx)
            continue
        end
        p2 = nextpoint(p1, r, θ)
        drawline(ctx, p1, p2, colr, 2)
        θ = θ + δθ
        r = r + δr
        p1 = p2
    end
end

show(can)

while true
    angleiters[2] = angles[angleiters[1] % 3 + 1]
    angleiters[1] += 1
    angleiters[3] = angleiters[3] == 0 ? 1 : 0
    draw(can)
    yield()
    sleep(0.5)
end

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.0

import java.awt.*
import java.awt.event.ActionEvent
import javax.swing.*

class PolySpiral() : JPanel() {
    private var inc = 0.0

    init {
        preferredSize = Dimension(640, 640)
        background = Color.white
        Timer(40) {
            inc = (inc + 0.05) % 360.0
            repaint()
        }.start()
    }

    private fun drawSpiral(g: Graphics2D, length: Int, angleIncrement: Double) {
        var x1 = width / 2.0
        var y1 = height / 2.0
        var len = length
        var angle = angleIncrement       
        for (i in 0 until 150) {
            g.setColor(Color.getHSBColor(i / 150f, 1.0f, 1.0f))
            val x2 = x1 + Math.cos(angle) * len
            val y2 = y1 - Math.sin(angle) * len
            g.drawLine(x1.toInt(), y1.toInt(), x2.toInt(), y2.toInt())
            x1 = x2
            y1 = y2
            len += 3
            angle = (angle + angleIncrement) % (Math.PI * 2.0)
        }
    }

    override protected fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON) 
        drawSpiral(g, 5, Math.toRadians(inc))
    } 
} 

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        f.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        f.title = "PolySpiral"
        f.setResizable(true)
        f.add(PolySpiral(), BorderLayout.CENTER)
        f.pack()
        f.setLocationRelativeTo(null)
        f.setVisible(true)
    }
}
```



## Lua

{{libheader|LÖVE}}
LÖVE defaults to animating at sixty frames per second, so the patterns become very complex very quickly.

```Lua
function love.load ()
    love.window.setTitle("Polyspiral")
    incr = 0
end

function love.update (dt)
    incr = (incr + 0.05) % 360
    x1 = love.graphics.getWidth() / 2
    y1 = love.graphics.getHeight() / 2
    length = 5
    angle = incr
end

function love.draw ()
    for i = 1, 150 do
        x2 = x1 + math.cos(angle) * length
        y2 = y1 + math.sin(angle) * length
        love.graphics.line(x1, y1, x2, y2)
        x1, y1 = x2, y2
        length = length + 3
        angle = (angle + incr) % 360
    end
end
```

[[File:love2dPolyspiral.jpg]]


## PARI/GP


{{Works with|PARI/GP|2.7.4 and above}}


### Plotting helper functions

Both versions #1 and #2 are based on using my own small plotting helper functions.
You can find a few others on [http://oeis.org/wiki/User:Anatoly_E._Voevudko/VoeLib.gp#Plotting_helper_functions OEIS Wiki] and here on RC Wiki. 


```parigp

\\ Plot the line from x1,y1 to x2,y2.
plotline(x1,y1,x2,y2,w=0)={plotmove(w, x1,y1);plotrline(w,x2-x1,y2-y1);}
\\ Convert degrees to radians.
rad2(degs)={return(degs*Pi/180.0)}
\\ Convert Polar coordinates to Cartesian.
cartes2(r,a,rndf=0)={my(v,x,y); x=r*cos(a); y=r*sin(a);
  if(rndf==0, return([x,y]), return(round([x,y])))}

```


===Version #1. Polyspiral (a spiral made of multiple line segments).===

In this version function plotpspiral() was translated from Java and J. Some tweaks and options were added to make it reusable and outputting differently looking polyspirals.
There are no animation features in PARI/GP.

[[File:Polyspiral1.png|100px|right|thumb|Output Polyspiral1.png]]
[[File:Polyspiral2.png|100px|right|thumb|Output Polyspiral2.png]]
[[File:Polyspiral3.png|100px|right|thumb|Output Polyspiral3.png]]
[[File:Polyspiral3b.png|100px|right|thumb|Output Polyspiral3b.png]]
[[File:Polyspiral4.png|100px|right|thumb|Output Polyspiral4.png]]


```parigp

\\Polyspiral (a spiral made of multiple line segments)
\\ 4/15/16 aev
plotpspiral(size,lim,ai,d,di,c)={
my(x1,y1,x2,y2,air=ai*Pi,a,sai=Strprintf("%.3f",ai));
print(" *** Polyspiral, size=",size," lim=",lim," ai=",sai," d=",d," di=",di);
x1=0; y1=0;
a=air;
for(i=0, lim,
    if(c==0, x2=x1+cos(a)*d; y2=y1-sin(a)*d,
             x2=x1-sin(a)*d; y2=y1+cos(a)*d;);
    plotline(x1,y1,x2,y2);
    x1=x2; y1=y2; d+=di; a+=air;
   );\\fend i
}

\\ Polyspiral() - Where: ai is an angle increment (in radians), d is a distance/length,
\\ c is a direction 0/1 (clockwise/counter-clockwise); other parameters are self explanative. 
\\ 4/15/16 aev  Last updated: 4/18/16
polyspiral(size,lim,ai,d,di,c=0)={
plotinit(0);
plotcolor(0,3); \\blue
plotscale(0, -size,size, -size,size); 
plotmove(0, 0,0);
plotpspiral(size,lim,ai,d,di,c);
plotdraw([0,size,size]);
}

{\\ Executing:
polyspiral(1500,1500,0.25,9,5);  \\Polyspiral1.png
polyspiral(1500,1500,0.25,3,2);  \\Polyspiral2.png
polyspiral(10000,10000,0.03,3,2);  \\Polyspiral3.png
polyspiral(10000,10000,0.03,3,2,1);  \\Polyspiral3b.png
polyspiral(100000,100000,0.03,3,2);\\Polyspiral4.png
}

```
 

{{Output}}


```txt


> polyspiral(1500,1500,0.25,9,5);  \\Polyspiral1.png
*** Polyspiral, size=1500 lim=1500 ai=0.250 d=9 di=5
 
> polyspiral(1500,1500,0.25,3,2);  \\Polyspiral2.png
*** Polyspiral, size=1500 lim=1500 ai=0.250 d=3 di=2
 
> polyspiral(10000,10000,0.03,3,2);  \\Polyspiral3.png
*** Polyspiral, size=100000 lim=100000 ai=0.030 d=3 di=2

> polyspiral(10000,10000,0.03,3,2,1);  \\Polyspiral3b.png
*** Polyspiral, size=100000 lim=100000 ai=0.030 d=3 di=2

> polyspiral(100000,100000,0.03,3,2);  \\Polyspiral4.png
*** Polyspiral, size=100000 lim=100000 ai=0.030 d=3 di=2


```


===Version #2. Multi-spiral figure translated from zkl.===
This is definitely not a polyspiral, but a very nice "multi-spiral" figure similar to shown in zkl
and in a few other languages. Also, there is a very nice and impressive animation created in zkl,
but not possible in PARI/GP. 

{{trans|zkl}}

[[File:Spiralz1.png|100px|right|thumb|Output Spiralz.png]]


```parigp

\\ plotpspiralz() Multi-spiral figure translated from zkl using my own ploting functions.
\\ 4/15/16 aev
plotpspiralz(size,lim,ai,di,lim2)={
my(x1,y1,u1,v1,air=rad2(ai),a,sai=Strprintf("%.3f",ai),sdi=Strprintf("%.3f",di),
   sz2=size\2,aj,inc,ao,x,y,u,v,vc,r2i=rad2(130.0),d=0.0);
print(" *** Spiralz: size=",size," lim=",lim," ai=",sai," di=",sdi," lim2=",lim2);
x1=0; y1=0; u1=0; v1=0;
for(i=1, lim,
  r=0.0; a=0.0;ao=0.0;
  if(i>1, inc=air+r2i, inc=air);
  for(j=1, lim2,
    d=r+di; aj=a+inc;
    vc=cartes2(r,a); x=vc[1]; y=vc[2];
    vc=cartes2(r,aj); u=vc[1]; v=vc[2];
    plotline(ao+x,ao+y,ao+u,ao+v);
    r=d; a=aj;
  );\\fend j
  air+=0.05;
);\\fend i
}

\\ Spiralz() - Where: ai is an angle increment (in radians), di is a distance/length
\\ increment, other parameters are self explanative. 
\\ 4/15/16 aev
Spiralz(size,lim,ai,di,lim2)={
plotinit(0); plotcolor(0,3); \\blue
plotscale(0, -size,size, -size,size); 
\\plotscale(0, 0,size, 0,size); 
plotmove(0, 0,0);
plotpspiralz(size,lim,ai,di,lim2);
plotdraw([0,size,size]);
}

{\\ Executing:
Spiralz(640,2,3.0,3.0,128);  \\Spiralz1.png
}

```
 

{{Output}}


```txt

> Spiralz(640,2,3.0,3.0,128);  \\Spiralz1.png
 *** Spiralz: size=640 lim=2 ai=3.000 di=3.000 lim2=128

```



## Perl 6

{{works with|Rakudo|2018.09}}

===SVG "pseudo-animation"===
Sort of an ersatz animation. Write updates to a svg file, most modern viewers will update as the content changes. 


```perl6
use SVG;
my $w = 600;
my $h = 600;

for 3..33  -> $a {
    my $angle = $a/τ;
    my $x1 = $w/2;
    my $y1 = $h/2;
    my @lines;

    for 1..144 {
        my $length = 3 * $_;
        my ($x2, $y2) = ($x1, $y1) «+« |cis($angle * $_).reals».round(.01) »*» $length ;
        @lines.push: 'line' => [:x1($x1.clone), :y1($y1.clone), :x2($x2.clone), :y2($y2.clone),
                                :style("stroke:rgb({hsv2rgb(($_*5 % 360)/360,1,1).join: ','})")];
        ($x1, $y1) = $x2, $y2;
    }

    my $fname = "./polyspiral-perl6.svg".IO.open(:w);
    $fname.say( SVG.serialize(
        svg => [
            width => $w, height => $h, style => 'stroke:rgb(0,0,0)',
            :rect[:width<100%>, :height<100%>, :fill<black>],
            |@lines,
        ],)
    );
    $fname.close;
    sleep .15;
}

sub hsv2rgb ( $h, $s, $v ){ # inputs normalized 0-1
    my $c = $v * $s;
    my $x = $c * (1 - abs( (($h*6) % 2) - 1 ) );
    my $m = $v - $c;
    my ($r, $g, $b) = do given $h {
        when   0..^(1/6) { $c, $x, 0 }
        when 1/6..^(1/3) { $x, $c, 0 }
        when 1/3..^(1/2) { 0, $c, $x }
        when 1/2..^(2/3) { 0, $x, $c }
        when 2/3..^(5/6) { $x, 0, $c }
        when 5/6..1      { $c, 0, $x }
    }
    ( $r, $g, $b ).map: ((*+$m) * 255).Int
}
```

{{out}}
See [https://github.com/thundergnat/rc/blob/master/img/polyspiral-perl6.gif polyspiral-perl6.gif]  (offsite animated gif image)


### SDL full animation

Uses the same basic algorithm but fully animated. Use the up / down arrow keys to speed up / slow down the update speed. Use PgUp / PgDn keys to increment / decrement animation speed by large amounts. Use left / right arrow keys to reverse the "direction" of angle change. Press Space bar to toggle animation / reset to minimum speed. Left Control key to toggle stationary / rotating center. Use + / - keys to add remove line segments.


```perl6
use SDL2::Raw;

my $width  = 900;
my $height = 900;

SDL_Init(VIDEO);

my $window = SDL_CreateWindow(
    'Polyspiral',
    SDL_WINDOWPOS_CENTERED_MASK,
    SDL_WINDOWPOS_CENTERED_MASK,
    $width, $height,
    RESIZABLE
);

my $render = SDL_CreateRenderer($window, -1, ACCELERATED +| PRESENTVSYNC);

my $event = SDL_Event.new;

enum KEY_CODES (
    K_UP     => 82,
    K_DOWN   => 81,
    K_LEFT   => 80,
    K_RIGHT  => 79,
    K_SPACE  => 44,
    K_PGUP   => 75,
    K_PGDN   => 78,
    K_LCTRL  => 224,
    K_PLUS   => 87,
    K_MINUS  => 86,
    K_SPLUS  => 46,
    K_SMINUS => 45,
);

my $angle = 0;
my $lines = 240;
my @rgb = palette($lines);
my ($x1, $y1);
my $dir = 1;
my $rot = 0;
my $incr = .0001/π;
my $step = $incr*70;

main: loop {
    while SDL_PollEvent($event) {
        my $casted_event = SDL_CastEvent($event);
        given $casted_event {
            when *.type == QUIT { last main }
            when *.type == KEYDOWN {
                if KEY_CODES(.scancode) -> $comm {
                    given $comm {
                        when 'K_LEFT'   { $dir = $rot ??  1 !! -1 }
                        when 'K_RIGHT'  { $dir = $rot ?? -1 !!  1 }
                        when 'K_UP'     { $step += $incr }
                        when 'K_DOWN'   { $step -= $incr if $step > $incr }
                        when 'K_PGUP'   { $step += $incr*50 }
                        when 'K_PGDN'   { $step -= $incr*50; $step = $step < $incr ?? $incr !! $step }
                        when 'K_SPACE'  { $step = $step ?? 0 !! $incr }
                        when 'K_LCTRL'  { $rot  = $rot  ?? 0 !! -1; $dir *= -1 }
                        when 'K_PLUS'   { $lines = ($lines + 5) min 360; @rgb = palette($lines) }
                        when 'K_SPLUS'  { $lines = ($lines + 5) min 360; @rgb = palette($lines) }
                        when 'K_MINUS'  { $lines = ($lines - 5) max 60;  @rgb = palette($lines) }
                        when 'K_SMINUS' { $lines = ($lines - 5) max 60;  @rgb = palette($lines) }
                    }
                }
                #say .scancode; # unknown key pressed
            }
            when *.type == WINDOWEVENT {
                if .event == 5 {
                    $width  = .data1;
                    $height = .data2;
                }
            }
        }
    }

    $angle = ($angle + $dir * $step) % τ;
    ($x1, $y1) = $width div 2, $height div 2;
    my $dim = $width min $height;
    my $scale = (2 + .33 * abs(π - $angle)) * $dim / $lines;
    $scale *= ($angle > π) ?? (1 - $angle/τ) !! $angle/τ;
    $scale max= $dim/$lines/$lines;
    for ^$lines {
        my $length = $scale + $scale * $_;
        my ($x2, $y2) = ($x1, $y1) «+« cis(($angle * $rot * $lines) + $angle * $_).reals »*» $length;
        SDL_SetRenderDrawColor($render, |@rgb[$_], 255);
        SDL_RenderDrawLine($render, |($x1, $y1, $x2, $y2)».round(1));
        ($x1, $y1) = $x2, $y2;
    }
    @rgb.=rotate($lines/60);
    SDL_RenderPresent($render);
    SDL_SetRenderDrawColor($render, 0, 0, 0, 0);
    SDL_RenderClear($render);
}

SDL_Quit();

sub palette ($l) { (^$l).map: { hsv2rgb(($_ * 360/$l % 360)/360, 1, 1).list } };

sub hsv2rgb ( $h, $s, $v ){ # inputs normalized 0-1
    my $c = $v * $s;
    my $x = $c * (1 - abs( (($h*6) % 2) - 1 ) );
    my $m = $v - $c;
    my ($r, $g, $b) = do given $h {
        when   0..^(1/6) { $c, $x, 0 }
        when 1/6..^(1/3) { $x, $c, 0 }
        when 1/3..^(1/2) { 0, $c, $x }
        when 1/2..^(2/3) { 0, $x, $c }
        when 2/3..^(5/6) { $x, 0, $c }
        when 5/6..1      { $c, 0, $x }
    }
    ( $r, $g, $b ).map: ((*+$m) * 255).Int
}
```



## Phix

Space toggles the timer, '+' increases speed (up to 100 FPS), '-' decreases speed.
'M' toggles "mod360", which inverts the angle every 360/2PI or so, since sin/cos
accept arguments in radians not degrees (and mod 2*PI changes nothing), producing 
non-true polyspirals, but quite interesting nevertheless.
{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\Polyspiral.exw
--
include pGUI.e

Ihandle dlg, canvas, timer
cdCanvas cddbuffer, cdcanvas

constant TITLE = "Polyspiral"

atom incr = 0
bool mod360 = false

procedure Polyspiral(atom x1, y1)
atom angle = incr
integer len = 5
    incr += 0.05
    if mod360 then
        incr = mod(incr,360)
    end if
    for i=1 to 150 do
        atom x2 = x1 + cos(angle)*len
        atom y2 = y1 + sin(angle)*len
        cdCanvasSetForeground(cddbuffer, i*#200+i*#40+i*#10)
        cdCanvasLine(cddbuffer, x1, y1, x2, y2)
        {x1, y1} = {x2, y2}
        len += 3
        angle += incr
        if mod360 then
            angle = mod(angle,360)
        end if
    end for
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    integer {w, h} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    Polyspiral(w/2, h/2)
    cdCanvasFlush(cddbuffer)
    integer ms = IupGetInt(timer,"TIME")
    IupSetStrAttribute(dlg, "TITLE", "%s (timer=%d [%g FPS], angle %3.2f%s)",
                       {TITLE,ms,1000/ms,incr,iff(mod360?" (mod360)":"")})
    return IUP_DEFAULT
end function

function timer_cb(Ihandle /*ih*/)
    IupUpdate(canvas)
    return IUP_IGNORE
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    cdCanvasSetForeground(cddbuffer, CD_GRAY)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c=' ' then
        IupSetInt(timer,"RUN",not IupGetInt(timer,"RUN"))
    elsif find(c,"+-") then
        -- ('+' increases speed, by decreasing TIME)
        IupSetInt(timer,"TIME",max(10,IupGetInt(timer,"TIME")-(','-c)*10))
        IupSetInt(timer,"RUN",0)
        IupSetInt(timer,"RUN",1)
    elsif upper(c)='M' then
        mod360 = not mod360
    end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "640x640")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    timer = IupTimer(Icallback("timer_cb"), 20)

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", TITLE)
    IupSetCallback(dlg, "K_ANY", Icallback("esc_close"))

    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## Python

{{libheader|Pygame}}

```Python
import math

import pygame
from pygame.locals import *

pygame.init()
screen = pygame.display.set_mode((1024, 600))

pygame.display.set_caption("Polyspiral")

incr = 0

running = True

while running:
	pygame.time.Clock().tick(60)
	for event in pygame.event.get():
		if event.type==QUIT:
			running = False
			break

	incr = (incr + 0.05) % 360
	x1 = pygame.display.Info().current_w / 2
	y1 = pygame.display.Info().current_h / 2
	length = 5
	angle = incr

	screen.fill((255,255,255))

	for i in range(1,151):
		x2 = x1 + math.cos(angle) * length
		y2 = y1 + math.sin(angle) * length
		pygame.draw.line(screen, (255,0,0), (x1, y1), (x2, y2), 1)
		# pygame.draw.aaline(screen, (255,0,0), (x1, y1), (x2, y2)) # Anti-Aliased
		x1, y1 = x2, y2
		length += 3
		angle = (angle + incr) % 360

	pygame.display.flip()

```



## Racket

Uses the *universe* animation


```racket
#lang racket

(require 2htdp/universe pict racket/draw)

(define ((polyspiral width height segment-length-increment n-segments) tick/s/28)
  (define turn-angle (degrees->radians (/ tick/s/28 8)))
  (pict->bitmap
   (dc (λ (dc dx dy)
         (define old-brush (send dc get-brush))
         (define old-pen (send dc get-pen))
         (define path (new dc-path%))
         (define x (/ width #i2))
         (define y (/ height #i2))
         (send path move-to x y)
         (for/fold ((x x) (y y) (l segment-length-increment) (a #i0))
                   ((seg n-segments))
           (define x′ (+ x (* l (cos a))))
           (define y′ (+ y (* l (sin a))))
           (send path line-to x y)
           (values x′ y′ (+ l segment-length-increment) (+ a turn-angle)))
         (send dc draw-path path dx dy)
         (send* dc (set-brush old-brush) (set-pen old-pen)))
       width height)))

(animate (polyspiral 400 400 2 1000))
```


See the output for yourself!


## Ring


```ring

# Project : Polyspiral

load "guilib.ring"

paint = null
incr = 1
x1 = 1000
y1 = 1080
angle = 10
length = 10

new qapp 
        {
        win1 = new qwidget() {
                  setwindowtitle("")
                  setgeometry(10,10,1000,1080)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,1000,1080)
                              settext("")
                  }
                  new qpushbutton(win1) {
                          setgeometry(150,30,100,30)
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
        paint = new qpainter() {
                  begin(p1)
                  setpen(pen)
        for i = 1 to 150 
             x2 = x1 + cos(angle) * length
             y2 = y1 + sin(angle) * length
             drawline(x1, y1, x2, y2)
             x1 = x2
             y1 = y2
             length = length + 3
             angle = (angle + incr) % 360
        next

        endpaint()
        }
        label1 { setpicture(p1) show() }

```

Output:

https://www.dropbox.com/s/zwnpimbndekbd5k/PolySpiral.jpg?dl=0


## Scala


### Java Swing Interoperability


```Scala
import java.awt._
import java.awt.event.ActionEvent

import javax.swing._

object PolySpiral extends App {

  SwingUtilities.invokeLater(() =>
    new JFrame("PolySpiral") {

      class PolySpiral extends JPanel {
        private var inc = 0.0

        override def paintComponent(gg: Graphics): Unit = {
          val g = gg.asInstanceOf[Graphics2D]
          def drawSpiral(g: Graphics2D, l: Int, angleIncrement: Double): Unit = {
            var len = l
            var (x1, y1) = (getWidth / 2d, getHeight / 2d)
            var angle = angleIncrement
            for (i <- 0 until 150) {
              g.setColor(Color.getHSBColor(i / 150f, 1.0f, 1.0f))
              val x2 = x1 + math.cos(angle) * len
              val y2 = y1 - math.sin(angle) * len
              g.drawLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
              x1 = x2
              y1 = y2
              len += 3
              angle = (angle + angleIncrement) % (math.Pi * 2)
            }
          }

          super.paintComponent(gg)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          drawSpiral(g, 5, math.toRadians(inc))
        }

        setBackground(Color.white)
        setPreferredSize(new Dimension(640, 640))

        new Timer(40, (_: ActionEvent) => {
          inc = (inc + 0.05) % 360
          repaint()
        }).start()
      }

      add(new PolySpiral, BorderLayout.CENTER)
      pack()
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setLocationRelativeTo(null)
      setResizable(true)
      setVisible(true)
    }
  )

}
```


## SPL


```spl
width,height = #.scrsize()
#.angle(#.degrees)
#.scroff()
incr = 0
>
  incr = (incr+0.05)%360
  x = width/2
  y = height/2
  length = 5
  angle = incr
  #.scrclear()
  #.drawline(x,y,x,y)
  > i, 1..150
    x += length*#.cos(angle)
    y += length*#.sin(angle)
    #.drawcolor(#.hsv2rgb(angle,1,1):3)
    #.drawline(x,y)
    length += 3
    angle = (angle+incr)%360
  <
  #.scr()
<
```



## zkl

[[File:PolySprial.zkl.gif|250px|thumb|right]]
If you click on the image, it is animated.

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
w,h:=640,640;
bitmap:=PPM(w,h,0xFF|FF|FF);  // White background
angleIncrement:=(3.0).toRad();
while(True){
   r,angle:=0.0, 0.0;
   ao,len,inc:=w/2, 2.5, angleIncrement+(130.0).toRad();
   foreach c in (128){
      s,a:=r + len, angle + inc;
      x,y:=r.toRectangular(angle);
      u,v:=r.toRectangular(a);
      c=c.shiftLeft(21) + c.shiftLeft(10) + c*8;  // convert c to a RGB
      bitmap.line(ao+x,ao+y, ao+u,ao+v, c);
      r,angle=s,a;
   }
   bitmap.writeJPGFile("polyspiral.zkl.jpg");
   bitmap.fill(0xFF|FF|FF);  // White background
   angleIncrement=(angleIncrement + 0.05);
   Atomic.sleep(3);
}
```

