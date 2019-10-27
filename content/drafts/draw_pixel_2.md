+++
title = "Draw pixel 2"
description = ""
date = 2019-09-15T10:21:18Z
aliases = []
[extra]
id = 21824
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

;Task:
Create a window and draw a pixel in it, subject to the following:

::#  the window is 640 x 480
::#  the color of the pixel must be yellow (255,255,0)
::#  the position of the pixel is random



## C

Same as the [[Draw a pixel]] task, uses the random number functions of stdlib.h to plot a random point. Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.

```C

#include<graphics.h>
#include<stdlib.h>
#include<time.h>

int main()
{
	srand(time(NULL));
	
	initwindow(640,480,"Yellow Random Pixel");
	
	putpixel(rand()%640,rand()%480,YELLOW);
	
	getch();
	
	return 0;
}

```



## Factor

{{works with|Factor|0.99 development release 2019-07-10}}

```factor
USING: kernel random raylib.ffi ;

640 480 2dup "random yellow pixel" init-window [ random ] bi@

60 set-target-fps [ window-should-close ] [
    begin-drawing BLACK clear-background 2dup YELLOW draw-pixel
    end-drawing
] until close-window
```



## FreeBASIC


```freebasic
' version 04-07-2018
' compile with: fbc -s console
'           or: fbc -s gui

Screen 18, 24              ' Screen 18: 640x480, 24bit colordepth
'ScreenRes 640, 480, 24    ' Screenres: 640x480, 24bit colordepth

If ScreenPtr = 0 Then
    Print "Error setting video mode!"
    End
End If

Randomize Timer
Dim As UInteger x = Rnd * 640, y = Rnd * 480

PSet (x, y), RGB(255,255,0) ' yellow

' empty keyboard buffer
While Inkey <> "" : Wend
WindowTitle "0, 0 is top left,  pixel is at " & x & ", " & y & "        hit any key to end program"
Sleep
End
```



## Go


```go
package main

import (
    "fmt"
    "image"
    "image/color"
    "image/draw"
    "math/rand"
    "time"
)

func main() {
    rect := image.Rect(0, 0, 640, 480)
    img := image.NewRGBA(rect)

    // Use blue background, say.
    blue := color.RGBA{0, 0, 255, 255}
    draw.Draw(img, rect, &image.Uniform{blue}, image.ZP, draw.Src)

    // Set color of a random pixel to yellow
    yellow := color.RGBA{255, 255, 0, 255}
    width := img.Bounds().Dx()
    height := img.Bounds().Dy()
    rand.Seed(time.Now().UnixNano())
    x := rand.Intn(width)
    y := rand.Intn(height)
    img.Set(x, y, yellow)

    // Check there's exactly one random yellow pixel.
    cmap := map[color.Color]string{blue: "blue", yellow: "yellow"}
    for i := 0; i < width; i++ {
        for j := 0; j < height; j++ {
            c := img.At(i, j)
            if cmap[c] == "yellow" {
                fmt.Printf("The color of the pixel at (%d, %d) is yellow\n", i, j)
            }
        }
    }
}
```


{{out}}
Sample output:

```txt

The color of the pixel at (525, 163) is yellow

```



## javascript


```javascript
let w = window.open("", "", "width=640,height=480");
let canvas = document.createElement("canvas");
canvas.width = 640;
canvas.height = 480;
w.document.body.appendChild(canvas);
w.document.body.style.margin = 0;
let ctx = canvas.getContext("2d");
ctx.fillStyle = "#FFFF00";
let x = Math.random() * 641;
let y = Math.random() * 481;
ctx.fillRect(x, y, 1, 1);

```



## Julia


```julia
using Gtk, Graphics

const can = @GtkCanvas()
const win = GtkWindow(can, "Draw a Pixel 2", 640, 480)

draw(can) do widget
    ctx = getgc(can)
    set_source_rgb(ctx, 255, 255, 0)
    x = rand(collect(1:639))
    y = rand(collect(1:480))
    println("The pixel is at $x, $y.")
    move_to(ctx, x, y)
    line_to(ctx, x + 1, y)
    stroke(ctx)
end

draw(can)
show(can)
const cond = Condition()
endit(w) = notify(cond)
signal_connect(endit, win, :destroy)
wait(cond)

```



## Kotlin

This is a variation of the [[Draw a pixel]] task and so therefore is the code to accomplish it.

```scala
// Version 1.2.41

import java.awt.Color
import java.awt.Graphics
import java.awt.image.BufferedImage
import java.util.Random

class BasicBitmapStorage(width: Int, height: Int) {
    val image = BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    fun fill(c: Color) {
        val g = image.graphics
        g.color = c
        g.fillRect(0, 0, image.width, image.height)
    }

    fun setPixel(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB())

    fun getPixel(x: Int, y: Int) = Color(image.getRGB(x, y))
}

fun main(args: Array<String>) {
    val rand = Random()
    val bbs = BasicBitmapStorage(640, 480)
    with (bbs) {
        fill(Color.white) // say
        val x = rand.nextInt(image.width)
        val y = rand.nextInt(image.height)
        setPixel(x, y, Color.yellow)
        // check there's exactly one random yellow pixel
        for (i in 0 until image.width) {
            for (j in 0 until image.height) {
                if (getPixel(i, j) == Color.yellow) {
                    println("The color of the pixel at ($i, $j) is yellow")
                }
            }
        }
    }
}
```


{{output}}
Sample output:

```txt

The color of the pixel at (296, 15) is yellow

```



## Perl

{{libheader|Gtk3}}

```perl
use Gtk3 '-init';

my $width  = 640;
my $height = 480;

my $window = Gtk3::Window->new();
$window->set_default_size($width, $height);
$window->set_border_width(10);
$window->set_title("Draw Pixel 2");
$window->set_app_paintable(TRUE);

my $da = Gtk3::DrawingArea->new();
$da->signal_connect('draw' => \&draw_in_drawingarea);
$window->add($da);
$window->show_all();

Gtk3->main;

sub draw_in_drawingarea
{
  my ($widget, $cr, $data) = @_;
  $cr->set_source_rgb(1, 1, 0);
  $cr->set_line_width(1);
  $cr->rectangle( int rand $width , int rand $height, 1, 1);
  $cr->stroke;
}
```



## Perl 6

{{works with|Rakudo|2018.05}}
Coordinates of random pixel displayed in window title. To make the single pixel show up better I filled in the drawing area background with black to get better contrast.


```perl6
use GTK::Simple;
use GTK::Simple::DrawingArea;
use Cairo;
my ($w, $h) = 640, 480;
my ($x, $y) = (^$w).pick, (^$h).pick;

my $app = GTK::Simple::App.new(:title("Draw Pixel 2 @ $x,$y"));
my $da  = GTK::Simple::DrawingArea.new;
gtk_simple_use_cairo;

$app.set-content( $da );
$app.border-width = 5;
$da.size-request($w, $h);

sub rect-do( $d, $ctx ) {
    given $ctx {
        .rgb(0, 0, 0);
        .rectangle(0, 0, $w, $h);
        .fill;
        .rgb(1, 1, 0);
        .rectangle($x, $y, 1, 1);
        .fill;
    }
}

my $ctx = $da.add-draw-handler( &rect-do );
$app.run;
```



## Phix

{{libheader|pGUI}}
Resize the window to see the pixel jumping about.

```Phix
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    atom {w,h} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    cdCanvasPixel(cddbuffer, rand(w), rand(h), CD_YELLOW)
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_BLACK)
    return IUP_DEFAULT
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "240x50")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Draw pixel")
    IupCloseOnEscape(dlg)

    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## Racket



```racket
#lang racket/gui

(define WIDTH 640)
(define HEIGHT 480)
(define COLOR (make-color 255 255 0))
(define BACKGROUND-COLOR (make-color 0 0 0))

(define frame (new frame%
                   [label "Draw Pixel"]
                   [width WIDTH]
                   [height HEIGHT]))

(new canvas% [parent frame]
     [paint-callback
      (λ (canvas dc)
        (send dc set-background BACKGROUND-COLOR)
        (send dc clear)
        (send dc set-pen COLOR 1 'solid)
        (send dc draw-point (random WIDTH) (random HEIGHT)))])

(send frame show #t)
```



## Ring


```ring
# Project  : Draw pixel 2

load "guilib.ring"

new qapp {
       nwidth = 320
       nheight= 240
       win1 = new qwidget() {
                  setwindowtitle("Draw Pixel 2")
                  setgeometry(100,100,640,480)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,640,480)
                              settext("")
                  }
                  new qpushbutton(win1) {
                         setgeometry(200,400,100,30)
                         settext("draw")
                         setclickevent("draw()")
                  }
                  new qpushbutton(win1) {
                         setgeometry(300,400,100,30)
                         settext("get pixel color")
                         setclickevent("PixelColor()")
                  }
                  show()
       }
       exec()
}

func draw()
        p1 = new qpicture()
        color = new qcolor() {
                   setrgb(255,255,0,255)
                  }
        pen = new qpen() {
                  setcolor(color)
                  setwidth(10)
                  }
        new qpainter() {
               begin(p1)
               setpen(pen)
               x = random(nwidth-1) + 1
               y = random(nheight-1) + 1
               see "x = " + x + " y = " + y + nl
               drawpoint(x,y)
               endpaint()
               }
               label1 { setpicture(p1) show() }

func PixelColor()
       oapp = new qapp(0,null)  {
                  screen = win1.windowhandle().screen()
                  pixmap = screen.grabwindow(0,0,0,-1,-1)
                  image = pixmap.toimage()
                  color = image.pixel(100,100)
                  mycolor = new qcolor()
                  mycolor.setrgb(255,255,0,255)
                  see nl+"red : " + mycolor.red() + nl
                  see "green : " + mycolor.green() + nl
                  see "blue : " + mycolor.blue() + nl
                  }
```

Outputimage:
 
[https://www.dropbox.com/s/n31ppq8cu9ipru8/CalmoSoftPixelColor.jpg?dl=0 Draw pixel 2]

## Scala


### Java Swing Interoperability

{{Out}}See it yourself by running in your browser either by [https://scastie.scala-lang.org/9jfWoH2uRomUBKDBzFGzkw Scastie (remote JVM)].
{{libheader|Scala Java Swing interoperability}}
{{libheader|Scala Test provided}}
{{libheader|Scala Javanese style}}
{{libheader|Scastie qualified}}
{{works with|Scala|2.13}}

```Scala
import java.awt.image.BufferedImage
import java.awt.Color
import scala.language.reflectiveCalls

object RgbBitmap extends App {
  // Even Javanese style testing is still possible.
  private val img0 =
    new RgbBitMap(50, 60) { // Wrappers to enable adhoc Javanese style
      def getPixel(x: Int, y: Int) = this(x, y)
      def setPixel(x: Int, y: Int, c: Color) = this(x, y) = c
  }

  class RgbBitMap(val dim: (Int, Int)) {
    private val image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    def apply(x: Int, y: Int) = new Color(image.getRGB(x, y))

    def update(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB)

    def fill(c: Color) = {
      val g = image.getGraphics
      g.setColor(c)
      g.fillRect(0, 0, width, height)
    }

    def width = dim._1
    def height = dim._2
  }

  private val (x,y) = (util.Random.nextInt(50), util.Random.nextInt(60))

  img0.fill(Color.CYAN)
  img0.setPixel(x, y, Color.BLUE)
  // Testing in Java style
  assert(img0.getPixel(x, y) == Color.BLUE)
  assert(img0.width == 50)
  assert(img0.height == 60)
  println("Tests successfully completed with no errors found.")

}
```

