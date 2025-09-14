+++
title = "Colour pinstripe/Printer"
description = ""
date = 2019-04-15T17:57:31Z
aliases = []
[extra]
id = 9748
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "bbc_basic",
  "go",
  "phix",
  "picolisp",
  "python",
  "racket",
  "tcl",
]
+++

The task is to create 1 point wide colour vertical pinstripes with a sufficient number of pinstripes to span the entire width of the colour graphics printer. The pinstripes should alternate between each individual cartridge ink and ink pair and black and white pinstripes should be included. A typical pinstripe sequence woud be black, red, green, blue, magenta, cyan, yellow, white.

After the first inch of printing, we switch to a wider 2 pixel wide vertical pinstripe pattern. and to 3 point wide vertical for the next inch, and then 4 point wide, etc. This trend continues for the entire length of the page (or for 12 inches of run length in the case of a printer using continuous roll stationery). After printing the test pattern the page is ejected (or the test pattern is rolled clear of the printer enclosure, in the case of continuous roll printers).

Note that it is an acceptable solution to use the smallest marks that the language provides, rather than working at native printer resolution, where this is not achievable from within the language.

Optionally, on systems where the printer resolution cannot be determined, it is permissible to prompt the user for printer resolution, and to calculate point size based on user input, enabling fractional point sizes to be used. 


## BBC BASIC

This program first displays a Print Dialogue so the printer can be selected.

```bbcbasic
      PD_RETURNDC = 256
      _LOGPIXELSY = 90
      
      DIM pd{lStructSize%, hwndOwner%, hDevMode%, hDevNames%, \
      \      hdc%, flags%, nFromPage{l&,h&}, nToPage{l&,h&}, \
      \      nMinPage{l&,h&}, nMaxPage{l&,h&}, nCopies{l&,h&}, \
      \      hInstance%, lCustData%, lpfnPrintHook%, lpfnSetupHook%, \
      \      lpPrintTemplateName%, lpSetupTemplateName%, \
      \      hPrintTemplate%, hSetupTemplate%}
      pd.lStructSize% = DIM(pd{})
      pd.hwndOwner% = @hwnd%
      pd.flags% = PD_RETURNDC
      
      SYS "PrintDlg", pd{} TO ok%
      IF ok%=0 THEN QUIT
      SYS "DeleteDC", @prthdc%
      @prthdc% = pd.hdc%
      *MARGINS 0,0,0,0
      
      dx% = @vdu%!236-@vdu%!232
      dy% = @vdu%!244-@vdu%!240
      SYS "GetDeviceCaps", @prthdc%, _LOGPIXELSY TO dpi%
      
      DIM rc{l%,t%,r%,b%}
      DIM colour%(7)
      colour%() = &000000, &0000FF, &00FF00, &FF0000, \
      \           &FF00FF, &FFFF00, &00FFFF, &FFFFFF
      
      VDU 2,1,32,3
      pitch% = 1
      FOR y% = 0 TO dy% STEP dpi%
        col% = 0
        FOR x% = 0 TO dx%-pitch% STEP pitch%
          rc.l% = x% : rc.r% = x% + pitch%
          rc.t% = y% : rc.b% = y% + dpi%
          SYS "CreateSolidBrush", colour%(col% MOD 8) TO brush%
          SYS "FillRect", @prthdc%, rc{}, brush%
          SYS "DeleteObject", brush%
          col% += 1
        NEXT
        pitch% += 1
      NEXT y%
      VDU 2,1,12,3
```



## Go

The code for this task is basically the same as for [[Colour_pinstripe/Display#Go]] except that the drawing parameters have been tweaked to produce 1 inch bands when printing on A4 paper and some code has been added to dump the image to the default printer.

```go
package main
 
import (
    "github.com/fogleman/gg"
    "log"
    "os/exec"
    "runtime"
)

var palette = [8]string{
    "000000", // black
    "FF0000", // red
    "00FF00", // green
    "0000FF", // blue
    "FF00FF", // magenta
    "00FFFF", // cyan
    "FFFF00", // yellow
    "FFFFFF", // white
}
 
func pinstripe(dc *gg.Context) {
    w := dc.Width()
    h := dc.Height() / 7
    for b := 1; b <= 11; b++ {
        for x, ci := 0, 0; x < w; x, ci = x+b, ci+1 {
            dc.SetHexColor(palette[ci%8])
            y := h * (b - 1)
            dc.DrawRectangle(float64(x), float64(y), float64(b), float64(h))
            dc.Fill()
        }
    }
}
 
func main() {
    dc := gg.NewContext(842, 595)
    pinstripe(dc)
    fileName := "color_pinstripe.png"
    dc.SavePNG(fileName)
    var cmd *exec.Cmd
    if runtime.GOOS == "windows" {
        cmd = exec.Command("mspaint", "/pt", fileName)
    } else {
        cmd = exec.Command("lp", fileName)
    }
    if err := cmd.Run(); err != nil {
        log.Fatal(err)
    }   
}
```



## Phix

See the print_cb function of [[Colour_pinstripe/Display#Phix]] and the final comments of that entry.


## PicoLisp


```PicoLisp
(load "@lib/ps.l")

# Using circular lists for an endless supply of colors
#      (black  red  green blue magenta cyan yellow white)
(setq
   Red   (0    100    0     0    100    0    100   100 .)
   Green (0     0    100    0     0    100   100   100 .)
   Blue  (0     0     0    100   100   100    0    100 .) )

(call 'lpr
   (pdf "pinstripes"
      (a4)  # 595 x 842 dots
      (let (I 0  Step 1)
         (for X 595
            (color (car Red) (car Green) (car Blue)
               (vline X 0 842) )
            (when (= Step (inc 'I))
               (zero I)
               (pop 'Red)
               (pop 'Green)
               (pop 'Blue) )
            (when (=0 (% X 72))  # 1 inch
               (zero I)
               (inc 'Step) ) ) )
      (page) ) )
```



## Python


```Python

from turtle import *
from PIL import Image
import time
import subprocess

"""

Only works on Windows. Assumes that you have Ghostscript
installed and in your path.

https://www.ghostscript.com/download/gsdnld.html

Hard coded to 100 pixels per inch.

"""

colors = ["black", "red", "green", "blue", "magenta", "cyan", "yellow", "white"]

screen = getscreen()

# width and height in pixels
# aspect ratio for 11 by 8.5 paper

inch_width = 11.0
inch_height = 8.5

pixels_per_inch = 100

pix_width = int(inch_width*pixels_per_inch)
pix_height = int(inch_height*pixels_per_inch)

screen.setup (width=pix_width, height=pix_height, startx=0, starty=0)

screen.screensize(pix_width,pix_height)

# center is 0,0

# get coordinates of the edges

left_edge = -screen.window_width()//2

right_edge = screen.window_width()//2

bottom_edge = -screen.window_height()//2

top_edge = screen.window_height()//2

# draw quickly

screen.delay(0)
screen.tracer(5)

for inch in range(int(inch_width)-1):
    line_width = inch + 1
    pensize(line_width)
    colornum = 0

    min_x = left_edge + (inch * pixels_per_inch)
    max_x = left_edge + ((inch+1) * pixels_per_inch)
    
    for y in range(bottom_edge,top_edge,line_width):
        penup()
        pencolor(colors[colornum])
        colornum = (colornum + 1) % len(colors)
        setposition(min_x,y)
        pendown()
        setposition(max_x,y)
         
screen.getcanvas().postscript(file="striped.eps")

# convert to jpeg
# won't work without Ghostscript.

im = Image.open("striped.eps")
im.save("striped.jpg")

# Got idea from http://rosettacode.org/wiki/Colour_pinstripe/Printer#Go
    
subprocess.run(["mspaint", "/pt", "striped.jpg"])

```



## Racket


The drawing code is exactly the same code as [[Colour_pinstripe/Display#Racket]],
only drawing onto a printer device context now.


```Racket

#lang racket/gui

(define parts 4)

(define dc (new printer-dc%))
(send* dc (start-doc "Colour Pinstripe") (start-page))

(define-values [W H] (send dc get-size))
(define parts 4)
(define colors
  '("Black" "Red" "Green" "Blue" "Magenta" "Cyan" "Yellow" "White"))
(send dc set-pen "black" 0 'transparent)
(send dc set-brush "black" 'solid)
(define H* (round (/ H parts)))
(for ([row parts])
  (define Y (* row H*))
  (for ([X (in-range 0 W (add1 row))] [c (in-cycle colors)])
    (send dc set-brush c 'solid)
    (send dc draw-rectangle X Y (add1 row) H*)))

(send* dc (end-page) (end-doc))

```



## Tcl

This code assumes that the page's printable area is 8.5"×11".
```tcl
package require Tk
# Allocate a temporary drawing surface
canvas .c
# The cycle of colors we want to use
set colors {black red green blue magenta cyan yellow white}
# Draw the output we want 
for {set y 0;set dx 1} {$y < 11*72} {incr y 72;incr dx} {
    for {set x 0;set c 0} {$x < 8.5*72} {incr x $dx;incr c} {
	.c create rectangle $x $y [expr {$x+$dx+1}] [expr {$y+73}] \
	    -fill [lindex $colors [expr {$c%[llength $colors]}]] -outline {}
    }
}
# Send postscript to default printer, scaled 1 pixel -> 1 point
exec lp - << [.c postscript -height $y -width $x -pageheight $y -pagewidth $x]
# Explicit exit; no GUI desired
exit
```


