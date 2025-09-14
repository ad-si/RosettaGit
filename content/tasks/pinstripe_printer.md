+++
title = "Pinstripe/Printer"
description = ""
date = 2019-02-06T17:54:21Z
aliases = []
[extra]
id = 9746
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "bbc_basic",
  "go",
  "liberty_basic",
  "phix",
  "picolisp",
  "racket",
  "tcl",
]
+++

## Task

{{task}}[[Category:Test card]][[Category:Printer]]
The task is to demonstrate the creation of a series of 1 point wide vertical pinstripes with a sufficient number of pinstripes to span the entire width of the printed page (except for the last pinstripe). The pinstripes should alternate one point white, one point black. (Where the printer does not support producing graphics in terms of points, pixels may be substituted in this task.)

After the first inch of printing, we switch to a wider 2 point wide vertical pinstripe pattern. alternating two points white, two points black. We then switch to 3 points wide for the next inch, and then 4 points wide, etc. This trend continues for the entire length of the page (or for 12 inches of run length in the case of a printer using continuous roll stationery). After printing the test pattern the page is ejected (or the test pattern is rolled clear of the printer enclosure, in the case of continuous roll printers).

Note that it is an acceptable solution to use the smallest marks that the language provides, rather than working at native printer resolution, where this is not achievable from within the language.

Optionally, on systems where the printer resolution cannot be determined, it is permissible to prompt the user for printer resolution, and to calculate point size based on user input, enabling fractional point sizes to be used.


## BBC BASIC

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
      SYS "CreateSolidBrush", 0 TO brush%
      
      VDU 2,1,32,3
      pitch% = 2
      FOR y% = 0 TO dy% STEP dpi%
        FOR x% = 0 TO dx%-pitch% STEP pitch%
          rc.l% = x% : rc.r% = x% + pitch%/2
          rc.t% = y% : rc.b% = y% + dpi%
          SYS "FillRect", @prthdc%, rc{}, brush%
        NEXT
        pitch% += 2
      NEXT y%
      VDU 2,1,12,3
```



## Go

The code for this task is basically the same as for [[Pinstripe/Display#Go]] except that the drawing parameters have been tweaked to produce 1 inch bands when printing on A4 paper and some code has been added to dump the image to the default printer.

```go
package main
 
import (
    "github.com/fogleman/gg"
    "log"
    "os/exec"
    "runtime"
)

var palette = [2]string{
    "FFFFFF", // white
    "000000", // black    
}
 
func pinstripe(dc *gg.Context) {
    w := dc.Width()
    h := dc.Height() / 7
    for b := 1; b <= 11; b++ {
        for x, ci := 0, 0; x < w; x, ci = x+b, ci+1 {
            dc.SetHexColor(palette[ci%2])
            y := h * (b - 1)
            dc.DrawRectangle(float64(x), float64(y), float64(b), float64(h))
            dc.Fill()
        }
    }
}
 
func main() {
    dc := gg.NewContext(842, 595)
    pinstripe(dc)
    fileName := "w_pinstripe.png"
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



## Liberty BASIC

Draws the pattern in a window onto a large graphic box, then dumps to the printer.

```lb

nomainwin

'paperW =  8.5  ' for US letter paper
'paperH = 11

paperW =  8.2677165   '   for A4 paper
paperH = 11.6929134

dpi= 300

prompt "Enter your printer DPI" +chr$( 13) + "(300 is OK for laser one, 360 likely for inkjet)"; dpi

w = paperW *dpi    'pixel size of paper
h = paperH *dpi

graphicbox #main.gr, 0, 0, 300, 300 'picture could be bigger

open "Pinstripe/Printer" for window as #main

#main    "trapclose [quit]"
#main.gr "autoresize"   'now we can maximize window with picture
#main.gr "down"
#main.gr "horizscrollbar on 0 "; w -300  'so we can scroll it
#main.gr "vertscrollbar  on 0 "; h -300
#main.gr "place 0 0"
#main.gr "color white"
#main.gr "boxfilled "; w; " ";h
#main.gr "color black"
#main.gr "backcolor black"

for i = 0 to int( paperH)
    ww = i + 1
    yy =( i + 1) * dpi
    if yy > h then yy = h
    for x = ww to w step ww * 2   'start with white strip
        x1 = x + ww
        if x1 >= w then x1 = w
        #main.gr "place "; x; " "; i * dpi
        #main.gr "boxfilled "; x1; " "; yy
    next
next

#main.gr "flush"
#main.gr "print "; w

wait
[quit]
close #main
end

```



## Phix

See the print_cb function of [[Colour_pinstripe/Display#Phix]] and the final comments of that entry.


## PicoLisp



```PicoLisp
(load "@lib/ps.l")

(call 'lpr
   (pdf "pinstripes"
      (a4)  # 595 x 842 dots
      (for X 595
         (gray (if (bit? 1 X) 0 100)
            (vline X 0 842) ) )
      (page) ) )
```



## Racket

The drawing code is exactly the same code as [[Pinstripe/Display#Racket]], only
drawing onto a printer device context now.

```Racket

#lang racket/gui

(define parts 4)

(define dc (new printer-dc%))
(send* dc (start-doc "Pinstripe") (start-page))

(define-values [W H] (send dc get-size))
(send dc set-pen "black" 0 'solid)
(send dc set-brush "black" 'solid)
(define H* (round (/ H parts)))
(for ([row parts])
  (define Y (* row H*))
  (for ([X (in-range 0 W (* (add1 row) 2))])
    (send dc draw-rectangle X Y (add1 row) H*)))

(send* dc (end-page) (end-doc))

```



## Tcl


This code assumes that the page's printable area is 8.5"&times;11".
```tcl
package require Tk
# Allocate a temporary drawing surface
canvas .c
# Draw the output we want 
for {set y 0;set dx 1} {$y < 11*72} {incr y 72;incr dx} {
    for {set x 0;set c 0} {$x < 8.5*72} {incr x $dx;set c [expr {!$c}]} {
	.c create rectangle $x $y [expr {$x+$dx+1}] [expr {$y+73}] \
	    -fill [lindex {black white} $c] -outline {}
    }
}
# Send postscript to default printer, scaled 1 pixel -> 1 point
exec lp - << [.c postscript -height $y -width $x -pageheight $y -pagewidth $x]
# Explicit exit; no GUI desired
exit
```


