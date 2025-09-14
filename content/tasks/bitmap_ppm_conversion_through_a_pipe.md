+++
title = "Bitmap/PPM conversion through a pipe"
description = ""
date = 2018-12-29T18:44:20Z
aliases = []
[extra]
id = 3217
[taxonomies]
categories = ["task", "Raster graphics operations"]
tags = []
languages = [
  "c",
  "go",
  "julia",
  "kotlin",
  "mathematica",
  "perl_6",
  "picolisp",
  "python",
  "racket",
  "ruby",
  "tcl",
  "zkl",
]
+++

## Task

Using the data storage type defined [[Basic_bitmap_storage|on this page]] for raster images, delegate writing a JPEG file through a '''pipe''' using the <tt>output_ppm</tt> function defined [[Write_ppm_file|on this other page]].

There are various utilities that can be used for this task, for example: '''cjpeg''' (package ''"jpeg-progs"'' on Linux), '''ppmtojpeg''' (package ''"netpbm"'' on Linux), '''convert''' (from ''ImageMagick'', multi-platform).


## C

This one uses the ImageMagick <tt>convert</tt> tool.


```c
/* interface */
void print_jpg(image img, int qual);
```



```c
#define MAXCMDBUF 100
void print_jpg(image img, int qual)
{
   char buf[MAXCMDBUF];
   unsigned int n;
   FILE *pipe;
   
   snprintf(buf, MAXCMDBUF, "convert ppm:- -quality %d jpg:-", qual);
   pipe = popen(buf, "w");
   if ( pipe != NULL )
   {
           fprintf(pipe, "P6\n%d %d\n255\n", img->width, img->height);
           n = img->width * img->height;
           fwrite(img->buf, sizeof(pixel), n, pipe);
           pclose(pipe);
   }
}
```


The code that writes to the pipe is the same of [[Write_ppm_file|output_ppm]] of course. A complete example is


```c
#include "imglib.h"

int main()
{
      image img;
      
      img = alloc_img(100,100);
      fill_img(img, 50, 20, 200);
      draw_line(img, 0, 0, 80, 80, 255, 0, 0);
      print_jpg(img, 75);
      free_img(img);
}
```


In order to make it working, you must link it with the raster image functions given by the codes [[Bresenham's_line_algorithm#C|here]] and [[Basic_bitmap_storage#C|here]]

## Go

{{works with|Go weekly.2011-12-14}} (Go 1 should be close)
Using cjpeg:

```go
package main

// Files required to build supporting package raster are found in:
// * Bitmap
// * Write a PPM file

import (
    "fmt"
    "math/rand"
    "os/exec"
    "raster"
)

func main() {
    b := raster.NewBitmap(400, 300)
    // a little extravagant, this draws a design of dots and lines
    b.FillRgb(0xc08040)
    for i := 0; i < 2000; i++ {
        b.SetPxRgb(rand.Intn(400), rand.Intn(300), 0x804020)
    }
    for x := 0; x < 400; x++ {
        for y := 240; y < 245; y++ {
            b.SetPxRgb(x, y, 0x804020)
        }
        for y := 260; y < 265; y++ {
            b.SetPxRgb(x, y, 0x804020)
        }
    }
    for y := 0; y < 300; y++ {
        for x := 80; x < 85; x++ {
            b.SetPxRgb(x, y, 0x804020)
        }
        for x := 95; x < 100; x++ {
            b.SetPxRgb(x, y, 0x804020)
        }
    }

    // pipe logic
    c := exec.Command("cjpeg", "-outfile", "pipeout.jpg")
    pipe, err := c.StdinPipe()
    if err != nil {
        fmt.Println(err)
        return
    }
    err = c.Start()
    if err != nil {
        fmt.Println(err)
        return
    }
    err = b.WritePpmTo(pipe)
    if err != nil {
        fmt.Println(err)
        return
    }
    err = pipe.Close()
    if err != nil {
        fmt.Println(err)
    }
}
```



## Julia

```julia
using Images, FileIO

ppmimg = load("data/bitmapInputTest.ppm")
save("data/bitmapOutputTest.jpg", ppmimg)
```



## Kotlin

In order to provide a complete runnable example, we repeat bits of code from other relevant tasks and add code which pipes .ppm data to ImageMagick's 'convert' tool which then writes the corresponding .jpg file to disk.

```scala
// Version 1.2.40

import java.awt.Color
import java.awt.Graphics
import java.awt.image.BufferedImage

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
    // create BasicBitmapStorage object
    val width = 640
    val height = 640
    val bbs = BasicBitmapStorage(width, height)
    for (y in 0 until height) {
        for (x in 0 until width) {
            val c = Color(x % 256, y % 256, (x * y) % 256)
            bbs.setPixel(x, y, c)
        }
    }

    // now write the object in PPM format to ImageMagick's STDIN via a pipe
    // so it can be converted to a .jpg file and written to disk
    val pb = ProcessBuilder("convert", "-", "output_piped.jpg")
    pb.directory(null)
    pb.redirectInput(ProcessBuilder.Redirect.PIPE)
    val buffer = ByteArray(width * 3) // write one line at a time
    val proc = pb.start()
    val pStdIn = proc.outputStream
    pStdIn.use {
        val header = "P6\n$width $height\n255\n".toByteArray()
        with (it) {
            write(header)
            for (y in 0 until height) {
                for (x in 0 until width) {
                    val c = bbs.getPixel(x, y)
                    buffer[x * 3] = c.red.toByte()
                    buffer[x * 3 + 1] = c.green.toByte()
                    buffer[x * 3 + 2] = c.blue.toByte()
                }
                write(buffer)
            }
        }
    }
}
```



## Mathematica

The Windows command line does not like quotes in the middle of text, so strings have been turned into character codes.

```Mathematica
convert[image_,out_]:=Module[{process=StartProcess[{
"wolfram","-noinit","-noprompt","-run",
"Export[FromCharacterCode["~~ToString[ToCharacterCode[out]]~~"],ImportString[StringRiffle[Table[InputString[],{4}],FromCharacterCode[10]],FromCharacterCode[{80,80,77}]]]"
}]},
WriteLine[process,image];
WriteLine[process,"Quit[]"];
];
```


== {{Header|OCaml}} ==


```ocaml
let print_jpeg ~img ?(quality=96) () =
  let cmd = Printf.sprintf "cjpeg -quality %d" quality in
  (*
  let cmd = Printf.sprintf "ppmtojpeg -quality %d" quality in
  let cmd = Printf.sprintf "convert ppm:- -quality %d jpg:-" quality in
  *)
  let ic, oc = Unix.open_process cmd in
  output_ppm ~img ~oc;
  try
    while true do
      let c = input_char ic in
      print_char c
    done
  with End_of_file -> ()
;;
```



## Perl 6


```perl6
#!/usr/bin/env perl6

# Reference:
# https://rosettacode.org/wiki/Bitmap/Write_a_PPM_file#Perl_6

use v6;

class Pixel { has uint8 ($.R, $.G, $.B) }
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
      ) is rw { @!data[$i*$!height + $j] }

    method data { @!data }
}

role PPM {
    method P6 returns Blob {
        "P6\n{self.width} {self.height}\n255\n".encode('ascii')
        ~ Blob.new: flat map { .R, .G, .B }, self.data
    }
}

my Bitmap $b = Bitmap.new(width => 125, height => 125) but PPM;
for flat ^$b.height X ^$b.width -> $i, $j {
    $b.pixel($i, $j) = Pixel.new: :R($i*2), :G($j*2), :B(255-$i*2);
}

my $proc = run '/usr/bin/convert','-','output_piped.jpg', :in;
$proc.in.write: $b.P6;
$proc.in.close;
```

```txt
file output_piped.jpg
output_piped.jpg: JPEG image data, JFIF standard 1.01, aspect ratio, density 1x1, segment length 16, baseline, precision 8, 125x125, frames 3
```



## PicoLisp


```PicoLisp
# Create an empty image of 120 x 90 pixels
(setq *Ppm (make (do 90 (link (need 120)))))

# Fill background with green color
(ppmFill *Ppm 0 255 0)

# Draw a diagonal line
(for I 80 (ppmSetPixel *Ppm I I 0 0 0))

# Write to "img.jpg" through a pipe
(ppmWrite *Ppm '("convert" "-" "img.jpg"))
```



## Python


```Python

"""
Adapted from https://stackoverflow.com/questions/26937143/ppm-to-jpeg-jpg-conversion-for-python-3-4-1
Requires pillow-5.3.0 with Python 3.7.1 32-bit on Windows.
Sample ppm graphics files from http://www.cs.cornell.edu/courses/cs664/2003fa/images/
"""

from PIL import Image

im = Image.open("boxes_1.ppm")
im.save("boxes_1.jpg")

```

Does not need to pipe through a conversion utility 
because the Pillow module does the conversion.


## Racket


```racket

(define (ppm->jpeg bitmap [jpg-file "output"] [quality 75])
  (define command (format "convert ppm:- -quality ~a jpg:~a.jpg" quality jpg-file))
  (match-define (list in out pid err ctrl)  (process command))
  (bitmap->ppm bitmap out)
  (close-input-port in)
  (close-output-port out))

(ppm->jpeg bm)
```



## Ruby

Extends [[Write ppm file#Ruby]]. Uses the ImageMagick <code>convert</code> tool.
Additionally, for debugging, allow writing in pixmap P3 (ascii) format.


```ruby
class Pixmap
  PIXMAP_FORMATS = ["P3", "P6"]   # implemented output formats
  PIXMAP_BINARY_FORMATS = ["P6"]  # implemented output formats which are binary

  def write_ppm(ios, format="P6")
    if not PIXMAP_FORMATS.include?(format)
      raise NotImplementedError, "pixmap format #{format} has not been implemented" 
    end
    ios.puts format, "#{@width} #{@height}", "255"
    ios.binmode if PIXMAP_BINARY_FORMATS.include?(format)
    @height.times do |y|
      @width.times do |x|
        case format
        when "P3" then ios.print @data[x][y].values.join(" "),"\n"
        when "P6" then ios.print @data[x][y].values.pack('C3')
        end
      end
    end
  end

  def save(filename, opts={:format=>"P6"})
    File.open(filename, 'w') do |f|
      write_ppm(f, opts[:format])
    end
  end

  def print(opts={:format=>"P6"})
    write_ppm($stdout, opts[:format])
  end

  def save_as_jpeg(filename, quality=75)
    pipe = IO.popen("convert ppm:- -quality #{quality} jpg:#{filename}", 'w')
    write_ppm(pipe)
    pipe.close
  end
end

image = Pixmap.open('file.ppm')
image.save_as_jpeg('file.jpg')
```



## Tcl

Referring to [[Write ppm file#Tcl]] and [[Basic bitmap storage#Tcl]]

```tcl
package require Tk

proc output_jpeg {image filename {quality 75}} {
    set f [open |[list convert ppm:- -quality $quality jpg:- > $filename] w]
    fconfigure $f -translation binary
    puts -nonewline $f [$image data -format ppm]
    close $f
}
```

However, it is more normal to do this directly with the {{libheader|TkImg}} which is bundled with many Tcl distributions.

```tcl
package require Tk
package require img::jpeg

proc output_jpeg {image filename} {
    $image write $filename -format jpeg
}
set img [image create photo -filename filename.ppm]
output_jpeg $img filename.jpg
```



## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

Using the convert utility by ImageMagick:

```zkl
img:=PPM.readPPMFile("fractal.ppm");
p:=System.popen(0'|convert ppm:- jpg:"fractal.jpg"|,"w");
img.write(p); p.close();
```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have an external OS/command processor. -->
