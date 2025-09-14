+++
title = "Bitmap/Read an image through a pipe"
description = ""
date = 2018-12-08T23:10:57Z
aliases = []
[extra]
id = 3246
[taxonomies]
categories = ["task", "Raster graphics operations"]
tags = []
languages = [
  "auto_hotkey",
  "c",
  "go",
  "julia",
  "kotlin",
  "mathematica",
  "ocaml",
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

This task is the ''opposite'' of the [[PPM conversion through a pipe]]. In this task, using a delegate tool (like '''cjpeg''', one of the netpbm package, or '''convert''' of the ImageMagick package) we read an image file and load it into the data storage type [[Basic bitmap storage|defined here]]. We can also use the code from [[Read ppm file]], so that we can use PPM format like a (natural) bridge between the foreign image format and our simple data storage.


## AutoHotkey

Uses [http://www.autohotkey.com/forum/viewtopic.php?t=16823 StdoutTovar.ahk] 

```AutoHotkey
ppm := Run("cmd.exe /c convert lena50.jpg ppm:-") 
                       ; pipe in from imagemagick
img := ppm_read("", ppm) ;   
x := img[4,4] ; get pixel(4,4)
y := img[24,24] ; get pixel(24,24)
msgbox % x.rgb() " " y.rgb()
img.write("lena50copy.ppm")
return
 
ppm_read(filename, ppmo=0) ; only ppm6 files supported 
{
if !ppmo  ; if image not already in memory, read from filename
  fileread, ppmo, % filename 

  index := 1  
  pos := 1

  loop, parse, ppmo, `n, `r
  {
    if (substr(A_LoopField, 1, 1) == "#")
      continue
loop, 
{ 
 if !pos := regexmatch(ppmo, "\d+", pixel, pos)
break
    bitmap%A_Index% := pixel
    if (index == 4)
      Break
    pos := regexmatch(ppmo, "\s", x, pos)
    index ++
}
  }
 
  type := bitmap1
  width := bitmap2
  height := bitmap3
  maxcolor := bitmap4
  bitmap := Bitmap(width, height, color(0,0,0))
  index := 1
  i := 1
  j := 1
 bits := pos 
loop % width * height 
  {
      bitmap[i, j, "r"]  := numget(ppmo, 3 * A_Index + bits, "uchar")
      bitmap[i, j, "g"]  := numget(ppmo, 3 * A_Index + bits + 1, "uchar")
      bitmap[i, j, "b"]  := numget(ppmo, 3 * A_Index + bits + 2, "uchar")

      if (j == width)
{
	j := 1
	i += 1
}
      else
	j++
}
 return bitmap  
  }
#include bitmap_storage.ahk ; from http://rosettacode.org/wiki/Basic_bitmap_storage/AutoHotkey
#include run.ahk ; http://www.autohotkey.com/forum/viewtopic.php?t=16823

```



## C

Here I've used '''convert''' by ImageMagick. It is up to the program to ''understand'' the source file type; in this way, we can read theoretically any image format ImageMagick can handle. The <tt>get_ppm</tt> function defined in [[Read ppm file]] is used.


```c
image read_image(const char *name);
```



```c
#include "imglib.h"

#define MAXCMDBUF 100
#define MAXFILENAMELEN 256
#define MAXFULLCMDBUF (MAXCMDBUF + MAXFILENAMELEN)
image read_image(const char *name)
{
      FILE *pipe;
      char buf[MAXFULLCMDBUF];
      image im;
      
      FILE *test = fopen(name, "r");
      if ( test == NULL ) {
         fprintf(stderr, "cannot open file %s\n", name);
         return NULL;
      }
      fclose(test);
      
      snprintf(buf, MAXFULLCMDBUF, "convert \"%s\" ppm:-", name);
      pipe = popen(buf, "r");
      if ( pipe != NULL )
      {
           im = get_ppm(pipe);
           pclose(pipe);
           return im;
      }
      return NULL;
}
```



## Go

This example uses convert to convert the test image for the flood fill task.  It reads through the pipe as required for this task, then writes as a .ppm file convenient for the flood fill task.

```go
package main

// Files required to build supporting package raster are found in:
// * Bitmap
// * Read a PPM file
// * Write a PPM file

import (
    "log"
    "os/exec"
    "raster"
)

func main() {
    c := exec.Command("convert", "Unfilledcirc.png", "-depth", "1", "ppm:-")
    pipe, err := c.StdoutPipe()
    if err != nil {
        log.Fatal(err)
    }
    if err = c.Start(); err != nil {
        log.Fatal(err)
    }
    b, err := raster.ReadPpmFrom(pipe)
    if err != nil {
        log.Fatal(err)
    }
    if err = b.WritePpmFile("Unfilledcirc.ppm"); err != nil {
        log.Fatal(err)
    }
}
```



## Julia

```julia
using Images, FileIO

img = load("data/bitmapOutputTest.jpg")
save("data/bitmapOutputTest.ppm", img)
```



## Kotlin

The code for this is similar to that for the [[Bitmap/Read a PPM file]] task except that the .jpg file is converted via a pipe to .ppm format using the ImageMagick 'convert' tool and stored in a BasicBitmapStorage object. It is then converted to grayscale and saved back to disk as a .jpg file.

```scala
// Version 1.2.40

import java.awt.Color
import java.awt.Graphics
import java.awt.image.BufferedImage
import java.io.PushbackInputStream
import java.io.File
import javax.imageio.ImageIO

class BasicBitmapStorage(width: Int, height: Int) {
    val image = BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    fun fill(c: Color) {
        val g = image.graphics
        g.color = c
        g.fillRect(0, 0, image.width, image.height)
    }

    fun setPixel(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB())

    fun getPixel(x: Int, y: Int) = Color(image.getRGB(x, y))

    fun toGrayScale() {
        for (x in 0 until image.width) {
            for (y in 0 until image.height) {
                var rgb  = image.getRGB(x, y)
                val red   = (rgb shr 16) and 0xFF
                val green = (rgb shr  8) and 0xFF
                val blue  =  rgb and 0xFF
                val lumin = (0.2126 * red + 0.7152 * green + 0.0722 * blue).toInt()
                rgb = (lumin shl 16) or (lumin shl 8) or lumin
                image.setRGB(x, y, rgb)
            }
        }
    }
}

fun PushbackInputStream.skipComment() {
    while (read().toChar() != '\n') {}
}

fun PushbackInputStream.skipComment(buffer: ByteArray) {
    var nl: Int
    while (true) {
        nl = buffer.indexOf(10) // look for newline at end of comment
        if (nl != -1) break
        read(buffer)  // read another buffer full if newline not yet found
    }
    val len = buffer.size
    if (nl < len - 1) unread(buffer, nl + 1, len - nl - 1)
}

fun Byte.toUInt() = if (this < 0) 256 + this else this.toInt()

fun main(args: Array<String>) {
    // use file, output_piped.jpg, created in the
    // Bitmap/PPM conversion through a pipe task
    val pb = ProcessBuilder("convert", "output_piped.jpg", "ppm:-")
    pb.directory(null)
    pb.redirectOutput(ProcessBuilder.Redirect.PIPE)
    val proc = pb.start()
    val pStdOut = proc.inputStream
    val pbis = PushbackInputStream(pStdOut, 80)
    pbis.use {
        with (it) {
            val h1 = read().toChar()
            val h2 = read().toChar()
            val h3 = read().toChar()
            if (h1 != 'P' || h2 != '6' || h3 != '\n') {
                println("Not a P6 PPM file")
                System.exit(1)
            }
            val sb = StringBuilder()
            while (true) {
                val r = read().toChar()
                if (r == '#') { skipComment(); continue }
                if (r == ' ') break  // read until space reached
                sb.append(r.toChar())
            }
            val width = sb.toString().toInt()
            sb.setLength(0)
            while (true) {
                val r = read().toChar()
                if (r == '#') { skipComment(); continue }
                if (r == '\n') break  // read until new line reached
                sb.append(r.toChar())
            }
            val height = sb.toString().toInt()
            sb.setLength(0)
            while (true) {
                val r = read().toChar()
                if (r == '#') { skipComment(); continue }
                if (r == '\n') break  // read until new line reached
                sb.append(r.toChar())
            }
            val maxCol = sb.toString().toInt()
            if (maxCol !in 0..255) {
                println("Maximum color value is outside the range 0..255")
                System.exit(1)
            }
            var buffer = ByteArray(80)
            // get rid of any more opening comments before reading data
            while (true) {
                read(buffer)
                if (buffer[0].toChar() == '#') {
                    skipComment(buffer)
                }
                else {
                    unread(buffer)
                    break
                }
            }
            // read data
            val bbs = BasicBitmapStorage(width, height)
            buffer = ByteArray(width * 3)
            var y = 0
            while (y < height) {
                read(buffer)
                for (x in 0 until width) {
                    val c = Color(
                        buffer[x * 3].toUInt(),
                        buffer[x * 3 + 1].toUInt(),
                        buffer[x * 3 + 2].toUInt()
                    )
                    bbs.setPixel(x, y, c)
                }
                y++
            }
            // convert to grayscale and save to a file
            bbs.toGrayScale()
            val grayFile = File("output_piped_gray.jpg")
            ImageIO.write(bbs.image, "jpg", grayFile)
        }
    }
}
```



## Mathematica

Based off the Julia program.

```Mathematica
Export["data/bitmapOutputTest.ppm",Import["data/bitmapOutputTest.jpg"]];
```



## OCaml

The <code>read_ppm</code> function of the page [[read ppm file]] and used by the code below would need to be changed to take as parameter an input channel instead of the filename.

```ocaml
let read_image ~filename =
  if not(Sys.file_exists filename)
  then failwith(Printf.sprintf "the file %s does not exist" filename);
  let cmd = Printf.sprintf "convert \"%s\" ppm:-" filename in
  let ic, oc = Unix.open_process cmd in
  let img = read_ppm ~ic in
  (img)
;;
```



## Perl 6

Uses pieces from [[Bitmap#Perl_6| Bitmap]] and [[Bitmap/Read_a_PPM_file#Perl_6| Read a PPM file]] tasks. Included here to make a complete, runnable program.

Uses imagemagick convert to pipe the image in. 


```perl6
class Pixel { has UInt ($.R, $.G, $.B) }
class Bitmap {
    has UInt ($.width, $.height);
    has Pixel @.data;
}

role PPM {
    method P6 returns Blob {
	"P6\n{self.width} {self.height}\n255\n".encode('ascii')
	~ Blob.new: flat map { .R, .G, .B }, self.data
    }
}

sub getline ( $proc ) {
    my $line = '#'; # skip comment when reading a .png
    $line = $proc.out.get while $line.substr(0,1) eq '#';
    $line;
}

my $filename = './camelia.png';

my $proc = run 'convert', $filename, 'ppm:-', :enc('ISO-8859-1'), :out;

my $type = getline($proc);
my ($width, $height) = getline($proc).split: ' ';
my $depth = getline($proc);

my Bitmap $b = Bitmap.new( width => $width.Int, height => $height.Int) but PPM;

$b.data = $proc.out.slurp.ords.rotor(3).map:
  { Pixel.new(R => .[0], G => .[1], B => .[2]) };

'./camelia.ppm'.IO.open(:bin, :w).write: $b.P6;
```


See [https://github.com/thundergnat/rc/blob/master/img/camelia.png camelia image here].


## PicoLisp


```PicoLisp
(setq *Ppm (ppmRead '("convert" "img.jpg" "ppm:-")))
```



## Python


```Python

"""
Adapted from https://stackoverflow.com/questions/26937143/ppm-to-jpeg-jpg-conversion-for-python-3-4-1
Requires pillow-5.3.0 with Python 3.7.1 32-bit on Windows.
Sample ppm graphics files from http://www.cs.cornell.edu/courses/cs664/2003fa/images/
"""

from PIL import Image

# boxes_1.jpg is the jpg version of boxes_1.ppm

im = Image.open("boxes_1.jpg")
im.save("boxes_1v2.ppm")

```

Does not need to pipe through a conversion utility 
because the Pillow module does the conversion.


## Racket


```racket


(define (read-ppm port)
  (parameterize ([current-input-port port])
    (define magic (read-line))
    (match-define (list w h) (string-split (read-line) " "))
    (define width (string->number w))
    (define height (string->number h))
    (define maxcol (string->number (read-line)))
    (define bm (make-object bitmap% width height))
    (define dc (new bitmap-dc% [bitmap bm]))
    (send dc set-smoothing 'unsmoothed)
    (define (adjust v) (* 255 (/ v maxcol)))
    (for/list ([x width])
      (for/list ([y height])
        (define red (read-byte))
        (define green (read-byte))
        (define blue (read-byte))
        (define color (make-object color% (adjust red) (adjust green) (adjust blue)))
        (send dc set-pen color 1 'solid)
        (send dc draw-point x y)))
    bm))

(define (image->bmp filename)
  (define command (format "convert ~a ppm:-" filename))
  (match-define (list in out pid err ctrl)  (process command))
  (define bmp (read-ppm in))
  (close-input-port in)
  (close-output-port out)
  bmp)

(image->bmp "input.jpg")
```



## Ruby

Extending [[Read ppm file#Ruby]] and [[PPM conversion through a pipe#Ruby]]. Uses the ImageMagick <code>convert</code> tool.


```ruby
class Pixmap
  def self.read_ppm(ios)
    format = ios.gets.chomp
    width, height = ios.gets.chomp.split.map {|n| n.to_i }
    max_colour = ios.gets.chomp

    if (not PIXMAP_FORMATS.include?(format)) or 
        width < 1 or height < 1 or
        max_colour != '255'
    then
      ios.close
      raise StandardError, "file '#{filename}' does not start with the expected header"
    end
    ios.binmode if PIXMAP_BINARY_FORMATS.include?(format)

    bitmap = self.new(width, height)
    height.times do |y|
      width.times do |x|
        # read 3 bytes
        red, green, blue = case format
          when 'P3' then ios.gets.chomp.split
          when 'P6' then ios.read(3).unpack('C3')
        end
        bitmap[x,y] = RGBColour.new(red, green, blue)
      end
    end
    ios.close
    bitmap
  end

  def self.open(filename)
    read_ppm(File.open(filename, 'r'))
  end

  def self.open_from_jpeg(filename)
    read_ppm(IO.popen("convert jpg:#{filename} ppm:-", 'r'))
  end
end

bitmap = Pixmap.open_from_jpeg('file.jpg')
```



## Tcl

```tcl
package require Tk

proc magickalReadImage {bufferImage fileName} {
    set f [open |[list convert [file normalize $fileName] ppm:-] "rb"]
    try {
        $bufferImage put [read $f] -format ppm
    } finally {
        close $f
    }
}
```



## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

Using the convert utility by ImageMagick:

```zkl
p:=System.popen(0'|convert "fractalTree.jpg" ppm:-|,"r");
img:=PPM.readPPM(p); p.close();
```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have an external OS/command processor. -->
