+++
title = "Bitmap/Write a PPM file"
description = ""
date = 2019-08-24T23:01:31Z
aliases = []
[extra]
id = 3216
[taxonomies]
categories = []
tags = []
+++

{{task|Raster graphics operations}}
[[Category:Input Output]]

Using the data storage type defined [[Basic_bitmap_storage|on this page]] for raster images, write the image to a PPM file (binary P6 prefered). <BR>
(Read [[wp:Netpbm_format|the definition of PPM file]] on Wikipedia.)


## Ada


```ada
with Ada.Characters.Latin_1;
with Ada.Streams.Stream_IO;  use Ada.Streams.Stream_IO;

procedure Put_PPM (File : File_Type; Picture : Image) is
   use Ada.Characters.Latin_1;
   Size   : constant String := Integer'Image (Picture'Length (2)) & Integer'Image (Picture'Length (1));
   Buffer : String (1..Picture'Length (2) * 3);
   Color  : Pixel;
   Index  : Positive;
begin
   String'Write (Stream (File), "P6" & LF);
   String'Write (Stream (File), Size (2..Size'Last) & LF);
   String'Write (Stream (File), "255" & LF);
   for I in Picture'Range (1) loop
      Index := Buffer'First;
      for J in Picture'Range (2) loop
         Color := Picture (I, J);
         Buffer (Index)     := Character'Val (Color.R);
         Buffer (Index + 1) := Character'Val (Color.G);
         Buffer (Index + 2) := Character'Val (Color.B);
         Index := Index + 3;
      end loop;
      String'Write (Stream (File), Buffer);
   end loop;
   Character'Write (Stream (File), LF);
end Put_PPM;
```

The solution writes the image into an opened file. The file format might fail to work on certain [[OS]]es, because output might mangle control characters like LF, CR, FF, HT, VT etc. The OS might also limit the line length of a text file. In general it is a bad idea to mix binary and text output in one file. This solution uses ''stream I/O'', which should be as portable as possible.



## Aime


```aime
integer i, h, j, w;
file f;

w = 640;
h = 320;

f.create("out.ppm", 00644);
f.form("P6\n~ ~\n255\n", w, h);

j = 0;
do {
    srand(j >> 4);
    i = 0;
    do {
        16.times(f_bytes, f, drand(255), drand(255), drand(255));
    } while ((i += 16) < w);
} while ((j += 1) < h);
```




## AutoHotkey

{{works with|AutoHotkey_L|45}}

```AutoHotkey

cyan := color(0,255,255) ; r,g,b
cyanppm := Bitmap(10, 10, cyan) ; width, height, background-color
Bitmap_write_ppm3(cyanppm, "cyan.ppm")
run, cyan.ppm
return

#include bitmap_storage.ahk  ; see basic bitmap storage task

Bitmap_write_ppm3(bitmap, filename)
{
file := FileOpen(filename, 0x11) ; utf-8, write
file.seek(0,0) ; overwrite BOM created with fileopen()
file.write("P3`n"  ;  `n = \n in ahk
. bitmap.width . " " . bitmap.height . "`n"
. "255`n")
  loop % bitmap.height
  {
    height := A_Index
    loop % bitmap.width
    {
      width := A_Index
      color := bitmap[height, width]
      file.Write(color.R . " ")
      file.Write(color.G . " ")
      file.Write(color.B . " ")
    }
    file.write("`n")
  }
  file.close()
 return 0
}

```




## AWK


```AWK
#!/usr/bin/awk -f
BEGIN {
split("255,0,0,255,255,0",R,",");
split("0,255,0,255,255,0",G,",");
split("0,0,255,0,0,0",B,",");

outfile = "P3.ppm";
printf("P3\n2 3\n255\n") >outfile;
for (k=1; k<=length(R); k++) {
   printf("%3i %3i %3i\n",R[k],G[k],B[k])>outfile
}
close(outfile);
}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      Width% = 200
      Height% = 200

      VDU 23,22,Width%;Height%;8,16,16,128
      *display c:\lena

      f% = OPENOUT("c:\lena.ppm")
      IF f%=0 ERROR 100, "Failed to open output file"
      BPUT #f%, "P6"
      BPUT #f%, "# Created using BBC BASIC"
      BPUT #f%, STR$(Width%) + " " +STR$(Height%)
      BPUT #f%, "255"

      FOR y% = Height%-1 TO 0 STEP -1
        FOR x% = 0 TO Width%-1
          rgb% = FNgetpixel(x%,y%)
          BPUT #f%, rgb% >> 16
          BPUT #f%, (rgb% >> 8) AND &FF
          BPUT #f%, rgb% AND &FF
        NEXT
      NEXT y%
      CLOSE#f%

      END

      DEF FNgetpixel(x%,y%)
      LOCAL col%
      col% = TINT(x%*2,y%*2)
      SWAP ?^col%,?(^col%+2)
      = col%
```



## C


This is one file program which writes one color in each step :

```cpp
#include <iostream>
#include <stdio.h>

int main(void)
{
  const int dimx = 800, dimy = 800;
  int i, j;
  FILE *fp = fopen("first.ppm", "wb"); /* b - binary mode */
  (void) fprintf(fp, "P6\n%d %d\n255\n", dimx, dimy);
  for (j = 0; j < dimy; ++j)
  {
    for (i = 0; i < dimx; ++i)
    {
      static unsigned char color[3];
      color[0] = i % 256;  /* red */
      color[1] = j % 256;  /* green */
      color[2] = (i * j) % 256;  /* blue */
      (void) fwrite(color, 1, 3, fp);
    }
  }
  (void) fclose(fp);
  return EXIT_SUCCESS;
}
```



This program writes whole array in one step :


```c
#include <stdio.h>

int main()
{
  const char *filename = "n.pgm";
  int x, y;
  /* size of the image */
  const int x_max = 100;  /* width */
  const int y_max = 100;  /* height */
  /* 2D array for colors (shades of gray) */
  unsigned char data[y_max][x_max];
  /* color component is coded from 0 to 255 ;  it is 8 bit color file */
  const int MaxColorComponentValue = 255;
  FILE * fp;
  /* comment should start with # */
  const char *comment = "# this is my new binary pgm file";

  /* fill the data array */
  for (y = 0; y < y_max; ++y) {
    for (x = 0; x < x_max; ++x) {
      data[y][x] = (x + y) & 255;
    }
  }

  /* write the whole data array to ppm file in one step */
  /* create new file, give it a name and open it in binary mode */
  fp = fopen(filename, "wb");
  /* write header to the file */
  fprintf(fp, "P5\n %s\n %d\n %d\n %d\n", comment, x_max, y_max,
          MaxColorComponentValue);
  /* write image data bytes to the file */
  fwrite(data, sizeof(data), 1, fp);
  fclose(fp);
  printf("OK - file %s saved\n", filename);
  return 0;
}
```




Here is a program which uses imglib library. One must create files imglib.h and imglib.c using code from category [[:Category:Raster graphics operations| Raster graphics operations]]. Start from [[Bitmap| bitmap page]]
This program writes whole array in one step.

Interface:


```c
void output_ppm(FILE *fd, image img);
```


Implementation:


```c
#include "imglib.h"

void output_ppm(FILE *fd, image img)
{
  unsigned int n;
  (void) fprintf(fd, "P6\n%d %d\n255\n", img->width, img->height);
  n = img->width * img->height;
  (void) fwrite(img->buf, sizeof(pixel), n, fd);
  (void) fflush(fd);
}
```


=={{header|C sharp|C#}}==
This implementation uses a StreamWriter to write the header in text, then writes the pixel data in binary using a BinaryWriter.

```csharp
using System;
using System.IO;
class PPMWriter
{
    public static void WriteBitmapToPPM(string file, Bitmap bitmap)
        {
            //Use a streamwriter to write the text part of the encoding
            var writer = new StreamWriter(file);
            writer.WriteLine("P6");
            writer.WriteLine($"{bitmap.Width}  {bitmap.Height}");
            writer.WriteLine("255");
            writer.Close();
            //Switch to a binary writer to write the data
            var writerB = new BinaryWriter(new FileStream(file, FileMode.Append));
            for (int x = 0; x < bitmap.Height; x++)
                for (int y = 0; y < bitmap.Width; y++)
                {
                    Color color = bitmap.GetPixel(y, x);
                    writerB.Write(color.R);
                    writerB.Write(color.G);
                    writerB.Write(color.B);
                }
            writerB.Close();
        }
}
```



## Common Lisp



```lisp
(defun write-rgb-buffer-to-ppm-file (filename buffer)
  (with-open-file (stream filename
			  :element-type '(unsigned-byte 8)
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
    (let* ((dimensions (array-dimensions buffer))
	   (width (first dimensions))
	   (height (second dimensions))
	   (header (format nil "P6~A~D ~D~A255~A"
			   #\newline
			   width height #\newline
			   #\newline)))
      (loop
	 :for char :across header
	 :do (write-byte (char-code char) stream)) #| Assumes char-codes match ASCII |#

      (loop
	 :for x :upfrom 0 :below width
	 :do (loop :for y :upfrom 0 :below height
		:do (let ((pixel (rgb-pixel buffer x y)))
		      (let ((red (rgb-pixel-red pixel))
			    (green (rgb-pixel-green pixel))
			    (blue (rgb-pixel-blue pixel)))
			(write-byte red stream)
			(write-byte green stream)
			(write-byte blue stream)))))))
  filename)
```



## D

The Image module contains a savePPM6 function to save binary PPM images.


## E


The code for this task is incorporated into [[Basic bitmap storage#E]].


## Erlang

Writes a bitmap to PPM file. Uses 24 bit color depth (color max value 255).

```erlang

-module(ppm).

-export([ppm/1, write/2]).

-define(WHITESPACE, <<10>>).
-define(SPACE, <<32>>).

% data structure introduced in task Bitmap (module ros_bitmap.erl)
-record(bitmap, {
    pixels = nil,
    shape = {0, 0}
  }).

% create ppm image from bitmap record
ppm(Bitmap) ->
    {Width, Height} = Bitmap#bitmap.shape,
    Pixels = ppm_pixels(Bitmap),
    Maxval = 255,  % original ppm format maximum
    list_to_binary([
      header(), width_and_height(Width, Height), maxval(Maxval), Pixels]).

% write bitmap as ppm file
write(Bitmap, Filename) ->
    Ppm = ppm(Bitmap),
    {ok, File} = file:open(Filename, [binary, write]),
    file:write(File, Ppm),
    file:close(File).

%%%%%%%%%%%% four parts of ppm file %%%%%%%%%%%%%%%%%%%%%%
header() ->
    [<<"P6">>, ?WHITESPACE].

width_and_height(Width, Height) ->
    [encode_decimal(Width), ?SPACE, encode_decimal(Height), ?WHITESPACE].

maxval(Maxval) ->
   [encode_decimal(Maxval), ?WHITESPACE].

ppm_pixels(Bitmap) ->
    % 24 bit color depth
    array:to_list(Bitmap#bitmap.pixels).

%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encode_decimal(Number) ->
    integer_to_list(Number).


```



## Euphoria

{{trans|C}}

```euphoria
constant dimx = 800, dimy = 800
constant fn = open("first.ppm","wb") -- b - binary mode
sequence color
printf(fn, "P6\n%d %d\n255\n", {dimx,dimy})
for j = 0 to dimy-1 do
    for i = 0 to dimx-1 do
        color = {
            remainder(i,256), -- red
            remainder(j,256), -- green
            remainder(i*j,256) -- blue
        }
        puts(fn,color)
    end for
end for
close(fn)
```


Procedure writing [[Bitmap#Euphoria|bitmap]] data storage:

```euphoria
procedure write_ppm(sequence filename, sequence image)
    integer fn,dimx,dimy
    dimy = length(image[1])
    dimx = length(image)
    fn = open(filename,"wb")
    printf(fn, "P6\n%d %d\n255\n", {dimx,dimy})
    for y = 1 to dimy do
        for x = 1 to dimx do
            puts(fn, and_bits(image[x][y], {#FF0000,#FF00,#FF}) /
                                           {#010000,#0100,#01}) -- unpack color triple
        end for
    end for
    close(fn)
end procedure
```



## FBSL

This code converts a Windows BMP to a PPM. Uses FBSL volatiles for brevity.

'''24-bpp P.O.T.-size BMP solution:'''
[[File:FBSLWritePpm.PNG|right]]

```qbasic
#ESCAPECHARS ON

DIM bmpin = ".\\LenaClr.bmp", ppmout = ".\\Lena.ppm", bmpblob = 54 ' Size of BMP file headers
FILEGET(FILEOPEN(bmpin, BINARY), FILELEN(bmpin)): FILECLOSE(FILEOPEN) ' Fill buffer

DIM ppmheader AS STRING * 256, breadth, height
LET(breadth, height) = 128 ' Image width and height
SPRINTF(ppmheader, "P6\n%d %d\n255\n", breadth, height) ' Create PPM file header

DIM ppmdata AS STRING * (STRLEN(ppmheader) + FILELEN - bmpblob)
DIM head = @ppmdata + STRLEN, tail = @FILEGET + FILELEN - breadth * 3 - 2 ' Start of last scanline
ppmdata = ppmheader ' Copy PPM file header

WHILE tail >= @FILEGET + bmpblob ' Flip upside down
	FOR DIM w = 0 TO (breadth - 1) * 3 STEP 3
		POKE(head + 0 + w, CHR(PEEK(tail + 2 + w, 1))) ' Swap R
		POKE(head + 1 + w, CHR(PEEK(tail + 1 + w, 1))) ' Keep G
		POKE(head + 2 + w, CHR(PEEK(tail + 0 + w, 1))) ' Swap B
	NEXT
	INCR(head, breadth * 3): DECR(tail, breadth * 3) ' Next scanline
WEND

FILEPUT(FILEOPEN(ppmout, BINARY_NEW), ppmdata): FILECLOSE(FILEOPEN)
```



## Forth


```forth
: write-ppm { bmp fid -- }
  s" P6"             fid write-line throw
  bmp bdim swap
  0 <# bl hold #s #> fid write-file throw
  0 <#         #s #> fid write-line throw
  s" 255"            fid write-line throw
  bmp bdata  bmp bdim * pixels
  bounds do
    i 3              fid write-file throw
  pixel +loop ;

s" red.ppm" w/o create-file throw
test over write-ppm
close-file throw
```



## Fortran

{{works with|Fortran|90 and later}}
It loads <code>RCImageBasic</code> module, which is defined [[Basic bitmap storage#Fortran|here]].

```fortran
module RCImageIO
  use RCImageBasic

  implicit none

contains

  subroutine output_ppm(u, img)
    integer, intent(in) :: u
    type(rgbimage), intent(in) :: img
    integer :: i, j

    write(u, '(A2)') 'P6'
    write(u, '(I0,'' '',I0)') img%width, img%height
    write(u, '(A)') '255'

    do j=1, img%height
       do i=1, img%width
          write(u, '(3A1)', advance='no') achar(img%red(i,j)), achar(img%green(i,j)), &
                                          achar(img%blue(i,j))
       end do
    end do

  end subroutine output_ppm

end module RCImageIO
```


## GAP


```gap
# Dirty implementation
# Only P3 format, an image is a list of 3 matrices (r, g, b)
# Max color is always 255
WriteImage := function(name, img)
  local f, r, g, b, i, j, maxcolor, nrow, ncol, dim;
  f := OutputTextFile(name, false);
  r := img[1];
  g := img[2];
  b := img[3];
  dim := DimensionsMat(r);
  nrow := dim[1];
  ncol := dim[2];
  maxcolor := 255;
  WriteLine(f, "P3");
  WriteLine(f, Concatenation(String(ncol), " ", String(nrow), " ", String(maxcolor)));
  for i in [1 .. nrow] do
    for j in [1 .. ncol] do
      WriteLine(f, Concatenation(String(r[i][j]), " ", String(g[i][j]), " ", String(b[i][j])));
    od;
  od;
  CloseStream(f);
end;

PutPixel := function(img, i, j, color)
  img[1][i][j] := color[1];
  img[2][i][j] := color[2];
  img[3][i][j] := color[3];
end;

GetPixel := function(img, i, j)
  return [img[1][i][j], img[2][i][j], img[3][i][j]];
end;

NewImage := function(nrow, ncol, color)
  local r, g, b;
  r := color[1] + NullMat(nrow, ncol);
  g := color[2] + NullMat(nrow, ncol);
  b := color[3] + NullMat(nrow, ncol);
  return [r, g, b];
end;

# Reproducing the example from Wikipedia
black := [ 0, 0, 0 ];
g := NewImage(2, 3, black);
PutPixel(g, 1, 1, [255, 0, 0]);
PutPixel(g, 1, 2, [0, 255, 0]);
PutPixel(g, 1, 3, [0, 0, 255]);
PutPixel(g, 2, 1, [255, 255, 0]);
PutPixel(g, 2, 2, [255, 255, 255]);
PutPixel(g, 2, 3, [0, 0, 0]);
WriteImage("example.ppm", g);
```


## Go

Code below writes 8-bit P6 format only.  See Bitmap task for additional file needed to build working raster package.

```go
package raster

import (
    "fmt"
    "io"
    "os"
)

// WriteTo outputs 8-bit P6 PPM format to an io.Writer.
func (b *Bitmap) WritePpmTo(w io.Writer) (err error) {
    // magic number
    if _, err = fmt.Fprintln(w, "P6"); err != nil {
        return
    }

    // comments
    for _, c := range b.Comments {
        if _, err = fmt.Fprintln(w, c); err != nil {
            return
        }
    }

    // x, y, depth
    _, err = fmt.Fprintf(w, "%d %d\n255\n", b.cols, b.rows)
    if err != nil {
        return
    }

    // raster data in a single write
    b3 := make([]byte, 3*len(b.px))
    n1 := 0
    for _, px := range b.px {
        b3[n1] = px.R
        b3[n1+1] = px.G
        b3[n1+2] = px.B
        n1 += 3
    }
    if _, err = w.Write(b3); err != nil {
        return
    }
    return
}

// WriteFile writes to the specified filename.
func (b *Bitmap) WritePpmFile(fn string) (err error) {
    var f *os.File
    if f, err = os.Create(fn); err != nil {
        return
    }
    if err = b.WritePpmTo(f); err != nil {
        return
    }
    return f.Close()
}
```

Demonstration program.  Note that it imports package raster.  To build package raster, put code above in one file, put code from Bitmap task in another, and compile and link them into a Go package.

```go
package main

// Files required to build supporting package raster are found in:
// * This task (immediately above)
// * Bitmap task

import (
    "raster"
    "fmt"
)

func main() {
    b := raster.NewBitmap(400, 300)
    b.FillRgb(0x240008) // a dark red
    err := b.WritePpmFile("write.ppm")
    if err != nil {
        fmt.Println(err)
    }
}
```



## Haskell


```haskell
{-# LANGUAGE ScopedTypeVariables #-}

module Bitmap.Netpbm(readNetpbm, writeNetpbm) where

import Bitmap
import Data.Char
import System.IO
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

nil :: a
nil = undefined

readNetpbm :: forall c. Color c => FilePath -> IO (Image RealWorld c)
readNetpbm path = do
    let die = fail "readNetpbm: bad format"
    ppm <- readFile path
    let (s, rest) = splitAt 2 ppm
    unless (s == magicNumber) die
    let getNum :: String -> IO (Int, String)
        getNum ppm = do
            let (s, rest) = span isDigit $ skipBlanks ppm
            when (null s) die
            return (read s, rest)
    (width, rest) <- getNum rest
    (height, rest) <- getNum rest
    (_, c : rest) <-
        if getMaxval then getNum rest else return (nil, rest)
    unless (isSpace c) die
    i <- stToIO $ listImage width height $
        fromNetpbm $ map fromEnum rest
    return i
  where skipBlanks =
           dropWhile isSpace .
           until ((/= '#') . head) (tail . dropWhile (/= '\n')) .
           dropWhile isSpace
        magicNumber = netpbmMagicNumber (nil :: c)
        getMaxval = not $ null $ netpbmMaxval (nil :: c)

writeNetpbm :: forall c. Color c => FilePath -> Image RealWorld c -> IO ()
writeNetpbm path i = withFile path WriteMode $ \h -> do
    (width, height) <- stToIO $ dimensions i
    let w = hPutStrLn h
    w $ magicNumber
    w $ show width ++ " " ++ show height
    unless (null maxval) (w maxval)
    stToIO (getPixels i) >>= hPutStr h . toNetpbm
  where magicNumber = netpbmMagicNumber (nil :: c)
        maxval = netpbmMaxval (nil :: c)
```



## J

'''Solution:'''

```j
require 'files'

NB. ($x) is height, width, colors per pixel
writeppm=:dyad define
  header=. 'P6',LF,(":1 0{$x),LF,'255',LF
  (header,,x{a.) fwrite y
)
```

'''Example:'''
Using routines from [[Basic_bitmap_storage#J|Basic Bitmap Storage]]:

```j
   NB. create 10 by 10 block of magenta pixels in top right quadrant of a 300 wide by 600 high green image
   pixellist=: >,{;~i.10
   myimg=: ((150 + pixellist) ; 255 0 255) setPixels 0 255 0 makeRGB 600 300
   myimg writeppm jpath '~temp/myimg.ppm'
540015
```



## Julia

{{works with|Julia|0.6}}


```julia
using Images, FileIO

h, w = 50, 70
img = zeros(RGB{N0f8}, h, w)
img[10:40, 5:35] = colorant"skyblue"
for i in 26:50, j in (i-25):40
    img[i, j] = colorant"sienna1"
end

save("data/bitmapWrite.ppm", img)
save("data/bitmapWrite.png", img)
```



## Kotlin

For convenience, we repeat the code for the class used in the [[Bitmap]] task here.

```scala
// Version 1.2.40

import java.awt.Color
import java.awt.Graphics
import java.awt.image.BufferedImage
import java.io.FileOutputStream

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

    // now write it to a PPM file
    val fos = FileOutputStream("output.ppm")
    val buffer = ByteArray(width * 3)  // write one line at a time
    fos.use {
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



## Lua


```lua


-- helper function, simulates PHP's array_fill function
local array_fill = function(vbegin, vend, value)
    local t = {}
    for i=vbegin, vend do
        t[i] = value
    end
    return t
end

Bitmap = {}
Bitmap.__index = Bitmap

function Bitmap.new(width, height)
    local self = {}
    setmetatable(self, Bitmap)
    local white = array_fill(0, width, {255, 255, 255})
    self.data = array_fill(0, height, white)
    self.width = width
    self.height = height
    return self
end

function Bitmap:writeRawPixel(file, c)
    local dt
    dt = string.format("%c", c)
    file:write(dt)
end

function Bitmap:writeComment(fh, ...)
    local strings = {...}
    local str = ""
    local result
    for _, s in pairs(strings) do
        str = str .. tostring(s)
    end
    result = string.format("# %s\n", str)
    fh:write(result)
end

function Bitmap:writeP6(filename)
    local fh = io.open(filename, 'w')
    if not fh then
        error(string.format("failed to open %q for writing", filename))
    else
        fh:write(string.format("P6 %d %d 255\n", self.width, self.height))
        self:writeComment(fh, "automatically generated at ", os.date())
        for _, row in pairs(self.data) do
            for _, pixel in pairs(row) do
                self:writeRawPixel(fh, pixel[1])
                self:writeRawPixel(fh, pixel[2])
                self:writeRawPixel(fh, pixel[3])
            end
        end
    end
end

function Bitmap:fill(x, y, width, height, color)
    width = (width == nil) and self.width or width
    height = (height == nil) and self.height or height
    width = width + x
    height = height + y
    for i=y, height do
        for j=x, width do
            self:setPixel(j, i, color)
        end
    end
end

function Bitmap:setPixel(x, y, color)
    if x >= self.width then
        --error("x is bigger than self.width!")
        return false
    elseif x < 0 then
        --error("x is smaller than 0!")
        return false
    elseif y >= self.height then
        --error("y is bigger than self.height!")
        return false
    elseif y < 0 then
        --error("y is smaller than 0!")
        return false
    end
    self.data[y][x] = color
    return true
end

function example_colorful_stripes()
    local w = 260*2
    local h = 260*2
    local b = Bitmap.new(w, h)
    --b:fill(2, 2, 18, 18, {240,240,240})
    b:setPixel(0, 15, {255,68,0})
    for i=1, w do
        for j=1, h do
            b:setPixel(i, j, {
                    (i + j * 8) % 256,
                    (j + (255 * i)) % 256,
                    (i * j) % 256
                }
            );
        end
    end
    return b
end

example_colorful_stripes():writeP6('p6.ppm')

```



## M2000 Interpreter

Added ToFile in group which return the function Bitmap. In this example we export using ToFile and get bytes (unsigned values) from buffer, and we export from outside, using getpixel and convert the RGB value to  bytes (color returned as a negative number, so we have to invert before further process it)

### P3 type


```M2000 Interpreter

Module Checkit {
      Function Bitmap (x as long, y as long) {
            if x<1 or y<1 then  Error "Wrong dimensions"
            structure rgb {
                  red as byte
                  green as byte
                  blue as byte
            }
            m=len(rgb)*x mod 4
            if m>0 then m=4-m  ' add some bytes to raster line
            m+=len(rgb) *x
            Structure rasterline {
                  {
                        pad as byte*m
                  }
                  \\ union pad+hline
                  hline as rgb*x
            }

            Structure Raster {
                  magic as integer*4
                  w as integer*4
                  h as integer*4
                  lines as rasterline*y
            }
            Buffer Clear Image1 as Raster
            \\ 24 chars as header to be used from bitmap render build in functions
            Return Image1, 0!magic:="cDIB", 0!w:=Hex$(x,2), 0!h:=Hex$(y, 2)
            \\ fill white (all 255)
            \\ Str$(string) convert to ascii, so we get all characters from words  width to byte width
            Return Image1, 0!lines:=Str$(String$(chrcode$(255), Len(rasterline)*y))
            Buffer Clear Pad as Byte*4
            SetPixel=Lambda Image1, Pad,aLines=Len(Raster)-Len(Rasterline), blines=-Len(Rasterline) (x, y, c) ->{
                  where=alines+3*x+blines*y
                  if c>0 then c=color(c)
                  c-!
                  Return Pad, 0:=c as long
                  Return Image1, 0!where:=Eval(Pad, 2) as byte, 0!where+1:=Eval(Pad, 1) as byte, 0!where+2:=Eval(Pad, 0) as byte

            }
            GetPixel=Lambda Image1,aLines=Len(Raster)-Len(Rasterline), blines=-Len(Rasterline) (x,y) ->{
                  where=alines+3*x+blines*y
                  =color(Eval(image1, where+2 as byte), Eval(image1, where+1 as byte), Eval(image1, where as byte))
            }
            StrDib$=Lambda$ Image1, Raster -> {
                  =Eval$(Image1, 0, Len(Raster))
            }
            CopyImage=Lambda Image1 (image$) -> {
                  if left$(image$,12)=Eval$(Image1, 0, 24 ) Then  {
                         Return Image1, 0:=Image$
                  } Else Error "Can't Copy Image"
            }
            Export2File=Lambda Image1, x, y (f) -> {
                  \\ use this between open and close
                  Print #f, "P3"
                  Print #f,"# Created using M2000 Interpreter"
                  Print #f, x;" ";y
                  Print #f, 255
                  x2=x-1
                  where=24
                  For y1= 0 to y-1 {
                        a$=""
                        For x1=0 to x2 {
                              Print #f, a$;Eval(Image1, where +2 as byte);" ";
                              Print #f,  Eval(Image1, where+1 as byte);" ";
                              Print #f,  Eval(Image1, where as byte);
                              where+=3
                              a$=" "
                        }
                        Print #f
                        m=where mod 4
                        if m<>0 then where+=4-m
                  }
            }
            Group Bitmap {
                  SetPixel=SetPixel
                  GetPixel=GetPixel
                  Image$=StrDib$
                  Copy=CopyImage
                  ToFile=Export2File
            }
            =Bitmap
      }

      A=Bitmap(10, 10)
      Call A.SetPixel(5,5, color(128,0,255))
      Open "A2.PPM" for Output as #F
            Call A.ToFile(F)
      Close #f
      ' is the same as this one
      Try {
            Open "A.PPM" for Output as #F
            Print #f, "P3"
            Print #f,"# Created using M2000 Interpreter"
            Print #f, 10;" ";10
            Print #f, 255
            For y=10-1 to 0 {
                  a$=""
                  For x=0 to 10-1 {
                        rgb=-A.GetPixel(x, y)
                        Print #f, a$;Binary.And(rgb, 0xFF); " ";
                        Print #f, Binary.And(Binary.Shift(rgb, -8), 0xFF); " ";
                        Print #f, Binary.Shift(rgb, -16);
                        a$=" "
                  }
                  Print #f
            }
            Close #f
      }
}
Checkit


```


{{out}}
<pre style="height:30ex;overflow:scroll">
P3
# Created using M2000 Interpreter
10 10
255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 128 0 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
</pre >


### P6 type


```M2000 Interpreter

Module PPMbinaryP6 {
      If Version<9.4 then 1000
      If Version=9.4 Then if Revision<19 then 1000
      Module Checkit {
            Function Bitmap  {
                  def x as long, y as long
                  If match("NN") then {
                       Read x, y
                  } else.if Match("N") Then  {
                        E$="Not a ppm file"
                        Read f as long
                        buffer whitespace as byte
                        if not Eof(f) then {
                              get #f, whitespace : iF eof(f) then Error E$
                              P6$=eval$(whitespace)
                              get #f, whitespace : iF eof(f) then Error E$
                              P6$+=eval$(whitespace)
                              def boolean getW=true, getH=true, getV=true
                              def long v
                              \\ str$("P6") has 2 bytes. "P6" has 4 bytes
                              If p6$=str$("P6") Then {
                                    do {
                                          get #f, whitespace
                                          if Eval$(whitespace)=str$("#") then {
                                                do {
                                                      iF eof(f) then Error E$
                                                      get #f, whitespace
                                                } until eval(whitespace)=10
                                          } else  {
                                               select case eval(whitespace)
                                                case 32, 9, 13, 10
                                                {
                                                      if getW and x<>0 then {
                                                            getW=false
                                                      } else.if getH  and y<>0 then {
                                                            getH=false
                                                      } else.if getV and v<>0 then {
                                                            getV=false
                                                      }
                                                }
                                                case 48 to 57
                                                {
                                                      if getW then {
                                                           x*=10
                                                           x+=eval(whitespace, 0)-48
                                                      } else.if getH then {
                                                           y*=10
                                                           y+=eval(whitespace, 0)-48
                                                      } else.if getV then {
                                                           v*=10
                                                           v+=eval(whitespace, 0)-48
                                                      }
                                                }
                                                End Select
                                          }
                                          iF eof(f) then Error E$
                                    } until getV=false
                              }  else Error "Not a P6 ppm"
                        }
                  } else Error "No proper arguments"
                  if x<1 or y<1 then  Error "Wrong dimensions"
                  structure rgb {
                        red as byte
                        green as byte
                        blue as byte
                  }
                  m=len(rgb)*x mod 4
                  if m>0 then m=4-m  ' add some bytes to raster line
                  m+=len(rgb) *x
                  Structure rasterline {
                        {
                              pad as byte*m
                        }
                        \\ union pad+hline
                        hline as rgb*x
                  }
                  \\ we use union linesB and lines
                  \\ so we can address linesb as bytes
                  Structure Raster {
                        magic as integer*4
                        w as integer*4
                        h as integer*4
                        {
                              linesB as byte*len(rasterline)*y
                        }
                        lines as rasterline*y
                  }
                  Buffer Clear Image1 as Raster
                  \\ 24 chars as header to be used from bitmap render build in functions
                  Return Image1, 0!magic:="cDIB", 0!w:=Hex$(x,2), 0!h:=Hex$(y, 2)
                  \\ fill white (all 255)
                  \\ Str$(string) convert to ascii, so we get all characters from words  width to byte width
                  if not valid(f) then  Return Image1, 0!lines:=Str$(String$(chrcode$(255), Len(rasterline)*y))
                  Buffer Clear Pad as Byte*4
                  SetPixel=Lambda Image1, Pad,aLines=Len(Raster)-Len(Rasterline), blines=-Len(Rasterline) (x, y, c) ->{
                        where=alines+3*x+blines*y
                        if c>0 then c=color(c)
                        c-!
                        Return Pad, 0:=c as long
                        Return Image1, 0!where:=Eval(Pad, 2) as byte, 0!where+1:=Eval(Pad, 1) as byte, 0!where+2:=Eval(Pad, 0) as byte
                  }
                  GetPixel=Lambda Image1,aLines=Len(Raster)-Len(Rasterline), blines=-Len(Rasterline) (x,y) ->{
                        where=alines+3*x+blines*y
                        =color(Eval(image1, where+2 as byte), Eval(image1, where+1 as byte), Eval(image1, where as byte))
                  }
                  StrDib$=Lambda$ Image1, Raster -> {
                        =Eval$(Image1, 0, Len(Raster))
                  }
                  CopyImage=Lambda Image1 (image$) -> {
                        if left$(image$,12)=Eval$(Image1, 0, 24 ) Then  {
                               Return Image1, 0:=Image$
                        } Else Error "Can't Copy Image"
                  }
                  Export2File=Lambda Image1, x, y (f) -> {
                        \\ use this between open and close
                        Print #f, "P6";chr$(10);
                        Print #f,"# Created using M2000 Interpreter";chr$(10);
                        Print #f, x;" ";y;" 255";chr$(10);
                        x2=x-1
                        where=0
                        Buffer pad as byte*3
                        For y1= 0 to y-1 {
                              For x1=0 to x2 {
                                    \\ use linesB which is array of bytes
                                   Return pad, 0:=eval$(image1, 0!linesB!where, 3)
                                   Push Eval(pad, 2)
                                   Return pad, 2:=Eval(pad, 0), 0:=Number
                                   Put #f, pad
                                   where+=3
                              }
                              m=where mod 4
                              if m<>0 then where+=4-m
                        }
                  }
                  if valid(F) then {
                        x0=x-1
                        where=0
                        Buffer Pad1 as byte*3
                              For y1=y-1 to 0 {
                                    For x1=0 to x0 {
                                          Get #f, Pad1  ' Read binary
                                          \\ reverse rgb
                                          Push Eval(pad1, 2)
                                          Return pad1, 2:=Eval(pad1, 0), 0:=Number
                                          Return Image1, 0!linesB!where:=Eval$(Pad1)
                                          where+=3
                                    }
                                    m=where mod 4
                                    if m<>0 then where+=4-m
                              }
                  }
                  Group Bitmap {
                        SetPixel=SetPixel
                        GetPixel=GetPixel
                        Image$=StrDib$
                        Copy=CopyImage
                        ToFile=Export2File
                  }
                  =Bitmap
            }
            A=Bitmap(10, 10)
            Call A.SetPixel(5,5, color(128,0,255))
            Open "A.PPM" for Output as #F
                  Call A.ToFile(F)
            Close #f

            Print "Saved"
            Open "A.PPM" for Input as #F
                  C=Bitmap(f)
                  Copy 400*twipsx,200*twipsy use C.Image$()
            Close #f
         }
      Checkit
      End
      1000  Error "Need Version 9.4, Revision 19 or higher"
}
PPMbinaryP6


```


=={{header|Mathematica}}/ {{header|Wolfram Language}}==

```Mathematica
Export["file.ppm",image,"PPM"]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
R=[255,0,0;255,255,0];
G=[0,255,0;255,255,0];
B=[0,0,255;0,0,0];


r = R'; r(:);
g = R'; g(:);
b = R'; b(:);
fid=fopen('p6.ppm','w');
fprintf(fid,'P6\n%i %i\n255\n',size(R));
fwrite(fid,[r,g,b]','uint8');
fclose(fid);
```



=={{header|Modula-3}}==
<code>Bitmap</code> is the module from [[Basic_bitmap_storage#Modula-3|Basic Bitmap Storage]].

```modula3
INTERFACE PPM;

IMPORT Bitmap, Pathname;

PROCEDURE Create(imgfile: Pathname.T; img: Bitmap.T);

END PPM.
```


```modula3
MODULE PPM;

IMPORT Bitmap, Wr, FileWr, Pathname;
FROM Fmt IMPORT F, Int;

<*FATAL ANY*>

VAR imgfilewr: FileWr.T;

PROCEDURE Create(imgfile: Pathname.T; img: Bitmap.T) =
  VAR height := LAST(img^);
      width := LAST(img[0]);
      color: Bitmap.Pixel;
  BEGIN
    imgfilewr := FileWr.Open(imgfile);
    Wr.PutText(imgfilewr, F("P6\n%s %s\n255\n", Int(height + 1), Int(width + 1)));
    FOR i := 0 TO height DO
      FOR j := 0 TO width DO
        color := img[i,j];
        Wr.PutChar(imgfilewr, VAL(color.R, CHAR));
        Wr.PutChar(imgfilewr, VAL(color.G, CHAR));
        Wr.PutChar(imgfilewr, VAL(color.B, CHAR));
      END;
    END;
    Wr.PutChar(imgfilewr, '\n');
    Wr.Flush(imgfilewr);
  END Create;

BEGIN
END PPM.
```


== {{Header|Nim}} ==

```nim
proc writePPM(img: Image, f: TFile) =
  f.writeln "P6\n", img.w, " ", img.h, "\n255"

  for x,y in img.indices:
    f.write char(img[x,y].r)
    f.write char(img[x,y].g)
    f.write char(img[x,y].b)
```


== {{Header|OCaml}} ==


```ocaml
let output_ppm ~oc ~img:(_, r_channel, g_channel, b_channel) =
  let width = Bigarray.Array2.dim1 r_channel
  and height = Bigarray.Array2.dim2 r_channel in
  Printf.fprintf oc "P6\n%d %d\n255\n" width height;
  for y = 0 to pred height do
    for x = 0 to pred width do
      output_char oc (char_of_int r_channel.{x,y});
      output_char oc (char_of_int g_channel.{x,y});
      output_char oc (char_of_int b_channel.{x,y});
    done;
  done;
  output_char oc '\n';
  flush oc;
;;
```



## Oz

As a function in the module <code>BitmapIO.oz</code>:

```oz
functor
import
   Bitmap
   Open
export
   %% Read
   Write
define
   %% Omitted: Read

   proc {Write B=bitmap(array2d(width:W height:H ...)) Filename}
      F = {New Open.file init(name:Filename flags:[write create truncate binary])}

      proc {WriteColor8 color(R G B)}
	 {F write(vs:[R G B])}
      end

      fun {ToBytes C}
	 [C div 0x100  C mod 0x100]
      end

      proc {WriteColor16 color(R G B)}
	 {F write(vs:{Flatten {Map [R G B] ToBytes}})}
      end

      MaxCol = {Bitmap.maxValue B}
      MaxVal#Writer = if MaxCol =< 0xff then 0xff#WriteColor8
		      else 0xffff#WriteColor16
		      end
      Header = "P6\n"#W#" "#H#" "#MaxVal#"\n"
   in
      try
	 {F write(vs:Header)}
	 {Bitmap.forAllPixels B Writer}
      finally
	 {F close}
      end
   end
end
```



## Perl

{{libheader|Imager}}

```perl
use Imager;

$image = Imager->new(xsize => 200, ysize => 200);
$image->box(filled => 1, color => red);
$image->box(filled => 1, color => black,
            xmin =>  50, ymin =>  50,
            xmax => 150, ymax => 150);
$image->write(file => 'bitmap.ppm') or die $image->errstr;
```



## Perl 6

{{works with|Rakudo|2016-01}}


```perl6
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

$*OUT.write: $b.P6;
```

Converted to a png. (ppm files not locally supported)

[[File:Ppm-perl6.png‎]]


## Phix

Copy of [[Bitmap/Write_a_PPM_file#Euphoria|Euphoria]]. Included as demo\rosetta\Bitmap_write_ppm.exw The results may be verified with demo\rosetta\viewppm.exw

```Phix
constant dimx = 512, dimy = 512
constant fn = open("first.ppm","wb") -- b - binary mode
sequence color
printf(fn, "P6\n%d %d\n255\n", {dimx,dimy})
for y=0 to dimy-1 do
    for x=0 to dimx-1 do
        color = {remainder(x,256),   -- red
                 remainder(y,256),   -- green
                 remainder(x*y,256)} -- blue
        puts(fn,color)
    end for
end for
close(fn)

```

The following more general purpose routine is used in several other examples:

```Phix
procedure write_ppm(sequence filename, sequence image)
integer fn,dimx,dimy
sequence colour_triple
    fn = open(filename,"wb")
    dimx = length(image)
    dimy = length(image[1])
    printf(fn, "P6\n%d %d\n255\n", {dimx,dimy})
    for y = 1 to dimy do
        for x = 1 to dimx do
            colour_triple = sq_div(sq_and_bits(image[x][y], {#FF0000,#FF00,#FF}),
                                                            {#010000,#0100,#01})
            puts(fn, colour_triple)
        end for
    end for
    close(fn)
end procedure
```



## PHP

Writes a P6 binary file

```PHP
class Bitmap {
  public $data;
  public $w;
  public $h;
  public function __construct($w = 16, $h = 16){
    $white = array_fill(0, $w, array(255,255,255));
    $this->data = array_fill(0, $h, $white);
    $this->w = $w;
    $this->h = $h;
  }
  //Fills a rectangle, or the whole image with black by default
  public function fill($x = 0, $y = 0, $w = null, $h = null, $color = array(0,0,0)){
    if (is_null($w)) $w = $this->w;
    if (is_null($h)) $h = $this->h;
    $w += $x;
    $h += $y;
    for ($i = $y; $i < $h; $i++){
      for ($j = $x; $j < $w; $j++){
        $this->setPixel($j, $i, $color);
      }
    }
  }
  public function setPixel($x, $y, $color = array(0,0,0)){
    if ($x >= $this->w) return false;
    if ($x < 0) return false;
    if ($y >= $this->h) return false;
    if ($y < 0) return false;
    $this->data[$y][$x] = $color;
  }
  public function getPixel($x, $y){
    return $this->data[$y][$x];
  }
  public function writeP6($filename){
    $fh = fopen($filename, 'w');
    if (!$fh) return false;
    fputs($fh, "P6 {$this->w} {$this->h} 255\n");
    foreach ($this->data as $row){
      foreach($row as $pixel){
        fputs($fh, pack('C', $pixel[0]));
        fputs($fh, pack('C', $pixel[1]));
        fputs($fh, pack('C', $pixel[2]));
      }
    }
    fclose($fh);
  }
}

$b = new Bitmap(16,16);
$b->fill();
$b->fill(2, 2, 18, 18, array(240,240,240));
$b->setPixel(0, 15, array(255,0,0));
$b->writeP6('p6.ppm');
```



## PL/I


```PL/I
/* BITMAP FILE: write out a file in PPM format, P6 (binary). 14/5/2010 */
test: procedure options (main);
   declare image (0:19,0:19) bit (24);
   declare 1 pixel union,
            2 color bit (24) aligned,
            2 primaries,
               3 R character (1),
               3 G character (1),
               3 B character (1);
   declare ch character (1);
   declare (i, j) fixed binary;
   declare out file record;

   open file (out) title ('/IMAGE.PPM,TYPE(FIXED),RECSIZE(1)' ) OUTPUT;

   ch = 'P'; write file (out) from (ch);
   ch = '6'; write file (out) from (ch);

   call put_integer (hbound(image, 1));
   call put_integer (hbound(image, 2));
   call put_integer (255);

   do i = 0 to hbound(image,1);
      do j = 0 to hbound(image, 2);
         color = image(i,j);
         write file (out) from (R);
         write file (out) from (G);
         write file (out) from (B);
      end;
   end;

put_integer: procedure (k);
   declare k fixed binary;
   declare s character (30) varying;
   declare i fixed binary;
   declare ch character (1);

   s = k;
   s = trim(s);
   do i = 1 to length(s);
      ch = substr(s, i, 1);
      write file (out) from (ch);
   end;
   ch = '09'x;
   write file (out) from (ch);
end put_integer;
end test;
```



## PicoLisp


```PicoLisp
(de ppmWrite (Ppm File)
   (out File
      (prinl "P6")
      (prinl (length (car Ppm)) " " (length Ppm))
      (prinl 255)
      (for Y Ppm (for X Y (apply wr X))) ) )
```



## PureBasic


```PureBasic
Procedure SaveImageAsPPM(Image, file$, Binary = 1)
  ; Author Roger Rösch (Nickname Macros)
  IDFiIe = CreateFile(#PB_Any, file$)
  If IDFiIe
    If StartDrawing(ImageOutput(Image))
      WriteStringN(IDFiIe, "P" + Str(3 + 3*Binary))
      WriteStringN(IDFiIe, "#Created with PureBasic using a Function created from Macros for Rosettacode.org ")
      width  = ImageWidth(Image)
      height = ImageHeight(Image)
      WriteStringN(IDFiIe, Str(width) + " " + Str(height))
      WriteStringN(IDFiIe, "255")
      If Binary = 0
        For y = 0 To height - 1
          For x = 0 To width - 1
            color = Point(x, y)
            WriteString(IDFiIe, Str(Red(color)) + " " + Str(Green(color)) + " " + Str(Blue(color)) + "  ")
          Next
          WriteStringN(IDFiIe, "")
        Next
      Else  ; Save in Binary Format
        For y = 0 To height - 1
          For x = 0 To width - 1
            color = Point(x, y)
            WriteByte(IDFiIe, Red(color))
            WriteByte(IDFiIe, Green(color))
            WriteByte(IDFiIe, Blue(color))
          Next
        Next
      EndIf
      StopDrawing()
    EndIf
    CloseFile(IDFiIe)
  EndIf
EndProcedure
```



## Python

{{works with|Python|3.1}}

Extending the example given [[Basic_bitmap_storage#Alternative_version|here]]

```python

# String masquerading as ppm file (version P3)
import io
ppmfileout = io.StringIO('')

def writeppmp3(self, f):
    self.writeppm(f, ppmformat='P3')

def writeppm(self, f, ppmformat='P6'):
    assert ppmformat in ['P3', 'P6'], 'Format wrong'
    magic = ppmformat + '\n'
    comment = '# generated from Bitmap.writeppm\n'
    maxval = max(max(max(bit) for bit in row) for row in self.map)
    assert ppmformat == 'P3' or 0 <= maxval < 256, 'R,G,B must fit in a byte'
    if ppmformat == 'P6':
        fwrite = lambda s: f.write(bytes(s, 'UTF-8'))
        maxval = 255
    else:
        fwrite = f.write
        numsize=len(str(maxval))
    fwrite(magic)
    fwrite(comment)
    fwrite('%i %i\n%i\n' % (self.width, self.height, maxval))
    for h in range(self.height-1, -1, -1):
        for w in range(self.width):
            r, g, b = self.get(w, h)
            if ppmformat == 'P3':
                fwrite('   %*i %*i %*i' % (numsize, r, numsize, g, numsize, b))
            else:
                fwrite('%c%c%c' % (r, g, b))
        if ppmformat == 'P3':
            fwrite('\n')

Bitmap.writeppmp3 = writeppmp3
Bitmap.writeppm = writeppm

# Draw something simple
bitmap = Bitmap(4, 4, black)
bitmap.fillrect(1, 0, 1, 2, white)
bitmap.set(3, 3, Colour(127, 0, 63))
# Write to the open 'file' handle
bitmap.writeppmp3(ppmfileout)
# Whats in the generated PPM file
print(ppmfileout.getvalue())

'''
The print statement above produces the following output :

P3
# generated from Bitmap.writeppmp3
4 4
255
     0   0   0     0   0   0     0   0   0   127   0  63
     0   0   0     0   0   0     0   0   0     0   0   0
     0   0   0   255 255 255     0   0   0     0   0   0
     0   0   0   255 255 255     0   0   0     0   0   0

'''

# Write a P6 file
ppmfileout = open('tmp.ppm', 'wb')
bitmap.writeppm(ppmfileout)
ppmfileout.close()

```



## R

{{libheader|pixmap}}

```r

# View the existing code in the library
library(pixmap)
pixmap::write.pnm

#Usage
write.pnm(theimage, filename)

```



## Racket


```racket

;P3
(define (bitmap->ppm bitmap output-port)
  (define height (send bitmap get-height))
  (define width (send bitmap get-width))
  (define buffer (make-bytes (* width height 4))) ;buffer for storing argb data
  (send bitmap get-argb-pixels 0 0 width height buffer) ;copy pixels
  (parameterize ([current-output-port output-port])
    (printf "P3\n~a ~a\n255" width height) ;header
    (for ([i (* width height)])
      (define pixel-position (* 4 i))
      (when (= (modulo i width) 0) (printf "\n")) ;end of row
      (printf "~s ~s ~s "
              (bytes-ref buffer (+ pixel-position 1)) ;r
              (bytes-ref buffer (+ pixel-position 2)) ;g
              (bytes-ref buffer (+ pixel-position 3)))))) ;b


(call-with-output-file "image.ppm" #:exists 'replace #:mode 'text
  (lambda (out)
    (bitmap->ppm bm out)))

; P6
(define (bitmap->ppm bitmap output-port)
  (define height (send bitmap get-height))
  (define width (send bitmap get-width))
  (define buffer (make-bytes (* width height 4))) ;buffer for storing argb data
  (send bitmap get-argb-pixels 0 0 width height buffer) ;copy pixels
  (parameterize ([current-output-port output-port])
    (printf "P6\n~a ~a\n255\n" width height) ;header
    (for ([i (* width height)])
      (define pixel-position (* 4 i))
      (write-byte (bytes-ref buffer (+ pixel-position 1))) ; r
      (write-byte (bytes-ref buffer (+ pixel-position 2))) ; g
      (write-byte (bytes-ref buffer (+ pixel-position 3)))))) ;b

(call-with-output-file "image.ppm" #:exists 'replace #:mode 'binary
  (lambda (out)
    (bitmap->ppm bm out)))

;or any other output port


```



## REXX


```rexx
/*REXX program writes a  PPM  formatted image file, also known as a  P6  (binary) file. */
green  = 00ff00                                  /*define a pixel with the color green. */
parse arg oFN width height color .               /*obtain optional arguments from the CL*/
if    oFN=='' |    oFN==","  then    oFN='IMAGE' /*Not specified?  Then use the default.*/
if  width=='' |  width==","  then  width=   20   /* "      "         "   "   "     "    */
if height=='' | height==","  then height=   20   /* "      "         "   "   "     "    */
if  color=='' |  color==","  then  color= green  /* "      "         "   "   "     "    */
oFID= oFN'.PPM'                                  /*define  oFID  by adding an extension.*/
 @. = x2c(color)                                 /*set all pixels of image a hex color. */
 $  = '9'x                                       /*define the separator (in the header).*/
 #  = 255                                        /*  "     "  max value for all colors. */
call charout oFID, ,  1                          /*set the position of the file's output*/
call charout oFID,'P6'width || $ || height || $ || # || $     /*write file header info. */
_=
       do j     =1  for width
            do k=1  for height;  _=_ || @.j.k    /*write the PPM file, 1 pixel at a time*/
            end   /*k*/                          /* ↑    a pixel contains three bytes,  */
       end        /*j*/                          /* └────which defines the pixel's color*/
call charout oFID, _                             /*write the image's raster to the file.*/
call charout oFID                                /*close the output file just to be safe*/
                                                 /*stick a fork in it,  we're all done. */
```






## Ruby

Extending [[Basic_bitmap_storage#Ruby]]

```ruby
class RGBColour
  def values
    [@red, @green, @blue]
  end
end

class Pixmap
  def save(filename)
    File.open(filename, 'w') do |f|
      f.puts "P6", "#{@width} #{@height}", "255"
      f.binmode
      @height.times do |y|
        @width.times do |x|
          f.print @data[x][y].values.pack('C3')
        end
      end
    end
  end
  alias_method :write, :save
end
```



## Rust


```rust
use std::path::Path;
use std::io::Write;
use std::fs::File;

pub struct RGB {
    r: u8,
    g: u8,
    b: u8,
}

pub struct PPM {
    height: u32,
    width: u32,
    data: Vec<u8>,
}

impl PPM {
    pub fn new(height: u32, width: u32) -> PPM {
        let size = 3 * height * width;
        let buffer = vec![0; size as usize];
        PPM { height: height, width: width, data: buffer }
    }

    fn buffer_size(&self) -> u32 {
        3 * self.height * self.width
    }

    fn get_offset(&self, x: u32, y: u32) -> Option<usize> {
        let offset = (y * self.width * 3) + (x * 3);
        if offset < self.buffer_size() {
            Some(offset as usize)
        } else {
            None
        }
    }

    pub fn get_pixel(&self, x: u32, y: u32) -> Option<RGB> {
        match self.get_offset(x, y) {
            Some(offset) => {
                let r = self.data[offset];
                let g = self.data[offset + 1];
                let b = self.data[offset + 2];
                Some(RGB {r: r, g: g, b: b})
            },
            None => None
        }
    }

    pub fn set_pixel(&mut self, x: u32, y: u32, color: RGB) -> bool {
        match self.get_offset(x, y) {
            Some(offset) => {
                self.data[offset] = color.r;
                self.data[offset + 1] = color.g;
                self.data[offset + 2] = color.b;
                true
            },
            None => false
        }
    }

    pub fn write_file(&self, filename: &str) -> std::io::Result<()> {
        let path = Path::new(filename);
        let mut file = try!(File::create(&path));
        let header = format!("P6 {} {} 255\n", self.width, self.height);
        try!(file.write(header.as_bytes()));
        try!(file.write(&self.data));
        Ok(())
    }
}
```



## Scala

Extends Pixmap class from task [[Read_ppm_file#Scala|Read PPM file]].

```scala
object Pixmap {
   def save(bm:RgbBitmap, filename:String)={
      val out=new DataOutputStream(new FileOutputStream(filename))

      out.writeBytes("P6\u000a%d %d\u000a%d\u000a".format(bm.width, bm.height, 255))

      for(y <- 0 until bm.height; x <- 0 until bm.width; c=bm.getPixel(x, y)){
         out.writeByte(c.getRed)
         out.writeByte(c.getGreen)
         out.writeByte(c.getBlue)
      }
   }
}
```



## Scheme

{{Works with|Scheme|R<math>^5</math>RS}}

```scheme
(define (write-ppm image file)
  (define (write-image image)
    (define (write-row row)
      (define (write-colour colour)
        (if (not (null? colour))
            (begin (write-char (integer->char (car colour)))
                   (write-colour (cdr colour)))))
      (if (not (null? row))
          (begin (write-colour (car row)) (write-row (cdr row)))))
    (if (not (null? image))
        (begin (write-row (car image)) (write-image (cdr image)))))
  (with-output-to-file file
    (lambda ()
      (begin (display "P6")
             (newline)
             (display (length (car image)))
             (display " ")
             (display (length image))
             (newline)
             (display 255)
             (newline)
             (write-image image)))))
```

Example using definitions in [[Basic bitmap storage#Scheme]]:

```scheme
(define image (make-image 800 600))
(image-fill! image *black*)
(image-set! image 400 300 *blue*)
(write-ppm image "out.ppm")
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "draw.s7i";
  include "color.s7i";

const proc: writePPM (in string: fileName, in PRIMITIVE_WINDOW: aWindow) is func
  local
    var file: ppmFile is STD_NULL;
    var integer: x is 0;
    var integer: y is 0;
    var color: pixColor is black;
  begin
    ppmFile := open(fileName, "w");
    if ppmFile <> STD_NULL then
      writeln(ppmFile, "P6");
      writeln(ppmFile, width(aWindow) <& " " <& height(aWindow));
      writeln(ppmFile, "255");
      for y range 0 to pred(height(aWindow)) do
        for x range 0 to pred(width(aWindow)) do
          pixColor := getPixelColor(aWindow, x, y);
          write(ppmFile, str(chr(pixColor.redLight)) <& chr(pixColor.greenLight) <& chr(pixColor.blueLight));
        end for;
      end for;
      close(ppmFile);
    end if;
  end func;
```



## Sidef

{{trans|Perl 6}}

```ruby
subset Int   < Number {|n| n.is_int  }
subset UInt  < Int    {|n| n >= 0    }
subset UInt8 < Int    {|n| n ~~ ^256 }

struct Pixel {
    R < UInt8,
    G < UInt8,
    B < UInt8
}

class Bitmap(width < UInt, height < UInt) {
    has data = []

    method fill(Pixel p) {
        data = (width*height -> of { Pixel(p.R, p.G, p.B) })
    }

    method setpixel(i < UInt, j < UInt, Pixel p) {

        subset WidthLimit  < UInt { |n| n ~~ ^width  }
        subset HeightLimit < UInt { |n| n ~~ ^height }

        func (w < WidthLimit, h < HeightLimit) {
            data[w*height + h] = p
        }(i, j)
    }

    method p6 {
        <<-EOT + data.map {|p| [p.R, p.G, p.B].pack('C3') }.join
        P6
        #{width} #{height}
        255
        EOT
    }
}

var b = Bitmap(width: 125, height: 125)

for i,j in (^b.height ~X ^b.width) {
    b.setpixel(i, j, Pixel(2*i, 2*j, 255 - 2*i))
}

%f"palette.ppm".write(b.p6, :raw)
```



## Tcl

{{libheader|Tk}}
Referring to [[Basic bitmap storage#Tcl]]:

```tcl
package require Tk

proc output_ppm {image filename} {
    $image write $filename -format ppm
}

set img [newImage 150 150]
fill $img red
setPixel $img green 40 40
output_ppm $img filename.ppm

# check the file format:
set fh [open filename.ppm]
puts [gets $fh] ;# ==> P6
puts [gets $fh] ;# ==> 150 150
puts [gets $fh] ;# ==> 255
binary scan [read $fh 3] c3 pixel
foreach colour $pixel {puts [expr {$colour & 0xff}]} ;# ==> 255 \n 0 \n 0 \n
close $fh
```



## UNIX Shell

{{works with|ksh93}}
Ref: [[Bitmap#UNIX Shell]]

Add the following function to the <tt>Bitmap_t</tt> type

```bash
    function write {
        _.to_s > "$1"
    }
```

Then you can:

```bash
Bitmap_t b
# do stuff to b, and save it:
b.write '$HOME/tmp/bitmap.ppm'
```



## Visual Basic .NET



```vbnet
Public Shared Sub SaveRasterBitmapToPpmFile(ByVal rasterBitmap As RasterBitmap, ByVal filepath As String)
   Dim header As String = String.Format("P6{0}{1}{2}{3}{0}255{0}", vbLf, rasterBitmap.Width, " "c, rasterBitmap.Height)
   Dim bufferSize As Integer = header.Length + (rasterBitmap.Width * rasterBitmap.Height * 3)
   Dim bytes(bufferSize - 1) As Byte
   Buffer.BlockCopy(Encoding.ASCII.GetBytes(header.ToString), 0, bytes, 0, header.Length)
   Dim index As Integer = header.Length
   For y As Integer = 0 To rasterBitmap.Height - 1
      For x As Integer = 0 To rasterBitmap.Width - 1
         Dim color As Rgb = rasterBitmap.GetPixel(x, y)
         bytes(index) = color.R
         bytes(index + 1) = color.G
         bytes(index + 2) = color.B
         index += 3
      Next
   Next
   My.Computer.FileSystem.WriteAllBytes(filepath, bytes, False)
End Sub
```



## Vedit macro language


This routine creates a RAW PPM file (binary).
Pixel data must be stored in edit buffer pointed by numeric register #10.
The data in the buffer is assumed to be in R,G,B order, which is the order used by PPM file.

```txt

/////////////////////////////////////////////////////////////////////
//
//  Save image as PPM file.
//  @10 = filename.  Buffer #10 contains the Pixel data.
//
:SAVE_PPM:
Buf_Switch(#10)
BOF
IT("P6") IN
Num_Ins(#11, LEFT)	  // width of image
Num_Ins(#12, LEFT)	  // height of image
Num_Ins(255, LEFT+NOCR)	  // maxval
IC(10)
File_Save_As(@10, OK)
Return

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
def Width=180, Height=135, Color=$123456;

proc WriteImage;                \Write screen image to a a PPM file
int  X, Y, C;
[Text(3,"P6 "); IntOut(3,Width); ChOut(3,^ ); IntOut(3,Height); Text(3," 255
");
for Y:= 0 to Height-1 do
    for X:= 0 to Width-1 do
        [C:= ReadPix(X, Y);
        ChOut(3, C>>16);
        ChOut(3, C>>8);
        ChOut(3, C);
        ];
];

proc OpenOutFile(FN);           \Open for output the named file
char FN;        \file name string
int  H;         \handle
[H:= FOpen(FN, 1);
FSet(H, ^o);                    \small buffer allows multiple files, and it is
OpenO(3);                       \ closed automatically when the program exits
];

proc MakeImage;                 \Make a bitmap image
int  X, Y;
[for Y:= 0 to Height-1 do       \fill area with Color
    for X:= 0 to Width-1 do
        Point(X, Y, Color);
Move(60, 60);  HexOut(6, ReadPix(0,0)); \show hex value of color of pixel at 0,0
];

[SetVid($112);  \set display for 640x480 graphics in 24-bit RGB color
MakeImage;
OpenOutFile("IMAGE.PPM");
WriteImage;
SetVid(3);                      \restore display to normal text mode
]
```



## Yabasic


```Yabasic
clear screen

wid = 150 : hei = 200
open window wid, hei
window origin "cc"
color 255, 0, 0
fill circle 0, 0, 50
color 0, 255, 0
fill circle 0, 0, 35
color 0, 0, 255
fill circle 0, 0, 20
window origin "lt"

header$ = "P6\n" + str$(wid) + " " + str$(hei) + "\n255\n"

fn = open("exmaple.PPM", "wb")

print #fn header$

for x = 0 to hei - 1
	for y = 0 to wid - 1
		c$ = right$(getbit$(y, x, y, x), 6)
		poke #fn, dec(left$(c$, 2))
		poke #fn, dec(right$(c$, 2))
		poke #fn, dec(mid$(c$, 3, 2))
	next y
next x

poke #fn, asc("\n")
close #fn
```



## zkl


```zkl
// convert Windows BMP (bit map) image to PPM

    // Read BMP file
bmp:=File.stdin.read().howza(0); // BMP to memory (byte bucket), treat as bytes
_assert_(bmp[0]==0x42,"Stdin not a BMP file");
width:=bmp.toLittleEndian(18,2,False); height:=bmp.toLittleEndian(22,2,False); // signed
println(width," x ",height);
bmp.del(0,14 + bmp.toLittleEndian(14,2));  // get rid of header

    // Write BMP to PPM image (in memory)
ppm:=Data(width*height*3 + 100);  // sized byte bucket plus some header slop
ppm.write("P6\n#rosettacode BMP to PPM test\n%d %d\n255\n".fmt(width,height));
foreach y in ([height - 1 .. 0,-1]){  // BGR 1 byte each, image is stored upside down
   bmp[y*width*3,width*3].pump(ppm,T(Void.Read,2),fcn(b,g,r){ return(r,g,b) });
}

File("foo.ppm","wb").write(ppm);  // File.stdout isn't binary, let GC close file
```

{{out}}

```txt

$ zkl bbb < lena.bmp
512 x 512
$ ls -l foo.ppm
-rw-r--r-- 1 craigd craigd 786476 Aug 30 01:31 foo.ppm

```


{{omit from|PARI/GP}}
