+++
title = "Bitmap"
description = ""
date = 2019-10-14T03:21:04Z
aliases = []
[extra]
id = 3212
task = """
  Show a basic storage type to handle a simple RGB raster graphics image
  and some primitive associated functions.
"""
[taxonomies]
categories = []
tags = ["graphics", "raster-graphics"]
languages = [
  "actionscript_3",
  "ada",
  "algol_68",
  "autohotkey",
  "axe",
  "bash",
  "basic256",
  "bbc_basic",
  "c",
  "csharp",
  "cpp",
  "clojure",
  "crystal",
  "d",
  "e",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "forth",
  "fsharp",
  "go",
  "haskell",
  "icon",
  "j",
  "java",
  "javascript",
  "julia",
  "konsolscript",
  "lingo",
  "lisp",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "matlab",
  "maxscript",
  "modula-3",
  "nim",
  "ocaml",
  "octave",
  "oxygenbasic",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "purebasic",
  "qbasic",
  "r",
  "racket",
  "rapidq",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "tcl",
  "visual_basic",
  "visual_basic_.net",
  "vedit_macro_language",
  "xpl0",
  "zkl",
]
+++


If possible provide a function to allocate an uninitialised image,
given its width and height, and provide 3 additional functions:

- one to fill an image with a plain RGB color,
- one to set a given pixel with a color,
- one to get the color of a pixel.

(If there are specificities about the storage or the allocation, explain those.)

These functions are used as a base for the
[tasks with the "raster-graphics" tag](/tags/raster-graphics)
and a basic output function to check the results
is available in the task [write ppm file](/tasks/write_ppm_file).''


## ActionScript

ActionScript 3 has a
[BitmapData class](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/display/BitmapData.html)
(in the <code>flash.display</code> package)
which can be used for storage and handling of bitmap images.
To display these images, the
[Bitmap](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/display/Bitmap.html)
class can be used.


```ActionScript3
// To import the BitmapData class:
import flash.display.BitmapData;

// Creates a new BitmapData object with a width of 500 pixels and a height of 300 pixels.
var bitmap:BitmapData = new BitmapData(500, 300);

// Create a BitmapData with transparency disallowed
var opaqueBitmap:BitmapData = new BitmapData(500, 300, false);

// Bitmap with initial fill color, as 0xAARRGGBB (default is white)
var redFilledBitmap:BitmapData = new BitmapData(400, 300, true, 0xFFFF0000);

// Get the color value of the pixel at point (200, 200)
bitmap.getPixel(200, 200)     // As 0xRRGGBB
bitmap.getPixel32(200, 200)   // As 0xAARRGGBB

// Set the color value of the pixel at point (300, 200) to blue
bitmap.setPixel(300, 200, 0x0000FF);       // As 0xRRGGBB
bitmap.setPixel32(300, 200, 0xFF0000FF);   // As 0xAARRGGBB

// Fill the bitmap with a given color (as 0xAARRGGBB) after construction
bitmap.fillRect(bitmap.rect, 0xFF44FF44);
```


## Ada

The package interface:

```ada
package Bitmap_Store is
   type Luminance is mod 2**8;
   type Pixel is record
      R, G, B : Luminance := Luminance'First;
   end record;
   Black : constant Pixel := (others => Luminance'First);
   White : constant Pixel := (others => Luminance'Last);
   type Image is array (Positive range <>, Positive range <>) of Pixel;

   procedure Fill (Picture : in out Image; Color : Pixel);

   procedure Print (Picture : Image);

   type Point is record
      X, Y : Positive;
   end record;
end Bitmap_Store;
```

The implementation of:

```ada
with Ada.Text_IO;  use Ada.Text_IO;

package body Bitmap_Store is

   procedure Fill (Picture : in out Image; Color : Pixel) is
   begin
      for p of Picture loop x:= Color;end loop;
   end Fill;

   procedure Print (Picture : Image) is
   begin
      for I in Picture'Range (1) loop
         for J in Picture'Range (2) loop
               Put (if Picture (I, J) = White then ' ' else 'H');
         end loop;
         New_Line;
      end loop;
   end Print;

end Bitmap_Store;
```

This can be used like:

```ada
use Bitmap_Store;  with Bitmap_Store;
   ...
   X : Image (1..64, 1..64);
begin
   Fill (X, (255, 255, 255));
   X (1, 2) := (R => 255, others => 0);
   X (3, 4) := X (1, 2);
```


## ALGOL 68

Translated from ada

Note: '''short''' and '''shorten''' need to be tuned (added or removed)
to match the underlying graphic hardware color depth.

Works with ALGOL 68|Revision 1 - one minor extension to language used - PRAGMA READ, similar to C's #include directive..
Works with ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.6 algol68g-2.6]..
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release
[1.8-8d](http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download) -
due to extensive use of '''format'''[ted] ''transput''.}}
'''File: prelude/Bitmap.a68'''

```algol68
# -*- coding: utf-8 -*- #

MODE PIXEL = STRUCT(#SHORT# BITS red,green,blue);
MODE POINT = STRUCT(INT x,y);

MODE IMAGE = [0,0]PIXEL; # instance attributes #

MODE CLASSIMAGE = STRUCT ( # class attributes #
  PIXEL black, red, green, blue, white,
  PROC (REF IMAGE)REF IMAGE init,
  PROC (REF IMAGE, PIXEL)VOID fill,
  PROC (REF IMAGE)VOID print,
# virtual: #
  REF PROC (REF IMAGE, POINT, POINT, PIXEL)VOID line,
  REF PROC (REF IMAGE, POINT, INT, PIXEL)VOID circle,
  REF PROC (REF IMAGE, POINT, POINT, POINT, POINT, PIXEL, UNION(INT, VOID))VOID cubic bezier
);

CLASSIMAGE class image = (
  # black = # (#SHORTEN# 16r00, #SHORTEN# 16r00, #SHORTEN# 16r00),
  # red   = # (#SHORTEN# 16rff, #SHORTEN# 16r00, #SHORTEN# 16r00),
  # green = # (#SHORTEN# 16r00, #SHORTEN# 16rff, #SHORTEN# 16r00),
  # blue  = # (#SHORTEN# 16r00, #SHORTEN# 16r00, #SHORTEN# 16rff),
  # white = # (#SHORTEN# 16rff, #SHORTEN# 16rff, #SHORTEN# 16rff),
  # PROC init = # (REF IMAGE self)REF IMAGE:
    BEGIN
      (fill OF class image)(self, black OF class image);
      self
    END,

  # PROC fill = # (REF IMAGE self, PIXEL color)VOID:
      FOR x FROM 1 LWB self TO 1 UPB self DO
        FOR y FROM 2 LWB self TO 2 UPB self DO
          self[x,y] := color
        OD
      OD,
  # PROC print = # (REF IMAGE self)VOID:
      printf(($n(UPB self)(3(16r2d))l$, self)),
# virtual: #
  # REF PROC line = # LOC PROC (REF IMAGE, POINT, POINT, PIXEL)VOID,
  # REF PROC circle = # LOC PROC (REF IMAGE, POINT, INT, PIXEL)VOID,
  # REF PROC cubic bezier = # LOC PROC (REF IMAGE, POINT, POINT, POINT, POINT, PIXEL, UNION(INT, VOID))VOID
);

OP CLASSOF = (IMAGE image)CLASSIMAGE: class image;
OP INIT = (REF IMAGE image)REF IMAGE: (init OF (CLASSOF image))(image);

SKIP
```

'''File: test/Bitmap.a68'''

```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

### The test program ###
PR READ "prelude/Bitmap.a68" PR;

test:(
   REF IMAGE x := INIT LOC[1:16, 1:16]PIXEL;
   (fill OF class image) (x, white OF class image);
   (print OF class image) (x)
)
```

Output: (A 16x16 white block)

```txt

ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

```


## AutoHotkey

Works with AutoHotkey_L.

```AutoHotkey
test:
blue := color(0,0,255)  ; rgb
cyan := color(0,255,255)
blue_square := Bitmap(10, 10, blue)
cyanppm := Bitmap(10, 10, cyan)
x := blue_square[4,4] ; get pixel(4,4)
msgbox % "blue: 4,4,R,G,B, RGB: " x.R ", " x.G ", " x.B ", " x.rgb()
blue_square[4,4] := cyan ; set pixel(4,4)
x := blue_square[4,4] ; get pixel(4,4)
blue_square.write("blue.ppm")
return

Bitmap(width = 1, height = 1, background = 0)
{
global black
black := color(0,0,0)
if !background
background := black

    static BitmapType
    if !BitmapType
        BitmapType
        := Object("fill", "Bitmap_Fill"
	         ,"write", "Bitmap_write_ppm3")

	img := Object("width", width
                ,"height", height
                , "base"    , BitmapType)

	img._SetCapacity(height) ; an array of rows
  img.fill(background)
Return img
}


Bitmap_Fill(bitmap, color)
{
r := color.r
g := color.g
b := color.b
  loop % bitmap.height
  {
    height := A_Index
    loop % bitmap.width
    {
      width := A_Index
      bitmap[height, width] := color(r, g, b)
    }
  }
 return bitmap
}

Bitmap_write_ppm3(bitmap, filename)
{
file := FileOpen(filename, 0x11) ; utf-8, write
file.seek(0,0)
file.write("P3`n"
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

Color(r, g, b)
{
    static ColorType
    if !ColorType
        ColorType
        := Object("rgb"   , "Color_rgb")

    return Object("r" , r, "g", g, "b", b
                , "base"    , ColorType)

 ; return Object("r" , r, "g", g, "b", b, "rgb", "Color_rgb")
}

Color_rgb(clr)
{
return clr.R << 16 | clr.G << 8 | clr.B
}
```


## Axe

All of the functions specified in the task are built in to Axe.
Note that bitmaps are always 96x64 black and white.
Thus, since each pixel takes 1 bit, a complete bitmap is 768 bytes.

Two bitmaps can be masked together to create 3- and 4-color grayscale.


```axe
Buff(768)→Pic1
Fill(Pic1,768,255)
Pxl-Off(45,30,Pic1)

.Display the bitmap to demonstrate
Copy(Pic1)
DispGraph
Pause 4500

Disp pxl-Test(50,50,Pic1)▶Dec,i
```



## BASIC256

[[Image:BASIC256_bitmap.png|right]]

```BASIC256
graphsize 30,30
call fill(rgb(255,0,0))
call setpixel(10,10,rgb(0,255,255))
print "pixel 10,10 is " + pixel(10,10)
print "pixel 20,20 is " + pixel(20,10)

imgsave "BASIC256_bitmap.png"
end

subroutine fill(c)
color c
rect 0,0,graphwidth, graphheight
end subroutine

subroutine setpixel(x,y,c)
color c
plot x,y
end subroutine
```

Output:

```txt
pixel 10,10 is 4278255615
pixel 20,20 is 4294901760
```


## BBC BASIC

Works with BBC BASIC for Windows.
BBC BASIC expects a bitmap always to be associated with a window;
for simplicity this code uses the main output window.

```bbcbasic
      Width% = 200
      Height% = 200

      REM Set window size:
      VDU 23,22,Width%;Height%;8,16,16,128

      REM Fill with an RGB color:
      PROCfill(100,150,200)

      REM Set a pixel:
      PROCsetpixel(100,100,255,255,0)

      REM Get a pixel:
      rgb% = FNgetpixel(100,100)
      PRINT RIGHT$("00000" + STR$~rgb%, 6)
      END

      DEF PROCfill(r%,g%,b%)
      COLOR 1,r%,g%,b%
      GCOL 1+128
      CLG
      ENDPROC

      DEF PROCsetpixel(x%,y%,r%,g%,b%)
      COLOR 1,r%,g%,b%
      GCOL 1
      LINE x%*2,y%*2,x%*2,y%*2
      ENDPROC

      DEF FNgetpixel(x%,y%)
      LOCAL col%
      col% = TINT(x%*2,y%*2)
      SWAP ?^col%,?(^col%+2)
      = col%
```



## C


Working excerpt from <tt>imglib.h</tt> usable as "interface"
(some includes are needed for other functions of the same category).
This code uses functions from category [[:Category:Raster graphics operations| Raster graphics operations]].
One must create files imglib.h and imglib.c using code from these pages.
Start from [[Bitmap| bitmap page]]


```c
#ifndef _IMGLIB_0
#define _IMGLIB_0

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <math.h>
#include <sys/queue.h>

typedef unsigned char color_component;
typedef color_component pixel[3];
typedef struct {
    unsigned int width;
    unsigned int height;
    pixel * buf;
} image_t;
typedef image_t * image;

image alloc_img(unsigned int width, unsigned int height);
void free_img(image);
void fill_img(image img,
        color_component r,
        color_component g,
        color_component b );
void put_pixel_unsafe(
       	image img,
        unsigned int x,
        unsigned int y,
        color_component r,
        color_component g,
        color_component b );
void put_pixel_clip(
       	image img,
        unsigned int x,
        unsigned int y,
        color_component r,
        color_component g,
        color_component b );
#define GET_PIXEL(IMG, X, Y) (IMG->buf[ ((Y) * IMG->width + (X)) ])
#endif
```


```c
image alloc_img(unsigned int width, unsigned int height)
{
    image img;
    img = malloc(sizeof(image_t));
    img->buf = malloc(width * height * sizeof(pixel));
    img->width = width;
    img->height = height;
    return img;
}

void free_img(image img)
{
    free(img->buf);
    free(img);
}

void fill_img(
        image img,
        color_component r,
        color_component g,
        color_component b )
{
    unsigned int i, n;
    n = img->width * img->height;
    for (i=0; i < n; ++i)
    {
        img->buf[i][0] = r;
        img->buf[i][1] = g;
        img->buf[i][2] = b;
    }
}

void put_pixel_unsafe(
       	image img,
        unsigned int x,
        unsigned int y,
        color_component r,
        color_component g,
        color_component b )
{
    unsigned int ofs;
    ofs = (y * img->width) + x;
    img->buf[ofs][0] = r;
    img->buf[ofs][1] = g;
    img->buf[ofs][2] = b;
}

void put_pixel_clip(
       	image img,
        unsigned int x,
        unsigned int y,
        color_component r,
        color_component g,
        color_component b )
{
    if (x < img->width && y < img->height)
      put_pixel_unsafe(img, x, y, r, g, b);
}
```


## C++

Works with C++98.
{{libheader|boost}}


```c++
#include <iostream

#include <boost/gil/gil_all.hpp>
int main()
{
    using namespace boost::gil;
    // create 30x40 image
    rgb8_image_t img(30, 40);

    // fill with red
    rgb8_pixel_t red(255, 0, 0);
    fill_pixels(view(img), red);

    // set pixel at 10x20 to blue
    rgb8_pixel_t blue(0, 0, 255);
    view(img)(10, 20) = blue;

    // read the value of pixel at 11x20
    rgb8_pixel_t px = const_view(img)(11, 20);
    std::cout << "the pixel at 11, 20 is " << (unsigned)px[0] << ':' << (unsigned)px[1] << ':' << (unsigned)px[2]  << '\n';
}
```

See also [[Basic bitmap storage/C++]]


## C \#

This implementation uses a multidemensional array to store the Color structure
(which stores the RGB values).
No exception catching for out-of-bounds errors if they occur,
but provides Height and Width properties so a program using it can avoid them.

```c#
public class Bitmap
{
    public struct Color
    {
        public byte Red { get; set; }
        public byte Blue { get; set; }
        public byte Green { get; set; }
    }
    Color[,] _imagemap;
    public int Width { get { return _imagemap.GetLength(0); } }
    public int Height { get { return _imagemap.GetLength(1); } }
    public Bitmap(int width, int height)
    {
        _imagemap = new Color[width, height];
    }
    public void Fill(Color color)
    {
        for (int y = 0; y < Height; y++)
            for (int x = 0; x < Width; x++)
            {
                _imagemap[x, y] = color;
            }
    }
    public Color GetPixel(int x, int y)
    {
        return _imagemap[x, y];
    }
    public void SetPixel(int x, int y, Color color)
    {
        _imagemap[x, y] = color;
    }
}
```


## Clojure

```Clojure
(import '[java.awt Color Graphics Image]
	'[java.awt.image BufferedImage])

(defn blank-bitmap [width height]
  (BufferedImage. width height BufferedImage/TYPE_3BYTE_BGR))

(defn fill [image color]
  (doto (.getGraphics image)
    (.setColor color)
    (.fillRect 0 0 (.getWidth image) (.getHeight image))))

(defn set-pixel [image x y color]
  (.setRGB image x y (.getRGB color)))

(defn get-pixel [image x y]
  (Color. (.getRGB image x y)))
```


## Common Lisp

```lisp
(defpackage #:rgb-pixel-buffer
  (:use #:common-lisp)
  (:export #:rgb-pixel-component #:rgb-pixel #:rgb-pixel-buffer
           #:+red+ #:+green+ #:+blue+ #:+black+ #:+white+
           #:make-rgb-pixel #:make-rgb-pixel-buffer #:rgb-pixel-buffer-width
           #:rgb-pixel-buffer-height #:rgb-pixel-red #:rgb-pixel-green
           #:rgb-pixel-blue #:fill-rgb-pixel-buffer))
```

```lisp
(in-package #:rgb-pixel-buffer)

(deftype rgb-pixel-component ()
  '(unsigned-byte 8))

(deftype rgb-pixel ()
  '(unsigned-byte 24))

(deftype rgb-pixel-buffer (&optional (width '*) (height '*))
  `(array rgb-pixel (,width ,height)))

(defconstant +black+ 0)
(defconstant +white+ #xFFFFFF)
(defconstant +red+ #xFF0000)
(defconstant +green+ #x00FF00)
(defconstant +blue+ #x0000FF)

(defun make-rgb-pixel (r g b)
  (declare (type rgb-pixel-component r g b))
  (logior (ash r 16) (ash g 8) b))

(defun rgb-pixel-red (rgb)
  (declare (type rgb-pixel rgb))
  (logand (ash rgb -16) #xFF))

(defun rgb-pixel-green (rgb)
  (declare (type rgb-pixel rgb))
  (logand (ash rgb -8) #xFF))

(defun rgb-pixel-blue (rgb)
  (declare (type rgb-pixel rgb))
  (logand rgb #xFF))

(defun make-rgb-pixel-buffer (width height &optional (initial-element +black+))
  (declare (type (integer 1) width height))
  (declare (type rgb-pixel initial-element))
  (make-array (list width height)
    :element-type 'rgb-pixel
    :initial-element initial-element))

(defun rgb-pixel-buffer-width (buffer)
  (first (array-dimensions buffer)))

(defun rgb-pixel-buffer-height (buffer)
  (second (array-dimensions buffer)))

(defun rgb-pixel (buffer x y)
  (declare (type rgb-pixel-buffer buffer))
  (declare (type (integer 0) x y))
  (aref buffer x y))

(defun (setf rgb-pixel) (value buffer x y)
  (declare (type rgb-pixel-buffer buffer))
  (declare (type rgb-pixel value))
  (declare (type (integer 0) x y))
  (setf (aref buffer x y) value))

(defun fill-rgb-pixel-buffer (buffer pixel)
  (declare (type rgb-pixel-buffer buffer))
  (declare (type rgb-pixel pixel))
  (let* ((dimensions (array-dimensions buffer))
	 (width (first dimensions))
	 (height (second dimensions)))
    (loop
       :for y :of-type fixnum :upfrom 0 :below height
       :do (loop
	      :for x :of-type fixnum :upfrom 0 :below width
	      :do (setf (rgb-pixel buffer x y) pixel)))
    buffer))
```

Example:

```lisp
(defvar *buffer* (make-rgb-pixel-buffer 10 10))
(fill-rgb-pixel-buffer *buffer* +white+)
(setf (rgb-pixel *buffer* 0 0) +red+)
(setf (rgb-pixel *buffer* 0 9) +red+)
(setf (rgb-pixel *buffer* 9 0) +red+)
(setf (rgb-pixel *buffer* 9 9) +red+)
```


## Crystal

```crystal
class RGBColor
  getter red, green, blue

  def initialize(@red = 0_u8, @green = 0_u8, @blue = 0_u8)
  end

  RED   = new(red: 255_u8)
  GREEN = new(green: 255_u8)
  BLUE  = new(blue: 255_u8)
  BLACK = new
  WHITE = new(255_u8, 255_u8, 255_u8)
end

class Pixmap
  getter width, height
  @data : Array(Array(RGBColor))

  def initialize(@width : Int32, @height : Int32)
    @data = Array.new(@width) { Array.new(@height, RGBColor::WHITE) }
  end

  def fill(color)
    @data.each &.fill(color)
  end

  def [](x, y)
    @data[x][y]
  end

  def []=(x, y, color)
    @data[x][y] = color
  end
end

bmap = Pixmap.new(5, 5)
pp bmap
```


## D

This code is a little complex
because many tasks use this module for various purposes.

```d
module bitmap;

import std.stdio, std.array, std.exception, std.string, std.conv,
       std.algorithm, std.ascii;

final class Image(T) {
    static if (is(typeof({ auto x = T.black; })))
        const static T black = T.black;
    else
        const static T black = T.init;
    static if (is(typeof({ auto x = T.white; })))
        const static T white = T.white;

    T[] image;
    private size_t nx_, ny_;

    this(in int nxx=0, in int nyy=0, in bool inizialize=true)
    pure nothrow {
        allocate(nxx, nyy, inizialize);
    }

    void allocate(in int nxx=0, in int nyy=0, in bool inizialize=true)
    pure nothrow @safe in {
        assert(nxx >= 0 && nyy >= 0);
    } body {
        this.nx_ = nxx;
        this.ny_ = nyy;
        if (nxx * nyy > 0) {
            if (inizialize)
                image.length = nxx * nyy;
            else // Optimization.
                image = minimallyInitializedArray!(typeof(image))
                                                  (nxx * nyy);
        }
    }

    @property Image dup() const pure nothrow @safe {
        auto result = new Image();
        result.image = this.image.dup;
        result.nx_ = this.nx;
        result.ny_ = this.ny;
        return result;
    }

    static Image fromData(T[] data, in size_t nxx=0, in size_t nyy=0)
    pure nothrow @safe in {
        assert(nxx >= 0 && nyy >= 0 && data.length == nxx * nyy);
    } body {
        auto result = new Image();
        result.image = data;
        result.nx_ = nxx;
        result.ny_ = nyy;
        return result;
    }

    @property size_t nx() const pure nothrow @safe @nogc { return nx_; }
    @property size_t ny() const pure nothrow @safe @nogc { return ny_; }

    ref T opIndex(in size_t x, in size_t y) pure nothrow @safe @nogc
    in {
        assert(x < nx_ && y < ny_);
        //assert(x < nx_, format("opIndex, x=%d, nx=%d", x, nx));
        //assert(y < ny_, format("opIndex, y=%d, ny=%d", y, ny));
    } body {
        return image[x + y * nx_];
    }

    T opIndex(in size_t x, in size_t y) const pure nothrow @safe @nogc
    in {
        assert(x < nx_ && y < ny_);
        //assert(x < nx_, format("opIndex, x=%d, nx=%d", x, nx));
        //assert(y < ny_, format("opIndex, y=%d, ny=%d", y, ny));
    } body {
        return image[x + y * nx_];
    }

    T opIndexAssign(in T color, in size_t x, in size_t y)
    pure nothrow @safe @nogc
    in {
        assert(x < nx_ && y < ny_);
        //assert(x < nx_, format("opIndex, x=%d, nx=%d", x, nx));
        //assert(y < ny_, format("opIndex, y=%d, ny=%d", y, ny));
    } body {
        return image[x + y * nx_] = color;
    }

    void opIndexUnary(string op)(in size_t x, in size_t y)
    pure nothrow @safe @nogc
    if (op == "++" || op == "--") in {
        assert(x < nx_ && y < ny_);
    } body {
        mixin("image[x + y * nx_] " ~ op ~ ";");
    }

    void clear(in T color=this.black) pure nothrow @safe @nogc {
        image[] = color;
    }

    /// Convert a 2D array of chars to a binary Image.
    static Image fromText(in string txt,
                          in char one='#', in char zero='.') pure {
        auto M = txt
                 .strip
                 .split
                 .map!(row => row
                              .filter!(c => c == one || c == zero)
                              .map!(c => T(c == one))
                              .array)
                 .array;
        assert(M.join.length > 0); // Not empty.
        foreach (row; M)
            assert(row.length == M[0].length); // Rectangular
        return Image.fromData(M.join, M[0].length, M.length);
    }

    /// The axis origin is at the top left.
    void textualShow(in char bl='#', in char wh='.') const nothrow {
        size_t i = 0;
        foreach (immutable y; 0 .. ny_) {
            foreach (immutable x; 0 .. nx_)
                putchar(image[i++] == black ? bl : wh);
            putchar('\n');
        }
    }
}


struct RGB {
    ubyte r, g, b;
    static immutable black = typeof(this)();
    static immutable white = typeof(this)(255, 255, 255);
}


Image!RGB loadPPM6(ref Image!RGB img, in string fileName) {
    if (img is null)
        img = new Image!RGB;
    auto f = File(fileName, "rb");
    enforce(f.readln.strip == "P6");
    string line;
    do {
        line = f.readln();
    } while (line.length && line[0] == '#'); // Skip comments.
    const size = line.split;
    enforce(size.length == 2);
    img.allocate(size[0].to!uint, size[1].to!uint);
    enforce(f.readln().strip() == "255");
    auto l = new ubyte[img.nx * 3];
    size_t i = 0;
    foreach (immutable y; 0 .. img.ny) {
        f.rawRead!ubyte(l);
        foreach (immutable x; 0 .. img.nx)
            img.image[i++] = RGB(l[x * 3], l[x * 3 + 1], l[x * 3 + 2]);
    }
    return img;
}


void savePPM6(in Image!RGB img, in string fileName)
in {
    assert(img !is null);
    assert(img.nx > 0 && img.nx > 0);
} body {
    auto f = File(fileName, "wb");
    f.writefln("P6\n%d %d\n255", img.nx, img.ny);
    size_t i = 0;
    foreach (immutable y; 0 .. img.ny)
        foreach (immutable x; 0 .. img.nx) {
            immutable p = img.image[i++];
            f.write(cast(char)p.r, cast(char)p.g, cast(char)p.b);
        }
}

version (bitmap_main) {
    void main() {
        auto img = new Image!RGB(30, 10);
        img[4, 5] = RGB.white;
        img.textualShow;
    }
}
```

Compiling it with `version=bitmap_main` prints:

```txt
##############################
##############################
##############################
##############################
##############################
####.#########################
##############################
##############################
##############################
##############################
```


## E

This example includes the [write ppm file](/tasks/write_ppm_file) code,
because it is most naturally written as a method on the image object.


```e
def makeFlexList := <elib:tables.makeFlexList

def format := <import:java.lang.makeString>.format

def CHANNELS := 3
def UByte := 0..255

def makeColor {
  to fromFloat(r, g, b) {
    return makeColor.fromByte((r * 255).round(),
                              (g * 255).round(),
                              (b * 255).round())
  }
  to fromByte(r :UByte, g :UByte, b :UByte) {
    def color {
      to __printOn(out) {
        out.print(format("%02x%02x%02x", [color.rb(), color.gb(), color.bb()]))
      }
      to rf() { return r / 255 }
      to gf() { return g / 255 }
      to bf() { return b / 255 }
      to rb() { return r }
      to gb() { return g }
      to bb() { return b }
    }
    return color
  }
}

/** Convert 0..255 into 0..127 -128..-1 */
def sign(v) {
  return v %% 256 - 2*(v & 128)
}

def makeImage(width, height) {
  # NOTE: The primary E implementation is in Java and Java's fixed-size integers only
  # come in signed varieties. Therefore, there is a little bit of extra arithmetic.
  #
  # In an ideal E implementation we would specify the type 0..255, but this is not
  # currently possible everywhere, or efficient.

  def storage := makeFlexList.fromType(<type:java.lang.Byte>, width * height * CHANNELS)
  storage.setSize(width * height * CHANNELS)

  def X := 0..!width
  def Y := 0..!height

  def flexImage {
    to __printOn(out) {
      for y in Y {
        out.print("[")
        for x in X {
          out.print(flexImage[x, y], " ")
        }
        out.println("]")
      }
    }
    to width() { return width }
    to height() { return height }
    to fill(color) {
      for x in X {
        for y in Y {
          flexImage[x, y] := color
        }
      }
    }
    to get(x :X, y :Y) {
      def base := (y * width + x) * CHANNELS
      return makeColor.fromByte(storage[base + 0] %% 256,
                                storage[base + 1] %% 256,
                                storage[base + 2] %% 256)
    }
    /** Provided to make [[Flood fill]] slightly less insanely slow. */
    to test(x :X, y :Y, c) {
      def base := (y * width + x) * CHANNELS
      return storage[base + 0] <=> sign(c.rb()) &&
             storage[base + 1] <=> sign(c.gb()) &&
             storage[base + 2] <=> sign(c.bb())
    }
    to put(x :X, y :Y, c) {
      def base := (y * width + x) * CHANNELS
      storage[base + 0] := sign(c.rb())
      storage[base + 1] := sign(c.gb())
      storage[base + 2] := sign(c.bb())
    }
    to writePPM(outputStream) {
      outputStream.write(`P6$\n$width $height$\n255$\n`.getBytes("US-ASCII"))
      outputStream.write(storage.getArray())
    }
    /** Used for [[Read ppm file]] */
    to replace(list :List) {
      require(list.size() == width * height * CHANNELS)
      storage(0) := list
    }
  }

  return flexImage
}
```


Examples/tests:


```e
? def i := makeImage(3, 3)
# value: [000000 000000 000000 ]
#        [000000 000000 000000 ]
#        [000000 000000 000000 ]
#

? i.fill(makeColor.fromFloat(1, 0, 0))
? i
# value: [ff0000 ff0000 ff0000 ]
#        [ff0000 ff0000 ff0000 ]
#        [ff0000 ff0000 ff0000 ]
#

? i[1, 1] := makeColor.fromFloat(0.5, 0.5, 0.5)
# value: 808080

? i
# value: [ff0000 ff0000 ff0000 ]
#        [ff0000 808080 ff0000 ]
#        [ff0000 ff0000 ff0000 ]
#

? i[0, 1]
# value: ff0000

? i[1, 1]
# value: 808080

? i.writePPM(<import:java.io.makeFileOutputStream>(<file:~/Desktop/Rosetta.ppm>))
```


## EchoLisp

```scheme
(lib 'plot)
(define width 600)
(define height 400)

(plot-size width height) ;; set image size

(define (blue x y) (rgb 0.0 0.0 1.0)) ;; a constant function
(plot-rgb blue 1 1) ;; blue everywhere

(lib 'types) ;; uint32 and uint8 vector types

;; bit-map pixel access
(define bitmap (pixels->uint32-vector)) ;; screen to vector of int32
    → 240000

(define (pix-at x y) (vector-ref bitmap (+ x (* y width))))
(rgb->list (pix-at 100 200)) → (0 0 255 255)  ;; rgb blue

;; writing to bitmap
(define (set-color-xy x y col) (vector-set! bitmap (+ x (* y width)) col))

(for* ((x 100)(y 200)) (set-color-xy x y (rgb 1 1 0))) ;; to bitmap
(vector->pixels bitmap) ;; bitmap to screen


;; bit-map color components (r g b a) = index (0 1 2 3) access
(define bitmap (pixels->uint8-clamped-vector)) ;; screen to vector of uint8
(vector-length bitmap)
    → 960000
(define (blue-at-xy x y) (vector-ref bitmap (+ x 3 (* y width)))) ;; 3 = blue component
(blue-at-xy 100 200)
    → 255
```


## Elixir

Translation of the erlang version of the code.

```elixir
defmodule RosBitmap do
  defrecord Bitmap, pixels: nil, shape: {0, 0}

  defp new(width, height, {:rgb, r, g, b}) do
    Bitmap[
      pixels: :array.new(width * height,
        {:default, <<r::size(8), g::size(8), b::size(8)>>}),
      shape: {width, height}]
  end

  def new(width, height), do: new(width, height, {:rgb, 0, 0, 0})

  def fill(Bitmap[shape: {width, height}], {:rgb, _r, _g, _b}=color) do
    new(width, height, color)
  end

  def set_pixel(Bitmap[pixels: pixels, shape: {width, _height}]=bitmap,
      {:at, x, y}, {:rgb, r, g, b}) do
    index = x + y * width
    bitmap.pixels(:array.set(index, <<r::size(8), g::size(8), b::size(8)>>, pixels))
  end

  def get_pixel(Bitmap[pixels: pixels, shape: {width, _height}], {:at, x, y}) do
    index = x + y * width
    <<r::size(8), g::size(8), b::size(8)>> = :array.get(index, pixels)
    {:rgb, r, g, b}
  end
end
```


## Erlang

Stores pixels as a 1d array and colors as binaries.

```erlang
-module(ros_bitmap).

-export([new/2, fill/2, set_pixel/3, get_pixel/2]).

-record(bitmap, {
    pixels = nil,
    shape = {0, 0}
  }).

new(Width, Height) ->
  #bitmap{pixels=array:new(Width * Height, {default, <<0:8, 0:8, 0:8>>}), shape={Width, Height}}.

fill(#bitmap{shape={Width, Height}}, {rgb, R, G, B}) ->
  #bitmap{
    pixels=array:new(Width * Height, {default, <<R:8, G:8, B:8>>}),
    shape={Width, Height}}.

set_pixel(#bitmap{pixels=Pixels, shape={Width, _Height}}=Bitmap, {at, X, Y}, {rgb, R, G, B}) ->
  Index = X + Y * Width,
  Bitmap#bitmap{pixels=array:set(Index, <<R:8, G:8, B:8>>, Pixels)}.

get_pixel(#bitmap{pixels=Pixels, shape={Width, _Height}}, {at, X, Y}) ->
  Index = X + Y * Width,
  <<R:8, G:8, B:8>> = array:get(Index, Pixels),
  {rgb, R, G, B}.
```


## Euphoria

```euphoria
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

-- Usage example:
sequence image
image = new_image(800,600,black)

-- Set pixel color:
image[400][300] = red

-- Get pixel color
atom color
color = image[400][300] -- Now color is #FF0000
```

?color -- Should print out 16711680


## F\#

FSharp can accomplish this task in several ways.
This version is purely functional.
The bitmap data structure does not mutate.
Set pixel, for example, simply transforms the input bitmap
into a new bitmap with that pixel set to the input color.
If you have Framework 4.5,
you can use `ImmutableArray` to force this immutability.

```fsharp

//pure functional version ... changing a pixel color provides a new Bitmap
type Color = {red: byte; green: byte; blue: byte}
type Point = {x:uint32; y:uint32}
type Bitmap = {color: Color array; maxX: uint32; maxY: uint32}

let colorBlack = {red = (byte) 0; green = (byte) 0; blue = (byte) 0}
let emptyBitmap = {color = Array.empty; maxX = (uint32) 0; maxY = (uint32) 0}
let bitmap (width: uint32) (height: uint32) =
    match width, height with
    | 0u,0u | 0u,_ | _, 0u -> emptyBitmap
    | _,_ -> {color = Array.create ((int) (width * height)) colorBlack;
            maxX = width;
            maxY = height}
let getPixel point bitmap =
    match bitmap.color with
    | c when c |> Array.isEmpty -> None
    | c when (uint32) c.Length <= (point.y * bitmap.maxY + point.x) -> None
    | c -> Some c.[(int) (point.y * bitmap.maxY + point.x)]
let setPixel point color bitmap =
    {bitmap with color = bitmap.color |> Array.mapi (function
                | i when i = (int) (point.y * bitmap.maxY + point.x) ->
                    (fun _ -> color)
                | _ -> id)}
let fill color bitmap = {bitmap with color = bitmap.color |> Array.map (fun _ ->color)}

```

'''Tests:'''

```fsharp
//setups
//==check pixel for color function
let check bitmap color (x,y) =
    match (getPixel {x=x;y=y} bitmap) with
    | Some(v) -> v = color
    | _ -> false
let allPixels i j = [for x in [0u..(i-1u)] do for y in [0u..(j-1u)] -> (x,y)]

//create new empty bitmap
let myBitmap = bitmap 0u 0u
printfn "Is empty: %b" (myBitmap = emptyBitmap)
let myBitmap2 = bitmap 1u 0u
printfn "Is empty: %b" (myBitmap2 = emptyBitmap)
let myBitmap3 = bitmap 0u 1u
printfn "Is empty: %b" (myBitmap3 = emptyBitmap)
//create normal bitmap
let myBitmap4 = bitmap 14u 14u
printfn "Is not empty: %b" (not (myBitmap4 = emptyBitmap))
//just check one color
printfn "Is 1,1 black: %b" (check myBitmap4 colorBlack (1u,1u))
//check out of range color
printfn "Is 100,100 nothing: %b" (not(check myBitmap4 colorBlack (100u,100u)))
//make sure all pixels are black
printfn "Is all black: %b" ((allPixels 14u 14u) |> List.forall (check myBitmap4 colorBlack))
//fill bitmap color
let colorWhite = {red = (byte) 255; green = (byte) 255; blue = (byte) 255}
let myBitmap5 = myBitmap4 |> fill colorWhite
printfn "Is all white: %b" ((allPixels 14u 14u) |> List.forall (check myBitmap5 colorWhite))
//change just one pixel
let myBitmap6 = myBitmap5 |> setPixel {x=5u;y=10u} colorBlack
printfn "Is 5,10 black: %b" (check myBitmap4 colorBlack (5u,10u))
```

Output:

```txt
Is empty: true

Is empty: true

Is empty: true

Is not empty: true

Is 1,1 black: true

Is 100,100 nothing: true

Is all black: true

Is all white: true

Is 5,10 black: true
```

'''Usage:'''

```fsharp
bitmap 14u 14u
|> fill {red = (byte) 200; green = (byte) 0; blue = (byte) 10}
|> setPixel {x=5u;y=10u} {red = (byte) 0; green = (byte) 0; blue = (byte) 0}
|> getPixel {x=5u;y=10u}
|> printfn "%A"
```

Output:

```txt
Some {red = 0uy;
      green = 0uy;
      blue = 0uy;}
```


## FBSL

Volatility in FBSL is a feature uncommon to most other languages.
It is the ability of its intrinsic functions as well as its user-defined functions,
DynAsm and DynC blocks, and functions imported from 3rd-party DLL's to preserve their return values between function calls in FBSL Variants that have the same names as their respective functions but use neither the parentheses nor the arguments.
These Variants belong to the global namespace and can be used throughout the entire script until another fully qualified function call to their respective functions is made,
whereby they change their values accordingly.
The feature minimizes the need for temporary variables and assignments.

This feature is a logical extension of VisualBasic way to formalize its function return value by assigning it to a Variant of the same name as that of the respective function.
However, the VB Variant is only effective within the scope of its own function.

'''Using pure FBSL's built-in graphics functions:'''

```qbasic
#DEFINE WM_LBUTTONDOWN 513
#DEFINE WM_RBUTTONDOWN 516
#DEFINE WM_CLOSE 16

FBSLSETFORMCOLOR(ME, RGB(0, 255, 255)) ' Cyan: set persistent background color
DRAWWIDTH(5) ' Adjust point size
FBSL.GETDC(ME) ' Use volatile FBSL.GETDC below to avoid extra assignments

RESIZE(ME, 0, 0, 300, 200)
CENTER(ME)
SHOW(ME)

BEGIN EVENTS
	SELECT CASE CBMSG
		CASE WM_LBUTTONDOWN ' Set color at current coords as hex literal
			PSET(FBSL.GETDC, LOWORD(CBLPARAM), HIWORD(CBLPARAM), &H0000FF) ' Red: Windows stores colors in BGR order
		CASE WM_RBUTTONDOWN ' Get color at current coords as hex literal
			FBSLSETTEXT(ME, "&H" & HEX(POINT(FBSL.GETDC, LOWORD(CBLPARAM), HIWORD(CBLPARAM))))
		CASE WM_CLOSE ' Clean up
			FBSL.RELEASEDC(ME, FBSL.GETDC)
	END SELECT
END EVENTS
```

Output: [[File:FBSL_RC_Bitmap.PNG]]


## Factor

The image is a matrix of triples {R,G,B}.
The various utilities could be defined in another file,
most of them are not used right now, but we need them for drawing
so I put every thing here..

```factor
USING: arrays fry kernel math.matrices sequences ;
IN: rosettacode.raster.storage

! Various utilities
: meach ( matrix quot -- ) [ each ] curry each ; inline
: meach-index ( matrix quot -- )
    [ swap 2array ] prepose
    [ curry each-index ] curry each-index ; inline
: mmap ( matrix quot -- matrix' ) [ map ] curry map ; inline
: mmap! ( matrix quot -- matrix' ) [ map! ] curry map! ; inline
: mmap-index ( matrix quot -- matrix' )
    [ swap 2array ] prepose
    [ curry map-index ] curry map-index ; inline

: matrix-dim ( matrix -- i j ) [ length ] [ first length ] bi ;
: set-Mi,j ( elt {i,j} matrix -- ) [ first2 swap ] dip nth set-nth ;
: Mi,j ( {i,j} matrix -- elt ) [ first2 swap ] dip nth nth ;

! The storage functions
: <raster-image> ( width height -- image )
    zero-matrix [ drop { 0 0 0 } ] mmap ;
: fill-image ( {R,G,B} image -- image )
    swap '[ drop _ ] mmap! ;
: set-pixel ( {R,G,B} {i,j} image -- ) set-Mi,j ; inline
: get-pixel ( {i,j} image -- pixel ) Mi,j ; inline
```



## Forth

This creates bitmaps on the heap (they may be deallocated with "FREE").
32-bit or greater cells are assumed, one pixel per cell.
This automatically word-aligns rows, so a separate ''stride'' field is not required.

```forth
hex
0000ff constant red
00ff00 constant green
ff0000 constant blue
decimal

1 cells constant pixel
: pixels cells ;

: bdim ( bmp -- w h ) 2@ ;
: bheight ( bmp -- h ) @ ;
: bwidth ( bmp -- w ) bdim drop ;
: bdata ( bmp -- addr ) 2 cells + ;

: bitmap ( w h -- bmp )
  2dup * pixels bdata allocate throw
  dup >r 2! r> ;

: bfill ( pixel bmp -- )
  dup bdata swap bdim * pixels
  bounds do
    dup i !
  pixel +loop
  drop ;

: bxy ( x y bmp -- addr )
  dup >r bwidth * + pixels r> bdata + ;

: b@ ( x y bmp -- pixel ) bxy @ ;
: b! ( pixel x y bmp -- ) bxy ! ;

: bshow ( bmp -- )
  hex
  dup bdim
  0 do cr
    dup 0 do
      over i j rot b@ if [char] * else bl then emit  \ 7 u.r
    loop
  loop
  2drop decimal ;

4 3 bitmap value test
red test bfill
test bshow cr
```


## Fortran

See [[Basic bitmap storage/Fortran]]


## Go

### Standard library

Go's standard library include image, color, and drawing packages and the source for them is easy to read.
There is also a [http://blog.golang.org/go-image-package Go Blog article on the image package]
The [https://golang.org/pkg/image/#NRGBA <code>image.NRGBA</code>] type supports everything this task requires (the 'A' is for alpha channel, each are 8 bits, if 16 bits each of RGBA is desired there is also the <code>image.NRGBA64</code> type). The 'N' of NRGBA stands for Non-alpha-premultiplied, color values can trivially be converted to/from alpha-premultiplied RGBA values via a <code>color.Model</code>.

Here's how to use the standard packages to do what this task requires:

```go
package main

import (
	"bytes"
	"fmt"
	"image"
	"image/color"
	"image/draw"
	"image/png"
)

func main() {
	// A rectangle from 0,0 to 300,240.
	r := image.Rect(0, 0, 300, 240)

	// Create an image
	im := image.NewNRGBA(r)

	// set some color variables for convience
	var (
		red  = color.RGBA{0xff, 0x00, 0x00, 0xff}
		blue = color.RGBA{0x00, 0x00, 0xff, 0xff}
	)

	// Fill with a uniform color
	draw.Draw(im, r, &image.Uniform{red}, image.ZP, draw.Src)

	// Set individual pixels
	im.Set(10, 20, blue)
	im.Set(20, 30, color.Black)
	im.Set(30, 40, color.RGBA{0x10, 0x20, 0x30, 0xff})

	// Get the values of specific pixels as color.Color types.
	// The color will be in the color.Model of the image (in this
	// case color.NRGBA) but color models can convert their values
	// to other models.
	c1 := im.At(0, 0)
	c2 := im.At(10, 20)

	// or directly as RGB components (scaled values)
	redc, greenc, bluec, _ := c1.RGBA()
	redc, greenc, bluec, _ = im.At(30, 40).RGBA()

	// Images can be read and writen in various formats
	var buf bytes.Buffer
	err := png.Encode(&buf, im)
	if err != nil {
		fmt.Println(err)
	}

	fmt.Println("Image size:", im.Bounds().Dx(), "×", im.Bounds().Dy())
	fmt.Println(buf.Len(), "bytes when encoded as PNG.")
	fmt.Printf("Pixel at %7v is %v\n", image.Pt(0, 0), c1)
	fmt.Printf("Pixel at %7v is %#v\n", image.Pt(10, 20), c2) // %#v shows type details
	fmt.Printf("Pixel at %7v has R=%d, G=%d, B=%d\n",
		image.Pt(30, 40), redc, greenc, bluec)
}
```

Output:

```txt

Image size: 300 × 240
786 bytes when encoded as PNG.
Pixel at   (0,0) is {255 0 0 255}
Pixel at (10,20) is color.NRGBA{R:0x0, G:0x0, B:0xff, A:0xff}
Pixel at (30,40) has R=4112, G=8224, B=12336

```


### DIY

Not a complete working program.
Presented here are just types and functions requested by the task.

```go
// Raster package used with a number of RC tasks.
//
// For each task, documentation in package main source will list this
// file and others that are necessary to build a raster package with
// sufficient functionality for the task.  To build a working program,
// build a raster package from the files listed, install the package,
// and then compile and link the package main that completes the task.
//
// Alternatively, files in the raster package can be combined as desired
// to build a package that meets the needs of multiple tasks.
package raster

// Rgb is a 24 bit color value represented with a 32 bit int
// in the conventional way.  This is expected to be convenient
// for the programmer in many cases.
type Rgb int32

// Pixel has r, g, and b as separate fields.  This is used as
// the in-memory representation of a bitmap.
type Pixel struct {
    R, G, B byte
}

// Pixel returns a new Pixel from a Rgb value
func (c Rgb) Pixel() Pixel {
    return Pixel{R: byte(c >> 16), G: byte(c >> 8), B: byte(c)}
}

// Rgb returns a single Rgb value computed from rgb fields of a Pixel
// of a Pixel.
func (p Pixel) Rgb() Rgb {
    return Rgb(p.R)<<16 | Rgb(p.G)<<8 | Rgb(p.B)
}

// Bitmap is the in-memory representation, or image storage type of a bitmap.
// Zero value for type is a valid zero-size bitmap.
// The only exported field is Comments.  Remaining fields have interdepencies
// that are managed by package code and so should not be directly accessed
// from outside the package.
type Bitmap struct {
    Comments   []string
    rows, cols int
    px         []Pixel   // all pixels as a single slice, row major order
    pxRow      [][]Pixel // rows of pixels as slices of px
}

const creator = "# Creator: RosettaGit https://www.rosettagit.org"

// New is a Bitmap "constructor."  Parameters x and y are extents.
// That is, the new bitmap will have x columns and y rows.
func NewBitmap(x, y int) (b *Bitmap) {
    b = &Bitmap{
        Comments: []string{creator},
        rows:     y, // named fields here to prevent possible mix-ups.
        cols:     x,
        px:       make([]Pixel, x*y),
        pxRow:    make([][]Pixel, y),
    }
    // Note rows of pixels are not allocated separately.
    // Rather the whole bitmap is allocted in one chunk as px.
    // This simplifies allocation and maintains locality.
    x0, x1 := 0, x
    for i := range b.pxRow {
        b.pxRow[i] = b.px[x0:x1] // slice operation. does no allocation.
        x0, x1 = x1, x1+x
    }
    return b
}

// Extent returns bitmap dimensions.
func (b *Bitmap) Extent() (cols, rows int) {
    return b.cols, b.rows
}

// Fill entire bitmap with solid color.
func (b *Bitmap) Fill(p Pixel) {
    for i := range b.px {
        b.px[i] = p
    }
}

func (b *Bitmap) FillRgb(c Rgb) {
    b.Fill(c.Pixel())
}

// Set a single pixel color value.
// Clips to bitmap boundaries.
// Returns true if pixel was set, false if clipped.
func (b *Bitmap) SetPx(x, y int, p Pixel) bool {
    defer func() { recover() }()
    b.pxRow[y][x] = p
    return true
}

func (b *Bitmap) SetPxRgb(x, y int, c Rgb) bool {
    return b.SetPx(x, y, c.Pixel())
}

// Note:  Clipping to bitmap boundaries is needed for program correctness
// but is otherwise not required by the task.  It is implemented with the
// combination of pxRow and the deferred recover.  SetPx, GetPx return the
// clipping result as a way for higher level graphics functions to track
// plotting and clipping status.  As this is not required by tasks though,
// it is generally not implemented.

// Get a single pixel color value.
// Returns pixel and ok=true if coordinates are within bitmap boundaries.
// Returns ok=false if coordinates are outside bitmap boundaries.
func (b *Bitmap) GetPx(x, y int) (p Pixel, ok bool) {
    defer func() { recover() }()
    return b.pxRow[y][x], true
}

func (b *Bitmap) GetPxRgb(x, y int) (Rgb, bool) {
    p, ok := b.GetPx(x, y)
    if !ok {
        return 0, false
    }
    return p.Rgb(), true
}
```


## Haskell

We implement the `Image` type as an `STArray`
so that we can use it in an imperative fashion in the `ST` monad.

```haskell
module Bitmap(module Bitmap) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

newtype Pixel = Pixel (Int, Int) deriving Eq

instance Ord Pixel where
    compare (Pixel (x1, y1)) (Pixel (x2, y2)) =
        case compare y1 y2 of
            EQ -> compare x1 x2
            v  -> v

instance Ix Pixel where
{- This instance differs from the one for (Int, Int) in that
the ordering of indices is
  (0,0), (1,0), (2,0), (0,1), (1,1), (2,1)
instead of
  (0,0), (0,1), (1,0), (1,1), (2,0), (2,1). -}
    range (Pixel (xa, ya), Pixel (xz, yz)) =
        [Pixel (x, y) | y <- [ya .. yz], x <- [xa .. xz]]
    index (Pixel (xa, ya), Pixel (xz, _)) (Pixel (xi, yi)) =
        (yi - ya)*(xz - xa + 1) + (xi - xa)
    inRange (Pixel (xa, ya), Pixel (xz, yz)) (Pixel (xi, yi)) =
        not $ xi < xa || xi > xz || yi < ya || yi > yz
    rangeSize (Pixel (xa, ya), Pixel (xz, yz)) =
        (xz - xa + 1) * (yz - ya + 1)

instance Show Pixel where
    show (Pixel p) = show p

class Ord c => Color c where
    luminance :: c -> Int
     -- The Int should be in the range [0 .. 255].
    black, white :: c
    toNetpbm :: [c] -> String
    fromNetpbm :: [Int] -> [c]
    netpbmMagicNumber, netpbmMaxval :: c -> String
      {- The argument to these two functions is ignored; the
      parameter is only for typechecking. -}

newtype Color c => Image s c = Image (STArray s Pixel c)

image :: Color c => Int -> Int -> c -> ST s (Image s c)
{- Creates a new image with the given width and height, filled
with the given color. -}
image w h = liftM Image .
    newArray (Pixel (0, 0), Pixel (w - 1, h - 1))

listImage :: Color c => Int -> Int -> [c] -> ST s (Image s c)
{- Creates a new image with the given width and height, with
each pixel set to the corresponding element of the given list. -}
listImage w h = liftM Image .
    newListArray (Pixel (0, 0), Pixel (w - 1, h - 1))

dimensions :: Color c => Image s c -> ST s (Int, Int)
dimensions (Image i) = do
    (_, Pixel (x, y)) <- getBounds i
    return (x + 1, y + 1)

getPix :: Color c => Image s c -> Pixel -> ST s c
getPix (Image i) = readArray i

getPixels :: Color c => Image s c -> ST s [c]
getPixels (Image i) = getElems i

setPix :: Color c => Image s c -> Pixel -> c -> ST s ()
setPix (Image i) = writeArray i

fill :: Color c => Image s c -> c -> ST s ()
fill (Image i) c = getBounds i >>= mapM_ f . range
  where f p = writeArray i p c

mapImage :: (Color c, Color c') =>
    (c -> c') -> Image s c -> ST s (Image s c')
mapImage f (Image i) = liftM Image $ mapArray f i
```

This module provides an instance of `Color`.

```haskell
module Bitmap.RGB(module Bitmap.RGB) where

import Bitmap
import Control.Monad.ST

newtype RGB = RGB (Int, Int, Int) deriving (Eq, Ord)

instance Color RGB where
    luminance (RGB (r, g, b)) = round x
      where x = 0.2126*r' + 0.7152*g' + 0.0722*b'
            (r', g', b') = (toEnum r, toEnum g, toEnum b)
    black = RGB (0,   0,   0)
    white = RGB (255, 255, 255)
    toNetpbm = concatMap f
      where f (RGB (r, g, b)) = [toEnum r, toEnum g, toEnum b]
    fromNetpbm [] = []
    fromNetpbm (r : g : b : rest) = RGB (r, g, b) : fromNetpbm rest
    netpbmMagicNumber _ = "P6"
    netpbmMaxval _ = "255"

toRGBImage :: Color c => Image s c -> ST s (Image s RGB)
toRGBImage = mapImage $ f . luminance
  where f x = RGB (x, x, x)
```



## Icon and Unicon

The language has a built-in window data type with associated graphics primitives.
A bitmap is just a window that isn't visible on-screen at the moment.

```Icon
procedure makebitmap(width,height)
   return open("bitmap", "g", "canvas=hidden",
             "size="||width||","||height)
end
procedure fillimage(w,color)
   Fg(w,color)
   FillRectangle(w)
end
procedure setpixel(w,x,y,color)
   Fg(w,color)
   DrawPixel(x,y)
end
procedure getpixel(w,x,y)
   return Pixel(w,x,y)
end
```


## J

A number of addon packages are available for J that work with common image formats (including PPM),
but here we will show a basic bitmap storage type as per the task description.

The structure is a 3-dimensional array of numbers.
The shape of the array is height by width by 3.
Each 1-dimensional cell of size 3 contains R, G and B numbers, in that order.
Indexing is zero based. (We could instead have encoded RGB in a single integer...)

No parameter validity checks are currently implemented.

In J, allocating an uninitialized image would not normally be separated from creating
the colored image, so `makeRGB` allows the specification of color during allocation.
As a monad, `makeRGB` creates a black image with the specified height and width.
It can also take a left argument (dyadic form) specifying the color(s) of the image.
`fillRGB` requires a left argument specifying the color(s),
but takes a bitmap (RGB) structure as the right argument.

'''Solution:'''

```j
makeRGB=: 0&$: : (($,)~ ,&3)
fillRGB=: makeRGB }:@$
setPixels=: (1&{::@[)`(<"1@(0&{::@[))`]}
getPixels=: <"1@[ { ]
```

'''Examples:'''

```j
   myimg=: makeRGB 5 8               NB. create a bitmap with height 5 and width 8 (black)
   myimg=: 255 makeRGB 5 8           NB. create a white bitmap with height 5 and width 8
   myimg=: 127 makeRGB 5 8           NB. create a gray bitmap with height 5 and width 8
   myimg=: 0 255 0 makeRGB 5 8       NB. create a green bitmap with height 5 and width 8
   myimg=: 0 0 255 fillRGB myimg     NB. fill myimg with blue
   colors=: 0 255 {~ #: i.8          NB. black,blue,green,cyan,red,magenta,yellow,white
   myimg=: colors fillRGB myimg      NB. fill myimg with vertical stripes of colors
   2 4 getPixels myimg               NB. get the pixel color from point (2, 4)
255 0 0

   myimg=: (2 4 ; 255 255 255) setPixels myimg   NB. set pixel at point (2, 4) to white
   2 4 getPixels myimg               NB. get the pixel color from point (2, 4)
255 255 255

   }:$ myimg                         NB. get height and width of the image
5 8
```

`getPixels` and `setPixels` are generalized to set and get lists/arrays of pixels.


```j
pixellist=: ,"0/~ i. 10  NB. row and column indices for 10 by 10 block of pixels

NB. create 10 by 10 block of magenta pixels in the middle of a 300 by 300 green image
myimg=: ((145 + pixellist) ; 255 0 255) setPixels 0 255 0 makeRGB 300 300

NB. get pixel color for 10x10 block offset from magenta block
subimg=: (140 + pixellist) getPixels myimg
```


To display the image in a window at any point for verification:


```j
require 'viewmat'
viewRGB=: [: viewrgb 256&#.

viewRGB myimg
```


Note that height comes before width here.
This is inconsistent with marketing of display resolutions,
but matches J's treatment of dimensions.


## Java

Solution AWT

```java
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;

public class BasicBitmapStorage {

    private final BufferedImage image;

    public BasicBitmapStorage(int width, int height) {
        image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    }

    public void fill(Color c) {
        Graphics g = image.getGraphics();
        g.setColor(c);
        g.fillRect(0, 0, image.getWidth(), image.getHeight());
    }

    public void setPixel(int x, int y, Color c) {
        image.setRGB(x, y, c.getRGB());
    }

    public Color getPixel(int x, int y) {
        return new Color(image.getRGB(x, y));
    }

    public Image getImage() {
        return image;
    }
}
```

Test Program JUnit

```java
import static org.junit.Assert.assertEquals;

import java.awt.Color;
import org.junit.Test;

public class BasicBitmapStorageTest {

    @Test
    public void testHappy() {
        int width = 640;
        int height = 480;

        BasicBitmapStorage bbs = new BasicBitmapStorage(width, height);
        bbs.fill(Color.CYAN);
        bbs.setPixel(width / 2, height / 2, Color.BLACK);
        Color c1 = bbs.getPixel(width / 2, height / 2);
        Color c2 = bbs.getPixel(20, 20);

        assertEquals(Color.BLACK, c1);
        assertEquals(Color.CYAN, c2);
    }
}
```


## JavaScript

JavaScript can interact with a drawing context using the HTML5 Canvas API.


```javascript
// Set up the canvas
var canvas = document.createElement("canvas"),
    ctx = canvas.getContext("2d"),
    width = 400, height = 400;

ctx.canvas.width = width;
ctx.canvas.height = height;

// Optionaly add it to the current page
document.body.appendChild(canvas);

// Draw an image
var img = document.createElement("img");
img.onload = function(){
    // Draw the element into the top-left of the canvas
    ctx.drawImage(img, 0, 0);
};
img.src = "//placehold.it/400x400";

// Fill the canvas with a solid blue color
ctx.fillStyle = "blue";
ctx.fillRect(0, 0, width, height);

// Place a black pixel in the middle
// Note that a pixel is a 1 by 1 rectangle
// This is the fastest method as of 2012 benchmarks
ctx.fillStyle = "black";
ctx.fillRect(width / 2, height / 2, 1, 1);
```


## Julia

Works with Julia 0.6.

'''Using packages'''
([https://github.com/JuliaImages/Images.jl Images.jl],
[https://github.com/JuliaGraphics/Colors.jl Colors.jl]):

```julia
using Images, Colors

Base.hex(p::RGB{T}) where T = join(hex(c(p), 2) for c in (red, green, blue))
function showhex(m::Matrix{RGB{T}}, pad::Integer=4) where T
    for r in 1:size(m, 1)
        println(" " ^ pad, join(hex.(m[r, :]), " "))
    end
end

w, h = 5, 7
cback = RGB(1, 0, 1)
cfore = RGB(0, 1, 0)

img = Array{RGB{N0f8}}(h, w);
println("Uninitialized image:")
showhex(img)

fill!(img, cback)
println("\nImage filled with background color:")
showhex(img)

img[2, 3] = cfore
println("\nImage with a pixel set for foreground color:")
showhex(img)
```

Output:

```txt
Uninitialized image:
    10DFF8 7F0000 F84A00 0030DA 007F00
    4A007F B0DDF8 7F0000 F84A00 00D0DB
    0000F0 4A007F D0D9F8 7F0000 F84A00
    DFF84A 000050 4A007F 50DAF8 7F0000
    007F00 D9F84A 000010 4A007F 30DCF8
    0050E0 007F00 DAF84A 000050 4A007F
    F84A00 00B0D9 007F00 DBF84A 000050

Image filled with background color:
    FF00FF FF00FF FF00FF FF00FF FF00FF
    FF00FF FF00FF FF00FF FF00FF FF00FF
    FF00FF FF00FF FF00FF FF00FF FF00FF
    FF00FF FF00FF FF00FF FF00FF FF00FF
    FF00FF FF00FF FF00FF FF00FF FF00FF
    FF00FF FF00FF FF00FF FF00FF FF00FF
    FF00FF FF00FF FF00FF FF00FF FF00FF

Image with a pixel set for foreground color:
    FF00FF FF00FF FF00FF FF00FF FF00FF
    FF00FF FF00FF 00FF00 FF00FF FF00FF
    FF00FF FF00FF FF00FF FF00FF FF00FF
    FF00FF FF00FF FF00FF FF00FF FF00FF
    FF00FF FF00FF FF00FF FF00FF FF00FF
    FF00FF FF00FF FF00FF FF00FF FF00FF
    FF00FF FF00FF FF00FF FF00FF FF00FF
```


## Kotlin

Translated from Java

```scala
// version 1.1.4-3

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
    val width = 640
    val height = 480
    val bbs = BasicBitmapStorage(width, height)
    with (bbs) {
        fill(Color.cyan)
        setPixel(width / 2, height / 2, Color.black)
        val c1 = getPixel(width / 2, height / 2)
        val c2 = getPixel(20, 20)
        print("The color of the pixel at (${width / 2}, ${height / 2}) is ")
        println(if (c1 == Color.black) "black" else "unknown")
        print("The color of the pixel at (120, 120) is ")
        println(if (c2 == Color.cyan) "cyan" else "unknown")
    }
}
```

Output:

```txt
The color of the pixel at (320, 240) is black
The color of the pixel at (120, 120) is cyan
```


## KonsolScript

```KonsolScript
function main() {
  Var:Number shape;

  Image:New(50, 50, shape)
  Draw:RectFill(0, 0, 50, 50, 0xFF0000, shape)  //one to fill an image with a plain RED color

  Draw:Pixel(30, 30, 0x0000FF, shape)           //set a given pixel at (30,30) with a BLUE color

  while (B1 == false) {
    Image:Blit(10, 10, shape, screen)
    Screen:Render()
  }
}
```


## Lingo

```lingo
-- Creates a new image object of size 640x480 pixel and 32-bit color depth
img = image(640, 480, 32)

-- Fills image with plain red
img.fill(img.rect, rgb(255,0,0))

-- Gets the color value of the pixel at point (320, 240)
col = img.getPixel(320, 240)

-- Changes the color of the pixel at point (320, 240) to black
img.setPixel(320, 240, rgb(0,0,0))
```


## Lua

```lua
function Allocate_Bitmap( width, height )
    local bitmap = {}
    for i = 1, width do
        bitmap[i] = {}
        for j = 1, height do
            bitmap[i][j] = {}
        end
    end
    return bitmap
end

function Fill_Bitmap( bitmap, color )
    for i = 1, #bitmap do
        for j = 1, #bitmap[1] do
            bitmap[i][j] = color
        end
    end
end

function Get_Pixel( bitmap, x, y )
    return bitmap[x][y]
end
```

This can be used like:

```lua
bitmap = Allocate_Bitmap( 100, 50 )
Fill_Bitmap( bitmap, { 15, 200, 80 } )
pixel = Get_Pixel( bitmap, 20, 25 )
print( pixel[1], pixel[2], pixel[3] )
```


## M2000 Interpreter

The easy way is to make a function to return an object with all functions on it,
for specific image.
We have to make the image in a way to render it to screen.
The render statement get data in a string using a header of 12 characters (24 bytes).
Raster lines are in down-top order. So last raster line is the top one.
Also RGB is BGR in this data structure. Raster lines has to be aligned proper,
so we may have add some bytes.

There are four functions (lambda functions) in the returned group,
each of them has closures, an one of that closure is a pointer to a buffer object.
We use this object to set and get pixels.

First two functions are for set and get pixel. Third return image as a string.
Forth function copy an image as string to buffer,
if they have the same width and height (else we get error)


### P3 ppm

```M2000 Interpreter
\ Bitmap width in pixels, height in pixels
\ Return a group object with some lambda as members: SetPixel, GetPixel, Image$
\ copyimage
\ using Copy x, y Use Image$ we can display image$ to x, y  as twips
\ we can use  x*twipsx, y*twipsy  for x,y as  pixels
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
      Group Bitmap {
            SetPixel=SetPixel
            GetPixel=GetPixel
            Image$=StrDib$
            Copy=CopyImage
      }
      =Bitmap
}
A=Bitmap(100,100)
Call A.SetPixel(50,50, color(128,0,255))
Print A.GetPixel(50,50)=color(128,0,255)
\\ display image to screen at 100, 50 pixel
copy 100*twipsx,50*twipsy use A.Image$()
A1=Bitmap(100,100)
Call A1.copy(A.Image$())
copy 500*twipsx,50*twipsy use A1.Image$()
```


### P6 ppm

Need Version 9.4, Rev >=19

```M2000 Interpreter
Module  P6 {
      Function Bitmap  {
            def x as long, y as long, Import as boolean

            If match("NN") then {
                 Read x, y
            } else.if Match("N") Then  {
                  \\ is a file?
                  Read f as long
                  buffer whitespace as byte
                  if not Eof(f) then {
                        get #f, whitespace :P6$=eval$(whitespace)
                        get #f, whitespace : P6$+=eval$(whitespace)
                        def boolean getW=true, getH=true, getV=true
                        def long v
                        \\ str$("P6") has 2 bytes. "P6" has 4 bytes
                        If p6$=str$("P6") Then {
                              do {
                                    get #f, whitespace
                                    if Eval$(whitespace)=str$("#") then {
                                          do {get #f, whitespace} until eval(whitespace)=10
                                    } else  {
                                         select case eval(whitespace)
                                          case 32, 9, 13, 10
                                          { if getW and x<>0 then {
                                                      getW=false
                                                } else.if getH  and y<>0 then {
                                                      getH=false
                                                } else.if getV and v<>0 then {
                                                      getV=false
                                                }
                                          }
                                          case 48 to 57
                                          {if getW then {
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
                                    iF eof(f) then Error "Not a ppm file"
                              } until getV=false
                        }  else Error "Not a P6 ppm"
                        Import=True
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
                  hline as rgb*x
            }
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
            Return Image1, 0!magic:="cDIB", 0!w:=Hex$(x,2), 0!h:=Hex$(y, 2)
            if not Import then  Return Image1, 0!lines:=Str$(String$(chrcode$(255), Len(rasterline)*y))
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
                  Print #f, "P6";chr$(10);"# Created using M2000 Interpreter";chr$(10);
                  Print #f, x;" ";y;" 255";chr$(10);
                  x2=x-1 :  where=0
                  Buffer pad as byte*3
                  For y1= 0 to y-1 {
                        For x1=0 to x2 {
                             Return pad, 0:=eval$(image1, 0!linesB!where, 3)
                             Push Eval(pad, 2) : Return pad, 2:=Eval(pad, 0), 0:=Number
                             Put #f, pad : where+=3
                        }
                        m=where mod 4 : if m<>0 then where+=4-m
                  }
            }
            if Import then {
                  x0=x-1 : where=0
                  Buffer Pad1 as byte*3
                  For y1=y-1 to 0 {
                        For x1=0 to x0 {Get #f, Pad1 : Push Eval(pad1, 2) : Return pad1, 2:=Eval(pad1, 0), 0:=Number
                              Return Image1, 0!linesB!where:=Eval$(Pad1) : where+=3}
                        m=where mod 4 : if m<>0 then where+=4-m}
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
      A=Bitmap(150,100)
      For i=0  to 98  {
            Call A.SetPixel(i, i, 0)
            Call A.SetPixel(99, i, 0)
      }
      Call A.SetPixel(i,i,0)
      Copy 200*twipsx, 100*twipsy use A.Image$()
      Profiler
      Open "a.ppm" for output as #F
      Call A.tofile(f)
      Close #f
      Print Filelen("a.ppm")
      Print Timecount/1000;"sec"
      Profiler
      Image A.Image$() Export "a.jpg", 100  ' per cent quality
      Print Filelen("a.jpg")
      Image A.Image$() Export "a1.jpg", 10  ' per cent quality
      Print Filelen("a1.jpg")
      Image A.Image$() Export "a.bmp"
      Print Filelen("a.bmp")  ' no compression
      Print Timecount/1000;"sec"
      Move 5000,5000   ' twips
      Image "a.jpg"
      Move 5000,8000
      Image "a1.jpg"
      Move 8000, 5000
      Image "a.bmp"
}
P6
```

Export using M2000 code for ppm is slower than using internal jpg and bmp encoders.
Jpg encoder has a 100% quality, and because this image is black and white we get the best compression.
Time 0.304sec is for three exports, two jpg and one bmp.

Output:

```txt
     45049
47.3661341sec
      1018
       691
     45254
0.3040944sec
```


## Maple

```Maple
allocateImg := proc(width, height)
	return Array(1..width, 1..height, 1..3);
end proc:
fillColor := proc(img, rgb::list)
	local i;
	for i from 1 to 3 do
		img[..,..,i] := map(x->rgb[i], img[..,..,i]):
	end do:
end proc:
setColor := proc(x, y, img, rgb::list)
	local i:
	for i from 1 to 3 do
		img[x,y,i] := rgb[i]:
	end do:
end proc:
getColor := proc(x,y,img)
	local rgb,i:
	rgb := Array(1..3):
	for i from 1 to 3 do
		rgb(i) := img[x,y,i]:
	end do:
	return rgb:
end proc:
```

Usage:

```txt
a := allocateImg(200,200);
fillColor(a,[255,223,0]);
setColor(150,150, a, [0,0,0]);
getColor(150,150,a);
#Output the image
ImageTools:-Embed(ImageTools:-Create(a))
```


## Mathematica / Wolfram Language

In Mathematica 7/8:

```Mathematica
img = Image[ConstantArray[{1, 0, 0}, {1000, 1000}]];
img = ReplacePart[img, {1, 1, 1} -> {0, 0, 1}];
ImageValue[img, {1, 1}]
```

In Mathematica 9:

```Mathematica
img = Image[ConstantArray[{1, 0, 0}, {1000, 1000}]];
img = ReplacePixelValue[img, {1, 1} -> {0, 0, 1}];
ImageValue[img, {1, 1}]
```


## MATLAB

Save this in a file named Bitmap.mat in a folder
named @Bitmap in your MATLAB root directory.

```MATLAB
%Bitmap class
%
%Implements a class to manage bitmap images without the need for the
%various conversion and display functions
%
%Available functions:
%
%fill(obj,color)
%setPixel(obj,pixel,color)
%getPixel(obj,pixel,[optional: color channel])
%display(obj)
%disp(obj)
%plot(obj)
%image(obj)
%save(obj)
%open(obj)

classdef Bitmap

    %% Public Properties
    properties

        %Channel arrays
        red;
        green;
        blue;

    end

    %% Public Methods
    methods

        %Creates image and defaults it to black
        function obj = Bitmap(width,height)
            obj.red   = zeros(height,width,'uint8');
            obj.green = zeros(height,width,'uint8');
            obj.blue  = zeros(height,width,'uint8');
        end % End Bitmap Constructor

        %Fill the image with a specified color
        %color = [red green blue] max for each is 255
        function fill(obj,color)
            obj.red(:,:)   = color(1);
            obj.green(:,:) = color(2);
            obj.blue(:,:)  = color(3);
            assignin('caller',inputname(1),obj); %saves the changes to the object
        end

        %Set a pixel to a specified color
        %pixel = [x y]
        %color = [red green blue]
        function setPixel(obj,pixel,color)
            obj.red(pixel(2),pixel(1))   = color(1);
            obj.green(pixel(2),pixel(1)) = color(2);
            obj.blue(pixel(2),pixel(1))  = color(3);
            assignin('caller',inputname(1),obj); %saves the changes to the object
        end

        %Get pixel color
        %pixel = [x y]
        %varargin can be:
        %  no input for all channels
        %  'r' or 'red' for red channel
        %  'g' or 'green' for green channel
        %  'b' or 'blue' for blue channel
        function color = getPixel(obj,pixel,varargin)

            if( ~isempty(varargin) )
                switch (varargin{1})
                    case {'r','red'}
                        color = obj.red(pixel(2),pixel(1));
                    case {'g','green'}
                        color = obj.red(pixel(2),pixel(1));
                    case {'b','blue'}
                        color = obj.red(pixel(2),pixel(1));
                end
            else
                color = [obj.red(pixel(2),pixel(1)) obj.green(pixel(2),pixel(1)) obj.blue(pixel(2),pixel(1))];
            end

        end

        %Display the image
        %varargin can be:
        %  no input for all channels
        %  'r' or 'red' for red channel
        %  'g' or 'green' for green channel
        %  'b' or 'blue' for blue channel
        function display(obj,varargin)

            if( ~isempty(varargin) )
                switch (varargin{1})
                    case {'r','red'}
                        image(obj.red)
                    case {'g','green'}
                        image(obj.green)
                    case {'b','blue'}
                        image(obj.blue)
                end

                colormap bone;
            else
                bitmap = cat(3,obj.red,obj.green,obj.blue);
                image(bitmap);
            end
        end

        %Overload several commonly used display functions
        function disp(obj,varargin)
            display(obj,varargin{:});
        end

        function plot(obj,varargin)
            display(obj,varargin{:});
        end

        function image(obj,varargin)
            display(obj,varargin{:});
        end

        %Saves the image
        function save(obj)

            %Open file dialogue
            [fileName,pathName,success] = uiputfile({'*.bmp','Bitmap Image (*.bmp)'},'Save Bitmap As');

            if( not(success == 0) )
                imwrite(cat(3,obj.red,obj.green,obj.blue),[pathName fileName],'bmp'); %Write image file to disk
                disp('Save Complete');
            end
        end

        %Opens an image and overwrites what is currently stored
        function success = open(obj)

            %Open file dialogue
            [fileName,pathName,success] = uigetfile({'*.bmp','Bitmap Image (*.bmp)'},'Open Bitmap ');

            if( not(success == 0) )

                channels = imread([pathName fileName], 'bmp'); %returns color indexed data

                %Store each channel
                obj.red   = channels(:,:,1);
                obj.green = channels(:,:,2);
                obj.blue  = channels(:,:,3);

                assignin('caller',inputname(1),obj); %saves the changes to the object
                success = true;
                return
            else
                success = false;
                return
            end
        end


    end %methods
end %classdef
```

Sample Usage:

```MATLAB
>> img = Bitmap(20,30);
>> img.fill([30 30 150]);
>> img.setPixel([10 15],[20 130 66]);
>> disp(img)
>> img.getPixel([10 15])

ans =

   20  130   66

>> img.getPixel([10 15],'red')

ans =

   20

>> img.save()
Save Complete
```


## MAXScript

MAXScript provides a built-in Bitmap class.

```maxscript
local myBitmap = bitmap 512 512
```

Filling the image with a single color can be accomplished at creation time
by setting the color property.

```maxscript
local myBitmap = bitmap 512 512 color:(color 128 128 128)
```

Use setPixels to set the color of a pixel.
This function takes an array of colors
and is optimized to set the colors of a whole row of pixels.

```maxscript
setPixels myBitmap [256, 256] #((color 255 255 255))
```

Use getPixels to retrieve the color of a pixel.
As with setPixels,
this function is optimised to retrieve one row at a time
as an array of color values.

```maxscript
local myPixel = getPixels myBitmap [256, 256] 1
```


## Modula-3

Since this code is for use with other tasks,
it uses an interface as well as the implementation module.

```modula3
INTERFACE Bitmap;

TYPE UByte = BITS 8 FOR [0 .. 16_FF];
     Pixel = RECORD R, G, B: UByte; END;
     Point = RECORD x, y: UByte; END;
     T = REF ARRAY OF ARRAY OF Pixel;

CONST
  Black = Pixel{0, 0, 0};
  White = Pixel{255, 255, 255};
  Red = Pixel{255, 0, 0};
  Green = Pixel{0, 255, 0};
  Blue = Pixel{0, 0, 255};
  Yellow = Pixel{255, 255, 0};

EXCEPTION BadImage;
          BadColor;

PROCEDURE NewImage(height, width: UByte): T RAISES {BadImage};
PROCEDURE Fill(VAR pic: T; color: Pixel);
PROCEDURE GetPixel(VAR pic: T; point: Point): Pixel RAISES {BadColor};
PROCEDURE SetPixel(VAR pic: T; point: Point; color: Pixel);

END Bitmap.
```


```modula3
MODULE Bitmap;

PROCEDURE NewImage(height, width: UByte): T RAISES {BadImage} =
  (* To make things easier, limit image size to also
     be UByte (0 to 255), and to have equal dimensions. *)
  BEGIN
    IF height # width THEN
      RAISE BadImage;
    END;
    RETURN NEW(T, height, width);
  END NewImage;

PROCEDURE Fill(VAR pic: T; color: Pixel) =
  BEGIN
    FOR i := FIRST(pic^) TO LAST(pic^) DO
      FOR j := FIRST(pic[0]) TO LAST(pic[0]) DO
        pic[i,j] := color;
      END;
    END;
  END Fill;

PROCEDURE GetPixel(VAR pic: T; point: Point): Pixel RAISES {BadColor} =
  VAR pixel := pic[point.x, point.y];
  BEGIN
    IF pixel = White THEN
      RETURN White;
    ELSIF pixel = Black THEN
      RETURN Black;
    ELSIF pixel = Red THEN
      RETURN Red;
    ELSIF pixel = Green THEN
      RETURN Green;
    ELSIF pixel = Blue THEN
      RETURN Blue;
    ELSIF pixel = Yellow THEN
      RETURN Yellow;
    ELSE
      RAISE BadColor;
    END;
  END GetPixel;

PROCEDURE SetPixel(VAR pic: T; point: Point; color: Pixel) =
  BEGIN
    pic[point.x, point.y] := color;
  END SetPixel;

BEGIN
END Bitmap.
```


## Nim

```nim
{.experimental.}

import unsigned

type
  Luminance = uint8
  Index = int

  Pixel = tuple
    r, g, b: Luminance

  Image = object
    w, h: Index
    pixels: seq[Pixel]

  Point = tuple
    x, y: Index

proc px(r, g, b): Pixel =
  result.r = r.uint8
  result.g = g.uint8
  result.b = b.uint8

proc img(w, h: int): Image =
  result.w = w
  result.h = h
  result.pixels.newSeq(w * h)

const
  Black = px(  0,   0,   0)
  White = px(255, 255, 255)

iterator indices(img: Image): tuple[x, y: int] =
  for x in 0 .. < img.w:
    for y in 0 .. < img.h:
      yield (x,y)

proc `[]`(img: Image, x, y: int): Pixel =
  img.pixels[y * img.w + x]

proc `[]=`(img: var Image, x, y: int, c: Pixel) =
  img.pixels[y * img.w + x] = c

proc fill(img: var Image, color: Pixel) =
  for x,y in img.indices:
    img[x,y] = color

proc print(img: Image) =
  using stdout
  for x,y in img.indices:
    if img[x,y] == White:
      write ' '
    else:
      write 'H'
    write "\n"

when isMainModule:
  var x = img(64, 64)
  x.fill px(255,255,255)
  x[1,2] = px(255, 0, 0)
  x[3,4] = x[1,2]
```


## OCaml

```ocaml
let new_img ~width ~height =
  let all_channels =
    let kind = Bigarray.int8_unsigned
    and layout = Bigarray.c_layout
    in
    Bigarray.Array3.create kind layout 3 width height
  in
  let r_channel = Bigarray.Array3.slice_left_2 all_channels 0
  and g_channel = Bigarray.Array3.slice_left_2 all_channels 1
  and b_channel = Bigarray.Array3.slice_left_2 all_channels 2
  in
  (all_channels,
   r_channel,
   g_channel,
   b_channel)
```

and here is the type of the raster image this function returns:

```ocaml
 type raster =
   (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array3.t *
   (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t *
   (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t *
   (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t
```

What is particular with this allocation and its associated type
is that there is not only one buffer for each RGB channel,
but also an additionnal one that handles all the three channels,
and what is important here is that it is not additionnal memory,
the memory is shared, so there are 2 ways to access the raster buffer:
through the separated RGB channels, or through the joint channel (all_channels).

This solution have a lot of advantages across a more naive one:
this type is compatible to memory-map a file
(a ppm file for instance, where the data is not compressed),
the buffer can be shared/exchanged with C
(for OpenGL textures for instance), etc.

A more naive form would be this one:


```ocaml
let new_img ~width ~height =
  let r_channel, g_channel, b_channel =
    let kind = Bigarray.int8_unsigned
    and layout = Bigarray.c_layout
    in
    (Bigarray.Array2.create kind layout width height,
     Bigarray.Array2.create kind layout width height,
     Bigarray.Array2.create kind layout width height)
  in
  (r_channel,
   g_channel,
   b_channel)
```

Here are the functions to fill with a color and to set one given pixel:

```ocaml
let fill_img ~img:(_, r_channel, g_channel, b_channel) ~color:(r,g,b) =
  Bigarray.Array2.fill r_channel r;
  Bigarray.Array2.fill g_channel g;
  Bigarray.Array2.fill b_channel b;
;;
```

```ocaml
let put_pixel_unsafe (_, r_channel, g_channel, b_channel) (r,g,b) =
  (fun x y ->
    r_channel.{x,y} <- r;
    g_channel.{x,y} <- g;
    b_channel.{x,y} <- b;
  )
```

```ocaml
let get_pixel_unsafe (_, r_channel, g_channel, b_channel) =
  (fun x y ->
    (r_channel.{x,y},
     g_channel.{x,y},
     b_channel.{x,y})
  )
```

we can overload these functions to make some bound checks:

```ocaml
let put_pixel img color x y =
  let _, r_channel,_,_ = img in
  let width = Bigarray.Array2.dim1 r_channel
  and height = Bigarray.Array2.dim2 r_channel in

  if (x < 0) || (x >= width) then invalid_arg "x out of bounds";
  if (y < 0) || (y >= height) then invalid_arg "y out of bounds";

  let r, g, b = color in
  if (r < 0) || (r > 255) then invalid_arg "red out of bounds";
  if (g < 0) || (g > 255) then invalid_arg "green out of bounds";
  if (b < 0) || (b > 255) then invalid_arg "blue out of bounds";

  put_pixel_unsafe img color x y;
;;

let get_pixel ~img ~pt:(x, y) =
  let _, r_channel,_,_ = img in
  let width = Bigarray.Array2.dim1 r_channel
  and height = Bigarray.Array2.dim2 r_channel in
  if (x < 0) || (x >= width) then invalid_arg "x out of bounds";
  if (y < 0) || (y >= height) then invalid_arg "y out of bounds";
  get_pixel_unsafe img x y;
;;
```

and a function to get the dimensions:

```ocaml
let get_dims ~img:(_, r_channel, _, _) =
  let width = Bigarray.Array2.dim1 r_channel
  and height = Bigarray.Array2.dim2 r_channel in
  (width, height)
```


## Octave

In Octave, images are a matrix.
A grayscale W×H image is stored as a W×H matrix,
and RGB W×H image is stored as a W×H×3 image.
Possible levels depend on the class of the storage: if it is double,
the intensity is a floating point double number between 0 and 1;
if it is uint8, the intensity is from 0 to 255;
if it is uint16, the intensity is between 0 and 65535.

```octave
im = zeros(W, H, 3, "uint8"); % create an RGB image of width W and height H
                              % and intensity from 0 to 255; black (all zeros)
im(:,:,1) = 255;              % set R to 255
im(:,:,2) = 100;              % set G to 100
im(:,:,3) = 155;              % set B to 155
im(floor(W/2), floor(H/2), :) = 0;  % pixel in the center made black
disp(im(floor(W/3), floor(H/3), :)) % display intensities of the pixel
                                    % at W/3, H/3
p = im(40,40,:); % or just store it in the vector p, so that
                 % p(1) is R, p(2) G and p(3) is B
```

We can hide this in helper functions like:

```octave
function im = create_rgb_image(w, h)
  im = zeros(w, h, 3, "uint8");
endfunction

function set_background(im, colorvector)
  im(:,:,1) = colorvector(1);
  im(:,:,2) = colorvector(2);
  im(:,:,3) = colorvector(3);
endfunction

function set_pixel(im, coord, colorvector)
  im(coord(1), coord(2), 1) = colorvector(1);
  im(coord(1), coord(2), 2) = colorvector(2);
  im(coord(1), coord(2), 3) = colorvector(3);
endfunction

function [r, g, b] = get_pixel(im, coord)
  r = im(coord(1), coord(2), 1)
  g = im(coord(1), coord(2), 2)
  b = im(coord(1), coord(2), 3)
endfunction
```

The only thing to note is that indexing start from 1.

```octave
%example
im = create_rgb_image(200,200);
for x = 1:128
   im = set_pixel(im, [x, x], [200, 50, 220]);
endfor

% it seems like saveimage wants double class on [0,1]
saveimage("image.ppm", double(im)./256, "ppm");
```


## OxygenBasic

```oxygenbasic
'GENERIC BITMAP

type pixel byte r,g,b

'
### =====

class BitMap
'
### =====


% sp   sizeof(pixel)
sys    wx,wy,px,py
string buf
sys    pb
method Constructor(sys x=640,y=480) { wx=x : wy=y : buf=nuls x*y*sp : pb=strptr buf}
method Destructor()                 {buf="" : wx=0 : wy=0 : pb=0}
method GetPixel(sys x,y,pixel*p)    {copy @p,pb+(y*wx+x)*sp,sp}
method SetPixel(sys x,y,pixel*p)    {copy pb+(y*wx+x)*sp,@p,sp}
'
method Fill(pixel*p)
  sys i, e=wx*wy*sp+pb-1
  for i=pb to e step sp {copy i,@p,sp}
end method

end class

pixel p,q

new BitMap m(400,400) 'width, height in pixels

p<=100,120,140 'red,green,blue

m.fill p

m.getPixel 200,100,q
print "" q.r "," q.g "," q.b 'result 100,120,140
q<=10,20,40
m.setPixel 200,100,q
m.getPixel 200,100,p
print "" p.r "," p.g "," p.b 'result 10,20,40


del m
```


## Oz

We first create a 2D array data type as a functor in a file "Array2D.oz":

```oz
functor
export
   New
   Get
   Set
   Transform
define
   fun {New Width Height Init}
      C = {Array.new 1 Height unit}
   in
      for Row in 1..Height do
	 C.Row := {Array.new 1 Width Init}
      end

      array2d(width:Width
	      height:Height
	      contents:C)
   end

   fun {Get array2d(contents:C ...) X Y}
      C.Y.X
   end

   proc {Set array2d(contents:C ...) X Y Val}
      C.Y.X := Val
   end

   proc {Transform array2d(contents:C width:W height:H ...) Fun}
      for Y in 1..H do
	 for X in 1..W do
	    C.Y.X := {Fun C.Y.X}
	 end
      end
   end

   %% omitted: Clone, Map, Fold, ForAll
end
```

Based on this, we create a functor "Bitmap.oz":

```oz
%% For real task prefer QTk's images:
%% http://www.mozart-oz.org/home/doc/mozart-stdlib/wp/qtk/html/node38.html

functor
import
   Array2D
export
   New
   Fill
   GetPixel
   SetPixel
define
   Black = color(0x00 0x00 0x00)

   fun {New Width Height}
      bitmap( {Array2D.new Width Height Black} )
   end

   proc {Fill bitmap(Arr) Color}
      {Array2D.transform Arr fun {$ _} Color end}
   end

   fun {GetPixel bitmap(Arr) X Y}
      {Array2D.get Arr X Y}
   end

   proc {SetPixel bitmap(Arr) X Y Color}
      {Array2D.set Arr X Y Color}
   end

   %% Omitted: MaxValue, ForAllPixels, Transform
end
```

Some functions that are used in other tasks were omitted.
See here for the complete module definitions: [[Basic bitmap storage/Oz]]


## Pascal

```Pascal
Interface
uses  crt,   { GetDir }
      graph; { function GetPixel }

type { integer numbers }
  { from unit bitmaps XPERT software production Tamer Fakhoury }
  _bit     = $00000000..$00000001; {number 1 bit   without sign = (0..1) }
  _byte    = $00000000..$000000FF; {number 1 byte  without sign = (0..255)}
  _word    = $00000000..$0000FFFF; {number 2 bytes without sign = (0..65 535)}
  _dWord   = $00000000..$7FFFFFFF; {number 4 bytes without sign = (0..4 294 967 296)}
  _longInt = $80000000..$7FFFFFFF; {number 4 bytes with sign
                                    = (-2 147 483 648..2 147 483 648}

  TbmpFileHeader =
  record
    ID: _word;             { Must be 'BM' =19778=$424D for windows }
    FileSize: _dWord;      { Size of this file in bytes }
    Reserved: _dWord;      { ??? }
    bmpDataOffset: _dword; { = 54 = $36 from begining of file to begining of bmp data }
  end;

  TbmpInfoHeader =
  record
    InfoHeaderSize: _dword;      { Size of Info header
                                   = 28h = 40 (decimal)
                                   for windows }
    Width,
    Height: _longInt;    { Width and Height of image in pixels }
    Planes,              { number of planes of bitmap }
    BitsPerPixel: _word; { Bits can be 1, 4, 8, 24 or 32 }
    Compression,
    bmpDataSize: _dword;    { in bytes rounded to the next 4 byte boundary }
    XPixPerMeter,           { horizontal resolution in pixels }
    YPixPerMeter: _longInt; { vertical }
    NumbColorsUsed,
    NumbImportantColors: _dword;   {= NumbColorUsed}
  end; { TbmpHeader = Record ... }

  T32Color =
  record { 4 byte = 32 bit }
    Blue:  byte;
    Green: byte;
    Red:   byte;
    Alfa:  byte
  end;

var directory,
    bmpFileName:    string;
    bmpFile:        file; { untyped file }
    bmpFileHeader:  TbmpFileHeader;
    bmpInfoHeader:  TbmpInfoHeader;
    color32:        T32Color;
    RowSizeInBytes: integer;
    BytesPerPixel:  integer;

const defaultBmpFileName = 'test';
      DefaultDirectory   = 'c:\bp\';
      DefaultExtension   = '.bmp';
      bmpFileHeaderSize  = 14;
      { compression specyfication }
      bi_RGB             = 0;  { compression }
      bi_RLE8            = 1;
      bi_RLE4            = 2;
      bi_BITFIELDS       = 3;

      bmp_OK          = 0;
      bmp_NotBMP      = 1;
      bmp_OpenError   = 2;
      bmp_ReadError   = 3;

Procedure CreateBmpFile32(directory: string; FileName: string;
                           iWidth, iHeight: _LongInt);

{************************************************}
Implementation  {-----------------------------}
{************************************************}

Procedure CreateBmpFile32(directory: string; FileName: string;
                           iWidth, iHeight: _LongInt);
  var
    x, y: integer;
  begin
    if directory = '' then
      GetDir(0, directory);
    if FileName = '' then
      FileName: = DefaultBmpFileName;
    { create a new file on a disk in a given directory with given name }
    Assign(bmpFile, directory + FileName + DefaultExtension);
    ReWrite(bmpFile, 1);

    { fill the headers }
    with bmpInfoHeader, bmpFileHeader do
    begin
      ID := 19778;
      InfoheaderSize := 40;
      width := iWidth;
      height := iHeight;
      BitsPerPixel := 32;
      BytesPerPixel := BitsPerPixel div 8;
      reserved := 0;
      bmpDataOffset := InfoHeaderSize + bmpFileHeaderSize;
      planes := 1;
      compression := bi_RGB;
      XPixPerMeter := 0;
      YPixPerMeter := 0;
      NumbColorsUsed := 0;
      NumbImportantColors := 0;

      RowSizeInBytes := (Width * BytesPerPixel); { only for >=8 bits per pixel }
      bmpDataSize := height * RowSizeinBytes;
      FileSize := InfoHeaderSize + bmpFileHeaderSize + bmpDataSize;

      { copy headers to disk file }
      BlockWrite(bmpFile, bmpFileHeader, bmpFileHeaderSize);
      BlockWrite(bmpFile, bmpInfoHeader, infoHeaderSize);

      { fill the pixel data area }
      for y := (height - 1) downto 0  do
      begin
	for x := 0 to (width - 1) do
	begin { Pixel(x,y) }
	  color32.Blue  := 255;
	  color32.Green := 0;
	  color32.Red   := 0;
	  color32.Alfa  := 0;
	  BlockWrite(bmpFile, color32, 4);
	end; { for x ... }
      end; { for y ... }
      Close(bmpFile);
    end; { with bmpInfoHeader, bmpFileHeader }
   end; { procedure }
```


## Perl

```perl
#! /usr/bin/perl

use strict;

use Image::Imlib2;

# create the "canvas"
my $img = Image::Imlib2->new(200,200);

# fill with a plain RGB(A) color
$img->set_color(255, 0, 0, 255);
$img->fill_rectangle(0,0, 200, 200);

# set a pixel to green (at 40,40)
$img->set_color(0, 255, 0, 255);
$img->draw_point(40,40);

# "get" pixel rgb(a)
my ($red, $green, $blue, $alpha) = $img->query_pixel(40,40);
undef $img;

# another way of creating a canvas with a bg color (or from
# an existing "raw" data)
my $col = pack("CCCC", 255, 255, 0, 0); # a, r, g, b
my $img = Image::Imlib2->new_using_data(200, 200, $col x (200 * 200));

exit 0;
```


## Perl 6

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

my Bitmap $b = Bitmap.new( width => 10, height => 10);

$b.fill( Pixel.new( R => 0, G => 0, B => 200) );

$b.set-pixel( 7, 5, Pixel.new( R => 100, G => 200, B => 0) );

say $b.perl;
```

Thanks to the <tt>rw</tt> trait on the <tt>pixel</tt> method,
we don't actually need to define two separate methods,
<tt>set-pixel</tt> and <tt>get-pixel</tt>, but that is an explicit requirement of the task.
(Beware your presuppositions!
In Perl 6, accessors only determine identity, not use.
In particular, identity is considered orthogonal to lvalue/rvalue context.)


## Phix

Copy of [[Bitmap#Euphoria|Euphoria]]

```Phix
-- Some color constants:
constant black = #000000,
--       blue  = #0000FF,
--       green = #00FF00,
--       red   = #FF0000,
         white = #FFFFFF

-- Create new image filled with some color
function new_image(integer width, integer height, integer fill_colour=black)
    return repeat(repeat(fill_colour,height),width)
end function

-- Usage example:
sequence image = new_image(800,600)

-- Set pixel color:
image[400][300] = white

-- Get pixel color
integer color = image[400][300] -- Now color is #FF0000
```


## PHP

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
}

$b = new Bitmap(16,16);
$b->fill();
$b->fill(2, 2, 18, 18, array(240,240,240));
$b->setPixel(0, 15, array(255,0,0));
print_r($b->getPixel(3,3)); //(240,240,240)
```


## PL/I

```PL/I
/* Declaration for an image, suitable for BMP files. */
declare image(0:500, 0:500) bit (24) aligned;

image = '000000000000000011111111'b;
   /* Sets the entire image to red. */

image(10,40) = '111111110000000000000000'b;
   /* Sets one pixel to blue. */

declare color bit (24) aligned;
color = image(20,50); /* Obtain the color of a pixel */



/* To allocate an image of size (x,y) */
allocate_image: procedure (image, x, y);
   declare image (*, *) controlled bit (24) aligned;
   declare (x, y) fixed binary (31);

   allocate image (0:x, 0:y);
end allocate_image;

/* To use the above procedure, it's necessary to define   */
/* the image in the calling program thus, for BMP images: */

declare image(*,*) controlled bit (24) aligned;
```


## PicoLisp

For time critical applications this would be done with inline-C in PicoLisp,
but especially for small bitmaps the following makes sense.

```PicoLisp
# Create an empty image of 120 x 90 pixels
(setq *Ppm (make (do 90 (link (need 120)))))

# Fill an image with a given color
(de ppmFill (Ppm R G B)
   (for Y Ppm
      (map
         '((X) (set X (list R G B)))
         Y ) ) )

# Set pixel with a color
(de ppmSetPixel (Ppm X Y R G B)
   (set (nth Ppm Y X) (list R G B)) )

# Get the color of a pixel
(de ppmGetPixel (Ppm X Y)
   (get Ppm Y X) )
```


## PureBasic

```PureBasic
w=800 : h=600
CreateImage(1,w,h)
;1 is internal id of image
StartDrawing(ImageOutput(1))
; fill with color red
Box(0,0,w,h,$ff)
; or using another (but slower) way in green
FillArea(0,0,-1,$ff00)
; a green Dot
Plot(10,10,$ff0000)
; check if we set it right (should be 255)
Debug Blue(Point(10,10))
```


## Python

See [[Basic bitmap storage/Python]]


## R

{{libheader|pixmap}}

R can write to most bitmap image formats by default (mostly for the purpose of saving graphs),
however there is no built-in way of manipulating images.
The pixmap package reads, writes and manipulates portable bitmap file types: PBM, PGM, PPM.
See also, the image function, and the rimage and ReadImage packages,
which use libjpeg to read JPEG and PNG files.

```r
#  See the class definitions and constructors with, e.g.
getClass("pixmapIndexed", package=pixmap)
pixmapIndexed

# Image with all one color
plot(p1 <- pixmapIndexed(matrix(0, nrow=3, ncol=4), col="red"))

# Image with one pixel specified
cols <- rep("blue", 12); cols[7] <- "red"
plot(p2 <- pixmapIndexed(matrix(1:12, nrow=3, ncol=4), col=cols))

# Retrieve color of a pixel
getcol <- function(pm, i, j)
{
   pmcol <- pm@col
   dim(pmcol) <- dim(pm@index)
   pmcol[i,j]
}
getcol(p2, 3, 4)  #red
```


## Racket

```racket
#lang racket

;; The racket/draw libraries provide imperative drawing functions.
;; http://docs.racket-lang.org/draw/index.html
(require racket/draw)

;; To create an image with width and height, use the make-bitmap
;; function.

;; For example, let's make a small image here:
(define bm (make-bitmap 640 480))

;; We use a drawing context handle, a "dc", to operate on the bitmap.
(define dc (send bm make-dc))

;; We can fill the bitmap with a color by using a combination of
;; setting the background, and clearing.
(send dc set-background (make-object color% 0 0 0)) ;; Color it black.
(send dc clear)

;; Let's set a few pixels to a greenish color with set-pixel:
(define aquamarine (send the-color-database find-color "aquamarine"))
(for ([i 480])
  (send dc set-pixel i i aquamarine))

;; We can get at the color of a bitmap pixel by using the get-pixel
;; method.  However, it may be faster to use get-argb-pixels if we
;; need a block of the pixels.  Let's use get-argb-pixels and look
;; at a row starting at (0, 42)
(define buffer (make-bytes (* 480 4)))  ;; alpha, red, green, blue
(send dc get-argb-pixels 0 42 480 1 buffer)

;; We can inspect the buffer
(bytes-ref buffer 0) ;;  and see that the first pixel's alpha is 255,
(bytes-ref buffer 1) ;;  and the red, green, and blue components are 0.
(bytes-ref buffer 2)
(bytes-ref buffer 3)

;; If we are using DrRacket, we can just print the bm as a toplevel expression
;; to view the final image:
bm
```


## RapidQ

QCanvas is an empty image on which you can draw.
QForm is the main window of the application.
The commands to draw on the canvas are in the procedure PaintCanvas,
which is executed each time the canvas need to be (re)painted.


```rapidq
DECLARE SUB PaintCanvas

CREATE form AS QForm
    Width  = 640
    Height = 480
    CREATE canvas AS QCanvas
        Height  = form.ClientHeight
	Width   = form.ClientWidth
	OnPaint = PaintCanvas
    END CREATE
END CREATE

SUB PaintCanvas
    ' Fill background
    canvas.FillRect(0, 0, canvas.Width, canvas.Height, &H301000)

    ' Draw a pixel
    canvas.Pset(300, 200, &H00ddff)

    ' Read pixel color
    PRINT canvas.Pixel(300, 200)
END SUB

form.ShowModal
```


## REXX

### version 1

The REXX language has no need to declare the size of (stemmed) arrays.

Indeed, there is no way to declare array sizes   (or any variable, for that matter).

The image (raster) created was also written to a file   ('''image.PPM''')   to show verification of the image.

```rexx
/*REXX program demonstrates how to process/display a simple  RGB  raster graphics image.*/
red   = 'ff 00 00'x                              /*a method to define a   red   value.  */
blue  = '00 00 ff'x                              /*"    "    "    "   "   blue    "     */
@.    =                                          /*define entire  @.  array  to  nulls. */
outFN = 'image'                                  /*the filename of the output image PPM */
sWidth = 500;     sHeight= 500                   /*the screen width and height in pixels*/
call RGBfill      red                            /*set the entire   image   to red.     */
            x= 10;       y= 40                   /*set pixel's coördinates.             */
call RGBset x, y, blue                           /*set a pixel (at  10,40)  to blue.    */
color = RGBget(x, y)                             /*get the color of a pixel.            */
hexV  = c2x(color)                               /*get hex    value of pixel's color.   */
binV  = x2b(hexV)                                /* "  binary   "    "    "      "      */
bin3V = left(binV, 8)    substr(binV, 9, 8)    right(binV, 8)
hex3V = left(hexV, 2)    substr(hexV, 3, 2)    right(hexV, 2)
xy= '(' || x","y')'                              /*create a handy─dandy literal for SAY.*/
say  xy    ' pixel in binary: '     binV         /*show the binary value of  20,50      */
say  xy    ' pixel in binary: '     bin3V        /*show again, but with spaces.         */
say                                              /*show a blank between binary and hex. */
say  xy    ' pixel in hex:    '     hexV         /*show again, but in hexadecimal.      */
say  xy    ' pixel in hex:    '     hex3V        /*show again, but with spaces.         */
call PPMwrite outFN, sWidth, sHeight             /*create a PPM (output) file of image. */      /* ◄■■■■■■■■ not part of this task.*/
say                                              /*show a blank.                        */
say 'The file ' outFN".PPM  was created."        /*inform user that a file was created. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
RGBfill:  @.=arg(1);                          return          /*fill image with a color.*/
RGBget:   parse arg px,py;                    return @.px.py  /*get a pixel's color.    */
RGBset:   parse arg px,py,p$;  @.px.py=p$;    return          /*set "    "      "       */
/*──────────────────────────────────────────────────────────────────────────────────────*/
PPMwrite: parse arg oFN, width, height           /*obtain output filename, width, height*/
          oFID= oFN'.PPM';   $='9'x;   #=255     /*fileID;  separator;  max color value.*/
          call charout oFID, ,  1                /*set the position of the file's output*/
          call charout oFID,'P6'width || $ || height || $ || # || $    /*write hdr info.*/
            do   j=1  for width
              do k=1  for height;     call charout oFID, @.j.k
              end   /*k*/                        /*  ↑          write the PPM file, ··· */
            end     /*j*/                        /*  └───────── ··· one pixel at a time.*/
          call charout oFID;      return         /*close the output file just to be safe*/
```

Output:

```txt
(10,40)  pixel in binary:  000000000000000011111111
(10,40)  pixel in binary:  00000000 00000000 11111111

(10,40)  pixel in hex:     0000FF
(10,40)  pixel in hex:     00 00 FF

The file  image.PPM  was created.
```


### version 2

This program actually creates a BMP file

```rexx
/* REXX ***************************************************************
* Draw a picture from pixels
* 16.06.2014 Walter Pachl
**********************************************************************/
oid='pic.bmp'; 'erase' oid

blue ='FF0000'x;
green='00FF00'x;
red  ='0000FF'x;
white='ffffff'x;
black='000000'x;

w=600                        /* width  */
h=300                        /* height */
w3=w*3

bfType         ='BM'
bfSize         ='46000000'x
bfReserved     ='00000000'x
bfOffBits      ='36000000'x
biSize         ='28000000'x
biWidth        =lend(w)
biHeight       =lend(h)
biPlanes       ='0100'x
biBitCount     ='1800'x
biCompression  ='00000000'x
biSizeImage    ='10000000'x
biXPelsPerMeter='00000000'x
biYPelsPerMeter='00000000'x
biClrUsed      ='00000000'x
biClrImportant ='00000000'x

s=bfType||,
  bfSize||,
  bfReserved||,
  bfOffBits||,
  biSize||,
  biWidth||,
  biHeight||,
  biPlanes||,
  biBitCount||,
  biCompression||,
  biSizeImage||,
  biXPelsPerMeter||,
  biYPelsPerMeter||,
  biClrUsed||,
  biClrImportant

pic=copies(red,w*h)             /* fill the rectangle with color red */
Call rect 100,100,180,180,green /* draw a green rectangle            */
Call rect 100,100,160,160,blue  /* and a blue rectangle within that  */
Call dot 120,120,white          /* one pixel is hardly visible       */
Do x=98 To 102                  /* draw a square of 25 pixels        */
  Do y=98 To 102
    Call dot x,y,white
    End
  End
Call charout oid,s||pic         /* write the picture to file         */
dmy=col(97,98)
dmy=col(98,98)
Exit

lend: Procedure
/**********************************************************************
* compute the representation of a number (little endian)
**********************************************************************/
Parse Arg n
res=reverse(d2c(n,4))
rev=reverse(res)
say 'lend:' arg(1) '->' c2x(res) '=>' c2d(rev)
Return res

rect: Procedure Expose pic w h w3
/**********************************************************************
* Fill a rectangle with center at x,y and width/height = wr/hr
**********************************************************************/
Parse Arg x,y,wr,hr,color
Say x y wr hr c2x(color)
i=w3*(y-1)+3*(x-1)+1               /* Pixel position of center       */
ia=max(w3*(y-1)+1,i-3*(wr%2))      /* position of left border        */
ib=min(i+3*wr%2,w3*y)              /* position of right border       */
lc=ib-ia                           /* length of horizontal line      */
If lc>=0 Then Do
  os=copies(color,lc%3)            /* the horizontal line            */
  Do hi=-hr%2 to hr%2              /* loop from lower to upper border*/
    i=trunc(ia+w3*hi)              /* position of line's left border */
    If i>1 Then Do
      pic=overlay(os,pic,i)        /* put the line into the picture  */
      j=i%w3
      End
    End
  End
Return

dot: Procedure Expose pic w h w3
/**********************************************************************
* Put a dot at position x/y into the picture
**********************************************************************/
Parse Arg x,y,color
i=w3*(y-1)+3*(x-1)
pic=overlay(color,pic,i+1)
Return

col: Procedure Expose pic w h w3
/**********************************************************************
* get the color at position x/y
**********************************************************************/
Parse Arg x,y,color
i=w3*(y-1)+3*(x-1)
say 'color at pixel' x'/'y'='c2x(substr(pic,i+1,3))
Return c2x(substr(pic,i+1,3))
```

Output:

```txt
lend: 600 -> 58020000 => 600
lend: 300 -> 2C010000 => 300
100 100 180 180 00FF00
100 100 160 160 FF0000
color at pixel 97/98=FF0000
color at pixel 98/98=FFFFFF

and have a look at the file pic.bmp created by this program
```


## Ruby

I haven't been able to find any kind of package for manipulating bitmap images,
so let's roll one.

```ruby
class RGBColour
  def initialize(red, green, blue)
    unless red.between?(0,255) and green.between?(0,255) and blue.between?(0,255)
      raise ArgumentError, "invalid RGB parameters: #{[red, green, blue].inspect}"
    end
    @red, @green, @blue = red, green, blue
  end
  attr_reader :red, :green, :blue
  alias_method :r, :red
  alias_method :g, :green
  alias_method :b, :blue

  RED   = RGBColour.new(255,0,0)
  GREEN = RGBColour.new(0,255,0)
  BLUE  = RGBColour.new(0,0,255)
  BLACK = RGBColour.new(0,0,0)
  WHITE = RGBColour.new(255,255,255)
end

class Pixmap
  def initialize(width, height)
    @width = width
    @height = height
    @data = fill(RGBColour::WHITE)
  end
  attr_reader :width, :height

  def fill(color)
    @data = Array.new(@width) {Array.new(@height, color)}
  end

  def validate_pixel(x,y)
    unless x.between?(0, @width-1) and y.between?(0, @height-1)
      raise ArgumentError, "requested pixel (#{x}, #{y}) is outside dimensions of this bitmap"
    end
  end

  def [](x,y)
    validate_pixel(x,y)
    @data[x][y]
  end
  alias_method :get_pixel, :[]

  def []=(x,y,color)
    validate_pixel(x,y)
    @data[x][y] = color
  end
  alias_method :set_pixel, :[]=
end
```


## Rust

```Rust
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Rgb {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Rgb {
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Rgb { r, g, b }
    }

    pub const BLACK: Rgb = Rgb { r: 0, g: 0, b: 0 };
    pub const RED: Rgb = Rgb { r: 255, g: 0, b: 0 };
    pub const GREEN: Rgb = Rgb { r: 0, g: 255, b: 0 };
    pub const BLUE: Rgb = Rgb { r: 0, g: 0, b: 255 };
}

#[derive(Clone, Debug)]
pub struct Image {
    width: usize,
    height: usize,
    pixels: Vec<Rgb>,
}

impl Image {
    pub fn new(width: usize, height: usize) -> Self {
        Image {
            width,
            height,
            pixels: vec![Rgb::BLACK; width * height],
        }
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn height(&self) -> usize {
        self.height
    }

    pub fn fill(&mut self, color: Rgb) {
        for pixel in &mut self.pixels {
            *pixel = color;
        }
    }

    pub fn get(&self, row: usize, col: usize) -> Option<&Rgb> {
        if row >= self.width {
            return None;
        }
        self.pixels.get(row * self.width + col)
    }

    pub fn get_mut(&mut self, row: usize, col: usize) -> Option<&mut Rgb> {
        if row >= self.width {
            return None;
        }
        self.pixels.get_mut(row * self.width + col)
    }
}

fn main() {
    let mut image = Image::new(16, 9);
    assert_eq!(Some(&Rgb::BLACK), image.get(3, 4));
    assert!(image.get(22, 3).is_none());

    image.fill(Rgb::RED);
    assert_eq!(Some(&Rgb::RED), image.get(3, 4));

    if let Some(pixel) = image.get_mut(3, 4) {
        *pixel = Rgb::GREEN;
    }
    assert_eq!(Some(&Rgb::GREEN), image.get(3, 4));

    if let Some(pixel) = image.get_mut(3, 4) {
        pixel.g -= 100;
        pixel.b = 20;
    }
    assert_eq!(Some(&Rgb::new(0, 155, 20)), image.get(3, 4));
}
```


## Scala

### Java translation

Translated from Java

```scala
import java.awt.image.BufferedImage
import java.awt.Color

class RgbBitmap(val width:Int, val height:Int) {
   val image=new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

   def fill(c:Color)={
      val g=image.getGraphics()
      g.setColor(c)
      g.fillRect(0, 0, width, height)
   }

   def setPixel(x:Int, y:Int, c:Color)=image.setRGB(x, y, c.getRGB())
   def getPixel(x:Int, y:Int)=new Color(image.getRGB(x, y))
}
```

Usage:

```scala
val img=new RgbBitmap(50, 50);
img.fill(Color.CYAN)
img.setPixel(5, 5, Color.BLUE)

assert(img.getPixel(1,1)==Color.CYAN)
assert(img.getPixel(5,5)==Color.BLUE)
assert(img.width==50)
assert(img.height==50)
```


### Scala idiom

A more Scalesque version could be with the use of its idiom.
Best experienced in your browser
[https://scastie.scala-lang.org/cy2uZB9DSaWVMnZjTJgQNA with Scastie
(remote JVM)].

```scala
import java.awt.image.BufferedImage
import java.awt.Color

object RgbBitmap extends App {
  class RgbBitmap(val dim: (Int, Int)) {
    def width = dim._1
    def height = dim._2

    private val image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    def apply(x: Int, y: Int) = new Color(image.getRGB(x, y))

    def update(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB)

    def fill(c: Color) = {
      val g = image.getGraphics
      g.setColor(c)
      g.fillRect(0, 0, width, height)
    }
  }

  object RgbBitmap {
    def apply(width: Int, height: Int) = new RgbBitmap(width, height)
  }


  /** Even Javanese style testing is still possible.
    */
  private val img0 = new RgbBitmap(50, 60) { // Wrappers to enable adhoc Javanese style
    def getPixel(x: Int, y: Int) = this(x, y)
    def setPixel(x: Int, y: Int, c: Color) = this(x, y) = c
  }

  img0.fill(Color.CYAN)
  img0.setPixel(5, 6, Color.BLUE)
  // Testing in Java style
  assert(img0.getPixel(0, 1) == Color.CYAN)
  assert(img0.getPixel(5, 6) == Color.BLUE)
  assert(img0.width == 50)
  assert(img0.height == 60)
  println("Tests successfully completed with no errors found.")
}
```


## Scheme

Definitions of list procedures:

```scheme
(define (make-list length object)
  (if (= length 0)
      (list)
      (cons object (make-list (- length 1) object))))

(define (list-fill! list object)
  (if (not (null? list))
      (begin (set-car! list object) (list-fill! (cdr list) object))))

(define (list-set! list element object)
  (if (= element 1)
      (set-car! list object)
      (list-set! (cdr list) (- element 1) object)))

(define (list-get list element)
  (if (= element 1)
      (car list)
      (list-get (cdr list) (- element 1))))
```

Definitions of image procedures:

```scheme
(define (make-image columns rows)
  (if (= rows 0)
      (list)
      (cons (make-list columns (list)) (make-image columns (- rows 1)))))

(define (image-fill! image color)
  (if (not (null? image))
      (begin (list-fill! (car image) color) (image-fill! (cdr image) color))))

(define (image-set! image column row color)
  (list-set! (list-get image row) column color))

(define (image-get image column row)
  (list-get (list-get image row) column))
```

Definitions of some colors:

```scheme
(define *black* (list   0   0   0))
(define *white* (list 255 255 255))
(define *red*   (list 255   0   0))
(define *green* (list   0 255   0))
(define *blue*  (list   0   0 255))
```

This creates a small image with a black background and a single blue pixel:

```scheme
(define image (make-image 3 2))
(image-fill! image *black*)
(image-set! image 2 1 *blue*)
(display image)
(newline)
```

Output:
```txt
(((0 0 0) (0 0 255) (0 0 0)) ((0 0 0) (0 0 0) (0 0 0)))
```


## Seed7

The types and functions requested are predefined in the libraries
[graph.s7i](http://seed7.sourceforge.net/libraries/graph.htm) and
[draw.s7i](http://seed7.sourceforge.net/libraries/draw.htm):

- The type to handle an RGB raster graphics image is PRIMITIVE_WINDOW.
- The function to create an image is
  [newPixmap](http://seed7.sourceforge.net/libraries/draw.htm#newPixmap%28in_integer,in_integer%29).
- An imaged can be filled with a color with
  [clear](http://seed7.sourceforge.net/libraries/draw.htm#clear%28inout_PRIMITIVE_WINDOW,in_color%29).
- A given pixel can be set with
  [point](http://seed7.sourceforge.net/libraries/draw.htm#point(inout_PRIMITIVE_WINDOW,in_integer,in_integer,in_color).
- The color of a pixel can be retrieved with
  [getPixelColor](http://seed7.sourceforge.net/libraries/draw.htm#getPixelColor(in_PRIMITIVE_WINDOW,in_integer,in_integer).


```seed7
$ include "seed7_05.s7i";
  include "draw.s7i";

const proc: main is func
  local
    var PRIMITIVE_WINDOW: myPixmap is PRIMITIVE_WINDOW.value;
    var color: myColor is black;
  begin
    myPixmap := newPixmap(300, 200);
    clear(myPixmap, light_green);
    point(myPixmap, 20, 30, color(256, 512, 768));
    myColor := getPixelColor(myPixmap, 20, 30);
    writeln(myColor.redLight <& " " <& myColor.greenLight <& " " <& myColor.blueLight);
  end func;
```


## SequenceL

```seed7
RGB ::= (R: int(0), G: int(0), B: int(0));

newBitmap: int * int -> RGB(2);
newBitmap(width, height)[y, x] :=
	 (R: 0, G: 0, B: 0)
	 foreach y within 1 ... height,
	 		 x within 1 ... width;

fill: RGB(2) * RGB -> RGB(2);
fill(bitmap(2), color)[y, x] :=
	color
	foreach y within 1 ... size(bitmap),
			x within 1 ... size(bitmap[y]);

setColorAt: RGB(2) * int * int * RGB -> RGB(2);
setColorAt(bitmap(2), x, y, color)[Y, X] :=
		color when Y = y and X = x
	else
		bitmap[Y, X];

getColorAt: RGB(2) * int * int -> RGB;
getColorAt(bitmap(2), x, y) := bitmap[y, x];

lightGreen := (R: 51, G: 255, B: 51);
lightRed := (R: 255, G: 51, B: 51);

main(args(2)) :=
	let
		width := 1920;
		height := 1200;

		cleanImage := newBitmap(width, height);

		filledGreen := fill(cleanImage, lightGreen);

		redCenter := setColorAt(filledGreen, width / 2, height / 2, lightRed);
	in
		getColorAt(redCenter, width / 2, height / 2);
```

Output:

```txt
cmd:> main.exe
(B:51,G:51,R:255)
```


## Tcl

```tcl
package require Tcl 8.5
package require Tk
namespace path ::tcl::mathfunc ;# for [max] function

proc newImage {width height} {
    return [image create photo -width $width -height $height]
}
proc fill {image color} {
    $image put $color -to 0 0 [$image cget -width] [$image cget -height]
}
proc setPixel {image color point} {
    lassign $point x y
    $image put $color -to [max 0 $x] [max 0 $y]
}
proc getPixel {image point} {
    lassign $point x y
    # [$img get] returns a list: {r g b}; this proc should return a color value
    format {#%02x%02x%02x} {*}[$image get $x $y]
}

# create the image and display it
set img [newImage 150 150]
label .l -image $img
pack .l

fill $img red

setPixel $img green {40 40}

set rbg [getPixel $img {40 40}]
```


## TI-89 BASIC

TI-89 BASIC does not have user-defined data structures.
Tasks which use this image type
have instead been implemented using the TI-89's graph screen.


## UNIX Shell

Works with ksh93.

```bash
typeset -T RGBColor_t=(
    integer r g b
    function to_s {
        printf "%d %d %d" ${_.r} ${_.g} ${_.b}
    }
    function white   { print "255 255 255"; }
    function black   { print "0 0 0"; }
    function red     { print "255 0 0"; }
    function green   { print "0 255 0"; }
    function blue    { print "0 0 255"; }
    function yellow  { print "255 255 0"; }
    function magenta { print "255 0 255"; }
    function cyan    { print "0 255 255"; }
)

typeset -T Bitmap_t=(
    integer height
    integer width
    typeset -a data

    function fill {
        typeset color=$1
        if [[ -z ${color:+set} ]]; then
            print -u2 "error: no fill color specified"
            return 1
        fi
        integer x y
        for ((y=0; y<_.height; y++)); do
            for ((x=0; x<_.width; x++)); do
                _.data[y][x]="$color"
            done
        done
    }

    function setpixel {
        integer x=$1 y=$2
        typeset color=$3
        _.data[y][x]=$color
    }

    function getpixel {
        integer x=$1 y=$2
        print "${_.data[y][x]}"
    }

    function to_s {
        typeset ppm=""
        ppm+="P3"$'\n'
        ppm+="${_.width} ${_.height}"$'\n'
        ppm+="255"$'\n'
        typeset sep
        for ((y=0; y<_.height; y++)); do
            sep=""
            for ((x=0; x<_.width; x++)); do
                ppm+="$sep${_.data[y][x]}"
                sep=" "
            done
            ppm+=$'\n'
        done
        print -- "$ppm"
    }
)

RGBColor_t color
Bitmap_t b=( width=3  height=2 )
b.fill "$(color.white)"
b.setpixel 0 0 "$(color.red)"
b.setpixel 1 0 "$(color.green)"
b.setpixel 2 0 "$(color.blue)"
b.setpixel 0 1 "$(color.yellow)"
b.setpixel 1 1 "$(color.white)"
b.setpixel 2 1 "$(color.black)"
echo "$(b.getpixel 0 0)"
b.to_s
```

Output:

```txt
255 0 0
P3
3 2
255
255 0 0 0 255 0 0 0 255
255 255 0 255 255 255 0 0 0
```


## Visual Basic .NET

```vbnet
' The StructLayout attribute allows fields to overlap in memory.
<System.Runtime.InteropServices.StructLayout(LayoutKind.Explicit)> _
Public Structure Rgb

    <FieldOffset(0)> _
    Public Rgb As Integer

    <FieldOffset(0)> _
    Public B As Byte

    <FieldOffset(1)> _
    Public G As Byte

    <FieldOffset(2)> _
    Public R As Byte

    Public Sub New(ByVal r As Byte, ByVal g As Byte, ByVal b As Byte)
        Me.R = r
        Me.G = g
        Me.B = b
    End Sub

End Structure
```


```vbnet
Public Class RasterBitmap

    Private m_pixels() As Rgb

    Private m_width As Integer
    Public ReadOnly Property Width As Integer
        Get
            Return m_width
        End Get
    End Property

    Private m_height As Integer
    Public ReadOnly Property Height As Integer
        Get
            Return m_height
        End Get
    End Property

    Public Sub New(ByVal width As Integer, ByVal height As Integer)
        m_pixels = New Rgb(width * height - 1) {}
        m_width = width
        m_height = height
    End Sub

    Public Sub Clear(ByVal color As Rgb)
        For i As Integer = 0 To m_pixels.Length - 1
            m_pixels(i) = color
        Next
    End Sub

    Public Sub SetPixel(ByVal x As Integer, ByVal y As Integer, ByVal color As Rgb)
        m_pixels((y * m_width) + x) = color
    End Sub

    Public Function GetPixel(ByVal x As Integer, ByVal y As Integer) As Rgb
        Return m_pixels((y * m_width) + x)
    End Function

End Class
```


## Vedit macro language

An edit buffer is used to store pixel data.
In order to allow unlimited image size,
a temporary file (here pixel.data) can be assosicated to the buffer.
You could directly open the image file you are creating
(as in the task [[Dragon_curve#Vedit_macro_language|Dragon_curve]],
but here we first create just the plain pixel data
so that the required image file format can be decided later.

```vedit
#11 = 400		// Width of the image
#12 = 300		// Height of the image

// Create an empty RGB image and fill it with black color
//
File_Open("|(VEDIT_TEMP)\pixel.data", OVERWRITE+NOEVENT)
BOF
Del_Char(ALL)
#10 = Buf_Num
Repeat(#11 * #12) {
    Ins_Char(0, COUNT, 3)
}

// Fill the image with dark blue color
//
#5 = 0				// Red
#6 = 0				// Green
#7 = 64				// Blue
Call("FILL_IMAGE")

// Draw one pixel in orange color
//
#1 = 100			// x
#2 = 50				// y
#5 = 255 #6 = 128 #7 = 0	// Orange color
Call("DRAW_PIXEL")

// Get the color of a pixel
//
#1 = 10
#2 = 3
Call("GET_COLOR")

Buf_Switch(#10) Buf_Quit(OK)
Return

/////////////////////////////////////////////////////////////////////
//
//  Fill image with given color: #5 = Red, #6 = Green, #7 = Blue
//
:FILL_IMAGE:
BOF
Repeat (File_Size/3) {
    IC(#5,OVERWRITE) IC(#6,OVERWRITE) IC(#7,OVERWRITE)
}
Return

/////////////////////////////////////////////////////////////////////
//
//  Daw a pixel. #1 = x, #2 = y
//
:DRAW_PIXEL:
Goto_Pos((#1 + #2*#11)*3)
IC(#5,OVERWRITE) IC(#6,OVERWRITE) IC(#7,OVERWRITE)
Return

/////////////////////////////////////////////////////////////////////
//
//  Get color of a pixel. #1 = x, #2 = y
//  Return: #5 = Red, #6 = Green, #7 = Blue
//
:GET_COLOR:
Goto_Pos((#1 + #2*#11)*3)
#5 = Cur_Char
#6 = Cur_Char(1)
#7 = Cur_Char(2)
Return
```


## Xojo

```vb
Function CreatePicture(width As Integer, height As Integer) As Picture
  Return New Picture(width, height)
End Function

Sub FillPicture(ByRef p As Picture, FillColor As Color)
  p.Graphics.ForeColor = FillColor
  p.Graphics.FillRect(0, 0, p.Width, p.Height)
End Sub

Function GetPixelColor(p As Picture, x As Integer, y As Integer) As Color
  Return p.RGBSurface.Pixel(x, y)
End Function

Sub SetPixelColor(p As Picture, x As Integer, y As Integer, pColor As Color)
  p.RGBSurface.Pixel(x, y) = pColor
End Sub

```


## XPL0

[[File:BitmapXPL0.png|right]]

```XPL0
include c:\cxpl\codes;  \include 'code' declarations
def Width=180, Height=135, Color=$123456;
int X, Y;
[SetVid($112);  \set display for 640x480 graphics in 24-bit RGB color
for Y:= 0 to Height-1 do    \fill area with Color one pixel at a time
    for X:= 0 to Width-1 do \(this takes 4.12 ms on a Duron 850)
        Point(X, Y, Color);
Move(60, 60); HexOut(6, ReadPix(0,0));  \show color of pixel at 0,0
X:= ChIn(1);            \wait for keystroke
SetVid(3);              \restore display to normal text mode
]
```


## zkl

This solution creates a PPM image. No error checking or clipping.

```zkl
class PPM{	// (0,0) is logically bottom left
   fcn init(width,height){
      sz:=width*height*3;
      var [const]
         data=sz.pump(Data(sz),0),  // initialize to Black (RGB=000)
	 w=width, h=height;
   }
   fcn fill(rgb){
      sz:=data.len()/3;
      data.clear(); sz.pump(data,T(Void,rgb.toBigEndian(3)));
   }
   fcn __sGet(x,y)    { data.toBigEndian(3*y*w + 3*x,3);        } //ppm[x,y]
   fcn __sSet(rbg,x,y){	data[3*y*w + 3*x,3]=rbg.toBigEndian(3); } //ppm[x,y]=rgb
   fcn write(out){   // write bottom to top to move (0,0) from bottom left to bottom left
      out.write("P6\n#rosettacode PPM\n%d %d\n255\n".fmt(w,h));
      [h-1..0, -1].pump(out,'wrap(h){ data.seek(3*h*w); data.read(3*w) });
      out.close();
   }
}
```

```zkl
ppm:=PPM(256,256);
ppm.fill(0x00FF88);
foreach x in ([50..200]){ ppm[x,50]=0xff|00|00; } // horizontal red line

ppm.write(File("foo.ppm","wb"));
```

Output:

```txt
$ zkl hexDump foo.ppm | less
   0: 50 36 0a 23 72 6f 73 65 | 74 74 61 63 6f 64 65 20   P6.#rosettacode
  16: 50 50 4d 0a 32 35 36 20 | 32 35 36 0a 32 35 35 0a   PPM.256 256.255.
  32: 00 ff 88 00 ff 88 00 ff | 88 00 ff 88 00 ff 88 00   ................
  48: ff 88 00 ff 88 00 ff 88 | 00 ff 88 00 ff 88 00 ff   ................
  64: 88 00 ff 88 00 ff 88 00 | ff 88 00 ff 88 00 ff 88   ................
  80: 00 ff 88 00 ff 88 00 ff | 88 00 ff 88 00 ff 88 00   ................
...
```

