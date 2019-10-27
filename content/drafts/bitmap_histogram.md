+++
title = "Bitmap/Histogram"
description = ""
date = 2018-06-28T09:15:27Z
aliases = []
[extra]
id = 3226
[taxonomies]
categories = []
tags = []
+++

{{task|Image processing}}

Extend the basic bitmap storage defined [[Basic_bitmap_storage|on this page]] to support dealing with image histograms. The image histogram contains for each luminance the count of image pixels having this luminance. Choosing a histogram representation take care about the data type used for the counts. It must have range of at least 0..NxM, where N is the image width and M is the image height.

'''Test task'''

Histogram is useful for many image processing operations. As an example, use it to convert an image into black and white art. The method works as follows:
* Convert image to grayscale;
* Compute the histogram
* Find the median: defined as the luminance such that the image has an approximately equal number of pixels with lesser and greater luminance.
* Replace each pixel of luminance lesser than the median to black, and others to white.
Use [[read ppm file | read]]/[[write ppm file]], and [[grayscale image]] solutions. 


## Ada

Histogram of an image:

```ada
type Pixel_Count is mod 2**64;
type Histogram is array (Luminance) of Pixel_Count;
   
function Get_Histogram (Picture : Grayscale_Image) return Histogram is
   Result : Histogram := (others => 0);
begin
   for I in Picture'Range (1) loop
      for J in Picture'Range (2) loop
         declare
            Count : Pixel_Count renames Result (Picture (I, J));
         begin
            Count := Count + 1;
         end;
      end loop;
   end loop;
   return Result;
end Get_Histogram;
```

Median of a histogram:

```ada
function Median (H : Histogram) return Luminance is
   From  : Luminance   := Luminance'First;
   To    : Luminance   := Luminance'Last;
   Left  : Pixel_Count := H (From);
   Right : Pixel_Count := H (To);
begin
   while From /= To loop
      if Left < Right then
         From := From + 1;
         Left := Left + H (From);
      else
         To    := To    - 1;
         Right := Right + H (To);         
      end if;
   end loop;
   return From;
end Median;
```

Conversion of an image to black and white art:

```ada
   F1, F2 : File_Type;
begin
   Open (F1, In_File, "city.ppm");
   declare
      X : Image := Get_PPM (F1);
      Y : Grayscale_Image := Grayscale (X);
      T : Luminance := Median (Get_Histogram (Y));
   begin
      Close (F1);
      Create (F2, Out_File, "city_art.ppm");
      for I in Y'Range (1) loop
         for J in Y'Range (2) loop
            if Y (I, J) < T then
               X (I, J) := Black;
            else
               X (I, J) := White;
            end if;
         end loop;
      end loop;      
      Put_PPM (F2, X);
   end;
   Close (F2);
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
[[Image:greyscale_bbc.jpg|right]]
[[Image:histogram_bbc.gif|right]]

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      Sort% = FN_sortinit(0,0)
      
      Width% = 200
      Height% = 200
      
      VDU 23,22,Width%;Height%;8,16,16,128
      *display c:\lenagrey
      
      DIM hist%(255), idx%(255)
      FOR i% = 0 TO 255 : idx%(i%) = i% : NEXT
      
      REM Build histogram:
      FOR y% = 0 TO Height%-1
        FOR x% = 0 TO Width%-1
          l% = FNgetpixel(x%,y%) AND &FF
          hist%(l%) += 1
        NEXT
      NEXT y%
      
      REM Sort histogram:
      C% = 256
      CALL Sort%, hist%(0), idx%(0)
      
      REM Find median:
      total% = SUM(hist%())
      half% = 0
      FOR i% = 0 TO 255
        half% += hist%(i%)
        IF half% >= total%/2 THEN
          median% = idx%(i%)
          EXIT FOR
        ENDIF
      NEXT
      
      REM Display black & white version:
      FOR y% = 0 TO Height%-1
        FOR x% = 0 TO Width%-1
          l% = FNgetpixel(x%,y%) AND &FF
          IF l% > median% THEN
            PROCsetpixel(x%,y%,255,255,255)
          ELSE
            PROCsetpixel(x%,y%,0,0,0)
          ENDIF
        NEXT
      NEXT y%
      END
      
      DEF PROCsetpixel(x%,y%,r%,g%,b%)
      COLOUR 1,r%,g%,b%
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



```c
typedef unsigned int histogram_t;
typedef histogram_t *histogram;

#define GET_LUM(IMG, X, Y) ( (IMG)->buf[ (Y) * (IMG)->width + (X)][0] )

histogram get_histogram(grayimage im);
luminance histogram_median(histogram h);
```



```c
histogram get_histogram(grayimage im)
{
   histogram t;
   unsigned int x, y;
   
   if ( im == NULL ) return NULL;
   t = malloc( sizeof(histogram_t)*256 );
   memset(t, 0, sizeof(histogram_t)*256 );
   if (t!=NULL)
   {
       for(x=0; x < im->width; x++ )
       {
         for(y=0; y < im->height; y++ )
         {
            t[ GET_LUM(im, x, y) ]++;
         }
       }
   }
   return t;
}
```


The given <tt>histogram</tt> must be freed with a simple <tt>free(histogram)</tt>.

{{trans|Ada}}


```c
luminance histogram_median(histogram h)
{
    luminance From, To;
    unsigned int Left, Right;
    
    From = 0; To = (1 << (8*sizeof(luminance)))-1;
    Left = h[From]; Right = h[To];
    
    while( From != To )
    {
       if ( Left < Right )
       {
          From++; Left += h[From];
       } else {
          To--; Right += h[To];
       }
    }
    return From;
}
```


An example of usage is the following code.


```c>#include <stdio.h

#include <stdlib.h>
#include "imglib.h"

/* usage example */

#define BLACK 0,0,0
#define WHITE 255,255,255

int main(int argc, char **argv)
{
    image color_img;
    grayimage g_img;
    histogram h;
    luminance T;
    unsigned int x, y;
    
    if ( argc < 2 )
    {
       fprintf(stderr, "histogram FILE\n");
       exit(1);
    }
    color_img = read_image(argv[1]);
    if ( color_img == NULL ) exit(1);
    g_img = tograyscale(color_img);
    h = get_histogram(g_img);
    if ( h != NULL )
    {
          T = histogram_median(h);
          
          for(x=0; x < g_img->width; x++)
          {
            for(y=0; y < g_img->height; y++)
            {
               if ( GET_LUM(g_img,x,y) < T )
               {
                   put_pixel_unsafe(color_img, x, y, BLACK);
               } else {
                   put_pixel_unsafe(color_img, x, y, WHITE);
               }
            }
          }
          output_ppm(stdout, color_img);
          /* print_jpg(color_img, 90); */
          free(h);
    }
       
    free_img((image)g_img);
    free_img(color_img);
}
```


Which reads from the file specified from the command line and outputs to the standard out the PPM B/W version of the input image. The input image can be of any format handled by ImageMagick (see [[Read image file through a pipe]])


## Common Lisp

{{libheader|opticl}}

```lisp
(defpackage #:histogram
  (:use #:cl
        #:opticl))

(in-package #:histogram)

(defun color->gray-image (image)
  (check-type image 8-bit-rgb-image)
  (let ((gray-image (with-image-bounds (height width) image
                      (make-8-bit-gray-image height width :initial-element 0))))
    (do-pixels (i j) image
      (multiple-value-bind (r g b) (pixel image i j)
        (let ((gray (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))
          (setf (pixel gray-image i j) (round gray)))))
    gray-image))

(defun make-histogram (image)
  (check-type image 8-bit-gray-image)
  (let ((histogram (make-array 256 :element-type 'fixnum :initial-element 0)))
    (do-pixels (i j) image
      (incf (aref histogram (pixel image i j))))
    histogram))

(defun find-median (histogram)
  (loop with num-pixels = (loop for count across histogram sum count)
        with half = (/ num-pixels 2)
        for count across histogram
        for i from 0
        sum count into acc
        when (>= acc half)
          return i))

(defun gray->black&white-image (image)
  (check-type image 8-bit-gray-image)
  (let* ((histogram (make-histogram image))
         (median (find-median histogram))
         (bw-image (with-image-bounds (height width) image
                     (make-1-bit-gray-image height width :initial-element 0))))
    (do-pixels (i j) image
      (setf (pixel bw-image i j) (if (<= (pixel image i j) median) 1 0)))
    bw-image))

(defun main ()
  (let* ((image (read-jpeg-file "lenna.jpg"))
         (bw-image (gray->black&white-image (color->gray-image image))))
    (write-pbm-file "lenna-bw.pbm" bw-image)))
```



## D

{{trans|Ada}}
It uses the grayscale_image from the Grayscale image Task. The loaded frog image is from the Color quantization Task.

```d
import grayscale_image;

Color findSingleChannelMedian(Color)(in Image!Color img)
pure nothrow @nogc if (Color.tupleof.length == 1) // Hack.
in {
    assert(img !is null);
} body {
    size_t[Color.max + 1] hist;
    foreach (immutable c; img.image)
        hist[c]++;

    // Slower indexes, but not significantly so.
    auto from = Color(0);
    auto to = Color(hist.length - 1);

    auto left = hist[from];
    auto right = hist[to];

    while (from != to)
        if (left < right) {
            from++;
            left += hist[from];
        } else {
            to--;
            right += hist[to];
        }

    return from;
}

Image!Color binarizeInPlace(Color)(Image!Color img,
                                   in Color thresh)
pure nothrow @nogc in {
    assert(img !is null);
} body {
    foreach (immutable i, ref c; img.image)
        c = (c < thresh) ? Color.min : Color.max;
    return img;
}

void main() {
    Image!RGB im;
    im.loadPPM6("quantum_frog.ppm");
    auto img = im.rgb2grayImage();
    img.binarizeInPlace(img.findSingleChannelMedian())
       .savePGM("quantum_frog_bin.pgm");
}
```



## FBSL

FBSL volatiles and function call concatenation used heavily for brevity.

'''24-bpp P.O.T.-size BMP solution:'''
[[File:FBSLHistogram.PNG|right]]

```qbasic
#DEFINE WM_CLOSE 16

DIM colored = ".\LenaClr.bmp", grayscale = ".\LenaGry.bmp", blackwhite = ".\LenaBnw.bmp"
DIM head, tail, r, g, b, l, m, ptr, blobsize = 54 ' sizeof BMP headers

FILEGET(FILEOPEN(colored, BINARY), FILELEN(colored)): FILECLOSE(FILEOPEN) ' fill buffer
head = @FILEGET + blobsize: tail = @FILEGET + FILELEN ' get buffer bounds

ToGrayScale() ' derive grayscale image and save it to disk
ToBlackAndWhite() ' ditto, black-and-white image

FBSLSETTEXT(ME, "Clr") ' display colored image
FBSLTILE(ME, FBSLLOADIMAGE(colored))
RESIZE(ME, 0, 0, 136, 162): CENTER(ME): SHOW(ME)

FBSLTILE(FBSLFORM("Gry"), FBSLLOADIMAGE(grayscale)) ' ditto, grayscale
RESIZE(FBSLFORM, 0, 0, 136, 162): CENTER(FBSLFORM): SHOW(FBSLFORM)

FBSLTILE(FBSLFORM("B/w"), FBSLLOADIMAGE(blackwhite)) ' ditto, black-and-white
RESIZE(FBSLFORM, 0, 0, 136, 162): CENTER(FBSLFORM): SHOW(FBSLFORM)

BEGIN EVENTS ' main message loop
	IF CBMSG = WM_CLOSE THEN DESTROY(ME) ' click any [X] button to quit
END EVENTS

SUB ToGrayScale()
	FOR ptr = head TO tail STEP 3
		b = PEEK(ptr + 0, 1) ' Windows stores colors in BGR order
		g = PEEK(ptr + 1, 1)
		r = PEEK(ptr + 2, 1)
		l = 0.2126 * r + 0.7152 * g + 0.0722 * b ' derive luminance
		POKE(ptr + 0, CHR(l))(ptr + 1, CHR)(ptr + 2, CHR) ' set pixel to shade of gray
		m = m + l
	NEXT
	FILEPUT(FILEOPEN(grayscale, BINARY_NEW), FILEGET): FILECLOSE(FILEOPEN) ' save grayscale image
END SUB

SUB ToBlackAndWhite()
	STATIC b = CHR(0), w = CHR(255) ' initialize once
	
	m = m / (tail - head) * 3 ' derive median
	FOR ptr = head TO tail STEP 3
		IF PEEK(ptr + 0, 1) < m THEN ' set pixel black
			POKE(ptr + 0, b)(ptr + 1, b)(ptr + 2, b)
		ELSE ' set pixel white
			POKE(ptr + 0, w)(ptr + 1, w)(ptr + 2, w)
		END IF
	NEXT
	FILEPUT(FILEOPEN(blackwhite, BINARY_NEW), FILEGET): FILECLOSE(FILEOPEN) ' save b/w image
END SUB
```



## Forth


```forth
: histogram ( array gmp -- )
  over 256 cells erase
  dup bdim * over bdata +  swap bdata
  do 1 over i c@ cells + +! loop drop ;
```



## Fortran

{{works with|Fortran|90 and later}}

'''Note''': ''luminance'' range is hard-encoded and is from 0 to 255. This could be enhanced.


```fortran
module RCImageProcess
  use RCImageBasic
  implicit none
contains

  subroutine get_histogram(img, histogram)
    type(scimage), intent(in) :: img
    integer, dimension(0:255), intent(out) :: histogram
    
    integer :: i

    histogram = 0
    do i = 0,255
       histogram(i) = sum(img%channel, img%channel == i)
    end do
  end subroutine get_histogram

  function histogram_median(histogram)
    integer, dimension(0:255), intent(in) :: histogram
    integer :: histogram_median
    
    integer :: from, to, left, right

    from = 0
    to = 255
    left = histogram(from)
    right = histogram(to)
    do while ( from /= to )
       if ( left < right ) then
          from = from + 1
          left = left + histogram(from)
       else
          to = to - 1
          right = right + histogram(to)
       end if
    end do
    histogram_median = from
  end function histogram_median
  
end module RCImageProcess
```


Example:


```fortran
program BasicImageTests
  use RCImageBasic
  use RCImageIO
  use RCImageProcess

  implicit none

  type(rgbimage) :: animage
  type(scimage) :: gray
  integer, dimension(0:255) :: histo
  integer :: ml

  open(unit=10, file='lenna.ppm', action='read', status='old')
  call read_ppm(10, animage)
  close(10)

  call init_img(gray)
  ! or
  ! call alloc_img(gray, animage%width, animage%height)

  gray = animage

  call get_histogram(gray, histo)
  ml = histogram_median(histo)
  where ( gray%channel >= ml )
     animage%red = 255
     animage%green = 255
     animage%blue = 255
  elsewhere
     animage%red = 0
     animage%green = 0
     animage%blue = 0
  end where

  open(unit=10, file='elaborated.ppm', action='write')
  call output_ppm(10, animage)
  close(10)

  call free_img(animage)
  call free_img(gray)

end program BasicImageTests
```



## Go

Histogram and Threshold functions are be added to the Grmap type for this task:

```go
package raster

import "math"

func (g *Grmap) Histogram(bins int) []int {
    if bins <= 0 {
        bins = g.cols
    }
    h := make([]int, bins)
    for _, p := range g.px {
        h[int(p)*(bins-1)/math.MaxUint16]++
    }
    return h
}

func (g *Grmap) Threshold(t uint16) {
    for i, p := range g.px {
        if p < t {
            g.px[i] = 0
        } else {
            g.px[i] = math.MaxUint16
        }
    }
}
```

Demonstration program computes the median:

```go
package main

// Files required to build supporting package raster are found in:
// * This task (immediately above)
// * Bitmap
// * Grayscale image
// * Read a PPM file
// * Write a PPM file

import (
    "raster"
    "fmt"
    "math"
)

func main() {
    // (A file with this name is output by the Go solution to the task
    // "Bitmap/Read an image through a pipe," but of course any 8-bit
    // P6 PPM file should work.)
    b, err := raster.ReadPpmFile("pipein.ppm")
    if err != nil {
        fmt.Println(err)
        return
    }
    g := b.Grmap()
    h := g.Histogram(0)
    // compute median
    lb, ub := 0, len(h)-1
    var lSum, uSum int
    for lb <= ub {
        if lSum+h[lb] < uSum+h[ub] {
            lSum += h[lb]
            lb++
        } else {
            uSum += h[ub]
            ub--
        }
    }
    // apply threshold and write output file
    g.Threshold(uint16(ub * math.MaxUint16 / len(h)))
    err = g.Bitmap().WritePpmFile("threshold.ppm")
    if err != nil {
        fmt.Println(err)
    }
}
```



## Haskell

First, an implementation of a black-and-white instance of <tt>Color</tt>. For simplicty, we use ASCII PBM for output instead of the raw format.

```haskell
module Bitmap.BW(module Bitmap.BW) where

import Bitmap
import Control.Monad.ST

newtype BW = BW Bool deriving (Eq, Ord)

instance Color BW where
    luminance (BW False) = 0
    luminance _          = 255
    black = BW False
    white = BW True
    toNetpbm [] = ""
    toNetpbm l = init (concatMap f line) ++ "\n" ++ toNetpbm rest
      where (line, rest) = splitAt 35 l
            f (BW False) = "1 "
            f _          = "0 "
    fromNetpbm = map f
      where f 1 = black
            f _ = white
    netpbmMagicNumber _ = "P1"
    netpbmMaxval _ = ""

toBWImage :: Color c => Image s c -> ST s (Image s BW)
toBWImage = toBWImage' 128

toBWImage' :: Color c => Int -> Image s c -> ST s (Image s BW)
{- The first argument gives the darkest luminance assigned
to white. -}
toBWImage' darkestWhite = mapImage $ f . luminance
  where f x | x < darkestWhite = black
            | otherwise        = white
```


Every instance of <tt>Color</tt> has a <tt>luminance</tt> method, so we don't need to convert an image to <tt>Gray</tt> to calculate its histogram.

```haskell
import Bitmap
import Bitmap.RGB
import Bitmap.BW
import Bitmap.Netpbm
import Control.Monad.ST
import Data.Array

main = do
    i <- readNetpbm "original.ppm" :: IO (Image RealWorld RGB)
    writeNetpbm "bw.pbm" =<< stToIO (do
        h <- histogram i
        toBWImage' (medianIndex h) i)

histogram :: Color c => Image s c -> ST s [Int]
histogram = liftM f . getPixels where
    f = elems . accumArray (+) 0 (0, 255) . map (\i -> (luminance i, 1))

medianIndex :: [Int] -> Int
{- Given a list l, finds the index i that minimizes
  abs $ sum (take i l) - sum (drop i l) -}
medianIndex l = result
  where (result, _, _, _, _) =
            iterate f (0, 0, 0, l, reverse l) !! (length l - 1)
        f (n, left, right, lL@(l : ls), rL@(r : rs)) =
            if   left < right
            then (n + 1, left + l, right,     ls, rL)
            else (n,     left,     right + r, lL, rs)
```



## J

'''Solution:'''

Using <code>toGray</code> from [[Grayscale image#J|Grayscale image]].

```j
getImgHist=: ([: /:~ ~. ,. #/.~)@,
medianHist=: {."1 {~ [: (+/\ I. -:@(+/)) {:"1
toBW=: 255 * medianHist@getImgHist < toGray
```


'''Example Usage:'''

Use [http://rosettacode.org/mw/images/b/b6/Lenna100.jpg Lenna100.jpg] for testing (read using the [[j:Addons/media/platimg|media/platimg]] addon and convert to ppm file).


```j
   require 'media/platimg'
   'Lenna100.ppm' writeppm~ 256#.inv readimg 'Lenna100.jpg'
786447
```


Read ppm file, convert to black and white and write to a new ppm file using <code>writeppm</code>, <code>readppm</code> and <code>toColor</code> from the [[read ppm file#J | read]]/[[write ppm file#J|write ppm file]], and [[grayscale image#J|grayscale image]] solutions.

```j
   'Lenna100BW.ppm' writeppm~ toColor toBW readppm 'Lenna100.ppm'
786447
```



## Java


```Java
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;

public enum ImageProcessing {
    ;

    public static void main(String[] args) throws IOException {
        BufferedImage img = ImageIO.read(new File("example.png"));

        BufferedImage bwimg = toBlackAndWhite(img);

        ImageIO.write(bwimg, "png", new File("example-bw.png"));
    }

    private static int luminance(int rgb) {
        int r = (rgb >> 16) & 0xFF;
        int g = (rgb >> 8) & 0xFF;
        int b = rgb & 0xFF;
        return (r + b + g) / 3;
    }

    private static BufferedImage toBlackAndWhite(BufferedImage img) {
        int width = img.getWidth();
        int height = img.getHeight();

        int[] histo = computeHistogram(img);

        int median = getMedian(width * height, histo);

        BufferedImage bwimg = new BufferedImage(width, height, img.getType());
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                bwimg.setRGB(x, y, luminance(img.getRGB(x, y)) >= median ? 0xFFFFFFFF : 0xFF000000);
            }
        }
        return bwimg;
    }

    private static int[] computeHistogram(BufferedImage img) {
        int width = img.getWidth();
        int height = img.getHeight();

        int[] histo = new int[256];
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                histo[luminance(img.getRGB(x, y))]++;
            }
        }
        return histo;
    }

    private static int getMedian(int total, int[] histo) {
        int median = 0;
        int sum = 0;
        for (int i = 0; i < histo.length && sum + histo[i] < total / 2; i++) {
            sum += histo[i];
            median++;
        }
        return median;
    }
}
```



## Julia

{{works with|Julia|0.6}}


```julia
using Images, FileIO

ima = load("data/lenna50.jpg")
imb = Gray.(ima)

medcol = median(imb)
imb[imb .≤ medcol] = Gray(0.0)
imb[imb .> medcol] = Gray(1.0)
save("data/lennaGray.jpg", imb)
```



## Kotlin

Uses the image from the [[Percentage difference between images]] task as an example.

```scala
// version 1.2.10

import java.io.File
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

const val BLACK = 0xff000000.toInt()
const val WHITE = 0xffffffff.toInt()

fun luminance(argb: Int): Int {
    val red   = (argb shr 16) and 0xFF
    val green = (argb shr  8) and 0xFF
    val blue  =  argb and 0xFF
    return (0.2126 * red + 0.7152 * green + 0.0722 * blue).toInt()
}

val BufferedImage.histogram: IntArray
    get() {
        val lumCount = IntArray(256)
        for (x in 0 until width) {
            for (y in 0 until height) {
                var argb = getRGB(x, y)
                lumCount[luminance(argb)]++
            }
        }
        return lumCount
    }

fun findMedian(histogram: IntArray): Int {
    var lSum  = 0
    var rSum  = 0
    var left  = 0
    var right = 255
    do {
        if (lSum < rSum) lSum += histogram[left++]
        else             rSum += histogram[right--]
    }
    while (left != right)
    return left
}

fun BufferedImage.toBlackAndWhite(median: Int) {
    for (x in 0 until width) {
        for (y in 0 until height) {
            val argb = getRGB(x, y)
            val lum  = luminance(argb)
            if (lum < median)
                setRGB(x, y, BLACK)
            else
                setRGB(x, y, WHITE)
        }
    }
}

fun main(args: Array<String>) {
    val image = ImageIO.read(File("Lenna100.jpg"))
    val median = findMedian(image.histogram)
    image.toBlackAndWhite(median)
    val bwFile = File("Lenna_bw.jpg")
    ImageIO.write(image, "jpg", bwFile)
}
```



## Lua

This solution uses functions defined at:
[[Read ppm file#Lua]],
[[Write ppm file#Lua]],
[[Basic bitmap storage#Lua]],
[[Grayscale image#Lua]].

```lua
function Histogram( image )
    local size_x, size_y = #image, #image[1]
    
    local histo = {}
    for i = 0, 255 do
        histo[i] = 0
    end
    
    for i = 1, size_x do
        for j = 1, size_y do
            histo[ image[i][j] ] = histo[ image[i][j] ] + 1 
        end
    end
    
    return histo
end

function FindMedian( histogram )
    local sum_l, sum_r = 0, 0
    local left, right = 0, 255
    
    repeat
        if sum_l < sum_r then
            sum_l = sum_l + histogram[left]
            left = left + 1
        else
            sum_r = sum_r + histogram[right]
            right = right - 1
        end
    until left == right
    
    return left
end


bitmap = Read_PPM( "inputimage.ppm" )
gray_im = ConvertToGrayscaleImage( bitmap )
histogram = Histogram( gray_im )
median = FindMedian( histogram )

for i = 1, #gray_im do
    for j = 1, #gray_im[1] do
        if gray_im[i][j] < median then
            gray_im[i][j] = 0
        else
            gray_im[i][j] = 255
        end
    end
end

bitmap = ConvertToColorImage( gray_im ) 
Write_PPM( "outputimage.ppm", bitmap )
```



## Mathematica


```Mathematica

ImageLevels[img];

```



## OCaml

{{Trans|C}}


```ocaml
type histogram = int array

let get_histogram ~img:gray_channel =
  let width = Bigarray.Array2.dim1 gray_channel
  and height = Bigarray.Array2.dim2 gray_channel in
  let t = Array.make 256 0 in
  for x = 0 to pred width do
    for y = 0 to pred height do
      let v = gray_get_pixel_unsafe gray_channel x y in
      t.(v) <- t.(v) + 1;
    done;
  done;
  (t: histogram)
;;
```



```ocaml
let histogram_median (h : histogram) =

  let from = 0 and to_ = 255 in
  let left = h.(from) and right = h.(to_) in

  let rec aux from to_ left right =
    if from = to_
    then (from)
    else
      if left < right
      then aux (succ from) to_ (left + h.(from)) right
      else aux from (pred to_) left (right + h.(to_))
  in
  aux from to_ left right
;;
```


main:

```ocaml
let () =
  let img = read_ppm ~filename:"/tmp/foo.ppm" in

  let width, height = get_dims img in
  let res = new_img ~width ~height in

  let g_img = to_grayscale ~img in
  let h = get_histogram g_img in
  let m = histogram_median h in

  let light = (255, 255, 0)
  and dark = (127, 0, 127) in

  for x = 0 to pred width do
    for y = 0 to pred height do
      let v = gray_get_pixel_unsafe g_img x y in
      if v > m
      then put_pixel_unsafe res light x y
      else put_pixel_unsafe res dark x y
    done;
  done;

  output_ppm ~oc:stdout ~img:res;
;;
```



## Octave

'''Using package''' [http://octave.sourceforge.net/image/index.html Image]

```octave
function h = imagehistogram(imago)
  if ( isgray(imago) )
    for j = 0:255
      h(j+1) = numel(imago( imago == j ));
    endfor
  else
    error("histogram on gray img only");
  endif
endfunction

% test
im = jpgread("Lenna100.jpg");
img = rgb2gray(im);
h = imagehistogram(img);
% let's try to show the histogram
bar(h);
pause;

% in order to obtain the requested filtering, we
% can use median directly on the img, and then
% use that value, this way:
m = median(reshape(img, 1, numel(img)));
disp(m);
ibw = img;
ibw( img > m ) = 255;
ibw( img <= m ) = 0;
jpgwrite("lennamed_.jpg", ibw, 100);
% which disagree (128) with the m computed with histog_med (130).
% If we compute it this way:
% m = sort(reshape(img, 1, numel(img)))(ceil(numel(img)/2));
% we obtain 130... but builtin median works as expected, since
% N (number of pixel of Lenna) is even, not odd.

% but let's use our histogram h instead
function m = histog_med(histog)
  from = 0; to = 255;
  left = histog(from + 1); right = histog(to+1);
  while ( from != to )
    if ( left < right ) 
      from++; left += histog(from+1);
    else
      to--; right += histog(to+1);
    endif
  endwhile
  m = from;
endfunction

m = histog_med(h);
disp(m);
ibw( img > m ) = 255;
ibw( img <= m ) = 0;
jpgwrite("lennamed.jpg", ibw, 100);
```



## Perl 6

{{works with|Rakudo|2017.09}}
Uses pieces from [[Bitmap#Perl_6| Bitmap]], [[Bitmap/Write_a_PPM_file#Perl_6| Write a PPM file]] and [[Grayscale_image#Perl_6| Grayscale image]] tasks. Included here to make a complete, runnable program.


```perl6
class Pixel { has UInt ($.R, $.G, $.B) }
class Bitmap {
    has UInt ($.width, $.height);
    has Pixel @.data;
}

role PBM {
    has @.BM;
    method P4 returns Blob {
	"P4\n{self.width} {self.height}\n".encode('ascii')
	~ Blob.new: self.BM
    }
}

sub getline ( $fh ) {
    my $line = '#'; # skip comments when reading image file
    $line = $fh.get while $line.substr(0,1) eq '#';
    $line;
}

sub load-ppm ( $ppm ) {
    my $fh    = $ppm.IO.open( :enc('ISO-8859-1') );
    my $type  = $fh.&getline;
    my ($width, $height) = $fh.&getline.split: ' ';
    my $depth = $fh.&getline;
    Bitmap.new( width => $width.Int, height => $height.Int,
      data => ( $fh.slurp.ords.rotor(3).map:
        { Pixel.new(R => $_[0], G => $_[1], B => $_[2]) } )
    )
}

sub grayscale ( Bitmap $bmp ) {
    map { (.R*0.2126 + .G*0.7152 + .B*0.0722).round(1) min 255 }, $bmp.data;
}

sub histogram ( Bitmap $bmp ) {
    my @gray = grayscale($bmp);
    my $threshold = @gray.sum / @gray;
    for @gray.rotor($bmp.width) {
        my @row = $_.list;
        @row.push(0) while @row % 8;
        $bmp.BM.append: @row.rotor(8).map: { :2(($_ X< $threshold)».Numeric.join) }
    }
}

my $filename = './Lenna.ppm';

my Bitmap $b = load-ppm( $filename ) but PBM;

histogram($b);

'./Lenna-bw.pbm'.IO.open(:bin, :w).write: $b.P4;
```


See [https://github.com/thundergnat/rc/blob/master/img/Lenna.png Lenna], and [https://github.com/thundergnat/rc/blob/master/img/Lenna-bw.png Lenna-bw] images. (converted to .png as .ppm format is not widely supported).


## Phix

Requires read_ppm() from [[Bitmap/Read_a_PPM_file#Phix|Read_a_PPM_file]], write_ppm() from [[Bitmap/Write_a_PPM_file#Phix|Write_a_PPM_file]]. 
Uses lena.ppm, which you will have to find/download to demo/rosetta yourself.
Included as demo\rosetta\Bitmap_Histogram.exw, results may be verified with demo\rosetta\viewppm.exw

```Phix
function to_bw(sequence image)
sequence color
integer lum
sequence hist = repeat(0,256) 
integer l = 1, r = 256
integer ltot, rtot
    for i=1 to length(image) do
        for j=1 to length(image[i]) do
            color = sq_div(sq_and_bits(image[i][j], {#FF0000,#FF00,#FF}),
                                                    {#010000,#0100,#01})
            lum = floor(0.2126*color[1] + 0.7152*color[2] + 0.0722*color[3])
            image[i][j] = lum
            hist[lum+1] += 1
        end for 
    end for 
    ltot = hist[l]
    rtot = hist[r]
    while l!=r do
        if ltot<rtot then
            l += 1
            ltot += hist[l]
        else
            r -= 1
            rtot += hist[r]
        end if
    end while
    lum = l
    for i=1 to length(image) do
        for j=1 to length(image[i]) do
            image[i][j] = iff(image[i][j]<lum?black:white)
        end for 
    end for 
    return image
end function

sequence img = read_ppm("Lena.ppm")
    img = to_bw(img)
    write_ppm("LenaBW.ppm",img)
```



## PHP



```PHP

define('src_name', 'input.jpg');	// source image
define('dest_name', 'output.jpg');	// destination image

$img = imagecreatefromjpeg(src_name);	// read image

if(empty($img)){
	echo 'Image could not be loaded!'; 
	exit; 
}

$black = imagecolorallocate($img, 0, 0, 0);
$white = imagecolorallocate($img, 255, 255, 255);
$width = imagesx($img);
$height = imagesy($img);

$array_lum = array(); 	// for storage of luminosity of each pixel
$sum_lum = 0;		// total sum of luminosity
$average_lum = 0;	// average luminosity of whole image

for($x = 0; $x < $width; $x++){	
	for($y = 0; $y < $height; $y++){
		// read pixel value
		$color = imagecolorat($img, $x, $y);
		$r = ($color >> 16) & 0xFF;
		$g = ($color >> 8) & 0xFF;
		$b = $color & 0xFF;
		// save pixel luminosity in temporary array
		$array_lum[$x][$y] = ($r + $g + $b);
		// add pixel luminosity to sum
		$sum_lum += $array_lum[$x][$y];
	}
}

// calculate average luminosity
$average_lum = $sum_lum / ($width * $height);

for($x = 0; $x < $width; $x++){	
	for($y = 0; $y < $height; $y++){
		// pixel is brighter than average -> set white
		// else -> set black
		if($array_lum[$x][$y] > $average_lum){
			imagesetpixel($img, $x, $y, $white);
		}
		else{
			imagesetpixel($img, $x, $y, $black);
		}
	}
}
// save black and white image to dest_name
imagejpeg($img, dest_name);

if(!file_exists(dest_name)){
	echo 'Image not saved! Check permission!';
}

```

Example: 

<div>
[[File:Input.jpg|200px|thumb|left]]
[[File:Output.jpg|200px|thumb|center]]
</div>


<div>
<i style="text-align: center;">
The Image on the left is read in and the average luminosity calculated.

Every pixel darker than average is painted black; brighter painted white.

The black and white image on the right is then saved to the file system.

</i>
</div>


## PicoLisp

{{trans|Forth}}

```PicoLisp
(de histogram (Pgm)
   (let H (need 256 0)
      (for L Pgm
         (for G L
            (inc (nth H (inc G))) ) )
      H ) )
```



## PureBasic

Also requires PureBasic solutions for [[Bitmap/Read_a_PPM_file#PureBasic|Read a PPM file]], [[Grayscale_image#PureBasic|Grayscale image]], and [[Bitmap/Write_a_PPM_file#PureBasic|Write a PPM file]].

```PureBasic
Procedure getHistogram(image, Array histogram(1))
  Protected w = ImageWidth(image) - 1
  Protected h = ImageHeight(image) - 1
  Dim histogram(255) ;output
  
  StartDrawing(ImageOutput(image))
    For x = 0 To w
      For y = 0 To h 
        lum = Red(Point(x, y)) ;the Green or Blue color components could be used also
        histogram(lum) + 1
      Next
    Next
  StopDrawing()
EndProcedure

Procedure median(Array histogram(1))
  Protected low, high = 255, left, right
  
  While low <> high
    If left < right
      low + 1
      left + histogram(low)
    Else
      high - 1
      right + histogram(high)         
    EndIf
  Wend
  ProcedureReturn low
EndProcedure

Procedure blackAndWhite(image, median)
  Protected w = ImageWidth(image) - 1
  Protected h = ImageHeight(image) - 1
  CallDebugger
  StartDrawing(ImageOutput(image))
    For x = 0 To w
      For y = 0 To h
        If Red(Point(x, y)) < median ;the Green or Blue color components could be used also
          Plot(x, y, $000000) ;black
        Else
          Plot(x, y, $FFFFFF) ;white
        EndIf
      Next
    Next
  StopDrawing()
EndProcedure

Define sourceFile.s, outputFile.s, image = 3, m
Dim histogram(255)

sourceFile = OpenFileRequester("Select source image file", "*.ppm", "PPM image (*.ppm)|PPM", 0)

If sourceFile And LCase(GetExtensionPart(sourceFile)) = "ppm"
  LoadImagePPM(image, sourceFile)
  ImageGrayout(image)
  
  getHistogram(image,histogram())
  m = median(histogram())
  blackAndWhite(image, m)
  
  outputFile = Left(sourceFile, Len(sourceFile) - Len(GetExtensionPart(sourceFile))) + "_bw." + GetExtensionPart(sourceFile)
  SaveImageAsPPM(image, outputFile, 1)
EndIf
```



## Python

Makes use of the Pillow library (PIL) you can install it using pip. The code is probably not the fastest or the image I used (1960x1960) is just too big.

```python
from PIL import Image

# Open the image
image = Image.open("lena.jpg")
# Get the width and height of the image
width, height = image.size
# Calculate the amount of pixels
amount = width * height

# Total amount of greyscale
total = 0
# B/W image
bw_image = Image.new('L', (width, height), 0)
# Bitmap image
bm_image = Image.new('1', (width, height), 0)

for h in range(0, height):
    for w in range(0, width):
        r, g, b = image.getpixel((w, h))

        greyscale = int((r + g + b) / 3)
        total += greyscale

        bw_image.putpixel((w, h), gray_scale)

# The average greyscale
avg = total / amount

black = 0
white = 1

for h in range(0, height):
    for w in range(0, width):
        v = bw_image.getpixel((w, h))

        if v >= avg:
            bm_image.putpixel((w, h), white)
        else:
            bm_image.putpixel((w, h), black)

bw_image.show()
bm_image.show()
```



## Racket


```racket
 #lang racket
(require racket/draw math/statistics racket/require
         (filtered-in
          (lambda (name) (regexp-replace #rx"unsafe-" name ""))
          racket/unsafe/ops))

;; CIE formula as discussed in "Greyscale image" task
(define (L r g b)
  ;; In fact there is no need, statistically for L to be divided by 10000
  (fx+ (fx* r 2126) (fx+ (fx* g 7152) (fx* b 722))))

(define (prepare-bytes bm depth load-content?)
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define rv (make-bytes (* w h depth)))
  (define just-alpha? #f)
  (define pre-multiply? #t); let racket cope with alpha-ness
  (when load-content? (send bm get-argb-pixels 0 0 w h rv just-alpha? pre-multiply?))
  rv)

(define (bitmap-histogram bm)
  (unless (send bm is-color?) (error 'bitmap->histogram "bitmap must be colour"))
  (define pxls (prepare-bytes bm 4 #t))
  (define l# (make-hash))
  (for ((r (in-bytes pxls 1 #f 4)) (g (in-bytes pxls 2 #f 4)) (b (in-bytes pxls 3 #f 4)))
    (hash-update! l# (L r g b) add1 0))
  (define xs (hash-keys l#))   ; the colour values
  (define ws (hash-values l#)) ; the "weights" i.e. counts for median
  (values xs ws))

(define (bitmap-quantile q bm (hist-xs #f) (hist-ws #f))
  (define-values (xs ws) (if (and hist-xs hist-ws)
                             (values hist-xs hist-ws)
                             (bitmap-histogram bm)))
  (quantile q < xs ws))

;; we don't return a 1-depth bitmap, so we can do more interesting things with colour
(define (bitmap->monochrome q bm (hist-xs #f) (hist-ws #f))
  (define Q (bitmap-quantile q bm hist-xs hist-ws))
  (define pxls (prepare-bytes bm 4 #t))
  (for ((r (in-bytes pxls 1 #f 4))
        (g (in-bytes pxls 2 #f 4))
        (b (in-bytes pxls 3 #f 4))
        (i (sequence-map (curry fx* 4) (in-naturals))))
    (define l (L r g b))
    (define rgb+ (cond [(fx< l Q) 0] [else 255]))
    (bytes-set! pxls (fx+ i 1) rgb+)
    (bytes-set! pxls (fx+ i 2) rgb+)
    (bytes-set! pxls (fx+ i 3) rgb+))
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define rv (make-bitmap w h #f))
  (send rv set-argb-pixels 0 0 w h pxls)
  rv)

(module+ main
  (define bm (read-bitmap "271px-John_Constable_002.jpg"))
  (define-values (xs ws) (bitmap-histogram bm))
  (void
   (send (bitmap->monochrome 1/4 bm) save-file "histogram-racket-0.25.png" 'png)
   (send (bitmap->monochrome 1/2 bm) save-file "histogram-racket-0.50.png" 'png) ; median
   (send (bitmap->monochrome 3/4 bm xs ws) save-file "histogram-racket-0.75.png" 'png)))
```


{{out}}
[http://timb.net/images/RC/Bitmap/Histogram/271px-John_Constable_002.jpg Original Image],
[http://timb.net/images/RC/Bitmap/Histogram/histogram-racket-0.25.png 25% image],
[http://timb.net/images/RC/Bitmap/Histogram/histogram-racket-0.50.png 50% image],
[http://timb.net/images/RC/Bitmap/Histogram/histogram-racket-0.75.png 75% image]

Sorry guys... I just give up on linking/displaying these images any other way!


## Ruby


```ruby
class Pixmap
  def histogram
    histogram = Hash.new(0)
    @height.times do |y|
      @width.times do |x|
        histogram[self[x,y].luminosity] += 1
      end
    end
    histogram 
  end

  def to_blackandwhite
    hist = histogram

    # find the median luminosity
    median = nil
    sum = 0
    hist.keys.sort.each do |lum|
      sum += hist[lum]
      if sum > @height * @width / 2
        median = lum
        break
      end
    end

    # create the black and white image
    bw = self.class.new(@width, @height)
    @height.times do |y|
      @width.times do |x|
        bw[x,y] = self[x,y].luminosity < median ? RGBColour::BLACK : RGBColour::WHITE
      end
    end
    bw
  end

  def save_as_blackandwhite(filename)
    to_blackandwhite.save(filename)
  end
end

Pixmap.open('file.ppm').save_as_blackandwhite('file_bw.ppm')
```



## Scala

See also
* [[Basic_bitmap_storage#Scala|Basic Bitmap Storage]] for RgbBitmap class
* [[Grayscale_image#Scala|Grayscale Bitmap Task]] for luminosity method
* [[Read_ppm_file#Scala|Read a PPM File]] image loading


```scala
object BitmapOps {
   def histogram(bm:RgbBitmap)={
      val hist=new Array[Int](255)
      for(x <- 0 until bm.width; y <- 0 until bm.height; l=luminosity(bm.getPixel(x,y)))
         hist(l)+=1
      hist
   }

   def histogram_median(hist:Array[Int])={
      var from=0
      var to=hist.size-1
      var left=hist(from)
      var right=hist(to)

      while(from!=to){
         if (left<right)
            {from+=1; left+=hist(from)}
         else
            {to-=1; right+=hist(to)}
      }
      from
   }

   def monochrom(bm:RgbBitmap, threshold:Int)={
      val image=new RgbBitmap(bm.width, bm.height)
      val c1=Color.BLACK
      val c2=Color.WHITE
      for(x <- 0 until bm.width; y <- 0 until bm.height; l=luminosity(bm.getPixel(x,y)))
         image.setPixel(x, y, if(l>threshold) c2 else c1)
      image		
   }
}
```


Usage:

```scala
val img=Pixmap.load("image.ppm").get
val hist=BitmapOps.histogram(img)
val mid=BitmapOps.histogram_median(hist);

val mainframe=new MainFrame(){
   title="Test"
   visible=true
   contents=new Label(){
      icon=new ImageIcon(BitmapOps.monochrom(img, mid).image)
   }
}
```



## Tcl

{{libheader|Tk}}
Uses [[read ppm file#Tcl|readPPM]], [[grayscale image#Tcl|grayscale]] and [[write ppm file#Tcl|output_ppm]] from other pages.

```tcl
package require Tcl 8.5
package require Tk

proc convert_to_blackandwhite {filename} {
    set img [image create photo]
    readPPM $img $filename
    grayscale $img
    set hist [histogram $img]
    set median [median $img $hist]
    blackandwhite $img $median
    output_ppm $img bw_$filename
}

proc histogram {image} {
    set hist [dict create]
    for {set x 0} {$x < [image width $image]} {incr x} {
        for {set y 0} {$y < [image height $image]} {incr y} {
            dict incr hist [luminance {*}[$image get $x $y]]
        }
    }
    return $hist
}

proc luminance {r g b} {
    expr {
        int(0.2126*$r + 0.7152*$g + 0.0722*$b)
    }
}

proc median {img hist} {
    set sum [expr {[image width $img] * [image height $img]}]
    set total 0
    foreach luminance [lsort -integer [dict keys $hist]] {
        incr total [dict get $hist $luminance]
        if {$total > $sum / 2} break
    }
    return $luminance
}

proc blackandwhite {image median} {
    for {set x 0} {$x < [image width $image]} {incr x} {
        for {set y 0} {$y < [image height $image]} {incr y} {
            if {[luminance {*}[$image get $x $y]] < $median} {
                $image put black -to $x $y
            } else {
                $image put white -to $x $y
            }
        }
    }
}
```



## Vedit macro language

The input image is in edit buffer pointed by numeric register #15.
On return, #30 points to buffer containing histogram data.
The histogram data is given as ASCII decimal values, one value per line.

```vedit
:HISTOGRAM:
#30 = Buf_Free			     // #30 = buffer to store histogram data
for (#9=0; #9<256; #9++) {
    Out_Reg(21) TC(#9) Out_Reg(Clear)		// @21 = intensity value to be counted
    Buf_Switch(#15)				// switch to image buffer
    #8 = Search(@21, CASE+BEGIN+ALL+NOERR)	// count intensity values
    Buf_Switch(#30)				// switch to histogram buffer
    Num_Ins(#8, FILL)				// store count
}
Return
```



## zkl

{{trans|C}}
Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
fcn histogram(image){
   hist:=List.createLong(256,0);	// array[256] of zero
   image.data.howza(0).pump(Void,'wrap(c){ hist[c]+=1 });  // byte by byte loop
   hist;
}
fcn histogramMedian(hist){
   from,to:=0,(2).pow(8) - 1; // 16 bits of luminance
   left,right:=hist[from],hist[to];
   while(from!=to){
      if(left<right){ from+=1; left +=hist[from]; }
      else 	    { to  -=1; right+=hist[to];   }
   }
   from
}
```


```zkl
img:=PPM.readPPMFile("lenaGrey.ppm"); // a grey scale image
median:=histogramMedian(histogram(img));
median.println();

bw:=PPM(img.w,img.h);
  // stream bytes from orginal, convert to black/white, write to new image
  // each pixel is 24 bit RGB
img.data.pump(bw.data.clear(),'wrap(c){ if(c>median) 0xff else 0  });

bw.write(File("foo.ppm","wb"));
```

{{out}}
```txt
101
```

See the BBC Basic entry or:
http://www.zenkinetic.com/Images/RosettaCode/lenaBW.jpg

{{omit from|AWK}}
{{omit from|PARI/GP}}
