+++
title = "Median filter"
description = ""
date = 2019-10-12T21:21:10Z
aliases = []
[extra]
id = 3230
[taxonomies]
categories = ["task", "Image processing"]
tags = []
languages = [
  "ada",
  "bbc_basic",
  "c",
  "d",
  "gdl",
  "go",
  "j",
  "julia",
  "kotlin",
  "mathematica",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "ruby",
  "tcl",
  "zkl",
]
+++

## Task

The median filter takes in the neighbourhood the median color (see [[wp:Median filter|Median filter]])

''(to test the function below, you can use these [[Read_ppm_file|input]] and [[Write_ppm_file|output]] solutions)''


## Ada


```ada
function Median (Picture : Image; Radius : Positive) return Image is
   type Extended_Luminance is range 0..10_000_000;
   type VRGB is record
      Color : Pixel;
      Value : Luminance;
   end record;
   Width : constant Positive := 2*Radius*(Radius+1);
   type Window is array (-Width..Width) of VRGB;
   Sorted : Window;
   Next   : Integer;

   procedure Put (Color : Pixel) is -- Sort using binary search
      pragma Inline (Put);
      This   : constant Luminance :=
                  Luminance
                  (  (  2_126 * Extended_Luminance (Color.R)
                     +  7_152 * Extended_Luminance (Color.G)
                     +    722 * Extended_Luminance (Color.B)
                     )
                  /  10_000
                  );
      That   : Luminance;
      Low    : Integer := Window'First;
      High   : Integer := Next - 1;
      Middle : Integer := (Low + High) / 2;
   begin
      while Low <= High loop
         That   := Sorted (Middle).Value;
         if That > This then
            High := Middle - 1;
         elsif That < This then
            Low := Middle + 1;
         else
            exit;
         end if;
         Middle := (Low + High) / 2;
      end loop;
      Sorted (Middle + 1..Next) := Sorted (Middle..Next - 1);
      Sorted (Middle) := (Color, This);
      Next := Next + 1;
   end Put;
   Result : Image (Picture'Range (1), Picture'Range (2));
begin
   for I in Picture'Range (1) loop
      for J in Picture'Range (2) loop
         Next := Window'First;
         for X in I - Radius .. I + Radius loop
            for Y in J - Radius .. J + Radius loop
               Put
               (  Picture
                  (  Integer'Min (Picture'Last (1), Integer'Max (Picture'First (1), X)),
                     Integer'Min (Picture'Last (2), Integer'Max (Picture'First (2), Y))
               )  );
            end loop;
         end loop;
         Result (I, J) := Sorted (0).Color;
      end loop;
   end loop;
   return Result;
end Median;
```

The implementation works with an arbitrary window width. The window is specified by its radius ''R''>0. The resulting width is 2''R''+1. The filter uses the original pixels of the image from the median of the window sorted according to the luminance. The image edges are extrapolated using the nearest pixel on the border. Sorting uses binary search. (For practical use, note that median filter is extremely slow.)

The following sample code illustrates use:

```ada
   F1, F2 : File_Type;
begin
   Open (F1, In_File, "city.ppm");
   Create (F2, Out_File, "city_median.ppm");
   Put_PPM (F2, Median (Get_PPM (F1), 1)); -- Window 3x3
   Close (F1);
   Close (F2);
```



## BBC BASIC

This example is a 5 x 5 median filter:
[[Image:greyscale_bbc.jpg|right]]
[[Image:median_bbc.jpg|right]]

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      Sort% = FN_sortinit(0,0)

      Width% = 200
      Height% = 200

      DIM out&(Width%-1, Height%-1)

      VDU 23,22,Width%;Height%;8,16,16,128
      *DISPLAY Lenagrey
      OFF

      REM Do the median filtering:
      DIM pix&(24)
      C% = 25
      FOR Y% = 2 TO Height%-3
        FOR X% = 2 TO Width%-3
          P% = 0
          FOR I% = -2 TO 2
            FOR J% = -2 TO 2
              pix&(P%) = TINT((X%+I%)*2, (Y%+J%)*2) AND &FF
              P% += 1
            NEXT
          NEXT
          CALL Sort%, pix&(0)
          out&(X%, Y%) = pix&(12)
        NEXT
      NEXT Y%

      REM Display:
      GCOL 1
      FOR Y% = 0 TO Height%-1
        FOR X% = 0 TO Width%-1
          COLOUR 1, out&(X%,Y%), out&(X%,Y%), out&(X%,Y%)
          LINE X%*2,Y%*2,X%*2,Y%*2
        NEXT
      NEXT Y%

      REPEAT
        WAIT 1
      UNTIL FALSE
```



## C

O(n) filter with histogram.

```c
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>

typedef struct { unsigned char r, g, b; } rgb_t;
typedef struct {
	int w, h;
	rgb_t **pix;
} image_t, *image;

typedef struct {
	int r[256], g[256], b[256];
	int n;
} color_histo_t;

int write_ppm(image im, char *fn)
{
	FILE *fp = fopen(fn, "w");
	if (!fp) return 0;
	fprintf(fp, "P6\n%d %d\n255\n", im->w, im->h);
	fwrite(im->pix[0], 1, sizeof(rgb_t) * im->w * im->h, fp);
	fclose(fp);
	return 1;
}

image img_new(int w, int h)
{
	int i;
	image im = malloc(sizeof(image_t) + h * sizeof(rgb_t*)
			+ sizeof(rgb_t) * w * h);
	im->w = w; im->h = h;
	im->pix = (rgb_t**)(im + 1);
	for (im->pix[0] = (rgb_t*)(im->pix + h), i = 1; i < h; i++)
		im->pix[i] = im->pix[i - 1] + w;
	return im;
}

int read_num(FILE *f)
{
	int n;
	while (!fscanf(f, "%d ", &n)) {
		if ((n = fgetc(f)) == '#') {
			while ((n = fgetc(f)) != '\n')
				if (n == EOF) break;
			if (n == '\n') continue;
		} else return 0;
	}
	return n;
}

image read_ppm(char *fn)
{
	FILE *fp = fopen(fn, "r");
	int w, h, maxval;
	image im = 0;
	if (!fp) return 0;

	if (fgetc(fp) != 'P' || fgetc(fp) != '6' || !isspace(fgetc(fp)))
		goto bail;

	w = read_num(fp);
	h = read_num(fp);
	maxval = read_num(fp);
	if (!w || !h || !maxval) goto bail;

	im = img_new(w, h);
	fread(im->pix[0], 1, sizeof(rgb_t) * w * h, fp);
bail:
	if (fp) fclose(fp);
	return im;
}

void del_pixels(image im, int row, int col, int size, color_histo_t *h)
{
	int i;
	rgb_t *pix;

	if (col < 0 || col >= im->w) return;
	for (i = row - size; i <= row + size && i < im->h; i++) {
		if (i < 0) continue;
		pix = im->pix[i] + col;
		h->r[pix->r]--;
		h->g[pix->g]--;
		h->b[pix->b]--;
		h->n--;
	}
}

void add_pixels(image im, int row, int col, int size, color_histo_t *h)
{
	int i;
	rgb_t *pix;

	if (col < 0 || col >= im->w) return;
	for (i = row - size; i <= row + size && i < im->h; i++) {
		if (i < 0) continue;
		pix = im->pix[i] + col;
		h->r[pix->r]++;
		h->g[pix->g]++;
		h->b[pix->b]++;
		h->n++;
	}
}

void init_histo(image im, int row, int size, color_histo_t*h)
{
	int j;

	memset(h, 0, sizeof(color_histo_t));

	for (j = 0; j < size && j < im->w; j++)
		add_pixels(im, row, j, size, h);
}

int median(const int *x, int n)
{
	int i;
	for (n /= 2, i = 0; i < 256 && (n -= x[i]) > 0; i++);
	return i;
}

void median_color(rgb_t *pix, const color_histo_t *h)
{
	pix->r = median(h->r, h->n);
	pix->g = median(h->g, h->n);
	pix->b = median(h->b, h->n);
}

image median_filter(image in, int size)
{
	int row, col;
	image out = img_new(in->w, in->h);
	color_histo_t h;

	for (row = 0; row < in->h; row ++) {
		for (col = 0; col < in->w; col++) {
			if (!col) init_histo(in, row, size, &h);
			else {
				del_pixels(in, row, col - size, size, &h);
				add_pixels(in, row, col + size, size, &h);
			}
			median_color(out->pix[row] + col, &h);
		}
	}

	return out;
}

int main(int c, char **v)
{
	int size;
	image in, out;
	if (c <= 3) {
		printf("Usage: %s size ppm_in ppm_out\n", v[0]);
		return 0;
	}
	size = atoi(v[1]);
	printf("filter size %d\n", size);
	if (size < 0) size = 1;

	in = read_ppm(v[2]);
	out = median_filter(in, size);
	write_ppm(out, v[3]);
	free(in);
	free(out);

	return 0;
}
```



## D


This uses modules of the [[Bitmap]] and [[Grayscale image]] Tasks.

The implementation uses algorithm described in [http://nomis80.org/ctmf.html Median Filtering in Constant Time]
paper with some slight differences, that shouldn't have impact on complexity.

Currently this code works only on greyscale images.

```d
import grayscale_image;

Image!Color medianFilter(uint radius=10, Color)(in Image!Color img)
pure nothrow @safe if (radius > 0) in {
    assert(img.nx >= radius && img.ny >= radius);
} body {
    alias Hist = uint[256];

    static ubyte median(uint no)(in ref Hist cumulative)
    pure nothrow @safe @nogc {
        size_t localSum = 0;
        foreach (immutable k, immutable v; cumulative)
            if (v) {
                localSum += v;
                if (localSum > no / 2)
                    return k;
            }
        return 0;
    }

    // Copy image borders in the result image.
    auto result = new Image!Color(img.nx, img.ny);
    foreach (immutable y; 0 .. img.ny)
        foreach (immutable x; 0 .. img.nx)
            if (x < radius || x > img.nx - radius - 1 ||
                y < radius || y > img.ny - radius - 1)
                result[x, y] = img[x, y];

    enum edge = 2 * radius + 1;
    auto hCol = new Hist[img.nx];

    // Create histogram columns.
    foreach (immutable y; 0 .. edge - 1)
        foreach (immutable x, ref hx; hCol)
            hx[img[x, y]]++;

    foreach (immutable y; radius .. img.ny - radius) {
        // Add to each histogram column lower pixel.
        foreach (immutable x, ref hx; hCol)
            hx[img[x, y + radius]]++;

        // Calculate main Histogram using first edge-1 columns.
        Hist H;
        foreach (immutable x; 0 .. edge - 1)
            foreach (immutable k, immutable v; hCol[x])
                if (v)
                    H[k] += v;

        foreach (immutable x; radius .. img.nx - radius) {
            // Add right-most column.
            foreach (immutable k, immutable v; hCol[x + radius])
                if (v)
                    H[k] += v;

            result[x, y] = Color(median!(edge ^^ 2)(H));

            // Drop left-most column.
            foreach (immutable k, immutable v; hCol[x - radius])
                if (v)
                    H[k] -= v;
        }

        // Substract the upper pixels.
        foreach (immutable x, ref hx; hCol)
            hx[img[x, y - radius]]--;
    }

    return result;
}

version (median_filter_main)
    void main() { // Demo.
        loadPGM!Gray(null, "lena.pgm").
        medianFilter!10
        .savePGM("lena_median_r10.pgm");
    }
```

Compile with -version=median_filter_main to run the demo.


## GDL

GDL has no inbuilt median filter function, which is native in IDL. This example is based on pseudocode here: http://en.wikipedia.org/wiki/Median_filter#2D_median_filter_pseudo_code, however, it works with 1D arrays only. It does not process boundaries.

```GDL

FUNCTION MEDIANF,ARRAY,WINDOW
	RET=fltarr(N_ELEMENTS(ARRAY),1)
	EDGEX=WINDOW/2
	FOR X=EDGEX, N_ELEMENTS(ARRAY)-EDGEX DO BEGIN
		PRINT, "X", X
		COLARRAY=fltarr(WINDOW,1)
		FOR FX=0, WINDOW-1 DO BEGIN
			COLARRAY[FX]=ARRAY[X + FX - EDGEX]
		END
		T=COLARRAY[SORT(COLARRAY)]
		RET[X]=T[WINDOW/2]
	END
	RETURN, RET
END

```

Usage:

```GDL
Result = MEDIANF(ARRAY, WINDOW)
```


## Go

Implemented with existing GetPx/SetPx functions at Grayscale image task.  It could be sped up by putting code in the raster package, but if you're concerned about speed, you should implement one of the O(n) algorithms available.

```go
package main

// Files required to build supporting package raster are found in:
// * Bitmap
// * Grayscale image
// * Read a PPM file
// * Write a PPM file

import (
    "fmt"
    "raster"
)

var g0, g1 *raster.Grmap
var ko [][]int
var kc []uint16
var mid int

func init() {
    // hard code box of 9 pixels
    ko = [][]int{
        {-1, -1}, {0, -1}, {1, -1},
        {-1,  0}, {0,  0}, {1,  0},
        {-1,  1}, {0,  1}, {1,  1}}
    kc = make([]uint16, len(ko))
    mid = len(ko) / 2
}

func main() {
    // Example file used here is Lenna50.jpg from the task "Percentage
    // difference between images" converted with with the command
    // convert Lenna50.jpg -colorspace gray Lenna50.ppm
    // It shows very obvious compression artifacts when viewed at higher
    // zoom factors.
    b, err := raster.ReadPpmFile("Lenna50.ppm")
    if err != nil {
        fmt.Println(err)
        return
    }
    g0 = b.Grmap()
    w, h := g0.Extent()
    g1 = raster.NewGrmap(w, h)
    for y := 0; y < h; y++ {
        for x := 0; x < w; x++ {
            g1.SetPx(x, y, median(x, y))
        }
    }
    // side by side comparison with input file shows compression artifacts
    // greatly smoothed over, although at some loss of contrast.
    err = g1.Bitmap().WritePpmFile("median.ppm")
    if err != nil {
        fmt.Println(err)
    }
}

func median(x, y int) uint16 {
    var n int
    // construct sorted list as pixels are read.  insertion sort can't be
    // beat for a small number of items, plus there would be lots of overhead
    // just to get numbers in and out of a library sort routine.
    for _, o := range ko {
        // read a pixel of the kernel
        c, ok := g0.GetPx(x+o[0], y+o[1])
        if !ok {
            continue
        }
        // insert it in sorted order
        var i int
        for ; i < n; i++ {
            if c < kc[i] {
                for j := n; j > i; j-- {
                    kc[j] = kc[j-1]
                }
                break
            }
        }
        kc[i] = c
        n++
    }
    // compute median from sorted list
    switch {
    case n == len(kc): // the usual case, pixel with complete neighborhood
        return kc[mid]
    case n%2 == 1: // edge case, odd number of pixels
        return kc[n/2]
    }
    // else edge case, even number of pixels
    m := n / 2
    return (kc[m-1] + kc[m]) / 2
}
```



## J

The task could be solved the following way. First, for each pixel of input, collect pixels which fall into the corresponding window, where median value will be calculated. Then, for each window - the set of pixels - find the median value. To compare 3-channel pixels we first convert them into 1-channel gray values.

The following verbs are used to work with bitmaps:


```J

makeRGB=: 0&$: : (($,)~ ,&3)
toGray=: <. @: (+/) @: (0.2126 0.7152 0.0722 & *)"1

```


We'll determine the window as a square zone around each pixel, with the given pixel in the center of the zone. Such a window always have odd height and width. We'll say the window radius is 0 if the window contain only the given pixel - in this case the resulting picture will be identical to the input. The radius is 1 if the window is 3x3 pixels, with given pixel in the center. Radius is 2 if the window is 5x5 pixels, with given pixel in the center, etc.

To get all pixels in the window, first calculate coordinates - or indexes - of those pixels. For the pixels on the edges of the input bitmap, include only those indexes which correspond to actually existing pixels - no negative indexes and no indexes outside of the bitmap boundaries.


```J

median_filter =: dyad define
 win =. y -~ i. >: +: y
 height =. {: }: $ x
 width =. {. }: $ x
 h_indexes =. < @ (#~ >:&0 * <&height) @ (win&+)"0 i. height
 w_indexes =. < @ (#~ >:&0 * <&width) @ (win&+)"0 i. width
 sets =. w_indexes < @ ({&x) @ < @ ,"0 0/ h_indexes
 medians =. ({~ <. @ -: @ {. @ $) @ ({~ /: @: toGray) @ (,/) @ > sets
)

```


Example:

```J

   ] bmp =. ?. 256 + makeRGB 4 5
 34  39 168
133 133  40
210 137 244
 66 183 114
211 241  75

212  68  13
 91 246 128
203 236 213
162  92 165
 90 203 161

104 124 113
199  61  60
135 179 241
142 156 125
 64  77  61

130  70 200
114  32  55
 94 211 182
 29  49 252
116 139 217
   bmp median_filter 1
133 133  40
210 137 244
210 137 244
 90 203 161
 90 203 161

104 124 113
133 133  40
 66 183 114
210 137 244
 66 183 114

212  68  13
104 124 113
142 156 125
142 156 125
116 139 217

130  70 200
104 124 113
142 156 125
142 156 125
116 139 217

```



## Julia

```julia
using Images, ImageFiltering, FileIO
Base.isless(a::RGB{T}, b::RGB{T}) where T =
    red(a) < red(b) || green(a) < green(b) || blue(a) < blue(b)
Base.middle(x::RGB) = x

img = load("data/lenna100.jpg")
mapwindow(median!, img, (3, 3))
```



## Kotlin

We reuse the class in the [[Bitmap]] task for this and add a member function to filter the image as per the Wikipedia pseudo-code. The colors in the Window array are sorted by their luminance.

To test the function we use the left half of the sample image file (Medianfilterp.png) in the Wikipedia article and see if we can get close to the right half.

```scala
// Version 1.2.41
import java.awt.Color
import java.awt.Graphics
import java.awt.image.BufferedImage
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

    fun medianFilter(windowWidth: Int, windowHeight: Int) {
        val window = Array(windowWidth * windowHeight) { Color.black }
        val edgeX = windowWidth / 2
        val edgeY = windowHeight / 2
        val compareByLuminance = {
            c: Color -> 0.2126 * c.red + 0.7152 * c.green + 0.0722 * c.blue
        }
        for (x in edgeX until image.width - edgeX) {
            for (y in edgeY until image.height - edgeY) {
                var i = 0
                for (fx in 0 until windowWidth) {
                    for (fy in 0 until windowHeight) {
                        window[i] = getPixel(x + fx - edgeX, y + fy - edgeY)
                        i++
                    }
                }
                window.sortBy(compareByLuminance)
                setPixel(x, y, window[windowWidth * windowHeight / 2])
            }
        }
    }
}

fun main(args: Array<String>) {
    val img = ImageIO.read(File("Medianfilterp.png"))
    val bbs = BasicBitmapStorage(img.width / 2, img.height)
    with (bbs) {
        for (y in 0 until img.height) {
            for (x in 0 until img.width / 2) {
                setPixel(x, y, Color(img.getRGB(x, y)))
            }
        }
        medianFilter(3, 3)
        val mfFile = File("Medianfilterp2.png")
        ImageIO.write(image, "png", mfFile)
    }
}
```


```txt

Similar to right-half of Wikipedia image - color definition and brightness seem better but remaining distortion more evident.

```



## Mathematica



```Mathematica

MedianFilter[img,n]

```



## OCaml



```ocaml
let color_add (r1,g1,b1) (r2,g2,b2) =
  ( (r1 + r2),
    (g1 + g2),
    (b1 + b2) )

let color_div (r,g,b) d =
  ( (r / d),
    (g / d),
    (b / d) )

let compare_as_grayscale (r1,g1,b1) (r2,g2,b2) =
  let v1 = (2_126 * r1 +  7_152 * g1 + 722 * b1)
  and v2 = (2_126 * r2 +  7_152 * g2 + 722 * b2) in
  (Pervasives.compare v1 v2)

let get_rgb img x y =
  let _, r_channel,_,_ = img in
  let width = Bigarray.Array2.dim1 r_channel
  and height = Bigarray.Array2.dim2 r_channel in
  if (x < 0) || (x >= width) then (0,0,0) else
  if (y < 0) || (y >= height) then (0,0,0) else  (* feed borders with black *)
  (get_pixel img x y)


let median_value img radius =
  let samples = (radius*2+1) * (radius*2+1) in
  fun x y ->
    let sample = ref [] in

    for _x = (x - radius) to (x + radius) do
      for _y = (y - radius) to (y + radius) do

        let v = get_rgb img _x _y in

        sample := v :: !sample;
      done;
    done;

    let ssample = List.sort compare_as_grayscale !sample in
    let mid = (samples / 2) in

    if (samples mod 2) = 1
    then List.nth ssample (mid+1)
    else
      let median1 = List.nth ssample (mid)
      and median2 = List.nth ssample (mid+1) in
      (color_div (color_add median1 median2) 2)


let median img radius =
  let _, r_channel,_,_ = img in
  let width = Bigarray.Array2.dim1 r_channel
  and height = Bigarray.Array2.dim2 r_channel in

  let _median_value = median_value img radius in

  let res = new_img ~width ~height in
  for y = 0 to pred height do
    for x = 0 to pred width do
      let color = _median_value x y in
      put_pixel res color x y;
    done;
  done;
  (res)
```


an alternate version of the function <tt>median_value</tt> using arrays instead of lists:

```ocaml
let median_value img radius =
  let samples = (radius*2+1) * (radius*2+1) in
  let sample = Array.make samples (0,0,0) in
  fun x y ->
    let i = ref 0 in
    for _x = (x - radius) to (x + radius) do
      for _y = (y - radius) to (y + radius) do
        let v = get_rgb img _x _y in
        sample.(!i) <- v;
        incr i;
      done;
    done;

    Array.sort compare_as_grayscale sample;
    let mid = (samples / 2) in

    if (samples mod 2) = 1
    then sample.(mid+1)
    else (color_div (color_add sample.(mid)
                               sample.(mid+1)) 2)
```



## Perl


```perl
use strict 'vars';
use warnings;

use PDL;
use PDL::Image2D;

my $image = rpic 'plasma.png';
my $smoothed = med2d $image, ones(3,3), {Boundary => Truncate};
wpic $smoothed, 'plasma_median.png';
```

Compare offsite images: [https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/plasma.png plasma.png] vs.
[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/plasma_median.png plasma_median.png]

## Perl 6

Clone of Perl 5, for now.

```perl6>use PDL:from<Perl5
;
use PDL::Image2D:from<Perl5>;

my $image = rpic 'plasma.png';
my $smoothed = med2d($image, ones(3,3), {Boundary => 'Truncate'});
wpic $smoothed, 'plasma_median.png';
```

Compare offsite images: [https://github.com/SqrtNegInf/Rosettacode-Perl6-Smoke/blob/master/ref/plasma-perl6.png plasma.png] vs.
[https://github.com/SqrtNegInf/Rosettacode-Perl6-Smoke/blob/master/ref/plasma_median.png plasma_median.png]


## Phix

Requires read_ppm() from [[Bitmap/Read_a_PPM_file#Phix|Read_a_PPM_file]], write_ppm() from [[Bitmap/Write_a_PPM_file#Phix|Write_a_PPM_file]],
which are both now part of demo\rosetta\ppm.e. Results may be verified with demo\rosetta\viewppm.exw

```Phix
-- demo\rosetta\Bitmap_Median_filter.exw
include ppm.e

constant neigh = {{-1,-1},{0,-1},{1,-1},
                  {-1, 0},{0, 0},{1, 0},
                  {-1, 1},{0, 1},{1, 1}}

--constant neigh = {{-2,-2},{-1,-2},{0,-2},{1,-2},{2,-2},
--                {-2,-1},{-1,-1},{0,-1},{1,-1},{2,-1},
--                {-2, 0},{-1, 0},{0, 0},{1, 0},{2, 0},
--                {-2, 1},{-1, 1},{0, 1},{1, 1},{2, 1},
--                {-2, 2},{-1, 2},{0, 2},{1, 2},{2, 2}}

sequence kn = repeat(0,length(neigh))

function median(sequence image)
    integer h = length(image),
            w = length(image[1])
    for i=1 to length(image) do
        for j=1 to length(image[i]) do
            integer n = 0, c, p, x, y
            for k=1 to length(neigh) do
                x = i+neigh[k][1]
                y = j+neigh[k][2]
                if  x>=1 and x<=h
                and y>=1 and y<=w then
                    n += 1
                    c = image[x,j]
                    p = n
                    while p>1 do
                        if c>kn[p-1] then exit end if
                        kn[p] = kn[p-1]
                        p -= 1
                    end while
                    kn[p] = c
                end if
            end for
            if and_bits(n,1) then
                c = kn[(n+1)/2]
            else
                c = floor((kn[n/2]+kn[n/2+1])/2)
            end if
            image[i,j] = c
        end for
    end for
    return image
end function

sequence img = read_ppm("Lena.ppm")
    img = median(img)
    write_ppm("LenaMedian.ppm",img)
```



## PicoLisp


```PicoLisp
(de ppmMedianFilter (Radius Ppm)
   (let Len (inc (* 2 Radius))
      (make
         (chain (head Radius Ppm))
         (for (Y Ppm  T  (cdr Y))
            (NIL (nth Y Len)
               (chain (tail Radius Y)) )
            (link
               (make
                  (chain (head Radius (get Y (inc Radius))))
                  (for (X (head Len Y) T)
                     (NIL (nth X 1 Len)
                        (chain (tail Radius (get X (inc Radius)))) )
                     (link
                        (cdr
                           (get
                              (sort
                                 (mapcan
                                    '((Y)
                                       (mapcar
                                          '((C)
                                             (cons
                                                (+
                                                   (* (car C) 2126)     # Red
                                                   (* (cadr C) 7152)    # Green
                                                   (* (caddr C) 722) )  # Blue
                                                C ) )
                                          (head Len Y) ) )
                                    X ) )
                              (inc Radius) ) ) )
                     (map pop X) ) ) ) ) ) ) )
```

Test using 'ppmRead' from [[Bitmap/Read a PPM file#PicoLisp]] and 'ppmWrite'
from [[Bitmap/Write a PPM file#PicoLisp]]:

```txt
(ppmWrite (ppmMedianFilter 2 (ppmRead "Lenna100.ppm")) "a.ppm")
```



## Python

```python
import Image, ImageFilter
im = Image.open('image.ppm')

median = im.filter(ImageFilter.MedianFilter(3))
median.save('image2.ppm')
```



## Racket

Due to the use of flomaps the solution below works for all types of images.

```racket

#lang racket
(require images/flomap math)

(define lena <<paste image of Lena here>> )
(define bm (send lena get-bitmap))
(define fm (bitmap->flomap bm))

(flomap->bitmap
 (build-flomap
  4 (send bm get-width) (send bm get-height)
  (λ (k x y)
    (define (f x y) (flomap-ref fm k x y))
    (median < (list (f (- x 1) (- y 1))
                    (f (- x 1)    y)
                    (f (- x 1) (+ y 1))
                    (f    x    (- y 1))
                    (f    x       y)
                    (f    x    (+ y 1))
                    (f (+ x 1) (- y 1))
                    (f (+ x 1)    y)
                    (f (+ x 1) (+ y 1)))))))

```



## Ruby

```ruby
class Pixmap
  def median_filter(radius=3)
    radius += 1 if radius.even?
    filtered = self.class.new(@width, @height)
    pb = ProgressBar.new(@height) if $DEBUG
    @height.times do |y|
      @width.times do |x|
        window = []
        (x - radius).upto(x + radius).each do |win_x|
          (y - radius).upto(y + radius).each do |win_y|
            win_x = 0 if win_x < 0
            win_y = 0 if win_y < 0
            win_x = @width-1 if win_x >= @width
            win_y = @height-1 if win_y >= @height
            window << self[win_x, win_y]
          end
        end
        # median
        filtered[x, y] = window.sort[window.length / 2]
      end
      pb.update(y) if $DEBUG
    end
    pb.close if $DEBUG
    filtered
  end
end

class RGBColour
  # refactoring
  def luminosity
    Integer(0.2126*@red + 0.7152*@green + 0.0722*@blue)
  end
  def to_grayscale
    l = luminosity
    self.class.new(l, l, l)
  end

  # defines how to compare (and hence, sort)
  def <=>(other)
    self.luminosity <=> other.luminosity
  end
end

class ProgressBar
  def initialize(max)
    $stdout.sync = true
    @progress_max = max
    @progress_pos = 0
    @progress_view = 68
    $stdout.print "[#{'-'*@progress_view}]\r["
  end

  def update(n)
    new_pos = n * @progress_view/@progress_max
    if new_pos > @progress_pos
      @progress_pos = new_pos
      $stdout.print '='
    end
  end

  def close
    $stdout.puts '=]'
  end
end

bitmap = Pixmap.open('file')
filtered = bitmap.median_filter
```



## Tcl

```tcl
package require Tk

# Set the color of a pixel
proc applyMedian {srcImage x y -> dstImage} {
    set x0 [expr {$x==0 ? 0 : $x-1}]
    set y0 [expr {$y==0 ? 0 : $y-1}]
    set x1 $x
    set y1 $y
    set x2 [expr {$x+1==[image width $srcImage]  ? $x : $x+1}]
    set y2 [expr {$y+1==[image height $srcImage] ? $y : $y+1}]

    set r [set g [set b {}]]
    foreach X [list $x0 $x1 $x2] {
	foreach Y [list $y0 $y1 $y2] {
	    lassign [$srcImage get $X $Y] rPix gPix bPix
	    lappend r $rPix
	    lappend g $gPix
	    lappend b $bPix
	}
    }
    set r [lindex [lsort -integer $r] 4]
    set g [lindex [lsort -integer $g] 4]
    set b [lindex [lsort -integer $b] 4]
    $dstImage put [format "#%02x%02x%02x" $r $g $b] -to $x $y
}
# Apply the filter to the whole image
proc medianFilter {srcImage {dstImage ""}} {
    if {$dstImage eq ""} {
	set dstImage [image create photo]
    }
    set w [image width $srcImage]
    set h [image height $srcImage]
    for {set x 0} {$x < $w} {incr x} {
	for {set y 0} {$y < $h} {incr y} {
	    applyMedian $srcImage $x $y -> $dstImage
	}
    }
    return $dstImage
}

# Demonstration code using the Tk widget demo's teapot image
image create photo teapot -file $tk_library/demos/images/teapot.ppm
pack [labelframe .src -text Source] -side left
pack [label .src.l -image teapot]
update
pack [labelframe .dst -text Median] -side left
pack [label .dst.l -image [medianFilter teapot]]
```



## zkl

Uses Image Magick and the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl


```zkl
fcn medianFilter(img){  //-->new image
   var [const] window=[-2..2].walk(), edge=(window.len()/2);  // 5x5 window

   MX,MY,new := img.w,img.h,PPM(MX,MY);
   pixel,pixels:=List(),List();
   foreach x,y in ([edge..MX-edge-1],[edge..MY-edge-1]){
      pixels.clear();
      foreach ox,oy in (window,window){   // construct sorted list as pixels are read.
	 pixels.merge(pixel.clear(img[x+ox, y+oy]));    // merge sort two lists
      }
      new[x,y]=pixels[4];  // median value
   }
   new
}
```


```zkl
filtered:=medianFilter(PPM.readJPGFile("lena.jpg"));
filtered.writeJPGFile("lenaMedianFiltered.zkl.jpg");
```

See the [http://www.zenkinetic.com/Images/RosettaCode/lenaMedianFiltered.zkl.jpg filtered image]
and the [http://www.zenkinetic.com/Images/RosettaCode/lena.jpg orginal].

