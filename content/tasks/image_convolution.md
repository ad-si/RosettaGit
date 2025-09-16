+++
title = "Image convolution"
description = ""
date = 2019-10-14T03:42:39Z
aliases = []
[extra]
id = 3228
[taxonomies]
categories = ["task", "Image processing"]
tags = []
languages = [
  "ada",
  "bbc_basic",
  "c",
  "common_lisp",
  "d",
  "go",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "liberty_basic",
  "maple",
  "matlab",
  "ocaml",
  "octave",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "ruby",
  "tcl",
]
+++

## Task

One class of image digital filters is described by a rectangular matrix of real coefficients called '''kernel''' convoluted in a sliding window of image pixels. Usually the kernel is square <math>K_{kl}</math>, where <i>k</i>, <i>l</i> are in the range -<i>R</i>,-<i>R</i>+1,..,<i>R</i>-1,<i>R</i>. <i>W</i>=2<i>R</i>+1 is the kernel width. The filter determines the new value of a monochromatic image pixel P<sub><i>ij</i></sub> as a convolution of the image pixels in the window centered in <i>i</i>, <i>j</i> and the kernel values:

<blockquote>
<math>P_{ij}=\displaystyle\sum_{k=-R}^R \sum_{l=-R}^R P_{i+k\ j+l} K_{k l}</math>
</blockquote>

Color images are usually split into the channels which are filtered independently. A color model can be changed as well, i.e. filtration is performed not necessarily in RGB. Common kernels sizes are 3x3 and 5x5. The complexity of filtrating grows quadratically ([[O]](<i>n</i><sup>2</sup>)) with the kernel width.

'''Task''': Write a generic convolution 3x3 kernel filter. Optionally show some end user filters that use this generic one.

''(You can use, to test the functions below, these [[Read_ppm_file|input]] and [[Write_ppm_file|output]] solutions.)''


## Ada

First we define floating-point stimulus and color pixels which will be then used for filtration:

```ada
type Float_Luminance is new Float;

type Float_Pixel is record
   R, G, B : Float_Luminance := 0.0;
end record;

function "*" (Left : Float_Pixel; Right : Float_Luminance) return Float_Pixel is
   pragma Inline ("*");
begin
   return (Left.R * Right, Left.G * Right, Left.B * Right);
end "*";

function "+" (Left, Right : Float_Pixel) return Float_Pixel is
   pragma Inline ("+");
begin
   return (Left.R + Right.R, Left.G + Right.G, Left.B + Right.B);
end "+";

function To_Luminance (X : Float_Luminance) return Luminance is
   pragma Inline (To_Luminance);
begin
   if X <= 0.0 then
      return 0;
   elsif X >= 255.0 then
      return 255;
   else
      return Luminance (X);
   end if;
end To_Luminance;

function To_Pixel (X : Float_Pixel) return Pixel is
   pragma Inline (To_Pixel);
begin
   return (To_Luminance (X.R), To_Luminance (X.G), To_Luminance (X.B));
end To_Pixel;
```

Float_Luminance is an unconstrained equivalent of Luminance. Float_Pixel is one to Pixel. Conversion operations To_Luminance and To_Pixel saturate the corresponding values. The operation + is defined per channels. The operation * is defined as multiplying by a scalar. (I.e. Float_Pixel is a vector space.)

Now we are ready to implement the filter. The operation is performed in memory. The access to the image array is minimized using a slid window. The filter is in fact a triplet of filters handling each image channel independently. It can be used with other color models as well.

```ada
type Kernel_3x3 is array (-1..1, -1..1) of Float_Luminance;

procedure Filter (Picture : in out Image; K : Kernel_3x3) is
   function Get (I, J : Integer) return Float_Pixel is
      pragma Inline (Get);
   begin
      if I in Picture'Range (1) and then J in Picture'Range (2) then
         declare
            Color : Pixel := Picture (I, J);
         begin
            return (Float_Luminance (Color.R), Float_Luminance (Color.G), Float_Luminance (Color.B));
         end;
      else
         return (others => 0.0);
      end if;
   end Get;
   W11, W12, W13 : Float_Pixel; -- The image window
   W21, W22, W23 : Float_Pixel;
   W31, W32, W33 : Float_Pixel;
   Above : array (Picture'First (2) - 1..Picture'Last (2) + 1) of Float_Pixel;
   This  : Float_Pixel;
begin
   for I in Picture'Range (1) loop
      W11 := Above (Picture'First (2) - 1); -- The upper row is taken from the cache
      W12 := Above (Picture'First (2)    );
      W13 := Above (Picture'First (2) + 1);
      W21 := (others => 0.0);               -- The middle row
      W22 := Get (I, Picture'First (2)    );
      W23 := Get (I, Picture'First (2) + 1);
      W31 := (others => 0.0);               -- The bottom row
      W32 := Get (I+1, Picture'First (2)    );
      W33 := Get (I+1, Picture'First (2) + 1);
      for J in Picture'Range (2) loop
         This :=
            W11 * K (-1, -1) + W12 * K (-1, 0) + W13 * K (-1, 1) +
            W21 * K ( 0, -1) + W22 * K ( 0, 0) + W23 * K ( 0, 1) +
            W31 * K ( 1, -1) + W32 * K ( 1, 0) + W33 * K ( 1, 1);
         Above (J-1) := W21;
         W11 := W12; W12 := W13; W13 := Above (J+1);     -- Shift the window
         W21 := W22; W22 := W23; W23 := Get (I,   J+1);
         W31 := W32; W32 := W23; W33 := Get (I+1, J+1);
         Picture (I, J) := To_Pixel (This);
      end loop;
      Above (Picture'Last (2)) := W21;
   end loop;
end Filter;
```

Example of use:

```ada
   F1, F2 : File_Type;
begin
   Open (F1, In_File, "city.ppm");
   declare
      X : Image := Get_PPM (F1);
   begin
      Close (F1);
      Create (F2, Out_File, "city_sharpen.ppm");
      Filter (X, ((-1.0, -1.0, -1.0), (-1.0, 9.0, -1.0), (-1.0, -1.0, -1.0)));
      Put_PPM (F2, X);
   end;
   Close (F2);
```



## BBC BASIC

[[Image:original_bbc.jpg|right]]
[[Image:sharpened_bbc.jpg|right]]

```bbcbasic
      Width% = 200
      Height% = 200

      DIM out&(Width%-1, Height%-1, 2)

      VDU 23,22,Width%;Height%;8,16,16,128
      *DISPLAY Lena
      OFF

      DIM filter%(2, 2)
      filter%() = -1, -1, -1, -1, 12, -1, -1, -1, -1

      REM Do the convolution:
      FOR Y% = 1 TO Height%-2
        FOR X% = 1 TO Width%-2
          R% = 0 : G% = 0 : B% = 0
          FOR I% = -1 TO 1
            FOR J% = -1 TO 1
              C% = TINT((X%+I%)*2, (Y%+J%)*2)
              F% = filter%(I%+1,J%+1)
              R% += F% * (C% AND &FF)
              G% += F% * (C% >> 8 AND &FF)
              B% += F% * (C% >> 16)
            NEXT
          NEXT
          IF R% < 0 R% = 0 ELSE IF R% > 1020 R% = 1020
          IF G% < 0 G% = 0 ELSE IF G% > 1020 G% = 1020
          IF B% < 0 B% = 0 ELSE IF B% > 1020 B% = 1020
          out&(X%, Y%, 0) = R% / 4 + 0.5
          out&(X%, Y%, 1) = G% / 4 + 0.5
          out&(X%, Y%, 2) = B% / 4 + 0.5
        NEXT
      NEXT Y%

      REM Display:
      GCOL 1
      FOR Y% = 0 TO Height%-1
        FOR X% = 0 TO Width%-1
          COLOUR 1, out&(X%,Y%,0), out&(X%,Y%,1), out&(X%,Y%,2)
          LINE X%*2,Y%*2,X%*2,Y%*2
        NEXT
      NEXT Y%

      REPEAT
        WAIT 1
      UNTIL FALSE
```



## C


Interface:


```c
image filter(image img, double *K, int Ks, double, double);
```


The implementation (the <tt>Ks</tt> argument is so that 1 specifies a 3&times;3 matrix, 2 a 5&times;5 matrix ...
N a (2N+1)&times;(2N+1) matrix).


```c
#include "imglib.h"

inline static color_component GET_PIXEL_CHECK(image img, int x, int y, int l) {
  if ( (x<0) || (x >= img->width) || (y<0) || (y >= img->height) ) return 0;
  return GET_PIXEL(img, x, y)[l];
}

image filter(image im, double *K, int Ks, double divisor, double offset)
{
  image oi;
  unsigned int ix, iy, l;
  int kx, ky;
  double cp[3];

  oi = alloc_img(im->width, im->height);
  if ( oi != NULL ) {
    for(ix=0; ix < im->width; ix++) {
      for(iy=0; iy < im->height; iy++) {
	cp[0] = cp[1] = cp[2] = 0.0;
	for(kx=-Ks; kx <= Ks; kx++) {
	  for(ky=-Ks; ky <= Ks; ky++) {
	    for(l=0; l<3; l++)
	      cp[l] += (K[(kx+Ks) +
                        (ky+Ks)*(2*Ks+1)]/divisor) *
                        ((double)GET_PIXEL_CHECK(im, ix+kx, iy+ky, l)) + offset;
	  }
	}
	for(l=0; l<3; l++)
	  cp[l] = (cp[l]>255.0) ? 255.0 : ((cp[l]<0.0) ? 0.0 : cp[l]) ;
	put_pixel_unsafe(oi, ix, iy,
			 (color_component)cp[0],
			 (color_component)cp[1],
			 (color_component)cp[2]);
      }
    }
    return oi;
  }
  return NULL;
}
```


Usage example:

The <tt>read_image</tt> function is from [[Read image file through a pipe|here]].


```c
#include <stdio.h>
#include "imglib.h"

const char *input = "Lenna100.jpg";
const char *output = "filtered_lenna%d.ppm";

double emboss_kernel[3*3] = {
  -2., -1.,  0.,
  -1.,  1.,  1.,
  0.,  1.,  2.,
};

double sharpen_kernel[3*3] = {
  -1.0, -1.0, -1.0,
  -1.0,  9.0, -1.0,
  -1.0, -1.0, -1.0
};
double sobel_emboss_kernel[3*3] = {
  -1., -2., -1.,
  0.,  0.,  0.,
  1.,  2.,  1.,
};
double box_blur_kernel[3*3] = {
  1.0, 1.0, 1.0,
  1.0, 1.0, 1.0,
  1.0, 1.0, 1.0,
};

double *filters[4] = {
  emboss_kernel, sharpen_kernel, sobel_emboss_kernel, box_blur_kernel
};
const double filter_params[2*4] = {
  1.0, 0.0,
  1.0, 0.0,
  1.0, 0.5,
  9.0, 0.0
};

int main()
{
  image ii, oi;
  int i;
  char lennanames[30];

  ii = read_image(input);
  if ( ii != NULL ) {
    for(i=0; i<4; i++) {
      sprintf(lennanames, output, i);
      oi = filter(ii, filters[i], 1, filter_params[2*i], filter_params[2*i+1]);
      if ( oi != NULL ) {
	FILE *outfh = fopen(lennanames, "w");
	if ( outfh != NULL ) {
	  output_ppm(outfh, oi);
	  fclose(outfh);
	} else { fprintf(stderr, "out err %s\n", output); }
	free_img(oi);
      } else { fprintf(stderr, "err creating img filters %d\n", i); }
    }
    free_img(ii);
  } else { fprintf(stderr, "err reading %s\n", input); }
}
```


## Common Lisp

Uses the RGB pixel buffer package defined here [[Basic bitmap storage#Common Lisp]]. Also the PPM file IO functions defined in
[[Bitmap/Read a PPM file#Common_Lisp]] and [[Bitmap/Write a PPM file#Common_Lisp]] merged into one package.

```lisp
(load "rgb-pixel-buffer")
(load "ppm-file-io")

(defpackage #:convolve
  (:use #:common-lisp #:rgb-pixel-buffer #:ppm-file-io))

(in-package #:convolve)
(defconstant +row-offsets+ '(-1 -1 -1 0 0 0 1 1 1))
(defconstant +col-offsets+ '(-1 0 1 -1 0 1 -1 0 1))
(defstruct cnv-record descr width kernel divisor offset)
(defparameter *cnv-lib* (make-hash-table))
(setf (gethash 'emboss *cnv-lib*)
      (make-cnv-record :descr "emboss-filter" :width 3
                       :kernel '(-2.0 -1.0 0.0 -1.0 1.0 1.0 0.0 1.0 2.0) :divisor 1.0))
(setf (gethash 'sharpen *cnv-lib*)
      (make-cnv-record :descr "sharpen-filter" :width 3
                       :kernel '(-1.0 -1.0 -1.0 -1.0 9.0 -1.0 -1.0 -1.0 -1.0) :divisor 1.0))
(setf (gethash 'sobel-emboss *cnv-lib*)
      (make-cnv-record :descr "sobel-emboss-filter" :width 3
                       :kernel '(-1.0 -2.0 -1.0 0.0 0.0 0.0 1.0 2.0 1.0 :divisor 1.0 :offset 0.5)))
(setf (gethash 'box-blur *cnv-lib*)
      (make-cnv-record :descr "box-blur-filter" :width 3
                       :kernel '(1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0) :divisor 9.0))

(defun convolve (filename params)
  (let* ((buf (read-ppm-file-to-rgb-pixel-buffer filename))
         (width (first (array-dimensions buf)))
         (height (second (array-dimensions buf)))
         (obuf (make-rgb-pixel-buffer width height)))

    ;;; constrain a value to some range
    ;;; (int,int,int)->int
    (defun constrain (val minv maxv)
      (declare (type integer val minv maxv))
      (min maxv (max minv val)))

    ;;; convolve a single channel
    ;;; list ubyte8->ubyte8
    (defun convolve-channel (band)
      (constrain (round (apply #'+ (mapcar #'* band (cnv-record-kernel params)))) 0 255))

    ;;; return the rgb convolution of a list of pixels
    ;;; list uint24->uint24
    (defun convolve-pixels (pixels)
      (let ((reds (list)) (greens (list)) (blues (list)))
        (dolist (pel (reverse pixels))
          (push (rgb-pixel-red pel) reds)
          (push (rgb-pixel-green pel) greens)
          (push (rgb-pixel-blue pel) blues))
        (make-rgb-pixel (convolve-channel reds) (convolve-channel greens) (convolve-channel blues))))

    ;;; return the list of pixels to which the kernel will be applied
    ;;; (int,int)->list uint24
    (defun kernel-pixels (c r)
      (mapcar (lambda (coff roff) (rgb-pixel buf (constrain (+ c coff) 0 (1- width)) (constrain (+ r roff) 0 (1- height))))
              +col-offsets+ +row-offsets+))

    ;;; body of function
    (dotimes (r height)
      (dotimes (c width)
        (setf (rgb-pixel obuf c r) (convolve-pixels (kernel-pixels c r)))))

    (write-rgb-pixel-buffer-to-ppm-file (concatenate 'string (format nil "convolve-~A-" (cnv-record-descr params)) filename) obuf)))

(in-package #:cl-user)
(defun main ()
  (loop for pars being the hash-values of convolve::*cnv-lib*
    do (princ (convolve::convolve "lena_color.ppm" pars)) (terpri)))

```



## D

This requires the module from the Grayscale Image Task.

```d
import std.string, std.math, std.algorithm, grayscale_image;

struct ConvolutionFilter {
    double[][] kernel;
    double divisor, offset_;
    string name;
}


Image!Color convolve(Color)(in Image!Color im,
                            in ConvolutionFilter filter)
pure nothrow in {
    assert(im !is null);
    assert(!filter.divisor.isNaN && !filter.offset_.isNaN);
    assert(filter.divisor != 0);
    assert(filter.kernel.length > 0 && filter.kernel[0].length > 0);
    foreach (const row; filter.kernel) // Is rectangular.
        assert(row.length == filter.kernel[0].length);
    assert(filter.kernel.length % 2 == 1); // Odd sized kernel.
    assert(filter.kernel[0].length % 2 == 1);
    assert(im.ny >= filter.kernel.length);
    assert(im.nx >= filter.kernel[0].length);
} out(result) {
    assert(result !is null);
    assert(result.nx == im.nx && result.ny == im.ny);
} body {
    immutable knx2 = filter.kernel[0].length / 2;
    immutable kny2 = filter.kernel.length / 2;
    auto io = new Image!Color(im.nx, im.ny);

    static if (is(Color == RGB))
        alias CT = typeof(Color.r); // Component type.
    else static if (is(typeof(Color.c)))
        alias CT = typeof(Color.c);
    else
        alias CT = Color;

    foreach (immutable y; kny2 .. im.ny - kny2) {
        foreach (immutable x; knx2 .. im.nx - knx2) {
            static if (is(Color == RGB))
                double[3] total = 0.0;
            else
                double total = 0.0;

            foreach (immutable sy, const kRow; filter.kernel) {
                foreach (immutable sx, immutable k; kRow) {
                    immutable p = im[x + sx - knx2, y + sy - kny2];
                    static if (is(Color == RGB)) {
                        total[0] += p.r * k;
                        total[1] += p.g * k;
                        total[2] += p.b * k;
                    } else {
                        total += p * k;
                    }
                }
            }

            immutable D = filter.divisor;
            immutable O = filter.offset_ * CT.max;
            static if (is(Color == RGB)) {
                io[x, y] = Color(
                    cast(CT)min(max(total[0]/ D + O, CT.min), CT.max),
                    cast(CT)min(max(total[1]/ D + O, CT.min), CT.max),
                    cast(CT)min(max(total[2]/ D + O, CT.min), CT.max));
            } else static if (is(typeof(Color.c))) {
                io[x, y] = Color(
                    cast(CT)min(max(total / D + O, CT.min), CT.max));
            } else {
                // If Color doesn't have a 'c' field, then Color is
                // assumed to be a built-in type.
                io[x, y] =
                    cast(CT)min(max(total / D + O, CT.min), CT.max);
            }
        }
    }

    return io;
}


void main() {
    immutable ConvolutionFilter[] filters = [
        {[[-2.0, -1.0, 0.0],
          [-1.0,  1.0, 1.0],
          [ 0.0,  1.0, 2.0]], divisor:1.0, offset_:0.0, name:"Emboss"},

        {[[-1.0, -1.0, -1.0],
          [-1.0,  9.0, -1.0],
          [-1.0, -1.0, -1.0]], divisor:1.0, 0.0, "Sharpen"},

        {[[-1.0, -2.0, -1.0],
          [ 0.0,  0.0,  0.0],
          [ 1.0,  2.0,  1.0]], divisor:1.0, 0.5, "Sobel_emboss"},

        {[[1.0, 1.0, 1.0],
          [1.0, 1.0, 1.0],
          [1.0, 1.0, 1.0]], divisor:9.0, 0.0, "Box_blur"},

        {[[1,  4,  7,  4, 1],
          [4, 16, 26, 16, 4],
          [7, 26, 41, 26, 7],
          [4, 16, 26, 16, 4],
          [1,  4,  7,  4, 1]], divisor:273, 0.0, "Gaussian_blur"}];

    Image!RGB im;
    im.loadPPM6("Lenna100.ppm");

    foreach (immutable filter; filters)
        im.convolve(filter)
        .savePPM6(format("lenna_%s.ppm", filter.name));

    const img = im.rgb2grayImage();
    foreach (immutable filter; filters)
        img.convolve(filter)
        .savePGM(format("lenna_gray_%s.ppm", filter.name));
}
```



## Go

Using standard image library:

```go
package main

import (
    "fmt"
    "image"
    "image/color"
    "image/jpeg"
    "math"
    "os"
)

// kf3 is a generic convolution 3x3 kernel filter that operatates on
// images of type image.Gray from the Go standard image library.
func kf3(k *[9]float64, src, dst *image.Gray) {
    for y := src.Rect.Min.Y; y < src.Rect.Max.Y; y++ {
        for x := src.Rect.Min.X; x < src.Rect.Max.X; x++ {
            var sum float64
            var i int
            for yo := y - 1; yo <= y+1; yo++ {
                for xo := x - 1; xo <= x+1; xo++ {
                    if (image.Point{xo, yo}).In(src.Rect) {
                        sum += k[i] * float64(src.At(xo, yo).(color.Gray).Y)
                    } else {
                        sum += k[i] * float64(src.At(x, y).(color.Gray).Y)
                    }
                    i++
                }
            }
            dst.SetGray(x, y,
                color.Gray{uint8(math.Min(255, math.Max(0, sum)))})
        }
    }
}

var blur = [9]float64{
    1. / 9, 1. / 9, 1. / 9,
    1. / 9, 1. / 9, 1. / 9,
    1. / 9, 1. / 9, 1. / 9}

// blurY example function applies blur kernel to Y channel
// of YCbCr image using generic kernel filter function kf3
func blurY(src *image.YCbCr) *image.YCbCr {
    dst := *src

    // catch zero-size image here
    if src.Rect.Max.X == src.Rect.Min.X || src.Rect.Max.Y == src.Rect.Min.Y {
        return &dst
    }

    // pass Y channels as gray images
    srcGray := image.Gray{src.Y, src.YStride, src.Rect}
    dstGray := srcGray
    dstGray.Pix = make([]uint8, len(src.Y))
    kf3(&blur, &srcGray, &dstGray) // call generic convolution function

    // complete result
    dst.Y = dstGray.Pix                   // convolution result
    dst.Cb = append([]uint8{}, src.Cb...) // Cb, Cr are just copied
    dst.Cr = append([]uint8{}, src.Cr...)
    return &dst
}

func main() {
    // Example file used here is Lenna100.jpg from the task "Percentage
    // difference between images"
    f, err := os.Open("Lenna100.jpg")
    if err != nil {
        fmt.Println(err)
        return
    }
    img, err := jpeg.Decode(f)
    if err != nil {
        fmt.Println(err)
        return
    }
    f.Close()
    y, ok := img.(*image.YCbCr)
    if !ok {
        fmt.Println("expected color jpeg")
        return
    }
    f, err = os.Create("blur.jpg")
    if err != nil {
        fmt.Println(err)
        return
    }
    err = jpeg.Encode(f, blurY(y), &jpeg.Options{90})
    if err != nil {
        fmt.Println(err)
    }
}
```

Alternative version, building on code from bitmap task.

New function for raster package:

```go
package raster

import "math"

func (g *Grmap) KernelFilter3(k []float64) *Grmap {
    if len(k) != 9 {
        return nil
    }
    r := NewGrmap(g.cols, g.rows)
    r.Comments = append([]string{}, g.Comments...)
    // Filter edge pixels with minimal code.
    // Execution time per pixel is high but there are few edge pixels
    // relative to the interior.
    o3 := [][]int{
        {-1, -1}, {0, -1}, {1, -1},
        {-1, 0}, {0, 0}, {1, 0},
        {-1, 1}, {0, 1}, {1, 1}}
    edge := func(x, y int) uint16 {
        var sum float64
        for i, o := range o3 {
            c, ok := g.GetPx(x+o[0], y+o[1])
            if !ok {
                c = g.pxRow[y][x]
            }
            sum += float64(c) * k[i]
        }
        return uint16(math.Min(math.MaxUint16, math.Max(0,sum)))
    }
    for x := 0; x < r.cols; x++ {
        r.pxRow[0][x] = edge(x, 0)
        r.pxRow[r.rows-1][x] = edge(x, r.rows-1)
    }
    for y := 1; y < r.rows-1; y++ {
        r.pxRow[y][0] = edge(0, y)
        r.pxRow[y][r.cols-1] = edge(r.cols-1, y)
    }
    if r.rows < 3 || r.cols < 3 {
        return r
    }

    // Interior pixels can be filtered much more efficiently.
    otr := -g.cols + 1
    obr := g.cols + 1
    z := g.cols + 1
    c2 := g.cols - 2
    for y := 1; y < r.rows-1; y++ {
        tl := float64(g.pxRow[y-1][0])
        tc := float64(g.pxRow[y-1][1])
        tr := float64(g.pxRow[y-1][2])
        ml := float64(g.pxRow[y][0])
        mc := float64(g.pxRow[y][1])
        mr := float64(g.pxRow[y][2])
        bl := float64(g.pxRow[y+1][0])
        bc := float64(g.pxRow[y+1][1])
        br := float64(g.pxRow[y+1][2])
        for x := 1; ; x++ {
            r.px[z] = uint16(math.Min(math.MaxUint16, math.Max(0,
                tl*k[0] + tc*k[1] + tr*k[2] +
                ml*k[3] + mc*k[4] + mr*k[5] +
                bl*k[6] + bc*k[7] + br*k[8])))
            if x == c2 {
                break
            }
            z++
            tl, tc, tr = tc, tr, float64(g.px[z+otr])
            ml, mc, mr = mc, mr, float64(g.px[z+1])
            bl, bc, br = bc, br, float64(g.px[z+obr])
        }
        z += 3
    }
    return r
}
```

Demonstration program:

```go
package main

// Files required to build supporting package raster are found in:
// * This task (immediately above)
// * Bitmap
// * Grayscale image
// * Read a PPM file
// * Write a PPM file

import (
    "fmt"
    "raster"
)

var blur = []float64{
    1./9, 1./9, 1./9,
    1./9, 1./9, 1./9,
    1./9, 1./9, 1./9}

var sharpen = []float64{
    -1, -1, -1,
    -1,  9, -1,
    -1, -1, -1}

func main() {
    // Example file used here is Lenna100.jpg from the task "Percentage
    // difference between images" converted with with the command
    // convert Lenna100.jpg -colorspace gray Lenna100.ppm
    b, err := raster.ReadPpmFile("Lenna100.ppm")
    if err != nil {
        fmt.Println(err)
        return
    }
    g0 := b.Grmap()
    g1 := g0.KernelFilter3(blur)
    err = g1.Bitmap().WritePpmFile("blur.ppm")
    if err != nil {
        fmt.Println(err)
    }
}
```



## J



```J
NB. pad the edges of an array with border pixels
NB. (increasing the first two dimensions by 1 less than the kernel size)
pad=: adverb define
  'a b'=. (<. ,. >.) 0.5 0.5 p. $m
  a"_`(0 , ] - 1:)`(# 1:)}~&# # b"_`(0 , ] - 1:)`(# 1:)}~&(1 { $) #"1 ]
)

kernel_filter=: adverb define
   ($m)+/ .*&(,m)&(,/);._3 m pad
)
```



This code assumes that the leading dimensions of the array represent pixels and any trailing dimensions represent structure to be preserved (this is a fairly common approach and matches the J implementation at [[Basic bitmap storage]]). Note also that we assume that the image is larger than a single pixel in both directions. Any sized kernel is supported (as long as it's at least one pixel in each direction).

Example use:


```J
   NB. kernels borrowed from C and TCL implementations
   sharpen_kernel=: _1+10*4=i.3 3
   blur_kernel=: 3 3$%9
   emboss_kernel=: _2 _1 0,_1 1 1,:0 1 2
   sobel_emboss_kernel=: _1 _2 _1,0,:1 2 1

   'blurred.ppm' writeppm~ blur_kernel kernel_filter readppm 'original.ppm'
```



## Java


'''Code:'''

```Java
import java.awt.image.*;
import java.io.File;
import java.io.IOException;
import javax.imageio.*;

public class ImageConvolution
{
  public static class ArrayData
  {
    public final int[] dataArray;
    public final int width;
    public final int height;

    public ArrayData(int width, int height)
    {
      this(new int[width * height], width, height);
    }

    public ArrayData(int[] dataArray, int width, int height)
    {
      this.dataArray = dataArray;
      this.width = width;
      this.height = height;
    }

    public int get(int x, int y)
    {  return dataArray[y * width + x];  }

    public void set(int x, int y, int value)
    {  dataArray[y * width + x] = value;  }
  }

  private static int bound(int value, int endIndex)
  {
    if (value < 0)
      return 0;
    if (value < endIndex)
      return value;
    return endIndex - 1;
  }

  public static ArrayData convolute(ArrayData inputData, ArrayData kernel, int kernelDivisor)
  {
    int inputWidth = inputData.width;
    int inputHeight = inputData.height;
    int kernelWidth = kernel.width;
    int kernelHeight = kernel.height;
    if ((kernelWidth <= 0) || ((kernelWidth & 1) != 1))
      throw new IllegalArgumentException("Kernel must have odd width");
    if ((kernelHeight <= 0) || ((kernelHeight & 1) != 1))
      throw new IllegalArgumentException("Kernel must have odd height");
    int kernelWidthRadius = kernelWidth >>> 1;
    int kernelHeightRadius = kernelHeight >>> 1;

    ArrayData outputData = new ArrayData(inputWidth, inputHeight);
    for (int i = inputWidth - 1; i >= 0; i--)
    {
      for (int j = inputHeight - 1; j >= 0; j--)
      {
        double newValue = 0.0;
        for (int kw = kernelWidth - 1; kw >= 0; kw--)
          for (int kh = kernelHeight - 1; kh >= 0; kh--)
            newValue += kernel.get(kw, kh) * inputData.get(
                          bound(i + kw - kernelWidthRadius, inputWidth),
                          bound(j + kh - kernelHeightRadius, inputHeight));
        outputData.set(i, j, (int)Math.round(newValue / kernelDivisor));
      }
    }
    return outputData;
  }

  public static ArrayData[] getArrayDatasFromImage(String filename) throws IOException
  {
    BufferedImage inputImage = ImageIO.read(new File(filename));
    int width = inputImage.getWidth();
    int height = inputImage.getHeight();
    int[] rgbData = inputImage.getRGB(0, 0, width, height, null, 0, width);
    ArrayData reds = new ArrayData(width, height);
    ArrayData greens = new ArrayData(width, height);
    ArrayData blues = new ArrayData(width, height);
    for (int y = 0; y < height; y++)
    {
      for (int x = 0; x < width; x++)
      {
        int rgbValue = rgbData[y * width + x];
        reds.set(x, y, (rgbValue >>> 16) & 0xFF);
        greens.set(x, y, (rgbValue >>> 8) & 0xFF);
        blues.set(x, y, rgbValue & 0xFF);
      }
    }
    return new ArrayData[] { reds, greens, blues };
  }

  public static void writeOutputImage(String filename, ArrayData[] redGreenBlue) throws IOException
  {
    ArrayData reds = redGreenBlue[0];
    ArrayData greens = redGreenBlue[1];
    ArrayData blues = redGreenBlue[2];
    BufferedImage outputImage = new BufferedImage(reds.width, reds.height,
                                                  BufferedImage.TYPE_INT_ARGB);
    for (int y = 0; y < reds.height; y++)
    {
      for (int x = 0; x < reds.width; x++)
      {
        int red = bound(reds.get(x, y), 256);
        int green = bound(greens.get(x, y), 256);
        int blue = bound(blues.get(x, y), 256);
        outputImage.setRGB(x, y, (red << 16) | (green << 8) | blue | -0x01000000);
      }
    }
    ImageIO.write(outputImage, "PNG", new File(filename));
    return;
  }

  public static void main(String[] args) throws IOException
  {
    int kernelWidth = Integer.parseInt(args[2]);
    int kernelHeight = Integer.parseInt(args[3]);
    int kernelDivisor = Integer.parseInt(args[4]);
    System.out.println("Kernel size: " + kernelWidth + "x" + kernelHeight +
                       ", divisor=" + kernelDivisor);
    int y = 5;
    ArrayData kernel = new ArrayData(kernelWidth, kernelHeight);
    for (int i = 0; i < kernelHeight; i++)
    {
      System.out.print("[");
      for (int j = 0; j < kernelWidth; j++)
      {
        kernel.set(j, i, Integer.parseInt(args[y++]));
        System.out.print(" " + kernel.get(j, i) + " ");
      }
      System.out.println("]");
    }

    ArrayData[] dataArrays = getArrayDatasFromImage(args[0]);
    for (int i = 0; i < dataArrays.length; i++)
      dataArrays[i] = convolute(dataArrays[i], kernel, kernelDivisor);
    writeOutputImage(args[1], dataArrays);
    return;
  }
}
```



[[Image:JavaImageConvolution.png|320px240px|thumb|right|Output from example pentagon image]]'''Example 5x5 Gaussian blur, using Pentagon.png from the Hough transform task:'''

```txt
java ImageConvolution pentagon.png JavaImageConvolution.png 5 5 273 1 4 7 4 1  4 16 26 16 4  7 26 41 26 7  4 16 26 16 4  1 4 7 4 1
Kernel size: 5x5, divisor=273
[ 1  4  7  4  1 ]
[ 4  16  26  16  4 ]
[ 7  26  41  26  7 ]
[ 4  16  26  16  4 ]
[ 1  4  7  4  1 ]
```



## JavaScript


'''Code:'''

```javascript
// Image imageIn, Array kernel, function (Error error, Image imageOut)
// precondition: Image is loaded
// returns loaded Image to asynchronous callback function
function convolve(imageIn, kernel, callback) {
    var dim = Math.sqrt(kernel.length),
        pad = Math.floor(dim / 2);

    if (dim % 2 !== 1) {
        return callback(new RangeError("Invalid kernel dimension"), null);
    }

    var w = imageIn.width,
        h = imageIn.height,
        can = document.createElement('canvas'),
        cw,
        ch,
        ctx,
        imgIn, imgOut,
        datIn, datOut;

    can.width = cw = w + pad * 2; // add padding
    can.height = ch = h + pad * 2; // add padding

    ctx = can.getContext('2d');
    ctx.fillStyle = '#000'; // fill with opaque black
    ctx.fillRect(0, 0, cw, ch);
    ctx.drawImage(imageIn, pad, pad);

    imgIn = ctx.getImageData(0, 0, cw, ch);
    datIn = imgIn.data;

    imgOut = ctx.createImageData(w, h);
    datOut = imgOut.data;

    var row, col, pix, i, dx, dy, r, g, b;

    for (row = pad; row <= h; row++) {
        for (col = pad; col <= w; col++) {
            r = g = b = 0;

            for (dx = -pad; dx <= pad; dx++) {
                for (dy = -pad; dy <= pad; dy++) {
                    i = (dy + pad) * dim + (dx + pad); // kernel index
                    pix = 4 * ((row + dy) * cw + (col + dx)); // image index
                    r += datIn[pix++] * kernel[i];
                    g += datIn[pix++] * kernel[i];
                    b += datIn[pix  ] * kernel[i];
                }
            }

            pix = 4 * ((row - pad) * w + (col - pad)); // destination index
            datOut[pix++] = (r + .5) ^ 0;
            datOut[pix++] = (g + .5) ^ 0;
            datOut[pix++] = (b + .5) ^ 0;
            datOut[pix  ] = 255; // we want opaque image
        }
    }

    // reuse canvas
    can.width = w;
    can.height = h;

    ctx.putImageData(imgOut, 0, 0);

    var imageOut = new Image();

    imageOut.addEventListener('load', function () {
        callback(null, imageOut);
    });

    imageOut.addEventListener('error', function (error) {
        callback(error, null);
    });

    imageOut.src = can.toDataURL('image/png');
}
```


'''Example Usage:'''

```txt
var image = new Image();

image.addEventListener('load', function () {
    image.alt = 'Player';
    document.body.appendChild(image);

    // laplace filter
    convolve(image,
             [0, 1, 0,
              1,-4, 1,
              0, 1, 0],
             function (error, result) {
                 if (error !== null) {
                     console.error(error);
                 } else {
                     result.alt = 'Boundary';
                     document.body.appendChild(result);
                 }
             }
    );
});

image.src = '/img/player.png';
```



## Julia


```julia

using FileIO, Images

img = load("image.jpg")

sharpenkernel = reshape([-1.0, -1.0, -1.0, -1.0,  9.0, -1.0, -1.0, -1.0, -1.0], (3,3))

imfilt = imfilter(img, sharpenkernel)

save("imagesharper.png", imfilt)

```



## Kotlin

```scala
// version 1.2.10

import kotlin.math.round
import java.awt.image.*
import java.io.File
import javax.imageio.*

class ArrayData(val width: Int, val height: Int) {
    var dataArray = IntArray(width * height)

    operator fun get(x: Int, y: Int) = dataArray[y * width + x]

    operator fun set(x: Int, y: Int, value: Int) {
        dataArray[y * width + x] = value
    }
}

fun bound(value: Int, endIndex: Int) = when {
    value < 0        -> 0
    value < endIndex -> value
    else             -> endIndex - 1
}

fun convolute(
    inputData: ArrayData,
    kernel: ArrayData,
    kernelDivisor: Int
): ArrayData {
    val inputWidth = inputData.width
    val inputHeight = inputData.height
    val kernelWidth = kernel.width
    val kernelHeight = kernel.height
    if (kernelWidth <= 0 || (kernelWidth and 1) != 1)
        throw IllegalArgumentException("Kernel must have odd width")
    if (kernelHeight <= 0 || (kernelHeight and 1) != 1)
        throw IllegalArgumentException("Kernel must have odd height")
    val kernelWidthRadius = kernelWidth ushr 1
    val kernelHeightRadius = kernelHeight ushr 1

    val outputData = ArrayData(inputWidth, inputHeight)
    for (i in inputWidth - 1 downTo 0) {
        for (j in inputHeight - 1 downTo 0) {
            var newValue = 0.0
            for (kw in kernelWidth - 1 downTo 0) {
                for (kh in kernelHeight - 1 downTo 0) {
                    newValue += kernel[kw, kh] * inputData[
                        bound(i + kw - kernelWidthRadius, inputWidth),
                        bound(j + kh - kernelHeightRadius, inputHeight)
                    ].toDouble()
                    outputData[i, j] = round(newValue / kernelDivisor).toInt()
                }
            }
        }
    }
    return outputData
}

fun getArrayDatasFromImage(filename: String): Array<ArrayData> {
    val inputImage = ImageIO.read(File(filename))
    val width = inputImage.width
    val height = inputImage.height
    val rgbData = inputImage.getRGB(0, 0, width, height, null, 0, width)
    val reds = ArrayData(width, height)
    val greens = ArrayData(width, height)
    val blues = ArrayData(width, height)
    for (y in 0 until height) {
        for (x in 0 until width) {
            val rgbValue = rgbData[y * width + x]
            reds[x, y] = (rgbValue ushr 16) and 0xFF
            greens[x,y] = (rgbValue ushr 8) and 0xFF
            blues[x, y] = rgbValue and 0xFF
        }
    }
    return arrayOf(reds, greens, blues)
}

fun writeOutputImage(filename: String, redGreenBlue: Array<ArrayData>) {
    val (reds, greens, blues) = redGreenBlue
    val outputImage = BufferedImage(
        reds.width, reds.height, BufferedImage.TYPE_INT_ARGB
    )
    for (y in 0 until reds.height) {
        for (x in 0 until reds.width) {
            val red = bound(reds[x , y], 256)
            val green = bound(greens[x , y], 256)
            val blue = bound(blues[x, y], 256)
            outputImage.setRGB(
                x, y, (red shl 16) or (green shl 8) or blue or -0x01000000
            )
        }
    }
    ImageIO.write(outputImage, "PNG", File(filename))
}

fun main(args: Array<String>) {
    val kernelWidth = args[2].toInt()
    val kernelHeight = args[3].toInt()
    val kernelDivisor = args[4].toInt()
    println("Kernel size: $kernelWidth x $kernelHeight, divisor = $kernelDivisor")
    var y = 5
    val kernel = ArrayData(kernelWidth, kernelHeight)
    for (i in 0 until kernelHeight) {
        print("[")
        for (j in 0 until kernelWidth) {
            kernel[j, i] = args[y++].toInt()
            print(" ${kernel[j, i]} ")
        }
        println("]")
    }

    val dataArrays = getArrayDatasFromImage(args[0])
    for (i in 0 until dataArrays.size) {
        dataArrays[i] = convolute(dataArrays[i], kernel, kernelDivisor)
    }
    writeOutputImage(args[1], dataArrays)
}
```


```txt

Same as Java entry when using identical command line arguments

```



## Liberty BASIC

In the following a 128x128 bmp file is loaded and its brightness values are read into an array.


We then convolve it with a 'sharpen' 3x3 matrix. Results are shown directly on screen.


NB Things like convolution would be best done by combining LB with ImageMagick, which is easily called from LB.

```lb

    dim result( 300, 300), image( 300, 300), mask( 100, 100)
    w =128
    h =128

    nomainwin

    WindowWidth  = 460
    WindowHeight = 210

    open "Convolution" for graphics_nsb_nf as #w

    #w "trapclose [quit]"

    #w "down ; fill darkblue"

    hw = hwnd( #w)
    calldll #user32,"GetDC", hw as ulong, hdc as ulong

    loadbmp "img", "alpha25.bmp"'   128x128 pixels
    #w "drawbmp img   20, 20"

    #w "up ; color white ; goto 292 20 ; down ; box 420 148"
    #w "up ; goto 180 60 ; down ; backcolor darkblue ; color cyan"
    #w "\"; "Convolved with"

    for y =0 to 127 '   fill in the input matrix
        for x =0 to 127
            xx =x + 20
            yy =y + 20
            CallDLL #gdi32, "GetPixel", hdc as uLong, xx as long, yy as long, pixcol as ulong
            call getRGB pixcol, b, g, r
            image( x, y) =b
            '#w "color "; image( x, y); " 0 "; 255 -image( x, y)
            '#w "set "; x + 20; " "; y +20 +140
        next x
    next y

    #w "flush"
    print " Input matrix filled."

    #w "size 8"
    for y =0 to 2  '   fill in the mask matrix
        for x =0 to 2
            read mask
            mask( x, y) =mask
            if mask = ( 0 -1) then #w "color yellow" else #w "color red"
            #w "set "; 8 *x +200; " "; 8 *y +80
        next x
    next y
    data -1,-1,-1,-1,9,-1,-1,-1,-1

    #w "flush"
    print " Mask matrix filled."

    #w "size 1"
    mxx =0: mnn =0

    for x =0 to 127 -2 '   since any further overlaps image edge
        for y =0 to 127 -2
            result( x, y) =0
            for kx =0 to 2
                for ky =0 to 2
                    result( x, y) =result( x, y) +image( x +kx, y +ky) *mask( kx, ky)
                next ky
                if mxx <result( x, y) then mxx =result( x, y)
                if mnn >result( x, y) then mnn =result( x, y)
            next kx
            scan
        next y
    next x

    range =mxx -mnn
    for x =0 to 127 -2
        for y =0 to 127 -2
            c =int( 255 *( result( x, y) -mnn) /range)
            '#w "color "; c; " "; c; " "; c
            if c >128 then #w "color white" else #w "color black"
            #w "set "; x +292 +1; " "; y +20 +1
            scan
        next y
    next x
    #w "flush"

    wait

    sub getRGB pixcol, byref r, byref g, byref b
        b = int( pixcol / (256 *256))
        g = int( ( pixcol - b *256 *256) / 256)
        r = int( pixcol - b *256 *256 - g *256)
    end sub

    [quit]
    close #w
    CallDLL #user32, "ReleaseDC", hw as ulong, hdc as ulong
    end

```

 Screenview is available at [[http://www.diga.me.uk/convolved.gif]]


## Maple

Builtin command ImageTools:-Convolution()

```Maple
pic:=Import("smiling_dog.jpg"):
mask := Matrix([[1,2,3],[4,5,6],[7,8,9]]);
pic := ImageTools:-Convolution(pic, mask);
```



=={{header|Mathematica}} / {{header|Wolfram Language}}==
Most image processing functions introduced in Mathematica 7

```mathematica
img = Import[NotebookDirectory[] <> "Lenna50.jpg"];
kernel = {{0, -1, 0}, {-1, 4, -1}, {0, -1, 0}};
ImageConvolve[img, kernel]
ImageConvolve[img, GaussianMatrix[35] ]
ImageConvolve[img, BoxMatrix[1] ]
```



## MATLAB

The built-in function [http://www.mathworks.com/help/matlab/ref/conv2.html conv2] handles the basic convolution. Below is a program that has several more options that may be useful in different image processing applications (see comments under convImage for specifics).

```MATLAB
function testConvImage
    Im = [1 2 1 5 5 ; ...
          1 2 7 9 9 ; ...
          5 5 5 5 5 ; ...
          5 2 2 2 2 ; ...
          1 1 1 1 1 ];      % Sample image for example illustration only
    Ker = [1 2 1 ; ...
           2 4 2 ; ...
           1 2 1 ];         % Gaussian smoothing (without normalizing)
    fprintf('Original image:\n')
    disp(Im)
    fprintf('Original kernel:\n')
    disp(Ker)
    fprintf('Padding with zeroes:\n')
    disp(convImage(Im, Ker, 'zeros'))
    fprintf('Padding with fives:\n')
    disp(convImage(Im, Ker, 'value', 5))
    fprintf('Duplicating border pixels to pad image:\n')
    disp(convImage(Im, Ker, 'extend'))
    fprintf('Renormalizing kernel and using only values within image:\n')
    disp(convImage(Im, Ker, 'partial'))
    fprintf('Only processing inner (non-border) pixels:\n')
    disp(convImage(Im, Ker, 'none'))
%     Ker = [1 2 1 ; ...
%            2 4 2 ; ...
%            1 2 1 ]./16;
%     Im = imread('testConvImageTestImage.png', 'png');
%     figure
%     imshow(imresize(Im, 10))
%     title('Original image')
%     figure
%     imshow(imresize(convImage(Im, Ker, 'zeros'), 10))
%     title('Padding with zeroes')
%     figure
%     imshow(imresize(convImage(Im, Ker, 'value', 50), 10))
%     title('Padding with fifty: 50')
%     figure
%     imshow(imresize(convImage(Im, Ker, 'extend'), 10))
%     title('Duplicating border pixels to pad image')
%     figure
%     imshow(imresize(convImage(Im, Ker, 'partial'), 10))
%     title('Renormalizing kernel and using only values within image')
%     figure
%     imshow(imresize(convImage(Im, Ker, 'none'), 10))
%     title('Only processing inner (non-border) pixels')
end

function ImOut = convImage(Im, Ker, varargin)
% ImOut = convImage(Im, Ker)
%   Filters an image using sliding-window kernel convolution.
%   Convolution is done layer-by-layer. Use rgb2gray if single-layer needed.
%   Zero-padding convolution will be used if no border handling is specified.
%   Im - Array containing image data (output from imread)
%   Ker - 2-D array to convolve image, needs odd number of rows and columns
%   ImOut - Filtered image, same dimensions and datatype as Im
%
% ImOut = convImage(Im, Ker, 'zeros')
%   Image will be padded with zeros when calculating convolution
%   (useful for magnitude calculations).
%
% ImOut = convImage(Im, Ker, 'value', padVal)
%   Image will be padded with padVal when calculating convolution
%   (possibly useful for emphasizing certain data with unusual kernel)
%
% ImOut = convImage(Im, Ker, 'extend')
%   Image will be padded with the value of the closest image pixel
%   (useful for smoothing or blurring filters).
%
% ImOut = convImage(Im, Ker, 'partial')
%   Image will not be padded. Borders will be convoluted with only valid pixels,
%   and convolution matrix will be renormalized counting only the pixels within
%   the image (also useful for smoothing or blurring filters).
%
% ImOut = convImage(Im, Ker, 'none')
%   Image will not be padded. Convolution will only be applied to inner pixels
%   (useful for edge and corner detection filters)

    % Handle input
    if mod(size(Ker, 1), 2) ~= 1 || mod(size(Ker, 2), 2) ~= 1
        eid = sprintf('%s:evenRowsCols', mfilename);
        error(eid,'''Ker'' parameter must have odd number of rows and columns.')
    elseif nargin > 4
        eid = sprintf('%s:maxrhs', mfilename);
        error(eid, 'Too many input arguments.');
    elseif nargin == 4 && ~strcmp(varargin{1}, 'value')
        eid = sprintf('%s:invalidParameterCombination', mfilename);
        error(eid, ['The ''padVal'' parameter is only valid with the ' ...
            '''value'' option.'])
    elseif nargin < 4 && strcmp(varargin{1}, 'value')
        eid = sprintf('%s:minrhs', mfilename);
        error(eid, 'Not enough input arguments.')
    elseif nargin < 3
        method = 'zeros';
    else
        method = lower(varargin{1});
        if ~any(strcmp(method, {'zeros' 'value' 'extend' 'partial' 'none'}))
            eid = sprintf('%s:invalidParameter', mfilename);
            error(eid, 'Invalid option parameter. Must be one of:%s', ...
                sprintf('\n\t\t%s', ...
                'zeros', 'value', 'extend', 'partial', 'none'))
        end
    end

    % Gather information and prepare for convolution
    [nImRows, nImCols, nImLayers] = size(Im);
    classIm = class(Im);
    Im = double(Im);
    ImOut = zeros(nImRows, nImCols, nImLayers);
    [nKerRows, nKerCols] = size(Ker);
    nPadRows = nImRows+nKerRows-1;
    nPadCols = nImCols+nKerCols-1;
    padH = (nKerRows-1)/2;
    padW = (nKerCols-1)/2;

    % Convolute on a layer-by-layer basis
    for k = 1:nImLayers
        if strcmp(method, 'zeros')
            ImOut(:, :, k) = conv2(Im(:, :, k), Ker, 'same');
        elseif strcmp(method, 'value')
            padding = varargin{2}.*ones(nPadRows, nPadCols);
            padding(padH+1:end-padH, padW+1:end-padW) = Im(:, :, k);
            ImOut(:, :, k) = conv2(padding, Ker, 'valid');
        elseif strcmp(method, 'extend')
            padding = zeros(nPadRows, nPadCols);
            padding(padH+1:end-padH, padW+1:end-padW) = Im(:, :, k);  % Middle
            padding(1:padH, 1:padW) = Im(1, 1, k);                    % TopLeft
            padding(end-padH+1:end, 1:padW) = Im(end, 1, k);          % BotLeft
            padding(1:padH, end-padW+1:end) = Im(1, end, k);          % TopRight
            padding(end-padH+1:end, end-padW+1:end) = Im(end, end, k);% BotRight
            padding(padH+1:end-padH, 1:padW) = ...
                repmat(Im(:, 1, k), 1, padW);                         % Left
            padding(padH+1:end-padH, end-padW+1:end) = ...
                repmat(Im(:, end, k), 1, padW);                       % Right
            padding(1:padH, padW+1:end-padW) = ...
                repmat(Im(1, :, k), padH, 1);                         % Top
            padding(end-padH+1:end, padW+1:end-padW) = ...
                repmat(Im(end, :, k), padH, 1);                       % Bottom
            ImOut(:, :, k) = conv2(padding, Ker, 'valid');
        elseif strcmp(method, 'partial')
            ImOut(padH+1:end-padH, padW+1:end-padW, k) = ...
                conv2(Im(:, :, k), Ker, 'valid');                     % Middle
            unprocessed = true(nImRows, nImCols);
            unprocessed(padH+1:end-padH, padW+1:end-padW) = false;    % Border
            for r = 1:nImRows
                for c = 1:nImCols
                    if unprocessed(r, c)
                        limitedIm = Im(max(1, r-padH):min(nImRows, r+padH), ...
                            max(1, c-padW):min(nImCols, c+padW), k);
                        limitedKer = Ker(max(1, 2-r+padH): ...
                            min(nKerRows, nKerRows+nImRows-r-padH), ...
                            max(1, 2-c+padW):...
                            min(nKerCols, nKerCols+nImCols-c-padW));
                        limitedKer = limitedKer.*sum(Ker(:))./ ...
                            sum(limitedKer(:));
                        ImOut(r, c, k) = sum(sum(limitedIm.*limitedKer));
                    end
                end
            end
        else    % method is 'none'
            ImOut(:, :, k) = Im(:, :, k);
            ImOut(padH+1:end-padH, padW+1:end-padW, k) = ...
                conv2(Im(:, :, k), Ker, 'valid');
        end
    end

    % Convert back to former image data type
    ImOut = cast(ImOut, classIm);
end
```

```txt
Original image:
     1     2     1     5     5
     1     2     7     9     9
     5     5     5     5     5
     5     2     2     2     2
     1     1     1     1     1

Original kernel:
     1     2     1
     2     4     2
     1     2     1

Padding with zeroes:
    12    24    43    66    57
    27    50    79   104    84
    46    63    73    82    63
    42    46    40    40    30
    18    19    16    16    12

Padding with fives:
    47    44    63    86    92
    47    50    79   104   104
    66    63    73    82    83
    62    46    40    40    50
    53    39    36    36    47

Duplicating border pixels to pad image:
    20    30    52    82    96
    35    50    79   104   112
    62    63    73    82    84
    58    46    40    40    40
    29    23    20    20    20

Renormalizing kernel and using only values within image:
   21.3333   32.0000   57.3333   88.0000  101.3333
   36.0000   50.0000   79.0000  104.0000  112.0000
   61.3333   63.0000   73.0000   82.0000   84.0000
   56.0000   46.0000   40.0000   40.0000   40.0000
   32.0000   25.3333   21.3333   21.3333   21.3333

Only processing inner (non-border) pixels:
     1     2     1     5     5
     1    50    79   104     9
     5    63    73    82     5
     5    46    40    40     2
     1     1     1     1     1
```



## OCaml



```ocaml
let get_rgb img x y =
  let _, r_channel,_,_ = img in
  let width = Bigarray.Array2.dim1 r_channel
  and height = Bigarray.Array2.dim2 r_channel in
  if (x < 0) || (x >= width) then (0,0,0) else
  if (y < 0) || (y >= height) then (0,0,0) else  (* feed borders with black *)
  get_pixel img x y


let convolve_get_value img kernel divisor offset = fun x y ->
  let sum_r = ref 0.0
  and sum_g = ref 0.0
  and sum_b = ref 0.0 in

  for i = -1 to 1 do
    for j = -1 to 1 do
      let r, g, b = get_rgb img (x+i) (y+j) in
      sum_r := !sum_r +. kernel.(j+1).(i+1) *. (float r);
      sum_g := !sum_g +. kernel.(j+1).(i+1) *. (float g);
      sum_b := !sum_b +. kernel.(j+1).(i+1) *. (float b);
    done;
  done;
  ( !sum_r /. divisor +. offset,
    !sum_g /. divisor +. offset,
    !sum_b /. divisor +. offset )


let color_to_int (r,g,b) =
  (truncate r,
   truncate g,
   truncate b)

let bounded (r,g,b) =
  ((max 0 (min r 255)),
   (max 0 (min g 255)),
   (max 0 (min b 255)))


let convolve_value ~img ~kernel ~divisor ~offset =
  let _, r_channel,_,_ = img in
  let width = Bigarray.Array2.dim1 r_channel
  and height = Bigarray.Array2.dim2 r_channel in

  let res = new_img ~width ~height in

  let conv = convolve_get_value img kernel divisor offset in

  for y = 0 to pred height do
    for x = 0 to pred width do
      let color = conv x y in
      let color = color_to_int color in
      put_pixel res (bounded color) x y;
    done;
  done;
  (res)
```



```ocaml
let emboss img =
  let kernel = [|
    [| -2.; -1.;  0. |];
    [| -1.;  1.;  1. |];
    [|  0.;  1.;  2. |];
  |] in
  convolve_value ~img ~kernel ~divisor:1.0 ~offset:0.0;
;;

let sharpen img =
  let kernel = [|
    [| -1.; -1.; -1. |];
    [| -1.;  9.; -1. |];
    [| -1.; -1.; -1. |];
  |] in
  convolve_value ~img ~kernel ~divisor:1.0 ~offset:0.0;
;;

let sobel_emboss img =
  let kernel = [|
    [| -1.; -2.; -1. |];
    [|  0.;  0.;  0. |];
    [|  1.;  2.;  1. |];
  |] in
  convolve_value ~img ~kernel ~divisor:1.0 ~offset:0.5;
;;

let box_blur img =
  let kernel = [|
    [|  1.;  1.;  1. |];
    [|  1.;  1.;  1. |];
    [|  1.;  1.;  1. |];
  |] in
  convolve_value ~img ~kernel ~divisor:9.0 ~offset:0.0;
;;
```



## Octave


'''Use package''' [http://octave.sourceforge.net/image/index.html Image]


```octave
function [r, g, b] = rgbconv2(a, c)
    r = im2uint8(mat2gray(conv2(a(:,:,1), c)));
    g = im2uint8(mat2gray(conv2(a(:,:,2), c)));
    b = im2uint8(mat2gray(conv2(a(:,:,3), c)));
endfunction

im = jpgread("Lenna100.jpg");
emboss = [-2, -1,  0;
	  -1,  1,  1;
	  0,  1,  2 ];
sobel = [-1., -2., -1.;
	 0.,  0.,  0.;
	 1.,  2.,  1. ];
sharpen =   [ -1.0, -1.0, -1.0;
	     -1.0,  9.0, -1.0;
	     -1.0, -1.0, -1.0 ];

[r, g, b] = rgbconv2(im, emboss);
jpgwrite("LennaEmboss.jpg", r, g, b, 100);
[r, g, b] = rgbconv2(im, sobel);
jpgwrite("LennaSobel.jpg", r, g, b, 100);
[r, g, b] = rgbconv2(im, sharpen);
jpgwrite("LennaSharpen.jpg", r, g, b, 100);
```



## Perl


```perl
use strict;
use warnings;

use PDL;
use PDL::Image2D;

my $kernel = pdl [[-2, -1, 0],[-1, 1, 1], [0, 1, 2]]; # emboss

my $image = rpic 'pythagoras_tree.png';
my $smoothed = conv2d $image, $kernel, {Boundary => 'Truncate'};
wpic $smoothed, 'pythagoras_convolution.png';
```

Compare offsite images: [https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/frog.png frog.png] vs.
[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/frog_convolution.png frog_convolution.png]


## Perl 6


```perl6
use PDL:from<Perl5>
;
use PDL::Image2D:from<Perl5>;

my $kernel = pdl [[-2, -1, 0],[-1, 1, 1], [0, 1, 2]]; # emboss

my $image = rpic 'frog.png';
my $smoothed = conv2d $image, $kernel, {Boundary => 'Truncate'};
wpic $smoothed, 'frog_convolution.png';
```

Compare offsite images: [https://github.com/SqrtNegInf/Rosettacode-Perl6-Smoke/blob/master/ref/frog.png frog.png] vs.
[https://github.com/SqrtNegInf/Rosettacode-Perl6-Smoke/blob/master/ref/frog_convolution.png frog_convolution.png]


## Phix

```Phix
-- demo\rosetta\Image_convolution.exw
include pGUI.e

constant filters = {-- Emboss
                    {{-2.0, -1.0, 0.0},
                     {-1.0,  1.0, 1.0},
                     { 0.0,  1.0, 2.0}},
                    -- Sharpen
                    {{-1.0, -1.0, -1.0},
                     {-1.0,  9.0, -1.0},
                     {-1.0, -1.0, -1.0}},
                    -- Sobel_emboss
                    {{-1.0, -2.0, -1.0},
                     { 0.0,  0.0,  0.0},
                     { 1.0,  2.0,  1.0}},
                    -- Box_blur
                    {{ 1.0, 1.0, 1.0},
                     { 1.0, 1.0, 1.0},
                     { 1.0, 1.0, 1.0}},
                    -- Gaussian_blur
                    {{1,  4,  7,  4, 1},
                     {4, 16, 26, 16, 4},
                     {7, 26, 41, 26, 7},
                     {4, 16, 26, 16, 4},
                     {1,  4,  7,  4, 1}}}

function convolute(imImage img, integer fdx)
integer width = im_width(img),
        height = im_height(img)
sequence original = repeat(repeat(0,width),height),
         new_image,
         filter = filters[fdx]
integer fh = length(filter), hh=(fh-1)/2,
        fw = length(filter[1]), hw=(fw-1)/2,
        divisor = max(sum(filter),1)

    for y=height-1 to 0 by -1 do
        for x=0 to width-1 do
            original[height-y,x+1] = im_pixel(img, x, y)
        end for
    end for
    new_image = original

    for y=hh+1 to height-hh-1 do
        for x=hw+1 to width-hw-1 do
            sequence newrgb = {0,0,0}
            for i=-hh to +hh do
                for j=-hw to +hw do
                    newrgb = sq_add(newrgb,sq_mul(filter[i+hh+1,j+hw+1],original[y+i,x+j]))
                end for
            end for
            new_image[y,x] = sq_max(sq_min(sq_floor_div(newrgb,divisor),255),0)
        end for
    end for

    new_image = flatten(new_image) -- (as needed by IupImageRGB)
    Ihandle new_img = IupImageRGB(width, height, new_image)
    return new_img
end function

IupOpen()

constant w = machine_word(),
         TITLE = "Image convolution"
atom pError = allocate(w)
imImage im1 = imFileImageLoadBitmap("Quantum_frog.512.png",0,pError)

if im1=NULL then
    ?{"error opening image",peekNS(pError,w,1)}
    {} = wait_key()
    abort(0)
end if

Ihandle dlg,
        filter = IupList("DROPDOWN=YES, VALUE=1")

Ihandln image1 = IupImageFromImImage(im1),
        image2 = convolute(im1,1),
        label1 = IupLabel(),
        label2 = IupLabel()
IupSetAttributeHandle(label1, "IMAGE", image1)
IupSetAttributeHandle(label2, "IMAGE", image2)

function valuechanged_cb(Ihandle /*filter*/)
    IupSetAttribute(dlg,"TITLE","Working...")
    IupSetAttributeHandle(label2, "IMAGE", NULL)
    IupDestroy(image2)
    image2 = convolute(im1,IupGetInt(filter,"VALUE"))
    IupSetAttributeHandle(label2, "IMAGE", image2)
    IupSetAttribute(dlg,"TITLE",TITLE)
    IupRefresh(dlg)
    return IUP_DEFAULT
end function
IupSetCallback(filter,"VALUECHANGED_CB",Icallback("valuechanged_cb"))

IupSetAttributes(filter,"1=Emboss, 2=Sharpen, 3=\"Sobel emboss\", 4=\"Box_blur\", 5=Gaussian_blur")
IupSetAttributes(filter,"VISIBLEITEMS=6") -- (still dunno why this trick works)
dlg = IupDialog(IupVbox({filter,
                         IupFill(),
                         IupHbox({IupFill(),label1, label2,IupFill()}),
                         IupFill()}))
IupSetAttribute(dlg, "TITLE", TITLE)
IupCloseOnEscape(dlg)
IupShow(dlg)

IupMainLoop()
IupClose()
```



## PicoLisp


```PicoLisp
(scl 3)

(de ppmConvolution (Ppm Kernel)
   (let (Len (length (car Kernel))  Radius (/ Len 2))
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
                        (make
                           (for C 3
                              (let Val 0
                                 (for K Len
                                    (for L Len
                                       (inc 'Val
                                          (* (get X K L C) (get Kernel K L)) ) ) )
                                 (link (min 255 (max 0 (*/ Val 1.0)))) ) ) ) )
                     (map pop X) ) ) ) ) ) ) )
```

Test using 'ppmRead' from [[Bitmap/Read a PPM file#PicoLisp]] and 'ppmWrite'
from [[Bitmap/Write a PPM file#PicoLisp]]:

```txt
# Sharpen
(ppmWrite
   (ppmConvolution
      (ppmRead "Lenna100.ppm")
      '((-1.0 -1.0 -1.0) (-1.0 +9.0 -1.0) (-1.0 -1.0 -1.0)) )
   "a.ppm" )

# Blur
(ppmWrite
   (ppmConvolution
      (ppmRead "Lenna100.ppm")
      '((0.1 0.1 0.1) (0.1 0.1 0.1) (0.1 0.1 0.1)) )
   "b.ppm" )
```



## Python

Image manipulation is normally done using an image processing library. For PIL/Pillow do:


```python
#!/bin/python
from PIL import Image, ImageFilter

if __name__=="__main__":
	im = Image.open("test.jpg")

	kernelValues = [-2,-1,0,-1,1,1,0,1,2] #emboss
	kernel = ImageFilter.Kernel((3,3), kernelValues)

	im2 = im.filter(kernel)

	im2.show()
```


Alternatively, SciPy can be used but programmers need to be careful about the colors being clipped since they are normally limited to the 0-255 range:


```python
#!/bin/python
import numpy as np
from scipy.ndimage.filters import convolve
from scipy.misc import imread, imshow

if __name__=="__main__":
	im = imread("test.jpg", mode="RGB")
	im = np.array(im, dtype=float) #Convert to float to prevent clipping colors

	kernel = np.array([[[0,-2,0],[0,-1,0],[0,0,0]],
						[[0,-1,0],[0,1,0],[0,1,0]],
						[[0,0,0],[0,1,0],[0,2,0]]])#emboss

	im2 = convolve(im, kernel)
	im3 = np.array(np.clip(im2, 0, 255), dtype=np.uint8) #Apply color clipping

	imshow(im3)
```



## Racket

This example uses typed/racket, since that gives access to ''inline-build-flomap'',
which delivers quite a performance boost over ''build-flomap''.

[http://timb.net/images/rosettacode/image_convolution/271px-John_Constable_002.jpg 271px-John_Constable_002.jpg]
[http://timb.net/images/rosettacode/image_convolution/convolve-etch-3x3.png convolve-etch-3x3.png]



```racket
#lang typed/racket
(require images/flomap racket/flonum)

(provide flomap-convolve)

(: perfect-square? (Nonnegative-Fixnum -> (U Nonnegative-Fixnum #f)))
(define (perfect-square? n)
  (define rt-n (integer-sqrt n))
  (and (= n (sqr rt-n)) rt-n))

(: flomap-convolve (flomap FlVector -> flomap))
(define (flomap-convolve F K)
  (unless (flomap? F) (error "arg1 not a flowmap"))
  (unless (flvector? K) (error "arg2 not a flvector"))
  (define R (perfect-square? (flvector-length K)))
  (cond
    [(not (and R (odd? R))) (error "K is not odd-sided square")]
    [else
     (define R/2 (quotient R 2))
     (define R/-2 (quotient R -2))
     (define-values (sz-w sz-h) (flomap-size F))
     (define-syntax-rule (convolution c x y i)
       (if (= 0 c)
           (flomap-ref F c x y) ; c=3 is alpha channel
           (for*/fold: : Flonum
             ((acc : Flonum 0.))
             ((k (in-range 0 (add1 R/2)))
              (l (in-range 0 (add1 R/2)))
              (kl (in-value (+ (* k R) l)))
              (kx (in-value (+ x k R/-2)))
              (ly (in-value (+ y l R/-2)))
              #:when (< 0 kx (sub1 sz-w))
              #:when (< 0 ly (sub1 sz-h)))
             (+ acc (* (flvector-ref K kl) (flomap-ref F c kx ly))))))

     (inline-build-flomap 4 sz-w sz-h convolution)]))

(module* test racket
  (require racket/draw images/flomap racket/flonum (only-in 2htdp/image save-image))
  (require (submod ".."))
  (define flmp (bitmap->flomap (read-bitmap "jpg/271px-John_Constable_002.jpg")))
  (save-image
   (flomap->bitmap (flomap-convolve flmp (flvector 1.)))
   "out/convolve-unit-1x1.png")
  (save-image
   (flomap->bitmap (flomap-convolve flmp (flvector 0. 0. 0. 0. 1. 0. 0. 0. 0.)))
   "out/convolve-unit-3x3.png")
  (save-image
   (flomap->bitmap (flomap-convolve flmp (flvector -1. -1. -1. -1. 4. -1. -1. -1. -1.)))
   "out/convolve-etch-3x3.png"))
```



## Ruby

```ruby
class Pixmap
  # Apply a convolution kernel to a whole image
  def convolute(kernel)
    newimg = Pixmap.new(@width, @height)
    pb = ProgressBar.new(@width) if $DEBUG
    @width.times do |x|
      @height.times do |y|
        apply_kernel(x, y, kernel, newimg)
      end
      pb.update(x) if $DEBUG
    end
    pb.close if $DEBUG
    newimg
  end

  # Applies a convolution kernel to produce a single pixel in the destination
  def apply_kernel(x, y, kernel, newimg)
    x0 = x==0 ? 0 : x-1
    y0 = y==0 ? 0 : y-1
    x1 = x
    y1 = y
    x2 = x+1==@width  ? x : x+1
    y2 = y+1==@height ? y : y+1

    r = g = b = 0.0
    [x0, x1, x2].zip(kernel).each do |xx, kcol|
      [y0, y1, y2].zip(kcol).each do |yy, k|
        r += k * self[xx,yy].r
        g += k * self[xx,yy].g
        b += k * self[xx,yy].b
      end
    end
    newimg[x,y] = RGBColour.new(luma(r), luma(g), luma(b))
  end

  # Function for clamping values to those that we can use with colors
  def luma(value)
    if value < 0
      0
    elsif value > 255
      255
    else
      value
    end
  end
end


# Demonstration code using the teapot image from Tk's widget demo
teapot = Pixmap.open('teapot.ppm')
[ ['Emboss',  [[-2.0, -1.0, 0.0],  [-1.0, 1.0, 1.0],  [0.0, 1.0, 2.0]]],
  ['Sharpen', [[-1.0, -1.0, -1.0], [-1.0, 9.0, -1.0], [-1.0, -1.0, -1.0]]],
  ['Blur',    [[0.1111,0.1111,0.1111],[0.1111,0.1111,0.1111],[0.1111,0.1111,0.1111]]],
].each do |label, kernel|
  savefile = 'teapot_' + label.downcase + '.ppm'
  teapot.convolute(kernel).save(savefile)
end
```



## Tcl

```tcl
package require Tk

# Function for clamping values to those that we can use with colors
proc tcl::mathfunc::luma channel {
    set channel [expr {round($channel)}]
    if {$channel < 0} {
	return 0
    } elseif {$channel > 255} {
	return 255
    } else {
	return $channel
    }
}
# Applies a convolution kernel to produce a single pixel in the destination
proc applyKernel {srcImage x y -- kernel -> dstImage} {
    set x0 [expr {$x==0 ? 0 : $x-1}]
    set y0 [expr {$y==0 ? 0 : $y-1}]
    set x1 $x
    set y1 $y
    set x2 [expr {$x+1==[image width $srcImage]  ? $x : $x+1}]
    set y2 [expr {$y+1==[image height $srcImage] ? $y : $y+1}]

    set r [set g [set b 0.0]]
    foreach X [list $x0 $x1 $x2] kcol $kernel {
	foreach Y [list $y0 $y1 $y2] k $kcol {
	    lassign [$srcImage get $X $Y] rPix gPix bPix
	    set r [expr {$r + $k * $rPix}]
	    set g [expr {$g + $k * $gPix}]
	    set b [expr {$b + $k * $bPix}]
	}
    }

    $dstImage put [format "#%02x%02x%02x" \
		       [expr {luma($r)}] [expr {luma($g)}] [expr {luma($b)}]]\
	-to $x $y
}
# Apply a convolution kernel to a whole image
proc convolve {srcImage kernel {dstImage ""}} {
    if {$dstImage eq ""} {
	set dstImage [image create photo]
    }
    set w [image width $srcImage]
    set h [image height $srcImage]
    for {set x 0} {$x < $w} {incr x} {
	for {set y 0} {$y < $h} {incr y} {
	    applyKernel $srcImage $x $y -- $kernel -> $dstImage
	}
    }
    return $dstImage
}

# Demonstration code using the teapot image from Tk's widget demo
image create photo teapot -file $tk_library/demos/images/teapot.ppm
pack [labelframe .src -text Source] -side left
pack [label .src.l -image teapot]
foreach {label kernel} {
    Emboss {
	{-2. -1. 0.}
	{-1.  1. 1.}
	{ 0.  1. 2.}
    }
    Sharpen {
	{-1. -1. -1}
	{-1.  9. -1}
	{-1. -1. -1}
    }
    Blur {
	{.1111 .1111 .1111}
	{.1111 .1111 .1111}
	{.1111 .1111 .1111}
    }
} {
    set name [string tolower $label]
    update
    pack [labelframe .$name -text $label] -side left
    pack [label .$name.l -image [convolve teapot $kernel]]
}
```


