+++
title = "Bilinear interpolation"
description = ""
date = 2019-10-14T09:37:44Z
aliases = []
[extra]
id = 14064
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "csharp",
  "d",
  "go",
  "j",
  "java",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "scala",
  "sidef",
  "tcl",
  "visual_basic_dotnet",
  "zkl",
]
+++

[[wp:Bilinear interpolation|Bilinear interpolation]] is linear interpolation in 2 dimensions, and is typically used for image scaling and for 2D finite element analysis.

## Task

Open an image file, enlarge it by 60% using bilinear interpolation, then either display the result or save the result to a file.


## C


```c
#include <stdint.h>

typedef struct {
    uint32_t *pixels;
    unsigned int w;
    unsigned int h;
} image_t;
#define getByte(value, n) (value >> (n*8) & 0xFF)

uint32_t getpixel(image_t *image, unsigned int x, unsigned int y){
    return image->pixels[(y*image->w)+x];
}
float lerp(float s, float e, float t){return s+(e-s)*t;}
float blerp(float c00, float c10, float c01, float c11, float tx, float ty){
    return lerp(lerp(c00, c10, tx), lerp(c01, c11, tx), ty);
}
void putpixel(image_t *image, unsigned int x, unsigned int y, uint32_t color){
    image->pixels[(y*image->w) + x] = color;
}
void scale(image_t *src, image_t *dst, float scalex, float scaley){
    int newWidth = (int)src->w*scalex;
    int newHeight= (int)src->h*scaley;
    int x, y;
    for(x= 0, y=0; y < newHeight; x++){
        if(x > newWidth){
            x = 0; y++;
        }
        float gx = x / (float)(newWidth) * (src->w-1);
        float gy = y / (float)(newHeight) * (src->h-1);
        int gxi = (int)gx;
        int gyi = (int)gy;
        uint32_t result=0;
        uint32_t c00 = getpixel(src, gxi, gyi);
        uint32_t c10 = getpixel(src, gxi+1, gyi);
        uint32_t c01 = getpixel(src, gxi, gyi+1);
        uint32_t c11 = getpixel(src, gxi+1, gyi+1);
        uint8_t i;
        for(i = 0; i < 3; i++){
            //((uint8_t*)&result)[i] = blerp( ((uint8_t*)&c00)[i], ((uint8_t*)&c10)[i], ((uint8_t*)&c01)[i], ((uint8_t*)&c11)[i], gxi - gx, gyi - gy); // this is shady
            result |= (uint8_t)blerp(getByte(c00, i), getByte(c10, i), getByte(c01, i), getByte(c11, i), gx - gxi, gy -gyi) << (8*i);
        }
        putpixel(dst,x, y, result);
    }
}
```


## C#
Seems to have some artifacting in the output, but the image is at least recognizable.

```c#
using System;
using System.Drawing;

namespace BilinearInterpolation {
    class Program {
        private static float Lerp(float s, float e, float t) {
            return s + (e - s) * t;
        }

        private static float Blerp(float c00, float c10, float c01, float c11, float tx, float ty) {
            return Lerp(Lerp(c00, c10, tx), Lerp(c01, c11, tx), ty);
        }

        private static Image Scale(Bitmap self, float scaleX, float scaleY) {
            int newWidth = (int)(self.Width * scaleX);
            int newHeight = (int)(self.Height * scaleY);
            Bitmap newImage = new Bitmap(newWidth, newHeight, self.PixelFormat);

            for (int x = 0; x < newWidth; x++) {
                for (int y = 0; y < newHeight; y++) {
                    float gx = ((float)x) / newWidth * (self.Width - 1);
                    float gy = ((float)y) / newHeight * (self.Height - 1);
                    int gxi = (int)gx;
                    int gyi = (int)gy;
                    Color c00 = self.GetPixel(gxi, gyi);
                    Color c10 = self.GetPixel(gxi + 1, gyi);
                    Color c01 = self.GetPixel(gxi, gyi + 1);
                    Color c11 = self.GetPixel(gxi + 1, gyi + 1);

                    int red = (int)Blerp(c00.R, c10.R, c01.R, c11.R, gx - gxi, gy - gyi);
                    int green = (int)Blerp(c00.G, c10.G, c01.G, c11.G, gx - gxi, gy - gyi);
                    int blue = (int)Blerp(c00.B, c10.B, c01.B, c11.B, gx - gxi, gy - gyi);
                    Color rgb = Color.FromArgb(red, green, blue);
                    newImage.SetPixel(x, y, rgb);
                }
            }

            return newImage;
        }

        static void Main(string[] args) {
            Image newImage = Image.FromFile("Lenna100.jpg");
            if (newImage is Bitmap oi) {
                Image result = Scale(oi, 1.6f, 1.6f);
                result.Save("Lenna100_larger.jpg");
            } else {
                Console.WriteLine("Could not open the source file.");
            }
        }
    }
}
```



## D

This uses the module from the Grayscale Image task.
```d
import grayscale_image;

/// Currently this accepts only a Grayscale image, for simplicity.
Image!Gray rescaleGray(in Image!Gray src, in float scaleX, in float scaleY)
pure nothrow @safe
in {
    assert(src !is null, "Input Image is null.");
    assert(src.nx > 1 && src.ny > 1, "Minimal input image size is 2x2.");
    assert(cast(uint)(src.nx * scaleX) > 0, "Output image width must be > 0.");
    assert(cast(uint)(src.ny * scaleY) > 0, "Output image height must be > 0.");
} body {
    alias FP = float;
    static FP lerp(in FP s, in FP e, in FP t) pure nothrow @safe @nogc {
        return s + (e - s) * t;
    }

    static FP blerp(in FP c00, in FP c10, in FP c01, in FP c11,
                    in FP tx, in FP ty) pure nothrow @safe @nogc {
        return lerp(lerp(c00, c10, tx), lerp(c01, c11, tx), ty);
    }

    immutable newWidth = cast(uint)(src.nx * scaleX);
    immutable newHeight = cast(uint)(src.ny * scaleY);
    auto result = new Image!Gray(newWidth, newHeight, true);

    foreach (immutable y; 0 .. newHeight)
        foreach (immutable x; 0 .. newWidth) {
            immutable FP gx = x / FP(newWidth) * (src.nx - 1);
            immutable FP gy = y / FP(newHeight) * (src.ny - 1);
            immutable gxi = cast(uint)gx;
            immutable gyi = cast(uint)gy;

            immutable c00 = src[gxi,     gyi    ];
            immutable c10 = src[gxi + 1, gyi    ];
            immutable c01 = src[gxi,     gyi + 1];
            immutable c11 = src[gxi + 1, gyi + 1];

            immutable pixel = blerp(c00, c10, c01, c11, gx - gxi, gy - gyi);
            result[x, y] = Gray(cast(ubyte)pixel);
        }

    return result;
}

void main() {
    const im = loadPGM!Gray(null, "lena.pgm");
    im.rescaleGray(0.3, 0.1).savePGM("lena_smaller.pgm");
    im.rescaleGray(1.3, 1.8).savePGM("lena_larger.pgm");
}
```


=={{header|F#|F sharp}}==
```fsharp
open System
open System.Drawing

let lerp (s:float) (e:float) (t:float) =
    s + (e - s) * t

let blerp c00 c10 c01 c11 tx ty =
    lerp (lerp c00 c10 tx) (lerp c01 c11 tx) ty

let scale (self:Bitmap) (scaleX:float) (scaleY:float) =
    let newWidth  = int ((float self.Width)  * scaleX)
    let newHeight = int ((float self.Height) * scaleY)
    let newImage = new Bitmap(newWidth, newHeight, self.PixelFormat)
    for x in 0..newWidth-1 do
        for y in 0..newHeight-1 do
            let gx = (float x) / (float newWidth) *  (float (self.Width  - 1))
            let gy = (float y) / (float newHeight) * (float (self.Height - 1))
            let gxi = int gx
            let gyi = int gy
            let c00 = self.GetPixel(gxi,     gyi)
            let c10 = self.GetPixel(gxi + 1, gyi)
            let c01 = self.GetPixel(gxi,     gyi + 1)
            let c11 = self.GetPixel(gxi + 1, gyi + 1)
            let red   = int (blerp (float c00.R) (float c10.R) (float c01.R) (float c11.R) (gx - (float gxi)) (gy - (float gyi)))
            let green = int (blerp (float c00.G) (float c10.G) (float c01.G) (float c11.G) (gx - (float gxi)) (gy - (float gyi)))
            let blue  = int (blerp (float c00.B) (float c10.B) (float c01.B) (float c11.B) (gx - (float gxi)) (gy - (float gyi)))
            let rgb = Color.FromArgb(red, green, blue)
            newImage.SetPixel(x, y, rgb)
    newImage

// Taken from https://stackoverflow.com/a/2362114
let castAs<'T when 'T : null> (o:obj) =
    match o with
    | :? 'T as res -> res
    | _ -> Unchecked.defaultof<'T>

[<EntryPoint>]
let main _ =
    let newImage = Image.FromFile("Lenna100.jpg")
    let oi = castAs<Bitmap>(newImage)
    if oi = null then
        Console.WriteLine("Could not open the source file.")
    else
        let result = scale oi 1.6 1.6
        result.Save("Lenna100_larger.jpg")

    0 // return an integer exit code
```



## Go

(and also just using
<code>[https://godoc.org/golang.org/x/image/draw#BiLinear draw.BiLinear]</code>
from the <code>golang.org/x/image/draw</code> pacakge).

```Go
package main

import (
	"image"
	"image/color"
	"image/jpeg"
	"log"
	"math"
	"os"

	"golang.org/x/image/draw"
)

func scale(dst draw.Image, src image.Image) {
	sr := src.Bounds()
	dr := dst.Bounds()
	mx := float64(sr.Dx()-1) / float64(dr.Dx())
	my := float64(sr.Dy()-1) / float64(dr.Dy())
	for x := dr.Min.X; x < dr.Max.X; x++ {
		for y := dr.Min.Y; y < dr.Max.Y; y++ {
			gx, tx := math.Modf(float64(x) * mx)
			gy, ty := math.Modf(float64(y) * my)
			srcX, srcY := int(gx), int(gy)
			r00, g00, b00, a00 := src.At(srcX, srcY).RGBA()
			r10, g10, b10, a10 := src.At(srcX+1, srcY).RGBA()
			r01, g01, b01, a01 := src.At(srcX, srcY+1).RGBA()
			r11, g11, b11, a11 := src.At(srcX+1, srcY+1).RGBA()
			result := color.RGBA64{
				R: blerp(r00, r10, r01, r11, tx, ty),
				G: blerp(g00, g10, g01, g11, tx, ty),
				B: blerp(b00, b10, b01, b11, tx, ty),
				A: blerp(a00, a10, a01, a11, tx, ty),
			}
			dst.Set(x, y, result)
		}
	}
}

func lerp(s, e, t float64) float64 { return s + (e-s)*t }
func blerp(c00, c10, c01, c11 uint32, tx, ty float64) uint16 {
	return uint16(lerp(
		lerp(float64(c00), float64(c10), tx),
		lerp(float64(c01), float64(c11), tx),
		ty,
	))
}

func main() {
	src, err := readImage("Lenna100.jpg")
	if err != nil {
		log.Fatal(err)
	}
	sr := src.Bounds()
	dr := image.Rect(0, 0, sr.Dx()*16/10, sr.Dy()*16/10)
	dst := image.NewRGBA(dr)

	// Using the above bilinear interpolation code:
	scale(dst, src)
	err = writeJPEG(dst, "Lenna100_larger.jpg")
	if err != nil {
		log.Fatal(err)
	}

	// Using the golang.org/x/image/draw package
	// (which also provides other iterpolators).
	draw.BiLinear.Scale(dst, dr, src, sr, draw.Src, nil)
	err = writeJPEG(dst, "Lenna100_larger.draw.jpg")
	if err != nil {
		log.Fatal(err)
	}
}

func readImage(filename string) (image.Image, error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer f.Close() // nolint: errcheck
	m, _, err := image.Decode(f)
	return m, err
}

func writeJPEG(m image.Image, filename string) error {
	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	err = jpeg.Encode(f, m, nil)
	if cerr := f.Close(); err == nil {
		err = cerr
	}
	return err
}
```



## J


```J

Note 'FEA'
   Here we develop a general method to generate isoparametric interpolants.

   The interpolant is the dot product of the four shape function values evaluated
   at the coordinates within the element with the known values at the nodes.
   The sum of four shape functions of two variables (xi, eta) is 1 at each of four nodes.
   Let the base element have nodal coordinates (xi, eta) of +/-1.


    2               3 (1,1)
   +---------------+
   |               |
   |               |
   |        (0,0)  |
   |       *       |
   |               |
   |               |
   |               |
   +---------------+
    0               1

   determine f0(xi,eta), ..., f3(xi,eta).
   f0(-1,-1) = 1, f0(all other corners) is 0.
   f1( 1,-1) = 1, f1(all other corners) is 0.
   ...

   Choose a shape function.
   Use shape functions C0 + C1*xi + C2*eta + C3*xi*eta .
   Given (xi,eta) as the vector y form a vector of the
   coefficients of the constants (1, xi, eta, and their product)

      shape_function =: 1 , {. , {: , */

      CORNERS NB. are the ordered coordinates of the corners
   _1 _1
    1 _1
   _1  1
    1  1

      (=i.4)  NB. rows of the identity matrix are the values of each shape functions at each corner
   1 0 0 0
   0 1 0 0
   0 0 1 0
   0 0 0 1

      (=i.4x) %. shape_function"1 x: CORNERS  NB. Compute the values of the constants as rational numbers.
    1r4  1r4  1r4 1r4
   _1r4  1r4 _1r4 1r4
   _1r4 _1r4  1r4 1r4
    1r4 _1r4 _1r4 1r4

   This method extends to higher order interpolants having more nodes or to other dimensions.
)

mp =: +/ .*  NB. matrix product

CORNERS =: 21 A.-.+:#:i.4
shape_function =: 1 , ] , */
COEFFICIENTS =: (=i.4) %. shape_function"1 CORNERS
shape_functions =: COEFFICIENTS mp~ shape_function
interpolate =: mp shape_functions

```


```txt

Note 'demonstrate the interpolant with a saddle'
   lower left has value 1,
   lower right: 2
   upper left: 2.2
   upper right: 0.7
)

require'viewmat'
GRID =: |.,~"0/~(%~i:)100
SADDLE =: 1 2 2.2 0.7 interpolate"_ 1 GRID
viewmat SADDLE
assert 0.7 2.2 -: (<./ , >./) , SADDLE

```


[[Image:J_bilinear_interpolant.jpg]]

Let n mean shape function, C mean constants, i mean interpolant, and the three digits meaning dimensionality, number of corners, and (in base 36) the number of nodes we construct various linear and quadratic interpolants in 1, 2, and 3 dimensions as

```J

Note 'Some elemental information'

   Node order
   1D:

   0   2   1


   2D:

   2   7   3

   5   8   6   Node 8 at origin, Node 3 at (1,1)

   0   4   1

   Names for shape functions and constants:
   n249: n means shape function, 2 dimensions, 4 corners (quadrilateral), 9 nodes
   C244: C       constants for   2 dimensions, 4 corners (quadrilateral), 4 nodes


   3D
   At z = _1           z = 1            z = 0
   2   b   3           6   j   7        e   o   f

   9   k   a           h   p   i        m   q   n

   0   8   1           4   g   5        c   l   d
)
mp =: ($: |:) : (+/ .*)  NB. A Atranspose : matrix product A B
identity =: =@:i.        NB. generate identity matrix


NB. 1D
NB. master nodes
N1 =: ,._1 1 0x
NB. form of shape functions
n122 =: 1 , ]
n123 =: [: , ^/&(i.3)
NB. constants
C122 =: x:inv@:(x:@:identity@:# %. n122"1)2{.N1
C123 =: x:inv@:(x:@:identity@:# %. n123"1)3{.N1
NB. interpolants
i122 =: mp (C122 mp~ n122)
i123 =: mp (C123 mp~ n123)


NB. 2D
NB. nodes are arranged 4&{. are the corners, 8&{. the corners and edges, ] include the center.
N2 =: 336330 A.-.3x#.inv i.*:3   NB. 336330 (-: A.) 8 2 6 0 5 7 1 3 4

NB. terms of shape functions
n244 =: [: , [: *// ^/&(i.2)            NB. all linear combinations
n248 =: }:@:n249                        NB. exclude (xi eta)^2
n249 =: [: , [: *// ^/&(i.3)            NB. all quadratic combinations

NB. constants
C244 =: x:inv@:(x:@:identity@:# %. n244"1)4{.N2 NB. serendipity
C248 =: x:inv@:(x:@:identity@:# %. n248"1)8{.N2 NB. serendipity
C249 =: x:inv@:(x:@:identity@:# %. n249"1)9{.N2 NB. non-serendipity

NB. interpolants
i244 =: mp (C244 mp~ n244)
i248 =: mp (C248 mp~ n248)
i249 =: mp (C249 mp~ n249)

NB. 3D
N3 =: 267337661061030402017459663x A.<:3#.inv i.3^3  NB. 267337661061030402017459663x (-: A.) 0 18 6 24 2 20 8 26 9 3 21 15 1 19 7 25 11 5 23 17 12 10 4 22 16 14 13
NB. corners
n388 =: [: , [: *// 1 , ]               NB. all linear combinations

Note 'simplification not yet apparent to me'
   combinations =: 4 : 0
     if. x e. 0 1 do. z=.<((x!y),x)$ i.y
     else. t=. |.(<.@-:)^:(i.<. 2^.x)x
       z=.({.t) ([:(,.&.><@;\.)/ >:@-~[\i.@]) ({.t)+y-x
       for_j. 2[\t do.
         z=.([ ;@:(<"1@[ (,"1 ({.j)+])&.> ])&.> <@;\.({&.><)~ (1+({.j)-~{:"1)&.>) z
         if. 2|{:j do. z=.(i.1+y-x)(,.>:)&.> <@;\.z end.
       end.
     end.
     ;z
   NB.)
   n38k =: 1 , ] , */"1@:((2 combinations 3)&{) , *: , (1&, * */) , ,@:(*:@:|. (*"0 1) (2 combinations 3)&{) NB. include mid-edge nodes
)
n38q =: }:@:n38r             NB. include mid-face nodes, all quadratic combinations but (xyz)^2
n38r =: [: , [: *// ^/&(i.3) NB. now this is simple!  3*3*3 nodal grid.
C388 =: x:inv@:(x:@:identity@:# %. n388"1)8{.N3
NB.C38k =: x:inv@:(x:@:identity@:# %. n38k"1)36bk{.N3
C38q =: x:inv@:(x:@:identity@:# %. x:@:n38q"1)36bq{.N3
C38r =: x:inv@:(x:@:identity@:# %. x:@:n38r"1)36br{.N3
i388 =: mp (C388 mp~ n388)
NB.i38k =: mp (C38k mp~ n38k)
i38q =: mp (C38r mp~ n38r)
i38r =: mp (C38r mp~ n38r)

```



## Java

```Java
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

public class BilinearInterpolation {
    /* gets the 'n'th byte of a 4-byte integer */
    private static int get(int self, int n) {
        return (self >> (n * 8)) & 0xFF;
    }

    private static float lerp(float s, float e, float t) {
        return s + (e - s) * t;
    }

    private static float blerp(final Float c00, float c10, float c01, float c11, float tx, float ty) {
        return lerp(lerp(c00, c10, tx), lerp(c01, c11, tx), ty);
    }

    private static BufferedImage scale(BufferedImage self, float scaleX, float scaleY) {
        int newWidth = (int) (self.getWidth() * scaleX);
        int newHeight = (int) (self.getHeight() * scaleY);
        BufferedImage newImage = new BufferedImage(newWidth, newHeight, self.getType());
        for (int x = 0; x < newWidth; ++x) {
            for (int y = 0; y < newHeight; ++y) {
                float gx = ((float) x) / newWidth * (self.getWidth() - 1);
                float gy = ((float) y) / newHeight * (self.getHeight() - 1);
                int gxi = (int) gx;
                int gyi = (int) gy;
                int rgb = 0;
                int c00 = self.getRGB(gxi, gyi);
                int c10 = self.getRGB(gxi + 1, gyi);
                int c01 = self.getRGB(gxi, gyi + 1);
                int c11 = self.getRGB(gxi + 1, gyi + 1);
                for (int i = 0; i <= 2; ++i) {
                    float b00 = get(c00, i);
                    float b10 = get(c10, i);
                    float b01 = get(c01, i);
                    float b11 = get(c11, i);
                    int ble = ((int) blerp(b00, b10, b01, b11, gx - gxi, gy - gyi)) << (8 * i);
                    rgb = rgb | ble;
                }
                newImage.setRGB(x, y, rgb);
            }
        }
        return newImage;
    }

    public static void main(String[] args) throws IOException {
        File lenna = new File("Lenna100.jpg");
        BufferedImage image = ImageIO.read(lenna);
        BufferedImage image2 = scale(image, 1.6f, 1.6f);
        File lenna2 = new File("Lenna100_larger.jpg");
        ImageIO.write(image2, "jpg", lenna2);
    }
}
```



## Julia


```julia
using Images, FileIO, Interpolations

function enlarge(A::Matrix, factor::AbstractFloat)
    lx, ly = size(A)
    nx, ny = round.(Int, factor .* (lx, ly))
    vx, vy = LinRange(1, lx, nx), LinRange(1, ly, ny)
    itp = interpolate(A, BSpline(Linear()))
    return itp(vx, vy)
end

A = load("data/lenna100.jpg") |> Matrix{RGB{Float64}};
Alarge = enlarge(A, 1.6);
save("data/lennaenlarged.jpg", Alarge)

```




## Kotlin

```scala
// version 1.2.21

import java.io.File
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

/* gets the 'n'th byte of a 4-byte integer */
operator fun Int.get(n: Int) = (this shr (n * 8)) and 0xFF

fun lerp(s: Float, e: Float, t: Float) = s + (e - s) * t

fun blerp(c00: Float, c10: Float, c01: Float, c11: Float, tx: Float, ty: Float) =
    lerp(lerp(c00, c10, tx), lerp(c01,c11, tx), ty)

fun BufferedImage.scale(scaleX: Float, scaleY: Float): BufferedImage {
    val newWidth  = (width * scaleX).toInt()
    val newHeight = (height * scaleY).toInt()
    val newImage  = BufferedImage(newWidth, newHeight, type)
    for (x in 0 until newWidth) {
        for (y in 0 until newHeight) {
            val gx = x.toFloat() / newWidth * (width - 1)
            val gy = y.toFloat() / newHeight * (height - 1)
            val gxi = gx.toInt()
            val gyi = gy.toInt()
            var rgb = 0
            val c00 = getRGB(gxi, gyi)
            val c10 = getRGB(gxi + 1, gyi)
            val c01 = getRGB(gxi, gyi + 1)
            val c11 = getRGB(gxi + 1, gyi + 1)
            for (i in 0..2) {
                val b00 = c00[i].toFloat()
                val b10 = c10[i].toFloat()
                val b01 = c01[i].toFloat()
                val b11 = c11[i].toFloat()
                val ble = blerp(b00, b10, b01, b11, gx - gxi, gy - gyi).toInt() shl (8 * i)
                rgb = rgb or ble
            }
            newImage.setRGB(x, y, rgb)
        }
    }
    return newImage
}

fun main(args: Array<String>) {
    val lenna = File("Lenna100.jpg")  // from the Percentage difference between images task
    val image = ImageIO.read(lenna)
    val image2 = image.scale(1.6f, 1.6f)
    val lenna2 = File("Lenna100_larger.jpg")
    ImageIO.write(image2, "jpg", lenna2)
}
```



## Perl


```perl
use strict;
use warnings;

use GD;

my $image = GD::Image->newFromPng('color_wheel.png');
$image->interpolationMethod( ['GD_BILINEAR_FIXED'] );
my($width,$height) = $image->getBounds();
my $image2 = $image->copyScaleInterpolated( 1.6*$width, 1.6*$height );

$image2->_file('color_wheel_interpolated.png');
```

Compare offsite images: [https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/color_wheel.png color_wheel.png] vs.
[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/color_wheel_interpolated.png color_wheel_interpolated.png]


## Perl 6


```perl6
#!/usr/bin/env perl6

use v6;
use GD::Raw;

# Reference:
# https://github.com/dagurval/perl6-gd-raw

my $fh1 = fopen('./Lenna100.jpg', "rb") or die;
my $img1 = gdImageCreateFromJpeg($fh1);

my $fh2 = fopen('./Lenna100-larger.jpg',"wb") or die;

my $img1X = gdImageSX($img1);
my $img1Y = gdImageSY($img1);

my $NewX = $img1X * 1.6;
my $NewY = $img1Y * 1.6;

gdImageSetInterpolationMethod($img1, +GD_BILINEAR_FIXED);

my $img2 = gdImageScale($img1, $NewX.ceiling, $NewY.ceiling);

gdImageJpeg($img2,$fh2,-1);

gdImageDestroy($img1);
gdImageDestroy($img2);

fclose($fh1);
fclose($fh2);

```


```txt
file Lenna100*
Lenna100.jpg:        JPEG image data, JFIF standard 1.01, resolution (DPI), density 72x72, segment length 16, baseline, precision 8, 512x512, frames 3
Lenna100-larger.jpg: JPEG image data, JFIF standard 1.01, resolution (DPI), density 96x96, segment length 16, comment: "CREATOR: gd-jpeg v1.0 (using IJG JPEG v80), default quality", baseline, precision 8, 820x820, frames 3
```



## Phix

Gui app with slider for between 2 and 200% scaling. Various bits of this code scavenged from C#/Go/Kotlin/Wikipedia.

```Phix
-- demo\rosetta\Bilinear_interpolation.exw
include pGUI.e

function interpolate(atom s, e, f)
--
-- s,e are the start and end values (one original pixel apart),
-- f is a fraction of some point between them, 0(==s)..1(==e).
-- eg s=91 (f=0.2) e=101, we want 0.8 of the 91 + 0.2 of 101,
-- aka if f is 4 times closer to s than e, we want 4 times as
-- much of s as we want of e, with sum(fractions_taken)==1.
--
    return s + (e-s)*f  -- aka s*(1-f) + e*f
end function

function bilinear(integer c00, c10, c01, c11, atom fx, fy)
--
-- for some output pixel, we have calculated the exact point
-- on the original, and extracted the four pixels surrounding
-- that, with fx,fy as the fractional x,y part of the 1x1 box.
-- Like a capital H, we want some fraction on the left and the
-- same on the right, then some fraction along the horizontal.
-- It would be equivalent to do top/bottom then the vertical,
-- which is handy since I am no longer certain which of those
-- the following actually does, especially since we got the
-- pixels from original[y,x] rather than original[x,y], and
-- imImage and IupImage have {0,0} in different corners - but
-- the output looks pretty good, and I think you would easily
-- notice were this even slightly wrong, and in fact an early
-- accidental typo of r10/r01 indeed proved very evident.
--
    atom left = interpolate(c00,c10,fx),
         right = interpolate(c01,c11,fx)
    return floor(interpolate(left,right,fy))
end function

function scale_image(imImage img, atom scaleX, scaleY)
integer width = im_width(img),
        height = im_height(img),
        new_width = floor(width * scaleX)-1,
        new_height = floor(height * scaleY)-1
atom mx = (width-1)/new_width,
     my = (height-1)/new_height
sequence original = repeat(repeat(0,width),height)
sequence new_image = repeat(repeat(0,new_width),new_height)

    -- Extract the original pixels from the image [about
    -- twice as fast as 4*im_pixel() in the main loop.]
    for y=height-1 to 0 by -1 do
        for x=0 to width-1 do
            original[height-y,x+1] = im_pixel(img, x, y)
        end for
    end for

    for x=0 to new_width-1 do
        for y=0 to new_height-1 do
            atom ax = x*mx,         -- map onto original
                 ay = y*my
            integer ix = floor(ax), -- top left
                    iy = floor(ay)
            ax -= ix                -- fraction of the 1x1 box
            ay -= iy
            integer {r00,g00,b00} = original[iy+1,ix+1],
                    {r10,g10,b10} = original[iy+1,ix+2],
                    {r01,g01,b01} = original[iy+2,ix+1],
                    {r11,g11,b11} = original[iy+2,ix+2],
                    r = bilinear(r00,r10,r01,r11,ax,ay),
                    g = bilinear(g00,g10,g01,g11,ax,ay),
                    b = bilinear(b00,b10,b01,b11,ax,ay)
            new_image[y+1,x+1] = {r,g,b}
        end for
    end for
    new_image = flatten(new_image) -- (as needed by IupImageRGB)
    Ihandle new_img = IupImageRGB(new_width, new_height, new_image)
    return new_img
end function

IupOpen()

constant w = machine_word()
atom pError = allocate(w)
imImage im1 = imFileImageLoadBitmap("Lena.ppm",0,pError)
if im1=NULL then
    ?{"error opening image",peekNS(pError,w,1)}
    {} = wait_key()
    abort(0)
end if

Ihandle dlg,
        scale = IupValuator(NULL,"MIN=2,MAX=200,VALUE=160"),
        redraw = IupButton("redraw (160%)")

Ihandln image1 = IupImageFromImImage(im1),
        image2 = scale_image(im1,1.6,1.6),
        label1 = IupLabel(),
        label2 = IupLabel()
IupSetAttributeHandle(label1, "IMAGE", image1)
IupSetAttributeHandle(label2, "IMAGE", image2)

function valuechanged_cb(Ihandle /*scale*/)
    atom v = IupGetDouble(scale,"VALUE")
    IupSetStrAttribute(redraw,"TITLE","redraw (%d%%)",{v})
    return IUP_DEFAULT
end function
IupSetCallback(scale,"VALUECHANGED_CB",Icallback("valuechanged_cb"))

function redraw_cb(Ihandle /*redraw*/)
    IupSetAttributeHandle(label2, "IMAGE", NULL)
    IupDestroy(image2)
    atom v = IupGetDouble(scale,"VALUE")/100
    image2 = scale_image(im1,v,v)
    IupSetAttributeHandle(label2, "IMAGE", image2)
    IupSetAttribute(dlg,"SIZE",NULL)
    IupRefresh(dlg)
    return IUP_DEFAULT
end function
IupSetCallback(redraw,"ACTION",Icallback("redraw_cb"))

dlg = IupDialog(IupVbox({IupHbox({scale, redraw}),
                         IupHbox({label1, label2})}))
IupSetAttribute(dlg, "TITLE", "Bilinear interpolation")
IupCloseOnEscape(dlg)
IupShow(dlg)

IupMainLoop()
IupClose()
```



## Python


Of course, it is much faster to use PIL, Pillow or SciPy to resize an image than to rely on this code.


```python
#!/bin/python
import numpy as np
from scipy.misc import imread, imshow
from scipy import ndimage

def GetBilinearPixel(imArr, posX, posY):
	out = []

	#Get integer and fractional parts of numbers
	modXi = int(posX)
	modYi = int(posY)
	modXf = posX - modXi
	modYf = posY - modYi
	modXiPlusOneLim = min(modXi+1,imArr.shape[1]-1)
	modYiPlusOneLim = min(modYi+1,imArr.shape[0]-1)

	#Get pixels in four corners
	for chan in range(imArr.shape[2]):
		bl = imArr[modYi, modXi, chan]
		br = imArr[modYi, modXiPlusOneLim, chan]
		tl = imArr[modYiPlusOneLim, modXi, chan]
		tr = imArr[modYiPlusOneLim, modXiPlusOneLim, chan]

		#Calculate interpolation
		b = modXf * br + (1. - modXf) * bl
		t = modXf * tr + (1. - modXf) * tl
		pxf = modYf * t + (1. - modYf) * b
		out.append(int(pxf+0.5))

	return out

if __name__=="__main__":

	im = imread("test.jpg", mode="RGB")
	enlargedShape = list(map(int, [im.shape[0]*1.6, im.shape[1]*1.6, im.shape[2]]))
	enlargedImg = np.empty(enlargedShape, dtype=np.uint8)
	rowScale = float(im.shape[0]) / float(enlargedImg.shape[0])
	colScale = float(im.shape[1]) / float(enlargedImg.shape[1])

	for r in range(enlargedImg.shape[0]):
		for c in range(enlargedImg.shape[1]):
			orir = r * rowScale #Find position in original image
			oric = c * colScale
			enlargedImg[r, c] = GetBilinearPixel(im, oric, orir)

	imshow(enlargedImg)

```



## Racket

This mimics the Wikipedia example.

```racket
#lang racket
(require images/flomap)

(define fm
  (draw-flomap
   (λ (dc)
     (define (pixel x y color)
       (send dc set-pen color 1 'solid)
       (send dc draw-point (+ x .5) (+ y 0.5)))
     (send dc set-alpha 1)
     (pixel 0 0 "blue")
     (pixel 0 1 "red")
     (pixel 1 0 "red")
     (pixel 1 1 "green"))
   2 2))

(flomap->bitmap
 (build-flomap
  4 250 250
  (λ (k x y)
    (flomap-bilinear-ref
     fm k (+ 1/2 (/ x 250)) (+ 1/2 (/ y 250))))))
```



## Scala


### Imperative solution


```Scala
import java.awt.image.BufferedImage
import java.io.{File, IOException}

import javax.imageio.ImageIO

object BilinearInterpolation {
  @throws[IOException]
  def main(args: Array[String]): Unit = {
    val lenna = new File("Lenna100.jpg")
    val image = ImageIO.read(lenna)
    val image2 = scale(image, 1.6f, 1.6f)
    val lenna2 = new File("Lenna100_larger.jpg")
    ImageIO.write(image2, "jpg", lenna2)
  }

  private def scale(self: BufferedImage, scaleX: Float, scaleY: Float) = {
    val newWidth = (self.getWidth * scaleX).toInt
    val newHeight = (self.getHeight * scaleY).toInt
    val newImage = new BufferedImage(newWidth, newHeight, self.getType)
    var x = 0
    while (x < newWidth) {
      var y = 0
      while (y < newHeight) {
        val gx = x.toFloat / newWidth * (self.getWidth - 1)
        val gy = y.toFloat / newHeight * (self.getHeight - 1)
        val gxi = gx.toInt
        val gyi = gy.toInt
        var rgb = 0
        val c00 = self.getRGB(gxi, gyi)
        val c10 = self.getRGB(gxi + 1, gyi)
        val c01 = self.getRGB(gxi, gyi + 1)
        val c11 = self.getRGB(gxi + 1, gyi + 1)
        var i = 0
        while (i <= 2) {
          val b00 = get(c00, i)
          val b10 = get(c10, i)
          val b01 = get(c01, i)
          val b11 = get(c11, i)
          val ble = blerp(b00, b10, b01, b11, gx - gxi, gy - gyi).toInt << (8 * i)
          rgb = rgb | ble

          i += 1
        }
        newImage.setRGB(x, y, rgb)

        y += 1
      }
      x += 1
    }
    newImage
  }

  /* gets the 'n'th byte of a 4-byte integer */
  private def get(self: Int, n: Int) = (self >> (n * 8)) & 0xFF

  private def blerp(c00: Float, c10: Float, c01: Float, c11: Float, tx: Float, ty: Float) = lerp(lerp(c00, c10, tx), lerp(c01, c11, tx), ty)

  private def lerp(s: Float, e: Float, t: Float) = s + (e - s) * t
}
```


## Sidef

```ruby
require('Imager')

func scale(img, scaleX, scaleY) {
    var (width, height) = (img.getwidth, img.getheight)
    var (newWidth, newHeight) = (int(width*scaleX), int(height*scaleY))

    var out = %O<Imager>.new(xsize => newWidth, ysize => newHeight)

    var lerp = { |s, e, t|
        s + t*(e-s)
    }

    var blerp = { |c00, c10, c01, c11, tx, ty|
        lerp(lerp(c00, c10, tx), lerp(c01, c11, tx), ty)
    }

    for x,y in (^newWidth ~X ^newHeight) {
        var gxf = (x/newWidth  * (width  - 1))
        var gyf = (y/newHeight * (height - 1))

        var gx = gxf.int
        var gy = gyf.int

        var *c00 = img.getpixel(x => gx,   y => gy  ).rgba
        var *c10 = img.getpixel(x => gx+1, y => gy  ).rgba
        var *c01 = img.getpixel(x => gx,   y => gy+1).rgba
        var *c11 = img.getpixel(x => gx+1, y => gy+1).rgba

        var rgb = 3.of { |i|
            blerp(c00[i], c10[i], c01[i], c11[i], gxf - gx, gyf - gy).int
        }

        out.setpixel(x => x, y => y, color => rgb)
    }

    return out
}

var img = %O<Imager>.new(file => "input.png")
var out = scale(img, 1.6, 1.6)
out.write(file => "output.png")
```



## Tcl


This uses the polynomial expansion described in wikipedia, and draws the same example as illustrated in that page with a different pallette.  It's not particularly fast - about 300ms for a 200x200 surface on an arbitrary machine.

The script below will show the computed image in a GUI frame, and present a button to save it.


```Tcl

package require Tk

proc pixel {f} {
    if {$f < 0} {
        error "why is $f?"
    }
    set i [expr {0xff & entier(0xff*$f)}]
    format #%02x%02x%02x $i [expr {255-$i}] 127
}

proc bilerp {im O X Y XY} {
    set w [image width $im]
    set h [image height $im]
    set dx [expr {1.0/$w}]
    set dy [expr {1.0/$h}]
    set a0 $O
    set a1 [expr {$X - $O}]
    set a2 [expr {$Y - $O}]
    set a3 [expr {$O + $XY - ($X + $Y)}]
    for {set y 0} {$y < $h} {incr y} {
        for {set x 0} {$x < $w} {incr x} {
            set i [expr {$x * $dx}]
            set j [expr {$y * $dy}]
            set xv [expr {$a0 + $a1*$i + $a2*$j + $a3*$i*$j}]
            set y [expr {$h - $y}] ;# invert for screen coords
            $im put [pixel $xv] -to $x $y
        }
    }
}

proc save {im} {
    set fn [tk_getSaveFile -defaultextension png]
    if {$fn eq ""} return
    set fd [open $fn wb]
    puts -nonewline $fd [$im data -format png]
    close $fd
    tk_messageBox -message "Saved as $fn!"
}

set im [image create photo -width 200 -height 200]
puts [time {bilerp $im 0 1 1 0.5} 1]
pack [label .l1 -image $im]
pack [button .b -text "save" -command [list save $im]]


```


## Visual Basic .NET

```vbnet
Imports System.Drawing

Module Module1

    Function Lerp(s As Single, e As Single, t As Single) As Single
        Return s + (e - s) * t
    End Function

    Function Blerp(c00 As Single, c10 As Single, c01 As Single, c11 As Single, tx As Single, ty As Single) As Single
        Return Lerp(Lerp(c00, c10, tx), Lerp(c01, c11, tx), ty)
    End Function

    Function Scale(self As Bitmap, scaleX As Single, scaleY As Single) As Image
        Dim newWidth = CInt(Math.Floor(self.Width * scaleX))
        Dim newHeight = CInt(Math.Floor(self.Height * scaleY))
        Dim newImage As New Bitmap(newWidth, newHeight, self.PixelFormat)

        For x = 0 To newWidth - 1
            For y = 0 To newHeight - 1
                Dim gx = CSng(x) / newWidth * (self.Width - 1)
                Dim gy = CSng(y) / newHeight * (self.Height - 1)
                Dim gxi = CInt(Math.Floor(gx))
                Dim gyi = CInt(Math.Floor(gy))
                Dim c00 = self.GetPixel(gxi, gyi)
                Dim c10 = self.GetPixel(gxi + 1, gyi)
                Dim c01 = self.GetPixel(gxi, gyi + 1)
                Dim c11 = self.GetPixel(gxi + 1, gyi + 1)

                Dim red = CInt(Blerp(c00.R, c10.R, c01.R, c11.R, gx - gxi, gy - gyi))
                Dim green = CInt(Blerp(c00.G, c10.G, c01.G, c11.G, gx - gxi, gy - gyi))
                Dim blue = CInt(Blerp(c00.B, c10.B, c01.B, c11.B, gx - gxi, gy - gyi))
                Dim rgb = Color.FromArgb(red, green, blue)

                newImage.SetPixel(x, y, rgb)
            Next
        Next

        Return newImage
    End Function

    Sub Main()
        Dim newImage = Image.FromFile("Lenna100.jpg")
        If TypeOf newImage Is Bitmap Then
            Dim oi As Bitmap = newImage
            Dim result = Scale(oi, 1.6, 1.6)
            result.Save("Lenna100_larger.jpg")
        Else
            Console.WriteLine("Could not open the source file.")
        End If
    End Sub

End Module
```



## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl.

Not fast enough to be called slow.

```zkl
fcn lerp(s,e,t){ s + (e-s)*t; }
fcn blerp(c00,c10,c01,c11, tx,ty){ lerp(lerp(c00,c10,tx), lerp(c01,c11,tx),ty) }
fcn scale(src, scaleX,scaleY){
   newWidth,newHeight := Int(scaleX*src.w), Int(scaleY*src.h);
   dst:=PPM(newWidth,newHeight);
   foreach y,x in ([0.0..newHeight-1],[0.0..newWidth-1]){
      gx:=x/newWidth  *(src.w-1);
      gy:=y/newHeight *(src.h-1);
      gxi,gyi:=Int(gx), Int(gy);

      	// cxy=RGB, cxy.toBigEndian(3)-->(R,G,B)
      c00,c10 := src[gxi,gyi].toBigEndian(3), src[gxi+1,gyi].toBigEndian(3);
      c01     := src[gxi,gyi+1]  .toBigEndian(3);
      c11     := src[gxi+1,gyi+1].toBigEndian(3);

      dst[x,y] = (3).pump(Data(),  // Data is a byte bucket
         'wrap(i){ blerp(c00[i],c10[i],c01[i],c11[i], gx-gxi, gy-gyi) })
	 .toBigEndian(0,3);
   }
   dst
}
```


```zkl
img:=PPM.readPPMFile("lena.ppm");
img2:=scale(img,1.5,1.5);
img2.write(File("lena1.5.ppm","wb"));
scale(img,0.5,0.5).write(File("lena.5.ppm","wb"));
```

http://www.zenkinetic.com/Images/RosettaCode/3Lenas.jpg
