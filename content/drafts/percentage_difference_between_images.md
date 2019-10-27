+++
title = "Percentage difference between images"
description = ""
date = 2019-09-04T08:39:18Z
aliases = []
[extra]
id = 3112
[taxonomies]
categories = []
tags = []
+++

{{task|Image processing}}Compute the percentage of difference between 2 JPEG images of the same size. Alternatively, compare two bitmaps as defined in [[basic bitmap storage]].

Useful for comparing two JPEG images saved with a different compression ratios.

You can use these pictures for testing (use the full-size version of each):

{|
|-
!50% quality JPEG
!100% quality JPEG
|-
|[[Image:Lenna50.jpg|200px]]
|[[Image:Lenna100.jpg|200px]]
|-
|[http://rosettacode.org/mw/images/3/3c/Lenna50.jpg link to full size 50% image]
|[http://rosettacode.org/mw/images/b/b6/Lenna100.jpg link to full size 100% image]
|}

The expected difference for these two images is 1.62125%


## Ada


```ada
type Count is mod 2**64;
```

[[wp:Distance|1-norm distance]] in the luminance space:

```ada
function "-" (Left, Right : Luminance) return Count is
begin
   if Left > Right then
      return Count (Left) - Count (Right);
   else
      return Count (Right) - Count (Left);
   end if;
end "-";
```

1-norm distance in the color space multiplied to 3:

```ada
function "-" (Left, Right : Pixel) return Count is
begin
   return (Left.R - Right.R) + (Left.G - Left.G) + (Left.B - Right.B);
end "-";
```

Mean of 1-norm distances. Constraint_Error is propagated when images have different size.

```ada
function Diff (Left, Right : Image) return Float is
   Offs_I : constant Integer := Right'First (1) - Left'First (1);
   Offs_J : constant Integer := Right'First (2) - Left'First (2);
   Sum : Count := 0;
begin
   if Left'Length (1) /= Right'Length (1) or else Left'Length (2) /= Right'Length (2) then
      raise Constraint_Error;
   end if;
   for I in Left'Range (1) loop
      for J in Left'Range (2) loop
         Sum := Sum + (Left (I, J) - Right (I + Offs_I, J + Offs_J));
      end loop;
   end loop;
   return Float (Sum) / (3.0 * Float (Left'Length (1) * Left'Length (2)));
end Diff;
```

Example of use:

```ada
   F1, F2 : File_Type;
begin
   Open (F1, In_File, "city.ppm");
   Open (F2, In_File, "city_emboss.ppm");
   Ada.Text_IO.Put_Line ("Diff" & Float'Image (Diff (Get_PPM (F1), Get_PPM (F2))));
   Close (F1);
   Close (F2);
```


## AutoHotkey

{{works with | AutoHotkey_L}}
uses [http://www.autohotkey.com/forum/topic32238.html gdip.ahk] 

```AutoHotkey
startup()
dibSection := getPixels("lenna100.jpg")
dibSection2 := getPixels("lenna50.jpg") ; ("File-Lenna100.jpg")
pixels := dibSection.pBits
pixels2 := dibSection2.pBits
z := 0
loop % dibSection.width * dibSection.height * 4
{
x := numget(pixels - 1, A_Index, "uchar") 
y := numget(pixels2 - 1, A_Index, "uchar")
z += abs(y - x)
}
msgbox % z / (dibSection.width * dibSection.height * 3 * 255 / 100 ) ; 1.626
return

CreateDIBSection2(hDC, nW, nH, bpp = 32, ByRef pBits = "")
{
dib := object()
	NumPut(VarSetCapacity(bi, 40, 0), bi)
	NumPut(nW, bi, 4)
	NumPut(nH, bi, 8)
	NumPut(bpp, NumPut(1, bi, 12, "UShort"), 0, "Ushort")
	NumPut(0,  bi,16)
hbm := DllCall("gdi32\CreateDIBSection", "Uint", hDC, "Uint", &bi, "Uint", 0, "UintP", pBits, "Uint", 0, "Uint", 0)

dib.hbm := hbm
dib.pBits := pBits
dib.width := nW
dib.height := nH
dib.bpp := bpp
dib.header := header
	Return	dib
}



startup()
{
global disposables
disposables := object()
disposables.pBitmaps := object()
disposables.hBitmaps := object()

If !(disposables.pToken := Gdip_Startup())
{
	MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
	ExitApp
}
  OnExit, gdipExit
}


gdipExit:
loop % disposables.hBitmaps._maxindex()
DllCall("DeleteObject", "Uint", disposables.hBitmaps[A_Index])
Gdip_Shutdown(disposables.pToken)
ExitApp

getPixels(imageFile)
{
global disposables ; hBitmap will be disposed later
pBitmapFile1 := Gdip_CreateBitmapFromFile(imageFile)
hbmi := Gdip_CreateHBITMAPFromBitmap(pBitmapFile1)
width := Gdip_GetImageWidth(pBitmapFile1)
height := Gdip_GetImageHeight(pBitmapFile1)
 
	mDCo := DllCall("CreateCompatibleDC", "Uint", 0)
	mDCi := DllCall("CreateCompatibleDC", "Uint", 0)
	dibSection := CreateDIBSection2(mDCo, width, height)
	hBMo := dibSection.hbm

	oBM := DllCall("SelectObject", "Uint", mDCo, "Uint", hBMo)
	iBM := DllCall("SelectObject", "Uint", mDCi, "Uint", hbmi)

	DllCall("BitBlt", "Uint", mDCo, "int", 0, "int", 0, "int", width, "int", height, "Uint", mDCi, "int", 0, "int", 0, "Uint", 0x40000000 | 0x00CC0020)

	DllCall("SelectObject", "Uint", mDCo, "Uint", oBM)
DllCall("DeleteDC", "Uint", 0, "Uint", mDCi)
DllCall("DeleteDC", "Uint", 0, "Uint", mDCo)
Gdip_DisposeImage(pBitmapFile1)
DllCall("DeleteObject", "Uint", hBMi)
disposables.hBitmaps._insert(hBMo)
return dibSection
}
#Include Gdip.ahk  ; Thanks to tic (Tariq Porter) for his GDI+ Library

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
I would have preferred to calculate the RMS difference but it would be inconsistent with other results.

```bbcbasic
      hbm1% = FNloadimage("C:lenna50.jpg")
      hbm2% = FNloadimage("C:lenna100.jpg")
      
      SYS "CreateCompatibleDC", @memhdc% TO hdc1%
      SYS "CreateCompatibleDC", @memhdc% TO hdc2%
      
      SYS "SelectObject", hdc1%, hbm1%
      SYS "SelectObject", hdc2%, hbm2%
      
      diff% = 0
      FOR y% = 0 TO 511
        FOR x% = 0 TO 511
          SYS "GetPixel", hdc1%, x%, y% TO rgb1%
          SYS "GetPixel", hdc2%, x%, y% TO rgb2%
          diff% += ABS((rgb1% AND &FF) - (rgb2% AND &FF))
          diff% += ABS((rgb1% >> 8 AND &FF) - (rgb2% >> 8 AND &FF))
          diff% += ABS((rgb1% >> 16) - (rgb2% >> 16))
        NEXT
      NEXT y%
      PRINT "Image difference = "; 100 * diff% / 512^2 / 3 / 255 " %"
      
      SYS "DeleteDC", hdc1%
      SYS "DeleteDC", hdc2%
      SYS "DeleteObject", hbm1%
      SYS "DeleteObject", hbm2%
      END
      
      DEF FNloadimage(file$)
      LOCAL iid{}, hbm%, pic%, ole%, olpp%, text%
      DIM iid{a%,b%,c%,d%}, text% LOCAL 513
      
      iid.a% = &7BF80980 : REM. 128-bit iid
      iid.b% = &101ABF32
      iid.c% = &AA00BB8B
      iid.d% = &AB0C3000
      
      SYS "MultiByteToWideChar", 0, 0, file$, -1, text%, 256
      
      SYS "LoadLibrary", "OLEAUT32.DLL" TO ole%
      SYS "GetProcAddress", ole%, "OleLoadPicturePath" TO olpp%
      IF olpp%=0 THEN = 0
      
      SYS olpp%, text%, 0, 0, 0, iid{}, ^pic% : REM. OleLoadPicturePath
      IF pic%=0 THEN = 0
      
      SYS !(!pic%+12), pic%, ^hbm% : REM. IPicture::get_Handle
      SYS "FreeLibrary", ole%
      = hbm%
```

'''Output:'''

```txt

Image difference = 1.6255931 %

```



## C


The <tt>read_image</tt> function is from [[Read image file through a pipe|here]].


```c>#include <stdio.h

#include <stdlib.h>
#include <math.h>
/* #include "imglib.h" */

#define RED_C 0
#define GREEN_C 1
#define BLUE_C 2
#define GET_PIXEL(IMG, X, Y) ((IMG)->buf[ (Y) * (IMG)->width + (X) ])

int main(int argc, char **argv)
{
   image im1, im2;
   double totalDiff = 0.0;
   unsigned int x, y;
   
   if ( argc < 3 )
   {
      fprintf(stderr, "usage:\n%s FILE1 FILE2\n", argv[0]);
      exit(1);
   }
   im1 = read_image(argv[1]);
   if ( im1 == NULL ) exit(1);
   im2 = read_image(argv[2]);
   if ( im2 == NULL ) { free_img(im1); exit(1); }
   if ( (im1->width != im2->width) || (im1->height != im2->height) )
   {
      fprintf(stderr, "width/height of the images must match!\n");
   } else {
   /* BODY: the "real" part! */
      for(x=0; x < im1->width; x++)
      {
         for(y=0; y < im1->width; y++)
         {
           totalDiff += fabs( GET_PIXEL(im1, x, y)[RED_C] - GET_PIXEL(im2, x, y)[RED_C] ) / 255.0;
           totalDiff += fabs( GET_PIXEL(im1, x, y)[GREEN_C] - GET_PIXEL(im2, x, y)[GREEN_C] ) / 255.0;
           totalDiff += fabs( GET_PIXEL(im1, x, y)[BLUE_C] - GET_PIXEL(im2, x, y)[BLUE_C] ) / 255.0;
         }
      }
      printf("%lf\n", 100.0 * totalDiff / (double)(im1->width * im1->height * 3) );
   /* BODY ends */
   }
   free_img(im1);
   free_img(im2);
}
```


The output on Lenna is:


```txt

1.625587

```



## C++


based upon C version, using Qt 4.4

```Cpp>#include <QImage

#include <cstdlib>
#include <QColor>
#include <iostream>

int main( int argc , char *argv[ ] ) {
   if ( argc != 3 ) {
      std::cout << "Call this with imagecompare <file of image 1>" 
	 << " <file of image 2>\n" ;
      return 1 ;
   }
   QImage firstImage ( argv[ 1 ] ) ;
   QImage secondImage ( argv[ 2 ] ) ;
   double totaldiff = 0.0 ; //holds the number of different pixels
   int h = firstImage.height( ) ;
   int w = firstImage.width( ) ;
   int hsecond = secondImage.height( ) ;
   int wsecond = secondImage.width( ) ;
   if ( w != wsecond || h != hsecond ) {
      std::cerr << "Error, pictures must have identical dimensions!\n" ;
      return 2 ;
   } 
   for ( int y = 0 ; y < h ; y++ ) {
      uint *firstLine = ( uint* )firstImage.scanLine( y ) ;
      uint *secondLine = ( uint* )secondImage.scanLine( y ) ;
      for ( int x = 0 ; x < w ; x++ ) {
	 uint pixelFirst = firstLine[ x ] ;
	 int rFirst = qRed( pixelFirst ) ;
	 int gFirst = qGreen( pixelFirst ) ;
	 int bFirst = qBlue( pixelFirst ) ;
	 uint pixelSecond = secondLine[ x ] ;
	 int rSecond = qRed( pixelSecond ) ;
	 int gSecond = qGreen( pixelSecond ) ;
	 int bSecond = qBlue( pixelSecond ) ;
	 totaldiff += std::abs( rFirst - rSecond ) / 255.0 ;
	 totaldiff += std::abs( gFirst - gSecond ) / 255.0 ;
	 totaldiff += std::abs( bFirst -bSecond ) / 255.0 ;
      }
   }
   std::cout << "The difference of the two pictures is " <<
      (totaldiff * 100)  / (w * h * 3)  << " % !\n" ;
   return 0 ;
}
```


output on pictures given;

```txt

The difference of the two pictures is 1.62559 % !

```



## C#



```Csharp
using System;
using System.Drawing;

class Program
{
    static void Main()
    {
        Bitmap img1 = new Bitmap("Lenna50.jpg");
        Bitmap img2 = new Bitmap("Lenna100.jpg");

        if (img1.Size != img2.Size)
        {
            Console.Error.WriteLine("Images are of different sizes");
            return;
        }

        float diff = 0;

        for (int y = 0; y < img1.Height; y++)
        {
            for (int x = 0; x < img1.Width; x++)
            {
                Color pixel1 = img1.GetPixel(x, y);
                Color pixel2 = img2.GetPixel(x, y);

                diff += Math.Abs(pixel1.R - pixel2.R);
                diff += Math.Abs(pixel1.G - pixel2.G);
                diff += Math.Abs(pixel1.B - pixel2.B);
            }
        }

        Console.WriteLine("diff: {0} %", 100 * (diff / 255) / (img1.Width * img1.Height * 3));
    }
}
```



## Common Lisp


This is based upon the C version. Strangely enough, the percentage is 1.77% which is off by about a tenth of a percent.


```lisp
(require 'cl-jpeg)
;;; the JPEG library uses simple-vectors to store data. this is insane!
(defun compare-images (file1 file2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (image1 height width) (jpeg:decode-image file1)
    (let ((image2 (jpeg:decode-image file2))) 
      (loop for i of-type (unsigned-byte 8) across (the simple-vector image1)
            for j of-type (unsigned-byte 8) across (the simple-vector image2)
            sum (the fixnum (abs (- i j))) into difference of-type fixnum
            finally (return (coerce (/ difference width height #.(* 3 255))
                                    'double-float))))))

  CL-USER> (* 100 (compare-images "Lenna50.jpg" "Lenna100.jpg"))
  1.774856467652165d0
```



## D

{{trans|Python}}

```d
import std.stdio, std.exception, std.range, std.math, bitmap;

void main() {
    Image!RGB i1, i2;
    i1 = i1.loadPPM6("Lenna50.ppm");
    i2 = i2.loadPPM6("Lenna100.ppm");

    enforce(i1.nx == i2.nx && i1.ny == i2.ny, "Different sizes.");

    real dif = 0.0;
    foreach (p, q; zip(i1.image, i2.image))
        dif += abs(p.r - q.r) + abs(p.g - q.g) + abs(p.b - q.b);

    immutable nData = i1.nx * i1.ny * 3;
    writeln("Difference (percentage): ",
            (dif / 255.0 * 100) / nData);
}
```

{{out}}

```txt
Difference (percentage): 1.62559
```



## E


By dividing only at the end, we work with integers only as the sum and avoid floating-point error from adding small numbers (per-pixel difference) to large ones (sum of differences).


```e
def imageDifference(a, b) {
  require(a.width() == b.width())
  require(a.height() == b.height())
  def X := 0..!(a.width())
  def Y := 0..!(a.height())
  
  var sumByteDiff := 0
  for y in Y {
    for x in X {
      def ca := a[x, y]
      def cb := b[x, y]
      sumByteDiff += (ca.rb() - cb.rb()).abs() \
                   + (ca.gb() - cb.gb()).abs() \
                   + (ca.bb() - cb.bb()).abs()
    }
    println(y)
  }
  return sumByteDiff / (255 * 3 * a.width() * a.height())
}

def imageDifferenceTask() {
  println("Read 1...")
  def a := readPPM(<import:java.io.makeFileInputStream>(<file:Lenna50.ppm>))
  println("Read 2...")
  def b := readPPM(<import:java.io.makeFileInputStream>(<file:Lenna100.ppm>))
  println("Compare...")
  def d := imageDifference(a, b)
  println(`${d * 100}% different.`)
}
```


The result on the provided images is 1.6255930981604882%.

=={{header|F_Sharp|F#}}==

```fsharp

//Percentage difference between 2 images. Nigel Galloway April 18th., 2018
let img50 = new System.Drawing.Bitmap("Lenna50.jpg")
let img100 = new System.Drawing.Bitmap("Lenna100.jpg")
let diff=Seq.cast<System.Drawing.Color*System.Drawing.Color>(Array2D.init img50.Width img50.Height (fun n g->(img50.GetPixel(n,g),img100.GetPixel(n,g))))|>Seq.fold(fun i (e,l)->i+abs(int(e.R)-int(l.R))+abs(int(e.B)-int(l.B))+abs(int(e.G)-int(l.G))) 0
printfn "%f" ((float diff)*100.00/(float(img50.Height*img50.Width)*255.0*3.0))
```

{{out}}

```txt

1.774691

```



## Forth


```forth
: pixel-diff ( pixel1 pixel2 -- n )
  over 255 and over 255 and - abs >r 8 rshift swap 8 rshift
  over 255 and over 255 and - abs >r 8 rshift swap 8 rshift
                            - abs r> + r> + ;
: bdiff ( bmp1 bmp2 -- fdiff )
  2dup bdim rot bdim d<> abort" images not comparable"
  0e               ( F: total diff   )
  dup bdim * >r    ( R: total pixels )
  bdata swap bdata
  r@ 0 do
    over @ over @ pixel-diff 0 d>f f+
    cell+ swap cell+
  loop 2drop
  r> 3 * 255 * 0 d>f f/ ;

: .bdiff ( bmp1 bmp2 -- )
  cr bdiff 100e f* f. ." percent different" ;
```



## Fortran



```fortran
program ImageDifference

  use RCImageBasic
  use RCImageIO

  implicit none

  integer, parameter :: input1_u = 20, &
                        input2_u = 21

  type(rgbimage) :: lenna1, lenna2
  real           :: totaldiff


  open(input1_u, file="Lenna100.ppm", action="read")
  open(input2_u, file="Lenna50.ppm", action="read")
  call read_ppm(input1_u, lenna1)
  call read_ppm(input2_u, lenna2)
  close(input1_u)
  close(input2_u)

  totaldiff = sum(  (abs(lenna1%red - lenna2%red) + &
                     abs(lenna1%green - lenna2%green) + &
                     abs(lenna1%blue - lenna2%blue)) / 255.0 )


  print *, 100.0 * totaldiff / (lenna1%width * lenna1%height * 3.0)

  call free_img(lenna1)
  call free_img(lenna2)

end program ImageDifference
```


This gives 1.6555154.


## Frink



```frink

img1 = new image["file:Lenna50.jpg"]
img2 = new image["file:Lenna100.jpg"]

[w1, h1] = img1.getSize[]
[w2, h2] = img2.getSize[]

sum = 0
for x=0 to w1-1
   for y=0 to h1-1
   {
      [r1,g1,b1] = img1.getPixel[x,y]
      [r2,g2,b2] = img2.getPixel[x,y]
      sum = sum + abs[r1-r2] + abs[g1-g2] + abs[b1-b2]
   }

errors = sum / (w1 * h1 * 3)
println["Error is " + (errors->"percent")]

```


This gives an error of approximately 1.625593 percent.


## Go

Using standard image library:

```go
package main

import (
    "fmt"
    "image/jpeg"
    "os"
    "log"
    "image"
)

func loadJpeg(filename string) (image.Image, error) {
    f, err := os.Open(filename)
    if err != nil {
        return nil, err
    }
    defer f.Close()

    img, err := jpeg.Decode(f)
    if err != nil {
        return nil, err
    }

    return img, nil
}

func diff(a, b uint32) int64 {
    if a > b {
        return int64(a - b)
    }
    return int64(b - a)
}

func main() {
    i50, err := loadJpeg("Lenna50.jpg")
    if err != nil {
        log.Fatal(err)
    }

    i100, err := loadJpeg("Lenna100.jpg")
    if err != nil {
        log.Fatal(err)
    }

    if i50.ColorModel() != i100.ColorModel() {
        log.Fatal("different color models")
    }

    b := i50.Bounds()
    if !b.Eq(i100.Bounds()) {
        log.Fatal("different image sizes")
    }

    var sum int64
    for y := b.Min.Y; y < b.Max.Y; y++ {
        for x := b.Min.X; x < b.Max.X; x++ {
            r1, g1, b1, _ := i50.At(x, y).RGBA()
            r2, g2, b2, _ := i100.At(x, y).RGBA()
            sum += diff(r1, r2)
            sum += diff(g1, g2)
            sum += diff(b1, b2)
        }
    }

    nPixels := (b.Max.X - b.Min.X) * (b.Max.Y - b.Min.Y)
    fmt.Printf("Image difference: %f%%\n",
        float64(sum*100)/(float64(nPixels)*0xffff*3))
}
```

Output:

```txt

Image difference: 1.774785%

```

Using code from bitmap task:

```go
package main

// Files required to build supporting package raster are found in:
// * Bitmap
// * Read a PPM file

import (
    "fmt"
    "io"
    "os/exec"
    "raster"
)

func readJpeg(j string) (b *raster.Bitmap, err error) {
    c := exec.Command("convert", j, "ppm:-")
    var pipe io.Reader
    pipe, err = c.StdoutPipe()
    if err != nil {
        return
    }
    err = c.Start()
    if err != nil {
        return
    }
    return raster.ReadPpmFrom(pipe)
}

func main() {
    b1, err := readJpeg("Lenna50.jpg")
    if err != nil {
        fmt.Println(err)
        return
    }
    b2, err := readJpeg("Lenna100.jpg")
    if err != nil {
        fmt.Println(err)
        return
    }
    b1c, b1r := b1.Extent()
    b2c, b2r := b2.Extent()
    if b1c != b2c || b1r != b2r {
        fmt.Println("image extents not the same")
        return
    }
    var sum int64
    for y := 0; y < b1r; y++ {
        for x := 0; x < b1c; x++ {
            p1, _ := b1.GetPx(x, y)
            p2, _ := b2.GetPx(x, y)
            d := int64(p1.R) - int64(p2.R)
            if d < 0 {
                sum -= d
            } else {
                sum += d
            }
            d = int64(p1.G) - int64(p2.G)
            if d < 0 {
                sum -= d
            } else {
                sum += d
            }
            d = int64(p1.B) - int64(p2.B)
            if d < 0 {
                sum -= d
            } else {
                sum += d
            }
        }
    }
    fmt.Printf("Image difference: %f%%\n",
        float64(sum)*100/(float64(b1c*b1r)*255*3))
}
```

Output:

```txt

Image difference: 1.625593%

```



## Haskell


This implementation takes PPMs as input. It uses modules defined in [[Basic bitmap storage]] and [[Write ppm file]].


```haskell
import Bitmap
import Bitmap.Netpbm
import Bitmap.RGB

import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)

main = do
    [path1, path2] <- getArgs
    image1 <- readNetpbm path1
    image2 <- readNetpbm path2
    diff <- stToIO $ imageDiff image1 image2
    putStrLn $ "Difference: " ++ show (100 * diff) ++ "%"

imageDiff :: Image s RGB -> Image s RGB -> ST s Double
imageDiff image1 image2 = do
      i1 <- getPixels image1
      i2 <- getPixels image2
      unless (length i1 == length i2) $
          fail "imageDiff: Images are of different sizes"
      return $
          toEnum (sum $ zipWith minus i1 i2) /
          toEnum (3 * 255 * length i1)
  where (RGB (r1, g1, b1)) `minus` (RGB (r2, g2, b2)) =
            abs (r1 - r2) + abs (g1 - g2) + abs (b1 - b2)
```


=={{header|Icon}} and {{header|Unicon}}==
The images are opened as hidden windows and directly queried for their size and pixel contents.  RGB colour intensities range from 0-65535 and all calculations are done with large integers and converted to real at the end.
The Icon and Unicon graphics facilities are under documented with respect to some features.  Unicon can support reading and writing to a number of additional image formats.  I'm not sure if this will run under Icon or not.   Some minor reworking of the open would be the minimum requirement; however, Icon may not have read support for jpg files.


```Unicon
link printf   # for main only

procedure main()    # % difference between images
   fn1 := "Lenna100.jpg"
   fn2 := "Lenna50.jpg"
   printf("%%difference of files %i & %i = %r\n",fn1,fn2,ImageDiff(fn1,fn2)) 
end
	
procedure ImageDiff(fn1,fn2)  #: return % difference of two images
   img1 := open(1,"g","canvas=hidden","image="||fn1) | stop("Open failed ",fn1)
   img2 := open(2,"g","canvas=hidden","image="||fn2) | stop("Open failed ",fn2)
	
   if WAttrib(img1,"width") ~= WAttrib(img2,"width") | 
      WAttrib(img1,"height") ~= WAttrib(img2,"height") then 
         stop("Images must be the same size")

   pix1    := create Pixel(img1)  # access pixels one at a time
   pix2    := create Pixel(img2)  # ... facilitate interleaved access

   sum := pix := 0 
   while pix +:= 1 & p1 := csv2l(@pix1) & p2 := csv2l(@pix2) do 
      every sum +:= abs(p1[i := 1 to *p1] - p2[i])  
   every close(img1|img2)      
   return sum / (pix * 3 * 65535 / 100. )
end

procedure csv2l(p)   #: return a list from a comma separated string
L := []
p ? until pos(0) do {
   put(L,tab(find(",")|0)) 
   move(1)
   }
return L
end
```


Output:
```txt
%difference of files "Lenna100.jpg" & "Lenna50.jpg" = 1.625587
```



## J


```j
   require 'media/image3'
   'Lenna50.jpg' (+/@,@:|@:- % 2.55 * */@$@])&read_image 'Lenna100.jpg'
1.62559
```





## Java



```java
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;

public enum ImgDiffPercent {
    ;

    public static void main(String[] args) throws IOException {
        // https://rosettacode.org/mw/images/3/3c/Lenna50.jpg
        // https://rosettacode.org/mw/images/b/b6/Lenna100.jpg
        BufferedImage img1 = ImageIO.read(new File("Lenna50.jpg"));
        BufferedImage img2 = ImageIO.read(new File("Lenna100.jpg"));

        double p = getDifferencePercent(img1, img2);
        System.out.println("diff percent: " + p);
    }

    private static double getDifferencePercent(BufferedImage img1, BufferedImage img2) {
        int width = img1.getWidth();
        int height = img1.getHeight();
        int width2 = img2.getWidth();
        int height2 = img2.getHeight();
        if (width != width2 || height != height2) {
            throw new IllegalArgumentException(String.format("Images must have the same dimensions: (%d,%d) vs. (%d,%d)", width, height, width2, height2));
        }

        long diff = 0;
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                diff += pixelDiff(img1.getRGB(x, y), img2.getRGB(x, y));
            }
        }
        long maxDiff = 3L * 255 * width * height;

        return 100.0 * diff / maxDiff;
    }

    private static int pixelDiff(int rgb1, int rgb2) {
        int r1 = (rgb1 >> 16) & 0xff;
        int g1 = (rgb1 >>  8) & 0xff;
        int b1 =  rgb1        & 0xff;
        int r2 = (rgb2 >> 16) & 0xff;
        int g2 = (rgb2 >>  8) & 0xff;
        int b2 =  rgb2        & 0xff;
        return Math.abs(r1 - r2) + Math.abs(g1 - g2) + Math.abs(b1 - b2);
    }
}
```



## JavaScript



```javascript
function getImageData(url, callback) {
	var img = document.createElement('img');
	var canvas = document.createElement('canvas');

	img.onload = function () {
		canvas.width = img.width;
		canvas.height = img.height;
		var ctx = canvas.getContext('2d');
		ctx.drawImage(img, 0, 0);
		callback(ctx.getImageData(0, 0, img.width, img.height));
	};

	img.src = url;
}

function compare(firstImage, secondImage, callback) {
	getImageData(firstImage, function (img1) {
		getImageData(secondImage, function (img2) {
			if (img1.width !== img2.width || img1.height != img2.height) {
				callback(NaN);
				return;
			}

			var diff = 0;
			
			for (var i = 0; i < img1.data.length / 4; i++) {
				diff += Math.abs(img1.data[4 * i + 0] - img2.data[4 * i + 0]) / 255;
				diff += Math.abs(img1.data[4 * i + 1] - img2.data[4 * i + 1]) / 255;
				diff += Math.abs(img1.data[4 * i + 2] - img2.data[4 * i + 2]) / 255;
			}

			callback(100 * diff / (img1.width * img1.height * 3));
		});
	});
}

compare('Lenna50.jpg', 'Lenna100.jpg', function (result) {
	console.log(result);
});
```



## Julia

{{works with|Julia|0.6}}


```julia
using Images, FileIO

absdiff(a::RGB{T}, b::RGB{T}) where T = sum(abs(col(a) - col(b)) for col in (red, green, blue))
function pctdiff(A::Matrix{Color{T}}, B::Matrix{Color{T}}) where T
    size(A) != size(B) && throw(ArgumentError("images must be same-size"))
    s = zero(T)
    for (a, b) in zip(A, B)
        s += absdiff(a, b)
    end
    return 100s / 3prod(size(A))
end

img50  = load("data/lenna50.jpg") |> Matrix{RGB{Float64}};
img100 = load("data/lenna100.jpg") |> Matrix{RGB{Float64}};

d = pctdiff(img50, img100)
@printf("Percentage difference: %.4f%%\n", d)
```


{{out}}

```txt
Percentage difference: 1.6247%
```



## Kotlin

{{trans|Java}}

```scala
// version 1.2.10

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import kotlin.math.abs

fun getDifferencePercent(img1: BufferedImage, img2: BufferedImage): Double {
    val width = img1.width
    val height = img1.height
    val width2 = img2.width
    val height2 = img2.height
    if (width != width2 || height != height2) {
        val f = "(%d,%d) vs. (%d,%d)".format(width, height, width2, height2)
        throw IllegalArgumentException("Images must have the same dimensions: $f")
    }
    var diff = 0L
    for (y in 0 until height) {
        for (x in 0 until width) {
            diff += pixelDiff(img1.getRGB(x, y), img2.getRGB(x, y))
        }
    }
    val maxDiff = 3L * 255 * width * height
    return 100.0 * diff / maxDiff
}

fun pixelDiff(rgb1: Int, rgb2: Int): Int {
    val r1 = (rgb1 shr 16) and 0xff
    val g1 = (rgb1 shr 8)  and 0xff
    val b1 =  rgb1         and 0xff
    val r2 = (rgb2 shr 16) and 0xff
    val g2 = (rgb2 shr 8)  and 0xff
    val b2 =  rgb2         and 0xff
    return abs(r1 - r2) + abs(g1 - g2) + abs(b1 - b2)
}

fun main(args: Array<String>) {
    val img1 = ImageIO.read(File("Lenna50.jpg"))
    val img2 = ImageIO.read(File("Lenna100.jpg"))

    val p = getDifferencePercent(img1, img2)
    println("The percentage difference is ${"%.6f".format(p)}%")
}
```


{{out}}

```txt

The percentage difference is 1.625593%

```



## Liberty BASIC

LB uses here a DLL to allow loading the jpgs. I get the 'other' result if I use LB's native bmp load and convert the jpgs to bmp with the Gimp!
The GUI shows the 'difference image'. [http://www.diga.me.uk/right.gif SceenDisplay]

```lb

now =time$( "seconds")
nomainwin

WindowWidth  = 1620
WindowHeight =  660

open "jpeg.dll" for dll as #j   '   "JPEG.DLL copyright Alyce Watson, 2003. "

open "RC Image Comparison- difference shown as 20 times abs( pixel_difference." for graphics_nf_nsb as #1
    #1 "trapclose [quit]"
    #1 "down ; fill black"

    hW =hwnd( #1)
    calldll #user32,"GetDC", hW as ulong, hdc as ulong

    jname1$   ="Lenna50.jpg"
    hPic1     =LoadImageFile( hW, jname1$)
    if hPic1 =0 then notice "Function failed.": wait
    loadbmp "demo1", hPic1
    hDemo1 =hbmp( "demo1")
    #1 "drawbmp demo1 10 10 ; flush"

    jname2$ ="Lenna100.jpg"
    hPic2     =LoadImageFile( hW, jname2$)
    if hPic2 =0 then notice "Function failed.": wait
    loadbmp "demo2", hPic2
    hDemo1    =hbmp( "demo2")
    #1 "drawbmp demo2 550 10 ; flush"

    c1 =16777216
    c2 =   65536
    c3 =     256
    x1 =10
    y1 =10
    x2 =550
    y2 =10

    for y =0 to 511
        for x =0 to 511
            pixel1  =( GetPixel( hdc,  x1 +x, y1 +y) + c1) mod c1
            b1      = int(  pixel1  /c2)
            g1      = int(( pixel1 -b1 *c2) /c3)
            r1      = int(  pixel1 -b1 *c2 -g1 *c3)

            pixel2  =( GetPixel( hdc, x2 +x, y2 +y) + c1) mod c1
            b2      = int(  pixel2  /c2)
            g2      = int(( pixel2 -b2 *c2) /c3)
            r2      = int(  pixel2 -b2 *c2 -g2 *c3)

            totalDiff =totalDiff +abs( r1 -r2) +abs( g1 -g2)+ abs( b1 -b2)

            #1 "color "; 20 *abs( r2 -r1); " "; 20 *abs( g2 -g1); " "; 20 *abs( b2 -b1)
            #1 "set "; 1090 +x; " "; 10 +y
            scan
        next x
    next y

   #1 "place 90 575 ; color white ; backcolor black"
   #1 "font courier 9 bold"
   #1 "\"; " Difference between images          =";  using( "#.#####", 100.0 *totalDiff / 512 /512 /3 /255); "%."
   #1 "\"; " Rosetta Code for these two images  =1.62125%."; "   Time taken ="; time$( "seconds") -now; " seconds."
   #1 "flush"

wait

function LoadImageFile( hWnd, file$)
    calldll #j,     "LoadImageFile",   hWnd as ulong, file$ as ptr, LoadImageFile as ulong
end function

function GetPixel( hDC, x, y)
    calldll #gdi32, "GetPixel",        hDC As uLong,  x As long,    y As long,     GetPixel As long
end function

[quit]
    if hPic1  <>0 then calldll #gdi32, "DeleteObject", hPic1 as long, re as long
    if hPic2  <>0 then calldll #gdi32, "DeleteObject", hPic2 as long, re as long
    if hDemo1 <>0 then unloadbmp "demo1"
    if hDemo2 <>0 then unloadbmp "demo2"
    close #1
    close #j
    end

```



## Mathematica


```Mathematica
img50 = ImageData@Import[NotebookDirectory[] <> "Lenna50.jpg"];
img100 = ImageData@Import[NotebookDirectory[] <> "Lenna100.jpg"];
diff = img50 - img100;
Print["Total Difference between both Lenas = ", 
 Total@Abs@Flatten@diff/Times @@ Dimensions@img50*100, "%"]
```


'''Output'''

```txt
Total Difference between both Lenas = 1.62559%
```



## MATLAB


```MATLAB

% Percentage difference between images
function p = PercentageDifferenceBetweenImages(im1,im2)
    
if numel(im1)~=numel(im2),
    error('Error: images have to be the same size');
end

d = abs(single(im1) - single(im2))./255;
p = sum(d(:))./numel(im1)*100;

disp(['Percentage difference between images is: ', num2str(p), '%'])
```


'''Output'''

```txt
Percentage difference between images is: 1.6256%
```



## MAXScript


```maxscript
fn diffImages =
(
	local img1 = selectBitmap caption:"Select Image 1"
	local img2 = selectBitmap caption:"Select Image 2"
	local totalDiff = 0
	for i in 0 to (img1.height-1) do
	(
		local img1Row = getPixels img1 [0, i] img1.width
		local img2Row = getPixels img2 [0, i] img2.width
		
		for j in 1 to img1.width do
		(
			totalDiff += (abs (img1Row[j].r - img2Row[j].r)) / 255.0
			totalDiff += (abs (img1Row[j].g - img2Row[j].g)) / 255.0
			totalDiff += (abs (img1Row[j].b - img2Row[j].b)) / 255.0
		)
	)
	format "Diff: %\%\n" (totalDiff / ((img1.width * img1.height * 3) as float) * 100)
)
```



## OCaml

{{libheader|glMLite}}


```ocaml
#! /usr/bin/env ocaml
#directory "+glMLite/"
#load "jpeg_loader.cma"
#load "bigarray.cma"
open Jpeg_loader

let () =
  let img1, width1, height1, col_comp1, color_space1 = load_img (Filename Sys.argv.(1))
  and img2, width2, height2, col_comp2, color_space2 = load_img (Filename Sys.argv.(2)) in

  assert(width1 = width2);
  assert(height1 = height2);
  assert(col_comp1 = col_comp2);  (* number of color components *)
  assert(color_space1 = color_space2);

  let img1 = Bigarray.array3_of_genarray img1
  and img2 = Bigarray.array3_of_genarray img2 in

  let sum = ref 0.0
  and num = ref 0 in

  for x=0 to pred width1 do
    for y=0 to pred height1 do
      for c=0 to pred col_comp1 do
        let v1 = float img1.{x,y,c}
        and v2 = float img2.{x,y,c} in
        let diff = (abs_float (v1 -. v2)) /. 255. in
        sum := diff +. !sum;
        incr num;
      done;
    done;
  done;

  let diff_percent = !sum /. float !num in
  Printf.printf " diff: %f percent\n" diff_percent;
;;
```



## Perl


```perl
use Image::Imlib2;

my $img1 = Image::Imlib2->load('Lenna50.jpg')  || die;
my $img2 = Image::Imlib2->load('Lenna100.jpg') || die;

my $w = $img1->width;
my $h = $img1->height;

my $sum = 0;

for my $x (0..$w-1) {
    for my $y (0..$h-1) {
        my ($r1, $g1, $b1) = $img1->query_pixel($x, $y);
        my ($r2, $g2, $b2) = $img2->query_pixel($x, $y);
        $sum += abs($r1-$r2) + abs($g1-$g2) + abs($b1-$b2);
    }
}

printf "%% difference = %.4f\n", 100 * $sum / ($w * $h * 3 * 255);
```

{{out}}

```txt
% difference = 1.7747
```


Alternative solution:

```perl
use Imager;
use List::AllUtils qw(sum pairwise);

sub img_diff {
    my ($file1, $file2) = @_;

    my $img1 = Imager->new(file => $file1);
    my $img2 = Imager->new(file => $file2);

    my ($w1, $h1) = ($img1->getwidth, $img1->getheight);
    my ($w2, $h2) = ($img2->getwidth, $img2->getheight);

    if ($w1 != $w2 or $h1 != $h2) {
        die "Can't compare images of different sizes";
    }

    my $sum = 0;
    foreach my $y (0 .. $h1 - 1) {
        foreach my $x (0 .. $w1 - 1) {
            my @rgba1 = $img1->getpixel(x => $x, y => $y)->rgba;
            my @rgba2 = $img2->getpixel(x => $x, y => $y)->rgba;
            $sum += sum(pairwise { abs($a - $b) } @rgba1, @rgba2);
        }
    }

    $sum / ($w1 * $h1 * 255 * 3);
}

printf "difference = %f%%\n", 100 * img_diff('Lenna50.jpg', 'Lenna100.jpg');
```

{{out}}

```txt

difference = 1.625593%

```



## Perl 6


```perl6
#!/usr/bin/env perl6

use v6;
use GD::Raw;

# Reference:
# https://github.com/dagurval/perl6-gd-raw

my $fh1 = fopen('./Lenna50.jpg', "rb") or die;
my $img1 = gdImageCreateFromJpeg($fh1);
my $fh2 = fopen('./Lenna100.jpg', "rb") or die;
my $img2 = gdImageCreateFromJpeg($fh2);

my $img1X = gdImageSX($img1);
my $img1Y = gdImageSY($img1);
my $img2X = gdImageSX($img2);
my $img2Y = gdImageSY($img2);

($img1X == $img2X and $img1Y == $img2Y) or die "Image dimensions must match.";

my $diff = 0;
my ($px1, $px2);
loop (my $i = 0; $i < $img1X; $i++) {
   loop (my $j = 0; $j < $img1Y; $j++) {

      $px1 = gdImageGetPixel($img1, $i, $j);
      $px2 = gdImageGetPixel($img2, $i, $j);

      $diff += abs(gdImageRed($img1, $px1) - gdImageRed($img2, $px2));
      $diff += abs(gdImageGreen($img1, $px1) - gdImageGreen($img2, $px2));
      $diff += abs(gdImageBlue($img1, $px1) - gdImageBlue($img2, $px2));
   }
}

say "%difference = ", $diff/($img1X*$img1Y*3*255)*100;

gdImageDestroy($img1);
gdImageDestroy($img2);

```

{{out}}

```txt

%difference = 1.625593098

```



## Phix


```Phix
-- demo\rosetta\Percentage_difference_between_images.exw
include ppm.e

function split_colour(integer c)
    return sq_div(sq_and_bits(c, {#FF0000,#FF00,#FF}),
                                 {#010000,#0100,#01})
end function

function percentage_diff(sequence img1, img2)
    if length(img1)!=length(img2)
    or length(img1[1])!=length(img2[1]) then
        return "sizes do not match"
    end if
    atom diff = 0
    for i=1 to length(img1) do
        for j=1 to length(img1[i]) do
            integer {r1,g1,b1} = split_colour(img1[i,j]),
                    {r2,g2,b2} = split_colour(img2[i,j])
            diff += abs(r1-r2)+abs(g1-g2)+abs(b1-b2)
        end for 
    end for 
    return 100*diff/(length(img1)*length(img1[1]))/3/255
end function

sequence img1 = read_ppm("Lenna50.ppm"),
         img2 = read_ppm("Lenna100.ppm")
?percentage_diff(img1,img2)
```

{{out}}

```txt

1.625593098

```



## PicoLisp


```PicoLisp
(call "convert" "Lenna50.jpg" (tmp "Lenna50.ppm"))
(call "convert" "Lenna100.jpg" (tmp "Lenna100.ppm"))

(let (Total 0  Diff 0)
   (in (tmp "Lenna50.ppm")
      (in (tmp "Lenna100.ppm")
         (while (rd 1)
            (inc 'Diff
               (*/
                  (abs (- @ (in -1 (rd 1))))
                  1000000
                  255 ) )
            (inc 'Total) ) ) )
   (prinl "Difference is " (format (*/ Diff Total) 4) " percent") )
```

Output:

```txt
Difference is 1.6256 percent
```



## PureBasic

This program downloads both jpg files, decodes them & saves them in 2D-arrays for simple comparison which part is comparable with the other languages.

```PureBasic
#URL1="http://rosettacode.org/mw/images/3/3c/Lenna50.jpg"
#URL2="http://rosettacode.org/mw/images/b/b6/Lenna100.jpg"
 
Procedure.s GetTempFileName(basename$="",Extension$=".tmp")
  Protected file$, i
  Repeat: file$=GetTemporaryDirectory()+basename$+"_"+Str(i)+Extension$: i+1
  Until FileSize(file$) = -1 ; E.g. File not found
  ProcedureReturn file$
EndProcedure
 
Procedure ImageToMatrix(Image,Array P(2))
  Protected Width=ImageWidth(0)-1, Height=ImageHeight(0)-1, x, y
  ; Scaling down Width & Height by -1 to compensate for using 0-based arrays
  Dim P(Width,Height)
  StartDrawing(ImageOutput(Image))
  For x=0 To Width
    For y=0 To Height
      P(x,y)=Point(x,y)
    Next y
  Next x
  StopDrawing()
EndProcedure
 
Define File1$, File2$, totalDiff, x, y, w, h
 
; Load the pictures from RoettaCode
InitNetwork()
File1$=GetTempFileName("",".jpg"): ReceiveHTTPFile(#URL1,File1$)
File2$=GetTempFileName("",".jpg"): ReceiveHTTPFile(#URL2,File2$)
 
; Decode the images & clean up temporary files
UseJPEGImageDecoder()
LoadImage(0,File1$):LoadImage(1,File2$)
DeleteFile(File1$): DeleteFile(File2$)
 
; Make two 2D arrays to hold the data
Dim Pic1(0,0): Dim Pic2(0,0)
 
;Load the image data into the matrixes
ImageToMatrix(0,Pic1()): ImageToMatrix(1,Pic2())
 
; Compare the data
w=ArraySize(pic1()): h=ArraySize(pic1(),2)
For x=0 To w
  For y=0 To h
    totalDiff+ Abs(  Red(Pic1(x,y)) -   Red(Pic2(x,y)))
    totalDiff+ Abs(Green(Pic1(x,y)) - Green(Pic2(x,y)))
    totalDiff+ Abs( Blue(Pic1(x,y)) -  Blue(Pic2(x,y)))
  Next y
Next x
 
MessageRequester("Result","Diff= "+StrD(100*totalDiff/(255*w*h*3),3)+" %")
```



## Python

You must install the [https://pillow.readthedocs.io/ Python Imaging Library] to use this example.

{{works with|python version 3.x}}

```python
import Image

i1 = Image.open("image1.jpg")
i2 = Image.open("image2.jpg")
assert i1.mode == i2.mode, "Different kinds of images."
assert i1.size == i2.size, "Different sizes."

pairs = zip(i1.getdata(), i2.getdata())
if len(i1.getbands()) == 1:
    # for gray-scale jpegs
    dif = sum(abs(p1-p2) for p1,p2 in pairs)
else:
    dif = sum(abs(c1-c2) for p1,p2 in pairs for c1,c2 in zip(p1,p2))

ncomponents = i1.size[0] * i1.size[1] * 3
print "Difference (percentage):", (dif / 255.0 * 100) / ncomponents
```


{{works with|python version 2.x}}

```python
from itertools import izip
import Image

i1 = Image.open("image1.jpg")
i2 = Image.open("image2.jpg")
assert i1.mode == i2.mode, "Different kinds of images."
assert i1.size == i2.size, "Different sizes."

pairs = izip(i1.getdata(), i2.getdata())
if len(i1.getbands()) == 1:
    # for gray-scale jpegs
    dif = sum(abs(p1-p2) for p1,p2 in pairs)
else:
    dif = sum(abs(c1-c2) for p1,p2 in pairs for c1,c2 in zip(p1,p2))

ncomponents = i1.size[0] * i1.size[1] * 3
print "Difference (percentage):", (dif / 255.0 * 100) / ncomponents
```



## Racket

For some reason, the result is around 1.77% and I can't wrap my head about why. (Tom)
Note: On OS X I get 1.6192% as the result. (soegaard) 


```Racket
#lang racket
(require racket/draw)

(define (percentage-difference bitmap1 bitmap2)
  (define width (send bitmap1 get-width))
  (define height (send bitmap1 get-height))
  (define buffer1 (make-bytes (* width height 4)))
  (define buffer2 (make-bytes (* width height 4)))
  (send (send bitmap1 make-dc) get-argb-pixels 0 0 width height buffer1)
  (send (send bitmap2 make-dc) get-argb-pixels 0 0 width height buffer2)
  (/ (* 100.0
        (for/fold ((difference 0))
          ((i (in-naturals)) (x1 (in-bytes buffer1)) (x2 (in-bytes buffer2)))
          (if (zero? (remainder i 4))
              difference
              (+ difference (abs (- x1 x2))))))
     width height 3 256))

(define lenna50 (read-bitmap "lenna50.jpg"))
(define lenna100 (read-bitmap "lenna100.jpg"))

(percentage-difference lenna50 lenna100) ;-> 1.7749329408009846
```



## REBOL


```REBOL
REBOL [
	Title: "Percent Image Difference"
	URL: http://rosettacode.org/wiki/Percentage_of_difference_between_2_images
]

; Load from local storage. Un/comment as preferred.
; a: load-image %lenna50.jpg
; b: load-image %lenna100.jpg

; Download from rosettacode.org.
a: load-image http://rosettacode.org/mw/images/3/3c/Lenna50.jpg
b: load-image http://rosettacode.org/mw/images/b/b6/Lenna100.jpg

if a/size <> b/size [print "Image dimensions must match."  halt]

; Compute difference. REBOL has built-in image processing as part of
; its GUI package that I can take advantage of here:

diff: to-image layout/tight [image a effect [difference b]]

; Calculate deviation. I use 'repeat' to rip through the image pixels
; (it knows how to deal with images) and sum, then average. Note that
; I can treat the image like an array to get number of pixels.

t: 0
repeat p diff [t: t + p/1 + p/2 + p/3]
print rejoin ["Difference: "  100 * t / (255 * 3 * length? diff)  "%"]

; Optional: Since I now have a difference image, I may as well show
; it. Use the buttons or keys 'a', 'b' and 'd' to switch between the
; various images.

flip: func [
	"Change to new image and label."
	name [word!] "Image to switch to."
][x/text: rejoin ["Image " name]  x/image: get name  show x]

; Because the differences between the images are very small, I enhance
; the diff with a high contrast to make the result easier to
; see. Comment this out for the "pure" image.

diff: to-image layout/tight [image diff effect [contrast 100]]

view l: layout [
	x: image diff
	across 
	button "a" #"a"          [flip 'a]
	button "b" #"b"          [flip 'b]
	button "difference" #"d" [flip 'diff]
]
```


Output:


```txt
Difference: 1.62559309816049%
```


Note that this image has been contrast enhanced to highlight the differences. 

[[File:pdiff.png]]


## Ruby

uses the <code>[[Raster graphics operations/Ruby|raster_graphics]]</code> library

```ruby
require 'raster_graphics'

class RGBColour
  # the difference between two colours
  def -(a_colour)
    (@red - a_colour.red).abs +
    (@green - a_colour.green).abs +
    (@blue - a_colour.blue).abs
  end
end

class Pixmap
  # the difference between two images
  def -(a_pixmap)
    if @width != a_pixmap.width or @height != a_pixmap.height
      raise ArgumentError, "can't compare images with different sizes"
    end
    sum = 0
    each_pixel {|x,y| sum += self[x,y] - a_pixmap[x,y]}
    Float(sum) / (@width * @height * 255 * 3)
  end
end

lenna50 = Pixmap.open_from_jpeg('Lenna50.jpg')
lenna100 = Pixmap.open_from_jpeg('Lenna100.jpg')

puts "difference: %.5f%%" % (100.0 * (lenna50 - lenna100))
```


produces:

```txt
difference: 1.62559%
```



## Rust


```rust
extern crate image;

use image::{GenericImageView, Rgba};

fn diff_rgba3(rgba1 : Rgba<u8>, rgba2 : Rgba<u8>) -> i32 {
    (rgba1[0] as i32 - rgba2[0] as i32).abs()
    + (rgba1[1] as i32 - rgba2[1] as i32).abs()
    + (rgba1[2] as i32 - rgba2[2] as i32).abs()
}

fn main() {
    let img1 = image::open("Lenna100.jpg").unwrap();
    let img2 = image::open("Lenna50.jpg").unwrap();
    let mut accum = 0;
    let zipper = img1.pixels().zip(img2.pixels());
    for (pixel1, pixel2) in zipper {
        accum += diff_rgba3(pixel1.2, pixel2.2);
    }
    println!("Percent difference {}", accum as f64 * 100.0/ (255.0 * 3.0 * (img1.width() * img1.height()) as f64));
}
```


produces:

```txt
Percent difference 1.6260633281632966
```



## Sidef


```ruby
require('Imager')

func img_diff(a, b) {

    func from_file(name) {
         %s|Imager|.new(file => name)
    }

    func size(img) {
        (img.getwidth, img.getheight)
    }

    func pixel_diff(p1, p2) {
        [p1.rgba] »-« [p2.rgba] -> map { .abs }.sum
    }

    func read_pixel(img, x, y) {
        img.getpixel(x => x, y => y)
    }

    var(img1, img2) = (from_file(a), from_file(b))

    var(w1, h1) = size(img1)
    var(w2, h2) = size(img2)

    if ((w1 != w2) || (h1 != h2)) {
        return nil
    }

    var sum = 0
    for y,x in (^h1 ~X ^w1) {
        sum += pixel_diff(read_pixel(img1, x, y), read_pixel(img2, x, y))
    }

    sum / (w1 * h1 * 255 * 3)
}

say 100*img_diff('Lenna50.jpg', 'Lenna100.jpg')
```

{{out}}

```txt

1.62559309816048815359477124183007

```



## Swift


```swift
func pixelValues(fromCGImage imageRef: CGImage?) -> [UInt8]?
{
    var width = 0
    var height = 0
    var pixelValues: [UInt8]?
    
    if let imageRef = imageRef {
        width = imageRef.width
        height = imageRef.height
        let bitsPerComponent = imageRef.bitsPerComponent
        let bytesPerRow = imageRef.bytesPerRow
        let totalBytes = height * bytesPerRow
        let bitmapInfo = imageRef.bitmapInfo
        
        let colorSpace = CGColorSpaceCreateDeviceRGB()
        var intensities = [UInt8](repeating: 0, count: totalBytes)
        
        let contextRef = CGContext(data: &intensities,
                                  width: width,
                                 height: height,
                       bitsPerComponent: bitsPerComponent,
                            bytesPerRow: bytesPerRow,
                                  space: colorSpace,
                             bitmapInfo: bitmapInfo.rawValue)
        contextRef?.draw(imageRef, in: CGRect(x: 0.0, y: 0.0, width: CGFloat(width), height: CGFloat(height)))
        
        pixelValues = intensities
    }
    
    return pixelValues
}

func compareImages(image1: UIImage, image2: UIImage) -> Double? {
    guard let data1 = pixelValues(fromCGImage: image1.cgImage),
        let data2 = pixelValues(fromCGImage: image2.cgImage),
        data1.count == data2.count else {
            return nil
    }
    
    let width = Double(image1.size.width)
    let height = Double(image1.size.height)
    
    return zip(data1, data2)
        .enumerated()
        .reduce(0.0) {
            $1.offset % 4 == 3 ? $0 : $0 + abs(Double($1.element.0) - Double($1.element.1))
        }
        * 100 / (width * height * 3.0) / 255.0
}

let image1 = UIImage(named: "Lenna50")
let image2 = UIImage(named: "Lenna100")

compareImages(image1: image1, image2: image2)


```



## Tcl

{{libheader|Tk}}
This version uses the '''Img''' package, but only to provide a convenient JPEG loader; it's utterly unnecessary for the process of actually computing the difference.

```tcl
package require Tk

proc imageDifference {img1 img2} {
    if {
	[image width $img1] != [image width $img2] ||
	[image height $img1] != [image height $img2]
    } then {
	return -code error "images are different size"
    }
    set diff 0
    for {set x 0} {$x<[image width $img1]} {incr x} {
	for {set y 0} {$y<[image height $img1]} {incr y} {
	    lassign [$img1 get $x $y] r1 g1 b1
	    lassign [$img2 get $x $y] r2 g2 b2
	    incr diff [expr {abs($r1-$r2)+abs($g1-$g2)+abs($b1-$b2)}]
	}
    }
    expr {$diff/double($x*$y*3*255)}
}

# Package only used for JPEG loader
package require Img
image create photo lenna50 -file lenna50.jpg
image create photo lenna100 -file lenna100.jpg
puts "difference is [expr {[imageDifference lenna50 lenna100]*100.}]%"
exit ;# Need explicit exit here; don't want a GUI
```

It produces this output:
 difference is 1.6255930981604882%


## Vedit macro language

This implementation compares two BMP images.


```vedit
Chdir("|(USER_MACRO)\Rosetta\data")
File_Open("Lenna50.bmp", BROWSE)
#10 = Buf_Num				// #10 = buffer for 1st image
File_Open("Lenna100.bmp", BROWSE)
#20 = Buf_Num				// #20 = buffer for 2nd image

Goto_Pos(10)  				// offset to pixel data
Goto_Pos(Cur_Char + Cur_Char(1)*256)
Buf_Switch(#10)
Goto_Pos(10)
Goto_Pos(Cur_Char + Cur_Char(1)*256)

#15 = 0					// #15 = difference
#16 = 0					// #16 = total number of samples
While(!At_EOF) {
    #11 = Cur_Char; Char
    Buf_Switch(#20)
    #21 = Cur_Char; Char
    #15 += abs(#11-#21)
    #16++
    Buf_Switch(#10)
}

#19 = #15 / (#16*256/100000)
M("Sum of diff: ") NT(#15)
M("Total bytes: ") NT(#16)
M("Difference:  ") NT(#19/1000,LEFT+NOCR) M(".") NT(#19%1000,LEFT+NOCR) M("%\n")

Buf_Switch(#10) Buf_Quit(OK)
Buf_Switch(#20) Buf_Quit(OK)
```


Output, when comparing the Lenna images that were converted to BMP:

```txt

Sum of diff: 3259967
Total bytes: 786432
Difference:  1.619%

```



## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
fcn imageDiff(img1,img2){
   if(img1.w!=img2.w or img1.h!=img2.h)
      throw(Exception.ValueError("width/height of the images must match!"));
   img1.data.howza(0).walker().zip(img2.data.howza(0)) // lazy bytes, not strings
   .reduce(fcn(totalDiff,[(a,b)]){ totalDiff + (a - b).abs() },0)
   .toFloat()/img1.w/img1.h/3/255; // or: .toFloat()/img1.data.len()/255
}
```

Take the bytes in each image, zip them together [lazily], sum the differences between each byte and normalize.

```zkl
fcn readJPG2PPM(fileName){
   p:=System.popen("convert \"%s\" ppm:-".fmt(fileName),"r");
      img:=PPM.readPPM(p);
   p.close();
   img
}
```

Use the convert utility from ImageMagick to convert a JPEG image to PPM.

```zkl
imageDiff(readJPG2PPM("lenna50.jpg"),readJPG2PPM("lenna100.jpg")) :
"Image difference = %f%%".fmt(_*100).println();
```

Compute the diff between the two Lennas, format and print it. More conventionally, this would be written as

```zkl
println("Image difference = %f%%".fmt(
   imageDiff(readJPG2PPM("lenna50.jpg"),readJPG2PPM("lenna100.jpg")) * 100)
);
```
 
{{out}}

```txt

512  512  # verbiage from convert
512  512
Image difference = 1.624730%

```


{{omit from|AWK}}
{{omit from|GUISS}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|PARI/GP}}
