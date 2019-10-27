+++
title = "Hough transform"
description = ""
date = 2019-09-11T03:05:17Z
aliases = []
[extra]
id = 5346
[taxonomies]
categories = []
tags = []
+++

{{task|Image processing}}
[[Category:Graphics algorithms]]

;Task:
Implement the [[wp:Hough transform|Hough transform]], which is used as part of feature extraction with digital images.

It is a tool that makes it far easier to identify straight lines in the source image, whatever their orientation.

The transform maps each point in the target image, <math>(\rho,\theta)</math>, to the average color of the pixels on the corresponding line of the source image (in <math>(x,y)</math>-space, where the line corresponds to points of the form <math>x\cos\theta + y\sin\theta = \rho</math>). The idea is that where there is a straight line in the original image, it corresponds to a bright (or dark, depending on the color of the background field) spot; by applying a suitable filter to the results of the transform, it is possible to extract the locations of the lines in the original image.

[[Image:Pentagon.png|thumb|Sample PNG image to use for the Hough transform.]]
The target space actually uses polar coordinates, but is conventionally plotted on rectangular coordinates for display. There's no specification of exactly how to map polar coordinates to a flat surface for display, but a convenient method is to use one axis for <math>\theta</math> and the other for <math>\rho</math>, with the center of the source image being the origin.

There is also a spherical Hough transform, which is more suited to identifying planes in 3D data.





## BBC BASIC

{{works with|BBC BASIC for Windows}}
BBC BASIC uses Cartesian coordinates so the image is 'upside down' compared with some other solutions.
[[Image:hough_bbc.gif|right]]

```bbcbasic
      Width% = 320
      Height% = 240
      
      VDU 23,22,Width%;Height%;8,16,16,128
      *DISPLAY Pentagon.bmp
      OFF
      
      DIM hist%(Width%-1, Height%-1)
      
      rs = 2 * SQR(Width%^2 + Height%^2) / Height% : REM Radial step
      ts = PI / Width% : REM Angular step
      h% = Height% / 2
      
      REM Hough transform:
      FOR y% = 0 TO Height%-1
        FOR x% = 0 TO Width%-1
          IF TINT(x%*2, y%*2) = 0 THEN
            FOR t% = 0 TO Width%-1
              th = t% * ts
              r% = (x%*COS(th) + y%*SIN(th)) / rs + h% + 0.5
              hist%(t%,r%) += 1
            NEXT
          ENDIF
        NEXT
      NEXT y%
      
      REM Find max:
      max% = 0
      FOR y% = 0 TO Height%-1
        FOR x% = 0 TO Width%-1
          IF hist%(x%,y%) > max% max% = hist%(x%,y%)
        NEXT
      NEXT y%
      
      REM Plot:
      GCOL 1
      FOR y% = 0 TO Height%-1
        FOR x% = 0 TO Width%-1
          c% = 255 * hist%(x%,y%) / max%
          COLOUR 1, c%, c%, c%
          LINE x%*2,y%*2,x%*2,y%*2
        NEXT
      NEXT y%
      
      REPEAT
        WAIT 1
      UNTIL FALSE
```



## C


* see [[Example:Hough transform/C]]


## D

{{trans|Go}}
This uses the module from the Grayscale image Task. The output image is the same as in the Go solution.

```d
import std.math, grayscale_image;

Image!Gray houghTransform(in Image!Gray im,
                          in size_t hx=460, in size_t hy=360)
pure nothrow in {
    assert(im !is null);
    assert(hx > 0 && hy > 0);
    assert((hy & 1) == 0, "hy argument must be even.");
} body {
    auto result = new Image!Gray(hx, hy);
    result.clear(Gray.white);

    immutable double rMax = hypot(im.nx, im.ny);
    immutable double dr = rMax / (hy / 2.0);
    immutable double dTh = PI / hx;

    foreach (immutable y; 0 .. im.ny) {
        foreach (immutable x; 0 .. im.nx) {
            if (im[x, y] == Gray.white)
                continue;
            foreach (immutable iTh; 0 .. hx) {
                immutable double th = dTh * iTh;
                immutable double r = x * cos(th) + y * sin(th);
                immutable iry = hy / 2 - cast(int)floor(r / dr + 0.5);
                if (result[iTh, iry] > Gray(0))
                    result[iTh, iry]--;
            }
        }
    }
    return result;
}

void main() {
    (new Image!RGB)
    .loadPPM6("Pentagon.ppm")
    .rgb2grayImage()
    .houghTransform()
    .savePGM("Pentagon_hough.pgm");
}
```



## Go

[[file:GoHough.png|right|thumb|Output png]]
{{trans|Python}}

```go
package main

import (
    "fmt"
    "image"
    "image/color"
    "image/draw"
    "image/png"
    "math"
    "os"
)

func hough(im image.Image, ntx, mry int) draw.Image {
    nimx := im.Bounds().Max.X
    mimy := im.Bounds().Max.Y
    mry = int(mry/2) * 2
    him := image.NewGray(image.Rect(0, 0, ntx, mry))
    draw.Draw(him, him.Bounds(), image.NewUniform(color.White),
        image.ZP, draw.Src)

    rmax := math.Hypot(float64(nimx), float64(mimy))
    dr := rmax / float64(mry/2)
    dth := math.Pi / float64(ntx)

    for jx := 0; jx < nimx; jx++ {
        for iy := 0; iy < mimy; iy++ {
            col := color.GrayModel.Convert(im.At(jx, iy)).(color.Gray)
            if col.Y == 255 {
                continue
            }
            for jtx := 0; jtx < ntx; jtx++ {
                th := dth * float64(jtx)
                r := float64(jx)*math.Cos(th) + float64(iy)*math.Sin(th)
                iry := mry/2 - int(math.Floor(r/dr+.5))
                col = him.At(jtx, iry).(color.Gray)
                if col.Y > 0 {
                    col.Y--
                    him.SetGray(jtx, iry, col)
                }
            }
        }
    }
    return him
}

func main() {
    f, err := os.Open("Pentagon.png")
    if err != nil {
        fmt.Println(err)
        return
    }
    pent, err := png.Decode(f)
    if err != nil {
        fmt.Println(err)
        return
    }
    if err = f.Close(); err != nil {
        fmt.Println(err)
    }
    h := hough(pent, 460, 360)
    if f, err = os.Create("hough.png"); err != nil {
        fmt.Println(err)
        return
    }
    if err = png.Encode(f, h); err != nil {
        fmt.Println(err)
    }
    if cErr := f.Close(); cErr != nil && err == nil {
        fmt.Println(err)
    }
}
```



## Haskell

{{libheader|JuicyPixels}}

```Haskell
import Control.Monad (forM_, when)
import Data.Array ((!))
import Data.Array.ST (newArray, writeArray, readArray, runSTArray)
import qualified Data.Foldable as F (maximum)
import System.Environment (getArgs, getProgName)

-- Library JuicyPixels:
import Codec.Picture
       (DynamicImage(ImageRGB8, ImageRGBA8), Image, PixelRGB8(PixelRGB8),
        PixelRGBA8(PixelRGBA8), imageWidth, imageHeight, pixelAt,
        generateImage, readImage, pixelMap, savePngImage)
import Codec.Picture.Types (extractLumaPlane, dropTransparency)

dot
  :: Num a
  => (a, a) -> (a, a) -> a
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

mag
  :: Floating a
  => (a, a) -> a
mag a = sqrt $ dot a a

sub
  :: Num a
  => (a, a) -> (a, a) -> (a, a)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

fromIntegralP
  :: (Integral a, Num b)
  => (a, a) -> (b, b)
fromIntegralP (x, y) = (fromIntegral x, fromIntegral y)

{-
  Create a Hough space image with y+ measuring the distance from
  the center of the input image on the range of 0 to half the hypotenuse
  and x+ measuring from [0, 2 * pi].
  The origin is in the upper left, so y is increasing down.
  The image is scaled according to thetaSize and distSize.
-}
hough :: Image PixelRGB8 -> Int -> Int -> Image PixelRGB8
hough image thetaSize distSize = hImage
  where
    width = imageWidth image
    height = imageHeight image
    wMax = width - 1
    hMax = height - 1
    xCenter = wMax `div` 2
    yCenter = hMax `div` 2
    lumaMap = extractLumaPlane image
    gradient x y =
      let orig = pixelAt lumaMap x y
          x_ = pixelAt lumaMap (min (x + 1) wMax) y
          y_ = pixelAt lumaMap x (min (y + 1) hMax)
      in fromIntegralP (orig - x_, orig - y_)
    gradMap =
      [ ((x, y), gradient x y)
      | x <- [0 .. wMax] 
      , y <- [0 .. hMax] ]
    -- The longest distance from the center, half the hypotenuse of the image.
    distMax :: Double
    distMax = (sqrt . fromIntegral $ height ^ 2 + width ^ 2) / 2
    {-
      The accumulation bins of the polar values.
      For each value in the gradient image, if the gradient length exceeds
      some threshold, consider it evidence of a line and plot all of the
      lines that go through that point in Hough space.
    -}
    accBin =
      runSTArray $
      do arr <- newArray ((0, 0), (thetaSize, distSize)) 0
         forM_ gradMap $
           \((x, y), grad) -> do
             let (x_, y_) = fromIntegralP $ (xCenter, yCenter) `sub` (x, y)
             when (mag grad > 127) $
               forM_ [0 .. thetaSize] $
               \theta -> do
                 let theta_ =
                       fromIntegral theta * 360 / fromIntegral thetaSize / 180 *
                       pi :: Double
                     dist = cos theta_ * x_ + sin theta_ * y_
                     dist_ = truncate $ dist * fromIntegral distSize / distMax
                     idx = (theta, dist_)
                 when (dist_ >= 0 && dist_ < distSize) $
                   do old <- readArray arr idx
                      writeArray arr idx $ old + 1
         return arr
    maxAcc = F.maximum accBin
    -- The image representation of the accumulation bins.
    hTransform x y =
      let l = 255 - truncate ((accBin ! (x, y)) / maxAcc * 255)
      in PixelRGB8 l l l
    hImage = generateImage hTransform thetaSize distSize

houghIO :: FilePath -> FilePath -> Int -> Int -> IO ()
houghIO path outpath thetaSize distSize = do
  image <- readImage path
  case image of
    Left err -> putStrLn err
    Right (ImageRGB8 image_) -> doImage image_
    Right (ImageRGBA8 image_) -> doImage $ pixelMap dropTransparency image_
    _ -> putStrLn "Expecting RGB8 or RGBA8 image"
  where
    doImage image = do
      let houghImage = hough image thetaSize distSize
      savePngImage outpath $ ImageRGB8 houghImage

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  case args of
    [path, outpath, thetaSize, distSize] ->
      houghIO path outpath (read thetaSize) (read distSize)
    _ ->
      putStrLn $
      "Usage: " ++ prog ++ " <image-file> <out-file.png> <width> <height>"
```

'''Example use:'''
<lang>HoughTransform Pentagon.png hough.png 360 360
```



## J

'''Solution:'''

```j
NB.*houghTransform v Produces a density plot of image y in hough space
NB. y is picture as an array with 1 at non-white points,
NB. x is resolution (width,height) of resulting image
houghTransform=: dyad define
  'w h'=. x                               NB. width and height of target image
  theta=. o. (%~ 0.5+i.) w                NB. theta in radians from 0 to π
  rho=. (4$.$. |.y) +/ .* 2 1 o./theta    NB. rho for each pixel at each theta
  'min max'=. (,~-) +/&.:*: $y            NB. min/max possible rho
  rho=. <. 0.5+ h * (rho-min) % max-min   NB. Rescale rho from 0 to h and round to int
  |.([: <:@(#/.~) (i.h)&,)"1&.|: rho      NB. consolidate into picture
)
```

[[Image:JHoughTransform.png|320px200px|thumb|right|Resulting viewmat image from J implementation of Hough Transform on sample pentagon image]]'''Example use:'''

```j
   require 'viewmat'
   require 'media/platimg'       NB. addon required pre J8
   Img=: readimg_jqtide_ jpath '~temp/pentagon.png'
   viewmat 460 360 houghTransform _1 > Img
```

<br style="clear:both" />


## Java

'''Code:'''

```Java
import java.awt.image.*;
import java.io.File;
import java.io.IOException;
import javax.imageio.*;

public class HoughTransform
{
  public static ArrayData houghTransform(ArrayData inputData, int thetaAxisSize, int rAxisSize, int minContrast)
  {
    int width = inputData.width;
    int height = inputData.height;
    int maxRadius = (int)Math.ceil(Math.hypot(width, height));
    int halfRAxisSize = rAxisSize >>> 1;
    ArrayData outputData = new ArrayData(thetaAxisSize, rAxisSize);
    // x output ranges from 0 to pi
    // y output ranges from -maxRadius to maxRadius
    double[] sinTable = new double[thetaAxisSize];
    double[] cosTable = new double[thetaAxisSize];
    for (int theta = thetaAxisSize - 1; theta >= 0; theta--)
    {
      double thetaRadians = theta * Math.PI / thetaAxisSize;
      sinTable[theta] = Math.sin(thetaRadians);
      cosTable[theta] = Math.cos(thetaRadians);
    }
    
    for (int y = height - 1; y >= 0; y--)
    {
      for (int x = width - 1; x >= 0; x--)
      {
        if (inputData.contrast(x, y, minContrast))
        {
          for (int theta = thetaAxisSize - 1; theta >= 0; theta--)
          {
            double r = cosTable[theta] * x + sinTable[theta] * y;
            int rScaled = (int)Math.round(r * halfRAxisSize / maxRadius) + halfRAxisSize;
            outputData.accumulate(theta, rScaled, 1);
          }
        }
      }
    }
    return outputData;
  }
  
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
    
    public void accumulate(int x, int y, int delta)
    {  set(x, y, get(x, y) + delta);  }
    
    public boolean contrast(int x, int y, int minContrast)
    {
      int centerValue = get(x, y);
      for (int i = 8; i >= 0; i--)
      {
        if (i == 4)
          continue;
        int newx = x + (i % 3) - 1;
        int newy = y + (i / 3) - 1;
        if ((newx < 0) || (newx >= width) || (newy < 0) || (newy >= height))
          continue;
        if (Math.abs(get(newx, newy) - centerValue) >= minContrast)
          return true;
      }
      return false;
    }
    
    public int getMax()
    {
      int max = dataArray[0];
      for (int i = width * height - 1; i > 0; i--)
        if (dataArray[i] > max)
          max = dataArray[i];
      return max;
    }
  }
  
  public static ArrayData getArrayDataFromImage(String filename) throws IOException
  {
    BufferedImage inputImage = ImageIO.read(new File(filename));
    int width = inputImage.getWidth();
    int height = inputImage.getHeight();
    int[] rgbData = inputImage.getRGB(0, 0, width, height, null, 0, width);
    ArrayData arrayData = new ArrayData(width, height);
    // Flip y axis when reading image
    for (int y = 0; y < height; y++)
    {
      for (int x = 0; x < width; x++)
      {
        int rgbValue = rgbData[y * width + x];
        rgbValue = (int)(((rgbValue & 0xFF0000) >>> 16) * 0.30 + ((rgbValue & 0xFF00) >>> 8) * 0.59 + (rgbValue & 0xFF) * 0.11);
        arrayData.set(x, height - 1 - y, rgbValue);
      }
    }
    return arrayData;
  }
  
  public static void writeOutputImage(String filename, ArrayData arrayData) throws IOException
  {
    int max = arrayData.getMax();
    BufferedImage outputImage = new BufferedImage(arrayData.width, arrayData.height, BufferedImage.TYPE_INT_ARGB);
    for (int y = 0; y < arrayData.height; y++)
    {
      for (int x = 0; x < arrayData.width; x++)
      {
        int n = Math.min((int)Math.round(arrayData.get(x, y) * 255.0 / max), 255);
        outputImage.setRGB(x, arrayData.height - 1 - y, (n << 16) | (n << 8) | 0x90 | -0x01000000);
      }
    }
    ImageIO.write(outputImage, "PNG", new File(filename));
    return;
  }
  
  public static void main(String[] args) throws IOException
  {
    ArrayData inputData = getArrayDataFromImage(args[0]);
    int minContrast = (args.length >= 4) ? 64 : Integer.parseInt(args[4]);
    ArrayData outputData = houghTransform(inputData, Integer.parseInt(args[2]), Integer.parseInt(args[3]), minContrast);
    writeOutputImage(args[1], outputData);
    return;
  }
}
```


[[Image:JavaHoughTransform.png|640px480px|thumb|right|Output from example pentagon image]]'''Example use:'''

```txt
java HoughTransform pentagon.png JavaHoughTransform.png 640 480 100
```

<br style="clear:both" />



## Julia


```julia
using ImageFeatures

img = fill(false,5,5)
img[3,:] .= true

println(hough_transform_standard(img))

```
 {{output}} 
```txt

Tuple{Float64,Float64}[(3.0, 1.5708)]

```



## Kotlin

{{trans|Java}}

```scala
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

internal class ArrayData(val dataArray: IntArray, val width: Int, val height: Int) {

    constructor(width: Int, height: Int) : this(IntArray(width * height), width, height)

    operator fun get(x: Int, y: Int) = dataArray[y * width + x]

    operator fun set(x: Int, y: Int, value: Int) {
        dataArray[y * width + x] = value
    }

    operator fun invoke(thetaAxisSize: Int, rAxisSize: Int, minContrast: Int): ArrayData {
        val maxRadius = Math.ceil(Math.hypot(width.toDouble(), height.toDouble())).toInt()
        val halfRAxisSize = rAxisSize.ushr(1)
        val outputData = ArrayData(thetaAxisSize, rAxisSize)
        // x output ranges from 0 to pi
        // y output ranges from -maxRadius to maxRadius
        val sinTable = DoubleArray(thetaAxisSize)
        val cosTable = DoubleArray(thetaAxisSize)
        for (theta in thetaAxisSize - 1 downTo 0) {
            val thetaRadians = theta * Math.PI / thetaAxisSize
            sinTable[theta] = Math.sin(thetaRadians)
            cosTable[theta] = Math.cos(thetaRadians)
        }

        for (y in height - 1 downTo 0)
            for (x in width - 1 downTo 0)
                if (contrast(x, y, minContrast))
                    for (theta in thetaAxisSize - 1 downTo 0) {
                        val r = cosTable[theta] * x + sinTable[theta] * y
                        val rScaled = Math.round(r * halfRAxisSize / maxRadius).toInt() + halfRAxisSize
                        outputData.accumulate(theta, rScaled, 1)
                    }

        return outputData
    }

    fun writeOutputImage(filename: String) {
        val max = dataArray.max()!!
        val image = BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
        for (y in 0..height - 1)
            for (x in 0..width - 1) {
                val n = Math.min(Math.round(this[x, y] * 255.0 / max).toInt(), 255)
                image.setRGB(x, height - 1 - y, n shl 16 or (n shl 8) or 0x90 or -0x01000000)
            }

        ImageIO.write(image, "PNG", File(filename))
    }

    private fun accumulate(x: Int, y: Int, delta: Int) {
        set(x, y, get(x, y) + delta)
    }

    private fun contrast(x: Int, y: Int, minContrast: Int): Boolean {
        val centerValue = get(x, y)
        for (i in 8 downTo 0)
            if (i != 4) {
                val newx = x + i % 3 - 1
                val newy = y + i / 3 - 1
                if (newx >= 0 && newx < width && newy >= 0 && newy < height
                        && Math.abs(get(newx, newy) - centerValue) >= minContrast)
                    return true
            }
        return false
    }
}

internal fun readInputFromImage(filename: String): ArrayData {
    val image = ImageIO.read(File(filename))
    val w = image.width
    val h = image.height
    val rgbData = image.getRGB(0, 0, w, h, null, 0, w)
    // flip y axis when reading image
    val array = ArrayData(w, h)
    for (y in 0..h - 1)
        for (x in 0..w - 1) {
            var rgb = rgbData[y * w + x]
            rgb = ((rgb and 0xFF0000).ushr(16) * 0.30 + (rgb and 0xFF00).ushr(8) * 0.59 + (rgb and 0xFF) * 0.11).toInt()
            array[x, h - 1 - y] = rgb
        }

    return array
}

fun main(args: Array<out String>) {
    val inputData = readInputFromImage(args[0])
    val minContrast = if (args.size >= 4) 64 else args[4].toInt()
    inputData(args[2].toInt(), args[3].toInt(), minContrast).writeOutputImage(args[1])
}
```



## Maple


```Maple
with(ImageTools):
img := Read("pentagon.png")[..,..,1]:
img_x := Convolution (img, Matrix ([[1,2,1], [0,0,0],[-1,-2,-1]])):
img_y := Convolution (img, Matrix ([[-1,0,1],[-2,0,2],[-1,0,1]])):
img := Array (abs (img_x) + abs (img_y), datatype=float[8]):
countPixels := proc(M)
	local r,c,i,j,row,col:
	row := Array([]);
	col := Array([]);
	r,c := LinearAlgebra:-Dimensions(M);
	for i from 1 to r do
		for j from 1 to c do
			if M[i,j] <> 0 then 
				ArrayTools:-Append(row, i, inplace=true):
				ArrayTools:-Append(col, j, inplace=true):
			end if:
		end do:
	end do:
	return row,col:
end proc:
row,col := countPixels(img);
pTheta := proc(acc,r,c,x,y)
	local j, pos:
	for j from 1 to c do
		pos := ceil(x*cos((j-1)*Pi/180)+y*sin((j-1)*Pi/180)+r/2):
		acc[pos,j] := acc[pos,j]+1;
	end do:
end proc:
HoughTransform := proc(img,row,col)
   local r,c,pMax,theta,numThetas,numPs,acc,i:
   r,c := LinearAlgebra:-Dimensions(img);
   pMax := ceil(sqrt(r^2+c^2)):
   theta := [seq(evalf(i), i = 1..181, 1)]:
   numThetas := numelems(theta):
   numPs := 2*pMax+1:
   acc := Matrix(numPs, numThetas, fill=0,datatype=integer[4]):
   for i from 1 to numelems(row) do
   	pTheta(acc,numPs,numThetas,col[i],row[i]):
   end do:
   return acc;
end proc:
result :=HoughTransform(img,row,col);
Embed(Scale(FitIntensity(Create(result)), 1..500,1..500));
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==


```Mathematica

Radon[image, Method -> "Hough"]

```



## MATLAB


* see [[Example:Hough transform/MATLAB]]



## Perl

{{trans|Sidef}}

```perl
use Imager;

use constant pi => 3.14159265;

sub hough {
    my($im)     = shift;
    my($width)  = shift || 460;
    my($height) = shift || 360;
    $height = 2 * int $height/2;
 
    $height = 2 * int $height/2;
    my($xsize, $ysize) = ($im->getwidth, $im->getheight);
    my $ht = Imager->new(xsize => $width, ysize => $height);
    for my $i (0..$height-1) { for my $j (0..$width-1) { $canvas[$i][$j] = 255 } }
    $ht->box(filled => 1, color => 'white');

    $rmax = sqrt($xsize**2 + $ysize**2);
    $dr   = 2 * $rmax / $height;
    $dth  = pi / $width;

    for $x (0..$xsize-1) {
      for $y (0..$ysize-1) {
        my $col = $im->getpixel(x => $x, y => $y);
        my($r,$g,$b) = $col->rgba;
        next if $r==255; # && $g==255 && $b==255;
        for $k (0..$width) {
            $th = $dth*$k;
            $r = ($x*cos($th) + $y*sin($th));
            $iry = ($height/2 + int($r/$dr + 0.5));
            $ht->setpixel(x => $k, y => $iry, color => [ ($canvas[$iry][$k]--) x 3] );
        }
      }
    }
    return $ht;
}

my $img = Imager->new;
$img->read(file => 'ref/pentagon.png') or die "Cannot read: ", $img->errstr;
$ht->write(file => 'hough_transform.png');

```



## Perl 6

The <code>GD</code> module the output palette to 255 colors, so only transform darker pixels in the image.
{{trans|Perl}}

```perl6
use GD;

my $filename = 'pentagon.ppm';
my $in = open($filename, :r, :enc<iso-8859-1>);
my ($type, $dim, $depth) = $in.lines[^3];
my ($xsize,$ysize) = split ' ', $dim;

my ($width, $height) = 460, 360;
my $image = GD::Image.new($width, $height);

my @canvas = [255 xx $width] xx $height;

my $rmax = sqrt($xsize**2 + $ysize**2);
my $dr   = 2 * $rmax / $height;
my $dth  = π / $width;

my $pixel = 0;
my %cstore;
for $in.lines.ords -> $r, $g, $b {
    $pixel++;
    next if $r > 130;

    my $x =       $pixel % $xsize;
    my $y = floor $pixel / $xsize;

    (0..^$width).race.map: -> $k {
        my $th = $dth*$k;
        my $r = ($x*cos($th) + $y*sin($th));
        my $iry = ($height/2 + ($r/$dr).round(1)).Int;
        my $c = '#' ~ (@canvas[$iry][$k]--).base(16) x 3;
        %cstore{$c} = $image.colorAllocate($c) if %cstore{$c}:!exists;
        $image.pixel($k, $iry, %cstore{$c});
    }
}

my $png_fh = $image.open("hough-transform.png", "wb");
$image.output($png_fh, GD_PNG);
$png_fh.close;
```

See [https://github.com/thundergnat/rc/blob/master/img/hough-transform.png Hough Transform] (offsite .png image)


## Phix

{{libheader|pGUI}}
{{trans|Sidef}}

```Phix
-- demo\rosetta\Hough_transform.exw
include pGUI.e

function hypot(atom a,b) return sqrt(a*a+b*b) end function

function hough_transform(imImage im, integer width=460, height=360)
    height = 2*floor(height / 2)
    integer xsize = im_width(im),
            ysize = im_width(im)
    sequence ht = repeat(repeat(repeat(255,3),width),height)
    sequence canvas = repeat(repeat(255,width),height)
    atom rmax = hypot(xsize, ysize),
         dr = 2*(rmax / height),
         dth = (PI / width)
    for y=0 to ysize-1 do
        for x=0 to xsize-1 do
            integer {r,g,b} = im_pixel(im, x, y)
            if r!=255 then
                for k=1 to width do
                    atom th = dth*(k-1),
                         r2 = (x*cos(th) + y*sin(th))
                    integer iry = (height/2 + floor(r2/dr + 0.5))+1,
                            cik = canvas[iry][k] - 1
                    canvas[iry][k] = cik
                    ht[iry][k] = repeat(cik,3)
                end for
            end if
        end for
    end for
    ht = flatten(ht) -- (needed by IupImageRGB)
    Ihandle new_img = IupImageRGB(width, height, ht) 
    return new_img
end function
 
IupOpen()

atom pError = allocate(machine_word())
imImage im1 = imFileImageLoadBitmap("Pentagon.png",0,pError)
if im1=NULL then ?"error opening Pentagon.png" abort(0) end if
Ihandln image1 = IupImageFromImImage(im1),
        image2 = hough_transform(im1),
        label1 = IupLabel(),
        label2 = IupLabel()
IupSetAttributeHandle(label1, "IMAGE", image1)
IupSetAttributeHandle(label2, "IMAGE", image2)

Ihandle dlg = IupDialog(IupHbox({label1, label2}))
IupSetAttribute(dlg, "TITLE", "Hough transform")
IupCloseOnEscape(dlg)
IupShow(dlg)

IupMainLoop()
IupClose()
```



## Python

{{libheader|PIL}}
This is the classical Hough transform as described in wikipedia. The code does not compute averages; it merely makes a point on the transformed image darker if a lot of points on the original image lie on the corresponding line. The output is almost identical to that of the Tcl code. The code works only with gray-scale images, but it is easy to extend to RGB.

```python

from math import hypot, pi, cos, sin
from PIL import Image


def hough(im, ntx=460, mry=360):
    "Calculate Hough transform."
    pim = im.load()
    nimx, mimy = im.size
    mry = int(mry/2)*2          #Make sure that this is even
    him = Image.new("L", (ntx, mry), 255)
    phim = him.load()

    rmax = hypot(nimx, mimy)
    dr = rmax / (mry/2)
    dth = pi / ntx

    for jx in xrange(nimx):
        for iy in xrange(mimy):
            col = pim[jx, iy]
            if col == 255: continue
            for jtx in xrange(ntx):
                th = dth * jtx
                r = jx*cos(th) + iy*sin(th)
                iry = mry/2 + int(r/dr+0.5)
                phim[jtx, iry] -= 1
    return him


def test():
    "Test Hough transform with pentagon."
    im = Image.open("pentagon.png").convert("L")
    him = hough(im)
    him.save("ho5.bmp")


if __name__ == "__main__": test()


```


{{omit from|PARI/GP}}


## Racket

* see [[Example:Hough transform/Racket]]


## Ruby



```Ruby

require 'mathn'
require 'rubygems'
require 'gd2'
include GD2

def hough_transform(img)
  mx, my = img.w*0.5, img.h*0.5
  max_d = Math.sqrt(mx**2 + my**2)
  min_d = max_d * -1
  hough = Hash.new(0)
  (0..img.w).each do |x|
    puts "#{x} of #{img.w}"
    (0..img.h).each do |y|
      if img.pixel2color(img.get_pixel(x,y)).g > 32
        (0...180).each do |a|
          rad = a * (Math::PI / 180.0)
          d = (x-mx) * Math.cos(rad) + (y-my) * Math.sin(rad)
          hough["#{a.to_i}_#{d.to_i}"] = hough["#{a.to_i}_#{d.to_i}"] + 1
        end
      end
    end
  end
  heat = GD2::Image.import 'heatmap.png'
  out = GD2::Image::TrueColor.new(180,max_d*2)
  max = hough.values.max
  p max
  hough.each_pair do |k,v|
    a,d = k.split('_').map(&:to_i)
    c = (v / max) * 255
    c = heat.get_pixel(c,0)
    out.set_pixel(a, max_d + d, c)
  end
  out
end
```



## Scala

{{trans|Kotlin}}

```scala
import java.awt.image._
import java.io.File
import javax.imageio._

object HoughTransform extends App {
    override def main(args: Array[String]) {
        val inputData = readDataFromImage(args(0))
        val minContrast = if (args.length >= 4) 64 else args(4).toInt
        inputData(args(2).toInt, args(3).toInt, minContrast).writeOutputImage(args(1))
    }

    private def readDataFromImage(filename: String) = {
        val image = ImageIO.read(new File(filename))
        val width = image.getWidth
        val height = image.getHeight
        val rgbData = image.getRGB(0, 0, width, height, null, 0, width)
        val arrayData = new ArrayData(width, height)
        for (y <- 0 until height; x <- 0 until width) {
            var rgb = rgbData(y * width + x)
            rgb = (((rgb & 0xFF0000) >>> 16) * 0.30 + ((rgb & 0xFF00) >>> 8) * 0.59 +
                    (rgb & 0xFF) * 0.11).toInt
            arrayData(x, height - 1 - y) = rgb
        }
        arrayData
    }
}

class ArrayData(val width: Int, val height: Int) {
    def update(x: Int, y: Int, value: Int) {
        dataArray(x)(y) = value
    }

    def apply(thetaAxisSize: Int, rAxisSize: Int, minContrast: Int) = {
        val maxRadius = Math.ceil(Math.hypot(width, height)).toInt
        val halfRAxisSize = rAxisSize >>> 1
        val outputData = new ArrayData(thetaAxisSize, rAxisSize)
        val sinTable = Array.ofDim[Double](thetaAxisSize)
        val cosTable = sinTable.clone()
        for (theta <- thetaAxisSize - 1 until -1 by -1) {
            val thetaRadians = theta * Math.PI / thetaAxisSize
            sinTable(theta) = Math.sin(thetaRadians)
            cosTable(theta) = Math.cos(thetaRadians)
        }
        for (y <- height - 1 until -1 by -1; x <- width - 1 until -1 by -1)
            if (contrast(x, y, minContrast))
                for (theta <- thetaAxisSize - 1 until -1 by -1) {
                    val r = cosTable(theta) * x + sinTable(theta) * y
                    val rScaled = Math.round(r * halfRAxisSize / maxRadius).toInt + halfRAxisSize
                    outputData.dataArray(theta)(rScaled) += 1
                }

        outputData
    }

    def writeOutputImage(filename: String) {
        var max = Int.MinValue
        for (y <- 0 until height; x <- 0 until width) {
            val v = dataArray(x)(y)
            if (v > max) max = v
        }
        val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
        for (y <- 0 until height; x <- 0 until width) {
            val n = Math.min(Math.round(dataArray(x)(y) * 255.0 / max).toInt, 255)
            image.setRGB(x, height - 1 - y, (n << 16) | (n << 8) | 0x90 | -0x01000000)
        }
        ImageIO.write(image, "PNG", new File(filename))
    }

    private def contrast(x: Int, y: Int, minContrast: Int): Boolean = {
        val centerValue = dataArray(x)(y)
        for (i <- 8 until -1 by -1 if i != 4) {
            val newx = x + (i % 3) - 1
            val newy = y + (i / 3) - 1
            if (newx >= 0 && newx < width && newy >= 0 && newy < height &&
                    Math.abs(dataArray(newx)(newy) - centerValue) >= minContrast)
                return true
        }

        false
    }

    private val dataArray = Array.ofDim[Int](width, height)
}
```



## SequenceL

{{trans|Java}}
'''Tail-Recursive SequenceL Code:'''


```sequencel>import <Utilities/Sequence.sl
;
import <Utilities/Math.sl>;

hough: int(2) * int * int * int -> int(2);
hough(image(2), thetaAxisSize, rAxisSize, minContrast) :=
    let
        initialResult[r,theta] := 0 foreach r within 1 ... rAxisSize, theta within 1 ... thetaAxisSize;
        
        result := houghHelper(image, minContrast, 1, 1, initialResult);
        
        max := vectorMax(vectorMax(result));
    in
        255 - min(round((result * 255 / max)), 255);

houghHelper(image(2), minContrast, x, y, result(2)) :=
    let
        thetaAxisSize := size(head(result));
        rAxisSize := size(result);
        
        width := size(head(image));
        height := size(image);
        maxRadius := ceiling(sqrt(width^2 + height^2));
        halfRAxisSize := rAxisSize / 2;
        
        rs[theta] := round((cos(theta) * x + sin(theta) * y) * halfRAxisSize / maxRadius) + halfRAxisSize
                     foreach theta within (0 ... (thetaAxisSize-1)) * pi / thetaAxisSize;
        
        newResult[r,theta] := result[r,theta] + 1 when rs[theta] = r-1 else result[r,theta];
        
        nextResult := result when not checkContrast(image, x, y, minContrast) else newResult;
        
        nextX := 1 when x = width else x + 1;
        nextY := y + 1 when x = width else y;
    in
        nextResult when x = width and y = height
    else
        houghHelper(image, minContrast, nextX, nextY, nextResult);
        
checkContrast(image(2), x, y, minContrast) := 
    let
        neighbors[i,j] := image[i,j] when i > 0 and i < size(image) and j > 0 and j < size(image[i])
                          foreach i within y-1 ... y+1, 
                                  j within x-1 ... x+1;
    in
        some(some(abs(image[y,x] - neighbors) >= minContrast));
```


'''C++ Driver Code:'''

{{libheader|CImg}}

```c
#include "SL_Generated.h"
#include "CImg.h"

using namespace cimg_library;

int main( int argc, char** argv )
{
    string fileName = "Pentagon.bmp";
    if(argc > 1) fileName = argv[1];
    int thetaAxisSize = 640; if(argc > 2) thetaAxisSize = atoi(argv[2]);
    int rAxisSize = 480; if(argc > 3) rAxisSize = atoi(argv[3]);
    int minContrast = 64; if(argc > 4) minContrast = atoi(argv[4]);
    int threads = 0; if(argc > 5) threads = atoi(argv[5]);
    char titleBuffer[200];
    SLTimer t;

    CImg<int> image(fileName.c_str());
    int imageDimensions[] = {image.height(), image.width(), 0};
    Sequence<Sequence<int> > imageSeq((void*) image.data(), imageDimensions);
    Sequence< Sequence<int> > result;

    sl_init(threads);

    t.start();
    sl_hough(imageSeq, thetaAxisSize, rAxisSize, minContrast, threads, result);
    t.stop();
    
    CImg<int> resultImage(result[1].size(), result.size());
    for(int y = 0; y < result.size(); y++)
        for(int x = 0; x < result[y+1].size(); x++)
            resultImage(x,result.size() - 1 - y) = result[y+1][x+1];
    
    sprintf(titleBuffer, "SequenceL Hough Transformation: %d X %d Image to %d X %d Result | %d Cores | Processed in %f sec\0", 
                         image.width(), image.height(), resultImage.width(), resultImage.height(), threads, t.getTime());
    resultImage.display(titleBuffer);

    sl_done();
    return 0;
}
```


{{out}}
[http://i.imgur.com/McCuZP3.png Output Screenshot]


## Sidef

{{trans|Python}}

```ruby
require('Imager')

func hough(im, width=460, height=360) {

    height = 2*floor(height / 2)

    var xsize = im.getwidth
    var ysize = im.getheight

    var ht = %s|Imager|.new(xsize => width, ysize => height)
    var canvas = height.of { width.of(255) }

    ht.box(filled => true, color => 'white')

    var rmax = hypot(xsize, ysize)
    var dr = 2*(rmax / height)
    var dth = (Num.pi / width)

    for y,x in (^ysize ~X ^xsize) {
        var col = im.getpixel(x => x, y => y)
        var (r,g,b) = col.rgba
        (r==255 && g==255 && b==255) && next
        for k in ^width {
            var th = dth*k
            var r = (x*cos(th) + y*sin(th))
            var iry = (height/2 + int(r/dr + 0.5))
            ht.setpixel(x => k, y => iry, color => 3.of(--canvas[iry][k]))
        }
    }

    return ht
}

var img = %s|Imager|.new(file => 'Pentagon.png')
var ht = hough(img)
ht.write(file => 'Hough transform.png')
```



## Tcl

{{libheader|Tk}}

* See [[Example:Hough transform/Tcl]]


## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
{{trans|D}}

```zkl
const WHITE=0xffFFff, X=0x010101;
fcn houghTransform(image,hx=460,hy=360){
   if(hy.isOdd) hy-=1; // hy argument must be even
   out:=PPM(hx,hy,WHITE);
   rMax:=image.w.toFloat().hypot(image.h);
   dr,dTh:=rMax/(hy/2), (0.0).pi/hx;

   foreach y,x in (image.h,image.w){
      if(image[x,y]==WHITE) continue;
      foreach iTh in (hx){
         th,r:=dTh*iTh, th.cos()*x + th.sin()*y;
	 iry:=hy/2 + (r/dr + 0.5).floor();  // y==0 is top 
	 if(out[iTh,iry]>0) out[iTh,iry]=out[iTh,iry] - X;
      }
   }
   out
}
```



```zkl
fcn readPNG2PPM(fileName){
   p:=System.popen("convert \"%s\" ppm:-".fmt(fileName),"r");
      img:=PPM.readPPM(p);
   p.close();
   img
}
houghTransform(readPNG2PPM("pentagon.png"))
.write(File("pentagon_hough.ppm","wb"));
```

{{out}}
The output image looks the same as in the Go solution.

http://www.zenkinetic.com/Images/RosettaCode/pentagon_hough.jpg

{{omit from|PARI/GP}}

[[Category:Geometry]]
