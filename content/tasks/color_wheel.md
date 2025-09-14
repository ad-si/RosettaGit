+++
title = "Color wheel"
description = ""
date = 2019-10-14T00:53:23Z
aliases = []
[extra]
id = 21070
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "applescript",
  "formulae",
  "gml",
  "go",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "perl",
  "perl_6",
  "phix",
  "python",
  "sidef",
  "smart_basic",
  "zkl",
]
+++

## Task

Write a function to draw a HSV color wheel<ref>[https://en.wikipedia.org/wiki/HSL_and_HSV]</ref> completely with code. 

This is strictly for learning purposes only. It's highly recommended that you use an image in an actual application to actually draw the color wheel   (as procedurally drawing is super slow). This does help you understand how color wheels work and this can easily be used to determine a color value based on a position within a circle.




## AppleScript



```AppleScript
 
choose color default color {0, 0, 0, 0}

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Color_wheel this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## GML


```GML
 
for (var i = 1; i <= 360; i++) {
    for (var j = 0; j < 255; j++) {

        var hue = 255*(i/360);
        var saturation = j;
        var value = 255;

        var c = make_colour_hsv(hue,saturation,value);
        
        //size of circle determined by how far from the center it is
        //if you just draw them too small the circle won't be full. 
        //it will have patches inside it that didn't get filled in with color
        var r = max(1,3*(j/255));

        //Math for built-in GMS functions
        //lengthdir_x(len,dir) = +cos(degtorad(direction))*length;
        //lengthdir_y(len,dir) = -sin(degtorad(direction))*length;
        draw_circle_colour(x+lengthdir_x(m_radius*(j/255),i),y+lengthdir_y(m_radius*(j/255),i),r,c,c,false);
    }
}

```



## Go

```go
package main

import (
    "github.com/fogleman/gg"
    "math"
)

const tau = 2 * math.Pi

func hsb2rgb(hue, sat, bri float64) (r, g, b int) {
    u := int(bri*255 + 0.5)
    if sat == 0 {
        r, g, b = u, u, u
    } else {
        h := (hue - math.Floor(hue)) * 6
        f := h - math.Floor(h)
        p := int(bri*(1-sat)*255 + 0.5)
        q := int(bri*(1-sat*f)*255 + 0.5)
        t := int(bri*(1-sat*(1-f))*255 + 0.5)
        switch int(h) {
        case 0:
            r, g, b = u, t, p
        case 1:
            r, g, b = q, u, p
        case 2:
            r, g, b = p, u, t
        case 3:
            r, g, b = p, q, u
        case 4:
            r, g, b = t, p, u
        case 5:
            r, g, b = u, p, q
        }
    }
    return
}

func colorWheel(dc *gg.Context) {
    width, height := dc.Width(), dc.Height()
    centerX, centerY := width/2, height/2
    radius := centerX
    if centerY < radius {
        radius = centerY
    }
    for y := 0; y < height; y++ {
        dy := float64(y - centerY)
        for x := 0; x < width; x++ {
            dx := float64(x - centerX)
            dist := math.Sqrt(dx*dx + dy*dy)
            if dist <= float64(radius) {
                theta := math.Atan2(dy, dx)
                hue := (theta + math.Pi) / tau
                r, g, b := hsb2rgb(hue, 1, 1)
                dc.SetRGB255(r, g, b)
                dc.SetPixel(x, y)
            }
        }
    }
}

func main() {
    const width, height = 480, 480
    dc := gg.NewContext(width, height)
    dc.SetRGB(1, 1, 1) // set background color to white
    dc.Clear()
    colorWheel(dc)
    dc.SavePNG("color_wheel.png")
}
```


```txt

Image is same as Kotlin entry

```




## Julia


```julia
using Gtk, Graphics, Colors

const win = GtkWindow("Color Wheel", 450, 450) |> (const can = @GtkCanvas())
set_gtk_property!(can, :expand, true)

@guarded draw(can) do widget
    ctx = getgc(can)
    h = height(can)
    w = width(can)
    center = (x = w / 2, y = h / 2)
    anglestep = 1/w
    for θ in 0:0.1:360
        rgb = RGB(HSV(θ, 1, 1))
        set_source_rgb(ctx, rgb.r, rgb.g, rgb.b)
        line_to(ctx, center...)
        arc(ctx, center.x, center.y, w/2.2, 2π * θ / 360, anglestep)
        line_to(ctx, center...)
        stroke(ctx)
    end
end

show(can)
const condition = Condition()
endit(w) = notify(condition)
signal_connect(endit, win, :destroy)
wait(condition)

```



## Kotlin

We reuse the class in the Bitmap task for this and add a member function to draw the color wheel. To give a more 'wheel-like' image, a constant 'saturation' of 1.0 has been used rather than one which varies in line with distance from the center.

```scala
// Version 1.2.41

import java.awt.Color
import java.awt.Graphics
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import kotlin.math.*

class BasicBitmapStorage(width: Int, height: Int) {
    val image = BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    fun fill(c: Color) {
        val g = image.graphics
        g.color = c
        g.fillRect(0, 0, image.width, image.height)
    }

    fun setPixel(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB())

    fun getPixel(x: Int, y: Int) = Color(image.getRGB(x, y))

    fun colorWheel() {
        val centerX = image.width / 2
        val centerY = image.height / 2
        val radius = minOf(centerX, centerY)
        for (y in 0 until image.height) {
            val dy = (y - centerY).toDouble()
            for (x in 0 until image.width) {
                val dx = (x - centerX).toDouble()
                val dist = sqrt(dx * dx + dy * dy)
                if (dist <= radius) {
                    val theta = atan2(dy, dx)
                    val hue = (theta + PI) / (2.0 * PI)
                    val rgb = Color.HSBtoRGB(hue.toFloat(), 1.0f, 1.0f)
                    setPixel(x, y, Color(rgb))
                }
            }
        }
    }
}

fun main(args: Array<String>) {
    val bbs = BasicBitmapStorage(480, 480)
    with (bbs) {
        fill(Color.white)
        colorWheel()
        val cwFile = File("Color_wheel.png")
        ImageIO.write(image, "png", cwFile)
    }
}

```


```txt

Looks like mirror image of Smart BASIC entry 

```


## M2000 Interpreter


```M2000 Interpreter

Module Check {
      \\ we use an internal object for Math functions (here for Atan2)
      Declare Math Math
      Const tau=2*Pi, Center=2
      \\ change console size,  and center it ( using ;) to current monitor      
      Window 12, 800*twipsX,600*twipsY;
      \\ actual size maybe less (so can fit text exactly)
      Double  ' Double height characters
      Report Center, "Color wheel"
      Normal  ' restore to normal
      Atan2=Lambda Math (a, b) ->{
            Method Math, "Atan2", a, b As ret
            =ret
      }
      \\ brightness=1 for this program
      hsb2rgb=Lambda (hue, sat) ->{
            If sat == 0 Then {
                = 255, 255, 255
           } Else {
                  h=frac(hue+1)*6
                  f = frac(h)  
                  p = Int((1-sat)*255 + 0.5)
                  q = Int((1-sat*f)*255 + 0.5)
                  t = Int((1-sat*(1-f))*255 + 0.5)
                  Select Case Int(h)
                  Case 1
                      = q, 255, p
                  Case 2
                      = p, 255, t
                  Case 3
                     = p, q, 255
                  Case 4
                      = t, p, 255
                  Case 5
                      = 255, p, q
                  Else Case
                      = 255, t, p
                  End Select
          }
      }
      Let OffsetX=X.twips/2-128*TwipsX, OffsetY=Y.twips/2-128*TwipsY
      \\ a pixel has a size of TwipsX x TwipsY
      OffsetX=(OffsetX div TwipsX)*TwipsX
      OffsetY=(OffsetY div TwipsY)*TwipsY
      \\ We set hsb2rgb, OffsetX, OffsetY as closures to PrintPixel
      \\ We send to stack the R G B values using Stack ! array
      \\ hsb2rgb() return an array of values
      \\ we pop these values using Number
      PrintPixel = Lambda  hsb2rgb, OffsetX, OffsetY (x,y, theta, sat)  -> {
            Stack ! hsb2rgb(theta,sat)  
            PSet Color(number, number, number), x*TwipsX+offsetX, y*TwipsY+offsetY
      }
      \\ set Atan2, tau as closures to HueCircle
      \\ we can rotate/flip the wheel by changing signs in Atan2() and
      \\ by changing order of arguments (dx,dy) or (dy,dx). 8 combinations
      HueCircle= Lambda Atan2, tau (PrintPixel) -> {
            Let  c_width=256, c_height=256
            Let  cx=c_width/2, cy=c_height/2
            Let  radius=If(cx<=cy->cx, cy)
            c_width--
            c_height--
            dy=-cy
            For y=0 To c_height {
                  dy++ : dy2=dy*dy : dx=-cx
                  For x=0 To c_width {
                        dx++ : dist=Sqrt(dx^2+dy2)
                        If dist>radius Then continue
                        Call PrintPixel(x,y, Atan2(dx, -dy)/tau, dist/radius)
                  }
            }
      }
      Call HueCircle(PrintPixel)
      Scr$=""  ' we use this string  to load an image
      Move 0,0
      \\ scale.x, scale.y are twips height and width, of current layer
      Copy scale.x, scale.y to Scr$
      Clipboard Scr$  ' save window to clipboard
}
Check

```

see [https://4.bp.blogspot.com/-0swVvNDaTjE/XDlPfuGQkBI/AAAAAAAAHno/wU3eyo1BUIEtPjZMyjGXkbN425zHJlc7wCLcBGAs/s1600/colorwheel.png this image]


## Perl

```perl
use Imager;
use Math::Complex qw(cplx i pi);

my ($width, $height) = (300, 300);
my $center = cplx($width/2, $height/2);

my $img = Imager->new(xsize => $width,
                      ysize => $height);

foreach my $y (0 .. $height - 1) {
    foreach my $x (0 .. $width - 1) {

        my $vec = $center - $x - $y * i;
        my $mag = 2 * abs($vec) / $width;
        my $dir = (pi + atan2($vec->Re, $vec->Im)) / (2 * pi);

        $img->setpixel(x => $x, y => $y,
            color => {hsv => [360 * $dir, $mag, $mag < 1 ? 1 : 0]});
    }
}

$img->write(file => 'color_wheel.png');
```



## Perl 6

```perl6
use Image::PNG::Portable;

my ($w, $h) = 300, 300;

my $out = Image::PNG::Portable.new: :width($w), :height($h);

my $center = $w/2 + $h/2*i;

color-wheel($out);

$out.write: 'Color-wheel-perl6.png';

sub color-wheel ( $png ) {
    ^$w .race.map: -> $x {
        for ^$h -> $y {
            my $vector    = $center - $x - $y*i;
            my $magnitude = $vector.abs * 2 / $w;
            my $direction = ( π + atan2( |$vector.reals ) ) / τ;
            $png.set: $x, $y, |hsv2rgb( $direction, $magnitude, $magnitude < 1 );
        }
    }
}

sub hsv2rgb ( $h, $s, $v ){
    my $c = $v * $s;
    my $x = $c * (1 - abs( (($h*6) % 2) - 1 ) );
    my $m = $v - $c;
    (do given $h {
        when   0..^1/6 { $c, $x, 0 }
        when 1/6..^1/3 { $x, $c, 0 }
        when 1/3..^1/2 { 0, $c, $x }
        when 1/2..^2/3 { 0, $x, $c }
        when 2/3..^5/6 { $x, 0, $c }
        when 5/6..1    { $c, 0, $x }
    } ).map: ((*+$m) * 255).Int
}
```


Until local image uploading is re-enabled, see [https://github.com/thundergnat/rc/blob/master/img/Color-wheel-perl6.png Color-wheel-perl6.png]


## Phix

```Phix
-- demo\rosetta\Colour_wheel.exw
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

function hsv_to_rgb(atom h, s, v)
    atom r,g,b
    if s=0 then
        {r,g,b} @= v
    else
        integer i = floor(h*6)
        atom f = h*6-i,
             p = v*(1-s),
             q = v*(1-s*f),
             t = v*(1-s*(1-f))
        switch i do
            case 0,
                 6: {r,g,b} = {v, t, p}
            case 1: {r,g,b} = {q, v, p}
            case 2: {r,g,b} = {p, v, t}
            case 3: {r,g,b} = {p, q, v}
            case 4: {r,g,b} = {t, p, v}
            case 5: {r,g,b} = {v, p, q}
        end switch
    end if
    return cdEncodeColor(r*255, g*255, b*255)
end function

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    integer {w, h} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    integer radius = floor(min(w,h)/2)
    integer cx = floor(w/2),
            cy = floor(h/2)
    for x=1 to w do
        for y=1 to h do
            integer rx = x - cx,
                    ry = y - cy
            atom s = sqrt(rx*rx+ry*ry) / radius
            if s <= 1.0 then
                atom hue = ((atan2(ry, rx) / PI) + 1.0) / 2.0
                cdCanvasPixel(cddbuffer, x, h-y, hsv_to_rgb(hue, s, 1)) 
            end if 
        end for
    end for
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    cdCanvasSetForeground(cddbuffer, CD_MAGENTA)
    return IUP_DEFAULT
end function

procedure main()
    IupOpen()
    
    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "400x400") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Colour wheel")
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))
    IupCloseOnEscape(dlg)

    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## Python



```python
from PIL import Image
import colorsys
import math

if __name__ == "__main__":

	im = Image.new("RGB", (300,300))
	radius = min(im.size)/2.0
	centre = im.size[0]/2, im.size[1]/2
	pix = im.load()

	for x in range(im.width):
		for y in range(im.height):
			rx = x - centre[0]
			ry = y - centre[1]
			s = ((x - centre[0])**2.0 + (y - centre[1])**2.0)**0.5 / radius
			if s <= 1.0:
				h = ((math.atan2(ry, rx) / math.pi) + 1.0) / 2.0
				rgb = colorsys.hsv_to_rgb(h, s, 1.0)
				pix[x,y] = tuple([int(round(c*255.0)) for c in rgb])

	im.show()
```



## Racket


With the colors package


```racket
#lang racket

(require racket/draw
         colors)

(define DIM 500)
(define target (make-bitmap DIM DIM))
(define dc (new bitmap-dc% [bitmap target]))
(define radius 200)
(define center (/ DIM 2))

(define (atan2 y x) (if (= 0 y x) 0 (atan y x)))

(for* ([x (in-range DIM)]
       [y (in-range DIM)]
       [rx (in-value (- x center))]
       [ry (in-value (- y center))]
       [s (in-value (/ (sqrt (+ (sqr rx) (sqr ry))) radius))]
       #:when (<= s 1))
  (define h (* 0.5 (+ 1 (/ (atan2 ry rx) pi))))
  (send dc set-pen (hsv->color (hsv (if (= 1 h) 0 h) s 1)) 1 'solid)
  (send dc draw-point x y))

target
```



## Run BASIC


```Runbasic
' -----------------------------------
' color wheel
' -----------------------------------
global pi
pi	= 22 / 7
steps	= 1

graphic #g, 525, 525


for x =0 to 525 step steps
	for y =0 to 525 step steps
		angle   = atan2(y - 250, x - 250) * 360 / 2 / pi      '  full degrees....
		sector  = int(angle / 60)                             '    60 degree sectors (0 to 5)
		slope   = (angle mod 60) /60 * 255                    '     1 degree sectors.
		
		if  sector = 0 then  col$    = "255 ";                    str$( int( slope));      "   0"
		if  sector = 1 then  col$    = str$(int(256 - slope)); " 255                          0"
		if  sector = 2 then  col$    = "0                         255 ";                     str$( int( slope))
		if  sector = 3 then  col$    = "0 ";                      str$( int( 256 -slope)); " 255"
		if  sector = 4 then  col$    = str$(int(slope));    "     0                        255"
		if  sector = 5 then  col$    = "255                         0 ";                     str$( int( 256 -slope))
		
		red	= val( word$( col$, 1))
		grn	= val( word$( col$, 2))
		blu	= val( word$( col$, 3))
		p	= ((x -270)^2 +(y -270)^2)^0.5 / 250
		r	= min(255,p * red)
		g	= min(255,p * grn)
		b	= min(255,p * blu)
		if p > 1 then  #g "color white" else #g color(r,g,b)
		#g "set "; x; " "; y
	next y
next x
render #g
end

function atan2(y,x)
if (x = 0) and (y <> 0) then
	r$ = "Y"
	if y > 0 then atan2 = pi /2
	if y < 0 then atan2 = 3 * pi /2
end if

if y = 0 and (x <> 0) then
	r$ = "Y"
	if x > 0 then atan2 = 0
	if x < 0 then atan2 = pi
end if

If r$ <> "Y" then
	if x = 0 and y = 0 then
		atan2	= 0
		else
		baseAngle = atn(abs(y) / abs(x))
		if x > 0 then
			if y > 0 then atan2 = baseAngle
			If y < 0 then atan2 = 2 * pi - baseAngle
		end if
		if x < 0 then
			If y > 0 then atan2 = pi - baseAngle
			If y < 0 then atan2 = pi + baseAngle
		end if
	end if
end if
end function
```



## Sidef

```ruby
require('Imager')

var (width, height) = (300, 300)
var center = Complex(width/2 , height/2)

var img = %O<Imager>.new(xsize => width, ysize => height)

for y=(^height), x=(^width) {
    var vector    = (center - x - y.i)
    var magnitude = (vector.abs * 2 / width)
    var direction = ((Num.pi + atan2(vector.real, vector.imag)) / Num.tau)
    img.setpixel(x => x, y => y,
        color => Hash(hsv => [360*direction, magnitude, magnitude < 1 ? 1 : 0])
    )
}

img.write(file => 'color_wheel.png')
```

Output image: [https://github.com/trizen/rc/blob/master/img/color-wheel-sidef.png Color wheel]


## Smart BASIC


```smart basic
' Runs on iOS
GET SCREEN SIZE sw,sh
xmax=0.45*3/7*(sw+sh)
x0=sw/2!y0=sh/2
twopi=2*3.1415926
GRAPHICS
GRAPHICS CLEAR
DIM triX(1000), triY(1000)
triX(0)=x0 ! triY(0)=y0
steps=INT(1^2*360)+1
dAngle=twopi/steps
dAngle2=dAngle/2
REFRESH OFF
FOR i=0 TO steps-1
  pal(i/steps+TintOffset)
  ANGLE=i*dAngle
  FILL COLOR pal.r,pal.g,pal.b
  DRAW COLOR pal.r,pal.g,pal.b
  x=x0+(xmax-radius)*COS(ANGLE)
  y=y0-(xmax-radius)*SIN(ANGLE)
  k=0
  FOR j=-dAngle2 TO dAngle2 STEP 0.02
    k+=1
    triX(k)=x0+xmax*COS(ANGLE+j)
    triY(k)=y0-xmax*SIN(ANGLE+j)
  NEXT j
  k+=1
  triX(k)=x0+xmax*COS(ANGLE+dAngle2)
  triY(k)=y0-xmax*SIN(ANGLE+dAngle2)
  DRAW POLY triX,triY COUNT k+1
  FILL POLY triX,triY COUNT k+1
NEXT i
REFRESH ON
END

DEF pal(tint)
tint=tint*360
h=(tint%360)/60 ! f=FRACT(h) ! z=1-f ! ic=FLOOR(h)+1
ON ic GOTO s1,s2,s3,s4,s5,s6
  s1: r=1 ! g=f ! b=0 ! GOTO done
  s2: r=z ! g=1 ! b=0 ! GOTO done
  s3: r=0 ! g=1 ! b=f ! GOTO done
  s4: r=0 ! g=z ! b=1 ! GOTO done
  s5: r=f ! g=0 ! b=1 ! GOTO done
  s6: r=1 ! g=0 ! b=z ! done:
END DEF
```

View the output on Dropbox
https://www.dropbox.com/s/g3l5rbywo34bnp6/IMG_4600.PNG?dl=0


## zkl

Uses Image Magick and
the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
var w=300,h=300,out=PPM(w,h);
colorWheel(out);
out.writeJPGFile("colorWheel.zkl.jpg");

fcn colorWheel(ppm){
   zero,R:=ppm.w/2, zero;
   foreach x,y in (w,h){
      v,hue:=(x - zero).toFloat().toPolar(y - zero); 
      if(v<=R){    // only render in the circle
	 if((hue = hue.toDeg())<0) hue+=360;  // (-pi..pi] to [0..2pi)
	 s:=v/R;  // scale saturation zero at center to 1 at edge
	 ppm[x,y]=hsv2rgb(hue,1.0,s);
      }
   }
}

fcn hsv2rgb(hue,v,s){  //  0<=H<360, 0<=v(brightness)<=1, 0<=saturation<=1
		       // --> 24 bit RGB each R,G,B in [0..255]
   to24bit:=fcn(r,g,b,m){
      r,g,b=((r+m)*255).toInt(),((g+m)*255).toInt(),((b+m)*255).toInt();
      r*0x10000 + g*0x100 + b
   };
   c:=v*s;
   x:=c*(1.0 - (hue.toFloat()/60%2 - 1).abs());
   m:=v - c;
   if     (0  <=hue< 60) return(to24bit(c,  x,  0.0,m));
   else if(60 <=hue<120) return(to24bit(x,  c,  0.0,m));
   else if(120<=hue<180) return(to24bit(0.0,c,  x,  m));
   else if(180<=hue<240) return(to24bit(0.0,x,  c,  m));
   else if(240<=hue<300) return(to24bit(x,  0.0,c,  m));
   else			 return(to24bit(c,  0.0,x,  m));
}
```

See [http://www.zenkinetic.com/Images/RosettaCode/colorWheel.zkl.jpg this image]

==References==

<references/>
