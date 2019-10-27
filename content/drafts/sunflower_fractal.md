+++
title = "Sunflower fractal"
description = ""
date = 2019-09-01T06:47:16Z
aliases = []
[extra]
id = 21914
[taxonomies]
categories = []
tags = []
+++

{{task}}
Draw a [https://1drv.ms/u/s!AqDUIunCqVnIg1U89bApXAzPU9XH Sunflower fractal]







## C

The colouring of the "fractal" is determined with every iteration to ensure that the resulting graphic looks similar to a real Sunflower, thus the parameter ''diskRatio'' determines the radius of the central disk as the maximum radius of the flower is known from the number of iterations. The scaling factor is currently hardcoded but can also be externalized. Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.


```C

/*Abhishek Ghosh, 14th September 2018*/

#include<graphics.h>
#include<math.h>

#define pi M_PI

void sunflower(int winWidth, int winHeight, double diskRatio, int iter){
	double factor = .5 + sqrt(1.25),r,theta;
	double x = winWidth/2.0, y = winHeight/2.0;
	double maxRad = pow(iter,factor)/iter;
	
	int i;
	
	setbkcolor(LIGHTBLUE);
	
	for(i=0;i<=iter;i++){
		r = pow(i,factor)/iter;
		
		r/maxRad < diskRatio?setcolor(BLACK):setcolor(YELLOW);
		
		theta = 2*pi*factor*i;
		circle(x + r*sin(theta), y + r*cos(theta), 10 * i/(1.0*iter));
	}
}

int main()
{
	initwindow(1000,1000,"Sunflower...");
	
	sunflower(1000,1000,0.5,3000);
	
	getch();
	
	closegraph();
	
	return 0;
}

```



## FreeBASIC


```freebasic

Const PI As Double = 4 * Atn(1)
Const ancho = 400
Const alto =  400

Screenres ancho, alto, 8
Windowtitle" Pulsa una tecla para finalizar"
Cls

Sub Sunflower(semillas As Integer)
    Dim As Double c = (Sqr(5)+1)/2
    
    For i As Integer = 0 To semillas
        Dim As Double r = (i^c) / semillas
        Dim As Double angulo = 2 * Pi * c * i
        Dim As Double x = r * Sin(angulo) + 200
        Dim As Double y = r * Cos(angulo) + 200
        
        Circle (x, y), i/semillas*10, i/semillas*10
    Next i
End Sub

Sunflower(2000)
Bsave "sunflower_fractal.bmp",0
Sleep
End

```



## Go

{{libheader|Go Graphics}} 
{{trans|Ring}}


The image produced, when viewed with (for example) EOG, is similar to the Ring entry.

```go
package main

import (
    "github.com/fogleman/gg"
    "math"
)

func main() {
    dc := gg.NewContext(400, 400)
    dc.SetRGB(1, 1, 1)
    dc.Clear()
    dc.SetRGB(0, 0, 1)
    c := (math.Sqrt(5) + 1) / 2
    numberOfSeeds := 3000
    for i := 0; i <= numberOfSeeds; i++ {
        fi := float64(i)
        fn := float64(numberOfSeeds)
        r := math.Pow(fi, c) / fn
        angle := 2 * math.Pi * c * fi
        x := r*math.Sin(angle) + 200
        y := r*math.Cos(angle) + 200
        fi /= fn / 5
        dc.DrawCircle(x, y, fi)
    }
    dc.SetLineWidth(1)
    dc.Stroke()
    dc.SavePNG("sunflower_fractal.png")
}
```



## javascript

HTML to test

```txt

<!DOCTYPE html>
<html>
    <head>
        <meta charset="utf-8" />
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <title>Vibrating rectangles</title>
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <style>
            body{background-color:black;text-align:center;margin-top:150px}
        </style>
        <script src="sunflower.js"></script>
    </head>
    <body onload="start()">
        <div id='wnd'></div>
    </body>
</html>

```


```javascript

const SIZE = 400, HS = SIZE >> 1, WAIT = .005, SEEDS = 3000, 
      TPI = Math.PI * 2, C = (Math.sqrt(10) + 1) / 2;
class Sunflower {
    constructor() {
        this.wait = WAIT;
        this.colorIndex = 0;
        this.dimension = 0;
        this.lastTime = 0;
        this.accumulator = 0;
        this.deltaTime = 1 / 60;
        this.colors = ["#ff0000", "#ff8000", "#ffff00", "#80ff00", "#00ff00", "#00ff80", 
                       "#00ffff", "#0080ff", "#0000ff", "#8000ff", "#ff00ff", "#ff0080"];
        this.canvas = document.createElement('canvas');
        this.canvas.width = SIZE;
        this.canvas.height = SIZE;
        const d = document.getElementById("wnd");
        d.appendChild(this.canvas);
        this.ctx = this.canvas.getContext('2d');
    }
    draw(clr, d) {
        let r = Math.pow(d, C) / SEEDS;
        let angle = TPI * C * d;
        let x = HS + r * Math.sin(angle), 
            y = HS + r * Math.cos(angle);
        this.ctx.strokeStyle = clr;
        this.ctx.beginPath();
        this.ctx.arc(x, y, d / (SEEDS / 50), 0, TPI);
        this.ctx.closePath();
        this.ctx.stroke();
    }
    update(dt) {
        if((this.wait -= dt) < 0) {
            this.draw(this.colors[this.colorIndex], this.dimension);
            this.wait = WAIT;
            if((this.dimension++) > 600) {
                this.dimension = 0;
                this.colorIndex = (this.colorIndex + 1) % this.colors.length;
            }
        }
    }
    start() {
        this.loop = (time) => {
            this.accumulator += (time - this.lastTime) / 1000;
            while(this.accumulator > this.deltaTime) {
                this.accumulator -= this.deltaTime;
                this.update(Math.min(this.deltaTime));
            }
            this.lastTime = time;
            requestAnimationFrame(this.loop);
        }
        this.loop(0);
    }
}
function start() {
    const sunflower = new Sunflower();
    sunflower.start();
}


```




## Julia

{{trans|R}}
Run from REPL.

```julia
using Makie

function sunflowerplot()
    len = 2000
    ϕ = 0.5 + sqrt(5) / 2
    r = LinRange(0.0, 100.0, len)
    θ = zeros(len)
    markersizes = zeros(Int, len)
    for i in 2:length(r)
        θ[i] = θ[i - 1] + 2π * ϕ
        markersizes[i] = div(i, 500) + 3
    end
    x = r .* cos.(θ)
    y = r .* sin.(θ)
    scene = Scene(backgroundcolor=:green)
    scatter!(scene, x, y, color=:gold, markersize=markersizes, strokewidth=1, fill=false, show_axis=false)
end

sunflowerplot()

```



## Microsoft Small Basic

{{trans|Ring}}

```smallbasic
' Sunflower fractal - 24/07/2018
  GraphicsWindow.Width=410
  GraphicsWindow.Height=400
  c=(Math.SquareRoot(5)+1)/2
  numberofseeds=3000
  For i=0 To numberofseeds
    r=Math.Power(i,c)/numberofseeds
    angle=2*Math.Pi*c*i
    x=r*Math.Sin(angle)+200
    y=r*Math.Cos(angle)+200
    GraphicsWindow.DrawEllipse(x, y, i/numberofseeds*10, i/numberofseeds*10)
  EndFor 
```

{{out}}
[https://1drv.ms/u/s!AoFH_AlpH9oZgf5kvtRou1Wuc5lSCg Sunflower fractal]


## Objeck

{{trans|C}}

```perl
use Game.SDL2;
use Game.Framework;

class Test {
  @framework : GameFramework;
  @colors : Color[];
  
  function : Main(args : String[]) ~ Nil {
    Test->New()->Run();
  }
  
  New() {
    @framework := GameFramework->New(GameConsts->SCREEN_WIDTH, GameConsts->SCREEN_HEIGHT, "Test");
    @framework->SetClearColor(Color->New(0, 0, 0));
    @colors := Color->New[2];
    @colors[0] := Color->New(255,128,0); 
    @colors[1] := Color->New(255,255,25); 
  }
  
  method : Run() ~ Nil {
    if(@framework->IsOk()) {
      e := @framework->GetEvent();
      
      quit := false;
      while(<>quit) {
        # process input
        while(e->Poll() <> 0) {
          if(e->GetType() = EventType->SDL_QUIT) {
            quit := true;
          };
        };

        @framework->FrameStart();
        Render(525,525,0.50,3000);
        @framework->FrameEnd();
      };
    }
    else {
      "--- Error Initializing Environment ---"->ErrorLine();
      return;
    };

    leaving {
      @framework->Quit();
    };
  }

  method : Render(winWidth : Int, winHeight : Int, diskRatio : Float, iter : Int) ~ Nil {
    renderer := @framework->GetRenderer();

    @framework->Clear();

    factor := 0.5 + 1.25->SquareRoot();
    x := winWidth / 2.0;
    y := winHeight / 2.0;
    maxRad := Float->Power(iter, factor) / iter;

    for(i:=0;i<=iter;i+=1;) {
      r := Float->Power(i,factor)/iter;
      color := r/maxRad < diskRatio ? @colors[0] : @colors[1];
      theta := 2*Float->Pi()*factor*i;
      renderer->CircleColor(x + r*theta->Sin(), y + r*theta->Cos(), 10 * i/(1.0*iter), color);
    };
        
    @framework->Show();
  }
}

consts GameConsts {
  SCREEN_WIDTH := 640,
  SCREEN_HEIGHT := 480
}

```



## Perl

{{trans|Sidef}}

```perl
use utf8;
use constant π => 3.14159265;
use constant φ => (1 + sqrt(5)) / 2;

my $scale = 600;
my $seeds = 5*$scale;

print qq{<svg xmlns="http://www.w3.org/2000/svg" width="$scale" height="$scale" style="stroke:gold">
           <rect width="100%" height="100%" fill="black" />\n};

for $i (1..$seeds) {
    $r = 2 * ($i**φ) / $seeds;
    $t = 2 * π * φ * $i;
    $x = $r * sin($t) + $scale/2;
    $y = $r * cos($t) + $scale/2;
    printf qq{<circle cx="%.2f" cy="%.2f" r="%.1f" />\n}, $x, $y, sqrt($i)/13;
}

print "</svg>\n";
```

See [https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/sunflower.svg Phi-packing image] (SVG image)


## Perl 6

{{works with|Rakudo|2018.06}}
This is not really a fractal. It is more accurately an example of a Fibonacci spiral or Phi-packing.

Or, to be completely accurate: It is a variation of a generative [[wp:Fermat's_spiral|Fermat's spiral]] using the Vogel model to implement phi-packing. See: [https://thatsmaths.com/2014/06/05/sunflowers-and-fibonacci-models-of-efficiency/ https://thatsmaths.com/2014/06/05/sunflowers-and-fibonacci-models-of-efficiency] 


```perl6
use SVG;

my $seeds  = 3000;
my @center = 300, 300;
my $scale  = 5;

constant \φ = (3 - 5.sqrt) / 2;

my @c = map {
    my ($x, $y) = ($scale * .sqrt) «*« |cis($_ * φ * τ).reals »+« @center;
    [ $x.round(.01), $y.round(.01), (.sqrt * $scale / 100).round(.1) ]
}, 1 .. $seeds;

say SVG.serialize(
    svg => [
        :600width, :600height, :style<stroke:yellow>,
        :rect[:width<100%>, :height<100%>, :fill<black>],
        |@c.map( { :circle[:cx(.[0]), :cy(.[1]), :r(.[2])] } ),
    ],
);
```

See: [https://github.com/thundergnat/rc/blob/master/img/phi-packing-perl6.svg Phi packing] (SVG image)


## Phix


```Phix
constant numberofseeds = 3000

include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

procedure cdCanvasCircle(cdCanvas cddbuffer, atom x, y, r)
    cdCanvasArc(cddbuffer,x,y,r,r,0,360)
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)

    integer {hw, hh} = sq_floor_div(IupGetIntInt(canvas, "DRAWSIZE"),2)
    atom s = min(hw,hh)/150,
         f = min(hw,hh)*8/125
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    atom c = (sqrt(5)+1)/2
    for i=0 to numberofseeds do
        atom r = power(i,c)/numberofseeds,
             angle = 2*PI*c*i,
             x = s*r*sin(angle)+hw,
             y = s*r*cos(angle)+hh
        cdCanvasCircle(cddbuffer,x,y,i/numberofseeds*f)
    end for 
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    cdCanvasSetForeground(cddbuffer, CD_BLACK)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()
    
    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "602x502") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Sunflower")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL) -- release the minimum limitation
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure
main()
```



## R


```R

phi=1/2+sqrt(5)/2
r=seq(0,1,length.out=2000)
theta=numeric(length(r))
theta[1]=0
for(i in 2:length(r)){
  theta[i]=theta[i-1]+phi*2*pi
}
x=r*cos(theta)
y=r*sin(theta)
par(bg="black")
plot(x,y)
size=seq(.5,2,length.out = length(x))
thick=seq(.1,2,length.out = length(x))
for(i in 1:length(x)){
  points(x[i],y[i],cex=size[i],lwd=thick[i],col="goldenrod1")
}

```

{{Out}}
[https://raw.githubusercontent.com/schwartstack/sunflower/master/sunflower2.png Sunflower]


## Racket


{{trans|C}}


```racket
#lang racket

(require 2htdp/image)

(define N 3000)
(define DISK-RATIO 0.5)
(define factor (+ 0.5 (sqrt 1.25)))
(define WIDTH 500)
(define HEIGHT 500)
(define max-rad (/ (expt N factor) N))

(for/fold ([image (empty-scene WIDTH HEIGHT)]) ([i (in-range N)])
  (define r (/ (expt i factor) N))
  (define color (if (< (/ r max-rad) DISK-RATIO) 'brown 'darkyellow))
  (define theta (* 2 pi factor i))
  (place-image (circle (* 10 i (/ 1 N)) 'outline color)
               (+ (/ WIDTH 2) (* r (sin theta)))
               (+ (/ HEIGHT 2) (* r (cos theta)))
               image))
```



## Ring


```ring

# Project : Sunflower fractal

load "guilib.ring"

paint = null

new qapp 
        {
        win1 = new qwidget() {
                  setwindowtitle("Sunflower fractal")
                  setgeometry(100,100,320,500)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,400,400)
                              settext("")
                  }
                  new qpushbutton(win1) {
                          setgeometry(100,400,100,30)
                          settext("draw")
                          setclickevent("draw()")
                  }
                  show()
        }
        exec()
        }

func draw
        p1 = new qpicture()
               color = new qcolor() {
               setrgb(0,0,255,255)
        }
        pen = new qpen() {
                 setcolor(color)
                 setwidth(1)
        }
        paint = new qpainter() {
                  begin(p1)
                  setpen(pen)

        c = (sqrt(5) + 1) / 2
        numberofseeds = 3000
        for i = 0 to numberofseeds
              r = pow(i, c ) / (numberofseeds)
              angle = 2 * 3.14 * c * i
              x = r * sin(angle) + 100
              y = r * cos(angle) + 100
             drawellipse(x, y, i / (numberofseeds / 10), i / (numberofseeds / 10))
        next

        endpaint()
        }
        label1 { setpicture(p1) show() }

```

Output:

[https://1drv.ms/u/s!AqDUIunCqVnIg1U89bApXAzPU9XH Sunflower fractal]


## Sidef

{{trans|Go}}

```ruby
require('Imager')

func draw_sunflower(seeds=3000) {
    var img = %O<Imager>.new(
        xsize => 400,
        ysize => 400,
    )

    var c = (sqrt(1.25) + 0.5)
    { |i|
        var r = (i**c / seeds)
        var θ = (2 * Num.pi * c * i)
        var x = (r * sin(θ) + 200)
        var y = (r * cos(θ) + 200)
        img.circle(x => x, y => y, r => i/(5*seeds))
    } * seeds

    return img
}

var img = draw_sunflower()
img.write(file => "sunflower.png")
```

Output image: [https://github.com/trizen/rc/blob/master/img/sunflower-sidef.png Sunflower fractal]


## zkl

{{trans|Go}}
Uses Image Magick and
the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
fcn sunflower(seeds=3000){
   img,color := PPM(400,400), 0x00ff00;		// green
   c:=((5.0).sqrt() + 1)/2;
   foreach n in ([0.0 .. seeds]){  // floats
      r:=n.pow(c)/seeds;
      x,y := r.toRectangular(r.pi*c*n*2);
      r=(n/seeds*5).toInt();
      img.circle(200 + x, 200 + y, r,color);
   }
   img.writeJPGFile("sunflower.zkl.jpg");
}();
```

{{out}}
Image at [http://www.zenkinetic.com/Images/RosettaCode/sunflower.zkl.jpg sunflower fractal]
