+++
title = "Vibrating rectangles"
description = ""
date = 2019-08-31T13:38:27Z
aliases = []
[extra]
id = 21906
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

::#Draw at least 20 rectangles with a common center, to be more precise, the circumcenter of all the rectangles must coincide. None of the rectangles must touch or intersect any other rectangle. 
::#Animate the colours of the rectangles by ''fading in'' the colour from the outermost rectangle to the innermost. 
::#The animation loop can continue for a definite number of iterations or forever. 

Create [https://1drv.ms/v/s!AqDUIunCqVnIg1MxKPi5DzwUbJEf Vibrating rectangles]



## C

Dimensions of the rectangles, their number and the animation delay can be configured. Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.

```C

/*Abhishek Ghosh, 15th September 2018*/

#include<graphics.h>

void vibratingRectangles(int winWidth,int winHeight,int leastLength,int leastWidth,int num, int msec)
{
	int color = 1,i,x = winWidth/2, y = winHeight/2;
	
	while(!kbhit()){
		setcolor(color++);
		for(i=num;i>0;i--){
			rectangle(x - i*leastLength,y - i*leastWidth,x + i*leastLength,y + i*leastWidth);
			delay(msec);
		}

		if(color>MAXCOLORS){
			color = 1;
		}
	}
}

int main()
{
	initwindow(1000,1000,"Vibrating Rectangles...");
	
	vibratingRectangles(1000,1000,30,15,20,500);
	
	closegraph();
	
	return 0;
}

```



## EasyLang


[https://easylang.online/apps/run.html?code=on%20timer%0Asz%20-%3D%202%0Aif%20sz%20%3C%200%0Asz%20%3D%2049%0Acolor%20random%201000%0A.%0Amove%2050%20-%20sz%2050%20-%20sz%0Aline%2050%20%2B%20sz%2050%20-%20sz%0Aline%2050%20%2B%20sz%2050%20%2B%20sz%0Aline%2050%20-%20sz%2050%20%2B%20sz%0Aline%2050%20-%20sz%2050%20-%20sz%0Atimer%200.2%0A.%0Atimer%200.2 Run it]

<lang>on timer
  sz -= 2
  if sz < 0
    sz = 49
    color random 1000
  .
  move 50 - sz 50 - sz
  line 50 + sz 50 - sz
  line 50 + sz 50 + sz
  line 50 - sz 50 + sz
  line 50 - sz 50 - sz
  timer 0.2
.
timer 0.2
```



## Factor


```factor
USING: accessors calendar colors.constants combinators kernel
locals math math.vectors opengl timers ui ui.gadgets
ui.gadgets.worlds ui.pens.solid ui.render ;
IN: rosetta-code.vibrating-squares

TUPLE: vibrating < gadget
    { old-color initial: COLOR: black }
    { new-color initial: COLOR: red }
    { frame initial: 0 } ;

DEFER: on-tick

: <vibrating-gadget> ( -- gadget )
    vibrating new COLOR: black <solid> >>interior COLOR: red
    >>new-color COLOR: black >>old-color dup [ on-tick ] curry f
    250 milliseconds <timer> start-timer ;

M: vibrating pref-dim* drop { 420 420 } ;

: draw-squares ( loc dim n -- loc' dim' )
    [ 2dup gl-rect [ { 10 10 } v+ ] [ { -20 -20 } v+ ] bi* ]
    times ;

M:: vibrating draw-gadget* ( GADGET -- )
    GADGET frame>> 20 mod :> n
    GADGET new-color>> gl-color
    { 10 10 } { 400 400 } n draw-squares
    GADGET old-color>> gl-color
    20 n - draw-squares 2drop ;

:: on-tick ( GADGET -- )
    GADGET relayout-1
    GADGET [ 1 + ] change-frame frame>> 20 mod zero? [
        GADGET new-color>> GADGET old-color<<
        GADGET new-color>> {
            { COLOR: red [ COLOR: green ] }
            { COLOR: green [ COLOR: blue ] }
            [ drop COLOR: red ]
        } case GADGET new-color<<
    ] when ;

MAIN-WINDOW: vibrating-squares
    {
        { title "Vibrating Squares" }
        { window-controls
            { normal-title-bar close-button minimize-button } }
    } <vibrating-gadget> >>gadgets ;
```



## Go

This uses Go's 'image' packages in its standard library to create an animated GIF.

When played this is somewhat similar to the Python entry except that it uses a 7 (rather than 6) color palette and repeats indefinitely.

Although the .gif works fine in Firefox it might not do so in EOG due to optimizations made during its creation. If so, then the following ImageMagick command should fix it:

```txt

  $ convert vibrating.gif -coalesce vibrating2.gif
  $ eog vibrating2.gif

```


```go
package main

import (
    "image"
    "image/color"
    "image/gif"
    "log"
    "os"
)

var (
    black   = color.RGBA{0, 0, 0, 255}
    red     = color.RGBA{255, 0, 0, 255}
    green   = color.RGBA{0, 255, 0, 255}
    blue    = color.RGBA{0, 0, 255, 255}
    magenta = color.RGBA{255, 0, 255, 255}
    cyan    = color.RGBA{0, 255, 255, 255}
    yellow  = color.RGBA{255, 255, 0, 255}
    white   = color.RGBA{255, 255, 255, 255}
)

var palette = []color.Color{red, green, blue, magenta, cyan, yellow, white, black}

func hline(img *image.Paletted, x1, y, x2 int, ci uint8) {
    for ; x1 <= x2; x1++ {
        img.SetColorIndex(x1, y, ci)
    }
}

func vline(img *image.Paletted, x, y1, y2 int, ci uint8) {
    for ; y1 <= y2; y1++ {
        img.SetColorIndex(x, y1, ci)
    }
}

func setBackgroundColor(img *image.Paletted, w, h int, ci uint8) {
    for x := 0; x < w; x++ {
        for y := 0; y < h; y++ {
            img.SetColorIndex(x, y, ci)
        }
    }
}

func drawRectangle(img *image.Paletted, x1, y1, x2, y2 int, ci uint8) {
    hline(img, x1, y1, x2, ci)
    hline(img, x1, y2, x2, ci)
    vline(img, x1, y1, y2, ci)
    vline(img, x2, y1, y2, ci)
}

func main() {
    const nframes = 140
    const delay = 10 // 100ms
    width, height := 500, 500
    anim := gif.GIF{LoopCount: nframes}
    rect := image.Rect(0, 0, width, height)
    for c := uint8(0); c < 7; c++ {
        for f := 0; f < 20; f++ {
            img := image.NewPaletted(rect, palette)
            setBackgroundColor(img, width, height, 7) // black background
            for r := 0; r < 20; r++ {
                ix := c
                if r < f {
                    ix = (ix + 1) % 7
                }
                x := width * (r + 1) / 50
                y := height * (r + 1) / 50
                w := width - x
                h := height - y
                drawRectangle(img, x, y, w, h, ix)
            }
            anim.Delay = append(anim.Delay, delay)
            anim.Image = append(anim.Image, img)
        }
    }
    file, err := os.Create("vibrating.gif")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close() 
    if err2 := gif.EncodeAll(file, &anim); err != nil {
        log.Fatal(err2)
    }   
}
```



## J


```txt

   NB. warning: overwrites /tmp/10[0-3][0-9].jpg
   NB.          and        /tmp/r.gif

   NB. uses imagemagic convert and a browser to display the animation.
   NB. works on linux

   NB. https://rosettacode.org/wiki/Ulam_spiral_(for_primes)#J

   require'jpeg'

   spiral =: ,~ $ [: /: }.@(2 # >:@i.@-) +/\@# <:@+: $ (, -)@(1&,)
   ulamspiral =: *: - spiral

   NB. Corners are the squares of every other odd number.
   NB. Rectangles ulams with first column < and <: second column of CORNERS
   [CORNERS=: *: 1 2 p. i. _20 2
5929 6241
5329 5625
4761 5041
4225 4489
3721 3969
3249 3481
2809 3025
2401 2601
2025 2209
1681 1849
1369 1521
1089 1225
 841  961
 625  729
 441  529
 289  361
 169  225
  81  121
  25   49
   1    9
   

   NB. S is a sufficiently large Ulam spiral matrix
   S=: ulamspiral 81
   
   NB. A are 20 Boolean bitmaps of squares
   A =: CORNERS ((> {.)~ *. (<: {:)~)"1 _ S
   
   NB. B is a bitmap of all the squares
   B =: +/ A

   NB. C is a running sum.  first 6 upper left corners shown
   <"2 ] 6 10 10 {. C =: B +"2 +/\ A
┌───────────────────┬───────────────────┬───────────────────┬───────────────────┬───────────────────┬───────────────────┐
│0 0 0 0 0 0 0 0 0 0│0 0 0 0 0 0 0 0 0 0│0 0 0 0 0 0 0 0 0 0│0 0 0 0 0 0 0 0 0 0│0 0 0 0 0 0 0 0 0 0│0 0 0 0 0 0 0 0 0 0│
│0 2 2 2 2 2 2 2 2 2│0 2 2 2 2 2 2 2 2 2│0 2 2 2 2 2 2 2 2 2│0 2 2 2 2 2 2 2 2 2│0 2 2 2 2 2 2 2 2 2│0 2 2 2 2 2 2 2 2 2│
│0 2 0 0 0 0 0 0 0 0│0 2 0 0 0 0 0 0 0 0│0 2 0 0 0 0 0 0 0 0│0 2 0 0 0 0 0 0 0 0│0 2 0 0 0 0 0 0 0 0│0 2 0 0 0 0 0 0 0 0│
│0 2 0 1 1 1 1 1 1 1│0 2 0 2 2 2 2 2 2 2│0 2 0 2 2 2 2 2 2 2│0 2 0 2 2 2 2 2 2 2│0 2 0 2 2 2 2 2 2 2│0 2 0 2 2 2 2 2 2 2│
│0 2 0 1 0 0 0 0 0 0│0 2 0 2 0 0 0 0 0 0│0 2 0 2 0 0 0 0 0 0│0 2 0 2 0 0 0 0 0 0│0 2 0 2 0 0 0 0 0 0│0 2 0 2 0 0 0 0 0 0│
│0 2 0 1 0 1 1 1 1 1│0 2 0 2 0 1 1 1 1 1│0 2 0 2 0 2 2 2 2 2│0 2 0 2 0 2 2 2 2 2│0 2 0 2 0 2 2 2 2 2│0 2 0 2 0 2 2 2 2 2│
│0 2 0 1 0 1 0 0 0 0│0 2 0 2 0 1 0 0 0 0│0 2 0 2 0 2 0 0 0 0│0 2 0 2 0 2 0 0 0 0│0 2 0 2 0 2 0 0 0 0│0 2 0 2 0 2 0 0 0 0│
│0 2 0 1 0 1 0 1 1 1│0 2 0 2 0 1 0 1 1 1│0 2 0 2 0 2 0 1 1 1│0 2 0 2 0 2 0 2 2 2│0 2 0 2 0 2 0 2 2 2│0 2 0 2 0 2 0 2 2 2│
│0 2 0 1 0 1 0 1 0 0│0 2 0 2 0 1 0 1 0 0│0 2 0 2 0 2 0 1 0 0│0 2 0 2 0 2 0 2 0 0│0 2 0 2 0 2 0 2 0 0│0 2 0 2 0 2 0 2 0 0│
│0 2 0 1 0 1 0 1 0 1│0 2 0 2 0 1 0 1 0 1│0 2 0 2 0 2 0 1 0 1│0 2 0 2 0 2 0 2 0 1│0 2 0 2 0 2 0 2 0 2│0 2 0 2 0 2 0 2 0 2│
└───────────────────┴───────────────────┴───────────────────┴───────────────────┴───────────────────┴───────────────────┘

   NB. D is C catenated to itself with the ones and twos swapped.
   D=: (, ]`(0,2,:1"0)}) C

   NB. E is the 40 matrices with ID, in 3 levels
   E=: (;"2 0 (1000 + i.@#))@:(*&16b301070) D

   NB. overwrite the files then get some help from the shell
   empty@:(writejpeg('.jpg' ,~ '/tmp/' , ":))&>/"1 E
   2!:0'convert -resize 600%  /tmp/10[0-3][0-9].jpg -delay 10 -loop 0 /tmp/r.gif'

   echo 'please view the animation /tmp/r.gif in chrome browser'

```



## Javascript

HTML you'll need for testing

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
        <script src="vibRects.js"></script>
    </head>
    <body onload="start()">
        <div id='wnd'></div>
    </body>
</html>

```


```javascript

const SIZE = 400, WAIT = .025;
class VibRects {
    constructor() {
        this.wait = WAIT;
        this.colorIndex = 0;
        this.dimension = 5;
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
        for(let d = this.dimension; d < SIZE / 2; d += 10) {
            this.draw("#404040", d);
        }
    }
    draw(clr, d) {
        this.ctx.strokeStyle = clr;
        this.ctx.beginPath();
        this.ctx.moveTo(d, d);
        this.ctx.lineTo(SIZE - d, d);
        this.ctx.lineTo(SIZE - d, SIZE - d);
        this.ctx.lineTo(d, SIZE - d);
        this.ctx.closePath();
        this.ctx.stroke();
    }
    update(dt) {
        if((this.wait -= dt) < 0) {
            this.draw(this.colors[this.colorIndex], this.dimension);
            this.wait = WAIT;
            if((this.dimension += 10) > SIZE / 2) {
                this.dimension = 5;
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
    const vibRects = new VibRects();
    vibRects.start();
}

```




## Julia


```julia
using Gtk, Graphics, Colors
 
const height, width, x0, y0 = 480, 640, 320, 240
const can = @GtkCanvas()
const win = GtkWindow(can, "Vibrating Rectangles", width, height)
const colrs = colormap("rdBu")
const sizes = collect(2:4:div(width, 2))
const params = [1, 2]

draw(can) do widget
    ctx = getgc(can)
    set_line_width(ctx, 1)
    c = colrs[params[1]]
    set_source_rgb(ctx, c.r, c.g, c.b)
    i = sizes[params[2]]
    rectangle(ctx, x0 - i, y0 - i, 2i, div(8i, 3))
    stroke(ctx)
end

while true
    params[1] = params[1] % 99 + 1
    params[2] = params[2] % (length(sizes) - 1) + 1
    draw(can)
    show(can)
    sleep(0.25)
end

```



## Objeck

Uses SLD2 libraries and 80's neon colors.

```objeck
use Game.SDL2;
use Game.Framework;

class Vibrating {
  @framework : GameFramework;
  @rec_offset : Int;
  @rec_colors : Color[];
  @rec_color_index : Int;

  function : Main(args : String[]) ~ Nil {
    vibrating := Vibrating->New();
    vibrating->Run();
  }

  New() {
    @framework := GameFramework->New(GameConsts->SCREEN_WIDTH, GameConsts->SCREEN_HEIGHT, "Vibrating Rectangles");
    @framework->SetClearColor(Color->New(0, 0, 0));
    @rec_colors := Color->New[5];
    @rec_colors[0] := Color->New(255, 240, 1); 
    @rec_colors[1] := Color->New(253, 25, 153); 
    @rec_colors[2] := Color->New(153, 252, 32);  
    @rec_colors[3] := Color->New(0, 230, 254);
    @rec_colors[4] := Color->New(161, 14, 236);
  }
  
  method : Run() ~ Nil {
    if(@framework->IsOk()) {
      e := @framework->GetEvent();
      
      frame_count := 0;
      quit := false;
      while(<>quit) {
        @framework->FrameStart();
        
        # process input
        while(e->Poll() <> 0) {
          if(e->GetType() = EventType->SDL_QUIT) {
            quit := true;
          };
        };

        Render(frame_count);

        @framework->FrameEnd();

        frame_count += 1;
        if(frame_count >= @framework->GetFps()) {
          frame_count := 0;
        };
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

  method : Render(frame_count : Int) ~ Nil {
    # rectangle offsets
    if(frame_count % GameConsts->REC_REFRESH = 0) {
      @rec_offset += 1;
      if(@rec_offset >= GameConsts->REC_MAX) {
        @rec_offset := 0;
        @rec_color_index += 1;
      };
    };

    # rectangle colors
    first_color := @rec_colors[@rec_color_index];
    second_color : Color;
    if(@rec_color_index + 1 < @rec_colors->Size()) {
      second_color := @rec_colors[@rec_color_index + 1];
    }
    else {
      second_color := @rec_colors[0];
      @rec_color_index := 0;
    };

    @framework->Clear();

    for(i := 1; i < GameConsts->REC_MAX; i += 1;) {
      if(i < @rec_offset) {
        DrawRectangle(i, first_color);
      }
      else {
        DrawRectangle(i, second_color);
      };
    };

    @framework->Show();
  }

  method : DrawRectangle(step : Int, color : Color) ~ Nil {
    x := step * GameConsts->REC_DIST; w := GameConsts->SCREEN_WIDTH - x * 2;
    y := step * GameConsts->REC_DIST; h := GameConsts->SCREEN_HEIGHT - y * 2;

    renderer := @framework->GetRenderer();
    renderer->SetDrawColor(color->GetR(), color->GetG(), color->GetB(), 0);
    renderer->DrawRect(Rect->New(x, y, w, h));
    renderer->DrawRect(Rect->New(x + 1, y + 1, w - 2, h - 2));
    renderer->DrawRect(Rect->New(x + 2, y + 2, w - 4, h - 4));
  }
}

consts GameConsts {
  SCREEN_WIDTH := 640,
  SCREEN_HEIGHT := 480,
  REC_DIST := 12,
  REC_MAX := 20,
  REC_REFRESH := 15
}

```



## Perl

Using the core module <code>Time::HiRres</code> to get sub-second <tt>sleep</tt>
```perl
use utf8;
binmode STDOUT, ":utf8";
use Time::HiRes qw(sleep);

%r = ('tl' => qw<┌>, 'tr' => qw<┐>, 'h' => qw<─>, 'v' => qw<│>, 'bl' => qw<└>, 'br' => qw<┘>);
@colors = ("\e[1;31m", "\e[1;32m", "\e[1;33m", "\e[1;34m", "\e[1;35m", "\e[1;36m");

print "\e[?25l"; # hide the cursor

$SIG{INT} = sub { print "\e[0H\e[0J\e[?25h"; exit; }; # clean up on exit

while (1) {
    @c = palette() unless $n % 16;
    rect($_, 31-$_) for 0..15;
    display(@vibe);
    sleep .20;
    push @c, $c[0]; shift @c;
    $n++;
}

sub palette {
    my @c = sort { -1 + 2*int(rand 2) } @colors;
    ($c[0], $c[1], $c[2]) x 12;
}

sub rect {
    my ($b, $e) = @_;
    my $c = $c[$b % @c];
    my @bb = ($c.$r{tl}, (($r{h})x($e-$b-1)), $r{tr}."\e[0m");
    my @ee = ($c.$r{bl}, (($r{h})x($e-$b-1)), $r{br}."\e[0m");
    $vibe[$b][$_] = shift @bb for $b .. $e;
    $vibe[$e][$_] = shift @ee for $b .. $e;
    $vibe[$_][$b] = $vibe[$_][$e] = $c.$r{v}."\e[0m" for $b+1 .. $e-1;
}

sub display {
    my(@rect) = @_;
    print "\e[0H\e[0J\n\n";
    for my $row (@rect) {
        print "\t\t\t";
        print $_ // ' ' for @$row;
        print "\n";
    }
}
```



## Perl 6

### ANSI graphics


Ok. The task description is essentially non-existent. In looking at the reference implementation (Ring) it seems like we are supposed to draw a series of concentric rectangles and then alter the colors step-wise. No actual vibration apparent.

Could fire up a GUI but WTH, let's try it at a command line with ANSI.

Draws a series of concentric rectangles then rotates through the color palette. Every three seconds, chooses new random palette colors and reverses rotation direction. 


```perl6
# box drawing characters
my %r = :tl<┌>, :h<─>, :tr<┐>, :v<│>, :bl<└>, :br<┘>;

my @colors = « \e[1;31m \e[1;94m \e[1;33m \e[1;35m \e[1;36m \e[1;32m \e[1;34m »;

# color palette
my @c = flat @colors[0] xx 12, @colors[3] xx 12, @colors[2] xx 12;

print "\e[?25l"; # hide the cursor

signal(SIGINT).tap: {
    print "\e[0H\e[0J\e[?25h"; # clean up on exit
    exit;
}

my $rot = 1;

my @vibe;

loop {
    rect($_, 31-$_) for ^15;
    display @vibe;
    @c.=rotate($rot);
    if ++$ %% 30 {
        @c = |@colors.pick(3);
        @c = sort(flat @c xx 12);
        $rot *= -1;
    }
    sleep .1;
}

sub rect ($b, $e) {
    @vibe[$b;$b..$e] = @c[$b % @c]~%r<tl>, |((%r<h>) xx ($e - $b - 1)), %r<tr>~"\e[0m";
    @vibe[$e;$b..$e] = @c[$b % @c]~%r<bl>, |((%r<h>) xx ($e - $b - 1)), %r
~"\e[0m";
    ($b ^..^ $e).map: { @vibe[$_;$b] = @vibe[$_;$e] = @c[$b % @c]~%r<v>~"\e[0m" }
}

sub display (@rect) {
    print "\e[0H\e[0J\n\n";
    for @rect -> @row {
        print "\t\t\t";
        print $_ // ' ' for @row;
        print "\n";
    }
}
```

See: [https://github.com/thundergnat/rc/blob/master/img/vibrating-rectangles-perl6.gif Vibrating rectangles] (.gif image)


### SDL Animation

Fully animated SDL2 graphics lib version. Will adjust rendered rectangles to fill resized windows. Hit the space bar to toggle palette rotation direction.


```perl6
use SDL2::Raw;

my $width  = 1200;
my $height = 800;

SDL_Init(VIDEO);

my $window = SDL_CreateWindow(
    'Vibrating rectangles',
    SDL_WINDOWPOS_CENTERED_MASK,
    SDL_WINDOWPOS_CENTERED_MASK,
    $width, $height,
    RESIZABLE
);

my $render = SDL_CreateRenderer($window, -1, ACCELERATED +| PRESENTVSYNC);

my $event = SDL_Event.new;

enum KEY_CODES (
    K_SPACE  => 44,
);

my $num = 80;
my @rgb = palette($num);
my ($cx, $cy);
my $dir = 1;

main: loop {
    while SDL_PollEvent($event) {
        my $casted_event = SDL_CastEvent($event);
        given $casted_event {
            when *.type == QUIT { last main }
            when *.type == WINDOWEVENT {
                if .event == 5 {
                    $width  = .data1;
                    $height = .data2;
                }
            }
            when *.type == KEYDOWN {
                if KEY_CODES(.scancode) -> $comm {
                    given $comm {
                        when 'K_SPACE'  { $dir *= -1; }
                    }
                }
                #say .scancode; # unknown key pressed
            }
        }
    }
    ($cx, $cy) = $width div 2, $height div 2;

    for 1..^$num {
        my ($x, $y) = ($cx - ($width/2/$num*$_), $cy - ($height/2/$num*$_))».round;
        my ($w, $h) = ($width/$num*$_, $height/$num*$_)».round;
        SDL_SetRenderDrawColor($render, |@rgb[$_], 255);
        SDL_RenderDrawRect($render, SDL_Rect.new(:x($x), :y($y), :w($w), :h($h)));
    }
    @rgb.=rotate($dir);
    SDL_RenderPresent($render);
    SDL_SetRenderDrawColor($render, 0, 0, 0, 0);
    SDL_RenderClear($render);
}

SDL_Quit();

sub palette ($l) { (^$l).map: { hsv2rgb(($_ * 360/$l % 360)/360, 1, 1).list } };

sub hsv2rgb ( $h, $s, $v ){ # inputs normalized 0-1
    my $c = $v * $s;
    my $x = $c * (1 - abs( (($h*6) % 2) - 1 ) );
    my $m = $v - $c;
    my ($r, $g, $b) = do given $h {
        when   0..^(1/6) { $c, $x, 0 }
        when 1/6..^(1/3) { $x, $c, 0 }
        when 1/3..^(1/2) { 0, $c, $x }
        when 1/2..^(2/3) { 0, $x, $c }
        when 2/3..^(5/6) { $x, 0, $c }
        when 5/6..1      { $c, 0, $x }
    }
    ( $r, $g, $b ).map: ((*+$m) * 255).Int
}
```



## Phix


```Phix
-- demo\rosetta\vibrect.exw
--
--  Draws concentric rectangles in random colours to simulate vibration.
--  Press +/- to increase/decrease the number of rectangles being drawn.
--  Resizing the window, as it turns out, achieves much the same effect 
--  as +/-, only much quicker (by increasing/decreasing the spacing).
--
integer numrects = 125  -- (max non-touching for a height of 500)

include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    integer {w,h} = IupGetIntInt(canvas, "DRAWSIZE")
    atom dw = w/(numrects*2+1),
         dh = h/(numrects*2+1)

    cdCanvasActivate(cddbuffer)
    for i=1 to numrects do
        cdCanvasSetForeground(cddbuffer,rand(#FFFFFF))
        atom wd = i*dw,
             hd = i*dh
        cdCanvasRect(cddbuffer, wd, w-wd, hd, h-hd) 
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

function timer_cb(Ihandle /*ih*/)
    IupUpdate(canvas)
    return IUP_IGNORE
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c='+' or (c='-' and numrects>3) then
        numrects -= c-','
        cdCanvasClear(cddbuffer)
        IupUpdate(canvas)
    end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()
    
    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "602x502") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Vibrating Rectangles")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL) -- release the minimum limitation
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    Ihandle timer = IupTimer(Icallback("timer_cb"), 40)
    IupMainLoop()
    IupClose()
end procedure
main()
```



## Python


```python
import turtle
from itertools import cycle
from time import sleep

def rect(t, x, y):
    x2, y2 = x/2, y/2
    t.setpos(-x2, -y2)
    t.pendown()
    for pos in [(-x2, y2), (x2, y2), (x2, -y2), (-x2, -y2)]: 
        t.goto(pos)
    t.penup()

def rects(t, colour, wait_between_rect=0.1):
    for x in range(550, 0, -25):
        t.color(colour)
        rect(t, x, x*.75)
        sleep(wait_between_rect)

tl=turtle.Turtle()
screen=turtle.Screen()
screen.setup(620,620)
screen.bgcolor('black')
screen.title('Rosetta Code Vibrating Rectangles')
tl.pensize(3)
tl.speed(0)
tl.penup()
tl.ht() 
colours = 'red green blue orange white yellow'.split()
for colour in cycle(colours):
    rects(tl, colour)
    sleep(0.5)

```


Hmm, maybe [https://repl.it/@PaddyMcCarthy/DeafeningDroopyBusiness this]?


## Racket


Via big-bang.


```racket
#lang racket

(require 2htdp/image
         2htdp/universe)

(define N 20)
(define SIZE 400)
(define OFFSET 80)
(define RATE 0.2)

;; a state is a pair of color index and position

(define colors '(red orange yellow green blue indigo violet))
(define (mod x) (modulo x (length colors)))

(big-bang (cons 0 (sub1 N))
  [on-tick
   (match-lambda
     [(cons m 0) (cons (mod (add1 m)) (sub1 N))]
     [(cons m n) (cons m (sub1 n))])
   RATE]
  [to-draw
   (match-lambda
     [(cons m n) 
      (apply
       overlay
       (append
        (for/list ([i (in-range N 0 -1)])
          (square (* i (/ (- SIZE OFFSET) N))
                  'outline
                  (if (> i n)
                      (list-ref colors (mod (add1 m)))
                      (list-ref colors m))))
        (list (empty-scene SIZE SIZE 'black))))])])
```



## Ring


```ring

# Project : Vibrating rectangles

Load "guilib.ring"

color1  = new qcolor() { setrgb( 255,0,0,255 ) }
pen1    = new qpen()   { setcolor(color1)  setwidth(2) }

color2  = new qcolor() { setrgb( 0,255,0,255 ) }
pen2    = new qpen()   { setcolor(color2)  setwidth(2) }

color3  = new qcolor() { setrgb( 0,0,255,255 ) }
pen3    = new qpen()   { setcolor(color3)  setwidth(2) }

penArray = [pen1, pen2, pen3]
penNbr   =  1

New qapp
{
   win1 =  new qwidget()
    {
              setwindowtitle("Drawing using QPixMap")
              setgeometry(100,100,500,500)
              label1 = new qlabel(win1)
              {
                           setgeometry(10,10,500,500)
                           settext("")
              }       
              Canvas = new qlabel(win1)
             {              
                            MonaLisa = new qPixMap2( 500,500)  
                            color = new qcolor(){ setrgb(255,0,0,255) }

                           daVinci = new qpainter() 
                           {
                                         begin(MonaLisa)               
                           }            
             setpixmap(MonaLisa)         
             }       
	nCounter = 0
	oTimer = new qTimer(win1) {
		setinterval(500)
		settimeoutevent("DrawCounter()")
		start()
	} 
       show()      
    }
   exec()
}
DrawCounter()

func DrawCounter()
        nCounter++
        if nCounter < 15
           Draw(penArray[penNbr])
        elseif nCounter % 15 = 0
            nCounter = 0
            penNbr++
            if penNbr > 3
               penNbr = 1
            ok       
            Draw(penArray[penNbr])
        ok
return
 
Func Draw(pen1)
         daVinci.setpen(penArray[penNbr])
         daVinci.drawrect(50+nCounter*10, 50+nCounter*10, 300-nCounter*20, 300-nCounter*20)
         Canvas.setpixmap(MonaLisa)      
         win1.show()                     
return

```

Output:

[https://1drv.ms/v/s!AqDUIunCqVnIg1MxKPi5DzwUbJEf Vibrating rectangles]
