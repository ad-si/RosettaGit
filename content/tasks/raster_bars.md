+++
title = "Raster bars"
description = ""
date = 2019-06-11T17:27:50Z
aliases = []
[extra]
id = 22356
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "perl_6",
]
+++

Display bars of color, moving up and down across the screen for horizontal raster bars, or left and right across the screen for vertical raster bars, making the raster bars move as stated by swaying side-to-side the respective way if possible. Use multiple colors in succession and carefully make a gradient between the colors if possible to achieve an effect of metallic-looking horizontal bars. Horizontal and/or vertical raster bars are accepted.


## Go

This uses Go's 'image' packages in its standard library to create an animated GIF.

After each 20 frames, it switches between horizontal and vertical bars and repeats the whole process 10 times. 

WARNING: If you have problems with flashing images, either switch to a longer delay than 150ms between 'sways' or don't run it at all.

Although the .gif works fine in Firefox it might not do so in EOG due to optimizations made during its creation. If so, then the following ImageMagick command should fix it:

```txt

  $ convert raster_bars.gif -coalesce raster_bars2.gif
  $ eog raster_bars2.gif

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
    c0 = color.RGBA{166, 124, 0, 255}
    c1 = color.RGBA{191, 155, 48, 255}
    c2 = color.RGBA{255, 191, 0, 255}
    c3 = color.RGBA{255, 207, 64, 255}
    c4 = color.RGBA{255, 220, 115, 255}
)

var palette = []color.Color{c0, c1, c2, c3, c4}

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

func drawHbar(img *image.Paletted, x1, y1, x2, y2 int, ci uint8) {
    for ; y1 <= y2; y1++ {
        hline(img, x1, y1, x2, ci)
    }
}

func drawVbar(img *image.Paletted, x1, y1, x2, y2 int, ci uint8) {
    for ; x1 <= x2; x1++ {
        vline(img, x1, y1, y2, ci)
    }
}

func main() {
    const nframes = 40
    const delay = 15 // 150ms
    width, height := 500, 500
    anim := gif.GIF{LoopCount: 9} // repeats 9 + 1 times
    rect := image.Rect(0, 0, width, height)

    for f := 0; f < nframes; f++ {
        img := image.NewPaletted(rect, palette)
        c := uint8(f % 2)
        if f < nframes/2 {
            for y := 0; y < height; y += 20 {
                drawHbar(img, 0, y, width, y+19, c)
                c++
                if c == 5 {
                    c = 0
                }
            }
        } else {
            for x := 0; x < width; x += 20 {
                drawVbar(img, x, 0, x+19, height, c)
                c++
                if c == 5 {
                    c = 0
                }
            }
        }
        anim.Delay = append(anim.Delay, delay)
        anim.Image = append(anim.Image, img)
    }

    file, err := os.Create("raster_bars.gif")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()
    if err2 := gif.EncodeAll(file, &anim); err != nil {
        log.Fatal(err2)
    }
}
```



## Perl 6

As there is no reference implementation, and rather sketchy task instructions, this may or may not fulfill the original task authors intent. 

Not really sure what is meant by "swaying".

Generate random colored bars and display them. (They ended up looking more plastic than metallic; ah well.)

* Use Up / Down to change the scroll speed.
* Use Pg Up / Pg Dn to adjust raster bar height.
* Use Left / Right to adjust the gap between the raster bars.
* Use Space bar to pause / resume scrolling.
* Use Left Ctrl to reverse scroll direction.
* Use Z / X to change the angle of the raster bars.
* Press Q to exit.


```perl6
use SDL2::Raw;
use Cairo;

my $width  = 800;
my $height = 800;

SDL_Init(VIDEO);

my $window = SDL_CreateWindow(
    'Raster Bars - Perl 6',
    SDL_WINDOWPOS_CENTERED_MASK,
    SDL_WINDOWPOS_CENTERED_MASK,
    $width, $height, RESIZABLE
);

my $render = SDL_CreateRenderer($window, -1, ACCELERATED +| PRESENTVSYNC);

my @bars = (^256).map: { gen-bar( rand xx 3 ) };

my $event = SDL_Event.new;

enum KEY_CODES (
    K_UP     => 82,
    K_DOWN   => 81,
    K_LEFT   => 80,
    K_RIGHT  => 79,
    K_SPACE  => 44,
    K_PGUP   => 75,
    K_PGDN   => 78,
    K_LCTRL  => 224,
    K_Z      => 29,
    K_X      => 27,
    K_Q      => 20,
);

my $dir   = -1;
my $step  = 5;
my $incr  = 1;
my $gap   = 100;
my $bh    = 64;
$height  += 32;
my $port  = +@bars * $gap;
my $y     = $dir > 0 ?? $height - $port !! $height;
my $angle = 0;

main: loop {
    handle-event($event) while SDL_PollEvent($event);

    $y = $step * $dir + $y;

    if $dir > 0 {
        $y = $height - $port if $y > 0
    } else {
        $y = 0 if $y < $height - $port
    }

    for ^@bars {
        my $offset = $gap / cos(π * $angle / 180);
        SDL_RenderCopyEx( $render, @bars[$_], Nil,
          SDL_Rect.new( -$gap, $y + $offset * $_, $width * 2, $bh),
          $angle.Num, SDL_Point.new(:x(0),:y(0)), 0
        )
    }

    SDL_RenderPresent($render);

    SDL_RenderClear($render);
}

SDL_Quit();

sub gen-bar (@color) {
    my $bar = Cairo::Image.create( Cairo::FORMAT_ARGB32, 1, 32 );
    given Cairo::Context.new($bar) {
        my Cairo::Pattern::Gradient::Linear $lpat .= create(0.0, 0.0,  0.0, 32.0);
        $lpat.add_color_stop_rgba(  1, |(@color »*» .3), 1);
        $lpat.add_color_stop_rgba( .2, |(@color),        1);
        $lpat.add_color_stop_rgba( .8, |(@color),        1);
        $lpat.add_color_stop_rgba(  0, |(@color »+» .8), 1);
        .rectangle(0, 0, 1, 32);
        .pattern($lpat);
        .fill;
        $lpat.destroy;
    }

    my $bar_texture = SDL_CreateTexture(
        $render, %PIXELFORMAT<ARGB8888>,
        STATIC, 1, 32
    );

    SDL_UpdateTexture(
        $bar_texture,
        SDL_Rect.new(:x(0), :y(0), :w(1), :h(32)),
        $bar.data, $bar.stride // 1
    );

    $bar_texture
}

sub handle-event ($event) {
    my $casted_event = SDL_CastEvent($event);
    given $casted_event {
        when *.type == QUIT    { last main }
        when *.type == KEYDOWN {
            if KEY_CODES(.scancode) -> $comm {
                given $comm {
                    when 'K_UP'     { $step += $incr }
                    when 'K_DOWN'   { $step -= $incr if $step > $incr }
                    when 'K_LEFT'   { $gap = $gap < 32 ?? $gap !! $gap - 1; $port = +@bars * $gap; }
                    when 'K_RIGHT'  { $gap++; $port += +@bars; }
                    when 'K_PGUP'   { $bh += 2 }
                    when 'K_PGDN'   { $bh = $bh >= 34 ?? $bh - 2!! $bh }
                    when 'K_SPACE'  { $step = $step ?? 0 !! $incr }
                    when 'K_LCTRL'  { $dir  *= -1 }
                    when 'K_Z'      { $angle = $angle > -45 ?? $angle - 1 !! $angle }
                    when 'K_X'      { $angle = $angle <  45 ?? $angle + 1 !! $angle }
                    when 'K_Q'      { last main }
                }
            }
        }
        when *.type == WINDOWEVENT {
            if .event == 5 {
                $width  = .data1;
                $height = .data2 + 32;
            }
        }
    }
}
```


Screenshot still of typical run: [https://github.com/thundergnat/rc/blob/master/img/Raster-bars-Perl6.png (offsite .png image)]
