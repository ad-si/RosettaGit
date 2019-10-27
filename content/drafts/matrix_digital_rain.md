+++
title = "Matrix Digital Rain"
description = ""
date = 2019-10-20T18:30:38Z
aliases = []
[extra]
id = 22107
[taxonomies]
categories = []
tags = []
+++

{{task}}

Implement the Matrix Digital Rain visual effect from the movie "The Matrix" as described in [[wp:Matrix_digital_rain|Wikipedia]].

Provided is a reference implementation in Common Lisp to be run in a terminal.





## Batch File

Adding more '''%RANDOM%''' will increase the number of columns.

```dos

@echo off
cls
color a
:start
echo %RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%
goto start

```




## Common Lisp


{{works with|SBCL}}

Runs in the terminal (using the Ncurses C library and the croatoan Lisp wrapper).


```lisp

(defun matrix-digital-rain ()
  (with-screen (scr :input-echoing nil :input-blocking nil :cursor-visible nil)
    (let* ((width (width scr))
           (height (height scr))
           ;; start at a random height in each column.
           (positions (loop repeat width collect (random height)))
           ;; run each column at a random speed.
           (speeds (loop repeat width collect (random 4))))
      ;; generate a random ascii char
      (flet ((randch () (+ 64 (random 58))))
        ;; hit the q key to exit the main loop.
        (bind scr #\q 'exit-event-loop)
        (bind scr nil
          (lambda (win event)
            (loop for col from 0 to (1- width) do
              (loop repeat (nth col speeds) do
                ;; position of the first point in the current column
                (let ((pos (nth col positions)))
                  (setf (attributes win) '(:bold))
                  (setf (fgcolor win) :green)
                  (add win (randch) :y (mod pos height) :x col :fgcolor :white)
                  (add win (randch) :y (mod (- pos 1) height) :x col)
                  (add win (randch) :y (mod (- pos 2) height) :x col)
                  (setf (attributes win) '())
                  (add win (randch) :y (mod (- pos 3) height) :x col)
                  ;; overwrite the last char half the height from the first char.
                  (add win #\space  :y (mod (- pos (floor height 2)) height) :x col)
                  (refresh win)
                  ;; advance the current column
                  (setf (nth col positions) (mod (+ pos 1) height))))))))
      (setf (frame-rate scr) 20)
      (run-event-loop scr))))

```


{{out|Sample output}}
[https://i.imgur.com/17b36O3.png https://i.imgur.com/17b36O3.png]


## Go

{{libheader|goncurses}}
This is a translation of the C code [https://github.com/ruscoe/Digital-Rain/blob/master/drain.c here] which uses the ncurses library.

Rather than pressing Ctrl+C to stop the program, I've added code so that it stops automatically after 1 minute and restores the terminal to its original state.

```go
package main

import (
    gc "github.com/rthornton128/goncurses"
    "log"
    "math/rand"
    "time"
)

// Time between row updates in microseconds.
// Controls the speed of the digital rain effect.
const rowDelay = 40000

func main() {
    start := time.Now()
    rand.Seed(time.Now().UnixNano())

    // Characters to randomly appear in the rain sequence.
    chars := []byte("0123456789")
    totalChars := len(chars)

    // Set up ncurses screen and colors.
    stdscr, err := gc.Init()
    if err != nil {
        log.Fatal("init", err)
    }
    defer gc.End()

    gc.Echo(false)
    gc.Cursor(0)

    if !gc.HasColors() {
        log.Fatal("Program requires a colour capable terminal")
    }

    if err := gc.StartColor(); err != nil {
        log.Fatal(err)
    }

    if err := gc.InitPair(1, gc.C_GREEN, gc.C_BLACK); err != nil {
        log.Fatal("InitPair failed: ", err)
    }
    stdscr.ColorOn(1)
    maxY, maxX := stdscr.MaxYX()

    /* Create slices of columns based on screen width. */

    // Slice containing the current row of each column.
    columnsRow := make([]int, maxX)

    // Slice containing the active status of each column.
    // A column draws characters on a row when active.
    columnsActive := make([]int, maxX)

    // Set top row as current row for all columns.
    for i := 0; i < maxX; i++ {
        columnsRow[i] = -1
        columnsActive[i] = 0
    }

    for {
        for i := 0; i < maxX; i++ {
            if columnsRow[i] == -1 {
                // If a column is at the top row, pick a
                // random starting row and active status.
                columnsRow[i] = rand.Intn(maxY + 1)
                columnsActive[i] = rand.Intn(2)
            }
        }

        // Loop through columns and draw characters on rows.
        for i := 0; i < maxX; i++ {
            if columnsActive[i] == 1 {
                // Draw a random character at this column's current row.
                charIndex := rand.Intn(totalChars)
                stdscr.MovePrintf(columnsRow[i], i, "%c", chars[charIndex])
            } else {
                // Draw an empty character if the column is inactive.
                stdscr.MovePrintf(columnsRow[i], i, "%c", ' ')
            }

            columnsRow[i]++

            // When a column reaches the bottom row, reset to top.
            if columnsRow[i] >= maxY {
                columnsRow[i] = -1
            }

            // Randomly alternate the column's active status.
            if rand.Intn(1001) == 0 {
                if columnsActive[i] == 0 {
                    columnsActive[i] = 1
                } else {
                    columnsActive[i] = 0
                }
            }
        }
        time.Sleep(rowDelay * time.Microsecond)
        stdscr.Refresh()
        elapsed := time.Since(start)
        // Stop after 1 minute.
        if elapsed.Minutes() >= 1 {
            break
        }
    }
}
```



## Perl

Probably shouldn't, but until someone posts something better... here's something somewhat relevant [https://www.perlmonks.org/?node_id=536000 I wrote back in 2006].

Follow the bouncing Neo!


```perl
#!/user/bin/perl

use strict;
use warnings;
use Tk;

my $delay      = 50;                            # milliseconds
my $fade       = 8;                             # number of characters to "fade"
my $base_color = '#004000';                     # dark green
my $fontname   = 'Times';                       # Whatever
my $fontsize   = 12;                            # point size
my $font       = "{$fontname} $fontsize bold";
my @objects;

my ( $xv, $yv ) = ( 0, 0 );

my $top = MainWindow->new();

$top->geometry('800x600');

my $run = 1;

$top->protocol( 'WM_DELETE_WINDOW' => sub { $run = 0; } );

my @letters = ( 'A' .. 'Z', 'a' .. 'z', '0' .. '9' );

my $canvas = $top->Canvas(
    -background => 'black'
)->pack(
    -fill   => 'both',
    -expand => 'y'
);

my $testch = $canvas->createText(
    100, 100,
    -text => 'o',
    -fill => 'black',
    -font => $font
);

$top->update;

my @coords = $canvas->bbox($testch);

$canvas->delete($testch);

my $lwidth  = $coords[2] - $coords[0];
my $lheight = ( $coords[3] - $coords[1] ) * .8;
my $cols    = int $canvas->width / $lwidth;
my $rows    = int $canvas->height / $lheight;

for my $y ( 0 .. $rows ) {
    for my $x ( 0 .. $cols ) {
        $objects[$x][$y] = $canvas->createText(
            $x * $lwidth, $y * $lheight,
            -text => $letters[ int rand @letters ],
            -fill => $base_color,
            -font => $font
        );
    }
}

my $neo_image = $top->Photo( -data => neo() );

my $neo = $canvas->createImage(
    $canvas->width / 2,
    $canvas->height / 2,
    -image => $neo_image
);

while ($run) {
    drop('Nothing Like The Matrix');
}
exit;

MainLoop;

sub drop {
    my @phrase = split //, reverse shift;
    my $x = int rand $cols;
    my @orig;
    for my $y ( 0 .. $rows ) {
        $orig[$y] = $canvas->itemcget( $objects[$x][$y], '-text' );
    }
    for my $y ( 0 .. $rows + @phrase + $fade ) {
        for my $letter ( 0 .. @phrase ) {
            last if ( $y - $letter < 0 );
            $canvas->itemconfigure(
                $objects[$x][ $y - $letter ],
                -text => $phrase[$letter],
                -fill => "#00FF00"
            );
        }
        if ( $y > @phrase ) {
            $canvas->itemconfigure(
                $objects[$x][ $y - @phrase ],
                -text => $orig[ $y - @phrase ],
                -fill => "#009000"
            );
        }
        if ( $y > @phrase + 2 ) {
            $canvas->itemconfigure( $objects[$x][ $y - @phrase - int ($fade / 2) ],
                -fill => "#006000" );
            $canvas->itemconfigure( $objects[$x][ $y - @phrase - $fade + 1 ],
                -fill => $base_color );
        }
        last unless $run;
        $top->after($delay);
        neo_move();
        $top->update;
    }
}

sub neo_move {
    $xv += ( ( rand 2 ) - 1 > 0 ) ? 1 : -1;
    $yv += ( ( rand 2 ) - 1 > 0 ) ? 1 : -1;
    my ( $x, $y ) = $canvas->coords($neo);
    $xv = -$xv if ( ( $x < 0 ) or ( $x > $canvas->width ) );
    $yv = -$yv if ( ( $y < 0 ) or ( $y > $canvas->height ) );
    $canvas->move( $neo, $xv, $yv );
}

sub neo {
return '
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
8SLGjBo3cuzo8SPIkCJHkixp8iTKiQEBADs=
';
}

```



## Perl 6

{{works with|Rakudo|2018.11}}
Kind-of cheap and cheesy, but what the heck... Probably will only work in a POSIX compatible terminal. Runs until you hit ^C to exit.

The "lightning" effect is actually a bug, but I liked it so I kept it.


```perl6
# clean up on exit, reset ANSI codes, scroll, re-show the cursor & clear screen
signal(SIGINT).tap: { print "\e[0m", "\n" xx 50, "\e[H\e[J\e[?25h"; exit(0) }

# a list of glyphs to use
my @codes = flat 'Α' .. 'Π', 'Ѐ' .. 'ѵ', 'Ҋ' .. 'ԯ', 'Ϣ' .. 'ϯ', 'ｦ'.. 'ﾝ',
                 'Ⲁ' .. '⳩', '∀' .. '∗', '℀' .. '℺', '⨀' .. '⫿';

# palette of gradient ANSI foreground colors
my @palette = flat  "\e[38;2;255;255;255m", (255,245 … 30).map({"\e[38;2;0;$_;0m"}),
              "\e[38;2;0;25;0m" xx 75;

my @screen; # buffer to hold glyphs
my @rotate; # palette rotation position buffer

my ($rows, $cols) = qx/stty size/.words; # get the terminal size
init($rows, $cols); # set up the screen buffer and palette offsets

my $size-check;

print "\e[?25l\e[48;5;232m"; # hide the cursor, set the background color

loop {
     if ++$size-check %% 20 {                         # periodically check for
         my ($r, $c) = qx/stty size/.words;           # resized terminal and
         init($r, $c) if $r != $rows or $c != $cols;  # re-initialize screen buffer
         $size-check = 0
     }
     print "\e[1;1H";                                 # set cursor to top left
     print join '', (^@screen).map: {
         @rotate[$_] = (@rotate[$_] + 1) % +@palette; # rotate the palettes
         flat @palette[@rotate[$_]], @screen[$_]      # and print foreground, glyph
     }
     @screen[(^@screen).pick] = @codes.roll for ^30;  # replace some random glyphs
}

sub init ($r, $c) {
    @screen = @codes.roll($r * $c);
    ($rows, $cols) = $r, $c;
    my @offset = (^@palette).pick xx $cols;
    for ^$rows -> $row {
        @rotate[$row * $cols ..^ $row * $cols + $cols] = @offset;
        # for no "lightning" effect, add   '1 + '  ↓ here: (1 + $_ % 3)
        @offset = (^@offset).map: {(@offset[$_] - ($_ % 3)) % +@palette};
    }
}
```


{{out|Sample output}}
See [https://github.com/thundergnat/rc/blob/master/img/matrix-digital-rain-perl6.png matrix-digital-rain-perl6.png] (offsite png image)


## Racket


{{trans|Perl 6}}


```racket
#lang racket

(define codes '((Α Π) (Ѐ ѵ) (Ҋ ԯ) (Ϣ ϯ) (ｦ ﾝ) (Ⲁ ⳩) (∀ ∗) (℀ ℺) (⨀ ⫿)))

(define (symbol->integer s) (char->integer (string-ref (symbol->string s) 0)))
(define (pick xs) (list-ref xs (random (length xs))))

(define glyphs
  (map
   integer->char
   (append*
    (for/list ([c (in-list codes)])
      (range (symbol->integer (first c)) (add1 (symbol->integer (second c))))))))

(define palette (vector-append (vector "\e[38;2;255;255;255m")
                               (for/vector ([n (in-range 245 29 -10)])
                                 (format "\e[38;2;0;~a;0m" n))
                               (make-vector 75 "\e[38;2;0;25;0m")))

(match-define (list (app (compose1 sub1 string->number) rows)
                    (app string->number cols))
  (string-split (with-output-to-string (thunk (system "stty size")))))

(define screen (for/vector ([_ (in-range (* rows cols))]) (pick glyphs)))
(define offsets (for/vector ([col cols]) (random (vector-length palette))))

(display "\e[?25l\e[48;5;232m") ; hide the cursor, set the background color

(define (main)
  (for ([iter (in-naturals)])
    (sleep 0.1)
    (display "\e[1;1H") ; reset cursor to top left
    (for ([i (in-range 30)]) (vector-set! screen (random (* rows cols)) (pick glyphs)))
    (for ([i (in-range rows)])
      (for ([j (in-range cols)])
        (display (vector-ref palette (modulo (+ (- i) iter (vector-ref offsets j))
                                             (vector-length palette))))
        (display (vector-ref screen (+ (* cols i) j))))
      (display "\n"))))

(with-handlers ([exn:break? (thunk*
                             ; reset ANSI codes, reshow cursor, clear screen
                             (display "\e[0m")
                             (display "\e[H\e[J\e[?25h"))])
  (main))
```



## REXX


```rexx
/*REXX program creates/displays Matrix (the movie) digital rain; favors non-Latin chars.*/
signal on halt                                   /*allow the user to halt/stop this pgm.*/
parse arg pc seed .                              /*obtain optional arguments from the CL*/
if pc=='' | pc==","     then pc= 20              /*Not specified?  Then use the default.*/
if datatype(seed, 'W')  then call random ,,seed  /*Numeric?  Use seed for repeatability.*/
parse value  scrsize()  with  sd  sw  .          /*obtain the dimensions of the screen. */
if sd==0  then sd= 54;  sd= sd - 2               /*Not defined? Then use default; adjust*/
if sw==0  then sw= 80;  sw= sw - 1               /* "      "      "   "     "        "  */
lowC= c2d(' ')  +  1                             /*don't use any characters  ≤  a blank.*/
@.= ' '                                          /*PC  is the % new Matric rain streams.*/
cloud= copies(@., sw)                            /*the cloud, where matrix rain is born.*/
cls= 'CLS'                                       /*DOS command used to clear the screen.*/
                  do  forever;   call nimbus     /*define bottom of cloud  (the drops). */
                                 call rain       /*generate rain, display the raindrops.*/
                  end   /*j*/
halt:  exit                                      /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
rain:   do a=sd  by -1  for sd-1;   _= a-1;   @.a= @._;   end;      call fogger;    return
show:   cls;  @.1= cloud;    do r=1  for sd;  say strip(@.r, 'T');  end  /*r*/;     return
/*──────────────────────────────────────────────────────────────────────────────────────*/
nimbus: if random(0, 100)<pc  then call mist     /*should this be a new rain stream ?   */
                              else call unmist   /*should any of the rain streams cease?*/
        return
        if random(0, 100)<pc        then return  /*should this be a new rain stream ?   */
        ?= random(1, sw)                         /*pick a random rain cloud position.   */
        if substr(cloud,?,1)\==' '  then return  /*This cloud position not blank? Return*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
mist:   ?= random(1, sw)                         /*obtain a random column in cloud.     */
        if substr(cloud,?,1)\==' '  then return  /*if this stream is active, return.    */
        if random(0, 100)<pc        then return  /*should this be a new rain stream ?   */
        cloud= overlay(drop(), cloud, ?)         /*seed cloud with new matrix rain drop.*/
        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
unmist: ?= random(1, sw)                         /*obtain a random column in cloud.     */
        if substr(cloud,?,1) ==' '  then return  /*if this stream is dry,  return.      */
        if random(0, 100)>pc        then return  /*should this be a new dry stream ?    */
        cloud= overlay(' ',    cloud, ?)         /*seed cloud with new matrix rain drop.*/
        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
drop:   Lat= random(1, 4)                        /*Now, chose a matrix rain stream char.*/
        tChr= 254;  if Lat==1  then tChr= 127    /*choose the  type of rain stream char.*/
        return d2c( random(lowC, tChr) )         /*Lat = 1?   This favors Latin letters.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
fogger: do f=1  for sw                           /*display a screen full of rain streams*/
        if substr(cloud, f, 1) \== ' '   then cloud= overlay( drop(), cloud, f)
        end   /*f*/;        call show;   return  /* [↑]  if raindrop, then change drop. */
```

Programming note:

This REXX program makes use of   '''SCRSIZE'''   REXX program (or
BIF) which is used to determine the screen

width and depth of the terminal (console).    Some REXXes don't
have this BIF.

The   '''SCRSIZE.REX'''   REXX program is included
here   ───►   [[SCRSIZE.REX]].


{{out|output|text=  when using the default input:}}

(A screen snapshot shown at half size.)

<pre style="font-size:50%">
              ░      O       3              k       ò         cº     Θ     &           F
              ~      Ö       ¡              )       Å         tπ     ÷     ç           ╛
              a      ±       █              q       L         P}     8     ╕           y
              Ü      d       |              Γ       1         P»     ≥     ╙           ù
              ╖      ¥       9              ╠       ▒         SÉ     k     ╟           }
              o      U       '              Æ       í         (N     E     C           0
              e      2       ╜              ─       ₧         bH     ï     Z           ╢
              o      å       -              S       2         ò┘     ║     P           ╖
              s      @       ê              ≈       ╝         ·v     F     4           û
              Æ      k       ┤              .ÿ      s         ╕@     b     U           ^
              9      ╩       «              ┐·      w         ╛-     e     U           =
              ≈      a       ┬              δu      ┬         ît     ╤     ╖           σ
              N      Ä       F              'N      @         ╒R     φ     O
              ô      @       Y              ÷x      X         ∩P     ¡     8
              9      ║       ╔              #■      ▌         d%     v     «
              ô      m       I              xm      :         Um     E     &
              ╚      »       r              >       ~         H@     /     ┤
              ⌂      k       E              1       ╜         Wc     J     s
              8      g       E              m       ƒ         Ωε     q     U
              V      ë       &              |       ï         ■~     ≥     X
              à      Ö       .              î       3         ₧=     ⌂     ±
              π      ì       è              n       ┬         ·▌     Z     W
              ¢      6       n              S       £         [x     £     {
              '      &       ?              ╖       )         jÑ     ╞     α
              î      ╔       x              F       ╨         ☺▌     7     ┘
              á      ë       j              *       -         rⁿ     ú     )
              '      S       ╥              ∙       E         v≡     o     ú
              .      <       V              ²       °         ]G     7     H
              p      8       ╣              o       ╤         FÇ     S      
              ┴      t       ö              ^       ╣         n╟     ╕     >
              *      æ       ò              ╡       4         d_     ├     «
              ╔      â       -              Q       P         un     ≥     M
              6      ²       ╢              ╔       E         ≤A     °     *
              /      :       q              L       R         cë     %     ≡
              3      x       $              ⌡       ¥         .Ω     E     :
              E      ┤       y              i       U         Z      ú     O
              ⌂      Σ                      .       ¥         }      Ä     °
              V      φ                              ≤         ╙      ╫     j
              }      à                      ?       3         '      ≤     .
              í      ▐                      ╞       =         ~      Ä     ç
              B      î                      ö                 (      ¬     ╘
              ┬      [                      6                 ═      ╡     ▌
              r      %                      f                 ╔      [     C
         _    {      è                      %                 ╫      ▒     Z
         1    ≈      ]                      !                 -      ╨     o        ¢
              ?      ÿ                      9                 Å      e     ß        σ
         µ    ■      α                      Z                 h      4     ⌠        4
         ñ    ╙      2                      8                 ;      2     ╣        !
         J    Ñ      :                      ~                 █      ª     ░        I
         ╥    í      ┴                      c                 !      U     ;        [
         Ω    Ç      ╫                      N                 W      ⌂     $        ╨
         ì    +      ₧                      _                 v      k     ò        X
         c    s      ¿                      3                 %      ½     F        ╠
         x    Γ      p                      ?                 £      S     ç        -
         g           /                      ╡                 :      é     æ        ^
         √           ⌐                      0                 Ü      .     9        Γ
         ⌡           ╕                      w                 0      î     ╖        l
         ║           ñ                      ½                 █      D     ≥        Θ
         6           F                      ù                 ;      O     %        à
         m           -                      4                 n      '     ╬        {
         ├           ╚                      K                 &      G     R        ╗

```



## Yabasic


```Yabasic
open window 640,512,"swiss12"
backcolor 0,0,0
clear window
mx=50
my=42
dim scr(mx,my)
for y=0 to my
    for x=0 to mx
        scr(x,y)=int(ran(96)+33)
    next x
next y
ms=50
dim sx(ms)
dim sy(ms)
for a=1 to ms
    sx(a)=int(ran(mx))
    sy(a)=int(ran(my))
next a
do
    for s=1 to ms
        x=sx(s)
        y=sy(s)
        
        letter(0,255,0)
        y=y-1
        
        letter(0,200,0)
        y=y-1
        
        letter(0,150,0)
        y=y-1
        
        color 0,0,0
        fill rect x*12.8-1,y*12.8+4 to x*12.8+12,y*12.8-10
        letter(0,70,0)
        y=y-24
        
        color 0,0,0
        fill rect x*12.8-1,y*12.8+4 to x*12.8+12,y*12.8-10
    next s
    for s=1 to ms
        if int(ran(5)+1)=1 sy(s)=sy(s)+1
        if sy(s)>my+25 then
            sy(s)=0
            sx(s)=int(ran(mx))
        end if
    next s
loop

sub letter(r,g,b)
    if y<0 or y>my return
    c=scr(x,y)
    color r,g,b
    text x*12.8,y*12.8,chr$(c)
end sub
```



## zkl

{{trans|Perl6}}

```zkl
var [const] codes=Walker.chain(  // a bunch of UTF non ascii chars
         [0x0391..0x03a0], [0x03a3..0x0475], [0x0400..0x0475],
         [0x048a..0x052f], [0x03e2..0x03ef], [0x2c80..0x2ce9],
         [0x2200..0x2217], [0x2100..0x213a], [0x2a00..0x2aff])
	 .apply(fcn(utf){ utf.toString(-8) }),  // jeez this is lame
    codeSz=codes.len(),	// 970
    c=L("\e[38;2;255;255;255m",[255..30,-15].apply("\e[38;2;0;%d;0m".fmt),
        (250).pump(List,T(Void,"\e[38;2;0;25;0m"))).flatten(),
    csz=c.len(); // 267, c is ANSI escape code fg colors: 38;2;<r;g;b>m
 
// query the ANSI terminal
rows,cols := System.popen("stty size","r").readln().split().apply("toInt");

o,s,fg := buildScreen(rows,cols);
ssz:=s.len();

print("\e[?25l\e[48;5;232m");  // hide the cursor, set background color to dark
while(1){		       // ignore screen resizes
   print("\e[1;1H");	       // move cursor to 1,1
   foreach n in (ssz){	       // print a screen full
      print( c[fg[n]], s[n] ); // forground color, character
      fg[n]=(fg[n] + 1)%csz;   // fade to black
   }
   do(100){ s[(0).random(ssz)]=codes[(0).random(codeSz)] }  // some new chars
   Atomic.sleep(0.1);	       // frame rate for my system, up to 200x41 terminal
}

fcn buildScreen(rows,cols){    // build a row major array as list
   // s --> screen full of characters
   s:=(rows*cols).pump(List(), fcn{ codes[(0).random(codeSz)]});
   // array fb-->( fg color, fg ..) where fg is an ANSI term 48;5;<n>m color
   fg:=List.createLong(s.len(),0);
   o:=csz.pump(List()).shuffle()[0,cols];  // cols random #s
   foreach row in (rows){		   // set fg indices
      foreach col in (cols){ fg[row*cols + col] = o[col] }
      o=o.apply(fcn(n){ n-=1; if(n<0) n=csz-1; n%csz });  // fade out
   }
   return(o,s,fg);
}
```

Offsite Image: [http://www.zenkinetic.com/Images/RosettaCode/matrixRainDance.jpg Matrix rain dance]

[[Category:Ncurses]] 
[[Category:Curses]]
