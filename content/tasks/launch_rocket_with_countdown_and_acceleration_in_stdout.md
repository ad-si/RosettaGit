+++
title = "Launch rocket with countdown and acceleration in stdout"
description = ""
date = 2019-10-16T12:40:34Z
aliases = []
[extra]
id = 22451
[taxonomies]
categories = ["task"]
tags = []
+++

;Task Description

The task is to simulate the countdown of a rocket launch from 5 down to 0 seconds and then display the moving, accelerating rocket on the standard output device as a simple ASCII art animation.





## Go

...though my rocket is a bit fancier :)

```go
package main

import (
    "fmt"
    "time"
)

const rocket = `
    /\
   (  )
   (  )
  /|/\|\
 /_||||_\
`

func printRocket(above int) {
    fmt.Print(rocket)
    for i := 1; i <= above; i++ {
        fmt.Println("    ||")
    }
}

func cls() {
    fmt.Print("\x1B[2J")
}

func main() {
    // counting
    for n := 5; n >= 1; n-- {
        cls()
        fmt.Printf("%d =>\n", n)
        printRocket(0)
        time.Sleep(time.Second)
    }

    // ignition
    cls()
    fmt.Println("Liftoff !")
    printRocket(1)
    time.Sleep(time.Second)

    // liftoff
    ms := time.Duration(1000)
    for n := 2; n < 100; n++ {
        cls()
        printRocket(n)
        time.Sleep(ms * time.Millisecond)
        if ms >= 40 {
            ms -= 40
        } else {
            ms = 0
        }
    }
}
```



## Julia


```julia
rocket() = println("        /..\\\n        |==|\n        |  |\n        |  |\n",
    "        |  |\n       /____\\\n       |    |\n       |SATU|\n       |    |\n",
    "       |    |\n      /| |  |\\\n     / | |  | \\\n    /__|_|__|__\\\n       /_\\/_\\\n")

exhaust() = println("       *****")
cls() = print("\x1B[2J")
curup(n) = print("\e[$(n)A")
curdown(n) = print("\e[$(n)B")

function countdown(secs)
    print("Countdown...T minus ")
    for i in secs:-1:1
        print(i, "... ")
        sleep(1)
    end
    print("LIFTOFF!")
end

engineburn(rows) = (println("\n"); for i in 1:rows exhaust(); sleep(0.9^i); end)

testrocket() = (cls(); rocket(); curup(16); countdown(5); curdown(13); engineburn(30))

testrocket()

```
```txt

Countdown...T minus 5... 4... 3...
        /..\
        |==|
        |  |
        |  |
        |  |
       /____\
       |    |
       |SATU|
       |    |
       |    |
      /| |  |\
     / | |  | \
    /__|_|__|__\
       /_\/_\

```



## Perl

```perl
use strict;
use warnings;
use Time::HiRes qw(sleep);

$SIG{INT} = \&clean_up;

my ($rows,$cols) = split /\s+/, qx/stty size/;
my $v =  $rows - 9;
my $h =  int $cols / 2 - 4;
my $a =  0;
my $i =  0;
my $j =  0;
my $t = -5;
my $start = $^T;

my @r = (q'   |',    q'  /_\\',   q'  | |',   q' /| |\\', q'/_|_|_\\');
my @x = (q' (/|\\)', q' {/|\\}',  q'  \\|/',  q'   |');
my @y = (q'  /|\\',  q' // \\\\', q' (/ \\)', q'  \\ /');

my $sp = ' ' x $h;

my $altitude = 0;
my $velocity = 0;

my @pal = ("\e[38;2;255;0;0m", "\e[38;2;255;255;0m", "\e[38;2;255;155;0m");
use constant W => "\e[38;2;255;255;255m";

print "\e[?25l\e[48;5;232m";

while (1) {
    if ($t >= 0) {
        $velocity = 5 * $t**2;
        $altitude = $velocity * $t / 2;
        $a = int 0.5 + ($altitude / $v);
    }
    clean_up() if $a > $rows + 5;

    print "\e[H\e[J", ("\n")x$v, W, $sp, join("\n$sp",@r), "\n", $sp;

    if ($t < 0) {
        print q'\\/   \\/'
    } else {
        exhaust( $pal[$i], $a )
    }
    print W, "\n", '▔' x $cols, $pal[1];
    printf "\n Time: T %-4s %9s  Altitude: %6.2f meters  Velocity: %5.1f m/sec\n",
    $t < 0 ? '- ' . int 0.5 + abs $t : '+ ' . int 0.5 + $t,
    $t < 0 ? '' : $a == 0 ? 'Ignition!' : 'Lift-off!',
    sprintf('%.2f', $altitude), sprintf('%.1f', $velocity);

    ++$i;
    $i %= 3;
    ++$j;
    $j %= 2;
    $t = (time() - $start - 5);
    sleep .05;
}

sub exhaust {
    my($clr, $a) = @_;
    print q'\\/', $clr, q'/^\\', W, q'\\/';
    return if $a == 0;
    if ($a < 4) {
        print "\n", $clr,
        $sp, ( $j ? join("\n$sp", @x[0..$a-1]) : join("\n$sp", @y[0..$a-1]) )
    } else {
        print "\n", $clr,
        $sp, ( $j ? join("\n$sp",@x) : join("\n$sp",@y) );
        print "\n" x ($a-4);
    }
}

# clean up on exit, reset ANSI codes, scroll, re-show the cursor & clear screen
sub clean_up { print "\e[0m", ("\n")x50, "\e[H\e[J\e[?25h"; exit(0) }
```



## Perl 6

Uses ANSI graphics. Works best in a 24 bit ANSI terminal at least 80x24, though bigger is better.

This is a very simple simulation. It assumes a constant ~2G+ acceleration in a gravitational field; so net +10 meters per second². It completely neglects the effects of [[wp:Drag_(physics)|air friction]], [[wp:Impulse|impulse]], [[wp:Snap,_Crackle_and_Pop#Physics|snap, crackle & pop]] and has an unrealistically clean fuel burn (no contrail). It does however (unlike most of the entries at this time) start at the base of the terminal (on the ground) and go up, rather than starting at the top and dropping a contrail. It calculates and displays an accurate displacement and velocity over time and uses those to scale its vertical screen displacement. 

The motion is a little "notchy" as the vertical resolution in a terminal is rather low. Exits after the rocket leaves the visible area of the terminal. See the [https://github.com/thundergnat/rc/blob/master/img/rocket-perl6.gif example animated gif]


```perl6
signal(SIGINT).tap: { cleanup() }

my ($rows,$cols) = qx/stty size/.words;
my $v = floor $rows - 9;
my $h = floor $cols / 2 - 4;
my $a = 0;
my $start = now;
my $t = -5;
my $i = 0;
my $j = 0;

my @r = Q'   |',   Q'  /_\',  Q'  | |',  Q' /| |\', Q'/_|_|_\';
my @x = Q' (/|\)', Q' {/|\}', Q'  \|/',  Q'   |';
my @y = Q'  /|\',  Q' // \\', Q' (/ \)', Q'  \ /'; #'

my $sp = ' ' x $h;

my $altitude = 0;
my $velocity = 0;

my @pal = "\e[38;2;255;0;0m", "\e[38;2;255;255;0m", "\e[38;2;255;155;0m";
constant \W = "\e[38;2;255;255;255m";

print "\e[?25l\e[48;5;232m";

loop {
    if $t >= 0 {
        $velocity = 5 * $t²;
        $altitude = $velocity * $t / 2;
        $a = ($altitude / $v).round;
    }
    cleanup() if $a > $rows + 5;

    print "\e[H\e[J", "\n" xx $v, W, $sp, @r.join("\n$sp"), "\n", $sp;
    if $t < 0 {
        print Q'\/   \/'
    } else {
        exhaust( @pal[$i], $a )
    }
    print W, "\n", '▔' x $cols, @pal[1];
    printf "\n Time: T %-4s %9s  Altitude: %6.2f meters  Velocity: %5.1f m/sec\n",
    $t < 0 ?? "- {$t.round.abs}" !! "+ {$t.round}",
    $t < 0 ?? '' !! $a == 0 ?? 'Ignition!' !! 'Lift-off!',
    $altitude.round(.01), $velocity.round(.1);

    ++$i;
    $i %= 3;
    ++$j;
    $j %= 2;
    $t = (now - $start - 5);
    sleep .05;
}

sub exhaust ($clr, $a) {
    print Q'\/', $clr, Q'/^\', W, Q'\/'; #'
    return if $a == 0;
    if $a < 4 {
        print "\n", $clr,
        $sp, ( $j ?? @x[^$a].join("\n$sp") !! @y[^$a].join("\n$sp"))
    } else {
        print "\n", $clr,
        $sp, ( $j ?? @x.join("\n$sp") !! @y.join("\n$sp"));
        print "\n" x $a - 4;
    }
}

# clean up on exit, reset ANSI codes, scroll, re-show the cursor & clear screen
sub cleanup () {  print "\e[0m", "\n" xx 50, "\e[H\e[J\e[?25h"; exit(0) }
```


See [https://github.com/thundergnat/rc/blob/master/img/rocket-perl6.gif rocket-perl6.gif] (offsite animated gif image)


## Phix


```Phix
sequence rocket = split("""
     /\   
    /  \  
    |  |  
    |  |  
   /|/\|\ 
  /_||||_\
""","\n")

integer lines = 0, l = 0, t = 5
atom s = 1

while length(rocket) do
    if t>0 then
        rocket[$] = sprintf("T minus %d...",t)
        -- allow console resize during countdown:
        lines = video_config()[VC_SCRNLINES]
        if l!=lines-7 then l = 0 end if
    else
        if l=1 then
            rocket = rocket[2..$]
        else
            l -= (length(rocket)>6)
            if length(rocket)<12 then
                rocket = append(rocket,"     **     ")
            elsif length(rocket)=12 then
                rocket = append(rocket,"       ")
            end if
        end if
        s = s*0.95
    end if
    if l=0 then
        clear_screen()
        cursor(NO_CURSOR)
        l = lines-7
    end if
    position(l,1)
    puts(1,join(rocket,"\n"))
    sleep(s)
    t -= 1
    if t=0 then rocket = rocket[1..$-1] end if
end while
cursor(BLOCK_CURSOR)
```



## Racket


```racket
#lang racket

(define rocket #<<EOF
   /\
  (  )
  (  )
 /|/\|\
/_||||_\
EOF
  )

(define (cls) (displayln "\x1B[2J"))

(define (print-rocket n)
  (displayln rocket)
  (for ([i (in-range n)]) (displayln "")))

(for ([i (in-range 5 0 -1)])
  (cls)
  (printf "~a =>\n" i)
  (print-rocket 0)
  (sleep 1))

(cls)
(printf "Liftoff!\n")
(print-rocket 1)
(sleep 1)

(for/fold ([ms 1000] #:result (void)) ([n (in-range 2 100)])
  (cls)
  (print-rocket n)
  (sleep (/ ms 1000))
  (if (>= ms 40) (- ms 40) 0))
```



## REXX

This REXX program hard-codes the name of the (OS) command to clear the terminal screen   ('''CLS''').

```rexx
/*REXX pgm does a countdown and then display the launching of a rocket (ASCII animation)*/
parse arg cntDown .                              /*obtain optional argument from the CL.*/
if cntDown=='' | cntDown==","  then cntDown= 5   /*Not specified?  Then use the default.*/
  @. =                                           /* [↓]  glyphs for the rocket ship.    */
  @.1= '   /\   '
  @.2= '  |  |  '
  @.3= '  |  |  '
  @.4= '  |  |  '
  @.5= ' /|/\|\ '
  @.6= '/_||||_\'
                    do rs=1  while @.rs\==''     /*determine size of the rocket (height)*/
                    end   /*rs*/
rs= rs - 1                                       /*the true  rocket size  (height).     */
cls= 'CLS'                                       /*the command used to clear the screen.*/
parse value  scrsize()  with  sd sw .
sw= sw - 1                                       /*usable screen width on some systems. */
sd= sd - 3                                       /*   "      "   depth  "   "     "     */
air= sd - 1 - rs                                 /*"amount" of sky above the rocket.    */
say
      do j=cntDown  by -1  to 1                  /* [↓]  perform countdown; show rocket.*/
      cls                                        /*use this command to clear the screen.*/
      say  right(j, 9) 'seconds'                 /*display the amount of seconds to go. */
      call sky                                   /*display the sky above the rocket.    */
      call rocket                                /*display the rocket  (on the ground). */
      call delay 1                               /*wait one second during the countdown.*/
      end   /*j*/

say left('',9)       "liftoff!"                  /*announce liftoff of the rocket.      */
cls                                              /*use this command to clear the screen.*/
call sky                                         /*display the sky above the rocket.    */
period= 1
dt= period / sd                                  /*acceleration  (period is decreasing).*/
call rocket                                      /*display the rocket  (in flight).     */
             do  sd+4;      say                  /*"make" the rocket appear to fly.     */
             period= format(period-period*dt,,3) /*calculate the decrease in the period.*/
             call delay max(period, .001)        /*wait for a diminishing time interval.*/
             end   /*sd+4*/
exit                                             /*stick a fork in it, da rocket is gone*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
sky:     do air;   say;  end  /*air*/;   return  /*display the sky above the rocket.    */
rocket:  do ship=1  for rs;   say left('', sw%2 - 5)  @.ship;   end  /*ship*/;      return
```

This REXX program makes use of   '''SCRSIZE'''   REXX program (or
BIF) which is used to determine the screen

width and depth of the terminal (console).    Some REXXes don't
have this BIF.

The   '''SCRSIZE.REX'''   REXX program is included
here   ───►   [[SCRSIZE.REX]]. 




## Rust


```rust

use std::{thread, time};

fn print_rocket(above: u32) {
	print!(
"  oo
 oooo
 oooo
 oooo
");
for _num in 1..above+1 {
 println!("  ||");
}
}

fn main() {

    // counting
    for number in (1..6).rev() {
        print!("\x1B[2J");
      	println!("{} =>", number);
        print_rocket(0);
	let dur = time::Duration::from_millis(1000);
        thread::sleep(dur);
    }

    // ignition
    print!("\x1B[2J");
    println!("Liftoff !");
    print_rocket(1);
    let dur = time::Duration::from_millis(1000);
    thread::sleep(dur);

    // liftoff
    let mut dur_time : u64 = 1000;
    for number in 2..100 {
    	print!("\x1B[2J");
        print_rocket(number);	
	let dur = time::Duration::from_millis(dur_time);
        thread::sleep(dur);
	dur_time -= if dur_time >= 30 {30} else {dur_time};
    }
}


```



## zkl

Uses ANSI terminal codes.

```zkl
var [const] rocket=
#<<<
0'~
    /\
   (  )
   (  )
  /|/\|\
 /_||||_\
~,           flame="    **";
#<<<
 
fcn cls		 { print("\x1B[2J") }
fcn cursorUp(n)  { print("\e[%dA".fmt(n)) }
fcn cursorDown(n){ print("\e[%dB".fmt(n)) }
fcn cursor2Col(n){ print("\e[%dG".fmt(n)) }

fcn __main__{
   tall,tall := rocket.counts(), tall[tall.find("\n")+1];
   cls(); print(rocket); cursorUp(tall);

   // count down to ignition
   print("T minus: ");
   foreach n in ([5..1, -1]){ print(n," "); Atomic.sleep(1); }
   print("    Liftoff !"); cursorDown(tall); cursor2Col(1);
 
   // liftoff
   ms:=1.0;		// 1 sec
   do(25){
      println(flame); Atomic.sleep(ms);
      ms=(ms - 0.04).max(0);   // 40 milliseconds faster than last time
   }
}
```

```txt

T minus: 5 4 3 2 1     Liftoff !
    /\
   (  )
   (  )
  /|/\|\
 /_||||_\
    **
    **
    **
    **

```

